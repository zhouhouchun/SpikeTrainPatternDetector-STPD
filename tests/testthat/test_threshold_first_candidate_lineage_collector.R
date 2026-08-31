collector_error <- function(expr) {
  tryCatch(expr, error = identity)
}

collector_bind <- function(level = "full", run_id = "run_A",
                           trains = c("train_1", "train_2")) {
  collector <- stpd_candidate_lineage_collector_new(level)
  stpd_candidate_lineage_collector_bind_run(
    collector,
    run_id = run_id,
    params_hash = paste0("params_", run_id),
    dataset_id = "dataset_A",
    target_trains = trains
  )
  collector
}

collector_complete_train <- function(collector, train,
                                     pipeline = "hf_protected") {
  shard <- stpd_candidate_lineage_collector_begin_train(collector, train)
  stpd_candidate_lineage_collector_note_pipeline(
    shard, pipeline, train = train
  )
  stpd_candidate_lineage_collector_end_train(collector, shard)
  shard
}

test_that("audit level is runtime-only and dormant run config fails closed", {
  expect_identical(
    stpd_candidate_lineage_resolve_audit_level(), "off"
  )
  expect_identical(
    stpd_candidate_lineage_resolve_audit_level("summary"), "summary"
  )
  current <- list(threshold_first = stpd_threshold_first_run_config(
    engine_algorithm = "legacy",
    threshold_source = "ordered_fallback",
    manual_policy = "lock",
    audit_level = "summary"
  ))
  for (requested in list(NULL, "summary", "full")) {
    dormant <- collector_error(
      stpd_candidate_lineage_resolve_audit_level(requested, current)
    )
    expect_s3_class(
      dormant, "stpd_candidate_lineage_run_config_not_activated"
    )
  }

  invalid <- list(
    NA_character_, c("off", "full"), factor("off"), TRUE, "ful"
  )
  for (value in invalid) {
    expect_s3_class(
      collector_error(stpd_candidate_lineage_resolve_audit_level(value)),
      "stpd_threshold_first_error"
    )
  }

  # collect_diagnostics does not participate in this resolver. A legacy params
  # list with no explicit current run config remains collector-off.
  expect_identical(
    stpd_candidate_lineage_resolve_audit_level(
      NULL, list(collect_diagnostics = TRUE)
    ),
    "off"
  )
})

test_that("collector lifecycle is run-local, train-local, and deep-copied", {
  first <- collector_bind("full", "run_A")
  second <- collector_bind("summary", "run_B")

  first_2 <- collector_complete_train(first, "train_2")
  second_1 <- collector_complete_train(second, "train_1")
  first_1 <- collector_complete_train(first, "train_1")
  second_2 <- collector_complete_train(second, "train_2")

  expect_identical(first_1$run_id, "run_A")
  expect_identical(first_2$run_id, "run_A")
  expect_identical(second_1$run_id, "run_B")
  expect_identical(second_2$run_id, "run_B")

  snap_first <- stpd_candidate_lineage_collector_snapshot(first)
  snap_second <- stpd_candidate_lineage_collector_snapshot(second)
  expect_identical(
    snap_first$receipt_version,
    STPD_CANDIDATE_LINEAGE_REQUEST_RECEIPT_VERSION
  )
  expect_identical(snap_first$run_id, "run_A")
  expect_identical(snap_first$params_hash, "params_run_A")
  expect_identical(snap_first$dataset_id, "dataset_A")
  expect_identical(snap_first$target_trains, c("train_1", "train_2"))
  expect_identical(snap_first$train_receipts$train,
                   c("train_1", "train_2"))
  expect_true(all(snap_first$train_receipts$train_state == "completed"))
  expect_true(all(snap_second$train_receipts$train_state == "completed"))
  expect_true(all(snap_first$train_receipts$run_id == "run_A"))
  expect_true(all(snap_second$train_receipts$run_id == "run_B"))

  snap_first$train_receipts$train_state[] <- "tampered"
  fresh <- stpd_candidate_lineage_collector_snapshot(first)
  expect_true(all(fresh$train_receipts$train_state == "completed"))

  duplicate <- collector_error(
    stpd_candidate_lineage_collector_begin_train(first, "train_1")
  )
  expect_s3_class(duplicate, "stpd_candidate_lineage_collector_train_duplicate")

  third <- collector_bind("full", "run_C", "train_1")
  third_shard <- stpd_candidate_lineage_collector_begin_train(third, "train_1")
  stpd_candidate_lineage_collector_note_pipeline(
    third_shard, "hf_protected", train = "train_1"
  )
  cross_run <- collector_error(
    stpd_candidate_lineage_collector_end_train(first, third_shard)
  )
  expect_s3_class(cross_run, "stpd_candidate_lineage_collector_cross_run_shard")
  stpd_candidate_lineage_collector_abort(third)
})

test_that("off is a true no-op and unavailable finalization is fail-closed", {
  off <- collector_bind("off", "run_off")
  expect_null(
    stpd_candidate_lineage_collector_begin_train(off, "train_1")
  )
  off_snapshot <- stpd_candidate_lineage_collector_snapshot(off)
  expect_true(all(off_snapshot$train_receipts$train_state == "disabled"))
  scientific <- list(science = list(value = 1L))
  off_result <- stpd_candidate_lineage_collector_finalize_unavailable(
    scientific, off
  )
  expect_identical(off_result, scientific)
  expect_identical(off$state, "sealed")

  full <- collector_bind("full", "run_full")
  collector_complete_train(full, "train_1")
  collector_complete_train(full, "train_2")
  full_result <- stpd_candidate_lineage_collector_finalize_unavailable(
    scientific, full
  )
  expect_identical(full_result$science, scientific$science)
  expect_true("candidate_lineage_audit" %in% names(full_result))
  expect_identical(
    full_result$candidate_lineage_audit$metadata$audit_level, "off"
  )
  expect_identical(
    full_result$candidate_lineage_audit$metadata$evidence_authority,
    "diagnostic_unavailable"
  )
  expect_false(full_result$candidate_lineage_audit$metadata$universe_complete)
  expect_identical(
    full_result$candidate_lineage_audit$metadata$unavailable_reason,
    "pipeline_not_instrumented"
  )
  receipt <- attr(
    full_result$candidate_lineage_audit,
    "candidate_lineage_request_receipt",
    exact = TRUE
  )
  expect_identical(receipt$requested_audit_level, "full")
  expect_identical(receipt$collector_state, "sealed")
  expect_identical(receipt$run_id, "run_full")
  expect_identical(receipt$params_hash, "params_run_full")
  expect_true(all(receipt$train_receipts$train_state == "completed"))
  expect_identical(full$state, "sealed")
  expect_false(any(vapply(full_result, is.environment, logical(1))))

  incomplete <- collector_bind("full", "run_incomplete")
  collector_complete_train(incomplete, "train_1")
  expect_s3_class(
    collector_error(stpd_candidate_lineage_collector_finalize_unavailable(
      scientific, incomplete
    )),
    "stpd_candidate_lineage_collector_train_scope_incomplete"
  )
  stpd_candidate_lineage_collector_abort(incomplete)
  expect_identical(incomplete$state, "aborted")
  expect_silent(stpd_candidate_lineage_collector_abort(incomplete))
})

test_that("collector lifecycle preserves RNG and process options", {
  old_options <- options()
  set.seed(8128)
  seed_before <- .Random.seed

  collector <- collector_bind("full", "run_rng", "train_1")
  collector_complete_train(collector, "train_1")
  result <- stpd_candidate_lineage_collector_finalize_unavailable(
    list(science = 1L), collector
  )
  seed_after <- .Random.seed
  options_after <- options()

  expect_identical(seed_after, seed_before)
  expect_identical(options_after, old_options)
  expect_identical(result$science, 1L)
})

test_that("collector does not create an RNG seed when none exists", {
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv)
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    rm(".Random.seed", envir = .GlobalEnv)
  }

  collector <- collector_bind("summary", "run_no_seed", "train_1")
  collector_complete_train(collector, "train_1")
  stpd_candidate_lineage_collector_finalize_unavailable(
    list(science = TRUE), collector
  )
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
})
