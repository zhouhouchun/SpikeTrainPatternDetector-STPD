c3_dataset <- function(offset = 0) {
  train <- data.frame(timestamp_sec = offset + seq(0, 0.19, by = 0.01))
  SpikeTrainPatternDetector:::make_dataset(
    "phase-c", "synthetic", list(t1 = train)
  )
}

c3_bundle <- function(dataset = c3_dataset()) {
  artifact <- list(
    schema_version = "stpd_external_interval_artifact_v1",
    provider_version = "fixture-1.0.0",
    provider_code_sha256 = paste(rep("c", 64L), collapse = ""),
    information_access = "unknown",
    coordinate_profile_id = "train_row_isi_one_closed_v1",
    records = list(list(
      source_record_key = "source-1", train_key = "t1",
      source_start = 4L, source_end = 8L, semantic_track = "event",
      proposed_label = "burst", provider_decision = "positive",
      score_name = NULL, score_value = NULL,
      score_direction = "not_applicable", uncertainty_kind = "none",
      uncertainty_lower = NULL, uncertainty_upper = NULL,
      evidence_manifest_sha256 = NULL
    ))
  )
  raw <- charToRaw(enc2utf8(jsonlite::toJSON(
    artifact, auto_unbox = TRUE, null = "null", na = "null", digits = 17
  )))
  request <- list(
    schema_version = "stpd_provider_import_request_v1",
    adapter_key = "external_interval_v1", adapter_version = "1.0.0",
    output_role = "automatic_prediction", generation_mode = "external_only",
    selected_train_keys = "t1", parameters = list()
  )
  stpd_import_provider_batch(dataset, request, raw)
}

c3_request <- function(product, bundle, action = "accept_as_is",
                       operation = "operation-1", start = NULL, end = NULL,
                       target = NULL) {
  list(
    schema_version = "stpd_provider_adjudication_request_v2",
    operation_id = operation, action = action,
    provider_run_id = bundle$provider_runs$provider_run_id[[1L]],
    source_record_id = bundle$candidate_intervals$candidate_id[[1L]],
    expected_product_sha256 = product$product_sha256,
    reviewer_id = "reviewer-pseudonym-1", reason = "predeclared review",
    decided_utc = "2026-08-27T12:00:00Z",
    adjusted_start_isi = start, adjusted_end_isi = end,
    target_decision_id = target
  )
}

c3_error <- function(expr, code) {
  error <- tryCatch(force(expr), error = function(e) e)
  expect_s3_class(error, "stpd_provider_contract_error")
  expect_identical(error$code, code)
  expect_true(inherits(error, code))
  invisible(error)
}

test_that("Phase C accept is append-only, replayable, and AUTO immutable", {
  dataset <- c3_dataset()
  bundle <- c3_bundle(dataset)
  bundle_bytes <- serialize(bundle, NULL, version = 3L)
  dataset_bytes <- serialize(dataset, NULL, version = 3L)
  product <- stpd_new_provider_adjudication(bundle)
  expect_silent(stpd_validate_provider_adjudication(bundle, product))

  request <- c3_request(product, bundle)
  accepted <- stpd_apply_provider_adjudication(bundle, product, request)
  expect_equal(nrow(accepted$history), 1L)
  expect_identical(accepted$current_decisions$effect, "accepted")
  expect_equal(nrow(accepted$adjudicated_intervals), 1L)
  expect_identical(
    accepted$adjudicated_intervals$geometry_origin, "source"
  )
  expect_identical(serialize(bundle, NULL, version = 3L), bundle_bytes)
  expect_identical(serialize(dataset, NULL, version = 3L), dataset_bytes)
  expect_identical(
    stpd_replay_provider_adjudication(bundle, accepted$history), accepted
  )
  expect_identical(
    stpd_apply_provider_adjudication(bundle, accepted, request), accepted
  )
})

test_that("Phase C public request builder is exact and reusable", {
  bundle <- c3_bundle()
  empty <- stpd_new_provider_adjudication(bundle)
  request <- stpd_provider_review_request(
    empty, "accept_as_is", bundle$provider_runs$provider_run_id[[1L]],
    bundle$candidate_intervals$candidate_id[[1L]], "builder-operation",
    "reviewer-pseudonym-1", "builder contract",
    "2026-08-27T12:01:00Z"
  )
  expect_identical(names(request), names(c3_request(empty, bundle)))
  expect_true(is.na(request$adjusted_start_isi))
  expect_silent(
    stpd_apply_provider_adjudication(bundle, empty, request)
  )
  invalid_time <- request
  invalid_time$operation_id <- "invalid-time"
  invalid_time$decided_utc <- "2026-99-99T12:01:00Z"
  c3_error(
    stpd_apply_provider_adjudication(bundle, empty, invalid_time),
    "review_precondition_mismatch"
  )
})

test_that("Phase C requires compensation before a changed decision", {
  bundle <- c3_bundle()
  empty <- stpd_new_provider_adjudication(bundle)
  accepted <- stpd_apply_provider_adjudication(
    bundle, empty, c3_request(empty, bundle)
  )
  c3_error(
    stpd_apply_provider_adjudication(
      bundle, accepted,
      c3_request(accepted, bundle, "reject", "operation-2")
    ),
    "review_precondition_mismatch"
  )
  void_request <- c3_request(
    accepted, bundle, "void_prior_decision", "operation-2",
    target = accepted$current_decisions$current_decision_id[[1L]]
  )
  voided <- stpd_apply_provider_adjudication(bundle, accepted, void_request)
  expect_equal(nrow(voided$current_decisions), 0L)
  expect_equal(nrow(voided$adjudicated_intervals), 0L)
  rejected <- stpd_apply_provider_adjudication(
    bundle, voided, c3_request(voided, bundle, "reject", "operation-3")
  )
  expect_identical(rejected$current_decisions$effect, "rejected")
  expect_equal(nrow(rejected$history), 3L)
  expect_identical(
    stpd_replay_provider_adjudication(bundle, rejected$history), rejected
  )
})

test_that("Phase C adjusts only with exact train-boundary proof", {
  dataset <- c3_dataset()
  bundle <- c3_bundle(dataset)
  empty <- stpd_new_provider_adjudication(bundle)
  request <- c3_request(
    empty, bundle, "adjust_bounds", "operation-adjust", 3L, 10L
  )
  c3_error(
    stpd_apply_provider_adjudication(bundle, empty, request),
    "review_hard_boundary_crossed"
  )
  adjusted <- stpd_apply_provider_adjudication(
    bundle, empty, request, dataset = dataset
  )
  interval <- adjusted$adjudicated_intervals
  expect_identical(interval$canonical_start_isi, 3L)
  expect_identical(interval$canonical_end_isi, 10L)
  expect_identical(interval$canonical_start_spike, 2L)
  expect_identical(interval$canonical_end_spike, 10L)
  expect_identical(interval$geometry_origin, "adjusted")

  stale <- c3_dataset(offset = 1)
  c3_error(
    stpd_apply_provider_adjudication(bundle, empty, request, stale),
    "review_source_stale"
  )
  disjoint <- c3_request(
    empty, bundle, "adjust_bounds", "operation-disjoint", 12L, 14L
  )
  c3_error(
    stpd_apply_provider_adjudication(bundle, empty, disjoint, dataset),
    "review_bounds_invalid"
  )
  outside <- c3_request(
    empty, bundle, "adjust_bounds", "operation-outside", 1L, 25L
  )
  c3_error(
    stpd_apply_provider_adjudication(bundle, empty, outside, dataset),
    "review_hard_boundary_crossed"
  )
})

test_that("scientific derived interval ID excludes audit-only fields", {
  dataset <- c3_dataset()
  bundle <- c3_bundle(dataset)
  first_parent <- stpd_new_provider_adjudication(bundle)
  second_parent <- stpd_new_provider_adjudication(bundle)
  first_request <- c3_request(
    first_parent, bundle, "adjust_bounds", "audit-a", 3L, 10L
  )
  second_request <- c3_request(
    second_parent, bundle, "adjust_bounds", "audit-b", 3L, 10L
  )
  second_request$reviewer_id <- "reviewer-pseudonym-2"
  second_request$reason <- "independent audit wording"
  second_request$decided_utc <- "2026-08-27T13:00:00Z"
  first <- stpd_apply_provider_adjudication(
    bundle, first_parent, first_request, dataset
  )
  second <- stpd_apply_provider_adjudication(
    bundle, second_parent, second_request, dataset
  )
  expect_identical(
    first$adjudicated_intervals$adjudicated_interval_id,
    second$adjudicated_intervals$adjudicated_interval_id
  )
  expect_false(identical(first$product_sha256, second$product_sha256))
})

test_that("Phase C fails closed for unsupported, stale, and tampered input", {
  bundle <- c3_bundle()
  empty <- stpd_new_provider_adjudication(bundle)
  unsupported <- c3_request(empty, bundle)
  unsupported$action <- "split"
  c3_error(
    stpd_apply_provider_adjudication(bundle, empty, unsupported),
    "review_action_unsupported"
  )
  missing <- c3_request(empty, bundle)
  missing$source_record_id <- paste(rep("0", 64L), collapse = "")
  c3_error(
    stpd_apply_provider_adjudication(bundle, empty, missing),
    "review_parent_not_found"
  )
  changed_bundle <- c3_bundle(c3_dataset(offset = 1))
  c3_error(
    stpd_validate_provider_adjudication(changed_bundle, empty),
    "review_source_stale"
  )
  accepted <- stpd_apply_provider_adjudication(
    bundle, empty, c3_request(empty, bundle)
  )
  tampered <- accepted
  tampered$history$reason[[1L]] <- "changed after review"
  c3_error(
    stpd_validate_provider_adjudication(bundle, tampered),
    "review_precondition_mismatch"
  )
})

test_that("Phase C export is complete and refuses overwrite", {
  bundle <- c3_bundle()
  empty <- stpd_new_provider_adjudication(bundle)
  accepted <- stpd_apply_provider_adjudication(
    bundle, empty, c3_request(empty, bundle)
  )
  out <- file.path(tempdir(), paste0("adjudication-v2-", Sys.getpid(), "-",
                                     sample.int(1000000L, 1L)))
  expect_identical(
    stpd_write_provider_adjudication(bundle, accepted, out), out
  )
  expect_true(all(c(
    "adjudication_v2.rds", "history.csv", "current_decisions.csv",
    "adjudicated_intervals.csv", "lineage.csv", "manifest.json"
  ) %in% list.files(out)))
  manifest <- jsonlite::fromJSON(file.path(out, "manifest.json"))
  expect_identical(manifest$product_sha256, accepted$product_sha256)
  expect_error(
    stpd_write_provider_adjudication(bundle, accepted, out),
    "already exists", fixed = TRUE
  )
})
