baseline_script <- testthat::test_path(
  "..", "..", "validation", "current_worktree_baseline",
  "current_worktree_baseline.R"
)

if (!file.exists(baseline_script)) {
  testthat::skip("Current-worktree baseline script is unavailable in this source tree.")
}

baseline_env <- new.env(parent = globalenv())
sys.source(baseline_script, envir = baseline_env)

make_baseline_test_result <- function(run_id = "run_a", params_hash = "params_a",
                                      event_start_time = 0.10) {
  train_02 <- data.frame(
    idx = 1:3,
    ISI_sec = c(NA_real_, 0.70, 0.04),
    pattern_auto = c("", "pause", ""),
    auto_score = c(NA_real_, 1.25, NA_real_),
    stringsAsFactors = FALSE
  )
  train_01 <- data.frame(
    idx = 1:4,
    ISI_sec = c(NA_real_, 0.02, 0.02, 0.80),
    pattern_auto = c("", "burst", "burst", ""),
    auto_score = c(NA_real_, 2.5, 2.5, NA_real_),
    stringsAsFactors = FALSE
  )
  events <- data.frame(
    train = c("Train_02", "Train_01"),
    pattern = c("pause", "burst"),
    auto_pattern_majority = c("pause", "burst"),
    label_source = c("auto", "auto"),
    start_isi = c(2L, 2L),
    end_isi = c(2L, 3L),
    start_spike_idx = c(1L, 1L),
    end_spike_idx = c(2L, 3L),
    n_spikes = c(2L, 3L),
    n_isi = c(1L, 2L),
    start_time_sec = c(0.80, event_start_time),
    end_time_sec = c(1.50, 0.14),
    duration_sec = c(0.70, 0.04),
    auto_score = c(1.25, 2.5),
    run_id = run_id,
    params_hash = params_hash,
    audit_time = if (identical(run_id, "run_a")) {
      "2026-08-10T10:00:00Z"
    } else {
      "2026-08-11T11:00:00Z"
    },
    stringsAsFactors = FALSE
  )
  candidates <- data.frame(
    train = c("Train_02", "Train_01", "Train_01"),
    candidate_id = c("pause_1", "burst_2", "profile"),
    candidate_layer = c("pause_layer", "burst_layer", "profile_layer"),
    candidate_class = c("pause", "burst", "train_profile"),
    final_label = c("pause", "burst", "profile"),
    action = c("accept", "accept", "audit_only"),
    gate_status = c("pass", "pass", "profile"),
    decision_path = c("pause_pass", "burst_pass", "profile_only"),
    start_isi = c(2L, 2L, NA_integer_),
    end_isi = c(2L, 3L, NA_integer_),
    score = c(1.25, 2.5, NA_real_),
    priority = c(320, 1250, NA_real_),
    selected_for_auto = c(TRUE, TRUE, FALSE),
    selection_status = c("selected", "selected", "not_selected"),
    suppressed_original_label = c("", "", ""),
    run_id = run_id,
    params_hash = params_hash,
    stringsAsFactors = FALSE
  )
  list(
    trains = list(Train_02 = train_02, Train_01 = train_01),
    results = list(
      events = events,
      candidate_diagnostic_audit = candidates
    )
  )
}

test_that("normalization covers rows, selected events, and legacy candidate status", {
  first <- baseline_env$stpd_baseline_normalize_result(
    make_baseline_test_result("run_a", "params_a")
  )
  second <- baseline_env$stpd_baseline_normalize_result(
    make_baseline_test_result("run_b", "params_b")
  )

  expect_identical(first, second)
  expect_identical(
    baseline_env$stpd_baseline_checksums(first),
    baseline_env$stpd_baseline_checksums(second)
  )
  expect_identical(names(first$detector_rows), names(baseline_env$STPD_BASELINE_ISI_TYPES))
  expect_identical(names(first$selected_events), names(baseline_env$STPD_BASELINE_EVENT_TYPES))
  expect_identical(
    names(first$legacy_candidates),
    names(baseline_env$STPD_BASELINE_CANDIDATE_TYPES)
  )
  expect_false(any(c("run_id", "params_hash", "audit_time") %in%
    names(first$selected_events)))
  expect_identical(
    first$detector_rows$train,
    c(rep("Train_01", 4L), rep("Train_02", 3L))
  )
  expect_equal(sum(is.finite(first$detector_rows$ISI_sec)), 5L)
})

test_that("candidate schema fills absent source and rejection fields with typed NA", {
  payload <- baseline_env$stpd_baseline_normalize_result(make_baseline_test_result())

  expect_type(payload$legacy_candidates$candidate_source, "character")
  expect_true(all(is.na(payload$legacy_candidates$candidate_source)))
  expect_type(payload$legacy_candidates$rejection_reason, "character")
  expect_true(all(is.na(payload$legacy_candidates$rejection_reason)))
  expect_type(payload$legacy_candidates$selected_for_auto, "logical")
})

test_that("all-schema event and candidate ordering is source-order invariant", {
  expected <- baseline_env$stpd_baseline_normalize_result(make_baseline_test_result())
  reversed_result <- make_baseline_test_result()
  reversed_result$trains <- rev(reversed_result$trains)
  reversed_result$results$events <- reversed_result$results$events[
    rev(seq_len(nrow(reversed_result$results$events))), , drop = FALSE
  ]
  reversed_result$results$candidate_diagnostic_audit <-
    reversed_result$results$candidate_diagnostic_audit[
      rev(seq_len(nrow(reversed_result$results$candidate_diagnostic_audit))), , drop = FALSE
    ]
  actual <- baseline_env$stpd_baseline_normalize_result(reversed_result)

  expect_identical(actual, expected)
  expect_true(baseline_env$stpd_baseline_compare_payloads(actual, expected)$equal)
})

test_that("comparison detects exact score, label, geometry, and selection changes", {
  expected <- baseline_env$stpd_baseline_normalize_result(make_baseline_test_result())

  score_changed_result <- make_baseline_test_result()
  score_changed_result$trains$Train_01$auto_score[2L] <- 2.500000000000001
  score_changed <- baseline_env$stpd_baseline_normalize_result(score_changed_result)
  score_comparison <- baseline_env$stpd_baseline_compare_payloads(score_changed, expected)
  expect_false(score_comparison$equal)
  expect_false(score_comparison$summary$exact_equal[
    score_comparison$summary$component == "detector_rows"
  ])

  label_changed_result <- make_baseline_test_result()
  label_changed_result$trains$Train_01$pattern_auto[2L] <- "possible_burst"
  label_changed <- baseline_env$stpd_baseline_normalize_result(label_changed_result)
  expect_false(baseline_env$stpd_baseline_compare_payloads(label_changed, expected)$equal)

  geometry_changed <- baseline_env$stpd_baseline_normalize_result(
    make_baseline_test_result(event_start_time = 0.100000000000001)
  )
  geometry_comparison <- baseline_env$stpd_baseline_compare_payloads(geometry_changed, expected)
  expect_false(geometry_comparison$equal)
  expect_false(geometry_comparison$summary$exact_equal[
    geometry_comparison$summary$component == "selected_events"
  ])

  selection_changed_result <- make_baseline_test_result()
  selection_changed_result$results$candidate_diagnostic_audit$selected_for_auto[2L] <- FALSE
  selection_changed <- baseline_env$stpd_baseline_normalize_result(selection_changed_result)
  candidate_comparison <- baseline_env$stpd_baseline_compare_payloads(selection_changed, expected)
  expect_false(candidate_comparison$equal)
  expect_false(candidate_comparison$summary$exact_equal[
    candidate_comparison$summary$component == "legacy_candidates"
  ])
})

test_that("reference writing requires explicit confirmation and preserves effective params", {
  payload <- baseline_env$stpd_baseline_normalize_result(make_baseline_test_result())
  effective_params <- default_params()
  output_dir <- tempfile("stpd_explicit_reference_")
  on.exit(unlink(output_dir, recursive = TRUE, force = TRUE), add = TRUE)

  metadata <- list(
    input_relative_path = "fixture.csv",
    input_sha256 = paste(rep("a", 64L), collapse = ""),
    detector_source_sha256 = paste(rep("b", 64L), collapse = ""),
    detection_mode = "label_blind",
    package_version = as.character(utils::packageVersion("SpikeTrainPatternDetector")),
    train_count = 2L
  )
  expect_error(
    baseline_env$stpd_baseline_write_payload(
      payload, output_dir, effective_params = effective_params, metadata = metadata
    ),
    "confirm = TRUE"
  )
  expect_false(dir.exists(output_dir))

  baseline_env$stpd_baseline_write_payload(
    payload,
    output_dir = output_dir,
    effective_params = effective_params,
    metadata = metadata,
    confirm = TRUE
  )
  expect_true(all(file.exists(file.path(
    output_dir,
    unname(baseline_env$STPD_BASELINE_REFERENCE_FILES)
  ))))

  reference <- baseline_env$stpd_baseline_read_reference(output_dir)
  expect_true(baseline_env$stpd_baseline_compare_payloads(
    reference$payload, payload
  )$equal)
  expect_identical(
    reference$manifest_values[["effective_params_hash"]],
    stpd_params_hash(reference$effective_params)
  )
  expect_identical(reference$manifest_values[["detection_mode"]], "label_blind")
  same_reference <- baseline_env$stpd_baseline_compare_reference(
    payload,
    metadata = metadata,
    effective_params = effective_params,
    reference = reference
  )
  expect_true(same_reference$equal)

  changed_params <- effective_params
  changed_params$event_grammar$structural_burst_rescue_compression_min <- 999
  params_mismatch <- baseline_env$stpd_baseline_compare_reference(
    payload,
    metadata = metadata,
    effective_params = changed_params,
    reference = reference
  )
  expect_false(params_mismatch$equal)
  expect_false(params_mismatch$metadata$exact_equal[
    params_mismatch$metadata$field == "effective_params_hash"
  ])
  expect_error(
    baseline_env$stpd_baseline_write_payload(
      payload,
      output_dir = output_dir,
      effective_params = effective_params,
      metadata = metadata,
      confirm = TRUE
    ),
    "already exist"
  )
})

test_that("current runner cannot write through an output path alone", {
  loader_calls <- 0L
  original_loader <- baseline_env$stpd_baseline_load_worktree
  on.exit(
    assign("stpd_baseline_load_worktree", original_loader, envir = baseline_env),
    add = TRUE
  )
  assign(
    "stpd_baseline_load_worktree",
    function(...) {
      loader_calls <<- loader_calls + 1L
      stop("baseline loader must not run for an invalid request", call. = FALSE)
    },
    envir = baseline_env
  )

  expect_error(
    baseline_env$stpd_baseline_run_current(
      output_dir = tempfile("must_not_write_")
    ),
    "write_baseline is not TRUE"
  )
  expect_identical(loader_calls, 0L)

  expect_error(
    baseline_env$stpd_baseline_run_current(write_baseline = TRUE),
    "requires an explicit output_dir"
  )
  expect_identical(loader_calls, 0L)
})

reference_dir <- file.path(dirname(baseline_script), "reference")

test_that("frozen 30-train reference is internally verifiable when present", {
  testthat::skip_if_not(
    file.exists(file.path(reference_dir, "baseline_manifest.csv")),
    "Root generates the real reference explicitly after source stabilization."
  )
  reference <- baseline_env$stpd_baseline_read_reference(reference_dir)

  expect_equal(nrow(reference$payload$detector_rows), 8776L)
  expect_equal(sum(is.finite(reference$payload$detector_rows$ISI_sec)), 8746L)
  expect_equal(nrow(reference$payload$selected_events), 519L)
  expect_gt(nrow(reference$payload$legacy_candidates), 0L)
  expect_identical(
    reference$manifest_values[["scientific_payload_sha256"]],
    unname(baseline_env$stpd_baseline_checksums(reference$payload)[[
      "scientific_payload_sha256"
    ]])
  )
})

test_that("optional slow test compares a fresh real run with the frozen reference", {
  testthat::skip_if_not(
    identical(Sys.getenv("STPD_RUN_30_TRAIN_BASELINE"), "true"),
    "Set STPD_RUN_30_TRAIN_BASELINE=true for the explicit slow comparison."
  )
  testthat::skip_if_not(dir.exists(reference_dir), "Frozen reference is unavailable.")

  run <- baseline_env$stpd_baseline_run_current(compare_dir = reference_dir)
  expect_true(run$comparison$equal)
})
