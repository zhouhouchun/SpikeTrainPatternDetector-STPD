phase2c_validation_candidate <- function(
    id, label, start, end, score = 20, priority = 800) {
  data.frame(
    candidate_id = id,
    candidate_layer = "phase2c_validation_fixture",
    candidate_source = "synthetic_unit_fixture",
    final_label = label,
    start_isi = as.integer(start),
    end_isi = as.integer(end),
    n_isi = as.integer(end - start + 1L),
    score = as.numeric(score),
    priority = as.numeric(priority),
    stringsAsFactors = FALSE
  )
}

phase2c_validation_default_pool <- function() {
  dplyr::bind_rows(
    phase2c_validation_candidate(
      "automatic_burst", "burst", 20L, 23L, score = 30, priority = 1200
    ),
    phase2c_validation_candidate(
      "hfs_state", "high_frequency_spiking", 10L, 45L,
      score = 20, priority = 900
    ),
    phase2c_validation_candidate(
      "review_burst", "possible_burst", 20L, 23L,
      score = 12, priority = 300
    ),
    phase2c_validation_candidate(
      "inactive_review_burst", "possible_burst", 21L, 24L,
      score = 5, priority = 300
    )
  )
}

phase2c_validation_fixture <- function(
    pool = phase2c_validation_default_pool(), label_blind = TRUE,
    two_trains = FALSE) {
  params <- default_params()
  params$spiketrainpattern$multitrack_preview$enabled <- TRUE
  params$meta <- params$meta %||% list()
  params$meta$label_blind <- isTRUE(label_blind)
  n <- 60L
  dat <- data.frame(
    idx = seq_len(n),
    timestamp_sec = c(0, cumsum(rep(0.04, n - 1L))),
    ISI_sec = c(NA_real_, rep(0.04, n - 1L)),
    pattern_manual = rep("", n),
    pattern_manual_negative = rep("", n),
    pattern_auto = rep("", n),
    stringsAsFactors = FALSE
  )
  phase1a <- SpikeTrainPatternDetector:::stpd_multitrack_shadow_select(
    pool,
    patterns = unique(as.character(pool$final_label)),
    params = params
  )
  phase1b <- SpikeTrainPatternDetector:::stpd_multitrack_compatibility_shadow(
    phase1a, dat = dat, params = params, variable_params = NULL,
    min_isi_sec = 0.001
  )
  attr(dat, "multitrack_shadow") <- phase1a
  attr(dat, "multitrack_compatibility_shadow") <- phase1b
  trains <- list(train_1 = dat)
  if (isTRUE(two_trains)) trains$train_2 <- dat
  params_hash <- stpd_params_hash(params)
  selected <- sort(names(trains), method = "radix")
  run_id <- "phase2c_validation_test_run"
  ds <- list(
    trains = trains,
    params_effective = params,
    results = list(run_metadata = data.frame(
      run_id = run_id,
      params_hash = params_hash,
      label_blind = isTRUE(label_blind),
      detection_mode = if (isTRUE(label_blind)) "label_blind" else "manual_aware",
      selected_trains = paste(selected, collapse = ";"),
      stringsAsFactors = FALSE
    )),
    meta = list(display_name = "phase2c_validation_fixture", unit_in = "s")
  )
  ds$results$multitrack_preview <-
    SpikeTrainPatternDetector:::stpd_multitrack_preview_materialize(
      ds, params, selected_trains = selected,
      run_id = run_id, params_hash = params_hash
    )
  list(ds = ds, params = params)
}

phase2c_validation_truth <- function(
    id, train, track, label, start, end) {
  data.frame(
    truth_interval_id = as.character(id),
    train = as.character(train),
    semantic_track = as.character(track),
    label = as.character(label),
    start_isi = as.integer(start),
    end_isi = as.integer(end),
    stringsAsFactors = FALSE
  )
}

phase2c_validation_default_truth <- function(trains = "train_1") {
  rows <- lapply(trains, function(train) {
    dplyr::bind_rows(
      phase2c_validation_truth(
        paste0(train, "_burst_truth"), train, "event", "burst", 20L, 23L
      ),
      phase2c_validation_truth(
        paste0(train, "_hfs_truth"), train, "state",
        "high_frequency_spiking", 10L, 45L
      )
    )
  })
  dplyr::bind_rows(rows)
}

phase2c_validation_expect_code <- function(expression, code) {
  error <- tryCatch(expression, error = function(e) e)
  expect_s3_class(error, "stpd_multitrack_validation_error")
  expect_identical(error$code, code)
  invisible(error)
}

phase2c_validation_automatic <- function(
    ds, truth, iou_threshold = 0.5, selected_trains = NULL) {
  stpd_multitrack_validation(
    ds,
    truth,
    prediction_source = "automatic_preview",
    truth_role = "independent_reference",
    selected_trains = selected_trains,
    iou_threshold = iou_threshold
  )
}

phase2c_validation_final <- function(
    ds, truth, iou_threshold = 0.5,
    truth_role = "adjudicated_reference", selected_trains = NULL) {
  stpd_multitrack_validation(
    ds,
    truth,
    prediction_source = "phase2b_final",
    truth_role = truth_role,
    selected_trains = selected_trains,
    iou_threshold = iou_threshold
  )
}

test_that("Event Burst and State HFS coexist as primary true positives", {
  fixture <- phase2c_validation_fixture()
  truth <- phase2c_validation_default_truth()
  ds_before <- serialize(fixture$ds, NULL)
  truth_before <- serialize(truth, NULL)

  result <- phase2c_validation_automatic(fixture$ds, truth)

  expect_s3_class(result, "stpd_multitrack_validation")
  expect_identical(names(result), c(
    "metadata", "primary_predictions", "truth_intervals", "matches",
    "metrics_by_label", "metrics_by_track", "coverage_metrics",
    "review_candidates", "review_summary"
  ))
  expect_identical(
    result$metadata$matching_rule,
    "ordered_dp_max_cardinality_then_total_iou_v1"
  )
  expect_true(result$metadata$source_label_blind)
  expect_false(result$metadata$source_authoritative)
  expect_match(result$metadata$prediction_artifact_sha256, "^[0-9a-f]{64}$")
  expect_match(result$metadata$truth_sha256, "^[0-9a-f]{64}$")
  expect_identical(result$metadata$selected_trains, "train_1")
  expect_identical(
    result$metadata$split_policy,
    "split_table_deferred_v1__explicit_train_subset_only"
  )
  expect_identical(
    result$metadata$metric_aggregation,
    "track_separate__never_pool_event_state_gap"
  )
  expect_identical(
    result$metadata$uncertainty_policy,
    "raw_counts_only__cluster_ci_deferred"
  )
  expect_identical(
    result$metadata$review_handling,
    "target_coverage_only__excluded_from_primary_precision_recall_f1"
  )
  expect_equal(nrow(result$primary_predictions), 2L)
  expect_setequal(
    paste(result$primary_predictions$semantic_track,
          result$primary_predictions$label, sep = "/"),
    c("event/burst", "state/high_frequency_spiking")
  )
  expect_equal(nrow(result$matches), 2L)
  expect_setequal(result$matches$semantic_track, c("event", "state"))
  expect_true(all(result$matches$iou == 1))

  primary_track <- result$metrics_by_track[
    result$metrics_by_track$semantic_track %in% c("event", "state"),
    , drop = FALSE
  ]
  expect_identical(primary_track$matched_n, c(1L, 1L))
  expect_true(all(primary_track$precision == 1))
  expect_true(all(primary_track$recall == 1))

  expect_equal(nrow(result$review_candidates), 2L)
  expect_true(all(result$review_candidates$label == "possible_burst"))
  expect_true(all(result$review_candidates$target_track == "event"))
  expect_true(all(result$review_candidates$target_label == "burst"))
  expect_setequal(
    result$review_candidates$review_status,
    c("pending_review", "inactive_review")
  )
  expect_false(any(
    result$primary_predictions$prediction_interval_id %in%
      result$review_candidates$review_interval_id
  ))
  expect_identical(result$review_summary$review_candidate_n, 2L)
  expect_identical(result$review_summary$active_candidate_n, 1L)
  expect_identical(result$review_summary$inactive_candidate_n, 1L)
  expect_identical(result$review_summary$pending_candidate_n, 1L)
  expect_identical(result$review_summary$confirmed_candidate_n, 0L)
  expect_identical(
    result$review_summary$truth_target_at_iou_threshold_by_pending_n, 1L
  )
  expect_identical(serialize(fixture$ds, NULL), ds_before)
  expect_identical(serialize(truth, NULL), truth_before)
})

test_that("ordered DP maximizes match count before total IoU", {
  pool <- dplyr::bind_rows(
    phase2c_validation_candidate(
      "p1", "burst", 2L, 10L, score = 20, priority = 1200
    ),
    phase2c_validation_candidate(
      "p2", "burst", 11L, 19L, score = 20, priority = 1200
    )
  )
  fixture <- phase2c_validation_fixture(pool = pool)
  truth <- dplyr::bind_rows(
    phase2c_validation_truth("t1", "train_1", "event", "burst", 2L, 3L),
    phase2c_validation_truth("t2", "train_1", "event", "burst", 4L, 12L)
  )

  result <- phase2c_validation_automatic(
    fixture$ds, truth, iou_threshold = 0.1
  )
  expect_equal(nrow(result$matches), 2L)
  matches <- result$matches[order(result$matches$prediction_start_isi),
                            , drop = FALSE]
  # A highest-IoU-first greedy matcher takes p1->t2 (IoU 7/11), leaving
  # only one match. Ordered DP takes p1->t1 and p2->t2 to obtain two.
  expect_identical(matches$prediction_start_isi, c(2L, 11L))
  expect_identical(matches$truth_start_isi, c(2L, 4L))
  expect_equal(matches$iou, c(2 / 9, 2 / 16))
  burst_metrics <- result$metrics_by_label[
    result$metrics_by_label$semantic_track == "event" &
      result$metrics_by_label$label == "burst", , drop = FALSE
  ]
  expect_identical(burst_metrics$matched_n, 2L)
  expect_identical(burst_metrics$false_positive_n, 0L)
  expect_identical(burst_metrics$false_negative_n, 0L)
})

test_that("imperfect predictions produce literal interval and ISI-coverage metrics", {
  pool <- dplyr::bind_rows(
    phase2c_validation_candidate(
      "matched_prediction", "burst", 2L, 4L, score = 20, priority = 1200
    ),
    phase2c_validation_candidate(
      "false_positive_prediction", "burst", 20L, 22L,
      score = 20, priority = 1200
    )
  )
  fixture <- phase2c_validation_fixture(pool = pool)
  truth <- dplyr::bind_rows(
    phase2c_validation_truth(
      "matched_truth", "train_1", "event", "burst", 2L, 4L
    ),
    phase2c_validation_truth(
      "missed_truth_a", "train_1", "event", "burst", 8L, 11L
    ),
    phase2c_validation_truth(
      "missed_truth_b", "train_1", "event", "burst", 30L, 31L
    )
  )

  result <- phase2c_validation_automatic(fixture$ds, truth)
  metric <- result$metrics_by_label[
    result$metrics_by_label$semantic_track == "event" &
      result$metrics_by_label$label == "burst", , drop = FALSE
  ]
  expect_identical(metric$predicted_n, 2L)
  expect_identical(metric$truth_n, 3L)
  expect_identical(metric$matched_n, 1L)
  expect_identical(metric$false_positive_n, 1L)
  expect_identical(metric$false_negative_n, 2L)
  expect_equal(metric$precision, 1 / 2)
  expect_equal(metric$recall, 1 / 3)
  expect_equal(metric$f1, 2 / 5)
  expect_equal(metric$mean_iou, 1)

  track_metric <- result$metrics_by_track[
    result$metrics_by_track$semantic_track == "event", , drop = FALSE
  ]
  expect_identical(track_metric$predicted_n, 2L)
  expect_identical(track_metric$truth_n, 3L)
  expect_identical(track_metric$matched_n, 1L)
  expect_identical(track_metric$false_positive_n, 1L)
  expect_identical(track_metric$false_negative_n, 2L)
  expect_equal(track_metric$precision, 1 / 2)
  expect_equal(track_metric$recall, 1 / 3)
  expect_equal(track_metric$f1, 2 / 5)

  coverage <- result$coverage_metrics[
    result$coverage_metrics$semantic_track == "event" &
      result$coverage_metrics$label == "burst", , drop = FALSE
  ]
  expect_identical(coverage$predicted_isi_n, 6L)
  expect_identical(coverage$truth_isi_n, 9L)
  expect_identical(coverage$overlap_isi_n, 3L)
  expect_identical(coverage$union_isi_n, 12L)
  expect_equal(coverage$isi_precision, 1 / 2)
  expect_equal(coverage$isi_recall, 1 / 3)
  expect_equal(coverage$isi_f1, 2 / 5)
  expect_equal(coverage$isi_iou, 1 / 4)
})

test_that("truth and source hashes are exact and truth-sensitive", {
  fixture <- phase2c_validation_fixture()
  truth <- phase2c_validation_default_truth()
  first <- phase2c_validation_automatic(fixture$ds, truth)
  expected_truth_hash <- SpikeTrainPatternDetector:::stpd_multitrack_review_table_hash(
    first$truth_intervals
  )
  expected_source_hash <- SpikeTrainPatternDetector:::stpd_multitrack_preview_table_hash(
    fixture$ds$results$multitrack_preview$table_manifest
  )
  expect_identical(first$metadata$truth_sha256, expected_truth_hash)
  expect_identical(
    first$metadata$prediction_artifact_sha256, expected_source_hash
  )

  changed_truth <- truth
  changed_truth$start_isi[1] <- 19L
  changed_truth$end_isi[1] <- 23L
  changed <- phase2c_validation_automatic(fixture$ds, changed_truth)
  expect_false(identical(changed$metadata$truth_sha256,
                         first$metadata$truth_sha256))
  expect_identical(changed$metadata$prediction_artifact_sha256,
                   first$metadata$prediction_artifact_sha256)
})

test_that("ordered DP resolves equal-IoU ties by stable pair identity", {
  pool <- phase2c_validation_candidate(
    "p_tie", "burst", 10L, 19L, score = 20, priority = 1200
  )
  fixture <- phase2c_validation_fixture(pool = pool)
  truth <- dplyr::bind_rows(
    phase2c_validation_truth(
      "truth_z", "train_1", "event", "burst", 10L, 14L
    ),
    phase2c_validation_truth(
      "truth_a", "train_1", "event", "burst", 15L, 19L
    )
  )
  first <- phase2c_validation_automatic(
    fixture$ds, truth, iou_threshold = 0.5
  )
  second <- phase2c_validation_automatic(
    fixture$ds, truth[2:1, , drop = FALSE], iou_threshold = 0.5
  )
  expect_identical(nrow(first$matches), 1L)
  expect_identical(first$matches$truth_interval_id, "truth_a")
  expect_equal(first$matches$iou, 0.5)
  expect_identical(second, first)
})

test_that("automatic scoring fails closed for missing, stale, or manual-aware sources", {
  truth <- phase2c_validation_default_truth()
  fixture <- phase2c_validation_fixture()

  phase2c_validation_expect_code(
    stpd_multitrack_validation(
      fixture$ds, truth,
      truth_role = "independent_reference"
    ),
    "prediction_source_missing"
  )
  phase2c_validation_expect_code(
    stpd_multitrack_validation(
      fixture$ds, truth,
      prediction_source = "automatic_preview",
      truth_role = "adjudicated_reference"
    ),
    "validation_truth_role_contract_invalid"
  )

  missing <- fixture$ds
  missing$results$multitrack_preview <- NULL
  phase2c_validation_expect_code(
    phase2c_validation_automatic(missing, truth),
    "automatic_preview_missing"
  )

  manual_aware <- phase2c_validation_fixture(label_blind = FALSE)
  phase2c_validation_expect_code(
    phase2c_validation_automatic(manual_aware$ds, truth),
    "automatic_preview_not_label_blind"
  )

  tampered <- fixture$ds
  tampered$results$multitrack_preview$intervals$label[1] <- "long_burst"
  error <- phase2c_validation_expect_code(
    phase2c_validation_automatic(tampered, truth),
    "automatic_preview_invalid"
  )
  expect_identical(error$source_code, "preview_manifest_payload_mismatch")
})

test_that("validation rejects malformed source, threshold, and truth contracts", {
  fixture <- phase2c_validation_fixture()
  truth <- phase2c_validation_default_truth()

  phase2c_validation_expect_code(
    stpd_multitrack_validation(
      fixture$ds, truth, prediction_source = "legacy_events",
      truth_role = "independent_reference"
    ),
    "prediction_source_invalid"
  )
  phase2c_validation_expect_code(
    phase2c_validation_automatic(fixture$ds, truth, iou_threshold = 0),
    "iou_threshold_invalid"
  )

  wrong_type <- truth
  wrong_type$start_isi <- as.numeric(wrong_type$start_isi)
  phase2c_validation_expect_code(
    phase2c_validation_automatic(fixture$ds, wrong_type),
    "truth_schema_invalid"
  )
  duplicate_id <- truth
  duplicate_id$truth_interval_id[2] <- duplicate_id$truth_interval_id[1]
  phase2c_validation_expect_code(
    phase2c_validation_automatic(fixture$ds, duplicate_id),
    "truth_identity_invalid"
  )
  invalid_track <- truth
  invalid_track$semantic_track[1] <- "review"
  phase2c_validation_expect_code(
    phase2c_validation_automatic(fixture$ds, invalid_track),
    "truth_track_invalid"
  )
  missing_train <- truth
  missing_train$train[1] <- "ghost_train"
  phase2c_validation_expect_code(
    phase2c_validation_automatic(fixture$ds, missing_train),
    "truth_parent_train_missing"
  )
})

test_that("truth ontology, parent geometry, and within-track isolation are strict", {
  fixture <- phase2c_validation_fixture()
  truth <- phase2c_validation_default_truth()

  wrong_track <- truth
  wrong_track$semantic_track[1] <- "state"
  phase2c_validation_expect_code(
    phase2c_validation_automatic(fixture$ds, wrong_track),
    "truth_label_track_mismatch"
  )

  outside <- truth
  outside$end_isi[1] <- 61L
  phase2c_validation_expect_code(
    phase2c_validation_automatic(fixture$ds, outside),
    "truth_parent_geometry_invalid"
  )

  overlapping <- dplyr::bind_rows(
    phase2c_validation_truth(
      "burst_a", "train_1", "event", "burst", 10L, 20L
    ),
    phase2c_validation_truth(
      "burst_b", "train_1", "event", "long_burst", 20L, 30L
    )
  )
  phase2c_validation_expect_code(
    phase2c_validation_automatic(fixture$ds, overlapping),
    "truth_within_track_overlap"
  )

  # The same geometry on different semantic tracks is intentionally valid.
  cross_track <- dplyr::bind_rows(
    phase2c_validation_truth(
      "event_overlap", "train_1", "event", "burst", 20L, 23L
    ),
    phase2c_validation_truth(
      "state_overlap", "train_1", "state",
      "high_frequency_spiking", 20L, 23L
    )
  )
  expect_silent(phase2c_validation_automatic(fixture$ds, cross_track))
})

test_that("Phase 2B final scoring reports adjudicated agreement without duplicate Burst", {
  fixture <- phase2c_validation_fixture()
  truth <- phase2c_validation_default_truth()
  preview_intervals <- fixture$ds$results$multitrack_preview$intervals
  review <- preview_intervals[
    preview_intervals$semantic_track == "review" &
      preview_intervals$active_in_preview,
    , drop = FALSE
  ]
  request <- data.frame(
    train = review$train,
    source_review_interval_id = review$interval_id,
    stringsAsFactors = FALSE
  )
  preflight <- stpd_multitrack_review_preflight(
    fixture$ds, request, "confirm"
  )
  confirmed <- stpd_multitrack_review_confirm(
    fixture$ds, request, preflight$precondition_sha256,
    reviewer = "phase2c_reviewer",
    reason = "synthetic adjudicated agreement fixture",
    operation_id = "phase2c_validation_confirm"
  )
  confirmed_before <- serialize(confirmed$dataset, NULL)

  result <- phase2c_validation_final(confirmed$dataset, truth)
  expect_true(result$metadata$source_authoritative)
  expect_identical(result$metadata$estimand, "adjudicated_agreement")
  expect_equal(nrow(result$matches), 2L)
  expect_equal(sum(
    result$primary_predictions$semantic_track == "event" &
      result$primary_predictions$label == "burst"
  ), 1L)
  expect_equal(nrow(result$review_candidates), 2L)
  expect_setequal(
    result$review_candidates$review_status,
    c("confirmed", "inactive_review")
  )
  expect_identical(result$review_summary$review_candidate_n, 2L)
  expect_identical(result$review_summary$active_candidate_n, 1L)
  expect_identical(result$review_summary$inactive_candidate_n, 1L)
  expect_identical(result$review_summary$pending_candidate_n, 0L)
  expect_identical(result$review_summary$confirmed_candidate_n, 1L)
  expect_identical(
    result$review_summary$truth_target_reached_by_any_review_n, 1L
  )
  expect_identical(serialize(confirmed$dataset, NULL), confirmed_before)

  independent <- phase2c_validation_final(
    confirmed$dataset, truth, truth_role = "independent_reference"
  )
  expect_identical(independent$metadata$estimand, "adjudicated_agreement")
  expect_identical(independent$metadata$truth_role, "independent_reference")

  tampered <- confirmed$dataset
  key <- SpikeTrainPatternDetector:::stpd_multitrack_review_result_key()
  event_row <- which(
    tampered$results[[key]]$final_intervals$semantic_track == "event" &
      tampered$results[[key]]$final_intervals$label == "burst"
  )[1]
  tampered$results[[key]]$final_intervals$label[event_row] <- "long_burst"
  invalid <- phase2c_validation_expect_code(
    phase2c_validation_final(tampered, truth),
    "phase2b_final_invalid"
  )
  expect_identical(invalid$source_code, "review_manifest_payload_mismatch")

  phase2c_validation_expect_code(
    phase2c_validation_final(fixture$ds, truth),
    "phase2b_final_missing"
  )
  stale <- confirmed$dataset
  stale$results[[key]] <-
    SpikeTrainPatternDetector:::stpd_multitrack_review_stale_product(
      confirmed$product, "phase2c stale fixture"
    )
  phase2c_validation_expect_code(
    phase2c_validation_final(stale, truth),
    "phase2b_final_not_active"
  )
})

test_that("multi-train results are permutation invariant and inputs are immutable", {
  fixture <- phase2c_validation_fixture(two_trains = TRUE)
  truth <- phase2c_validation_default_truth(c("train_1", "train_2"))
  permuted <- truth[c(4L, 2L, 3L, 1L), , drop = FALSE]
  rownames(permuted) <- NULL
  ds_before <- serialize(fixture$ds, NULL)
  truth_before <- serialize(truth, NULL)
  permuted_before <- serialize(permuted, NULL)

  first <- phase2c_validation_automatic(fixture$ds, truth)
  second <- phase2c_validation_automatic(fixture$ds, permuted)

  expect_identical(first, second)
  expect_equal(nrow(first$matches), 4L)
  expect_setequal(first$matches$train, c("train_1", "train_2"))
  expect_identical(first$review_summary$review_candidate_n, 4L)
  expect_identical(first$review_summary$active_candidate_n, 2L)
  expect_identical(first$review_summary$inactive_candidate_n, 2L)
  expect_identical(first$review_summary$pending_candidate_n, 2L)
  expect_identical(first$review_summary$truth_target_n, 2L)
  expect_identical(
    first$review_summary$truth_target_at_iou_threshold_by_pending_n, 2L
  )
  expect_identical(serialize(fixture$ds, NULL), ds_before)
  expect_identical(serialize(truth, NULL), truth_before)
  expect_identical(serialize(permuted, NULL), permuted_before)

  train_1_truth <- truth[truth$train == "train_1", , drop = FALSE]
  scoped <- phase2c_validation_automatic(
    fixture$ds, train_1_truth, selected_trains = "train_1"
  )
  expect_identical(scoped$metadata$selected_train_n, 1L)
  expect_identical(scoped$metadata$selected_trains, "train_1")
  expect_equal(nrow(scoped$matches), 2L)
  phase2c_validation_expect_code(
    phase2c_validation_automatic(
      fixture$ds, truth, selected_trains = "train_1"
    ),
    "truth_outside_selected_scope"
  )
  phase2c_validation_expect_code(
    phase2c_validation_automatic(
      fixture$ds, train_1_truth, selected_trains = "train_3"
    ),
    "selected_train_scope_invalid"
  )
  expect_false(
    "split_table" %in% names(formals(stpd_multitrack_validation))
  )

  preview <- fixture$ds$results$multitrack_preview$intervals
  train_1_review <- preview[
    preview$train == "train_1" & preview$semantic_track == "review" &
      preview$active_in_preview,
    , drop = FALSE
  ]
  request <- data.frame(
    train = train_1_review$train,
    source_review_interval_id = train_1_review$interval_id,
    stringsAsFactors = FALSE
  )
  preflight <- stpd_multitrack_review_preflight(
    fixture$ds, request, "confirm"
  )
  confirmed <- stpd_multitrack_review_confirm(
    fixture$ds, request, preflight$precondition_sha256,
    reviewer = "phase2c_multitrain_reviewer",
    reason = "verify train-qualified final Review status join",
    operation_id = "phase2c_multitrain_confirm"
  )
  final <- phase2c_validation_final(confirmed$dataset, truth)
  active_review <- final$review_candidates[
    final$review_candidates$active_in_automatic_preview, , drop = FALSE
  ]
  expect_identical(
    active_review$review_status[active_review$train == "train_1"],
    "confirmed"
  )
  expect_identical(
    active_review$review_status[active_review$train == "train_2"],
    "pending_review"
  )
})

test_that("public validation documentation fixes estimands and interpretation boundaries", {
  rd_path <- testthat::test_path(
    "..", "..", "man", "stpd_multitrack_validation.Rd"
  )
  rd <- if (file.exists(rd_path)) {
    tools::parse_Rd(rd_path)
  } else {
    tools::Rd_db(package = "SpikeTrainPatternDetector")[[
      "stpd_multitrack_validation.Rd"
    ]]
  }
  expect_false(is.null(rd))
  rendered <- paste(
    capture.output(tools::Rd2txt(rd)),
    collapse = " "
  )
  rendered <- gsub("[[:space:]]+", " ", rendered)
  expect_match(rendered, "detector_performance", fixed = TRUE)
  expect_match(rendered, "adjudicated_agreement", fixed = TRUE)
  expect_match(rendered, "does not by itself establish biological validity", fixed = TRUE)
  expect_match(rendered, "Neither estimand by itself establishes biological validity", fixed = TRUE)
  expect_match(rendered, "Review target coverage is not Review precision", fixed = TRUE)
  expect_match(rendered, "interval rows are not independent sampling units", fixed = TRUE)
  expect_match(rendered, "Predictions in every selected train enter the denominators", fixed = TRUE)
})
