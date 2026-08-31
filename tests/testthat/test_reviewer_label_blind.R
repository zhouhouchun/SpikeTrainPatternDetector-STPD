reviewer_regular_hfs_dataset <- function() {
  timestamp_sec <- seq(0, 1.2, by = 0.01)
  n <- length(timestamp_sec)
  dat <- data.frame(
    idx = seq_len(n),
    timestamp_sec = timestamp_sec,
    ISI_sec = c(NA_real_, diff(timestamp_sec)),
    pattern_manual = rep("", n),
    pattern_manual_negative = rep("", n),
    pattern_auto = rep("", n),
    stringsAsFactors = FALSE
  )
  SpikeTrainPatternDetector:::make_dataset(
    "reviewer_label_blind", "synthetic", list(train_1 = dat), unit_in = "s"
  )
}

test_that("label-blind AUTO predictions do not depend on manual labels", {
  unlabelled <- reviewer_regular_hfs_dataset()
  labelled <- reviewer_regular_hfs_dataset()
  labelled$trains$train_1$pattern_manual[40:51] <- "burst"

  original_manual <- labelled$trains$train_1$pattern_manual
  original_negative <- labelled$trains$train_1$pattern_manual_negative

  blind_unlabelled <- stpd_detect(
    unlabelled, default_params(), lock_manual = FALSE,
    collect_diagnostics = FALSE, label_blind = TRUE
  )
  blind_labelled <- stpd_detect(
    labelled, default_params(), lock_manual = FALSE,
    collect_diagnostics = FALSE, label_blind = TRUE
  )

  expect_identical(
    as.character(blind_labelled$trains$train_1$pattern_auto),
    as.character(blind_unlabelled$trains$train_1$pattern_auto)
  )
  expect_equal(
    blind_labelled$trains$train_1$auto_score,
    blind_unlabelled$trains$train_1$auto_score
  )
  expect_true(isTRUE(blind_labelled$results$label_blind))
  expect_identical(blind_labelled$results$detection_mode, "label_blind")

  # Manual columns are restored only after all blind detection and validation
  # work is complete, so callers retain their review labels in the returned ds.
  expect_identical(blind_labelled$trains$train_1$pattern_manual, original_manual)
  expect_identical(blind_labelled$trains$train_1$pattern_manual_negative, original_negative)
  expect_identical(labelled$trains$train_1$pattern_manual, original_manual)
  expect_identical(labelled$trains$train_1$pattern_manual_negative, original_negative)

  # Manual-aware behavior remains available, but a manual Burst is an Event
  # overlay and therefore cannot split or delete the continuous HFS State.
  aware_unlabelled <- stpd_detect(
    unlabelled, default_params(), lock_manual = FALSE,
    collect_diagnostics = FALSE, label_blind = FALSE
  )
  aware_labelled <- stpd_detect(
    labelled, default_params(), lock_manual = FALSE,
    collect_diagnostics = FALSE, label_blind = FALSE
  )
  expect_equal(sum(aware_unlabelled$trains$train_1$pattern_auto != ""), 120)
  expect_equal(sum(aware_labelled$trains$train_1$pattern_auto != ""), 120)
  expect_identical(
    as.character(aware_labelled$trains$train_1$pattern_auto),
    as.character(aware_unlabelled$trains$train_1$pattern_auto)
  )
  expect_false(isTRUE(aware_labelled$results$label_blind))
  expect_identical(aware_labelled$results$detection_mode, "manual_aware")
})

test_that("label-blind copies remove only explicitly manual-derived train settings", {
  ds <- reviewer_regular_hfs_dataset()
  ds$train_settings$burst_isi_ranges <- list(
    train_1 = list(source = "manual_burst", high_sec = 0.012),
    external_calibration = list(source = "held_out_calibration", high_sec = 0.011)
  )
  original_settings <- ds$train_settings$burst_isi_ranges

  blinded <- SpikeTrainPatternDetector:::stpd_label_blind_dataset_copy(ds)

  expect_null(blinded$train_settings$burst_isi_ranges$train_1)
  expect_equal(
    blinded$train_settings$burst_isi_ranges$external_calibration,
    original_settings$external_calibration
  )
  expect_identical(ds$train_settings$burst_isi_ranges, original_settings)
  expect_true(all(blinded$trains$train_1$pattern_manual == ""))
  expect_true(all(blinded$trains$train_1$pattern_manual_negative == ""))

  cached <- default_params()
  cached$event_grammar$effective_bands <- list(
    burst = list(seed_upper_sec = 0.012, seed_upper_sec_source = "manual")
  )
  prepared <- SpikeTrainPatternDetector:::stpd_label_blind_prepare_params(cached)
  expect_null(prepared$event_grammar$effective_bands)
})

test_that("label-blind copies remove legacy adjudication without mutating the source", {
  ds <- reviewer_regular_hfs_dataset()
  dat <- ds$trains$train_1
  dat$pattern_auto_original <- dat$pattern_auto
  dat$pattern_manual_before_user_override <- ""
  dat$pattern_manual_negative_before_user_override <- ""
  dat$pattern_user_override <- "burst"
  dat$pattern_user_override_reason <- "review"
  dat$pattern_audit_final <- "burst"
  dat$pattern_audit_id <- "audit_1"
  ds$trains$train_1 <- dat
  ds$results <- list(
    final_audit_history = data.frame(audit_id = "audit_1"),
    final_audit_policy = list(promote_possible = TRUE),
    possible_burst_promotion_audit = data.frame(audit_id = "audit_1"),
    unrelated_detector_result = data.frame(value = 1)
  )

  blinded <- SpikeTrainPatternDetector:::stpd_label_blind_dataset_copy(ds)

  expect_false(any(startsWith(names(blinded$trains$train_1), "pattern_user_override")))
  expect_false(any(startsWith(names(blinded$trains$train_1), "pattern_audit_")))
  expect_false("pattern_auto_original" %in% names(blinded$trains$train_1))
  expect_null(blinded$results$final_audit_history)
  expect_null(blinded$results$final_audit_policy)
  expect_null(blinded$results$possible_burst_promotion_audit)
  expect_equal(blinded$results$unrelated_detector_result$value, 1)

  expect_true("pattern_user_override" %in% names(ds$trains$train_1))
  expect_true("pattern_audit_final" %in% names(ds$trains$train_1))
  expect_equal(ds$results$final_audit_policy$promote_possible, TRUE)
})

test_that("legacy manual-vs-detector report uses a label-blind range-blinded shadow", {
  burst_truth <- reviewer_regular_hfs_dataset()
  tonic_truth <- reviewer_regular_hfs_dataset()
  burst_truth$trains$train_1$pattern_manual[40:51] <- "burst"
  tonic_truth$trains$train_1$pattern_manual[40:51] <- "tonic"

  # Deliberately different cached train settings must not enter the default
  # shadow pass used for the exported validation report.
  burst_truth$train_settings$highfreq_isi_ranges <- list(
    train_1 = list(source = "manual", high_sec = 0.012)
  )
  tonic_truth$train_settings$highfreq_isi_ranges <- list(
    train_1 = list(source = "manual", high_sec = 0.030)
  )

  burst_eval <- SpikeTrainPatternDetector:::evaluate_detector_against_manual(
    burst_truth, default_params(), selected_trains = "train_1"
  )
  tonic_eval <- SpikeTrainPatternDetector:::evaluate_detector_against_manual(
    tonic_truth, default_params(), selected_trains = "train_1"
  )

  expect_gt(nrow(burst_eval$predictions), 0L)
  expect_identical(burst_eval$predictions$prediction, tonic_eval$predictions$prediction)
  expect_true(burst_eval$meta$label_blind_shadow_detection)
  expect_true(burst_eval$meta$manual_labels_used_for_scoring_only)
  expect_false(burst_eval$meta$learned_ranges_used)
  expect_identical(
    burst_eval$meta$threshold_training_scope,
    "range_blinded_shadow_detection"
  )
})
