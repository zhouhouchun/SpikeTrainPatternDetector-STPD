make_stat_strength_train <- function(isi = c(0.10, 0.20, 0.008, 0.012, 0.008, 0.20, 0.10),
                                     label = "burst") {
  t <- cumsum(c(0, isi))
  manual <- rep("", length(t))
  manual[4:6] <- label
  data.frame(
    idx = seq_along(t),
    timestamp_sec = t,
    ISI_sec = c(NA_real_, diff(t)),
    pattern_manual = manual,
    pattern_auto = rep("", length(t)),
    auto_score = rep(NA_real_, length(t)),
    stringsAsFactors = FALSE
  )
}

make_stat_strength_params <- function() {
  p <- default_params()
  p$event_core$seed_band_upper_sec <- 0.013
  p$spiketrainpattern$burst$seed_upper_sec <- 0.013
  p$event_grammar$threshold_source_mode <- "default"
  p$spiketrainpattern$engine$threshold_source_mode <- "default"
  p
}

test_that("event metrics expose Wilson and train-cluster bootstrap intervals", {
  truth <- data.frame(
    train = c("t1", "t2"),
    pattern = c("burst", "burst"),
    start_isi = c(2L, 2L),
    end_isi = c(4L, 4L),
    stringsAsFactors = FALSE
  )
  pred <- data.frame(
    train = c("t1", "t2", "t2"),
    pattern = c("burst", "burst", "tonic"),
    start_isi = c(2L, 10L, 2L),
    end_isi = c(4L, 11L, 4L),
    auto_score = c(0.9, 0.2, 0.6),
    stringsAsFactors = FALSE
  )

  metrics <- stpd_event_level_metrics_ci(stpd_event_level_metrics(pred, truth, iou_min = 0.25))
  expect_true(all(c("precision_ci_low", "recall_ci_high", "F1_ci_low") %in% names(metrics)))

  boot <- stpd_event_level_cluster_bootstrap(pred, truth, iou_min = 0.25, n_bootstrap = 8, seed = 123)
  expect_true(is.data.frame(boot$summary))
  expect_true(any(boot$summary$metric == "F1"))
  expect_true(any(boot$summary$pattern == "burst"))

  calibrated <- stpd_score_calibration(pred, truth, iou_min = 0.25, n_bins = 2)
  expect_true(is.data.frame(calibrated$calibration))
  expect_true(any(calibrated$calibration$pattern == "all"))
  expect_true(calibrated$summary$score_is_probability)
})

test_that("threshold freeze resolves only the requested calibration trains", {
  ds <- make_dataset(
    "threshold_freeze",
    "synthetic",
    list(
      calibration = make_stat_strength_train(),
      validation = make_stat_strength_train(c(0.12, 0.18, 0.009, 0.011, 0.009, 0.18, 0.12))
    ),
    unit_in = "s"
  )
  frozen <- stpd_freeze_thresholds_for_trains(ds, make_stat_strength_params(), calibration_trains = "calibration")
  expect_false(isTRUE(frozen$detector$freeze_dataset_thresholds))
  expect_equal(frozen$event_grammar$threshold_training_train_n, 1L)
  expect_equal(frozen$event_grammar$threshold_training_trains, "calibration")
  expect_true(is.data.frame(frozen$event_grammar$threshold_table))
})

test_that("validation report can add calibration freeze, CI, bootstrap, and score calibration", {
  ds <- make_dataset(
    "stat_validation",
    "synthetic",
    list(
      calibration = make_stat_strength_train(),
      validation = make_stat_strength_train(c(0.12, 0.18, 0.009, 0.011, 0.009, 0.18, 0.12))
    ),
    unit_in = "s"
  )
  split <- data.frame(train = c("calibration", "validation"), split = c("calibration", "validation"), stringsAsFactors = FALSE)
  report <- stpd_event_level_validation_report(
    ds,
    make_stat_strength_params(),
    selected_trains = c("calibration", "validation"),
    split_table = split,
    threshold_freeze = "calibration",
    bootstrap_ci = TRUE,
    n_bootstrap = 5,
    bootstrap_seed = 11,
    iou_min = 0.25
  )
  expect_equal(report$meta$threshold_freeze_status, "frozen")
  expect_true(all(c("precision_ci_low", "recall_ci_high") %in% names(report$metrics)))
  expect_true(is.data.frame(report$bootstrap_ci))
  expect_true(is.data.frame(report$score_calibration))
})

test_that("detector-level surrogate false alarm report returns empirical FDR columns", {
  ds <- make_dataset(
    "surrogate_false_alarm",
    "synthetic",
    list(train_1 = make_stat_strength_train()),
    unit_in = "s"
  )
  report <- stpd_detector_surrogate_false_alarm(
    ds,
    make_stat_strength_params(),
    selected_trains = "train_1",
    n_surrogates = 1,
    methods = "isi_permutation",
    seed = 7
  )
  expect_true(is.data.frame(report$summary))
  expect_true(all(c("empirical_p_count_ge_observed", "detector_level_fdr_estimate", "fdr_interpretation", "empirical_q_BH") %in% names(report$summary)))
  expect_true(all(is.na(report$summary$detector_level_fdr_estimate[report$summary$observed_event_n == 0])))
})

test_that("surrogate train generation removes manual positive and negative labels", {
  dat <- make_stat_strength_train()
  dat$pattern_manual_negative <- rep("", nrow(dat))
  dat$pattern_manual_negative[3:4] <- "not_burst"
  sur <- stpd_surrogate_train(dat, method = "isi_permutation")
  expect_true(all(as.character(sur$pattern_manual) == ""))
  expect_true(all(as.character(sur$pattern_manual_negative) == ""))
  expect_true(all(as.character(sur$pattern_auto) == ""))
})

test_that("manual uncertainty reports boundary tolerance and inter-rater agreement", {
  truth <- data.frame(
    train = c("t1", "t1"),
    pattern = c("burst", "manual_uncertain"),
    start_isi = c(2L, 8L),
    end_isi = c(4L, 10L),
    stringsAsFactors = FALSE
  )
  pred <- data.frame(
    train = "t1",
    pattern = "burst",
    start_isi = 2L,
    end_isi = 4L,
    auto_score = 0.8,
    stringsAsFactors = FALSE
  )
  sens <- stpd_boundary_tolerance_sensitivity(pred, truth, iou_grid = c(0.1, 0.5))
  expect_true(all(c("iou_min", "ambiguous_excluded_n") %in% names(sens)))
  expect_true(all(sens$ambiguous_excluded_n == 1L))

  r1 <- truth[1, , drop = FALSE]
  r2 <- data.frame(train = "t1", pattern = "burst", start_isi = 2L, end_isi = 5L, stringsAsFactors = FALSE)
  irr <- stpd_inter_rater_reliability(list(rater_a = r1, rater_b = r2), iou_min = 0.25)
  expect_true(is.data.frame(irr))
  expect_true(all(c("label_kappa", "event_f1_same_label", "mean_iou_matched") %in% names(irr)))
})

test_that("frozen score calibrator fits on calibration and reports validation reliability", {
  truth_cal <- data.frame(train = c("c1", "c2"), pattern = c("burst", "burst"), start_isi = c(2L, 2L), end_isi = c(4L, 4L))
  pred_cal <- data.frame(train = c("c1", "c2", "c2"), pattern = c("burst", "burst", "burst"),
                         start_isi = c(2L, 9L, 2L), end_isi = c(4L, 10L, 4L), auto_score = c(0.9, 0.1, 0.8))
  truth_val <- data.frame(train = "v1", pattern = "burst", start_isi = 2L, end_isi = 4L)
  pred_val <- data.frame(train = c("v1", "v1"), pattern = c("burst", "burst"),
                         start_isi = c(2L, 9L), end_isi = c(4L, 10L), auto_score = c(0.85, 0.2))
  frozen <- stpd_score_calibration_frozen(pred_cal, truth_cal, pred_val, truth_val, method = "platt", n_bins = 2)
  expect_true(is.data.frame(frozen$reliability))
  expect_true("validation_ECE" %in% names(frozen$summary))
  expect_true("calibrated_probability" %in% names(frozen$calibrated_predictions))
  expect_equal(frozen$summary$freeze_policy, "fit_on_calibration_apply_to_validation")
})

test_that("repeated stratified holdout supports nested calibration-only tuning", {
  ds <- make_dataset(
    "repeated_holdout",
    "synthetic",
    list(
      train_1 = make_stat_strength_train(),
      train_2 = make_stat_strength_train(c(0.12, 0.18, 0.009, 0.011, 0.009, 0.18, 0.12)),
      train_3 = make_stat_strength_train(c(0.11, 0.19, 0.008, 0.010, 0.008, 0.19, 0.11)),
      train_4 = make_stat_strength_train(c(0.13, 0.17, 0.009, 0.012, 0.009, 0.17, 0.13))
    ),
    unit_in = "s"
  )
  meta <- data.frame(train = paste0("train_", 1:4), nucleus = c("A", "A", "B", "B"), condition = c("x", "y", "x", "y"))
  rep <- stpd_repeated_train_holdout_validation(
    ds,
    make_stat_strength_params(),
    metadata = meta,
    n_repeats = 2,
    validation_fraction = 0.5,
    nested_tuning = TRUE,
    tuning_paths = "event_core.seed_band_upper_sec",
    max_tuning_params = 1,
    iou_min = 0.25
  )
  expect_true(is.data.frame(rep$splits))
  expect_true(is.data.frame(rep$repeat_summary))
  expect_true(all(c("stratum", "split") %in% names(rep$splits)))
  expect_true(is.data.frame(rep$tuning))
})
