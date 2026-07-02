make_sensitivity_validation_train <- function() {
  isi <- c(0.10, 0.20, 0.008, 0.012, 0.008, 0.20, 0.10)
  t <- cumsum(c(0, isi))
  manual <- rep("", length(t))
  manual[4:6] <- "burst"
  data.frame(
    idx = seq_along(t),
    timestamp_sec = t,
    ISI_sec = c(NA_real_, diff(t)),
    pattern_manual = manual,
    pattern_auto = rep("", length(t)),
    stringsAsFactors = FALSE
  )
}

make_sensitivity_validation_params <- function() {
  p <- default_params()
  p$event_core$seed_band_upper_sec <- 0.013
  p$spiketrainpattern$burst$seed_upper_sec <- 0.013
  p$event_grammar$threshold_source_mode <- "default"
  p$spiketrainpattern$engine$threshold_source_mode <- "default"
  p
}

test_that("event-level match table separates label confusion from extras", {
  truth <- data.frame(event_id = 1L, train = "t1", pattern = "burst", start_isi = 2L, end_isi = 4L)
  pred <- data.frame(event_id = 1L, train = "t1", pattern = "tonic", start_isi = 2L, end_isi = 4L)
  matches <- stpd_event_level_match_table(pred, truth, iou_min = 0.25)
  expect_true(any(matches$error_type == "label_confusion"))
  expect_true(any(matches$match_status == "false_positive"))
  expect_true(any(matches$match_status == "false_negative"))
  expect_false(any(matches$match_status == "true_positive"))

  extra <- data.frame(event_id = 1L, train = "t1", pattern = "tonic", start_isi = 10L, end_isi = 11L)
  matches_extra <- stpd_event_level_match_table(extra, truth, iou_min = 0.25)
  expect_true(any(matches_extra$error_type == "extra_detector_event"))
  expect_false(any(matches_extra$error_type == "label_confusion"))
})

test_that("event-level validation report produces IoU metrics and exports", {
  ds <- make_dataset(
    "event_validation",
    "synthetic",
    list(train_1 = make_sensitivity_validation_train()),
    unit_in = "s"
  )
  params <- make_sensitivity_validation_params()
  report <- stpd_event_level_validation_report(ds, params, selected_trains = "train_1", iou_min = 0.25)

  expect_true(is.data.frame(report$metrics))
  expect_true(any(report$metrics$pattern == "burst" & report$metrics$true_positive_n >= 1))
  expect_true(any(report$matches$match_status == "true_positive"))
  expect_true(all(c("start_boundary_error_isi", "end_boundary_error_isi", "boundary_abs_error_isi") %in% names(report$matches)))

  out_dir <- tempfile("event_level_validation_export_")
  stpd_event_level_validation_export(report, out_dir)
  expect_true(file.exists(file.path(out_dir, "Event_level_validation_summary.csv")))
  expect_true(file.exists(file.path(out_dir, "Event_level_validation_metrics.csv")))
  expect_true(file.exists(file.path(out_dir, "Manual_detector_event_matches.csv")))
})

test_that("parameter sensitivity scan is dry-run and exports methods records", {
  ds <- make_dataset(
    "parameter_sensitivity",
    "synthetic",
    list(train_1 = make_sensitivity_validation_train(), train_2 = make_sensitivity_validation_train()),
    unit_in = "s"
  )
  ds_before <- ds
  params <- make_sensitivity_validation_params()
  scan <- stpd_parameter_sensitivity_scan(
    ds,
    params,
    selected_trains = c("train_1", "train_2"),
    paths = "event_core.seed_band_upper_sec",
    max_params = 1,
    max_trains = 2,
    relative_step = 0.5,
    iou_min = 0.25,
    permutation_n = 31
  )

  expect_true(is.data.frame(scan$summary))
  expect_true(any(scan$summary$variant_id == "baseline_current"))
  expect_true(any(scan$summary$parameter_path == "event_core.seed_band_upper_sec"))
  expect_true(all(c("macro_precision", "macro_recall", "macro_F1", "changed_event_n") %in% names(scan$summary)))
  expect_true(all(c("sensitivity_raw_p_value", "sensitivity_q_value", "robust_parameter_flag") %in% names(scan$summary)))
  expect_true(is.data.frame(scan$metrics))
  expect_true(is.data.frame(scan$matches))
  expect_true(is.data.frame(scan$train_metrics))
  expect_true(is.data.frame(scan$multiple_comparison_tests))
  expect_identical(ds$trains, ds_before$trains)
  expect_identical(ds$results, ds_before$results)

  out_dir <- tempfile("parameter_sensitivity_export_")
  stpd_parameter_sensitivity_export(scan, out_dir)
  expect_true(file.exists(file.path(out_dir, "Parameter_sensitivity_summary.csv")))
  expect_true(file.exists(file.path(out_dir, "Event_level_validation_metrics.csv")))
  expect_true(file.exists(file.path(out_dir, "Manual_detector_event_matches.csv")))
  expect_true(file.exists(file.path(out_dir, "Parameter_sensitivity_multiple_comparison_tests.csv")))
})
