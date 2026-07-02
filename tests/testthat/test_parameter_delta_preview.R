make_delta_preview_train <- function(isi) {
  t <- cumsum(c(0, isi))
  data.frame(
    idx = seq_along(t),
    timestamp_sec = t,
    ISI_sec = c(NA_real_, diff(t)),
    pattern_manual = rep("", length(t)),
    pattern_auto = rep("", length(t)),
    stringsAsFactors = FALSE
  )
}

test_that("parameter delta preview detects burst gained when seed upper is relaxed", {
  ds <- make_dataset(
    "delta_burst",
    "synthetic",
    list(train_1 = make_delta_preview_train(c(0.10, 0.20, 0.008, 0.012, 0.008, 0.20, 0.10))),
    unit_in = "s"
  )
  baseline <- default_params()
  baseline$event_core$seed_band_upper_sec <- 0.006
  baseline$spiketrainpattern$burst$seed_upper_sec <- 0.006
  baseline$event_grammar$threshold_source_mode <- "default"
  baseline$event_grammar$burst_detector_pipeline <- "threshold_resolved_base"
  baseline$spiketrainpattern$engine$threshold_source_mode <- "default"

  current <- baseline
  current$event_core$seed_band_upper_sec <- 0.013
  current$spiketrainpattern$burst$seed_upper_sec <- 0.013

  preview <- stpd_parameter_delta_preview(ds, current, baseline, selected_trains = "train_1", max_trains = 1, iou_min = 0.25)
  expect_true(any(preview$counts$pattern == "burst" & preview$counts$delta_n > 0))
  expect_true(any(preview$event_diff$status == "added_event" & preview$event_diff$current_pattern == "burst"))
  expect_true(any(preview$parameter_changes$path == "event_core.seed_band_upper_sec"))

  overlay <- stpd_parameter_delta_overlay_rows(preview, ds$trains, selected_trains = "train_1")
  expect_true(any(overlay$status == "added_event"))
  expect_true(all(is.finite(overlay$start_align_sec)))
  expect_true(all(overlay$end_align_sec >= overlay$start_align_sec))

  out_dir <- tempfile("delta_preview_export_")
  stpd_parameter_delta_export(preview, out_dir)
  expect_true(file.exists(file.path(out_dir, "Parameter_delta_preview_summary.csv")))
  expect_true(file.exists(file.path(out_dir, "Parameter_delta_preview_counts.csv")))
  expect_true(file.exists(file.path(out_dir, "Parameter_delta_preview_events.csv")))
})

test_that("parameter delta preview detects pause removed when pause threshold is tightened", {
  ds <- make_dataset(
    "delta_pause",
    "synthetic",
    list(train_1 = make_delta_preview_train(c(0.05, 0.05, 0.05, 0.18, 0.05, 0.05, 0.30, 0.05))),
    unit_in = "s"
  )
  baseline <- default_params()
  baseline$detector$patterns_to_run <- "pause"
  baseline$spiketrainpattern$engine$threshold_source_mode <- "default"
  baseline$event_grammar$threshold_source_mode <- "default"
  baseline$pause$T_seed <- 0.10
  baseline$pause$T_strong <- 0.10
  baseline$spiketrainpattern$pause$min_isi_sec <- 0.10
  baseline$spiketrainpattern$pause$max_isi_sec <- 0.10

  current <- baseline
  current$pause$T_seed <- 0.35
  current$pause$T_strong <- 0.35
  current$spiketrainpattern$pause$min_isi_sec <- 0.35
  current$spiketrainpattern$pause$max_isi_sec <- 0.35

  preview <- stpd_parameter_delta_preview(ds, current, baseline, selected_trains = "train_1", max_trains = 1, iou_min = 0.25)
  expect_true(any(preview$counts$pattern == "pause" & preview$counts$delta_n < 0))
  expect_true(any(preview$event_diff$status == "removed_event" & preview$event_diff$baseline_pattern == "pause"))
})

test_that("parameter delta preview is a dry-run and does not pollute formal dataset results", {
  ds <- stpd_golden_test_dataset("middle_burst")
  ds_before <- ds
  current <- default_params()
  current$event_core$seed_band_upper_sec <- 0.012
  current$spiketrainpattern$burst$seed_upper_sec <- 0.012

  preview <- stpd_parameter_delta_preview(ds, current, default_params(), selected_trains = "train_1", max_trains = 1)
  expect_true(is.list(preview))
  expect_identical(ds$trains, ds_before$trains)
  expect_identical(ds$results, ds_before$results)
  expect_identical(ds$params_last, ds_before$params_last)
})

test_that("real-data dry-run preview works on a small loaded subset", {
  path <- system.file("extdata", "STN_2017_subset.csv", package = "SpikeTrainPatternDetector")
  skip_if(!file.exists(path))
  ds <- build_spike_dataset(path, mode = "raw", unit_in = "s")
  ds_before <- ds
  target <- head(names(ds$trains), 1)
  current <- default_params()
  current$event_core$seed_band_upper_sec <- 0.012
  current$spiketrainpattern$burst$seed_upper_sec <- 0.012
  preview <- stpd_parameter_delta_preview(ds, current, default_params(), selected_trains = target, max_trains = 1)
  expect_true(is.data.frame(preview$summary))
  expect_equal(preview$selected_trains, target)
  expect_identical(ds$results, ds_before$results)
})
