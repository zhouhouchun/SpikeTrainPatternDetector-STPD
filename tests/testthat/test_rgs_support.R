test_that("RGS auxiliary support exposes Burst, Pause, QC, and fitted thresholds", {
  make_train <- function(offset = 0) {
    isi <- c(rep(0.10, 80L), rep(0.004, 6L), rep(0.10, 70L), 1.2,
             rep(0.10, 80L))
    offset + c(0, cumsum(isi))
  }
  dataset <- SpikeTrainPatternDetector:::make_dataset(
    "rgs-provider-test", "synthetic",
    list(
      t1 = data.frame(timestamp_sec = make_train(0)),
      t2 = data.frame(timestamp_sec = make_train(0.2)),
      t3 = data.frame(timestamp_sec = make_train(0.4))
    )
  )
  support <- stpd_rgs_support_dataset(
    dataset,
    groups = c(t1 = "test", t2 = "test", t3 = "test")
  )
  expect_s3_class(support, "stpd_rgs_support")
  expect_identical(support$method, "robust_gaussian_surprise")
  expect_identical(support$authority_scope, "support_evidence_only")
  expect_identical(support$calibration_mode, "same_data_reference")
  expect_gt(nrow(support$bursts), 0L)
  expect_gt(nrow(support$pauses), 0L)
  expect_true(all(c(
    "method", "start_isi", "end_isi", "start_spike", "end_spike",
    "n_spikes", "duration_sec", "mean_ISI_sec", "median_ISI_sec",
    "inter_event_gap_sec", "inter_event_onset_interval_sec"
  ) %in% names(support$bursts)))
  expect_setequal(
    unique(support$thresholds$threshold_name),
    c("central_mu", "central_sigma", "burst_seed_threshold",
      "pause_seed_threshold", "alpha")
  )
  expect_true(all(support$thresholds$support_role ==
                    "auxiliary_burst_pause_evidence_only"))
  expect_equal(nrow(support$qc), 3L)
})

test_that("RGS supports frozen-reference prediction without changing STPD labels", {
  make_train <- function(offset = 0) {
    isi <- c(rep(0.10, 80L), rep(0.004, 6L), rep(0.10, 70L), 1.2,
             rep(0.10, 80L))
    offset + c(0, cumsum(isi))
  }
  calibration <- SpikeTrainPatternDetector:::make_dataset(
    "rgs-calibration", "synthetic",
    list(c1 = data.frame(timestamp_sec = make_train(0)),
         c2 = data.frame(timestamp_sec = make_train(0.1)))
  )
  target <- SpikeTrainPatternDetector:::make_dataset(
    "rgs-target", "synthetic",
    list(h1 = data.frame(timestamp_sec = make_train(0.2)))
  )
  fit <- stpd_rgs_fit(calibration, groups = c(c1 = "group", c2 = "group"))
  before <- target$results
  support <- stpd_rgs_support_dataset(
    target, groups = c(h1 = "group"), fit = fit
  )
  expect_identical(support$calibration_mode, "frozen_reference_prediction")
  expect_identical(target$results, before)
  expect_gt(nrow(support$bursts), 0L)
  expect_gt(nrow(support$reference_diagnostics), 0L)
})

test_that("RGS support rejects duplicate timestamps and exports audit tables", {
  bad <- SpikeTrainPatternDetector:::make_dataset(
    "rgs-bad", "synthetic",
    list(t1 = data.frame(timestamp_sec = c(0, 0.1, 0.1, 0.2, 0.3)))
  )
  expect_error(stpd_rgs_support_dataset(bad), "not strictly increasing")

  isi <- c(rep(0.10, 80L), rep(0.004, 6L), rep(0.10, 70L), 1.2,
           rep(0.10, 80L))
  x <- c(0, cumsum(isi))
  dataset <- SpikeTrainPatternDetector:::make_dataset(
    "rgs-export", "synthetic",
    list(t1 = data.frame(timestamp_sec = x),
         t2 = data.frame(timestamp_sec = x + 0.2))
  )
  support <- stpd_rgs_support_dataset(dataset)
  out_dir <- tempfile("stpd-rgs-export-")
  on.exit(unlink(out_dir, recursive = TRUE, force = TRUE), add = TRUE)
  stpd_rgs_support_export(support, out_dir)
  expect_true(all(c(
    "RGS_thresholds.csv", "RGS_burst_features.csv",
    "RGS_pause_features.csv", "RGS_reference_diagnostics.csv",
    "RGS_QC.csv", "RGS_analysis.rds"
  ) %in% list.files(out_dir)))
})

test_that("PS and RGS exported tables cannot be mistaken for raw timestamps", {
  for (filename in c("PS_burst_features.csv", "RGS_pause_features.csv")) {
    path <- file.path(tempdir(), filename)
    utils::write.csv(
      data.frame(train = "t1", start_isi = 1L, end_isi = 3L,
                 duration_sec = 0.02),
      path, row.names = FALSE
    )
    expect_error(build_spike_dataset(path, mode = "raw"),
                 "high-confidence derived output")
    unlink(path)
  }
})
