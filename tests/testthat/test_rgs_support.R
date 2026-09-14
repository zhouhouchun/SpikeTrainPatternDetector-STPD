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
      "pause_seed_threshold", "candidate_count_p", "familywise_alpha")
  )
  expect_true(all(support$thresholds$support_role ==
                    "auxiliary_burst_pause_evidence_only"))
  expect_equal(nrow(support$qc), 3L)
})

test_that("RGS family-wise alpha does not change Bonferroni K", {
  make_train <- function(offset = 0) {
    isi <- c(rep(0.10, 80L), rep(0.004, 6L), rep(0.10, 70L), 1.2,
             rep(0.10, 80L))
    offset + c(0, cumsum(isi))
  }
  dataset <- SpikeTrainPatternDetector:::make_dataset(
    "rgs-bonferroni", "synthetic",
    list(t1 = data.frame(timestamp_sec = make_train(0)),
         t2 = data.frame(timestamp_sec = make_train(0.1)))
  )
  common <- stpd_rgs_default_parameters()
  loose <- stpd_rgs_support_dataset(
    dataset, groups = c(t1 = "g", t2 = "g"),
    params = utils::modifyList(common, list(familywise_alpha = 0.05))
  )
  strict <- stpd_rgs_support_dataset(
    dataset, groups = c(t1 = "g", t2 = "g"),
    params = utils::modifyList(common, list(familywise_alpha = 0.01))
  )
  expect_identical(
    loose$burst_candidates$bonferroni_k,
    strict$burst_candidates$bonferroni_k
  )
  expect_identical(
    loose$pause_candidates$bonferroni_k,
    strict$pause_candidates$bonferroni_k
  )
})

test_that("RGS candidate-count threshold changes K independently", {
  candidates <- data.frame(
    seed_isi = c(1L, 10L, 20L),
    start_isi = c(1L, 10L, 20L),
    end_isi = c(3L, 12L, 22L),
    n_isi = 3L,
    n_spikes = 4L,
    duration_sec = 0.01,
    raw_log_p = log(c(0.001, 0.02, 0.20)),
    raw_p = c(0.001, 0.02, 0.20),
    stringsAsFactors = FALSE
  )
  finalize <- getFromNamespace("rgs_finalize_candidates", "SpikeTrainPatternDetector")
  defaults <- stpd_rgs_default_parameters()
  narrow <- finalize(
    candidates,
    utils::modifyList(defaults, list(candidate_count_p = 0.01)),
    "burst"
  )
  wide <- finalize(
    candidates,
    utils::modifyList(defaults, list(candidate_count_p = 0.05)),
    "burst"
  )
  expect_identical(unique(narrow$bonferroni_k), 1L)
  expect_identical(unique(wide$bonferroni_k), 2L)
  expect_identical(
    unique(narrow$bonferroni_k),
    unique(finalize(
      candidates,
      utils::modifyList(defaults, list(
        candidate_count_p = 0.01,
        familywise_alpha = 0.001
      )),
      "burst"
    )$bonferroni_k)
  )
})

test_that("RGS interval probability matches the published Gaussian-sum equation", {
  interval_log_p <- getFromNamespace(
    "rgs_interval_log_p", "SpikeTrainPatternDetector"
  )
  observed <- interval_log_p(
    nlisi = c(-2, -1), start_isi = 1L, end_isi = 2L,
    mu = 0, sigma = 1, event_type = "burst"
  )
  expected <- stats::pnorm(-3, mean = 0, sd = sqrt(2), log.p = TRUE)
  expect_equal(observed, expected, tolerance = 1e-14)
})

test_that("RGS reports scientific estimability and publication mode fails closed", {
  make_train <- function(offset = 0) {
    offset + c(0, cumsum(c(rep(0.10, 80L), rep(0.004, 6L),
                           rep(0.10, 70L), 1.2, rep(0.10, 80L))))
  }
  dataset <- SpikeTrainPatternDetector:::make_dataset(
    "rgs-estimability", "synthetic",
    list(t1 = data.frame(timestamp_sec = make_train(0)),
         t2 = data.frame(timestamp_sec = make_train(0.1)))
  )
  exploratory_params <- utils::modifyList(
    stpd_rgs_default_parameters(),
    list(min_qq_correlation = 1, scientific_mode = "exploratory")
  )
  exploratory <- stpd_rgs_support_dataset(
    dataset, groups = c(t1 = "g", t2 = "g"), params = exploratory_params
  )
  expect_identical(exploratory$scientific_status, "not_estimable")
  publication_params <- utils::modifyList(
    exploratory_params, list(scientific_mode = "publication")
  )
  expect_error(
    stpd_rgs_support_dataset(
      dataset, groups = c(t1 = "g", t2 = "g"), params = publication_params
    ),
    "not estimable"
  )
})

test_that("RGS export is schema-stable and compact by default", {
  make_train <- function(offset = 0) {
    offset + c(0, cumsum(c(rep(0.10, 80L), rep(0.004, 6L),
                           rep(0.10, 70L), 1.2, rep(0.10, 80L))))
  }
  dataset <- SpikeTrainPatternDetector:::make_dataset(
    "rgs-compact-export", "synthetic",
    list(t1 = data.frame(timestamp_sec = make_train(0)),
         t2 = data.frame(timestamp_sec = make_train(0.1)))
  )
  support <- stpd_rgs_support_dataset(
    dataset, groups = c(t1 = "g", t2 = "g")
  )
  out <- tempfile("rgs_export_")
  stpd_rgs_support_export(support, out)
  expected <- c(
    "RGS_thresholds.csv", "RGS_burst_features.csv",
    "RGS_pause_features.csv", "RGS_burst_candidates_audit.csv",
    "RGS_pause_candidates_audit.csv", "RGS_ISI_support.csv",
    "RGS_support_summary.csv", "RGS_reference_diagnostics.csv",
    "RGS_seed_candidate_counts.csv", "RGS_QC.csv",
    "RGS_run_status.csv", "RGS_analysis.rds"
  )
  expect_setequal(list.files(out), expected)
  compact <- readRDS(file.path(out, "RGS_analysis.rds"))
  expect_false("timestamps_sec" %in% names(compact))
  expect_false("training_timestamps_sec" %in% names(compact$fit))
  expect_false(file.exists(file.path(out, "RGS_analysis_full.rds")))
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

test_that("PS and RGS tokens in raw filenames do not trigger a false block", {
  hard_name <- getFromNamespace(
    "stpd_event_grammar_hard_derived_csv_filename",
    "SpikeTrainPatternDetector"
  )
  expect_false(hard_name("subject_PS_raw_timestamps.csv"))
  expect_false(hard_name("patient_RGS_recording.csv"))
  expect_true(hard_name("PS_burst_features.csv"))
  expect_true(hard_name("RGS_pause_features.csv"))
})
