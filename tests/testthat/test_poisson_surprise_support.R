test_that("Poisson-surprise uses the declared event-count convention", {
  details <- SpikeTrainPatternDetector:::stpd_ps_surprise_details(
    n_spikes = 4L,
    duration_sec = 0.03,
    baseline_rate_hz = 100,
    log_base = "e"
  )
  expected <- -stats::ppois(
    q = 2L, lambda = 3, lower.tail = FALSE, log.p = TRUE
  )
  expect_equal(details$surprise, expected, tolerance = 1e-12)
  expect_identical(details$counted_events, 3L)
})

test_that("Poisson-surprise detects a compact packet without labeling tonic", {
  spike_times <- c(0, cumsum(c(
    rep(0.10, 50L), rep(0.005, 7L), rep(0.10, 50L)
  )))
  result <- stpd_detect_poisson_surprise(
    spike_times,
    surprise_threshold = 5,
    log_base = "e",
    min_spikes = 3L
  )
  expect_s3_class(result, "poisson_surprise_result")
  expect_gte(nrow(result$bursts), 1L)
  expect_true(all(c(
    "method", "start_isi", "end_isi", "start_spike", "end_spike",
    "n_isi", "n_spikes", "duration_sec", "mean_ISI_sec",
    "median_ISI_sec", "min_ISI_sec", "max_ISI_sec", "q90_ISI_sec",
    "q95_ISI_sec", "pre_ISI_sec", "post_ISI_sec", "firing_rate_Hz",
    "inter_burst_gap_sec", "inter_burst_onset_interval_sec"
  ) %in% names(result$bursts)))
  expect_true(any(
    result$bursts$start_spike_index <= 52L &
      result$bursts$end_spike_index >= 58L
  ))
  expect_true(all(result$bursts$n_spikes >= 3L))
  expect_false(any(c("tonic", "pause", "broad_hfs") %in%
                     names(result$bursts)))

  regular <- stpd_detect_poisson_surprise(
    seq(0, 2, by = 0.01), surprise_threshold = 3, log_base = "10"
  )
  expect_equal(nrow(regular$bursts), 0L)
})

test_that("Poisson-surprise dataset support exposes candidates and audit data", {
  spike_times <- c(0, cumsum(c(
    rep(0.10, 50L), rep(0.005, 7L), rep(0.10, 50L)
  )))
  dataset <- SpikeTrainPatternDetector:::make_dataset(
    "ps-provider-test", "synthetic",
    list(t1 = data.frame(timestamp_sec = spike_times))
  )
  support <- stpd_poisson_surprise_support_dataset(
    dataset,
    surprise_threshold = 5,
    log_base = "e",
    min_spikes = 3L
  )
  expect_s3_class(support, "stpd_poisson_surprise_support")
  expect_identical(support$method, "poisson_surprise")
  expect_identical(support$authority_scope,
                   "support_evidence_only")
  expect_gt(nrow(support$bursts), 0L)
  expect_equal(nrow(support$spike_support), length(spike_times))
  expect_gt(nrow(support$candidates), 0L)
  expect_equal(nrow(support$isi_support), length(spike_times) - 1L)
  expect_true(any(support$isi_support$in_burst))
  expect_setequal(
    support$thresholds$threshold_name,
    c(
      "surprise_threshold", "seed_isi_sec", "rejection_isi_sec",
      "baseline_rate_hz"
    )
  )
  expect_true(all(support$thresholds$support_role ==
                    "auxiliary_burst_evidence_only"))
  expect_error(
    stpd_poisson_surprise_support_dataset(dataset, min_spikes = 2L),
    "min_spikes must be an integer >= 3"
  )
})

test_that("Poisson-surprise reports unambiguous inter-Burst timing", {
  spike_times <- c(0, cumsum(c(
    rep(0.10, 20L), rep(0.004, 5L), rep(0.10, 8L),
    rep(0.004, 5L), rep(0.10, 20L)
  )))
  result <- stpd_detect_poisson_surprise(
    spike_times,
    surprise_threshold = 3,
    seed_isi_sec = 0.01,
    rejection_isi_sec = 0.05,
    log_base = "e"
  )
  expect_gte(nrow(result$bursts), 2L)
  second <- result$bursts[2L, ]
  first <- result$bursts[1L, ]
  expect_equal(second$previous_burst_id, first$burst_id)
  expect_equal(
    second$inter_burst_gap_sec,
    second$start_time_sec - first$end_time_sec
  )
  expect_equal(
    second$inter_burst_onset_interval_sec,
    second$start_time_sec - first$start_time_sec
  )
  expect_equal(second$interburst_interval_sec, second$inter_burst_gap_sec)
  expect_equal(
    result$summary$mean_inter_burst_gap_sec,
    mean(result$bursts$inter_burst_gap_sec[-1L])
  )
  expect_equal(
    result$summary$mean_inter_burst_onset_interval_sec,
    mean(result$bursts$inter_burst_onset_interval_sec[-1L])
  )
})

test_that("Poisson-surprise support exports the standard result tables", {
  spike_times <- c(0, cumsum(c(
    rep(0.10, 20L), rep(0.004, 5L), rep(0.10, 20L)
  )))
  dataset <- SpikeTrainPatternDetector:::make_dataset(
    "ps-export-test", "synthetic",
    list(t1 = data.frame(timestamp_sec = spike_times))
  )
  support <- stpd_poisson_surprise_support_dataset(
    dataset,
    surprise_threshold = 3,
    seed_isi_sec = 0.01,
    rejection_isi_sec = 0.05
  )
  out_dir <- tempfile("stpd-ps-export-")
  on.exit(unlink(out_dir, recursive = TRUE, force = TRUE), add = TRUE)
  stpd_poisson_surprise_support_export(support, out_dir)
  expect_setequal(
    list.files(out_dir),
    c(
      "PS_all_candidates.csv", "PS_burst_features.csv", "PS_ISI_support.csv",
      "PS_QC.csv", "PS_spike_support.csv", "PS_support_summary.csv",
      "PS_thresholds.csv"
    )
  )
  exported <- utils::read.csv(file.path(out_dir, "PS_burst_features.csv"))
  expect_true(all(c(
    "duration_sec", "n_spikes", "n_isi", "inter_burst_gap_sec",
    "inter_burst_onset_interval_sec"
  ) %in% names(exported)))
})
