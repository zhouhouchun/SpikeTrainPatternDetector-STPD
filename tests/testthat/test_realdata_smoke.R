test_that("bundled real-data subset can be read and QC'd", {
  path <- system.file("extdata", "Grechishnikova_STN_2017_subset.csv", package = "SpikeTrainPatternDetector")
  skip_if(!file.exists(path))
  ds <- build_spike_dataset(path, mode = "raw", unit_in = "s")
  expect_true(length(ds$trains) >= 1)
  qc <- run_qc(ds, default_params())
  expect_true(nrow(qc) >= 1)
})

test_that("Grechishnikova RT2D03.89 burst packets are not swallowed by HF spiking", {
  pd_candidates <- c(
    file.path("..", "PD", "STN", "Grechishnikova_STN_2017", "Grechishnikova_STN_2017.csv"),
    file.path("..", "..", "PD", "STN", "Grechishnikova_STN_2017", "Grechishnikova_STN_2017.csv"),
    file.path("..", "..", "..", "PD", "STN", "Grechishnikova_STN_2017", "Grechishnikova_STN_2017.csv")
  )
  path <- pd_candidates[file.exists(pd_candidates)][1]
  skip_if(is.na(path) || !file.exists(path))

  ds <- build_spike_dataset(path, mode = "raw", unit_in = "s")
  params <- default_params()
  burst_train <- "RT2D03.89_fon_nw_minus_7_08_minus_1_1"
  hf_train <- "RT1D-0.15_fon1_1_nw_minus_7_08_minus_1_1"
  skip_if(!all(c(burst_train, hf_train) %in% names(ds$trains)))

  burst_dat <- run_detector_one_train(ds$trains[[burst_train]], params, min_isi_sec = 0.001, train = burst_train)
  burst_events <- extract_events_for_train(burst_dat, source = "auto", train = burst_train, min_isi_sec = 0.001)
  early_hfs <- burst_events[
    burst_events$pattern == "high_frequency_spiking" &
      burst_events$start_isi <= 346L & burst_events$end_isi >= 89L,
    ,
    drop = FALSE
  ]
  early_bursts <- burst_events[
    burst_events$pattern %in% c("burst", "long_burst") &
      burst_events$start_isi >= 89L & burst_events$end_isi <= 346L,
    ,
    drop = FALSE
  ]
  expect_equal(nrow(early_hfs), 0)
  expect_gte(nrow(early_bursts), 6)

  hf_dat <- run_detector_one_train(ds$trains[[hf_train]], params, min_isi_sec = 0.001, train = hf_train)
  hf_events <- extract_events_for_train(hf_dat, source = "auto", train = hf_train, min_isi_sec = 0.001)
  expect_true(any(hf_events$pattern == "high_frequency_spiking" & hf_events$n_isi >= 200L))
})

test_that("bundled subset runs the full native-path detector without crashing", {
  # Regression for a segfault in the native stpd_local_median_cache_c kernel:
  # exercises the C path (percentiles, local-median cache, structure scan)
  # end-to-end on the dataset that previously crashed. A broken native build
  # aborts the test process, so the suite catches it.
  path <- system.file("extdata", "Grechishnikova_STN_2017_subset.csv", package = "SpikeTrainPatternDetector")
  skip_if(!file.exists(path))
  ds <- build_spike_dataset(path, mode = "raw", unit_in = "s")
  res <- stpd_detect(ds, default_params(), selected_trains = names(ds$trains))
  expect_equal(length(res$trains), length(ds$trains))
  expect_true(all(vapply(res$trains, function(t) length(t$pattern_auto) == nrow(t), logical(1))))
  labels <- unlist(lapply(res$trains, function(t) t$pattern_auto[nzchar(t$pattern_auto)]))
  expect_true(length(labels) > 0)
})
