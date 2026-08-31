make_hfspiking_pause_train <- function(left_n = 90L, right_n = 90L, fast_isi = 0.010, pause_isi = 0.110) {
  isi <- c(rep(fast_isi, left_n), pause_isi, rep(fast_isi, right_n))
  ts <- c(0, cumsum(isi))
  data.frame(
    idx = seq_along(ts),
    timestamp_sec = ts,
    ISI_sec = c(NA_real_, diff(ts)),
    pattern_manual = rep("", length(ts)),
    pattern_auto = rep("", length(ts)),
    stringsAsFactors = FALSE
  )
}

test_that("pause embedded in an HF-spiking state is detected as a pause", {
  left_n <- 90L
  pause_idx <- left_n + 2L
  dat <- make_hfspiking_pause_train(left_n = left_n)

  params <- default_params()
  params$spiketrainpattern$engine$threshold_source_mode <- "default"
  params$event_grammar$threshold_source_mode <- "default"
  params$spiketrainpattern$pause$min_isi_sec <- 0.100
  params$spiketrainpattern$pause$max_isi_sec <- 0.100
  params$pause$T_seed <- 0.100
  params$pause$T_strong <- 0.100
  params$spiketrainpattern$high_frequency_spiking$tolerated_gap_isi_sec <- 0.120
  params$highfreq$spiking_tolerated_gap_ISI_sec <- 0.120

  out <- run_detector_one_train(dat, params, min_isi_sec = 0.001, train = "hf_pause")
  auto <- as.character(out$pattern_auto)
  expect_equal(auto[pause_idx], "pause")
  expect_true(any(auto[seq_len(pause_idx - 1L)] == "high_frequency_spiking"))
  expect_true(any(auto[seq.int(pause_idx + 1L, length(auto))] == "high_frequency_spiking"))

  audit <- attr(out, "candidate_diagnostic_audit")
  expect_true(is.data.frame(audit))
  selected <- audit[as.logical(audit$selected_for_auto %||% FALSE), , drop = FALSE]
  selected_hfs <- selected[as.character(selected$final_label) == "high_frequency_spiking", , drop = FALSE]
  expect_false(any(
    as.integer(selected_hfs$start_isi) <= pause_idx &
      as.integer(selected_hfs$end_isi) >= pause_idx,
    na.rm = TRUE
  ))
})

test_that("an isolated supra-Max_ISI connector preserves an HFS episode", {
  # Each flank is deliberately shorter than the 20-spike HFS floor.  Detection
  # therefore succeeds only if the moderate connector is considered before the
  # minimum-size gate rather than splitting and deleting both flanks.
  left_n <- 11L
  gap_idx <- left_n + 2L
  dat <- make_hfspiking_pause_train(
    left_n = left_n, right_n = 11L, fast_isi = 0.010, pause_isi = 0.060
  )

  params <- default_params()
  params$spiketrainpattern$engine$threshold_source_mode <- "default"
  params$event_grammar$threshold_source_mode <- "default"
  params$detector$pattern_isi_limits$high_frequency_spiking <- list(min_sec = 0, max_sec = 0.040)
  params$detector$patterns_to_run <- c("high_frequency_spiking", "pause")
  params$highfreq$spiking_tolerated_gap_ISI_sec <- 0.075

  out <- run_detector_one_train(dat, params, min_isi_sec = 0.001, train = "hf_connector")
  auto <- as.character(out$pattern_auto)
  expect_true(all(auto[2:length(auto)] == "high_frequency_spiking"))

  audit <- attr(out, "candidate_diagnostic_audit")
  hfs <- audit[
    as.character(audit$final_label) == "high_frequency_spiking" &
      as.logical(audit$selected_for_auto), , drop = FALSE
  ]
  expect_equal(nrow(hfs), 1L)
  expect_equal(as.integer(hfs$hf_spiking_connector_count), 1L)
  expect_equal(as.numeric(hfs$hf_spiking_connector_max_sec), 0.060, tolerance = 1e-10)
  expect_gt(
    as.numeric(hfs$hf_spiking_tolerated_gap_sec),
    as.numeric(hfs$hf_spiking_pattern_max_ISI_sec)
  )
})

test_that("HF-spiking final gate accepts a bounded minority connector", {
  params <- default_params()
  params$detector$pattern_isi_limits$high_frequency_spiking <- list(min_sec = 0, max_sec = 0.040)
  vals <- c(rep(0.010, 40L), 0.047985, rep(0.010, 40L))

  gate <- getFromNamespace("stpd_pattern_isi_gate_pass", "SpikeTrainPatternDetector")(
    vals,
    "high_frequency_spiking",
    params,
    min_isi_sec = 0.001
  )

  expect_true(gate$pass)
  expect_identical(gate$reason, "pattern_isi_gate_pass")
})

test_that("canonical Pause remains a hard HFS boundary", {
  params <- default_params()
  params$detector$pattern_isi_limits$high_frequency_spiking <- list(min_sec = 0, max_sec = 0.040)
  vals <- c(rep(0.010, 40L), 0.110, rep(0.010, 40L))

  gate <- getFromNamespace("stpd_pattern_isi_gate_pass", "SpikeTrainPatternDetector")(
    vals,
    "high_frequency_spiking",
    params,
    min_isi_sec = 0.001
  )

  expect_false(gate$pass)
  expect_match(gate$reason, "hf_spiking_connector_above_tolerated_gap")
})

test_that("connector allowance is not globally capped by the Pause seed", {
  dat <- make_hfspiking_pause_train(
    left_n = 11L, right_n = 11L, fast_isi = 0.010, pause_isi = 0.055
  )
  params <- default_params()
  params$spiketrainpattern$engine$threshold_source_mode <- "default"
  params$event_grammar$threshold_source_mode <- "default"
  params$detector$patterns_to_run <- c("high_frequency_spiking", "pause")
  params$detector$pattern_isi_limits$high_frequency_spiking <- list(
    min_sec = 0, max_sec = 0.040
  )
  params$spiketrainpattern$pause$min_isi_sec <- 0.050
  params$pause$T_seed <- 0.050
  params$spiketrainpattern$high_frequency_spiking$tolerated_gap_isi_sec <- 0.100
  params$highfreq$spiking_tolerated_gap_ISI_sec <- 0.100

  vp <- stpd_event_grammar_params_impl(
    dat, params, min_isi_sec = 0.001, train = "hf_adaptive_connector"
  )
  vp$hf_spiking_short_upper <- 0.040
  vp$hf_spiking_q80_max <- 0.040
  vp$hf_spiking_q90_max <- 0.040
  vp$hf_spiking_epoch_bridge <- 0.040
  vp$hf_spiking_min_spikes <- 20L
  hfs <- stpd_event_core_detect_hf_spiking(
    dat, params, vp, min_isi_sec = 0.001,
    train = "hf_adaptive_connector", hard_boundaries = data.frame()
  )

  expect_equal(nrow(hfs), 1L)
  expect_equal(as.numeric(hfs$hf_spiking_connector_max_sec), 0.055, tolerance = 1e-10)
  expect_true(is.na(as.numeric(hfs$hf_spiking_connector_pause_allowance_sec)))
  expect_equal(as.numeric(hfs$hf_spiking_pause_seed_audit_sec), 0.050)
  expect_identical(
    as.character(hfs$hf_spiking_pause_boundary_policy),
    "instance_level_canonical_boundary_not_global_pause_seed"
  )
})

test_that("a low contextual Pause seed cannot collapse direct HFS support", {
  dat <- make_hfspiking_pause_train(
    left_n = 24L, right_n = 0L, fast_isi = 0.010, pause_isi = 0.010
  )
  params <- default_params()
  params$event_grammar$threshold_source_mode <- "default"
  params$detector$pattern_isi_limits$high_frequency_spiking <- list(
    min_sec = 0, max_sec = 0.040
  )
  params$highfreq$spiking_min_spikes <- 20L
  setup_vp <- stpd_event_grammar_params_impl(
    dat, params, min_isi_sec = 0.001, train = "low_pause_seed"
  )
  setup_vp$pause_thr <- 0.007
  setup_vp$hf_spiking_short_upper <- 0.040
  setup_vp$hf_spiking_q80_max <- 0.040
  setup_vp$hf_spiking_q90_max <- 0.040
  setup_vp$hf_spiking_epoch_bridge <- 0.050
  setup_vp$hf_spiking_min_spikes <- 20L

  hfs <- stpd_event_core_detect_hf_spiking(
    dat, params, setup_vp, min_isi_sec = 0.001,
    train = "low_pause_seed", hard_boundaries = data.frame()
  )

  expect_equal(nrow(hfs), 1L)
  expect_equal(as.numeric(hfs$hf_spiking_pause_seed_audit_sec), 0.007)
  expect_equal(as.integer(hfs$hf_spiking_canonical_boundary_n), 0L)
})

test_that("an accepted canonical Pause remains an instance-level HFS boundary", {
  dat <- make_hfspiking_pause_train(
    left_n = 35L, right_n = 35L, fast_isi = 0.010, pause_isi = 0.060
  )
  boundary_idx <- 37L
  params <- default_params()
  params$event_grammar$threshold_source_mode <- "default"
  params$detector$pattern_isi_limits$high_frequency_spiking <- list(
    min_sec = 0, max_sec = 0.040
  )
  params$highfreq$spiking_min_spikes <- 20L
  vp <- stpd_event_grammar_params_impl(
    dat, params, min_isi_sec = 0.001, train = "canonical_boundary"
  )
  vp$pause_thr <- 0.007
  vp$hf_spiking_short_upper <- 0.040
  vp$hf_spiking_q80_max <- 0.040
  vp$hf_spiking_q90_max <- 0.040
  vp$hf_spiking_epoch_bridge <- 0.100
  vp$hf_spiking_min_spikes <- 20L
  boundaries <- data.frame(
    start_isi = boundary_idx, end_isi = boundary_idx,
    boundary_kind = "canonical_pause", stringsAsFactors = FALSE
  )

  hfs <- stpd_event_core_detect_hf_spiking(
    dat, params, vp, min_isi_sec = 0.001,
    train = "canonical_boundary", hard_boundaries = boundaries
  )

  expect_equal(nrow(hfs), 2L)
  expect_false(any(
    as.integer(hfs$start_isi) <= boundary_idx &
      as.integer(hfs$end_isi) >= boundary_idx
  ))
  expect_true(all(as.integer(hfs$hf_spiking_canonical_boundary_n) == 1L))
})

test_that("a remote canonical Pause does not raise the HFS spike-count floor", {
  # The non-HF remainder is deliberately close to the HF-like classification
  # boundary.  Removing the remote Pause from the background must not change a
  # valid 21-spike HFS episode into a rejection by escalating the calibrated
  # 20-spike requirement to an unrelated 30-spike fallback.
  isi <- c(
    0.350,
    rep(c(0.030, 0.034, 0.028, 0.036), 18L),
    0.300,
    c(0.0225, 0.0299, 0.0337, 0.0200, 0.0418, 0.0205, 0.0230,
      0.0146, 0.0183, 0.0106, 0.0097, 0.0100, 0.0086, 0.0173,
      0.0210, 0.0095, 0.0263, 0.0227, 0.0212, 0.0136)
  )
  ts <- c(0, cumsum(isi))
  dat <- data.frame(
    idx = seq_along(ts), timestamp_sec = ts,
    ISI_sec = c(NA_real_, isi), pattern_manual = "", pattern_auto = "",
    stringsAsFactors = FALSE
  )
  params <- default_params()
  params$event_grammar$threshold_source_mode <- "default"
  params$detector$pattern_isi_limits$high_frequency_spiking <- list(
    min_sec = 0, max_sec = 0.0383
  )
  params$highfreq$spiking_min_spikes <- 20L
  vp <- stpd_event_grammar_params_impl(
    dat, params, min_isi_sec = 0.001, train = "remote_pause"
  )
  vp$hf_spiking_short_upper <- 0.0383
  vp$hf_spiking_q80_max <- 0.0383
  vp$hf_spiking_q90_max <- 0.0383
  vp$hf_spiking_epoch_bridge <- 0.0509
  vp$hf_spiking_min_spikes <- 20L
  pause_row <- 75L

  without_boundary <- stpd_event_core_detect_hf_spiking(
    dat, params, vp, min_isi_sec = 0.001,
    train = "remote_pause", hard_boundaries = data.frame()
  )
  with_boundary <- stpd_event_core_detect_hf_spiking(
    dat, params, vp, min_isi_sec = 0.001, train = "remote_pause",
    hard_boundaries = data.frame(
      start_isi = pause_row, end_isi = pause_row,
      boundary_kind = "canonical_pause", stringsAsFactors = FALSE
    )
  )

  target_without <- without_boundary[
    without_boundary$start_isi > pause_row, , drop = FALSE
  ]
  target_with <- with_boundary[
    with_boundary$start_isi > pause_row, , drop = FALSE
  ]
  expect_equal(nrow(target_without), 1L)
  expect_equal(nrow(target_with), 1L)
  expect_equal(target_with$start_isi, target_without$start_isi)
  expect_equal(target_with$end_isi, target_without$end_isi)
  expect_identical(
    as.integer(target_with$hf_spiking_route_min_spikes_required), 20L
  )
  expect_true(target_with$hf_spiking_route_spike_count_pass)
})

test_that("HFS connector policy is scale-aware but remains below Pause", {
  policy <- stpd_hfs_connector_policy(
    default_params(), direct_max_sec = 0.300,
    pause_break_sec = 1.800, epoch_bridge_sec = 0.320
  )

  expect_gte(policy$effective_tolerated_gap_sec, 0.300)
  expect_lte(policy$effective_tolerated_gap_sec, 0.480)
  expect_lt(policy$effective_tolerated_gap_sec, 1.800)
})

test_that("too many consecutive moderate gaps do not create HFS", {
  isi <- c(rep(0.010, 11L), rep(0.060, 4L), rep(0.010, 11L))
  ts <- c(0, cumsum(isi))
  dat <- data.frame(
    idx = seq_along(ts),
    timestamp_sec = ts,
    ISI_sec = c(NA_real_, isi),
    pattern_manual = rep("", length(ts)),
    pattern_auto = rep("", length(ts)),
    stringsAsFactors = FALSE
  )
  params <- default_params()
  params$spiketrainpattern$engine$threshold_source_mode <- "default"
  params$event_grammar$threshold_source_mode <- "default"
  params$detector$patterns_to_run <- c("high_frequency_spiking", "pause")
  params$detector$pattern_isi_limits$high_frequency_spiking <- list(
    min_sec = 0, max_sec = 0.040
  )
  params$highfreq$spiking_tolerated_gap_ISI_sec <- 0.075
  params$highfreq$spiking_max_consecutive_large_isi <- 3L

  out <- run_detector_one_train(
    dat, params, min_isi_sec = 0.001, train = "hf_too_many_connectors"
  )
  expect_false(any(as.character(out$pattern_auto) == "high_frequency_spiking"))
})
