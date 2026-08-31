make_compact_hfs_train <- function(n_spikes = 24L, isi_sec = 0.010) {
  timestamp <- c(0, cumsum(rep(isi_sec, n_spikes - 1L)))
  data.frame(
    idx = seq_along(timestamp),
    timestamp_sec = timestamp,
    ISI_sec = c(NA_real_, diff(timestamp)),
    pattern_manual = rep("", length(timestamp)),
    pattern_manual_negative = rep("", length(timestamp)),
    pattern_auto = rep("", length(timestamp)),
    stringsAsFactors = FALSE
  )
}

make_hfs_train_from_isi <- function(isi) {
  isi <- as.numeric(isi)
  timestamp <- c(0, cumsum(isi))
  data.frame(
    idx = seq_along(timestamp),
    timestamp_sec = timestamp,
    ISI_sec = c(NA_real_, isi),
    pattern_manual = rep("", length(timestamp)),
    pattern_manual_negative = rep("", length(timestamp)),
    pattern_auto = rep("", length(timestamp)),
    stringsAsFactors = FALSE
  )
}

hfs_test_params_and_vp <- function(dat) {
  params <- default_params()
  # These fixtures exercise the explicitly calibrated absolute-HFS route. The
  # automatic histogram route is covered separately and correctly requires an
  # independently slower adjacent background.
  params$spiketrainpattern$engine$threshold_source_mode <- "default"
  params$event_grammar$threshold_source_mode <- "default"
  params$highfreq$spiking_min_spikes <- 20L
  params$highfreq$spiking_min_duration <- 0
  params$highfreq$spiking_background_contrast_min <- 1.35
  params$highfreq$spiking_homogeneous_coverage_min <- 0.80
  params$highfreq$spiking_background_min_isi_count <- 10L
  params$detector$pattern_isi_limits$high_frequency_spiking <- list(
    min_sec = 0.001, max_sec = 0.021
  )
  vp <- stpd_event_grammar_params_impl(dat, params, train = "hfs_gate_test")
  vp$hf_spiking_min_spikes <- 20L
  vp$hf_spiking_min_duration <- 0
  vp$hf_spiking_short_upper <- 0.020
  vp$hf_spiking_q80_max <- 0.020
  vp$hf_spiking_q90_max <- 0.020
  vp$hf_spiking_epoch_bridge <- 0.020
  vp$hf_spiking_hard_break <- 0.040
  vp$pause_thr <- 0.080
  list(params = params, vp = vp)
}

test_that("compact >=20-spike HFS is not rejected by an incompatible duration floor", {
  dat <- make_compact_hfs_train(n_spikes = 24L, isi_sec = 0.010)
  params <- default_params()
  params$spiketrainpattern$engine$threshold_source_mode <- "default"
  params$event_grammar$threshold_source_mode <- "default"
  params$highfreq$spiking_min_spikes <- 20L
  params$highfreq$spiking_min_duration <- 1.0
  params$spiketrainpattern$high_frequency_spiking$min_spikes <- 20L
  params$spiketrainpattern$high_frequency_spiking$min_duration_sec <- 1.0

  out <- run_detector_one_train(
    dat, params, min_isi_sec = 0.001, train = "compact_hfs"
  )
  audit <- attr(out, "candidate_diagnostic_audit")
  hfs <- audit[
    as.character(audit$final_label) == "high_frequency_spiking" &
      as.character(audit$action) == "accept" &
      as.logical(audit$selected_for_auto), , drop = FALSE
  ]

  expect_true(nrow(hfs) >= 1L)
  expect_true(any(as.integer(hfs$n_spikes) >= 20L))
  expect_true(all(as.logical(hfs$hf_spiking_duration_gate_pass) == FALSE))
  expect_true(all(
    as.character(hfs$hf_spiking_duration_gate_role) ==
      "advisory_min_spikes_and_compactness_authoritative"
  ))
  expect_true(all(
    as.character(hfs$hf_spiking_evidence_route) ==
      "absolute_homogeneous_coverage"
  ))
  expect_equal(
    sum(as.character(out$pattern_auto) == "high_frequency_spiking"),
    23L
  )
  posthoc <- attr(out, "posthoc_fragment_audit")
  expect_false(
    is.data.frame(posthoc) && nrow(posthoc) > 0L &&
      any(as.character(posthoc$pattern) == "high_frequency_spiking")
  )
  shadow <- attr(out, "multitrack_shadow")
  selected_shadow <- shadow$selected_candidates
  expect_true(any(
    as.character(selected_shadow$final_label) == "high_frequency_spiking"
  ))
  expect_true(any(
    as.character(selected_shadow$final_label) %in% c("burst", "long_burst")
  ))
})

test_that("the HFS spike-count floor remains authoritative", {
  dat <- make_compact_hfs_train(n_spikes = 19L, isi_sec = 0.010)
  params <- default_params()
  params$spiketrainpattern$engine$threshold_source_mode <- "default"
  params$event_grammar$threshold_source_mode <- "default"
  params$highfreq$spiking_min_spikes <- 20L
  params$highfreq$spiking_min_duration <- 0
  params$spiketrainpattern$high_frequency_spiking$min_spikes <- 20L
  params$spiketrainpattern$high_frequency_spiking$min_duration_sec <- 0

  out <- run_detector_one_train(
    dat, params, min_isi_sec = 0.001, train = "too_short_hfs"
  )
  audit <- attr(out, "candidate_diagnostic_audit")
  expect_false(any(
    as.character(audit$final_label) == "high_frequency_spiking" &
      as.character(audit$action) == "accept" &
      as.logical(audit$selected_for_auto)
  ))
})

test_that("the public HFS defaults use the operational 20-spike floor", {
  params <- default_params()

  expect_identical(as.integer(params$highfreq$spiking_min_spikes), 20L)
  expect_identical(
    as.integer(params$spiketrainpattern$high_frequency_spiking$min_spikes),
    20L
  )
})

test_that("a localized HFS run passes when it is faster than reliable background", {
  dat <- make_hfs_train_from_isi(c(rep(0.050, 30L), rep(0.010, 23L), rep(0.050, 30L)))
  setup <- hfs_test_params_and_vp(dat)
  hfs <- stpd_event_core_detect_hf_spiking(
    dat, setup$params, setup$vp, train = "localized_hfs"
  )

  expect_equal(nrow(hfs), 1L)
  expect_true(isTRUE(hfs$hf_spiking_background_available[[1L]]))
  expect_gte(hfs$hf_spiking_background_contrast[[1L]], 1.35)
  expect_identical(
    as.character(hfs$hf_spiking_evidence_route[[1L]]),
    "absolute_with_supporting_local_background_contrast"
  )
})

test_that("weak local contrast is audited but does not veto absolute HFS", {
  dat <- make_hfs_train_from_isi(c(rep(0.022, 30L), rep(0.019, 23L), rep(0.022, 30L)))
  setup <- hfs_test_params_and_vp(dat)
  hfs <- stpd_event_core_detect_hf_spiking(
    dat, setup$params, setup$vp, train = "weak_local_contrast"
  )

  expect_equal(nrow(hfs), 1L)
  expect_false(isTRUE(hfs$hf_spiking_background_gate_pass[[1L]]))
  expect_identical(
    as.character(hfs$hf_spiking_evidence_route[[1L]]),
    "absolute_despite_weak_local_background_contrast"
  )
})

test_that("HF-like remainder does not replace the calibrated HFS support floor", {
  dat <- make_hfs_train_from_isi(c(rep(0.019, 23L), 0.090, rep(0.019, 23L)))
  setup <- hfs_test_params_and_vp(dat)
  hfs <- stpd_event_core_detect_hf_spiking(
    dat, setup$params, setup$vp, train = "hf_like_remainder"
  )

  # Both sides satisfy the calibrated 20-spike parent-state contract.  The
  # presence of another HF-like run is descriptive evidence, not a reason to
  # replace that contract with a hidden 30-spike requirement.
  expect_equal(nrow(hfs), 2L)
  expect_true(all(as.integer(hfs$n_spikes) == 24L))
  expect_true(all(
    as.integer(hfs$hf_spiking_route_min_spikes_required) == 20L
  ))
  expect_true(all(as.logical(hfs$hf_spiking_route_spike_count_pass)))
  expect_true(all(
    as.character(hfs$hf_spiking_evidence_route) ==
      "absolute_hf_like_remainder"
  ))
})
