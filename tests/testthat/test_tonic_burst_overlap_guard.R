make_tonic_burst_overlap_train <- function(tonic_isi = 0.032, auto_burst = TRUE) {
  isi <- c(0.030, 0.030, 0.030, 0.200, rep(tonic_isi, 6L), 0.200)
  ts <- c(0, cumsum(isi))
  dat <- data.frame(
    idx = seq_along(ts),
    timestamp_sec = ts,
    ISI_sec = c(NA_real_, diff(ts)),
    pattern_manual = rep("", length(ts)),
    pattern_auto = rep("", length(ts)),
    stringsAsFactors = FALSE
  )
  if (isTRUE(auto_burst)) dat$pattern_auto[2:4] <- "burst"
  dat
}

test_that("legacy tonic burst-overlap settings are audit-only", {
  detect_tonic <- getFromNamespace("detect_tonic_train", "SpikeTrainPatternDetector")

  params <- SpikeTrainPatternDetector::default_params()
  dat <- make_tonic_burst_overlap_train(tonic_isi = 0.032)
  occupied <- dat$pattern_auto != ""

  guarded <- detect_tonic(
    dat, occupied, params$tonic, T_B_seed = 0.010,
    min_isi_sec = 0.001, train = "train_1"
  )

  p_unguarded <- params$tonic
  p_unguarded$burst_overlap_guard <- FALSE
  unguarded <- detect_tonic(
    dat, occupied, p_unguarded, T_B_seed = 0.010,
    min_isi_sec = 0.001, train = "train_1"
  )

  expect_gt(nrow(guarded), 0L)
  expect_gt(nrow(unguarded), 0)
  expect_identical(
    unname(as.integer(guarded[c("start_isi", "end_isi")])),
    unname(as.integer(unguarded[c("start_isi", "end_isi")]))
  )
  expect_true(all(!guarded$tonic_burst_overlap_veto_applied))
  expect_true(all(!guarded$tonic_anti_burst_veto_applied))
})

test_that("AUTO tonic still accepts stable ISIs separated from the AUTO burst band", {
  detect_tonic <- getFromNamespace("detect_tonic_train", "SpikeTrainPatternDetector")

  params <- SpikeTrainPatternDetector::default_params()
  dat <- make_tonic_burst_overlap_train(tonic_isi = 0.040)
  occupied <- dat$pattern_auto != ""

  out <- detect_tonic(
    dat, occupied, params$tonic, T_B_seed = 0.010,
    min_isi_sec = 0.001, train = "train_1"
  )

  expect_gt(nrow(out), 0)
  expect_true("tonic_burst_overlap_ref_sec" %in% names(out))
  expect_equal(out$tonic_burst_overlap_ref_sec[1], 0.030, tolerance = 1e-8)
})

test_that("manual tonic anchors remain independent of Burst Event overlap", {
  detect_tonic <- getFromNamespace("detect_tonic_train", "SpikeTrainPatternDetector")

  params <- SpikeTrainPatternDetector::default_params()
  dat <- make_tonic_burst_overlap_train(tonic_isi = 0.032)
  occupied <- dat$pattern_auto != ""

  p_anchor <- params$tonic
  p_anchor$adaptive_use_train_ranges <- TRUE
  p_anchor$adaptive_train_ranges_hard <- TRUE
  p_anchor$adaptive_range_mode <- "absolute_only"
  p_anchor$adaptive_train_ranges <- list(
    train_1 = list(
      low_sec = 0.030,
      high_sec = 0.034,
      anchor_center_sec = 0.032,
      anchor_spread_log = 0.15,
      anchor_n = 6L,
      anchor_confidence = 0.70,
      source = "manual_tonic",
      method = "manual tonic anchor"
    )
  )

  out <- detect_tonic(
    dat, occupied, p_anchor, T_B_seed = 0.010,
    min_isi_sec = 0.001, train = "train_1"
  )

  expect_gt(nrow(out), 0L)
  expect_true(all(!out$tonic_burst_overlap_veto_applied))
})

test_that("event-core tonic records but does not apply the old Burst veto", {
  event_core_params <- getFromNamespace("stpd_event_core_params", "SpikeTrainPatternDetector")
  detect_tonic_core <- getFromNamespace("stpd_event_core_detect_tonic", "SpikeTrainPatternDetector")
  productize <- getFromNamespace("stpd_productize_params", "SpikeTrainPatternDetector")

  params <- SpikeTrainPatternDetector::default_params()
  params$spiketrainpattern$burst$seed_upper_sec <- 0.030
  params$spiketrainpattern$burst$bridge_upper_sec <- 0.030
  params <- productize(params, prefer = "canonical")

  near_burst <- make_tonic_burst_overlap_train(tonic_isi = 0.032, auto_burst = FALSE)
  far_from_burst <- make_tonic_burst_overlap_train(tonic_isi = 0.040, auto_burst = FALSE)

  vp_near <- event_core_params(near_burst, params, min_isi_sec = 0.001)
  vp_far <- event_core_params(far_from_burst, params, min_isi_sec = 0.001)

  near <- detect_tonic_core(near_burst, params, vp_near, min_isi_sec = 0.001, train = "train_1")
  far <- detect_tonic_core(far_from_burst, params, vp_far, min_isi_sec = 0.001, train = "train_1")

  expect_gt(nrow(near), 0L)
  expect_gt(nrow(far), 0)
  expect_true(all(!near$tonic_burst_overlap_veto_applied))
  expect_true(all(!near$tonic_seed_fraction_veto_applied))
  expect_true("tonic_burst_overlap_ref_sec" %in% names(far))
})

test_that("event-core HFT keeps State support across a Burst-like core", {
  event_core_params <- getFromNamespace(
    "stpd_event_core_params", "SpikeTrainPatternDetector"
  )
  detect_hft <- getFromNamespace(
    "stpd_event_core_detect_hf_tonic", "SpikeTrainPatternDetector"
  )

  isi <- c(rep(0.015, 5L), 0.005, 0.005, rep(0.015, 5L))
  dat <- data.frame(
    timestamp_sec = c(0, cumsum(isi)),
    ISI_sec = c(NA_real_, isi),
    pattern_manual = "",
    pattern_auto = "",
    stringsAsFactors = FALSE
  )
  params <- SpikeTrainPatternDetector::default_params()
  params$highfreq$tonic_burst_core_veto <- TRUE
  vp <- event_core_params(dat, params, min_isi_sec = 0.001)
  vp$hf_tonic_floor <- 0.001
  vp$hf_tonic_low_tail_max <- 1
  vp$hf_tonic_cv_max <- 10
  vp$hf_tonic_lv_max <- 10
  vp$hf_tonic_mm_max <- 10
  vp$hf_tonic_min_spikes <- 6L
  vp$hf_tonic_burst_core_veto <- TRUE
  vp$hf_tonic_core_veto_min_isi_n <- 2L

  out <- detect_hft(
    dat, params, vp, min_isi_sec = 0.001, train = "train_1"
  )

  expect_gt(nrow(out), 0L)
  expect_true(any(out$hf_tonic_burst_like_core_present))
  expect_true(all(!out$hf_tonic_burst_core_veto_applied))
})

test_that("seed-bridge HFT also treats the old Burst veto as audit-only", {
  detect_hft <- getFromNamespace(
    "stpd_seed_bridge_detect_hf_tonic", "SpikeTrainPatternDetector"
  )
  isi <- c(rep(0.015, 5L), 0.005, 0.005, rep(0.015, 5L))
  dat <- data.frame(
    timestamp_sec = c(0, cumsum(isi)),
    ISI_sec = c(NA_real_, isi),
    pattern_manual = "",
    pattern_auto = "",
    stringsAsFactors = FALSE
  )
  params <- SpikeTrainPatternDetector::default_params()
  params$highfreq$tonic_burst_core_veto <- TRUE
  params$highfreq$tonic_min_ISI_floor_sec <- 0.001
  params$highfreq$tonic_low_tail_fraction_max <- 1
  params$highfreq$stable_CV_max <- 10
  params$highfreq$stable_LV_max <- 10
  params$highfreq$stable_MM_max <- 10

  out <- detect_hft(
    dat, params, min_isi_sec = 0.001, train = "train_1"
  )

  expect_gt(nrow(out), 0L)
  expect_true(any(out$hf_tonic_core_veto_requested))
  expect_true(all(!out$hf_tonic_core_veto_applied))
})
