test_that("manual tonic anchors do not become hard boundaries", {
  detect_tonic <- getFromNamespace("detect_tonic_train", "SpikeTrainPatternDetector")

  isi <- c(NA, rep(0.050, 12))
  dat <- data.frame(
    idx = seq_along(isi),
    timestamp_sec = cumsum(c(0, isi[-1])),
    ISI_sec = isi,
    pattern_manual = "",
    pattern_auto = "",
    stringsAsFactors = FALSE
  )

  params <- SpikeTrainPatternDetector::default_params()
  occupied <- rep(FALSE, nrow(dat))
  base <- detect_tonic(dat, occupied, params$tonic, params$burst$T_seed,
                       min_isi_sec = 0.001, train = "train_1")

  p_manual <- params$tonic
  p_manual$adaptive_use_train_ranges <- TRUE
  p_manual$adaptive_train_ranges_hard <- TRUE
  p_manual$adaptive_range_mode <- "absolute_only"
  p_manual$adaptive_train_ranges <- list(
    train_1 = list(
      low_sec = 0.025,
      high_sec = 0.035,
      anchor_center_sec = 0.030,
      anchor_spread_log = 0.30,
      anchor_n = 6L,
      anchor_confidence = 0.50,
      source = "manual_tonic",
      method = "manual tonic anchor"
    )
  )
  manual <- detect_tonic(dat, occupied, p_manual, params$burst$T_seed,
                         min_isi_sec = 0.001, train = "train_1")

  p_explicit <- p_manual
  p_explicit$adaptive_train_ranges[[1]]$source <- "ui_tab"
  p_explicit$adaptive_train_ranges[[1]]$method <- "explicit hard tonic range"
  explicit <- detect_tonic(dat, occupied, p_explicit, params$burst$T_seed,
                           min_isi_sec = 0.001, train = "train_1")

  expect_gt(nrow(base), 0)
  expect_gt(nrow(manual), 0)
  expect_equal(nrow(explicit), 0L)
})

test_that("manual pause anchors do not become hard boundaries and cannot create pause alone", {
  detect_pause <- getFromNamespace("detect_pause_train", "SpikeTrainPatternDetector")

  params <- SpikeTrainPatternDetector::default_params()
  dat <- data.frame(
    idx = 1:8,
    timestamp_sec = cumsum(c(0, 0.05, 0.05, 0.05, 0.25, 0.05, 0.05, 0.05)),
    ISI_sec = c(NA, 0.05, 0.05, 0.05, 0.25, 0.05, 0.05, 0.05),
    pattern_manual = "",
    pattern_auto = "",
    stringsAsFactors = FALSE
  )

  p_manual <- params$pause
  p_manual$adaptive_use_train_ranges <- TRUE
  p_manual$adaptive_train_ranges_hard <- TRUE
  p_manual$adaptive_range_mode <- "absolute_only"
  p_manual$adaptive_train_ranges <- list(
    train_1 = list(
      low_sec = 0.10,
      high_sec = 0.15,
      anchor_center_sec = 0.12,
      anchor_spread_log = 0.30,
      anchor_n = 6L,
      anchor_confidence = 0.50,
      source = "manual_pause",
      method = "manual pause anchor"
    )
  )

  base <- detect_pause(dat, rep(FALSE, nrow(dat)), params$pause, params$tonic,
                       min_isi_sec = 0.001, current_labels = rep("", nrow(dat)), train = "train_1")
  manual <- detect_pause(dat, rep(FALSE, nrow(dat)), p_manual, params$tonic,
                         min_isi_sec = 0.001, current_labels = rep("", nrow(dat)), train = "train_1")

  p_explicit <- p_manual
  p_explicit$adaptive_train_ranges[[1]]$source <- "ui_tab"
  p_explicit$adaptive_train_ranges[[1]]$method <- "explicit hard pause range"
  explicit <- detect_pause(dat, rep(FALSE, nrow(dat)), p_explicit, params$tonic,
                           min_isi_sec = 0.001, current_labels = rep("", nrow(dat)), train = "train_1")

  expect_gt(nrow(base), 0)
  expect_gt(nrow(manual), 0)
  expect_equal(nrow(explicit), 0L)

  ordinary <- data.frame(
    idx = 1:8,
    timestamp_sec = cumsum(c(0, rep(0.05, 7))),
    ISI_sec = c(NA, rep(0.05, 7)),
    pattern_manual = "",
    pattern_auto = "",
    stringsAsFactors = FALSE
  )
  p_anchor_ordinary <- params$pause
  p_anchor_ordinary$adaptive_use_train_ranges <- TRUE
  p_anchor_ordinary$adaptive_train_ranges_hard <- TRUE
  p_anchor_ordinary$adaptive_range_mode <- "absolute_only"
  p_anchor_ordinary$adaptive_train_ranges <- list(
    train_1 = list(
      low_sec = 0.045,
      high_sec = 0.055,
      anchor_center_sec = 0.050,
      anchor_spread_log = 0.30,
      anchor_n = 6L,
      anchor_confidence = 0.50,
      source = "manual_pause",
      method = "manual pause anchor"
    )
  )
  ordinary_pause <- detect_pause(ordinary, rep(FALSE, nrow(ordinary)), p_anchor_ordinary, params$tonic,
                                 min_isi_sec = 0.001, current_labels = rep("", nrow(ordinary)), train = "train_1")
  expect_equal(nrow(ordinary_pause), 0L)
})

test_that("manual high-frequency anchors support HF candidates without replacing structure", {
  detect_hf <- getFromNamespace("detect_high_frequency_modes_train", "SpikeTrainPatternDetector")

  isi <- c(NA, 0.12, 0.12, rep(0.030, 8), 0.12, 0.12)
  dat <- data.frame(
    idx = seq_along(isi),
    timestamp_sec = cumsum(c(0, isi[-1])),
    ISI_sec = isi,
    pattern_manual = "",
    pattern_auto = "",
    stringsAsFactors = FALSE
  )

  params <- SpikeTrainPatternDetector::default_params()
  p <- params$highfreq
  p$T_high_max <- 0.010
  p$ISI_abs_max <- 0.010
  p$pct_max <- 1
  p$ISI_pct_max <- 1
  p$spiking_use_abs_max <- TRUE
  p$spiking_max_ISI_abs <- 0.010
  p$spiking_use_pct_max <- TRUE
  p$spiking_max_ISI_pct <- 1
  p$adaptive_use_train_ranges <- FALSE

  no_anchor <- detect_hf(dat, rep(FALSE, nrow(dat)), p, min_isi_sec = 0.001, train = "train_1")

  p_anchor <- p
  p_anchor$adaptive_use_train_ranges <- TRUE
  p_anchor$adaptive_train_ranges_hard <- TRUE
  p_anchor$adaptive_range_mode <- "absolute_only"
  p_anchor$adaptive_train_ranges <- list(
    train_1 = list(
      low_sec = 0.025,
      high_sec = 0.035,
      anchor_center_sec = 0.030,
      anchor_spread_log = 0.30,
      anchor_n = 8L,
      anchor_confidence = 0.60,
      source = "manual_high_frequency",
      method = "manual high-frequency anchor"
    )
  )
  with_anchor <- detect_hf(dat, rep(FALSE, nrow(dat)), p_anchor, min_isi_sec = 0.001, train = "train_1")

  expect_equal(nrow(no_anchor), 0L)
  expect_gt(nrow(with_anchor), 0)
  expect_true(any(as.character(with_anchor$class) == "high_frequency_tonic"))
  expect_true(any(with_anchor$manual_anchor_active))

  ordinary <- data.frame(
    idx = 1:12,
    timestamp_sec = cumsum(c(0, rep(0.050, 11))),
    ISI_sec = c(NA, rep(0.050, 11)),
    pattern_manual = "",
    pattern_auto = "",
    stringsAsFactors = FALSE
  )
  p_bad_anchor <- p_anchor
  p_bad_anchor$adaptive_train_ranges[[1]]$low_sec <- 0.045
  p_bad_anchor$adaptive_train_ranges[[1]]$high_sec <- 0.055
  p_bad_anchor$adaptive_train_ranges[[1]]$anchor_center_sec <- 0.050
  p_bad_anchor$stable_LV_max <- 0.0001
  p_bad_anchor$LV_stable_max <- 0.0001
  p_bad_anchor$stable_CV_max <- 0.0001
  p_bad_anchor$CV_stable_max <- 0.0001
  p_bad_anchor$stable_MM_max <- 0.90
  p_bad_anchor$MM_stable_max <- 0.90
  not_hf <- detect_hf(ordinary, rep(FALSE, nrow(ordinary)), p_bad_anchor, min_isi_sec = 0.001, train = "train_1")
  expect_equal(nrow(not_hf), 0L)
})

test_that("manual high-frequency labels derive soft anchors", {
  derive_hf <- getFromNamespace("derive_highfreq_isi_ranges_from_manual", "SpikeTrainPatternDetector")

  isi <- c(NA, 0.10, 0.08, rep(0.020, 6), 0.10)
  dat <- data.frame(
    idx = seq_along(isi),
    timestamp_sec = cumsum(c(0, isi[-1])),
    ISI_sec = isi,
    pattern_manual = c("", "", "", rep("high_frequency_tonic", 6), ""),
    pattern_auto = "",
    stringsAsFactors = FALSE
  )
  ds <- SpikeTrainPatternDetector:::make_dataset("hf_anchor", "synthetic", list(train_1 = dat), unit_in = "s")
  learned <- derive_hf(ds, min_isi_sec = 0.001)

  expect_true("train_1" %in% names(learned))
  expect_true(isTRUE(getFromNamespace("stpd_range_is_manual_anchor", "SpikeTrainPatternDetector")(learned$train_1)))
  expect_equal(learned$train_1$anchor_n, 6L)
  expect_true(is.finite(learned$train_1$anchor_center_sec))
})
