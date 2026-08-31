make_tonic_frozen_band_mixed_train <- function() {
  isi <- c(
    rep(0.012, 12L), 0.220,
    c(0.094, 0.101, 0.098, 0.104, 0.096, 0.102, 0.099, 0.103),
    0.220, rep(0.014, 8L)
  )
  timestamp <- c(0, cumsum(isi))
  data.frame(
    idx = seq_along(timestamp),
    timestamp_sec = timestamp,
    ISI_sec = c(NA_real_, isi),
    pattern_manual = "",
    pattern_auto = "",
    stringsAsFactors = FALSE
  )
}

test_that("resolved user/manual Tonic bands do not inherit whole-train HFS q10", {
  bounds_fun <- getFromNamespace(
    "stpd_event_core_tonic_adaptive_bounds", "SpikeTrainPatternDetector"
  )
  dat <- make_tonic_frozen_band_mixed_train()
  isi <- dat$ISI_sec
  valid <- is.finite(isi)

  base_vp <- list(
    tonic_min = 0.080, tonic_max = 0.120,
    tonic_burst_overlap_ref = 0.025,
    tonic_burst_overlap_guard_factor = 1.15,
    pause_thr = 0.180
  )
  user <- bounds_fun(
    isi, valid, c(base_vp, list(tonic_threshold_source_mode = "user")),
    min_isi_sec = 0.001
  )
  manual <- bounds_fun(
    isi, valid, c(base_vp, list(tonic_threshold_source_mode = "manual")),
    min_isi_sec = 0.001
  )
  default <- bounds_fun(
    isi, valid, c(base_vp, list(tonic_threshold_source_mode = "default")),
    min_isi_sec = 0.001
  )
  automatic <- bounds_fun(
    isi, valid, c(base_vp, list(tonic_threshold_source_mode = "auto")),
    min_isi_sec = 0.001
  )
  auto_priority <- bounds_fun(
    isi, valid, c(base_vp, list(tonic_threshold_source_mode = "auto_priority")),
    min_isi_sec = 0.001
  )
  histogram <- bounds_fun(
    isi, valid, c(base_vp, list(tonic_threshold_source_mode = "histogram")),
    min_isi_sec = 0.001
  )

  expect_equal(user$lower, 0.080)
  expect_equal(manual$lower, 0.080)
  expect_equal(default$lower, 0.080)
  expect_equal(user$upper, 0.120)
  expect_equal(manual$upper, 0.120)
  expect_equal(default$upper, 0.120)
  expect_equal(user$core_lower, 0.080)
  expect_equal(manual$core_lower, 0.080)
  expect_equal(default$core_lower, 0.080)
  expect_false(user$tonic_train_lower_adaptation_applied)
  expect_false(manual$tonic_train_lower_adaptation_applied)
  expect_false(default$tonic_train_lower_adaptation_applied)
  expect_false(user$tonic_train_upper_adaptation_applied)
  expect_false(manual$tonic_train_upper_adaptation_applied)
  expect_false(default$tonic_train_upper_adaptation_applied)
  expect_lt(automatic$lower, 0.080)
  expect_gt(automatic$upper, 0.120)
  expect_gt(automatic$core_lower, automatic$lower)
  expect_true(automatic$tonic_train_lower_adaptation_applied)
  expect_true(automatic$tonic_train_upper_adaptation_applied)
  expect_lt(auto_priority$lower, 0.080)
  expect_gt(auto_priority$upper, 0.120)
  expect_true(auto_priority$tonic_train_lower_adaptation_applied)
  expect_true(auto_priority$tonic_train_upper_adaptation_applied)
  expect_lt(histogram$lower, 0.080)
  expect_gt(histogram$upper, 0.120)
  expect_true(histogram$tonic_train_lower_adaptation_applied)
  expect_true(histogram$tonic_train_upper_adaptation_applied)
})

test_that("short-train Tonic bounds preserve the complete audit schema", {
  bounds_fun <- getFromNamespace(
    "stpd_event_core_tonic_adaptive_bounds", "SpikeTrainPatternDetector"
  )
  vp <- list(
    tonic_min = 0.080, tonic_max = 0.120,
    tonic_burst_overlap_ref = 0.025,
    tonic_burst_overlap_guard_factor = 1.15,
    tonic_threshold_source_mode = "manual"
  )
  isi <- c(NA_real_, 0.095, 0.101, 0.099, 0.104)
  out <- bounds_fun(isi, is.finite(isi), vp, min_isi_sec = 0.001)

  expect_true(all(c(
    "lower", "upper", "q10", "q75", "q90",
    "legacy_burst_floor_audit_sec", "burst_floor_applied",
    "tonic_threshold_source_mode", "tonic_frozen_lower_sec",
    "tonic_frozen_upper_sec", "tonic_train_lower_adaptation_applied",
    "tonic_train_upper_adaptation_applied", "core_lower"
  ) %in% names(out)))
  expect_equal(out$lower, 0.080)
  expect_equal(out$upper, 0.120)
  expect_equal(out$core_lower, 0.080)
  expect_equal(out$tonic_threshold_source_mode, "manual")
  expect_false(out$burst_floor_applied)
  expect_false(out$tonic_train_lower_adaptation_applied)
})

test_that("frozen Tonic entry excludes an HFS plateau before regularity gating", {
  event_params <- getFromNamespace(
    "stpd_event_grammar_params", "SpikeTrainPatternDetector"
  )
  detect_tonic <- getFromNamespace(
    "stpd_event_core_detect_tonic", "SpikeTrainPatternDetector"
  )
  dat <- make_tonic_frozen_band_mixed_train()
  params <- SpikeTrainPatternDetector::default_params()
  params$event_grammar$threshold_source_mode <- "user"
  params$event_grammar$user$tonic <- list(
    enable = TRUE, seed_lower_sec = 0.080,
    seed_upper_sec = 0.120, bridge_upper_sec = 0.120,
    contrast_S = NA_real_
  )
  params$spiketrainpattern$engine$threshold_source_mode <- "user"
  params$spiketrainpattern$tonic$min_isi_sec <- 0.080
  params$spiketrainpattern$tonic$max_isi_sec <- 0.120
  params$spiketrainpattern$tonic$bridge_upper_sec <- 0.120
  params$spiketrainpattern$tonic$min_spikes <- 5L
  params$spiketrainpattern$tonic$lv_max <- 0.5
  params <- getFromNamespace(
    "effective_params_for_detector", "SpikeTrainPatternDetector"
  )(params)

  vp <- event_params(dat, params, min_isi_sec = 0.001, train = "train_1")
  out <- detect_tonic(
    dat, params, vp, min_isi_sec = 0.001, train = "train_1"
  )

  expect_gt(nrow(out), 0L)
  expect_true(all(out$start_isi >= 15L))
  expect_true(all(out$end_isi <= 22L))
  expect_true(all(out$tonic_threshold_source_mode == "user"))
  expect_true(all(!out$tonic_train_lower_adaptation_applied))
})

test_that("calibration-derived Tonic duration rejects chance fragments", {
  event_params <- getFromNamespace(
    "stpd_event_grammar_params", "SpikeTrainPatternDetector"
  )
  detect_tonic <- getFromNamespace(
    "stpd_event_core_detect_tonic", "SpikeTrainPatternDetector"
  )
  isi <- c(0.030, 0.031, 0.200, 0.030, 0.031, 0.032, 0.033)
  timestamp <- c(0, cumsum(isi))
  dat <- data.frame(
    idx = seq_along(timestamp),
    timestamp_sec = timestamp,
    ISI_sec = c(NA_real_, isi),
    pattern_manual = "", pattern_auto = "",
    stringsAsFactors = FALSE
  )
  params <- SpikeTrainPatternDetector::default_params()
  params$event_grammar$threshold_source_mode <- "user"
  params$event_grammar$user$tonic <- list(
    enable = TRUE, seed_lower_sec = 0.020,
    seed_upper_sec = 0.040, bridge_upper_sec = 0.040,
    contrast_S = NA_real_
  )
  params$spiketrainpattern$engine$threshold_source_mode <- "user"
  params$spiketrainpattern$tonic$min_isi_sec <- 0.020
  params$spiketrainpattern$tonic$max_isi_sec <- 0.040
  params$spiketrainpattern$tonic$bridge_upper_sec <- 0.040
  params$spiketrainpattern$tonic$min_spikes <- 3L
  params$spiketrainpattern$tonic$lv_max <- 0.5
  params$tonic$D_min <- 0.090
  params$spiketrainpattern$tonic$min_duration_sec <- 0.090
  params <- getFromNamespace(
    "effective_params_for_detector", "SpikeTrainPatternDetector"
  )(params)

  vp <- event_params(dat, params, min_isi_sec = 0.001, train = "train_1")
  expect_equal(vp$tonic_min_duration, 0.090)
  out <- detect_tonic(
    dat, params, vp, min_isi_sec = 0.001, train = "train_1"
  )

  expect_gt(nrow(out), 0L)
  expect_true(all(out$start_isi >= 5L))
  expect_true(all(out$tonic_duration_sec >= 0.090))
  expect_true(all(out$tonic_duration_gate_applied))
  expect_true(all(out$tonic_min_duration_sec == 0.090))
})

test_that("Tonic short-support learning is group-frozen and informative", {
  learn_short <- getFromNamespace(
    "stpd_manual_tonic_short_route_estimates", "SpikeTrainPatternDetector"
  )
  groups <- rep(paste0("g", 1:5), each = 2L)
  informative <- learn_short(
    data.frame(n_spikes = c(8L, 8L, 9L, 9L, 10L, 10L, 11L, 11L, 12L, 12L)),
    group = groups
  )
  expect_true(informative$enabled)
  expect_equal(informative$evidence_n, 10L)
  expect_equal(informative$group_n, 5L)
  expect_equal(informative$min_isi_count, 7L)
  expect_equal(informative$logo_q10_min, 7)
  expect_equal(informative$mode, "calibration_group_logo_q10_v1")

  uninformative <- learn_short(
    data.frame(n_spikes = rep(3L, 10L)),
    group = groups
  )
  expect_false(uninformative$enabled)
  expect_equal(uninformative$min_isi_count, 2L)
  expect_equal(
    uninformative$status,
    "disabled__calibration_support_not_above_structural_floor"
  )
})

test_that("calibrated short regular support can complement Tonic duration", {
  event_params <- getFromNamespace(
    "stpd_event_grammar_params", "SpikeTrainPatternDetector"
  )
  detect_tonic <- getFromNamespace(
    "stpd_event_core_detect_tonic", "SpikeTrainPatternDetector"
  )
  isi <- rep(0.100, 7L)
  timestamp <- c(0, cumsum(isi))
  dat <- data.frame(
    idx = seq_along(timestamp),
    timestamp_sec = timestamp,
    ISI_sec = c(NA_real_, isi),
    pattern_manual = "", pattern_auto = "",
    stringsAsFactors = FALSE
  )
  params <- SpikeTrainPatternDetector::default_params()
  params$event_grammar$threshold_source_mode <- "user"
  params$event_grammar$user$tonic <- list(
    enable = TRUE, seed_lower_sec = 0.080,
    seed_upper_sec = 0.120, bridge_upper_sec = 0.120,
    contrast_S = NA_real_
  )
  params$spiketrainpattern$engine$threshold_source_mode <- "user"
  params$spiketrainpattern$tonic$min_isi_sec <- 0.080
  params$spiketrainpattern$tonic$max_isi_sec <- 0.120
  params$spiketrainpattern$tonic$bridge_upper_sec <- 0.120
  params$spiketrainpattern$tonic$min_spikes <- 8L
  params$spiketrainpattern$tonic$lv_max <- 0.5
  params$tonic$D_min <- 0.900
  params$spiketrainpattern$tonic$min_duration_sec <- 0.900
  params$tonic$short_regular_route_enabled <- TRUE
  params$tonic$short_regular_min_isi_count <- 7L
  params$tonic$short_regular_evidence_n <- 10L
  params$tonic$short_regular_group_n <- 5L
  params$tonic$short_regular_count_q10_full <- 7
  params$tonic$short_regular_logo_q10_min <- 7
  params$tonic$short_regular_logo_q10_max <- 7
  params$tonic$short_regular_structural_floor_spikes <- 3L
  params$tonic$short_regular_mode <- "calibration_group_logo_q10_v1"
  params$tonic$short_regular_status <-
    "enabled__calibration_frozen_short_regular_support"
  for (nm in c(
    "short_regular_route_enabled", "short_regular_min_isi_count",
    "short_regular_evidence_n", "short_regular_group_n",
    "short_regular_count_q10_full", "short_regular_logo_q10_min",
    "short_regular_logo_q10_max",
    "short_regular_structural_floor_spikes", "short_regular_mode",
    "short_regular_status"
  )) {
    params$spiketrainpattern$tonic[[nm]] <- params$tonic[[nm]]
  }
  params <- getFromNamespace(
    "effective_params_for_detector", "SpikeTrainPatternDetector"
  )(params)

  vp <- event_params(dat, params, min_isi_sec = 0.001, train = "train_1")
  out <- detect_tonic(
    dat, params, vp, min_isi_sec = 0.001, train = "train_1"
  )

  expect_gt(nrow(out), 0L)
  expect_true(all(out$tonic_duration_sec < out$tonic_min_duration_sec))
  expect_true(all(!out$tonic_duration_support_pass))
  expect_true(all(out$tonic_short_regular_route_pass))
  expect_true(all(
    out$tonic_acceptance_route == "calibration_short_regular_support"
  ))

  params$tonic$short_regular_route_enabled <- FALSE
  params$spiketrainpattern$tonic$short_regular_route_enabled <- FALSE
  vp_disabled <- event_params(
    dat, params, min_isi_sec = 0.001, train = "train_1"
  )
  disabled <- detect_tonic(
    dat, params, vp_disabled, min_isi_sec = 0.001, train = "train_1"
  )
  expect_equal(nrow(disabled), 0L)
})

test_that("Tonic short route fails closed and survives the public round-trip", {
  event_params <- getFromNamespace(
    "stpd_event_grammar_params", "SpikeTrainPatternDetector"
  )
  detect_tonic <- getFromNamespace(
    "stpd_event_core_detect_tonic", "SpikeTrainPatternDetector"
  )
  public_only <- getFromNamespace(
    "stpd_public_params_only", "SpikeTrainPatternDetector"
  )
  effective <- getFromNamespace(
    "effective_params_for_detector", "SpikeTrainPatternDetector"
  )
  timestamp <- c(0, cumsum(rep(0.100, 7L)))
  dat <- data.frame(
    idx = seq_along(timestamp), timestamp_sec = timestamp,
    ISI_sec = c(NA_real_, rep(0.100, 7L)),
    pattern_manual = "", pattern_auto = "", stringsAsFactors = FALSE
  )
  params <- SpikeTrainPatternDetector::default_params()
  params$spiketrainpattern$engine$threshold_source_mode <- "user"
  params$event_grammar$threshold_source_mode <- "user"
  params$event_grammar$user$tonic <- list(
    enable = TRUE, seed_lower_sec = 0.080,
    seed_upper_sec = 0.120, bridge_upper_sec = 0.120,
    contrast_S = NA_real_
  )
  tp <- params$spiketrainpattern$tonic
  tp$min_isi_sec <- 0.080
  tp$max_isi_sec <- 0.120
  tp$bridge_upper_sec <- 0.120
  tp$min_spikes <- 8L
  tp$lv_max <- 0.5
  tp$mm_min <- 0.91
  tp$connector_max_isi_count <- 4L
  tp$min_duration_sec <- 0.900
  tp$short_regular_route_enabled <- TRUE
  tp$short_regular_min_isi_count <- 7L
  tp$short_regular_evidence_n <- 10L
  tp$short_regular_group_n <- 5L
  tp$short_regular_count_q10_full <- 7
  tp$short_regular_logo_q10_min <- 7
  tp$short_regular_logo_q10_max <- 7
  tp$short_regular_structural_floor_spikes <- 3L
  tp$short_regular_mode <- "calibration_group_logo_q10_v1"
  tp$short_regular_status <-
    "enabled__calibration_frozen_short_regular_support"
  params$spiketrainpattern$tonic <- tp
  params <- effective(params)

  restored <- effective(public_only(params))
  expect_equal(restored$tonic$D_min, 0.900)
  expect_true(restored$tonic$short_regular_route_enabled)
  expect_equal(restored$tonic$short_regular_min_isi_count, 7L)
  expect_equal(restored$tonic$short_regular_count_q10_full, 7)
  expect_equal(restored$tonic$tonic_mm_min, 0.91)
  expect_equal(restored$tonic$connector_max_n, 4L)
  vp <- event_params(dat, restored, min_isi_sec = 0.001, train = "train_1")
  valid <- detect_tonic(
    dat, restored, vp, min_isi_sec = 0.001, train = "train_1"
  )
  expect_gt(nrow(valid), 0L)
  expect_true(all(valid$tonic_short_regular_contract_valid))

  invalid <- restored
  invalid$spiketrainpattern$tonic$short_regular_evidence_n <- 0L
  invalid <- effective(invalid)
  invalid_vp <- event_params(
    dat, invalid, min_isi_sec = 0.001, train = "train_1"
  )
  rejected <- detect_tonic(
    dat, invalid, invalid_vp, min_isi_sec = 0.001, train = "train_1"
  )
  expect_equal(nrow(rejected), 0L)
  issues <- stpd_validate_params(invalid)
  expect_true(any(
    issues$severity == "error" &
      grepl("invalid calibration-frozen contract", issues$issue, fixed = TRUE)
  ))

  weakened <- restored
  weakened$spiketrainpattern$tonic$short_regular_evidence_n <- 10L
  weakened$spiketrainpattern$tonic$short_regular_min_isi_count <- 3L
  weakened <- effective(weakened)
  weakened_vp <- event_params(
    dat, weakened, min_isi_sec = 0.001, train = "train_1"
  )
  expect_equal(nrow(detect_tonic(
    dat, weakened, weakened_vp, min_isi_sec = 0.001, train = "train_1"
  )), 0L)
})

test_that("public Tonic candidate API uses the event-core State generator", {
  timestamp <- c(0, cumsum(rep(0.100, 7L)))
  dat <- data.frame(
    idx = seq_along(timestamp), timestamp_sec = timestamp,
    ISI_sec = c(NA_real_, rep(0.100, 7L)),
    pattern_manual = "", pattern_auto = "", stringsAsFactors = FALSE
  )
  params <- SpikeTrainPatternDetector::default_params()
  params$spiketrainpattern$engine$threshold_source_mode <- "user"
  params$event_grammar$threshold_source_mode <- "user"
  params$event_grammar$user$tonic <- list(
    enable = TRUE, seed_lower_sec = 0.080,
    seed_upper_sec = 0.120, bridge_upper_sec = 0.120,
    contrast_S = NA_real_
  )
  tp <- params$spiketrainpattern$tonic
  tp$min_isi_sec <- 0.080; tp$max_isi_sec <- 0.120
  tp$bridge_upper_sec <- 0.120; tp$min_spikes <- 8L
  tp$lv_max <- 0.5; tp$min_duration_sec <- 0.900
  tp$short_regular_route_enabled <- TRUE
  tp$short_regular_min_isi_count <- 7L
  tp$short_regular_evidence_n <- 10L; tp$short_regular_group_n <- 5L
  tp$short_regular_count_q10_full <- 7
  tp$short_regular_logo_q10_min <- 7; tp$short_regular_logo_q10_max <- 7
  tp$short_regular_structural_floor_spikes <- 3L
  tp$short_regular_mode <- "calibration_group_logo_q10_v1"
  tp$short_regular_status <-
    "enabled__calibration_frozen_short_regular_support"
  params$spiketrainpattern$tonic <- tp

  api <- SpikeTrainPatternDetector::detect_tonic_candidates(
    dat, params = params, train = "train_1", min_isi_sec = 0.001
  )
  expect_gt(nrow(api), 0L)
  expect_identical(names(api), c(
    "start_isi", "end_isi", "tonic_burst_overlap_ref_sec",
    "tonic_burst_overlap_guard", "tonic_burst_overlap_veto_applied",
    "tonic_anti_burst_veto_applied"
  ))
  expect_equal(api$start_isi, 2L)
  expect_equal(api$end_isi, 8L)
  blocked <- SpikeTrainPatternDetector::detect_tonic_candidates(
    dat, occupied_idx = 4L, params = params,
    train = "train_1", min_isi_sec = 0.001
  )
  expect_equal(nrow(blocked), 0L)
  expect_identical(names(blocked), c("start_isi", "end_isi"))
  expect_error(
    SpikeTrainPatternDetector::detect_tonic_candidates(
      dat, occupied_idx = 0L, params = params,
      train = "train_1", min_isi_sec = 0.001
    ),
    "out-of-range"
  )
  expect_error(
    SpikeTrainPatternDetector::detect_tonic_candidates(
      dat, occupied_idx = 3.5, params = params,
      train = "train_1", min_isi_sec = 0.001
    ),
    "non-integer"
  )
})
