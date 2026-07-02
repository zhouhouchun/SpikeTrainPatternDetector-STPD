# ============================================================
# event core Core event-grammar detector
# ============================================================
# This script intentionally replaces the previous engine/dataset ISI patch-stack detector
# at the final run_detector_one_train entry point.  The new core is built around
# explicit ISI-band semantics and event/state grammar:
#   1) Dataset/user/manual ISI bands define the meaning of burst seed, bridge,
#      HF-tonic floor, tonic range, and pause range.
#   2) Burst and long_burst are event labels: seed-centered compact short-ISI
#      cores, optional bridge ISIs, and a flank contrast rule
#        S = min(pre_gap, post_gap) / intra_q90.
#   3) High-frequency spiking is a long high-frequency state, usually >=30 spikes,
#      allowing occasional larger ISIs and lacking required burst event grammar.
#   4) High-frequency tonic is a regular tonic-like high-frequency state and must
#      not be dominated by the extreme burst-core ISI band.
#   5) Tonic is a stable mid-ISI state; pause is an independent long-gap layer.
#
# The seed-bridge pipeline remains available when the event core is disabled.

stpd_train_pipeline_seed_bridge_classicity <- stpd_detect_train_seed_bridge_classicity

stpd_event_core_num <- function(x, default = NA_real_) {
  y <- suppressWarnings(as.numeric(x))
  if (length(y) == 0 || !is.finite(y[1])) return(default)
  y[1]
}

stpd_event_core_int <- function(x, default = 0L) {
  y <- suppressWarnings(as.integer(round(as.numeric(x))))
  if (length(y) == 0 || !is.finite(y[1])) return(as.integer(default))
  as.integer(y[1])
}

stpd_event_core_bool <- function(x, default = FALSE) {
  if (length(x) == 0 || is.na(x[1])) return(default)
  isTRUE(x[1])
}

stpd_event_core_quantile <- function(x, p, default = NA_real_) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  if (length(x) == 0) return(default)
  suppressWarnings(as.numeric(stats::quantile(x, p, na.rm = TRUE, names = FALSE, type = 7)))
}

stpd_event_core_cv <- function(x) {
  x <- suppressWarnings(as.numeric(x)); x <- x[is.finite(x)]
  if (length(x) < 2) return(NA_real_)
  mu <- mean(x, na.rm = TRUE)
  if (!is.finite(mu) || mu <= 0) return(NA_real_)
  stats::sd(x, na.rm = TRUE) / mu
}

stpd_event_core_lv <- function(x) {
  x <- suppressWarnings(as.numeric(x)); x <- x[is.finite(x)]
  if (length(x) < 2) return(NA_real_)
  a <- x[-length(x)]; b <- x[-1]
  denom <- a + b
  ok <- is.finite(denom) & denom > 0
  if (!any(ok)) return(NA_real_)
  3 * mean(((a[ok] - b[ok]) / denom[ok])^2, na.rm = TRUE)
}

stpd_event_core_mm <- function(x) {
  x <- suppressWarnings(as.numeric(x)); x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  mu <- mean(x, na.rm = TRUE)
  if (!is.finite(mu) || mu <= 0) return(NA_real_)
  max(x, na.rm = TRUE) / mu
}

stpd_event_core_bool_runs <- function(flag) {
  flag <- isTRUE(TRUE) & as.logical(flag)
  flag[is.na(flag)] <- FALSE
  if (length(flag) == 0 || !any(flag)) return(data.frame(start_isi = integer(), end_isi = integer()))
  d <- diff(c(FALSE, flag, FALSE))
  data.frame(start_isi = which(d == 1), end_isi = which(d == -1) - 1L)
}

stpd_event_core_max_consecutive_true <- function(flag) {
  r <- stpd_event_core_bool_runs(flag)
  if (nrow(r) == 0) return(0L)
  as.integer(max(r$end_isi - r$start_isi + 1L))
}

stpd_event_core_valid_train_isis <- function(isi, valid = NULL, min_isi_sec = 0.001) {
  x <- suppressWarnings(as.numeric(isi))
  if (!is.null(valid) && length(valid) == length(x)) x <- x[as.logical(valid)]
  valid_isi_values(x, min_isi_sec)
}

stpd_event_core_pause_global_floor <- function(isi, valid = NULL, vp = list(), min_isi_sec = 0.001) {
  vals <- stpd_event_core_valid_train_isis(isi, valid, min_isi_sec)
  if (length(vals) == 0) return(stpd_event_core_num(vp$pause_thr, NA_real_))
  q90 <- stpd_event_core_quantile(vals, 0.90)
  max(c(stpd_event_core_num(vp$pause_thr, NA_real_), q90), na.rm = TRUE)
}

stpd_event_core_tonic_adaptive_bounds <- function(isi, valid, vp, min_isi_sec = 0.001) {
  vals <- stpd_event_core_valid_train_isis(isi, valid, min_isi_sec)
  raw_min <- stpd_event_core_num(vp$tonic_min, min_isi_sec)
  raw_max <- stpd_event_core_num(vp$tonic_max, NA_real_)
  if (length(vals) < 6) {
    return(list(lower = raw_min, upper = raw_max, q10 = NA_real_, q75 = NA_real_, q90 = NA_real_))
  }
  q10 <- stpd_event_core_quantile(vals, 0.10)
  q25 <- stpd_event_core_quantile(vals, 0.25)
  q75 <- stpd_event_core_quantile(vals, 0.75)
  q90 <- stpd_event_core_quantile(vals, 0.90)
  burst_ref <- stpd_event_core_num(vp$tonic_burst_overlap_ref, NA_real_)
  burst_factor <- stpd_event_core_num(vp$tonic_burst_overlap_guard_factor, 1.15)
  burst_floor <- if (is.finite(burst_ref) && burst_ref > 0) burst_ref * max(1, burst_factor) else NA_real_

  lower_entry <- min(c(raw_min, q10), na.rm = TRUE)
  lower <- max(c(min_isi_sec, lower_entry, burst_floor), na.rm = TRUE)
  if (!is.finite(lower)) lower <- raw_min

  pause_floor <- stpd_event_core_pause_global_floor(isi, valid, vp, min_isi_sec)
  q90_tonic_candidate <- if (is.finite(q75) && q75 > 0 && is.finite(q90) && q90 <= q75 * 1.50) q90 * 1.02 else NA_real_
  upper_candidates <- c(raw_max, q75 * 1.25, q90_tonic_candidate, lower * 1.5)
  upper <- max(upper_candidates[is.finite(upper_candidates)], na.rm = TRUE)
  if (!is.finite(upper)) upper <- raw_max
  if (is.finite(pause_floor) && is.finite(q90) && pause_floor > q90 * 1.15 && pause_floor > lower) {
    upper <- min(upper, pause_floor * 0.98)
  }
  if (!is.finite(upper) || upper <= lower) upper <- max(c(raw_max, q75, lower + min_isi_sec), na.rm = TRUE)
  list(lower = lower, upper = upper, q10 = q10, q75 = q75, q90 = q90)
}

stpd_event_core_safe_seq <- function(s, e) {
  s <- as.integer(s); e <- as.integer(e)
  if (!is.finite(s) || !is.finite(e) || e < s) return(integer())
  s:e
}

stpd_event_core_is_enabled <- function(params) {
  ec <- params$event_core %||% list()
  isTRUE(ec$enabled %||% TRUE)
}

stpd_event_core_manual_isis <- function(dat, labels, min_isi_sec = 0.001) {
  if (is.null(dat) || !("pattern_manual" %in% names(dat)) || !("ISI_sec" %in% names(dat))) return(numeric())
  lab <- as.character(dat$pattern_manual); lab[is.na(lab)] <- ""
  isi <- suppressWarnings(as.numeric(dat$ISI_sec))
  art <- is_artifact_isi(isi, min_isi_sec)
  idx <- which(lab %in% labels & is.finite(isi) & !art)
  idx <- idx[idx >= 2]
  isi[idx]
}

stpd_event_core_manual_bands <- function(dat, params, min_isi_sec = 0.001) {
  ec <- params$event_core %||% list()
  use_manual <- isTRUE(ec$use_manual_isi_calibration %||% TRUE)
  if (!use_manual) {
    return(list(has_manual_burst = FALSE, manual_burst_n = 0L))
  }
  burst_vals <- stpd_event_core_manual_isis(dat, c("burst", "long_burst", "possible_burst"), min_isi_sec)
  hf_tonic_vals <- stpd_event_core_manual_isis(dat, c("high_frequency_tonic"), min_isi_sec)
  hf_spiking_vals <- stpd_event_core_manual_isis(dat, c("high_frequency_spiking"), min_isi_sec)
  tonic_vals <- stpd_event_core_manual_isis(dat, c("tonic"), min_isi_sec)
  list(
    has_manual_burst = length(burst_vals) >= stpd_event_core_int(ec$manual_min_burst_isi_count %||% 3L, 3L),
    manual_burst_n = length(burst_vals),
    burst_q40 = stpd_event_core_quantile(burst_vals, 0.40),
    burst_q90 = stpd_event_core_quantile(burst_vals, 0.90),
    burst_q95 = stpd_event_core_quantile(burst_vals, 0.95),
    has_manual_hf_tonic = length(hf_tonic_vals) >= 4L,
    hf_tonic_q10 = stpd_event_core_quantile(hf_tonic_vals, 0.10),
    hf_tonic_q90 = stpd_event_core_quantile(hf_tonic_vals, 0.90),
    has_manual_hf_spiking = length(hf_spiking_vals) >= 8L,
    hf_spiking_q90 = stpd_event_core_quantile(hf_spiking_vals, 0.90),
    has_manual_tonic = length(tonic_vals) >= 4L,
    tonic_q10 = stpd_event_core_quantile(tonic_vals, 0.10),
    tonic_q90 = stpd_event_core_quantile(tonic_vals, 0.90)
  )
}

stpd_event_core_params_impl <- function(dat, params, min_isi_sec = 0.001) {
  ec <- params$event_core %||% list()
  bp <- params$burst %||% list()
  hp <- params$highfreq %||% list()
  tp <- params$tonic %||% list()
  pp <- params$pause %||% list()
  mb <- stpd_event_core_manual_bands(dat, params, min_isi_sec)

  seed_low <- stpd_event_core_num(ec$seed_band_lower_sec %||% 0.001, 0.001)
  seed_high_user <- stpd_event_core_num(ec$seed_band_upper_sec %||% 0.010, 0.010)
  seed_low <- max(0, seed_low)
  seed_high <- if (isTRUE(mb$has_manual_burst) && isTRUE(ec$manual_can_expand_seed_band %||% TRUE)) {
    max(seed_high_user, stpd_event_core_num(mb$burst_q90, seed_high_user), na.rm = TRUE)
  } else seed_high_user
  if (!is.finite(seed_high) || seed_high <= seed_low) seed_high <- max(seed_low + min_isi_sec, 0.010)

  bridge_user <- stpd_event_core_num(ec$bridge_band_upper_sec %||% 0.015, 0.015)
  bridge_high <- if (isTRUE(mb$has_manual_burst) && isTRUE(ec$manual_can_expand_bridge_band %||% TRUE)) {
    max(bridge_user, stpd_event_core_num(mb$burst_q95, bridge_user), na.rm = TRUE)
  } else bridge_user
  if (!is.finite(bridge_high) || bridge_high <= 0) bridge_high <- seed_high * 1.5
  bridge_high <- max(bridge_high, seed_high)

  # Boundary floor is a reference/audit by default.  It is not a hard gate unless
  # boundary_floor_hard is explicitly enabled.  This prevents valid structures
  # such as 16-4-3-5-3-2-3-15 from being rejected by an arbitrary absolute floor.
  boundary_floor <- stpd_event_core_num(ec$boundary_floor_sec %||% 0, 0)
  if (!is.finite(boundary_floor) || boundary_floor < 0) boundary_floor <- 0

  s_default <- stpd_event_core_num(ec$burst_contrast_min %||% 2.5, 2.5)
  poss_default <- stpd_event_core_num(ec$possible_burst_contrast_min %||% 2.0, 2.0)

  extreme_upper <- if (isTRUE(mb$has_manual_burst) && is.finite(mb$burst_q40)) mb$burst_q40 else seed_high
  hf_tonic_floor_user <- stpd_event_core_num(hp$tonic_min_ISI_floor_sec %||% 0.010, 0.010)
  hf_tonic_floor <- if (is.finite(hf_tonic_floor_user) && hf_tonic_floor_user > 0) hf_tonic_floor_user else extreme_upper
  if (isTRUE(mb$has_manual_hf_tonic) && isTRUE(ec$manual_can_set_hf_tonic_floor %||% FALSE)) {
    hf_tonic_floor <- max(0, stpd_event_core_num(mb$hf_tonic_q10, hf_tonic_floor))
  }

  hf_q90_max <- stpd_event_core_num(hp$spiking_q90_max_ISI_sec %||% hp$spiking_max_ISI_abs %||% 0.020, 0.020)
  if (isTRUE(mb$has_manual_hf_spiking) && isTRUE(ec$manual_can_expand_hf_spiking_q90 %||% TRUE)) {
    hf_q90_max <- max(hf_q90_max, stpd_event_core_num(mb$hf_spiking_q90, hf_q90_max), na.rm = TRUE)
  }

  tonic_min <- stpd_event_core_num(tp$T_min %||% 0.020, 0.020)
  tonic_max <- stpd_event_core_num(tp$T_max %||% 0.060, 0.060)
  if (isTRUE(mb$has_manual_tonic) && isTRUE(ec$manual_can_set_tonic_band %||% FALSE)) {
    tonic_min <- stpd_event_core_num(mb$tonic_q10, tonic_min)
    tonic_max <- stpd_event_core_num(mb$tonic_q90, tonic_max)
  }

  list(
    seed_low = seed_low,
    seed_high = seed_high,
    seed_high_user = seed_high_user,
    bridge_high = bridge_high,
    bridge_user = bridge_user,
    boundary_floor = boundary_floor,
    boundary_floor_hard = isTRUE(ec$boundary_floor_hard %||% FALSE),
    S = max(1, s_default),
    S_possible = max(1, poss_default),
    min_seed_isi_n = max(1L, stpd_event_core_int(ec$min_seed_isi_count %||% 2L, 2L)),
    max_bridge_n = max(0L, stpd_event_core_int(ec$max_bridge_isi_count %||% 4L, 4L)),
    max_bridge_frac = min(max(stpd_event_core_num(ec$max_bridge_isi_fraction %||% 0.60, 0.60), 0), 1),
    max_expand = max(0L, stpd_event_core_int(ec$max_expansion_isi_each_side %||% 4L, 4L)),
    min_spikes = max(2L, stpd_event_core_int(ec$min_spikes %||% bp$G_min %||% 3L, 3L)),
    classic_max_spikes = max(3L, stpd_event_core_int(ec$classic_max_spikes %||% bp$classic_burst_max_spikes %||% 10L, 10L)),
    long_min_spikes = max(3L, stpd_event_core_int(ec$long_min_spikes %||% bp$long_burst_min_spikes %||% 11L, 11L)),
    long_max_spikes = stpd_event_core_int(ec$long_max_spikes %||% bp$long_burst_max_spikes %||% 15L, 15L),
    prolonged_min_spikes = stpd_event_core_int(ec$prolonged_min_spikes %||% 16L, 16L),
    prolonged_max_spikes = stpd_event_core_int(ec$prolonged_max_spikes %||% 29L, 29L),
    max_candidates = max(100L, stpd_event_core_int(ec$max_candidates_per_train %||% 3000L, 3000L)),
    allow_boundary_possible = isTRUE(ec$allow_boundary_possible_burst %||% TRUE),
    extreme_core_upper = extreme_upper,
    manual_bands = mb,
    hf_spiking_min_spikes = max(3L, stpd_event_core_int(hp$spiking_min_spikes %||% 30L, 30L)),
    hf_spiking_min_duration = max(0, stpd_event_core_num(hp$spiking_min_duration %||% 0, 0)),
    hf_spiking_epoch_bridge = stpd_event_core_num(hp$spiking_epoch_bridge_ISI_sec %||% 0.030, 0.030),
    hf_spiking_q90_max = hf_q90_max,
    hf_spiking_short_fraction_min = min(max(stpd_event_core_num(hp$spiking_short_fraction_min %||% 0.70, 0.70), 0), 1),
    hf_spiking_allowed_large_frac = min(max(stpd_event_core_num(hp$spiking_allowed_large_isi_fraction %||% 0.20, 0.20), 0), 1),
    hf_spiking_max_consec_large = max(0L, stpd_event_core_int(hp$spiking_max_consecutive_large_isi %||% 2L, 2L)),
    hf_spiking_break_isi = stpd_event_core_num(hp$spiking_hard_break_ISI_sec %||% max(stpd_event_core_num(pp$T_seed %||% 0.100, 0.100), 2 * stpd_event_core_num(hp$spiking_epoch_bridge_ISI_sec %||% 0.030, 0.030)), 0.100),
    hf_tonic_floor = hf_tonic_floor,
    hf_tonic_high_max = stpd_event_core_num(hp$T_high_max %||% 0.020, 0.020),
    hf_tonic_low_tail_max = min(max(stpd_event_core_num(hp$tonic_low_tail_fraction_max %||% 0.05, 0.05), 0), 1),
    hf_tonic_burst_core_veto = isTRUE(hp$tonic_burst_core_veto %||% TRUE),
    hf_tonic_core_veto_min_isi_n = max(1L, stpd_event_core_int(hp$tonic_burst_core_veto_min_isi_n %||% 2L, 2L)),
    hf_tonic_min_spikes = max(3L, stpd_event_core_int(hp$G_min %||% 6L, 6L)),
    hf_tonic_cv_max = stpd_event_core_num(hp$stable_CV_max %||% 0.30, 0.30),
    hf_tonic_lv_max = stpd_event_core_num(hp$stable_LV_max %||% 0.35, 0.35),
    hf_tonic_mm_max = stpd_event_core_num(hp$stable_MM_max %||% 1.25, 1.25),
    tonic_min = tonic_min,
    tonic_max = tonic_max,
    tonic_min_spikes = max(3L, stpd_event_core_int(tp$G_min %||% 5L, 5L)),
    tonic_lv_max = stpd_event_core_num(tp$LV_core %||% 0.5, 0.5),
    tonic_mm_max = stpd_event_core_num(tp$tonic_mm_max %||% 1.25, 1.25),
    tonic_mm_min = stpd_event_core_num(tp$tonic_mm_min %||% 0.85, 0.85),
    tonic_burst_overlap_ref = suppressWarnings(max(c(seed_high, bridge_high, mb$burst_q95), na.rm = TRUE)),
    tonic_burst_overlap_guard = isTRUE(tp$burst_overlap_guard %||% TRUE),
    tonic_burst_overlap_guard_factor = stpd_event_core_num(tp$burst_overlap_guard_factor %||% 1.15, 1.15),
    tonic_burst_overlap_lower_quantile = stpd_event_core_num(tp$burst_overlap_lower_quantile %||% 0.10, 0.10),
    tonic_burst_overlap_low_fraction_max = stpd_event_core_num(tp$burst_overlap_low_fraction_max %||% 0.05, 0.05),
    tonic_burst_overlap_reference_quantile = stpd_event_core_num(tp$burst_overlap_reference_quantile %||% 0.95, 0.95),
    pause_thr = stpd_event_core_num(pp$T_seed %||% 0.100, 0.100)
  )
}

stpd_event_core_train_profile_row <- function(dat, params, vp, min_isi_sec = 0.001, train = "") {
  isi <- suppressWarnings(as.numeric(dat$ISI_sec))
  art <- is_artifact_isi(isi, min_isi_sec)
  valid <- is.finite(isi) & !art
  if (length(valid) > 0) valid[1] <- FALSE
  vals <- isi[valid]
  seed_flag <- valid & isi >= vp$seed_low & isi <= vp$seed_high
  seed_runs <- stpd_event_core_bool_runs(seed_flag)
  seed_frac <- if (length(vals) > 0) mean(vals >= vp$seed_low & vals <= vp$seed_high, na.rm = TRUE) else NA_real_
  seed_high_pct <- if (length(vals) > 0) mean(vals <= vp$seed_high, na.rm = TRUE) * 100 else NA_real_
  seed_low_pct <- if (length(vals) > 0) mean(vals <= vp$seed_low, na.rm = TRUE) * 100 else NA_real_
  pause_frac <- if (length(vals) > 0 && is.finite(vp$pause_thr)) mean(vals >= vp$pause_thr, na.rm = TRUE) else NA_real_
  max_seed_run <- if (nrow(seed_runs) > 0) max(seed_runs$end_isi - seed_runs$start_isi + 1L) else 0L
  phenotype <- "mixed"
  if (length(vals) < 10) phenotype <- "low_spike_count_unreliable"
  else if (is.finite(seed_frac) && seed_frac >= 0.40 && max_seed_run >= 10L) phenotype <- "hf_spiking_like_seed_dominant"
  else if (is.finite(seed_frac) && seed_frac <= 0.02) phenotype <- "seed_sparse_tonic_or_slow"
  else if (is.finite(pause_frac) && pause_frac >= 0.25) phenotype <- "pause_dominant"
  else if (nrow(seed_runs) > 0 && is.finite(seed_frac) && seed_frac > 0.02) phenotype <- "burst_capable"
  data.frame(
    train = as.character(train %||% ""),
    candidate_layer = "event_core_train_isi_band_profile",
    candidate_class = "train_profile",
    final_label = "profile",
    gate_status = "profile",
    decision_path = "dataset_manual_isi_band_profile_percentiles_are_outputs",
    action = "audit_only",
    selected_for_auto = FALSE,
    start_isi = NA_integer_, end_isi = NA_integer_, n_spikes = NA_integer_, n_isi = NA_integer_,
    n_valid_isi = length(vals),
    event_core_seed_low_sec = vp$seed_low,
    event_core_seed_high_sec = vp$seed_high,
    event_core_bridge_high_sec = vp$bridge_high,
    event_core_boundary_floor_sec = vp$boundary_floor,
    event_core_boundary_floor_hard = vp$boundary_floor_hard,
    burst_contrast_S = vp$S,
    possible_contrast_S = vp$S_possible,
    seed_low_percentile_in_train = seed_low_pct,
    seed_high_percentile_in_train = seed_high_pct,
    seed_band_fraction = seed_frac,
    seed_run_count = nrow(seed_runs),
    max_seed_run_length = max_seed_run,
    median_ISI_sec = if (length(vals) > 0) stats::median(vals, na.rm = TRUE) else NA_real_,
    q10_ISI_sec = stpd_event_core_quantile(vals, 0.10),
    q25_ISI_sec = stpd_event_core_quantile(vals, 0.25),
    q90_ISI_sec = stpd_event_core_quantile(vals, 0.90),
    pause_fraction = pause_frac,
    phenotype_prior = phenotype,
    stringsAsFactors = FALSE
  )
}

stpd_event_core_span_metrics <- function(dat, s, e, params, vp, min_isi_sec = 0.001, train = "", label = "") {
  n <- nrow(dat)
  s <- as.integer(s); e <- as.integer(e)
  if (!is.finite(s) || !is.finite(e) || e < s || s < 2L || e > n) return(NULL)
  isi <- suppressWarnings(as.numeric(dat$ISI_sec))
  art <- is_artifact_isi(isi, min_isi_sec)
  idx <- s:e
  vals <- isi[idx]
  valid_idx <- idx[is.finite(vals) & !art[idx]]
  vals <- isi[valid_idx]
  if (length(vals) == 0) return(NULL)
  q10 <- stpd_event_core_quantile(vals, 0.10)
  q40 <- stpd_event_core_quantile(vals, 0.40)
  q50 <- stpd_event_core_quantile(vals, 0.50)
  q90 <- stpd_event_core_quantile(vals, 0.90)
  q95 <- stpd_event_core_quantile(vals, 0.95)
  pre <- if (s > 2L) isi[s - 1L] else NA_real_
  post <- if (e < n) isi[e + 1L] else NA_real_
  min_edge <- suppressWarnings(min(pre, post, na.rm = TRUE))
  if (!is.finite(min_edge)) min_edge <- NA_real_
  contrast <- if (is.finite(min_edge) && is.finite(q90) && q90 > 0) min_edge / q90 else NA_real_
  duration <- NA_real_
  if (s > 1L && e <= n && "timestamp_sec" %in% names(dat)) {
    t0 <- suppressWarnings(as.numeric(dat$timestamp_sec[s - 1L]))
    t1 <- suppressWarnings(as.numeric(dat$timestamp_sec[e]))
    if (is.finite(t0) && is.finite(t1)) duration <- t1 - t0
  }
  neg_overlap <- FALSE
  neg_frac <- 0
  if ("pattern_manual_negative" %in% names(dat)) {
    neg <- as.character(dat$pattern_manual_negative); neg[is.na(neg)] <- ""
    neg_overlap <- any(neg[idx] %in% c("not_burst", "hard_negative_burst", "not_hf", "not_high_frequency", "hard_negative"), na.rm = TRUE)
    neg_frac <- mean(neg[idx] != "", na.rm = TRUE)
  }
  core_count <- sum(isi[idx] >= vp$seed_low & isi[idx] <= vp$seed_high & is.finite(isi[idx]) & !art[idx], na.rm = TRUE)
  bridge_count <- sum(isi[idx] > vp$seed_high & isi[idx] <= vp$bridge_high & is.finite(isi[idx]) & !art[idx], na.rm = TRUE)
  data.frame(
    train = as.character(train %||% ""),
    start_isi = s,
    end_isi = e,
    n_isi = length(idx),
    n_valid_isi = length(vals),
    n_spikes = e - s + 2L,
    duration_sec = duration,
    intra_q10_sec = q10,
    intra_q40_sec = q40,
    intra_q50_sec = q50,
    intra_q90_sec = q90,
    intra_q95_sec = q95,
    max_intra_ISI_sec = max(vals, na.rm = TRUE),
    mean_intra_ISI_sec = mean(vals, na.rm = TRUE),
    CV = stpd_event_core_cv(vals),
    LV = stpd_event_core_lv(vals),
    MM = stpd_event_core_mm(vals),
    pre_gap_sec = pre,
    post_gap_sec = post,
    burst_contrast_score = contrast,
    edge_ratio = contrast,
    core_isi_count = core_count,
    bridge_isi_count = bridge_count,
    bridge_fraction = bridge_count / max(1L, length(vals)),
    manual_negative_veto = neg_overlap,
    manual_negative_overlap_fraction = neg_frac,
    stringsAsFactors = FALSE
  )
}

stpd_event_core_candidate_row <- function(metrics, layer, cls, final_label, status, decision, action, score, priority, extra = list()) {
  if (is.null(metrics) || nrow(metrics) == 0) return(NULL)
  m <- metrics
  m$candidate_layer <- layer
  m$candidate_class <- cls
  m$class <- final_label
  m$final_label <- final_label
  m$gate_status <- status
  m$decision_path <- decision
  m$action <- action
  m$score <- score
  m$priority <- priority
  m$selected_for_auto <- FALSE
  m$selection_status <- "not_selected"
  if (length(extra) > 0) {
    for (nm in names(extra)) m[[nm]] <- extra[[nm]]
  }
  m
}

stpd_event_core_left_extensions <- function(seed_s, isi, valid, bridge_high, max_steps) {
  out <- as.integer(seed_s)
  cur <- as.integer(seed_s)
  steps <- 0L
  while (cur > 2L && steps < max_steps) {
    z <- cur - 1L
    if (!isTRUE(valid[z]) || !is.finite(isi[z]) || isi[z] > bridge_high) break
    out <- c(out, z); cur <- z; steps <- steps + 1L
  }
  unique(as.integer(out))
}

stpd_event_core_right_extensions <- function(seed_e, n, isi, valid, bridge_high, max_steps) {
  out <- as.integer(seed_e)
  cur <- as.integer(seed_e)
  steps <- 0L
  while (cur < n && steps < max_steps) {
    z <- cur + 1L
    if (z > n || !isTRUE(valid[z]) || !is.finite(isi[z]) || isi[z] > bridge_high) break
    out <- c(out, z); cur <- z; steps <- steps + 1L
  }
  unique(as.integer(out))
}

stpd_event_core_detect_burst_events <- function(dat, params, vp, min_isi_sec = 0.001, train = "") {
  n <- nrow(dat); rows <- list()
  if (n <= 2) return(data.frame())
  isi <- suppressWarnings(as.numeric(dat$ISI_sec))
  art <- is_artifact_isi(isi, min_isi_sec)
  valid <- is.finite(isi) & !art
  valid[1] <- FALSE
  seed_flag <- valid & isi >= vp$seed_low & isi <= vp$seed_high
  runs <- stpd_event_core_bool_runs(seed_flag)
  if (nrow(runs) == 0) return(data.frame())
  seen <- list(); counter <- 0L
  for (rr in seq_len(nrow(runs))) {
    ss <- as.integer(runs$start_isi[rr]); ee <- as.integer(runs$end_isi[rr])
    if (sum(seed_flag[ss:ee], na.rm = TRUE) < vp$min_seed_isi_n) next
    lefts <- stpd_event_core_left_extensions(ss, isi, valid, vp$bridge_high, vp$max_expand)
    rights <- stpd_event_core_right_extensions(ee, n, isi, valid, vp$bridge_high, vp$max_expand)
    for (s in lefts) {
      for (e in rights) {
        if (length(rows) >= vp$max_candidates) break
        key <- paste0(s, "_", e)
        if (!is.null(seen[[key]])) next
        seen[[key]] <- TRUE
        idx <- stpd_event_core_safe_seq(s, e)
        if (length(idx) == 0) next
        core_n <- sum(seed_flag[idx], na.rm = TRUE)
        if (core_n < vp$min_seed_isi_n) next
        n_spikes <- e - s + 2L
        if (n_spikes < vp$min_spikes) next
        m <- stpd_event_core_span_metrics(dat, s, e, params, vp, min_isi_sec, train, "event_core_burst_event")
        if (is.null(m)) next
        bridge_count_pass <- is.finite(m$bridge_isi_count[1]) && m$bridge_isi_count[1] <= vp$max_bridge_n
        bridge_fraction_pass <- is.finite(m$bridge_fraction[1]) && m$bridge_fraction[1] <= vp$max_bridge_frac
        q90_pass <- is.finite(m$intra_q90_sec[1]) && m$intra_q90_sec[1] <= vp$bridge_high
        has_two_edges <- is.finite(m$pre_gap_sec[1]) && is.finite(m$post_gap_sec[1])
        req_strict <- vp$S * m$intra_q90_sec[1]
        req_possible <- vp$S_possible * m$intra_q90_sec[1]
        if (isTRUE(vp$boundary_floor_hard) && is.finite(vp$boundary_floor) && vp$boundary_floor > 0) {
          req_strict <- max(req_strict, vp$boundary_floor)
        }
        strict_boundary <- has_two_edges && is.finite(req_strict) && m$pre_gap_sec[1] >= req_strict && m$post_gap_sec[1] >= req_strict
        possible_boundary <- has_two_edges && is.finite(req_possible) && m$pre_gap_sec[1] >= req_possible && m$post_gap_sec[1] >= req_possible
        one_edge_possible <- !has_two_edges && isTRUE(vp$allow_boundary_possible) && is.finite(m$burst_contrast_score[1]) && m$burst_contrast_score[1] >= vp$S_possible
        neg <- isTRUE(m$manual_negative_veto[1])
        size_label <- "prolonged_burst_like"
        if (n_spikes <= vp$classic_max_spikes) size_label <- "burst"
        else if (n_spikes >= vp$long_min_spikes && (vp$long_max_spikes <= 0 || n_spikes <= vp$long_max_spikes)) size_label <- "long_burst"
        final <- "reject"; status <- "event_core_reject"; action <- "reject"; decision <- "event_core_reject"
        priority <- 0
        if (!neg && bridge_count_pass && bridge_fraction_pass && q90_pass && strict_boundary) {
          if (size_label %in% c("burst", "long_burst")) {
            final <- size_label; status <- "event_core_strict_burst_event_pass"; action <- "accept"
            decision <- paste0("seed_centered_event_grammar_pass__", size_label)
            priority <- if (final == "burst") 1000 else 980
          } else {
            final <- "possible_burst"; status <- "event_core_prolonged_burst_like_review"; action <- "demote_to_possible"
            decision <- "strict_burst_structure_but_spike_count_exceeds_long_burst_range"
            priority <- 120
          }
        } else if (!neg && bridge_count_pass && bridge_fraction_pass && q90_pass && (possible_boundary || one_edge_possible)) {
          final <- "possible_burst"; status <- if (one_edge_possible) "event_core_boundary_possible_burst" else "event_core_possible_burst"
          action <- "demote_to_possible"; decision <- "seed_centered_possible_burst_contrast_review"
          priority <- 120
        } else {
          reasons <- c(
            if (neg) "manual_negative_veto",
            if (!bridge_count_pass) "too_many_bridge_isis",
            if (!bridge_fraction_pass) "bridge_fraction_too_high",
            if (!q90_pass) "intra_q90_exceeds_bridge_band",
            if (!strict_boundary && !possible_boundary && !one_edge_possible) "burst_contrast_boundary_fail"
          )
          decision <- paste(reasons, collapse = ";")
          if (!nzchar(decision)) decision <- "event_core_reject"
        }
        score <- (if (is.finite(m$burst_contrast_score[1])) m$burst_contrast_score[1] else 0) +
          0.08 * m$core_isi_count[1] - 0.15 * m$bridge_isi_count[1] - 0.25 * m$bridge_fraction[1]
        counter <- counter + 1L
        rows[[length(rows) + 1L]] <- stpd_event_core_candidate_row(
          m, "event_core_burst_event", "event_core_seed_centered_burst", final, status, decision, action, score, priority,
          list(candidate_id = paste0("event_core_burst_", counter), seed_run_start_isi = ss, seed_run_end_isi = ee,
               seed_band_lower_sec = vp$seed_low, seed_band_upper_sec = vp$seed_high,
               bridge_band_upper_sec = vp$bridge_high, burst_contrast_required = vp$S,
               possible_contrast_required = vp$S_possible,
               required_gap_sec = req_strict, possible_required_gap_sec = req_possible,
               boundary_floor_sec = vp$boundary_floor, boundary_floor_hard = vp$boundary_floor_hard,
               strict_boundary_pass = strict_boundary, possible_boundary_pass = possible_boundary || one_edge_possible,
               bridge_count_pass = bridge_count_pass, bridge_fraction_pass = bridge_fraction_pass,
               q90_bridge_pass = q90_pass, size_label_before_review = size_label)
        )
      }
      if (length(rows) >= vp$max_candidates) break
    }
    if (length(rows) >= vp$max_candidates) break
  }
  if (length(rows) == 0) return(data.frame())
  dplyr::bind_rows(rows)
}

stpd_event_core_candidate_from_run <- function(dat, s, e, params, vp, min_isi_sec, train, layer, cls, label, status, decision, action, score, priority, extra = list()) {
  m <- stpd_event_core_span_metrics(dat, s, e, params, vp, min_isi_sec, train, cls)
  if (is.null(m)) return(NULL)
  stpd_event_core_candidate_row(m, layer, cls, label, status, decision, action, score, priority, extra)
}

stpd_event_core_direct_hard_runs <- function(flag, valid, min_run_isi = 1L) {
  flag <- as.logical(flag) & as.logical(valid)
  flag[is.na(flag)] <- FALSE
  runs <- stpd_event_core_bool_runs(flag)
  if (nrow(runs) == 0) return(runs)
  min_run_isi <- max(1L, stpd_event_core_int(min_run_isi, 1L))
  runs[(runs$end_isi - runs$start_isi + 1L) >= min_run_isi, , drop = FALSE]
}

stpd_event_core_detect_hard_isi_thresholds <- function(dat, params, vp, min_isi_sec = 0.001, train = "") {
  n <- nrow(dat)
  if (n <= 1L || !nzchar(as.character(train %||% ""))) return(data.frame())
  isi <- suppressWarnings(as.numeric(dat$ISI_sec))
  art <- is_artifact_isi(isi, min_isi_sec)
  valid <- is.finite(isi) & !art
  valid[1] <- FALSE

  rows <- list()
  counter <- 0L
  add_row <- function(s, e, layer, cls, label, status, decision, score, priority, extra) {
    counter <<- counter + 1L
    extra$candidate_id <- paste0("hard_isi_threshold_", counter)
    row <- stpd_event_core_candidate_from_run(
      dat, s, e, params, vp, min_isi_sec, train,
      layer, cls, label, status, decision, "accept",
      score, priority, extra
    )
    if (!is.null(row) && nrow(row) > 0) rows[[length(rows) + 1L]] <<- row
    invisible(NULL)
  }

  brr <- get_train_burst_range(params$burst %||% list(), train = train)
  if (!is.null(brr) && isTRUE(stpd_train_isi_threshold_is_hard(brr))) {
    bmax <- range_value(brr, "high_sec", NA_real_)
    if (is.finite(bmax) && bmax > 0) {
      bridge_high <- max(c(bmax, stpd_event_core_num(vp$bridge_high, bmax), bmax * 1.25), na.rm = TRUE)
      seed_flag <- valid & isi <= bmax
      bridge_flag <- valid & isi <= bridge_high
      runs <- stpd_event_core_bool_runs(bridge_flag)
      min_core <- max(1L, stpd_event_core_int(vp$min_seed_isi_n %||% 2L, 2L))
      min_spikes <- max(2L, stpd_event_core_int(vp$min_spikes %||% 3L, 3L))
      for (rr in seq_len(nrow(runs))) {
        s <- as.integer(runs$start_isi[rr]); e <- as.integer(runs$end_isi[rr])
        idx <- stpd_event_core_safe_seq(s, e)
        if (length(idx) == 0) next
        core_n <- sum(seed_flag[idx], na.rm = TRUE)
        n_spikes <- e - s + 2L
        if (core_n < min_core || n_spikes < min_spikes) next
        size_label <- if (n_spikes <= stpd_event_core_int(vp$classic_max_spikes %||% 10L, 10L)) "burst" else "long_burst"
        score <- 30 + core_n + 0.05 * n_spikes - 0.25 * sum(bridge_flag[idx] & !seed_flag[idx], na.rm = TRUE)
        add_row(
          s, e,
          "isi_profile_hard_threshold_burst",
          "isi_profile_hard_threshold_burst",
          size_label,
          "isi_profile_hard_threshold_burst_pass",
          "hard_threshold_direct_seed_bridge_without_flank_contrast_gate",
          score,
          if (identical(size_label, "burst")) 1450 else 1360,
          list(
            threshold_mode = "hard_threshold",
            hard_threshold = TRUE,
            hard_threshold_pattern = "burst",
            hard_burst_seed_upper_sec = bmax,
            hard_burst_bridge_upper_sec = bridge_high,
            hard_burst_core_isi_count = as.integer(core_n),
            hard_threshold_source = as.character(brr$source %||% "ui_isi_profile_threshold_line")
          )
        )
      }
    }
  }

  if (length(rows) == 0) return(data.frame())
  dplyr::bind_rows(rows)
}

stpd_event_core_detect_hf_spiking <- function(dat, params, vp, min_isi_sec = 0.001, train = "") {
  n <- nrow(dat); if (n <= 2) return(data.frame())
  isi <- suppressWarnings(as.numeric(dat$ISI_sec))
  art <- is_artifact_isi(isi, min_isi_sec)
  valid <- is.finite(isi) & !art
  valid[1] <- FALSE
  break_thr <- max(vp$hf_spiking_break_isi, vp$hf_spiking_epoch_bridge, vp$hf_spiking_q90_max, na.rm = TRUE)
  pause_break <- suppressWarnings(as.numeric(vp$pause_thr %||% NA_real_))[1]
  if (!is.finite(pause_break) || pause_break <= 0) pause_break <- NA_real_
  base_flag <- valid & isi <= break_thr
  if (is.finite(pause_break)) base_flag <- base_flag & isi < pause_break
  runs <- stpd_event_core_bool_runs(base_flag)
  rows <- list(); counter <- 0L
  for (rr in seq_len(nrow(runs))) {
    s <- as.integer(runs$start_isi[rr]); e <- as.integer(runs$end_isi[rr])
    idx <- stpd_event_core_safe_seq(s, e); if (length(idx) == 0) next
    vals <- isi[idx][valid[idx]]; if (length(vals) == 0) next
    n_spikes <- e - s + 2L
    if (n_spikes < vp$hf_spiking_min_spikes) next
    duration <- if ("timestamp_sec" %in% names(dat)) suppressWarnings(as.numeric(dat$timestamp_sec[e]) - as.numeric(dat$timestamp_sec[s - 1L])) else NA_real_
    if (is.finite(vp$hf_spiking_min_duration) && vp$hf_spiking_min_duration > 0 && (!is.finite(duration) || duration < vp$hf_spiking_min_duration)) next
    short_frac <- mean(vals <= vp$hf_spiking_q90_max, na.rm = TRUE)
    large_flag <- vals > vp$hf_spiking_epoch_bridge
    large_frac <- mean(large_flag, na.rm = TRUE)
    max_consec_large <- stpd_event_core_max_consecutive_true(large_flag)
    q90 <- stpd_event_core_quantile(vals, 0.90)
    pass <- is.finite(q90) && q90 <= vp$hf_spiking_q90_max &&
      short_frac >= vp$hf_spiking_short_fraction_min &&
      large_frac <= vp$hf_spiking_allowed_large_frac &&
      max_consec_large <= vp$hf_spiking_max_consec_large
    if (!pass) next
    score <- 10 + 0.03 * n_spikes + short_frac - large_frac
    counter <- counter + 1L
    row <- stpd_event_core_candidate_from_run(dat, s, e, params, vp, min_isi_sec, train,
      "event_core_hf_spiking_state", "event_core_long_hf_spiking_epoch", "high_frequency_spiking",
      "event_core_hf_spiking_pass", "long_high_frequency_epoch_without_required_burst_event_grammar", "accept",
      score, 700,
      list(candidate_id = paste0("event_core_hfs_", counter), hf_spiking_q90_sec = q90,
           hf_spiking_short_fraction = short_frac, hf_spiking_large_fraction = large_frac,
           hf_spiking_max_consecutive_large_isi = max_consec_large,
           hf_spiking_pause_break_sec = pause_break,
           hf_spiking_min_spikes_required = vp$hf_spiking_min_spikes))
    rows[[length(rows) + 1L]] <- row
  }
  if (length(rows) == 0) return(data.frame())
  dplyr::bind_rows(rows)
}

stpd_event_core_detect_hf_tonic <- function(dat, params, vp, min_isi_sec = 0.001, train = "") {
  n <- nrow(dat); if (n <= 2) return(data.frame())
  isi <- suppressWarnings(as.numeric(dat$ISI_sec))
  art <- is_artifact_isi(isi, min_isi_sec)
  valid <- is.finite(isi) & !art
  valid[1] <- FALSE
  flag <- valid & isi <= vp$hf_tonic_high_max
  runs <- stpd_event_core_bool_runs(flag)
  rows <- list(); counter <- 0L
  seed_flag <- valid & isi >= vp$seed_low & isi <= vp$seed_high
  for (rr in seq_len(nrow(runs))) {
    s <- as.integer(runs$start_isi[rr]); e <- as.integer(runs$end_isi[rr])
    idx <- stpd_event_core_safe_seq(s, e); if (length(idx) == 0) next
    vals <- isi[idx][valid[idx]]; if (length(vals) == 0) next
    n_spikes <- e - s + 2L
    if (n_spikes < vp$hf_tonic_min_spikes) next
    q10 <- stpd_event_core_quantile(vals, 0.10); q90 <- stpd_event_core_quantile(vals, 0.90)
    low_tail <- mean(vals < vp$hf_tonic_floor, na.rm = TRUE)
    core_run_len <- stpd_event_core_max_consecutive_true(seed_flag[idx])
    cv <- stpd_event_core_cv(vals); lv <- stpd_event_core_lv(vals); mm <- stpd_event_core_mm(vals)
    pass <- is.finite(q90) && q90 <= vp$hf_tonic_high_max &&
      (is.finite(q10) && q10 >= vp$hf_tonic_floor || low_tail <= vp$hf_tonic_low_tail_max) &&
      (!isTRUE(vp$hf_tonic_burst_core_veto) || core_run_len < vp$hf_tonic_core_veto_min_isi_n) &&
      (!is.finite(cv) || cv <= vp$hf_tonic_cv_max) &&
      (!is.finite(lv) || lv <= vp$hf_tonic_lv_max) &&
      (!is.finite(mm) || mm <= vp$hf_tonic_mm_max)
    if (!pass) next
    score <- 5 + (1 - min(low_tail, 1)) + if (is.finite(cv)) 1 / (1 + cv) else 0
    counter <- counter + 1L
    rows[[length(rows) + 1L]] <- stpd_event_core_candidate_from_run(dat, s, e, params, vp, min_isi_sec, train,
      "event_core_hf_tonic_state", "event_core_hf_tonic", "high_frequency_tonic",
      "event_core_hf_tonic_pass", "stable_high_frequency_tonic_state_above_extreme_burst_core_floor", "accept",
      score, 500,
      list(candidate_id = paste0("event_core_hft_", counter), hf_tonic_floor_sec = vp$hf_tonic_floor,
           hf_tonic_low_tail_fraction = low_tail, hf_tonic_core_run_len = core_run_len,
           hf_tonic_q10_sec = q10, hf_tonic_q90_sec = q90))
  }
  if (length(rows) == 0) return(data.frame())
  dplyr::bind_rows(rows)
}

stpd_event_core_detect_tonic <- function(dat, params, vp, min_isi_sec = 0.001, train = "") {
  n <- nrow(dat); if (n <= 2) return(data.frame())
  isi <- suppressWarnings(as.numeric(dat$ISI_sec))
  art <- is_artifact_isi(isi, min_isi_sec)
  valid <- is.finite(isi) & !art
  valid[1] <- FALSE
  overlap_p <- list(
    burst_overlap_guard = vp$tonic_burst_overlap_guard,
    burst_overlap_guard_factor = vp$tonic_burst_overlap_guard_factor,
    burst_overlap_lower_quantile = vp$tonic_burst_overlap_lower_quantile,
    burst_overlap_low_fraction_max = vp$tonic_burst_overlap_low_fraction_max,
    burst_overlap_reference_quantile = vp$tonic_burst_overlap_reference_quantile
  )
  bounds <- stpd_event_core_tonic_adaptive_bounds(isi, valid, vp, min_isi_sec)
  tonic_lower <- stpd_event_core_num(bounds$lower, vp$tonic_min)
  tonic_upper <- stpd_event_core_num(bounds$upper, vp$tonic_max)
  upper_ok <- if (!is.finite(tonic_upper) || tonic_upper <= 0) rep(TRUE, length(isi)) else isi <= tonic_upper
  flag <- valid & isi >= tonic_lower & upper_ok
  runs <- stpd_event_core_bool_runs(flag)
  rows <- list(); counter <- 0L
  seen <- list()
  add_tonic_run <- function(s, e, source = "full_run", source_s = s, source_e = e) {
    key <- paste0(as.integer(s), "_", as.integer(e), "_", source)
    if (!is.null(seen[[key]])) return(NULL)
    seen[[key]] <<- TRUE
    idx <- stpd_event_core_safe_seq(s, e); vals <- isi[idx][valid[idx]]
    if (length(vals) == 0) return(NULL)
    n_spikes <- e - s + 2L
    if (n_spikes < vp$tonic_min_spikes) return(NULL)
    if (!stpd_tonic_burst_overlap_ok(vals, vp$tonic_burst_overlap_ref, p = overlap_p, min_isi_sec = min_isi_sec)) return(NULL)
    lv <- stpd_event_core_lv(vals); mm <- stpd_event_core_mm(vals); cv <- stpd_event_core_cv(vals)
    if (is.finite(lv) && lv > vp$tonic_lv_max) return(NULL)
    tonic_mm_max_effective <- vp$tonic_mm_max
    if (is.finite(lv) && lv <= min(vp$tonic_lv_max, 0.15, na.rm = TRUE) &&
        is.finite(cv) && cv <= 0.30) {
      tonic_mm_max_effective <- max(tonic_mm_max_effective, 1.40, na.rm = TRUE)
    }
    if (is.finite(mm) && mm > tonic_mm_max_effective) return(NULL)
    if (is.finite(mm) && mm < vp$tonic_mm_min) return(NULL)
    seed_frac <- mean(vals >= vp$seed_low & vals <= vp$seed_high, na.rm = TRUE)
    if (is.finite(seed_frac) && seed_frac > 0.20) return(NULL)
    score <- 3 + if (is.finite(lv)) 1 / (1 + lv) else 0
    counter <<- counter + 1L
    gate <- if (identical(source, "core_trim")) "event_core_tonic_core_trim_pass" else "event_core_tonic_pass"
    decision <- if (identical(source, "core_trim")) "stable_mid_isi_tonic_core_after_transition_trim" else "stable_mid_isi_tonic_state"
    priority <- if (identical(source, "core_trim")) 340 else 350
    row <- stpd_event_core_candidate_from_run(dat, s, e, params, vp, min_isi_sec, train,
      "event_core_tonic_state", "event_core_tonic", "tonic",
      gate, decision, "accept",
      score, priority,
      list(candidate_id = paste0("event_core_tonic_", counter),
           tonic_seed_fraction = seed_frac,
           tonic_adaptive_lower_sec = tonic_lower,
           tonic_adaptive_upper_sec = tonic_upper,
           tonic_train_q10_sec = bounds$q10,
           tonic_train_q75_sec = bounds$q75,
           tonic_train_q90_sec = bounds$q90,
           tonic_cv = cv,
           tonic_mm_effective_max = tonic_mm_max_effective,
           tonic_burst_overlap_ref_sec = vp$tonic_burst_overlap_ref,
           tonic_burst_overlap_guard = isTRUE(overlap_p$burst_overlap_guard),
           tonic_candidate_source = source,
           tonic_source_run_start_isi = source_s,
           tonic_source_run_end_isi = source_e))
    if (!is.null(row) && nrow(row) > 0) rows[[length(rows) + 1L]] <<- row
    invisible(NULL)
  }
  core_lower <- max(c(tonic_lower, stpd_event_core_num(bounds$q75, tonic_lower) * 0.65), na.rm = TRUE)
  if (!is.finite(core_lower) || core_lower > tonic_upper) core_lower <- tonic_lower
  for (rr in seq_len(nrow(runs))) {
    s <- as.integer(runs$start_isi[rr]); e <- as.integer(runs$end_isi[rr])
    add_tonic_run(s, e, source = "full_run", source_s = s, source_e = e)
    idx <- stpd_event_core_safe_seq(s, e)
    if (length(idx) == 0) next
    core_flag <- valid[idx] & is.finite(isi[idx]) & isi[idx] >= core_lower & isi[idx] <= tonic_upper
    core_runs <- stpd_event_core_bool_runs(core_flag)
    if (nrow(core_runs) == 0) next
    for (cc in seq_len(nrow(core_runs))) {
      cs <- idx[as.integer(core_runs$start_isi[cc])]
      ce <- idx[as.integer(core_runs$end_isi[cc])]
      if (!is.finite(cs) || !is.finite(ce) || cs > ce) next
      if (cs == s && ce == e) next
      add_tonic_run(cs, ce, source = "core_trim", source_s = s, source_e = e)
    }
  }
  if (length(rows) == 0) return(data.frame())
  dplyr::bind_rows(rows)
}

stpd_event_core_detect_pause <- function(dat, params, vp, min_isi_sec = 0.001, train = "") {
  n <- nrow(dat); if (n <= 2 || !is.finite(vp$pause_thr) || vp$pause_thr <= 0) return(data.frame())
  isi <- suppressWarnings(as.numeric(dat$ISI_sec))
  art <- is_artifact_isi(isi, min_isi_sec)
  valid <- is.finite(isi) & !art
  if (length(valid) > 0) valid[1] <- FALSE
  vals <- stpd_event_core_valid_train_isis(isi, valid, min_isi_sec)
  pause_floor_base <- stpd_event_core_pause_global_floor(isi, valid, vp, min_isi_sec)
  tonic_bounds <- stpd_event_core_tonic_adaptive_bounds(isi, valid, vp, min_isi_sec)
  tonic_guard_enabled <- length(vals) >= 10L
  tonic_pause_guard <- if (tonic_guard_enabled && is.finite(tonic_bounds$upper) && tonic_bounds$upper > 0) {
    tonic_bounds$upper * 1.15
  } else NA_real_
  pause_floor <- max(c(pause_floor_base, tonic_pause_guard), na.rm = TRUE)
  if (!is.finite(pause_floor)) pause_floor <- pause_floor_base
  pp <- params$pause %||% list()
  eg <- params$event_grammar %||% list()
  local_factor <- stpd_event_core_num(eg$pause_relative_local_factor %||% pp$relative_local_factor %||% 1.55, 1.55)
  global_factor <- stpd_event_core_num(eg$pause_relative_global_factor %||% pp$relative_global_factor %||% 1.25, 1.25)
  local_factor <- max(1, local_factor)
  global_factor <- max(1, global_factor)
  global_med <- if (length(vals) > 0) stats::median(vals, na.rm = TRUE) else NA_real_
  base_long <- valid & isi >= pause_floor
  base_long[is.na(base_long)] <- FALSE
  flag <- base_long
  if (length(vals) >= 6 && any(flag, na.rm = TRUE)) {
    long_idx <- which(base_long)
    for (ii in which(flag)) {
      loc <- get_local_median(isi, ii, exclude_idx = long_idx, min_isi_sec = min_isi_sec)
      if (!is.finite(loc)) loc <- get_local_median(isi, ii, exclude_idx = ii, min_isi_sec = min_isi_sec)
      local_ok <- !is.finite(loc) || isi[ii] >= loc * local_factor
      global_ok <- !is.finite(global_med) || isi[ii] >= global_med * global_factor
      flag[ii] <- local_ok && global_ok
    }
  }
  flag[1] <- FALSE
  runs <- stpd_event_core_bool_runs(flag)
  rows <- list(); counter <- 0L
  for (rr in seq_len(nrow(runs))) {
    s <- as.integer(runs$start_isi[rr]); e <- as.integer(runs$end_isi[rr])
    # Pause is a gap layer.  Keep consecutive pause ISIs together if present.
    vals <- isi[s:e]
    loc_vals <- vapply(s:e, function(ii) get_local_median(isi, ii, exclude_idx = which(base_long), min_isi_sec = min_isi_sec), numeric(1))
    loc_med <- if (length(loc_vals) > 0) stats::median(loc_vals[is.finite(loc_vals)], na.rm = TRUE) else NA_real_
    if (!is.finite(loc_med)) loc_med <- NA_real_
    score <- if (length(vals) > 0) max(vals, na.rm = TRUE) / pause_floor else 1
    counter <- counter + 1L
    rows[[length(rows) + 1L]] <- stpd_event_core_candidate_from_run(dat, s, e, params, vp, min_isi_sec, train,
      "event_core_pause_gap", "event_core_pause_gap", "pause",
      "event_core_pause_pass", "relative_long_isi_gap_layer", "accept",
      score, 300,
      list(candidate_id = paste0("event_core_pause_", counter),
           pause_threshold_sec = vp$pause_thr,
           pause_base_threshold_sec = pause_floor_base,
           pause_effective_threshold_sec = pause_floor,
           pause_tonic_guard_threshold_sec = tonic_pause_guard,
           pause_local_median_sec = loc_med,
           pause_global_median_sec = global_med,
           pause_relative_local_factor = local_factor,
           pause_relative_global_factor = global_factor))
  }
  if (length(rows) == 0) return(data.frame())
  dplyr::bind_rows(rows)
}

stpd_event_core_candidate_value_label_base <- function(row) {
  lab <- as.character(row$final_label[1] %||% "")
  pri <- switch(lab,
    burst = 1000000,
    long_burst = 900000,
    high_frequency_spiking = 500000,
    high_frequency_tonic = 250000,
    tonic = 160000,
    pause = 120000,
    possible_burst = 20000,
    0
  )
  sc <- stpd_event_core_num(row$score[1], 0)
  n_isi <- stpd_event_core_num(row$n_isi[1], 0)
  pri + 100 * sc + n_isi
}

stpd_event_core_weighted_select <- function(cands, locked = NULL, patterns = NULL) {
  if (is.null(cands) || nrow(cands) == 0) return(cands)
  cands$selected_for_auto <- FALSE
  cands$selection_status <- "not_selected"
  lab <- as.character(cands$final_label %||% "")
  keep_label <- lab != "" & !(lab %in% c("reject", "profile"))
  keep_label[is.na(keep_label)] <- FALSE
  if (!is.null(patterns)) {
    allowed <- patterns
    # possible_burst is a review subtype of the burst family.
    if (any(c("burst", "long_burst") %in% allowed)) allowed <- unique(c(allowed, "possible_burst"))
    keep_label <- keep_label & lab %in% allowed
    keep_label[is.na(keep_label)] <- FALSE
  }
  starts <- suppressWarnings(as.integer(cands$start_isi))
  ends <- suppressWarnings(as.integer(cands$end_isi))
  valid_int <- keep_label & is.finite(starts) & is.finite(ends) & starts <= ends
  valid_int[is.na(valid_int)] <- FALSE
  if (!is.null(locked)) {
    for (i in which(valid_int)) {
      idx <- starts[i]:ends[i]
      if (any(locked[idx], na.rm = TRUE)) {
        valid_int[i] <- FALSE
        cands$selection_status[i] <- "blocked_by_manual_label"
      }
    }
  }
  sel_pool <- cands[valid_int, , drop = FALSE]
  if (nrow(sel_pool) == 0) return(cands)
  sel_pool$.__orig_i <- which(valid_int)
  sel_pool$.__value <- vapply(seq_len(nrow(sel_pool)), function(i) stpd_event_core_candidate_value(sel_pool[i, , drop = FALSE]), numeric(1))
  ord <- order(suppressWarnings(as.integer(sel_pool$end_isi)), suppressWarnings(as.integer(sel_pool$start_isi)))
  pool <- sel_pool[ord, , drop = FALSE]
  s <- suppressWarnings(as.integer(pool$start_isi)); e <- suppressWarnings(as.integer(pool$end_isi)); val <- suppressWarnings(as.numeric(pool$.__value))
  m <- nrow(pool)
  p <- integer(m)
  for (j in seq_len(m)) {
    ok <- which(e < s[j])
    p[j] <- if (length(ok) == 0) 0L else max(ok)
  }
  dp <- numeric(m + 1L); take <- logical(m)
  for (j in seq_len(m)) {
    incl <- val[j] + dp[p[j] + 1L]
    excl <- dp[j]
    if (incl > excl) { dp[j + 1L] <- incl; take[j] <- TRUE } else { dp[j + 1L] <- excl; take[j] <- FALSE }
  }
  chosen <- integer(); j <- m
  while (j >= 1L) {
    incl <- val[j] + dp[p[j] + 1L]
    if (take[j] && incl >= dp[j]) {
      chosen <- c(chosen, j)
      j <- p[j]
    } else j <- j - 1L
  }
  if (length(chosen) > 0) {
    orig <- as.integer(pool$.__orig_i[chosen])
    cands$selected_for_auto[orig] <- TRUE
    cands$selection_status[orig] <- "selected_by_event_core_weighted_interval_grammar"
  }
  cands
}

stpd_detect_train_event_core_impl <- function(dat, params, min_isi_sec = 0.001, train = "", lock_manual = TRUE) {
  params <- effective_params_for_detector(params)
  dat <- ensure_train_isi_percentiles(dat, min_isi_sec, force = TRUE)
  n <- nrow(dat)
  if (!('pattern_manual_negative' %in% names(dat))) dat$pattern_manual_negative <- rep("", n)
  if (!('auto_score' %in% names(dat))) dat$auto_score <- rep(NA_real_, n)
  if (n <= 1) { dat$pattern_auto <- ""; dat$auto_score <- NA_real_; return(dat) }
  vp <- stpd_event_core_params_impl(dat, params, min_isi_sec)
  manual_for_lock <- if (isTRUE(lock_manual) && !is.null(dat$pattern_manual)) as.character(dat$pattern_manual) else rep("", n)
  manual_for_lock[is.na(manual_for_lock)] <- ""
  locked <- manual_for_lock != ""
  patterns <- params$detector$patterns_to_run %||% stpd_default_patterns_to_run()

  profile <- stpd_event_core_train_profile_row(dat, params, vp, min_isi_sec, train)
  cand_rows <- list(profile)
  hard_thr <- stpd_event_core_detect_hard_isi_thresholds(dat, params, vp, min_isi_sec, train)
  if (nrow(hard_thr) > 0) cand_rows[[length(cand_rows) + 1L]] <- hard_thr
  if (any(c("burst", "long_burst") %in% patterns)) {
    b <- stpd_event_core_detect_burst_events(dat, params, vp, min_isi_sec, train)
    if (nrow(b) > 0) cand_rows[[length(cand_rows) + 1L]] <- b
  }
  if ("high_frequency_spiking" %in% patterns) {
    hfs <- stpd_event_core_detect_hf_spiking(dat, params, vp, min_isi_sec, train)
    if (nrow(hfs) > 0) cand_rows[[length(cand_rows) + 1L]] <- hfs
  }
  if ("high_frequency_tonic" %in% patterns) {
    hft <- stpd_event_core_detect_hf_tonic(dat, params, vp, min_isi_sec, train)
    if (nrow(hft) > 0) cand_rows[[length(cand_rows) + 1L]] <- hft
  }
  if ("tonic" %in% patterns) {
    ton <- stpd_event_core_detect_tonic(dat, params, vp, min_isi_sec, train)
    if (nrow(ton) > 0) cand_rows[[length(cand_rows) + 1L]] <- ton
  }
  if ("pause" %in% patterns) {
    pau <- stpd_event_core_detect_pause(dat, params, vp, min_isi_sec, train)
    if (nrow(pau) > 0) cand_rows[[length(cand_rows) + 1L]] <- pau
  }

  audit <- dplyr::bind_rows(cand_rows)
  audit <- stpd_event_core_weighted_select(audit, locked = locked, patterns = patterns)
  pat <- rep("", n); score <- rep(NA_real_, n)
  if (nrow(audit) > 0) {
    sel_flag <- as.logical(audit$selected_for_auto); sel_flag[is.na(sel_flag)] <- FALSE
    selected <- audit[sel_flag, , drop = FALSE]
    # Write selected events.  Full-event integrity is preserved: no residual fragments.
    if (nrow(selected) > 0) {
      selected <- selected[order(suppressWarnings(as.integer(selected$start_isi))), , drop = FALSE]
      for (i in seq_len(nrow(selected))) {
        lab <- as.character(selected$final_label[i] %||% "")
        s <- suppressWarnings(as.integer(selected$start_isi[i])); e <- suppressWarnings(as.integer(selected$end_isi[i]))
        if (!nzchar(lab) || lab %in% c("reject", "profile") || !is.finite(s) || !is.finite(e) || e < s || s < 2L || e > n) next
        idx <- s:e
        if (any(locked[idx] | pat[idx] != "", na.rm = TRUE)) next
        pat[idx] <- lab
        score[idx] <- suppressWarnings(as.numeric(selected$score[i] %||% NA_real_))
      }
    }
  }
  if ("others" %in% patterns && isTRUE(params$detector$fill_others_auto %||% FALSE)) {
    isi <- suppressWarnings(as.numeric(dat$ISI_sec)); art <- is_artifact_isi(isi, min_isi_sec)
    fill_idx <- which(seq_len(n) >= 2L & is.finite(isi) & !art & manual_for_lock == "" & pat == "")
    pat[fill_idx] <- "others"
  }
  dat$pattern_auto <- pat
  dat$auto_score <- score
  dat <- stpd_post_validate_auto_event_sizes(dat, params, min_isi_sec = min_isi_sec, lock_manual = lock_manual, train = train)
  attr(dat, "candidate_diagnostic_audit") <- audit
  attr(dat, "event_core_params") <- vp
  dat
}

stpd_detect_train_event_grammar_core <- function(dat, params, min_isi_sec = 0.001, train = "", lock_manual = TRUE) {
  params <- effective_params_for_detector(params)
  if (stpd_event_core_is_enabled(params)) {
    return(stpd_detect_train_event_core_impl(dat, params, min_isi_sec = min_isi_sec, train = train, lock_manual = lock_manual))
  }
  stpd_train_pipeline_seed_bridge_classicity(dat, params, min_isi_sec = min_isi_sec, train = train, lock_manual = lock_manual)
}
