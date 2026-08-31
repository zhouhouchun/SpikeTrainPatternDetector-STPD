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

stpd_event_core_merge_state_runs <- function(runs, isi, valid, core_upper,
                                             bridge_upper, max_bridge_n = 1L) {
  if (is.null(runs) || nrow(runs) == 0) return(runs)
  runs <- as.data.frame(runs, stringsAsFactors = FALSE)
  runs$state_bridge_isi_n <- 0L
  runs$merged_core_run_n <- 1L
  core_upper <- stpd_event_core_num(core_upper, NA_real_)
  bridge_upper <- stpd_event_core_num(bridge_upper, core_upper)
  max_bridge_n <- max(0L, stpd_event_core_int(max_bridge_n, 1L))
  if (nrow(runs) == 1L || max_bridge_n == 0L ||
      !is.finite(core_upper) || !is.finite(bridge_upper) || bridge_upper <= core_upper) {
    return(runs)
  }

  merged <- list()
  current <- runs[1, , drop = FALSE]
  for (ii in 2:nrow(runs)) {
    next_run <- runs[ii, , drop = FALSE]
    gap <- seq.int(as.integer(current$end_isi[1]) + 1L, as.integer(next_run$start_isi[1]) - 1L)
    if (length(gap) == 2L && gap[1] > gap[2]) gap <- integer()
    gap_valid <- length(gap) > 0L &&
      (as.integer(current$state_bridge_isi_n[1]) + length(gap)) <= max_bridge_n &&
      all(valid[gap] & is.finite(isi[gap]) & isi[gap] > core_upper & isi[gap] <= bridge_upper)
    if (isTRUE(gap_valid)) {
      current$end_isi <- next_run$end_isi
      current$state_bridge_isi_n <- as.integer(current$state_bridge_isi_n) + length(gap)
      current$merged_core_run_n <- as.integer(current$merged_core_run_n) + 1L
    } else {
      merged[[length(merged) + 1L]] <- current
      current <- next_run
    }
  }
  merged[[length(merged) + 1L]] <- current
  dplyr::bind_rows(merged)
}

stpd_event_core_valid_train_isis <- function(isi, valid = NULL, min_isi_sec = 0.001) {
  x <- suppressWarnings(as.numeric(isi))
  if (!is.null(valid) && length(valid) == length(x)) x <- x[as.logical(valid)]
  valid_isi_values(x, min_isi_sec)
}

stpd_event_core_pause_global_floor <- function(isi, valid = NULL, vp = list(), min_isi_sec = 0.001) {
  vals <- stpd_event_core_valid_train_isis(isi, valid, min_isi_sec)
  entry <- stpd_event_core_num(
    vp$pause_thr %||% vp$pause_entry_thr, NA_real_
  )
  strong <- stpd_event_core_num(
    vp$pause_strong_thr %||% vp$pause_thr, NA_real_
  )
  source_mode <- tolower(as.character(
    vp$pause_threshold_source_mode %||% "auto"
  )[1L])
  frozen <- source_mode %in% c("user", "manual", "default")
  # Pause is a one-sided Gap definition.  A user/manual/default lower bound is
  # already the resolved scientific threshold and must be honoured exactly;
  # the separately retained upper/strong value is an audit descriptor, not a
  # second entry gate.  This also makes train-specific UI overrides effective.
  if (frozen) return(entry)
  # Automatic mode already resolves `entry` from the pooled, label-blind q90.
  # When an independently audited extreme upper tail is stable, it is the more
  # specific automatic threshold (typical of sparse, strongly separated Pause
  # mechanisms).  When that tail is unresolved, it must not veto ordinary
  # Pause: fall back to the pooled q90 entry instead.
  if (isTRUE(vp$pause_strong_active %||% FALSE) &&
      is.finite(strong) && strong > 0) return(strong)
  if (is.finite(entry) && entry > 0) return(entry)
  strong
}

stpd_event_core_tonic_adaptive_bounds <- function(isi, valid, vp, min_isi_sec = 0.001) {
  vals <- stpd_event_core_valid_train_isis(isi, valid, min_isi_sec)
  raw_min <- stpd_event_core_num(vp$tonic_min, min_isi_sec)
  raw_max <- stpd_event_core_num(vp$tonic_max, NA_real_)
  threshold_source_mode <- tolower(as.character(
    vp$tonic_threshold_source_mode %||% "auto"
  )[1])
  if (!length(threshold_source_mode) || is.na(threshold_source_mode) ||
      !nzchar(threshold_source_mode)) {
    threshold_source_mode <- "auto"
  }
  frozen_band <- threshold_source_mode %in% c("user", "manual", "default")
  burst_ref <- stpd_event_core_num(vp$tonic_burst_overlap_ref, NA_real_)
  burst_factor <- stpd_event_core_num(vp$tonic_burst_overlap_guard_factor, 1.15)
  burst_floor <- if (is.finite(burst_ref) && burst_ref > 0) {
    burst_ref * max(1, burst_factor)
  } else {
    NA_real_
  }
  if (length(vals) < 6) {
    return(list(
      lower = raw_min, upper = raw_max,
      q10 = NA_real_, q75 = NA_real_, q90 = NA_real_,
      legacy_burst_floor_audit_sec = burst_floor,
      burst_floor_applied = FALSE,
      tonic_threshold_source_mode = threshold_source_mode,
      tonic_frozen_lower_sec = raw_min,
      tonic_frozen_upper_sec = raw_max,
      tonic_train_lower_adaptation_applied = FALSE,
      tonic_train_upper_adaptation_applied = FALSE,
      core_lower = raw_min
    ))
  }
  q10 <- stpd_event_core_quantile(vals, 0.10)
  q25 <- stpd_event_core_quantile(vals, 0.25)
  q75 <- stpd_event_core_quantile(vals, 0.75)
  q90 <- stpd_event_core_quantile(vals, 0.90)
  # A user/manual/default band is already a resolved scientific input. Do not
  # silently change either boundary with held-out whole-train quantiles: in a
  # mixed train those quantiles are dominated by other States/Events and would
  # make a calibration-frozen evaluation transductive. Automatic/histogram
  # modes may still use label-blind train evidence to propose softer bounds.
  lower_entry <- if (frozen_band) raw_min else min(c(raw_min, q10), na.rm = TRUE)
  # The former burst-derived floor made Event evidence determine tonic State
  # support before multi-track arbitration.  Keep it only as an audit value;
  # the active lower bound is label-blind and uses the tonic/train evidence.
  lower <- max(c(min_isi_sec, lower_entry), na.rm = TRUE)
  if (!is.finite(lower)) lower <- raw_min

  if (frozen_band) {
    upper <- raw_max
  } else {
    pause_floor <- stpd_event_core_pause_global_floor(isi, valid, vp, min_isi_sec)
    q90_tonic_candidate <- if (is.finite(q75) && q75 > 0 && is.finite(q90) && q90 <= q75 * 1.50) q90 * 1.02 else NA_real_
    upper_candidates <- c(raw_max, q75 * 1.25, q90_tonic_candidate, lower * 1.5)
    upper <- max(upper_candidates[is.finite(upper_candidates)], na.rm = TRUE)
    if (!is.finite(upper)) upper <- raw_max
    if (is.finite(pause_floor) && is.finite(q90) && pause_floor > q90 * 1.15 && pause_floor > lower) {
      upper <- min(upper, pause_floor * 0.98)
    }
    if (!is.finite(upper) || upper <= lower) {
      upper <- max(c(raw_max, q75, lower + min_isi_sec), na.rm = TRUE)
    }
  }
  core_lower <- if (frozen_band) {
    lower
  } else {
    max(c(lower, stpd_event_core_num(q75, lower) * 0.65), na.rm = TRUE)
  }
  if (!is.finite(core_lower) || !is.finite(upper) || core_lower > upper) {
    core_lower <- lower
  }
  list(
    lower = lower, upper = upper, q10 = q10, q75 = q75, q90 = q90,
    legacy_burst_floor_audit_sec = burst_floor,
    burst_floor_applied = FALSE,
    tonic_threshold_source_mode = threshold_source_mode,
    tonic_frozen_lower_sec = raw_min,
    tonic_frozen_upper_sec = raw_max,
    tonic_train_lower_adaptation_applied = !frozen_band &&
      is.finite(q10) && is.finite(raw_min) && lower < raw_min,
    tonic_train_upper_adaptation_applied = !frozen_band &&
      is.finite(raw_max) && is.finite(upper) && !isTRUE(all.equal(upper, raw_max)),
    core_lower = core_lower
  )
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
  tonic_mm_contract <- stpd_first_nonnull(
    tp$mm_relaxation_contract,
    (((params$spiketrainpattern %||% list())$tonic %||% list())$
       mm_relaxation_contract),
    list()
  )

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
    hf_tonic_bridge_upper = max(
      stpd_event_core_num(hp$T_high_max %||% 0.020, 0.020),
      stpd_event_core_num(hp$tonic_bridge_upper_sec %||% 0.035, 0.035)
    ),
    hf_tonic_connector_max_n = max(0L, stpd_event_core_int(hp$tonic_connector_max_n %||% 1L, 1L)),
    hf_tonic_low_tail_max = min(max(stpd_event_core_num(hp$tonic_low_tail_fraction_max %||% 0.05, 0.05), 0), 1),
    # Deprecated compatibility input.  Burst is an Event overlay and therefore
    # cannot veto an independently supported HFT State (D-021).  Keep the
    # requested value for audit/migration, but never apply it to State selection.
    hf_tonic_burst_core_veto = FALSE,
    hf_tonic_burst_core_veto_requested = isTRUE(
      hp$tonic_burst_core_veto %||% FALSE
    ),
    hf_tonic_core_veto_min_isi_n = max(1L, stpd_event_core_int(hp$tonic_burst_core_veto_min_isi_n %||% 2L, 2L)),
    hf_tonic_min_spikes = max(3L, stpd_event_core_int(hp$G_min %||% 6L, 6L)),
    hf_tonic_cv_max = stpd_event_core_num(hp$stable_CV_max %||% 0.30, 0.30),
    hf_tonic_lv_max = stpd_event_core_num(hp$stable_LV_max %||% 0.35, 0.35),
    hf_tonic_mm_max = stpd_event_core_num(hp$stable_MM_max %||% 1.25, 1.25),
    tonic_min = tonic_min,
    tonic_max = tonic_max,
    tonic_threshold_source_mode = as.character(
      (params$event_grammar %||% list())$threshold_source_mode %||% "auto"
    )[1],
    tonic_bridge_upper = max(
      tonic_max,
      stpd_event_core_num(tp$bridge_upper_sec %||% (tonic_max * 1.25), tonic_max * 1.25)
    ),
    tonic_connector_max_n = max(0L, stpd_event_core_int(tp$connector_max_n %||% 1L, 1L)),
    tonic_min_spikes = max(3L, stpd_event_core_int(tp$G_min %||% 5L, 5L)),
    # Tonic is a State, so its calibration-derived duration floor is part of
    # the acceptance evidence.  This value is learned from manual examples (or
    # explicitly supplied by the user); it is not a fixed absolute ISI cutoff.
    tonic_min_duration = max(
      0,
      stpd_event_core_num(tp$D_min %||% 0, 0)
    ),
    tonic_short_regular_route_enabled = isTRUE(
      tp$short_regular_route_enabled %||% FALSE
    ),
    tonic_short_regular_min_isi_count = max(
      0L,
      stpd_event_core_int(tp$short_regular_min_isi_count %||% 0L, 0L)
    ),
    tonic_short_regular_evidence_n = max(
      0L,
      stpd_event_core_int(tp$short_regular_evidence_n %||% 0L, 0L)
    ),
    tonic_short_regular_group_n = max(
      0L,
      stpd_event_core_int(tp$short_regular_group_n %||% 0L, 0L)
    ),
    tonic_short_regular_count_q10_full = stpd_event_core_num(
      tp$short_regular_count_q10_full, NA_real_
    ),
    tonic_short_regular_logo_q10_min = stpd_event_core_num(
      tp$short_regular_logo_q10_min, NA_real_
    ),
    tonic_short_regular_logo_q10_max = stpd_event_core_num(
      tp$short_regular_logo_q10_max, NA_real_
    ),
    tonic_short_regular_structural_floor_spikes = max(
      2L,
      stpd_event_core_int(
        tp$short_regular_structural_floor_spikes %||% 3L, 3L
      )
    ),
    tonic_short_regular_mode = as.character(
      tp$short_regular_mode %||% "not_calibrated"
    )[1],
    tonic_short_regular_status = as.character(
      tp$short_regular_status %||% "not_calibrated"
    )[1],
    tonic_lv_max = stpd_event_core_num(tp$LV_core %||% 0.5, 0.5),
    tonic_mm_max = stpd_event_core_num(tp$tonic_mm_max %||% 1.25, 1.25),
    tonic_mm_min = stpd_event_core_num(tp$tonic_mm_min %||% 0.85, 0.85),
    tonic_mm_relax_lv_max = stpd_event_core_num(
      tp$tonic_mm_relax_lv_max %||% 0.15, 0.15
    ),
    tonic_mm_relax_cv_max = stpd_event_core_num(
      tp$tonic_mm_relax_cv_max %||% 0.30, 0.30
    ),
    tonic_mm_relaxed_max = stpd_event_core_num(
      tp$tonic_mm_relaxed_max %||% 1.40, 1.40
    ),
    tonic_mm_relaxation_contract = tonic_mm_contract,
    tonic_mm_relaxation_mode = as.character(
      tonic_mm_contract$mode %||%
        "prespecified_default_v1"
    )[1],
    tonic_mm_relaxation_status = as.character(
      tonic_mm_contract$status %||%
        "default_guardrail"
    )[1],
    tonic_burst_overlap_ref = suppressWarnings(max(c(seed_high, bridge_high, mb$burst_q95), na.rm = TRUE)),
    tonic_burst_overlap_guard = isTRUE(tp$burst_overlap_guard %||% TRUE),
    tonic_burst_overlap_guard_factor = stpd_event_core_num(tp$burst_overlap_guard_factor %||% 1.15, 1.15),
    tonic_burst_overlap_lower_quantile = stpd_event_core_num(tp$burst_overlap_lower_quantile %||% 0.10, 0.10),
    tonic_burst_overlap_low_fraction_max = stpd_event_core_num(tp$burst_overlap_low_fraction_max %||% 0.05, 0.05),
    tonic_burst_overlap_reference_quantile = stpd_event_core_num(tp$burst_overlap_reference_quantile %||% 0.95, 0.95),
    pause_thr = stpd_event_core_num(pp$T_seed %||% 0.100, 0.100),
    pause_entry_thr = stpd_event_core_num(pp$T_seed %||% 0.100, 0.100),
    pause_strong_thr = stpd_event_core_num(
      pp$T_strong %||% pp$T_seed %||% 0.150, 0.150
    ),
    pause_strong_active = TRUE,
    pause_strong_status = "configured_parameter",
    pause_threshold_source_mode = "default"
  )
}

stpd_event_core_train_profile_row <- function(dat, params, vp, min_isi_sec = 0.001, train = "") {
  isi <- suppressWarnings(as.numeric(dat$ISI_sec))
  art <- is_artifact_isi(isi, min_isi_sec)
  valid <- is.finite(isi) & !art
  if (length(valid) > 0) valid[1] <- FALSE
  vals <- isi[valid]
  structure_first_settings <- stpd_event_core_structure_first_settings(params, vp)
  structure_first_context <- stpd_event_core_structure_first_train_context(
    isi, valid, structure_first_settings, min_isi_sec = min_isi_sec
  )
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
    decision_path = if (isTRUE(structure_first_settings$enabled)) {
      "structure_first_local_flank_separation_precedes_seed_band;dataset_manual_isi_band_profile_percentiles_are_outputs"
    } else {
      "structure_first_disabled;seed_band_threshold_centred_initial_screen;dataset_manual_isi_band_profile_percentiles_are_outputs"
    },
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
    initial_burst_screen = if (isTRUE(structure_first_settings$enabled)) "structure_first_local_flank_separation" else "seed_band_threshold_centred",
    structure_first_enabled = isTRUE(structure_first_settings$enabled),
    structure_first_min_isi_count = structure_first_settings$min_isi_n,
    structure_first_max_isi_count = structure_first_settings$max_isi_n,
    structure_first_contrast_min = structure_first_settings$contrast_min,
    structure_first_geom_contrast_min = structure_first_settings$geom_contrast_min,
    structure_first_min_train_valid_isi = structure_first_settings$min_train_valid_isi,
    structure_first_train_valid_isi_n = structure_first_context$valid_isi_n,
    structure_first_train_quantile_probability = structure_first_settings$compactness_quantile,
    structure_first_train_quantile_sec = structure_first_context$train_compactness_quantile_sec,
    structure_first_background_fraction = structure_first_settings$background_fraction,
    structure_first_compact_upper_sec = structure_first_context$compact_upper_sec,
    structure_first_compactness_gate_active = structure_first_context$compactness_gate_active,
    structure_first_max_internal_tail_ratio = structure_first_settings$max_internal_tail_ratio,
    structure_first_allow_endpoint = structure_first_settings$allow_endpoint,
    structure_first_endpoint_as_canonical = structure_first_settings$endpoint_as_canonical,
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

stpd_event_core_refractory_suspect_threshold <- function(params, min_isi_sec = 0.001) {
  detector <- params$detector %||% list()
  burst <- params$burst %||% list()
  product_qc <- ((params$spiketrainpattern %||% list())$qc %||% list())
  candidates <- list(
    detector$refractory_suspect_sec,
    burst$refractory_suspect_sec,
    product_qc$refractory_suspect_isi_sec,
    0.0010
  )
  for (value in candidates) {
    threshold <- suppressWarnings(as.numeric(value))[1]
    if (is.finite(threshold) && threshold > 0) return(threshold)
  }
  suppressWarnings(as.numeric(min_isi_sec))[1]
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
  refractory_suspect_threshold <- stpd_event_core_refractory_suspect_threshold(params, min_isi_sec)
  refractory_suspect <- is_refractory_suspect_isi(
    isi[idx],
    min_isi_sec = min_isi_sec,
    refractory_suspect_sec = refractory_suspect_threshold
  )
  refractory_suspect_n <- sum(refractory_suspect, na.rm = TRUE)
  refractory_suspect_fraction <- refractory_suspect_n / max(1L, length(valid_idx))
  refractory_suspect_min <- if (refractory_suspect_n > 0) {
    min(isi[idx][refractory_suspect], na.rm = TRUE)
  } else {
    NA_real_
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
    refractory_suspect_threshold_sec = refractory_suspect_threshold,
    refractory_suspect_n = as.integer(refractory_suspect_n),
    refractory_suspect_fraction = refractory_suspect_fraction,
    refractory_suspect_min_ISI_sec = refractory_suspect_min,
    manual_negative_veto = neg_overlap,
    manual_negative_overlap_fraction = neg_frac,
    stringsAsFactors = FALSE
  )
}

stpd_event_core_burst_subtype_from_spike_count <- function(n_spikes, vp, review_label = "possible_burst") {
  n_spikes <- suppressWarnings(as.integer(n_spikes))[1]
  if (!is.finite(n_spikes)) return(as.character(review_label)[1])
  classic_max <- max(3L, stpd_event_core_int(vp$classic_max_spikes %||% 10L, 10L))
  long_min <- max(classic_max + 1L, stpd_event_core_int(vp$long_min_spikes %||% 11L, 11L))
  long_max <- stpd_event_core_int(vp$long_max_spikes %||% 15L, 15L)
  if (n_spikes <= classic_max) return("burst")
  if (n_spikes >= long_min && (long_max <= 0L || n_spikes <= long_max)) return("long_burst")
  as.character(review_label)[1]
}

stpd_event_core_apply_refractory_suspect_policy <- function(cands, params, dat = NULL, vp = NULL,
                                                            min_isi_sec = NULL) {
  if (is.null(cands) || nrow(cands) == 0 || !("refractory_suspect_n" %in% names(cands))) return(cands)

  action <- as.character(
    (params$detector %||% list())$refractory_suspect_action %||%
      (params$burst %||% list())$refractory_suspect_action %||%
      "demote_to_possible"
  )[1]
  action <- tolower(trimws(action))
  action <- gsub("-", "_", action, fixed = TRUE)
  if (action %in% c("demote", "demote_burst", "demote_to_possible_burst", "review")) action <- "demote_to_possible"
  if (action %in% c("split", "split_candidate", "split_at_refractory")) action <- "split_at_suspect"
  if (action %in% c("exclude_suspect", "exclude_suspect_isi", "reevaluate_fragments")) action <- "exclude_suspect_isi_and_reevaluate"
  if (action %in% c("exclude", "reject", "drop", "reject_candidate")) action <- "exclude_candidate"
  if (action %in% c("multiunit", "mark_multiunit")) action <- "mark_multiunit_contamination"
  valid_actions <- c(
    "warn_only", "demote_to_possible", "split_at_suspect",
    "exclude_suspect_isi_and_reevaluate", "exclude_candidate",
    "mark_multiunit_contamination"
  )
  if (!(action %in% valid_actions)) action <- "warn_only"

  suspect_n <- suppressWarnings(as.numeric(cands$refractory_suspect_n))
  if (!("refractory_suspect_policy_applied" %in% names(cands))) {
    cands$refractory_suspect_policy_applied <- FALSE
  }
  already_applied <- as.logical(cands$refractory_suspect_policy_applied)
  already_applied[is.na(already_applied)] <- FALSE
  suspect <- is.finite(suspect_n) & suspect_n > 0 & !already_applied
  if (!any(suspect)) return(cands)

  if (!("refractory_suspect_action" %in% names(cands))) cands$refractory_suspect_action <- ""
  if (!("refractory_suspect_warning" %in% names(cands))) cands$refractory_suspect_warning <- FALSE
  if (!("uncertainty_reason" %in% names(cands))) cands$uncertainty_reason <- ""
  cands$refractory_suspect_action[suspect] <- action
  cands$refractory_suspect_warning[suspect] <- TRUE

  append_note <- function(x, note) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    already_present <- grepl(note, x, fixed = TRUE)
    ifelse(already_present, x, ifelse(nzchar(x), paste0(x, ";", note), note))
  }
  cands$uncertainty_reason[suspect] <- append_note(
    cands$uncertainty_reason[suspect],
    "contains_refractory_suspect_ISI"
  )

  if (identical(action, "warn_only")) {
    cands$refractory_suspect_action[suspect] <- "warn_only"
    cands$refractory_suspect_policy_applied[suspect] <- TRUE
    return(cands)
  }

  labels <- as.character(cands$final_label %||% cands$class %||% "")
  labels[is.na(labels)] <- ""
  if (!("label_before_refractory_suspect_policy" %in% names(cands))) cands$label_before_refractory_suspect_policy <- ""
  if (!("gate_status_before_refractory_suspect_policy" %in% names(cands))) {
    cands$gate_status_before_refractory_suspect_policy <- ""
  }
  if (!("reject_reason" %in% names(cands))) cands$reject_reason <- ""

  if (action %in% c("demote_to_possible", "mark_multiunit_contamination")) {
    demote <- suspect & labels %in% c("burst", "long_burst")
    cands$label_before_refractory_suspect_policy[demote] <- labels[demote]
    if ("final_label" %in% names(cands)) cands$final_label[demote] <- "possible_burst"
    if ("class" %in% names(cands)) cands$class[demote] <- "possible_burst"
    if ("candidate_diagnostic_class" %in% names(cands)) {
      cands$candidate_diagnostic_class[demote] <- if (identical(action, "mark_multiunit_contamination")) {
        "possible_burst__possible_multiunit_contamination"
      } else {
        "possible_burst__refractory_suspect_review"
      }
    }
    if ("gate_status" %in% names(cands)) {
      cands$gate_status_before_refractory_suspect_policy[demote] <- as.character(cands$gate_status[demote])
      cands$gate_status[demote] <- if (identical(action, "mark_multiunit_contamination")) {
        "refractory_suspect_marked_possible_multiunit_contamination"
      } else {
        "refractory_suspect_demoted_to_possible_burst"
      }
    }
    if ("decision_path" %in% names(cands)) {
      note <- if (identical(action, "mark_multiunit_contamination")) {
        "contains_refractory_suspect_ISI;mark_possible_multiunit_contamination"
      } else {
        "contains_refractory_suspect_ISI;demote_to_possible_burst"
      }
      cands$decision_path[demote] <- append_note(cands$decision_path[demote], note)
    }
    if ("action" %in% names(cands)) cands$action[demote] <- "demote_to_possible"
    cands$reject_reason[suspect] <- append_note(
      cands$reject_reason[suspect],
      if (identical(action, "mark_multiunit_contamination")) "possible_multiunit_contamination" else "refractory_suspect_review"
    )
    cands$refractory_suspect_action[suspect] <- ifelse(
      demote[suspect],
      if (identical(action, "mark_multiunit_contamination")) "marked_possible_multiunit_contamination" else "demoted_to_possible_burst",
      "already_possible_or_nonburst"
    )
    cands$refractory_suspect_policy_applied[suspect] <- TRUE
    return(cands)
  }

  if (identical(action, "exclude_candidate")) {
    cands$label_before_refractory_suspect_policy[suspect] <- labels[suspect]
    if ("final_label" %in% names(cands)) cands$final_label[suspect] <- "reject"
    if ("class" %in% names(cands)) cands$class[suspect] <- "reject"
    if ("candidate_diagnostic_class" %in% names(cands)) {
      cands$candidate_diagnostic_class[suspect] <- "reject__refractory_suspect_candidate"
    }
    if ("gate_status" %in% names(cands)) {
      cands$gate_status_before_refractory_suspect_policy[suspect] <- as.character(cands$gate_status[suspect])
      cands$gate_status[suspect] <- "refractory_suspect_candidate_rejected"
    }
    if ("decision_path" %in% names(cands)) {
      cands$decision_path[suspect] <- append_note(
        cands$decision_path[suspect], "contains_refractory_suspect_ISI;exclude_entire_candidate"
      )
    }
    if ("priority" %in% names(cands)) cands$priority[suspect] <- 0
    if ("selected_for_auto" %in% names(cands)) cands$selected_for_auto[suspect] <- FALSE
    if ("selection_status" %in% names(cands)) cands$selection_status[suspect] <- "rejected_refractory_suspect"
    if ("action" %in% names(cands)) cands$action[suspect] <- "reject"
    cands$reject_reason[suspect] <- append_note(cands$reject_reason[suspect], "refractory_suspect_candidate")
    cands$refractory_suspect_action[suspect] <- "excluded_entire_candidate"
    cands$refractory_suspect_policy_applied[suspect] <- TRUE
    return(cands)
  }

  # Split/exclude-suspect policies need the underlying train to reconstruct
  # biologically valid contiguous fragments.  If a low-level caller omitted the
  # train, fail conservatively into review rather than claiming a split occurred.
  if (is.null(dat) || !is.data.frame(dat)) {
    fallback <- cands
    fallback$refractory_suspect_action[suspect] <- paste0(action, "_unavailable_demoted_to_possible")
    fallback$uncertainty_reason[suspect] <- append_note(
      fallback$uncertainty_reason[suspect], "refractory_split_context_unavailable"
    )
    demote <- suspect & labels %in% c("burst", "long_burst")
    fallback$label_before_refractory_suspect_policy[demote] <- labels[demote]
    if ("final_label" %in% names(fallback)) fallback$final_label[demote] <- "possible_burst"
    if ("class" %in% names(fallback)) fallback$class[demote] <- "possible_burst"
    if ("action" %in% names(fallback)) fallback$action[demote] <- "demote_to_possible"
    fallback$refractory_suspect_policy_applied[suspect] <- TRUE
    return(fallback)
  }

  if (is.null(min_isi_sec) || !is.finite(suppressWarnings(as.numeric(min_isi_sec))[1])) {
    min_isi_sec <- stpd_event_core_num((params$detector %||% list())$min_valid_isi_sec %||% 0.0009, 0.0009)
  }
  if (is.null(vp)) vp <- stpd_event_core_params_impl(dat, params, min_isi_sec = min_isi_sec)
  threshold <- stpd_event_core_refractory_suspect_threshold(params, min_isi_sec)
  isi <- suppressWarnings(as.numeric(dat$ISI_sec))
  min_spikes <- max(3L, stpd_event_core_int(vp$min_spikes %||% 3L, 3L))
  rows <- list()
  for (ii in seq_len(nrow(cands))) {
    row <- cands[ii, , drop = FALSE]
    if (!isTRUE(suspect[ii])) {
      rows[[length(rows) + 1L]] <- row
      next
    }
    original_label <- labels[ii]
    parent <- row
    parent$label_before_refractory_suspect_policy <- original_label
    if ("final_label" %in% names(parent)) parent$final_label <- "reject"
    if ("class" %in% names(parent)) parent$class <- "reject"
    if ("candidate_diagnostic_class" %in% names(parent)) parent$candidate_diagnostic_class <- "reject__refractory_split_parent"
    if ("gate_status" %in% names(parent)) {
      parent$gate_status_before_refractory_suspect_policy <- as.character(parent$gate_status)
      parent$gate_status <- "refractory_suspect_split_parent_rejected"
    }
    if ("decision_path" %in% names(parent)) {
      parent$decision_path <- append_note(parent$decision_path, paste0("contains_refractory_suspect_ISI;", action))
    }
    if ("priority" %in% names(parent)) parent$priority <- 0
    if ("selected_for_auto" %in% names(parent)) parent$selected_for_auto <- FALSE
    if ("selection_status" %in% names(parent)) parent$selection_status <- "rejected_after_refractory_split"
    if ("action" %in% names(parent)) parent$action <- "reject"
    parent$reject_reason <- append_note(parent$reject_reason, "refractory_suspect_split_parent")
    parent$refractory_suspect_action <- paste0(action, "_parent_rejected")
    parent$refractory_suspect_policy_applied <- TRUE
    rows[[length(rows) + 1L]] <- parent

    s <- suppressWarnings(as.integer(row$start_isi[1]))
    e <- suppressWarnings(as.integer(row$end_isi[1]))
    if (!is.finite(s) || !is.finite(e) || s > e || s < 2L || e > nrow(dat)) next
    idx <- s:e
    split_mask <- is_refractory_suspect_isi(
      isi[idx], min_isi_sec = min_isi_sec,
      refractory_suspect_sec = threshold
    )
    keep_idx <- idx[!split_mask]
    if (length(keep_idx) == 0) next
    groups <- split(keep_idx, cumsum(c(TRUE, diff(keep_idx) != 1L)))
    fragment_counter <- 0L
    for (group in groups) {
      if (length(group) + 1L < min_spikes) next
      ns <- min(group); ne <- max(group)
      metrics <- stpd_event_core_span_metrics(
        dat, ns, ne, params, vp, min_isi_sec = min_isi_sec,
        train = as.character(row$train[1] %||% ""),
        label = "refractory_split_fragment"
      )
      if (is.null(metrics) || nrow(metrics) == 0) next
      fragment_counter <- fragment_counter + 1L
      fragment <- row
      for (nm in intersect(names(metrics), names(fragment))) fragment[[nm]] <- metrics[[nm]][1]
      fragment$start_isi <- ns
      fragment$end_isi <- ne
      if ("candidate_id" %in% names(fragment)) {
        fragment$candidate_id <- paste0(as.character(row$candidate_id[1] %||% paste0("candidate_", ii)), "__ref_fragment_", fragment_counter)
      }
      if ("final_label" %in% names(fragment)) fragment$final_label <- "possible_burst"
      if ("class" %in% names(fragment)) fragment$class <- "possible_burst"
      if ("candidate_diagnostic_class" %in% names(fragment)) fragment$candidate_diagnostic_class <- "possible_burst__refractory_split_fragment"
      if ("gate_status" %in% names(fragment)) {
        fragment$gate_status_before_refractory_suspect_policy <- as.character(row$gate_status[1] %||% "")
        fragment$gate_status <- "refractory_suspect_split_fragment_review"
      }
      if ("decision_path" %in% names(fragment)) {
        fragment$decision_path <- append_note(fragment$decision_path, paste0(action, ";fragment_recomputed_after_excluding_suspect_ISI"))
      }
      if ("score" %in% names(fragment)) fragment$score <- stpd_event_core_num(fragment$score, 0) * 0.95
      if ("priority" %in% names(fragment)) fragment$priority <- 120
      if ("selected_for_auto" %in% names(fragment)) fragment$selected_for_auto <- FALSE
      if ("selection_status" %in% names(fragment)) fragment$selection_status <- "not_selected"
      if ("action" %in% names(fragment)) fragment$action <- "demote_to_possible"
      fragment$label_before_refractory_suspect_policy <- original_label
      fragment$refractory_suspect_warning <- TRUE
      fragment$uncertainty_reason <- append_note(fragment$uncertainty_reason, "refractory_suspect_split_fragment")
      fragment$reject_reason <- append_note(fragment$reject_reason, "requires_review_after_refractory_split")
      fragment$refractory_suspect_action <- paste0(action, "_fragment_recomputed")
      fragment$refractory_suspect_policy_applied <- TRUE
      rows[[length(rows) + 1L]] <- fragment
    }
  }
  dplyr::bind_rows(rows)
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

# Resolve the structure-first burst screen independently of the dataset-level
# seed/bridge bands.  This mirrors the first stage of the macOS detector: local
# flank separation proposes compact burst anchors before absolute ISI bands are
# allowed to generate fallback candidates.
stpd_event_core_structure_first_settings <- function(params, vp = list()) {
  product_burst <- ((params$spiketrainpattern %||% list())$burst %||% list())
  list(
    enabled = isTRUE(product_burst$structure_first_enabled %||% TRUE),
    min_isi_n = max(
      # A contrast-only anchor must contain at least four boundary spikes
      # (three contiguous ISIs).  Shorter doublet/triplet fluctuations remain
      # available to other evidence paths but are not canonical
      # structure-first Burst candidates.
      3L,
      stpd_event_core_int(product_burst$structure_first_min_isi_count %||% 3L, 3L),
      stpd_event_core_int(vp$min_spikes %||% 3L, 3L) - 1L
    ),
    max_isi_n = max(
      1L,
      min(
        stpd_event_core_int(product_burst$structure_first_max_isi_count %||% 8L, 8L),
        stpd_event_core_int(vp$classic_max_spikes %||% 10L, 10L) - 1L
      )
    ),
    contrast_min = max(
      1,
      stpd_event_core_num(product_burst$structure_first_contrast_min %||% 3.0, 3.0)
    ),
    geom_contrast_min = max(
      1,
      stpd_event_core_num(product_burst$structure_first_geom_contrast_min %||% 3.0, 3.0)
    ),
    compactness_quantile = min(
      1,
      max(0.5, stpd_event_core_num(product_burst$structure_first_compactness_quantile %||% 0.80, 0.80))
    ),
    background_fraction = min(
      1,
      max(0.01, stpd_event_core_num(product_burst$structure_first_background_fraction %||% 0.35, 0.35))
    ),
    min_train_valid_isi = max(
      1L,
      stpd_event_core_int(product_burst$structure_first_min_train_valid_isi %||% 8L, 8L)
    ),
    max_internal_tail_ratio = max(
      1,
      stpd_event_core_num(product_burst$structure_first_max_internal_tail_ratio %||% 1.25, 1.25)
    ),
    allow_endpoint = isTRUE(product_burst$structure_first_allow_endpoint %||% TRUE),
    endpoint_as_canonical = isTRUE(product_burst$allow_one_sided_as_canonical %||% FALSE)
  )
}

stpd_event_core_structure_first_train_context <- function(isi, valid, settings,
                                                           min_isi_sec = 0.001) {
  vals <- suppressWarnings(as.numeric(isi))[as.logical(valid)]
  # `valid` already applies the tolerance-aware artifact policy. Re-filtering at
  # an exact numeric floor would make near-boundary ISIs count in a candidate but
  # disappear from its train reference distribution.
  vals <- vals[is.finite(vals)]
  train_q <- if (length(vals) >= settings$min_train_valid_isi) {
    stpd_event_core_quantile(vals, settings$compactness_quantile)
  } else {
    NA_real_
  }
  # Match the Mac structure-first screen: when the train Q80 is at the
  # artifact floor there is no usable background-scale estimate, so the
  # adaptive compactness gate is unavailable rather than fixed to the floor.
  compact_upper <- if (is.finite(train_q) && train_q > min_isi_sec) {
    max(min_isi_sec, settings$background_fraction * train_q)
  } else {
    NA_real_
  }
  list(
    valid_isi_n = length(vals),
    train_compactness_quantile_sec = train_q,
    compact_upper_sec = compact_upper,
    compactness_gate_active = is.finite(compact_upper) && compact_upper > 0
  )
}

# Apply one train-level budget to the union of the structure-first and
# threshold-centred burst stages.  Stage-local limits prevent either generator
# from growing without bound, but they do not by themselves enforce the public
# max_candidates_per_train contract after the two stages are combined.
# Strict, accepted structure-first anchors are retained first; every remaining
# tie is resolved deterministically before the selected rows are restored to
# their original stage order.
stpd_event_core_cap_burst_candidate_union <- function(structure_first = NULL,
                                                       threshold_centred = NULL,
                                                       vp = list(),
                                                       candidate_lineage_collector = NULL,
                                                       train = "") {
  out <- dplyr::bind_rows(structure_first, threshold_centred)
  if (nrow(out) == 0L) {
    if (!is.null(candidate_lineage_collector)) {
      stpd_candidate_lineage_capture_burst_union(
        candidate_lineage_collector, structure_first, threshold_centred,
        out, train, max(
          1L,
          stpd_event_core_int(
            (vp %||% list())$max_candidates %||% 3000L, 3000L
          )
        )
      )
    }
    return(out)
  }

  max_candidates <- max(
    1L,
    stpd_event_core_int((vp %||% list())$max_candidates %||% 3000L, 3000L)
  )
  if (nrow(out) <= max_candidates) {
    if (!is.null(candidate_lineage_collector)) {
      stpd_candidate_lineage_capture_burst_union(
        candidate_lineage_collector, structure_first, threshold_centred,
        out, train, max_candidates
      )
    }
    return(out)
  }

  layer <- as.character(out$candidate_layer %||% rep("", nrow(out)))
  layer[is.na(layer)] <- ""
  label <- as.character(out$final_label %||% rep("", nrow(out)))
  label[is.na(label)] <- ""
  strict_boundary <- as.logical(out$strict_boundary_pass %||% rep(FALSE, nrow(out)))
  strict_boundary[is.na(strict_boundary)] <- FALSE
  strict_structure_anchor <- layer == "structure_first_burst_screen" &
    strict_boundary & label %in% c("burst", "long_burst", "possible_burst")

  priority <- suppressWarnings(as.numeric(out$priority %||% rep(NA_real_, nrow(out))))
  priority[!is.finite(priority)] <- -Inf
  score <- suppressWarnings(as.numeric(out$score %||% rep(NA_real_, nrow(out))))
  score[!is.finite(score)] <- -Inf
  start_isi <- suppressWarnings(as.integer(out$start_isi %||% rep(NA_integer_, nrow(out))))
  start_isi[!is.finite(start_isi)] <- .Machine$integer.max
  end_isi <- suppressWarnings(as.integer(out$end_isi %||% rep(NA_integer_, nrow(out))))
  end_isi[!is.finite(end_isi)] <- .Machine$integer.max
  candidate_id <- as.character(out$candidate_id %||% rep("", nrow(out)))
  candidate_id[is.na(candidate_id)] <- ""

  ranked <- order(
    -as.integer(strict_structure_anchor),
    -priority,
    -score,
    start_isi,
    end_isi,
    layer,
    candidate_id,
    seq_len(nrow(out)),
    method = "radix",
    na.last = TRUE
  )
  keep <- sort(head(ranked, max_candidates))
  out <- out[keep, , drop = FALSE]
  rownames(out) <- NULL
  if (!is.null(candidate_lineage_collector)) {
    stpd_candidate_lineage_capture_burst_union(
      candidate_lineage_collector, structure_first, threshold_centred,
      out, train, max_candidates
    )
  }
  out
}

stpd_event_core_structure_first_burst_candidates <- function(dat, params, vp,
                                                              min_isi_sec = 0.001,
                                                              train = "",
                                                              candidate_lineage_collector = NULL) {
  n <- nrow(dat)
  max_candidates <- max(
    1L, stpd_event_core_int(vp$max_candidates %||% 3000L, 3000L)
  )
  if (n <= 2L) {
    if (!is.null(candidate_lineage_collector)) {
      stpd_candidate_lineage_capture_structure_first(
        candidate_lineage_collector, dat, train, min_isi_sec,
        applicability_status = "not_applicable_short_train",
        scan_exhausted = FALSE, max_candidates = max_candidates
      )
    }
    return(data.frame())
  }
  settings <- stpd_event_core_structure_first_settings(params, vp)
  if (!isTRUE(settings$enabled) || settings$max_isi_n < settings$min_isi_n) {
    if (!is.null(candidate_lineage_collector)) {
      status <- if (!isTRUE(settings$enabled)) {
        "not_applied_disabled"
      } else {
        "not_applied_invalid_settings"
      }
      stpd_candidate_lineage_capture_structure_first(
        candidate_lineage_collector, dat, train, min_isi_sec,
        settings = settings, applicability_status = status,
        scan_exhausted = FALSE, max_candidates = max_candidates
      )
    }
    return(data.frame())
  }

  isi <- suppressWarnings(as.numeric(dat$ISI_sec))
  art <- is_artifact_isi(isi, min_isi_sec)
  valid <- is.finite(isi) & !art
  if (length(valid) > 0L) valid[1] <- FALSE
  context <- stpd_event_core_structure_first_train_context(
    isi, valid, settings, min_isi_sec = min_isi_sec
  )

  rows <- list()
  counter <- 0L
  expected_window_n <- as.integer(sum(vapply(
    seq.int(settings$min_isi_n, settings$max_isi_n),
    function(w) max(0L, n - as.integer(w)), integer(1)
  )))
  visited_window_n <- 0L
  valid_window_n <- 0L
  for (w in seq.int(settings$min_isi_n, settings$max_isi_n)) {
    max_start <- n - w + 1L
    if (max_start < 2L) next
    for (s in seq.int(2L, max_start)) {
      visited_window_n <- visited_window_n + 1L
      e <- s + w - 1L
      idx <- s:e
      if (!all(valid[idx])) next
      valid_window_n <- valid_window_n + 1L

      m <- stpd_event_core_span_metrics(
        dat, s, e, params, vp, min_isi_sec, train,
        "structure_first_classic_burst_anchor"
      )
      if (is.null(m)) next
      q90 <- stpd_event_core_num(m$intra_q90_sec[1], NA_real_)
      max_intra <- stpd_event_core_num(m$max_intra_ISI_sec[1], NA_real_)
      if (!is.finite(q90) || q90 <= 0 || !is.finite(max_intra) || max_intra <= 0) next

      pre_valid <- s > 2L && isTRUE(valid[s - 1L])
      post_valid <- e < n && isTRUE(valid[e + 1L])
      pre_gap <- if (pre_valid) isi[s - 1L] else NA_real_
      post_gap <- if (post_valid) isi[e + 1L] else NA_real_
      pre_ratio <- if (is.finite(pre_gap)) pre_gap / q90 else NA_real_
      post_ratio <- if (is.finite(post_gap)) post_gap / q90 else NA_real_
      edge_ratios <- c(pre_ratio, post_ratio)
      edge_ratios <- edge_ratios[is.finite(edge_ratios)]
      contrast_min_observed <- if (length(edge_ratios) > 0L) min(edge_ratios) else NA_real_
      contrast_geom <- if (is.finite(pre_ratio) && is.finite(post_ratio)) {
        sqrt(max(0, pre_ratio) * max(0, post_ratio))
      } else {
        NA_real_
      }

      two_sided <- is.finite(pre_ratio) && is.finite(post_ratio) &&
        pre_ratio >= settings$contrast_min &&
        post_ratio >= settings$contrast_min &&
        is.finite(contrast_geom) && contrast_geom >= settings$geom_contrast_min
      at_start <- s == 2L
      at_end <- e == n
      endpoint <- isTRUE(settings$allow_endpoint) && xor(at_start, at_end) &&
        length(edge_ratios) == 1L && edge_ratios[1] >= settings$contrast_min
      boundary_pass <- two_sided || endpoint
      if (!boundary_pass) next

      compact_gate_active <- isTRUE(context$compactness_gate_active)
      compact_tol <- if (compact_gate_active) {
        max(1e-12, abs(context$compact_upper_sec) * 1e-6)
      } else {
        0
      }
      q90_compact_pass <- !compact_gate_active ||
        q90 <= context$compact_upper_sec + compact_tol
      strict_max_pass <- !compact_gate_active ||
        max_intra <= context$compact_upper_sec + compact_tol
      tail_ratio <- if (compact_gate_active) max_intra / context$compact_upper_sec else NA_real_
      pre_ratio_max <- if (is.finite(pre_gap)) pre_gap / max_intra else NA_real_
      post_ratio_max <- if (is.finite(post_gap)) post_gap / max_intra else NA_real_
      tolerated_tail_pass <- compact_gate_active && !strict_max_pass && two_sided &&
        q90_compact_pass && is.finite(tail_ratio) &&
        tail_ratio <= settings$max_internal_tail_ratio &&
        is.finite(pre_ratio_max) && pre_ratio_max >= settings$contrast_min &&
        is.finite(post_ratio_max) && post_ratio_max >= settings$contrast_min
      compactness_pass <- q90_compact_pass && (strict_max_pass || tolerated_tail_pass)
      if (!compactness_pass) next

      manual_negative <- isTRUE(m$manual_negative_veto[1])
      final <- if (manual_negative) {
        "reject"
      } else if (two_sided || isTRUE(settings$endpoint_as_canonical)) {
        "burst"
      } else {
        "possible_burst"
      }
      status <- if (manual_negative) {
        "structure_first_manual_negative_veto"
      } else if (two_sided) {
        "structure_first_two_sided_pass"
      } else if (isTRUE(settings$endpoint_as_canonical)) {
        "structure_first_endpoint_pass_user_allowed_canonical"
      } else {
        "structure_first_endpoint_possible_review"
      }
      action <- if (manual_negative) "reject" else if (identical(final, "possible_burst")) "demote_to_possible" else "accept"
      decision_parts <- c(
        "structure_first_classic_burst",
        "no_default_burst_seed_band_used=true",
        "source=local_flank_contrast",
        if (two_sided) "boundary=two_sided" else if (at_start) "boundary=train_start" else "boundary=train_end",
        if (tolerated_tail_pass) "compactness=tolerated_internal_tail" else if (compact_gate_active) "compactness=train_adaptive_pass" else "compactness=short_train_gate_unavailable"
      )
      if (manual_negative) decision_parts <- c(decision_parts, "manual_negative_veto=true")
      decision <- paste(decision_parts, collapse = ";")
      score <- log1p(stpd_event_core_num(m$n_valid_isi[1], w)) +
        log(max(stpd_event_core_num(contrast_min_observed, 1), 1))
      priority <- if (manual_negative) 0 else if (two_sided) 1260 else if (identical(final, "burst")) 1030 else 480
      counter <- counter + 1L
      rows[[length(rows) + 1L]] <- stpd_event_core_candidate_row(
        m,
        "structure_first_burst_screen",
        if (two_sided) "structure_first_two_sided_classic_burst" else "structure_first_endpoint_classic_burst",
        final,
        status,
        decision,
        action,
        score,
        priority,
        list(
          candidate_id = paste0("structure_first_burst_", counter),
          initial_burst_screen = "structure_first_local_flank_separation",
          structure_first_screen = TRUE,
          structure_first_no_seed_band_gate = TRUE,
          structure_first_contrast_min = settings$contrast_min,
          structure_first_geom_contrast_min = settings$geom_contrast_min,
          structure_first_min_train_valid_isi = settings$min_train_valid_isi,
          structure_first_anchor_band_lower_sec = min_isi_sec,
          structure_first_anchor_band_upper_sec = q90,
          structure_first_anchor_band_source = "structure",
          pre_ratio_q90 = pre_ratio,
          post_ratio_q90 = post_ratio,
          edge_contrast_min_q90 = contrast_min_observed,
          edge_contrast_geom_q90 = contrast_geom,
          pre_ratio_max_intra = pre_ratio_max,
          post_ratio_max_intra = post_ratio_max,
          boundary_type = if (two_sided) "two_sided" else if (at_start) "train_start_endpoint" else "train_end_endpoint",
          strict_boundary_pass = two_sided,
          one_sided_boundary_pass = endpoint,
          possible_boundary_pass = boundary_pass,
          structure_first_train_valid_isi_n = context$valid_isi_n,
          structure_first_train_quantile_probability = settings$compactness_quantile,
          structure_first_train_quantile_sec = context$train_compactness_quantile_sec,
          structure_first_background_fraction = settings$background_fraction,
          structure_first_compact_upper_sec = context$compact_upper_sec,
          structure_first_compactness_gate_active = compact_gate_active,
          structure_first_q90_compact_pass = q90_compact_pass,
          structure_first_max_intra_strict_pass = strict_max_pass,
          structure_first_internal_tail_ratio = tail_ratio,
          structure_first_max_internal_tail_ratio = settings$max_internal_tail_ratio,
          structure_first_tolerated_tail_pass = tolerated_tail_pass,
          structure_first_endpoint_as_canonical = settings$endpoint_as_canonical,
          bridge_count_pass = NA,
          bridge_fraction_pass = NA,
          q90_bridge_pass = NA,
          q95_bridge_pass = NA,
          seed_purity = NA_real_,
          failure_reason = if (manual_negative) "manual_negative_veto" else "",
          candidate_diagnostic_class = if (manual_negative) {
            "rejected__structure_first_manual_negative_veto"
          } else {
            paste0(final, "__", status)
          },
          threshold_source_summary = "structure_first:local_flank_contrast;seed_band_not_used"
        )
      )
    }
  }

  if (length(rows) == 0L) {
    if (!is.null(candidate_lineage_collector)) {
      stpd_candidate_lineage_capture_structure_first(
        candidate_lineage_collector, dat, train, min_isi_sec,
        settings = settings, context = context,
        applicability_status = "scan_exhausted_zero",
        expected_window_n = expected_window_n,
        visited_window_n = visited_window_n,
        valid_window_n = valid_window_n, scan_exhausted = TRUE,
        max_candidates = max_candidates
      )
    }
    return(data.frame())
  }
  pre_cap_out <- dplyr::bind_rows(rows)
  out <- pre_cap_out
  if (nrow(out) > max_candidates) {
    ord <- order(-suppressWarnings(as.numeric(out$priority)),
                 -suppressWarnings(as.numeric(out$score)),
                 suppressWarnings(as.integer(out$start_isi)),
                 suppressWarnings(as.integer(out$end_isi)),
                 method = "radix", na.last = TRUE)
    out <- out[head(ord, max_candidates), , drop = FALSE]
  }
  scientific_out <- out[
    order(out$start_isi, out$end_isi, -out$priority, -out$score),
    , drop = FALSE
  ]
  if (!is.null(candidate_lineage_collector)) {
    stpd_candidate_lineage_capture_structure_first(
      candidate_lineage_collector, dat, train, min_isi_sec,
      settings = settings, context = context,
      pre_cap_candidates = pre_cap_out,
      post_cap_candidates = scientific_out,
      applicability_status = "scan_exhausted_with_proposals",
      expected_window_n = expected_window_n,
      visited_window_n = visited_window_n,
      valid_window_n = valid_window_n, scan_exhausted = TRUE,
      max_candidates = max_candidates
    )
  }
  scientific_out
}

stpd_event_core_detect_burst_events <- function(dat, params, vp, min_isi_sec = 0.001, train = "") {
  n <- nrow(dat); threshold_rows <- list()
  if (n <= 2) return(data.frame())
  isi <- suppressWarnings(as.numeric(dat$ISI_sec))
  art <- is_artifact_isi(isi, min_isi_sec)
  valid <- is.finite(isi) & !art
  valid[1] <- FALSE
  seed_flag <- valid & isi >= vp$seed_low & isi <= vp$seed_high
  runs <- stpd_event_core_bool_runs(seed_flag)
  structure_first <- stpd_event_core_structure_first_burst_candidates(
    dat, params, vp, min_isi_sec = min_isi_sec, train = train
  )
  seen <- list(); counter <- 0L
  for (rr in seq_len(nrow(runs))) {
    ss <- as.integer(runs$start_isi[rr]); ee <- as.integer(runs$end_isi[rr])
    if (sum(seed_flag[ss:ee], na.rm = TRUE) < vp$min_seed_isi_n) next
    lefts <- stpd_event_core_left_extensions(ss, isi, valid, vp$bridge_high, vp$max_expand)
    rights <- stpd_event_core_right_extensions(ee, n, isi, valid, vp$bridge_high, vp$max_expand)
    for (s in lefts) {
      for (e in rights) {
        if (length(threshold_rows) >= vp$max_candidates) break
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
        threshold_rows[[length(threshold_rows) + 1L]] <- stpd_event_core_candidate_row(
          m, "event_core_burst_event", "event_core_seed_centered_burst", final, status, decision, action, score, priority,
          list(candidate_id = paste0("event_core_burst_", counter), seed_run_start_isi = ss, seed_run_end_isi = ee,
               seed_band_lower_sec = vp$seed_low, seed_band_upper_sec = vp$seed_high,
               bridge_band_upper_sec = vp$bridge_high, burst_contrast_required = vp$S,
               cross_train_borrowing_applied = isTRUE(vp$cross_train_borrowing_applied %||% FALSE),
               cross_train_borrowing_status = as.character(vp$cross_train_borrowing_status %||% "not_configured"),
               cross_train_borrowing_seed_unchanged = isTRUE(vp$cross_train_borrowing_seed_unchanged %||% TRUE),
               cross_train_borrowing_bridge_original_sec = suppressWarnings(as.numeric(vp$cross_train_borrowing_bridge_original_sec %||% vp$bridge_high)),
               cross_train_borrowing_bridge_effective_sec = suppressWarnings(as.numeric(vp$cross_train_borrowing_bridge_effective_sec %||% vp$bridge_high)),
               cross_train_borrowing_contract_sha256 = as.character(vp$cross_train_borrowing_contract_sha256 %||% ""),
               possible_contrast_required = vp$S_possible,
               required_gap_sec = req_strict, possible_required_gap_sec = req_possible,
               boundary_floor_sec = vp$boundary_floor, boundary_floor_hard = vp$boundary_floor_hard,
               strict_boundary_pass = strict_boundary, possible_boundary_pass = possible_boundary || one_edge_possible,
               bridge_count_pass = bridge_count_pass, bridge_fraction_pass = bridge_fraction_pass,
               q90_bridge_pass = q90_pass, size_label_before_review = size_label)
        )
      }
      if (length(threshold_rows) >= vp$max_candidates) break
    }
    if (length(threshold_rows) >= vp$max_candidates) break
  }
  threshold_centred <- if (length(threshold_rows) == 0L) data.frame() else dplyr::bind_rows(threshold_rows)
  candidates <- stpd_event_core_cap_burst_candidate_union(
    structure_first, threshold_centred, vp
  )
  if (nrow(candidates) == 0L) return(data.frame())
  stpd_event_core_apply_refractory_suspect_policy(
    candidates, params, dat = dat, vp = vp, min_isi_sec = min_isi_sec
  )
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
  add_row <- function(s, e, layer, cls, label, status, decision, score, priority, extra,
                      action = "accept") {
    counter <<- counter + 1L
    extra$candidate_id <- paste0("hard_isi_threshold_", counter)
    row <- stpd_event_core_candidate_from_run(
      dat, s, e, params, vp, min_isi_sec, train,
      layer, cls, label, status, decision, action,
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
        size_label <- stpd_event_core_burst_subtype_from_spike_count(
          n_spikes, vp, review_label = "possible_burst"
        )
        review_required <- identical(size_label, "possible_burst")
        score <- 30 + core_n + 0.05 * n_spikes - 0.25 * sum(bridge_flag[idx] & !seed_flag[idx], na.rm = TRUE)
        add_row(
          s, e,
          "isi_profile_hard_threshold_burst",
          "isi_profile_hard_threshold_burst",
          size_label,
          if (review_required) "isi_profile_hard_threshold_prolonged_review" else "isi_profile_hard_threshold_burst_pass",
          if (review_required) "hard_threshold_structure_exceeds_long_burst_spike_count_range" else "hard_threshold_direct_seed_bridge_without_flank_contrast_gate",
          score,
          if (identical(size_label, "burst")) 1450 else if (identical(size_label, "long_burst")) 1360 else 120,
          list(
            threshold_mode = "hard_threshold",
            hard_threshold = TRUE,
            hard_threshold_pattern = "burst",
            hard_burst_seed_upper_sec = bmax,
            hard_burst_bridge_upper_sec = bridge_high,
            hard_burst_core_isi_count = as.integer(core_n),
            hard_threshold_source = as.character(brr$source %||% "ui_isi_profile_threshold_line")
          ),
          action = if (review_required) "demote_to_possible" else "accept"
        )
      }
    }
  }

  if (length(rows) == 0) return(data.frame())
  stpd_event_core_apply_refractory_suspect_policy(
    dplyr::bind_rows(rows), params, dat = dat, vp = vp, min_isi_sec = min_isi_sec
  )
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
  runs <- stpd_event_core_merge_state_runs(
    runs, isi, valid,
    core_upper = vp$hf_tonic_high_max,
    bridge_upper = vp$hf_tonic_bridge_upper,
    max_bridge_n = vp$hf_tonic_connector_max_n
  )
  rows <- list(); counter <- 0L
  seed_flag <- valid & isi >= vp$seed_low & isi <= vp$seed_high
  for (rr in seq_len(nrow(runs))) {
    s <- as.integer(runs$start_isi[rr]); e <- as.integer(runs$end_isi[rr])
    idx <- stpd_event_core_safe_seq(s, e); if (length(idx) == 0) next
    vals <- isi[idx][valid[idx]]; if (length(vals) == 0) next
    bridge_n <- stpd_event_core_int(runs$state_bridge_isi_n[rr] %||% 0L, 0L)
    core_vals <- vals[vals <= vp$hf_tonic_high_max]
    if (length(core_vals) == 0) next
    n_spikes <- e - s + 2L
    if (n_spikes < vp$hf_tonic_min_spikes) next
    q10 <- stpd_event_core_quantile(core_vals, 0.10); q90 <- stpd_event_core_quantile(core_vals, 0.90)
    low_tail <- mean(core_vals < vp$hf_tonic_floor, na.rm = TRUE)
    core_run_len <- stpd_event_core_max_consecutive_true(seed_flag[idx])
    cv <- stpd_event_core_cv(vals); lv <- stpd_event_core_lv(vals); mm <- stpd_event_core_mm(vals)
    pass <- is.finite(q90) && q90 <= vp$hf_tonic_high_max &&
      (is.finite(q10) && q10 >= vp$hf_tonic_floor || low_tail <= vp$hf_tonic_low_tail_max) &&
      (!is.finite(cv) || cv <= vp$hf_tonic_cv_max) &&
      (!is.finite(lv) || lv <= vp$hf_tonic_lv_max) &&
      (!is.finite(mm) || mm <= vp$hf_tonic_mm_max)
    if (!pass) next
    score <- 5 + (1 - min(low_tail, 1)) + if (is.finite(cv)) 1 / (1 + cv) else 0
    counter <- counter + 1L
    rows[[length(rows) + 1L]] <- stpd_event_core_candidate_from_run(dat, s, e, params, vp, min_isi_sec, train,
      "event_core_hf_tonic_state", "event_core_hf_tonic", "high_frequency_tonic",
      "event_core_hf_tonic_pass", "stable_high_frequency_tonic_state_with_non_destructive_event_overlay", "accept",
      score, 500,
      list(candidate_id = paste0("event_core_hft_", counter), hf_tonic_floor_sec = vp$hf_tonic_floor,
           hf_tonic_low_tail_fraction = low_tail, hf_tonic_core_run_len = core_run_len,
           hf_tonic_burst_like_core_present = core_run_len >= vp$hf_tonic_core_veto_min_isi_n,
           hf_tonic_burst_core_veto_applied = FALSE,
           hf_tonic_q10_sec = q10, hf_tonic_q90_sec = q90,
           hf_tonic_bridge_upper_sec = vp$hf_tonic_bridge_upper,
           hf_tonic_bridge_isi_n = bridge_n,
           hf_tonic_merged_core_run_n = stpd_event_core_int(runs$merged_core_run_n[rr] %||% 1L, 1L)))
  }
  if (length(rows) == 0) return(data.frame())
  dplyr::bind_rows(rows)
}

# One authoritative MM-ceiling calculation shared by canonical Tonic entry,
# review-only proposals, and compatibility-fragment re-gating. The ordinary MM
# ceiling is never relaxed unless both CV and LV establish strong regularity.
stpd_tonic_mm_gate <- function(vp, cv = NA_real_, lv = NA_real_) {
  base_max <- stpd_event_core_num(vp$tonic_mm_max, 1.25)
  contract <- stpd_tonic_mm_relaxation_contract(vp)
  lv_limit <- min(
    stpd_event_core_num(vp$tonic_mm_relax_lv_max, 0.15),
    0.15,
    na.rm = TRUE
  )
  cv_limit <- min(
    stpd_event_core_num(vp$tonic_mm_relax_cv_max, 0.30),
    0.30,
    na.rm = TRUE
  )
  relaxed_max <- max(
    base_max,
    stpd_event_core_num(contract$runtime_relaxed_max, 1.40),
    na.rm = TRUE
  )
  active_lv_limit <- min(
    stpd_event_core_num(vp$tonic_lv_max, lv_limit),
    lv_limit,
    na.rm = TRUE
  )
  applied <- is.finite(lv) && is.finite(active_lv_limit) &&
    lv <= active_lv_limit &&
    is.finite(cv) && is.finite(cv_limit) && cv <= cv_limit
  list(
    base_max = base_max,
    relaxed_max = relaxed_max,
    configured_relaxed_max = contract$configured_relaxed_max,
    effective_max = if (isTRUE(applied)) relaxed_max else base_max,
    applied = isTRUE(applied),
    lv_limit = active_lv_limit,
    cv_limit = cv_limit,
    contract_required = isTRUE(contract$required),
    contract_valid = isTRUE(contract$valid),
    contract_reason = contract$reason,
    fallback_applied = isTRUE(contract$fallback_applied),
    mode = contract$mode,
    status = contract$status
  )
}

stpd_event_core_detect_tonic <- function(dat, params, vp, min_isi_sec = 0.001,
                                         train = "") {
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
  runs <- stpd_event_core_merge_state_runs(
    runs, isi, valid,
    core_upper = tonic_upper,
    bridge_upper = max(tonic_upper, stpd_event_core_num(vp$tonic_bridge_upper, tonic_upper)),
    max_bridge_n = vp$tonic_connector_max_n
  )
  short_contract <- stpd_tonic_short_route_contract(vp)
  rows <- list(); counter <- 0L
  seen <- list()
  add_tonic_run <- function(s, e, source = "full_run", source_s = s, source_e = e,
                            state_bridge_n = 0L, merged_core_run_n = 1L) {
    key <- paste0(as.integer(s), "_", as.integer(e), "_", source)
    if (!is.null(seen[[key]])) return(NULL)
    seen[[key]] <<- TRUE
    idx <- stpd_event_core_safe_seq(s, e); vals <- isi[idx][valid[idx]]
    if (length(vals) == 0) return(NULL)
    n_spikes <- e - s + 2L
    if (n_spikes < vp$tonic_min_spikes) return(NULL)
    duration <- if ("timestamp_sec" %in% names(dat) && s > 1L) {
      suppressWarnings(
        as.numeric(dat$timestamp_sec[e]) - as.numeric(dat$timestamp_sec[s - 1L])
      )
    } else {
      sum(vals, na.rm = TRUE)
    }
    if (!is.finite(duration)) duration <- sum(vals, na.rm = TRUE)
    # Keep the short-ISI overlap calculation as an audit feature only.  A
    # Burst-like Event cannot veto a tonic State; regularity, frequency support,
    # Pause/QC boundaries, and connector limits decide State geometry (D-021).
    burst_overlap_audit_pass <- stpd_tonic_burst_overlap_ok(
      vals, vp$tonic_burst_overlap_ref, p = overlap_p,
      min_isi_sec = min_isi_sec
    )
    lv <- stpd_event_core_lv(vals); mm <- stpd_event_core_mm(vals); cv <- stpd_event_core_cv(vals)
    if (is.finite(lv) && lv > vp$tonic_lv_max) return(NULL)
    mm_gate <- stpd_tonic_mm_gate(vp, cv = cv, lv = lv)
    tonic_mm_max_effective <- mm_gate$effective_max
    if (is.finite(mm) && mm > tonic_mm_max_effective) return(NULL)
    if (is.finite(mm) && mm < vp$tonic_mm_min) return(NULL)
    duration_gate_active <- is.finite(vp$tonic_min_duration) &&
      vp$tonic_min_duration > 0
    duration_support_pass <- !duration_gate_active ||
      (is.finite(duration) && duration >= vp$tonic_min_duration)
    short_count_pass <- isTRUE(short_contract$valid) &&
      length(vals) >= short_contract$min_isi_count
    short_regular_route_pass <- !duration_support_pass &&
      isTRUE(short_count_pass)
    if (!duration_support_pass && !short_regular_route_pass) return(NULL)
    tonic_acceptance_route <- if (!duration_gate_active) {
      "duration_not_required"
    } else if (duration_support_pass) {
      "calibration_duration_support"
    } else {
      "calibration_short_regular_support"
    }
    seed_frac <- mean(vals >= vp$seed_low & vals <= vp$seed_high, na.rm = TRUE)
    score <- 3 + if (is.finite(lv)) 1 / (1 + lv) else 0
    counter <<- counter + 1L
    gate <- if (identical(source, "core_trim")) "event_core_tonic_core_trim_pass" else "event_core_tonic_pass"
    decision <- if (identical(source, "core_trim")) "stable_mid_isi_tonic_core_after_transition_trim" else "stable_mid_isi_tonic_state"
    priority <- if (identical(source, "core_trim")) {
      340
    } else if (identical(source, "bridge_merged")) {
      350 * max(1L, stpd_event_core_int(merged_core_run_n, 1L)) + 10
    } else {
      350
    }
    row <- stpd_event_core_candidate_from_run(dat, s, e, params, vp, min_isi_sec, train,
      "event_core_tonic_state", "event_core_tonic", "tonic",
      gate, decision, "accept",
      score, priority,
      list(candidate_id = paste0("event_core_tonic_", counter),
           tonic_seed_fraction = seed_frac,
           tonic_adaptive_lower_sec = tonic_lower,
           tonic_adaptive_upper_sec = tonic_upper,
           tonic_threshold_source_mode = bounds$tonic_threshold_source_mode,
           tonic_frozen_lower_sec = bounds$tonic_frozen_lower_sec,
           tonic_frozen_upper_sec = bounds$tonic_frozen_upper_sec,
           tonic_train_lower_adaptation_applied =
             isTRUE(bounds$tonic_train_lower_adaptation_applied),
           tonic_train_upper_adaptation_applied =
             isTRUE(bounds$tonic_train_upper_adaptation_applied),
           tonic_legacy_burst_floor_audit_sec = bounds$legacy_burst_floor_audit_sec,
           tonic_burst_floor_applied = isTRUE(bounds$burst_floor_applied),
           tonic_train_q10_sec = bounds$q10,
           tonic_train_q75_sec = bounds$q75,
           tonic_train_q90_sec = bounds$q90,
           tonic_cv = cv,
           tonic_duration_sec = duration,
           tonic_min_duration_sec = vp$tonic_min_duration,
           tonic_duration_gate_applied = duration_gate_active,
           tonic_duration_support_pass = duration_support_pass,
           tonic_short_regular_route_enabled =
             isTRUE(vp$tonic_short_regular_route_enabled),
           tonic_short_regular_route_pass = short_regular_route_pass,
           tonic_short_regular_min_isi_count =
             vp$tonic_short_regular_min_isi_count,
           tonic_short_regular_evidence_n =
             vp$tonic_short_regular_evidence_n,
           tonic_short_regular_group_n = vp$tonic_short_regular_group_n,
           tonic_short_regular_count_q10_full =
             vp$tonic_short_regular_count_q10_full,
           tonic_short_regular_logo_q10_min =
             vp$tonic_short_regular_logo_q10_min,
           tonic_short_regular_logo_q10_max =
             vp$tonic_short_regular_logo_q10_max,
           tonic_short_regular_structural_floor_spikes =
             vp$tonic_short_regular_structural_floor_spikes,
           tonic_short_regular_mode = vp$tonic_short_regular_mode,
           tonic_short_regular_status = vp$tonic_short_regular_status,
           tonic_short_regular_contract_valid =
             isTRUE(short_contract$valid),
           tonic_short_regular_contract_reason = short_contract$reason,
           tonic_acceptance_route = tonic_acceptance_route,
           tonic_mm_base_max = mm_gate$base_max,
           tonic_mm_relaxed_max = mm_gate$relaxed_max,
           tonic_mm_effective_max = tonic_mm_max_effective,
           tonic_mm_relaxation_applied = mm_gate$applied,
           tonic_mm_relaxation_contract_required =
             mm_gate$contract_required,
           tonic_mm_relaxation_contract_valid = mm_gate$contract_valid,
           tonic_mm_relaxation_contract_reason = mm_gate$contract_reason,
           tonic_mm_relaxation_fallback_applied =
             mm_gate$fallback_applied,
           tonic_mm_relax_lv_max = mm_gate$lv_limit,
           tonic_mm_relax_cv_max = mm_gate$cv_limit,
           tonic_mm_relaxation_mode = mm_gate$mode,
           tonic_mm_relaxation_status = mm_gate$status,
           tonic_burst_overlap_ref_sec = vp$tonic_burst_overlap_ref,
           tonic_burst_overlap_guard = isTRUE(overlap_p$burst_overlap_guard),
           tonic_burst_overlap_audit_pass = isTRUE(burst_overlap_audit_pass),
           tonic_burst_overlap_veto_applied = FALSE,
           tonic_seed_fraction_veto_applied = FALSE,
           tonic_bridge_upper_sec = max(tonic_upper, stpd_event_core_num(vp$tonic_bridge_upper, tonic_upper)),
           tonic_bridge_isi_n = stpd_event_core_int(state_bridge_n, 0L),
           tonic_merged_core_run_n = stpd_event_core_int(merged_core_run_n, 1L),
           tonic_core_lower_sec = stpd_event_core_num(bounds$core_lower, tonic_lower),
           tonic_candidate_source = source,
           tonic_source_run_start_isi = source_s,
           tonic_source_run_end_isi = source_e))
    if (!is.null(row) && nrow(row) > 0) rows[[length(rows) + 1L]] <<- row
    invisible(NULL)
  }
  core_lower <- stpd_event_core_num(bounds$core_lower, tonic_lower)
  if (!is.finite(core_lower) || core_lower > tonic_upper) core_lower <- tonic_lower
  for (rr in seq_len(nrow(runs))) {
    s <- as.integer(runs$start_isi[rr]); e <- as.integer(runs$end_isi[rr])
    state_bridge_n <- stpd_event_core_int(runs$state_bridge_isi_n[rr] %||% 0L, 0L)
    merged_core_run_n <- stpd_event_core_int(runs$merged_core_run_n[rr] %||% 1L, 1L)
    add_tonic_run(
      s, e,
      source = if (state_bridge_n > 0L) "bridge_merged" else "full_run",
      source_s = s, source_e = e,
      state_bridge_n = state_bridge_n,
      merged_core_run_n = merged_core_run_n
    )
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

stpd_event_core_pause_hard_boundaries <- function(pause_candidates) {
  empty <- data.frame(
    start_isi = integer(), end_isi = integer(),
    boundary_kind = character(), stringsAsFactors = FALSE
  )
  if (is.null(pause_candidates) || nrow(pause_candidates) == 0L) return(empty)
  semantics <- as.character(pause_candidates$gap_semantics %||% "")
  hard <- as.logical(pause_candidates$hard_for_event %||% FALSE)
  action <- as.character(
    pause_candidates$action %||% pause_candidates$decision_action %||% "accept"
  )
  keep <- semantics == "canonical_pause" & hard & action == "accept"
  keep[is.na(keep)] <- FALSE
  if (!any(keep)) return(empty)
  out <- data.frame(
    start_isi = as.integer(pause_candidates$start_isi[keep]),
    end_isi = as.integer(pause_candidates$end_isi[keep]),
    boundary_kind = "canonical_pause",
    stringsAsFactors = FALSE
  )
  unique(out[order(out$start_isi, out$end_isi), , drop = FALSE])
}

# Final Event boundaries are resolved only after Burst ownership has been
# frozen.  The raw-canonical helper above deliberately remains restricted to
# the first, diagnostic Pause proposal ledger.  This projection additionally
# includes accepted contextual inter-Burst Pauses, because they are genuine
# Event separators even when their ISI is below the ordinary train-level Pause
# threshold.
stpd_event_core_final_event_boundaries <- function(pause_candidates) {
  empty <- data.frame(
    start_isi = integer(), end_isi = integer(),
    boundary_kind = character(), stringsAsFactors = FALSE
  )
  if (is.null(pause_candidates) || nrow(pause_candidates) == 0L) return(empty)
  semantics <- as.character(pause_candidates$gap_semantics %||% "")
  hard <- as.logical(pause_candidates$hard_for_event %||% FALSE)
  action <- as.character(
    pause_candidates$action %||% pause_candidates$decision_action %||% "accept"
  )
  keep <- semantics %in% c(
    "canonical_pause", "contextual_interburst_pause"
  ) & hard & action == "accept"
  keep[is.na(keep)] <- FALSE
  if (!any(keep)) return(empty)
  out <- data.frame(
    start_isi = as.integer(pause_candidates$start_isi[keep]),
    end_isi = as.integer(pause_candidates$end_isi[keep]),
    boundary_kind = semantics[keep],
    stringsAsFactors = FALSE
  )
  unique(out[order(out$start_isi, out$end_isi), , drop = FALSE])
}

stpd_event_core_mask_hard_boundaries <- function(dat, boundaries) {
  out <- dat
  if (is.null(boundaries) || nrow(boundaries) == 0L ||
      !("ISI_sec" %in% names(out))) return(out)
  for (i in seq_len(nrow(boundaries))) {
    idx <- stpd_event_core_safe_seq(
      max(2L, as.integer(boundaries$start_isi[i])),
      min(nrow(out), as.integer(boundaries$end_isi[i]))
    )
    if (length(idx) > 0L) out$ISI_sec[idx] <- NA_real_
  }
  out
}

stpd_event_core_apply_hard_boundaries_to_burst_candidates <- function(
    candidates, boundaries) {
  if (is.null(candidates) || nrow(candidates) == 0L ||
      is.null(boundaries) || nrow(boundaries) == 0L) return(candidates)
  starts <- suppressWarnings(as.integer(candidates$start_isi))
  ends <- suppressWarnings(as.integer(candidates$end_isi))
  blocked <- vapply(seq_len(nrow(candidates)), function(i) {
    if (!is.finite(starts[i]) || !is.finite(ends[i])) return(FALSE)
    any(
      as.integer(boundaries$start_isi) <= ends[i] &
        as.integer(boundaries$end_isi) >= starts[i]
    )
  }, logical(1))
  if (!any(blocked)) return(candidates)
  if (!("action" %in% names(candidates))) candidates$action <- "accept"
  candidates$action[blocked] <- "reject"
  candidates$hard_boundary_conflict <- blocked
  candidates$hard_boundary_conflict_kind <- ifelse(
    blocked, "canonical_pause", ""
  )
  candidates$hard_boundary_decision <- ifelse(
    blocked, "rejected_without_redetection", "not_applicable"
  )
  candidates
}

stpd_event_core_freeze_burst_bridges <- function(dat, params, vp, burst_candidates,
                                                  min_isi_sec = 0.001, train = "",
                                                  hard_boundaries = NULL) {
  # Resolve Burst ownership before generating any Pause. A bounded, slightly
  # longer ISI can be an internal Burst bridge, not an inter-burst Pause.
  if (is.null(burst_candidates) || nrow(burst_candidates) == 0L) return(data.frame())
  labels <- as.character(burst_candidates$final_label %||% "")
  # Raw Burst candidates use `action`; `decision_action` is present only in
  # some compatibility/audit fixtures.  Treat either accepted representation
  # as eligible for ownership resolution.
  action <- as.character(
    burst_candidates$action %||% burst_candidates$decision_action %||% ""
  )
  keep <- labels %in% c("burst", "long_burst") & action == "accept"
  keep[is.na(keep)] <- FALSE
  accepted_pool <- burst_candidates[keep, , drop = FALSE]
  atoms <- accepted_pool
  if (nrow(atoms) == 0L) return(data.frame())
  atoms <- stpd_event_core_weighted_select(
    atoms, locked = NULL, patterns = c("burst", "long_burst")
  )
  selected <- as.logical(atoms$selected_for_auto); selected[is.na(selected)] <- FALSE
  atoms <- atoms[selected, , drop = FALSE]
  if (nrow(atoms) < 2L) return(atoms)

  isi <- suppressWarnings(as.numeric(dat$ISI_sec))
  artifact <- is_artifact_isi(isi, min_isi_sec)
  valid <- is.finite(isi) & !artifact
  if (length(valid) > 0L) valid[1L] <- FALSE
  eg <- params$event_grammar %||% list()
  bridge_factor <- max(1, stpd_event_core_num(
    eg$structural_burst_episode_bridge_factor %||% 1.75, 1.75
  ))
  bridge_upper <- stpd_event_core_num(vp$bridge_high, NA_real_)
  if (!is.finite(bridge_upper) || bridge_upper <= 0) return(atoms)
  # This is a bounded allowance for one internal bridge, not a Pause threshold.
  # The allowance is relative to the calibrated bridge; no absolute ISI cap is
  # imposed across data sets or time scales.
  bridge_ceiling <- max(bridge_upper, bridge_upper * bridge_factor)
  max_gap_n <- max(1L, stpd_event_core_int(vp$max_bridge_n, 4L))
  max_bridge_frac <- min(max(stpd_event_core_num(vp$max_bridge_frac, 0.60), 0), 1)

  merge_pair <- function(left, right, counter) {
    s <- suppressWarnings(as.integer(left$start_isi[1L]))
    e <- suppressWarnings(as.integer(right$end_isi[1L]))
    if (!is.null(hard_boundaries) && nrow(hard_boundaries) > 0L && any(
      as.integer(hard_boundaries$start_isi) <= e &
        as.integer(hard_boundaries$end_isi) >= s
    )) return(NULL)
    # Do not manufacture a larger Burst from two adjacent events.  The
    # underlying Burst generator must already have independently proposed a
    # positive candidate spanning both atoms. This is the evidence that the
    # intervening ISI is a bridge rather than a Pause.
    witness <- accepted_pool[
      as.integer(accepted_pool$start_isi) <= s &
        as.integer(accepted_pool$end_isi) >= e,
      , drop = FALSE
    ]
    if (nrow(witness) == 0L) return(NULL)
    gap <- stpd_event_core_safe_seq(
      suppressWarnings(as.integer(left$end_isi[1L])) + 1L,
      suppressWarnings(as.integer(right$start_isi[1L])) - 1L
    )
    # The ordinary seed/bridge detector already owns gaps within bridge_upper.
    # Only one *expanded* bridge (strictly above that upper bound) receives
    # the Burst-first protection. This avoids fusing neighbouring ordinary
    # episodes before Pause detection.
    if (length(gap) != 1L || length(gap) > max_gap_n || any(!valid[gap])) return(NULL)
    idx <- stpd_event_core_safe_seq(s, e)
    vals <- isi[idx][valid[idx]]
    if (length(vals) == 0L || any(isi[gap] > bridge_ceiling)) return(NULL)
    core_n <- sum(vals >= vp$seed_low & vals <= vp$seed_high, na.rm = TRUE)
    bridge_n <- sum(vals > vp$seed_high & vals <= bridge_ceiling, na.rm = TRUE)
    expanded_bridge_n <- sum(isi[gap] > bridge_upper, na.rm = TRUE)
    q90 <- stpd_event_core_quantile(vals, 0.90)
    if (core_n < vp$min_seed_isi_n || bridge_n > vp$max_bridge_n ||
        bridge_n / length(vals) > max_bridge_frac || expanded_bridge_n != 1L ||
        !is.finite(q90) || q90 > bridge_ceiling) return(NULL)
    n_spikes <- e - s + 2L
    label <- if (n_spikes <= vp$classic_max_spikes) "burst" else "long_burst"
    stpd_event_core_candidate_from_run(
      dat, s, e, params, vp, min_isi_sec, train,
      "event_core_burst_bridge_merge", "event_core_burst_bridge_merge", label,
      "event_core_burst_bridge_merge_pass",
      "burst_bridge_ownership_resolved_before_pause_generation", "accept",
      20 + n_spikes, 1400,
      list(
        candidate_id = paste0("event_core_bridge_merged_burst_", counter),
        bridge_band_upper_sec = bridge_upper,
        burst_bridge_merge_ceiling_sec = bridge_ceiling,
        burst_bridge_merge_factor = bridge_factor,
        burst_bridge_merge_gap_isi_n = length(gap),
        burst_bridge_merge_expanded_bridge_n = expanded_bridge_n,
        burst_bridge_merge_core_isi_n = core_n,
        burst_bridge_merge_bridge_isi_n = bridge_n,
        burst_bridge_merge_source_candidate_ids = paste(
          as.character(left$candidate_id[1L] %||% ""),
          as.character(right$candidate_id[1L] %||% ""), sep = ";"
        ),
        burst_bridge_merge_witness_candidate_ids = paste(
          as.character(witness$candidate_id %||% ""), collapse = ";"
        )
      )
    )
  }

  # One adjacency pass only.  A newly merged envelope is never fed back into
  # the bridge search, preventing chain/percolation merging across a train.
  current <- atoms[order(as.integer(atoms$start_isi), as.integer(atoms$end_isi)), , drop = FALSE]
  next_rows <- list(); counter <- 0L; ii <- 1L
  while (ii <= nrow(current)) {
    if (ii < nrow(current)) {
      merged <- merge_pair(
        current[ii, , drop = FALSE], current[ii + 1L, , drop = FALSE],
        counter + 1L
      )
      if (!is.null(merged) && nrow(merged) > 0L) {
        counter <- counter + 1L
        next_rows[[length(next_rows) + 1L]] <- merged
        ii <- ii + 2L
        next
      }
    }
    next_rows[[length(next_rows) + 1L]] <- current[ii, , drop = FALSE]
    ii <- ii + 1L
  }
  current <- dplyr::bind_rows(next_rows)
  current$selected_for_auto <- TRUE
  current$selection_status <- "burst_bridge_ownership_frozen_before_pause"
  current
}

stpd_event_core_interburst_pause_candidates <- function(dat, params, vp, burst_candidates,
                                                        min_isi_sec = 0.001, train = "") {
  n <- nrow(dat)
  if (n <= 2L || is.null(burst_candidates) || nrow(burst_candidates) < 2L) {
    return(data.frame())
  }

  bursts <- burst_candidates
  labels <- as.character(bursts$final_label %||% bursts$class %||% "")
  labels[is.na(labels)] <- ""
  keep <- labels %in% c("burst", "long_burst")
  if ("decision_action" %in% names(bursts)) {
    action <- as.character(bursts$decision_action)
    action[is.na(action)] <- ""
    keep <- keep & action == "accept"
  }
  bursts <- bursts[keep, , drop = FALSE]
  if (nrow(bursts) < 2L) return(data.frame())

  selected_flag <- if ("selected_for_auto" %in% names(bursts)) {
    flag <- as.logical(bursts$selected_for_auto)
    flag[is.na(flag)] <- FALSE
    flag
  } else {
    rep(FALSE, nrow(bursts))
  }
  if (!any(selected_flag)) {
    bursts <- stpd_event_core_weighted_select(
      bursts, locked = NULL, patterns = c("burst", "long_burst")
    )
    selected_flag <- as.logical(bursts$selected_for_auto)
    selected_flag[is.na(selected_flag)] <- FALSE
  }
  bursts <- bursts[selected_flag, , drop = FALSE]
  if (nrow(bursts) < 2L) return(data.frame())

  starts <- suppressWarnings(as.integer(bursts$start_isi))
  ends <- suppressWarnings(as.integer(bursts$end_isi))
  ok <- is.finite(starts) & is.finite(ends) & starts >= 2L & ends <= n & starts <= ends
  bursts <- bursts[ok, , drop = FALSE]
  starts <- starts[ok]; ends <- ends[ok]
  if (nrow(bursts) < 2L) return(data.frame())
  ord <- order(starts, ends)
  bursts <- bursts[ord, , drop = FALSE]
  starts <- starts[ord]; ends <- ends[ord]

  isi <- suppressWarnings(as.numeric(dat$ISI_sec))
  artifact <- is_artifact_isi(isi, min_isi_sec)
  valid <- is.finite(isi) & !artifact
  if (length(valid) > 0L) valid[1] <- FALSE
  pp <- params$pause %||% list()
  eg <- params$event_grammar %||% list()
  # An inter-burst Pause has a different meaning from an ordinary long-ISI
  # Pause. It is a structural separator between two independently accepted
  # Burst events, so an absolute or train-median Pause floor would incorrectly
  # reject short-but-non-bridge gaps in a high-rate train.
  local_factor <- stpd_event_core_num(
    eg$pause_relative_local_factor %||% pp$relative_local_factor %||% 1.55, 1.55
  )
  local_factor <- max(1, local_factor)
  burst_factor <- max(1.5, local_factor)
  burst_params <- params$burst %||% list()
  merge_gap_n <- max(
    1L,
    stpd_event_core_int(burst_params$merge_gap_max_n %||% 2L, 2L)
  )
  # A Pause separates two independently accepted Burst events; it is not a
  # Burst connector.  The legacy fragment-merge cap is therefore too narrow
  # for this context.  Reuse the existing event-core bridge-count safeguard as
  # the wider, contract-managed ceiling, while keeping the search local and
  # bounded. The candidate must still be the largest ISI in the gap and pass
  # the local/Burst-flank contrast gates below.
  context_gap_n <- max(
    1L,
    stpd_event_core_int(
      burst_params$event_core_max_bridge_isi_count %||% merge_gap_n,
      merge_gap_n
    )
  )
  max_gap_n <- max(merge_gap_n, context_gap_n)

  rows <- list(); counter <- 0L
  for (ii in seq_len(nrow(bursts) - 1L)) {
    gap <- stpd_event_core_safe_seq(ends[ii] + 1L, starts[ii + 1L] - 1L)
    if (length(gap) < 1L || length(gap) > max_gap_n) next
    gap <- gap[gap >= 2L & gap <= n & valid[gap]]
    if (length(gap) < 1L) next
    gap_isi <- isi[gap]
    candidate_isi <- gap[which.max(gap_isi)]
    candidate_value <- isi[candidate_isi]

    left_values <- isi[stpd_event_core_safe_seq(starts[ii], ends[ii])]
    right_values <- isi[stpd_event_core_safe_seq(starts[ii + 1L], ends[ii + 1L])]
    flank_values <- valid_isi_values(c(left_values, right_values), min_isi_sec)
    burst_median <- if (length(flank_values) > 0L) stats::median(flank_values, na.rm = TRUE) else NA_real_

    # A gap inside the Burst bridge band is a fragmented Burst, not a Pause.
    # Outside the bridge band, compare it only with its flanking Burst cores;
    # do not require it to meet the train's ordinary long-ISI Pause threshold.
    bridge_upper <- stpd_event_core_num(vp$bridge_high, NA_real_)
    if (!is.finite(bridge_upper) || bridge_upper <= 0) next
    burst_contrast_threshold <- if (is.finite(burst_median) && burst_median > 0) {
      burst_factor * burst_median
    } else {
      NA_real_
    }
    context_threshold <- max(c(bridge_upper, burst_contrast_threshold, min_isi_sec), na.rm = TRUE)
    if (!is.finite(context_threshold) || candidate_value <= context_threshold) next

    counter <- counter + 1L
    row <- stpd_event_core_candidate_from_run(
      dat, candidate_isi, candidate_isi, params, vp, min_isi_sec, train,
      "event_core_pause_gap", "event_core_pause_gap", "pause",
      "event_core_pause_gap", "interburst_structural_non_bridge_gap", "accept",
      candidate_value / context_threshold, 300,
      list(
        candidate_id = paste0("event_core_interburst_pause_", counter),
        pause_threshold_sec = vp$pause_thr,
        pause_base_threshold_sec = vp$pause_thr,
        pause_effective_threshold_sec = context_threshold,
        pause_local_median_sec = burst_median,
        pause_global_median_sec = NA_real_,
        pause_relative_local_factor = burst_factor,
        pause_relative_global_factor = NA_real_,
        pause_context_kind = "between_consecutive_burst_events",
        pause_context_rule = "non_bridge_gap_with_flanking_burst_contrast",
        pause_context_gap_isi_n = length(gap),
        pause_context_gap_isi_cap = max_gap_n,
        pause_context_burst_median_sec = burst_median,
        pause_context_burst_factor = burst_factor,
        pause_context_bridge_upper_sec = bridge_upper,
        pause_context_is_below_generic_pause_threshold = is.finite(vp$pause_thr) && candidate_value < vp$pause_thr,
        pause_context_left_burst_candidate_id = as.character(bursts$candidate_id[ii] %||% ""),
        pause_context_right_burst_candidate_id = as.character(bursts$candidate_id[ii + 1L] %||% ""),
        gap_semantics = "contextual_interburst_pause",
        hard_for_event = TRUE,
        hard_for_state_direct_support = TRUE,
        envelope_bridge_eligible = TRUE,
        pause_candidate_decision = "accepted",
        pause_candidate_reason_code =
          "two_independent_burst_cores_with_non_bridge_gap"
      )
    )
    if (!is.null(row) && nrow(row) > 0L) rows[[length(rows) + 1L]] <- row
  }
  if (length(rows) == 0L) return(data.frame())
  dplyr::bind_rows(rows)
}

stpd_event_core_detect_pause <- function(dat, params, vp, min_isi_sec = 0.001, train = "",
                                         burst_candidates = NULL,
                                         generic_candidates = NULL) {
  pause_entry <- stpd_event_core_num(
    vp$pause_thr %||% vp$pause_entry_thr, NA_real_
  )
  pause_strong <- stpd_event_core_num(
    vp$pause_strong_thr %||% vp$pause_thr, NA_real_
  )
  n <- nrow(dat)
  if (n <= 2 || !is.finite(pause_entry) || pause_entry <= 0 ||
      !is.finite(pause_strong) || pause_strong <= 0) return(data.frame())
  isi <- suppressWarnings(as.numeric(dat$ISI_sec))
  art <- is_artifact_isi(isi, min_isi_sec)
  valid <- is.finite(isi) & !art
  if (length(valid) > 0) valid[1] <- FALSE
  vals <- stpd_event_core_valid_train_isis(isi, valid, min_isi_sec)
  pause_source_mode <- tolower(as.character(
    vp$pause_threshold_source_mode %||% "auto"
  )[1L])
  frozen_pause_threshold <- pause_source_mode %in%
    c("user", "manual", "default")
  pause_floor_base <- stpd_event_core_pause_global_floor(isi, valid, vp, min_isi_sec)
  tonic_bounds <- stpd_event_core_tonic_adaptive_bounds(isi, valid, vp, min_isi_sec)
  # In automatic mode, the pooled q90 can fall directly on a dominant regular
  # Tonic band and would then turn the whole band into one Pause run. Use the
  # estimated upper Tonic background only as a lower guard for the *ordinary*
  # long-tail Pause route. A user/manual threshold remains authoritative, and
  # short train-relative inter-Burst pauses are recovered independently by the
  # contextual route below.
  tonic_guard_enabled <- length(vals) >= 10L
  tonic_pause_guard <- if (tonic_guard_enabled && is.finite(tonic_bounds$upper) && tonic_bounds$upper > 0) {
    tonic_bounds$upper * 1.15
  } else NA_real_
  pause_floor <- pause_floor_base
  if (!frozen_pause_threshold && is.finite(tonic_pause_guard)) {
    pause_floor <- max(pause_floor_base, tonic_pause_guard)
  }
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
  # A resolved one-sided Pause threshold is sufficient evidence on its own.
  # In automatic mode that threshold is the pooled label-blind q90; the local
  # and global values remain diagnostic rather than additional vetoes.
  relative_gate_applied <- FALSE
  if (relative_gate_applied) {
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
    strong_active <- isTRUE(vp$pause_strong_active %||% FALSE)
    strong_status <- as.character(
      vp$pause_strong_status %||% "unresolved_q95_relative_fallback"
    )[1L]
    automatic_entry <- !frozen_pause_threshold
    rows[[length(rows) + 1L]] <- stpd_event_core_candidate_from_run(dat, s, e, params, vp, min_isi_sec, train,
      "event_core_pause_gap", "event_core_pause_gap", "pause",
      "event_core_pause_pass",
      if (automatic_entry) "automatic_pooled_q90_pause_gap" else
        "resolved_one_sided_pause_gap",
      "accept",
      score, 300,
      list(candidate_id = paste0("event_core_pause_", counter),
           pause_threshold_sec = pause_entry,
           pause_entry_threshold_sec = pause_entry,
           pause_strong_threshold_sec = pause_strong,
           pause_strong_tail_active = strong_active,
           pause_strong_tail_status = strong_status,
           pause_base_threshold_sec = pause_floor_base,
           pause_effective_threshold_sec = pause_floor,
           pause_tonic_guard_threshold_sec = tonic_pause_guard,
           pause_local_median_sec = loc_med,
           pause_global_median_sec = global_med,
           pause_relative_local_factor = local_factor,
           pause_relative_global_factor = global_factor,
           gap_semantics = "canonical_pause",
           hard_for_event = TRUE,
           hard_for_state_direct_support = TRUE,
           envelope_bridge_eligible = FALSE,
           pause_candidate_decision = "accepted",
           pause_candidate_reason_code =
             if (automatic_entry && strong_active) {
               "automatic_stable_strong_tail_threshold"
             } else if (automatic_entry) {
               "automatic_q90_entry_strong_tail_unresolved"
             } else {
               "resolved_one_sided_pause_threshold"
             }))
  }
  generic <- if (length(rows) == 0L) data.frame() else dplyr::bind_rows(rows)
  if (!is.null(generic_candidates)) {
    generic <- as.data.frame(generic_candidates, stringsAsFactors = FALSE)
  }
  # Burst ownership is resolved before *either* Pause route.  Without this
  # mask, the generic long-ISI route can relabel a bridge already accepted as
  # internal Burst support, even though the structural inter-burst route has
  # correctly omitted it.  A canonical Pause never belongs to frozen Burst
  # support, so this is an ownership rule rather than a threshold relaxation.
  if (nrow(generic) > 0L && !is.null(burst_candidates) &&
      nrow(burst_candidates) > 0L) {
    support_labels <- as.character(burst_candidates$final_label %||% "")
    support_selected <- if ("selected_for_auto" %in% names(burst_candidates)) {
      x <- as.logical(burst_candidates$selected_for_auto); x[is.na(x)] <- FALSE; x
    } else rep(TRUE, nrow(burst_candidates))
    support <- burst_candidates[
      support_selected & support_labels %in% c("burst", "long_burst"),
      , drop = FALSE
    ]
    if (nrow(support) > 0L) {
      blocked <- vapply(seq_len(nrow(generic)), function(i) {
        any(
          as.integer(support$start_isi) <= as.integer(generic$end_isi[i]) &
            as.integer(support$end_isi) >= as.integer(generic$start_isi[i])
        )
      }, logical(1))
      # Preserve every candidate in the append-only ledger.  A conflicting
      # generic Pause loses hard-boundary authority but remains reviewable as
      # an ambiguous connector-versus-Pause observation.
      if (any(blocked)) {
        generic$gap_semantics[blocked] <- "ambiguous_gap"
        generic$hard_for_event[blocked] <- FALSE
        generic$hard_for_state_direct_support[blocked] <- FALSE
        generic$envelope_bridge_eligible[blocked] <- TRUE
        generic$pause_candidate_decision[blocked] <-
          "blocked_by_frozen_burst_support"
        generic$pause_candidate_reason_code[blocked] <-
          "generic_pause_overlaps_frozen_burst_bridge_or_boundary"
        if ("action" %in% names(generic)) generic$action[blocked] <- "reject"
        if ("decision_action" %in% names(generic)) {
          generic$decision_action[blocked] <- "reject"
        }
      }
    }
  }
  interburst <- stpd_event_core_interburst_pause_candidates(
    dat, params, vp, burst_candidates,
    min_isi_sec = min_isi_sec, train = train
  )
  if (nrow(generic) == 0L) return(interburst)
  if (nrow(interburst) == 0L) return(generic)
  dplyr::bind_rows(generic, interburst)
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
  decision_action <- if ("action" %in% names(cands)) {
    as.character(cands$action)
  } else if ("decision_action" %in% names(cands)) {
    as.character(cands$decision_action)
  } else {
    rep("accept", nrow(cands))
  }
  decision_action[is.na(decision_action) | !nzchar(decision_action)] <- "accept"
  rejected <- decision_action %in% c("reject", "abstain", "blocked")
  keep_label <- keep_label & !rejected
  cands$selection_status[rejected] <- paste0(
    "not_selectable_candidate_action__", decision_action[rejected]
  )
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
  if (n <= 1) {
    dat$pattern_auto <- ""
    dat$auto_score <- NA_real_
    attr(dat, "tonic_review_candidates") <- stpd_tonic_review_empty_candidates()
    return(dat)
  }
  vp <- stpd_event_core_params_impl(dat, params, min_isi_sec)
  manual_for_lock <- if (isTRUE(lock_manual) && !is.null(dat$pattern_manual)) as.character(dat$pattern_manual) else rep("", n)
  manual_for_lock[is.na(manual_for_lock)] <- ""
  locked <- manual_for_lock != ""
  patterns <- params$detector$patterns_to_run %||% stpd_default_patterns_to_run()

  raw_pause_candidates <- data.frame()
  pause_hard_boundaries <- stpd_event_core_pause_hard_boundaries(NULL)
  burst_detection_dat <- dat
  if ("pause" %in% patterns) {
    raw_pause_candidates <- stpd_event_core_detect_pause(
      dat, params, vp, min_isi_sec, train, burst_candidates = NULL
    )
    pause_hard_boundaries <- stpd_event_core_pause_hard_boundaries(
      raw_pause_candidates
    )
  }

  profile <- stpd_event_core_train_profile_row(dat, params, vp, min_isi_sec, train)
  cand_rows <- list(profile)
  ton <- data.frame()
  burst_candidates <- data.frame()
  burst_pause_support <- data.frame()
  if (any(c("burst", "long_burst") %in% patterns)) {
    b <- stpd_event_core_detect_burst_events(
      burst_detection_dat, params, vp, min_isi_sec, train
    )
    b <- stpd_event_core_apply_hard_boundaries_to_burst_candidates(
      b, pause_hard_boundaries
    )
    if (nrow(b) > 0) {
      burst_pause_support <- stpd_event_core_freeze_burst_bridges(
        dat, params, vp, b, min_isi_sec = min_isi_sec, train = train,
        hard_boundaries = pause_hard_boundaries
      )
      burst_candidates <- b
      cand_rows[[length(cand_rows) + 1L]] <- b
    }
  }
  hard_thr <- stpd_event_core_detect_hard_isi_thresholds(
    burst_detection_dat, params, vp, min_isi_sec, train
  )
  hard_thr <- stpd_event_core_apply_hard_boundaries_to_burst_candidates(
    hard_thr, pause_hard_boundaries
  )
  if (nrow(hard_thr) > 0) cand_rows[[length(cand_rows) + 1L]] <- hard_thr
  if ("high_frequency_spiking" %in% patterns) {
    hfs <- stpd_event_core_detect_hf_spiking(
      dat, params, vp, min_isi_sec, train,
      hard_boundaries = pause_hard_boundaries
    )
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
    pau <- stpd_event_core_detect_pause(
      dat, params, vp, min_isi_sec, train,
      burst_candidates = burst_pause_support,
      generic_candidates = raw_pause_candidates
    )
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
  # Review-only Tonic proposals are computed after the canonical projection is
  # frozen and live exclusively on a train attribute.  They never enter audit,
  # AUTO, State selection, or any Event/Gap gate.
  attr(dat, "tonic_review_candidates") <- stpd_tonic_review_candidates(
    dat, params, vp, min_isi_sec = min_isi_sec, train = train,
    canonical_tonic = ton, pause_boundaries = pause_hard_boundaries
  )
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
