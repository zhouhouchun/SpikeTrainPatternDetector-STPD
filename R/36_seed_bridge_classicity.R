# ============================================================
# seed-bridge Seed-Bridge-Classicity burst kernel and HF-state guards
# ============================================================
# Biological semantics implemented here:
#   * classic burst / long_burst are event-like packets:
#       short-ISI seed(s) + optional small bridge ISIs + large-ISI--burst--large-ISI boundary.
#   * long_burst remains bounded (default 11-15 spikes) and is not a sustained HF epoch.
#   * high_frequency_tonic is a sustained, regular HF state with an ISI floor; it must not
#       absorb the extreme burst-core ISI band.
#   * high_frequency_spiking is a long HF epoch (default >=30 spikes), allows occasional
#       larger ISIs, and lacks the classic large-ISI--burst--large-ISI event structure.

stpd_seed_bridge_num <- function(x, default = NA_real_) {
  y <- suppressWarnings(as.numeric(x))
  if (length(y) == 0 || !is.finite(y[1])) return(default)
  y[1]
}

stpd_seed_bridge_int <- function(x, default = 0L) {
  y <- suppressWarnings(as.integer(round(as.numeric(x))))
  if (length(y) == 0 || !is.finite(y[1])) return(as.integer(default))
  as.integer(y[1])
}

stpd_seed_bridge_bool_runs <- function(flag) {
  flag <- as.logical(flag)
  flag[is.na(flag)] <- FALSE
  idx <- which(flag)
  if (length(idx) == 0) return(data.frame(start_isi = integer(), end_isi = integer()))
  cuts <- c(1L, which(diff(idx) != 1L) + 1L, length(idx) + 1L)
  rows <- vector("list", length(cuts) - 1L)
  for (i in seq_len(length(cuts) - 1L)) {
    part <- idx[cuts[i]:(cuts[i + 1L] - 1L)]
    rows[[i]] <- data.frame(start_isi = as.integer(part[1]), end_isi = as.integer(part[length(part)]))
  }
  do.call(rbind, rows)
}

stpd_seed_bridge_max_consecutive_true <- function(flag) {
  flag <- as.logical(flag)
  flag[is.na(flag)] <- FALSE
  if (!any(flag)) return(0L)
  runs <- stpd_seed_bridge_bool_runs(flag)
  as.integer(max(runs$end_isi - runs$start_isi + 1L, na.rm = TRUE))
}

stpd_seed_bridge_safe_quantile <- function(x, prob, default = NA_real_) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  if (length(x) == 0) return(default)
  as.numeric(stats::quantile(x, prob, na.rm = TRUE, names = FALSE, type = 7))
}

stpd_seed_bridge_thresholds_classicity <- function(dat, params, min_isi_sec = 0.001) {
  bp <- params$burst %||% list()
  isi <- suppressWarnings(as.numeric(dat$ISI_sec))
  art <- is_artifact_isi(isi, min_isi_sec)
  valid <- is.finite(isi) & !art
  valid[1] <- FALSE
  valid_vals <- isi[valid]

  core_abs <- stpd_seed_bridge_num(bp$seed_bridge_burst_core_max_ISI_sec %||% 0.010, 0.010)
  core_abs_active <- is.finite(core_abs) && core_abs > 0
  core_pct <- stpd_seed_bridge_num(bp$seed_bridge_burst_core_pct_max %||% 25, 25)
  core_pct <- max(0, min(100, core_pct))
  core_pct_thr <- if (length(valid_vals) >= 5 && is.finite(core_pct) && core_pct > 0) {
    stpd_seed_bridge_safe_quantile(valid_vals, core_pct / 100, default = NA_real_)
  } else NA_real_

  # enginec correction:
  #   Candidate discovery should be sensitive. Absolute seed evidence and train-percentile
  #   seed evidence are treated as parallel evidence streams, not as min(abs, percentile).
  #   A very low percentile in a fast train should not tighten a user-entered biological
  #   core ceiling so much that obvious bursts disappear.
  seed_ref <- if (core_abs_active) core_abs else if (is.finite(core_pct_thr) && core_pct_thr > 0) core_pct_thr else 0.010

  bridge_abs <- stpd_seed_bridge_num(bp$seed_bridge_burst_bridge_max_ISI_sec %||% 0.015, 0.015)
  bridge_factor <- stpd_seed_bridge_num(bp$seed_bridge_burst_bridge_factor %||% 1.50, 1.50)
  if (!is.finite(bridge_factor) || bridge_factor < 1) bridge_factor <- 1.50
  if (!is.finite(bridge_abs) || bridge_abs <= 0) bridge_abs <- seed_ref * bridge_factor
  bridge_thr <- max(seed_ref, bridge_abs)

  # core_thr is the extreme-fast/burst-core band for vetoes and display.
  # Percentile-derived seed evidence is capped by the bridge threshold during detection,
  # rather than replacing the absolute core band.
  core_thr <- if (core_abs_active) core_abs else min(seed_ref, bridge_thr)
  core_pct_seed_thr <- if (is.finite(core_pct_thr) && core_pct_thr > 0) min(core_pct_thr, bridge_thr) else NA_real_

  list(
    core_thr = core_thr,
    core_abs = if (core_abs_active) core_abs else NA_real_,
    core_pct_thr = core_pct_thr,
    core_pct_seed_thr = core_pct_seed_thr,
    core_pct = core_pct,
    bridge_thr = bridge_thr,
    bridge_factor = bridge_factor
  )
}

# Override event arbitration pattern-specific final gate with seed-bridge robust semantics.
# Burst-family Max_ISI is interpreted as a core/q90 ceiling, not as "every ISI must be <= Max_ISI".
# HF-spiking Max_ISI is a user-facing hard ceiling.  The separate HF-spiking
# q90/tolerated-gap controls can still allow moderate internal variability when
# this pattern-specific gate is disabled.
stpd_pattern_isi_gate_pass <- function(vals, label, params, min_isi_sec = 0.001) {
  lim <- stpd_pattern_isi_limits_for_label(label, params)
  vals <- suppressWarnings(as.numeric(vals))
  vals <- vals[is.finite(vals) & vals >= min_isi_sec]
  min_active <- is.finite(lim$min_sec) && lim$min_sec > 0
  max_active <- is.finite(lim$max_sec) && lim$max_sec > 0
  if (!min_active && !max_active) {
    return(list(pass = TRUE, min_sec = lim$min_sec, max_sec = lim$max_sec, reason = "pattern_isi_gate_disabled"))
  }
  if (length(vals) == 0) {
    return(list(pass = FALSE, min_sec = lim$min_sec, max_sec = lim$max_sec, reason = "no_valid_isi_for_pattern_isi_gate"))
  }
  label <- as.character(label %||% "")
  q10 <- stpd_seed_bridge_safe_quantile(vals, 0.10, default = NA_real_)
  q90 <- stpd_seed_bridge_safe_quantile(vals, 0.90, default = NA_real_)

  if (label %in% c("burst", "long_burst", "possible_burst")) {
    min_pass <- !min_active || all(vals >= lim$min_sec, na.rm = TRUE)
    max_pass <- !max_active || (is.finite(q90) && q90 <= lim$max_sec)
    reason <- paste(c(if (!min_pass) "below_pattern_Min_ISI", if (!max_pass) "burst_family_q90_above_pattern_Max_ISI"), collapse = ";")
  } else if (label == "high_frequency_spiking") {
    min_pass <- !min_active || all(vals >= lim$min_sec, na.rm = TRUE)
    max_pass <- !max_active || all(vals <= lim$max_sec, na.rm = TRUE)
    reason <- paste(c(if (!min_pass) "below_pattern_Min_ISI", if (!max_pass) "hf_spiking_above_pattern_Max_ISI"), collapse = ";")
  } else if (label == "high_frequency_tonic") {
    min_pass <- !min_active || (is.finite(q10) && q10 >= lim$min_sec)
    max_pass <- !max_active || (is.finite(q90) && q90 <= lim$max_sec)
    reason <- paste(c(if (!min_pass) "hf_tonic_q10_below_pattern_Min_ISI", if (!max_pass) "hf_tonic_q90_above_pattern_Max_ISI"), collapse = ";")
  } else {
    min_pass <- !min_active || all(vals >= lim$min_sec, na.rm = TRUE)
    max_pass <- !max_active || all(vals <= lim$max_sec, na.rm = TRUE)
    reason <- paste(c(if (!min_pass) "below_pattern_Min_ISI", if (!max_pass) "above_pattern_Max_ISI"), collapse = ";")
  }
  if (!nzchar(reason)) reason <- "pattern_isi_gate_pass"
  list(pass = isTRUE(min_pass && max_pass), min_sec = lim$min_sec, max_sec = lim$max_sec, reason = reason)
}

stpd_seed_bridge_enrich_candidate_metrics <- function(m, layer, final_label, gate_status, decision_path, action,
                                                score = NA_real_, extra = list()) {
  if (is.null(m) || nrow(m) == 0) return(NULL)
  m$candidate_layer <- layer
  m$final_label <- final_label
  m$gate_status <- gate_status
  m$decision_path <- decision_path
  m$action <- action
  m$score <- score
  if (length(extra) > 0) {
    for (nm in names(extra)) m[[nm]] <- extra[[nm]]
  }
  m
}

stpd_seed_bridge_detect_burst_seed_bridge_classicity <- function(dat, params, min_isi_sec = 0.001, train = "") {
  dat <- ensure_train_isi_percentiles(dat, min_isi_sec, force = TRUE)
  n <- nrow(dat)
  empty <- data.frame()
  if (n <= 2) return(empty)
  bp <- params$burst %||% list()
  isi <- suppressWarnings(as.numeric(dat$ISI_sec))
  art <- is_artifact_isi(isi, min_isi_sec)
  valid <- is.finite(isi) & !art
  valid[1] <- FALSE
  th <- stpd_seed_bridge_thresholds(dat, params, min_isi_sec)

  isi_pct <- suppressWarnings(as.numeric(dat$ISI_pct))
  core_abs_flag <- if (is.finite(th$core_abs) && th$core_abs > 0) valid & isi <= th$core_abs else rep(FALSE, length(isi))
  core_pct_flag <- if (is.finite(th$core_pct_seed_thr) && th$core_pct_seed_thr > 0 && is.finite(th$core_pct) && th$core_pct > 0) {
    valid & is.finite(isi_pct) & isi_pct <= th$core_pct & isi <= th$core_pct_seed_thr
  } else rep(FALSE, length(isi))
  core_flag <- valid & (core_abs_flag | core_pct_flag)
  bridge_flag <- valid & isi <= th$bridge_thr
  bridge_flag[1] <- FALSE
  runs <- stpd_seed_bridge_bool_runs(bridge_flag)
  if (nrow(runs) == 0) return(empty)

  min_core_n <- max(1L, stpd_seed_bridge_int(bp$seed_bridge_burst_core_min_isi_n %||% 2L, 2L))
  max_bridge_n <- max(0L, stpd_seed_bridge_int(bp$seed_bridge_burst_bridge_max_count %||% 4L, 4L))
  max_bridge_frac <- stpd_seed_bridge_num(bp$seed_bridge_burst_bridge_fraction_max %||% 0.60, 0.60)
  max_bridge_frac <- max(0, min(1, max_bridge_frac))
  classicity_min <- stpd_seed_bridge_num(bp$seed_bridge_burst_classicity_multiplier %||% bp$canonical_burst_edge_multiplier %||% 3.0, 3.0)
  possible_classicity <- stpd_seed_bridge_num(bp$seed_bridge_burst_possible_classicity_multiplier %||% 2.0, 2.0)
  context_min <- stpd_seed_bridge_num(bp$seed_bridge_context_compression_min %||% 1.00, 1.00)
  edge_return_min <- stpd_seed_bridge_num(bp$seed_bridge_edge_return_min %||% 0.00, 0.00)
  classic_max_spikes <- stpd_seed_bridge_int(bp$classic_burst_max_spikes %||% 10L, 10L)
  long_min_spikes <- stpd_seed_bridge_int(bp$long_burst_min_spikes %||% 11L, 11L)
  long_max_spikes <- stpd_seed_bridge_int(bp$long_burst_max_spikes %||% 15L, 15L)
  min_spikes <- stpd_seed_bridge_int(bp$G_min %||% 3L, 3L)

  rows <- list()
  for (i in seq_len(nrow(runs))) {
    s <- as.integer(runs$start_isi[i]); e <- as.integer(runs$end_isi[i])
    if (!is.finite(s) || !is.finite(e) || s < 2L || e > n || e < s) next
    idx <- s:e
    vals <- isi[idx][valid[idx]]
    if (length(vals) == 0) next
    n_spikes <- e - s + 2L
    if (n_spikes < min_spikes) next
    core_n <- sum(core_flag[idx], na.rm = TRUE)
    if (core_n < min_core_n) next
    bridge_n <- sum(bridge_flag[idx] & !core_flag[idx], na.rm = TRUE)
    bridge_frac <- bridge_n / max(1L, length(idx))
    bridge_count_pass <- bridge_n <= max_bridge_n
    bridge_frac_pass <- bridge_frac <= max_bridge_frac
    m <- stpd_arbitration_span_metrics(dat, s, e, params, min_isi_sec = min_isi_sec, train = train, candidate_class = "seed_bridge_burst_seed_bridge")
    if (is.null(m)) next
    has_two_edges <- is.finite(m$pre_gap_sec) && is.finite(m$post_gap_sec)
    edge_pass <- has_two_edges && is.finite(m$edge_ratio) && m$edge_ratio >= classicity_min
    possible_edge_pass <- is.finite(m$edge_ratio) && m$edge_ratio >= possible_classicity
    context_active <- is.finite(context_min) && context_min > 1
    edge_return_active <- is.finite(edge_return_min) && edge_return_min > 0
    context_pass <- !context_active || !is.finite(m$context_compression) || m$context_compression >= context_min
    edge_return_pass <- !edge_return_active || !is.finite(m$edge_return_ratio) || m$edge_return_ratio >= edge_return_min
    core_q90_pass <- is.finite(m$intra_q90_sec) && m$intra_q90_sec <= th$bridge_thr
    neg <- isTRUE(m$manual_negative_veto) && isTRUE(params$detector$manual_negative_labels_enabled %||% TRUE)
    size_label <- "oversized_burst_family"
    if (n_spikes <= classic_max_spikes) size_label <- "burst"
    else if (n_spikes >= long_min_spikes && (long_max_spikes <= 0 || n_spikes <= long_max_spikes)) size_label <- "long_burst"

    final_label <- "reject"
    gate_status <- "reject"
    decision <- "seed_bridge_reject"
    action <- "reject"
    if (!neg && bridge_count_pass && bridge_frac_pass && core_q90_pass && context_pass && edge_return_pass && edge_pass) {
      if (size_label %in% c("burst", "long_burst")) {
        final_label <- size_label
        gate_status <- "seed_bridge_classic_pass"
        action <- "accept"
        decision <- paste0("seed_bridge_classicity_pass__", size_label)
      } else {
        final_label <- "possible_burst"
        gate_status <- "seed_bridge_oversized_review"
        action <- "demote_to_possible"
        decision <- "classic_structure_but_exceeds_long_burst_max_spikes"
      }
    } else if (!neg && bridge_count_pass && bridge_frac_pass && core_q90_pass && possible_edge_pass && context_pass) {
      final_label <- "possible_burst"
      gate_status <- "seed_bridge_possible"
      action <- "demote_to_possible"
      decision <- "seed_bridge_possible_classicity_review"
    } else {
      reasons <- c(
        if (neg) "manual_negative_veto",
        if (!bridge_count_pass) "too_many_bridge_isis",
        if (!bridge_frac_pass) "bridge_fraction_too_high",
        if (!core_q90_pass) "core_q90_exceeds_bridge_threshold",
        if (!context_pass) "context_compression_fail",
        if (!edge_return_pass) "edge_return_fail",
        if (!possible_edge_pass) "classicity_fail"
      )
      decision <- paste(reasons, collapse = ";")
      if (!nzchar(decision)) decision <- "seed_bridge_reject"
    }
    extra <- list(
      train = as.character(train %||% ""),
      candidate_class = "seed_bridge_burst_seed_bridge",
      core_threshold_sec = th$core_thr,
      core_abs_threshold_sec = th$core_abs,
      core_pct_threshold_sec = th$core_pct_thr,
      core_pct_seed_threshold_sec = th$core_pct_seed_thr,
      core_pct_max = th$core_pct,
      bridge_threshold_sec = th$bridge_thr,
      core_abs_isi_count = as.integer(sum(core_abs_flag[idx], na.rm = TRUE)),
      core_pct_isi_count = as.integer(sum(core_pct_flag[idx], na.rm = TRUE)),
      burst_classicity_score = m$edge_ratio,
      burst_classicity_required = classicity_min,
      burst_possible_classicity_required = possible_classicity,
      core_isi_count = as.integer(core_n),
      bridge_isi_count = as.integer(bridge_n),
      bridge_fraction = bridge_frac,
      bridge_count_pass = bridge_count_pass,
      bridge_fraction_pass = bridge_frac_pass,
      core_q90_pass = core_q90_pass,
      edge_gate_pass = edge_pass,
      context_gate_pass = context_pass,
      edge_return_pass = edge_return_pass,
      manual_negative_veto = neg
    )
    score <- if (is.finite(m$edge_ratio)) m$edge_ratio else NA_real_
    rows[[length(rows) + 1L]] <- stpd_seed_bridge_enrich_candidate_metrics(m, "seed_bridge_burst_seed_bridge", final_label, gate_status, decision, action, score, extra)
  }
  if (length(rows) == 0) return(empty)
  dplyr::bind_rows(rows)
}

stpd_seed_bridge_short_flag <- function(dat, params, min_isi_sec = 0.001) {
  dat <- ensure_train_isi_percentiles(dat, min_isi_sec, force = TRUE)
  hp <- params$highfreq %||% list()
  isi <- suppressWarnings(as.numeric(dat$ISI_sec))
  pct <- suppressWarnings(as.numeric(dat$ISI_pct))
  art <- is_artifact_isi(isi, min_isi_sec)
  valid <- is.finite(isi) & !art
  valid[1] <- FALSE
  use_abs <- isTRUE(hp$spiking_use_abs_max %||% TRUE)
  use_pct <- isTRUE(hp$spiking_use_pct_max %||% TRUE)
  abs_max <- stpd_seed_bridge_num(hp$spiking_max_ISI_abs %||% hp$ISI_abs_max %||% hp$T_high_max %||% 0.020, 0.020)
  pct_max <- stpd_seed_bridge_num(hp$spiking_max_ISI_pct %||% hp$ISI_pct_max %||% hp$pct_max %||% 30, 30)
  pct_max <- max(0, min(100, pct_max))
  logic <- as.character(hp$spiking_gate_logic %||% "either")
  abs_flag <- if (use_abs && is.finite(abs_max) && abs_max > 0) is.finite(isi) & isi <= abs_max else rep(FALSE, length(isi))
  pct_flag <- if (use_pct) is.finite(pct) & pct <= pct_max else rep(FALSE, length(isi))
  short <- if (use_abs && use_pct && logic == "both") abs_flag & pct_flag else if (use_abs && use_pct) abs_flag | pct_flag else if (use_abs) abs_flag else if (use_pct) pct_flag else abs_flag | pct_flag
  short & valid
}

stpd_seed_bridge_detect_hf_spiking <- function(dat, params, min_isi_sec = 0.001, train = "") {
  dat <- ensure_train_isi_percentiles(dat, min_isi_sec, force = TRUE)
  n <- nrow(dat)
  empty <- data.frame()
  if (n <= 2) return(empty)
  hp <- params$highfreq %||% list()
  bp <- params$burst %||% list()
  isi <- suppressWarnings(as.numeric(dat$ISI_sec))
  art <- is_artifact_isi(isi, min_isi_sec)
  valid <- is.finite(isi) & !art
  valid[1] <- FALSE
  short_flag <- stpd_seed_bridge_short_flag(dat, params, min_isi_sec)
  abs_max <- stpd_seed_bridge_num(hp$spiking_max_ISI_abs %||% hp$ISI_abs_max %||% hp$T_high_max %||% 0.020, 0.020)
  epoch_bridge <- stpd_seed_bridge_num(hp$spiking_epoch_bridge_ISI_sec %||% 0.030, 0.030)
  if (!is.finite(epoch_bridge) || epoch_bridge <= 0) epoch_bridge <- max(abs_max * 1.5, abs_max)
  hfs_lim <- stpd_pattern_isi_limits_for_label("high_frequency_spiking", params)
  hfs_max_sec <- suppressWarnings(as.numeric(hfs_lim$max_sec %||% NA_real_))[1]
  if (!is.finite(hfs_max_sec) || hfs_max_sec <= 0) hfs_max_sec <- NA_real_
  if (is.finite(hfs_max_sec)) epoch_bridge <- min(epoch_bridge, hfs_max_sec)
  epoch_flag <- valid & isi <= epoch_bridge
  runs <- stpd_seed_bridge_bool_runs(epoch_flag)
  if (nrow(runs) == 0) return(empty)

  min_spikes <- stpd_seed_bridge_int(hp$spiking_min_spikes %||% 30L, 30L)
  min_dur <- stpd_seed_bridge_num(hp$spiking_min_duration %||% 0, 0)
  short_frac_min <- stpd_seed_bridge_num(hp$spiking_short_fraction_min %||% 0.70, 0.70)
  q90_max <- stpd_seed_bridge_num(hp$spiking_q90_max_ISI_sec %||% abs_max, abs_max)
  large_frac_max <- stpd_seed_bridge_num(hp$spiking_allowed_large_isi_fraction %||% 0.20, 0.20)
  max_consec_large <- stpd_seed_bridge_int(hp$spiking_max_consecutive_large_isi %||% 2L, 2L)
  classicity_min <- stpd_seed_bridge_num(bp$seed_bridge_burst_classicity_multiplier %||% bp$canonical_burst_edge_multiplier %||% 3.0, 3.0)

  rows <- list()
  for (i in seq_len(nrow(runs))) {
    s <- as.integer(runs$start_isi[i]); e <- as.integer(runs$end_isi[i])
    if (s < 2L || e > n || e < s) next
    idx <- s:e
    vals <- isi[idx][valid[idx]]
    if (length(vals) == 0) next
    if (is.finite(hfs_max_sec) && any(vals > hfs_max_sec, na.rm = TRUE)) next
    n_spikes <- e - s + 2L
    if (n_spikes < min_spikes) next
    m <- stpd_arbitration_span_metrics(dat, s, e, params, min_isi_sec = min_isi_sec, train = train, candidate_class = "high_frequency_spiking")
    if (is.null(m)) next
    dur <- m$duration_sec
    if (is.finite(min_dur) && min_dur > 0 && (!is.finite(dur) || dur < min_dur)) next
    short_frac <- mean(short_flag[idx], na.rm = TRUE)
    if (!is.finite(short_frac)) short_frac <- 0
    q90_pass <- !is.finite(q90_max) || q90_max <= 0 || (is.finite(m$intra_q90_sec) && m$intra_q90_sec <= q90_max)
    large_flag <- vals > abs_max
    large_frac <- mean(large_flag, na.rm = TRUE)
    if (!is.finite(large_frac)) large_frac <- 0
    consec_large <- stpd_seed_bridge_max_consecutive_true(isi[idx] > abs_max & valid[idx])
    lacks_classic_boundary <- !(is.finite(m$edge_ratio) && m$edge_ratio >= classicity_min && is.finite(m$pre_gap_sec) && is.finite(m$post_gap_sec))
    pass <- short_frac >= short_frac_min && q90_pass && large_frac <= large_frac_max && consec_large <= max_consec_large && lacks_classic_boundary
    if (!pass) next
    extra <- list(
      train = as.character(train %||% ""),
      candidate_class = "high_frequency_spiking",
      hf_short_fraction = short_frac,
      hf_large_isi_fraction = large_frac,
      hf_max_consecutive_large_isi = as.integer(consec_large),
      hf_epoch_bridge_ISI_sec = epoch_bridge,
      hf_q90_gate_pass = q90_pass,
      hf_lacks_classic_boundary = lacks_classic_boundary
    )
    rows[[length(rows) + 1L]] <- stpd_seed_bridge_enrich_candidate_metrics(m, "seed_bridge_hf_spiking_epoch", "high_frequency_spiking", "hf_spiking_pass", "long_hf_epoch_without_classic_burst_boundary", "accept", short_frac, extra)
  }
  if (length(rows) == 0) return(empty)
  dplyr::bind_rows(rows)
}

stpd_seed_bridge_detect_hf_tonic <- function(dat, params, min_isi_sec = 0.001, train = "") {
  dat <- ensure_train_isi_percentiles(dat, min_isi_sec, force = TRUE)
  n <- nrow(dat)
  empty <- data.frame()
  if (n <= 2) return(empty)
  hp <- params$highfreq %||% list()
  bp <- params$burst %||% list()
  isi <- suppressWarnings(as.numeric(dat$ISI_sec))
  art <- is_artifact_isi(isi, min_isi_sec)
  valid <- is.finite(isi) & !art
  valid[1] <- FALSE
  th <- stpd_seed_bridge_thresholds(dat, params, min_isi_sec)
  hf_max <- stpd_seed_bridge_num(hp$T_high_max %||% hp$ISI_abs_max %||% 0.020, 0.020)
  if (!is.finite(hf_max) || hf_max <= 0) return(empty)
  floor_min <- stpd_seed_bridge_num(hp$tonic_min_ISI_floor_sec %||% 0.010, 0.010)
  if (!is.finite(floor_min) || floor_min <= 0) floor_min <- th$core_thr * 1.10
  low_tail_max <- stpd_seed_bridge_num(hp$tonic_low_tail_fraction_max %||% 0.05, 0.05)
  low_tail_max <- max(0, min(1, low_tail_max))
  veto_core <- isTRUE(hp$tonic_burst_core_veto %||% TRUE)
  veto_core_min <- max(1L, stpd_seed_bridge_int(hp$tonic_burst_core_veto_min_isi_n %||% bp$seed_bridge_burst_core_min_isi_n %||% 2L, 2L))
  classicity_min <- stpd_seed_bridge_num(bp$seed_bridge_burst_classicity_multiplier %||% bp$canonical_burst_edge_multiplier %||% 3.0, 3.0)

  flag <- valid & isi <= hf_max
  runs <- stpd_seed_bridge_bool_runs(flag)
  if (nrow(runs) == 0) return(empty)
  min_spikes <- stpd_seed_bridge_int(hp$G_min %||% ((hp$min_isi_n %||% 5L) + 1L), 6L)
  min_dur <- stpd_seed_bridge_num(hp$D_min %||% 0, 0)
  stable_cv <- stpd_seed_bridge_num(hp$stable_CV_max %||% hp$CV_stable_max %||% 0.30, 0.30)
  stable_lv <- stpd_seed_bridge_num(hp$stable_LV_max %||% hp$LV_stable_max %||% 0.35, 0.35)
  stable_mm <- stpd_seed_bridge_num(hp$stable_MM_max %||% hp$MM_stable_max %||% 1.25, 1.25)
  short_frac_min <- stpd_seed_bridge_num(hp$short_fraction_min %||% 0.80, 0.80)

  rows <- list()
  for (i in seq_len(nrow(runs))) {
    s <- as.integer(runs$start_isi[i]); e <- as.integer(runs$end_isi[i])
    if (s < 2L || e > n || e < s) next
    idx <- s:e
    vals <- isi[idx][valid[idx]]
    if (length(vals) < 2) next
    n_spikes <- e - s + 2L
    if (n_spikes < min_spikes) next
    m <- stpd_arbitration_span_metrics(dat, s, e, params, min_isi_sec = min_isi_sec, train = train, candidate_class = "high_frequency_tonic")
    if (is.null(m)) next
    if (is.finite(min_dur) && min_dur > 0 && (!is.finite(m$duration_sec) || m$duration_sec < min_dur)) next
    q10 <- stpd_seed_bridge_safe_quantile(vals, 0.10, default = NA_real_)
    low_tail <- mean(vals < floor_min, na.rm = TRUE)
    if (!is.finite(low_tail)) low_tail <- 0
    low_tail_pass <- (is.finite(q10) && q10 >= floor_min) || low_tail <= low_tail_max
    core_consec <- stpd_seed_bridge_max_consecutive_true((isi[idx] <= th$core_thr) & valid[idx])
    core_veto_pass <- !veto_core || core_consec < veto_core_min
    stable <- is.finite(m$CV) && is.finite(m$LV) && is.finite(m$MM) && m$CV <= stable_cv && m$LV <= stable_lv && m$MM <= stable_mm
    short_frac <- mean(vals <= hf_max, na.rm = TRUE)
    no_classic_boundary <- !(is.finite(m$edge_ratio) && m$edge_ratio >= classicity_min && is.finite(m$pre_gap_sec) && is.finite(m$post_gap_sec))
    pass <- stable && short_frac >= short_frac_min && low_tail_pass && core_veto_pass && no_classic_boundary
    if (!pass) next
    extra <- list(
      train = as.character(train %||% ""),
      candidate_class = "high_frequency_tonic",
      hf_tonic_min_floor_sec = floor_min,
      hf_tonic_low_tail_fraction = low_tail,
      hf_tonic_low_tail_pass = low_tail_pass,
      hf_tonic_core_run_len = as.integer(core_consec),
      hf_tonic_core_veto_pass = core_veto_pass,
      hf_tonic_no_classic_boundary = no_classic_boundary
    )
    rows[[length(rows) + 1L]] <- stpd_seed_bridge_enrich_candidate_metrics(m, "seed_bridge_hf_tonic_state", "high_frequency_tonic", "hf_tonic_pass", "regular_hf_state_above_burst_core_floor", "accept", -m$LV, extra)
  }
  if (length(rows) == 0) return(empty)
  dplyr::bind_rows(rows)
}

stpd_detect_train_seed_bridge_impl <- function(dat, params, min_isi_sec = 0.001, train = "", lock_manual = TRUE) {
  params <- effective_params_for_detector(params)
  dat <- ensure_train_isi_percentiles(dat, min_isi_sec, force = TRUE)
  dat <- ensure_train_local_median_cache(dat, window = (params$burst$local_window %||% 11L), min_isi_sec = min_isi_sec, force = TRUE)
  n <- nrow(dat)
  if (!('pattern_manual_negative' %in% names(dat))) dat$pattern_manual_negative <- rep("", n)
  if (n <= 1) {
    dat$pattern_auto <- ""; dat$auto_score <- NA_real_; return(dat)
  }
  manual_for_lock <- if (isTRUE(lock_manual)) stpd_arbitration_vec_label(dat$pattern_manual, n) else rep("", n)
  locked <- manual_for_lock != ""
  patterns <- params$detector$patterns_to_run %||% stpd_default_patterns_to_run()
  fill_others <- isTRUE(params$detector$fill_others_auto)
  pat <- rep("", n); autoscore <- rep(NA_real_, n)
  audit_rows <- list()

  burst_candidates <- data.frame()
  if (any(c("burst", "long_burst") %in% patterns)) {
    burst_candidates <- stpd_seed_bridge_detect_burst_seed_bridge(dat, params, min_isi_sec = min_isi_sec, train = train)
    if (nrow(burst_candidates) > 0) audit_rows[[length(audit_rows) + 1L]] <- burst_candidates
  }

  hf_spiking <- data.frame()
  if ("high_frequency_spiking" %in% patterns && isTRUE(params$highfreq$enable %||% TRUE)) {
    hf_spiking <- stpd_seed_bridge_detect_hf_spiking(dat, params, min_isi_sec = min_isi_sec, train = train)
    if (nrow(hf_spiking) > 0) audit_rows[[length(audit_rows) + 1L]] <- hf_spiking
  }

  hf_tonic <- data.frame()
  if ("high_frequency_tonic" %in% patterns && isTRUE(params$highfreq$enable %||% TRUE)) {
    hf_tonic <- stpd_seed_bridge_detect_hf_tonic(dat, params, min_isi_sec = min_isi_sec, train = train)
    if (nrow(hf_tonic) > 0) audit_rows[[length(audit_rows) + 1L]] <- hf_tonic
  }

  tonic_candidates <- data.frame()
  if ("tonic" %in% patterns) {
    tb <- detect_tonic_train(dat, rep(FALSE, n), params$tonic, params$burst$T_seed, min_isi_sec = min_isi_sec, train = train)
    if (nrow(tb) > 0) {
      tb$class <- "tonic"; tb$score <- NA_real_
      rows <- list()
      for (i in seq_len(nrow(tb))) rows[[length(rows) + 1L]] <- stpd_arbitration_candidate_row(dat, tb[i, , drop = FALSE], params, "tonic_candidate", min_isi_sec, train)
      rows <- rows[!vapply(rows, is.null, logical(1))]
      if (length(rows) > 0) {
        tonic_candidates <- dplyr::bind_rows(rows)
        audit_rows[[length(audit_rows) + 1L]] <- tonic_candidates
      }
    }
  }

  pause_candidates <- data.frame()
  if ("pause" %in% patterns) {
    pb <- detect_pause_train(dat, rep(FALSE, n), params$pause, params$tonic, min_isi_sec = min_isi_sec, current_labels = rep("", n), train = train)
    attr(dat, "pause_diag") <- pb
    if (nrow(pb) > 0) {
      pb$class <- "pause"; if (!("score" %in% names(pb))) pb$score <- NA_real_
      rows <- list()
      for (i in seq_len(nrow(pb))) rows[[length(rows) + 1L]] <- stpd_arbitration_candidate_row(dat, pb[i, , drop = FALSE], params, "pause_candidate", min_isi_sec, train)
      rows <- rows[!vapply(rows, is.null, logical(1))]
      if (length(rows) > 0) {
        pause_candidates <- dplyr::bind_rows(rows)
        audit_rows[[length(audit_rows) + 1L]] <- pause_candidates
      }
    }
  } else {
    attr(dat, "pause_diag") <- data.frame()
  }

  if (nrow(burst_candidates) > 0) {
    acc <- burst_candidates[as.character(burst_candidates$final_label) %in% c("burst", "long_burst"), , drop = FALSE]
    acc <- acc[order(-suppressWarnings(as.numeric(acc$burst_classicity_score %||% acc$edge_ratio)), suppressWarnings(as.numeric(acc$start_isi))), , drop = FALSE]
    tmp <- stpd_arbitration_write_candidates(pat, autoscore, locked, acc, label_filter = c("burst", "long_burst"))
    pat <- tmp$pat; autoscore <- tmp$score
  }

  if (nrow(hf_spiking) > 0) {
    keep <- rep(TRUE, nrow(hf_spiking))
    for (i in seq_len(nrow(hf_spiking))) {
      idx <- as.integer(hf_spiking$start_isi[i]):as.integer(hf_spiking$end_isi[i])
      keep[i] <- !any(pat[idx] %in% c("burst", "long_burst"), na.rm = TRUE)
    }
    hf2 <- hf_spiking[keep, , drop = FALSE]
    hf2 <- hf2[order(-suppressWarnings(as.numeric(hf2$n_spikes)), suppressWarnings(as.numeric(hf2$start_isi))), , drop = FALSE]
    tmp <- stpd_arbitration_write_candidates(pat, autoscore, locked, hf2, label_filter = "high_frequency_spiking")
    pat <- tmp$pat; autoscore <- tmp$score
  }

  if (nrow(burst_candidates) > 0) {
    poss <- burst_candidates[as.character(burst_candidates$final_label) == "possible_burst", , drop = FALSE]
    poss <- poss[order(-suppressWarnings(as.numeric(poss$burst_classicity_score %||% poss$edge_ratio)), suppressWarnings(as.numeric(poss$start_isi))), , drop = FALSE]
    tmp <- stpd_arbitration_write_candidates(pat, autoscore, locked, poss, label_filter = "possible_burst")
    pat <- tmp$pat; autoscore <- tmp$score
  }

  if (nrow(hf_tonic) > 0) {
    keep <- rep(TRUE, nrow(hf_tonic))
    for (i in seq_len(nrow(hf_tonic))) {
      idx <- as.integer(hf_tonic$start_isi[i]):as.integer(hf_tonic$end_isi[i])
      keep[i] <- !any(pat[idx] %in% c("burst", "long_burst", "possible_burst", "high_frequency_spiking"), na.rm = TRUE)
    }
    hf2 <- hf_tonic[keep, , drop = FALSE]
    hf2 <- hf2[order(-suppressWarnings(as.numeric(hf2$n_spikes)), suppressWarnings(as.numeric(hf2$start_isi))), , drop = FALSE]
    tmp <- stpd_arbitration_write_candidates(pat, autoscore, locked, hf2, label_filter = "high_frequency_tonic")
    pat <- tmp$pat; autoscore <- tmp$score
  }

  if (nrow(tonic_candidates) > 0) {
    tonic_candidates <- tonic_candidates[order(-suppressWarnings(as.numeric(tonic_candidates$n_spikes)), suppressWarnings(as.numeric(tonic_candidates$start_isi))), , drop = FALSE]
    tmp <- stpd_arbitration_write_candidates(pat, autoscore, locked, tonic_candidates, label_filter = "tonic")
    pat <- tmp$pat; autoscore <- tmp$score
  }

  if (nrow(pause_candidates) > 0) {
    pause_candidates <- pause_candidates[order(-suppressWarnings(as.numeric(pause_candidates$max_intra_ISI_sec)), suppressWarnings(as.numeric(pause_candidates$start_isi))), , drop = FALSE]
    tmp <- stpd_arbitration_write_candidates(pat, autoscore, locked, pause_candidates, label_filter = "pause")
    pat <- tmp$pat; autoscore <- tmp$score
  }

  if ("others" %in% patterns && fill_others) {
    art <- is_artifact_isi(dat$ISI_sec, min_isi_sec)
    fill_idx <- which(dat$idx >= 2 & !art & is.finite(dat$ISI_sec) & manual_for_lock == "" & pat == "")
    pat[fill_idx] <- "others"
  }

  dat$pattern_auto <- pat
  dat$auto_score <- autoscore
  dat <- stpd_post_validate_auto_event_sizes(dat, params, min_isi_sec = min_isi_sec, lock_manual = lock_manual, train = train)
  if (length(audit_rows) > 0) attr(dat, "candidate_diagnostic_audit") <- dplyr::bind_rows(audit_rows) else attr(dat, "candidate_diagnostic_audit") <- stpd_arbitration_empty_audit()
  attr(dat, "seed_bridge_thresholds") <- stpd_seed_bridge_thresholds(dat, params, min_isi_sec)
  dat
}

stpd_train_pipeline_arbitrated <- stpd_detect_train_arbitrated

stpd_detect_train_seed_bridge_classicity <- function(dat, params, min_isi_sec = 0.001, train = "", lock_manual = TRUE) {
  params <- effective_params_for_detector(params)
  if (!isTRUE((params$arbitration %||% list())$enabled %||% TRUE)) {
    return(stpd_train_pipeline_near_miss_augmented(dat, params, min_isi_sec = min_isi_sec, train = train, lock_manual = lock_manual))
  }
  if (isTRUE(params$burst$seed_bridge_classicity_enabled %||% TRUE)) {
    return(stpd_detect_train_seed_bridge_impl(dat, params, min_isi_sec = min_isi_sec, train = train, lock_manual = lock_manual))
  }
  stpd_train_pipeline_arbitrated(dat, params, min_isi_sec = min_isi_sec, train = train, lock_manual = lock_manual)
}
