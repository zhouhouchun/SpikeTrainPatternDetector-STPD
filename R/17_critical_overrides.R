# Auto-generated modular extraction from modular reference script.
# Do not edit generated function names blindly; use git for version history.

# ============================================================
# refined critical algorithm overrides
# These definitions intentionally override legacy definitions above.
# ============================================================

empty_seed_candidates_tbl <- function() {
  tibble(
    seed_id = integer(), train = character(), start_isi = integer(), end_isi = integer(),
    start_spike_idx = integer(), end_spike_idx = integer(), n_isi = integer(), n_spikes = integer(),
    start_time_sec = numeric(), end_time_sec = numeric(), duration_sec = numeric(),
    min_ISI_sec = numeric(), median_ISI_sec = numeric(), q_ISI_sec = numeric(), q_ISI_pct = numeric(), max_ISI_sec = numeric(),
    mean_ISI_sec = numeric(), MM = numeric(), LV = numeric(), CV = numeric(),
    pre_edge_ISI_sec = numeric(), post_edge_ISI_sec = numeric(),
    edge_pre_ratio_q = numeric(), edge_post_ratio_q = numeric(),
    edge_contrast_min_q = numeric(), edge_contrast_geom_q = numeric(), edge_contrast_pct_q = numeric(),
    local_median_sec = numeric(), seed_score = numeric(), seed_source = character(), seed_primary = logical(), keep_seed = logical()
  )
}

empty_bridge_candidates_tbl <- function() {
  tibble(
    bridge_id = integer(), train = character(), left_seed_id = integer(), right_seed_id = integer(),
    bridge_start_isi = integer(), bridge_end_isi = integer(), bridge_n_isi = integer(),
    bridge_ISI_max_sec = numeric(), bridge_ISI_mean_sec = numeric(), bridge_ISI_sum_sec = numeric(),
    bridge_ISI_max_pct = numeric(), bridge_pct_ok = logical(), bridge_pct_check_enabled = logical(), bridge_pct_reason = character(), required_bridge_pct_max = numeric(),
    left_seed_q_sec = numeric(), right_seed_q_sec = numeric(), seed_ref_max_q_sec = numeric(), seed_ref_geom_q_sec = numeric(),
    bridge_core_inflate = numeric(), bridge_dynamic_used = logical(), seed_ref_max_bridge_sec = numeric(), seed_ref_geom_bridge_sec = numeric(),
    bridge_ratio_left_q_raw = numeric(), bridge_ratio_right_q_raw = numeric(), bridge_ratio_max_seed_q_raw = numeric(), bridge_ratio_geom_seed_q_raw = numeric(),
    bridge_ratio_left_q = numeric(), bridge_ratio_right_q = numeric(), bridge_ratio_max_seed_q = numeric(), bridge_ratio_geom_seed_q = numeric(),
    required_bridge_ratio_max = numeric(), required_bridge_core_inflate = numeric(),
    merged_start_isi = integer(), merged_end_isi = integer(), merged_n_spikes = integer(), merged_duration_sec = numeric(),
    merged_core_q_sec = numeric(), merged_pre_edge_ISI_sec = numeric(), merged_post_edge_ISI_sec = numeric(),
    merged_edge_contrast_min_q = numeric(), merged_edge_contrast_geom_q = numeric(), merged_edge_min_q = numeric(), merged_edge_geom_q = numeric(),
    raw_ok = logical(), ratio_ok = logical(), ratio_possible = logical(), edge_ok = logical(), weak_edge_ok = logical(),
    bridge_score = numeric(), bridge_class = character(), bridge_reason = character(), rejection_reason = character()
  )
}

seed_row <- function(dat, s_isi, e_isi, seed_id = NA_integer_, train = "", source = "seed", p, min_isi_sec = 0.001) {
  n <- nrow(dat)
  if (s_isi < 2 || e_isi > n || e_isi < s_isi) return(empty_seed_candidates_tbl())
  # Keep percentile cache current even if this helper is called directly from diagnostics/preview.
  dat <- ensure_train_isi_percentiles(dat, min_isi_sec, force = FALSE)
  vals <- valid_isi_values(dat$ISI_sec[s_isi:e_isi], min_isi_sec)
  if (length(vals) == 0) return(empty_seed_candidates_tbl())
  qv <- as.numeric(stats::quantile(vals, clamp(p$contrast_q %||% 0.90, 0.50, 1.00), na.rm = TRUE, names = FALSE))
  qv_pct <- isi_percentile_scalar(qv, dat$ISI_sec, min_isi_sec = min_isi_sec, isi_cache = attr(dat, "isi_cache"))
  edge <- calc_edge_contrast_stats(dat$ISI_sec, s_isi, e_isi, min_isi_sec, p$contrast_q %||% 0.90)
  local_idx <- s_isi:e_isi
  if ("local_median_ISI_sec" %in% names(dat) && length(dat$local_median_ISI_sec) == nrow(dat)) {
    locs <- suppressWarnings(as.numeric(dat$local_median_ISI_sec[local_idx]))
  } else {
    locs <- vapply(local_idx, function(ii) get_local_median(dat$ISI_sec, ii, window = p$local_window %||% 11L, exclude_idx = ii, min_isi_sec = min_isi_sec), numeric(1))
  }
  local_med <- safe_median(locs, default = NA_real_)
  dur <- dat$timestamp_sec[e_isi] - dat$timestamp_sec[s_isi - 1L]
  mm <- if (length(vals) > 0) max(vals) / mean(vals) else NA_real_
  lv <- calc_LV(vals); cv <- calc_CV(vals)
  edge_geom <- edge$contrast_geom_q; edge_min <- edge$contrast_min_q
  local_comp <- if (is.finite(local_med) && is.finite(qv) && qv > 0) local_med / qv else NA_real_
  seed_score <- 0
  if (is.finite(edge_geom) && edge_geom > 0) seed_score <- seed_score + log(edge_geom)
  if (is.finite(local_comp) && local_comp > 0) seed_score <- seed_score + 0.10 * log(local_comp)
  seed_score <- seed_score + 0.05 * log(max(1L, e_isi - s_isi + 2L))
  if (is.finite(mm)) seed_score <- seed_score - 0.05 * max(0, mm - (p$mm_penalty_start %||% 2.50))
  if (is.finite(lv)) seed_score <- seed_score - 0.03 * max(0, lv - (p$lv_penalty_start %||% 1.50))
  keep <- TRUE
  if (is.finite(p$seed_q_max %||% NA_real_) && (p$seed_q_max %||% 0) > 0) keep <- keep && is.finite(qv) && qv <= ((p$seed_q_max %||% 0) * (p$seed_q_loosen %||% 1.35))
  if (is.finite(p$seed_duration_max %||% 0) && (p$seed_duration_max %||% 0) > 0) keep <- keep && is.finite(dur) && dur <= (p$seed_duration_max %||% Inf)
  if (is.finite(p$seed_edge_contrast_min %||% NA_real_) && (p$seed_edge_contrast_min %||% 0) > 1) keep <- keep && (!is.finite(edge_min) || edge_min >= (p$seed_edge_contrast_min %||% 1.05) || is.finite(local_comp))
  tibble(
    seed_id = as.integer(seed_id), train = train, start_isi = as.integer(s_isi), end_isi = as.integer(e_isi),
    start_spike_idx = as.integer(s_isi - 1L), end_spike_idx = as.integer(e_isi),
    n_isi = as.integer(e_isi - s_isi + 1L), n_spikes = as.integer(e_isi - s_isi + 2L),
    start_time_sec = dat$timestamp_sec[s_isi - 1L], end_time_sec = dat$timestamp_sec[e_isi], duration_sec = dur,
    min_ISI_sec = min(vals), median_ISI_sec = stats::median(vals), q_ISI_sec = qv, q_ISI_pct = qv_pct, max_ISI_sec = max(vals),
    mean_ISI_sec = mean(vals), MM = mm, LV = lv, CV = cv,
    pre_edge_ISI_sec = edge$pre, post_edge_ISI_sec = edge$post,
    edge_pre_ratio_q = edge$pre_ratio_q, edge_post_ratio_q = edge$post_ratio_q,
    edge_contrast_min_q = edge$contrast_min_q, edge_contrast_geom_q = edge$contrast_geom_q,
    edge_contrast_pct_q = edge$contrast_pct_q,
    local_median_sec = local_med, seed_score = seed_score, seed_source = source,
    seed_primary = source %in% c("structure", "manual_structure"), keep_seed = keep
  )
}

structure_candidate_row <- function(dat, s_isi, e_isi, structure_id = NA_integer_, train = "", p, min_isi_sec = 0.001, isi_cache = NULL) {
  n <- nrow(dat)
  if (!valid_isi_interval(s_isi, e_isi, n, require_flanks = TRUE)) return(empty_structure_candidates_tbl())
  # Keep percentile cache current even if this helper is called directly from diagnostics/preview.
  dat <- ensure_train_isi_percentiles(dat, min_isi_sec, force = FALSE)
  if (is.null(isi_cache)) isi_cache <- attr(dat, "isi_cache") %||% make_isi_cache(dat$ISI_sec, min_isi_sec)
  vals <- valid_isi_values(dat$ISI_sec[s_isi:e_isi], min_isi_sec)
  if (length(vals) != (e_isi - s_isi + 1L) || length(vals) == 0) return(empty_structure_candidates_tbl())
  vals_pct <- suppressWarnings(as.numeric(dat$ISI_pct[s_isi:e_isi]))
  pre <- dat$ISI_sec[s_isi - 1L]; post <- dat$ISI_sec[e_isi + 1L]
  pre <- if (is.finite(pre) && pre >= min_isi_sec) pre else NA_real_
  post <- if (is.finite(post) && post >= min_isi_sec) post else NA_real_
  qprob <- clamp(p$contrast_q %||% 0.90, 0.50, 1.00)
  core_q <- as.numeric(stats::quantile(vals, qprob, na.rm = TRUE, names = FALSE))
  core_max <- max(vals); core_min <- min(vals); core_median <- stats::median(vals); core_mean <- mean(vals)
  core_q_pct <- isi_percentile_scalar(core_q, dat$ISI_sec, min_isi_sec, isi_cache = isi_cache)
  core_min_pct <- isi_percentile_scalar(core_min, dat$ISI_sec, min_isi_sec, isi_cache = isi_cache)
  core_median_pct <- isi_percentile_scalar(core_median, dat$ISI_sec, min_isi_sec, isi_cache = isi_cache)
  core_max_pct <- isi_percentile_scalar(core_max, dat$ISI_sec, min_isi_sec, isi_cache = isi_cache)
  pre_pct <- isi_percentile_scalar(pre, dat$ISI_sec, min_isi_sec, isi_cache = isi_cache)
  post_pct <- isi_percentile_scalar(post, dat$ISI_sec, min_isi_sec, isi_cache = isi_cache)
  qsum <- calc_ratio_summary(pre, post, core_q); msum <- calc_ratio_summary(pre, post, core_max)
  dur <- dat$timestamp_sec[e_isi] - dat$timestamp_sec[s_isi - 1L]
  mm <- if (mean(vals) > 0) max(vals) / mean(vals) else NA_real_
  lv <- calc_LV(vals); cv <- calc_CV(vals)
  pct_reliable <- train_percentile_reliable(dat, p, min_isi_sec)
  q_min <- p$structure_core_q_min %||% 0; q_max <- p$structure_core_q_max %||% 0
  if (!is.finite(q_max) || q_max <= 0) q_max <- (p$seed_q_max %||% 0.060)
  q_loosen <- p$structure_core_q_loosen %||% 1.25
  q_ok_abs <- is.finite(core_q) && (!is.finite(q_min) || q_min <= 0 || core_q >= q_min) && (!is.finite(q_max) || q_max <= 0 || core_q <= q_max * q_loosen)
  pct_seed_max <- p$adaptive_core_pct_seed_max %||% 25; pct_possible_max <- p$adaptive_core_pct_possible_max %||% 35
  pct_ok_seed <- pct_reliable && isTRUE(p$adaptive_apply_core_pct_to_structure %||% TRUE) && is.finite(core_q_pct) && core_q_pct <= pct_seed_max
  pct_ok_possible <- pct_reliable && isTRUE(p$adaptive_apply_core_pct_to_structure %||% TRUE) && is.finite(core_q_pct) && core_q_pct <= pct_possible_max
  rr <- get_train_burst_range(p, train = train)
  manual_anchor <- stpd_manual_anchor_score(
    value_sec = core_q,
    value_pct = if (pct_reliable) core_q_pct else NA_real_,
    rr = rr
  )
  manual_anchor_active <- isTRUE(manual_anchor$active)
  train_range_ok <- TRUE; train_range_possible <- FALSE; train_range_hard_applied <- FALSE
  if (isTRUE(p$adaptive_use_train_ranges %||% TRUE) && !is.null(rr)) {
    range_match <- train_range_match(
      value_sec = core_q,
      value_pct = if (pct_reliable) core_q_pct else NA_real_,
      rr = rr,
      mode = p$adaptive_range_mode %||% "percentile_or_absolute",
      enforce_lower_sec = isTRUE(p$adaptive_enforce_learned_low %||% FALSE),
      default_low_pct = 0,
      default_high_pct = pct_possible_max
    )
    if (manual_anchor_active) {
      # MANUAL-derived information is a scale anchor, not a hard learned boundary.
      train_range_possible <- isTRUE(manual_anchor$soft_support) || isTRUE(range_match)
    } else {
      train_range_possible <- isTRUE(range_match)
      if (isTRUE(p$adaptive_train_ranges_hard %||% FALSE)) {
        train_range_ok <- train_range_possible
        train_range_hard_applied <- TRUE
      }
    }
  }
  q_ok <- (q_ok_abs || pct_ok_seed || train_range_possible) && train_range_ok
  q_ok_possible <- (q_ok_abs || pct_ok_possible || train_range_possible) && train_range_ok
  dur_ok <- TRUE
  if (is.finite(p$structure_duration_max %||% 0) && (p$structure_duration_max %||% 0) > 0) dur_ok <- is.finite(dur) && dur <= (p$structure_duration_max %||% Inf)
  min_flanks <- safe_int(p$structure_min_flanks %||% 2L, 2L)
  strong <- qsum$n_flank >= min_flanks && is.finite(qsum$contrast_min) && is.finite(qsum$contrast_geom) && qsum$contrast_min >= (p$structure_edge_min %||% 1.25) && qsum$contrast_geom >= (p$structure_edge_geom_min %||% 1.35) && q_ok && dur_ok
  possible <- qsum$n_flank >= 1L && is.finite(qsum$contrast_min) && is.finite(qsum$contrast_geom) && qsum$contrast_min >= (p$structure_edge_possible_min %||% 1.05) && qsum$contrast_geom >= (p$structure_edge_possible_geom_min %||% 1.10) && q_ok_possible && dur_ok
  tonic_like <- is.finite(lv) && is.finite(mm) && lv <= (p$structure_tonic_lv_max %||% 0.35) && mm <= (p$structure_tonic_mm_max %||% 1.20) && length(vals) >= max(3L, safe_int(p$seed_min_isi_n %||% 2L, 2L))
  cls <- if (strong) "structure_seed" else if (possible) "possible_structure" else "reject"
  if (isTRUE(p$structure_exclude_tonic_like %||% FALSE) && tonic_like && cls == "structure_seed") cls <- "possible_structure"
  score <- 0
  if (is.finite(qsum$contrast_geom) && qsum$contrast_geom > 0) score <- score + log(qsum$contrast_geom)
  if (is.finite(qsum$contrast_min) && qsum$contrast_min > 0) score <- score + 0.20 * log(qsum$contrast_min)
  score <- score + 0.04 * log(max(1L, e_isi - s_isi + 2L))
  if (is.finite(mm)) score <- score - 0.06 * max(0, mm - (p$mm_penalty_start %||% 2.50))
  if (is.finite(lv)) score <- score - 0.04 * max(0, lv - (p$lv_penalty_start %||% 1.50))
  if (tonic_like) score <- score - 0.10
  if (manual_anchor_active && is.finite(manual_anchor$score)) score <- score + manual_anchor$score
  reason <- ""
  if (!q_ok_possible) reason <- paste(reason, "core_q_or_pct_range_failed")
  if (!pct_reliable) reason <- paste(reason, "short_train_pct_disabled")
  if (!train_range_ok) reason <- paste(reason, "train_specific_range_failed")
  if (manual_anchor_active && !isTRUE(manual_anchor$soft_support)) reason <- paste(reason, "manual_anchor_distant_soft")
  if (!dur_ok) reason <- paste(reason, "duration_failed")
  if (qsum$n_flank < min_flanks) reason <- paste(reason, "insufficient_flanks")
  if (!is.finite(qsum$contrast_min) || qsum$contrast_min < (p$structure_edge_possible_min %||% 1.05)) reason <- paste(reason, "edge_min_failed")
  if (!is.finite(qsum$contrast_geom) || qsum$contrast_geom < (p$structure_edge_possible_geom_min %||% 1.10)) reason <- paste(reason, "edge_geom_failed")
  if (tonic_like) reason <- paste(reason, "tonic_like")
  hint <- manual_hint_for_interval(dat, s_isi, e_isi)
  decision <- if (cls == "structure_seed") "accept_seed" else if (cls == "possible_structure") "possible_seed" else "reject"
  tibble(structure_id = as.integer(structure_id), train = train, structure_class = cls,
         start_isi = as.integer(s_isi), end_isi = as.integer(e_isi), start_spike_idx = as.integer(s_isi - 1L), end_spike_idx = as.integer(e_isi),
         n_isi = as.integer(e_isi - s_isi + 1L), n_spikes = as.integer(e_isi - s_isi + 2L), start_time_sec = dat$timestamp_sec[s_isi - 1L], end_time_sec = dat$timestamp_sec[e_isi], duration_sec = dur,
         pre_ISI_sec = pre, post_ISI_sec = post, pre_ISI_pct = pre_pct, post_ISI_pct = post_pct,
         core_min_ISI_sec = core_min, core_median_ISI_sec = core_median, core_q_ISI_sec = core_q, core_max_ISI_sec = core_max, core_mean_ISI_sec = core_mean,
         core_min_ISI_pct = core_min_pct, core_median_ISI_pct = core_median_pct, core_q_ISI_pct = core_q_pct, core_max_ISI_pct = core_max_pct,
         core_values_sec = paste(signif(vals, 7), collapse = ";"), core_values_pct = paste(signif(vals_pct, 5), collapse = ";"),
         pre_ratio_q = qsum$pre_ratio, post_ratio_q = qsum$post_ratio, edge_contrast_min_q = qsum$contrast_min, edge_contrast_geom_q = qsum$contrast_geom,
         pre_ratio_max = msum$pre_ratio, post_ratio_max = msum$post_ratio, edge_contrast_min_max = msum$contrast_min, edge_contrast_geom_max = msum$contrast_geom,
         MM = mm, LV = lv, CV = cv, structure_score = score,
         manual_anchor_active = manual_anchor_active,
         manual_anchor_soft_support = isTRUE(manual_anchor$soft_support),
         manual_anchor_score = manual_anchor$score,
         manual_anchor_closeness = manual_anchor$closeness,
         manual_anchor_distance_log = manual_anchor$distance_log,
         manual_anchor_center_sec = manual_anchor$center_sec,
         manual_anchor_spread_log = manual_anchor$spread_log,
         manual_anchor_confidence = manual_anchor$confidence,
         manual_anchor_n = as.integer(manual_anchor$n),
         manual_anchor_source = manual_anchor$source,
         train_range_support = train_range_possible,
         train_range_hard_applied = train_range_hard_applied,
         tonic_like = tonic_like, manual_hint = hint, seed_decision = decision, reject_reason = trimws(reason))
}

stpd_burst_sublabel_labels <- function(dat, min_isi_sec = 0.001) {
  n <- nrow(dat)
  manual <- if ("pattern_manual" %in% names(dat)) as.character(dat$pattern_manual) else rep("", n)
  auto <- if ("pattern_auto" %in% names(dat)) as.character(dat$pattern_auto) else rep("", n)
  manual[is.na(manual)] <- ""
  auto[is.na(auto)] <- ""
  if (exists("compute_final_pattern", mode = "function")) {
    out <- tryCatch(
      compute_final_pattern(manual, auto, dat$ISI_sec, auto_others = FALSE, min_isi_sec = min_isi_sec),
      error = function(e) NULL
    )
    if (!is.null(out) && length(out) == n) {
      out <- as.character(out)
      out[is.na(out)] <- ""
      return(out)
    }
  }
  ifelse(nzchar(manual), manual, auto)
}

stpd_burst_family_runs <- function(labels, allowed_labels = c("burst", "long_burst")) {
  labels <- as.character(labels %||% character())
  labels[is.na(labels)] <- ""
  allowed_labels <- as.character(allowed_labels %||% c("burst", "long_burst"))
  if (length(allowed_labels) == 1L && grepl(",", allowed_labels, fixed = TRUE)) {
    allowed_labels <- trimws(strsplit(allowed_labels, ",", fixed = TRUE)[[1]])
  }
  allowed_labels <- intersect(allowed_labels, c("burst", "long_burst", "possible_burst"))
  if (length(allowed_labels) == 0) allowed_labels <- c("burst", "long_burst")
  n <- length(labels)
  if (n == 0) return(data.frame(start_isi = integer(), end_isi = integer(), label = character()))
  burst_family <- labels %in% allowed_labels
  if (!any(burst_family)) return(data.frame(start_isi = integer(), end_isi = integer(), label = character()))
  r <- rle(burst_family)
  ends <- cumsum(r$lengths)
  starts <- ends - r$lengths + 1L
  rows <- list()
  for (ii in seq_along(r$values)) {
    if (!isTRUE(r$values[ii])) next
    s <- starts[ii]
    e <- ends[ii]
    labs <- labels[s:e]
    labs <- labs[labs %in% allowed_labels]
    lab <- if (length(labs) == 0) "burst" else names(sort(table(labs), decreasing = TRUE))[1]
    rows[[length(rows) + 1L]] <- data.frame(start_isi = s, end_isi = e, label = lab, stringsAsFactors = FALSE)
  }
  if (length(rows) == 0) data.frame(start_isi = integer(), end_isi = integer(), label = character()) else dplyr::bind_rows(rows)
}

stpd_regular_packet_ok <- function(dat, s_isi, e_isi, burst_s, burst_e, p, min_isi_sec = 0.001) {
  n <- nrow(dat)
  if (!valid_isi_interval(s_isi, e_isi, n, require_flanks = TRUE)) return(NULL)
  if (!valid_isi_interval(burst_s, burst_e, n, require_flanks = FALSE)) return(NULL)
  vals <- valid_isi_values(dat$ISI_sec[s_isi:e_isi], min_isi_sec)
  if (length(vals) != (e_isi - s_isi + 1L) || length(vals) < 2L) return(NULL)
  burst_vals <- valid_isi_values(dat$ISI_sec[burst_s:burst_e], min_isi_sec)
  if (length(burst_vals) == 0) return(NULL)

  med <- stats::median(vals)
  q10 <- as.numeric(stats::quantile(vals, 0.10, na.rm = TRUE, names = FALSE))
  q90 <- as.numeric(stats::quantile(vals, 0.90, na.rm = TRUE, names = FALSE))
  maxv <- max(vals)
  minv <- min(vals)
  meanv <- mean(vals)
  mm <- if (is.finite(meanv) && meanv > 0) maxv / meanv else NA_real_
  lv <- calc_LV(vals)
  cv <- calc_CV(vals)
  burst_med <- stats::median(burst_vals)
  burst_q90 <- as.numeric(stats::quantile(burst_vals, 0.90, na.rm = TRUE, names = FALSE))

  min_sec <- suppressWarnings(as.numeric(p$burst_sublabel_regular_min_ISI_sec %||% 0.012))
  max_sec <- suppressWarnings(as.numeric(p$burst_sublabel_regular_max_ISI_sec %||% 0.060))
  cv_max <- suppressWarnings(as.numeric(p$burst_sublabel_regular_cv_max %||% 0.45))
  lv_max <- suppressWarnings(as.numeric(p$burst_sublabel_regular_lv_max %||% 0.60))
  mm_max <- suppressWarnings(as.numeric(p$burst_sublabel_regular_mm_max %||% 1.65))
  spread_max <- suppressWarnings(as.numeric(p$burst_sublabel_regular_q90_q10_max %||% 2.40))
  ratio_min <- suppressWarnings(as.numeric(p$burst_sublabel_packet_to_burst_ratio_min %||% 1.35))
  if (!is.finite(min_sec) || min_sec <= 0) min_sec <- 0.012
  if (!is.finite(max_sec) || max_sec <= min_sec) max_sec <- 0.060
  if (!is.finite(cv_max) || cv_max <= 0) cv_max <- 0.45
  if (!is.finite(lv_max) || lv_max <= 0) lv_max <- 0.60
  if (!is.finite(mm_max) || mm_max <= 0) mm_max <- 1.65
  if (!is.finite(spread_max) || spread_max <= 1) spread_max <- 2.40
  if (!is.finite(ratio_min) || ratio_min <= 0) ratio_min <- 1.35

  ratio_med <- if (is.finite(burst_med) && burst_med > 0) med / burst_med else NA_real_
  ratio_q90 <- if (is.finite(burst_q90) && burst_q90 > 0) q90 / burst_q90 else NA_real_
  spread <- if (is.finite(q10) && q10 > 0) q90 / q10 else NA_real_

  pass <- is.finite(med) && is.finite(q10) && is.finite(q90) &&
    q10 >= min_sec && q90 <= max_sec &&
    is.finite(cv) && cv <= cv_max &&
    is.finite(lv) && lv <= lv_max &&
    is.finite(mm) && mm <= mm_max &&
    is.finite(spread) && spread <= spread_max &&
    is.finite(ratio_q90) && ratio_q90 >= ratio_min
  if (!pass) return(NULL)

  list(
    median = med, q90 = q90, q10 = q10, cv = cv, lv = lv, mm = mm,
    burst_median = burst_med, burst_q90 = burst_q90,
    ratio_median = ratio_med, ratio_q90 = ratio_q90,
    score = (2.2 - cv - 0.7 * lv - 0.25 * max(0, mm - 1)) +
      0.08 * log(max(1L, e_isi - s_isi + 2L)) +
      0.35 * log(max(1.01, ratio_q90))
  )
}

stpd_mine_burst_associated_regular_packets <- function(dat, p, min_isi_sec = 0.001, train = "", start_id = 1L) {
  n <- nrow(dat)
  if (n < 8) return(empty_structure_candidates_tbl())
  labels <- stpd_burst_sublabel_labels(dat, min_isi_sec = min_isi_sec)
  linked_labels <- p$burst_sublabel_link_labels %||% c("burst", "long_burst")
  runs <- stpd_burst_family_runs(labels, allowed_labels = linked_labels)
  if (nrow(runs) == 0) return(empty_structure_candidates_tbl())
  dat <- ensure_train_isi_percentiles(dat, min_isi_sec, force = FALSE)
  isi_cache <- attr(dat, "isi_cache") %||% make_isi_cache(dat$ISI_sec, min_isi_sec)

  min_w <- max(3L, safe_int(p$burst_sublabel_regular_min_isi_n %||% 4L, 4L))
  max_w <- max(min_w, safe_int(p$burst_sublabel_regular_max_isi_n %||% 16L, 16L))
  max_gap_n <- max(0L, safe_int(p$burst_sublabel_regular_max_gap_isi_n %||% 0L, 0L))
  max_gap_sec <- suppressWarnings(as.numeric(p$burst_sublabel_regular_max_gap_sec %||% 0))
  if (!is.finite(max_gap_sec) || max_gap_sec < 0) max_gap_sec <- 0
  max_keep <- max(1L, safe_int(p$burst_sublabel_regular_max_candidates_per_train %||% 120L, 120L))

  rows <- list()
  sid <- as.integer(start_id)
  for (rr in seq_len(nrow(runs))) {
    bs <- suppressWarnings(as.integer(runs$start_isi[rr]))
    be <- suppressWarnings(as.integer(runs$end_isi[rr]))
    blab <- as.character(runs$label[rr])
    for (direction in c("regular_before_burst", "regular_after_burst")) {
      for (gap_n in 0:max_gap_n) {
        for (w in min_w:max_w) {
          if (identical(direction, "regular_before_burst")) {
            e_isi <- bs - 1L - gap_n
            s_isi <- e_isi - w + 1L
            gap_idx <- if (gap_n > 0) (e_isi + 1L):(bs - 1L) else integer(0)
          } else {
            s_isi <- be + 1L + gap_n
            e_isi <- s_isi + w - 1L
            gap_idx <- if (gap_n > 0) (be + 1L):(s_isi - 1L) else integer(0)
          }
          if (!valid_isi_interval(s_isi, e_isi, n, require_flanks = TRUE)) next
          packet_labs <- labels[s_isi:e_isi]
          packet_labs[is.na(packet_labs)] <- ""
          if (any(packet_labs %in% c("burst", "long_burst", "possible_burst"))) next
          gap_sec <- 0
          if (length(gap_idx) > 0) {
            gap_vals <- valid_isi_values(dat$ISI_sec[gap_idx], min_isi_sec)
            if (length(gap_vals) != length(gap_idx)) next
            gap_sec <- sum(gap_vals)
          }
          if (is.finite(max_gap_sec) && gap_sec > max_gap_sec) next
          ok <- stpd_regular_packet_ok(dat, s_isi, e_isi, bs, be, p, min_isi_sec = min_isi_sec)
          if (is.null(ok)) next
          row <- structure_candidate_row(dat, s_isi, e_isi, structure_id = sid, train = train, p = p,
                                         min_isi_sec = min_isi_sec, isi_cache = isi_cache)
          if (nrow(row) == 0) next
          row$structure_class <- "burst_associated_regular_packet"
          row$burst_sublabel <- "interesting_structure"
          row$burst_motif_type <- direction
          row$linked_burst_label <- blab
          row$linked_burst_start_isi <- bs
          row$linked_burst_end_isi <- be
          row$linked_burst_start_time_sec <- if (bs > 1L) dat$timestamp_sec[bs - 1L] else NA_real_
          row$linked_burst_end_time_sec <- dat$timestamp_sec[be]
          row$motif_gap_isi_n <- as.integer(gap_n)
          row$motif_gap_sec <- gap_sec
          row$packet_to_burst_median_ratio <- ok$ratio_median
          row$packet_to_burst_q90_ratio <- ok$ratio_q90
          row$seed_decision <- if (identical(direction, "regular_before_burst")) "burst_sublabel_regular_before_burst" else "burst_sublabel_regular_after_burst"
          row$reject_reason <- ""
          row$structure_score <- ok$score + if (gap_n == 0L) 0.20 else 0
          row$structure_prescan_backend <- "burst_sublabel_posthoc"
          rows[[length(rows) + 1L]] <- row
          sid <- sid + 1L
        }
      }
    }
  }
  if (length(rows) == 0) return(empty_structure_candidates_tbl())
  cand <- dplyr::bind_rows(rows) %>%
    dplyr::arrange(dplyr::desc(.data$structure_score), .data$motif_gap_isi_n, .data$start_isi, .data$end_isi)
  keep <- rep(TRUE, nrow(cand))
  chosen <- list()
  for (ii in seq_len(nrow(cand))) {
    s0 <- suppressWarnings(as.integer(cand$start_isi[ii]))
    e0 <- suppressWarnings(as.integer(cand$end_isi[ii]))
    b0 <- suppressWarnings(as.integer(cand$linked_burst_start_isi[ii]))
    b1 <- suppressWarnings(as.integer(cand$linked_burst_end_isi[ii]))
    dir0 <- as.character(cand$burst_motif_type[ii])
    suppress <- FALSE
    for (ch in chosen) {
      same_burst <- identical(b0, ch$burst_start) && identical(b1, ch$burst_end) && identical(dir0, ch$direction)
      ov <- max(0L, min(e0, ch$end) - max(s0, ch$start) + 1L)
      denom <- max(1L, min(e0 - s0 + 1L, ch$end - ch$start + 1L))
      if (same_burst && ov / denom >= 0.50) {
        suppress <- TRUE
        break
      }
    }
    if (suppress) {
      keep[ii] <- FALSE
    } else {
      chosen[[length(chosen) + 1L]] <- list(start = s0, end = e0, burst_start = b0, burst_end = b1, direction = dir0)
    }
  }
  out <- cand[keep, , drop = FALSE]
  if (nrow(out) > max_keep) out <- utils::head(out, max_keep)
  out <- out %>% dplyr::arrange(.data$start_isi, .data$end_isi, .data$burst_motif_type)
  out$structure_id <- as.integer(start_id) + seq_len(nrow(out)) - 1L
  stpd_normalize_structure_sublabel_columns(out)
}

stpd_normalize_structure_sublabel_columns <- function(df) {
  if (is.null(df) || !is.data.frame(df)) return(df)
  char_cols <- c("burst_sublabel", "burst_motif_type", "linked_burst_label")
  int_cols <- c("linked_burst_start_isi", "linked_burst_end_isi", "motif_gap_isi_n")
  num_cols <- c("linked_burst_start_time_sec", "linked_burst_end_time_sec", "motif_gap_sec",
                "packet_to_burst_median_ratio", "packet_to_burst_q90_ratio")
  for (cc in char_cols) {
    if (!(cc %in% names(df))) df[[cc]] <- ""
    df[[cc]] <- as.character(df[[cc]])
    df[[cc]][is.na(df[[cc]])] <- ""
  }
  for (cc in int_cols) {
    if (!(cc %in% names(df))) df[[cc]] <- NA_integer_
    df[[cc]] <- suppressWarnings(as.integer(df[[cc]]))
  }
  for (cc in num_cols) {
    if (!(cc %in% names(df))) df[[cc]] <- NA_real_
    df[[cc]] <- suppressWarnings(as.numeric(df[[cc]]))
  }
  df
}

stpd_append_burst_sublabel_structures <- function(structures, dat, p, min_isi_sec = 0.001, train = "", run_id = NULL, params_hash = NULL) {
  structures <- stpd_normalize_structure_sublabel_columns(structures %||% empty_structure_candidates_tbl())
  start_id <- if (!is.null(structures) && nrow(structures) > 0 && "structure_id" %in% names(structures)) {
    max(suppressWarnings(as.integer(structures$structure_id)), na.rm = TRUE) + 1L
  } else 1L
  if (!is.finite(start_id)) start_id <- 1L
  motifs <- stpd_mine_burst_associated_regular_packets(dat, p, min_isi_sec = min_isi_sec, train = train, start_id = start_id)
  if (is.null(motifs) || nrow(motifs) == 0) return(structures)
  if (!is.null(run_id) && !("run_id" %in% names(motifs))) motifs$run_id <- run_id
  if (!is.null(params_hash) && !("params_hash" %in% names(motifs))) motifs$params_hash <- params_hash
  out <- dplyr::bind_rows(structures, motifs)
  key_cols <- intersect(c("train", "structure_class", "start_isi", "end_isi", "burst_motif_type",
                          "linked_burst_start_isi", "linked_burst_end_isi"), names(out))
  if (length(key_cols) > 0) out <- dplyr::distinct(out, dplyr::across(dplyr::all_of(key_cols)), .keep_all = TRUE)
  out <- stpd_normalize_structure_sublabel_columns(out %>% dplyr::arrange(start_isi, end_isi, structure_class))
  out$structure_id <- seq_len(nrow(out))
  out
}

stpd_append_burst_sublabel_structures_for_dataset <- function(structures, trains, p, min_isi_sec = 0.001,
                                                             target_trains = NULL, run_id = NULL, params_hash = NULL) {
  structures <- structures %||% empty_structure_candidates_tbl()
  if (is.null(trains) || length(trains) == 0) return(structures)
  target_trains <- target_trains %||% names(trains)
  target_trains <- intersect(target_trains, names(trains))
  out <- structures
  for (tr in target_trains) {
    out <- stpd_append_burst_sublabel_structures(out, trains[[tr]], p, min_isi_sec = min_isi_sec,
                                                 train = tr, run_id = run_id, params_hash = params_hash)
  }
  out
}


mine_structure_candidates <- function(dat, p, min_isi_sec = 0.001, train = "") {
  dat <- ensure_train_isi_percentiles(dat, min_isi_sec, force = FALSE)
  isi_cache <- attr(dat, "isi_cache") %||% make_isi_cache(dat$ISI_sec, min_isi_sec)
  n <- nrow(dat)
  if (n < 5) return(empty_structure_candidates_tbl())

  min_w <- max(1L, safe_int(p$structure_core_min_isi_n %||% p$seed_min_isi_n %||% 2L, 2L))
  max_w <- max(min_w, safe_int(p$structure_core_max_isi_n %||% p$seed_max_isi_n %||% 8L, 8L))
  qprob <- clamp(p$contrast_q %||% 0.90, 0.50, 1.00)
  q_max <- p$structure_core_q_max %||% p$seed_q_max %||% 0.060
  q_loosen <- p$structure_core_q_loosen %||% 1.25
  pct_reliable <- train_percentile_reliable(dat, p, min_isi_sec)
  pct_possible_max <- p$adaptive_core_pct_possible_max %||% 35
  rr_prefilter <- get_train_burst_range(p, train = train)
  range_prefilter_enabled <- isTRUE(p$adaptive_use_train_ranges %||% TRUE) && !is.null(rr_prefilter)
  manual_anchor_prefilter <- range_prefilter_enabled && stpd_range_is_manual_anchor(rr_prefilter)
  use_spread_guard <- isTRUE(p$structure_prefilter_use_spread_guard %||% TRUE)
  core_max_pct_limit <- clamp(suppressWarnings(as.numeric(p$structure_prefilter_core_max_pct %||% 70)), 0, 100)
  core_spread_pct_max <- clamp(suppressWarnings(as.numeric(p$structure_prefilter_core_spread_pct_max %||% 45)), 0, 100)
  max_large_isi_n <- max(0L, safe_int(p$structure_prefilter_max_large_isi_n %||% 1L, 1L))

  # schema: the structure-candidate main path now uses the native C pre-scan
  # wrapper scan_structure_candidates(). R-level scoring is still retained for
  # semantic compatibility: C proposes windows, R computes the full structure row.
  qmax_scan <- if (range_prefilter_enabled) Inf else if (is.finite(q_max) && q_max > 0) q_max * q_loosen * 1.50 else Inf
  pct_scan <- if (range_prefilter_enabled || !pct_reliable) 100 else min(100, pct_possible_max + 15)
  edge_scan_min <- suppressWarnings(as.numeric(p$structure_native_prescan_edge_min %||% 1.00))
  edge_scan_geom <- suppressWarnings(as.numeric(p$structure_native_prescan_edge_geom %||% 1.00))
  if (!is.finite(edge_scan_min)) edge_scan_min <- 1.00
  if (!is.finite(edge_scan_geom)) edge_scan_geom <- 1.00

  windows <- tryCatch(
    scan_structure_candidates(
      isi_sec = dat$ISI_sec,
      isi_pct = dat$ISI_pct,
      min_core_isi_n = min_w,
      max_core_isi_n = max_w,
      core_q90_max_sec = qmax_scan,
      core_pct_max = pct_scan,
      edge_min = edge_scan_min,
      edge_geom = edge_scan_geom,
      min_isi_sec = min_isi_sec
    ),
    error = function(e) NULL
  )

  # Conservative R fallback if the native wrapper is unavailable. This is not
  # the preferred path, but keeps direct source() workflows from failing.
  if (is.null(windows) || !(all(c("start_isi", "end_isi") %in% names(windows)))) {
    rows0 <- list()
    for (w in min_w:max_w) {
      max_s <- n - w
      if (max_s < 3) next
      for (s_isi in 3:max_s) {
        e_isi <- s_isi + w - 1L
        vals <- valid_isi_values(dat$ISI_sec[s_isi:e_isi], min_isi_sec)
        if (length(vals) != w || length(vals) == 0) next
        core_q <- as.numeric(stats::quantile(vals, qprob, na.rm = TRUE, names = FALSE))
        core_pct <- isi_percentile_scalar(core_q, dat$ISI_sec, min_isi_sec, isi_cache = isi_cache)
        pre <- dat$ISI_sec[s_isi - 1L]; post <- dat$ISI_sec[e_isi + 1L]
        if (!is.finite(core_q) || core_q <= 0 || !is.finite(pre) || !is.finite(post)) next
        q_abs_possible <- !is.finite(qmax_scan) || core_q <= qmax_scan
        q_pct_possible <- is.finite(core_pct) && core_pct <= pct_scan
        emin <- min(pre / core_q, post / core_q)
        egeom <- sqrt((pre / core_q) * (post / core_q))
        if ((q_abs_possible || q_pct_possible) && is.finite(emin) && is.finite(egeom) && emin >= edge_scan_min && egeom >= edge_scan_geom) {
          rows0[[length(rows0) + 1L]] <- data.frame(start_isi = s_isi, end_isi = e_isi)
        }
      }
    }
    windows <- if (length(rows0) == 0) data.frame(start_isi = integer(), end_isi = integer()) else dplyr::bind_rows(rows0)
  }

  if (is.null(windows) || nrow(windows) == 0) {
    return(stpd_normalize_structure_sublabel_columns(
      stpd_mine_burst_associated_regular_packets(dat, p, min_isi_sec = min_isi_sec, train = train, start_id = 1L)
    ))
  }
  windows <- windows %>% dplyr::distinct(start_isi, end_isi, .keep_all = TRUE) %>% dplyr::arrange(start_isi, end_isi)

  rows <- list(); sid <- 1L
  for (ii in seq_len(nrow(windows))) {
    s_isi <- suppressWarnings(as.integer(windows$start_isi[ii])); e_isi <- suppressWarnings(as.integer(windows$end_isi[ii]))
    if (!valid_isi_interval(s_isi, e_isi, n, require_flanks = TRUE)) next
    vals <- valid_isi_values(dat$ISI_sec[s_isi:e_isi], min_isi_sec)
    if (length(vals) != (e_isi - s_isi + 1L) || length(vals) == 0) next

    # Preserve explicit hard range prefilters that cannot be represented in the C
    # pre-scan. MANUAL-derived ranges are soft anchors and must not veto strong
    # structural windows before they reach the audited scorer.
    if (range_prefilter_enabled && !manual_anchor_prefilter) {
      core_q <- as.numeric(stats::quantile(vals, qprob, na.rm = TRUE, names = FALSE))
      core_pct <- isi_percentile_scalar(core_q, dat$ISI_sec, min_isi_sec, isi_cache = isi_cache)
      q_abs_possible <- !is.finite(q_max) || q_max <= 0 || (is.finite(core_q) && core_q <= q_max * q_loosen * 1.50)
      q_pct_possible <- pct_reliable && isTRUE(p$adaptive_apply_core_pct_to_structure %||% TRUE) && is.finite(core_pct) && core_pct <= min(100, pct_possible_max + 15)
      q_range_possible <- train_range_match(
        value_sec = core_q,
        value_pct = if (pct_reliable) core_pct else NA_real_,
        rr = rr_prefilter,
        mode = p$adaptive_range_mode %||% "percentile_or_absolute",
        enforce_lower_sec = isTRUE(p$adaptive_enforce_learned_low %||% FALSE),
        default_low_pct = 0,
        default_high_pct = min(100, pct_possible_max + 15)
      )
      if (isTRUE(p$structure_prefilter_rejects %||% TRUE) && !(q_abs_possible || q_pct_possible || q_range_possible)) next
    }

    if (use_spread_guard && pct_reliable) {
      vals_pct <- suppressWarnings(as.numeric(dat$ISI_pct[s_isi:e_isi]))
      vals_pct_ok <- vals_pct[is.finite(vals_pct)]
      if (length(vals_pct_ok) == length(vals)) {
        core_max_pct <- max(vals_pct_ok, na.rm = TRUE)
        core_min_pct <- min(vals_pct_ok, na.rm = TRUE)
        n_large <- sum(vals_pct_ok > core_max_pct_limit, na.rm = TRUE)
        spread_too_wide <- is.finite(core_max_pct) && is.finite(core_min_pct) && (core_max_pct - core_min_pct) > core_spread_pct_max
        too_many_large <- n_large > max_large_isi_n
        if (too_many_large && spread_too_wide) next
      }
    }

    row <- structure_candidate_row(dat, s_isi, e_isi, structure_id = sid, train = train, p = p, min_isi_sec = min_isi_sec, isi_cache = isi_cache)
    if (nrow(row) > 0 && (row$structure_class != "reject" || (is.finite(row$structure_score) && row$structure_score > -0.25))) {
      row$structure_prescan_backend <- "C_wrapper"
      rows[[length(rows) + 1L]] <- row; sid <- sid + 1L
    }
  }
  if (length(rows) == 0) {
    return(stpd_normalize_structure_sublabel_columns(
      stpd_mine_burst_associated_regular_packets(dat, p, min_isi_sec = min_isi_sec, train = train, start_id = 1L)
    ))
  }
  out <- bind_rows(rows) %>% mutate(priority = case_when(structure_class == "structure_seed" ~ 3L, structure_class == "possible_structure" ~ 2L, TRUE ~ 1L)) %>% arrange(desc(priority), desc(structure_score), start_isi, end_isi)
  max_keep <- safe_int(p$structure_max_candidates_per_train %||% 2000L, 2000L)
  if (nrow(out) > max_keep) out <- head(out, max_keep)
  motifs <- stpd_mine_burst_associated_regular_packets(dat, p, min_isi_sec = min_isi_sec, train = train, start_id = nrow(out) + 1L)
  if (!is.null(motifs) && nrow(motifs) > 0) out <- bind_rows(out, motifs)
  out <- out %>% arrange(start_isi, end_isi, desc(priority), desc(structure_score))
  out$structure_id <- seq_len(nrow(out)); out$priority <- NULL; stpd_normalize_structure_sublabel_columns(out)
}


mine_burst_seeds <- function(dat, p, min_isi_sec = 0.001, train = "") {
  n <- nrow(dat)
  if (n <= 2) return(empty_seed_candidates_tbl())
  dat <- ensure_train_isi_percentiles(dat, min_isi_sec, force = FALSE)
  dat <- ensure_train_local_median_cache(dat, window = p$local_window %||% 11L, min_isi_sec = min_isi_sec, force = FALSE)
  isi <- dat$ISI_sec; art <- is_artifact_isi(isi, min_isi_sec); valid <- is.finite(isi) & !art; valid[1] <- FALSE
  seed_source_mode <- p$seed_source_mode %||% "structure_primary"
  if (!seed_source_mode %in% c("hybrid_current", "structure_primary", "structure_only", "fallback_absolute_local")) seed_source_mode <- "structure_primary"
  structure_candidates <- empty_structure_candidates_tbl(); structure_windows <- tibble(start_isi = integer(), end_isi = integer(), source = character())
  if (isTRUE(p$use_structure_candidates %||% TRUE)) {
    structure_candidates <- mine_structure_candidates(dat, p, min_isi_sec = min_isi_sec, train = train)
    structure_windows <- structure_to_seed_windows(structure_candidates, p)
  }
  pieces <- list()
  if (nrow(structure_windows) > 0) pieces[[length(pieces) + 1L]] <- structure_windows
  use_fallback <- seed_source_mode != "structure_only"
  if (seed_source_mode == "fallback_absolute_local" && nrow(structure_windows) > 0) use_fallback <- FALSE
  if (use_fallback) {
    loc_med <- dat$local_median_ISI_sec
    if (is.null(loc_med) || length(loc_med) != n) {
      loc_med <- rep(NA_real_, n)
      if (isTRUE(p$use_local_compression_seed)) for (ii in 2:n) loc_med[ii] <- get_local_median(isi, ii, window = p$local_window %||% 11L, exclude_idx = ii, min_isi_sec = min_isi_sec)
    }
    seed_abs <- valid & FALSE
    if (is.finite(p$T_seed %||% NA_real_) && (p$T_seed %||% 0) > 0) seed_abs <- seed_abs | (valid & isi <= (p$T_seed %||% Inf))
    if (is.finite(p$seed_q_max %||% NA_real_) && (p$seed_q_max %||% 0) > 0) seed_abs <- seed_abs | (valid & isi <= (p$seed_q_max %||% Inf))
    comp_ratio <- max(1.05, suppressWarnings(as.numeric(p$local_compression_min %||% 1.40)))
    seed_rel <- valid & isTRUE(p$use_local_compression_seed) & is.finite(loc_med) & loc_med > 0 & isi <= loc_med / comp_ratio
    seed_mask <- seed_abs | seed_rel; seed_mask[1] <- FALSE
    seg0 <- find_segments(ifelse(seed_mask, "seed", ""), "seed")
    if (nrow(seg0) > 0) for (ii in seq_len(nrow(seg0))) {
      sp <- split_seed_block(isi, seg0$start_isi[ii], seg0$end_isi[ii], p, min_isi_sec = min_isi_sec)
      if (nrow(sp) > 0) {
        sp$source <- vapply(seq_len(nrow(sp)), function(rr) {
          s <- sp$start_isi[rr]; e <- sp$end_isi[rr]
          if (all(seed_abs[s:e], na.rm = TRUE)) "absolute" else if (all(seed_rel[s:e], na.rm = TRUE)) "local_compression" else "mixed"
        }, character(1))
        pieces[[length(pieces) + 1L]] <- sp
      }
    }
  }
  if (length(pieces) == 0) return(empty_seed_candidates_tbl())
  pieces <- bind_rows(pieces) %>% mutate(source = ifelse(is.na(source) | source == "", "seed", source)) %>% distinct(start_isi, end_isi, .keep_all = TRUE) %>% arrange(start_isi, end_isi)
  if (seed_source_mode == "structure_primary" && nrow(structure_windows) > 0 && nrow(pieces) > 0) {
    is_struct <- pieces$source %in% c("structure", "possible_structure")
    keep <- rep(TRUE, nrow(pieces)); struct_ranges <- pieces[is_struct, c("start_isi", "end_isi"), drop = FALSE]
    for (ii in seq_len(nrow(pieces))) if (!is_struct[ii]) {
      s0 <- pieces$start_isi[ii]; e0 <- pieces$end_isi[ii]
      if (nrow(struct_ranges) > 0 && any(!(e0 < struct_ranges$start_isi | s0 > struct_ranges$end_isi))) keep[ii] <- FALSE
    }
    pieces <- pieces[keep, , drop = FALSE]
  }
  min_n <- max(1L, safe_int(p$seed_min_isi_n %||% 2L, 2L)); rows <- list()
  for (ii in seq_len(nrow(pieces))) {
    s0 <- pieces$start_isi[ii]; e0 <- pieces$end_isi[ii]
    if ((e0 - s0 + 1L) < min_n) next
    rows[[length(rows) + 1L]] <- seed_row(dat, s0, e0, seed_id = length(rows) + 1L, train = train, source = pieces$source[ii] %||% "seed", p = p, min_isi_sec = min_isi_sec)
  }
  if (length(rows) == 0) return(empty_seed_candidates_tbl())
  out <- bind_rows(rows) %>% filter(keep_seed) %>% arrange(start_isi, end_isi)
  if (nrow(out) == 0) return(empty_seed_candidates_tbl())
  out <- nms_seed_candidates(out, p = p)
  max_keep <- safe_int(p$seed_bridge_max_seed_candidates %||% 1200L, 1200L)
  if (nrow(out) > max_keep) { out <- out %>% arrange(desc(seed_score), start_isi) %>% head(max_keep) %>% arrange(start_isi, end_isi); out$seed_id <- seq_len(nrow(out)) }
  attr(out, "structure_candidates") <- structure_candidates; out
}



bridge_row <- function(dat, seeds, left_i, right_i, bridge_id = NA_integer_, train = "", p, min_isi_sec = 0.001) {
  n <- nrow(dat)
  dat <- ensure_train_isi_percentiles(dat, min_isi_sec, force = FALSE)
  left <- seeds[left_i, , drop = FALSE]; right <- seeds[right_i, , drop = FALSE]
  gap_s <- left$end_isi + 1L; gap_e <- right$start_isi - 1L; gap_n <- gap_e - gap_s + 1L
  if (gap_n < 0) return(empty_bridge_candidates_tbl())
  if (gap_n > safe_int(p$bridge_gap_max_n %||% 1L, 1L)) return(empty_bridge_candidates_tbl())
  if (gap_n > 0 && !valid_isi_interval(gap_s, gap_e, n, require_flanks = FALSE)) return(empty_bridge_candidates_tbl())
  gap_vals <- if (gap_n > 0) valid_isi_values(dat$ISI_sec[gap_s:gap_e], min_isi_sec) else numeric(0)
  if (gap_n > 0 && length(gap_vals) != gap_n) return(empty_bridge_candidates_tbl())
  bridge_max <- if (gap_n > 0) max(gap_vals) else 0
  bridge_mean <- if (gap_n > 0) mean(gap_vals) else 0
  bridge_sum <- if (gap_n > 0) sum(gap_vals) else 0
  pct_vals <- if (gap_n > 0 && "ISI_pct" %in% names(dat)) suppressWarnings(as.numeric(dat$ISI_pct[gap_s:gap_e])) else numeric(0)
  pct_vals <- pct_vals[is.finite(pct_vals)]
  bridge_pct <- if (gap_n > 0 && length(pct_vals) > 0) max(pct_vals, na.rm = TRUE) else NA_real_
  lq <- left$q_ISI_sec; rq <- right$q_ISI_sec
  ref_max_raw <- max(lq, rq, na.rm = TRUE); ref_geom_raw <- if (is.finite(lq) && is.finite(rq) && lq > 0 && rq > 0) sqrt(lq * rq) else ref_max_raw
  base_inflate <- max(1.00, suppressWarnings(as.numeric(p$bridge_core_inflate %||% 1.25)))
  bridge_core_inflate <- base_inflate
  dynamic_used <- FALSE
  if (isTRUE(p$bridge_dynamic_inflate %||% TRUE)) {
    high_quality_seed <- TRUE
    if (isTRUE(p$bridge_dynamic_requires_strong_seed %||% TRUE)) {
      src_ok <- all(c(as.character(left$seed_source), as.character(right$seed_source)) %in% c("structure", "manual_structure", "possible_structure"))
      score_ok <- is.finite(left$seed_score) && is.finite(right$seed_score) && min(left$seed_score, right$seed_score, na.rm = TRUE) >= (p$score_possible %||% 0.35)
      high_quality_seed <- src_ok || score_ok
    }
    if (high_quality_seed) {
      lv_lr <- safe_median(c(left$LV, right$LV), default = NA_real_); mm_lr <- safe_median(c(left$MM, right$MM), default = NA_real_)
      dyn <- 1 + 0.15 * max(0, (lv_lr %||% 0) - 0.35) + 0.10 * max(0, (mm_lr %||% 1) - 1.20)
      dyn_max <- max(1.00, suppressWarnings(as.numeric(p$bridge_dynamic_inflate_max %||% 1.75)))
      bridge_core_inflate <- clamp(base_inflate * dyn, 1.00, base_inflate * dyn_max)
      dynamic_used <- is.finite(bridge_core_inflate) && abs(bridge_core_inflate - base_inflate) > 1e-12
    }
  }
  lq_bridge <- if (is.finite(lq) && lq > 0) lq * bridge_core_inflate else NA_real_; rq_bridge <- if (is.finite(rq) && rq > 0) rq * bridge_core_inflate else NA_real_
  ref_max <- max(lq_bridge, rq_bridge, na.rm = TRUE); ref_geom <- if (is.finite(lq_bridge) && is.finite(rq_bridge) && lq_bridge > 0 && rq_bridge > 0) sqrt(lq_bridge * rq_bridge) else ref_max
  ratio_left_raw <- if (is.finite(lq) && lq > 0) bridge_max / lq else NA_real_; ratio_right_raw <- if (is.finite(rq) && rq > 0) bridge_max / rq else NA_real_
  ratio_max_raw <- if (is.finite(ref_max_raw) && ref_max_raw > 0) bridge_max / ref_max_raw else NA_real_; ratio_geom_raw <- if (is.finite(ref_geom_raw) && ref_geom_raw > 0) bridge_max / ref_geom_raw else NA_real_
  ratio_left <- if (is.finite(lq_bridge) && lq_bridge > 0) bridge_max / lq_bridge else NA_real_; ratio_right <- if (is.finite(rq_bridge) && rq_bridge > 0) bridge_max / rq_bridge else NA_real_
  ratio_max <- if (is.finite(ref_max) && ref_max > 0) bridge_max / ref_max else NA_real_; ratio_geom <- if (is.finite(ref_geom) && ref_geom > 0) bridge_max / ref_geom else NA_real_
  required_bridge_ratio_max <- ratio_max
  required_bridge_core_inflate <- if (is.finite(ratio_max_raw) && is.finite(p$bridge_ratio_max %||% NA_real_) && (p$bridge_ratio_max %||% 0) > 0) ratio_max_raw / (p$bridge_ratio_max %||% 3.50) else NA_real_
  merged_s <- left$start_isi; merged_e <- right$end_isi
  edge <- calc_edge_contrast_stats(dat$ISI_sec, merged_s, merged_e, min_isi_sec, p$contrast_q %||% 0.90)
  dur <- dat$timestamp_sec[merged_e] - dat$timestamp_sec[merged_s - 1L]
  raw_ok <- TRUE
  if (is.finite(p$bridge_raw_max %||% 0) && (p$bridge_raw_max %||% 0) > 0) raw_ok <- bridge_max <= (p$bridge_raw_max %||% Inf)
  ratio_ok <- is.finite(ratio_max) && ratio_max <= (p$bridge_ratio_max %||% 3.50)
  ratio_possible <- is.finite(ratio_max) && ratio_max <= (p$bridge_ratio_possible_max %||% 5.00)
  pct_check_enabled <- isTRUE(p$bridge_use_pct %||% TRUE) && gap_n > 0 && train_percentile_reliable(dat, p, min_isi_sec) && is.finite(bridge_pct)
  pct_ok <- TRUE; required_bridge_pct_max <- NA_real_; pct_reason <- if (gap_n == 0) "adjacent_no_pct_needed" else "bridge_pct_unavailable_or_unreliable"
  if (pct_check_enabled) {
    left_pct <- suppressWarnings(as.numeric(left$q_ISI_pct %||% NA_real_)); right_pct <- suppressWarnings(as.numeric(right$q_ISI_pct %||% NA_real_))
    seed_pct_ref <- max(left_pct, right_pct, na.rm = TRUE); if (!is.finite(seed_pct_ref)) seed_pct_ref <- 0
    allowed_pct <- clamp(max(seed_pct_ref + (p$bridge_pct_margin %||% 10), p$bridge_pct_max %||% 35), 0, 100)
    pct_ok <- bridge_pct <= allowed_pct; required_bridge_pct_max <- bridge_pct
    pct_reason <- if (pct_ok) "bridge_pct_ok" else "bridge_percentile_too_large"
  }
  edge_ok <- is.finite(edge$contrast_min_q) && is.finite(edge$contrast_geom_q) && edge$contrast_min_q >= (p$bridge_merged_edge_min %||% 1.25) && edge$contrast_geom_q >= (p$bridge_merged_edge_geom_min %||% 1.30)
  weak_edge_ok <- is.finite(edge$contrast_min_q) && is.finite(edge$contrast_geom_q) && edge$contrast_min_q >= (p$contrast_min_possible %||% 1.20) && edge$contrast_geom_q >= (p$contrast_geom_possible %||% 1.30)
  bridge_score <- 0
  if (is.finite(ratio_max) && ratio_max > 0) bridge_score <- bridge_score - log(ratio_max)
  if (is.finite(edge$contrast_geom_q) && edge$contrast_geom_q > 0) bridge_score <- bridge_score + log(edge$contrast_geom_q)
  if (is.finite(ref_max) && is.finite(bridge_max) && bridge_max > 0) bridge_score <- bridge_score + 0.05 * log(ref_max / bridge_max)
  if (pct_check_enabled && is.finite(bridge_pct)) bridge_score <- bridge_score - 0.005 * max(0, bridge_pct - (p$bridge_pct_max %||% 35))
  cls <- "reject"; reason <- "failed"
  if (gap_n == 0) { cls <- "accepted"; reason <- "adjacent_seed" }
  else if (raw_ok && ratio_ok && pct_ok) { cls <- "accepted"; reason <- if (edge_ok) "ratio+pct+edge_ok" else "ratio+pct_only" }
  else if (raw_ok && ratio_possible && pct_ok) { cls <- "possible"; reason <- if (weak_edge_ok) "possible_ratio+weak_edge" else "possible_ratio_only" }
  else if (!raw_ok) reason <- "raw_bridge_too_large" else if (!pct_ok) reason <- pct_reason else if (!ratio_possible) reason <- "bridge_to_seed_ratio_too_large" else reason <- "bridge_failed"
  tibble(bridge_id = as.integer(bridge_id), train = train, left_seed_id = as.integer(left$seed_id), right_seed_id = as.integer(right$seed_id),
         bridge_start_isi = as.integer(gap_s), bridge_end_isi = as.integer(gap_e), bridge_n_isi = as.integer(gap_n),
         bridge_ISI_max_sec = bridge_max, bridge_ISI_mean_sec = bridge_mean, bridge_ISI_sum_sec = bridge_sum,
         bridge_ISI_max_pct = bridge_pct, bridge_pct_ok = pct_ok, bridge_pct_check_enabled = pct_check_enabled, bridge_pct_reason = pct_reason, required_bridge_pct_max = required_bridge_pct_max,
         left_seed_q_sec = lq, right_seed_q_sec = rq, seed_ref_max_q_sec = ref_max_raw, seed_ref_geom_q_sec = ref_geom_raw,
         bridge_core_inflate = bridge_core_inflate, bridge_dynamic_used = dynamic_used, seed_ref_max_bridge_sec = ref_max, seed_ref_geom_bridge_sec = ref_geom,
         bridge_ratio_left_q_raw = ratio_left_raw, bridge_ratio_right_q_raw = ratio_right_raw, bridge_ratio_max_seed_q_raw = ratio_max_raw, bridge_ratio_geom_seed_q_raw = ratio_geom_raw,
         bridge_ratio_left_q = ratio_left, bridge_ratio_right_q = ratio_right, bridge_ratio_max_seed_q = ratio_max, bridge_ratio_geom_seed_q = ratio_geom,
         required_bridge_ratio_max = required_bridge_ratio_max, required_bridge_core_inflate = required_bridge_core_inflate,
         merged_start_isi = as.integer(merged_s), merged_end_isi = as.integer(merged_e), merged_n_spikes = as.integer(merged_e - merged_s + 2L),
         merged_duration_sec = dur,
         merged_core_q_sec = edge$core_q, merged_pre_edge_ISI_sec = edge$pre, merged_post_edge_ISI_sec = edge$post,
         merged_edge_contrast_min_q = edge$contrast_min_q, merged_edge_contrast_geom_q = edge$contrast_geom_q,
         merged_edge_min_q = edge$contrast_min_q, merged_edge_geom_q = edge$contrast_geom_q,
         raw_ok = raw_ok, ratio_ok = ratio_ok, ratio_possible = ratio_possible, edge_ok = edge_ok, weak_edge_ok = weak_edge_ok,
         bridge_class = cls, bridge_score = bridge_score, bridge_reason = reason, rejection_reason = reason)
}


candidate_row_from_component <- function(dat, seeds, bridges, seed_ids, candidate_id = NA_integer_, train = "", p, min_isi_sec = 0.001) {
  ss <- seeds %>% filter(seed_id %in% seed_ids) %>% arrange(start_isi)
  if (nrow(ss) == 0) return(empty_burst_candidates_tbl())
  n <- nrow(dat); s_isi <- min(ss$start_isi); e_isi <- max(ss$end_isi)
  if (!valid_isi_interval(s_isi, e_isi, n, require_flanks = FALSE)) return(empty_burst_candidates_tbl())
  vals <- valid_isi_values(dat$ISI_sec[s_isi:e_isi], min_isi_sec)
  if (length(vals) == 0) return(empty_burst_candidates_tbl())
  edge <- calc_edge_contrast_stats(dat$ISI_sec, s_isi, e_isi, min_isi_sec, p$contrast_q %||% 0.90)
  seed_vals <- numeric(0)
  for (jj in seq_len(nrow(ss))) seed_vals <- c(seed_vals, valid_isi_values(dat$ISI_sec[ss$start_isi[jj]:ss$end_isi[jj]], min_isi_sec))
  seed_core_q <- if (length(seed_vals) > 0) as.numeric(stats::quantile(seed_vals, clamp(p$contrast_q %||% 0.90, 0.50, 1.00), na.rm = TRUE, names = FALSE)) else edge$core_q
  seed_sum <- calc_ratio_summary(edge$pre, edge$post, seed_core_q); seed_edge_min <- seed_sum$contrast_min; seed_edge_geom <- seed_sum$contrast_geom
  br <- bridges %>% filter(left_seed_id %in% seed_ids & right_seed_id %in% seed_ids & bridge_class == "accepted")
  dur <- dat$timestamp_sec[e_isi] - dat$timestamp_sec[s_isi - 1L]; n_spk <- e_isi - s_isi + 2L
  mm <- max(vals) / mean(vals); lv <- calc_LV(vals); cv <- calc_CV(vals)
  tonic_like_final <- isTRUE(p$final_tonic_like_veto %||% TRUE) && n_spk >= safe_int(p$final_tonic_like_min_spikes %||% 6L, 6L) && is.finite(lv) && is.finite(cv) && is.finite(mm) && lv <= (p$final_tonic_like_lv_max %||% 0.35) && cv <= (p$final_tonic_like_cv_max %||% 0.30) && mm <= (p$final_tonic_like_mm_max %||% 1.20)
  tonic_action <- ""; score <- 0
  if (is.finite(seed_edge_geom) && seed_edge_geom > 0) score <- score + log(seed_edge_geom)
  if (is.finite(seed_edge_min) && seed_edge_min > 0) score <- score + 0.20 * log(seed_edge_min)
  if (is.finite(edge$contrast_geom_q) && edge$contrast_geom_q > 0) score <- score + 0.05 * log(edge$contrast_geom_q)
  score <- score + 0.06 * log(max(1L, n_spk)) + 0.08 * log(max(1L, nrow(ss)))
  if (nrow(br) > 0) score <- score + 0.03 * log(1 + nrow(br))
  if (is.finite(mm)) score <- score - 0.10 * max(0, mm - (p$mm_penalty_start %||% 2.50))
  if (is.finite(lv)) score <- score - 0.05 * max(0, lv - (p$lv_penalty_start %||% 1.50))
  reason <- ""; pass_basic <- n_spk >= (p$G_min %||% 3L) && dur >= (p$D_min %||% 0)
  if (!pass_basic) reason <- paste(reason, "basic_size_or_duration_failed")
  if (is.finite(p$D_max %||% 0) && (p$D_max %||% 0) > 0 && dur > (p$D_max %||% Inf)) { pass_basic <- FALSE; reason <- paste(reason, "D_max_failed") }
  if (is.finite(p$final_max_duration %||% 0) && (p$final_max_duration %||% 0) > 0 && dur > (p$final_max_duration %||% Inf)) { pass_basic <- FALSE; reason <- paste(reason, "final_max_duration_failed") }
  if (safe_int(p$final_max_n_spikes %||% 0L, 0L) > 0 && n_spk > safe_int(p$final_max_n_spikes, 0L)) { pass_basic <- FALSE; reason <- paste(reason, "final_max_n_spikes_failed") }
  if (nrow(br) > safe_int(p$max_bridge_count_per_burst %||% 3L, 3L)) { pass_basic <- FALSE; reason <- paste(reason, "too_many_bridges") }
  edge_min_thr <- p$final_edge_contrast_min %||% p$contrast_min_high %||% 1.45; edge_geom_thr <- p$final_edge_contrast_geom_min %||% p$contrast_geom_high %||% 1.50; score_high_thr <- p$score_high %||% 0.65
  high <- pass_basic && is.finite(seed_edge_min) && is.finite(seed_edge_geom) && seed_sum$n_flank >= (p$contrast_min_flanks %||% 2L) && seed_edge_min >= edge_min_thr && seed_edge_geom >= edge_geom_thr && score >= score_high_thr
  possible <- pass_basic && is.finite(seed_edge_min) && is.finite(seed_edge_geom) && seed_sum$n_flank >= 1L && seed_edge_min >= (p$contrast_min_possible %||% 1.20) && seed_edge_geom >= (p$contrast_geom_possible %||% 1.30) && score >= (p$score_possible %||% 0.35)
  # local-compression burst mode. This admits short micro-bursts in high-rate trains
  # where pre/post ISIs are not long in absolute terms but are still locally several-fold larger
  # than the compressed core. The size cap protects high-frequency tonic from being called burst.
  seed_core_q_pct <- isi_percentile_scalar(seed_core_q, dat$ISI_sec, min_isi_sec = min_isi_sec, isi_cache = attr(dat, "isi_cache"))
  loc_ref_vals <- if ("local_median_ISI_sec" %in% names(dat)) suppressWarnings(as.numeric(dat$local_median_ISI_sec[s_isi:e_isi])) else numeric(0)
  loc_ref <- safe_median(loc_ref_vals[is.finite(loc_ref_vals) & loc_ref_vals > 0], default = NA_real_)
  local_median_core_ratio <- if (is.finite(loc_ref) && is.finite(seed_core_q) && seed_core_q > 0) loc_ref / seed_core_q else NA_real_
  lc_max_spk <- safe_int(p$local_compression_max_n_spikes %||% 8L, 8L)
  lc_max_dur <- suppressWarnings(as.numeric(p$local_compression_max_duration %||% 0))
  lc_size_ok <- (lc_max_spk <= 0L || n_spk <= lc_max_spk) && (!is.finite(lc_max_dur) || lc_max_dur <= 0 || dur <= lc_max_dur)
  lc_pct_ok <- !train_percentile_reliable(dat, p, min_isi_sec) || (is.finite(seed_core_q_pct) && seed_core_q_pct <= (p$local_compression_core_pct_max %||% 30))
  lc_local_ok <- is.finite(local_median_core_ratio) && local_median_core_ratio >= (p$local_compression_local_ratio_min %||% 2.20)
  lc_edge_ok <- is.finite(seed_edge_min) && is.finite(seed_edge_geom) && seed_edge_min >= (p$local_compression_edge_min %||% 1.80) && seed_edge_geom >= (p$local_compression_edge_geom %||% 2.50)
  local_compression_burst <- isTRUE(p$local_compression_burst_mode %||% TRUE) && pass_basic && lc_size_ok && lc_pct_ok && lc_local_ok && lc_edge_ok
  boundary_burst <- FALSE
  if (isTRUE(p$boundary_burst_mode %||% FALSE) && pass_basic && (s_isi <= 2L || e_isi >= n)) {
    b_pct_ok <- !train_percentile_reliable(dat, p, min_isi_sec) || (is.finite(seed_core_q_pct) && seed_core_q_pct <= (p$boundary_core_pct_max %||% 30))
    b_ratio_ok <- seed_sum$n_flank >= 1L && is.finite(seed_edge_geom) && seed_edge_geom >= (p$boundary_one_flank_ratio_min %||% 2.50)
    b_local_ok <- !is.finite(local_median_core_ratio) || local_median_core_ratio >= (p$boundary_local_ratio_min %||% 2.20)
    b_max_spk <- safe_int(p$boundary_max_n_spikes %||% 8L, 8L)
    b_max_dur <- suppressWarnings(as.numeric(p$boundary_max_duration %||% 0))
    b_size_ok <- (b_max_spk <= 0L || n_spk <= b_max_spk) && (!is.finite(b_max_dur) || b_max_dur <= 0 || dur <= b_max_dur)
    boundary_burst <- b_pct_ok && b_ratio_ok && b_local_ok && b_size_ok && score >= (p$boundary_score_possible %||% p$score_possible %||% 0.25)
  }
  cls <- if (high) "burst" else if (possible) "possible_burst" else if (local_compression_burst) (p$local_compression_candidate_class %||% p$local_compression_burst_label %||% "possible_burst") else if (boundary_burst) (p$boundary_burst_label %||% "possible_burst") else "reject"
  if (local_compression_burst && !high && !possible) {
    reason <- paste(reason, "local_compression_burst_mode")
    score <- score + 0.15
  }
  if (boundary_burst && !high && !possible && !local_compression_burst) {
    reason <- paste(reason, "boundary_one_sided_possible_burst")
    score <- score + 0.05
  }
  # classify event-like larger burst candidates as long_burst before
  # high-frequency modes are considered. Long burst still requires classic edge
  # contrast and bounded spike count/duration; sustained high-rate states should
  # fall through to high_frequency_tonic/spiking instead.
  long_burst_candidate <- FALSE
  long_short_fraction <- NA_real_
  if (isTRUE(p$long_burst_enable %||% FALSE) && cls == "burst") {
    lb_min_spk <- safe_int(p$long_burst_min_spikes %||% 11L, 11L)
    lb_max_spk <- safe_int(p$long_burst_max_spikes %||% 0L, 0L)
    lb_min_dur <- suppressWarnings(as.numeric(p$long_burst_min_duration %||% 0))
    lb_max_dur <- suppressWarnings(as.numeric(p$long_burst_max_duration %||% 0))
    lb_edge_min <- suppressWarnings(as.numeric(p$long_burst_edge_contrast_min %||% edge_min_thr))
    lb_edge_geom <- suppressWarnings(as.numeric(p$long_burst_edge_contrast_geom %||% edge_geom_thr))
    lb_pct <- suppressWarnings(as.numeric(p$long_burst_core_pct_max %||% 35))
    lb_frac_min <- suppressWarnings(as.numeric(p$long_burst_short_fraction_min %||% 0.65))
    core_pct_vec <- suppressWarnings(as.numeric(dat$ISI_pct[s_isi:e_isi]))
    if (length(core_pct_vec) > 0 && any(is.finite(core_pct_vec))) {
      long_short_fraction <- mean(is.finite(core_pct_vec) & core_pct_vec <= lb_pct, na.rm = TRUE)
    } else if (is.finite(seed_core_q) && seed_core_q > 0) {
      long_short_fraction <- mean(vals <= seed_core_q * 1.50, na.rm = TRUE)
    }
    lb_size_ok <- n_spk >= lb_min_spk && (lb_max_spk <= 0L || n_spk <= lb_max_spk)
    lb_dur_ok <- (!is.finite(lb_min_dur) || lb_min_dur <= 0 || dur >= lb_min_dur) && (!is.finite(lb_max_dur) || lb_max_dur <= 0 || dur <= lb_max_dur)
    lb_edge_ok <- is.finite(seed_edge_min) && is.finite(seed_edge_geom) && seed_edge_min >= lb_edge_min && seed_edge_geom >= lb_edge_geom
    lb_short_ok <- is.finite(long_short_fraction) && long_short_fraction >= lb_frac_min
    long_burst_candidate <- lb_size_ok && lb_dur_ok && lb_edge_ok && lb_short_ok
    if (long_burst_candidate) {
      cls <- as.character(p$long_burst_output_class %||% "long_burst")
      if (!cls %in% c("long_burst", "burst")) cls <- "long_burst"
      reason <- trimws(paste(reason, "long_burst_size_and_edge_criteria"))
    } else {
      # If an otherwise accepted burst-like segment is too large/long to be a
      # biologically finite long burst, keep it out of high-confidence burst so
      # the downstream high-frequency-spiking/tonic detector can classify it.
      exceeds_long_bounds <- (lb_max_spk > 0L && n_spk > lb_max_spk) || (is.finite(lb_max_dur) && lb_max_dur > 0 && dur > lb_max_dur)
      if (n_spk >= lb_min_spk && exceeds_long_bounds) {
        cls <- "possible_burst"
        reason <- trimws(paste(reason, "exceeds_long_burst_bounds_review_or_hf_state"))
      }
    }
  }
  action <- p$final_tonic_like_action %||% "demote_to_possible"
  if (!action %in% c("off", "annotate_only", "demote_to_possible", "reject")) action <- "demote_to_possible"
  if (tonic_like_final && cls %in% c("burst", "long_burst", "possible_burst") && action != "off") {
    if (action == "annotate_only") { tonic_action <- "annotated_only"; reason <- paste(reason, "final_tonic_like_warning") }
    else if (action == "demote_to_possible") { if (cls %in% c("burst", "long_burst")) cls <- "possible_burst"; tonic_action <- "demoted_to_possible_burst"; reason <- paste(reason, "final_tonic_like_demoted") }
    else if (action == "reject") { cls <- "reject"; tonic_action <- "rejected"; reason <- paste(reason, "final_tonic_like_rejected") }
  }
  accepted <- cls %in% c("burst", "long_burst") || (cls == "possible_burst" && isTRUE(p$label_possible_burst))
  if (cls == "reject" && trimws(reason) == "") reason <- "seed_core_edge_contrast_or_score_failed"
  req_edge_min <- if (is.finite(seed_edge_min) && seed_edge_min < edge_min_thr) seed_edge_min else NA_real_; req_edge_geom <- if (is.finite(seed_edge_geom) && seed_edge_geom < edge_geom_thr) seed_edge_geom else NA_real_; req_score <- if (is.finite(score) && score < score_high_thr) score else NA_real_
  tibble(candidate_id = as.integer(candidate_id), train = train, class = cls, start_isi = as.integer(s_isi), end_isi = as.integer(e_isi), start_spike_idx = as.integer(s_isi - 1L), end_spike_idx = as.integer(e_isi), n_isi = as.integer(e_isi - s_isi + 1L), n_spikes = as.integer(n_spk), start_time_sec = dat$timestamp_sec[s_isi - 1L], end_time_sec = dat$timestamp_sec[e_isi], duration_sec = dur, seed_count = as.integer(nrow(ss)), bridge_count = as.integer(nrow(br)), seed_ids = paste(ss$seed_id, collapse = ";"), bridge_ids = paste(br$bridge_id, collapse = ";"), core_q_sec = edge$core_q, seed_core_q_sec = seed_core_q, max_ISI_sec = max(vals), mean_ISI_sec = mean(vals), MM = mm, LV = lv, CV = cv, pre_edge_ISI_sec = edge$pre, post_edge_ISI_sec = edge$post, edge_contrast_min_q = edge$contrast_min_q, edge_contrast_geom_q = edge$contrast_geom_q, edge_pre_ratio_seed_q = seed_sum$pre_ratio, edge_post_ratio_seed_q = seed_sum$post_ratio, edge_contrast_min_seed_q = seed_edge_min, edge_contrast_geom_seed_q = seed_edge_geom, score = score, accepted = accepted, reject_reason = trimws(reason), final_tonic_like = tonic_like_final, final_tonic_action = tonic_action, required_final_edge_min = req_edge_min, required_final_edge_geom = req_edge_geom, required_score_high = req_score,
         local_compression_burst = local_compression_burst, boundary_burst = boundary_burst, long_burst_candidate = long_burst_candidate, long_burst_short_fraction = long_short_fraction, local_median_core_ratio = local_median_core_ratio, seed_core_q_pct = seed_core_q_pct)
}
