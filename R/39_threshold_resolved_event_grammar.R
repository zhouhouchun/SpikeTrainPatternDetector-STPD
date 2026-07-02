
# ============================================================
# event grammar Threshold-resolved event-grammar detector
# ============================================================
# event grammar is a core algorithm reform, not a small patch to earlier engine/dataset ISI/event core
# gates.  It introduces one single threshold resolver used by both UI and
# detector:
#   user override > manual-derived structure > histogram suggestion > default.
# All detector code reads only the resolved/effective thresholds.

stpd_train_pipeline_event_grammar_core <- stpd_detect_train_event_grammar_core

stpd_threshold_pattern_names_impl <- function() {
  c("burst", "high_frequency_spiking", "high_frequency_tonic", "tonic", "pause")
}

stpd_threshold_pattern_label_impl <- function(pat) {
  switch(as.character(pat),
         burst = "\u7206\u53D1 / burst",
         high_frequency_spiking = "\u9AD8\u9891\u8FDE\u7EED\u53D1\u653E / HF spiking",
         high_frequency_tonic = "\u9AD8\u9891\u5F3A\u76F4\u53D1\u653E / HF tonic",
         tonic = "\u5F3A\u76F4\u53D1\u653E / tonic",
         pause = "\u6682\u505C / pause",
         as.character(pat))
}

stpd_threshold_pattern_color_impl <- function(pat, source = "manual") {
  pal <- tryCatch(pattern_palette("pattern_color"), error = function(e) data.frame())
  if (nrow(pal) > 0 && pat %in% pal$pattern) {
    col <- pal[pal$pattern == pat, if (identical(source, "auto")) "auto" else "manual", drop = TRUE][1]
    if (is.character(col) && nzchar(col)) return(col)
  }
  c(burst = "#FB8DB8", high_frequency_spiking = "#FF5A59", high_frequency_tonic = "#63F28E", tonic = "#CAF99D", pause = "#4BCEE6")[[pat]] %||% "#999999"
}

stpd_event_grammar_num <- function(x, default = NA_real_) {
  y <- suppressWarnings(as.numeric(x))
  if (length(y) == 0 || !is.finite(y[1])) return(default)
  y[1]
}

stpd_event_grammar_int <- function(x, default = 0L) {
  y <- suppressWarnings(as.integer(round(as.numeric(x))))
  if (length(y) == 0 || !is.finite(y[1])) return(as.integer(default))
  as.integer(y[1])
}

stpd_event_grammar_q <- function(x, p, default = NA_real_) {
  x <- suppressWarnings(as.numeric(x)); x <- x[is.finite(x)]
  if (length(x) == 0) return(default)
  suppressWarnings(as.numeric(stats::quantile(x, p, na.rm = TRUE, names = FALSE, type = 7)))
}

stpd_event_grammar_safe_seq <- function(a, b) {
  a <- as.integer(a); b <- as.integer(b)
  if (!is.finite(a) || !is.finite(b) || b < a) return(integer())
  a:b
}

stpd_event_grammar_bool_runs <- function(flag) {
  flag <- as.logical(flag); flag[is.na(flag)] <- FALSE
  if (length(flag) == 0 || !any(flag)) return(data.frame(start_isi = integer(), end_isi = integer()))
  d <- diff(c(FALSE, flag, FALSE))
  data.frame(start_isi = which(d == 1), end_isi = which(d == -1) - 1L)
}

stpd_event_grammar_valid_isis_by_train <- function(trains, min_isi_sec = 0.001) {
  if (is.null(trains) || length(trains) == 0) return(list())
  out <- list()
  for (nm in names(trains)) {
    dat <- trains[[nm]]
    if (is.null(dat) || !("ISI_sec" %in% names(dat))) next
    isi <- suppressWarnings(as.numeric(dat$ISI_sec))
    ok <- is.finite(isi) & isi >= min_isi_sec
    if (length(ok) > 0) ok[1] <- FALSE
    v <- isi[ok]
    if (length(v) > 0) out[[nm]] <- v
  }
  out
}

stpd_event_grammar_histogram_suggest <- function(vals_by_train, min_isi_sec = 0.001, bin_width_sec = 0.005) {
  vals <- unlist(vals_by_train, use.names = FALSE)
  vals <- suppressWarnings(as.numeric(vals))
  vals <- vals[is.finite(vals) & vals >= min_isi_sec]
  if (length(vals) < 5) {
    burst_hi <- 0.010
    return(list(
      burst = list(seed_lower_sec = max(min_isi_sec, 0.001), seed_upper_sec = burst_hi, bridge_upper_sec = 0.015, contrast_S = 2.5),
      high_frequency_spiking = list(seed_lower_sec = max(min_isi_sec, 0.001), seed_upper_sec = 0.020, bridge_upper_sec = 0.030),
      high_frequency_tonic = list(seed_lower_sec = 0.010, seed_upper_sec = 0.030, bridge_upper_sec = 0.035),
      tonic = list(seed_lower_sec = 0.020, seed_upper_sec = 0.060, bridge_upper_sec = 0.080),
      pause = list(seed_lower_sec = 0.100, seed_upper_sec = 0.250, bridge_upper_sec = 0.250)
    ))
  }
  bin_width_sec <- stpd_event_grammar_num(bin_width_sec, 0.005)
  if (!is.finite(bin_width_sec) || bin_width_sec <= 0) bin_width_sec <- 0.005
  q005 <- stpd_event_grammar_q(vals, 0.005, min_isi_sec)
  q10 <- stpd_event_grammar_q(vals, 0.10, 0.010)
  q15 <- stpd_event_grammar_q(vals, 0.15, q10)
  q20 <- stpd_event_grammar_q(vals, 0.20, q15)
  q25 <- stpd_event_grammar_q(vals, 0.25, 0.030)
  q50 <- stpd_event_grammar_q(vals, 0.50, 0.060)
  q75 <- stpd_event_grammar_q(vals, 0.75, 0.100)
  q90 <- stpd_event_grammar_q(vals, 0.90, 0.150)
  q95 <- stpd_event_grammar_q(vals, 0.95, 0.250)
  min_obs <- suppressWarnings(min(vals, na.rm = TRUE))
  slow_structural_tail <- is.finite(min_obs) && min_obs > 0.015 &&
    is.finite(q50) && q50 > 0 && min_obs <= q50 * 0.45
  low_xmax <- min(max(stpd_event_grammar_q(vals, 0.35, 0.050), 0.030), 0.080)
  low <- vals[vals <= low_xmax]
  burst_hi <- NA_real_
  if (length(low) >= 8) {
    br <- seq(0, ceiling(low_xmax / bin_width_sec) * bin_width_sec + bin_width_sec, by = bin_width_sec)
    if (length(br) >= 4) {
      hh <- hist(low, breaks = br, plot = FALSE, include.lowest = TRUE, right = FALSE)
      cnt <- as.numeric(hh$counts); mids <- (hh$breaks[-1] + hh$breaks[-length(hh$breaks)]) / 2
      search_max <- min(low_xmax, max(0.040, q15, min_obs * 1.10, na.rm = TRUE))
      search <- which(mids >= min_isi_sec & mids <= search_max)
      if (length(search) > 0 && max(cnt[search], na.rm = TRUE) > 0) {
        pk <- search[which.max(cnt[search])]
        after <- seq(pk + 1L, min(length(cnt), pk + 10L))
        if (length(after) > 0) {
          # First local valley after the low-ISI peak.  If no formal valley exists,
          # use the first bin whose height drops below 55% of the peak.
          local_min <- integer()
          for (ii in after) {
            if (ii > 1 && ii < length(cnt) && cnt[ii] <= cnt[ii-1] && cnt[ii] <= cnt[ii+1]) { local_min <- ii; break }
          }
          if (length(local_min) == 0) {
            drop <- after[cnt[after] <= 0.55 * cnt[pk]]
            if (length(drop) > 0) local_min <- drop[1]
          }
          if (length(local_min) > 0) burst_hi <- hh$breaks[local_min + 1L]
        }
      }
    }
  }
  if (!is.finite(burst_hi) || burst_hi <= min_isi_sec) {
    if (slow_structural_tail) {
      # Slow-train structural fallback: when the whole dataset lies outside the
      # classical 1-15 ms burst seed prior, use the observed low tail as the
      # seed-entry band and let flank contrast/bridge rules decide eventness.
      burst_hi <- max(q10, q15, min_obs * 1.05, min_obs, na.rm = TRUE)
      if (is.finite(q25)) burst_hi <- min(burst_hi, max(q25, min_obs, na.rm = TRUE))
    } else {
      # Classical fallback: use the low-tail quantile but avoid very broad seed bands.
      burst_hi <- min(max(q10, 0.006), 0.015)
    }
  }
  if (slow_structural_tail && is.finite(min_obs) && burst_hi < min_obs) burst_hi <- min_obs
  burst_lo <- max(min_isi_sec, min(q005, burst_hi * 0.25, na.rm = TRUE))
  burst_lo <- max(min_isi_sec, burst_lo)
  bridge_hi <- max(burst_hi * 1.5, burst_hi + bin_width_sec)
  if (slow_structural_tail) {
    bridge_cap <- max(burst_hi * 1.8, q20, min(q25, q50 * 0.85, na.rm = TRUE), burst_hi, na.rm = TRUE)
    bridge_hi <- min(max(bridge_hi, burst_hi), bridge_cap)
    if (!is.finite(bridge_hi) || bridge_hi <= burst_hi) bridge_hi <- max(burst_hi * 1.25, burst_hi + bin_width_sec)
  } else {
    bridge_hi <- min(max(bridge_hi, burst_hi), 0.050)
  }
  list(
    burst = list(seed_lower_sec = burst_lo, seed_upper_sec = burst_hi, bridge_upper_sec = bridge_hi, contrast_S = 2.5),
    high_frequency_spiking = list(seed_lower_sec = burst_lo, seed_upper_sec = max(bridge_hi, burst_hi), bridge_upper_sec = max(bridge_hi * 2, 0.030)),
    high_frequency_tonic = list(seed_lower_sec = max(burst_hi, min(q25, 0.030, na.rm = TRUE)), seed_upper_sec = max(q25, bridge_hi, 0.020, na.rm = TRUE), bridge_upper_sec = max(q25, bridge_hi * 1.3, 0.030, na.rm = TRUE)),
    tonic = list(seed_lower_sec = max(q25, bridge_hi, 0.020, na.rm = TRUE), seed_upper_sec = max(q75, q50, 0.050, na.rm = TRUE), bridge_upper_sec = max(q90, q75, 0.080, na.rm = TRUE)),
    pause = list(seed_lower_sec = max(q90, 0.080, na.rm = TRUE), seed_upper_sec = max(q95, q90, 0.150, na.rm = TRUE), bridge_upper_sec = max(q95, q90, 0.150, na.rm = TRUE))
  )
}

stpd_event_grammar_manual_events_one_train <- function(dat, train = "", min_isi_sec = 0.001) {
  if (is.null(dat) || !("ISI_sec" %in% names(dat)) || !("pattern_manual" %in% names(dat))) return(data.frame())
  isi <- suppressWarnings(as.numeric(dat$ISI_sec))
  lab <- as.character(dat$pattern_manual); lab[is.na(lab)] <- ""
  patterns <- c("burst", "long_burst", "possible_burst", "high_frequency_spiking", "high_frequency_tonic", "tonic", "pause")
  rows <- list(); rr <- 0L
  for (pat in patterns) {
    flag <- lab == pat & is.finite(isi) & isi >= min_isi_sec
    if (length(flag) > 0) flag[1] <- FALSE
    runs <- stpd_event_grammar_bool_runs(flag)
    if (nrow(runs) == 0) next
    for (ii in seq_len(nrow(runs))) {
      s <- as.integer(runs$start_isi[ii]); e <- as.integer(runs$end_isi[ii]); idx <- stpd_event_grammar_safe_seq(s, e)
      vals <- isi[idx]; vals <- vals[is.finite(vals) & vals >= min_isi_sec]
      if (length(vals) == 0) next
      pre <- if (s > 1) isi[s - 1L] else NA_real_
      post <- if (e + 1L <= length(isi)) isi[e + 1L] else NA_real_
      q90 <- stpd_event_grammar_q(vals, 0.90); q95 <- stpd_event_grammar_q(vals, 0.95)
      pre_ratio <- if (is.finite(pre) && is.finite(q90) && q90 > 0) pre / q90 else NA_real_
      post_ratio <- if (is.finite(post) && is.finite(q90) && q90 > 0) post / q90 else NA_real_
      rr <- rr + 1L
      rows[[rr]] <- data.frame(
        train = train, pattern = pat, start_isi = s, end_isi = e,
        n_isi = length(vals), n_spikes = e - s + 2L,
        intra_q05_sec = stpd_event_grammar_q(vals, 0.05), intra_q10_sec = stpd_event_grammar_q(vals, 0.10),
        intra_q40_sec = stpd_event_grammar_q(vals, 0.40), intra_q90_sec = q90, intra_q95_sec = q95,
        intra_max_sec = max(vals, na.rm = TRUE), pre_gap_sec = pre, post_gap_sec = post,
        pre_ratio_q90 = pre_ratio, post_ratio_q90 = post_ratio,
        min_flank_ratio_q90 = if (is.finite(pre_ratio) && is.finite(post_ratio)) min(pre_ratio, post_ratio) else NA_real_,
        max_flank_ratio_q90 = if (is.finite(pre_ratio) || is.finite(post_ratio)) max(pre_ratio, post_ratio, na.rm = TRUE) else NA_real_,
        boundary_type = if (is.finite(pre_ratio) && is.finite(post_ratio)) "two_sided_observed" else if (is.finite(pre_ratio)) "pre_only_observed" else if (is.finite(post_ratio)) "post_only_observed" else "unobserved",
        stringsAsFactors = FALSE)
    }
  }
  if (length(rows) == 0) data.frame() else do.call(rbind, rows)
}

stpd_event_grammar_manual_event_table <- function(trains, min_isi_sec = 0.001) {
  if (is.null(trains) || length(trains) == 0) return(data.frame())
  rows <- list()
  for (nm in names(trains)) {
    d <- stpd_event_grammar_manual_events_one_train(trains[[nm]], train = nm, min_isi_sec = min_isi_sec)
    if (nrow(d) > 0) rows[[length(rows) + 1L]] <- d
  }
  if (length(rows) == 0) data.frame() else do.call(rbind, rows)
}

stpd_event_grammar_manual_suggest <- function(manual_events) {
  empty <- function() list(seed_lower_sec = NA_real_, seed_upper_sec = NA_real_, bridge_upper_sec = NA_real_, contrast_S = NA_real_)
  out <- setNames(vector("list", length(stpd_threshold_pattern_names_impl())), stpd_threshold_pattern_names_impl())
  for (p in names(out)) out[[p]] <- empty()
  if (is.null(manual_events) || nrow(manual_events) == 0) return(out)
  burst_ev <- manual_events[manual_events$pattern %in% c("burst", "long_burst", "possible_burst"), , drop = FALSE]
  if (nrow(burst_ev) > 0) {
    vals <- c(burst_ev$intra_q05_sec, burst_ev$intra_q10_sec, burst_ev$intra_q40_sec, burst_ev$intra_q90_sec, burst_ev$intra_q95_sec)
    out$burst$seed_lower_sec <- max(stpd_event_grammar_q(burst_ev$intra_q05_sec, 0.25, NA_real_), 0, na.rm = TRUE)
    out$burst$seed_upper_sec <- stpd_event_grammar_q(burst_ev$intra_q90_sec, 0.50, NA_real_)
    out$burst$bridge_upper_sec <- max(stpd_event_grammar_q(burst_ev$intra_q95_sec, 0.75, NA_real_), out$burst$seed_upper_sec, na.rm = TRUE)
    svals <- burst_ev$min_flank_ratio_q90[is.finite(burst_ev$min_flank_ratio_q90)]
    out$burst$contrast_S <- if (length(svals) > 0) max(1.2, stpd_event_grammar_q(svals, 0.25, 2.5)) else NA_real_
  }
  derive_pat <- function(pat, lowq = 0.10, highq = 0.90, bridgeq = 0.95) {
    ev <- manual_events[manual_events$pattern == pat, , drop = FALSE]
    if (nrow(ev) == 0) return(empty())
    list(seed_lower_sec = stpd_event_grammar_q(ev$intra_q10_sec, 0.50, NA_real_),
         seed_upper_sec = stpd_event_grammar_q(ev$intra_q90_sec, 0.50, NA_real_),
         bridge_upper_sec = stpd_event_grammar_q(ev$intra_q95_sec, 0.75, NA_real_),
         contrast_S = NA_real_)
  }
  out$high_frequency_spiking <- derive_pat("high_frequency_spiking")
  out$high_frequency_tonic <- derive_pat("high_frequency_tonic")
  out$tonic <- derive_pat("tonic")
  out$pause <- derive_pat("pause")
  out
}

stpd_event_grammar_user_suggest <- function(params) {
  eg <- params$event_grammar %||% list()
  usr <- eg$user %||% list()
  out <- list()
  for (pat in stpd_threshold_pattern_names_impl()) {
    u <- usr[[pat]] %||% list()
    if (!isTRUE(u$enable %||% FALSE)) {
      out[[pat]] <- list(seed_lower_sec = NA_real_, seed_upper_sec = NA_real_, bridge_upper_sec = NA_real_, contrast_S = NA_real_)
    } else {
      out[[pat]] <- list(
        seed_lower_sec = stpd_event_grammar_num(u$seed_lower_sec, NA_real_),
        seed_upper_sec = stpd_event_grammar_num(u$seed_upper_sec, NA_real_),
        bridge_upper_sec = stpd_event_grammar_num(u$bridge_upper_sec, NA_real_),
        contrast_S = stpd_event_grammar_num(u$contrast_S, NA_real_)
      )
    }
  }
  out
}

stpd_event_grammar_default_suggest <- function(params) {
  ec <- params$event_core %||% list()
  bp <- params$burst %||% list(); hp <- params$highfreq %||% list(); tp <- params$tonic %||% list(); pp <- params$pause %||% list()
  list(
    burst = list(seed_lower_sec = stpd_event_grammar_num(ec$seed_band_lower_sec %||% 0.001, 0.001), seed_upper_sec = stpd_event_grammar_num(ec$seed_band_upper_sec %||% 0.010, 0.010), bridge_upper_sec = stpd_event_grammar_num(ec$bridge_band_upper_sec %||% 0.015, 0.015), contrast_S = stpd_event_grammar_num(ec$burst_contrast_min %||% 2.5, 2.5)),
    high_frequency_spiking = list(seed_lower_sec = stpd_event_grammar_num(ec$seed_band_lower_sec %||% 0.001, 0.001), seed_upper_sec = stpd_event_grammar_num(hp$spiking_q90_max_ISI_sec %||% 0.020, 0.020), bridge_upper_sec = stpd_event_grammar_num(hp$spiking_epoch_bridge_ISI_sec %||% 0.030, 0.030), contrast_S = NA_real_),
    high_frequency_tonic = list(seed_lower_sec = stpd_event_grammar_num(hp$tonic_min_ISI_floor_sec %||% 0.010, 0.010), seed_upper_sec = stpd_event_grammar_num(hp$T_high_max %||% 0.020, 0.020), bridge_upper_sec = stpd_event_grammar_num(hp$T_high_max %||% 0.020, 0.020) * 1.25, contrast_S = NA_real_),
    tonic = list(seed_lower_sec = stpd_event_grammar_num(tp$T_min %||% 0.020, 0.020), seed_upper_sec = stpd_event_grammar_num(tp$T_max %||% 0.060, 0.060), bridge_upper_sec = stpd_event_grammar_num(tp$T_max %||% 0.060, 0.060) * 1.25, contrast_S = NA_real_),
    pause = list(seed_lower_sec = stpd_event_grammar_num(pp$T_seed %||% 0.100, 0.100), seed_upper_sec = stpd_event_grammar_num(pp$T_strong %||% 0.150, 0.150), bridge_upper_sec = stpd_event_grammar_num(pp$T_strong %||% 0.150, 0.150), contrast_S = NA_real_)
  )
}

stpd_event_grammar_choose <- function(field, pat, mode, user, manual, hist, def) {
  candidates <- switch(as.character(mode %||% "auto"),
                       user = c("user", "manual", "histogram", "default"),
                       manual = c("manual", "user", "histogram", "default"),
                       histogram = c("histogram", "manual", "user", "default"),
                       default = c("default", "user", "manual", "histogram"),
                       c("user", "manual", "histogram", "default"))
  values <- list(user = user[[pat]][[field]], manual = manual[[pat]][[field]], histogram = hist[[pat]][[field]], default = def[[pat]][[field]])
  for (src in candidates) {
    val <- stpd_event_grammar_num(values[[src]], NA_real_)
    if (is.finite(val) && (field == "contrast_S" || val > 0)) return(list(value = val, source = src))
  }
  list(value = NA_real_, source = "none")
}

stpd_resolve_thresholds_for_dataset_impl <- function(trains, params, min_isi_sec = 0.001, bin_width_sec = 0.005) {
  eg <- params$event_grammar %||% list()
  vals_by_train <- stpd_event_grammar_valid_isis_by_train(trains, min_isi_sec = min_isi_sec)
  manual_events <- stpd_event_grammar_manual_event_table(trains, min_isi_sec = min_isi_sec)
  user <- stpd_event_grammar_user_suggest(params)
  manual <- stpd_event_grammar_manual_suggest(manual_events)
  hist <- stpd_event_grammar_histogram_suggest(vals_by_train, min_isi_sec = min_isi_sec, bin_width_sec = bin_width_sec)
  def <- stpd_event_grammar_default_suggest(params)
  mode <- as.character(eg$threshold_source_mode %||% "auto")
  rows <- list(); eff <- list(); k <- 0L
  for (pat in stpd_threshold_pattern_names_impl()) {
    eff[[pat]] <- list()
    for (field in c("seed_lower_sec", "seed_upper_sec", "bridge_upper_sec", "contrast_S")) {
      ch <- stpd_event_grammar_choose(field, pat, mode, user, manual, hist, def)
      # Structural safeguards and monotonicity are applied after all fields are chosen.
      k <- k + 1L
      rows[[k]] <- data.frame(pattern = pat, pattern_label = stpd_threshold_pattern_label_impl(pat), field = field,
                              user_sec = stpd_event_grammar_num(user[[pat]][[field]], NA_real_),
                              manual_sec = stpd_event_grammar_num(manual[[pat]][[field]], NA_real_),
                              histogram_sec = stpd_event_grammar_num(hist[[pat]][[field]], NA_real_),
                              default_sec = stpd_event_grammar_num(def[[pat]][[field]], NA_real_),
                              effective_sec = stpd_event_grammar_num(ch$value, NA_real_),
                              source = ch$source, stringsAsFactors = FALSE)
      eff[[pat]][[field]] <- stpd_event_grammar_num(ch$value, NA_real_)
      eff[[pat]][[paste0(field, "_source")]] <- ch$source
    }
  }
  tab <- do.call(rbind, rows)
  # Enforce valid band geometry per pattern.
  for (pat in names(eff)) {
    lo <- stpd_event_grammar_num(eff[[pat]]$seed_lower_sec, NA_real_); hi <- stpd_event_grammar_num(eff[[pat]]$seed_upper_sec, NA_real_); br <- stpd_event_grammar_num(eff[[pat]]$bridge_upper_sec, NA_real_)
    if (!is.finite(lo) || lo < 0) lo <- min_isi_sec
    if (!is.finite(hi) || hi <= lo) hi <- max(lo + min_isi_sec, stpd_event_grammar_num(def[[pat]]$seed_upper_sec, lo + min_isi_sec))
    if (!is.finite(br) || br < hi) br <- hi
    eff[[pat]]$seed_lower_sec <- lo; eff[[pat]]$seed_upper_sec <- hi; eff[[pat]]$bridge_upper_sec <- br
    tab$effective_sec[tab$pattern == pat & tab$field == "seed_lower_sec"] <- lo
    tab$effective_sec[tab$pattern == pat & tab$field == "seed_upper_sec"] <- hi
    tab$effective_sec[tab$pattern == pat & tab$field == "bridge_upper_sec"] <- br
  }
  # HF tonic must not be dominated by the extreme burst-core band.  Its floor is
  # never lower than burst seed upper unless the user explicitly overrides HF tonic.
  hft_user <- isTRUE(((eg$user %||% list())$high_frequency_tonic %||% list())$enable %||% FALSE)
  if (!hft_user && is.finite(eff$burst$seed_upper_sec)) {
    eff$high_frequency_tonic$seed_lower_sec <- max(eff$high_frequency_tonic$seed_lower_sec, eff$burst$seed_upper_sec)
    if (eff$high_frequency_tonic$seed_upper_sec <= eff$high_frequency_tonic$seed_lower_sec) {
      eff$high_frequency_tonic$seed_upper_sec <- max(eff$high_frequency_tonic$seed_lower_sec * 1.5, eff$high_frequency_tonic$seed_lower_sec + min_isi_sec)
    }
    if (eff$high_frequency_tonic$bridge_upper_sec < eff$high_frequency_tonic$seed_upper_sec) eff$high_frequency_tonic$bridge_upper_sec <- eff$high_frequency_tonic$seed_upper_sec
    for (fld in c("seed_lower_sec", "seed_upper_sec", "bridge_upper_sec")) tab$effective_sec[tab$pattern == "high_frequency_tonic" & tab$field == fld] <- eff$high_frequency_tonic[[fld]]
  }
  list(threshold_table = tab, effective_bands = eff, manual_events = manual_events, manual_suggest = manual, histogram_suggest = hist, valid_isis_by_train = vals_by_train)
}

stpd_attach_thresholds_to_params_impl <- function(params, ds = NULL, min_isi_sec = NULL, bin_width_sec = NULL) {
  if (is.null(params$event_grammar)) params$event_grammar <- list()
  if (!isTRUE(params$event_grammar$enabled %||% (params$event_core %||% list())$enabled %||% TRUE)) return(params)
  if (is.null(min_isi_sec)) min_isi_sec <- params$detector$min_valid_isi_sec %||% 0.001
  if (is.null(bin_width_sec)) bin_width_sec <- params$event_grammar$histogram_bin_width_sec %||% (params$event_core %||% list())$histogram_bin_width_sec %||% 0.005
  trains <- if (!is.null(ds) && !is.null(ds$trains)) ds$trains else list()
  resolved <- stpd_resolve_thresholds_for_dataset_impl(trains, params, min_isi_sec = min_isi_sec, bin_width_sec = bin_width_sec)
  params$event_grammar$threshold_table <- resolved$threshold_table
  params$event_grammar$effective_bands <- resolved$effective_bands
  params$event_grammar$manual_event_table <- resolved$manual_events
  params$event_grammar$manual_suggest <- resolved$manual_suggest
  params$event_grammar$histogram_suggest <- resolved$histogram_suggest
  params
}

stpd_event_grammar_params_impl <- function(dat, params, min_isi_sec = 0.001, train = "") {
  if (is.null((params$event_grammar %||% list())$effective_bands)) params <- stpd_attach_thresholds_to_params_impl(params, ds = list(trains = list(current = dat)), min_isi_sec = min_isi_sec)
  vp <- stpd_event_core_params_impl(dat, params, min_isi_sec)
  eg <- params$event_grammar %||% list()
  b <- eg$effective_bands %||% list()
  if (!is.null(b$burst)) {
    vp$seed_low <- b$burst$seed_lower_sec
    vp$seed_high <- b$burst$seed_upper_sec
    vp$bridge_high <- b$burst$bridge_upper_sec
    if (is.finite(b$burst$contrast_S)) vp$S <- b$burst$contrast_S
  }
  if (!is.null(b$high_frequency_spiking)) {
    vp$hf_spiking_q90_max <- b$high_frequency_spiking$seed_upper_sec
    vp$hf_spiking_epoch_bridge <- b$high_frequency_spiking$bridge_upper_sec
    vp$hf_spiking_break_isi <- max(vp$hf_spiking_break_isi, b$high_frequency_spiking$bridge_upper_sec, na.rm = TRUE)
  }
  if (!is.null(b$high_frequency_tonic)) {
    vp$hf_tonic_floor <- b$high_frequency_tonic$seed_lower_sec
    vp$hf_tonic_high_max <- b$high_frequency_tonic$seed_upper_sec
  }
  if (!is.null(b$tonic)) {
    vp$tonic_min <- b$tonic$seed_lower_sec
    vp$tonic_max <- b$tonic$seed_upper_sec
  }
  if (!is.null(b$pause)) {
    vp$pause_thr <- b$pause$seed_lower_sec
  }
  vp <- stpd_apply_train_isi_thresholds_to_event_vp(vp, params, train = train, min_isi_sec = min_isi_sec)
  vp$tonic_burst_overlap_ref <- suppressWarnings(max(c(
    stpd_event_grammar_num(vp$tonic_burst_overlap_ref, NA_real_),
    stpd_event_grammar_num(vp$seed_high, NA_real_),
    stpd_event_grammar_num(vp$bridge_high, NA_real_)
  ), na.rm = TRUE))
  vp$threshold_table <- eg$threshold_table
  vp
}

stpd_event_grammar_detect_burst_events_threshold_resolved_base <- function(dat, params, vp, min_isi_sec = 0.001, train = "") {
  n <- nrow(dat); rows <- list()
  if (n <= 2) return(data.frame())
  isi <- suppressWarnings(as.numeric(dat$ISI_sec))
  art <- is_artifact_isi(isi, min_isi_sec)
  valid <- is.finite(isi) & !art
  if (length(valid) > 0) valid[1] <- FALSE
  seed_flag <- valid & isi >= vp$seed_low & isi <= vp$seed_high
  runs <- stpd_event_grammar_bool_runs(seed_flag)
  if (nrow(runs) == 0) return(data.frame())
  seen <- list(); counter <- 0L
  eg <- params$event_grammar %||% list()
  one_sided_as_canonical <- isTRUE(eg$allow_one_sided_burst_as_canonical %||% FALSE)
  one_sided_S <- stpd_event_grammar_num(eg$one_sided_burst_contrast_min %||% (vp$S + 0.5), vp$S + 0.5)
  for (rr in seq_len(nrow(runs))) {
    ss <- as.integer(runs$start_isi[rr]); ee <- as.integer(runs$end_isi[rr])
    if (sum(seed_flag[ss:ee], na.rm = TRUE) < vp$min_seed_isi_n) next
    lefts <- stpd_event_core_left_extensions(ss, isi, valid, vp$bridge_high, vp$max_expand)
    rights <- stpd_event_core_right_extensions(ee, n, isi, valid, vp$bridge_high, vp$max_expand)
    for (s in lefts) for (e in rights) {
      if (length(rows) >= vp$max_candidates) break
      key <- paste0(s, "_", e); if (!is.null(seen[[key]])) next; seen[[key]] <- TRUE
      idx <- stpd_event_grammar_safe_seq(s, e); if (length(idx) == 0) next
      core_n <- sum(seed_flag[idx], na.rm = TRUE); if (core_n < vp$min_seed_isi_n) next
      n_spikes <- e - s + 2L; if (n_spikes < vp$min_spikes) next
      m <- stpd_event_core_span_metrics(dat, s, e, params, vp, min_isi_sec, train, "event_grammar_burst_event")
      if (is.null(m)) next
      bridge_count_pass <- is.finite(m$bridge_isi_count[1]) && m$bridge_isi_count[1] <= vp$max_bridge_n
      bridge_fraction_pass <- is.finite(m$bridge_fraction[1]) && m$bridge_fraction[1] <= vp$max_bridge_frac
      q95_pass <- is.finite(m$intra_q95_sec[1]) && m$intra_q95_sec[1] <= vp$bridge_high
      q90_pass <- is.finite(m$intra_q90_sec[1]) && m$intra_q90_sec[1] <= vp$bridge_high
      pre_ratio <- if (is.finite(m$pre_gap_sec[1]) && is.finite(m$intra_q90_sec[1]) && m$intra_q90_sec[1] > 0) m$pre_gap_sec[1] / m$intra_q90_sec[1] else NA_real_
      post_ratio <- if (is.finite(m$post_gap_sec[1]) && is.finite(m$intra_q90_sec[1]) && m$intra_q90_sec[1] > 0) m$post_gap_sec[1] / m$intra_q90_sec[1] else NA_real_
      two_sided <- is.finite(pre_ratio) && is.finite(post_ratio) && pre_ratio >= vp$S && post_ratio >= vp$S
      two_possible <- is.finite(pre_ratio) && is.finite(post_ratio) && pre_ratio >= vp$S_possible && post_ratio >= vp$S_possible
      one_sided <- (is.finite(pre_ratio) && pre_ratio >= one_sided_S) || (is.finite(post_ratio) && post_ratio >= one_sided_S)
      edge_limited <- (!is.finite(pre_ratio) || !is.finite(post_ratio)) && one_sided
      neg <- isTRUE(m$manual_negative_veto[1])
      size_label <- "prolonged_burst_like"
      if (n_spikes <= vp$classic_max_spikes) size_label <- "burst"
      else if (n_spikes >= vp$long_min_spikes && (vp$long_max_spikes <= 0 || n_spikes <= vp$long_max_spikes)) size_label <- "long_burst"
      final <- "reject"; status <- "event_grammar_reject"; action <- "reject"; decision <- "event_grammar_reject"; priority <- 0
      if (!neg && bridge_count_pass && bridge_fraction_pass && q90_pass && q95_pass && two_sided) {
        if (size_label %in% c("burst", "long_burst")) {
          final <- size_label; status <- "event_grammar_two_sided_burst_event_pass"; action <- "accept"; decision <- paste0("event_grammar_two_sided_event_grammar_pass__", size_label); priority <- if (final == "burst") 1200 else 1120
        } else { final <- "possible_burst"; status <- "event_grammar_prolonged_burst_like_review"; action <- "demote_to_possible"; decision <- "two_sided_structure_but_spike_count_exceeds_long_burst_range"; priority <- 150 }
      } else if (!neg && bridge_count_pass && bridge_fraction_pass && q90_pass && q95_pass && one_sided) {
        if (one_sided_as_canonical && size_label %in% c("burst", "long_burst")) {
          final <- size_label; status <- "event_grammar_one_sided_burst_event_pass_user_allowed"; action <- "accept"; decision <- paste0("event_grammar_one_sided_event_grammar_pass__", size_label); priority <- if (final == "burst") 950 else 900
        } else {
          final <- "possible_burst"; status <- if (edge_limited) "event_grammar_edge_limited_possible_burst" else "event_grammar_one_sided_possible_burst"; action <- "demote_to_possible"; decision <- "one_sided_flank_contrast_pass_q95_core_pass"; priority <- 160
        }
      } else if (!neg && bridge_count_pass && bridge_fraction_pass && q90_pass && two_possible) {
        final <- "possible_burst"; status <- "event_grammar_possible_two_sided_burst"; action <- "demote_to_possible"; decision <- "two_sided_possible_contrast_pass"; priority <- 140
      } else {
        reasons <- c(if (neg) "manual_negative_veto", if (!bridge_count_pass) "too_many_bridge_isis", if (!bridge_fraction_pass) "bridge_fraction_too_high", if (!q90_pass) "intra_q90_exceeds_bridge_band", if (!q95_pass) "intra_q95_exceeds_bridge_band", if (!two_sided && !one_sided && !two_possible) "flank_contrast_fail")
        decision <- paste(reasons, collapse = ";"); if (!nzchar(decision)) decision <- "event_grammar_reject"
      }
      score <- (if (is.finite(m$burst_contrast_score[1])) m$burst_contrast_score[1] else 0) + 0.12 * m$core_isi_count[1] - 0.18 * m$bridge_isi_count[1] - 0.35 * m$bridge_fraction[1]
      counter <- counter + 1L
      rows[[length(rows) + 1L]] <- stpd_event_core_candidate_row(m, "event_grammar_burst_event", "event_grammar_seed_centered_burst", final, status, decision, action, score, priority,
        list(candidate_id = paste0("event_grammar_burst_", counter), seed_run_start_isi = ss, seed_run_end_isi = ee,
             seed_band_lower_sec = vp$seed_low, seed_band_upper_sec = vp$seed_high, bridge_band_upper_sec = vp$bridge_high,
             burst_contrast_required = vp$S, one_sided_contrast_required = one_sided_S, possible_contrast_required = vp$S_possible,
             pre_ratio_q90 = pre_ratio, post_ratio_q90 = post_ratio,
             boundary_type = if (two_sided) "two_sided" else if (one_sided) "one_sided_or_edge_limited" else "failed",
             strict_boundary_pass = two_sided, one_sided_boundary_pass = one_sided, possible_boundary_pass = two_possible,
             bridge_count_pass = bridge_count_pass, bridge_fraction_pass = bridge_fraction_pass, q90_bridge_pass = q90_pass, q95_bridge_pass = q95_pass,
             threshold_source_summary = paste0("seed=", vp$threshold_table$source[vp$threshold_table$pattern=="burst" & vp$threshold_table$field=="seed_upper_sec"][1] %||% "")))
    }
    if (length(rows) >= vp$max_candidates) break
  }
  if (length(rows) == 0) return(data.frame())
  dplyr::bind_rows(rows)
}

stpd_detect_train_threshold_resolved_impl <- function(dat, params, min_isi_sec = 0.001, train = "", lock_manual = TRUE) {
  params <- effective_params_for_detector(params)
  dat <- ensure_train_isi_percentiles(dat, min_isi_sec, force = TRUE)
  n <- nrow(dat)
  if (!("pattern_manual_negative" %in% names(dat))) dat$pattern_manual_negative <- rep("", n)
  if (!("auto_score" %in% names(dat))) dat$auto_score <- rep(NA_real_, n)
  if (n <= 1) { dat$pattern_auto <- ""; dat$auto_score <- NA_real_; return(dat) }
  vp <- stpd_event_grammar_params_impl(dat, params, min_isi_sec, train = train)
  manual_for_lock <- if (isTRUE(lock_manual) && !is.null(dat$pattern_manual)) as.character(dat$pattern_manual) else rep("", n)
  manual_for_lock[is.na(manual_for_lock)] <- ""
  locked <- manual_for_lock != ""
  patterns <- params$detector$patterns_to_run %||% stpd_default_patterns_to_run()
  profile <- stpd_event_core_train_profile_row(dat, params, vp, min_isi_sec, train)
  cand_rows <- list(profile)
  hard_thr <- stpd_event_core_detect_hard_isi_thresholds(dat, params, vp, min_isi_sec, train)
  if (nrow(hard_thr) > 0) cand_rows[[length(cand_rows)+1L]] <- hard_thr
  if (any(c("burst", "long_burst") %in% patterns)) {
    b <- stpd_event_grammar_detect_burst_events(dat, params, vp, min_isi_sec, train); if (nrow(b) > 0) cand_rows[[length(cand_rows)+1L]] <- b
  }
  if ("high_frequency_spiking" %in% patterns) { hfs <- stpd_event_core_detect_hf_spiking(dat, params, vp, min_isi_sec, train); if (nrow(hfs) > 0) cand_rows[[length(cand_rows)+1L]] <- hfs }
  if ("high_frequency_tonic" %in% patterns) { hft <- stpd_event_core_detect_hf_tonic(dat, params, vp, min_isi_sec, train); if (nrow(hft) > 0) cand_rows[[length(cand_rows)+1L]] <- hft }
  if ("tonic" %in% patterns) { ton <- stpd_event_core_detect_tonic(dat, params, vp, min_isi_sec, train); if (nrow(ton) > 0) cand_rows[[length(cand_rows)+1L]] <- ton }
  if ("pause" %in% patterns) { pau <- stpd_event_core_detect_pause(dat, params, vp, min_isi_sec, train); if (nrow(pau) > 0) cand_rows[[length(cand_rows)+1L]] <- pau }
  audit <- dplyr::bind_rows(cand_rows)
  audit <- stpd_event_core_weighted_select(audit, locked = locked, patterns = patterns)
  pat <- rep("", n); score <- rep(NA_real_, n)
  if (nrow(audit) > 0) {
    sel_flag <- as.logical(audit$selected_for_auto); sel_flag[is.na(sel_flag)] <- FALSE
    selected <- audit[sel_flag, , drop = FALSE]
    if (nrow(selected) > 0) {
      selected <- selected[order(suppressWarnings(as.integer(selected$start_isi))), , drop = FALSE]
      for (i in seq_len(nrow(selected))) {
        lab <- as.character(selected$final_label[i] %||% ""); s <- suppressWarnings(as.integer(selected$start_isi[i])); e <- suppressWarnings(as.integer(selected$end_isi[i]))
        if (!nzchar(lab) || lab %in% c("reject", "profile") || !is.finite(s) || !is.finite(e) || e < s || s < 2L || e > n) next
        idx <- s:e; if (any(locked[idx] | pat[idx] != "", na.rm = TRUE)) next
        pat[idx] <- lab; score[idx] <- suppressWarnings(as.numeric(selected$score[i] %||% NA_real_))
      }
    }
  }
  if ("others" %in% patterns && isTRUE(params$detector$fill_others_auto %||% FALSE)) {
    isi <- suppressWarnings(as.numeric(dat$ISI_sec)); art <- is_artifact_isi(isi, min_isi_sec)
    fill_idx <- which(seq_len(n) >= 2L & is.finite(isi) & !art & manual_for_lock == "" & pat == "")
    pat[fill_idx] <- "others"
  }
  dat$pattern_auto <- pat; dat$auto_score <- score
  dat <- stpd_post_validate_auto_event_sizes(dat, params, min_isi_sec = min_isi_sec, lock_manual = lock_manual, train = train)
  attr(dat, "candidate_diagnostic_audit") <- audit
  attr(dat, "event_grammar_params") <- vp
  dat
}

stpd_detect_train_threshold_resolved <- function(dat, params, min_isi_sec = 0.001, train = "", lock_manual = TRUE) {
  params <- effective_params_for_detector(params)
  if (isTRUE((params$event_grammar %||% list())$enabled %||% TRUE)) {
    return(stpd_detect_train_threshold_resolved_impl(dat, params, min_isi_sec = min_isi_sec, train = train, lock_manual = lock_manual))
  }
  stpd_train_pipeline_event_grammar_core(dat, params, min_isi_sec = min_isi_sec, train = train, lock_manual = lock_manual)
}

# ============================================================
# event grammar optimization patch
# ------------------------------------------------------------
# This patch keeps the event grammar threshold-resolved/event-grammar structure, but
# fixes several practical failure modes observed in raster output:
#   1) q95 bridge overflow is a soft penalty by default, not a hard reject;
#   2) possible_burst receives dynamic priority based on contrast/core purity;
#   3) one-sided/edge-limited clean burst candidates are retained with useful
#      review priority instead of disappearing behind tonic/pause layers;
#   4) weighted interval selection uses candidate priority instead of a fixed
#      label-only priority table.
# ============================================================

stpd_event_grammar_possible_priority <- function(m, vp, two_possible = FALSE, one_sided = FALSE, q95_excess_ratio = 1) {
  contrast <- stpd_event_grammar_num(m$burst_contrast_score[1], 0)
  core_n <- stpd_event_grammar_num(m$core_isi_count[1], 0)
  bridge_frac <- stpd_event_grammar_num(m$bridge_fraction[1], 1)
  seed_purity <- stpd_event_grammar_num(m$seed_purity[1], NA_real_)
  if (!is.finite(seed_purity)) seed_purity <- core_n / max(1, stpd_event_grammar_num(m$n_valid_isi[1], 1))
  closeness <- if (is.finite(vp$S) && vp$S > 0) min(1.5, contrast / vp$S) else 0
  pri <- 260 + 180 * closeness + 120 * min(1, seed_purity) + 20 * min(5, core_n)
  if (two_possible) pri <- pri + 80
  if (one_sided) pri <- pri + 50
  if (is.finite(q95_excess_ratio) && q95_excess_ratio > 1) pri <- pri - 60 * min(2, q95_excess_ratio - 1)
  if (is.finite(bridge_frac)) pri <- pri - 120 * max(0, bridge_frac - 0.35)
  max(180, min(760, pri))
}

# Priority-aware weighted selection.  Earlier event core code used a fixed label-only
# table that kept possible_burst below tonic/pause.  event grammar candidate rows now carry
# explicit priorities, so use them directly.
stpd_event_core_candidate_value_priority_aware <- function(row) {
  explicit <- stpd_event_grammar_num(row$priority[1], NA_real_)
  lab <- as.character(row$final_label[1] %||% "")
  if (is.finite(explicit) && explicit > 0) {
    sc <- stpd_event_grammar_num(row$score[1], 0)
    n_isi <- stpd_event_grammar_num(row$n_isi[1], 0)
    return(explicit * 10000 + 100 * sc + n_isi)
  }
  pri <- switch(lab,
    burst = 1200,
    long_burst = 1120,
    high_frequency_spiking = 700,
    high_frequency_tonic = 560,
    tonic = 420,
    pause = 320,
    possible_burst = 280,
    0
  )
  sc <- stpd_event_grammar_num(row$score[1], 0)
  n_isi <- stpd_event_grammar_num(row$n_isi[1], 0)
  pri * 10000 + 100 * sc + n_isi
}

stpd_event_grammar_detect_burst_events_threshold_resolved_optimized <- function(dat, params, vp, min_isi_sec = 0.001, train = "") {
  n <- nrow(dat); rows <- list()
  if (n <= 2) return(data.frame())
  isi <- suppressWarnings(as.numeric(dat$ISI_sec))
  art <- is_artifact_isi(isi, min_isi_sec)
  valid <- is.finite(isi) & !art
  if (length(valid) > 0) valid[1] <- FALSE
  seed_flag <- valid & isi >= vp$seed_low & isi <= vp$seed_high
  runs <- stpd_event_grammar_bool_runs(seed_flag)
  if (nrow(runs) == 0) return(data.frame())

  eg <- params$event_grammar %||% list()
  strict_q95 <- isTRUE(eg$strict_q95_bridge_gate %||% FALSE)
  q95_severe_ratio <- stpd_event_grammar_num(eg$q95_soft_severe_ratio %||% 1.35, 1.35)
  q95_severe_ratio <- max(1.0, q95_severe_ratio)
  one_sided_as_canonical <- isTRUE(eg$allow_one_sided_burst_as_canonical %||% FALSE)
  one_sided_S <- stpd_event_grammar_num(eg$one_sided_burst_contrast_min %||% (vp$S + 0.5), vp$S + 0.5)
  one_sided_seed_purity_min <- stpd_event_grammar_num(eg$one_sided_seed_purity_min %||% 0.65, 0.65)
  one_sided_bridge_frac_max <- stpd_event_grammar_num(eg$one_sided_bridge_fraction_max %||% min(0.35, vp$max_bridge_frac), min(0.35, vp$max_bridge_frac))

  seen <- list(); counter <- 0L
  for (rr in seq_len(nrow(runs))) {
    ss <- as.integer(runs$start_isi[rr]); ee <- as.integer(runs$end_isi[rr])
    if (sum(seed_flag[ss:ee], na.rm = TRUE) < vp$min_seed_isi_n) next
    lefts <- stpd_event_core_left_extensions(ss, isi, valid, vp$bridge_high, vp$max_expand)
    rights <- stpd_event_core_right_extensions(ee, n, isi, valid, vp$bridge_high, vp$max_expand)
    for (s in lefts) for (e in rights) {
      if (length(rows) >= vp$max_candidates) break
      key <- paste0(s, "_", e); if (!is.null(seen[[key]])) next; seen[[key]] <- TRUE
      idx <- stpd_event_grammar_safe_seq(s, e); if (length(idx) == 0) next
      core_n <- sum(seed_flag[idx], na.rm = TRUE); if (core_n < vp$min_seed_isi_n) next
      n_spikes <- e - s + 2L; if (n_spikes < vp$min_spikes) next
      m <- stpd_event_core_span_metrics(dat, s, e, params, vp, min_isi_sec, train, "event_grammar_burst_event")
      if (is.null(m)) next

      valid_n <- max(1, stpd_event_grammar_num(m$n_valid_isi[1], length(idx)))
      seed_purity <- core_n / valid_n
      m$seed_purity <- seed_purity
      bridge_count_pass <- is.finite(m$bridge_isi_count[1]) && m$bridge_isi_count[1] <= vp$max_bridge_n
      bridge_fraction_pass <- is.finite(m$bridge_fraction[1]) && m$bridge_fraction[1] <= vp$max_bridge_frac
      q90_pass <- is.finite(m$intra_q90_sec[1]) && m$intra_q90_sec[1] <= vp$bridge_high
      q95_raw_pass <- is.finite(m$intra_q95_sec[1]) && m$intra_q95_sec[1] <= vp$bridge_high
      q95_excess_ratio <- if (is.finite(m$intra_q95_sec[1]) && is.finite(vp$bridge_high) && vp$bridge_high > 0) m$intra_q95_sec[1] / vp$bridge_high else NA_real_
      q95_severe <- is.finite(q95_excess_ratio) && q95_excess_ratio > q95_severe_ratio
      q95_gate_pass <- q95_raw_pass || (!strict_q95 && !q95_severe)

      pre_ratio <- if (is.finite(m$pre_gap_sec[1]) && is.finite(m$intra_q90_sec[1]) && m$intra_q90_sec[1] > 0) m$pre_gap_sec[1] / m$intra_q90_sec[1] else NA_real_
      post_ratio <- if (is.finite(m$post_gap_sec[1]) && is.finite(m$intra_q90_sec[1]) && m$intra_q90_sec[1] > 0) m$post_gap_sec[1] / m$intra_q90_sec[1] else NA_real_
      two_sided <- is.finite(pre_ratio) && is.finite(post_ratio) && pre_ratio >= vp$S && post_ratio >= vp$S
      two_possible <- is.finite(pre_ratio) && is.finite(post_ratio) && pre_ratio >= vp$S_possible && post_ratio >= vp$S_possible
      one_sided <- (is.finite(pre_ratio) && pre_ratio >= one_sided_S) || (is.finite(post_ratio) && post_ratio >= one_sided_S)
      edge_limited <- (!is.finite(pre_ratio) || !is.finite(post_ratio)) && one_sided
      clean_one_sided <- one_sided && seed_purity >= one_sided_seed_purity_min && is.finite(m$bridge_fraction[1]) && m$bridge_fraction[1] <= one_sided_bridge_frac_max && q90_pass && q95_gate_pass
      neg <- isTRUE(m$manual_negative_veto[1])

      size_label <- "prolonged_burst_like"
      if (n_spikes <= vp$classic_max_spikes) size_label <- "burst"
      else if (n_spikes >= vp$long_min_spikes && (vp$long_max_spikes <= 0 || n_spikes <= vp$long_max_spikes)) size_label <- "long_burst"

      final <- "reject"; status <- "event_grammar_reject"; action <- "reject"; decision <- "event_grammar_reject"; priority <- 0
      core_pass <- !neg && bridge_count_pass && bridge_fraction_pass && q90_pass && q95_gate_pass
      q95_note <- if (!q95_raw_pass && q95_gate_pass) ";q95_soft_exceeds_bridge" else ""
      if (core_pass && two_sided) {
        if (size_label %in% c("burst", "long_burst")) {
          final <- size_label; status <- "event_grammar_two_sided_burst_event_pass"; action <- "accept"; decision <- paste0("event_grammar_two_sided_event_grammar_pass__", size_label, q95_note); priority <- if (final == "burst") 1250 else 1160
        } else { final <- "possible_burst"; status <- "event_grammar_prolonged_burst_like_review"; action <- "demote_to_possible"; decision <- paste0("two_sided_structure_but_spike_count_exceeds_long_burst_range", q95_note); priority <- stpd_event_grammar_possible_priority(m, vp, two_possible = TRUE, one_sided = FALSE, q95_excess_ratio = q95_excess_ratio) }
      } else if (core_pass && clean_one_sided) {
        if (one_sided_as_canonical && size_label %in% c("burst", "long_burst")) {
          final <- size_label; status <- "event_grammar_clean_one_sided_burst_event_pass_user_allowed"; action <- "accept"; decision <- paste0("event_grammar_clean_one_sided_event_grammar_pass__", size_label, q95_note); priority <- if (final == "burst") 980 else 930
        } else {
          final <- "possible_burst"; status <- if (edge_limited) "event_grammar_clean_edge_limited_possible_burst" else "event_grammar_clean_one_sided_possible_burst"; action <- "demote_to_possible"; decision <- paste0("clean_one_sided_flank_contrast_pass_core_compact", q95_note); priority <- stpd_event_grammar_possible_priority(m, vp, two_possible = FALSE, one_sided = TRUE, q95_excess_ratio = q95_excess_ratio)
        }
      } else if (!neg && bridge_count_pass && bridge_fraction_pass && q90_pass && q95_gate_pass && two_possible) {
        final <- "possible_burst"; status <- "event_grammar_possible_two_sided_burst"; action <- "demote_to_possible"; decision <- paste0("two_sided_possible_contrast_pass", q95_note); priority <- stpd_event_grammar_possible_priority(m, vp, two_possible = TRUE, one_sided = FALSE, q95_excess_ratio = q95_excess_ratio)
      } else {
        reasons <- c(if (neg) "manual_negative_veto", if (!bridge_count_pass) "too_many_bridge_isis", if (!bridge_fraction_pass) "bridge_fraction_too_high", if (!q90_pass) "intra_q90_exceeds_bridge_band", if (strict_q95 && !q95_raw_pass) "intra_q95_exceeds_bridge_band_strict", if (!strict_q95 && q95_severe) "intra_q95_severely_exceeds_bridge_band", if (!two_sided && !one_sided && !two_possible) "flank_contrast_fail")
        decision <- paste(reasons, collapse = ";"); if (!nzchar(decision)) decision <- "event_grammar_reject"
      }
      q95_penalty <- if (is.finite(q95_excess_ratio) && q95_excess_ratio > 1) 0.45 * min(2, q95_excess_ratio - 1) else 0
      score <- (if (is.finite(m$burst_contrast_score[1])) m$burst_contrast_score[1] else 0) + 0.16 * m$core_isi_count[1] + 0.35 * seed_purity - 0.20 * m$bridge_isi_count[1] - 0.45 * m$bridge_fraction[1] - q95_penalty
      counter <- counter + 1L
      rows[[length(rows) + 1L]] <- stpd_event_core_candidate_row(m, "event_grammar_burst_event", "event_grammar_seed_centered_burst", final, status, decision, action, score, priority,
        list(candidate_id = paste0("event_grammar_burst_", counter), seed_run_start_isi = ss, seed_run_end_isi = ee,
             seed_band_lower_sec = vp$seed_low, seed_band_upper_sec = vp$seed_high, bridge_band_upper_sec = vp$bridge_high,
             burst_contrast_required = vp$S, one_sided_contrast_required = one_sided_S, possible_contrast_required = vp$S_possible,
             pre_ratio_q90 = pre_ratio, post_ratio_q90 = post_ratio,
             seed_purity = seed_purity,
             q95_excess_ratio = q95_excess_ratio,
             q95_bridge_hard_gate = strict_q95,
             q95_bridge_soft_pass = q95_gate_pass,
             q95_bridge_severe = q95_severe,
             q95_bridge_penalty = q95_penalty,
             boundary_type = if (two_sided) "two_sided" else if (clean_one_sided) "clean_one_sided_or_edge_limited" else if (one_sided) "weak_one_sided" else "failed",
             strict_boundary_pass = two_sided, one_sided_boundary_pass = one_sided, clean_one_sided_pass = clean_one_sided, possible_boundary_pass = two_possible,
             bridge_count_pass = bridge_count_pass, bridge_fraction_pass = bridge_fraction_pass, q90_bridge_pass = q90_pass, q95_bridge_pass = q95_raw_pass,
             threshold_source_summary = paste0("seed=", vp$threshold_table$source[vp$threshold_table$pattern=="burst" & vp$threshold_table$field=="seed_upper_sec"][1] %||% "")))
    }
    if (length(rows) >= vp$max_candidates) break
  }
  if (length(rows) == 0) return(data.frame())
  dplyr::bind_rows(rows)
}
