# Distributional evidence layer.
#
# This module adds a second analysis layer between candidate generation and
# final audited interpretation. It does not rewrite detector labels. Instead it
# quantifies how strongly each candidate/event and each train is supported by
# ISI distribution, spike-count PMF, Fano factor and logISI higher moments.

stpd_as_finite_positive <- function(x, min_value = 0) {
  x <- finite_num(x)
  x[is.finite(x) & x > min_value]
}

stpd_calc_CV2 <- function(Ti) {
  Ti <- stpd_as_finite_positive(Ti)
  if (length(Ti) < 2L) return(NA_real_)
  a <- head(Ti, -1L)
  b <- tail(Ti, -1L)
  denom <- a + b
  ok <- is.finite(denom) & denom > 0
  if (!any(ok)) return(NA_real_)
  mean(2 * abs(a[ok] - b[ok]) / denom[ok], na.rm = TRUE)
}

stpd_calc_LvR <- function(Ti, refractory_sec = 0.005) {
  Ti <- stpd_as_finite_positive(Ti)
  if (length(Ti) < 2L) return(NA_real_)
  refractory_sec <- suppressWarnings(as.numeric(refractory_sec)[1])
  if (!is.finite(refractory_sec) || refractory_sec < 0) refractory_sec <- 0
  a <- head(Ti, -1L)
  b <- tail(Ti, -1L)
  denom <- a + b
  ok <- is.finite(denom) & denom > 0
  if (!any(ok)) return(NA_real_)
  mean(3 * (1 - (4 * a[ok] * b[ok]) / (denom[ok]^2)) *
         (1 + (4 * refractory_sec) / denom[ok]), na.rm = TRUE)
}

stpd_moment_skewness <- function(x) {
  x <- finite_num(x)
  if (length(x) < 3L) return(NA_real_)
  m <- mean(x)
  s <- stats::sd(x)
  if (!is.finite(s) || s <= 0) return(NA_real_)
  mean(((x - m) / s)^3, na.rm = TRUE)
}

stpd_moment_excess_kurtosis <- function(x) {
  x <- finite_num(x)
  if (length(x) < 4L) return(NA_real_)
  m <- mean(x)
  s <- stats::sd(x)
  if (!is.finite(s) || s <= 0) return(NA_real_)
  mean(((x - m) / s)^4, na.rm = TRUE) - 3
}

stpd_ecdf_mass <- function(x, threshold, direction = c("le", "ge")) {
  direction <- match.arg(direction)
  x <- finite_num(x)
  threshold <- suppressWarnings(as.numeric(threshold)[1])
  if (length(x) == 0L || !is.finite(threshold)) return(NA_real_)
  if (direction == "le") mean(x <= threshold, na.rm = TRUE) else mean(x >= threshold, na.rm = TRUE)
}

stpd_safe_ks_distance <- function(x, y) {
  x <- finite_num(x)
  y <- finite_num(y)
  if (length(x) < 2L || length(y) < 2L) return(NA_real_)
  grid <- sort(unique(c(x, y)))
  if (length(grid) == 0L) return(NA_real_)
  max(abs(stats::ecdf(x)(grid) - stats::ecdf(y)(grid)), na.rm = TRUE)
}

stpd_wasserstein_1d <- function(x, y, n_grid = 101L) {
  x <- finite_num(x)
  y <- finite_num(y)
  if (length(x) == 0L || length(y) == 0L) return(NA_real_)
  n_grid <- max(11L, suppressWarnings(as.integer(n_grid)[1] %||% 101L))
  probs <- seq(0, 1, length.out = n_grid)
  qx <- stats::quantile(x, probs = probs, names = FALSE, na.rm = TRUE, type = 8)
  qy <- stats::quantile(y, probs = probs, names = FALSE, na.rm = TRUE, type = 8)
  mean(abs(qx - qy), na.rm = TRUE)
}

stpd_logisi_moments <- function(isi_sec, min_isi_sec = 0.001) {
  min_isi_sec <- suppressWarnings(as.numeric(min_isi_sec)[1])
  if (!is.finite(min_isi_sec) || min_isi_sec < 0) min_isi_sec <- 0
  x <- stpd_as_finite_positive(isi_sec, min_isi_sec)
  if (length(x) == 0L) {
    return(data.frame(
      n_valid_isi = 0L,
      logISI_mean = NA_real_,
      logISI_sd = NA_real_,
      logISI_skewness = NA_real_,
      logISI_excess_kurtosis = NA_real_,
      logISI_q10 = NA_real_,
      logISI_q50 = NA_real_,
      logISI_q90 = NA_real_,
      logISI_q95 = NA_real_,
      logISI_robust_skew_q90_q10 = NA_real_,
      logISI_tail_ratio_q95_q50 = NA_real_,
      stringsAsFactors = FALSE
    ))
  }
  lx <- log10(x)
  q <- safe_q(lx, c(0.10, 0.50, 0.90, 0.95))
  robust_skew <- if (all(is.finite(q[1:3])) && (q[3] - q[1]) > 0) {
    ((q[3] + q[1]) - 2 * q[2]) / (q[3] - q[1])
  } else {
    NA_real_
  }
  tail_ratio <- if (is.finite(q[4]) && is.finite(q[2])) 10^(q[4] - q[2]) else NA_real_
  data.frame(
    n_valid_isi = length(x),
    logISI_mean = mean(lx, na.rm = TRUE),
    logISI_sd = if (length(lx) >= 2L) stats::sd(lx, na.rm = TRUE) else NA_real_,
    logISI_skewness = stpd_moment_skewness(lx),
    logISI_excess_kurtosis = stpd_moment_excess_kurtosis(lx),
    logISI_q10 = q[1],
    logISI_q50 = q[2],
    logISI_q90 = q[3],
    logISI_q95 = q[4],
    logISI_robust_skew_q90_q10 = robust_skew,
    logISI_tail_ratio_q95_q50 = tail_ratio,
    stringsAsFactors = FALSE
  )
}

stpd_ms_label <- function(sec) {
  sec <- suppressWarnings(as.numeric(sec)[1])
  if (!is.finite(sec)) return("NAms")
  paste0(trimws(formatC(sec * 1000, format = "fg", digits = 4)), "ms")
}

stpd_spike_count_features_for_times <- function(times_sec, window_sec) {
  times <- sort(finite_num(times_sec))
  window_sec <- suppressWarnings(as.numeric(window_sec)[1])
  if (length(times) == 0L || !is.finite(window_sec) || window_sec <= 0) {
    return(list(
      n_windows = 0L,
      mean_N = NA_real_,
      var_N = NA_real_,
      fano = NA_real_,
      P_N0 = NA_real_,
      P_N1 = NA_real_,
      P_Nge2 = NA_real_,
      P_Nge3 = NA_real_,
      poisson_deviation_L1 = NA_real_
    ))
  }
  start <- min(0, floor(min(times) / window_sec) * window_sec)
  end <- ceiling(max(times) / window_sec) * window_sec
  if (!is.finite(end) || end <= start) end <- start + window_sec
  breaks <- seq(start, end + window_sec, by = window_sec)
  if (length(breaks) < 2L) breaks <- c(start, start + window_sec)
  counts <- hist(times, breaks = breaks, plot = FALSE, right = FALSE, include.lowest = TRUE)$counts
  counts <- suppressWarnings(as.integer(counts))
  if (length(counts) == 0L) {
    return(list(
      n_windows = 0L,
      mean_N = NA_real_,
      var_N = NA_real_,
      fano = NA_real_,
      P_N0 = NA_real_,
      P_N1 = NA_real_,
      P_Nge2 = NA_real_,
      P_Nge3 = NA_real_,
      poisson_deviation_L1 = NA_real_
    ))
  }
  mean_n <- mean(counts)
  var_n <- if (length(counts) >= 2L) stats::var(counts) else 0
  fano <- if (is.finite(mean_n) && mean_n > 0) var_n / mean_n else NA_real_
  k <- 0:max(counts)
  empirical <- tabulate(counts + 1L, nbins = length(k)) / length(counts)
  poisson <- if (is.finite(mean_n)) stats::dpois(k, lambda = mean_n) else rep(NA_real_, length(k))
  list(
    n_windows = length(counts),
    mean_N = mean_n,
    var_N = var_n,
    fano = fano,
    P_N0 = mean(counts == 0L),
    P_N1 = mean(counts == 1L),
    P_Nge2 = mean(counts >= 2L),
    P_Nge3 = mean(counts >= 3L),
    poisson_deviation_L1 = if (all(is.finite(poisson))) sum(abs(empirical - poisson), na.rm = TRUE) else NA_real_
  )
}

stpd_spike_count_pmf <- function(ds_or_trains,
                                 selected_trains = NULL,
                                 windows_sec = c(0.020, 0.050, 0.100)) {
  trains <- if (!is.null(ds_or_trains$trains)) ds_or_trains$trains else ds_or_trains
  if (is.null(trains) || length(trains) == 0L) return(tibble::tibble())
  target <- intersect(as.character(selected_trains %||% names(trains)), names(trains))
  if (length(target) == 0L) return(tibble::tibble())
  windows_sec <- finite_num(windows_sec)
  windows_sec <- windows_sec[windows_sec > 0]
  if (length(windows_sec) == 0L) return(tibble::tibble())
  rows <- list()
  for (tr in target) {
    dat <- trains[[tr]]
    times <- finite_num(dat$timestamp_sec %||% numeric())
    for (w in windows_sec) {
      feat <- stpd_spike_count_features_for_times(times, w)
      rows[[length(rows) + 1L]] <- data.frame(
        train = tr,
        window_sec = w,
        window_label = stpd_ms_label(w),
        n_windows = feat$n_windows,
        mean_N = feat$mean_N,
        var_N = feat$var_N,
        fano = feat$fano,
        P_N0 = feat$P_N0,
        P_N1 = feat$P_N1,
        P_Nge2 = feat$P_Nge2,
        P_Nge3 = feat$P_Nge3,
        poisson_deviation_L1 = feat$poisson_deviation_L1,
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(rows) == 0L) tibble::tibble() else tibble::as_tibble(do.call(rbind, rows))
}

stpd_distributional_score <- function(x, default = 0) {
  x <- suppressWarnings(as.numeric(x)[1])
  if (is.finite(x)) x else default
}

stpd_train_distribution_features <- function(ds_or_trains,
                                             selected_trains = NULL,
                                             min_isi_sec = 0.001,
                                             burst_thresholds_sec = c(0.005, 0.010, 0.020),
                                             pause_thresholds_sec = c(0.100, 0.200, 0.500),
                                             count_windows_sec = c(0.020, 0.050, 0.100),
                                             refractory_sec = 0.005) {
  trains <- if (!is.null(ds_or_trains$trains)) ds_or_trains$trains else ds_or_trains
  if (is.null(trains) || length(trains) == 0L) return(tibble::tibble())
  target <- intersect(as.character(selected_trains %||% names(trains)), names(trains))
  if (length(target) == 0L) return(tibble::tibble())
  burst_thresholds_sec <- finite_num(burst_thresholds_sec)
  pause_thresholds_sec <- finite_num(pause_thresholds_sec)
  count_windows_sec <- finite_num(count_windows_sec)
  rows <- list()
  for (tr in target) {
    dat <- trains[[tr]]
    isi <- stpd_as_finite_positive(dat$ISI_sec %||% numeric(), min_isi_sec)
    times <- finite_num(dat$timestamp_sec %||% numeric())
    duration <- if (length(times) >= 2L) max(times, na.rm = TRUE) - min(times, na.rm = TRUE) else sum(isi, na.rm = TRUE)
    n_spikes <- length(times)
    rate <- if (is.finite(duration) && duration > 0) n_spikes / duration else NA_real_
    isi_q <- safe_q(isi, c(0.10, 0.25, 0.50, 0.75, 0.90))
    logmom <- stpd_logisi_moments(isi, min_isi_sec = min_isi_sec)
    base <- data.frame(
      train = tr,
      n_spikes = n_spikes,
      n_valid_isi = length(isi),
      recording_duration_sec = duration,
      mean_firing_rate_hz = rate,
      mean_ISI_sec = if (length(isi) > 0L) mean(isi, na.rm = TRUE) else NA_real_,
      median_ISI_sec = isi_q[3],
      IQR_ISI_sec = if (all(is.finite(isi_q[c(2, 4)]))) isi_q[4] - isi_q[2] else NA_real_,
      q10_ISI_sec = isi_q[1],
      q90_ISI_sec = isi_q[5],
      CV = calc_CV(isi),
      CV2 = stpd_calc_CV2(isi),
      LV = calc_LV(isi),
      LvR = stpd_calc_LvR(isi, refractory_sec = refractory_sec),
      stringsAsFactors = FALSE
    )
    for (thr in burst_thresholds_sec) {
      nm <- paste0("ISI_CDF_mass_le_", stpd_ms_label(thr))
      base[[nm]] <- stpd_ecdf_mass(isi, thr, "le")
    }
    for (thr in pause_thresholds_sec) {
      nm <- paste0("ISI_tail_mass_ge_", stpd_ms_label(thr))
      base[[nm]] <- stpd_ecdf_mass(isi, thr, "ge")
    }
    for (w in count_windows_sec) {
      feat <- stpd_spike_count_features_for_times(times, w)
      suffix <- stpd_ms_label(w)
      base[[paste0("P_N0_", suffix)]] <- feat$P_N0
      base[[paste0("P_N1_", suffix)]] <- feat$P_N1
      base[[paste0("P_Nge2_", suffix)]] <- feat$P_Nge2
      base[[paste0("P_Nge3_", suffix)]] <- feat$P_Nge3
      base[[paste0("Fano_", suffix)]] <- feat$fano
      base[[paste0("PMF_poisson_L1_", suffix)]] <- feat$poisson_deviation_L1
    }
    burst_cols <- grep("^ISI_CDF_mass_le_|^P_Nge2_|^P_Nge3_", names(base), value = TRUE)
    pause_tail_cols <- grep("^ISI_tail_mass_ge_", names(base), value = TRUE)
    burst_signal <- if (length(burst_cols) > 0L) max(finite_num(unlist(base[burst_cols])), na.rm = TRUE) else NA_real_
    pause_tail_signal <- if (length(pause_tail_cols) > 0L) max(finite_num(unlist(base[pause_tail_cols])), na.rm = TRUE) else NA_real_
    if (!is.finite(burst_signal)) burst_signal <- 0
    if (!is.finite(pause_tail_signal)) pause_tail_signal <- 0
    q10_median_contrast <- if (is.finite(base$q10_ISI_sec) && is.finite(base$median_ISI_sec) && base$median_ISI_sec > 0) {
      max(0, 1 - base$q10_ISI_sec / base$median_ISI_sec)
    } else {
      0
    }
    tail_spread <- if (is.finite(base$q90_ISI_sec) && is.finite(base$median_ISI_sec) && base$median_ISI_sec > 0) {
      r <- base$q90_ISI_sec / base$median_ISI_sec
      max(0, (r - 1) / (r + 1))
    } else {
      0
    }
    variability_score <- mean(c(
      stpd_distributional_score(base$CV, 0) / (1 + stpd_distributional_score(base$CV, 0)),
      stpd_distributional_score(base$CV2, 0) / (1 + stpd_distributional_score(base$CV2, 0)),
      stpd_distributional_score(base$LV, 0) / (1 + stpd_distributional_score(base$LV, 0))
    ), na.rm = TRUE)
    regularity <- 1 / (1 + mean(c(stpd_distributional_score(base$CV2, 1),
                                  stpd_distributional_score(base$LV, 1),
                                  stpd_distributional_score(base$CV, 1)), na.rm = TRUE))
    tonic_score <- regularity * (1 - min(0.8, q10_median_contrast)) * (1 - min(0.8, tail_spread))
    burst_score <- min(1, 0.55 * q10_median_contrast + 0.20 * burst_signal +
                         0.15 * stpd_distributional_score(base$LV, 0) / (1 + stpd_distributional_score(base$LV, 0)) +
                         0.10 * stpd_distributional_score(base$CV2, 0) / (1 + stpd_distributional_score(base$CV2, 0)))
    pause_score <- min(1, 0.60 * tail_spread + 0.20 * pause_tail_signal * variability_score +
                         0.20 * variability_score)
    base$phenotype_short_tail_contrast <- q10_median_contrast
    base$phenotype_long_tail_spread <- tail_spread
    base$phenotype_variability_score <- variability_score
    score_vec <- c(tonic_like = tonic_score, burst_like = burst_score, pause_like = pause_score)
    ord <- order(score_vec, decreasing = TRUE)
    phenotype <- if (length(isi) < 5L) {
      "insufficient_data"
    } else if (burst_score >= 0.45 && pause_score >= 0.45) {
      "mixed_burst_pause_like"
    } else {
      names(score_vec)[ord[1]]
    }
    confidence <- if (length(ord) >= 2L) max(0, score_vec[ord[1]] - score_vec[ord[2]]) else NA_real_
    base$phenotype_tonic_score <- tonic_score
    base$phenotype_burst_score <- burst_score
    base$phenotype_pause_score <- pause_score
    base$dominant_phenotype <- phenotype
    base$phenotype_confidence <- confidence
    logmom$n_valid_isi <- NULL
    rows[[length(rows) + 1L]] <- cbind(base, logmom, stringsAsFactors = FALSE)
  }
  if (length(rows) == 0L) tibble::tibble() else tibble::as_tibble(do.call(rbind, rows))
}

stpd_row_value <- function(row, names, default = NA) {
  if (is.null(row) || !is.data.frame(row) || nrow(row) == 0L) return(default)
  for (nm in names) {
    if (nm %in% names(row)) {
      val <- row[[nm]][1]
      if (length(val) > 0L && !is.na(val)) return(val)
    }
  }
  default
}

stpd_row_label <- function(row) {
  val <- stpd_row_value(
    row,
    c("final_candidate_class", "recommended_final_class", "recommended_subtype",
      "raw_candidate_class", "pattern", "label", "final_label_majority"),
    ""
  )
  val <- as.character(val %||% "")[1]
  if (is.na(val)) "" else val
}

stpd_distribution_support_call <- function(label, n_local, local_short, global_short,
                                           local_tail, global_tail, median_ratio,
                                           local_cv2, local_lv, ks_global) {
  if (!is.finite(n_local) || n_local < 3L) {
    return(c("insufficient_data", "fewer than 3 valid local ISIs"))
  }
  label_norm <- tolower(as.character(label %||% ""))
  short_enrich <- local_short - global_short
  tail_enrich <- local_tail - global_tail
  if (grepl("burst", label_norm)) {
    if (is.finite(local_short) && is.finite(short_enrich) &&
        (local_short >= 0.60 || short_enrich >= 0.30 || (is.finite(ks_global) && ks_global >= 0.35))) {
      return(c("strong", "local short-ISI mass is clearly enriched"))
    }
    if (is.finite(local_short) && is.finite(short_enrich) &&
        (local_short >= 0.40 || short_enrich >= 0.15)) {
      return(c("moderate", "local short-ISI mass is moderately enriched"))
    }
    if (is.finite(median_ratio) && median_ratio >= 0.80 && is.finite(short_enrich) && short_enrich <= 0.05) {
      return(c("contradictory", "local ISIs are not shorter than the train background"))
    }
    return(c("weak", "burst-like distributional enrichment is small"))
  }
  if (grepl("pause", label_norm)) {
    if ((is.finite(local_tail) && is.finite(tail_enrich) && (local_tail >= 0.50 || tail_enrich >= 0.30)) ||
        (is.finite(median_ratio) && median_ratio >= 2.0)) {
      return(c("strong", "local long-ISI tail is clearly enriched"))
    }
    if ((is.finite(local_tail) && is.finite(tail_enrich) && (local_tail >= 0.25 || tail_enrich >= 0.15)) ||
        (is.finite(median_ratio) && median_ratio >= 1.5)) {
      return(c("moderate", "local long-ISI tail is moderately enriched"))
    }
    if (is.finite(tail_enrich) && tail_enrich <= 0.05 && is.finite(median_ratio) && median_ratio <= 1.2) {
      return(c("contradictory", "local ISIs are not longer than the train background"))
    }
    return(c("weak", "pause-like tail evidence is small"))
  }
  if (grepl("tonic", label_norm)) {
    if ((is.finite(local_cv2) && local_cv2 <= 0.25) || (is.finite(local_lv) && local_lv <= 0.35)) {
      return(c("strong", "local ISIs are regular by CV2/LV"))
    }
    if ((is.finite(local_cv2) && local_cv2 <= 0.50) || (is.finite(local_lv) && local_lv <= 0.75)) {
      return(c("moderate", "local ISIs are moderately regular"))
    }
    if ((is.finite(local_cv2) && local_cv2 > 1.0) || (is.finite(local_lv) && local_lv > 1.5)) {
      return(c("contradictory", "local ISIs are irregular for a tonic candidate"))
    }
    return(c("weak", "tonic regularity evidence is small"))
  }
  if (is.finite(ks_global) && ks_global >= 0.35) {
    return(c("moderate", "local distribution differs from the train background"))
  }
  c("weak", "distributional contrast is small")
}

stpd_mode_nonempty_safe <- function(x) {
  x <- as.character(x %||% character())
  x[is.na(x)] <- ""
  x <- x[nzchar(x)]
  if (length(x) == 0L) return("")
  names(sort(table(x), decreasing = TRUE))[1]
}

stpd_event_distribution_evidence <- function(ds,
                                             candidates = NULL,
                                             selected_trains = NULL,
                                             min_isi_sec = 0.001,
                                             burst_threshold_sec = NA_real_,
                                             pause_threshold_sec = NA_real_,
                                             flank_n = 10L,
                                             refractory_sec = 0.005) {
  if (is.null(ds) || is.null(ds$trains)) return(tibble::tibble())
  candidates <- candidates %||% ds$results$candidate_features %||% ds$results$candidate_ledger %||% data.frame()
  if (is.null(candidates) || !is.data.frame(candidates) || nrow(candidates) == 0L) return(tibble::tibble())
  target <- intersect(as.character(selected_trains %||% names(ds$trains)), names(ds$trains))
  if (length(target) == 0L) return(tibble::tibble())
  flank_n <- max(1L, suppressWarnings(as.integer(flank_n)[1] %||% 10L))
  rows <- list()
  for (ii in seq_len(nrow(candidates))) {
    row <- candidates[ii, , drop = FALSE]
    tr <- as.character(stpd_row_value(row, "train", ""))[1]
    if (!nzchar(tr) || !tr %in% target || !tr %in% names(ds$trains)) next
    dat <- ds$trains[[tr]]
    n <- nrow(dat)
    s0 <- suppressWarnings(as.integer(stpd_row_value(row, "start_isi", NA_integer_)))
    e0 <- suppressWarnings(as.integer(stpd_row_value(row, "end_isi", NA_integer_)))
    if (!is.finite(s0) || !is.finite(e0) || s0 < 2L || e0 < s0 || e0 > n) next
    isi_all_raw <- suppressWarnings(as.numeric(dat$ISI_sec %||% rep(NA_real_, n)))
    global <- stpd_as_finite_positive(isi_all_raw, min_isi_sec)
    idx <- seq(max(2L, s0), min(n, e0))
    local <- stpd_as_finite_positive(isi_all_raw[idx], min_isi_sec)
    pre_idx <- if (s0 > 2L) seq(max(2L, s0 - flank_n), s0 - 1L) else integer(0)
    post_idx <- if (e0 < n) seq(e0 + 1L, min(n, e0 + flank_n)) else integer(0)
    pre <- stpd_as_finite_positive(isi_all_raw[pre_idx], min_isi_sec)
    post <- stpd_as_finite_positive(isi_all_raw[post_idx], min_isi_sec)
    flank <- c(pre, post)
    adaptive_short <- safe_q(global, 0.25)
    adaptive_long <- safe_q(global, 0.90)
    short_thr <- suppressWarnings(as.numeric(burst_threshold_sec)[1])
    if (!is.finite(short_thr) || short_thr <= min_isi_sec) short_thr <- adaptive_short
    long_thr <- suppressWarnings(as.numeric(pause_threshold_sec)[1])
    if (!is.finite(long_thr) || long_thr <= min_isi_sec) long_thr <- adaptive_long
    g_med <- safe_median(global)
    l_med <- safe_median(local)
    f_med <- safe_median(flank)
    pre_med <- safe_median(pre)
    post_med <- safe_median(post)
    local_short <- stpd_ecdf_mass(local, short_thr, "le")
    global_short <- stpd_ecdf_mass(global, short_thr, "le")
    flank_short <- stpd_ecdf_mass(flank, short_thr, "le")
    local_tail <- stpd_ecdf_mass(local, long_thr, "ge")
    global_tail <- stpd_ecdf_mass(global, long_thr, "ge")
    flank_tail <- stpd_ecdf_mass(flank, long_thr, "ge")
    median_ratio <- if (is.finite(l_med) && is.finite(g_med) && g_med > 0) l_med / g_med else NA_real_
    label <- stpd_row_label(row)
    cv2 <- stpd_calc_CV2(local)
    lv <- calc_LV(local)
    lvr <- stpd_calc_LvR(local, refractory_sec = refractory_sec)
    ks_g <- stpd_safe_ks_distance(log10(local), log10(global))
    ks_f <- stpd_safe_ks_distance(log10(local), log10(flank))
    wass_g <- stpd_wasserstein_1d(log10(local), log10(global))
    support <- stpd_distribution_support_call(
      label = label,
      n_local = length(local),
      local_short = local_short,
      global_short = global_short,
      local_tail = local_tail,
      global_tail = global_tail,
      median_ratio = median_ratio,
      local_cv2 = cv2,
      local_lv = lv,
      ks_global = ks_g
    )
    audit_labels <- tryCatch(stpd_audit_final_labels(dat, min_isi_sec = min_isi_sec), error = function(e) character(0))
    audit_final <- if (length(audit_labels) >= max(idx)) stpd_mode_nonempty_safe(audit_labels[idx]) else ""
    rows[[length(rows) + 1L]] <- data.frame(
      candidate_id = as.character(stpd_row_value(row, c("candidate_id", "event_id"), paste0("candidate_", ii))),
      train = tr,
      start_isi = s0,
      end_isi = e0,
      n_isi = length(idx),
      n_valid_local_isi = length(local),
      start_time_sec = suppressWarnings(as.numeric(stpd_row_value(row, "start_time_sec", dat$timestamp_sec[max(1L, s0 - 1L)]))),
      end_time_sec = suppressWarnings(as.numeric(stpd_row_value(row, "end_time_sec", dat$timestamp_sec[min(n, e0)]))),
      grammar_detected_label = label,
      audit_final_label = audit_final,
      adaptive_short_threshold_sec = adaptive_short,
      adaptive_long_threshold_sec = adaptive_long,
      short_threshold_used_sec = short_thr,
      long_threshold_used_sec = long_thr,
      local_median_ISI_sec = l_med,
      global_median_ISI_sec = g_med,
      flank_median_ISI_sec = f_med,
      pre_median_ISI_sec = pre_med,
      post_median_ISI_sec = post_med,
      local_global_median_ratio = median_ratio,
      local_short_ISI_mass = local_short,
      global_short_ISI_mass = global_short,
      flank_short_ISI_mass = flank_short,
      short_ISI_enrichment_vs_global = local_short - global_short,
      short_ISI_enrichment_vs_flank = local_short - flank_short,
      local_long_ISI_tail_mass = local_tail,
      global_long_ISI_tail_mass = global_tail,
      flank_long_ISI_tail_mass = flank_tail,
      long_ISI_tail_enrichment_vs_global = local_tail - global_tail,
      long_ISI_tail_enrichment_vs_flank = local_tail - flank_tail,
      local_CV = calc_CV(local),
      local_CV2 = cv2,
      local_LV = lv,
      local_LvR = lvr,
      local_tonic_regular_index = 1 / (1 + mean(c(stpd_distributional_score(cv2, 1), stpd_distributional_score(lv, 1)), na.rm = TRUE)),
      KS_logISI_local_vs_global = ks_g,
      KS_logISI_local_vs_flank = ks_f,
      W1_logISI_local_vs_global = wass_g,
      distribution_support = support[1],
      distribution_support_reason = support[2],
      stringsAsFactors = FALSE
    )
  }
  if (length(rows) == 0L) tibble::tibble() else tibble::as_tibble(do.call(rbind, rows))
}

stpd_add_distributional_results <- function(ds,
                                            params = default_params_sec(),
                                            selected_trains = NULL,
                                            candidates = NULL) {
  if (is.null(ds) || is.null(ds$trains)) return(ds)
  if (is.null(ds$results)) ds$results <- list()
  min_isi <- suppressWarnings(as.numeric(params$detector$min_valid_isi_sec %||% params$detector$effective_min_isi_sec %||% 0.001))
  if (!is.finite(min_isi) || min_isi < 0) min_isi <- 0.001
  target <- intersect(as.character(selected_trains %||% names(ds$trains)), names(ds$trains))
  ds$results$event_distribution_evidence <- tryCatch(
    stpd_event_distribution_evidence(
      ds,
      candidates = candidates %||% ds$results$candidate_features %||% ds$results$candidate_ledger %||% data.frame(),
      selected_trains = target,
      min_isi_sec = min_isi
    ),
    error = function(e) data.frame(error = paste0("event_distribution_evidence failed: ", conditionMessage(e)), stringsAsFactors = FALSE)
  )
  ds$results$train_distribution_features <- tryCatch(
    stpd_train_distribution_features(ds, selected_trains = target, min_isi_sec = min_isi),
    error = function(e) data.frame(error = paste0("train_distribution_features failed: ", conditionMessage(e)), stringsAsFactors = FALSE)
  )
  ds$results$spike_count_pmf <- tryCatch(
    stpd_spike_count_pmf(ds, selected_trains = target),
    error = function(e) data.frame(error = paste0("spike_count_pmf failed: ", conditionMessage(e)), stringsAsFactors = FALSE)
  )
  ds$results$distributional_evidence_note <- data.frame(
    item = c("interpretation", "detector_influence", "threshold_policy"),
    value = c(
      "Distributional evidence is an audit/support layer for candidate interpretation and downstream analysis.",
      "This layer does not rewrite pattern_auto, pattern_manual, pattern_audit_final or event boundaries.",
      "Event-level support uses adaptive train ECDF thresholds by default; fixed 5/10/20 ms and long-tail features are retained in train summaries."
    ),
    stringsAsFactors = FALSE
  )
  ds
}
