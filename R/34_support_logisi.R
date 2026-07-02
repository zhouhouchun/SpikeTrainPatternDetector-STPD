# LogISI / newBD burst threshold support layer
# -----------------------------------------------------------------------------
# Article-conformant support implementation of Pasquale, Martinoia &
# Chiappalone's logISIH / newBD procedure.
#
# Role in SpikeTrainPatternDetector:
#   - external burst-threshold support layer
#   - candidate-support layer
#   - not final ground-truth classifier
#
# Internal units:
#   - spike timestamps: seconds
#   - ISI thresholds: seconds
#   - logISIH bins: log10(ISI_ms), matching the Pasquale et al. convention.
# -----------------------------------------------------------------------------

stpd_logisi_num1 <- function(x, default = NA_real_) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) < 1L || !is.finite(x[1])) default else x[1]
}

stpd_logisi_chr1 <- function(x, default = "") {
  x <- as.character(x)
  if (length(x) < 1L || is.na(x[1])) default else x[1]
}

stpd_logisi_find_local_peaks <- function(y, min_peak_distance = 2L) {
  y <- suppressWarnings(as.numeric(y))
  n <- length(y)
  if (n < 3L) return(integer())
  cand <- which(is.finite(y[-c(1L, n)]) &
                  y[-c(1L, n)] > y[-c(n - 1L, n)] &
                  y[-c(1L, n)] > y[-c(1L, 2L)]) + 1L
  if (length(cand) <= 1L) return(cand)

  min_peak_distance <- max(1L, as.integer(min_peak_distance))
  ord <- cand[order(y[cand], decreasing = TRUE)]
  keep <- integer()
  for (idx in ord) {
    if (!length(keep) || all(abs(idx - keep) >= min_peak_distance)) {
      keep <- c(keep, idx)
    }
  }
  sort(keep)
}

stpd_build_logisih_pasquale <- function(
  isi_sec,
  min_valid_isi_sec = 0.001,
  bin_width_log10 = 0.1,
  lowess_span = 0.12
) {
  isi_sec <- suppressWarnings(as.numeric(isi_sec))
  valid <- is.finite(isi_sec) & isi_sec >= min_valid_isi_sec & isi_sec > 0
  isi_ms <- isi_sec[valid] * 1000

  if (length(isi_ms) < 3L) {
    return(data.frame(
      bin_center_log10_ms = numeric(),
      bin_center_ms = numeric(),
      probability = numeric(),
      smoothed_probability = numeric()
    ))
  }

  log_isi <- log10(isi_ms)
  lo <- floor(min(log_isi, na.rm = TRUE) / bin_width_log10) * bin_width_log10
  hi <- ceiling(max(log_isi, na.rm = TRUE) / bin_width_log10) * bin_width_log10
  if (!is.finite(lo) || !is.finite(hi) || hi <= lo) hi <- lo + bin_width_log10

  breaks <- seq(lo, hi + bin_width_log10, by = bin_width_log10)
  h <- hist(log_isi, breaks = breaks, plot = FALSE, right = FALSE)
  y <- h$counts / max(1L, sum(h$counts))
  x <- h$mids

  if (length(x) >= 5L && any(y > 0)) {
    sm <- tryCatch(stats::lowess(x, y, f = lowess_span, iter = 0), error = function(e) NULL)
    ys <- if (!is.null(sm) && length(sm$y) == length(y)) sm$y else y
  } else {
    ys <- y
  }
  ys <- pmax(0, suppressWarnings(as.numeric(ys)))

  data.frame(
    bin_center_log10_ms = x,
    bin_center_ms = 10^x,
    probability = y,
    smoothed_probability = ys
  )
}

stpd_estimate_logisi_threshold_pasquale <- function(
  spike_times_sec = NULL,
  isi_sec = NULL,
  min_valid_isi_sec = 0.001,
  bin_width_log10 = 0.1,
  lowess_span = 0.12,
  min_peak_distance = 2L,
  intraburst_peak_window_ms = 100,
  void_threshold = 0.7,
  max_reasonable_threshold_sec = 1.0,
  valley_selection = c("max_void", "first_eligible")
) {
  valley_selection <- match.arg(valley_selection)

  if (is.null(isi_sec)) {
    x <- suppressWarnings(as.numeric(spike_times_sec))
    x <- sort(x[is.finite(x)])
    isi_sec <- diff(x)
  }

  hist_df <- stpd_build_logisih_pasquale(
    isi_sec = isi_sec,
    min_valid_isi_sec = min_valid_isi_sec,
    bin_width_log10 = bin_width_log10,
    lowess_span = lowess_span
  )

  n_valid <- sum(is.finite(isi_sec) & isi_sec >= min_valid_isi_sec & isi_sec > 0)
  if (nrow(hist_df) < 3L || n_valid < 3L) {
    return(list(
      method = "pasquale_logisi",
      threshold_sec = NA_real_,
      threshold_ms = NA_real_,
      threshold_status = "unresolved_too_few_valid_isi",
      n_valid_isi = n_valid,
      logisih = hist_df,
      peaks = data.frame(),
      valleys = data.frame()
    ))
  }

  y <- hist_df$smoothed_probability
  xx <- hist_df$bin_center_log10_ms
  peaks <- stpd_logisi_find_local_peaks(y, min_peak_distance = min_peak_distance)

  if (!length(peaks)) {
    return(list(
      method = "pasquale_logisi",
      threshold_sec = NA_real_,
      threshold_ms = NA_real_,
      threshold_status = "unresolved_no_peaks",
      n_valid_isi = n_valid,
      logisih = hist_df,
      peaks = data.frame(),
      valleys = data.frame()
    ))
  }

  peak_df <- data.frame(
    peak_index = peaks,
    peak_log10_ms = xx[peaks],
    peak_ms = 10^xx[peaks],
    peak_height = y[peaks]
  )

  peak_within <- peak_df[peak_df$peak_ms <= intraburst_peak_window_ms, , drop = FALSE]
  if (!nrow(peak_within)) {
    return(list(
      method = "pasquale_logisi",
      threshold_sec = NA_real_,
      threshold_ms = NA_real_,
      threshold_status = "unresolved_no_peak_below_100ms",
      n_valid_isi = n_valid,
      logisih = hist_df,
      peaks = peak_df,
      valleys = data.frame()
    ))
  }

  first_peak_row <- peak_within[which.max(peak_within$peak_height), , drop = FALSE]
  first_idx <- as.integer(first_peak_row$peak_index[1])

  following <- peak_df[peak_df$peak_index > first_idx, , drop = FALSE]
  if (!nrow(following)) {
    return(list(
      method = "pasquale_logisi",
      threshold_sec = NA_real_,
      threshold_ms = NA_real_,
      threshold_status = "unresolved_only_one_principal_peak",
      n_valid_isi = n_valid,
      logisih = hist_df,
      peaks = peak_df,
      first_peak = first_peak_row,
      valleys = data.frame()
    ))
  }

  candidates <- lapply(seq_len(nrow(following)), function(j) {
    p2 <- as.integer(following$peak_index[j])
    if (p2 <= first_idx + 1L) return(NULL)
    valley_idx_range <- seq.int(first_idx + 1L, p2 - 1L)
    if (!length(valley_idx_range)) return(NULL)
    valley_idx <- valley_idx_range[which.min(y[valley_idx_range])]
    y_min <- y[valley_idx]
    y1 <- y[first_idx]
    y2 <- y[p2]
    denom <- sqrt(max(y1, 0) * max(y2, 0))
    void <- if (is.finite(denom) && denom > 0) 1 - y_min / denom else NA_real_
    data.frame(
      first_peak_index = first_idx,
      second_peak_index = p2,
      valley_index = valley_idx,
      first_peak_ms = 10^xx[first_idx],
      second_peak_ms = 10^xx[p2],
      valley_ms = 10^xx[valley_idx],
      valley_sec = (10^xx[valley_idx]) / 1000,
      void_parameter = void,
      eligible = is.finite(void) && void >= void_threshold
    )
  })
  candidates <- candidates[!vapply(candidates, is.null, logical(1))]
  valley_df <- if (length(candidates)) do.call(rbind, candidates) else data.frame()

  if (!nrow(valley_df) || !any(valley_df$eligible)) {
    return(list(
      method = "pasquale_logisi",
      threshold_sec = NA_real_,
      threshold_ms = NA_real_,
      threshold_status = "unresolved_no_valley_void_ge_threshold",
      n_valid_isi = n_valid,
      logisih = hist_df,
      peaks = peak_df,
      first_peak = first_peak_row,
      valleys = valley_df
    ))
  }

  eligible <- valley_df[valley_df$eligible, , drop = FALSE]
  if (identical(valley_selection, "first_eligible")) {
    chosen <- eligible[order(eligible$second_peak_index, eligible$valley_index), ][1, , drop = FALSE]
  } else {
    chosen <- eligible[order(eligible$void_parameter, decreasing = TRUE), ][1, , drop = FALSE]
  }

  status <- "resolved"
  if (chosen$valley_sec[1] > max_reasonable_threshold_sec) {
    status <- "resolved_above_reasonable_threshold"
  }

  list(
    method = "pasquale_logisi",
    threshold_sec = chosen$valley_sec[1],
    threshold_ms = chosen$valley_ms[1],
    threshold_status = status,
    n_valid_isi = n_valid,
    logisih = hist_df,
    peaks = peak_df,
    first_peak = first_peak_row,
    valleys = valley_df,
    chosen_valley = chosen,
    void_threshold = void_threshold,
    intraburst_peak_window_ms = intraburst_peak_window_ms
  )
}

stpd_logisi_detect_runs_below <- function(isi_sec, threshold_sec, min_valid_isi_sec = 0.001) {
  isi_sec <- suppressWarnings(as.numeric(isi_sec))
  ok <- is.finite(isi_sec) & isi_sec >= min_valid_isi_sec & isi_sec < threshold_sec
  idx <- which(ok)
  if (!length(idx)) return(data.frame(start_isi = integer(), end_isi = integer()))
  starts <- idx[c(TRUE, diff(idx) > 1L)]
  ends <- idx[c(diff(idx) > 1L, TRUE)]
  data.frame(start_isi = starts, end_isi = ends)
}

stpd_logisi_burst_feature_table <- function(
  spike_times_sec,
  segments,
  method = "pasquale_newBD",
  threshold_sec = NA_real_,
  maxISI1_sec = NA_real_,
  maxISI2_sec = NA_real_,
  threshold_status = "",
  min_valid_isi_sec = 0.001
) {
  x <- sort(suppressWarnings(as.numeric(spike_times_sec)))
  x <- x[is.finite(x)]
  isi <- diff(x)
  if (!is.data.frame(segments) || !nrow(segments)) return(data.frame())

  rows <- lapply(seq_len(nrow(segments)), function(i) {
    s <- as.integer(segments$start_isi[i])
    e <- as.integer(segments$end_isi[i])
    if (!is.finite(s) || !is.finite(e) || s < 1L || e < s || e > length(isi)) return(NULL)
    seg <- isi[s:e]
    valid <- is.finite(seg) & seg >= min_valid_isi_sec
    if (!any(valid)) return(NULL)
    q90 <- as.numeric(stats::quantile(seg[valid], 0.90, na.rm = TRUE, names = FALSE))
    q95 <- as.numeric(stats::quantile(seg[valid], 0.95, na.rm = TRUE, names = FALSE))
    pre <- if (s > 1L) isi[s - 1L] else NA_real_
    post <- if (e < length(isi)) isi[e + 1L] else NA_real_
    n_sp <- e - s + 2L
    dur <- sum(seg[valid], na.rm = TRUE)
    data.frame(
      method = method,
      burst_id = i,
      start_isi = s,
      end_isi = e,
      start_spike = s,
      end_spike = e + 1L,
      start_time_sec = x[s],
      end_time_sec = x[e + 1L],
      n_isi = e - s + 1L,
      n_spikes = n_sp,
      duration_sec = dur,
      mean_ISI_sec = mean(seg[valid], na.rm = TRUE),
      median_ISI_sec = median(seg[valid], na.rm = TRUE),
      min_ISI_sec = min(seg[valid], na.rm = TRUE),
      max_ISI_sec = max(seg[valid], na.rm = TRUE),
      q90_ISI_sec = q90,
      q95_ISI_sec = q95,
      pre_ISI_sec = pre,
      post_ISI_sec = post,
      pre_core_ratio = if (is.finite(pre) && is.finite(q90) && q90 > 0) pre / q90 else NA_real_,
      post_core_ratio = if (is.finite(post) && is.finite(q90) && q90 > 0) post / q90 else NA_real_,
      edge_min = if (is.finite(pre) && is.finite(post) && is.finite(q90) && q90 > 0) min(pre / q90, post / q90) else NA_real_,
      edge_geom = if (is.finite(pre) && is.finite(post) && is.finite(q90) && q90 > 0) sqrt((pre / q90) * (post / q90)) else NA_real_,
      firing_rate_Hz = if (is.finite(dur) && dur > 0) n_sp / dur else NA_real_,
      ISIth_sec = threshold_sec,
      maxISI1_sec = maxISI1_sec,
      maxISI2_sec = maxISI2_sec,
      threshold_status = threshold_status,
      stringsAsFactors = FALSE
    )
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows)) do.call(rbind, rows) else data.frame()
}

stpd_detect_logisi_newBD_pasquale <- function(
  spike_times_sec,
  min_valid_isi_sec = 0.001,
  min_num_spikes = 5L,
  core_reference_sec = 0.100,
  max_reasonable_threshold_sec = 1.0,
  fallback_ch = TRUE,
  fallback_maxISI_sec = 0.100,
  multiple_core_mode = c("split_by_core", "join_loose_window"),
  ...
) {
  multiple_core_mode <- match.arg(multiple_core_mode)
  x <- sort(suppressWarnings(as.numeric(spike_times_sec)))
  x <- x[is.finite(x)]
  if (length(x) < min_num_spikes) {
    return(list(
      threshold = list(threshold_status = "unresolved_too_few_spikes"),
      bursts = data.frame(),
      core_segments = data.frame(),
      loose_segments = data.frame(),
      method_warning = "too_few_spikes"
    ))
  }
  isi <- diff(x)

  th <- stpd_estimate_logisi_threshold_pasquale(
    spike_times_sec = x,
    min_valid_isi_sec = min_valid_isi_sec,
    max_reasonable_threshold_sec = max_reasonable_threshold_sec,
    ...
  )

  use_fallback <- !is.finite(th$threshold_sec) || th$threshold_sec > max_reasonable_threshold_sec
  if (use_fallback && !fallback_ch) {
    return(list(
      threshold = th,
      bursts = data.frame(),
      core_segments = data.frame(),
      loose_segments = data.frame(),
      method_warning = "logISI_unresolved_no_fallback"
    ))
  }

  if (use_fallback) {
    maxISI1 <- fallback_maxISI_sec
    maxISI2 <- fallback_maxISI_sec
    extendFlag <- FALSE
    method <- "pasquale_newBD_fallback_CH"
  } else if (th$threshold_sec > core_reference_sec) {
    maxISI1 <- core_reference_sec
    maxISI2 <- th$threshold_sec
    extendFlag <- TRUE
    method <- "pasquale_newBD_two_threshold"
  } else {
    maxISI1 <- th$threshold_sec
    maxISI2 <- th$threshold_sec
    extendFlag <- FALSE
    method <- "pasquale_newBD_single_threshold"
  }

  cores <- stpd_logisi_detect_runs_below(isi, maxISI1, min_valid_isi_sec = min_valid_isi_sec)
  if (nrow(cores)) {
    cores$n_spikes <- cores$end_isi - cores$start_isi + 2L
    valid_cores <- cores[cores$n_spikes >= min_num_spikes, , drop = FALSE]
  } else {
    valid_cores <- data.frame(start_isi = integer(), end_isi = integer(), n_spikes = integer())
  }

  if (!nrow(valid_cores)) {
    return(list(
      threshold = th,
      bursts = data.frame(),
      core_segments = cores,
      loose_segments = data.frame(),
      method_warning = "no_valid_burst_core"
    ))
  }

  if (!extendFlag) {
    bursts <- stpd_logisi_burst_feature_table(
      spike_times_sec = x,
      segments = valid_cores[, c("start_isi", "end_isi"), drop = FALSE],
      method = method,
      threshold_sec = th$threshold_sec,
      maxISI1_sec = maxISI1,
      maxISI2_sec = maxISI2,
      threshold_status = stpd_logisi_chr1(th$threshold_status),
      min_valid_isi_sec = min_valid_isi_sec
    )
    return(list(
      threshold = th,
      bursts = bursts,
      core_segments = valid_cores,
      loose_segments = data.frame(),
      method_warning = ""
    ))
  }

  loose <- stpd_logisi_detect_runs_below(isi, maxISI2, min_valid_isi_sec = min_valid_isi_sec)
  if (!nrow(loose)) {
    return(list(
      threshold = th,
      bursts = data.frame(),
      core_segments = valid_cores,
      loose_segments = loose,
      method_warning = "no_loose_boundary_segments"
    ))
  }

  out_segments <- list()
  for (i in seq_len(nrow(loose))) {
    ls <- loose$start_isi[i]
    le <- loose$end_isi[i]
    inside <- valid_cores[valid_cores$start_isi >= ls & valid_cores$end_isi <= le, , drop = FALSE]
    if (!nrow(inside)) next
    if (nrow(inside) == 1L || identical(multiple_core_mode, "join_loose_window")) {
      out_segments[[length(out_segments) + 1L]] <- data.frame(start_isi = ls, end_isi = le)
    } else {
      for (j in seq_len(nrow(inside))) {
        out_segments[[length(out_segments) + 1L]] <- data.frame(
          start_isi = inside$start_isi[j],
          end_isi = inside$end_isi[j]
        )
      }
    }
  }

  segs <- if (length(out_segments)) unique(do.call(rbind, out_segments)) else data.frame(start_isi = integer(), end_isi = integer())

  bursts <- stpd_logisi_burst_feature_table(
    spike_times_sec = x,
    segments = segs,
    method = method,
    threshold_sec = th$threshold_sec,
    maxISI1_sec = maxISI1,
    maxISI2_sec = maxISI2,
    threshold_status = stpd_logisi_chr1(th$threshold_status),
    min_valid_isi_sec = min_valid_isi_sec
  )

  list(
    threshold = th,
    bursts = bursts,
    core_segments = valid_cores,
    loose_segments = loose,
    method_warning = if (!nrow(bursts)) "no_burst_after_boundary_logic" else ""
  )
}

stpd_logisi_support_dataset <- function(
  ds,
  params = default_params_sec(),
  selected_trains = NULL,
  min_valid_isi_sec = NULL,
  min_num_spikes = 5L,
  void_threshold = 0.7,
  intraburst_peak_window_ms = 100,
  core_reference_sec = 0.100,
  max_reasonable_threshold_sec = 1.0,
  fallback_ch = TRUE,
  fallback_maxISI_sec = 0.100,
  overlap_fraction = 0.10,
  ...
) {
  if (is.null(ds) || is.null(ds$trains)) stop("Dataset has no trains.", call. = FALSE)
  params <- params %||% default_params_sec()
  min_valid_isi_sec <- min_valid_isi_sec %||% (params$detector$min_valid_isi_sec %||% 0.001)
  trains <- selected_trains %||% names(ds$trains)
  trains <- intersect(trains, names(ds$trains))
  thresholds <- list(); bursts <- list(); hist_parts <- list(); peaks_parts <- list(); valleys_parts <- list(); summaries <- list()
  ledger <- ds$results$candidate_ledger %||% data.frame()

  for (tr in trains) {
    dat <- ds$trains[[tr]]
    x <- suppressWarnings(as.numeric(dat$timestamp_sec))
    res <- stpd_detect_logisi_newBD_pasquale(
      spike_times_sec = x,
      min_valid_isi_sec = min_valid_isi_sec,
      min_num_spikes = min_num_spikes,
      core_reference_sec = core_reference_sec,
      max_reasonable_threshold_sec = max_reasonable_threshold_sec,
      fallback_ch = fallback_ch,
      fallback_maxISI_sec = fallback_maxISI_sec,
      void_threshold = void_threshold,
      intraburst_peak_window_ms = intraburst_peak_window_ms,
      ...
    )
    th <- res$threshold
    threshold_row <- data.frame(
      train = tr,
      method = "pasquale_logisi_newBD",
      threshold_sec = stpd_logisi_num1(th$threshold_sec),
      threshold_ms = stpd_logisi_num1(th$threshold_ms),
      threshold_status = stpd_logisi_chr1(th$threshold_status),
      n_valid_isi = stpd_logisi_num1(th$n_valid_isi),
      void_threshold = stpd_logisi_num1(th$void_threshold, default = void_threshold),
      intraburst_peak_window_ms = stpd_logisi_num1(th$intraburst_peak_window_ms, default = intraburst_peak_window_ms),
      method_warning = stpd_logisi_chr1(res$method_warning),
      stringsAsFactors = FALSE
    )
    thresholds[[tr]] <- threshold_row

    if (is.data.frame(th$logisih) && nrow(th$logisih)) {
      h <- th$logisih
      h$train <- tr
      h$threshold_sec <- threshold_row$threshold_sec
      hist_parts[[tr]] <- h
    }
    if (is.data.frame(th$peaks) && nrow(th$peaks)) {
      pk <- th$peaks
      pk$train <- tr
      peaks_parts[[tr]] <- pk
    }
    if (is.data.frame(th$valleys) && nrow(th$valleys)) {
      vl <- th$valleys
      vl$train <- tr
      valleys_parts[[tr]] <- vl
    }

    b <- res$bursts
    if (!is.null(b) && nrow(b) > 0) {
      b$train <- tr
      bursts[[tr]] <- b
    }

    if (!is.null(b) && nrow(b) > 0) {
      isi <- diff(sort(suppressWarnings(as.numeric(x))))
      burst_isi_values <- unlist(lapply(seq_len(nrow(b)), function(i) {
        s <- as.integer(b$start_isi[i]); e <- as.integer(b$end_isi[i])
        if (length(isi) >= e && is.finite(s) && is.finite(e)) isi[s:e] else numeric(0)
      }), use.names = FALSE)
      burst_isi_values <- burst_isi_values[is.finite(burst_isi_values) & burst_isi_values >= min_valid_isi_sec]
      q <- function(p) if (length(burst_isi_values) > 0) as.numeric(stats::quantile(burst_isi_values, p, na.rm = TRUE, type = 7)) else NA_real_
      overlap_n <- stpd_overlap_count_misi_detector_candidates(b, ledger, tr, min_overlap_fraction = overlap_fraction)
      n_b <- nrow(b)
      status <- as.character(threshold_row$threshold_status[1])
      support_level <- if (!grepl("resolved", status)) "unresolved" else if (is.finite(overlap_n) && overlap_n >= max(1L, ceiling(0.5 * n_b))) "strong" else if (is.finite(overlap_n) && overlap_n > 0) "moderate" else if (n_b > 0) "logisi_only" else "weak"
      suggestion_status <- if (support_level %in% c("strong", "moderate")) "suggestion_available_review_required" else if (support_level == "logisi_only") "logisi_only_review_required" else support_level
      summaries[[tr]] <- data.frame(
        train = tr,
        method = "pasquale_logisi_newBD",
        threshold_sec = threshold_row$threshold_sec,
        threshold_status = status,
        n_logisi_bursts = n_b,
        n_burst_isi = length(burst_isi_values),
        burst_isi_q50_sec = q(0.50),
        burst_isi_q90_sec = q(0.90),
        burst_isi_q95_sec = q(0.95),
        burst_isi_max_sec = if (length(burst_isi_values) > 0) max(burst_isi_values, na.rm = TRUE) else NA_real_,
        mean_n_spikes = mean(b$n_spikes, na.rm = TRUE),
        mean_duration_sec = mean(b$duration_sec, na.rm = TRUE),
        overlap_with_detector_burst_candidates = overlap_n,
        support_level = support_level,
        suggested_burst_max_ISI_sec = q(0.90),
        suggestion_status = suggestion_status,
        method_warning = res$method_warning %||% "",
        stringsAsFactors = FALSE
      )
    } else {
      summaries[[tr]] <- data.frame(
        train = tr,
        method = "pasquale_logisi_newBD",
        threshold_sec = threshold_row$threshold_sec,
        threshold_status = threshold_row$threshold_status,
        n_logisi_bursts = 0L,
        n_burst_isi = 0L,
        burst_isi_q50_sec = NA_real_, burst_isi_q90_sec = NA_real_, burst_isi_q95_sec = NA_real_, burst_isi_max_sec = NA_real_,
        mean_n_spikes = NA_real_, mean_duration_sec = NA_real_,
        overlap_with_detector_burst_candidates = stpd_overlap_count_misi_detector_candidates(data.frame(), ledger, tr, min_overlap_fraction = overlap_fraction),
        support_level = if (grepl("resolved", threshold_row$threshold_status)) "no_candidates" else "unresolved",
        suggested_burst_max_ISI_sec = NA_real_,
        suggestion_status = "no_suggestion",
        method_warning = res$method_warning %||% "",
        stringsAsFactors = FALSE
      )
    }
  }

  list(
    thresholds = if (length(thresholds)) dplyr::bind_rows(thresholds) else data.frame(),
    bursts = if (length(bursts)) dplyr::bind_rows(bursts) else data.frame(),
    logisih = if (length(hist_parts)) dplyr::bind_rows(hist_parts) else data.frame(),
    peaks = if (length(peaks_parts)) dplyr::bind_rows(peaks_parts) else data.frame(),
    valleys = if (length(valleys_parts)) dplyr::bind_rows(valleys_parts) else data.frame(),
    support_report = if (length(summaries)) dplyr::bind_rows(summaries) else data.frame()
  )
}

stpd_logisi_support_export <- function(support, out_dir) {
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  if (!is.null(support$thresholds) && nrow(support$thresholds) > 0) write_csv_safe(support$thresholds, file.path(out_dir, "LogISI_thresholds.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  if (!is.null(support$bursts) && nrow(support$bursts) > 0) write_csv_safe(support$bursts, file.path(out_dir, "LogISI_burst_candidates.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  if (!is.null(support$logisih) && nrow(support$logisih) > 0) write_csv_safe(support$logisih, file.path(out_dir, "LogISI_logISIH.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  if (!is.null(support$peaks) && nrow(support$peaks) > 0) write_csv_safe(support$peaks, file.path(out_dir, "LogISI_peaks.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  if (!is.null(support$valleys) && nrow(support$valleys) > 0) write_csv_safe(support$valleys, file.path(out_dir, "LogISI_valleys.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  if (!is.null(support$support_report) && nrow(support$support_report) > 0) write_csv_safe(support$support_report, file.path(out_dir, "Burst_threshold_support_LogISI.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  invisible(out_dir)
}
