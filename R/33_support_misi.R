# ============================================================
# Mean-ISI burst threshold support layer
# ============================================================
# This module implements a literature-conformant support method based on
# Chen et al. (2009), "Detection of bursts in neuronal spike trains by the
# mean inter-spike interval method". It is intentionally implemented as a
# SUPPORT layer: it estimates train-wise ML thresholds and candidate burst
# windows to guide threshold calibration, but it does not write AUTO labels
# and does not replace the main STPD detector.

stpd_merge_isi_ranges <- function(starts, ends) {
  if (length(starts) == 0L || length(ends) == 0L) {
    return(data.frame(start_isi = integer(), end_isi = integer()))
  }
  starts <- as.integer(starts)
  ends <- as.integer(ends)
  ok <- is.finite(starts) & is.finite(ends) & ends >= starts
  starts <- starts[ok]
  ends <- ends[ok]
  if (length(starts) == 0L) {
    return(data.frame(start_isi = integer(), end_isi = integer()))
  }
  ord <- order(starts, ends)
  starts <- starts[ord]
  ends <- ends[ord]
  out_s <- integer()
  out_e <- integer()
  cur_s <- starts[1L]
  cur_e <- ends[1L]
  if (length(starts) >= 2L) {
    for (i in seq.int(2L, length(starts))) {
      if (starts[i] <= cur_e + 1L) {
        cur_e <- max(cur_e, ends[i])
      } else {
        out_s <- c(out_s, cur_s)
        out_e <- c(out_e, cur_e)
        cur_s <- starts[i]
        cur_e <- ends[i]
      }
    }
  }
  out_s <- c(out_s, cur_s)
  out_e <- c(out_e, cur_e)
  data.frame(start_isi = out_s, end_isi = out_e, stringsAsFactors = FALSE)
}

stpd_estimate_misi_threshold <- function(spike_times_sec, min_valid_isi_sec = 0.001) {
  x <- suppressWarnings(as.numeric(spike_times_sec))
  x <- sort(x[is.finite(x)])
  n_spikes <- length(x)
  n_isi <- max(0L, n_spikes - 1L)
  if (n_spikes < 3L) {
    return(data.frame(
      method = "mean_isi_article",
      threshold_sec = NA_real_, threshold_status = "unresolved_too_few_spikes",
      mean_isi_sec = NA_real_, ML_sec = NA_real_,
      n_spikes = n_spikes, n_isi = n_isi, n_valid_isi = 0L, n_L = 0L,
      stringsAsFactors = FALSE
    ))
  }
  isi <- diff(x)
  valid <- is.finite(isi) & isi >= min_valid_isi_sec
  n_valid <- sum(valid, na.rm = TRUE)
  if (n_valid < 2L) {
    return(data.frame(
      method = "mean_isi_article",
      threshold_sec = NA_real_, threshold_status = "unresolved_too_few_valid_isi",
      mean_isi_sec = NA_real_, ML_sec = NA_real_,
      n_spikes = n_spikes, n_isi = n_isi, n_valid_isi = n_valid, n_L = 0L,
      stringsAsFactors = FALSE
    ))
  }
  valid_isi <- isi[valid]
  mean_all <- mean(valid_isi, na.rm = TRUE)
  L <- valid_isi[valid_isi < mean_all]
  if (!is.finite(mean_all) || length(L) < 1L) {
    return(data.frame(
      method = "mean_isi_article",
      threshold_sec = NA_real_, threshold_status = "unresolved_empty_L_sequence",
      mean_isi_sec = mean_all, ML_sec = NA_real_,
      n_spikes = n_spikes, n_isi = n_isi, n_valid_isi = n_valid, n_L = length(L),
      stringsAsFactors = FALSE
    ))
  }
  ML <- mean(L, na.rm = TRUE)
  status <- if (is.finite(ML) && ML > 0) "resolved" else "unresolved_invalid_ML"
  data.frame(
    method = "mean_isi_article",
    threshold_sec = if (identical(status, "resolved")) ML else NA_real_,
    threshold_status = status,
    mean_isi_sec = mean_all,
    ML_sec = ML,
    n_spikes = n_spikes,
    n_isi = n_isi,
    n_valid_isi = n_valid,
    n_L = length(L),
    stringsAsFactors = FALSE
  )
}

stpd_detect_misi_bursts_article <- function(
  spike_times_sec,
  min_valid_isi_sec = 0.001,
  min_isi_count = 2L,
  max_isi_count = Inf,
  max_windows = 2000000L,
  min_spikes = 3L,
  min_duration_sec = 0,
  collapse_exact_duplicates = FALSE
) {
  x <- suppressWarnings(as.numeric(spike_times_sec))
  x <- sort(x[is.finite(x)])
  if (collapse_exact_duplicates) x <- unique(x)
  thr <- stpd_estimate_misi_threshold(x, min_valid_isi_sec = min_valid_isi_sec)
  empty <- data.frame()
  if (!identical(as.character(thr$threshold_status[1]), "resolved")) {
    return(list(threshold = thr, bursts = empty, windows = empty, isi_table = empty,
                method_warning = as.character(thr$threshold_status[1])))
  }
  isi <- diff(x)
  n_isi <- length(isi)
  valid <- is.finite(isi) & isi >= min_valid_isi_sec
  min_isi_count <- max(2L, as.integer(min_isi_count))
  if (!is.finite(max_isi_count) || max_isi_count <= 0) max_isi_count <- n_isi
  max_isi_count <- min(as.integer(max_isi_count), n_isi)
  if (n_isi < min_isi_count) {
    return(list(threshold = thr, bursts = empty, windows = empty,
                isi_table = data.frame(isi_index = seq_along(isi), ISI_sec = isi, valid_ISI = valid),
                method_warning = "too_few_isi"))
  }
  # Guard UI usage for very long trains. If the requested complete search would
  # exceed max_windows, truncate the maximum window length and report it.
  requested_max_k <- max_isi_count
  approx_windows <- sum(pmax(0L, n_isi - seq.int(min_isi_count, max_isi_count) + 1L))
  search_status <- "complete"
  if (is.finite(max_windows) && approx_windows > max_windows) {
    # Choose the largest k that stays under the approximate budget.
    budget <- as.integer(max_windows)
    total <- 0L
    max_k_new <- min_isi_count - 1L
    for (k in seq.int(min_isi_count, max_isi_count)) {
      add <- max(0L, n_isi - k + 1L)
      if (total + add > budget) break
      total <- total + add
      max_k_new <- k
    }
    max_isi_count <- max(min_isi_count, max_k_new)
    search_status <- paste0("truncated_at_k_", max_isi_count, "_of_", requested_max_k)
  }
  ML <- as.numeric(thr$ML_sec[1])
  cs_isi <- c(0, cumsum(ifelse(valid, isi, 0)))
  cs_valid <- c(0, cumsum(as.integer(valid)))
  win_starts <- integer()
  win_ends <- integer()
  win_k <- integer()
  win_mean <- numeric()
  for (k in seq.int(min_isi_count, max_isi_count)) {
    starts <- seq.int(1L, n_isi - k + 1L)
    ends <- starts + k - 1L
    sum_isi <- cs_isi[ends + 1L] - cs_isi[starts]
    n_valid_window <- cs_valid[ends + 1L] - cs_valid[starts]
    ok <- n_valid_window == k & (sum_isi / k) <= ML
    if (any(ok)) {
      win_starts <- c(win_starts, starts[ok])
      win_ends <- c(win_ends, ends[ok])
      win_k <- c(win_k, rep(k, sum(ok)))
      win_mean <- c(win_mean, sum_isi[ok] / k)
    }
  }
  isi_table <- data.frame(
    isi_index = seq_along(isi),
    ISI_sec = isi,
    valid_ISI = valid,
    below_ML = valid & isi <= ML,
    stringsAsFactors = FALSE
  )
  if (length(win_starts) == 0L) {
    return(list(threshold = thr, bursts = empty,
                windows = data.frame(), isi_table = isi_table,
                method_warning = paste0("no_window_mean_below_ML;", search_status)))
  }
  merged <- stpd_merge_isi_ranges(win_starts, win_ends)
  bursts <- lapply(seq_len(nrow(merged)), function(j) {
    s_isi <- as.integer(merged$start_isi[j])
    e_isi <- as.integer(merged$end_isi[j])
    seg_isi <- isi[s_isi:e_isi]
    seg_valid <- valid[s_isi:e_isi]
    start_spike <- s_isi
    end_spike <- e_isi + 1L
    duration <- sum(seg_isi[seg_valid], na.rm = TRUE)
    n_isi_seg <- e_isi - s_isi + 1L
    n_spikes_seg <- n_isi_seg + 1L
    data.frame(
      burst_id = j,
      method = "mean_isi_article",
      start_isi = s_isi,
      end_isi = e_isi,
      start_spike = start_spike,
      end_spike = end_spike,
      start_time_sec = x[start_spike],
      end_time_sec = x[end_spike],
      n_isi = n_isi_seg,
      n_spikes = n_spikes_seg,
      duration_sec = duration,
      mean_ISI_sec = mean(seg_isi[seg_valid], na.rm = TRUE),
      median_ISI_sec = median(seg_isi[seg_valid], na.rm = TRUE),
      q90_ISI_sec = as.numeric(stats::quantile(seg_isi[seg_valid], 0.90, na.rm = TRUE, type = 7)),
      q95_ISI_sec = as.numeric(stats::quantile(seg_isi[seg_valid], 0.95, na.rm = TRUE, type = 7)),
      max_ISI_sec = max(seg_isi[seg_valid], na.rm = TRUE),
      min_ISI_sec = min(seg_isi[seg_valid], na.rm = TRUE),
      ML_sec = as.numeric(thr$ML_sec[1]),
      mean_all_ISI_sec = as.numeric(thr$mean_isi_sec[1]),
      threshold_status = as.character(thr$threshold_status[1]),
      search_status = search_status,
      pre_ISI_sec = if (s_isi > 1L) isi[s_isi - 1L] else NA_real_,
      post_ISI_sec = if (e_isi < n_isi) isi[e_isi + 1L] else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  bursts <- if (length(bursts) > 0) do.call(rbind, bursts) else data.frame()
  if (nrow(bursts) > 0) {
    keep <- bursts$n_spikes >= min_spikes
    if (is.finite(min_duration_sec) && min_duration_sec > 0) {
      keep <- keep & bursts$duration_sec >= min_duration_sec
    }
    bursts <- bursts[keep, , drop = FALSE]
  }
  windows <- data.frame(
    start_isi = win_starts,
    end_isi = win_ends,
    n_isi = win_k,
    mean_ISI_sec = win_mean,
    ML_sec = ML,
    stringsAsFactors = FALSE
  )
  list(
    threshold = thr,
    bursts = bursts,
    windows = windows,
    isi_table = isi_table,
    method_warning = if (nrow(bursts) == 0) paste0("no_burst_after_filters;", search_status) else search_status
  )
}

stpd_overlap_count_misi_detector_candidates <- function(misi_bursts, ledger, train, min_overlap_fraction = 0.10) {
  if (is.null(misi_bursts) || nrow(misi_bursts) == 0) return(0L)
  if (is.null(ledger) || nrow(ledger) == 0) return(NA_integer_)
  lg <- ledger
  if ("train" %in% names(lg)) lg <- lg[as.character(lg$train) == train, , drop = FALSE]
  if (nrow(lg) == 0) return(0L)
  cls <- as.character(lg$final_candidate_class %||% lg$raw_candidate_class %||% "")
  src <- as.character(lg$candidate_source %||% "")
  keep <- cls %in% c("burst", "long_burst", "possible_burst") | grepl("burst", src, ignore.case = TRUE)
  lg <- lg[keep, , drop = FALSE]
  if (nrow(lg) == 0) return(0L)
  count <- 0L
  for (i in seq_len(nrow(misi_bursts))) {
    a0 <- as.numeric(misi_bursts$start_time_sec[i]); a1 <- as.numeric(misi_bursts$end_time_sec[i])
    if (!is.finite(a0) || !is.finite(a1) || a1 <= a0) next
    best <- 0
    for (j in seq_len(nrow(lg))) {
      b0 <- as.numeric(lg$start_time_sec[j]); b1 <- as.numeric(lg$end_time_sec[j])
      if (!is.finite(b0) || !is.finite(b1) || b1 <= b0) next
      ov <- max(0, min(a1, b1) - max(a0, b0))
      denom <- min(a1 - a0, b1 - b0)
      frac <- if (denom > 0) ov / denom else 0
      best <- max(best, frac)
    }
    if (best >= min_overlap_fraction) count <- count + 1L
  }
  count
}

stpd_misi_support_dataset <- function(
  ds,
  params = default_params_sec(),
  selected_trains = NULL,
  min_valid_isi_sec = NULL,
  min_isi_count = 2L,
  max_isi_count = Inf,
  max_windows = 2000000L,
  min_spikes = 3L,
  min_duration_sec = 0,
  overlap_fraction = 0.10
) {
  if (is.null(ds) || is.null(ds$trains)) stop("Dataset has no trains.", call. = FALSE)
  params <- params %||% default_params_sec()
  min_valid_isi_sec <- min_valid_isi_sec %||% (params$detector$min_valid_isi_sec %||% 0.001)
  trains <- selected_trains %||% names(ds$trains)
  trains <- intersect(trains, names(ds$trains))
  thresholds <- list(); bursts <- list(); windows <- list(); summaries <- list()
  ledger <- ds$results$candidate_ledger %||% data.frame()
  for (tr in trains) {
    dat <- ds$trains[[tr]]
    x <- suppressWarnings(as.numeric(dat$timestamp_sec))
    res <- stpd_detect_misi_bursts_article(
      x,
      min_valid_isi_sec = min_valid_isi_sec,
      min_isi_count = min_isi_count,
      max_isi_count = max_isi_count,
      max_windows = max_windows,
      min_spikes = min_spikes,
      min_duration_sec = min_duration_sec,
      collapse_exact_duplicates = FALSE
    )
    th <- res$threshold
    th$train <- tr
    th$method_warning <- res$method_warning %||% ""
    thresholds[[tr]] <- th
    b <- res$bursts
    if (!is.null(b) && nrow(b) > 0) {
      b$train <- tr
      bursts[[tr]] <- b
    }
    w <- res$windows
    if (!is.null(w) && nrow(w) > 0) {
      w$train <- tr
      windows[[tr]] <- w
    }
    if (!is.null(b) && nrow(b) > 0) {
      burst_isi_values <- unlist(lapply(seq_len(nrow(b)), function(i) {
        s <- as.integer(b$start_isi[i]); e <- as.integer(b$end_isi[i])
        isi <- diff(x)
        if (length(isi) >= e) isi[s:e] else numeric(0)
      }), use.names = FALSE)
      burst_isi_values <- burst_isi_values[is.finite(burst_isi_values) & burst_isi_values >= min_valid_isi_sec]
      q <- function(p) if (length(burst_isi_values) > 0) as.numeric(stats::quantile(burst_isi_values, p, na.rm = TRUE, type = 7)) else NA_real_
      overlap_n <- stpd_overlap_count_misi_detector_candidates(b, ledger, tr, min_overlap_fraction = overlap_fraction)
      n_b <- nrow(b)
      status <- as.character(th$threshold_status[1])
      support_level <- if (!identical(status, "resolved")) "unresolved" else if (is.finite(overlap_n) && overlap_n >= max(1L, ceiling(0.5 * n_b))) "strong" else if (is.finite(overlap_n) && overlap_n > 0) "moderate" else if (n_b > 0) "misi_only" else "weak"
      suggestion_status <- if (support_level %in% c("strong", "moderate")) "suggestion_available_review_required" else if (support_level == "misi_only") "misi_only_review_required" else support_level
      summaries[[tr]] <- data.frame(
        train = tr,
        method = "mean_isi_article",
        threshold_sec = as.numeric(th$threshold_sec[1]),
        threshold_status = status,
        n_misi_bursts = n_b,
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
        method = "mean_isi_article",
        threshold_sec = as.numeric(th$threshold_sec[1]),
        threshold_status = as.character(th$threshold_status[1]),
        n_misi_bursts = 0L,
        n_burst_isi = 0L,
        burst_isi_q50_sec = NA_real_, burst_isi_q90_sec = NA_real_, burst_isi_q95_sec = NA_real_, burst_isi_max_sec = NA_real_,
        mean_n_spikes = NA_real_, mean_duration_sec = NA_real_,
        overlap_with_detector_burst_candidates = stpd_overlap_count_misi_detector_candidates(data.frame(), ledger, tr, min_overlap_fraction = overlap_fraction),
        support_level = if (identical(as.character(th$threshold_status[1]), "resolved")) "no_candidates" else "unresolved",
        suggested_burst_max_ISI_sec = NA_real_,
        suggestion_status = "no_suggestion",
        method_warning = res$method_warning %||% "",
        stringsAsFactors = FALSE
      )
    }
  }
  list(
    thresholds = if (length(thresholds) > 0) dplyr::bind_rows(thresholds) else data.frame(),
    bursts = if (length(bursts) > 0) dplyr::bind_rows(bursts) else data.frame(),
    windows = if (length(windows) > 0) dplyr::bind_rows(windows) else data.frame(),
    support_report = if (length(summaries) > 0) dplyr::bind_rows(summaries) else data.frame()
  )
}

stpd_misi_support_export <- function(support, out_dir) {
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  if (!is.null(support$thresholds) && nrow(support$thresholds) > 0) write_csv_safe(support$thresholds, file.path(out_dir, "MISI_thresholds.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  if (!is.null(support$bursts) && nrow(support$bursts) > 0) write_csv_safe(support$bursts, file.path(out_dir, "MISI_burst_candidates.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  if (!is.null(support$windows) && nrow(support$windows) > 0) write_csv_safe(support$windows, file.path(out_dir, "MISI_qualifying_windows.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  if (!is.null(support$support_report) && nrow(support$support_report) > 0) write_csv_safe(support$support_report, file.path(out_dir, "Burst_threshold_support_MISI.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  invisible(out_dir)
}
