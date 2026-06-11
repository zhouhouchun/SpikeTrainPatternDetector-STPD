
# canonical native wrappers for structure scanning and interval overlap.

compute_isi_percentiles <- compute_isi_percentiles_vector

stpd_native_local_window_hard_max <- function() 10001L
stpd_native_core_width_hard_max <- function() 10000L

stpd_native_int_arg <- function(x, default) {
  if (is.null(x) || length(x) == 0) return(as.integer(default))
  y <- suppressWarnings(as.numeric(x)[1])
  if (!is.finite(y)) return(as.integer(default))
  y <- round(y)
  if (y > .Machine$integer.max) return(.Machine$integer.max)
  if (y < -.Machine$integer.max) return(-.Machine$integer.max)
  as.integer(y)
}

stpd_structure_scan_empty <- function() {
  tibble::tibble(
    start_isi = integer(),
    end_isi = integer(),
    core_q90_ISI_sec = numeric(),
    core_max_pct = numeric(),
    edge_contrast_min = numeric(),
    edge_contrast_geom = numeric()
  )
}

stpd_bounded_local_window <- function(window, n, hard_max = stpd_native_local_window_hard_max()) {
  window <- stpd_native_int_arg(window, 11L)
  n <- safe_int(n, 0L)
  hard_max <- max(3L, safe_int(hard_max, 10001L))
  if (hard_max %% 2L == 0L) hard_max <- hard_max - 1L
  if (window < 3L) window <- 3L
  if (n > 0L) window <- min(window, max(3L, n))
  window <- min(window, hard_max)
  if (window %% 2L == 0L) window <- window + 1L
  min(window, hard_max)
}

compute_local_median_cache <- function(isi_sec, window = 11L, min_isi_sec = 0.001) {
  isi_sec <- as.numeric(isi_sec)
  window <- stpd_bounded_local_window(window, length(isi_sec))
  native <- tryCatch(.Call("stpd_local_median_cache_c", isi_sec, as.integer(window), as.numeric(min_isi_sec)), error = function(e) NULL)
  if (is.numeric(native) && length(native) == length(isi_sec)) return(native)
  dat <- data.frame(ISI_sec = isi_sec)
  ensure_train_local_median_cache(dat, window = window, min_isi_sec = min_isi_sec, force = TRUE)$local_median_ISI_sec
}

scan_structure_candidates <- function(isi_sec, isi_pct = NULL, min_core_isi_n = 2L, max_core_isi_n = 8L,
                                      core_q90_max_sec = Inf, core_pct_max = 35,
                                      edge_min = 1.25, edge_geom = 1.35,
                                      min_isi_sec = 0.001) {
  isi_sec <- as.numeric(isi_sec)
  n <- length(isi_sec)
  min_core_isi_n <- max(1L, stpd_native_int_arg(min_core_isi_n, 2L))
  max_core_isi_n <- max(min_core_isi_n, stpd_native_int_arg(max_core_isi_n, 8L))
  max_possible <- max(0L, n - 2L)
  if (max_possible < min_core_isi_n) return(stpd_structure_scan_empty())
  max_core_isi_n <- min(max_core_isi_n, max_possible)
  hard_max <- stpd_native_core_width_hard_max()
  if (min_core_isi_n > hard_max || max_core_isi_n > hard_max) {
    stop("scan_structure_candidates(): core ISI window is too large for native scanning.", call. = FALSE)
  }
  if (is.null(isi_pct)) {
    isi_pct <- compute_isi_percentiles(isi_sec, min_isi_sec = min_isi_sec)
  } else {
    isi_pct <- as.numeric(isi_pct)
    if (length(isi_pct) != length(isi_sec)) {
      stop("scan_structure_candidates(): isi_pct must have the same length as isi_sec.", call. = FALSE)
    }
  }
  native <- tryCatch(.Call("stpd_structure_scan_c", isi_sec, as.numeric(isi_pct), as.integer(min_core_isi_n), as.integer(max_core_isi_n), as.numeric(core_q90_max_sec), as.numeric(core_pct_max), as.numeric(edge_min), as.numeric(edge_geom), as.numeric(min_isi_sec)), error = function(e) NULL)
  if (is.list(native) && all(c("start_isi", "end_isi") %in% names(native))) return(tibble::as_tibble(native))
  # R fallback
  rows <- list()
  for (w in seq.int(min_core_isi_n, max_core_isi_n)) {
    for (s in seq.int(2L, max(2L, n - w))) {
      e <- s + w - 1L
      if (e >= n) next
      core <- isi_sec[s:e]
      core <- core[is.finite(core) & core >= min_isi_sec]
      if (length(core) == 0) next
      q90 <- as.numeric(stats::quantile(core, 0.9, na.rm = TRUE, names = FALSE, type = 7))
      pct <- max(isi_pct[s:e], na.rm = TRUE)
      pre <- isi_sec[s - 1L]; post <- isi_sec[e + 1L]
      if (!is.finite(q90) || q90 <= 0 || !is.finite(pre) || !is.finite(post)) next
      emin <- min(pre / q90, post / q90)
      egeom <- sqrt((pre / q90) * (post / q90))
      if ((q90 <= core_q90_max_sec || pct <= core_pct_max) && emin >= edge_min && egeom >= edge_geom) {
        rows[[length(rows)+1L]] <- data.frame(start_isi=s, end_isi=e, core_q90_ISI_sec=q90, core_max_pct=pct, edge_contrast_min=emin, edge_contrast_geom=egeom)
      }
    }
  }
  if (length(rows) == 0) stpd_structure_scan_empty() else dplyr::bind_rows(rows)
}

interval_best_overlap <- function(query_start, query_end, target_start, target_end) {
  query_start <- as.integer(query_start)
  query_end <- as.integer(query_end)
  target_start <- as.integer(target_start)
  target_end <- as.integer(target_end)
  if (length(query_start) != length(query_end)) {
    stop("interval_best_overlap(): query_start and query_end must have the same length.", call. = FALSE)
  }
  if (length(target_start) != length(target_end)) {
    stop("interval_best_overlap(): target_start and target_end must have the same length.", call. = FALSE)
  }
  native <- tryCatch(.Call("stpd_interval_best_overlap_c", query_start, query_end, target_start, target_end), error = function(e) NULL)
  if (is.list(native) && all(c("best_index", "overlap", "iou") %in% names(native))) return(tibble::as_tibble(native))
  out <- lapply(seq_along(query_start), function(i) {
    ov <- pmax(0L, pmin(query_end[i], target_end) - pmax(query_start[i], target_start) + 1L)
    union <- pmax(query_end[i], target_end) - pmin(query_start[i], target_start) + 1L
    iou <- ifelse(union > 0, ov / union, 0)
    j <- if (length(ov) == 0 || max(ov, na.rm = TRUE) <= 0) NA_integer_ else which.max(iou)
    data.frame(best_index = ifelse(is.na(j), NA_integer_, j), overlap = ifelse(is.na(j), 0L, ov[j]), iou = ifelse(is.na(j), NA_real_, iou[j]))
  })
  dplyr::bind_rows(out)
}
