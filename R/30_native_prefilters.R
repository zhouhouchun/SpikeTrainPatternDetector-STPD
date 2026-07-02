# engine native prefilter wrappers for short-ISI runs. These are performance
# building blocks for future local-compression and high-frequency detectors.

scan_short_isi_runs <- function(isi_sec, isi_pct = NULL, max_abs_sec = Inf, max_pct = Inf,
                                min_run_isi_n = 2L, min_isi_sec = 0.001,
                                gate = c("either", "both")) {
  gate <- match.arg(gate)
  isi_sec <- as.numeric(isi_sec)
  if (is.null(isi_pct)) {
    isi_pct <- compute_isi_percentiles(isi_sec, min_isi_sec = min_isi_sec)
  } else {
    isi_pct <- as.numeric(isi_pct)
    if (length(isi_pct) != length(isi_sec)) {
      stop("scan_short_isi_runs(): isi_pct must have the same length as isi_sec.", call. = FALSE)
    }
  }
  native <- tryCatch(.Call("stpd_short_runs_c", isi_sec, as.numeric(isi_pct), as.numeric(max_abs_sec), as.numeric(max_pct), as.integer(min_run_isi_n), as.numeric(min_isi_sec), as.integer(ifelse(gate == "both", 1L, 0L))), error = function(e) NULL)
  if (is.list(native) && all(c("start_isi", "end_isi") %in% names(native))) return(tibble::as_tibble(native))
  ok_abs <- is.finite(isi_sec) & isi_sec >= min_isi_sec & isi_sec <= max_abs_sec
  ok_pct <- is.finite(isi_pct) & isi_pct <= max_pct
  ok <- if (gate == "both") ok_abs & ok_pct else ok_abs | ok_pct
  ok[is.na(ok)] <- FALSE
  runs <- list(); ii <- 1L
  while (ii <= length(ok)) {
    if (!ok[ii]) { ii <- ii + 1L; next }
    jj <- ii
    while (jj <= length(ok) && ok[jj]) jj <- jj + 1L
    e <- jj - 1L
    if ((e - ii + 1L) >= min_run_isi_n) {
      vals <- isi_sec[ii:e]
      pcts <- isi_pct[ii:e]
      runs[[length(runs) + 1L]] <- data.frame(start_isi = ii, end_isi = e, n_isi = e - ii + 1L, mean_ISI_sec = mean(vals, na.rm = TRUE), max_ISI_sec = max(vals, na.rm = TRUE), mean_ISI_pct = mean(pcts, na.rm = TRUE), max_ISI_pct = max(pcts, na.rm = TRUE))
    }
    ii <- jj
  }
  if (length(runs) == 0) tibble::tibble(start_isi = integer(), end_isi = integer(), n_isi = integer(), mean_ISI_sec = numeric(), max_ISI_sec = numeric(), mean_ISI_pct = numeric(), max_ISI_pct = numeric()) else dplyr::bind_rows(runs)
}
