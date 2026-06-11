# Auto-generated modular extraction from modular reference script.
# Do not edit generated function names blindly; use git for version history.

# ============================================================
# Parameter suggestion modules
# ============================================================

estimate_mean_isi_threshold_train <- function(isi_sec, min_isi_sec = 0.001) {
  x <- as.numeric(isi_sec)
  x <- x[is.finite(x) & x >= min_isi_sec]
  if (length(x) < 5) return(NA_real_)
  m <- mean(x)
  L <- x[x < m]
  if (length(L) == 0) return(NA_real_)
  mean(L)
}

find_local_extrema <- function(y) {
  n <- length(y)
  maxima <- integer(0)
  minima <- integer(0)
  if (n < 3) return(list(maxima = maxima, minima = minima))
  for (i in 2:(n - 1)) {
    if (y[i] >= y[i - 1] && y[i] > y[i + 1]) maxima <- c(maxima, i)
    if (y[i] <= y[i - 1] && y[i] < y[i + 1]) minima <- c(minima, i)
  }
  list(maxima = maxima, minima = minima)
}

estimate_logisi_threshold_train_result <- function(isi_sec,
                                                   min_isi_sec = 0.001,
                                                   mcv_sec = 0.1,
                                                   bin_width_log10 = 0.1,
                                                   lowess_span = 0.12,
                                                   void_threshold = 0.7) {
  x <- as.numeric(isi_sec)
  x <- x[is.finite(x) & x >= min_isi_sec & x > 0]
  mcv_sec <- suppressWarnings(as.numeric(mcv_sec))
  if (!is.finite(mcv_sec) || mcv_sec <= 0) mcv_sec <- 0.1
  if (length(x) < 10) {
    return(list(
      method = "pasquale_logisi",
      threshold_sec = NA_real_,
      raw_threshold_sec = NA_real_,
      threshold_status = "unresolved_too_few_valid_isi",
      accepted = FALSE,
      n_valid_isi = length(x)
    ))
  }

  th <- tryCatch(
    stpd_estimate_logisi_threshold_pasquale(
      isi_sec = x,
      min_valid_isi_sec = min_isi_sec,
      bin_width_log10 = bin_width_log10,
      lowess_span = lowess_span,
      intraburst_peak_window_ms = mcv_sec * 1000,
      void_threshold = void_threshold,
      max_reasonable_threshold_sec = mcv_sec
    ),
    error = function(e) list(
      method = "pasquale_logisi",
      threshold_sec = NA_real_,
      threshold_status = paste0("error:", conditionMessage(e)),
      n_valid_isi = length(x)
    )
  )

  raw_threshold <- suppressWarnings(as.numeric(th$threshold_sec %||% NA_real_))[1]
  status <- as.character(th$threshold_status %||% "threshold_unresolved")[1]
  accepted <- identical(status, "resolved") && is.finite(raw_threshold) && raw_threshold <= mcv_sec
  list(
    method = as.character(th$method %||% "pasquale_logisi")[1],
    threshold_sec = if (accepted) raw_threshold else NA_real_,
    raw_threshold_sec = raw_threshold,
    threshold_ms = suppressWarnings(as.numeric(th$threshold_ms %||% NA_real_))[1],
    threshold_status = if (accepted) "resolved" else status,
    accepted = accepted,
    n_valid_isi = suppressWarnings(as.integer(th$n_valid_isi %||% length(x)))[1]
  )
}

estimate_logisi_threshold_train <- function(isi_sec, min_isi_sec = 0.001, mcv_sec = 0.1, bin_width_log10 = 0.1) {
  estimate_logisi_threshold_train_result(
    isi_sec = isi_sec,
    min_isi_sec = min_isi_sec,
    mcv_sec = mcv_sec,
    bin_width_log10 = bin_width_log10
  )$threshold_sec
}
