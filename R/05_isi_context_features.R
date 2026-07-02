# Auto-generated modular extraction from modular reference script.
# Do not edit generated function names blindly; use git for version history.

# ============================================================
# Context contrast helpers
# ============================================================

valid_isi_values <- function(x, min_isi_sec = 0.001) {
  x <- as.numeric(x)
  x[is.finite(x) & x >= min_isi_sec]
}


compute_isi_percentiles_vector <- function(isi_sec, min_isi_sec = 0.001, isi_cache = NULL) {
  isi_sec <- suppressWarnings(as.numeric(isi_sec))
  # Native C fast path. Falls back to the original R implementation if the
  # package native library is unavailable, for example when this file is sourced
  # outside the installed package.
  if (is.null(isi_cache)) {
    native <- tryCatch(
      .Call("stpd_isi_percentiles_c", isi_sec, as.numeric(min_isi_sec)),
      error = function(e) NULL
    )
    if (is.numeric(native) && length(native) == length(isi_sec)) return(native)
  }
  out <- rep(NA_real_, length(isi_sec))
  cache <- isi_cache %||% make_isi_cache(isi_sec, min_isi_sec = min_isi_sec)
  if (is.null(cache) || cache$n_valid <= 0) return(out)
  valid <- is.finite(isi_sec) & isi_sec >= min_isi_sec
  out[valid] <- 100 * findInterval(isi_sec[valid], cache$sorted_isi, rightmost.closed = TRUE) / cache$n_valid
  out
}



compute_isi_range_metrics_vector <- function(isi_sec, min_isi_sec = 0.001) {
  x <- suppressWarnings(as.numeric(isi_sec))
  out <- data.frame(
    ISI_range_pct_linear = rep(NA_real_, length(x)),
    ISI_range_pct_log = rep(NA_real_, length(x)),
    ISI_robust_range_pct_log = rep(NA_real_, length(x))
  )
  valid <- is.finite(x) & x >= min_isi_sec
  vals <- x[valid]
  if (length(vals) < 2) return(out)
  clamp01 <- function(z) pmin(100, pmax(0, z))
  vmin <- min(vals, na.rm = TRUE); vmax <- max(vals, na.rm = TRUE)
  if (is.finite(vmin) && is.finite(vmax) && vmax > vmin) {
    out$ISI_range_pct_linear[valid] <- clamp01(100 * (x[valid] - vmin) / (vmax - vmin))
  }
  lv <- log(vals)
  lmin <- min(lv, na.rm = TRUE); lmax <- max(lv, na.rm = TRUE)
  if (is.finite(lmin) && is.finite(lmax) && lmax > lmin) {
    out$ISI_range_pct_log[valid] <- clamp01(100 * (log(x[valid]) - lmin) / (lmax - lmin))
  }
  qlo <- suppressWarnings(as.numeric(stats::quantile(vals, probs = 0.01, na.rm = TRUE, names = FALSE, type = 7)))
  qhi <- suppressWarnings(as.numeric(stats::quantile(vals, probs = 0.99, na.rm = TRUE, names = FALSE, type = 7)))
  if (is.finite(qlo) && is.finite(qhi) && qlo > 0 && qhi > qlo) {
    lqlo <- log(qlo); lqhi <- log(qhi)
    out$ISI_robust_range_pct_log[valid] <- clamp01(100 * (log(x[valid]) - lqlo) / (lqhi - lqlo))
  }
  out
}

make_isi_cache <- function(isi_sec, min_isi_sec = 0.001) {
  x <- suppressWarnings(as.numeric(isi_sec))
  valid <- is.finite(x) & x >= min_isi_sec
  sx <- sort(x[valid])
  list(sorted_isi = sx, n_valid = length(sx), min_isi_sec = min_isi_sec, source_n = length(x))
}

isi_percentile_from_cache <- function(value_sec, cache) {
  value_sec <- suppressWarnings(as.numeric(value_sec))
  if (!is.finite(value_sec) || is.null(cache) || is.null(cache$sorted_isi) || cache$n_valid <= 0) return(NA_real_)
  100 * findInterval(value_sec, cache$sorted_isi, rightmost.closed = TRUE) / cache$n_valid
}

train_percentile_reliable <- function(dat, p = NULL, min_isi_sec = 0.001) {
  min_n <- safe_int(if (!is.null(p)) p$adaptive_min_isi_for_percentile %||% 50L else 50L, 50L)
  n_valid <- NA_integer_
  if ("ISI_rank_n" %in% names(dat) && length(dat$ISI_rank_n) > 0) {
    tmp <- suppressWarnings(as.integer(dat$ISI_rank_n[which(!is.na(dat$ISI_rank_n))[1]]))
    if (is.finite(tmp)) n_valid <- tmp
  }
  if (!is.finite(n_valid)) n_valid <- sum(is.finite(dat$ISI_sec) & dat$ISI_sec >= min_isi_sec, na.rm = TRUE)
  n_valid >= min_n
}

isi_percentile_scalar <- function(value_sec, isi_sec = NULL, min_isi_sec = 0.001, isi_cache = NULL) {
  value_sec <- suppressWarnings(as.numeric(value_sec))
  if (!is.finite(value_sec)) return(NA_real_)
  if (is.null(isi_cache)) isi_cache <- make_isi_cache(isi_sec, min_isi_sec = min_isi_sec)
  isi_percentile_from_cache(value_sec, isi_cache)
}

ensure_train_isi_percentiles <- function(dat, min_isi_sec = 0.001, force = FALSE) {
  if (is.null(dat) || nrow(dat) == 0) return(dat)
  old_min <- attr(dat, "isi_percentile_min_isi_sec")
  old_cache <- attr(dat, "isi_cache")
  has_cached <- all(c("ISI_pct", "ISI_rank_n", "ISI_range_pct_linear", "ISI_range_pct_log", "ISI_robust_range_pct_log") %in% names(dat)) && !is.null(old_cache) && old_cache$source_n == length(dat$ISI_sec)
  if (!isTRUE(force) && has_cached && is.finite(old_min) && abs(old_min - min_isi_sec) < 1e-12) return(dat)
  cache <- make_isi_cache(dat$ISI_sec, min_isi_sec = min_isi_sec)
  dat$ISI_pct <- compute_isi_percentiles_vector(dat$ISI_sec, min_isi_sec = min_isi_sec, isi_cache = cache)
  dat$ISI_rank_n <- cache$n_valid
  rm <- compute_isi_range_metrics_vector(dat$ISI_sec, min_isi_sec = min_isi_sec)
  dat$ISI_range_pct_linear <- rm$ISI_range_pct_linear
  dat$ISI_range_pct_log <- rm$ISI_range_pct_log
  dat$ISI_robust_range_pct_log <- rm$ISI_robust_range_pct_log
  attr(dat, "isi_cache") <- cache
  attr(dat, "isi_percentile_min_isi_sec") <- min_isi_sec
  attr(dat, "isi_percentile_reliable") <- cache$n_valid >= 50L
  dat
}

ensure_train_local_median_cache <- function(dat, window = 11L, min_isi_sec = 0.001, force = FALSE) {
  if (is.null(dat) || nrow(dat) == 0 || is.null(dat$ISI_sec)) return(dat)
  window <- max(3L, safe_int(window, 11L))
  if (window %% 2L == 0L) window <- window + 1L
  old_window <- attr(dat, "local_median_window")
  old_min <- attr(dat, "local_median_min_isi_sec")
  has_cached <- "local_median_ISI_sec" %in% names(dat) && length(dat$local_median_ISI_sec) == nrow(dat)
  if (!isTRUE(force) && has_cached && is.finite(old_window) && old_window == window && is.finite(old_min) && abs(old_min - min_isi_sec) < 1e-12) return(dat)
  isi <- suppressWarnings(as.numeric(dat$ISI_sec))
  n <- length(isi)
  native <- tryCatch(
    .Call("stpd_local_median_cache_c", isi, as.integer(window), as.numeric(min_isi_sec)),
    error = function(e) NULL
  )
  if (is.numeric(native) && length(native) == n) {
    out <- native
  } else {
    out <- rep(NA_real_, n)
    half <- floor(window / 2L)
    for (ii in seq_len(n)) {
      lo <- max(2L, ii - half)
      hi <- min(n, ii + half)
      idx <- lo:hi
      idx <- idx[idx != ii]
      vals <- isi[idx]
      vals <- vals[is.finite(vals) & vals >= min_isi_sec]
      if (length(vals) > 0) out[ii] <- stats::median(vals, na.rm = TRUE)
    }
  }
  dat$local_median_ISI_sec <- out
  attr(dat, "local_median_window") <- window
  attr(dat, "local_median_min_isi_sec") <- min_isi_sec
  dat
}

precompute_trains_isi_percentiles <- function(trains, min_isi_sec = 0.001, force = FALSE, progress = NULL) {
  if (is.null(trains) || length(trains) == 0) return(trains)
  train_names <- names(trains)
  total <- max(1L, length(train_names))
  for (ii in seq_along(train_names)) {
    tr <- train_names[[ii]]
    if (is.function(progress)) {
      try(progress(train = tr, index = ii, total = total), silent = TRUE)
    }
    trains[[tr]] <- ensure_train_isi_percentiles(trains[[tr]], min_isi_sec = min_isi_sec, force = force)
  }
  trains
}

train_range_dataframe <- function(range_list, pattern = "burst", factor = 1, unit = "s") {
  if (is.null(range_list) || length(range_list) == 0) {
    return(data.frame(pattern = character(), train = character(), low_pct = numeric(), high_pct = numeric(),
                      low_ISI = numeric(), high_ISI = numeric(), unit = character(), n_valid_isi = numeric(),
                      n_manual_isi = numeric(), source = character(), method = character(), updated_at = character(),
                      stringsAsFactors = FALSE))
  }
  rows <- lapply(names(range_list), function(tr) {
    x <- range_list[[tr]]
    data.frame(
      pattern = pattern,
      train = tr,
      low_pct = range_value(x, "low_pct", NA_real_),
      high_pct = range_value(x, "high_pct", NA_real_),
      low_ISI = range_value(x, "low_sec", NA_real_) * factor,
      high_ISI = range_value(x, "high_sec", NA_real_) * factor,
      unit = unit,
      n_valid_isi = range_value(x, "n_valid_isi", NA_real_),
      n_manual_isi = range_value(x, paste0("n_manual_", pattern, "_isi"), range_value(x, "n_manual_burst_isi", range_value(x, "n_manual_isi", NA_real_))),
      anchor_center_sec = range_value(x, "anchor_center_sec", NA_real_),
      anchor_spread_log = range_value(x, "anchor_spread_log", NA_real_),
      anchor_confidence = range_value(x, "anchor_confidence", NA_real_),
      anchor_n = range_value(x, "anchor_n", NA_real_),
      range_mode = as.character(x$range_mode %||% ""),
      abs_low_override = isTRUE(x$abs_low_override %||% FALSE),
      abs_high_override = isTRUE(x$abs_high_override %||% FALSE),
      source = as.character(x$source %||% ""),
      method = as.character(x$method %||% ""),
      updated_at = as.character(x$updated_at %||% ""),
      stringsAsFactors = FALSE
    )
  })
  bind_rows(rows)
}



# ============================================================
# train-specific ISI threshold overrides
# ============================================================

stpd_train_isi_threshold_mode <- function(x) {
  mode <- as.character(x$threshold_mode %||% x$mode %||% "")
  mode <- mode[1]
  if (!nzchar(mode)) {
    mode <- if (isTRUE(x$hard_threshold %||% FALSE)) "hard_threshold" else "soft_anchor"
  }
  if (!mode %in% c("soft_anchor", "hard_threshold")) mode <- "soft_anchor"
  mode
}

stpd_train_isi_threshold_is_hard <- function(x) {
  identical(stpd_train_isi_threshold_mode(x), "hard_threshold") || isTRUE(x$hard_threshold %||% FALSE)
}

stpd_train_isi_threshold_source <- function(x, pattern = "") {
  src <- as.character(x$source %||% "")
  src <- src[1]
  if (nzchar(src)) return(src)
  if (isTRUE(stpd_train_isi_threshold_is_hard(x))) {
    paste0("ui_isi_profile_threshold_line", if (nzchar(pattern)) paste0("_", pattern) else "")
  } else {
    paste0("isi_profile_threshold_line_soft_anchor", if (nzchar(pattern)) paste0("_", pattern) else "")
  }
}

stpd_train_isi_threshold_range <- function(x, pattern, low_sec = NA_real_, high_sec = NA_real_) {
  low_sec <- suppressWarnings(as.numeric(low_sec))
  high_sec <- suppressWarnings(as.numeric(high_sec))
  hard <- stpd_train_isi_threshold_is_hard(x)
  vals <- c(low_sec, high_sec)
  vals <- vals[is.finite(vals) & vals > 0]
  center <- if (length(vals) == 0) NA_real_ else if (length(vals) == 1) vals[1] else exp(mean(log(vals)))
  spread <- if (length(vals) >= 2 && all(vals > 0)) abs(diff(log(range(vals)))) / 2 else 0.35
  if (!is.finite(spread) || spread <= 0) spread <- 0.35
  list(
    low_sec = ifelse(is.finite(low_sec) && low_sec > 0, low_sec, NA_real_),
    high_sec = ifelse(is.finite(high_sec) && high_sec > 0, high_sec, NA_real_),
    anchor_center_sec = center,
    anchor_spread_log = max(0.25, spread),
    anchor_n = 1L,
    anchor_confidence = if (hard) 1 else 0.55,
    abs_low_override = is.finite(low_sec) && low_sec > 0,
    abs_high_override = is.finite(high_sec) && high_sec > 0,
    threshold_mode = stpd_train_isi_threshold_mode(x),
    hard_threshold = hard,
    source = stpd_train_isi_threshold_source(x, pattern),
    method = if (hard) {
      paste0("explicit hard ", pattern, " threshold line from ISI temporal profile")
    } else {
      paste0("soft ", pattern, " scale anchor from ISI temporal profile threshold line")
    },
    updated_at = as.character(x$updated_at %||% Sys.time())
  )
}

train_isi_threshold_dataframe <- function(thresholds, factor = 1, unit = "s") {
  if (is.null(thresholds) || length(thresholds) == 0) {
    return(data.frame(train = character(), burst_max_ISI = numeric(), pause_min_ISI = numeric(),
                      tonic_min_ISI = numeric(), tonic_max_ISI = numeric(), unit = character(),
                      active = logical(), threshold_mode = character(), hard_threshold = logical(),
                      updated_at = character(), stringsAsFactors = FALSE))
  }
  rows <- lapply(names(thresholds), function(tr) {
    x <- thresholds[[tr]]
    b <- suppressWarnings(as.numeric(x$burst_max_sec %||% 0))
    pmin <- suppressWarnings(as.numeric(x$pause_min_sec %||% 0))
    tmin <- suppressWarnings(as.numeric(x$tonic_min_sec %||% 0))
    tmax <- suppressWarnings(as.numeric(x$tonic_max_sec %||% 0))
    data.frame(
      train = tr,
      burst_max_ISI = ifelse(is.finite(b) && b > 0, b * factor, 0),
      pause_min_ISI = ifelse(is.finite(pmin) && pmin > 0, pmin * factor, 0),
      tonic_min_ISI = ifelse(is.finite(tmin) && tmin > 0, tmin * factor, 0),
      tonic_max_ISI = ifelse(is.finite(tmax) && tmax > 0, tmax * factor, 0),
      unit = unit,
      active = isTRUE((is.finite(b) && b > 0) || (is.finite(pmin) && pmin > 0) || (is.finite(tmin) && tmin > 0) || (is.finite(tmax) && tmax > 0)),
      threshold_mode = stpd_train_isi_threshold_mode(x),
      hard_threshold = stpd_train_isi_threshold_is_hard(x),
      updated_at = as.character(x$updated_at %||% ""),
      source = stpd_train_isi_threshold_source(x),
      stringsAsFactors = FALSE
    )
  })
  dplyr::bind_rows(rows)
}

merge_train_isi_thresholds_into_params <- function(params, thresholds) {
  if (is.null(params) || is.null(thresholds) || length(thresholds) == 0) return(params)
  any_burst <- FALSE; any_tonic <- FALSE; any_pause <- FALSE
  if (is.null(params$burst$adaptive_train_ranges)) params$burst$adaptive_train_ranges <- list()
  if (is.null(params$tonic$adaptive_train_ranges)) params$tonic$adaptive_train_ranges <- list()
  if (is.null(params$pause$adaptive_train_ranges)) params$pause$adaptive_train_ranges <- list()
  for (tr in names(thresholds)) {
    x <- thresholds[[tr]]
    bmax <- suppressWarnings(as.numeric(x$burst_max_sec %||% 0))
    pmin <- suppressWarnings(as.numeric(x$pause_min_sec %||% 0))
    tmin <- suppressWarnings(as.numeric(x$tonic_min_sec %||% 0))
    tmax <- suppressWarnings(as.numeric(x$tonic_max_sec %||% 0))
    if (is.finite(bmax) && bmax > 0) {
      rr <- params$burst$adaptive_train_ranges[[tr]] %||% list()
      rr <- utils::modifyList(rr, stpd_train_isi_threshold_range(x, "burst", high_sec = bmax))
      params$burst$adaptive_train_ranges[[tr]] <- rr
      any_burst <- TRUE
    }
    if ((is.finite(tmin) && tmin > 0) || (is.finite(tmax) && tmax > 0)) {
      rr <- params$tonic$adaptive_train_ranges[[tr]] %||% list()
      rr <- utils::modifyList(rr, stpd_train_isi_threshold_range(x, "tonic", low_sec = tmin, high_sec = tmax))
      params$tonic$adaptive_train_ranges[[tr]] <- rr
      any_tonic <- TRUE
    }
    if (is.finite(pmin) && pmin > 0) {
      rr <- params$pause$adaptive_train_ranges[[tr]] %||% list()
      rr <- utils::modifyList(rr, stpd_train_isi_threshold_range(x, "pause", low_sec = pmin))
      params$pause$adaptive_train_ranges[[tr]] <- rr
      any_pause <- TRUE
    }
  }
  params$detector$train_isi_thresholds <- thresholds
  if (any_burst) params$burst$adaptive_use_train_ranges <- TRUE
  if (any_tonic) params$tonic$adaptive_use_train_ranges <- TRUE
  if (any_pause) params$pause$adaptive_use_train_ranges <- TRUE
  params
}

stpd_apply_train_isi_thresholds_to_event_vp <- function(vp, params, train = "", min_isi_sec = 0.001) {
  if (is.null(vp) || is.null(params) || !nzchar(as.character(train %||% ""))) return(vp)
  train <- as.character(train)
  min_isi_sec <- suppressWarnings(as.numeric(min_isi_sec))
  if (!is.finite(min_isi_sec) || min_isi_sec <= 0) min_isi_sec <- 0.001

  brr <- get_train_burst_range(params$burst %||% list(), train = train)
  if (!is.null(brr) && isTRUE((params$burst %||% list())$adaptive_use_train_ranges %||% TRUE)) {
    bmax <- range_value(brr, "high_sec", NA_real_)
    if (is.finite(bmax) && bmax > 0) {
      if (isTRUE(stpd_train_isi_threshold_is_hard(brr))) {
        vp$seed_high <- bmax
      } else {
        vp$seed_high <- max(c(vp$seed_high, bmax), na.rm = TRUE)
      }
      vp$bridge_high <- max(c(vp$bridge_high, bmax, bmax * 1.25), na.rm = TRUE)
      vp$tonic_burst_overlap_ref <- max(c(vp$tonic_burst_overlap_ref, bmax), na.rm = TRUE)
      vp$train_isi_threshold_burst_high_sec <- bmax
    }
  }

  trr <- get_train_tonic_range(params$tonic %||% list(), train = train)
  if (!is.null(trr) && isTRUE((params$tonic %||% list())$adaptive_use_train_ranges %||% TRUE)) {
    tmin <- range_value(trr, "low_sec", NA_real_)
    tmax <- range_value(trr, "high_sec", NA_real_)
    if (is.finite(tmin) && tmin > 0) vp$tonic_min <- tmin
    if (is.finite(tmax) && tmax > 0) vp$tonic_max <- tmax
    burst_ref <- suppressWarnings(as.numeric(vp$tonic_burst_overlap_ref %||% NA_real_))
    guard_factor <- suppressWarnings(as.numeric((params$tonic %||% list())$burst_overlap_guard_factor %||% vp$tonic_burst_overlap_guard_factor %||% 1.15))
    if (!is.finite(guard_factor) || guard_factor < 1) guard_factor <- 1.15
    if (isTRUE((params$tonic %||% list())$burst_overlap_guard %||% vp$tonic_burst_overlap_guard %||% TRUE) &&
        is.finite(burst_ref) && burst_ref > 0) {
      vp$tonic_min <- max(c(vp$tonic_min, burst_ref * guard_factor), na.rm = TRUE)
    }
    if (is.finite(vp$tonic_max %||% NA_real_) && is.finite(vp$tonic_min %||% NA_real_) &&
        vp$tonic_max <= vp$tonic_min) {
      vp$tonic_max <- max(c(vp$tonic_min + min_isi_sec, vp$tonic_min * 1.25), na.rm = TRUE)
    }
    vp$train_isi_threshold_tonic_min_sec <- tmin
    vp$train_isi_threshold_tonic_max_sec <- tmax
  }

  prr <- get_train_pause_range(params$pause %||% list(), train = train)
  if (!is.null(prr) && isTRUE((params$pause %||% list())$adaptive_use_train_ranges %||% TRUE)) {
    pmin <- range_value(prr, "low_sec", NA_real_)
    if (is.finite(pmin) && pmin > 0) {
      vp$pause_thr <- pmin
      vp$train_isi_threshold_pause_min_sec <- pmin
    }
  }

  vp
}

seed_interval_overlap_fraction <- function(s1, e1, s2, e2) {
  ov <- max(0L, min(e1, e2) - max(s1, s2) + 1L)
  if (ov <= 0L) return(0)
  len1 <- max(1L, e1 - s1 + 1L)
  len2 <- max(1L, e2 - s2 + 1L)
  ov / min(len1, len2)
}


train_isi_cutoff_by_pct <- function(dat, pct, min_isi_sec = 0.001) {
  x <- valid_isi_values(dat$ISI_sec, min_isi_sec)
  if (length(x) == 0) return(NA_real_)
  pct <- clamp(pct, 0, 100)
  as.numeric(stats::quantile(x, probs = pct / 100, na.rm = TRUE, names = FALSE, type = 7))
}

get_train_burst_range <- function(p, train = "") {
  ranges <- p$adaptive_train_ranges %||% p$train_burst_ranges %||% list()
  if (is.null(ranges) || length(ranges) == 0 || train == "") return(NULL)
  rr <- ranges[[train]]
  if (is.null(rr)) return(NULL)
  rr
}

stpd_manual_anchor_from_values <- function(values, min_isi_sec = 0.001) {
  vals <- suppressWarnings(as.numeric(values))
  vals <- vals[is.finite(vals) & vals >= min_isi_sec]
  n <- length(vals)
  if (n == 0) {
    return(list(
      anchor_center_sec = NA_real_, anchor_spread_log = NA_real_,
      anchor_n = 0L, anchor_confidence = 0
    ))
  }
  log_vals <- log(vals)
  center <- stats::median(vals, na.rm = TRUE)
  spread <- stats::mad(log_vals, center = stats::median(log_vals, na.rm = TRUE),
                       constant = 1.4826, na.rm = TRUE)
  if (!is.finite(spread) || spread <= 0) {
    iqr <- stats::IQR(log_vals, na.rm = TRUE)
    spread <- if (is.finite(iqr) && iqr > 0) iqr / 1.349 else NA_real_
  }
  # Sparse manual labels are anchors with broad uncertainty, not hard intervals.
  min_spread <- if (n < 3L) 0.80 else if (n < 6L) 0.55 else 0.30
  if (!is.finite(spread) || spread <= 0) spread <- min_spread
  spread <- max(spread, min_spread)
  confidence <- n / (n + 6)
  list(
    anchor_center_sec = center,
    anchor_spread_log = spread,
    anchor_n = as.integer(n),
    anchor_confidence = confidence
  )
}

stpd_range_is_manual_anchor <- function(rr) {
  if (is.null(rr)) return(FALSE)
  if (isTRUE(rr$hard_threshold %||% FALSE)) return(FALSE)
  mode <- as.character(rr$threshold_mode %||% "")
  if (length(mode) > 0 && identical(mode[1], "hard_threshold")) return(FALSE)
  src <- tolower(paste(as.character(rr$source %||% ""), as.character(rr$method %||% ""), collapse = " "))
  grepl("manual|anchor", src)
}

stpd_range_policy <- function(rr, hard_requested = FALSE) {
  is_anchor <- stpd_range_is_manual_anchor(rr)
  list(
    is_manual_anchor = isTRUE(is_anchor),
    hard_requested = isTRUE(hard_requested),
    hard_allowed = isTRUE(hard_requested) && !isTRUE(is_anchor)
  )
}

stpd_manual_anchor_score <- function(value_sec,
                                     value_pct = NA_real_,
                                     rr = NULL,
                                     support_min = 0.30,
                                     bonus_weight = 0.45,
                                     penalty_weight = 0.12) {
  empty <- list(
    active = FALSE, soft_support = FALSE, score = 0,
    closeness = NA_real_, distance_log = NA_real_,
    center_sec = NA_real_, spread_log = NA_real_,
    confidence = 0, n = 0L, source = ""
  )
  if (is.null(rr) || !stpd_range_is_manual_anchor(rr)) return(empty)
  value_sec <- suppressWarnings(as.numeric(value_sec))
  if (!is.finite(value_sec) || value_sec <= 0) return(empty)

  center <- range_value(rr, "anchor_center_sec", NA_real_)
  if (!is.finite(center) || center <= 0) {
    lo <- range_value(rr, "low_sec", NA_real_)
    hi <- range_value(rr, "high_sec", NA_real_)
    if (is.finite(lo) && lo > 0 && is.finite(hi) && hi > 0) {
      center <- exp(mean(log(c(lo, hi))))
    } else if (is.finite(hi) && hi > 0) {
      center <- hi
    }
  }
  if (!is.finite(center) || center <= 0) return(empty)

  n <- safe_int(rr$anchor_n %||% rr$n_manual_burst_isi %||% rr$n_manual_isi %||% 0L, 0L)
  spread <- range_value(rr, "anchor_spread_log", NA_real_)
  min_spread <- if (n < 3L) 0.80 else if (n < 6L) 0.55 else 0.30
  if (!is.finite(spread) || spread <= 0) spread <- min_spread
  spread <- max(spread, min_spread)
  confidence <- range_value(rr, "anchor_confidence", NA_real_)
  if (!is.finite(confidence)) confidence <- if (n > 0L) n / (n + 6) else 0.35
  confidence <- clamp(confidence, 0, 1)

  dist <- abs(log(value_sec / center))
  closeness <- exp(-0.5 * (dist / spread)^2)
  soft_support <- is.finite(closeness) && closeness >= support_min
  score <- confidence * (bonus_weight * closeness - penalty_weight * (1 - closeness))
  list(
    active = TRUE,
    soft_support = soft_support,
    score = score,
    closeness = closeness,
    distance_log = dist,
    center_sec = center,
    spread_log = spread,
    confidence = confidence,
    n = as.integer(n),
    source = as.character(rr$source %||% "manual_anchor")
  )
}

stpd_range_anchor_support <- function(value_sec,
                                      value_pct = NA_real_,
                                      rr = NULL,
                                      mode = "percentile_or_absolute",
                                      enforce_lower_sec = FALSE,
                                      default_low_pct = 0,
                                      default_high_pct = 100,
                                      hard_requested = FALSE,
                                      support_min = 0.30) {
  policy <- stpd_range_policy(rr, hard_requested = hard_requested)
  range_match <- FALSE
  if (!is.null(rr)) {
    range_match <- train_range_match(
      value_sec = value_sec,
      value_pct = value_pct,
      rr = rr,
      mode = mode,
      enforce_lower_sec = enforce_lower_sec,
      default_low_pct = default_low_pct,
      default_high_pct = default_high_pct
    )
  }
  anchor <- stpd_manual_anchor_score(value_sec, value_pct = value_pct, rr = rr, support_min = support_min)
  soft_support <- if (isTRUE(policy$is_manual_anchor)) {
    isTRUE(anchor$soft_support) || isTRUE(range_match)
  } else {
    isTRUE(range_match)
  }
  list(
    range_match = isTRUE(range_match),
    soft_support = isTRUE(soft_support),
    policy = policy,
    anchor = anchor
  )
}

derive_burst_isi_ranges_from_manual <- function(ds,
                                                   min_isi_sec = 0.001,
                                                   q_low = 0.01,
                                                   q_high = 0.95,
                                                   expand_pct = 5,
                                                   expand_factor = 1.25) {
  if (is.null(ds) || is.null(ds$trains) || length(ds$trains) == 0) return(list())
  out <- list()
  for (tr in names(ds$trains)) {
    dat <- ds$trains[[tr]]
    if (is.null(dat$pattern_manual) || is.null(dat$ISI_sec)) next
    valid <- is.finite(dat$ISI_sec) & dat$ISI_sec >= min_isi_sec
    burst_vals <- dat$ISI_sec[valid & dat$pattern_manual == "burst"]
    if (length(burst_vals) == 0) next
    lo <- as.numeric(stats::quantile(burst_vals, probs = q_low, na.rm = TRUE, names = FALSE, type = 7))
    hi_raw <- as.numeric(stats::quantile(burst_vals, probs = q_high, na.rm = TRUE, names = FALSE, type = 7))
    spread <- stats::IQR(burst_vals, na.rm = TRUE)
    if (!is.finite(spread) || spread <= 0) spread <- safe_median(abs(burst_vals - safe_median(burst_vals)), default = 0)
    hi <- hi_raw + max(0, expand_factor - 1) * spread
    train_valid_vals <- valid_isi_values(dat$ISI_sec, min_isi_sec)
    if (length(train_valid_vals) > 0 && is.finite(hi)) hi <- min(hi, max(train_valid_vals, na.rm = TRUE))
    cache <- make_isi_cache(dat$ISI_sec, min_isi_sec = min_isi_sec)
    lo_pct <- isi_percentile_scalar(lo, dat$ISI_sec, min_isi_sec = min_isi_sec, isi_cache = cache)
    hi_pct_raw <- isi_percentile_scalar(hi_raw, dat$ISI_sec, min_isi_sec = min_isi_sec, isi_cache = cache)
    hi_pct <- isi_percentile_scalar(hi, dat$ISI_sec, min_isi_sec = min_isi_sec, isi_cache = cache)
    if (is.finite(hi_pct_raw)) hi_pct <- max(hi_pct, min(100, hi_pct_raw + max(0, expand_pct)))
    anchor <- stpd_manual_anchor_from_values(burst_vals, min_isi_sec = min_isi_sec)
    out[[tr]] <- list(
      train = tr,
      low_pct = lo_pct,
      high_pct = hi_pct,
      low_sec = lo,
      high_sec = hi,
      anchor_center_sec = anchor$anchor_center_sec,
      anchor_spread_log = anchor$anchor_spread_log,
      anchor_n = anchor$anchor_n,
      anchor_confidence = anchor$anchor_confidence,
      abs_low_override = FALSE,
      abs_high_override = FALSE,
      n_valid_isi = sum(valid, na.rm = TRUE),
      n_manual_burst_isi = length(burst_vals),
      source = "manual_burst",
      method = paste0("manual burst anchor plus ISI q", q_low, "-q", q_high, "; upper expanded by pct=", expand_pct, ", factor=", expand_factor, "; anchor is soft, not a hard boundary"),
      updated_at = as.character(Sys.time())
    )
  }
  out
}


get_train_tonic_range <- function(p, train = "") {
  ranges <- p$adaptive_train_ranges %||% p$train_tonic_ranges %||% list()
  if (is.null(ranges) || length(ranges) == 0 || train == "") return(NULL)
  rr <- ranges[[train]]
  if (is.null(rr)) return(NULL)
  rr
}

get_train_pause_range <- function(p, train = "") {
  ranges <- p$adaptive_train_ranges %||% p$train_pause_ranges %||% list()
  if (is.null(ranges) || length(ranges) == 0 || train == "") return(NULL)
  rr <- ranges[[train]]
  if (is.null(rr)) return(NULL)
  rr
}

get_train_highfreq_range <- function(p, train = "") {
  ranges <- p$adaptive_train_ranges %||% p$train_highfreq_ranges %||% p$train_hf_ranges %||% list()
  if (is.null(ranges) || length(ranges) == 0 || train == "") return(NULL)
  rr <- ranges[[train]]
  if (is.null(rr)) return(NULL)
  rr
}

derive_tonic_isi_ranges_from_manual <- function(ds,
                                                   min_isi_sec = 0.001,
                                                   q_low = 0.05,
                                                   q_high = 0.95) {
  if (is.null(ds) || is.null(ds$trains) || length(ds$trains) == 0) return(list())
  out <- list()
  for (tr in names(ds$trains)) {
    dat <- ds$trains[[tr]]
    if (is.null(dat$pattern_manual) || is.null(dat$ISI_sec)) next
    valid <- is.finite(dat$ISI_sec) & dat$ISI_sec >= min_isi_sec
    vals <- dat$ISI_sec[valid & dat$pattern_manual == "tonic"]
    if (length(vals) < 2) next
    lo <- as.numeric(stats::quantile(vals, probs = q_low, na.rm = TRUE, names = FALSE, type = 7))
    hi <- as.numeric(stats::quantile(vals, probs = q_high, na.rm = TRUE, names = FALSE, type = 7))
    lo_pct <- isi_percentile_scalar(lo, dat$ISI_sec, min_isi_sec = min_isi_sec)
    hi_pct <- isi_percentile_scalar(hi, dat$ISI_sec, min_isi_sec = min_isi_sec)
    anchor <- stpd_manual_anchor_from_values(vals, min_isi_sec = min_isi_sec)

    seg <- find_segments(as.character(dat$pattern_manual), "tonic")
    lv_vals <- numeric(0); cv_vals <- numeric(0); mm_vals <- numeric(0)
    if (nrow(seg) > 0) {
      for (ii in seq_len(nrow(seg))) {
        vv <- valid_isi_values(dat$ISI_sec[seg$start_isi[ii]:seg$end_isi[ii]], min_isi_sec)
        if (length(vv) >= 2) {
          lv_vals <- c(lv_vals, calc_LV(vv))
          cv_vals <- c(cv_vals, calc_CV(vv))
          mm_vals <- c(mm_vals, max(vv) / mean(vv))
        }
      }
    }

    out[[tr]] <- list(
      train = tr,
      low_pct = lo_pct,
      high_pct = hi_pct,
      low_sec = lo,
      high_sec = hi,
      anchor_center_sec = anchor$anchor_center_sec,
      anchor_spread_log = anchor$anchor_spread_log,
      anchor_n = anchor$anchor_n,
      anchor_confidence = anchor$anchor_confidence,
      abs_low_override = FALSE,
      abs_high_override = FALSE,
      n_valid_isi = sum(valid, na.rm = TRUE),
      n_manual_tonic_isi = length(vals),
      learned_LV_q95 = safe_q(lv_vals, 0.95, default = NA_real_),
      learned_CV_q95 = safe_q(cv_vals, 0.95, default = NA_real_),
      learned_MM_q95 = safe_q(mm_vals, 0.95, default = NA_real_),
      source = "manual_tonic",
      method = paste0("manual tonic anchor plus ISI q", q_low, "-q", q_high, "; LV/CV/MM q95 diagnostics; anchor is soft, not a hard boundary"),
      updated_at = as.character(Sys.time())
    )
  }
  out
}

derive_pause_isi_ranges_from_manual <- function(ds,
                                                   min_isi_sec = 0.001,
                                                   q_low = 0.05,
                                                   q_high = 0.99) {
  if (is.null(ds) || is.null(ds$trains) || length(ds$trains) == 0) return(list())
  out <- list()
  for (tr in names(ds$trains)) {
    dat <- ds$trains[[tr]]
    if (is.null(dat$pattern_manual) || is.null(dat$ISI_sec)) next
    valid <- is.finite(dat$ISI_sec) & dat$ISI_sec >= min_isi_sec
    vals <- dat$ISI_sec[valid & dat$pattern_manual == "pause"]
    if (length(vals) == 0) next
    lo <- as.numeric(stats::quantile(vals, probs = q_low, na.rm = TRUE, names = FALSE, type = 7))
    hi <- as.numeric(stats::quantile(vals, probs = q_high, na.rm = TRUE, names = FALSE, type = 7))
    lo_pct <- isi_percentile_scalar(lo, dat$ISI_sec, min_isi_sec = min_isi_sec)
    hi_pct <- isi_percentile_scalar(hi, dat$ISI_sec, min_isi_sec = min_isi_sec)
    anchor <- stpd_manual_anchor_from_values(vals, min_isi_sec = min_isi_sec)
    out[[tr]] <- list(
      train = tr,
      low_pct = lo_pct,
      high_pct = hi_pct,
      low_sec = lo,
      high_sec = hi,
      anchor_center_sec = anchor$anchor_center_sec,
      anchor_spread_log = anchor$anchor_spread_log,
      anchor_n = anchor$anchor_n,
      anchor_confidence = anchor$anchor_confidence,
      abs_low_override = FALSE,
      abs_high_override = FALSE,
      n_valid_isi = sum(valid, na.rm = TRUE),
      n_manual_pause_isi = length(vals),
      source = "manual_pause",
      method = paste0("manual pause anchor plus ISI q", q_low, "-q", q_high, "; lower bound used as pause-seed aid; anchor is soft, not a hard boundary"),
      updated_at = as.character(Sys.time())
    )
  }
  out
}

derive_highfreq_isi_ranges_from_manual <- function(ds,
                                                   min_isi_sec = 0.001,
                                                   q_low = 0.01,
                                                   q_high = 0.95) {
  if (is.null(ds) || is.null(ds$trains) || length(ds$trains) == 0) return(list())
  out <- list()
  hf_labels <- c("high_frequency_tonic", "high_frequency_spiking")
  for (tr in names(ds$trains)) {
    dat <- ds$trains[[tr]]
    if (is.null(dat$pattern_manual) || is.null(dat$ISI_sec)) next
    labs <- normalize_pattern_label(dat$pattern_manual, fill_blank_others = FALSE)
    valid <- is.finite(dat$ISI_sec) & dat$ISI_sec >= min_isi_sec
    vals <- dat$ISI_sec[valid & labs %in% hf_labels]
    if (length(vals) == 0) next
    lo <- as.numeric(stats::quantile(vals, probs = q_low, na.rm = TRUE, names = FALSE, type = 7))
    hi <- as.numeric(stats::quantile(vals, probs = q_high, na.rm = TRUE, names = FALSE, type = 7))
    lo_pct <- isi_percentile_scalar(lo, dat$ISI_sec, min_isi_sec = min_isi_sec)
    hi_pct <- isi_percentile_scalar(hi, dat$ISI_sec, min_isi_sec = min_isi_sec)
    anchor <- stpd_manual_anchor_from_values(vals, min_isi_sec = min_isi_sec)

    segs <- lapply(hf_labels, function(lbl) find_segments(labs, lbl))
    segs <- dplyr::bind_rows(segs)
    dur_vals <- numeric(0); n_spike_vals <- numeric(0)
    lv_vals <- numeric(0); cv_vals <- numeric(0); mm_vals <- numeric(0)
    if (nrow(segs) > 0) {
      for (ii in seq_len(nrow(segs))) {
        s <- segs$start_isi[ii]; e <- segs$end_isi[ii]
        vv <- valid_isi_values(dat$ISI_sec[s:e], min_isi_sec)
        if (length(vv) == 0) next
        dur_vals <- c(dur_vals, sum(vv, na.rm = TRUE))
        n_spike_vals <- c(n_spike_vals, e - s + 2L)
        if (length(vv) >= 2) {
          lv_vals <- c(lv_vals, calc_LV(vv))
          cv_vals <- c(cv_vals, calc_CV(vv))
          mm_vals <- c(mm_vals, max(vv) / mean(vv))
        }
      }
    }

    out[[tr]] <- list(
      train = tr,
      low_pct = lo_pct,
      high_pct = hi_pct,
      low_sec = lo,
      high_sec = hi,
      anchor_center_sec = anchor$anchor_center_sec,
      anchor_spread_log = anchor$anchor_spread_log,
      anchor_n = anchor$anchor_n,
      anchor_confidence = anchor$anchor_confidence,
      abs_low_override = FALSE,
      abs_high_override = FALSE,
      n_valid_isi = sum(valid, na.rm = TRUE),
      n_manual_highfreq_isi = length(vals),
      learned_duration_q95 = safe_q(dur_vals, 0.95, default = NA_real_),
      learned_n_spikes_q50 = safe_q(n_spike_vals, 0.50, default = NA_real_),
      learned_LV_q95 = safe_q(lv_vals, 0.95, default = NA_real_),
      learned_CV_q95 = safe_q(cv_vals, 0.95, default = NA_real_),
      learned_MM_q95 = safe_q(mm_vals, 0.95, default = NA_real_),
      source = "manual_high_frequency",
      method = paste0("manual high-frequency anchor plus ISI q", q_low, "-", q_high, "; anchor is soft, not a hard boundary"),
      updated_at = as.character(Sys.time())
    )
  }
  out
}

# Generic train-specific range matcher.
# mode controls how a percentile interval and an absolute interval are combined.
# For burst ranges, learned lower absolute bounds are ignored by default because a
# real burst can be faster than the manually observed examples; explicit absolute
# lower bounds remain available as an artifact/minimum-ISI guard.
train_range_match <- function(value_sec,
                                 value_pct,
                                 rr,
                                 mode = "percentile_or_absolute",
                                 enforce_lower_sec = FALSE,
                                 default_low_pct = 0,
                                 default_high_pct = 100) {
  if (is.null(rr)) return(FALSE)
  value_sec <- suppressWarnings(as.numeric(value_sec))
  value_pct <- suppressWarnings(as.numeric(value_pct))
  if (!is.finite(value_sec) && !is.finite(value_pct)) return(FALSE)

  mode <- mode %||% "percentile_or_absolute"
  if (!mode %in% c("percentile_only", "absolute_only", "percentile_and_absolute", "percentile_or_absolute")) {
    mode <- "percentile_or_absolute"
  }

  lo_pct <- range_value(rr, "low_pct", default_low_pct)
  hi_pct <- range_value(rr, "high_pct", default_high_pct)
  if (!is.finite(lo_pct)) lo_pct <- default_low_pct
  if (!is.finite(hi_pct)) hi_pct <- default_high_pct
  if (hi_pct < lo_pct) { tmp <- lo_pct; lo_pct <- hi_pct; hi_pct <- tmp }
  pct_available <- is.finite(value_pct) && is.finite(lo_pct) && is.finite(hi_pct)
  pct_in <- pct_available && value_pct >= lo_pct && value_pct <= hi_pct

  lo_sec <- range_value(rr, "low_sec", NA_real_)
  hi_sec <- range_value(rr, "high_sec", NA_real_)
  if (is.finite(lo_sec) && is.finite(hi_sec) && hi_sec < lo_sec) { tmp <- lo_sec; lo_sec <- hi_sec; hi_sec <- tmp }
  sec_available <- is.finite(value_sec) && (is.finite(lo_sec) || is.finite(hi_sec))
  sec_in <- sec_available
  if (isTRUE(sec_available)) {
    use_low <- isTRUE(enforce_lower_sec) || isTRUE(rr$abs_low_override %||% FALSE)
    if (use_low && is.finite(lo_sec) && lo_sec > 0) sec_in <- sec_in && value_sec >= lo_sec
    if (is.finite(hi_sec) && hi_sec > 0) sec_in <- sec_in && value_sec <= hi_sec
  }

  if (mode == "percentile_only") return(pct_in)
  if (mode == "absolute_only") return(sec_available && sec_in)
  if (mode == "percentile_and_absolute") return(pct_in && (!sec_available || sec_in))
  pct_in || (sec_available && sec_in)
}

range_value <- function(rr, nm, default = NA_real_) {
  if (is.null(rr) || is.null(rr[[nm]])) return(default)
  v <- suppressWarnings(as.numeric(rr[[nm]]))
  if (is.finite(v)) v else default
}

get_local_median <- function(isi, i, window = 11L, exclude_idx = integer(0), min_isi_sec = 0.001) {
  n <- length(isi)
  idx <- seq(max(2, i - window), min(n, i + window))
  idx <- setdiff(idx, exclude_idx)
  x <- valid_isi_values(isi[idx], min_isi_sec)
  if (length(x) == 0) return(NA_real_)
  stats::median(x, na.rm = TRUE)
}

get_context_values <- function(isi, s_isi, e_isi, k = 5L, min_isi_sec = 0.001) {
  n <- length(isi)
  k <- max(1L, safe_int(k, 5L))
  
  pre_vals <- numeric(0)
  if (s_isi > 2) {
    j <- s_isi - 1
    while (j >= 2 && length(pre_vals) < k) {
      if (is.finite(isi[j]) && isi[j] >= min_isi_sec) pre_vals <- c(pre_vals, isi[j])
      j <- j - 1
    }
  }
  
  post_vals <- numeric(0)
  if (e_isi < n) {
    j <- e_isi + 1
    while (j <= n && length(post_vals) < k) {
      if (is.finite(isi[j]) && isi[j] >= min_isi_sec) post_vals <- c(post_vals, isi[j])
      j <- j + 1
    }
  }
  
  list(
    pre_values = pre_vals,
    post_values = post_vals,
    pre_median = if (length(pre_vals) > 0) stats::median(pre_vals) else NA_real_,
    post_median = if (length(post_vals) > 0) stats::median(post_vals) else NA_real_
  )
}

calc_ratio_summary <- function(pre, post, ref) {
  empty <- list(
    n_flank = 0L,
    pre_ratio = NA_real_,
    post_ratio = NA_real_,
    contrast_min = NA_real_,
    contrast_geom = NA_real_,
    contrast_log_geom = NA_real_,
    contrast_pct = NA_real_
  )
  if (!is.finite(ref) || ref <= 0) return(empty)
  
  pre_ok <- is.finite(pre) && pre > 0
  post_ok <- is.finite(post) && post > 0
  pre_ratio <- if (pre_ok) pre / ref else NA_real_
  post_ratio <- if (post_ok) post / ref else NA_real_
  rr <- c(pre_ratio, post_ratio)
  rr <- rr[is.finite(rr) & rr > 0]
  
  if (length(rr) == 0) return(empty)
  geom <- exp(mean(log(rr)))
  list(
    n_flank = length(rr),
    pre_ratio = pre_ratio,
    post_ratio = post_ratio,
    contrast_min = min(rr),
    contrast_geom = geom,
    contrast_log_geom = log(geom),
    contrast_pct = 100 * (geom - 1)
  )
}

calc_event_contrast_stats <- function(isi, s_isi, e_isi,
                                      min_isi_sec = 0.001,
                                      robust_q = 0.90,
                                      context_k = 5L) {
  n <- length(isi)
  empty <- list(
    n_flank = 0L,
    n_flank_ctx = 0L,
    core_max = NA_real_,
    core_q = NA_real_,
    pre_ratio_max = NA_real_,
    post_ratio_max = NA_real_,
    contrast_min_max = NA_real_,
    contrast_geom_max = NA_real_,
    contrast_log_geom_max = NA_real_,
    contrast_pct_max = NA_real_,
    pre_ratio_q = NA_real_,
    post_ratio_q = NA_real_,
    contrast_min_q = NA_real_,
    contrast_geom_q = NA_real_,
    contrast_log_geom_q = NA_real_,
    contrast_pct_q = NA_real_,
    context_pre_ISI_sec = NA_real_,
    context_post_ISI_sec = NA_real_,
    context_pre_ratio_max = NA_real_,
    context_post_ratio_max = NA_real_,
    contrast_min_ctx_max = NA_real_,
    contrast_geom_ctx_max = NA_real_,
    contrast_log_geom_ctx_max = NA_real_,
    contrast_pct_ctx_max = NA_real_,
    context_pre_ratio_q = NA_real_,
    context_post_ratio_q = NA_real_,
    contrast_min_ctx_q = NA_real_,
    contrast_geom_ctx_q = NA_real_,
    contrast_log_geom_ctx_q = NA_real_,
    contrast_pct_ctx_q = NA_real_
  )
  
  if (!is.finite(s_isi) || !is.finite(e_isi) || s_isi < 2 || e_isi > n || e_isi < s_isi) {
    return(empty)
  }
  
  x <- valid_isi_values(isi[s_isi:e_isi], min_isi_sec)
  if (length(x) == 0) return(empty)
  
  robust_q <- clamp(robust_q, 0.50, 1.00)
  core_max <- max(x)
  core_q <- as.numeric(stats::quantile(x, robust_q, na.rm = TRUE, names = FALSE))
  
  pre <- if (s_isi > 2) isi[s_isi - 1] else NA_real_
  post <- if (e_isi < n) isi[e_isi + 1] else NA_real_
  pre <- if (is.finite(pre) && pre >= min_isi_sec) pre else NA_real_
  post <- if (is.finite(post) && post >= min_isi_sec) post else NA_real_
  
  imm_max <- calc_ratio_summary(pre, post, core_max)
  imm_q <- calc_ratio_summary(pre, post, core_q)
  
  ctx <- get_context_values(isi, s_isi, e_isi, k = context_k, min_isi_sec = min_isi_sec)
  ctx_max <- calc_ratio_summary(ctx$pre_median, ctx$post_median, core_max)
  ctx_q <- calc_ratio_summary(ctx$pre_median, ctx$post_median, core_q)
  
  list(
    n_flank = imm_q$n_flank,
    n_flank_ctx = ctx_q$n_flank,
    core_max = core_max,
    core_q = core_q,
    
    pre_ratio_max = imm_max$pre_ratio,
    post_ratio_max = imm_max$post_ratio,
    contrast_min_max = imm_max$contrast_min,
    contrast_geom_max = imm_max$contrast_geom,
    contrast_log_geom_max = imm_max$contrast_log_geom,
    contrast_pct_max = imm_max$contrast_pct,
    
    pre_ratio_q = imm_q$pre_ratio,
    post_ratio_q = imm_q$post_ratio,
    contrast_min_q = imm_q$contrast_min,
    contrast_geom_q = imm_q$contrast_geom,
    contrast_log_geom_q = imm_q$contrast_log_geom,
    contrast_pct_q = imm_q$contrast_pct,
    
    context_pre_ISI_sec = ctx$pre_median,
    context_post_ISI_sec = ctx$post_median,
    
    context_pre_ratio_max = ctx_max$pre_ratio,
    context_post_ratio_max = ctx_max$post_ratio,
    contrast_min_ctx_max = ctx_max$contrast_min,
    contrast_geom_ctx_max = ctx_max$contrast_geom,
    contrast_log_geom_ctx_max = ctx_max$contrast_log_geom,
    contrast_pct_ctx_max = ctx_max$contrast_pct,
    
    context_pre_ratio_q = ctx_q$pre_ratio,
    context_post_ratio_q = ctx_q$post_ratio,
    contrast_min_ctx_q = ctx_q$contrast_min,
    contrast_geom_ctx_q = ctx_q$contrast_geom,
    contrast_log_geom_ctx_q = ctx_q$contrast_log_geom,
    contrast_pct_ctx_q = ctx_q$contrast_pct
  )
}
