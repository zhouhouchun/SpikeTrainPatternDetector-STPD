# Auto-generated modular extraction from modular reference script.
# Do not edit generated function names blindly; use git for version history.

# ============================================================
# Data I/O
# ============================================================

stpd_empty_task_events <- function() {
  data.frame(
    event_id = character(),
    event_name = character(),
    event_time_sec = numeric(),
    event_column = character(),
    event_index = integer(),
    trial_id = character(),
    source = character(),
    stringsAsFactors = FALSE
  )
}

stpd_is_task_event_column <- function(x) {
  x <- as.character(x %||% character(0))
  x0 <- trimws(x)
  grepl("^event($|[_ .:-])", x0, ignore.case = TRUE, perl = TRUE)
}

stpd_task_event_columns <- function(nms) {
  nms <- as.character(nms %||% character(0))
  nms[stpd_is_task_event_column(nms)]
}

stpd_clean_task_event_name <- function(x) {
  x <- as.character(x %||% "")
  x <- sub("^event([_ .:-]+)?", "", trimws(x), ignore.case = TRUE, perl = TRUE)
  x <- gsub("[_]+", " ", x)
  x <- gsub("\\s+", " ", x)
  x <- trimws(x)
  x[!nzchar(x)] <- "Event"
  x
}

stpd_normalize_task_events <- function(events, source = "") {
  if (is.null(events) || !is.data.frame(events) || nrow(events) == 0L) {
    return(stpd_empty_task_events())
  }
  n <- nrow(events)
  get_col <- function(nm, default) {
    if (nm %in% names(events)) return(events[[nm]])
    if (length(default) == n) default else rep(default, n)
  }
  tt <- suppressWarnings(as.numeric(get_col("event_time_sec", NA_real_)))
  ok <- is.finite(tt)
  if (!any(ok)) return(stpd_empty_task_events())
  out <- data.frame(
    event_id = as.character(get_col("event_id", "")),
    event_name = as.character(get_col("event_name", "Event")),
    event_time_sec = tt,
    event_column = as.character(get_col("event_column", "")),
    event_index = suppressWarnings(as.integer(get_col("event_index", seq_len(n)))),
    trial_id = as.character(get_col("trial_id", "")),
    source = as.character(get_col("source", source %||% "")),
    stringsAsFactors = FALSE
  )
  out <- out[ok, , drop = FALSE]
  out$event_name[is.na(out$event_name) | !nzchar(trimws(out$event_name))] <- "Event"
  out$event_column[is.na(out$event_column)] <- ""
  out$source[is.na(out$source) | !nzchar(out$source)] <- as.character(source %||% "")
  out <- out[order(out$event_time_sec, out$event_name, out$event_index), , drop = FALSE]
  if (!all(is.finite(out$event_index))) out$event_index <- seq_len(nrow(out))
  missing_trial <- is.na(out$trial_id) | !nzchar(out$trial_id)
  out$trial_id[missing_trial] <- paste0("trial_", seq_len(sum(missing_trial)))
  out$trial_id <- make.unique(out$trial_id, sep = "_")
  missing_id <- is.na(out$event_id) | !nzchar(out$event_id)
  out$event_id[missing_id] <- paste0(
    "event_",
    seq_len(sum(missing_id)),
    "_",
    gsub("[^A-Za-z0-9]+", "_", out$event_name[missing_id]),
    "_",
    formatC(out$event_time_sec[missing_id], format = "f", digits = 6)
  )
  out$event_id <- make.unique(out$event_id, sep = "_")
  rownames(out) <- NULL
  out
}

stpd_extract_task_events_from_data_frame <- function(df, unit_in = c("s", "ms"), source = "") {
  unit_in <- match.arg(unit_in)
  if (is.null(df) || !is.data.frame(df) || ncol(df) == 0L) return(stpd_empty_task_events())
  event_cols <- stpd_task_event_columns(names(df))
  if (length(event_cols) == 0L) return(stpd_empty_task_events())
  rows <- list()
  for (cc in event_cols) {
    x <- suppressWarnings(as.numeric(df[[cc]]))
    ok <- is.finite(x)
    if (!any(ok)) next
    tt <- to_sec(x[ok], unit_in)
    idx <- which(ok)
    nm <- stpd_clean_task_event_name(cc)
    rows[[length(rows) + 1L]] <- data.frame(
      event_id = paste0("event_", seq_along(tt), "_", gsub("[^A-Za-z0-9]+", "_", nm), "_", formatC(tt, format = "f", digits = 6)),
      event_name = rep(nm, length(tt)),
      event_time_sec = tt,
      event_column = rep(cc, length(tt)),
      event_index = idx,
      trial_id = paste0(gsub("[^A-Za-z0-9]+", "_", nm), "_", seq_along(tt)),
      source = rep(as.character(source %||% ""), length(tt)),
      stringsAsFactors = FALSE
    )
  }
  out <- do.call(rbind, rows)
  stpd_normalize_task_events(out, source = source)
}

stpd_extract_task_events_from_raw <- function(path, header = TRUE, unit_in = c("s", "ms")) {
  unit_in <- match.arg(unit_in)
  df <- read_csv_fast(path, header = header)
  stpd_extract_task_events_from_data_frame(df, unit_in = unit_in, source = basename(path))
}

stpd_task_events_for_slicetca <- function(events, event_names = NULL) {
  events <- stpd_normalize_task_events(events)
  if (nrow(events) == 0L) return(data.frame())
  if (!is.null(event_names) && length(event_names) > 0L) {
    events <- events[as.character(events$event_name) %in% as.character(event_names), , drop = FALSE]
  }
  if (nrow(events) == 0L) return(data.frame())
  data.frame(
    trial_id = events$trial_id,
    event_time_sec = events$event_time_sec,
    condition = events$event_name,
    event_id = events$event_id,
    stringsAsFactors = FALSE
  )
}

build_trains_from_raw_impl <- function(path, header = TRUE, unit_in = c("s", "ms"), duplicate_policy = c("error_keep", "warn_keep", "collapse_exact")) {
  unit_in <- match.arg(unit_in)
  duplicate_policy <- match.arg(duplicate_policy)
  df <- read_csv_fast(path, header = header)
  df <- df[, colSums(!is.na(df)) > 0, drop = FALSE]
  if (ncol(df) == 0) stop("Raw CSV has no valid columns (all NA).")
  event_cols <- stpd_task_event_columns(colnames(df))
  if (length(event_cols) > 0L) {
    df <- df[, setdiff(colnames(df), event_cols), drop = FALSE]
  }
  if (ncol(df) == 0) stop("Raw CSV has task-event columns but no valid spike train columns.")
  
  out <- list()
  for (tr in colnames(df)) {
    x <- df[[tr]]
    x <- x[!is.na(x)]
    x <- suppressWarnings(as.numeric(x))
    x <- x[is.finite(x)]
    if (length(x) == 0) next
    
    x_sec_input <- to_sec(x, unit_in)
    d_input <- if (length(x_sec_input) >= 2) diff(x_sec_input) else numeric(0)
    n_nonmono_input <- sum(is.finite(d_input) & d_input < 0, na.rm = TRUE)
    n_dup_input <- sum(is.finite(d_input) & d_input == 0, na.rm = TRUE)
    n_nonpos_input <- sum(is.finite(d_input) & d_input <= 0, na.rm = TRUE)
    input_was_unsorted <- n_nonmono_input > 0
    ord <- order(x_sec_input)
    x <- x_sec_input[ord]
    input_order_sorted <- ord
    n_dup_sorted <- if (length(x) >= 2) sum(diff(x) == 0, na.rm = TRUE) else 0L
    n_dropped_duplicates <- 0L
    if (identical(duplicate_policy, "collapse_exact") && length(x) > 0) {
      keep <- !duplicated(x)
      n_dropped_duplicates <- sum(!keep, na.rm = TRUE)
      x <- x[keep]
      input_order_sorted <- input_order_sorted[keep]
    }
    isi <- c(NA_real_, diff(x))
    
    out[[tr]] <- data.frame(
      idx = seq_along(x),
      timestamp_sec = x,
      ISI_sec = isi,
      input_order_idx = input_order_sorted,
      input_was_unsorted = rep(input_was_unsorted, length(x)),
      duplicate_timestamp_policy = rep(duplicate_policy, length(x)),
      n_dropped_duplicate_timestamps = rep(as.integer(n_dropped_duplicates), length(x)),
      n_duplicate_timestamps_sorted_pre_policy = rep(as.integer(n_dup_sorted), length(x)),
      n_nonmonotonic_input_steps = rep(as.integer(n_nonmono_input), length(x)),
      n_duplicate_timestamps_input_order = rep(as.integer(n_dup_input), length(x)),
      n_zero_or_negative_input_order = rep(as.integer(n_nonpos_input), length(x)),
      pattern_manual = rep("", length(x)),
      pattern_manual_negative = rep("", length(x)),
      pattern_auto = rep("", length(x)),
      stringsAsFactors = FALSE
    )
  }
  if (length(out) == 0) stop("No valid spike train found in raw CSV.")
  out
}

build_trains_from_annot <- function(path, unit_in = c("s", "ms"), duplicate_policy = c("error_keep", "warn_keep", "collapse_exact")) {
  unit_in <- match.arg(unit_in)
  duplicate_policy <- match.arg(duplicate_policy)
  df <- read_csv_fast(path, header = TRUE)
  cn <- colnames(df)
  ts_cols <- grep("_timestamp$", cn, value = TRUE)
  if (length(ts_cols) == 0) stop("Labeled CSV: cannot find *_timestamp columns.")
  
  out <- list()
  for (ts_col in ts_cols) {
    tr_name <- sub("_timestamp$", "", ts_col)
    isi_col <- paste0(tr_name, "_ISI")
    
    man_candidates <- c(
      paste0(tr_name, "_pattern_manual"),
      paste0(tr_name, "_manual"),
      paste0(tr_name, "_pattern")
    )
    neg_candidates <- c(
      paste0(tr_name, "_pattern_manual_negative"),
      paste0(tr_name, "_manual_negative"),
      paste0(tr_name, "_not_burst")
    )
    auto_candidates <- c(
      paste0(tr_name, "_pattern_auto"),
      paste0(tr_name, "_auto")
    )
    final_candidates <- c(
      paste0(tr_name, "_pattern_final"),
      paste0(tr_name, "_final")
    )
    
    if (!(isi_col %in% cn)) next
    
    ts_raw <- suppressWarnings(as.numeric(df[[ts_col]]))
    isi_raw <- suppressWarnings(as.numeric(df[[isi_col]]))
    valid <- is.finite(ts_raw)
    if (!any(valid)) next
    valid_idx <- which(valid)
    ts_vec_input <- to_sec(ts_raw[valid_idx], unit_in)
    d_input <- if (length(ts_vec_input) >= 2) diff(ts_vec_input) else numeric(0)
    n_nonmono_input <- sum(is.finite(d_input) & d_input < 0, na.rm = TRUE)
    n_dup_input <- sum(is.finite(d_input) & d_input == 0, na.rm = TRUE)
    n_nonpos_input <- sum(is.finite(d_input) & d_input <= 0, na.rm = TRUE)
    input_was_unsorted <- n_nonmono_input > 0
    ts_vec <- ts_vec_input
    isi_file_vec <- to_sec(isi_raw[valid_idx], unit_in)
    ord <- order(ts_vec)
    ts_vec <- ts_vec[ord]
    isi_file_vec <- isi_file_vec[ord]
    valid_idx_sorted <- valid_idx[ord]
    n_dup_sorted <- if (length(ts_vec) >= 2) sum(diff(ts_vec) == 0, na.rm = TRUE) else 0L
    n_dropped_duplicates <- 0L
    duplicate_keep <- rep(TRUE, length(ts_vec))
    if (identical(duplicate_policy, "collapse_exact") && length(ts_vec) > 0) {
      duplicate_keep <- !duplicated(ts_vec)
      n_dropped_duplicates <- sum(!duplicate_keep, na.rm = TRUE)
      ts_vec <- ts_vec[duplicate_keep]
      isi_file_vec <- isi_file_vec[duplicate_keep]
      valid_idx_sorted <- valid_idx_sorted[duplicate_keep]
    }
    # refined: timestamp-derived ISI is authoritative; external labeled CSV
    # ISI columns are retained for QC but not trusted blindly.
    isi_vec <- c(NA_real_, diff(ts_vec))
    
    manual <- rep("", length(ts_vec))
    manual_negative <- rep("", length(ts_vec))
    auto <- rep("", length(ts_vec))
    
    man_col <- man_candidates[man_candidates %in% cn][1]
    neg_col <- neg_candidates[neg_candidates %in% cn][1]
    auto_col <- auto_candidates[auto_candidates %in% cn][1]
    final_col <- final_candidates[final_candidates %in% cn][1]
    
    if (!is.na(man_col) && length(man_col) == 1) {
      v <- as.character(df[[man_col]][valid_idx]); v[is.na(v)] <- ""
      manual <- v[ord][duplicate_keep]
    } else if (!is.na(final_col) && length(final_col) == 1) {
      v <- as.character(df[[final_col]][valid_idx]); v[is.na(v)] <- ""
      manual <- v[ord][duplicate_keep]
    }
    
    if (!is.na(neg_col) && length(neg_col) == 1) {
      v <- as.character(df[[neg_col]][valid_idx]); v[is.na(v)] <- ""
      manual_negative <- v[ord][duplicate_keep]
    }
    if (!is.na(auto_col) && length(auto_col) == 1) {
      v <- as.character(df[[auto_col]][valid_idx]); v[is.na(v)] <- ""
      auto <- v[ord][duplicate_keep]
    }
    
    out[[tr_name]] <- data.frame(
      idx = seq_along(ts_vec),
      timestamp_sec = ts_vec,
      input_order_idx = valid_idx_sorted,
      input_was_unsorted = rep(input_was_unsorted, length(ts_vec)),
      duplicate_timestamp_policy = rep(duplicate_policy, length(ts_vec)),
      n_dropped_duplicate_timestamps = rep(as.integer(n_dropped_duplicates), length(ts_vec)),
      n_duplicate_timestamps_sorted_pre_policy = rep(as.integer(n_dup_sorted), length(ts_vec)),
      n_nonmonotonic_input_steps = rep(as.integer(n_nonmono_input), length(ts_vec)),
      n_duplicate_timestamps_input_order = rep(as.integer(n_dup_input), length(ts_vec)),
      n_zero_or_negative_input_order = rep(as.integer(n_nonpos_input), length(ts_vec)),
      ISI_sec = isi_vec,
      ISI_file_sec = isi_file_vec,
      ISI_file_mismatch_sec = abs(isi_file_vec - isi_vec),
      pattern_manual = manual,
      pattern_manual_negative = manual_negative,
      pattern_auto = auto,
      stringsAsFactors = FALSE
    )
  }
  if (length(out) == 0) stop("No valid spike train found in labeled CSV.")
  out
}


# ============================================================
# Duplicate timestamp cleanup utilities (duplicate timestamp)
# ============================================================

collapse_duplicate_timestamps_train <- function(dat, policy_label = "collapse_manual") {
  if (is.null(dat) || nrow(dat) == 0 || !("timestamp_sec" %in% names(dat))) {
    return(list(data = dat, dropped = 0L, duplicate_groups = 0L))
  }
  ts0 <- suppressWarnings(as.numeric(dat$timestamp_sec))
  finite <- is.finite(ts0)
  if (!any(finite)) return(list(data = dat, dropped = 0L, duplicate_groups = 0L))

  # The detector assumes sorted timestamps. Keep the first occurrence in sorted
  # order and recompute indices/ISIs after removing exact duplicates.
  ord <- order(ts0, seq_along(ts0), na.last = TRUE)
  dat_sorted <- dat[ord, , drop = FALSE]
  ts <- suppressWarnings(as.numeric(dat_sorted$timestamp_sec))
  dup_flag <- is.finite(ts) & duplicated(ts)
  dropped <- sum(dup_flag, na.rm = TRUE)
  duplicate_groups <- length(unique(ts[is.finite(ts) & duplicated(ts)]))
  if (dropped == 0L) {
    # Still normalize ordering/ISI for safety, but report no deletion.
    dat_sorted$idx <- seq_len(nrow(dat_sorted))
    dat_sorted$ISI_sec <- c(NA_real_, diff(suppressWarnings(as.numeric(dat_sorted$timestamp_sec))))
    return(list(data = dat_sorted, dropped = 0L, duplicate_groups = 0L))
  }

  keep <- !dup_flag
  out <- dat_sorted[keep, , drop = FALSE]
  out$idx <- seq_len(nrow(out))
  out$timestamp_sec <- suppressWarnings(as.numeric(out$timestamp_sec))
  out$ISI_sec <- c(NA_real_, diff(out$timestamp_sec))

  old_dropped <- 0L
  if ("n_dropped_duplicate_timestamps" %in% names(out)) {
    old_dropped <- suppressWarnings(max(as.integer(out$n_dropped_duplicate_timestamps), na.rm = TRUE))
    if (!is.finite(old_dropped)) old_dropped <- 0L
  }
  out$n_dropped_duplicate_timestamps <- as.integer(old_dropped + dropped)

  # Keep a record that the dataset has been explicitly collapsed after loading.
  out$duplicate_timestamp_policy <- as.character(policy_label)

  # Current data no longer contain exact duplicates, but keep pre-policy metadata
  # when present and ensure input-order anomaly columns exist.
  if (!("n_duplicate_timestamps_sorted_pre_policy" %in% names(out))) {
    out$n_duplicate_timestamps_sorted_pre_policy <- as.integer(dropped)
  }
  if (!("n_nonmonotonic_input_steps" %in% names(out))) out$n_nonmonotonic_input_steps <- 0L
  if (!("n_duplicate_timestamps_input_order" %in% names(out))) out$n_duplicate_timestamps_input_order <- 0L
  if (!("n_zero_or_negative_input_order" %in% names(out))) out$n_zero_or_negative_input_order <- 0L
  if (!("input_was_unsorted" %in% names(out))) out$input_was_unsorted <- FALSE

  # If an external ISI column exists, retain it but recompute mismatch against
  # timestamp-derived ISI for the kept rows.
  if ("ISI_file_sec" %in% names(out)) {
    file_isi <- suppressWarnings(as.numeric(out$ISI_file_sec))
    calc_isi <- suppressWarnings(as.numeric(out$ISI_sec))
    out$ISI_file_mismatch_sec <- abs(file_isi - calc_isi)
  }

  list(data = out, dropped = as.integer(dropped), duplicate_groups = as.integer(duplicate_groups))
}

collapse_duplicate_timestamps_trains <- function(trains, policy_label = "collapse_manual") {
  if (is.null(trains) || length(trains) == 0) {
    return(list(trains = trains, summary = data.frame()))
  }
  res <- lapply(names(trains), function(tr) {
    x <- collapse_duplicate_timestamps_train(trains[[tr]], policy_label = policy_label)
    list(train = tr, data = x$data, dropped = x$dropped, duplicate_groups = x$duplicate_groups)
  })
  out_trains <- setNames(lapply(res, function(x) x$data), names(trains))
  summary <- data.frame(
    train = vapply(res, `[[`, character(1), "train"),
    dropped_duplicate_spikes = vapply(res, function(x) as.integer(x$dropped), integer(1)),
    duplicate_groups = vapply(res, function(x) as.integer(x$duplicate_groups), integer(1)),
    stringsAsFactors = FALSE
  )
  list(trains = out_trains, summary = summary)
}
