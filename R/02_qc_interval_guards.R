# Auto-generated modular extraction from modular reference script.
# Do not edit generated function names blindly; use git for version history.

# ============================================================
# Data quality checks and interval guards
# ============================================================

valid_isi_interval <- function(s_isi, e_isi, n, require_flanks = FALSE) {
  s_isi <- suppressWarnings(as.integer(s_isi))
  e_isi <- suppressWarnings(as.integer(e_isi))
  n <- suppressWarnings(as.integer(n))
  if (!is.finite(s_isi) || !is.finite(e_isi) || !is.finite(n)) return(FALSE)
  if (s_isi < 2L || e_isi > n || e_isi < s_isi) return(FALSE)
  if (isTRUE(require_flanks) && (s_isi < 3L || e_isi > (n - 1L))) return(FALSE)
  TRUE
}

read_csv_fast <- function(path, header = TRUE) {
  if (requireNamespace("data.table", quietly = TRUE)) {
    return(as.data.frame(data.table::fread(path, header = header, data.table = FALSE, check.names = FALSE)))
  }
  read.csv(path, header = header, check.names = FALSE)
}

validate_train_quality <- function(dat, train = "", min_isi_sec = 0.001, unit_hint = "s", refractory_suspect_sec = 0.0010, display_unit = "s") {
  if (is.null(dat) || nrow(dat) == 0) {
    return(data.frame(
      train = train, n_spikes = 0L, duration_sec = NA_real_, isi_rate_Hz = NA_real_, firing_rate_Hz = NA_real_, mean_rate_Hz = NA_real_,
      artifact_threshold_sec = min_isi_sec, refractory_suspect_threshold_sec = refractory_suspect_sec,
      qc_time_unit = display_unit, artifact_threshold = NA_character_, refractory_suspect_threshold = NA_character_, raw_min_ISI = NA_character_,
      raw_min_ISI_sec = NA_real_, min_ISI_sec = NA_real_, min_valid_ISI_sec = NA_real_,
      artifact_min_ISI_sec = NA_real_, artifact_ISI_preview_sec = "",
      n_refractory_suspect_ISI = 0L, refractory_suspect_fraction = NA_real_, refractory_suspect_ISI_preview_sec = "",
      max_ISI_sec = NA_real_, median_ISI_sec = NA_real_,
      n_duplicate_timestamps = 0L, n_zero_or_negative_ISI = 0L, n_zero_or_negative_timestamp_steps = 0L,
      input_was_unsorted = FALSE, n_nonmonotonic_input_steps = 0L,
      n_duplicate_timestamps_input_order = 0L, n_zero_or_negative_input_order = 0L,
      n_artifact_ISI = 0L, artifact_fraction = NA_real_, n_valid_ISI = 0L,
      percentile_status = "unavailable", timestamp_ISI_mismatch = FALSE,
      warning_level = "error", warning_message = "empty train", stringsAsFactors = FALSE
    ))
  }

  ts <- suppressWarnings(as.numeric(dat$timestamp_sec))
  isi <- suppressWarnings(as.numeric(dat$ISI_sec))
  display_unit <- if (identical(display_unit, "ms")) "ms" else "s"
  time_scale <- if (identical(display_unit, "ms")) 1000 else 1
  fmt_num_plain <- function(x, digits = 6) {
    x <- suppressWarnings(as.numeric(x))
    if (length(x) == 0 || !is.finite(x[1])) return("")
    out <- formatC(x[1], format = "f", digits = digits)
    out <- sub("\\.?0+$", "", out)
    if (!nzchar(out) || identical(out, "-0")) out <- "0"
    out
  }
  fmt_time_plain <- function(x) fmt_num_plain(x * time_scale, digits = if (identical(display_unit, "ms")) 6 else 6)
  fmt_time_values <- function(values, max_n = 12L) {
    values <- suppressWarnings(as.numeric(values)); values <- values[is.finite(values)]
    if (length(values) == 0) return(c(text = "", more = ""))
    txt <- paste(vapply(head(values, max_n), fmt_time_plain, character(1)), collapse = ", ")
    more <- if (length(values) > max_n) paste0(" +", length(values) - max_n, " more") else ""
    c(text = txt, more = more)
  }
  n <- length(ts)
  dur <- if (n >= 2 && all(is.finite(c(ts[1], ts[n])))) ts[n] - ts[1] else NA_real_
  dts <- if (n >= 2) diff(ts) else numeric(0)
  dup_ts <- sum(is.finite(dts) & dts == 0, na.rm = TRUE)
  nonpos_ts <- sum(is.finite(dts) & dts <= 0, na.rm = TRUE)
  isi_row <- seq_along(isi)
  nonpos_isi <- sum(is.finite(isi) & isi_row >= 2L & isi <= 0, na.rm = TRUE)
  nonpos <- max(nonpos_ts, nonpos_isi, na.rm = TRUE)

  const_int <- function(...) {
    for (nm in c(...)) {
      if (nm %in% names(dat)) {
        v <- suppressWarnings(as.integer(dat[[nm]][1]))
        if (length(v) == 1L && is.finite(v)) return(v)
      }
    }
    0L
  }
  const_bool <- function(nm) {
    if (nm %in% names(dat)) {
      v <- dat[[nm]][1]
      if (!is.na(v)) return(isTRUE(as.logical(v)))
    }
    FALSE
  }
  nonmono_input <- const_int("n_nonmonotonic_input_steps")
  dup_input <- const_int("n_duplicate_input_timestamps", "n_duplicate_timestamps_input_order")
  nonpos_input <- const_int("n_zero_or_negative_input_steps", "n_zero_or_negative_input_order")
  input_was_unsorted <- const_bool("input_was_unsorted") || nonmono_input > 0
  duplicate_policy <- if ("duplicate_timestamp_policy" %in% names(dat)) as.character(dat$duplicate_timestamp_policy[1]) else "error_keep"
  if (is.na(duplicate_policy) || !nzchar(duplicate_policy)) duplicate_policy <- "error_keep"
  n_dropped_duplicates <- const_int("n_dropped_duplicate_timestamps")
  n_dup_sorted_pre_policy <- const_int("n_duplicate_timestamps_sorted_pre_policy")

  art <- is_artifact_isi(isi, min_isi_sec)
  if (length(art) > 0) art[1] <- FALSE

  refractory_suspect_sec <- suppressWarnings(as.numeric(refractory_suspect_sec %||% 0.0010))
  tol_ref <- max(1e-12, abs(refractory_suspect_sec) * 1e-6)
  refr <- is.finite(isi) & isi >= min_isi_sec & isi < (refractory_suspect_sec - tol_ref)
  if (length(refr) > 0) refr[1] <- FALSE

  valid <- is.finite(isi) & isi >= min_isi_sec
  if (length(valid) > 0) valid[1] <- FALSE
  finite_isi <- is.finite(isi)
  if (length(finite_isi) > 0) finite_isi[1] <- FALSE

  n_valid <- sum(valid, na.rm = TRUE)
  raw_min_isi <- if (any(finite_isi, na.rm = TRUE)) min(isi[finite_isi], na.rm = TRUE) else NA_real_
  valid_min_isi <- if (any(valid, na.rm = TRUE)) min(isi[valid], na.rm = TRUE) else NA_real_
  artifact_min_isi <- if (any(art, na.rm = TRUE)) min(isi[art], na.rm = TRUE) else NA_real_
  max_isi <- if (any(finite_isi, na.rm = TRUE)) max(isi[finite_isi], na.rm = TRUE) else NA_real_

  mismatch <- FALSE
  mismatch_n <- NA_integer_
  if ("ISI_file_sec" %in% names(dat)) {
    file_isi <- suppressWarnings(as.numeric(dat$ISI_file_sec))
    calc_isi <- c(NA_real_, diff(ts))
    ok <- is.finite(file_isi) & is.finite(calc_isi)
    mismatch_n <- sum(ok & abs(file_isi - calc_isi) > max(1e-9, min_isi_sec * 1e-3), na.rm = TRUE)
    mismatch <- mismatch_n > 0
  }

  percentile_status <- if (n_valid < 30) "disabled_lt30" else if (n_valid < 50) "weak_lt50" else "reliable"
  spike_rate <- if (is.finite(dur) && dur > 0) n / dur else NA_real_
  isi_rate <- if (is.finite(dur) && dur > 0 && n >= 2) (n - 1) / dur else NA_real_
  artifact_fraction <- if (n >= 2) sum(art, na.rm = TRUE) / max(1L, n - 1L) else NA_real_
  refractory_fraction <- if (n >= 2) sum(refr, na.rm = TRUE) / max(1L, n - 1L) else NA_real_
  n_art <- sum(art, na.rm = TRUE)
  n_refr <- sum(refr, na.rm = TRUE)

  art_prev <- fmt_time_values(isi[art])
  refr_prev <- fmt_time_values(isi[refr])

  level <- "ok"
  duplicate_integrity_problem <- nonpos_ts > 0 || dup_ts > 0
  isi_integrity_problem <- nonpos_isi > 0
  hard_duplicate_policy <- !(duplicate_policy %in% c("warn_keep", "collapse_exact"))
  if (!is.finite(dur) || dur <= 0) level <- "error"
  if (level == "ok" && (isi_integrity_problem || (duplicate_integrity_problem && hard_duplicate_policy))) level <- "error"
  if (level == "ok" && (duplicate_integrity_problem || input_was_unsorted || dup_input > 0 || nonpos_input > 0 || n_dropped_duplicates > 0 || n_art > 0 || n_refr > 0 || n_valid < 50 || mismatch)) level <- "warning"
  if (level == "ok" && is.finite(artifact_fraction) && artifact_fraction > 0.05) level <- "warning"
  if (level == "ok" && is.finite(isi_rate) && isi_rate > 200) level <- "warning"

  # QC message policy (QC message): table columns retain all QC metrics, while
  # warning_message lists only active warning/error triggers. Do not report
  # zero-count checks such as n_artifact_ISI=0 or n_refractory_suspect_ISI=0;
  # those values remain available as separate table columns.
  messages <- character(0)
  if (!is.finite(dur) || dur <= 0) messages <- c(messages, "invalid_duration=TRUE")
  if (dup_ts > 0) messages <- c(messages, paste0("duplicate_timestamps=", dup_ts, "; policy=", duplicate_policy))
  if (nonpos_isi > 0) messages <- c(messages, paste0("zero_or_negative_ISI=", nonpos_isi))
  if (nonpos_ts > 0 && nonpos_ts != nonpos_isi) messages <- c(messages, paste0("zero_or_negative_timestamp_steps=", nonpos_ts))
  if (n_dropped_duplicates > 0) messages <- c(messages, paste0("dropped_duplicate_timestamps=", n_dropped_duplicates, "; policy=collapse_exact"))
  if (n_art > 0) {
    messages <- c(messages, paste0("n_artifact_ISI=", n_art))
    messages <- c(messages, paste0("artifact_ISI_", display_unit, "=[", art_prev["text"], "] (<", fmt_time_plain(min_isi_sec), " ", display_unit, ")", art_prev["more"]))
  }
  if (n_refr > 0) {
    messages <- c(messages, paste0("n_refractory_suspect_ISI=", n_refr))
    messages <- c(messages, paste0("refractory_suspect_ISI_", display_unit, "=[", refr_prev["text"], "] (<", fmt_time_plain(refractory_suspect_sec), " ", display_unit, ")", refr_prev["more"]))
  }
  if (input_was_unsorted) messages <- c(messages, paste0("input_was_unsorted=TRUE; n_nonmonotonic_input_steps=", nonmono_input, "; timestamps sorted for detection"))
  if (dup_input > 0 && dup_input != dup_ts) messages <- c(messages, paste0("n_duplicate_timestamps_input_order=", dup_input))
  if (nonpos_input > 0 && nonpos_input != nonpos) messages <- c(messages, paste0("n_zero_or_negative_input_order=", nonpos_input))
  if (is.finite(artifact_fraction) && artifact_fraction > 0.05) messages <- c(messages, paste0("artifact_fraction=", signif(artifact_fraction, 4), " (>0.05)"))
  if (n_valid < 30) messages <- c(messages, paste0("percentile_status=", percentile_status, "; percentile_constraints=disabled"))
  else if (n_valid < 50) messages <- c(messages, paste0("percentile_status=", percentile_status, "; percentile_constraints=weak"))
  if (is.finite(isi_rate) && isi_rate > 200) messages <- c(messages, paste0("very_high_isi_rate_Hz=", round(isi_rate, 2)))
  if (mismatch) messages <- c(messages, paste0("timestamp_ISI_mismatch=", mismatch_n, " rows; timestamp-derived ISI used"))

  data.frame(
    train = train,
    n_spikes = as.integer(n),
    duration_sec = dur,
    isi_rate_Hz = isi_rate,
    firing_rate_Hz = isi_rate,
    spike_rate_Hz = spike_rate,
    mean_rate_Hz = isi_rate,
    artifact_threshold_sec = min_isi_sec,
    refractory_suspect_threshold_sec = refractory_suspect_sec,
    qc_time_unit = display_unit,
    artifact_threshold = paste0(fmt_time_plain(min_isi_sec), " ", display_unit),
    refractory_suspect_threshold = paste0(fmt_time_plain(refractory_suspect_sec), " ", display_unit),
    raw_min_ISI = if (is.finite(raw_min_isi)) paste0(fmt_time_plain(raw_min_isi), " ", display_unit) else "",
    min_valid_ISI = if (is.finite(valid_min_isi)) paste0(fmt_time_plain(valid_min_isi), " ", display_unit) else "",
    artifact_min_ISI = if (is.finite(artifact_min_isi)) paste0(fmt_time_plain(artifact_min_isi), " ", display_unit) else "",
    raw_min_ISI_sec = raw_min_isi,
    min_ISI_sec = raw_min_isi,
    min_valid_ISI_sec = valid_min_isi,
    artifact_min_ISI_sec = artifact_min_isi,
    artifact_ISI_preview_sec = if (n_art > 0) paste0("[", art_prev["text"], "] ", display_unit, art_prev["more"]) else "",
    n_refractory_suspect_ISI = as.integer(n_refr),
    refractory_suspect_fraction = refractory_fraction,
    refractory_suspect_ISI_preview_sec = if (n_refr > 0) paste0("[", refr_prev["text"], "] ", display_unit, refr_prev["more"]) else "",
    max_ISI_sec = max_isi,
    median_ISI_sec = safe_median(isi[valid], default = NA_real_),
    n_duplicate_timestamps = as.integer(dup_ts),
    n_zero_or_negative_ISI = as.integer(nonpos_isi),
    n_zero_or_negative_timestamp_steps = as.integer(nonpos_ts),
    input_was_unsorted = as.logical(input_was_unsorted),
    duplicate_timestamp_policy = duplicate_policy,
    n_dropped_duplicate_timestamps = as.integer(n_dropped_duplicates),
    n_duplicate_timestamps_sorted_pre_policy = as.integer(n_dup_sorted_pre_policy),
    n_nonmonotonic_input_steps = as.integer(nonmono_input),
    n_duplicate_timestamps_input_order = as.integer(dup_input),
    n_zero_or_negative_input_order = as.integer(nonpos_input),
    n_artifact_ISI = as.integer(n_art),
    artifact_fraction = artifact_fraction,
    n_valid_ISI = as.integer(n_valid),
    percentile_status = percentile_status,
    timestamp_ISI_mismatch = mismatch,
    warning_level = level,
    warning_message = paste(messages, collapse = "; "),
    stringsAsFactors = FALSE
  )
}

validate_dataset_quality_impl <- function(trains, min_isi_sec = 0.001, unit_hint = "s", refractory_suspect_sec = 0.0010, display_unit = "s") {
  if (is.null(trains) || length(trains) == 0) return(data.frame())
  q <- bind_rows(lapply(names(trains), function(tr) {
    validate_train_quality(trains[[tr]], train = tr, min_isi_sec = min_isi_sec, unit_hint = unit_hint,
                                   refractory_suspect_sec = refractory_suspect_sec, display_unit = display_unit)
  }))
  if (nrow(q) == 0) return(q)
  if (!("firing_rate_Hz" %in% names(q)) && "mean_rate_Hz" %in% names(q)) q$firing_rate_Hz <- q$mean_rate_Hz

  # modular reference: add a lightweight nonstationarity check. It is advisory and
  # does not change detector labels. It flags train-level shifts in the sliding
  # median ISI, especially relevant for pause threshold interpretation.
  stat <- tryCatch(stationarity_qc(trains, min_isi_sec = min_isi_sec), error = function(e) data.frame())
  if (!is.null(stat) && nrow(stat) > 0 && "train" %in% names(stat)) {
    q <- dplyr::left_join(q, stat, by = "train")
    # Stationarity is an interpretation risk, not a data-integrity error.
    # It may make global pause thresholds unreliable, but it should not elevate
    # the whole train to ERROR unless timestamp/artifact checks already did so.
    idx_stat <- which(q$stationarity_status %in% c("warning", "error", "nonstationary", "high_drift", "warning_high_drift") & !(q$warning_level %in% c("error")))
    if (length(idx_stat) > 0) q$warning_level[idx_stat] <- "warning"
    has_msg <- is.finite(suppressWarnings(as.numeric(q$stationarity_drift_ratio))) & q$stationarity_status %in% c("warning", "error", "nonstationary", "high_drift", "warning_high_drift")
    if (any(has_msg, na.rm = TRUE)) {
      stationarity_msg <- paste0("stationarity_status=", q$stationarity_status[has_msg],
                                 "; stationarity_drift_ratio=", signif(q$stationarity_drift_ratio[has_msg], 4),
                                 "; pause/global thresholds may be state-dependent")
      base_msg <- q$warning_message[has_msg]
      base_msg[is.na(base_msg) | base_msg == "OK"] <- ""
      q$warning_message[has_msg] <- ifelse(nzchar(base_msg), paste0(base_msg, "; ", stationarity_msg), stationarity_msg)
    }
  }

  first_cols <- c("warning_level", "train", "warning_message", "n_spikes", "duration_sec", "firing_rate_Hz",
                  "qc_time_unit", "raw_min_ISI", "artifact_threshold", "refractory_suspect_threshold",
                  "n_artifact_ISI", "n_refractory_suspect_ISI", "artifact_fraction", "n_valid_ISI", "percentile_status",
                  "stationarity_status", "stationarity_drift_ratio", "stationarity_warning")
  q[, c(intersect(first_cols, names(q)), setdiff(names(q), first_cols)), drop = FALSE]
}

artifact_isi_details <- function(trains, min_isi_sec = 0.001, display_unit = "s") {
  if (is.null(trains) || length(trains) == 0) return(data.frame())
  display_unit <- if (identical(display_unit, "ms")) "ms" else "s"
  time_scale <- if (identical(display_unit, "ms")) 1000 else 1
  rows <- lapply(names(trains), function(tr) {
    dat <- trains[[tr]]
    if (is.null(dat) || nrow(dat) < 2 || !("ISI_sec" %in% names(dat))) return(NULL)
    isi <- suppressWarnings(as.numeric(dat$ISI_sec))
    ts <- suppressWarnings(as.numeric(dat$timestamp_sec))
    art <- is_artifact_isi(isi, min_isi_sec)
    if (length(art) > 0) art[1] <- FALSE
    idx <- which(art & is.finite(isi))
    if (length(idx) == 0) return(NULL)
    data.frame(
      train = tr,
      isi_index = idx,
      left_spike_time_sec = ifelse(idx > 1, ts[idx - 1L], NA_real_),
      right_spike_time_sec = ts[idx],
      ISI_sec = isi[idx],
      ISI_value = isi[idx] * time_scale,
      artifact_threshold_sec = min_isi_sec,
      artifact_threshold_value = min_isi_sec * time_scale,
      qc_time_unit = display_unit,
      stringsAsFactors = FALSE
    )
  })
  out <- bind_rows(rows)
  if (is.null(out) || nrow(out) == 0) return(data.frame())
  out[order(out$train, out$isi_index), , drop = FALSE]
}

duplicate_timestamp_details <- function(trains, display_unit = "s") {
  if (is.null(trains) || length(trains) == 0) return(data.frame())
  display_unit <- if (identical(display_unit, "ms")) "ms" else "s"
  time_scale <- if (identical(display_unit, "ms")) 1000 else 1
  rows <- lapply(names(trains), function(tr) {
    dat <- trains[[tr]]
    if (is.null(dat) || nrow(dat) == 0 || !("timestamp_sec" %in% names(dat))) return(NULL)
    ts <- suppressWarnings(as.numeric(dat$timestamp_sec))
    if (!any(is.finite(ts))) return(NULL)
    dup_vals <- sort(unique(ts[is.finite(ts) & duplicated(ts)]))
    if (length(dup_vals) == 0) return(NULL)
    do.call(rbind, lapply(dup_vals, function(tv) {
      idx <- which(is.finite(ts) & ts == tv)
      data.frame(
        train = tr,
        timestamp_sec = tv,
        timestamp_value = tv * time_scale,
        qc_time_unit = display_unit,
        duplicate_count = length(idx),
        row_indices_sorted = paste(idx, collapse = ";"),
        input_order_indices = if ("input_order_idx" %in% names(dat)) paste(dat$input_order_idx[idx], collapse = ";") else "",
        policy = if ("duplicate_timestamp_policy" %in% names(dat)) as.character(dat$duplicate_timestamp_policy[idx[1]]) else "error_keep",
        stringsAsFactors = FALSE
      )
    }))
  })
  out <- bind_rows(rows)
  if (is.null(out) || nrow(out) == 0) return(data.frame())
  out[order(out$train, out$timestamp_sec), , drop = FALSE]
}

quality_notification_text <- function(qc) {
  if (is.null(qc) || nrow(qc) == 0) return("No quality table available.")
  bad <- qc[qc$warning_level %in% c("warning", "error"), , drop = FALSE]
  if (nrow(bad) == 0) return("Data quality check passed.")
  paste0(nrow(bad), " train(s) have quality warnings. Open the Data QC tab for details.")
}



# ============================================================
# visual visualization semantics
# ============================================================

pattern_palette <- function(mode = c("pattern_color", "source_priority")) {
  # Pattern identity is encoded by the thin horizontal strip.
  # Source/confidence is encoded separately by spike tick overlays.
  mode <- match.arg(mode)
  data.frame(
    pattern = c("burst", "long_burst", "possible_burst", "tonic",
                "high_frequency_tonic", "pause", "others", "high_frequency_spiking"),
    manual = c("#FB8DB8", "#825CD5", "#D7B6F5", "#CAF99D",
               "#63F28E", "#4BCEE6", "#FFC99E", "#FF5A59"),
    auto = c("#CF57D5", "#987FE2", "#B88AF2", "#95F163",
             "#15E261", "#175AEB", "#F6EB50", "#FF0D2B"),
    stringsAsFactors = FALSE
  )
}


# Unified Plotly hover label style used across raster, ISI profile, diagnostics,
# and histogram plots. This controls the tooltip box background, not the data
# colors or pattern strips.
stpd_hoverlabel_style <- function() {
  list(
    bgcolor = "#809AD6",
    bordercolor = "#809AD6",
    font = list(color = "#FFFFFF")
  )
}

# Vector-safe extended ISI metric hover text.
# IMPORTANT: Do not use ifelse(single_boolean, vector_text, "") here;
# base::ifelse() would recycle/truncate and can repeat the first row's
# range metrics across all hover labels.
extended_isi_metrics_hover <- function(linear_pct, log_pct, robust_log_pct, show = TRUE) {
  n <- max(length(linear_pct), length(log_pct), length(robust_log_pct), 1L)
  linear_pct <- rep_len(suppressWarnings(as.numeric(linear_pct)), n)
  log_pct <- rep_len(suppressWarnings(as.numeric(log_pct)), n)
  robust_log_pct <- rep_len(suppressWarnings(as.numeric(robust_log_pct)), n)
  if (!isTRUE(show)) return(rep("", n))
  out <- rep("", n)
  add <- function(x, label) {
    ifelse(is.finite(x), paste0("<br>", label, ": ", round(x, 2), "%"), "")
  }
  paste0(
    add(linear_pct, "ISI linear range position"),
    add(log_pct, "ISI log-range position"),
    add(robust_log_pct, "ISI robust log-range position")
  )
}

pattern_strip_style <- function(pattern, source = c("manual", "auto")) {
  source <- match.arg(source)
  pal <- pattern_palette("pattern_color")
  pat <- as.character(pattern %||% "")
  row <- pal[pal$pattern == pat, , drop = FALSE]
  if (nrow(row) == 0) {
    row <- data.frame(pattern = pat, manual = "#BDBDBD", auto = "#6B6B6B", stringsAsFactors = FALSE)
  }
  col <- if (identical(source, "manual")) row$manual[1] else row$auto[1]
  list(color = col, dash = "solid", width = 4.5)
}

source_spike_style <- function(source = c("manual", "auto", "review", "none")) {
  # visual visual semantics: vertical spike ticks are morphology-only.
  # They must not encode label source, confidence, or pattern identity, because
  # unequal darkness/width can be misread as different spike density.
  source <- match.arg(source)
  list(color = "#000000", dash = "solid", width = 1.0)
}

# Compatibility wrapper used by older plotting code. Pattern identity is now
# encoded by horizontal strips/overlays; spike ticks remain uniform black.
pattern_style <- function(pattern, source = c("manual", "auto", "review"), mode = c("source_priority", "pattern_color")) {
  source <- match.arg(source)
  source_spike_style(source)
}

base_spike_color <- function(train, mode = c("neutral", "by_train")) {
  # Keep the arguments for backward compatibility, but intentionally ignore
  # them. All spike ticks should have the same black solid style.
  "#000000"
}

base_spike_line_width <- function() 1.0
raster_label_line_width <- function() 1.0
pattern_strip_line_width <- function() 4.5

stable_train_color <- function(train) {
  # Deterministic train color; prevents Plotly's automatic palette from
  # changing when the visible time window or trace order changes.
  pal <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
           "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
           "#3366cc", "#dc3912", "#109618", "#990099", "#0099c6",
           "#dd4477", "#66aa00", "#b82e2e", "#316395", "#994499")
  key <- as.character(train %||% "")
  h <- digest::digest(key, algo = "xxhash32", serialize = FALSE)
  idx <- suppressWarnings(strtoi(substr(h, 1, 6), base = 16))
  if (!is.finite(idx)) idx <- sum(utf8ToInt(key))
  pal[(idx %% length(pal)) + 1L]
}
