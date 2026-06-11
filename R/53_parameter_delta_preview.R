# Parameter delta dry-run preview.
# These helpers rerun a small train subset with baseline and current parameters,
# compare AUTO events, and return only audit tables. They never mutate the input
# dataset or write into ds$results.

stpd_delta_event_table <- function(ds, params, selected_trains, source = c("auto", "final")) {
  source <- match.arg(source)
  if (is.null(ds) || is.null(ds$trains)) return(empty_events_tbl())
  target <- intersect(as.character(selected_trains %||% names(ds$trains)), names(ds$trains))
  if (length(target) == 0) return(empty_events_tbl())
  p <- effective_params_for_detector(params)
  min_isi <- suppressWarnings(as.numeric(p$detector$min_valid_isi_sec %||% 0.0009))
  if (!is.finite(min_isi)) min_isi <- 0.0009
  out <- derive_interval_tables(
    ds$trains[target],
    source = source,
    auto_others = FALSE,
    dataset_map = stats::setNames(rep(ds$meta$display_name %||% "dataset", length(target)), target),
    min_isi_sec = min_isi,
    contrast_q = p$burst$contrast_q %||% 0.90,
    context_k = p$burst$context_k %||% 5L
  )$events
  if (is.null(out) || nrow(out) == 0) return(empty_events_tbl())
  out$event_row_id <- seq_len(nrow(out))
  out
}

stpd_delta_match_events_any_label <- function(current_events, baseline_events, iou_min = 0.25) {
  if (is.null(current_events) || is.null(baseline_events) || nrow(current_events) == 0 || nrow(baseline_events) == 0) {
    return(data.frame(pred_index = integer(), truth_index = integer(), train = character(), pattern = character(), iou = numeric(), stringsAsFactors = FALSE))
  }
  cur <- as.data.frame(current_events, stringsAsFactors = FALSE)
  base <- as.data.frame(baseline_events, stringsAsFactors = FALSE)
  cur$.delta_any_label <- "event"
  base$.delta_any_label <- "event"
  stpd_match_events_greedy(cur, base, class_col = ".delta_any_label", iou_min = iou_min)
}

stpd_parameter_delta_count_table <- function(baseline_events, current_events) {
  patterns <- sort(unique(c(as.character(baseline_events$pattern %||% character()), as.character(current_events$pattern %||% character()))))
  patterns <- patterns[nzchar(patterns) & !is.na(patterns)]
  if (length(patterns) == 0) {
    return(data.frame(pattern = character(), baseline_n = integer(), current_n = integer(), delta_n = integer(), direction = character(), stringsAsFactors = FALSE))
  }
  base_tab <- table(factor(as.character(baseline_events$pattern %||% character()), levels = patterns))
  cur_tab <- table(factor(as.character(current_events$pattern %||% character()), levels = patterns))
  out <- data.frame(
    pattern = patterns,
    baseline_n = as.integer(base_tab),
    current_n = as.integer(cur_tab),
    stringsAsFactors = FALSE
  )
  out$delta_n <- out$current_n - out$baseline_n
  out$direction <- ifelse(out$delta_n > 0, "increased", ifelse(out$delta_n < 0, "decreased", "unchanged"))
  out
}

stpd_parameter_delta_event_diff <- function(baseline_events, current_events, iou_min = 0.25) {
  empty <- data.frame(
    status = character(),
    train = character(),
    baseline_pattern = character(),
    current_pattern = character(),
    baseline_start_isi = integer(),
    baseline_end_isi = integer(),
    current_start_isi = integer(),
    current_end_isi = integer(),
    iou = numeric(),
    baseline_event_id = integer(),
    current_event_id = integer(),
    stringsAsFactors = FALSE
  )
  base <- as.data.frame(baseline_events %||% empty_events_tbl(), stringsAsFactors = FALSE)
  cur <- as.data.frame(current_events %||% empty_events_tbl(), stringsAsFactors = FALSE)
  if (nrow(base) > 0 && !("event_row_id" %in% names(base))) base$event_row_id <- seq_len(nrow(base))
  if (nrow(cur) > 0 && !("event_row_id" %in% names(cur))) cur$event_row_id <- seq_len(nrow(cur))

  matches <- stpd_delta_match_events_any_label(cur, base, iou_min = iou_min)
  rows <- list()
  used_cur <- integer()
  used_base <- integer()
  if (!is.null(matches) && nrow(matches) > 0) {
    for (ii in seq_len(nrow(matches))) {
      ci <- as.integer(matches$pred_index[ii])
      bi <- as.integer(matches$truth_index[ii])
      if (!ci %in% seq_len(nrow(cur)) || !bi %in% seq_len(nrow(base))) next
      used_cur <- c(used_cur, ci)
      used_base <- c(used_base, bi)
      same_label <- identical(as.character(cur$pattern[ci]), as.character(base$pattern[bi]))
      same_span <- identical(as.integer(cur$start_isi[ci]), as.integer(base$start_isi[bi])) &&
        identical(as.integer(cur$end_isi[ci]), as.integer(base$end_isi[bi]))
      status <- if (!same_label) "label_changed" else if (!same_span) "boundary_changed" else "unchanged_event"
      rows[[length(rows) + 1L]] <- data.frame(
        status = status,
        train = as.character(cur$train[ci] %||% base$train[bi] %||% ""),
        baseline_pattern = as.character(base$pattern[bi] %||% ""),
        current_pattern = as.character(cur$pattern[ci] %||% ""),
        baseline_start_isi = as.integer(base$start_isi[bi] %||% NA_integer_),
        baseline_end_isi = as.integer(base$end_isi[bi] %||% NA_integer_),
        current_start_isi = as.integer(cur$start_isi[ci] %||% NA_integer_),
        current_end_isi = as.integer(cur$end_isi[ci] %||% NA_integer_),
        iou = as.numeric(matches$iou[ii] %||% NA_real_),
        baseline_event_id = as.integer(base$event_id[bi] %||% bi),
        current_event_id = as.integer(cur$event_id[ci] %||% ci),
        stringsAsFactors = FALSE
      )
    }
  }
  if (nrow(cur) > 0) {
    for (ci in setdiff(seq_len(nrow(cur)), used_cur)) {
      rows[[length(rows) + 1L]] <- data.frame(
        status = "added_event",
        train = as.character(cur$train[ci] %||% ""),
        baseline_pattern = "",
        current_pattern = as.character(cur$pattern[ci] %||% ""),
        baseline_start_isi = NA_integer_,
        baseline_end_isi = NA_integer_,
        current_start_isi = as.integer(cur$start_isi[ci] %||% NA_integer_),
        current_end_isi = as.integer(cur$end_isi[ci] %||% NA_integer_),
        iou = NA_real_,
        baseline_event_id = NA_integer_,
        current_event_id = as.integer(cur$event_id[ci] %||% ci),
        stringsAsFactors = FALSE
      )
    }
  }
  if (nrow(base) > 0) {
    for (bi in setdiff(seq_len(nrow(base)), used_base)) {
      rows[[length(rows) + 1L]] <- data.frame(
        status = "removed_event",
        train = as.character(base$train[bi] %||% ""),
        baseline_pattern = as.character(base$pattern[bi] %||% ""),
        current_pattern = "",
        baseline_start_isi = as.integer(base$start_isi[bi] %||% NA_integer_),
        baseline_end_isi = as.integer(base$end_isi[bi] %||% NA_integer_),
        current_start_isi = NA_integer_,
        current_end_isi = NA_integer_,
        iou = NA_real_,
        baseline_event_id = as.integer(base$event_id[bi] %||% bi),
        current_event_id = NA_integer_,
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(rows) == 0) return(empty)
  out <- dplyr::bind_rows(rows)
  out[order(out$status == "unchanged_event", out$train, out$baseline_start_isi, out$current_start_isi, out$baseline_pattern, out$current_pattern), , drop = FALSE]
}

stpd_parameter_delta_summary <- function(event_diff, count_table, selected_trains, parameter_changes, source = "auto") {
  event_diff <- event_diff %||% data.frame(status = character(), stringsAsFactors = FALSE)
  count_table <- count_table %||% data.frame()
  status_n <- table(factor(as.character(event_diff$status %||% character()), levels = c("added_event", "removed_event", "label_changed", "boundary_changed", "unchanged_event")))
  changed_n <- sum(event_diff$status %in% c("added_event", "removed_event", "label_changed", "boundary_changed"), na.rm = TRUE)
  data.frame(
    metric = c(
      "source_compared", "selected_train_n", "selected_trains", "parameter_change_n",
      "baseline_event_n", "current_event_n", "changed_event_n",
      "added_event_n", "removed_event_n", "label_changed_n", "boundary_changed_n", "unchanged_event_n"
    ),
    value = c(
      source,
      as.character(length(selected_trains)),
      paste(selected_trains, collapse = ";"),
      as.character(if (is.null(parameter_changes) || !("path" %in% names(parameter_changes))) 0L else nrow(parameter_changes)),
      as.character(sum(count_table$baseline_n %||% 0L, na.rm = TRUE)),
      as.character(sum(count_table$current_n %||% 0L, na.rm = TRUE)),
      as.character(changed_n),
      as.character(status_n[["added_event"]]),
      as.character(status_n[["removed_event"]]),
      as.character(status_n[["label_changed"]]),
      as.character(status_n[["boundary_changed"]]),
      as.character(status_n[["unchanged_event"]])
    ),
    stringsAsFactors = FALSE
  )
}

stpd_parameter_delta_overlay_rows <- function(preview, trains, selected_trains = NULL) {
  diff <- preview$event_diff %||% data.frame()
  if (is.null(diff) || nrow(diff) == 0 || is.null(trains) || length(trains) == 0) {
    return(data.frame(
      delta_row_index = integer(),
      status = character(),
      train = character(),
      baseline_pattern = character(),
      current_pattern = character(),
      start_isi = integer(),
      end_isi = integer(),
      start_align_sec = numeric(),
      end_align_sec = numeric(),
      iou = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  target <- as.character(selected_trains %||% names(trains))
  rows <- list()
  for (ii in seq_len(nrow(diff))) {
    tr <- as.character(diff$train[ii] %||% "")
    if (!nzchar(tr) || !(tr %in% names(trains)) || !(tr %in% target)) next
    dat <- trains[[tr]]
    if (is.null(dat) || nrow(dat) == 0 || !("timestamp_sec" %in% names(dat))) next
    starts <- suppressWarnings(as.integer(c(diff$baseline_start_isi[ii], diff$current_start_isi[ii])))
    ends <- suppressWarnings(as.integer(c(diff$baseline_end_isi[ii], diff$current_end_isi[ii])))
    starts <- starts[is.finite(starts)]
    ends <- ends[is.finite(ends)]
    if (length(starts) == 0 || length(ends) == 0) next
    s_isi <- max(1L, min(starts, na.rm = TRUE))
    e_isi <- max(ends, na.rm = TRUE)
    if (!is.finite(s_isi) || !is.finite(e_isi) || e_isi < s_isi) next
    s_spk <- max(1L, s_isi - 1L)
    e_spk <- min(nrow(dat), e_isi)
    if (s_spk > nrow(dat) || e_spk < 1L || e_spk < s_spk) next
    t0 <- suppressWarnings(as.numeric(dat$timestamp_sec[s_spk] - dat$timestamp_sec[1]))
    t1 <- suppressWarnings(as.numeric(dat$timestamp_sec[e_spk] - dat$timestamp_sec[1]))
    if (!is.finite(t0) || !is.finite(t1)) next
    rows[[length(rows) + 1L]] <- data.frame(
      delta_row_index = ii,
      status = as.character(diff$status[ii] %||% ""),
      train = tr,
      baseline_pattern = as.character(diff$baseline_pattern[ii] %||% ""),
      current_pattern = as.character(diff$current_pattern[ii] %||% ""),
      start_isi = s_isi,
      end_isi = e_isi,
      start_align_sec = min(t0, t1),
      end_align_sec = max(t0, t1),
      iou = suppressWarnings(as.numeric(diff$iou[ii] %||% NA_real_)),
      stringsAsFactors = FALSE
    )
  }
  if (length(rows) == 0) {
    return(data.frame(
      delta_row_index = integer(),
      status = character(),
      train = character(),
      baseline_pattern = character(),
      current_pattern = character(),
      start_isi = integer(),
      end_isi = integer(),
      start_align_sec = numeric(),
      end_align_sec = numeric(),
      iou = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  out <- dplyr::bind_rows(rows)
  for (nm in c("status", "train", "baseline_pattern", "current_pattern")) {
    out[[nm]][is.na(out[[nm]])] <- ""
  }
  out
}

stpd_parameter_delta_export <- function(preview, out_dir) {
  if (is.null(preview) || !is.list(preview)) stop("No parameter delta preview is available to export.", call. = FALSE)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  summary <- preview$summary %||% data.frame(message = "No parameter delta preview summary.", stringsAsFactors = FALSE)
  counts <- preview$counts %||% data.frame(message = "No parameter delta preview counts.", stringsAsFactors = FALSE)
  events <- preview$event_diff %||% data.frame(message = "No parameter delta preview changed events.", stringsAsFactors = FALSE)
  write_csv_safe(summary, file.path(out_dir, "Parameter_delta_preview_summary.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write_csv_safe(counts, file.path(out_dir, "Parameter_delta_preview_counts.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write_csv_safe(events, file.path(out_dir, "Parameter_delta_preview_events.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  invisible(out_dir)
}

stpd_parameter_delta_preview <- function(ds, params_current = default_params_sec(),
                                         params_baseline = default_params_sec(),
                                         selected_trains = NULL,
                                         max_trains = 3L,
                                         iou_min = 0.25,
                                         source = c("auto", "final"),
                                         lock_manual = TRUE,
                                         collect_diagnostics = FALSE) {
  source <- match.arg(source)
  if (is.null(ds) || is.null(ds$trains)) stop("stpd_parameter_delta_preview(): ds must be a dataset with a trains list.", call. = FALSE)
  target <- intersect(as.character(selected_trains %||% names(ds$trains)), names(ds$trains))
  if (length(target) == 0) stop("No target trains found for parameter delta preview.", call. = FALSE)
  max_trains <- suppressWarnings(as.integer(max_trains %||% 3L))
  if (!is.finite(max_trains) || max_trains < 1L) max_trains <- 3L
  target <- head(target, max_trains)
  iou_min <- suppressWarnings(as.numeric(iou_min %||% 0.25))
  if (!is.finite(iou_min)) iou_min <- 0.25
  iou_min <- max(0.01, min(1, iou_min))

  baseline_run <- stpd_detect(ds, params_baseline, selected_trains = target, lock_manual = lock_manual, collect_diagnostics = collect_diagnostics)
  current_run <- stpd_detect(ds, params_current, selected_trains = target, lock_manual = lock_manual, collect_diagnostics = collect_diagnostics)
  baseline_events <- stpd_delta_event_table(baseline_run, params_baseline, target, source = source)
  current_events <- stpd_delta_event_table(current_run, params_current, target, source = source)
  event_diff <- stpd_parameter_delta_event_diff(baseline_events, current_events, iou_min = iou_min)
  count_table <- stpd_parameter_delta_count_table(baseline_events, current_events)
  parameter_changes <- stpd_parameter_change_preview(params_current, baseline = params_baseline)
  if ("message" %in% names(parameter_changes)) parameter_changes <- data.frame()
  summary <- stpd_parameter_delta_summary(event_diff, count_table, target, parameter_changes, source = source)
  list(
    summary = summary,
    counts = count_table,
    event_diff = event_diff[event_diff$status != "unchanged_event", , drop = FALSE],
    event_diff_all = event_diff,
    parameter_changes = parameter_changes,
    baseline_events = baseline_events,
    current_events = current_events,
    selected_trains = target,
    iou_min = iou_min,
    source = source
  )
}
