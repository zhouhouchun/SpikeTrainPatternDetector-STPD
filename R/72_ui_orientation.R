# Pure UI-orientation contracts shared by the Shiny navigation and event table.
# These helpers do not alter detector output or project semantic tracks back to
# the legacy, single-label per-ISI representation.

stpd_ui_orientation_groups <- function() {
  groups <- list(
    c("\u6570\u636E QC"),
    c("\u68C0\u6D4B\u5668 / \u53C2\u6570"),
    c(
      "\u5BF9\u9F50\u65F6\u95F4\u6233\u56FE",
      "\u539F\u59CB\u65F6\u95F4\u6233\u56FE",
      "ISI \u65F6\u95F4\u5256\u9762",
      "\u533A\u95F4\u76F4\u65B9\u56FE",
      "\u6570\u636E\u96C6 ISI \u76F4\u65B9\u56FE"
    ),
    c("\u7ED3\u6784\u5019\u9009", "\u9608\u503C\u9884\u89C8"),
    c(
      "\u624B\u52A8\u6807\u8BB0\u4E0E\u68C0\u6D4B\u5668\u62A5\u544A",
      "\u79D1\u5B66\u9A8C\u8BC1"
    ),
    c("\u4E8B\u4EF6 / \u8F93\u51FA", "\u6279\u5904\u7406 / API"),
    c(
      "ISI \u72B6\u6001\u7A7A\u95F4",
      "\u72B6\u6001\u8F68\u8FF9",
      "\u4E8B\u4EF6\u5BF9\u9F50\u6D3B\u52A8",
      "\u795E\u7ECF\u6D41\u5F62",
      "\u652F\u6301\u65B9\u6CD5"
    ),
    c(
      "\u79CD\u5B50 / \u6865\u63A5\u8BCA\u65AD",
      "\u65B9\u6CD5 / \u5BA1\u8BA1\u8BF4\u660E",
      "\u81EA\u9002\u5E94 train \u8C03\u53C2"
    )
  )
  stats::setNames(
    groups,
    c(
      "\u6570\u636E", "\u68C0\u6D4B", "\u6D4F\u89C8", "\u590D\u6838",
      "\u9A8C\u8BC1", "\u5BFC\u51FA", "\u5206\u6790", "\u4E13\u5BB6"
    )
  )
}

stpd_ui_orientation_workflow <- function() {
  data.frame(
    step_id = c(
      "orientation_step_data", "orientation_step_params",
      "orientation_step_run", "orientation_step_validate",
      "orientation_step_export"
    ),
    step_order = seq_len(5L),
    label = c(
      "\u5BFC\u5165\u6570\u636E", "\u8BBE\u7F6E\u5173\u952E\u53C2\u6570",
      "\u8FD0\u884C\u68C0\u6D4B", "\u590D\u6838\u4E0E\u9A8C\u8BC1",
      "\u5BFC\u51FA\u7ED3\u679C"
    ),
    short_label = c(
      "\u6570\u636E", "\u53C2\u6570\u9879", "\u68C0\u6D4B", "\u590D\u6838", "\u5BFC\u51FA"
    ),
    note = c(
      "CSV / RDS\uFF0CQC \u548C\u6570\u636E\u96C6\u9009\u62E9",
      "\u57FA\u7840\u53C2\u6570\u4F18\u5148\uFF0C\u4E13\u5BB6\u9879\u6298\u53E0",
      "\u8DF3\u8F6C\u5230\u8FD0\u884C\u63A7\u4EF6\uFF0C\u4E0D\u4F1A\u81EA\u52A8\u542F\u52A8",
      "\u5DEE\u5F02\u9884\u89C8\u3001IoU\u3001\u654F\u611F\u6027",
      "CSV / ZIP / YAML \u53EF\u590D\u73B0\u8BB0\u5F55"
    ),
    target_tab = c(
      "\u6570\u636E QC", "\u68C0\u6D4B\u5668 / \u53C2\u6570",
      "\u68C0\u6D4B\u5668 / \u53C2\u6570",
      "\u624B\u52A8\u6807\u8BB0\u4E0E\u68C0\u6D4B\u5668\u62A5\u544A",
      "\u4E8B\u4EF6 / \u8F93\u51FA"
    ),
    control_anchor = c(
      "stpd_data_controls", "stpd_detect_controls", "stpd_run_controls",
      NA_character_, "stpd_export_controls"
    ),
    stringsAsFactors = FALSE
  )
}

stpd_ui_orientation_tab_group <- function(tab) {
  tab <- as.character(tab)
  groups <- stpd_ui_orientation_groups()
  lookup <- unlist(lapply(names(groups), function(group) {
    stats::setNames(rep(group, length(groups[[group]])), groups[[group]])
  }), use.names = TRUE)
  unname(lookup[tab])
}

stpd_ui_orientation_tab_step <- function(tab) {
  group <- stpd_ui_orientation_tab_group(tab)
  out <- rep(NA_character_, length(group))
  out[group %in% "\u6570\u636E"] <- "orientation_step_data"
  out[group %in% c("\u68C0\u6D4B", "\u4E13\u5BB6")] <- "orientation_step_params"
  out[group %in% c("\u6D4F\u89C8", "\u5206\u6790")] <- "orientation_step_run"
  out[group %in% c("\u590D\u6838", "\u9A8C\u8BC1")] <- "orientation_step_validate"
  out[group %in% "\u5BFC\u51FA"] <- "orientation_step_export"
  unname(out)
}

stpd_orientation_normalize_event_label <- function(label) {
  label <- tolower(trimws(as.character(label)))
  label[is.na(label)] <- ""
  gsub("[ -]+", "_", label)
}

stpd_orientation_semantic_track <- function(label) {
  label <- stpd_orientation_normalize_event_label(label)
  label[label == "hft"] <- "high_frequency_tonic"
  label[label == "hfs"] <- "high_frequency_spiking"

  track <- rep("diagnostic", length(label))
  track[label %in% c(
    "burst", "long_burst", "high_frequency_burst", "hf_burst"
  )] <- "event"
  track[label %in% c(
    "tonic", "high_frequency_tonic", "high_frequency_spiking"
  )] <- "state"
  track[label == "pause"] <- "gap"
  track[label == "possible_burst"] <- "review"
  track[label == "profile"] <- "profile"
  unname(track)
}

# Limit the AUTO interval table to the trains that belong to the latest
# detector run. This is presentation-only: stored AUTO labels and detector
# products are never rewritten here.
stpd_orientation_event_scope <- function(events, source = "auto",
                                         run_state = NULL,
                                         run_identity = NULL) {
  if (!is.data.frame(events)) {
    stop("events must be a data frame.", call. = FALSE)
  }
  source <- as.character(source %||% "auto")[1]
  code <- as.character((run_state %||% list())$code %||%
    "run_state_unknown")[1]
  scope_label <- as.character((run_state %||% list())$scope_label %||%
    "?/?")[1]
  selected <- stpd_ui_state_parse_trains(
    (run_identity %||% list())$selected_trains %||%
      (run_state %||% list())$selected_trains %||% character()
  )

  status <- "legacy_or_non_auto"
  current <- NA
  out <- events
  if (identical(source, "auto")) {
    selected_n <- suppressWarnings(as.integer(
      (run_identity %||% list())$selected_train_n %||% NA_integer_
    ))[1]
    total_n <- suppressWarnings(as.integer(
      (run_identity %||% list())$total_train_n %||% NA_integer_
    ))[1]
    scope_available <- isTRUE((run_identity %||% list())$has_run) &&
      length(selected) > 0L && !is.na(selected_n) &&
      identical(selected_n, length(selected)) && !is.na(total_n) &&
      total_n > 0L && selected_n <= total_n
    if (scope_available) {
      if (!("train" %in% names(out))) {
        out <- out[FALSE, , drop = FALSE]
      } else {
        out <- out[as.character(out$train) %in% selected, , drop = FALSE]
      }
    } else {
      # AUTO rows without a trustworthy latest-run scope cannot be presented
      # as current (or as historical rows that are safe to navigate to).
      out <- out[FALSE, , drop = FALSE]
    }
    if (scope_available && identical(code, "run_partial_current")) {
      status <- "partial_current"
      current <- TRUE
    } else if (scope_available && identical(code, "run_current")) {
      status <- "current"
      current <- TRUE
    } else {
      # Historical AUTO rows can remain useful for diagnosis, but callers must
      # label them as non-current rather than present them as the latest run.
      status <- if (scope_available) {
        "historical_scoped"
      } else {
        "historical_unverified"
      }
      current <- FALSE
    }
  }

  list(
    events = out,
    source = source,
    run_code = code,
    scope_label = scope_label,
    selected_trains = selected,
    status = status,
    current = current
  )
}

stpd_orientation_event_column <- function(events, name, default = NA) {
  if (name %in% names(events)) return(events[[name]])
  n <- nrow(events)
  if (length(default) == n) return(default)
  if (length(default) == 1L) return(rep(default, n))
  stop(name, " default must have length one or one value per event row.", call. = FALSE)
}

stpd_orientation_event_recycle <- function(x, n, field) {
  if (n == 0L) return(x[FALSE])
  if (length(x) == 1L) return(rep(x, n))
  if (length(x) != n) {
    stop(field, " must have length one or one value per event row.", call. = FALSE)
  }
  x
}

stpd_orientation_event_number_token <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  out <- rep(NA_character_, length(x))
  ok <- is.finite(x)
  x[ok & x == 0] <- 0
  out[ok] <- sprintf("%.17e", x[ok])
  out
}

stpd_orientation_event_row_key <- function(events, source = NULL,
                                           dataset_id = NULL) {
  if (!is.data.frame(events)) {
    stop("events must be a data frame.", call. = FALSE)
  }
  n <- nrow(events)
  if (n == 0L) return(character())

  dataset <- if (is.null(dataset_id)) {
    stpd_orientation_event_column(events, "dataset", "")
  } else {
    stpd_orientation_event_recycle(dataset_id, n, "dataset_id")
  }
  source <- if (is.null(source)) {
    if ("event_source" %in% names(events)) events$event_source
    else if ("source" %in% names(events)) events$source
    else stpd_orientation_event_column(events, "label_source", "")
  } else {
    stpd_orientation_event_recycle(source, n, "source")
  }

  dataset <- trimws(as.character(dataset))
  source <- trimws(as.character(source))
  train <- trimws(as.character(stpd_orientation_event_column(events, "train", "")))
  pattern <- stpd_orientation_normalize_event_label(
    stpd_orientation_event_column(events, "pattern", "")
  )
  start <- stpd_orientation_event_number_token(
    stpd_orientation_event_column(events, "start_time_sec", NA_real_)
  )
  end <- stpd_orientation_event_number_token(
    stpd_orientation_event_column(events, "end_time_sec", NA_real_)
  )
  event_id <- trimws(as.character(
    stpd_orientation_event_column(events, "event_id", "")
  ))

  invalid <- is.na(dataset) | !nzchar(dataset) |
    is.na(source) | !nzchar(source) |
    is.na(train) | !nzchar(train) |
    is.na(pattern) | !nzchar(pattern) |
    is.na(start) | is.na(end) |
    is.na(event_id) | !nzchar(event_id)
  start_num <- suppressWarnings(as.numeric(
    stpd_orientation_event_column(events, "start_time_sec", NA_real_)
  ))
  end_num <- suppressWarnings(as.numeric(
    stpd_orientation_event_column(events, "end_time_sec", NA_real_)
  ))
  invalid <- invalid | end_num < start_num

  out <- rep(NA_character_, n)
  for (i in which(!invalid)) {
    fields <- c(dataset[i], source[i], train[i], pattern[i], start[i], end[i], event_id[i])
    payload <- paste0(nchar(fields, type = "bytes"), ":", fields, collapse = "|")
    out[i] <- paste0(
      "orientation_event_",
      digest::digest(payload, algo = "sha256", serialize = FALSE)
    )
  }
  out
}

stpd_orientation_event_display_model <- function(
    events, source, dataset_id = NULL, time_unit = c("ms", "s"),
    mode = c("compact", "full")) {
  if (!is.data.frame(events)) {
    stop("events must be a data frame.", call. = FALSE)
  }
  time_unit <- match.arg(time_unit)
  mode <- match.arg(mode)
  n <- nrow(events)
  factor <- if (identical(time_unit, "ms")) 1000 else 1

  dataset <- if (is.null(dataset_id)) {
    as.character(stpd_orientation_event_column(events, "dataset", ""))
  } else {
    as.character(stpd_orientation_event_recycle(dataset_id, n, "dataset_id"))
  }
  event_source <- as.character(
    stpd_orientation_event_recycle(source, n, "source")
  )
  start_sec <- suppressWarnings(as.numeric(
    stpd_orientation_event_column(events, "start_time_sec", NA_real_)
  ))
  end_sec <- suppressWarnings(as.numeric(
    stpd_orientation_event_column(events, "end_time_sec", NA_real_)
  ))
  duration_sec <- suppressWarnings(as.numeric(
    stpd_orientation_event_column(events, "duration_sec", end_sec - start_sec)
  ))
  duration_sec[!is.finite(duration_sec) & is.finite(start_sec) & is.finite(end_sec)] <-
    end_sec[!is.finite(duration_sec) & is.finite(start_sec) & is.finite(end_sec)] -
    start_sec[!is.finite(duration_sec) & is.finite(start_sec) & is.finite(end_sec)]

  model <- data.frame(
    event_row_key = stpd_orientation_event_row_key(
      events, source = event_source, dataset_id = dataset
    ),
    event_source = event_source,
    dataset = dataset,
    train = as.character(stpd_orientation_event_column(events, "train", "")),
    semantic_track = stpd_orientation_semantic_track(
      stpd_orientation_event_column(events, "pattern", "")
    ),
    pattern = as.character(stpd_orientation_event_column(events, "pattern", "")),
    event_id = as.character(stpd_orientation_event_column(events, "event_id", "")),
    start_time = start_sec * factor,
    end_time = end_sec * factor,
    duration = duration_sec * factor,
    n_spikes = suppressWarnings(as.integer(
      stpd_orientation_event_column(events, "n_spikes", NA_integer_)
    )),
    n_isi = suppressWarnings(as.integer(
      stpd_orientation_event_column(events, "n_isi", NA_integer_)
    )),
    label_source = as.character(
      stpd_orientation_event_column(events, "label_source", "")
    ),
    auto_score = suppressWarnings(as.numeric(
      stpd_orientation_event_column(events, "auto_score", NA_real_)
    )),
    stringsAsFactors = FALSE
  )

  if (identical(mode, "full")) {
    converted <- c(
      pre_ISI = "pre_ISI_sec", post_ISI = "post_ISI_sec",
      context_pre_ISI = "context_pre_ISI_sec",
      context_post_ISI = "context_post_ISI_sec",
      mean_ISI = "mean_ISI_sec", median_ISI = "median_ISI_sec",
      min_ISI = "min_ISI_sec", max_ISI = "max_ISI_sec",
      core_q_ISI = "core_q_ISI_sec"
    )
    for (display_name in names(converted)) {
      model[[display_name]] <- suppressWarnings(as.numeric(
        stpd_orientation_event_column(events, converted[[display_name]], NA_real_)
      )) * factor
    }
    passthrough <- c(
      "start_spike_idx", "end_spike_idx", "MM", "LV", "CV", "Pre_LV",
      "After_LV", "n_flank", "n_flank_ctx", "contrast_min_q",
      "contrast_geom_q", "contrast_pct_q", "contrast_min_ctx_q",
      "contrast_geom_ctx_q", "contrast_pct_ctx_q",
      "user_promoted_possible_burst", "n_user_promoted_isi",
      "auto_pattern_majority", "user_override_reason"
    )
    for (name in passthrough) {
      model[[name]] <- stpd_orientation_event_column(events, name, NA)
    }
  }

  attr(model, "time_unit") <- time_unit
  attr(model, "mode") <- mode
  model
}

stpd_orientation_invalid_event_jump <- function(reason) {
  list(
    valid = FALSE,
    reason = as.character(reason)[1],
    train = "",
    event_row_key = NA_character_,
    target_tab = NA_character_,
    raw_window_sec = c(NA_real_, NA_real_),
    aligned_window_sec = c(NA_real_, NA_real_),
    target_window_sec = c(NA_real_, NA_real_),
    target_window = c(NA_real_, NA_real_),
    display_unit = NA_character_,
    padding_sec = NA_real_
  )
}

stpd_orientation_event_jump_plan <- function(
    event_row, dataset, source = NULL, dataset_id = NULL,
    display_unit = c("ms", "s"), padding_sec = NULL,
    min_padding_sec = 0.025, padding_multiplier = 3) {
  if (!is.data.frame(event_row) || nrow(event_row) != 1L) {
    return(stpd_orientation_invalid_event_jump("event_row_invalid"))
  }
  if (!is.list(dataset) || !is.list(dataset$trains)) {
    return(stpd_orientation_invalid_event_jump("dataset_invalid"))
  }
  display_unit <- as.character(display_unit)[1]
  if (is.na(display_unit) || !(display_unit %in% c("ms", "s"))) {
    return(stpd_orientation_invalid_event_jump("display_unit_invalid"))
  }

  train <- trimws(as.character(
    stpd_orientation_event_column(event_row, "train", "")
  )[1])
  if (!nzchar(train) || !(train %in% names(dataset$trains))) {
    return(stpd_orientation_invalid_event_jump("train_not_found"))
  }
  train_data <- dataset$trains[[train]]
  if (!is.data.frame(train_data) || !("timestamp_sec" %in% names(train_data))) {
    return(stpd_orientation_invalid_event_jump("train_geometry_invalid"))
  }
  timestamps <- suppressWarnings(as.numeric(train_data$timestamp_sec))
  if (length(timestamps) < 2L || any(!is.finite(timestamps)) ||
      any(diff(timestamps) < 0) || timestamps[length(timestamps)] <= timestamps[1]) {
    return(stpd_orientation_invalid_event_jump("train_geometry_invalid"))
  }

  start <- suppressWarnings(as.numeric(
    stpd_orientation_event_column(event_row, "start_time_sec", NA_real_)
  )[1])
  end <- suppressWarnings(as.numeric(
    stpd_orientation_event_column(event_row, "end_time_sec", NA_real_)
  )[1])
  if (!is.finite(start) || !is.finite(end) || end < start) {
    return(stpd_orientation_invalid_event_jump("event_geometry_invalid"))
  }
  lower <- timestamps[1]
  upper <- timestamps[length(timestamps)]
  tolerance <- max(1e-12, .Machine$double.eps * max(1, abs(c(lower, upper))) * 16)
  if (start < lower - tolerance || end > upper + tolerance) {
    return(stpd_orientation_invalid_event_jump("event_outside_train"))
  }
  start <- min(upper, max(lower, start))
  end <- min(upper, max(lower, end))

  min_padding_sec <- suppressWarnings(as.numeric(min_padding_sec)[1])
  padding_multiplier <- suppressWarnings(as.numeric(padding_multiplier)[1])
  if (!is.finite(min_padding_sec) || min_padding_sec < 0 ||
      !is.finite(padding_multiplier) || padding_multiplier < 0) {
    return(stpd_orientation_invalid_event_jump("padding_invalid"))
  }
  if (is.null(padding_sec)) {
    padding_sec <- max(min_padding_sec, (end - start) * padding_multiplier)
  } else {
    padding_sec <- suppressWarnings(as.numeric(padding_sec)[1])
    if (!is.finite(padding_sec) || padding_sec < 0) {
      return(stpd_orientation_invalid_event_jump("padding_invalid"))
    }
  }

  raw_window_sec <- c(max(lower, start - padding_sec), min(upper, end + padding_sec))
  if (any(!is.finite(raw_window_sec)) || raw_window_sec[2] <= raw_window_sec[1]) {
    return(stpd_orientation_invalid_event_jump("window_geometry_invalid"))
  }
  aligned_window_sec <- raw_window_sec - lower

  aligned_tab <- "\u5BF9\u9F50\u65F6\u95F4\u6233\u56FE"
  raw_tab <- "\u539F\u59CB\u65F6\u95F4\u6233\u56FE"
  target_tab <- if (exists(
    "stpd_default_timestamp_tab_for_dataset", mode = "function", inherits = TRUE
  )) {
    tryCatch(
      stpd_default_timestamp_tab_for_dataset(dataset),
      error = function(e) aligned_tab
    )
  } else {
    task_events <- dataset$task_events
    has_task_events <- is.data.frame(task_events) &&
      "event_time_sec" %in% names(task_events) &&
      any(is.finite(suppressWarnings(as.numeric(task_events$event_time_sec))))
    if (isTRUE(has_task_events)) raw_tab else aligned_tab
  }
  if (!(target_tab %in% c(aligned_tab, raw_tab))) target_tab <- aligned_tab
  target_window_sec <- if (identical(target_tab, raw_tab)) {
    raw_window_sec
  } else {
    aligned_window_sec
  }
  factor <- if (identical(display_unit, "ms")) 1000 else 1

  event_source <- source
  if (is.null(event_source)) {
    if ("event_source" %in% names(event_row)) event_source <- event_row$event_source
    else if ("source" %in% names(event_row)) event_source <- event_row$source
    else event_source <- stpd_orientation_event_column(event_row, "label_source", "")
  }
  row_key <- stpd_orientation_event_row_key(
    event_row, source = event_source, dataset_id = dataset_id
  )[1]
  if (is.na(row_key) || !nzchar(row_key)) {
    return(stpd_orientation_invalid_event_jump("event_identity_invalid"))
  }

  list(
    valid = TRUE,
    reason = "ready",
    train = train,
    event_row_key = row_key,
    target_tab = target_tab,
    raw_window_sec = unname(raw_window_sec),
    aligned_window_sec = unname(aligned_window_sec),
    target_window_sec = unname(target_window_sec),
    target_window = unname(target_window_sec * factor),
    display_unit = display_unit,
    padding_sec = padding_sec
  )
}
