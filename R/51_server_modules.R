# Server modules extracted from R/19_server.R.
# Each installer evaluates in the live Shiny server environment so existing
# reactive values and helper closures keep the same lexical behavior.

stpd_selection_has_points <- function(sel) {
  if (is.null(sel) || !is.data.frame(sel) || nrow(sel) == 0) return(FALSE)
  if (!all(c("x", "y") %in% names(sel))) return(FALSE)
  x <- suppressWarnings(as.numeric(sel$x))
  y <- suppressWarnings(as.numeric(sel$y))
  any(is.finite(x) & is.finite(y))
}

stpd_push_manual_undo <- function(rv, dataset_id, trains, action = "\u624B\u52A8\u64CD\u4F5C") {
  rv$manual_undo_snapshot <- list(
    dataset_id = dataset_id,
    trains = trains,
    action = as.character(action %||% "\u624B\u52A8\u64CD\u4F5C")[1],
    time = Sys.time()
  )
  invisible(TRUE)
}

stpd_error_mentions_qc <- function(e) {
  msg <- as.character(conditionMessage(e) %||% "")
  grepl(
    "Pre-detection QC|data-integrity|duplicate_timestamps|duplicate timestamp|zero_or_negative_ISI|0 ISI|artifact_ISI|n_artifact_ISI",
    msg,
    ignore.case = TRUE
  )
}

stpd_shiny_detector_error_message <- function(e, prefix = "\u68C0\u6D4B\u5668\u8FD0\u884C\u5931\u8D25", max_chars = 1400L) {
  msg <- as.character(conditionMessage(e) %||% "")
  msg <- msg[1]
  if (is.na(msg) || !nzchar(msg)) msg <- "\u672A\u77E5\u9519\u8BEF\u3002"

  friendly_prefix <- paste0(prefix, "\uFF1A")
  if (stpd_error_mentions_qc(e)) {
    friendly_prefix <- paste0(
      friendly_prefix,
      "\u68C0\u6D4B\u524D QC \u53D1\u73B0\u6570\u636E\u5B8C\u6574\u6027\u95EE\u9898\uFF08\u5E38\u89C1\u539F\u56E0\u662F\u91CD\u590D timestamp \u4EA7\u751F 0 ISI\uFF0C",
      "\u6216 ISI \u5C0F\u4E8E\u6700\u5C0F\u6709\u6548\u9608\u503C\uFF09\u3002\u8BF7\u5230\u201C\u6570\u636E QC\u201D\u67E5\u770B\u660E\u7EC6\uFF1B",
      "\u5982\u786E\u8BA4\u662F\u5BFC\u51FA\u91CD\u590D\u884C\uFF0C\u53EF\u5728\u9AD8\u7EA7 QC \u4E2D\u5408\u5E76\u91CD\u590D timestamp \u540E\u518D\u8FD0\u884C\u3002\n"
    )
  }

  msg <- gsub("[\r\n]+", " ", msg)
  max_chars <- suppressWarnings(as.integer(max_chars %||% 1400L))
  if (!is.finite(max_chars) || max_chars < 200L) max_chars <- 1400L
  if (nchar(msg, type = "chars", allowNA = FALSE, keepNA = FALSE) > max_chars) {
    msg <- paste0(substr(msg, 1L, max_chars), " ...")
  }
  paste0(friendly_prefix, msg)
}

stpd_safe_read_rds <- function(path, max_bytes, label = "RDS") {
  if (is.null(path) || length(path) == 0 || !file.exists(path[1])) {
    stop(label, " file does not exist.", call. = FALSE)
  }
  info <- file.info(path[1])
  size <- suppressWarnings(as.numeric(info$size[1]))
  max_bytes <- suppressWarnings(as.numeric(max_bytes))
  if (!is.finite(size) || size < 0) stop(label, " file size cannot be verified.", call. = FALSE)
  if (is.finite(max_bytes) && size > max_bytes) {
    stop(label, " file is too large for safe import.", call. = FALSE)
  }
  readRDS(path[1])
}

stpd_shiny_train_frame_valid <- function(dat, max_rows = 5000000L) {
  if (!is.data.frame(dat)) return(FALSE)
  if (nrow(dat) > max_rows) return(FALSE)
  all(c("idx", "timestamp_sec", "ISI_sec") %in% names(dat))
}

stpd_workspace_rds_is_valid <- function(obj, max_datasets = 500L, max_trains = 5000L) {
  if (!is.list(obj) || !is.list(obj$datasets)) return(FALSE)
  if (length(obj$datasets) > max_datasets) return(FALSE)
  for (ds in obj$datasets) {
    if (!is.list(ds) || !is.list(ds$trains)) return(FALSE)
    if (length(ds$trains) > max_trains) return(FALSE)
    ok <- vapply(ds$trains, stpd_shiny_train_frame_valid, logical(1))
    if (!all(ok)) return(FALSE)
  }
  TRUE
}

stpd_nn_model_rds_is_valid <- function(obj) {
  if (!is.list(obj) || is.null(obj$model) || !is.character(obj$feature_cols)) return(FALSE)
  length(obj$feature_cols) > 0 &&
    length(obj$feature_cols) <= 10000L &&
    all(!is.na(obj$feature_cols) & nzchar(obj$feature_cols))
}

stpd_server_install_parameters_module <- function(server_env) {
  evalq({
  output$core_detector_params_panel <- renderUI({
    schema_ui_controls(
      prefix = "schema_param_",
      group_by = TRUE,
      group_field = "group",
      open_groups = TRUE,
      show_notes = TRUE
    )
  })

  observeEvent(input$qc_isi_unit, {
    old_u <- rv$qc_isi_unit_last %||% "ms"
    new_u <- qc_isi_unit()
    if (identical(old_u, new_u) || isTRUE(rv$syncing_threshold_unit)) return()
    rv$syncing_threshold_unit <- TRUE
    convert_value <- function(v) {
      v <- suppressWarnings(as.numeric(v))
      if (length(v) == 0 || !is.finite(v[1])) return(NA_real_)
      sec <- v[1] * threshold_unit_factor_to_sec(old_u)
      sec * threshold_unit_factor_from_sec(new_u)
    }
    art_v <- convert_value(input$artifact_isi_ms)
    min_v <- convert_value(input$min_valid_isi_ms)
    ref_v <- convert_value(input$refractory_suspect_ms)
    ref_param_v <- convert_value(input$refractory_suspect_ms_param)
    updateNumericInput(session, "artifact_isi_ms", label = paste0("\u4F2A\u8FF9 / \u6700\u5C0F\u6709\u6548 ISI \u9608\u503C\uFF08", new_u, ")"), value = art_v, step = if (identical(new_u, "s")) 0.0001 else 0.1)
    updateNumericInput(session, "min_valid_isi_ms", label = paste0("\u6700\u5C0F\u6709\u6548 ISI\uFF08\u540C\u4F2A\u8FF9\u9608\u503C\uFF0C", new_u, ")"), value = min_v, step = if (identical(new_u, "s")) 0.0001 else 0.1)
    updateNumericInput(session, "refractory_suspect_ms", label = paste0("\u7591\u4F3C\u4E0D\u5E94\u671F ISI \u9608\u503C\uFF08", new_u, ")"), value = ref_v, step = if (identical(new_u, "s")) 0.0001 else 0.1)
    updateNumericInput(session, "refractory_suspect_ms_param", label = paste0("\u7591\u4F3C\u4E0D\u5E94\u671F\u9608\u503C\uFF08", new_u, ")"), value = ref_param_v, step = if (identical(new_u, "s")) 0.0001 else 0.1)
    for (pat in pattern_isi_gate_patterns()) {
      min_id <- paste0("pattern_min_isi_", pat)
      max_id <- paste0("pattern_max_isi_", pat)
      min_v <- convert_value(tryCatch(input[[min_id]], error = function(e) 0))
      max_v <- convert_value(tryCatch(input[[max_id]], error = function(e) 0))
      updateNumericInput(session, min_id, value = min_v, step = if (identical(new_u, "s")) 0.0001 else 0.1)
      updateNumericInput(session, max_id, value = max_v, step = if (identical(new_u, "s")) 0.0001 else 0.1)
    }
    rv$qc_isi_unit_last <- new_u
    rv$syncing_threshold_unit <- FALSE
  }, ignoreInit = FALSE)

  observeEvent(input$artifact_isi_ms, {
    if (isTRUE(rv$syncing_min_isi)) return()
    rv$syncing_min_isi <- TRUE
    updateNumericInput(session, "min_valid_isi_ms", value = input$artifact_isi_ms)
    rv$syncing_min_isi <- FALSE
  }, ignoreInit = TRUE)

  observeEvent(input$min_valid_isi_ms, {
    if (isTRUE(rv$syncing_min_isi)) return()
    rv$syncing_min_isi <- TRUE
    updateNumericInput(session, "artifact_isi_ms", value = input$min_valid_isi_ms)
    rv$syncing_min_isi <- FALSE
  }, ignoreInit = TRUE)

  observeEvent(input$refractory_suspect_ms, {
    if (isTRUE(rv$syncing_refractory_suspect)) return()
    rv$syncing_refractory_suspect <- TRUE
    updateNumericInput(session, "refractory_suspect_ms_param", value = input$refractory_suspect_ms)
    rv$syncing_refractory_suspect <- FALSE
  }, ignoreInit = TRUE)

  observeEvent(input$refractory_suspect_ms_param, {
    if (isTRUE(rv$syncing_refractory_suspect)) return()
    rv$syncing_refractory_suspect <- TRUE
    updateNumericInput(session, "refractory_suspect_ms", value = input$refractory_suspect_ms_param)
    rv$syncing_refractory_suspect <- FALSE
  }, ignoreInit = TRUE)

  nice_xrange_tick_step <- function(max_plot, target_n = 8) {
    max_plot <- suppressWarnings(as.numeric(max_plot)[1])
    if (!is.finite(max_plot) || max_plot <= 0) return(1)
    raw <- max_plot / max(1, target_n)
    pow <- 10 ^ floor(log10(raw))
    frac <- raw / pow
    mult <- if (frac <= 1) 1 else if (frac <= 2) 2 else if (frac <= 5) 5 else 10
    mult * pow
  }

  nice_xrange_ticks <- function(max_plot, unit = "ms", target_n = 8) {
    max_plot <- suppressWarnings(as.numeric(max_plot)[1])
    if (!is.finite(max_plot) || max_plot <= 0) return(0)
    step <- nice_xrange_tick_step(max_plot, target_n = target_n)
    ticks <- seq(0, floor(max_plot / step) * step, by = step)
    if (length(ticks) < 2) ticks <- c(0, max_plot)
    if (identical(unit, "ms")) ticks <- round(ticks)
    unique(ticks[is.finite(ticks) & ticks >= 0 & ticks <= max_plot])
  }

  format_xrange_tick <- function(x, unit = "ms") {
    x <- suppressWarnings(as.numeric(x)[1])
    if (!is.finite(x)) return("")
    if (identical(unit, "ms")) {
      format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE)
    } else if (abs(x) >= 10) {
      format(round(x, 1), big.mark = ",", scientific = FALSE, trim = TRUE)
    } else {
      format(round(x, 2), big.mark = ",", scientific = FALSE, trim = TRUE)
    }
  }

  xrange_ticks_ui <- function(max_plot = NULL, unit = NULL) {
    max_plot <- suppressWarnings(as.numeric(max_plot %||% 1000)[1])
    if (!is.finite(max_plot) || max_plot <= 0) max_plot <- 1000
    unit <- as.character(unit %||% "ms")[1]
    ticks <- nice_xrange_ticks(max_plot, unit = unit)
    if (length(ticks) == 0) return(NULL)
    tags$div(
      class = "xrange-nice-ticks",
      lapply(seq_along(ticks), function(ii) {
        x <- ticks[ii]
        cls <- c("xrange-nice-tick")
        if (ii == 1L) cls <- c(cls, "is-first")
        if (ii == length(ticks)) cls <- c(cls, "is-last")
        tags$span(
          class = paste(cls, collapse = " "),
          style = sprintf("left: %.6f%%;", 100 * x / max_plot),
          tags$span(class = "xrange-nice-tick-label", format_xrange_tick(x, unit))
        )
      })
    )
  }

  xrange_step_out <- function(unit = NULL) {
    unit <- as.character(unit %||% input$time_unit %||% "ms")[1]
    if (identical(unit, "ms")) 1 else 0.001
  }

  xrange_window_width <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    if (length(x) != 2L || any(!is.finite(x))) return(NA_real_)
    x <- sort(x)
    if (x[2] <= x[1]) return(NA_real_)
    x[2] - x[1]
  }

  valid_xrange_window <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    length(x) == 2L && all(is.finite(x)) && x[2] > x[1]
  }

  same_xrange_numeric <- function(a, b, tol = 1e-9) {
    a <- suppressWarnings(as.numeric(a))[1]
    b <- suppressWarnings(as.numeric(b))[1]
    is.finite(a) && is.finite(b) && abs(a - b) <= tol
  }

  freeze_xrange_inputs <- function(ids) {
    for (id in ids) {
      try(shiny::freezeReactiveValue(input, id), silent = TRUE)
    }
    invisible(NULL)
  }

  update_xrange_numeric_input <- function(id, value, min = NULL, max = NULL, step = NULL) {
    freeze_xrange_inputs(id)
    updateNumericInput(session, id, min = min, max = max, step = step, value = value)
    invisible(NULL)
  }

  update_xrange_slider_input <- function(id, value, min = NULL, max = NULL, step = NULL) {
    freeze_xrange_inputs(id)
    updateSliderInput(session, id, min = min, max = max, step = step, value = value)
    invisible(NULL)
  }

  sync_xrange_length_inputs <- function(x = NULL, force = FALSE) {
    width <- xrange_window_width(x %||% input$xrange)
    max_plot <- suppressWarnings(as.numeric(rv$xrange_max_plot %||% 1000)[1])
    if (!is.finite(max_plot) || max_plot <= 0) max_plot <- 1000
    step_out <- xrange_step_out(rv$xrange_unit %||% input$time_unit %||% "ms")
    min_width <- if (identical(rv$xrange_unit %||% input$time_unit %||% "ms", "ms")) 1 else 0.001
    if (!is.finite(width) || width <= 0) width <- min(max_plot, if (identical(input$time_unit, "ms")) 1000 else 1)
    width <- max(min_width, min(width, max_plot))
    maybe_update <- function(id) {
      current <- tryCatch(isolate(input[[id]]), error = function(e) NA_real_)
      if (isTRUE(force) || !same_xrange_numeric(current, width)) {
        update_xrange_numeric_input(id, min = min_width, max = max_plot, step = step_out, value = width)
      }
    }
    maybe_update("xrange_window_length")
    maybe_update("xrange_plot_window_length")
    invisible(width)
  }

  current_xrange_window <- function(prefer_view = TRUE) {
    x <- if (isTRUE(prefer_view)) rv$view_align_x else NULL
    if (!stpd_valid_xrange_window(x)) x <- input$xrange
    if (!stpd_valid_xrange_window(x)) x <- input$xrange_plot
    if (!stpd_valid_xrange_window(x)) c(0, min(rv$xrange_max_plot %||% 1000, if (identical(input$time_unit, "ms")) 1000 else 1)) else sort(as.numeric(x))
  }

  apply_xrange_window_length <- function(length_value) {
    length_value <- suppressWarnings(as.numeric(length_value)[1])
    max_plot <- suppressWarnings(as.numeric(rv$xrange_max_plot %||% 1000)[1])
    if (!is.finite(max_plot) || max_plot <= 0) max_plot <- 1000
    unit <- rv$xrange_unit %||% input$time_unit %||% "ms"
    min_width <- if (identical(unit, "ms")) 1 else 0.001
    if (!is.finite(length_value) || length_value <= 0) return(invisible(NULL))
    length_value <- max(min_width, min(length_value, max_plot))
    cur <- current_xrange_window(prefer_view = TRUE)
    start <- suppressWarnings(as.numeric(cur[1]))
    if (!is.finite(start)) start <- 0
    start <- max(0, min(start, max(0, max_plot - length_value)))
    new_range <- c(start, start + length_value)
    rv$view_align_x <- NULL
    rv$syncing_xrange <- TRUE
    update_xrange_slider_input("xrange", min = 0, max = max_plot, value = new_range, step = xrange_step_out(unit))
    update_xrange_slider_input("xrange_plot", min = 0, max = max_plot, value = new_range, step = xrange_step_out(unit))
    sync_xrange_length_inputs(new_range)
    session$onFlushed(function() rv$syncing_xrange <- FALSE, once = TRUE)
    invisible(new_range)
  }

  refresh_xrange_slider <- function(trains = NULL, reset = FALSE) {
    td <- trains
    if (is.null(td)) td <- tryCatch(current_trains(), error = function(e) NULL)
    if (is.null(td) || length(td) == 0) return(invisible(NULL))
    f <- if (identical(input$time_unit, "ms")) 1000 else 1
    max_t <- max(vapply(td, function(dat) {
      if (is.null(dat) || nrow(dat) <= 1) 0 else dat$timestamp_sec[nrow(dat)] - dat$timestamp_sec[1]
    }, numeric(1)), na.rm = TRUE)
    max_t <- max(1, max_t)
    max_plot <- ceiling(max_t * f)
    rv$xrange_max_plot <- max_plot
    rv$xrange_unit <- input$time_unit %||% "ms"
    default_width <- if (identical(input$time_unit, "ms")) 1000 else 1
    step_out <- xrange_step_out(input$time_unit %||% "ms")
    cur <- isolate(input$xrange)
    if (isTRUE(reset) || is.null(cur) || length(cur) != 2 || any(!is.finite(cur))) {
      cur <- c(0, min(max_plot, default_width))
    } else {
      cur[1] <- max(0, min(cur[1], max_plot))
      cur[2] <- max(0, min(cur[2], max_plot))
      if (cur[2] <= cur[1]) cur <- c(0, min(max_plot, default_width))
    }
    rv$syncing_xrange <- TRUE
    update_xrange_slider_input("xrange", min = 0, max = max_plot, value = cur, step = step_out)
    update_xrange_slider_input("xrange_plot", min = 0, max = max_plot, value = cur, step = step_out)
    sync_xrange_length_inputs(cur, force = TRUE)
    rv$view_align_x <- NULL
    session$onFlushed(function() rv$syncing_xrange <- FALSE, once = TRUE)
    invisible(NULL)
  }

  output$xrange_ticks <- renderUI({
    xrange_ticks_ui(rv$xrange_max_plot %||% 1000, rv$xrange_unit %||% input$time_unit %||% "ms")
  })

  output$xrange_plot_ticks <- renderUI({
    xrange_ticks_ui(rv$xrange_max_plot %||% 1000, rv$xrange_unit %||% input$time_unit %||% "ms")
  })
  
  # ----------------------------------------------------------
  # Read files into memory
  # ----------------------------------------------------------

  }, envir = server_env)
}

stpd_server_install_data_io_module <- function(server_env) {
  evalq({
  data_load_progress <- function(value, detail = "", message = NULL, type = "active") {
    value <- suppressWarnings(as.numeric(value)[1])
    if (!is.finite(value)) value <- 0
    value <- max(0, min(1, value))
    detail <- as.character(detail %||% "")[1]
    message <- as.character(message %||% "\u6B63\u5728\u52A0\u8F7D spike train \u6570\u636E")[1]
    type <- as.character(type %||% "active")[1]
    rv$data_load_active <- !type %in% c("success", "error", "idle", "hide")
    rv$data_load_progress_value <- value
    rv$data_load_progress_message <- message
    rv$data_load_progress_detail <- detail
    rv$data_load_progress_type <- type
    if (identical(type, "active")) {
      rv$raster_plot_progress_active <- TRUE
    } else if (type %in% c("success", "error", "idle", "hide")) {
      rv$raster_plot_progress_active <- FALSE
    }
    if (identical(type, "success")) {
      rv$raster_plot_refresh_token <- safe_int(rv$raster_plot_refresh_token %||% 0L, 0L) + 1L
    }
    session$sendCustomMessage(
      "stpdDataLoadProgress",
      list(type = type, value = value, message = message, detail = detail)
    )
    plot_type <- if (identical(type, "error")) "error" else if (identical(type, "success")) "success" else "active"
    plot_message <- if (identical(type, "error")) "\u6570\u636E\u52A0\u8F7D\u5931\u8D25" else if (identical(type, "success")) "\u6570\u636E\u5DF2\u8FDB\u5165\u5185\u5B58\uFF0Cplot \u89C6\u56FE\u5373\u5C06\u5237\u65B0" else "\u6B63\u5728\u52A0\u8F7D spike train \u5E76\u51C6\u5907 plot \u89C6\u56FE"
    session$sendCustomMessage(
      "stpdPlotRenderProgress",
      list(
        outputId = "raster_plot",
        type = plot_type,
        value = value,
        message = plot_message,
        detail = detail
      )
    )
    try(setProgress(value = value, detail = detail), silent = TRUE)
    invisible(NULL)
  }

  dataset_selector_labels <- function(ds) {
    vapply(
      ds,
      function(x) paste0("[", x$meta$source, "] ", x$meta$display_name, " (", length(x$trains), " \u6761 train\uFF09"),
      character(1)
    )
  }

  sync_dataset_selector <- function(selected_id = NULL) {
    ds <- rv$datasets
    if (length(ds) == 0L) return(invisible(NULL))
    ids <- names(ds)
    selected_id <- as.character(selected_id %||% rv$current_id %||% ids[1])[1]
    if (!nzchar(selected_id) || !(selected_id %in% ids)) selected_id <- ids[1]
    labels <- dataset_selector_labels(ds)
    rv$current_id <- selected_id
    session$onFlushed(function() {
      updateSelectizeInput(
        session,
        "dataset_id",
        choices = setNames(ids, labels),
        selected = selected_id
      )
    }, once = TRUE)
    invisible(NULL)
  }

  observeEvent(input$file_raw, {
    req(input$file_raw)
    files <- input$file_raw
    ds <- rv$datasets
    n_files <- max(1L, nrow(files))
    withProgress(message = "\u6B63\u5728\u52A0\u8F7D spike train \u6570\u636E", value = 0, {
      loaded_count <- 0L
      failed_count <- 0L
      data_load_progress(
        0.01,
        sprintf("\u51C6\u5907\u8BFB\u53D6 %d \u4E2A\u539F\u59CB CSV \u6587\u4EF6", nrow(files)),
        "\u6B63\u5728\u52A0\u8F7D\u539F\u59CB spike train"
      )
      for (i in seq_len(nrow(files))) {
        path <- files$datapath[i]
        nm <- files$name[i]
        file_start <- (i - 1) / n_files
        file_span <- 1 / n_files
        data_load_progress(
          file_start + 0.05 * file_span,
          sprintf("\u8BFB\u53D6\u539F\u59CB CSV %d/%d\uFF1A%s", i, nrow(files), nm),
          "\u6B63\u5728\u52A0\u8F7D\u539F\u59CB spike train"
        )
        id <- paste0("raw_", digest(paste0(nm, "_", file.info(path)$size), algo = "xxhash64"))
        if (id %in% names(ds)) {
          data_load_progress(
            file_start + file_span,
            sprintf("\u5DF2\u8DF3\u8FC7\u91CD\u590D\u6570\u636E\u96C6\uFF1A%s", nm),
            "\u6B63\u5728\u52A0\u8F7D\u539F\u59CB spike train"
          )
          next
        }
        trains <- tryCatch(
          build_trains_from_raw(path, header = isTRUE(input$header_raw), unit_in = input$unit_in_raw, duplicate_policy = input$duplicate_timestamp_policy %||% "error_keep"),
          error = function(e) {
            failed_count <<- failed_count + 1L
            data_load_progress(
              file_start + 0.95 * file_span,
              paste0(nm, " | ", e$message),
              "\u539F\u59CB\u6587\u4EF6\u52A0\u8F7D\u5931\u8D25",
              "error"
            )
            showNotification(paste0("\u539F\u59CB\u6587\u4EF6\u52A0\u8F7D\u5931\u8D25\uFF1A", nm, " | ", e$message), type = "error", duration = 8)
            NULL
          }
        )
        if (!is.null(trains)) {
          task_events <- tryCatch(
            stpd_extract_task_events_from_raw(path, header = isTRUE(input$header_raw), unit_in = input$unit_in_raw),
            error = function(e) stpd_empty_task_events()
          )
          data_load_progress(
            file_start + 0.55 * file_span,
            sprintf("\u89E3\u6790\u5230 %d \u6761 train\uFF0C\u6B63\u5728\u6784\u5EFA\u6570\u636E\u96C6", length(trains)),
            "\u6B63\u5728\u52A0\u8F7D\u539F\u59CB spike train"
          )
          ds[[id]] <- make_dataset(name = nm, source = "raw", trains = trains, unit_in = input$unit_in_raw, task_events = task_events)
          data_load_progress(
            file_start + 0.78 * file_span,
            "\u6B63\u5728\u8FD0\u884C\u5BFC\u5165 QC",
            "\u6B63\u5728\u52A0\u8F7D\u539F\u59CB spike train"
          )
          ds[[id]]$quality <- validate_dataset_quality_impl(trains, min_isi_sec = min_valid_isi_sec(), unit_hint = input$unit_in_raw, refractory_suspect_sec = refractory_suspect_sec(), display_unit = qc_isi_unit())
          rv$current_id <- id
          loaded_count <- loaded_count + 1L
          data_load_progress(
            file_start + 0.94 * file_span,
            "\u6B63\u5728\u5237\u65B0\u6570\u636E\u96C6\u9009\u62E9\u548C\u56FE\u5F62\u7A97\u53E3",
            "\u6B63\u5728\u52A0\u8F7D\u539F\u59CB spike train"
          )
          task_note <- if (nrow(task_events) > 0L) paste0("\n\u5DF2\u8BC6\u522B ", nrow(task_events), " \u4E2A\u4EFB\u52A1/\u884C\u4E3A\u4E8B\u4EF6\u65F6\u95F4\u6233\uFF08\u4E0D\u53C2\u4E0E\u6838\u5FC3\u68C0\u6D4B\uFF09\u3002") else ""
          showNotification(paste0(quality_notification_text(ds[[id]]$quality), task_note), type = ifelse(any(ds[[id]]$quality$warning_level == "error"), "error", ifelse(any(ds[[id]]$quality$warning_level == "warning"), "warning", "message")), duration = 7)
        }
      }
      rv$datasets <- ds
      sync_dataset_selector(rv$current_id)
      if (!is.null(rv$current_id) && rv$current_id %in% names(rv$datasets)) {
        data_load_progress(
          0.98,
          "\u6B63\u5728\u540C\u6B65\u663E\u793A\u65F6\u95F4\u7A97",
          "\u6B63\u5728\u52A0\u8F7D\u539F\u59CB spike train"
        )
        refresh_xrange_slider(rv$datasets[[rv$current_id]]$trains, reset = TRUE)
      }
      if (loaded_count > 0L) {
        data_load_progress(
          1,
          sprintf("\u5DF2\u52A0\u8F7D %d \u4E2A\u539F\u59CB\u6570\u636E\u96C6\uFF0C\u5931\u8D25 %d \u4E2A\u3002", loaded_count, failed_count),
          "\u52A0\u8F7D\u5B8C\u6210",
          "success"
        )
      } else {
        data_load_progress(
          1,
          "\u6CA1\u6709\u65B0\u6570\u636E\u96C6\u8FDB\u5165\u5185\u5B58\u3002\u8BF7\u68C0\u67E5\u6587\u4EF6\u683C\u5F0F\u3001\u91CD\u590D\u6570\u636E\u96C6\u6216\u9519\u8BEF\u901A\u77E5\u3002",
          "\u672A\u52A0\u8F7D\u65B0\u6570\u636E\u96C6",
          "error"
        )
      }
    })
  }, ignoreNULL = TRUE)
  
  observeEvent(input$file_annot, {
    req(input$file_annot)
    files <- input$file_annot
    ds <- rv$datasets
    n_files <- max(1L, nrow(files))
    withProgress(message = "\u6B63\u5728\u52A0\u8F7D\u5DF2\u6807\u8BB0 spike train", value = 0, {
      loaded_count <- 0L
      failed_count <- 0L
      data_load_progress(
        0.01,
        sprintf("\u51C6\u5907\u8BFB\u53D6 %d \u4E2A\u5DF2\u6807\u8BB0 CSV \u6587\u4EF6", nrow(files)),
        "\u6B63\u5728\u52A0\u8F7D\u5DF2\u6807\u8BB0 spike train"
      )
      for (i in seq_len(nrow(files))) {
        path <- files$datapath[i]
        nm <- files$name[i]
        file_start <- (i - 1) / n_files
        file_span <- 1 / n_files
        data_load_progress(
          file_start + 0.08 * file_span,
          sprintf("\u8BFB\u53D6\u5DF2\u6807\u8BB0 CSV %d/%d\uFF1A%s", i, nrow(files), nm),
          "\u6B63\u5728\u52A0\u8F7D\u5DF2\u6807\u8BB0 spike train"
        )
        id <- paste0("annot_", digest(paste0(nm, "_", file.info(path)$size), algo = "xxhash64"))
        if (id %in% names(ds)) {
          data_load_progress(
            file_start + file_span,
            sprintf("\u5DF2\u8DF3\u8FC7\u91CD\u590D\u6570\u636E\u96C6\uFF1A%s", nm),
            "\u6B63\u5728\u52A0\u8F7D\u5DF2\u6807\u8BB0 spike train"
          )
          next
        }
        trains <- tryCatch(
          build_trains_from_annot(path, unit_in = input$unit_in_annot, duplicate_policy = input$duplicate_timestamp_policy %||% "error_keep"),
          error = function(e) {
            failed_count <<- failed_count + 1L
            data_load_progress(
              file_start + 0.95 * file_span,
              paste0(nm, " | ", e$message),
              "\u5DF2\u6807\u8BB0\u6587\u4EF6\u52A0\u8F7D\u5931\u8D25",
              "error"
            )
            showNotification(paste0("\u5DF2\u6807\u8BB0\u6587\u4EF6\u52A0\u8F7D\u5931\u8D25\uFF1A", nm, " | ", e$message), type = "error", duration = 8)
            NULL
          }
        )
        if (!is.null(trains)) {
          task_events <- tryCatch(
            stpd_extract_task_events_from_raw(path, header = TRUE, unit_in = input$unit_in_annot),
            error = function(e) stpd_empty_task_events()
          )
          data_load_progress(
            file_start + 0.55 * file_span,
            sprintf("\u89E3\u6790\u5230 %d \u6761 train\uFF0C\u6B63\u5728\u6784\u5EFA\u6570\u636E\u96C6", length(trains)),
            "\u6B63\u5728\u52A0\u8F7D\u5DF2\u6807\u8BB0 spike train"
          )
          ds[[id]] <- make_dataset(name = nm, source = "annot", trains = trains, unit_in = input$unit_in_annot, task_events = task_events)
          data_load_progress(
            file_start + 0.78 * file_span,
            "\u6B63\u5728\u8FD0\u884C\u5BFC\u5165 QC",
            "\u6B63\u5728\u52A0\u8F7D\u5DF2\u6807\u8BB0 spike train"
          )
          ds[[id]]$quality <- validate_dataset_quality_impl(trains, min_isi_sec = min_valid_isi_sec(), unit_hint = input$unit_in_annot, refractory_suspect_sec = refractory_suspect_sec(), display_unit = qc_isi_unit())
          rv$current_id <- id
          loaded_count <- loaded_count + 1L
          data_load_progress(
            file_start + 0.94 * file_span,
            "\u6B63\u5728\u5237\u65B0\u6570\u636E\u96C6\u9009\u62E9\u548C\u56FE\u5F62\u7A97\u53E3",
            "\u6B63\u5728\u52A0\u8F7D\u5DF2\u6807\u8BB0 spike train"
          )
          task_note <- if (nrow(task_events) > 0L) paste0("\n\u5DF2\u8BC6\u522B ", nrow(task_events), " \u4E2A\u4EFB\u52A1/\u884C\u4E3A\u4E8B\u4EF6\u65F6\u95F4\u6233\uFF08\u4E0D\u53C2\u4E0E\u6838\u5FC3\u68C0\u6D4B\uFF09\u3002") else ""
          showNotification(paste0(quality_notification_text(ds[[id]]$quality), task_note), type = ifelse(any(ds[[id]]$quality$warning_level == "error"), "error", ifelse(any(ds[[id]]$quality$warning_level == "warning"), "warning", "message")), duration = 7)
        }
      }
      rv$datasets <- ds
      sync_dataset_selector(rv$current_id)
      if (!is.null(rv$current_id) && rv$current_id %in% names(rv$datasets)) {
        data_load_progress(
          0.98,
          "\u6B63\u5728\u540C\u6B65\u663E\u793A\u65F6\u95F4\u7A97",
          "\u6B63\u5728\u52A0\u8F7D\u5DF2\u6807\u8BB0 spike train"
        )
        refresh_xrange_slider(rv$datasets[[rv$current_id]]$trains, reset = TRUE)
      }
      if (loaded_count > 0L) {
        data_load_progress(
          1,
          sprintf("\u5DF2\u52A0\u8F7D %d \u4E2A\u5DF2\u6807\u8BB0\u6570\u636E\u96C6\uFF0C\u5931\u8D25 %d \u4E2A\u3002", loaded_count, failed_count),
          "\u52A0\u8F7D\u5B8C\u6210",
          "success"
        )
      } else {
        data_load_progress(
          1,
          "\u6CA1\u6709\u65B0\u6570\u636E\u96C6\u8FDB\u5165\u5185\u5B58\u3002\u8BF7\u68C0\u67E5\u6587\u4EF6\u683C\u5F0F\u3001\u91CD\u590D\u6570\u636E\u96C6\u6216\u9519\u8BEF\u901A\u77E5\u3002",
          "\u672A\u52A0\u8F7D\u65B0\u6570\u636E\u96C6",
          "error"
        )
      }
    })
  }, ignoreNULL = TRUE)
  
  observeEvent(input$workspace_in, {
    req(input$workspace_in)
    withProgress(message = "\u6B63\u5728\u52A0\u8F7D\u5DE5\u4F5C\u533A", value = 0, {
      data_load_progress(
        0.05,
        "\u6B63\u5728\u8BFB\u53D6 RDS \u6587\u4EF6",
        "\u6B63\u5728\u52A0\u8F7D\u5DE5\u4F5C\u533A"
      )
      obj <- tryCatch(
        stpd_safe_read_rds(input$workspace_in$datapath, max_bytes = 100 * 1024^2, label = "workspace RDS"),
        error = function(e) {
          data_load_progress(
            1,
            e$message,
            "\u5DE5\u4F5C\u533A RDS \u5BFC\u5165\u5931\u8D25",
            "error"
          )
          showNotification(paste0("\u5DE5\u4F5C\u533A RDS \u5BFC\u5165\u5931\u8D25\uFF1A", e$message), type = "error", duration = 8)
          NULL
        }
      )
      if (is.null(obj)) return(invisible(NULL))
      data_load_progress(
        0.18,
        "\u6B63\u5728\u9A8C\u8BC1\u5DE5\u4F5C\u533A\u7ED3\u6784",
        "\u6B63\u5728\u52A0\u8F7D\u5DE5\u4F5C\u533A"
      )
      if (!stpd_workspace_rds_is_valid(obj)) {
        data_load_progress(
          1,
          "\u5DE5\u4F5C\u533A\u6587\u4EF6\u65E0\u6548\u6216\u8D85\u51FA\u5B89\u5168\u5BFC\u5165\u9650\u5236\u3002",
          "\u5DE5\u4F5C\u533A RDS \u5BFC\u5165\u5931\u8D25",
          "error"
        )
      }
      validate(need(stpd_workspace_rds_is_valid(obj), "\u5DE5\u4F5C\u533A\u6587\u4EF6\u65E0\u6548\u6216\u8D85\u51FA\u5B89\u5168\u5BFC\u5165\u9650\u5236\u3002"))
      rv$datasets <- obj$datasets
      dataset_ids <- names(rv$datasets)
      if (length(rv$datasets) > 0) {
        for (ii in seq_along(dataset_ids)) {
          id <- dataset_ids[[ii]]
          ds_start <- 0.22 + 0.58 * (ii - 1) / max(1L, length(dataset_ids))
          ds_span <- 0.58 / max(1L, length(dataset_ids))
          data_load_progress(
            ds_start,
            sprintf("\u9884\u8BA1\u7B97 ISI \u767E\u5206\u4F4D %d/%d\uFF1A%s", ii, length(dataset_ids), id),
            "\u6B63\u5728\u52A0\u8F7D\u5DE5\u4F5C\u533A"
          )
          if (!is.null(rv$datasets[[id]]$trains)) {
            rv$datasets[[id]]$trains <- precompute_trains_isi_percentiles(
              rv$datasets[[id]]$trains,
              min_isi_sec = min_valid_isi_sec(),
              force = FALSE,
              progress = function(train, index, total) {
                data_load_progress(
                  value = ds_start + ds_span * min(0.95, max(0, index / max(1L, total))),
                  detail = sprintf("\u9884\u8BA1\u7B97 %s\uFF1Atrain %d/%d", id, index, total),
                  message = "\u6B63\u5728\u52A0\u8F7D\u5DE5\u4F5C\u533A"
                )
              }
            )
          }
        }
      }
      data_load_progress(
        0.84,
        "\u6B63\u5728\u6062\u590D\u5DE5\u4F5C\u533A\u72B6\u6001",
        "\u6B63\u5728\u52A0\u8F7D\u5DE5\u4F5C\u533A"
      )
      rv$current_id <- obj$current_id %||% if (length(obj$datasets) > 0) names(obj$datasets)[1] else NULL
      sync_dataset_selector(rv$current_id)
      rv$nn_model <- obj$nn_model %||% NULL
      rv$nn_training_info <- obj$nn_training_info %||% NULL
	    rv$nn_eval <- obj$nn_eval %||% NULL
	    rv$manual_detector_eval <- obj$manual_detector_eval %||% NULL
	    rv$scientific_validation <- obj$scientific_validation %||% NULL
	    rv$parameter_sensitivity <- obj$parameter_sensitivity %||% NULL
	    rv$parameter_sensitivity_status <- obj$parameter_sensitivity_status %||% "\u5C1A\u672A\u8FD0\u884C\u4E8B\u4EF6\u7EA7\u53C2\u6570\u654F\u611F\u6027\u626B\u63CF\u3002"
	    rv$last_detector_summary <- obj$last_detector_summary %||% "\u5C1A\u65E0\u68C0\u6D4B\u5668\u91CD\u8DD1\u6458\u8981\u3002"
      rv$near_miss_rerun_summary <- obj$near_miss_rerun_summary %||% "\u5C1A\u65E0\u9608\u503C\u5E94\u7528/\u91CD\u8DD1\u6458\u8981\u3002"
      rv$batch_status <- obj$batch_status %||% "\u5C1A\u672A\u8FD0\u884C\u6279\u5904\u7406\u3002"
      rv$isi_profile_ref <- obj$isi_profile_ref %||% NULL
      if (!is.null(rv$current_id) && rv$current_id %in% names(rv$datasets)) {
        data_load_progress(
          0.95,
          "\u6B63\u5728\u540C\u6B65\u663E\u793A\u65F6\u95F4\u7A97",
          "\u6B63\u5728\u52A0\u8F7D\u5DE5\u4F5C\u533A"
        )
        refresh_xrange_slider(rv$datasets[[rv$current_id]]$trains, reset = TRUE)
      }
      data_load_progress(
        1,
        sprintf("\u5DF2\u6062\u590D %d \u4E2A\u6570\u636E\u96C6\u3002", length(rv$datasets)),
        "\u5DE5\u4F5C\u533A\u52A0\u8F7D\u5B8C\u6210",
        "success"
      )
    })
  })
  
  output$workspace_out <- downloadHandler(
    filename = function() paste0("spike_detector_workspace_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"),
    content = function(file) {
      saveRDS(list(datasets = rv$datasets, current_id = rv$current_id,
                   nn_model = rv$nn_model,
                   nn_training_info = rv$nn_training_info,
	                   nn_eval = rv$nn_eval,
	                   manual_detector_eval = rv$manual_detector_eval,
	                   scientific_validation = rv$scientific_validation,
	                   parameter_sensitivity = rv$parameter_sensitivity,
	                   parameter_sensitivity_status = rv$parameter_sensitivity_status,
	                   last_detector_summary = rv$last_detector_summary,
                   near_miss_rerun_summary = rv$near_miss_rerun_summary,
                   batch_status = rv$batch_status,
                   isi_profile_ref = rv$isi_profile_ref), file)
    }
  )
  
  observeEvent(input$clear_all_datasets, {
    rv$datasets <- list()
    rv$current_id <- NULL
  })
  
  output$dataset_selector <- renderUI({
    ds <- rv$datasets
    if (length(ds) == 0) return(helpText("\u672A\u52A0\u8F7D\u6570\u636E\u96C6\u3002"))
    ids <- names(ds)
    labels <- dataset_selector_labels(ds)
    selectizeInput("dataset_id", "\u5F53\u524D\u6570\u636E\u96C6", choices = setNames(ids, labels), selected = rv$current_id %||% ids[1], multiple = FALSE)
  })
  
  observeEvent(input$dataset_id, {
    id <- as.character(input$dataset_id %||% "")[1]
    if (!nzchar(id) || !(id %in% names(rv$datasets))) return()
    rv$current_id <- id
  }, ignoreInit = TRUE)
  
  output$pool_dataset_selector <- renderUI({
    ds <- rv$datasets
    if (length(ds) <= 1) return(NULL)
    ids <- names(ds)
    labels <- vapply(ds, function(x) paste0("[", x$meta$source, "] ", x$meta$display_name), character(1))
    selectizeInput("pool_ids", "\u7528\u4E8E\u53C2\u6570\u4F30\u8BA1 / \u5408\u5E76\u76F4\u65B9\u56FE\u7684\u6570\u636E\u96C6",
                   choices = setNames(ids, labels), selected = rv$current_id %||% ids[1], multiple = TRUE,
                   options = list(placeholder = "\u9009\u62E9\u4E00\u4E2A\u6216\u591A\u4E2A\u6570\u636E\u96C6"))
  })
  
  observeEvent(input$remove_dataset, {
    id <- rv$current_id
    req(id)
    ds <- rv$datasets
    if (!(id %in% names(ds))) return()
    ds[[id]] <- NULL
    rv$datasets <- ds
    rv$current_id <- if (length(ds) > 0) names(ds)[1] else NULL
    sync_dataset_selector(rv$current_id)
  })

  collapse_duplicate_spikes_for_dataset <- function(ds, dataset_label = "\u5F53\u524D\u6570\u636E\u96C6") {
    if (is.null(ds) || is.null(ds$trains) || length(ds$trains) == 0) {
      return(list(dataset = ds, dropped = 0L, summary = data.frame()))
    }
    res <- collapse_duplicate_timestamps_trains(ds$trains, policy_label = "collapse_manual")
    dropped <- sum(res$summary$dropped_duplicate_spikes, na.rm = TRUE)
    if (dropped <= 0) {
      return(list(dataset = ds, dropped = 0L, summary = res$summary))
    }
    ds$trains <- precompute_trains_isi_percentiles(res$trains, min_isi_sec = min_valid_isi_sec(), force = TRUE)
    # AUTO labels and detector results become stale after row deletion / ISI recomputation.
    ds$trains <- lapply(ds$trains, function(dat) {
      if (!is.null(dat) && nrow(dat) > 0 && "pattern_auto" %in% names(dat)) dat$pattern_auto <- ""
      dat
    })
    ds$results <- list()
    ds$quality <- validate_dataset_quality_impl(ds$trains,
      min_isi_sec = min_valid_isi_sec(),
      unit_hint = ds$meta$unit_in %||% "s",
      refractory_suspect_sec = refractory_suspect_sec(),
      display_unit = qc_isi_unit()
    )
    ds$meta$duplicate_collapse_last <- list(
      time = as.character(Sys.time()),
      dropped_duplicate_spikes = as.integer(dropped),
      policy = "collapse_manual",
      dataset_label = dataset_label
    )
    list(dataset = ds, dropped = as.integer(dropped), summary = res$summary)
  }

  observeEvent(input$collapse_duplicate_spikes_current, {
    id <- rv$current_id
    req(id)
    ds_all <- rv$datasets
    if (!(id %in% names(ds_all))) return()
    res <- collapse_duplicate_spikes_for_dataset(normalize_dataset(ds_all[[id]]), dataset_label = id)
    ds_all[[id]] <- res$dataset
    rv$datasets <- ds_all
    if (res$dropped > 0) {
      refresh_xrange_slider(res$dataset$trains, reset = TRUE)
      showNotification(paste0("\u5DF2\u5408\u5E76 ", res$dropped, " \u4E2A\u5F53\u524D\u6570\u636E\u96C6\u4E2D\u7684\u5B8C\u5168\u91CD\u590D spike \u65F6\u95F4\u6233\u3002AUTO \u6807\u7B7E/\u7ED3\u679C\u5DF2\u6E05\u7A7A\uFF1B\u8BF7\u91CD\u65B0\u8FD0\u884C\u68C0\u6D4B\u5668\u3002"), type = "message", duration = 8)
    } else {
      showNotification("\u5F53\u524D\u6570\u636E\u96C6\u4E2D\u672A\u53D1\u73B0\u5B8C\u5168\u91CD\u590D\u65F6\u95F4\u6233\u3002", type = "message", duration = 5)
    }
  })

  observeEvent(input$collapse_duplicate_spikes_all, {
    if (length(rv$datasets) == 0) return()
    ds_all <- rv$datasets
    total <- 0L
    for (id in names(ds_all)) {
      res <- collapse_duplicate_spikes_for_dataset(normalize_dataset(ds_all[[id]]), dataset_label = id)
      ds_all[[id]] <- res$dataset
      total <- total + res$dropped
    }
    rv$datasets <- ds_all
    if (!is.null(rv$current_id) && rv$current_id %in% names(rv$datasets)) {
      refresh_xrange_slider(rv$datasets[[rv$current_id]]$trains, reset = TRUE)
    }
    if (total > 0) {
      showNotification(paste0("\u5DF2\u5408\u5E76 ", total, " \u4E2A\u6240\u6709\u5DF2\u52A0\u8F7D\u6570\u636E\u96C6\u4E2D\u7684\u5B8C\u5168\u91CD\u590D spike timestamp\u3002AUTO \u6807\u7B7E/\u7ED3\u679C\u5DF2\u6E05\u7A7A\uFF1B\u8BF7\u91CD\u65B0\u8FD0\u884C\u68C0\u6D4B\u5668\u3002"), type = "message", duration = 8)
    } else {
      showNotification("\u5DF2\u52A0\u8F7D\u6570\u636E\u96C6\u4E2D\u672A\u53D1\u73B0\u5B8C\u5168\u91CD\u590D timestamp\u3002", type = "message", duration = 5)
    }
  })
  
  # ----------------------------------------------------------
  # Train selector and x range
  # ----------------------------------------------------------
  output$train_page_selector <- renderUI({
    td <- current_trains()
    choices <- metadata_filtered_train_names()
    if (length(choices) == 0) return(NULL)
    per <- safe_int(input$visible_trains_per_page, 10L)
    per <- max(1L, min(per, max(1L, length(choices))))
    n_pages <- max(1L, ceiling(length(choices) / per))
    numericInput("train_page", "Train \u9875\u7801\uFF08\u8BB0\u5F55\u6761\u76EE\u9875\u7801\uFF09", value = min(safe_int(input$train_page, 1L), n_pages), min = 1, max = n_pages, step = 1)
  })

  output$train_selector <- renderUI({
    td <- current_trains()
    choices <- metadata_filtered_train_names()
    validate(need(length(choices) > 0, "\u5F53\u524D\u5143\u6570\u636E\u8FC7\u6EE4\u540E\u6CA1\u6709\u53EF\u7528 train\u3002"))
    if (identical(input$train_display_mode %||% "paged_all", "paged_all")) {
      per <- safe_int(input$visible_trains_per_page, 10L)
      per <- max(1L, min(per, length(choices)))
      page <- max(1L, safe_int(input$train_page, 1L))
      n_pages <- max(1L, ceiling(length(choices) / per))
      page <- min(page, n_pages)
      idx <- ((page - 1L) * per + 1L):min(length(choices), page * per)
      return(tags$div(class = "small-note",
                      paste0(length(choices), " \u6761 metadata-filtered train \u5904\u4E8E\u6D3B\u52A8\u72B6\u6001\u3002\u5F53\u524D\u6E32\u67D3\u7B2C ", page,
                             "/", n_pages, ": ", paste(choices[idx], collapse = ", "))))
    }
    selectizeInput("trains", "\u9009\u62E9\u8981\u663E\u793A\u7684 trains", choices = choices, multiple = TRUE,
                   selected = head(choices, 10), options = list(placeholder = "\u9009\u62E9 trains\uFF08\u8BB0\u5F55\u6761\u76EE\uFF09"))
  })

  displayed_train_names <- reactive({
    td <- current_trains()
    choices <- metadata_filtered_train_names()
    if (length(choices) == 0) return(character(0))
    if (identical(input$train_display_mode %||% "paged_all", "paged_all")) {
      per <- safe_int(input$visible_trains_per_page, 10L)
      per <- max(1L, min(per, length(choices)))
      page <- max(1L, safe_int(input$train_page, 1L))
      n_pages <- max(1L, ceiling(length(choices) / per))
      page <- min(page, n_pages)
      idx <- ((page - 1L) * per + 1L):min(length(choices), page * per)
      return(choices[idx])
    }
    sel <- intersect(input$trains %||% head(choices, 10), choices)
    if (length(sel) == 0) sel <- head(choices, 1)
    sel
  })

  active_train_names_for_ops <- function(default_all = FALSE) {
    td <- tryCatch(current_trains(), error = function(e) NULL)
    if (is.null(td) || length(td) == 0) return(character(0))
    if (isTRUE(default_all)) return(names(td))
    displayed_train_names()
  }

  output$burst_range_selector <- renderUI({
    td <- current_trains()
    choices <- names(td)
    # Preserve the user's explicit selection, including an intentionally empty
    # selection.  Do NOT auto-repopulate this selector from displayed trains
    # after detector runs or dataset updates.  These controls are legacy
    # train-specific calibration controls and should not behave like the
    # detector target-train selector.
    selected <- if (is.null(input$burst_range_trains)) character(0) else intersect(input$burst_range_trains, choices)
    selectizeInput("burst_range_trains", "\u7528\u4E8E\u65E7\u7248 burst-ISI \u8303\u56F4\u5206\u914D\u7684 train(s)", choices = choices,
                   selected = selected, multiple = TRUE,
                   options = list(placeholder = "\u53EF\u9009\uFF1A\u9009\u62E9\u8981\u4FDD\u5B58/\u6E05\u9664\u65E7\u7248\u8303\u56F4\u7684 train(s)", plugins = list("remove_button")))
  })
  
  output$burst_range_selector_tab <- renderUI({
    td <- current_trains()
    choices <- names(td)
    selected <- if (is.null(input$burst_range_trains_tab)) character(0) else intersect(input$burst_range_trains_tab, choices)
    selectizeInput("burst_range_trains_tab", "\u65E7\u7248 train(s)", choices = choices,
                   selected = selected, multiple = TRUE,
                   options = list(placeholder = "\u53EF\u9009\uFF1A\u9009\u62E9\u8981\u4FDD\u5B58/\u6E05\u9664\u65E7\u7248\u8303\u56F4\u7684 train(s)", plugins = list("remove_button")))
  })
  
  output$isi_table_train_selector <- renderUI({
    td <- current_trains()
    choices <- names(td)
    selected <- intersect(displayed_train_names() %||% head(choices, 1), choices)
    selectizeInput("isi_table_trains", "\u5F53\u524D\u663E\u793A\u7684 train(s)", choices = choices,
                   selected = selected, multiple = TRUE, options = list(maxItems = 20, placeholder = "\u9009\u62E9 train(s)"))
  })
  
  track_step <- reactive({
    td <- current_trains()
    sel <- displayed_train_names()
    k <- length(sel)
    if (k <= 0) return(1)
    step <- 7.2 / k
    step <- min(1, step)
    step <- max(0.45, step)
    step
  })
  

  }, envir = server_env)
}

stpd_server_install_detection_module <- function(server_env) {
  evalq({
    detector_notify_error <- function(e, prefix = "\u68C0\u6D4B\u5668\u8FD0\u884C\u5931\u8D25", status = c("detector", "batch", "parameter_sensitivity", "near_miss")) {
      status <- match.arg(status)
      msg <- stpd_shiny_detector_error_message(e, prefix = prefix)
      if (identical(status, "batch")) {
        rv$batch_status <- msg
      } else if (identical(status, "parameter_sensitivity")) {
        rv$parameter_sensitivity_status <- msg
      } else if (identical(status, "near_miss")) {
        rv$near_miss_rerun_summary <- msg
      } else {
        rv$last_detector_summary <- msg
      }
      showNotification(msg, type = "error", duration = 15)
      if (isTRUE(stpd_error_mentions_qc(e))) updateTabsetPanel(session, "main_tabs", selected = "\u6570\u636E QC")
      invisible(NULL)
    }

	  run_detector_from_ui <- function(params_override = NULL, message = "\u6B63\u5728\u8FD0\u884C\u68C0\u6D4B\u5668", switch_to_plot = TRUE, notify = TRUE,
                                     target_trains_override = NULL) {
	    ds <- current_dataset()
	    p <- params_override %||% read_params_from_ui()
	    param_issues <- stpd_validate_params(p)
	    if (any(param_issues$severity == "error", na.rm = TRUE)) {
	      bad <- param_issues[param_issues$severity == "error", , drop = FALSE]
	      msg <- paste0("\u53C2\u6570 contract \u9A8C\u8BC1\u5931\u8D25\uFF0C\u5DF2\u963B\u6B62\u8FD0\u884C\uFF1A", paste(head(paste(bad$path, bad$issue, sep = " - "), 3), collapse = "; "))
	      showNotification(msg, type = "error", duration = 10)
	      rv$last_detector_summary <- msg
	      return(invisible(NULL))
	    }
	    p <- effective_params_for_detector(p)
	    override_target <- !is.null(target_trains_override)
	    selected_only <- isTRUE(input$detector_selected_only)
	    target_trains <- names(ds$trains)
      scope_txt <- NULL
      if (isTRUE(override_target)) {
        target_trains <- intersect(as.character(target_trains_override), names(ds$trains))
        selected_only <- TRUE
        if (length(target_trains) == 0) {
          msg <- "No target trains to run detector on."
          rv$last_detector_summary <- msg
          showNotification(msg, type = "error", duration = 8)
          return(invisible(NULL))
        }
        scope_txt <- paste0("specified trains: ", length(target_trains))
      } else if (selected_only) {
	      target_trains <- intersect(displayed_train_names() %||% character(0), names(ds$trains))
	      if (length(target_trains) == 0) {
	        msg <- "No selected trains to run detector on."
        rv$last_detector_summary <- msg
        showNotification(msg, type = "error", duration = 8)
        return(invisible(NULL))
      }
    }
    pre_qc <- tryCatch(
      stpd_product_pre_detection_qc(ds, p, target_trains),
      error = function(e) data.frame(warning_level = "error", train = "", warning_message = conditionMessage(e), stringsAsFactors = FALSE)
    )
    qc_msg <- stpd_product_qc_error_message(pre_qc)
    auto_collapsed_duplicates <- 0L
    if (nzchar(qc_msg)) {
      if (isTRUE(stpd_qc_errors_are_exact_duplicate_only(pre_qc))) {
        res <- collapse_duplicate_spikes_for_dataset(normalize_dataset(ds), dataset_label = rv$current_id %||% "\u5F53\u524D\u6570\u636E\u96C6")
        ds <- res$dataset
        auto_collapsed_duplicates <- as.integer(res$dropped %||% 0L)
        if (!is.null(rv$current_id) && nzchar(rv$current_id)) set_dataset(rv$current_id, ds)
        if (auto_collapsed_duplicates > 0) {
          target_trains <- intersect(target_trains, names(ds$trains))
          refresh_xrange_slider(ds$trains, reset = FALSE)
          showNotification(
            paste0("\u68C0\u6D4B\u524D QC \u53D1\u73B0\u5B8C\u5168\u91CD\u590D timestamp\uFF0C\u5DF2\u81EA\u52A8\u5408\u5E76 ", auto_collapsed_duplicates, " \u4E2A\u91CD\u590D spike \u540E\u7EE7\u7EED\u8FD0\u884C\u3002"),
            type = "warning",
            duration = 10
          )
        }
      } else {
        msg <- paste0(
          stpd_shiny_detector_error_message(simpleError(qc_msg), prefix = "\u68C0\u6D4B\u5668\u8FD0\u884C\u5DF2\u963B\u6B62"),
          "\n\u8BF7\u5148\u5728\u201C\u6570\u636E QC\u201D\u4E2D\u67E5\u770B\uFF0C\u6216\u5728\u5DE6\u4FA7\u9AD8\u7EA7 QC \u4E2D\u5904\u7406\u91CD\u590D/0 ISI \u540E\u518D\u8FD0\u884C\u3002"
        )
        rv$last_detector_summary <- msg
        showNotification(msg, type = "error", duration = 15)
        updateTabsetPanel(session, "main_tabs", selected = "\u6570\u636E QC")
        return(invisible(NULL))
      }
    }
	    before_counts <- detector_event_counts(ds, target_trains = if (selected_only) target_trains else NULL)
	    run_error <- NULL
	    ds <- tryCatch(
	      withProgress(message = message, value = 0, {
	        detector_progress <- function(phase, train = NULL, index = NULL, total = NULL, detail = NULL) {
	          total_n <- max(1L, suppressWarnings(as.integer(total %||% length(target_trains))))
	          idx <- suppressWarnings(as.integer(index %||% 0L))
	          if (!is.finite(idx)) idx <- 0L
	          train_txt <- as.character(train %||% "")
	          phase <- as.character(phase %||% "")
	          value <- switch(
	            phase,
	            prepare = 0.02,
	            thresholds = 0.06,
	            train_start = 0.08 + 0.72 * max(0, idx - 1L) / total_n,
	            train_done = 0.08 + 0.72 * min(total_n, max(0, idx)) / total_n,
	            assemble_events = 0.82,
	            diagnostics = 0.86,
	            ledger = 0.90,
	            features = 0.93,
	            final_audits = 0.95,
	            report_tables = 0.965,
	            complete = 0.97,
	            public_ledgers = 0.975,
	            public_features = 0.982,
	            public_final = 0.988,
	            distributional_evidence = 0.990,
	            public_reports = 0.993,
	            public_complete = 0.995,
	            0.08
	          )
	          detail_txt <- detail %||% switch(
	            phase,
	            prepare = "\u6B63\u5728\u8FDB\u884C\u68C0\u6D4B\u524D QC",
	            thresholds = "\u6B63\u5728\u89E3\u6790 dataset/MANUAL \u9608\u503C",
	            train_start = paste0("\u6B63\u5728\u68C0\u6D4B train ", idx, "/", total_n, if (nzchar(train_txt)) paste0(": ", train_txt) else ""),
	            train_done = paste0("\u5DF2\u5B8C\u6210 train ", idx, "/", total_n, if (nzchar(train_txt)) paste0(": ", train_txt) else ""),
	            assemble_events = "\u6B63\u5728\u91CD\u5EFA\u4E8B\u4EF6\u8868",
	            diagnostics = "\u6B63\u5728\u6C47\u603B\u8BCA\u65AD\u8868",
	            ledger = "\u6B63\u5728\u91CD\u5EFA candidate/event ledger",
	            features = "\u6B63\u5728\u8BA1\u7B97\u5019\u9009\u7279\u5F81",
	            final_audits = "\u6B63\u5728\u8BA1\u7B97\u6700\u7EC8\u5206\u7C7B\u5BA1\u8BA1",
	            report_tables = "\u6B63\u5728\u751F\u6210\u9A8C\u8BC1\u4E0E\u62A5\u544A\u8868",
	            public_ledgers = "\u6B63\u5728\u540C\u6B65\u516C\u5171\u5019\u9009\u8868",
	            public_features = "\u6B63\u5728\u540C\u6B65\u516C\u5171\u5019\u9009\u7279\u5F81",
	            public_final = "\u6B63\u5728\u540C\u6B65\u516C\u5171\u6700\u7EC8\u51B3\u7B56",
	            distributional_evidence = "\u6B63\u5728\u8BA1\u7B97\u5206\u5E03\u8BC1\u636E\u548C train \u7EA7 phenotype",
	            public_reports = "\u6B63\u5728\u751F\u6210\u4E00\u81F4\u6027\u548C\u9A8C\u8BC1\u6458\u8981",
	            public_complete = "\u516C\u5171\u8F93\u51FA\u5DF2\u5B8C\u6210",
	            complete = "\u68C0\u6D4B\u7ED3\u679C\u5DF2\u540C\u6B65",
	            "\u6B63\u5728\u8FD0\u884C\u68C0\u6D4B\u5668"
	          )
	          setProgress(value = min(0.995, max(0, value)), detail = detail_txt)
	        }
	        out_ds <- stpd_detect(
	          ds,
	          p,
	          selected_trains = target_trains,
	          lock_manual = TRUE,
	          collect_diagnostics = TRUE,
	          progress_callback = detector_progress
	        )
	        audit_policy <- ds$results$final_audit_policy %||% list()
	        out_ds <- stpd_apply_final_audit(
	          out_ds,
	          selected_trains = target_trains,
	          promote_possible = isTRUE(audit_policy$promote_possible %||% FALSE),
	          min_isi_sec = min_valid_isi_sec(),
	          reason = "detector_run_sync_final_audit",
	          user = Sys.info()[["user"]] %||% NA_character_
	        )$dataset
	        out_ds <- stpd_add_distributional_results(
	          out_ds,
	          params = p,
	          selected_trains = target_trains,
	          candidates = out_ds$results$candidate_features %||% out_ds$results$candidate_ledger %||% data.frame()
	        )
	        setProgress(value = 1, detail = "Detector, diagnostics, candidate ledger, and result layers rebuilt")
	        out_ds
	      }),
	      error = function(e) {
	        run_error <<- e
	        ds
	      }
	    )
	    if (!is.null(run_error)) {
	      msg <- stpd_shiny_detector_error_message(run_error, prefix = "\u68C0\u6D4B\u5668\u8FD0\u884C\u5931\u8D25")
	      rv$last_detector_summary <- msg
	      showNotification(msg, type = "error", duration = 15)
	      if (stpd_error_mentions_qc(run_error) || grepl("Pre-detection QC|data-integrity|duplicate|artifact|ISI", conditionMessage(run_error), ignore.case = TRUE)) {
	        updateTabsetPanel(session, "main_tabs", selected = "\u6570\u636E QC")
	      }
	      return(invisible(NULL))
	    }
    set_dataset(rv$current_id, ds)
    if (isTRUE(switch_to_plot)) updateTabsetPanel(session, "main_tabs", selected = "\u5BF9\u9F50\u65F6\u95F4\u6233\u56FE")
    after_counts <- detector_event_counts(ds, target_trains = if (selected_only) target_trains else NULL)
	    if (is.null(scope_txt)) scope_txt <- if (selected_only) paste0("selected trains only: ", length(target_trains)) else paste0("all trains: ", length(target_trains))
    rv$last_detector_summary <- format_detector_before_after(before_counts, after_counts, scope_txt = scope_txt)
    if (auto_collapsed_duplicates > 0) {
      rv$last_detector_summary <- paste0(
        "Pre-detection QC auto-collapsed ", auto_collapsed_duplicates, " exact duplicate timestamp row(s).\n",
        rv$last_detector_summary
      )
    }
    if (isTRUE(notify)) {
      showNotification(paste0("\u68C0\u6D4B\u5668\u5B8C\u6210\uFF08", scope_txt, "). AUTO labels, diagnostics, candidate ledger, and result layers are synchronized. Events: burst=", after_counts["burst"],
                              ", long_burst=", after_counts["long_burst"],
                              ", possible_burst=", after_counts["possible_burst"],
                              ", pause=", after_counts["pause"],
                              ", tonic=", after_counts["tonic"],
                              ", hf_tonic=", after_counts["high_frequency_tonic"],
                              ", hf_spiking=", after_counts["high_frequency_spiking"], "."),
                       type = "message", duration = 8)
    }
    invisible(list(dataset = ds, params = p, before = before_counts, after = after_counts, target_trains = target_trains))
  }

  observeEvent(input$run_detector, {
    tryCatch(
      run_detector_from_ui(),
      shiny.silent.error = function(e) detector_notify_error(e, prefix = "\u68C0\u6D4B\u5668\u8FD0\u884C\u672A\u5B8C\u6210"),
      error = function(e) detector_notify_error(e, prefix = "\u68C0\u6D4B\u5668\u8FD0\u884C\u5931\u8D25")
    )
  })


  }, envir = server_env)
}

stpd_server_install_ml_module <- function(server_env) {
  evalq({
  # ----------------------------------------------------------
  # refined ML feature extraction and optional nnet workflow
  # ----------------------------------------------------------
  ml_feature_table_current <- reactive({
    ds <- current_dataset()
    extract_ml_feature_table(ds$trains,
                             source = input$ml_label_source %||% "audit_final",
                             auto_others = FALSE,
                             fill_blank_others = isTRUE(input$ml_fill_blank_others),
                             min_isi_sec = min_valid_isi_sec(),
                             context_n = safe_int(input$ml_context_n, 3L),
                                    ml_mode = input$ml_label_mode %||% "strict_high_confidence")
  })
  
  output$ml_feature_preview <- renderDT({
    df <- ml_feature_table_current()
    if (nrow(df) == 0) return(datatable(data.frame(message = "No ML feature rows."), options = list(dom = "t")))
    datatable(head(df, 500), rownames = FALSE, options = list(pageLength = 20, scrollX = TRUE))
  })
  
  observeEvent(input$train_nn_model, {
    ids <- if (isTRUE(input$ml_train_pool)) pool_dataset_ids() else rv$current_id
    ids <- ids[ids %in% names(rv$datasets)]
    validate(need(length(ids) > 0, "No dataset selected for NN training."))
    parts <- list()
    for (id in ids) {
      d0 <- normalize_dataset(rv$datasets[[id]])
      x <- extract_ml_feature_table(d0$trains,
                                    source = input$ml_label_source %||% "audit_final",
                                    auto_others = FALSE,
                                    fill_blank_others = isTRUE(input$ml_fill_blank_others),
                                    min_isi_sec = min_valid_isi_sec(),
                                    context_n = safe_int(input$ml_context_n, 3L),
                                    ml_mode = input$ml_label_mode %||% "strict_high_confidence")
      if (nrow(x) > 0) {
        x$dataset <- d0$meta$display_name
        x$train_display <- x$train
        x$train <- paste0(d0$meta$display_name, "::", x$train)
        parts[[length(parts) + 1L]] <- x
      }
    }
    df <- if (length(parts) > 0) bind_rows(parts) else data.frame()
    ds <- current_dataset()
    ds <- normalize_dataset(ds)
    ds$ml$last_feature_table <- df
    set_dataset(rv$current_id, ds)
    mdl <- tryCatch(
      train_nnet_pattern_model(df,
                               hidden = safe_int(input$ml_hidden, 12L),
                               decay = safe_ui_value(input$ml_decay, 0.001),
                               maxit = safe_int(input$ml_maxit, 300L),
                               ml_mode = input$ml_label_mode %||% "strict_high_confidence"),
      error = function(e) {
        showNotification(paste0("NN training failed: ", e$message), type = "error", duration = 8)
        NULL
      }
    )
    if (!is.null(mdl)) {
      rv$nn_model <- mdl
      rv$nn_training_info <- list(
        dataset_ids = ids,
        label_source = input$ml_label_source %||% "audit_final",
        ml_label_mode = input$ml_label_mode %||% "strict_high_confidence",
        n_rows_extracted = nrow(df),
        trained_at = as.character(Sys.time())
      )
      rv$nn_eval <- NULL
      showNotification("NN model trained and stored in memory. Use Download trained NN model to save it.", type = "message", duration = 5)
    }
  })
  
  observeEvent(input$nn_model_in, {
    req(input$nn_model_in)
    obj <- tryCatch(
      stpd_safe_read_rds(input$nn_model_in$datapath, max_bytes = 50 * 1024^2, label = "NN model RDS"),
      error = function(e) {
        showNotification(paste0("NN model RDS import failed: ", e$message), type = "error", duration = 8)
        NULL
      }
    )
    validate(need(stpd_nn_model_rds_is_valid(obj), "Invalid model RDS or unsafe model schema."))
    rv$nn_model <- obj
    rv$nn_training_info <- NULL
    rv$nn_eval <- NULL
    showNotification("Loaded trained NN model.", type = "message", duration = 4)
  })
  
  observeEvent(input$apply_nn_model, {
    validate(need(!is.null(rv$nn_model), "Train or load a neural-network model first."))
    ds <- current_dataset()
    td <- ds$trains
    params <- read_params_from_ui()
    feat <- extract_ml_feature_table(td, source = "none", auto_others = FALSE, fill_blank_others = FALSE,
                                     min_isi_sec = min_valid_isi_sec(), context_n = safe_int(input$ml_context_n, 3L),
                                    ml_mode = input$ml_label_mode %||% "strict_high_confidence")
    pred <- tryCatch(
      predict_nnet_pattern_model(rv$nn_model, feat, confidence_cutoff = safe_ui_value(input$ml_confidence, 0.60)),
      error = function(e) {
        showNotification(paste0("NN prediction failed: ", e$message), type = "error", duration = 8)
        NULL
      }
    )
    if (is.null(pred) || nrow(pred) == 0) return()

    applied_n <- 0L
    use_nn_event_guardrails <- isTRUE(input$ml_nn_event_guardrails) || isTRUE(input$ml_event_smoothing)
    if (isTRUE(use_nn_event_guardrails)) {
      event_rows <- list()
      for (tr in names(td)) {
        dat <- td[[tr]]
        if (!("auto_score" %in% names(dat))) dat$auto_score <- NA_real_
        pp <- pred[pred$train == tr, , drop = FALSE]
        ev <- postprocess_nn_predictions_for_train(
          dat, pp, params,
          min_isi_sec = min_valid_isi_sec(),
          train = tr,
          apply_others = isTRUE(input$ml_apply_others)
        )
        if (nrow(ev) > 0) {
          event_rows[[length(event_rows) + 1L]] <- ev
          for (ii in seq_len(nrow(ev))) {
            idx <- ev$start_isi[ii]:ev$end_isi[ii]
            idx <- idx[!is.na(dat$pattern_manual[idx]) & dat$pattern_manual[idx] == ""]
            if (length(idx) == 0) next
            dat$pattern_auto[idx] <- ev$label[ii]
            dat$auto_score[idx] <- ev$mean_confidence[ii]
            applied_n <- applied_n + length(idx)
          }
        }
        td[[tr]] <- dat
      }
      pred$event_validated <- FALSE
      if (length(event_rows) > 0) {
        ev_all <- bind_rows(event_rows)
        pred$event_validated <- vapply(seq_len(nrow(pred)), function(ii) {
          any(ev_all$train == pred$train[ii] & pred$isi_idx[ii] >= ev_all$start_isi & pred$isi_idx[ii] <= ev_all$end_isi)
        }, logical(1))
      }
    } else {
      for (tr in names(td)) {
        dat <- td[[tr]]
        if (!("auto_score" %in% names(dat))) dat$auto_score <- NA_real_
        pp <- pred[pred$train == tr & pred$accepted, , drop = FALSE]
        if (nrow(pp) == 0) next
        for (ii in seq_len(nrow(pp))) {
          idx <- safe_int(pp$isi_idx[ii], NA_integer_)
          if (!is.finite(idx) || idx < 2 || idx > nrow(dat)) next
          if (!is.na(dat$pattern_manual[idx]) && dat$pattern_manual[idx] != "") next
          lab <- normalize_pattern_label(pp$pred_label[ii], fill_blank_others = FALSE)[1]
          if (lab == "burst_family") lab <- "possible_burst"
          if (lab == "others" && !isTRUE(input$ml_apply_others)) next
          dat$pattern_auto[idx] <- lab
          dat$auto_score[idx] <- pp$pred_confidence[ii]
          applied_n <- applied_n + 1L
        }
        td[[tr]] <- dat
      }
    }

    ds$trains <- td
    ds <- normalize_dataset(ds)
    ds$ml$last_prediction_table <- pred
    set_dataset(rv$current_id, ds)
    showNotification(paste0("NN predictions applied to AUTO for ", applied_n, " ISIs after current filtering/validation", ifelse(isTRUE(use_nn_event_guardrails), " (event-grammar guardrails ON).", " (raw per-ISI mode; guardrails OFF).")), type = "message", duration = 6)
  })
  
  observeEvent(input$evaluate_nn_model, {
    mode <- input$ml_eval_mode %||% "loaded_current"
    ds <- current_dataset()
    ds <- normalize_dataset(ds)

    if (mode == "leave_one_dataset_out") {
      ids <- pool_dataset_ids()
      ids <- ids[ids %in% names(rv$datasets)]
      validate(need(length(ids) >= 2, "Leave-one-dataset-out evaluation requires at least two selected datasets."))
      all_eval <- list()
      all_cm <- list()
      all_metrics <- list()
      withProgress(message = "Leave-one-dataset-out NN evaluation", value = 0, {
        for (ii in seq_along(ids)) {
          test_id <- ids[ii]
          train_ids <- setdiff(ids, test_id)
          incProgress(1 / max(length(ids), 1), detail = paste0("held out: ", test_id))
          train_parts <- list()
          for (id in train_ids) {
            d0 <- normalize_dataset(rv$datasets[[id]])
            x <- extract_ml_feature_table(d0$trains,
                                          source = input$ml_label_source %||% "audit_final",
                                          auto_others = FALSE,
                                          fill_blank_others = isTRUE(input$ml_fill_blank_others),
                                          min_isi_sec = min_valid_isi_sec(),
                                          context_n = safe_int(input$ml_context_n, 3L),
                                    ml_mode = input$ml_label_mode %||% "strict_high_confidence")
            if (nrow(x) > 0) {
              x$dataset <- d0$meta$display_name
              x$train <- paste0(d0$meta$display_name, "::", x$train)
              train_parts[[length(train_parts) + 1L]] <- x
            }
          }
          train_df <- if (length(train_parts) > 0) bind_rows(train_parts) else data.frame()
          if (nrow(train_df) < 10) next
          mdl <- tryCatch(
            train_nnet_pattern_model(train_df,
                                     hidden = safe_int(input$ml_hidden, 12L),
                                     decay = safe_ui_value(input$ml_decay, 0.001),
                                     maxit = safe_int(input$ml_maxit, 300L),
                               ml_mode = input$ml_label_mode %||% "strict_high_confidence"),
            error = function(e) NULL
          )
          if (is.null(mdl)) next
          dtest <- normalize_dataset(rv$datasets[[test_id]])
          test_df <- extract_ml_feature_table(dtest$trains,
                                              source = input$ml_eval_source %||% "manual",
                                              auto_others = FALSE,
                                              fill_blank_others = FALSE,
                                              min_isi_sec = min_valid_isi_sec(),
                                              context_n = safe_int(input$ml_context_n, 3L),
                                    ml_mode = input$ml_label_mode %||% "strict_high_confidence")
          test_df$label <- normalize_pattern_label(test_df$label, fill_blank_others = FALSE, ml_mode = input$ml_label_mode %||% "strict_high_confidence")
          ml_classes <- ml_label_levels(input$ml_label_mode %||% "strict_high_confidence")
          test_df <- test_df[test_df$label %in% ml_classes, , drop = FALSE]
          if (nrow(test_df) == 0) next
          pred <- tryCatch(predict_nnet_pattern_model(mdl, test_df, confidence_cutoff = 0), error = function(e) NULL)
          if (is.null(pred) || nrow(pred) == 0) next
          ev <- pred
          ev$truth <- test_df$label
          ev$dataset <- dtest$meta$display_name
          ev$heldout_id <- test_id
          ev$correct <- ev$pred_label == ev$truth
          all_eval[[length(all_eval) + 1L]] <- ev
          ce <- classification_eval(ev$pred_label, ev$truth, classes = ml_label_levels(input$ml_label_mode %||% "strict_high_confidence"), ml_mode = input$ml_label_mode %||% "strict_high_confidence")
          cm <- ce$confusion; cm$heldout_id <- test_id; cm$dataset <- dtest$meta$display_name
          met <- ce$metrics; met$heldout_id <- test_id; met$dataset <- dtest$meta$display_name; met$accuracy <- ce$accuracy; met$n <- ce$n
          all_cm[[length(all_cm) + 1L]] <- cm
          all_metrics[[length(all_metrics) + 1L]] <- met
        }
      })
      eval <- if (length(all_eval) > 0) bind_rows(all_eval) else data.frame()
      validate(need(nrow(eval) > 0, "No evaluable held-out predictions were produced. Check labels and selected pool."))
      ce_all <- classification_eval(eval$pred_label, eval$truth, classes = ml_label_levels(input$ml_label_mode %||% "strict_high_confidence"), ml_mode = input$ml_label_mode %||% "strict_high_confidence")
      rv$nn_eval <- list(
        mode = "leave_one_dataset_out",
        accuracy = ce_all$accuracy,
        n = ce_all$n,
        source = input$ml_eval_source %||% "manual",
        confusion = ce_all$confusion,
        metrics = ce_all$metrics,
        fold_metrics = if (length(all_metrics) > 0) bind_rows(all_metrics) else data.frame(),
        predictions = eval
      )
      ds$ml$last_eval_table <- ce_all$confusion
      ds$ml$last_eval_metrics <- ce_all$metrics
      set_dataset(rv$current_id, ds)
      showNotification(paste0("Leave-one-dataset-out evaluation finished: accuracy=", round(100 * ce_all$accuracy, 2), "% on ", ce_all$n, " labeled ISIs."), type = "message", duration = 6)
      return()
    }

    validate(need(!is.null(rv$nn_model), "Train or load a neural-network model first."))
    feat <- extract_ml_feature_table(ds$trains,
                                     source = input$ml_eval_source %||% "manual",
                                     auto_others = FALSE,
                                     fill_blank_others = FALSE,
                                     min_isi_sec = min_valid_isi_sec(),
                                     context_n = safe_int(input$ml_context_n, 3L),
                                    ml_mode = input$ml_label_mode %||% "strict_high_confidence")
    feat$label <- normalize_pattern_label(feat$label, fill_blank_others = FALSE, ml_mode = input$ml_label_mode %||% "strict_high_confidence")
    ml_classes <- ml_label_levels(input$ml_label_mode %||% "strict_high_confidence")
    feat <- feat[feat$label %in% ml_classes, , drop = FALSE]
    validate(need(nrow(feat) > 0, "No labeled ISIs available for evaluation under the selected label source."))
    pred <- tryCatch(
      predict_nnet_pattern_model(rv$nn_model, feat, confidence_cutoff = 0),
      error = function(e) {
        showNotification(paste0("NN evaluation failed: ", e$message), type = "error", duration = 8)
        NULL
      }
    )
    if (is.null(pred) || nrow(pred) == 0) return()
    eval <- pred
    eval$truth <- feat$label
    eval$correct <- eval$pred_label == eval$truth
    ce <- classification_eval(eval$pred_label, eval$truth, classes = ml_label_levels(input$ml_label_mode %||% "strict_high_confidence"), ml_mode = input$ml_label_mode %||% "strict_high_confidence")
    rv$nn_eval <- list(
      mode = "loaded_current",
      accuracy = ce$accuracy,
      n = ce$n,
      source = input$ml_eval_source %||% "manual",
      confusion = ce$confusion,
      metrics = ce$metrics,
      predictions = eval
    )
    ds$ml$last_eval_table <- ce$confusion
    ds$ml$last_eval_metrics <- ce$metrics
    set_dataset(rv$current_id, ds)
    showNotification(paste0("NN evaluation finished: accuracy=", round(100 * ce$accuracy, 2), "% on ", ce$n, " labeled ISIs."), type = "message", duration = 6)
  })
  
  output$nn_model_summary <- renderText({
    mdl <- rv$nn_model
    if (is.null(mdl)) return("No neural-network model trained or loaded.")
    counts <- mdl$training_counts
    counts_txt <- if (!is.null(counts) && nrow(counts) > 0) paste(paste0(counts$label, "=", counts$Freq), collapse = ", ") else "unknown"
    info <- rv$nn_training_info
    info_txt <- if (is.null(info)) "" else paste0(
      "\nTraining source: ", info$label_source,
      "\nTraining rows extracted: ", info$n_rows_extracted,
      "\nTraining datasets: ", paste(info$dataset_ids, collapse = ", ")
    )
    paste0(
      "Model type: ", mdl$model_type %||% "unknown", "\n",
      "Created: ", mdl$created_at %||% "", "\n",
      "Classes: ", paste(mdl$label_levels, collapse = ", "), "\n",
      "Feature count: ", length(mdl$feature_cols), "\n",
      "Training counts: ", counts_txt, "\n",
      info_txt, "\n",
      "Note: this is an optional ISI-window neural net. Keep manual/final labels as the ground truth reference."
    )
  })
  
  output$nn_eval_table <- renderDT({
    ev <- rv$nn_eval
    if (is.null(ev) || is.null(ev$confusion) || nrow(ev$confusion) == 0) {
      return(datatable(data.frame(message = "No NN evaluation yet."), options = list(dom = "t")))
    }
    overview <- data.frame(
      section = "overview",
      metric = c("mode", "label_source", "accuracy", "n"),
      class = "",
      value = c(ev$mode %||% "", ev$source %||% "", paste0(round(100 * ev$accuracy, 2), "%"), as.character(ev$n)),
      truth = "", pred_label = "", count = NA_integer_,
      stringsAsFactors = FALSE
    )
    metrics <- ev$metrics %||% data.frame()
    if (nrow(metrics) > 0) {
      metrics_show <- metrics %>%
        mutate(section = "per_class",
               metric = "precision/recall/F1",
               value = paste0("P=", round(precision, 3), "; R=", round(recall, 3), "; F1=", round(F1, 3), "; support=", support),
               truth = "", pred_label = "", count = NA_integer_) %>%
        select(section, metric, class, value, truth, pred_label, count)
    } else {
      metrics_show <- data.frame(section = character(), metric = character(), class = character(), value = character(), truth = character(), pred_label = character(), count = integer())
    }
    cm_show <- ev$confusion %>%
      mutate(section = "confusion", metric = "n", class = "", value = "", count = n) %>%
      select(section, metric, class, value, truth, pred_label, count)
    out <- bind_rows(overview, metrics_show, cm_show)
    datatable(out, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
  })
  
  output$nn_prediction_table <- renderDT({
    ds <- current_dataset()
    pred <- normalize_dataset(ds)$ml$last_prediction_table
    if (is.null(pred) || nrow(pred) == 0) return(datatable(data.frame(message = "No prediction table yet."), options = list(dom = "t")))
    datatable(head(pred, 1000), rownames = FALSE, options = list(pageLength = 20, scrollX = TRUE))
  })
  
  output$download_ml_features_csv <- downloadHandler(
    filename = function() paste0("spike_train_ml_features_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content = function(file) {
      df <- ml_feature_table_current()
      write_csv_safe(df, file, row.names = FALSE)
    }
  )
  
  output$download_nn_model <- downloadHandler(
    filename = function() paste0("spike_train_nnet_model_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"),
    content = function(file) {
      validate(need(!is.null(rv$nn_model), "\u5F53\u524D\u6CA1\u6709\u53EF\u4E0B\u8F7D\u7684\u795E\u7ECF\u7F51\u7EDC\u6A21\u578B\u3002\u8BF7\u5148\u8BAD\u7EC3\u6A21\u578B\u6216\u52A0\u8F7D .rds \u6A21\u578B\u3002"))
      saveRDS(rv$nn_model, file)
    }
  )
  
  output$download_nn_model_tab <- downloadHandler(
    filename = function() paste0("spike_train_nnet_model_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"),
    content = function(file) {
      validate(need(!is.null(rv$nn_model), "\u5F53\u524D\u6CA1\u6709\u53EF\u4E0B\u8F7D\u7684\u795E\u7ECF\u7F51\u7EDC\u6A21\u578B\u3002\u8BF7\u5148\u8BAD\u7EC3\u6A21\u578B\u6216\u52A0\u8F7D .rds \u6A21\u578B\u3002"))
      saveRDS(rv$nn_model, file)
    }
  )
  
  }, envir = server_env)
}

stpd_server_install_export_module <- function(server_env) {
  evalq({
  # ----------------------------------------------------------
  # Export functions
  # ----------------------------------------------------------
  output$download_labeled_csv <- downloadHandler(
    filename = function() {
      ds <- current_dataset()
      nm <- gsub("[^A-Za-z0-9_\\-\\.]", "_", ds$meta$display_name)
      paste0(nm, "_labeled_wide_", input$time_unit, ".csv")
    },
    content = function(file) {
      ds <- current_dataset()
      td <- ds$trains
      u <- input$time_unit
      max_len <- max(sapply(td, nrow))
      out_df <- NULL
      
      for (i in seq_along(td)) {
        tr <- names(td)[i]
        dat <- td[[tr]]
        n <- nrow(dat)
        final <- compute_final_pattern(dat$pattern_manual, dat$pattern_auto, dat$ISI_sec,
                                       auto_others = isTRUE(input$auto_others),
                                       min_isi_sec = min_valid_isi_sec())
        audit_final <- stpd_audit_final_labels(dat,
                                               min_isi_sec = min_valid_isi_sec(),
                                               auto_others = isTRUE(input$auto_others),
                                               prefer_stored = TRUE)
        score <- suppressWarnings(as.numeric(dat$auto_score %||% rep(NA_real_, n)))
        ts_col <- c(from_sec(dat$timestamp_sec, unit_out = u), rep(NA, max_len - n))
        isi_col <- c(from_sec(dat$ISI_sec, unit_out = u), rep(NA, max_len - n))
	        man_col <- c(dat$pattern_manual, rep("", max_len - n))
	        neg_col <- c((dat$pattern_manual_negative %||% rep("", n)), rep("", max_len - n))
	        aut_col <- c(dat$pattern_auto, rep("", max_len - n))
	        aut_orig_col <- c(stpd_chr_vec(dat$pattern_auto_original, n), rep("", max_len - n))
	        fin_col <- c(final, rep("", max_len - n))
	        audit_fin_col <- c(audit_final, rep("", max_len - n))
	        audit_from_col <- c(stpd_chr_vec(dat$pattern_audit_from, n), rep("", max_len - n))
	        audit_to_col <- c(stpd_chr_vec(dat$pattern_audit_to, n), rep("", max_len - n))
	        audit_action_col <- c(stpd_chr_vec(dat$pattern_audit_action, n), rep("", max_len - n))
	        audit_source_col <- c(stpd_chr_vec(dat$pattern_audit_source, n), rep("", max_len - n))
	        audit_reason_col <- c(stpd_chr_vec(dat$pattern_audit_reason, n), rep("", max_len - n))
	        audit_id_col <- c(stpd_chr_vec(dat$pattern_audit_id, n), rep("", max_len - n))
	        user_ov_col <- c(stpd_chr_vec(dat$pattern_user_override, n), rep("", max_len - n))
	        user_reason_col <- c(stpd_chr_vec(dat$pattern_user_override_reason, n), rep("", max_len - n))
	        user_source_col <- c(stpd_chr_vec(dat$pattern_user_override_source, n), rep("", max_len - n))
	        user_time_col <- c(stpd_chr_vec(dat$pattern_user_override_time, n), rep("", max_len - n))
	        user_id_col <- c(stpd_chr_vec(dat$pattern_user_override_id, n), rep("", max_len - n))
	        score_col <- c(score, rep(NA_real_, max_len - n))
	        
	        block <- data.frame(
	          setNames(list(ts_col), paste0(tr, "_timestamp")),
	          setNames(list(isi_col), paste0(tr, "_ISI")),
	          setNames(list(man_col), paste0(tr, "_pattern_manual")),
	          setNames(list(neg_col), paste0(tr, "_pattern_manual_negative")),
	          setNames(list(aut_col), paste0(tr, "_pattern_auto")),
	          setNames(list(aut_orig_col), paste0(tr, "_pattern_auto_original")),
	          setNames(list(fin_col), paste0(tr, "_pattern_final")),
	          setNames(list(audit_fin_col), paste0(tr, "_pattern_audit_final")),
	          setNames(list(audit_from_col), paste0(tr, "_pattern_audit_from")),
	          setNames(list(audit_to_col), paste0(tr, "_pattern_audit_to")),
	          setNames(list(audit_action_col), paste0(tr, "_pattern_audit_action")),
	          setNames(list(audit_source_col), paste0(tr, "_pattern_audit_source")),
	          setNames(list(audit_reason_col), paste0(tr, "_pattern_audit_reason")),
	          setNames(list(audit_id_col), paste0(tr, "_pattern_audit_id")),
	          setNames(list(user_ov_col), paste0(tr, "_pattern_user_override")),
	          setNames(list(user_reason_col), paste0(tr, "_pattern_user_override_reason")),
	          setNames(list(user_source_col), paste0(tr, "_pattern_user_override_source")),
	          setNames(list(user_time_col), paste0(tr, "_pattern_user_override_time")),
	          setNames(list(user_id_col), paste0(tr, "_pattern_user_override_id")),
	          setNames(list(score_col), paste0(tr, "_auto_score")),
	          stringsAsFactors = FALSE,
	          check.names = FALSE
	        )
	        if (is.null(out_df)) out_df <- block else {
	          sep_col <- data.frame(setNames(list(rep(NA, max_len)), paste0("sep_", i)), check.names = FALSE)
	          out_df <- cbind(out_df, sep_col, block)
	        }
	      }
	      out_df <- stpd_drop_empty_columns(out_df)
	      write_csv_safe(out_df, file, row.names = FALSE, fileEncoding = "UTF-8")
	    }
	  )
  
  widen_isi_columns <- function(events_df, unit_out = "ms") {
    if (nrow(events_df) == 0) return(events_df)
    f <- if (unit_out == "ms") 1000 else 1
    max_nisi <- max(lengths(events_df$isi_values_sec))
    if (max_nisi <= 0) return(events_df)
    isi_mat <- lapply(events_df$isi_values_sec, function(x) {
      xx <- as.numeric(x) * f
      c(xx, rep(NA_real_, max_nisi - length(xx)))
    })
    isi_mat <- do.call(rbind, isi_mat)
    colnames(isi_mat) <- paste0("ISI", seq_len(max_nisi))
    cbind(events_df %>% select(-isi_values_sec), as.data.frame(isi_mat))
  }
  
  export_event_csv <- function(ev, pattern_name, out_path, unit_out = "ms") {
    u <- unit_out
    df <- ev %>% filter(pattern == pattern_name) %>% arrange(train, start_time_sec)
    if (nrow(df) == 0) {
      write_csv_safe(data.frame(message = paste0("No ", pattern_name, " events.")), out_path, row.names = FALSE, fileEncoding = "UTF-8")
      return()
    }
    df <- df %>%
      group_by(dataset, train, pattern) %>%
      mutate(inter_event_interval_sec = lead(start_time_sec) - end_time_sec) %>%
      ungroup() %>%
      mutate(
        start_time = from_sec(start_time_sec, u),
        end_time = from_sec(end_time_sec, u),
        duration = from_sec(duration_sec, u),
        pre_ISI = from_sec(pre_ISI_sec, u),
        post_ISI = from_sec(post_ISI_sec, u),
        context_pre_ISI = from_sec(context_pre_ISI_sec, u),
        context_post_ISI = from_sec(context_post_ISI_sec, u),
        inter_event_interval = from_sec(inter_event_interval_sec, u),
        mean_ISI = from_sec(mean_ISI_sec, u),
        median_ISI = from_sec(median_ISI_sec, u),
        max_ISI = from_sec(max_ISI_sec, u),
        min_ISI = from_sec(min_ISI_sec, u),
        core_q_ISI = from_sec(core_q_ISI_sec, u)
      ) %>%
      select(
        `Spike train recording item name` = train,
        Pattern = pattern,
        `Spike number` = n_spikes,
        `Start time` = start_time,
        `End time` = end_time,
        Duration = duration,
        `Pre ISI` = pre_ISI,
        `Post ISI` = post_ISI,
        `Context pre ISI` = context_pre_ISI,
        `Context post ISI` = context_post_ISI,
        `Inter-event Interval` = inter_event_interval,
        `Max ISI` = max_ISI,
        `Min ISI` = min_ISI,
        `Median ISI` = median_ISI,
        `Mean ISI` = mean_ISI,
        `Core q ISI` = core_q_ISI,
        MM, LV, CV, Pre_LV, After_LV,
        `Immediate contrast min q` = contrast_min_q,
        `Immediate contrast geom q` = contrast_geom_q,
        `Immediate contrast pct q` = contrast_pct_q,
	        `Context contrast min q` = contrast_min_ctx_q,
	        `Context contrast geom q` = contrast_geom_ctx_q,
	        `Context contrast pct q` = contrast_pct_ctx_q,
	        `Label source` = label_source,
	        `User promoted possible_burst` = user_promoted_possible_burst,
	        `User promoted ISI count` = n_user_promoted_isi,
	        `Auto pattern majority` = auto_pattern_majority,
	        `User override reason` = user_override_reason,
	        `Auto score` = auto_score,
	        isi_values_sec
	      )
    df <- widen_isi_columns(df, unit_out = u)
    write_csv_safe(df, out_path, row.names = FALSE, fileEncoding = "UTF-8")
  }
  
  output$download_results_zip <- downloadHandler(
    filename = function() {
      ds <- current_dataset()
      nm <- gsub("[^A-Za-z0-9_\\-\\.]", "_", ds$meta$display_name)
      paste0(nm, "_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
    },
    content = function(file) {
      ds <- current_dataset()
      td <- ds$trains
      u <- input$time_unit
      p <- current_param_for_tables()
      bundle <- derive_interval_tables(
        td,
        source = "audit_final",
        auto_others = isTRUE(input$auto_others),
        dataset_map = setNames(rep(ds$meta$display_name, length(td)), names(td)),
        min_isi_sec = min_valid_isi_sec(),
        contrast_q = p$burst$contrast_q %||% 0.90,
        context_k = p$burst$context_k %||% 5L
      )
      ev <- bundle$events
      # pause event exports include local/global threshold context when available.
      run_id_export <- (ds$results$run_metadata$run_id %||% "export_run")[1]
      phash_export <- (ds$results$run_metadata$params_hash %||% compute_params_hash(p))[1]
      ev <- enrich_events_with_pause_thresholds(ev, td, run_id = run_id_export, params_hash = phash_export)
      lab <- bundle$labels
      
      out_dir <- file.path(tempdir(), paste0("spike_detector_export_", format(Sys.time(), "%Y%m%d_%H%M%S")))
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
      
      export_event_csv(ev, "burst", file.path(out_dir, "Burst_events.csv"), unit_out = u)
      export_event_csv(ev, "long_burst", file.path(out_dir, "Long_burst_events.csv"), unit_out = u)
      export_event_csv(ev, "possible_burst", file.path(out_dir, "Possible_burst_events.csv"), unit_out = u)
      export_event_csv(ev, "tonic", file.path(out_dir, "Tonic_events.csv"), unit_out = u)
      export_event_csv(ev, "high_frequency_tonic", file.path(out_dir, "High_frequency_tonic_events.csv"), unit_out = u)
      export_event_csv(ev, "high_frequency_spiking", file.path(out_dir, "High_frequency_spiking_events.csv"), unit_out = u)
      export_event_csv(ev, "pause", file.path(out_dir, "Pause_events.csv"), unit_out = u)
      
      labels_out <- lab %>%
        group_by(dataset, train) %>%
        arrange(idx, .by_group = TRUE) %>%
        mutate(spike_i_time_sec = dplyr::lag(timestamp_sec)) %>%
        ungroup() %>%
        mutate(
          `Spike train recording item name` = train,
          `ISI index` = idx,
          `Spike i time` = from_sec(spike_i_time_sec, u),
          `Spike i+1 time` = from_sec(timestamp_sec, u),
          ISI = from_sec(ISI_sec, u),
          is_artifact = is_artifact,
	          manual_label = manual_label,
	          manual_negative_label = manual_negative_label,
		          auto_label = auto_label,
		          auto_label_original = auto_label_original,
		          final_label = final_label,
		          audit_final_label = audit_final_label,
		          audit_base_final_label = audit_base_final_label,
		          audit_from_label = audit_from_label,
		          audit_to_label = audit_to_label,
		          audit_action = audit_action,
		          audit_source = audit_source,
		          audit_reason = audit_reason,
		          audit_id = audit_id,
		          audit_time = audit_time,
		          user_override_label = user_override_label,
		          user_override_from = user_override_from,
		          user_override_to = user_override_to,
	          user_override_reason = user_override_reason,
	          user_override_source = user_override_source,
	          user_override_time = user_override_time,
	          user_override_id = user_override_id,
	          auto_score = auto_score
	        ) %>%
		        select(`Spike train recording item name`, `ISI index`, `Spike i time`, `Spike i+1 time`, ISI,
		               is_artifact, manual_label, manual_negative_label, auto_label, auto_label_original,
		               final_label, audit_final_label, audit_base_final_label, audit_from_label,
		               audit_to_label, audit_action, audit_source, audit_reason, audit_id, audit_time,
		               user_override_label, user_override_from, user_override_to,
		               user_override_reason, user_override_source, user_override_time, user_override_id,
		               auto_score)
		      labels_out <- stpd_drop_empty_columns(labels_out)
		      write_csv_safe(labels_out, file.path(out_dir, "ISI_labels.csv"), row.names = FALSE, fileEncoding = "UTF-8")
		      if (!is.null(ds$results$final_audit_summary) && nrow(ds$results$final_audit_summary) > 0) {
		        write_csv_safe(ds$results$final_audit_summary, file.path(out_dir, "Final_audit_summary.csv"), row.names = FALSE, fileEncoding = "UTF-8")
		      }
		      if (!is.null(ds$results$final_audit_events) && nrow(ds$results$final_audit_events) > 0) {
		        write_csv_safe(ds$results$final_audit_events, file.path(out_dir, "Final_audit_events.csv"), row.names = FALSE, fileEncoding = "UTF-8")
		      }
		      if (!is.null(ds$results$final_audit_history) && nrow(ds$results$final_audit_history) > 0) {
		        write_csv_safe(ds$results$final_audit_history, file.path(out_dir, "Final_audit_history.csv"), row.names = FALSE, fileEncoding = "UTF-8")
		      }
		      if (!is.null(ds$results$final_audit_event_history) && nrow(ds$results$final_audit_event_history) > 0) {
		        write_csv_safe(ds$results$final_audit_event_history, file.path(out_dir, "Final_audit_event_history.csv"), row.names = FALSE, fileEncoding = "UTF-8")
		      }
		      task_events_out <- stpd_normalize_task_events(ds$task_events %||% data.frame(), source = ds$meta$display_name %||% "")
		      if (nrow(task_events_out) > 0L) {
		        write_csv_safe(task_events_out, file.path(out_dir, "Task_events.csv"), row.names = FALSE, fileEncoding = "UTF-8")
		      }
	      if (!is.null(ds$results$possible_burst_promotion_audit) && nrow(ds$results$possible_burst_promotion_audit) > 0) {
	        write_csv_safe(ds$results$possible_burst_promotion_audit, file.path(out_dir, "Possible_burst_promotion_audit.csv"), row.names = FALSE, fileEncoding = "UTF-8")
	      }
	      if (!is.null(ds$results$possible_burst_promotion_summary) && nrow(ds$results$possible_burst_promotion_summary) > 0) {
	        write_csv_safe(ds$results$possible_burst_promotion_summary, file.path(out_dir, "Possible_burst_promotion_summary.csv"), row.names = FALSE, fileEncoding = "UTF-8")
	      }
      
      # structure structure-seed-bridge diagnostic exports. These tables are intended for parameter tuning:
      # Structure_candidates.csv supports Pre-Core-Post structure review; Seed_candidates.csv supports seed ISI / edge contrast review;
      # Bridge_candidates.csv supports bridge-ISI and bridge/seed-ratio review; Burst_candidates_structure.csv records
      # final seed-component candidates before/after acceptance; Near_miss_candidates.csv records threshold-preview rows.
      if (!is.null(ds$results$structure_candidates) && nrow(ds$results$structure_candidates) > 0) {
        write_csv_safe(ds$results$structure_candidates, file.path(out_dir, "Structure_candidates.csv"), row.names = FALSE, fileEncoding = "UTF-8")
      }
      if (!is.null(ds$results$seed_candidates) && nrow(ds$results$seed_candidates) > 0) {
        write_csv_safe(ds$results$seed_candidates, file.path(out_dir, "Seed_candidates.csv"), row.names = FALSE, fileEncoding = "UTF-8")
      }
      if (!is.null(ds$results$bridge_candidates) && nrow(ds$results$bridge_candidates) > 0) {
        write_csv_safe(ds$results$bridge_candidates, file.path(out_dir, "Bridge_candidates.csv"), row.names = FALSE, fileEncoding = "UTF-8")
      }
      if (!is.null(ds$results$burst_candidates) && nrow(ds$results$burst_candidates) > 0) {
        write_csv_safe(ds$results$burst_candidates, file.path(out_dir, "Burst_candidates_structure.csv"), row.names = FALSE, fileEncoding = "UTF-8")
      }
      if (!is.null(ds$results$burst_candidates_raw) && nrow(ds$results$burst_candidates_raw) > 0) {
        write_csv_safe(ds$results$burst_candidates_raw, file.path(out_dir, "Burst_candidates_raw.csv"), row.names = FALSE, fileEncoding = "UTF-8")
      }
      if (!is.null(ds$results$burst_candidates_final) && nrow(ds$results$burst_candidates_final) > 0) {
        write_csv_safe(ds$results$burst_candidates_final, file.path(out_dir, "Burst_candidates_final.csv"), row.names = FALSE, fileEncoding = "UTF-8")
      }
      if (!is.null(ds$results$pause_candidates) && nrow(ds$results$pause_candidates) > 0) {
        write_csv_safe(ds$results$pause_candidates, file.path(out_dir, "Pause_candidates_with_thresholds.csv"), row.names = FALSE, fileEncoding = "UTF-8")
      }
      if (!is.null(ds$results$near_miss_candidates) && nrow(ds$results$near_miss_candidates) > 0) {
        write_csv_safe(ds$results$near_miss_candidates, file.path(out_dir, "Near_miss_candidates.csv"), row.names = FALSE, fileEncoding = "UTF-8")
      }
      ds_norm <- normalize_dataset(ds)
      # tiered result exports. These files separate high-confidence events, review candidates,
      # burst-family candidate metrics, and the full candidate ledger with demotion/rejection reasons.
      write_tiered_result_exports(ds_norm, p, out_dir)
      # explicit audit artifacts for reproducibility and biological interpretation.
      if (!is.null(ds_norm$results$candidate_features) && nrow(ds_norm$results$candidate_features) > 0) {
        write_csv_safe(ds_norm$results$candidate_features, file.path(out_dir, "Candidate_features_audit.csv"), row.names = FALSE, fileEncoding = "UTF-8")
      }
      if (!is.null(ds_norm$results$event_distribution_evidence) && nrow(ds_norm$results$event_distribution_evidence) > 0) {
        write_csv_safe(ds_norm$results$event_distribution_evidence, file.path(out_dir, "Event_distribution_evidence.csv"), row.names = FALSE, fileEncoding = "UTF-8")
      }
      if (!is.null(ds_norm$results$train_distribution_features) && nrow(ds_norm$results$train_distribution_features) > 0) {
        write_csv_safe(ds_norm$results$train_distribution_features, file.path(out_dir, "Train_distribution_features.csv"), row.names = FALSE, fileEncoding = "UTF-8")
      }
      if (!is.null(ds_norm$results$spike_count_pmf) && nrow(ds_norm$results$spike_count_pmf) > 0) {
        write_csv_safe(ds_norm$results$spike_count_pmf, file.path(out_dir, "Spike_count_PMF.csv"), row.names = FALSE, fileEncoding = "UTF-8")
      }
      rep_export <- ds_norm$results$consistency_audit %||% ds_norm$results$semantic_consistency_report %||% data.frame()
      if (!is.null(rep_export) && nrow(rep_export) > 0) {
        write_csv_safe(rep_export, file.path(out_dir, "Semantic_consistency_report.csv"), row.names = FALSE, fileEncoding = "UTF-8")
      }
      writeLines(stpd_methodological_warning(as_vector = TRUE), file.path(out_dir, "Methodological_warnings.txt"), useBytes = TRUE)
      write_csv_safe(preset_catalog(), file.path(out_dir, "Preset_catalog.csv"), row.names = FALSE, fileEncoding = "UTF-8")
      if (!is.null(ds_norm$results$run_metadata) && nrow(ds_norm$results$run_metadata) > 0) {
        write_csv_safe(ds_norm$results$run_metadata, file.path(out_dir, "Detector_run_metadata.csv"), row.names = FALSE, fileEncoding = "UTF-8")
      }
      qc_export <- validate_dataset_quality_impl(ds_norm$trains, min_isi_sec = min_valid_isi_sec(), unit_hint = ds_norm$meta$unit_in %||% "s", refractory_suspect_sec = refractory_suspect_sec(), display_unit = qc_isi_unit())
      if (!is.null(qc_export) && nrow(qc_export) > 0) {
        write_csv_safe(qc_export, file.path(out_dir, "Data_quality_QC.csv"), row.names = FALSE, fileEncoding = "UTF-8")
      }
      dup_details <- tryCatch(duplicate_timestamp_details(ds_norm$trains, display_unit = qc_isi_unit()), error = function(e) data.frame())
      if (!is.null(dup_details) && nrow(dup_details) > 0) {
        write_csv_safe(dup_details, file.path(out_dir, "Duplicate_timestamp_details.csv"), row.names = FALSE, fileEncoding = "UTF-8")
      }
      art_details <- tryCatch(artifact_isi_details(ds_norm$trains, min_isi_sec = min_valid_isi_sec(), display_unit = qc_isi_unit()), error = function(e) data.frame())
      if (!is.null(art_details) && nrow(art_details) > 0) {
        write_csv_safe(art_details, file.path(out_dir, "Artifact_ISI_details.csv"), row.names = FALSE, fileEncoding = "UTF-8")
      }
      rr <- ds_norm$train_settings$burst_isi_ranges %||% list()
      if (length(rr) > 0) {
        range_rows <- bind_rows(lapply(names(rr), function(tr) {
          x <- rr[[tr]]
          data.frame(
            train = tr,
            low_pct = range_value(x, "low_pct", NA_real_),
            high_pct = range_value(x, "high_pct", NA_real_),
            low_sec = range_value(x, "low_sec", NA_real_),
            high_sec = range_value(x, "high_sec", NA_real_),
            n_valid_isi = range_value(x, "n_valid_isi", NA_real_),
            n_manual_burst_isi = range_value(x, "n_manual_burst_isi", NA_real_),
            range_mode = as.character(x$range_mode %||% ""),
            abs_low_override = isTRUE(x$abs_low_override %||% FALSE),
            abs_high_override = isTRUE(x$abs_high_override %||% FALSE),
            source = as.character(x$source %||% ""),
            method = as.character(x$method %||% ""),
            updated_at = as.character(x$updated_at %||% ""),
            stringsAsFactors = FALSE
          )
        }))
        write_csv_safe(range_rows, file.path(out_dir, "Train_burst_ISI_ranges.csv"), row.names = FALSE, fileEncoding = "UTF-8")
      }
      for (kind in c("tonic", "pause", "highfreq")) {
        rr2 <- switch(kind,
                      tonic = ds_norm$train_settings$tonic_isi_ranges,
                      pause = ds_norm$train_settings$pause_isi_ranges,
                      highfreq = ds_norm$train_settings$highfreq_isi_ranges)
        rr2 <- rr2 %||% list()
        if (length(rr2) > 0) {
          range_rows2 <- bind_rows(lapply(names(rr2), function(tr) {
            x <- rr2[[tr]]
            data.frame(
              train = tr,
              low_pct = range_value(x, "low_pct", NA_real_),
              high_pct = range_value(x, "high_pct", NA_real_),
              low_sec = range_value(x, "low_sec", NA_real_),
              high_sec = range_value(x, "high_sec", NA_real_),
              n_valid_isi = range_value(x, "n_valid_isi", NA_real_),
              n_manual_label_isi = switch(kind,
                                           tonic = range_value(x, "n_manual_tonic_isi", NA_real_),
                                           pause = range_value(x, "n_manual_pause_isi", NA_real_),
                                           highfreq = range_value(x, "n_manual_highfreq_isi", NA_real_)),
              anchor_center_sec = range_value(x, "anchor_center_sec", NA_real_),
              anchor_spread_log = range_value(x, "anchor_spread_log", NA_real_),
              anchor_confidence = range_value(x, "anchor_confidence", NA_real_),
              anchor_n = range_value(x, "anchor_n", NA_real_),
              learned_LV_q95 = range_value(x, "learned_LV_q95", NA_real_),
              learned_CV_q95 = range_value(x, "learned_CV_q95", NA_real_),
              learned_MM_q95 = range_value(x, "learned_MM_q95", NA_real_),
              source = as.character(x$source %||% ""),
              method = as.character(x$method %||% ""),
              updated_at = as.character(x$updated_at %||% ""),
              stringsAsFactors = FALSE
            )
          }))
          suffix <- if (identical(kind, "highfreq")) "ISI_anchors" else "ISI_ranges"
          write_csv_safe(range_rows2, file.path(out_dir, paste0("Train_", kind, "_", suffix, ".csv")), row.names = FALSE, fileEncoding = "UTF-8")
        }
      }
      thr_export <- train_isi_threshold_dataframe(ds_norm$train_settings$isi_thresholds %||% list(), factor = if (identical(u, "ms")) 1000 else 1, unit = u)
      if (!is.null(thr_export) && nrow(thr_export) > 0) {
        write_csv_safe(thr_export, file.path(out_dir, "Train_specific_ISI_thresholds.csv"), row.names = FALSE, fileEncoding = "UTF-8")
      }
      if (!is.null(ds_norm$ml$last_feature_table) && nrow(ds_norm$ml$last_feature_table) > 0) {
        write_csv_safe(ds_norm$ml$last_feature_table, file.path(out_dir, "ML_feature_table.csv"), row.names = FALSE, fileEncoding = "UTF-8")
      }
      if (!is.null(ds_norm$ml$last_prediction_table) && nrow(ds_norm$ml$last_prediction_table) > 0) {
        write_csv_safe(ds_norm$ml$last_prediction_table, file.path(out_dir, "NN_prediction_table.csv"), row.names = FALSE, fileEncoding = "UTF-8")
      }
      if (!is.null(ds_norm$ml$last_eval_table) && nrow(ds_norm$ml$last_eval_table) > 0) {
        write_csv_safe(ds_norm$ml$last_eval_table, file.path(out_dir, "NN_evaluation_confusion.csv"), row.names = FALSE, fileEncoding = "UTF-8")
      }
      if (!is.null(ds_norm$ml$last_eval_metrics) && nrow(ds_norm$ml$last_eval_metrics) > 0) {
        write_csv_safe(ds_norm$ml$last_eval_metrics, file.path(out_dir, "NN_evaluation_metrics.csv"), row.names = FALSE, fileEncoding = "UTF-8")
      }
      # Recompute Manual-vs-detector evaluation for the dataset being exported.
      # This avoids exporting a stale report generated for a different \u5F53\u524D\u6570\u636E\u96C6.
      md <- evaluate_detector_against_manual(ds_norm, p, selected_trains = names(ds_norm$trains), min_isi_sec = p$detector$min_valid_isi_sec %||% min_valid_isi_sec(), metric_mode = "strict_high_confidence")
      if (!is.null(md$meta) && nrow(md$meta) > 0) {
        write_csv_safe(md$meta, file.path(out_dir, "Manual_vs_detector_meta.csv"), row.names = FALSE, fileEncoding = "UTF-8")
      }
      if (!is.null(md$metrics) && nrow(md$metrics) > 0) {
        write_csv_safe(md$metrics, file.path(out_dir, "Manual_vs_detector_metrics.csv"), row.names = FALSE, fileEncoding = "UTF-8")
      }
      if (!is.null(md$confusion) && nrow(md$confusion) > 0) {
        write_csv_safe(md$confusion, file.path(out_dir, "Manual_vs_detector_confusion.csv"), row.names = FALSE, fileEncoding = "UTF-8")
      }
      if (!is.null(md$events) && nrow(md$events) > 0) {
        write_csv_safe(md$events, file.path(out_dir, "Manual_vs_detector_events.csv"), row.names = FALSE, fileEncoding = "UTF-8")
      }
      
      params_out <- capture.output(str(p))
      writeLines(params_out, file.path(out_dir, "Detector_params.txt"))
      
      old <- setwd(out_dir); on.exit(setwd(old), add = TRUE)
      utils::zip(zipfile = file, files = list.files(out_dir))
    }
  )


  }, envir = server_env)
}

stpd_server_install_visualization_module <- function(server_env) {
  evalq({
  stpd_empty_plotly_message <- function(text, source = NULL, events = character(0)) {
    p <- plot_ly(x = numeric(0), y = numeric(0), type = "scatter", mode = "markers", source = source) %>%
      layout(
        xaxis = list(visible = FALSE, zeroline = FALSE, showgrid = FALSE),
        yaxis = list(visible = FALSE, zeroline = FALSE, showgrid = FALSE),
        annotations = list(list(
          x = 0.02,
          y = 0.98,
          xref = "paper",
          yref = "paper",
          text = text,
          showarrow = FALSE,
          xanchor = "left",
          yanchor = "top",
          font = list(size = 14, color = "#64748b")
        )),
        margin = list(l = 24, r = 24, t = 24, b = 24)
      )
    for (ev in events) p <- event_register(p, ev)
    config(p, displaylogo = FALSE)
  }

  stpd_safe_plotly_event_data <- function(event, source) {
    tryCatch(
      suppressWarnings(event_data(event, source = source)),
      warning = function(w) NULL,
      error = function(e) NULL
    )
  }

  normalize_xrange_window <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    if (length(x) != 2L || any(!is.finite(x)) || x[2] <= x[1]) return(NULL)
    sort(x)
  }

  raster_default_xrange_window <- function() {
    max_plot <- suppressWarnings(as.numeric(rv$xrange_max_plot %||% 1000)[1])
    if (!is.finite(max_plot) || max_plot <= 0) max_plot <- 1000
    default_width <- if (identical(input$time_unit, "ms")) 1000 else 1
    c(0, min(max_plot, default_width))
  }

  raster_prefetch_fraction <- function() 0.35

  raster_slider_window_source <- reactive({
    x <- normalize_xrange_window(input$xrange)
    if (is.null(x)) x <- normalize_xrange_window(input$xrange_plot)
    x
  })

  raster_render_window <- shiny::debounce(raster_slider_window_source, millis = 180)

  raster_window_for_plot <- function(debounced = TRUE, prefer_view = TRUE) {
    x <- if (isTRUE(prefer_view)) rv$view_align_x else NULL
    if (is.null(normalize_xrange_window(x)) && isTRUE(debounced)) {
      x <- tryCatch(raster_render_window(), error = function(e) NULL)
    }
    if (is.null(normalize_xrange_window(x))) x <- isolate(input$xrange)
    if (is.null(normalize_xrange_window(x))) x <- isolate(input$xrange_plot)
    x <- normalize_xrange_window(x)
    if (is.null(x)) return(raster_default_xrange_window())
    x
  }

  relayout_raster_xaxis <- function(x) {
    if (length(rv$datasets) == 0L) return(invisible(NULL))
    x <- normalize_xrange_window(x)
    if (is.null(x)) return(invisible(NULL))
    payload <- list(x[1], x[2])
    names(payload) <- c("xaxis.range[0]", "xaxis.range[1]")
    try({
      proxy <- plotlyProxy("raster_plot", session)
      plotlyProxyInvoke(proxy, "relayout", payload)
    }, silent = TRUE)
    invisible(NULL)
  }

  plot_render_progress <- function(output_id = "raster_plot", value = 0, detail = "",
                                   message = NULL, type = "active") {
    value <- suppressWarnings(as.numeric(value)[1])
    if (!is.finite(value)) value <- 0
    value <- max(0, min(1, value))
    detail <- as.character(detail %||% "")[1]
    message <- as.character(message %||% "\u6B63\u5728\u751F\u6210 plot \u89C6\u56FE")[1]
    type <- as.character(type %||% "active")[1]
    session$sendCustomMessage(
      "stpdPlotRenderProgress",
      list(outputId = output_id, type = type, value = value, message = message, detail = detail)
    )
    invisible(NULL)
  }

  suppress_raster_plot_progress <- function() {
    if (!isTRUE(rv$data_load_active)) {
      rv$raster_plot_progress_active <- FALSE
      plot_render_progress("raster_plot", value = 0, detail = "", message = "", type = "hide")
    }
    invisible(NULL)
  }

  output$raster_plot_shell <- renderUI({
    progress_value <- suppressWarnings(as.numeric(rv$data_load_progress_value %||% 0))
    if (!is.finite(progress_value)) progress_value <- 0
    progress_value <- max(0, min(1, progress_value))
    progress_type <- as.character(rv$data_load_progress_type %||% "idle")[1]
    progress_active <- isTRUE(rv$data_load_active) || isTRUE(rv$raster_plot_progress_active) || progress_type %in% c("active", "error")
    progress_class <- paste(
      "plot-render-progress",
      if (progress_active) {
        if (identical(progress_type, "error")) "is-error" else "is-active"
      } else {
        "is-idle"
      }
    )
    progress_message <- as.character(rv$data_load_progress_message %||% "\u6B63\u5728\u51C6\u5907 plot \u89C6\u56FE")[1]
    if (!nzchar(progress_message)) progress_message <- "\u6B63\u5728\u51C6\u5907 plot \u89C6\u56FE"
    progress_detail <- as.character(rv$data_load_progress_detail %||% "\u7B49\u5F85\u6570\u636E\u96C6\u5BFC\u5165\u540E\u751F\u6210 plot \u89C6\u56FE\u3002")[1]
    div(
      class = paste("plot-output-wrap", if (length(rv$datasets) == 0L) "plot-output-wrap-empty" else ""),
      plotlyOutput("raster_plot", height = "68vh", width = "100%"),
      if (length(rv$datasets) == 0L) {
        div(
          class = "plot-output-empty",
          div(class = "plot-output-empty-title", "\u8BF7\u81F3\u5C11\u4E0A\u4F20\u4E00\u4E2A\u6570\u636E\u96C6\u3002"),
          div("\u4E0A\u4F20\u5B8C\u6210\u540E\uFF0C\u53F3\u4FA7\u4F1A\u663E\u793A plot \u89C6\u56FE\u751F\u6210\u8FDB\u5EA6\u3002")
        )
      },
      div(
        id = "raster_plot_progress",
        class = progress_class,
        div(
          class = "plot-render-progress-head",
          tags$span(class = "plot-render-progress-title", progress_message),
          tags$span(class = "plot-render-progress-percent", paste0(round(progress_value * 100), "%"))
        ),
        div(class = "plot-render-progress-track", div(class = "plot-render-progress-bar", style = paste0("width:", round(progress_value * 100), "%;"))),
        div(class = "plot-render-progress-detail", progress_detail)
      )
    )
  })

  observeEvent(list(rv$current_id, input$time_unit), {
    refresh_xrange_slider(reset = FALSE)
  }, ignoreInit = FALSE)

  same_xrange_window <- function(a, b) {
    if (is.null(a) || is.null(b) || length(a) != 2 || length(b) != 2) return(FALSE)
    a <- suppressWarnings(as.numeric(a))
    b <- suppressWarnings(as.numeric(b))
    all(is.finite(a)) && all(is.finite(b)) && max(abs(a - b)) < 1e-9
  }

  observeEvent(input$xrange, {
    # When the slider is moved, prefer the slider window over any previous
    # Plotly pan/zoom window stored in rv$view_align_x.
    if (isTRUE(rv$syncing_xrange)) return()
    suppress_raster_plot_progress()
    rv$view_align_x <- NULL
    rv$syncing_xrange <- TRUE
    if (!same_xrange_window(isolate(input$xrange_plot), input$xrange)) {
      update_xrange_slider_input("xrange_plot", value = input$xrange)
    }
    sync_xrange_length_inputs(input$xrange)
    relayout_raster_xaxis(input$xrange)
    session$onFlushed(function() rv$syncing_xrange <- FALSE, once = TRUE)
  }, ignoreInit = TRUE, priority = 1000)

  observeEvent(input$xrange_plot, {
    if (isTRUE(rv$syncing_xrange)) return()
    suppress_raster_plot_progress()
    rv$view_align_x <- NULL
    rv$syncing_xrange <- TRUE
    if (!same_xrange_window(isolate(input$xrange), input$xrange_plot)) {
      update_xrange_slider_input("xrange", value = input$xrange_plot)
    }
    sync_xrange_length_inputs(input$xrange_plot)
    relayout_raster_xaxis(input$xrange_plot)
    session$onFlushed(function() rv$syncing_xrange <- FALSE, once = TRUE)
  }, ignoreInit = TRUE, priority = 1000)

  observeEvent(input$xrange_window_length, {
    if (isTRUE(rv$syncing_xrange)) return()
    suppress_raster_plot_progress()
    current_width <- xrange_window_width(current_xrange_window(prefer_view = TRUE))
    if (same_xrange_numeric(current_width, input$xrange_window_length)) return()
    relayout_raster_xaxis(apply_xrange_window_length(input$xrange_window_length))
  }, ignoreInit = TRUE, priority = 1000)

  observeEvent(input$xrange_plot_window_length, {
    if (isTRUE(rv$syncing_xrange)) return()
    suppress_raster_plot_progress()
    current_width <- xrange_window_width(current_xrange_window(prefer_view = TRUE))
    if (same_xrange_numeric(current_width, input$xrange_plot_window_length)) return()
    relayout_raster_xaxis(apply_xrange_window_length(input$xrange_plot_window_length))
  }, ignoreInit = TRUE, priority = 1000)
  
	  observeEvent({
	    if (length(rv$datasets) == 0L) {
	      NULL
	    } else if (!identical(input$main_tabs %||% "", "\u5BF9\u9F50\u65F6\u95F4\u6233\u56FE")) {
	      NULL
	    } else {
	      stpd_safe_plotly_event_data("plotly_relayout", source = "raster")
	    }
	  }, {
	    r <- stpd_safe_plotly_event_data("plotly_relayout", source = "raster")
	    if (is.null(r)) return()
	    if (!is.null(r[["xaxis.autorange"]]) && isTRUE(r[["xaxis.autorange"]])) {
	      rv$view_align_x <- NULL
	      return()
	    }
	    if (!is.null(r[["xaxis.range[0]"]]) && !is.null(r[["xaxis.range[1]"]])) {
	      # Keep the app-level time window owned by the Shiny sliders/numeric
	      # inputs. Plotly can emit relayout events during tab switches, widget
	      # resize, or redraws from other Plotly panels (notably state trajectory
	      # with small bins). Writing those events back into rv$view_align_x makes
	      # the visible raster window jump while the user is dragging the window.
	      return()
	    }
	  }, ignoreInit = TRUE)
  
  # ----------------------------------------------------------
  # Plotting data
  # ----------------------------------------------------------
  raster_draw_window_sec <- function(with_padding = TRUE) {
    f <- unit_factor()
    x_use <- raster_window_for_plot(debounced = TRUE, prefer_view = TRUE)
    draw_sec <- sort(suppressWarnings(as.numeric(x_use))) / f
    pad_sec <- if (isTRUE(with_padding)) max(0.002, diff(draw_sec) * raster_prefetch_fraction()) else 0
    c(max(0, draw_sec[1] - pad_sec), draw_sec[2] + pad_sec)
  }

  selected_axis_table <- reactive({
    selected <- displayed_train_names()
    validate(need(length(selected) > 0, "\u8BF7\u81F3\u5C11\u9009\u62E9\u4E00\u6761 train\u3002"))
    step <- track_step()
    y_index <- rev(seq_along(selected))
    data.frame(
      train = selected,
      train_label = selected,
      train_order = seq_along(selected),
      y = 1 + (y_index - 1) * step,
      stringsAsFactors = FALSE
    )
  })

  aligned_data <- reactive({
    ds <- current_dataset()
    td <- ds$trains
    ledger <- ds$results$candidate_ledger %||% data.frame()
    selected <- displayed_train_names()
    validate(need(length(selected) > 0, "\u8BF7\u81F3\u5C11\u9009\u62E9\u4E00\u6761 train\u3002"))
    step <- track_step()
    
    y_index <- rev(seq_along(selected))
    y_pos <- 1 + (y_index - 1) * step
    
    out <- list()
    for (i in seq_along(selected)) {
      tr <- selected[i]
      dat <- ensure_train_isi_percentiles(td[[tr]], min_valid_isi_sec())
      if (nrow(dat) == 0) next
      t_align <- dat$timestamp_sec - dat$timestamp_sec[1]
      pat_final <- compute_final_pattern(dat$pattern_manual, dat$pattern_auto, dat$ISI_sec,
                                         auto_others = isTRUE(input$auto_others),
                                         min_isi_sec = min_valid_isi_sec())
      pat_audit_final <- stpd_audit_final_labels(dat,
                                                 min_isi_sec = min_valid_isi_sec(),
                                                 auto_others = isTRUE(input$auto_others),
                                                 prefer_stored = TRUE)
      pat_show <- switch(input$pattern_view,
                         manual = dat$pattern_manual,
                         auto = dat$pattern_auto,
                         final = pat_final,
                         audit_final = pat_audit_final)
      label_source <- ifelse(as.character(dat$pattern_manual) != "", "manual", ifelse(as.character(dat$pattern_auto) != "", "auto", ifelse(as.character(pat_final) != "", "implicit_final", "none")))
      pb_subtype <- rep("", nrow(dat)); pb_candidate_id <- rep("", nrow(dat)); pb_reason <- rep("", nrow(dat))
      if (!is.null(ledger) && nrow(ledger) > 0 && all(c("train", "start_isi", "end_isi") %in% names(ledger))) {
        lr <- ledger[as.character(ledger$train) == as.character(tr), , drop = FALSE]
        if (nrow(lr) > 0) {
          for (jj in seq_len(nrow(lr))) {
            s0 <- suppressWarnings(as.integer(lr$start_isi[jj])); e0 <- suppressWarnings(as.integer(lr$end_isi[jj]))
            if (!is.finite(s0) || !is.finite(e0) || e0 < s0) next
            idx0 <- seq(max(2L, s0), min(nrow(dat), e0))
            if (length(idx0) == 0) next
            fc <- as.character(lr$final_candidate_class[jj] %||% "")
            rc <- as.character(lr$raw_candidate_class[jj] %||% "")
            if (fc == "possible_burst" || rc == "possible_burst" || grepl("possible", as.character(lr$uncertainty_reason[jj] %||% ""))) {
              pb_subtype[idx0] <- as.character(lr$possible_burst_subtype[jj] %||% "")
              pb_candidate_id[idx0] <- as.character(lr$candidate_id[jj] %||% "")
              pb_reason[idx0] <- as.character(lr$uncertainty_reason[jj] %||% "")
            }
          }
        }
      }
      out[[i]] <- data.frame(
        train = tr,
        train_label = tr,
        train_order = i,
        y = y_pos[i],
        idx = dat$idx,
        time_align_sec = t_align,
        timestamp_sec = dat$timestamp_sec,
        ISI_sec = dat$ISI_sec,
        ISI_pct = dat$ISI_pct,
        ISI_range_pct_linear = dat$ISI_range_pct_linear %||% rep(NA_real_, nrow(dat)),
        ISI_range_pct_log = dat$ISI_range_pct_log %||% rep(NA_real_, nrow(dat)),
        ISI_robust_range_pct_log = dat$ISI_robust_range_pct_log %||% rep(NA_real_, nrow(dat)),
        ISI_rank_n = dat$ISI_rank_n,
        pattern_manual = dat$pattern_manual,
        pattern_auto = dat$pattern_auto,
        pattern_final = pat_final,
        pattern_audit_final = pat_audit_final,
        pattern_show = pat_show,
        label_source = label_source,
        possible_burst_subtype = pb_subtype,
        candidate_id = pb_candidate_id,
        uncertainty_reason = pb_reason,
        auto_score = suppressWarnings(as.numeric(dat$auto_score %||% NA_real_)),
        stringsAsFactors = FALSE
      )
    }
    bind_rows(out)
  })

  aligned_window_data <- reactive({
    ds <- current_dataset()
    td <- ds$trains
    ledger <- ds$results$candidate_ledger %||% data.frame()
    axis_tbl <- selected_axis_table()
    draw_sec_pad <- raster_draw_window_sec(with_padding = TRUE)
    min_isi <- min_valid_isi_sec()
    auto_others_on <- isTRUE(input$auto_others)

    out <- list()
    for (i in seq_len(nrow(axis_tbl))) {
      tr <- as.character(axis_tbl$train[i])
      dat <- td[[tr]]
      if (is.null(dat) || nrow(dat) == 0) next
      n <- nrow(dat)
      ts <- suppressWarnings(as.numeric(dat$timestamp_sec))
      if (length(ts) != n || !any(is.finite(ts))) next
      t_align <- ts - ts[1]
      prev_t <- c(NA_real_, head(t_align, -1L))

      spike_idx <- which(is.finite(t_align) & t_align >= draw_sec_pad[1] & t_align <= draw_sec_pad[2])
      isi_idx <- which(seq_len(n) >= 2L & is.finite(t_align) & is.finite(prev_t) &
                         t_align >= draw_sec_pad[1] & prev_t <= draw_sec_pad[2])
      keep <- sort(unique(c(spike_idx, isi_idx, pmax(1L, isi_idx - 1L), pmin(n, spike_idx + 1L))))
      if (length(keep) == 0) next

      manual_all <- as.character(dat$pattern_manual %||% rep("", n)); manual_all[is.na(manual_all)] <- ""
      auto_all <- as.character(dat$pattern_auto %||% rep("", n)); auto_all[is.na(auto_all)] <- ""
      final_all <- compute_final_pattern(manual_all, auto_all, dat$ISI_sec, auto_others = auto_others_on, min_isi_sec = min_isi)
      audit_final_all <- stpd_audit_final_labels(dat, min_isi_sec = min_isi,
                                                 auto_others = auto_others_on,
                                                 prefer_stored = TRUE)
      pattern_view_mode <- input$pattern_view %||% "audit_final"
      pat_show <- switch(pattern_view_mode,
                         manual = manual_all[keep],
                         auto = auto_all[keep],
                         final = final_all[keep],
                         audit_final = audit_final_all[keep])
      if (is.null(pat_show)) pat_show <- final_all[keep]
      label_source <- ifelse(manual_all[keep] != "", "manual",
                             ifelse(auto_all[keep] != "", "auto",
                                    ifelse(as.character(final_all[keep]) != "", "implicit_final", "none")))

      pb_subtype <- rep("", length(keep)); pb_candidate_id <- rep("", length(keep)); pb_reason <- rep("", length(keep))
      if (!is.null(ledger) && nrow(ledger) > 0 && all(c("train", "start_isi", "end_isi") %in% names(ledger))) {
        lr <- ledger[as.character(ledger$train) == tr, , drop = FALSE]
        if (nrow(lr) > 0) {
          keep_min <- min(keep); keep_max <- max(keep)
          lr <- lr[suppressWarnings(as.integer(lr$end_isi)) >= keep_min & suppressWarnings(as.integer(lr$start_isi)) <= keep_max, , drop = FALSE]
          for (jj in seq_len(nrow(lr))) {
            s0 <- suppressWarnings(as.integer(lr$start_isi[jj])); e0 <- suppressWarnings(as.integer(lr$end_isi[jj]))
            if (!is.finite(s0) || !is.finite(e0) || e0 < s0) next
            idx0 <- keep[keep >= max(2L, s0) & keep <= min(n, e0)]
            if (length(idx0) == 0) next
            pos <- match(idx0, keep)
            fc <- as.character(lr$final_candidate_class[jj] %||% "")
            rc <- as.character(lr$raw_candidate_class[jj] %||% "")
            if (fc == "possible_burst" || rc == "possible_burst" || grepl("possible", as.character(lr$uncertainty_reason[jj] %||% ""))) {
              pb_subtype[pos] <- as.character(lr$possible_burst_subtype[jj] %||% "")
              pb_candidate_id[pos] <- as.character(lr$candidate_id[jj] %||% "")
              pb_reason[pos] <- as.character(lr$uncertainty_reason[jj] %||% "")
            }
          }
        }
      }

      get_num_col <- function(nm) {
        if (nm %in% names(dat)) suppressWarnings(as.numeric(dat[[nm]][keep])) else rep(NA_real_, length(keep))
      }
      get_any_col <- function(nm, default = "") {
        if (nm %in% names(dat)) dat[[nm]][keep] else rep(default, length(keep))
      }
      out[[length(out) + 1L]] <- data.frame(
        train = tr,
        train_label = as.character(axis_tbl$train_label[i]),
        train_order = axis_tbl$train_order[i],
        y = axis_tbl$y[i],
        idx = dat$idx[keep],
        time_align_sec = t_align[keep],
        timestamp_sec = ts[keep],
        ISI_sec = suppressWarnings(as.numeric(dat$ISI_sec[keep])),
        ISI_pct = get_num_col("ISI_pct"),
        ISI_range_pct_linear = get_num_col("ISI_range_pct_linear"),
        ISI_range_pct_log = get_num_col("ISI_range_pct_log"),
        ISI_robust_range_pct_log = get_num_col("ISI_robust_range_pct_log"),
        ISI_rank_n = get_num_col("ISI_rank_n"),
        pattern_manual = manual_all[keep],
        pattern_auto = auto_all[keep],
        pattern_final = final_all[keep],
        pattern_audit_final = audit_final_all[keep],
        pattern_show = pat_show,
        label_source = label_source,
        possible_burst_subtype = pb_subtype,
        candidate_id = pb_candidate_id,
        uncertainty_reason = pb_reason,
        auto_score = suppressWarnings(as.numeric(get_any_col("auto_score", NA_real_))),
        stringsAsFactors = FALSE
      )
    }
    if (length(out) == 0) return(data.frame())
    bind_rows(out)
  })
  
  raster_lod_state <- reactive({
    td <- current_trains()
    selected <- displayed_train_names()
    if (length(selected) == 0) return(list(n = 0L, mode = "none", message = ""))
    draw_sec_pad <- raster_draw_window_sec(with_padding = FALSE)
    n_visible <- 0L
    for (tr in intersect(selected, names(td))) {
      dat <- td[[tr]]
      if (is.null(dat) || nrow(dat) == 0) next
      t_align <- suppressWarnings(as.numeric(dat$timestamp_sec)) - suppressWarnings(as.numeric(dat$timestamp_sec[1]))
      n_visible <- n_visible + sum(is.finite(t_align) & t_align >= draw_sec_pad[1] & t_align <= draw_sec_pad[2], na.rm = TRUE)
    }
    full_limit <- safe_int(input$plot_max_visible_spikes_full, 50000L)
    interactive_limit <- max(full_limit, safe_int(input$plot_max_visible_spikes_interactive, 100000L))
    lod_mode <- input$plot_lod_mode %||% "auto"
    full <- identical(lod_mode, "full") || (identical(lod_mode, "auto") && n_visible <= full_limit)
    interactive <- full || (identical(lod_mode, "auto") && n_visible <= interactive_limit)
    mode <- if (full) "full" else if (interactive) "reduced" else "minimal"
    msg <- if (full) "" else if (interactive) {
      paste0("LOD \u8B66\u544A\uFF1A\u5F53\u524D\u7A97\u53E3\u5305\u542B ", n_visible, " \u4E2A spikes\u3002hover/\u9009\u62E9\u5DF2\u7B80\u5316\uFF1B\u8BF7\u653E\u5927\u540E\u8FDB\u884C\u7CBE\u786E\u624B\u52A8\u6807\u8BB0\u3002")
    } else {
      paste0("LOD \u8B66\u544A\uFF1A\u5F53\u524D\u7A97\u53E3\u5305\u542B ", n_visible, " \u4E2A spikes\uFF0C\u8D85\u8FC7\u4EA4\u4E92\u4E0A\u9650\uFF08", interactive_limit,
             "\uFF09\u3002\u56FE\u4E2D\u4F1A\u9690\u85CF\u5927\u90E8\u5206 hover/\u9009\u62E9\u6807\u8BB0\u3002\u8BF7\u5728\u624B\u52A8\u6807\u8BB0\u524D\u7F29\u5C0F\u65F6\u95F4\u7A97\u3002")
    }
    list(n = n_visible, mode = mode, message = msg)
  })

  output$raster_lod_warning <- renderUI({
    if (length(rv$datasets) == 0L) return(NULL)
    st <- raster_lod_state()
    if (is.null(st$message) || st$message == "") return(NULL)
    div(style = "background:#fff4e6;border:1px solid #ffd08a;border-radius:6px;padding:8px;margin-bottom:8px;color:#5c3b00;",
        strong("\u5927\u7A97\u53E3\u663E\u793A\u6A21\u5F0F\uFF1A"), st$message)
  })

  raw_spike_data <- reactive({
    td <- current_trains()
    selected <- displayed_train_names()
    validate(need(length(selected) > 0, "\u8BF7\u81F3\u5C11\u9009\u62E9\u4E00\u6761 train\u3002"))
    step <- track_step()
    
    y_index <- rev(seq_along(selected))
    y_pos <- 1 + (y_index - 1) * step
    
    out <- list()
    for (i in seq_along(selected)) {
      tr <- selected[i]
      dat <- ensure_train_isi_percentiles(td[[tr]], min_valid_isi_sec())
      if (nrow(dat) == 0) next
      out[[i]] <- data.frame(train = tr, train_label = tr, train_order = i, y = y_pos[i], idx = dat$idx,
                             time_orig_sec = dat$timestamp_sec, ISI_sec = dat$ISI_sec,
                             ISI_pct = dat$ISI_pct,
                             ISI_range_pct_linear = dat$ISI_range_pct_linear %||% rep(NA_real_, nrow(dat)),
                             ISI_range_pct_log = dat$ISI_range_pct_log %||% rep(NA_real_, nrow(dat)),
                             ISI_robust_range_pct_log = dat$ISI_robust_range_pct_log %||% rep(NA_real_, nrow(dat)),
                             ISI_rank_n = dat$ISI_rank_n,
                             stringsAsFactors = FALSE)
    }
    bind_rows(out)
  })

  task_events_current <- reactive({
    ds <- tryCatch(current_dataset(), error = function(e) NULL)
    if (is.null(ds)) return(stpd_empty_task_events())
    stpd_normalize_task_events(ds$task_events %||% data.frame(), source = ds$meta$display_name %||% "")
  })

  task_event_selected_names <- function(events = NULL) {
    events <- events %||% task_events_current()
    if (is.null(events) || nrow(events) == 0L) return(character(0))
    all_names <- sort(unique(as.character(events$event_name)))
    sel <- as.character(input$task_event_names %||% input$neural_manifold_dataset_event_names %||% all_names)
    sel <- intersect(sel, all_names)
    if (length(sel) == 0L) sel <- all_names
    sel
  }

  task_events_filtered <- function(events = NULL, use_neural_input = FALSE) {
    events <- events %||% task_events_current()
    if (is.null(events) || nrow(events) == 0L) return(stpd_empty_task_events())
    all_names <- sort(unique(as.character(events$event_name)))
    sel <- if (isTRUE(use_neural_input)) {
      as.character(input$neural_manifold_dataset_event_names %||% all_names)
    } else {
      as.character(input$task_event_names %||% all_names)
    }
    sel <- intersect(sel, all_names)
    if (length(sel) == 0L) sel <- all_names
    events[as.character(events$event_name) %in% sel, , drop = FALSE]
  }

  output$task_event_selector <- renderUI({
    events <- task_events_current()
    if (nrow(events) == 0L) {
      return(tags$div(class = "small-note", "\u5F53\u524D\u6570\u636E\u96C6\u6CA1\u6709 Event / Event_* \u4EFB\u52A1\u4E8B\u4EF6\u5217\u3002"))
    }
    all_names <- sort(unique(as.character(events$event_name)))
    selected_names <- intersect(as.character(isolate(input$task_event_names) %||% all_names), all_names)
    if (length(selected_names) == 0L) selected_names <- all_names
    ev <- events[as.character(events$event_name) %in% selected_names, , drop = FALSE]
    jump_labels <- paste0(
      ev$event_name,
      " @ ",
      format(round(ev$event_time_sec, 4), trim = TRUE, scientific = FALSE),
      " s"
    )
    tagList(
      selectizeInput(
        "task_event_names",
        "\u4EFB\u52A1\u4E8B\u4EF6\u7C7B\u578B",
        choices = all_names,
        selected = selected_names,
        multiple = TRUE,
        options = list(plugins = list("remove_button"), closeAfterSelect = TRUE)
      ),
      selectInput(
        "task_event_jump_id",
        "\u8DF3\u8F6C\u5230\u67D0\u6B21\u4E8B\u4EF6",
        choices = stats::setNames(ev$event_id, jump_labels),
        selected = ev$event_id[1] %||% ""
      ),
      tags$div(class = "small-note", "\u4E8B\u4EF6\u4EC5\u4F5C\u4E3A\u56FE\u4E0A\u6CE8\u91CA\u3001\u884C\u4E3A\u5BF9\u9F50\u548C\u4E0B\u6E38\u9A8C\u8BC1\u5C42\uFF1B\u4E0D\u4F1A\u53C2\u4E0E\u6838\u5FC3 burst/pause/tonic \u68C0\u6D4B\u3002")
    )
  })

  output$neural_manifold_dataset_event_selector <- renderUI({
    events <- task_events_current()
    if (nrow(events) == 0L) {
      return(tags$div(class = "small-note", "No embedded task-event columns were found in the current dataset."))
    }
    all_names <- sort(unique(as.character(events$event_name)))
    selected_names <- intersect(as.character(isolate(input$neural_manifold_dataset_event_names) %||% all_names), all_names)
    if (length(selected_names) == 0L) selected_names <- all_names
    tagList(
      selectizeInput(
        "neural_manifold_dataset_event_names",
        "Dataset task events",
        choices = all_names,
        selected = selected_names,
        multiple = TRUE,
        options = list(plugins = list("remove_button"), closeAfterSelect = TRUE)
      ),
      tags$div(class = "small-note", "These events annotate peri-movement bins and can supply sliceTCA trial times when no separate trial CSV is uploaded.")
    )
  })

  raw_time_bounds_sec <- reactive({
    td <- tryCatch(current_trains(), error = function(e) NULL)
    selected <- tryCatch(displayed_train_names(), error = function(e) character(0))
    selected <- intersect(selected, names(td %||% list()))
    vals <- unlist(lapply(selected, function(tr) {
      dat <- td[[tr]]
      if (is.null(dat) || !("timestamp_sec" %in% names(dat))) return(numeric(0))
      ts <- suppressWarnings(as.numeric(dat$timestamp_sec))
      ts[is.finite(ts)]
    }), use.names = FALSE)
    if (length(vals) == 0L) return(c(0, 1))
    rng <- range(vals, na.rm = TRUE)
    if (!all(is.finite(rng)) || rng[2] <= rng[1]) rng <- c(rng[1], rng[1] + 1)
    rng
  })

  raw_window_for_plot <- function() {
    bounds <- raw_time_bounds_sec()
    f <- unit_factor()
    x <- rv$raw_view_x
    if (!stpd_valid_xrange_window(x)) {
      xin <- tryCatch(input$raw_xrange, error = function(e) NULL)
      if (stpd_valid_xrange_window(xin)) x <- sort(as.numeric(xin)) / f
    }
    if (!stpd_valid_xrange_window(x)) x <- bounds
    x <- sort(as.numeric(x))
    x[1] <- max(bounds[1], x[1])
    x[2] <- min(bounds[2], x[2])
    if (x[2] <= x[1]) x <- bounds
    x
  }

  output$raw_time_window_controls <- renderUI({
    if (length(rv$datasets) == 0L) return(NULL)
    bounds <- raw_time_bounds_sec()
    f <- unit_factor()
    u <- input$time_unit %||% "s"
    min_plot <- floor(bounds[1] * f)
    max_plot <- ceiling(bounds[2] * f)
    if (!is.finite(min_plot) || !is.finite(max_plot) || max_plot <= min_plot) {
      min_plot <- 0
      max_plot <- if (identical(u, "ms")) 1000 else 1
    }
    cur <- raw_window_for_plot() * f
    cur[1] <- max(min_plot, min(cur[1], max_plot))
    cur[2] <- max(min_plot, min(cur[2], max_plot))
    if (cur[2] <= cur[1]) cur <- c(min_plot, max_plot)
    sliderInput(
      "raw_xrange",
      paste0("\u539F\u59CB timestamp \u65F6\u95F4\u7A97\uFF08", u, "\uFF09"),
      min = min_plot,
      max = max_plot,
      value = cur,
      step = if (identical(u, "ms")) 1 else 0.001,
      ticks = FALSE,
      width = "100%"
    )
  })

  observeEvent(input$raw_xrange, {
    f <- unit_factor()
    x <- suppressWarnings(as.numeric(input$raw_xrange))
    if (stpd_valid_xrange_window(x)) rv$raw_view_x <- sort(x) / f
  }, ignoreInit = TRUE)

  observeEvent(input$jump_to_task_event, {
    events <- task_events_filtered()
    if (nrow(events) == 0L) {
      showNotification("\u5F53\u524D\u6570\u636E\u96C6\u6CA1\u6709\u53EF\u8DF3\u8F6C\u7684\u4EFB\u52A1\u4E8B\u4EF6\u3002", type = "warning", duration = 5)
      return()
    }
    ev_id <- as.character(input$task_event_jump_id %||% "")[1]
    hit <- events[as.character(events$event_id) == ev_id, , drop = FALSE]
    if (nrow(hit) == 0L) hit <- events[1, , drop = FALSE]
    t0 <- suppressWarnings(as.numeric(hit$event_time_sec[1]))
    pre <- suppressWarnings(as.numeric(input$task_event_jump_pre_sec %||% 1)[1])
    post <- suppressWarnings(as.numeric(input$task_event_jump_post_sec %||% 2)[1])
    if (!is.finite(pre) || pre < 0) pre <- 1
    if (!is.finite(post) || post <= 0) post <- 2
    if (!is.finite(t0)) return()
    raw_bounds <- raw_time_bounds_sec()
    rv$raw_view_x <- c(max(raw_bounds[1], t0 - pre), min(raw_bounds[2], t0 + post))
    if (rv$raw_view_x[2] <= rv$raw_view_x[1]) rv$raw_view_x <- raw_bounds
    f <- unit_factor()
    updateSliderInput(session, "raw_xrange", value = rv$raw_view_x * f)

    axis_tbl <- tryCatch(selected_axis_table(), error = function(e) data.frame())
    td <- tryCatch(current_trains(), error = function(e) list())
    if (nrow(axis_tbl) > 0L) {
      tr0 <- as.character(axis_tbl$train[1])
      dat0 <- td[[tr0]]
      if (!is.null(dat0) && "timestamp_sec" %in% names(dat0)) {
        first_ts <- suppressWarnings(as.numeric(dat0$timestamp_sec[1]))
        x0 <- t0 - first_ts
        if (is.finite(x0)) {
          aligned_bounds_plot <- c(max(0, x0 - pre), x0 + post) * f
          max_plot <- suppressWarnings(as.numeric(rv$xrange_max_plot %||% max(aligned_bounds_plot, na.rm = TRUE))[1])
          if (is.finite(max_plot) && max_plot > 0) aligned_bounds_plot <- pmax(0, pmin(aligned_bounds_plot, max_plot))
          if (aligned_bounds_plot[2] > aligned_bounds_plot[1]) {
            rv$view_align_x <- aligned_bounds_plot
            update_xrange_slider_input("xrange", value = aligned_bounds_plot)
            update_xrange_slider_input("xrange_plot", value = aligned_bounds_plot)
            sync_xrange_length_inputs(aligned_bounds_plot)
          }
        }
      }
    }
    updateTabsetPanel(session, "main_tabs", selected = "\u539F\u59CB\u65F6\u95F4\u6233\u56FE")
  }, ignoreInit = TRUE)

  task_event_overlay_aligned <- function(axis_tbl, draw_sec_pad) {
    if (!isTRUE(input$show_task_events)) return(data.frame())
    events <- task_events_filtered()
    if (nrow(events) == 0L || is.null(axis_tbl) || nrow(axis_tbl) == 0L) return(data.frame())
    td <- current_trains()
    rows <- list()
    for (ii in seq_len(nrow(axis_tbl))) {
      tr <- as.character(axis_tbl$train[ii])
      dat <- td[[tr]]
      if (is.null(dat) || !("timestamp_sec" %in% names(dat))) next
      first_ts <- suppressWarnings(as.numeric(dat$timestamp_sec[1]))
      if (!is.finite(first_ts)) next
      xx <- events$event_time_sec - first_ts
      keep <- is.finite(xx) & xx >= draw_sec_pad[1] & xx <= draw_sec_pad[2]
      if (!any(keep)) next
      ev <- events[keep, , drop = FALSE]
      rows[[length(rows) + 1L]] <- data.frame(
        train = tr,
        train_label = as.character(axis_tbl$train_label[ii]),
        y = suppressWarnings(as.numeric(axis_tbl$y[ii])),
        event_id = ev$event_id,
        event_name = ev$event_name,
        event_time_sec = ev$event_time_sec,
        x_sec = xx[keep],
        stringsAsFactors = FALSE
      )
    }
    out <- do.call(rbind, rows)
    if (is.null(out)) data.frame() else out
  }

  task_event_overlay_raw <- function(window_sec = NULL) {
    if (!isTRUE(input$show_task_events)) return(data.frame())
    events <- task_events_filtered()
    if (nrow(events) == 0L) return(data.frame())
    if (!is.null(window_sec) && length(window_sec) == 2L && all(is.finite(window_sec))) {
      events <- events[events$event_time_sec >= min(window_sec) & events$event_time_sec <= max(window_sec), , drop = FALSE]
    }
    events
  }

  dbs_track_scope <- reactive({
    scope <- as.character(input$dbs_track_dataset_scope %||% "current")[1]
    if (identical(scope, "all")) "all" else "current"
  })

  dbs_track_dataset_for_view <- reactive({
    if (identical(dbs_track_scope(), "all")) {
      validate(need(length(rv$datasets) > 0, "\u8BF7\u5148\u52A0\u8F7D\u81F3\u5C11\u4E00\u4E2A\u6570\u636E\u96C6\u3002"))
      return(stpd_dbs_track_combine_datasets(rv$datasets, ids = names(rv$datasets)))
    }
    current_dataset()
  })

  dbs_track_metadata_for_view <- reactive({
    if (identical(dbs_track_scope(), "all")) {
      ds <- dbs_track_dataset_for_view()
      return(tryCatch(stpd_dbs_track_metadata(ds), error = function(e) data.frame()))
    }
    tryCatch(current_train_metadata(), error = function(e) {
      ds <- dbs_track_dataset_for_view()
      ds$meta$train_metadata %||% data.frame()
    })
  })

  output$dbs_track_structure_selector <- renderUI({
    meta <- tryCatch(dbs_track_metadata_for_view(), error = function(e) data.frame())
    if (is.null(meta) || nrow(meta) == 0) {
      return(tags$div(class = "small-note", "\u4E0A\u4F20\u6570\u636E\u540E\u53EF\u6309\u6838\u56E2\u7B5B\u9009\u3002"))
    }
    if (!("side" %in% names(meta))) meta$side <- NA_character_
    if (!("recording_depth" %in% names(meta))) meta$recording_depth <- NA_real_
    if (!("structure" %in% names(meta))) meta$structure <- "unknown"
    side <- toupper(as.character(meta$side))
    depth <- suppressWarnings(as.numeric(meta$recording_depth))
    structures <- as.character(meta$structure[side %in% c("L", "R") & is.finite(depth)])
    structures <- sort(unique(structures[!is.na(structures) & nzchar(structures)]))
    if (length(structures) == 0) {
      return(tags$div(class = "small-note", "\u5F53\u524D\u6570\u636E\u6CA1\u6709\u53EF\u89E3\u6790 LT/RT + D \u6DF1\u5EA6\u7684 train\u3002"))
    }
    selectizeInput(
      "dbs_track_structures",
      "\u6838\u56E2 / structure",
      choices = structures,
      selected = structures,
      multiple = TRUE,
      options = list(placeholder = "\u9009\u62E9\u8981\u663E\u793A\u7684\u6838\u56E2")
    )
  })

  dbs_track_data <- reactive({
    ds <- dbs_track_dataset_for_view()
    meta <- tryCatch(dbs_track_metadata_for_view(), error = function(e) ds$meta$train_metadata %||% data.frame())
    selected <- NULL
    if (identical(dbs_track_scope(), "current") && isTRUE(input$dbs_track_visible_only)) {
      selected <- tryCatch(as.character(selected_axis_table()$train), error = function(e) character(0))
    }
    start_sec <- input$dbs_track_start_sec %||% 0
    window_sec <- input$dbs_track_window_sec %||% 0.5
    if (isTRUE(input$dbs_track_sync_raster_window) && identical(input$dbs_track_time_origin %||% "aligned", "aligned")) {
      f <- unit_factor()
      x_use <- raster_window_for_plot(debounced = TRUE, prefer_view = TRUE)
      x_use <- suppressWarnings(as.numeric(x_use))
      if (length(x_use) == 2L && all(is.finite(x_use)) && x_use[2] > x_use[1] && is.finite(f) && f > 0) {
        sec <- sort(x_use) / f
        start_sec <- max(0, sec[1])
        window_sec <- max(0.001, diff(sec))
      }
    }
    stpd_dbs_track_prepare(
      ds,
      metadata = meta,
      selected_trains = selected,
      structures = input$dbs_track_structures,
      sides = input$dbs_track_sides %||% c("L", "R"),
      start_sec = start_sec,
      window_sec = window_sec,
      max_trains_per_side = input$dbs_track_max_trains_per_side %||% 0,
      time_origin = input$dbs_track_time_origin %||% "aligned",
      pattern_mode = input$dbs_track_pattern_mode %||% "audit_final",
      auto_others = isTRUE(input$auto_others),
      min_isi_sec = min_valid_isi_sec()
    )
  })

  output$dbs_track_plot <- renderPlotly({
    prep <- dbs_track_data()
    if (identical(input$dbs_track_view_mode %||% "dot", "2d")) {
      stpd_dbs_track_plotly(
        prep,
        time_unit = if (identical(input$time_unit, "ms")) "ms" else "s",
        show_labels = isTRUE(input$dbs_track_show_labels),
        show_anatomical_context = isTRUE(input$dbs_track_show_context),
        depth_direction = input$dbs_track_depth_direction %||% "larger_deeper"
      )
    } else {
      stpd_dbs_track_plotly_dot_model(
        prep,
        time_unit = if (identical(input$time_unit, "ms")) "ms" else "s",
        show_labels = isTRUE(input$dbs_track_show_labels),
        show_anatomical_context = isTRUE(input$dbs_track_show_context),
        depth_direction = input$dbs_track_depth_direction %||% "larger_deeper",
        animate_particles = isTRUE(input$dbs_track_particle_flow)
      )
    }
  })

  dbs_track_static_size <- function() {
    width <- suppressWarnings(as.numeric(input$dbs_track_static_width %||% 11))[1]
    height <- suppressWarnings(as.numeric(input$dbs_track_static_height %||% 6.5))[1]
    dpi <- suppressWarnings(as.numeric(input$dbs_track_static_dpi %||% 600))[1]
    if (!is.finite(width) || width <= 0) width <- 11
    if (!is.finite(height) || height <= 0) height <- 6.5
    if (!is.finite(dpi) || dpi <= 0) dpi <- 600
    list(width = max(4, min(24, width)), height = max(3, min(18, height)), dpi = max(150, min(1200, dpi)))
  }

  dbs_track_static_plot <- function() {
    validate(need(requireNamespace("ggplot2", quietly = TRUE), "\u8BF7\u5148\u5B89\u88C5 ggplot2 \u4EE5\u5BFC\u51FA\u8BBA\u6587\u9759\u6001\u56FE\u3002"))
    prep <- dbs_track_data()
    stpd_dbs_track_ggplot_static(
      prep,
      time_unit = if (identical(input$time_unit, "ms")) "ms" else "s",
      show_labels = isTRUE(input$dbs_track_show_labels),
      show_anatomical_context = isTRUE(input$dbs_track_show_context),
      depth_direction = input$dbs_track_depth_direction %||% "larger_deeper"
    )
  }

  output$download_dbs_track_static_png <- downloadHandler(
    filename = function() {
      ds <- dbs_track_dataset_for_view()
      nm <- gsub("[^A-Za-z0-9_\\-\\.]", "_", ds$meta$display_name %||% "dataset")
      paste0(nm, "_DBS_target_static_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
    },
    content = function(file) {
      sz <- dbs_track_static_size()
      plt <- dbs_track_static_plot()
      ggplot2::ggsave(file, plot = plt, width = sz$width, height = sz$height, units = "in",
                      dpi = sz$dpi, bg = "white", limitsize = FALSE)
    }
  )

  output$download_dbs_track_static_pdf <- downloadHandler(
    filename = function() {
      ds <- dbs_track_dataset_for_view()
      nm <- gsub("[^A-Za-z0-9_\\-\\.]", "_", ds$meta$display_name %||% "dataset")
      paste0(nm, "_DBS_target_static_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
    },
    content = function(file) {
      sz <- dbs_track_static_size()
      plt <- dbs_track_static_plot()
      ggplot2::ggsave(file, plot = plt, width = sz$width, height = sz$height, units = "in",
                      device = grDevices::pdf, bg = "white", limitsize = FALSE)
    }
  )

  output$dbs_track_inventory_table <- DT::renderDT({
    prep <- dbs_track_data()
    rows <- prep$rows %||% data.frame()
    validate(need(nrow(rows) > 0, "\u6CA1\u6709\u53EF\u7ED8\u5236\u7684 LT/RT + \u6DF1\u5EA6 spike train\u3002"))
    cols <- c(
      "dataset", "source_train", "train", "structure", "side", "trajectory", "recording_depth",
      "channel_type", "wire", "unit_id", "n_spikes_window",
      "window_start_sec", "window_end_sec", "time_origin"
    )
    cols <- intersect(cols, names(rows))
    DT::datatable(
      rows[, cols, drop = FALSE],
      rownames = FALSE,
      filter = "top",
      options = list(pageLength = 8, scrollX = TRUE)
    )
  })
  
  # ----------------------------------------------------------
  # Aligned plot
  # ----------------------------------------------------------
  output$raster_plot <- renderPlotly({
    rv$raster_plot_refresh_token
    if (length(rv$datasets) == 0L) {
      p0 <- plot_ly(source = "raster", type = "scatter", mode = "markers", x = numeric(0), y = numeric(0))
      p0 <- layout(
        p0,
        xaxis = list(visible = FALSE, zeroline = FALSE, showgrid = FALSE, showticklabels = FALSE),
        yaxis = list(visible = FALSE, zeroline = FALSE, showgrid = FALSE, showticklabels = FALSE),
        margin = list(l = 0, r = 0, t = 0, b = 0),
        paper_bgcolor = "#ffffff",
        plot_bgcolor = "#ffffff"
      )
      p0 <- event_register(p0, "plotly_selected")
      p0 <- event_register(p0, "plotly_relayout")
      return(config(p0, displaylogo = FALSE))
    }
    show_raster_progress <- isolate(isTRUE(rv$raster_plot_progress_active))
    build_raster_plot <- function() {
    if (isTRUE(show_raster_progress)) {
      on.exit({
        rv$raster_plot_progress_active <- FALSE
      }, add = TRUE)
    }
    raster_progress <- function(value, detail, type = "active") {
      if (isTRUE(show_raster_progress)) {
        plot_render_progress(
          "raster_plot",
          value = value,
          detail = detail,
          message = "\u6B63\u5728\u751F\u6210\u5BF9\u9F50\u65F6\u95F4\u6233\u56FE",
          type = type
        )
        setProgress(value, detail = detail)
        if (type %in% c("success", "error")) rv$raster_plot_progress_active <- FALSE
      }
    }
    raster_progress(0.05, "\u6B63\u5728\u51C6\u5907\u53EF\u89C1 spike trains")
    dat_all <- aligned_window_data()
    if (nrow(dat_all) == 0L) {
      raster_progress(1, "\u5F53\u524D\u7A97\u53E3\u6CA1\u6709\u53EF\u663E\u793A spike", "success")
      return(stpd_empty_plotly_message("\u5F53\u524D\u7A97\u53E3\u6CA1\u6709\u53EF\u663E\u793A spike\u3002", source = "raster", events = c("plotly_selected", "plotly_relayout")))
    }
    
    raster_progress(0.18, "\u6B63\u5728\u6574\u7406 spike \u548C\u6A21\u5F0F\u6807\u7B7E")
    dat_all <- dat_all %>%
      group_by(train) %>%
      arrange(idx, .by_group = TRUE) %>%
      mutate(
        isi_start_sec = lag(time_align_sec),
        isi_end_sec = time_align_sec,
        isi_mid_sec = (isi_start_sec + isi_end_sec) / 2,
        timestamp_left_sec = lag(timestamp_sec),
        timestamp_right_sec = timestamp_sec,
        pattern_manual_chr = as.character(pattern_manual %||% ""),
        pattern_auto_chr = as.character(pattern_auto %||% ""),
        pattern_final_chr = as.character(pattern_final %||% ""),
        pattern_audit_final_chr = as.character(.data$pattern_audit_final),
        spike_pattern_manual = ifelse(pattern_manual_chr != "", pattern_manual_chr,
                                      dplyr::lead(pattern_manual_chr, default = "")),
        spike_pattern_auto = ifelse(pattern_auto_chr != "", pattern_auto_chr,
                                    dplyr::lead(pattern_auto_chr, default = "")),
        spike_pattern_final = ifelse(pattern_final_chr != "", pattern_final_chr,
                                     dplyr::lead(pattern_final_chr, default = "")),
        spike_pattern_audit_final = ifelse(.data$pattern_audit_final_chr != "", .data$pattern_audit_final_chr,
                                           dplyr::lead(.data$pattern_audit_final_chr, default = "")),
        spike_source_final = dplyr::case_when(
          spike_pattern_manual != "" ~ "manual",
          spike_pattern_auto == "possible_burst" ~ "review",
          spike_pattern_auto != "" ~ "auto",
          spike_pattern_final == "possible_burst" ~ "review",
          spike_pattern_final != "" ~ "auto",
          TRUE ~ "none"
        )
      ) %>%
      ungroup()
    
    f <- unit_factor()
    u <- input$time_unit
    step <- track_step()
    
    x_use <- raster_window_for_plot(debounced = TRUE, prefer_view = TRUE)
    draw_sec <- sort(x_use) / f
    pad_sec <- max(0.002, diff(draw_sec) * raster_prefetch_fraction())
    draw_sec_pad <- c(max(0, draw_sec[1] - pad_sec), draw_sec[2] + pad_sec)
    
    has_visible_spike <- dat_all %>% filter(time_align_sec >= draw_sec[1], time_align_sec <= draw_sec[2])
    has_visible_isi <- dat_all %>% filter(!is.na(isi_start_sec), isi_end_sec >= draw_sec[1], isi_start_sec <= draw_sec[2])
    has_buffer_spike <- dat_all %>% filter(time_align_sec >= draw_sec_pad[1], time_align_sec <= draw_sec_pad[2])
    has_buffer_isi <- dat_all %>% filter(!is.na(isi_start_sec), isi_end_sec >= draw_sec_pad[1], isi_start_sec <= draw_sec_pad[2])
    if (!(nrow(has_buffer_spike) > 0 || nrow(has_buffer_isi) > 0)) {
      raster_progress(1, "\u5F53\u524D\u7A97\u53E3\u6CA1\u6709 spike/ISI", "success")
      return(stpd_empty_plotly_message("\u5F53\u524D\u7A97\u53E3\u6CA1\u6709 spike/ISI\u3002", source = "raster", events = c("plotly_selected", "plotly_relayout")))
    }
    raster_progress(0.34, "\u6B63\u5728\u8BA1\u7B97\u53EF\u89C6\u533A\u548C LOD")
    full_limit <- safe_int(input$plot_max_visible_spikes_full, 50000L)
    interactive_limit <- max(full_limit, safe_int(input$plot_max_visible_spikes_interactive, 100000L))
	    lod_mode <- input$plot_lod_mode %||% "auto"
	    visible_spike_n <- nrow(has_visible_spike)
	    visible_train_n <- length(unique(as.character(c(has_visible_spike$train, has_visible_isi$train))))
	    many_train_overview <- visible_train_n >= 12L && visible_spike_n > 8000L && identical(lod_mode, "auto")
	    lod_full <- identical(lod_mode, "full") || (identical(lod_mode, "auto") && visible_spike_n <= full_limit && !many_train_overview)
	    lod_interactive <- lod_full || (identical(lod_mode, "auto") && visible_spike_n <= interactive_limit)
	    lod_note <- if (!lod_full) paste0(
	      "LOD \u6A21\u5F0F\uFF1A", visible_spike_n, " \u4E2A\u53EF\u89C1 spikes\uFF1Bhover/\u9009\u62E9\u6807\u8BB0\u5DF2\u7B80\u5316\u3002\u8BF7\u653E\u5927\u540E\u624B\u52A8\u6807\u8BB0\u3002",
	      if (many_train_overview) " \u5F53\u524D\u9875 train \u8F83\u591A\uFF0C\u5DF2\u81EA\u52A8\u4F7F\u7528\u6982\u89C8\u6E32\u67D3\u3002" else ""
	    ) else ""
    
    dat_plot <- dat_all %>%
      mutate(time_plot = time_align_sec * f,
             timestamp_plot = timestamp_sec * f,
             timestamp_left_plot = timestamp_left_sec * f,
             timestamp_right_plot = timestamp_right_sec * f,
             ISI_plot = ISI_sec * f,
             isi_start_plot = isi_start_sec * f,
             isi_end_plot = isi_end_sec * f,
             isi_mid_plot = isi_mid_sec * f,
             train_label_html = stpd_html_escape(train_label),
             pattern_final_html = stpd_html_escape(pattern_final),
             pattern_audit_final_html = stpd_html_escape(.data$pattern_audit_final),
             label_source_html = stpd_html_escape(label_source),
             possible_burst_subtype_html = stpd_html_escape(possible_burst_subtype),
             uncertainty_reason_html = stpd_html_escape(uncertainty_reason))
    
    axis_tbl <- selected_axis_table() %>% dplyr::arrange(y)
    selected <- as.character(axis_tbl$train)
    k <- nrow(axis_tbl)
    y_tickvals <- axis_tbl$y
    y_ticktext <- stpd_html_escape(axis_tbl$train_label)
	    y_range <- c(min(axis_tbl$y, na.rm = TRUE) - 0.5 * step, max(axis_tbl$y, na.rm = TRUE) + 0.5 * step)
	    spike_h_eff <- min(input$spike_height, max(0.10, 0.90 * step))
	    tick_font_size <- if (k >= 10) 10 else if (k >= 8) 11 else 12
	    delta_overlay <- data.frame()
	    if (isTRUE(input$show_parameter_delta_overlay) && !is.null(rv$parameter_delta_preview)) {
	      delta_overlay <- tryCatch(
	        stpd_parameter_delta_overlay_rows(rv$parameter_delta_preview, current_dataset()$trains, selected_trains = selected),
	        error = function(e) data.frame()
	      )
	      if (!is.null(delta_overlay) && nrow(delta_overlay) > 0) {
	        y_map <- stats::setNames(axis_tbl$y, as.character(axis_tbl$train))
	        delta_overlay$y <- suppressWarnings(as.numeric(y_map[as.character(delta_overlay$train)])) + 0.28 * step
	        delta_overlay$x0 <- suppressWarnings(as.numeric(delta_overlay$start_align_sec)) * f
	        delta_overlay$x1 <- suppressWarnings(as.numeric(delta_overlay$end_align_sec)) * f
	        delta_overlay <- delta_overlay[is.finite(delta_overlay$y) & is.finite(delta_overlay$x0) & is.finite(delta_overlay$x1), , drop = FALSE]
	        delta_overlay <- delta_overlay[delta_overlay$x1 >= draw_sec_pad[1] * f & delta_overlay$x0 <= draw_sec_pad[2] * f, , drop = FALSE]
	        delta_overlay$delta_text <- paste0(
	          "\u53C2\u6570 dry-run \u5DEE\u5F02<br>",
	          "\u72B6\u6001\uFF1A", stpd_html_escape(delta_overlay$status),
	          "<br>Train\uFF1A", stpd_html_escape(delta_overlay$train),
	          "<br>Baseline\uFF1A", ifelse(nzchar(delta_overlay$baseline_pattern), stpd_html_escape(delta_overlay$baseline_pattern), "none"),
	          "<br>Current\uFF1A", ifelse(nzchar(delta_overlay$current_pattern), stpd_html_escape(delta_overlay$current_pattern), "none"),
	          "<br>ISI\uFF1A", delta_overlay$start_isi, "-", delta_overlay$end_isi,
	          "<br>IoU\uFF1A", ifelse(is.finite(delta_overlay$iou), signif(delta_overlay$iou, 4), "NA")
	        )
	      }
	    }
	    
		    raster_progress(0.48, "\u6B63\u5728\u6784\u5EFA Plotly raster traces")
		    p <- plot_ly(source = "raster")
	    cand_audit <- current_dataset()$results$candidate_diagnostic_audit %||% current_dataset()$results$burst_candidates %||% data.frame()
	    structure_overlay <- current_dataset()$results$structure_candidates %||% data.frame()

    raster_batch_mode <- k >= 12L && !isTRUE(input$show_rejected_burst_candidates) && !isTRUE(input$show_burst_sublabel_structures)
    if (isTRUE(raster_batch_mode)) {
      raster_progress(0.54, sprintf("\u6B63\u5728\u6982\u89C8\u6E32\u67D3 %d \u6761 train", k))
      visible_spike <- dat_plot[
        dat_plot$time_align_sec >= draw_sec_pad[1] &
          dat_plot$time_align_sec <= draw_sec_pad[2],
        ,
        drop = FALSE
      ]
      visible_isi <- dat_plot[
        !is.na(dat_plot$isi_start_sec) &
          dat_plot$isi_end_sec >= draw_sec_pad[1] &
          dat_plot$isi_start_sec <= draw_sec_pad[2],
        ,
        drop = FALSE
      ]

      if (nrow(visible_spike) > 0) {
        visible_spike$y0 <- visible_spike$y - spike_h_eff / 2
        visible_spike$y1 <- visible_spike$y + spike_h_eff / 2
        p <- add_segments(
          p,
          data = visible_spike,
          x = ~time_plot, xend = ~time_plot,
          y = ~y0, yend = ~y1,
          type = "scatter", mode = "lines",
          line = list(width = base_spike_line_width(), color = "#000000", dash = "solid"),
          name = "spikes", showlegend = FALSE,
          hoverinfo = if (lod_full) "text" else "none",
          text = ~paste0(
	            "Train: ", train_label_html,
	            "<br>Spike index: ", idx,
	            "<br>Timestamp: ", round(timestamp_sec, 6), " s"
          )
        )

        p <- add_markers(
          p,
          data = if (lod_interactive) visible_spike else visible_spike[0, , drop = FALSE],
          x = ~time_plot, y = ~y,
          opacity = 0,
          marker = list(size = 12),
          showlegend = FALSE, hoverinfo = "none",
          customdata = ~paste0(train, "__", idx),
          inherit = FALSE
        )
      }
      raster_progress(0.66, sprintf("\u5DF2\u6784\u5EFA %d \u4E2A\u53EF\u89C1 spike ticks", nrow(visible_spike)))

      if (nrow(visible_isi) > 0) {
        pats <- c("burst", "long_burst", "possible_burst", "pause", "tonic", "high_frequency_tonic", "high_frequency_spiking")
        if (!isTRUE(input$show_possible)) pats <- setdiff(pats, "possible_burst")
        if (isTRUE(input$show_others)) pats <- c(pats, "others")

        add_pattern_strips <- function(pp, df, pat, source_kind) {
          if (nrow(df) == 0) return(pp)
          style <- pattern_strip_style(pat, source = if (identical(source_kind, "manual")) "manual" else "auto")
          add_segments(
            pp,
            data = df,
            x = ~isi_start_plot, xend = ~isi_end_plot,
            y = ~y, yend = ~y,
            type = "scatter", mode = "lines",
            line = list(width = pattern_strip_line_width(), color = style$color, dash = style$dash),
            hoverinfo = "none", showlegend = FALSE,
            inherit = FALSE
          )
        }

        if (identical(input$pattern_view, "final")) {
          for (pat in pats) {
            sub_pat <- visible_isi[visible_isi$pattern_auto == pat, , drop = FALSE]
            if (nrow(sub_pat) > 0) p <- add_pattern_strips(p, sub_pat, pat, "auto")
          }
          for (pat in pats) {
            sub_pat <- visible_isi[visible_isi$pattern_manual == pat, , drop = FALSE]
            if (nrow(sub_pat) > 0) p <- add_pattern_strips(p, sub_pat, pat, "manual")
          }
        } else if (identical(input$pattern_view, "manual")) {
          for (pat in pats) {
            sub_pat <- visible_isi[visible_isi$pattern_manual == pat, , drop = FALSE]
            if (nrow(sub_pat) > 0) p <- add_pattern_strips(p, sub_pat, pat, "manual")
          }
        } else if (identical(input$pattern_view, "auto")) {
          for (pat in pats) {
            sub_pat <- visible_isi[visible_isi$pattern_auto == pat, , drop = FALSE]
            if (nrow(sub_pat) > 0) p <- add_pattern_strips(p, sub_pat, pat, "auto")
          }
        } else {
          for (pat in pats) {
            sub_pat <- visible_isi[visible_isi$pattern_show == pat, , drop = FALSE]
            if (nrow(sub_pat) > 0) p <- add_pattern_strips(p, sub_pat, pat, "auto")
          }
        }

        if (isTRUE(input$show_manual_others_always)) {
          sub_m_oth <- visible_isi[visible_isi$pattern_manual == "others", , drop = FALSE]
          if (nrow(sub_m_oth) > 0) p <- add_pattern_strips(p, sub_m_oth, "others", "manual")
        }

        pv <- rv$preview_candidate
        if (isTRUE(input$show_near_miss_preview) && !is.null(pv) && isTRUE(pv$active)) {
          pv_s <- suppressWarnings(as.integer(pv$start_isi))
          pv_e <- suppressWarnings(as.integer(pv$end_isi))
          pv_dat <- visible_isi[
            visible_isi$train == as.character(pv$train) &
              visible_isi$idx >= pv_s & visible_isi$idx <= pv_e,
            ,
            drop = FALSE
          ]
          if (nrow(pv_dat) > 0) {
            pv_col <- switch(
              pv$pattern,
              "burst" = "#c026d3",
              "tonic" = "#65a30d",
              "pause" = "#2563eb",
              "#f97316"
            )
            pv_dat$near_miss_text <- paste0(
              "Selected candidate ISI<br>",
              "Pattern: ", stpd_html_escape(pv$pattern),
              "<br>Source: ", stpd_html_escape(pv$category),
              "<br>Evidence/parameter: ", stpd_html_escape(pv$parameter),
              ifelse(nzchar(as.character(pv$details %||% "")), paste0("<br>Details: ", stpd_html_escape(pv$details)), ""),
              "<br>ISI index: ", pv_dat$idx,
              ifelse(is.finite(pv_dat$ISI_sec), paste0("<br>ISI: ", signif(pv_dat$ISI_sec, 6), " s"), ""),
              "<br>Candidate ISI range: ", pv_s, "-", pv_e
            )
            p <- add_segments(
              p,
              data = pv_dat,
              x = ~isi_start_plot, xend = ~isi_end_plot,
              y = ~y, yend = ~y,
              type = "scatter", mode = "lines",
              line = list(width = max(7, raster_label_line_width() + 3), color = pv_col, dash = "solid"),
              hoverinfo = "text",
              text = ~near_miss_text,
              showlegend = FALSE,
              inherit = FALSE
            )
            p <- add_markers(
              p,
              data = pv_dat,
              x = ~isi_mid_plot, y = ~y,
              marker = list(size = 11, color = pv_col, symbol = "diamond", line = list(width = 1.5, color = "#ffffff")),
              hoverinfo = "text",
              text = ~near_miss_text,
              showlegend = FALSE,
              inherit = FALSE
            )
          }
        }

        p <- add_markers(
          p,
          data = if (lod_full) visible_isi else visible_isi[0, , drop = FALSE],
          x = ~isi_mid_plot, y = ~y,
          opacity = 0, marker = list(size = 10),
          showlegend = FALSE, hoverinfo = "text",
          text = ~paste0(
	            "Train: ", train_label_html,
	            "<br>Left spike timestamp: ", round(timestamp_left_sec, 6), " s",
	            "<br>Right spike timestamp: ", round(timestamp_right_sec, 6), " s",
	            ifelse(is.na(ISI_sec), "<br>ISI: NA", paste0("<br>ISI: ", signif(ISI_sec, 6), " s (", round(ISI_plot, 3), " ", u, ")")),
	            ifelse(is.na(ISI_pct), "", paste0("<br>ISI percentile in this train: ", round(ISI_pct, 2), "%")),
            extended_isi_metrics_hover(
              ISI_range_pct_linear,
              ISI_range_pct_log,
              ISI_robust_range_pct_log,
              show = isTRUE(input$show_extended_isi_metrics)
            ),
	            "<br>Final label: ", ifelse(pattern_final == "", "none", pattern_final_html),
	            "<br>Final audit label: ", ifelse(pattern_audit_final == "", "none", pattern_audit_final_html),
	            "<br>Label source: ", label_source_html,
	            ifelse(pattern_final == "possible_burst" & possible_burst_subtype != "", paste0("<br>possible_burst subtype: ", possible_burst_subtype_html), ""),
	            ifelse(pattern_final == "possible_burst" & uncertainty_reason != "", paste0("<br>Uncertainty: ", uncertainty_reason_html), "")
          )
        )
      }
      raster_progress(0.84, sprintf("\u5DF2\u6784\u5EFA %d \u4E2A\u53EF\u89C1 ISI/\u6A21\u5F0F\u533A\u6BB5", nrow(visible_isi)))
    }
	    
    draw_trains <- if (isTRUE(raster_batch_mode)) character(0) else selected
    draw_train_n <- length(draw_trains)
    for (tr_i in seq_along(draw_trains)) {
      tr <- draw_trains[[tr_i]]
      raster_progress(
        0.50 + 0.38 * (tr_i - 1) / max(1, draw_train_n),
        sprintf("\u6B63\u5728\u7ED8\u5236 train %d/%d\uFF1A%s", tr_i, draw_train_n, substr(as.character(tr), 1, 80))
      )
      sub_spike <- dat_plot[dat_plot$train == tr &
                              dat_plot$time_align_sec >= draw_sec_pad[1] &
                              dat_plot$time_align_sec <= draw_sec_pad[2], , drop = FALSE]
      sub_isi <- dat_plot[dat_plot$train == tr &
                            !is.na(dat_plot$isi_start_sec) &
                            dat_plot$isi_end_sec >= draw_sec_pad[1] &
                            dat_plot$isi_start_sec <= draw_sec_pad[2], , drop = FALSE]
      
      if (nrow(sub_spike) > 0) {
        sub_spike$y0 <- sub_spike$y - spike_h_eff / 2
        sub_spike$y1 <- sub_spike$y + spike_h_eff / 2
        
        p <- add_segments(
          p,
          data = sub_spike,
          x = ~time_plot, xend = ~time_plot,
          y = ~y0, yend = ~y1,
          type = "scatter", mode = "lines",
          line = list(width = base_spike_line_width(), color = "#000000", dash = "solid"),
          name = tr, showlegend = TRUE,
          hoverinfo = if (lod_full) "text" else "none",
          text = ~paste0(
            "Train\uFF1A", train_label_html,
            "<br>Spike \u7D22\u5F15\uFF1A", idx,
            "<br>Timestamp\uFF1A", round(timestamp_sec, 6), " s"
          )
        )
        
        p <- add_markers(
          p,
          data = if (lod_interactive) sub_spike else sub_spike[0, , drop = FALSE],
          x = ~time_plot, y = ~y,
          opacity = 0,
          marker = list(size = 12),
          showlegend = FALSE, hoverinfo = "none",
          customdata = ~paste0(train, "__", idx),
          inherit = FALSE
        )
      }
      
      if (nrow(sub_isi) > 0) {
        pats <- c("burst", "long_burst", "possible_burst", "pause", "tonic", "high_frequency_tonic", "high_frequency_spiking")
        if (!isTRUE(input$show_possible)) pats <- setdiff(pats, "possible_burst")
        if (isTRUE(input$show_others)) pats <- c(pats, "others")

        # visual raster semantics:
        #   1) all vertical spike ticks are identical black solid lines;
        #   2) labeled status, source and pattern identity are not encoded by
        #      spike tick darkness/width;
        #   3) thin horizontal strips at the row center encode biological pattern
        #      identity using the manual/auto pattern palette requested by the user.
        label_mode <- "pattern_strip_only"

        add_source_spike_ticks <- function(pp, df, source_kind) {
          if (nrow(df) == 0) return(pp)
          style <- source_spike_style(source_kind)
          if (!is.finite(style$width) || is.na(style$color)) return(pp)
          add_segments(
            pp,
            data = df,
            x = ~time_plot, xend = ~time_plot,
            y = ~y0, yend = ~y1,
            type = "scatter", mode = "lines",
            line = list(width = style$width, color = style$color, dash = style$dash),
            hoverinfo = "none", showlegend = FALSE,
            inherit = FALSE
          )
        }

        add_pattern_strips <- function(pp, df, pat, source_kind) {
          if (nrow(df) == 0) return(pp)
          style <- pattern_strip_style(pat, source = if (identical(source_kind, "manual")) "manual" else "auto")
          add_segments(
            pp,
            data = df,
            x = ~isi_start_plot, xend = ~isi_end_plot,
            y = ~y, yend = ~y,
            type = "scatter", mode = "lines",
            line = list(width = pattern_strip_line_width(), color = style$color, dash = style$dash),
            hoverinfo = "none", showlegend = FALSE,
            inherit = FALSE
          )
        }

        if (identical(label_mode, "source_pattern")) {
          # Source-coded spike ticks. Draw AUTO/REVIEW first, MANUAL last.
          if (identical(input$pattern_view, "final")) {
            sp_auto <- sub_spike[sub_spike$spike_pattern_auto != "" & sub_spike$spike_pattern_manual == "", , drop = FALSE]
            p <- add_source_spike_ticks(p, sp_auto[sp_auto$spike_pattern_auto != "possible_burst", , drop = FALSE], "auto")
            p <- add_source_spike_ticks(p, sp_auto[sp_auto$spike_pattern_auto == "possible_burst", , drop = FALSE], "review")
            sp_manual <- sub_spike[sub_spike$spike_pattern_manual != "", , drop = FALSE]
            p <- add_source_spike_ticks(p, sp_manual[sp_manual$spike_pattern_manual != "possible_burst", , drop = FALSE], "manual")
            p <- add_source_spike_ticks(p, sp_manual[sp_manual$spike_pattern_manual == "possible_burst", , drop = FALSE], "review")
          } else if (identical(input$pattern_view, "manual")) {
            sp_manual <- sub_spike[sub_spike$spike_pattern_manual != "", , drop = FALSE]
            p <- add_source_spike_ticks(p, sp_manual[sp_manual$spike_pattern_manual != "possible_burst", , drop = FALSE], "manual")
            p <- add_source_spike_ticks(p, sp_manual[sp_manual$spike_pattern_manual == "possible_burst", , drop = FALSE], "review")
          } else {
            sp_auto <- sub_spike[sub_spike$spike_pattern_auto != "", , drop = FALSE]
            p <- add_source_spike_ticks(p, sp_auto[sp_auto$spike_pattern_auto != "possible_burst", , drop = FALSE], "auto")
            p <- add_source_spike_ticks(p, sp_auto[sp_auto$spike_pattern_auto == "possible_burst", , drop = FALSE], "review")
          }
        }

        # Pattern-color strips. These are deliberately thin and centered on the
        # row, so they identify the pattern without changing the perceived spike
        # density produced by vertical tick marks.
        if (identical(input$pattern_view, "final")) {
          for (pat in pats) {
            sub_pat <- sub_isi[sub_isi$pattern_auto == pat, , drop = FALSE]
            if (nrow(sub_pat) > 0) p <- add_pattern_strips(p, sub_pat, pat, "auto")
          }
          for (pat in pats) {
            sub_pat <- sub_isi[sub_isi$pattern_manual == pat, , drop = FALSE]
            if (nrow(sub_pat) > 0) p <- add_pattern_strips(p, sub_pat, pat, "manual")
          }
        } else if (identical(input$pattern_view, "manual")) {
          for (pat in pats) {
            sub_pat <- sub_isi[sub_isi$pattern_manual == pat, , drop = FALSE]
            if (nrow(sub_pat) > 0) p <- add_pattern_strips(p, sub_pat, pat, "manual")
          }
        } else if (identical(input$pattern_view, "auto")) {
          for (pat in pats) {
            sub_pat <- sub_isi[sub_isi$pattern_auto == pat, , drop = FALSE]
            if (nrow(sub_pat) > 0) p <- add_pattern_strips(p, sub_pat, pat, "auto")
          }
        } else {
          for (pat in pats) {
            sub_pat <- sub_isi[sub_isi$pattern_show == pat, , drop = FALSE]
            if (nrow(sub_pat) > 0) p <- add_pattern_strips(p, sub_pat, pat, "auto")
          }
        }

        # Always make explicitly MANUAL others visible if requested.
        if (isTRUE(input$show_manual_others_always)) {
          sub_m_oth <- sub_isi[sub_isi$pattern_manual == "others", , drop = FALSE]
          if (nrow(sub_m_oth) > 0) p <- add_pattern_strips(p, sub_m_oth, "others", "manual")
        }

        # seed-bridge threshold-preview overlay. This is visual-only and does not alter labels.
        pv <- rv$preview_candidate
        if (isTRUE(input$show_near_miss_preview) && !is.null(pv) && isTRUE(pv$active) && identical(as.character(pv$train), as.character(tr))) {
          pv_s <- suppressWarnings(as.integer(pv$start_isi))
          pv_e <- suppressWarnings(as.integer(pv$end_isi))
          pv_dat <- sub_isi[sub_isi$idx >= pv_s & sub_isi$idx <= pv_e, , drop = FALSE]
          if (nrow(pv_dat) > 0) {
            pv_col <- switch(
              pv$pattern,
              "burst" = "#c026d3",
              "tonic" = "#65a30d",
              "pause" = "#2563eb",
              "#f97316"
            )
            pv_dat$near_miss_text <- paste0(
              "Selected candidate ISI<br>",
              "Pattern: ", stpd_html_escape(pv$pattern),
              "<br>Source: ", stpd_html_escape(pv$category),
              "<br>Evidence/parameter: ", stpd_html_escape(pv$parameter),
              ifelse(nzchar(as.character(pv$details %||% "")), paste0("<br>Details: ", stpd_html_escape(pv$details)), ""),
              "<br>ISI index: ", pv_dat$idx,
              ifelse(is.finite(pv_dat$ISI_sec), paste0("<br>ISI: ", signif(pv_dat$ISI_sec, 6), " s"), ""),
              "<br>Candidate ISI range: ", pv_s, "-", pv_e
            )
            p <- add_segments(
              p,
              data = pv_dat,
              x = ~isi_start_plot, xend = ~isi_end_plot,
              y = ~y, yend = ~y,
              type = "scatter", mode = "lines",
              line = list(width = max(7, raster_label_line_width() + 3), color = pv_col, dash = "solid"),
              hoverinfo = "text",
              text = ~near_miss_text,
              showlegend = FALSE,
              inherit = FALSE
            )
            p <- add_markers(
              p,
              data = pv_dat,
              x = ~isi_mid_plot, y = ~y,
              marker = list(size = 11, color = pv_col, symbol = "diamond", line = list(width = 1.5, color = "#ffffff")),
              hoverinfo = "text",
              text = ~near_miss_text,
              showlegend = FALSE,
              inherit = FALSE
            )
          }
        }

        if (isTRUE(input$show_burst_sublabel_structures) && !is.null(structure_overlay) && nrow(structure_overlay) > 0) {
          so <- structure_overlay[as.character(structure_overlay$train) == as.character(tr), , drop = FALSE]
          if (nrow(so) > 0 && "burst_sublabel" %in% names(so)) {
            so_sublabel <- as.character(so$burst_sublabel %||% "")
            so_sublabel[is.na(so_sublabel)] <- ""
            so <- so[so_sublabel == "interesting_structure", , drop = FALSE]
          } else {
            so <- so[0, , drop = FALSE]
          }
          if (nrow(so) > 0) {
            so_class <- as.character(so$structure_class %||% "")
            so_class[is.na(so_class)] <- ""
            so <- so[so_class == "burst_associated_regular_packet", , drop = FALSE]
          }
          if (nrow(so) > 0) {
            get_so <- function(df, nm, default = "") {
              if (nm %in% names(df)) df[[nm]] else rep(default, nrow(df))
            }
            so <- utils::head(so[order(suppressWarnings(as.integer(so$start_isi)), suppressWarnings(as.integer(so$end_isi))), , drop = FALSE], 80)
            for (so_i in seq_len(nrow(so))) {
              ss <- suppressWarnings(as.integer(so$start_isi[so_i]))
              ee <- suppressWarnings(as.integer(so$end_isi[so_i]))
              so_dat <- sub_isi[sub_isi$idx >= ss & sub_isi$idx <= ee, , drop = FALSE]
              if (nrow(so_dat) == 0) next
              motif_type <- as.character(get_so(so[so_i, , drop = FALSE], "burst_motif_type", ""))[1]
              linked_label <- as.character(get_so(so[so_i, , drop = FALSE], "linked_burst_label", ""))[1]
              linked_s <- suppressWarnings(as.integer(get_so(so[so_i, , drop = FALSE], "linked_burst_start_isi", NA_integer_))[1])
              linked_e <- suppressWarnings(as.integer(get_so(so[so_i, , drop = FALSE], "linked_burst_end_isi", NA_integer_))[1])
              rr_col <- if (identical(motif_type, "regular_after_burst")) "#111827" else "#7c2d12"
              so_dat$y_sublabel <- so_dat$y + 0.29 * step
              so_text <- paste0(
                "Burst sublabel: interesting_structure<br>",
                "Motif: ", stpd_html_escape(motif_type),
                "<br>Linked burst: ", stpd_html_escape(linked_label), " ISI ", linked_s, "-", linked_e,
                "<br>Packet ISI: ", ss, "-", ee,
                "<br>n spikes: ", as.integer(get_so(so[so_i, , drop = FALSE], "n_spikes", NA_integer_))[1],
                "<br>duration: ", signif(suppressWarnings(as.numeric(get_so(so[so_i, , drop = FALSE], "duration_sec", NA_real_))[1]) * 1000, 5), " ms",
                "<br>median/q90 ISI: ",
                signif(suppressWarnings(as.numeric(get_so(so[so_i, , drop = FALSE], "core_median_ISI_sec", NA_real_))[1]) * 1000, 5),
                " / ",
                signif(suppressWarnings(as.numeric(get_so(so[so_i, , drop = FALSE], "core_q_ISI_sec", NA_real_))[1]) * 1000, 5),
                " ms",
                "<br>CV/LV/MM: ",
                signif(suppressWarnings(as.numeric(get_so(so[so_i, , drop = FALSE], "CV", NA_real_))[1]), 4),
                " / ",
                signif(suppressWarnings(as.numeric(get_so(so[so_i, , drop = FALSE], "LV", NA_real_))[1]), 4),
                " / ",
                signif(suppressWarnings(as.numeric(get_so(so[so_i, , drop = FALSE], "MM", NA_real_))[1]), 4),
                "<br>packet/burst q90 ratio: ",
                signif(suppressWarnings(as.numeric(get_so(so[so_i, , drop = FALSE], "packet_to_burst_q90_ratio", NA_real_))[1]), 4)
              )
              p <- add_segments(
                p, data = so_dat,
                x = ~isi_start_plot, xend = ~isi_end_plot,
                y = ~y_sublabel, yend = ~y_sublabel,
                type = "scatter", mode = "lines",
                line = list(width = 3.2, color = rr_col, dash = "solid"),
                hoverinfo = "text", text = so_text,
                showlegend = FALSE, inherit = FALSE
              )
              p <- add_markers(
                p, data = so_dat[c(1L, nrow(so_dat)), , drop = FALSE],
                x = ~isi_mid_plot, y = ~y_sublabel,
                type = "scatter", mode = "markers",
                marker = list(size = 8, color = "#ffffff", symbol = "square-open", line = list(width = 2, color = rr_col)),
                hoverinfo = "text", text = so_text,
                showlegend = FALSE, inherit = FALSE
              )
            }
          }
        }
        
        if (isTRUE(input$show_rejected_burst_candidates) && !is.null(cand_audit) && nrow(cand_audit) > 0) {
          ca <- cand_audit[as.character(cand_audit$train) == as.character(tr), , drop = FALSE]
          if (nrow(ca) > 0) {
            get_ca <- function(df, nm, default = "") {
              if (nm %in% names(df)) df[[nm]] else rep(default, nrow(df))
            }
            ca_layer <- as.character(get_ca(ca, "candidate_layer", ""))
            ca_label <- as.character(get_ca(ca, "final_label", get_ca(ca, "class", "")))
            ca_status <- as.character(get_ca(ca, "gate_status", ""))
            ca_decision <- as.character(get_ca(ca, "decision_path", get_ca(ca, "failure_reason", "")))
            ca_selected <- suppressWarnings(as.logical(get_ca(ca, "selected_for_auto", FALSE)))
            ca_selected[is.na(ca_selected)] <- FALSE
            keep_ca <- grepl("burst", ca_layer, ignore.case = TRUE) |
                       ca_label %in% c("burst", "long_burst", "possible_burst", "reject") |
                       grepl("burst|q95|q90|bridge|flank|boundary|contrast", ca_decision, ignore.case = TRUE)
            ca <- ca[keep_ca, , drop = FALSE]
            if (nrow(ca) > 0) {
              # Prefer showing non-selected/rejected/downgraded candidates, plus a few selected ones for comparison.
              sel <- suppressWarnings(as.logical(get_ca(ca, "selected_for_auto", FALSE))); sel[is.na(sel)] <- FALSE
              lab2 <- as.character(get_ca(ca, "final_label", ""))
              ca <- ca[(!sel) | lab2 %in% c("possible_burst", "reject"), , drop = FALSE]
            }
            if (nrow(ca) > 0) ca <- utils::head(ca, 180)
            if (nrow(ca) > 0) {
              get_ca <- function(df, nm, default = "") {
                if (nm %in% names(df)) df[[nm]] else rep(default, nrow(df))
              }
              for (rr_i in seq_len(nrow(ca))) {
                rr_s <- suppressWarnings(as.integer(ca$start_isi[rr_i])); rr_e <- suppressWarnings(as.integer(ca$end_isi[rr_i]))
                rr_dat <- sub_isi[sub_isi$idx >= rr_s & sub_isi$idx <= rr_e, , drop = FALSE]
                if (nrow(rr_dat) == 0) next
                rr_lab <- as.character(get_ca(ca[rr_i, , drop = FALSE], "final_label", ""))[1]
                rr_status <- as.character(get_ca(ca[rr_i, , drop = FALSE], "gate_status", ""))[1]
                rr_decision <- as.character(get_ca(ca[rr_i, , drop = FALSE], "decision_path", get_ca(ca[rr_i, , drop = FALSE], "failure_reason", "")))[1]
                rr_q95 <- as.character(get_ca(ca[rr_i, , drop = FALSE], "q95_bridge_pass", ""))[1]
                rr_col <- if (identical(rr_lab, "possible_burst")) "#F2B600" else if (identical(rr_lab, "reject")) "#777777" else if (grepl("q95", rr_decision, ignore.case = TRUE) || identical(rr_q95, "FALSE")) "#E67E22" else "#7B3294"
                rr_dash <- if (identical(rr_lab, "reject")) "dash" else if (identical(rr_lab, "possible_burst")) "dot" else "dashdot"
                rr_dat$y_audit <- rr_dat$y + 0.16 * step
                rr_text <- paste0(
                  "Burst \u5019\u9009\u8BCA\u65AD<br>",
                  "\u6700\u7EC8\u6807\u7B7E\uFF1A", stpd_html_escape(rr_lab), "<br>",
                  "\u72B6\u6001\uFF1A", stpd_html_escape(rr_status), "<br>",
                  "\u539F\u56E0\uFF1A", stpd_html_escape(rr_decision), "<br>",
                  "\u8FB9\u754C\u7C7B\u578B\uFF1A", stpd_html_escape(as.character(get_ca(ca[rr_i, , drop = FALSE], "boundary_type", ""))[1]), "<br>",
                  "ISI\uFF1A", rr_s, "-", rr_e,
                  "<br>intra q90\uFF1A", signif(suppressWarnings(as.numeric(get_ca(ca[rr_i, , drop = FALSE], "intra_q90_sec", NA_real_))[1]) * 1000, 4), " ms",
                  "<br>intra q95\uFF1A", signif(suppressWarnings(as.numeric(get_ca(ca[rr_i, , drop = FALSE], "intra_q95_sec", NA_real_))[1]) * 1000, 4), " ms",
                  "<br>pre/q90\uFF1A", signif(suppressWarnings(as.numeric(get_ca(ca[rr_i, , drop = FALSE], "pre_ratio_q90", NA_real_))[1]), 4),
                  "<br>post/q90\uFF1A", signif(suppressWarnings(as.numeric(get_ca(ca[rr_i, , drop = FALSE], "post_ratio_q90", NA_real_))[1]), 4),
                  "<br>seed \u7EAF\u5EA6\uFF1A", signif(suppressWarnings(as.numeric(get_ca(ca[rr_i, , drop = FALSE], "seed_purity", NA_real_))[1]), 4),
                  "<br>bridge fraction\uFF1A", signif(suppressWarnings(as.numeric(get_ca(ca[rr_i, , drop = FALSE], "bridge_fraction", NA_real_))[1]), 4)
                )
                p <- add_segments(
                  p, data = rr_dat,
                  x = ~isi_start_plot, xend = ~isi_end_plot,
                  y = ~y_audit, yend = ~y_audit,
                  type = "scatter", mode = "lines",
                  line = list(width = 2.5, color = rr_col, dash = rr_dash),
                  hoverinfo = "text", text = rr_text,
                  showlegend = FALSE, inherit = FALSE
                )
              }
            }
          }
        }
        
        p <- add_markers(
          p,
          data = if (lod_full) sub_isi else sub_isi[0, , drop = FALSE],
          x = ~isi_mid_plot, y = ~y,
          opacity = 0, marker = list(size = 10),
          showlegend = FALSE, hoverinfo = "text",
          text = ~paste0(
            "Train\uFF1A", train_label_html,
            "<br>\u5DE6\u4FA7 spike \u65F6\u95F4\u6233\uFF1A", round(timestamp_left_sec, 6), " s",
            "<br>\u53F3\u4FA7 spike \u65F6\u95F4\u6233\uFF1A", round(timestamp_right_sec, 6), " s",
            ifelse(is.na(ISI_sec), "<br>ISI\uFF1ANA", paste0("<br>ISI\uFF1A", signif(ISI_sec, 6), " s (", round(ISI_plot, 3), " ", u, ")")),
            ifelse(is.na(ISI_pct), "", paste0("<br>ISI \u5728\u672C train \u4E2D\u7684\u767E\u5206\u4F4D\uFF1A", round(ISI_pct, 2), "%")),
            extended_isi_metrics_hover(
              ISI_range_pct_linear,
              ISI_range_pct_log,
              ISI_robust_range_pct_log,
              show = isTRUE(input$show_extended_isi_metrics)
            ),
            "<br>\u6700\u7EC8\u6807\u7B7E\uFF1A", ifelse(pattern_final == "", "none", pattern_final_html),
            "<br>\u6700\u7EC8\u5BA1\u8BA1\u6807\u7B7E\uFF1A", ifelse(pattern_audit_final == "", "none", pattern_audit_final_html),
            "<br>\u6807\u7B7E\u6765\u6E90\uFF1A", label_source_html,
            ifelse(pattern_final == "possible_burst" & possible_burst_subtype != "", paste0("<br>possible_burst \u4E9A\u578B\uFF1A", possible_burst_subtype_html), ""),
            ifelse(pattern_final == "possible_burst" & uncertainty_reason != "", paste0("<br>\u4E0D\u786E\u5B9A\u6027\uFF1A", uncertainty_reason_html), "")
          )
        )
	      }
	    }
    if (draw_train_n > 0L) raster_progress(0.88, sprintf("%d \u6761\u53EF\u89C1 train \u5DF2\u7ED8\u5236\u5B8C\u6210", draw_train_n))

	    if (!is.null(delta_overlay) && nrow(delta_overlay) > 0) {
	      delta_styles <- data.frame(
	        status = c("added_event", "removed_event", "label_changed", "boundary_changed"),
	        color = c("#1B9E77", "#D95F02", "#7570B3", "#E6AB02"),
	        dash = c("solid", "dash", "dot", "dashdot"),
	        stringsAsFactors = FALSE
	      )
	      for (ss in delta_styles$status) {
	        dd <- delta_overlay[as.character(delta_overlay$status) == ss, , drop = FALSE]
	        if (nrow(dd) == 0) next
	        st <- delta_styles[delta_styles$status == ss, , drop = FALSE]
	        p <- add_segments(
	          p,
	          data = dd,
	          x = ~x0, xend = ~x1,
	          y = ~y, yend = ~y,
	          type = "scatter", mode = "lines",
	          line = list(width = 4.5, color = st$color[1], dash = st$dash[1]),
	          hoverinfo = "text",
	          text = ~delta_text,
	          showlegend = FALSE,
	          inherit = FALSE
	        )
	      }
	      sel_row <- suppressWarnings(as.integer(rv$parameter_delta_preview_selected_row %||% NA_integer_))
	      if (is.finite(sel_row)) {
	        dd_sel <- delta_overlay[delta_overlay$delta_row_index == sel_row, , drop = FALSE]
	        if (nrow(dd_sel) > 0) {
	          dd_sel$y_sel <- dd_sel$y + 0.08 * step
	          p <- add_segments(
	            p,
	            data = dd_sel,
	            x = ~x0, xend = ~x1,
	            y = ~y_sel, yend = ~y_sel,
	            type = "scatter", mode = "lines",
	            line = list(width = 2.5, color = "#000000", dash = "solid"),
	            hoverinfo = "text",
	            text = ~paste0(delta_text, "<br>\u8868\u683C\u9009\u4E2D\u884C"),
	            showlegend = FALSE,
	            inherit = FALSE
	          )
	        }
	      }
	    }
	    
	    aligned_events <- task_event_overlay_aligned(axis_tbl, draw_sec_pad)
	    if (!is.null(aligned_events) && nrow(aligned_events) > 0L) {
	      aligned_events$x_plot <- aligned_events$x_sec * f
	      aligned_events$y0 <- aligned_events$y - 0.46 * step
	      aligned_events$y1 <- aligned_events$y + 0.46 * step
	      aligned_events$event_text <- paste0(
	        "\u4EFB\u52A1/\u884C\u4E3A\u4E8B\u4EF6<br>",
	        "\u4E8B\u4EF6\uFF1A", stpd_html_escape(aligned_events$event_name),
	        "<br>Raw timestamp\uFF1A", signif(aligned_events$event_time_sec, 7), " s",
	        "<br>Aligned to this train\uFF1A", signif(aligned_events$x_sec, 7), " s",
	        "<br>Train\uFF1A", stpd_html_escape(aligned_events$train_label)
	      )
	      p <- add_segments(
	        p,
	        data = aligned_events,
	        x = ~x_plot, xend = ~x_plot,
	        y = ~y0, yend = ~y1,
	        type = "scatter", mode = "lines",
	        line = list(width = 1.8, color = "#0f766e", dash = "dash"),
	        hoverinfo = "text",
	        text = ~event_text,
	        showlegend = FALSE,
	        inherit = FALSE
	      )
	      p <- add_markers(
	        p,
	        data = aligned_events,
	        x = ~x_plot, y = ~y,
	        marker = list(size = 7, color = "#0f766e", symbol = "diamond-open", line = list(width = 1.6, color = "#0f766e")),
	        hoverinfo = "text",
	        text = ~event_text,
	        showlegend = FALSE,
	        inherit = FALSE
	      )
	    }

	    p <- layout(
      p,
      hoverlabel = stpd_hoverlabel_style(),
      showlegend = FALSE,
      uirevision = "keep_align_view",
      dragmode = "select",
      xaxis = list(title = paste0("\u5BF9\u9F50\u65F6\u95F4\uFF08", u, ")"), range = x_use),
      yaxis = list(title = list(text = "Spike train\uFF08\u8BB0\u5F55\u6761\u76EE\uFF09", standoff = 40), tickmode = "array",
                   tickvals = y_tickvals, ticktext = y_ticktext, tickfont = list(size = tick_font_size),
                   range = y_range, zeroline = FALSE, automargin = TRUE),
      margin = list(l = 220, r = 20, t = 40, b = 50),
      hovermode = "closest",
      annotations = if (!lod_full) list(list(xref = "paper", yref = "paper", x = 0.01, y = 1.03, text = lod_note, showarrow = FALSE, xanchor = "left", font = list(size = 11))) else NULL
    )
	    raster_progress(0.92, "\u6B63\u5728\u5E94\u7528 Plotly layout \u548C\u4EA4\u4E92\u4E8B\u4EF6")
	    p <- event_register(p, "plotly_selected")
	    p <- event_register(p, "plotly_relayout")
	    raster_progress(1, "plot \u89C6\u56FE\u5DF2\u5B8C\u6210", "success")
	    if (isTRUE(show_raster_progress)) rv$raster_plot_progress_active <- FALSE
	    config(p, displaylogo = FALSE)
    }
    if (isTRUE(show_raster_progress)) {
      withProgress(message = "\u6B63\u5728\u52A0\u8F7D plot \u89C6\u56FE", value = 0, {
        build_raster_plot()
      })
    } else {
      build_raster_plot()
    }
  })
  
  # ----------------------------------------------------------
  # Original timestamp plot
  # ----------------------------------------------------------
  output$raster_raw_plot <- renderPlotly({
    if (length(rv$datasets) == 0L) {
      return(stpd_empty_plotly_message("\u8BF7\u5148\u4E0A\u4F20\u4E00\u4E2A\u6570\u636E\u96C6\u3002"))
    }
    withProgress(message = "\u6B63\u5728\u52A0\u8F7D\u539F\u59CB timestamp plot", value = 0, {
      setProgress(0.05, detail = "\u6B63\u5728\u51C6\u5907\u539F\u59CB timestamp \u6570\u636E")
      ds <- current_dataset()
      td <- ds$trains
      axis_tbl <- selected_axis_table() %>% dplyr::arrange(y)
      if (nrow(axis_tbl) == 0L) return(stpd_empty_plotly_message("\u8BF7\u81F3\u5C11\u9009\u62E9\u4E00\u6761 train\u3002"))
      f <- unit_factor()
      u <- input$time_unit %||% "s"
      step <- track_step()
      draw_sec <- raw_window_for_plot()
      pad_sec <- max(0.002, diff(draw_sec) * raster_prefetch_fraction())
      draw_sec_pad <- c(draw_sec[1] - pad_sec, draw_sec[2] + pad_sec)
      min_isi <- min_valid_isi_sec()
      auto_others_on <- isTRUE(input$auto_others)

      rows <- list()
      for (ii in seq_len(nrow(axis_tbl))) {
        tr <- as.character(axis_tbl$train[ii])
        dat <- td[[tr]]
        if (is.null(dat) || nrow(dat) == 0L || !("timestamp_sec" %in% names(dat))) next
        dat <- ensure_train_isi_percentiles(dat, min_isi)
        n <- nrow(dat)
        ts <- suppressWarnings(as.numeric(dat$timestamp_sec))
        prev_ts <- c(NA_real_, head(ts, -1L))
        spike_idx <- which(is.finite(ts) & ts >= draw_sec_pad[1] & ts <= draw_sec_pad[2])
        isi_idx <- which(seq_len(n) >= 2L & is.finite(ts) & is.finite(prev_ts) &
                           ts >= draw_sec_pad[1] & prev_ts <= draw_sec_pad[2])
        keep <- sort(unique(c(spike_idx, isi_idx, pmax(1L, isi_idx - 1L), pmin(n, spike_idx + 1L))))
        if (length(keep) == 0L) next
        manual_all <- as.character(dat$pattern_manual %||% rep("", n)); manual_all[is.na(manual_all)] <- ""
        auto_all <- as.character(dat$pattern_auto %||% rep("", n)); auto_all[is.na(auto_all)] <- ""
        final_all <- compute_final_pattern(manual_all, auto_all, dat$ISI_sec,
                                           auto_others = auto_others_on,
                                           min_isi_sec = min_isi)
        audit_final_all <- stpd_audit_final_labels(dat, min_isi_sec = min_isi,
                                                   auto_others = auto_others_on,
                                                   prefer_stored = TRUE)
        pattern_view_mode <- input$pattern_view %||% "audit_final"
        pat_show <- switch(pattern_view_mode,
                           manual = manual_all[keep],
                           auto = auto_all[keep],
                           final = final_all[keep],
                           audit_final = audit_final_all[keep])
        if (is.null(pat_show)) pat_show <- final_all[keep]
        label_source <- ifelse(manual_all[keep] != "", "manual",
                               ifelse(auto_all[keep] != "", "auto",
                                      ifelse(as.character(final_all[keep]) != "", "implicit_final", "none")))
        get_num_col <- function(nm) {
          if (nm %in% names(dat)) suppressWarnings(as.numeric(dat[[nm]][keep])) else rep(NA_real_, length(keep))
        }
        rows[[length(rows) + 1L]] <- data.frame(
          train = tr,
          train_label = as.character(axis_tbl$train_label[ii]),
          train_order = axis_tbl$train_order[ii],
          y = axis_tbl$y[ii],
          idx = dat$idx[keep],
          timestamp_sec = ts[keep],
          timestamp_left_sec = prev_ts[keep],
          ISI_sec = suppressWarnings(as.numeric(dat$ISI_sec[keep])),
          ISI_pct = get_num_col("ISI_pct"),
          ISI_range_pct_linear = get_num_col("ISI_range_pct_linear"),
          ISI_range_pct_log = get_num_col("ISI_range_pct_log"),
          ISI_robust_range_pct_log = get_num_col("ISI_robust_range_pct_log"),
          pattern_manual = manual_all[keep],
          pattern_auto = auto_all[keep],
          pattern_final = final_all[keep],
          pattern_audit_final = audit_final_all[keep],
          pattern_show = pat_show,
          label_source = label_source,
          stringsAsFactors = FALSE
        )
      }
      dat_all <- dplyr::bind_rows(rows)
      if (nrow(dat_all) == 0L) {
        setProgress(1, detail = "\u5F53\u524D\u539F\u59CB\u65F6\u95F4\u7A97\u6CA1\u6709 spike/ISI")
        return(stpd_empty_plotly_message("\u5F53\u524D\u539F\u59CB\u65F6\u95F4\u7A97\u6CA1\u6709 spike/ISI\u3002"))
      }

      setProgress(0.32, detail = "\u6B63\u5728\u540C\u6B65\u6A21\u5F0F\u6807\u7B7E\u5230\u539F\u59CB\u65F6\u95F4")
      dat_all <- dat_all %>%
        group_by(train) %>%
        arrange(idx, .by_group = TRUE) %>%
        mutate(
          pattern_manual_chr = as.character(pattern_manual %||% ""),
          pattern_auto_chr = as.character(pattern_auto %||% ""),
          pattern_final_chr = as.character(pattern_final %||% ""),
          pattern_audit_final_chr = as.character(.data$pattern_audit_final),
          spike_pattern_manual = ifelse(pattern_manual_chr != "", pattern_manual_chr,
                                        dplyr::lead(pattern_manual_chr, default = "")),
          spike_pattern_auto = ifelse(pattern_auto_chr != "", pattern_auto_chr,
                                      dplyr::lead(pattern_auto_chr, default = "")),
          spike_pattern_final = ifelse(pattern_final_chr != "", pattern_final_chr,
                                       dplyr::lead(pattern_final_chr, default = "")),
          spike_pattern_audit_final = ifelse(.data$pattern_audit_final_chr != "", .data$pattern_audit_final_chr,
                                             dplyr::lead(.data$pattern_audit_final_chr, default = "")),
          isi_mid_sec = (timestamp_left_sec + timestamp_sec) / 2
        ) %>%
        ungroup() %>%
        mutate(
          time_plot = timestamp_sec * f,
          timestamp_left_plot = timestamp_left_sec * f,
          timestamp_right_plot = timestamp_sec * f,
          isi_mid_plot = isi_mid_sec * f,
          ISI_plot = ISI_sec * f,
          train_label_html = stpd_html_escape(train_label),
          pattern_final_html = stpd_html_escape(pattern_final),
          pattern_audit_final_html = stpd_html_escape(.data$pattern_audit_final),
          label_source_html = stpd_html_escape(label_source)
        )

      visible_spike <- dat_all[dat_all$timestamp_sec >= draw_sec_pad[1] & dat_all$timestamp_sec <= draw_sec_pad[2], , drop = FALSE]
      visible_isi <- dat_all[!is.na(dat_all$timestamp_left_sec) &
                               dat_all$timestamp_sec >= draw_sec_pad[1] &
                               dat_all$timestamp_left_sec <= draw_sec_pad[2], , drop = FALSE]
      k <- nrow(axis_tbl)
      y_tickvals <- axis_tbl$y
      y_ticktext <- stpd_html_escape(axis_tbl$train_label)
      y_range <- c(min(axis_tbl$y, na.rm = TRUE) - 0.5 * step, max(axis_tbl$y, na.rm = TRUE) + 0.5 * step)
      spike_h_eff <- min(input$spike_height, max(0.10, 0.90 * step))
      tick_font_size <- if (k >= 10) 10 else if (k >= 8) 11 else 12
      p <- plot_ly(source = "raster_raw")

      if (nrow(visible_spike) > 0L) {
        visible_spike$y0 <- visible_spike$y - spike_h_eff / 2
        visible_spike$y1 <- visible_spike$y + spike_h_eff / 2
        p <- add_segments(
          p,
          data = visible_spike,
          x = ~time_plot, xend = ~time_plot,
          y = ~y0, yend = ~y1,
          type = "scatter", mode = "lines",
          line = list(width = base_spike_line_width(), color = "#000000", dash = "solid"),
          name = "spikes", showlegend = FALSE,
          hoverinfo = if (k >= 12L && nrow(visible_spike) > 8000L) "none" else "text",
          text = ~paste0(
            "Train: ", train_label_html,
            "<br>Spike index: ", idx,
            "<br>Raw timestamp: ", round(timestamp_sec, 6), " s"
          ),
          inherit = FALSE
        )
      }

      if (nrow(visible_isi) > 0L) {
        pats <- c("burst", "long_burst", "possible_burst", "pause", "tonic", "high_frequency_tonic", "high_frequency_spiking")
        if (!isTRUE(input$show_possible)) pats <- setdiff(pats, "possible_burst")
        if (isTRUE(input$show_others)) pats <- c(pats, "others")
        add_pattern_strips <- function(pp, df, pat, source_kind) {
          if (nrow(df) == 0L) return(pp)
          style <- pattern_strip_style(pat, source = if (identical(source_kind, "manual")) "manual" else "auto")
          add_segments(
            pp,
            data = df,
            x = ~timestamp_left_plot, xend = ~timestamp_right_plot,
            y = ~y, yend = ~y,
            type = "scatter", mode = "lines",
            line = list(width = pattern_strip_line_width(), color = style$color, dash = style$dash),
            hoverinfo = "none", showlegend = FALSE,
            inherit = FALSE
          )
        }
        if (identical(input$pattern_view, "final")) {
          for (pat in pats) {
            sub_pat <- visible_isi[visible_isi$pattern_auto == pat, , drop = FALSE]
            if (nrow(sub_pat) > 0L) p <- add_pattern_strips(p, sub_pat, pat, "auto")
          }
          for (pat in pats) {
            sub_pat <- visible_isi[visible_isi$pattern_manual == pat, , drop = FALSE]
            if (nrow(sub_pat) > 0L) p <- add_pattern_strips(p, sub_pat, pat, "manual")
          }
        } else if (identical(input$pattern_view, "manual")) {
          for (pat in pats) {
            sub_pat <- visible_isi[visible_isi$pattern_manual == pat, , drop = FALSE]
            if (nrow(sub_pat) > 0L) p <- add_pattern_strips(p, sub_pat, pat, "manual")
          }
        } else if (identical(input$pattern_view, "auto")) {
          for (pat in pats) {
            sub_pat <- visible_isi[visible_isi$pattern_auto == pat, , drop = FALSE]
            if (nrow(sub_pat) > 0L) p <- add_pattern_strips(p, sub_pat, pat, "auto")
          }
        } else {
          for (pat in pats) {
            sub_pat <- visible_isi[visible_isi$pattern_show == pat, , drop = FALSE]
            if (nrow(sub_pat) > 0L) p <- add_pattern_strips(p, sub_pat, pat, "auto")
          }
        }
        if (isTRUE(input$show_manual_others_always)) {
          sub_m_oth <- visible_isi[visible_isi$pattern_manual == "others", , drop = FALSE]
          if (nrow(sub_m_oth) > 0L) p <- add_pattern_strips(p, sub_m_oth, "others", "manual")
        }
        p <- add_markers(
          p,
          data = visible_isi,
          x = ~isi_mid_plot, y = ~y,
          opacity = 0,
          marker = list(size = 10),
          showlegend = FALSE,
          hoverinfo = "text",
          text = ~paste0(
            "Train: ", train_label_html,
            "<br>Left spike raw timestamp: ", round(timestamp_left_sec, 6), " s",
            "<br>Right spike raw timestamp: ", round(timestamp_sec, 6), " s",
            ifelse(is.na(ISI_sec), "<br>ISI: NA", paste0("<br>ISI: ", signif(ISI_sec, 6), " s (", round(ISI_plot, 3), " ", u, ")")),
            ifelse(is.na(ISI_pct), "", paste0("<br>ISI percentile in this train: ", round(ISI_pct, 2), "%")),
            extended_isi_metrics_hover(
              ISI_range_pct_linear,
              ISI_range_pct_log,
              ISI_robust_range_pct_log,
              show = isTRUE(input$show_extended_isi_metrics)
            ),
            "<br>Final label: ", ifelse(pattern_final == "", "none", pattern_final_html),
            "<br>Final audit label: ", ifelse(pattern_audit_final == "", "none", pattern_audit_final_html),
            "<br>Label source: ", label_source_html
          ),
          inherit = FALSE
        )
      }

      raw_events <- task_event_overlay_raw(draw_sec_pad)
      if (!is.null(raw_events) && nrow(raw_events) > 0L) {
        raw_events$x_plot <- raw_events$event_time_sec * f
        raw_events$y0 <- y_range[1]
        raw_events$y1 <- y_range[2]
        raw_events$event_text <- paste0(
          "\u4EFB\u52A1/\u884C\u4E3A\u4E8B\u4EF6<br>",
          "\u4E8B\u4EF6\uFF1A", stpd_html_escape(raw_events$event_name),
          "<br>Raw timestamp\uFF1A", signif(raw_events$event_time_sec, 7), " s"
        )
        p <- add_segments(
          p,
          data = raw_events,
          x = ~x_plot, xend = ~x_plot,
          y = ~y0, yend = ~y1,
          type = "scatter", mode = "lines",
          line = list(width = 1.8, color = "#0f766e", dash = "dash"),
          hoverinfo = "text",
          text = ~event_text,
          showlegend = FALSE,
          inherit = FALSE
        )
        label_events <- raw_events[!duplicated(raw_events$event_id), , drop = FALSE]
        label_events$y_label <- y_range[2] - 0.05 * diff(y_range)
        p <- add_markers(
          p,
          data = label_events,
          x = ~x_plot, y = ~y_label,
          marker = list(size = 7, color = "#0f766e", symbol = "diamond-open", line = list(width = 1.6, color = "#0f766e")),
          hoverinfo = "text",
          text = ~event_text,
          showlegend = FALSE,
          inherit = FALSE
        )
      }

      p <- layout(
        p,
        hoverlabel = stpd_hoverlabel_style(),
        showlegend = FALSE,
        uirevision = "keep_raw_view",
        xaxis = list(title = paste0("\u539F\u59CB timestamp\uFF08", u, "\uFF09"), range = draw_sec * f),
        yaxis = list(title = list(text = "Spike train\uFF08\u8BB0\u5F55\u6761\u76EE\uFF09", standoff = 40), tickmode = "array",
                     tickvals = y_tickvals, ticktext = y_ticktext, tickfont = list(size = tick_font_size),
                     range = y_range, zeroline = FALSE, automargin = TRUE),
        margin = list(l = 220, r = 20, t = 40, b = 50),
        hovermode = "closest"
      )
      setProgress(1, detail = "\u539F\u59CB timestamp plot \u5DF2\u5B8C\u6210")
      config(p, displaylogo = FALSE)
    })
  })
  
  # ----------------------------------------------------------
  # ISI temporal profile diagnostic
  # ----------------------------------------------------------
  output$isi_profile_train_selector <- renderUI({
    td <- current_trains()
    choices <- names(td)
    if (length(choices) == 0) return(NULL)
    mode <- input$isi_profile_display_mode %||% "multi"
    default <- intersect(displayed_train_names(), choices)
    if (length(default) == 0) default <- head(choices, 1)
    if (identical(mode, "focused")) {
      return(selectInput("isi_profile_train", "\u805A\u7126 train", choices = choices, selected = default[1]))
    }
    max_n <- safe_int(input$isi_profile_max_trains, 8L)
    max_n <- max(1L, min(10L, max_n))
    selectizeInput("isi_profile_trains_multi", "\u4EE5\u72EC\u7ACB\u9762\u677F\u663E\u793A\u7684 trains",
                   choices = choices,
                   selected = head(default, max_n),
                   multiple = TRUE,
                   options = list(maxItems = 10, placeholder = "\u8BF7\u9009\u62E9 5\u201310 \u6761 train \u4EE5\u4F7F\u7528\u72EC\u7ACB\u9762\u677F"))
  })

  isi_profile_selected_trains <- reactive({
    td <- current_trains()
    choices <- names(td)
    if (length(choices) == 0) return(character(0))
    mode <- input$isi_profile_display_mode %||% "multi"
    if (identical(mode, "focused")) {
      tr <- input$isi_profile_train
      if (is.null(tr) || !(tr %in% choices)) tr <- intersect(displayed_train_names(), choices)[1]
      if (is.null(tr) || !is.finite(match(tr, choices))) tr <- choices[1]
      return(as.character(tr))
    }
    max_n <- safe_int(input$isi_profile_max_trains, 8L)
    max_n <- max(1L, min(10L, max_n))
    sel <- intersect(input$isi_profile_trains_multi %||% character(0), choices)
    if (length(sel) == 0) sel <- intersect(displayed_train_names(), choices)
    if (length(sel) == 0) sel <- head(choices, max_n)
    head(sel, max_n)
  })

  output$isi_profile_custom_window_ui <- renderUI({
    if (!identical(input$isi_profile_time_range_mode %||% "full", "custom")) return(NULL)
    td <- current_trains()
    sel <- intersect(isi_profile_selected_trains(), names(td))
    if (length(sel) == 0) return(NULL)
    starts <- vapply(sel, function(tr) {
      dat <- td[[tr]]
      if (is.null(dat) || nrow(dat) < 2 || !("timestamp_sec" %in% names(dat))) return(NA_real_)
      ts <- suppressWarnings(as.numeric(dat$timestamp_sec))
      ts <- ts[is.finite(ts)]
      if (length(ts) == 0) NA_real_ else min(ts)
    }, numeric(1))
    ends <- vapply(sel, function(tr) {
      dat <- td[[tr]]
      if (is.null(dat) || nrow(dat) < 2 || !("timestamp_sec" %in% names(dat))) return(NA_real_)
      ts <- suppressWarnings(as.numeric(dat$timestamp_sec))
      ts <- ts[is.finite(ts)]
      if (length(ts) == 0) NA_real_ else max(ts)
    }, numeric(1))
    if (!any(is.finite(starts)) || !any(is.finite(ends))) return(NULL)
    min_t <- min(starts[is.finite(starts)], na.rm = TRUE)
    max_t <- max(ends[is.finite(ends)], na.rm = TRUE)
    if (!is.finite(min_t) || !is.finite(max_t) || max_t <= min_t) return(NULL)
    # Keep the ISI temporal-profile Time X-axis in the same unit as the
    # underlying spike timestamps: seconds.  The global Display unit still
    # controls ISI values on the Y-axis and train-specific ISI thresholds.
    min_plot <- floor(min_t * 1000) / 1000
    max_plot <- ceiling(max_t * 1000) / 1000
    if (!is.finite(min_plot) || !is.finite(max_plot) || max_plot <= min_plot) return(NULL)
    default_width <- min(max_plot - min_plot, 1)
    default_end <- min(max_plot, min_plot + default_width)
    sliderInput("isi_profile_custom_range", "\u81EA\u5B9A\u4E49\u5256\u9762\u65F6\u95F4\u6233\u7A97\u53E3\uFF08s\uFF09",
                min = min_plot, max = max_plot, value = c(min_plot, default_end), step = 0.001)
  })

  output$isi_profile_plot_ui <- renderUI({
    n <- length(isi_profile_selected_trains())
    mode <- input$isi_profile_display_mode %||% "multi"
    height_px <- if (identical(mode, "focused")) 650 else max(650, min(10L, max(1L, n)) * 260)
    plotlyOutput("isi_profile_plot", height = paste0(height_px, "px"))
  })

  build_isi_profile_train_data <- function(tr, dat) {
    dat <- ensure_train_isi_percentiles(dat, min_valid_isi_sec())
    if (is.null(dat) || nrow(dat) < 2) return(data.frame())
    dat <- dat %>% arrange(idx)
    t0 <- suppressWarnings(as.numeric(dat$timestamp_sec[1]))
    t_end <- suppressWarnings(as.numeric(dat$timestamp_sec[nrow(dat)]))
    left_time <- c(NA_real_, suppressWarnings(as.numeric(dat$timestamp_sec[-nrow(dat)])))
    right_time <- suppressWarnings(as.numeric(dat$timestamp_sec))
    out <- data.frame(
      train = tr,
      idx = dat$idx,
      left_idx = dat$idx - 1L,
      right_idx = dat$idx,
      train_start_sec = t0,
      train_end_sec = t_end,
      left_time_sec = left_time,
      right_time_sec = right_time,
      left_align_sec = left_time - t0,
      right_align_sec = right_time - t0,
      train_duration_sec = t_end - t0,
      ISI_sec = suppressWarnings(as.numeric(dat$ISI_sec)),
      ISI_pct = suppressWarnings(as.numeric(dat$ISI_pct)),
      ISI_range_pct_linear = suppressWarnings(as.numeric(dat$ISI_range_pct_linear %||% rep(NA_real_, nrow(dat)))),
      ISI_range_pct_log = suppressWarnings(as.numeric(dat$ISI_range_pct_log %||% rep(NA_real_, nrow(dat)))),
      ISI_robust_range_pct_log = suppressWarnings(as.numeric(dat$ISI_robust_range_pct_log %||% rep(NA_real_, nrow(dat)))),
      pattern_manual = dat$pattern_manual %||% "",
      pattern_auto = dat$pattern_auto %||% "",
      pattern_final = compute_final_pattern(dat$pattern_manual, dat$pattern_auto, dat$ISI_sec,
                                            auto_others = isTRUE(input$auto_others),
                                            min_isi_sec = min_valid_isi_sec()),
      stringsAsFactors = FALSE
    )
    out <- out[is.finite(out$ISI_sec) & out$ISI_sec >= min_valid_isi_sec() & out$idx >= 2, , drop = FALSE]
    if (nrow(out) == 0) return(out)
    out$mid_time_sec <- (out$left_time_sec + out$right_time_sec) / 2
    out$mid_align_sec <- (out$left_align_sec + out$right_align_sec) / 2
    out$isi_index <- seq_len(nrow(out))
    out
  }

  isi_profile_data <- reactive({
    td <- current_trains()
    sel <- intersect(isi_profile_selected_trains(), names(td))
    validate(need(length(sel) > 0, "\u8BF7\u81F3\u5C11\u9009\u62E9\u4E00\u6761 spike train \u7528\u4E8E ISI \u5256\u9762\u3002"))
    out <- bind_rows(lapply(sel, function(tr) build_isi_profile_train_data(tr, td[[tr]])))
    validate(need(nrow(out) > 0, "\u6240\u9009 train \u6CA1\u6709\u6709\u6548 ISI\u3002"))
    out
  })

  observeEvent({
    if (length(rv$datasets) == 0L) {
      NULL
    } else if (!identical(input$main_tabs %||% "", "ISI \u65F6\u95F4\u5256\u9762")) {
      NULL
    } else {
      stpd_safe_plotly_event_data("plotly_click", source = "isi_profile")
    }
  }, {
    ev <- stpd_safe_plotly_event_data("plotly_click", source = "isi_profile")
    if (is.null(ev) || nrow(ev) == 0 || is.null(ev$customdata)) return()
    parts <- strsplit(as.character(ev$customdata[1]), "\\|", fixed = FALSE)[[1]]
    if (length(parts) < 3) return()
    rv$isi_profile_ref <- list(train = parts[1], idx = suppressWarnings(as.integer(parts[2])), isi_sec = suppressWarnings(as.numeric(parts[3])))
  }, ignoreInit = TRUE)

  observeEvent(input$clear_isi_profile_ref, {
    rv$isi_profile_ref <- NULL
  })

  output$isi_profile_ref_text <- renderText({
    ref <- rv$isi_profile_ref
    if (is.null(ref) || !is.finite(ref$isi_sec)) return("\u6CA1\u6709\u9501\u5B9A\u7684\u53C2\u8003 ISI\u3002")
    paste0("\u53C2\u8003 train\uFF1A", ref$train, "\nISI idx: ", ref$idx, "\nReference ISI\uFF1A", signif(ref$isi_sec, 6), " s")
  })


	  isi_threshold_targets <- reactive({
	    td <- current_trains()
      choices <- names(td)
      scope <- input$isi_threshold_apply_scope %||% "profile"
      if (identical(scope, "all")) return(choices)
      if (identical(scope, "custom")) {
        return(intersect(input$isi_threshold_apply_trains %||% character(0), choices))
      }
	    sel <- isi_profile_selected_trains()
	    intersect(sel, choices)
	  })

    output$isi_threshold_apply_trains_ui <- renderUI({
      if (!identical(input$isi_threshold_apply_scope %||% "profile", "custom")) return(NULL)
      td <- current_trains()
      choices <- names(td)
      selected <- intersect(input$isi_threshold_apply_trains %||% isi_profile_selected_trains(), choices)
      selectizeInput("isi_threshold_apply_trains", "\u81EA\u9009\u5E94\u7528 train(s)",
                     choices = choices, selected = selected, multiple = TRUE,
                     options = list(placeholder = "\u9009\u62E9\u8981\u5E94\u7528\u8FD9\u4E9B\u9608\u503C\u7EBF\u7684 spike train", plugins = list("remove_button")))
    })

    isi_profile_ref_to_threshold <- function(input_id) {
      ref <- rv$isi_profile_ref
      if (is.null(ref) || !is.finite(ref$isi_sec) || ref$isi_sec <= 0) {
        showNotification("\u8BF7\u5148\u5728 ISI \u65F6\u95F4\u5256\u9762\u56FE\u4E2D\u70B9\u51FB\u4E00\u4E2A ISI \u4F5C\u4E3A\u53C2\u8003\u7EBF\u3002", type = "warning", duration = 6)
        return(invisible(NULL))
      }
      updateNumericInput(session, input_id, value = round(ref$isi_sec * unit_factor(), 6))
      invisible(NULL)
    }

    observeEvent(input$isi_ref_to_burst, isi_profile_ref_to_threshold("train_thr_burst_max"), ignoreInit = TRUE)
    observeEvent(input$isi_ref_to_pause, isi_profile_ref_to_threshold("train_thr_pause_min"), ignoreInit = TRUE)
    observeEvent(input$isi_ref_to_tonic_min, isi_profile_ref_to_threshold("train_thr_tonic_min"), ignoreInit = TRUE)
    observeEvent(input$isi_ref_to_tonic_max, isi_profile_ref_to_threshold("train_thr_tonic_max"), ignoreInit = TRUE)

	  observeEvent(list(input$isi_profile_train, input$isi_profile_display_mode, input$time_unit, input$isi_threshold_apply_scope, input$isi_threshold_apply_trains, rv$current_id), {
	    ds <- get_dataset()
	    if (is.null(ds)) return()
	    sel <- intersect(isi_profile_selected_trains(), names(ds$trains))
      if (length(sel) == 0) sel <- isi_threshold_targets()
	    if (length(sel) == 0) return()
	    thr <- ds$train_settings$isi_thresholds[[sel[1]]] %||% list()
	    f <- unit_factor()
	    updateNumericInput(session, "train_thr_burst_max", value = round((thr$burst_max_sec %||% 0) * f, 6))
	    updateNumericInput(session, "train_thr_pause_min", value = round((thr$pause_min_sec %||% 0) * f, 6))
	    updateNumericInput(session, "train_thr_tonic_min", value = round((thr$tonic_min_sec %||% 0) * f, 6))
	    updateNumericInput(session, "train_thr_tonic_max", value = round((thr$tonic_max_sec %||% 0) * f, 6))
      updateRadioButtons(session, "isi_threshold_mode", selected = stpd_train_isi_threshold_mode(thr))
	  }, ignoreInit = FALSE)

    save_train_isi_thresholds_from_ui <- function(run_after = FALSE) {
	    ds <- get_dataset(); if (is.null(ds)) return()
	    targets <- isi_threshold_targets()
	    if (length(targets) == 0) return(showNotification("\u672A\u9009\u62E9\u5256\u9762 train\u3002", type = "warning"))
	    f <- unit_factor()
      mode <- input$isi_threshold_mode %||% "soft_anchor"
      if (!mode %in% c("soft_anchor", "hard_threshold")) mode <- "soft_anchor"
      bmax <- max(0, safe_ui_value(input$train_thr_burst_max, 0) / f)
      pmin <- max(0, safe_ui_value(input$train_thr_pause_min, 0) / f)
      tmin <- max(0, safe_ui_value(input$train_thr_tonic_min, 0) / f)
      tmax <- max(0, safe_ui_value(input$train_thr_tonic_max, 0) / f)
      if (bmax > 0 && tmin > 0 && tmin <= bmax) {
        tmin <- bmax * 1.15
        updateNumericInput(session, "train_thr_tonic_min", value = round(tmin * f, 6))
        showNotification("\u5DF2\u81EA\u52A8\u5C06 tonic \u4E0B\u754C\u63D0\u9AD8\u5230 burst line \u4E4B\u4E0A\uFF0C\u907F\u514D tonic \u4E0E burst ISI \u533A\u95F4\u4EA4\u53C9\u3002", type = "warning", duration = 8)
      }
      if (tmin > 0 && tmax > 0 && tmax <= tmin) {
        showNotification("tonic \u6700\u5927 ISI \u5FC5\u987B\u5927\u4E8E tonic \u6700\u5C0F ISI\u3002", type = "error", duration = 8)
        return(invisible(NULL))
      }
      if (bmax <= 0 && pmin <= 0 && tmin <= 0 && tmax <= 0) {
        showNotification("\u8BF7\u81F3\u5C11\u8BBE\u7F6E\u4E00\u6761 burst/tonic/pause \u9608\u503C\u7EBF\u3002", type = "warning", duration = 6)
        return(invisible(NULL))
      }
	    vals <- list(
	      burst_max_sec = bmax,
	      pause_min_sec = pmin,
	      tonic_min_sec = tmin,
	      tonic_max_sec = tmax,
        threshold_mode = mode,
        hard_threshold = identical(mode, "hard_threshold"),
	      source = if (identical(mode, "hard_threshold")) "ui_isi_profile_threshold_line" else "isi_profile_threshold_line_soft_anchor",
        scope = input$isi_threshold_apply_scope %||% "profile",
	      updated_at = as.character(Sys.time())
	    )
	    if (is.null(ds$train_settings)) ds$train_settings <- list()
	    if (is.null(ds$train_settings$isi_thresholds)) ds$train_settings$isi_thresholds <- list()
	    for (tr in targets) ds$train_settings$isi_thresholds[[tr]] <- vals
	    set_dataset(rv$current_id, ds)
      showNotification(paste0("\u5DF2\u4E3A ", length(targets), " \u6761 train \u4FDD\u5B58 ISI \u9608\u503C\u7EBF\uFF08", mode, "\uFF09\u3002"), type = "message")
      if (isTRUE(run_after) || isTRUE(input$run_detector_after_train_isi_thresholds)) {
        runner <- get0("run_detector_from_ui", mode = "function", inherits = TRUE)
        if (is.function(runner)) {
          runner(message = "\u6B63\u5728\u6309 ISI \u9608\u503C\u7EBF\u91CD\u8DD1\u68C0\u6D4B\u5668", switch_to_plot = TRUE, notify = TRUE,
                 target_trains_override = targets)
        } else {
          showNotification("\u9608\u503C\u5DF2\u4FDD\u5B58\uFF0C\u4F46\u5F53\u524D session \u4E2D\u6CA1\u6709\u53EF\u7528\u7684\u68C0\u6D4B\u5668 runner\u3002", type = "warning", duration = 8)
        }
      }
      invisible(targets)
    }

	  observeEvent(input$save_train_isi_thresholds, {
      save_train_isi_thresholds_from_ui(run_after = FALSE)
	  }, ignoreInit = TRUE)

    observeEvent(input$apply_train_isi_thresholds_and_run, {
      save_train_isi_thresholds_from_ui(run_after = TRUE)
    }, ignoreInit = TRUE)

  observeEvent(input$clear_train_isi_thresholds, {
    ds <- get_dataset(); if (is.null(ds)) return()
    targets <- isi_threshold_targets()
    if (length(targets) == 0) return(showNotification("\u672A\u9009\u62E9\u5256\u9762 train\u3002", type = "warning"))
    if (is.null(ds$train_settings)) ds$train_settings <- list()
    if (is.null(ds$train_settings$isi_thresholds)) ds$train_settings$isi_thresholds <- list()
    for (tr in targets) ds$train_settings$isi_thresholds[[tr]] <- NULL
    set_dataset(rv$current_id, ds)
    showNotification(paste0("\u5DF2\u6E05\u9664 ", length(targets), " \u6761 train \u7684 train-specific ISI \u9608\u503C\u3002"), type = "message")
  }, ignoreInit = TRUE)

  observeEvent(input$clear_all_train_isi_thresholds, {
    ds <- get_dataset(); if (is.null(ds)) return()
    ds$train_settings$isi_thresholds <- list()
    set_dataset(rv$current_id, ds)
    showNotification("\u5DF2\u6E05\u9664\u6240\u6709 train-specific ISI \u9608\u503C\u3002", type = "message")
  }, ignoreInit = TRUE)

  output$train_isi_thresholds_table <- DT::renderDT({
    ds <- current_dataset()
    f <- unit_factor(); u <- input$time_unit %||% "ms"
    dat <- train_isi_threshold_dataframe(ds$train_settings$isi_thresholds %||% list(), factor = f, unit = u)
    DT::datatable(dat, options = list(pageLength = 6, scrollX = TRUE), rownames = FALSE)
  })

  output$isi_profile_plot <- renderPlotly({
    dat_all <- isi_profile_data()
    f <- unit_factor()
    u <- input$time_unit
    x_axis <- input$isi_profile_x_axis %||% "time"
    y_scale <- input$isi_profile_y_scale %||% "log"
    range_mode <- input$isi_profile_time_range_mode %||% "full"
    trains <- unique(dat_all$train)

    make_panel <- function(dat, tr) {
      dat <- dat[dat$train == tr, , drop = FALSE]
      if (nrow(dat) == 0) return(NULL)
      if (identical(x_axis, "index")) {
        dat$x0 <- dat$isi_index - 0.5
        dat$x1 <- dat$isi_index + 0.5
        dat$x_mid <- dat$isi_index
        x_title <- "ISI \u987A\u5E8F\u7D22\u5F15"
        x_range <- NULL
      } else {
        # Use absolute spike timestamps on the ISI temporal-profile X-axis.
        # This axis is intentionally fixed in seconds so that the plotted X
        # coordinate matches the timestamp values shown in the hover tooltip.
        # The global Display unit still controls the ISI Y-axis.
        dat$x0 <- dat$left_time_sec
        dat$x1 <- dat$right_time_sec
        dat$x_mid <- dat$mid_time_sec
        x_title <- "Spike timestamp\uFF08s\uFF09"
        x_range <- NULL
        train_start <- suppressWarnings(as.numeric(dat$train_start_sec[1]))
        train_end <- suppressWarnings(as.numeric(dat$train_end_sec[1]))
	        if (!is.finite(train_start)) train_start <- min(dat$left_time_sec, na.rm = TRUE)
	        if (!is.finite(train_end)) train_end <- max(dat$right_time_sec, na.rm = TRUE)
	        if (identical(range_mode, "sync")) {
	          x_use <- raster_window_for_plot(debounced = TRUE, prefer_view = TRUE)
	          if (!is.null(x_use) && length(x_use) == 2 && all(is.finite(x_use)) && is.finite(train_start)) {
	            # Raster windows remain aligned to each train's first spike and use
	            # the global Display unit. Convert that aligned window into this
            # train's absolute timestamp window in seconds.
            sec_range <- sort(x_use) / f + train_start
            dat <- dat[dat$right_time_sec >= sec_range[1] & dat$left_time_sec <= sec_range[2], , drop = FALSE]
            x_range <- sec_range
          }
        } else if (identical(range_mode, "custom")) {
          x_use <- input$isi_profile_custom_range
          if (!is.null(x_use) && length(x_use) == 2 && all(is.finite(x_use))) {
            sec_range <- sort(x_use)
            dat <- dat[dat$right_time_sec >= sec_range[1] & dat$left_time_sec <= sec_range[2], , drop = FALSE]
            x_range <- sec_range
          }
        } else {
          # Full-duration mode: each panel starts at that train's first spike timestamp.
          if (is.finite(train_start) && is.finite(train_end) && train_end > train_start) {
            x_range <- c(train_start, train_end)
          } else {
            x_range <- c(min(dat$left_time_sec, na.rm = TRUE), max(dat$right_time_sec, na.rm = TRUE))
          }
        }
      }
      if (nrow(dat) == 0) return(NULL)
      dat$ISI_plot <- dat$ISI_sec * f
      dat$hover_text <- paste0(
        "Train\uFF1A", stpd_html_escape(dat$train),
        "<br>\u5DE6\u4FA7 spike \u65F6\u95F4\u6233\uFF1A", round(dat$left_time_sec, 6), " s",
        "<br>\u53F3\u4FA7 spike \u65F6\u95F4\u6233\uFF1A", round(dat$right_time_sec, 6), " s",
        "<br>ISI\uFF1A", signif(dat$ISI_sec, 6), " s (", round(dat$ISI_plot, 4), " ", u, ")",
        ifelse(is.finite(dat$ISI_pct), paste0("<br>ISI \u5728\u672C train \u4E2D\u7684\u767E\u5206\u4F4D\uFF1A", round(dat$ISI_pct, 2), "%"), ""),
        extended_isi_metrics_hover(
          dat$ISI_range_pct_linear,
          dat$ISI_range_pct_log,
          dat$ISI_robust_range_pct_log,
          show = isTRUE(input$show_extended_isi_metrics)
        )
	      )
	      dat$custom_payload <- paste0(dat$train, "|", dat$idx, "|", dat$ISI_sec)
	      delta_profile <- data.frame()
	      if (isTRUE(input$show_parameter_delta_overlay) && !is.null(rv$parameter_delta_preview)) {
	        delta_base <- tryCatch(
	          stpd_parameter_delta_overlay_rows(rv$parameter_delta_preview, current_dataset()$trains, selected_trains = tr),
	          error = function(e) data.frame()
	        )
	        if (!is.null(delta_base) && nrow(delta_base) > 0) {
	          delta_rows <- list()
	          for (ii in seq_len(nrow(delta_base))) {
	            ev <- delta_base[ii, , drop = FALSE]
	            dd <- dat[dat$idx >= ev$start_isi[1] & dat$idx <= ev$end_isi[1], , drop = FALSE]
	            if (nrow(dd) == 0) next
	            dd$delta_row_index <- ev$delta_row_index[1]
	            dd$delta_status <- as.character(ev$status[1] %||% "")
	            dd$delta_baseline_pattern <- as.character(ev$baseline_pattern[1] %||% "")
	            dd$delta_current_pattern <- as.character(ev$current_pattern[1] %||% "")
	            dd$delta_iou <- suppressWarnings(as.numeric(ev$iou[1] %||% NA_real_))
	            baseline_pattern <- if (nzchar(dd$delta_baseline_pattern[1])) dd$delta_baseline_pattern[1] else "none"
	            current_pattern <- if (nzchar(dd$delta_current_pattern[1])) dd$delta_current_pattern[1] else "none"
	            dd$delta_text <- paste0(
	              "\u53C2\u6570 dry-run \u5DEE\u5F02<br>",
	              "\u72B6\u6001\uFF1A", stpd_html_escape(dd$delta_status),
	              "<br>Train\uFF1A", stpd_html_escape(tr),
	              "<br>Baseline\uFF1A", stpd_html_escape(baseline_pattern),
	              "<br>Current\uFF1A", stpd_html_escape(current_pattern),
	              "<br>\u4E8B\u4EF6 ISI\uFF1A", ev$start_isi[1], "-", ev$end_isi[1],
	              "<br>\u5F53\u524D ISI idx\uFF1A", dd$idx,
	              "<br>IoU\uFF1A", ifelse(is.finite(dd$delta_iou), signif(dd$delta_iou, 4), "NA")
	            )
	            delta_rows[[length(delta_rows) + 1L]] <- dd
	          }
	          if (length(delta_rows) > 0) delta_profile <- dplyr::bind_rows(delta_rows)
	        }
	      }
	
	      pp <- plot_ly(source = "isi_profile")
      if (isTRUE(input$isi_profile_show_labels)) {
        shade_pats <- c("burst", "long_burst", "possible_burst", "tonic", "high_frequency_tonic", "high_frequency_spiking", "pause", "others")
        col_map <- c(burst = "rgba(214,39,40,0.10)", long_burst = "rgba(166,54,3,0.10)", possible_burst = "rgba(255,127,14,0.10)",
                     tonic = "rgba(44,160,44,0.10)", high_frequency_tonic = "rgba(23,190,207,0.10)", high_frequency_spiking = "rgba(148,103,189,0.10)", pause = "rgba(31,119,180,0.10)", others = "rgba(120,120,120,0.08)")
        for (pat in shade_pats) {
          sub <- dat[dat$pattern_final == pat, , drop = FALSE]
          if (nrow(sub) == 0) next
          pp <- add_segments(pp, data = sub, x = ~x0, xend = ~x1, y = ~ISI_plot, yend = ~ISI_plot,
                             type = "scatter", mode = "lines",
                             line = list(width = 10, color = col_map[[pat]] %||% "rgba(120,120,120,0.08)"),
                             hoverinfo = "none", showlegend = FALSE, inherit = FALSE)
        }
      }
	      if (isTRUE(input$isi_profile_show_thresholds)) {
	        ds_thr <- tryCatch(current_dataset(), error = function(e) NULL)
	        thr <- if (!is.null(ds_thr)) ds_thr$train_settings$isi_thresholds[[tr]] %||% list() else list()
          thr_hard <- stpd_train_isi_threshold_is_hard(thr)
          thr_dash <- if (isTRUE(thr_hard)) "solid" else "dash"
          thr_width <- if (isTRUE(thr_hard)) 2.2 else 1.5
          thr_suffix <- if (isTRUE(thr_hard)) "hard threshold" else "soft anchor"
	        x_min_thr <- if (is.null(x_range)) min(dat$x0, na.rm = TRUE) else x_range[1]
	        x_max_thr <- if (is.null(x_range)) max(dat$x1, na.rm = TRUE) else x_range[2]
	        add_thr_line <- function(pp, value_sec, color, dash, label, width = 1.5) {
	          value_sec <- suppressWarnings(as.numeric(value_sec))
	          if (!is.finite(value_sec) || value_sec <= 0) return(pp)
	          add_segments(pp, x = x_min_thr, xend = x_max_thr, y = value_sec * f, yend = value_sec * f,
	                       type = "scatter", mode = "lines",
	                       line = list(width = width, color = color, dash = dash),
	                       hoverinfo = "text", text = paste0(stpd_html_escape(label), " [", thr_suffix, "]: ", signif(value_sec, 6), " s (", round(value_sec * f, 4), " ", u, ")"),
	                       showlegend = FALSE, inherit = FALSE)
	        }
	        pp <- add_thr_line(pp, thr$burst_max_sec %||% 0, stpd_ui_pattern_color("burst", "auto"), thr_dash, "burst \u6700\u5927ISI", thr_width)
	        pp <- add_thr_line(pp, thr$pause_min_sec %||% 0, stpd_ui_pattern_color("pause", "auto"), thr_dash, "pause \u6700\u5C0FISI", thr_width)
	        pp <- add_thr_line(pp, thr$tonic_min_sec %||% 0, stpd_ui_pattern_color("tonic", "auto"), if (isTRUE(thr_hard)) "solid" else "dot", "tonic \u6700\u5C0FISI", thr_width)
	        pp <- add_thr_line(pp, thr$tonic_max_sec %||% 0, stpd_ui_pattern_color("tonic", "auto"), if (isTRUE(thr_hard)) "solid" else "dot", "tonic \u6700\u5927ISI", thr_width)
	      }

      pp <- add_segments(pp, data = dat, x = ~x0, xend = ~x1, y = ~ISI_plot, yend = ~ISI_plot,
                         type = "scatter", mode = "lines",
                         line = list(width = 2, color = stable_train_color(tr)),
                         hoverinfo = "text", text = ~hover_text,
                         customdata = ~custom_payload,
                         showlegend = FALSE, inherit = FALSE)
	      pp <- add_markers(pp, data = dat, x = ~x_mid, y = ~ISI_plot,
	                        marker = list(size = 5, color = stable_train_color(tr)),
	                        opacity = 0.55, hoverinfo = "text", text = ~hover_text,
	                        customdata = ~custom_payload,
	                        showlegend = FALSE, inherit = FALSE)
	      if (!is.null(delta_profile) && nrow(delta_profile) > 0) {
	        delta_styles <- data.frame(
	          status = c("added_event", "removed_event", "label_changed", "boundary_changed"),
	          color = c("#1B9E77", "#D95F02", "#7570B3", "#E6AB02"),
	          dash = c("solid", "dash", "dot", "dashdot"),
	          stringsAsFactors = FALSE
	        )
	        for (ss in delta_styles$status) {
	          dd <- delta_profile[as.character(delta_profile$delta_status) == ss, , drop = FALSE]
	          if (nrow(dd) == 0) next
	          st <- delta_styles[delta_styles$status == ss, , drop = FALSE]
	          pp <- add_segments(pp, data = dd, x = ~x0, xend = ~x1, y = ~ISI_plot, yend = ~ISI_plot,
	                             type = "scatter", mode = "lines",
	                             line = list(width = 7, color = st$color[1], dash = st$dash[1]),
	                             opacity = 0.78,
	                             hoverinfo = "text", text = ~delta_text,
	                             showlegend = FALSE, inherit = FALSE)
	        }
	        sel_row <- suppressWarnings(as.integer(rv$parameter_delta_preview_selected_row %||% NA_integer_))
	        if (is.finite(sel_row)) {
	          dd_sel <- delta_profile[delta_profile$delta_row_index == sel_row, , drop = FALSE]
	          if (nrow(dd_sel) > 0) {
	            pp <- add_segments(pp, data = dd_sel, x = ~x0, xend = ~x1, y = ~ISI_plot, yend = ~ISI_plot,
	                               type = "scatter", mode = "lines",
	                               line = list(width = 2.5, color = "#000000", dash = "solid"),
	                               hoverinfo = "text", text = ~paste0(delta_text, "<br>\u8868\u683C\u9009\u4E2D\u884C"),
	                               showlegend = FALSE, inherit = FALSE)
	          }
	        }
	      }
	
	      ref <- rv$isi_profile_ref
      apply_ref <- !is.null(ref) && is.finite(ref$isi_sec) && ref$isi_sec > 0 &&
        (isTRUE(input$isi_profile_ref_all_panels) || identical(as.character(ref$train), as.character(tr)))
      if (isTRUE(apply_ref)) {
        tol <- suppressWarnings(as.numeric(input$isi_profile_ref_tol %||% 0.10))
        tol <- ifelse(is.finite(tol) && tol > 0, tol, 0.10)
        ratio <- dat$ISI_sec / ref$isi_sec
        sim <- dat[is.finite(ratio) & ratio >= (1 - tol) & ratio <= (1 + tol), , drop = FALSE]
        ref_y <- ref$isi_sec * f
        x_min <- if (is.null(x_range)) min(dat$x0, na.rm = TRUE) else x_range[1]
        x_max <- if (is.null(x_range)) max(dat$x1, na.rm = TRUE) else x_range[2]
        pp <- add_segments(pp, x = x_min, xend = x_max, y = ref_y, yend = ref_y,
                           type = "scatter", mode = "lines",
                           line = list(color = "rgba(0,0,0,0.45)", width = 1, dash = "dash"),
                           hoverinfo = "none", showlegend = FALSE, inherit = FALSE)
        if (nrow(sim) > 0) {
          pp <- add_markers(pp, data = sim, x = ~x_mid, y = ~ISI_plot,
                            marker = list(size = 8, symbol = "circle-open", color = "black", line = list(width = 1.5, color = "black")),
                            hoverinfo = "text", text = ~paste0(hover_text, "<br>Similar to locked reference"),
                            showlegend = FALSE, inherit = FALSE)
        }
      }
      yaxis <- list(title = paste0("ISI (", u, ")"))
      if (identical(y_scale, "log")) yaxis$type <- "log"
      pp <- layout(pp,
             hoverlabel = stpd_hoverlabel_style(),
             title = list(text = tr, font = list(size = 12)),
             xaxis = list(title = x_title, range = x_range, exponentformat = "none", separatethousands = FALSE),
             yaxis = yaxis,
             hovermode = "closest",
             margin = list(l = 70, r = 20, t = 35, b = 45))
      event_register(pp, "plotly_click")
    }

    panels <- lapply(trains, function(tr) make_panel(dat_all, tr))
    panels <- panels[!vapply(panels, is.null, logical(1))]
    validate(need(length(panels) > 0, "No valid ISIs in selected profile window."))
    if (length(panels) == 1) {
      return(config(event_register(panels[[1]], "plotly_click"), displaylogo = FALSE))
    }
    subplot(panels, nrows = length(panels), shareX = FALSE, shareY = FALSE, margin = 0.03, titleY = TRUE) %>%
      event_register("plotly_click") %>%
      layout(hoverlabel = stpd_hoverlabel_style(), title = "ISI \u65F6\u95F4\u5256\u9762\uFF1A\u6BCF\u6761 spike train \u4F7F\u7528\u72EC\u7ACB timestamp \u8F74", showlegend = FALSE) %>%
      config(displaylogo = FALSE)
  })

  # ----------------------------------------------------------
  # ISI state-space PCA and phase portrait
  # ----------------------------------------------------------
  output$isi_state_space_train_selector <- renderUI({
    td <- current_trains()
    choices <- names(td)
    if (length(choices) == 0) return(NULL)
    default <- intersect(displayed_train_names(), choices)
    if (length(default) == 0) default <- head(choices, 1)
    selectizeInput(
      "isi_state_space_train",
      "\u5355\u6761 spike train",
      choices = choices,
      selected = default[1],
      multiple = FALSE,
      options = list(placeholder = "\u9009\u62E9\u4E00\u6761 train \u8FDB\u884C PCA / phase portrait")
    )
  })

  isi_state_space_selected_train <- reactive({
    td <- current_trains()
    choices <- names(td)
    validate(need(length(choices) > 0, "\u8BF7\u5148\u4E0A\u4F20\u6570\u636E\u3002"))
    tr <- input$isi_state_space_train
    if (is.null(tr) || !(tr %in% choices)) tr <- intersect(displayed_train_names(), choices)[1]
    if (is.null(tr) || length(tr) == 0 || !(tr %in% choices)) tr <- choices[1]
    as.character(tr)
  })

  output$isi_state_space_custom_window_ui <- renderUI({
    if (!identical(input$isi_state_space_time_range_mode %||% "full", "custom")) return(NULL)
    td <- current_trains()
    tr <- isi_state_space_selected_train()
    if (!(tr %in% names(td))) return(NULL)
    dat <- td[[tr]]
    if (is.null(dat) || nrow(dat) < 2 || !("timestamp_sec" %in% names(dat))) return(NULL)
    ts <- suppressWarnings(as.numeric(dat$timestamp_sec))
    ts <- ts[is.finite(ts)]
    if (length(ts) < 2) return(NULL)
    min_t <- floor(min(ts) * 1000) / 1000
    max_t <- ceiling(max(ts) * 1000) / 1000
    if (!is.finite(min_t) || !is.finite(max_t) || max_t <= min_t) return(NULL)
    default_width <- min(max_t - min_t, 1)
    sliderInput(
      "isi_state_space_custom_range",
      "\u81EA\u5B9A\u4E49 timestamp \u7A97\u53E3\uFF08s\uFF09",
      min = min_t,
      max = max_t,
      value = c(min_t, min(max_t, min_t + default_width)),
      step = 0.001
    )
  })

  isi_state_space_filter_time <- function(df, train_start_sec) {
    if (is.null(df) || nrow(df) == 0) return(df)
	    mode <- input$isi_state_space_time_range_mode %||% "full"
	    if (identical(mode, "sync")) {
	      x_use <- raster_window_for_plot(debounced = TRUE, prefer_view = TRUE)
	      if (!is.null(x_use) && length(x_use) == 2 && all(is.finite(x_use)) && is.finite(train_start_sec)) {
	        sec_range <- sort(x_use) / unit_factor() + train_start_sec
	        return(df[df$right_time_sec >= sec_range[1] & df$left_time_sec <= sec_range[2], , drop = FALSE])
      }
    } else if (identical(mode, "custom")) {
      x_use <- input$isi_state_space_custom_range
      if (!is.null(x_use) && length(x_use) == 2 && all(is.finite(x_use))) {
        sec_range <- sort(x_use)
        return(df[df$right_time_sec >= sec_range[1] & df$left_time_sec <= sec_range[2], , drop = FALSE])
      }
    }
    df
  }

  isi_state_space_feature_data <- reactive({
    td <- current_trains()
    tr <- isi_state_space_selected_train()
    validate(need(tr %in% names(td), "\u8BF7\u9009\u62E9\u6709\u6548 train\u3002"))
    dat <- ensure_train_isi_percentiles(td[[tr]], min_valid_isi_sec())
    train_start <- suppressWarnings(as.numeric(dat$timestamp_sec[1]))
    feats <- stpd_make_isi_state_space_features(
      dat,
      train = tr,
      label_source = input$isi_state_space_label_source %||% "audit_final",
      k = safe_int(input$isi_state_space_k, 3L),
      min_isi_sec = min_valid_isi_sec(),
      auto_others = isTRUE(input$auto_others),
      winsorize = isTRUE(input$isi_state_space_winsorize)
    )
    feats <- isi_state_space_filter_time(feats, train_start)
    validate(need(nrow(feats) >= 3, "\u5F53\u524D train / \u65F6\u95F4\u7A97\u5185\u6709\u6548 ISI \u592A\u5C11\uFF0C\u65E0\u6CD5\u8FDB\u884C PCA\u3002"))
    feats
  })

  isi_state_space_pca_result <- reactive({
    stpd_run_isi_state_pca(
      isi_state_space_feature_data(),
      scaling = input$isi_state_space_scaling %||% "robust"
    )
  })

  isi_state_space_isomap_result <- reactive({
    feats <- isi_state_space_feature_data()
    validate(need(nrow(feats) >= 20, "\u5F53\u524D train / \u65F6\u95F4\u7A97\u5185\u6709\u6548 ISI \u592A\u5C11\uFF0CIsomap \u81F3\u5C11\u9700\u8981 20 \u4E2A\u70B9\u3002"))
    tryCatch(
      stpd_run_isi_state_isomap(
        feats,
        n_neighbors = safe_int(input$isi_state_space_isomap_neighbors, 15L),
        max_points = safe_int(input$isi_state_space_isomap_max_points, 600L),
        scaling = input$isi_state_space_scaling %||% "robust",
        ndim = 3L,
        component = "largest"
      ),
      error = function(e) {
        validate(need(FALSE, paste0("Isomap \u8BA1\u7B97\u5931\u8D25\uFF1A", conditionMessage(e))))
      }
    )
  })

  isi_state_space_phase_data <- reactive({
    td <- current_trains()
    tr <- isi_state_space_selected_train()
    validate(need(tr %in% names(td), "\u8BF7\u9009\u62E9\u6709\u6548 train\u3002"))
    dat <- td[[tr]]
    train_start <- suppressWarnings(as.numeric(dat$timestamp_sec[1]))
    ph <- stpd_make_logisi_phase_portrait(
      dat,
      train = tr,
      label_source = input$isi_state_space_label_source %||% "audit_final",
      min_isi_sec = min_valid_isi_sec(),
      auto_others = isTRUE(input$auto_others),
      lag = 1L,
      winsorize = isTRUE(input$isi_state_space_winsorize)
    )
    ph <- isi_state_space_filter_time(ph, train_start)
    validate(need(nrow(ph) >= 2, "\u5F53\u524D train / \u65F6\u95F4\u7A97\u5185\u76F8\u90BB ISI \u5BF9\u592A\u5C11\u3002"))
    ph
  })

  isi_state_space_nature_palette <- function() {
    c(
      burst = "#C65A9B",
      long_burst = "#8E75C9",
      possible_burst = "#B58BE0",
      tonic = "#8CD36A",
      high_frequency_tonic = "#49B86A",
      high_frequency_spiking = "#E64B52",
      pause = "#3D6FD8",
      others = "#D6C94C",
      unlabeled = "#9AA3AF",
      not_burst = "#111827"
    )
  }

  isi_state_space_color_map <- function(labels) {
    labels <- as.character(labels)
    base <- isi_state_space_nature_palette()
    cols <- setNames(rep("#9AA3AF", length(labels)), labels)
    hit <- intersect(names(cols), names(base))
    cols[hit] <- base[hit]
    missing <- setdiff(names(cols), names(base))
    if (length(missing) > 0) {
      pal <- tryCatch(pattern_palette("pattern_color"), error = function(e) data.frame())
      if (!is.null(pal) && nrow(pal) > 0) {
        fallback <- setNames(as.character(pal$auto), as.character(pal$pattern))
        hit2 <- intersect(missing, names(fallback))
        cols[hit2] <- fallback[hit2]
      }
    }
    cols
  }

  isi_state_space_label_name <- function(label) {
    out <- tryCatch(stpd_ui_pattern_display(label), error = function(e) label)
    out <- as.character(out)
    out[is.na(out) | !nzchar(out)] <- label[is.na(out) | !nzchar(out)]
    out
  }

  isi_state_space_short_label <- function(x, max_chars = 64L) {
    x <- as.character(x %||% "")
    if (!nzchar(x) || nchar(x) <= max_chars) return(x)
    paste0(substr(x, 1, max(1L, max_chars - 1L)), "\u2026")
  }

	  isi_state_space_is_english <- function() {
	    identical(as.character(input$ui_language %||% "zh")[1], "en")
	  }

	  isi_state_space_axis_title <- function(col) {
	    u <- input$time_unit %||% "ms"
	    if (isTRUE(isi_state_space_is_english())) {
	      labs <- c(
	        PC1 = "PC1",
	        PC2 = "PC2",
	        PC3 = "PC3",
	        Isomap1 = "Isomap 1",
	        Isomap2 = "Isomap 2",
	        Isomap3 = "Isomap 3",
	        Diffusion1 = "Diffusion 1",
	        Diffusion2 = "Diffusion 2",
	        Diffusion3 = "Diffusion 3",
	        PHATE1 = "PHATE 1",
	        PHATE2 = "PHATE 2",
	        PHATE3 = "PHATE 3",
	        UMAP1 = "UMAP 1",
	        UMAP2 = "UMAP 2",
	        UMAP3 = "UMAP 3",
	        tSNE1 = "t-SNE 1",
	        tSNE2 = "t-SNE 2",
	        tSNE3 = "t-SNE 3",
	        time_from_start_plot = paste0("time (", u, ")"),
	        ISI_plot = paste0("ISI (", u, ")"),
	        log_isi = "log10(ISI)",
	        local_rate_hz = "local firing rate (Hz)",
	        local_cv2 = "local CV2",
	        local_lv = "local LV",
	        prepost_ratio = "pre/post ratio",
	        delta_logisi = "delta logISI",
	        next_delta_logisi = "next delta logISI"
	      )
	    } else {
	      labs <- c(
	        PC1 = "PC1",
	        PC2 = "PC2",
	        PC3 = "PC3",
	        Isomap1 = "Isomap 1",
	        Isomap2 = "Isomap 2",
	        Isomap3 = "Isomap 3",
	        Diffusion1 = "Diffusion 1",
	        Diffusion2 = "Diffusion 2",
	        Diffusion3 = "Diffusion 3",
	        PHATE1 = "PHATE 1",
	        PHATE2 = "PHATE 2",
	        PHATE3 = "PHATE 3",
	        UMAP1 = "UMAP 1",
	        UMAP2 = "UMAP 2",
	        UMAP3 = "UMAP 3",
	        tSNE1 = "t-SNE 1",
	        tSNE2 = "t-SNE 2",
	        tSNE3 = "t-SNE 3",
	        time_from_start_plot = paste0("\u65F6\u95F4\uFF08", u, "\uFF09"),
	        ISI_plot = paste0("ISI\uFF08", u, "\uFF09"),
	        log_isi = "log10(ISI)",
	        local_rate_hz = "\u5C40\u90E8\u53D1\u653E\u7387 (Hz)",
	        local_cv2 = "\u5C40\u90E8 CV2",
	        local_lv = "\u5C40\u90E8 LV",
	        prepost_ratio = "pre/post ratio",
	        delta_logisi = "\u0394 logISI",
	        next_delta_logisi = "next \u0394 logISI"
	      )
	    }
	    labs[[col]] %||% col
	  }

	  isi_state_space_legend <- function(y) {
	    list(
	      orientation = "h",
	      x = 0,
	      y = y,
	      itemsizing = "constant",
	      font = list(size = 11, color = "#334155")
	    )
	  }

	  isi_state_space_axis_style <- function(title, reversed = FALSE) {
	    out <- list(
	      title = list(text = title, font = list(size = 12, color = "#1f2937")),
	      tickfont = list(size = 11, color = "#374151"),
	      showline = TRUE,
	      linecolor = "#475569",
	      linewidth = 1,
	      mirror = FALSE,
	      ticks = "outside",
	      tickcolor = "#64748b",
	      gridcolor = "rgba(15, 23, 42, 0.08)",
	      zerolinecolor = "rgba(15, 23, 42, 0.22)",
	      zerolinewidth = 1
	    )
	    if (isTRUE(reversed)) out$autorange <- "reversed"
	    out
	  }

	  isi_state_space_plot_layout <- function(p, title, x_title, y_title,
	                                          legend_y = -0.2,
	                                          margin = list(l = 65, r = 18, t = 70, b = 95)) {
	    layout(
	      p,
	      hoverlabel = stpd_hoverlabel_style(),
	      title = list(text = title, x = 0, font = list(size = 14, color = "#111827")),
	      xaxis = isi_state_space_axis_style(x_title),
	      yaxis = isi_state_space_axis_style(y_title),
	      legend = isi_state_space_legend(legend_y),
	      margin = margin,
	      hovermode = "closest",
	      plot_bgcolor = "#ffffff",
	      paper_bgcolor = "#ffffff",
	      font = list(color = "#1f2937")
	    )
	  }

	  isi_state_space_scene_axis <- function(title) {
	    list(
	      title = list(text = title, font = list(size = 11, color = "#1f2937")),
	      tickfont = list(size = 10, color = "#374151"),
	      showbackground = FALSE,
	      gridcolor = "rgba(15, 23, 42, 0.10)",
	      zerolinecolor = "rgba(15, 23, 42, 0.24)",
	      linecolor = "#64748b"
	    )
	  }

  isi_state_space_add_label_markers <- function(p, dd, x_col, y_col, size = 6) {
    labels <- unique(as.character(dd$label))
    preferred <- c("burst", "long_burst", "possible_burst", "high_frequency_spiking",
                   "high_frequency_tonic", "tonic", "pause", "others", "unlabeled")
    labels <- c(intersect(preferred, labels), setdiff(labels, preferred))
    cols <- isi_state_space_color_map(labels)
    for (lb in labels) {
      sub <- dd[as.character(dd$label) == lb, , drop = FALSE]
      if (nrow(sub) == 0) next
      p <- add_markers(
        p,
        data = sub,
        x = as.formula(paste0("~", x_col)),
        y = as.formula(paste0("~", y_col)),
        name = isi_state_space_label_name(lb),
        marker = list(size = size, color = unname(cols[lb]), line = list(width = 0.55, color = "rgba(255,255,255,0.9)")),
        opacity = 0.88,
        hoverinfo = "text",
        text = ~hover_text,
        inherit = FALSE
      )
    }
    p
  }

  isi_state_space_add_label_markers_3d <- function(p, dd, x_col, y_col, z_col, size = 3.5) {
    labels <- unique(as.character(dd$label))
    preferred <- c("burst", "long_burst", "possible_burst", "high_frequency_spiking",
                   "high_frequency_tonic", "tonic", "pause", "others", "unlabeled")
    labels <- c(intersect(preferred, labels), setdiff(labels, preferred))
    cols <- isi_state_space_color_map(labels)
    for (lb in labels) {
      sub <- dd[as.character(dd$label) == lb, , drop = FALSE]
      if (nrow(sub) == 0) next
      p <- add_trace(
        p,
        data = sub,
        x = as.formula(paste0("~", x_col)),
        y = as.formula(paste0("~", y_col)),
        z = as.formula(paste0("~", z_col)),
        type = "scatter3d",
        mode = "markers",
        name = isi_state_space_label_name(lb),
        marker = list(size = size, color = unname(cols[lb]), opacity = 0.86),
        hoverinfo = "text",
        text = ~hover_text,
        inherit = FALSE
      )
    }
    p
  }

  isi_state_space_enrich_scores <- function(dd) {
    f <- unit_factor()
    u <- input$time_unit %||% "ms"
    train_start <- suppressWarnings(min(dd$left_time_sec, na.rm = TRUE))
    if (!is.finite(train_start)) train_start <- suppressWarnings(min(dd$time_mid_sec, na.rm = TRUE))
    dd$time_from_start_sec <- dd$time_mid_sec - train_start
    dd$time_from_start_plot <- dd$time_from_start_sec * f
    dd$ISI_plot <- dd$ISI_sec * f

    score_cols <- intersect(c("PC1", "PC2", "PC3", "Isomap1", "Isomap2", "Isomap3",
                              "Diffusion1", "Diffusion2", "Diffusion3",
                              "PHATE1", "PHATE2", "PHATE3",
                              "UMAP1", "UMAP2", "UMAP3",
                              "tSNE1", "tSNE2", "tSNE3"), names(dd))
    score_text <- rep("", nrow(dd))
    score_sep <- if (isTRUE(isi_state_space_is_english())) ": " else "\uFF1A"
    for (nm in score_cols) {
      score_text <- paste0(score_text, "<br>", nm, score_sep, round(dd[[nm]], 4))
    }
    if (isTRUE(isi_state_space_is_english())) {
      dd$hover_text <- paste0(
        "Train: ", stpd_html_escape(dd$train),
        "<br>ISI idx: ", dd$idx,
        "<br>time_mid: ", round(dd$time_mid_sec, 6), " s",
        "<br>relative time: ", round(dd$time_from_start_plot, 4), " ", u,
        "<br>ISI: ", signif(dd$ISI_sec, 6), " s (", round(dd$ISI_plot, 4), " ", u, ")",
        "<br>label: ", stpd_html_escape(dd$label),
        score_text,
        "<br>local LV: ", signif(dd$local_lv, 4),
        "<br>local CV2: ", signif(dd$local_cv2, 4)
      )
    } else {
      dd$hover_text <- paste0(
        "Train\uFF1A", stpd_html_escape(dd$train),
        "<br>ISI idx\uFF1A", dd$idx,
        "<br>time_mid\uFF1A", round(dd$time_mid_sec, 6), " s",
        "<br>\u76F8\u5BF9\u65F6\u95F4\uFF1A", round(dd$time_from_start_plot, 4), " ", u,
        "<br>ISI\uFF1A", signif(dd$ISI_sec, 6), " s (", round(dd$ISI_plot, 4), " ", u, ")",
        "<br>label\uFF1A", stpd_html_escape(dd$label),
        score_text,
        "<br>local LV\uFF1A", signif(dd$local_lv, 4),
        "<br>local CV2\uFF1A", signif(dd$local_cv2, 4)
      )
    }
    break_thr <- max(0, safe_ui_value(input$isi_state_space_break_isi, 150) / f)
    break_flag <- rep(FALSE, nrow(dd))
    if (isTRUE(input$isi_state_space_break_pause)) {
      break_flag <- as.character(dd$label) == "pause"
      if (is.finite(break_thr) && break_thr > 0) break_flag <- break_flag | (is.finite(dd$ISI_sec) & dd$ISI_sec >= break_thr)
    }
    dd$line_group <- cumsum(c(1L, as.integer(head(break_flag, -1))))
    dd
  }

  isi_state_space_pca_plot_bundle <- reactive({
    res <- isi_state_space_pca_result()
    dd <- res$scores
    validate(need(nrow(dd) >= 2, "\u65E0 PCA \u5F97\u5206\u3002"))
    dd <- isi_state_space_enrich_scores(dd)
    list(res = res, scores = dd, unit = input$time_unit %||% "ms")
  })

  output$isi_state_space_pca_plot <- renderPlotly({
    bundle <- isi_state_space_pca_plot_bundle()
    res <- bundle$res
    dd <- bundle$scores
    validate(need(nrow(dd) >= 2, "\u65E0 PCA \u5F97\u5206\u3002"))
    x_col <- input$isi_state_space_x_axis %||% "PC1"
    y_col <- input$isi_state_space_y_axis %||% "PC2"
    validate(need(all(c(x_col, y_col) %in% names(dd)), "\u6240\u9009 2D \u8F74\u5728\u5F53\u524D\u6570\u636E\u4E2D\u4E0D\u5B58\u5728\u3002"))
    ok <- is.finite(dd[[x_col]]) & is.finite(dd[[y_col]])
    dd <- dd[ok, , drop = FALSE]
    validate(need(nrow(dd) >= 2, "\u6240\u9009 2D \u8F74\u7684\u6709\u6548\u70B9\u592A\u5C11\u3002"))
    p <- plot_ly(source = "isi_state_space_pca")
    for (gg in unique(dd$line_group)) {
      sub <- dd[dd$line_group == gg, , drop = FALSE]
      if (nrow(sub) < 2) next
      p <- add_trace(
        p,
        data = sub,
        x = as.formula(paste0("~", x_col)),
        y = as.formula(paste0("~", y_col)),
        type = "scatter",
        mode = "lines",
        line = list(color = "rgba(100,116,139,0.24)", width = 1),
        hoverinfo = "none",
        showlegend = FALSE,
        inherit = FALSE
      )
    }
    p <- isi_state_space_add_label_markers(p, dd, x_col, y_col, size = 6)
    var_tbl <- res$variance
    subtitle <- if (nrow(var_tbl) >= 2) {
      paste0("PC1 ", round(100 * var_tbl$variance[1], 1), "%; PC2 ", round(100 * var_tbl$variance[2], 1), "%")
    } else ""
    isi_state_space_plot_layout(
      p,
      title = paste0("ISI-PCA \u8F68\u8FF9", if (nzchar(subtitle)) paste0("<br><sup>", subtitle, " | ", isi_state_space_short_label(unique(dd$train)[1]), "</sup>") else ""),
      x_title = isi_state_space_axis_title(x_col),
      y_title = isi_state_space_axis_title(y_col),
      legend_y = -0.2,
      margin = list(l = 65, r = 18, t = 78, b = 98)
    ) %>% config(displaylogo = FALSE)
  })

	  output$isi_state_space_isomap_plot <- renderPlotly({
	    res <- isi_state_space_isomap_result()
	    dd <- res$scores
	    validate(need(nrow(dd) >= 2, "\u65E0 Isomap \u5F97\u5206\u3002"))
	    dd <- isi_state_space_enrich_scores(dd)
	    x_col <- input$isi_state_space_isomap_x_axis %||% "Isomap1"
	    y_col <- input$isi_state_space_isomap_y_axis %||% "Isomap2"
	    validate(need(all(c(x_col, y_col) %in% names(dd)), "\u6240\u9009 Isomap 2D \u8F74\u5728\u5F53\u524D\u6570\u636E\u4E2D\u4E0D\u5B58\u5728\u3002"))
	    ok <- is.finite(dd[[x_col]]) & is.finite(dd[[y_col]])
	    dd <- dd[ok, , drop = FALSE]
	    validate(need(nrow(dd) >= 2, "Isomap \u6709\u6548\u70B9\u592A\u5C11\u3002"))
	    p <- plot_ly(source = "isi_state_space_isomap")
    for (gg in unique(dd$line_group)) {
      sub <- dd[dd$line_group == gg, , drop = FALSE]
      if (nrow(sub) < 2) next
      p <- add_trace(
	        p,
	        data = sub,
	        x = as.formula(paste0("~", x_col)),
	        y = as.formula(paste0("~", y_col)),
	        type = "scatter",
	        mode = "lines",
        line = list(color = "rgba(100,116,139,0.24)", width = 1),
        hoverinfo = "none",
        showlegend = FALSE,
        inherit = FALSE
	      )
	    }
	    p <- isi_state_space_add_label_markers(p, dd, x_col, y_col, size = 6)
    diag <- res$diagnostics
    subtitle <- if (nrow(diag) > 0) {
      paste0(
        "k=", diag$n_neighbors[1],
        "; embedded ", diag$n_embedded[1], "/", diag$n_input[1],
        "; components=", diag$component_n[1],
        "; residual variance=", ifelse(is.finite(diag$residual_variance[1]), round(diag$residual_variance[1], 3), "NA")
      )
    } else ""
    isi_state_space_plot_layout(
      p,
      title = paste0("Isomap \u72B6\u6001\u8F68\u8FF9", if (nzchar(subtitle)) paste0("<br><sup>", subtitle, " | ", isi_state_space_short_label(unique(dd$train)[1]), "</sup>") else ""),
      x_title = isi_state_space_axis_title(x_col),
      y_title = isi_state_space_axis_title(y_col),
      legend_y = -0.2,
      margin = list(l = 65, r = 18, t = 78, b = 98)
    ) %>% config(displaylogo = FALSE)
  })

  output$isi_state_space_phase_plot <- renderPlotly({
    dd <- isi_state_space_phase_data()
    f <- unit_factor()
    u <- input$time_unit %||% "ms"
    dd$ISI_plot <- dd$ISI_sec * f
    dd$next_ISI_plot <- dd$next_ISI_sec * f
    dd$hover_text <- paste0(
      "Train\uFF1A", stpd_html_escape(dd$train),
      "<br>ISI idx\uFF1A", dd$idx, " \u2192 ", dd$next_idx,
      "<br>time_mid\uFF1A", round(dd$time_mid_sec, 6), " s",
      "<br>ISI_i\uFF1A", signif(dd$ISI_sec, 6), " s (", round(dd$ISI_plot, 4), " ", u, ")",
      "<br>ISI_i+1\uFF1A", signif(dd$next_ISI_sec, 6), " s (", round(dd$next_ISI_plot, 4), " ", u, ")",
      "<br>transition\uFF1A", stpd_html_escape(dd$transition),
      "<br>logISI_i\uFF1A", round(dd$logISI_i, 4),
      "<br>logISI_i+1\uFF1A", round(dd$logISI_next, 4)
    )
    p <- plot_ly(source = "isi_state_space_phase")
    if (nrow(dd) >= 2) {
      p <- add_trace(
        p,
        data = dd,
        x = ~logISI_i,
        y = ~logISI_next,
        type = "scatter",
        mode = "lines",
        line = list(color = "rgba(100,116,139,0.18)", width = 0.9),
        hoverinfo = "none",
        showlegend = FALSE,
        inherit = FALSE
      )
    }
    rng <- range(c(dd$logISI_i, dd$logISI_next), finite = TRUE)
    if (length(rng) == 2 && all(is.finite(rng)) && rng[2] > rng[1]) {
      p <- add_segments(
        p,
        x = rng[1], xend = rng[2], y = rng[1], yend = rng[2],
        line = list(color = "rgba(71,85,105,0.35)", width = 1, dash = "dash"),
        hoverinfo = "none",
        showlegend = FALSE,
        inherit = FALSE
      )
    }
    p <- isi_state_space_add_label_markers(p, dd, "logISI_i", "logISI_next", size = 7)
    isi_state_space_plot_layout(
      p,
      title = paste0("logISI phase portrait<br><sup>", isi_state_space_short_label(unique(dd$train)[1]), "</sup>"),
      x_title = "log10(ISI_i)",
      y_title = "log10(ISI_i+1)",
      legend_y = -0.2,
      margin = list(l = 72, r = 18, t = 78, b = 98)
    ) %>% config(displaylogo = FALSE)
  })

  output$isi_state_space_3d_plot <- renderPlotly({
    bundle <- isi_state_space_pca_plot_bundle()
    dd <- bundle$scores
    x_col <- input$isi_state_space_x_axis %||% "PC1"
    y_col <- input$isi_state_space_y_axis %||% "PC2"
    z_col <- input$isi_state_space_z_axis %||% "time_from_start_plot"
    needed <- c(x_col, y_col, z_col)
    validate(need(all(needed %in% names(dd)), "\u6240\u9009 3D \u8F74\u5728\u5F53\u524D\u6570\u636E\u4E2D\u4E0D\u5B58\u5728\u3002"))
    ok <- is.finite(dd[[x_col]]) & is.finite(dd[[y_col]]) & is.finite(dd[[z_col]])
    dd <- dd[ok, , drop = FALSE]
    validate(need(nrow(dd) >= 3, "\u6240\u9009 3D \u8F74\u7684\u6709\u6548\u70B9\u592A\u5C11\u3002"))
    p <- plot_ly(source = "isi_state_space_3d")
    for (gg in unique(dd$line_group)) {
      sub <- dd[dd$line_group == gg, , drop = FALSE]
      if (nrow(sub) < 2) next
      p <- add_trace(
        p,
        data = sub,
        x = as.formula(paste0("~", x_col)),
        y = as.formula(paste0("~", y_col)),
        z = as.formula(paste0("~", z_col)),
        type = "scatter3d",
        mode = "lines",
        line = list(color = "rgba(100,116,139,0.26)", width = 2),
        hoverinfo = "none",
        showlegend = FALSE,
        inherit = FALSE
      )
    }
    p <- isi_state_space_add_label_markers_3d(p, dd, x_col, y_col, z_col, size = 3.5)
    layout(
      p,
      hoverlabel = stpd_hoverlabel_style(),
      title = list(
        text = paste0("3D ISI \u72B6\u6001\u7A7A\u95F4<br><sup>", isi_state_space_short_label(unique(dd$train)[1]), "</sup>"),
        x = 0,
        font = list(size = 14)
      ),
      scene = list(
        xaxis = isi_state_space_scene_axis(isi_state_space_axis_title(x_col)),
        yaxis = isi_state_space_scene_axis(isi_state_space_axis_title(y_col)),
        zaxis = isi_state_space_scene_axis(isi_state_space_axis_title(z_col)),
        camera = list(eye = list(x = 1.6, y = 1.7, z = 1.2))
      ),
	      legend = isi_state_space_legend(-0.12),
      margin = list(l = 0, r = 0, t = 80, b = 85),
      hovermode = "closest",
      paper_bgcolor = "#ffffff",
      font = list(color = "#1f2937")
    ) %>% config(displaylogo = FALSE)
  })

	  output$isi_state_space_isomap_3d_plot <- renderPlotly({
	    res <- isi_state_space_isomap_result()
	    dd <- res$scores
	    validate(need(nrow(dd) >= 3, "\u65E0 Isomap 3D \u5F97\u5206\u3002"))
	    dd <- isi_state_space_enrich_scores(dd)
	    x_col <- input$isi_state_space_isomap_3d_x_axis %||% "Isomap1"
	    y_col <- input$isi_state_space_isomap_3d_y_axis %||% "Isomap2"
	    z_col <- input$isi_state_space_isomap_3d_z_axis %||% "Isomap3"
	    needed <- c(x_col, y_col, z_col)
	    validate(need(all(needed %in% names(dd)), "\u6240\u9009 Isomap 3D \u8F74\u5728\u5F53\u524D\u6570\u636E\u4E2D\u4E0D\u5B58\u5728\u3002"))
	    ok <- is.finite(dd[[x_col]]) & is.finite(dd[[y_col]]) & is.finite(dd[[z_col]])
	    dd <- dd[ok, , drop = FALSE]
	    validate(need(nrow(dd) >= 3, "\u6240\u9009 Isomap 3D \u8F74\u7684\u6709\u6548\u70B9\u592A\u5C11\u3002"))
	    p <- plot_ly(source = "isi_state_space_isomap_3d")
	    for (gg in unique(dd$line_group)) {
	      sub <- dd[dd$line_group == gg, , drop = FALSE]
	      if (nrow(sub) < 2) next
	      p <- add_trace(
	        p,
	        data = sub,
	        x = as.formula(paste0("~", x_col)),
	        y = as.formula(paste0("~", y_col)),
	        z = as.formula(paste0("~", z_col)),
	        type = "scatter3d",
	        mode = "lines",
	        line = list(color = "rgba(100,116,139,0.26)", width = 2),
        hoverinfo = "none",
        showlegend = FALSE,
	        inherit = FALSE
	      )
	    }
	    p <- isi_state_space_add_label_markers_3d(p, dd, x_col, y_col, z_col, size = 3.5)
	    diag <- res$diagnostics
	    subtitle <- if (nrow(diag) > 0) {
	      paste0(
        "k=", diag$n_neighbors[1],
        "; embedded ", diag$n_embedded[1], "/", diag$n_input[1],
        "; components=", diag$component_n[1],
        "; residual variance=", ifelse(is.finite(diag$residual_variance[1]), round(diag$residual_variance[1], 3), "NA")
      )
    } else ""
    layout(
      p,
      hoverlabel = stpd_hoverlabel_style(),
      title = list(
        text = paste0("Isomap 3D \u72B6\u6001\u8F68\u8FF9", if (nzchar(subtitle)) paste0("<br><sup>", subtitle, " | ", isi_state_space_short_label(unique(dd$train)[1]), "</sup>") else ""),
        x = 0,
        font = list(size = 14)
	      ),
	      scene = list(
	        xaxis = isi_state_space_scene_axis(isi_state_space_axis_title(x_col)),
	        yaxis = isi_state_space_scene_axis(isi_state_space_axis_title(y_col)),
	        zaxis = isi_state_space_scene_axis(isi_state_space_axis_title(z_col)),
	        camera = list(eye = list(x = 1.6, y = 1.7, z = 1.2))
	      ),
	      legend = isi_state_space_legend(-0.12),
      margin = list(l = 0, r = 0, t = 80, b = 85),
      hovermode = "closest",
      paper_bgcolor = "#ffffff",
      font = list(color = "#1f2937")
    ) %>% config(displaylogo = FALSE)
  })

  output$isi_state_space_variance_table <- DT::renderDT({
    dat <- isi_state_space_pca_result()$variance
    dat$variance_pct <- round(100 * dat$variance, 3)
    dat$cumulative_pct <- round(100 * dat$cumulative, 3)
    DT::datatable(dat[, c("PC", "variance_pct", "cumulative_pct"), drop = FALSE],
                  rownames = FALSE, options = list(dom = "t", pageLength = 5))
  })

  output$isi_state_space_isomap_diagnostics_table <- DT::renderDT({
    dat <- isi_state_space_isomap_result()$diagnostics
    numeric_cols <- names(dat)[vapply(dat, is.numeric, logical(1))]
    for (nm in numeric_cols) dat[[nm]] <- round(dat[[nm]], 4)
    DT::datatable(dat, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
  })

  output$isi_state_space_loading_table <- DT::renderDT({
    dat <- isi_state_space_pca_result()$loadings
    dat$abs_PC1_PC2_PC3 <- pmax(abs(dat$PC1), abs(dat$PC2), abs(dat$PC3), na.rm = TRUE)
    dat <- dat[order(dat$abs_PC1_PC2_PC3, decreasing = TRUE), , drop = FALSE]
    dat$PC1 <- round(dat$PC1, 4)
    dat$PC2 <- round(dat$PC2, 4)
    dat$PC3 <- round(dat$PC3, 4)
    DT::datatable(head(dat[, c("feature", "PC1", "PC2", "PC3"), drop = FALSE], 30),
                  rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })

  output$isi_state_space_feature_table <- DT::renderDT({
    dat <- isi_state_space_feature_data()
    show_cols <- c("train", "idx", "time_mid_sec", "ISI_sec", "log_isi", "label",
                   "local_median_isi_sec", "local_rate_hz", "local_cv2", "local_lv",
                   "prepost_ratio", "delta_logisi", "next_delta_logisi")
    show_cols <- intersect(show_cols, names(dat))
    out <- dat[, show_cols, drop = FALSE]
    numeric_cols <- names(out)[vapply(out, is.numeric, logical(1))]
    for (nm in numeric_cols) out[[nm]] <- round(out[[nm]], 6)
    DT::datatable(out, rownames = FALSE, filter = "top",
                  options = list(pageLength = 20, scrollX = TRUE))
  })

  isi_state_space_parse_int_grid <- function(x, default) {
    x <- as.character(x %||% "")
    vals <- suppressWarnings(as.integer(strsplit(gsub("[;\\s]+", ",", x), ",", fixed = FALSE)[[1]]))
    vals <- vals[is.finite(vals) & vals > 0]
    vals <- sort(unique(vals))
    if (length(vals) == 0) vals <- default
    vals
  }

  isi_state_sequence_data <- reactive({
    feats <- isi_state_space_feature_data()
    out <- data.frame(
      train = as.character(feats$train %||% ""),
      position = seq_len(nrow(feats)),
      row_number = suppressWarnings(as.integer(feats$row_number %||% seq_len(nrow(feats)))),
      idx = suppressWarnings(as.integer(feats$idx %||% seq_len(nrow(feats)))),
      time_mid_sec = suppressWarnings(as.numeric(feats$time_mid_sec %||% NA_real_)),
      duration_isi_sec = suppressWarnings(as.numeric(feats$ISI_sec %||% NA_real_)),
      label = as.character(feats$label %||% "unlabeled"),
      label_source = as.character(feats$label_source %||% input$isi_state_space_label_source %||% "audit_final"),
      stringsAsFactors = FALSE
    )
    out$label[is.na(out$label) | !nzchar(out$label)] <- "unlabeled"
    out
  })

  isi_state_transition_result <- reactive({
    stpd_state_transition_matrix(isi_state_sequence_data(), normalize = "row")
  })

  output$isi_state_transition_heatmap <- renderPlotly({
    tm <- isi_state_transition_result()
    mat <- tm$matrix
    validate(need(length(mat) > 0 && nrow(mat) > 0 && ncol(mat) > 0, "\u65E0\u53EF\u7528\u72B6\u6001\u8F6C\u79FB\u3002"))
    plot_ly(
      x = colnames(mat),
      y = rownames(mat),
      z = mat,
      type = "heatmap",
      colorscale = list(c(0, "#F7FBFF"), c(0.35, "#D4E6F4"), c(0.7, "#7DAED3"), c(1, "#235B8C")),
      colorbar = list(thickness = 12, len = 0.72, outlinewidth = 0, tickfont = list(size = 10, color = "#374151")),
      hovertemplate = "from=%{y}<br>to=%{x}<br>P=%{z:.3f}<extra></extra>"
    ) %>%
      layout(
        title = list(text = "\u72B6\u6001\u8F6C\u79FB\u6982\u7387\u77E9\u9635", x = 0, font = list(size = 14, color = "#111827")),
        xaxis = isi_state_space_axis_style("to"),
        yaxis = isi_state_space_axis_style("from"),
        margin = list(l = 92, r = 30, t = 58, b = 90),
        plot_bgcolor = "#ffffff",
        paper_bgcolor = "#ffffff",
        font = list(color = "#1f2937")
      ) %>%
      config(displaylogo = FALSE)
  })

  output$isi_state_transition_table <- DT::renderDT({
    dat <- isi_state_transition_result()$table
    dat$prob <- round(dat$prob, 4)
    DT::datatable(dat, rownames = FALSE, filter = "top",
                  options = list(pageLength = 10, scrollX = TRUE))
  })

  isi_state_dwell_data <- reactive({
    stpd_state_dwell_times(isi_state_sequence_data())
  })

  output$isi_state_dwell_plot <- renderPlotly({
    dwell <- isi_state_dwell_data()
    validate(need(nrow(dwell) > 0, "\u65E0 dwell-time \u7247\u6BB5\u3002"))
    agg <- stats::aggregate(n_isi ~ label, data = dwell, FUN = sum)
    agg <- agg[order(agg$n_isi, decreasing = TRUE), , drop = FALSE]
    cols <- isi_state_space_color_map(as.character(agg$label))
    plot_ly(
      agg,
      x = ~label,
      y = ~n_isi,
      type = "bar",
      marker = list(color = unname(cols[as.character(agg$label)]), line = list(color = "rgba(255,255,255,0.95)", width = 0.6)),
      hovertemplate = "state=%{x}<br>total ISI=%{y}<extra></extra>"
    ) %>%
      layout(
        title = list(text = "Dwell-time \u603B\u91CF\uFF08ISI \u6570\uFF09", x = 0, font = list(size = 14, color = "#111827")),
        xaxis = isi_state_space_axis_style(""),
        yaxis = isi_state_space_axis_style("ISI count"),
        margin = list(l = 70, r = 18, t = 58, b = 105),
        plot_bgcolor = "#ffffff",
        paper_bgcolor = "#ffffff",
        font = list(color = "#1f2937")
      ) %>%
      config(displaylogo = FALSE)
  })

  output$isi_state_dwell_table <- DT::renderDT({
    dat <- isi_state_dwell_data()
    numeric_cols <- names(dat)[vapply(dat, is.numeric, logical(1))]
    for (nm in numeric_cols) dat[[nm]] <- round(dat[[nm]], 6)
    DT::datatable(dat, rownames = FALSE, filter = "top",
                  options = list(pageLength = 10, scrollX = TRUE))
  })

  output$isi_state_transition_entropy_table <- DT::renderDT({
    dat <- stpd_transition_entropy(isi_state_sequence_data())
    numeric_cols <- names(dat)[vapply(dat, is.numeric, logical(1))]
    for (nm in numeric_cols) dat[[nm]] <- round(dat[[nm]], 5)
    DT::datatable(dat, rownames = FALSE, options = list(pageLength = 8, scrollX = TRUE))
  })

  output$isi_state_motif_table <- DT::renderDT({
    dat <- stpd_motif_frequency(isi_state_sequence_data(), motif_length = 3L)
    dat$rate <- round(dat$rate, 5)
    DT::datatable(head(dat, 30), rownames = FALSE, filter = "top",
                  options = list(pageLength = 10, scrollX = TRUE))
  })

  output$isi_state_surrogate_summary_table <- DT::renderDT({
    seq <- isi_state_sequence_data()
    validate(need(nrow(seq) >= 6, "\u6709\u6548 state \u5E8F\u5217\u592A\u77ED\uFF0C\u65E0\u6CD5\u8FD0\u884C surrogate controls\u3002"))
    res <- stpd_state_surrogate_controls(
      seq,
      n_surrogates = safe_int(input$isi_state_space_surrogate_n, 49L),
      methods = c("label_permutation", "block_shuffle", "run_shuffle", "markov", "renewal"),
      block_length = safe_int(input$isi_state_space_surrogate_block, 10L),
      seed = 1L
    )
    dat <- res$summary
    numeric_cols <- names(dat)[vapply(dat, is.numeric, logical(1))]
    for (nm in numeric_cols) dat[[nm]] <- round(dat[[nm]], 5)
    DT::datatable(dat, rownames = FALSE, filter = "top",
                  options = list(pageLength = 10, scrollX = TRUE))
  })

  isi_state_space_embedding_plot <- function(scores, x_col, y_col, title) {
    dd <- isi_state_space_enrich_scores(scores)
    validate(need(all(c(x_col, y_col) %in% names(dd)), "\u6240\u9009\u5D4C\u5165\u8F74\u4E0D\u5B58\u5728\u3002"))
    ok <- is.finite(dd[[x_col]]) & is.finite(dd[[y_col]])
    dd <- dd[ok, , drop = FALSE]
    validate(need(nrow(dd) >= 2, "\u5D4C\u5165\u6709\u6548\u70B9\u592A\u5C11\u3002"))
    p <- plot_ly()
    for (gg in unique(dd$line_group)) {
      sub <- dd[dd$line_group == gg, , drop = FALSE]
      if (nrow(sub) < 2) next
      p <- add_trace(
        p,
        data = sub,
        x = as.formula(paste0("~", x_col)),
        y = as.formula(paste0("~", y_col)),
        type = "scatter",
        mode = "lines",
        line = list(color = "rgba(100,116,139,0.22)", width = 0.9),
        hoverinfo = "none",
        showlegend = FALSE,
        inherit = FALSE
      )
    }
    p <- isi_state_space_add_label_markers(p, dd, x_col, y_col, size = 5.5)
    isi_state_space_plot_layout(
      p,
      title = title,
      x_title = isi_state_space_axis_title(x_col),
      y_title = isi_state_space_axis_title(y_col),
      legend_y = -0.2,
      margin = list(l = 65, r = 18, t = 64, b = 98)
    ) %>% config(displaylogo = FALSE)
  }

  isi_state_diffusion_result <- reactive({
    feats <- isi_state_space_feature_data()
    validate(need(nrow(feats) >= 10, "\u5F53\u524D\u70B9\u6570\u592A\u5C11\uFF0C\u65E0\u6CD5\u8FD0\u884C Diffusion map\u3002"))
    stpd_run_isi_state_diffusion_map(
      feats,
      ndim = 3L,
      n_neighbors = safe_int(input$isi_state_space_diffusion_neighbors, 15L),
      max_points = safe_int(input$isi_state_space_explore_max_points, 600L),
      scaling = input$isi_state_space_scaling %||% "robust"
    )
  })

  output$isi_state_diffusion_plot <- renderPlotly({
    res <- isi_state_diffusion_result()
    isi_state_space_embedding_plot(res$scores, "Diffusion1", "Diffusion2", "Diffusion map \u63A2\u7D22\u8F68\u8FF9")
  })

  isi_state_phate_result <- reactive({
    feats <- isi_state_space_feature_data()
    validate(need(nrow(feats) >= 10, "\u5F53\u524D\u70B9\u6570\u592A\u5C11\uFF0C\u65E0\u6CD5\u8FD0\u884C PHATE\u3002"))
    stpd_run_isi_state_phate(
      feats,
      ndim = 3L,
      diffusion_time = 5L,
      n_neighbors = safe_int(input$isi_state_space_diffusion_neighbors, 15L),
      max_points = safe_int(input$isi_state_space_explore_max_points, 600L),
      scaling = input$isi_state_space_scaling %||% "robust",
      use_phateR = TRUE
    )
  })

  output$isi_state_phate_plot <- renderPlotly({
    res <- isi_state_phate_result()
    title <- "PHATE / diffusion-potential \u63A2\u7D22\u8F68\u8FF9"
    if (nrow(res$diagnostics) > 0 && nzchar(as.character(res$diagnostics$note[1] %||% ""))) {
      title <- paste0(title, "<br><sup>", res$diagnostics$note[1], "</sup>")
    }
    isi_state_space_embedding_plot(res$scores, "PHATE1", "PHATE2", title)
  })

  isi_state_recurrence_result <- reactive({
    feats <- isi_state_space_feature_data()
    validate(need(nrow(feats) >= 10, "\u5F53\u524D\u70B9\u6570\u592A\u5C11\uFF0C\u65E0\u6CD5\u8FD0\u884C recurrence / RQA\u3002"))
    stpd_make_recurrence_plot(
      feats,
      recurrence_rate = safe_ui_value(input$isi_state_space_recurrence_rate, 0.05),
      max_points = safe_int(input$isi_state_space_explore_max_points, 600L),
      scaling = input$isi_state_space_scaling %||% "robust"
    )
  })

  output$isi_state_recurrence_plot <- renderPlotly({
    rec <- isi_state_recurrence_result()
    z <- rec$matrix * 1
    plot_ly(
      x = seq_len(ncol(z)),
      y = seq_len(nrow(z)),
      z = z,
      type = "heatmap",
      colorscale = list(c(0, "#ffffff"), c(1, "#111827")),
      showscale = FALSE,
      hovertemplate = "i=%{y}<br>j=%{x}<br>recurrent=%{z}<extra></extra>"
    ) %>%
      layout(
        title = list(text = "Recurrence plot", x = 0, font = list(size = 14, color = "#111827")),
        xaxis = isi_state_space_axis_style("state index"),
        yaxis = isi_state_space_axis_style("state index", reversed = TRUE),
        margin = list(l = 70, r = 18, t = 62, b = 70),
        plot_bgcolor = "#ffffff",
        paper_bgcolor = "#ffffff",
        font = list(color = "#1f2937")
      ) %>%
      config(displaylogo = FALSE)
  })

  output$isi_state_rqa_table <- DT::renderDT({
    rec <- isi_state_recurrence_result()
    dat <- cbind(rec$diagnostics, rec$metrics)
    numeric_cols <- names(dat)[vapply(dat, is.numeric, logical(1))]
    for (nm in numeric_cols) dat[[nm]] <- round(dat[[nm]], 5)
    DT::datatable(dat, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
  })

  output$isi_state_isomap_sweep_table <- DT::renderDT({
    feats <- isi_state_space_feature_data()
    grid <- isi_state_space_parse_int_grid(input$isi_state_space_isomap_sweep_grid, c(5L, 8L, 10L, 15L, 20L, 30L))
    validate(need(nrow(feats) >= 20, "Isomap sweep \u9700\u8981\u66F4\u591A\u70B9\u3002"))
    res <- stpd_run_isi_state_isomap_sweep(
      feats,
      neighbor_grid = grid,
      max_points = safe_int(input$isi_state_space_explore_max_points, 600L),
      scaling = input$isi_state_space_scaling %||% "robust"
    )
    dat <- res$diagnostics
    numeric_cols <- names(dat)[vapply(dat, is.numeric, logical(1))]
    for (nm in numeric_cols) dat[[nm]] <- round(dat[[nm]], 5)
    DT::datatable(dat, rownames = FALSE, options = list(pageLength = 8, scrollX = TRUE))
  })

  output$isi_state_umap_tsne_table <- DT::renderDT({
    feats <- isi_state_space_feature_data()
    u <- stpd_run_isi_state_umap(feats, max_points = safe_int(input$isi_state_space_explore_max_points, 600L))
    t <- stpd_run_isi_state_tsne(feats, max_points = safe_int(input$isi_state_space_explore_max_points, 600L))
    ud <- u$diagnostics; ud$visual <- "UMAP"
    td <- t$diagnostics; td$visual <- "t-SNE"
    dat <- dplyr::bind_rows(ud, td)
    DT::datatable(dat, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
  })

  isi_state_rule_states <- reactive({
    stpd_candidate_states_rule_based(isi_state_space_feature_data())
  })

  output$isi_state_rule_counts_table <- DT::renderDT({
    dat <- as.data.frame(table(isi_state_rule_states()$candidate_state), stringsAsFactors = FALSE)
    names(dat) <- c("candidate_state", "n")
    dat <- dat[order(dat$n, decreasing = TRUE), , drop = FALSE]
    DT::datatable(dat, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
  })

  isi_state_gmm_result <- reactive({
    feats <- isi_state_space_feature_data()
    grid <- isi_state_space_parse_int_grid(input$isi_state_space_gmm_states, 2:5)
    validate(need(nrow(feats) >= max(8L, min(grid) + 2L), "\u70B9\u6570\u592A\u5C11\uFF0C\u65E0\u6CD5\u8FD0\u884C GMM\u3002"))
    tryCatch(
      stpd_candidate_states_gmm(
        feats,
        n_states = grid,
        scaling = input$isi_state_space_scaling %||% "robust",
        seed = 1L
      ),
      error = function(e) e
    )
  })

  output$isi_state_gmm_diagnostics_table <- DT::renderDT({
    res <- isi_state_gmm_result()
    if (inherits(res, "error")) {
      dat <- data.frame(message = conditionMessage(res), stringsAsFactors = FALSE)
    } else {
      dat <- res$diagnostics
      numeric_cols <- names(dat)[vapply(dat, is.numeric, logical(1))]
      for (nm in numeric_cols) dat[[nm]] <- round(dat[[nm]], 5)
    }
    DT::datatable(dat, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
  })

  output$isi_state_gmm_state_table <- DT::renderDT({
    res <- isi_state_gmm_result()
    dat <- if (inherits(res, "error")) {
      data.frame(message = conditionMessage(res), stringsAsFactors = FALSE)
    } else {
      res$state_stats
    }
    numeric_cols <- names(dat)[vapply(dat, is.numeric, logical(1))]
    for (nm in numeric_cols) dat[[nm]] <- round(dat[[nm]], 5)
    DT::datatable(dat, rownames = FALSE, options = list(pageLength = 8, scrollX = TRUE))
  })

  isi_state_candidate_labels <- reactive({
    gmm <- isi_state_gmm_result()
    if (!inherits(gmm, "error") && !is.null(gmm$scores$candidate_state)) {
      as.character(gmm$scores$candidate_state)
    } else {
      as.character(isi_state_rule_states()$candidate_state)
    }
  })

  isi_state_hsmm_result <- reactive({
    labels <- isi_state_candidate_labels()
    validate(need(length(labels) >= 6, "\u5019\u9009 state \u5E8F\u5217\u592A\u77ED\uFF0C\u65E0\u6CD5\u8FD0\u884C HSMM-style \u89E3\u7801\u3002"))
    stpd_decode_hsmm(labels, max_duration = 50L)
  })

  output$isi_state_hsmm_segments_table <- DT::renderDT({
    dat <- isi_state_hsmm_result()$segments
    numeric_cols <- names(dat)[vapply(dat, is.numeric, logical(1))]
    for (nm in numeric_cols) dat[[nm]] <- round(dat[[nm]], 6)
    DT::datatable(dat, rownames = FALSE, filter = "top",
                  options = list(pageLength = 10, scrollX = TRUE))
  })

  output$isi_state_hsmm_agreement_table <- DT::renderDT({
    hs <- isi_state_hsmm_result()
    dat <- stpd_label_agreement(hs$decoded, isi_state_candidate_labels())$per_label
    numeric_cols <- names(dat)[vapply(dat, is.numeric, logical(1))]
    for (nm in numeric_cols) dat[[nm]] <- round(dat[[nm]], 5)
    DT::datatable(dat, rownames = FALSE, options = list(pageLength = 8, scrollX = TRUE))
  })

  output$isi_state_model_validation_table <- DT::renderDT({
    labels <- isi_state_candidate_labels()
    held <- tryCatch(stpd_hsmm_heldout_likelihood(labels, max_duration = 50L), error = function(e) data.frame(metric = "heldout_error", value = conditionMessage(e), stringsAsFactors = FALSE))
    boot <- tryCatch(stpd_state_bootstrap_metrics(labels, n_bootstrap = 49L, seed = 1L)$summary, error = function(e) data.frame(metric = "bootstrap_error", value = conditionMessage(e), stringsAsFactors = FALSE))
    held_long <- data.frame(metric = names(held), value = as.character(unlist(held[1, , drop = TRUE])), stringsAsFactors = FALSE)
    boot$metric <- paste0("bootstrap_", boot$metric)
    dat <- dplyr::bind_rows(held_long, boot)
    numeric_cols <- names(dat)[vapply(dat, is.numeric, logical(1))]
    for (nm in numeric_cols) dat[[nm]] <- round(dat[[nm]], 5)
    DT::datatable(dat, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })

  isi_state_train_transition_data <- reactive({
    td <- current_trains()
    trains <- intersect(metadata_filtered_train_names(), names(td))
    if (length(trains) == 0) trains <- names(td)
    meta <- tryCatch(current_train_metadata(), error = function(e) data.frame(train = names(td), stringsAsFactors = FALSE))
    stpd_build_transition_model_data(
      td,
      metadata = meta,
      selected_trains = trains,
      label_source = input$isi_state_space_label_source %||% "audit_final",
      min_isi_sec = min_valid_isi_sec(),
      auto_others = isTRUE(input$auto_others),
      drop_unlabeled = TRUE
    )
  })

  output$isi_state_train_transition_model_data_table <- DT::renderDT({
    dat <- isi_state_train_transition_data()
    validate(need(nrow(dat) > 0, "\u65E0\u53EF\u7528\u8DE8 train \u8F6C\u79FB\u6570\u636E\u3002"))
    DT::datatable(dat, rownames = FALSE, filter = "top",
                  options = list(pageLength = 10, scrollX = TRUE))
  })

  output$isi_state_train_transition_model_summary_table <- DT::renderDT({
    dat <- isi_state_train_transition_data()
    validate(need(nrow(dat) > 0, "\u65E0\u53EF\u7528\u8DE8 train \u8F6C\u79FB\u6570\u636E\u3002"))
    fixed <- intersect(c("from", "structure", "nucleus", "side"), names(dat))
    if (length(fixed) == 0) fixed <- "from"
    fit <- tryCatch(
      stpd_fit_transition_statistical_model(dat, fixed_effects = fixed, method = "one_vs_rest_glm"),
      error = function(e) e
    )
    if (inherits(fit, "error")) {
      out <- data.frame(method = "one_vs_rest_glm", message = conditionMessage(fit), stringsAsFactors = FALSE)
    } else {
      out <- data.frame(
        target_state = names(fit$fits),
        AIC = vapply(fit$fits, stats::AIC, numeric(1)),
        n = nrow(fit$data),
        fixed_effects = paste(fit$fixed_effects, collapse = " + "),
        stringsAsFactors = FALSE
      )
      out$AIC <- round(out$AIC, 4)
    }
    DT::datatable(out, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
  })

  state_trajectory_meta_col <- function(meta, nm, default = "") {
    n <- if (is.data.frame(meta)) nrow(meta) else 0L
    if (n <= 0L) return(character(0))
    if (nm %in% names(meta)) meta[[nm]] else rep(default, n)
  }

  state_trajectory_dataset_ids <- reactive({
    ds <- rv$datasets
    if (length(ds) == 0L) return(character(0))
    ids <- as.character(input$state_trajectory_dataset_ids %||% character(0))
    ids <- intersect(ids, names(ds))
    if (length(ids) == 0L) {
      fallback <- rv$current_id %||% names(ds)[1]
      ids <- intersect(fallback, names(ds))
    }
    ids
  })

  state_trajectory_train_labels <- function(meta, trains) {
    trains <- as.character(trains)
    labs <- trains
    if (!is.data.frame(meta) || nrow(meta) == 0L || !("train" %in% names(meta))) return(labs)
    mm <- meta[match(trains, as.character(meta$train)), , drop = FALSE]
    dataset <- as.character(state_trajectory_meta_col(mm, "dataset", ""))
    structure <- as.character(state_trajectory_meta_col(mm, "structure", ""))
    side <- as.character(state_trajectory_meta_col(mm, "side", ""))
    depth <- suppressWarnings(as.numeric(state_trajectory_meta_col(mm, "recording_depth", NA_real_)))
    source_train <- as.character(state_trajectory_meta_col(mm, "source_train", ""))
    source_train[is.na(source_train) | !nzchar(source_train)] <- trains[is.na(source_train) | !nzchar(source_train)]
    prefix <- paste0(
      ifelse(!is.na(dataset) & nzchar(dataset), paste0("[", dataset, "] "), ""),
      ifelse(!is.na(structure) & nzchar(structure), paste0(structure, " "), ""),
      ifelse(!is.na(side) & nzchar(side), paste0(side, " "), ""),
      ifelse(is.finite(depth), paste0("D", signif(depth, 4), " | "), "")
    )
    paste0(prefix, source_train)
  }

  output$state_trajectory_dataset_selector <- renderUI({
    ds <- rv$datasets
    if (length(ds) == 0L) return(NULL)
    ids <- names(ds)
	    labels <- vapply(ids, function(id) {
	      d <- ds[[id]]
	      display <- as.character(d$meta$display_name %||% id)[1]
	      source <- as.character(d$meta$source %||% "")[1]
	      if (is.na(display) || !nzchar(display)) display <- id
	      if (is.na(source)) source <- ""
	      paste0("[", source, "] ", display, " (", length(d$trains %||% list()), " trains)")
	    }, character(1))
    selected <- state_trajectory_dataset_ids()
    if (length(selected) == 0L) selected <- rv$current_id %||% ids[1]
    selectizeInput(
      "state_trajectory_dataset_ids",
      "\u5206\u6790\u6570\u636E\u96C6",
      choices = stats::setNames(ids, labels),
      selected = selected,
      multiple = TRUE,
      options = list(plugins = list("remove_button"), placeholder = "\u53EF\u9009\u591A\u4E2A\u5DF2\u52A0\u8F7D\u6570\u636E\u96C6")
    )
  })

  state_trajectory_pool_data <- reactive({
    ds_all <- rv$datasets
    ids <- state_trajectory_dataset_ids()
    validate(need(length(ids) > 0L, "\u8BF7\u5148\u9009\u62E9\u7528\u4E8E state trajectory \u7684\u6570\u636E\u96C6\u3002"))
    multi_dataset <- length(ids) > 1L
    trains <- list()
    meta_parts <- list()
    for (id in ids) {
      if (!(id %in% names(ds_all))) next
      ds <- normalize_dataset(ds_all[[id]])
      raw_names <- names(ds$trains %||% list())
      if (length(raw_names) == 0L) next
	      display <- as.character(ds$meta$display_name %||% id)[1]
	      if (is.na(display) || !nzchar(display)) display <- id
	      source <- as.character(ds$meta$source %||% "")[1]
	      if (is.na(source)) source <- ""
      parsed <- tryCatch(
        parse_spike_train_column_metadata(raw_names, dataset_name = display),
        error = function(e) data.frame(train = raw_names, stringsAsFactors = FALSE)
      )
      stored <- ds$meta$train_metadata
      meta <- parsed
      if (is.data.frame(stored) && nrow(stored) > 0L && "train" %in% names(stored)) {
        stored <- stored[match(raw_names, as.character(stored$train)), , drop = FALSE]
        for (nm in setdiff(names(stored), "train")) {
          vals <- stored[[nm]]
          if (!(nm %in% names(meta))) {
            meta[[nm]] <- vals
          } else {
            replace <- !is.na(vals)
            meta[[nm]][replace] <- vals[replace]
          }
        }
      }
      stats <- lapply(raw_names, function(tr) {
        dat <- ds$trains[[tr]]
        ts <- if (!is.null(dat) && "timestamp_sec" %in% names(dat)) suppressWarnings(as.numeric(dat$timestamp_sec)) else numeric(0)
        ts <- ts[is.finite(ts)]
        data.frame(
          train = tr,
          n_spikes = length(ts),
          first_timestamp_sec = if (length(ts) > 0) min(ts, na.rm = TRUE) else NA_real_,
          last_timestamp_sec = if (length(ts) > 0) max(ts, na.rm = TRUE) else NA_real_,
          duration_sec = if (length(ts) >= 2) max(ts, na.rm = TRUE) - min(ts, na.rm = TRUE) else NA_real_,
          stringsAsFactors = FALSE
        )
      })
      stats <- dplyr::bind_rows(stats)
      meta <- dplyr::left_join(meta, stats, by = "train")
      keys <- if (multi_dataset) paste0(id, "::", raw_names) else raw_names
      for (ii in seq_along(raw_names)) trains[[keys[ii]]] <- ds$trains[[raw_names[ii]]]
      meta$source_train <- raw_names
      meta$train <- keys
      meta$dataset_id <- id
      meta$dataset <- display
      meta$dataset_source <- source
      meta_parts[[id]] <- meta
    }
    meta_all <- dplyr::bind_rows(meta_parts)
    filtered <- names(trains)
    if (isTRUE(input$use_train_metadata_filter) &&
        is.data.frame(meta_all) && nrow(meta_all) > 0L && "train" %in% names(meta_all)) {
      m <- meta_all[as.character(meta_all$train) %in% filtered, , drop = FALSE]
      keep_all_if_empty <- function(x) is.null(x) || length(x) == 0L
      if ("structure" %in% names(m) && !keep_all_if_empty(input$metadata_filter_structure)) {
        m <- m[as.character(m$structure) %in% as.character(input$metadata_filter_structure), , drop = FALSE]
      }
      if ("side" %in% names(m) && !keep_all_if_empty(input$metadata_filter_side)) {
        m <- m[as.character(m$side) %in% as.character(input$metadata_filter_side), , drop = FALSE]
      }
      if ("trajectory" %in% names(m) && !keep_all_if_empty(input$metadata_filter_trajectory)) {
        m <- m[as.character(m$trajectory) %in% as.character(input$metadata_filter_trajectory), , drop = FALSE]
      }
      if ("recording_depth" %in% names(m) && !is.null(input$metadata_filter_depth) && length(input$metadata_filter_depth) == 2L) {
        d <- suppressWarnings(as.numeric(m$recording_depth))
        rng <- suppressWarnings(as.numeric(input$metadata_filter_depth))
        if (all(is.finite(rng))) m <- m[is.finite(d) & d >= min(rng) & d <= max(rng), , drop = FALSE]
      }
      filtered <- intersect(as.character(m$train), names(trains))
    }
    label_map <- stats::setNames(state_trajectory_train_labels(meta_all, names(trains)), names(trains))
    list(
      trains = trains,
      meta = meta_all,
      filtered_trains = filtered,
      dataset_ids = ids,
      train_labels = label_map
    )
  })

  output$state_trajectory_train_selector <- renderUI({
    pool <- state_trajectory_pool_data()
    trains <- intersect(pool$filtered_trains %||% character(0), names(pool$trains))
    if (length(trains) == 0L) trains <- names(pool$trains)
    if (length(trains) == 0L) {
      return(tags$div(class = "small-note", "\u8BF7\u5148\u4E0A\u4F20 spike train \u6570\u636E\u3002"))
    }
    labs <- pool$train_labels[trains]
    labs[is.na(labs) | !nzchar(labs)] <- trains[is.na(labs) | !nzchar(labs)]
    selected <- intersect(as.character(isolate(input$state_trajectory_trains) %||% character(0)), trains)
    selectizeInput(
      "state_trajectory_trains",
      "\u9009\u62E9 spike trains",
      choices = stats::setNames(trains, labs),
      selected = selected,
      multiple = TRUE,
      options = list(
        plugins = list("remove_button"),
        closeAfterSelect = TRUE,
        placeholder = "\u8BF7\u624B\u52A8\u9009\u62E9 trains\uFF08\u9ED8\u8BA4\u4E0D\u5168\u9009\uFF09"
      )
    )
  })

  state_trajectory_selected_trains <- shiny::debounce(reactive({
    pool <- state_trajectory_pool_data()
    selected <- as.character(input$state_trajectory_trains %||% character(0))
    intersect(selected, names(pool$trains))
  }), millis = 300)

  state_trajectory_result <- reactive({
    pool <- state_trajectory_pool_data()
    td <- pool$trains
    selected <- state_trajectory_selected_trains()
    selected <- intersect(selected, names(td))
    validate(need(length(selected) >= 1L, "\u8BF7\u81F3\u5C11\u9009\u62E9\u4E00\u6761 spike train\u3002"))
    bin_sec <- suppressWarnings(as.numeric(input$state_trajectory_bin_ms %||% 100))[1] / 1000
    if (!is.finite(bin_sec) || bin_sec <= 0) bin_sec <- 0.1
    time_origin <- input$state_trajectory_time_origin %||% "aligned"
    start_sec <- suppressWarnings(as.numeric(input$state_trajectory_start_sec %||% 0))[1]
    if (!is.finite(start_sec) || start_sec < 0) start_sec <- NULL
    if (identical(time_origin, "raw") && isTRUE(all.equal(start_sec, 0))) start_sec <- NULL
    end_sec <- suppressWarnings(as.numeric(input$state_trajectory_end_sec %||% 0))[1]
    if (!is.finite(end_sec) || (!is.null(start_sec) && end_sec <= start_sec) || end_sec <= 0) end_sec <- NULL
    coord_mode <- input$state_trajectory_coordinate_mode %||% "pattern_axes"
    embedding_methods <- unique(c("pca", if (coord_mode %in% c("fa", "isomap", "tsne", "umap")) coord_mode else character(0)))
    res <- stpd_make_state_trajectory(
      td,
      selected_trains = selected,
      bin_sec = bin_sec,
      start_sec = start_sec,
      end_sec = end_sec,
      time_origin = time_origin,
      label_source = input$state_trajectory_label_source %||% "audit_final",
      min_isi_sec = min_valid_isi_sec(),
      auto_others = isTRUE(input$auto_others),
      smoothing_sigma_bins = input$state_trajectory_smooth_bins %||% 1,
      embedding_methods = embedding_methods,
      embedding_n_neighbors = input$state_trajectory_n_neighbors %||% 15,
      embedding_tsne_perplexity = input$state_trajectory_tsne_perplexity %||% 30,
      embedding_umap_min_dist = input$state_trajectory_umap_min_dist %||% 0.1,
      embedding_seed = input$state_trajectory_embedding_seed %||% 1,
      embedding_max_points = input$state_trajectory_embedding_max_points %||% 900
    )
    validate(need(nrow(res$features %||% data.frame()) >= 2L, "\u6709\u6548 time bin \u592A\u5C11\uFF0C\u65E0\u6CD5\u6784\u5EFA\u8F68\u8FF9\u3002"))
    res$selected_dataset_ids <- pool$dataset_ids
    res$train_metadata <- pool$meta
    res$selected_train_labels <- pool$train_labels[res$selected_trains]
    res
  })

  output$state_trajectory_window_summary <- renderUI({
    res <- state_trajectory_result()
    ws <- res$window_summary
    tw <- res$train_windows
    if (is.null(ws) || nrow(ws) == 0L) return(NULL)
    fmt <- function(x, digits = 3) {
      if (!is.finite(x)) return("NA")
      format(round(x, digits), trim = TRUE, nsmall = min(1L, digits))
    }
    msg <- paste0(
      "\u5B9E\u9645\u5206\u6790\u7A97\uFF1A",
      fmt(ws$window_start_sec[1]), "-", fmt(ws$window_end_sec[1]), " s",
      " \uFF08", fmt(ws$window_duration_sec[1]), " s\uFF09; ",
      ws$n_bins[1], " bins @ ", fmt(1000 * ws$bin_sec[1], 1), " ms. ",
      "\u6240\u9009 train \u539F\u59CB\u65F6\u957F\uFF1Amedian ",
      fmt(ws$train_duration_median_sec[1]), " s",
      " \uFF08range ", fmt(ws$train_duration_min_sec[1]), "-",
      fmt(ws$train_duration_max_sec[1]), " s\uFF09."
    )
    if (is.data.frame(tw) && nrow(tw) > 0L) {
      raw_start <- suppressWarnings(min(tw$raw_start_sec, na.rm = TRUE))
      raw_end <- suppressWarnings(max(tw$raw_end_sec, na.rm = TRUE))
      if (is.finite(raw_start) && is.finite(raw_end)) {
        msg <- paste0(msg, " Raw timestamp range: ", fmt(raw_start), "-", fmt(raw_end), " s.")
      }
    }
    tags$div(class = "small-note state-trajectory-window-summary", msg)
  })

	  output$state_pair_controls <- renderUI({
	    res <- state_trajectory_result()
	    trains <- as.character(res$selected_trains %||% character(0))
	    if (length(trains) < 2L) {
	      return(tags$div(class = "small-note", "State-pair analysis requires at least two selected spike trains."))
	    }
	    labs <- res$selected_train_labels %||% stats::setNames(trains, trains)
	    labs <- labs[trains]
	    labs[is.na(labs) | !nzchar(labs)] <- trains[is.na(labs) | !nzchar(labs)]
	    train_choices <- stats::setNames(trains, labs)
	    fluidRow(
	      column(6, selectizeInput("state_pair_trains", "Trains for joint-state analysis",
	                               choices = train_choices,
	                               selected = trains[seq_len(min(4L, length(trains)))],
	                               multiple = TRUE)),
      column(2, numericInput("state_pair_lag_bins", "Lag bins", value = 0, min = -1000, max = 1000, step = 1)),
      column(4, selectInput(
        "state_pair_heatmap_value", "Matrix value",
        choices = c(
          "log2 enrichment" = "log2_enrichment",
          "observed count" = "observed_count",
          "observed probability" = "observed_prob",
          "standardized residual" = "standardized_residual",
          "observed / expected" = "observed_expected_ratio"
        ),
        selected = "log2_enrichment"
      )),
      column(
        12,
	        tags$div(
	          class = "small-note",
	          "Select two or more spike trains. Lag bins > 0 pairs the first selected train at time t with the other selected trains at t + lag. Enrichment compares observed joint-state counts with the independence expectation from each train's marginal state frequencies."
	        )
      )
    )
  })

  state_pair_result <- reactive({
    res <- state_trajectory_result()
    validate(need(length(res$selected_trains %||% character(0)) >= 2L, "\u8BF7\u81F3\u5C11\u9009\u62E9\u4E24\u6761 spike train \u8FDB\u884C state-pair \u5206\u6790\u3002"))
    lag_bins <- suppressWarnings(as.integer(round(as.numeric(input$state_pair_lag_bins %||% 0)[1])))
    if (!is.finite(lag_bins)) lag_bins <- 0L
    chosen <- as.character(input$state_pair_trains %||% res$selected_trains[seq_len(min(4L, length(res$selected_trains)))])
    chosen <- chosen[nzchar(chosen) & chosen %in% res$selected_trains]
    if (length(chosen) < 2L) chosen <- res$selected_trains[seq_len(min(2L, length(res$selected_trains)))]
    out <- stpd_make_state_pair_analysis(
      res,
      trains = chosen,
      lag_bins = lag_bins
    )
    validate(need(nrow(out$pair_bins %||% data.frame()) >= 1L, "\u6CA1\u6709\u53EF\u7528\u7684 state-pair bins\u3002"))
    out
  })

  output$state_pair_timeline_plot <- renderPlotly({
    stpd_state_pair_timeline_plot(state_pair_result())
  })

  output$state_pair_heatmap_plot <- renderPlotly({
    value <- input$state_pair_heatmap_value %||% "log2_enrichment"
    stpd_state_pair_heatmap(state_pair_result(), value = value)
  })

  output$state_pair_transition_heatmap_plot <- renderPlotly({
    stpd_state_pair_transition_heatmap(state_pair_result(), value = "prob")
  })

  output$state_pair_matrix_table <- DT::renderDT({
    dat <- state_pair_result()$matrix
	    if (is.null(dat) || nrow(dat) == 0L) {
	      dat <- data.frame(message = "No state-pair matrix available.", stringsAsFactors = FALSE)
	    } else {
	      state_cols <- names(dat)[grepl("^state__", names(dat))]
	      show_cols <- unique(c("state_x", "state_y", state_cols, "joint_state_labeled", "joint_state",
	                            "observed_count", "expected_count", "observed_prob",
	                            "expected_prob", "observed_expected_ratio", "log2_enrichment",
	                            "standardized_residual", "odds_ratio", "p_value", "p_fdr",
	                            "association", "complete_joint_grid"))
	      dat <- dat[, intersect(show_cols, names(dat)), drop = FALSE]
	      numeric_cols <- names(dat)[vapply(dat, is.numeric, logical(1))]
	      for (nm in numeric_cols) dat[[nm]] <- round(dat[[nm]], 6)
    }
    DT::datatable(dat, rownames = FALSE, filter = "top",
                  options = list(pageLength = 12, scrollX = TRUE))
  })

  output$state_pair_transition_table <- DT::renderDT({
    dat <- state_pair_result()$transitions
    if (is.null(dat) || nrow(dat) == 0L) {
      dat <- data.frame(message = "At least two paired bins are required for joint-state transitions.", stringsAsFactors = FALSE)
    } else {
      dat$prob <- round(dat$prob, 6)
    }
    DT::datatable(dat, rownames = FALSE, filter = "top",
                  options = list(pageLength = 12, scrollX = TRUE))
  })

	  output$state_pair_bin_table <- DT::renderDT({
	    dat <- state_pair_result()$pair_bins
	    state_cols <- names(dat)[grepl("^state__", names(dat))]
	    fraction_cols <- names(dat)[grepl("^state_fraction__", names(dat))]
	    rate_cols <- names(dat)[grepl("^state_rate_hz__", names(dat))]
	    show_cols <- unique(c("pair_bin_id", "bin_id", "bin_id_y", "bin_start_sec", "bin_end_sec",
	                          "time_mid_sec", "time_mid_sec_y", "train_x", "state_x",
	                          "state_x_fraction", "state_x_rate_hz", "train_y", "state_y",
	                          "state_y_fraction", "state_y_rate_hz", state_cols, fraction_cols,
	                          rate_cols, "joint_state_labeled", "joint_state", "lag_bins", "train_count"))
	    dat <- dat[, intersect(show_cols, names(dat)), drop = FALSE]
    numeric_cols <- names(dat)[vapply(dat, is.numeric, logical(1))]
    for (nm in numeric_cols) dat[[nm]] <- round(dat[[nm]], 6)
    DT::datatable(dat, rownames = FALSE, filter = "top",
                  options = list(pageLength = 15, scrollX = TRUE))
  })

  output$state_trajectory_plot <- renderPlotly({
    res <- state_trajectory_result()
    mode <- input$state_trajectory_coordinate_mode %||% "pattern_axes"
    title_prefix <- switch(
      mode,
      pca = "PCA state trajectory",
      fa = "Factor-analysis state trajectory",
      isomap = "Isomap state trajectory",
      tsne = "t-SNE state trajectory",
      umap = "UMAP state trajectory",
      "Custom pattern-state trajectory"
    )
    title <- paste0(title_prefix, " | ", length(res$selected_trains %||% character(0)), " trains")
    axis_cols <- c(
      input$state_trajectory_x_axis %||% "burst_activity",
      input$state_trajectory_y_axis %||% "pause_activity",
      input$state_trajectory_z_axis %||% "tonic_activity"
    )
    stpd_state_trajectory_plot(
      res,
      coordinate_mode = input$state_trajectory_coordinate_mode %||% "pattern_axes",
      axis_cols = axis_cols,
      title = title
    )
  })

  output$state_trajectory_feature_table <- DT::renderDT({
    dat <- state_trajectory_result()$features
    show_cols <- c(
      "bin_id", "bin_start_sec", "bin_end_sec", "time_mid_sec", "n_trains",
      "firing_rate_hz", "burst_activity", "pause_activity", "tonic_activity", "hf_spiking_activity",
      "burst_fraction", "pause_fraction", "tonic_fraction", "hf_spiking_fraction",
      "dominant_state", "PC1", "PC2", "PC3", "FA1", "FA2", "FA3",
      "Isomap1", "Isomap2", "Isomap3", "tSNE1", "tSNE2", "tSNE3",
      "UMAP1", "UMAP2", "UMAP3"
    )
    show_cols <- intersect(show_cols, names(dat))
    out <- dat[, show_cols, drop = FALSE]
    numeric_cols <- names(out)[vapply(out, is.numeric, logical(1))]
    for (nm in numeric_cols) out[[nm]] <- round(out[[nm]], 6)
    DT::datatable(out, rownames = FALSE, filter = "top",
                  options = list(pageLength = 15, scrollX = TRUE))
  })

  output$state_trajectory_variance_table <- DT::renderDT({
    res <- state_trajectory_result()
    dat <- res$embedding_diagnostics %||% data.frame()
    if (is.null(dat) || nrow(dat) == 0L) {
      dat <- data.frame(message = "No embedding diagnostics available.", stringsAsFactors = FALSE)
    } else {
      if (is.data.frame(res$variance) && nrow(res$variance) > 0L) {
        pca_var <- data.frame(
          method = "PCA",
          component = res$variance$PC,
          metric = "variance_pct",
          value = as.character(round(100 * res$variance$variance, 3)),
          note = "Percentage of scaled feature variance explained by each principal component.",
          stringsAsFactors = FALSE
        )
        dat <- rbind(dat, pca_var)
      }
    }
    DT::datatable(dat, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
  })

  output$state_trajectory_loading_table <- DT::renderDT({
    res <- state_trajectory_result()
    mode <- input$state_trajectory_coordinate_mode %||% "pattern_axes"
    dat <- if (identical(mode, "fa") && is.data.frame(res$fa_loadings) && nrow(res$fa_loadings) > 0L) {
      res$fa_loadings
    } else {
      res$loadings
    }
    if (is.null(dat) || nrow(dat) == 0L) {
      dat <- data.frame(message = "No linear loadings available for the current feature matrix.", stringsAsFactors = FALSE)
    } else {
      loading_cols <- intersect(c("PC1", "PC2", "PC3", "FA1", "FA2", "FA3"), names(dat))
      if (length(loading_cols) > 0L) {
        dat$max_abs_loading <- do.call(pmax, c(lapply(dat[loading_cols], function(x) abs(suppressWarnings(as.numeric(x)))), list(na.rm = TRUE)))
        dat <- dat[order(dat$max_abs_loading, decreasing = TRUE), , drop = FALSE]
      }
      numeric_cols <- names(dat)[vapply(dat, is.numeric, logical(1))]
      for (nm in numeric_cols) dat[[nm]] <- round(dat[[nm]], 5)
      keep <- intersect(c("feature", "PC1", "PC2", "PC3", "FA1", "FA2", "FA3", "uniqueness"), names(dat))
      dat <- head(dat[, keep, drop = FALSE], 30)
    }
    DT::datatable(dat, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })

	  output$state_trajectory_transition_table <- DT::renderDT({
	    dat <- state_trajectory_result()$features
	    states <- c("burst", "pause", "tonic", "hf_spiking", "others", "unlabeled")
	    tm <- stpd_state_transition_matrix(dat$dominant_state, states = states, normalize = "row")
    out <- tm$table
    out <- out[out$n > 0 | is.finite(out$prob), , drop = FALSE]
    out$prob <- round(out$prob, 5)
	    DT::datatable(out, rownames = FALSE, filter = "top",
	                  options = list(pageLength = 12, scrollX = TRUE))
	  })

	  output$event_aligned_train_selector <- renderUI({
	    ds <- tryCatch(current_dataset(), error = function(e) NULL)
	    if (is.null(ds) || is.null(ds$trains) || length(ds$trains) == 0L) {
	      return(tags$div(class = "small-note", "Please load spike-train data first."))
	    }
	    ds <- normalize_dataset(ds)
	    trains <- names(ds$trains)
	    meta <- tryCatch(
	      parse_spike_train_column_metadata(trains, dataset_name = ds$meta$display_name %||% ""),
	      error = function(e) data.frame(train = trains, stringsAsFactors = FALSE)
	    )
	    labs <- state_trajectory_train_labels(meta, trains)
	    labs[is.na(labs) | !nzchar(labs)] <- trains[is.na(labs) | !nzchar(labs)]
	    selected <- intersect(as.character(isolate(input$event_aligned_trains) %||% character(0)), trains)
	    if (length(selected) == 0L) selected <- trains[seq_len(min(12L, length(trains)))]
	    selectizeInput(
	      "event_aligned_trains",
	      "Spike trains / neurons",
	      choices = stats::setNames(trains, labs),
	      selected = selected,
	      multiple = TRUE,
	      options = list(plugins = list("remove_button"), closeAfterSelect = TRUE,
	                     placeholder = "Select trains for event-aligned activity")
	    )
	  })

	  output$event_aligned_event_selector <- renderUI({
	    events <- task_events_current()
	    if (nrow(events) == 0L) {
	      return(tags$div(class = "small-note", "\u5F53\u524D\u6570\u636E\u96C6\u6CA1\u6709 Event / Event_* \u4EFB\u52A1\u4E8B\u4EF6\u5217\u3002"))
	    }
	    all_names <- sort(unique(as.character(events$event_name)))
	    selected_names <- intersect(as.character(isolate(input$event_aligned_event_names) %||% all_names), all_names)
	    if (length(selected_names) == 0L) selected_names <- all_names
	    tagList(
	      selectizeInput(
	        "event_aligned_event_names",
	        "\u4EFB\u52A1\u4E8B\u4EF6\u7C7B\u578B",
	        choices = all_names,
	        selected = selected_names,
	        multiple = TRUE,
	        options = list(plugins = list("remove_button"), closeAfterSelect = TRUE)
	      ),
	      tags$div(class = "small-note", paste0("Available event timestamps: ", nrow(events), "."))
	    )
	  })

	  event_aligned_selected_trains <- shiny::debounce(reactive({
	    ds <- tryCatch(current_dataset(), error = function(e) NULL)
	    if (is.null(ds) || is.null(ds$trains)) return(character(0))
	    selected <- as.character(input$event_aligned_trains %||% character(0))
	    intersect(selected, names(ds$trains))
	  }), millis = 300)

	  event_aligned_events_filtered <- reactive({
	    events <- task_events_current()
	    if (nrow(events) == 0L) return(stpd_empty_task_events())
	    all_names <- sort(unique(as.character(events$event_name)))
	    selected <- as.character(input$event_aligned_event_names %||% all_names)
	    selected <- intersect(selected, all_names)
	    if (length(selected) == 0L) selected <- all_names
	    events[as.character(events$event_name) %in% selected, , drop = FALSE]
	  })

	  event_aligned_result <- reactive({
	    ds <- current_dataset()
	    ds <- normalize_dataset(ds)
	    selected <- event_aligned_selected_trains()
	    validate(need(length(selected) >= 1L, "Select at least one spike train / neuron."))
	    events <- event_aligned_events_filtered()
	    validate(need(nrow(events) > 0L, "Load or select at least one task event from Event / Event_* columns."))
	    bin_sec <- suppressWarnings(as.numeric(input$event_aligned_bin_ms %||% 50))[1] / 1000
	    if (!is.finite(bin_sec) || bin_sec <= 0) bin_sec <- 0.05
	    pre_sec <- suppressWarnings(as.numeric(input$event_aligned_pre_sec %||% 1))[1]
	    post_sec <- suppressWarnings(as.numeric(input$event_aligned_post_sec %||% 2))[1]
	    smooth_bins <- suppressWarnings(as.numeric(input$event_aligned_smooth_bins %||% 1))[1]
	    base_start <- suppressWarnings(as.numeric(input$event_aligned_baseline_start_sec %||% -1))[1]
	    base_end <- suppressWarnings(as.numeric(input$event_aligned_baseline_end_sec %||% -0.2))[1]
	    lag_sec <- suppressWarnings(as.numeric(input$event_aligned_correlogram_lag_ms %||% 250))[1] / 1000
	    lag_bin_sec <- suppressWarnings(as.numeric(input$event_aligned_correlogram_bin_ms %||% 50))[1] / 1000
	    max_pairs <- suppressWarnings(as.integer(round(as.numeric(input$event_aligned_max_pairs %||% 30)[1])))
	    res <- stpd_event_aligned_activity(
	      ds$trains,
	      task_events = events,
	      selected_trains = selected,
	      event_names = sort(unique(as.character(events$event_name))),
	      pre_sec = pre_sec,
	      post_sec = post_sec,
	      bin_sec = bin_sec,
	      smoothing_sigma_bins = smooth_bins,
	      baseline_start_sec = base_start,
	      baseline_end_sec = base_end,
	      label_source = input$event_aligned_label_source %||% "audit_final",
	      min_isi_sec = min_valid_isi_sec(),
	      auto_others = FALSE,
	      correlogram_lag_sec = lag_sec,
	      correlogram_bin_sec = lag_bin_sec,
	      max_correlogram_pairs = max_pairs
	    )
	    validate(need(identical(res$status, "ok"), res$message %||% "Event-aligned activity failed."))
	    res
	  })

	  output$event_aligned_raster_plot <- renderPlotly({
	    max_spikes <- suppressWarnings(as.integer(round(as.numeric(input$event_aligned_max_raster_spikes %||% 5000)[1])))
	    stpd_event_aligned_raster_plot(event_aligned_result(), max_spikes = max_spikes)
	  })

	  output$event_aligned_psth_plot <- renderPlotly({
	    stpd_event_aligned_psth_plot(event_aligned_result())
	  })

	  output$event_aligned_population_plot <- renderPlotly({
	    stpd_event_aligned_population_plot(event_aligned_result())
	  })

	  output$event_aligned_heatmap_plot <- renderPlotly({
	    stpd_event_aligned_heatmap_plot(event_aligned_result())
	  })

	  output$event_aligned_correlation_plot <- renderPlotly({
	    stpd_event_aligned_correlation_plot(event_aligned_result())
	  })

	  output$event_aligned_correlogram_plot <- renderPlotly({
	    stpd_event_aligned_correlogram_plot(event_aligned_result())
	  })

	  output$event_aligned_summary_table <- DT::renderDT({
	    dat <- event_aligned_result()$summary %||% data.frame()
	    DT::datatable(dat, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
	  })

	  output$event_aligned_population_table <- DT::renderDT({
	    dat <- event_aligned_result()$population %||% data.frame()
	    numeric_cols <- names(dat)[vapply(dat, is.numeric, logical(1))]
	    for (nm in numeric_cols) dat[[nm]] <- round(dat[[nm]], 6)
	    DT::datatable(dat, rownames = FALSE, filter = "top",
	                  options = list(pageLength = 12, scrollX = TRUE))
	  })

	  output$event_aligned_psth_table <- DT::renderDT({
	    dat <- event_aligned_result()$psth %||% data.frame()
	    numeric_cols <- names(dat)[vapply(dat, is.numeric, logical(1))]
	    for (nm in numeric_cols) dat[[nm]] <- round(dat[[nm]], 6)
	    DT::datatable(dat, rownames = FALSE, filter = "top",
	                  options = list(pageLength = 12, scrollX = TRUE))
	  })

	  output$event_aligned_correlation_table <- DT::renderDT({
	    dat <- event_aligned_result()$correlation %||% data.frame()
	    numeric_cols <- names(dat)[vapply(dat, is.numeric, logical(1))]
	    for (nm in numeric_cols) dat[[nm]] <- round(dat[[nm]], 6)
	    DT::datatable(dat, rownames = FALSE, filter = "top",
	                  options = list(pageLength = 12, scrollX = TRUE))
	  })

	  neural_manifold_behavior_data <- reactive({
	    file <- input$neural_manifold_behavior_file
	    if (is.null(file) || is.null(file$datapath) || !nzchar(file$datapath)) return(NULL)
	    tryCatch(
	      utils::read.csv(file$datapath, stringsAsFactors = FALSE, check.names = FALSE),
	      error = function(e) {
	        showNotification(paste0("Behavior CSV load failed: ", e$message), type = "error", duration = 8)
	        NULL
	      }
	    )
	  })

	  output$neural_manifold_behavior_columns <- renderUI({
	    dat <- neural_manifold_behavior_data()
	    if (is.null(dat) || !is.data.frame(dat) || ncol(dat) < 2L) {
	      return(tags$div(class = "small-note", "Optional: upload a behavior/movement CSV with a time column and one numeric/categorical behavior column."))
	    }
	    nms <- names(dat)
	    lower <- tolower(nms)
	    time_guess <- nms[which(lower %in% c("time", "timestamp", "timestamp_sec", "time_sec", "t"))[1]]
	    if (is.na(time_guess) || !nzchar(time_guess)) time_guess <- nms[1]
	    value_guess <- setdiff(nms, time_guess)[1] %||% nms[min(2L, length(nms))]
	    tagList(
	      selectInput("neural_manifold_behavior_time_col", "Behavior time column", choices = nms, selected = time_guess),
	      selectInput("neural_manifold_behavior_value_col", "Behavior / movement variable", choices = nms, selected = value_guess)
	    )
	  })

	  neural_manifold_trial_events_data <- reactive({
	    file <- input$neural_manifold_trial_file
	    if (is.null(file) || is.null(file$datapath) || !nzchar(file$datapath)) return(NULL)
	    tryCatch(
	      utils::read.csv(file$datapath, stringsAsFactors = FALSE, check.names = FALSE),
	      error = function(e) {
	        showNotification(paste0("Trial/event CSV load failed: ", e$message), type = "error", duration = 8)
	        NULL
	      }
	    )
	  })

	  output$neural_manifold_trial_columns <- renderUI({
	    dat <- neural_manifold_trial_events_data()
	    if (is.null(dat) || !is.data.frame(dat) || ncol(dat) < 1L) {
	      return(tags$div(class = "small-note", "Optional for sliceTCA: upload trial/event times, for example movement_onset_sec with trial_id and condition columns."))
	    }
	    nms <- names(dat)
	    lower <- tolower(nms)
	    time_guess <- nms[which(lower %in% c("event_time", "event_time_sec", "movement_onset", "movement_onset_sec", "onset", "onset_sec", "time", "timestamp_sec"))[1]]
	    if (is.na(time_guess) || !nzchar(time_guess)) time_guess <- nms[1]
	    trial_guess <- nms[which(lower %in% c("trial", "trial_id", "trialid", "trial_index"))[1]]
	    if (is.na(trial_guess) || !nzchar(trial_guess)) trial_guess <- ""
	    condition_guess <- nms[which(lower %in% c("condition", "movement", "movement_type", "direction", "side", "label", "phase"))[1]]
	    if (is.na(condition_guess) || !nzchar(condition_guess)) condition_guess <- ""
	    tagList(
	      selectInput("neural_manifold_trial_time_col", "sliceTCA event time column", choices = nms, selected = time_guess),
	      selectInput("neural_manifold_trial_id_col", "sliceTCA trial id column", choices = c("auto sequence" = "", nms), selected = trial_guess),
	      selectInput("neural_manifold_trial_condition_col", "sliceTCA condition / movement column", choices = c("none" = "", nms), selected = condition_guess)
	    )
	  })

	  output$neural_manifold_train_selector <- renderUI({
	    ds <- tryCatch(current_dataset(), error = function(e) NULL)
	    if (is.null(ds) || is.null(ds$trains) || length(ds$trains) == 0L) {
	      return(tags$div(class = "small-note", "Please load spike-train data first."))
	    }
	    ds <- normalize_dataset(ds)
	    trains <- names(ds$trains)
	    meta <- tryCatch(
	      parse_spike_train_column_metadata(trains, dataset_name = ds$meta$display_name %||% ""),
	      error = function(e) data.frame(train = trains, stringsAsFactors = FALSE)
	    )
	    labs <- state_trajectory_train_labels(meta, trains)
	    labs[is.na(labs) | !nzchar(labs)] <- trains[is.na(labs) | !nzchar(labs)]
	    selected <- intersect(as.character(isolate(input$neural_manifold_trains) %||% character(0)), trains)
	    if (length(selected) == 0L) selected <- trains[seq_len(min(12L, length(trains)))]
	    selectizeInput(
	      "neural_manifold_trains",
	      "Spike trains / neurons",
	      choices = stats::setNames(trains, labs),
	      selected = selected,
	      multiple = TRUE,
	      options = list(plugins = list("remove_button"), closeAfterSelect = TRUE,
	                     placeholder = "Select simultaneously recorded neurons")
	    )
	  })

	  neural_manifold_selected_trains <- shiny::debounce(reactive({
	    ds <- tryCatch(current_dataset(), error = function(e) NULL)
	    if (is.null(ds) || is.null(ds$trains)) return(character(0))
	    selected <- as.character(input$neural_manifold_trains %||% character(0))
	    intersect(selected, names(ds$trains))
	  }), millis = 300)

	  neural_manifold_slicetca_result <- reactive({
	    ds <- tryCatch(current_dataset(), error = function(e) NULL)
	    if (is.null(ds) || is.null(ds$trains)) return(stpd_slicetca_empty_result("Load spike-train data before building a sliceTCA tensor."))
	    ds <- normalize_dataset(ds)
	    selected <- neural_manifold_selected_trains()
	    dataset_events <- if (isTRUE(input$neural_manifold_use_dataset_events)) {
	      stpd_task_events_for_slicetca(task_events_filtered(use_neural_input = TRUE))
	    } else {
	      data.frame()
	    }
	    use_dataset_events <- isTRUE(input$neural_manifold_use_dataset_events) && is.data.frame(dataset_events) && nrow(dataset_events) > 0L
	    events <- if (use_dataset_events) dataset_events else neural_manifold_trial_events_data()
	    if (length(selected) < 2L) return(stpd_slicetca_empty_result("Select at least two spike trains / neurons for sliceTCA."))
	    if (is.null(events) || !is.data.frame(events) || nrow(events) == 0L) {
	      return(stpd_slicetca_empty_result("Upload a trial/event CSV or load a dataset containing Event / Event_* columns to build a trial x neuron x time tensor."))
	    }
	    bin_sec <- suppressWarnings(as.numeric(input$neural_manifold_bin_ms %||% 50))[1] / 1000
	    if (!is.finite(bin_sec) || bin_sec <= 0) bin_sec <- 0.05
	    tensor_res <- stpd_make_slicetca_trial_tensor(
	      ds$trains,
	      selected_trains = selected,
	      trial_events = events,
	      event_time_col = if (use_dataset_events) "event_time_sec" else input$neural_manifold_trial_time_col %||% names(events)[1],
	      trial_id_col = if (use_dataset_events) "trial_id" else input$neural_manifold_trial_id_col %||% NULL,
	      condition_col = if (use_dataset_events) "condition" else input$neural_manifold_trial_condition_col %||% NULL,
	      pre_sec = input$neural_manifold_slicetca_pre_sec %||% 0.5,
	      post_sec = input$neural_manifold_slicetca_post_sec %||% 1.0,
	      bin_sec = bin_sec,
	      time_origin = input$neural_manifold_time_origin %||% "raw",
	      transform = input$neural_manifold_transform %||% "sqrt_count",
	      scaling = input$neural_manifold_scaling %||% "zscore",
	      smoothing_sigma_bins = input$neural_manifold_smooth_bins %||% 0,
      label_source = input$neural_manifold_event_label_source %||% "audit_final",
	      min_isi_sec = min_valid_isi_sec(),
	      auto_others = FALSE
	    )
	    if (!identical(tensor_res$status, "ready")) return(tensor_res)
	    stpd_run_slicetca_backend(
	      tensor_res,
	      ranks = stpd_slicetca_rank_parse(input$neural_manifold_slicetca_ranks %||% "2,0,2"),
	      run_python = isTRUE(input$neural_manifold_slicetca_run),
	      seed = input$neural_manifold_seed %||% 1,
	      max_iter = input$neural_manifold_slicetca_max_iter %||% 1000,
	      learning_rate = input$neural_manifold_slicetca_lr %||% 0.005,
	      positive = FALSE,
	      apply_invariance = TRUE
	    )
	  })

	  neural_manifold_result <- reactive({
	    ds <- current_dataset()
	    ds <- normalize_dataset(ds)
	    selected <- neural_manifold_selected_trains()
	    validate(need(length(selected) >= 2L, "Select at least two spike trains / neurons for neural manifold analysis."))
	    bin_sec <- suppressWarnings(as.numeric(input$neural_manifold_bin_ms %||% 50))[1] / 1000
	    if (!is.finite(bin_sec) || bin_sec <= 0) bin_sec <- 0.05
	    time_origin <- input$neural_manifold_time_origin %||% "raw"
	    start_sec <- suppressWarnings(as.numeric(input$neural_manifold_start_sec %||% 0))[1]
	    if (!is.finite(start_sec) || start_sec < 0 || isTRUE(all.equal(start_sec, 0))) start_sec <- NULL
	    end_sec <- suppressWarnings(as.numeric(input$neural_manifold_end_sec %||% 0))[1]
	    if (!is.finite(end_sec) || end_sec <= 0 || (!is.null(start_sec) && end_sec <= start_sec)) end_sec <- NULL
	    behavior <- neural_manifold_behavior_data()
	    task_events_for_pop <- if (isTRUE(input$neural_manifold_use_dataset_events)) {
	      task_events_filtered(use_neural_input = TRUE)
	    } else {
	      stpd_empty_task_events()
	    }
	    pop <- stpd_make_neural_population_matrix(
	      ds$trains,
	      selected_trains = selected,
	      bin_sec = bin_sec,
	      start_sec = start_sec,
	      end_sec = end_sec,
	      time_origin = time_origin,
	      transform = input$neural_manifold_transform %||% "sqrt_count",
	      smoothing_sigma_bins = input$neural_manifold_smooth_bins %||% 1,
	      scaling = input$neural_manifold_scaling %||% "zscore",
	      behavior = behavior,
	      behavior_time_col = input$neural_manifold_behavior_time_col %||% NULL,
	      behavior_value_col = input$neural_manifold_behavior_value_col %||% NULL,
	      task_events = task_events_for_pop,
	      task_event_names = NULL,
	      task_event_pre_sec = input$neural_manifold_task_pre_sec %||% 1,
	      task_event_post_sec = input$neural_manifold_task_post_sec %||% 2
	    )
	    validate(need(nrow(pop$features %||% data.frame()) >= 3L, "Not enough valid time bins for a neural manifold."))
	    method <- input$neural_manifold_method %||% "pca"
	    out <- tryCatch(
	      stpd_run_neural_manifold_embedding(
	        pop,
	        method = method,
	        n_neighbors = input$neural_manifold_n_neighbors %||% 15,
	        tsne_perplexity = input$neural_manifold_tsne_perplexity %||% 30,
	        umap_min_dist = input$neural_manifold_umap_min_dist %||% 0.1,
	        diffusion_time = input$neural_manifold_diffusion_time %||% 3,
	        seed = input$neural_manifold_seed %||% 1,
	        max_points = input$neural_manifold_max_points %||% 1200
	      ),
	      error = function(e) e
	    )
	    validate(need(!inherits(out, "error"), paste0("Neural manifold failed: ", out$message)))
	    event_out <- tryCatch(
	      stpd_neural_add_event_state_layer(
	        out,
	        ds$trains,
	        selected_trains = selected,
	        label_source = input$neural_manifold_event_label_source %||% "audit_final",
	        min_isi_sec = min_valid_isi_sec(),
	        auto_others = FALSE
	      ),
	      error = function(e) e
	    )
	    if (inherits(event_out, "error")) {
	      out$diagnostics <- rbind(
	        out$diagnostics %||% data.frame(),
	        data.frame(
	          method = out$method_label %||% out$method %||% "Neural manifold",
	          metric = "event_state_layer",
	          value = "failed",
	          note = paste0("Event-state annotation failed: ", event_out$message),
	          stringsAsFactors = FALSE
	        )
	      )
	    } else {
	      out <- event_out
	    }
	    event_permutations <- suppressWarnings(as.integer(round(as.numeric(input$neural_manifold_event_permutations %||% 199)[1])))
	    if (!is.finite(event_permutations) || event_permutations < 0L) event_permutations <- 199L
	    event_window_bins <- suppressWarnings(as.integer(round(as.numeric(input$neural_manifold_event_window_bins %||% 5)[1])))
	    if (!is.finite(event_window_bins) || event_window_bins < 1L) event_window_bins <- 5L
	    out$event_geometry <- stpd_neural_event_geometry(out)
	    out$event_distances <- stpd_neural_event_distance_tests(
	      out,
	      states = c("burst", "pause", "tonic", "hf_spiking"),
	      n_perm = event_permutations,
	      seed = input$neural_manifold_seed %||% 1
	    )
	    out$event_triggered <- stpd_neural_event_triggered_trajectory(
	      out,
	      states = c("burst", "pause"),
	      window_bins = event_window_bins
	    )
	    out$task_event_triggered <- stpd_neural_task_event_triggered_trajectory(out)
	    out$event_dynamics <- stpd_neural_event_dynamics_summary(
	      out,
	      states = c("burst", "pause"),
	      window_bins = event_window_bins
	    )
	    out$validation <- stpd_neural_manifold_validation(
	      out,
	      seed = input$neural_manifold_seed %||% 1,
	      n_neighbors = input$neural_manifold_n_neighbors %||% 10,
	      event_permutations = event_permutations
	    )
	    out$selected_trains <- selected
	    out
	  })

	  output$neural_manifold_plot <- renderPlotly({
	    stpd_neural_manifold_plot(neural_manifold_result())
	  })

	  output$neural_manifold_slicetca_plot <- renderPlotly({
	    stpd_slicetca_plot(neural_manifold_slicetca_result(), use_reconstruction = isTRUE(input$neural_manifold_slicetca_recon_plot))
	  })

	  output$neural_manifold_slicetca_summary_table <- DT::renderDT({
	    dat <- neural_manifold_slicetca_result()$tensor_summary %||% data.frame()
	    if (is.null(dat) || nrow(dat) == 0L) dat <- data.frame(message = "No sliceTCA tensor summary available.", stringsAsFactors = FALSE)
	    DT::datatable(dat, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
	  })

	  output$neural_manifold_slicetca_diagnostics_table <- DT::renderDT({
	    dat <- neural_manifold_slicetca_result()$diagnostics %||% data.frame()
	    if (is.null(dat) || nrow(dat) == 0L) dat <- stpd_slicetca_backend_status()
	    DT::datatable(dat, rownames = FALSE, filter = "top",
	                  options = list(pageLength = 10, scrollX = TRUE))
	  })

	  output$neural_manifold_slicetca_reconstruction_table <- DT::renderDT({
	    dat <- neural_manifold_slicetca_result()$reconstruction_metrics %||% data.frame()
	    if (is.null(dat) || nrow(dat) == 0L) dat <- data.frame(message = "No sliceTCA reconstruction metrics available.", stringsAsFactors = FALSE)
	    numeric_cols <- names(dat)[vapply(dat, is.numeric, logical(1))]
	    for (nm in numeric_cols) dat[[nm]] <- round(dat[[nm]], 6)
	    DT::datatable(dat, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
	  })

	  output$neural_manifold_slicetca_embedding_table <- DT::renderDT({
	    res <- neural_manifold_slicetca_result()
	    dat <- if (isTRUE(input$neural_manifold_slicetca_recon_plot) && is.data.frame(res$reconstructed_embedding) && nrow(res$reconstructed_embedding) > 0L) {
	      res$reconstructed_embedding
	    } else {
	      res$trial_embedding %||% data.frame()
	    }
	    if (is.null(dat) || nrow(dat) == 0L) dat <- data.frame(message = "No trial-time tensor coordinates available.", stringsAsFactors = FALSE)
	    show_cols <- intersect(c("trial_index", "trial_id", "condition", "rel_bin", "rel_time_sec",
	                             "event_state", "burst_fraction", "pause_fraction", "tonic_fraction",
	                             "TC1", "TC2", "TC3"), names(dat))
	    if (length(show_cols) > 0L) dat <- dat[, show_cols, drop = FALSE]
	    numeric_cols <- names(dat)[vapply(dat, is.numeric, logical(1))]
	    for (nm in numeric_cols) dat[[nm]] <- round(dat[[nm]], 6)
	    DT::datatable(dat, rownames = FALSE, filter = "top",
	                  options = list(pageLength = 15, scrollX = TRUE))
	  })

	  output$neural_manifold_validation_table <- DT::renderDT({
	    dat <- neural_manifold_result()$validation %||% data.frame()
	    if (is.null(dat) || nrow(dat) == 0L) dat <- data.frame(message = "No validation metrics available.", stringsAsFactors = FALSE)
	    numeric_cols <- names(dat)[vapply(dat, is.numeric, logical(1))]
	    for (nm in numeric_cols) dat[[nm]] <- round(dat[[nm]], 6)
	    DT::datatable(dat, rownames = FALSE, filter = "top",
	                  options = list(pageLength = 14, scrollX = TRUE))
	  })

	  output$neural_manifold_diagnostics_table <- DT::renderDT({
	    dat <- neural_manifold_result()$diagnostics %||% data.frame()
	    if (is.null(dat) || nrow(dat) == 0L) dat <- data.frame(message = "No embedding diagnostics available.", stringsAsFactors = FALSE)
	    DT::datatable(dat, rownames = FALSE, options = list(pageLength = 12, scrollX = TRUE))
	  })

	  output$neural_manifold_method_notes_table <- DT::renderDT({
	    DT::datatable(stpd_neural_manifold_method_notes(), rownames = FALSE,
	                  options = list(pageLength = 9, scrollX = TRUE))
	  })

	  output$neural_manifold_event_geometry_table <- DT::renderDT({
	    dat <- neural_manifold_result()$event_geometry %||% data.frame()
	    if (is.null(dat) || nrow(dat) == 0L) dat <- data.frame(message = "No event geometry available.", stringsAsFactors = FALSE)
	    numeric_cols <- names(dat)[vapply(dat, is.numeric, logical(1))]
	    for (nm in numeric_cols) dat[[nm]] <- round(dat[[nm]], 6)
	    DT::datatable(dat, rownames = FALSE, filter = "top",
	                  options = list(pageLength = 10, scrollX = TRUE))
	  })

	  output$neural_manifold_event_distance_table <- DT::renderDT({
	    dat <- neural_manifold_result()$event_distances %||% data.frame()
	    if (is.null(dat) || nrow(dat) == 0L) dat <- data.frame(message = "No event distance tests available.", stringsAsFactors = FALSE)
	    numeric_cols <- names(dat)[vapply(dat, is.numeric, logical(1))]
	    for (nm in numeric_cols) dat[[nm]] <- round(dat[[nm]], 6)
	    DT::datatable(dat, rownames = FALSE, filter = "top",
	                  options = list(pageLength = 10, scrollX = TRUE))
	  })

	  output$neural_manifold_event_dynamics_table <- DT::renderDT({
	    dyn <- neural_manifold_result()$event_dynamics %||% data.frame()
	    dec <- tryCatch(stpd_neural_event_label_decoding(neural_manifold_result()$features, seed = input$neural_manifold_seed %||% 1,
	                                                     n_perm = input$neural_manifold_event_permutations %||% 199),
	                    error = function(e) data.frame(metric = "event_label_decoding", value = NA_real_, status = "failed", note = e$message, stringsAsFactors = FALSE))
	    if (is.data.frame(dyn) && nrow(dyn) > 0L && !("message" %in% names(dyn))) {
	      dyn_long <- data.frame(
	        metric = paste0("event_dynamics_", dyn$event_state),
	        value = dyn$delta_speed_post_minus_pre,
	        status = "ok",
	        note = paste0("delta speed post-pre=", signif(dyn$delta_speed_post_minus_pre, 5),
	                      "; delta curvature post-pre=", signif(dyn$delta_curvature_post_minus_pre, 5),
	                      "; n_onsets=", dyn$n_onsets),
	        stringsAsFactors = FALSE
	      )
	      dat <- rbind(dec, dyn_long)
	    } else {
	      dat <- dec
	    }
	    numeric_cols <- names(dat)[vapply(dat, is.numeric, logical(1))]
	    for (nm in numeric_cols) dat[[nm]] <- round(dat[[nm]], 6)
	    DT::datatable(dat, rownames = FALSE, filter = "top",
	                  options = list(pageLength = 10, scrollX = TRUE))
	  })

	  output$neural_manifold_event_triggered_table <- DT::renderDT({
	    res <- neural_manifold_result()
	    state_dat <- res$event_triggered %||% data.frame()
	    task_dat <- res$task_event_triggered %||% data.frame()
	    rows <- list()
	    if (is.data.frame(state_dat) && nrow(state_dat) > 0L && !("message" %in% names(state_dat))) {
	      state_dat$trigger_type <- "detected_state"
	      state_dat$trigger_name <- as.character(state_dat$event_state %||% "")
	      rows[[length(rows) + 1L]] <- state_dat
	    }
	    if (is.data.frame(task_dat) && nrow(task_dat) > 0L && !("message" %in% names(task_dat))) {
	      task_dat$trigger_type <- "task_event"
	      task_dat$trigger_name <- as.character(task_dat$task_event_name %||% "")
	      rows[[length(rows) + 1L]] <- task_dat
	    }
	    dat <- if (length(rows) > 0L) dplyr::bind_rows(rows) else data.frame(message = "No event-triggered trajectory available.", stringsAsFactors = FALSE)
	    numeric_cols <- names(dat)[vapply(dat, is.numeric, logical(1))]
	    for (nm in numeric_cols) dat[[nm]] <- round(dat[[nm]], 6)
	    DT::datatable(dat, rownames = FALSE, filter = "top",
	                  options = list(pageLength = 15, scrollX = TRUE))
	  })

	  output$neural_manifold_feature_table <- DT::renderDT({
	    dat <- neural_manifold_result()$features
	    show_cols <- unique(c("bin_id", "bin_start_sec", "bin_end_sec", "time_mid_sec",
	                          "total_spike_count", "population_rate_hz", "behavior_value",
	                          "behavior_numeric", "event_state", "event_burst_fraction",
	                          "event_pause_fraction", "event_tonic_fraction", "latent_speed",
	                          "latent_curvature", "task_event_name", "task_event_rel_time_sec",
	                          "task_event_epoch", "task_event_in_window", "NM1", "NM2", "NM3",
	                          head(names(dat)[grepl("^rate_hz__", names(dat))], 20)))
	    out <- dat[, intersect(show_cols, names(dat)), drop = FALSE]
	    numeric_cols <- names(out)[vapply(out, is.numeric, logical(1))]
	    for (nm in numeric_cols) out[[nm]] <- round(out[[nm]], 6)
	    DT::datatable(out, rownames = FALSE, filter = "top",
	                  options = list(pageLength = 15, scrollX = TRUE))
	  })

	  output$neural_manifold_loading_table <- DT::renderDT({
	    dat <- neural_manifold_result()$loadings %||% data.frame()
	    if (is.null(dat) || nrow(dat) == 0L) {
	      dat <- data.frame(message = "No linear loadings/private-variance table for this method.", stringsAsFactors = FALSE)
	    } else {
	      numeric_cols <- names(dat)[vapply(dat, is.numeric, logical(1))]
	      for (nm in numeric_cols) dat[[nm]] <- round(dat[[nm]], 6)
	    }
	    DT::datatable(dat, rownames = FALSE, filter = "top",
	                  options = list(pageLength = 15, scrollX = TRUE))
	  })

	  output$neural_manifold_summary_table <- DT::renderDT({
	    res <- neural_manifold_result()
	    dat <- res$window_summary %||% data.frame()
	    if (is.null(dat) || nrow(dat) == 0L) dat <- data.frame(message = "No window summary available.", stringsAsFactors = FALSE)
	    DT::datatable(dat, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
	  })

	  # ----------------------------------------------------------
	  # Manual labeling
  # ----------------------------------------------------------
  selected_points <- reactive({
    if (length(rv$datasets) == 0L) return(NULL)
    if (!identical(input$main_tabs %||% "", "\u5BF9\u9F50\u65F6\u95F4\u6233\u56FE")) return(NULL)
    stpd_safe_plotly_event_data("plotly_selected", source = "raster")
  })

  selection_has_points <- function(sel) {
    stpd_selection_has_points(sel)
  }

  update_current_dataset_trains <- function(td) {
    ds <- current_dataset()
    id <- rv$current_id
    ds$trains <- td
    audit_policy <- ds$results$final_audit_policy %||% NULL
    if (!is.null(audit_policy)) {
      ds <- stpd_apply_final_audit(
        ds,
        selected_trains = names(td),
        promote_possible = isTRUE(audit_policy$promote_possible %||% FALSE),
        min_isi_sec = min_valid_isi_sec(),
        reason = "manual_edit_sync_final_audit",
        user = Sys.info()[["user"]] %||% NA_character_
      )$dataset
    }
    set_dataset(id, ds)
  }

  selection_from_cache <- function() {
    sel <- selected_points()
    if (!selection_has_points(sel)) sel <- rv$last_plotly_selection
    if (!selection_has_points(sel)) return(NULL)
    sel
  }

  selection_xy_range <- function(sel) {
    validate(need(selection_has_points(sel), "No finite points were captured by the current box selection."))
    f <- unit_factor()
    x <- suppressWarnings(as.numeric(sel$x))
    y <- suppressWarnings(as.numeric(sel$y))
    keep <- is.finite(x) & is.finite(y)
    x <- x[keep]
    y <- y[keep]
    list(
      x_min_sec = min(x, na.rm = TRUE) / f,
      x_max_sec = max(x, na.rm = TRUE) / f,
      y_center = (min(y, na.rm = TRUE) + max(y, na.rm = TRUE)) / 2
    )
  }

  selection_time_isi_indices <- function(sel) {
    validate(need(selection_has_points(sel), "Please Box Select on aligned plot first."))
    td <- current_trains()
    axis_tbl <- selected_axis_table()
    rng <- selection_xy_range(sel)
    x_min_sec <- rng$x_min_sec
    x_max_sec <- rng$x_max_sec
    y_center <- rng$y_center
    all_y_vals <- sort(unique(axis_tbl$y))
    validate(need(length(all_y_vals) > 0, "No train rows are available in the current plot."))
    y_val <- all_y_vals[which.min(abs(all_y_vals - y_center))]
    train_here <- unique(axis_tbl$train[axis_tbl$y == y_val])
    validate(need(length(train_here) == 1, "Selection must stay within ONE train row."))
    tr <- train_here[1]
    dat_tr <- td[[tr]]
    n <- nrow(dat_tr)
    validate(need(n > 1, "This train has <=1 spike."))
    t_align <- dat_tr$timestamp_sec - dat_tr$timestamp_sec[1]
    t_start <- t_align[-length(t_align)]
    t_end <- t_align[-1]
    idx_ISI <- 2:n
    covered <- which(t_start < x_max_sec & t_end > x_min_sec)
    validate(need(length(covered) > 0, "No ISI covered by selection."))
    list(train = tr, idx = idx_ISI[covered])
  }

  apply_manual_selection <- function(sel, pat, notify = TRUE) {
    validate(need(selection_has_points(sel), "Please Box Select on aligned plot first."))
    td <- current_trains()
    pat_raw <- tolower(trimws(as.character(pat)))
    neg_label <- pat_raw %in% c("not_burst", "hard_negative_burst", "not burst", "not-burst")
    pat <- if (neg_label) "not_burst" else normalize_pattern_label(pat, fill_blank_others = FALSE)[1]
    validate(need(pat %in% c("burst", "long_burst", "pause", "tonic", "high_frequency_tonic", "high_frequency_spiking", "others", "not_burst"), "Select a valid pattern."))

    if (pat %in% c("burst", "long_burst", "tonic", "high_frequency_tonic", "high_frequency_spiking") && "customdata" %in% names(sel)) {
      cd <- sel$customdata
      cd <- cd[!is.na(cd)]
      if (length(cd) >= 2) {
        parts <- strsplit(as.character(cd), "__", fixed = TRUE)
        tr_sel <- vapply(parts, function(z) if (length(z) >= 1) z[[1]] else "", character(1))
        idx_sel <- suppressWarnings(as.integer(vapply(parts, function(z) if (length(z) >= 2) z[[2]] else NA_character_, character(1))))
        ok <- !is.na(idx_sel) & tr_sel != ""
        tr_sel <- tr_sel[ok]
        idx_sel <- idx_sel[ok]
        train_here <- unique(tr_sel)
        validate(need(length(train_here) == 1, "Selection must stay within ONE train."))
        tr <- train_here[1]
        dat_tr <- td[[tr]]
        n <- nrow(dat_tr)
        validate(need(n > 1, "This train has <=1 spike."))
        idx_sel <- sort(unique(idx_sel))
        validate(need(length(idx_sel) >= 2, "Need at least 2 different spikes."))
        cuts <- c(1, which(diff(idx_sel) != 1) + 1)
        grp_starts <- idx_sel[cuts]
        grp_ends <- idx_sel[c(cuts[-1] - 1, length(idx_sel))]
        all_isi_idx <- integer(0)
        for (g in seq_along(grp_starts)) {
          L <- grp_starts[g]
          R <- grp_ends[g]
          if ((R - L) < 1) next
          tmp_idx <- (L + 1):R
          tmp_idx <- tmp_idx[tmp_idx >= 2 & tmp_idx <= n]
          all_isi_idx <- c(all_isi_idx, tmp_idx)
        }
        all_isi_idx <- sort(unique(all_isi_idx))
        validate(need(length(all_isi_idx) > 0, "No ISI generated from current spike selection."))
        stpd_push_manual_undo(rv, rv$current_id, td, paste0("\u6807\u8BB0 MANUAL ", pat))
        if (!("pattern_manual_negative" %in% names(dat_tr))) dat_tr$pattern_manual_negative <- rep("", nrow(dat_tr))
        if (identical(pat, "not_burst")) {
          dat_tr$pattern_manual_negative[all_isi_idx] <- "not_burst"
          dat_tr$pattern_manual[all_isi_idx] <- ""
        } else {
          dat_tr$pattern_manual[all_isi_idx] <- pat
          dat_tr$pattern_manual_negative[all_isi_idx] <- ""
        }
        td[[tr]] <- dat_tr
        update_current_dataset_trains(td)
        if (isTRUE(notify)) showNotification(paste0("\u5DF2\u6807\u8BB0 ", length(all_isi_idx), " \u4E2A ISI \u4E3A MANUAL ", pat, "."), type = "message", duration = 2)
        return(invisible(TRUE))
      }
    }

    # pause/others and fallback for burst/tonic: use the selected time range on
    # the nearest train row. This is also more robust in reduced LOD mode.
    loc <- selection_time_isi_indices(sel)
    dat_tr <- td[[loc$train]]
    stpd_push_manual_undo(rv, rv$current_id, td, paste0("\u6807\u8BB0 MANUAL ", pat))
    if (!("pattern_manual_negative" %in% names(dat_tr))) dat_tr$pattern_manual_negative <- rep("", nrow(dat_tr))
    if (identical(pat, "not_burst")) {
      dat_tr$pattern_manual_negative[loc$idx] <- "not_burst"
      dat_tr$pattern_manual[loc$idx] <- ""
    } else {
      dat_tr$pattern_manual[loc$idx] <- pat
      dat_tr$pattern_manual_negative[loc$idx] <- ""
    }
    td[[loc$train]] <- dat_tr
    update_current_dataset_trains(td)
    if (isTRUE(notify)) showNotification(paste0("\u5DF2\u6807\u8BB0 ", length(loc$idx), " \u4E2A ISI \u4E3A MANUAL ", pat, "."), type = "message", duration = 2)
    invisible(TRUE)
  }


  }, envir = server_env)
}
