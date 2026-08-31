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
  required <- c("idx", "timestamp_sec", "ISI_sec")
  if (!all(required %in% names(dat))) return(FALSE)
  if (!is.numeric(dat$idx) || !is.numeric(dat$timestamp_sec) ||
      !is.numeric(dat$ISI_sec)) return(FALSE)
  n <- nrow(dat)
  if (length(dat$idx) != n || length(dat$timestamp_sec) != n ||
      length(dat$ISI_sec) != n) return(FALSE)
  idx <- suppressWarnings(as.numeric(dat$idx))
  timestamp <- suppressWarnings(as.numeric(dat$timestamp_sec))
  isi <- suppressWarnings(as.numeric(dat$ISI_sec))
  all(is.finite(idx)) && all(is.finite(timestamp)) &&
    all(is.na(isi) | is.finite(isi))
}

stpd_workspace_rds_is_valid <- function(obj, max_datasets = 500L, max_trains = 5000L) {
  if (!is.list(obj) || !is.list(obj$datasets)) return(FALSE)
  if (length(obj$datasets) > max_datasets) return(FALSE)
  dataset_ids <- names(obj$datasets)
  if (length(obj$datasets) > 0L &&
      (is.null(dataset_ids) || length(dataset_ids) != length(obj$datasets) ||
       anyNA(dataset_ids) || any(!nzchar(dataset_ids)) || anyDuplicated(dataset_ids))) {
    return(FALSE)
  }
  for (ds in obj$datasets) {
    if (!is.list(ds) || !is.list(ds$trains)) return(FALSE)
    if (length(ds$trains) > max_trains) return(FALSE)
    train_ids <- names(ds$trains)
    if (length(ds$trains) > 0L &&
        (is.null(train_ids) || length(train_ids) != length(ds$trains) ||
         anyNA(train_ids) || any(!nzchar(train_ids)) || anyDuplicated(train_ids))) {
      return(FALSE)
    }
    ok <- vapply(ds$trains, stpd_shiny_train_frame_valid, logical(1))
    if (!all(ok)) return(FALSE)
  }
  TRUE
}

stpd_default_timestamp_tab_for_dataset <- function(ds) {
  events <- tryCatch(
    stpd_normalize_task_events(
      (ds %||% list())$task_events %||% data.frame(),
      source = ((ds %||% list())$meta %||% list())$display_name %||% ""
    ),
    error = function(e) stpd_empty_task_events()
  )
  if (is.data.frame(events) && nrow(events) > 0L) {
    return("\u539F\u59CB\u65F6\u95F4\u6233\u56FE")
  }
  "\u5BF9\u9F50\u65F6\u95F4\u6233\u56FE"
}

stpd_server_install_parameters_module <- function(server_env) {
  evalq({
  ui_inline_text <- function(zh, en) {
    if (identical(ui_language(), "en")) en else zh
  }

  output$core_detector_params_panel <- renderUI({
    schema_ui_controls(
      prefix = "schema_param_",
      group_by = TRUE,
      group_field = "group",
      open_groups = TRUE,
      show_notes = TRUE,
      lang = ui_language(),
      current_values = isolate(shiny::reactiveValuesToList(input))
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
    updateNumericInput(
      session, "artifact_isi_ms",
      label = ui_inline_text(
        paste0("\u4F2A\u8FF9 / \u6700\u5C0F\u6709\u6548 ISI \u9608\u503C\uFF08", new_u, "\uFF09"),
        paste0("Artifact / minimum valid ISI threshold (", new_u, ")")
      ),
      value = art_v, step = if (identical(new_u, "s")) 0.0001 else 0.1
    )
    updateNumericInput(
      session, "min_valid_isi_ms",
      label = ui_inline_text(
        paste0("\u6700\u5C0F\u6709\u6548 ISI\uFF08\u540C\u4F2A\u8FF9\u9608\u503C\uFF0C", new_u, "\uFF09"),
        paste0("Minimum valid ISI (same as artifact threshold, ", new_u, ")")
      ),
      value = min_v, step = if (identical(new_u, "s")) 0.0001 else 0.1
    )
    updateNumericInput(
      session, "refractory_suspect_ms",
      label = ui_inline_text(
        paste0("\u7591\u4F3C\u4E0D\u5E94\u671F ISI \u9608\u503C\uFF08", new_u, "\uFF09"),
        paste0("Suspected refractory-period ISI threshold (", new_u, ")")
      ),
      value = ref_v, step = if (identical(new_u, "s")) 0.0001 else 0.1
    )
    updateNumericInput(
      session, "refractory_suspect_ms_param",
      label = ui_inline_text(
        paste0("\u7591\u4F3C\u4E0D\u5E94\u671F\u9608\u503C\uFF08", new_u, "\uFF09"),
        paste0("Suspected refractory-period threshold (", new_u, ")")
      ),
      value = ref_param_v, step = if (identical(new_u, "s")) 0.0001 else 0.1
    )
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
  ui_inline_text <- function(zh, en) {
    if (identical(ui_language(), "en")) en else zh
  }

  select_default_timestamp_tab <- function(ds = NULL, defer = FALSE) {
    if (is.null(ds) && !is.null(rv$current_id) && rv$current_id %in% names(rv$datasets)) {
      ds <- rv$datasets[[rv$current_id]]
    }
    tab <- stpd_default_timestamp_tab_for_dataset(ds)
    select_tab <- function() updateTabsetPanel(session, "main_tabs", selected = tab)
    if (isTRUE(defer)) {
      session$onFlushed(select_tab, once = TRUE)
    } else {
      select_tab()
    }
    invisible(tab)
  }

  data_load_progress <- function(value, detail = "", message = NULL, type = "active") {
    call_env <- parent.frame()
    detail_expr <- substitute(detail)
    message_expr <- substitute(message)
    evaluate_copy <- function(expr, lang, default_key = NULL) {
      copy_env <- new.env(parent = call_env)
      copy_env$ui_text <- function(key, ...) {
        stpd_ui_copy(key, lang = lang, ...)
      }
      copy_env$ui_inline_text <- function(zh, en) {
        if (identical(lang, "en")) en else zh
      }
      copy_env$ui_condition_detail <- function(e) {
        stpd_ui_condition_detail(e, lang = lang)
      }
      out <- tryCatch(eval(expr, envir = copy_env), error = function(e) NULL)
      if (is.null(out) && !is.null(default_key)) {
        out <- stpd_ui_copy(default_key, lang = lang)
      }
      as.character(out %||% "")[1]
    }
    detail_i18n <- c(
      zh = evaluate_copy(detail_expr, "zh"),
      en = evaluate_copy(detail_expr, "en")
    )
    message_i18n <- c(
      zh = evaluate_copy(message_expr, "zh", default_key = "data_loading_default"),
      en = evaluate_copy(message_expr, "en", default_key = "data_loading_default")
    )
    active_lang <- if (identical(ui_language(), "en")) "en" else "zh"
    value <- suppressWarnings(as.numeric(value)[1])
    if (!is.finite(value)) value <- 0
    value <- max(0, min(1, value))
    detail <- unname(detail_i18n[[active_lang]])
    message <- unname(message_i18n[[active_lang]])
    type <- as.character(type %||% "active")[1]
    rv$data_load_active <- !type %in% c("success", "error", "idle", "hide")
    rv$data_load_progress_value <- value
    rv$data_load_progress_message <- message
    rv$data_load_progress_detail <- detail
    rv$data_load_progress_message_i18n <- message_i18n
    rv$data_load_progress_detail_i18n <- detail_i18n
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
    plot_message <- if (identical(type, "error")) {
      ui_text("data_loading_failed")
    } else if (identical(type, "success")) {
      ui_text("data_loaded_plot_pending")
    } else {
      ui_text("data_loading_plot_pending")
    }
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

  observeEvent(ui_language(), {
    message_i18n <- isolate(rv$data_load_progress_message_i18n)
    detail_i18n <- isolate(rv$data_load_progress_detail_i18n)
    if (is.null(message_i18n) && is.null(detail_i18n)) return()
    lang <- if (identical(ui_language(), "en")) "en" else "zh"
    pick_copy <- function(x, fallback = "") {
      if (is.null(x) || is.null(names(x)) || !(lang %in% names(x))) return(fallback)
      as.character(x[[lang]] %||% fallback)[1]
    }
    message <- pick_copy(message_i18n, ui_text("data_loading_default"))
    detail <- pick_copy(detail_i18n, "")
    type <- as.character(isolate(rv$data_load_progress_type %||% "idle"))[1]
    value <- suppressWarnings(as.numeric(isolate(rv$data_load_progress_value %||% 0))[1])
    if (!is.finite(value)) value <- 0
    value <- max(0, min(1, value))
    rv$data_load_progress_message <- as.character(message %||% "")[1]
    rv$data_load_progress_detail <- as.character(detail %||% "")[1]
    session$sendCustomMessage(
      "stpdDataLoadProgress",
      list(type = type, value = value, message = message, detail = detail)
    )
    plot_type <- if (identical(type, "error")) "error" else if (identical(type, "success")) "success" else "active"
    plot_message <- if (identical(type, "error")) {
      ui_text("data_loading_failed")
    } else if (identical(type, "success")) {
      ui_text("data_loaded_plot_pending")
    } else {
      ui_text("data_loading_plot_pending")
    }
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
  }, ignoreInit = TRUE)

  dataset_selector_labels <- function(ds) {
    vapply(
      ds,
      function(x) paste0(
        "[", x$meta$source, "] ", x$meta$display_name, " (", length(x$trains),
        ui_inline_text(" \u6761 train\uFF09", " trains)")
      ),
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
    withProgress(message = ui_text("data_loading_raw"), value = 0, {
      loaded_count <- 0L
      failed_count <- 0L
      data_load_progress(
        0.01,
        ui_text("preparing_raw_csv", n = nrow(files)),
        ui_text("data_loading_raw")
      )
      for (i in seq_len(nrow(files))) {
        path <- files$datapath[i]
        nm <- files$name[i]
        file_start <- (i - 1) / n_files
        file_span <- 1 / n_files
        data_load_progress(
          file_start + 0.05 * file_span,
          ui_text("reading_raw_csv", index = i, total = nrow(files), name = nm),
          ui_text("data_loading_raw")
        )
        id <- paste0("raw_", digest(paste0(nm, "_", file.info(path)$size), algo = "xxhash64"))
        if (id %in% names(ds)) {
          data_load_progress(
            file_start + file_span,
            ui_text("skipped_duplicate_dataset", name = nm),
            ui_text("data_loading_raw")
          )
          next
        }
        trains <- tryCatch(
          build_trains_from_raw(path, header = isTRUE(input$header_raw), unit_in = input$unit_in_raw, duplicate_policy = input$duplicate_timestamp_policy %||% "error_keep"),
          error = function(e) {
            failed_count <<- failed_count + 1L
            failure_title <- ui_text("raw_file_loading_failed")
            failure_detail <- ui_condition_detail(e)
            data_load_progress(
              file_start + 0.95 * file_span,
              paste0(nm, " | ", ui_condition_detail(e)),
              ui_text("raw_file_loading_failed"),
              "error"
            )
            showNotification(
              paste0(failure_title, if (identical(ui_language(), "en")) ": " else "\uFF1A", nm, " | ", failure_detail),
              type = "error", duration = 8
            )
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
            ui_text("parsed_trains_building_dataset", n = length(trains)),
            ui_text("data_loading_raw")
          )
          ds[[id]] <- make_dataset(name = nm, source = "raw", trains = trains, unit_in = input$unit_in_raw, task_events = task_events)
          ds[[id]] <- stpd_attach_input_provenance(
            ds[[id]], path, input_file_name = nm, parser_mode = "raw",
            unit_in = input$unit_in_raw, header = isTRUE(input$header_raw),
            duplicate_policy = input$duplicate_timestamp_policy %||% "error_keep"
          )
          data_load_progress(
            file_start + 0.78 * file_span,
            ui_text("running_import_qc"),
            ui_text("data_loading_raw")
          )
          ds[[id]]$quality <- validate_dataset_quality_impl(trains, min_isi_sec = min_valid_isi_sec(), unit_hint = input$unit_in_raw, refractory_suspect_sec = refractory_suspect_sec(), display_unit = qc_isi_unit())
          # Cache the QC detail tables at import time as well.  The QC tab then
          # only formats already computed data instead of launching three full
          # train-wide scans when it becomes visible.
          ds[[id]]$quality_artifact_details <- tryCatch(
            artifact_isi_details(trains, min_isi_sec = min_valid_isi_sec(), display_unit = qc_isi_unit()),
            error = function(e) data.frame()
          )
          ds[[id]]$quality_duplicate_details <- tryCatch(
            duplicate_timestamp_details(trains, display_unit = qc_isi_unit()),
            error = function(e) data.frame()
          )
          rv$current_id <- id
          loaded_count <- loaded_count + 1L
          data_load_progress(
            file_start + 0.94 * file_span,
            ui_text("refreshing_dataset_and_plot"),
            ui_text("data_loading_raw")
          )
          task_note <- if (nrow(task_events) > 0L) paste0("\n", ui_text("task_events_recognized", n = nrow(task_events))) else ""
          showNotification(paste0(quality_notification_text(ds[[id]]$quality, lang = ui_language()), task_note), type = ifelse(any(ds[[id]]$quality$warning_level == "error"), "error", ifelse(any(ds[[id]]$quality$warning_level == "warning"), "warning", "message")), duration = 7)
        }
      }
      rv$datasets <- ds
      sync_dataset_selector(rv$current_id)
      if (!is.null(rv$current_id) && rv$current_id %in% names(rv$datasets)) {
        data_load_progress(
          0.98,
          ui_text("syncing_display_window"),
          ui_text("data_loading_raw")
        )
        refresh_xrange_slider(rv$datasets[[rv$current_id]]$trains, reset = TRUE)
        if (loaded_count > 0L) {
          select_default_timestamp_tab(rv$datasets[[rv$current_id]], defer = TRUE)
        }
      }
      if (loaded_count > 0L) {
        reset_dataset_bound_ui_state()
        data_load_progress(
          1,
          ui_text("raw_datasets_loaded", loaded = loaded_count, failed = failed_count),
          ui_text("data_load_complete"),
          "success"
        )
      } else {
        data_load_progress(
          1,
          ui_text("no_new_datasets"),
          ui_text("no_new_dataset_title"),
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
    withProgress(message = ui_text("data_loading_labeled"), value = 0, {
      loaded_count <- 0L
      failed_count <- 0L
      data_load_progress(
        0.01,
        ui_text("preparing_labeled_csv", n = nrow(files)),
        ui_text("data_loading_labeled")
      )
      for (i in seq_len(nrow(files))) {
        path <- files$datapath[i]
        nm <- files$name[i]
        file_start <- (i - 1) / n_files
        file_span <- 1 / n_files
        data_load_progress(
          file_start + 0.08 * file_span,
          ui_text("reading_labeled_csv", index = i, total = nrow(files), name = nm),
          ui_text("data_loading_labeled")
        )
        id <- paste0("annot_", digest(paste0(nm, "_", file.info(path)$size), algo = "xxhash64"))
        if (id %in% names(ds)) {
          data_load_progress(
            file_start + file_span,
            ui_text("skipped_duplicate_dataset", name = nm),
            ui_text("data_loading_labeled")
          )
          next
        }
        trains <- tryCatch(
          build_trains_from_annot(path, unit_in = input$unit_in_annot, duplicate_policy = input$duplicate_timestamp_policy %||% "error_keep"),
          error = function(e) {
            failed_count <<- failed_count + 1L
            failure_title <- ui_text("labeled_file_loading_failed")
            failure_detail <- ui_condition_detail(e)
            data_load_progress(
              file_start + 0.95 * file_span,
              paste0(nm, " | ", ui_condition_detail(e)),
              ui_text("labeled_file_loading_failed"),
              "error"
            )
            showNotification(
              paste0(failure_title, if (identical(ui_language(), "en")) ": " else "\uFF1A", nm, " | ", failure_detail),
              type = "error", duration = 8
            )
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
            ui_text("parsed_trains_building_dataset", n = length(trains)),
            ui_text("data_loading_labeled")
          )
          ds[[id]] <- make_dataset(name = nm, source = "annot", trains = trains, unit_in = input$unit_in_annot, task_events = task_events)
          ds[[id]] <- stpd_attach_input_provenance(
            ds[[id]], path, input_file_name = nm, parser_mode = "labeled",
            unit_in = input$unit_in_annot, header = TRUE,
            duplicate_policy = input$duplicate_timestamp_policy %||% "error_keep"
          )
          data_load_progress(
            file_start + 0.78 * file_span,
            ui_text("running_import_qc"),
            ui_text("data_loading_labeled")
          )
          ds[[id]]$quality <- validate_dataset_quality_impl(trains, min_isi_sec = min_valid_isi_sec(), unit_hint = input$unit_in_annot, refractory_suspect_sec = refractory_suspect_sec(), display_unit = qc_isi_unit())
          ds[[id]]$quality_artifact_details <- tryCatch(
            artifact_isi_details(trains, min_isi_sec = min_valid_isi_sec(), display_unit = qc_isi_unit()),
            error = function(e) data.frame()
          )
          ds[[id]]$quality_duplicate_details <- tryCatch(
            duplicate_timestamp_details(trains, display_unit = qc_isi_unit()),
            error = function(e) data.frame()
          )
          rv$current_id <- id
          loaded_count <- loaded_count + 1L
          data_load_progress(
            file_start + 0.94 * file_span,
            ui_text("refreshing_dataset_and_plot"),
            ui_text("data_loading_labeled")
          )
          task_note <- if (nrow(task_events) > 0L) paste0("\n", ui_text("task_events_recognized", n = nrow(task_events))) else ""
          showNotification(paste0(quality_notification_text(ds[[id]]$quality, lang = ui_language()), task_note), type = ifelse(any(ds[[id]]$quality$warning_level == "error"), "error", ifelse(any(ds[[id]]$quality$warning_level == "warning"), "warning", "message")), duration = 7)
        }
      }
      rv$datasets <- ds
      sync_dataset_selector(rv$current_id)
      if (!is.null(rv$current_id) && rv$current_id %in% names(rv$datasets)) {
        data_load_progress(
          0.98,
          ui_text("syncing_display_window"),
          ui_text("data_loading_labeled")
        )
        refresh_xrange_slider(rv$datasets[[rv$current_id]]$trains, reset = TRUE)
        if (loaded_count > 0L) {
          select_default_timestamp_tab(rv$datasets[[rv$current_id]], defer = TRUE)
        }
      }
      if (loaded_count > 0L) {
        reset_dataset_bound_ui_state()
        data_load_progress(
          1,
          ui_text("labeled_datasets_loaded", loaded = loaded_count, failed = failed_count),
          ui_text("data_load_complete"),
          "success"
        )
      } else {
        data_load_progress(
          1,
          ui_text("no_new_datasets"),
          ui_text("no_new_dataset_title"),
          "error"
        )
      }
    })
  }, ignoreNULL = TRUE)

  apply_workspace_import_candidate <- function(candidate) {
    validate(need(is.list(candidate) && is.list(candidate$datasets),
                  ui_inline_text("\u5DE5\u4F5C\u533A\u5019\u9009\u5BF9\u8C61\u65E0\u6548\u3002",
                                 "The workspace candidate is invalid.")))
    rv$datasets <- candidate$datasets
    rv$current_id <- candidate$current_id
    reset_dataset_bound_ui_state()
    current_results <- if (!is.null(rv$current_id) &&
        rv$current_id %in% names(rv$datasets)) {
      rv$datasets[[rv$current_id]]$results %||% list()
    } else {
      list()
    }
    rv$manual_detector_eval <- current_results$manual_detector_eval_ui %||% NULL
    rv$scientific_validation <- current_results[["scientific_validation"]] %||% NULL
    legacy_manual_report <- candidate$manual_detector_eval %||% NULL
    legacy_manual_identity <- (legacy_manual_report %||% list())[["ui_identity"]] %||% NULL
    if (is.null(rv$manual_detector_eval) && !is.null(legacy_manual_identity) &&
        identical(as.character(legacy_manual_identity$dataset_id %||% "")[1],
                  as.character(rv$current_id %||% "")[1])) {
      rv$manual_detector_eval <- legacy_manual_report
    }
    legacy_scientific_report <- candidate[["scientific_validation"]] %||% NULL
    legacy_scientific_identity <-
      (legacy_scientific_report %||% list())[["ui_identity"]] %||% NULL
    if (is.null(rv$scientific_validation) &&
        !is.null(legacy_scientific_identity) &&
        identical(as.character(legacy_scientific_identity$dataset_id %||% "")[1],
                  as.character(rv$current_id %||% "")[1])) {
      rv$scientific_validation <- legacy_scientific_report
    }
    rv$manual_detector_eval_identity <-
      (rv$manual_detector_eval %||% list())[["ui_identity"]] %||% NULL
    rv$scientific_validation_identity <-
      (rv$scientific_validation %||% list())[["ui_identity"]] %||% NULL
    restored_run_identities <- candidate$ui_run_identity_by_dataset %||% list()
    if (!is.list(restored_run_identities)) restored_run_identities <- list()
    rv$ui_run_identity_by_dataset <- restored_run_identities[
      intersect(names(restored_run_identities), names(rv$datasets))
    ]
    sync_dataset_selector(rv$current_id)
    if (!is.null(rv$current_id) && rv$current_id %in% names(rv$datasets)) {
      refresh_xrange_slider(rv$datasets[[rv$current_id]]$trains, reset = TRUE)
      select_default_timestamp_tab(rv$datasets[[rv$current_id]], defer = TRUE)
    }
    rv$pending_workspace_import <- NULL
    data_load_progress(
      1,
      ui_inline_text(
        sprintf("\u5DF2\u6062\u590D %d \u4E2A\u6570\u636E\u96C6\u3002", length(rv$datasets)),
        sprintf("Restored %d dataset(s).", length(rv$datasets))
      ),
      ui_inline_text("\u5DE5\u4F5C\u533A\u52A0\u8F7D\u5B8C\u6210", "Workspace loading complete"),
      "success"
    )
    showNotification(
      ui_inline_text(
        paste0("\u5DE5\u4F5C\u533A\u5DF2\u52A0\u8F7D\uFF1A", length(rv$datasets), " \u4E2A\u6570\u636E\u96C6\u3002"),
        paste0("Workspace loaded: ", length(rv$datasets), " dataset(s).")
      ),
      type = "message", duration = 6
    )
    invisible(TRUE)
  }
  
  observeEvent(input$workspace_in, {
    req(input$workspace_in)
    workspace_loading <- ui_inline_text("\u6B63\u5728\u52A0\u8F7D\u5DE5\u4F5C\u533A", "Loading workspace")
    workspace_failed <- ui_inline_text("\u5DE5\u4F5C\u533A RDS \u5BFC\u5165\u5931\u8D25", "Workspace RDS import failed")
    withProgress(message = workspace_loading, value = 0, {
      data_load_progress(
        0.05,
        ui_inline_text("\u6B63\u5728\u8BFB\u53D6 RDS \u6587\u4EF6", "Reading the RDS file"),
        ui_inline_text("\u6B63\u5728\u52A0\u8F7D\u5DE5\u4F5C\u533A", "Loading workspace")
      )
      obj <- tryCatch(
        stpd_safe_read_rds(input$workspace_in$datapath, max_bytes = 100 * 1024^2, label = "workspace RDS"),
        error = function(e) {
          failure_title <- workspace_failed
          failure_detail <- ui_condition_detail(e)
          data_load_progress(
            1,
            ui_condition_detail(e),
            ui_inline_text("\u5DE5\u4F5C\u533A RDS \u5BFC\u5165\u5931\u8D25", "Workspace RDS import failed"),
            "error"
          )
          showNotification(paste(failure_title, failure_detail), type = "error", duration = 8)
          NULL
        }
      )
      if (is.null(obj)) return(invisible(NULL))
      data_load_progress(
        0.18,
        ui_inline_text("\u6B63\u5728\u9A8C\u8BC1\u5DE5\u4F5C\u533A\u7ED3\u6784", "Validating the workspace structure"),
        ui_inline_text("\u6B63\u5728\u52A0\u8F7D\u5DE5\u4F5C\u533A", "Loading workspace")
      )
      if (!stpd_workspace_rds_is_valid(obj)) {
        data_load_progress(
          1,
          ui_inline_text(
            "\u5DE5\u4F5C\u533A\u6587\u4EF6\u65E0\u6548\u6216\u8D85\u51FA\u5B89\u5168\u5BFC\u5165\u9650\u5236\u3002",
            "The workspace file is invalid or exceeds the safe import limits."
          ),
          ui_inline_text("\u5DE5\u4F5C\u533A RDS \u5BFC\u5165\u5931\u8D25", "Workspace RDS import failed"),
          "error"
        )
      }
      validate(need(
        stpd_workspace_rds_is_valid(obj),
        ui_inline_text(
          "\u5DE5\u4F5C\u533A\u6587\u4EF6\u65E0\u6548\u6216\u8D85\u51FA\u5B89\u5168\u5BFC\u5165\u9650\u5236\u3002",
          "The workspace file is invalid or exceeds the safe import limits."
        )
      ))
      candidate_datasets <- obj$datasets
      dataset_ids <- names(candidate_datasets)
      prepared <- tryCatch({
        if (length(candidate_datasets) > 0L) {
          for (ii in seq_along(dataset_ids)) {
            id <- dataset_ids[[ii]]
            ds_start <- 0.22 + 0.58 * (ii - 1) / max(1L, length(dataset_ids))
            ds_span <- 0.58 / max(1L, length(dataset_ids))
            data_load_progress(
              ds_start,
              ui_inline_text(
                sprintf("\u9884\u8BA1\u7B97 ISI \u767E\u5206\u4F4D %d/%d\uFF1A%s", ii, length(dataset_ids), id),
                sprintf("Precomputing ISI percentiles %d/%d: %s", ii, length(dataset_ids), id)
              ),
              ui_inline_text("\u6B63\u5728\u52A0\u8F7D\u5DE5\u4F5C\u533A", "Loading workspace")
            )
            candidate_datasets[[id]]$trains <- precompute_trains_isi_percentiles(
              candidate_datasets[[id]]$trains,
              min_isi_sec = min_valid_isi_sec(),
              force = FALSE,
              progress = function(train, index, total) {
                data_load_progress(
                  value = ds_start + ds_span * min(0.95, max(0, index / max(1L, total))),
                  detail = ui_inline_text(
                    sprintf("\u9884\u8BA1\u7B97 %s\uFF1Atrain %d/%d", id, index, total),
                    sprintf("Precomputing %s: train %d/%d", id, index, total)
                  ),
                  message = ui_inline_text("\u6B63\u5728\u52A0\u8F7D\u5DE5\u4F5C\u533A", "Loading workspace")
                )
              }
            )
            candidate_datasets[[id]] <- normalize_dataset(candidate_datasets[[id]])
          }
        }
        candidate_datasets
      }, error = identity)
      if (inherits(prepared, "error")) {
        failure_title <- ui_inline_text(
          "\u5DE5\u4F5C\u533A\u9A8C\u8BC1/\u9884\u5904\u7406\u5931\u8D25\uFF1B\u5F53\u524D session \u672A\u6539\u53D8\u3002",
          "Workspace validation/preprocessing failed; the current session is unchanged."
        )
        failure_detail <- ui_condition_detail(prepared)
        data_load_progress(
          1, ui_condition_detail(prepared),
          ui_inline_text("\u5DE5\u4F5C\u533A RDS \u5BFC\u5165\u5931\u8D25", "Workspace RDS import failed"), "error"
        )
        showNotification(
          paste(failure_title, failure_detail),
          type = "error", duration = 10
        )
        return(invisible(NULL))
      }
      candidate_current_id <- as.character(obj$current_id %||% "")[1]
      if (!nzchar(candidate_current_id) ||
          !(candidate_current_id %in% names(prepared))) {
        candidate_current_id <- if (length(prepared) > 0L) names(prepared)[1] else NULL
      }
      candidate <- list(
        datasets = prepared,
        current_id = candidate_current_id,
        manual_detector_eval = obj$manual_detector_eval %||% NULL,
        scientific_validation = obj[["scientific_validation"]] %||% NULL,
        ui_run_identity_by_dataset = obj$ui_run_identity_by_dataset %||% list()
      )
      if (length(rv$datasets) > 0L) {
        rv$pending_workspace_import <- candidate
        candidate_sha <- stpd_ui_state_sha256(candidate)
        data_load_progress(
          0.9,
          ui_inline_text(
            "\u5DE5\u4F5C\u533A\u5DF2\u9A8C\u8BC1\uFF1B\u7B49\u5F85\u786E\u8BA4\u662F\u5426\u66FF\u6362\u5F53\u524D session\u3002",
            "Workspace validated; waiting for confirmation to replace the current session."
          ),
          ui_inline_text("\u7B49\u5F85\u7528\u6237\u786E\u8BA4", "Waiting for confirmation")
        )
        show_ui_confirmation(
          "confirm_replace_workspace",
          ui_inline_text("\u786E\u8BA4\u52A0\u8F7D\u5DE5\u4F5C\u533A", "Confirm workspace loading"),
          ui_inline_text(
            paste0("\u5DF2\u5B8C\u6574\u9A8C\u8BC1\u65B0\u5DE5\u4F5C\u533A\uFF08", length(prepared),
                   " \u4E2A\u6570\u636E\u96C6\uFF09\u3002\u7EE7\u7EED\u5C06\u66FF\u6362\u5F53\u524D session \u4E2D\u7684 ",
                   length(rv$datasets), " \u4E2A\u6570\u636E\u96C6\uFF1B\u672C session \u4E2D\u53EF\u64A4\u9500\u3002"),
            paste0("The new workspace has been fully validated (", length(prepared),
                   " dataset(s)). Continuing will replace ", length(rv$datasets),
                   " dataset(s) in the current session; this can be undone during this session.")
          ),
          context = list(candidate_sha256 = candidate_sha)
        )
      } else {
        apply_workspace_import_candidate(candidate)
      }
    })
  })

  observeEvent(input$confirm_replace_workspace, {
    pending <- consume_ui_confirmation("confirm_replace_workspace")
    if (is.null(pending)) return()
    candidate <- isolate(rv$pending_workspace_import)
    rv$pending_workspace_import <- NULL
    expected_sha <- as.character(
      ((pending$context %||% list())$candidate_sha256 %||% "")
    )[1]
    actual_sha <- if (is.list(candidate)) stpd_ui_state_sha256(candidate) else ""
    if (!nzchar(expected_sha) || !identical(expected_sha, actual_sha)) {
      data_load_progress(
        1,
        ui_inline_text(
          "\u5F85\u52A0\u8F7D\u5DE5\u4F5C\u533A\u5DF2\u6539\u53D8\uFF1B\u5F53\u524D session \u672A\u6539\u53D8\u3002",
          "The pending workspace changed; the current session is unchanged."
        ),
        ui_inline_text("\u5DE5\u4F5C\u533A\u66FF\u6362\u5DF2\u963B\u6B62", "Workspace replacement blocked"),
        "error"
      )
      showNotification(
        ui_inline_text(
          "\u5F85\u52A0\u8F7D\u5DE5\u4F5C\u533A\u5728\u786E\u8BA4\u524D\u53D1\u751F\u4E86\u53D8\u5316\uFF1B\u5F53\u524D session \u672A\u6539\u53D8\u3002\u8BF7\u91CD\u65B0\u9009\u62E9\u5DE5\u4F5C\u533A\u6587\u4EF6\u3002",
          "The pending workspace changed before confirmation; the current session is unchanged. Select the workspace file again."
        ),
        type = "error", duration = 8
      )
      return()
    }
    tryCatch(
      stpd_ui_transaction_run(
        rv,
        action_id = "replace_workspace",
        dataset_ids = names(rv$datasets),
        target_count = length(rv$datasets),
        details = list(candidate_dataset_n = length(candidate$datasets)),
        max_depth = 5L,
        max_bytes = 64 * 1024^2,
        mutate = function() {
          apply_workspace_import_candidate(candidate)
          TRUE
        }
      ),
      error = function(e) {
        failure_title <- ui_inline_text(
          "\u5DE5\u4F5C\u533A\u66FF\u6362\u5931\u8D25\uFF0C\u539F session \u5DF2\u81EA\u52A8\u6062\u590D\u3002",
          "Workspace replacement failed; the previous session was restored automatically."
        )
        failure_detail <- ui_condition_detail(e)
        data_load_progress(
          1, ui_condition_detail(e),
          ui_inline_text("\u5DE5\u4F5C\u533A\u66FF\u6362\u5931\u8D25", "Workspace replacement failed"), "error"
        )
        showNotification(
          paste(failure_title, failure_detail),
          type = "error", duration = 10
        )
      }
    )
  })
  
  output$workspace_out <- downloadHandler(
    filename = function() paste0("spike_detector_workspace_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"),
    content = function(file) {
      saveRDS(list(datasets = rv$datasets, current_id = rv$current_id,
	                   manual_detector_eval = rv$manual_detector_eval,
	                   scientific_validation = rv$scientific_validation,
	                   ui_run_identity_by_dataset = rv$ui_run_identity_by_dataset), file)
    }
  )
  
  observeEvent(input$clear_all_datasets, {
    if (length(rv$datasets) == 0L) return()
    show_ui_confirmation(
      "confirm_clear_all_datasets",
      ui_inline_text("\u786E\u8BA4\u6E05\u7A7A\u5185\u5B58", "Confirm clearing session data"),
      ui_inline_text(
        paste0("\u5C06\u4ECE\u5F53\u524D session \u79FB\u9664 ", length(rv$datasets), " \u4E2A\u6570\u636E\u96C6\uFF0C\u5E76\u6E05\u9664\u4E0E\u5B83\u4EEC\u7ED1\u5B9A\u7684\u754C\u9762\u9A8C\u8BC1\u72B6\u6001\u3002\u6B64\u64CD\u4F5C\u53EF\u5728\u672C session \u4E2D\u64A4\u9500\u3002"),
        paste0("This will remove ", length(rv$datasets), " dataset(s) from the current session and clear their linked UI validation state. This can be undone during this session.")
      ),
      typed_token = "\u6E05\u7A7A",
      context = list(dataset_ids = sort(names(rv$datasets), method = "radix"))
    )
  })

  observeEvent(input$confirm_clear_all_datasets, {
    token <- trimws(as.character(input$confirm_clear_all_datasets_token %||% "")[1])
    if (!identical(token, "\u6E05\u7A7A")) {
      showNotification(
        ui_inline_text("\u8BF7\u8F93\u5165\u201C\u6E05\u7A7A\u201D\u540E\u518D\u786E\u8BA4\u3002", "Enter \"\u6E05\u7A7A\" before confirming."),
        type = "error", duration = 5
      )
      return()
    }
    pending <- consume_ui_confirmation("confirm_clear_all_datasets")
    if (is.null(pending)) return()
    target_ids <- sort(as.character(
      (pending$context %||% list())$dataset_ids %||% character()
    ), method = "radix")
    if (!identical(target_ids, sort(names(rv$datasets), method = "radix"))) {
      showNotification(
        ui_inline_text(
          "\u6570\u636E\u96C6\u5217\u8868\u5728\u786E\u8BA4\u7A97\u53E3\u6253\u5F00\u540E\u53D1\u751F\u4E86\u53D8\u5316\uFF1B\u672A\u6E05\u7A7A\u4EFB\u4F55\u6570\u636E\u3002\u8BF7\u91CD\u65B0\u53D1\u8D77\u64CD\u4F5C\u3002",
          "The dataset list changed after the confirmation opened; no data were cleared. Start the action again."
        ),
        type = "error", duration = 7
      )
      return()
    }
    ok <- run_manual_ui_action({
      stpd_ui_transaction_run(
        rv,
        action_id = "clear_all_datasets",
        dataset_ids = target_ids,
        target_count = length(target_ids),
        details = list(
          ui_action_label = ui_inline_text("\u6E05\u7A7A\u5185\u5B58", "Clear session data"),
          ui_action_label_zh = "\u6E05\u7A7A\u5185\u5B58",
          ui_action_label_en = "Clear session data"
        ),
        max_depth = 5L,
        max_bytes = 64 * 1024^2,
        mutate = function() {
          rv$datasets <- list()
          rv$current_id <- NULL
	          reset_dataset_bound_ui_state()
	          rv$ui_run_identity_by_dataset <- list()
          TRUE
        }
      )
    }, prefix = ui_inline_text("\u6E05\u7A7A\u5185\u5B58\u5931\u8D25", "Failed to clear session data"))
    if (!isTRUE(ok)) return()
    showNotification(
      ui_inline_text(
        "\u5DF2\u6E05\u7A7A\u5F53\u524D session \u6570\u636E\u96C6\uFF1B\u53EF\u4F7F\u7528\u201C\u64A4\u9500\u4E0A\u4E00\u6B21\u53D8\u66F4\u201D\u6062\u590D\u3002",
        "Current-session datasets were cleared. Use Undo last change to restore them."
      ),
      type = "message", duration = 6
    )
  })
  
  output$dataset_selector <- renderUI({
    ds <- rv$datasets
    if (length(ds) == 0) return(helpText(ui_inline_text("\u672A\u52A0\u8F7D\u6570\u636E\u96C6\u3002", "No datasets are loaded.")))
    ids <- names(ds)
    labels <- dataset_selector_labels(ds)
    selectizeInput("dataset_id", ui_inline_text("\u5F53\u524D\u6570\u636E\u96C6", "Current dataset"), choices = setNames(ids, labels), selected = rv$current_id %||% ids[1], multiple = FALSE)
  })
  
  observeEvent(input$dataset_id, {
    id <- as.character(input$dataset_id %||% "")[1]
    if (!nzchar(id) || !(id %in% names(rv$datasets))) return()
    if (identical(id, rv$current_id)) return()
    reset_dataset_bound_ui_state()
    rv$current_id <- id
    ds <- rv$datasets[[id]]
    rv$manual_detector_eval <- (ds$results %||% list())$manual_detector_eval_ui %||% NULL
    rv$scientific_validation <- (ds$results %||% list())[["scientific_validation"]] %||% NULL
    rv$manual_detector_eval_identity <- (rv$manual_detector_eval %||% list())[["ui_identity"]] %||% NULL
    rv$scientific_validation_identity <- (rv$scientific_validation %||% list())[["ui_identity"]] %||% NULL
    select_default_timestamp_tab(rv$datasets[[id]], defer = TRUE)
  }, ignoreInit = TRUE)
  
  output$pool_dataset_selector <- renderUI({
    ds <- rv$datasets
    if (length(ds) <= 1) return(NULL)
    ids <- names(ds)
    labels <- vapply(ds, function(x) paste0("[", x$meta$source, "] ", x$meta$display_name), character(1))
    selected <- intersect(
      as.character(ui_control_remembered(
        "pool_ids", rv$current_id %||% ids[1], dataset_bound = FALSE
      )),
      ids
    )
    selectizeInput("pool_ids", ui_inline_text("\u7528\u4E8E\u53C2\u6570\u4F30\u8BA1 / \u5408\u5E76\u76F4\u65B9\u56FE\u7684\u6570\u636E\u96C6", "Datasets for parameter estimation / pooled histograms"),
                   choices = setNames(ids, labels), selected = selected, multiple = TRUE,
                   options = list(placeholder = ui_inline_text("\u9009\u62E9\u4E00\u4E2A\u6216\u591A\u4E2A\u6570\u636E\u96C6", "Select one or more datasets")))
  })
  
  observeEvent(input$remove_dataset, {
    id <- rv$current_id
    req(id)
    ds <- rv$datasets[[id]]
    if (is.null(ds)) return()
    nm <- as.character((ds$meta %||% list())$display_name %||% id)[1]
    show_ui_confirmation(
      "confirm_remove_dataset",
      ui_inline_text("\u786E\u8BA4\u79FB\u9664\u6570\u636E\u96C6", "Confirm dataset removal"),
      ui_inline_text(
        paste0("\u5C06\u79FB\u9664\u201C", nm, "\u201D\uFF08", length(ds$trains %||% list()), " \u6761 trains\uFF09\u3002\u672C session \u4E2D\u53EF\u64A4\u9500\u3002"),
        paste0("Remove \"", nm, "\" (", length(ds$trains %||% list()), " train(s)). This can be undone during this session.")
      ),
      context = list(dataset_id = id)
    )
  })

  observeEvent(input$confirm_remove_dataset, {
    pending <- consume_ui_confirmation("confirm_remove_dataset")
    if (is.null(pending)) return()
    id <- as.character((pending$context %||% list())$dataset_id %||% "")[1]
    if (!nzchar(id)) return()
    ds <- rv$datasets
    if (!(id %in% names(ds))) {
      showNotification(
        ui_inline_text("\u786E\u8BA4\u7684\u76EE\u6807\u6570\u636E\u96C6\u5DF2\u4E0D\u5B58\u5728\uFF1B\u6CA1\u6709\u8FDB\u884C\u4EFB\u4F55\u66F4\u6539\u3002", "The confirmed target dataset no longer exists; no changes were made."),
        type = "error", duration = 6
      )
      return()
    }
    ok <- run_manual_ui_action({
      stpd_ui_transaction_run(
        rv,
        action_id = "remove_dataset",
        dataset_ids = id,
        target_count = 1L,
        details = list(
          ui_action_label = ui_inline_text(paste0("\u79FB\u9664\u6570\u636E\u96C6 ", id), paste0("Remove dataset ", id)),
          ui_action_label_zh = paste0("\u79FB\u9664\u6570\u636E\u96C6 ", id),
          ui_action_label_en = paste0("Remove dataset ", id)
        ),
        max_depth = 5L,
        max_bytes = 64 * 1024^2,
        mutate = function() {
          ds[[id]] <- NULL
          rv$datasets <- ds
          rv$current_id <- if (length(ds) > 0) names(ds)[1] else NULL
	          reset_dataset_bound_ui_state()
	          next_ds <- if (!is.null(rv$current_id)) ds[[rv$current_id]] else NULL
	          rv$manual_detector_eval <- ((next_ds %||% list())$results %||% list())$manual_detector_eval_ui %||% NULL
	          rv$scientific_validation <- ((next_ds %||% list())$results %||% list())[["scientific_validation"]] %||% NULL
	          rv$manual_detector_eval_identity <- (rv$manual_detector_eval %||% list())[["ui_identity"]] %||% NULL
	          rv$scientific_validation_identity <- (rv$scientific_validation %||% list())[["ui_identity"]] %||% NULL
	          run_identities <- rv$ui_run_identity_by_dataset %||% list()
	          run_identities[[id]] <- NULL
	          rv$ui_run_identity_by_dataset <- run_identities
          TRUE
        }
      )
    }, prefix = ui_inline_text("\u79FB\u9664\u6570\u636E\u96C6\u5931\u8D25", "Failed to remove the dataset"))
    if (!isTRUE(ok)) return()
    sync_dataset_selector(rv$current_id)
    if (!is.null(rv$current_id) && rv$current_id %in% names(rv$datasets)) {
      select_default_timestamp_tab(rv$datasets[[rv$current_id]], defer = TRUE)
    }
    showNotification(
      ui_inline_text("\u6570\u636E\u96C6\u5DF2\u79FB\u9664\uFF1B\u53EF\u4F7F\u7528\u201C\u64A4\u9500\u4E0A\u4E00\u6B21\u53D8\u66F4\u201D\u6062\u590D\u3002", "Dataset removed. Use Undo last change to restore it."),
      type = "message", duration = 5
    )
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
    ds$quality_artifact_details <- tryCatch(
      artifact_isi_details(ds$trains, min_isi_sec = min_valid_isi_sec(), display_unit = qc_isi_unit()),
      error = function(e) data.frame()
    )
    ds$quality_duplicate_details <- tryCatch(
      duplicate_timestamp_details(ds$trains, display_unit = qc_isi_unit()),
      error = function(e) data.frame()
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
    show_ui_confirmation(
      "confirm_collapse_duplicate_spikes_current",
      ui_inline_text("\u786E\u8BA4\u5408\u5E76\u91CD\u590D timestamp", "Confirm merging duplicate timestamps"),
      ui_inline_text(
        "\u5C06\u5220\u9664\u5F53\u524D\u6570\u636E\u96C6\u5185\u5B8C\u5168\u91CD\u590D\u7684 spike timestamp\uFF0C\u5E76\u4F7F AUTO \u4E0E\u68C0\u6D4B\u7ED3\u679C\u5931\u6548\u3002\u672C session \u4E2D\u53EF\u64A4\u9500\u3002",
        "Exact duplicate spike timestamps in the current dataset will be removed, invalidating AUTO labels and detector results. This can be undone during this session."
      ),
      context = list(dataset_id = id)
    )
  })

  observeEvent(input$confirm_collapse_duplicate_spikes_current, {
    pending <- consume_ui_confirmation("confirm_collapse_duplicate_spikes_current")
    if (is.null(pending)) return()
    id <- as.character((pending$context %||% list())$dataset_id %||% "")[1]
    if (!nzchar(id)) return()
    ds_all <- rv$datasets
    if (!(id %in% names(ds_all))) return()
    res <- collapse_duplicate_spikes_for_dataset(normalize_dataset(ds_all[[id]]), dataset_label = id)
    if (res$dropped > 0) {
      ok <- run_manual_ui_action({
        stpd_ui_transaction_run(
          rv,
          action_id = "collapse_duplicate_spikes",
          dataset_ids = id,
          target_count = res$dropped,
          details = list(
            ui_action_label = ui_inline_text(paste0("\u5408\u5E76\u91CD\u590D timestamp\uFF1A", id), paste0("Merge duplicate timestamps: ", id)),
            ui_action_label_zh = paste0("\u5408\u5E76\u91CD\u590D timestamp\uFF1A", id),
            ui_action_label_en = paste0("Merge duplicate timestamps: ", id)
          ),
          max_depth = 5L,
          max_bytes = 64 * 1024^2,
          mutate = function() {
            ds_all[[id]] <- res$dataset
            rv$datasets <- ds_all
            refresh_xrange_slider(res$dataset$trains, reset = TRUE)
            TRUE
          }
        )
      }, prefix = ui_inline_text("\u5408\u5E76\u91CD\u590D timestamp \u5931\u8D25", "Failed to merge duplicate timestamps"))
      if (!isTRUE(ok)) return()
    }
    if (res$dropped > 0) {
      showNotification(
        ui_inline_text(
          paste0("\u5DF2\u5408\u5E76 ", res$dropped, " \u4E2A\u5F53\u524D\u6570\u636E\u96C6\u4E2D\u7684\u5B8C\u5168\u91CD\u590D spike \u65F6\u95F4\u6233\u3002AUTO \u6807\u7B7E/\u7ED3\u679C\u5DF2\u6E05\u7A7A\uFF1B\u8BF7\u91CD\u65B0\u8FD0\u884C\u68C0\u6D4B\u5668\u3002"),
          paste0("Merged ", res$dropped, " exact duplicate spike timestamp(s) in the current dataset. AUTO labels/results were cleared; rerun the detector.")
        ),
        type = "message", duration = 8
      )
    } else {
      showNotification(ui_inline_text("\u5F53\u524D\u6570\u636E\u96C6\u4E2D\u672A\u53D1\u73B0\u5B8C\u5168\u91CD\u590D\u65F6\u95F4\u6233\u3002", "No exact duplicate timestamps were found in the current dataset."), type = "message", duration = 5)
    }
  })

  observeEvent(input$collapse_duplicate_spikes_all, {
    if (length(rv$datasets) == 0) return()
    show_ui_confirmation(
      "confirm_collapse_duplicate_spikes_all",
      ui_inline_text("\u786E\u8BA4\u5408\u5E76\u6240\u6709\u6570\u636E\u96C6\u7684\u91CD\u590D timestamp", "Confirm merging duplicates across all datasets"),
      ui_inline_text(
        paste0("\u5C06\u68C0\u67E5\u5E76\u4FEE\u6539 ", length(rv$datasets), " \u4E2A\u6570\u636E\u96C6\uFF0C\u6709\u53D8\u5316\u7684\u6570\u636E\u96C6\u5176 AUTO \u4E0E\u68C0\u6D4B\u7ED3\u679C\u5C06\u5931\u6548\u3002\u672C session \u4E2D\u53EF\u64A4\u9500\u3002"),
        paste0("This will inspect and modify ", length(rv$datasets), " dataset(s). AUTO labels and detector results will be invalidated for changed datasets. This can be undone during this session.")
      ),
      context = list(dataset_ids = sort(names(rv$datasets), method = "radix"))
    )
  })

  observeEvent(input$confirm_collapse_duplicate_spikes_all, {
    pending <- consume_ui_confirmation("confirm_collapse_duplicate_spikes_all")
    if (is.null(pending)) return()
    target_ids <- sort(as.character(
      (pending$context %||% list())$dataset_ids %||% character()
    ), method = "radix")
    if (length(target_ids) == 0L ||
        !identical(target_ids, sort(names(rv$datasets), method = "radix"))) {
      showNotification(
        ui_inline_text(
          "\u6570\u636E\u96C6\u5217\u8868\u5728\u786E\u8BA4\u7A97\u53E3\u6253\u5F00\u540E\u53D1\u751F\u4E86\u53D8\u5316\uFF1B\u6CA1\u6709\u5408\u5E76\u4EFB\u4F55\u6570\u636E\u3002\u8BF7\u91CD\u65B0\u53D1\u8D77\u64CD\u4F5C\u3002",
          "The dataset list changed after the confirmation opened; no duplicate timestamps were merged. Start the action again."
        ),
        type = "error", duration = 7
      )
      return()
    }
    ds_all <- rv$datasets
    total <- 0L
    for (id in names(ds_all)) {
      res <- collapse_duplicate_spikes_for_dataset(normalize_dataset(ds_all[[id]]), dataset_label = id)
      ds_all[[id]] <- res$dataset
      total <- total + res$dropped
    }
    if (total > 0) {
      ok <- run_manual_ui_action({
        stpd_ui_transaction_run(
          rv,
          action_id = "collapse_duplicate_spikes",
          dataset_ids = target_ids,
          target_count = total,
          details = list(
            ui_action_label = ui_inline_text("\u5408\u5E76\u6240\u6709\u6570\u636E\u96C6\u7684\u91CD\u590D timestamp", "Merge duplicate timestamps in all datasets"),
            ui_action_label_zh = "\u5408\u5E76\u6240\u6709\u6570\u636E\u96C6\u7684\u91CD\u590D timestamp",
            ui_action_label_en = "Merge duplicate timestamps in all datasets"
          ),
          max_depth = 5L,
          max_bytes = 64 * 1024^2,
          mutate = function() {
            rv$datasets <- ds_all
            if (!is.null(rv$current_id) && rv$current_id %in% names(rv$datasets)) {
              refresh_xrange_slider(rv$datasets[[rv$current_id]]$trains, reset = TRUE)
            }
            TRUE
          }
        )
      }, prefix = ui_inline_text("\u5408\u5E76\u6240\u6709\u6570\u636E\u96C6\u5931\u8D25", "Failed to merge duplicates in all datasets"))
      if (!isTRUE(ok)) return()
      showNotification(
        ui_inline_text(
          paste0("\u5DF2\u5408\u5E76 ", total, " \u4E2A\u6240\u6709\u5DF2\u52A0\u8F7D\u6570\u636E\u96C6\u4E2D\u7684\u5B8C\u5168\u91CD\u590D spike timestamp\u3002AUTO \u6807\u7B7E/\u7ED3\u679C\u5DF2\u6E05\u7A7A\uFF1B\u8BF7\u91CD\u65B0\u8FD0\u884C\u68C0\u6D4B\u5668\u3002"),
          paste0("Merged ", total, " exact duplicate spike timestamp(s) across loaded datasets. AUTO labels/results were cleared; rerun the detector.")
        ),
        type = "message", duration = 8
      )
    } else {
      showNotification(ui_inline_text("\u5DF2\u52A0\u8F7D\u6570\u636E\u96C6\u4E2D\u672A\u53D1\u73B0\u5B8C\u5168\u91CD\u590D timestamp\u3002", "No exact duplicate timestamps were found in the loaded datasets."), type = "message", duration = 5)
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
    numericInput("train_page", ui_inline_text("Train \u9875\u7801\uFF08\u8BB0\u5F55\u6761\u76EE\u9875\u7801\uFF09", "Train page"), value = min(safe_int(input$train_page, 1L), n_pages), min = 1, max = n_pages, step = 1)
  })

  output$train_selector <- renderUI({
    td <- current_trains()
    choices <- metadata_filtered_train_names()
    validate(need(length(choices) > 0, ui_inline_text("\u5F53\u524D\u5143\u6570\u636E\u8FC7\u6EE4\u540E\u6CA1\u6709\u53EF\u7528 train\u3002", "No trains remain after applying the metadata filter.")))
    if (identical(input$train_display_mode %||% "paged_all", "paged_all")) {
      per <- safe_int(input$visible_trains_per_page, 10L)
      per <- max(1L, min(per, length(choices)))
      page <- max(1L, safe_int(input$train_page, 1L))
      n_pages <- max(1L, ceiling(length(choices) / per))
      page <- min(page, n_pages)
      idx <- ((page - 1L) * per + 1L):min(length(choices), page * per)
      return(tags$div(class = "small-note",
                      ui_inline_text(
                        paste0(length(choices), " \u6761\u7ECF\u5143\u6570\u636E\u7B5B\u9009\u7684 train \u5904\u4E8E\u6D3B\u72B6\u6001\u3002\u5F53\u524D\u6E32\u67D3\u7B2C ", page,
                               "/", n_pages, " \u9875\uFF1A", paste(choices[idx], collapse = ", ")),
                        paste0(length(choices), " metadata-filtered train(s) are active. Rendering page ", page,
                               "/", n_pages, ": ", paste(choices[idx], collapse = ", "))
                      )))
    }
    selected <- intersect(
      as.character(ui_control_remembered("trains", head(choices, 10))),
      choices
    )
    selectizeInput("trains", ui_inline_text("\u9009\u62E9\u8981\u663E\u793A\u7684 trains", "Select trains to display"), choices = choices, multiple = TRUE,
                   selected = selected, options = list(placeholder = ui_inline_text("\u9009\u62E9 trains\uFF08\u8BB0\u5F55\u6761\u76EE\uFF09", "Select train recording items")))
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
    # A cleared Shiny multi-select is reported as NULL, which is
    # indistinguishable from its brief pre-initialization state. Fail closed:
    # selectize supplies the first-ten default through `selected` after it
    # initializes, while NULL means no active selected-only scope here.
    requested <- input$trains %||% character(0)
    intersect(as.character(requested), choices)
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
    selectizeInput("burst_range_trains", ui_inline_text("\u7528\u4E8E\u65E7\u7248 burst-ISI \u8303\u56F4\u5206\u914D\u7684 train(s)", "Train(s) for legacy burst-ISI range assignment"), choices = choices,
                   selected = selected, multiple = TRUE,
                   options = list(placeholder = ui_inline_text("\u53EF\u9009\uFF1A\u9009\u62E9\u8981\u4FDD\u5B58/\u6E05\u9664\u65E7\u7248\u8303\u56F4\u7684 train(s)", "Optional: select train(s) whose legacy ranges will be saved or cleared"), plugins = list("remove_button")))
  })
  
  output$burst_range_selector_tab <- renderUI({
    td <- current_trains()
    choices <- names(td)
    selected <- if (is.null(input$burst_range_trains_tab)) character(0) else intersect(input$burst_range_trains_tab, choices)
    selectizeInput("burst_range_trains_tab", ui_inline_text("\u65E7\u7248 train(s)", "Legacy train(s)"), choices = choices,
                   selected = selected, multiple = TRUE,
                   options = list(placeholder = ui_inline_text("\u53EF\u9009\uFF1A\u9009\u62E9\u8981\u4FDD\u5B58/\u6E05\u9664\u65E7\u7248\u8303\u56F4\u7684 train(s)", "Optional: select train(s) whose legacy ranges will be saved or cleared"), plugins = list("remove_button")))
  })
  
  output$isi_table_train_selector <- renderUI({
    td <- current_trains()
    choices <- names(td)
    fallback_selected <- intersect(displayed_train_names() %||% head(choices, 1), choices)
    selected <- intersect(
      as.character(ui_control_remembered("isi_table_trains", fallback_selected)),
      choices
    )
    selectizeInput("isi_table_trains", ui_inline_text("\u5F53\u524D\u663E\u793A\u7684 train(s)", "Currently displayed train(s)"), choices = choices,
                   selected = selected, multiple = TRUE, options = list(maxItems = 20, placeholder = ui_inline_text("\u9009\u62E9 train(s)", "Select train(s)")))
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

stpd_server_install_provider_workbench_module <- function(server_env) {
  evalq({
    if (!exists("ui_current_copy", mode = "function", inherits = TRUE)) {
      ui_current_copy <- function(zh, en) {
        lang <- if (exists("ui_language", mode = "function", inherits = TRUE)) {
          ui_language()
        } else "en"
        if (identical(lang, "en")) as.character(en)[[1L]] else
          as.character(zh)[[1L]]
      }
    }
    provider_clear_descendants <- function(keep_reference = TRUE) {
      rv$provider_composition <- NULL
      rv$provider_review_view <- NULL
      rv$provider_score <- NULL
      if (!isTRUE(keep_reference)) rv$provider_reference <- NULL
      invisible(TRUE)
    }

    provider_notify_error <- function(prefix_zh, prefix_en, error) {
      detail <- ui_condition_detail(error)
      rv$provider_workbench_message <- list(
        level = "danger", detail = paste(ui_current_copy(prefix_zh, prefix_en), detail)
      )
      showNotification(rv$provider_workbench_message$detail,
                       type = "error", duration = 10)
      invisible(NULL)
    }

    provider_read_rds <- function(file, label) {
      if (is.null(file) || is.null(file$datapath)) {
        stop(label, " file is missing.", call. = FALSE)
      }
      stpd_safe_read_rds(file$datapath, max_bytes = 100 * 1024^2,
                         label = label)
    }

    observeEvent(input$provider_bundle_in, {
      req(input$provider_bundle_in)
      tryCatch({
        product <- provider_read_rds(input$provider_bundle_in, "provider bundle RDS")
        stpd_validate_provider_bundle(product)
        rv$provider_bundle <- product
        rv$provider_adjudication <- NULL
        provider_clear_descendants(keep_reference = FALSE)
        rv$provider_workbench_message <- list(
          level = "success",
          detail = ui_current_copy(
            paste0("Provider bundle \u5DF2\u9A8C\u8BC1\uFF1A", nrow(product$provider_runs),
                   " \u4E2A run\uFF1B\u8BF7\u660E\u786E\u9009\u62E9\u4E00\u4E2A\u3002"),
            paste0("Provider bundle validated: ", nrow(product$provider_runs),
                   " run(s); select exactly one.")
          )
        )
      }, error = function(e) provider_notify_error(
        "Provider bundle \u5BFC\u5165\u5931\u8D25\uFF1A", "Provider bundle import failed:", e
      ))
    }, ignoreNULL = TRUE)

    observeEvent(input$provider_adjudication_in, {
      req(input$provider_adjudication_in)
      tryCatch({
        if (is.null(rv$provider_bundle)) stop(
          "Import the exact provider bundle before its adjudication product.",
          call. = FALSE
        )
        product <- provider_read_rds(
          input$provider_adjudication_in, "provider adjudication RDS"
        )
        stpd_validate_provider_adjudication(rv$provider_bundle, product)
        rv$provider_adjudication <- product
        provider_clear_descendants(keep_reference = TRUE)
        rv$provider_workbench_message <- list(
          level = "success",
          detail = ui_current_copy(
            "\u88C1\u51B3\u4EA7\u54C1\u5DF2\u901A\u8FC7\u7236 bundle \u91CD\u653E\u4E0E\u54C8\u5E0C\u9A8C\u8BC1\u3002",
            "Adjudication passed parent-bundle replay and hash validation."
          )
        )
      }, error = function(e) provider_notify_error(
        "\u88C1\u51B3\u4EA7\u54C1\u5BFC\u5165\u5931\u8D25\uFF1A", "Adjudication import failed:", e
      ))
    }, ignoreNULL = TRUE)

    observeEvent(input$provider_reference_in, {
      req(input$provider_reference_in)
      tryCatch({
        product <- provider_read_rds(
          input$provider_reference_in, "provider reference RDS"
        )
        stpd_validate_provider_reference_bundle(product)
        rv$provider_reference <- product
        rv$provider_score <- NULL
        rv$provider_workbench_message <- list(
          level = "success",
          detail = ui_current_copy(
            "\u53C2\u8003\u4EA7\u54C1\u5DF2\u9A8C\u8BC1\uFF1B\u8BC4\u4EF7\u65F6\u4ECD\u4F1A\u91CD\u67E5 dataset hash \u548C estimand \u6743\u9650\u3002",
            "Reference validated; scoring will still recheck the dataset hash and estimand authority."
          )
        )
      }, error = function(e) provider_notify_error(
        "\u53C2\u8003\u4EA7\u54C1\u5BFC\u5165\u5931\u8D25\uFF1A", "Reference import failed:", e
      ))
    }, ignoreNULL = TRUE)

    output$provider_run_selector <- renderUI({
      bundle <- rv$provider_bundle
      if (is.null(bundle)) return(helpText(ui_current_copy(
        "\u8BF7\u5148\u5BFC\u5165 provider bundle\u3002",
        "Import a provider bundle first."
      )))
      runs <- bundle$provider_runs
      labels <- paste0(
        runs$provider_key, " | ", runs$output_role, " | ",
        substr(runs$provider_run_id, 1L, 12L)
      )
      selectInput(
        "provider_selected_run", ui_current_copy("\u5355\u4E00 provider run", "Single provider run"),
        choices = stats::setNames(runs$provider_run_id, labels),
        selected = runs$provider_run_id[[1L]], multiple = FALSE
      )
    })

    observeEvent(list(input$provider_selected_run, input$provider_source_mode), {
      view <- isolate(rv$provider_review_view)
      if (is.null(view)) return()
      requested_run <- as.character(input$provider_selected_run %||% "")[[1L]]
      requested_mode <- as.character(input$provider_source_mode %||% "")[[1L]]
      if (!identical(requested_run, view$metadata$provider_run_id[[1L]]) ||
          !identical(requested_mode, view$metadata$source_mode[[1L]])) {
        provider_clear_descendants(keep_reference = TRUE)
        rv$provider_workbench_message <- list(
          level = "warning",
          detail = ui_current_copy(
            "Provider run \u6216\u6765\u6E90\u6A21\u5F0F\u5DF2\u6539\u53D8\uFF1B\u65E7\u89C6\u56FE\u548C\u8BC4\u5206\u5DF2\u5931\u6548\uFF0C\u8BF7\u91CD\u65B0\u751F\u6210\u3002",
            "The provider run or source mode changed; the prior view and score were invalidated. Rebuild them."
          )
        )
      }
    }, ignoreInit = TRUE)

    output$provider_record_selector <- renderUI({
      bundle <- rv$provider_bundle
      run_id <- as.character(input$provider_selected_run %||% "")[[1L]]
      if (is.null(bundle) || !nzchar(run_id)) return(NULL)
      records <- bundle$candidate_intervals
      records <- records[
        records$provider_run_id == run_id &
          records$provider_decision == "positive", , drop = FALSE
      ]
      if (!nrow(records)) return(helpText(ui_current_copy(
        "\u6240\u9009 run \u6CA1\u6709\u53EF\u88C1\u51B3\u7684 positive record\u3002",
        "The selected run has no positive record to adjudicate."
      )))
      labels <- paste0(
        records$train_key, " | ", records$semantic_track, "/",
        records$proposed_label, " | ", records$canonical_start_isi, "-",
        records$canonical_end_isi, " | ",
        substr(records$candidate_id, 1L, 10L)
      )
      selectInput(
        "provider_selected_record",
        ui_current_copy("\u5355\u4E00 provider record", "Single provider record"),
        choices = stats::setNames(records$candidate_id, labels),
        selected = records$candidate_id[[1L]], multiple = FALSE
      )
    })

    observeEvent(input$provider_selected_record, {
      bundle <- rv$provider_bundle
      record_id <- as.character(input$provider_selected_record %||% "")[[1L]]
      if (is.null(bundle) || !nzchar(record_id)) return()
      record <- bundle$candidate_intervals[
        bundle$candidate_intervals$candidate_id == record_id, , drop = FALSE
      ]
      if (nrow(record) != 1L) return()
      updateNumericInput(session, "provider_adjusted_start",
                         value = record$canonical_start_isi[[1L]])
      updateNumericInput(session, "provider_adjusted_end",
                         value = record$canonical_end_isi[[1L]])
    }, ignoreNULL = TRUE)

    output$provider_prior_decision_selector <- renderUI({
      if (!identical(as.character(input$provider_review_action %||% "")[[1L]],
                     "void_prior_decision")) return(NULL)
      adjudication <- rv$provider_adjudication
      record_id <- as.character(input$provider_selected_record %||% "")[[1L]]
      if (is.null(adjudication) || !nzchar(record_id)) return(helpText(
        ui_current_copy(
          "\u8BE5 record \u5C1A\u65E0\u53EF\u64A4\u9500\u7684\u5F53\u524D\u88C1\u51B3\u3002",
          "This record has no current decision to void."
        )
      ))
      current <- adjudication$current_decisions
      current <- current[current$source_record_id == record_id, , drop = FALSE]
      if (nrow(current) != 1L) return(helpText(ui_current_copy(
        "\u8BE5 record \u5C1A\u65E0\u53EF\u64A4\u9500\u7684\u5F53\u524D\u88C1\u51B3\u3002",
        "This record has no current decision to void."
      )))
      selectInput(
        "provider_target_decision", ui_current_copy(
          "\u8981\u64A4\u9500\u7684\u5F53\u524D decision", "Current decision to void"
        ),
        choices = stats::setNames(
          current$current_decision_id,
          paste0(current$action, " | ", substr(current$current_decision_id, 1L, 12L))
        ), selected = current$current_decision_id[[1L]], multiple = FALSE
      )
    })

    observeEvent(input$provider_apply_review, {
      tryCatch({
        bundle <- rv$provider_bundle
        if (is.null(bundle)) stop("Import a provider bundle first.", call. = FALSE)
        run_id <- as.character(input$provider_selected_run %||% "")[[1L]]
        record_id <- as.character(input$provider_selected_record %||% "")[[1L]]
        if (!nzchar(run_id) || !nzchar(record_id)) stop(
          "Select exactly one provider run and one positive record.", call. = FALSE
        )
        candidate <- bundle$candidate_intervals[
          bundle$candidate_intervals$provider_run_id == run_id &
            bundle$candidate_intervals$candidate_id == record_id &
            bundle$candidate_intervals$provider_decision == "positive",
          , drop = FALSE
        ]
        if (nrow(candidate) != 1L) stop(
          "The selected run/record pair is stale or non-positive.", call. = FALSE
        )
        adjudication <- rv$provider_adjudication
        if (is.null(adjudication)) {
          adjudication <- stpd_new_provider_adjudication(bundle)
        } else {
          stpd_validate_provider_adjudication(bundle, adjudication)
        }
        action <- as.character(input$provider_review_action %||% "")[[1L]]
        now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC")
        operation_id <- stpd_provider_hash_domain(
          "stpd-provider-ui-operation-v1",
          list(parent = adjudication$product_sha256, action = action,
               run = run_id, record = record_id, decided_utc = now)
        )
        request <- stpd_provider_review_request(
          adjudication, action, run_id, record_id, operation_id,
          reviewer_id = as.character(input$provider_reviewer_id %||% "")[[1L]],
          reason = as.character(input$provider_review_reason %||% "")[[1L]],
          decided_utc = now,
          adjusted_start_isi = if (action == "adjust_bounds")
            as.integer(input$provider_adjusted_start) else NULL,
          adjusted_end_isi = if (action == "adjust_bounds")
            as.integer(input$provider_adjusted_end) else NULL,
          target_decision_id = if (action == "void_prior_decision")
            as.character(input$provider_target_decision %||% "")[[1L]] else NULL
        )
        dataset <- NULL
        if (action == "adjust_bounds") {
          current_id <- as.character(rv$current_id %||% "")[[1L]]
          datasets <- rv$datasets %||% list()
          if (!nzchar(current_id) || is.null(datasets[[current_id]])) stop(
            "Boundary adjustment requires the exact source dataset loaded as the current dataset.",
            call. = FALSE
          )
          dataset <- datasets[[current_id]]
        }
        rv$provider_adjudication <- stpd_apply_provider_adjudication(
          bundle, adjudication, request, dataset = dataset
        )
        provider_clear_descendants(keep_reference = TRUE)
        updateRadioButtons(session, "provider_source_mode",
                           selected = "adjudicated")
        rv$provider_workbench_message <- list(
          level = "success",
          detail = ui_current_copy(
            paste0("\u5DF2\u8FFD\u52A0 ", action,
                   " \u88C1\u51B3\uFF1Bprovider AUTO \u5B57\u8282\u672A\u6539\u53D8\u3002\u8BF7\u91CD\u65B0\u751F\u6210\u590D\u6838\u89C6\u56FE\u3002"),
            paste0("Appended ", action,
                   "; provider AUTO bytes are unchanged. Rebuild the review view.")
          )
        )
      }, error = function(e) provider_notify_error(
        "\u8FFD\u52A0\u88C1\u51B3\u5931\u8D25\uFF1A", "Appending adjudication failed:", e
      ))
    })

    observeEvent(input$provider_build_view, {
      tryCatch({
        bundle <- rv$provider_bundle
        if (is.null(bundle)) stop("Import a provider bundle first.", call. = FALSE)
        run_id <- as.character(input$provider_selected_run %||% "")[[1L]]
        if (!nzchar(run_id)) stop("Select one provider run.", call. = FALSE)
        mode <- as.character(input$provider_source_mode %||% "auto")[[1L]]
        adjudication <- if (identical(mode, "adjudicated")) {
          if (is.null(rv$provider_adjudication)) stop(
            "Adjudicated mode requires the exact validated Phase C product.",
            call. = FALSE
          )
          rv$provider_adjudication
        } else NULL
        decision <- stpd_provider_composer_decision(
          bundle, run_id, mode, adjudication,
          scientific_owner = as.character(
            input$provider_scientific_owner %||% ""
          )[[1L]],
          rationale = as.character(
            input$provider_composition_rationale %||% ""
          )[[1L]],
          decided_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
        )
        composition <- stpd_compose_provider_science(
          bundle, decision, adjudication
        )
        view <- stpd_provider_review_view(
          bundle, composition, adjudication
        )
        rv$provider_composition <- composition
        rv$provider_review_view <- view
        rv$provider_score <- NULL
        rv$provider_workbench_message <- list(
          level = "success",
          detail = ui_current_copy(
            paste0("\u5DF2\u751F\u6210 ", mode, " \u590D\u6838\u89C6\u56FE\uFF1A",
                   nrow(view$selected_intervals), " \u4E2A\u9009\u4E2D\u533A\u95F4\u3002"),
            paste0("Built the ", mode, " review view with ",
                   nrow(view$selected_intervals), " selected interval(s).")
          )
        )
      }, error = function(e) provider_notify_error(
        "\u751F\u6210\u590D\u6838\u89C6\u56FE\u5931\u8D25\uFF1A", "Review-view construction failed:", e
      ))
    })

    observeEvent(input$provider_run_score, {
      tryCatch({
        if (is.null(rv$provider_review_view) ||
            is.null(rv$provider_composition)) stop(
          "Build a review view before scoring.", call. = FALSE
        )
        if (is.null(rv$provider_reference)) stop(
          "Import a validated reference before scoring.", call. = FALSE
        )
        adjudication <- if (
          identical(rv$provider_review_view$metadata$source_mode[[1L]],
                    "adjudicated")
        ) rv$provider_adjudication else NULL
        score <- stpd_score_provider_view(
          rv$provider_bundle, rv$provider_composition,
          rv$provider_review_view, rv$provider_reference,
          estimand = as.character(input$provider_score_estimand %||%
                                    "detector_performance")[[1L]],
          iou_threshold = as.numeric(input$provider_score_iou %||% 0.5),
          adjudication = adjudication
        )
        rv$provider_score <- score
        rv$provider_workbench_message <- list(
          level = "success",
          detail = ui_current_copy(
            paste0("\u8BC4\u4EF7\u5B8C\u6210\uFF1A", score$metadata$estimand[[1L]],
                   "\uFF1B\u672A\u5408\u5E76 provider / track / label\u3002"),
            paste0("Scoring complete: ", score$metadata$estimand[[1L]],
                   "; provider, track, and label were not pooled.")
          )
        )
      }, error = function(e) provider_notify_error(
        "\u89C4\u8303\u5316\u8BC4\u4EF7\u5931\u8D25\uFF1A", "Normalized scoring failed:", e
      ))
    })

    observeEvent(list(input$provider_score_estimand, input$provider_score_iou), {
      score <- isolate(rv$provider_score)
      if (is.null(score)) return()
      estimand <- as.character(input$provider_score_estimand %||% "")[[1L]]
      iou <- suppressWarnings(as.numeric(input$provider_score_iou %||% NA_real_))
      if (!identical(estimand, score$metadata$estimand[[1L]]) ||
          !isTRUE(all.equal(iou, score$metadata$iou_threshold[[1L]],
                           tolerance = 0))) {
        rv$provider_score <- NULL
        rv$provider_workbench_message <- list(
          level = "warning",
          detail = ui_current_copy(
            "Estimand \u6216 IoU \u5DF2\u6539\u53D8\uFF1B\u65E7\u8BC4\u5206\u5DF2\u5931\u6548\uFF0C\u8BF7\u91CD\u65B0\u8FD0\u884C\u3002",
            "The estimand or IoU changed; the prior score was invalidated. Run scoring again."
          )
        )
      }
    }, ignoreInit = TRUE)

    output$provider_workbench_status <- renderUI({
      view <- rv$provider_review_view
      message <- rv$provider_workbench_message
      if (is.null(view)) {
        detail <- if (is.list(message)) message$detail else ui_current_copy(
          "\u5C1A\u672A\u751F\u6210\u590D\u6838\u89C6\u56FE\u3002",
          "No provider review view has been built."
        )
        return(tags$div(class = "stpd-state-card", detail))
      }
      metadata <- view$metadata[1L, , drop = FALSE]
      tags$div(
        class = "stpd-state-card stpd-state-current",
        tags$b(paste0("Run: ", metadata$provider_run_id[[1L]])),
        tags$br(),
        paste0("Source: ", metadata$source_mode[[1L]],
               " | authority: ", metadata$authority_scope[[1L]],
               " | access: ", metadata$information_access[[1L]]),
        tags$br(),
        paste0("Estimand gate: ", metadata$estimand_notice[[1L]]),
        if (is.list(message)) tags$div(class = "small-note", message$detail)
      )
    })

    provider_render_table <- function(accessor) {
      DT::renderDT({
        table <- accessor()
        if (is.null(table) || !is.data.frame(table)) table <- data.frame()
        DT::datatable(
          table, rownames = FALSE, filter = "top",
          options = list(pageLength = 8, scrollX = TRUE, autoWidth = TRUE)
        )
      })
    }

    output$provider_catalog_table <- provider_render_table(function() {
      (rv$provider_review_view %||% list())$provider_catalog
    })
    output$provider_delta_table <- provider_render_table(function() {
      (rv$provider_review_view %||% list())$auto_adjudicated_delta
    })
    output$provider_selected_intervals_table <- provider_render_table(function() {
      (rv$provider_review_view %||% list())$selected_intervals
    })
    output$provider_relationships_table <- provider_render_table(function() {
      (rv$provider_review_view %||% list())$relationships
    })
    output$provider_regimes_table <- provider_render_table(function() {
      (rv$provider_review_view %||% list())$regimes
    })
    output$provider_conflicts_table <- provider_render_table(function() {
      (rv$provider_review_view %||% list())$conflicts
    })
    output$provider_score_metrics_table <- provider_render_table(function() {
      (rv$provider_score %||% list())$metrics_by_label
    })
    output$provider_score_coverage_table <- provider_render_table(function() {
      (rv$provider_score %||% list())$coverage_by_label
    })
    output$provider_score_fragmentation_table <- provider_render_table(function() {
      (rv$provider_score %||% list())$fragmentation
    })
    output$provider_score_matches_table <- provider_render_table(function() {
      (rv$provider_score %||% list())$matches
    })

    output$provider_workbench_out <- downloadHandler(
      filename = function() paste0(
        "stpd_provider_workbench_", format(Sys.time(), "%Y%m%d_%H%M%S"),
        ".zip"
      ),
      content = function(file) {
        if (is.null(rv$provider_review_view) ||
            is.null(rv$provider_composition)) stop(
          "Build and validate a provider review view before export.",
          call. = FALSE
        )
        adjudication <- if (
          identical(rv$provider_review_view$metadata$source_mode[[1L]],
                    "adjudicated")
        ) rv$provider_adjudication else NULL
        root <- tempfile("stpd-provider-ui-export-")
        if (!dir.create(root)) stop("Could not create export root.", call. = FALSE)
        on.exit(unlink(root, recursive = TRUE), add = TRUE)
        out_dir <- file.path(root, "provider_workbench")
        score <- rv$provider_score
        reference <- if (is.null(score)) NULL else rv$provider_reference
        stpd_write_provider_workbench(
          rv$provider_bundle, rv$provider_composition,
          rv$provider_review_view, out_dir,
          adjudication = adjudication, score = score, reference = reference
        )
        files <- list.files(out_dir, full.names = TRUE)
        status <- utils::zip(zipfile = file, files = files, flags = "-j")
        if (!identical(as.integer(status), 0L) || !file.exists(file)) stop(
          "Could not create provider workbench ZIP.", call. = FALSE
        )
      }
    )
  }, envir = server_env)
}

stpd_server_install_detection_module <- function(server_env) {
  evalq({
    detector_summary_bilingual <- function(zh, en) {
      structure(
        c(zh = as.character(zh %||% "")[1], en = as.character(en %||% "")[1]),
        class = c("stpd_ui_bilingual", "character")
      )
    }

    detector_summary_copy <- function(key, ...) {
      args <- list(...)
      detector_summary_bilingual(
        do.call(stpd_ui_copy, c(list(key = key, lang = "zh"), args)),
        do.call(stpd_ui_copy, c(list(key = key, lang = "en"), args))
      )
    }

    detector_summary_condition <- function(zh_prefix, en_prefix, e) {
      detector_summary_bilingual(
        paste(as.character(zh_prefix)[1], stpd_ui_condition_detail(e, lang = "zh")),
        paste(as.character(en_prefix)[1], stpd_ui_condition_detail(e, lang = "en"))
      )
    }

    detector_error_prefixes <- function(status = "detector", zh_prefix = NULL) {
      defaults <- switch(
        as.character(status %||% "detector")[1],
        batch = c(
          zh = "\u6279\u5904\u7406\u5931\u8D25\u3002",
          en = "Batch processing failed."
        ),
        parameter_sensitivity = c(
          zh = "\u53C2\u6570\u654F\u611F\u6027\u626B\u63CF\u5931\u8D25\u3002",
          en = "Parameter-sensitivity scan failed."
        ),
        near_miss = c(
          zh = "Near-miss \u9608\u503C\u5E94\u7528\u540E\u7684\u68C0\u6D4B\u5668\u91CD\u8DD1\u5931\u8D25\u3002",
          en = "Detector rerun after near-miss threshold application failed."
        ),
        c(
          zh = "\u68C0\u6D4B\u5668\u8FD0\u884C\u5931\u8D25\u3002",
          en = "Detector execution failed."
        )
      )
      supplied <- trimws(as.character(zh_prefix %||% "")[1])
      if (!is.na(supplied) && nzchar(supplied)) {
        if (!grepl("[\u3002\uFF01\uFF1F.!?]$", supplied)) supplied <- paste0(supplied, "\u3002")
        defaults[["zh"]] <- supplied
      }
      defaults
    }

    detector_pre_qc_summary <- function(qc, qc_msg) {
      bad <- if (!is.null(qc) && nrow(qc) > 0L && "warning_level" %in% names(qc)) {
        qc[as.character(qc$warning_level) == "error", , drop = FALSE]
      } else {
        data.frame()
      }
      raw_messages <- if (nrow(bad) > 0L && "warning_message" %in% names(bad)) {
        as.character(bad$warning_message %||% "")
      } else {
        as.character(qc_msg %||% "")
      }
      raw_messages[is.na(raw_messages)] <- ""
      raw_joined <- paste(raw_messages, collapse = " | ")
      any_code <- function(pattern) {
        any(grepl(pattern, raw_messages, ignore.case = TRUE, perl = TRUE), na.rm = TRUE)
      }
      any_numeric <- function(column, predicate) {
        if (!(column %in% names(bad))) return(FALSE)
        values <- suppressWarnings(as.numeric(bad[[column]]))
        any(is.finite(values) & predicate(values), na.rm = TRUE)
      }
      any_logical <- function(column) {
        if (!(column %in% names(bad))) return(FALSE)
        any(as.logical(bad[[column]]), na.rm = TRUE)
      }

      empty_train <- any_code("(^|[;|[:space:]])empty train($|[;|[:space:]])") ||
        any_numeric("n_spikes", function(x) x <= 0)
      invalid_duration <- any_code("invalid_duration") ||
        ("duration_sec" %in% names(bad) && any(
          !is.finite(suppressWarnings(as.numeric(bad$duration_sec))) |
            suppressWarnings(as.numeric(bad$duration_sec)) <= 0,
          na.rm = TRUE
        ))
      duplicate_timestamp <- any_code("duplicate[_ ]timestamps?") ||
        any_numeric("n_duplicate_timestamps", function(x) x > 0)
      nonpositive_interval <- any_code("zero_or_negative_(ISI|timestamp_steps)") ||
        any_numeric("n_zero_or_negative_ISI", function(x) x > 0) ||
        any_numeric("n_zero_or_negative_timestamp_steps", function(x) x > 0)
      isi_mismatch <- any_code("timestamp_ISI_mismatch") ||
        any_logical("timestamp_ISI_mismatch")

      zh_issues <- character()
      en_issues <- character()
      add_issue <- function(flag, zh, en) {
        if (isTRUE(flag)) {
          zh_issues <<- c(zh_issues, zh)
          en_issues <<- c(en_issues, en)
        }
      }
      add_issue(empty_train,
                "\u5B58\u5728\u6CA1\u6709\u53EF\u68C0\u6D4B spike \u7684\u7A7A train",
                "one or more trains contain no detectable spikes")
      add_issue(invalid_duration,
                "\u8BB0\u5F55\u65F6\u957F\u65E0\u6548\u6216\u4E0D\u5927\u4E8E 0",
                "recording duration is invalid or not greater than zero")
      add_issue(duplicate_timestamp,
                "\u5B58\u5728\u5B8C\u5168\u91CD\u590D\u7684 timestamp",
                "exact duplicate timestamps are present")
      add_issue(nonpositive_interval,
                "\u5B58\u5728 0/\u8D1F ISI \u6216\u975E\u9012\u589E timestamp",
                "zero/negative ISIs or non-increasing timestamps are present")
      add_issue(isi_mismatch,
                "\u6587\u4EF6 ISI \u4E0E timestamp \u63A8\u5BFC\u7684 ISI \u4E0D\u4E00\u81F4",
                "file-provided ISIs disagree with timestamp-derived ISIs")
      if (length(zh_issues) == 0L) {
        zh_issues <- "QC \u62A5\u544A\u4E86\u65E0\u6CD5\u5B89\u5168\u7EE7\u7EED\u68C0\u6D4B\u7684\u6570\u636E\u5B8C\u6574\u6027\u95EE\u9898"
        en_issues <- "QC reported a data-integrity problem that prevents safe detection"
      }
      affected_n <- if (nrow(bad) > 0L) nrow(bad) else 1L
      affected <- if (nrow(bad) > 0L && "train" %in% names(bad)) {
        unique(trimws(as.character(bad$train)))
      } else {
        character()
      }
      affected <- affected[!is.na(affected) & nzchar(affected)]
      affected_zh <- if (length(affected) > 0L) {
        paste0("\u53D7\u5F71\u54CD train\uFF1A", paste(utils::head(affected, 5L), collapse = ", "), "\u3002")
      } else {
        ""
      }
      affected_en <- if (length(affected) > 0L) {
        paste0("Affected train(s): ", paste(utils::head(affected, 5L), collapse = ", "), ".")
      } else {
        ""
      }
      raw_detail_text <- as.character(qc_msg %||% "")[1]
      if (is.na(raw_detail_text) || !nzchar(raw_detail_text)) raw_detail_text <- raw_joined
      raw_detail <- simpleError(raw_detail_text)
      detector_summary_bilingual(
        paste(
          paste0("\u68C0\u6D4B\u524D QC \u53D1\u73B0 ", affected_n, " \u6761 train \u5B58\u5728\u6570\u636E\u5B8C\u6574\u6027\u9519\u8BEF\uFF0C\u5DF2\u505C\u6B62\u68C0\u6D4B\u3002"),
          paste0("\u5DF2\u8BC6\u522B\u95EE\u9898\uFF1A", paste(zh_issues, collapse = "\uFF1B"), "\u3002"),
          affected_zh,
          "\u8BF7\u5728\u201C\u6570\u636E QC\u201D\u4E2D\u68C0\u67E5\u76F8\u5173 train\uFF0C\u4FEE\u6B63\u540E\u91CD\u65B0\u8FD0\u884C\u3002",
          stpd_ui_condition_detail(raw_detail, lang = "zh"),
          sep = "\n"
        ),
        paste(
          paste0("Pre-detection QC found data-integrity errors in ", affected_n, " train(s); detection was stopped."),
          paste0("Identified issue(s): ", paste(en_issues, collapse = "; "), "."),
          affected_en,
          "Review the affected train(s) on the Data QC tab, correct the data, and rerun detection.",
          stpd_ui_condition_detail(raw_detail, lang = "en"),
          sep = "\n"
        )
      )
    }

    detector_parameter_issue_summary <- function(issues) {
      bad <- utils::head(issues, 3L)
      zh_bad <- stpd_ui_localize_parameter_issues(bad, lang = "zh")
      en_bad <- stpd_ui_localize_parameter_issues(bad, lang = "en")
      raw_issue <- as.character(bad$issue %||% "")
      zh_issue <- as.character(zh_bad$issue %||% "")
      en_issue <- as.character(en_bad$issue %||% "")
      known <- !is.na(raw_issue) & nzchar(trimws(raw_issue)) &
        !is.na(zh_issue) & (zh_issue != raw_issue)
      known[is.na(known)] <- FALSE
      zh_issue[!known] <- "\u672A\u8BC6\u522B\u7684\u53C2\u6570 contract \u95EE\u9898\uFF08\u89C1\u6280\u672F\u8BE6\u60C5\uFF09"
      en_issue[!known] <- "Unrecognized parameter-contract issue (see technical details)"
      paths <- as.character(bad$path %||% "")
      zh_rows <- paste(paths, zh_issue, sep = " - ")
      en_rows <- paste(paths, en_issue, sep = " - ")
      zh <- paste0(
        "\u53C2\u6570 contract \u9A8C\u8BC1\u5931\u8D25\uFF0C\u5DF2\u963B\u6B62\u8FD0\u884C\u3002\n",
        "\u5DF2\u8BC6\u522B\u95EE\u9898\uFF1A", paste(zh_rows, collapse = "\uFF1B")
      )
      en <- paste0(
        "Parameter-contract validation failed; execution was blocked.\n",
        "Identified issue(s): ", paste(en_rows, collapse = "; ")
      )
      if (any(!known)) {
        unknown_detail <- simpleError(paste(
          paste(paths[!known], raw_issue[!known], sep = " - "),
          collapse = "; "
        ))
        zh <- paste(zh, stpd_ui_condition_detail(unknown_detail, lang = "zh"), sep = "\n")
        en <- paste(en, stpd_ui_condition_detail(unknown_detail, lang = "en"), sep = "\n")
      }
      detector_summary_bilingual(zh, en)
    }

    detector_summary_current <- function(value = rv$last_detector_summary, lang = ui_language()) {
      lang <- if (identical(as.character(lang %||% "zh")[1], "en")) "en" else "zh"
      if (inherits(value, "stpd_ui_bilingual") ||
          (is.character(value) && all(c("zh", "en") %in% names(value)))) {
        return(unname(as.character(value[[lang]] %||% "")[1]))
      }
      text <- as.character(value %||% "")[1]
      if (identical(lang, "en") && exists("stpd_i18n_translate_text", mode = "function")) {
        text <- stpd_i18n_translate_text(text, "en")
      }
      text
    }

    detector_summary_set <- function(value) {
      rv$last_detector_summary <- value
      invisible(value)
    }

    detector_summary_counts <- function(before, after, scope, time = Sys.time()) {
      pats <- union(names(before), names(after))
      before <- before[pats]
      after <- after[pats]
      before[is.na(before)] <- 0L
      after[is.na(after)] <- 0L
      render_one <- function(lang) {
        lines <- c(
          stpd_ui_copy("detector_last_run", lang = lang, time = format(time, "%Y-%m-%d %H:%M:%S")),
          stpd_ui_copy("scope_label", lang = lang, scope = unname(scope[[lang]])),
          stpd_ui_copy("event_count_change", lang = lang)
        )
        for (pat in pats) {
          lines <- c(lines, sprintf("  %-15s %6d -> %6d  (%+d)", pat, before[pat], after[pat], after[pat] - before[pat]))
        }
        paste(lines, collapse = "\n")
      }
      detector_summary_bilingual(render_one("zh"), render_one("en"))
    }

    detector_notify_error <- function(e, prefix = "\u68C0\u6D4B\u5668\u8FD0\u884C\u5931\u8D25", status = c("detector", "batch", "parameter_sensitivity", "near_miss")) {
      status <- match.arg(status)
      prefixes <- detector_error_prefixes(status, prefix)
      summary_value <- detector_summary_condition(
        prefixes[["zh"]],
        prefixes[["en"]],
        e
      )
      msg <- detector_summary_current(summary_value)
      if (identical(status, "batch")) {
        rv$batch_status <- summary_value
      } else if (identical(status, "parameter_sensitivity")) {
        rv$parameter_sensitivity_status <- summary_value
      } else if (identical(status, "near_miss")) {
        rv$near_miss_rerun_summary <- summary_value
      } else {
	        detector_summary_set(summary_value)
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
	      summary_value <- detector_parameter_issue_summary(bad)
	      msg <- detector_summary_current(summary_value)
	      showNotification(msg, type = "error", duration = 10)
	      detector_summary_set(summary_value)
	      return(invisible(NULL))
	    }
	    p <- effective_params_for_detector(p)
	    override_target <- !is.null(target_trains_override)
	    selected_only <- isTRUE(input$detector_selected_only)
	    target_trains <- names(ds$trains)
      scope_value <- NULL
      scope_txt <- NULL
      if (isTRUE(override_target)) {
        target_trains <- intersect(as.character(target_trains_override), names(ds$trains))
        selected_only <- TRUE
        if (length(target_trains) == 0) {
          summary_value <- detector_summary_copy("no_target_trains")
          msg <- detector_summary_current(summary_value)
          detector_summary_set(summary_value)
          showNotification(msg, type = "error", duration = 8)
          return(invisible(NULL))
        }
        scope_value <- detector_summary_copy("detector_scope_specified", n = length(target_trains))
        scope_txt <- detector_summary_current(scope_value)
      } else if (selected_only) {
		      target_trains <- intersect(displayed_train_names() %||% character(0), names(ds$trains))
		      if (length(target_trains) == 0) {
		        summary_value <- detector_summary_copy("no_selected_trains")
		        msg <- detector_summary_current(summary_value)
	        detector_summary_set(summary_value)
        showNotification(msg, type = "error", duration = 8)
        return(invisible(NULL))
      }
    }
    pre_qc <- tryCatch(
      stpd_product_pre_detection_qc(ds, p, target_trains),
      error = function(e) data.frame(warning_level = "error", train = "", warning_message = conditionMessage(e), stringsAsFactors = FALSE)
    )
    qc_msg <- stpd_product_qc_error_message(pre_qc)
    if (nzchar(qc_msg)) {
      if (isTRUE(stpd_qc_errors_are_exact_duplicate_only(pre_qc))) {
        zh_msg <- paste0(
          "\u68C0\u6D4B\u524D QC \u53D1\u73B0\u5B8C\u5168\u91CD\u590D timestamp\u3002",
          "\u4E3A\u907F\u514D\u672A\u7ECF\u786E\u8BA4\u5C31\u5220\u9664 spike\uFF0C\u672C\u6B21\u68C0\u6D4B\u5DF2\u505C\u6B62\uFF1B",
          "\u8BF7\u5728\u201C\u6570\u636E QC\u201D\u4E2D\u4F7F\u7528\u201C\u5408\u5E76\u5F53\u524D\u6570\u636E\u96C6\u4E2D\u7684\u91CD\u590D timestamp\u201D\uFF0C\u786E\u8BA4\u540E\u518D\u91CD\u65B0\u8FD0\u884C\u3002"
        )
	        en_msg <- paste0(
	          "Pre-detection QC found exact duplicate timestamps. Detection stopped to avoid deleting spikes without confirmation. ",
	          "Use 'merge duplicate timestamps in the current dataset' on the Data QC tab, confirm the change, and rerun detection."
	        )
	        summary_value <- detector_summary_bilingual(zh_msg, en_msg)
	        msg <- detector_summary_current(summary_value)
	        detector_summary_set(summary_value)
        showNotification(msg, type = "error", duration = 15)
        updateTabsetPanel(session, "main_tabs", selected = "\u6570\u636E QC")
        return(invisible(NULL))
      } else {
	        summary_value <- detector_pre_qc_summary(pre_qc, qc_msg)
	        msg <- detector_summary_current(summary_value)
	        detector_summary_set(summary_value)
        showNotification(msg, type = "error", duration = 15)
        updateTabsetPanel(session, "main_tabs", selected = "\u6570\u636E QC")
        return(invisible(NULL))
      }
    }
		    before_counts <- detector_event_counts(ds, target_trains = if (selected_only) target_trains else NULL)
		    run_error <- NULL
		    if (isTRUE(getOption("stpd.debug.detector_progress", FALSE)) &&
		        !is.null(message) && nzchar(as.character(message)[1])) {
		      base::message("[detector progress] requested heading: ", as.character(message)[1])
		    }
		    ds <- tryCatch(
		      withProgress(message = ui_text("detector_progress_default"), value = 0, {
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
		          if (isTRUE(getOption("stpd.debug.detector_progress", FALSE)) &&
		              !is.null(detail) && nzchar(as.character(detail)[1])) {
		            base::message("[detector progress] ", phase, ": ", as.character(detail)[1])
		          }
		          detail_txt <- switch(
		            phase,
		            prepare = ui_text("detector_progress_prepare"),
		            thresholds = ui_text("detector_progress_thresholds"),
		            train_start = ui_text("detector_progress_train_start", index = idx, total = total_n, train = train_txt),
		            train_done = ui_text("detector_progress_train_done", index = idx, total = total_n, train = train_txt),
		            assemble_events = ui_text("detector_progress_assemble_events"),
		            diagnostics = ui_text("detector_progress_diagnostics"),
		            ledger = ui_text("detector_progress_ledger"),
		            features = ui_text("detector_progress_features"),
		            final_audits = ui_text("detector_progress_final_audits"),
		            report_tables = ui_text("detector_progress_report_tables"),
		            public_ledgers = ui_text("detector_progress_public_ledgers"),
		            public_features = ui_text("detector_progress_public_features"),
		            public_final = ui_text("detector_progress_public_final"),
		            distributional_evidence = ui_text("detector_progress_distributional_evidence"),
		            public_reports = ui_text("detector_progress_public_reports"),
		            public_complete = ui_text("detector_progress_public_complete"),
		            complete = ui_text("detector_progress_complete"),
		            ui_text("detector_progress_default")
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
	        legacy_promote_possible <-
	          stpd_legacy_final_audit_promote_from_policy(ds, audit_policy)
	        out_ds <- stpd_apply_final_audit(
	          out_ds,
	          selected_trains = target_trains,
	          promote_possible = legacy_promote_possible,
	          min_isi_sec = min_valid_isi_sec(),
	          reason = "detector_run_sync_final_audit",
	          user = Sys.info()[["user"]] %||% NA_character_
	        )$dataset
		        setProgress(value = 1, detail = ui_text("detector_rebuild_complete"))
	        out_ds
	      }),
	      error = function(e) {
	        run_error <<- e
	        ds
	      }
	    )
	    if (!is.null(run_error)) {
	      summary_value <- detector_summary_condition(
	        "\u68C0\u6D4B\u5668\u8FD0\u884C\u5931\u8D25\u3002",
	        "Detector execution failed.",
	        run_error
	      )
	      msg <- detector_summary_current(summary_value)
	      detector_summary_set(summary_value)
	      showNotification(msg, type = "error", duration = 15)
	      if (stpd_error_mentions_qc(run_error) || grepl("Pre-detection QC|data-integrity|duplicate|artifact|ISI", conditionMessage(run_error), ignore.case = TRUE)) {
	        updateTabsetPanel(session, "main_tabs", selected = "\u6570\u636E QC")
	      }
	      return(invisible(NULL))
	    }
    set_dataset(rv$current_id, ds)
    run_identities <- rv$ui_run_identity_by_dataset %||% list()
    run_identities[[rv$current_id]] <- stpd_ui_run_identity(
      ds, params = p, dataset_id = rv$current_id,
      selected_trains = target_trains
    )
    rv$ui_run_identity_by_dataset <- run_identities
    if (isTRUE(switch_to_plot)) select_default_timestamp_tab(ds)
    after_counts <- detector_event_counts(ds, target_trains = if (selected_only) target_trains else NULL)
	    if (is.null(scope_value)) {
	      scope_value <- if (selected_only) {
	        detector_summary_copy("detector_scope_selected", n = length(target_trains))
	      } else {
	        detector_summary_copy("detector_scope_all", n = length(target_trains))
	      }
	    }
	    scope_txt <- detector_summary_current(scope_value)
	    detector_summary_set(detector_summary_counts(before_counts, after_counts, scope_value))
    if (isTRUE(notify)) {
      showNotification(ui_text(
                         "detector_complete",
                         scope = scope_txt,
                         burst = after_counts["burst"],
                         long_burst = after_counts["long_burst"],
                         possible_burst = after_counts["possible_burst"],
                         pause = after_counts["pause"],
                         tonic = after_counts["tonic"],
                         hf_tonic = after_counts["high_frequency_tonic"],
                         hf_spiking = after_counts["high_frequency_spiking"]
                       ),
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
  }, ignoreInit = TRUE)


  }, envir = server_env)
}

stpd_server_install_export_module <- function(server_env) {
  evalq({
  # Standalone helper tests install this module without the full Shiny server.
  # Production always supplies these three closures from R/19_server.R.
  if (!exists("ui_language", mode = "function", inherits = TRUE)) {
    ui_language <- function() "en"
  }
  if (!exists("ui_text", mode = "function", inherits = TRUE)) {
    ui_text <- function(key, ...) stpd_ui_copy(key, lang = ui_language(), ...)
  }
  if (!exists("ui_condition_detail", mode = "function", inherits = TRUE)) {
    ui_condition_detail <- function(e) stpd_ui_condition_detail(e, lang = ui_language())
  }
  if (exists("detector_summary_current", mode = "function", inherits = TRUE)) {
    output$detector_before_after_summary <- renderText({
      detector_summary_current(rv$last_detector_summary, lang = ui_language())
    })
  }

  # ----------------------------------------------------------
  # Export functions
  # ----------------------------------------------------------
  formal_export_timestamp <- function() {
    format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
  }

  formal_export_progress_begin <- function(kind, ds) {
    current_values <- isolate(list(
      sequence_id = rv$formal_export_sequence %||% 0L,
      dataset_id = rv$current_id %||% ""
    ))
    sequence_id <- suppressWarnings(as.integer(current_values$sequence_id))
    if (length(sequence_id) != 1L || is.na(sequence_id) || sequence_id < 0L) {
      sequence_id <- 0L
    }
    sequence_id <- sequence_id + 1L
    rv$formal_export_sequence <- sequence_id
    export_id <- paste0(
      "formal_export_", Sys.getpid(), "_",
      format(Sys.time(), "%Y%m%dT%H%M%OS6", tz = "UTC"), "_",
      sequence_id
    )
    started_at <- formal_export_timestamp()
    rv$formal_export_progress <- list(
      export_id = export_id,
      kind = as.character(kind)[1],
      state = "preparing",
      dataset_id = as.character(current_values$dataset_id)[1],
      dataset_name = as.character(ds$meta$display_name %||% "")[1],
      run_id = "",
      params_sha256 = "",
      scope = "?/?",
      selected_train_n = NA_integer_,
      total_train_n = NA_integer_,
      selected_trains = character(),
      phase = "validate",
      progress = 0,
      started_at = started_at,
      updated_at = started_at,
      completed_at = "",
      detail = "",
      detail_i18n = c(zh = "", en = ""),
      file_size_bytes = NA_real_,
      error_message = ""
    )
    export_id
  }

  formal_export_progress_bind_context <- function(export_id, export_context) {
    current <- isolate(rv$formal_export_progress)
    if (!is.list(current) ||
        !identical(as.character(current$export_id %||% "")[1], export_id)) {
      return(invisible(FALSE))
    }
    if (as.character(current$state %||% "preparing")[1] %in%
        c("generated", "error")) {
      return(invisible(FALSE))
    }
    state <- export_context$state %||% list()
    scope_n <- suppressWarnings(as.integer(state$selected_train_n %||% NA_integer_))
    scope_total <- suppressWarnings(as.integer(state$total_train_n %||% NA_integer_))
    current$run_id <- as.character(state$run_id %||% "")[1]
    current$params_sha256 <- as.character(state$params_sha256 %||% "")[1]
    current$selected_train_n <- scope_n[1]
    current$total_train_n <- scope_total[1]
    current$selected_trains <- as.character(state$selected_trains %||% character())
    current$scope <- if (length(scope_n) == 1L && length(scope_total) == 1L &&
        !is.na(scope_n) && !is.na(scope_total)) {
      paste0(scope_n, "/", scope_total)
    } else {
      "?/?"
    }
    current$updated_at <- formal_export_timestamp()
    rv$formal_export_progress <- current
    invisible(TRUE)
  }

  formal_export_progress_update <- function(
      export_id, phase, progress = NULL, detail = NULL,
      state = "preparing", file_size_bytes = NULL,
      error_message = NULL, detail_i18n = NULL) {
    current <- isolate(rv$formal_export_progress)
    if (!is.list(current) ||
        !identical(as.character(current$export_id %||% "")[1], export_id)) {
      # A newer download has already become authoritative for this session.
      return(invisible(FALSE))
    }
    terminal_states <- c("generated", "error")
    current_state <- as.character(current$state %||% "preparing")[1]
    next_state <- as.character(state %||% "preparing")[1]
    if (current_state %in% terminal_states) {
      return(invisible(FALSE))
    }
    if (!is.null(progress)) {
      value <- suppressWarnings(as.numeric(progress)[1])
      if (is.finite(value)) {
        prior <- suppressWarnings(as.numeric(current$progress %||% 0)[1])
        if (!is.finite(prior)) prior <- 0
        current$progress <- max(
          prior,
          max(0, min(1, value))
        )
      }
    }
    current$phase <- as.character(phase %||% current$phase)[1]
    current$state <- next_state
    if (!is.null(detail)) current$detail <- as.character(detail)[1]
    if (!is.null(detail_i18n)) {
      detail_i18n_names <- names(detail_i18n)
      detail_i18n <- as.character(detail_i18n)
      names(detail_i18n) <- detail_i18n_names
      if (all(c("zh", "en") %in% names(detail_i18n))) {
        current$detail_i18n <- c(
          zh = unname(detail_i18n[["zh"]])[1],
          en = unname(detail_i18n[["en"]])[1]
        )
      }
    } else if (!is.null(detail)) {
      current$detail_i18n <- c(zh = as.character(detail)[1], en = as.character(detail)[1])
    }
    if (!is.null(file_size_bytes)) {
      current$file_size_bytes <- suppressWarnings(as.numeric(file_size_bytes)[1])
    }
    if (!is.null(error_message)) {
      current$error_message <- as.character(error_message)[1]
    }
    current$updated_at <- formal_export_timestamp()
    if (next_state %in% terminal_states) {
      current$completed_at <- current$updated_at
    }
    rv$formal_export_progress <- current
    invisible(TRUE)
  }

  formal_export_copy_pair <- function(expr, call_env = parent.frame()) {
    expr <- force(expr)
    render_one <- function(lang) {
      copy_env <- new.env(parent = call_env)
      copy_env$ui_text <- function(key, ...) {
        stpd_ui_copy(key, lang = lang, ...)
      }
      copy_env$formal_export_inline_text <- function(zh, en) {
        if (identical(lang, "en")) en else zh
      }
      copy_env$ui_condition_detail <- function(e) {
        stpd_ui_condition_detail(e, lang = lang)
      }
      out <- tryCatch(eval(expr, envir = copy_env), error = function(e) "")
      as.character(out %||% "")[1]
    }
    c(zh = render_one("zh"), en = render_one("en"))
  }

  formal_export_progress_phase <- function(export_id, phase, progress, detail) {
    detail_i18n <- formal_export_copy_pair(substitute(detail), parent.frame())
    lang <- if (identical(ui_language(), "en")) "en" else "zh"
    detail_current <- unname(detail_i18n[[lang]])
    formal_export_progress_update(
      export_id, phase = phase, progress = progress, detail = detail_current,
      state = "preparing", detail_i18n = detail_i18n
    )
    setProgress(value = progress, detail = detail_current)
    invisible(NULL)
  }

  formal_export_inline_text <- function(zh, en) {
    if (identical(ui_language(), "en")) en else zh
  }

  formal_export_generated_file_size <- function(file) {
    info <- suppressWarnings(file.info(file))
    size <- if (nrow(info) == 1L) suppressWarnings(as.numeric(info$size[1])) else NA_real_
    if (!file.exists(file) || !is.finite(size) || size <= 0) {
      stop(ui_text("formal_export_empty"), call. = FALSE)
    }
    size
  }

  formal_export_notify_error <- function(export_id, kind_label, error) {
    message <- conditionMessage(error)
    detail <- ui_condition_detail(error)
    detail_i18n <- c(
      zh = stpd_ui_condition_detail(error, lang = "zh"),
      en = stpd_ui_condition_detail(error, lang = "en")
    )
    updated <- formal_export_progress_update(
      export_id, phase = "error", detail = detail, state = "error",
      error_message = message, detail_i18n = detail_i18n
    )
    if (isTRUE(updated)) {
      try(
        showNotification(
          paste0(
            kind_label,
            formal_export_inline_text("\u751F\u6210\u5931\u8D25\u3002", " generation failed. "),
            detail
          ),
          type = "error", duration = 15
        ),
        silent = TRUE
      )
    }
    invisible(updated)
  }

  formal_export_notify_generated <- function(export_id, kind_label, file) {
    size <- formal_export_generated_file_size(file)
    generated_detail_i18n <- c(
      zh = paste0("\u670D\u52A1\u5668\u5DF2\u751F\u6210\u6587\u4EF6\uFF08", size, " bytes\uFF09\u3002"),
      en = paste0("Server-side file generated (", size, " bytes).")
    )
    progress_record <- isolate(rv$formal_export_progress)
    if (is.list(progress_record) &&
        identical(as.character(progress_record$export_id %||% "")[1],
                  export_id) &&
        identical(as.character(progress_record$kind %||% "")[1],
                  "results_zip")) {
      archive_state <- stpd_ui_validate_zip_archive(
        file,
        required_files = c(
          "Detector_run_metadata.csv", "Export_run_metadata.csv",
          "ISI_labels.csv", "Validation_export_status.csv"
        ),
        sentinel_files = c(
          "Detector_run_metadata.csv", "Export_run_metadata.csv",
          "ISI_labels.csv", "Validation_export_status.csv"
        )
      )
      if (!isTRUE(archive_state$valid)) {
        stop(
          ui_text(
            "formal_zip_failed",
            code = archive_state$code,
            detail = archive_state$reason
          ),
          call. = FALSE
        )
      }
    }
    updated <- formal_export_progress_update(
      export_id, phase = "generated", progress = 1,
      detail = unname(generated_detail_i18n[[if (identical(ui_language(), "en")) "en" else "zh"]]),
      state = "generated", file_size_bytes = size, error_message = "",
      detail_i18n = generated_detail_i18n
    )
    if (isTRUE(updated)) {
      try(
        showNotification(
          paste0(
            kind_label,
            formal_export_inline_text(
              "\u5DF2\u5728\u670D\u52A1\u5668\u7AEF\u751F\u6210\uFF1B\u6D4F\u89C8\u5668\u5C06\u7EE7\u7EED\u5904\u7406\u4E0B\u8F7D\u3002",
              " has been generated on the server; the browser will continue handling the download."
            )
          ),
          type = "message", duration = 10
        ),
        silent = TRUE
      )
    }
    invisible(updated)
  }

  formal_results_export_context <- function(ds = current_dataset()) {
    id <- rv$current_id
    p <- current_ui_params()
    dataset_identity <- stpd_ui_dataset_identity(ds, id)
    run_identity <- ui_run_identity_for(ds, id, dataset_identity)
    state <- stpd_ui_formal_export_state(
      ds, params = p, dataset_id = id,
      run_identity = run_identity,
      dataset_identity = dataset_identity
    )
    validate(need(
      isTRUE(state$eligible),
      ui_text("formal_export_blocked_detail", code = as.character(state$code %||% "formal_export_blocked")[1])
    ))
    list(
      params = p, state = state,
      dataset_identity = dataset_identity,
      run_identity = run_identity
    )
  }

  output$download_labeled_csv <- downloadHandler(
    filename = function() {
      ds <- current_dataset()
      nm <- gsub("[^A-Za-z0-9_\\-\\.]", "_", ds$meta$display_name)
      paste0(nm, "_labeled_wide_", input$time_unit, ".csv")
    },
    content = function(file) {
      ds <- current_dataset()
      export_id <- formal_export_progress_begin("labeled_csv", ds)
      tryCatch({
        withProgress(
          message = paste(ui_text("formal_labeled_csv"), ui_text("validating_formal_export")),
          value = 0,
          {
            formal_export_progress_phase(
              export_id, "validate", 0.03,
              ui_text("validating_current_run")
            )
            export_context <- formal_results_export_context(ds)
            formal_export_progress_bind_context(export_id, export_context)
            td <- ds$trains
            u <- input$time_unit
            max_len <- max(sapply(td, nrow))
            out_df <- NULL

            formal_export_progress_phase(
              export_id, "assemble_labels", 0.12,
              ui_text("preparing_labeled_trains", n = length(td))
            )
            for (i in seq_along(td)) {
              tr <- names(td)[i]
              dat <- td[[tr]]
              n <- nrow(dat)
              final <- compute_final_pattern(dat$pattern_manual, dat$pattern_auto, dat$ISI_sec,
                                             auto_others = FALSE,
                                             min_isi_sec = min_valid_isi_sec())
              audit_final <- stpd_audit_final_labels(dat,
                                                     min_isi_sec = min_valid_isi_sec(),
                                                     auto_others = FALSE,
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
              formal_export_progress_phase(
                export_id, "assemble_labels", 0.12 + 0.70 * i / length(td),
                formal_export_inline_text(
                  paste0("\u5DF2\u51C6\u5907 train ", i, "/", length(td), "\uFF1A", tr),
                  paste0("Prepared train ", i, "/", length(td), ": ", tr)
                )
              )
	            }
            formal_export_progress_phase(
              export_id, "write_csv", 0.88,
              formal_export_inline_text(
                "\u6B63\u5728\u79FB\u9664\u7A7A\u5217\u5E76\u5199\u5165 UTF-8 CSV\u3002",
                "Removing empty columns and writing UTF-8 CSV."
              )
            )
	          out_df <- stpd_drop_empty_columns(out_df)
	          write_csv_safe(out_df, file, row.names = FALSE, fileEncoding = "UTF-8")
            formal_export_progress_phase(
              export_id, "verify", 0.97,
              formal_export_inline_text("\u6B63\u5728\u9A8C\u8BC1\u5DF2\u751F\u6210\u7684 CSV \u6587\u4EF6\u3002", "Verifying the generated CSV file.")
            )
          }
        )
        formal_export_notify_generated(export_id, ui_text("formal_labeled_csv"), file)
      }, error = function(e) {
        formal_export_notify_error(export_id, ui_text("formal_labeled_csv"), e)
        stop(e)
      })
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
      export_id <- formal_export_progress_begin("results_zip", ds)
      tryCatch({
        withProgress(
          message = paste(ui_text("formal_results_zip"), ui_text("validating_formal_export")),
          value = 0,
          {
            formal_export_progress_phase(
              export_id, "validate", 0.03,
              ui_text("validating_current_run")
            )
            export_context <- formal_results_export_context(ds)
            formal_export_progress_bind_context(export_id, export_context)
            p <- export_context$params
            export_state <- export_context$state
            validation <- (ds$results %||% list())[["scientific_validation"]] %||%
              NULL
            validation_decision <- stpd_ui_validation_export_decision(
              ds = ds, validation = validation, params = p,
              dataset_id = rv$current_id,
              run_identity = export_context$run_identity,
              validation_identity =
                (validation %||% list())[["ui_identity"]] %||% NULL,
              dataset_identity = export_context$dataset_identity,
              stale_policy = "omit"
            )
            ds_for_export <- ds
            if (!isTRUE(validation_decision$include) &&
                is.list(ds_for_export$results)) {
              ds_for_export$results[["scientific_validation"]] <- NULL
            }
            td <- ds$trains
            u <- input$time_unit
            formal_export_progress_phase(
              export_id, "derive_intervals", 0.09,
              formal_export_inline_text(
                "\u6B63\u5728\u751F\u6210\u6700\u7EC8\u5BA1\u8BA1\u4E8B\u4EF6\u8868\u548C ISI \u6807\u7B7E\u8868\u3002",
                "Deriving audit-final event and ISI label tables."
              )
            )
            bundle <- derive_interval_tables(
              td,
              source = "audit_final",
              auto_others = FALSE,
              dataset_map = setNames(rep(ds$meta$display_name, length(td)), names(td)),
              min_isi_sec = min_valid_isi_sec(),
              contrast_q = p$burst$contrast_q %||% 0.90,
              context_k = p$burst$context_k %||% 5L
            )
            ev <- bundle$events
            # pause event exports include local/global threshold context when available.
            run_id_export <- export_state$run_id
            phash_export <- export_state$params_sha256
            ev <- enrich_events_with_pause_thresholds(ev, td, run_id = run_id_export, params_hash = phash_export)
            lab <- bundle$labels
            formal_export_progress_phase(
              export_id, "derive_intervals", 0.18,
              formal_export_inline_text(
                paste0("\u5DF2\u751F\u6210 ", nrow(ev), " \u884C\u4E8B\u4EF6\u548C ", nrow(lab), " \u884C ISI \u6807\u7B7E\u3002"),
                paste0("Derived ", nrow(ev), " event row(s) and ", nrow(lab), " ISI label row(s).")
              )
            )
      
	      # Use a unique directory for every download. A seconds-only name could
	      # collide and package stale files from a prior Preview-enabled export.
	      out_dir <- tempfile("spike_detector_export_", tmpdir = tempdir())
	      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
	      on.exit(unlink(out_dir, recursive = TRUE, force = TRUE), add = TRUE)
      
      export_event_csv(ev, "burst", file.path(out_dir, "Burst_events.csv"), unit_out = u)
      export_event_csv(ev, "long_burst", file.path(out_dir, "Long_burst_events.csv"), unit_out = u)
      export_event_csv(ev, "possible_burst", file.path(out_dir, "Possible_burst_events.csv"), unit_out = u)
      export_event_csv(ev, "tonic", file.path(out_dir, "Tonic_events.csv"), unit_out = u)
      export_event_csv(ev, "high_frequency_tonic", file.path(out_dir, "High_frequency_tonic_events.csv"), unit_out = u)
      export_event_csv(ev, "high_frequency_spiking", file.path(out_dir, "High_frequency_spiking_events.csv"), unit_out = u)
      export_event_csv(ev, "pause", file.path(out_dir, "Pause_events.csv"), unit_out = u)
      formal_export_progress_phase(
        export_id, "write_event_tables", 0.30,
        formal_export_inline_text("\u5DF2\u5199\u5165\u6240\u6709\u652F\u6301\u4E8B\u4EF6\u7C7B\u578B\u7684\u6B63\u5F0F\u4E8B\u4EF6\u8868\u3002", "Wrote formal event tables for all supported event families.")
      )
      
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
		      final_audit_summary <- ds$results[["final_audit_summary"]]
		      final_audit_events <- ds$results[["final_audit_events"]]
		      final_audit_history <- ds$results[["final_audit_history"]]
		      final_audit_event_history <- ds$results[["final_audit_event_history"]]
		      if (!is.null(final_audit_summary) && nrow(final_audit_summary) > 0) {
		        write_csv_safe(final_audit_summary, file.path(out_dir, "Final_audit_summary.csv"), row.names = FALSE, fileEncoding = "UTF-8")
		      }
		      if (!is.null(final_audit_events) && nrow(final_audit_events) > 0) {
		        write_csv_safe(final_audit_events, file.path(out_dir, "Final_audit_events.csv"), row.names = FALSE, fileEncoding = "UTF-8")
		      }
		      if (!is.null(final_audit_history) && nrow(final_audit_history) > 0) {
		        write_csv_safe(final_audit_history, file.path(out_dir, "Final_audit_history.csv"), row.names = FALSE, fileEncoding = "UTF-8")
		      }
		      if (!is.null(final_audit_event_history) && nrow(final_audit_event_history) > 0) {
		        write_csv_safe(final_audit_event_history, file.path(out_dir, "Final_audit_event_history.csv"), row.names = FALSE, fileEncoding = "UTF-8")
		      }
		      task_events_out <- stpd_normalize_task_events(ds$task_events %||% data.frame(), source = ds$meta$display_name %||% "")
		      if (nrow(task_events_out) > 0L) {
		        write_csv_safe(task_events_out, file.path(out_dir, "Task_events.csv"), row.names = FALSE, fileEncoding = "UTF-8")
		      }
	      promotion_audit <- ds$results[["possible_burst_promotion_audit"]]
	      promotion_summary <- ds$results[["possible_burst_promotion_summary"]]
	      if (!is.null(promotion_audit) && nrow(promotion_audit) > 0) {
	        write_csv_safe(promotion_audit, file.path(out_dir, "Possible_burst_promotion_audit.csv"), row.names = FALSE, fileEncoding = "UTF-8")
	      }
	      if (!is.null(promotion_summary) && nrow(promotion_summary) > 0) {
	        write_csv_safe(promotion_summary, file.path(out_dir, "Possible_burst_promotion_summary.csv"), row.names = FALSE, fileEncoding = "UTF-8")
	      }
      formal_export_progress_phase(
        export_id, "write_label_audits", 0.42,
        formal_export_inline_text("\u5DF2\u5199\u5165 ISI \u6807\u7B7E\u3001\u4EFB\u52A1\u4E8B\u4EF6\u548C\u53EF\u7528\u7684\u5BA1\u8BA1\u8D26\u672C\u3002", "Wrote ISI labels, task events, and available audit ledgers.")
      )
      
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
      ds_norm <- normalize_dataset(ds_for_export)
      formal_export_progress_phase(
        export_id, "write_diagnostics", 0.51,
        ui_text("writing_detector_artifacts")
      )
      # tiered result exports. These files separate high-confidence events, review candidates,
      # burst-family candidate metrics, and the full candidate ledger with demotion/rejection reasons.
      write_tiered_result_exports(ds_norm, p, out_dir)
      write_csv_safe(
        data.frame(
          artifact = validation_decision$artifact_name,
          action = validation_decision$action,
          code = validation_decision$code,
          reason = validation_decision$reason,
          stringsAsFactors = FALSE
        ),
        file.path(out_dir, "Validation_export_status.csv"),
        row.names = FALSE, fileEncoding = "UTF-8"
      )
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
      metadata_export <- ds_norm$results$run_metadata_public %||% ds_norm$results$run_metadata %||% data.frame()
      if (!is.null(metadata_export) && nrow(metadata_export) > 0) {
        write_csv_safe(metadata_export, file.path(out_dir, "Detector_run_metadata.csv"), row.names = FALSE, fileEncoding = "UTF-8")
      }
      if (!is.null(ds_norm$results$threshold_table) && nrow(ds_norm$results$threshold_table) > 0) {
        write_csv_safe(ds_norm$results$threshold_table, file.path(out_dir, "Detector_threshold_table.csv"), row.names = FALSE, fileEncoding = "UTF-8")
      }
      stpd_write_effective_params_artifact(ds_norm, p, out_dir)
      formal_export_progress_phase(
        export_id, "write_provenance", 0.64,
        ui_text("writing_provenance")
      )
      export_metadata <- stpd_export_run_metadata(
        ds_norm,
        events = ev,
        labels = lab,
        export_source = "shiny_audit_final_zip",
        final_label_source = "audit_final"
      )
      write_csv_safe(export_metadata, file.path(out_dir, "Export_run_metadata.csv"), row.names = FALSE, fileEncoding = "UTF-8")
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
      formal_export_progress_phase(
        export_id, "write_qc", 0.74,
        formal_export_inline_text("\u5DF2\u5199\u5165\u6570\u636E\u8D28\u91CF\u548C\u5355 train \u4E13\u5C5E\u9608\u503C\u4EA7\u7269\u3002", "Wrote data-quality and train-specific threshold artifacts.")
      )
      # Recompute Manual-vs-detector evaluation for the dataset being exported.
      # This avoids exporting a stale report generated for a different \u5F53\u524D\u6570\u636E\u96C6.
      md <- evaluate_detector_against_manual(
        ds_norm, p,
        selected_trains = names(ds_norm$trains),
        min_isi_sec = p$detector$min_valid_isi_sec %||% min_valid_isi_sec(),
        use_learned_ranges = FALSE,
        metric_mode = "strict_high_confidence"
      )
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
      formal_export_progress_phase(
        export_id, "write_validation", 0.83,
        formal_export_inline_text("\u5DF2\u5199\u5165\u91CD\u65B0\u8BA1\u7B97\u7684 MANUAL \u4E0E\u68C0\u6D4B\u5668\u5BF9\u7167\u8BC4\u4F30\u3002", "Wrote the freshly recomputed manual-versus-detector evaluation.")
      )
      
      params_out <- capture.output(str(ds_norm$params_effective %||% p))
      writeLines(params_out, file.path(out_dir, "Detector_params.txt"))

      # The Shiny ZIP path does not pass through the API exporter.  Reuse the
      # same result-driven writer so GUI and API bundles expose an identical
      # opt-in multi-track preview contract.
      stpd_write_multitrack_preview(ds_norm, out_dir)
      if (exists("stpd_write_multitrack_review", mode = "function")) {
        stpd_write_multitrack_review(ds_norm, out_dir)
      }
      if (exists("stpd_write_multitrack_gate_b_fail_soft", mode = "function") &&
          !is.null((ds_norm$results %||% list())$multitrack_gate_b)) {
        stpd_write_multitrack_gate_b_fail_soft(ds_norm, out_dir)
      }
      formal_export_progress_phase(
        export_id, "archive", 0.93,
        formal_export_inline_text("\u6B63\u5728\u5C06\u6240\u6709\u5DF2\u751F\u6210\u4EA7\u7269\u6253\u5305\u4E3A ZIP\u3002", "Packaging all generated artifacts into the ZIP archive.")
      )

      old <- setwd(out_dir); on.exit(setwd(old), add = TRUE)
      utils::zip(zipfile = file, files = list.files(out_dir))
      formal_export_progress_phase(
        export_id, "verify", 0.98,
        formal_export_inline_text("\u6B63\u5728\u9A8C\u8BC1\u5DF2\u751F\u6210\u7684 ZIP \u6587\u4EF6\u3002", "Verifying the generated ZIP file.")
      )
          }
        )
        formal_export_notify_generated(export_id, ui_text("formal_results_zip"), file)
      }, error = function(e) {
        formal_export_notify_error(export_id, ui_text("formal_results_zip"), e)
        stop(e)
      })
    }
  )


  }, envir = server_env)
}

stpd_server_install_visualization_module <- function(server_env) {
  evalq({
  ui_inline_text <- function(zh, en) {
    if (identical(ui_language(), "en")) en else zh
  }

  ui_extended_isi_metrics_hover <- function(linear_pct, log_pct, robust_log_pct, show = TRUE) {
    out <- extended_isi_metrics_hover(linear_pct, log_pct, robust_log_pct, show = show)
    if (!identical(ui_language(), "en")) {
      out <- gsub("ISI linear range position", "ISI \u7EBF\u6027\u8303\u56F4\u4F4D\u7F6E", out, fixed = TRUE)
      out <- gsub("ISI log-range position", "ISI \u5BF9\u6570\u8303\u56F4\u4F4D\u7F6E", out, fixed = TRUE)
      out <- gsub("ISI robust log-range position", "ISI \u7A33\u5065\u5BF9\u6570\u8303\u56F4\u4F4D\u7F6E", out, fixed = TRUE)
    }
    out
  }

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
    message <- as.character(message %||% ui_inline_text("\u6B63\u5728\u751F\u6210\u56FE\u5F62", "Generating plot view"))[1]
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
    lang <- if (identical(ui_language(), "en")) "en" else "zh"
    pick_progress_copy <- function(x, fallback = "") {
      if (is.null(x) || is.null(names(x)) || !(lang %in% names(x))) return(fallback)
      as.character(x[[lang]] %||% fallback)[1]
    }
    progress_message <- pick_progress_copy(
      rv$data_load_progress_message_i18n,
      rv$data_load_progress_message %||% ""
    )
    progress_message <- as.character(progress_message %||% ui_inline_text("\u6B63\u5728\u51C6\u5907\u56FE\u5F62\u89C6\u56FE", "Preparing plot view"))[1]
    if (!nzchar(progress_message)) progress_message <- ui_inline_text("\u6B63\u5728\u51C6\u5907\u56FE\u5F62\u89C6\u56FE", "Preparing plot view")
    progress_detail <- pick_progress_copy(
      rv$data_load_progress_detail_i18n,
      rv$data_load_progress_detail %||% ""
    )
    progress_detail <- as.character(progress_detail %||% ui_inline_text(
      "\u7B49\u5F85\u6570\u636E\u96C6\u5BFC\u5165\u540E\u751F\u6210\u56FE\u5F62\u89C6\u56FE\u3002",
      "Waiting for a dataset import before generating the plot view."
    ))[1]
    div(
      class = paste("plot-output-wrap", if (length(rv$datasets) == 0L) "plot-output-wrap-empty" else ""),
      plotlyOutput("raster_plot", height = "68vh", width = "100%"),
      if (length(rv$datasets) == 0L) {
        div(
          class = "plot-output-empty",
          div(class = "plot-output-empty-title", ui_inline_text("\u8BF7\u81F3\u5C11\u4E0A\u4F20\u4E00\u4E2A\u6570\u636E\u96C6\u3002", "Upload at least one dataset.")),
          div(ui_inline_text("\u4E0A\u4F20\u5B8C\u6210\u540E\uFF0C\u53F3\u4FA7\u4F1A\u663E\u793A\u56FE\u5F62\u89C6\u56FE\u7684\u751F\u6210\u8FDB\u5EA6\u3002", "After upload, plot-generation progress will appear on the right."))
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
    validate(need(length(selected) > 0, ui_inline_text("\u8BF7\u81F3\u5C11\u9009\u62E9\u4E00\u6761 train\u3002", "Select at least one train.")))
    step <- track_step()
    y_index <- rev(seq_along(selected))
    ds <- current_dataset()
    id <- rv$current_id
    dataset_identity <- tryCatch(current_ui_dataset_identity(), error = function(e) NULL)
    identity <- tryCatch(ui_run_identity_for(ds, id, dataset_identity), error = function(e) NULL)
    state <- tryCatch(
      stpd_ui_run_state(
        ds, params = current_ui_params(), dataset_id = id,
        run_identity = identity, dataset_identity = dataset_identity
      ),
      error = function(e) NULL
    )
    scope <- stpd_ui_state_parse_trains((identity %||% list())$selected_trains)
    state_code <- as.character((state %||% list())$code %||% "run_identity_unverifiable")[1]
    run_known <- !is.null(identity) && isTRUE(identity$has_run) && length(scope) > 0L
    train_status <- vapply(selected, function(train) {
      if (!run_known) {
        if (!is.null(identity) && isTRUE(identity$has_run)) {
          ui_inline_text("\u72B6\u6001\u672A\u77E5", "status unknown")
        } else {
          ui_inline_text("\u672A\u5206\u6790", "not analyzed")
        }
      } else if (!(train %in% scope)) {
        ui_inline_text("\u672A\u5206\u6790", "not analyzed")
      } else if (state_code %in% c("run_current", "run_partial_current")) {
        ui_inline_text("\u5DF2\u5206\u6790", "analyzed")
      } else {
        ui_inline_text("\u7ED3\u679C\u8FC7\u671F", "results stale")
      }
    }, character(1))
    data.frame(
      train = selected,
      train_label = paste0(selected, " [", train_status, "]"),
      analysis_status = train_status,
      train_order = seq_along(selected),
      y = 1 + (y_index - 1) * step,
      stringsAsFactors = FALSE
    )
  })

  aligned_data <- reactive({
    ds <- current_dataset()
    td <- ds$trains
    ledger <- ds$results$candidate_ledger %||% data.frame()
    v2_source <- if (identical(input$pattern_view, "auto")) "automatic" else "preferred"
    v2_per_isi <- tryCatch(
      stpd_multitrack_authoritative_per_isi(ds, v2_source),
      error = function(e) stpd_multitrack_auto_empty_per_isi()
    )
    selected <- displayed_train_names()
    validate(need(length(selected) > 0, ui_inline_text("\u8BF7\u81F3\u5C11\u9009\u62E9\u4E00\u6761 train\u3002", "Select at least one train.")))
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
      v2_train <- v2_per_isi[v2_per_isi$train == tr, , drop = FALSE]
      v2_hit <- match(as.integer(dat$idx), as.integer(v2_train$isi_index))
      v2_chr <- function(name) {
        out <- rep("", nrow(dat)); ok <- !is.na(v2_hit)
        if (any(ok)) out[ok] <- as.character(v2_train[[name]][v2_hit[ok]])
        out[is.na(out)] <- ""; out
      }
      v2_authoritative <- rep(FALSE, nrow(dat))
      ok_v2 <- !is.na(v2_hit)
      if (any(ok_v2)) v2_authoritative[ok_v2] <-
        as.logical(v2_train$authoritative[v2_hit[ok_v2]])
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
        v2_event_family = v2_chr("event_family"),
        v2_event_extent = v2_chr("event_extent_class"),
        v2_event_frequency = v2_chr("event_frequency_class"),
        v2_state_class = v2_chr("state_class"),
        v2_gap_class = v2_chr("gap_class"),
        v2_review_label = v2_chr("review_label"),
        v2_authoritative = v2_authoritative,
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
    v2_source <- if (identical(input$pattern_view, "auto")) "automatic" else "preferred"
    v2_per_isi <- tryCatch(
      stpd_multitrack_authoritative_per_isi(ds, v2_source),
      error = function(e) stpd_multitrack_auto_empty_per_isi()
    )
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
      v2_train <- v2_per_isi[v2_per_isi$train == tr, , drop = FALSE]
      v2_hit <- match(as.integer(dat$idx[keep]), as.integer(v2_train$isi_index))
      v2_chr <- function(name) {
        out <- rep("", length(keep)); ok <- !is.na(v2_hit)
        if (any(ok)) out[ok] <- as.character(v2_train[[name]][v2_hit[ok]])
        out[is.na(out)] <- ""; out
      }
      v2_authoritative <- rep(FALSE, length(keep))
      ok_v2 <- !is.na(v2_hit)
      if (any(ok_v2)) v2_authoritative[ok_v2] <-
        as.logical(v2_train$authoritative[v2_hit[ok_v2]])
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
        v2_event_family = v2_chr("event_family"),
        v2_event_extent = v2_chr("event_extent_class"),
        v2_event_frequency = v2_chr("event_frequency_class"),
        v2_state_class = v2_chr("state_class"),
        v2_gap_class = v2_chr("gap_class"),
        v2_review_label = v2_chr("review_label"),
        v2_authoritative = v2_authoritative,
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
      ui_inline_text(
        paste0("LOD \u8B66\u544A\uFF1A\u5F53\u524D\u7A97\u53E3\u5305\u542B ", n_visible, " \u4E2A\u8109\u51B2\u3002\u60AC\u505C/\u9009\u62E9\u5DF2\u7B80\u5316\uFF1B\u8BF7\u653E\u5927\u540E\u8FDB\u884C\u7CBE\u786E\u624B\u52A8\u6807\u8BB0\u3002"),
        paste0("LOD warning: the current window contains ", n_visible, " spikes. Hover/selection is simplified; zoom in before precise manual labeling.")
      )
    } else {
      ui_inline_text(
        paste0("LOD \u8B66\u544A\uFF1A\u5F53\u524D\u7A97\u53E3\u5305\u542B ", n_visible, " \u4E2A\u8109\u51B2\uFF0C\u8D85\u8FC7\u4EA4\u4E0A\u9650\uFF08", interactive_limit,
               "\uFF09\u3002\u56FE\u4E2D\u4F1A\u9690\u85CF\u5927\u90E8\u5206\u60AC\u505C/\u9009\u62E9\u6807\u8BB0\u3002\u8BF7\u5728\u624B\u52A8\u6807\u8BB0\u524D\u7F29\u5C0F\u65F6\u95F4\u7A97\u3002"),
        paste0("LOD warning: the current window contains ", n_visible, " spikes, above the interactive limit (", interactive_limit,
               "). Most hover/selection markers are hidden. Narrow the time window before manual labeling.")
      )
    }
    list(n = n_visible, mode = mode, message = msg)
  })

  output$raster_lod_warning <- renderUI({
    if (length(rv$datasets) == 0L) return(NULL)
    st <- raster_lod_state()
    if (is.null(st$message) || st$message == "") return(NULL)
    div(style = "background:#fff4e6;border:1px solid #ffd08a;border-radius:6px;padding:8px;margin-bottom:8px;color:#5c3b00;",
        strong(ui_inline_text("\u5927\u7A97\u53E3\u663E\u793A\u6A21\u5F0F\uFF1A", "Large-window display mode: ")), st$message)
  })

  raw_spike_data <- reactive({
    td <- current_trains()
    selected <- displayed_train_names()
    validate(need(length(selected) > 0, ui_inline_text("\u8BF7\u81F3\u5C11\u9009\u62E9\u4E00\u6761 train\u3002", "Select at least one train.")))
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
      return(tags$div(class = "small-note", ui_text("no_task_events")))
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
    jump_selected <- as.character(ui_control_remembered(
      "task_event_jump_id", ev$event_id[1] %||% ""
    ))[1]
    if (!jump_selected %in% as.character(ev$event_id)) {
      jump_selected <- as.character(ev$event_id[1] %||% "")
    }
    tagList(
      selectizeInput(
        "task_event_names",
        ui_inline_text("\u4EFB\u52A1\u4E8B\u4EF6\u7C7B\u578B", "Task-event types"),
        choices = all_names,
        selected = selected_names,
        multiple = TRUE,
        options = list(plugins = list("remove_button"), closeAfterSelect = TRUE)
      ),
      selectInput(
        "task_event_jump_id",
        ui_inline_text("\u8DF3\u8F6C\u5230\u67D0\u6B21\u4E8B\u4EF6", "Jump to an event occurrence"),
        choices = stats::setNames(ev$event_id, jump_labels),
        selected = jump_selected
      ),
      tags$div(class = "small-note", ui_inline_text(
        "\u4E8B\u4EF6\u4EC5\u4F5C\u4E3A\u56FE\u4E0A\u6CE8\u91CA\u3001\u884C\u4E3A\u5BF9\u9F50\u548C\u4E0B\u6E38\u9A8C\u8BC1\u5C42\uFF1B\u4E0D\u4F1A\u53C2\u4E0E\u6838\u5FC3 burst/pause/tonic \u68C0\u6D4B\u3002",
        "Events are used only for plot annotations, behavioral alignment, and downstream validation; they do not affect core burst/pause/tonic detection."
      ))
    )
  })

  output$neural_manifold_dataset_event_selector <- renderUI({
    events <- task_events_current()
    if (nrow(events) == 0L) {
      return(tags$div(class = "small-note", ui_text("no_task_events")))
    }
    all_names <- sort(unique(as.character(events$event_name)))
    selected_names <- intersect(as.character(isolate(input$neural_manifold_dataset_event_names) %||% all_names), all_names)
    if (length(selected_names) == 0L) selected_names <- all_names
    tagList(
      selectizeInput(
        "neural_manifold_dataset_event_names",
        ui_inline_text("\u6570\u636E\u96C6\u4EFB\u52A1\u4E8B\u4EF6", "Dataset task events"),
        choices = all_names,
        selected = selected_names,
        multiple = TRUE,
        options = list(plugins = list("remove_button"), closeAfterSelect = TRUE)
      ),
      tags$div(
        class = "small-note",
        ui_inline_text(
          "\u8FD9\u4E9B\u4E8B\u4EF6\u7528\u4E8E\u6807\u6CE8\u8FD0\u52A8\u5468\u8FB9\u7684\u65F6\u95F4\u7BB1\uFF1B\u672A\u4E0A\u4F20\u72EC\u7ACB\u8BD5\u6B21 CSV \u65F6\uFF0C\u4E5F\u53EF\u4F5C\u4E3A sliceTCA \u7684\u8BD5\u6B21\u65F6\u95F4\u3002",
          "These events annotate peri-movement bins and can supply sliceTCA trial times when no separate trial CSV is uploaded."
        )
      )
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
      ui_inline_text(
        paste0("\u539F\u59CB timestamp \u65F6\u95F4\u7A97\uFF08", u, "\uFF09"),
        paste0("Raw timestamp window (", u, ")")
      ),
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
      showNotification(ui_inline_text("\u5F53\u524D\u6570\u636E\u96C6\u6CA1\u6709\u53EF\u8DF3\u8F6C\u7684\u4EFB\u52A1\u4E8B\u4EF6\u3002", "The current dataset has no task event to jump to."), type = "warning", duration = 5)
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

  # ----------------------------------------------------------
  # Aligned plot
  # ----------------------------------------------------------
  output$raster_overview_plot <- renderPlotly({
    # Establish a direct reactive dependency on the Shiny slider.  The helper
    # used by the main raster intentionally isolates inputs in a few fallback
    # branches; calling the reactive source here prevents the overview from
    # remaining frozen after the first render.
    slider_window <- raster_slider_window_source()
    td <- current_trains()
    validate(need(length(td) > 0, ui_inline_text("\u5C1A\u65E0\u53EF\u9884\u89C8\u7684 spike train\u3002", "No spike trains are available for the overview.")))
    f <- unit_factor()
    rows <- lapply(seq_along(td), function(ii) {
      dat <- td[[ii]]
      if (is.null(dat) || nrow(dat) == 0L || !("timestamp_sec" %in% names(dat))) return(NULL)
      x <- suppressWarnings(as.numeric(dat$time_align_sec %||% dat$timestamp_sec)) * f
      x <- x[is.finite(x)]
      if (length(x) == 0L) return(NULL)
      data.frame(x = x, y = ii, y0 = ii - 0.34, y1 = ii + 0.34)
    })
    rows <- rows[!vapply(rows, is.null, logical(1))]
    dat <- if (length(rows) > 0L) dplyr::bind_rows(rows) else data.frame()
    validate(need(nrow(dat) > 0, ui_inline_text("\u5C1A\u65E0\u53EF\u9884\u89C8\u7684 timestamp\u3002", "No timestamps are available for the overview.")))
    # Keep the overview cheap even for unusually large datasets while
    # preserving deterministic coverage across the full recording.
    max_ticks <- 120000L
    if (nrow(dat) > max_ticks) {
      keep <- unique(round(seq(1, nrow(dat), length.out = max_ticks)))
      dat <- dat[keep, , drop = FALSE]
    }
    x_full <- range(dat$x, finite = TRUE)
    if (length(x_full) != 2L || !all(is.finite(x_full)) || x_full[2] <= x_full[1]) x_full <- c(0, 1)
    win <- slider_window %||% raster_window_for_plot(debounced = FALSE, prefer_view = TRUE)
    win <- sort(as.numeric(win))
    win <- win[is.finite(win)]
    if (length(win) != 2L) win <- x_full
    win <- c(max(x_full[1], win[1]), min(x_full[2], win[2]))
    if (win[2] <= win[1]) win <- x_full
    p <- plot_ly(source = "raster_overview")
    p <- add_segments(p, data = dat, x = ~x, xend = ~x, y = ~y0, yend = ~y1,
                      type = "scatter", mode = "lines",
                      line = list(color = "#6b7280", width = 1), hoverinfo = "none",
                      showlegend = FALSE, inherit = FALSE)
    p <- layout(
      p,
      title = list(text = ui_inline_text("\u5168\u5C40 spike train \u9884\u89C8", "Global spike-train overview"), font = list(size = 12)),
      xaxis = list(title = ui_inline_text("\u65F6\u95F4\uFF08\u5F53\u524D\u5355\u4F4D\uFF09", "Time (display unit)"), range = x_full, fixedrange = TRUE,
                   showgrid = FALSE, zeroline = FALSE),
      yaxis = list(range = c(0.5, length(td) + 0.5), visible = FALSE, fixedrange = TRUE),
      shapes = list(list(type = "rect", xref = "x", yref = "paper", x0 = win[1], x1 = win[2], y0 = 0, y1 = 1,
                         fillcolor = "rgba(37,99,235,0.18)", line = list(color = "rgba(37,99,235,0.75)", width = 1))),
      margin = list(l = 8, r = 8, t = 28, b = 30),
      hovermode = FALSE
    )
    config(p, displaylogo = FALSE, scrollZoom = FALSE)
  })

  # The raw-timestamp plot has its own overview.  It must not reuse the
  # aligned overview's time base: timestamp_sec and time_align_sec have
  # different origins and can have very different ranges.
  output$raster_raw_overview_plot <- renderPlotly({
    raw_slider_window <- input$raw_xrange
    td <- current_trains()
    validate(need(length(td) > 0, ui_inline_text("\u5C1A\u65E0\u53EF\u9884\u89C8\u7684 spike train\u3002", "No spike trains are available for the overview.")))
    f <- unit_factor()
    rows <- lapply(seq_along(td), function(ii) {
      dat <- td[[ii]]
      if (is.null(dat) || nrow(dat) == 0L || !("timestamp_sec" %in% names(dat))) return(NULL)
      x <- suppressWarnings(as.numeric(dat$timestamp_sec)) * f
      x <- x[is.finite(x)]
      if (length(x) == 0L) return(NULL)
      data.frame(x = x, y = ii, y0 = ii - 0.34, y1 = ii + 0.34)
    })
    rows <- rows[!vapply(rows, is.null, logical(1))]
    dat <- if (length(rows) > 0L) dplyr::bind_rows(rows) else data.frame()
    validate(need(nrow(dat) > 0, ui_inline_text("\u5C1A\u65E0\u53EF\u9884\u89C8\u7684 timestamp\u3002", "No timestamps are available for the overview.")))
    max_ticks <- 120000L
    if (nrow(dat) > max_ticks) {
      keep <- unique(round(seq(1, nrow(dat), length.out = max_ticks)))
      dat <- dat[keep, , drop = FALSE]
    }
    x_full <- range(dat$x, finite = TRUE)
    if (length(x_full) != 2L || !all(is.finite(x_full)) || x_full[2] <= x_full[1]) x_full <- c(0, 1)
    win <- raw_slider_window
    if (!stpd_valid_xrange_window(win)) win <- raw_window_for_plot() * f
    win <- sort(as.numeric(win))
    win <- c(max(x_full[1], win[1]), min(x_full[2], win[2]))
    if (win[2] <= win[1]) win <- x_full
    p <- plot_ly(source = "raster_raw_overview")
    p <- add_segments(p, data = dat, x = ~x, xend = ~x, y = ~y0, yend = ~y1,
                      type = "scatter", mode = "lines",
                      line = list(color = "#6b7280", width = 1), hoverinfo = "none",
                      showlegend = FALSE, inherit = FALSE)
    p <- layout(
      p,
      title = list(text = ui_inline_text("\u5168\u5C40\u539F\u59CB\u65F6\u95F4\u6233\u9884\u89C8", "Global raw-timestamp overview"), font = list(size = 12)),
      xaxis = list(title = ui_inline_text("\u539F\u59CB\u65F6\u95F4\u6233\uFF08\u5F53\u524D\u5355\u4F4D\uFF09", "Raw timestamp (display unit)"), range = x_full,
                   fixedrange = TRUE, showgrid = FALSE, zeroline = FALSE),
      yaxis = list(range = c(0.5, length(td) + 0.5), visible = FALSE, fixedrange = TRUE),
      shapes = list(list(type = "rect", xref = "x", yref = "paper", x0 = win[1], x1 = win[2], y0 = 0, y1 = 1,
                         fillcolor = "rgba(37,99,235,0.18)", line = list(color = "rgba(37,99,235,0.75)", width = 1))),
      margin = list(l = 8, r = 8, t = 28, b = 30), hovermode = FALSE
    )
    config(p, displaylogo = FALSE, scrollZoom = FALSE)
  })

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
          message = ui_inline_text("\u6B63\u5728\u751F\u6210\u5BF9\u9F50\u65F6\u95F4\u6233\u56FE", "Generating aligned-timestamp plot"),
          type = type
        )
        setProgress(value, detail = detail)
        if (type %in% c("success", "error")) rv$raster_plot_progress_active <- FALSE
      }
    }
    raster_progress(0.05, ui_inline_text("\u6B63\u5728\u51C6\u5907\u53EF\u89C1 spike trains", "Preparing visible spike trains"))
    dat_all <- aligned_window_data()
    if (nrow(dat_all) == 0L) {
      no_visible_spikes <- ui_inline_text("\u5F53\u524D\u7A97\u53E3\u6CA1\u6709\u53EF\u663E\u793A spike\u3002", "No spikes are visible in the current window.")
      raster_progress(1, no_visible_spikes, "success")
      return(stpd_empty_plotly_message(no_visible_spikes, source = "raster", events = c("plotly_selected", "plotly_relayout")))
    }
    
    raster_progress(0.18, ui_inline_text("\u6B63\u5728\u6574\u7406 spike \u548C\u6A21\u5F0F\u6807\u7B7E", "Organizing spikes and pattern labels"))
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
      no_window_data <- ui_inline_text("\u5F53\u524D\u7A97\u53E3\u6CA1\u6709 spike/ISI\u3002", "No spike/ISI data are visible in the current window.")
      raster_progress(1, no_window_data, "success")
      return(stpd_empty_plotly_message(no_window_data, source = "raster", events = c("plotly_selected", "plotly_relayout")))
    }
    raster_progress(0.34, ui_inline_text("\u6B63\u5728\u8BA1\u7B97\u53EF\u89C6\u533A\u548C LOD", "Calculating the visible region and LOD"))
    full_limit <- safe_int(input$plot_max_visible_spikes_full, 50000L)
    interactive_limit <- max(full_limit, safe_int(input$plot_max_visible_spikes_interactive, 100000L))
	    lod_mode <- input$plot_lod_mode %||% "auto"
	    visible_spike_n <- nrow(has_visible_spike)
	    visible_train_n <- length(unique(as.character(c(has_visible_spike$train, has_visible_isi$train))))
	    many_train_overview <- visible_train_n >= 12L && visible_spike_n > 8000L && identical(lod_mode, "auto")
	    lod_full <- identical(lod_mode, "full") || (identical(lod_mode, "auto") && visible_spike_n <= full_limit && !many_train_overview)
	    lod_interactive <- lod_full || (identical(lod_mode, "auto") && visible_spike_n <= interactive_limit)
	    lod_note <- if (!lod_full) ui_inline_text(
	      paste0(
	        "LOD \u6A21\u5F0F\uFF1A", visible_spike_n, " \u4E2A\u53EF\u89C1 spike\uFF1B\u60AC\u505C/\u9009\u62E9\u6807\u8BB0\u5DF2\u7B80\u5316\u3002\u8BF7\u653E\u5927\u540E\u624B\u52A8\u6807\u8BB0\u3002",
	        if (many_train_overview) " \u5F53\u524D\u9875 train \u8F83\u591A\uFF0C\u5DF2\u81EA\u52A8\u4F7F\u7528\u6982\u89C8\u6E32\u67D3\u3002" else ""
	      ),
	      paste0(
	        "LOD mode: ", visible_spike_n, " visible spikes; hover/selection markers are simplified. Zoom in before manual labeling.",
	        if (many_train_overview) " Many trains are visible on this page, so overview rendering was enabled automatically." else ""
	      )
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
             label_source_html = stpd_html_escape(ifelse(label_source == "none", ui_inline_text("\u65E0", "none"), label_source)),
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
          ui_inline_text("\u53C2\u6570\u8BD5\u8FD0\u884C\u5DEE\u5F02<br>", "Parameter dry-run difference<br>"),
	          ui_inline_text("\u72B6\u6001\uFF1A", "Status: "), stpd_html_escape(delta_overlay$status),
	          ui_inline_text("<br>Train\uFF1A", "<br>Train: "), stpd_html_escape(delta_overlay$train),
	          ui_inline_text("<br>\u57FA\u7EBF\uFF1A", "<br>Baseline: "), ifelse(nzchar(delta_overlay$baseline_pattern), stpd_html_escape(delta_overlay$baseline_pattern), ui_inline_text("\u65E0", "none")),
	          ui_inline_text("<br>\u5F53\u524D\uFF1A", "<br>Current: "), ifelse(nzchar(delta_overlay$current_pattern), stpd_html_escape(delta_overlay$current_pattern), ui_inline_text("\u65E0", "none")),
	          "<br>ISI\uFF1A", delta_overlay$start_isi, "-", delta_overlay$end_isi,
	          "<br>IoU\uFF1A", ifelse(is.finite(delta_overlay$iou), signif(delta_overlay$iou, 4), "NA")
	        )
	      }
	    }
	    
	    raster_progress(0.48, ui_inline_text("\u6B63\u5728\u6784\u5EFA\u6805\u683C\u56FE\u7684 Plotly \u8F68\u8FF9", "Building Plotly raster traces"))
		    p <- plot_ly(source = "raster")
	    cand_audit <- current_dataset()$results$candidate_diagnostic_audit %||% current_dataset()$results$burst_candidates %||% data.frame()
	    structure_overlay <- current_dataset()$results$structure_candidates %||% data.frame()

    raster_batch_mode <- k >= 12L && !isTRUE(input$show_rejected_burst_candidates) && !isTRUE(input$show_burst_sublabel_structures)
    if (isTRUE(raster_batch_mode)) {
      raster_progress(0.54, ui_inline_text(sprintf("\u6B63\u5728\u6982\u89C8\u6E32\u67D3 %d \u6761 train", k), sprintf("Rendering an overview of %d train(s)", k)))
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
	            ui_inline_text("Train\uFF1A", "Train: "), train_label_html,
	            ui_inline_text("<br>Spike \u7D22\u5F15\uFF1A", "<br>Spike index: "), idx,
	            ui_inline_text("<br>Timestamp\uFF1A", "<br>Timestamp: "), round(timestamp_sec, 6), " s"
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
      raster_progress(0.66, ui_inline_text(sprintf("\u5DF2\u6784\u5EFA %d \u4E2A\u53EF\u89C1\u8109\u51B2\u523B\u7EBF", nrow(visible_spike)), sprintf("Built %d visible spike tick(s)", nrow(visible_spike))))

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

        add_gate_b_tracks <- function(pp, df) {
          authoritative <- df[df$v2_authoritative %in% TRUE, , drop = FALSE]
          if (nrow(authoritative) == 0L) return(pp)
          event <- authoritative[authoritative$v2_event_family == "burst", , drop = FALSE]
          if (nrow(event) > 0L) {
            event$y <- event$y + 0.12
            pp <- add_pattern_strips(pp, event, "burst", "auto")
          }
          for (pat in c("tonic", "high_frequency_tonic", "high_frequency_spiking")) {
            state <- authoritative[authoritative$v2_state_class == pat, , drop = FALSE]
            if (nrow(state) > 0L) pp <- add_pattern_strips(pp, state, pat, "auto")
          }
          gap <- authoritative[authoritative$v2_gap_class == "pause", , drop = FALSE]
          if (nrow(gap) > 0L) {
            gap$y <- gap$y - 0.12
            pp <- add_pattern_strips(pp, gap, "pause", "auto")
          }
          pp
        }

        if (identical(input$pattern_view, "final")) {
          if (any(visible_isi$v2_authoritative %in% TRUE)) p <- add_gate_b_tracks(p, visible_isi)
          else for (pat in pats) {
              sub_pat <- visible_isi[visible_isi$pattern_show == pat, , drop = FALSE]
              if (nrow(sub_pat) > 0) p <- add_pattern_strips(p, sub_pat, pat, "auto")
            }
        } else if (identical(input$pattern_view, "manual")) {
          for (pat in pats) {
            sub_pat <- visible_isi[visible_isi$pattern_manual == pat, , drop = FALSE]
            if (nrow(sub_pat) > 0) p <- add_pattern_strips(p, sub_pat, pat, "manual")
          }
        } else if (identical(input$pattern_view, "auto")) {
          if (any(visible_isi$v2_authoritative %in% TRUE)) p <- add_gate_b_tracks(p, visible_isi)
          else for (pat in pats) {
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
              ui_inline_text("\u5DF2\u9009\u5019\u9009 ISI<br>", "Selected candidate ISI<br>"),
              ui_inline_text("\u6A21\u5F0F\uFF1A", "Pattern: "), stpd_html_escape(pv$pattern),
              ui_inline_text("<br>\u6765\u6E90\uFF1A", "<br>Source: "), stpd_html_escape(pv$category),
              ui_inline_text("<br>\u8BC1\u636E/\u53C2\u6570\uFF1A", "<br>Evidence/parameter: "), stpd_html_escape(pv$parameter),
              ifelse(nzchar(as.character(pv$details %||% "")), paste0(ui_inline_text("<br>\u8BE6\u60C5\uFF1A", "<br>Details: "), stpd_html_escape(pv$details)), ""),
              ui_inline_text("<br>ISI \u7D22\u5F15\uFF1A", "<br>ISI index: "), pv_dat$idx,
              ifelse(is.finite(pv_dat$ISI_sec), paste0("<br>ISI: ", signif(pv_dat$ISI_sec, 6), " s"), ""),
              ui_inline_text("<br>\u5019\u9009 ISI \u8303\u56F4\uFF1A", "<br>Candidate ISI range: "), pv_s, "-", pv_e
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
	            ui_inline_text("Train\uFF1A", "Train: "), train_label_html,
	            ui_inline_text("<br>\u5DE6\u4FA7 spike timestamp\uFF1A", "<br>Left spike timestamp: "), round(timestamp_left_sec, 6), " s",
	            ui_inline_text("<br>\u53F3\u4FA7 spike timestamp\uFF1A", "<br>Right spike timestamp: "), round(timestamp_right_sec, 6), " s",
	            ifelse(is.na(ISI_sec), "<br>ISI: NA", paste0("<br>ISI: ", signif(ISI_sec, 6), " s (", round(ISI_plot, 3), " ", u, ")")),
	            ifelse(is.na(ISI_pct), "", paste0(ui_inline_text("<br>ISI \u5728\u672C train \u4E2D\u7684\u767E\u5206\u4F4D\uFF1A", "<br>ISI percentile in this train: "), round(ISI_pct, 2), "%")),
            ui_extended_isi_metrics_hover(
              ISI_range_pct_linear,
              ISI_range_pct_log,
              ISI_robust_range_pct_log,
              show = isTRUE(input$show_extended_isi_metrics)
            ),
	            ui_inline_text("<br>\u6700\u7EC8\u6807\u7B7E\uFF1A", "<br>Final label: "), ifelse(pattern_final == "", ui_inline_text("\u65E0", "none"), pattern_final_html),
	            ui_inline_text("<br>\u6700\u7EC8\u5BA1\u8BA1\u6807\u7B7E\uFF1A", "<br>Final audit label: "), ifelse(pattern_audit_final == "", ui_inline_text("\u65E0", "none"), pattern_audit_final_html),
	            ui_inline_text("<br>\u6807\u7B7E\u6765\u6E90\uFF1A", "<br>Label source: "), label_source_html,
	            ifelse(pattern_final == "possible_burst" & possible_burst_subtype != "", paste0(ui_inline_text("<br>possible_burst \u4E9A\u578B\uFF1A", "<br>possible_burst subtype: "), possible_burst_subtype_html), ""),
	            ifelse(pattern_final == "possible_burst" & uncertainty_reason != "", paste0(ui_inline_text("<br>\u4E0D\u786E\u5B9A\u6027\uFF1A", "<br>Uncertainty: "), uncertainty_reason_html), "")
          )
        )
      }
      raster_progress(0.84, ui_inline_text(sprintf("\u5DF2\u6784\u5EFA %d \u4E2A\u53EF\u89C1 ISI/\u6A21\u5F0F\u533A\u6BB5", nrow(visible_isi)), sprintf("Built %d visible ISI/pattern segment(s)", nrow(visible_isi))))
    }
	    
    draw_trains <- if (isTRUE(raster_batch_mode)) character(0) else selected
    draw_train_n <- length(draw_trains)
    for (tr_i in seq_along(draw_trains)) {
      tr <- draw_trains[[tr_i]]
      raster_progress(
        0.50 + 0.38 * (tr_i - 1) / max(1, draw_train_n),
        ui_inline_text(
          sprintf("\u6B63\u5728\u7ED8\u5236 train %d/%d\uFF1A%s", tr_i, draw_train_n, substr(as.character(tr), 1, 80)),
          sprintf("Drawing train %d/%d: %s", tr_i, draw_train_n, substr(as.character(tr), 1, 80))
        )
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
            ui_inline_text("Train\uFF1A", "Train: "), train_label_html,
            ui_inline_text("<br>Spike \u7D22\u5F15\uFF1A", "<br>Spike index: "), idx,
            ui_inline_text("<br>Timestamp\uFF1A", "<br>Timestamp: "), round(timestamp_sec, 6), " s"
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

        add_gate_b_tracks <- function(pp, df) {
          authoritative <- df[df$v2_authoritative %in% TRUE, , drop = FALSE]
          if (nrow(authoritative) == 0L) return(pp)
          event <- authoritative[authoritative$v2_event_family == "burst", , drop = FALSE]
          if (nrow(event) > 0L) {
            event$y <- event$y + 0.12
            pp <- add_pattern_strips(pp, event, "burst", "auto")
          }
          for (pat in c("tonic", "high_frequency_tonic", "high_frequency_spiking")) {
            state <- authoritative[authoritative$v2_state_class == pat, , drop = FALSE]
            if (nrow(state) > 0L) pp <- add_pattern_strips(pp, state, pat, "auto")
          }
          gap <- authoritative[authoritative$v2_gap_class == "pause", , drop = FALSE]
          if (nrow(gap) > 0L) {
            gap$y <- gap$y - 0.12
            pp <- add_pattern_strips(pp, gap, "pause", "auto")
          }
          pp
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
          if (any(sub_isi$v2_authoritative %in% TRUE)) p <- add_gate_b_tracks(p, sub_isi)
          else for (pat in pats) {
              sub_pat <- sub_isi[sub_isi$pattern_show == pat, , drop = FALSE]
              if (nrow(sub_pat) > 0) p <- add_pattern_strips(p, sub_pat, pat, "auto")
            }
        } else if (identical(input$pattern_view, "manual")) {
          for (pat in pats) {
            sub_pat <- sub_isi[sub_isi$pattern_manual == pat, , drop = FALSE]
            if (nrow(sub_pat) > 0) p <- add_pattern_strips(p, sub_pat, pat, "manual")
          }
        } else if (identical(input$pattern_view, "auto")) {
          if (any(sub_isi$v2_authoritative %in% TRUE)) p <- add_gate_b_tracks(p, sub_isi)
          else for (pat in pats) {
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
              ui_inline_text("\u5DF2\u9009\u5019\u9009 ISI<br>", "Selected candidate ISI<br>"),
              ui_inline_text("\u6A21\u5F0F\uFF1A", "Pattern: "), stpd_html_escape(pv$pattern),
              ui_inline_text("<br>\u6765\u6E90\uFF1A", "<br>Source: "), stpd_html_escape(pv$category),
              ui_inline_text("<br>\u8BC1\u636E/\u53C2\u6570\uFF1A", "<br>Evidence/parameter: "), stpd_html_escape(pv$parameter),
              ifelse(nzchar(as.character(pv$details %||% "")), paste0(ui_inline_text("<br>\u8BE6\u60C5\uFF1A", "<br>Details: "), stpd_html_escape(pv$details)), ""),
              ui_inline_text("<br>ISI \u7D22\u5F15\uFF1A", "<br>ISI index: "), pv_dat$idx,
              ifelse(is.finite(pv_dat$ISI_sec), paste0("<br>ISI: ", signif(pv_dat$ISI_sec, 6), " s"), ""),
              ui_inline_text("<br>\u5019\u9009 ISI \u8303\u56F4\uFF1A", "<br>Candidate ISI range: "), pv_s, "-", pv_e
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
                ui_inline_text("Burst \u5B50\u6807\u7B7E\uFF1Ainteresting_structure<br>", "Burst sublabel: interesting_structure<br>"),
                ui_inline_text("Motif\uFF1A", "Motif: "), stpd_html_escape(motif_type),
                ui_inline_text("<br>\u5173\u8054 burst\uFF1A", "<br>Linked burst: "), stpd_html_escape(linked_label), " ISI ", linked_s, "-", linked_e,
                ui_inline_text("<br>\u4E8B\u4EF6\u5305 ISI\uFF1A", "<br>Packet ISI: "), ss, "-", ee,
                ui_inline_text("<br>spike \u6570\uFF1A", "<br>n spikes: "), as.integer(get_so(so[so_i, , drop = FALSE], "n_spikes", NA_integer_))[1],
                ui_inline_text("<br>\u6301\u7EED\u65F6\u95F4\uFF1A", "<br>duration: "), signif(suppressWarnings(as.numeric(get_so(so[so_i, , drop = FALSE], "duration_sec", NA_real_))[1]) * 1000, 5), " ms",
                ui_inline_text("<br>median/q90 ISI\uFF1A", "<br>median/q90 ISI: "),
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
                ui_inline_text("<br>\u4E8B\u4EF6\u5305/burst q90 \u6BD4\u503C\uFF1A", "<br>packet/burst q90 ratio: "),
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
                  ui_inline_text("Burst \u5019\u9009\u8BCA\u65AD<br>", "Burst candidate diagnostics<br>"),
                  ui_inline_text("\u6700\u7EC8\u6807\u7B7E\uFF1A", "Final label: "), stpd_html_escape(rr_lab), "<br>",
                  ui_inline_text("\u72B6\u6001\uFF1A", "Status: "), stpd_html_escape(rr_status), "<br>",
                  ui_inline_text("\u539F\u56E0\uFF1A", "Reason: "), stpd_html_escape(rr_decision), "<br>",
                  ui_inline_text("\u8FB9\u754C\u7C7B\u578B\uFF1A", "Boundary type: "), stpd_html_escape(as.character(get_ca(ca[rr_i, , drop = FALSE], "boundary_type", ""))[1]), "<br>",
                  "ISI\uFF1A", rr_s, "-", rr_e,
                  "<br>intra q90\uFF1A", signif(suppressWarnings(as.numeric(get_ca(ca[rr_i, , drop = FALSE], "intra_q90_sec", NA_real_))[1]) * 1000, 4), " ms",
                  "<br>intra q95\uFF1A", signif(suppressWarnings(as.numeric(get_ca(ca[rr_i, , drop = FALSE], "intra_q95_sec", NA_real_))[1]) * 1000, 4), " ms",
                  "<br>pre/q90\uFF1A", signif(suppressWarnings(as.numeric(get_ca(ca[rr_i, , drop = FALSE], "pre_ratio_q90", NA_real_))[1]), 4),
                  "<br>post/q90\uFF1A", signif(suppressWarnings(as.numeric(get_ca(ca[rr_i, , drop = FALSE], "post_ratio_q90", NA_real_))[1]), 4),
                  "<br>seed \u7EAF\u5EA6\uFF1A", signif(suppressWarnings(as.numeric(get_ca(ca[rr_i, , drop = FALSE], "seed_purity", NA_real_))[1]), 4),
                  ui_inline_text("<br>bridge \u6BD4\u4F8B\uFF1A", "<br>bridge fraction: "), signif(suppressWarnings(as.numeric(get_ca(ca[rr_i, , drop = FALSE], "bridge_fraction", NA_real_))[1]), 4)
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
            ui_inline_text("Train\uFF1A", "Train: "), train_label_html,
            ui_inline_text("<br>\u5DE6\u4FA7 spike \u65F6\u95F4\u6233\uFF1A", "<br>Left spike timestamp: "), round(timestamp_left_sec, 6), " s",
            ui_inline_text("<br>\u53F3\u4FA7 spike \u65F6\u95F4\u6233\uFF1A", "<br>Right spike timestamp: "), round(timestamp_right_sec, 6), " s",
            ifelse(is.na(ISI_sec), "<br>ISI\uFF1ANA", paste0("<br>ISI\uFF1A", signif(ISI_sec, 6), " s (", round(ISI_plot, 3), " ", u, ")")),
            ifelse(is.na(ISI_pct), "", paste0(ui_inline_text("<br>ISI \u5728\u672C train \u4E2D\u7684\u767E\u5206\u4F4D\uFF1A", "<br>ISI percentile in this train: "), round(ISI_pct, 2), "%")),
            ui_extended_isi_metrics_hover(
              ISI_range_pct_linear,
              ISI_range_pct_log,
              ISI_robust_range_pct_log,
              show = isTRUE(input$show_extended_isi_metrics)
            ),
            ui_inline_text("<br>\u6700\u7EC8\u6807\u7B7E\uFF1A", "<br>Final label: "), ifelse(pattern_final == "", ui_inline_text("\u65E0", "none"), pattern_final_html),
            ui_inline_text("<br>\u6700\u7EC8\u5BA1\u8BA1\u6807\u7B7E\uFF1A", "<br>Final audit label: "), ifelse(pattern_audit_final == "", ui_inline_text("\u65E0", "none"), pattern_audit_final_html),
            ui_inline_text("<br>\u6807\u7B7E\u6765\u6E90\uFF1A", "<br>Label source: "), label_source_html,
            ifelse(pattern_final == "possible_burst" & possible_burst_subtype != "", paste0("<br>possible_burst \u4E9A\u578B\uFF1A", possible_burst_subtype_html), ""),
            ifelse(pattern_final == "possible_burst" & uncertainty_reason != "", paste0("<br>\u4E0D\u786E\u5B9A\u6027\uFF1A", uncertainty_reason_html), "")
          )
        )
	      }
	    }
    if (draw_train_n > 0L) raster_progress(0.88, ui_inline_text(sprintf("%d \u6761\u53EF\u89C1 train \u5DF2\u7ED8\u5236\u5B8C\u6210", draw_train_n), sprintf("Finished drawing %d visible train(s)", draw_train_n)))

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
	            text = ~paste0(delta_text, ui_inline_text("<br>\u8868\u683C\u9009\u4E2D\u884C", "<br>Selected table row")),
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
	        ui_inline_text("\u4EFB\u52A1/\u884C\u4E3A\u4E8B\u4EF6<br>", "Task/behavior event<br>"),
	        ui_inline_text("\u4E8B\u4EF6\uFF1A", "Event: "), stpd_html_escape(aligned_events$event_name),
	        ui_inline_text("<br>\u539F\u59CB\u65F6\u95F4\u6233\uFF1A", "<br>Raw timestamp: "), signif(aligned_events$event_time_sec, 7), " s",
	        ui_inline_text("<br>\u5BF9\u9F50\u5230\u672C train\uFF1A", "<br>Aligned to this train: "), signif(aligned_events$x_sec, 7), " s",
	        ui_inline_text("<br>Train\uFF1A", "<br>Train: "), stpd_html_escape(aligned_events$train_label)
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
      xaxis = list(title = ui_inline_text(paste0("\u5BF9\u9F50\u65F6\u95F4\uFF08", u, "\uFF09"), paste0("Aligned time (", u, ")")), range = x_use),
      yaxis = list(title = list(text = ui_inline_text("Spike train\uFF08\u8BB0\u5F55\u6761\u76EE\uFF09", "Spike train (recording item)"), standoff = 40), tickmode = "array",
                   tickvals = y_tickvals, ticktext = y_ticktext, tickfont = list(size = tick_font_size),
                   range = y_range, zeroline = FALSE, automargin = TRUE),
      margin = list(l = 220, r = 20, t = 40, b = 50),
      hovermode = "closest",
      annotations = if (!lod_full) list(list(xref = "paper", yref = "paper", x = 0.01, y = 1.03, text = lod_note, showarrow = FALSE, xanchor = "left", font = list(size = 11))) else NULL
    )
	    raster_progress(0.92, ui_inline_text("\u6B63\u5728\u5E94\u7528 Plotly \u5E03\u5C40\u548C\u4EA4\u4E92\u4E8B\u4EF6", "Applying the Plotly layout and interactions"))
	    p <- event_register(p, "plotly_selected")
	    p <- event_register(p, "plotly_relayout")
	    raster_progress(1, ui_inline_text("\u56FE\u5F62\u89C6\u56FE\u5DF2\u5B8C\u6210", "Plot view complete"), "success")
	    if (isTRUE(show_raster_progress)) rv$raster_plot_progress_active <- FALSE
	    config(p, displaylogo = FALSE)
    }
    if (isTRUE(show_raster_progress)) {
      withProgress(message = ui_inline_text("\u6B63\u5728\u52A0\u8F7D\u56FE\u5F62\u89C6\u56FE", "Loading plot view"), value = 0, {
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
      return(stpd_empty_plotly_message(ui_inline_text("\u8BF7\u5148\u4E0A\u4F20\u4E00\u4E2A\u6570\u636E\u96C6\u3002", "Upload a dataset first.")))
    }
    withProgress(message = ui_inline_text("\u6B63\u5728\u52A0\u8F7D\u539F\u59CB\u65F6\u95F4\u6233\u56FE\u5F62", "Loading raw-timestamp plot"), value = 0, {
      setProgress(0.05, detail = ui_inline_text("\u6B63\u5728\u51C6\u5907\u539F\u59CB timestamp \u6570\u636E", "Preparing raw-timestamp data"))
      ds <- current_dataset()
      td <- ds$trains
      axis_tbl <- selected_axis_table() %>% dplyr::arrange(y)
      if (nrow(axis_tbl) == 0L) return(stpd_empty_plotly_message(ui_inline_text("\u8BF7\u81F3\u5C11\u9009\u62E9\u4E00\u6761 train\u3002", "Select at least one train.")))
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
        no_raw_data <- ui_inline_text("\u5F53\u524D\u539F\u59CB\u65F6\u95F4\u7A97\u6CA1\u6709 spike/ISI\u3002", "No spike/ISI data are visible in the current raw-time window.")
        setProgress(1, detail = no_raw_data)
        return(stpd_empty_plotly_message(no_raw_data))
      }

      setProgress(0.32, detail = ui_inline_text("\u6B63\u5728\u540C\u6B65\u6A21\u5F0F\u6807\u7B7E\u5230\u539F\u59CB\u65F6\u95F4", "Aligning pattern labels to raw time"))
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
          label_source_html = stpd_html_escape(ifelse(label_source == "none", ui_inline_text("\u65E0", "none"), label_source))
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
            ui_inline_text("Train\uFF1A", "Train: "), train_label_html,
            ui_inline_text("<br>Spike \u7D22\u5F15\uFF1A", "<br>Spike index: "), idx,
            ui_inline_text("<br>\u539F\u59CB\u65F6\u95F4\u6233\uFF1A", "<br>Raw timestamp: "), round(timestamp_sec, 6), " s"
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
        add_gate_b_tracks <- function(pp, df) {
          authoritative <- df[df$v2_authoritative %in% TRUE, , drop = FALSE]
          if (nrow(authoritative) == 0L) return(pp)
          event <- authoritative[authoritative$v2_event_family == "burst", , drop = FALSE]
          if (nrow(event) > 0L) {
            event$y <- event$y + 0.12
            pp <- add_pattern_strips(pp, event, "burst", "auto")
          }
          for (pat in c("tonic", "high_frequency_tonic", "high_frequency_spiking")) {
            state <- authoritative[authoritative$v2_state_class == pat, , drop = FALSE]
            if (nrow(state) > 0L) pp <- add_pattern_strips(pp, state, pat, "auto")
          }
          gap <- authoritative[authoritative$v2_gap_class == "pause", , drop = FALSE]
          if (nrow(gap) > 0L) {
            gap$y <- gap$y - 0.12
            pp <- add_pattern_strips(pp, gap, "pause", "auto")
          }
          pp
        }
        if (identical(input$pattern_view, "final")) {
          if (any(visible_isi$v2_authoritative %in% TRUE)) p <- add_gate_b_tracks(p, visible_isi)
          else for (pat in pats) {
              sub_pat <- visible_isi[visible_isi$pattern_show == pat, , drop = FALSE]
              if (nrow(sub_pat) > 0L) p <- add_pattern_strips(p, sub_pat, pat, "auto")
            }
        } else if (identical(input$pattern_view, "manual")) {
          for (pat in pats) {
            sub_pat <- visible_isi[visible_isi$pattern_manual == pat, , drop = FALSE]
            if (nrow(sub_pat) > 0L) p <- add_pattern_strips(p, sub_pat, pat, "manual")
          }
        } else if (identical(input$pattern_view, "auto")) {
          if (any(visible_isi$v2_authoritative %in% TRUE)) p <- add_gate_b_tracks(p, visible_isi)
          else for (pat in pats) {
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
            ui_inline_text("Train\uFF1A", "Train: "), train_label_html,
            ui_inline_text("<br>\u5DE6\u4FA7 spike \u539F\u59CB timestamp\uFF1A", "<br>Left spike raw timestamp: "), round(timestamp_left_sec, 6), " s",
            ui_inline_text("<br>\u53F3\u4FA7 spike \u539F\u59CB timestamp\uFF1A", "<br>Right spike raw timestamp: "), round(timestamp_sec, 6), " s",
            ifelse(is.na(ISI_sec), "<br>ISI: NA", paste0("<br>ISI: ", signif(ISI_sec, 6), " s (", round(ISI_plot, 3), " ", u, ")")),
            ifelse(is.na(ISI_pct), "", paste0(ui_inline_text("<br>ISI \u5728\u672C train \u4E2D\u7684\u767E\u5206\u4F4D\uFF1A", "<br>ISI percentile in this train: "), round(ISI_pct, 2), "%")),
            ui_extended_isi_metrics_hover(
              ISI_range_pct_linear,
              ISI_range_pct_log,
              ISI_robust_range_pct_log,
              show = isTRUE(input$show_extended_isi_metrics)
            ),
            ui_inline_text("<br>\u6700\u7EC8\u6807\u7B7E\uFF1A", "<br>Final label: "), ifelse(pattern_final == "", ui_inline_text("\u65E0", "none"), pattern_final_html),
            ui_inline_text("<br>\u6700\u7EC8\u5BA1\u8BA1\u6807\u7B7E\uFF1A", "<br>Final audit label: "), ifelse(pattern_audit_final == "", ui_inline_text("\u65E0", "none"), pattern_audit_final_html),
            ui_inline_text("<br>\u6807\u7B7E\u6765\u6E90\uFF1A", "<br>Label source: "), label_source_html
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
          ui_inline_text("\u4EFB\u52A1/\u884C\u4E3A\u4E8B\u4EF6<br>", "Task/behavior event<br>"),
          ui_inline_text("\u4E8B\u4EF6\uFF1A", "Event: "), stpd_html_escape(raw_events$event_name),
          ui_inline_text("<br>\u539F\u59CB\u65F6\u95F4\u6233\uFF1A", "<br>Raw timestamp: "), signif(raw_events$event_time_sec, 7), " s"
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
        xaxis = list(title = ui_inline_text(paste0("\u539F\u59CB\u65F6\u95F4\u6233\uFF08", u, "\uFF09"), paste0("Raw timestamp (", u, ")")), range = draw_sec * f),
        yaxis = list(title = list(text = ui_inline_text("Spike train\uFF08\u8BB0\u5F55\u6761\u76EE\uFF09", "Spike train (recording item)"), standoff = 40), tickmode = "array",
                     tickvals = y_tickvals, ticktext = y_ticktext, tickfont = list(size = tick_font_size),
                     range = y_range, zeroline = FALSE, automargin = TRUE),
        margin = list(l = 220, r = 20, t = 40, b = 50),
        hovermode = "closest"
      )
      setProgress(1, detail = ui_inline_text("\u539F\u59CB\u65F6\u95F4\u6233\u56FE\u5F62\u5DF2\u5B8C\u6210", "Raw-timestamp plot complete"))
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
      selected <- as.character(ui_control_remembered("isi_profile_train", default[1]))[1]
      if (!selected %in% choices) selected <- default[1]
      return(selectInput("isi_profile_train", ui_inline_text("\u805A\u7126 train", "Focused train"), choices = choices, selected = selected))
    }
    max_n <- safe_int(input$isi_profile_max_trains, 8L)
    max_n <- max(1L, min(10L, max_n))
    selected <- intersect(
      as.character(ui_control_remembered("isi_profile_trains_multi", head(default, max_n))),
      choices
    )
    selectizeInput("isi_profile_trains_multi", ui_inline_text("\u9009\u62E9\u4E00\u6761\u6216\u591A\u6761 train\uFF08\u6BCF\u6761\u72EC\u7ACB\u9762\u677F\uFF09", "Select one or more trains (one panel per train)"),
                   choices = choices,
                   selected = head(selected, max_n),
                   multiple = TRUE,
                   options = list(maxItems = 10, placeholder = ui_inline_text("\u8BF7\u624B\u52A8\u9009\u62E9\u4E00\u6761\u6216\u591A\u6761 train", "Select one or more trains for separate panels")))
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
    range_value <- suppressWarnings(as.numeric(ui_control_remembered(
      "isi_profile_custom_range", c(min_plot, default_end)
    )))
    if (length(range_value) != 2L || any(!is.finite(range_value))) {
      range_value <- c(min_plot, default_end)
    }
    range_value <- pmax(min_plot, pmin(max_plot, sort(range_value)))
    sliderInput("isi_profile_custom_range", ui_inline_text("\u81EA\u5B9A\u4E49\u5256\u9762\u65F6\u95F4\u6233\u7A97\u53E3\uFF08s\uFF09", "Custom profile timestamp window (s)"),
                min = min_plot, max = max_plot, value = range_value, step = 0.001)
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
    validate(need(length(sel) > 0, ui_inline_text("\u8BF7\u81F3\u5C11\u9009\u62E9\u4E00\u6761 spike train \u7528\u4E8E ISI \u5256\u9762\u3002", "Select at least one spike train for the ISI profile.")))
    out <- bind_rows(lapply(sel, function(tr) build_isi_profile_train_data(tr, td[[tr]])))
    validate(need(nrow(out) > 0, ui_inline_text("\u6240\u9009 train \u6CA1\u6709\u6709\u6548 ISI\u3002", "The selected train(s) contain no valid ISIs.")))
    out
  })

  # Resolved thresholds from the most recent detector run.  This is kept
  # separate from train-specific editable anchors so the profile can show the
  # values that actually governed the detector, not merely the values currently
  # typed into the sidebar controls.
  isi_profile_detector_threshold_rows <- reactive({
    ds <- tryCatch(current_dataset(), error = function(e) NULL)
    tab <- if (!is.null(ds)) ds$results$threshold_table %||% data.frame() else data.frame()
    if (!is.data.frame(tab) || nrow(tab) == 0 ||
        !all(c("pattern", "field", "effective_sec") %in% names(tab))) return(data.frame())
    tab$effective_sec <- suppressWarnings(as.numeric(tab$effective_sec))
    tab <- tab[is.finite(tab$effective_sec) & tab$effective_sec > 0, , drop = FALSE]
    if (nrow(tab) == 0) return(data.frame())
    field_labels <- c(
      seed_lower_sec = ui_inline_text("\u4E0B\u754C", "lower bound"),
      seed_upper_sec = ui_inline_text("\u4E0A\u754C", "upper bound"),
      bridge_upper_sec = ui_inline_text("\u6865\u63A5\u4E0A\u754C", "bridge upper"),
      contrast_S = ui_inline_text("\u5BF9\u6BD4\u5EA6 S", "contrast S")
    )
    tab$pattern <- as.character(tab$pattern)
    tab$pattern_label <- vapply(tab$pattern, stpd_ui_pattern_display, character(1))
    tab$field_label <- unname(field_labels[as.character(tab$field)])
    tab$field_label[is.na(tab$field_label) | !nzchar(tab$field_label)] <- as.character(tab$field[is.na(tab$field_label) | !nzchar(tab$field_label)])
    tab
  })

  output$isi_profile_threshold_pattern_ui <- renderUI({
    tab <- isi_profile_detector_threshold_rows()
    all_patterns <- c("burst", "long_burst", "possible_burst", "tonic",
                      "high_frequency_tonic", "high_frequency_spiking", "pause", "others")
    available <- unique(c(as.character(tab$pattern), all_patterns))
    if (length(available) == 0) return(NULL)
    old <- intersect(as.character(input$isi_profile_threshold_patterns %||% character(0)), available)
    selected <- if (length(old) > 0) old else available
    checkboxGroupInput(
      "isi_profile_threshold_patterns",
      ui_inline_text("\u663E\u793A\u6A21\u5F0F / \u9608\u503C\u7EBF", "Patterns / threshold lines to show"),
      choices = setNames(available, vapply(available, stpd_ui_pattern_display, character(1))),
      selected = selected,
      inline = FALSE
    )
  })

  output$isi_profile_detector_thresholds_table <- DT::renderDT({
    tab <- isi_profile_detector_threshold_rows()
    if (nrow(tab) == 0) {
      return(datatable(
        data.frame(message = ui_inline_text("\u68C0\u6D4B\u7ED3\u679C\u9608\u503C\u5C06\u5728\u5B8C\u6210\u4E00\u6B21\u68C0\u6D4B\u540E\u663E\u793A\u3002", "Detector thresholds appear after a completed run.")),
        options = list(dom = "t", pageLength = 4), rownames = FALSE
      ))
    }
    selected <- as.character(input$isi_profile_threshold_patterns %||% unique(tab$pattern))
    tab <- tab[tab$pattern %in% selected, , drop = FALSE]
    if (nrow(tab) == 0) {
      return(datatable(
        data.frame(message = ui_inline_text("\u672A\u9009\u62E9\u8981\u663E\u793A\u7684\u68C0\u6D4B\u6A21\u5F0F\u3002", "No detector pattern is selected.")),
        options = list(dom = "t", pageLength = 4), rownames = FALSE
      ))
    }
    u <- input$time_unit %||% "ms"
    f <- unit_factor()
    out <- data.frame(
      mode = tab$pattern_label,
      threshold = tab$field_label,
      value = round(tab$effective_sec * f, 6),
      unit = u,
      source = as.character(tab$source %||% ""),
      stringsAsFactors = FALSE
    )
    names(out) <- c(
      ui_inline_text("\u6A21\u5F0F", "Pattern"),
      ui_inline_text("\u9608\u503C\u7C7B\u578B", "Threshold"),
      ui_inline_text("\u6570\u503C", "Value"),
      ui_inline_text("\u5355\u4F4D", "Unit"),
      ui_inline_text("\u6765\u6E90", "Source")
    )
    datatable(out, options = list(dom = "t", pageLength = 8, scrollX = TRUE), rownames = FALSE)
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
    if (is.null(ref) || !is.finite(ref$isi_sec)) return(ui_inline_text("\u6CA1\u6709\u9501\u5B9A\u7684\u53C2\u8003 ISI\u3002", "No reference ISI is locked."))
    ui_inline_text(
      paste0("\u53C2\u8003 train\uFF1A", ref$train, "\nISI idx\uFF1A", ref$idx, "\n\u53C2\u8003 ISI\uFF1A", signif(ref$isi_sec, 6), " s"),
      paste0("Reference train: ", ref$train, "\nISI index: ", ref$idx, "\nReference ISI: ", signif(ref$isi_sec, 6), " s")
    )
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
      selectizeInput("isi_threshold_apply_trains", ui_inline_text("\u81EA\u9009\u5E94\u7528 train(s)", "Custom target train(s)"),
                     choices = choices, selected = selected, multiple = TRUE,
                     options = list(placeholder = ui_inline_text("\u9009\u62E9\u8981\u5E94\u7528\u8FD9\u4E9B\u9608\u503C\u7EBF\u7684 spike train", "Select spike trains that will receive these threshold lines"), plugins = list("remove_button")))
    })

    isi_profile_ref_to_threshold <- function(input_id) {
      ref <- rv$isi_profile_ref
      if (is.null(ref) || !is.finite(ref$isi_sec) || ref$isi_sec <= 0) {
        showNotification(ui_inline_text("\u8BF7\u5148\u5728 ISI \u65F6\u95F4\u5256\u9762\u56FE\u4E2D\u70B9\u51FB\u4E00\u4E2A ISI \u4F5C\u4E3A\u53C2\u8003\u7EBF\u3002", "Click an ISI in the temporal-profile plot to set a reference line first."), type = "warning", duration = 6)
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
	    if (length(targets) == 0) return(showNotification(ui_inline_text("\u672A\u9009\u62E9\u5256\u9762 train\u3002", "No profile train is selected."), type = "warning"))
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
        showNotification(ui_inline_text("\u5DF2\u81EA\u52A8\u5C06 tonic \u4E0B\u754C\u63D0\u9AD8\u5230 burst \u9608\u503C\u7EBF\u4E4B\u4E0A\uFF0C\u907F\u514D tonic \u4E0E burst ISI \u533A\u95F4\u4EA4\u53C9\u3002", "The tonic lower bound was raised above the burst line automatically to prevent overlapping tonic and burst ISI ranges."), type = "warning", duration = 8)
      }
      if (tmin > 0 && tmax > 0 && tmax <= tmin) {
        showNotification(ui_inline_text("tonic \u6700\u5927 ISI \u5FC5\u987B\u5927\u4E8E tonic \u6700\u5C0F ISI\u3002", "The tonic maximum ISI must exceed the tonic minimum ISI."), type = "error", duration = 8)
        return(invisible(NULL))
      }
      if (bmax <= 0 && pmin <= 0 && tmin <= 0 && tmax <= 0) {
        showNotification(ui_inline_text("\u8BF7\u81F3\u5C11\u8BBE\u7F6E\u4E00\u6761 burst/tonic/pause \u9608\u503C\u7EBF\u3002", "Set at least one burst/tonic/pause threshold line."), type = "warning", duration = 6)
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
      showNotification(ui_inline_text(
        paste0("\u5DF2\u4E3A ", length(targets), " \u6761 train \u4FDD\u5B58 ISI \u9608\u503C\u7EBF\uFF08", mode, "\uFF09\u3002"),
        paste0("Saved ISI threshold lines for ", length(targets), " train(s) (", mode, ").")
      ), type = "message")
      if (isTRUE(run_after) || isTRUE(input$run_detector_after_train_isi_thresholds)) {
        runner <- get0("run_detector_from_ui", mode = "function", inherits = TRUE)
        if (is.function(runner)) {
          runner(message = ui_inline_text("\u6B63\u5728\u6309 ISI \u9608\u503C\u7EBF\u91CD\u8DD1\u68C0\u6D4B\u5668", "Rerunning the detector with the ISI threshold lines"), switch_to_plot = TRUE, notify = TRUE,
                 target_trains_override = targets)
        } else {
          showNotification(ui_inline_text("\u9608\u503C\u5DF2\u4FDD\u5B58\uFF0C\u4F46\u5F53\u524D session \u4E2D\u6CA1\u6709\u53EF\u7528\u7684\u68C0\u6D4B\u5668 runner\u3002", "Thresholds were saved, but no detector runner is available in the current session."), type = "warning", duration = 8)
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
    if (length(targets) == 0) return(showNotification(ui_inline_text("\u672A\u9009\u62E9\u5256\u9762 train\u3002", "No profile train is selected."), type = "warning"))
    if (is.null(ds$train_settings)) ds$train_settings <- list()
    if (is.null(ds$train_settings$isi_thresholds)) ds$train_settings$isi_thresholds <- list()
    for (tr in targets) ds$train_settings$isi_thresholds[[tr]] <- NULL
    set_dataset(rv$current_id, ds)
    showNotification(ui_inline_text(paste0("\u5DF2\u6E05\u9664 ", length(targets), " \u6761 train \u7684\u4E13\u5C5E ISI \u9608\u503C\u3002"), paste0("Cleared train-specific ISI thresholds for ", length(targets), " train(s).")), type = "message")
  }, ignoreInit = TRUE)

  observeEvent(input$clear_all_train_isi_thresholds, {
    ds <- get_dataset(); if (is.null(ds)) return()
    ds$train_settings$isi_thresholds <- list()
    set_dataset(rv$current_id, ds)
    showNotification(ui_inline_text("\u5DF2\u6E05\u9664\u6240\u6709\u5355 train \u4E13\u5C5E ISI \u9608\u503C\u3002", "Cleared all train-specific ISI thresholds."), type = "message")
  }, ignoreInit = TRUE)

  output$train_isi_thresholds_table <- DT::renderDT({
    ds <- current_dataset()
    f <- unit_factor(); u <- input$time_unit %||% "ms"
    dat <- train_isi_threshold_dataframe(ds$train_settings$isi_thresholds %||% list(), factor = f, unit = u)
    datatable(dat, options = list(pageLength = 6, scrollX = TRUE), rownames = FALSE)
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
        x_title <- ui_inline_text("ISI \u987A\u5E8F\u7D22\u5F15", "ISI sequence index")
        x_range <- NULL
      } else {
        # Use absolute spike timestamps on the ISI temporal-profile X-axis.
        # This axis is intentionally fixed in seconds so that the plotted X
        # coordinate matches the timestamp values shown in the hover tooltip.
        # The global Display unit still controls the ISI Y-axis.
        dat$x0 <- dat$left_time_sec
        dat$x1 <- dat$right_time_sec
        dat$x_mid <- dat$mid_time_sec
        x_title <- ui_inline_text("Spike timestamp\uFF08s\uFF09", "Spike timestamp (s)")
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
        ui_inline_text("Train\uFF1A", "Train: "), stpd_html_escape(dat$train),
        ui_inline_text("<br>\u5DE6\u4FA7 spike \u65F6\u95F4\u6233\uFF1A", "<br>Left spike timestamp: "), round(dat$left_time_sec, 6), " s",
        ui_inline_text("<br>\u53F3\u4FA7 spike \u65F6\u95F4\u6233\uFF1A", "<br>Right spike timestamp: "), round(dat$right_time_sec, 6), " s",
        "<br>ISI\uFF1A", signif(dat$ISI_sec, 6), " s (", round(dat$ISI_plot, 4), " ", u, ")",
        ifelse(is.finite(dat$ISI_pct), paste0(ui_inline_text("<br>ISI \u5728\u672C train \u4E2D\u7684\u767E\u5206\u4F4D\uFF1A", "<br>ISI percentile in this train: "), round(dat$ISI_pct, 2), "%"), ""),
        ui_extended_isi_metrics_hover(
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
	            baseline_pattern <- if (nzchar(dd$delta_baseline_pattern[1])) dd$delta_baseline_pattern[1] else ui_inline_text("\u65E0", "none")
	            current_pattern <- if (nzchar(dd$delta_current_pattern[1])) dd$delta_current_pattern[1] else ui_inline_text("\u65E0", "none")
	            dd$delta_text <- paste0(
	              ui_inline_text("\u53C2\u6570\u8BD5\u8FD0\u884C\u5DEE\u5F02<br>", "Parameter dry-run difference<br>"),
	              ui_inline_text("\u72B6\u6001\uFF1A", "Status: "), stpd_html_escape(dd$delta_status),
	              ui_inline_text("<br>Train\uFF1A", "<br>Train: "), stpd_html_escape(tr),
	              ui_inline_text("<br>\u57FA\u7EBF\uFF1A", "<br>Baseline: "), stpd_html_escape(baseline_pattern),
	              ui_inline_text("<br>\u5F53\u524D\uFF1A", "<br>Current: "), stpd_html_escape(current_pattern),
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
        selected_pats <- intersect(as.character(input$isi_profile_threshold_patterns %||% shade_pats), shade_pats)
        if (length(selected_pats) == 0) selected_pats <- shade_pats
        # Pattern is the visual encoding; train identity is carried by the
        # panel title and selection, never by a train-specific line colour.
        col_map <- c(burst = "rgba(214,39,40,0.42)", long_burst = "rgba(166,54,3,0.42)", possible_burst = "rgba(255,127,14,0.42)",
                     tonic = "rgba(44,160,44,0.42)", high_frequency_tonic = "rgba(23,190,207,0.42)", high_frequency_spiking = "rgba(148,103,189,0.42)", pause = "rgba(31,119,180,0.42)", others = "rgba(120,120,120,0.30)")
        pattern_label <- function(pat) {
          if (identical(ui_language(), "en")) {
            out <- c(burst = "Burst", long_burst = "Long burst", possible_burst = "Possible burst",
              tonic = "Tonic", high_frequency_tonic = "High-frequency tonic",
              high_frequency_spiking = "High-frequency spiking", pause = "Pause", others = "Other")[pat]
            if (is.na(out) || !nzchar(out)) pat else unname(out)
          } else {
            stpd_ui_pattern_display(pat)
          }
        }
        for (pat in shade_pats) {
          if (!pat %in% selected_pats) next
          sub <- dat[dat$pattern_final == pat, , drop = FALSE]
          if (nrow(sub) == 0) next
          pp <- add_segments(pp, data = sub, x = ~x0, xend = ~x1, y = ~ISI_plot, yend = ~ISI_plot,
                             type = "scatter", mode = "lines",
                             line = list(width = 10, color = col_map[[pat]] %||% "rgba(120,120,120,0.08)"),
                             hoverinfo = "none", showlegend = FALSE, inherit = FALSE)
        }
        # Add a stable pattern legend once.  Dummy traces keep the legend
        # complete even when the first selected train lacks one pattern.
        if (identical(as.character(tr), as.character(trains[1]))) {
          for (pat in selected_pats) {
            pp <- add_trace(pp, x = NA_real_, y = NA_real_, type = "scatter", mode = "lines",
                            line = list(width = 8, color = col_map[[pat]]),
                            name = pattern_label(pat), hoverinfo = "none", showlegend = TRUE,
                            inherit = FALSE)
          }
        }
      }
      if (isTRUE(input$isi_profile_show_thresholds)) {
	        ds_thr <- tryCatch(current_dataset(), error = function(e) NULL)
	        thr <- if (!is.null(ds_thr)) ds_thr$train_settings$isi_thresholds[[tr]] %||% list() else list()
          thr_hard <- stpd_train_isi_threshold_is_hard(thr)
          thr_dash <- if (isTRUE(thr_hard)) "solid" else "dash"
          thr_width <- if (isTRUE(thr_hard)) 2.2 else 1.5
          thr_suffix <- if (isTRUE(thr_hard)) {
            ui_inline_text("\u786C\u9608\u503C", "hard threshold")
          } else {
            ui_inline_text("\u8F6F\u951A\u70B9", "soft anchor")
          }
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
	        pp <- add_thr_line(pp, thr$burst_max_sec %||% 0, stpd_ui_pattern_color("burst", "auto"), thr_dash, ui_inline_text("burst \u6700\u5927 ISI", "burst maximum ISI"), thr_width)
	        pp <- add_thr_line(pp, thr$pause_min_sec %||% 0, stpd_ui_pattern_color("pause", "auto"), thr_dash, ui_inline_text("pause \u6700\u5C0F ISI", "pause minimum ISI"), thr_width)
	        pp <- add_thr_line(pp, thr$tonic_min_sec %||% 0, stpd_ui_pattern_color("tonic", "auto"), if (isTRUE(thr_hard)) "solid" else "dot", ui_inline_text("tonic \u6700\u5C0F ISI", "tonic minimum ISI"), thr_width)
	        pp <- add_thr_line(pp, thr$tonic_max_sec %||% 0, stpd_ui_pattern_color("tonic", "auto"), if (isTRUE(thr_hard)) "solid" else "dot", ui_inline_text("tonic \u6700\u5927 ISI", "tonic maximum ISI"), thr_width)

        # Overlay the thresholds actually used by the last detector run.  The
        # train-specific lines above are editable UI anchors; these lines are
        # the resolved event-grammar values written into results$threshold_table
        # and therefore make the plotted pattern calls auditable.  Keep them
        # visually distinct so a soft/manual train anchor is not mistaken for
        # the detector's resolved global threshold.
        detector_tab <- if (!is.null(ds_thr)) ds_thr$results$threshold_table %||% data.frame() else data.frame()
        if (is.data.frame(detector_tab) && nrow(detector_tab) > 0 &&
            all(c("pattern", "field", "effective_sec") %in% names(detector_tab))) {
          detector_tab$effective_sec <- suppressWarnings(as.numeric(detector_tab$effective_sec))
          detector_tab <- detector_tab[is.finite(detector_tab$effective_sec) & detector_tab$effective_sec > 0, , drop = FALSE]
          detector_specs <- list(
            burst = c(seed_upper_sec = "seed upper", bridge_upper_sec = "bridge upper"),
            possible_burst = c(seed_upper_sec = "seed upper", bridge_upper_sec = "bridge upper"),
            tonic = c(seed_lower_sec = "minimum", seed_upper_sec = "maximum"),
            high_frequency_tonic = c(seed_lower_sec = "minimum", seed_upper_sec = "maximum"),
            high_frequency_spiking = c(seed_upper_sec = "q90 upper", bridge_upper_sec = "epoch bridge"),
            pause = c(seed_lower_sec = "minimum", seed_upper_sec = "strong upper")
          )
          detector_label <- function(pat, field, desc) {
            pat_label <- if (identical(ui_language(), "en")) {
              c(burst = "Burst", possible_burst = "Possible burst", tonic = "Tonic",
                high_frequency_tonic = "High-frequency tonic", high_frequency_spiking = "High-frequency spiking",
                pause = "Pause")[pat] %||% pat
            } else stpd_ui_pattern_display(pat)
            paste0(ui_inline_text("\u68C0\u6D4B\u7ED3\u679C\u00B7", "Detector result \u00B7 "), pat_label, " ",
                   ui_inline_text(desc, desc))
          }
          detector_dash <- c(burst = "dash", possible_burst = "dashdot", tonic = "dot",
                             high_frequency_tonic = "dot", high_frequency_spiking = "longdash", pause = "dash")
          for (pat in names(detector_specs)) {
            if (!pat %in% selected_pats) next
            for (field in names(detector_specs[[pat]])) {
              row <- detector_tab[detector_tab$pattern == pat & detector_tab$field == field, , drop = FALSE]
              if (nrow(row) == 0) next
              pp <- add_thr_line(
                pp, row$effective_sec[1], stpd_ui_pattern_color(if (pat == "possible_burst") "burst" else pat, "auto"),
                detector_dash[[pat]] %||% "dash", detector_label(pat, field, unname(detector_specs[[pat]][field])), width = 1.35
              )
            }
          }
        }
	      }

      pp <- add_segments(pp, data = dat, x = ~x0, xend = ~x1, y = ~ISI_plot, yend = ~ISI_plot,
                         type = "scatter", mode = "lines",
                         line = list(width = 1.5, color = "#374151"),
                         hoverinfo = "text", text = ~hover_text,
                         customdata = ~custom_payload,
                         showlegend = FALSE, inherit = FALSE)
	      pp <- add_markers(pp, data = dat, x = ~x_mid, y = ~ISI_plot,
	                        marker = list(size = 4, color = "#374151"),
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
	                               hoverinfo = "text", text = ~paste0(delta_text, ui_inline_text("<br>\u8868\u683C\u9009\u4E2D\u884C", "<br>Selected table row")),
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
                            hoverinfo = "text", text = ~paste0(hover_text, ui_inline_text("<br>\u4E0E\u9501\u5B9A\u53C2\u8003\u503C\u76F8\u4F3C", "<br>Similar to locked reference")),
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
    validate(need(length(panels) > 0, ui_text("no_valid_profile_isi")))
    if (length(panels) == 1) {
      return(config(event_register(panels[[1]], "plotly_click"), displaylogo = FALSE))
    }
    subplot(panels, nrows = length(panels), shareX = FALSE, shareY = FALSE, margin = 0.03, titleY = TRUE) %>%
      event_register("plotly_click") %>%
      layout(hoverlabel = stpd_hoverlabel_style(), title = ui_inline_text("ISI \u65F6\u95F4\u5256\u9762\uFF1A\u6BCF\u6761 spike train \u4F7F\u7528\u72EC\u7ACB timestamp \u8F74", "ISI temporal profile: independent timestamp axis for each spike train"), showlegend = isTRUE(input$isi_profile_show_labels)) %>%
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
    selected <- as.character(ui_control_remembered("isi_state_space_train", default[1]))[1]
    if (!selected %in% choices) selected <- default[1]
    selectizeInput(
      "isi_state_space_train",
      ui_inline_text("\u5355\u6761 spike train", "Single spike train"),
      choices = choices,
      selected = selected,
      multiple = FALSE,
      options = list(placeholder = ui_inline_text("\u9009\u62E9\u4E00\u6761 train \u8FDB\u884C PCA / \u76F8\u56FE\u5206\u6790", "Select a train for PCA / phase portrait"))
    )
  })

  isi_state_space_selected_train <- reactive({
    td <- current_trains()
    choices <- names(td)
    validate(need(length(choices) > 0, ui_inline_text("\u8BF7\u5148\u4E0A\u4F20\u6570\u636E\u3002", "Upload data first.")))
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
    range_value <- suppressWarnings(as.numeric(ui_control_remembered(
      "isi_state_space_custom_range",
      c(min_t, min(max_t, min_t + default_width))
    )))
    if (length(range_value) != 2L || any(!is.finite(range_value))) {
      range_value <- c(min_t, min(max_t, min_t + default_width))
    }
    range_value <- pmax(min_t, pmin(max_t, sort(range_value)))
    sliderInput(
      "isi_state_space_custom_range",
      ui_inline_text("\u81EA\u5B9A\u4E49 timestamp \u7A97\u53E3\uFF08s\uFF09", "Custom timestamp window (s)"),
      min = min_t,
      max = max_t,
      value = range_value,
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
    validate(need(tr %in% names(td), ui_inline_text("\u8BF7\u9009\u62E9\u6709\u6548 train\u3002", "Select a valid train.")))
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
    validate(need(nrow(feats) >= 3, ui_inline_text("\u5F53\u524D train / \u65F6\u95F4\u7A97\u5185\u6709\u6548 ISI \u592A\u5C11\uFF0C\u65E0\u6CD5\u8FDB\u884C PCA\u3002", "The current train/time window has too few valid ISIs for PCA.")))
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
    validate(need(nrow(feats) >= 20, ui_inline_text("\u5F53\u524D train / \u65F6\u95F4\u7A97\u5185\u6709\u6548 ISI \u592A\u5C11\uFF0CIsomap \u81F3\u5C11\u9700\u8981 20 \u4E2A\u70B9\u3002", "The current train/time window has too few valid ISIs; Isomap requires at least 20 points.")))
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
        validate(need(FALSE, paste(ui_inline_text("Isomap \u8BA1\u7B97\u5931\u8D25\u3002", "Isomap computation failed."), ui_condition_detail(e))))
      }
    )
  })

  isi_state_space_phase_data <- reactive({
    td <- current_trains()
    tr <- isi_state_space_selected_train()
    validate(need(tr %in% names(td), ui_inline_text("\u8BF7\u9009\u62E9\u6709\u6548 train\u3002", "Select a valid train.")))
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
    validate(need(nrow(ph) >= 2, ui_inline_text("\u5F53\u524D train / \u65F6\u95F4\u7A97\u5185\u76F8\u90BB ISI \u5BF9\u592A\u5C11\u3002", "The current train/time window contains too few adjacent ISI pairs.")))
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
	        prepost_ratio = "\u524D/\u540E ISI \u6BD4\u503C",
	        delta_logisi = "\u0394 logISI",
	        next_delta_logisi = "\u4E0B\u4E00\u6B65 \u0394 logISI"
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
    validate(need(nrow(dd) >= 2, ui_inline_text("\u65E0 PCA \u5F97\u5206\u3002", "No PCA scores are available.")))
    dd <- isi_state_space_enrich_scores(dd)
    list(res = res, scores = dd, unit = input$time_unit %||% "ms")
  })

  output$isi_state_space_pca_plot <- renderPlotly({
    bundle <- isi_state_space_pca_plot_bundle()
    res <- bundle$res
    dd <- bundle$scores
    validate(need(nrow(dd) >= 2, ui_inline_text("\u65E0 PCA \u5F97\u5206\u3002", "No PCA scores are available.")))
    x_col <- input$isi_state_space_x_axis %||% "PC1"
    y_col <- input$isi_state_space_y_axis %||% "PC2"
    validate(need(all(c(x_col, y_col) %in% names(dd)), ui_inline_text("\u6240\u9009 2D \u8F74\u5728\u5F53\u524D\u6570\u636E\u4E2D\u4E0D\u5B58\u5728\u3002", "The selected 2D axes are not present in the current data.")))
    ok <- is.finite(dd[[x_col]]) & is.finite(dd[[y_col]])
    dd <- dd[ok, , drop = FALSE]
    validate(need(nrow(dd) >= 2, ui_inline_text("\u6240\u9009 2D \u8F74\u7684\u6709\u6548\u70B9\u592A\u5C11\u3002", "Too few valid points remain for the selected 2D axes.")))
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
      title = paste0(ui_inline_text("ISI-PCA \u8F68\u8FF9", "ISI-PCA trajectory"), if (nzchar(subtitle)) paste0("<br><sup>", subtitle, " | ", isi_state_space_short_label(unique(dd$train)[1]), "</sup>") else ""),
      x_title = isi_state_space_axis_title(x_col),
      y_title = isi_state_space_axis_title(y_col),
      legend_y = -0.2,
      margin = list(l = 65, r = 18, t = 78, b = 98)
    ) %>% config(displaylogo = FALSE)
  })

	  output$isi_state_space_isomap_plot <- renderPlotly({
	    res <- isi_state_space_isomap_result()
	    dd <- res$scores
	    validate(need(nrow(dd) >= 2, ui_inline_text("\u65E0 Isomap \u5F97\u5206\u3002", "No Isomap scores are available.")))
	    dd <- isi_state_space_enrich_scores(dd)
	    x_col <- input$isi_state_space_isomap_x_axis %||% "Isomap1"
	    y_col <- input$isi_state_space_isomap_y_axis %||% "Isomap2"
	    validate(need(all(c(x_col, y_col) %in% names(dd)), ui_inline_text("\u6240\u9009 Isomap 2D \u8F74\u5728\u5F53\u524D\u6570\u636E\u4E2D\u4E0D\u5B58\u5728\u3002", "The selected Isomap 2D axes are not present in the current data.")))
	    ok <- is.finite(dd[[x_col]]) & is.finite(dd[[y_col]])
	    dd <- dd[ok, , drop = FALSE]
	    validate(need(nrow(dd) >= 2, ui_inline_text("Isomap \u6709\u6548\u70B9\u592A\u5C11\u3002", "Too few valid Isomap points are available.")))
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
      if (isTRUE(isi_state_space_is_english())) {
        paste0(
          "k=", diag$n_neighbors[1],
          "; embedded ", diag$n_embedded[1], "/", diag$n_input[1],
          "; components=", diag$component_n[1],
          "; residual variance=", ifelse(is.finite(diag$residual_variance[1]), round(diag$residual_variance[1], 3), "NA")
        )
      } else {
        paste0(
          "k=", diag$n_neighbors[1],
          "\uFF1B\u5D4C\u5165 ", diag$n_embedded[1], "/", diag$n_input[1],
          "\uFF1B\u8FDE\u901A\u5206\u91CF=", diag$component_n[1],
          "\uFF1B\u6B8B\u5DEE\u65B9\u5DEE=", ifelse(is.finite(diag$residual_variance[1]), round(diag$residual_variance[1], 3), "NA")
        )
      }
    } else ""
    isi_state_space_plot_layout(
      p,
      title = paste0(ui_inline_text("Isomap \u72B6\u6001\u8F68\u8FF9", "Isomap state trajectory"), if (nzchar(subtitle)) paste0("<br><sup>", subtitle, " | ", isi_state_space_short_label(unique(dd$train)[1]), "</sup>") else ""),
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
    if (isTRUE(isi_state_space_is_english())) {
      dd$hover_text <- paste0(
        "Train: ", stpd_html_escape(dd$train),
        "<br>ISI idx: ", dd$idx, " \u2192 ", dd$next_idx,
        "<br>time_mid: ", round(dd$time_mid_sec, 6), " s",
        "<br>ISI_i: ", signif(dd$ISI_sec, 6), " s (", round(dd$ISI_plot, 4), " ", u, ")",
        "<br>ISI_i+1: ", signif(dd$next_ISI_sec, 6), " s (", round(dd$next_ISI_plot, 4), " ", u, ")",
        "<br>transition: ", stpd_html_escape(dd$transition),
        "<br>logISI_i: ", round(dd$logISI_i, 4),
        "<br>logISI_i+1: ", round(dd$logISI_next, 4)
      )
    } else {
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
    }
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
      title = paste0(ui_inline_text("logISI \u76F8\u56FE", "logISI phase portrait"), "<br><sup>", isi_state_space_short_label(unique(dd$train)[1]), "</sup>"),
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
    validate(need(all(needed %in% names(dd)), ui_inline_text("\u6240\u9009 3D \u8F74\u5728\u5F53\u524D\u6570\u636E\u4E2D\u4E0D\u5B58\u5728\u3002", "The selected 3D axes are not present in the current data.")))
    ok <- is.finite(dd[[x_col]]) & is.finite(dd[[y_col]]) & is.finite(dd[[z_col]])
    dd <- dd[ok, , drop = FALSE]
    validate(need(nrow(dd) >= 3, ui_inline_text("\u6240\u9009 3D \u8F74\u7684\u6709\u6548\u70B9\u592A\u5C11\u3002", "Too few valid points remain for the selected 3D axes.")))
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
        text = paste0(ui_inline_text("3D ISI \u72B6\u6001\u7A7A\u95F4", "3D ISI state space"), "<br><sup>", isi_state_space_short_label(unique(dd$train)[1]), "</sup>"),
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
	    validate(need(nrow(dd) >= 3, ui_inline_text("\u65E0 Isomap 3D \u5F97\u5206\u3002", "No 3D Isomap scores are available.")))
	    dd <- isi_state_space_enrich_scores(dd)
	    x_col <- input$isi_state_space_isomap_3d_x_axis %||% "Isomap1"
	    y_col <- input$isi_state_space_isomap_3d_y_axis %||% "Isomap2"
	    z_col <- input$isi_state_space_isomap_3d_z_axis %||% "Isomap3"
	    needed <- c(x_col, y_col, z_col)
	    validate(need(all(needed %in% names(dd)), ui_inline_text("\u6240\u9009 Isomap 3D \u8F74\u5728\u5F53\u524D\u6570\u636E\u4E2D\u4E0D\u5B58\u5728\u3002", "The selected Isomap 3D axes are not present in the current data.")))
	    ok <- is.finite(dd[[x_col]]) & is.finite(dd[[y_col]]) & is.finite(dd[[z_col]])
	    dd <- dd[ok, , drop = FALSE]
	    validate(need(nrow(dd) >= 3, ui_inline_text("\u6240\u9009 Isomap 3D \u8F74\u7684\u6709\u6548\u70B9\u592A\u5C11\u3002", "Too few valid points remain for the selected Isomap 3D axes.")))
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
	      if (isTRUE(isi_state_space_is_english())) {
	        paste0(
          "k=", diag$n_neighbors[1],
          "; embedded ", diag$n_embedded[1], "/", diag$n_input[1],
          "; components=", diag$component_n[1],
          "; residual variance=", ifelse(is.finite(diag$residual_variance[1]), round(diag$residual_variance[1], 3), "NA")
        )
	      } else {
	        paste0(
          "k=", diag$n_neighbors[1],
          "\uFF1B\u5D4C\u5165 ", diag$n_embedded[1], "/", diag$n_input[1],
          "\uFF1B\u8FDE\u901A\u5206\u91CF=", diag$component_n[1],
          "\uFF1B\u6B8B\u5DEE\u65B9\u5DEE=", ifelse(is.finite(diag$residual_variance[1]), round(diag$residual_variance[1], 3), "NA")
        )
	      }
    } else ""
    layout(
      p,
      hoverlabel = stpd_hoverlabel_style(),
      title = list(
        text = paste0(ui_inline_text("Isomap 3D \u72B6\u6001\u8F68\u8FF9", "Isomap 3D state trajectory"), if (nzchar(subtitle)) paste0("<br><sup>", subtitle, " | ", isi_state_space_short_label(unique(dd$train)[1]), "</sup>") else ""),
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
    datatable(dat[, c("PC", "variance_pct", "cumulative_pct"), drop = FALSE],
                  rownames = FALSE, options = list(dom = "t", pageLength = 5))
  })

  output$isi_state_space_isomap_diagnostics_table <- DT::renderDT({
    dat <- isi_state_space_isomap_result()$diagnostics
    numeric_cols <- names(dat)[vapply(dat, is.numeric, logical(1))]
    for (nm in numeric_cols) dat[[nm]] <- round(dat[[nm]], 4)
    datatable(dat, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
  })

  output$isi_state_space_loading_table <- DT::renderDT({
    dat <- isi_state_space_pca_result()$loadings
    dat$abs_PC1_PC2_PC3 <- pmax(abs(dat$PC1), abs(dat$PC2), abs(dat$PC3), na.rm = TRUE)
    dat <- dat[order(dat$abs_PC1_PC2_PC3, decreasing = TRUE), , drop = FALSE]
    dat$PC1 <- round(dat$PC1, 4)
    dat$PC2 <- round(dat$PC2, 4)
    dat$PC3 <- round(dat$PC3, 4)
    datatable(head(dat[, c("feature", "PC1", "PC2", "PC3"), drop = FALSE], 30),
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
    datatable(out, rownames = FALSE, filter = "top",
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
    validate(need(length(mat) > 0 && nrow(mat) > 0 && ncol(mat) > 0, ui_inline_text("\u65E0\u53EF\u7528\u72B6\u6001\u8F6C\u79FB\u3002", "No state transitions are available.")))
    plot_ly(
      x = colnames(mat),
      y = rownames(mat),
      z = mat,
      type = "heatmap",
      colorscale = list(c(0, "#F7FBFF"), c(0.35, "#D4E6F4"), c(0.7, "#7DAED3"), c(1, "#235B8C")),
      colorbar = list(thickness = 12, len = 0.72, outlinewidth = 0, tickfont = list(size = 10, color = "#374151")),
      hovertemplate = ui_inline_text(
        "\u8D77\u59CB\u72B6\u6001=%{y}<br>\u76EE\u6807\u72B6\u6001=%{x}<br>P=%{z:.3f}<extra></extra>",
        "from=%{y}<br>to=%{x}<br>P=%{z:.3f}<extra></extra>"
      )
    ) %>%
      layout(
        title = list(text = ui_inline_text("\u72B6\u6001\u8F6C\u79FB\u6982\u7387\u77E9\u9635", "State-transition probability matrix"), x = 0, font = list(size = 14, color = "#111827")),
        xaxis = isi_state_space_axis_style(ui_inline_text("\u76EE\u6807\u72B6\u6001", "to")),
        yaxis = isi_state_space_axis_style(ui_inline_text("\u8D77\u59CB\u72B6\u6001", "from")),
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
    datatable(dat, rownames = FALSE, filter = "top",
                  options = list(pageLength = 10, scrollX = TRUE))
  })

  isi_state_dwell_data <- reactive({
    stpd_state_dwell_times(isi_state_sequence_data())
  })

  output$isi_state_dwell_plot <- renderPlotly({
    dwell <- isi_state_dwell_data()
    validate(need(nrow(dwell) > 0, ui_inline_text("\u6682\u65E0\u9A7B\u7559\u65F6\u95F4\u7247\u6BB5\u3002", "No dwell-time segments are available.")))
    agg <- stats::aggregate(n_isi ~ label, data = dwell, FUN = sum)
    agg <- agg[order(agg$n_isi, decreasing = TRUE), , drop = FALSE]
    cols <- isi_state_space_color_map(as.character(agg$label))
    plot_ly(
      agg,
      x = ~label,
      y = ~n_isi,
      type = "bar",
      marker = list(color = unname(cols[as.character(agg$label)]), line = list(color = "rgba(255,255,255,0.95)", width = 0.6)),
      hovertemplate = ui_inline_text(
        "\u72B6\u6001=%{x}<br>ISI \u603B\u6570=%{y}<extra></extra>",
        "state=%{x}<br>total ISI=%{y}<extra></extra>"
      )
    ) %>%
      layout(
        title = list(text = ui_inline_text("\u9A7B\u7559\u65F6\u95F4\u603B\u91CF\uFF08ISI \u6570\uFF09", "Dwell-time total (ISI count)"), x = 0, font = list(size = 14, color = "#111827")),
        xaxis = isi_state_space_axis_style(""),
        yaxis = isi_state_space_axis_style(ui_inline_text("ISI \u6570", "ISI count")),
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
    datatable(dat, rownames = FALSE, filter = "top",
                  options = list(pageLength = 10, scrollX = TRUE))
  })

  output$isi_state_transition_entropy_table <- DT::renderDT({
    dat <- stpd_transition_entropy(isi_state_sequence_data())
    numeric_cols <- names(dat)[vapply(dat, is.numeric, logical(1))]
    for (nm in numeric_cols) dat[[nm]] <- round(dat[[nm]], 5)
    datatable(dat, rownames = FALSE, options = list(pageLength = 8, scrollX = TRUE))
  })

  output$isi_state_motif_table <- DT::renderDT({
    dat <- stpd_motif_frequency(isi_state_sequence_data(), motif_length = 3L)
    dat$rate <- round(dat$rate, 5)
    datatable(head(dat, 30), rownames = FALSE, filter = "top",
                  options = list(pageLength = 10, scrollX = TRUE))
  })

  output$isi_state_surrogate_summary_table <- DT::renderDT({
    seq <- isi_state_sequence_data()
    validate(need(nrow(seq) >= 6, ui_inline_text("\u6709\u6548\u72B6\u6001\u5E8F\u5217\u592A\u77ED\uFF0C\u65E0\u6CD5\u8FD0\u884C\u66FF\u4EE3\u6570\u636E\u5BF9\u7167\u3002", "The valid state sequence is too short for surrogate controls.")))
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
    datatable(dat, rownames = FALSE, filter = "top",
                  options = list(pageLength = 10, scrollX = TRUE))
  })

  isi_state_space_embedding_plot <- function(scores, x_col, y_col, title) {
    dd <- isi_state_space_enrich_scores(scores)
    validate(need(all(c(x_col, y_col) %in% names(dd)), ui_inline_text("\u6240\u9009\u5D4C\u5165\u8F74\u4E0D\u5B58\u5728\u3002", "The selected embedding axes are not available.")))
    ok <- is.finite(dd[[x_col]]) & is.finite(dd[[y_col]])
    dd <- dd[ok, , drop = FALSE]
    validate(need(nrow(dd) >= 2, ui_inline_text("\u5D4C\u5165\u6709\u6548\u70B9\u592A\u5C11\u3002", "Too few valid embedding points are available.")))
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
    validate(need(nrow(feats) >= 10, ui_inline_text("\u5F53\u524D\u70B9\u6570\u592A\u5C11\uFF0C\u65E0\u6CD5\u8FD0\u884C\u6269\u6563\u6620\u5C04\uFF08Diffusion map\uFF09\u3002", "Too few points are available for a diffusion map.")))
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
    isi_state_space_embedding_plot(
      res$scores,
      "Diffusion1",
      "Diffusion2",
      ui_inline_text("\u6269\u6563\u6620\u5C04\uFF08Diffusion map\uFF09\u63A2\u7D22\u8F68\u8FF9", "Diffusion-map exploratory trajectory")
    )
  })

  isi_state_phate_result <- reactive({
    feats <- isi_state_space_feature_data()
    validate(need(nrow(feats) >= 10, ui_inline_text("\u5F53\u524D\u70B9\u6570\u592A\u5C11\uFF0C\u65E0\u6CD5\u8FD0\u884C PHATE\u3002", "Too few points are available for PHATE.")))
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
    title <- ui_inline_text("PHATE / \u6269\u6563\u52BF\u63A2\u7D22\u8F68\u8FF9", "PHATE / diffusion-potential exploratory trajectory")
    if (nrow(res$diagnostics) > 0 && nzchar(as.character(res$diagnostics$note[1] %||% ""))) {
      title <- paste0(
        title,
        "<br><sup>",
        ui_inline_text("\u6280\u672F\u8BF4\u660E\uFF1A", "Technical note: "),
        res$diagnostics$note[1],
        "</sup>"
      )
    }
    isi_state_space_embedding_plot(res$scores, "PHATE1", "PHATE2", title)
  })

  isi_state_recurrence_result <- reactive({
    feats <- isi_state_space_feature_data()
    validate(need(nrow(feats) >= 10, ui_inline_text("\u5F53\u524D\u70B9\u6570\u592A\u5C11\uFF0C\u65E0\u6CD5\u8FD0\u884C\u590D\u73B0\u5206\u6790 / RQA\u3002", "Too few points are available for recurrence / RQA analysis.")))
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
      hovertemplate = ui_inline_text(
        "i=%{y}<br>j=%{x}<br>\u662F\u5426\u590D\u73B0=%{z}<extra></extra>",
        "i=%{y}<br>j=%{x}<br>recurrent=%{z}<extra></extra>"
      )
    ) %>%
      layout(
        title = list(text = ui_inline_text("\u590D\u73B0\u56FE", "Recurrence plot"), x = 0, font = list(size = 14, color = "#111827")),
        xaxis = isi_state_space_axis_style(ui_inline_text("\u72B6\u6001\u7D22\u5F15", "state index")),
        yaxis = isi_state_space_axis_style(ui_inline_text("\u72B6\u6001\u7D22\u5F15", "state index"), reversed = TRUE),
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
    datatable(dat, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
  })

  output$isi_state_isomap_sweep_table <- DT::renderDT({
    feats <- isi_state_space_feature_data()
    grid <- isi_state_space_parse_int_grid(input$isi_state_space_isomap_sweep_grid, c(5L, 8L, 10L, 15L, 20L, 30L))
    validate(need(nrow(feats) >= 20, ui_inline_text("Isomap \u53C2\u6570\u626B\u63CF\u9700\u8981\u66F4\u591A\u6570\u636E\u70B9\u3002", "The Isomap sweep requires more points.")))
    res <- stpd_run_isi_state_isomap_sweep(
      feats,
      neighbor_grid = grid,
      max_points = safe_int(input$isi_state_space_explore_max_points, 600L),
      scaling = input$isi_state_space_scaling %||% "robust"
    )
    dat <- res$diagnostics
    numeric_cols <- names(dat)[vapply(dat, is.numeric, logical(1))]
    for (nm in numeric_cols) dat[[nm]] <- round(dat[[nm]], 5)
    datatable(dat, rownames = FALSE, options = list(pageLength = 8, scrollX = TRUE))
  })

  output$isi_state_umap_tsne_table <- DT::renderDT({
    feats <- isi_state_space_feature_data()
    u <- stpd_run_isi_state_umap(feats, max_points = safe_int(input$isi_state_space_explore_max_points, 600L))
    t <- stpd_run_isi_state_tsne(feats, max_points = safe_int(input$isi_state_space_explore_max_points, 600L))
    ud <- u$diagnostics; ud$visual <- "UMAP"
    td <- t$diagnostics; td$visual <- "t-SNE"
    dat <- dplyr::bind_rows(ud, td)
    datatable(dat, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
  })

  isi_state_rule_states <- reactive({
    stpd_candidate_states_rule_based(isi_state_space_feature_data())
  })

  output$isi_state_rule_counts_table <- DT::renderDT({
    dat <- as.data.frame(table(isi_state_rule_states()$candidate_state), stringsAsFactors = FALSE)
    names(dat) <- c("candidate_state", "n")
    dat <- dat[order(dat$n, decreasing = TRUE), , drop = FALSE]
    datatable(dat, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
  })

  isi_state_gmm_result <- reactive({
    feats <- isi_state_space_feature_data()
    grid <- isi_state_space_parse_int_grid(input$isi_state_space_gmm_states, 2:5)
    validate(need(nrow(feats) >= max(8L, min(grid) + 2L), ui_inline_text("\u70B9\u6570\u592A\u5C11\uFF0C\u65E0\u6CD5\u8FD0\u884C GMM\u3002", "Too few points are available for GMM.")))
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
      dat <- data.frame(
        message = paste(ui_inline_text("GMM \u8BA1\u7B97\u5931\u8D25\u3002", "GMM computation failed."), ui_condition_detail(res)),
        stringsAsFactors = FALSE
      )
    } else {
      dat <- res$diagnostics
      numeric_cols <- names(dat)[vapply(dat, is.numeric, logical(1))]
      for (nm in numeric_cols) dat[[nm]] <- round(dat[[nm]], 5)
    }
    datatable(dat, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
  })

  output$isi_state_gmm_state_table <- DT::renderDT({
    res <- isi_state_gmm_result()
    dat <- if (inherits(res, "error")) {
      data.frame(
        message = paste(ui_inline_text("GMM \u8BA1\u7B97\u5931\u8D25\u3002", "GMM computation failed."), ui_condition_detail(res)),
        stringsAsFactors = FALSE
      )
    } else {
      res$state_stats
    }
    numeric_cols <- names(dat)[vapply(dat, is.numeric, logical(1))]
    for (nm in numeric_cols) dat[[nm]] <- round(dat[[nm]], 5)
    datatable(dat, rownames = FALSE, options = list(pageLength = 8, scrollX = TRUE))
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
    validate(need(length(labels) >= 6, ui_inline_text("\u5019\u9009\u72B6\u6001\u5E8F\u5217\u592A\u77ED\uFF0C\u65E0\u6CD5\u8FD0\u884C HSMM \u98CE\u683C\u89E3\u7801\u3002", "The candidate-state sequence is too short for HSMM-style decoding.")))
    stpd_decode_hsmm(labels, max_duration = 50L)
  })

  output$isi_state_hsmm_segments_table <- DT::renderDT({
    dat <- isi_state_hsmm_result()$segments
    numeric_cols <- names(dat)[vapply(dat, is.numeric, logical(1))]
    for (nm in numeric_cols) dat[[nm]] <- round(dat[[nm]], 6)
    datatable(dat, rownames = FALSE, filter = "top",
                  options = list(pageLength = 10, scrollX = TRUE))
  })

  output$isi_state_hsmm_agreement_table <- DT::renderDT({
    hs <- isi_state_hsmm_result()
    dat <- stpd_label_agreement(hs$decoded, isi_state_candidate_labels())$per_label
    numeric_cols <- names(dat)[vapply(dat, is.numeric, logical(1))]
    for (nm in numeric_cols) dat[[nm]] <- round(dat[[nm]], 5)
    datatable(dat, rownames = FALSE, options = list(pageLength = 8, scrollX = TRUE))
  })

  output$isi_state_model_validation_table <- DT::renderDT({
    labels <- isi_state_candidate_labels()
    held <- tryCatch(
      stpd_hsmm_heldout_likelihood(labels, max_duration = 50L),
      error = function(e) data.frame(metric = "heldout_error", value = ui_condition_detail(e), stringsAsFactors = FALSE)
    )
    boot <- tryCatch(
      stpd_state_bootstrap_metrics(labels, n_bootstrap = 49L, seed = 1L)$summary,
      error = function(e) data.frame(metric = "bootstrap_error", value = ui_condition_detail(e), stringsAsFactors = FALSE)
    )
    held_long <- data.frame(metric = names(held), value = as.character(unlist(held[1, , drop = TRUE])), stringsAsFactors = FALSE)
    boot$metric <- paste0("bootstrap_", boot$metric)
    dat <- dplyr::bind_rows(held_long, boot)
    numeric_cols <- names(dat)[vapply(dat, is.numeric, logical(1))]
    for (nm in numeric_cols) dat[[nm]] <- round(dat[[nm]], 5)
    datatable(dat, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
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
    validate(need(nrow(dat) > 0, ui_inline_text("\u65E0\u53EF\u7528\u7684\u8DE8 train \u8F6C\u79FB\u6570\u636E\u3002", "No cross-train transition data are available.")))
    datatable(dat, rownames = FALSE, filter = "top",
                  options = list(pageLength = 10, scrollX = TRUE))
  })

  output$isi_state_train_transition_model_summary_table <- DT::renderDT({
    dat <- isi_state_train_transition_data()
    validate(need(nrow(dat) > 0, ui_inline_text("\u65E0\u53EF\u7528\u7684\u8DE8 train \u8F6C\u79FB\u6570\u636E\u3002", "No cross-train transition data are available.")))
    fixed <- intersect(c("from", "structure", "nucleus", "side"), names(dat))
    if (length(fixed) == 0) fixed <- "from"
    fit <- tryCatch(
      stpd_fit_transition_statistical_model(dat, fixed_effects = fixed, method = "one_vs_rest_glm"),
      error = function(e) e
    )
    if (inherits(fit, "error")) {
      out <- data.frame(
        method = "one_vs_rest_glm",
        message = paste(ui_inline_text("\u8F6C\u79FB\u6A21\u578B\u62DF\u5408\u5931\u8D25\u3002", "Transition-model fitting failed."), ui_condition_detail(fit)),
        stringsAsFactors = FALSE
      )
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
    datatable(out, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
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
    unknown_display <- ui_inline_text("\u672A\u77E5", "unknown")
    for (nm in c("dataset", "structure", "side")) {
      value <- get(nm)
      value[!is.na(value) & value == "unknown"] <- unknown_display
      assign(nm, value)
    }
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
	      paste0(
	        "[", source, "] ", display, " (", length(d$trains %||% list()),
	        ui_inline_text(" \u6761 train)", " trains)")
	      )
	    }, character(1))
    selected <- state_trajectory_dataset_ids()
    if (length(selected) == 0L) selected <- rv$current_id %||% ids[1]
    selectizeInput(
      "state_trajectory_dataset_ids",
      ui_inline_text("\u5206\u6790\u6570\u636E\u96C6", "Analysis datasets"),
      choices = stats::setNames(ids, labels),
      selected = selected,
      multiple = TRUE,
      options = list(
        plugins = list("remove_button"),
        placeholder = ui_inline_text("\u53EF\u9009\u591A\u4E2A\u5DF2\u52A0\u8F7D\u6570\u636E\u96C6", "Select one or more loaded datasets")
      )
    )
  })

  state_trajectory_pool_data <- reactive({
    ds_all <- rv$datasets
    ids <- state_trajectory_dataset_ids()
    validate(need(length(ids) > 0L, ui_inline_text("\u8BF7\u5148\u9009\u62E9\u7528\u4E8E\u72B6\u6001\u8F68\u8FF9\u5206\u6790\u7684\u6570\u636E\u96C6\u3002", "Select a dataset for the state trajectory first.")))
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
      return(tags$div(class = "small-note", ui_inline_text("\u8BF7\u5148\u4E0A\u4F20 spike train \u6570\u636E\u3002", "Upload spike-train data first.")))
    }
    labs <- pool$train_labels[trains]
    labs[is.na(labs) | !nzchar(labs)] <- trains[is.na(labs) | !nzchar(labs)]
    selected <- intersect(as.character(isolate(input$state_trajectory_trains) %||% character(0)), trains)
    selectizeInput(
      "state_trajectory_trains",
      ui_inline_text("\u9009\u62E9 spike trains", "Select spike trains"),
      choices = stats::setNames(trains, labs),
      selected = selected,
      multiple = TRUE,
      options = list(
        plugins = list("remove_button"),
        closeAfterSelect = TRUE,
        placeholder = ui_inline_text("\u8BF7\u624B\u52A8\u9009\u62E9 trains\uFF08\u9ED8\u8BA4\u4E0D\u5168\u9009\uFF09", "Select trains manually (none are preselected)")
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
    validate(need(length(selected) >= 1L, ui_inline_text("\u8BF7\u81F3\u5C11\u9009\u62E9\u4E00\u6761 spike train\u3002", "Select at least one spike train.")))
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
    validate(need(nrow(res$features %||% data.frame()) >= 2L, ui_inline_text("\u6709\u6548\u65F6\u95F4\u7BB1\u592A\u5C11\uFF0C\u65E0\u6CD5\u6784\u5EFA\u8F68\u8FF9\u3002", "Too few valid time bins are available to build a trajectory.")))
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
    msg <- if (identical(ui_language(), "en")) {
      paste0(
        "Actual analysis window: ",
        fmt(ws$window_start_sec[1]), "-", fmt(ws$window_end_sec[1]), " s",
        " (", fmt(ws$window_duration_sec[1]), " s); ",
        ws$n_bins[1], " bins @ ", fmt(1000 * ws$bin_sec[1], 1), " ms. ",
        "Selected-train raw duration: median ",
        fmt(ws$train_duration_median_sec[1]), " s",
        " (range ", fmt(ws$train_duration_min_sec[1]), "-",
        fmt(ws$train_duration_max_sec[1]), " s)."
      )
    } else {
      paste0(
        "\u5B9E\u9645\u5206\u6790\u7A97\uFF1A",
        fmt(ws$window_start_sec[1]), "-", fmt(ws$window_end_sec[1]), " s",
        " \uFF08", fmt(ws$window_duration_sec[1]), " s\uFF09\uFF1B",
        ws$n_bins[1], " \u4E2A bin @ ", fmt(1000 * ws$bin_sec[1], 1), " ms\u3002",
        "\u6240\u9009 train \u539F\u59CB\u65F6\u957F\uFF1A\u4E2D\u4F4D\u6570 ",
        fmt(ws$train_duration_median_sec[1]), " s",
        "\uFF08\u8303\u56F4 ", fmt(ws$train_duration_min_sec[1]), "-",
        fmt(ws$train_duration_max_sec[1]), " s\uFF09\u3002"
      )
    }
    if (is.data.frame(tw) && nrow(tw) > 0L) {
      raw_start <- suppressWarnings(min(tw$raw_start_sec, na.rm = TRUE))
      raw_end <- suppressWarnings(max(tw$raw_end_sec, na.rm = TRUE))
      if (is.finite(raw_start) && is.finite(raw_end)) {
        msg <- paste0(
          msg,
          ui_inline_text(" \u539F\u59CB\u65F6\u95F4\u6233\u8303\u56F4\uFF1A", " Raw timestamp range: "),
          fmt(raw_start), "-", fmt(raw_end), " s."
        )
      }
    }
    tags$div(class = "small-note state-trajectory-window-summary", msg)
  })

	  output$state_pair_controls <- renderUI({
	    res <- state_trajectory_result()
	    trains <- as.character(res$selected_trains %||% character(0))
	    if (length(trains) < 2L) {
	      return(tags$div(class = "small-note", ui_text("select_two_spike_trains")))
	    }
	    labs <- res$selected_train_labels %||% stats::setNames(trains, trains)
	    labs <- labs[trains]
	    labs[is.na(labs) | !nzchar(labs)] <- trains[is.na(labs) | !nzchar(labs)]
	    train_choices <- stats::setNames(trains, labs)
	    selected_trains <- intersect(
	      as.character(ui_control_remembered(
	        "state_pair_trains", trains[seq_len(min(4L, length(trains)))]
	      )),
	      trains
	    )
	    lag_value <- suppressWarnings(as.numeric(ui_control_remembered(
	      "state_pair_lag_bins", 0
	    ))[1])
	    if (!is.finite(lag_value)) lag_value <- 0
	    lag_value <- max(-1000, min(1000, lag_value))
	    heat_value <- as.character(ui_control_remembered(
	      "state_pair_heatmap_value", "log2_enrichment"
	    ))[1]
	    heat_choices <- c(
	      "log2_enrichment", "observed_count", "observed_prob",
	      "standardized_residual", "observed_expected_ratio"
	    )
	    if (!heat_value %in% heat_choices) heat_value <- "log2_enrichment"
	    fluidRow(
	      column(6, selectizeInput("state_pair_trains", ui_inline_text("\u8054\u5408\u72B6\u6001\u5206\u6790\u7684 trains", "Trains for joint-state analysis"),
	                               choices = train_choices,
	                               selected = selected_trains,
	                               multiple = TRUE)),
      column(2, numericInput("state_pair_lag_bins", ui_inline_text("\u6EDE\u540E\u65F6\u95F4\u7BB1\u6570", "Lag bins"), value = lag_value, min = -1000, max = 1000, step = 1)),
      column(4, selectInput(
        "state_pair_heatmap_value", ui_inline_text("\u77E9\u9635\u6570\u503C", "Matrix value"),
        choices = c(
          stats::setNames("log2_enrichment", ui_inline_text("log2 \u5BCC\u96C6", "log2 enrichment")),
          stats::setNames("observed_count", ui_inline_text("\u89C2\u6D4B\u8BA1\u6570", "observed count")),
          stats::setNames("observed_prob", ui_inline_text("\u89C2\u6D4B\u6982\u7387", "observed probability")),
          stats::setNames("standardized_residual", ui_inline_text("\u6807\u51C6\u5316\u6B8B\u5DEE", "standardized residual")),
          stats::setNames("observed_expected_ratio", ui_inline_text("\u89C2\u6D4B\u503C / \u671F\u671B\u503C", "observed / expected"))
        ),
        selected = heat_value
      )),
      column(
        12,
	        tags$div(
	          class = "small-note",
	          ui_inline_text(
	            "\u8BF7\u9009\u62E9\u4E24\u6761\u6216\u66F4\u591A spike train\u3002\u6EDE\u540E\u65F6\u95F4\u7BB1\u6570 > 0 \u65F6\uFF0C\u5C06\u7B2C\u4E00\u6761 train \u5728 t \u65F6\u523B\u7684\u72B6\u6001\u4E0E\u5176\u4ED6\u6240\u9009 train \u5728 t + \u6EDE\u540E\u91CF\u65F6\u523B\u7684\u72B6\u6001\u914D\u5BF9\u3002\u5BCC\u96C6\u5EA6\u6BD4\u8F83\u8054\u5408\u72B6\u6001\u89C2\u6D4B\u8BA1\u6570\u4E0E\u7531\u5404 train \u8FB9\u9645\u72B6\u6001\u9891\u7387\u5F97\u5230\u7684\u72EC\u7ACB\u6027\u671F\u671B\u3002",
	            "Select two or more spike trains. Lag bins > 0 pairs the first selected train at time t with the other selected trains at t + lag. Enrichment compares observed joint-state counts with the independence expectation from each train's marginal state frequencies."
	          )
	        )
      )
    )
  })

  state_pair_result <- reactive({
    res <- state_trajectory_result()
    validate(need(length(res$selected_trains %||% character(0)) >= 2L, ui_inline_text("\u8BF7\u81F3\u5C11\u9009\u62E9\u4E24\u6761 spike train \u8FDB\u884C\u72B6\u6001\u5BF9\u5206\u6790\u3002", "Select at least two spike trains for state-pair analysis.")))
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
    validate(need(nrow(out$pair_bins %||% data.frame()) >= 1L, ui_inline_text("\u6CA1\u6709\u53EF\u7528\u7684\u72B6\u6001\u5BF9\u65F6\u95F4\u7BB1\u3002", "No state-pair bins are available.")))
    out
  })

  output$state_pair_timeline_plot <- renderPlotly({
    stpd_state_pair_timeline_plot(state_pair_result(), lang = ui_language())
  })

  output$state_pair_heatmap_plot <- renderPlotly({
    value <- input$state_pair_heatmap_value %||% "log2_enrichment"
    stpd_state_pair_heatmap(state_pair_result(), value = value, lang = ui_language())
  })

  output$state_pair_transition_heatmap_plot <- renderPlotly({
    stpd_state_pair_transition_heatmap(state_pair_result(), value = "prob", lang = ui_language())
  })

  output$state_pair_matrix_table <- DT::renderDT({
    dat <- state_pair_result()$matrix
	    if (is.null(dat) || nrow(dat) == 0L) {
	      dat <- data.frame(message = ui_inline_text("\u6682\u65E0\u53EF\u7528\u7684\u72B6\u6001\u5BF9\u77E9\u9635\u3002", "No state-pair matrix available."), stringsAsFactors = FALSE)
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
    datatable(dat, rownames = FALSE, filter = "top",
                  options = list(pageLength = 12, scrollX = TRUE))
  })

  output$state_pair_transition_table <- DT::renderDT({
    dat <- state_pair_result()$transitions
    if (is.null(dat) || nrow(dat) == 0L) {
      dat <- data.frame(message = ui_inline_text("\u8054\u5408\u72B6\u6001\u8F6C\u6362\u81F3\u5C11\u9700\u8981\u4E24\u4E2A\u6210\u5BF9\u65F6\u95F4\u7BB1\u3002", "At least two paired bins are required for joint-state transitions."), stringsAsFactors = FALSE)
    } else {
      dat$prob <- round(dat$prob, 6)
    }
    datatable(dat, rownames = FALSE, filter = "top",
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
    datatable(dat, rownames = FALSE, filter = "top",
                  options = list(pageLength = 15, scrollX = TRUE))
  })

  output$state_trajectory_plot <- renderPlotly({
    res <- state_trajectory_result()
    axis_cols <- c(
      input$state_trajectory_x_axis %||% "burst_activity",
      input$state_trajectory_y_axis %||% "pause_activity",
      input$state_trajectory_z_axis %||% "tonic_activity"
    )
    stpd_state_trajectory_plot(
      res,
      coordinate_mode = input$state_trajectory_coordinate_mode %||% "pattern_axes",
      axis_cols = axis_cols,
      lang = ui_language()
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
    datatable(out, rownames = FALSE, filter = "top",
                  options = list(pageLength = 15, scrollX = TRUE))
  })

  output$state_trajectory_variance_table <- DT::renderDT({
    res <- state_trajectory_result()
    dat <- res$embedding_diagnostics %||% data.frame()
    if (is.null(dat) || nrow(dat) == 0L) {
	      dat <- data.frame(message = ui_inline_text("\u6682\u65E0\u53EF\u7528\u7684\u5D4C\u5165\u8BCA\u65AD\u3002", "No embedding diagnostics available."), stringsAsFactors = FALSE)
    } else {
      if (is.data.frame(res$variance) && nrow(res$variance) > 0L) {
        pca_var <- data.frame(
          method = "PCA",
          component = res$variance$PC,
          metric = "variance_pct",
          value = as.character(round(100 * res$variance$variance, 3)),
          note = ui_inline_text(
            "\u6BCF\u4E2A\u4E3B\u6210\u5206\u89E3\u91CA\u7684\u6807\u51C6\u5316\u7279\u5F81\u65B9\u5DEE\u767E\u5206\u6BD4\u3002",
            "Percentage of scaled feature variance explained by each principal component."
          ),
          stringsAsFactors = FALSE
        )
        dat <- rbind(dat, pca_var)
      }
    }
    datatable(dat, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
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
	      dat <- data.frame(message = ui_inline_text("\u5F53\u524D\u7279\u5F81\u77E9\u9635\u6CA1\u6709\u53EF\u7528\u7684\u7EBF\u6027\u8F7D\u8377\u3002", "No linear loadings are available for the current feature matrix."), stringsAsFactors = FALSE)
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
    datatable(dat, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })

	  output$state_trajectory_transition_table <- DT::renderDT({
	    dat <- state_trajectory_result()$features
	    states <- c("burst", "pause", "tonic", "hf_spiking", "others", "unlabeled")
	    tm <- stpd_state_transition_matrix(dat$dominant_state, states = states, normalize = "row")
    out <- tm$table
    out <- out[out$n > 0 | is.finite(out$prob), , drop = FALSE]
    out$prob <- round(out$prob, 5)
	    datatable(out, rownames = FALSE, filter = "top",
	                  options = list(pageLength = 12, scrollX = TRUE))
	  })

  output$event_aligned_train_selector <- renderUI({
	    ds <- tryCatch(current_dataset(), error = function(e) NULL)
	    if (is.null(ds) || is.null(ds$trains) || length(ds$trains) == 0L) {
	      return(tags$div(class = "small-note", ui_text("select_spike_train")))
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
	      ui_inline_text("Spike train / neuron", "Spike trains / neurons"),
	      choices = stats::setNames(trains, labs),
	      selected = selected,
	      multiple = TRUE,
	      options = list(plugins = list("remove_button"), closeAfterSelect = TRUE,
	                     placeholder = ui_inline_text("\u9009\u62E9\u7528\u4E8E\u4E8B\u4EF6\u5BF9\u9F50\u6D3B\u52A8\u7684 train", "Select trains for event-aligned activity"))
	    )
	  })

	  output$event_aligned_event_selector <- renderUI({
	    events <- task_events_current()
	    if (nrow(events) == 0L) {
	      return(tags$div(class = "small-note", ui_text("no_task_events")))
	    }
	    all_names <- sort(unique(as.character(events$event_name)))
	    selected_names <- intersect(as.character(isolate(input$event_aligned_event_names) %||% all_names), all_names)
	    if (length(selected_names) == 0L) selected_names <- all_names
	    tagList(
	      selectizeInput(
	        "event_aligned_event_names",
	        ui_inline_text("\u4EFB\u52A1\u4E8B\u4EF6\u7C7B\u578B", "Task-event types"),
	        choices = all_names,
	        selected = selected_names,
	        multiple = TRUE,
	        options = list(plugins = list("remove_button"), closeAfterSelect = TRUE)
	      ),
	      tags$div(
	        class = "small-note",
	        ui_inline_text(
	          paste0("\u53EF\u7528\u4E8B\u4EF6\u65F6\u95F4\u6233\uFF1A", nrow(events), " \u4E2A\u3002"),
	          paste0("Available event timestamps: ", nrow(events), ".")
	        )
	      )
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
	    validate(need(length(selected) >= 1L, ui_text("select_spike_train")))
	    events <- event_aligned_events_filtered()
	    validate(need(nrow(events) > 0L, ui_text("select_task_event")))
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
	    event_failure <- ui_text("event_aligned_failed")
	    event_detail <- as.character(res$message %||% "")[1]
	    if (!is.na(event_detail) && nzchar(trimws(event_detail))) {
	      event_failure <- paste(event_failure, ui_condition_detail(simpleError(event_detail)))
	    }
	    validate(need(identical(res$status, "ok"), event_failure))
	    res
	  })

	  output$event_aligned_raster_plot <- renderPlotly({
	    max_spikes <- suppressWarnings(as.integer(round(as.numeric(input$event_aligned_max_raster_spikes %||% 5000)[1])))
	    stpd_event_aligned_raster_plot(
	      event_aligned_result(), max_spikes = max_spikes,
	      lang = ui_language()
	    )
	  })

	  output$event_aligned_psth_plot <- renderPlotly({
	    stpd_event_aligned_psth_plot(event_aligned_result(), lang = ui_language())
	  })

	  output$event_aligned_population_plot <- renderPlotly({
	    stpd_event_aligned_population_plot(event_aligned_result(), lang = ui_language())
	  })

	  output$event_aligned_heatmap_plot <- renderPlotly({
	    stpd_event_aligned_heatmap_plot(event_aligned_result(), lang = ui_language())
	  })

	  output$event_aligned_correlation_plot <- renderPlotly({
	    stpd_event_aligned_correlation_plot(event_aligned_result(), lang = ui_language())
	  })

	  output$event_aligned_correlogram_plot <- renderPlotly({
	    stpd_event_aligned_correlogram_plot(event_aligned_result(), lang = ui_language())
	  })

	  output$event_aligned_summary_table <- DT::renderDT({
	    dat <- event_aligned_result()$summary %||% data.frame()
	    datatable(dat, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
	  })

	  output$event_aligned_population_table <- DT::renderDT({
	    dat <- event_aligned_result()$population %||% data.frame()
	    numeric_cols <- names(dat)[vapply(dat, is.numeric, logical(1))]
	    for (nm in numeric_cols) dat[[nm]] <- round(dat[[nm]], 6)
	    datatable(dat, rownames = FALSE, filter = "top",
	                  options = list(pageLength = 12, scrollX = TRUE))
	  })

	  output$event_aligned_psth_table <- DT::renderDT({
	    dat <- event_aligned_result()$psth %||% data.frame()
	    numeric_cols <- names(dat)[vapply(dat, is.numeric, logical(1))]
	    for (nm in numeric_cols) dat[[nm]] <- round(dat[[nm]], 6)
	    datatable(dat, rownames = FALSE, filter = "top",
	                  options = list(pageLength = 12, scrollX = TRUE))
	  })

	  output$event_aligned_correlation_table <- DT::renderDT({
	    dat <- event_aligned_result()$correlation %||% data.frame()
	    numeric_cols <- names(dat)[vapply(dat, is.numeric, logical(1))]
	    for (nm in numeric_cols) dat[[nm]] <- round(dat[[nm]], 6)
	    datatable(dat, rownames = FALSE, filter = "top",
	                  options = list(pageLength = 12, scrollX = TRUE))
	  })

	  neural_manifold_behavior_data <- reactive({
	    file <- input$neural_manifold_behavior_file
	    if (is.null(file) || is.null(file$datapath) || !nzchar(file$datapath)) return(NULL)
	    tryCatch(
	      utils::read.csv(file$datapath, stringsAsFactors = FALSE, check.names = FALSE),
	      error = function(e) {
	        showNotification(paste(ui_text("behavior_csv_failed"), ui_condition_detail(e)), type = "error", duration = 8)
	        NULL
	      }
	    )
	  })

	  output$neural_manifold_behavior_columns <- renderUI({
	    dat <- neural_manifold_behavior_data()
	    if (is.null(dat) || !is.data.frame(dat) || ncol(dat) < 2L) {
	      return(tags$div(
	        class = "small-note",
	        ui_inline_text(
	          "\u53EF\u9009\uFF1A\u4E0A\u4F20\u542B\u65F6\u95F4\u5217\u4EE5\u53CA\u4E00\u4E2A\u6570\u503C/\u5206\u7C7B\u884C\u4E3A\u5217\u7684\u884C\u4E3A/\u8FD0\u52A8 CSV\u3002",
	          "Optional: upload a behavior/movement CSV with a time column and one numeric/categorical behavior column."
	        )
	      ))
	    }
	    nms <- names(dat)
	    lower <- tolower(nms)
	    time_guess <- nms[which(lower %in% c("time", "timestamp", "timestamp_sec", "time_sec", "t"))[1]]
	    if (is.na(time_guess) || !nzchar(time_guess)) time_guess <- nms[1]
	    value_guess <- setdiff(nms, time_guess)[1] %||% nms[min(2L, length(nms))]
	    time_selected <- as.character(ui_control_remembered(
	      "neural_manifold_behavior_time_col", time_guess
	    ))[1]
	    value_selected <- as.character(ui_control_remembered(
	      "neural_manifold_behavior_value_col", value_guess
	    ))[1]
	    if (!time_selected %in% nms) time_selected <- time_guess
	    if (!value_selected %in% nms) value_selected <- value_guess
	    tagList(
	      selectInput("neural_manifold_behavior_time_col", ui_inline_text("\u884C\u4E3A\u65F6\u95F4\u5217", "Behavior time column"), choices = nms, selected = time_selected),
	      selectInput("neural_manifold_behavior_value_col", ui_inline_text("\u884C\u4E3A/\u8FD0\u52A8\u53D8\u91CF", "Behavior / movement variable"), choices = nms, selected = value_selected)
	    )
	  })

	  neural_manifold_trial_events_data <- reactive({
	    file <- input$neural_manifold_trial_file
	    if (is.null(file) || is.null(file$datapath) || !nzchar(file$datapath)) return(NULL)
	    tryCatch(
	      utils::read.csv(file$datapath, stringsAsFactors = FALSE, check.names = FALSE),
	      error = function(e) {
	        showNotification(paste(ui_text("trial_event_csv_failed"), ui_condition_detail(e)), type = "error", duration = 8)
	        NULL
	      }
	    )
	  })

	  output$neural_manifold_trial_columns <- renderUI({
	    dat <- neural_manifold_trial_events_data()
	    if (is.null(dat) || !is.data.frame(dat) || ncol(dat) < 1L) {
	      return(tags$div(
	        class = "small-note",
	        ui_inline_text(
	          "sliceTCA \u53EF\u9009\u8F93\u5165\uFF1A\u4E0A\u4F20 trial/\u4E8B\u4EF6\u65F6\u95F4\uFF0C\u4F8B\u5982 movement_onset_sec\uFF0C\u5E76\u53EF\u5305\u542B trial_id \u548C condition \u5217\u3002",
	          "Optional for sliceTCA: upload trial/event times, for example movement_onset_sec with trial_id and condition columns."
	        )
	      ))
	    }
	    nms <- names(dat)
	    lower <- tolower(nms)
	    time_guess <- nms[which(lower %in% c("event_time", "event_time_sec", "movement_onset", "movement_onset_sec", "onset", "onset_sec", "time", "timestamp_sec"))[1]]
	    if (is.na(time_guess) || !nzchar(time_guess)) time_guess <- nms[1]
	    trial_guess <- nms[which(lower %in% c("trial", "trial_id", "trialid", "trial_index"))[1]]
	    if (is.na(trial_guess) || !nzchar(trial_guess)) trial_guess <- ""
	    condition_guess <- nms[which(lower %in% c("condition", "movement", "movement_type", "direction", "side", "label", "phase"))[1]]
	    if (is.na(condition_guess) || !nzchar(condition_guess)) condition_guess <- ""
	    time_selected <- as.character(ui_control_remembered(
	      "neural_manifold_trial_time_col", time_guess
	    ))[1]
	    trial_selected <- as.character(ui_control_remembered(
	      "neural_manifold_trial_id_col", trial_guess
	    ))[1]
	    condition_selected <- as.character(ui_control_remembered(
	      "neural_manifold_trial_condition_col", condition_guess
	    ))[1]
	    if (!time_selected %in% nms) time_selected <- time_guess
	    if (!trial_selected %in% c("", nms)) trial_selected <- trial_guess
	    if (!condition_selected %in% c("", nms)) condition_selected <- condition_guess
	    tagList(
	      selectInput("neural_manifold_trial_time_col", ui_inline_text("sliceTCA \u4E8B\u4EF6\u65F6\u95F4\u5217", "sliceTCA event time column"), choices = nms, selected = time_selected),
	      selectInput("neural_manifold_trial_id_col", ui_inline_text("sliceTCA trial ID \u5217", "sliceTCA trial id column"), choices = c(stats::setNames("", ui_inline_text("\u81EA\u52A8\u7F16\u53F7", "auto sequence")), nms), selected = trial_selected),
	      selectInput("neural_manifold_trial_condition_col", ui_inline_text("sliceTCA \u6761\u4EF6/\u8FD0\u52A8\u5217", "sliceTCA condition / movement column"), choices = c(stats::setNames("", ui_inline_text("\u65E0", "none")), nms), selected = condition_selected)
	    )
	  })

	  output$neural_manifold_train_selector <- renderUI({
	    ds <- tryCatch(current_dataset(), error = function(e) NULL)
	    if (is.null(ds) || is.null(ds$trains) || length(ds$trains) == 0L) {
	      return(tags$div(class = "small-note", ui_text("select_spike_train")))
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
	      ui_inline_text("Spike train / neuron", "Spike trains / neurons"),
	      choices = stats::setNames(trains, labs),
	      selected = selected,
	      multiple = TRUE,
	      options = list(plugins = list("remove_button"), closeAfterSelect = TRUE,
	                     placeholder = ui_inline_text("\u9009\u62E9\u540C\u65F6\u8BB0\u5F55\u7684 neuron", "Select simultaneously recorded neurons"))
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
	    if (is.null(ds) || is.null(ds$trains)) return(stpd_slicetca_empty_result(ui_text("select_spike_train")))
	    ds <- normalize_dataset(ds)
	    selected <- neural_manifold_selected_trains()
	    dataset_events <- if (isTRUE(input$neural_manifold_use_dataset_events)) {
	      stpd_task_events_for_slicetca(task_events_filtered(use_neural_input = TRUE))
	    } else {
	      data.frame()
	    }
	    use_dataset_events <- isTRUE(input$neural_manifold_use_dataset_events) && is.data.frame(dataset_events) && nrow(dataset_events) > 0L
	    events <- if (use_dataset_events) dataset_events else neural_manifold_trial_events_data()
	    if (length(selected) < 2L) return(stpd_slicetca_empty_result(ui_text("select_two_spike_trains")))
	    if (is.null(events) || !is.data.frame(events) || nrow(events) == 0L) {
	      return(stpd_slicetca_empty_result(ui_text("select_task_event")))
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
	    validate(need(length(selected) >= 2L, ui_text("select_two_spike_trains")))
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
	    validate(need(nrow(pop$features %||% data.frame()) >= 3L, ui_text("neural_manifold_insufficient_bins")))
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
	    manifold_failure <- if (inherits(out, "error")) {
	      paste(ui_text("neural_manifold_failed"), ui_condition_detail(out))
	    } else {
	      ui_text("neural_manifold_failed")
	    }
	    validate(need(!inherits(out, "error"), manifold_failure))
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
	          note = paste(ui_text("neural_manifold_failed"), ui_condition_detail(event_out)),
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
	    stpd_neural_manifold_plot(neural_manifold_result(), lang = ui_language())
	  })

	  output$neural_manifold_slicetca_plot <- renderPlotly({
	    stpd_slicetca_plot(
	      neural_manifold_slicetca_result(),
	      use_reconstruction = isTRUE(input$neural_manifold_slicetca_recon_plot),
	      lang = ui_language()
	    )
	  })

	  output$neural_manifold_slicetca_summary_table <- DT::renderDT({
	    dat <- neural_manifold_slicetca_result()$tensor_summary %||% data.frame()
	    if (is.null(dat) || nrow(dat) == 0L) dat <- data.frame(message = ui_inline_text("\u6682\u65E0\u53EF\u7528\u7684 sliceTCA \u5F20\u91CF\u6458\u8981\u3002", "No sliceTCA tensor summary is available."), stringsAsFactors = FALSE)
	    datatable(dat, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
	  })

	  output$neural_manifold_slicetca_diagnostics_table <- DT::renderDT({
	    dat <- neural_manifold_slicetca_result()$diagnostics %||% data.frame()
	    if (is.null(dat) || nrow(dat) == 0L) dat <- stpd_slicetca_backend_status()
	    datatable(dat, rownames = FALSE, filter = "top",
	                  options = list(pageLength = 10, scrollX = TRUE))
	  })

	  output$neural_manifold_slicetca_reconstruction_table <- DT::renderDT({
	    dat <- neural_manifold_slicetca_result()$reconstruction_metrics %||% data.frame()
	    if (is.null(dat) || nrow(dat) == 0L) dat <- data.frame(message = ui_inline_text("\u6682\u65E0\u53EF\u7528\u7684 sliceTCA \u91CD\u5EFA\u6307\u6807\u3002", "No sliceTCA reconstruction metrics are available."), stringsAsFactors = FALSE)
	    numeric_cols <- names(dat)[vapply(dat, is.numeric, logical(1))]
	    for (nm in numeric_cols) dat[[nm]] <- round(dat[[nm]], 6)
	    datatable(dat, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
	  })

	  output$neural_manifold_slicetca_embedding_table <- DT::renderDT({
	    res <- neural_manifold_slicetca_result()
	    dat <- if (isTRUE(input$neural_manifold_slicetca_recon_plot) && is.data.frame(res$reconstructed_embedding) && nrow(res$reconstructed_embedding) > 0L) {
	      res$reconstructed_embedding
	    } else {
	      res$trial_embedding %||% data.frame()
	    }
	    if (is.null(dat) || nrow(dat) == 0L) dat <- data.frame(message = ui_inline_text("\u6682\u65E0\u53EF\u7528\u7684\u8BD5\u6B21-\u65F6\u95F4\u5F20\u91CF\u5750\u6807\u3002", "No trial-time tensor coordinates are available."), stringsAsFactors = FALSE)
	    show_cols <- intersect(c("trial_index", "trial_id", "condition", "rel_bin", "rel_time_sec",
	                             "event_state", "burst_fraction", "pause_fraction", "tonic_fraction",
	                             "TC1", "TC2", "TC3"), names(dat))
	    if (length(show_cols) > 0L) dat <- dat[, show_cols, drop = FALSE]
	    numeric_cols <- names(dat)[vapply(dat, is.numeric, logical(1))]
	    for (nm in numeric_cols) dat[[nm]] <- round(dat[[nm]], 6)
	    datatable(dat, rownames = FALSE, filter = "top",
	                  options = list(pageLength = 15, scrollX = TRUE))
	  })

	  output$neural_manifold_validation_table <- DT::renderDT({
	    dat <- neural_manifold_result()$validation %||% data.frame()
	    if (is.null(dat) || nrow(dat) == 0L) dat <- data.frame(message = ui_inline_text("\u6682\u65E0\u53EF\u7528\u7684\u9A8C\u8BC1\u6307\u6807\u3002", "No validation metrics are available."), stringsAsFactors = FALSE)
	    numeric_cols <- names(dat)[vapply(dat, is.numeric, logical(1))]
	    for (nm in numeric_cols) dat[[nm]] <- round(dat[[nm]], 6)
	    datatable(dat, rownames = FALSE, filter = "top",
	                  options = list(pageLength = 14, scrollX = TRUE))
	  })

	  output$neural_manifold_diagnostics_table <- DT::renderDT({
	    dat <- neural_manifold_result()$diagnostics %||% data.frame()
	    if (is.null(dat) || nrow(dat) == 0L) dat <- data.frame(message = ui_inline_text("\u6682\u65E0\u53EF\u7528\u7684\u5D4C\u5165\u8BCA\u65AD\u3002", "No embedding diagnostics are available."), stringsAsFactors = FALSE)
	    datatable(dat, rownames = FALSE, options = list(pageLength = 12, scrollX = TRUE))
	  })

	  output$neural_manifold_method_notes_table <- DT::renderDT({
	    datatable(stpd_neural_manifold_method_notes(lang = ui_language()), rownames = FALSE,
	                  options = list(pageLength = 9, scrollX = TRUE))
	  })

	  output$neural_manifold_event_geometry_table <- DT::renderDT({
	    dat <- neural_manifold_result()$event_geometry %||% data.frame()
	    if (is.null(dat) || nrow(dat) == 0L) dat <- data.frame(message = ui_inline_text("\u6682\u65E0\u53EF\u7528\u7684\u4E8B\u4EF6\u51E0\u4F55\u7ED3\u679C\u3002", "No event geometry is available."), stringsAsFactors = FALSE)
	    numeric_cols <- names(dat)[vapply(dat, is.numeric, logical(1))]
	    for (nm in numeric_cols) dat[[nm]] <- round(dat[[nm]], 6)
	    datatable(dat, rownames = FALSE, filter = "top",
	                  options = list(pageLength = 10, scrollX = TRUE))
	  })

	  output$neural_manifold_event_distance_table <- DT::renderDT({
	    dat <- neural_manifold_result()$event_distances %||% data.frame()
	    if (is.null(dat) || nrow(dat) == 0L) dat <- data.frame(message = ui_inline_text("\u6682\u65E0\u53EF\u7528\u7684\u4E8B\u4EF6\u8DDD\u79BB\u68C0\u9A8C\u3002", "No event-distance tests are available."), stringsAsFactors = FALSE)
	    numeric_cols <- names(dat)[vapply(dat, is.numeric, logical(1))]
	    for (nm in numeric_cols) dat[[nm]] <- round(dat[[nm]], 6)
	    datatable(dat, rownames = FALSE, filter = "top",
	                  options = list(pageLength = 10, scrollX = TRUE))
	  })

	  output$neural_manifold_event_dynamics_table <- DT::renderDT({
	    dyn <- neural_manifold_result()$event_dynamics %||% data.frame()
	    dec <- tryCatch(stpd_neural_event_label_decoding(neural_manifold_result()$features, seed = input$neural_manifold_seed %||% 1,
	                                                     n_perm = input$neural_manifold_event_permutations %||% 199),
	                    error = function(e) data.frame(
	                      metric = "event_label_decoding",
	                      value = NA_real_,
	                      status = "failed",
	                      note = paste(ui_inline_text("\u6280\u672F\u8BE6\u60C5\uFF1A", "Technical details:"), conditionMessage(e)),
	                      stringsAsFactors = FALSE
	                    ))
	    if (is.data.frame(dyn) && nrow(dyn) > 0L && !("message" %in% names(dyn))) {
	      dyn_long <- data.frame(
	        metric = paste0("event_dynamics_", dyn$event_state),
	        value = dyn$delta_speed_post_minus_pre,
	        status = "ok",
	        note = ui_inline_text(
	          paste0("\u4E8B\u4EF6\u540E-\u4E8B\u4EF6\u524D\u901F\u5EA6\u5DEE=", signif(dyn$delta_speed_post_minus_pre, 5),
	                 "\uFF1B\u4E8B\u4EF6\u540E-\u4E8B\u4EF6\u524D\u66F2\u7387\u5DEE=", signif(dyn$delta_curvature_post_minus_pre, 5),
	                 "\uFF1B\u8D77\u59CB\u4E8B\u4EF6\u6570=", dyn$n_onsets),
	          paste0("delta speed post-pre=", signif(dyn$delta_speed_post_minus_pre, 5),
	                 "; delta curvature post-pre=", signif(dyn$delta_curvature_post_minus_pre, 5),
	                 "; n_onsets=", dyn$n_onsets)
	        ),
	        stringsAsFactors = FALSE
	      )
	      dat <- rbind(dec, dyn_long)
	    } else {
	      dat <- dec
	    }
	    numeric_cols <- names(dat)[vapply(dat, is.numeric, logical(1))]
	    for (nm in numeric_cols) dat[[nm]] <- round(dat[[nm]], 6)
	    datatable(dat, rownames = FALSE, filter = "top",
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
	    dat <- if (length(rows) > 0L) dplyr::bind_rows(rows) else data.frame(message = ui_inline_text("\u6682\u65E0\u53EF\u7528\u7684\u4E8B\u4EF6\u89E6\u53D1\u8F68\u8FF9\u3002", "No event-triggered trajectory is available."), stringsAsFactors = FALSE)
	    numeric_cols <- names(dat)[vapply(dat, is.numeric, logical(1))]
	    for (nm in numeric_cols) dat[[nm]] <- round(dat[[nm]], 6)
	    datatable(dat, rownames = FALSE, filter = "top",
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
	    datatable(out, rownames = FALSE, filter = "top",
	                  options = list(pageLength = 15, scrollX = TRUE))
	  })

	  output$neural_manifold_loading_table <- DT::renderDT({
	    dat <- neural_manifold_result()$loadings %||% data.frame()
	    if (is.null(dat) || nrow(dat) == 0L) {
	      dat <- data.frame(message = ui_inline_text("\u8BE5\u65B9\u6CD5\u6CA1\u6709\u7EBF\u6027\u8F7D\u8377/\u79C1\u6709\u65B9\u5DEE\u8868\u3002", "No linear loading/private-variance table is available for this method."), stringsAsFactors = FALSE)
	    } else {
	      numeric_cols <- names(dat)[vapply(dat, is.numeric, logical(1))]
	      for (nm in numeric_cols) dat[[nm]] <- round(dat[[nm]], 6)
	    }
	    datatable(dat, rownames = FALSE, filter = "top",
	                  options = list(pageLength = 15, scrollX = TRUE))
	  })

	  output$neural_manifold_summary_table <- DT::renderDT({
	    res <- neural_manifold_result()
	    dat <- res$window_summary %||% data.frame()
	    if (is.null(dat) || nrow(dat) == 0L) dat <- data.frame(message = ui_inline_text("\u6682\u65E0\u53EF\u7528\u7684\u65F6\u95F4\u7A97\u6458\u8981\u3002", "No window summary is available."), stringsAsFactors = FALSE)
	    datatable(dat, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
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
      legacy_promote_possible <-
        stpd_legacy_final_audit_promote_from_policy(ds, audit_policy)
      ds <- stpd_apply_final_audit(
        ds,
        selected_trains = names(td),
        promote_possible = legacy_promote_possible,
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
    validate(need(selection_has_points(sel), ui_text("no_box_points")))
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
    validate(need(selection_has_points(sel), ui_text("box_select_first")))
    td <- current_trains()
    axis_tbl <- selected_axis_table()
    rng <- selection_xy_range(sel)
    x_min_sec <- rng$x_min_sec
    x_max_sec <- rng$x_max_sec
    y_center <- rng$y_center
    all_y_vals <- sort(unique(axis_tbl$y))
    validate(need(length(all_y_vals) > 0, ui_text("no_train_rows")))
    y_val <- all_y_vals[which.min(abs(all_y_vals - y_center))]
    train_here <- unique(axis_tbl$train[axis_tbl$y == y_val])
    validate(need(length(train_here) == 1, ui_text("selection_one_train_row")))
    tr <- train_here[1]
    dat_tr <- td[[tr]]
    n <- nrow(dat_tr)
    validate(need(n > 1, ui_text("train_too_short")))
    t_align <- dat_tr$timestamp_sec - dat_tr$timestamp_sec[1]
    t_start <- t_align[-length(t_align)]
    t_end <- t_align[-1]
    idx_ISI <- 2:n
    covered <- which(t_start < x_max_sec & t_end > x_min_sec)
    validate(need(length(covered) > 0, ui_text("no_isi_in_selection")))
    list(train = tr, idx = idx_ISI[covered])
  }

  apply_manual_selection <- function(sel, pat, notify = TRUE) {
    validate(need(selection_has_points(sel), ui_text("box_select_first")))
    td <- current_trains()
    pat_raw <- tolower(trimws(as.character(pat)))
    neg_label <- pat_raw %in% c("not_burst", "hard_negative_burst", "not burst", "not-burst")
    pat <- if (neg_label) "not_burst" else stpd_normalize_pattern_label(pat, fill_blank_others = FALSE)[1]
    validate(need(pat %in% c("burst", "long_burst", "pause", "tonic", "high_frequency_tonic", "high_frequency_spiking", "others", "not_burst"), ui_text("select_valid_pattern")))

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
        validate(need(length(train_here) == 1, ui_text("selection_one_train")))
        tr <- train_here[1]
        dat_tr <- td[[tr]]
        n <- nrow(dat_tr)
        validate(need(n > 1, ui_text("train_too_short")))
        idx_sel <- sort(unique(idx_sel))
        validate(need(length(idx_sel) >= 2, ui_text("need_two_spikes")))
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
        validate(need(length(all_isi_idx) > 0, ui_text("no_isi_from_selection")))
        push_ui_undo(
          "manual_label_edit",
          ui_inline_text(paste0("\u6807\u8BB0 MANUAL ", pat), paste0("Mark MANUAL ", pat)),
          dataset_ids = rv$current_id, train_ids = tr,
          target_tracks = "manual", target_count = length(all_isi_idx)
        )
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
        if (isTRUE(notify)) {
          showNotification(
            ui_inline_text(
              paste0("\u5DF2\u5C06 ", length(all_isi_idx), " \u4E2A ISI \u6807\u8BB0\u4E3A MANUAL ", pat, "\u3002"),
              paste0("Marked ", length(all_isi_idx), " ISI(s) as MANUAL ", pat, ".")
            ),
            type = "message",
            duration = 2
          )
        }
        return(invisible(TRUE))
      }
    }

    # pause/others and fallback for burst/tonic: use the selected time range on
    # the nearest train row. This is also more robust in reduced LOD mode.
    loc <- selection_time_isi_indices(sel)
    dat_tr <- td[[loc$train]]
    push_ui_undo(
      "manual_label_edit",
      ui_inline_text(paste0("\u6807\u8BB0 MANUAL ", pat), paste0("Mark MANUAL ", pat)),
      dataset_ids = rv$current_id, train_ids = loc$train,
      target_tracks = "manual", target_count = length(loc$idx)
    )
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
    if (isTRUE(notify)) {
      showNotification(
        ui_inline_text(
          paste0("\u5DF2\u5C06 ", length(loc$idx), " \u4E2A ISI \u6807\u8BB0\u4E3A MANUAL ", pat, "\u3002"),
          paste0("Marked ", length(loc$idx), " ISI(s) as MANUAL ", pat, ".")
        ),
        type = "message",
        duration = 2
      )
    }
    invisible(TRUE)
  }


  }, envir = server_env)
}
