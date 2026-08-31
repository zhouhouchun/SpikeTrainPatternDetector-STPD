#' Launch an interactive Pause error reviewer
#'
#' The reviewer is a read-only diagnostic Shiny app. It displays the same raw
#' spike timestamps in three aligned lanes: raw spikes, the frozen reference
#' labels, and the frozen STPD prediction. It is intentionally separate from
#' the main detector app so held-out validation labels cannot affect detection.
#'
#' @param repo Repository root containing validation artifacts.
#' @param raw_spike_csv CSV with one real-data spike train per column.
#' @param real_validation_dir Frozen LOGO real-data validation output directory.
#' @param manual10_dir Directory containing the three manual-10 validation sets.
#' @param browser Whether to open a browser when launching the local app.
#' @param host Host for the local Shiny server.
#' @param port Optional port. `NULL` lets Shiny choose an available port.
#'
#' @return A running Shiny application.
#' @export
launch_pause_error_reviewer <- function(
  repo = getwd(), raw_spike_csv, real_validation_dir = NULL,
  manual10_dir = NULL, browser = TRUE, host = "127.0.0.1", port = NULL
) {
  repo <- normalizePath(repo, mustWork = TRUE)
  if (missing(raw_spike_csv) || !nzchar(as.character(raw_spike_csv)[1])) {
    stop("raw_spike_csv is required for the real-data review lane.", call. = FALSE)
  }
  raw_spike_csv <- normalizePath(raw_spike_csv, mustWork = TRUE)
  real_validation_dir <- real_validation_dir %||% file.path(
    repo, "test-results", "publication_validation",
    "real_grechishnikova_2017_reference_eligible_interburst_pause_structural_20260824"
  )
  manual10_dir <- manual10_dir %||% file.path(
    repo, "test-results", "simulation_60", "manual10_learned"
  )
  bundles <- stpd_pause_reviewer_load_bundles(
    real_validation_dir, raw_spike_csv, manual10_dir
  )
  shiny::runApp(
    shiny::shinyApp(stpd_pause_reviewer_ui(), stpd_pause_reviewer_server(bundles)),
    host = host, port = port, launch.browser = browser
  )
}

stpd_pause_reviewer_normalize_label <- function(x) {
  out <- tolower(trimws(as.character(x)))
  out[is.na(out) | !nzchar(out) | out %in% c("none", "others", "unlabeled")] <- "other"
  out[out %in% c("long_burst", "possible_burst")] <- "burst"
  out
}

stpd_pause_reviewer_error_label <- function(x) {
  labels <- c(
    fn_other = "\u6F0F\u68C0\uFF1A\u4EBA\u5DE5/\u6A21\u62DF Pause \u2192 STPD other",
    fn_burst = "\u6F0F\u68C0\uFF1A\u4EBA\u5DE5/\u6A21\u62DF Pause \u2192 STPD Burst",
    fp_other = "\u9519\u68C0\uFF1A\u4EBA\u5DE5/\u6A21\u62DF other \u2192 STPD Pause",
    fp_burst = "\u9519\u68C0\uFF1A\u4EBA\u5DE5/\u6A21\u62DF Burst \u2192 STPD Pause"
  )
  unname(labels[as.character(x)])
}

stpd_pause_reviewer_pattern_label <- function(x) {
  labels <- c(
    burst = "Burst", pause = "Pause", tonic = "Tonic",
    high_frequency_spiking = "Broad HFS", high_frequency_tonic = "HFT", other = "Other"
  )
  out <- unname(labels[as.character(x)])
  out[is.na(out)] <- as.character(x)[is.na(out)]
  out
}

stpd_pause_reviewer_palette <- function() c(
  burst = "#2563EB", pause = "#D9485F", tonic = "#4B5563",
  high_frequency_spiking = "#7C3AED", high_frequency_tonic = "#A855F7", other = "#CBD5E1",
  fn_other = "#B42318", fn_burst = "#7F1D1D", fp_other = "#9A6700", fp_burst = "#6B4F00"
)

stpd_pause_reviewer_error_type <- function(truth, prediction) {
  truth <- stpd_pause_reviewer_normalize_label(truth)
  prediction <- stpd_pause_reviewer_normalize_label(prediction)
  out <- rep(NA_character_, length(truth))
  out[truth == "pause" & prediction == "other"] <- "fn_other"
  out[truth == "pause" & prediction == "burst"] <- "fn_burst"
  out[truth == "other" & prediction == "pause"] <- "fp_other"
  out[truth == "burst" & prediction == "pause"] <- "fp_burst"
  out
}

stpd_pause_reviewer_require_columns <- function(x, columns, source_name) {
  missing <- setdiff(columns, names(x))
  if (length(missing)) stop(source_name, " is missing: ", paste(missing, collapse = ", "), call. = FALSE)
  invisible(x)
}

stpd_pause_reviewer_raw_spikes <- function(raw_spike_csv) {
  raw <- utils::read.csv(raw_spike_csv, check.names = FALSE, stringsAsFactors = FALSE)
  out <- lapply(raw, function(x) {
    time <- suppressWarnings(as.numeric(x))
    sort(time[is.finite(time)])
  })
  out[lengths(out) >= 2L]
}

stpd_pause_reviewer_add_times <- function(intervals, spikes) {
  intervals$Right_Spike_Index <- suppressWarnings(as.integer(intervals$Right_Spike_Index))
  intervals$left_time_sec <- NA_real_
  intervals$right_time_sec <- NA_real_
  for (train in unique(intervals$Train_ID)) {
    ii <- which(intervals$Train_ID == train)
    time <- spikes[[train]]
    if (is.null(time) || length(time) < 2L) next
    right <- intervals$Right_Spike_Index[ii]
    valid <- is.finite(right) & right >= 2L & right <= length(time)
    intervals$left_time_sec[ii[valid]] <- time[right[valid] - 1L]
    intervals$right_time_sec[ii[valid]] <- time[right[valid]]
  }
  intervals
}

stpd_pause_reviewer_catalog <- function(intervals, spikes, context_sec = 0.15) {
  errors <- intervals[!is.na(intervals$error_type), , drop = FALSE]
  empty <- data.frame(event_id = character(), Train_ID = character(), error_type = character(),
    start_isi = integer(), end_isi = integer(), start_time_sec = numeric(), end_time_sec = numeric(),
    context_start_sec = numeric(), context_end_sec = numeric(), n_isi = integer(), stringsAsFactors = FALSE)
  if (!nrow(errors)) return(empty)
  rows <- list(); row_id <- 0L
  for (train in sort(unique(errors$Train_ID), method = "radix")) {
    for (kind in sort(unique(errors$error_type[errors$Train_ID == train]), method = "radix")) {
      z <- errors[errors$Train_ID == train & errors$error_type == kind, , drop = FALSE]
      z <- z[order(z$Right_Spike_Index), , drop = FALSE]
      run_id <- cumsum(c(TRUE, diff(z$Right_Spike_Index) != 1L))
      for (run in split(z, run_id)) {
        raw_time <- spikes[[train]]
        start_time <- min(run$left_time_sec, na.rm = TRUE); end_time <- max(run$right_time_sec, na.rm = TRUE)
        if (is.null(raw_time) || !is.finite(start_time) || !is.finite(end_time)) next
        pad <- max(context_sec, min(1, 3 * (end_time - start_time)))
        row_id <- row_id + 1L
        rows[[row_id]] <- data.frame(
          event_id = paste(train, kind, min(run$Right_Spike_Index), max(run$Right_Spike_Index), sep = "__"),
          Train_ID = train, error_type = kind, start_isi = min(run$Right_Spike_Index),
          end_isi = max(run$Right_Spike_Index), start_time_sec = start_time, end_time_sec = end_time,
          context_start_sec = max(min(raw_time), start_time - pad), context_end_sec = min(max(raw_time), end_time + pad),
          n_isi = nrow(run), stringsAsFactors = FALSE
        )
      }
    }
  }
  if (!length(rows)) return(empty)
  out <- do.call(rbind, rows)
  out[order(out$Train_ID, out$start_isi, out$end_isi, method = "radix"), , drop = FALSE]
}

stpd_pause_reviewer_bundle <- function(intervals, spikes, id, label, provenance) {
  stpd_pause_reviewer_require_columns(intervals, c("Train_ID", "Right_Spike_Index", "Truth", "Prediction"), label)
  intervals$Train_ID <- as.character(intervals$Train_ID)
  intervals <- intervals[intervals$Train_ID %in% names(spikes), , drop = FALSE]
  intervals$Truth <- stpd_pause_reviewer_normalize_label(intervals$Truth)
  intervals$Prediction <- stpd_pause_reviewer_normalize_label(intervals$Prediction)
  intervals$error_type <- stpd_pause_reviewer_error_type(intervals$Truth, intervals$Prediction)
  intervals <- stpd_pause_reviewer_add_times(intervals, spikes)
  intervals <- intervals[is.finite(intervals$Right_Spike_Index) & is.finite(intervals$left_time_sec) & is.finite(intervals$right_time_sec), , drop = FALSE]
  list(id = id, label = label, provenance = provenance, intervals = intervals, spikes = spikes,
       catalog = stpd_pause_reviewer_catalog(intervals, spikes),
       trains = sort(unique(intervals$Train_ID), method = "radix"))
}

stpd_pause_reviewer_load_bundles <- function(real_validation_dir, raw_spike_csv, manual10_dir) {
  real_validation_dir <- normalizePath(real_validation_dir, mustWork = TRUE)
  manual10_dir <- normalizePath(manual10_dir, mustWork = TRUE)
  real_path <- file.path(real_validation_dir, "heldout_interval_predictions.csv")
  if (!file.exists(real_path)) stop("Missing frozen real-data prediction table: ", real_path, call. = FALSE)
  real <- utils::read.csv(real_path, check.names = FALSE, stringsAsFactors = FALSE)
  stpd_pause_reviewer_require_columns(real, "Axis", real_path)
  real <- real[as.character(real$Axis) == "event", , drop = FALSE]
  bundles <- list(real = stpd_pause_reviewer_bundle(
    real, stpd_pause_reviewer_raw_spikes(raw_spike_csv), "real",
    "\u771F\u5B9E\u6570\u636E\uFF1A\u4EBA\u5DE5\u6807\u8BB0 / held-out STPD",
    "1 \u540D\u60A3\u8005\u3001\u53CC\u4FA7 STN\uFF1B\u6BCF\u6761 train \u4F7F\u7528\u5176\u6240\u5C5E LOGO held-out fold \u7684\u51BB\u7ED3 STPD \u9884\u6D4B\u3002"
  ))
  titles <- c("\u77ED\u65F6\u95F4\u5C3A\u5EA6", "\u4E2D\u65F6\u95F4\u5C3A\u5EA6", "\u957F\u65F6\u95F4\u5C3A\u5EA6")
  for (class_id in seq_along(titles)) {
    path <- file.path(manual10_dir, paste0("class_", class_id), "manual10_learned_interval_predictions.csv")
    if (!file.exists(path)) next
    z <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
    stpd_pause_reviewer_require_columns(z, c("Left_Spike_Time_s", "Right_Spike_Time_s"), path)
    z$Train_ID <- as.character(z$Train_ID)
    spikes <- lapply(split(z, z$Train_ID), function(q) {
      q <- q[order(q$Right_Spike_Index), , drop = FALSE]
      c(q$Left_Spike_Time_s[[1L]], q$Right_Spike_Time_s)
    })
    bundles[[paste0("sim", class_id)]] <- stpd_pause_reviewer_bundle(
      z, spikes, paste0("sim", class_id), paste0("\u6A21\u62DF\u6570\u636E\uFF1A", titles[[class_id]], " / manual-10 held-out"),
      paste0("\u6A21\u62DF\u7C7B\u522B ", class_id, "\uFF1B10 \u6761 calibration train \u4E0E 10 \u6761\u72EC\u7ACB validation train \u5206\u79BB\uFF0C\u4EC5\u663E\u793A\u540E\u8005\u7684\u51BB\u7ED3\u9884\u6D4B\u3002")
    )
  }
  bundles
}

stpd_pause_reviewer_ui <- function() {
  shiny::fluidPage(
    shiny::tags$head(shiny::tags$style(shiny::HTML("\n      .pause-review-note{color:#475569;line-height:1.45;margin:8px 0 14px}.pause-review-card{background:#F8FAFC;border:1px solid #DCE5EF;border-radius:10px;padding:12px 14px;margin-bottom:12px}.pause-review-title{font-weight:700;color:#0F172A;margin-bottom:3px}.pause-review-help{font-size:13px;color:#475569}\n    "))),
    shiny::titlePanel("Pause \u9519\u68C0 / \u6F0F\u68C0\u4EA4\u4E92\u5BA1\u9605\u5668"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(width = 3,
        shiny::radioButtons("pause_review_source", "\u6570\u636E\u6765\u6E90", choices = c("\u8F7D\u5165\u4E2D\u2026" = "loading")),
        shiny::selectInput("pause_review_train", "Spike train", choices = c("\u8F7D\u5165\u4E2D\u2026" = "loading")),
        shiny::checkboxGroupInput("pause_review_error_types", "\u663E\u793A\u7684\u8BEF\u5DEE\u7C7B\u578B",
          choices = c("\u6F0F\u68C0\uFF1APause \u2192 other" = "fn_other", "\u6F0F\u68C0\uFF1APause \u2192 Burst" = "fn_burst", "\u9519\u68C0\uFF1Aother \u2192 Pause" = "fp_other", "\u9519\u68C0\uFF1ABurst \u2192 Pause" = "fp_burst"),
          selected = c("fn_other", "fn_burst", "fp_other", "fp_burst")),
        shiny::uiOutput("pause_review_event_ui"),
        shiny::fluidRow(shiny::column(6, shiny::actionButton("pause_review_previous", "\u2190 \u4E0A\u4E00\u4E2A", width = "100%")), shiny::column(6, shiny::actionButton("pause_review_next", "\u4E0B\u4E00\u4E2A \u2192", width = "100%"))),
        shiny::tags$hr(), shiny::uiOutput("pause_review_window_ui"),
        shiny::checkboxGroupInput("pause_review_patterns", "\u663E\u793A\u7684\u6A21\u5F0F\u6761\u5E26",
          choices = c("Pause" = "pause", "Burst" = "burst", "Tonic" = "tonic", "Broad HFS" = "high_frequency_spiking", "HFT" = "high_frequency_tonic", "Other\uFF08\u80CC\u666F\uFF09" = "other"), selected = c("pause", "burst")),
        shiny::checkboxInput("pause_review_show_error_overlays", "\u5728\u4EBA\u5DE5\u4E0E STPD \u884C\u4E0A\u53E0\u52A0\u8BEF\u5DEE\u6807\u8BB0", TRUE),
        shiny::tags$div(class = "pause-review-note", shiny::tags$b("\u64CD\u4F5C\uFF1A"), "\u62D6\u52A8\u56FE\u5F62\u53EF\u5DE6\u53F3\u5E73\u79FB\uFF1B\u9F20\u6807\u6EDA\u8F6E/\u5DE5\u5177\u680F\u53EF\u7F29\u653E\uFF1B\u56FE\u5E95\u90E8\u8303\u56F4\u6761\u53EF\u8DF3\u5230\u6574\u6761 spike train \u7684\u4EFB\u610F\u65F6\u95F4\u6BB5\u3002")
      ),
      shiny::mainPanel(width = 9, shiny::uiOutput("pause_review_provenance"),
        shiny::tags$div(class = "pause-review-card", shiny::tags$div(class = "pause-review-title", "\u4E09\u6761\u5E73\u884C spike train"), shiny::tags$div(class = "pause-review-help", "\u4E09\u884C\u4F7F\u7528\u5B8C\u5168\u76F8\u540C\u7684\u539F\u59CB spike \u65F6\u95F4\u6233\uFF1A\u539F\u59CB spike\u3001\u4EBA\u5DE5/\u6A21\u62DF\u53C2\u8003\u3001STPD\u3002\u989C\u8272\u6761\u662F\u5BF9\u5E94 ISI \u7684\u6A21\u5F0F\u6807\u7B7E\uFF1B\u865A\u7EBF/\u70B9\u7EBF\u6807\u8BB0\u9519\u8BEF\u3002")),
        plotly::plotlyOutput("pause_review_plot", height = "720px"), shiny::uiOutput("pause_review_selection_note")
      )
    )
  )
}

stpd_pause_reviewer_server <- function(bundles) {
  force(bundles)
  function(input, output, session) {
    source_labels <- vapply(bundles, `[[`, character(1), "label")
    session$onFlushed(function() {
      shiny::updateRadioButtons(
        session, "pause_review_source", choiceNames = unname(source_labels),
        choiceValues = names(source_labels), selected = names(source_labels)[[1L]]
      )
    }, once = TRUE)
    source_id <- shiny::reactive({
      id <- as.character(input$pause_review_source %||% "")[1L]
      if (!isTRUE(id %in% names(bundles))) id <- names(bundles)[[1L]]
      id
    })
    source_bundle <- shiny::reactive({ bundles[[source_id()]] })
    selected_train_id <- shiny::reactive({
      b <- source_bundle()
      train <- as.character(input$pause_review_train %||% "")[1L]
      if (!isTRUE(train %in% b$trains)) train <- b$trains[[1L]]
      train
    })
    selected_train_catalog <- shiny::reactive({
      b <- source_bundle(); train <- selected_train_id()
      types <- as.character(input$pause_review_error_types %||% character(0))
      if (!length(types)) types <- unique(b$catalog$error_type)
      b$catalog[b$catalog$Train_ID == train & b$catalog$error_type %in% types, , drop = FALSE]
    })
    shiny::observeEvent(source_id(), {
      b <- source_bundle(); counts <- table(b$catalog$Train_ID); ordered <- names(sort(counts, decreasing = TRUE))
      trains <- c(ordered, setdiff(b$trains, ordered))
      shiny::updateSelectInput(session, "pause_review_train", choices = trains, selected = trains[[1L]])
    }, ignoreInit = FALSE)
    output$pause_review_event_ui <- shiny::renderUI({
      events <- selected_train_catalog()
      if (!nrow(events)) return(shiny::tags$div(class = "pause-review-note", "\u8BE5\u7B5B\u9009\u4E0B\u6CA1\u6709 Pause \u9519\u68C0/\u6F0F\u68C0\u3002"))
      labels <- paste0(stpd_pause_reviewer_error_label(events$error_type), " | ISI ", events$start_isi, "\u2013", events$end_isi, " | ", sprintf("%.3f\u2013%.3f s", events$start_time_sec, events$end_time_sec))
      shiny::selectInput("pause_review_event", "\u9519\u8BEF\u7247\u6BB5", choices = stats::setNames(events$event_id, labels), selected = events$event_id[[1L]])
    })
    selected_event <- shiny::reactive({
      events <- selected_train_catalog(); if (!nrow(events)) return(NULL)
      id <- as.character(input$pause_review_event %||% events$event_id[[1L]])[1L]; z <- events[events$event_id == id, , drop = FALSE]
      if (!nrow(z)) z <- events[1L, , drop = FALSE]; z[1L, , drop = FALSE]
    })
    navigate <- function(direction) {
      events <- selected_train_catalog(); if (!nrow(events)) return()
      current <- as.character(input$pause_review_event %||% events$event_id[[1L]])[1L]; position <- match(current, events$event_id)
      if (is.na(position)) position <- 1L; position <- ((position - 1L + direction) %% nrow(events)) + 1L
      shiny::updateSelectInput(session, "pause_review_event", selected = events$event_id[[position]])
    }
    shiny::observeEvent(input$pause_review_previous, navigate(-1L)); shiny::observeEvent(input$pause_review_next, navigate(1L))
    output$pause_review_window_ui <- shiny::renderUI({
      b <- source_bundle(); train <- selected_train_id(); time <- b$spikes[[train]]
      shiny::validate(shiny::need(length(time) >= 2L, "\u8BE5 train \u6CA1\u6709\u8DB3\u591F\u539F\u59CB spike\u3002")); event <- selected_event()
      value <- if (is.null(event)) range(time) else c(event$context_start_sec, event$context_end_sec)
      shiny::sliderInput("pause_review_window", "\u663E\u793A\u65F6\u95F4\u7A97\uFF08s\uFF09", min = min(time), max = max(time), value = value, step = max(1e-6, diff(range(time)) / 300), ticks = FALSE)
    })
    output$pause_review_provenance <- shiny::renderUI({ b <- source_bundle(); shiny::tags$div(class = "pause-review-card", shiny::tags$div(class = "pause-review-title", b$label), shiny::tags$div(class = "pause-review-help", b$provenance)) })
    output$pause_review_selection_note <- shiny::renderUI({ event <- selected_event(); if (is.null(event)) return(shiny::tags$div(class = "pause-review-note", "\u8BF7\u9009\u62E9\u542B\u8BEF\u5DEE\u7684 spike train\u3002")); shiny::tags$div(class = "pause-review-note", shiny::tags$b(stpd_pause_reviewer_error_label(event$error_type)), sprintf("\uFF1B\u5F53\u524D\u9519\u8BEF\u7247\u6BB5\u542B %d \u4E2A ISI\uFF08ISI %d\u2013%d\uFF09\u3002", event$n_isi, event$start_isi, event$end_isi)) })
    output$pause_review_plot <- plotly::renderPlotly({
      b <- source_bundle(); train <- selected_train_id(); time <- b$spikes[[train]]
      shiny::validate(shiny::need(length(time) >= 2L, "\u8BE5 train \u6CA1\u6709\u8DB3\u591F\u539F\u59CB spike\u3002")); window <- suppressWarnings(as.numeric(input$pause_review_window)); if (length(window) != 2L || any(!is.finite(window)) || diff(window) <= 0) window <- range(time)
      intervals <- b$intervals[b$intervals$Train_ID == train, , drop = FALSE]; palette <- stpd_pause_reviewer_palette(); patterns <- unique(c(as.character(input$pause_review_patterns %||% character(0)), "pause"))
      ticks <- data.frame(time_sec = time, stringsAsFactors = FALSE)
      add_ticks <- function(p, lane) { d <- ticks; d$y0 <- lane - .27; d$y1 <- lane + .27; plotly::add_segments(p, data = d, x = ~time_sec, xend = ~time_sec, y = ~y0, yend = ~y1, inherit = FALSE, showlegend = FALSE, line = list(color = "#111827", width = 1), hoverinfo = "text", text = ~paste0("Raw spike<br>time: ", sprintf("%.6f s", time_sec))) }
      add_labels <- function(p, label_column, lane, lane_name) {
        values <- stpd_pause_reviewer_normalize_label(intervals[[label_column]])
        for (pattern in patterns) { d <- intervals[values == pattern, , drop = FALSE]; if (!nrow(d)) next; d$hover <- paste0(lane_name, ": ", stpd_pause_reviewer_pattern_label(pattern), "<br>ISI index: ", d$Right_Spike_Index, "<br>left/right: ", sprintf("%.6f", d$left_time_sec), " / ", sprintf("%.6f s", d$right_time_sec), "<br>Reference: ", stpd_pause_reviewer_pattern_label(d$Truth), "<br>STPD: ", stpd_pause_reviewer_pattern_label(d$Prediction)); p <- plotly::add_segments(p, data = d, x = ~left_time_sec, xend = ~right_time_sec, y = lane, yend = lane, inherit = FALSE, name = paste0(lane_name, " \u00B7 ", stpd_pause_reviewer_pattern_label(pattern)), showlegend = TRUE, line = list(color = palette[[pattern]] %||% "#64748B", width = 10), hoverinfo = "text", text = ~hover) }
        p
      }
      add_errors <- function(p) {
        if (!isTRUE(input$pause_review_show_error_overlays)) return(p); errors <- intervals[!is.na(intervals$error_type), , drop = FALSE]
        for (kind in unique(errors$error_type)) { d <- errors[errors$error_type == kind, , drop = FALSE]; if (!nrow(d)) next; d$hover <- paste0(stpd_pause_reviewer_error_label(kind), "<br>ISI index: ", d$Right_Spike_Index, "<br>Reference: ", stpd_pause_reviewer_pattern_label(d$Truth), "<br>STPD: ", stpd_pause_reviewer_pattern_label(d$Prediction)); dash <- if (grepl("^fn", kind)) "dash" else "dot"; for (lane in c(2.34, 1.34)) p <- plotly::add_segments(p, data = d, x = ~left_time_sec, xend = ~right_time_sec, y = lane, yend = lane, inherit = FALSE, name = stpd_pause_reviewer_error_label(kind), showlegend = identical(lane, 2.34), line = list(color = palette[[kind]], width = 3, dash = dash), hoverinfo = if (identical(lane, 2.34)) "text" else "skip", text = ~hover) }
        p
      }
      p <- plotly::plot_ly(source = "pause_error_reviewer"); p <- add_ticks(p, 3); p <- add_ticks(p, 2); p <- add_ticks(p, 1); p <- add_labels(p, "Truth", 2, "\u4EBA\u5DE5/\u6A21\u62DF\u53C2\u8003"); p <- add_labels(p, "Prediction", 1, "STPD"); p <- add_errors(p)
      plotly::config(
        plotly::layout(p, title = list(text = paste0(train, "\uFF1APause \u8BEF\u5DEE\u5BA1\u9605"), x = .02), dragmode = "pan", hovermode = "closest", xaxis = list(title = "\u539F\u59CB spike \u65F6\u95F4\uFF08s\uFF09", range = window, rangeslider = list(visible = TRUE), fixedrange = FALSE), yaxis = list(title = "", range = c(.5, 3.5), tickvals = c(3, 2, 1), ticktext = c("\u539F\u59CB spike train", "\u4EBA\u5DE5/\u6A21\u62DF\u53C2\u8003", "STPD"), fixedrange = TRUE, showgrid = FALSE, zeroline = FALSE), legend = list(orientation = "h", x = 0, y = -.20), margin = list(l = 145, r = 30, t = 55, b = 130), paper_bgcolor = "white", plot_bgcolor = "white"),
        scrollZoom = TRUE, displaylogo = FALSE
      )
    })
  }
}
