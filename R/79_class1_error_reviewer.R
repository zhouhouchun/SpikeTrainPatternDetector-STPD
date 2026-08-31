#' Launch an interactive Class-1 simulation error reviewer
#'
#' Read-only diagnostic app for the current frozen small-timescale validation split.
#' The four aligned lanes use the same spike timestamps: raw spikes, generator
#' reference, STPD Event/Gap output, and STPD Broad-HFS State output.  This
#' separation is deliberate: a Burst Event may coexist with an HFS State.
#'
#' @param scale_runs_rds Frozen diagnostic bundle. The x1/current run is used.
#' @param interval_truth_csv Simulator interval ground truth.
#' @param browser Whether to open a browser automatically.
#' @param host Local server host.
#' @param port Local server port.
#' @return A running Shiny application.
#' @export
launch_class1_error_reviewer <- function(
    scale_runs_rds,
    interval_truth_csv,
    browser = TRUE, host = "127.0.0.1", port = 7319L) {
  bundle <- stpd_class1_reviewer_load_bundle(
    scale_runs_rds = scale_runs_rds,
    interval_truth_csv = interval_truth_csv
  )
  shiny::runApp(
    shiny::shinyApp(
      stpd_class1_reviewer_ui(),
      stpd_class1_reviewer_server(bundle)
    ),
    host = host, port = port, launch.browser = browser
  )
}

stpd_class1_reviewer_normalize <- function(x) {
  out <- tolower(trimws(as.character(x)))
  out[is.na(out) | !nzchar(out) | out %in% c("none", "others", "unlabeled")] <-
    "other"
  out[out %in% c("long_burst", "possible_burst", "prolonged_burst")] <-
    "burst"
  out[out %in% c("high_frequency_tonic", "hf_irregular")] <-
    "high_frequency_spiking"
  out
}

stpd_class1_reviewer_palette <- function() c(
  burst = "#2563EB", pause = "#D9485F", tonic = "#64748B",
  high_frequency_spiking = "#7C3AED", other = "#CBD5E1",
  fn = "#B42318", fp = "#9A6700", overlap = "#0F766E"
)

stpd_class1_reviewer_pattern_label <- function(x) {
  labels <- c(
    burst = "Burst", pause = "Pause", tonic = "Tonic",
    high_frequency_spiking = "Broad HFS", other = "Other"
  )
  out <- unname(labels[as.character(x)])
  out[is.na(out)] <- as.character(x)[is.na(out)]
  out
}

stpd_class1_reviewer_mode_label <- function(x) {
  unname(c(
    burst = "Burst", pause = "Pause", broad_hfs = "Broad HFS",
    hfs_on_burst = "HFS \u8986\u76D6\u751F\u6210\u5668 Burst\uFF08\u8BED\u4E49\u5BA1\u67E5\uFF09"
  )[as.character(x)])
}

stpd_class1_reviewer_load_bundle <- function(
    scale_runs_rds, interval_truth_csv) {
  if (!file.exists(scale_runs_rds)) {
    stop("Missing frozen scale diagnostic: ", scale_runs_rds, call. = FALSE)
  }
  if (!file.exists(interval_truth_csv)) {
    stop("Missing simulator interval truth: ", interval_truth_csv,
         call. = FALSE)
  }
  runs <- readRDS(scale_runs_rds)
  scale <- vapply(runs, function(x) as.numeric(x$scale_factor), numeric(1))
  run <- runs[[which.min(abs(scale - 1))]]$result
  joined <- as.data.frame(run$joined, stringsAsFactors = FALSE)
  required <- c(
    "Train_ID", "Right_Spike_Index", "Axis", "ISI_s", "Truth",
    "Prediction"
  )
  missing <- setdiff(required, names(joined))
  if (length(missing)) {
    stop("Frozen predictions are missing: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  truth <- utils::read.csv(
    interval_truth_csv, check.names = FALSE, stringsAsFactors = FALSE
  )
  truth <- truth[truth$Class == 1L & truth$Train_ID %in% unique(joined$Train_ID),
                 , drop = FALSE]
  truth$Right_Spike_Index <- as.integer(truth$Right_Spike_Index)
  if (!"Pause_Subtype" %in% names(truth)) truth$Pause_Subtype <- NA_character_
  truth$reference_pattern <- stpd_class1_reviewer_normalize(truth$Pattern)
  key <- c("Train_ID", "Right_Spike_Index")
  axis_table <- function(axis, prefix) {
    z <- joined[joined$Axis == axis, c(key, "Truth", "Prediction"), drop = FALSE]
    names(z)[names(z) == "Truth"] <- paste0(prefix, "_truth")
    names(z)[names(z) == "Prediction"] <- paste0(prefix, "_prediction")
    z[[paste0(prefix, "_truth")]] <- stpd_class1_reviewer_normalize(
      z[[paste0(prefix, "_truth")]]
    )
    z[[paste0(prefix, "_prediction")]] <- stpd_class1_reviewer_normalize(
      z[[paste0(prefix, "_prediction")]]
    )
    z
  }
  intervals <- merge(
    truth,
    axis_table("event", "event"),
    by = key, all.x = FALSE, all.y = FALSE, sort = FALSE
  )
  intervals <- merge(
    intervals,
    axis_table("state_hf_family", "hfs"),
    by = key, all.x = TRUE, all.y = FALSE, sort = FALSE
  )
  intervals$hfs_prediction[is.na(intervals$hfs_prediction)] <- "other"
  intervals$hfs_truth[is.na(intervals$hfs_truth)] <- "other"
  intervals <- intervals[order(
    intervals$Train_ID, intervals$Right_Spike_Index, method = "radix"
  ), , drop = FALSE]
  intervals$error_burst <- ifelse(
    intervals$event_truth == "burst" & intervals$event_prediction != "burst",
    "fn", ifelse(
      intervals$event_truth != "burst" & intervals$event_prediction == "burst",
      "fp", NA_character_
    )
  )
  intervals$error_pause <- ifelse(
    intervals$event_truth == "pause" & intervals$event_prediction != "pause",
    "fn", ifelse(
      intervals$event_truth != "pause" & intervals$event_prediction == "pause",
      "fp", NA_character_
    )
  )
  intervals$error_broad_hfs <- ifelse(
    intervals$hfs_truth == "high_frequency_spiking" &
      intervals$hfs_prediction != "high_frequency_spiking",
    "fn", ifelse(
      intervals$hfs_truth != "high_frequency_spiking" &
        intervals$hfs_prediction == "high_frequency_spiking",
      "fp", NA_character_
    )
  )
  intervals$error_hfs_on_burst <- ifelse(
    intervals$reference_pattern == "burst" &
      intervals$hfs_prediction == "high_frequency_spiking",
    "overlap", NA_character_
  )
  trains <- sort(unique(intervals$Train_ID), method = "radix")
  spikes <- lapply(split(intervals, intervals$Train_ID), function(z) {
    z <- z[order(z$Right_Spike_Index), , drop = FALSE]
    c(z$Left_Spike_Time_s[[1L]], z$Right_Spike_Time_s)
  })
  names(spikes) <- names(split(intervals, intervals$Train_ID))
  list(
    intervals = intervals, spikes = spikes, trains = trains,
    split = run$split, runtime = run$runtime,
    provenance = paste0(
      "Class 1 semantics-final held-out split; native timestamps; validation trains only. ",
      "Reference and prediction were joined by Train_ID + right-spike index."
    )
  )
}

stpd_class1_reviewer_error_column <- function(mode) {
  paste0("error_", as.character(mode))
}

stpd_class1_reviewer_target <- function(mode) {
  switch(
    as.character(mode), burst = "burst", pause = "pause",
    broad_hfs = "high_frequency_spiking", hfs_on_burst = "burst",
    "burst"
  )
}

stpd_class1_reviewer_catalog <- function(intervals, mode, types) {
  column <- stpd_class1_reviewer_error_column(mode)
  if (!(column %in% names(intervals))) return(data.frame())
  z <- intervals[!is.na(intervals[[column]]) & intervals[[column]] %in% types,
                 , drop = FALSE]
  if (!nrow(z)) return(data.frame())
  rows <- list()
  for (train in sort(unique(z$Train_ID), method = "radix")) {
    zz <- z[z$Train_ID == train, , drop = FALSE]
    for (kind in sort(unique(zz[[column]]), method = "radix")) {
      q <- zz[zz[[column]] == kind, , drop = FALSE]
      q <- q[order(q$Right_Spike_Index), , drop = FALSE]
      run_id <- cumsum(c(TRUE, diff(q$Right_Spike_Index) != 1L))
      for (run in split(q, run_id)) {
        duration <- max(run$Right_Spike_Time_s) - min(run$Left_Spike_Time_s)
        pad <- max(0.05, min(0.75, 3 * duration))
        time <- range(intervals$Left_Spike_Time_s[intervals$Train_ID == train],
                      intervals$Right_Spike_Time_s[intervals$Train_ID == train])
        rows[[length(rows) + 1L]] <- data.frame(
          event_id = paste(
            mode, train, kind, min(run$Right_Spike_Index),
            max(run$Right_Spike_Index), sep = "__"
          ),
          mode = mode, Train_ID = train, error_type = kind,
          start_isi = min(run$Right_Spike_Index),
          end_isi = max(run$Right_Spike_Index),
          start_time_sec = min(run$Left_Spike_Time_s),
          end_time_sec = max(run$Right_Spike_Time_s),
          context_start_sec = max(time[[1L]], min(run$Left_Spike_Time_s) - pad),
          context_end_sec = min(time[[2L]], max(run$Right_Spike_Time_s) + pad),
          n_isi = nrow(run), median_isi_ms = 1000 * stats::median(run$ISI_s),
          q90_isi_ms = 1000 * as.numeric(stats::quantile(
            run$ISI_s, 0.9, names = FALSE
          )),
          hfs_overlap_fraction = mean(
            run$hfs_prediction == "high_frequency_spiking", na.rm = TRUE
          ),
          generator_run_length = max(run$Run_Length, na.rm = TRUE),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  out <- do.call(rbind, rows)
  out[order(out$Train_ID, out$start_isi, method = "radix"), , drop = FALSE]
}

stpd_class1_reviewer_metrics <- function(intervals, mode) {
  if (identical(mode, "hfs_on_burst")) {
    n <- sum(intervals$error_hfs_on_burst == "overlap", na.rm = TRUE)
    base <- sum(intervals$reference_pattern == "burst", na.rm = TRUE)
    return(list(
      tp = NA_integer_, fp = n, fn = NA_integer_, precision = NA_real_,
      recall = if (base) n / base else NA_real_, f1 = NA_real_
    ))
  }
  target <- stpd_class1_reviewer_target(mode)
  truth <- if (identical(mode, "broad_hfs")) {
    intervals$hfs_truth
  } else intervals$event_truth
  prediction <- if (identical(mode, "broad_hfs")) {
    intervals$hfs_prediction
  } else intervals$event_prediction
  tp <- sum(truth == target & prediction == target)
  fp <- sum(truth != target & prediction == target)
  fn <- sum(truth == target & prediction != target)
  precision <- if (tp + fp) tp / (tp + fp) else NA_real_
  recall <- if (tp + fn) tp / (tp + fn) else NA_real_
  f1 <- if (is.finite(precision + recall) && precision + recall > 0) {
    2 * precision * recall / (precision + recall)
  } else NA_real_
  list(tp = tp, fp = fp, fn = fn, precision = precision, recall = recall, f1 = f1)
}

stpd_class1_reviewer_ui <- function() {
  shiny::fluidPage(
    shiny::tags$head(shiny::tags$style(shiny::HTML("
      body{background:#F5F7FA;color:#0F172A}.c1-card{background:#FFF;border:1px solid #DCE5EF;border-radius:10px;padding:12px 14px;margin-bottom:12px}.c1-title{font-weight:700;margin-bottom:4px}.c1-help{font-size:13px;color:#475569;line-height:1.45}.c1-metrics{font-family:ui-monospace,SFMono-Regular,Menlo,monospace;white-space:pre-wrap}.c1-warning{border-left:4px solid #0F766E}
    "))),
    shiny::titlePanel("Class 1 \u5C0F\u5C3A\u5EA6\u68C0\u6D4B\u8BEF\u5DEE\u89C2\u5BDF\u5668"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 3,
        shiny::selectInput(
          "c1_mode", "\u89C2\u5BDF\u76EE\u6807",
          choices = c(
            "Burst \u6F0F\u68C0/\u9519\u68C0" = "burst", "Pause \u6F0F\u68C0/\u9519\u68C0" = "pause",
            "Broad HFS \u6F0F\u68C0/\u9519\u68C0" = "broad_hfs",
            "HFS \u8986\u76D6\u751F\u6210\u5668 Burst" = "hfs_on_burst"
          ), selected = "burst"
        ),
        shiny::checkboxGroupInput(
          "c1_error_types", "\u8BEF\u5DEE\u7C7B\u578B",
          choices = c("\u6F0F\u68C0" = "fn", "\u9519\u68C0" = "fp", "Event\u2013State \u91CD\u53E0" = "overlap"),
          selected = c("fn", "fp", "overlap")
        ),
        shiny::selectInput("c1_train", "Spike train", choices = "\u8F7D\u5165\u4E2D\u2026"),
        shiny::uiOutput("c1_event_ui"),
        shiny::fluidRow(
          shiny::column(6, shiny::actionButton("c1_previous", "\u2190 \u4E0A\u4E00\u4E2A", width = "100%")),
          shiny::column(6, shiny::actionButton("c1_next", "\u4E0B\u4E00\u4E2A \u2192", width = "100%"))
        ),
        shiny::tags$hr(),
        shiny::uiOutput("c1_window_ui"),
        shiny::checkboxInput("c1_show_all_errors", "\u663E\u793A\u5F53\u524D train \u7684\u5168\u90E8\u540C\u7C7B\u8BEF\u5DEE", TRUE),
        shiny::tags$div(
          class = "c1-card c1-metrics", shiny::textOutput("c1_metrics")
        ),
        shiny::tags$div(
          class = "c1-help",
          "\u64CD\u4F5C\uFF1A\u62D6\u52A8\u56FE\u5F62\u5E73\u79FB\uFF1B\u6EDA\u8F6E\u7F29\u653E\uFF1B\u5E95\u90E8\u8303\u56F4\u6761\u8DF3\u8F6C\u5168\u5C40\u4F4D\u7F6E\u3002\u60AC\u505C\u53EF\u67E5\u770B ISI\u3001\u751F\u6210\u5668\u6807\u7B7E\u548C\u4E24\u6761 STPD \u8F68\u9053\u3002"
        )
      ),
      shiny::mainPanel(
        width = 9,
        shiny::tags$div(
          class = "c1-card",
          shiny::tags$div(class = "c1-title", "\u56DB\u6761\u5BF9\u9F50\u8F68\u9053"),
          shiny::tags$div(
            class = "c1-help",
            "\u540C\u4E00\u7EC4\u539F\u59CB\u65F6\u95F4\u6233\uFF1A\u539F\u59CB spike\u3001\u751F\u6210\u5668\u53C2\u8003\u3001STPD Event/Pause\u3001STPD Broad HFS State\u3002Burst Event \u4E0E HFS State \u53EF\u540C\u65F6\u5B58\u5728\u3002"
          )
        ),
        plotly::plotlyOutput("c1_plot", height = "760px"),
        shiny::uiOutput("c1_selection_note")
      )
    )
  )
}

stpd_class1_reviewer_server <- function(bundle) {
  force(bundle)
  function(input, output, session) {
    mode <- shiny::reactive(as.character(input$c1_mode %||% "burst")[[1L]])
    types <- shiny::reactive({
      selected <- as.character(input$c1_error_types %||% character())
      allowed <- if (identical(mode(), "hfs_on_burst")) "overlap" else c("fn", "fp")
      intersect(selected, allowed)
    })
    catalog <- shiny::reactive({
      selected <- types()
      if (!length(selected)) selected <- if (identical(mode(), "hfs_on_burst")) "overlap" else c("fn", "fp")
      stpd_class1_reviewer_catalog(bundle$intervals, mode(), selected)
    })
    shiny::observeEvent(list(mode(), types()), {
      z <- catalog()
      counts <- if (nrow(z)) sort(table(z$Train_ID), decreasing = TRUE) else integer()
      trains <- c(names(counts), setdiff(bundle$trains, names(counts)))
      shiny::updateSelectInput(session, "c1_train", choices = trains,
                               selected = trains[[1L]])
    }, ignoreInit = FALSE)
    train <- shiny::reactive({
      value <- as.character(input$c1_train %||% bundle$trains[[1L]])[[1L]]
      if (!(value %in% bundle$trains)) value <- bundle$trains[[1L]]
      value
    })
    train_catalog <- shiny::reactive({
      z <- catalog()
      z[z$Train_ID == train(), , drop = FALSE]
    })
    output$c1_event_ui <- shiny::renderUI({
      z <- train_catalog()
      if (!nrow(z)) return(shiny::tags$div(class = "c1-help", "\u8BE5\u7B5B\u9009\u4E0B\u6CA1\u6709\u8BEF\u5DEE\u7247\u6BB5\u3002"))
      kind <- c(fn = "\u6F0F\u68C0", fp = "\u9519\u68C0", overlap = "\u91CD\u53E0\u5BA1\u67E5")[z$error_type]
      label <- paste0(
        kind, " | ISI ", z$start_isi, "\u2013", z$end_isi, " | ",
        sprintf("%.4f\u2013%.4f s", z$start_time_sec, z$end_time_sec),
        " | median ", sprintf("%.2f ms", z$median_isi_ms)
      )
      shiny::selectInput(
        "c1_event", "\u8BEF\u5DEE\u7247\u6BB5",
        choices = stats::setNames(z$event_id, label), selected = z$event_id[[1L]]
      )
    })
    selected_event <- shiny::reactive({
      z <- train_catalog()
      if (!nrow(z)) return(NULL)
      id <- as.character(input$c1_event %||% z$event_id[[1L]])[[1L]]
      q <- z[z$event_id == id, , drop = FALSE]
      if (!nrow(q)) q <- z[1L, , drop = FALSE]
      q[1L, , drop = FALSE]
    })
    navigate <- function(delta) {
      z <- train_catalog()
      if (!nrow(z)) return()
      current <- as.character(input$c1_event %||% z$event_id[[1L]])[[1L]]
      position <- match(current, z$event_id)
      if (is.na(position)) position <- 1L
      position <- ((position - 1L + delta) %% nrow(z)) + 1L
      shiny::updateSelectInput(session, "c1_event", selected = z$event_id[[position]])
    }
    shiny::observeEvent(input$c1_previous, navigate(-1L))
    shiny::observeEvent(input$c1_next, navigate(1L))
    output$c1_window_ui <- shiny::renderUI({
      time <- bundle$spikes[[train()]]
      event <- selected_event()
      value <- if (is.null(event)) range(time) else {
        c(event$context_start_sec, event$context_end_sec)
      }
      shiny::sliderInput(
        "c1_window", "\u663E\u793A\u65F6\u95F4\u7A97\uFF08s\uFF09", min = min(time), max = max(time),
        value = value, step = max(1e-6, diff(range(time)) / 500), ticks = FALSE
      )
    })
    output$c1_metrics <- shiny::renderText({
      m <- stpd_class1_reviewer_metrics(bundle$intervals, mode())
      if (identical(mode(), "hfs_on_burst")) {
        return(sprintf(
          "\u751F\u6210\u5668 Burst \u4E2D\u88AB Broad HFS State \u8986\u76D6\uFF1A%d ISI\uFF08%.1f%%\uFF09\n\u8BE5\u9879\u662F\u8BED\u4E49\u5BA1\u67E5\uFF0C\u4E0D\u76F4\u63A5\u8BA1\u4E3A FP\u3002",
          m$fp, 100 * m$recall
        ))
      }
      sprintf(
        "%s\uFF5CTP %d \u00B7 FN %d \u00B7 FP %d\nPrecision %.3f \u00B7 Recall %.3f \u00B7 F1 %.3f",
        stpd_class1_reviewer_mode_label(mode()), m$tp, m$fn, m$fp,
        m$precision, m$recall, m$f1
      )
    })
    output$c1_selection_note <- shiny::renderUI({
      event <- selected_event()
      if (is.null(event)) return(NULL)
      interpretation <- if (identical(event$error_type, "overlap")) {
        "\u8FD9\u91CC\u7684\u751F\u6210\u5668\u53C2\u8003\u662F Burst Event\uFF0C\u800C STPD \u540C\u65F6\u7ED9\u51FA Broad HFS State\uFF1B\u4E24\u8005\u53EF\u5171\u5B58\uFF0C\u4E0D\u80FD\u81EA\u52A8\u5224\u4E3A\u7B97\u6CD5\u9519\u8BEF\u3002"
      } else if (identical(event$error_type, "fn") && identical(mode(), "burst") &&
                 event$hfs_overlap_fraction > 0) {
        paste0(
          "\u8BE5 Burst \u6F0F\u68C0\u7247\u6BB5\u6709 ",
          sprintf("%.1f%%", 100 * event$hfs_overlap_fraction),
          " \u7684 ISI \u5DF2\u4F4D\u4E8E STPD Broad HFS State \u5185\uFF1B\u95EE\u9898\u96C6\u4E2D\u5728 Burst Event \u672A\u53E0\u52A0\uFF0C\u800C\u4E0D\u662F HFS \u72B6\u6001\u6F0F\u68C0\u3002"
        )
      } else {
        "\u8BF7\u7ED3\u5408\u56DB\u6761\u8F68\u9053\u3001\u5C40\u90E8 ISI \u548C\u76F8\u90BB\u6A21\u5F0F\u5224\u65AD\u662F\u9608\u503C\u3001\u8FB9\u754C\u8FD8\u662F\u53C2\u8003\u6807\u7B7E\u95EE\u9898\u3002"
      }
      shiny::tags$div(
        class = "c1-card c1-warning",
        shiny::tags$b(paste0(
          stpd_class1_reviewer_mode_label(mode()), " \u00B7 ",
          c(fn = "\u6F0F\u68C0", fp = "\u9519\u68C0", overlap = "\u91CD\u53E0")[event$error_type]
        )),
        shiny::tags$br(),
        sprintf(
          "ISI %d\u2013%d\uFF1B%d \u4E2A ISI\uFF1Bmedian %.2f ms\uFF1Bq90 %.2f ms\uFF1BHFS overlap %.1f%%\uFF1B\u751F\u6210\u5668 run length %d\u3002",
          event$start_isi, event$end_isi, event$n_isi,
          event$median_isi_ms, event$q90_isi_ms,
          100 * event$hfs_overlap_fraction,
          event$generator_run_length
        ),
        shiny::tags$br(), interpretation
      )
    })
    output$c1_plot <- plotly::renderPlotly({
      intervals <- bundle$intervals[bundle$intervals$Train_ID == train(), , drop = FALSE]
      time <- bundle$spikes[[train()]]
      window <- suppressWarnings(as.numeric(input$c1_window))
      if (length(window) != 2L || any(!is.finite(window)) || diff(window) <= 0) {
        window <- range(time)
      }
      palette <- stpd_class1_reviewer_palette()
      ticks <- data.frame(time_sec = time)
      add_ticks <- function(p, lane) {
        d <- ticks
        d$y0 <- lane - 0.22
        d$y1 <- lane + 0.22
        plotly::add_segments(
          p, data = d, x = ~time_sec, xend = ~time_sec,
          y = ~y0, yend = ~y1, inherit = FALSE, showlegend = FALSE,
          line = list(color = "#111827", width = 1), hoverinfo = "text",
          text = ~paste0("Spike time: ", sprintf("%.6f s", time_sec))
        )
      }
      add_labels <- function(p, values, lane, lane_name, shown) {
        for (pattern in shown) {
          d <- intervals[values == pattern, , drop = FALSE]
          if (!nrow(d)) next
          d$hover <- paste0(
            lane_name, ": ", stpd_class1_reviewer_pattern_label(pattern),
            "<br>ISI index: ", d$Right_Spike_Index,
            "<br>ISI: ", sprintf("%.3f ms", 1000 * d$ISI_s),
            "<br>Generator: ", stpd_class1_reviewer_pattern_label(d$reference_pattern),
            "<br>Pause subtype: ", ifelse(
              is.na(d$Pause_Subtype) | !nzchar(d$Pause_Subtype), "\u2014", d$Pause_Subtype
            ),
            "<br>STPD Event/Pause: ", stpd_class1_reviewer_pattern_label(d$event_prediction),
            "<br>STPD State: ", stpd_class1_reviewer_pattern_label(d$hfs_prediction)
          )
          p <- plotly::add_segments(
            p, data = d, x = ~Left_Spike_Time_s, xend = ~Right_Spike_Time_s,
            y = lane, yend = lane, inherit = FALSE,
            name = paste0(lane_name, " \u00B7 ", stpd_class1_reviewer_pattern_label(pattern)),
            showlegend = TRUE,
            line = list(color = palette[[pattern]] %||% "#64748B", width = 10),
            hoverinfo = "text", text = ~hover
          )
        }
        p
      }
      p <- plotly::plot_ly(source = "class1_error_reviewer")
      for (lane in c(4, 3, 2, 1)) p <- add_ticks(p, lane)
      p <- add_labels(
        p, intervals$reference_pattern, 3, "\u751F\u6210\u5668\u53C2\u8003",
        c("burst", "pause", "tonic", "high_frequency_spiking")
      )
      p <- add_labels(
        p, intervals$event_prediction, 2, "STPD Event/Gap",
        c("burst", "pause")
      )
      p <- add_labels(
        p, intervals$hfs_prediction, 1, "STPD State",
        "high_frequency_spiking"
      )
      error_column <- stpd_class1_reviewer_error_column(mode())
      errors <- intervals[!is.na(intervals[[error_column]]) &
                            intervals[[error_column]] %in% types(), , drop = FALSE]
      selected <- selected_event()
      if (!is.null(selected) && !isTRUE(input$c1_show_all_errors)) {
        errors <- errors[
          errors$Right_Spike_Index >= selected$start_isi &
            errors$Right_Spike_Index <= selected$end_isi, , drop = FALSE
        ]
      }
      if (nrow(errors)) {
        errors$error_kind <- errors[[error_column]]
        for (kind in unique(errors$error_kind)) {
          d <- errors[errors$error_kind == kind, , drop = FALSE]
          dash <- if (kind == "fn") "dash" else if (kind == "fp") "dot" else "dashdot"
          for (lane in c(3.32, 2.32, 1.32)) {
            p <- plotly::add_segments(
              p, data = d, x = ~Left_Spike_Time_s, xend = ~Right_Spike_Time_s,
              y = lane, yend = lane, inherit = FALSE,
              name = c(fn = "\u6F0F\u68C0", fp = "\u9519\u68C0", overlap = "Event\u2013State \u91CD\u53E0")[[kind]],
              showlegend = identical(lane, 3.32),
              line = list(color = palette[[kind]], width = 3, dash = dash),
              hoverinfo = if (identical(lane, 3.32)) "text" else "skip",
              text = ~paste0(
                "\u8BEF\u5DEE: ", error_kind, "<br>ISI index: ", Right_Spike_Index,
                "<br>ISI: ", sprintf("%.3f ms", 1000 * ISI_s)
              )
            )
          }
        }
      }
      plotly::config(
        plotly::layout(
          p,
          title = list(
            text = paste0(train(), " \u00B7 ", stpd_class1_reviewer_mode_label(mode())),
            x = 0.02
          ),
          dragmode = "pan", hovermode = "closest",
          xaxis = list(
            title = "Spike time (s)", range = window,
            rangeslider = list(visible = TRUE), fixedrange = FALSE
          ),
          yaxis = list(
            title = "", range = c(0.55, 4.45),
            tickvals = c(4, 3, 2, 1),
            ticktext = c(
              "\u539F\u59CB spike train", "\u6A21\u62DF\u751F\u6210\u5668\u53C2\u8003",
              "STPD Event / Pause", "STPD Broad HFS State"
            ),
            fixedrange = TRUE, showgrid = FALSE, zeroline = FALSE
          ),
          legend = list(orientation = "h", x = 0, y = -0.22),
          margin = list(l = 175, r = 30, t = 60, b = 145),
          paper_bgcolor = "white", plot_bgcolor = "white"
        ),
        scrollZoom = TRUE, displaylogo = FALSE
      )
    })
  }
}
