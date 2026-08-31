#' Launch the clean-benchmark Burst error reviewer
#'
#' Opens a read-only Shiny application for inspecting Burst false-positive,
#' false-negative, and imperfect-boundary episodes in the frozen three-scale
#' clean mechanism benchmark.  Event and State tracks remain independent so a
#' Burst can be reviewed inside a Broad-HFS State without changing either
#' reference axis.
#'
#' @param validation_dir Directory containing the `scoring` output from the
#'   clean-benchmark validation workflow.
#' @param benchmark_dir Root directory of the clean synthetic mechanism
#'   benchmark containing `detector_inputs` and `ground_truth`.
#' @param browser Whether to open a browser automatically.
#' @param host Local server host.
#' @param port Local server port.
#' @return The value returned by [shiny::runApp()] after the app exits.
#' @export
launch_clean_benchmark_burst_reviewer <- function(
    validation_dir, benchmark_dir, browser = TRUE,
    host = "127.0.0.1", port = 7321L) {
  bundle <- stpd_clean_burst_reviewer_load_bundle(
    validation_dir = validation_dir,
    benchmark_dir = benchmark_dir
  )
  shiny::runApp(
    shiny::shinyApp(
      stpd_clean_burst_reviewer_ui(),
      stpd_clean_burst_reviewer_server(bundle)
    ),
    host = host, port = as.integer(port), launch.browser = browser
  )
}

stpd_clean_burst_required <- function(x, required, source) {
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      source, " is missing required columns: ",
      paste(missing, collapse = ", "), call. = FALSE
    )
  }
  invisible(x)
}

stpd_clean_burst_read_csv <- function(path, required = character()) {
  if (!file.exists(path)) stop("Missing reviewer input: ", path, call. = FALSE)
  out <- utils::read.csv(
    path, check.names = FALSE, stringsAsFactors = FALSE
  )
  stpd_clean_burst_required(out, required, basename(path))
  out
}

stpd_clean_burst_span_key <- function(
    scale, sample, start_isi, end_isi) {
  paste(
    as.integer(scale), as.character(sample), as.integer(start_isi),
    as.integer(end_isi), sep = "\r"
  )
}

stpd_clean_burst_iou <- function(a_start, a_end, b_start, b_end) {
  values <- c(a_start, a_end, b_start, b_end)
  if (any(!is.finite(values))) return(NA_real_)
  overlap <- max(0, min(a_end, b_end) - max(a_start, b_start) + 1)
  union <- max(a_end, b_end) - min(a_start, b_start) + 1
  if (union <= 0) NA_real_ else overlap / union
}

stpd_clean_burst_nearest <- function(
    alternatives, scale, sample, start_isi, end_isi) {
  z <- alternatives[
    as.integer(alternatives$Scale_Factor) == as.integer(scale) &
      as.character(alternatives$Sample_ID) == as.character(sample),
    , drop = FALSE
  ]
  if (!nrow(z)) return(NULL)
  score <- vapply(seq_len(nrow(z)), function(ii) {
    stpd_clean_burst_iou(
      start_isi, end_isi, z$start_isi[[ii]], z$end_isi[[ii]]
    )
  }, numeric(1))
  score[!is.finite(score)] <- -Inf
  z <- z[which.max(score), , drop = FALSE]
  z$nearest_iou <- max(score)
  z
}

stpd_clean_burst_catalog <- function(
    intervals, predicted_episodes, truth_episodes, primary_matches) {
  pred <- predicted_episodes[
    tolower(as.character(predicted_episodes$target)) == "burst",
    , drop = FALSE
  ]
  truth <- truth_episodes[
    tolower(as.character(truth_episodes$target)) == "burst",
    , drop = FALSE
  ]
  matches <- primary_matches[
    tolower(as.character(primary_matches$target)) == "burst",
    , drop = FALSE
  ]
  for (z_name in c("pred", "truth")) {
    z <- get(z_name)
    z$Scale_Factor <- as.integer(z$Scale_Factor)
    z$Sample_ID <- as.character(z$Sample_ID)
    z$Template_ID <- as.character(z$Template_ID)
    z$start_isi <- as.integer(z$start_isi)
    z$end_isi <- as.integer(z$end_isi)
    assign(z_name, z)
  }
  pred$key <- stpd_clean_burst_span_key(
    pred$Scale_Factor, pred$Sample_ID, pred$start_isi, pred$end_isi
  )
  truth$key <- stpd_clean_burst_span_key(
    truth$Scale_Factor, truth$Sample_ID, truth$start_isi, truth$end_isi
  )
  if (anyDuplicated(pred$key) || anyDuplicated(truth$key)) {
    stop("Burst episode geometry is not unique within scale and sample.",
         call. = FALSE)
  }

  if (nrow(matches)) {
    matches$Scale_Factor <- as.integer(matches$Scale_Factor)
    matches$Sample_ID <- as.character(matches$train)
    matches$Template_ID <- as.character(matches$Template_ID)
    matches$pred_key <- stpd_clean_burst_span_key(
      matches$Scale_Factor, matches$Sample_ID,
      matches$predicted_start_isi, matches$predicted_end_isi
    )
    matches$truth_key <- stpd_clean_burst_span_key(
      matches$Scale_Factor, matches$Sample_ID,
      matches$truth_start_isi, matches$truth_end_isi
    )
  } else {
    matches$pred_key <- character()
    matches$truth_key <- character()
  }

  make_row <- function(
      error_type, scale, sample, template,
      truth_start = NA_integer_, truth_end = NA_integer_,
      predicted_start = NA_integer_, predicted_end = NA_integer_,
      iou = NA_real_, truth_subtype = NA_character_) {
    geometry <- c(truth_start, truth_end, predicted_start, predicted_end)
    finite <- geometry[is.finite(geometry)]
    data.frame(
      Scale_Factor = as.integer(scale), Sample_ID = as.character(sample),
      Template_ID = as.character(template), error_type = error_type,
      truth_start_isi = as.integer(truth_start),
      truth_end_isi = as.integer(truth_end),
      predicted_start_isi = as.integer(predicted_start),
      predicted_end_isi = as.integer(predicted_end),
      anchor_start_isi = if (length(finite)) as.integer(min(finite)) else NA_integer_,
      anchor_end_isi = if (length(finite)) as.integer(max(finite)) else NA_integer_,
      iou = as.numeric(iou), truth_subtype = as.character(truth_subtype),
      stringsAsFactors = FALSE
    )
  }

  rows <- list()
  unmatched_pred <- pred[!(pred$key %in% matches$pred_key), , drop = FALSE]
  if (nrow(unmatched_pred)) for (ii in seq_len(nrow(unmatched_pred))) {
    p <- unmatched_pred[ii, , drop = FALSE]
    nearest <- stpd_clean_burst_nearest(
      truth, p$Scale_Factor, p$Sample_ID, p$start_isi, p$end_isi
    )
    has_overlap <- !is.null(nearest) && is.finite(nearest$nearest_iou) &&
      nearest$nearest_iou > 0
    rows[[length(rows) + 1L]] <- make_row(
      "fp", p$Scale_Factor, p$Sample_ID, p$Template_ID,
      if (has_overlap) nearest$start_isi else NA_integer_,
      if (has_overlap) nearest$end_isi else NA_integer_,
      p$start_isi, p$end_isi,
      if (has_overlap) nearest$nearest_iou else 0,
      if (has_overlap && "subtype" %in% names(nearest)) nearest$subtype else NA_character_
    )
  }
  unmatched_truth <- truth[!(truth$key %in% matches$truth_key), , drop = FALSE]
  if (nrow(unmatched_truth)) for (ii in seq_len(nrow(unmatched_truth))) {
    q <- unmatched_truth[ii, , drop = FALSE]
    nearest <- stpd_clean_burst_nearest(
      pred, q$Scale_Factor, q$Sample_ID, q$start_isi, q$end_isi
    )
    has_overlap <- !is.null(nearest) && is.finite(nearest$nearest_iou) &&
      nearest$nearest_iou > 0
    rows[[length(rows) + 1L]] <- make_row(
      "fn", q$Scale_Factor, q$Sample_ID, q$Template_ID,
      q$start_isi, q$end_isi,
      if (has_overlap) nearest$start_isi else NA_integer_,
      if (has_overlap) nearest$end_isi else NA_integer_,
      if (has_overlap) nearest$nearest_iou else 0,
      if ("subtype" %in% names(q)) q$subtype else NA_character_
    )
  }
  boundary <- matches[
    !is.finite(matches$iou) | matches$iou < 1 - 1e-12 |
      matches$start_boundary_error_isi != 0 |
      matches$end_boundary_error_isi != 0,
    , drop = FALSE
  ]
  if (nrow(boundary)) for (ii in seq_len(nrow(boundary))) {
    q <- boundary[ii, , drop = FALSE]
    truth_row <- truth[truth$key == q$truth_key, , drop = FALSE]
    rows[[length(rows) + 1L]] <- make_row(
      "boundary", q$Scale_Factor, q$Sample_ID, q$Template_ID,
      q$truth_start_isi, q$truth_end_isi,
      q$predicted_start_isi, q$predicted_end_isi, q$iou,
      if (nrow(truth_row) && "subtype" %in% names(truth_row)) {
        truth_row$subtype[[1L]]
      } else NA_character_
    )
  }
  if (!length(rows)) return(data.frame())
  out <- do.call(rbind, rows)
  out$case_id <- paste0(
    "burst__", out$error_type, "__s", out$Scale_Factor, "__",
    out$Sample_ID, "__", out$anchor_start_isi, "_", out$anchor_end_isi
  )
  if (anyDuplicated(out$case_id)) {
    out$case_id <- make.unique(out$case_id, sep = "__")
  }

  enrich <- lapply(seq_len(nrow(out)), function(ii) {
    q <- out[ii, , drop = FALSE]
    z <- intervals[
      intervals$Scale_Factor == q$Scale_Factor &
        intervals$Sample_ID == q$Sample_ID,
      , drop = FALSE
    ]
    focus <- z[
      z$Right_Spike_Index >= q$anchor_start_isi &
        z$Right_Spike_Index <= q$anchor_end_isi,
      , drop = FALSE
    ]
    sample_range <- range(c(z$Start_s, z$End_s), finite = TRUE)
    focus_range <- range(c(focus$Start_s, focus$End_s), finite = TRUE)
    duration <- diff(focus_range)
    b_s <- unique(as.numeric(z$B_s))
    b_s <- b_s[is.finite(b_s) & b_s > 0]
    b_s <- if (length(b_s)) b_s[[1L]] else 0.1 * q$Scale_Factor
    padding <- max(2.5 * b_s, 6 * duration, na.rm = TRUE)
    padding <- min(padding, 20 * b_s)
    data.frame(
      start_time_sec = focus_range[[1L]], end_time_sec = focus_range[[2L]],
      context_start_sec = max(sample_range[[1L]], focus_range[[1L]] - padding),
      context_end_sec = min(sample_range[[2L]], focus_range[[2L]] + padding),
      n_isi = nrow(focus),
      median_isi_ms = 1000 * stats::median(focus$ISI_s, na.rm = TRUE),
      max_isi_ms = 1000 * max(focus$ISI_s, na.rm = TRUE),
      truth_hfs_fraction = mean(
        focus$hfs_truth == "high_frequency_spiking", na.rm = TRUE
      ),
      predicted_hfs_fraction = mean(
        focus$hfs_prediction == "high_frequency_spiking", na.rm = TRUE
      ),
      stringsAsFactors = FALSE
    )
  })
  out <- cbind(out, do.call(rbind, enrich))
  order_type <- match(out$error_type, c("fp", "fn", "boundary"))
  out <- out[order(
    out$Scale_Factor, order_type, out$Template_ID,
    out$Sample_ID, out$anchor_start_isi, method = "radix"
  ), , drop = FALSE]
  rownames(out) <- NULL
  out
}

stpd_clean_burst_reviewer_load_bundle <- function(
    validation_dir, benchmark_dir) {
  validation_dir <- normalizePath(validation_dir, mustWork = TRUE)
  benchmark_dir <- normalizePath(benchmark_dir, mustWork = TRUE)
  scoring <- file.path(validation_dir, "scoring")
  interval_long <- stpd_clean_burst_read_csv(
    file.path(scoring, "heldout_interval_truth_and_predictions.csv"),
    c(
      "Sample_ID", "Right_Spike_Index", "Axis", "Template_ID",
      "Scale_Factor", "B_s", "Start_s", "End_s", "ISI_s",
      "Truth", "Prediction"
    )
  )
  predicted <- stpd_clean_burst_read_csv(
    file.path(scoring, "heldout_predicted_episodes.csv"),
    c(
      "Sample_ID", "Template_ID", "Scale_Factor", "target",
      "start_isi", "end_isi"
    )
  )
  truth <- stpd_clean_burst_read_csv(
    file.path(scoring, "heldout_truth_episodes.csv"),
    c(
      "Sample_ID", "Template_ID", "Scale_Factor", "target",
      "start_isi", "end_isi", "subtype"
    )
  )
  matches <- stpd_clean_burst_read_csv(
    file.path(scoring, "primary_episode_matches_iou_050.csv"),
    c(
      "train", "pattern", "iou", "truth_start_isi", "truth_end_isi",
      "predicted_start_isi", "predicted_end_isi", "Scale_Factor",
      "Template_ID", "target", "start_boundary_error_isi",
      "end_boundary_error_isi"
    )
  )
  metrics <- stpd_clean_burst_read_csv(
    file.path(scoring, "primary_observed_metrics.csv"),
    c(
      "Scale_Factor", "level", "target", "tp", "fp", "fn",
      "precision", "recall", "F1"
    )
  )
  input <- stpd_clean_burst_read_csv(
    file.path(
      benchmark_dir, "detector_inputs", "spike_timestamps_blinded.csv"
    ),
    c("Sample_ID", "Spike_Index", "Time_s")
  )

  key <- c("Sample_ID", "Template_ID", "Scale_Factor", "Right_Spike_Index")
  base <- interval_long[
    interval_long$Axis == "event",
    c(key, "B_s", "Start_s", "End_s", "ISI_s"), drop = FALSE
  ]
  axis_values <- function(axis, prefix) {
    z <- interval_long[
      interval_long$Axis == axis,
      c(key, "Truth", "Prediction"), drop = FALSE
    ]
    names(z)[names(z) == "Truth"] <- paste0(prefix, "_truth")
    names(z)[names(z) == "Prediction"] <- paste0(prefix, "_prediction")
    z
  }
  intervals <- Reduce(
    function(x, y) merge(x, y, by = key, all = FALSE, sort = FALSE),
    list(
      base, axis_values("event", "event"),
      axis_values("state_hf_family", "hfs"),
      axis_values("state_strict", "tonic")
    )
  )
  label_columns <- c(
    "event_truth", "event_prediction", "hfs_truth", "hfs_prediction",
    "tonic_truth", "tonic_prediction"
  )
  for (column in label_columns) {
    intervals[[column]] <- stpd_class1_reviewer_normalize(intervals[[column]])
  }
  intervals$state_truth <- ifelse(
    intervals$hfs_truth == "high_frequency_spiking",
    "high_frequency_spiking",
    ifelse(intervals$tonic_truth == "tonic", "tonic", "other")
  )
  intervals$state_prediction <- ifelse(
    intervals$hfs_prediction == "high_frequency_spiking",
    "high_frequency_spiking",
    ifelse(intervals$tonic_prediction == "tonic", "tonic", "other")
  )
  intervals <- intervals[order(
    intervals$Scale_Factor, intervals$Template_ID, intervals$Sample_ID,
    intervals$Right_Spike_Index, method = "radix"
  ), , drop = FALSE]
  if (anyDuplicated(intervals[c(
    "Scale_Factor", "Sample_ID", "Right_Spike_Index"
  )])) {
    stop("Reviewer interval table is not one-to-one by scale/sample/ISI.",
         call. = FALSE)
  }

  sample_ids <- unique(intervals$Sample_ID)
  input <- input[input$Sample_ID %in% sample_ids, , drop = FALSE]
  spikes <- lapply(split(input, input$Sample_ID), function(z) {
    z <- z[order(as.integer(z$Spike_Index), method = "radix"), , drop = FALSE]
    time <- as.numeric(z$Time_s)
    if (any(!is.finite(time)) || any(diff(time) <= 0)) {
      stop("Invalid blinded timestamps for sample ", z$Sample_ID[[1L]],
           call. = FALSE)
    }
    time
  })
  if (!all(sample_ids %in% names(spikes))) {
    stop("One or more held-out samples have no blinded timestamps.",
         call. = FALSE)
  }
  catalog <- stpd_clean_burst_catalog(
    intervals, predicted, truth, matches
  )
  if (!nrow(catalog)) {
    stop("No Burst review cases were found in the supplied validation.",
         call. = FALSE)
  }
  sample_meta <- unique(intervals[c(
    "Sample_ID", "Template_ID", "Scale_Factor"
  )])
  sample_meta$label <- paste0(
    sample_meta$Scale_Factor, "\u00D7 \u00B7 ", sample_meta$Template_ID,
    " \u00B7 ", sample_meta$Sample_ID
  )
  list(
    intervals = intervals, spikes = spikes, catalog = catalog,
    metrics = metrics, sample_meta = sample_meta,
    validation_dir = validation_dir, benchmark_dir = benchmark_dir
  )
}

stpd_clean_burst_error_labels <- function() c(
  fp = "\u9519\u68C0 Burst episode\uFF08FP\uFF09",
  fn = "\u6F0F\u68C0 Burst episode\uFF08FN\uFF09",
  boundary = "\u5DF2\u5339\u914D\u4F46\u8FB9\u754C\u4E0D\u5B8C\u6574"
)

stpd_clean_burst_reviewer_ui <- function() {
  shiny::fluidPage(
    shiny::tags$head(shiny::tags$style(shiny::HTML("
      body{background:#F4F7FB;color:#0F172A}.burst-card{background:#FFF;border:1px solid #D8E2EE;border-radius:11px;padding:12px 14px;margin-bottom:12px}.burst-title{font-weight:750;margin-bottom:5px}.burst-help{font-size:13px;color:#475569;line-height:1.5}.burst-mono{font-family:ui-monospace,SFMono-Regular,Menlo,monospace;white-space:pre-wrap}.burst-case{border-left:5px solid #9A6700}.control-label{font-weight:650}
    "))),
    shiny::titlePanel("\u65B0\u7248\u4E09\u5C3A\u5EA6 Burst \u8BEF\u5DEE\u89C2\u5BDF\u5668"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 3,
        shiny::radioButtons(
          "burst_scale", "\u65F6\u95F4\u5C3A\u5EA6",
          choices = c("\u5168\u90E8" = "all", "1\u00D7" = "1", "4\u00D7" = "4", "10\u00D7" = "10"),
          selected = "all", inline = TRUE
        ),
        shiny::checkboxGroupInput(
          "burst_error_types", "\u95EE\u9898\u7C7B\u578B",
          choices = stats::setNames(
            names(stpd_clean_burst_error_labels()),
            unname(stpd_clean_burst_error_labels())
          ),
          selected = c("fp", "fn", "boundary")
        ),
        shiny::selectInput(
          "burst_template", "Template",
          choices = c("\u5168\u90E8" = "all"), selected = "all"
        ),
        shiny::selectInput(
          "burst_sample", "Spike train",
          choices = c("\u5168\u90E8" = "all"), selected = "all"
        ),
        shiny::uiOutput("burst_case_ui"),
        shiny::fluidRow(
          shiny::column(
            6, shiny::actionButton(
              "burst_previous", "\u2190 \u4E0A\u4E00\u4E2A", width = "100%"
            )
          ),
          shiny::column(
            6, shiny::actionButton(
              "burst_next", "\u4E0B\u4E00\u4E2A \u2192", width = "100%"
            )
          )
        ),
        shiny::tags$hr(),
        shiny::uiOutput("burst_window_ui"),
        shiny::checkboxInput(
          "burst_show_context", "\u663E\u793A Pause/Tonic/Broad HFS \u4E0A\u4E0B\u6587", TRUE
        ),
        shiny::checkboxInput(
          "burst_show_all_cases", "\u6807\u51FA\u5F53\u524D train \u5176\u4ED6 Burst \u95EE\u9898", TRUE
        ),
        shiny::tags$div(
          class = "burst-card burst-mono", shiny::textOutput("burst_metrics")
        ),
        shiny::tags$div(
          class = "burst-help",
          "\u9F20\u6807\u62D6\u52A8\u53EF\u5DE6\u53F3\u5E73\u79FB\uFF0C\u6EDA\u8F6E\u7F29\u653E\uFF0C\u5E95\u90E8\u8303\u56F4\u6761\u8DF3\u8F6C\u5168\u5C40\u4F4D\u7F6E\u3002\u4E0B\u65B9 ISI \u56FE\u7528\u5BF9\u6570 Y \u8F74\u663E\u793A\u5C40\u90E8\u7ED3\u6784\u3002"
        )
      ),
      shiny::mainPanel(
        width = 9,
        shiny::uiOutput("burst_case_note"),
        shiny::tags$div(
          class = "burst-card",
          shiny::tags$div(class = "burst-title", "\u4E94\u6761\u5BF9\u9F50 spike-train \u8F68\u9053"),
          shiny::tags$div(
            class = "burst-help",
            "\u4E8B\u4EF6\u8F68\uFF08Burst/Pause\uFF09\u4E0E\u72B6\u6001\u8F68\uFF08Tonic/Broad HFS\uFF09\u72EC\u7ACB\uFF1BBurst \u4E0E HFS \u53EF\u540C\u65F6\u5B58\u5728\u3002"
          )
        ),
        plotly::plotlyOutput("burst_raster_plot", height = "690px"),
        plotly::plotlyOutput("burst_isi_plot", height = "300px")
      )
    )
  )
}

stpd_clean_burst_case_label <- function(z) {
  kind <- unname(stpd_clean_burst_error_labels()[z$error_type])
  predicted <- if (is.finite(z$predicted_start_isi)) {
    paste0("pred ", z$predicted_start_isi, "\u2013", z$predicted_end_isi)
  } else "pred \u2014"
  truth <- if (is.finite(z$truth_start_isi)) {
    paste0("truth ", z$truth_start_isi, "\u2013", z$truth_end_isi)
  } else "truth \u2014"
  paste0(
    kind, " | ", z$Scale_Factor, "\u00D7 | ", z$Template_ID, " | ",
    z$Sample_ID, " | ", predicted, " | ", truth,
    " | IoU ", sprintf("%.2f", z$iou)
  )
}

stpd_clean_burst_reviewer_server <- function(bundle) {
  force(bundle)
  function(input, output, session) {
    labels <- stpd_clean_burst_error_labels()
    templates <- sort(unique(bundle$catalog$Template_ID), method = "radix")
    samples <- bundle$sample_meta[order(
      bundle$sample_meta$Scale_Factor, bundle$sample_meta$Template_ID,
      method = "radix"
    ), , drop = FALSE]
    shiny::updateSelectInput(
      session, "burst_template",
      choices = c("\u5168\u90E8" = "all", stats::setNames(templates, templates))
    )
    shiny::updateSelectInput(
      session, "burst_sample",
      choices = c(
        "\u5168\u90E8" = "all",
        stats::setNames(samples$Sample_ID, samples$label)
      )
    )

    filtered_catalog <- shiny::reactive({
      z <- bundle$catalog
      scale <- as.character(input$burst_scale %||% "all")[[1L]]
      types <- intersect(
        as.character(input$burst_error_types %||% character()), names(labels)
      )
      template <- as.character(input$burst_template %||% "all")[[1L]]
      sample <- as.character(input$burst_sample %||% "all")[[1L]]
      if (!identical(scale, "all")) {
        z <- z[z$Scale_Factor == as.integer(scale), , drop = FALSE]
      }
      if (length(types)) z <- z[z$error_type %in% types, , drop = FALSE]
      else z <- z[FALSE, , drop = FALSE]
      if (!identical(template, "all")) {
        z <- z[z$Template_ID == template, , drop = FALSE]
      }
      if (!identical(sample, "all")) {
        z <- z[z$Sample_ID == sample, , drop = FALSE]
      }
      z
    })

    output$burst_case_ui <- shiny::renderUI({
      z <- filtered_catalog()
      if (!nrow(z)) {
        return(shiny::tags$div(
          class = "burst-card burst-help",
          "\u5F53\u524D\u7B5B\u9009\u6CA1\u6709 Burst \u95EE\u9898 episode\u3002"
        ))
      }
      choice <- stats::setNames(
        z$case_id, vapply(seq_len(nrow(z)), function(ii) {
          stpd_clean_burst_case_label(z[ii, , drop = FALSE])
        }, character(1))
      )
      shiny::selectizeInput(
        "burst_case", "Burst \u95EE\u9898 episode", choices = choice,
        selected = z$case_id[[1L]], options = list(maxOptions = 2000)
      )
    })

    selected_case <- shiny::reactive({
      z <- filtered_catalog()
      if (!nrow(z)) return(NULL)
      id <- as.character(input$burst_case %||% z$case_id[[1L]])[[1L]]
      q <- z[z$case_id == id, , drop = FALSE]
      if (!nrow(q)) q <- z[1L, , drop = FALSE]
      q[1L, , drop = FALSE]
    })

    navigate <- function(delta) {
      z <- filtered_catalog()
      if (!nrow(z)) return()
      current <- as.character(input$burst_case %||% z$case_id[[1L]])[[1L]]
      position <- match(current, z$case_id)
      if (is.na(position)) position <- 1L
      position <- ((position - 1L + delta) %% nrow(z)) + 1L
      shiny::updateSelectizeInput(
        session, "burst_case", selected = z$case_id[[position]]
      )
    }
    shiny::observeEvent(input$burst_previous, navigate(-1L))
    shiny::observeEvent(input$burst_next, navigate(1L))

    output$burst_window_ui <- shiny::renderUI({
      case <- selected_case()
      if (is.null(case)) return(NULL)
      time <- bundle$spikes[[case$Sample_ID]]
      shiny::sliderInput(
        "burst_window", "\u663E\u793A\u65F6\u95F4\u7A97\uFF08s\uFF09",
        min = min(time), max = max(time),
        value = c(case$context_start_sec, case$context_end_sec),
        step = max(1e-6, diff(range(time)) / 1000), ticks = FALSE
      )
    })

    output$burst_metrics <- shiny::renderText({
      scale <- as.character(input$burst_scale %||% "all")[[1L]]
      z <- bundle$metrics[
        tolower(bundle$metrics$target) == "burst", , drop = FALSE
      ]
      if (!identical(scale, "all")) {
        z <- z[z$Scale_Factor == as.integer(scale), , drop = FALSE]
      }
      lines <- vapply(seq_len(nrow(z)), function(ii) sprintf(
        "%s\u00D7 %-16s TP %d FP %d FN %d | P %.3f R %.3f F1 %.3f",
        z$Scale_Factor[[ii]], z$level[[ii]], z$tp[[ii]], z$fp[[ii]],
        z$fn[[ii]], z$precision[[ii]], z$recall[[ii]], z$F1[[ii]]
      ), character(1))
      paste(c(
        paste0("\u5F53\u524D\u7B5B\u9009\u95EE\u9898 episode: ", nrow(filtered_catalog())),
        lines
      ), collapse = "\n")
    })

    output$burst_case_note <- shiny::renderUI({
      case <- selected_case()
      if (is.null(case)) return(NULL)
      explanation <- switch(
        case$error_type,
        fp = if (case$iou > 0) {
          "STPD Burst \u4E0E\u53C2\u8003 Burst \u6709\u91CD\u53E0\uFF0C\u4F46 IoU<0.50\uFF0C\u5728\u4E3B episode \u6307\u6807\u4E2D\u8BA1\u4E3A FP\u3002"
        } else "STPD \u7ED9\u51FA Burst\uFF0C\u4F46\u6CA1\u6709\u91CD\u53E0\u7684\u53C2\u8003 Burst\u3002",
        fn = if (case$iou > 0) {
          "\u53C2\u8003 Burst \u4EC5\u88AB\u4E00\u4E2A IoU<0.50 \u7684\u5019\u9009\u90E8\u5206\u8986\u76D6\uFF0C\u56E0\u6B64\u5728\u4E3B\u6307\u6807\u4E2D\u4ECD\u8BA1\u4E3A FN\u3002"
        } else "\u53C2\u8003 Burst \u6CA1\u6709\u4EFB\u4F55\u91CD\u53E0\u7684 STPD Burst \u5019\u9009\u3002",
        boundary = "\u8BE5 Burst \u5DF2\u5728 IoU\u22650.50 \u4E0B\u5339\u914D\uFF0C\u4F46\u8D77\u70B9\u6216\u7EC8\u70B9\u8FB9\u754C\u4E0D\u5B8C\u5168\u4E00\u81F4\u3002"
      )
      color <- c(fp = "#9A6700", fn = "#B42318", boundary = "#0F766E")[[
        case$error_type
      ]]
      shiny::tags$div(
        class = "burst-card burst-case",
        style = paste0("border-left-color:", color),
        shiny::tags$div(
          class = "burst-title",
          paste0(
            unname(labels[case$error_type]), " \u00B7 ", case$Scale_Factor,
            "\u00D7 \u00B7 ", case$Template_ID, " \u00B7 ", case$Sample_ID
          )
        ),
        sprintf(
          "Truth ISI %s\uFF1BSTPD ISI %s\uFF1BIoU %.3f\uFF1Bmedian %.2f ms\uFF1Bmax %.2f ms\u3002",
          if (is.finite(case$truth_start_isi)) paste0(
            case$truth_start_isi, "\u2013", case$truth_end_isi
          ) else "\u2014",
          if (is.finite(case$predicted_start_isi)) paste0(
            case$predicted_start_isi, "\u2013", case$predicted_end_isi
          ) else "\u2014",
          case$iou, case$median_isi_ms, case$max_isi_ms
        ),
        shiny::tags$br(),
        sprintf(
          "Truth Broad-HFS overlap %.1f%%\uFF1BSTPD Broad-HFS overlap %.1f%%\uFF1Btruth subtype: %s\u3002",
          100 * case$truth_hfs_fraction,
          100 * case$predicted_hfs_fraction,
          ifelse(is.na(case$truth_subtype) | !nzchar(case$truth_subtype),
                 "\u2014", case$truth_subtype)
        ),
        shiny::tags$br(), explanation
      )
    })

    plot_context <- shiny::reactive({
      case <- selected_case()
      if (is.null(case)) return(NULL)
      intervals <- bundle$intervals[
        bundle$intervals$Scale_Factor == case$Scale_Factor &
          bundle$intervals$Sample_ID == case$Sample_ID,
        , drop = FALSE
      ]
      window <- suppressWarnings(as.numeric(input$burst_window))
      if (length(window) != 2L || any(!is.finite(window)) || diff(window) <= 0) {
        window <- c(case$context_start_sec, case$context_end_sec)
      }
      list(case = case, intervals = intervals,
           spikes = bundle$spikes[[case$Sample_ID]], window = window)
    })

    output$burst_raster_plot <- plotly::renderPlotly({
      context <- plot_context()
      if (is.null(context)) return(NULL)
      case <- context$case
      intervals <- context$intervals
      palette <- stpd_class1_reviewer_palette()
      ticks <- data.frame(time_sec = context$spikes)
      p <- plotly::plot_ly(source = "clean_burst_error_reviewer")
      for (lane in 5:1) {
        d <- ticks
        d$y0 <- lane - 0.22
        d$y1 <- lane + 0.22
        p <- plotly::add_segments(
          p, data = d, x = ~time_sec, xend = ~time_sec,
          y = ~y0, yend = ~y1, inherit = FALSE, showlegend = FALSE,
          line = list(color = "#111827", width = 1), hoverinfo = "text",
          text = ~paste0("Spike time: ", sprintf("%.6f s", time_sec))
        )
      }
      add_label <- function(p, values, lane, lane_name, shown) {
        for (pattern in shown) {
          d <- intervals[values == pattern, , drop = FALSE]
          if (!nrow(d)) next
          d$hover <- paste0(
            lane_name, ": ", stpd_class1_reviewer_pattern_label(pattern),
            "<br>ISI index: ", d$Right_Spike_Index,
            "<br>ISI: ", sprintf("%.3f ms", 1000 * d$ISI_s),
            "<br>Truth event: ", stpd_class1_reviewer_pattern_label(d$event_truth),
            "<br>STPD event: ", stpd_class1_reviewer_pattern_label(d$event_prediction),
            "<br>Truth state: ", stpd_class1_reviewer_pattern_label(d$state_truth),
            "<br>STPD state: ", stpd_class1_reviewer_pattern_label(d$state_prediction)
          )
          p <- plotly::add_segments(
            p, data = d, x = ~Start_s, xend = ~End_s,
            y = lane, yend = lane, inherit = FALSE,
            name = paste0(
              lane_name, " \u00B7 ", stpd_class1_reviewer_pattern_label(pattern)
            ),
            line = list(
              color = palette[[pattern]] %||% "#64748B", width = 10
            ),
            hoverinfo = "text", text = ~hover, showlegend = TRUE
          )
        }
        p
      }
      event_shown <- if (isTRUE(input$burst_show_context)) {
        c("burst", "pause")
      } else "burst"
      state_shown <- if (isTRUE(input$burst_show_context)) {
        c("tonic", "high_frequency_spiking")
      } else character()
      p <- add_label(p, intervals$event_truth, 4, "Truth Event", event_shown)
      p <- add_label(p, intervals$state_truth, 3, "Truth State", state_shown)
      p <- add_label(
        p, intervals$event_prediction, 2, "STPD Event", event_shown
      )
      p <- add_label(
        p, intervals$state_prediction, 1, "STPD State", state_shown
      )

      if (isTRUE(input$burst_show_all_cases)) {
        cases <- bundle$catalog[
          bundle$catalog$Scale_Factor == case$Scale_Factor &
            bundle$catalog$Sample_ID == case$Sample_ID,
          , drop = FALSE
        ]
        if (nrow(cases)) for (kind in unique(cases$error_type)) {
          d <- cases[cases$error_type == kind, , drop = FALSE]
          p <- plotly::add_segments(
            p, data = d, x = ~start_time_sec, xend = ~end_time_sec,
            y = 5.34, yend = 5.34, inherit = FALSE,
            name = paste0("\u95EE\u9898\u4F4D\u7F6E \u00B7 ", unname(labels[kind])),
            line = list(
              color = c(
                fp = "#9A6700", fn = "#B42318", boundary = "#0F766E"
              )[[kind]], width = 4
            ),
            hoverinfo = "text",
            text = ~paste0("\u95EE\u9898: ", error_type, "<br>ISI ",
                           anchor_start_isi, "\u2013", anchor_end_isi),
            showlegend = TRUE
          )
        }
      }
      error_color <- c(
        fp = "rgba(154,103,0,0.14)", fn = "rgba(180,35,24,0.14)",
        boundary = "rgba(15,118,110,0.14)"
      )[[case$error_type]]
      p <- plotly::layout(
        p,
        title = list(
          text = paste0(
            case$Scale_Factor, "\u00D7 \u00B7 ", case$Template_ID, " \u00B7 ",
            case$Sample_ID, " \u00B7 ", unname(labels[case$error_type])
          ), x = 0.02
        ),
        dragmode = "pan", hovermode = "closest",
        xaxis = list(
          title = "Spike time (s)", range = context$window,
          rangeslider = list(visible = TRUE), fixedrange = FALSE
        ),
        yaxis = list(
          title = "", range = c(0.55, 5.5),
          tickvals = 5:1,
          ticktext = c(
            "\u539F\u59CB spike train", "\u53C2\u8003 Event", "\u53C2\u8003 State",
            "STPD Event", "STPD State"
          ),
          fixedrange = TRUE, showgrid = FALSE, zeroline = FALSE
        ),
        shapes = list(list(
          type = "rect", xref = "x", yref = "y",
          x0 = case$start_time_sec, x1 = case$end_time_sec,
          y0 = 0.57, y1 = 5.43, fillcolor = error_color,
          line = list(width = 0), layer = "below"
        )),
        legend = list(orientation = "h", x = 0, y = -0.24),
        margin = list(l = 145, r = 25, t = 60, b = 150),
        paper_bgcolor = "white", plot_bgcolor = "white"
      )
      plotly::config(
        p, scrollZoom = TRUE, displaylogo = FALSE,
        modeBarButtonsToAdd = c("drawline", "eraseshape")
      )
    })

    output$burst_isi_plot <- plotly::renderPlotly({
      context <- plot_context()
      if (is.null(context)) return(NULL)
      case <- context$case
      z <- context$intervals
      z$ISI_ms <- 1000 * z$ISI_s
      p <- plotly::plot_ly(
        data = z, x = ~End_s, y = ~ISI_ms, type = "scatter",
        mode = "lines+markers", name = "All ISIs",
        line = list(color = "#CBD5E1", width = 1),
        marker = list(color = "#64748B", size = 5),
        text = ~paste0(
          "ISI index: ", Right_Spike_Index,
          "<br>ISI: ", sprintf("%.3f ms", ISI_ms),
          "<br>Truth event: ", stpd_class1_reviewer_pattern_label(event_truth),
          "<br>STPD event: ", stpd_class1_reviewer_pattern_label(event_prediction),
          "<br>Truth state: ", stpd_class1_reviewer_pattern_label(state_truth),
          "<br>STPD state: ", stpd_class1_reviewer_pattern_label(state_prediction)
        ), hoverinfo = "text"
      )
      truth_burst <- z[z$event_truth == "burst", , drop = FALSE]
      pred_burst <- z[z$event_prediction == "burst", , drop = FALSE]
      selected <- z[
        z$Right_Spike_Index >= case$anchor_start_isi &
          z$Right_Spike_Index <= case$anchor_end_isi,
        , drop = FALSE
      ]
      if (nrow(truth_burst)) p <- plotly::add_markers(
        p, data = truth_burst, x = ~End_s, y = ~ISI_ms,
        inherit = FALSE, name = "Truth Burst ISI",
        marker = list(color = "#2563EB", size = 8, symbol = "circle"),
        hoverinfo = "skip"
      )
      if (nrow(pred_burst)) p <- plotly::add_markers(
        p, data = pred_burst, x = ~End_s, y = ~ISI_ms,
        inherit = FALSE, name = "STPD Burst ISI",
        marker = list(
          color = "#F59E0B", size = 10, symbol = "circle-open",
          line = list(width = 2, color = "#F59E0B")
        ), hoverinfo = "skip"
      )
      if (nrow(selected)) p <- plotly::add_markers(
        p, data = selected, x = ~End_s, y = ~ISI_ms,
        inherit = FALSE, name = "\u5F53\u524D\u95EE\u9898\u533A\u57DF",
        marker = list(color = "#B42318", size = 12, symbol = "diamond-open"),
        hoverinfo = "skip"
      )
      p <- plotly::layout(
        p, dragmode = "pan", hovermode = "closest",
        xaxis = list(
          title = "Spike time (s)", range = context$window, fixedrange = FALSE
        ),
        yaxis = list(title = "ISI (ms, log scale)", type = "log"),
        legend = list(orientation = "h", x = 0, y = 1.18),
        margin = list(l = 75, r = 25, t = 55, b = 55),
        paper_bgcolor = "white", plot_bgcolor = "white"
      )
      plotly::config(p, scrollZoom = TRUE, displaylogo = FALSE)
    })
  }
}
