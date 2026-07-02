# Auto-generated modular extraction from modular reference script.
# Do not edit generated function names blindly; use git for version history.

server <- function(input, output, session) {
  rv <- reactiveValues(
    datasets = list(),
    current_id = NULL,
    data_load_active = FALSE,
    data_load_progress_value = 0,
    data_load_progress_message = "",
    data_load_progress_detail = "",
    data_load_progress_type = "idle",
    raster_plot_progress_active = FALSE,
    raster_plot_refresh_token = 0L,
    view_align_x = NULL,
    raw_view_x = NULL,
    preview_candidate = NULL,
    last_plotly_selection = NULL,
    near_miss_idx = 1L,
    nn_model = NULL,
    nn_training_info = NULL,
    nn_eval = NULL,
    manual_detector_eval = NULL,
    scientific_validation = NULL,
    last_detector_summary = "\u5C1A\u65E0\u68C0\u6D4B\u5668\u91CD\u8DD1\u6458\u8981\u3002",
    near_miss_rerun_summary = "\u5C1A\u65E0\u9608\u503C\u5E94\u7528/\u91CD\u8DD1\u6458\u8981\u3002",
	    batch_status = "\u5C1A\u672A\u8FD0\u884C\u6279\u5904\u7406\u3002",
	    isi_profile_ref = NULL,
	    cluster_a = NULL,
	    cluster_b = NULL,
	    manual_undo_snapshot = NULL,
	    syncing_min_isi = FALSE,
    syncing_xrange = FALSE,
	    syncing_threshold_unit = FALSE,
	    qc_isi_unit_last = "ms",
	    last_param_ui_dataset_id = NULL,
	    last_param_yaml_import = NULL,
	    parameter_delta_preview = NULL,
	    parameter_delta_preview_selected_row = NULL,
	    distribution_evidence_selected_row = NULL,
	    parameter_delta_preview_status = "\u5C1A\u672A\u8FD0\u884C\u5C40\u90E8\u5DEE\u5F02\u91CD\u8DD1\u9884\u89C8\u3002",
	    parameter_sensitivity = NULL,
	    parameter_sensitivity_status = "\u5C1A\u672A\u8FD0\u884C\u4E8B\u4EF6\u7EA7\u53C2\u6570\u654F\u611F\u6027\u626B\u63CF\u3002",
	    possible_burst_promotion_preview = NULL,
	    possible_burst_promotion_status = "\u5C1A\u672A\u9884\u89C8 possible_burst \u6279\u91CF\u5347\u7EA7\u3002"
	  )
  
  get_dataset <- function(id = NULL) {
    datasets <- rv$datasets
    if (length(datasets) == 0) return(NULL)
    id <- id %||% rv$current_id
    input_id <- tryCatch(as.character(input$dataset_id %||% "")[1], error = function(e) "")
    if (is.null(id) || length(id) == 0 || !(id %in% names(datasets))) {
      if (nzchar(input_id) && input_id %in% names(datasets)) {
        id <- input_id
      } else {
        id <- names(datasets)[1]
      }
      isolate(rv$current_id <- id)
    }
    ds <- datasets[[id]]
    if (is.null(ds)) return(NULL)
    normalize_dataset(ds)
  }
  
  normalize_dataset <- function(ds) {
    if (is.null(ds$train_settings)) ds$train_settings <- list()
    if (is.null(ds$train_settings$burst_isi_ranges)) ds$train_settings$burst_isi_ranges <- list()
    if (is.null(ds$train_settings$tonic_isi_ranges)) ds$train_settings$tonic_isi_ranges <- list()
    if (is.null(ds$train_settings$pause_isi_ranges)) ds$train_settings$pause_isi_ranges <- list()
    if (is.null(ds$train_settings$highfreq_isi_ranges)) ds$train_settings$highfreq_isi_ranges <- list()
    if (is.null(ds$train_settings$isi_thresholds)) ds$train_settings$isi_thresholds <- list()
    ds$task_events <- stpd_normalize_task_events(ds$task_events %||% data.frame(), source = ds$meta$display_name %||% "")
    if (is.null(ds$ml)) ds$ml <- list(last_feature_table = data.frame(), last_prediction_table = data.frame())
    if (is.null(ds$ml$last_feature_table)) ds$ml$last_feature_table <- data.frame()
    if (is.null(ds$ml$last_prediction_table)) ds$ml$last_prediction_table <- data.frame()
    if (is.null(ds$ml$last_eval_table)) ds$ml$last_eval_table <- data.frame()
    if (is.null(ds$ml$last_eval_metrics)) ds$ml$last_eval_metrics <- data.frame()
    if (is.null(ds$quality) || nrow(ds$quality) == 0) {
      min_isi <- ds$params_last$detector$min_valid_isi_sec %||% ds$params_est$detector$min_valid_isi_sec %||% 0.0009
      ds$quality <- validate_dataset_quality_impl(ds$trains, min_isi_sec = min_isi, unit_hint = ds$meta$unit_in %||% "s", refractory_suspect_sec = refractory_suspect_sec(), display_unit = qc_isi_unit())
    }
    ds
  }
  
  set_dataset <- function(id, ds) {
    rv$datasets[[id]] <- normalize_dataset(ds)
  }
  
  current_dataset <- reactive({
    ds <- get_dataset()
    validate(need(!is.null(ds), "\u8BF7\u81F3\u5C11\u4E0A\u4F20\u4E00\u4E2A\u6570\u636E\u96C6\u3002"))
    ds
  })
  
  current_trains <- reactive({ current_dataset()$trains })

  current_train_metadata <- reactive({
    ds <- current_dataset()
    trains <- ds$trains
    meta <- ds$meta$train_metadata
    if (is.null(meta) || nrow(meta) == 0 || !("train" %in% names(meta))) {
      meta <- tryCatch(parse_spike_train_column_metadata(names(trains), dataset_name = ds$meta$display_name %||% ""), error = function(e) data.frame(train = names(trains), stringsAsFactors = FALSE))
    }
    # Add live spike-count / duration columns so the metadata table is also a recording inventory.
    stats <- lapply(names(trains), function(tr) {
      dat <- trains[[tr]]
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
    meta
  })

  metadata_filtered_train_names <- reactive({
    td <- current_trains()
    choices <- names(td)
    if (length(choices) == 0) return(character(0))
    if (!isTRUE(input$use_train_metadata_filter)) return(choices)
    meta <- current_train_metadata()
    if (is.null(meta) || nrow(meta) == 0 || !("train" %in% names(meta))) return(choices)
    m <- meta[as.character(meta$train) %in% choices, , drop = FALSE]
    keep_all_if_empty <- function(x) is.null(x) || length(x) == 0
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
    intersect(as.character(m$train), choices)
  })

  output$train_metadata_filters <- renderUI({
    if (!isTRUE(input$use_train_metadata_filter)) return(NULL)
    meta <- current_train_metadata()
    validate(need(!is.null(meta) && nrow(meta) > 0, "\u6CA1\u6709\u53EF\u7528\u7684 train \u5143\u6570\u636E\u3002"))
    clean_choices <- function(x) {
      x <- as.character(x)
      x <- x[!is.na(x) & nzchar(x) & x != "unknown"]
      sort(unique(x))
    }
    structures <- if ("structure" %in% names(meta)) clean_choices(meta$structure) else character(0)
    sides <- if ("side" %in% names(meta)) clean_choices(meta$side) else character(0)
    trajs <- if ("trajectory" %in% names(meta)) clean_choices(meta$trajectory) else character(0)
    depth <- if ("recording_depth" %in% names(meta)) suppressWarnings(as.numeric(meta$recording_depth)) else numeric(0)
    depth <- depth[is.finite(depth)]
    depth_ui <- NULL
    if (length(depth) > 0) {
      mn <- floor(min(depth, na.rm = TRUE) * 100) / 100
      mx <- ceiling(max(depth, na.rm = TRUE) * 100) / 100
      if (identical(mn, mx)) mx <- mn + 0.01
      depth_ui <- sliderInput("metadata_filter_depth", "\u4ECE\u5217\u540D\u89E3\u6790\u51FA\u7684\u8BB0\u5F55\u6DF1\u5EA6", min = mn, max = mx, value = c(mn, mx), step = 0.01)
    }
    tags$div(class = "soft-box",
      h5("\u5217\u540D\u5143\u6570\u636E\u8FC7\u6EE4\u5668"),
      if (length(structures) > 0) selectizeInput("metadata_filter_structure", "Structure", choices = structures, selected = structures, multiple = TRUE, options = list(placeholder = "\u6240\u6709\u89E3\u6790\u51FA\u7684\u7ED3\u6784")),
      if (length(sides) > 0) selectizeInput("metadata_filter_side", "Side", choices = sides, selected = sides, multiple = TRUE, options = list(placeholder = "L/R")),
      if (length(trajs) > 0) selectizeInput("metadata_filter_trajectory", "\u8F68\u8FF9 / track", choices = trajs, selected = trajs, multiple = TRUE, options = list(placeholder = "\u6240\u6709\u8F68\u8FF9")),
      depth_ui,
      tags$div(class = "small-note", "\u8FD9\u4E9B\u8FC7\u6EE4\u5668\u53EA\u63A7\u5236\u53EF\u89C1 trains \u548C selected-only \u64CD\u4F5C\uFF1B\u4E0D\u4F1A\u5220\u9664\u6570\u636E\u6216\u6539\u53D8\u6807\u7B7E\u3002")
    )
  })

  output$train_metadata_table <- DT::renderDT({
    meta <- current_train_metadata()
    validate(need(!is.null(meta) && nrow(meta) > 0, "\u6CA1\u6709\u53EF\u7528\u7684 train \u5143\u6570\u636E\u3002"))
    cols <- c("train", "structure", "side", "hemisphere", "trajectory", "recording_depth", "channel_type", "wire", "unit_id", "flag", "duplicate_name_suffix", "n_spikes", "first_timestamp_sec", "last_timestamp_sec", "duration_sec", "parse_ok")
    cols <- intersect(cols, names(meta))
    out <- meta[, cols, drop = FALSE]
    DT::datatable(out, rownames = FALSE, filter = "top", options = list(pageLength = 8, scrollX = TRUE))
  })

  pool_dataset_ids <- reactive({
    ds <- rv$datasets
    if (length(ds) == 0) return(character(0))
    ids <- input$pool_ids
    if (is.null(ids) || length(ids) == 0) rv$current_id else ids
  })
  
  pooled_trains <- reactive({
    ids <- pool_dataset_ids()
    ds <- rv$datasets
    validate(need(length(ids) > 0, "\u672A\u9009\u62E9\u7528\u4E8E pooling \u7684\u6570\u636E\u96C6\u3002"))
    trains <- list()
    ds_map <- list()
    for (id in ids) {
      if (!(id %in% names(ds))) next
      d <- ds[[id]]
      for (tr in names(d$trains)) {
        nm <- paste0(d$meta$display_name, "::", tr)
        trains[[nm]] <- d$trains[[tr]]
        ds_map[[nm]] <- d$meta$display_name
      }
    }
    list(trains = trains, dataset_map = ds_map)
  })
  
  unit_factor <- reactive({ if (identical(input$time_unit, "ms")) 1000 else 1 })
  unit_label <- reactive({ if (identical(input$time_unit, "ms")) "ms" else "s" })
  qc_isi_unit <- reactive({ if (identical(input$qc_isi_unit, "s")) "s" else "ms" })
  threshold_unit_factor_to_sec <- function(u) if (identical(u, "s")) 1 else 1 / 1000
  threshold_unit_factor_from_sec <- function(u) if (identical(u, "s")) 1 else 1000
  threshold_to_sec <- function(x, u = qc_isi_unit()) suppressWarnings(as.numeric(x)) * threshold_unit_factor_to_sec(u)
  threshold_from_sec <- function(x, u = qc_isi_unit()) suppressWarnings(as.numeric(x)) * threshold_unit_factor_from_sec(u)
  pattern_isi_gate_patterns <- function() c("burst", "long_burst", "tonic", "high_frequency_tonic", "high_frequency_spiking", "pause", "others")
  read_pattern_isi_limits <- function() {
    pats <- pattern_isi_gate_patterns()
    out <- list()
    for (pat in pats) {
      min_v <- tryCatch(input[[paste0("pattern_min_isi_", pat)]], error = function(e) 0)
      max_v <- tryCatch(input[[paste0("pattern_max_isi_", pat)]], error = function(e) 0)
      if (is.null(min_v) || length(min_v) == 0) min_v <- 0
      if (is.null(max_v) || length(max_v) == 0) max_v <- 0
      min_s <- threshold_to_sec(min_v)
      max_s <- threshold_to_sec(max_v)
      if (length(min_s) == 0 || !is.finite(min_s[1]) || min_s[1] < 0) min_s <- 0
      if (length(max_s) == 0 || !is.finite(max_s[1]) || max_s[1] < 0) max_s <- 0
      out[[pat]] <- list(min_sec = min_s, max_sec = max_s)
    }
    out
  }
	  min_valid_isi_sec <- reactive({ threshold_to_sec(input$artifact_isi_ms %||% input$min_valid_isi_ms %||% 0.9) })
	  refractory_suspect_sec <- reactive({ max(threshold_to_sec(input$refractory_suspect_ms %||% input$refractory_suspect_ms_param %||% 1.0), min_valid_isi_sec()) })
	  logisi_mcv_sec <- reactive({ as.numeric(input$logisi_mcv_ms %||% 100) / 1000 })

	  manual_action_error_message <- function(e) {
	    msg <- conditionMessage(e)
	    msg <- as.character(msg %||% "")
	    msg <- msg[1]
	    if (!nzchar(msg)) msg <- "\u5F53\u524D\u6846\u9009\u6CA1\u6709\u8986\u76D6\u6709\u6548 ISI\u3002\u8BF7\u91CD\u65B0\u6846\u9009\uFF0C\u6216\u5148\u6E05\u9664\u7F13\u5B58\u9009\u62E9\u3002"
	    map <- c(
	      "Please Box Select on aligned plot first." = "\u8BF7\u5148\u5728\u5BF9\u9F50 raster \u56FE\u4E0A\u6846\u9009\u6709\u6548\u533A\u57DF\u3002",
	      "No finite points were captured by the current box selection." = "\u8FD9\u6B21\u6846\u9009\u6CA1\u6709\u6355\u83B7\u5230\u6709\u6548\u5750\u6807\u70B9\uFF0C\u8BF7\u91CD\u65B0\u6846\u9009\u3002",
	      "No ISI covered by selection." = "\u5F53\u524D\u6846\u9009\u6CA1\u6709\u8986\u76D6\u5230 ISI\uFF0C\u53EF\u4EE5\u7A0D\u5FAE\u653E\u5927\u6846\u9009\u8303\u56F4\u3002",
	      "Selection must stay within ONE train." = "\u4E00\u6B21\u6807\u8BB0\u8BF7\u53EA\u6846\u9009\u540C\u4E00\u6761 train\u3002",
	      "Selection must stay within ONE train row." = "\u4E00\u6B21\u6807\u8BB0\u8BF7\u53EA\u6846\u9009\u540C\u4E00\u6761 train\u3002",
	      "Need at least 2 different spikes." = "\u8BE5\u6A21\u5F0F\u81F3\u5C11\u9700\u8981\u6846\u5230 2 \u4E2A\u4E0D\u540C spike\uFF1B\u5C0F ISI \u53EF\u4EE5\u653E\u5927\u89C6\u56FE\u540E\u518D\u6846\u9009\u3002"
	    )
	    mapped <- unname(map[msg])
	    if (!is.na(mapped) && nzchar(mapped)) mapped else msg
	  }

	  run_manual_ui_action <- function(expr, prefix = "\u624B\u52A8\u64CD\u4F5C\u672A\u5B8C\u6210") {
	    tryCatch(
	      {
	        force(expr)
	        TRUE
	      },
	      shiny.silent.error = function(e) {
	        showNotification(paste0(prefix, "\uFF1A", manual_action_error_message(e)), type = "warning", duration = 5)
	        FALSE
	      },
	      error = function(e) {
	        showNotification(paste0(prefix, "\uFF1A", manual_action_error_message(e)), type = "warning", duration = 5)
	        FALSE
	      }
	    )
	  }

	  push_manual_undo <- function(action = "\u624B\u52A8\u64CD\u4F5C") {
	    ds <- current_dataset()
	    stpd_push_manual_undo(rv, rv$current_id, ds$trains, action)
	  }

  stpd_server_install_parameters_module(environment())

  stpd_server_install_data_io_module(environment())

  stpd_server_install_visualization_module(environment())

	  observeEvent(selected_points(), {
	    sel <- selected_points()
	    if (!stpd_selection_has_points(sel)) return()
	    rv$last_plotly_selection <- sel
	    if (isTRUE(input$auto_label_selection)) {
	      run_manual_ui_action(
	        apply_manual_selection(sel, input$pattern, notify = FALSE),
	        prefix = "\u81EA\u52A8\u6807\u8BB0\u672A\u5B8C\u6210"
	      )
	    }
	  }, ignoreNULL = TRUE)

	  observeEvent(input$add_annot, {
	    ok <- run_manual_ui_action({
	      sel <- selection_from_cache()
	      apply_manual_selection(sel, input$pattern, notify = TRUE)
	    }, prefix = "\u624B\u52A8\u6807\u8BB0\u672A\u5B8C\u6210")
	    if (isTRUE(ok)) plotlyProxy("raster_plot", session) %>% plotlyProxyInvoke("relayout", list(dragmode = "select"))
	  })

	  observeEvent(input$undo_last_manual_action, {
	    run_manual_ui_action({
	      snap <- rv$manual_undo_snapshot
	      validate(need(!is.null(snap), "\u6CA1\u6709\u53EF\u64A4\u9500\u7684\u4E0A\u4E00\u6B21\u624B\u52A8\u64CD\u4F5C\u3002"))
	      validate(need(!is.null(snap$dataset_id) && snap$dataset_id %in% names(rv$datasets), "\u4E0A\u4E00\u6B21\u64CD\u4F5C\u6240\u5C5E\u7684\u6570\u636E\u96C6\u5DF2\u4E0D\u5B58\u5728\u3002"))
	      ds <- get_dataset(snap$dataset_id)
	      validate(need(!is.null(ds), "\u65E0\u6CD5\u8BFB\u53D6\u4E0A\u4E00\u6B21\u64CD\u4F5C\u6240\u5C5E\u7684\u6570\u636E\u96C6\u3002"))
	      ds$trains <- snap$trains
	      set_dataset(snap$dataset_id, ds)
	      rv$current_id <- snap$dataset_id
	      rv$last_plotly_selection <- NULL
	      rv$manual_undo_snapshot <- NULL
	      showNotification(paste0("\u5DF2\u64A4\u9500\u4E0A\u4E00\u6B21\u64CD\u4F5C\uFF1A", snap$action, "\u3002"), type = "message", duration = 3)
	    }, prefix = "\u64A4\u9500\u5931\u8D25")
	  })

  observeEvent(input$clear_cached_selection, {
    rv$last_plotly_selection <- NULL
    showNotification("\u5DF2\u6E05\u9664\u7F13\u5B58\u9009\u62E9\u3002\u6807\u8BB0\u6216\u6E05\u9664\u524D\u8BF7\u91CD\u65B0\u6846\u9009\u3002", type = "message", duration = 3)
  })

  cluster_summary_from_loc <- function(loc, label = "A") {
    td <- current_trains()
    ds <- current_dataset()
    tr <- loc$train
    dat <- td[[tr]]
    idx <- sort(unique(loc$idx))
    idx <- idx[idx >= 2 & idx <= nrow(dat)]
    if (length(idx) == 0) return(data.frame(cluster = label, message = "\u672A\u9009\u62E9\u6709\u6548 ISI\u3002", stringsAsFactors = FALSE))
    s0 <- min(idx); e0 <- max(idx)
    vals <- valid_isi_values(dat$ISI_sec[s0:e0], min_valid_isi_sec())
    pre <- if (s0 > 2) dat$ISI_sec[s0 - 1L] else NA_real_
    post <- if (e0 < nrow(dat)) dat$ISI_sec[e0 + 1L] else NA_real_
    qv <- if (length(vals) > 0) as.numeric(stats::quantile(vals, 0.90, na.rm = TRUE, names = FALSE)) else NA_real_
    pre_ratio <- if (is.finite(pre) && is.finite(qv) && qv > 0) pre / qv else NA_real_
    post_ratio <- if (is.finite(post) && is.finite(qv) && qv > 0) post / qv else NA_real_
    final <- compute_final_pattern(dat$pattern_manual, dat$pattern_auto, dat$ISI_sec, auto_others = FALSE, min_isi_sec = min_valid_isi_sec())
    cand_hit <- ""; cand_reason <- ""; cand_class <- ""
    ledger <- ds$results$candidate_ledger %||% data.frame()
    if (!is.null(ledger) && nrow(ledger) > 0 && all(c("train", "start_isi", "end_isi") %in% names(ledger))) {
      lr <- ledger[as.character(ledger$train) == as.character(tr), , drop = FALSE]
      if (nrow(lr) > 0) {
        ov <- pmax(0L, pmin(e0, suppressWarnings(as.integer(lr$end_isi))) - pmax(s0, suppressWarnings(as.integer(lr$start_isi))) + 1L)
        if (any(ov > 0, na.rm = TRUE)) {
          j <- which.max(ov)
          cand_hit <- as.character(lr$candidate_id[j] %||% "")
          cand_class <- as.character(lr$final_candidate_class[j] %||% lr$raw_candidate_class[j] %||% "")
          cand_reason <- as.character(lr$uncertainty_reason[j] %||% lr$rejection_reason[j] %||% "")
        }
      }
    }
    data.frame(
      cluster = label, train = tr, start_isi = s0, end_isi = e0,
      start_time_sec = dat$timestamp_sec[s0 - 1L], end_time_sec = dat$timestamp_sec[e0],
      n_isi = length(idx), n_spikes = e0 - s0 + 2L, duration_sec = dat$timestamp_sec[e0] - dat$timestamp_sec[s0 - 1L],
      core_q90_ISI_sec = qv, mean_ISI_sec = if (length(vals) > 0) mean(vals) else NA_real_,
      min_ISI_sec = if (length(vals) > 0) min(vals) else NA_real_, max_ISI_sec = if (length(vals) > 0) max(vals) else NA_real_,
      mean_ISI_pct = { tmp_pct <- suppressWarnings(as.numeric(dat$ISI_pct[s0:e0])); if (any(is.finite(tmp_pct))) mean(tmp_pct[is.finite(tmp_pct)]) else NA_real_ },
      max_ISI_pct = { tmp_pct <- suppressWarnings(as.numeric(dat$ISI_pct[s0:e0])); if (any(is.finite(tmp_pct))) max(tmp_pct[is.finite(tmp_pct)]) else NA_real_ },
      pre_ISI_sec = pre, post_ISI_sec = post, pre_core_ratio = pre_ratio, post_core_ratio = post_ratio,
      LV = if (length(vals) >= 2) calc_LV(vals) else NA_real_, CV = if (length(vals) >= 2) calc_CV(vals) else NA_real_,
      MM = if (length(vals) > 0) max(vals) / mean(vals) else NA_real_,
      manual_majority = mode_nonempty_label(dat$pattern_manual[s0:e0]),
      auto_majority = mode_nonempty_label(dat$pattern_auto[s0:e0]),
      final_majority = mode_nonempty_label(final[s0:e0]),
      overlapping_candidate = cand_hit, candidate_class = cand_class, candidate_reason = cand_reason,
      stringsAsFactors = FALSE
    )
  }

	  observeEvent(input$set_cluster_a, {
	    run_manual_ui_action({
	      sel <- selection_from_cache()
	      rv$cluster_a <- selection_time_isi_indices(sel)
	      showNotification("\u5DF2\u4ECE\u5F53\u524D\u9009\u62E9\u4FDD\u5B58\u7C07 A\u3002", type = "message", duration = 2)
	    }, prefix = "\u4FDD\u5B58\u7C07 A \u5931\u8D25")
	  })

	  observeEvent(input$set_cluster_b, {
	    run_manual_ui_action({
	      sel <- selection_from_cache()
	      rv$cluster_b <- selection_time_isi_indices(sel)
	      showNotification("\u5DF2\u4ECE\u5F53\u524D\u9009\u62E9\u4FDD\u5B58\u7C07 B\u3002", type = "message", duration = 2)
	    }, prefix = "\u4FDD\u5B58\u7C07 B \u5931\u8D25")
	  })

  output$cluster_compare_table <- renderDT({
    rows <- list()
    if (!is.null(rv$cluster_a)) rows[[length(rows) + 1L]] <- cluster_summary_from_loc(rv$cluster_a, "A")
    if (!is.null(rv$cluster_b)) rows[[length(rows) + 1L]] <- cluster_summary_from_loc(rv$cluster_b, "B")
    if (length(rows) == 0) return(datatable(data.frame(message = "\u5C1A\u672A\u4FDD\u5B58\u7C07\u3002\u8BF7\u5728 raster \u4E2D\u6846\u9009\u7C07\uFF0C\u7136\u540E\u70B9\u51FB\u201C\u8BBE\u7F6E\u6240\u9009\u7C07 A/B\u201D\u3002", stringsAsFactors = FALSE), rownames = FALSE))
    datatable(bind_rows(rows), rownames = FALSE, options = list(pageLength = 5, scrollX = TRUE))
  })

	  observeEvent(input$clear_selected_manual, {
	    run_manual_ui_action({
	      sel <- selection_from_cache()
	      loc <- selection_time_isi_indices(sel)
	      pats <- input$clear_patterns_manual
	      td <- current_trains()
	      dat <- td[[loc$train]]
	      if (!("pattern_manual_negative" %in% names(dat))) dat$pattern_manual_negative <- rep("", nrow(dat))
	      idx <- loc$idx
	      if (!is.null(pats) && length(pats) > 0) {
	        idx_pos <- if (any(pats != "not_burst")) idx[dat$pattern_manual[idx] %in% pats[pats != "not_burst"]] else integer(0)
	        idx_neg <- if ("not_burst" %in% pats) idx[dat$pattern_manual_negative[idx] != ""] else integer(0)
	        idx <- sort(unique(c(idx_pos, idx_neg)))
	      }
	      validate(need(length(idx) > 0, "\u6CA1\u6709\u4E0E\u8BF7\u6C42\u6A21\u5F0F\u5339\u914D\u7684\u5DF2\u9009 MANUAL \u6807\u7B7E\u3002"))
	      push_manual_undo("\u6E05\u9664\u6240\u9009 MANUAL \u6807\u7B7E")
	      dat$pattern_manual[idx] <- ""
	      dat$pattern_manual_negative[idx] <- ""
	      td[[loc$train]] <- dat
	      update_current_dataset_trains(td)
	      showNotification(paste0("Cleared ", length(idx), " \u4E2A\u6240\u9009\u533A\u57DF\u4E2D\u7684 MANUAL \u6807\u7B7E\u3002"), type = "message", duration = 3)
	    }, prefix = "\u6E05\u9664 MANUAL \u6807\u7B7E\u5931\u8D25")
	  })

	  observeEvent(input$clear_selected_auto, {
	    run_manual_ui_action({
	      sel <- selection_from_cache()
	      loc <- selection_time_isi_indices(sel)
	      pats <- input$clear_patterns_manual
	      td <- current_trains()
	      dat <- td[[loc$train]]
	      idx <- loc$idx
	      if (!is.null(pats) && length(pats) > 0) idx <- idx[dat$pattern_auto[idx] %in% pats]
	      validate(need(length(idx) > 0, "\u6CA1\u6709\u4E0E\u8BF7\u6C42\u6A21\u5F0F\u5339\u914D\u7684\u5DF2\u9009 AUTO \u6807\u7B7E\u3002"))
	      push_manual_undo("\u6E05\u9664\u6240\u9009 AUTO \u6807\u7B7E")
	      dat$pattern_auto[idx] <- ""
	      td[[loc$train]] <- dat
	      update_current_dataset_trains(td)
	      showNotification(paste0("Cleared ", length(idx), " \u4E2A\u6240\u9009\u533A\u57DF\u4E2D\u7684 AUTO \u6807\u7B7E\u3002"), type = "message", duration = 3)
	    }, prefix = "\u6E05\u9664 AUTO \u6807\u7B7E\u5931\u8D25")
	  })

		  observeEvent(input$clear_all_manual, {
		    run_manual_ui_action({
		      td <- current_trains()
		      push_manual_undo("\u6E05\u9664\u5168\u90E8 MANUAL \u6807\u7B7E")
	      for (tr in names(td)) {
	        td[[tr]]$pattern_manual[] <- ""
	        if ("pattern_manual_negative" %in% names(td[[tr]])) td[[tr]]$pattern_manual_negative[] <- ""
	      }
	      update_current_dataset_trains(td)
		    }, prefix = "\u6E05\u9664\u5168\u90E8 MANUAL \u6807\u7B7E\u5931\u8D25")
		  })

	  output$possible_burst_promote_train_selector <- renderUI({
	    td <- current_trains()
	    choices <- names(td)
	    validate(need(length(choices) > 0, "\u6CA1\u6709\u53EF\u7528 train\u3002"))
	    default <- tryCatch(displayed_train_names(), error = function(e) character(0))
	    default <- intersect(default, choices)
	    if (length(default) == 0) {
	      default <- intersect(metadata_filtered_train_names(), choices)
	    }
	    if (length(default) == 0) default <- choices
	    selectizeInput(
	      "possible_burst_promote_trains",
	      "\u9009\u62E9\u8981\u6279\u91CF\u5347\u7EA7 possible_burst \u7684 trains",
	      choices = choices,
	      selected = default,
	      multiple = TRUE,
	      options = list(placeholder = "\u53EF\u9009\u62E5\u591A\u6761 spike train")
	    )
	  })

	  possible_burst_promote_selected_trains <- reactive({
	    td <- current_trains()
	    sel <- input$possible_burst_promote_trains
	    if (is.null(sel) || length(sel) == 0) sel <- tryCatch(displayed_train_names(), error = function(e) names(td))
	    intersect(as.character(sel), names(td))
	  })

	  output$final_audit_train_selector <- renderUI({
	    td <- current_trains()
	    choices <- names(td)
	    validate(need(length(choices) > 0, "\u6CA1\u6709\u53EF\u7528 train\u3002"))
	    default <- tryCatch(displayed_train_names(), error = function(e) character(0))
	    default <- intersect(default, choices)
	    if (length(default) == 0) default <- intersect(metadata_filtered_train_names(), choices)
	    if (length(default) == 0) default <- choices
	    selectizeInput(
	      "final_audit_trains",
	      "\u9009\u62E9\u8981\u751F\u6210\u6700\u7EC8\u5BA1\u8BA1\u7ED3\u679C\u7684 trains",
	      choices = choices,
	      selected = default,
	      multiple = TRUE,
	      options = list(plugins = list("remove_button"), placeholder = "\u53EF\u9009\u591A\u6761 spike train")
	    )
	  })

	  final_audit_selected_trains <- reactive({
	    td <- current_trains()
	    sel <- input$final_audit_trains
	    if (is.null(sel) || length(sel) == 0) sel <- tryCatch(displayed_train_names(), error = function(e) names(td))
	    intersect(as.character(sel), names(td))
	  })

	  output$final_audit_status <- renderText({
	    rv$final_audit_status %||% "\u5C1A\u672A\u751F\u6210\u6700\u7EC8\u5BA1\u8BA1\u7ED3\u679C\u3002\u82E5\u672A\u751F\u6210\uFF0C\u4E0B\u6E38 audit_final \u4F1A\u56DE\u9000\u5230\u5F53\u524D final \u6807\u7B7E\u3002"
	  })

	  output$final_audit_summary_table <- renderDT({
	    sm <- stpd_final_audit_summary(current_dataset())
	    if (is.null(sm) || nrow(sm) == 0) {
	      return(datatable(data.frame(message = "\u5C1A\u65E0\u6700\u7EC8\u5BA1\u8BA1 summary\u3002", stringsAsFactors = FALSE), rownames = FALSE, options = list(dom = "t")))
	    }
	    datatable(sm, rownames = FALSE, options = list(pageLength = 8, scrollX = TRUE))
	  })

	  output$final_audit_event_table <- renderDT({
	    ev <- stpd_final_audit_events(current_dataset())
	    if (is.null(ev) || nrow(ev) == 0) {
	      return(datatable(data.frame(message = "\u5C1A\u65E0 possible \u5347\u7EA7 event\u3002", stringsAsFactors = FALSE), rownames = FALSE, options = list(dom = "t")))
	    }
	    datatable(ev, rownames = FALSE, options = list(pageLength = 8, scrollX = TRUE))
	  })

	  output$final_audit_history_table <- renderDT({
	    hist <- stpd_final_audit_history(current_dataset())
	    if (is.null(hist) || nrow(hist) == 0) {
	      return(datatable(data.frame(message = "\u5C1A\u65E0\u6700\u7EC8\u5BA1\u8BA1\u5386\u53F2\u3002", stringsAsFactors = FALSE), rownames = FALSE, options = list(dom = "t")))
	    }
	    datatable(hist, rownames = FALSE, options = list(pageLength = 8, scrollX = TRUE))
	  })

	  apply_final_audit_from_ui <- function(promote_possible) {
	    ds <- current_dataset()
	    sel <- final_audit_selected_trains()
	    validate(need(length(sel) > 0, "\u8BF7\u81F3\u5C11\u9009\u62E9\u4E00\u6761 train\u3002"))
	    res <- stpd_apply_final_audit(
	      ds,
	      selected_trains = sel,
	      promote_possible = isTRUE(promote_possible),
	      min_isi_sec = min_valid_isi_sec(),
	      reason = if (isTRUE(promote_possible)) "ui_promote_possible_to_final_audit" else "ui_rebuild_final_audit",
	      user = Sys.info()[["user"]] %||% NA_character_
	    )
	    set_dataset(rv$current_id, res$dataset)
	    rv$final_audit_last_summary <- stpd_final_audit_summary(res$dataset)
	    rv$final_audit_last_events <- stpd_final_audit_events(res$dataset)
	    rv$final_audit_status <- paste0(
	      "\u6700\u7EC8\u5BA1\u8BA1\u5DF2\u66F4\u65B0\uFF1A", length(sel), " \u6761 train\uFF1B",
	      "promoted ", sum(res$summary$n_promoted_events %||% 0L), " events / ",
	      sum(res$summary$n_promoted_isi %||% 0L), " ISIs\uFF1B",
	      "possible before=", sum(res$summary$n_possible_before %||% 0L),
	      ", after=", sum(res$summary$n_possible_after %||% 0L), "."
	    )
		    for (id in c("pattern_view", "isi_state_space_label_source", "state_trajectory_label_source",
		                 "neural_manifold_event_label_source", "ml_label_source", "hist_source",
		                 "events_view", "dbs_track_pattern_mode")) {
		      try(updateRadioButtons(session, id, selected = "audit_final"), silent = TRUE)
		    }
	    showNotification("\u6700\u7EC8\u5BA1\u8BA1\u7ED3\u679C\u5DF2\u751F\u6210\uFF0C\u4E0B\u6E38\u5206\u6790\u9ED8\u8BA4\u4F7F\u7528 audit_final\u3002", type = "message", duration = 5)
	    invisible(res)
	  }

	  observeEvent(input$rebuild_final_audit, {
	    run_manual_ui_action({
	      apply_final_audit_from_ui(promote_possible = FALSE)
	    }, prefix = "\u6700\u7EC8\u5BA1\u8BA1\u91CD\u5EFA\u5931\u8D25")
	  })

	  observeEvent(input$promote_possible_to_final_audit, {
	    run_manual_ui_action({
	      res <- apply_final_audit_from_ui(promote_possible = TRUE)
	      if (sum(res$summary$n_promoted_isi %||% 0L) == 0L) {
	        showNotification("\u6240\u9009 train \u6CA1\u6709\u53EF\u5347\u7EA7\u7684 possible_* \u6807\u7B7E\uFF1B\u5BA1\u8BA1\u5C42\u5DF2\u91CD\u5EFA\u3002", type = "warning", duration = 5)
	      }
	    }, prefix = "\u6700\u7EC8\u5BA1\u8BA1 possible \u5347\u7EA7\u5931\u8D25")
	  })

	  observeEvent(input$clear_final_audit, {
	    run_manual_ui_action({
	      ds <- current_dataset()
	      sel <- final_audit_selected_trains()
	      validate(need(length(sel) > 0, "\u8BF7\u81F3\u5C11\u9009\u62E9\u4E00\u6761 train\u3002"))
	      ds <- stpd_clear_final_audit(ds, selected_trains = sel)
	      set_dataset(rv$current_id, ds)
	      rv$final_audit_last_summary <- stpd_final_audit_summary(ds)
	      rv$final_audit_last_events <- stpd_final_audit_events(ds)
	      rv$final_audit_status <- paste0("\u5DF2\u6E05\u9664 ", length(sel), " \u6761 train \u7684\u6700\u7EC8\u5BA1\u8BA1\u5C42\uFF1B\u4E0B\u6E38 audit_final \u5C06\u56DE\u9000\u5230 final\u3002")
	      showNotification("\u5DF2\u6E05\u9664\u6700\u7EC8\u5BA1\u8BA1\u5C42\u3002", type = "message", duration = 4)
	    }, prefix = "\u6E05\u9664\u6700\u7EC8\u5BA1\u8BA1\u5C42\u5931\u8D25")
	  })

	  output$possible_burst_promotion_status <- renderText({
	    rv$possible_burst_promotion_status %||% ""
	  })

	  output$possible_burst_promotion_preview_table <- renderDT({
	    pr <- rv$possible_burst_promotion_preview
	    if (is.null(pr) || is.null(pr$summary) || nrow(pr$summary) == 0) {
	      return(datatable(data.frame(message = "\u8BF7\u5148\u70B9\u51FB\u201C\u9884\u89C8\u5347\u7EA7\u201D\u3002", stringsAsFactors = FALSE), rownames = FALSE, options = list(dom = "t")))
	    }
	    datatable(pr$summary, rownames = FALSE, options = list(pageLength = 8, scrollX = TRUE))
	  })

	  output$possible_burst_promotion_audit_table <- renderDT({
	    ds <- current_dataset()
	    au <- stpd_possible_burst_promotion_audit(ds)
	    if (is.null(au) || nrow(au) == 0) {
	      return(datatable(data.frame(message = "\u5C1A\u65E0 possible_burst \u5347\u7EA7/\u64A4\u56DE\u5BA1\u8BA1\u8BB0\u5F55\u3002", stringsAsFactors = FALSE), rownames = FALSE, options = list(dom = "t")))
	    }
	    datatable(au, rownames = FALSE, options = list(pageLength = 8, scrollX = TRUE))
	  })

	  observeEvent(input$preview_possible_burst_promotion, {
	    run_manual_ui_action({
	      ds <- current_dataset()
	      sel <- possible_burst_promote_selected_trains()
	      validate(need(length(sel) > 0, "\u8BF7\u81F3\u5C11\u9009\u62E9\u4E00\u6761 train\u3002"))
	      pr <- stpd_possible_burst_promotion_preview(
	        ds,
	        selected_trains = sel,
	        overwrite_manual = isTRUE(input$possible_burst_promote_overwrite_manual)
	      )
	      rv$possible_burst_promotion_preview <- pr
	      rv$possible_burst_promotion_status <- paste0(
	        "\u9884\u89C8\u5B8C\u6210\uFF1A",
	        length(sel), " \u6761 train\uFF1B\u53EF\u5347\u7EA7 ",
	        pr$total_eligible_events, " \u4E2A possible_burst event / ",
	        pr$total_eligible_isi, " \u4E2A ISI\u3002"
	      )
	    }, prefix = "possible_burst \u5347\u7EA7\u9884\u89C8\u5931\u8D25")
	  })

	  observeEvent(input$apply_possible_burst_promotion, {
	    run_manual_ui_action({
	      ds <- current_dataset()
	      sel <- possible_burst_promote_selected_trains()
	      validate(need(length(sel) > 0, "\u8BF7\u81F3\u5C11\u9009\u62E9\u4E00\u6761 train\u3002"))
	      pr <- stpd_possible_burst_promotion_preview(
	        ds,
	        selected_trains = sel,
	        overwrite_manual = isTRUE(input$possible_burst_promote_overwrite_manual)
	      )
	      validate(need(pr$total_eligible_isi > 0, "\u6240\u9009 train \u4E2D\u6CA1\u6709\u53EF\u5347\u7EA7\u7684 possible_burst\u3002"))
	      push_manual_undo("possible_burst \u6279\u91CF\u5347\u7EA7\u4E3A burst")
	      res <- stpd_promote_possible_burst(
	        ds,
	        selected_trains = sel,
	        overwrite_manual = isTRUE(input$possible_burst_promote_overwrite_manual),
	        reason = "user_promoted_possible_burst_from_ui",
	        user = Sys.info()[["user"]] %||% NA_character_
	      )
	      set_dataset(rv$current_id, res$dataset)
	      rv$possible_burst_promotion_preview <- res$preview
	      rv$possible_burst_promotion_status <- paste0(
	        "\u5DF2\u6267\u884C\u5347\u7EA7\uFF1A",
	        res$preview$total_eligible_events, " \u4E2A event / ",
	        res$preview$total_eligible_isi, " \u4E2A ISI \u5DF2\u4ECE AUTO possible_burst \u5199\u4E3A MANUAL burst\uFF1BAUTO \u539F\u59CB\u6807\u7B7E\u548C override \u5BA1\u8BA1\u5DF2\u4FDD\u7559\u3002"
	      )
	      updateRadioButtons(session, "pattern_view", selected = "audit_final")
	      showNotification("\u5DF2\u5B8C\u6210 possible_burst \u6279\u91CF\u5347\u7EA7\u3002", type = "message", duration = 4)
	    }, prefix = "possible_burst \u5347\u7EA7\u5931\u8D25")
	  })

	  observeEvent(input$revert_possible_burst_promotion, {
	    run_manual_ui_action({
	      ds <- current_dataset()
	      sel <- possible_burst_promote_selected_trains()
	      validate(need(length(sel) > 0, "\u8BF7\u81F3\u5C11\u9009\u62E9\u4E00\u6761 train\u3002"))
	      push_manual_undo("\u64A4\u56DE possible_burst \u6279\u91CF\u5347\u7EA7")
	      res <- stpd_revert_possible_burst_promotions(ds, selected_trains = sel, protect_manual_edits = TRUE)
	      set_dataset(rv$current_id, res$dataset)
	      pr <- stpd_possible_burst_promotion_preview(
	        res$dataset,
	        selected_trains = sel,
	        overwrite_manual = isTRUE(input$possible_burst_promote_overwrite_manual)
	      )
	      rv$possible_burst_promotion_preview <- pr
	      rv$possible_burst_promotion_status <- paste0(
	        "\u5DF2\u64A4\u56DE\uFF1A",
	        sum(res$summary$n_reverted_events %||% 0L), " \u4E2A event / ",
	        sum(res$summary$n_reverted_isi %||% 0L), " \u4E2A ISI\u3002\u540E\u7EED\u88AB\u624B\u52A8\u6539\u52A8\u8FC7\u7684\u884C\u672A\u88AB\u8986\u76D6\u3002"
	      )
	      showNotification("\u5DF2\u64A4\u56DE possible_burst \u5347\u7EA7\uFF08\u4FDD\u62A4\u540E\u7EED\u624B\u52A8\u4FEE\u6539\uFF09\u3002", type = "message", duration = 4)
	    }, prefix = "possible_burst \u5347\u7EA7\u64A4\u56DE\u5931\u8D25")
	  })

	  observeEvent(input$clear_auto, {
	    td <- current_trains()
	    for (tr in names(td)) {
      td[[tr]]$pattern_auto[] <- ""
      td[[tr]]$auto_score <- NA_real_
    }
    update_current_dataset_trains(td)
  })
  
  # ----------------------------------------------------------
  # Adaptive per-train burst-ISI percentile range controls
  # ----------------------------------------------------------
  save_burst_isi_range_for_trains <- function(target_trains, pct_range,
                                              abs_low = NA_real_,
                                              abs_high = NA_real_,
                                              source = "ui") {
    ds <- current_dataset()
    ds <- normalize_dataset(ds)
    td <- ds$trains
    target_trains <- intersect(target_trains %||% character(0), names(td))
    validate(need(length(target_trains) > 0, "Select at least one train for burst-ISI range assignment."))
    pct_range <- sort(suppressWarnings(as.numeric(pct_range)))
    validate(need(length(pct_range) == 2 && all(is.finite(pct_range)), "Invalid percentile range."))
    pct_range <- clamp(pct_range, 0, 100)
    f <- unit_factor()
    abs_low_sec <- suppressWarnings(as.numeric(abs_low)) / f
    abs_high_sec <- suppressWarnings(as.numeric(abs_high)) / f
    if (!is.finite(abs_low_sec)) abs_low_sec <- NA_real_
    if (!is.finite(abs_high_sec)) abs_high_sec <- NA_real_
    for (tr in target_trains) {
      dat <- td[[tr]]
      lo_pct_sec <- train_isi_cutoff_by_pct(dat, pct_range[1], min_valid_isi_sec())
      hi_pct_sec <- train_isi_cutoff_by_pct(dat, pct_range[2], min_valid_isi_sec())
      lo <- if (is.finite(abs_low_sec)) abs_low_sec else lo_pct_sec
      hi <- if (is.finite(abs_high_sec)) abs_high_sec else hi_pct_sec
      if (is.finite(lo) && is.finite(hi) && hi < lo) {
        tmp <- lo; lo <- hi; hi <- tmp
      }
      ds$train_settings$burst_isi_ranges[[tr]] <- list(
        train = tr,
        low_pct = pct_range[1], high_pct = pct_range[2],
        low_sec = lo, high_sec = hi,
        low_sec_from_pct = lo_pct_sec,
        high_sec_from_pct = hi_pct_sec,
        abs_low_override = is.finite(abs_low_sec),
        abs_high_override = is.finite(abs_high_sec),
        range_mode = input$burst_range_mode %||% "percentile_or_absolute",
        n_valid_isi = sum(is.finite(dat$ISI_sec) & dat$ISI_sec >= min_valid_isi_sec()),
        source = source,
        updated_at = as.character(Sys.time())
      )
    }
    set_dataset(rv$current_id, ds)
    showNotification(paste0("\u5DF2\u4E3A ", length(target_trains), " \u6761 train \u7684 train-specific ISI \u9608\u503C\u3002"), type = "message", duration = 4)
  }
  
  clear_burst_isi_range_for_trains <- function(target_trains) {
    ds <- current_dataset()
    ds <- normalize_dataset(ds)
    target_trains <- intersect(target_trains %||% character(0), names(ds$trains))
    validate(need(length(target_trains) > 0, "Select at least one train to clear."))
    for (tr in target_trains) ds$train_settings$burst_isi_ranges[[tr]] <- NULL
    set_dataset(rv$current_id, ds)
    showNotification(paste0("Cleared burst-ISI range for ", length(target_trains), " \u6761 train \u7684 train-specific ISI \u9608\u503C\u3002"), type = "message", duration = 4)
  }
  
  observeEvent(input$apply_burst_isi_range, {
    save_burst_isi_range_for_trains(input$burst_range_trains, input$burst_isi_pct_range,
                                    input$burst_isi_abs_low, input$burst_isi_abs_high,
                                    source = "ui_sidebar")
  })
  
  observeEvent(input$clear_burst_isi_range, {
    clear_burst_isi_range_for_trains(input$burst_range_trains)
  })
  
  observeEvent(input$apply_burst_isi_range_tab, {
    pct <- input$burst_isi_pct_range_tab
    if (isTRUE(input$sync_sidebar_range)) updateSliderInput(session, "burst_isi_pct_range", value = pct)
    save_burst_isi_range_for_trains(input$burst_range_trains_tab, pct,
                                    input$burst_isi_abs_low_tab, input$burst_isi_abs_high_tab,
                                    source = "ui_tab")
  })
  
  observeEvent(input$clear_burst_isi_range_tab, {
    clear_burst_isi_range_for_trains(input$burst_range_trains_tab)
  })
  
  learn_burst_isi_ranges_from_manual <- function() {
    ds <- current_dataset()
    ds <- normalize_dataset(ds)
    learned <- derive_burst_isi_ranges_from_manual(ds, min_isi_sec = min_valid_isi_sec(),
                                                       expand_pct = safe_ui_value(input$burst_range_expand_pct, 5),
                                                       expand_factor = safe_ui_value(input$burst_range_expand_factor, 1.25))
    validate(need(length(learned) > 0, "No MANUAL burst ISIs found. Label several bursts first, then learn ranges."))
    for (tr in names(learned)) ds$train_settings$burst_isi_ranges[[tr]] <- learned[[tr]]
    set_dataset(rv$current_id, ds)
    showNotification(paste0("\u5DF2\u4ECE MANUAL bursts \u5B66\u4E60 train-specific burst-ISI \u8303\u56F4\uFF0C\u5171 ", length(learned), " \u6761 train \u7684 train-specific ISI \u9608\u503C\u3002"), type = "message", duration = 5)
  }
  
  observeEvent(input$learn_burst_isi_range_manual, {
    learn_burst_isi_ranges_from_manual()
  })
  
  observeEvent(input$learn_burst_isi_range_manual_tab, {
    learn_burst_isi_ranges_from_manual()
  })

  learn_tonic_isi_ranges_from_manual <- function() {
    ds <- current_dataset()
    ds <- normalize_dataset(ds)
    learned <- derive_tonic_isi_ranges_from_manual(ds, min_isi_sec = min_valid_isi_sec())
    validate(need(length(learned) > 0, "No MANUAL tonic ISIs found. Label tonic periods first, then learn ranges."))
    for (tr in names(learned)) ds$train_settings$tonic_isi_ranges[[tr]] <- learned[[tr]]
    set_dataset(rv$current_id, ds)
    showNotification(paste0("\u5DF2\u5B66\u4E60 train-specific tonic-ISI \u8303\u56F4\uFF0C\u5171 ", length(learned), " \u6761 train \u7684 train-specific ISI \u9608\u503C\u3002"), type = "message", duration = 5)
  }

  learn_pause_isi_ranges_from_manual <- function() {
    ds <- current_dataset()
    ds <- normalize_dataset(ds)
    learned <- derive_pause_isi_ranges_from_manual(ds, min_isi_sec = min_valid_isi_sec())
    validate(need(length(learned) > 0, "No MANUAL pause ISIs found. Label pause intervals first, then learn ranges."))
    for (tr in names(learned)) ds$train_settings$pause_isi_ranges[[tr]] <- learned[[tr]]
    set_dataset(rv$current_id, ds)
    showNotification(paste0("\u5DF2\u5B66\u4E60 train-specific pause-ISI \u8303\u56F4\uFF0C\u5171 ", length(learned), " \u6761 train \u7684 train-specific ISI \u9608\u503C\u3002"), type = "message", duration = 5)
  }

  learn_highfreq_isi_ranges_from_manual <- function() {
    ds <- current_dataset()
    ds <- normalize_dataset(ds)
    learned <- derive_highfreq_isi_ranges_from_manual(ds, min_isi_sec = min_valid_isi_sec())
    validate(need(length(learned) > 0, "No MANUAL high-frequency tonic/spiking ISIs found. Label high-frequency periods first, then learn anchors."))
    for (tr in names(learned)) ds$train_settings$highfreq_isi_ranges[[tr]] <- learned[[tr]]
    set_dataset(rv$current_id, ds)
    showNotification(paste0("\u5DF2\u5B66\u4E60 train-specific high-frequency soft anchors\uFF0C\u5171 ", length(learned), " \u6761 train\u3002"), type = "message", duration = 5)
  }

  observeEvent(input$learn_tonic_isi_range_manual_tab, {
    learn_tonic_isi_ranges_from_manual()
  })

  observeEvent(input$learn_pause_isi_range_manual_tab, {
    learn_pause_isi_ranges_from_manual()
  })

  observeEvent(input$learn_highfreq_isi_range_manual_tab, {
    learn_highfreq_isi_ranges_from_manual()
  })

  pattern_range_dataframe <- function(kind = c("tonic", "pause", "highfreq")) {
    kind <- match.arg(kind)
    ds <- current_dataset()
    ds <- normalize_dataset(ds)
    rr <- switch(kind,
                 tonic = ds$train_settings$tonic_isi_ranges,
                 pause = ds$train_settings$pause_isi_ranges,
                 highfreq = ds$train_settings$highfreq_isi_ranges)
    rr <- rr %||% list()
    if (length(rr) == 0) {
      return(data.frame(message = paste0("\u65E0 train-specific ", kind, " soft anchors \u88AB\u5B66\u4E60\u3002"), stringsAsFactors = FALSE))
    }
    f <- unit_factor(); u <- unit_label()
    rows <- lapply(names(rr), function(tr) {
      x <- rr[[tr]]
      n_label <- switch(kind,
                        tonic = range_value(x, "n_manual_tonic_isi", NA_real_),
                        pause = range_value(x, "n_manual_pause_isi", NA_real_),
                        highfreq = range_value(x, "n_manual_highfreq_isi", NA_real_))
      data.frame(
        train = tr,
        low_pct = range_value(x, "low_pct", NA_real_),
        high_pct = range_value(x, "high_pct", NA_real_),
        low_ISI = range_value(x, "low_sec", NA_real_) * f,
        high_ISI = range_value(x, "high_sec", NA_real_) * f,
        unit = u,
        n_valid_isi = range_value(x, "n_valid_isi", NA_real_),
        n_manual_label_isi = n_label,
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
    })
    bind_rows(rows)
  }

  output$tonic_range_table <- renderDT({
    datatable(pattern_range_dataframe("tonic"), rownames = FALSE, options = list(pageLength = 8, scrollX = TRUE))
  })

  output$pause_range_table <- renderDT({
    datatable(pattern_range_dataframe("pause"), rownames = FALSE, options = list(pageLength = 8, scrollX = TRUE))
  })

  output$highfreq_range_table <- renderDT({
    datatable(pattern_range_dataframe("highfreq"), rownames = FALSE, options = list(pageLength = 8, scrollX = TRUE))
  })
  
  burst_range_dataframe <- reactive({
    ds <- current_dataset()
    ds <- normalize_dataset(ds)
    rr <- ds$train_settings$burst_isi_ranges %||% list()
    if (length(rr) == 0) {
      return(data.frame(message = "\u65E0 train-specific burst-ISI ranges saved.", stringsAsFactors = FALSE))
    }
    f <- unit_factor(); u <- unit_label()
    rows <- lapply(names(rr), function(tr) {
      x <- rr[[tr]]
      data.frame(
        train = tr,
        low_pct = range_value(x, "low_pct", NA_real_),
        high_pct = range_value(x, "high_pct", NA_real_),
        low_ISI = range_value(x, "low_sec", NA_real_) * f,
        high_ISI = range_value(x, "high_sec", NA_real_) * f,
        unit = u,
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
    })
    bind_rows(rows)
  })
  
  output$burst_range_table <- renderDT({
    datatable(burst_range_dataframe(), rownames = FALSE, options = list(pageLength = 12, scrollX = TRUE))
  })
  
  isi_percentile_dataframe <- reactive({
    td <- current_trains()
    trains <- intersect(input$isi_table_trains %||% displayed_train_names() %||% names(td), names(td))
    if (length(trains) == 0) trains <- head(names(td), 1)
    pct_filter <- sort(suppressWarnings(as.numeric(input$isi_table_pct_filter %||% c(0, 100))))
    if (length(pct_filter) != 2 || any(!is.finite(pct_filter))) pct_filter <- c(0, 100)
    pct_filter <- clamp(pct_filter, 0, 100)
    f <- unit_factor(); u <- unit_label()
    out <- list()
    for (tr in trains) {
      dat <- ensure_train_isi_percentiles(td[[tr]], min_valid_isi_sec())
      if (nrow(dat) <= 1) next
      pat_final <- compute_final_pattern(dat$pattern_manual, dat$pattern_auto, dat$ISI_sec,
                                         auto_others = isTRUE(input$auto_others),
                                         min_isi_sec = min_valid_isi_sec())
      dd <- data.frame(
        train = tr,
        isi_idx = dat$idx,
        between_spikes = paste0(dat$idx - 1L, "-", dat$idx),
        timestamp = dat$timestamp_sec * f,
        ISI = dat$ISI_sec * f,
        ISI_pct = dat$ISI_pct,
        manual = dat$pattern_manual,
        auto = dat$pattern_auto,
        final = pat_final,
        unit = u,
        stringsAsFactors = FALSE
      )
      dd <- dd[is.finite(dd$ISI_pct) & dd$ISI_pct >= pct_filter[1] & dd$ISI_pct <= pct_filter[2], , drop = FALSE]
      out[[length(out) + 1L]] <- dd
    }
    if (length(out) == 0) return(data.frame(message = "\u6CA1\u6709 ISI \u7B26\u5408\u5F53\u524D\u8FC7\u6EE4\u6761\u4EF6\u3002", stringsAsFactors = FALSE))
    bind_rows(out)
  })
  
  output$isi_percentile_table <- renderDT({
    datatable(isi_percentile_dataframe(), rownames = FALSE, options = list(pageLength = 25, scrollX = TRUE))
  })
  
  # ----------------------------------------------------------
  # Parameter estimation and UI sync
  # ----------------------------------------------------------
  pool_param_est <- reactive({
    pp <- pooled_trains()
    keep <- vapply(pp$trains, function(dat) any(dat$pattern_manual != "", na.rm = TRUE), logical(1))
    trains_use <- pp$trains[keep]
    dataset_map_use <- pp$dataset_map[names(trains_use)]
    validate(need(length(trains_use) > 0, "\u6240\u9009\u4F30\u8BA1\u6C60\u4E2D\u672A\u53D1\u73B0 MANUAL \u6807\u7B7E\u3002"))
    estimate_params_from_manual_pool(trains_use, dataset_map_use, min_isi_sec = min_valid_isi_sec(), logisi_mcv_sec = logisi_mcv_sec())
  })
  
  output$params_summary <- renderText({
    ds <- current_dataset()
    pe <- ds$params_est
    pl <- ds$params_last
    f <- unit_factor()
    u <- unit_label()
    fmt <- function(x) ifelse(is.finite(x), paste0(round(x * f, 4), " ", u), "NA")
    fmt_dim <- function(x) ifelse(is.finite(x), as.character(round(x, 4)), "NA")
    paste0(
      "\u5F53\u524D\u6570\u636E\u96C6: ", ds$meta$display_name, "\n",
      "Estimation pool datasets: ", paste(pool_dataset_ids(), collapse = ", "), "\n\n",
      "Estimated params:\n",
      if (is.null(pe)) "  <none>\n" else paste0(
        "  Burst seed-bridge: T_seed=", fmt(pe$burst$T_seed),
        ", seed_q_max=", fmt(pe$burst$seed_q_max),
        ", bridge ratio max=", fmt_dim(pe$burst$bridge_ratio_max),
        ", final edge min=", fmt_dim(pe$burst$final_edge_contrast_min), "\n",
        "  logISI\uFF1A", (pe$burst$T_log_status %||% ifelse(is.finite(pe$burst$T_log), "resolved", "threshold_unresolved")),
        " (resolved=", (pe$burst$T_log_resolved_n %||% NA), ", unresolved=", (pe$burst$T_log_unresolved_n %||% NA), ")\n",
        "  Pause: T_strong=", fmt(pe$pause$T_strong), ", T_seed=", fmt(pe$pause$T_seed), "\n",
        "  Tonic: T_min=", fmt(pe$tonic$T_min), ", T_max=", fmt(pe$tonic$T_max),
        ", LV_core=", fmt_dim(pe$tonic$LV_core), "\n"
      ),
      "\nLast detector params:\n",
      if (is.null(pl)) "  <none>\n" else paste0(
        "  Burst seed-bridge T_seed=", fmt(pl$burst$T_seed),
        ", seed_q_max=", fmt(pl$burst$seed_q_max),
        ", bridge ratio max=", fmt_dim(pl$burst$bridge_ratio_max),
        ", final edge min=", fmt_dim(pl$burst$final_edge_contrast_min),
        ", G_min=", pl$burst$G_min, "\n",
        "  Seed-bridge model=", pl$burst$use_seed_bridge_model,
        ", possible labels=", pl$burst$label_possible_burst, "\n"
      )
    )
  })
  
  safe_ui_value <- function(x, fallback = NA_real_) {
    if (is.null(x) || length(x) == 0) return(fallback)
    x <- suppressWarnings(as.numeric(x))
    if (length(x) == 0 || !is.finite(x[1])) fallback else x[1]
  }
  
  apply_params_to_ui <- function(p, preserve_pattern_isi_limits = FALSE) {
    if (exists("effective_params_for_detector", mode = "function")) {
      p <- tryCatch(effective_params_for_detector(p), error = function(e) p)
    }
	    f <- unit_factor()
	    ec <- p$event_core %||% list()
	    eg <- p$event_grammar %||% list()
	    ar <- p$arbitration %||% list()
	    event_core_on_for_ui <- isTRUE(ec$enabled %||% TRUE)
	    threshold_source_mode <- eg$threshold_source_mode %||%
	      (((p$spiketrainpattern %||% list())$engine %||% list())$threshold_source_mode) %||%
	      "auto"
	    updateSelectInput(session, "event_grammar_threshold_source_mode", selected = threshold_source_mode)
	    updateSelectInput(session, "plot_lod_mode", selected = p$detector$plot_lod_mode %||% "auto")
    updateNumericInput(session, "plot_max_visible_spikes_full", value = p$detector$plot_max_visible_spikes_full %||% 50000L)
    updateNumericInput(session, "plot_max_visible_spikes_interactive", value = p$detector$plot_max_visible_spikes_interactive %||% 100000L)
    updateNumericInput(session, "refractory_suspect_ms", value = threshold_from_sec(p$detector$refractory_suspect_sec %||% 0.0010))
    updateSelectInput(session, "refractory_suspect_action", selected = p$detector$refractory_suspect_action %||% p$burst$refractory_suspect_action %||% "demote_to_possible")
    updateNumericInput(session, "burst_T_manual", value = ifelse(is.finite(p$burst$T_manual), p$burst$T_manual * f, NA))
    updateNumericInput(session, "burst_T_MI", value = ifelse(is.finite(p$burst$T_MI), p$burst$T_MI * f, NA))
    updateNumericInput(session, "burst_T_log", value = ifelse(is.finite(p$burst$T_log), p$burst$T_log * f, NA))
    updateNumericInput(session, "burst_T_seed", value = p$burst$T_seed * f)
    updateNumericInput(session, "burst_T_bridge", value = p$burst$T_bridge * f)
    updateNumericInput(session, "burst_T_edge_pre", value = p$burst$T_edge_pre * f)
    updateNumericInput(session, "burst_T_edge_post", value = p$burst$T_edge_post * f)
    updateNumericInput(session, "burst_Gmin", value = p$burst$G_min)
    updateNumericInput(session, "burst_Dmin", value = p$burst$D_min * f)
    updateNumericInput(session, "burst_Dmax", value = (p$burst$D_max %||% 0) * f)
    updateCheckboxInput(session, "burst_allow_bridge", value = isTRUE(p$burst$allow_bridge))
    updateNumericInput(session, "burst_connector_n", value = p$burst$connector_max_n)
    updateCheckboxInput(session, "burst_extend_left", value = isTRUE(p$burst$allow_extend_left))
    updateCheckboxInput(session, "burst_extend_right", value = isTRUE(p$burst$allow_extend_right))
    updateNumericInput(session, "burst_extend_frac", value = p$burst$extend_frac)
    updateCheckboxInput(session, "burst_use_seed_bridge", value = isTRUE(p$burst$use_seed_bridge_model %||% TRUE))
    updateCheckboxInput(session, "burst_use_structure", value = isTRUE(p$burst$use_structure_candidates %||% TRUE))
    updateCheckboxInput(session, "burst_adaptive_pct", value = if (isTRUE(event_core_on_for_ui)) FALSE else isTRUE(p$burst$adaptive_apply_core_pct_to_structure %||% FALSE))
    updateCheckboxInput(session, "burst_adaptive_pct_params", value = if (isTRUE(event_core_on_for_ui)) FALSE else isTRUE(p$burst$adaptive_apply_core_pct_to_structure %||% FALSE))
    updateNumericInput(session, "burst_range_expand_pct", value = p$burst$adaptive_range_expand_pct %||% 5)
    updateNumericInput(session, "burst_range_expand_factor", value = p$burst$adaptive_range_expand_factor %||% 1.25)
    updateNumericInput(session, "burst_adaptive_min_isi", value = p$burst$adaptive_min_isi_for_percentile %||% 50)
    updateSelectInput(session, "burst_seed_source_mode", selected = p$burst$seed_source_mode %||% "structure_primary")
    updateSelectInput(session, "burst_seed_nms_mode", selected = p$burst$seed_nms_mode %||% "fractional")
    updateNumericInput(session, "burst_seed_nms_overlap_frac", value = p$burst$seed_nms_overlap_frac %||% 0.75)
    updateCheckboxInput(session, "burst_structure_seed_pre_nms", value = isTRUE(p$burst$structure_seed_pre_nms %||% FALSE))
    updateNumericInput(session, "burst_adaptive_pct_seed", value = p$burst$adaptive_core_pct_seed_max %||% 25)
    updateNumericInput(session, "burst_adaptive_pct_possible", value = p$burst$adaptive_core_pct_possible_max %||% 35)
    updateSelectInput(session, "burst_range_mode", selected = p$burst$adaptive_range_mode %||% "percentile_or_absolute")
    updateCheckboxInput(session, "burst_enforce_learned_low", value = isTRUE(p$burst$adaptive_enforce_learned_low %||% FALSE))
    updateCheckboxInput(session, "burst_use_saved_ranges", value = if (isTRUE(event_core_on_for_ui)) FALSE else isTRUE(p$burst$adaptive_use_train_ranges %||% FALSE))
    updateCheckboxInput(session, "burst_ranges_hard", value = isTRUE(p$burst$adaptive_train_ranges_hard %||% FALSE))
    updateNumericInput(session, "burst_structure_min_isi_n", value = p$burst$structure_core_min_isi_n %||% 2)
    updateNumericInput(session, "burst_structure_max_isi_n", value = p$burst$structure_core_max_isi_n %||% 8)
    updateNumericInput(session, "burst_structure_edge_min", value = p$burst$structure_edge_min %||% 1.25)
    updateNumericInput(session, "burst_structure_edge_geom", value = p$burst$structure_edge_geom_min %||% 1.35)
    updateNumericInput(session, "burst_structure_possible_edge_min", value = p$burst$structure_edge_possible_min %||% 1.05)
    updateNumericInput(session, "burst_structure_possible_edge_geom", value = p$burst$structure_edge_possible_geom_min %||% 1.10)
    updateNumericInput(session, "burst_structure_core_q_min", value = (p$burst$structure_core_q_min %||% 0) * f)
    updateNumericInput(session, "burst_structure_core_q_max", value = (p$burst$structure_core_q_max %||% 0.060) * f)
    updateNumericInput(session, "burst_structure_core_q_loosen", value = p$burst$structure_core_q_loosen %||% 1.25)
    updateCheckboxInput(session, "burst_structure_spread_guard", value = isTRUE(p$burst$structure_prefilter_use_spread_guard %||% TRUE))
    updateNumericInput(session, "burst_structure_core_max_pct", value = p$burst$structure_prefilter_core_max_pct %||% 70)
    updateNumericInput(session, "burst_structure_core_spread_pct", value = p$burst$structure_prefilter_core_spread_pct_max %||% 45)
    updateNumericInput(session, "burst_structure_max_large_isi_n", value = p$burst$structure_prefilter_max_large_isi_n %||% 1L)
    updateNumericInput(session, "burst_structure_duration_max", value = (p$burst$structure_duration_max %||% 0) * f)
    updateNumericInput(session, "burst_structure_min_flanks", value = p$burst$structure_min_flanks %||% 2)
    updateNumericInput(session, "burst_structure_max_candidates", value = p$burst$structure_max_candidates_per_train %||% 2000)
    updateCheckboxInput(session, "burst_structure_possible_as_seed", value = isTRUE(p$burst$structure_use_possible_as_seed %||% FALSE))
    updateCheckboxInput(session, "burst_structure_exclude_tonic_like", value = isTRUE(p$burst$structure_exclude_tonic_like %||% FALSE))
    updateNumericInput(session, "burst_structure_tonic_lv", value = p$burst$structure_tonic_lv_max %||% 0.35)
    updateNumericInput(session, "burst_structure_tonic_mm", value = p$burst$structure_tonic_mm_max %||% 1.20)
    updateNumericInput(session, "burst_sublabel_regular_min_isi", value = (p$burst$burst_sublabel_regular_min_ISI_sec %||% 0.012) * f)
    updateNumericInput(session, "burst_sublabel_regular_max_isi", value = (p$burst$burst_sublabel_regular_max_ISI_sec %||% 0.060) * f)
    updateNumericInput(session, "burst_sublabel_regular_min_isi_n", value = p$burst$burst_sublabel_regular_min_isi_n %||% 4L)
    updateNumericInput(session, "burst_sublabel_regular_max_isi_n", value = p$burst$burst_sublabel_regular_max_isi_n %||% 16L)
    updateNumericInput(session, "burst_seed_min_isi_n", value = p$burst$seed_min_isi_n %||% 2)
    updateNumericInput(session, "burst_seed_max_isi_n", value = p$burst$seed_max_isi_n %||% 8)
    updateNumericInput(session, "burst_seed_q_max", value = (p$burst$seed_q_max %||% 0.035) * f)
    updateNumericInput(session, "burst_seed_q_loosen", value = p$burst$seed_q_loosen %||% 1.35)
    updateNumericInput(session, "burst_seed_split_ratio", value = p$burst$seed_internal_bridge_split_ratio %||% 1.80)
    updateNumericInput(session, "burst_seed_edge_min", value = p$burst$seed_edge_contrast_min %||% 1.05)
    updateNumericInput(session, "burst_seed_edge_geom", value = p$burst$seed_edge_contrast_geom_min %||% 1.10)
    updateNumericInput(session, "burst_seed_duration_max", value = (p$burst$seed_duration_max %||% 0) * f)
    updateNumericInput(session, "burst_bridge_gap_n", value = p$burst$bridge_gap_max_n %||% 1)
    updateNumericInput(session, "burst_bridge_raw_max", value = (p$burst$bridge_raw_max %||% 0.080) * f)
    updateNumericInput(session, "burst_bridge_core_inflate", value = p$burst$bridge_core_inflate %||% 1.25)
    updateCheckboxInput(session, "burst_bridge_dynamic_inflate", value = isTRUE(p$burst$bridge_dynamic_inflate %||% TRUE))
    updateCheckboxInput(session, "burst_bridge_dynamic_requires_strong_seed", value = isTRUE(p$burst$bridge_dynamic_requires_strong_seed %||% TRUE))
    updateNumericInput(session, "burst_bridge_dynamic_inflate_max", value = p$burst$bridge_dynamic_inflate_max %||% 1.75)
    updateCheckboxInput(session, "burst_bridge_use_pct", value = isTRUE(p$burst$bridge_use_pct %||% TRUE))
    updateNumericInput(session, "burst_bridge_pct_max", value = p$burst$bridge_pct_max %||% 35)
    updateNumericInput(session, "burst_bridge_pct_margin", value = p$burst$bridge_pct_margin %||% 10)
    updateNumericInput(session, "burst_bridge_ratio_max", value = p$burst$bridge_ratio_max %||% 3.50)
    updateNumericInput(session, "burst_bridge_ratio_possible", value = p$burst$bridge_ratio_possible_max %||% 5.00)
    updateNumericInput(session, "burst_bridge_edge_min", value = p$burst$bridge_merged_edge_min %||% 1.25)
    updateNumericInput(session, "burst_bridge_edge_geom", value = p$burst$bridge_merged_edge_geom_min %||% 1.30)
    updateNumericInput(session, "burst_max_bridge_count", value = p$burst$max_bridge_count_per_burst %||% 3)
    updateNumericInput(session, "burst_final_edge_min", value = p$burst$final_edge_contrast_min %||% 1.45)
    updateNumericInput(session, "burst_final_edge_geom", value = p$burst$final_edge_contrast_geom_min %||% 1.50)
    updateNumericInput(session, "burst_final_duration_max", value = (p$burst$final_max_duration %||% 0) * f)
    updateNumericInput(session, "burst_final_nspikes_max", value = p$burst$final_max_n_spikes %||% 0)
    updateSelectInput(session, "burst_final_tonic_action", selected = p$burst$final_tonic_like_action %||% "demote_to_possible")
    updateCheckboxInput(session, "burst_final_tonic_veto", value = isTRUE(p$burst$final_tonic_like_veto %||% TRUE))
    updateNumericInput(session, "burst_final_tonic_lv", value = p$burst$final_tonic_like_lv_max %||% 0.35)
    updateNumericInput(session, "burst_final_tonic_cv", value = p$burst$final_tonic_like_cv_max %||% 0.30)
    updateNumericInput(session, "burst_final_tonic_mm", value = p$burst$final_tonic_like_mm_max %||% 1.20)
    updateNumericInput(session, "burst_final_tonic_min_spikes", value = p$burst$final_tonic_like_min_spikes %||% 6)
    updateCheckboxInput(session, "event_arbitration", value = isTRUE(ar$enabled %||% TRUE))
    updateCheckboxInput(session, "manual_negative_labels", value = isTRUE(p$detector$manual_negative_labels_enabled %||% TRUE))
    updateCheckboxInput(session, "seed_bridge_enable", value = isTRUE(p$burst$seed_bridge_classicity_enabled %||% TRUE))
    updateNumericInput(session, "seed_bridge_core_max", value = (p$burst$seed_bridge_burst_core_max_ISI_sec %||% 0.010) * f)
    updateNumericInput(session, "seed_bridge_core_pct", value = p$burst$seed_bridge_burst_core_pct_max %||% 25)
    updateNumericInput(session, "seed_bridge_bridge_max", value = (p$burst$seed_bridge_burst_bridge_max_ISI_sec %||% 0.015) * f)
    updateNumericInput(session, "seed_bridge_bridge_factor", value = p$burst$seed_bridge_burst_bridge_factor %||% 1.50)
    updateNumericInput(session, "seed_bridge_min_core_isi_n", value = p$burst$seed_bridge_burst_core_min_isi_n %||% 2L)
    updateNumericInput(session, "seed_bridge_bridge_count_max", value = p$burst$seed_bridge_burst_bridge_max_count %||% 4L)
    updateNumericInput(session, "seed_bridge_bridge_fraction_max", value = p$burst$seed_bridge_burst_bridge_fraction_max %||% 0.60)
    updateNumericInput(session, "seed_bridge_classicity", value = p$burst$seed_bridge_burst_classicity_multiplier %||% 3.00)
    updateNumericInput(session, "seed_bridge_possible_classicity", value = p$burst$seed_bridge_burst_possible_classicity_multiplier %||% 2.00)
    updateNumericInput(session, "seed_bridge_context_min", value = p$burst$seed_bridge_context_compression_min %||% 1.00)
    updateNumericInput(session, "seed_bridge_edge_return", value = p$burst$seed_bridge_edge_return_min %||% 0.00)
    updateCheckboxInput(session, "dataset_isi_enable", value = isTRUE(ec$enabled %||% TRUE))
    updateNumericInput(session, "event_core_seed_lower", value = (ec$seed_band_lower_sec %||% 0.001) * f)
    updateNumericInput(session, "event_core_seed_upper", value = (ec$seed_band_upper_sec %||% 0.010) * f)
    updateNumericInput(session, "event_core_bridge_upper", value = (ec$bridge_band_upper_sec %||% 0.015) * f)
    updateNumericInput(session, "event_core_boundary_floor", value = (ec$boundary_floor_sec %||% 0) * f)
    updateNumericInput(session, "event_core_classicity", value = ec$burst_contrast_min %||% 2.50)
    updateNumericInput(session, "event_core_possible_classicity", value = ec$possible_burst_contrast_min %||% 2.00)
    updateNumericInput(session, "event_core_min_seed_isi_n", value = ec$min_seed_isi_count %||% 2L)
    updateNumericInput(session, "event_core_bridge_count_max", value = ec$max_bridge_isi_count %||% 4L)
    updateNumericInput(session, "event_core_bridge_fraction_max", value = ec$max_bridge_isi_fraction %||% 0.50)
    updateNumericInput(session, "event_core_expand_each_side", value = ec$max_expansion_isi_each_side %||% 4L)
    updateNumericInput(session, "event_core_max_candidates", value = ec$max_candidates_per_train %||% 2000L)
    updateNumericInput(session, "event_core_hist_bin", value = (ec$histogram_bin_width_sec %||% eg$histogram_bin_width_sec %||% 0.005) * f)
    updateNumericInput(session, "burst_canonical_edge_multiplier", value = p$burst$canonical_burst_edge_multiplier %||% 3.00)
    updateNumericInput(session, "burst_canonical_context_min", value = p$burst$canonical_burst_context_contrast_min %||% 2.50)
    updateNumericInput(session, "burst_canonical_edge_return", value = p$burst$canonical_burst_edge_return_min %||% 0.60)
    updateNumericInput(session, "burst_canonical_abs_ceiling", value = (p$burst$canonical_burst_abs_ceiling_sec %||% 0) * f)
    updateSelectInput(session, "burst_canonical_fuzzy_pct", selected = as.character(p$burst$canonical_burst_abs_ceiling_fuzzy_pct %||% 0))
    updateCheckboxInput(session, "burst_canonical_allow_fuzzy", value = isTRUE(p$burst$canonical_burst_allow_fuzzy_canonical %||% FALSE))
    updateNumericInput(session, "burst_canonical_max_bridge", value = (p$burst$canonical_burst_max_bridge_ISI_sec %||% 0) * f)
    updateNumericInput(session, "burst_canonical_q95_q50", value = p$burst$canonical_burst_internal_q95_q50_ratio_max %||% 3.50)
    updateNumericInput(session, "burst_canonical_max_q50", value = p$burst$canonical_burst_internal_max_q50_ratio_max %||% 5.00)
    updateNumericInput(session, "burst_canonical_cv_max", value = p$burst$canonical_burst_internal_cv_max %||% 1.50)
    updateNumericInput(session, "burst_canonical_lv_max", value = p$burst$canonical_burst_internal_lv_max %||% 2.00)
    # Pattern-specific Min_ISI / Max_ISI gates are user-entered hard constraints.
    # Do not let Estimate Params / Apply Estimated / display-unit resync silently reset them.
    if (!isTRUE(preserve_pattern_isi_limits)) {
      lims <- p$detector$pattern_isi_limits %||% default_params_sec()$detector$pattern_isi_limits
      for (pat in pattern_isi_gate_patterns()) {
        lim <- lims[[pat]] %||% list(min_sec = 0, max_sec = 0)
        updateNumericInput(session, paste0("pattern_min_isi_", pat), value = threshold_from_sec(lim$min_sec %||% 0))
        updateNumericInput(session, paste0("pattern_max_isi_", pat), value = threshold_from_sec(lim$max_sec %||% 0))
      }
    }
    updateNumericInput(session, "burst_seed_diag_max", value = p$burst$seed_bridge_max_seed_candidates %||% 1200)
    updateNumericInput(session, "burst_bridge_diag_max", value = p$burst$seed_bridge_max_bridge_candidates %||% 1200)
    updateCheckboxInput(session, "burst_use_context", value = isTRUE(p$burst$use_context_proposals))
    updateCheckboxInput(session, "burst_fast_context", value = isTRUE(p$burst$fast_context_proposals %||% TRUE))
    updateCheckboxInput(session, "burst_use_local_seed", value = isTRUE(p$burst$use_local_compression_seed))
    updateCheckboxInput(session, "burst_optimize", value = isTRUE(p$burst$use_boundary_optimization))
    updateCheckboxInput(session, "burst_label_possible", value = isTRUE(p$burst$label_possible_burst))
    updateCheckboxInput(session, "burst_merge_fragments", value = isTRUE(p$burst$merge_candidate_fragments))
    updateNumericInput(session, "burst_merge_gap_n", value = p$burst$merge_gap_max_n %||% 2L)
    updateSelectInput(session, "burst_contrast_ref", selected = p$burst$contrast_ref %||% "q")
    updateNumericInput(session, "burst_contrast_q", value = p$burst$contrast_q %||% 0.90)
    updateNumericInput(session, "burst_context_k", value = p$burst$context_k %||% 5)
    updateNumericInput(session, "burst_local_window", value = p$burst$local_window %||% 11)
    updateNumericInput(session, "burst_local_compression", value = p$burst$local_compression_min %||% 1.4)
    updateCheckboxInput(session, "burst_local_compression_mode", value = isTRUE(p$burst$local_compression_burst_mode %||% TRUE))
    updateSelectInput(session, "burst_local_compression_label", selected = p$burst$local_compression_burst_label %||% p$burst$local_compression_candidate_class %||% "possible_burst")
    updateNumericInput(session, "burst_local_compression_pct", value = p$burst$local_compression_core_pct_max %||% 30)
    updateNumericInput(session, "burst_local_compression_local_ratio", value = p$burst$local_compression_local_ratio_min %||% 2.20)
    updateNumericInput(session, "burst_local_compression_edge_min", value = p$burst$local_compression_edge_min %||% p$burst$local_compression_flank_ratio_min %||% 1.80)
    updateNumericInput(session, "burst_local_compression_edge_geom", value = p$burst$local_compression_edge_geom %||% p$burst$local_compression_flank_geom_min %||% 2.50)
    updateNumericInput(session, "burst_local_compression_max_spikes", value = p$burst$local_compression_max_n_spikes %||% 8L)
    updateNumericInput(session, "burst_local_compression_max_duration", value = (p$burst$local_compression_max_duration %||% 0) * f)
    updateCheckboxInput(session, "burst_boundary_mode", value = isTRUE(p$burst$boundary_burst_mode %||% TRUE))
    updateSelectInput(session, "burst_boundary_label", selected = p$burst$boundary_burst_label %||% "possible_burst")
    updateCheckboxInput(session, "burst_label_boundary_possible", value = isTRUE(p$burst$label_boundary_possible_burst %||% TRUE))
    updateNumericInput(session, "burst_boundary_pct", value = p$burst$boundary_core_pct_max %||% 30)
    updateNumericInput(session, "burst_boundary_flank_ratio", value = p$burst$boundary_one_flank_ratio_min %||% 2.50)
    updateNumericInput(session, "burst_boundary_local_ratio", value = p$burst$boundary_local_ratio_min %||% 2.20)
    updateNumericInput(session, "burst_boundary_max_spikes", value = p$burst$boundary_max_n_spikes %||% 8L)
    updateNumericInput(session, "burst_boundary_max_duration", value = (p$burst$boundary_max_duration %||% 0) * f)
    updateCheckboxInput(session, "burst_long_enable", value = isTRUE(p$burst$long_burst_enable %||% TRUE))
    updateNumericInput(session, "burst_long_min_spikes", value = p$burst$long_burst_min_spikes %||% 11L)
    updateNumericInput(session, "burst_long_max_spikes", value = p$burst$long_burst_max_spikes %||% 15L)
    updateNumericInput(session, "burst_long_min_duration", value = (p$burst$long_burst_min_duration %||% 0) * f)
    updateNumericInput(session, "burst_long_max_duration", value = (p$burst$long_burst_max_duration %||% 0) * f)
    updateNumericInput(session, "burst_long_edge_min", value = p$burst$long_burst_edge_contrast_min %||% 1.45)
    updateNumericInput(session, "burst_long_edge_geom", value = p$burst$long_burst_edge_contrast_geom %||% 1.50)
    updateNumericInput(session, "burst_long_core_pct", value = p$burst$long_burst_core_pct_max %||% 35)
    updateNumericInput(session, "burst_long_short_fraction", value = p$burst$long_burst_short_fraction_min %||% 0.65)
    updateNumericInput(session, "burst_win_min", value = p$burst$proposal_window_min_isi %||% 2)
    updateNumericInput(session, "burst_win_max", value = p$burst$proposal_window_max_isi %||% 8)
    updateNumericInput(session, "burst_prop_cmin", value = p$burst$proposal_contrast_min %||% 1.20)
    updateNumericInput(session, "burst_prop_cgeom", value = p$burst$proposal_contrast_geom_min %||% 1.30)
    updateNumericInput(session, "burst_cmin_high", value = p$burst$contrast_min_high %||% 1.80)
    updateNumericInput(session, "burst_cgeom_high", value = p$burst$contrast_geom_high %||% 1.80)
    updateNumericInput(session, "burst_cmin_possible", value = p$burst$contrast_min_possible %||% 1.25)
    updateNumericInput(session, "burst_cgeom_possible", value = p$burst$contrast_geom_possible %||% 1.35)
    updateNumericInput(session, "burst_flanks", value = p$burst$contrast_min_flanks %||% 2)
    updateNumericInput(session, "burst_opt_radius", value = p$burst$optimize_radius %||% 1)
    updateNumericInput(session, "burst_score_high", value = p$burst$score_high %||% 0.65)
    updateNumericInput(session, "burst_score_possible", value = p$burst$score_possible %||% 0.35)
    updateNumericInput(session, "burst_max_candidates", value = p$burst$max_candidates_per_train %||% 600)
    updateNumericInput(session, "burst_max_opt_candidates", value = p$burst$max_optimize_candidates_per_train %||% 200)
    
    updateNumericInput(session, "tonic_seed_ratio", value = p$tonic$seed_ratio)
    updateNumericInput(session, "tonic_T_min", value = p$tonic$T_min * f)
    updateNumericInput(session, "tonic_T_max", value = p$tonic$T_max * f)
    updateNumericInput(session, "tonic_LV_core", value = p$tonic$LV_core)
    updateNumericInput(session, "tonic_LV_pre", value = p$tonic$LV_pre)
    updateNumericInput(session, "tonic_LV_post", value = p$tonic$LV_post)
    updateNumericInput(session, "tonic_local_min", value = p$tonic$local_ratio_min)
    updateNumericInput(session, "tonic_local_max", value = p$tonic$local_ratio_max)
    updateNumericInput(session, "tonic_mm_max", value = p$tonic$tonic_mm_max)
    updateNumericInput(session, "tonic_mm_min", value = p$tonic$tonic_mm_min)
    updateNumericInput(session, "tonic_Gmin", value = p$tonic$G_min)
    updateNumericInput(session, "tonic_Dmin", value = p$tonic$D_min * f)
    updateNumericInput(session, "tonic_connector_budget", value = p$tonic$connector_budget_frac)
    updateNumericInput(session, "tonic_connector_n", value = p$tonic$connector_max_n)
    updateNumericInput(session, "tonic_lv_delta", value = p$tonic$lv_delta)
    updateCheckboxInput(session, "tonic_anti_burst_veto", value = isTRUE(p$tonic$anti_burst_veto))
    updateCheckboxInput(session, "tonic_use_saved_ranges", value = isTRUE(p$tonic$adaptive_use_train_ranges %||% TRUE))
    updateCheckboxInput(session, "tonic_ranges_hard", value = isTRUE(p$tonic$adaptive_train_ranges_hard %||% FALSE))
    updateNumericInput(session, "hf_T_high_max", value = (p$highfreq$T_high_max %||% p$highfreq$ISI_abs_max %||% 0.020) * f)
    updateNumericInput(session, "hf_pct_max", value = p$highfreq$pct_max %||% p$highfreq$ISI_pct_max %||% 30)
    updateNumericInput(session, "hf_min_isi_n", value = p$highfreq$min_isi_n %||% 5L)
    updateNumericInput(session, "hf_short_fraction", value = p$highfreq$short_fraction_min %||% 0.80)
    updateNumericInput(session, "hf_stable_cv", value = p$highfreq$stable_CV_max %||% p$highfreq$CV_stable_max %||% 0.30)
    updateNumericInput(session, "hf_stable_lv", value = p$highfreq$stable_LV_max %||% p$highfreq$LV_stable_max %||% 0.35)
    updateNumericInput(session, "hf_stable_mm", value = p$highfreq$stable_MM_max %||% p$highfreq$MM_stable_max %||% 1.25)
    updateNumericInput(session, "hf_tonic_min_floor", value = (p$highfreq$tonic_min_ISI_floor_sec %||% 0.010) * f)
    updateNumericInput(session, "hf_tonic_low_tail", value = p$highfreq$tonic_low_tail_fraction_max %||% 0.05)
    updateCheckboxInput(session, "hf_tonic_burst_core_veto", value = isTRUE(p$highfreq$tonic_burst_core_veto %||% TRUE))
    updateNumericInput(session, "hf_irregular_cv", value = p$highfreq$irregular_CV_min %||% 0.35)
    updateNumericInput(session, "hf_irregular_lv", value = p$highfreq$irregular_LV_min %||% 0.50)
    updateNumericInput(session, "hf_irregular_mm", value = p$highfreq$irregular_MM_min %||% 1.50)
    updateNumericInput(session, "hf_spiking_min_spikes", value = p$highfreq$spiking_min_spikes %||% 30L)
    updateNumericInput(session, "hf_spiking_min_duration", value = (p$highfreq$spiking_min_duration %||% 0) * f)
    updateCheckboxInput(session, "hf_spiking_use_abs", value = isTRUE(p$highfreq$spiking_use_abs_max %||% TRUE))
    updateNumericInput(session, "hf_spiking_abs_max", value = (p$highfreq$spiking_max_ISI_abs %||% 0.020) * f)
    updateCheckboxInput(session, "hf_spiking_use_pct", value = isTRUE(p$highfreq$spiking_use_pct_max %||% TRUE))
    updateNumericInput(session, "hf_spiking_pct_max", value = p$highfreq$spiking_max_ISI_pct %||% 30)
    updateSelectInput(session, "hf_spiking_gate_logic", selected = p$highfreq$spiking_gate_logic %||% "either")
    updateNumericInput(session, "hf_spiking_short_fraction", value = p$highfreq$spiking_short_fraction_min %||% 0.70)
    updateNumericInput(session, "hf_spiking_epoch_bridge", value = (p$highfreq$spiking_epoch_bridge_ISI_sec %||% 0.030) * f)
    updateNumericInput(session, "hf_spiking_q90_max", value = (p$highfreq$spiking_q90_max_ISI_sec %||% 0.020) * f)
    updateNumericInput(session, "hf_spiking_large_fraction", value = p$highfreq$spiking_allowed_large_isi_fraction %||% 0.25)
    updateNumericInput(session, "hf_spiking_consecutive_large", value = p$highfreq$spiking_max_consecutive_large_isi %||% 3L)
    updateNumericInput(session, "hf_spiking_tolerated_gap", value = (p$highfreq$spiking_tolerated_gap_ISI_sec %||% 0.075) * f)
    
    updateNumericInput(session, "pause_T_strong", value = p$pause$T_strong * f)
    updateNumericInput(session, "pause_T_seed", value = p$pause$T_seed * f)
    updateNumericInput(session, "pause_Dmin", value = p$pause$D_min * f)
    updateNumericInput(session, "pause_Gmin", value = p$pause$G_min)
    updateNumericInput(session, "pause_alpha", value = p$pause$alpha)
    updateNumericInput(session, "pause_beta", value = p$pause$beta)
    updateNumericInput(session, "pause_ctx_relax", value = p$pause$context_relax)
    updateNumericInput(session, "pause_ctx_tight", value = p$pause$context_tight)
    updateCheckboxInput(session, "pause_exclude_occupied_context", value = isTRUE(p$pause$exclude_occupied_context %||% TRUE))
    updateCheckboxInput(session, "pause_global_median_guard", value = isTRUE(p$pause$global_median_guard %||% TRUE))
    updateNumericInput(session, "pause_global_median_factor", value = p$pause$global_median_factor %||% 2.5)
    updateCheckboxInput(session, "pause_anti_tonic_veto", value = isTRUE(p$pause$anti_tonic_veto))
    updateCheckboxInput(session, "pause_use_saved_ranges", value = isTRUE(p$pause$adaptive_use_train_ranges %||% TRUE))
    updateCheckboxInput(session, "pause_ranges_hard", value = isTRUE(p$pause$adaptive_train_ranges_hard %||% FALSE))
	    stpd_update_schema_inputs(session, p, prefix = "schema_param_")
	    stpd_update_schema_inputs(session, p, schema = stpd_contract_ui_schema(), prefix = "contract_param_", exclude_paths = character())
	  }

	  manual_threshold_priority_params <- function(p) {
	    if (is.null(p$event_grammar)) p$event_grammar <- list()
	    p$event_grammar$threshold_source_mode <- "manual"
	    if (is.null(p$spiketrainpattern)) p$spiketrainpattern <- list()
	    if (is.null(p$spiketrainpattern$engine)) p$spiketrainpattern$engine <- list()
	    p$spiketrainpattern$engine$threshold_source_mode <- "manual"
	    p
	  }

	  estimate_manual_params_into_ui <- function(prefer_manual_thresholds = FALSE, notify = TRUE) {
	    current_pattern_isi_limits <- read_pattern_isi_limits()
	    p <- pool_param_est()
	    # Manual pattern-specific Min_ISI / Max_ISI gates are not estimated from labels.
	    # Preserve the user's current hard gates instead of replacing them with default zeros.
	    if (is.null(p$detector)) p$detector <- list()
	    p$detector$pattern_isi_limits <- current_pattern_isi_limits
	    if (isTRUE(prefer_manual_thresholds)) p <- manual_threshold_priority_params(p)
	    ds <- current_dataset()
	    ds$params_est <- p
	    if (!is.null(ds$params_last)) {
	      if (is.null(ds$params_last$detector)) ds$params_last$detector <- list()
	      ds$params_last$detector$pattern_isi_limits <- current_pattern_isi_limits
	      if (isTRUE(prefer_manual_thresholds)) ds$params_last <- manual_threshold_priority_params(ds$params_last)
	    }
	    rv$prefer_params_est_once <- TRUE
	    set_dataset(rv$current_id, ds)
	    apply_params_to_ui(p, preserve_pattern_isi_limits = TRUE)
	    if (isTRUE(prefer_manual_thresholds)) {
	      updateSelectInput(session, "event_grammar_threshold_source_mode", selected = "manual")
	    }
	    if (isTRUE(notify)) {
	      suffix <- if (isTRUE(prefer_manual_thresholds)) "\u4E8B\u4EF6\u8BED\u6CD5\u9608\u503C\u5DF2\u5207\u5230 MANUAL \u4F18\u5148\u3002" else "\u5DF2\u4FDD\u7559\u5F53\u524D\u9608\u503C\u6765\u6E90\u4F18\u5148\u7EA7\u3002"
	      showNotification(
	        paste0("\u5DF2\u4ECE pooled MANUAL \u6807\u7B7E\u4F30\u8BA1\u53C2\u6570\u5E76\u5199\u5165 UI\u3002\u5DF2\u4FDD\u7559 pattern-specific ISI gates\u3002", suffix),
	        type = "message",
	        duration = 8
	      )
	    }
	    invisible(p)
	  }
	  
	  observeEvent(current_dataset(), {
	    ds <- current_dataset()
	    prefer_est <- isTRUE(rv$prefer_params_est_once)
	    p <- if (prefer_est && !is.null(ds$params_est)) ds$params_est else ds$params_last %||% ds$params_est %||% default_params_sec()
	    rv$prefer_params_est_once <- FALSE
	    same_dataset <- identical(rv$last_param_ui_dataset_id, rv$current_id)
    # Loading a different dataset/workspace may restore stored pattern gates.
    # Re-syncing the same dataset after Estimate Params / Run Detector must not
    # overwrite user-entered pattern-specific hard gates.
    apply_params_to_ui(p, preserve_pattern_isi_limits = same_dataset)
    rv$last_param_ui_dataset_id <- rv$current_id
  }, ignoreInit = TRUE)

  observeEvent(input$time_unit, {
    ds <- current_dataset()
    p <- ds$params_last %||% ds$params_est %||% default_params_sec()
    apply_params_to_ui(p, preserve_pattern_isi_limits = TRUE)
  }, ignoreInit = TRUE)
  
	  observeEvent(input$estimate_params, {
	    tryCatch({
	      estimate_manual_params_into_ui(prefer_manual_thresholds = FALSE, notify = TRUE)
	    }, error = function(e) {
	      showNotification(paste0("\u53C2\u6570\u4F30\u8BA1\u5931\u8D25\uFF1A", e$message), type = "error", duration = 10)
	    })
	  })

	  observeEvent(input$estimate_apply_manual_params, {
	    tryCatch({
	      estimate_manual_params_into_ui(prefer_manual_thresholds = TRUE, notify = TRUE)
	    }, error = function(e) {
	      showNotification(paste0("\u4ECE MANUAL \u66F4\u65B0\u53C2\u6570\u5931\u8D25\uFF1A", e$message), type = "error", duration = 10)
	    })
	  })
  
  observeEvent(input$apply_estimated_to_ui, {
    ds <- current_dataset()
    p <- ds$params_est
    validate(need(!is.null(p), "No estimated parameters available yet."))
    apply_params_to_ui(p, preserve_pattern_isi_limits = TRUE)
  })
  

  output$methodological_warning <- renderText({
    stpd_methodological_warning()
  })
  output$preset_catalog_table <- renderDT({
    datatable(preset_catalog(), options = list(pageLength = 5, scrollX = TRUE), rownames = FALSE)
  })
  output$semantic_consistency_report <- renderDT({
    ds <- current_dataset()
    rep <- if (!is.null(ds) && !is.null(ds$results)) (ds$results$semantic_consistency_report %||% ds$results$consistency_audit) else data.frame()
    datatable(rep %||% data.frame(), options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
  })
  output$governance_summary_table <- renderDT({
    ds <- current_dataset()
    dat <- if (!is.null(ds) && !is.null(ds$results)) ds$results$governance_summary else params_governance_summary(read_params_from_ui())
    datatable(dat %||% data.frame(), options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE)
  })
  output$parameters_report_table <- renderDT({
    ds <- current_dataset()
    dat <- if (!is.null(ds) && !is.null(ds$results) && !is.null(ds$results$parameters_report)) ds$results$parameters_report else parameter_report_table(read_params_from_ui())
    datatable(dat %||% data.frame(), options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE)
  })
  output$stationarity_qc_table <- renderDT({
    ds <- current_dataset()
    dat <- if (!is.null(ds) && !is.null(ds$results) && !is.null(ds$results$stationarity_qc)) ds$results$stationarity_qc else {
      if (!is.null(ds) && !is.null(ds$trains)) stationarity_qc(ds$trains, min_isi_sec = min_valid_isi_sec()) else data.frame()
    }
    datatable(dat %||% data.frame(), options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE)
  })
  output$validation_guidance_table <- renderDT({
    ds <- current_dataset()
    dat <- if (!is.null(ds) && !is.null(ds$results)) ds$results$validation_guidance else validation_guidance(NULL, read_params_from_ui())
    datatable(dat %||% data.frame(), options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE)
  })
  output$overfit_warning_table <- renderDT({
    ds <- current_dataset()
    dat <- if (!is.null(ds) && !is.null(ds$results) && !is.null(ds$results$overfit_warning_report)) ds$results$overfit_warning_report else {
      if (!is.null(ds)) overfit_warning_report(ds, read_params_from_ui()) else data.frame()
    }
    datatable(dat %||% data.frame(), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })
  output$development_roadmap_table_table <- renderDT({
    datatable(development_roadmap(), options = list(pageLength = 5, scrollX = TRUE), rownames = FALSE)
  })
  output$candidate_features_audit_table <- renderDT({
    ds <- current_dataset()
    dat <- if (!is.null(ds) && !is.null(ds$results)) ds$results$candidate_features else data.frame()
    datatable(dat %||% data.frame(), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })
  output$event_distribution_evidence_table <- renderDT({
    ds <- current_dataset()
    dat <- if (!is.null(ds) && !is.null(ds$results)) ds$results$event_distribution_evidence else data.frame()
    datatable(dat %||% data.frame(), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE, selection = "single")
  })
  distribution_evidence_current_row <- function() {
    ds <- current_dataset()
    dat <- if (!is.null(ds) && !is.null(ds$results)) ds$results$event_distribution_evidence else data.frame()
    if (is.null(dat) || !is.data.frame(dat) || nrow(dat) == 0L) return(NULL)
    sel <- suppressWarnings(as.integer(input$event_distribution_evidence_table_rows_selected %||% rv$distribution_evidence_selected_row %||% NA_integer_))
    if (length(sel) != 1L || !is.finite(sel)) return(NULL)
    if (sel < 1L || sel > nrow(dat)) return(NULL)
    dat[sel, , drop = FALSE]
  }
  jump_to_distribution_evidence_row <- function(row) {
    if (is.null(row) || !is.data.frame(row) || nrow(row) == 0L) return(invisible(FALSE))
    ds <- current_dataset()
    tr <- as.character(row$train[1] %||% "")
    if (is.null(ds) || !nzchar(tr) || !(tr %in% names(ds$trains))) return(invisible(FALSE))
    dat <- ds$trains[[tr]]
    s_isi <- suppressWarnings(as.integer(row$start_isi[1]))
    e_isi <- suppressWarnings(as.integer(row$end_isi[1]))
    if (!is.finite(s_isi) || !is.finite(e_isi) || s_isi < 2L || e_isi < s_isi || e_isi > nrow(dat)) {
      return(invisible(FALSE))
    }
    label <- as.character(row$audit_final_label[1] %||% "")
    if (!nzchar(label)) label <- as.character(row$grammar_detected_label[1] %||% "candidate")
    rv$preview_candidate <- list(
      train = tr,
      pattern = label,
      category = "distributional_evidence",
      parameter = as.character(row$distribution_support[1] %||% ""),
      start_isi = s_isi,
      end_isi = e_isi,
      start_time_sec = suppressWarnings(as.numeric(row$start_time_sec[1])),
      end_time_sec = suppressWarnings(as.numeric(row$end_time_sec[1])),
      details = as.character(row$distribution_support_reason[1] %||% ""),
      active = TRUE
    )
    updateCheckboxInput(session, "show_near_miss_preview", value = TRUE)
    sidx <- max(1L, s_isi - 1L)
    eidx <- min(nrow(dat), e_isi)
    t0 <- dat$timestamp_sec[sidx] - dat$timestamp_sec[1]
    t1 <- dat$timestamp_sec[eidx] - dat$timestamp_sec[1]
    if (!is.finite(t0) || !is.finite(t1) || t1 < t0) return(invisible(FALSE))
    pad <- max(0.025, (t1 - t0) * 3, (current_param_for_tables()$burst$preview_pad_sec %||% 0.150))
    f <- unit_factor()
    rv$view_align_x <- c(max(0, t0 - pad), t1 + pad) * f
    updateSliderInput(session, "xrange", value = rv$view_align_x)
    if (identical(input$train_display_mode %||% "paged_all", "selected_only")) {
      if (is.null(input$trains) || !(tr %in% input$trains)) {
        updateSelectizeInput(session, "trains", selected = unique(c(tr, input$trains %||% character(0))))
      }
    } else {
      td_now <- names(current_trains())
      pos <- match(tr, td_now)
      if (is.finite(pos)) {
        per <- safe_int(input$visible_trains_per_page, 10L)
        updateNumericInput(session, "train_page", value = max(1L, ceiling(pos / max(1L, per))))
      }
    }
    updateTabsetPanel(session, "main_tabs", selected = "\u5BF9\u9F50\u65F6\u95F4\u6233\u56FE")
    invisible(TRUE)
  }
  observeEvent(input$event_distribution_evidence_table_rows_selected, {
    sel <- input$event_distribution_evidence_table_rows_selected
    if (length(sel) == 1L && is.finite(suppressWarnings(as.integer(sel)))) {
      rv$distribution_evidence_selected_row <- suppressWarnings(as.integer(sel))
    }
  }, ignoreNULL = TRUE)
  observeEvent(input$distribution_evidence_jump, {
    row <- distribution_evidence_current_row()
    if (is.null(row)) {
      showNotification("\u8BF7\u5148\u5728\u5206\u5E03\u8BC1\u636E\u8868\u4E2D\u9009\u4E2D\u4E00\u884C\u5019\u9009\u4E8B\u4EF6\u3002", type = "warning", duration = 4)
      return()
    }
    ok <- jump_to_distribution_evidence_row(row)
    if (isTRUE(ok)) {
      showNotification("\u5DF2\u8DF3\u8F6C\u5230\u9009\u4E2D\u5206\u5E03\u8BC1\u636E\u5019\u9009\u7684 raster \u65F6\u95F4\u7A97\u3002", type = "message", duration = 4)
    } else {
      showNotification("\u65E0\u6CD5\u8DF3\u8F6C\uFF1A\u8BE5\u5019\u9009\u884C\u7F3A\u5C11\u6709\u6548 train/start_isi/end_isi\u3002", type = "warning", duration = 5)
    }
  }, ignoreNULL = TRUE)
  output$train_distribution_features_table <- renderDT({
    ds <- current_dataset()
    dat <- if (!is.null(ds) && !is.null(ds$results)) ds$results$train_distribution_features else data.frame()
    datatable(dat %||% data.frame(), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })
  output$spike_count_pmf_table <- renderDT({
    ds <- current_dataset()
    dat <- if (!is.null(ds) && !is.null(ds$results)) ds$results$spike_count_pmf else data.frame()
    datatable(dat %||% data.frame(), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })
  output$final_classification_audit_table <- renderDT({
    ds <- current_dataset()
    dat <- if (!is.null(ds) && !is.null(ds$results)) ds$results$final_classification_audit else data.frame()
    datatable(dat %||% data.frame(), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  observeEvent(input$apply_analysis_preset, {
    base <- read_params_from_ui()
    preset_name <- input$analysis_preset %||% "balanced_single_unit"
    pset <- apply_preset_to_params(base, preset_name)
    # Update a deliberately small set of high-impact UI controls; advanced values stay user-editable.
    updateNumericInput(session, "artifact_isi_ms", value = threshold_from_sec(pset$detector$min_valid_isi_sec %||% 0.0009))
    updateNumericInput(session, "refractory_suspect_ms", value = threshold_from_sec(pset$detector$refractory_suspect_sec %||% 0.0010))
    updateSelectInput(session, "refractory_suspect_action", selected = pset$detector$refractory_suspect_action %||% "demote_to_possible")
    updateCheckboxInput(session, "burst_label_possible", value = isTRUE(pset$burst$label_possible_burst))
    updateSelectInput(session, "burst_final_tonic_action", selected = pset$burst$final_tonic_like_action %||% "demote_to_possible")
    updateSelectInput(session, "burst_local_compression_label", selected = pset$burst$local_compression_candidate_class %||% "possible_burst")
    updateNumericInput(session, "burst_long_edge_min", value = pset$burst$long_burst_edge_contrast_min %||% 1.45)
    updateNumericInput(session, "burst_long_edge_geom", value = pset$burst$long_burst_edge_contrast_geom %||% 1.50)
    updateCheckboxInput(session, "pause_global_median_guard", value = isTRUE(pset$pause$global_median_guard %||% TRUE))
    updateNumericInput(session, "pause_global_median_factor", value = pset$pause$global_median_factor %||% 2.5)
    stpd_update_schema_inputs(session, pset, prefix = "schema_param_")
    stpd_update_schema_inputs(session, pset, schema = stpd_contract_ui_schema(), prefix = "contract_param_", exclude_paths = character())
    showNotification(paste0("\u5DF2\u5E94\u7528\u9884\u8BBE\uFF1A", preset_name, "\u3002\u8BF7\u8FD0\u884C\u68C0\u6D4B\u5668\u751F\u6210\u65B0\u7684 params_hash\u3002"), type = "message", duration = 5)
  }, ignoreInit = TRUE)

	  read_params_from_ui <- function() {
    f <- unit_factor()
    patterns_to_run_ui <- stpd_resolve_patterns_to_run(
      input$patterns_to_run,
      strict_subset = isTRUE(input$patterns_to_run_strict_subset %||% FALSE)
    )
    p <- list(
      burst = list(
        T_manual = safe_ui_value(input$burst_T_manual) / f,
        T_MI = safe_ui_value(input$burst_T_MI) / f,
        T_log = safe_ui_value(input$burst_T_log) / f,
        T_seed = safe_ui_value(input$burst_T_seed, 20) / f,
        T_bridge = safe_ui_value(input$burst_T_bridge, 35) / f,
        T_edge_pre = safe_ui_value(input$burst_T_edge_pre, 35) / f,
        T_edge_post = safe_ui_value(input$burst_T_edge_post, 35) / f,
        G_min = safe_int(input$burst_Gmin, 3L),
        D_min = safe_ui_value(input$burst_Dmin, 0) / f,
        D_max = safe_ui_value(input$burst_Dmax, 0) / f,
        allow_bridge = isTRUE(input$burst_allow_bridge),
        connector_max_n = safe_int(input$burst_connector_n, 1L),
        allow_extend_left = isTRUE(input$burst_extend_left),
        allow_extend_right = isTRUE(input$burst_extend_right),
        extend_frac = safe_ui_value(input$burst_extend_frac, 0.05),
        use_seed_bridge_model = isTRUE(input$burst_use_seed_bridge),
        use_structure_candidates = isTRUE(input$burst_use_structure),
        adaptive_apply_core_pct_to_structure = (!isTRUE(input$dataset_isi_enable %||% TRUE)) && (isTRUE(input$burst_adaptive_pct) || isTRUE(input$burst_adaptive_pct_params)),
        adaptive_core_pct_seed_max = safe_ui_value(input$burst_adaptive_pct_seed, 25),
        adaptive_core_pct_possible_max = safe_ui_value(input$burst_adaptive_pct_possible, 35),
        adaptive_min_isi_for_percentile = safe_int(input$burst_adaptive_min_isi, 50L),
        adaptive_min_isi_for_warning = min(30L, safe_int(input$burst_adaptive_min_isi, 50L)),
        adaptive_range_expand_pct = safe_ui_value(input$burst_range_expand_pct, 5),
        adaptive_range_expand_factor = safe_ui_value(input$burst_range_expand_factor, 1.25),
        adaptive_use_train_ranges = (!isTRUE(input$dataset_isi_enable %||% TRUE)) && isTRUE(input$burst_use_saved_ranges),
        adaptive_train_ranges_hard = isTRUE(input$burst_ranges_hard),
        adaptive_range_mode = input$burst_range_mode %||% "percentile_or_absolute",
        adaptive_enforce_learned_low = isTRUE(input$burst_enforce_learned_low),
        adaptive_train_ranges = if ((!isTRUE(input$dataset_isi_enable %||% TRUE)) && isTRUE(input$burst_use_saved_ranges)) (get_dataset()$train_settings$burst_isi_ranges %||% list()) else list(),
        structure_core_min_isi_n = safe_int(input$burst_structure_min_isi_n, 2L),
        structure_core_max_isi_n = safe_int(input$burst_structure_max_isi_n, 8L),
        structure_edge_min = safe_ui_value(input$burst_structure_edge_min, 1.25),
        structure_edge_geom_min = safe_ui_value(input$burst_structure_edge_geom, 1.35),
        structure_edge_possible_min = safe_ui_value(input$burst_structure_possible_edge_min, 1.05),
        structure_edge_possible_geom_min = safe_ui_value(input$burst_structure_possible_edge_geom, 1.10),
        structure_core_q_min = safe_ui_value(input$burst_structure_core_q_min, 0) / f,
        structure_core_q_max = safe_ui_value(input$burst_structure_core_q_max, 60) / f,
        structure_core_q_loosen = safe_ui_value(input$burst_structure_core_q_loosen, 1.25),
        structure_duration_max = safe_ui_value(input$burst_structure_duration_max, 0) / f,
        structure_min_flanks = safe_int(input$burst_structure_min_flanks, 2L),
        structure_max_candidates_per_train = safe_int(input$burst_structure_max_candidates, 2000L),
        structure_prefilter_rejects = TRUE,
        structure_prefilter_use_spread_guard = isTRUE(input$burst_structure_spread_guard),
        structure_prefilter_core_max_pct = safe_ui_value(input$burst_structure_core_max_pct, 70),
        structure_prefilter_core_spread_pct_max = safe_ui_value(input$burst_structure_core_spread_pct, 45),
        structure_prefilter_max_large_isi_n = safe_int(input$burst_structure_max_large_isi_n, 1L),
        structure_use_possible_as_seed = isTRUE(input$burst_structure_possible_as_seed),
        structure_seed_pre_nms = isTRUE(input$burst_structure_seed_pre_nms),
        structure_exclude_tonic_like = isTRUE(input$burst_structure_exclude_tonic_like),
        structure_tonic_lv_max = safe_ui_value(input$burst_structure_tonic_lv, 0.35),
        structure_tonic_mm_max = safe_ui_value(input$burst_structure_tonic_mm, 1.20),
        structure_weighted_hist = TRUE,
        burst_sublabel_regular_min_ISI_sec = safe_ui_value(input$burst_sublabel_regular_min_isi, 12) / f,
        burst_sublabel_regular_max_ISI_sec = safe_ui_value(input$burst_sublabel_regular_max_isi, 60) / f,
        burst_sublabel_regular_min_isi_n = safe_int(input$burst_sublabel_regular_min_isi_n, 4L),
        burst_sublabel_regular_max_isi_n = safe_int(input$burst_sublabel_regular_max_isi_n, 16L),
        burst_sublabel_regular_max_gap_isi_n = 0L,
        burst_sublabel_regular_max_gap_sec = 0,
        burst_sublabel_link_labels = c("burst", "long_burst"),
        seed_source_mode = input$burst_seed_source_mode %||% "structure_primary",
        seed_nms_mode = input$burst_seed_nms_mode %||% "fractional",
        seed_nms_overlap_frac = safe_ui_value(input$burst_seed_nms_overlap_frac, 0.75),
        seed_min_isi_n = safe_int(input$burst_seed_min_isi_n, 2L),
        seed_max_isi_n = safe_int(input$burst_seed_max_isi_n, 8L),
        seed_q_max = safe_ui_value(input$burst_seed_q_max, 35) / f,
        seed_q_loosen = safe_ui_value(input$burst_seed_q_loosen, 1.35),
        seed_internal_bridge_split_ratio = safe_ui_value(input$burst_seed_split_ratio, 1.80),
        seed_edge_contrast_min = safe_ui_value(input$burst_seed_edge_min, 1.05),
        seed_edge_contrast_geom_min = safe_ui_value(input$burst_seed_edge_geom, 1.10),
        seed_duration_max = safe_ui_value(input$burst_seed_duration_max, 0) / f,
        bridge_gap_max_n = safe_int(input$burst_bridge_gap_n, 1L),
        bridge_raw_max = safe_ui_value(input$burst_bridge_raw_max, 80) / f,
        bridge_core_inflate = safe_ui_value(input$burst_bridge_core_inflate, 1.25),
        bridge_dynamic_inflate = isTRUE(input$burst_bridge_dynamic_inflate),
        bridge_dynamic_requires_strong_seed = isTRUE(input$burst_bridge_dynamic_requires_strong_seed),
        bridge_dynamic_inflate_max = safe_ui_value(input$burst_bridge_dynamic_inflate_max, 1.75),
        bridge_use_pct = isTRUE(input$burst_bridge_use_pct),
        bridge_pct_max = safe_ui_value(input$burst_bridge_pct_max, 35),
        bridge_pct_margin = safe_ui_value(input$burst_bridge_pct_margin, 10),
        bridge_ratio_max = safe_ui_value(input$burst_bridge_ratio_max, 3.50),
        bridge_ratio_possible_max = safe_ui_value(input$burst_bridge_ratio_possible, 5.00),
        bridge_merged_edge_min = safe_ui_value(input$burst_bridge_edge_min, 1.25),
        bridge_merged_edge_geom_min = safe_ui_value(input$burst_bridge_edge_geom, 1.30),
        max_bridge_count_per_burst = safe_int(input$burst_max_bridge_count, 3L),
        final_edge_contrast_min = safe_ui_value(input$burst_final_edge_min, 1.45),
        final_edge_contrast_geom_min = safe_ui_value(input$burst_final_edge_geom, 1.50),
        final_max_duration = safe_ui_value(input$burst_final_duration_max, 0) / f,
        final_max_n_spikes = safe_int(input$burst_final_nspikes_max, 0L),
        final_tonic_like_veto = isTRUE(input$burst_final_tonic_veto) && !identical(input$burst_final_tonic_action, "off"),
        final_tonic_like_action = input$burst_final_tonic_action %||% "demote_to_possible",
        final_tonic_like_demote_to_possible = identical(input$burst_final_tonic_action, "demote_to_possible"),
        final_tonic_like_lv_max = safe_ui_value(input$burst_final_tonic_lv, 0.35),
        final_tonic_like_cv_max = safe_ui_value(input$burst_final_tonic_cv, 0.30),
        final_tonic_like_mm_max = safe_ui_value(input$burst_final_tonic_mm, 1.20),
        final_tonic_like_min_spikes = safe_int(input$burst_final_tonic_min_spikes, 6L),
        canonical_burst_edge_multiplier = safe_ui_value(input$burst_canonical_edge_multiplier, 3.00),
        canonical_burst_context_contrast_min = safe_ui_value(input$burst_canonical_context_min, 2.50),
        canonical_burst_edge_return_min = safe_ui_value(input$burst_canonical_edge_return, 0.60),
        canonical_burst_abs_ceiling_sec = safe_ui_value(input$burst_canonical_abs_ceiling, 0) / f,
        canonical_burst_abs_ceiling_fuzzy_pct = safe_ui_value(input$burst_canonical_fuzzy_pct, 0),
        canonical_burst_allow_fuzzy_canonical = isTRUE(input$burst_canonical_allow_fuzzy),
        canonical_burst_use_T_manual = TRUE,
        canonical_burst_max_bridge_ISI_sec = safe_ui_value(input$burst_canonical_max_bridge, 0) / f,
        canonical_burst_internal_q95_q50_ratio_max = safe_ui_value(input$burst_canonical_q95_q50, 3.50),
        canonical_burst_internal_max_q50_ratio_max = safe_ui_value(input$burst_canonical_max_q50, 5.00),
        canonical_burst_internal_cv_max = safe_ui_value(input$burst_canonical_cv_max, 1.50),
        canonical_burst_internal_lv_max = safe_ui_value(input$burst_canonical_lv_max, 2.00),
        canonical_burst_edge_multiplier_tonic_prior = 3.50,
        canonical_burst_context_min_tonic_prior = 3.00,
        canonical_burst_context_min_pause_prior = 3.00,
        seed_bridge_classicity_enabled = isTRUE(input$seed_bridge_enable %||% TRUE),
        seed_bridge_burst_core_max_ISI_sec = safe_ui_value(input$seed_bridge_core_max, 10) / f,
        seed_bridge_burst_core_pct_max = safe_ui_value(input$seed_bridge_core_pct, 25),
        seed_bridge_burst_bridge_max_ISI_sec = safe_ui_value(input$seed_bridge_bridge_max, 15) / f,
        seed_bridge_burst_bridge_factor = safe_ui_value(input$seed_bridge_bridge_factor, 1.50),
        seed_bridge_burst_core_min_isi_n = safe_int(input$seed_bridge_min_core_isi_n, 2L),
        seed_bridge_burst_bridge_max_count = safe_int(input$seed_bridge_bridge_count_max, 4L),
        seed_bridge_burst_bridge_fraction_max = safe_ui_value(input$seed_bridge_bridge_fraction_max, 0.60),
        seed_bridge_burst_classicity_multiplier = safe_ui_value(input$seed_bridge_classicity, 3.00),
        seed_bridge_burst_possible_classicity_multiplier = safe_ui_value(input$seed_bridge_possible_classicity, 2.00),
        seed_bridge_context_compression_min = safe_ui_value(input$seed_bridge_context_min, 1.00),
        seed_bridge_edge_return_min = safe_ui_value(input$seed_bridge_edge_return, 0.00),
        seed_bridge_max_seed_candidates = safe_int(input$burst_seed_diag_max, 1200L),
        seed_bridge_max_bridge_candidates = safe_int(input$burst_bridge_diag_max, 1200L),
        near_miss_max_relax = safe_ui_value(input$near_miss_max_relax, 0.25),
        near_miss_max_rows = safe_int(input$near_miss_max_rows, 600L),
        preview_pad_sec = 0.150,
        use_context_proposals = isTRUE(input$burst_use_context),
        fast_context_proposals = isTRUE(input$burst_fast_context),
        use_local_compression_seed = isTRUE(input$burst_use_local_seed),
        use_boundary_optimization = isTRUE(input$burst_optimize),
        label_possible_burst = isTRUE(input$burst_label_possible),
        label_boundary_possible_burst = isTRUE(input$burst_label_boundary_possible %||% TRUE),
        refractory_suspect_sec = refractory_suspect_sec(),
        refractory_suspect_action = input$refractory_suspect_action %||% "demote_to_possible",
        merge_candidate_fragments = isTRUE(input$burst_merge_fragments),
        merge_gap_max_n = safe_int(input$burst_merge_gap_n, 2L),
        contrast_ref = input$burst_contrast_ref %||% "q",
        contrast_q = clamp(safe_ui_value(input$burst_contrast_q, 0.90), 0.50, 1.00),
        context_k = safe_int(input$burst_context_k, 5L),
        local_window = safe_int(input$burst_local_window, 11L),
        local_compression_min = safe_ui_value(input$burst_local_compression, 1.40),
        local_compression_burst_mode = isTRUE(input$burst_local_compression_mode),
        local_compression_burst_label = input$burst_local_compression_label %||% "possible_burst",
        local_compression_core_pct_max = safe_ui_value(input$burst_local_compression_pct, 30),
        local_compression_local_ratio_min = safe_ui_value(input$burst_local_compression_local_ratio, 2.20),
        local_compression_edge_min = safe_ui_value(input$burst_local_compression_edge_min, 1.80),
        local_compression_edge_geom = safe_ui_value(input$burst_local_compression_edge_geom, 2.50),
        local_compression_flank_ratio_min = safe_ui_value(input$burst_local_compression_edge_min, 1.80),
        local_compression_flank_geom_min = safe_ui_value(input$burst_local_compression_edge_geom, 2.50),
        local_compression_candidate_class = input$burst_local_compression_label %||% "possible_burst",
        local_compression_core_cv_max = 1.10,
        local_compression_max_candidates = 300L,
        local_compression_max_n_spikes = safe_int(input$burst_local_compression_max_spikes, 8L),
        local_compression_max_duration = safe_ui_value(input$burst_local_compression_max_duration, 0) / f,
        boundary_burst_mode = isTRUE(input$burst_boundary_mode),
        boundary_burst_label = input$burst_boundary_label %||% "possible_burst",
        boundary_core_pct_max = safe_ui_value(input$burst_boundary_pct, 30),
        boundary_one_flank_ratio_min = safe_ui_value(input$burst_boundary_flank_ratio, 2.50),
        boundary_local_ratio_min = safe_ui_value(input$burst_boundary_local_ratio, 2.20),
        boundary_max_n_spikes = safe_int(input$burst_boundary_max_spikes, 8L),
        boundary_max_duration = safe_ui_value(input$burst_boundary_max_duration, 0) / f,
        long_burst_enable = isTRUE(input$burst_long_enable),
        long_burst_min_spikes = safe_int(input$burst_long_min_spikes, 11L),
        long_burst_max_spikes = safe_int(input$burst_long_max_spikes, 15L),
        long_burst_min_duration = safe_ui_value(input$burst_long_min_duration, 0) / f,
        long_burst_max_duration = safe_ui_value(input$burst_long_max_duration, 0) / f,
        long_burst_edge_contrast_min = safe_ui_value(input$burst_long_edge_min, 1.45),
        long_burst_edge_contrast_geom = safe_ui_value(input$burst_long_edge_geom, 1.50),
        long_burst_core_pct_max = safe_ui_value(input$burst_long_core_pct, 35),
        long_burst_short_fraction_min = safe_ui_value(input$burst_long_short_fraction, 0.65),
        long_burst_output_class = "long_burst",
        boundary_max_candidates = 100L,
        proposal_window_min_isi = safe_int(input$burst_win_min, 2L),
        proposal_window_max_isi = safe_int(input$burst_win_max, 8L),
        proposal_contrast_min = safe_ui_value(input$burst_prop_cmin, 1.20),
        proposal_contrast_geom_min = safe_ui_value(input$burst_prop_cgeom, 1.30),
        contrast_min_high = safe_ui_value(input$burst_cmin_high, 1.80),
        contrast_geom_high = safe_ui_value(input$burst_cgeom_high, 1.80),
        contrast_min_possible = safe_ui_value(input$burst_cmin_possible, 1.25),
        contrast_geom_possible = safe_ui_value(input$burst_cgeom_possible, 1.35),
        contrast_min_flanks = safe_int(input$burst_flanks, 2L),
        optimize_radius = safe_int(input$burst_opt_radius, 1L),
        score_high = safe_ui_value(input$burst_score_high, 0.65),
        score_possible = safe_ui_value(input$burst_score_possible, 0.35),
        mm_penalty_start = 2.50,
        lv_penalty_start = 1.50,
        max_candidates_per_train = safe_int(input$burst_max_candidates, 600L),
        max_optimize_candidates_per_train = safe_int(input$burst_max_opt_candidates, 200L),
        stitch_short_burst_gaps = isTRUE(input$burst_merge_fragments),
        stitch_gap_max_n = safe_int(input$burst_merge_gap_n, 2L),
        stitch_gap_local_frac = 0.85,
        promote_mixed_burst_family = TRUE
      ),
      tonic = list(
        seed_ratio = safe_ui_value(input$tonic_seed_ratio, 1.20),
        T_min = safe_ui_value(input$tonic_T_min, 20) / f,
        T_max = safe_ui_value(input$tonic_T_max, 60) / f,
        LV_core = safe_ui_value(input$tonic_LV_core, 0.5),
        LV_pre = safe_ui_value(input$tonic_LV_pre, 0.5),
        LV_post = safe_ui_value(input$tonic_LV_post, 0.5),
        local_ratio_min = safe_ui_value(input$tonic_local_min, 0.7),
        local_ratio_max = safe_ui_value(input$tonic_local_max, 1.3),
        tonic_mm_max = safe_ui_value(input$tonic_mm_max, 1.25),
        tonic_mm_min = safe_ui_value(input$tonic_mm_min, 0.85),
        G_min = safe_int(input$tonic_Gmin, 5L),
        D_min = safe_ui_value(input$tonic_Dmin, 0) / f,
        connector_budget_frac = safe_ui_value(input$tonic_connector_budget, 0.15),
        connector_max_n = safe_int(input$tonic_connector_n, 1L),
        lv_delta = safe_ui_value(input$tonic_lv_delta, 0.10),
        anti_burst_veto = isTRUE(input$tonic_anti_burst_veto),
        adaptive_use_train_ranges = isTRUE(input$tonic_use_saved_ranges),
        adaptive_train_ranges_hard = isTRUE(input$tonic_ranges_hard),
        adaptive_range_mode = "percentile_or_absolute",
        adaptive_train_ranges = (get_dataset()$train_settings$tonic_isi_ranges %||% list())
      ),
      highfreq = list(
        enable = any(c("high_frequency_tonic", "high_frequency_spiking") %in% patterns_to_run_ui),
        T_high_max = safe_ui_value(input$hf_T_high_max, 20) / f,
        ISI_abs_max = safe_ui_value(input$hf_T_high_max, 20) / f,
        pct_max = safe_ui_value(input$hf_pct_max, 30),
        ISI_pct_max = safe_ui_value(input$hf_pct_max, 30),
        min_isi_n = safe_int(input$hf_min_isi_n, 5L),
        G_min = safe_int(input$hf_min_isi_n, 5L) + 1L,
        D_min = 0,
        connector_max_n = 1L,
        short_fraction_min = safe_ui_value(input$hf_short_fraction, 0.80),
        stable_CV_max = safe_ui_value(input$hf_stable_cv, 0.30),
        CV_stable_max = safe_ui_value(input$hf_stable_cv, 0.30),
        stable_LV_max = safe_ui_value(input$hf_stable_lv, 0.35),
        LV_stable_max = safe_ui_value(input$hf_stable_lv, 0.35),
        stable_MM_max = safe_ui_value(input$hf_stable_mm, 1.25),
        MM_stable_max = safe_ui_value(input$hf_stable_mm, 1.25),
        tonic_min_ISI_floor_sec = safe_ui_value(input$hf_tonic_min_floor, 10) / f,
        tonic_low_tail_fraction_max = safe_ui_value(input$hf_tonic_low_tail, 0.05),
        tonic_burst_core_veto = isTRUE(input$hf_tonic_burst_core_veto %||% TRUE),
        tonic_burst_core_veto_min_isi_n = 2L,
        irregular_CV_min = safe_ui_value(input$hf_irregular_cv, 0.35),
        irregular_LV_min = safe_ui_value(input$hf_irregular_lv, 0.50),
        irregular_MM_min = safe_ui_value(input$hf_irregular_mm, 1.50),
        spiking_min_spikes = safe_int(input$hf_spiking_min_spikes, 30L),
        spiking_min_duration = safe_ui_value(input$hf_spiking_min_duration, 0) / f,
        spiking_use_abs_max = isTRUE(input$hf_spiking_use_abs),
        spiking_max_ISI_abs = safe_ui_value(input$hf_spiking_abs_max, 20) / f,
        spiking_use_pct_max = isTRUE(input$hf_spiking_use_pct),
        spiking_max_ISI_pct = safe_ui_value(input$hf_spiking_pct_max, 30),
        spiking_gate_logic = input$hf_spiking_gate_logic %||% "either",
        spiking_short_fraction_min = safe_ui_value(input$hf_spiking_short_fraction, 0.70),
        spiking_epoch_bridge_ISI_sec = safe_ui_value(input$hf_spiking_epoch_bridge, 30) / f,
        spiking_q90_max_ISI_sec = safe_ui_value(input$hf_spiking_q90_max, 20) / f,
        spiking_allowed_large_isi_fraction = safe_ui_value(input$hf_spiking_large_fraction, 0.25),
        spiking_max_consecutive_large_isi = safe_int(input$hf_spiking_consecutive_large, 3L),
        spiking_tolerated_gap_ISI_sec = safe_ui_value(input$hf_spiking_tolerated_gap, 75) / f,
        adaptive_use_train_ranges = isTRUE(input$highfreq_use_saved_ranges %||% TRUE),
        adaptive_train_ranges_hard = isTRUE(input$highfreq_ranges_hard %||% FALSE),
        adaptive_range_mode = "percentile_or_absolute",
        adaptive_train_ranges = (get_dataset()$train_settings$highfreq_isi_ranges %||% list())
      ),
      pause = list(
        T_strong = safe_ui_value(input$pause_T_strong, 150) / f,
        T_seed = safe_ui_value(input$pause_T_seed, 100) / f,
        D_min = safe_ui_value(input$pause_Dmin, 0) / f,
        G_min = safe_int(input$pause_Gmin, 2L),
        alpha = safe_ui_value(input$pause_alpha, 2.2),
        beta = safe_ui_value(input$pause_beta, 0.8),
        context_relax = safe_ui_value(input$pause_ctx_relax, 0.9),
        context_tight = safe_ui_value(input$pause_ctx_tight, 1.1),
        exclude_occupied_context = isTRUE(input$pause_exclude_occupied_context),
        global_median_guard = isTRUE(input$pause_global_median_guard),
        global_median_factor = safe_ui_value(input$pause_global_median_factor, 2.5),
        anti_tonic_veto = isTRUE(input$pause_anti_tonic_veto),
        adaptive_use_train_ranges = isTRUE(input$pause_use_saved_ranges),
        adaptive_train_ranges_hard = isTRUE(input$pause_ranges_hard),
        adaptive_range_mode = "percentile_or_absolute",
        adaptive_train_ranges = (get_dataset()$train_settings$pause_isi_ranges %||% list())
      ),
      event_core = list(
        enabled = isTRUE(input$dataset_isi_enable %||% TRUE),
        dataset_seed_band_enabled = isTRUE(input$dataset_isi_enable %||% TRUE),
        dataset_isi_burst_enabled = isTRUE(input$dataset_isi_enable %||% TRUE),
        use_manual_isi_calibration = TRUE,
        manual_can_expand_seed_band = TRUE,
        manual_can_expand_bridge_band = TRUE,
        seed_band_lower_sec = safe_ui_value(input$event_core_seed_lower, 1) / f,
        seed_band_upper_sec = safe_ui_value(input$event_core_seed_upper, 10) / f,
        bridge_band_upper_sec = safe_ui_value(input$event_core_bridge_upper, 15) / f,
        boundary_floor_sec = safe_ui_value(input$event_core_boundary_floor, 0) / f,
        boundary_floor_hard = FALSE,
        burst_contrast_min = safe_ui_value(input$event_core_classicity, 2.50),
        possible_burst_contrast_min = safe_ui_value(input$event_core_possible_classicity, 2.00),
        min_seed_isi_count = safe_int(input$event_core_min_seed_isi_n, 2L),
        max_bridge_isi_count = safe_int(input$event_core_bridge_count_max, 4L),
        max_bridge_isi_fraction = safe_ui_value(input$event_core_bridge_fraction_max, 0.60),
        max_expansion_isi_each_side = safe_int(input$event_core_expand_each_side, 4L),
        max_candidates_per_train = safe_int(input$event_core_max_candidates, 3000L),
        histogram_bin_width_sec = safe_ui_value(input$event_core_hist_bin, 5) / f,
        prolonged_min_spikes = 16L,
        prolonged_max_spikes = 29L
      ),
      event_grammar = list(
        enabled = isTRUE(input$dataset_isi_enable %||% TRUE),
        threshold_source_mode = input$event_grammar_threshold_source_mode %||% "auto",
        histogram_bin_width_sec = safe_ui_value(input$dataset_isi_hist_bin, safe_ui_value(input$event_core_hist_bin, 5)) / f,
        show_patterns = input$dataset_isi_hist_patterns %||% character(0),
        show_sources = input$dataset_isi_hist_sources %||% character(0),
        allow_one_sided_burst_as_canonical = isTRUE(input$event_grammar_allow_one_sided_canonical %||% FALSE),
        one_sided_burst_contrast_min = safe_ui_value(input$event_grammar_user_one_sided_S, safe_ui_value(input$event_core_classicity, 2.50) + 0.5),
        one_sided_seed_purity_min = safe_ui_value(input$event_grammar_one_sided_seed_purity_min, 0.65),
        strict_q95_bridge_gate = isTRUE(input$event_grammar_strict_q95_bridge_gate %||% FALSE),
        q95_soft_penalty_weight = 0.35,
        dynamic_possible_priority = isTRUE(input$event_grammar_dynamic_possible_priority %||% TRUE),
        user = list(
          burst = list(enable = isTRUE(input$event_grammar_user_burst_enable),
                       seed_lower_sec = safe_ui_value(input$event_grammar_user_burst_seed_lower, 1) / f,
                       seed_upper_sec = safe_ui_value(input$event_grammar_user_burst_seed_upper, 10) / f,
                       bridge_upper_sec = safe_ui_value(input$event_grammar_user_burst_bridge, 15) / f,
                       contrast_S = safe_ui_value(input$event_grammar_user_burst_S, 2.50),
                       one_sided_contrast_S = safe_ui_value(input$event_grammar_user_one_sided_S, 3.00)),
          high_frequency_spiking = list(enable = isTRUE(input$event_grammar_user_hfs_enable),
                       seed_lower_sec = safe_ui_value(input$event_grammar_user_hfs_seed_lower, 1) / f,
                       seed_upper_sec = safe_ui_value(input$event_grammar_user_hfs_seed_upper, 20) / f,
                       bridge_upper_sec = safe_ui_value(input$event_grammar_user_hfs_bridge, 30) / f),
          high_frequency_tonic = list(enable = isTRUE(input$event_grammar_user_hft_enable),
                       seed_lower_sec = safe_ui_value(input$event_grammar_user_hft_seed_lower, 10) / f,
                       seed_upper_sec = safe_ui_value(input$event_grammar_user_hft_seed_upper, 30) / f,
                       bridge_upper_sec = safe_ui_value(input$event_grammar_user_hft_bridge, 35) / f),
          tonic = list(enable = isTRUE(input$event_grammar_user_tonic_enable),
                       seed_lower_sec = safe_ui_value(input$event_grammar_user_tonic_seed_lower, 20) / f,
                       seed_upper_sec = safe_ui_value(input$event_grammar_user_tonic_seed_upper, 60) / f,
                       bridge_upper_sec = safe_ui_value(input$event_grammar_user_tonic_bridge, 80) / f),
          pause = list(enable = isTRUE(input$event_grammar_user_pause_enable),
                       seed_lower_sec = safe_ui_value(input$event_grammar_user_pause_seed_lower, 100) / f,
                       seed_upper_sec = safe_ui_value(input$event_grammar_user_pause_seed_upper, 150) / f,
                       bridge_upper_sec = safe_ui_value(input$event_grammar_user_pause_bridge, 150) / f)
        )
      ),
      arbitration = list(
        enabled = isTRUE(input$event_arbitration %||% TRUE)
      ),
      detector = list(
        min_valid_isi_sec = min_valid_isi_sec(),
        refractory_suspect_sec = refractory_suspect_sec(),
        refractory_suspect_action = input$refractory_suspect_action %||% "demote_to_possible",
        fill_others_auto = isTRUE(input$fill_others_auto),
        patterns_to_run = patterns_to_run_ui,
        pattern_isi_limits = read_pattern_isi_limits(),
        logisi_mcv_sec = logisi_mcv_sec(),
        plot_lod_mode = input$plot_lod_mode %||% "auto",
        plot_max_visible_spikes_full = safe_int(input$plot_max_visible_spikes_full, 50000L),
        plot_max_visible_spikes_interactive = safe_int(input$plot_max_visible_spikes_interactive, 100000L),
        analysis_role = "candidate_event_generator_plus_review",
        manual_negative_labels_enabled = isTRUE(input$manual_negative_labels %||% TRUE),
        preset_name = input$analysis_preset %||% "balanced_single_unit",
        require_human_or_model_review_for_publication = TRUE
      )
    )
    p <- schema_params_from_input(p, input, prefix = "schema_param_")
    p <- schema_params_from_input(p, input, schema = stpd_contract_ui_schema(), prefix = "contract_param_", exclude_paths = character())
    ds_tmp <- tryCatch(get_dataset(), error = function(e) NULL)
    if (!is.null(ds_tmp) && !is.null(ds_tmp$train_settings$isi_thresholds)) {
      p <- merge_train_isi_thresholds_into_params(p, ds_tmp$train_settings$isi_thresholds)
    }
    if (exists("stpd_attach_thresholds_to_params", mode = "function")) {
      p <- tryCatch(stpd_attach_thresholds_to_params(p, ds_tmp, min_isi_sec = p$detector$min_valid_isi_sec %||% min_valid_isi_sec(), bin_width_sec = p$event_grammar$histogram_bin_width_sec %||% 0.005),
                    error = function(e) p)
    }
    if (exists("stpd_productize_params", mode = "function")) {
      p <- tryCatch(stpd_productize_params(p, prefer = "legacy"), error = function(e) p)
    }
	    p
	  }

	  parameter_validation_bundle <- reactive({
	    input$validate_params_now
	    p <- read_params_from_ui()
	    issues <- tryCatch(stpd_validate_params(p), error = function(e) {
	      data.frame(severity = "error", path = "parameter_validation", issue = e$message, stringsAsFactors = FALSE)
	    })
	    list(params = p, issues = issues, summary = stpd_parameter_validation_summary(p, issues = issues))
	  })

	  contract_ui_level <- reactive({
	    level <- as.character(input$contract_ui_level %||% "basic")[1]
	    if (!level %in% c("basic", "advanced", "expert", "all")) "basic" else level
	  })

	  output$contract_parameter_controls <- renderUI({
	    stpd_contract_ui_controls(prefix = "contract_param_", ui_level = contract_ui_level())
	  })

	  output$parameter_validation_summary <- renderText({
	    b <- parameter_validation_bundle()
	    issues <- b$issues
	    errors <- sum(issues$severity == "error", na.rm = TRUE)
	    warnings <- sum(issues$severity == "warning", na.rm = TRUE)
	    infos <- sum(issues$severity == "info", na.rm = TRUE)
	    visible <- stpd_parameter_issue_table(issues, ui_level = contract_ui_level())
	    visible_warnings <- sum(visible$severity == "warning", na.rm = TRUE)
	    visible_infos <- sum(visible$severity == "info", na.rm = TRUE)
	    hash <- tryCatch(stpd_params_hash(b$params), error = function(e) stpd_params_hash_flat(b$params))
	    import_txt <- ""
	    if (!is.null(rv$last_param_yaml_import)) {
	      imp <- rv$last_param_yaml_import
	      imp_err <- if (!is.null(imp$validation) && nrow(imp$validation) > 0) sum(imp$validation$severity == "error", na.rm = TRUE) else 0L
	      import_txt <- paste0("\nLast YAML import: ", imp$status %||% "", " | ", imp$name %||% "", " | ", imp$params_hash %||% "", " | errors=", imp_err)
	    }
	    paste0(
	      "Current params_hash: ", hash, "\n",
	      "Validation total: ", errors, " error(s), ", warnings, " warning(s), ", infos, " info item(s).\n",
	      "Visible at ", contract_ui_level(), ": errors always shown; ", visible_warnings, " warning(s), ", visible_infos, " info item(s).",
	      import_txt
	    )
	  })

		  output$parameter_validation_table <- renderDT({
		    issues <- parameter_validation_bundle()$issues
	    if (!is.null(rv$last_param_yaml_import) &&
	        identical(rv$last_param_yaml_import$status %||% "", "rejected") &&
	        !is.null(rv$last_param_yaml_import$validation) &&
	        nrow(rv$last_param_yaml_import$validation) > 0) {
	      imp <- rv$last_param_yaml_import$validation
	      imp$source <- "last_yaml_import"
	      issues$source <- "current_ui"
	      issues <- rbind(imp[, names(issues), drop = FALSE], issues)
	    }
	    if (is.null(issues) || nrow(issues) == 0) {
	      issues <- data.frame(severity = "ok", path = "", issue = "No parameter contract issues.", stringsAsFactors = FALSE)
	    }
	    issues <- stpd_parameter_issue_table(issues, ui_level = contract_ui_level())
	    if (nrow(issues) == 0) {
	      issues <- data.frame(severity = "ok", path = "", issue = paste0("No visible parameter contract issues at level: ", contract_ui_level()), ui_level = contract_ui_level(), section = "", stringsAsFactors = FALSE)
	    }
		    datatable(issues, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
		  })
	
		  output$parameter_change_preview_table <- renderDT({
		    dat <- tryCatch(
		      stpd_parameter_change_preview(parameter_validation_bundle()$params),
		      error = function(e) data.frame(message = paste0("Parameter change preview failed: ", e$message), stringsAsFactors = FALSE)
		    )
		    datatable(dat, options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
		  })
	
		  output$parameter_delta_preview_train_selector <- renderUI({
		    td <- tryCatch(current_trains(), error = function(e) list())
		    choices <- names(td)
		    if (length(choices) == 0) return(tags$div(class = "small-note", "\u8BF7\u5148\u52A0\u8F7D\u6570\u636E\u96C6\u3002"))
		    selected <- tryCatch(intersect(displayed_train_names() %||% head(choices, 1), choices), error = function(e) head(choices, 1))
		    if (length(selected) == 0) selected <- head(choices, 1)
		    selectizeInput(
		      "parameter_delta_preview_trains",
		      "\u9884\u89C8 train(s)",
		      choices = choices,
		      selected = head(selected, safe_int(input$parameter_delta_preview_max_trains %||% 3L, 3L)),
		      multiple = TRUE,
		      options = list(maxItems = 20, placeholder = "\u9009\u62E9\u5C11\u91CF train \u505A dry-run \u5DEE\u5F02\u9884\u89C8")
		    )
		  })
	
		  observeEvent(input$run_parameter_delta_preview, {
		    ds <- current_dataset()
		    p_current <- read_params_from_ui()
		    issues <- stpd_validate_params(p_current)
		    if (any(issues$severity == "error", na.rm = TRUE)) {
		      bad <- issues[issues$severity == "error", , drop = FALSE]
		      showNotification(paste0("\u5F53\u524D\u53C2\u6570\u6709 contract error\uFF0C\u5DF2\u963B\u6B62\u5DEE\u5F02\u9884\u89C8\uFF1A", paste(head(paste(bad$path, bad$issue, sep = " - "), 3), collapse = "; ")), type = "error", duration = 10)
		      return()
		    }
		    p_baseline <- if (identical(input$parameter_delta_preview_baseline %||% "default", "last_run") && !is.null(ds$params_last)) ds$params_last else default_params_sec()
		    target <- intersect(input$parameter_delta_preview_trains %||% character(0), names(ds$trains))
		    if (length(target) == 0) target <- head(names(ds$trains), safe_int(input$parameter_delta_preview_max_trains %||% 3L, 3L))
		    max_tr <- safe_int(input$parameter_delta_preview_max_trains %||% 3L, 3L)
		    iou_min <- suppressWarnings(as.numeric(input$parameter_delta_preview_iou %||% 0.25))
		    tryCatch({
		      withProgress(message = "\u6B63\u5728\u8FD0\u884C\u5C40\u90E8 dry-run \u5DEE\u5F02\u9884\u89C8", value = 0.1, {
		        preview <- stpd_parameter_delta_preview(
		          ds,
		          params_current = p_current,
		          params_baseline = p_baseline,
		          selected_trains = target,
		          max_trains = max_tr,
		          iou_min = iou_min,
		          source = "auto",
		          lock_manual = TRUE,
		          collect_diagnostics = FALSE
		        )
		        incProgress(0.9, detail = "\u5DF2\u5B8C\u6210 baseline/current AUTO \u4E8B\u4EF6 IoU \u5BF9\u6BD4")
		        rv$parameter_delta_preview <- preview
		        changed <- preview$summary$value[preview$summary$metric == "changed_event_n"][1] %||% "0"
		        rv$parameter_delta_preview_status <- paste0(
		          "Dry-run \u9884\u89C8\u5B8C\u6210\uFF1A", length(preview$selected_trains), " train(s), AUTO source, IoU>=",
		          preview$iou_min, ", changed_event_n=", changed,
		          "\u3002\u6B63\u5F0F ds$results \u672A\u88AB\u8986\u76D6\u3002"
		        )
		      })
		      showNotification(rv$parameter_delta_preview_status, type = "message", duration = 8)
		    }, error = function(e) {
		      rv$parameter_delta_preview_status <- paste0("\u5C40\u90E8\u5DEE\u5F02\u9884\u89C8\u5931\u8D25\uFF1A", e$message)
		      rv$parameter_delta_preview <- NULL
		      showNotification(rv$parameter_delta_preview_status, type = "error", duration = 10)
		    })
		  }, ignoreNULL = TRUE)
	
		  output$parameter_delta_preview_status <- renderText({
		    rv$parameter_delta_preview_status %||% "\u5C1A\u672A\u8FD0\u884C\u5C40\u90E8\u5DEE\u5F02\u91CD\u8DD1\u9884\u89C8\u3002"
		  })
	
		  output$parameter_delta_preview_summary_table <- renderDT({
		    preview <- rv$parameter_delta_preview
		    dat <- if (is.null(preview)) data.frame(message = "\u70B9\u51FB\u201C\u8FD0\u884C\u5C40\u90E8\u5DEE\u5F02\u9884\u89C8\u201D\u540E\u663E\u793A\u6C47\u603B\u3002", stringsAsFactors = FALSE) else preview$summary
		    datatable(dat, options = list(pageLength = 12, scrollX = TRUE, dom = "tip"), rownames = FALSE)
		  })
	
		  output$parameter_delta_preview_counts_table <- renderDT({
		    preview <- rv$parameter_delta_preview
		    dat <- if (is.null(preview) || is.null(preview$counts) || nrow(preview$counts) == 0) {
		      data.frame(message = "\u6682\u65E0 pattern \u6570\u91CF\u5DEE\u5F02\u3002", stringsAsFactors = FALSE)
		    } else preview$counts
		    datatable(dat, options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
		  })
	
		  output$parameter_delta_preview_events_table <- renderDT({
		    preview <- rv$parameter_delta_preview
		    dat <- if (is.null(preview) || is.null(preview$event_diff) || nrow(preview$event_diff) == 0) {
		      data.frame(message = "\u672A\u53D1\u73B0\u65B0\u589E\u3001\u6D88\u5931\u3001\u6807\u7B7E\u53D8\u5316\u6216\u8FB9\u754C\u53D8\u5316\u4E8B\u4EF6\u3002", stringsAsFactors = FALSE)
		    } else preview$event_diff
		    datatable(dat, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE, selection = "single")
		  })

		  jump_to_parameter_delta_row <- function(row) {
		    if (is.null(row) || nrow(row) == 0) return(invisible(FALSE))
		    ds <- get_dataset()
		    tr <- as.character(row$train[1] %||% "")
		    if (is.null(ds) || !nzchar(tr) || !(tr %in% names(ds$trains))) return(invisible(FALSE))
		    dat <- ds$trains[[tr]]
		    starts <- suppressWarnings(as.integer(c(row$baseline_start_isi[1], row$current_start_isi[1])))
		    ends <- suppressWarnings(as.integer(c(row$baseline_end_isi[1], row$current_end_isi[1])))
		    starts <- starts[is.finite(starts)]
		    ends <- ends[is.finite(ends)]
		    if (length(starts) == 0 || length(ends) == 0) return(invisible(FALSE))
		    s_isi <- max(1L, min(starts, na.rm = TRUE))
		    e_isi <- max(ends, na.rm = TRUE)
		    sidx <- max(1L, s_isi - 1L)
		    eidx <- min(nrow(dat), e_isi)
		    if (!is.finite(sidx) || !is.finite(eidx) || sidx < 1 || eidx > nrow(dat) || eidx < sidx) return(invisible(FALSE))
		    t0 <- dat$timestamp_sec[sidx] - dat$timestamp_sec[1]
		    t1 <- dat$timestamp_sec[eidx] - dat$timestamp_sec[1]
		    pad <- max(0.025, (t1 - t0) * 3, (current_param_for_tables()$burst$preview_pad_sec %||% 0.150))
		    f <- unit_factor()
		    rv$view_align_x <- c(max(0, t0 - pad), t1 + pad) * f
		    updateSliderInput(session, "xrange", value = rv$view_align_x)
		    if (identical(input$train_display_mode %||% "paged_all", "selected_only")) {
		      if (is.null(input$trains) || !(tr %in% input$trains)) {
		        updateSelectizeInput(session, "trains", selected = unique(c(tr, input$trains %||% character(0))))
		      }
		    } else {
		      td_now <- names(current_trains())
		      pos <- match(tr, td_now)
		      if (is.finite(pos)) {
		        per <- safe_int(input$visible_trains_per_page, 10L)
		        updateNumericInput(session, "train_page", value = max(1L, ceiling(pos / max(1L, per))))
		      }
		    }
		      updateTabsetPanel(session, "main_tabs", selected = "\u5BF9\u9F50\u65F6\u95F4\u6233\u56FE")
		    invisible(TRUE)
		  }

		  observeEvent(input$parameter_delta_preview_events_table_rows_selected, {
		    sel <- input$parameter_delta_preview_events_table_rows_selected
		    preview <- rv$parameter_delta_preview
		    if (is.null(preview) || is.null(preview$event_diff) || length(sel) != 1 || nrow(preview$event_diff) < sel) return()
		    rv$parameter_delta_preview_selected_row <- sel
		    ok <- jump_to_parameter_delta_row(preview$event_diff[sel, , drop = FALSE])
		    if (isTRUE(ok)) {
		      showNotification("\u5DF2\u8DF3\u8F6C\u5230\u9009\u4E2D\u53C2\u6570\u5DEE\u5F02\u4E8B\u4EF6\u7684 raster \u65F6\u95F4\u7A97\u3002", type = "message", duration = 5)
		    }
		  }, ignoreNULL = TRUE)

		  output$download_parameter_delta_preview_zip <- downloadHandler(
		    filename = function() paste0("Parameter_delta_preview_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip"),
		    content = function(file) {
		      preview <- rv$parameter_delta_preview
		      if (is.null(preview)) stop("\u5C1A\u65E0\u53C2\u6570\u5DEE\u5F02\u9884\u89C8\u53EF\u5BFC\u51FA\u3002\u8BF7\u5148\u8FD0\u884C\u5C40\u90E8\u5DEE\u5F02\u9884\u89C8\u3002", call. = FALSE)
		      out_dir <- file.path(tempdir(), paste0("parameter_delta_preview_", format(Sys.time(), "%Y%m%d_%H%M%S")))
		      stpd_parameter_delta_export(preview, out_dir)
		      old <- setwd(out_dir)
		      on.exit(setwd(old), add = TRUE)
		      utils::zip(zipfile = file, files = list.files(out_dir))
		    }
		  )
	
		  output$parameter_roundtrip_report_table <- renderDT({
		    p <- parameter_validation_bundle()$params
	    dat <- tryCatch(stpd_parameter_yaml_roundtrip_report(p, source = "shiny_ui_probe"),
	                    error = function(e) data.frame(check = "yaml_roundtrip", status = "error", detail = e$message, stringsAsFactors = FALSE))
	    datatable(dat, options = list(pageLength = 5, scrollX = TRUE), rownames = FALSE)
	  })

	  observeEvent(input$params_yaml_in, {
	    req(input$params_yaml_in)
	    tryCatch({
	      imported <- stpd_read_params_yaml(input$params_yaml_in$datapath, strict = FALSE)
	      err_n <- sum(imported$validation$severity == "error", na.rm = TRUE)
	      if (err_n > 0) {
	        rv$last_param_yaml_import <- list(
	          status = "rejected",
	          name = input$params_yaml_in$name,
	          params_hash = imported$params_hash,
	          validation = imported$validation
	        )
	        showNotification(paste0("\u53C2\u6570 YAML \u5BFC\u5165\u88AB\u62D2\u7EDD\uFF1A", err_n, " \u4E2A contract error\u3002\u8BF7\u67E5\u770B\u9A8C\u8BC1\u9762\u677F\u3002"), type = "error", duration = 8)
	        return()
	      }
	      apply_params_to_ui(imported$params, preserve_pattern_isi_limits = FALSE)
	      rv$last_param_yaml_import <- list(
	        status = "loaded",
	        name = input$params_yaml_in$name,
	        params_hash = imported$params_hash,
	        validation = imported$validation
	      )
	      showNotification(paste0("\u5DF2\u5BFC\u5165\u53C2\u6570 YAML \u5E76\u56DE\u586B UI\uFF1A", input$params_yaml_in$name, "\u3002"), type = "message", duration = 6)
	    }, error = function(e) {
	      rv$last_param_yaml_import <- list(status = "error", name = input$params_yaml_in$name %||% "", params_hash = "", validation = data.frame())
	      showNotification(paste0("\u53C2\u6570 YAML \u5BFC\u5165\u5931\u8D25\uFF1A", e$message), type = "error", duration = 10)
	    })
	  }, ignoreNULL = TRUE)

	  output$params_yaml_out <- downloadHandler(
	    filename = function() paste0("spiketrainpattern_params_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".yml"),
	    content = function(file) {
	      stpd_write_params_yaml(read_params_from_ui(), file, source = "shiny_ui")
	    }
	  )
	  
	  detector_event_counts <- function(ds, target_trains = NULL) {
    ev <- if (!is.null(ds) && !is.null(ds$results)) ds$results$events else NULL
    pats <- c("burst", "long_burst", "possible_burst", "tonic", "high_frequency_tonic", "high_frequency_spiking", "pause", "others")
    out <- setNames(rep(0L, length(pats)), pats)
    if (!is.null(ev) && nrow(ev) > 0 && "pattern" %in% names(ev)) {
      if (!is.null(target_trains) && "train" %in% names(ev)) ev <- ev[as.character(ev$train) %in% as.character(target_trains), , drop = FALSE]
      tt <- table(as.character(ev$pattern))
      for (nm in intersect(names(tt), pats)) out[nm] <- as.integer(tt[[nm]])
    }
    out
  }

  format_detector_before_after <- function(before, after, scope_txt = "") {
    pats <- union(names(before), names(after))
    before <- before[pats]; after <- after[pats]
    before[is.na(before)] <- 0L; after[is.na(after)] <- 0L
    lines <- c(
      paste0("Last detector run: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
      if (nzchar(scope_txt)) paste0("Scope: ", scope_txt) else NULL,
      "Event count change:"
    )
    for (pat in pats) lines <- c(lines, sprintf("  %-15s %6d -> %6d  (%+d)", pat, before[pat], after[pat], after[pat] - before[pat]))
    paste(lines, collapse = "\n")
  }

  stpd_server_install_detection_module(environment())

  # ----------------------------------------------------------
  # Histogram data
  # ----------------------------------------------------------
  current_param_for_tables <- reactive({
    ds <- get_dataset()
    if (is.null(ds)) return(default_params_sec())
    p <- ds$params_last %||% ds$params_est %||% default_params_sec()
    if (exists("effective_params_for_detector", mode = "function")) {
      p <- tryCatch(effective_params_for_detector(p), error = function(e) p)
    }
    p
  })
  
  
  near_miss_bundle <- reactive({
    ds <- current_dataset()
    p <- current_param_for_tables()
    nm <- ds$results$near_miss_candidates
    # Workspaces saved before seed-bridge may not contain the cache; rebuild it from the current diagnostic tables.
    if (is.null(nm) || nrow(nm) == 0) {
      target_trains <- intersect(displayed_train_names() %||% names(ds$trains), names(ds$trains))
      if (length(target_trains) == 0) target_trains <- names(ds$trains)
      nm <- build_near_miss_table(ds, p, min_isi_sec = p$detector$min_valid_isi_sec %||% min_valid_isi_sec(), target_trains = target_trains)
    }
    nm
  })
  
	  near_miss_filtered <- reactive({
	    nm <- near_miss_bundle()
	    if (nrow(nm) == 0) return(nm)
    pat <- input$near_miss_pattern %||% "all"
    cat <- input$near_miss_category %||% "all"
    par <- input$near_miss_parameter %||% "all"
    max_rel <- safe_ui_value(input$near_miss_filter_relax, 0.25)
    out <- nm
    if (!identical(pat, "all")) out <- out %>% filter(pattern == pat)
    if (!identical(cat, "all")) out <- out %>% filter(category == cat)
    if (!identical(par, "all")) out <- out %>% filter(parameter == par)
    out <- out %>% filter(is.finite(relative_change), relative_change <= max_rel)
    sort_mode <- input$near_miss_sort %||% "default"
    if (sort_mode == "relax") {
      out <- out %>% arrange(relative_change, failure_count, desc(score), train, start_isi)
    } else if (sort_mode == "score") {
      out <- out %>% arrange(desc(score), relative_change, failure_count, train, start_isi)
    } else if (sort_mode == "time") {
      out <- out %>% arrange(train, start_isi, relative_change)
    } else {
      out <- out %>% arrange(failure_count, relative_change, desc(score), train, start_isi)
    }
	    out$nm_id <- seq_len(nrow(out))
	    out
	  })

	  near_miss_status_message <- function(nm_all, nm_filtered = NULL) {
	    n_all <- if (is.null(nm_all)) 0L else nrow(nm_all)
	    n_filtered <- if (is.null(nm_filtered)) 0L else nrow(nm_filtered)
	    if (n_all == 0L) {
	      return(paste0(
	        "No near-miss candidates have been generated. Run the detector first. ",
	        "If this remains empty, the current data/parameters have no candidates close to a threshold boundary, ",
	        "or the near-miss generation limits are too narrow."
	      ))
	    }
	    if (n_filtered == 0L) {
	      pat <- input$near_miss_pattern %||% "all"
	      cat <- input$near_miss_category %||% "all"
	      par <- input$near_miss_parameter %||% "all"
	      max_rel <- safe_ui_value(input$near_miss_filter_relax, 0.25)
	      return(paste0(
	        "Generated ", n_all, " near-miss candidates, but 0 match the current filters. Filters: pattern=", pat,
	        ", category=", cat, ", parameter=", par, ", relative_change <= ", signif(max_rel, 4),
	        ". Set pattern/category/parameter to all, or increase the allowed relative adjustment."
	      ))
	    }
	    paste0("Near-miss candidates: ", n_filtered, " filtered / ", n_all, " total.")
	  }

  enrich_near_miss_for_display <- function(nm, ds) {
    if (is.null(nm) || nrow(nm) == 0) return(nm)
    out <- as.data.frame(nm)
    n <- nrow(out)
    na_num <- rep(NA_real_, n)
    na_int <- rep(NA_integer_, n)
    out$start_timestamp_sec_display <- na_num
    out$end_timestamp_sec_display <- na_num
    out$start_aligned_sec_display <- na_num
    out$end_aligned_sec_display <- na_num
    out$duration_sec_display <- na_num
    out$n_spikes_display <- na_int
    out$n_isi_display <- na_int
    out$n_valid_isi_display <- na_int
    out$mean_rate_hz_display <- na_num
    out$mean_ISI_sec_display <- na_num
    out$median_ISI_sec_display <- na_num
    out$q10_ISI_sec_display <- na_num
    out$q90_ISI_sec_display <- na_num
    out$min_ISI_sec_display <- na_num
    out$max_ISI_sec_display <- na_num
    out$CV_display <- na_num
    out$CV2_display <- na_num
    out$LV_display <- na_num
    out$MM_display <- na_num
    out$ISI_index_range <- ""

    min_isi <- tryCatch(min_valid_isi_sec(), error = function(e) 0.001)
    calc_cv2_local <- function(x) {
      x <- finite_num(x)
      if (length(x) < 2) return(NA_real_)
      a <- head(x, -1)
      b <- tail(x, -1)
      denom <- a + b
      ok <- is.finite(denom) & denom > 0
      if (!any(ok)) return(NA_real_)
      mean(2 * abs(a[ok] - b[ok]) / denom[ok], na.rm = TRUE)
    }

    for (ii in seq_len(n)) {
      tr <- as.character(out$train[ii] %||% "")
      dat <- if (!is.null(ds) && !is.null(ds$trains) && tr %in% names(ds$trains)) ds$trains[[tr]] else NULL
      s <- suppressWarnings(as.integer(out$start_isi[ii] %||% NA_integer_))
      e <- suppressWarnings(as.integer(out$end_isi[ii] %||% NA_integer_))
      if (is.finite(s) && is.finite(e)) out$ISI_index_range[ii] <- paste0(s, "-", e)
      if (is.null(dat) || nrow(dat) == 0 || !("timestamp_sec" %in% names(dat)) ||
          !is.finite(s) || !is.finite(e) || e < s) next

      ts <- suppressWarnings(as.numeric(dat$timestamp_sec))
      if (length(ts) < 1 || !any(is.finite(ts))) next
      left_spike <- max(1L, s - 1L)
      right_spike <- min(nrow(dat), e)
      if (left_spike > nrow(dat) || right_spike < 1L || right_spike < left_spike) next

      st <- suppressWarnings(as.numeric(out$start_time_sec[ii] %||% NA_real_))
      en <- suppressWarnings(as.numeric(out$end_time_sec[ii] %||% NA_real_))
      if (!is.finite(st)) st <- ts[left_spike]
      if (!is.finite(en)) en <- ts[right_spike]
      first_ts <- ts[which(is.finite(ts))[1]]
      dur <- if (is.finite(st) && is.finite(en)) en - st else NA_real_

      out$start_timestamp_sec_display[ii] <- st
      out$end_timestamp_sec_display[ii] <- en
      out$start_aligned_sec_display[ii] <- if (is.finite(st) && is.finite(first_ts)) st - first_ts else NA_real_
      out$end_aligned_sec_display[ii] <- if (is.finite(en) && is.finite(first_ts)) en - first_ts else NA_real_
      out$duration_sec_display[ii] <- dur
      out$n_spikes_display[ii] <- right_spike - left_spike + 1L
      out$n_isi_display[ii] <- max(0L, e - s + 1L)
      if (is.finite(dur) && dur > 0 && is.finite(out$n_isi_display[ii])) {
        out$mean_rate_hz_display[ii] <- out$n_isi_display[ii] / dur
      }

      if (!("ISI_sec" %in% names(dat))) next
      isi_idx <- seq(max(1L, s), min(nrow(dat), e))
      vals <- suppressWarnings(as.numeric(dat$ISI_sec[isi_idx]))
      vals <- vals[is.finite(vals) & !is_artifact_isi(vals, min_isi)]
      out$n_valid_isi_display[ii] <- length(vals)
      if (length(vals) == 0) next
      out$mean_ISI_sec_display[ii] <- mean(vals)
      out$median_ISI_sec_display[ii] <- stats::median(vals)
      out$q10_ISI_sec_display[ii] <- safe_q(vals, 0.10)
      out$q90_ISI_sec_display[ii] <- safe_q(vals, 0.90)
      out$min_ISI_sec_display[ii] <- min(vals)
      out$max_ISI_sec_display[ii] <- max(vals)
      out$CV_display[ii] <- calc_CV(vals)
      out$CV2_display[ii] <- calc_cv2_local(vals)
      out$LV_display[ii] <- calc_LV(vals)
      out$MM_display[ii] <- if (is.finite(mean(vals)) && mean(vals) > 0) max(vals) / mean(vals) else NA_real_
    }
    out
  }
	  
	  observe({
	    nm <- near_miss_bundle()
	    pats <- if (nrow(nm) > 0) sort(unique(as.character(nm$pattern))) else character(0)
	    pats <- pats[nzchar(pats)]
	    pat_selected <- input$near_miss_pattern %||% "all"
	    if (!(pat_selected %in% c("all", pats))) pat_selected <- "all"
	    updateSelectInput(session, "near_miss_pattern", choices = c("all", pats), selected = pat_selected)

	    cats <- if (nrow(nm) > 0) {
	      sub <- nm
	      if (!identical(pat_selected, "all")) sub <- sub[sub$pattern == pat_selected, , drop = FALSE]
	      sort(unique(as.character(sub$category)))
	    } else character(0)
	    cats <- cats[nzchar(cats)]
	    cat_selected <- input$near_miss_category %||% "all"
	    if (!(cat_selected %in% c("all", cats))) cat_selected <- "all"
	    updateSelectInput(session, "near_miss_category", choices = c("all", cats), selected = cat_selected)

	    sub_for_param <- nm
	    if (nrow(sub_for_param) > 0 && !identical(pat_selected, "all")) {
	      sub_for_param <- sub_for_param[sub_for_param$pattern == pat_selected, , drop = FALSE]
	    }
	    if (nrow(sub_for_param) > 0 && !identical(cat_selected, "all")) {
	      sub_for_param <- sub_for_param[sub_for_param$category == cat_selected, , drop = FALSE]
	    }
	    pars <- if (nrow(sub_for_param) > 0) sort(unique(as.character(sub_for_param$parameter))) else character(0)
	    pars <- pars[nzchar(pars)]
	    selected <- input$near_miss_parameter %||% "all"
	    if (!(selected %in% c("all", pars))) selected <- "all"
	    updateSelectInput(session, "near_miss_parameter", choices = c("all", pars), selected = selected)
	  })
  
	  output$near_miss_candidate_selector <- renderUI({
	    nm_all <- near_miss_bundle()
	    nm <- near_miss_filtered()
	    if (nrow(nm) == 0) return(helpText(near_miss_status_message(nm_all, nm)))
    nm_label <- enrich_near_miss_for_display(nm, current_dataset())
    f <- unit_factor()
    u <- unit_label()
    labs <- paste0(
      nm_label$nm_id, " | ", nm_label$pattern, "/", nm_label$category,
      " | ", nm_label$parameter,
      " | \u0394=", round(100 * nm_label$relative_change, 1), "%",
      " | ", nm_label$train,
      " | aligned ", round(nm_label$start_aligned_sec_display * f, if (identical(u, "ms")) 1 else 4),
      "-", round(nm_label$end_aligned_sec_display * f, if (identical(u, "ms")) 1 else 4), " ", u,
      " | spikes=", nm_label$n_spikes_display,
      " | dur=", round(nm_label$duration_sec_display * f, if (identical(u, "ms")) 1 else 4), " ", u
    )
    selectizeInput("near_miss_selected_id", "Candidate", choices = setNames(as.character(nm$nm_id), labs),
                   selected = as.character(min(rv$near_miss_idx %||% 1L, nrow(nm))), options = list(maxOptions = 1000))
  })
  
  set_preview_from_near_miss <- function(row, jump = TRUE) {
    if (is.null(row) || nrow(row) == 0) return()
    rv$preview_candidate <- list(
      train = as.character(row$train[1]),
      pattern = as.character(row$pattern[1]),
      category = as.character(row$category[1]),
      parameter = as.character(row$parameter[1]),
      start_isi = as.integer(row$start_isi[1]),
      end_isi = as.integer(row$end_isi[1]),
      start_time_sec = suppressWarnings(as.numeric(row$start_time_sec[1])),
      end_time_sec = suppressWarnings(as.numeric(row$end_time_sec[1])),
      details = as.character(row$details[1]),
      active = TRUE
    )
    if (isTRUE(jump)) {
      updateCheckboxInput(session, "show_near_miss_preview", value = TRUE)
      ds <- get_dataset()
      if (!is.null(ds) && row$train[1] %in% names(ds$trains)) {
        dat <- ds$trains[[as.character(row$train[1])]]
        sidx <- max(1L, as.integer(row$start_isi[1]) - 1L)
        eidx <- min(nrow(dat), as.integer(row$end_isi[1]))
        if (is.finite(sidx) && is.finite(eidx) && sidx >= 1 && eidx <= nrow(dat)) {
          t0 <- dat$timestamp_sec[sidx] - dat$timestamp_sec[1]
          t1 <- dat$timestamp_sec[eidx] - dat$timestamp_sec[1]
          pad <- max(0.025, (t1 - t0) * 3, (current_param_for_tables()$burst$preview_pad_sec %||% 0.150))
          f <- unit_factor()
          rv$view_align_x <- c(max(0, t0 - pad), t1 + pad) * f
          updateSliderInput(session, "xrange", value = rv$view_align_x)
          if (identical(input$train_display_mode %||% "paged_all", "selected_only")) {
            if (!is.null(input$trains) && !(row$train[1] %in% input$trains)) {
              updateSelectizeInput(session, "trains", selected = unique(c(as.character(row$train[1]), input$trains)))
            }
          } else {
            td_now <- names(current_trains())
            pos <- match(as.character(row$train[1]), td_now)
            if (is.finite(pos)) {
              per <- safe_int(input$visible_trains_per_page, 10L)
              updateNumericInput(session, "train_page", value = max(1L, ceiling(pos / max(1L, per))))
            }
          }
          updateTabsetPanel(session, "main_tabs", selected = "\u5BF9\u9F50\u65F6\u95F4\u6233\u56FE")
        }
      }
    }
  }
  
  current_near_miss_row <- function() {
    nm <- near_miss_filtered()
    if (nrow(nm) == 0) return(NULL)
    idx <- suppressWarnings(as.integer(rv$near_miss_idx %||% input$near_miss_selected_id %||% 1L))
    idx <- max(1L, min(nrow(nm), idx))
    rv$near_miss_idx <- idx
    nm[idx, , drop = FALSE]
  }
  
  observeEvent(input$near_miss_table_rows_selected, {
    sel <- input$near_miss_table_rows_selected
    nm <- near_miss_filtered()
    if (length(sel) == 1 && nrow(nm) >= sel) {
      rv$near_miss_idx <- sel
      updateSelectizeInput(session, "near_miss_selected_id", selected = as.character(sel))
      set_preview_from_near_miss(nm[sel, , drop = FALSE], jump = TRUE)
    }
  })

  observeEvent(input$near_miss_selected_id, {
    idx <- suppressWarnings(as.integer(input$near_miss_selected_id %||% rv$near_miss_idx %||% 1L))
    if (is.finite(idx)) rv$near_miss_idx <- idx
    row <- current_near_miss_row()
    if (!is.null(row) && !is.null(rv$preview_candidate) && isTRUE(rv$preview_candidate$active)) {
      set_preview_from_near_miss(row, jump = FALSE)
    }
  }, ignoreInit = TRUE)

  observeEvent(rv$current_id, {
    rv$preview_candidate <- NULL
    rv$distribution_evidence_selected_row <- NULL
  }, ignoreInit = TRUE)
  
  observeEvent(input$near_miss_jump, {
    row <- current_near_miss_row()
    if (!is.null(row)) set_preview_from_near_miss(row, jump = TRUE)
  })
  
  observeEvent(input$near_miss_prev, {
    nm <- near_miss_filtered()
    if (nrow(nm) == 0) return()
    rv$near_miss_idx <- max(1L, (rv$near_miss_idx %||% 1L) - 1L)
    updateSelectizeInput(session, "near_miss_selected_id", selected = as.character(rv$near_miss_idx))
    set_preview_from_near_miss(nm[rv$near_miss_idx, , drop = FALSE], jump = TRUE)
  })
  
  observeEvent(input$near_miss_next, {
    nm <- near_miss_filtered()
    if (nrow(nm) == 0) return()
    rv$near_miss_idx <- min(nrow(nm), (rv$near_miss_idx %||% 1L) + 1L)
    updateSelectizeInput(session, "near_miss_selected_id", selected = as.character(rv$near_miss_idx))
    set_preview_from_near_miss(nm[rv$near_miss_idx, , drop = FALSE], jump = TRUE)
  })
  
  output$detector_before_after_summary <- renderText({
    rv$last_detector_summary %||% "\u5C1A\u65E0\u68C0\u6D4B\u5668\u91CD\u8DD1\u6458\u8981\u3002"
  })

	  output$near_miss_details <- renderText({
	    row <- current_near_miss_row()
	    if (is.null(row) || nrow(row) == 0) return(near_miss_status_message(near_miss_bundle(), near_miss_filtered()))
    row_display <- enrich_near_miss_for_display(row, current_dataset())
    f <- unit_factor()
    u <- unit_label()
    near_miss_detail_fmt <- function(x, digits = 5) {
      x <- suppressWarnings(as.numeric(x))
      if (length(x) == 0 || !is.finite(x[1])) return("NA")
      as.character(signif(x[1], digits))
    }
	    paste0(
      "Candidate: ", row$nm_id, "\n",
      "Pattern/category: ", row$pattern, " / ", row$category, "\n",
      "Train\uFF1A", row$train, " | timestamp ", near_miss_detail_fmt(row_display$start_timestamp_sec_display * f), "-", near_miss_detail_fmt(row_display$end_timestamp_sec_display * f), " ", u,
      " | aligned ", near_miss_detail_fmt(row_display$start_aligned_sec_display * f), "-", near_miss_detail_fmt(row_display$end_aligned_sec_display * f), " ", u, "\n",
      "Spikes/ISI: ", row_display$n_spikes_display, " / ", row_display$n_isi_display,
      " | duration: ", near_miss_detail_fmt(row_display$duration_sec_display * f), " ", u,
      " | rate: ", near_miss_detail_fmt(row_display$mean_rate_hz_display), " Hz",
      " | CV/LV/MM: ", near_miss_detail_fmt(row_display$CV_display, 4), " / ", near_miss_detail_fmt(row_display$LV_display, 4), " / ", near_miss_detail_fmt(row_display$MM_display, 4), "\n",
      "\u53C2\u6570\uFF1A", row$parameter, " (", row$direction, ")\n",
      "Current value: ", signif(row$current_value, 6), "\n",
      "Required value: ", signif(row$required_value, 6), "\n",
      "Relative adjustment: ", round(100 * row$relative_change, 2), "%\n",
      "Failure count: ", row$failure_count, "\n",
      "Reason: ", row$reason, "\n",
      "Details: ", row$details
    )
  })
  
	  output$near_miss_table <- renderDT({
	    nm_all <- near_miss_bundle()
	    nm <- near_miss_filtered()
	    if (nrow(nm) == 0) return(datatable(data.frame(message = near_miss_status_message(nm_all, nm)), options = list(dom = "t")))
    f <- unit_factor()
    u <- unit_label()
    time_digits <- if (identical(u, "ms")) 3L else 6L
    isi_digits <- if (identical(u, "ms")) 4L else 6L
    nm_show <- enrich_near_miss_for_display(nm, current_dataset())
    show <- data.frame(
      nm_id = nm_show$nm_id,
      pattern = nm_show$pattern,
      category = nm_show$category,
      train = nm_show$train,
      ISI_index_range = nm_show$ISI_index_range,
      n_spikes = nm_show$n_spikes_display,
      n_isi = nm_show$n_isi_display,
      valid_isi = nm_show$n_valid_isi_display,
      rate_Hz = round(nm_show$mean_rate_hz_display, 3),
      CV = signif(nm_show$CV_display, 4),
      CV2 = signif(nm_show$CV2_display, 4),
      LV = signif(nm_show$LV_display, 4),
      MM = signif(nm_show$MM_display, 4),
      parameter = nm_show$parameter,
      direction = nm_show$direction,
      current_value = signif(nm_show$current_value, 6),
      required_value = signif(nm_show$required_value, 6),
      relative_change_pct = round(100 * nm_show$relative_change, 2),
      failure_count = nm_show$failure_count,
      score = signif(nm_show$score, 4),
      reason = nm_show$reason,
      details = nm_show$details,
      stringsAsFactors = FALSE
    )
    show[[paste0("start_timestamp_", u)]] <- round(nm_show$start_timestamp_sec_display * f, time_digits)
    show[[paste0("end_timestamp_", u)]] <- round(nm_show$end_timestamp_sec_display * f, time_digits)
    show[[paste0("start_aligned_", u)]] <- round(nm_show$start_aligned_sec_display * f, time_digits)
    show[[paste0("end_aligned_", u)]] <- round(nm_show$end_aligned_sec_display * f, time_digits)
    show[[paste0("duration_", u)]] <- round(nm_show$duration_sec_display * f, time_digits)
    show[[paste0("mean_ISI_", u)]] <- round(nm_show$mean_ISI_sec_display * f, isi_digits)
    show[[paste0("median_ISI_", u)]] <- round(nm_show$median_ISI_sec_display * f, isi_digits)
    show[[paste0("q10_ISI_", u)]] <- round(nm_show$q10_ISI_sec_display * f, isi_digits)
    show[[paste0("q90_ISI_", u)]] <- round(nm_show$q90_ISI_sec_display * f, isi_digits)
    show[[paste0("min_ISI_", u)]] <- round(nm_show$min_ISI_sec_display * f, isi_digits)
    show[[paste0("max_ISI_", u)]] <- round(nm_show$max_ISI_sec_display * f, isi_digits)
    ordered_cols <- c(
      "nm_id", "pattern", "category", "train",
      paste0("start_timestamp_", u), paste0("end_timestamp_", u),
      paste0("start_aligned_", u), paste0("end_aligned_", u),
      paste0("duration_", u), "n_spikes", "n_isi", "valid_isi", "ISI_index_range",
      "rate_Hz", paste0("mean_ISI_", u), paste0("median_ISI_", u),
      paste0("q10_ISI_", u), paste0("q90_ISI_", u),
      paste0("min_ISI_", u), paste0("max_ISI_", u),
      "CV", "CV2", "LV", "MM",
      "parameter", "direction", "current_value", "required_value",
      "relative_change_pct", "failure_count", "score", "reason", "details"
    )
    show <- show[, ordered_cols, drop = FALSE]
    datatable(show, rownames = FALSE, selection = "single",
              options = list(pageLength = 12, scrollX = TRUE))
  })
  
	  output$near_miss_plot <- renderPlotly({
	    nm_all <- near_miss_bundle()
	    nm <- near_miss_filtered()
	    validate(need(nrow(nm) > 0, near_miss_status_message(nm_all, nm)))
    x <- 100 * nm$relative_change
    bw <- 2
    if (length(x) > 0 && max(x, na.rm = TRUE) <= 10) bw <- 1
    br <- seq(0, ceiling(max(x, na.rm = TRUE) / bw) * bw + bw, by = bw)
    h <- hist(x, breaks = br, plot = FALSE)
    dd <- data.frame(mid = h$mids, count = h$counts)
    hover <- character(length(h$mids))
    for (ii in seq_along(h$mids)) {
      lo <- br[ii]; hi <- br[ii+1]
      sub <- nm[x >= lo & x < hi, , drop = FALSE]
      if (nrow(sub) == 0) {
        hover[ii] <- paste0("[", lo, ", ", hi, ") %<br>\u8BA1\u6570: 0")
      } else {
        head_sub <- head(sub, 8)
        items <- paste0(head_sub$nm_id, " | ", head_sub$pattern, "/", head_sub$category, " | ",
                        head_sub$parameter, " | ", head_sub$train, " | ISI ",
                        head_sub$start_isi, "-", head_sub$end_isi)
        hover[ii] <- paste0("[", lo, ", ", hi, ") %<br>\u8BA1\u6570: ", nrow(sub),
                            "<br>", paste(items, collapse = "<br>"),
                            if (nrow(sub) > 8) paste0("<br>... +", nrow(sub) - 8, " more") else "")
      }
    }
    dd$hover <- hover
    plot_ly(dd, x = ~mid, y = ~count, type = "bar", hoverinfo = "text", text = ~hover) %>%
      layout(hoverlabel = stpd_hoverlabel_style(), xaxis = list(title = "\u6240\u9700\u76F8\u5BF9\u9608\u503C\u8C03\u6574\uFF08%\uFF09"),
             yaxis = list(title = "Near-miss \u5019\u9009"),
             margin = list(l = 60, r = 20, t = 30, b = 60)) %>%
      config(displaylogo = FALSE)
  })
  
	  near_miss_companion_rows <- function(row, nm = NULL) {
	    if (is.null(row) || nrow(row) == 0) return(row)
	    if (is.null(nm)) nm <- near_miss_bundle()
	    if (is.null(nm) || nrow(nm) == 0) return(row)
	    cref <- near_miss_chr1(row$candidate_ref)
	    if (nzchar(cref) && "candidate_ref" %in% names(nm)) {
	      crefs <- as.character(nm$candidate_ref)
	      crefs[is.na(crefs)] <- ""
	      out <- nm[crefs == cref, , drop = FALSE]
	      if (nrow(out) > 0) return(out)
	    }
    key_cols <- intersect(c("pattern", "category", "train", "start_isi", "end_isi"), names(nm))
    if (length(key_cols) == 0) return(row)
    same_key_value <- function(x, y) {
      if (is.numeric(x) || is.integer(x)) {
        xx <- suppressWarnings(as.numeric(x))
        yy <- suppressWarnings(as.numeric(y))
        return(is.finite(xx) & is.finite(yy) & xx == yy)
      }
      xx <- as.character(x)
      yy <- as.character(y)
      !is.na(xx) & !is.na(yy) & xx == yy
    }
    keep <- rep(TRUE, nrow(nm))
    for (cc in key_cols) keep <- keep & same_key_value(nm[[cc]], row[[cc]][1])
    out <- nm[keep, , drop = FALSE]
    if (nrow(out) > 0) out else row
  }

	  near_miss_applicable_rows <- function(rows) {
    if (is.null(rows) || nrow(rows) == 0) return(rows)
    ok <- vapply(seq_len(nrow(rows)), function(ii) {
      path <- near_miss_parameter_path(rows$parameter[ii])
      val <- suppressWarnings(as.numeric(rows$required_value[ii]))
      !is.null(path) && nzchar(path) && is.finite(val)
    }, logical(1))
	    rows[ok, , drop = FALSE]
	  }

  near_miss_chr1 <- function(x, default = "") {
    if (is.null(x) || length(x) == 0) return(default)
    out <- suppressWarnings(as.character(x[1]))
    if (length(out) == 0 || is.na(out)) default else out
  }

  near_miss_distinct_parameters <- function(rows) {
    if (is.null(rows) || nrow(rows) == 0 || !("parameter" %in% names(rows))) return(character(0))
    par <- as.character(rows$parameter)
    par <- par[!is.na(par) & nzchar(par)]
    unique(par)
  }

  apply_near_miss_threshold_to_ui <- function(row, params = NULL) {
    rows <- near_miss_applicable_rows(row)
    if (is.null(rows) || nrow(rows) == 0) return(FALSE)
    p <- params %||% read_params_from_ui()
    p2 <- apply_near_miss_thresholds_to_params(p, rows)
    apply_params_to_ui(p2, preserve_pattern_isi_limits = TRUE)
    TRUE
  }

  near_miss_parameter_path <- function(parameter) {
    switch(as.character(parameter %||% "")[1],
           "burst_structure_edge_min" = "burst.structure_edge_min",
           "burst_structure_edge_geom" = "burst.structure_edge_geom_min",
           "burst_structure_core_q_max" = "burst.structure_core_q_max",
           "burst_bridge_ratio_max" = "burst.bridge_ratio_max",
           "burst_bridge_core_inflate" = "burst.bridge_core_inflate",
           "burst_bridge_raw_max" = "burst.bridge_raw_max",
           "burst_bridge_edge_min" = "burst.bridge_merged_edge_min",
           "burst_bridge_edge_geom" = "burst.bridge_merged_edge_geom_min",
           "burst_final_edge_min" = "burst.final_edge_contrast_min",
           "burst_final_edge_geom" = "burst.final_edge_contrast_geom_min",
           "burst_score_high" = "burst.score_high",
           "burst_final_duration_max" = "burst.final_max_duration",
           "tonic_T_min" = "tonic.T_min",
           "tonic_T_max" = "tonic.T_max",
           "tonic_LV_core" = "tonic.LV_core",
           "tonic_seed_ratio" = "tonic.seed_ratio",
           "tonic_mm_max" = "tonic.tonic_mm_max",
           "tonic_mm_min" = "tonic.tonic_mm_min",
           "pause_alpha" = "pause.alpha",
           "pause_T_seed" = "pause.T_seed",
           "pause_T_strong" = "pause.T_strong",
           "pause_global_median_factor" = "pause.global_median_factor",
           "local_compression_local_ratio_min" = "burst.local_compression_local_ratio_min",
           "local_compression_core_pct_max" = "burst.local_compression_core_pct_max",
           "local_compression_edge_min" = "burst.local_compression_edge_min",
           "local_compression_edge_geom" = "burst.local_compression_edge_geom",
           "boundary_one_flank_ratio_min" = "burst.boundary_one_flank_ratio_min",
           "boundary_local_ratio_min" = "burst.boundary_local_ratio_min",
           "boundary_core_pct_max" = "burst.boundary_core_pct_max",
           NULL)
  }

  apply_near_miss_threshold_to_params <- function(p, row) {
    if (is.null(row) || nrow(row) == 0) return(p)
    par <- as.character(row$parameter[1])
    val <- suppressWarnings(as.numeric(row$required_value[1]))
    if (!is.finite(val)) return(p)
    path <- near_miss_parameter_path(par)
    if (is.null(path) || !nzchar(path)) return(p)
    p <- stpd_set_param(p, path, val)
    p
  }

  apply_near_miss_thresholds_to_params <- function(p, rows) {
    rows <- near_miss_applicable_rows(rows)
    if (is.null(rows) || nrow(rows) == 0) return(p)
    for (ii in seq_len(nrow(rows))) {
      p <- apply_near_miss_threshold_to_params(p, rows[ii, , drop = FALSE])
    }
    if (exists("stpd_productize_params", mode = "function")) {
      p <- tryCatch(stpd_productize_params(p, prefer = "legacy"), error = function(e) p)
    }
    p
  }

  summarize_events_for_threshold_preview <- function(ds, p, target_trains = NULL) {
    if (is.null(ds) || is.null(ds$trains)) return(data.frame())
    event_trains <- if (!is.null(target_trains)) ds$trains[intersect(target_trains, names(ds$trains))] else ds$trains
    ev <- derive_interval_tables(event_trains,
                                 source = "final",
                                 auto_others = FALSE,
                                 dataset_map = setNames(rep(ds$meta$display_name %||% "dataset", length(event_trains)), names(event_trains)),
                                 min_isi_sec = p$detector$min_valid_isi_sec %||% min_valid_isi_sec(),
                                 contrast_q = p$burst$contrast_q %||% 0.90,
                                 context_k = p$burst$context_k %||% 5L)$events
    labs <- c("burst", "possible_burst", "tonic", "high_frequency_tonic", "high_frequency_spiking", "pause", "others")
    out <- data.frame(pattern = labs, n_events = 0L, stringsAsFactors = FALSE)
    if (!is.null(ev) && nrow(ev) > 0 && "pattern" %in% names(ev)) {
      cc <- as.data.frame(table(ev$pattern), stringsAsFactors = FALSE)
      names(cc) <- c("pattern", "n_events")
      out <- merge(out[, "pattern", drop = FALSE], cc, by = "pattern", all.x = TRUE, sort = FALSE)
      out$n_events[is.na(out$n_events)] <- 0L
    }
    out$n_events <- as.integer(out$n_events)
    out
  }

  format_before_after_summary <- function(before, after, row) {
    if (is.null(before) || is.null(after) || nrow(before) == 0 || nrow(after) == 0) return("\u65E0 before/after \u6458\u8981\u53EF\u7528\u3002")
    m <- merge(before, after, by = "pattern", all = TRUE, suffixes = c("_before", "_after"))
    m$n_events_before[is.na(m$n_events_before)] <- 0L
    m$n_events_after[is.na(m$n_events_after)] <- 0L
    m$delta <- m$n_events_after - m$n_events_before
    param_txt <- if (!is.null(row) && nrow(row) > 1L) {
      vals <- signif(suppressWarnings(as.numeric(row$required_value)), 6)
      bits <- paste0(as.character(row$parameter), "=", vals)
      paste0("\u53C2\u6570\uFF1Aapplied ", nrow(row), " companion threshold(s): ", paste(bits, collapse = "; "))
    } else {
      paste0("\u53C2\u6570\uFF1A", as.character(row$parameter[1]), " | required value: ", signif(as.numeric(row$required_value[1]), 6))
    }
    lines <- c(
      "Threshold applied and detector re-run.",
      param_txt,
      "Event counts before -> after (delta):"
    )
    for (ii in seq_len(nrow(m))) {
      lines <- c(lines, paste0("  ", m$pattern[ii], ": ", m$n_events_before[ii], " -> ", m$n_events_after[ii],
                               " (", ifelse(m$delta[ii] >= 0, "+", ""), m$delta[ii], ")"))
    }
	    paste(lines, collapse = "\n")
	  }

  near_miss_candidate_label_summary <- function(ds, row, params = NULL) {
    if (is.null(ds) || is.null(row) || nrow(row) == 0 || is.null(ds$trains)) return("")
    tr <- as.character(row$train[1] %||% "")
    if (!nzchar(tr) || !(tr %in% names(ds$trains))) return("")
    dat <- ds$trains[[tr]]
    sidx <- suppressWarnings(as.integer(row$start_isi[1]))
    eidx <- suppressWarnings(as.integer(row$end_isi[1]))
    if (!is.finite(sidx) || !is.finite(eidx) || eidx < sidx || is.null(dat) || nrow(dat) == 0) return("")
    idx <- seq.int(sidx, eidx)
    idx <- idx[idx >= 1L & idx <= nrow(dat)]
    if (length(idx) == 0) return("")
    params <- params %||% read_params_from_ui()
    min_isi <- suppressWarnings(as.numeric(params$detector$min_valid_isi_sec %||% min_valid_isi_sec()))
    if (!is.finite(min_isi)) min_isi <- 0.001
    final <- compute_final_pattern(dat$pattern_manual, dat$pattern_auto, dat$ISI_sec, auto_others = FALSE, min_isi_sec = min_isi)
    expected <- as.character(row$pattern[1] %||% "")
    auto_lab <- mode_nonempty_label(dat$pattern_auto[idx])
    final_lab <- mode_nonempty_label(final[idx])
    manual_lab <- mode_nonempty_label(dat$pattern_manual[idx])
    printable <- function(x) if (nzchar(x %||% "")) x else "(none)"
    expected_ok <- if (identical(expected, "burst")) {
      final_lab %in% c("burst", "long_burst", "possible_burst")
    } else {
      identical(final_lab, expected)
    }
    status <- if (isTRUE(expected_ok)) {
      "\u5019\u9009\u5DF2\u5199\u6210\u76EE\u6807\u6A21\u5F0F\u3002"
    } else {
      "\u5019\u9009\u91CD\u8DD1\u540E\u4ECD\u672A\u5199\u6210\u76EE\u6807\u6A21\u5F0F\uFF1B\u8FD8\u6709\u9608\u503C\u4E4B\u5916\u7684\u95E8\u63A7\u3001\u4EF2\u88C1\u6216\u6700\u5C0F\u4E8B\u4EF6\u89C4\u5219\u5728\u963B\u65AD\u3002"
    }
    paste(
      "\u9009\u4E2D\u5019\u9009\u91CD\u8DD1\u540E\u6807\u7B7E\uFF1A",
      paste0("  expected: ", printable(expected)),
      paste0("  AUTO: ", printable(auto_lab)),
      paste0("  MANUAL: ", printable(manual_lab)),
      paste0("  FINAL: ", printable(final_lab)),
      paste0("  status: ", status),
      sep = "\n"
    )
  }

  near_miss_final_auto_gate <- function(ds, row, params = NULL) {
    if (is.null(ds) || is.null(row) || nrow(row) == 0 || is.null(ds$trains)) {
      return(list(pass = TRUE, reason = "no_candidate_context"))
    }
    tr <- near_miss_chr1(row$train)
    if (!nzchar(tr) || !(tr %in% names(ds$trains))) return(list(pass = TRUE, reason = "candidate_train_not_found"))
    dat <- ds$trains[[tr]]
    sidx <- suppressWarnings(as.integer(row$start_isi[1]))
    eidx <- suppressWarnings(as.integer(row$end_isi[1]))
    if (!is.finite(sidx) || !is.finite(eidx) || eidx < sidx || sidx < 2L || eidx > nrow(dat)) {
      return(list(pass = TRUE, reason = "candidate_range_not_checkable"))
    }
    params <- params %||% read_params_from_ui()
    lab <- near_miss_chr1(row$pattern, "burst")
    if (!(lab %in% c("burst", "long_burst", "possible_burst", "tonic", "high_frequency_tonic", "high_frequency_spiking", "pause", "others"))) {
      lab <- "burst"
    }
    idx <- sidx:eidx
    min_isi <- suppressWarnings(as.numeric(params$detector$min_valid_isi_sec %||% min_valid_isi_sec()))
    if (!is.finite(min_isi)) min_isi <- 0.001
    isi <- suppressWarnings(as.numeric(dat$ISI_sec))
    art <- is_artifact_isi(isi, min_isi)
    min_spk <- max(1L, stpd_min_spikes_for_label(lab, params))
    min_isi_n <- max(0L, min_spk - 1L)
    min_dur <- suppressWarnings(as.numeric(stpd_min_duration_for_label(lab, params)))
    if (!is.finite(min_dur)) min_dur <- 0
    n_spikes <- eidx - sidx + 2L
    n_isi <- eidx - sidx + 1L
    n_valid_isi <- sum(is.finite(isi[idx]) & !art[idx], na.rm = TRUE)
    start_t <- suppressWarnings(as.numeric(dat$timestamp_sec[sidx - 1L]))
    end_t <- suppressWarnings(as.numeric(dat$timestamp_sec[eidx]))
    dur <- end_t - start_t
    size_ok <- n_spikes >= min_spk && n_isi >= min_isi_n && n_valid_isi >= min_isi_n
    dur_ok <- !is.finite(min_dur) || min_dur <= 0 || (is.finite(dur) && dur >= min_dur)
    isi_gate <- stpd_pattern_isi_gate_pass(isi[idx], lab, params, min_isi_sec = min_isi)
    isi_ok <- isTRUE(isi_gate$pass)
    reasons <- c(
      if (!size_ok) paste0("minimum size gate: n_spikes=", n_spikes, ", required=", min_spk),
      if (!dur_ok) paste0("minimum duration gate: duration=", signif(dur, 5), " s, required=", signif(min_dur, 5), " s"),
      if (!isi_ok) paste0("pattern ISI gate: ", isi_gate$reason)
    )
    if (length(reasons) == 0) reasons <- "final AUTO gate pass"
    list(pass = isTRUE(size_ok && dur_ok && isi_ok), reason = paste(reasons, collapse = "; "))
  }

  near_miss_prune_candidate <- function(ds, row) {
    if (is.null(ds) || is.null(ds$results) || is.null(row) || nrow(row) == 0) return(ds)
    nm <- ds$results$near_miss_candidates
    if (is.null(nm) || nrow(nm) == 0) return(ds)
    keep <- rep(TRUE, nrow(nm))
    same_chr <- function(x, y) {
      xx <- as.character(x)
      yy <- as.character(y)[1]
      xx[is.na(xx)] <- ""
      yy[is.na(yy)] <- ""
      xx == yy
    }
    cref <- near_miss_chr1(row$candidate_ref)
    if (nzchar(cref) && "candidate_ref" %in% names(nm)) {
      crefs <- as.character(nm$candidate_ref)
      crefs[is.na(crefs)] <- ""
      keep <- keep & crefs != cref
    } else if (all(c("pattern", "category", "train", "start_isi", "end_isi") %in% names(nm))) {
      keep <- keep & !(
        same_chr(nm$pattern, row$pattern[1]) &
          same_chr(nm$category, row$category[1]) &
          same_chr(nm$train, row$train[1]) &
          suppressWarnings(as.integer(nm$start_isi)) == suppressWarnings(as.integer(row$start_isi[1])) &
          suppressWarnings(as.integer(nm$end_isi)) == suppressWarnings(as.integer(row$end_isi[1]))
      )
    }
    ds$results$near_miss_candidates <- nm[keep, , drop = FALSE]
    ds
  }

  near_miss_accept_candidate_as_manual <- function(row, action = "\u63A5\u53D7 near-miss \u5019\u9009") {
    validate(need(!is.null(row) && nrow(row) >= 1, "No near-miss candidate selected."))
    ds <- current_dataset()
    tr <- near_miss_chr1(row$train)
    validate(need(tr %in% names(ds$trains), "Selected candidate train not found."))
    dat <- ds$trains[[tr]]
    sidx <- suppressWarnings(as.integer(row$start_isi[1]))
    eidx <- suppressWarnings(as.integer(row$end_isi[1]))
    validate(need(is.finite(sidx) && is.finite(eidx) && sidx >= 2 && eidx <= nrow(dat), "Invalid candidate ISI range."))
    lab <- near_miss_chr1(row$pattern, "burst")
    allowed <- c("burst", "long_burst", "possible_burst", "tonic", "high_frequency_tonic", "high_frequency_spiking", "pause", "others")
    if (!(lab %in% allowed)) lab <- "burst"
    push_manual_undo(paste0(action, " -> MANUAL ", lab))
    idx <- sidx:eidx
    dat$pattern_manual[idx] <- lab
    if ("pattern_manual_negative" %in% names(dat)) dat$pattern_manual_negative[idx] <- ""
    ds$trains[[tr]] <- dat
    ds <- near_miss_prune_candidate(ds, row)
    set_dataset(rv$current_id, ds)
    rv$preview_candidate <- NULL
    rv$near_miss_idx <- 1L
    rv$distribution_evidence_selected_row <- NULL
    updateCheckboxInput(session, "show_near_miss_preview", value = FALSE)
    updateRadioButtons(session, "pattern_view", selected = "audit_final")
    updateTabsetPanel(session, "main_tabs", selected = "\u5BF9\u9F50\u65F6\u95F4\u6233\u56FE")
    list(dataset = ds, train = tr, start_isi = sidx, end_isi = eidx, label = lab, n_isi = length(idx))
  }

  output$near_miss_rerun_summary <- renderText({ rv$near_miss_rerun_summary %||% "" })

	  observeEvent(input$near_miss_apply_threshold, {
	    row <- current_near_miss_row()
	    companion_rows <- near_miss_companion_rows(row)
	    rows_to_apply <- near_miss_applicable_rows(companion_rows)
	    validate(need(!is.null(rows_to_apply) && nrow(rows_to_apply) > 0, "No applicable near-miss threshold is selected."))
    p_before <- read_params_from_ui()
    companion_param_n <- length(near_miss_distinct_parameters(companion_rows))
	    final_gate <- near_miss_final_auto_gate(current_dataset(), row, params = p_before)
	    needs_manual_review <- companion_param_n > 1L || !isTRUE(final_gate$pass)
		    if (isTRUE(needs_manual_review)) {
      review_action <- if (companion_param_n > 1L) "\u591A\u9608\u503C near-miss \u590D\u6838\u901A\u8FC7" else "\u6700\u7EC8 gate near-miss \u590D\u6838\u901A\u8FC7"
		      res <- near_miss_accept_candidate_as_manual(row, action = review_action)
	      vals <- signif(suppressWarnings(as.numeric(rows_to_apply$required_value)), 6)
	      params_txt <- paste0(as.character(rows_to_apply$parameter), "=", vals, collapse = "; ")
      reason_txt <- paste(c(
        if (companion_param_n > 1L) paste0("\u9700\u8981 ", companion_param_n, " \u4E2A\u9608\u503C\u540C\u65F6\u653E\u5BBD"),
        if (!isTRUE(final_gate$pass)) paste0("\u5355\u72EC\u8C03\u9608\u503C\u540E\u4ECD\u4F1A\u88AB\u6700\u7EC8 AUTO gate \u963B\u65AD\uFF1A", final_gate$reason)
      ), collapse = "\uFF1B")
	      rv$near_miss_rerun_summary <- paste(
	        paste0("\u8BE5 near-miss \u5019\u9009\u4E0D\u9002\u5408\u4F5C\u4E3A\u5355\u4E2A\u5168\u5C40\u9608\u503C\u8C03\u6574\uFF08", reason_txt, "\uFF09\uFF0C\u672C\u6B21\u672A\u4FEE\u6539\u4EFB\u4F55\u53C2\u6570\u3002"),
	        paste0("\u6D89\u53CA\u9608\u503C\uFF1A", params_txt),
	        paste0("\u5DF2\u5C06 ", res$train, " ISI ", res$start_isi, "-", res$end_isi,
	               " \u5199\u4E3A MANUAL ", res$label, "\uFF08\u6700\u7EC8\u6807\u7B7E\u4F1A\u4F7F\u7528\u8BE5\u590D\u6838\u7ED3\u679C\uFF09\u3002"),
	        sep = "\n"
	      )
	      showNotification(
	        paste0("\u8BE5\u5019\u9009\u4E0D\u662F\u5355\u9608\u503C\u60C5\u5F62\uFF1B\u5DF2\u6539\u4E3A\u76F4\u63A5\u63A5\u53D7\u4E3A MANUAL ", res$label, "\uFF0C\u672A\u6539\u5168\u5C40\u9608\u503C\u3002"),
	        type = "message",
	        duration = 8
	      )
	      return(invisible(NULL))
	    }
	    p_after <- apply_near_miss_thresholds_to_params(p_before, rows_to_apply)
    ok <- apply_near_miss_threshold_to_ui(rows_to_apply, params = p_before)
    if (!isTRUE(ok)) {
      showNotification("\u65E0\u6CD5\u81EA\u52A8\u5E94\u7528\u8BE5\u9608\u503C\u3002", type = "warning", duration = 5)
      return()
    }

    if (isTRUE(input$near_miss_apply_and_rerun)) {
      ds <- current_dataset()
      selected_only <- isTRUE(input$detector_selected_only)
      target_trains <- names(ds$trains)
      if (selected_only) {
        target_trains <- intersect(displayed_train_names() %||% character(0), names(ds$trains))
        validate(need(length(target_trains) > 0, "No selected trains to run detector on."))
      }
      before <- summarize_events_for_threshold_preview(ds, p_before, target_trains = if (selected_only) target_trains else NULL)
      run_detector <- get0("run_detector_from_ui", mode = "function", inherits = TRUE)
      if (!is.function(run_detector)) {
        detector_notify_error(simpleError("Detector runner is not available in the current Shiny session."), prefix = "Near-miss \u9608\u503C\u5DF2\u5E94\u7528\uFF0C\u4F46\u91CD\u8DD1\u68C0\u6D4B\u5931\u8D25", status = "near_miss")
        return(invisible(NULL))
      }
      run_result <- tryCatch(
        run_detector(
          params_override = p_after,
          message = "\u6B63\u5728\u5E94\u7528\u9608\u503C\u5E76\u91CD\u65B0\u8FD0\u884C\u68C0\u6D4B\u5668",
          switch_to_plot = FALSE,
          notify = FALSE
        ),
        shiny.silent.error = function(e) {
          detector_notify_error(e, prefix = "Near-miss \u9608\u503C\u5DF2\u5E94\u7528\uFF0C\u4F46\u91CD\u8DD1\u68C0\u6D4B\u672A\u5B8C\u6210", status = "near_miss")
          NULL
        },
        error = function(e) {
          detector_notify_error(e, prefix = "Near-miss \u9608\u503C\u5DF2\u5E94\u7528\uFF0C\u4F46\u91CD\u8DD1\u68C0\u6D4B\u5931\u8D25", status = "near_miss")
          NULL
        }
      )
      if (is.null(run_result) || is.null(run_result$dataset)) {
        return(invisible(NULL))
      }
	      ds_new <- run_result$dataset
	      after <- summarize_events_for_threshold_preview(ds_new, run_result$params %||% p_after, target_trains = if (selected_only) run_result$target_trains else NULL)
      candidate_summary <- near_miss_candidate_label_summary(ds_new, rows_to_apply, params = run_result$params %||% p_after)
	      rv$near_miss_idx <- 1L
	      rv$preview_candidate <- NULL
	      rv$distribution_evidence_selected_row <- NULL
	      updateCheckboxInput(session, "show_near_miss_preview", value = FALSE)
	      rv$near_miss_rerun_summary <- paste(c(format_before_after_summary(before, after, rows_to_apply), candidate_summary[nzchar(candidate_summary)]), collapse = "\n\n")
	      showNotification(paste0("\u5DF2\u5E94\u7528 ", nrow(rows_to_apply), " \u4E2A\u5EFA\u8BAE\u9608\u503C\u5E76\u91CD\u8DD1\u68C0\u6D4B\u5668\u3002\u65E7 near-miss \u9884\u89C8\u6807\u8BB0\u5DF2\u6E05\u9664\u3002"), type = "message", duration = 7)
	      updateTabsetPanel(session, "main_tabs", selected = "\u5BF9\u9F50\u65F6\u95F4\u6233\u56FE")
    } else {
      rv$near_miss_rerun_summary <- paste0("\u5EFA\u8BAE\u9608\u503C\u4EC5\u5E94\u7528\u5230 UI\uFF0C\u68C0\u6D4B\u5668\u672A\u91CD\u8DD1\u3002\u5DF2\u5E94\u7528 ", nrow(rows_to_apply), " \u4E2A companion threshold(s)\u3002")
      showNotification(paste0("\u5EFA\u8BAE\u9608\u503C\u5DF2\u5E94\u7528\u5230 UI\uFF08", nrow(rows_to_apply), " \u4E2A\uFF09\u3002\u8BF7\u91CD\u65B0\u8FD0\u884C\u68C0\u6D4B\u5668\u4EE5\u66F4\u65B0 AUTO \u6807\u7B7E\u3002"), type = "message", duration = 5)
      updateTabsetPanel(session, "main_tabs", selected = "\u68C0\u6D4B\u5668 / \u53C2\u6570")
    }
	  })
	  observeEvent(input$near_miss_accept_manual, {
	    row <- current_near_miss_row()
	    res <- near_miss_accept_candidate_as_manual(row)
	    rv$near_miss_rerun_summary <- paste0("\u5DF2\u5C06 ", res$train, " ISI ", res$start_isi, "-", res$end_isi,
	                                         " \u63A5\u53D7\u4E3A MANUAL ", res$label, "\uFF0C\u672A\u6539\u53D8\u4EFB\u4F55\u9608\u503C\u3002")
	    showNotification(paste0("\u5DF2\u5C06\u6240\u9009 near-miss \u5019\u9009\u63A5\u53D7\u4E3A MANUAL ", res$label, "."), type = "message", duration = 5)
	  })
  
  hist_bundle <- reactive({
    pp <- pooled_trains()
    p <- current_param_for_tables()
    derive_interval_tables(
      pp$trains,
      source = input$hist_source %||% "audit_final",
      auto_others = isTRUE(input$auto_others),
      dataset_map = pp$dataset_map,
      min_isi_sec = min_valid_isi_sec(),
      contrast_q = p$burst$contrast_q %||% 0.90,
      context_k = p$burst$context_k %||% 5L
    )
  })
  
  output$hist_meta_table <- renderDT({
    hb <- hist_bundle()
    ht <- input$hist_type
    if (ht == "logisi") {
      lg <- hb$logisi
      if (nrow(lg) == 0) return(datatable(data.frame(message = "\u6CA1\u6709 logISI \u6570\u636E\u3002"), options = list(dom = "t")))
      s <- lg %>% count(source_label, name = "n") %>% arrange(desc(n))
      return(datatable(s, rownames = FALSE, options = list(dom = "t", pageLength = 10)))
    }
    
    df <- hb$intervals[[ht]]
    if (is.null(df) || nrow(df) == 0) {
      return(datatable(data.frame(message = "\u8BE5\u533A\u95F4\u7C7B\u578B\u65E0\u6570\u636E\u3002"), options = list(dom = "t")))
    }
    
    if ("value" %in% colnames(df)) {
      summary_df <- df %>% summarise(n = n(), mean = mean(value, na.rm = TRUE), median = median(value, na.rm = TRUE),
                                     q05 = as.numeric(quantile(value, 0.05, na.rm = TRUE)),
                                     q95 = as.numeric(quantile(value, 0.95, na.rm = TRUE)))
    } else {
      summary_df <- df %>% summarise(n = n(), mean_sec = mean(value_sec, na.rm = TRUE), median_sec = median(value_sec, na.rm = TRUE),
                                     q05_sec = as.numeric(quantile(value_sec, 0.05, na.rm = TRUE)),
                                     q95_sec = as.numeric(quantile(value_sec, 0.95, na.rm = TRUE)))
    }
    datatable(summary_df, rownames = FALSE, options = list(dom = "t"))
  })
  
  build_hist_hover <- function(df, bins, value_col = "value", unit = "ms", max_items = 10) {
    htexts <- character(length(bins) - 1)
    for (i in seq_len(length(bins) - 1)) {
      lo <- bins[i]
      hi <- bins[i + 1]
      sub <- df[df[[value_col]] >= lo & df[[value_col]] < hi, , drop = FALSE]
      if (nrow(sub) == 0) {
        htexts[i] <- paste0("[", signif(lo, 4), ", ", signif(hi, 4), ") ", unit, "<br>\u8BA1\u6570: 0")
      } else {
        head_sub <- head(sub, max_items)
        items <- paste0(head_sub$dataset, " | ", head_sub$train, " | ", head_sub$pattern,
                        " | [", round(head_sub$start_time_out, 3), ", ", round(head_sub$end_time_out, 3), "] ", unit)
        more <- if (nrow(sub) > max_items) paste0("<br>... +", nrow(sub) - max_items, " more") else ""
        htexts[i] <- paste0("[", signif(lo, 4), ", ", signif(hi, 4), ") ", unit,
                            "<br>\u8BA1\u6570: ", nrow(sub), "<br>", paste(items, collapse = "<br>"), more)
      }
    }
    htexts
  }
  
  output$hist_plot <- renderPlotly({
    hb <- hist_bundle()
    ht <- input$hist_type
    f <- unit_factor()
    u <- unit_label()
    
    if (ht == "logisi") {
      lg <- hb$logisi
      validate(need(nrow(lg) > 0, "No logISI data available."))
      lg$label <- lg$source_label
      lg$label[lg$label == ""] <- "\u672A\u6807\u8BB0"
      
      br <- seq(floor(min(lg$log10_ISI, na.rm = TRUE) / 0.1) * 0.1,
                ceiling(max(lg$log10_ISI, na.rm = TRUE) / 0.1) * 0.1 + 0.1,
                by = 0.1)
      labels <- unique(lg$label)
      p <- plot_ly()
      for (lab in labels) {
        x <- lg$log10_ISI[lg$label == lab]
        h <- hist(x, breaks = br, plot = FALSE)
        dd <- data.frame(mid = h$mids, count = h$counts, label = lab)
        p <- add_bars(p, data = dd, x = ~mid, y = ~count, name = lab)
      }
      
      ds <- current_dataset()
      p_last <- ds$params_last %||% ds$params_est
      shapes <- list()
      annotations <- list()
      add_vline <- function(x_sec, color, label) {
        if (!is.finite(x_sec) || x_sec <= 0) return(NULL)
        x_log <- log10(x_sec)
        list(shape = list(type = "line", x0 = x_log, x1 = x_log, y0 = 0, y1 = 1,
                          xref = "x", yref = "paper", line = list(color = color, width = 2, dash = "dash")),
             annotation = list(x = x_log, y = 1, xref = "x", yref = "paper",
                               text = label, showarrow = FALSE, yshift = 10, font = list(color = color)))
      }
      if (!is.null(p_last)) {
        vv <- list(
          add_vline(p_last$burst$T_manual, "#d62728", "T_B_manual"),
          add_vline(p_last$burst$T_MI, "#9467bd", "T_B_MI"),
          add_vline(p_last$burst$T_log, "#ff7f0e", "T_B_log"),
          add_vline(p_last$burst$T_seed, "#111111", "T_B_seed")
        )
        vv <- vv[!vapply(vv, is.null, logical(1))]
        if (length(vv) > 0) {
          shapes <- lapply(vv, `[[`, "shape")
          annotations <- lapply(vv, `[[`, "annotation")
        }
      }
      p <- layout(p, hoverlabel = stpd_hoverlabel_style(), barmode = "stack", xaxis = list(title = "log10(ISI [s])"), yaxis = list(title = "\u8BA1\u6570"),
                  shapes = shapes, annotations = annotations, margin = list(l = 60, r = 20, t = 40, b = 60))
      return(config(p, displaylogo = FALSE))
    }
    
    df <- hb$intervals[[ht]]
    validate(need(!is.null(df) && nrow(df) > 0, "\u8BE5\u533A\u95F4\u7C7B\u578B\u65E0\u6570\u636E\u3002"))
    bw <- as.numeric(input$hist_bin)
    validate(need(is.finite(bw) && bw > 0, "Bin width must be > 0."))
    
    if ("value" %in% colnames(df)) {
      df <- df %>% mutate(value_out = value, start_time_out = start_time_sec * f, end_time_out = end_time_sec * f)
      x <- df$value_out[is.finite(df$value_out)]
      u_hist <- if (grepl("pct", ht)) "%" else "dimensionless"
    } else {
      df <- df %>% mutate(value_out = value_sec * f, start_time_out = start_time_sec * f, end_time_out = end_time_sec * f)
      x <- df$value_out[is.finite(df$value_out)]
      u_hist <- u
    }
    validate(need(length(x) > 0, "No finite values to plot."))
    
    lo <- min(x); hi <- max(x)
    if (lo == hi) { lo <- lo - bw; hi <- hi + bw }
    br <- seq(floor(lo / bw) * bw, ceiling(hi / bw) * bw + bw, by = bw)
    h <- hist(x, breaks = br, plot = FALSE)
    dd <- data.frame(mid = h$mids, count = h$counts)
    dd$hover <- build_hist_hover(df, br, value_col = "value_out", unit = u_hist, max_items = 10)
    
    plot_ly(dd, x = ~mid, y = ~count, type = "bar", hoverinfo = "text", text = ~hover) %>%
      layout(hoverlabel = stpd_hoverlabel_style(), xaxis = list(title = paste0(ht, " (", u_hist, ")")), yaxis = list(title = "\u8BA1\u6570"),
             margin = list(l = 60, r = 20, t = 40, b = 60)) %>%
      config(displaylogo = FALSE)
  })
  


  # ----------------------------------------------------------
  # Dataset-level ISI histogram / seed-band phenotype
  # ----------------------------------------------------------

  dataset_isi_values_by_train <- reactive({
    ds <- current_dataset()
    vals_by_train <- list()
    for (tr in names(ds$trains)) {
      dat <- ds$trains[[tr]]
      if (is.null(dat)) next
      isi <- NULL
      if ("ISI_sec" %in% names(dat)) {
        isi <- suppressWarnings(as.numeric(dat$ISI_sec))
      } else if ("timestamp_sec" %in% names(dat)) {
        ts <- suppressWarnings(as.numeric(dat$timestamp_sec))
        ts <- ts[is.finite(ts)]
        if (length(ts) >= 2) isi <- c(NA_real_, diff(ts))
      }
      if (is.null(isi) || length(isi) == 0) next
      valid <- is.finite(isi) & isi >= min_valid_isi_sec()
      if (length(valid) > 0) valid[1] <- FALSE
      v <- isi[valid]
      v <- v[is.finite(v) & v >= min_valid_isi_sec()]
      if (length(v) > 0) vals_by_train[[tr]] <- v
    }
    vals_by_train
  })

  dataset_isi_plot_band_params <- reactive({
    f <- unit_factor()
    seed_lo <- max(0, safe_ui_value(input$event_core_seed_lower, 1) / f)
    seed_hi <- max(0, safe_ui_value(input$event_core_seed_upper, 10) / f)
    if (seed_hi < seed_lo) {
      tmp <- seed_lo; seed_lo <- seed_hi; seed_hi <- tmp
    }
    bridge_hi <- max(seed_hi, safe_ui_value(input$event_core_bridge_upper, 15) / f)
    boundary_floor <- max(0, safe_ui_value(input$event_core_boundary_floor, 0) / f)
    bin_sec <- safe_ui_value(input$dataset_isi_hist_bin, safe_ui_value(input$event_core_hist_bin, 5)) / f
    if (!is.finite(bin_sec) || bin_sec <= 0) bin_sec <- 0.005
    x_max_sec <- safe_ui_value(input$dataset_isi_hist_xmax, 100) / f
    if (!is.finite(x_max_sec) || x_max_sec < 0) x_max_sec <- 0
    list(seed_lo = seed_lo, seed_hi = seed_hi, bridge_hi = bridge_hi,
         boundary_floor = boundary_floor, bin_sec = bin_sec, x_max_sec = x_max_sec)
  })


  observeEvent(input$dataset_isi_hist_patterns, {
    pats <- input$dataset_isi_hist_patterns %||% character(0)
    srcs <- input$dataset_isi_hist_sources %||% character(0)
    if (length(pats) > 0) {
      updateCheckboxInput(session, "dataset_isi_hist_show_event_core", value = TRUE)
      if (length(srcs) == 0) {
        updateCheckboxGroupInput(session, "dataset_isi_hist_sources", selected = "histogram")
      }
    }
  }, ignoreInit = TRUE)

  observeEvent(input$dataset_isi_hist_show_event_core, {
    if (isTRUE(input$dataset_isi_hist_show_event_core)) {
      srcs <- input$dataset_isi_hist_sources %||% character(0)
      if (length(srcs) == 0) {
        updateCheckboxGroupInput(session, "dataset_isi_hist_sources", selected = "histogram")
      }
    }
  }, ignoreInit = TRUE)

  # Removed stale dataset_isi_hist_plot renderer; threshold-resolved renderer below is authoritative.
  # Removed stale dataset_isi_seed_band_table renderer; effective-band table below is authoritative.

# ----------------------------------------------------------
  # Threshold-resolved histogram overlay and threshold tables
  # ----------------------------------------------------------
  event_grammar_hex_to_rgba <- function(hex, alpha = 0.16) {
    hex <- gsub("#", "", as.character(hex %||% "999999"))
    if (nchar(hex) != 6) hex <- "999999"
    r <- strtoi(substr(hex, 1, 2), 16L); g <- strtoi(substr(hex, 3, 4), 16L); b <- strtoi(substr(hex, 5, 6), 16L)
    paste0("rgba(", r, ",", g, ",", b, ",", alpha, ")")
  }

  threshold_current_params_for_hist <- reactive({ read_params_from_ui() })

  output$dataset_isi_seed_band_table <- DT::renderDT({
    vals_by_train <- dataset_isi_values_by_train()
    if (length(vals_by_train) == 0) return(DT::datatable(data.frame(message = "\u65E0\u6709\u6548 ISI\u3002"), options = list(dom = "t"), rownames = FALSE))
    pcur <- threshold_current_params_for_hist()
    eg <- pcur$event_grammar %||% list()
    b <- (eg$effective_bands %||% list())$burst %||% list()
    seed_lo <- suppressWarnings(as.numeric(b$seed_lower_sec %||% 0.001))
    seed_hi <- suppressWarnings(as.numeric(b$seed_upper_sec %||% 0.010))
    pause_floor <- suppressWarnings(as.numeric(((eg$effective_bands %||% list())$pause %||% list())$seed_lower_sec %||% 0.100))
    f <- unit_factor(); u <- unit_label()
    run_lengths <- function(flag) {
      flag <- as.logical(flag); if (length(flag) == 0) return(integer(0))
      d <- diff(c(FALSE, flag, FALSE)); starts <- which(d == 1); ends <- which(d == -1) - 1
      if (length(starts) == 0) integer(0) else ends - starts + 1
    }
    rows <- lapply(names(vals_by_train), function(tr) {
      v <- vals_by_train[[tr]]; v <- v[is.finite(v) & v >= min_valid_isi_sec()]
      n <- length(v); if (n == 0) return(NULL)
      seed_flag <- v >= seed_lo & v <= seed_hi
      rl <- run_lengths(seed_flag)
      seed_low_pct <- mean(v <= seed_lo, na.rm = TRUE) * 100
      seed_high_pct <- mean(v <= seed_hi, na.rm = TRUE) * 100
      seed_frac <- mean(seed_flag, na.rm = TRUE) * 100
      pause_frac <- if (is.finite(pause_floor) && pause_floor > 0) mean(v >= pause_floor, na.rm = TRUE) * 100 else NA_real_
      med <- stats::median(v, na.rm = TRUE)
      q10 <- as.numeric(stats::quantile(v, 0.10, na.rm = TRUE))
      q25 <- as.numeric(stats::quantile(v, 0.25, na.rm = TRUE))
      q90 <- as.numeric(stats::quantile(v, 0.90, na.rm = TRUE))
      max_run <- if (length(rl) > 0) max(rl) else 0L
      hint <- if (n < 10) "low_ISI_count" else if (seed_frac >= 50 || max_run >= 29) "HF_spiking / extreme-fast dominant" else if (!is.na(pause_frac) && pause_frac >= 35 && seed_frac < 5) "pause-prone / sparse burst-core" else if (seed_frac >= 8 && max_run >= 2) "burst-capable" else if (seed_frac < 2) "tonic-dominant / few burst-core ISIs" else "mixed / review"
      data.frame(train = tr, n_valid_ISI = n, seed_low_percentile = round(seed_low_pct, 2), seed_high_percentile = round(seed_high_pct, 2), seed_band_fraction = round(seed_frac, 2), seed_run_count = length(rl), max_seed_run_length = max_run, median_ISI = round(med * f, 4), q10_ISI = round(q10 * f, 4), q25_ISI = round(q25 * f, 4), q90_ISI = round(q90 * f, 4), pause_fraction_at_pause_seed = round(pause_frac, 2), phenotype_hint = hint, unit = u, stringsAsFactors = FALSE)
    })
    out <- dplyr::bind_rows(rows)
    DT::datatable(out, rownames = FALSE, filter = "top", options = list(pageLength = 10, scrollX = TRUE))
  })

  output$threshold_table <- DT::renderDT({
    pcur <- threshold_current_params_for_hist()
    tb <- (pcur$event_grammar %||% list())$threshold_table %||% data.frame()
    if (is.null(tb) || nrow(tb) == 0) return(DT::datatable(data.frame(message = "\u5C1A\u65E0\u9608\u503C\u89E3\u6790\u7ED3\u679C\u3002"), options = list(dom = "t"), rownames = FALSE))
    f <- unit_factor(); u <- unit_label()
    out <- tb
    is_ratio <- out$field == "contrast_S"
    scale_event_grammar_threshold <- function(x) {
      y <- suppressWarnings(as.numeric(x))
      y[!is_ratio] <- y[!is_ratio] * f
      y
    }
    for (nm in intersect(c("user_sec", "manual_sec", "histogram_sec", "default_sec", "effective_sec"), names(out))) {
      out[[sub("_sec$", "_value", nm)]] <- round(scale_event_grammar_threshold(out[[nm]]), 6)
    }
    out$unit <- ifelse(out$field == "contrast_S", "ratio", u)
    keep <- intersect(c("pattern_label", "field", "user_value", "manual_value", "histogram_value", "default_value", "effective_value", "unit", "source"), names(out))
    DT::datatable(out[, keep, drop = FALSE], rownames = FALSE, filter = "top", options = list(pageLength = 20, scrollX = TRUE))
  })

  output$event_grammar_manual_structure_table <- DT::renderDT({
    pcur <- threshold_current_params_for_hist()
    mt <- (pcur$event_grammar %||% list())$manual_event_table %||% data.frame()
    if (is.null(mt) || nrow(mt) == 0) return(DT::datatable(data.frame(message = "\u5C1A\u65E0\u624B\u52A8\u6A21\u5F0F\u4E8B\u4EF6\u3002\u624B\u52A8\u6807\u8BB0 burst / HF / tonic / pause \u540E\u4F1A\u5728\u6B64\u663E\u793A\u7ED3\u6784\u5B66\u4E60\u7ED3\u679C\u3002"), options = list(dom = "t"), rownames = FALSE))
    f <- unit_factor(); u <- unit_label()
    out <- mt
    for (nm in intersect(c("intra_q05_sec", "intra_q10_sec", "intra_q40_sec", "intra_q90_sec", "intra_q95_sec", "intra_max_sec", "pre_gap_sec", "post_gap_sec"), names(out))) {
      out[[sub("_sec$", paste0("_", u), nm)]] <- round(out[[nm]] * f, 6)
    }
    keep <- intersect(c("train", "pattern", "start_isi", "end_isi", "n_spikes", paste0("intra_q90_", u), paste0("intra_q95_", u), paste0("pre_gap_", u), paste0("post_gap_", u), "pre_ratio_q90", "post_ratio_q90", "min_flank_ratio_q90", "boundary_type"), names(out))
    DT::datatable(out[, keep, drop = FALSE], rownames = FALSE, filter = "top", options = list(pageLength = 12, scrollX = TRUE))
  })

  observeEvent(input$event_grammar_apply_hist_suggestions, {
    pcur <- threshold_current_params_for_hist()
    tb <- (pcur$event_grammar %||% list())$threshold_table %||% data.frame()
    validate(need(!is.null(tb) && nrow(tb) > 0, "\u6CA1\u6709\u53EF\u5E94\u7528\u7684 histogram \u5EFA\u8BAE\u3002"))
    f <- unit_factor()
    get_hist <- function(pat, fld, default = NA_real_) {
      row <- tb[tb$pattern == pat & tb$field == fld, , drop = FALSE]
      val <- if (nrow(row) > 0) suppressWarnings(as.numeric(row$histogram_sec[1])) else NA_real_
      if (!is.finite(val)) return(default)
      if (identical(fld, "contrast_S")) val else val * f
    }
    updateCheckboxInput(session, "event_grammar_user_burst_enable", value = TRUE)
    updateNumericInput(session, "event_grammar_user_burst_seed_lower", value = get_hist("burst", "seed_lower_sec", 0.001 * f))
    updateNumericInput(session, "event_grammar_user_burst_seed_upper", value = get_hist("burst", "seed_upper_sec", 0.010 * f))
    updateNumericInput(session, "event_grammar_user_burst_bridge", value = get_hist("burst", "bridge_upper_sec", 0.015 * f))
    updateNumericInput(session, "event_grammar_user_burst_S", value = get_hist("burst", "contrast_S", 2.5))
    updateCheckboxInput(session, "event_grammar_user_hfs_enable", value = TRUE)
    updateNumericInput(session, "event_grammar_user_hfs_seed_lower", value = get_hist("high_frequency_spiking", "seed_lower_sec", 0.001 * f))
    updateNumericInput(session, "event_grammar_user_hfs_seed_upper", value = get_hist("high_frequency_spiking", "seed_upper_sec", 0.020 * f))
    updateNumericInput(session, "event_grammar_user_hfs_bridge", value = get_hist("high_frequency_spiking", "bridge_upper_sec", 0.030 * f))
    updateCheckboxInput(session, "event_grammar_user_hft_enable", value = TRUE)
    updateNumericInput(session, "event_grammar_user_hft_seed_lower", value = get_hist("high_frequency_tonic", "seed_lower_sec", 0.010 * f))
    updateNumericInput(session, "event_grammar_user_hft_seed_upper", value = get_hist("high_frequency_tonic", "seed_upper_sec", 0.030 * f))
    updateNumericInput(session, "event_grammar_user_hft_bridge", value = get_hist("high_frequency_tonic", "bridge_upper_sec", 0.035 * f))
    updateCheckboxInput(session, "event_grammar_user_tonic_enable", value = TRUE)
    updateNumericInput(session, "event_grammar_user_tonic_seed_lower", value = get_hist("tonic", "seed_lower_sec", 0.020 * f))
    updateNumericInput(session, "event_grammar_user_tonic_seed_upper", value = get_hist("tonic", "seed_upper_sec", 0.060 * f))
    updateNumericInput(session, "event_grammar_user_tonic_bridge", value = get_hist("tonic", "bridge_upper_sec", 0.080 * f))
    updateCheckboxInput(session, "event_grammar_user_pause_enable", value = TRUE)
    updateNumericInput(session, "event_grammar_user_pause_seed_lower", value = get_hist("pause", "seed_lower_sec", 0.100 * f))
    updateNumericInput(session, "event_grammar_user_pause_seed_upper", value = get_hist("pause", "seed_upper_sec", 0.150 * f))
    updateNumericInput(session, "event_grammar_user_pause_bridge", value = get_hist("pause", "bridge_upper_sec", 0.150 * f))
    showNotification("\u5DF2\u5C06 histogram \u81EA\u52A8\u5EFA\u8BAE\u5199\u5165\u7528\u6237\u81EA\u5B9A\u4E49\u9608\u503C\u3002\u8BF7\u68C0\u67E5\u540E\u8FD0\u884C\u68C0\u6D4B\u5668\u3002", type = "message", duration = 6)
  })

  output$dataset_isi_hist_plot <- renderPlotly({
    vals_by_train <- dataset_isi_values_by_train()
    validate(need(length(vals_by_train) > 0, "\u5F53\u524D\u6570\u636E\u96C6\u6CA1\u6709\u6709\u6548 ISI\u3002"))
    all_vals <- unlist(vals_by_train, use.names = FALSE)
    all_vals <- all_vals[is.finite(all_vals) & all_vals >= min_valid_isi_sec()]
    validate(need(length(all_vals) > 0, "\u5F53\u524D\u6570\u636E\u96C6\u6CA1\u6709\u6709\u9650\u6709\u6548 ISI\u3002"))
    pcur <- threshold_current_params_for_hist()
    tb <- (pcur$event_grammar %||% list())$threshold_table %||% data.frame()
    f <- unit_factor(); u <- unit_label()
    bin_sec <- safe_ui_value(input$dataset_isi_hist_bin, 5) / f
    if (!is.finite(bin_sec) || bin_sec <= 0) bin_sec <- 0.005
    x_max_sec <- safe_ui_value(input$dataset_isi_hist_xmax, 100) / f
    if (!is.finite(x_max_sec) || x_max_sec <= 0) x_max_sec <- as.numeric(stats::quantile(all_vals, 0.995, na.rm = TRUE))
    hist_overlay_patterns <- input$dataset_isi_hist_patterns %||% character(0)
    hist_overlay_sources <- input$dataset_isi_hist_sources %||% character(0)
    if (length(hist_overlay_patterns) > 0 && length(hist_overlay_sources) == 0) hist_overlay_sources <- "histogram"
    hist_show_overlay <- length(hist_overlay_patterns) > 0 && length(hist_overlay_sources) > 0 && isTRUE(input$dataset_isi_hist_show_event_core)
    if (hist_show_overlay && !is.null(tb) && nrow(tb) > 0) {
      vcols <- c()
      for (src in hist_overlay_sources) vcols <- c(vcols, if (src == "effective") "effective_sec" else paste0(src, "_sec"))
      vcols <- intersect(unique(vcols), names(tb))
      tb_sub <- tb[tb$pattern %in% hist_overlay_patterns & tb$field %in% c("seed_lower_sec", "seed_upper_sec", "bridge_upper_sec") & length(vcols) > 0, , drop = FALSE]
      shown_vals <- suppressWarnings(as.numeric(unlist(tb_sub[, vcols, drop = FALSE])))
      shown_vals <- shown_vals[is.finite(shown_vals) & shown_vals >= 0]
      if (length(shown_vals) > 0) x_max_sec <- max(x_max_sec, shown_vals, na.rm = TRUE)
    }
    br <- seq(0, ceiling(x_max_sec / bin_sec) * bin_sec + bin_sec, by = bin_sec)
    if (length(br) < 2) br <- c(0, bin_sec)
    x_limit <- tail(br, 1)
    all_plot <- all_vals[all_vals >= 0 & all_vals <= x_limit]
    raw_count <- if (length(all_plot) > 0) as.numeric(hist(all_plot, breaks = br, plot = FALSE, include.lowest = TRUE, right = FALSE)$counts) else rep(0, length(br) - 1)
    raw_fraction <- if (length(all_vals) > 0) raw_count / length(all_vals) else raw_count
    mat <- lapply(vals_by_train, function(v) {
      v_all <- v[is.finite(v) & v >= min_valid_isi_sec()]
      if (length(v_all) == 0) return(rep(0, length(br) - 1))
      v_plot <- v_all[v_all >= 0 & v_all <= x_limit]
      cnt <- if (length(v_plot) > 0) as.numeric(hist(v_plot, breaks = br, plot = FALSE, include.lowest = TRUE, right = FALSE)$counts) else rep(0, length(br) - 1)
      cnt / length(v_all)
    })
    mat <- do.call(rbind, mat)
    balanced_fraction <- if (!is.null(mat) && nrow(mat) > 0) colMeans(mat, na.rm = TRUE) else rep(0, length(br) - 1)
    dd <- data.frame(mid = (br[-length(br)] + br[-1]) / 2 * f, bin_left = br[-length(br)] * f, bin_right = br[-1] * f,
                     raw_count = raw_count, raw_fraction = raw_fraction, balanced_fraction = balanced_fraction, stringsAsFactors = FALSE)
    dd$hover <- paste0("ISI bin: [", signif(dd$bin_left, 4), ", ", signif(dd$bin_right, 4), ") ", u,
                       "<br>Raw count: ", dd$raw_count,
                       "<br>Raw fraction: ", signif(dd$raw_fraction * 100, 4), "%",
                       "<br>Train-balanced fraction: ", signif(dd$balanced_fraction * 100, 4), "%")
    mode <- input$dataset_isi_hist_mode %||% "overlay"
    log_y <- isTRUE(input$dataset_isi_hist_log_y)
    p <- plot_ly(); bar_width <- bin_sec * f * 0.90
    if (identical(mode, "raw")) {
      y <- dd$raw_count; if (log_y) y[y <= 0] <- NA_real_
      p <- add_bars(p, data = dd, x = ~mid, y = y, width = bar_width, name = "\u539F\u59CB\u5408\u5E76\u8BA1\u6570", hoverinfo = "text", text = ~hover, textposition = "none", marker = list(color = "rgba(80,80,80,0.70)")); y_title <- "\u539F\u59CB\u5408\u5E76\u8BA1\u6570"
    } else if (identical(mode, "balanced")) {
      y <- dd$balanced_fraction; if (log_y) y[y <= 0] <- NA_real_
      p <- add_bars(p, data = dd, x = ~mid, y = y, width = bar_width, name = "Train-balanced fraction", hoverinfo = "text", text = ~hover, textposition = "none", marker = list(color = "rgba(138,127,255,0.70)")); y_title <- "\u6BCF\u6761 train \u5E73\u5747\u6BD4\u4F8B"
    } else {
      y1 <- dd$raw_fraction; y2 <- dd$balanced_fraction; if (log_y) { y1[y1 <= 0] <- NA_real_; y2[y2 <= 0] <- NA_real_ }
      p <- add_bars(p, data = dd, x = ~mid, y = y1, width = bar_width, name = "Raw pooled fraction", hoverinfo = "text", text = ~hover, textposition = "none", marker = list(color = "rgba(80,80,80,0.50)"))
      p <- add_bars(p, data = dd, x = ~mid, y = y2, width = bar_width, name = "Train-balanced fraction", hoverinfo = "text", text = ~hover, textposition = "none", marker = list(color = "rgba(138,127,255,0.50)")); y_title <- "\u6BD4\u4F8B"
    }
    shapes <- list(); annotations <- list()
    add_line <- function(x_sec, color, label, dash = "dash") {
      if (!is.finite(x_sec) || x_sec < 0) return()
      x <- x_sec * f
      shapes[[length(shapes)+1]] <<- list(type="line", x0=x, x1=x, y0=0, y1=1, xref="x", yref="paper", line=list(color=color, width=2, dash=dash))
      p <<- add_trace(p, x = c(NA_real_, NA_real_), y = c(NA_real_, NA_real_), type = "scatter", mode = "lines",
                       name = label, line = list(color = color, width = 2, dash = dash), hoverinfo = "none", showlegend = TRUE)
    }
    if (hist_show_overlay && !is.null(tb) && nrow(tb) > 0) {
      pats <- hist_overlay_patterns
      srcs <- hist_overlay_sources
      dash_map <- c(effective="solid", user="dash", manual="dot", histogram="longdash")
      alpha_map <- c(effective=0.16, user=0.09, manual=0.10, histogram=0.08)
      for (pat in pats) {
        col <- stpd_threshold_pattern_color(pat, "manual")
        for (src in srcs) {
          colname <- if (src == "effective") "effective_sec" else paste0(src, "_sec")
          seed_lo <- suppressWarnings(as.numeric(tb[tb$pattern==pat & tb$field=="seed_lower_sec", colname][1]))
          seed_hi <- suppressWarnings(as.numeric(tb[tb$pattern==pat & tb$field=="seed_upper_sec", colname][1]))
          bridge <- suppressWarnings(as.numeric(tb[tb$pattern==pat & tb$field=="bridge_upper_sec", colname][1]))
          if (is.finite(seed_lo) && is.finite(seed_hi) && seed_hi > seed_lo) {
            seed_label <- paste0(stpd_threshold_pattern_label(pat), " seed ", src)
            shapes[[length(shapes)+1]] <- list(type="rect", x0=seed_lo*f, x1=seed_hi*f, y0=0, y1=1, xref="x", yref="paper", fillcolor=event_grammar_hex_to_rgba(col, alpha_map[[src]] %||% 0.06), line=list(color=col, width=if (src=="effective") 2 else 1, dash=dash_map[[src]] %||% "dash"))
            p <- add_trace(p, x = c(NA_real_, NA_real_), y = c(NA_real_, NA_real_), type = "scatter", mode = "lines",
                           name = seed_label, line = list(color = col, width = if (src=="effective") 6 else 4, dash = dash_map[[src]] %||% "dash"),
                           hoverinfo = "none", showlegend = TRUE)
          }
          add_line(bridge, col, paste0(stpd_threshold_pattern_label(pat), " bridge ", src), if (src=="effective") "solid" else (dash_map[[src]] %||% "dash"))
        }
      }
    }
    if (isTRUE(input$dataset_isi_hist_show_qc)) {
      add_line(min_valid_isi_sec(), "#666666", "artifact/min valid", "dash")
      add_line(refractory_suspect_sec(), "#999999", "refractory-suspect", "dot")
    }
    yaxis <- list(title = y_title); if (log_y) yaxis$type <- "log"
    p %>% layout(hoverlabel = stpd_hoverlabel_style(), barmode = if (identical(mode, "overlay")) "overlay" else "group",
                 xaxis = list(title = paste0("ISI (", u, ")"), range = c(0, x_limit * f)), yaxis = yaxis,
                 shapes = shapes, annotations = annotations, legend = list(orientation = "h", x = 0, y = 1.12), margin = list(l = 70, r = 20, t = 40, b = 60)) %>% config(displaylogo = FALSE)
  })

  # ----------------------------------------------------------
  # seed-bridge seed / bridge diagnostics
  # ----------------------------------------------------------
  
  # ----------------------------------------------------------
  # structure Structure-candidate diagnostics
  # ----------------------------------------------------------
  structure_tables_current <- reactive({
    ds <- current_dataset()
    pcur <- current_param_for_tables()
    sc <- ds$results$structure_candidates
    if (is.null(sc) || nrow(sc) == 0) {
      target_trains <- intersect(displayed_train_names() %||% names(ds$trains), names(ds$trains))
      if (length(target_trains) == 0) target_trains <- names(ds$trains)
      parts <- list()
      for (tr in target_trains) {
        x <- mine_structure_candidates(ds$trains[[tr]], pcur$burst, min_isi_sec = pcur$detector$min_valid_isi_sec %||% min_valid_isi_sec(), train = tr)
        if (nrow(x) > 0) parts[[length(parts) + 1L]] <- x
      }
      sc <- if (length(parts) > 0) bind_rows(parts) else empty_structure_candidates_tbl()
    }
    if (exists("stpd_normalize_structure_sublabel_columns", mode = "function")) {
      sc <- stpd_normalize_structure_sublabel_columns(sc)
    }
    sc
  })
  
  structure_values_current <- reactive({
    df <- structure_tables_current()
    validate(need(!is.null(df) && nrow(df) > 0, "No structure candidates. Run detector or adjust structure parameters."))
    if (!isTRUE(input$structure_include_reject)) df <- df %>% filter(structure_class != "reject")
    validate(need(nrow(df) > 0, "No structure candidates after current filter."))
    f <- unit_factor()
    u <- unit_label()
    typ <- input$structure_diag_type %||% "core_q_hist"
    if (typ == "core_q_hist") {
      df$value <- df$core_q_ISI_sec * f; unit <- u; label <- "Structure core q90 ISI"
    } else if (typ == "core_q_pct_hist") {
      df$value <- df$core_q_ISI_pct; unit <- "%"; label <- "Structure core q90 percentile within train"
    } else if (typ == "edge_min_hist") {
      df$value <- df$edge_contrast_min_q; unit <- "ratio"; label <- "Structure edge contrast min"
    } else if (typ == "edge_geom_hist") {
      df$value <- df$edge_contrast_geom_q; unit <- "ratio"; label <- "Structure edge contrast geom"
    } else {
      df$value <- df$core_q_ISI_sec * f; unit <- u; label <- "Structure core q90 ISI"
    }
    list(df = df, unit = unit, label = label, type = typ)
  })
  
  output$structure_meta_table <- renderDT({
    sv <- structure_values_current()
    df <- sv$df
    if (nrow(df) == 0) return(datatable(data.frame(message = "No structure candidates."), options = list(dom = "t")))
    summary_df <- df %>%
      group_by(structure_class) %>%
      summarise(
        n = n(),
        core_q_median = median(core_q_ISI_sec, na.rm = TRUE) * unit_factor(),
        edge_min_median = median(edge_contrast_min_q, na.rm = TRUE),
        duration_median = median(duration_sec, na.rm = TRUE) * unit_factor(),
        .groups = "drop"
      )
    datatable(summary_df, rownames = FALSE, options = list(dom = "t"))
  })
  
  output$structure_plot <- renderPlotly({
    sv <- structure_values_current()
    df <- sv$df
    typ <- sv$type
    f <- unit_factor()
    u <- unit_label()
    validate(need(nrow(df) > 0, "No structure candidates to plot."))
    
    if (typ == "core_range") {
      topn <- min(300L, nrow(df))
      dd <- df %>% arrange(core_q_ISI_sec, structure_class, start_time_sec) %>% head(topn)
      dd$yidx <- seq_len(nrow(dd))
      dd$core_min_out <- dd$core_min_ISI_sec * f
      dd$core_max_out <- dd$core_max_ISI_sec * f
      dd$core_med_out <- dd$core_median_ISI_sec * f
      dd$core_q_out <- dd$core_q_ISI_sec * f
      txt <- paste0(
        "Train\uFF1A", dd$train,
        "<br>Structure: ", dd$structure_id,
        "<br>Class: ", dd$structure_class,
        "<br>Time: [", round(dd$start_time_sec, 4), ", ", round(dd$end_time_sec, 4), "] s",
        "<br>Core range: ", round(dd$core_min_out, 3), "-", round(dd$core_max_out, 3), " ", u,
        "<br>Median/q90: ", round(dd$core_med_out, 3), " / ", round(dd$core_q_out, 3), " ", u,
        "<br>Pre/Post ISI\uFF1A", round(dd$pre_ISI_sec * f, 3), " / ", round(dd$post_ISI_sec * f, 3), " ", u,
        "<br>Edge min/geom: ", signif(dd$edge_contrast_min_q, 4), " / ", signif(dd$edge_contrast_geom_q, 4)
      )
      p <- plot_ly()
      p <- add_segments(p, data = dd, x = ~core_min_out, xend = ~core_max_out, y = ~yidx, yend = ~yidx,
                        type = "scatter", mode = "lines", text = txt, hoverinfo = "text", showlegend = FALSE)
      p <- add_markers(p, data = dd, x = ~core_med_out, y = ~yidx, name = "median", text = txt, hoverinfo = "text")
      p <- add_markers(p, data = dd, x = ~core_q_out, y = ~yidx, name = "q90", text = txt, hoverinfo = "text")
      return(layout(p, hoverlabel = stpd_hoverlabel_style(), xaxis = list(title = paste0("Core ISI range (", u, ")")),
                    yaxis = list(title = "Structure candidates sorted by q90", showticklabels = FALSE),
                    margin = list(l = 60, r = 20, t = 40, b = 60)) %>% config(displaylogo = FALSE))
    }
    
    if (typ %in% c("core_q_duration", "core_q_lv")) {
      dd <- df
      dd$core_q_out <- dd$core_q_ISI_sec * f
      yval <- if (typ == "core_q_duration") dd$duration_sec * f else dd$LV
      ylab <- if (typ == "core_q_duration") paste0("Duration (", u, ")") else "LV"
      dd$yval <- yval
      txt <- paste0(
        "Train\uFF1A", dd$train,
        "<br>Structure: ", dd$structure_id,
        "<br>Class: ", dd$structure_class,
        "<br>Time: [", round(dd$start_time_sec, 4), ", ", round(dd$end_time_sec, 4), "] s",
        "<br>Core q90: ", round(dd$core_q_out, 3), " ", u,
        "<br>", ylab, ": ", signif(dd$yval, 4),
        "<br>Edge min/geom: ", signif(dd$edge_contrast_min_q, 4), " / ", signif(dd$edge_contrast_geom_q, 4)
      )
      return(plot_ly(dd, x = ~core_q_out, y = ~yval, type = "scatter", mode = "markers",
                     text = txt, hoverinfo = "text", color = ~structure_class) %>%
               layout(hoverlabel = stpd_hoverlabel_style(), xaxis = list(title = paste0("Core q90 ISI (", u, ")")),
                      yaxis = list(title = ylab),
                      margin = list(l = 60, r = 20, t = 40, b = 60)) %>%
               config(displaylogo = FALSE))
    }
    
    if (typ == "core_q_impact") {
      x <- sort(df$core_q_ISI_sec[is.finite(df$core_q_ISI_sec)] * f)
      validate(need(length(x) > 0, "No finite core q values."))
      dd <- data.frame(cutoff = x, count = seq_along(x))
      txt <- paste0("Cutoff: ", round(dd$cutoff, 3), " ", u, "<br>Accepted structures if q90 <= cutoff: ", dd$count)
      return(plot_ly(dd, x = ~cutoff, y = ~count, type = "scatter", mode = "lines+markers",
                     text = txt, hoverinfo = "text") %>%
               layout(hoverlabel = stpd_hoverlabel_style(), xaxis = list(title = paste0("Core q90 cutoff (", u, ")")),
                      yaxis = list(title = "Cumulative candidate count"),
                      margin = list(l = 60, r = 20, t = 40, b = 60)) %>%
               config(displaylogo = FALSE))
    }
    
    if (typ == "core_isi_weighted_hist") {
      rows <- list()
      for (ii in seq_len(nrow(df))) {
        vals <- suppressWarnings(as.numeric(strsplit(df$core_values_sec[ii], ";", fixed = TRUE)[[1]]))
        vals <- vals[is.finite(vals)]
        if (length(vals) == 0) next
        rows[[length(rows)+1L]] <- data.frame(value = vals * f, weight = rep(1 / length(vals), length(vals)),
                                              train = df$train[ii], structure_id = df$structure_id[ii], structure_class = df$structure_class[ii])
      }
      ddlong <- if (length(rows) > 0) bind_rows(rows) else data.frame(value = numeric(), weight = numeric())
      validate(need(nrow(ddlong) > 0, "No weighted core ISI values."))
      bw <- safe_ui_value(input$structure_bin, 1)
      lo <- min(ddlong$value); hi <- max(ddlong$value)
      if (lo == hi) { lo <- lo - bw; hi <- hi + bw }
      br <- seq(floor(lo / bw) * bw, ceiling(hi / bw) * bw + bw, by = bw)
      counts <- numeric(length(br) - 1L)
      for (ii in seq_len(length(br) - 1L)) {
        inb <- ddlong$value >= br[ii] & ddlong$value < br[ii + 1L]
        counts[ii] <- sum(ddlong$weight[inb], na.rm = TRUE)
      }
      hh <- data.frame(mid = (head(br, -1) + tail(br, -1)) / 2, count = counts)
      return(plot_ly(hh, x = ~mid, y = ~count, type = "bar") %>%
               layout(hoverlabel = stpd_hoverlabel_style(), xaxis = list(title = paste0("Weighted core ISI (", u, ")")),
                      yaxis = list(title = "Weighted count; each structure sums to 1"),
                      margin = list(l = 60, r = 20, t = 40, b = 60)) %>%
               config(displaylogo = FALSE))
    }
    
    # Default histogram.
    bw <- safe_ui_value(input$structure_bin, 1)
    validate(need(is.finite(bw) && bw > 0, "Bin width must be > 0."))
    x <- sv$df$value[is.finite(sv$df$value)]
    validate(need(length(x) > 0, "No finite values."))
    lo <- min(x); hi <- max(x)
    if (lo == hi) { lo <- lo - bw; hi <- hi + bw }
    br <- seq(floor(lo / bw) * bw, ceiling(hi / bw) * bw + bw, by = bw)
    h <- hist(x, breaks = br, plot = FALSE)
    hover <- character(length(br) - 1L)
    for (ii in seq_len(length(br) - 1L)) {
      sub <- df[sv$df$value >= br[ii] & sv$df$value < br[ii + 1L], , drop = FALSE]
      txt <- paste0("[", signif(br[ii], 4), ", ", signif(br[ii + 1L], 4), ") ", sv$unit, "<br>\u8BA1\u6570: ", nrow(sub))
      if (nrow(sub) > 0) {
        hs <- head(sub, 8)
        items <- paste0(hs$train, " | structure ", hs$structure_id, " | ", hs$structure_class,
                        " | q90=", signif(hs$core_q_ISI_sec * f, 4), " ", u,
                        " | [", round(hs$start_time_sec, 3), ", ", round(hs$end_time_sec, 3), "] s")
        more <- if (nrow(sub) > 8) paste0("<br>... +", nrow(sub) - 8, " more") else ""
        txt <- paste0(txt, "<br>", paste(items, collapse = "<br>"), more)
      }
      hover[ii] <- txt
    }
    dd <- data.frame(mid = h$mids, count = h$counts, hover = hover)
    plot_ly(dd, x = ~mid, y = ~count, type = "bar", text = ~hover, hoverinfo = "text") %>%
      layout(hoverlabel = stpd_hoverlabel_style(), xaxis = list(title = paste0(sv$label, " (", sv$unit, ")")),
             yaxis = list(title = "\u8BA1\u6570"),
             margin = list(l = 60, r = 20, t = 40, b = 60)) %>%
      config(displaylogo = FALSE)
  })
  
  output$structure_table <- renderDT({
    df <- structure_tables_current()
    if (!isTRUE(input$structure_include_reject)) df <- df %>% filter(structure_class != "reject")
    if (nrow(df) == 0) return(datatable(data.frame(message = "No structure candidates."), options = list(dom = "t")))
    f <- unit_factor()
    nmax <- safe_int(input$structure_table_n, 100L)
    show <- df %>%
      arrange(desc(structure_score), start_time_sec) %>%
      head(nmax) %>%
      mutate(
        start_time = start_time_sec * f,
        end_time = end_time_sec * f,
        duration = duration_sec * f,
        pre_ISI = pre_ISI_sec * f,
        post_ISI = post_ISI_sec * f,
        core_min_ISI = core_min_ISI_sec * f,
        core_median_ISI = core_median_ISI_sec * f,
        core_q90_ISI = core_q_ISI_sec * f,
        core_max_ISI = core_max_ISI_sec * f
      )
    base_cols <- c(
      "train", "structure_id", "structure_class", "seed_decision",
      "burst_sublabel", "burst_motif_type", "linked_burst_label",
      "linked_burst_start_isi", "linked_burst_end_isi",
      "motif_gap_isi_n", "motif_gap_sec",
      "packet_to_burst_median_ratio", "packet_to_burst_q90_ratio",
      "start_time", "end_time", "duration", "n_spikes", "n_isi",
      "pre_ISI", "post_ISI", "core_min_ISI", "core_median_ISI", "core_q90_ISI", "core_max_ISI",
      "pre_ratio_q", "post_ratio_q", "edge_contrast_min_q", "edge_contrast_geom_q",
      "MM", "LV", "CV", "tonic_like", "manual_hint", "structure_score", "reject_reason"
    )
    show <- show %>% dplyr::select(dplyr::all_of(intersect(base_cols, names(show))))
    datatable(show, rownames = FALSE, options = list(pageLength = 20, scrollX = TRUE))
  })
  
  
  
  diag_tables_current <- reactive({
    ds <- current_dataset()
    seeds <- ds$results$seed_candidates %||% empty_seed_candidates_tbl()
    bridges <- ds$results$bridge_candidates %||% empty_bridge_candidates_tbl()
    candidates <- ds$results$burst_candidates %||% empty_burst_candidates_tbl()
    if ((nrow(seeds) == 0 || nrow(bridges) == 0 || nrow(candidates) == 0) &&
        exists("stpd_seed_bridge_diagnostics_for_dataset", mode = "function")) {
      pcur <- current_param_for_tables()
      target_trains <- intersect(displayed_train_names() %||% names(ds$trains), names(ds$trains))
      if (length(target_trains) == 0) target_trains <- names(ds$trains)
      fallback <- tryCatch(
        stpd_seed_bridge_diagnostics_for_dataset(
          ds, pcur,
          min_isi_sec = pcur$detector$min_valid_isi_sec %||% min_valid_isi_sec(),
          target_trains = target_trains
        ),
        error = function(e) NULL
      )
      if (!is.null(fallback)) {
        if (nrow(seeds) == 0) seeds <- fallback$seeds %||% seeds
        if (nrow(bridges) == 0) bridges <- fallback$bridges %||% bridges
        if (nrow(candidates) == 0) candidates <- fallback$candidates %||% candidates
      }
    }
    list(seeds = seeds, bridges = bridges, candidates = candidates)
  })
  
  diag_values_current <- reactive({
    tb <- diag_tables_current()
    ht <- input$diag_type %||% "bridge_ratio_maxseed"
    f <- unit_factor()
    u <- unit_label()
    include_reject <- isTRUE(input$diag_show_reject)
    
    if (ht %in% c("seed_q", "seed_duration", "seed_edge_min", "seed_edge_geom", "seed_mm", "seed_lv")) {
      df <- tb$seeds
      validate(need(!is.null(df) && nrow(df) > 0, "No structure seed diagnostics yet. Run detector first."))
      if (ht == "seed_q") {
        df$value <- df$q_ISI_sec * f; unit <- u; label <- "Seed q90 ISI"
      } else if (ht == "seed_duration") {
        df$value <- df$duration_sec * f; unit <- u; label <- "Seed duration"
      } else if (ht == "seed_edge_min") {
        df$value <- df$edge_contrast_min_q; unit <- "ratio"; label <- "Seed edge contrast min"
      } else if (ht == "seed_edge_geom") {
        df$value <- df$edge_contrast_geom_q; unit <- "ratio"; label <- "Seed edge contrast geom"
      } else if (ht == "seed_mm") {
        df$value <- df$MM; unit <- "ratio"; label <- "Seed MM"
      } else {
        df$value <- df$LV; unit <- "dimensionless"; label <- "Seed LV"
      }
      df$row_type <- "seed"
    } else if (ht %in% c("bridge_raw", "bridge_ratio_maxseed", "bridge_ratio_geomseed", "bridge_merged_edge_min", "bridge_merged_edge_geom")) {
      df <- tb$bridges
      validate(need(!is.null(df) && nrow(df) > 0, "No structure bridge diagnostics yet. Run detector first."))
      if (!include_reject) df <- df %>% filter(bridge_class != "reject")
      validate(need(nrow(df) > 0, "No bridge rows after current filter."))
      if (ht == "bridge_raw") {
        df$value <- df$bridge_ISI_max_sec * f; unit <- u; label <- "Bridge raw max ISI"
      } else if (ht == "bridge_ratio_maxseed") {
        df$value <- df$bridge_ratio_max_seed_q; unit <- "ratio"; label <- "Bridge / inflated max(seed q90)"
      } else if (ht == "bridge_ratio_geomseed") {
        df$value <- df$bridge_ratio_geom_seed_q; unit <- "ratio"; label <- "Bridge / inflated geom(seed q90)"
      } else if (ht == "bridge_merged_edge_min") {
        df$value <- df$merged_edge_contrast_min_q; unit <- "ratio"; label <- "Bridge merged edge contrast min"
      } else {
        df$value <- df$merged_edge_contrast_geom_q; unit <- "ratio"; label <- "Bridge merged edge contrast geom"
      }
      df$row_type <- "bridge"
    } else {
      df <- tb$candidates
      validate(need(!is.null(df) && nrow(df) > 0, "No structure final candidate diagnostics yet. Run detector first."))
      if (!include_reject) df <- df %>% filter(class != "reject")
      validate(need(nrow(df) > 0, "No candidate rows after current filter."))
      if (ht == "cand_edge_min") {
        df$value <- if ("edge_contrast_min_seed_q" %in% names(df)) df$edge_contrast_min_seed_q else df$edge_contrast_min_q; unit <- "ratio"; label <- "Final candidate seed-core edge contrast min"
      } else if (ht == "cand_score") {
        df$value <- df$score; unit <- "score"; label <- "Final candidate score"
      } else {
        df$value <- df$duration_sec * f; unit <- u; label <- "Final candidate duration"
      }
      df$row_type <- "candidate"
    }
    df <- df %>% filter(is.finite(value))
    list(df = df, unit = unit, label = label)
  })
  
  output$diag_meta_table <- renderDT({
    dv <- diag_values_current()
    df <- dv$df
    if (nrow(df) == 0) return(datatable(data.frame(message = "No finite diagnostic values."), options = list(dom = "t")))
    summary_df <- data.frame(
      row_type = unique(df$row_type)[1],
      n = nrow(df),
      mean = mean(df$value, na.rm = TRUE),
      median = median(df$value, na.rm = TRUE),
      q05 = as.numeric(quantile(df$value, 0.05, na.rm = TRUE)),
      q95 = as.numeric(quantile(df$value, 0.95, na.rm = TRUE)),
      unit = dv$unit
    )
    datatable(summary_df, rownames = FALSE, options = list(dom = "t"))
  })
  
  output$diag_plot <- renderPlotly({
    dv <- diag_values_current()
    df <- dv$df
    validate(need(nrow(df) > 0, "No diagnostic values to plot."))
    bw <- safe_ui_value(input$diag_bin, 0.25)
    validate(need(is.finite(bw) && bw > 0, "Bin width must be > 0."))
    x <- df$value[is.finite(df$value)]
    lo <- min(x); hi <- max(x)
    if (lo == hi) { lo <- lo - bw; hi <- hi + bw }
    br <- seq(floor(lo / bw) * bw, ceiling(hi / bw) * bw + bw, by = bw)
    h <- hist(x, breaks = br, plot = FALSE)
    
    hover <- character(length(br) - 1L)
    for (ii in seq_len(length(br) - 1L)) {
      sub <- df[df$value >= br[ii] & df$value < br[ii + 1L], , drop = FALSE]
      txt <- paste0("[", signif(br[ii], 4), ", ", signif(br[ii + 1L], 4), ") ", dv$unit, "<br>\u8BA1\u6570: ", nrow(sub))
      if (nrow(sub) > 0) {
        hs <- head(sub, 8)
        if (unique(df$row_type)[1] == "seed") {
          items <- paste0(hs$train, " | seed ", hs$seed_id, " | q90=", signif(hs$q_ISI_sec, 4), " s | [", round(hs$start_time_sec, 3), ", ", round(hs$end_time_sec, 3), "] s")
        } else if (unique(df$row_type)[1] == "bridge") {
          items <- paste0(hs$train, " | bridge ", hs$bridge_id, " | class=", hs$bridge_class,
                          " | ratio=", signif(hs$bridge_ratio_max_seed_q, 4),
                          " | L/R seed=", hs$left_seed_id, "/", hs$right_seed_id)
        } else {
          items <- paste0(hs$train, " | cand ", hs$candidate_id, " | class=", hs$class,
                          " | score=", signif(hs$score, 4), " | [", round(hs$start_time_sec, 3), ", ", round(hs$end_time_sec, 3), "] s")
        }
        more <- if (nrow(sub) > 8) paste0("<br>... +", nrow(sub) - 8, " more") else ""
        txt <- paste0(txt, "<br>", paste(items, collapse = "<br>"), more)
      }
      hover[ii] <- txt
    }
    dd <- data.frame(mid = h$mids, count = h$counts, hover = hover)
    plot_ly(dd, x = ~mid, y = ~count, type = "bar", hoverinfo = "text", text = ~hover) %>%
      layout(hoverlabel = stpd_hoverlabel_style(), xaxis = list(title = paste0(dv$label, " (", dv$unit, ")")),
             yaxis = list(title = "\u8BA1\u6570"), margin = list(l = 60, r = 20, t = 35, b = 60)) %>%
      config(displaylogo = FALSE)
  })
  
  output$diag_scatter_plot <- renderPlotly({
    ds <- current_dataset()
    mode <- input$diag_scatter_type %||% "seed_pct_score"
    if (mode == "seed_pct_score") {
      df <- ds$results$seed_candidates
      validate(need(!is.null(df) && nrow(df) > 0, "No seed candidates available."))
      x <- suppressWarnings(as.numeric(df$q_ISI_pct))
      y <- suppressWarnings(as.numeric(df$seed_score))
      keep <- is.finite(x) & is.finite(y)
      validate(need(any(keep), "No finite seed q90 percentile / score pairs."))
      dd <- df[keep, , drop = FALSE]
      dd$x <- x[keep]; dd$y <- y[keep]
      plot_ly(dd, x = ~x, y = ~y, type = "scatter", mode = "markers",
              color = ~seed_source,
              hoverinfo = "text",
              text = ~paste0("Train\uFF1A", train,
                             "<br>Seed: ", seed_id,
                             "<br>\u6765\u6E90\uFF1A", seed_source,
                             "<br>q90 pct: ", round(x, 2), "%",
                             "<br>Score: ", round(y, 4),
                             "<br>ISI\uFF1A", start_isi, "-", end_isi)) %>%
        layout(hoverlabel = stpd_hoverlabel_style(), xaxis = list(title = "Seed q90 ISI percentile within train (%)"),
               yaxis = list(title = "Seed score"),
               margin = list(l = 60, r = 20, t = 30, b = 60)) %>%
        config(displaylogo = FALSE)
    } else if (mode == "bridge_pct_ratio") {
      df <- ds$results$bridge_candidates
      validate(need(!is.null(df) && nrow(df) > 0, "No bridge candidates available."))
      if (!isTRUE(input$diag_show_reject)) df <- df %>% filter(bridge_class != "reject")
      x <- suppressWarnings(as.numeric(df$bridge_ISI_max_pct))
      y <- suppressWarnings(as.numeric(df$bridge_ratio_max_seed_q))
      keep <- is.finite(x) & is.finite(y)
      validate(need(any(keep), "No finite bridge percentile / ratio pairs."))
      dd <- df[keep, , drop = FALSE]
      dd$x <- x[keep]; dd$y <- y[keep]
      plot_ly(dd, x = ~x, y = ~y, type = "scatter", mode = "markers",
              color = ~bridge_class,
              hoverinfo = "text",
              text = ~paste0("Train\uFF1A", train,
                             "<br>Bridge: ", bridge_id,
                             "<br>Class: ", bridge_class,
                             "<br>\u539F\u56E0\uFF1A", bridge_reason,
                             "<br>Bridge pct: ", round(x, 2), "%",
                             "<br>Bridge ratio: ", round(y, 4),
                             "<br>Left/right seed: ", left_seed_id, "/", right_seed_id)) %>%
        layout(hoverlabel = stpd_hoverlabel_style(), xaxis = list(title = "Bridge ISI max percentile within train (%)"),
               yaxis = list(title = "Bridge / inflated max(seed q90)"),
               margin = list(l = 60, r = 20, t = 30, b = 60)) %>%
        config(displaylogo = FALSE)
    } else {
      df <- ds$results$burst_candidates
      validate(need(!is.null(df) && nrow(df) > 0, "No final burst candidates available."))
      if (!isTRUE(input$diag_show_reject)) df <- df %>% filter(class != "reject")
      x <- suppressWarnings(as.numeric(df$edge_contrast_geom_q))
      y <- suppressWarnings(as.numeric(df$score))
      keep <- is.finite(x) & is.finite(y)
      validate(need(any(keep), "No finite final edge contrast / score pairs."))
      dd <- df[keep, , drop = FALSE]
      dd$x <- x[keep]; dd$y <- y[keep]
      plot_ly(dd, x = ~x, y = ~y, type = "scatter", mode = "markers",
              color = ~class,
              hoverinfo = "text",
              text = ~paste0("Train\uFF1A", train,
                             "<br>Candidate: ", candidate_id,
                             "<br>Class: ", class,
                             "<br>Score: ", round(y, 4),
                             "<br>Edge geom: ", round(x, 4),
                             "<br>ISI\uFF1A", start_isi, "-", end_isi,
                             "<br>\u539F\u56E0\uFF1A", reject_reason)) %>%
        layout(hoverlabel = stpd_hoverlabel_style(), xaxis = list(title = "Final candidate edge contrast geom/q"),
               yaxis = list(title = "Final candidate score"),
               margin = list(l = 60, r = 20, t = 30, b = 60)) %>%
        config(displaylogo = FALSE)
    }
  })

  output$diag_table <- renderDT({
    dv <- diag_values_current()
    df <- dv$df
    if (nrow(df) == 0) return(datatable(data.frame(message = "No diagnostic rows."), options = list(dom = "t")))
    show <- df %>% arrange(value) %>% head(500)
    # Convert selected time columns to current display unit while leaving raw seconds columns available in source names.
    f <- unit_factor()
    for (cn in intersect(c("start_time_sec", "end_time_sec", "duration_sec", "q_ISI_sec", "bridge_ISI_max_sec", "merged_duration_sec", "core_q_sec"), names(show))) {
      show[[paste0(sub("_sec$", "", cn), "_", unit_label())]] <- show[[cn]] * f
    }
    datatable(show, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))
  })
  
  output$qc_table <- renderDT({
    ds <- current_dataset()
    # Recompute QC for the current artifact/min-valid-ISI threshold; stored QC may come from an older threshold.
    q <- validate_dataset_quality_impl(ds$trains, min_isi_sec = min_valid_isi_sec(), unit_hint = ds$meta$unit_in %||% "s", refractory_suspect_sec = refractory_suspect_sec(), display_unit = qc_isi_unit())
    if (is.null(q) || nrow(q) == 0) return(datatable(data.frame(message = "No QC rows."), options = list(dom = "t")))
    show <- q
    if (!("firing_rate_Hz" %in% names(show)) && "mean_rate_Hz" %in% names(show)) show$firing_rate_Hz <- show$mean_rate_Hz
    level_rank <- c(error = 1, warning = 2, ok = 3)
    show$warning_rank <- unname(level_rank[as.character(show$warning_level)])
    show$warning_rank[!is.finite(show$warning_rank)] <- 99
    show <- show[order(show$warning_rank, show$train), , drop = FALSE]
    preferred <- c("warning_level", "train", "warning_message", "n_spikes", "duration_sec", "firing_rate_Hz",
                   "qc_time_unit", "raw_min_ISI", "artifact_threshold", "refractory_suspect_threshold",
                   "n_artifact_ISI", "artifact_fraction", "n_refractory_suspect_ISI", "n_valid_ISI", "percentile_status")
    show <- show[, intersect(preferred, names(show)), drop = FALSE]
    if ("warning_level" %in% names(show)) show$warning_level <- toupper(show$warning_level)
    datatable(show, rownames = FALSE,
              options = list(pageLength = 20, scrollX = TRUE))
  })


  output$artifact_isi_details_table <- renderDT({
    ds <- current_dataset()
    details <- artifact_isi_details(ds$trains, min_isi_sec = min_valid_isi_sec(), display_unit = qc_isi_unit())
    if (is.null(details) || nrow(details) == 0) {
      return(datatable(data.frame(message = "No artifact ISIs below the current threshold."), options = list(dom = "t")))
    }
    datatable(details, rownames = FALSE, options = list(pageLength = 20, scrollX = TRUE))
  })

  output$duplicate_timestamp_details_table <- renderDT({
    ds <- current_dataset()
    details <- duplicate_timestamp_details(ds$trains, display_unit = qc_isi_unit())
    if (is.null(details) || nrow(details) == 0) {
      return(datatable(data.frame(message = "\u5F53\u524D\u6570\u636E\u96C6\u4E2D\u6CA1\u6709\u5B8C\u5168\u91CD\u590D timestamp\u3002"), options = list(dom = "t")))
    }
    datatable(details, rownames = FALSE, options = list(pageLength = 20, scrollX = TRUE))
  })


  observeEvent(input$eval_manual_detector, {
    ds <- current_dataset()
    p <- read_params_from_ui()
    target_trains <- intersect(displayed_train_names() %||% names(ds$trains), names(ds$trains))
    if (length(target_trains) == 0) target_trains <- names(ds$trains)
    withProgress(message = "Evaluating detector against MANUAL labels", value = 0.2, {
      rv$manual_detector_eval <- evaluate_detector_against_manual(
        ds, p, selected_trains = target_trains, min_isi_sec = p$detector$min_valid_isi_sec,
        use_learned_ranges = !identical(input$eval_learned_ranges_mode %||% "use", "disable"),
        metric_mode = input$manual_eval_metric_mode %||% "strict_high_confidence"
      )
      incProgress(1)
    })
    if (is.null(rv$manual_detector_eval) || nrow(rv$manual_detector_eval$metrics) == 0) {
      showNotification("No manually labeled valid ISIs found for evaluation.", type = "warning", duration = 6)
    } else {
      showNotification("Manual-vs-detector evaluation finished. Open the Manual-vs-detector report tab.", type = "message", duration = 6)
      updateTabsetPanel(session, "main_tabs", selected = "\u624B\u52A8\u6807\u8BB0 vs \u68C0\u6D4B\u5668\u62A5\u544A")
    }
  })

  output$manual_detector_meta <- renderDT({
    ev <- rv$manual_detector_eval
    meta <- if (is.null(ev) || is.null(ev$meta)) data.frame(message = "No manual-vs-detector evaluation yet.") else ev$meta
    datatable(meta, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
  })

  output$manual_detector_metrics <- renderDT({
    ev <- rv$manual_detector_eval
    datatable(if (is.null(ev) || is.null(ev$metrics)) data.frame() else ev$metrics,
              rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })
  output$manual_detector_confusion <- renderDT({
    ev <- rv$manual_detector_eval
    datatable(if (is.null(ev) || is.null(ev$confusion)) data.frame() else ev$confusion,
              rownames = FALSE, options = list(pageLength = 20, scrollX = TRUE))
  })
  output$manual_detector_events <- renderDT({
    ev <- rv$manual_detector_eval
    datatable(if (is.null(ev) || is.null(ev$events)) data.frame() else ev$events,
              rownames = FALSE, options = list(pageLength = 20, scrollX = TRUE))
  })

  observeEvent(input$run_scientific_validation, {
    ds <- current_dataset()
    p <- read_params_from_ui()
    withProgress(message = "Running scientific validation report", value = 0.1, {
      rv$scientific_validation <- stpd_scientific_validation_report(
        ds, p,
        validation_fraction = input$sci_val_fraction %||% 0.25,
        seed = input$sci_val_seed %||% 1L,
        iou_min = input$sci_val_iou %||% 0.25,
        metric_mode = input$sci_val_metric_mode %||% "strict_high_confidence",
        use_learned_ranges = isTRUE(input$sci_val_use_learned_ranges)
      )
      ds$results$scientific_validation <- rv$scientific_validation
      set_dataset(rv$current_id, ds)
      incProgress(1)
    })
    showNotification("Scientific validation report finished. Open the Scientific validation tab.", type = "message", duration = 6)
    updateTabsetPanel(session, "main_tabs", selected = "\u79D1\u5B66\u9A8C\u8BC1")
  })

  output$scientific_validation_meta <- renderDT({
    r <- rv$scientific_validation
    datatable(if (is.null(r) || is.null(r$meta)) data.frame(message = "No scientific validation report yet.") else r$meta,
              rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
  })
  output$scientific_validation_split <- renderDT({
    r <- rv$scientific_validation
    datatable(if (is.null(r) || is.null(r$split)) data.frame() else r$split,
              rownames = FALSE, options = list(pageLength = 20, scrollX = TRUE))
  })
  output$scientific_validation_metrics <- renderDT({
    r <- rv$scientific_validation
    datatable(if (is.null(r) || is.null(r$validation_metrics)) data.frame() else r$validation_metrics,
              rownames = FALSE, options = list(pageLength = 20, scrollX = TRUE))
  })
  output$scientific_validation_calibration_metrics <- renderDT({
    r <- rv$scientific_validation
    datatable(if (is.null(r) || is.null(r$calibration_metrics)) data.frame() else r$calibration_metrics,
              rownames = FALSE, options = list(pageLength = 20, scrollX = TRUE))
  })
  output$scientific_validation_overfit_report <- renderDT({
    r <- rv$scientific_validation
    datatable(if (is.null(r) || is.null(r$overfit_report)) data.frame() else r$overfit_report,
              rownames = FALSE, options = list(pageLength = 20, scrollX = TRUE))
  })
	  output$scientific_validation_matches <- renderDT({
	    r <- rv$scientific_validation
	    datatable(if (is.null(r) || is.null(r$matches_validation)) data.frame() else r$matches_validation,
	              rownames = FALSE, options = list(pageLength = 20, scrollX = TRUE))
	  })

	  output$parameter_sensitivity_train_selector <- renderUI({
	    ds <- tryCatch(current_dataset(), error = function(e) NULL)
	    if (is.null(ds) || is.null(ds$trains) || length(ds$trains) == 0) {
	      return(tags$div(class = "small-note", "\u8BF7\u5148\u52A0\u8F7D\u6570\u636E\u96C6\u3002"))
	    }
	    choices <- names(ds$trains)
	    selected <- intersect(tryCatch(displayed_train_names(), error = function(e) choices), choices)
	    if (length(selected) == 0) selected <- choices
	    max_tr <- safe_int(input$parameter_sensitivity_max_trains %||% 3L, 3L)
	    selectizeInput(
	      "parameter_sensitivity_trains",
	      "\u7528\u4E8E\u654F\u611F\u6027\u626B\u63CF\u7684 trains",
	      choices = choices,
	      selected = head(selected, max_tr),
	      multiple = TRUE,
	      options = list(placeholder = "\u9009\u62E9\u542B MANUAL \u6807\u7B7E\u7684 train")
	    )
	  })

	  output$parameter_sensitivity_path_selector <- renderUI({
	    schema <- tryCatch(stpd_contract_ui_schema(ui_level = "basic"), error = function(e) data.frame())
	    schema <- schema[as.character(schema$type %||% "") %in% c("numeric", "integer", "logical"), , drop = FALSE]
	    if (is.null(schema) || nrow(schema) == 0) {
	      return(tags$div(class = "small-note", "\u6CA1\u6709\u53EF\u626B\u63CF\u7684 Basic \u53C2\u6570\u3002"))
	    }
	    max_p <- safe_int(input$parameter_sensitivity_max_params %||% 6L, 6L)
	    default_paths <- stpd_basic_sensitivity_paths(max_params = max_p)
	    labels <- paste0(schema$label %||% schema$path, " \u2014 ", schema$path)
	    choices <- stats::setNames(as.character(schema$path), labels)
	    selectizeInput(
	      "parameter_sensitivity_paths",
	      "\u626B\u63CF\u7684 Basic \u53C2\u6570",
	      choices = choices,
	      selected = intersect(default_paths, as.character(schema$path)),
	      multiple = TRUE,
	      options = list(placeholder = "\u9009\u62E9\u8981\u505A\u5C0F\u8303\u56F4 sweep \u7684\u53C2\u6570")
	    )
	  })

	  observeEvent(input$run_parameter_sensitivity_scan, {
	    ds <- current_dataset()
	    p <- read_params_from_ui()
	    target <- intersect(input$parameter_sensitivity_trains %||% character(0), names(ds$trains))
	    if (length(target) == 0) target <- intersect(tryCatch(displayed_train_names(), error = function(e) names(ds$trains)), names(ds$trains))
	    if (length(target) == 0) target <- names(ds$trains)
	    paths <- input$parameter_sensitivity_paths %||% stpd_basic_sensitivity_paths(max_params = input$parameter_sensitivity_max_params %||% 6L)
	    scan_error <- NULL
	    tryCatch(
	      withProgress(message = "Running event-level parameter sensitivity scan", value = 0.1, {
	        rv$parameter_sensitivity <- stpd_parameter_sensitivity_scan(
	          ds, p,
	          selected_trains = target,
	          paths = paths,
	          max_params = input$parameter_sensitivity_max_params %||% 6L,
	          max_trains = input$parameter_sensitivity_max_trains %||% 3L,
	          relative_step = input$parameter_sensitivity_relative_step %||% 0.25,
	          iou_min = input$sci_val_iou %||% 0.25,
	          metric_mode = input$sci_val_metric_mode %||% "strict_high_confidence",
	          use_learned_ranges = isTRUE(input$sci_val_use_learned_ranges),
	          collect_diagnostics = FALSE
	        )
	        incProgress(1)
	      }),
	      error = function(e) {
	        scan_error <<- e
	        NULL
	      }
	    )
	    if (!is.null(scan_error)) {
	      detector_notify_error(scan_error, prefix = "\u53C2\u6570\u654F\u611F\u6027\u626B\u63CF\u5931\u8D25", status = "parameter_sensitivity")
	      return(invisible(NULL))
	    }
	    scan <- rv$parameter_sensitivity
	    changed <- if (!is.null(scan$summary) && nrow(scan$summary) > 0) {
	      sum(suppressWarnings(as.numeric(scan$summary$changed_event_n)) > 0, na.rm = TRUE)
	    } else 0L
	    rv$parameter_sensitivity_status <- paste0(
	      "\u5DF2\u5B8C\u6210\u53C2\u6570\u654F\u611F\u6027\u626B\u63CF\uFF1A",
	      length(scan$paths %||% character(0)), " \u4E2A\u53C2\u6570\uFF0C",
	      length(scan$selected_trains %||% character(0)), " \u6761 train\uFF1B",
	      changed, " \u4E2A\u53D8\u4F53\u4EA7\u751F\u4E8B\u4EF6\u5DEE\u5F02\u3002"
	    )
	    showNotification(rv$parameter_sensitivity_status, type = "message", duration = 8)
	    updateTabsetPanel(session, "main_tabs", selected = "\u79D1\u5B66\u9A8C\u8BC1")
	  })

	  output$parameter_sensitivity_status <- renderText({
	    rv$parameter_sensitivity_status %||% "\u5C1A\u672A\u8FD0\u884C\u4E8B\u4EF6\u7EA7\u53C2\u6570\u654F\u611F\u6027\u626B\u63CF\u3002"
	  })

	  output$parameter_sensitivity_metric_plot <- renderPlotly({
	    scan <- rv$parameter_sensitivity
	    validate(need(!is.null(scan) && !is.null(scan$summary) && nrow(scan$summary) > 0,
	                  "\u8FD0\u884C\u4E8B\u4EF6\u7EA7\u53C2\u6570\u654F\u611F\u6027\u540E\u663E\u793A\u6307\u6807\u66F2\u7EBF\u3002"))
	    dat <- scan$summary
	    dat <- dat[nzchar(as.character(dat$parameter_path %||% "")), , drop = FALSE]
	    metric <- as.character(input$parameter_sensitivity_plot_metric %||% "macro_F1")[1]
	    if (!metric %in% c("macro_F1", "macro_precision", "macro_recall")) metric <- "macro_F1"
	    validate(need(nrow(dat) > 0 && metric %in% names(dat), "\u6CA1\u6709\u53EF\u7ED8\u5236\u7684\u53C2\u6570\u53D8\u4F53\u3002"))
	    dat$variant_numeric <- suppressWarnings(as.numeric(dat$variant_value))
	    dat$metric_value <- suppressWarnings(as.numeric(dat[[metric]]))
	    dat <- dat[is.finite(dat$variant_numeric) & is.finite(dat$metric_value), , drop = FALSE]
	    validate(need(nrow(dat) > 0, "\u654F\u611F\u6027\u7ED3\u679C\u4E2D\u6CA1\u6709\u6709\u9650\u7684\u6570\u503C\u6307\u6807\u3002"))
	    dat$hover_text <- paste0(
	      "\u53C2\u6570\uFF1A", dat$parameter_label,
	      "<br>Path\uFF1A", dat$parameter_path,
	      "<br>\u53D8\u4F53\u503C\uFF1A", dat$variant_value,
	      "<br>", metric, "\uFF1A", signif(dat$metric_value, 4),
	      "<br>\u4E8B\u4EF6\u5DEE\u5F02\uFF1A", dat$changed_event_n
	    )
	    plot_ly(
	      dat,
	      x = ~variant_numeric,
	      y = ~metric_value,
	      color = ~parameter_label,
	      type = "scatter",
	      mode = "lines+markers",
	      hoverinfo = "text",
	      text = ~hover_text
	    ) %>%
	      layout(
	        hoverlabel = stpd_hoverlabel_style(),
	        xaxis = list(title = "\u53C2\u6570\u53D8\u4F53\u503C"),
	        yaxis = list(title = metric, range = c(0, 1)),
	        legend = list(orientation = "h", x = 0, y = -0.25),
	        margin = list(l = 60, r = 20, t = 20, b = 80)
	      ) %>%
	      config(displaylogo = FALSE)
	  })

	  output$parameter_sensitivity_summary_table <- renderDT({
	    scan <- rv$parameter_sensitivity
	    dat <- if (is.null(scan) || is.null(scan$summary) || nrow(scan$summary) == 0) {
	      data.frame(message = "\u8FD0\u884C\u4E8B\u4EF6\u7EA7\u53C2\u6570\u654F\u611F\u6027\u540E\u663E\u793A\u6458\u8981\u3002", stringsAsFactors = FALSE)
	    } else scan$summary
	    datatable(dat, rownames = FALSE, options = list(pageLength = 12, scrollX = TRUE))
	  })

	  output$parameter_sensitivity_metrics_table <- renderDT({
	    scan <- rv$parameter_sensitivity
	    dat <- if (is.null(scan) || is.null(scan$metrics) || nrow(scan$metrics) == 0) data.frame() else scan$metrics
	    datatable(dat, rownames = FALSE, options = list(pageLength = 20, scrollX = TRUE))
	  })

	  output$parameter_sensitivity_matches_table <- renderDT({
	    scan <- rv$parameter_sensitivity
	    dat <- if (is.null(scan) || is.null(scan$matches) || nrow(scan$matches) == 0) data.frame() else scan$matches
	    datatable(dat, rownames = FALSE, options = list(pageLength = 20, scrollX = TRUE))
	  })

	  output$download_parameter_sensitivity_zip <- downloadHandler(
	    filename = function() paste0("Parameter_sensitivity_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip"),
	    content = function(file) {
	      scan <- rv$parameter_sensitivity
	      if (is.null(scan)) stop("\u5C1A\u65E0\u53C2\u6570\u654F\u611F\u6027\u626B\u63CF\u53EF\u5BFC\u51FA\u3002", call. = FALSE)
	      out_dir <- file.path(tempdir(), paste0("parameter_sensitivity_", format(Sys.time(), "%Y%m%d_%H%M%S")))
	      stpd_parameter_sensitivity_export(scan, out_dir)
	      old <- setwd(out_dir)
	      on.exit(setwd(old), add = TRUE)
	      utils::zip(zipfile = file, files = list.files(out_dir))
	    }
	  )

	  observeEvent(input$run_all_datasets, {
    tryCatch({
      validate(need(length(rv$datasets) > 0, "No datasets loaded."))
      p <- read_params_from_ui()
      ids <- names(rv$datasets)
      failures <- list()
      ok_n <- 0L
      withProgress(message = "Batch running detector on all loaded datasets", value = 0, {
        for (ii in seq_along(ids)) {
          id <- ids[ii]
          incProgress(1 / max(1, length(ids)), detail = id)
          ds <- normalize_dataset(rv$datasets[[id]])
          res <- tryCatch(
            stpd_detect(ds, p, selected_trains = names(ds$trains), lock_manual = TRUE, collect_diagnostics = TRUE),
            error = function(e) e
          )
          if (inherits(res, "error")) {
            failures[[id]] <- conditionMessage(res)
            next
          }
          rv$datasets[[id]] <- normalize_dataset(res)
          ok_n <- ok_n + 1L
        }
      })
      if (length(failures) > 0) {
        first <- paste(utils::head(paste0(names(failures), ": ", unlist(failures, use.names = FALSE)), 3L), collapse = " | ")
        rv$batch_status <- paste0(
          "\u6279\u5904\u7406\u5B8C\u6210\u4F46\u6709\u6570\u636E\u96C6\u88AB\u8DF3\u8FC7\uFF1A\u6210\u529F ",
          ok_n, " / ", length(ids), "\uFF1B\u5931\u8D25 ", length(failures), "\u3002", first
        )
        showNotification(stpd_shiny_detector_error_message(simpleError(rv$batch_status), prefix = "\u6279\u5904\u7406\u672A\u5B8C\u5168\u6210\u529F"), type = "error", duration = 15)
        if (any(vapply(failures, function(x) stpd_error_mentions_qc(simpleError(x)), logical(1)))) {
          updateTabsetPanel(session, "main_tabs", selected = "\u6570\u636E QC")
        }
      } else {
        rv$batch_status <- paste0("Last batch run: ", length(ids), " dataset(s), ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
        showNotification(rv$batch_status, type = "message", duration = 7)
      }
    },
    shiny.silent.error = function(e) detector_notify_error(e, prefix = "\u6279\u5904\u7406\u672A\u5B8C\u6210", status = "batch"),
    error = function(e) detector_notify_error(e, prefix = "\u6279\u5904\u7406\u5931\u8D25", status = "batch"))
  })
  output$batch_status <- renderText({ rv$batch_status %||% "\u5C1A\u672A\u8FD0\u884C\u6279\u5904\u7406\u3002" })

  output$download_batch_results_zip <- downloadHandler(
    filename = function() paste0("SpikeTrainDetector_batch_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip"),
    content = function(file) {
      validate(need(length(rv$datasets) > 0, "No datasets loaded."))
      p <- current_param_for_tables()
      root <- file.path(tempdir(), paste0("spike_detector_batch_", format(Sys.time(), "%Y%m%d_%H%M%S")))
      dir.create(root, recursive = TRUE, showWarnings = FALSE)
      for (id in names(rv$datasets)) {
        ds <- normalize_dataset(rv$datasets[[id]])
        safe_id <- gsub("[^A-Za-z0-9_\\-\\.]", "_", id)
        out_dir <- file.path(root, safe_id)
        stpd_export_results(ds, p, out_dir = out_dir, dataset_name = ds$meta$display_name %||% id, time_unit = input$time_unit %||% "ms")
        ev <- evaluate_detector_against_manual(ds, p, selected_trains = names(ds$trains), min_isi_sec = p$detector$min_valid_isi_sec, metric_mode = "strict_high_confidence")
        if (!is.null(ev$meta) && nrow(ev$meta) > 0) write_csv_safe(ev$meta, file.path(out_dir, "Manual_vs_detector_meta.csv"), row.names = FALSE, fileEncoding = "UTF-8")
        if (!is.null(ev$metrics) && nrow(ev$metrics) > 0) write_csv_safe(ev$metrics, file.path(out_dir, "Manual_vs_detector_metrics.csv"), row.names = FALSE, fileEncoding = "UTF-8")
        if (!is.null(ev$confusion) && nrow(ev$confusion) > 0) write_csv_safe(ev$confusion, file.path(out_dir, "Manual_vs_detector_confusion.csv"), row.names = FALSE, fileEncoding = "UTF-8")
        if (!is.null(ev$events) && nrow(ev$events) > 0) write_csv_safe(ev$events, file.path(out_dir, "Manual_vs_detector_events.csv"), row.names = FALSE, fileEncoding = "UTF-8")
      }
      old <- setwd(root); on.exit(setwd(old), add = TRUE)
      utils::zip(zipfile = file, files = list.files(root, recursive = TRUE))
    }
  )

  # ----------------------------------------------------------
  # Events table
  # ----------------------------------------------------------
  events_bundle_current <- reactive({
    ds <- current_dataset()
    p <- current_param_for_tables()
    b <- derive_interval_tables(
      ds$trains,
      source = input$events_view %||% "audit_final",
      auto_others = isTRUE(input$auto_others),
      dataset_map = setNames(rep(ds$meta$display_name, length(ds$trains)), names(ds$trains)),
      min_isi_sec = min_valid_isi_sec(),
      contrast_q = p$burst$contrast_q %||% 0.90,
      context_k = p$burst$context_k %||% 5L
    )
    if ((input$events_view %||% "audit_final") %in% c("final", "audit_final")) {
      run_id <- (ds$results$run_metadata$run_id %||% "ui_view")[1]
      phash <- (ds$results$run_metadata$params_hash %||% compute_params_hash(p))[1]
      b$events <- enrich_events_with_pause_thresholds(b$events, ds$trains, run_id = run_id, params_hash = phash)
    }
    b
  })
  
  stpd_server_install_ml_module(environment())



  output$posthoc_fragment_audit_table <- renderDT({
    ds <- current_dataset()
    pf <- ds$results$posthoc_fragment_audit %||% data.frame()
    if (is.null(pf) || nrow(pf) == 0) {
      return(datatable(data.frame(message = "No post-overlap minimum-size fragments were removed in the latest AUTO run."), options = list(dom = "t")))
    }
    f <- unit_factor()
    pf_show <- pf
    if ("duration_sec" %in% names(pf_show)) pf_show$duration <- suppressWarnings(as.numeric(pf_show$duration_sec)) * f
    if ("required_min_duration_sec" %in% names(pf_show)) pf_show$required_min_duration <- suppressWarnings(as.numeric(pf_show$required_min_duration_sec)) * f
    keep <- intersect(c("train", "pattern", "start_isi", "end_isi", "n_spikes_final", "n_isi_final", "n_valid_isi_final", "required_min_spikes", "required_min_isi", "duration", "required_min_duration", "action", "run_id", "params_hash"), names(pf_show))
    datatable(pf_show[, keep, drop = FALSE], rownames = FALSE, options = list(pageLength = 25, scrollX = TRUE))
  })

  output$events_table <- renderDT({
    eb <- events_bundle_current()
    ev <- eb$events
    if (nrow(ev) == 0) {
      return(datatable(data.frame(message = paste0("No events found for source: ", input$events_view %||% "audit_final")), options = list(dom = "t")))
    }
    f <- unit_factor()
    ev_show <- ev %>%
      mutate(start_time = start_time_sec * f, end_time = end_time_sec * f, duration = duration_sec * f,
             pre_ISI = pre_ISI_sec * f, post_ISI = post_ISI_sec * f,
             context_pre_ISI = context_pre_ISI_sec * f, context_post_ISI = context_post_ISI_sec * f,
             mean_ISI = mean_ISI_sec * f, median_ISI = median_ISI_sec * f,
             min_ISI = min_ISI_sec * f, max_ISI = max_ISI_sec * f, core_q_ISI = core_q_ISI_sec * f) %>%
      arrange(dataset, train, start_time_sec, pattern, event_id) %>%
      select(dataset, train, pattern, event_id,
             start_spike_idx, end_spike_idx, n_spikes, n_isi,
             start_time, end_time, duration,
             pre_ISI, post_ISI, context_pre_ISI, context_post_ISI,
             mean_ISI, median_ISI, min_ISI, max_ISI, core_q_ISI,
             MM, LV, CV, Pre_LV, After_LV,
	             n_flank, n_flank_ctx,
	             contrast_min_q, contrast_geom_q, contrast_pct_q,
	             contrast_min_ctx_q, contrast_geom_ctx_q, contrast_pct_ctx_q,
	             label_source, user_promoted_possible_burst, n_user_promoted_isi,
	             auto_pattern_majority, user_override_reason,
	             auto_score)
    datatable(ev_show, rownames = FALSE, options = list(pageLength = 25, scrollX = TRUE))
  })
  
  stpd_server_install_export_module(environment())

  # ----------------------------------------------------------
  # Support: article-conformant Mean-ISI burst threshold support
  # ----------------------------------------------------------
  compute_misi_support_now <- function() {
    ds <- current_dataset()
    p <- current_param_for_tables()
    target <- if (isTRUE(input$misi_support_visible_only)) {
      intersect(displayed_train_names() %||% names(ds$trains), names(ds$trains))
    } else {
      names(ds$trains)
    }
    max_k <- suppressWarnings(as.integer(input$misi_max_isi_count %||% 0L))
    if (!is.finite(max_k) || max_k <= 0L) max_k <- Inf
    stpd_misi_support_dataset(
      ds,
      params = p,
      selected_trains = target,
      min_valid_isi_sec = p$detector$min_valid_isi_sec %||% min_valid_isi_sec(),
      min_isi_count = as.integer(input$misi_min_isi_count %||% 2L),
      max_isi_count = max_k,
      max_windows = as.integer(input$misi_max_windows %||% 2000000L),
      min_spikes = as.integer(input$misi_min_spikes %||% 3L),
      min_duration_sec = (as.numeric(input$misi_min_duration_ms %||% 0) / 1000),
      overlap_fraction = as.numeric(input$misi_overlap_fraction %||% 0.10)
    )
  }

  misi_support_result <- eventReactive(input$run_misi_support, {
    compute_misi_support_now()
  }, ignoreInit = TRUE)

  output$misi_support_report_table <- DT::renderDT({
    res <- tryCatch(misi_support_result(), error = function(e) NULL)
    if (is.null(res)) return(DT::datatable(data.frame(message = "Run Mean-ISI support to generate report."), options = list(pageLength = 5)))
    df <- res$support_report %||% data.frame()
    if (nrow(df) == 0) return(DT::datatable(data.frame(message = "Run Mean-ISI support to generate report."), options = list(pageLength = 5)))
    factor <- if (identical(input$time_unit %||% "ms", "ms")) 1000 else 1
    unit <- input$time_unit %||% "ms"
    for (nm in intersect(c("threshold_sec", "burst_isi_q50_sec", "burst_isi_q90_sec", "burst_isi_q95_sec", "burst_isi_max_sec", "suggested_burst_max_ISI_sec", "mean_duration_sec"), names(df))) {
      df[[sub("_sec$", paste0("_", unit), nm)]] <- df[[nm]] * factor
    }
    DT::datatable(df, options = list(pageLength = 10, scrollX = TRUE))
  })

  output$misi_threshold_table <- DT::renderDT({
    res <- tryCatch(misi_support_result(), error = function(e) NULL)
    if (is.null(res)) return(DT::datatable(data.frame(message = "Run Mean-ISI support to generate threshold table."), options = list(pageLength = 5)))
    df <- res$thresholds %||% data.frame()
    if (nrow(df) == 0) return(DT::datatable(data.frame(message = "No threshold table yet."), options = list(pageLength = 5)))
    factor <- if (identical(input$time_unit %||% "ms", "ms")) 1000 else 1
    unit <- input$time_unit %||% "ms"
    for (nm in intersect(c("threshold_sec", "mean_isi_sec", "ML_sec"), names(df))) {
      df[[sub("_sec$", paste0("_", unit), nm)]] <- df[[nm]] * factor
    }
    DT::datatable(df, options = list(pageLength = 10, scrollX = TRUE))
  })

  output$misi_burst_table <- DT::renderDT({
    res <- tryCatch(misi_support_result(), error = function(e) NULL)
    if (is.null(res)) return(DT::datatable(data.frame(message = "Run Mean-ISI support to generate burst candidates."), options = list(pageLength = 5)))
    df <- res$bursts %||% data.frame()
    if (nrow(df) == 0) return(DT::datatable(data.frame(message = "No MISI support burst candidates."), options = list(pageLength = 5)))
    factor <- if (identical(input$time_unit %||% "ms", "ms")) 1000 else 1
    unit <- input$time_unit %||% "ms"
    for (nm in intersect(c("duration_sec", "mean_ISI_sec", "median_ISI_sec", "q90_ISI_sec", "q95_ISI_sec", "max_ISI_sec", "min_ISI_sec", "ML_sec", "mean_all_ISI_sec", "pre_ISI_sec", "post_ISI_sec"), names(df))) {
      df[[sub("_sec$", paste0("_", unit), nm)]] <- df[[nm]] * factor
    }
    DT::datatable(df, options = list(pageLength = 12, scrollX = TRUE))
  })

  output$download_misi_support_zip <- downloadHandler(
    filename = function() {
      ds <- current_dataset()
      nm <- gsub("[^A-Za-z0-9_\\-\\.]", "_", ds$meta$display_name %||% "dataset")
      paste0(nm, "_MISI_support_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
    },
    content = function(file) {
      res <- tryCatch(misi_support_result(), error = function(e) NULL)
      if (is.null(res)) res <- compute_misi_support_now()
      out_dir <- file.path(tempdir(), paste0("misi_support_", format(Sys.time(), "%Y%m%d_%H%M%S")))
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
      stpd_misi_support_export(res, out_dir)
      writeLines(c(
        "Mean-ISI burst threshold support layer",
        "This report implements Chen et al.'s mean inter-spike interval method as a support layer.",
        "It estimates ML thresholds and support candidates; it does not write AUTO labels or replace the main detector.",
        "Use suggestions only after reviewing eventness/context/regularity diagnostics."
      ), file.path(out_dir, "README_MISI_support.txt"), useBytes = TRUE)
      old <- setwd(out_dir); on.exit(setwd(old), add = TRUE)
      utils::zip(zipfile = file, files = list.files(out_dir, recursive = TRUE))
    }
  )


  # ----------------------------------------------------------
  # Support: Pasquale logISIH / newBD burst threshold support
  # ----------------------------------------------------------
  compute_logisi_support_now <- function() {
    ds <- current_dataset()
    p <- current_param_for_tables()
    target <- if (isTRUE(input$logisi_support_visible_only)) {
      intersect(displayed_train_names() %||% names(ds$trains), names(ds$trains))
    } else {
      names(ds$trains)
    }
    stpd_logisi_support_dataset(
      ds,
      params = p,
      selected_trains = target,
      min_valid_isi_sec = p$detector$min_valid_isi_sec %||% min_valid_isi_sec(),
      min_num_spikes = as.integer(input$logisi_min_num_spikes %||% 5L),
      void_threshold = as.numeric(input$logisi_void_threshold %||% 0.70),
      intraburst_peak_window_ms = as.numeric(input$logisi_peak_window_ms %||% 100),
      core_reference_sec = as.numeric(input$logisi_core_reference_ms %||% 100) / 1000,
      max_reasonable_threshold_sec = as.numeric(input$logisi_max_reasonable_ms %||% 1000) / 1000,
      fallback_ch = isTRUE(input$logisi_fallback_ch),
      fallback_maxISI_sec = as.numeric(input$logisi_fallback_maxisi_ms %||% 100) / 1000,
      overlap_fraction = as.numeric(input$logisi_overlap_fraction %||% 0.10)
    )
  }

  logisi_support_result <- eventReactive(input$run_logisi_support, {
    compute_logisi_support_now()
  }, ignoreInit = TRUE)

  # ----------------------------------------------------------
  # Support: visual overlay of support-method burst candidates
  # ----------------------------------------------------------
  support_burst_overlay_rows <- function() {
    methods <- as.character(input$support_overlay_methods %||% character(0))
    pieces <- list()

    if ("misi" %in% methods) {
      res <- tryCatch(misi_support_result(), error = function(e) NULL)
      b <- if (!is.null(res) && is.data.frame(res$bursts)) res$bursts else data.frame()
      if (nrow(b) > 0) {
        b$support_method <- "misi"
        b$support_method_label <- "Mean-ISI"
        b$support_threshold_sec <- suppressWarnings(as.numeric(b$ML_sec %||% NA_real_))
        pieces[[length(pieces) + 1L]] <- b
      }
    }

    if ("logisi" %in% methods) {
      res <- tryCatch(logisi_support_result(), error = function(e) NULL)
      b <- if (!is.null(res) && is.data.frame(res$bursts)) res$bursts else data.frame()
      if (nrow(b) > 0) {
        b$support_method <- "logisi"
        b$support_method_label <- "LogISI / newBD"
        b$support_threshold_sec <- suppressWarnings(as.numeric(b$ISIth_sec %||% b$maxISI2_sec %||% NA_real_))
        pieces[[length(pieces) + 1L]] <- b
      }
    }

    if (length(pieces) == 0L) return(data.frame())
    dplyr::bind_rows(pieces)
  }

  output$support_burst_raster_plot <- renderPlotly({
    dat_all <- aligned_data()
    req(nrow(dat_all) > 0)

    f <- unit_factor()
    u <- input$time_unit %||% "ms"
    step <- track_step()
    x_mode <- input$support_overlay_x_axis %||% "aligned"
    sync_window <- isTRUE(input$support_overlay_sync_window)
    win_plot <- input$xrange
    x_use <- rv$view_align_x
    if (is.null(x_use) || length(x_use) != 2 || any(!is.finite(x_use)) || x_use[2] <= x_use[1]) x_use <- win_plot
    draw_sec <- sort(x_use) / f
    pad_sec <- max(0.002, diff(draw_sec) * 0.01)
    draw_sec_pad <- if (sync_window) c(max(0, draw_sec[1] - pad_sec), draw_sec[2] + pad_sec) else c(-Inf, Inf)

    dat_all <- dat_all %>%
      group_by(train) %>%
      arrange(idx, .by_group = TRUE) %>%
      mutate(
        timestamp_first_sec = dplyr::first(timestamp_sec),
        isi_start_align_sec = dplyr::lag(time_align_sec),
        isi_end_align_sec = time_align_sec,
        isi_start_timestamp_sec = dplyr::lag(timestamp_sec),
        isi_end_timestamp_sec = timestamp_sec,
        pattern_auto_chr = as.character(pattern_auto %||% "")
      ) %>%
      ungroup()

    if (identical(x_mode, "timestamp")) {
      dat_plot <- dat_all %>% mutate(
        x_spike_plot = timestamp_sec,
        x_isi_start_plot = isi_start_timestamp_sec,
        x_isi_end_plot = isi_end_timestamp_sec
      )
      x_title <- "Original spike timestamp (s)"
    } else {
      dat_plot <- dat_all %>% mutate(
        x_spike_plot = time_align_sec * f,
        x_isi_start_plot = isi_start_align_sec * f,
        x_isi_end_plot = isi_end_align_sec * f
      )
      x_title <- paste0("\u5BF9\u9F50\u65F6\u95F4\uFF08", u, ")")
    }

    axis_tbl <- dat_plot %>% dplyr::distinct(train, train_label, train_order, y) %>% dplyr::arrange(y)
    validate(need(nrow(axis_tbl) > 0, "No visible spike trains."))
    selected <- as.character(axis_tbl$train)

    dat_view <- dat_plot[dat_plot$train %in% selected &
                           dat_plot$time_align_sec >= draw_sec_pad[1] &
                           dat_plot$time_align_sec <= draw_sec_pad[2], , drop = FALSE]
    isi_view <- dat_plot[dat_plot$train %in% selected &
                           !is.na(dat_plot$isi_start_align_sec) &
                           dat_plot$isi_end_align_sec >= draw_sec_pad[1] &
                           dat_plot$isi_start_align_sec <= draw_sec_pad[2], , drop = FALSE]
    validate(need(nrow(dat_view) > 0 || nrow(isi_view) > 0, "No spikes/ISI in the support overlay window."))

    support_b <- support_burst_overlay_rows()
    validate(need(nrow(support_b) > 0, "No support burst ISI strips yet. Click Run Mean-ISI support and/or Run LogISI support in the left panel, then keep the corresponding Overlay support detections checkbox selected."))
    support_b <- support_b[as.character(support_b$train) %in% selected, , drop = FALSE]
    validate(need(nrow(support_b) > 0, "No support burst ISI strips for the currently visible spike trains. Either run support on more trains by turning off 'Run on currently visible trains only', or select trains that contain support candidates."))

    first_tbl <- dat_plot %>%
      group_by(train) %>%
      arrange(idx, .by_group = TRUE) %>%
      summarise(first_timestamp_sec = dplyr::first(timestamp_sec), .groups = "drop")
    y_map <- stats::setNames(axis_tbl$y, as.character(axis_tbl$train))
    train_label_map <- stats::setNames(axis_tbl$train_label, as.character(axis_tbl$train))
    first_map <- stats::setNames(first_tbl$first_timestamp_sec, as.character(first_tbl$train))

    support_b$train <- as.character(support_b$train)
    support_b$first_timestamp_sec <- suppressWarnings(as.numeric(first_map[support_b$train]))
    support_b$y_base <- suppressWarnings(as.numeric(y_map[support_b$train]))
    support_b$train_label <- as.character(train_label_map[support_b$train])
    support_b$start_time_sec <- suppressWarnings(as.numeric(support_b$start_time_sec))
    support_b$end_time_sec <- suppressWarnings(as.numeric(support_b$end_time_sec))
    support_b$start_align_sec <- support_b$start_time_sec - support_b$first_timestamp_sec
    support_b$end_align_sec <- support_b$end_time_sec - support_b$first_timestamp_sec
    support_b <- support_b[is.finite(support_b$start_time_sec) & is.finite(support_b$end_time_sec) &
                             is.finite(support_b$start_align_sec) & is.finite(support_b$end_align_sec) &
                             is.finite(support_b$y_base) & support_b$end_time_sec >= support_b$start_time_sec, , drop = FALSE]
    if (sync_window) {
      support_b <- support_b[support_b$end_align_sec >= draw_sec_pad[1] & support_b$start_align_sec <= draw_sec_pad[2], , drop = FALSE]
    }
    validate(need(nrow(support_b) > 0, "No support burst ISI strips in the current time window. Uncheck 'Sync with main raster time-window' or zoom the main raster to a window containing support candidates."))

    support_b$method_offset <- ifelse(as.character(support_b$support_method) == "misi", 0.18 * step, -0.18 * step)
    support_b$y_support <- support_b$y_base + support_b$method_offset
    support_b$duration_ms <- suppressWarnings(as.numeric(support_b$duration_sec %||% NA_real_)) * 1000
    support_b$threshold_ms <- suppressWarnings(as.numeric(support_b$support_threshold_sec %||% NA_real_)) * 1000
    support_b$x0_plot <- if (identical(x_mode, "timestamp")) support_b$start_time_sec else support_b$start_align_sec * f
    support_b$x1_plot <- if (identical(x_mode, "timestamp")) support_b$end_time_sec else support_b$end_align_sec * f
    support_b$hover_text <- paste0(
      "Support burst candidate<br>",
      "Method: ", support_b$support_method_label,
      "<br>Train\uFF1A", support_b$train_label,
      "<br>Start timestamp: ", round(support_b$start_time_sec, 6), " s",
      "<br>End timestamp: ", round(support_b$end_time_sec, 6), " s",
      "<br>Start ISI index: ", support_b$start_isi,
      "<br>End ISI index: ", support_b$end_isi,
      "<br>Spikes: ", support_b$n_spikes,
      "<br>Duration: ", round(support_b$duration_ms, 3), " ms",
      "<br>Support threshold: ", round(support_b$threshold_ms, 3), " ms",
      "<br>Status: ", as.character(support_b$threshold_status %||% "")
    )

    # canonical6: draw support detections as ISI-segment strips.
    # Burst candidates are time clusters defined by contiguous ISIs. The default
    # visual encoding therefore marks each included ISI interval between two
    # adjacent spikes, rather than coloring the spike tick itself. A translucent
    # event envelope can be enabled as a guide, but the primary support layer is
    # ISI-anchored.
    support_b$support_event_id <- seq_len(nrow(support_b))
    support_isi <- lapply(seq_len(nrow(support_b)), function(ii) {
      tr <- as.character(support_b$train[ii])
      dtr <- dat_plot[as.character(dat_plot$train) == tr, , drop = FALSE]
      if (nrow(dtr) == 0) return(NULL)

      s_isi <- suppressWarnings(as.integer(support_b$start_isi[ii]))
      e_isi <- suppressWarnings(as.integer(support_b$end_isi[ii]))
      if (!is.finite(s_isi) || !is.finite(e_isi) || e_isi < s_isi) return(NULL)

      # In dat_plot, row idx = j stores the ISI from spike j-1 to spike j.
      # Therefore support ISI k maps to row idx = k + 1.
      dd <- dtr[dtr$idx >= (s_isi + 1L) & dtr$idx <= (e_isi + 1L) &
                  is.finite(dtr$x_isi_start_plot) & is.finite(dtr$x_isi_end_plot), , drop = FALSE]

      if (nrow(dd) == 0) {
        # Fallback for any future support method that reports times but not usable ISI indices.
        st <- suppressWarnings(as.numeric(support_b$start_time_sec[ii]))
        en <- suppressWarnings(as.numeric(support_b$end_time_sec[ii]))
        if (is.finite(st) && is.finite(en) && en >= st) {
          dd <- dtr[!is.na(dtr$isi_start_timestamp_sec) & !is.na(dtr$isi_end_timestamp_sec) &
                      dtr$isi_end_timestamp_sec >= st - 1e-12 &
                      dtr$isi_start_timestamp_sec <= en + 1e-12 &
                      is.finite(dtr$x_isi_start_plot) & is.finite(dtr$x_isi_end_plot), , drop = FALSE]
        }
      }

      if (sync_window && nrow(dd) > 0) {
        dd <- dd[!is.na(dd$isi_start_align_sec) & !is.na(dd$isi_end_align_sec) &
                   dd$isi_end_align_sec >= draw_sec_pad[1] &
                   dd$isi_start_align_sec <= draw_sec_pad[2], , drop = FALSE]
      }
      if (nrow(dd) == 0) return(NULL)

      data.frame(
        support_event_id = support_b$support_event_id[ii],
        support_method = as.character(support_b$support_method[ii]),
        support_method_label = as.character(support_b$support_method_label[ii]),
        train = as.character(dd$train),
        train_label = as.character(dd$train_label),
        isi_index = as.integer(dd$idx) - 1L,
        left_spike_idx = as.integer(dd$idx) - 1L,
        right_spike_idx = as.integer(dd$idx),
        x_isi0 = dd$x_isi_start_plot,
        x_isi1 = dd$x_isi_end_plot,
        y_support = support_b$y_support[ii],
        isi_start_timestamp_sec = dd$isi_start_timestamp_sec,
        isi_end_timestamp_sec = dd$isi_end_timestamp_sec,
        isi_duration_ms = (dd$isi_end_timestamp_sec - dd$isi_start_timestamp_sec) * 1000,
        parent_start_time_sec = support_b$start_time_sec[ii],
        parent_end_time_sec = support_b$end_time_sec[ii],
        parent_start_isi = support_b$start_isi[ii],
        parent_end_isi = support_b$end_isi[ii],
        parent_n_isi = support_b$n_isi[ii],
        parent_n_spikes = support_b$n_spikes[ii],
        parent_duration_ms = support_b$duration_ms[ii],
        parent_threshold_ms = support_b$threshold_ms[ii],
        parent_status = as.character(support_b$threshold_status[ii] %||% ""),
        stringsAsFactors = FALSE
      )
    })
    support_isi <- support_isi[!vapply(support_isi, is.null, logical(1))]
    support_isi <- if (length(support_isi)) dplyr::bind_rows(support_isi) else data.frame()
    validate(need(nrow(support_isi) > 0, "Support candidates exist, but no included ISI intervals could be mapped to the visible spike trains. This usually indicates an index/time mismatch in the support result."))
    if (nrow(support_isi) > 0) {
      support_isi$hover_text <- paste0(
        "Support burst ISI segment<br>",
        "Method: ", support_isi$support_method_label,
        "<br>Train\uFF1A", support_isi$train_label,
        "<br>ISI index: ", support_isi$isi_index,
        "<br>Left spike idx: ", support_isi$left_spike_idx,
        "<br>Right spike idx: ", support_isi$right_spike_idx,
        "<br>Left timestamp: ", round(support_isi$isi_start_timestamp_sec, 6), " s",
        "<br>Right timestamp: ", round(support_isi$isi_end_timestamp_sec, 6), " s",
        "<br>ISI duration: ", round(support_isi$isi_duration_ms, 3), " ms",
        "<br>Parent candidate: ISI ", support_isi$parent_start_isi, "-", support_isi$parent_end_isi,
        "<br>Parent ISIs: ", support_isi$parent_n_isi,
        "<br>Parent spikes: ", support_isi$parent_n_spikes,
        "<br>Parent duration: ", round(support_isi$parent_duration_ms, 3), " ms",
        "<br>Support threshold: ", round(support_isi$parent_threshold_ms, 3), " ms",
        "<br>Status: ", support_isi$parent_status
      )
    }

    y_tickvals <- axis_tbl$y
    y_ticktext <- axis_tbl$train_label
    y_range <- c(min(axis_tbl$y, na.rm = TRUE) - 0.6 * step, max(axis_tbl$y, na.rm = TRUE) + 0.6 * step)
    tick_font_size <- if (nrow(axis_tbl) >= 10) 10 else if (nrow(axis_tbl) >= 8) 11 else 12
    spike_h_eff <- min(as.numeric(input$spike_height %||% 0.6), max(0.10, 0.90 * step))
    if (!is.finite(spike_h_eff) || spike_h_eff <= 0) spike_h_eff <- 0.6
    dat_view$y0 <- dat_view$y - spike_h_eff / 2
    dat_view$y1 <- dat_view$y + spike_h_eff / 2

    visible_spike_n <- nrow(dat_view)
    full_limit <- safe_int(input$plot_max_visible_spikes_full, 50000L)
    draw_base_spikes <- visible_spike_n <= full_limit
    base_df <- if (draw_base_spikes) dat_view else dat_view[0, , drop = FALSE]
    lod_note <- if (!draw_base_spikes) paste0("Large support overlay window: ", visible_spike_n, " visible spikes; base spike ticks are suppressed. Zoom in or reduce the window to show spikes.") else ""

    p <- plot_ly(source = "support_raster")
    if (nrow(base_df) > 0) {
      p <- add_segments(
        p,
        data = base_df,
        x = ~x_spike_plot, xend = ~x_spike_plot,
        y = ~y0, yend = ~y1,
        type = "scatter", mode = "lines",
        line = list(width = base_spike_line_width(), color = "#000000", dash = "solid"),
        hoverinfo = "text",
        text = ~paste0(
          "Train\uFF1A", train_label,
          "<br>Spike \u7D22\u5F15\uFF1A", idx,
          "<br>Timestamp\uFF1A", round(timestamp_sec, 6), " s",
          "<br>Aligned time: ", round(time_align_sec * f, 6), " ", u
        ),
        name = "Spike", showlegend = FALSE,
        inherit = FALSE
      )
    }

    if (isTRUE(input$support_overlay_show_auto_burst_family) && nrow(isi_view) > 0) {
      for (pat in c("burst", "long_burst", "possible_burst")) {
        sub_pat <- isi_view[as.character(isi_view$pattern_auto_chr) == pat, , drop = FALSE]
        if (nrow(sub_pat) == 0) next
        st <- pattern_strip_style(pat, source = "auto")
        p <- add_segments(
          p,
          data = sub_pat,
          x = ~x_isi_start_plot, xend = ~x_isi_end_plot,
          y = ~y, yend = ~y,
          type = "scatter", mode = "lines",
          line = list(width = pattern_strip_line_width(), color = st$color, dash = st$dash),
          hoverinfo = "text",
          text = ~paste0(
            "Current AUTO burst-family label<br>",
            "\u6A21\u5F0F\uFF1A", pattern_auto_chr,
            "<br>Train\uFF1A", train_label,
            "<br>Left timestamp: ", round(isi_start_timestamp_sec, 6), " s",
            "<br>Right timestamp: ", round(isi_end_timestamp_sec, 6), " s"
          ),
          name = paste0("AUTO ", pat), showlegend = FALSE,
          inherit = FALSE
        )
      }
    }

    method_levels <- c("misi", "logisi")
    method_names <- c(misi = "Mean-ISI support ISI strip (#8A7FFF)", logisi = "LogISI / newBD support ISI strip (#F58E90)")
    method_span_names <- c(misi = "Mean-ISI translucent event envelope", logisi = "LogISI / newBD translucent event envelope")
    method_colors <- c(misi = "#8A7FFF", logisi = "#F58E90")

    if (isTRUE(input$support_overlay_show_span_strips)) {
      for (mm in method_levels) {
        sub_m <- support_b[as.character(support_b$support_method) == mm, , drop = FALSE]
        if (nrow(sub_m) == 0) next
        p <- add_segments(
          p,
          data = sub_m,
          x = ~x0_plot, xend = ~x1_plot,
          y = ~y_support, yend = ~y_support,
          type = "scatter", mode = "lines",
          line = list(width = max(1, pattern_strip_line_width() * 0.55), color = method_colors[[mm]], dash = "solid"),
          opacity = 0.35,
          hoverinfo = "text",
          text = ~hover_text,
          name = method_span_names[[mm]],
          showlegend = FALSE,
          legendgroup = paste0(mm, "_span"),
          inherit = FALSE
        )
      }
    }

    if (nrow(support_isi) > 0) {
      for (mm in method_levels) {
        sub_c <- support_isi[as.character(support_isi$support_method) == mm, , drop = FALSE]
        if (nrow(sub_c) == 0) next
        p <- add_segments(
          p,
          data = sub_c,
          x = ~x_isi0, xend = ~x_isi1,
          y = ~y_support, yend = ~y_support,
          type = "scatter", mode = "lines",
          line = list(width = pattern_strip_line_width(), color = method_colors[[mm]], dash = "solid"),
          hoverinfo = "text",
          text = ~hover_text,
          name = method_names[[mm]],
          showlegend = TRUE,
          legendgroup = mm,
          inherit = FALSE
        )
      }
    }

    x_values <- c(dat_view$x_spike_plot, support_b$x0_plot, support_b$x1_plot, support_isi$x_isi0, support_isi$x_isi1)
    x_values <- x_values[is.finite(x_values)]
    x_range <- NULL
    if (identical(x_mode, "aligned") && sync_window) {
      x_range <- x_use
    } else if (length(x_values) >= 2L) {
      x_range <- range(x_values, na.rm = TRUE)
      if (is.finite(diff(x_range)) && diff(x_range) == 0) x_range <- x_range + c(-0.001, 0.001)
    }

    annotations <- NULL
    if (nzchar(lod_note)) {
      annotations <- list(list(xref = "paper", yref = "paper", x = 0.01, y = 1.04,
                               text = lod_note, showarrow = FALSE, xanchor = "left", font = list(size = 11)))
    }

    p <- layout(
      p,
      hoverlabel = stpd_hoverlabel_style(),
      showlegend = TRUE,
      legend = list(orientation = "h", x = 0, y = 1.08),
      xaxis = list(title = x_title, range = x_range, exponentformat = "none", separatethousands = FALSE),
      yaxis = list(title = list(text = "Spike train\uFF08\u8BB0\u5F55\u6761\u76EE\uFF09", standoff = 40), tickmode = "array",
                   tickvals = y_tickvals, ticktext = y_ticktext, tickfont = list(size = tick_font_size),
                   range = y_range, zeroline = FALSE, automargin = TRUE),
      margin = list(l = 220, r = 20, t = 60, b = 50),
      hovermode = "closest",
      annotations = annotations
    )
    config(p, displaylogo = FALSE)
  })

  output$logisi_support_report_table <- DT::renderDT({
    res <- tryCatch(logisi_support_result(), error = function(e) NULL)
    if (is.null(res)) return(DT::datatable(data.frame(message = "Run LogISI support to generate report."), options = list(pageLength = 5)))
    df <- res$support_report %||% data.frame()
    if (nrow(df) == 0) return(DT::datatable(data.frame(message = "Run LogISI support to generate report."), options = list(pageLength = 5)))
    factor <- if (identical(input$time_unit %||% "ms", "ms")) 1000 else 1
    unit <- input$time_unit %||% "ms"
    for (nm in intersect(c("threshold_sec", "burst_isi_q50_sec", "burst_isi_q90_sec", "burst_isi_q95_sec", "burst_isi_max_sec", "suggested_burst_max_ISI_sec", "mean_duration_sec"), names(df))) {
      df[[sub("_sec$", paste0("_", unit), nm)]] <- df[[nm]] * factor
    }
    DT::datatable(df, options = list(pageLength = 10, scrollX = TRUE))
  })

  output$logisi_threshold_table <- DT::renderDT({
    res <- tryCatch(logisi_support_result(), error = function(e) NULL)
    if (is.null(res)) return(DT::datatable(data.frame(message = "Run LogISI support to generate threshold table."), options = list(pageLength = 5)))
    df <- res$thresholds %||% data.frame()
    if (nrow(df) == 0) return(DT::datatable(data.frame(message = "No LogISI threshold table yet."), options = list(pageLength = 5)))
    factor <- if (identical(input$time_unit %||% "ms", "ms")) 1000 else 1
    unit <- input$time_unit %||% "ms"
    for (nm in intersect(c("threshold_sec"), names(df))) {
      df[[sub("_sec$", paste0("_", unit), nm)]] <- df[[nm]] * factor
    }
    DT::datatable(df, options = list(pageLength = 10, scrollX = TRUE))
  })

  output$logisi_burst_table <- DT::renderDT({
    res <- tryCatch(logisi_support_result(), error = function(e) NULL)
    if (is.null(res)) return(DT::datatable(data.frame(message = "Run LogISI support to generate burst candidates."), options = list(pageLength = 5)))
    df <- res$bursts %||% data.frame()
    if (nrow(df) == 0) return(DT::datatable(data.frame(message = "No LogISI support burst candidates."), options = list(pageLength = 5)))
    factor <- if (identical(input$time_unit %||% "ms", "ms")) 1000 else 1
    unit <- input$time_unit %||% "ms"
    for (nm in intersect(c("duration_sec", "mean_ISI_sec", "median_ISI_sec", "q90_ISI_sec", "q95_ISI_sec", "max_ISI_sec", "min_ISI_sec", "pre_ISI_sec", "post_ISI_sec", "ISIth_sec", "maxISI1_sec", "maxISI2_sec"), names(df))) {
      df[[sub("_sec$", paste0("_", unit), nm)]] <- df[[nm]] * factor
    }
    DT::datatable(df, options = list(pageLength = 12, scrollX = TRUE))
  })

  output$logisi_hist_table <- DT::renderDT({
    res <- tryCatch(logisi_support_result(), error = function(e) NULL)
    if (is.null(res)) return(DT::datatable(data.frame(message = "Run LogISI support to generate logISIH table."), options = list(pageLength = 5)))
    df <- res$logisih %||% data.frame()
    if (nrow(df) == 0) return(DT::datatable(data.frame(message = "No logISIH table available."), options = list(pageLength = 5)))
    DT::datatable(df, options = list(pageLength = 12, scrollX = TRUE))
  })

  output$download_logisi_support_zip <- downloadHandler(
    filename = function() {
      ds <- current_dataset()
      nm <- gsub("[^A-Za-z0-9_\\-\\.]", "_", ds$meta$display_name %||% "dataset")
      paste0(nm, "_LogISI_support_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
    },
    content = function(file) {
      res <- tryCatch(logisi_support_result(), error = function(e) NULL)
      if (is.null(res)) res <- compute_logisi_support_now()
      out_dir <- file.path(tempdir(), paste0("logisi_support_", format(Sys.time(), "%Y%m%d_%H%M%S")))
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
      stpd_logisi_support_export(res, out_dir)
      writeLines(c(
        "Pasquale logISIH / newBD burst threshold support layer",
        "This report implements the logISIH threshold estimation and newBD support logic described by Pasquale, Martinoia and Chiappalone.",
        "It estimates ISIth thresholds and support candidates; it does not write AUTO labels or replace the main detector.",
        "Use suggestions only after reviewing eventness/context/regularity diagnostics."
      ), file.path(out_dir, "README_LogISI_support.txt"), useBytes = TRUE)
      old <- setwd(out_dir); on.exit(setwd(old), add = TRUE)
      utils::zip(zipfile = file, files = list.files(out_dir, recursive = TRUE))
    }
  )

}
