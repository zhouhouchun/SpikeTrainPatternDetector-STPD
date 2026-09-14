# Interactive multi-method patrol reviewer -----------------------------------

stpd_patrol_reviewer_palette <- function() c(
  burst = "#E68600", pause = "#1676B8", broad_hfs = "#7A3DB8",
  tonic = "#009E73", other = "#64748B"
)

stpd_patrol_diff_to_canonical_isi <- function(diff_index) {
  as.integer(diff_index) + 1L
}

stpd_patrol_annotation_geometry <- function(spikes, start_isi, end_isi) {
  spikes <- as.numeric(spikes)
  start_isi <- as.integer(start_isi)
  end_isi <- as.integer(end_isi)
  if (length(spikes) < 2L || length(start_isi) != 1L || length(end_isi) != 1L ||
      !is.finite(start_isi) || !is.finite(end_isi) || start_isi < 2L ||
      end_isi < start_isi || end_isi > length(spikes)) {
    stop("Invalid canonical train-row ISI interval.", call. = FALSE)
  }
  list(
    start_isi = start_isi, end_isi = end_isi,
    start_spike = start_isi - 1L, end_spike = end_isi,
    start_time_sec = spikes[[start_isi - 1L]], end_time_sec = spikes[[end_isi]]
  )
}

stpd_patrol_annotation_record <- function(
    bundle_sha256, region, train_key, cluster_id, decision, assigned_label,
    spikes, start_isi, end_isi, reviewer, note = "", created_at_utc = NULL) {
  if (is.null(created_at_utc)) {
    created_at_utc <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  }
  if (identical(decision, "reject_as_other")) assigned_label <- "other"
  if (identical(decision, "uncertain")) assigned_label <- "uncertain"
  track <- if (assigned_label == "pause") "gap" else if (
    assigned_label %in% c("broad_hfs", "tonic")) "state" else "event"
  geometry <- stpd_patrol_annotation_geometry(spikes, start_isi, end_isi)
  out <- data.frame(
    action_id = "", previous_action_id = "", transaction_id = "",
    bundle_sha256 = as.character(bundle_sha256), region = as.character(region),
    train_key = as.character(train_key), cluster_id = as.character(cluster_id),
    decision = as.character(decision), assigned_label = as.character(assigned_label),
    semantic_track = track, start_isi = geometry$start_isi,
    end_isi = geometry$end_isi, start_spike = geometry$start_spike,
    end_spike = geometry$end_spike, start_time_sec = geometry$start_time_sec,
    end_time_sec = geometry$end_time_sec, reviewer = trimws(as.character(reviewer)),
    note = trimws(as.character(note)), created_at_utc = as.character(created_at_utc),
    stringsAsFactors = FALSE
  )
  stpd_patrol_annotation_validate(out, verify_hash = FALSE, verify_chain = FALSE)
}

stpd_patrol_annotation_new_id <- function(bundle_sha256, region, train_key,
                                           start_isi, end_isi, label, nonce = NULL) {
  if (is.null(nonce)) nonce <- paste(format(Sys.time(), tz = "UTC", usetz = TRUE), runif(1))
  paste0("manual_", substr(stpd_patrol_hash("stpd-patrol-manual-interval-v1", c(
    bundle_sha256, region, train_key, start_isi, end_isi, label, nonce
  )), 1L, 24L))
}

stpd_patrol_annotation_undo_records <- function(history, reviewer,
                                                 created_at_utc = NULL) {
  history <- stpd_patrol_annotation_validate(history, verify_hash = TRUE)
  if (!nrow(history)) return(stpd_patrol_annotation_empty())
  if (is.null(created_at_utc)) {
    created_at_utc <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  }
  last_transaction <- tail(history$transaction_id, 1L)
  in_transaction <- history$transaction_id == last_transaction
  before <- history[seq_len(which(in_transaction)[[1L]] - 1L), , drop = FALSE]
  prior <- stpd_patrol_annotation_current(before)
  affected <- unique(history$cluster_id[in_transaction])
  rows <- lapply(affected, function(id) {
    old <- prior[prior$cluster_id == id, , drop = FALSE]
    latest <- tail(history[in_transaction & history$cluster_id == id, , drop = FALSE], 1L)
    if (nrow(old)) {
      out <- old
      out$action_id <- ""; out$previous_action_id <- ""; out$transaction_id <- ""
      out$reviewer <- reviewer
      out$note <- paste0("undo transaction ", last_transaction,
                         if (nzchar(old$note[[1L]])) paste0("; restored: ", old$note[[1L]]) else "")
      out$created_at_utc <- created_at_utc
      out
    } else {
      out <- latest
      out$action_id <- ""; out$previous_action_id <- ""; out$transaction_id <- ""
      out$decision <- "reject_as_other"; out$assigned_label <- "other"
      out$semantic_track <- "event"; out$reviewer <- reviewer
      out$note <- paste0("undo transaction ", last_transaction, "; removed new interval")
      out$created_at_utc <- created_at_utc
      out
    }
  })
  do.call(rbind, rows)
}

stpd_patrol_annotation_empty <- function() {
  data.frame(
    action_id = character(), previous_action_id = character(),
    transaction_id = character(),
    bundle_sha256 = character(), region = character(), train_key = character(),
    cluster_id = character(), decision = character(), assigned_label = character(),
    semantic_track = character(), start_isi = integer(), end_isi = integer(),
    start_spike = integer(), end_spike = integer(), start_time_sec = numeric(),
    end_time_sec = numeric(), reviewer = character(), note = character(),
    created_at_utc = character(), stringsAsFactors = FALSE
  )
}

stpd_patrol_annotation_action_hash <- function(record) {
  stpd_patrol_hash("stpd-patrol-annotation-v1", c(
    record$previous_action_id, record$transaction_id,
    record$bundle_sha256, record$region,
    record$train_key, record$cluster_id, record$decision,
    record$assigned_label, record$start_isi, record$end_isi,
    record$reviewer, record$note, record$created_at_utc
  ))
}

stpd_patrol_annotation_validate <- function(out, verify_hash = TRUE,
                                             verify_chain = TRUE) {
  prototype <- stpd_patrol_annotation_empty()
  if (!is.data.frame(out)) stop("Annotation history must be a data.frame.", call. = FALSE)
  missing <- setdiff(names(prototype), names(out))
  if (length(missing)) {
    stop("Patrol annotation history is missing: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  out <- out[, names(prototype), drop = FALSE]
  for (field in names(prototype)[vapply(prototype, is.character, logical(1))]) {
    out[[field]] <- as.character(out[[field]])
    out[[field]][is.na(out[[field]])] <- ""
  }
  for (field in c("start_isi", "end_isi", "start_spike", "end_spike")) {
    out[[field]] <- as.integer(out[[field]])
  }
  for (field in c("start_time_sec", "end_time_sec")) {
    out[[field]] <- as.numeric(out[[field]])
  }
  if (!nrow(out)) return(out)
  if (anyDuplicated(out$action_id)) {
    stop("Patrol annotation action_id values must be unique.", call. = FALSE)
  }
  allowed_decisions <- c("accept_as_label", "reject_as_other", "uncertain")
  allowed_labels <- c("burst", "pause", "broad_hfs", "tonic", "other", "uncertain")
  expected_track <- ifelse(out$assigned_label == "pause", "gap",
    ifelse(out$assigned_label %in% c("broad_hfs", "tonic"), "state", "event"))
  required_text <- c("bundle_sha256", "region", "train_key", "cluster_id",
                     "decision", "assigned_label", "semantic_track", "reviewer",
                     "created_at_utc")
  if (isTRUE(verify_hash)) required_text <- c("action_id", "transaction_id", required_text)
  if (any(!nzchar(trimws(unlist(out[required_text], use.names = FALSE))))) {
    stop("Patrol annotation history has empty required text.", call. = FALSE)
  }
  if (any(!grepl("^[0-9a-f]{64}$", out$bundle_sha256)) ||
      any(!out$decision %in% allowed_decisions) ||
      any(!out$assigned_label %in% allowed_labels) ||
      any(out$semantic_track != expected_track) ||
      any(out$decision == "reject_as_other" & out$assigned_label != "other") ||
      any(out$decision == "uncertain" & out$assigned_label != "uncertain")) {
    stop("Patrol annotation history has an invalid decision, label, or track.",
         call. = FALSE)
  }
  if (any(!is.finite(out$start_isi)) || any(!is.finite(out$end_isi)) ||
      any(out$start_isi < 2L) || any(out$end_isi < out$start_isi) ||
      any(out$start_spike != out$start_isi - 1L) ||
      any(out$end_spike != out$end_isi) ||
      any(!is.finite(out$start_time_sec)) || any(!is.finite(out$end_time_sec)) ||
      any(out$end_time_sec < out$start_time_sec)) {
    stop("Patrol annotation history has invalid closed interval geometry.",
         call. = FALSE)
  }
  if (isTRUE(verify_chain)) {
    expected_previous <- c("", head(out$action_id, -1L))
    if (!identical(out$previous_action_id, expected_previous)) {
      stop("Patrol annotation history hash chain is discontinuous.", call. = FALSE)
    }
  }
  if (isTRUE(verify_hash)) {
    expected_id <- vapply(seq_len(nrow(out)), function(i) {
      stpd_patrol_annotation_action_hash(out[i, , drop = FALSE])
    }, character(1))
    if (!identical(out$action_id, expected_id)) {
      stop("Patrol annotation history hash verification failed.", call. = FALSE)
    }
  }
  out
}

stpd_patrol_annotation_read <- function(path, verify_chain = TRUE) {
  prototype <- stpd_patrol_annotation_empty()
  if (!file.exists(path)) return(prototype)
  out <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  stpd_patrol_annotation_validate(
    out, verify_hash = TRUE, verify_chain = verify_chain
  )
}

stpd_patrol_annotation_current <- function(history) {
  if (!nrow(history)) return(history)
  key <- paste(history$bundle_sha256, history$region, history$train_key,
               history$cluster_id, sep = "\034")
  history[!duplicated(key, fromLast = TRUE), , drop = FALSE]
}

stpd_patrol_annotation_active <- function(history) {
  current <- stpd_patrol_annotation_current(history)
  current[current$decision == "accept_as_label" &
            !current$assigned_label %in% c("other", "uncertain"), , drop = FALSE]
}

stpd_patrol_annotation_atomic_csv <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(pattern = paste0(basename(path), "."), tmpdir = dirname(path))
  on.exit(unlink(tmp), add = TRUE)
  utils::write.csv(x, tmp, row.names = FALSE, na = "")
  if (!file.rename(tmp, path)) {
    stop("Could not atomically write patrol annotation file: ", path, call. = FALSE)
  }
  invisible(path)
}

stpd_patrol_annotation_append_many <- function(history_path, current_path, records,
                                                transaction_id = NULL) {
  prototype <- stpd_patrol_annotation_empty()
  if (!is.data.frame(records) || !nrow(records) ||
      !identical(names(records), names(prototype))) {
    stop("Annotation records must use the patrol annotation schema.",
         call. = FALSE)
  }
  history <- stpd_patrol_annotation_read(history_path)
  previous <- if (nrow(history)) tail(history$action_id, 1L) else ""
  if (is.null(transaction_id) || !nzchar(transaction_id)) {
    transaction_id <- stpd_patrol_hash("stpd-patrol-transaction-v1", c(
      previous, format(Sys.time(), tz = "UTC", usetz = TRUE),
      records$region, records$train_key, records$cluster_id,
      records$decision, records$assigned_label, records$start_isi, records$end_isi
    ))
  }
  for (i in seq_len(nrow(records))) {
    records$transaction_id[[i]] <- transaction_id
    records$previous_action_id[[i]] <- previous
    records$action_id[[i]] <- stpd_patrol_annotation_action_hash(
      records[i, , drop = FALSE]
    )
    previous <- records$action_id[[i]]
  }
  history <- stpd_patrol_annotation_validate(
    rbind(history, records), verify_hash = TRUE
  )
  stpd_patrol_annotation_atomic_csv(history, history_path)
  stpd_patrol_annotation_atomic_csv(
    stpd_patrol_annotation_current(history), current_path
  )
  history
}

stpd_patrol_annotation_append <- function(history_path, current_path, record) {
  stpd_patrol_annotation_append_many(history_path, current_path, record)
}

stpd_patrol_reviewer_validate_bundle <- function(bundle) {
  if (!is.list(bundle) || !length(bundle$regions)) {
    stop("Reviewer bundle must contain a non-empty regions list.", call. = FALSE)
  }
  for (region in names(bundle$regions)) {
    x <- bundle$regions[[region]]
    if (!is.list(x$spikes) || !inherits(x$patrol, "stpd_patrol_report") ||
        !is.data.frame(x$truth_segments)) {
      stop("Invalid reviewer bundle region: ", region, call. = FALSE)
    }
    missing <- setdiff(c("train_key", "proposed_label", "start_isi", "end_isi",
                         "start_time_sec", "end_time_sec"), names(x$truth_segments))
    if (length(missing)) {
      stop("truth_segments for ", region, " is missing: ",
           paste(missing, collapse = ", "), call. = FALSE)
    }
  }
  bundle
}

stpd_patrol_reviewer_truth_empty <- function() {
  data.frame(
    train_key = character(), proposed_label = character(),
    start_isi = integer(), end_isi = integer(), start_time_sec = numeric(),
    end_time_sec = numeric(), stringsAsFactors = FALSE
  )
}

stpd_patrol_reviewer_hydrate_event_geometry <- function(events, spikes) {
  if (!is.data.frame(events) || !nrow(events)) return(events)
  for (i in seq_len(nrow(events))) {
    train <- as.character(events$train[[i]])
    train_spikes <- as.numeric(spikes[[train]])
    if (!length(train_spikes)) next
    geometry <- tryCatch(
      stpd_patrol_annotation_geometry(
        train_spikes, as.integer(events$start_isi[[i]]),
        as.integer(events$end_isi[[i]])
      ), error = function(e) NULL
    )
    if (is.null(geometry)) next
    for (field in c("start_spike", "end_spike", "start_time_sec", "end_time_sec")) {
      if (!field %in% names(events)) events[[field]] <- NA_real_
      if (!is.finite(suppressWarnings(as.numeric(events[[field]][[i]])))) {
        events[[field]][[i]] <- geometry[[field]]
      }
    }
  }
  events
}

#' Build a patrol reviewer bundle from a current STPD dataset
#'
#' This helper runs the auxiliary support methods and packages their evidence,
#' the current label-blind STPD prediction, raw timestamps, and any available
#' manual reference intervals for use by the integrated patrol reviewer.
#' @export
stpd_patrol_reviewer_bundle_from_dataset <- function(
    ds, params = default_params_sec(), region_label = NULL,
    selected_trains = NULL,
    methods = c("mean_isi", "logisi_newbd", "poisson_surprise",
                "robust_gaussian_surprise"), strict = FALSE) {
  if (!is.list(ds) || !is.list(ds$trains) || !length(ds$trains)) {
    stop("A non-empty STPD dataset is required.", call. = FALSE)
  }
  selected <- selected_trains %||% names(ds$trains)
  selected <- intersect(as.character(selected), names(ds$trains))
  if (!length(selected)) stop("No selected spike trains are available.", call. = FALSE)
  spikes <- lapply(ds$trains[selected], function(x) {
    out <- suppressWarnings(as.numeric(x$timestamp_sec))
    out[is.finite(out)]
  })
  if (any(vapply(spikes, length, integer(1)) < 2L)) {
    stop("Every selected spike train must contain at least two timestamps.",
         call. = FALSE)
  }

  native <- tryCatch(
    as.data.frame(stpd_predicted_events(
      ds, params = params, selected_trains = selected,
      metric_mode = "candidate_family", prediction_source = "auto"
    )), error = function(e) data.frame()
  )
  if (nrow(native)) native <- stpd_patrol_reviewer_hydrate_event_geometry(native, spikes)
  patrol <- stpd_run_multi_method_patrol(
    ds, params = params, selected_trains = selected,
    native_events = if (nrow(native)) native else NULL,
    methods = methods, strict = strict
  )

  manual <- tryCatch(
    as.data.frame(stpd_manual_events(
      ds, params = params, selected_trains = selected,
      metric_mode = "candidate_family"
    )), error = function(e) data.frame()
  )
  truth <- stpd_patrol_reviewer_truth_empty()
  if (nrow(manual)) {
    manual <- stpd_patrol_reviewer_hydrate_event_geometry(manual, spikes)
    truth <- data.frame(
      train_key = as.character(manual$train),
      proposed_label = as.character(manual$pattern),
      start_isi = as.integer(manual$start_isi),
      end_isi = as.integer(manual$end_isi),
      start_time_sec = as.numeric(manual$start_time_sec),
      end_time_sec = as.numeric(manual$end_time_sec),
      stringsAsFactors = FALSE
    )
  }
  if (is.null(region_label) || !nzchar(trimws(as.character(region_label)[[1L]]))) {
    region_label <- ds$meta$display_name %||% "CURRENT"
  }
  region <- toupper(gsub("[^A-Za-z0-9_.-]+", "_", as.character(region_label)[[1L]]))
  if (!nzchar(region)) region <- "CURRENT"
  out <- list(
    schema_version = "stpd_multi_method_patrol_reviewer_v1",
    created_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    analysis_root = "integrated_stpd_session",
    regions = stats::setNames(list(list(
      label = as.character(region_label)[[1L]], spikes = spikes,
      truth_segments = truth, patrol = patrol,
      reference_yield = nrow(truth),
      provenance = list(source = "current_stpd_dataset",
                        dataset_name = ds$meta$display_name %||% region_label)
    )), region)
  )
  stpd_patrol_reviewer_validate_bundle(out)
}

#' Launch the multi-method patrol reviewer
#'
#' @param bundle A reviewer bundle or path to its RDS representation.
#' @param browser Open the system browser.
#' @param host Local host.
#' @param port Local port.
#' @param annotation_dir Directory for append-only annotation history and the
#'   current per-candidate annotation table. When `bundle` is a file path, the
#'   default is `review_annotations` beside that file.
#' @return The value returned by `shiny::runApp()` after exit.
launch_multi_method_patrol_reviewer <- function(
    bundle, browser = TRUE, host = "127.0.0.1", port = 7336L,
    annotation_dir = NULL) {
  bundle_path <- NULL
  if (is.character(bundle) && length(bundle) == 1L) {
    bundle_path <- normalizePath(bundle, mustWork = TRUE)
    bundle <- readRDS(bundle_path)
  }
  bundle <- stpd_patrol_reviewer_validate_bundle(bundle)
  if (is.null(annotation_dir)) {
    annotation_dir <- if (!is.null(bundle_path)) {
      file.path(dirname(bundle_path), "review_annotations")
    } else {
      file.path(tempdir(), "stpd_patrol_review_annotations")
    }
  }
  annotation_dir <- normalizePath(annotation_dir, mustWork = FALSE)
  dir.create(annotation_dir, recursive = TRUE, showWarnings = FALSE)
  bundle_sha256 <- if (!is.null(bundle_path)) {
    digest::digest(bundle_path, algo = "sha256", file = TRUE)
  } else {
    digest::digest(bundle, algo = "sha256", serialize = TRUE)
  }
  shiny::runApp(
    shiny::shinyApp(stpd_patrol_reviewer_ui(),
                    stpd_patrol_reviewer_server(
                      bundle, annotation_dir = annotation_dir,
                      bundle_sha256 = bundle_sha256
                    )),
    host = host, port = as.integer(port), launch.browser = browser
  )
}

stpd_patrol_reviewer_panel_ui <- function() {
  shiny::sidebarLayout(
      shiny::sidebarPanel(width = 3,
        shiny::selectInput("patrol_region", "数据集", choices = NULL),
        shiny::checkboxGroupInput(
          "patrol_status", "纠察类型",
          choices = c("语义冲突" = "semantic_conflict",
                      "边界分歧" = "boundary_disagreement",
                      "单方法候选" = "single_method_only",
                      "同家族一致" = "within_family_agreement",
                      "跨家族一致" = "cross_family_agreement"),
          selected = c("semantic_conflict", "boundary_disagreement",
                       "single_method_only", "within_family_agreement",
                       "cross_family_agreement")
        ),
        shiny::selectInput("patrol_train", "Spike train", choices = NULL),
        shiny::uiOutput("patrol_case_ui"),
        shiny::fluidRow(
          shiny::column(6, shiny::actionButton("patrol_prev", "← 上一个", width = "100%")),
          shiny::column(6, shiny::actionButton("patrol_next", "下一个 →", width = "100%"))
        ),
        shiny::numericInput("patrol_context", "问题区两侧时间（秒）", value = 0.5,
                            min = 0.02, max = 20, step = 0.1),
        shiny::checkboxInput("patrol_full_train", "显示完整 spike train", FALSE),
        shiny::checkboxGroupInput("patrol_providers", "显示轨道", choices = NULL),
        shiny::tags$div(class = "patrol-card",
          shiny::tags$div(class = "patrol-title", "人工标注编辑"),
          shiny::uiOutput("patrol_edit_status"),
          shiny::fluidRow(
            shiny::column(6, shiny::actionButton(
              "patrol_new", "＋新建区间", width = "100%")),
            shiny::column(6, shiny::actionButton(
              "patrol_undo", "撤销上次操作", width = "100%"))
          ),
          shiny::textInput("patrol_reviewer", "标注者", value = "Zhou Houchun"),
          shiny::selectInput("patrol_decision", "处理", choices = c(
            "接受并指定标签" = "accept_as_label",
            "拒绝候选／标为 other" = "reject_as_other",
            "暂不确定" = "uncertain"
          )),
          shiny::selectInput("patrol_label", "最终标签", choices = c(
            Burst = "burst", Pause = "pause", "Broad HFS" = "broad_hfs",
            Tonic = "tonic", Other = "other", Uncertain = "uncertain"
          )),
          shiny::fluidRow(
            shiny::column(6, shiny::numericInput(
              "patrol_start_isi", "起始 ISI", value = 2, min = 2, step = 1)),
            shiny::column(6, shiny::numericInput(
              "patrol_end_isi", "结束 ISI", value = 2, min = 2, step = 1))
          ),
          shiny::radioButtons("patrol_click_target", "点击 ISI 图设置",
            choices = c("起点" = "start", "终点" = "end"), inline = TRUE),
          shiny::textAreaInput("patrol_note", "备注", rows = 2),
          shiny::actionButton("patrol_save", "保存本条标注",
            class = "btn-primary", width = "100%"),
          shiny::numericInput("patrol_split_isi", "拆分位置（左段结束 ISI）",
            value = 2, min = 2, step = 1),
          shiny::fluidRow(
            shiny::column(6, shiny::actionButton(
              "patrol_split", "拆分所选", width = "100%")),
            shiny::column(6, shiny::actionButton(
              "patrol_delete", "擦除所选", width = "100%"))
          ),
          shiny::actionButton("patrol_merge", "合并多个所选区间",
            width = "100%"),
          shiny::uiOutput("patrol_save_status"),
          shiny::fluidRow(
            shiny::column(6, shiny::downloadButton(
              "patrol_download_current", "导出当前结果", width = "100%")),
            shiny::column(6, shiny::downloadButton(
              "patrol_download_history", "导出完整历史", width = "100%"))
          )
        ),
        shiny::tags$div(class = "patrol-note",
          "所有轨道使用同一组原始时间戳。点击局部 ISI 图可设置标注起点或终点。保存操作写入独立的追加式审查记录，不覆盖人工参考或检测器输出。")
      ),
      shiny::mainPanel(width = 9,
        shiny::uiOutput("patrol_summary"),
        shiny::tags$div(class = "patrol-card",
          shiny::tags$div(class = "patrol-title", "对齐的多方法证据轨"),
          plotly::plotlyOutput("patrol_raster", height = "610px")
        ),
        shiny::tags$div(class = "patrol-card",
          shiny::tags$div(class = "patrol-title", "局部 ISI 剖面"),
          plotly::plotlyOutput("patrol_isi", height = "300px")
        ),
        shiny::tags$div(class = "patrol-card",
          shiny::tags$div(class = "patrol-title", "本条 spike train 的人工修订"),
          shiny::tags$div(class = "patrol-note",
            "单选一行可继续编辑；多选同标签区间后可合并。"),
          DT::DTOutput("patrol_saved_annotations")
        ),
        shiny::tags$div(class = "patrol-card",
          shiny::tags$div(class = "patrol-title", "方法间边界与 IoU"),
          DT::DTOutput("patrol_pairwise")
        )
      )
  )
}

stpd_patrol_reviewer_ui <- function() {
  shiny::fluidPage(
    shiny::tags$head(shiny::tags$style(shiny::HTML("\n      body{background:#F4F7FB;color:#13213A}.patrol-card{background:white;border:1px solid #D8E1EC;border-radius:10px;padding:12px 14px;margin-bottom:12px}.patrol-title{font-weight:700;font-size:17px}.patrol-note{color:#53657D;font-size:13px;line-height:1.45}.patrol-pill{display:inline-block;padding:3px 8px;margin:2px;border-radius:999px;background:#EEF3F8;font-size:12px}.form-group{margin-bottom:10px}\n    "))),
    shiny::titlePanel("STPD 多算法纠察观察器"),
    stpd_patrol_reviewer_panel_ui()
  )
}

stpd_patrol_reviewer_server <- function(bundle, annotation_dir = tempdir(),
                                         bundle_sha256 = "") {
  function(input, output, session) {
    history_path <- file.path(annotation_dir, "patrol_annotation_history.csv")
    current_path <- file.path(annotation_dir, "patrol_annotation_current.csv")
    initial_history <- stpd_patrol_annotation_read(history_path)
    if (nrow(initial_history) && any(initial_history$bundle_sha256 != bundle_sha256)) {
      stop("Annotation directory is bound to a different reviewer bundle.", call. = FALSE)
    }
    annotation_history <- shiny::reactiveVal(initial_history)
    save_message <- shiny::reactiveVal("")
    active_annotation_id <- shiny::reactiveVal("")
    edit_origin <- shiny::reactiveVal("candidate")
    provider_labels <- c(
      manual_reference = "人工参考", native_stpd = "STPD",
      mean_isi = "Mean-ISI", logisi_newbd = "LogISI/newBD",
      mean_isi_calibrated = "Mean-ISI（校准）",
      logisi_newbd_calibrated = "LogISI/newBD（校准）",
      poisson_surprise = "Poisson Surprise",
      robust_gaussian_surprise = "RGS"
    )
    # The reviewer controls live inside a dynamically rendered panel.  A
    # preloaded bundle can be ready before `patrol_region` is bound, in which
    # case a one-shot update is silently lost.  Retry only until the browser
    # acknowledges a non-empty selection, then leave the user's choice alone.
    region_initialized <- shiny::reactiveVal(FALSE)
    shiny::observe({
      if (isTRUE(region_initialized())) return(invisible(NULL))
      shiny::invalidateLater(200, session)
      region_names <- names(bundle$regions)
      selected_region <- as.character(input$patrol_region %||% "")
      if (length(selected_region) && nzchar(selected_region[[1L]])) {
        region_initialized(TRUE)
        return(invisible(NULL))
      }
      shiny::updateSelectInput(
        session, "patrol_region",
        choices = stats::setNames(region_names, region_names),
        selected = if (length(region_names)) region_names[[1L]] else ""
      )
      invisible(NULL)
    })
    current_region <- shiny::reactive({
      shiny::req(input$patrol_region)
      bundle$regions[[input$patrol_region]]
    })
    catalog <- shiny::reactive({
      x <- current_region()$patrol$candidate_clusters
      if (!nrow(x)) return(x)
      status <- input$patrol_status %||% character()
      if (length(status)) x <- x[x$patrol_status %in% status, , drop = FALSE]
      x
    })
    shiny::observeEvent(list(input$patrol_region, input$patrol_status), {
      trains <- sort(names(current_region()$spikes), method = "radix")
      shiny::updateSelectInput(session, "patrol_train", choices = trains,
                               selected = if (length(trains)) trains[[1L]] else "")
    }, ignoreInit = FALSE)
    train_catalog <- shiny::reactive({
      x <- catalog(); shiny::req(input$patrol_train)
      x[x$train_key == input$patrol_train, , drop = FALSE]
    })
    output$patrol_case_ui <- shiny::renderUI({
      x <- train_catalog()
      if (!nrow(x)) return(shiny::helpText("当前筛选没有候选。"))
      labels <- paste0(seq_len(nrow(x)), ". ", x$target_family, " · ",
                       x$patrol_status, " · ISI ", x$start_isi, "–", x$end_isi)
      shiny::selectInput("patrol_case", "纠察候选", choices =
        stats::setNames(x$cluster_id, labels), selected = x$cluster_id[[1L]])
    })
    selected <- shiny::reactive({
      x <- train_catalog()
      case_id <- as.character(input$patrol_case %||% "")
      z <- x[x$cluster_id == case_id, , drop = FALSE]
      if (nrow(z) == 1L) {
        z$is_candidate <- TRUE
        return(z)
      }
      spikes <- as.numeric(current_region()$spikes[[input$patrol_train]])
      shiny::req(length(spikes) >= 2L)
      data.frame(
        cluster_id = "", train_key = input$patrol_train,
        target_family = "other", patrol_status = "manual_only",
        start_isi = 2L, end_isi = 2L, review_priority = "manual",
        method_count = 0L, evidence_family_count = 0L,
        nested_hfs_context = FALSE, providers = "无现有候选；可自由新建",
        is_candidate = FALSE, stringsAsFactors = FALSE
      )
    })
    shiny::observeEvent(selected(), {
      x <- selected()
      if (!isTRUE(x$is_candidate[[1L]])) {
        active_annotation_id("")
        edit_origin("new_manual")
        shiny::updateSelectInput(session, "patrol_decision", selected = "accept_as_label")
        shiny::updateSelectInput(session, "patrol_label", selected = "other")
        shiny::updateNumericInput(session, "patrol_start_isi", value = 2L)
        shiny::updateNumericInput(session, "patrol_end_isi", value = 2L)
        shiny::updateTextAreaInput(session, "patrol_note", value = "")
        save_message("当前 spike train 在筛选条件下没有候选；可直接新建人工区间。")
        return()
      }
      active_annotation_id(x$cluster_id[[1L]])
      edit_origin("candidate")
      current <- stpd_patrol_annotation_current(annotation_history())
      prior <- current[current$region == input$patrol_region &
                         current$bundle_sha256 == bundle_sha256 &
                         current$cluster_id == x$cluster_id[[1L]], , drop = FALSE]
      if (nrow(prior)) {
        shiny::updateSelectInput(session, "patrol_decision",
          selected = prior$decision[[1L]])
        shiny::updateSelectInput(session, "patrol_label",
          selected = prior$assigned_label[[1L]])
        shiny::updateNumericInput(session, "patrol_start_isi",
          value = prior$start_isi[[1L]])
        shiny::updateNumericInput(session, "patrol_end_isi",
          value = prior$end_isi[[1L]])
        shiny::updateTextAreaInput(session, "patrol_note", value = prior$note[[1L]])
      } else {
        shiny::updateSelectInput(session, "patrol_decision", selected = "accept_as_label")
        shiny::updateSelectInput(session, "patrol_label",
          selected = x$target_family[[1L]])
        shiny::updateNumericInput(session, "patrol_start_isi", value = x$start_isi[[1L]])
        shiny::updateNumericInput(session, "patrol_end_isi", value = x$end_isi[[1L]])
        shiny::updateTextAreaInput(session, "patrol_note", value = "")
      }
      save_message("")
    }, ignoreInit = FALSE)
    shiny::observeEvent(input$patrol_new, {
      x <- selected()
      active_annotation_id("")
      edit_origin("new_manual")
      shiny::updateSelectInput(session, "patrol_decision", selected = "accept_as_label")
      shiny::updateSelectInput(session, "patrol_label", selected = x$target_family[[1L]])
      shiny::updateNumericInput(session, "patrol_start_isi", value = x$start_isi[[1L]])
      shiny::updateNumericInput(session, "patrol_end_isi", value = x$end_isi[[1L]])
      shiny::updateTextAreaInput(session, "patrol_note", value = "")
      save_message("新建模式：请点击 ISI 图确定起点和终点，然后保存。")
    })
    shiny::observeEvent(input$patrol_decision, {
      if (identical(input$patrol_decision, "reject_as_other")) {
        shiny::updateSelectInput(session, "patrol_label", selected = "other")
      } else if (identical(input$patrol_decision, "uncertain")) {
        shiny::updateSelectInput(session, "patrol_label", selected = "uncertain")
      }
    }, ignoreInit = TRUE)
    move_case <- function(delta) {
      x <- train_catalog(); if (!nrow(x)) return()
      at <- match(input$patrol_case, x$cluster_id); if (is.na(at)) at <- 1L
      at <- max(1L, min(nrow(x), at + delta))
      shiny::updateSelectInput(session, "patrol_case", selected = x$cluster_id[[at]])
    }
    shiny::observeEvent(input$patrol_prev, move_case(-1L))
    shiny::observeEvent(input$patrol_next, move_case(1L))
    shiny::observeEvent(input$patrol_region, {
      ev <- current_region()$patrol$evidence
      providers <- unique(c("manual_reference", as.character(ev$provider_key)))
      providers <- providers[providers %in% names(provider_labels)]
      choices <- stats::setNames(providers, provider_labels[providers])
      shiny::updateCheckboxGroupInput(session, "patrol_providers",
        choices = choices, selected = providers)
    }, ignoreInit = FALSE)

    saved_annotations <- shiny::reactive({
      shiny::req(input$patrol_region, input$patrol_train)
      x <- stpd_patrol_annotation_active(annotation_history())
      x <- x[x$bundle_sha256 == bundle_sha256 & x$region == input$patrol_region &
               x$train_key == input$patrol_train,
             , drop = FALSE]
      x[order(x$start_isi, x$end_isi, x$cluster_id, method = "radix"), , drop = FALSE]
    })

    output$patrol_saved_annotations <- DT::renderDT({
      x <- saved_annotations()
      shown <- x[, c("assigned_label", "semantic_track", "start_isi", "end_isi",
                     "start_time_sec", "end_time_sec", "reviewer", "note",
                     "cluster_id"), drop = FALSE]
      names(shown)[1:4] <- c("label", "track", "start_ISI", "end_ISI")
      DT::datatable(shown, rownames = FALSE,
        selection = list(mode = "multiple", target = "row"),
        options = list(pageLength = 8, scrollX = TRUE, dom = "tip"))
    })

    selected_saved <- shiny::reactive({
      x <- saved_annotations()
      rows <- input$patrol_saved_annotations_rows_selected %||% integer()
      rows <- rows[rows >= 1L & rows <= nrow(x)]
      x[rows, , drop = FALSE]
    })

    shiny::observeEvent(input$patrol_saved_annotations_rows_selected, {
      x <- selected_saved()
      if (nrow(x) != 1L) return()
      active_annotation_id(x$cluster_id[[1L]])
      edit_origin("saved_manual")
      shiny::updateSelectInput(session, "patrol_decision", selected = x$decision[[1L]])
      shiny::updateSelectInput(session, "patrol_label", selected = x$assigned_label[[1L]])
      shiny::updateNumericInput(session, "patrol_start_isi", value = x$start_isi[[1L]])
      shiny::updateNumericInput(session, "patrol_end_isi", value = x$end_isi[[1L]])
      shiny::updateNumericInput(session, "patrol_split_isi",
        value = floor((x$start_isi[[1L]] + x$end_isi[[1L]]) / 2))
      shiny::updateTextAreaInput(session, "patrol_note", value = x$note[[1L]])
      save_message("已载入所选人工修订，可修改后再次保存。")
    })

    output$patrol_edit_status <- shiny::renderUI({
      id <- active_annotation_id()
      label <- if (identical(edit_origin(), "new_manual")) {
        "正在新建自由区间"
      } else if (identical(edit_origin(), "saved_manual")) {
        paste0("正在编辑已保存区间：", id)
      } else {
        paste0("正在复核候选：", id)
      }
      shiny::tags$div(class = "patrol-note", style = "margin:4px 0 8px;", label)
    })

    plot_data <- shiny::reactive({
      region <- current_region(); case <- selected(); train <- case$train_key[[1L]]
      spikes <- as.numeric(region$spikes[[train]])
      shiny::req(length(spikes) >= 2L)
      start_i <- max(2L, min(length(spikes), case$start_isi[[1L]]))
      end_i <- max(start_i, min(length(spikes), case$end_isi[[1L]]))
      candidate_geometry <- stpd_patrol_annotation_geometry(spikes, start_i, end_i)
      focus <- c(candidate_geometry$start_time_sec, candidate_geometry$end_time_sec)
      draft_start <- suppressWarnings(as.integer(input$patrol_start_isi))
      draft_end <- suppressWarnings(as.integer(input$patrol_end_isi))
      draft_valid <- length(draft_start) == 1L && length(draft_end) == 1L &&
        is.finite(draft_start) && is.finite(draft_end) && draft_start >= 2L &&
        draft_end >= draft_start && draft_end <= length(spikes)
      draft_focus <- if (draft_valid) {
        geometry <- stpd_patrol_annotation_geometry(spikes, draft_start, draft_end)
        c(geometry$start_time_sec, geometry$end_time_sec)
      } else focus
      if (isTRUE(input$patrol_full_train)) range <- range(spikes) else {
        pad <- max(0.02, as.numeric(input$patrol_context %||% 0.5))
        focus_union <- range(c(focus, draft_focus))
        range <- c(max(min(spikes), focus_union[[1L]] - pad),
                   min(max(spikes), focus_union[[2L]] + pad))
      }
      providers <- input$patrol_providers %||% character()
      ev <- region$patrol$evidence
      ev <- ev[ev$train_key == train & ev$provider_key %in% providers &
                 ev$end_time_sec >= range[[1L]] & ev$start_time_sec <= range[[2L]],
               , drop = FALSE]
      truth <- region$truth_segments
      truth <- truth[truth$train_key == train & "manual_reference" %in% providers &
                       truth$end_time_sec >= range[[1L]] &
                       truth$start_time_sec <= range[[2L]], , drop = FALSE]
      saved <- stpd_patrol_annotation_active(annotation_history())
      saved <- saved[saved$bundle_sha256 == bundle_sha256 &
                       saved$region == input$patrol_region & saved$train_key == train &
                       saved$end_time_sec >= range[[1L]] &
                       saved$start_time_sec <= range[[2L]], , drop = FALSE]
      list(spikes = spikes, evidence = ev, truth = truth, range = range,
           saved = saved,
           focus = focus, draft_focus = draft_focus, draft_valid = draft_valid,
           draft_start = draft_start, draft_end = draft_end,
           draft_label = as.character(input$patrol_label %||% "other"),
           case = case, train = train, providers = providers)
    })

    isi_click <- shiny::reactive({
      suppressWarnings(plotly::event_data(
        "plotly_click", source = "patrol_isi_plot", priority = "event"
      ))
    })
    shiny::observeEvent(isi_click(), {
      click <- isi_click()
      isi_index <- suppressWarnings(as.integer(click$customdata[[1L]]))
      if (!is.finite(isi_index)) return()
      if (identical(input$patrol_click_target, "end")) {
        start <- suppressWarnings(as.integer(input$patrol_start_isi))
        if (is.finite(start) && isi_index < start) {
          shiny::updateNumericInput(session, "patrol_start_isi", value = isi_index)
        }
        shiny::updateNumericInput(session, "patrol_end_isi", value = isi_index)
      } else {
        end <- suppressWarnings(as.integer(input$patrol_end_isi))
        shiny::updateNumericInput(session, "patrol_start_isi", value = isi_index)
        if (is.finite(end) && isi_index > end) {
          shiny::updateNumericInput(session, "patrol_end_isi", value = isi_index)
        }
      }
    })

    shiny::observeEvent(input$patrol_save, {
      region <- current_region(); case <- selected(); train <- case$train_key[[1L]]
      spikes <- as.numeric(region$spikes[[train]])
      start <- suppressWarnings(as.integer(input$patrol_start_isi))
      end <- suppressWarnings(as.integer(input$patrol_end_isi))
      if (!is.finite(start) || !is.finite(end) || start < 2L || end < start ||
          end > length(spikes)) {
        save_message("未保存：ISI 边界无效。")
        return()
      }
      reviewer <- trimws(as.character(input$patrol_reviewer %||% ""))
      if (!nzchar(reviewer)) {
        save_message("未保存：请填写标注者。")
        return()
      }
      decision <- as.character(input$patrol_decision)
      label <- as.character(input$patrol_label)
      if (identical(decision, "reject_as_other")) label <- "other"
      if (identical(decision, "uncertain")) label <- "uncertain"
      id <- active_annotation_id()
      if (!nzchar(id)) {
        id <- stpd_patrol_annotation_new_id(
          bundle_sha256, input$patrol_region, train, start, end, label
        )
      }
      record <- stpd_patrol_annotation_record(
        bundle_sha256, input$patrol_region, train, id, decision, label,
        spikes, start, end, reviewer, input$patrol_note %||% ""
      )
      updated <- tryCatch(
        stpd_patrol_annotation_append(history_path, current_path, record),
        error = function(e) e
      )
      if (inherits(updated, "error")) {
        save_message(paste("未保存：", conditionMessage(updated)))
      } else {
        annotation_history(updated)
        active_annotation_id(id)
        edit_origin("saved_manual")
        save_message(paste0("已保存：", label, " · ISI ", start, "–", end,
                            "（历史记录 ", nrow(updated), " 条）"))
      }
    })

    reviewer_value <- function() {
      reviewer <- trimws(as.character(input$patrol_reviewer %||% ""))
      if (!nzchar(reviewer)) stop("请填写标注者。", call. = FALSE)
      reviewer
    }
    append_records <- function(records, success_message) {
      updated <- tryCatch(
        stpd_patrol_annotation_append_many(history_path, current_path, records),
        error = function(e) e
      )
      if (inherits(updated, "error")) {
        save_message(paste("未保存：", conditionMessage(updated)))
        return(FALSE)
      }
      annotation_history(updated)
      save_message(success_message)
      TRUE
    }
    deletion_record <- function(row, reviewer, note) {
      spikes <- as.numeric(bundle$regions[[row$region[[1L]]]]$spikes[[row$train_key[[1L]]]])
      stpd_patrol_annotation_record(
        bundle_sha256, row$region[[1L]], row$train_key[[1L]],
        row$cluster_id[[1L]], "reject_as_other", "other", spikes,
        row$start_isi[[1L]], row$end_isi[[1L]], reviewer, note
      )
    }

    shiny::observeEvent(input$patrol_delete, {
      chosen <- selected_saved()
      if (!nrow(chosen)) {
        save_message("未擦除：请先在人工修订表中选择至少一行。")
        return()
      }
      reviewer <- tryCatch(reviewer_value(), error = function(e) e)
      if (inherits(reviewer, "error")) {
        save_message(paste("未擦除：", conditionMessage(reviewer))); return()
      }
      records <- do.call(rbind, lapply(seq_len(nrow(chosen)), function(i) {
        deletion_record(chosen[i, , drop = FALSE], reviewer, "manual erase")
      }))
      if (append_records(records, paste0("已擦除 ", nrow(chosen), " 个人工区间。"))) {
        active_annotation_id(""); edit_origin("new_manual")
      }
    })

    shiny::observeEvent(input$patrol_split, {
      chosen <- selected_saved()
      if (nrow(chosen) != 1L) {
        save_message("未拆分：请在人工修订表中只选择一个区间。")
        return()
      }
      split_at <- suppressWarnings(as.integer(input$patrol_split_isi))
      if (!is.finite(split_at) || split_at < chosen$start_isi[[1L]] ||
          split_at >= chosen$end_isi[[1L]]) {
        save_message("未拆分：拆分位置必须位于所选区间内部。")
        return()
      }
      reviewer <- tryCatch(reviewer_value(), error = function(e) e)
      if (inherits(reviewer, "error")) {
        save_message(paste("未拆分：", conditionMessage(reviewer))); return()
      }
      region <- chosen$region[[1L]]; train <- chosen$train_key[[1L]]
      spikes <- as.numeric(bundle$regions[[region]]$spikes[[train]])
      label <- chosen$assigned_label[[1L]]
      left_id <- stpd_patrol_annotation_new_id(
        bundle_sha256, region, train, chosen$start_isi[[1L]], split_at,
        label, paste0("split-left-", chosen$cluster_id[[1L]], "-", Sys.time()))
      right_id <- stpd_patrol_annotation_new_id(
        bundle_sha256, region, train, split_at + 1L, chosen$end_isi[[1L]],
        label, paste0("split-right-", chosen$cluster_id[[1L]], "-", Sys.time()))
      records <- rbind(
        deletion_record(chosen, reviewer, "manual split: parent retired"),
        stpd_patrol_annotation_record(
          bundle_sha256, region, train, left_id, "accept_as_label", label,
          spikes, chosen$start_isi[[1L]], split_at, reviewer,
          paste0("manual split from ", chosen$cluster_id[[1L]])),
        stpd_patrol_annotation_record(
          bundle_sha256, region, train, right_id, "accept_as_label", label,
          spikes, split_at + 1L, chosen$end_isi[[1L]], reviewer,
          paste0("manual split from ", chosen$cluster_id[[1L]]))
      )
      if (append_records(records, paste0("已拆分为 ISI ", chosen$start_isi[[1L]],
                                         "–", split_at, " 和 ", split_at + 1L,
                                         "–", chosen$end_isi[[1L]], "。"))) {
        active_annotation_id(left_id); edit_origin("saved_manual")
      }
    })

    shiny::observeEvent(input$patrol_merge, {
      chosen <- selected_saved()
      if (nrow(chosen) < 2L) {
        save_message("未合并：请在人工修订表中选择至少两个区间。")
        return()
      }
      if (length(unique(chosen$assigned_label)) != 1L ||
          length(unique(chosen$train_key)) != 1L ||
          length(unique(chosen$region)) != 1L) {
        save_message("未合并：只能合并同一 spike train 中标签相同的区间。")
        return()
      }
      reviewer <- tryCatch(reviewer_value(), error = function(e) e)
      if (inherits(reviewer, "error")) {
        save_message(paste("未合并：", conditionMessage(reviewer))); return()
      }
      region <- chosen$region[[1L]]; train <- chosen$train_key[[1L]]
      spikes <- as.numeric(bundle$regions[[region]]$spikes[[train]])
      label <- chosen$assigned_label[[1L]]
      start <- min(chosen$start_isi); end <- max(chosen$end_isi)
      merged_id <- stpd_patrol_annotation_new_id(
        bundle_sha256, region, train, start, end, label,
        paste0("merge-", paste(chosen$cluster_id, collapse = ";"), "-", Sys.time()))
      deletions <- do.call(rbind, lapply(seq_len(nrow(chosen)), function(i) {
        deletion_record(chosen[i, , drop = FALSE], reviewer, "manual merge: source retired")
      }))
      merged <- stpd_patrol_annotation_record(
        bundle_sha256, region, train, merged_id, "accept_as_label", label,
        spikes, start, end, reviewer,
        paste0("manual merge of ", paste(chosen$cluster_id, collapse = ";"))
      )
      if (append_records(rbind(deletions, merged),
                         paste0("已将 ", nrow(chosen), " 个区间合并为 ISI ",
                                start, "–", end, "。"))) {
        active_annotation_id(merged_id); edit_origin("saved_manual")
      }
    })

    shiny::observeEvent(input$patrol_undo, {
      history <- annotation_history()
      if (!nrow(history)) {
        save_message("没有可撤销的人工操作。")
        return()
      }
      reviewer <- tryCatch(reviewer_value(), error = function(e) e)
      if (inherits(reviewer, "error")) {
        save_message(paste("未撤销：", conditionMessage(reviewer))); return()
      }
      records <- tryCatch(
        stpd_patrol_annotation_undo_records(history, reviewer),
        error = function(e) e
      )
      if (inherits(records, "error")) {
        save_message(paste("未撤销：", conditionMessage(records))); return()
      }
      append_records(records, "已通过补偿记录撤销上一次人工操作。")
    })

    output$patrol_save_status <- shiny::renderUI({
      text <- save_message()
      current <- stpd_patrol_annotation_current(annotation_history())
      id <- active_annotation_id()
      prior <- current[current$region == input$patrol_region &
                         current$bundle_sha256 == bundle_sha256 &
                         current$cluster_id == id, , drop = FALSE]
      shiny::tags$div(class = "patrol-note", style = "margin:8px 0;",
        if (nzchar(text)) text else if (nrow(prior)) {
          paste0("已标注：", prior$assigned_label[[1L]], " · ISI ",
                 prior$start_isi[[1L]], "–", prior$end_isi[[1L]])
        } else "本候选尚未保存人工标注。")
    })

    output$patrol_download_current <- shiny::downloadHandler(
      filename = function() "patrol_annotation_current.csv",
      content = function(file) utils::write.csv(
        stpd_patrol_annotation_current(annotation_history()), file,
        row.names = FALSE, na = "")
    )
    output$patrol_download_history <- shiny::downloadHandler(
      filename = function() "patrol_annotation_history.csv",
      content = function(file) utils::write.csv(
        annotation_history(), file, row.names = FALSE, na = "")
    )

    output$patrol_summary <- shiny::renderUI({
      x <- selected()
      if (!isTRUE(x$is_candidate[[1L]])) {
        return(shiny::tags$div(class = "patrol-card",
          shiny::tags$span(class = "patrol-pill", "自由人工标注"),
          shiny::tags$span(class = "patrol-pill", "当前筛选无候选"),
          shiny::tags$div(class = "patrol-note",
            "可在 ISI 图选择边界，然后新建 Burst、Pause、Broad HFS、Tonic、Other 或 Uncertain 区间。")
        ))
      }
      shiny::tags$div(class = "patrol-card",
        shiny::tags$span(class = "patrol-pill", paste("优先级", x$review_priority)),
        shiny::tags$span(class = "patrol-pill", x$patrol_status),
        shiny::tags$span(class = "patrol-pill", paste("方法", x$method_count)),
        shiny::tags$span(class = "patrol-pill", paste("证据家族", x$evidence_family_count)),
        shiny::tags$span(class = "patrol-pill", paste("ISI", x$start_isi, "–", x$end_isi)),
        if (isTRUE(x$nested_hfs_context)) shiny::tags$span(class = "patrol-pill", "Burst 位于 Broad HFS 内"),
        shiny::tags$div(class = "patrol-note", paste("证据：", x$providers))
      )
    })

    output$patrol_raster <- plotly::renderPlotly({
      d <- plot_data(); palette <- stpd_patrol_reviewer_palette()
      providers <- d$providers
      lane_keys <- c("raw", "review_draft", "manual_revision", providers)
      lane_labels <- c(raw = "原始 spike train", review_draft = "当前标注草稿",
                       manual_revision = "已保存人工修订",
                       provider_labels[providers])
      y <- rev(seq_along(lane_keys)); names(y) <- lane_keys
      visible_spikes <- d$spikes[d$spikes >= d$range[[1L]] & d$spikes <= d$range[[2L]]]
      p <- plotly::plot_ly()
      for (lane in lane_keys) {
        yy <- y[[lane]]
        if (length(visible_spikes)) {
          p <- plotly::add_segments(p, x = visible_spikes, xend = visible_spikes,
            y = yy - 0.16, yend = yy + 0.16, line = list(color = "#1F2937", width = 0.7),
            hoverinfo = "skip", showlegend = FALSE)
        }
      }
      add_intervals <- function(p, z, lane) {
        if (!nrow(z) || !(lane %in% names(y))) return(p)
        for (i in seq_len(nrow(z))) {
          family <- stpd_patrol_label_family(z$proposed_label[[i]])
          color <- palette[[family]] %||% palette[["other"]]
          p <- plotly::add_segments(p, x = z$start_time_sec[[i]],
            xend = z$end_time_sec[[i]], y = y[[lane]] - 0.23,
            yend = y[[lane]] - 0.23, line = list(color = color, width = 7),
            text = paste0(z$proposed_label[[i]], "<br>ISI ", z$start_isi[[i]],
                          "–", z$end_isi[[i]]), hoverinfo = "text", showlegend = FALSE)
        }
        p
      }
      p <- add_intervals(p, d$truth, "manual_reference")
      if (nrow(d$saved)) {
        saved <- d$saved
        saved$proposed_label <- saved$assigned_label
        p <- add_intervals(p, saved, "manual_revision")
      }
      for (provider in setdiff(providers, "manual_reference")) {
        p <- add_intervals(p, d$evidence[d$evidence$provider_key == provider, , drop = FALSE],
                           provider)
      }
      if (isTRUE(d$draft_valid)) {
        draft <- data.frame(
          proposed_label = d$draft_label, start_isi = d$draft_start,
          end_isi = d$draft_end, start_time_sec = d$draft_focus[[1L]],
          end_time_sec = d$draft_focus[[2L]], stringsAsFactors = FALSE
        )
        p <- add_intervals(p, draft, "review_draft")
      }
      p <- plotly::layout(p,
        shapes = list(
          list(type = "rect", x0 = d$focus[[1L]], x1 = d$focus[[2L]],
            y0 = 0.5, y1 = max(y) + 0.5, fillcolor = "rgba(245,158,11,0.08)",
            line = list(color = "rgba(180,83,9,0.55)", dash = "dot"), layer = "below"),
          list(type = "rect", x0 = d$draft_focus[[1L]], x1 = d$draft_focus[[2L]],
            y0 = 0.5, y1 = max(y) + 0.5, fillcolor = "rgba(16,185,129,0.06)",
            line = list(color = "rgba(5,150,105,0.85)", dash = "dash"), layer = "below")
        ),
        xaxis = list(title = "Spike time (s)", range = d$range,
                     rangeslider = list(visible = TRUE, thickness = 0.08)),
        yaxis = list(title = "", tickmode = "array", tickvals = unname(y),
                     ticktext = unname(lane_labels[lane_keys]), range = c(0.5, max(y) + 0.5)),
        margin = list(l = 150, r = 25, t = 20, b = 70),
        hovermode = "closest", dragmode = "pan")
      plotly::config(p, displaylogo = FALSE, scrollZoom = TRUE)
    })

    output$patrol_isi <- plotly::renderPlotly({
      d <- plot_data(); isi <- diff(d$spikes)
      right_time <- d$spikes[-1L]
      keep <- right_time >= d$range[[1L]] & right_time <= d$range[[2L]] &
        is.finite(isi) & isi > 0
      p <- plotly::plot_ly(x = right_time[keep], y = 1000 * isi[keep],
        type = "scatter", mode = "lines+markers",
        source = "patrol_isi_plot",
        customdata = stpd_patrol_diff_to_canonical_isi(which(keep)),
        line = list(color = "#94A3B8", width = 1),
        marker = list(color = "#334155", size = 4),
        text = paste0("ISI ", stpd_patrol_diff_to_canonical_isi(which(keep)), "<br>",
                      signif(1000 * isi[keep], 5), " ms"),
        hoverinfo = "text")
      p <- plotly::layout(p,
        shapes = list(
          list(type = "rect", x0 = d$focus[[1L]], x1 = d$focus[[2L]],
            y0 = 0, y1 = 1, yref = "paper", fillcolor = "rgba(245,158,11,0.08)",
            line = list(color = "rgba(180,83,9,0.55)", dash = "dot"), layer = "below"),
          list(type = "rect", x0 = d$draft_focus[[1L]], x1 = d$draft_focus[[2L]],
            y0 = 0, y1 = 1, yref = "paper", fillcolor = "rgba(16,185,129,0.06)",
            line = list(color = "rgba(5,150,105,0.85)", dash = "dash"), layer = "below")
        ),
        xaxis = list(title = "Spike time (s)", range = d$range),
        yaxis = list(title = "ISI (ms, log scale)", type = "log"),
        margin = list(l = 75, r = 25, t = 15, b = 55), dragmode = "pan")
      p <- plotly::event_register(p, "plotly_click")
      plotly::config(p, displaylogo = FALSE, scrollZoom = TRUE)
    })

    output$patrol_pairwise <- DT::renderDT({
      report <- current_region()$patrol; case <- selected()
      if (!isTRUE(case$is_candidate[[1L]])) {
        return(DT::datatable(data.frame(说明 = "当前没有方法候选可比较。"),
          rownames = FALSE, options = list(dom = "t")))
      }
      ids <- report$cluster_membership$evidence_id[
        report$cluster_membership$cluster_id == case$cluster_id[[1L]]]
      z <- report$pairwise_disagreement[
        report$pairwise_disagreement$evidence_id_a %in% ids &
          report$pairwise_disagreement$evidence_id_b %in% ids, , drop = FALSE]
      if (nrow(z)) z$iou <- round(z$iou, 3)
      DT::datatable(z[, setdiff(names(z), "cluster_index"), drop = FALSE],
        rownames = FALSE, options = list(pageLength = 8, scrollX = TRUE,
                                         dom = "tip"))
    })
  }
}
