# UI helpers for pattern-aware labels and boxes.  Pattern identity is now shown
# with swatches and section borders; text stays neutral for readability.
stpd_ui_pattern_order <- function(include_not_burst = FALSE) {
  vals <- c("burst", "long_burst", "tonic", "high_frequency_tonic", "high_frequency_spiking", "pause", "others")
  if (isTRUE(include_not_burst)) vals <- c(vals, "not_burst")
  vals
}

stpd_ui_pattern_display <- function(pattern) {
  labs <- c(
    burst = "\u7206\u53D1\uFF08burst\uFF09",
    long_burst = "\u957F\u7206\u53D1\uFF08long burst\uFF09",
    possible_burst = "\u7591\u4F3C\u7206\u53D1\uFF08possible burst\uFF09",
    tonic = "\u5F3A\u76F4\u53D1\u653E\uFF08tonic\uFF09",
    high_frequency_tonic = "\u9AD8\u9891\u5F3A\u76F4\u53D1\u653E\uFF08HF tonic\uFF09",
    high_frequency_spiking = "\u9AD8\u9891\u8FDE\u7EED\u53D1\u653E\uFF08HF spiking\uFF09",
    pause = "\u6682\u505C\uFF08pause\uFF09",
    others = "\u5176\u4ED6\uFF08others\uFF09",
    not_burst = "\u975E\u7206\u53D1 / \u5F3A\u8D1F\u4F8B"
  )
  out <- labs[as.character(pattern)]
  ifelse(is.na(out), as.character(pattern), out)
}

stpd_ui_pattern_color <- function(pattern, source = "manual") {
  pattern <- as.character(pattern %||% "")
  if (pattern == "not_burst") return("#222222")
  pal <- tryCatch(pattern_palette("pattern_color"), error = function(e) data.frame())
  if (!is.null(pal) && nrow(pal) > 0 && pattern %in% pal$pattern) {
    col <- if (identical(source, "auto")) pal$auto[pal$pattern == pattern][1] else pal$manual[pal$pattern == pattern][1]
    if (is.character(col) && nzchar(col)) return(col)
  }
  "#333333"
}

stpd_ui_pattern_label <- function(label, pattern, bold = TRUE) {
  tags$span(
    class = "pattern-text-label",
    style = if (isTRUE(bold)) "font-weight:700;" else "",
    label
  )
}

stpd_ui_pattern_choice_names <- function(patterns) {
  lapply(patterns, function(pat) stpd_ui_pattern_label(stpd_ui_pattern_display(pat), pat))
}

stpd_ui_auto_pattern_legend <- function() {
  patterns <- c("burst", "long_burst", "possible_burst", "tonic",
                "high_frequency_tonic", "high_frequency_spiking", "pause", "others")
  short_labels <- c(
    burst = "\u7206\u53D1",
    long_burst = "\u957F\u7206\u53D1",
    possible_burst = "\u7591\u4F3C",
    tonic = "\u5F3A\u76F4",
    high_frequency_tonic = "HF tonic",
    high_frequency_spiking = "HF spiking",
    pause = "\u6682\u505C",
    others = "\u5176\u4ED6"
  )
  tags$div(
    class = "auto-pattern-legend",
    tags$span(class = "auto-pattern-legend-title", "AUTO \u56FE\u4F8B"),
    lapply(patterns, function(pat) {
      tags$span(
        class = "auto-pattern-legend-item",
        title = stpd_ui_pattern_display(pat),
        tags$span(class = "auto-pattern-swatch", style = paste0("background:", stpd_ui_pattern_color(pat, "auto"), ";")),
        tags$span(short_labels[[pat]] %||% pat)
      )
    })
  )
}

stpd_ui_pattern_isi_controls <- function() {
  pats <- stpd_ui_pattern_order(FALSE)
  rows <- lapply(pats, function(pat) {
    col <- stpd_ui_pattern_color(pat, "manual")
    tags$div(
      class = "pattern-isi-box",
      style = paste0("border-color:", col, "; --pattern-color:", col, ";"),
      tags$div(
        class = "pattern-box-title",
        tags$span(class = "auto-pattern-swatch", style = paste0("background:", col, ";")),
        stpd_ui_pattern_display(pat)
      ),
      fluidRow(
        column(6, numericInput(paste0("pattern_min_isi_", pat), "\u6700\u5C0F ISI", value = 0, min = 0, step = 0.1)),
        column(6, numericInput(paste0("pattern_max_isi_", pat), "\u6700\u5927 ISI", value = 0, min = 0, step = 0.1))
      )
    )
  })
  tags$details(
    open = FALSE,
    tags$summary(strong("\u5404\u6A21\u5F0F\u4E13\u5C5E\u6700\u5C0FISI / \u6700\u5927ISI\u95E8\u63A7")),
    tags$div(class = "small-note", "\u53EF\u9009\u7684\u6700\u7EC8\u4E8B\u4EF6\u7EA7 ISI \u95E8\u63A7\u30020 \u8868\u793A\u4E0D\u542F\u7528\u5BF9\u5E94\u95E8\u63A7\u3002\u6570\u503C\u5355\u4F4D\u8DDF\u968F\u201C\u4F2A\u8FF9/\u4E0D\u5E94\u671F\u9608\u503C\u5355\u4F4D\u201D\u3002"),
    do.call(tagList, rows)
  )
}

stpd_isi_state_axis_choices <- function(include_pc = TRUE, include_isomap = FALSE) {
  out <- c()
  if (isTRUE(include_pc)) out <- c(out, "PC1" = "PC1", "PC2" = "PC2", "PC3" = "PC3")
  if (isTRUE(include_isomap)) {
    out <- c(out, "Isomap 1" = "Isomap1", "Isomap 2" = "Isomap2", "Isomap 3" = "Isomap3")
  }
  c(
    out,
    "\u65F6\u95F4\uFF08\u5F53\u524D\u5355\u4F4D\uFF09" = "time_from_start_plot",
    "ISI\uFF08\u5F53\u524D\u5355\u4F4D\uFF09" = "ISI_plot",
    "log10(ISI)" = "log_isi",
    "\u5C40\u90E8\u53D1\u653E\u7387 Hz" = "local_rate_hz",
    "\u5C40\u90E8 CV2" = "local_cv2",
    "\u5C40\u90E8 LV" = "local_lv",
    "pre/post ratio" = "prepost_ratio",
    "\u0394 logISI" = "delta_logisi",
    "next \u0394 logISI" = "next_delta_logisi"
  )
}

stpd_isi_state_global_controls <- function() {
  tags$div(
    class = "state-space-controls state-space-global-controls",
    tags$div(
      class = "state-space-control-grid",
      tags$div(class = "state-space-control state-space-control-wide", uiOutput("isi_state_space_train_selector")),
      tags$div(
        class = "state-space-control state-space-radio",
        radioButtons(
          "isi_state_space_label_source", "\u70B9\u989C\u8272\u6807\u7B7E",
          choices = c("\u6700\u7EC8\u5BA1\u8BA1\u7ED3\u679C" = "audit_final", "\u6700\u7EC8\u6807\u7B7E" = "final", "MANUAL \u4F18\u5148" = "manual_priority", "AUTO" = "auto", "MANUAL" = "manual"),
          selected = "audit_final",
          inline = TRUE
        )
      ),
      tags$div(class = "state-space-control", numericInput("isi_state_space_k", "\u5C40\u90E8 ISI \u534A\u7A97 k", value = 3, min = 1, max = 10, step = 1)),
      tags$div(
        class = "state-space-control state-space-radio",
        radioButtons(
          "isi_state_space_scaling", "\u72B6\u6001\u7A7A\u95F4 scaling",
          choices = c("Robust median/MAD" = "robust", "Z-score" = "zscore"),
          selected = "robust",
          inline = TRUE
        )
      ),
      tags$div(
        class = "state-space-control state-space-checks",
        checkboxInput("isi_state_space_winsorize", "Winsorize \u6781\u7AEF logISI", TRUE),
        checkboxInput("isi_state_space_break_pause", "pause / \u957F ISI \u5904\u65AD\u7EBF", TRUE)
      ),
      tags$div(class = "state-space-control", numericInput("isi_state_space_break_isi", "\u65AD\u7EBF ISI \u9608\u503C\uFF08\u5F53\u524D\u5355\u4F4D\uFF09", value = 150, min = 0, step = 5)),
      tags$div(
        class = "state-space-control state-space-radio state-space-control-wide",
        radioButtons(
          "isi_state_space_time_range_mode", "\u65F6\u95F4\u8303\u56F4",
          choices = c("\u5168\u65F6\u957F" = "full", "\u540C\u6B65 raster \u65F6\u95F4\u7A97" = "sync", "\u81EA\u5B9A\u4E49\u7A97\u53E3" = "custom"),
          selected = "full",
          inline = TRUE
        )
      ),
      tags$div(class = "state-space-control state-space-control-wide", uiOutput("isi_state_space_custom_window_ui"))
    )
  )
}

stpd_isi_state_context_controls <- function() {
  pc_choices <- stpd_isi_state_axis_choices(include_pc = TRUE, include_isomap = FALSE)
  iso_choices <- stpd_isi_state_axis_choices(include_pc = FALSE, include_isomap = TRUE)
  tags$div(
    class = "state-space-controls state-space-context-controls",
    conditionalPanel(
      condition = "input.isi_state_space_view == 'pca' || input.isi_state_space_view == 'pca3d'",
      tags$div(
        class = "state-space-control-grid",
        tags$div(class = "state-space-control", selectInput("isi_state_space_x_axis", "X \u8F74", choices = pc_choices, selected = "PC1")),
        tags$div(class = "state-space-control", selectInput("isi_state_space_y_axis", "Y \u8F74", choices = pc_choices, selected = "PC2")),
        tags$div(class = "state-space-control", selectInput("isi_state_space_z_axis", "Z \u8F74\uFF083D\uFF09", choices = pc_choices, selected = "time_from_start_plot"))
      )
    ),
    conditionalPanel(
      condition = "input.isi_state_space_view == 'isomap' || input.isi_state_space_view == 'isomap3d'",
      tags$div(
        class = "state-space-control-grid",
        tags$div(class = "state-space-control", numericInput("isi_state_space_isomap_neighbors", "\u8FD1\u90BB\u6570 k", value = 15, min = 3, max = 80, step = 1)),
        tags$div(class = "state-space-control", numericInput("isi_state_space_isomap_max_points", "\u6700\u5927\u70B9\u6570", value = 600, min = 100, max = 3000, step = 100)),
        tags$div(class = "state-space-control", selectInput("isi_state_space_isomap_x_axis", "Isomap 2D X", choices = iso_choices, selected = "Isomap1")),
        tags$div(class = "state-space-control", selectInput("isi_state_space_isomap_y_axis", "Isomap 2D Y", choices = iso_choices, selected = "Isomap2")),
        tags$div(class = "state-space-control", selectInput("isi_state_space_isomap_3d_x_axis", "Isomap 3D X", choices = iso_choices, selected = "Isomap1")),
        tags$div(class = "state-space-control", selectInput("isi_state_space_isomap_3d_y_axis", "Isomap 3D Y", choices = iso_choices, selected = "Isomap2")),
        tags$div(class = "state-space-control", selectInput("isi_state_space_isomap_3d_z_axis", "Isomap 3D Z", choices = iso_choices, selected = "Isomap3"))
      )
    ),
    conditionalPanel(
      condition = "input.isi_state_space_view == 'core'",
      tags$div(
        class = "state-space-control-grid state-space-control-grid-compact",
        tags$div(class = "state-space-control", numericInput("isi_state_space_surrogate_n", "Surrogate \u6B21\u6570", value = 49, min = 1, max = 999, step = 10)),
        tags$div(class = "state-space-control", numericInput("isi_state_space_surrogate_block", "Block shuffle \u5757\u957F\uFF08ISI\u6570\uFF09", value = 10, min = 2, max = 200, step = 1))
      )
    ),
    conditionalPanel(
      condition = "input.isi_state_space_view == 'explore'",
      tags$div(
        class = "state-space-control-grid state-space-control-grid-compact",
        tags$div(class = "state-space-control", numericInput("isi_state_space_diffusion_neighbors", "Diffusion / PHATE kNN", value = 15, min = 3, max = 80, step = 1)),
        tags$div(class = "state-space-control", numericInput("isi_state_space_explore_max_points", "\u63A2\u7D22\u6700\u5927\u70B9\u6570", value = 600, min = 100, max = 3000, step = 100)),
        tags$div(class = "state-space-control", textInput("isi_state_space_isomap_sweep_grid", "Isomap sweep k", value = "5,8,10,15,20,30")),
        tags$div(class = "state-space-control", numericInput("isi_state_space_recurrence_rate", "RQA recurrence rate", value = 0.05, min = 0.005, max = 0.5, step = 0.005))
      )
    ),
    conditionalPanel(
      condition = "input.isi_state_space_view == 'model'",
      tags$div(
        class = "state-space-control-grid state-space-control-grid-compact",
        tags$div(class = "state-space-control", textInput("isi_state_space_gmm_states", "GMM \u5019\u9009 state \u6570", value = "2,3,4,5"))
      )
    )
  )
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      html, body {
        height: 100%;
        background: #f5f7fb;
        color: #1f2937;
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
      }
      body {
        font-size: 13px;
      }
      h4 {
        font-size: 15px;
        font-weight: 700;
        margin-top: 4px;
        margin-bottom: 10px;
        color: #111827;
      }
      .container-fluid {
        padding-left: 18px;
        padding-right: 18px;
      }
      .app-header {
        margin: 14px 0 10px 0;
        padding: 16px 18px;
        border: 1px solid #dbe3ef;
        border-radius: 8px;
        background: #ffffff;
        box-shadow: 0 6px 18px rgba(15, 23, 42, 0.06);
      }
      .app-title {
        font-size: 22px;
        font-weight: 800;
        letter-spacing: 0;
        color: #0f172a;
      }
      .app-subtitle {
        margin-top: 4px;
        color: #526174;
        font-size: 13px;
        line-height: 1.45;
      }
      .workflow-strip {
        display: grid;
        grid-template-columns: repeat(5, minmax(0, 1fr));
        gap: 8px;
        margin-bottom: 12px;
      }
      .workflow-step {
        display: flex;
        gap: 8px;
        align-items: flex-start;
        min-height: 64px;
        padding: 10px;
        border: 1px solid #dbe3ef;
        border-radius: 8px;
        background: #ffffff;
      }
      .workflow-index {
        flex: 0 0 auto;
        width: 24px;
        height: 24px;
        border-radius: 50%;
        background: #2563eb;
        color: #ffffff;
        text-align: center;
        line-height: 24px;
        font-size: 12px;
        font-weight: 800;
      }
      .workflow-title {
        font-weight: 800;
        color: #111827;
        line-height: 1.2;
      }
      .workflow-note {
        margin-top: 3px;
        color: #64748b;
        font-size: 12px;
        line-height: 1.25;
      }
      .stpd-workbench {
        position: relative;
      }
      .stpd-workbench-toolbar {
        position: sticky;
        top: 0;
        z-index: 30;
        display: flex;
        align-items: center;
        justify-content: flex-end;
        gap: 10px;
        margin: 0 0 8px;
        padding: 4px 0;
        background: #f5f7fb;
      }
      .stpd-sidebar-toggle {
        display: inline-flex;
        align-items: center;
        gap: 7px;
        border: 1px solid #cbd5e1;
        border-radius: 7px;
        background: #ffffff;
        color: #1e293b;
        min-height: 32px;
        padding: 6px 10px;
        font-size: 12px;
        font-weight: 800;
        box-shadow: 0 2px 8px rgba(15, 23, 42, 0.06);
      }
      .stpd-sidebar-toggle:hover,
      .stpd-sidebar-toggle:focus {
        border-color: #94a3b8;
        background: #f8fafc;
        outline: none;
      }
      .stpd-sidebar-toggle-icon {
        display: inline-flex;
        align-items: center;
        justify-content: center;
        width: 18px;
        height: 18px;
        border-radius: 50%;
        background: #e0f2fe;
        color: #075985;
        font-size: 16px;
        line-height: 1;
      }
      .stpd-workbench-hint {
        color: #64748b;
        font-size: 12px;
      }
      .data-load-progress {
        margin: 10px 0 12px;
        padding: 10px 11px;
        border: 1px solid #cbd5e1;
        border-radius: 8px;
        background: #f8fafc;
        box-shadow: inset 0 1px 0 rgba(255, 255, 255, 0.7);
      }
      .data-load-progress.is-active {
        border-color: #93c5fd;
        background: #eff6ff;
      }
      .data-load-progress.is-success {
        border-color: #86efac;
        background: #f0fdf4;
      }
      .data-load-progress.is-error {
        border-color: #fecaca;
        background: #fef2f2;
      }
      .data-load-progress-head {
        display: flex;
        align-items: baseline;
        justify-content: space-between;
        gap: 10px;
        margin-bottom: 7px;
      }
      .data-load-progress-title {
        font-weight: 800;
        color: #1e293b;
      }
      .data-load-progress-percent {
        color: #475569;
        font-size: 12px;
        font-weight: 800;
      }
      .data-load-progress-track {
        overflow: hidden;
        height: 8px;
        border-radius: 999px;
        background: #e2e8f0;
      }
      .data-load-progress-bar {
        width: 0%;
        height: 100%;
        border-radius: inherit;
        background: linear-gradient(90deg, #2563eb, #22c55e);
        transition: width 180ms ease;
      }
      .data-load-progress-detail {
        margin-top: 6px;
        color: #64748b;
        font-size: 12px;
        line-height: 1.35;
      }
      .plot-output-wrap {
        position: relative;
        min-height: 68vh;
      }
      .plot-output-empty {
        min-height: 68vh;
        display: flex;
        flex-direction: column;
        align-items: flex-start;
        justify-content: flex-start;
        gap: 4px;
        padding: 18px 10px;
        color: #64748b;
        font-size: 14px;
      }
      .plot-output-empty-title {
        color: #475569;
        font-weight: 800;
      }
      .plot-render-progress {
        display: none;
        position: absolute;
        top: 18px;
        left: 50%;
        transform: translateX(-50%);
        z-index: 30;
        width: min(560px, calc(100% - 44px));
        padding: 12px 14px;
        border: 1px solid #93c5fd;
        border-radius: 8px;
        background: rgba(239, 246, 255, 0.96);
        box-shadow: 0 12px 32px rgba(15, 23, 42, 0.14);
        backdrop-filter: blur(3px);
      }
      .plot-render-progress.is-active,
      .plot-render-progress.is-success,
      .plot-render-progress.is-error {
        display: block;
      }
      .plot-render-progress.is-success {
        border-color: #86efac;
        background: rgba(240, 253, 244, 0.96);
      }
      .plot-render-progress.is-error {
        border-color: #fecaca;
        background: rgba(254, 242, 242, 0.96);
      }
      .plot-render-progress-head {
        display: flex;
        align-items: baseline;
        justify-content: space-between;
        gap: 12px;
        margin-bottom: 8px;
      }
      .plot-render-progress-title {
        font-weight: 900;
        color: #0f172a;
      }
      .plot-render-progress-percent {
        color: #475569;
        font-size: 12px;
        font-weight: 900;
      }
      .plot-render-progress-track {
        overflow: hidden;
        height: 8px;
        border-radius: 999px;
        background: #dbeafe;
      }
      .plot-render-progress-bar {
        width: 8%;
        height: 100%;
        border-radius: inherit;
        background: linear-gradient(90deg, #2563eb, #22c55e);
        transition: width 180ms ease;
      }
      .plot-render-progress-detail {
        margin-top: 7px;
        color: #475569;
        font-size: 12px;
        line-height: 1.35;
      }
      .stpd-workbench > .row > .col-sm-4,
      .stpd-workbench > .row > .col-sm-8 {
        transition: width 180ms ease, opacity 140ms ease;
      }
      body.stpd-sidebar-collapsed .stpd-workbench > .row > .col-sm-4 {
        display: none !important;
      }
      body.stpd-sidebar-collapsed .stpd-workbench > .row > .col-sm-8 {
        float: none !important;
        width: 100% !important;
        max-width: 100% !important;
        padding-left: 15px;
        padding-right: 15px;
      }
      body.stpd-sidebar-collapsed .stpd-sidebar-toggle-icon {
        background: #dcfce7;
        color: #166534;
      }
      .sidebar-fixed {
        height: calc(100vh - 196px);
        overflow-y: auto;
        padding: 10px 12px;
      }
      .sidebar-fixed.well {
        background: transparent;
        border: 0;
        box-shadow: none;
      }
      .main-fixed {
        height: calc(100vh - 196px);
        overflow: hidden;
      }
      .main-fixed > .tabbable > .nav-tabs {
        border-bottom: 1px solid #dbe3ef;
        margin-bottom: 8px;
      }
      .main-fixed > .tabbable > .nav-tabs > li > a {
        border-radius: 8px 8px 0 0;
        color: #475569;
        padding: 8px 11px;
      }
      .main-fixed > .tabbable > .nav-tabs > li.active > a {
        color: #0f172a;
        font-weight: 700;
        background: #ffffff;
        border-color: #dbe3ef #dbe3ef transparent;
      }
      .plot-scroll {
        overflow-y: auto;
        overflow-x: hidden;
        border: 1px solid #dbe3ef;
        border-radius: 8px;
        padding: 12px;
        margin-bottom: 14px;
        background: #fff;
        box-shadow: 0 6px 18px rgba(15, 23, 42, 0.04);
      }
      .state-space-shell {
        max-height: 85vh;
        padding: 0;
        overflow: auto;
        background: #ffffff;
      }
      .state-space-header {
        display: flex;
        align-items: baseline;
        justify-content: space-between;
        gap: 14px;
        padding: 12px 14px 10px;
        border-bottom: 1px solid #e5e7eb;
      }
      .state-space-title {
        font-size: 15px;
        font-weight: 800;
        color: #111827;
      }
      .state-space-note {
        color: #64748b;
        font-size: 12px;
        line-height: 1.35;
      }
      .state-space-controls {
        padding: 10px 14px;
        border-bottom: 1px solid #edf1f5;
        background: #fbfcfe;
      }
      .state-space-context-controls {
        padding-top: 8px;
        padding-bottom: 8px;
        background: #ffffff;
      }
      .state-space-control-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(178px, 1fr));
        gap: 8px 12px;
        align-items: end;
      }
      .state-space-control-grid-compact {
        grid-template-columns: repeat(auto-fit, minmax(150px, 240px));
      }
      .state-space-control-wide {
        grid-column: span 2;
      }
      .state-space-control .form-group,
      .state-space-control .shiny-input-container {
        width: 100% !important;
        margin-bottom: 0;
      }
      .state-space-control label {
        margin-bottom: 3px;
        font-size: 11px;
        font-weight: 800;
        color: #334155;
        letter-spacing: 0;
      }
      .state-space-control .form-control,
      .state-space-control .selectize-input {
        min-height: 32px;
        border-color: #d9e0ea;
        border-radius: 6px;
        box-shadow: none;
      }
      .state-space-radio .radio-inline {
        margin-right: 12px;
        color: #334155;
        font-weight: 500;
      }
      .state-space-checks {
        display: flex;
        align-items: center;
        gap: 14px;
        min-height: 32px;
        flex-wrap: wrap;
      }
      .state-space-checks .checkbox {
        margin: 0;
      }
      .state-space-main {
        padding: 12px 14px 16px;
      }
      .state-space-main > .tabbable > .nav-pills,
      .state-space-subtabs > .tabbable > .nav-pills {
        border-bottom: 1px solid #e5e7eb;
        margin-bottom: 12px;
      }
      .state-space-main > .tabbable > .nav-pills > li > a,
      .state-space-subtabs > .tabbable > .nav-pills > li > a {
        border-radius: 6px 6px 0 0;
        padding: 7px 11px;
        color: #48607a;
        font-weight: 700;
        background: transparent;
      }
      .state-space-main > .tabbable > .nav-pills > li.active > a,
      .state-space-subtabs > .tabbable > .nav-pills > li.active > a {
        color: #ffffff;
        background: #3f6fa8;
      }
      .state-space-view {
        min-height: 62vh;
      }
      .state-space-figure {
        width: 100%;
        min-height: 360px;
      }
      .state-space-data-drawer {
        margin-top: 10px;
        padding-top: 8px;
        border-top: 1px solid #e5e7eb;
      }
      .state-space-data-drawer > summary {
        cursor: pointer;
        color: #334155;
        font-size: 12px;
        font-weight: 800;
        margin-bottom: 8px;
      }
      .state-space-table-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(360px, 1fr));
        gap: 14px;
      }
      .state-space-table-block {
        min-width: 0;
      }
      .state-space-table-title {
        margin: 0 0 6px;
        font-size: 12px;
        font-weight: 800;
        color: #111827;
      }
      .state-space-shell .dataTables_wrapper {
        font-size: 11.5px;
      }
      .state-space-shell table.dataTable thead th {
        border-bottom: 1px solid #cbd5e1;
        color: #334155;
        font-weight: 800;
      }
      .state-space-shell table.dataTable tbody td {
        border-top: 1px solid #eef2f7;
      }
      @media (max-width: 1100px) {
        .state-space-control-wide {
          grid-column: span 1;
        }
      }
      .soft-box {
        border: 1px solid #dbe3ef;
        border-radius: 8px;
        padding: 12px;
        margin-bottom: 12px;
        background: #ffffff;
        box-shadow: 0 4px 14px rgba(15, 23, 42, 0.04);
      }
      .soft-box.compact {
        padding: 10px 12px;
      }
      .small-note {
        color: #64748b;
        font-size: 12px;
        line-height: 1.45;
      }
      .section-kicker {
        display: inline-block;
        margin-bottom: 6px;
        padding: 2px 7px;
        border-radius: 999px;
        background: #e0f2fe;
        color: #075985;
        font-size: 11px;
        font-weight: 800;
        letter-spacing: 0.02em;
        text-transform: uppercase;
      }
      .stpd-fold {
        border: 1px solid #dbe3ef;
        border-radius: 8px;
        padding: 0;
        background: #ffffff;
      }
      .stpd-fold summary {
        cursor: pointer;
        padding: 10px 12px;
        font-weight: 800;
        color: #1e293b;
        list-style-position: inside;
      }
      .stpd-fold[open] summary {
        border-bottom: 1px solid #e5edf7;
        margin-bottom: 10px;
      }
      .stpd-fold > *:not(summary) {
        margin-left: 12px;
        margin-right: 12px;
      }
      .stpd-fold > *:last-child {
        margin-bottom: 12px;
      }
      .expert-fold summary {
        color: #334155;
      }
      .audit-fold summary {
        color: #7c2d12;
      }
      .parameter-workbench {
        padding: 2px 0;
      }
      .parameter-control-card {
        border: 1px solid #dbe3ef;
        border-radius: 8px;
        background: #ffffff;
        padding: 12px;
        margin-bottom: 12px;
      }
      .parameter-results-card {
        border: 1px solid #dbe3ef;
        border-radius: 8px;
        background: #ffffff;
        padding: 12px;
      }
      .parameter-results-card .nav-pills > li > a {
        padding: 7px 10px;
        border-radius: 8px;
        color: #475569;
      }
      .parameter-results-card .nav-pills > li.active > a {
        background: #2563eb;
        color: #ffffff;
        font-weight: 700;
      }
      .result-hero {
        min-height: 72vh;
        border: 1px solid #dbe3ef;
        border-radius: 8px;
        background: #ffffff;
        padding: 10px;
      }
      .dbs-track-shell {
        max-height: none;
        background: #ffffff;
      }
      .dbs-track-controls {
        border: 1px solid #dbe3ef;
        border-radius: 8px;
        background: #f8fafc;
        padding: 12px;
      }
      .dbs-track-controls .form-group,
      .dbs-track-controls .shiny-input-container {
        width: 100% !important;
      }
      .dbs-track-figure {
        border: 1px solid #e5edf7;
        border-radius: 8px;
        background: #ffffff;
        padding: 8px;
      }
      .dbs-track-caption {
        color: #64748b;
        font-size: 12px;
        line-height: 1.45;
        margin: 8px 0 10px;
      }
      .auto-pattern-legend {
        display: flex;
        flex-wrap: wrap;
        align-items: center;
        gap: 8px 12px;
        padding: 8px 10px;
        margin-bottom: 8px;
        border: 1px solid #e5edf7;
        border-radius: 8px;
        background: #f8fafc;
      }
      .auto-pattern-legend-title {
        color: #334155;
        font-size: 12px;
        font-weight: 800;
        margin-right: 2px;
      }
      .auto-pattern-legend-item {
        display: inline-flex;
        align-items: center;
        gap: 5px;
        color: #475569;
        font-size: 12px;
        line-height: 1.2;
        white-space: nowrap;
      }
      .auto-pattern-swatch {
        display: inline-block;
        width: 18px;
        height: 6px;
        border-radius: 999px;
        box-shadow: inset 0 0 0 1px rgba(15, 23, 42, 0.18);
      }
      .plot-xrange-control {
        width: 100%;
        margin-top: 8px;
        padding: 8px 10px 2px 10px;
        border: 1px solid #e5edf7;
        border-radius: 8px;
        background: #f8fafc;
      }
      .plot-xrange-control .form-group {
        margin-bottom: 4px;
      }
      .plot-xrange-control .irs {
        width: 100%;
      }
      .xrange-nice-ticks {
        position: relative;
        height: 30px;
        margin: 2px 7px 0 7px;
        border-top: 1px solid #cbd5e1;
      }
      .xrange-nice-tick {
        position: absolute;
        top: 0;
        height: 100%;
        transform: translateX(-50%);
      }
      .xrange-nice-tick::before {
        content: '';
        display: block;
        width: 1px;
        height: 8px;
        margin: 0 auto 4px auto;
        background: #475569;
      }
      .xrange-nice-tick-label {
        display: block;
        color: #475569;
        font-size: 11px;
        line-height: 1;
        white-space: nowrap;
      }
      .xrange-nice-tick.is-first {
        transform: translateX(0);
      }
      .xrange-nice-tick.is-first::before {
        margin-left: 0;
      }
      .xrange-nice-tick.is-last {
        transform: translateX(-100%);
      }
      .xrange-nice-tick.is-last::before {
        margin-right: 0;
      }
      .pattern-text-label {
        color: #111827;
      }
      .pattern-isi-box,
      .schema-section-box {
        border: 1px solid #dbe3ef;
        border-left-width: 5px;
        border-radius: 8px;
        background: #ffffff;
        padding: 10px 10px 2px 10px;
        margin: 10px 0;
      }
      .pattern-box-title {
        display: inline-flex;
        align-items: center;
        gap: 7px;
        color: #111827;
        font-size: 13px;
        font-weight: 800;
        margin-bottom: 8px;
      }
      .schema-section-box {
        padding: 0;
        background: #ffffff;
      }
      .schema-section-box summary {
        padding: 10px 12px;
      }
      .status-note {
        border-left: 4px solid #2563eb;
        background: #eff6ff;
        color: #1e3a8a;
        padding: 9px 11px;
        border-radius: 6px;
        margin-bottom: 10px;
      }
      .status-warning {
        border-left-color: #d97706;
        background: #fffbeb;
        color: #78350f;
      }
      .status-error {
        border-left-color: #dc2626;
        background: #fef2f2;
        color: #7f1d1d;
      }
      .schema-contract-group {
        padding: 8px 0 10px 12px;
      }
      .schema-contract-group .form-group {
        margin-bottom: 10px;
      }
      .parameter-validation-summary {
        white-space: pre-wrap;
        background: #f8fafc;
        border: 1px solid #dbe3ef;
        border-radius: 6px;
        padding: 9px 10px;
        margin-bottom: 10px;
        color: #334155;
      }
      .method-warning {
        border-left: 4px solid #d97706;
        background: #fffbeb;
        padding: 10px 12px;
        border-radius: 6px;
        margin-bottom: 12px;
        color: #78350f;
      }
      .btn, .btn-default {
        border-radius: 7px;
      }
      #run_detector, #apply_analysis_preset, #run_parameter_delta_preview,
      #validate_params_now, #run_scientific_validation {
        background: #2563eb;
        border-color: #1d4ed8;
        color: #ffffff;
        font-weight: 800;
      }
      #download_results_zip, #download_parameter_delta_preview_zip, #params_yaml_out {
        background: #0f766e;
        border-color: #0f766e;
        color: #ffffff;
        font-weight: 700;
      }
      .dataTables_wrapper {
        font-size: 12px;
      }
      @media (max-width: 900px) {
        .workflow-strip {
          grid-template-columns: 1fr;
        }
        body.stpd-sidebar-collapsed .stpd-workbench > .row > .col-sm-4 {
          display: none;
        }
        .stpd-workbench-toolbar {
          justify-content: flex-start;
        }
        .sidebar-fixed, .main-fixed {
          height: auto;
          overflow: visible;
        }
      }
      /* Pattern identity is carried by section borders/swatches; labels stay neutral. */
      label[for^='burst_'], label[for^='schema_param_burst__'],
      label[for^='burst_long_'], label[for^='schema_param_burst__long_burst'],
      label[for^='tonic_'], label[for^='schema_param_tonic__'], label[for^='schema_param_state__classic_tonic'],
      label[for^='hf_stable_'], label[for^='schema_param_state__hf_tonic'],
      label[for^='hf_irregular_'], label[for^='hf_spiking_'], label[for^='schema_param_highfreq__'], label[for^='schema_param_state__hf_spiking'],
      label[for^='pause_'], label[for^='schema_param_pause__'],
      label[for^='pattern_min_isi_'], label[for^='pattern_max_isi_'] { color: #111827; font-weight: 700; }
    ")),
    tags$script(HTML("
      (function() {
        var storageKey = 'stpd_sidebar_collapsed';
        function resizePlots() {
          try {
            window.dispatchEvent(new Event('resize'));
            if (window.Plotly) {
              document.querySelectorAll('.js-plotly-plot').forEach(function(el) {
                try { window.Plotly.Plots.resize(el); } catch (err) {}
              });
            }
          } catch (err) {}
        }
        function setCollapsed(collapsed) {
          document.body.classList.toggle('stpd-sidebar-collapsed', collapsed);
          try { window.localStorage.setItem(storageKey, collapsed ? '1' : '0'); } catch (err) {}
          var btn = document.getElementById('stpd_sidebar_toggle');
          if (btn) {
            btn.setAttribute('aria-expanded', collapsed ? 'false' : 'true');
            var text = btn.querySelector('.stpd-sidebar-toggle-text');
            if (text) text.textContent = collapsed ? '\u5C55\u5F00\u63A7\u5236\u680F' : '\u6298\u53E0\u63A7\u5236\u680F';
            var icon = btn.querySelector('.stpd-sidebar-toggle-icon');
            if (icon) icon.textContent = collapsed ? '\u203A' : '\u2039';
          }
          window.setTimeout(resizePlots, 40);
          window.setTimeout(resizePlots, 260);
        }
        document.addEventListener('DOMContentLoaded', function() {
          var saved = false;
          try { saved = window.localStorage.getItem(storageKey) === '1'; } catch (err) {}
          setCollapsed(saved);
          var btn = document.getElementById('stpd_sidebar_toggle');
          if (btn) {
            btn.addEventListener('click', function() {
              setCollapsed(!document.body.classList.contains('stpd-sidebar-collapsed'));
            });
          }
        });
      })();
    ")),
	    tags$script(HTML("
	      (function() {
	        function clampPercent(value) {
          value = Number(value);
          if (!isFinite(value)) value = 0;
          if (value <= 1) value = value * 100;
          return Math.max(0, Math.min(100, value));
        }
        function setDataLoadProgress(payload) {
          payload = payload || {};
          var el = document.getElementById('stpd_data_load_progress');
          if (!el) return;
          var type = payload.type || 'active';
          var percent = clampPercent(payload.value == null ? 0 : payload.value);
	          var message = payload.message || '\u6b63\u5728\u52a0\u8f7d spike train \u6570\u636e';
          var detail = payload.detail || '';
          el.style.display = 'block';
          el.classList.remove('is-active', 'is-success', 'is-error', 'is-idle');
          el.classList.add(type === 'success' ? 'is-success' : (type === 'error' ? 'is-error' : 'is-active'));
          var title = el.querySelector('.data-load-progress-title');
          var pct = el.querySelector('.data-load-progress-percent');
          var bar = el.querySelector('.data-load-progress-bar');
          var detailEl = el.querySelector('.data-load-progress-detail');
          if (title) title.textContent = message;
          if (pct) pct.textContent = Math.round(percent) + '%';
          if (bar) bar.style.width = percent + '%';
          if (detailEl) detailEl.textContent = detail;
        }
        window.stpdSetDataLoadProgress = setDataLoadProgress;
        var handlerRegistered = false;
        function registerShinyHandler() {
          if (window.Shiny && window.Shiny.addCustomMessageHandler) {
            if (handlerRegistered) return;
            window.Shiny.addCustomMessageHandler('stpdDataLoadProgress', setDataLoadProgress);
            handlerRegistered = true;
          }
        }
        document.addEventListener('change', function(event) {
          var input = event.target;
          if (!input || ['file_raw', 'file_annot', 'workspace_in'].indexOf(input.id) < 0) return;
          if (input.files && input.files.length > 0) {
            setDataLoadProgress({
              type: 'active',
              value: 0.03,
	              message: '\u6b63\u5728\u4e0a\u4f20\u6587\u4ef6',
	              detail: '\u4e0a\u4f20\u5b8c\u6210\u540e\uff0c\u670d\u52a1\u5668\u4f1a\u7ee7\u7eed\u89e3\u6790 spike train / \u5de5\u4f5c\u533a\u6570\u636e\u3002'
            });
            if (typeof window.stpdSetPlotRenderProgress === 'function') {
              window.stpdSetPlotRenderProgress({
                outputId: 'raster_plot',
                type: 'active',
                value: 0.03,
	                message: '\u6b63\u5728\u4e0a\u4f20 spike train \u5e76\u51c6\u5907 plot \u89c6\u56fe',
	                detail: '\u4e0a\u4f20\u5b8c\u6210\u540e\uff0c\u53f3\u4fa7\u4f1a\u7ee7\u7eed\u663e\u793a\u89e3\u6790\u548c plot \u6e32\u67d3\u8fdb\u5ea6\u3002'
              });
            }
          }
        }, true);
        document.addEventListener('DOMContentLoaded', function() {
          registerShinyHandler();
          window.setTimeout(registerShinyHandler, 500);
          window.setTimeout(registerShinyHandler, 1500);
        });
	        document.addEventListener('shiny:connected', registerShinyHandler);
        registerShinyHandler();
        window.setTimeout(registerShinyHandler, 500);
        window.setTimeout(registerShinyHandler, 1500);
	      })();
	    ")),
	    tags$script(HTML("
	      (function() {
	        function clampPercent(value) {
	          value = Number(value);
	          if (!isFinite(value)) value = 0;
	          if (value <= 1) value = value * 100;
	          return Math.max(0, Math.min(100, value));
	        }
	        function progressElement(outputId) {
	          if (!outputId) outputId = 'raster_plot';
	          return document.getElementById(outputId + '_progress');
	        }
	        var plotProgressTimers = {};
	        var rasterProgressSuppressUntil = 0;
	        function progressPercent(el) {
	          if (!el) return 0;
	          var pct = el.querySelector('.plot-render-progress-percent');
	          if (pct) {
	            var parsed = Number(String(pct.textContent || '').replace(/[^0-9.]/g, ''));
	            if (isFinite(parsed)) return parsed;
	          }
	          var bar = el.querySelector('.plot-render-progress-bar');
	          if (bar) {
	            var width = Number(String(bar.style.width || '').replace(/[^0-9.]/g, ''));
	            if (isFinite(width)) return width;
	          }
	          return 0;
	        }
	        function writeProgressPercent(el, percent) {
	          percent = Math.max(0, Math.min(100, Number(percent) || 0));
	          var pct = el.querySelector('.plot-render-progress-percent');
	          var bar = el.querySelector('.plot-render-progress-bar');
	          if (pct) pct.textContent = Math.round(percent) + '%';
	          if (bar) bar.style.width = percent + '%';
	        }
	        function stopPlotProgressTicker(outputId) {
	          outputId = outputId || 'raster_plot';
	          if (plotProgressTimers[outputId]) {
	            window.clearInterval(plotProgressTimers[outputId]);
	            delete plotProgressTimers[outputId];
	          }
	        }
	        function startPlotProgressTicker(outputId) {
	          outputId = outputId || 'raster_plot';
	          if (plotProgressTimers[outputId]) return;
	          plotProgressTimers[outputId] = window.setInterval(function() {
	            var el = progressElement(outputId);
	            if (!el || el.style.display === 'none' || !el.classList.contains('is-active')) {
	              stopPlotProgressTicker(outputId);
	              return;
	            }
	            var current = progressPercent(el);
	            if (current >= 95) return;
	            var step = Math.max(0.7, Math.min(4, (95 - current) * 0.08));
	            writeProgressPercent(el, Math.min(95, current + step));
	          }, 350);
	        }
	        function setPlotProgress(payload) {
	          payload = payload || {};
	          var outputId = payload.outputId || payload.output || payload.id || 'raster_plot';
	          var el = progressElement(outputId);
	          if (!el) return;
	          var type = payload.type || 'active';
	          if (outputId === 'raster_plot' && type === 'active' && Date.now() < rasterProgressSuppressUntil) {
	            stopPlotProgressTicker(outputId);
	            el.classList.remove('is-active', 'is-success', 'is-error');
	            el.style.display = 'none';
	            return;
	          }
	          if (type === 'hide' || type === 'idle') {
	            stopPlotProgressTicker(outputId);
	            el.classList.remove('is-active', 'is-success', 'is-error');
	            el.style.display = 'none';
	            return;
	          }
	          var wasActive = el.style.display !== 'none' && el.classList.contains('is-active');
	          var percent = clampPercent(payload.value == null ? 0.08 : payload.value);
	          if (type === 'active' && wasActive) {
	            percent = Math.max(percent, progressPercent(el));
	          }
	          var title = el.querySelector('.plot-render-progress-title');
	          var detail = el.querySelector('.plot-render-progress-detail');
	          el.style.display = 'block';
	          el.classList.remove('is-active', 'is-success', 'is-error');
	          el.classList.add(type === 'success' ? 'is-success' : (type === 'error' ? 'is-error' : 'is-active'));
		          if (title) title.textContent = payload.message || '\u6b63\u5728\u751f\u6210 plot \u89c6\u56fe';
	          writeProgressPercent(el, percent);
		          if (detail) detail.textContent = payload.detail || '\u6b63\u5728\u6574\u7406\u53ef\u89c1 spike train \u548c\u6a21\u5f0f\u6807\u7b7e\u3002';
	          if (type === 'active') startPlotProgressTicker(outputId);
	          if (type === 'error') stopPlotProgressTicker(outputId);
	          if (type === 'success') {
	            stopPlotProgressTicker(outputId);
	            window.setTimeout(function() {
	              var now = progressElement(outputId);
	              if (now && now.classList.contains('is-success')) {
	                now.classList.remove('is-active', 'is-success', 'is-error');
	                now.style.display = 'none';
	              }
	            }, 900);
	          }
	        }
	        function suppressRasterProgressForInteraction() {
	          rasterProgressSuppressUntil = Date.now() + 2000;
	          setPlotProgress({ outputId: 'raster_plot', type: 'hide' });
	        }
	        function isRasterWindowControl(event) {
	          var el = event && event.target;
	          while (el && el !== document) {
	            if (['xrange', 'xrange_plot', 'xrange_window_length', 'xrange_plot_window_length'].indexOf(el.id) >= 0) return true;
	            if (el.matches && el.matches('#xrange, #xrange_plot, #xrange_window_length, #xrange_plot_window_length')) return true;
	            el = el.parentElement;
	          }
	          return false;
	        }
	        function hideRasterProgressSoon() {
	          var existing = progressElement('raster_plot');
	          if (!existing || existing.style.display === 'none') return;
	          if (!existing.classList.contains('is-active') && !existing.classList.contains('is-error')) return;
	          window.setTimeout(function() {
	            setPlotProgress({
	              outputId: 'raster_plot',
	              type: 'success',
	              value: 1,
		              message: 'plot \u89c6\u56fe\u5df2\u5b8c\u6210',
	              detail: ''
	            });
	          }, 180);
	        }
	        function bindPlotlyAfterPlot(outputId) {
	          var root = document.getElementById(outputId);
	          if (!root) return;
	          var graph = root.classList && root.classList.contains('js-plotly-plot') ? root : root.querySelector('.js-plotly-plot');
	          if (!graph || graph._stpdPlotProgressBound || !graph.on) return;
	          graph._stpdPlotProgressBound = true;
	          graph.on('plotly_afterplot', function() {
	            hideRasterProgressSoon();
	          });
	        }
	        function handleInvalidated(event) {
	          // Ordinary raster invalidations include slider/pan window redraws.
	          // Those are quick interaction updates and should not show a loading
	          // overlay. The server explicitly opens progress only for data-load
	          // and first-render work.
	        }
	        function handleChanged(event) {
	          var target = event && event.target;
	          var id = target && target.id;
	          if (id !== 'raster_plot') return;
	          bindPlotlyAfterPlot(id);
	          window.setTimeout(function() {
	            bindPlotlyAfterPlot(id);
	            hideRasterProgressSoon();
	          }, 1800);
	        }
	        function handleError(event) {
	          var target = event && event.target;
	          var id = target && target.id;
	          if (id === 'raster_plot') {
	            var existing = progressElement('raster_plot');
	            var msg = '';
	            if (event && typeof event.message === 'string') msg = event.message;
	            if (!msg && event && event.detail) {
	              msg = String(event.detail.message || event.detail.error || event.detail || '');
	            }
	            if (/event tied a source ID|is not registered/.test(msg)) return;
	            if (!existing || existing.style.display === 'none' || !existing.classList.contains('is-active')) return;
	            setPlotProgress({
	              outputId: 'raster_plot',
	              type: 'error',
	              value: 1,
		              message: 'plot \u89c6\u56fe\u751f\u6210\u5931\u8d25',
		              detail: '\u8bf7\u67e5\u770b R console \u4e2d\u7684\u9519\u8bef\u4fe1\u606f\u3002'
	            });
	          }
	        }
	        function registerPlotHandler() {
	          if (window.Shiny && window.Shiny.addCustomMessageHandler && !window._stpdPlotProgressHandlerRegistered) {
	            window.Shiny.addCustomMessageHandler('stpdPlotRenderProgress', setPlotProgress);
	            window._stpdPlotProgressHandlerRegistered = true;
	          }
	        }
	        document.addEventListener('shiny:outputinvalidated', handleInvalidated);
	        document.addEventListener('shiny:outputchanged', handleChanged);
	        document.addEventListener('shiny:error', handleError);
	        document.addEventListener('input', function(event) {
	          if (isRasterWindowControl(event)) suppressRasterProgressForInteraction();
	        }, true);
	        document.addEventListener('change', function(event) {
	          if (isRasterWindowControl(event)) suppressRasterProgressForInteraction();
	        }, true);
	        document.addEventListener('DOMContentLoaded', function() {
	          registerPlotHandler();
	          window.setTimeout(registerPlotHandler, 500);
	          window.setTimeout(registerPlotHandler, 1500);
	        });
	        document.addEventListener('shiny:connected', registerPlotHandler);
	        registerPlotHandler();
	        window.setTimeout(registerPlotHandler, 500);
	        window.setTimeout(registerPlotHandler, 1500);
	        if (window.jQuery) {
	          window.jQuery(document).on('shiny:outputinvalidated', handleInvalidated);
	          window.jQuery(document).on('shiny:outputchanged', handleChanged);
	          window.jQuery(document).on('shiny:error', handleError);
	        }
	        window.stpdSetPlotRenderProgress = setPlotProgress;
	      })();
	    ")),
	    stpd_i18n_assets()
	  ),

  div(
    class = "app-header",
    div(
      class = "app-header-main",
      div(
        class = "app-title-block",
        div(class = "app-title", "Spike Train Pattern Detector"),
        div(
          class = "app-subtitle",
          "\u57FA\u5E95\u795E\u7ECF\u8282 spike train \u5DE5\u4F5C\u53F0\uFF1A\u5BFC\u5165\u6570\u636E\u3001\u8BBE\u7F6E\u5173\u952E\u53C2\u6570\u3001\u8FD0\u884C\u4E8B\u4EF6\u8BED\u6CD5\u68C0\u6D4B\uFF0C\u7136\u540E\u590D\u6838\u5DEE\u5F02\u3001\u9A8C\u8BC1\u548C\u5BFC\u51FA\u3002"
        )
      ),
      div(
        class = "stpd-language-toggle",
        tags$span(class = "stpd-language-label", "\u8BED\u8A00 / Language"),
        radioButtons("ui_language", NULL, choices = c("\u4E2D\u6587" = "zh", "English" = "en"), selected = "zh", inline = TRUE)
      )
    )
  ),

  div(
    class = "workflow-strip",
    div(class = "workflow-step",
        div(class = "workflow-index", "1"),
        div(div(class = "workflow-title", "\u5BFC\u5165\u6570\u636E"),
            div(class = "workflow-note", "CSV / RDS\uFF0CQC \u548C\u6570\u636E\u96C6\u9009\u62E9"))),
    div(class = "workflow-step",
        div(class = "workflow-index", "2"),
        div(div(class = "workflow-title", "\u5173\u952E\u53C2\u6570"),
            div(class = "workflow-note", "Basic \u53C2\u6570\u4F18\u5148\uFF0C\u4E13\u5BB6\u9879\u6298\u53E0"))),
    div(class = "workflow-step",
        div(class = "workflow-index", "3"),
        div(div(class = "workflow-title", "\u8FD0\u884C\u68C0\u6D4B"),
            div(class = "workflow-note", "\u9884\u8BBE + \u5F53\u524D train \u5FEB\u901F\u6267\u884C"))),
    div(class = "workflow-step",
        div(class = "workflow-index", "4"),
        div(div(class = "workflow-title", "\u5DEE\u5F02 / \u9A8C\u8BC1"),
            div(class = "workflow-note", "delta preview\u3001IoU\u3001\u654F\u611F\u6027"))),
    div(class = "workflow-step",
        div(class = "workflow-index", "5"),
        div(div(class = "workflow-title", "\u5BFC\u51FA\u7ED3\u679C"),
            div(class = "workflow-note", "CSV / ZIP / YAML \u53EF\u590D\u73B0\u8BB0\u5F55")))
  ),
  
  div(
    class = "stpd-workbench",
    div(
      class = "stpd-workbench-toolbar",
      tags$button(
        id = "stpd_sidebar_toggle",
        type = "button",
        class = "stpd-sidebar-toggle",
        `aria-expanded` = "true",
        title = "\u6298\u53E0/\u5C55\u5F00\u5DE6\u4FA7\u63A7\u5236\u680F",
        tags$span(class = "stpd-sidebar-toggle-icon", "\u2039"),
        tags$span(class = "stpd-sidebar-toggle-text", "\u6298\u53E0\u63A7\u5236\u680F")
      ),
      tags$span(class = "stpd-workbench-hint", "\u6298\u53E0\u540E\u53F3\u4FA7\u56FE\u4F1A\u81EA\u52A8\u91CD\u7B97\u5BBD\u5EA6")
    ),
    sidebarLayout(
    sidebarPanel(
      class = "sidebar-fixed",

      div(class = "method-warning",
          strong("\u89E3\u91CA\u6CE8\u610F\u4E8B\u9879"),
          br(),
          "\u672C\u7A0B\u5E8F\u751F\u6210\u5019\u9009\u4E8B\u4EF6\u548C\u53EF\u590D\u6838\u6807\u7B7E\uFF0C\u5E76\u4E0D\u662F\u65E0\u504F\u7684\u6700\u7EC8\u5206\u7C7B\u5668\u3002\u8BF7\u5206\u522B\u62A5\u544A\u9AD8\u7F6E\u4FE1\u4E8B\u4EF6\u3001\u5F85\u590D\u6838\u5019\u9009\u4E8B\u4EF6\u548C burst-family \u6C47\u603B\u7ED3\u679C\u3002"
      ),

      div(class = "soft-box",
          div(class = "section-kicker", "\u5BFC\u5165"),
          h4("\u6570\u636E\u4E0E QC"),
          radioButtons("unit_in_raw", "\u539F\u59CB\u6587\u4EF6\u65F6\u95F4\u5355\u4F4D", choices = c("s", "ms"), inline = TRUE),
          checkboxInput("header_raw", "\u539F\u59CB CSV \u7B2C\u4E00\u884C\u5305\u542B\u5217\u540D", TRUE),
          fileInput("file_raw", "\u4E0A\u4F20\u539F\u59CB\u65F6\u95F4\u6233 CSV \u6587\u4EF6", accept = ".csv", multiple = TRUE),
          tags$div(
            id = "stpd_data_load_progress",
            class = "data-load-progress is-idle",
            style = "display:none;",
            role = "status",
            `aria-live` = "polite",
            tags$div(
              class = "data-load-progress-head",
              tags$span(class = "data-load-progress-title", "\u7B49\u5F85\u52A0\u8F7D"),
              tags$span(class = "data-load-progress-percent", "0%")
            ),
            tags$div(class = "data-load-progress-track", tags$div(class = "data-load-progress-bar")),
            tags$div(class = "data-load-progress-detail", "")
          ),
          tags$hr(),
          radioButtons("unit_in_annot", "\u5DF2\u6807\u8BB0\u6587\u4EF6\u65F6\u95F4\u5355\u4F4D", choices = c("s", "ms"), inline = TRUE),
          fileInput("file_annot", "\u4E0A\u4F20\u5DF2\u6807\u8BB0 CSV \u6587\u4EF6", accept = ".csv", multiple = TRUE),
          tags$hr(),
          fileInput("workspace_in", "\u52A0\u8F7D\u5DE5\u4F5C\u533A\uFF08.rds\uFF09", accept = ".rds"),
          fluidRow(
            column(6, downloadButton("workspace_out", "\u4FDD\u5B58\u5DE5\u4F5C\u533A", width = "100%")),
            column(6, actionButton("clear_all_datasets", "\u6E05\u7A7A\u5185\u5B58", width = "100%"))
          ),
          tags$hr(),
          radioButtons("qc_isi_unit", "\u4F2A\u8FF9/\u4E0D\u5E94\u671F\u9608\u503C\u5355\u4F4D", choices = c("ms", "s"), selected = "ms", inline = TRUE),
          numericInput("artifact_isi_ms", "\u4F2A\u8FF9 / \u6700\u5C0F\u6709\u6548 ISI \u9608\u503C", value = 0.9, min = 0, step = 0.1),
          numericInput("refractory_suspect_ms", "\u7591\u4F3C\u4E0D\u5E94\u671F ISI \u9608\u503C", value = 1.0, min = 0, step = 0.1),
          selectInput("refractory_suspect_action", "burst \u68C0\u6D4B\u4E2D\u7591\u4F3C\u4E0D\u5E94\u671F ISI \u7684\u5904\u7406\u65B9\u5F0F",
                      choices = c("\u4EC5\u8B66\u544A" = "warn_only",
                                  "\u5C06 burst \u964D\u7EA7\u4E3A\u53EF\u590D\u6838 possible_burst" = "demote_to_possible",
                                  "\u5728\u7591\u4F3C ISI \u5904\u5207\u5206\u5019\u9009" = "split_at_suspect",
                                  "\u6392\u9664\u7591\u4F3C ISI \u5E76\u91CD\u65B0\u8BC4\u4F30\u7247\u6BB5" = "exclude_suspect_isi_and_reevaluate",
                                  "\u62D2\u7EDD\u6574\u4E2A burst \u5019\u9009" = "exclude_candidate",
                                  "\u6807\u8BB0\u53EF\u80FD\u5B58\u5728\u591A\u5355\u5143\u6C61\u67D3" = "mark_multiunit_contamination"),
                      selected = "demote_to_possible"),
          tags$div(class = "small-note", "\u4F2A\u8FF9\u9608\u503C\u662F\u786C\u6027\u7684\u6700\u5C0F\u6709\u6548 ISI\u3002\u7591\u4F3C\u4E0D\u5E94\u671F ISI \u4ECD\u4FDD\u7559\u4E3A\u6709\u6548 ISI\uFF0C\u4F46\u4F1A\u88AB\u6807\u8BB0\u4E3A\u53EF\u7591\uFF1B\u9ED8\u8BA4\u7B56\u7565\u4F1A\u5C06\u53D7\u5F71\u54CD\u7684 burst \u5019\u9009\u964D\u7EA7\u4E3A possible_burst \u4EE5\u4FBF\u590D\u6838\u3002"),
          tags$details(
            class = "stpd-fold expert-fold",
            tags$summary("\u9AD8\u7EA7 QC\uFF1A\u91CD\u590D timestamp \u4E0E\u5408\u5E76"),
          selectInput("duplicate_timestamp_policy", "\u6BCF\u6761 spike train \u5185\u91CD\u590D\u65F6\u95F4\u6233\u7684\u5904\u7406\u7B56\u7565",
                      choices = c("\u62A5\u9519\uFF1A\u4FDD\u6301\u4E0D\u53D8" = "error_keep",
                                  "\u8B66\u544A\uFF1A\u4FDD\u6301\u4E0D\u53D8" = "warn_keep",
                                  "\u5408\u5E76\u5B8C\u5168\u91CD\u590D\u65F6\u95F4\u6233" = "collapse_exact"),
                      selected = "error_keep"),
          tags$div(class = "small-note", "\u5B8C\u5168\u91CD\u590D timestamp \u4F1A\u4EA7\u751F 0 ISI\u3002\u53EA\u6709\u5728\u786E\u8BA4\u91CD\u590D\u884C\u662F\u5BFC\u51FA\u91CD\u590D\u800C\u975E\u4E0D\u540C\u5355\u4F4D/\u4E8B\u4EF6\u65F6\uFF0C\u624D\u5E94\u5408\u5E76\u3002"),
          fluidRow(
            column(6, actionButton("collapse_duplicate_spikes_current", "\u5408\u5E76\u5F53\u524D\u6570\u636E\u96C6\u4E2D\u7684\u91CD\u590D timestamp", width = "100%")),
            column(6, actionButton("collapse_duplicate_spikes_all", "\u5408\u5E76\u6240\u6709\u6570\u636E\u96C6\u4E2D\u7684\u91CD\u590D timestamp", width = "100%"))
          ),
          tags$div(class = "small-note", "\u4E00\u952E\u5408\u5E76\u4F1A\u5220\u9664\u6BCF\u6761 train \u5185\u5B8C\u5168\u91CD\u590D\u7684 spike timestamp\uFF0C\u4EC5\u4FDD\u7559\u7B2C\u4E00\u6B21\u51FA\u73B0\u3002\u4FDD\u7559 spike \u4E0A\u7684 MANUAL \u6807\u7B7E\u4F1A\u88AB\u4FDD\u7559\uFF1BAUTO \u7ED3\u679C\u4F1A\u88AB\u6E05\u7A7A\uFF0C\u9700\u8981\u91CD\u65B0\u8FD0\u884C\u68C0\u6D4B\u3002")
          ),
          uiOutput("dataset_selector"),
          uiOutput("pool_dataset_selector"),
          actionButton("remove_dataset", "\u79FB\u9664\u5F53\u524D\u6570\u636E\u96C6", width = "100%")
      ),

      tags$details(
          class = "soft-box stpd-fold expert-fold",
          tags$summary("\u9AD8\u7EA7\uFF1A\u663E\u793A\u3001\u7B5B\u9009\u4E0E\u53E0\u52A0\u5C42"),
          tags$div(class = "small-note", "\u5E38\u89C4\u4F7F\u7528\u65F6\u4FDD\u6301\u9ED8\u8BA4\u5373\u53EF\u3002\u8FD9\u4E9B\u63A7\u4EF6\u53EA\u5F71\u54CD\u56FE\u50CF\u5448\u73B0\u548C\u5C40\u90E8\u6D4F\u89C8\u3002"),
          radioButtons("time_unit", "\u663E\u793A\u5355\u4F4D", choices = c("s", "ms"), inline = TRUE, selected = "ms"),
          radioButtons("train_display_mode", "Train \u663E\u793A\u6A21\u5F0F\uFF08\u8BB0\u5F55\u6761\u76EE\u663E\u793A\u6A21\u5F0F\uFF09",
                       choices = c("\u5168\u90E8 train\uFF0C\u5206\u9875\u663E\u793A" = "paged_all", "\u624B\u52A8\u9009\u62E9 train\uFF08\u8BB0\u5F55\u6761\u76EE\uFF09" = "selected_only"),
                       selected = "paged_all"),
          checkboxInput("use_train_metadata_filter", "\u4F7F\u7528\u89E3\u6790\u51FA\u7684 train \u5143\u6570\u636E\u8FC7\u6EE4\u5668", FALSE),
          uiOutput("train_metadata_filters"),
          tags$details(
            tags$summary("\u89E3\u6790\u51FA\u7684 train \u5143\u6570\u636E / \u5217\u540D\u5206\u7EC4"),
            tags$div(class = "small-note", "\u7CFB\u7EDF\u4F1A\u5C3D\u53EF\u80FD\u4ECE spike-train \u5217\u540D\u63A8\u65AD\u7ED3\u6784\u3001\u5DE6\u53F3\u4FA7\u3001\u8F68\u8FF9\u3001\u6DF1\u5EA6\u3001wire/unit \u548C flag\u3002"),
            DT::DTOutput("train_metadata_table")
          ),
          fluidRow(
            column(6, numericInput("visible_trains_per_page", "\u6BCF\u9875\u53EF\u89C1 train \u6570", value = 10, min = 1, max = 50, step = 1)),
            column(6, uiOutput("train_page_selector"))
          ),
          uiOutput("train_selector"),
          sliderInput("xrange", "\u663E\u793A\u65F6\u95F4\u7A97", min = 0, max = 1000, value = c(0, 1000), step = 1, ticks = FALSE),
          numericInput("xrange_window_length", "\u663E\u793A\u65F6\u95F4\u7A97\u957F\u5EA6\uFF08\u5F53\u524D\u5355\u4F4D\uFF09", value = 1000, min = 0.001, step = 1),
          uiOutput("xrange_ticks"),
          sliderInput("spike_height", "Spike \u7AD6\u7EBF\u9AD8\u5EA6\uFF08\u5CF0\u7535\u4F4D\u7EBF\u9AD8\u5EA6\uFF09", min = 0.1, max = 0.9, value = 0.6, step = 0.05),
          tags$div(class = "small-note", "\u6240\u6709 spike tick \u5747\u7ED8\u5236\u4E3A\u76F8\u540C\u7684\u9ED1\u8272\u5B9E\u7EBF\uFF1B\u6A21\u5F0F/\u6765\u6E90\u4FE1\u606F\u53EA\u901A\u8FC7\u6C34\u5E73\u6761\u5E26\u548C\u53E0\u52A0\u5C42\u663E\u793A\uFF0C\u4E0D\u901A\u8FC7 spike \u989C\u8272\u6DF1\u6D45\u6216\u7C97\u7EC6\u8868\u793A\u3002"),
          tags$small("Mean-ISI \u4E0E Pasquale logISIH/newBD \u652F\u6301\u5C42\u4EC5\u63D0\u4F9B\u9608\u503C\u8BC1\u636E\uFF1BAUTO \u6807\u7B7E\u4ECD\u7531\u4E3B\u68C0\u6D4B\u5668\u548C\u590D\u6838\u6D41\u7A0B\u63A7\u5236\u3002"),
          tags$hr(),
          checkboxInput("show_others", "\u5728 raster \u4E2D\u7ED8\u5236\u81EA\u52A8/\u9690\u5F0F\u201C\u5176\u4ED6\u201D", FALSE),
          checkboxInput("show_manual_others_always", "\u59CB\u7EC8\u7ED8\u5236\u624B\u52A8\u6807\u8BB0\u7684 others", TRUE),
	          checkboxInput("show_possible", "\u5728 raster \u4E2D\u7ED8\u5236\u201C\u7591\u4F3C burst\u201D", TRUE),
	          checkboxInput("show_near_miss_preview", "\u7ED8\u5236\u9009\u4E2D\u5019\u9009 / near-miss \u9884\u89C8\u53E0\u52A0\u5C42", FALSE),
	          checkboxInput("show_parameter_delta_overlay", "\u7ED8\u5236\u53C2\u6570\u5DEE\u5F02 dry-run \u4E8B\u4EF6\u53E0\u52A0\u5C42", TRUE),
	          checkboxInput("show_rejected_burst_candidates", "\u7ED8\u5236\u88AB\u62D2\u7EDD/\u964D\u7EA7\u7684 burst-like \u5019\u9009\u5BA1\u8BA1\u53E0\u52A0\u5C42", FALSE),
	          checkboxInput("show_burst_sublabel_structures", "\u7ED8\u5236 burst \u9644\u5C5E interesting-structure \u53E0\u52A0\u5C42", TRUE),
          checkboxInput("show_task_events", "\u663E\u793A\u4EFB\u52A1/\u884C\u4E3A\u4E8B\u4EF6\u865A\u7EBF", TRUE),
          uiOutput("task_event_selector"),
          fluidRow(
            column(6, numericInput("task_event_jump_pre_sec", "\u4E8B\u4EF6\u8DF3\u8F6C\u524D\u7A97\u53E3\uFF08s\uFF09", value = 1, min = 0, step = 0.1)),
            column(6, numericInput("task_event_jump_post_sec", "\u4E8B\u4EF6\u8DF3\u8F6C\u540E\u7A97\u53E3\uFF08s\uFF09", value = 2, min = 0.05, step = 0.1))
          ),
          actionButton("jump_to_task_event", "\u8DF3\u8F6C\u5230\u9009\u4E2D\u4EFB\u52A1\u4E8B\u4EF6", width = "100%"),
          checkboxInput("auto_others", "\u5728 FINAL \u89C6\u56FE\u4E2D\u5C06\u5269\u4F59\u6709\u6548 ISI \u81EA\u52A8\u6807\u4E3A\u201C\u5176\u4ED6\u201D", FALSE),
          radioButtons("pattern_view", "Raster \u53E0\u52A0\u6807\u7B7E", choices = c("\u6700\u7EC8\u5BA1\u8BA1\u7ED3\u679C" = "audit_final", "\u6700\u7EC8\u6807\u7B7E" = "final", "\u624B\u52A8\u6807\u8BB0" = "manual", "\u81EA\u52A8\u68C0\u6D4B" = "auto"), inline = TRUE, selected = "audit_final"),
          tags$div(class = "small-note", "Raster \u6807\u7B7E\u663E\u793A\uFF1A\u4EC5\u4F7F\u7528\u6A21\u5F0F\u989C\u8272\u6761\u5E26\uFF1B\u5782\u76F4 spike tick \u4FDD\u6301\u7EDF\u4E00\u9ED1\u8272\u5B9E\u7EBF\u3002"),
          checkboxInput("show_extended_isi_metrics", "hover \u4E2D\u663E\u793A\u6269\u5C55 ISI \u533A\u95F4\u6307\u6807", FALSE),
          selectInput("plot_lod_mode", "Raster \u7EC6\u8282\u5C42\u7EA7", choices = c("\u81EA\u52A8" = "auto", "\u5B8C\u6574\u4EA4\u4E92" = "full", "\u7B80\u5316\u60AC\u505C/\u9009\u62E9" = "reduced"), selected = "auto"),
          fluidRow(
            column(6, numericInput("plot_max_visible_spikes_full", "\u5B8C\u6574\u663E\u793A spike \u4E0A\u9650", value = 50000, min = 1000, step = 5000)),
            column(6, numericInput("plot_max_visible_spikes_interactive", "\u4EA4\u4E92 spike \u4E0A\u9650", value = 100000, min = 5000, step = 5000))
          )
      ),

      tags$details(
          class = "soft-box stpd-fold expert-fold",
          tags$summary("\u9AD8\u7EA7\uFF1A\u624B\u52A8\u6807\u8BB0 / \u6821\u6B63"),
          tags$div(class = "small-note", "\u9700\u8981\u534A\u76D1\u7763\u6821\u51C6\u6216\u91D1\u6807\u7B7E\u65F6\u518D\u5C55\u5F00\u3002"),
          radioButtons("pattern", "\u9009\u62E9\u6A21\u5F0F", choiceNames = stpd_ui_pattern_choice_names(stpd_ui_pattern_order(TRUE)), choiceValues = stpd_ui_pattern_order(TRUE), inline = TRUE),
          helpText("Aligned raster\uFF1A\u8BF7\u4F7F\u7528\u6846\u9009\u3002burst/tonic \u9700\u8981\u4ECE\u540C\u4E00\u6761 train \u4E2D\u9009\u62E9\u81F3\u5C11 2 \u4E2A spike\uFF1Bpause/others/high-frequency \u548C NOT-burst \u5F3A\u8D1F\u4F8B\u53EF\u4F7F\u7528\u65F6\u95F4\u8303\u56F4\u9009\u62E9\u3002"),
          tags$div(class = "small-note", "\u89E3\u91CA\uFF1A\u9AD8\u9891\u5F3A\u76F4\u53D1\u653E = \u591A\u4E2A\u77ED ISI \u4E14\u53D8\u5F02\u6027\u4F4E\uFF1B\u9AD8\u9891\u8FDE\u7EED\u53D1\u653E = \u591A\u4E2A\u77ED ISI\uFF0C\u4F46 ISI \u957F\u77ED\u4E0D\u89C4\u5219\u3002"),
	          checkboxInput("auto_label_selection", "\u6BCF\u6B21\u65B0\u6846\u9009\u540E\u81EA\u52A8\u7528\u5F53\u524D\u6A21\u5F0F\u6807\u8BB0", FALSE),
	          actionButton("add_annot", "\u6807\u8BB0\u5F53\u524D/\u7F13\u5B58\u9009\u62E9\uFF08MANUAL\uFF09", width = "100%"),
	          actionButton("undo_last_manual_action", "\u64A4\u9500\u4E0A\u4E00\u6B21\u624B\u52A8\u64CD\u4F5C", width = "100%"),
	          actionButton("clear_cached_selection", "\u6E05\u9664\u7F13\u5B58\u9009\u62E9", width = "100%"),
          fluidRow(
            column(6, actionButton("set_cluster_a", "\u8BBE\u7F6E\u6240\u9009\u7C07 A", width = "100%")),
            column(6, actionButton("set_cluster_b", "\u8BBE\u7F6E\u6240\u9009\u7C07 B", width = "100%"))
          ),
          tags$div(class = "small-note", "\u4F7F\u7528\u7C07 A/B \u6BD4\u8F83\u4E24\u4E2A\u89C6\u89C9\u76F8\u4F3C\u7684\u7C07\uFF0C\u67E5\u770B\u4E00\u4E2A\u88AB\u63A5\u53D7\u800C\u53E6\u4E00\u4E2A\u88AB\u62D2\u7EDD\u7684\u539F\u56E0\u3002"),
          tags$hr(),
          checkboxGroupInput("clear_patterns_manual", "\u5728\u6240\u9009\u533A\u57DF\u5185\u8981\u6E05\u9664\u7684\u6A21\u5F0F\uFF08\u7A7A = \u4EFB\u610F\uFF09", choiceNames = stpd_ui_pattern_choice_names(stpd_ui_pattern_order(TRUE)), choiceValues = stpd_ui_pattern_order(TRUE), inline = TRUE),
	          fluidRow(
	            column(4, actionButton("clear_selected_manual", "\u6E05\u9664\u6240\u9009\u624B\u52A8\u6807\u7B7E", width = "100%")),
	            column(4, actionButton("clear_selected_auto", "\u6E05\u9664\u6240\u9009 AUTO", width = "100%")),
	            column(4, actionButton("clear_all_manual", "\u6E05\u9664\u5168\u90E8\u624B\u52A8\u6807\u7B7E", width = "100%"))
	          ),
	          tags$hr(),
	          tags$details(
	            class = "stpd-fold",
	            tags$summary("\u6700\u7EC8\u5BA1\u8BA1\u7ED3\u679C\uFF08\u4E0B\u6E38\u5206\u6790\u6807\u7B7E\u5C42\uFF09"),
	            tags$div(class = "small-note", "\u8BE5\u5C42\u4E0D\u6539\u5199 AUTO \u6216 MANUAL\uFF1B\u5B83\u5728\u68C0\u6D4B\u540E\u751F\u6210 pattern_audit_final\u3002\u70B9\u51FB\u201C\u5C06 possible \u5347\u7EA7\u4E3A real\u201D\u540E\uFF0Cpossible_* \u4F1A\u5728\u6700\u7EC8\u5BA1\u8BA1\u5C42\u53D8\u6210\u5BF9\u5E94\u771F\u5B9E\u6A21\u5F0F\uFF0C\u4E0B\u6E38 manifold / state trajectory / ML \u9ED8\u8BA4\u4F7F\u7528\u8FD9\u4E00\u5C42\u3002"),
	            uiOutput("final_audit_train_selector"),
	            fluidRow(
	              column(4, actionButton("rebuild_final_audit", "\u751F\u6210/\u91CD\u5EFA\uFF08\u4FDD\u7559 possible\uFF09", width = "100%")),
	              column(4, actionButton("promote_possible_to_final_audit", "\u5C06 possible \u5347\u7EA7\u4E3A real", class = "btn-primary", width = "100%")),
	              column(4, actionButton("clear_final_audit", "\u6E05\u9664\u5BA1\u8BA1\u5C42", width = "100%"))
		            ),
		            verbatimTextOutput("final_audit_status"),
		            h5("\u5F53\u524D\u6700\u7EC8\u5BA1\u8BA1 summary"),
		            DTOutput("final_audit_summary_table"),
		            tags$details(
		              tags$summary("\u5F53\u524D\u5BA1\u8BA1 event \u8BB0\u5F55"),
		              DTOutput("final_audit_event_table")
		            ),
		            tags$details(
		              tags$summary("\u5BA1\u8BA1\u5386\u53F2\uFF08history log\uFF09"),
		              DTOutput("final_audit_history_table")
		            )
		          ),
	          tags$hr(),
	          tags$details(
	            class = "stpd-fold",
	            tags$summary("possible_burst \u6279\u91CF\u5347\u7EA7\u4E3A burst\uFF08\u7528\u6237\u590D\u6838\u5C42\uFF09"),
	            tags$div(class = "small-note", "\u9009\u62E5\u82E5\u5E72\u6761 spike train \u540E\u5148\u9884\u89C8\uFF0C\u518D\u5C06 AUTO possible_burst \u5199\u4E3A MANUAL burst\u3002AUTO \u539F\u59CB\u6807\u7B7E\u548C user override \u5BA1\u8BA1\u5B57\u6BB5\u4F1A\u4FDD\u7559\uFF0C\u53EF\u4EE5\u64A4\u56DE\u3002"),
	            uiOutput("possible_burst_promote_train_selector"),
	            checkboxInput("possible_burst_promote_overwrite_manual", "\u5141\u8BB8\u8986\u76D6\u5DF2\u6709 MANUAL / NOT-burst \u6807\u7B7E", FALSE),
	            fluidRow(
	              column(4, actionButton("preview_possible_burst_promotion", "\u9884\u89C8\u5347\u7EA7", width = "100%")),
	              column(4, actionButton("apply_possible_burst_promotion", "\u6267\u884C\u4E00\u952E\u5347\u7EA7", class = "btn-primary", width = "100%")),
	              column(4, actionButton("revert_possible_burst_promotion", "\u64A4\u56DE\u5347\u7EA7", width = "100%"))
	            ),
	            verbatimTextOutput("possible_burst_promotion_status"),
	            DTOutput("possible_burst_promotion_preview_table"),
	            tags$details(
	              tags$summary("\u5BA1\u8BA1\u8BB0\u5F55"),
	              DTOutput("possible_burst_promotion_audit_table")
	            )
	          )
	      ),

      div(class = "soft-box",
          div(class = "section-kicker", "\u68C0\u6D4B"),
          h4("\u9884\u8BBE\u4E0E\u8FD0\u884C"),
          selectInput("analysis_preset", "\u5206\u6790\u9884\u8BBE",
                      choices = setNames(preset_catalog()$preset_name, preset_catalog()$label),
                      selected = "balanced_single_unit"),
          actionButton("apply_analysis_preset", "\u5C06\u9884\u8BBE\u5E94\u7528\u5230\u5173\u952E\u53C2\u6570", width = "100%"),
          tags$div(class = "small-note", "\u9884\u8BBE\u53EA\u8BBE\u7F6E\u5173\u952E\u7B56\u7565\u9608\u503C\u3002\u5B8C\u6574\u53C2\u6570\u548C params_hash \u4F1A\u5BFC\u51FA\u4EE5\u4FDD\u8BC1\u53EF\u590D\u73B0\u6027\u3002"),
          uiOutput("core_detector_params_panel"),
          tags$details(
            class = "stpd-fold expert-fold",
            tags$summary("Advanced / Expert\uFF1Aburst \u9644\u5C5E interesting-structure"),
            tags$div(class = "small-note", "\u8FD9\u662F burst \u7684\u9644\u5C5E motif\uFF0C\u4E0D\u6539\u5199\u4E3B AUTO \u6807\u7B7E\u3002packet \u5FC5\u987B\u76F4\u63A5\u7D27\u8D34 confirmed burst/long_burst\uFF1B\u9ED8\u8BA4\u4E0D\u5141\u8BB8\u4E2D\u95F4\u6709\u7A7A\u767D ISI\u3002"),
            fluidRow(
              column(6, numericInput("burst_sublabel_regular_min_isi", "\u9644\u5C5E packet \u6700\u5C0F ISI\uFF08\u5F53\u524D\u5355\u4F4D\uFF09", value = 12, min = 0, step = 0.5)),
              column(6, numericInput("burst_sublabel_regular_max_isi", "\u9644\u5C5E packet \u6700\u5927 ISI\uFF08\u5F53\u524D\u5355\u4F4D\uFF09", value = 60, min = 0, step = 1))
            ),
            fluidRow(
              column(6, numericInput("burst_sublabel_regular_min_isi_n", "\u9644\u5C5E packet \u6700\u5C0F ISI \u6570", value = 4, min = 2, step = 1)),
              column(6, numericInput("burst_sublabel_regular_max_isi_n", "\u9644\u5C5E packet \u6700\u5927 ISI \u6570", value = 16, min = 2, step = 1))
            )
          ),
          checkboxInput("detector_selected_only", "\u4EC5\u5BF9\u5F53\u524D\u53EF\u89C1 trains \u8FD0\u884C\u68C0\u6D4B\uFF08\u66F4\u5FEB\uFF09", TRUE),
          fluidRow(
            column(6, actionButton("estimate_apply_manual_params", "\u4ECE MANUAL \u66F4\u65B0\u53C2\u6570", width = "100%")),
            column(6, actionButton("run_detector", "\u8FD0\u884C\u68C0\u6D4B", class = "btn-primary", width = "100%"))
          ),
          verbatimTextOutput("detector_before_after_summary"),
          tags$div(class = "small-note", "\u624B\u52A8\u6807\u8BB0\u4F1A\u5199\u5165\u5F53\u524D UI \u53C2\u6570\uFF0C\u5E76\u5C06\u4E8B\u4EF6\u8BED\u6CD5\u9608\u503C\u8BBE\u4E3A MANUAL \u4F18\u5148\u3002"),
          tags$details(
            class = "stpd-fold expert-fold",
            tags$summary("Advanced / Expert\uFF1A\u6A21\u5F0F\u9009\u62E9\u3001\u81EA\u9002\u5E94\u4F30\u8BA1\u4E0E\u65E7\u7248\u6821\u51C6"),
            checkboxGroupInput("patterns_to_run", "\u9700\u8981\u68C0\u6D4B\u7684\u6A21\u5F0F", choiceNames = stpd_ui_pattern_choice_names(stpd_ui_pattern_order(FALSE)), choiceValues = stpd_ui_pattern_order(FALSE),
                               selected = c("burst", "long_burst", "tonic", "high_frequency_tonic", "high_frequency_spiking", "pause"), inline = FALSE),
            checkboxInput("patterns_to_run_strict_subset", "\u4E13\u5BB6\uFF1A\u4E25\u683C\u53EA\u8FD0\u884C\u4E0A\u9762\u52FE\u9009\u7684\u6A21\u5F0F", FALSE),
            tags$div(class = "small-note", "\u5173\u95ED\u65F6\uFF0C\u81EA\u52A8\u68C0\u6D4B\u4F1A\u81EA\u52A8\u8865\u5168 burst / tonic / pause / HF \u7B49\u9ED8\u8BA4\u6838\u5FC3\u6A21\u5F0F\uFF0C\u907F\u514D\u9690\u85CF\u591A\u9009\u6846\u8BEF\u5173\u6389 tonic \u6216 pause\u3002"),
            stpd_ui_pattern_isi_controls(),
            checkboxInput("fill_others_auto", "\u68C0\u6D4B\u5668\u5C06\u5269\u4F59\u6709\u6548 ISI \u586B\u5145\u4E3A others\uFF08AUTO\uFF09", FALSE),
            fluidRow(
              column(6, actionButton("estimate_params", "\u4F30\u8BA1\u53C2\u6570", width = "100%")),
              column(6, actionButton("clear_auto", "\u6E05\u9664 AUTO", width = "100%"))
            ),
            actionButton("apply_estimated_to_ui", "\u5C06\u4F30\u8BA1\u503C\u5E94\u7528\u5230 UI", width = "100%"),
            tags$hr(),
            tags$div(class = "small-note", style = "font-weight:600;",
                     "\u65E7\u7248 train-specific burst-ISI \u8303\u56F4\u3002\u4E8B\u4EF6\u8BED\u6CD5\u68C0\u6D4B\u4F7F\u7528 dataset/manual ISI seed band\uFF1B\u8FD9\u4E9B\u63A7\u4EF6\u4EC5\u5728\u663E\u5F0F\u542F\u7528\u65E7\u7248/\u5907\u7528\u6821\u51C6\u65F6\u4F7F\u7528\u3002"),
            uiOutput("burst_range_selector"),
            sliderInput("burst_isi_pct_range", "\u6240\u9009 train \u7684\u65E7\u7248\u767E\u5206\u4F4D\u533A\u95F4", min = 0, max = 100, value = c(0, 25), step = 1),
            fluidRow(
              column(6, numericInput("burst_isi_abs_low", "\u7EDD\u5BF9\u6700\u5C0F\u503C\uFF08\u7A7A = \u767E\u5206\u4F4D\uFF09", value = NA, min = 0, step = 1)),
              column(6, numericInput("burst_isi_abs_high", "\u7EDD\u5BF9\u6700\u5927\u503C\uFF08\u7A7A = \u767E\u5206\u4F4D\uFF09", value = NA, min = 0, step = 1))
            ),
            selectInput("burst_range_mode", "\u65E7\u7248\u5DF2\u4FDD\u5B58 burst \u8303\u56F4\u903B\u8F91",
                        choices = c("\u767E\u5206\u4F4D OR \u7EDD\u5BF9\u503C" = "percentile_or_absolute",
                                    "\u767E\u5206\u4F4D AND \u7EDD\u5BF9\u503C" = "percentile_and_absolute",
                                    "\u4EC5\u767E\u5206\u4F4D" = "percentile_only",
                                    "\u4EC5\u7EDD\u5BF9\u503C" = "absolute_only"),
                        selected = "percentile_or_absolute"),
            checkboxInput("burst_adaptive_pct", "\u65E7\u7248\uFF1A\u52A0\u5165 per-train ISI \u767E\u5206\u4F4D\u6761\u4EF6\uFF08\u6838\u5FC3\u68C0\u6D4B\u5FFD\u7565\uFF09", FALSE),
            checkboxInput("burst_use_saved_ranges", "\u65E7\u7248\uFF1A\u5728\u5907\u7528\u68C0\u6D4B\u5668\u4E2D\u4F7F\u7528\u5DF2\u4FDD\u5B58\u7684 train-specific burst-ISI \u8303\u56F4", FALSE),
            checkboxInput("burst_ranges_hard", "\u5C06\u5DF2\u4FDD\u5B58\u8303\u56F4\u4F5C\u4E3A\u786C\u7EA6\u675F", FALSE),
            checkboxInput("burst_enforce_learned_low", "\u5F3A\u5236\u4F7F\u7528\u5B66\u4E60\u5F97\u5230\u7684\u7EDD\u5BF9\u4E0B\u754C", FALSE),
            fluidRow(
              column(6, numericInput("burst_range_expand_pct", "\u5B66\u4E60\u4E0A\u754C\u7684\u767E\u5206\u4F4D\u6269\u5C55", value = 5, min = 0, max = 50, step = 1)),
              column(6, numericInput("burst_range_expand_factor", "\u5B66\u4E60\u4E0A\u754C\u7684 IQR/MAD \u6269\u5C55", value = 1.25, min = 0, step = 0.05))
            ),
            fluidRow(
              column(6, actionButton("apply_burst_isi_range", "\u4FDD\u5B58\u8303\u56F4", width = "100%")),
              column(6, actionButton("clear_burst_isi_range", "\u6E05\u9664\u8303\u56F4", width = "100%"))
            ),
            actionButton("learn_burst_isi_range_manual", "\u4ECE\u624B\u52A8\u6807\u8BB0\u7684 burst \u6821\u51C6\u8303\u56F4", width = "100%")
          )
      ),

      div(class = "soft-box",
          div(class = "section-kicker", "\u5BFC\u51FA"),
          h4("\u7ED3\u679C\u4E0E\u590D\u73B0\u6587\u4EF6"),
          downloadButton("download_labeled_csv", "\u4E0B\u8F7D\u6807\u8BB0 CSV\uFF08\u5BBD\u8868\uFF09", width = "100%"),
          downloadButton("download_results_zip", "\u4E0B\u8F7D\u7ED3\u679C ZIP", width = "100%"),
          tags$hr(),
          downloadButton("download_ml_features_csv", "\u4E0B\u8F7D\u673A\u5668\u5B66\u4E60\u7279\u5F81 CSV", width = "100%"),
          downloadButton("download_nn_model", "\u4E0B\u8F7D\u5DF2\u8BAD\u7EC3\u795E\u7ECF\u7F51\u7EDC\u6A21\u578B", width = "100%")
      )
    ),
    
    mainPanel(
      class = "main-fixed",
      tabsetPanel(
        id = "main_tabs",
        type = "tabs",
        
        tabPanel("\u5BF9\u9F50\u65F6\u95F4\u6233\u56FE",
                 div(class = "plot-scroll", style = "max-height: 85vh;",
	                     div(class = "result-hero",
	                         stpd_ui_auto_pattern_legend(),
	                         htmlOutput("raster_lod_warning"),
	                         uiOutput("raster_plot_shell"),
	                         div(class = "plot-xrange-control",
                             sliderInput("xrange_plot", "\u663E\u793A\u65F6\u95F4\u7A97", min = 0, max = 1000, value = c(0, 1000), step = 1, ticks = FALSE, width = "100%"),
                             numericInput("xrange_plot_window_length", "\u7A97\u53E3\u957F\u5EA6\uFF08\u5F53\u524D\u5355\u4F4D\uFF09", value = 1000, min = 0.001, step = 1, width = "220px"),
                             uiOutput("xrange_plot_ticks")
                         )))),
        
        tabPanel("\u539F\u59CB\u65F6\u95F4\u6233\u56FE",
                 div(class = "plot-scroll", style = "max-height: 85vh;",
                     div(class = "result-hero",
                         stpd_ui_auto_pattern_legend(),
                         plotlyOutput("raster_raw_plot", height = "76vh"),
                         div(class = "plot-xrange-control", uiOutput("raw_time_window_controls"))
                     ))),

        tabPanel("\u76EE\u6807\u6838\u56E2\u6DF1\u5EA6\u56FE",
                 div(class = "plot-scroll dbs-track-shell",
                     fluidRow(
                       column(
                         3,
                         div(class = "dbs-track-controls",
                             h4("\u76EE\u6807\u6838\u56E2 spike train"),
                             radioButtons("dbs_track_view_mode", "\u89C6\u56FE\u6A21\u5F0F",
                                          choices = c("2.5D \u5C0F\u5706\u70B9\u8F6E\u5ED3\u6838\u56E2\u6A21\u578B" = "dot",
                                                      "2D \u6DF1\u5EA6\u5BA1\u8BA1\u56FE" = "2d"),
                                          selected = "dot", inline = TRUE),
                             radioButtons("dbs_track_dataset_scope", "DBS \u6570\u636E\u6765\u6E90",
                                          choices = c("\u5F53\u524D\u6570\u636E\u96C6" = "current",
                                                      "\u6240\u6709\u5DF2\u52A0\u8F7D\u6570\u636E\u96C6" = "all"),
                                          selected = "current", inline = TRUE),
                             uiOutput("dbs_track_structure_selector"),
                             checkboxGroupInput("dbs_track_sides", "\u8111\u5185\u8BB0\u5F55\u4FA7",
                                                choices = c("LT" = "L", "RT" = "R"),
                                                selected = c("L", "R"), inline = TRUE),
                             checkboxInput("dbs_track_visible_only", "\u4EC5\u4F7F\u7528\u5F53\u524D\u7B5B\u9009/\u5206\u9875\u53EF\u89C1 trains", FALSE),
                             radioButtons("dbs_track_depth_direction", "D \u6DF1\u5EA6\u65B9\u5411",
                                          choices = c("D \u503C\u8D8A\u5927\u8D8A\u6DF1\uFF08\u5411\u4E0B\uFF09" = "larger_deeper",
                                                      "D \u503C\u8D8A\u5927\u8D8A\u6D45\uFF08\u5411\u4E0A\uFF09" = "larger_shallower"),
                                          selected = "larger_deeper"),
                             radioButtons("dbs_track_time_origin", "\u65F6\u95F4\u539F\u70B9",
                                          choices = c("\u6BCF\u6761 train \u4ECE\u9996\u4E2A spike \u5BF9\u9F50" = "aligned",
                                                      "\u539F\u59CB timestamp" = "raw"),
                                          selected = "aligned"),
                             checkboxInput("dbs_track_sync_raster_window", "\u540C\u6B65\u5BF9\u9F50 plot \u7684\u663E\u793A\u65F6\u95F4\u7A97", TRUE),
                             numericInput("dbs_track_start_sec", "\u8282\u9009\u8D77\u70B9\uFF08s\uFF09", value = 0, min = 0, step = 0.1),
                             numericInput("dbs_track_window_sec", "\u8282\u9009\u7A97\u957F\uFF08s\uFF09", value = 0.5, min = 0.01, step = 0.05),
                             numericInput("dbs_track_max_trains_per_side", "\u6BCF\u4FA7\u6700\u591A\u663E\u793A trains\uFF080 = \u5168\u90E8\uFF09", value = 0, min = 0, max = 200, step = 1),
                             radioButtons("dbs_track_pattern_mode", "\u6A21\u5F0F\u53E0\u52A0",
                                          choices = c("\u6700\u7EC8\u5BA1\u8BA1\u7ED3\u679C" = "audit_final",
                                                      "\u6700\u7EC8\u6807\u7B7E\uFF08manual \u4F18\u5148\uFF09" = "final",
                                                      "AUTO \u6807\u7B7E" = "auto",
                                                      "MANUAL \u6807\u7B7E" = "manual",
                                                      "\u4E0D\u663E\u793A" = "none"),
                                          selected = "audit_final"),
                             checkboxInput("dbs_track_show_labels", "\u663E\u793A\u6BCF\u6761 train \u7684\u6DF1\u5EA6\u4E0E\u65F6\u95F4\u5C3A\u5EA6", TRUE),
                             checkboxInput("dbs_track_show_context", "\u663E\u793A GPe/GPi/STN atlas-like \u89E3\u5256\u8F6E\u5ED3\u80CC\u666F", TRUE),
                             checkboxInput("dbs_track_particle_flow", "\u7C92\u5B50\u968F\u673A\u6D41\u52A8\uFF08\u9650\u5236\u5728\u6838\u56E2\u8F6E\u5ED3\u5185\uFF0C\u4EC5 2.5D \u4EA4\u4E92\u56FE\uFF09", FALSE),
                             tags$hr(),
                             tags$strong("\u8BBA\u6587\u9759\u6001\u7248\u5BFC\u51FA"),
                             fluidRow(
                               column(6, numericInput("dbs_track_static_width", "\u5BBD\uFF08in\uFF09", value = 11, min = 4, max = 24, step = 0.5)),
                               column(6, numericInput("dbs_track_static_height", "\u9AD8\uFF08in\uFF09", value = 6.5, min = 3, max = 18, step = 0.5))
                             ),
                             numericInput("dbs_track_static_dpi", "PNG DPI", value = 600, min = 150, max = 1200, step = 50),
                             downloadButton("download_dbs_track_static_png", "\u4E0B\u8F7D\u8BBA\u6587\u9759\u6001\u56FE PNG", width = "100%"),
                             downloadButton("download_dbs_track_static_pdf", "\u4E0B\u8F7D\u8BBA\u6587\u9759\u6001\u56FE PDF", width = "100%"),
                             tags$div(class = "small-note",
                                      "\u56FE\u4E2D\u4E3A 2D \u753B\u5E03\u4E0A\u7684\u5C0F\u5706\u70B9 / dot-node target-nucleus schematic\uFF1BD \u6DF1\u5EA6\u8303\u56F4\u6620\u5C04\u5230\u5F53\u524D\u9009\u62E9\u7684\u6570\u636E\u5BF9\u5E94\u6838\u56E2\u4E0A/\u4E0B\u7AEF\uFF0Cspike tick \u6765\u81EA\u771F\u5B9E timestamp\u3002\u9009\u62E9\u201C\u6240\u6709\u5DF2\u52A0\u8F7D\u6570\u636E\u96C6\u201D\u65F6\uFF0C\u4EC5\u5728 DBS \u89C6\u56FE\u5185\u5408\u5E76\u5C55\u793A\uFF0C\u4E0D\u6539\u53D8\u5F53\u524D\u4E3B\u6570\u636E\u96C6\u3002")
                         )
                       ),
                       column(
                         9,
                         div(class = "dbs-track-figure",
                             plotlyOutput("dbs_track_plot", height = "86vh", width = "100%")),
                         div(class = "dbs-track-caption",
                             "\u5B9A\u4F4D\u5C42\u9762\u4E3A target-nucleus schematic depth view\uFF0C\u7528\u4E8E\u5C55\u793A\u5355\u75C5\u4EBA\u76EE\u6807\u6838\u56E2\u5185\u4E0D\u540C\u8111\u4FA7\u548C\u6DF1\u5EA6\u7684\u771F\u5B9E spike timestamps\uFF1B\u4E0D\u4EE3\u8868 MRI/CT \u7CBE\u786E\u89E3\u5256\u5750\u6807\u3002"),
                         DT::DTOutput("dbs_track_inventory_table")
                       )
                     ))),

        tabPanel("ISI \u65F6\u95F4\u5256\u9762",
                 div(class = "plot-scroll", style = "max-height: 85vh;",
                     fluidRow(
                       column(3,
                              h4("ISI \u65F6\u95F4\u5256\u9762"),
                              radioButtons("isi_profile_display_mode", "\u5256\u9762\u663E\u793A\u6A21\u5F0F",
                                           choices = c("\u4EC5\u805A\u7126\u5355\u6761 train" = "focused", "\u591A\u6761 train\uFF0C\u72EC\u7ACB\u9762\u677F" = "multi"),
                                           selected = "multi"),
                              uiOutput("isi_profile_train_selector"),
                              numericInput("isi_profile_max_trains", "\u591A\u9762\u677F\u6700\u5927 train \u6570", value = 8, min = 1, max = 10, step = 1),
                              radioButtons("isi_profile_time_range_mode", "\u5256\u9762\u65F6\u95F4\u8303\u56F4",
                                           choices = c("\u6BCF\u6761 train \u5168\u65F6\u957F" = "full", "\u540C\u6B65 raster \u65F6\u95F4\u7A97" = "sync", "\u81EA\u5B9A\u4E49\u5256\u9762\u7A97\u53E3" = "custom"),
                                           selected = "full"),
                              uiOutput("isi_profile_custom_window_ui"),
                              radioButtons("isi_profile_x_axis", "X \u8F74", choices = c("\u65F6\u95F4" = "time", "ISI \u7D22\u5F15" = "index"), selected = "time", inline = TRUE),
                              radioButtons("isi_profile_y_scale", "Y \u8F74", choices = c("log10(ISI)" = "log", "\u7EBF\u6027 ISI" = "linear"), selected = "log", inline = TRUE),
                              selectInput("isi_profile_ref_tol", "\u53C2\u8003\u7EBF\u5BB9\u5DEE", choices = c("\u00B15%" = 0.05, "\u00B110%" = 0.10, "\u00B120%" = 0.20), selected = 0.10),
	                              checkboxInput("isi_profile_show_labels", "\u9634\u5F71\u663E\u793A\u5DF2\u6807\u8BB0\u533A\u95F4", TRUE),
	                              checkboxInput("isi_profile_show_thresholds", "\u663E\u793A train-specific \u9608\u503C\u7EBF", TRUE),
	                              checkboxInput("isi_profile_ref_all_panels", "\u5C06\u9501\u5B9A\u53C2\u8003\u7EBF\u5E94\u7528\u5230\u6240\u6709\u53EF\u89C1\u9762\u677F", FALSE),
	                              actionButton("clear_isi_profile_ref", "\u6E05\u9664\u53C2\u8003\u7EBF", width = "100%"),
	                              tags$div(class = "small-note", "\u591A train \u6A21\u5F0F\u4E3A\u6BCF\u6761 spike train \u4F7F\u7528\u72EC\u7ACB X-Y \u8F74\u3002\u65F6\u95F4 X \u8F74\u4F7F\u7528\u771F\u5B9E spike \u65F6\u95F4\u6233\uFF08\u79D2\uFF09\uFF1B\u663E\u793A\u5355\u4F4D\u63A7\u5236 Y \u8F74 ISI \u6570\u503C\u3002\u70B9\u51FB ISI \u70B9/\u7EBF\u6BB5\u53EF\u9501\u5B9A\u6C34\u5E73\u53C2\u8003\u7EBF\u3002"),
	                              verbatimTextOutput("isi_profile_ref_text"),
	                              tags$hr(),
	                              h4("Train-specific ISI \u9608\u503C\uFF08\u5355\u6761\u8BB0\u5F55\u9608\u503C\uFF09"),
	                              tags$div(class = "small-note", "\u5355\u4F4D\u8DDF\u968F\u663E\u793A\u5355\u4F4D\u30020 = \u4E0D\u542F\u7528\u3002\u53EF\u5728\u56FE\u4E2D\u70B9\u51FB\u4E00\u4E2A ISI \u4F5C\u4E3A\u53C2\u8003\uFF0C\u518D\u4E00\u952E\u8BBE\u7F6E burst line\u3001pause line \u6216 tonic \u4E24\u6761 line\u3002\u8F6F\u951A\u70B9\u662F\u63A8\u8350\u6A21\u5F0F\uFF1B\u786C\u9608\u503C\u9700\u663E\u5F0F\u9009\u62E9\u3002"),
	                              fluidRow(
	                                column(6, actionButton("isi_ref_to_burst", "\u53C2\u8003 -> burst \u7EBF", width = "100%")),
	                                column(6, actionButton("isi_ref_to_pause", "\u53C2\u8003 -> pause \u7EBF", width = "100%"))
	                              ),
	                              fluidRow(
	                                column(6, actionButton("isi_ref_to_tonic_min", "\u53C2\u8003 -> tonic \u4E0B\u754C", width = "100%")),
	                                column(6, actionButton("isi_ref_to_tonic_max", "\u53C2\u8003 -> tonic \u4E0A\u754C", width = "100%"))
	                              ),
	                              numericInput("train_thr_burst_max", "burst \u6700\u5927 ISI", value = 0, min = 0, step = 0.1),
	                              numericInput("train_thr_pause_min", "pause \u6700\u5C0F ISI", value = 0, min = 0, step = 0.1),
	                              fluidRow(
	                                column(6, numericInput("train_thr_tonic_min", "tonic \u6700\u5C0F ISI", value = 0, min = 0, step = 0.1)),
	                                column(6, numericInput("train_thr_tonic_max", "tonic \u6700\u5927 ISI", value = 0, min = 0, step = 0.1))
	                              ),
	                              radioButtons("isi_threshold_mode", "\u9608\u503C\u7EBF\u89E3\u91CA",
	                                           choices = c("\u8F6F\u951A\u70B9\uFF08\u63A8\u8350\uFF0C\u7ED3\u6784\u8BED\u6CD5\u4ECD\u9700\u901A\u8FC7\uFF09" = "soft_anchor",
	                                                       "\u786C\u9608\u503C\uFF08\u663E\u5F0F\u7EA6\u675F\uFF09" = "hard_threshold"),
	                                           selected = "soft_anchor"),
	                              radioButtons("isi_threshold_apply_scope", "\u5E94\u7528\u8303\u56F4",
	                                           choices = c("\u5F53\u524D\u5256\u9762 train(s)" = "profile",
	                                                       "\u81EA\u9009 train(s)" = "custom",
	                                                       "\u5168\u90E8 train" = "all"),
	                                           selected = "profile"),
	                              uiOutput("isi_threshold_apply_trains_ui"),
	                              checkboxInput("run_detector_after_train_isi_thresholds", "\u4FDD\u5B58\u540E\u7ACB\u5373\u91CD\u8DD1\u68C0\u6D4B\u5668", TRUE),
	                              actionButton("save_train_isi_thresholds", "\u4FDD\u5B58\u9608\u503C\u7EBF", width = "100%"),
	                              actionButton("apply_train_isi_thresholds_and_run", "\u4FDD\u5B58\u5E76\u6309\u8FD9\u4E9B\u9608\u503C\u68C0\u6D4B", width = "100%"),
	                              actionButton("clear_train_isi_thresholds", "\u6E05\u9664\u5F53\u524D\u663E\u793A train \u7684\u9608\u503C", width = "100%"),
	                              actionButton("clear_all_train_isi_thresholds", "\u6E05\u9664\u6240\u6709 train-specific \u9608\u503C\uFF08\u5355\u6761\u8BB0\u5F55\u9608\u503C\uFF09", width = "100%"),
	                              DT::DTOutput("train_isi_thresholds_table")
                       ),
                       column(9, uiOutput("isi_profile_plot_ui"))
                     ))),

        tabPanel("ISI \u72B6\u6001\u7A7A\u95F4",
                 div(
                   class = "plot-scroll state-space-shell",
                   div(
                     class = "state-space-header",
                     div(class = "state-space-title", "ISI \u72B6\u6001\u7A7A\u95F4\u5206\u6790"),
                     div(class = "state-space-note", "label-free ISI features | labels as overlays")
                   ),
                   stpd_isi_state_global_controls(),
                   stpd_isi_state_context_controls(),
                   div(
                     class = "state-space-main",
                     tabsetPanel(
                       id = "isi_state_space_view",
                       type = "pills",
                       tabPanel(
                         "PCA \u8F68\u8FF9",
                         value = "pca",
                         div(
                           class = "state-space-view",
                           div(class = "state-space-figure", plotlyOutput("isi_state_space_pca_plot", height = "64vh")),
                           tags$details(
                             class = "state-space-data-drawer",
                             tags$summary("PCA variance / loading"),
                             div(
                               class = "state-space-table-grid",
                               div(class = "state-space-table-block", div(class = "state-space-table-title", "Explained variance"), DT::DTOutput("isi_state_space_variance_table")),
                               div(class = "state-space-table-block", div(class = "state-space-table-title", "Feature loading"), DT::DTOutput("isi_state_space_loading_table"))
                             )
                           )
                         )
                       ),
                       tabPanel(
                         "Isomap \u8F68\u8FF9",
                         value = "isomap",
                         div(
                           class = "state-space-view",
                           div(class = "state-space-figure", plotlyOutput("isi_state_space_isomap_plot", height = "64vh")),
                           tags$details(
                             class = "state-space-data-drawer",
                             tags$summary("Isomap diagnostics"),
                             DT::DTOutput("isi_state_space_isomap_diagnostics_table")
                           )
                         )
                       ),
                       tabPanel(
                         "\u6838\u5FC3\u8BC1\u636E",
                         value = "core",
                         div(
                           class = "state-space-subtabs",
                           tabsetPanel(
                             id = "isi_state_core_view",
                             type = "pills",
                             tabPanel(
                               "Transition matrix",
                               value = "transition",
                               div(class = "state-space-view",
                                   div(class = "state-space-figure", plotlyOutput("isi_state_transition_heatmap", height = "62vh")),
                                   tags$details(class = "state-space-data-drawer", tags$summary("Transition table"), DT::DTOutput("isi_state_transition_table")))
                             ),
                             tabPanel(
                               "Dwell-time",
                               value = "dwell",
                               div(class = "state-space-view",
                                   div(class = "state-space-figure", plotlyOutput("isi_state_dwell_plot", height = "62vh")),
                                   tags$details(class = "state-space-data-drawer", tags$summary("Dwell segments"), DT::DTOutput("isi_state_dwell_table")))
                             ),
                             tabPanel(
                               "Transition entropy",
                               value = "entropy",
                               div(class = "state-space-view", DT::DTOutput("isi_state_transition_entropy_table"))
                             ),
                             tabPanel(
                               "Motif frequency",
                               value = "motif",
                               div(class = "state-space-view", DT::DTOutput("isi_state_motif_table"))
                             ),
                             tabPanel(
                               "Surrogates",
                               value = "surrogate",
                               div(class = "state-space-view", DT::DTOutput("isi_state_surrogate_summary_table"))
                             )
                           )
                         )
                       ),
                       tabPanel(
                         "\u63A2\u7D22\u5C42",
                         value = "explore",
                         div(
                           class = "state-space-subtabs",
                           tabsetPanel(
                             id = "isi_state_explore_view",
                             type = "pills",
                             tabPanel("Diffusion map", value = "diffusion", div(class = "state-space-view", div(class = "state-space-figure", plotlyOutput("isi_state_diffusion_plot", height = "64vh")))),
                             tabPanel("PHATE", value = "phate", div(class = "state-space-view", div(class = "state-space-figure", plotlyOutput("isi_state_phate_plot", height = "64vh")))),
                             tabPanel("RQA / recurrence", value = "rqa", div(class = "state-space-view",
                                                                               div(class = "state-space-figure", plotlyOutput("isi_state_recurrence_plot", height = "64vh")),
                                                                               tags$details(class = "state-space-data-drawer", tags$summary("RQA metrics"), DT::DTOutput("isi_state_rqa_table")))),
                             tabPanel("Isomap sweep", value = "isomap_sweep", div(class = "state-space-view", DT::DTOutput("isi_state_isomap_sweep_table"))),
                             tabPanel("UMAP / t-SNE", value = "umap_tsne", div(class = "state-space-view", DT::DTOutput("isi_state_umap_tsne_table")))
                           )
                         )
                       ),
                       tabPanel(
                         "\u6A21\u578B\u5C42",
                         value = "model",
                         div(
                           class = "state-space-subtabs",
                           tabsetPanel(
                             id = "isi_state_model_view",
                             type = "pills",
                             tabPanel(
                               "Rule / GMM",
                               value = "candidate",
                               div(
                                 class = "state-space-view state-space-table-grid",
                                 div(class = "state-space-table-block", div(class = "state-space-table-title", "Rule-based states"), DT::DTOutput("isi_state_rule_counts_table")),
                                 div(class = "state-space-table-block", div(class = "state-space-table-title", "GMM diagnostics"), DT::DTOutput("isi_state_gmm_diagnostics_table")),
                                 div(class = "state-space-table-block", div(class = "state-space-table-title", "GMM state stats"), DT::DTOutput("isi_state_gmm_state_table"))
                               )
                             ),
                             tabPanel(
                               "HSMM",
                               value = "hsmm",
                               div(
                                 class = "state-space-view state-space-table-grid",
                                 div(class = "state-space-table-block", div(class = "state-space-table-title", "Decoded segments"), DT::DTOutput("isi_state_hsmm_segments_table")),
                                 div(class = "state-space-table-block", div(class = "state-space-table-title", "Label agreement"), DT::DTOutput("isi_state_hsmm_agreement_table"))
                               )
                             ),
                             tabPanel("Validation", value = "validation", div(class = "state-space-view", DT::DTOutput("isi_state_model_validation_table"))),
                             tabPanel(
                               "\u8DE8 train \u7EDF\u8BA1",
                               value = "train_model",
                               div(
                                 class = "state-space-view state-space-table-grid",
                                 div(class = "state-space-table-block", div(class = "state-space-table-title", "Model data"), DT::DTOutput("isi_state_train_transition_model_data_table")),
                                 div(class = "state-space-table-block", div(class = "state-space-table-title", "Model summary"), DT::DTOutput("isi_state_train_transition_model_summary_table"))
                               )
                             )
                           )
                         )
                       ),
                       tabPanel("logISI phase portrait", value = "phase", div(class = "state-space-view", div(class = "state-space-figure", plotlyOutput("isi_state_space_phase_plot", height = "68vh")))),
                       tabPanel("3D \u81EA\u7531\u65CB\u8F6C", value = "pca3d", div(class = "state-space-view", div(class = "state-space-figure", plotlyOutput("isi_state_space_3d_plot", height = "68vh")))),
                       tabPanel("Isomap 3D", value = "isomap3d", div(class = "state-space-view", div(class = "state-space-figure", plotlyOutput("isi_state_space_isomap_3d_plot", height = "68vh")))),
                       tabPanel("\u7279\u5F81\u8868", value = "features", div(class = "state-space-view", DT::DTOutput("isi_state_space_feature_table")))
                     )
                 )
                 )),

        tabPanel("State trajectory",
                 div(
                   class = "plot-scroll state-space-shell",
                   div(
                     class = "state-space-header",
                     div(class = "state-space-title", "State trajectory"),
                     div(class = "state-space-note", "multi-train pattern-state trajectory | pseudo-population when trains are not simultaneous")
                   ),
                   fluidRow(
                     column(
                       3,
	                       div(
	                         class = "state-space-controls",
	                         uiOutput("state_trajectory_dataset_selector"),
	                         uiOutput("state_trajectory_train_selector"),
                         radioButtons(
                           "state_trajectory_label_source", "\u6807\u7B7E\u6765\u6E90",
                           choices = c("\u6700\u7EC8\u5BA1\u8BA1\u7ED3\u679C" = "audit_final", "\u6700\u7EC8\u6807\u7B7E" = "final", "AUTO" = "auto", "MANUAL" = "manual"),
                           selected = "audit_final",
                           inline = TRUE
                         ),
                         numericInput("state_trajectory_bin_ms", "Bin width (ms)", value = 100, min = 5, max = 5000, step = 5),
                         numericInput("state_trajectory_smooth_bins", "Gaussian smoothing sigma (bins)", value = 0, min = 0, max = 20, step = 0.25),
                         radioButtons(
                           "state_trajectory_time_origin", "\u65F6\u95F4\u539F\u70B9",
                           choices = c("\u6BCF\u6761 train \u4ECE\u9996\u4E2A spike \u5BF9\u9F50" = "aligned",
                                       "\u539F\u59CB timestamp\uFF08\u9700\u540C\u6B65\u8BB0\u5F55\uFF09" = "raw"),
                           selected = "aligned"
                         ),
                         numericInput("state_trajectory_start_sec", "Start (s)", value = 0, min = 0, step = 0.1),
                         numericInput("state_trajectory_end_sec", "End (s; 0 = auto)", value = 0, min = 0, step = 0.1),
                         radioButtons(
                           "state_trajectory_coordinate_mode", "3D coordinates",
                           choices = stpd_state_trajectory_embedding_choices(),
                           selected = "pattern_axes"
                         ),
                         conditionalPanel(
                           condition = "input.state_trajectory_coordinate_mode == 'pattern_axes'",
                           selectInput("state_trajectory_x_axis", "X axis", choices = stpd_state_trajectory_axis_choices(), selected = "burst_activity"),
                           selectInput("state_trajectory_y_axis", "Y axis", choices = stpd_state_trajectory_axis_choices(), selected = "pause_activity"),
                           selectInput("state_trajectory_z_axis", "Z axis", choices = stpd_state_trajectory_axis_choices(), selected = "tonic_activity")
                         ),
                         conditionalPanel(
                           condition = "['isomap','umap'].indexOf(input.state_trajectory_coordinate_mode) >= 0",
                           numericInput("state_trajectory_n_neighbors", "Nearest neighbors", value = 15, min = 2, max = 200, step = 1)
                         ),
                         conditionalPanel(
                           condition = "input.state_trajectory_coordinate_mode == 'tsne'",
                           numericInput("state_trajectory_tsne_perplexity", "t-SNE perplexity", value = 30, min = 1, max = 200, step = 1)
                         ),
                         conditionalPanel(
                           condition = "input.state_trajectory_coordinate_mode == 'umap'",
                           numericInput("state_trajectory_umap_min_dist", "UMAP min_dist", value = 0.1, min = 0, max = 1, step = 0.05)
                         ),
                         conditionalPanel(
                           condition = "['fa','isomap','tsne','umap'].indexOf(input.state_trajectory_coordinate_mode) >= 0",
                           numericInput("state_trajectory_embedding_seed", "Embedding random seed", value = 1, min = 1, max = 1000000, step = 1),
                           numericInput("state_trajectory_embedding_max_points", "Max embedded bins", value = 900, min = 20, max = 5000, step = 50)
                         ),
                         uiOutput("state_trajectory_window_summary"),
                         tags$div(
                           class = "small-note",
                           "For simultaneous recordings, use raw timestamp mode. Direct axes show interpretable pattern-family activity. PCA and factor analysis are linear summaries; Isomap, t-SNE, and UMAP are exploratory nonlinear embeddings and should be interpreted as neighborhood/geometry views, not predictive classifiers. Point color is assigned from unsmoothed per-bin pattern occupancy, while coordinates can optionally be smoothed. If trains were not recorded simultaneously, interpret the result as a pattern-state / pseudo-population trajectory, not a strict simultaneous neural manifold."
                         )
                       )
                     ),
                     column(
                       9,
                       div(class = "state-space-view",
                           div(class = "state-space-figure", plotlyOutput("state_trajectory_plot", height = "68vh"))),
                       tags$details(
                         class = "state-space-data-drawer",
                         open = TRUE,
                         tags$summary("Joint-state map / state-pair matrix"),
                         div(
                           class = "state-space-controls",
                           uiOutput("state_pair_controls")
                         ),
                         div(class = "state-space-view",
                             div(class = "state-space-figure", plotlyOutput("state_pair_timeline_plot", height = "230px"))),
                         div(class = "state-space-view",
                             div(class = "state-space-figure", plotlyOutput("state_pair_heatmap_plot", height = "430px"))),
                         div(class = "state-space-view",
                             div(class = "state-space-figure", plotlyOutput("state_pair_transition_heatmap_plot", height = "430px"))),
                         div(
                           class = "state-space-table-grid",
                           div(class = "state-space-table-block", div(class = "state-space-table-title", "Observed vs expected joint states"), DT::DTOutput("state_pair_matrix_table")),
                           div(class = "state-space-table-block", div(class = "state-space-table-title", "Joint-state transitions"), DT::DTOutput("state_pair_transition_table")),
                           div(class = "state-space-table-block", div(class = "state-space-table-title", "Per-bin joint states"), DT::DTOutput("state_pair_bin_table"))
                         )
                       ),
                       tags$details(
                         class = "state-space-data-drawer",
                         tags$summary("State trajectory data"),
                         div(
                           class = "state-space-table-grid",
                           div(class = "state-space-table-block", div(class = "state-space-table-title", "Time-bin features"), DT::DTOutput("state_trajectory_feature_table")),
                           div(class = "state-space-table-block", div(class = "state-space-table-title", "Embedding diagnostics"), DT::DTOutput("state_trajectory_variance_table")),
                           div(class = "state-space-table-block", div(class = "state-space-table-title", "Linear loadings"), DT::DTOutput("state_trajectory_loading_table")),
                           div(class = "state-space-table-block", div(class = "state-space-table-title", "Dominant-state transitions"), DT::DTOutput("state_trajectory_transition_table"))
                         )
                       )
	                     )
	                   )
	                 )),

        tabPanel("\u4E8B\u4EF6\u5BF9\u9F50\u6D3B\u52A8",
                 div(
                   class = "plot-scroll state-space-shell",
                   div(
                     class = "state-space-header",
                     div(class = "state-space-title", "\u4E8B\u4EF6\u5BF9\u9F50\u6D3B\u52A8"),
                     div(class = "state-space-note", "event-aligned raster, PSTH, population rate, neuron heatmap, and spike-count synchrony")
                   ),
                   fluidRow(
                     column(
                       3,
                       div(
                         class = "state-space-controls",
                         uiOutput("event_aligned_train_selector"),
                         uiOutput("event_aligned_event_selector"),
                         numericInput("event_aligned_pre_sec", "Pre-event window (s)", value = 1, min = 0, max = 20, step = 0.05),
                         numericInput("event_aligned_post_sec", "Post-event window (s)", value = 2, min = 0.05, max = 20, step = 0.05),
                         numericInput("event_aligned_bin_ms", "Bin width (ms)", value = 50, min = 1, max = 5000, step = 1),
                         numericInput("event_aligned_smooth_bins", "Gaussian smoothing sigma (bins)", value = 1, min = 0, max = 20, step = 0.25),
                         fluidRow(
                           column(6, numericInput("event_aligned_baseline_start_sec", "Baseline start (s)", value = -1, min = -20, max = 0, step = 0.05)),
                           column(6, numericInput("event_aligned_baseline_end_sec", "Baseline end (s)", value = -0.2, min = -20, max = 0, step = 0.05))
                         ),
                         radioButtons(
                           "event_aligned_label_source",
                           "Spike label source for hover",
                           choices = c("\u6700\u7EC8\u5BA1\u8BA1\u7ED3\u679C" = "audit_final",
                                       "\u6700\u7EC8\u6807\u7B7E" = "final",
                                       "\u624B\u52A8\u4F18\u5148" = "manual_priority",
                                       "\u81EA\u52A8\u68C0\u6D4B" = "auto",
                                       "\u624B\u52A8\u6807\u8BB0" = "manual"),
                           selected = "audit_final"
                         ),
                         tags$hr(),
                         numericInput("event_aligned_correlogram_lag_ms", "Cross-correlogram max lag (ms)", value = 250, min = 1, max = 5000, step = 5),
                         numericInput("event_aligned_correlogram_bin_ms", "Cross-correlogram bin (ms)", value = 50, min = 1, max = 1000, step = 1),
                         numericInput("event_aligned_max_pairs", "Max correlogram pairs", value = 30, min = 1, max = 500, step = 1),
                         numericInput("event_aligned_max_raster_spikes", "Max raster spikes displayed", value = 5000, min = 100, max = 100000, step = 500),
                         tags$div(
                           class = "small-note",
                           "\u8FD9\u4E00\u5C42\u4F18\u5148\u56DE\u7B54\u4E8B\u4EF6\u524D\u540E\u653E\u7535\u7387\u3001\u5355\u5143\u54CD\u5E94\u548C spike-count \u540C\u6B65\u6027\uFF1B\u4E0D\u6539\u53D8\u6838\u5FC3 burst/pause/tonic \u68C0\u6D4B\u7ED3\u679C\u3002"
                         )
                       )
                     ),
                     column(
                       9,
                       div(class = "state-space-view",
                           div(class = "state-space-figure", plotlyOutput("event_aligned_raster_plot", height = "38vh"))),
                       div(class = "state-space-view",
                           div(class = "state-space-figure", plotlyOutput("event_aligned_population_plot", height = "32vh"))),
                       tags$details(
                         class = "state-space-data-drawer",
                         open = TRUE,
                         tags$summary("Neuron-level firing-rate evidence"),
                         div(class = "state-space-view",
                             div(class = "state-space-figure", plotlyOutput("event_aligned_psth_plot", height = "36vh"))),
                         div(class = "state-space-view",
                             div(class = "state-space-figure", plotlyOutput("event_aligned_heatmap_plot", height = "42vh")))
                       ),
                       tags$details(
                         class = "state-space-data-drawer",
                         open = TRUE,
                         tags$summary("Synchrony / correlation"),
                         div(class = "state-space-view",
                             div(class = "state-space-figure", plotlyOutput("event_aligned_correlation_plot", height = "38vh"))),
                         div(class = "state-space-view",
                             div(class = "state-space-figure", plotlyOutput("event_aligned_correlogram_plot", height = "38vh")))
                       ),
                       tags$details(
                         class = "state-space-data-drawer",
                         tags$summary("Event-aligned data tables"),
                         div(
                           class = "state-space-table-grid",
                           div(class = "state-space-table-block", div(class = "state-space-table-title", "Summary"), DT::DTOutput("event_aligned_summary_table")),
                           div(class = "state-space-table-block", div(class = "state-space-table-title", "Population PSTH"), DT::DTOutput("event_aligned_population_table")),
                           div(class = "state-space-table-block", div(class = "state-space-table-title", "Neuron PSTH"), DT::DTOutput("event_aligned_psth_table")),
                           div(class = "state-space-table-block", div(class = "state-space-table-title", "Spike-count correlation"), DT::DTOutput("event_aligned_correlation_table"))
                         )
                       )
                     )
                   )
                 )),

        tabPanel("\u795E\u7ECF\u6D41\u5F62",
                 div(
                   class = "plot-scroll state-space-shell",
                   div(
                     class = "state-space-header",
                     div(class = "state-space-title", "\u795E\u7ECF\u6D41\u5F62"),
                     div(class = "state-space-note", "population spike-count / firing-rate manifold | event labels are post hoc annotations")
                   ),
                   fluidRow(
                     column(
                       3,
                       div(
                         class = "state-space-controls",
                         uiOutput("neural_manifold_train_selector"),
                         radioButtons(
                           "neural_manifold_time_origin", "\u65F6\u95F4\u539F\u70B9",
                           choices = c("\u539F\u59CB timestamp\uFF08\u540C\u6B65\u8BB0\u5F55\uFF09" = "raw",
                                       "\u6BCF\u6761 train \u4ECE\u9996\u4E2A spike \u5BF9\u9F50\uFF08pseudo-population\uFF09" = "aligned"),
                           selected = "raw"
                         ),
                         numericInput("neural_manifold_bin_ms", "Bin width (ms)", value = 50, min = 5, max = 5000, step = 5),
                         numericInput("neural_manifold_smooth_bins", "Gaussian smoothing sigma (bins)", value = 1, min = 0, max = 20, step = 0.25),
                         selectInput("neural_manifold_transform", "Activity transform", choices = stpd_neural_manifold_transform_choices(), selected = "sqrt_count"),
                         radioButtons("neural_manifold_scaling", "Neuron scaling",
                                      choices = c("Z-score" = "zscore", "Robust median/MAD" = "robust", "None" = "none"),
                                      selected = "zscore"),
                         numericInput("neural_manifold_start_sec", "Start (s; blank/0 = auto)", value = 0, min = 0, step = 0.1),
                         numericInput("neural_manifold_end_sec", "End (s; 0 = auto)", value = 0, min = 0, step = 0.1),
                         radioButtons("neural_manifold_method", "3D manifold method",
                                      choices = stpd_neural_manifold_method_choices(),
                                      selected = "pca"),
                         conditionalPanel(
                           condition = "['isomap','phate','umap'].indexOf(input.neural_manifold_method) >= 0",
                           numericInput("neural_manifold_n_neighbors", "Nearest neighbors", value = 15, min = 2, max = 200, step = 1)
                         ),
                         conditionalPanel(
                           condition = "input.neural_manifold_method == 'phate'",
                           numericInput("neural_manifold_diffusion_time", "PHATE diffusion time", value = 3, min = 1, max = 50, step = 1)
                         ),
                         conditionalPanel(
                           condition = "input.neural_manifold_method == 'tsne'",
                           numericInput("neural_manifold_tsne_perplexity", "t-SNE perplexity", value = 30, min = 1, max = 200, step = 1)
                         ),
                         conditionalPanel(
                           condition = "input.neural_manifold_method == 'umap'",
                           numericInput("neural_manifold_umap_min_dist", "UMAP min_dist", value = 0.1, min = 0, max = 1, step = 0.05)
                         ),
                         conditionalPanel(
                           condition = "['umap','tsne'].indexOf(input.neural_manifold_method) >= 0",
                           numericInput("neural_manifold_seed", "Embedding random seed", value = 1, min = 1, max = 1000000, step = 1)
                         ),
                         numericInput("neural_manifold_max_points", "Max embedded bins", value = 1200, min = 20, max = 5000, step = 50),
                         tags$hr(),
                         radioButtons("neural_manifold_event_label_source", "Event label source",
                                      choices = c("\u6700\u7EC8\u5BA1\u8BA1\u7ED3\u679C" = "audit_final",
                                                  "\u6700\u7EC8\u6807\u7B7E" = "final",
                                                  "\u624B\u52A8\u4F18\u5148" = "manual_priority",
                                                  "\u81EA\u52A8\u68C0\u6D4B" = "auto",
                                                  "\u624B\u52A8\u6807\u8BB0" = "manual"),
                                      selected = "audit_final"),
                         numericInput("neural_manifold_event_permutations", "Event permutation / shift controls", value = 199, min = 0, max = 4999, step = 50),
                         numericInput("neural_manifold_event_window_bins", "Event-trigger window (bins)", value = 5, min = 1, max = 100, step = 1),
                         tags$hr(),
                         fileInput("neural_manifold_behavior_file", "Behavior / movement CSV", accept = ".csv"),
                         uiOutput("neural_manifold_behavior_columns"),
                         tags$hr(),
                         fileInput("neural_manifold_trial_file", "Trial / movement-event CSV for sliceTCA", accept = ".csv"),
                         checkboxInput("neural_manifold_use_dataset_events", "Use task events embedded in current dataset", TRUE),
                         uiOutput("neural_manifold_dataset_event_selector"),
                         numericInput("neural_manifold_task_pre_sec", "Task-event pre window for manifold annotation (s)", value = 1, min = 0, max = 20, step = 0.05),
                         numericInput("neural_manifold_task_post_sec", "Task-event post window for manifold annotation (s)", value = 2, min = 0.05, max = 20, step = 0.05),
                         uiOutput("neural_manifold_trial_columns"),
                         numericInput("neural_manifold_slicetca_pre_sec", "sliceTCA pre-event window (s)", value = 0.5, min = 0, max = 20, step = 0.05),
                         numericInput("neural_manifold_slicetca_post_sec", "sliceTCA post-event window (s)", value = 1.0, min = 0.05, max = 20, step = 0.05),
                         textInput("neural_manifold_slicetca_ranks", "sliceTCA ranks trial,neuron,time", value = "2,0,2"),
                         numericInput("neural_manifold_slicetca_max_iter", "sliceTCA max_iter", value = 1000, min = 10, max = 100000, step = 100),
                         numericInput("neural_manifold_slicetca_lr", "sliceTCA learning_rate", value = 0.005, min = 0.00001, max = 1, step = 0.001),
                         checkboxInput("neural_manifold_slicetca_run", "Run Python sliceTCA backend", FALSE),
                         checkboxInput("neural_manifold_slicetca_recon_plot", "Plot sliceTCA reconstruction when available", TRUE),
                         tags$div(
                           class = "small-note",
                           "Use this panel for simultaneous recordings whenever possible. Coordinates are computed from binned population activity, not burst/pause/tonic labels. Behavior variables are used for color/decoding or supervised behavior-axis analysis."
                         )
                       )
                     ),
                     column(
                       9,
                       div(class = "state-space-view",
                           div(class = "state-space-figure", plotlyOutput("neural_manifold_plot", height = "68vh"))),
                       tags$details(
                         class = "state-space-data-drawer",
                         open = TRUE,
                         tags$summary("Validation and method notes"),
                         div(
                           class = "state-space-table-grid",
                           div(class = "state-space-table-block", div(class = "state-space-table-title", "Validation metrics"), DT::DTOutput("neural_manifold_validation_table")),
                           div(class = "state-space-table-block", div(class = "state-space-table-title", "Embedding diagnostics"), DT::DTOutput("neural_manifold_diagnostics_table")),
                           div(class = "state-space-table-block", div(class = "state-space-table-title", "Method recommendations"), DT::DTOutput("neural_manifold_method_notes_table"))
                         )
                       ),
                       tags$details(
                         class = "state-space-data-drawer",
                         open = TRUE,
                         tags$summary("Tensor / sliceTCA"),
                         div(class = "state-space-view",
                             div(class = "state-space-figure", plotlyOutput("neural_manifold_slicetca_plot", height = "58vh"))),
                         div(
                           class = "state-space-table-grid",
                           div(class = "state-space-table-block", div(class = "state-space-table-title", "sliceTCA tensor summary"), DT::DTOutput("neural_manifold_slicetca_summary_table")),
                           div(class = "state-space-table-block", div(class = "state-space-table-title", "Python backend diagnostics"), DT::DTOutput("neural_manifold_slicetca_diagnostics_table")),
                           div(class = "state-space-table-block", div(class = "state-space-table-title", "Reconstruction metrics"), DT::DTOutput("neural_manifold_slicetca_reconstruction_table")),
                           div(class = "state-space-table-block", div(class = "state-space-table-title", "Trial-time coordinates / event labels"), DT::DTOutput("neural_manifold_slicetca_embedding_table"))
                         )
                       ),
                       tags$details(
                         class = "state-space-data-drawer",
                         open = TRUE,
                         tags$summary("Event-state geometry"),
                         div(
                           class = "state-space-table-grid",
                           div(class = "state-space-table-block", div(class = "state-space-table-title", "Centroid / dispersion by event state"), DT::DTOutput("neural_manifold_event_geometry_table")),
                           div(class = "state-space-table-block", div(class = "state-space-table-title", "State distances and controls"), DT::DTOutput("neural_manifold_event_distance_table")),
                           div(class = "state-space-table-block", div(class = "state-space-table-title", "Event decoding and dynamics"), DT::DTOutput("neural_manifold_event_dynamics_table")),
                           div(class = "state-space-table-block", div(class = "state-space-table-title", "Event-triggered 3D trajectory"), DT::DTOutput("neural_manifold_event_triggered_table"))
                         )
                       ),
                       tags$details(
                         class = "state-space-data-drawer",
                         tags$summary("Population matrix and loadings"),
                         div(
                           class = "state-space-table-grid",
                           div(class = "state-space-table-block", div(class = "state-space-table-title", "Time-bin activity / coordinates"), DT::DTOutput("neural_manifold_feature_table")),
                           div(class = "state-space-table-block", div(class = "state-space-table-title", "Linear loadings / private variance"), DT::DTOutput("neural_manifold_loading_table")),
                           div(class = "state-space-table-block", div(class = "state-space-table-title", "Window summary"), DT::DTOutput("neural_manifold_summary_table"))
                         )
                       )
                     )
                   )
                 )),
	        
        tabPanel("\u533A\u95F4\u76F4\u65B9\u56FE",
                 div(class = "plot-scroll", style = "max-height: 85vh;",
                     fluidRow(
                       column(
                         3,
                         radioButtons("hist_source", "\u6807\u7B7E\u6765\u6E90", choices = c("\u6700\u7EC8\u5BA1\u8BA1\u7ED3\u679C" = "audit_final", "\u624B\u52A8\u6807\u8BB0" = "manual", "\u81EA\u52A8\u68C0\u6D4B" = "auto", "\u6700\u7EC8\u6807\u7B7E" = "final"), inline = FALSE, selected = "audit_final"),
                         selectInput("hist_type", "\u76F4\u65B9\u56FE\u7C7B\u578B",
                                     choices = c(
                                       "burst \u5185\u90E8 ISI" = "intra_burst",
                                       "\u957F\u7206\u53D1\u5185\u90E8 ISI" = "intra_long_burst",
                                       "\u7591\u4F3C burst \u5185\u90E8 ISI" = "intra_possible_burst",
                                       "burst \u524D ISI" = "pre_burst",
                                       "burst \u540E ISI" = "after_burst",
                                       "burst \u95F4\u9694" = "inter_burst",
                                       "possible_burst \u95F4\u9694" = "inter_possible_burst",
                                       "tonic \u5185\u90E8 ISI" = "intra_tonic",
                                       "\u9AD8\u9891\u5F3A\u76F4\u53D1\u653E\u5185\u90E8 ISI" = "intra_high_frequency_tonic",
                                       "\u9AD8\u9891\u8FDE\u7EED\u53D1\u653E\u5185\u90E8 ISI" = "intra_high_frequency_spiking",
                                       "tonic \u524D ISI" = "pre_tonic",
                                       "tonic \u540E ISI" = "after_tonic",
                                       "tonic \u95F4\u9694" = "inter_tonic",
                                       "pause ISI" = "pause_isi",
                                       "pause \u6301\u7EED\u65F6\u95F4" = "pause_duration",
                                       "\u5176\u4ED6 ISI" = "others_isi",
                                       "tonic LV" = "tonic_lv",
                                       "tonic \u524D LV" = "tonic_pre_lv",
                                       "tonic \u540E LV" = "tonic_after_lv",
                                       "burst \u5373\u65F6\u5BF9\u6BD4 min/q90" = "burst_contrast_min_q",
                                       "burst \u5373\u65F6\u5BF9\u6BD4 geom/q90" = "burst_contrast_geom_q",
                                       "burst \u5373\u65F6\u5BF9\u6BD4 pct/q90" = "burst_contrast_pct_q",
                                       "burst \u5373\u65F6\u5BF9\u6BD4 min/max" = "burst_contrast_min_max",
                                       "burst \u5373\u65F6\u5BF9\u6BD4 geom/max" = "burst_contrast_geom_max",
                                       "burst \u4E0A\u4E0B\u6587\u5BF9\u6BD4 min/q90" = "burst_context_min_q",
                                       "burst \u4E0A\u4E0B\u6587\u5BF9\u6BD4 geom/q90" = "burst_context_geom_q",
                                       "burst \u4E0A\u4E0B\u6587\u5BF9\u6BD4 pct/q90" = "burst_context_pct_q",
                                       "burst \u4E0A\u4E0B\u6587\u5BF9\u6BD4 min/max" = "burst_context_min_max",
                                       "burst \u4E0A\u4E0B\u6587\u5BF9\u6BD4 geom/max" = "burst_context_geom_max",
                                       "possible \u4E0A\u4E0B\u6587\u5BF9\u6BD4 min/q90" = "possible_context_min_q",
                                       "possible \u4E0A\u4E0B\u6587\u5BF9\u6BD4 geom/q90" = "possible_context_geom_q",
                                       "\u6309\u6807\u7B7E\u5206\u7EC4\u7684 log10(ISI)" = "logisi"
                                     ),
                                     selected = "intra_burst"),
                         numericInput("hist_bin", "Bin \u5BBD\u5EA6\uFF08\u7EBF\u6027\u76F4\u65B9\u56FE\uFF1B\u5F53\u524D\u5355\u4F4D\u6216\u65E0\u91CF\u7EB2\uFF09", value = 1, min = 0.01, step = 0.1),
                         tags$hr(),
                         DTOutput("hist_meta_table")
                       ),
                       column(9, plotlyOutput("hist_plot", height = "78vh"))
                     ))),
        
        

        tabPanel("\u6570\u636E\u96C6 ISI \u76F4\u65B9\u56FE",
                 div(class = "plot-scroll", style = "max-height: 85vh;",
                     fluidRow(
                       column(
                         3,
                         h4("\u6570\u636E\u96C6\u5C42\u7EA7 ISI \u76F4\u65B9\u56FE"),
                         tags$div(class = "small-note", "\u8BE5\u56FE\u6C47\u603B\u5F53\u524D\u6570\u636E\u96C6\u6240\u6709\u6709\u6548 ISI\uFF0C\u5E2E\u52A9\u7528\u6237\u9009\u62E9\u6570\u636E\u96C6\u5C42\u7EA7\u7684 burst seed ISI \u533A\u95F4\u3002per-train \u767E\u5206\u4F4D\u4EC5\u4F5C\u4E3A\u8F93\u51FA/\u5BA1\u8BA1\u6307\u6807\uFF0C\u4E0D\u4F5C\u4E3A\u786C\u6027 seed \u95E8\u63A7\u3002"),
                         selectInput("dataset_isi_hist_mode", "\u76F4\u65B9\u56FE\u6A21\u5F0F",
                                     choices = c("\u539F\u59CB\u5408\u5E76 ISI" = "raw",
                                                 "Train \u5E73\u8861\u6BD4\u4F8B" = "balanced",
                                                 "\u53E0\u52A0\u5F52\u4E00\u5316\u539F\u59CB\u5206\u5E03\u4E0E train-balanced \u5206\u5E03" = "overlay"),
                                     selected = "overlay"),
                         numericInput("dataset_isi_hist_bin", "Bin \u5BBD\u5EA6\uFF08\u5F53\u524D\u663E\u793A\u5355\u4F4D\uFF09", value = 5, min = 0.001, step = 1),
                         numericInput("dataset_isi_hist_xmax", "X \u8F74\u6700\u5927\u503C\uFF080 = \u81EA\u52A8\uFF1B\u5F53\u524D\u663E\u793A\u5355\u4F4D\uFF09", value = 100, min = 0, step = 5),
                         checkboxInput("dataset_isi_hist_log_y", "Y \u8F74\u4F7F\u7528\u5BF9\u6570\u5C3A\u5EA6", FALSE),
                         checkboxInput("dataset_isi_hist_show_event_core", "\u663E\u793A\u6A21\u5F0F\u9608\u503C\u533A\u95F4\uFF08\u70B9\u9009\u6A21\u5F0F\u540E\u81EA\u52A8\u542F\u7528\uFF09", FALSE),
                         checkboxInput("dataset_isi_hist_show_qc", "\u663E\u793A\u4F2A\u8FF9 / \u4E0D\u5E94\u671F\u9608\u503C", FALSE),
                         tags$hr(),
                         h4("\u9608\u503C\u6765\u6E90"),
                         selectInput("event_grammar_threshold_source_mode", "\u9608\u503C\u6765\u6E90\u4F18\u5148\u7EA7",
                                     choices = c("\u81EA\u52A8\u4F18\u5148\u7EA7\uFF1A\u7528\u6237 > \u624B\u52A8\u6807\u8BB0 > histogram > \u9ED8\u8BA4" = "auto",
                                                 "\u5F3A\u5236\u4F18\u5148\u4F7F\u7528\u7528\u6237\u8F93\u5165" = "user",
                                                 "\u5F3A\u5236\u4F18\u5148\u4F7F\u7528\u624B\u52A8\u6807\u8BB0\u63A8\u5BFC" = "manual",
                                                 "\u5F3A\u5236\u4F18\u5148\u4F7F\u7528 histogram \u5EFA\u8BAE" = "histogram",
                                                 "\u4F7F\u7528\u9ED8\u8BA4\u503C" = "default"),
                                     selected = "auto"),
                         checkboxGroupInput("dataset_isi_hist_patterns", "\u663E\u793A\u54EA\u4E9B\u6A21\u5F0F\u533A\u95F4",
                                            choices = c("\u7206\u53D1 burst" = "burst",
                                                        "\u9AD8\u9891\u8FDE\u7EED\u53D1\u653E HF spiking" = "high_frequency_spiking",
                                                        "\u9AD8\u9891\u5F3A\u76F4 HF tonic" = "high_frequency_tonic",
                                                        "\u5F3A\u76F4 tonic" = "tonic",
                                                        "\u6682\u505C pause" = "pause"),
                                            selected = character(0)),
                         checkboxGroupInput("dataset_isi_hist_sources", "\u663E\u793A\u54EA\u4E9B\u9608\u503C\u6765\u6E90",
                                            choices = c("\u5B9E\u9645\u4F7F\u7528 effective" = "effective",
                                                        "\u7528\u6237\u8F93\u5165 user" = "user",
                                                        "\u624B\u52A8\u6807\u8BB0 manual" = "manual",
                                                        "histogram \u81EA\u52A8\u5EFA\u8BAE" = "histogram"),
                                            selected = "histogram"),
                         tags$div(class = "small-note", "\u521D\u59CB\u72B6\u6001\u53EA\u663E\u793A\u6570\u636E\u96C6\u81EA\u8EAB ISI \u5206\u5E03\uFF1B\u70B9\u9009\u67D0\u4E2A\u6A21\u5F0F\u540E\u4F1A\u81EA\u52A8\u663E\u793A histogram \u81EA\u52A8\u5EFA\u8BAE\u3002\u5982\u9700\u67E5\u770B\u5B9E\u9645\u4F7F\u7528 / \u7528\u6237\u8F93\u5165 / \u624B\u52A8\u6807\u8BB0\u6765\u6E90\uFF0C\u8BF7\u518D\u52FE\u9009\u5BF9\u5E94\u6765\u6E90\u3002"),
                         actionButton("event_grammar_apply_hist_suggestions", "\u5C06 histogram \u5EFA\u8BAE\u5199\u5165\u7528\u6237\u9608\u503C", icon = icon("wand-magic-sparkles")),
                         tags$div(class = "small-note", "\u6838\u5FC3\u68C0\u6D4B\u5668\u53EA\u8BFB\u53D6\u89E3\u6790\u540E\u7684 effective thresholds\u3002\u6765\u6E90\u4F18\u5148\u7EA7\u4E3A\uFF1A\u7528\u6237\u8F93\u5165 > \u624B\u52A8\u6807\u8BB0\u7ED3\u6784\u5B66\u4E60 > histogram \u5EFA\u8BAE > \u9ED8\u8BA4\u3002"),
                         tags$hr(),
                         tags$details(open = FALSE,
                           tags$summary("\u7528\u6237\u81EA\u5B9A\u4E49\u9608\u503C\u8986\u76D6\uFF08\u6700\u9AD8\u4F18\u5148\u7EA7\uFF09"),
                           checkboxInput("event_grammar_user_burst_enable", stpd_ui_pattern_label("\u542F\u7528 burst \u7528\u6237\u9608\u503C", "burst"), FALSE),
                           fluidRow(column(6, numericInput("event_grammar_user_burst_seed_lower", "burst seed \u4E0B\u754C", value = 1, min = 0, step = 0.5)),
                                    column(6, numericInput("event_grammar_user_burst_seed_upper", "burst seed \u4E0A\u754C", value = 10, min = 0, step = 0.5))),
                           fluidRow(column(6, numericInput("event_grammar_user_burst_bridge", "burst bridge \u4E0A\u754C", value = 15, min = 0, step = 0.5)),
                                    column(6, numericInput("event_grammar_user_burst_S", "burst \u5BF9\u6BD4\u5EA6 S", value = 2.5, min = 1, step = 0.05))),
                           fluidRow(column(4, numericInput("event_grammar_user_one_sided_S", "\u5355\u4FA7 boundary burst \u5BF9\u6BD4\u5EA6 S", value = 3.0, min = 1, step = 0.05)),
                                    column(4, numericInput("event_grammar_one_sided_seed_purity_min", "\u5355\u4FA7 burst seed \u7EAF\u5EA6\u4E0B\u9650", value = 0.65, min = 0, max = 1, step = 0.05)),
                                    column(4, checkboxInput("event_grammar_allow_one_sided_canonical", "\u5141\u8BB8\u5E72\u51C0\u5355\u4FA7 boundary \u76F4\u63A5\u4F5C\u4E3A canonical burst", FALSE))),
                           fluidRow(column(6, checkboxInput("event_grammar_strict_q95_bridge_gate", "\u4E25\u683C\u8981\u6C42 burst \u5185 q95 \u2264 bridge \u4E0A\u754C\uFF08\u9ED8\u8BA4\u5173\u95ED\uFF09", FALSE)),
                                    column(6, checkboxInput("event_grammar_dynamic_possible_priority", "\u542F\u7528 possible_burst \u52A8\u6001\u4F18\u5148\u7EA7", TRUE))),
                           tags$div(class = "small-note", "\u5EFA\u8BAE\u9ED8\u8BA4\u4FDD\u6301 q95 \u8F6F\u60E9\u7F5A\uFF1Aq90 \u662F\u6838\u5FC3 gate\uFF0Cq95 \u53EA\u964D\u4F4E\u5206\u6570\uFF0C\u907F\u514D\u4E00\u4E2A\u7A0D\u5927\u7684\u5185\u90E8 ISI \u6740\u6389\u7C7B\u4F3C 16-4-3-5-3-2-3-15 \u7684\u7ECF\u5178 burst\u3002"),
                           checkboxInput("event_grammar_user_hfs_enable", stpd_ui_pattern_label("\u542F\u7528 HF spiking \u7528\u6237\u9608\u503C", "high_frequency_spiking"), FALSE),
                           fluidRow(column(4, numericInput("event_grammar_user_hfs_seed_lower", "HF spiking seed \u4E0B\u754C", value = 1, min = 0, step = 0.5)),
                                    column(4, numericInput("event_grammar_user_hfs_seed_upper", "HF spiking seed \u4E0A\u754C", value = 20, min = 0, step = 0.5)),
                                    column(4, numericInput("event_grammar_user_hfs_bridge", "HF spiking bridge \u4E0A\u754C", value = 30, min = 0, step = 0.5))),
                           checkboxInput("event_grammar_user_hft_enable", stpd_ui_pattern_label("\u542F\u7528 HF tonic \u7528\u6237\u9608\u503C", "high_frequency_tonic"), FALSE),
                           fluidRow(column(4, numericInput("event_grammar_user_hft_seed_lower", "HF tonic seed \u4E0B\u754C", value = 10, min = 0, step = 0.5)),
                                    column(4, numericInput("event_grammar_user_hft_seed_upper", "HF tonic seed \u4E0A\u754C", value = 30, min = 0, step = 0.5)),
                                    column(4, numericInput("event_grammar_user_hft_bridge", "HF tonic bridge \u4E0A\u754C", value = 35, min = 0, step = 0.5))),
                           checkboxInput("event_grammar_user_tonic_enable", stpd_ui_pattern_label("\u542F\u7528 tonic \u7528\u6237\u9608\u503C", "tonic"), FALSE),
                           fluidRow(column(4, numericInput("event_grammar_user_tonic_seed_lower", "tonic seed \u4E0B\u754C", value = 20, min = 0, step = 1)),
                                    column(4, numericInput("event_grammar_user_tonic_seed_upper", "tonic seed \u4E0A\u754C", value = 60, min = 0, step = 1)),
                                    column(4, numericInput("event_grammar_user_tonic_bridge", "tonic bridge \u4E0A\u754C", value = 80, min = 0, step = 1))),
                           checkboxInput("event_grammar_user_pause_enable", stpd_ui_pattern_label("\u542F\u7528 pause \u7528\u6237\u9608\u503C", "pause"), FALSE),
                           fluidRow(column(4, numericInput("event_grammar_user_pause_seed_lower", "pause seed \u4E0B\u754C", value = 100, min = 0, step = 5)),
                                    column(4, numericInput("event_grammar_user_pause_seed_upper", "pause seed \u4E0A\u754C", value = 150, min = 0, step = 5)),
                                    column(4, numericInput("event_grammar_user_pause_bridge", "pause bridge \u4E0A\u754C", value = 150, min = 0, step = 5)))
                         )
                       ),
                       column(
                         9,
                         plotlyOutput("dataset_isi_hist_plot", height = "48vh"),
                         tags$hr(),
                         h4("\u9608\u503C\u6765\u6E90 / \u5B9E\u9645\u68C0\u6D4B\u9608\u503C"),
                         tags$div(class = "small-note", "\u6BCF\u4E00\u884C\u663E\u793A\u4E00\u4E2A\u6A21\u5F0F\u53C2\u6570\u7684\u7528\u6237\u8F93\u5165\u3001\u624B\u52A8\u6807\u8BB0\u63A8\u5BFC\u3001histogram \u81EA\u52A8\u5EFA\u8BAE\u3001\u9ED8\u8BA4\u503C\u3001\u5B9E\u9645\u4F7F\u7528\u503C\u548C\u6765\u6E90\u3002"),
                         DTOutput("threshold_table"),
                         tags$hr(),
                         h4("\u624B\u52A8\u6807\u8BB0\u7ED3\u6784\u5B66\u4E60"),
                         tags$div(class = "small-note", "\u624B\u52A8 burst \u4F1A\u5B66\u4E60 intra-burst ISI\uFF0C\u4E5F\u4F1A\u5B66\u4E60 before/after-burst ISI \u4E0E intra-q90 \u7684\u6BD4\u503C\u3002"),
                         DTOutput("event_grammar_manual_structure_table"),
                         tags$hr(),
                         h4("Per-train seed-band \u8868\u578B\uFF08\u5355\u6761\u8BB0\u5F55 seed-band \u8868\u578B\uFF09"),
                         tags$div(class = "small-note", "\u767E\u5206\u4F4D\u5217\u663E\u793A\u6570\u636E\u96C6\u5C42\u7EA7 seed band \u5728\u6BCF\u6761 spike train \u5185\u7684\u4F4D\u7F6E\uFF1B\u8FD9\u4E9B\u662F\u7528\u4E8E\u8868\u578B\u89E3\u91CA\u7684\u8BCA\u65AD\u8F93\u51FA\u3002"),
                         DTOutput("dataset_isi_seed_band_table")
                       )
                     ))),
        
        tabPanel("\u7ED3\u6784\u5019\u9009",
                 div(class = "plot-scroll", style = "max-height: 85vh;",
                     fluidRow(
                       column(
                         3,
                         h4("Pre-Core-Post \u7ED3\u6784"),
                         selectInput("structure_diag_type", "\u7ED3\u6784\u8BCA\u65AD",
                                     choices = c(
                                       "Core q90 \u76F4\u65B9\u56FE" = "core_q_hist",
                                       "Core q90 \u767E\u5206\u4F4D\u76F4\u65B9\u56FE" = "core_q_pct_hist",
                                       "\u52A0\u6743 core ISI \u76F4\u65B9\u56FE" = "core_isi_weighted_hist",
                                       "Core \u8303\u56F4\u56FE" = "core_range",
                                       "Core q90 vs \u6301\u7EED\u65F6\u95F4" = "core_q_duration",
                                       "Core q90 vs LV" = "core_q_lv",
                                       "Core q90 \u9608\u503C\u5F71\u54CD" = "core_q_impact",
                                       "\u8FB9\u7F18\u5BF9\u6BD4 min \u76F4\u65B9\u56FE" = "edge_min_hist",
                                       "\u8FB9\u7F18\u5BF9\u6BD4 geom \u76F4\u65B9\u56FE" = "edge_geom_hist"
                                     ), selected = "core_q_hist"),
                         numericInput("structure_bin", "Bin \u5BBD\u5EA6\uFF08\u5F53\u524D\u5355\u4F4D\u6216\u65E0\u91CF\u7EB2\uFF09", value = 1, min = 0.001, step = 0.1),
                         checkboxInput("structure_include_reject", "\u5305\u542B\u88AB\u62D2\u7EDD\u7684\u7ED3\u6784\u5019\u9009", FALSE),
                         numericInput("structure_table_n", "\u7ED3\u6784\u8868\u663E\u793A\u884C\u6570", value = 100, min = 20, step = 20),
                         tags$hr(),
                         DTOutput("structure_meta_table")
                       ),
                       column(
                         9,
                         plotlyOutput("structure_plot", height = "45vh"),
                         tags$hr(),
                         DTOutput("structure_table")
                       )
                     ))),
        
        tabPanel("Seed / Bridge \u8BCA\u65AD",
                 div(class = "plot-scroll", style = "max-height: 85vh;",
                     fluidRow(
                       column(
                         3,
                         selectInput("diag_type", "\u8BCA\u65AD\u76F4\u65B9\u56FE",
                                     choices = c(
                                       "Seed q90 ISI" = "seed_q",
                                       "Seed \u6301\u7EED\u65F6\u95F4" = "seed_duration",
                                       "Seed \u8FB9\u7F18\u5BF9\u6BD4 min" = "seed_edge_min",
                                       "Seed \u8FB9\u7F18\u5BF9\u6BD4 geom" = "seed_edge_geom",
                                       "Seed MM" = "seed_mm",
                                       "Seed LV" = "seed_lv",
                                       "Bridge \u539F\u59CB\u6700\u5927 ISI" = "bridge_raw",
                                       "Bridge / \u81A8\u80C0\u540E max(seed q90)" = "bridge_ratio_maxseed",
                                       "Bridge / \u81A8\u80C0\u540E geom(seed q90)" = "bridge_ratio_geomseed",
                                       "Bridge \u5408\u5E76\u8FB9\u7F18\u5BF9\u6BD4 min" = "bridge_merged_edge_min",
                                       "Bridge \u5408\u5E76\u8FB9\u7F18\u5BF9\u6BD4 geom" = "bridge_merged_edge_geom",
                                       "\u6700\u7EC8\u5019\u9009\u8FB9\u7F18\u5BF9\u6BD4 min" = "cand_edge_min",
                                       "\u6700\u7EC8\u5019\u9009\u8BC4\u5206" = "cand_score",
                                       "\u6700\u7EC8\u5019\u9009\u6301\u7EED\u65F6\u95F4" = "cand_duration"
                                     ), selected = "seed_q"),
                         numericInput("diag_bin", "Bin \u5BBD\u5EA6\uFF08\u5F53\u524D\u5355\u4F4D\u6216\u65E0\u91CF\u7EB2\uFF09", value = 0.25, min = 0.001, step = 0.05),
                         checkboxInput("diag_show_reject", "\u5305\u542B\u88AB\u62D2\u7EDD bridge/\u5019\u9009\u884C", TRUE),
                         tags$hr(),
                         selectInput("diag_scatter_type", "\u8BCA\u65AD\u6563\u70B9\u56FE",
                                     choices = c("Seed q90 \u767E\u5206\u4F4D vs \u8BC4\u5206" = "seed_pct_score",
                                                 "Bridge \u767E\u5206\u4F4D vs \u6BD4\u7387" = "bridge_pct_ratio",
                                                 "\u6700\u7EC8\u8BC4\u5206 vs \u8FB9\u7F18\u5BF9\u6BD4" = "cand_score_edge"),
                                     selected = "seed_pct_score"),
                         DTOutput("diag_meta_table")
                       ),
                       column(
                         9,
                         plotlyOutput("diag_plot", height = "32vh"),
                         tags$hr(),
                         plotlyOutput("diag_scatter_plot", height = "32vh"),
                         tags$hr(),
                         DTOutput("diag_table")
                       )
                     ))),
        
        tabPanel("\u9608\u503C\u9884\u89C8",
                 div(class = "plot-scroll", style = "max-height: 85vh;",
                     fluidRow(
                       column(
                         3,
                         h4("Near-miss \u63A2\u7D22\u5668"),
                         selectInput("near_miss_pattern", "\u6A21\u5F0F", choices = c("\u5168\u90E8" = "all", "burst" = "burst", "tonic" = "tonic", "pause" = "pause"), selected = "all"),
                         selectInput("near_miss_category", "\u5019\u9009\u7C7B\u522B", choices = c("\u5168\u90E8" = "all", "\u7ED3\u6784" = "structure", "bridge" = "bridge", "\u6700\u7EC8" = "final", "tonic \u7A97\u53E3" = "tonic_window", "\u5355\u4E2A ISI" = "single_isi"), selected = "all"),
                         selectInput("near_miss_parameter", "\u53C2\u6570", choices = c("\u5168\u90E8" = "all"), selected = "all"),
                         numericInput("near_miss_filter_relax", "\u663E\u793A\u6240\u9700\u76F8\u5BF9\u8C03\u6574 \u2264 \u8BE5\u503C\u7684\u5019\u9009", value = 0.25, min = 0.001, max = 1, step = 0.01),
                         selectInput("near_miss_sort", "\u6392\u5E8F\u4F9D\u636E", choices = c("\u5931\u8D25\u9608\u503C\u6700\u5C11\uFF0C\u7136\u540E\u8C03\u6574\u6700\u5C0F" = "default",
                                                                              "\u8C03\u6574\u6700\u5C0F" = "relax",
                                                                              "\u6700\u9AD8\u8BC4\u5206" = "score",
                                                                              "time" = "time"), selected = "default"),
                         uiOutput("near_miss_candidate_selector"),
                         fluidRow(
                           column(6, actionButton("near_miss_prev", "\u4E0A\u4E00\u4E2A", width = "100%")),
                           column(6, actionButton("near_miss_next", "\u4E0B\u4E00\u4E2A", width = "100%"))
                         ),
                         actionButton("near_miss_jump", "\u8DF3\u8F6C\u5230 raster \u4E2D\u9009\u4E2D\u9879", width = "100%"),
                         tags$hr(),
                         checkboxInput("near_miss_apply_and_rerun", "\u5E94\u7528\u9608\u503C\u5E76\u7ACB\u5373\u91CD\u65B0\u8FD0\u884C\u68C0\u6D4B\u5668", FALSE),
                         actionButton("near_miss_apply_threshold", "\u5E94\u7528\u5EFA\u8BAE\u9608\u503C", width = "100%"),
	                         actionButton("near_miss_accept_manual", "\u5C06\u6240\u9009\u9879\u63A5\u53D7\u4E3A MANUAL \u6807\u7B7E", width = "100%"),
	                         tags$div(class = "small-note",
		                                  "\u9884\u89C8\u884C\u662F\u53CD\u4E8B\u5B9E\u7ED3\u679C\uFF1A\u82E5\u6240\u9009\u5019\u9009\u53EA\u9700\u6539\u4E00\u4E2A\u9608\u503C\u4E14\u80FD\u901A\u8FC7\u6700\u7EC8 AUTO gate\uFF0C\u201C\u5E94\u7528\u5EFA\u8BAE\u9608\u503C\u201D\u4F1A\u66F4\u65B0\u8BE5\u53C2\u6570\uFF1B\u82E5\u9700\u591A\u4E2A\u9608\u503C\u540C\u65F6\u653E\u5BBD\u6216\u4ECD\u4F1A\u88AB\u6700\u7EC8 gate \u963B\u65AD\uFF0C\u5219\u4E0D\u6539\u5168\u5C40\u9608\u503C\uFF0C\u800C\u662F\u5C06\u5DF2\u590D\u6838\u7684\u8BE5\u5177\u4F53\u5019\u9009\u5199\u4E3A MANUAL \u6700\u7EC8\u6807\u7B7E\u3002")
	                       ),
                       column(
                         9,
                         plotlyOutput("near_miss_plot", height = "32vh"),
                         tags$hr(),
                         verbatimTextOutput("near_miss_rerun_summary"),
                         verbatimTextOutput("near_miss_details"),
                         DTOutput("near_miss_table")
                       )
                     ))),
        

        tabPanel("\u652F\u6301\u65B9\u6CD5",
                 div(class = "plot-scroll", style = "max-height: 85vh;",
                     fluidRow(
                       column(3,
                              h4("Burst \u9608\u503C\u652F\u6301"),
                              tags$div(class = "small-note",
                                       "\u652F\u6301\u65B9\u6CD5\u4E3A burst-ISI \u9608\u503C\u6821\u51C6\u63D0\u4F9B\u5916\u90E8\u8BC1\u636E\uFF1B\u4E0D\u4F1A\u5199\u5165 AUTO \u6807\u7B7E\uFF0C\u4E5F\u4E0D\u4F1A\u66FF\u4EE3\u4E3B\u68C0\u6D4B\u5668\u3002"),
                              selectInput("support_method_view", "\u652F\u6301\u65B9\u6CD5\u9762\u677F",
                                          choices = c("Mean-ISI \u6587\u7AE0\u65B9\u6CD5" = "misi",
                                                      "Pasquale LogISI / newBD" = "logisi"),
                                          selected = "misi"),
                              conditionalPanel("input.support_method_view == 'misi'",
                                tags$hr(),
                                h4("Mean-ISI \u652F\u6301"),
                                tags$div(class = "small-note",
                                         "\u5C06 Chen \u7B49\u4EBA\u7684\u81EA\u9002\u5E94 mean inter-spike interval \u65B9\u6CD5\u4F5C\u4E3A\u652F\u6301\u5C42\u5B9E\u73B0\u3002"),
                                checkboxInput("misi_support_visible_only", "\u4EC5\u5728\u5F53\u524D\u53EF\u89C1 trains \u4E0A\u8FD0\u884C", TRUE),
                                numericInput("misi_min_isi_count", "\u6700\u5C0F\u8FDE\u7EED ISI \u6570 k", value = 2, min = 2, step = 1),
                                numericInput("misi_max_isi_count", "\u6700\u5927\u8FDE\u7EED ISI \u6570 k\uFF080 = \u5B8C\u6574\u641C\u7D22\uFF09", value = 0, min = 0, step = 1),
                                numericInput("misi_max_windows", "\u6700\u5927\u6D4B\u8BD5\u7A97\u53E3\u4FDD\u62A4", value = 2000000, min = 1000, step = 1000),
                                numericInput("misi_min_spikes", "\u6BCF\u4E2A\u652F\u6301 burst \u7684\u6700\u5C0F spike \u6570", value = 3, min = 2, step = 1),
                                numericInput("misi_min_duration_ms", "\u652F\u6301 burst \u6700\u5C0F\u6301\u7EED\u65F6\u95F4\uFF08ms\uFF1B0 = \u4E0D\u542F\u7528\uFF09", value = 0, min = 0, step = 1),
                                numericInput("misi_overlap_fraction", "\u4E0E burst-family \u5019\u9009\u7684\u91CD\u53E0\u6BD4\u4F8B", value = 0.10, min = 0, max = 1, step = 0.05),
                                actionButton("run_misi_support", "\u8FD0\u884C Mean-ISI \u652F\u6301", width = "100%"),
                                downloadButton("download_misi_support_zip", "\u4E0B\u8F7D MISI \u652F\u6301 ZIP", width = "100%")
                              ),
                              tags$hr(),
                              h4("\u652F\u6301\u65B9\u6CD5 burst ISI \u6761\u5E26"),
                              tags$div(class = "small-note",
                                       tags$b("\u652F\u6301 burst \u6761\u5E26\u5728\u54EA\u91CC\uFF1F"),
                                       "\u8FD0\u884C Mean-ISI \u548C/\u6216 LogISI \u652F\u6301\u540E\uFF0C\u5019\u9009 burst \u4F1A\u6807\u8BB0\u5728\u76F8\u90BB spike \u4E4B\u95F4\u7684 ISI \u533A\u95F4\u4E0A\uFF0C\u800C\u4E0D\u662F\u5B64\u7ACB spike tick \u4E0A\u3002Mean-ISI ISI \u6761\u5E26\u7ED8\u5236\u5728\u6BCF\u6761 spike-train \u884C\u7A0D\u4E0A\u65B9\uFF08#8A7FFF\uFF09\uFF1BLogISI / newBD ISI \u6761\u5E26\u7ED8\u5236\u5728\u7A0D\u4E0B\u65B9\uFF08#F58E90\uFF09\u3002\u53EF\u9009\u4E8B\u4EF6\u5305\u7EDC\u4EE5\u534A\u900F\u660E\u65B9\u5F0F\u663E\u793A\u5B8C\u6574\u5019\u9009\u8303\u56F4\u3002"),
                              checkboxGroupInput("support_overlay_methods", "\u53E0\u52A0\u652F\u6301\u65B9\u6CD5\u68C0\u6D4B\u7ED3\u679C",
                                                 choices = c("Mean-ISI" = "misi", "LogISI / newBD" = "logisi"),
                                                 selected = c("misi", "logisi")),
                              radioButtons("support_overlay_x_axis", "\u652F\u6301\u65B9\u6CD5 raster X \u8F74",
                                           choices = c("\u5BF9\u9F50\u65F6\u95F4\uFF0C\u4E0E\u4E3B raster \u540C\u5C3A\u5EA6" = "aligned",
                                                       "\u539F\u59CB spike timestamp\uFF08\u79D2\uFF09" = "timestamp"),
                                           selected = "aligned"),
                              checkboxInput("support_overlay_sync_window", "\u540C\u6B65\u4E3B raster \u65F6\u95F4\u7A97", TRUE),
                              checkboxInput("support_overlay_show_span_strips", "\u540C\u65F6\u7ED8\u5236\u534A\u900F\u660E\u4E8B\u4EF6\u5305\u7EDC\u6761", FALSE),
                              checkboxInput("support_overlay_show_auto_burst_family", "\u540C\u65F6\u663E\u793A\u5F53\u524D AUTO burst-family \u6761\u5E26", TRUE),
                              conditionalPanel("input.support_method_view == 'logisi'",
                                tags$hr(),
                                h4("LogISI / newBD \u652F\u6301"),
                                tags$div(class = "small-note",
                                         "\u5C06 Pasquale \u7B49\u4EBA\u7684 logISIH \u9608\u503C\u4F30\u8BA1\u548C newBD \u903B\u8F91\u4F5C\u4E3A\u652F\u6301\u5C42\u5B9E\u73B0\u3002"),
                                checkboxInput("logisi_support_visible_only", "\u4EC5\u5728\u5F53\u524D\u53EF\u89C1 trains \u4E0A\u8FD0\u884C", TRUE),
                                numericInput("logisi_min_num_spikes", "\u6BCF\u4E2A\u652F\u6301 burst \u7684\u6700\u5C0F spike \u6570", value = 5, min = 2, step = 1),
                                numericInput("logisi_void_threshold", "Void \u53C2\u6570\u9608\u503C", value = 0.70, min = 0, max = 1, step = 0.05),
                                numericInput("logisi_peak_window_ms", "burst \u5185\u5CF0\u503C\u7A97\u53E3\uFF08ms\uFF09", value = 100, min = 1, step = 1),
                                numericInput("logisi_core_reference_ms", "Core \u53C2\u8003 maxISI1\uFF08ms\uFF09", value = 100, min = 1, step = 1),
                                numericInput("logisi_max_reasonable_ms", "\u5408\u7406 ISIth \u6700\u5927\u503C\uFF08ms\uFF09", value = 1000, min = 1, step = 10),
                                checkboxInput("logisi_fallback_ch", "ISIth \u65E0\u6CD5\u89E3\u6790\u65F6\u56DE\u9000\u5230 100 ms CH-style \u68C0\u6D4B\u5668", TRUE),
                                numericInput("logisi_fallback_maxisi_ms", "\u56DE\u9000 maxISI\uFF08ms\uFF09", value = 100, min = 1, step = 1),
                                numericInput("logisi_overlap_fraction", "\u4E0E burst-family \u5019\u9009\u7684\u91CD\u53E0\u6BD4\u4F8B", value = 0.10, min = 0, max = 1, step = 0.05),
                                actionButton("run_logisi_support", "\u8FD0\u884C LogISI \u652F\u6301", width = "100%"),
                                downloadButton("download_logisi_support_zip", "\u4E0B\u8F7D LogISI \u652F\u6301 ZIP", width = "100%")
                              )
                       ),
                       column(9,
                              h4("\u652F\u6301\u65B9\u6CD5 burst ISI \u6761\u5E26 on spike trains"),
                              tags$div(class = "small-note",
                                       tags$b("\u9605\u8BFB\u6307\u5357\uFF1A"),
                                       "\u652F\u6301\u65B9\u6CD5\u68C0\u6D4B\u5230\u7684 burst \u5019\u9009\u4EE5\u65B9\u6CD5\u989C\u8272\u6761\u663E\u793A\u5728\u76F8\u90BB spike \u7684 ISI \u533A\u95F4\u4E0A\uFF1A\u4E0A\u65B9 ISI \u6761 = Mean-ISI (#8A7FFF)\uFF1B\u4E0B\u65B9 ISI \u6761 = LogISI / newBD (#F58E90)\u3002\u9ED1\u8272\u7AD6\u7EBF\u4ECD\u8868\u793A spike timestamp\u3002\u9ED8\u8BA4\u5173\u95ED\u534A\u900F\u660E\u4E8B\u4EF6\u5305\u7EDC\u3002\u542F\u7528\u65F6\uFF0C\u4E2D\u95F4\u6761\u5E26\u8868\u793A\u5F53\u524D AUTO burst-family\u3002"),
                              plotlyOutput("support_burst_raster_plot", height = "42vh"),
                              tags$hr(),
                              tabsetPanel(
                                tabPanel("Mean-ISI",
                                  h4("Mean-ISI \u9608\u503C\u652F\u6301\u6458\u8981"),
                                  DTOutput("misi_support_report_table"),
                                  tags$hr(),
                                  h4("\u6309 train \u7684 ML \u9608\u503C"),
                                  DTOutput("misi_threshold_table"),
                                  tags$hr(),
                                  h4("MISI \u652F\u6301 burst \u5019\u9009"),
                                  DTOutput("misi_burst_table")
                                ),
                                tabPanel("LogISI / newBD",
                                  h4("LogISI \u9608\u503C\u652F\u6301\u6458\u8981"),
                                  DTOutput("logisi_support_report_table"),
                                  tags$hr(),
                                  h4("\u6309 train \u7684 ISIth \u9608\u503C"),
                                  DTOutput("logisi_threshold_table"),
                                  tags$hr(),
                                  h4("LogISI \u652F\u6301 burst \u5019\u9009"),
                                  DTOutput("logisi_burst_table"),
                                  tags$hr(),
                                  h4("logISIH \u8868"),
                                  DTOutput("logisi_hist_table")
                                )
                              ))
                     ))),
        
        tabPanel("\u624B\u52A8\u6807\u8BB0 vs \u68C0\u6D4B\u5668\u62A5\u544A",
                 fluidRow(
                   column(4,
                          radioButtons("eval_learned_ranges_mode", "\u8BC4\u4F30\u6A21\u5F0F",
                                       choices = c("\u4F7F\u7528\u5F53\u524D\u5B66\u4E60\u5230\u7684 train-specific \u8303\u56F4" = "use",
                                                   "shadow \u8BC4\u4F30\u65F6\u7981\u7528\u5DF2\u5B66\u4E60\u8303\u56F4" = "disable"),
                                       selected = "use"),
                          selectInput("manual_eval_metric_mode", "\u6307\u6807\u89E3\u91CA",
                                      choices = c("\u4E25\u683C\u9AD8\u7F6E\u4FE1\uFF1Apossible_burst \u5355\u72EC\u7EDF\u8BA1" = "strict_high_confidence",
                                                  "\u5019\u9009\u5BB6\u65CF\uFF1Aburst + possible_burst = burst_family" = "candidate_family",
                                                  "\u590D\u6838\u8F85\u52A9\uFF1A\u5355\u72EC\u62A5\u544A\u6700\u7EC8\u590D\u6838\u6807\u7B7E" = "review_assisted"),
                                      selected = "strict_high_confidence"),
                          actionButton("eval_manual_detector", "\u8BC4\u4F30\u68C0\u6D4B\u5668 vs \u624B\u52A8\u6807\u7B7E", width = "100%")),
                   column(8,
                          helpText("\u8FD0\u884C\u4E00\u6B21\u4E0D\u9501\u5B9A manual \u6807\u7B7E\u7684 shadow \u68C0\u6D4B\uFF0C\u7136\u540E\u5728\u624B\u52A8\u6807\u8BB0\u5B50\u96C6\u4E0A\u6BD4\u8F83 AUTO \u9884\u6D4B\u4E0E manual \u6807\u7B7E\u3002\u4E0D\u4F1A\u4FEE\u6539\u6570\u636E\u96C6\u3002"),
                          helpText("\u4E25\u683C\u6A21\u5F0F\u5C06 possible_burst \u4F5C\u4E3A\u72EC\u7ACB\u590D\u6838\u7C7B\u522B\u3002\u5019\u9009\u5BB6\u65CF\u6A21\u5F0F\u4EC5\u5408\u5E76 burst + possible_burst \u4EE5\u8BC4\u4F30\u5019\u9009\u751F\u6210\u7075\u654F\u5EA6\u3002"),
                          helpText("\u4F7F\u7528\u5F53\u524D\u5DF2\u5B66\u4E60\u8303\u56F4\u63D0\u4F9B\u6821\u51C6/\u62A5\u544A\u89C6\u89D2\uFF1B\u7981\u7528\u5DF2\u5B66\u4E60\u8303\u56F4\u66F4\u63A5\u8FD1\u65E0 train-specific manual \u5148\u9A8C\u7684\u89C4\u5219\u68C0\u6D4B\u5668\u76F2\u68C0\u3002"),
                          DTOutput("manual_detector_meta"))
                 ),
                 h4("\u624B\u52A8\u6807\u8BB0 ISI \u7684\u9010\u7C7B\u6307\u6807"),
                 DTOutput("manual_detector_metrics"),
                 h4("\u6DF7\u6DC6\u77E9\u9635\u8BA1\u6570"),
                 DTOutput("manual_detector_confusion"),
                 h4("\u624B\u52A8\u4E8B\u4EF6\u7EA7\u91CD\u53E0"),
                 DTOutput("manual_detector_events")),

        tabPanel("\u79D1\u5B66\u9A8C\u8BC1",
                 fluidRow(
                   column(4,
                          sliderInput("sci_val_fraction", "\u9A8C\u8BC1 train \u6BD4\u4F8B", min = 0.1, max = 0.8, value = 0.25, step = 0.05),
                          numericInput("sci_val_seed", "\u968F\u673A\u79CD\u5B50", value = 1, min = 1, step = 1),
                          numericInput("sci_val_iou", "\u4E8B\u4EF6\u7EA7 IoU \u9608\u503C", value = 0.25, min = 0.05, max = 1, step = 0.05),
                          selectInput("sci_val_metric_mode", "\u6307\u6807\u89E3\u91CA",
                                      choices = c("\u4E25\u683C\u9AD8\u7F6E\u4FE1" = "strict_high_confidence",
                                                  "\u5019\u9009\u5BB6\u65CF / \u5019\u9009\u53EC\u56DE" = "candidate_family"),
                                      selected = "strict_high_confidence"),
	                          checkboxInput("sci_val_use_learned_ranges", "\u4F7F\u7528\u5F53\u524D\u5B66\u4E60\u5230\u7684 train-specific \u8303\u56F4", value = TRUE),
	                          actionButton("run_scientific_validation", "\u8FD0\u884C\u79D1\u5B66\u9A8C\u8BC1\u62A5\u544A", width = "100%"),
	                          tags$hr(),
	                          h4("Basic \u53C2\u6570\u654F\u611F\u6027"),
	                          uiOutput("parameter_sensitivity_train_selector"),
	                          uiOutput("parameter_sensitivity_path_selector"),
	                          numericInput("parameter_sensitivity_max_params", "\u6700\u591A\u626B\u63CF\u53C2\u6570", value = 6, min = 1, max = 12, step = 1),
	                          numericInput("parameter_sensitivity_max_trains", "\u6700\u591A\u626B\u63CF train", value = 3, min = 1, max = 20, step = 1),
	                          numericInput("parameter_sensitivity_relative_step", "\u5355\u53C2\u6570\u76F8\u5BF9\u6270\u52A8", value = 0.25, min = 0.05, max = 0.95, step = 0.05),
	                          selectInput("parameter_sensitivity_plot_metric", "\u66F2\u7EBF\u6307\u6807",
	                                      choices = c("macro F1" = "macro_F1", "macro precision" = "macro_precision", "macro recall" = "macro_recall"),
	                                      selected = "macro_F1"),
	                          actionButton("run_parameter_sensitivity_scan", "\u8FD0\u884C\u4E8B\u4EF6\u7EA7\u53C2\u6570\u654F\u611F\u6027", width = "100%"),
	                          downloadButton("download_parameter_sensitivity_zip", "\u4E0B\u8F7D\u654F\u611F\u6027 CSV ZIP", width = "100%")),
	                   column(8,
	                          helpText("\u79D1\u5B66\u9A8C\u8BC1\u4EE5\u624B\u52A8\u6807\u7B7E\u4F5C\u4E3A\u771F\u503C\uFF0C\u8FD0\u884C\u4E00\u6B21\u4E0D\u9501\u5B9A\u624B\u52A8\u533A\u95F4\u7684 shadow \u68C0\u6D4B\uFF0C\u5C06\u5DF2\u6807\u8BB0 train \u5206\u6210\u6821\u51C6/\u9A8C\u8BC1\u96C6\uFF0C\u5E76\u62A5\u544A\u4E8B\u4EF6\u7EA7\u6307\u6807\u3002"),
	                          helpText("\u4E25\u683C\u6A21\u5F0F\u5C06 possible_burst \u4F5C\u4E3A\u590D\u6838\u7C7B\u3002\u5019\u9009\u5BB6\u65CF\u6A21\u5F0F\u5408\u5E76 burst + long_burst + possible_burst\uFF0C\u7528\u4E8E\u8BC4\u4F30\u5019\u9009\u53EC\u56DE\uFF0C\u800C\u975E\u9AD8\u7F6E\u4FE1 burst \u51C6\u786E\u7387\u3002"),
	                          DTOutput("scientific_validation_meta"))
                 ),
                 h4("Train \u5212\u5206"),
                 DTOutput("scientific_validation_split"),
                 h4("\u9A8C\u8BC1\u6307\u6807"),
                 DTOutput("scientific_validation_metrics"),
                 h4("\u6821\u51C6\u6307\u6807"),
                 DTOutput("scientific_validation_calibration_metrics"),
	                 h4("\u8FC7\u62DF\u5408\u62A5\u544A"),
	                 DTOutput("scientific_validation_overfit_report"),
	                 h4("\u9A8C\u8BC1\u4E8B\u4EF6\u5339\u914D"),
	                 DTOutput("scientific_validation_matches"),
	                 h4("\u53C2\u6570\u654F\u611F\u6027\u6458\u8981"),
	                 div(class = "parameter-validation-summary", verbatimTextOutput("parameter_sensitivity_status")),
	                 plotlyOutput("parameter_sensitivity_metric_plot", height = "34vh"),
	                 DTOutput("parameter_sensitivity_summary_table"),
	                 h4("\u4E8B\u4EF6\u7EA7\u6307\u6807 vs \u53C2\u6570\u53D8\u4F53"),
	                 DTOutput("parameter_sensitivity_metrics_table"),
	                 h4("\u4E8B\u4EF6\u5339\u914D\u660E\u7EC6 vs \u53C2\u6570\u53D8\u4F53"),
	                 DTOutput("parameter_sensitivity_matches_table")),
        tabPanel("\u6279\u5904\u7406 / API",
                 h4("\u5F53\u524D\u5185\u5B58\u4E2D\u7684\u6279\u5904\u7406"),
                 helpText("\u4F7F\u7528\u5F53\u524D UI \u53C2\u6570\u5904\u7406\u5185\u5B58\u4E2D\u5DF2\u52A0\u8F7D\u7684\u6240\u6709\u6570\u636E\u96C6\u3002\u7ED3\u679C\u53EF\u5BFC\u51FA\u4E3A\u5355\u4E2A ZIP\u3002\u547D\u4EE4\u884C\u4F7F\u7528\u65F6\u53EF\u8C03\u7528\u5305 API \u4E2D\u7684 stpd_detect() \u6216 run_detector()\u3002"),
                 fluidRow(
                   column(4, actionButton("run_all_datasets", "\u5BF9\u6240\u6709\u5DF2\u52A0\u8F7D\u6570\u636E\u96C6\u8FD0\u884C\u68C0\u6D4B\u5668", width = "100%")),
                   column(4, downloadButton("download_batch_results_zip", "\u4E0B\u8F7D\u6279\u5904\u7406\u7ED3\u679C ZIP", width = "100%")),
                   column(4, verbatimTextOutput("batch_status"))
                 )),
        tabPanel("\u65B9\u6CD5 / \u5BA1\u8BA1\u8BF4\u660E",
                 div(class = "plot-scroll", style = "max-height: 85vh;",
                     h4("\u89E3\u91CA\u4E0E\u53EF\u590D\u73B0\u6027\u8BF4\u660E"),
                     verbatimTextOutput("methodological_warning"),
                     h4("\u9884\u8BBE\u76EE\u5F55"),
                     DTOutput("preset_catalog_table"),
                     h4("\u53C2\u6570\u6CBB\u7406\u6458\u8981"),
                     DTOutput("governance_summary_table"),
                     h4("\u53C2\u6570\u62A5\u544A\uFF1A\u5F53\u524D\u503C vs \u9ED8\u8BA4\u503C / \u9884\u8BBE"),
                     helpText("modular reference\uFF1A\u65B9\u6CD5\u62A5\u544A\u53EF\u4F7F\u7528\u8BE5\u8868\u3002\u5B83\u5217\u51FA\u5168\u90E8\u53C2\u6570\uFF0C\u5E76\u9AD8\u4EAE\u4E0E\u9ED8\u8BA4\u503C\u6216\u6240\u9009\u9884\u8BBE\u4E0D\u540C\u7684\u503C\u3002"),
                     DTOutput("parameters_report_table"),
                     h4("\u5E73\u7A33\u6027 QC"),
                     helpText("\u4EC5\u4F9B\u53C2\u8003\uFF1A\u6ED1\u52A8\u4E2D\u4F4D ISI \u5927\u5E45\u6F02\u79FB\u63D0\u793A\u72B6\u6001\u53D8\u5316\uFF1Bpause/global \u9608\u503C\u53EF\u80FD\u9700\u8981\u5206\u6BB5\u5904\u7406\u3002"),
                     DTOutput("stationarity_qc_table"),
                     h4("\u9A8C\u8BC1\u5EFA\u8BAE"),
                     DTOutput("validation_guidance_table"),
                     h4("\u8FC7\u62DF\u5408\u8B66\u544A\u62A5\u544A"),
                     DTOutput("overfit_warning_table"),
                     h4("\u8BED\u4E49\u4E00\u81F4\u6027\u62A5\u544A"),
                     DTOutput("semantic_consistency_report"),
                     h4("\u5019\u9009\u7279\u5F81\u5BA1\u8BA1\u9884\u89C8"),
                     DTOutput("candidate_features_audit_table"),
                     h4("\u5206\u5E03\u8BC1\u636E\u5C42\uFF08\u5019\u9009\u4E8B\u4EF6\uFF09"),
                     helpText("\u5206\u5E03\u8BC1\u636E\u4EC5\u4F5C\u4E3A audit/support layer\uFF1A\u5B83\u91CF\u5316 local ISI CDF/tail\u3001logISI KS/W1\u3001CV2/LV/LvR \u7B49\u8BC1\u636E\uFF0C\u4E0D\u76F4\u63A5\u6539\u5199\u68C0\u6D4B\u6807\u7B7E\u6216\u8FB9\u754C\u3002"),
                     fluidRow(
                       column(6, actionButton("distribution_evidence_jump", "\u8DF3\u8F6C\u5230 raster \u4E2D\u9009\u4E2D\u5019\u9009", width = "100%")),
                       column(6, tags$div(class = "small-note", "\u9009\u4E2D\u4E00\u884C\u540E\u53EF\u76F4\u63A5\u8DF3\u8F6C\u5E76\u5728 raster \u4E2D\u9AD8\u4EAE\u8BE5\u5019\u9009\u533A\u95F4\u3002"))
                     ),
                     DTOutput("event_distribution_evidence_table"),
                     h4("Train-level distribution phenotype"),
                     DTOutput("train_distribution_features_table"),
                     h4("Spike-count PMF / Fano"),
                     DTOutput("spike_count_pmf_table"),
                     h4("\u6700\u7EC8\u5206\u7C7B\u5BA1\u8BA1\u9884\u89C8"),
                     DTOutput("final_classification_audit_table"),
                     h4("modular migration \u8FC1\u79FB\u8DEF\u7EBF\u56FE"),
                     DTOutput("migration_roadmap_table")
                 )
        ),
        tabPanel("\u68C0\u6D4B\u5668 / \u53C2\u6570",
                 div(class = "plot-scroll parameter-workbench", style = "max-height: 85vh;",
                     fluidRow(
                       column(
                         4,
                         div(
                           class = "parameter-control-card",
                           div(class = "section-kicker", "\u53C2\u6570"),
                           h4("Basic \u53C2\u6570"),
                           selectInput("contract_ui_level", "\u663E\u793A\u5C42\u7EA7",
                                       choices = c("Basic" = "basic", "Advanced" = "advanced", "Expert" = "expert", "All" = "all"),
                                       selected = "basic"),
                           tags$div(class = "small-note", "\u9ED8\u8BA4\u53EA\u5C55\u793A\u751F\u7269\u5B66\u7528\u6237\u6700\u5E38\u8C03\u7684 Basic \u53C2\u6570\uFF1BAdvanced / Expert \u4ECD\u53EF\u901A\u8FC7\u4E0A\u65B9\u7B5B\u9009\u8BBF\u95EE\u3002"),
                           actionButton("validate_params_now", "\u5237\u65B0\u9A8C\u8BC1", width = "100%"),
                           tags$hr(),
                           uiOutput("contract_parameter_controls")
                         ),
                         div(
                           class = "parameter-control-card",
                           div(class = "section-kicker", "\u9884\u89C8"),
                           h4("\u5C40\u90E8\u5DEE\u5F02\u91CD\u8DD1"),
                           uiOutput("parameter_delta_preview_train_selector"),
                           selectInput("parameter_delta_preview_baseline", "\u5BF9\u7167\u53C2\u6570",
                                       choices = c("\u9ED8\u8BA4\u53C2\u6570" = "default", "\u6700\u8FD1\u6B63\u5F0F\u8FD0\u884C\u53C2\u6570" = "last_run"),
                                       selected = "default"),
                           numericInput("parameter_delta_preview_max_trains", "\u6700\u591A train", value = 3, min = 1, max = 20, step = 1),
                           numericInput("parameter_delta_preview_iou", "\u5339\u914D IoU", value = 0.25, min = 0.05, max = 1, step = 0.05),
                           actionButton("run_parameter_delta_preview", "\u8FD0\u884C\u5DEE\u5F02\u9884\u89C8", width = "100%"),
                           downloadButton("download_parameter_delta_preview_zip", "\u5BFC\u51FA\u5DEE\u5F02 CSV ZIP", width = "100%"),
                           tags$div(class = "small-note", "\u9884\u89C8\u662F dry-run\uFF1A\u53EA\u6BD4\u8F83 AUTO \u4E8B\u4EF6\u5DEE\u5F02\uFF0C\u4E0D\u8986\u76D6\u6B63\u5F0F\u68C0\u6D4B\u7ED3\u679C\u3002")
                         ),
                         tags$details(
                           class = "parameter-control-card stpd-fold expert-fold",
                           tags$summary("YAML \u5BFC\u5165 / \u5BFC\u51FA"),
                           fileInput("params_yaml_in", "\u5BFC\u5165\u53C2\u6570 YAML", accept = c(".yml", ".yaml")),
                           downloadButton("params_yaml_out", "\u5BFC\u51FA\u5F53\u524D\u53C2\u6570 YAML", width = "100%"),
                           tags$div(class = "small-note", "\u5BFC\u51FA\u6587\u4EF6\u5305\u542B\u5F53\u524D UI \u53C2\u6570\u6811\u3001schema \u7248\u672C\u3001params_hash \u548C\u9A8C\u8BC1\u6458\u8981\u3002\u5BFC\u5165\u540E\u4F1A\u56DE\u586B\u4E13\u7528 UI \u4E0E contract-generated UI\u3002")
                         )
                       ),
                       column(
                         8,
                         div(
                           class = "parameter-results-card",
                           tabsetPanel(
                             type = "pills",
                             tabPanel(
                               "\u9A8C\u8BC1",
                               div(class = "parameter-validation-summary", verbatimTextOutput("parameter_validation_summary")),
                               DTOutput("parameter_validation_table")
                             ),
                             tabPanel(
                               "\u53D8\u66F4\u9884\u89C8",
                               tags$div(class = "small-note", "\u8FD9\u91CC\u4EC5\u5217\u51FA\u76F8\u5BF9\u9ED8\u8BA4\u503C\u5DF2\u6539\u53D8\u7684\u53C2\u6570\uFF0C\u5E76\u6807\u6CE8 Basic / Advanced / Expert \u5C42\u7EA7\u548C\u4E3B\u8981\u68C0\u6D4B\u5F71\u54CD\u3002"),
                               DTOutput("parameter_change_preview_table")
                             ),
                             tabPanel(
                               "\u5DEE\u5F02\u4E8B\u4EF6",
                               div(class = "parameter-validation-summary", verbatimTextOutput("parameter_delta_preview_status")),
                               DTOutput("parameter_delta_preview_summary_table"),
                               DTOutput("parameter_delta_preview_counts_table"),
                               DTOutput("parameter_delta_preview_events_table")
                             ),
                             tabPanel(
                               "Round-trip",
                               tags$div(class = "small-note", "\u5C06\u5F53\u524D UI \u53C2\u6570\u5199\u5165\u4E34\u65F6 YAML\uFF0C\u518D\u8BFB\u56DE\u5E76\u6BD4\u8F83 hash\uFF0C\u7528\u4E8E\u9632\u6B62\u5BFC\u5165/\u5BFC\u51FA\u4EA7\u751F\u9759\u9ED8\u6F02\u79FB\u3002"),
                               DTOutput("parameter_roundtrip_report_table")
                             )
                           )
                         )
                       )
                     )
                 )),
        
        tabPanel("\u81EA\u9002\u5E94 train \u8C03\u53C2",
                 div(class = "plot-scroll", style = "max-height: 85vh;",
                     fluidRow(
                       column(3,
                              h4("\u65E7\u7248 train-specific burst-ISI \u8303\u56F4"),
                              tags$div(class = "small-note",
                                       "\u6838\u5FC3\u68C0\u6D4B\u4F7F\u7528 dataset/manual ISI bands\u3002\u672C\u9762\u677F\u53EA\u7528\u4E8E\u4FDD\u5B58\u6216\u6E05\u9664\u65E7\u7248 train-specific \u8303\u56F4\uFF0C\u4EE5\u4F5C fallback/\u8BCA\u65AD\u7528\u9014\u3002"),
                              uiOutput("burst_range_selector_tab"),
                              sliderInput("burst_isi_pct_range_tab", "\u65E7\u7248\u767E\u5206\u4F4D\u533A\u95F4", min = 0, max = 100, value = c(0, 25), step = 1),
                              fluidRow(
                                column(6, numericInput("burst_isi_abs_low_tab", "\u7EDD\u5BF9\u6700\u5C0F\u503C", value = NA, min = 0, step = 1)),
                                column(6, numericInput("burst_isi_abs_high_tab", "\u7EDD\u5BF9\u6700\u5927\u503C", value = NA, min = 0, step = 1))
                              ),
                              checkboxInput("sync_sidebar_range", "\u5C06\u8BE5\u533A\u95F4\u7528\u4E8E\u4FA7\u680F\u201C\u4FDD\u5B58\u8303\u56F4\u201D", TRUE),
                              actionButton("apply_burst_isi_range_tab", "\u4E3A\u6240\u9009 trains \u4FDD\u5B58\u8303\u56F4", width = "100%"),
                              actionButton("clear_burst_isi_range_tab", "\u6E05\u9664\u6240\u9009 trains \u7684\u8303\u56F4", width = "100%"),
	                              actionButton("learn_burst_isi_range_manual_tab", "\u4ECE MANUAL bursts \u6821\u51C6 burst \u8303\u56F4", width = "100%"),
	                              tags$hr(),
	                              h4("\u624B\u52A8\u6807\u8BB0\u5F15\u5BFC\u6821\u51C6"),
	                              tags$div(class = "small-note", "\u8BE5\u529F\u80FD\u4ECE\u624B\u52A8\u793A\u4F8B\u5B66\u4E60 train-specific soft anchors\u3002\u624B\u52A8\u6807\u8BB0\u662F\u5C3A\u5EA6\u9528\u70B9\uFF0C\u4E0D\u662F\u786C\u8FB9\u754C\u6216\u76D1\u7763\u5206\u7C7B\u5668\uFF1B\u8BF7\u4F7F\u7528\u201C\u624B\u52A8\u6807\u8BB0 vs \u68C0\u6D4B\u5668\u62A5\u544A\u201D\u9A8C\u8BC1\u6CDB\u5316\u80FD\u529B\u3002"),
	                              actionButton("learn_tonic_isi_range_manual_tab", "\u4ECE MANUAL tonic \u5B66\u4E60 tonic soft anchors", width = "100%"),
	                              actionButton("learn_pause_isi_range_manual_tab", "\u4ECE MANUAL pause \u5B66\u4E60 pause soft anchors", width = "100%"),
	                              actionButton("learn_highfreq_isi_range_manual_tab", "\u4ECE MANUAL HF \u5B66\u4E60 high-frequency soft anchors", width = "100%"),
	                              tags$div(class = "small-note", "Tonic/pause/HF anchors \u53EA\u63D0\u4F9B\u5C3A\u5EA6\u5B9A\u4F4D\u548C\u6709\u754C\u52A0\u6743\uFF1B\u6700\u7EC8 AUTO \u4ECD\u7531\u5C40\u90E8\u7ED3\u6784\u3001\u8FDE\u7EED\u6027\u548C\u5BF9\u6BD4\u51B3\u5B9A\u3002"),
                              tags$hr(),
                              h4("ISI \u8868"),
                              uiOutput("isi_table_train_selector"),
                              sliderInput("isi_table_pct_filter", "\u663E\u793A\u6307\u5B9A\u767E\u5206\u4F4D\u5185\u7684 ISI", min = 0, max = 100, value = c(0, 100), step = 1)
                       ),
                       column(9,
                              h4("\u5DF2\u4FDD\u5B58 burst-ISI \u8303\u56F4"),
                              DTOutput("burst_range_table"),
                              tags$hr(),
                              h4("\u624B\u52A8\u6821\u51C6\u7684 tonic-ISI \u8303\u56F4"),
                              DTOutput("tonic_range_table"),
                              tags$hr(),
	                              h4("\u624B\u52A8\u6821\u51C6\u7684 pause-ISI \u8303\u56F4"),
	                              DTOutput("pause_range_table"),
	                              tags$hr(),
	                              h4("\u624B\u52A8\u6821\u51C6\u7684 high-frequency soft anchors"),
	                              DTOutput("highfreq_range_table"),
	                              tags$hr(),
	                              h4("Per-train ISI \u767E\u5206\u4F4D\u8868"),
                              DTOutput("isi_percentile_table")
                       )
                     ))),
        
        tabPanel("\u795E\u7ECF\u7F51\u7EDC\u6A21\u578B",
                 div(class = "plot-scroll", style = "max-height: 85vh;",
                     fluidRow(
                       column(3,
                              h4("\u8BAD\u7EC3\u6570\u636E"),
                              radioButtons("ml_label_source", "\u7279\u5F81\u8868\u4F7F\u7528\u7684\u6807\u7B7E", choices = c("\u6700\u7EC8\u5BA1\u8BA1\u7ED3\u679C" = "audit_final", "\u624B\u52A8\u6807\u8BB0" = "manual", "\u81EA\u52A8\u68C0\u6D4B" = "auto", "\u6700\u7EC8\u6807\u7B7E" = "final"), selected = "audit_final"),
                              selectInput("ml_label_mode", "\u673A\u5668\u5B66\u4E60\u6807\u7B7E\u6A21\u5F0F",
                                          choices = c("\u4E25\u683C\u9AD8\u7F6E\u4FE1\uFF1A\u5C06 possible_burst \u5355\u72EC\u4FDD\u7559" = "strict_high_confidence",
                                                      "\u5019\u9009\u5BB6\u65CF\uFF1Aburst + long_burst + possible_burst = burst_family" = "candidate_family"),
                                          selected = "strict_high_confidence"),
                              tags$div(class = "small-note", "\u4E25\u683C\u6A21\u5F0F\u7528\u4E8E\u9AD8\u7F6E\u4FE1\u5206\u7C7B\u5668\uFF1B\u5019\u9009\u5BB6\u65CF\u6A21\u5F0F\u7528\u4E8E\u5019\u9009\u53EC\u56DE\u6A21\u578B\uFF0C\u800C\u975E\u6700\u7EC8 burst \u51C6\u786E\u7387\u3002"),
                              checkboxInput("ml_train_pool", "\u4ECE\u6240\u9009\u6570\u636E\u96C6\u6C60\u8BAD\u7EC3", TRUE),
                              numericInput("ml_context_n", "\u4E24\u4FA7\u90BB\u8FD1 ISI \u7A97\u53E3", value = 3, min = 1, max = 10, step = 1),
                              checkboxInput("ml_fill_blank_others", "\u5C06\u672A\u6807\u8BB0\u6709\u6548 ISI \u89C6\u4E3A\u201C\u5176\u4ED6\u201D", FALSE),
                              tags$hr(),
                              h4("nnet \u6A21\u578B"),
                              numericInput("ml_hidden", "\u9690\u85CF\u5355\u5143\u6570", value = 12, min = 2, max = 100, step = 1),
                              numericInput("ml_decay", "\u6743\u91CD\u8870\u51CF", value = 0.001, min = 0, step = 0.001),
                              numericInput("ml_maxit", "\u6700\u5927\u8FED\u4EE3\u6B21\u6570", value = 300, min = 50, step = 50),
                              numericInput("ml_confidence", "\u5E94\u7528\u7F6E\u4FE1\u5EA6\u622A\u65AD", value = 0.60, min = 0, max = 1, step = 0.01),
                              checkboxInput("ml_event_smoothing", "\u5E94\u7528\u524D\u8FDB\u884C\u4E8B\u4EF6\u7EA7\u5E73\u6ED1/\u9A8C\u8BC1", TRUE),
                              checkboxInput("ml_nn_event_guardrails", "\u5F3A\u5236 NN \u4E8B\u4EF6\u8BED\u6CD5\u4FDD\u62A4\uFF08\u63A8\u8350\uFF0C\u9632\u6B62\u5355\u4E2A ISI \u88AB\u6807\u4E3A HF spiking\uFF09", TRUE),
                              checkboxInput("ml_apply_others", "\u5C06\u795E\u7ECF\u7F51\u7EDC others \u6807\u7B7E\u5E94\u7528\u5230 AUTO", FALSE),
                              actionButton("train_nn_model", "\u4F7F\u7528\u5F53\u524D\u6807\u7B7E\u8BAD\u7EC3\u6A21\u578B", width = "100%"),
                              downloadButton("download_nn_model_tab", "\u4E0B\u8F7D\u5F53\u524D\u795E\u7ECF\u7F51\u7EDC\u6A21\u578B\uFF08.rds\uFF09", width = "100%"),
                              tags$div(class = "small-note", "\u8BAD\u7EC3\u6216\u52A0\u8F7D\u6A21\u578B\u540E\uFF0C\u53EF\u70B9\u51FB\u4E0A\u65B9\u6309\u94AE\u5C06\u5F53\u524D\u6A21\u578B\u4FDD\u5B58\u4E3A .rds \u6587\u4EF6\u3002"),
                              fileInput("nn_model_in", "\u52A0\u8F7D\u5DF2\u8BAD\u7EC3\u6A21\u578B\uFF08.rds\uFF09", accept = ".rds"),
                              actionButton("apply_nn_model", "\u5C06\u6A21\u578B\u5E94\u7528\u5230\u5F53\u524D\u6570\u636E\u96C6 AUTO", width = "100%"),
                              tags$hr(),
                              radioButtons("ml_eval_source", "\u8BC4\u4F30\u6807\u7B7E", choices = c("\u624B\u52A8\u6807\u8BB0" = "manual", "\u6700\u7EC8\u5BA1\u8BA1\u7ED3\u679C" = "audit_final", "\u81EA\u52A8\u68C0\u6D4B" = "auto", "\u6700\u7EC8\u6807\u7B7E" = "final"), selected = "manual"),
                              radioButtons("ml_eval_mode", "\u8BC4\u4F30\u6A21\u5F0F",
                                           choices = c("\u5F53\u524D\u6570\u636E\u96C6\u4E0A\u7684\u5DF2\u52A0\u8F7D/\u5F53\u524D\u6A21\u578B" = "loaded_current",
                                                       "\u5728\u6240\u9009\u6570\u636E\u96C6\u6C60\u4E2D\u7559\u4E00\u6570\u636E\u96C6\u4EA4\u53C9\u9A8C\u8BC1" = "leave_one_dataset_out"),
                                           selected = "loaded_current"),
                              actionButton("evaluate_nn_model", "\u8BC4\u4F30\u6A21\u578B", width = "100%")
                       ),
                       column(9,
                              h4("\u6A21\u578B\u6458\u8981"),
                              verbatimTextOutput("nn_model_summary"),
                              tags$hr(),
                              h4("\u8BC4\u4F30"),
                              DTOutput("nn_eval_table"),
                              tags$hr(),
                              h4("\u7279\u5F81\u9884\u89C8"),
                              DTOutput("ml_feature_preview"),
                              tags$hr(),
                              h4("\u6700\u8FD1\u9884\u6D4B\u8868"),
                              DTOutput("nn_prediction_table")
                       )
                     ))),
        
        tabPanel("\u6570\u636E QC",
                 div(class = "plot-scroll", style = "max-height: 85vh;",
                     h4("\u6570\u636E\u96C6\u8D28\u91CF\u68C0\u67E5"),
                     tags$div(class = "small-note", "QC \u6839\u636E timestamp \u548C ISI \u8BA1\u7B97\u3002\u8868\u683C\u5217\u4FDD\u7559\u6240\u6709 QC \u6307\u6807\uFF1Bwarning_message \u53EA\u5217\u51FA\u5F53\u524D\u89E6\u53D1\u7684 warning/error\u3002"),
                     DTOutput("qc_table"),
                     tags$hr(),
                     h4("\u4F2A\u8FF9 ISI \u8BE6\u60C5"),
                     tags$div(class = "small-note", "\u4E0B\u65B9\u884C\u663E\u793A\u4F4E\u4E8E\u5F53\u524D\u4F2A\u8FF9/\u6700\u5C0F\u6709\u6548\u9608\u503C\u7684\u5177\u4F53 ISI \u533A\u95F4\u3002"),
                     DTOutput("artifact_isi_details_table"),
                     tags$hr(),
                     h4("\u91CD\u590D\u65F6\u95F4\u6233\u8BE6\u60C5"),
                     tags$div(class = "small-note", "\u4E0B\u65B9\u884C\u8BC6\u522B\u5F53\u524D\u6392\u5E8F train \u4E2D\u7684\u5B8C\u5168\u91CD\u590D timestamp\u3002\u8FD9\u4E9B\u4F1A\u4EA7\u751F 0 ISI\uFF0C\u9664\u975E\u4F60\u5728\u5BFC\u5165\u65F6\u660E\u786E\u9009\u62E9\u8B66\u544A/\u5408\u5E76\u7B56\u7565\uFF0C\u5426\u5219\u4F1A\u88AB\u89C6\u4E3A\u6570\u636E\u5B8C\u6574\u6027\u9519\u8BEF\u3002"),
                     DTOutput("duplicate_timestamp_details_table")
                 )),
        
        tabPanel("\u4E8B\u4EF6 / \u8F93\u51FA",
                 div(class = "plot-scroll", style = "max-height: 85vh;",
                     fluidRow(
                       column(4, radioButtons("events_view", "\u4E8B\u4EF6\u8868\u6765\u6E90", choices = c("\u6700\u7EC8\u5BA1\u8BA1\u7ED3\u679C" = "audit_final", "\u624B\u52A8\u6807\u8BB0" = "manual", "\u81EA\u52A8\u68C0\u6D4B" = "auto", "\u6700\u7EC8\u6807\u7B7E" = "final"), selected = "audit_final")),
                       column(8, DTOutput("events_table"))
                     ),
                     tags$hr(),
                     h4("\u91CD\u53E0\u540E\u6700\u5C0F\u89C4\u6A21\u5F3A\u5236\u68C0\u67E5"),
                     tags$div(class = "small-note", "\u6700\u7EC8\u4F18\u5148\u7EA7/\u91CD\u53E0\u89E3\u6790\u540E\u88AB\u79FB\u9664\u7684\u7247\u6BB5\u3002\u4F8B\u5982\uFF0C\u77ED\u4E8E HF spiking \u6700\u5C0F spike \u6570\u7684\u9AD8\u9891\u8FDE\u7EED\u53D1\u653E\u7247\u6BB5\u4F1A\u4ECE AUTO \u6807\u7B7E\u4E2D\u79FB\u9664\uFF0C\u800C\u4E0D\u4F1A\u663E\u793A\u4E3A\u6709\u6548\u4E8B\u4EF6\u3002"),
                     DTOutput("posthoc_fragment_audit_table"),
                     tags$hr(),
                     h4("\u6240\u9009\u7C07 A/B \u6BD4\u8F83"),
                     tags$div(class = "small-note", "\u5728 raster \u4E2D\u6846\u9009\u4E00\u4E2A\u7C07\uFF0C\u5E76\u5728\u5DE6\u4FA7\u9762\u677F\u70B9\u51FB\u201C\u8BBE\u7F6E\u6240\u9009\u7C07 A/B\u201D\u3002\u8868\u683C\u4F1A\u62A5\u544A spike \u6570\u3001\u6301\u7EED\u65F6\u95F4\u3001\u8FB9\u7F18\u5BF9\u6BD4\u3001\u53D8\u5F02\u6027\u548C\u5F53\u524D\u6807\u7B7E\u7684\u5BA2\u89C2\u5DEE\u5F02\u3002"),
                     DTOutput("cluster_compare_table")
                 ))
      )
    )
    )
  )
)
