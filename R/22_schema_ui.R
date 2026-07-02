
stpd_schema_ui_num_arg <- function(x) {
  out <- suppressWarnings(as.numeric(x))
  if (length(out) == 0 || !is.finite(out[1])) return(NA_real_)
  out[1]
}

stpd_schema_ui_choices <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x[1]) || !nzchar(as.character(x[1]))) return(character())
  strsplit(as.character(x[1]), "|", fixed = TRUE)[[1]]
}

stpd_schema_ui_group_label <- function(group) {
  labels <- stpd_contract_section_labels()
  out <- labels[as.character(group)]
  ifelse(is.na(out), as.character(group), out)
}

stpd_schema_ui_pattern_hint <- function(path = "", group = "", section = "") {
  txt <- tolower(paste(path, group, section, collapse = " "))
  if (grepl("long_burst", txt, fixed = TRUE)) return("long_burst")
  if (grepl("hf_tonic|high_frequency_tonic", txt)) return("high_frequency_tonic")
  if (grepl("hf_spiking|high_frequency_spiking|highfreq|high frequency|hf spiking", txt)) return("high_frequency_spiking")
  if (grepl("pause|\u6682\u505C", txt)) return("pause")
  if (grepl("tonic|\u5F3A\u76F4", txt)) return("tonic")
  if (grepl("burst|event_core|event_grammar|\u7206\u53D1", txt)) return("burst")
  ""
}

stpd_schema_ui_pattern_color <- function(pattern) {
  pattern <- as.character(pattern %||% "")
  if (!nzchar(pattern)) return("#cbd5e1")
  pal <- tryCatch(pattern_palette("pattern_color"), error = function(e) data.frame())
  if (!is.null(pal) && nrow(pal) > 0 && pattern %in% pal$pattern) {
    col <- pal$manual[pal$pattern == pattern][1]
    if (is.character(col) && nzchar(col)) return(col)
  }
  "#cbd5e1"
}

stpd_schema_ui_box_style <- function(pattern) {
  col <- stpd_schema_ui_pattern_color(pattern)
  paste0("border-color:", col, "; --pattern-color:", col, ";")
}

stpd_schema_ui_visible_schema <- function(schema, exclude_paths = character(), groups = NULL,
                                          scopes = NULL,
                                          ui_levels = NULL,
                                          include_types = c("numeric", "integer", "logical", "choice", "choice_vector", "text")) {
  if (is.null(schema) || nrow(schema) == 0) return(schema[0, , drop = FALSE])
  if (!is.null(exclude_paths) && length(exclude_paths) > 0 && "path" %in% names(schema)) {
    schema <- schema[!(as.character(schema$path) %in% as.character(exclude_paths)), , drop = FALSE]
  }
  if (!is.null(groups) && length(groups) > 0 && "group" %in% names(schema)) {
    schema <- schema[as.character(schema$group) %in% as.character(groups), , drop = FALSE]
  }
  if (!is.null(scopes) && length(scopes) > 0 && "schema_scope" %in% names(schema)) {
    schema <- schema[as.character(schema$schema_scope) %in% as.character(scopes), , drop = FALSE]
  }
  if (!is.null(ui_levels) && length(ui_levels) > 0 && "ui_level" %in% names(schema)) {
    ui_levels <- as.character(ui_levels)
    if (!("all" %in% ui_levels)) schema <- schema[as.character(schema$ui_level) %in% ui_levels, , drop = FALSE]
  }
  if (!is.null(include_types) && length(include_types) > 0 && "type" %in% names(schema)) {
    schema <- schema[as.character(schema$type) %in% as.character(include_types), , drop = FALSE]
  }
  if (nrow(schema) > 0 && all(c("section_order", "ui_order") %in% names(schema))) {
    schema <- schema[order(suppressWarnings(as.numeric(schema$section_order)), suppressWarnings(as.numeric(schema$ui_order)), schema$path), , drop = FALSE]
  }
  schema
}

stpd_schema_ui_control <- function(row, prefix = "schema_param_", show_notes = TRUE) {
  id <- paste0(prefix, row$input_id)
  label <- row$label
  pat_hint <- ""
  path_txt <- as.character(row$path %||% "")
  pat_hint <- stpd_schema_ui_pattern_hint(
    path = path_txt,
    group = as.character(row$group %||% ""),
    section = as.character(row$section %||% "")
  )
  if (nzchar(pat_hint) && exists("stpd_ui_pattern_label", mode = "function")) {
    label <- stpd_ui_pattern_label(label, pat_hint)
  }
  typ <- as.character(row$type)
  control_type <- as.character(row$control_type %||% "")
  if (!nzchar(control_type)) control_type <- stpd_contract_control_type(typ)
  val <- suppressWarnings(stpd_schema_value(row))
  choices <- stpd_schema_ui_choices(row$choices)
  ctrl <- switch(control_type,
    number = shiny::numericInput(id, label, value = as.numeric(val), min = stpd_schema_ui_num_arg(row$min), max = stpd_schema_ui_num_arg(row$max), step = stpd_schema_ui_num_arg(row$step)),
    integer = shiny::numericInput(id, label, value = as.integer(val), min = stpd_schema_ui_num_arg(row$min), max = stpd_schema_ui_num_arg(row$max), step = stpd_schema_ui_num_arg(row$step)),
    checkbox = shiny::checkboxInput(id, label, value = isTRUE(val)),
    select = shiny::selectInput(id, label, choices = choices, selected = as.character(val)),
    multiselect = shiny::checkboxGroupInput(id, label, choices = choices, selected = as.character(val)),
    shiny::textInput(id, label, value = as.character(val))
  )
  note <- as.character(row$help_text %||% "")
  if (!nzchar(note)) note <- as.character(row$scientific_note %||% "")
  unit <- as.character(row$unit %||% "")
  ui_level <- as.character(row$ui_level %||% "")
  section <- as.character(row$section %||% "")
  meta <- paste(c(if (nzchar(path_txt)) path_txt else NULL, if (nzchar(section)) section else NULL, if (nzchar(ui_level)) paste0("level: ", ui_level) else NULL, if (nzchar(unit)) paste0("unit: ", unit) else NULL), collapse = " | ")
  shiny::tagList(
    ctrl,
    if (isTRUE(show_notes) && nzchar(note)) shiny::tags$div(class = "small-note", note),
    if (!isTRUE(show_notes) && nzchar(meta)) shiny::tags$div(class = "small-note", meta)
  )
}

schema_ui_controls <- function(schema = stpd_parameter_schema(), prefix = "schema_param_",
                               exclude_paths = stpd_schema_ui_excluded_paths(),
                               groups = NULL, scopes = NULL,
                               ui_levels = NULL,
                               include_types = c("numeric", "integer", "logical", "choice", "choice_vector", "text"),
                               group_by = FALSE, title = "\u6838\u5FC3\u68C0\u6D4B\u5668\u53C2\u6570",
                               note = "\u4F2A\u8FF9\u3001\u7591\u4F3C\u4E0D\u5E94\u671F\u3001\u91CD\u590D\u65F6\u95F4\u6233\u7B56\u7565\u548C\u68C0\u6D4B\u5668\u5BB6\u65CF\u9009\u62E9\u5747\u5728\u5404\u81EA\u4E13\u5C5E UI \u533A\u57DF\u63A7\u5236\uFF0C\u4EE5\u907F\u514D\u91CD\u590D\u53C2\u6570\u6765\u6E90\u3002",
                               show_notes = TRUE, open_groups = FALSE, group_field = "group") {
  schema <- stpd_schema_ui_visible_schema(schema, exclude_paths = exclude_paths, groups = groups, scopes = scopes, ui_levels = ui_levels, include_types = include_types)
  if (nrow(schema) == 0) return(shiny::tagList())
  render_rows <- function(df) {
    lapply(seq_len(nrow(df)), function(ii) stpd_schema_ui_control(df[ii, , drop = FALSE], prefix = prefix, show_notes = show_notes))
  }
  controls <- if (isTRUE(group_by) && group_field %in% names(schema)) {
    group_order <- unique(as.character(schema[[group_field]]))
    lapply(group_order, function(grp) {
      rows <- schema[as.character(schema[[group_field]]) == grp, , drop = FALSE]
      pat_hint <- stpd_schema_ui_pattern_hint(
        path = "",
        group = paste(unique(as.character(rows$group %||% "")), collapse = " "),
        section = grp
      )
      shiny::tags$details(
        class = "schema-section-box",
        style = stpd_schema_ui_box_style(pat_hint),
        open = if (isTRUE(open_groups)) TRUE else NULL,
        shiny::tags$summary(shiny::strong(grp), " (", nrow(rows), ")"),
        shiny::tags$div(class = "schema-contract-group", render_rows(rows))
      )
    })
  } else {
    render_rows(schema)
  }
  shiny::tagList(
    if (!is.null(title) && nzchar(title)) shiny::tags$hr(),
    if (!is.null(title) && nzchar(title)) shiny::h5(title),
    if (!is.null(note) && nzchar(note)) shiny::tags$div(class = "small-note", note),
    controls
  )
}

stpd_contract_ui_schema <- function(groups = c("burst", "event_core", "event_grammar", "arbitration", "tonic", "highfreq", "pause", "detector", "classification", "state"),
                                    exclude_paths = stpd_contract_ui_excluded_paths(),
                                    scopes = "full_contract",
                                    ui_level = "all") {
  schema <- stpd_parameter_schema(scope = "all")
  stpd_schema_ui_visible_schema(
    schema,
    exclude_paths = exclude_paths,
    groups = groups,
    scopes = scopes,
    ui_levels = ui_level,
    include_types = c("numeric", "integer", "logical", "choice", "choice_vector", "text")
  )
}

stpd_contract_ui_excluded_paths <- function() {
  unique(c(stpd_parameter_schema()$path))
}

stpd_contract_ui_controls <- function(prefix = "contract_param_",
                                      ui_level = "basic",
                                      groups = c("burst", "event_core", "event_grammar", "arbitration", "tonic", "highfreq", "pause", "detector", "classification", "state"),
                                      exclude_paths = stpd_contract_ui_excluded_paths()) {
  schema <- stpd_contract_ui_schema(groups = groups, exclude_paths = exclude_paths, ui_level = ui_level)
  level_label <- c(basic = "Basic", advanced = "Advanced", expert = "Expert", all = "All")
  level_txt <- level_label[as.character(ui_level)[1]]
  if (is.na(level_txt)) level_txt <- as.character(ui_level)[1]
  shiny::tagList(
    shiny::h4(paste0("\u53C2\u6570\u5951\u7EA6\uFF08", level_txt, "\uFF09")),
    shiny::tags$div(class = "small-note", paste0("\u6B64\u9762\u677F\u7531 inst/config/parameters.yml \u7684 parameter_contract \u751F\u6210\uFF1B\u5F53\u524D\u663E\u793A ", nrow(schema), " \u4E2A\u53C2\u6570\u3002Basic \u9762\u5411\u5E38\u89C4\u79D1\u7814\u4F7F\u7528\uFF0CAdvanced \u9762\u5411\u7B97\u6CD5\u8C03\u53C2\uFF0CExpert \u9762\u5411\u8BCA\u65AD/\u517C\u5BB9/\u8FB9\u754C\u4FDD\u62A4\u3002")),
    schema_ui_controls(
      schema = schema,
      prefix = prefix,
      exclude_paths = character(),
      group_by = TRUE,
      title = NULL,
      note = NULL,
      show_notes = identical(as.character(ui_level)[1], "basic"),
      open_groups = identical(as.character(ui_level)[1], "basic"),
      group_field = "section"
    )
  )
}

stpd_schema_ui_dedicated_paths <- function() {
  c(
    "detector.min_valid_isi_sec",
    "detector.refractory_suspect_sec",
    "detector.refractory_suspect_action",
    "detector.patterns_to_run",
    "burst.local_compression_local_ratio_min",
    "burst.local_compression_candidate_class",
    "burst.final_tonic_like_action",
    "burst.burst_sublabel_regular_min_ISI_sec",
    "burst.burst_sublabel_regular_max_ISI_sec",
    "burst.burst_sublabel_regular_min_isi_n",
    "burst.burst_sublabel_regular_max_isi_n",
    "burst.burst_sublabel_regular_max_gap_isi_n",
    "burst.burst_sublabel_regular_max_gap_sec",
    "burst.burst_sublabel_link_labels",
    "burst.long_burst_enable",
    "burst.long_burst_min_spikes",
    "burst.long_burst_max_spikes",
    "burst.long_burst_edge_contrast_min",
    "burst.long_burst_edge_contrast_geom",
    "pause.global_median_guard",
    "pause.global_median_factor",
    "pause.exclude_occupied_context",
    "highfreq.spiking_min_spikes",
    "highfreq.spiking_short_fraction_min",
    "highfreq.spiking_gate_logic"
  )
}

stpd_schema_ui_excluded_paths <- function() {
  stpd_schema_ui_dedicated_paths()
}

schema_params_from_input <- function(params = default_params_sec(), input, schema = stpd_parameter_schema(), prefix = "schema_param_", exclude_paths = stpd_schema_ui_excluded_paths()) {
  p <- params
  if (!is.null(exclude_paths) && length(exclude_paths) > 0 && "path" %in% names(schema)) {
    schema <- schema[!(as.character(schema$path) %in% as.character(exclude_paths)), , drop = FALSE]
  }
  if (nrow(schema) == 0) return(p)
  for (ii in seq_len(nrow(schema))) {
    row <- schema[ii, , drop = FALSE]
    id <- paste0(prefix, row$input_id)
    val <- tryCatch(input[[id]], error = function(e) NULL)
    if (is.null(val)) next
    typ <- as.character(row$type)
    if (typ == "numeric") val <- suppressWarnings(as.numeric(val))
    if (typ == "integer") val <- suppressWarnings(as.integer(val))
    if (typ == "logical") val <- isTRUE(val)
    if (typ == "choice_vector") val <- as.character(val)
    p <- stpd_set_param(p, row$path, val)
  }
  p
}

stpd_params_from_schema_inputs <- schema_params_from_input

stpd_update_schema_inputs <- function(session, params, schema = stpd_parameter_schema(), prefix = "schema_param_", exclude_paths = stpd_schema_ui_excluded_paths()) {
  if (!is.null(exclude_paths) && length(exclude_paths) > 0 && "path" %in% names(schema)) {
    schema <- schema[!(as.character(schema$path) %in% as.character(exclude_paths)), , drop = FALSE]
  }
  if (nrow(schema) == 0) return(invisible(NULL))
  for (ii in seq_len(nrow(schema))) {
    row <- schema[ii, , drop = FALSE]
    id <- paste0(prefix, row$input_id)
    val <- stpd_get_param(params, row$path, stpd_schema_value(row))
    typ <- as.character(row$type)
    if (typ %in% c("numeric", "integer")) shiny::updateNumericInput(session, id, value = val)
    else if (typ == "logical") shiny::updateCheckboxInput(session, id, value = isTRUE(val))
	    else if (typ %in% c("choice", "choice_vector")) {
	      if (typ == "choice") shiny::updateSelectInput(session, id, selected = as.character(val))
	      else shiny::updateCheckboxGroupInput(session, id, selected = as.character(val))
	    } else if (typ %in% c("text", "character")) shiny::updateTextInput(session, id, value = as.character(val)[1])
	  }
	}
