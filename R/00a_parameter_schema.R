
# Declarative parameter schema.
# The schema is used for defaults, generated UI controls for non-duplicated
# key parameters, reports, and hashes. Parameters with dedicated UI sections
# remain in the schema for reporting but are not duplicated in the generated UI.

stpd_parameter_config_cache <- new.env(parent = emptyenv())

stpd_parameter_config_path <- function(path = NULL) {
  if (!is.null(path) && nzchar(path)) return(path)
  installed <- system.file("config", "parameters.yml", package = "SpikeTrainPatternDetector", mustWork = FALSE)
  if (nzchar(installed) && file.exists(installed)) return(installed)
  local <- file.path("inst", "config", "parameters.yml")
  if (file.exists(local)) return(normalizePath(local, mustWork = FALSE))
  stop("Parameter YAML config not found: inst/config/parameters.yml", call. = FALSE)
}

stpd_parameter_config <- function(path = NULL, reload = FALSE) {
  cfg_path <- stpd_parameter_config_path(path)
  cfg_info <- file.info(cfg_path)
  cfg_key <- if (is.null(path)) {
    paste("package-default", cfg_info$size %||% NA, as.numeric(cfg_info$mtime %||% NA), sep = ":")
  } else {
    normalizePath(cfg_path, winslash = "/", mustWork = FALSE)
  }
  if (!isTRUE(reload) && identical(stpd_parameter_config_cache$path, cfg_key) && is.list(stpd_parameter_config_cache$config)) {
    return(stpd_parameter_config_cache$config)
  }
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Package 'yaml' is required to load SpikeTrainPatternDetector parameter config.", call. = FALSE)
  }
  cfg <- yaml::read_yaml(cfg_path, eval.expr = FALSE)
  required <- c("runtime_defaults", "product_defaults", "key_schema", "eventness_schema", "parameter_contract")
  missing <- required[!vapply(required, function(nm) !is.null(cfg[[nm]]), logical(1))]
  if (length(missing) > 0) {
    stop("Parameter YAML is missing required section(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }
  stpd_parameter_config_cache$path <- cfg_key
  stpd_parameter_config_cache$config <- cfg
  cfg
}

stpd_parameter_config_section <- function(section, default = NULL, path = NULL) {
  cfg <- stpd_parameter_config(path = path)
  value <- cfg[[section]]
  if (is.null(value)) default else value
}

stpd_schema_rows_from_config <- function(section) {
  rows <- stpd_parameter_config_section(section, default = list())
  cols <- c(
    "path", "input_id", "group", "type", "default", "label", "choices", "min", "max", "step", "unit",
    "scientific_note", "schema_scope", "required", "ui_level", "ui_order", "section", "section_order",
    "advanced", "expert_only", "visible_if", "help_text", "control_type"
  )
  empty <- as.data.frame(stats::setNames(rep(list(character()), length(cols)), cols), stringsAsFactors = FALSE)
  if (is.null(rows) || length(rows) == 0) return(empty)
  out <- lapply(rows, function(row) {
    row <- as.list(row)
    vals <- stats::setNames(rep(list(""), length(cols)), cols)
    for (nm in intersect(names(row), cols)) {
      val <- row[[nm]]
      vals[[nm]] <- if (is.null(val) || length(val) == 0 || all(is.na(val))) "" else paste(as.character(val), collapse = if (identical(nm, "choices")) "|" else ",")
    }
    as.data.frame(vals, stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}

stpd_runtime_default_params <- function() {
  stpd_parameter_config_section("runtime_defaults", default = list())
}

stpd_default_patterns_to_run <- function() {
  defaults <- tryCatch(stpd_runtime_default_params(), error = function(e) list())
  defaults$detector$patterns_to_run %||% c("burst", "long_burst", "tonic", "high_frequency_tonic", "high_frequency_spiking", "pause", "others")
}

stpd_resolve_patterns_to_run <- function(selected = NULL, strict_subset = FALSE) {
  defaults <- as.character(stpd_default_patterns_to_run())
  selected <- as.character(selected %||% character(0))
  selected <- unique(selected[!is.na(selected) & nzchar(selected)])
  if (length(selected) == 0) return(defaults)
  if (isTRUE(strict_subset)) return(selected)
  unique(c(selected, defaults))
}

stpd_key_parameter_schema <- function() {
  rows <- stpd_schema_rows_from_config("key_schema")
  rows[, c("path", "input_id", "group", "type", "default", "label", "choices", "min", "max", "step", "scientific_note"), drop = FALSE]
}
stpd_schema_defaults <- function() {
  sch <- stpd_parameter_schema()
  stats::setNames(sch$default, sch$path)
}

stpd_schema_value <- function(row) {
  val <- as.character(row$default[1])
  typ <- as.character(row$type[1])
  if (typ %in% c("numeric")) return(as.numeric(val))
  if (typ %in% c("integer")) return(as.integer(val))
  if (typ %in% c("logical")) return(toupper(val) == "TRUE")
  if (typ %in% c("choice_vector")) return(strsplit(val, ",", fixed = TRUE)[[1]])
  val
}

stpd_contract_section_labels <- function() {
  c(
    detector = "Detector policy",
    event_core = "Event core",
    event_grammar = "Threshold grammar",
    arbitration = "Arbitration",
    burst = "Burst family",
    highfreq = "High frequency",
    tonic = "Tonic",
    pause = "Pause",
    classification = "Eventness audit",
    state = "State audit",
    spiketrainpattern = "Public product namespace",
    metadata = "Metadata"
  )
}

stpd_contract_section_order <- function(group) {
  ord <- c(
    detector = 10, event_core = 20, event_grammar = 30, arbitration = 40,
    burst = 50, highfreq = 60, tonic = 70, pause = 80,
    classification = 90, state = 100, spiketrainpattern = 900, metadata = 910
  )
  out <- unname(ord[as.character(group)])
  out[is.na(out)] <- 500
  out
}

stpd_contract_basic_paths <- function() {
  c(
    "detector.min_valid_isi_sec",
    "detector.refractory_suspect_sec",
    "detector.refractory_suspect_action",
    "detector.patterns_to_run",
    "event_core.enabled",
    "event_core.seed_band_lower_sec",
    "event_core.seed_band_upper_sec",
    "event_core.bridge_band_upper_sec",
    "event_core.boundary_floor_sec",
    "event_core.burst_contrast_min",
    "event_core.possible_burst_contrast_min",
    "event_core.min_seed_isi_count",
    "event_core.max_bridge_isi_count",
    "event_core.max_bridge_isi_fraction",
    "event_core.max_expansion_isi_each_side",
    "event_grammar.threshold_source_mode",
    "event_grammar.allow_one_sided_possible",
    "arbitration.enabled",
    "burst.T_seed",
    "burst.T_bridge",
    "burst.G_min",
    "burst.label_possible_burst",
    "burst.long_burst_enable",
    "burst.long_burst_min_spikes",
    "burst.long_burst_max_spikes",
    "burst.long_burst_edge_contrast_min",
    "highfreq.T_high_max",
    "highfreq.spiking_min_spikes",
    "highfreq.spiking_max_ISI_abs",
    "highfreq.spiking_short_fraction_min",
    "pause.T_seed",
    "pause.T_strong",
    "pause.global_median_guard",
    "pause.global_median_factor",
    "tonic.T_min",
    "tonic.T_max",
    "tonic.G_min"
  )
}

stpd_contract_ui_level_for_path <- function(path, group = "", schema_scope = "") {
  path <- as.character(path)
  group <- as.character(group)
  schema_scope <- as.character(schema_scope)
  out <- rep("advanced", length(path))
  out[path %in% stpd_contract_basic_paths()] <- "basic"
  audit <- schema_scope %in% c("eventness_audit", "extension") | group %in% c("classification", "state", "metadata", "spiketrainpattern")
  expert_pat <- paste(
    c(
      "adaptive", "train_.*ranges", "diagnostic", "audit", "candidate", "max_candidates", "max_rows",
      "preview", "cache", "plot_", "lod", "metadata", "freeze", "compat", "legacy", "fallback",
      "prefilter", "nms", "histogram", "bin_width", "q95", "soft_penalty", "dynamic",
      "manual_can", "manual_min", "dataset_isi", "seed_bridge_max", "structure_", "governance"
    ),
    collapse = "|"
  )
  out[audit | grepl(expert_pat, path, ignore.case = TRUE)] <- "expert"
  out[path %in% stpd_contract_basic_paths()] <- "basic"
  out
}

stpd_contract_control_type <- function(type) {
  type <- as.character(type %||% "")
  out <- ifelse(type == "logical", "checkbox",
                ifelse(type == "choice", "select",
                       ifelse(type == "choice_vector", "multiselect",
                              ifelse(type == "integer", "integer",
                                     ifelse(type == "numeric", "number",
                                            ifelse(type %in% c("text", "character"), "text", "none"))))))
  out
}

stpd_apply_contract_ui_metadata <- function(rows) {
  if (is.null(rows) || nrow(rows) == 0) return(rows)
  labels <- stpd_contract_section_labels()
  group <- as.character(rows$group %||% sub("\\..*$", "", rows$path))
  section <- unname(labels[group])
  section[is.na(section) | !nzchar(section)] <- group[is.na(section) | !nzchar(section)]
  derived_level <- stpd_contract_ui_level_for_path(rows$path, group = group, schema_scope = rows$schema_scope %||% "")
  derived_section_order <- stpd_contract_section_order(group)
  derived_ui_order <- derived_section_order * 1000 + stats::ave(seq_len(nrow(rows)), group, FUN = seq_along)
  generic_note <- grepl("YAML parameter contract row generated", as.character(rows$scientific_note %||% ""), fixed = TRUE)
  derived_help <- ifelse(
    generic_note,
    paste0("Contract-managed ", derived_level, " parameter: ", rows$path, ". Review biological range and validation warnings before changing."),
    as.character(rows$scientific_note %||% "")
  )
  fill <- function(name, value) {
    if (!(name %in% names(rows))) rows[[name]] <<- ""
    blank <- is.na(rows[[name]]) | !nzchar(as.character(rows[[name]]))
    value <- rep_len(as.character(value), nrow(rows))
    rows[[name]][blank] <<- value[blank]
  }
  fill("ui_level", derived_level)
  fill("section", section)
  fill("section_order", derived_section_order)
  fill("ui_order", derived_ui_order)
  fill("advanced", derived_level != "basic")
  fill("expert_only", derived_level == "expert")
  fill("visible_if", rep("", nrow(rows)))
  fill("help_text", derived_help)
  fill("control_type", stpd_contract_control_type(rows$type))
  rows
}

stpd_parameter_contract <- function() {
  rows <- stpd_schema_rows_from_config("parameter_contract")
  if (nrow(rows) == 0) return(rows)
  rows$group[!nzchar(rows$group)] <- sub("\\..*$", "", rows$path[!nzchar(rows$group)])
  rows$input_id[!nzchar(rows$input_id)] <- gsub("[^A-Za-z0-9_]+", "__", rows$path[!nzchar(rows$input_id)])
  rows$label[!nzchar(rows$label)] <- rows$path[!nzchar(rows$label)]
  rows$schema_scope[!nzchar(rows$schema_scope)] <- "full_contract"
  rows$required[!nzchar(rows$required)] <- "TRUE"
  rows <- stpd_apply_contract_ui_metadata(rows)
  rows[order(suppressWarnings(as.numeric(rows$section_order)), suppressWarnings(as.numeric(rows$ui_order)), rows$group, rows$path), , drop = FALSE]
}

stpd_get_param <- function(params, path, default = NULL) {
  parts <- strsplit(path, "\\.")[[1]]
  x <- params
  for (p in parts) {
    if (!is.list(x) || is.null(x[[p]])) return(default)
    x <- x[[p]]
  }
  x
}

stpd_set_param <- function(params, path, value) {
  parts <- strsplit(path, "\\.")[[1]]
  if (length(parts) == 0 || any(!nzchar(parts))) return(params)
  if (is.null(params) || !is.list(params)) params <- list()
  set_rec <- function(x, p) {
    if (length(p) == 1L) {
      x[[p[1]]] <- value
      return(x)
    }
    if (is.null(x[[p[1]]]) || !is.list(x[[p[1]]])) x[[p[1]]] <- list()
    x[[p[1]]] <- set_rec(x[[p[1]]], p[-1])
    x
  }
  set_rec(params, parts)
}

apply_schema_defaults <- function(params = default_params_sec(), schema = stpd_parameter_schema()) {
  for (ii in seq_len(nrow(schema))) {
    path <- schema$path[ii]
    if (is.null(stpd_get_param(params, path, NULL))) params <- stpd_set_param(params, path, stpd_schema_value(schema[ii, , drop = FALSE]))
  }
  params
}


# schema: full schema coverage. The key schema remains the default UI layer;
# the full schema is an audit/configuration layer that enumerates every scalar
# parameter found in the YAML-backed defaults. This keeps parameter reporting
# and future UI generation schema-driven without flooding the current Shiny panel.
stpd_flatten_params_for_schema <- function(x, prefix = "") {
  rows <- list()
  walk <- function(obj, pref) {
    if (is.list(obj) && !is.data.frame(obj)) {
      nms <- names(obj); if (is.null(nms)) nms <- as.character(seq_along(obj))
      for (ii in seq_along(obj)) {
        path <- if (nzchar(pref)) paste0(pref, ".", nms[ii]) else nms[ii]
        val <- obj[[ii]]
        if (is.list(val) && !is.data.frame(val) && !grepl("adaptive_train_ranges|train_.*ranges", path)) walk(val, path)
        else rows[[length(rows) + 1L]] <<- list(path = path, value = val)
      }
    } else rows[[length(rows) + 1L]] <<- list(path = pref, value = obj)
  }
  walk(x, prefix)
  rows
}

stpd_parameter_schema_full <- function(params = default_params_sec()) {
  contract <- stpd_parameter_contract()
  flat <- stpd_flatten_params_for_schema(params)
  if (nrow(contract) == 0 && length(flat) == 0) return(stpd_schema_add_eventness(stpd_key_parameter_schema()))
  existing <- contract$path %||% character()
  auto_rows <- lapply(flat, function(r) {
    if (r$path %in% existing) return(NULL)
    v <- r$value
    typ <- if (is.logical(v)) "logical" else if (is.integer(v)) "integer" else if (is.numeric(v)) "numeric" else if (is.character(v) && length(v) > 1) "choice_vector" else if (is.data.frame(v)) "data_frame" else if (is.list(v)) "list" else "text"
    val <- if (is.list(v) && !is.data.frame(v)) paste0("<list:", length(v), ">") else if (is.data.frame(v)) paste0("<data.frame:", nrow(v), "x", ncol(v), ">") else paste(as.character(v), collapse = ",")
    data.frame(
      path = r$path,
      input_id = gsub("[^A-Za-z0-9_]+", "__", r$path),
      group = sub("\\..*$", "", r$path),
      type = typ,
      default = val,
      label = r$path,
      choices = "",
      min = "",
      max = "",
      step = "",
      unit = "",
      scientific_note = "\u4ECE inst/config/parameters.yml \u81EA\u52A8\u53D1\u73B0\uFF1B\u7528\u4E8E\u5BA1\u8BA1/\u62A5\u544A\u548C schema-driven UI\u3002",
      schema_scope = "extension",
      required = "FALSE",
      ui_level = "expert",
      ui_order = "",
      section = sub("\\..*$", "", r$path),
      section_order = "500",
      advanced = "TRUE",
      expert_only = "TRUE",
      visible_if = "",
      help_text = "\u81EA\u52A8\u53D1\u73B0\u7684 extension \u53C2\u6570\uFF1B\u672A\u7ECF YAML contract \u5B8C\u6574\u6CE8\u91CA\u524D\u4EC5\u4F5C\u4E13\u5BB6\u5BA1\u8BA1\u7528\u9014\u3002",
      control_type = if (typ == "logical") "checkbox" else if (typ == "choice_vector") "multiselect" else if (typ == "numeric") "number" else if (typ == "integer") "integer" else if (typ == "text") "text" else "none",
      stringsAsFactors = FALSE
    )
  })
  auto <- dplyr::bind_rows(auto_rows)
  out <- dplyr::bind_rows(contract, auto)
  out[order(out$group, out$path), , drop = FALSE]
}

stpd_parameter_schema <- function(scope = c("key", "all"), params = default_params_sec()) {
  scope <- match.arg(scope)
  if (scope == "key") stpd_schema_add_eventness(stpd_key_parameter_schema()) else stpd_parameter_schema_full(params)
}

stpd_schema_coverage_report <- function(params = default_params_sec()) {
  full <- stpd_parameter_schema_full(params)
  data.frame(
    total_parameters = nrow(full),
    key_ui_parameters = sum(full$schema_scope == "key_ui", na.rm = TRUE),
    eventness_parameters = sum(full$schema_scope == "eventness_audit", na.rm = TRUE),
    full_contract_parameters = sum(full$schema_scope == "full_contract", na.rm = TRUE),
    basic_parameters = sum(full$ui_level == "basic", na.rm = TRUE),
    advanced_parameters = sum(full$ui_level == "advanced", na.rm = TRUE),
    expert_parameters = sum(full$ui_level == "expert", na.rm = TRUE),
    auto_full_parameters = sum(full$schema_scope %in% c("full_contract", "extension", "auto_full"), na.rm = TRUE),
    extension_parameters = sum(full$schema_scope == "extension", na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}

# Eventness-audit schema extension. These key parameters expose the
# biological decision layer without changing detector generation semantics.
stpd_schema_add_eventness <- function(schema) {
  if (is.null(schema) || nrow(schema) == 0) schema <- data.frame()
  rows <- stpd_schema_rows_from_config("eventness_schema")
  if (nrow(rows) == 0) return(schema)
  if (ncol(schema) > 0) rows <- rows[, intersect(names(rows), names(schema)), drop = FALSE]
  miss <- setdiff(rows$path, schema$path)
  if (length(miss) > 0) schema <- rbind(schema, rows[rows$path %in% miss, , drop = FALSE])
  schema
}
