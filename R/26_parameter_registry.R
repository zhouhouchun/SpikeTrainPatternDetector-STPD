# Parameter registry and validation layer.
# This module is intended to replace hand-written parameter plumbing gradually.
# It keeps the existing detector behavior but makes parameter origin, defaults,
# and reporting explicit and machine-checkable.

stpd_flatten_params <- function(x, prefix = "") {
  rows <- list()
  walk <- function(obj, pref) {
    if (is.list(obj) && !is.data.frame(obj)) {
      nms <- names(obj)
      if (is.null(nms)) nms <- as.character(seq_along(obj))
      for (ii in seq_along(obj)) {
        nm <- nms[ii]
        val <- obj[[ii]]
        path <- if (nzchar(pref)) paste0(pref, ".", nm) else nm
        if (is.list(val) && !is.data.frame(val) && !grepl("adaptive_train_ranges|train_.*ranges|ranges$", path)) {
          walk(val, path)
        } else {
          rows[[length(rows) + 1L]] <<- list(path = path, value = val)
        }
      }
    } else {
      rows[[length(rows) + 1L]] <<- list(path = pref, value = obj)
    }
  }
  walk(x, prefix)
  if (length(rows) == 0) return(data.frame(path = character(), value_string = character(), value_type = character(), stringsAsFactors = FALSE))
  dplyr::bind_rows(lapply(rows, function(r) {
    v <- r$value
    value_type <- if (is.null(v)) "null" else if (is.logical(v)) "logical" else if (is.integer(v)) "integer" else if (is.numeric(v)) "numeric" else if (is.character(v)) "character" else if (is.list(v)) "list" else class(v)[1]
    value_string <- if (is.null(v)) "<NULL>" else if (is.list(v) && !is.data.frame(v)) paste0("<list:", length(v), ">") else paste(as.character(v), collapse = ",")
    data.frame(path = r$path, value_string = value_string, value_type = value_type, stringsAsFactors = FALSE)
  }))
}

stpd_parameter_registry <- function(defaults = default_params_sec(), key_schema = stpd_parameter_schema(scope = "key")) {
  flat <- stpd_flatten_params(defaults)
  contract <- stpd_parameter_contract()
  if (nrow(contract) == 0) return(key_schema)
  out <- merge(contract, flat, by = "path", all.x = TRUE, sort = FALSE)
  out$value_string[is.na(out$value_string) | !nzchar(out$value_string)] <- out$default[is.na(out$value_string) | !nzchar(out$value_string)]
  out$value_type[is.na(out$value_type) | !nzchar(out$value_type)] <- out$type[is.na(out$value_type) | !nzchar(out$value_type)]
  out$is_key <- out$path %in% key_schema$path
  out$registry_scope <- ifelse(out$schema_scope == "key_ui", "key_ui",
                               ifelse(out$schema_scope == "eventness_audit", "eventness_audit", "full_contract"))
  extra <- flat[!(flat$path %in% contract$path), , drop = FALSE]
  if (nrow(extra) > 0) {
    extra$group <- sub("\\..*$", "", extra$path)
    extra$is_key <- FALSE
    extra$input_id <- gsub("[^A-Za-z0-9_]+", "__", extra$path)
    extra$type <- extra$value_type
    extra$default <- extra$value_string
    extra$label <- extra$path
    extra$choices <- ""
    extra$min <- ""
    extra$max <- ""
    extra$step <- ""
    extra$unit <- ""
    extra$scientific_note <- "Extension parameter not present in YAML contract; retained for compatibility."
    extra$schema_scope <- "extension"
    extra$required <- "FALSE"
    extra$ui_level <- "expert"
    extra$ui_order <- ""
    extra$section <- extra$group
    extra$section_order <- "500"
    extra$advanced <- "TRUE"
    extra$expert_only <- "TRUE"
    extra$visible_if <- ""
    extra$help_text <- "Extension parameter retained for expert compatibility/audit."
    extra$control_type <- stpd_contract_control_type(extra$type)
    extra$registry_scope <- "extension"
    out <- dplyr::bind_rows(out, extra[, names(out), drop = FALSE])
  }
  out[order(suppressWarnings(as.numeric(out$section_order)), suppressWarnings(as.numeric(out$ui_order)), out$group, out$path), , drop = FALSE]
}

stpd_contract_default_allows_missing <- function(rr) {
  if (is.null(rr) || nrow(rr) == 0) return(FALSE)
  typ <- as.character(rr$type[1] %||% "")
  default <- trimws(as.character(rr$default[1] %||% ""))
  minv <- suppressWarnings(as.numeric(rr$min[1]))
  maxv <- suppressWarnings(as.numeric(rr$max[1]))
  typ == "numeric" && identical(toupper(default), "NA") &&
    !is.finite(minv) && !is.finite(maxv)
}

stpd_validate_params <- function(params = default_params_sec(), registry = stpd_parameter_registry(), strict = FALSE) {
  flat <- stpd_flatten_params(params)
  reg_paths <- registry$path
  issues <- list()
  missing_key <- registry$path[registry$registry_scope == "key_ui" & !(registry$path %in% flat$path)]
  if (length(missing_key)) {
    issues[[length(issues) + 1L]] <- data.frame(severity = "warning", path = missing_key, issue = "key parameter missing from params; schema default will be applied", stringsAsFactors = FALSE)
  }
  extra <- flat$path[!(flat$path %in% reg_paths)]
  if (length(extra)) {
    issues[[length(issues) + 1L]] <- data.frame(severity = "info", path = extra, issue = "parameter not present in registry; retained as legacy/extension parameter", stringsAsFactors = FALSE)
  }
  add_issue <- function(severity, path, issue) {
    issues[[length(issues) + 1L]] <<- data.frame(severity = severity, path = path, issue = issue, stringsAsFactors = FALSE)
  }
  split_choices <- function(x) {
    if (is.null(x) || length(x) == 0 || is.na(x[1]) || !nzchar(x[1])) return(character())
    strsplit(as.character(x[1]), "|", fixed = TRUE)[[1]]
  }
  type_ok <- function(value, expected) {
    expected <- as.character(expected %||% "")
    if (!nzchar(expected) || is.null(value)) return(TRUE)
    if (expected == "numeric") return(is.numeric(value))
    if (expected == "integer") return(is.integer(value) || (is.numeric(value) && all(is.na(value) | abs(value - round(value)) < .Machine$double.eps^0.5)))
    if (expected == "logical") return(is.logical(value))
    if (expected %in% c("choice", "choice_vector", "text", "character")) return(is.character(value))
    if (expected == "list") return(is.list(value) && !is.data.frame(value))
    if (expected == "data_frame") return(is.data.frame(value))
    TRUE
  }
  for (ii in seq_len(nrow(flat))) {
    rr <- registry[registry$path == flat$path[ii], , drop = FALSE]
    if (nrow(rr) == 0) next
    value <- stpd_get_param(params, flat$path[ii], NULL)
    expected_type <- as.character(rr$type[1] %||% "")
    if (!type_ok(value, expected_type)) {
      add_issue("error", flat$path[ii], paste0("type mismatch: expected ", expected_type, ", got ", flat$value_type[ii]))
      next
    }
    if (expected_type == "logical") {
      if (length(value) == 0 || any(is.na(value))) {
        add_issue("error", flat$path[ii], "logical parameter has missing or invalid value")
        next
      }
    }
    if (expected_type %in% c("numeric", "integer")) {
      vals_all <- suppressWarnings(as.numeric(value))
      missing_or_invalid <- length(vals_all) == 0 || any(is.na(vals_all))
      if (missing_or_invalid && !stpd_contract_default_allows_missing(rr)) {
        add_issue("error", flat$path[ii], paste0(expected_type, " parameter has missing or invalid value"))
        next
      }
      if (expected_type == "integer") {
        finite_vals <- vals_all[is.finite(vals_all)]
        if (any(!is.finite(vals_all))) {
          add_issue("error", flat$path[ii], "integer parameter must be finite")
          next
        }
        if (length(finite_vals) > 0 && any(abs(finite_vals - round(finite_vals)) >= .Machine$double.eps^0.5)) {
          add_issue("error", flat$path[ii], "integer parameter contains a fractional value")
          next
        }
      }
    }
    choices <- split_choices(rr$choices)
    if (length(choices) > 0 && expected_type %in% c("choice", "choice_vector", "text", "character")) {
      vv <- as.character(value)
      bad <- setdiff(vv[!is.na(vv) & nzchar(vv)], choices)
      if (length(bad) > 0) add_issue("error", flat$path[ii], paste0("value outside choices: ", paste(bad, collapse = ",")))
    }
    if (expected_type %in% c("numeric", "integer")) {
      vals <- suppressWarnings(as.numeric(value))
      vals <- vals[!is.na(vals)]
      minv <- suppressWarnings(as.numeric(rr$min[1]))
      maxv <- suppressWarnings(as.numeric(rr$max[1]))
      if (length(vals) > 0 && is.finite(minv) && any(vals < minv)) {
        add_issue("error", flat$path[ii], paste0("value below contract minimum ", minv))
      }
      if (length(vals) > 0 && is.finite(maxv) && any(vals > maxv)) {
        add_issue("error", flat$path[ii], paste0("value above contract maximum ", maxv))
      }
    }
  }
  out <- if (length(issues)) dplyr::bind_rows(issues) else data.frame(severity = character(), path = character(), issue = character(), stringsAsFactors = FALSE)
  if (strict && any(out$severity == "error")) stop(paste(out$path[out$severity == "error"], out$issue[out$severity == "error"], collapse = "; "), call. = FALSE)
  out
}

stpd_parameter_issue_table <- function(issues, registry = stpd_parameter_registry(), ui_level = "all") {
  if (is.null(issues) || nrow(issues) == 0) {
    return(data.frame(severity = character(), path = character(), issue = character(), ui_level = character(), section = character(), stringsAsFactors = FALSE))
  }
  meta_cols <- intersect(names(registry), c("path", "ui_level", "section", "section_order", "ui_order", "registry_scope"))
  out <- merge(issues, registry[, meta_cols, drop = FALSE], by = "path", all.x = TRUE, sort = FALSE)
  out$ui_level[is.na(out$ui_level) | !nzchar(out$ui_level)] <- "unclassified"
  out$section[is.na(out$section) | !nzchar(out$section)] <- "Unclassified"
  level <- as.character(ui_level %||% "all")[1]
  if (!identical(level, "all")) {
    out <- out[out$severity == "error" | out$ui_level == level, , drop = FALSE]
  }
  out[order(out$severity != "error", suppressWarnings(as.numeric(out$section_order)), suppressWarnings(as.numeric(out$ui_order)), out$path), , drop = FALSE]
}

stpd_parameter_report_flat <- function(params = default_params_sec(), baseline = default_params_sec(), preset = NULL) {
  cur <- stpd_flatten_params(params)
  base <- stpd_flatten_params(baseline)
  names(base)[names(base) == "value_string"] <- "default_value"
  out <- merge(cur[, c("path", "value_string", "value_type")], base[, c("path", "default_value")], by = "path", all = TRUE, sort = FALSE)
  out$current_value <- out$value_string
  out$value_string <- NULL
  out$changed_from_default <- !is.na(out$current_value) & !is.na(out$default_value) & out$current_value != out$default_value
  if (!is.null(preset)) {
    pre <- stpd_flatten_params(preset)
    names(pre)[names(pre) == "value_string"] <- "preset_value"
    out <- merge(out, pre[, c("path", "preset_value")], by = "path", all.x = TRUE, sort = FALSE)
    out$changed_from_preset <- !is.na(out$current_value) & !is.na(out$preset_value) & out$current_value != out$preset_value
  } else {
    out$preset_value <- NA_character_
    out$changed_from_preset <- NA
  }
  reg <- stpd_parameter_registry(baseline)
  reg <- reg[, intersect(names(reg), c("path", "group", "label", "scientific_note", "registry_scope", "ui_level", "section", "section_order", "ui_order", "advanced", "expert_only", "help_text", "control_type")), drop = FALSE]
  out <- merge(out, reg, by = "path", all.x = TRUE, sort = FALSE)
  out[order(suppressWarnings(as.numeric(out$section_order)), suppressWarnings(as.numeric(out$ui_order)), out$group, out$path), , drop = FALSE]
}

stpd_parameter_change_preview <- function(params = default_params_sec(), baseline = default_params_sec(),
                                          include_non_ui = FALSE) {
  report <- parameter_report_table(params, defaults = baseline)
  if (is.null(report) || nrow(report) == 0) {
    return(data.frame(message = "No parameters available for change preview.", stringsAsFactors = FALSE))
  }
  changed_col <- if ("changed_from_default" %in% names(report)) "changed_from_default" else "differs_from_default"
  changed <- report[isTRUE(report[[changed_col]]) | (!is.na(report[[changed_col]]) & report[[changed_col]]), , drop = FALSE]
  if (!isTRUE(include_non_ui) && nrow(changed) > 0) {
    key_paths <- tryCatch(stpd_parameter_schema(scope = "key")$path, error = function(e) character())
    contract_paths <- tryCatch(stpd_contract_ui_schema(ui_level = "all")$path, error = function(e) character())
    ui_paths <- unique(c(as.character(key_paths), as.character(contract_paths)))
    changed <- changed[as.character(changed$path) %in% ui_paths, , drop = FALSE]
  }
  if (nrow(changed) == 0) {
    return(data.frame(message = "No UI-visible parameters differ from defaults.", stringsAsFactors = FALSE))
  }
  for (nm in c("label", "ui_level", "section", "section_order", "ui_order", "help_text", "scientific_note", "value", "default_value", "preset_value")) {
    if (!(nm %in% names(changed))) changed[[nm]] <- ""
  }
  changed$ui_level[is.na(changed$ui_level) | !nzchar(as.character(changed$ui_level))] <- "unclassified"
  changed$section[is.na(changed$section) | !nzchar(as.character(changed$section))] <- "Unclassified"
  changed$label[is.na(changed$label) | !nzchar(as.character(changed$label))] <- changed$path[is.na(changed$label) | !nzchar(as.character(changed$label))]
  impact <- as.character(changed$help_text %||% "")
  blank_impact <- is.na(impact) | !nzchar(impact)
  impact[blank_impact] <- as.character(changed$scientific_note %||% "")[blank_impact]
  impact[is.na(impact) | !nzchar(impact)] <- "Changed from default; review detector output after rerun."
  changed$impact_preview <- impact
  changed$level_order <- match(as.character(changed$ui_level), c("basic", "advanced", "expert", "unclassified"))
  changed$level_order[is.na(changed$level_order)] <- 99L
  changed <- changed[order(
    changed$level_order,
    suppressWarnings(as.numeric(changed$section_order)),
    suppressWarnings(as.numeric(changed$ui_order)),
    changed$path
  ), , drop = FALSE]
  out <- changed[, c("path", "label", "ui_level", "section", "value", "default_value", "impact_preview"), drop = FALSE]
  names(out) <- c("path", "label", "ui_level", "section", "current_value", "default_value", "impact")
  rownames(out) <- NULL
  out
}

stpd_params_hash_flat <- function(params) {
  digest::digest(stpd_flatten_params(params)[, c("path", "value_string")], algo = "sha256")
}
