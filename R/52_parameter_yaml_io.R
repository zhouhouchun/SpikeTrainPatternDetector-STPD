
# Parameter YAML import/export and round-trip helpers.
# This layer turns the parameter contract into a durable user-facing config
# boundary shared by Shiny, validation, reports, and batch/API use.

stpd_param_config_format <- function() "spiketrainpattern-params-1"

stpd_list_deep_merge <- function(base, override) {
  if (is.null(base) || !is.list(base) || is.data.frame(base)) base <- list()
  if (is.null(override) || !is.list(override) || is.data.frame(override)) return(override)
  out <- base
  nms <- names(override)
  if (is.null(nms)) nms <- character(length(override))
  for (ii in seq_along(override)) {
    nm <- nms[ii]
    if (!nzchar(nm)) next
    val <- override[[ii]]
    if (is.list(val) && !is.data.frame(val) && is.list(out[[nm]]) && !is.data.frame(out[[nm]])) {
      out[[nm]] <- stpd_list_deep_merge(out[[nm]], val)
    } else {
      out[[nm]] <- val
    }
  }
  out
}

stpd_contract_coerce_value <- function(value, type) {
  type <- as.character(type %||% "")
  if (!nzchar(type) || is.null(value)) return(value)
  if (type == "numeric") return(suppressWarnings(as.numeric(value)))
  if (type == "integer") {
    num <- suppressWarnings(as.numeric(value))
    ok <- is.finite(num) & abs(num - round(num)) < .Machine$double.eps^0.5
    out <- rep(NA_integer_, length(num))
    out[ok] <- as.integer(round(num[ok]))
    return(out)
  }
  if (type == "logical") {
    if (is.logical(value)) return(value)
    vv <- tolower(trimws(as.character(value)))
    out <- rep(NA, length(vv))
    out[vv %in% c("true", "t", "1", "yes", "y")] <- TRUE
    out[vv %in% c("false", "f", "0", "no", "n")] <- FALSE
    return(out)
  }
  if (type %in% c("choice", "text", "character")) return(as.character(value)[1])
  if (type == "choice_vector") return(as.character(value))
  value
}

stpd_validate_raw_yaml_params <- function(params, schema = stpd_parameter_contract()) {
  if (is.null(params) || !is.list(params) || is.null(schema) || nrow(schema) == 0) {
    return(data.frame(severity = character(), path = character(), issue = character(), stringsAsFactors = FALSE))
  }
  flat <- stpd_flatten_params(params)
  issues <- list()
  add_issue <- function(path, issue) {
    issues[[length(issues) + 1L]] <<- data.frame(severity = "error", path = path, issue = issue, stringsAsFactors = FALSE)
  }
  logical_tokens <- c("true", "t", "1", "yes", "y", "false", "f", "0", "no", "n")
  for (ii in seq_len(nrow(flat))) {
    rr <- schema[schema$path == flat$path[ii], , drop = FALSE]
    if (nrow(rr) == 0) next
    typ <- as.character(rr$type[1] %||% "")
    if (!(typ %in% c("numeric", "integer", "logical"))) next
    value <- stpd_get_param(params, flat$path[ii], NULL)
    raw_missing <- is.null(value) || length(value) == 0 || all(is.na(value))
    if (typ %in% c("numeric", "integer")) {
      num <- suppressWarnings(as.numeric(value))
      invalid <- length(num) == 0 || any(is.na(num))
      if (typ == "integer" && !invalid) {
        invalid <- any(!is.finite(num)) ||
          any(abs(num - round(num)) >= .Machine$double.eps^0.5)
      }
      if (invalid && !(raw_missing && stpd_contract_default_allows_missing(rr))) {
        add_issue(flat$path[ii], paste0("invalid YAML ", typ, " value"))
      }
    } else if (typ == "logical") {
      if (is.logical(value)) {
        invalid <- length(value) == 0 || any(is.na(value))
      } else {
        vv <- tolower(trimws(as.character(value)))
        invalid <- length(vv) == 0 || any(is.na(vv) | !(vv %in% logical_tokens))
      }
      if (invalid) add_issue(flat$path[ii], "invalid YAML logical value")
    }
  }
  if (length(issues)) dplyr::bind_rows(issues) else data.frame(severity = character(), path = character(), issue = character(), stringsAsFactors = FALSE)
}

stpd_coerce_params_to_contract <- function(params, schema = stpd_parameter_contract()) {
  out <- params
  if (is.null(schema) || nrow(schema) == 0) return(out)
  for (ii in seq_len(nrow(schema))) {
    path <- as.character(schema$path[ii])
    if (!nzchar(path)) next
    cur <- stpd_get_param(out, path, NULL)
    if (is.null(cur)) next
    out <- stpd_set_param(out, path, stpd_contract_coerce_value(cur, schema$type[ii]))
  }
  out
}

stpd_prepare_params_for_yaml <- function(params, baseline = default_params_sec(), coerce = TRUE, productize = TRUE) {
  p <- stpd_list_deep_merge(baseline, params %||% list())
  if (isTRUE(coerce)) p <- stpd_coerce_params_to_contract(p)
  if (isTRUE(productize) && exists("stpd_productize_params", mode = "function")) {
    p <- tryCatch(stpd_productize_params(p, prefer = "legacy"), error = function(e) p)
  }
  p
}

stpd_parameter_validation_summary <- function(params, issues = NULL) {
  if (is.null(issues)) issues <- stpd_validate_params(params)
  counts <- table(factor(issues$severity, levels = c("error", "warning", "info")))
  data.frame(
    item = c("params_hash", "errors", "warnings", "info"),
    value = c(
      tryCatch(stpd_params_hash(params), error = function(e) stpd_params_hash_flat(params)),
      as.character(counts[["error"]]),
      as.character(counts[["warning"]]),
      as.character(counts[["info"]])
    ),
    stringsAsFactors = FALSE
  )
}

stpd_parameter_yaml_bundle <- function(params,
                                       source = "api",
                                       baseline = default_params_sec(),
                                       include_validation = TRUE) {
  p <- stpd_prepare_params_for_yaml(params, baseline = baseline, coerce = TRUE)
  issues <- stpd_validate_params(p)
  cfg <- tryCatch(stpd_parameter_config(), error = function(e) list(schema_version = NA_character_))
  bundle <- list(
    format = stpd_param_config_format(),
    schema_version = as.character(cfg$schema_version %||% NA_character_),
    package_version = tryCatch(as.character(utils::packageVersion("SpikeTrainPatternDetector")), error = function(e) NA_character_),
    exported_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    source = as.character(source %||% "api"),
    params_hash = tryCatch(stpd_params_hash(p), error = function(e) stpd_params_hash_flat(p)),
    parameters = p
  )
  if (isTRUE(include_validation)) {
    bundle$validation_summary <- stpd_parameter_validation_summary(p, issues = issues)
  }
  bundle
}

stpd_write_params_yaml <- function(params, file, source = "api", baseline = default_params_sec()) {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Package 'yaml' is required to write SpikeTrainPatternDetector parameter YAML.", call. = FALSE)
  }
  bundle <- stpd_parameter_yaml_bundle(params, source = source, baseline = baseline, include_validation = TRUE)
  yaml::write_yaml(bundle, file)
  invisible(bundle)
}

stpd_extract_params_from_yaml_object <- function(obj) {
  if (!is.list(obj)) stop("Parameter YAML must contain a mapping/list object.", call. = FALSE)
  fmt <- as.character(obj$format %||% obj$config_format %||% "")
  fmt <- fmt[1] %||% ""
  if (is.na(fmt)) fmt <- ""
  if (nzchar(fmt) && !identical(fmt, stpd_param_config_format())) {
    stop("Unsupported parameter YAML format: ", fmt, call. = FALSE)
  }
  params <- obj$parameters %||% obj$params
  if (is.null(params)) {
    known <- unique(c(stpd_parameter_contract()$group, "spiketrainpattern", "metadata"))
    has_param_groups <- any(names(obj) %in% known)
    if (isTRUE(has_param_groups)) params <- obj
  }
  if (is.null(params) || !is.list(params)) {
    stop("Parameter YAML does not contain a 'parameters' mapping.", call. = FALSE)
  }
  params
}

stpd_read_params_yaml <- function(file, baseline = default_params_sec(), strict = FALSE) {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Package 'yaml' is required to read SpikeTrainPatternDetector parameter YAML.", call. = FALSE)
  }
  obj <- yaml::read_yaml(file, eval.expr = FALSE)
  imported <- stpd_extract_params_from_yaml_object(obj)
  input_issues <- stpd_validate_raw_yaml_params(imported)
  raw <- stpd_prepare_params_for_yaml(imported, baseline = baseline, coerce = TRUE, productize = FALSE)
  raw_issues <- stpd_validate_params(raw)
  p <- stpd_prepare_params_for_yaml(imported, baseline = baseline, coerce = TRUE, productize = TRUE)
  issues <- unique(rbind(input_issues, raw_issues, stpd_validate_params(p)))
  if (isTRUE(strict) && any(issues$severity == "error")) {
    bad <- issues[issues$severity == "error", , drop = FALSE]
    stop(paste(bad$path, bad$issue, collapse = "; "), call. = FALSE)
  }
  list(
    params = p,
    validation = issues,
    summary = stpd_parameter_validation_summary(p, issues = issues),
    params_hash = tryCatch(stpd_params_hash(p), error = function(e) stpd_params_hash_flat(p)),
    metadata = obj[setdiff(names(obj), c("parameters", "params"))]
  )
}

stpd_parameter_yaml_roundtrip_report <- function(params, source = "roundtrip_probe") {
  tmp <- tempfile(fileext = ".yml")
  on.exit(unlink(tmp), add = TRUE)
  bundle <- stpd_write_params_yaml(params, tmp, source = source)
  imported <- stpd_read_params_yaml(tmp)
  original_hash <- as.character(bundle$params_hash %||% "")
  imported_hash <- as.character(imported$params_hash %||% "")
  issues <- imported$validation
  counts <- table(factor(issues$severity, levels = c("error", "warning", "info")))
  data.frame(
    check = c("write_read_yaml", "hash_preserved", "validation_errors", "validation_warnings", "validation_info"),
    status = c(
      "ok",
      if (identical(original_hash, imported_hash)) "ok" else "mismatch",
      as.character(counts[["error"]]),
      as.character(counts[["warning"]]),
      as.character(counts[["info"]])
    ),
    detail = c(
      basename(tmp),
      paste0(original_hash, " -> ", imported_hash),
      "errors after YAML reload",
      "warnings after YAML reload",
      "informational issues after YAML reload"
    ),
    stringsAsFactors = FALSE
  )
}
