# Pure identity, status, grouping, and export helpers for auxiliary support runs.

stpd_support_run_schema <- function() "stpd_auxiliary_support_run_v1"

stpd_support_normalize_trains <- function(x) {
  x <- as.character(x %||% character())
  x <- x[!is.na(x) & nzchar(x)]
  sort(unique(x), method = "radix")
}

stpd_support_run_identity <- function(
    ds, dataset_id, selected_trains, support_method, parameters,
    reference_definition = NULL, code_commit = Sys.getenv("STPD_GIT_COMMIT", "")) {
  selected <- stpd_support_normalize_trains(selected_trains)
  if (!length(selected)) stop("A support run requires at least one selected train.", call. = FALSE)
  scope <- stpd_ui_state_scope_hashes(ds, selected)
  if (!isTRUE(scope$valid) || !nzchar(scope$data_sha256 %||% "")) {
    stop("The selected support-run dataset scope is not verifiable.", call. = FALSE)
  }
  method <- trimws(as.character(support_method %||% "")[1])
  if (!nzchar(method)) stop("support_method is required.", call. = FALSE)
  parameter_sha <- stpd_ui_state_sha256(parameters %||% list())
  reference_sha <- stpd_ui_state_sha256(reference_definition %||% list())
  created <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  package_version <- tryCatch(
    as.character(utils::packageVersion("SpikeTrainPatternDetector")),
    error = function(e) "development"
  )
  core <- list(
    schema_version = stpd_support_run_schema(),
    dataset_id = as.character(dataset_id %||% "")[1],
    dataset_data_sha256 = scope$data_sha256,
    selected_trains = selected,
    selected_train_n = length(selected),
    support_method = method,
    parameter_sha256 = parameter_sha,
    reference_definition_sha256 = reference_sha,
    package_version = package_version,
    code_commit = as.character(code_commit %||% "")[1]
  )
  core$run_id <- paste0("support_", substr(stpd_ui_state_sha256(
    list(core = core, created_at_utc = created)
  ), 1L, 24L))
  core$created_at_utc <- created
  structure(core, class = c("stpd_support_run_identity", "list"))
}

stpd_support_run_record <- function(status = "not_run", result = NULL,
                                    identity = NULL, error_code = "",
                                    error_message = "") {
  allowed <- c(
    "not_run", "running", "success", "zero_events", "not_estimable", "error"
  )
  status <- as.character(status %||% "not_run")[1]
  if (!(status %in% allowed)) stop("Unknown support-run status: ", status, call. = FALSE)
  structure(
    list(
      schema_version = stpd_support_run_schema(),
      status = status,
      result = result,
      identity = identity,
      error_code = as.character(error_code %||% "")[1],
      error_message = as.character(error_message %||% "")[1]
    ),
    class = c("stpd_support_run_record", "list")
  )
}

stpd_support_run_state <- function(record, ds, dataset_id, selected_trains = NULL,
                                   parameters = NULL, reference_definition = NULL) {
  if (is.null(record) || !inherits(record, "stpd_support_run_record")) {
    return(list(status = "not_run", current = FALSE, reason = "not_run", result = NULL))
  }
  if (record$status %in% c("not_run", "running", "error")) {
    return(list(
      status = record$status, current = FALSE, reason = record$error_code %||% record$status,
      error_message = record$error_message %||% "", result = NULL,
      identity = record$identity
    ))
  }
  identity <- record$identity
  if (!is.list(identity)) {
    return(list(status = "stale", current = FALSE, reason = "identity_missing", result = NULL))
  }
  current_id <- as.character(dataset_id %||% "")[1]
  if (!identical(as.character(identity$dataset_id %||% "")[1], current_id)) {
    return(list(status = "stale", current = FALSE, reason = "foreign_dataset", result = NULL,
                identity = identity))
  }
  selected <- stpd_support_normalize_trains(identity$selected_trains)
  if (!is.null(selected_trains) && !identical(
      selected, stpd_support_normalize_trains(selected_trains)
  )) {
    return(list(status = "stale", current = FALSE, reason = "train_scope_changed", result = NULL,
                identity = identity))
  }
  if (!is.null(parameters) && !identical(
      stpd_ui_state_sha256(parameters), as.character(identity$parameter_sha256 %||% "")
  )) {
    return(list(status = "stale", current = FALSE, reason = "support_parameters_changed", result = NULL,
                identity = identity))
  }
  if (!is.null(reference_definition) && !identical(
      stpd_ui_state_sha256(reference_definition),
      as.character(identity$reference_definition_sha256 %||% "")
  )) {
    return(list(status = "stale", current = FALSE, reason = "reference_definition_changed", result = NULL,
                identity = identity))
  }
  scope <- stpd_ui_state_scope_hashes(ds, selected)
  if (!isTRUE(scope$valid) || !identical(
      as.character(scope$data_sha256 %||% ""),
      as.character(identity$dataset_data_sha256 %||% "")
  )) {
    return(list(status = "stale", current = FALSE, reason = "spike_data_changed", result = NULL,
                identity = identity))
  }
  list(
    status = record$status, current = TRUE, reason = record$status,
    result = record$result, identity = identity, error_message = ""
  )
}

stpd_support_groupable_columns <- function(ds, trains = names(ds$trains)) {
  trains <- intersect(stpd_support_normalize_trains(trains), names(ds$trains) %||% character())
  if (!length(trains)) return(character())
  excluded <- c(
    "idx", "timestamp", "timestamp_sec", "ISI", "ISI_sec", "pattern_auto",
    "pattern_manual", "pattern_manual_negative", "auto_score"
  )
  common <- Reduce(intersect, lapply(ds$trains[trains], names))
  setdiff(common, excluded)
}

stpd_support_groups_from_metadata <- function(ds, trains, column) {
  trains <- stpd_support_normalize_trains(trains)
  column <- as.character(column %||% "")[1]
  if (!nzchar(column)) stop("Select a metadata column for RGS grouping.", call. = FALSE)
  out <- vapply(trains, function(train) {
    dat <- ds$trains[[train]]
    if (!is.data.frame(dat) || !(column %in% names(dat))) {
      stop("Metadata column '", column, "' is missing for train '", train, "'.", call. = FALSE)
    }
    value <- trimws(as.character(dat[[column]]))
    value <- unique(value[!is.na(value) & nzchar(value)])
    if (length(value) != 1L) {
      stop("Metadata column '", column, "' must contain exactly one group value for train '",
           train, "'.", call. = FALSE)
    }
    value
  }, character(1))
  stats::setNames(out, trains)
}

stpd_support_validate_group_table <- function(table, trains) {
  trains <- stpd_support_normalize_trains(trains)
  if (!is.data.frame(table)) stop("The RGS group map must be a data frame.", call. = FALSE)
  train_col <- intersect(c("Spike_train", "spike_train", "train"), names(table))
  group_col <- intersect(c("Reference_group", "reference_group", "group"), names(table))
  if (!length(train_col) || !length(group_col)) {
    stop("The RGS group map requires train and reference_group columns.", call. = FALSE)
  }
  train <- trimws(as.character(table[[train_col[1L]]]))
  group <- trimws(as.character(table[[group_col[1L]]]))
  if (any(!nzchar(train)) || any(!nzchar(group)) || anyDuplicated(train)) {
    stop("RGS group-map train and group values must be non-empty and train names unique.", call. = FALSE)
  }
  missing <- setdiff(trains, train)
  extra <- setdiff(train, trains)
  if (length(missing) || length(extra)) {
    stop(
      "RGS group-map scope mismatch. Missing: ", paste(missing, collapse = ", "),
      "; extra: ", paste(extra, collapse = ", "), ".", call. = FALSE
    )
  }
  stats::setNames(group[match(trains, train)], trains)
}

stpd_support_write_validated_zip <- function(
    file, support, exporter, prefix, readme_name, readme_lines,
    required_files, sentinel_files = required_files) {
  if (!is.function(exporter)) stop("exporter must be a function.", call. = FALSE)
  out_dir <- tempfile(pattern = paste0(prefix, "_"), tmpdir = tempdir())
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(out_dir, recursive = TRUE, force = TRUE), add = TRUE)
  exporter(support, out_dir)
  writeLines(as.character(readme_lines), file.path(out_dir, readme_name), useBytes = TRUE)
  members <- list.files(out_dir, recursive = TRUE, all.files = FALSE, no.. = TRUE)
  if (!length(members)) stop("The support export produced no files.", call. = FALSE)
  old <- setwd(out_dir)
  on.exit(setwd(old), add = TRUE)
  status <- suppressWarnings(utils::zip(zipfile = file, files = members))
  if (!identical(as.integer(status), 0L) || !file.exists(file)) {
    stop("The support ZIP could not be created.", call. = FALSE)
  }
  checked <- stpd_ui_validate_zip_archive(
    file,
    required_files = unique(c(required_files, readme_name)),
    sentinel_files = unique(c(sentinel_files, readme_name))
  )
  if (!isTRUE(checked$valid)) {
    stop("The support ZIP failed validation: ", checked$code, " - ", checked$reason,
         call. = FALSE)
  }
  invisible(checked)
}
