# Formal-export integrity helpers.
#
# These helpers are deliberately independent of Shiny.  They provide a fixed
# detector-output fingerprint, explicit validation-artifact disposition,
# fail-closed per-dataset batch planning, collision-resistant paths, and ZIP
# verification.  They do not mutate scientific datasets or detector results.

stpd_ui_export_integrity_schema_version <- function() {
  "stpd_ui_export_integrity_v1"
}

stpd_ui_detector_output_schema_version <- function() {
  "stpd_ui_detector_output_v2"
}

stpd_ui_detector_output_artifact_names <- function() {
  c(
    "train_auto_output", "legacy_final_audit_train_output",
    "legacy_override_train_output",
    "events", "events_high_confidence", "events_review_candidates",
    "events_burst_family", "events_all_family_map",
    "candidate_ledger", "event_ledger", "event_audit",
    "candidate_features", "candidate_features_internal",
    "final_decisions", "final_decisions_internal",
    "final_classification_audit", "eventness_audit",
    "structure_candidates", "seed_candidates", "bridge_candidates",
    "burst_candidates", "burst_candidates_raw", "burst_candidates_final",
    "pause_candidates", "near_miss_candidates", "posthoc_fragment_audit",
    "candidate_diagnostic_audit", "threshold_table",
    "event_distribution_evidence", "train_distribution_features",
    "spike_count_pmf", "distributional_evidence_note",
    "consistency_audit", "semantic_consistency_report", "result_consistency",
    "validation_guidance", "governance_summary", "parameters_report",
    "parameter_report", "parameter_validation", "stationarity_qc",
    "overfit_warning_report", "scientific_validation_summary",
    "event_level_validation_strict",
    "event_level_validation_candidate_family",
    "final_audit_summary", "final_audit_events",
    "final_audit_history", "final_audit_event_history",
    "possible_burst_promotion_audit",
    "possible_burst_promotion_summary",
    "run_metadata", "run_metadata_public", "detection_mode", "label_blind",
    "multitrack_preview", "multitrack_auto", "multitrack_final",
    "multitrack_gate_b", "data_quality"
  )
}

stpd_ui_detector_output_required_groups <- function() {
  list(
    train_auto_output = "train_auto_output",
    events = "events",
    candidate_ledger = "candidate_ledger",
    event_ledger = c("event_ledger", "event_audit"),
    candidate_features = c("candidate_features", "candidate_features_internal"),
    final_decisions = c(
      "final_decisions", "final_decisions_internal",
      "final_classification_audit"
    ),
    run_metadata = c("run_metadata_public", "run_metadata")
  )
}

stpd_ui_export_integrity_supported_value <- function(x) {
  if (is.null(x) || is.atomic(x) || is.factor(x) || inherits(x, "POSIXt") ||
      inherits(x, "Date")) return(TRUE)
  if (is.data.frame(x) || is.matrix(x) || is.array(x)) {
    return(all(vapply(unclass(x), stpd_ui_export_integrity_supported_value,
                      logical(1))))
  }
  if (is.list(x)) {
    return(all(vapply(x, stpd_ui_export_integrity_supported_value, logical(1))))
  }
  FALSE
}

stpd_ui_export_integrity_normalize_atomic <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  if (inherits(x, "POSIXt")) x <- format(x, tz = "UTC", usetz = TRUE)
  if (inherits(x, "Date")) x <- format(x, "%Y-%m-%d")
  value_names <- names(x)
  attributes(x) <- NULL
  if (!is.null(value_names) && length(value_names) == length(x) &&
      !anyNA(value_names) && !anyDuplicated(value_names)) {
    ord <- order(value_names, method = "radix")
    x <- x[ord]
    names(x) <- value_names[ord]
  }
  x
}

stpd_ui_export_integrity_normalize <- function(x) {
  if (is.null(x)) return(list(type = "null", value = NULL))
  if (is.data.frame(x)) {
    table <- stpd_ui_state_normalize_table(x, sort_rows = TRUE)
    columns <- lapply(table, stpd_ui_export_integrity_normalize)
    names(columns) <- names(table)
    return(list(
      type = "data.frame", nrow = as.integer(nrow(table)),
      columns = columns
    ))
  }
  if (is.matrix(x) || is.array(x)) {
    return(list(
      type = if (is.matrix(x)) "matrix" else "array",
      dim = as.integer(dim(x)),
      dimnames = stpd_ui_export_integrity_normalize(dimnames(x)),
      values = stpd_ui_export_integrity_normalize_atomic(as.vector(x))
    ))
  }
  if (is.atomic(x) || is.factor(x) || inherits(x, "POSIXt") ||
      inherits(x, "Date")) {
    return(list(
      type = paste(class(x), collapse = "/"),
      value = stpd_ui_export_integrity_normalize_atomic(x)
    ))
  }
  if (is.list(x)) {
    value_names <- names(x)
    if (!is.null(value_names) && length(value_names) == length(x) &&
        !anyNA(value_names) && !anyDuplicated(value_names)) {
      ord <- order(value_names, method = "radix")
      x <- x[ord]
      value_names <- value_names[ord]
    }
    values <- lapply(x, stpd_ui_export_integrity_normalize)
    if (!is.null(value_names)) names(values) <- value_names
    return(list(type = "list", values = values))
  }
  stop("Unsupported value in detector-output identity.", call. = FALSE)
}

stpd_ui_detector_output_scope <- function(ds, selected_trains = NULL) {
  all_trains <- stpd_ui_state_train_names(ds)
  supplied <- !is.null(selected_trains)
  selected <- stpd_ui_state_parse_trains(selected_trains)
  if (!supplied) {
    metadata <- stpd_ui_state_run_metadata(ds)
    selected <- stpd_ui_state_parse_trains(
      stpd_ui_state_metadata_value(metadata, "selected_trains")
    )
    if (length(selected) == 0L) selected <- all_trains
  }
  reasons <- character()
  if (length(selected) == 0L) reasons <- c(reasons, "empty_scope")
  if (length(all_trains) == 0L || !is.list(ds) || !is.list(ds$trains)) {
    reasons <- c(reasons, "dataset_trains_invalid")
  }
  if (length(setdiff(selected, all_trains)) > 0L) {
    reasons <- c(reasons, "scope_contains_unknown_train")
  }
  list(
    valid = length(reasons) == 0L,
    selected_trains = selected,
    all_trains = all_trains,
    reason_codes = unique(reasons)
  )
}

stpd_ui_detector_output_artifact_value <- function(ds, artifact,
                                                    selected_trains) {
  if (identical(artifact, "train_auto_output")) {
    return(stpd_ui_state_auto_payload(ds, selected_trains))
  }
  if (artifact %in% c(
      "legacy_final_audit_train_output", "legacy_override_train_output"
    )) {
    review_columns <- if (identical(
        artifact, "legacy_final_audit_train_output"
      )) {
      c(
        "pattern_audit_final", "pattern_audit_base_final",
        "pattern_audit_from", "pattern_audit_to", "pattern_audit_action",
        "pattern_audit_source", "pattern_audit_reason", "pattern_audit_id",
        "pattern_audit_time"
      )
    } else {
      c(
        "pattern_auto_original", "pattern_user_override",
        "pattern_user_override_from", "pattern_user_override_to",
        "pattern_user_override_reason", "pattern_user_override_source",
        "pattern_user_override_time", "pattern_user_override_id"
      )
    }
    payload <- lapply(selected_trains, function(train) {
      dat <- ds$trains[[train]]
      if (!is.data.frame(dat)) {
        return(list(train = train, valid = FALSE, n = NA_integer_))
      }
      n <- nrow(dat)
      values <- lapply(review_columns, function(column) {
        value <- if (column %in% names(dat)) dat[[column]] else rep("", n)
        value <- as.character(value)
        value[is.na(value)] <- ""
        unname(value)
      })
      names(values) <- review_columns
      list(train = train, valid = TRUE, n = as.integer(n), values = values)
    })
    names(payload) <- selected_trains
    return(payload)
  }
  if (identical(artifact, "data_quality")) {
    return(if (is.list(ds)) ds[["quality"]] %||% NULL else NULL)
  }
  results <- if (is.list(ds) && is.list(ds$results)) ds$results else list()
  results[[artifact]] %||% NULL
}

stpd_ui_detector_output_identity <- function(ds, selected_trains = NULL) {
  schema_version <- stpd_ui_detector_output_schema_version()
  artifact_names <- stpd_ui_detector_output_artifact_names()
  scope <- stpd_ui_detector_output_scope(ds, selected_trains)
  results <- if (is.list(ds) && is.list(ds$results)) ds$results else list()

  rows <- lapply(artifact_names, function(artifact) {
    value <- if (isTRUE(scope$valid) || identical(artifact, "train_auto_output")) {
      tryCatch(
        stpd_ui_detector_output_artifact_value(
          ds, artifact, scope$selected_trains
        ),
        error = function(e) structure(list(message = conditionMessage(e)),
                                     class = "stpd_identity_error")
      )
    } else {
      NULL
    }
    present <- if (artifact %in% c(
        "train_auto_output", "legacy_final_audit_train_output",
        "legacy_override_train_output"
      )) {
      is.list(ds) && is.list(ds$trains) && length(scope$selected_trains) > 0L &&
        all(scope$selected_trains %in% names(ds$trains))
    } else if (identical(artifact, "data_quality")) {
      is.list(ds) && !is.null(ds[["quality"]])
    } else {
      artifact %in% names(results) && !is.null(results[[artifact]])
    }
    supported <- present &&
      !inherits(value, "stpd_identity_error") &&
      stpd_ui_export_integrity_supported_value(value)
    normalized <- if (supported) {
      tryCatch(
        stpd_ui_export_integrity_normalize(value),
        error = function(e) NULL
      )
    } else {
      NULL
    }
    sha <- if (!is.null(normalized)) {
      stpd_ui_state_valid_sha256(stpd_ui_state_sha256(normalized))
    } else {
      ""
    }
    data.frame(
      artifact = artifact,
      present = isTRUE(present),
      supported = isTRUE(supported) && nzchar(sha),
      object_type = if (present && !inherits(value, "stpd_identity_error")) {
        paste(class(value), collapse = "/")
      } else {
        ""
      },
      nrow = if (is.data.frame(value)) as.integer(nrow(value)) else NA_integer_,
      ncol = if (is.data.frame(value)) as.integer(ncol(value)) else NA_integer_,
      sha256 = sha,
      stringsAsFactors = FALSE
    )
  })
  artifacts <- do.call(rbind, rows)
  rownames(artifacts) <- NULL

  required_groups <- stpd_ui_detector_output_required_groups()
  present_names <- artifacts$artifact[artifacts$present & artifacts$supported]
  missing_required <- names(required_groups)[!vapply(
    required_groups,
    function(alternatives) any(alternatives %in% present_names),
    logical(1)
  )]
  unsupported <- artifacts$artifact[artifacts$present & !artifacts$supported]
  combined_payload <- list(
    schema_version = schema_version,
    selected_trains = scope$selected_trains,
    artifacts = lapply(seq_len(nrow(artifacts)), function(i) {
      list(
        artifact = artifacts$artifact[[i]],
        present = artifacts$present[[i]],
        sha256 = artifacts$sha256[[i]]
      )
    })
  )
  combined_sha <- stpd_ui_state_valid_sha256(
    stpd_ui_state_sha256(combined_payload)
  )
  reasons <- unique(c(
    scope$reason_codes,
    if (length(missing_required) > 0L) "required_artifact_missing",
    if (length(unsupported) > 0L) "artifact_unhashable",
    if (!nzchar(combined_sha)) "combined_hash_unavailable"
  ))
  structure(
    list(
      schema_version = schema_version,
      selected_train_n = as.integer(length(scope$selected_trains)),
      selected_trains = scope$selected_trains,
      artifact_names = artifact_names,
      artifacts = artifacts,
      combined_sha256 = combined_sha,
      missing_required = as.character(missing_required),
      unsupported_artifacts = as.character(unsupported),
      reason_codes = reasons,
      verifiable = isTRUE(scope$valid) && length(missing_required) == 0L &&
        length(unsupported) == 0L && nzchar(combined_sha)
    ),
    class = c("stpd_ui_detector_output_identity", "list")
  )
}

stpd_ui_detector_output_state <- function(ds, expected_identity,
                                          selected_trains = NULL) {
  actual <- NULL
  state <- function(current, code, detail, expected = expected_identity,
                    changed_artifacts = character(), reason_codes = code) {
    structure(
      list(
        current = isTRUE(current),
        code = as.character(code)[1],
        detail = as.character(detail)[1],
        reason_codes = unique(as.character(reason_codes)),
        expected_sha256 = stpd_ui_state_chr(expected$combined_sha256),
        current_sha256 = stpd_ui_state_chr((actual %||% list())$combined_sha256),
        changed_artifacts = sort(unique(as.character(changed_artifacts)),
                                 method = "radix"),
        expected_identity = expected,
        current_identity = actual
      ),
      class = c("stpd_ui_detector_output_state", "list")
    )
  }

  if (!is.list(expected_identity) ||
      !identical(expected_identity$schema_version,
                 stpd_ui_detector_output_schema_version())) {
    return(state(
      FALSE, "detector_output_expected_identity_invalid",
      "The stored detector-output identity is missing or uses an unsupported schema.",
      reason_codes = "expected_identity_invalid"
    ))
  }
  expected_scope <- stpd_ui_state_parse_trains(expected_identity$selected_trains)
  all_trains <- stpd_ui_state_train_names(ds)
  if (length(expected_scope) == 0L ||
      length(setdiff(expected_scope, all_trains)) > 0L) {
    return(state(
      FALSE, "detector_output_scope_invalid",
      "The stored detector-output scope is empty or contains an unknown train.",
      reason_codes = "scope_invalid"
    ))
  }
  requested_scope <- if (is.null(selected_trains)) {
    expected_scope
  } else {
    stpd_ui_state_parse_trains(selected_trains)
  }
  if (!identical(requested_scope, expected_scope)) {
    return(state(
      FALSE, "detector_output_scope_mismatch",
      "The requested export scope differs from the stored detector-output scope.",
      reason_codes = "scope_mismatch"
    ))
  }

  actual <- stpd_ui_detector_output_identity(
    ds, selected_trains = expected_scope
  )
  if (!isTRUE(expected_identity$verifiable) || !isTRUE(actual$verifiable)) {
    reasons <- unique(c(
      "identity_unverifiable",
      expected_identity$reason_codes %||% character(),
      actual$reason_codes %||% character()
    ))
    return(state(
      FALSE, "detector_output_identity_unverifiable",
      "The detector-output identity is incomplete or contains an unhashable artifact.",
      reason_codes = reasons
    ))
  }

  expected_artifacts <- expected_identity$artifacts
  current_artifacts <- actual$artifacts
  changed <- character()
  if (is.data.frame(expected_artifacts) && is.data.frame(current_artifacts) &&
      all(c("artifact", "present", "sha256") %in% names(expected_artifacts)) &&
      all(c("artifact", "present", "sha256") %in% names(current_artifacts))) {
    expected_map <- setNames(
      paste(expected_artifacts$present, expected_artifacts$sha256, sep = ":"),
      expected_artifacts$artifact
    )
    current_map <- setNames(
      paste(current_artifacts$present, current_artifacts$sha256, sep = ":"),
      current_artifacts$artifact
    )
    artifact_union <- union(names(expected_map), names(current_map))
    changed <- artifact_union[vapply(artifact_union, function(name) {
      !identical(unname(expected_map[[name]] %||% ""),
                 unname(current_map[[name]] %||% ""))
    }, logical(1))]
  }
  if (!identical(
      stpd_ui_state_chr(expected_identity$combined_sha256),
      stpd_ui_state_chr(actual$combined_sha256))) {
    return(state(
      FALSE, "detector_output_changed",
      "One or more detector result artifacts changed after the stored run snapshot.",
      changed_artifacts = changed,
      reason_codes = c("combined_sha256_changed", paste0("artifact:", changed))
    ))
  }
  state(
    TRUE, "detector_output_current",
    "All fixed-schema detector result artifacts match the stored run snapshot.",
    reason_codes = character()
  )
}

stpd_ui_validation_export_decision <- function(
    ds, validation, params = NULL, dataset_id = NULL,
    run_identity = NULL, validation_identity = NULL,
    current_truth_sha256 = NULL, dataset_identity = NULL,
    stale_policy = c("omit", "block"),
    artifact_name = "scientific_validation") {
  stale_policy <- match.arg(stale_policy)
  artifact_name <- stpd_ui_state_chr(artifact_name, "scientific_validation")
  make <- function(action, code, reason, current = FALSE, state = NULL) {
    structure(
      list(
        artifact_name = artifact_name,
        action = action,
        include = identical(action, "include"),
        current = isTRUE(current),
        code = code,
        reason = reason,
        reason_codes = unique(as.character(
          (state %||% list())$reason_codes %||% code
        )),
        state = state
      ),
      class = c("stpd_ui_validation_export_decision", "list")
    )
  }
  if (!is.list(validation) || length(validation) == 0L) {
    return(make(
      "omit", "validation_export_absent",
      paste0(artifact_name, " is absent; no validation artifact will be exported.")
    ))
  }
  dataset_identity <- dataset_identity %||%
    stpd_ui_dataset_identity(ds, dataset_id)
  run_identity <- run_identity %||% stpd_ui_run_identity(
    ds, params = params, dataset_id = dataset_id,
    dataset_identity = dataset_identity
  )
  validation_identity <- validation_identity %||%
    validation[["ui_identity"]] %||%
    list(has_validation = TRUE, verifiable = FALSE)
  expected_report_sha <- stpd_ui_state_valid_sha256(
    validation_identity$report_sha256
  )
  current_report_sha <- stpd_ui_validation_report_sha256(validation)
  report_hash_current <- nzchar(expected_report_sha) &&
    nzchar(current_report_sha) &&
    identical(expected_report_sha, current_report_sha)
  if (!report_hash_current) {
    action <- if (identical(stale_policy, "block")) "block" else "omit"
    code <- if (!nzchar(expected_report_sha) || !nzchar(current_report_sha)) {
      "validation_export_report_identity_unverifiable"
    } else {
      "validation_export_report_changed"
    }
    return(make(
      action, code,
      paste0(
        artifact_name,
        " content does not match its captured report hash; it will ",
        if (identical(action, "omit")) "be omitted from this export."
        else "block this export."
      )
    ))
  }
  validation_state <- tryCatch(
    stpd_ui_validation_state(
      ds, params = params, dataset_id = dataset_id,
      run_identity = run_identity,
      validation_identity = validation_identity,
      current_truth_sha256 = current_truth_sha256,
      dataset_identity = dataset_identity
    ),
    error = function(e) stpd_ui_status_record(
      "validation_identity_unverifiable", "warning",
      "Validation identity could not be evaluated", conditionMessage(e),
      reason_codes = "identity_unverifiable", verifiable = FALSE
    )
  )
  current_codes <- c("validation_current", "validation_partial_current")
  if (validation_state$code %in% current_codes &&
      isTRUE(validation_state$verifiable)) {
    return(make(
      "include", "validation_export_current",
      paste0(
        artifact_name, " is current (", validation_state$code,
        ") and will be included."
      ),
      current = TRUE, state = validation_state
    ))
  }
  action <- if (identical(stale_policy, "block")) "block" else "omit"
  code <- paste0(
    "validation_export_", if (identical(action, "block")) "blocked_" else "omitted_",
    stpd_ui_state_chr(validation_state$code, "identity_unverifiable")
  )
  make(
    action, code,
    paste0(
      artifact_name, " is not current (",
      stpd_ui_state_chr(validation_state$code, "identity_unverifiable"),
      "): ", stpd_ui_state_chr(validation_state$detail,
                                "identity could not be verified"),
      if (identical(action, "omit"))
        "; it will be omitted from this export." else
        "; the export is blocked."
    ),
    current = FALSE, state = validation_state
  )
}

stpd_ui_export_named_value <- function(values, id) {
  if (!is.list(values)) return(NULL)
  value_names <- names(values)
  if (is.null(value_names) || anyNA(value_names) || anyDuplicated(value_names) ||
      !(id %in% value_names)) return(NULL)
  values[[id]]
}

stpd_ui_export_safe_directory_names <- function(dataset_ids) {
  ids <- as.character(dataset_ids)
  ids[is.na(ids)] <- ""
  used <- character()
  output <- character(length(ids))
  for (i in seq_along(ids)) {
    id <- ids[[i]]
    transliterated <- suppressWarnings(iconv(id, from = "", to = "ASCII//TRANSLIT"))
    if (is.na(transliterated)) transliterated <- id
    base <- gsub("[^A-Za-z0-9._-]+", "_", transliterated)
    base <- gsub("^[._-]+|[._-]+$", "", base)
    if (!nzchar(base) || base %in% c(".", "..")) base <- "dataset"
    base <- substr(base, 1L, 48L)
    suffix <- stpd_ui_state_valid_sha256(stpd_ui_state_sha256(id))
    if (!nzchar(suffix)) suffix <- sprintf("%064d", i)
    candidate <- paste0(base, "__", suffix)
    ordinal <- 1L
    while (candidate %in% used) {
      ordinal <- ordinal + 1L
      candidate <- paste0(base, "__", suffix, "__", ordinal)
    }
    output[[i]] <- candidate
    used <- c(used, candidate)
  }
  names(output) <- ids
  output
}

stpd_ui_export_temp_path <- function(
    prefix = "stpd_export", tmpdir = tempdir(), extension = "",
    create = c("none", "file", "directory"), max_attempts = 100L) {
  create <- match.arg(create)
  if (length(tmpdir) != 1L || is.na(tmpdir) || !dir.exists(tmpdir)) {
    stop("tmpdir must be one existing directory.", call. = FALSE)
  }
  tmpdir <- normalizePath(tmpdir, winslash = "/", mustWork = TRUE)
  prefix <- stpd_ui_state_chr(prefix, "stpd_export")
  prefix <- gsub("[^A-Za-z0-9._-]+", "_", prefix)
  prefix <- gsub("^[._-]+", "", prefix)
  if (!nzchar(prefix)) prefix <- "stpd_export"
  extension <- stpd_ui_state_chr(extension)
  if (nzchar(extension) && !startsWith(extension, ".")) {
    extension <- paste0(".", extension)
  }
  if (grepl("[/\\\\]", extension) || grepl("\\.\\.", extension)) {
    stop("extension must be a simple filename extension.", call. = FALSE)
  }
  max_attempts <- suppressWarnings(as.integer(max_attempts)[1])
  if (is.na(max_attempts) || max_attempts < 1L) max_attempts <- 100L
  for (attempt in seq_len(max_attempts)) {
    path <- tempfile(
      pattern = paste0(substr(prefix, 1L, 48L), "_", Sys.getpid(), "_"),
      tmpdir = tmpdir, fileext = extension
    )
    if (file.exists(path) || dir.exists(path)) next
    created <- switch(
      create,
      none = TRUE,
      file = isTRUE(file.create(path, showWarnings = FALSE)),
      directory = isTRUE(dir.create(
        path, recursive = FALSE, showWarnings = FALSE, mode = "0700"
      ))
    )
    if (created) return(normalizePath(path, winslash = "/", mustWork = FALSE))
  }
  stop("Could not allocate a unique temporary export path.", call. = FALSE)
}

stpd_ui_batch_export_plan <- function(
    datasets, params_by_dataset = NULL, run_identities = NULL,
    expected_detector_outputs = NULL, dataset_ids = names(datasets),
    output_root = NULL) {
  empty_failures <- data.frame(
    dataset_id = character(), code = character(), detail = character(),
    stringsAsFactors = FALSE
  )
  invalid_plan <- function(code, detail) {
    structure(
      list(
        eligible = FALSE, code = code, detail = detail,
        entries = list(), eligible_entries = list(),
        failures = data.frame(
          dataset_id = "", code = code, detail = detail,
          stringsAsFactors = FALSE
        ),
        output_root = stpd_ui_state_chr(output_root)
      ),
      class = c("stpd_ui_batch_export_plan", "list")
    )
  }
  if (!is.list(datasets) || length(datasets) == 0L) {
    return(invalid_plan("batch_export_no_datasets", "No datasets are available for batch export."))
  }
  ids <- as.character(dataset_ids %||% character())
  if (length(ids) != length(datasets) || anyNA(ids) || any(!nzchar(ids)) ||
      anyDuplicated(ids)) {
    return(invalid_plan(
      "batch_export_dataset_ids_invalid",
      "Batch export requires one non-empty, unique dataset id per dataset."
    ))
  }
  names(datasets) <- ids
  safe_names <- stpd_ui_export_safe_directory_names(ids)
  root <- stpd_ui_state_chr(output_root)
  if (nzchar(root)) root <- normalizePath(root, winslash = "/", mustWork = FALSE)

  entries <- vector("list", length(ids))
  names(entries) <- ids
  failure_rows <- list()
  for (i in seq_along(ids)) {
    id <- ids[[i]]
    ds <- datasets[[id]]
    params <- stpd_ui_export_named_value(params_by_dataset, id)
    if (is.null(params) && is.list(ds)) params <- ds$params_effective %||% NULL
    effective_params_sha256 <- if (is.list(ds) &&
        is.list(ds$params_effective)) {
      stpd_ui_state_params_sha256(ds$params_effective)
    } else {
      ""
    }
    run_identity <- stpd_ui_export_named_value(run_identities, id)
    expected_output <- stpd_ui_export_named_value(
      expected_detector_outputs, id
    )
    directory_name <- unname(safe_names[[i]])
    output_dir <- if (nzchar(root)) file.path(root, directory_name) else directory_name
    entry <- list(
      dataset_id = id,
      dataset_name = stpd_ui_state_chr((ds$meta %||% list())$display_name, id),
      eligible = FALSE,
      code = "batch_export_unchecked",
      detail = "",
      params = params,
      params_sha256 = stpd_ui_state_params_sha256(params),
      effective_params_sha256 = effective_params_sha256,
      run_identity = run_identity,
      batch_gate_run_identity = NULL,
      formal_export_state = NULL,
      expected_detector_output = expected_output,
      detector_output_state = NULL,
      directory_name = directory_name,
      output_dir = output_dir
    )
    if (!is.list(ds) || !is.list(ds$trains)) {
      entry$code <- "batch_export_dataset_invalid"
      entry$detail <- "The dataset is missing a valid trains list."
    } else if (!is.list(params) || !nzchar(entry$params_sha256)) {
      entry$code <- "batch_export_params_missing"
      entry$detail <- "No valid per-dataset effective parameter snapshot is available."
    } else if (!nzchar(entry$effective_params_sha256) ||
        !identical(entry$params_sha256, entry$effective_params_sha256)) {
      entry$code <- "batch_export_params_effective_mismatch"
      entry$detail <- paste0(
        "The supplied dataset parameters do not match the frozen ",
        "params_effective snapshot."
      )
    } else if (!is.list(run_identity)) {
      entry$code <- "batch_export_run_identity_missing"
      entry$detail <- "No captured run identity is available for this dataset."
    } else if (!is.list(expected_output)) {
      entry$code <- "batch_export_detector_output_identity_missing"
      entry$detail <- "No captured detector-output identity is available for this dataset."
    } else {
      # Batch exports are built from each dataset's frozen effective params,
      # not from whichever dataset happens to be active in the UI. Preserve
      # the original run identity for validation provenance, but align a
      # private gate view's UI hash to that effective snapshot. The formal
      # gate still independently verifies run metadata, params_effective,
      # scope, and every required detector-output table.
      batch_gate_run_identity <- run_identity
      batch_gate_run_identity$ui_params_sha256 <- entry$params_sha256
      entry$batch_gate_run_identity <- batch_gate_run_identity
      dataset_identity <- tryCatch(
        stpd_ui_dataset_identity(ds, id), error = function(e) NULL
      )
      gate <- tryCatch(
        stpd_ui_formal_export_state(
          ds, params = params, dataset_id = id,
          run_identity = batch_gate_run_identity,
          dataset_identity = dataset_identity
        ),
        error = function(e) list(
          eligible = FALSE, code = "formal_export_gate_error",
          detail = conditionMessage(e)
        )
      )
      output_state <- tryCatch(
        stpd_ui_detector_output_state(
          ds, expected_output,
          selected_trains = run_identity$selected_trains
        ),
        error = function(e) list(
          current = FALSE, code = "detector_output_gate_error",
          detail = conditionMessage(e)
        )
      )
      entry$formal_export_state <- gate
      entry$detector_output_state <- output_state
      if (!isTRUE(gate$eligible)) {
        entry$code <- paste0("batch_export_", stpd_ui_state_chr(
          gate$code, "formal_gate_failed"
        ))
        entry$detail <- stpd_ui_state_chr(
          gate$detail, "The formal export gate rejected this dataset."
        )
      } else if (!isTRUE(output_state$current)) {
        entry$code <- paste0("batch_export_", stpd_ui_state_chr(
          output_state$code, "detector_output_gate_failed"
        ))
        entry$detail <- stpd_ui_state_chr(
          output_state$detail,
          "The detector-output identity gate rejected this dataset."
        )
      } else {
        entry$eligible <- TRUE
        entry$code <- "batch_export_dataset_ready"
        entry$detail <- "The dataset has current full-run, parameter, scope, and detector-output identities."
      }
    }
    entries[[id]] <- entry
    if (!isTRUE(entry$eligible)) {
      failure_rows[[length(failure_rows) + 1L]] <- data.frame(
        dataset_id = id, code = entry$code, detail = entry$detail,
        stringsAsFactors = FALSE
      )
    }
  }
  failures <- if (length(failure_rows) > 0L) {
    do.call(rbind, failure_rows)
  } else {
    empty_failures
  }
  eligible <- nrow(failures) == 0L && length(entries) > 0L
  structure(
    list(
      eligible = eligible,
      code = if (eligible) "batch_export_ready" else "batch_export_blocked",
      detail = if (eligible) {
        paste0("All ", length(entries), " datasets are eligible for a provenance-bound batch export.")
      } else {
        paste0(nrow(failures), " of ", length(entries), " datasets failed the batch export gate; no batch should be written.")
      },
      entries = entries,
      eligible_entries = entries[vapply(entries, function(x) isTRUE(x$eligible), logical(1))],
      failures = failures,
      output_root = root
    ),
    class = c("stpd_ui_batch_export_plan", "list")
  )
}

stpd_ui_zip_normalize_member <- function(x) {
  value <- as.character(x)
  value <- sub("^\\./", "", value)
  value
}

stpd_ui_zip_member_is_unsafe <- function(x) {
  value <- as.character(x)
  normalized <- stpd_ui_zip_normalize_member(value)
  parts <- strsplit(normalized, "/", fixed = TRUE)
  starts_absolute <- startsWith(normalized, "/") ||
    grepl("^[A-Za-z]:", normalized)
  has_backslash <- grepl("\\\\", value)
  has_parent <- any(vapply(parts, function(part) ".." %in% part, logical(1)))
  !nzchar(normalized) || starts_absolute || has_backslash || has_parent
}

stpd_ui_validate_zip_archive <- function(
    file, required_files, sentinel_files = required_files) {
  required <- unique(stpd_ui_zip_normalize_member(required_files))
  required <- required[!is.na(required) & nzchar(required)]
  sentinels <- unique(stpd_ui_zip_normalize_member(sentinel_files))
  sentinels <- sentinels[!is.na(sentinels) & nzchar(sentinels)]
  make <- function(valid, code, reason, members = character(),
                   missing = character(), duplicates = character(),
                   unsafe = character(), empty = character(),
                   extracted = character()) {
    size <- suppressWarnings(file.info(file)$size)
    if (length(size) == 0L || is.na(size)) size <- NA_real_
    structure(
      list(
        valid = isTRUE(valid), code = code, reason = reason,
        archive_size_bytes = as.numeric(size)[1],
        member_n = as.integer(length(members)),
        members = as.character(members),
        required_files = required,
        sentinel_files = sentinels,
        missing_files = as.character(missing),
        duplicate_members = as.character(duplicates),
        unsafe_members = as.character(unsafe),
        empty_sentinels = as.character(empty),
        extracted_files = as.character(extracted),
        sentinel_verified = length(sentinels) > 0L &&
          length(missing) == 0L && length(empty) == 0L && isTRUE(valid)
      ),
      class = c("stpd_ui_zip_validation", "list")
    )
  }
  if (length(file) != 1L || is.na(file) || !file.exists(file) ||
      isTRUE(file.info(file)$isdir) || !is.finite(file.info(file)$size) ||
      file.info(file)$size <= 0) {
    return(make(FALSE, "zip_file_missing_or_empty",
                "The ZIP path is missing, is a directory, or is empty."))
  }
  if (any(vapply(c(required, sentinels), stpd_ui_zip_member_is_unsafe,
                 logical(1)))) {
    return(make(FALSE, "zip_requested_member_unsafe",
                "A required or sentinel member name is unsafe."))
  }
  listing <- tryCatch(
    withCallingHandlers(
      utils::unzip(file, list = TRUE),
      warning = function(w) stop(conditionMessage(w), call. = FALSE)
    ),
    error = identity
  )
  if (inherits(listing, "error") || !is.data.frame(listing) ||
      !("Name" %in% names(listing))) {
    reason <- if (inherits(listing, "error")) conditionMessage(listing) else
      "The archive directory could not be read."
    return(make(FALSE, "zip_unreadable", reason))
  }
  members <- stpd_ui_zip_normalize_member(listing$Name)
  unsafe <- unique(members[vapply(
    listing$Name, stpd_ui_zip_member_is_unsafe, logical(1)
  )])
  if (length(unsafe) > 0L) {
    return(make(FALSE, "zip_unsafe_member",
                "The archive contains an absolute or parent-traversal member.",
                members = members, unsafe = unsafe))
  }
  duplicates <- unique(members[duplicated(members)])
  if (length(duplicates) > 0L) {
    return(make(FALSE, "zip_duplicate_member",
                "The archive contains duplicate member names.",
                members = members, duplicates = duplicates))
  }
  missing <- setdiff(required, members)
  if (length(missing) > 0L) {
    return(make(FALSE, "zip_required_file_missing",
                "One or more required archive members are missing.",
                members = members, missing = missing))
  }
  missing_sentinels <- setdiff(sentinels, members)
  if (length(missing_sentinels) > 0L) {
    return(make(FALSE, "zip_sentinel_missing",
                "One or more sentinel archive members are missing.",
                members = members, missing = missing_sentinels))
  }

  extract_dir <- tryCatch(
    stpd_ui_export_temp_path(
      prefix = "stpd_zip_verify", tmpdir = tempdir(), create = "directory"
    ),
    error = identity
  )
  if (inherits(extract_dir, "error")) {
    return(make(FALSE, "zip_verification_workspace_failed",
                conditionMessage(extract_dir), members = members))
  }
  on.exit(unlink(extract_dir, recursive = TRUE, force = TRUE), add = TRUE)
  extracted <- tryCatch(
    withCallingHandlers(
      utils::unzip(file, exdir = extract_dir, overwrite = FALSE),
      warning = function(w) stop(conditionMessage(w), call. = FALSE)
    ),
    error = identity
  )
  if (inherits(extracted, "error")) {
    return(make(FALSE, "zip_invalid_archive", conditionMessage(extracted),
                members = members))
  }
  required_paths <- file.path(extract_dir, required)
  missing_after_extract <- required[!file.exists(required_paths)]
  if (length(missing_after_extract) > 0L) {
    return(make(FALSE, "zip_required_file_not_extracted",
                "A required member could not be extracted.",
                members = members, missing = missing_after_extract,
                extracted = extracted))
  }
  sentinel_paths <- file.path(extract_dir, sentinels)
  sentinel_sizes <- suppressWarnings(file.info(sentinel_paths)$size)
  empty <- sentinels[
    !file.exists(sentinel_paths) | is.na(sentinel_sizes) | sentinel_sizes <= 0
  ]
  if (length(empty) > 0L) {
    return(make(FALSE, "zip_sentinel_empty",
                "A required sentinel is missing after extraction or is empty.",
                members = members, empty = empty, extracted = extracted))
  }
  make(TRUE, "zip_valid",
       "The ZIP directory, extraction, and required sentinels are valid.",
       members = members, extracted = extracted)
}
