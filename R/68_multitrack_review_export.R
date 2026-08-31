# Phase 2B: fail-soft export of the auditable Review -> Event product.
#
# The persisted product is treated as untrusted input.  Export paths are owned
# by code, the active product is rebuilt from its append-only history and the
# immutable Phase 2A Preview, and a stale product is permitted to carry history
# only.  None of these routines modifies the dataset, its legacy projections,
# or its automatic Preview.

stpd_multitrack_review_export_filenames <- function() {
  c(
    unname(stpd_multitrack_review_manifest_filenames()),
    "Multitrack_review_manifest.csv",
    "Multitrack_review.rds"
  )
}

stpd_multitrack_review_cleanup_exports <- function(out_dir) {
  if (length(out_dir) != 1L || is.na(out_dir) || !nzchar(out_dir)) {
    stop("A single non-empty Phase 2B export directory is required.",
         call. = FALSE)
  }
  if (!dir.exists(out_dir)) return(invisible(character()))
  fixed <- stpd_multitrack_review_export_filenames()
  stale <- file.path(out_dir, fixed)
  exists_or_link <- function(path) {
    link <- Sys.readlink(path)
    file.exists(path) | dir.exists(path) | (!is.na(link) & nzchar(link))
  }
  stale <- stale[exists_or_link(stale)]
  if (length(stale) > 0L) {
    unlink(stale, recursive = TRUE, force = TRUE)
  }
  remaining <- file.path(out_dir, fixed)
  remaining <- remaining[exists_or_link(remaining)]
  if (length(remaining) > 0L) {
    stop(
      paste0(
        "Could not remove stale Phase 2B review artifacts: ",
        paste(basename(remaining), collapse = ", "), "."
      ),
      call. = FALSE
    )
  }
  invisible(stale)
}

stpd_multitrack_review_export_abort <- function(code, message) {
  condition <- structure(
    list(
      message = as.character(message)[1],
      call = NULL,
      code = as.character(code)[1]
    ),
    class = c(
      "stpd_multitrack_review_export_validation_error", "error", "condition"
    )
  )
  stop(condition)
}

stpd_multitrack_review_export_result <- function(
    paths = character(), status, code = "", message = "") {
  out <- as.character(paths)
  names(out) <- names(paths)
  attr(out, "multitrack_review_export_status") <- as.character(status)[1]
  attr(out, "multitrack_review_export_code") <- as.character(code)[1]
  attr(out, "multitrack_review_export_message") <- as.character(message)[1]
  out
}

stpd_multitrack_review_append_export_warning <- function(
    out_dir, status, code, message) {
  if (length(out_dir) != 1L || is.na(out_dir) || !nzchar(out_dir)) {
    return(invisible(FALSE))
  }
  warning_file <- file.path(out_dir, "Methodological_warnings.txt")
  if (!file.exists(warning_file) || dir.exists(warning_file)) {
    return(invisible(FALSE))
  }
  line <- paste0(
    "component=multitrack_review; status=", status,
    "; code=", code,
    "; legacy_export_action=continue; automatic_preview_action=unchanged; message=",
    gsub("[\r\n]+", " ", as.character(message)[1], perl = TRUE)
  )
  cat("\n", line, "\n", file = warning_file, append = TRUE, sep = "")
  invisible(TRUE)
}

stpd_multitrack_review_export_table_specs <- function() {
  list(
    metadata = stpd_multitrack_review_empty_metadata(),
    transition_history = stpd_multitrack_review_empty_history(),
    current_decisions = stpd_multitrack_review_empty_current(),
    manual_intervals = stpd_multitrack_review_empty_manual_intervals(),
    final_intervals = stpd_multitrack_review_empty_final_intervals(),
    final_relationships = stpd_multitrack_review_empty_links(),
    final_per_isi = stpd_multitrack_review_empty_per_isi(),
    invariants = stpd_multitrack_review_empty_invariants()
  )
}

stpd_multitrack_review_export_is_sha256 <- function(x) {
  value <- as.character(x)
  length(value) > 0L && !anyNA(value) &&
    all(grepl("^[0-9a-f]{64}$", value))
}

stpd_multitrack_review_export_require_table_schema <- function(
    table, prototype, table_name) {
  if (!is.data.frame(table)) {
    stpd_multitrack_review_export_abort(
      "review_table_not_data_frame",
      paste0("Phase 2B component '", table_name, "' is not a data.frame.")
    )
  }
  expected_names <- names(prototype)
  actual_names <- names(table)
  expected_types <- unname(vapply(prototype, typeof, character(1)))
  actual_types <- unname(vapply(table, typeof, character(1)))
  if (!identical(class(table), "data.frame") ||
      length(setdiff(names(attributes(table)),
                     c("names", "class", "row.names"))) > 0L ||
      !identical(actual_names, expected_names) ||
      !identical(actual_types, expected_types)) {
    stpd_multitrack_review_export_abort(
      "review_table_schema_invalid",
      paste0(
        "Phase 2B table '", table_name,
        "' does not match its fixed column order and storage types."
      )
    )
  }
  invisible(TRUE)
}

stpd_multitrack_review_export_validate_history <- function(history) {
  validated <- tryCatch(
    stpd_multitrack_review_validate_history(history),
    error = function(e) e
  )
  if (inherits(validated, "error")) {
    stpd_multitrack_review_export_abort(
      "review_history_hash_chain_invalid", conditionMessage(validated)
    )
  }
  if (!identical(validated, history)) {
    stpd_multitrack_review_export_abort(
      "review_history_order_invalid",
      "The persisted Phase 2B history is not in canonical transition order."
    )
  }
  if (nrow(history) == 0L) return(invisible(TRUE))
  if (anyNA(history)) {
    stpd_multitrack_review_export_abort(
      "review_history_missing_value",
      "The append-only Phase 2B transition history must not contain missing values."
    )
  }

  hashes_only <- c(
    history$source_row_sha256,
    history$parent_preview_manifest_sha256,
    history$precondition_sha256,
    history$transition_sha256
  )
  if (!stpd_multitrack_review_export_is_sha256(hashes_only)) {
    stpd_multitrack_review_export_abort(
      "review_history_hash_field_invalid",
      "Phase 2B history contains a malformed SHA-256 provenance field."
    )
  }
  nonempty <- c(
    history$transition_id, history$operation_id, history$train,
    history$source_review_interval_id, history$source_candidate_id,
    history$source_candidate_key, history$source_candidate_layer,
    history$target_event_interval_id,
    history$reviewer, history$reason, history$server_time_utc
  )
  if (anyNA(nonempty) || any(!nzchar(nonempty)) ||
      any(history$schema_version != stpd_multitrack_review_schema_version()) ||
      any(history$conflict_policy_version !=
            stpd_multitrack_review_conflict_policy_version()) ||
      any(history$transition_status != "applied") ||
      any(!(history$transition_action %in% c("confirm", "revoke"))) ||
      any(history$review_target_track != "event") ||
      any(history$review_target_label != "burst") ||
      any(!(history$proposed_effect %in%
              c("create_event", "link_existing_event"))) ||
      anyNA(history$source_start_isi) || anyNA(history$source_end_isi) ||
      any(history$source_start_isi < 1L) ||
      any(history$source_end_isi < history$source_start_isi)) {
    stpd_multitrack_review_export_abort(
      "review_history_semantics_invalid",
      "Phase 2B transition history contains invalid identity, action, target, or geometry fields."
    )
  }
  expected_transition_ids <- vapply(seq_len(nrow(history)), function(i) {
    paste0(
      "mtr_transition_",
      substr(stpd_multitrack_review_sha256(
        paste(
          history$operation_id[i], history$train[i],
          history$source_review_interval_id[i],
          history$transition_action[i], sep = "|"
        ),
        serialize = FALSE
      ), 1L, 24L)
    )
  }, character(1))
  if (!identical(history$transition_id, expected_transition_ids)) {
    stpd_multitrack_review_export_abort(
      "review_history_transition_id_invalid",
      "Phase 2B transition IDs do not match their immutable operation/source payloads."
    )
  }

  # Validate the per-source state machine as well as the global hash chain.
  state <- new.env(parent = emptyenv())
  source_payload <- new.env(parent = emptyenv())
  for (i in seq_len(nrow(history))) {
    # A detector rerun invalidates active decisions but preserves the append-only
    # audit chain. The same stable Review interval may therefore be confirmed
    # again under a new immutable parent Preview without becoming a duplicate
    # confirmation in the old run's state machine.
    key <- paste(
      history$parent_preview_schema_version[i],
      history$parent_preview_policy_hash[i],
      history$parent_preview_run_id[i],
      history$parent_preview_params_hash[i],
      history$parent_preview_manifest_sha256[i],
      history$train[i], history$source_review_interval_id[i], sep = "\r"
    )
    current <- if (exists(key, envir = state, inherits = FALSE)) {
      get(key, envir = state, inherits = FALSE)
    } else {
      "unreviewed"
    }
    action <- history$transition_action[i]
    if ((action == "confirm" && current == "confirmed") ||
        (action == "revoke" && current != "confirmed")) {
      stpd_multitrack_review_export_abort(
        "review_history_state_machine_invalid",
        paste0("Invalid ", action, " transition sequence for Review source '", key, "'.")
      )
    }
    payload <- paste(
      history$source_row_sha256[i], history$source_candidate_id[i],
      history$source_candidate_key[i], history$source_candidate_layer[i],
      history$source_candidate_source[i], history$source_start_isi[i],
      history$source_end_isi[i], history$proposed_effect[i],
      history$target_event_interval_id[i], sep = "\r"
    )
    if (exists(key, envir = source_payload, inherits = FALSE) &&
        !identical(get(key, envir = source_payload, inherits = FALSE), payload)) {
      stpd_multitrack_review_export_abort(
        "review_history_source_payload_changed",
        "A Review source changes immutable identity, geometry, or target across its history."
      )
    }
    assign(key, payload, envir = source_payload)
    assign(key, if (action == "confirm") "confirmed" else "revoked",
           envir = state)
  }

  # One operation may contain several atomic rows, but its audit payload must
  # be identical across those rows and it may occupy only one contiguous block.
  operations <- unique(history$operation_id)
  for (operation in operations) {
    idx <- which(history$operation_id == operation)
    if (!identical(idx, seq.int(min(idx), max(idx)))) {
      stpd_multitrack_review_export_abort(
        "review_history_operation_not_contiguous",
        "Rows belonging to one Phase 2B operation are not contiguous."
      )
    }
    same_fields <- c(
      "transition_action", "precondition_sha256", "reviewer", "reason",
      "server_time_utc", "parent_preview_schema_version",
      "parent_preview_policy_hash", "parent_preview_run_id",
      "parent_preview_params_hash", "parent_preview_manifest_sha256",
      "conflict_policy_version"
    )
    if (any(vapply(same_fields, function(field) {
      length(unique(as.character(history[[field]][idx]))) != 1L
    }, logical(1)))) {
      stpd_multitrack_review_export_abort(
        "review_history_operation_payload_invalid",
        "Rows belonging to one Phase 2B operation do not share one immutable audit payload."
      )
    }
  }
  invisible(TRUE)
}

stpd_multitrack_review_export_validate_common <- function(product) {
  specs <- stpd_multitrack_review_export_table_specs()
  required_components <- c(names(specs), "table_manifest")
  if (!is.list(product) || !identical(names(product), required_components)) {
    stpd_multitrack_review_export_abort(
      "review_component_schema_invalid",
      paste0(
        "The Phase 2B component set/order is not fixed; expected: ",
        paste(required_components, collapse = ", "), "."
      )
    )
  }
  if (!identical(class(product), c("stpd_multitrack_review_product", "list")) ||
      length(setdiff(names(attributes(product)), c("names", "class"))) > 0L) {
    stpd_multitrack_review_export_abort(
      "review_product_class_invalid",
      "The Phase 2B product class or top-level attributes are not canonical."
    )
  }

  # Reconstruct an ordinary, unclassed list. Subsetting the classed product
  # directly would retain its top-level class and would alter provenance hashes.
  tables <- lapply(names(specs), function(name) product[[name]])
  names(tables) <- names(specs)
  for (name in names(specs)) {
    stpd_multitrack_review_export_require_table_schema(
      tables[[name]], specs[[name]], name
    )
  }
  manifest <- product$table_manifest
  stpd_multitrack_review_export_require_table_schema(
    manifest, stpd_multitrack_review_empty_manifest(), "table_manifest"
  )
  metadata <- tables$metadata
  if (nrow(metadata) != 1L) {
    stpd_multitrack_review_export_abort(
      "review_metadata_cardinality_invalid",
      "Phase 2B metadata must contain exactly one row."
    )
  }
  lifecycle <- as.character(metadata$lifecycle_status[1])
  expected_authoritative <- identical(lifecycle, "active")
  if (anyNA(metadata) ||
      !identical(metadata$schema_version,
                 stpd_multitrack_review_schema_version()) ||
      !identical(metadata$conflict_policy_version,
                 stpd_multitrack_review_conflict_policy_version()) ||
      !(lifecycle %in% c("active", "stale_after_detector_rerun")) ||
      !identical(metadata$authoritative[1], expected_authoritative) ||
      !nzchar(metadata$run_id[1]) || !nzchar(metadata$params_hash[1]) ||
      !nzchar(metadata$parent_preview_schema_version[1]) ||
      !stpd_multitrack_review_export_is_sha256(c(
        metadata$params_hash,
        metadata$parent_preview_policy_hash,
        metadata$parent_preview_manifest_sha256,
        metadata$transition_history_sha256,
        metadata$product_sha256
      )) ||
      isTRUE(metadata$legacy_projection_changed[1]) ||
      isTRUE(metadata$automatic_preview_changed[1])) {
    stpd_multitrack_review_export_abort(
      "review_metadata_semantics_invalid",
      "Phase 2B metadata identity, provenance, or immutability flags are invalid."
    )
  }

  schema_version <- stpd_multitrack_review_schema_version()
  run_id <- metadata$run_id[1]
  params_hash <- metadata$params_hash[1]
  for (name in names(tables)) {
    table <- tables[[name]]
    if (nrow(table) == 0L) next
    if ("schema_version" %in% names(table) &&
        (anyNA(table$schema_version) ||
         any(table$schema_version != schema_version))) {
      stpd_multitrack_review_export_abort(
        "review_table_schema_version_mismatch",
        paste0("Table '", name, "' contains a foreign schema version.")
      )
    }
    if ("run_id" %in% names(table) &&
        (anyNA(table$run_id) || any(table$run_id != run_id))) {
      stpd_multitrack_review_export_abort(
        "review_table_run_id_mismatch",
        paste0("Table '", name, "' contains a foreign run_id.")
      )
    }
    if ("params_hash" %in% names(table) &&
        (anyNA(table$params_hash) || any(table$params_hash != params_hash))) {
      stpd_multitrack_review_export_abort(
        "review_table_params_hash_mismatch",
        paste0("Table '", name, "' contains a foreign params_hash.")
      )
    }
    if ("authoritative" %in% names(table)) {
      table_authoritative <- if (identical(name, "metadata")) {
        rep(expected_authoritative, nrow(table))
      } else {
        rep(TRUE, nrow(table))
      }
      if (anyNA(table$authoritative) ||
          !identical(as.logical(table$authoritative), table_authoritative)) {
      stpd_multitrack_review_export_abort(
        "review_table_authority_invalid",
          paste0("Table '", name, "' has an authority flag inconsistent with its lifecycle.")
      )
      }
    }
  }

  fixed_files <- stpd_multitrack_review_manifest_filenames()
  expected_table_names <- names(fixed_files)
  if (nrow(manifest) != length(expected_table_names) ||
      anyDuplicated(manifest$table_name) ||
      !identical(as.character(manifest$table_name), expected_table_names)) {
    stpd_multitrack_review_export_abort(
      "review_manifest_table_identity_invalid",
      "Phase 2B manifest rows do not exactly match the fixed eight-table set."
    )
  }
  if (!identical(as.character(manifest$file_name), unname(fixed_files))) {
    stpd_multitrack_review_export_abort(
      "review_manifest_filename_invalid",
      paste(
        "Phase 2B manifest file names do not match the fixed safe mapping.",
        "Persisted file names are never used as write paths."
      )
    )
  }
  expected_rows <- unname(vapply(tables, nrow, integer(1)))
  expected_columns <- unname(vapply(tables, ncol, integer(1)))
  expected_types <- unname(vapply(
    tables, stpd_multitrack_review_column_types, character(1)
  ))
  expected_hashes <- unname(vapply(
    tables, stpd_multitrack_review_table_hash, character(1)
  ))
  if (!identical(as.integer(manifest$row_count), expected_rows) ||
      !identical(as.integer(manifest$column_count), expected_columns) ||
      !identical(as.character(manifest$column_types), expected_types) ||
      !identical(as.character(manifest$table_sha256), expected_hashes)) {
    stpd_multitrack_review_export_abort(
      "review_manifest_payload_mismatch",
      paste(
        "Phase 2B manifest row counts, column counts, storage types, or",
        "real-time table hashes do not match the persisted tables."
      )
    )
  }
  manifest_identity_ok <-
    identical(as.character(manifest$schema_version),
              rep(schema_version, nrow(manifest))) &&
    identical(as.character(manifest$run_id), rep(run_id, nrow(manifest))) &&
    identical(as.character(manifest$params_hash),
              rep(params_hash, nrow(manifest))) &&
    identical(
      as.logical(manifest$authoritative),
      rep(expected_authoritative, nrow(manifest))
    )
  if (!manifest_identity_ok) {
    stpd_multitrack_review_export_abort(
      "review_manifest_identity_mismatch",
      "Phase 2B manifest identity fields do not match metadata."
    )
  }

  stpd_multitrack_review_export_validate_history(tables$transition_history)
  history_hash <- stpd_multitrack_review_table_hash(tables$transition_history)
  product_payload <- tables[setdiff(names(tables), "metadata")]
  product_hash <- stpd_multitrack_review_sha256(product_payload)
  if (!identical(metadata$transition_history_sha256, history_hash) ||
      !identical(metadata$product_sha256, product_hash)) {
    stpd_multitrack_review_export_abort(
      "review_metadata_hash_mismatch",
      "Phase 2B metadata history/product hashes do not match their live payloads."
    )
  }
  expected_counts <- c(
    transition_n = nrow(tables$transition_history),
    confirmed_n = sum(tables$current_decisions$current_status == "confirmed"),
    manual_interval_n = nrow(tables$manual_intervals),
    final_interval_n = nrow(tables$final_intervals),
    final_relationship_n = nrow(tables$final_relationships),
    final_per_isi_n = nrow(tables$final_per_isi)
  )
  actual_counts <- c(
    transition_n = metadata$transition_n,
    confirmed_n = metadata$confirmed_n,
    manual_interval_n = metadata$manual_interval_n,
    final_interval_n = metadata$final_interval_n,
    final_relationship_n = metadata$final_relationship_n,
    final_per_isi_n = metadata$final_per_isi_n
  )
  if (!identical(as.integer(actual_counts), as.integer(expected_counts))) {
    stpd_multitrack_review_export_abort(
      "review_metadata_count_mismatch",
      "Phase 2B metadata row counts do not match the live component tables."
    )
  }
  list(tables = tables, manifest = manifest, metadata = metadata,
       fixed_files = fixed_files)
}

stpd_multitrack_review_export_validate_parent_history <- function(
    history, metadata, preview, verify_source_rows = TRUE) {
  if (nrow(history) == 0L) return(invisible(TRUE))
  preview_metadata <- preview$metadata
  expected_parent <- list(
    schema = stpd_multitrack_review_chr(preview_metadata$schema_version),
    policy = stpd_multitrack_review_chr(preview_metadata$policy_hash),
    run = stpd_multitrack_review_chr(preview_metadata$run_id),
    params = stpd_multitrack_review_chr(preview_metadata$params_hash),
    manifest = stpd_multitrack_review_preview_manifest_hash(preview)
  )
  # History is append-only across detector runs. Only transitions belonging to
  # the current parent Preview are replayed against its source rows; older rows
  # retain their own immutable parent identity and hash-chain position.
  current_history <- history[
    history$parent_preview_schema_version == expected_parent$schema &
      history$parent_preview_policy_hash == expected_parent$policy &
      history$parent_preview_run_id == expected_parent$run &
      history$parent_preview_params_hash == expected_parent$params &
      history$parent_preview_manifest_sha256 == expected_parent$manifest,
    , drop = FALSE
  ]
  terminal <- history[nrow(history), , drop = FALSE]
  terminal_parent_ok <-
    identical(terminal$parent_preview_schema_version, expected_parent$schema) &&
    identical(terminal$parent_preview_policy_hash, expected_parent$policy) &&
    identical(terminal$parent_preview_run_id, expected_parent$run) &&
    identical(terminal$parent_preview_params_hash, expected_parent$params) &&
    identical(terminal$parent_preview_manifest_sha256, expected_parent$manifest)
  history_parent_ok <- nrow(current_history) > 0L && terminal_parent_ok &&
    all(current_history$parent_preview_schema_version == expected_parent$schema) &&
    all(current_history$parent_preview_policy_hash == expected_parent$policy) &&
    all(current_history$parent_preview_manifest_sha256 == expected_parent$manifest)
  metadata_parent_ok <-
    identical(metadata$parent_preview_schema_version, expected_parent$schema) &&
    identical(metadata$parent_preview_policy_hash, expected_parent$policy) &&
    identical(metadata$run_id, expected_parent$run) &&
    identical(metadata$params_hash, expected_parent$params) &&
    identical(metadata$parent_preview_manifest_sha256,
              expected_parent$manifest)
  if (!history_parent_ok || !metadata_parent_ok) {
    stpd_multitrack_review_export_abort(
      "review_parent_preview_identity_mismatch",
      "Phase 2B metadata/history do not identify the current immutable parent Preview."
    )
  }
  if (!isTRUE(verify_source_rows)) return(invisible(TRUE))

  intervals <- preview$intervals
  for (i in seq_len(nrow(current_history))) {
    hit <- which(
      intervals$train == current_history$train[i] &
        intervals$interval_id == current_history$source_review_interval_id[i]
    )
    if (length(hit) != 1L) {
      stpd_multitrack_review_export_abort(
        "review_source_foreign_key_invalid",
        "A transition does not resolve to one train-qualified parent Review interval."
      )
    }
    source <- intervals[hit, , drop = FALSE]
    d012_ok <- identical(stpd_multitrack_review_chr(source$interval_kind), "candidate") &&
      identical(stpd_multitrack_review_chr(source$semantic_track), "review") &&
      identical(stpd_multitrack_review_chr(source$label), "possible_burst") &&
      isTRUE(source$active_in_preview[1]) &&
      identical(stpd_multitrack_review_chr(source$review_target_track), "event") &&
      identical(stpd_multitrack_review_chr(source$review_target_label), "burst") &&
      isTRUE(source$review_promotion_required[1])
    source_identity_ok <- d012_ok &&
      identical(current_history$source_row_sha256[i],
                stpd_multitrack_review_source_row_hash(source)) &&
      identical(current_history$source_candidate_id[i],
                stpd_multitrack_review_chr(source$candidate_id)) &&
      identical(current_history$source_candidate_key[i],
                stpd_multitrack_review_chr(source$candidate_key)) &&
      identical(current_history$source_candidate_layer[i],
                stpd_multitrack_review_chr(source$candidate_layer)) &&
      identical(current_history$source_candidate_source[i],
                stpd_multitrack_review_chr(source$candidate_source)) &&
      identical(current_history$source_start_isi[i], as.integer(source$start_isi[1])) &&
      identical(current_history$source_end_isi[i], as.integer(source$end_isi[1]))
    if (!source_identity_ok) {
      stpd_multitrack_review_export_abort(
        "review_source_provenance_mismatch",
        "A Phase 2B transition no longer matches its immutable D-012 Review source row."
      )
    }
    expected_target <- if (current_history$proposed_effect[i] == "create_event") {
      stpd_multitrack_review_effect_interval_id(
        expected_parent$run, current_history$train[i],
        current_history$source_review_interval_id[i]
      )
    } else {
      event_hit <- which(
        intervals$interval_id == current_history$target_event_interval_id[i] &
          intervals$train == current_history$train[i] &
          intervals$semantic_track == "event" & intervals$label == "burst" &
          intervals$active_in_preview &
          intervals$start_isi == current_history$source_start_isi[i] &
          intervals$end_isi == current_history$source_end_isi[i]
      )
      if (length(event_hit) != 1L) {
        stpd_multitrack_review_export_abort(
          "review_existing_event_link_invalid",
          "A same-span confirmation does not resolve to one active automatic Burst."
        )
      }
      intervals$interval_id[event_hit]
    }
    if (!identical(current_history$target_event_interval_id[i], expected_target)) {
      stpd_multitrack_review_export_abort(
        "review_target_event_identity_invalid",
        "A transition target Event identity is inconsistent with its declared effect."
      )
    }
  }
  invisible(TRUE)
}

stpd_multitrack_review_export_validate_foreign_keys <- function(
    tables, preview, ds) {
  history <- tables$transition_history
  current <- tables$current_decisions
  manual <- tables$manual_intervals
  final <- tables$final_intervals
  links <- tables$final_relationships
  per_isi <- tables$final_per_isi
  preview_intervals <- preview$intervals

  if (anyDuplicated(current[c("train", "source_review_interval_id")]) ||
      anyDuplicated(manual$manual_interval_id) ||
      anyDuplicated(final$final_interval_id) ||
      anyDuplicated(links$relationship_id) ||
      anyDuplicated(per_isi[c("train", "isi_index")])) {
    stpd_multitrack_review_export_abort(
      "review_final_identity_not_unique",
      "Phase 2B current, interval, relationship, or per-ISI identities are not unique."
    )
  }
  if (nrow(current) > 0L) {
    transition_hit <- match(current$current_transition_id, history$transition_id)
    source_key <- paste(current$train, current$source_review_interval_id, sep = "\r")
    preview_key <- paste(preview_intervals$train, preview_intervals$interval_id,
                         sep = "\r")
    if (anyNA(transition_hit) || !all(source_key %in% preview_key) ||
        !identical(current$current_transition_sha256,
                   history$transition_sha256[transition_hit]) ||
        !identical(current$source_row_sha256,
                   history$source_row_sha256[transition_hit])) {
      stpd_multitrack_review_export_abort(
        "review_current_foreign_key_invalid",
        "Current decisions contain broken history or parent Review references."
      )
    }
  }
  if (nrow(manual) > 0L) {
    for (i in seq_len(nrow(manual))) {
      row <- manual[i, , drop = FALSE]
      history_hit <- which(
        history$train == row$train &
          history$transition_id == row$source_transition_id &
          history$source_review_interval_id == row$source_review_interval_id
      )
      source_hit <- which(
        preview_intervals$train == row$train &
          preview_intervals$interval_id == row$source_review_interval_id
      )
      target_hit <- which(
        final$train == row$train &
          final$final_interval_id == row$linked_event_interval_id
      )
      manual_ok <- length(history_hit) == 1L && length(source_hit) == 1L &&
        length(target_hit) == 1L
      if (manual_ok) {
        transition <- history[history_hit, , drop = FALSE]
        source <- preview_intervals[source_hit, , drop = FALSE]
        target <- final[target_hit, , drop = FALSE]
        effect <- as.character(transition$proposed_effect[1])
        creates_new <- identical(effect, "create_event")
        expected_final_source <- if (creates_new) {
          "phase2b_review_confirmation"
        } else {
          "automatic_preview"
        }
        target_provenance_ok <- if (creates_new) {
          identical(target$source_review_interval_id,
                    row$source_review_interval_id) &&
            identical(target$source_transition_id, row$source_transition_id)
        } else {
          nzchar(stpd_multitrack_review_chr(
            target$source_preview_interval_id
          ))
        }
        manual_ok <-
          identical(as.character(transition$transition_action), "confirm") &&
          identical(as.character(transition$target_event_interval_id),
                    as.character(row$linked_event_interval_id)) &&
          identical(effect, as.character(row$action_effect[1])) &&
          identical(as.logical(row$creates_new_event), creates_new) &&
          identical(as.character(row$semantic_track), "event") &&
          identical(as.character(row$label), "burst") &&
          identical(as.character(source$semantic_track), "review") &&
          identical(as.character(source$label), "possible_burst") &&
          identical(as.integer(row$start_isi),
                    as.integer(transition$source_start_isi)) &&
          identical(as.integer(row$end_isi),
                    as.integer(transition$source_end_isi)) &&
          identical(as.integer(row$start_isi), as.integer(source$start_isi)) &&
          identical(as.integer(row$end_isi), as.integer(source$end_isi)) &&
          identical(as.character(target$semantic_track), "event") &&
          identical(as.character(target$label), "burst") &&
          identical(as.integer(row$start_isi), as.integer(target$start_isi)) &&
          identical(as.integer(row$end_isi), as.integer(target$end_isi)) &&
          identical(as.character(target$final_source), expected_final_source) &&
          isTRUE(target_provenance_ok)
      }
      if (!isTRUE(manual_ok)) {
        stpd_multitrack_review_export_abort(
          "review_manual_foreign_key_invalid",
          paste(
            "A manual confirmation does not resolve within one train to its",
            "source transition, Review row, and exact-span final Event/Burst."
          )
        )
      }
    }
  }
  if (nrow(final) > 0L) {
    automatic <- final$final_source == "automatic_preview"
    promoted <- final$final_source == "phase2b_review_confirmation"
    if (anyNA(automatic) || anyNA(promoted) ||
        any(!(automatic | promoted)) ||
        !all(final$source_preview_interval_id[automatic] %in%
               preview_intervals$interval_id) ||
        !all(final$source_review_interval_id[promoted] %in%
               history$source_review_interval_id) ||
        !all(final$source_transition_id[promoted] %in% history$transition_id)) {
      stpd_multitrack_review_export_abort(
        "review_final_interval_foreign_key_invalid",
        "Final intervals contain broken automatic or manual provenance references."
      )
    }
  }
  if (nrow(links) > 0L) {
    for (i in seq_len(nrow(links))) {
      row <- links[i, , drop = FALSE]
      history_hit <- which(
        history$train == row$train &
          history$transition_id == row$source_transition_id &
          history$source_review_interval_id == row$source_review_interval_id
      )
      manual_hit <- which(
        manual$train == row$train &
          manual$source_transition_id == row$source_transition_id &
          manual$source_review_interval_id == row$source_review_interval_id &
          manual$linked_event_interval_id == row$target_final_interval_id &
          manual$action_effect == row$action_effect
      )
      source_hit <- which(
        preview_intervals$train == row$train &
          preview_intervals$interval_id == row$source_review_interval_id
      )
      target_hit <- which(
        final$train == row$train &
          final$final_interval_id == row$target_final_interval_id
      )
      link_ok <- length(history_hit) == 1L && length(manual_hit) == 1L &&
        length(source_hit) == 1L && length(target_hit) == 1L
      if (link_ok) {
        transition <- history[history_hit, , drop = FALSE]
        manual_row <- manual[manual_hit, , drop = FALSE]
        source <- preview_intervals[source_hit, , drop = FALSE]
        target <- final[target_hit, , drop = FALSE]
        expected_relationship_id <- stpd_multitrack_review_relationship_id(
          row$run_id, row$train, row$source_review_interval_id,
          row$target_final_interval_id
        )
        link_ok <-
          identical(as.character(row$relationship_id),
                    expected_relationship_id) &&
          identical(as.character(row$relationship_type),
                    "review_confirmation_to_event") &&
          isTRUE(row$non_destructive[1]) &&
          identical(as.character(transition$transition_action), "confirm") &&
          identical(as.character(transition$proposed_effect),
                    as.character(row$action_effect)) &&
          identical(as.character(transition$target_event_interval_id),
                    as.character(row$target_final_interval_id)) &&
          identical(as.integer(transition$source_start_isi),
                    as.integer(source$start_isi)) &&
          identical(as.integer(transition$source_end_isi),
                    as.integer(source$end_isi)) &&
          identical(as.integer(manual_row$start_isi),
                    as.integer(source$start_isi)) &&
          identical(as.integer(manual_row$end_isi),
                    as.integer(source$end_isi)) &&
          identical(as.character(target$semantic_track), "event") &&
          identical(as.character(target$label), "burst") &&
          identical(as.integer(target$start_isi),
                    as.integer(source$start_isi)) &&
          identical(as.integer(target$end_isi),
                    as.integer(source$end_isi))
      }
      if (!isTRUE(link_ok)) {
        stpd_multitrack_review_export_abort(
          "review_relationship_foreign_key_invalid",
          paste(
            "A final Review-to-Event relationship does not match one",
            "same-train transition/manual row and exact-span final Event/Burst."
          )
        )
      }
    }
  }

  selected <- stpd_multitrack_preview_selected_train_values(
    preview$metadata$selected_trains
  )
  expected_nonempty <- selected[vapply(selected, function(train) {
    is.data.frame(ds$trains[[train]]) && nrow(ds$trains[[train]]) > 0L
  }, logical(1))]
  if (!setequal(unique(as.character(per_isi$train)), expected_nonempty)) {
    stpd_multitrack_review_export_abort(
      "review_per_isi_scope_invalid",
      "Final per-ISI rows do not exactly cover the parent Preview train scope."
    )
  }
  for (train in selected) {
    dat <- ds$trains[[train]]
    part <- per_isi[per_isi$train == train, , drop = FALSE]
    part <- part[order(part$isi_index, method = "radix"), , drop = FALSE]
    if (!is.data.frame(dat) ||
        !identical(part$isi_index, seq_len(nrow(dat))) ||
        !identical(as.numeric(part$timestamp_sec),
                   suppressWarnings(as.numeric(dat$timestamp_sec))) ||
        !identical(as.numeric(part$ISI_sec),
                   suppressWarnings(as.numeric(dat$ISI_sec)))) {
      stpd_multitrack_review_export_abort(
        "review_per_isi_parent_mismatch",
        paste0("Final per-ISI rows do not match parent train '", train, "'.")
      )
    }
    projection_columns <- grep("^pattern_final_", names(part), value = TRUE)
    if (nrow(part) > 0L && any(nzchar(as.character(
      unlist(part[1L, projection_columns, drop = FALSE], use.names = FALSE)
    )))) {
      stpd_multitrack_review_export_abort(
        "review_per_isi_first_row_invalid",
        "The first spike contains a final semantic-track projection."
      )
    }
  }
  id_columns <- grep("_interval_id$", names(per_isi), value = TRUE)
  projection_columns <- grep("^pattern_final_", names(per_isi), value = TRUE)
  if (nrow(per_isi) > 0L && anyNA(per_isi[projection_columns])) {
    stpd_multitrack_review_export_abort(
      "review_per_isi_projection_missing",
      "Final per-ISI semantic-track projections must use empty strings, not missing values."
    )
  }
  projected_ids <- unique(unlist(lapply(
    per_isi[id_columns], function(x) as.character(x[nzchar(as.character(x))])
  ), use.names = FALSE))
  if (length(projected_ids) > 0L &&
      !all(projected_ids %in% final$final_interval_id)) {
    stpd_multitrack_review_export_abort(
      "review_per_isi_foreign_key_invalid",
      "Final per-ISI projections reference unknown final intervals."
    )
  }
  invisible(TRUE)
}

stpd_multitrack_review_export_validate_active <- function(ds, common) {
  metadata <- common$metadata
  if (!identical(metadata$lifecycle_status, "active") ||
      !identical(metadata$stale_reason, "")) {
    stpd_multitrack_review_export_abort(
      "review_active_lifecycle_invalid",
      "An active Phase 2B product has inconsistent lifecycle metadata."
    )
  }
  preview <- tryCatch(
    stpd_multitrack_review_require_preview(ds),
    error = function(e) e
  )
  if (inherits(preview, "error")) {
    stpd_multitrack_review_export_abort(
      "review_parent_preview_invalid",
      paste0("The active Phase 2B parent Preview is invalid: ",
             conditionMessage(preview))
    )
  }
  stpd_multitrack_review_export_validate_parent_history(
    common$tables$transition_history, metadata, preview,
    verify_source_rows = TRUE
  )
  stpd_multitrack_review_export_validate_foreign_keys(
    common$tables, preview, ds
  )

  rebuilt <- tryCatch(
    stpd_multitrack_review_build_product(
      ds, preview, common$tables$transition_history
    ),
    error = function(e) e
  )
  if (inherits(rebuilt, "error")) {
    stpd_multitrack_review_export_abort(
      "review_deterministic_rebuild_failed",
      paste0("Phase 2B deterministic rebuild failed: ",
             conditionMessage(rebuilt))
    )
  }
  component_names <- c(names(common$tables), "table_manifest")
  mismatch <- character()
  for (name in component_names) {
    persisted <- if (identical(name, "table_manifest")) {
      common$manifest
    } else {
      common$tables[[name]]
    }
    if (!identical(persisted, rebuilt[[name]])) mismatch <- c(mismatch, name)
  }
  if (length(mismatch) > 0L) {
    stpd_multitrack_review_export_abort(
      "review_deterministic_rebuild_mismatch",
      paste0(
        "Persisted Phase 2B tables differ from the deterministic history + ",
        "parent Preview rebuild: ", paste(mismatch, collapse = ", "), "."
      )
    )
  }
  list(product = rebuilt, lifecycle_status = "active")
}

stpd_multitrack_review_export_validate_stale <- function(product, common) {
  metadata <- common$metadata
  if (!identical(metadata$lifecycle_status, "stale_after_detector_rerun") ||
      !nzchar(metadata$stale_reason)) {
    stpd_multitrack_review_export_abort(
      "review_stale_lifecycle_invalid",
      "A stale Phase 2B history product must contain an explicit stale reason."
    )
  }
  derived_names <- c(
    "current_decisions", "manual_intervals", "final_intervals",
    "final_relationships", "final_per_isi", "invariants"
  )
  if (any(vapply(common$tables[derived_names], nrow, integer(1)) != 0L) ||
      metadata$confirmed_n != 0L || metadata$manual_interval_n != 0L ||
      metadata$final_interval_n != 0L ||
      metadata$final_relationship_n != 0L ||
      metadata$final_per_isi_n != 0L) {
    stpd_multitrack_review_export_abort(
      "review_stale_contains_active_final",
      paste(
        "A stale Phase 2B product may export append-only history only; it",
        "must not contain current decisions or an active final projection."
      )
    )
  }
  history <- common$tables$transition_history
  if (nrow(history) > 0L) {
    terminal <- history[nrow(history), , drop = FALSE]
    parent_ok <-
      identical(
        terminal$parent_preview_schema_version,
        metadata$parent_preview_schema_version
      ) &&
      identical(
        terminal$parent_preview_policy_hash,
        metadata$parent_preview_policy_hash
      ) &&
      identical(terminal$parent_preview_run_id, metadata$run_id) &&
      identical(
        terminal$parent_preview_params_hash, metadata$params_hash
      ) &&
      identical(
        terminal$parent_preview_manifest_sha256,
        metadata$parent_preview_manifest_sha256
      )
    if (!parent_ok) {
      stpd_multitrack_review_export_abort(
        "review_stale_history_provenance_mismatch",
        "Stale history rows do not share the archived parent Preview identity."
      )
    }
  }
  # A stale product is exported exactly as a history-only audit record.  It is
  # intentionally never rebuilt against the new detector run and can therefore
  # never reactivate an old confirmation.
  rebuilt <- stpd_multitrack_review_stale_product(
    product, metadata$stale_reason[1]
  )
  required <- c(names(common$tables), "table_manifest")
  mismatch <- required[!vapply(required, function(name) {
    identical(product[[name]], rebuilt[[name]])
  }, logical(1))]
  if (length(mismatch) > 0L) {
    stpd_multitrack_review_export_abort(
      "review_stale_deterministic_rebuild_mismatch",
      paste0(
        "The stale history-only product is not canonical: ",
        paste(mismatch, collapse = ", "), "."
      )
    )
  }
  list(product = rebuilt, lifecycle_status = "stale_after_detector_rerun")
}

stpd_multitrack_review_validate_for_export <- function(ds) {
  product <- if (is.list(ds) && is.list(ds$results)) {
    ds$results[[stpd_multitrack_review_result_key()]] %||% NULL
  } else {
    NULL
  }
  if (is.null(product)) {
    stpd_multitrack_review_export_abort(
      "review_product_missing", "No Phase 2B product is present."
    )
  }
  common <- stpd_multitrack_review_export_validate_common(product)
  lifecycle <- common$metadata$lifecycle_status[1]
  validated <- if (identical(lifecycle, "active")) {
    stpd_multitrack_review_export_validate_active(ds, common)
  } else if (identical(lifecycle, "stale_after_detector_rerun")) {
    stpd_multitrack_review_export_validate_stale(product, common)
  } else {
    stpd_multitrack_review_export_abort(
      "review_lifecycle_status_invalid",
      paste0("Unsupported Phase 2B lifecycle_status: '", lifecycle, "'.")
    )
  }
  list(
    product = validated$product,
    tables = common$tables,
    manifest = common$manifest,
    fixed_files = common$fixed_files,
    lifecycle_status = validated$lifecycle_status
  )
}

stpd_write_multitrack_review_strict <- function(ds, out_dir) {
  validated <- stpd_multitrack_review_validate_for_export(ds)
  if (length(out_dir) != 1L || is.na(out_dir) || !nzchar(out_dir)) {
    stop("A single non-empty Phase 2B export directory is required.",
         call. = FALSE)
  }
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(out_dir)) {
    stop("Could not create the Phase 2B review export directory.",
         call. = FALSE)
  }
  staging_dir <- tempfile(".stpd_multitrack_review_", tmpdir = out_dir)
  if (!dir.create(staging_dir, recursive = FALSE, showWarnings = FALSE)) {
    stop("Could not create the Phase 2B review staging directory.",
         call. = FALSE)
  }
  on.exit(unlink(staging_dir, recursive = TRUE, force = TRUE), add = TRUE)

  paths <- character()
  for (name in names(validated$tables)) {
    path <- file.path(staging_dir, validated$fixed_files[[name]])
    write_csv_safe(
      validated$tables[[name]], path,
      row.names = FALSE, fileEncoding = "UTF-8"
    )
    paths[name] <- path
  }
  rds_path <- file.path(staging_dir, "Multitrack_review.rds")
  saveRDS(validated$product, rds_path, version = 2)
  if (!identical(readRDS(rds_path), validated$product)) {
    stop("The staged Phase 2B RDS did not round-trip exactly.", call. = FALSE)
  }
  paths["rds"] <- rds_path
  # The manifest is staged after every data table and the canonical RDS.
  manifest_path <- file.path(staging_dir, "Multitrack_review_manifest.csv")
  write_csv_safe(
    validated$manifest, manifest_path,
    row.names = FALSE, fileEncoding = "UTF-8"
  )
  paths["manifest"] <- manifest_path

  expected_names <- stpd_multitrack_review_export_filenames()
  if (!identical(sort(basename(paths), method = "radix"),
                 sort(expected_names, method = "radix")) ||
      !all(file.exists(paths))) {
    stop("The staged Phase 2B artifact set is incomplete.", call. = FALSE)
  }
  stpd_multitrack_review_cleanup_exports(out_dir)
  # The final manifest is the completion marker.  It is committed only after
  # all eight data tables and the canonical RDS are in their final locations.
  commit_order <- c(
    setdiff(names(paths), c("rds", "manifest")), "rds", "manifest"
  )
  paths <- paths[commit_order]
  final_paths <- file.path(out_dir, basename(paths))
  committed <- file.rename(paths, final_paths)
  if (length(committed) != length(paths) || !all(committed)) {
    stop("Could not commit the complete Phase 2B review artifact set.",
         call. = FALSE)
  }
  names(final_paths) <- names(paths)
  status <- if (identical(validated$lifecycle_status, "active")) {
    "written"
  } else {
    "written_stale_history_only"
  }
  stpd_multitrack_review_export_result(final_paths, status = status)
}

#' Export the auditable Phase 2B Review product
#'
#' The writer is fail-soft: invalid persisted Phase 2B state is removed from
#' the fixed Phase 2B export paths, recorded in Methodological_warnings.txt
#' when that file exists, and never interrupts an already completed legacy or
#' automatic Preview export.
#'
#' @param ds Dataset that may contain `results$multitrack_review`.
#' @param out_dir Export directory shared with the legacy bundle.
#' @return A named character vector of written paths with status attributes, or
#'   an empty status-bearing vector when Phase 2B is absent or invalid.
#' @export
stpd_write_multitrack_review <- function(ds, out_dir) {
  product <- if (is.list(ds) && is.list(ds$results)) {
    ds$results[[stpd_multitrack_review_result_key()]] %||% NULL
  } else {
    NULL
  }
  if (is.null(product)) {
    cleanup_error <- tryCatch(
      {
        stpd_multitrack_review_cleanup_exports(out_dir)
        NULL
      },
      error = function(e) e
    )
    if (is.null(cleanup_error)) {
      return(invisible(stpd_multitrack_review_export_result(
        status = "not_present"
      )))
    }
    message <- conditionMessage(cleanup_error)
    tryCatch(
      stpd_multitrack_review_append_export_warning(
        out_dir, "cleanup_failed", "multitrack_review_cleanup_failed", message
      ),
      error = function(e) NULL
    )
    tryCatch(
      warning(
        paste0(
          "[multitrack_review_export_cleanup_failed] ", message,
          " Legacy and automatic Preview export continue, but stale Phase 2B paths may remain."
        ),
        call. = FALSE
      ),
      error = function(e) NULL
    )
    return(invisible(stpd_multitrack_review_export_result(
      status = "cleanup_failed", code = "multitrack_review_cleanup_failed",
      message = message
    )))
  }

  tryCatch(
    stpd_write_multitrack_review_strict(ds, out_dir),
    error = function(e) {
      cleanup_error <- tryCatch(
        {
          stpd_multitrack_review_cleanup_exports(out_dir)
          NULL
        },
        error = function(cleanup_condition) cleanup_condition
      )
      invalid <- inherits(
        e, "stpd_multitrack_review_export_validation_error"
      )
      status <- if (invalid) {
        "skipped_invalid_persisted_review"
      } else {
        "skipped_review_write_error"
      }
      code <- if (invalid) {
        stpd_multitrack_review_chr(
          e$code, "multitrack_review_export_validation_failed"
        )
      } else {
        "multitrack_review_export_write_failed"
      }
      message <- conditionMessage(e)
      if (!is.null(cleanup_error)) {
        status <- "cleanup_failed"
        code <- "multitrack_review_cleanup_failed"
        message <- paste(
          message, "Cleanup also failed:", conditionMessage(cleanup_error)
        )
      }
      tryCatch(
        stpd_multitrack_review_append_export_warning(
          out_dir, status, code, message
        ),
        error = function(e) NULL
      )
      # options(warn = 2) must not allow this optional export to abort the
      # already completed legacy/Preview export path.
      tryCatch(
        warning(
          paste0(
            "[multitrack_review_export_skipped] ", message,
            " Legacy and automatic Preview exports continue without Phase 2B artifacts."
          ),
          call. = FALSE
        ),
        error = function(e) NULL
      )
      invisible(stpd_multitrack_review_export_result(
        status = status, code = code, message = message
      ))
    }
  )
}
