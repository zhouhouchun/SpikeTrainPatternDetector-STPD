# Flag-gated, non-authoritative public preview of the Phase 1 multi-track
# shadows.  This module materializes additive result tables only.  It never
# changes the established pattern_auto projection or either Phase 1 ledger.

stpd_multitrack_preview_parameter_path <- function() {
  "spiketrainpattern.multitrack_preview"
}

stpd_multitrack_preview_schema_version <- function() {
  "stpd_multitrack_public_preview_v1"
}

stpd_multitrack_preview_block <- function(params = NULL) {
  defaults <- (stpd_product_schema_defaults() %||% list())$multitrack_preview
  if (is.null(defaults) || !is.list(defaults)) {
    defaults <- list(
      enabled = FALSE,
      schema_version = stpd_multitrack_preview_schema_version()
    )
  }
  configured <- if (is.null(params)) {
    list()
  } else {
    stpd_path_get(
      params, stpd_multitrack_preview_parameter_path(), default = list()
    )
  }
  if (!is.list(configured)) {
    stop(
      "spiketrainpattern.multitrack_preview must be a named parameter block.",
      call. = FALSE
    )
  }
  stpd_fill_defaults(configured, defaults)
}

stpd_multitrack_preview_policy_hash <- function(params = NULL) {
  block <- stpd_multitrack_preview_block(params)
  normalized <- if (exists("stpd_params_hash_normalize", mode = "function")) {
    stpd_params_hash_normalize(block)
  } else {
    block
  }
  digest::digest(normalized, algo = "sha256", serialize = TRUE)
}

# Compatibility alias for the parameter-report integration added in Phase 2A.
stpd_multitrack_preview_hash <- stpd_multitrack_preview_policy_hash

stpd_multitrack_preview_policy <- function(params = NULL) {
  block <- stpd_multitrack_preview_block(params)
  enabled <- block$enabled
  if (length(enabled) != 1L || is.na(enabled) || !is.logical(enabled)) {
    stop(
      "spiketrainpattern.multitrack_preview.enabled must be one non-missing logical value.",
      call. = FALSE
    )
  }
  schema_version <- as.character(block$schema_version %||% "")[1]
  expected <- stpd_multitrack_preview_schema_version()
  if (!identical(schema_version, expected)) {
    stop(
      paste0(
        "Unsupported multi-track preview schema_version '", schema_version,
        "'. Expected '", expected,
        "'. Update the implementation and schema version together."
      ),
      call. = FALSE
    )
  }
  list(
    enabled = isTRUE(enabled),
    schema_version = schema_version,
    authoritative = FALSE,
    parameter_path = stpd_multitrack_preview_parameter_path(),
    policy_hash = stpd_multitrack_preview_policy_hash(params),
    policy_hash_algorithm = "SHA-256"
  )
}

stpd_multitrack_preview_abort <- function(code, message) {
  condition <- structure(
    list(
      message = as.character(message)[1],
      call = NULL,
      code = as.character(code)[1]
    ),
    class = c("stpd_multitrack_preview_error", "error", "condition")
  )
  stop(condition)
}

stpd_multitrack_preview_chr <- function(x, default = "") {
  out <- as.character(x %||% default)[1]
  if (length(out) == 0L || is.na(out)) default else out
}

stpd_multitrack_preview_int <- function(x, default = NA_integer_) {
  out <- suppressWarnings(as.integer(x))[1]
  if (length(out) == 0L || is.na(out)) default else out
}

stpd_multitrack_preview_num <- function(x, default = NA_real_) {
  out <- suppressWarnings(as.numeric(x))[1]
  if (length(out) == 0L || is.na(out)) default else out
}

stpd_multitrack_preview_lgl <- function(x, default = FALSE) {
  out <- suppressWarnings(as.logical(x))[1]
  if (length(out) == 0L || is.na(out)) default else out
}

stpd_multitrack_preview_interval_id <- function(
    train, interval_kind, candidate_key, start_isi, end_isi) {
  payload <- paste(
    stpd_multitrack_preview_schema_version(),
    stpd_multitrack_preview_chr(train),
    stpd_multitrack_preview_chr(interval_kind),
    stpd_multitrack_preview_chr(candidate_key),
    stpd_multitrack_preview_int(start_isi),
    stpd_multitrack_preview_int(end_isi),
    sep = "\u001f"
  )
  paste0(
    "mtpi_",
    digest::digest(payload, algo = "sha256", serialize = FALSE)
  )
}

stpd_multitrack_preview_relationship_id <- function(
    train, type, rule, source_interval_id, target_interval_id,
    overlap_start_isi, overlap_end_isi) {
  payload <- paste(
    stpd_multitrack_preview_schema_version(), train, type, rule,
    source_interval_id, target_interval_id, overlap_start_isi,
    overlap_end_isi, sep = "\u001f"
  )
  paste0(
    "mtpr_",
    digest::digest(payload, algo = "sha256", serialize = FALSE)
  )
}

stpd_multitrack_preview_empty_intervals <- function() {
  data.frame(
    schema_version = character(),
    policy_hash = character(),
    run_id = character(),
    params_hash = character(),
    authoritative = logical(),
    train = character(),
    interval_id = character(),
    interval_kind = character(),
    candidate_id = character(),
    candidate_key = character(),
    root_candidate_id = character(),
    parent_interval_id = character(),
    candidate_layer = character(),
    candidate_source = character(),
    semantic_track = character(),
    label = character(),
    gap_semantics = character(),
    hard_for_event = logical(),
    hard_for_state_direct_support = logical(),
    envelope_bridge_eligible = logical(),
    candidate_decision = character(),
    candidate_reason_code = character(),
    start_isi = integer(),
    end_isi = integer(),
    n_isi = integer(),
    start_time_sec = double(),
    end_time_sec = double(),
    selected_within_track = logical(),
    active_in_preview = logical(),
    activity_status = character(),
    track_selection_status = character(),
    review_target_track = character(),
    review_target_label = character(),
    review_promotion_required = logical(),
    review_policy_status = character(),
    eligible_for_primary_event_metrics = logical(),
    eligible_for_state_split = logical(),
    eligible_for_hfs_dominance = logical(),
    split_kind = character(),
    gate_evaluated = logical(),
    provisional_gate_pass = logical(),
    provisional_gate_status = character(),
    source_track_policy_version = character(),
    stringsAsFactors = FALSE
  )
}

stpd_multitrack_preview_empty_relationships <- function() {
  data.frame(
    schema_version = character(),
    policy_hash = character(),
    run_id = character(),
    params_hash = character(),
    authoritative = logical(),
    train = character(),
    relationship_id = character(),
    relationship_type = character(),
    compatibility_rule = character(),
    source_interval_id = character(),
    target_interval_id = character(),
    source_candidate_id = character(),
    target_candidate_id = character(),
    source_shadow_candidate_id = character(),
    target_shadow_candidate_id = character(),
    source_track = character(),
    target_track = character(),
    overlap_start_isi = integer(),
    overlap_end_isi = integer(),
    overlap_isi_n = integer(),
    source_overlap_fraction = double(),
    target_overlap_fraction = double(),
    intersection_over_union = double(),
    policy_role = character(),
    track_policy_version = character(),
    non_destructive = logical(),
    stringsAsFactors = FALSE
  )
}

stpd_multitrack_preview_empty_per_isi <- function() {
  data.frame(
    schema_version = character(),
    policy_hash = character(),
    run_id = character(),
    params_hash = character(),
    authoritative = logical(),
    train = character(),
    isi_index = integer(),
    timestamp_sec = double(),
    ISI_sec = double(),
    pattern_auto_event_preview = character(),
    pattern_auto_event_preview_interval_id = character(),
    pattern_auto_state_preview = character(),
    pattern_auto_state_preview_interval_id = character(),
    pattern_auto_gap_preview = character(),
    pattern_auto_gap_preview_interval_id = character(),
    pattern_auto_review_preview = character(),
    pattern_auto_review_preview_interval_id = character(),
    review_candidate_count = integer(),
    stringsAsFactors = FALSE
  )
}

stpd_multitrack_preview_empty_hfs_dominance <- function() {
  base <- stpd_multitrack_compatibility_empty_tables()$hfs_dominance
  prefix <- data.frame(
    schema_version = character(),
    policy_hash = character(),
    run_id = character(),
    params_hash = character(),
    authoritative = logical(),
    train = character(),
    hfs_interval_id = character(),
    contributor_interval_ids = character(),
    stringsAsFactors = FALSE
  )
  dplyr::bind_cols(prefix, base)
}

stpd_multitrack_preview_empty_invariants <- function() {
  data.frame(
    schema_version = character(),
    policy_hash = character(),
    run_id = character(),
    params_hash = character(),
    authoritative = logical(),
    invariant_id = character(),
    train = character(),
    check_name = character(),
    status = character(),
    severity = character(),
    message = character(),
    stringsAsFactors = FALSE
  )
}

stpd_multitrack_preview_invariant_row <- function(
    policy, run_id, params_hash, check_name, status = "pass",
    severity = "information", message = "", train = "") {
  payload <- paste(
    policy$schema_version, policy$policy_hash, run_id, train, check_name,
    status, severity, message, sep = "\u001f"
  )
  data.frame(
    schema_version = policy$schema_version,
    policy_hash = policy$policy_hash,
    run_id = run_id,
    params_hash = params_hash,
    authoritative = FALSE,
    invariant_id = paste0(
      "mtpv_", digest::digest(payload, algo = "sha256", serialize = FALSE)
    ),
    train = train,
    check_name = check_name,
    status = status,
    severity = severity,
    message = message,
    stringsAsFactors = FALSE
  )
}

stpd_multitrack_preview_empty_tables <- function() {
  list(
    intervals = stpd_multitrack_preview_empty_intervals(),
    relationships = stpd_multitrack_preview_empty_relationships(),
    per_isi = stpd_multitrack_preview_empty_per_isi(),
    hfs_dominance_evidence = stpd_multitrack_preview_empty_hfs_dominance(),
    invariants = stpd_multitrack_preview_empty_invariants()
  )
}

stpd_multitrack_preview_table_hash <- function(x) {
  normalized <- if (exists("stpd_params_hash_normalize", mode = "function")) {
    stpd_params_hash_normalize(x)
  } else {
    x
  }
  digest::digest(normalized, algo = "sha256", serialize = TRUE)
}

stpd_multitrack_preview_column_types <- function(x) {
  if (!is.data.frame(x) || ncol(x) == 0L) return("")
  paste0(
    names(x), ":", vapply(x, typeof, character(1)),
    collapse = ";"
  )
}

stpd_multitrack_preview_table_export_filenames <- function() {
  c(
    metadata = "Multitrack_preview_metadata.csv",
    intervals = "Multitrack_preview_intervals.csv",
    relationships = "Multitrack_preview_relationships.csv",
    per_isi = "Multitrack_preview_per_isi.csv",
    hfs_dominance_evidence =
      "Multitrack_preview_hfs_dominance_evidence.csv",
    invariants = "Multitrack_preview_invariants.csv"
  )
}

stpd_multitrack_preview_manifest <- function(
    tables, policy, run_id, params_hash) {
  file_names <- stpd_multitrack_preview_table_export_filenames()
  table_names <- names(file_names)
  data.frame(
    schema_version = rep(policy$schema_version, length(table_names)),
    policy_hash = rep(policy$policy_hash, length(table_names)),
    run_id = rep(run_id, length(table_names)),
    params_hash = rep(params_hash, length(table_names)),
    authoritative = rep(FALSE, length(table_names)),
    table_name = table_names,
    file_name = unname(file_names),
    row_count = vapply(table_names, function(name) {
      as.integer(nrow(tables[[name]]))
    }, integer(1)),
    column_count = vapply(table_names, function(name) {
      as.integer(ncol(tables[[name]]))
    }, integer(1)),
    column_types = vapply(table_names, function(name) {
      stpd_multitrack_preview_column_types(tables[[name]])
    }, character(1)),
    table_sha256 = vapply(table_names, function(name) {
      stpd_multitrack_preview_table_hash(tables[[name]])
    }, character(1)),
    stringsAsFactors = FALSE
  )
}

stpd_multitrack_preview_metadata <- function(
    policy, run_id, params_hash, selected_trains, status, failure_code,
    failure_message, tables, source_shadow_versions = character(),
    source_compatibility_versions = character(), label_blind = FALSE) {
  collapse_versions <- function(x) {
    x <- sort(unique(as.character(x)), method = "radix")
    x <- x[!is.na(x) & nzchar(x)]
    paste(x, collapse = ";")
  }
  data.frame(
    schema_version = policy$schema_version,
    policy_hash = policy$policy_hash,
    policy_hash_algorithm = "SHA-256",
    policy_parameter_path = stpd_multitrack_preview_parameter_path(),
    run_id = run_id,
    params_hash = params_hash,
    enabled = isTRUE(policy$enabled),
    authoritative = FALSE,
    label_blind = isTRUE(label_blind),
    detection_mode = if (isTRUE(label_blind)) {
      "label_blind"
    } else {
      "manual_aware"
    },
    automatic_tracks_only = TRUE,
    manual_final_migration_pending = TRUE,
    canonical_typed_artifact = "Multitrack_preview.rds",
    csv_serialization_role = "human_readable_schema_described_by_manifest",
    materialization_status = status,
    failure_code = failure_code,
    failure_message = failure_message,
    selected_train_n = as.integer(length(selected_trains)),
    selected_trains = paste(selected_trains, collapse = ";"),
    source_shadow_policy_versions = collapse_versions(source_shadow_versions),
    source_compatibility_policy_versions =
      collapse_versions(source_compatibility_versions),
    legacy_projection_changed = FALSE,
    intervals_n = as.integer(nrow(tables$intervals)),
    relationships_n = as.integer(nrow(tables$relationships)),
    per_isi_n = as.integer(nrow(tables$per_isi)),
    hfs_dominance_evidence_n =
      as.integer(nrow(tables$hfs_dominance_evidence)),
    invariants_n = as.integer(nrow(tables$invariants)),
    stringsAsFactors = FALSE
  )
}

stpd_multitrack_preview_metadata_types <- function() {
  c(
    schema_version = "character",
    policy_hash = "character",
    policy_hash_algorithm = "character",
    policy_parameter_path = "character",
    run_id = "character",
    params_hash = "character",
    enabled = "logical",
    authoritative = "logical",
    label_blind = "logical",
    detection_mode = "character",
    automatic_tracks_only = "logical",
    manual_final_migration_pending = "logical",
    canonical_typed_artifact = "character",
    csv_serialization_role = "character",
    materialization_status = "character",
    failure_code = "character",
    failure_message = "character",
    selected_train_n = "integer",
    selected_trains = "character",
    source_shadow_policy_versions = "character",
    source_compatibility_policy_versions = "character",
    legacy_projection_changed = "logical",
    intervals_n = "integer",
    relationships_n = "integer",
    per_isi_n = "integer",
    hfs_dominance_evidence_n = "integer",
    invariants_n = "integer"
  )
}

stpd_multitrack_preview_finalize <- function(
    policy, run_id, params_hash, selected_trains, tables,
    status = "materialized", failure_code = "", failure_message = "",
    source_shadow_versions = character(),
    source_compatibility_versions = character(), label_blind = FALSE) {
  metadata <- stpd_multitrack_preview_metadata(
    policy = policy,
    run_id = run_id,
    params_hash = params_hash,
    selected_trains = selected_trains,
    status = status,
    failure_code = failure_code,
    failure_message = failure_message,
    tables = tables,
    source_shadow_versions = source_shadow_versions,
    source_compatibility_versions = source_compatibility_versions,
    label_blind = label_blind
  )
  manifest_tables <- c(list(metadata = metadata), tables)
  manifest <- stpd_multitrack_preview_manifest(
    manifest_tables, policy, run_id, params_hash
  )
  structure(
    list(
      metadata = metadata,
      intervals = tables$intervals,
      relationships = tables$relationships,
      per_isi = tables$per_isi,
      hfs_dominance_evidence = tables$hfs_dominance_evidence,
      invariants = tables$invariants,
      table_manifest = manifest
    ),
    class = c("stpd_multitrack_public_preview", "list")
  )
}

stpd_multitrack_preview_failed <- function(
    policy, run_id, params_hash, selected_trains, code, message,
    label_blind = FALSE) {
  tables <- stpd_multitrack_preview_empty_tables()
  tables$invariants <- stpd_multitrack_preview_invariant_row(
    policy = policy,
    run_id = run_id,
    params_hash = params_hash,
    check_name = "preview_materialization",
    status = "failed_closed",
    severity = "error",
    message = message
  )
  stpd_multitrack_preview_finalize(
    policy = policy,
    run_id = run_id,
    params_hash = params_hash,
    selected_trains = selected_trains,
    tables = tables,
    status = "failed_closed",
    failure_code = code,
    failure_message = message,
    label_blind = label_blind
  )
}

stpd_multitrack_preview_run_identity <- function(
    ds, run_id = NULL, params_hash = NULL) {
  metadata <- (ds$results %||% list())$run_metadata %||% data.frame()
  metadata_public <-
    (ds$results %||% list())$run_metadata_public %||% data.frame()
  first_nonempty <- function(...) {
    values <- list(...)
    for (value in values) {
      value <- as.character(value %||% "")[1]
      if (length(value) > 0L && !is.na(value) && nzchar(value)) return(value)
    }
    ""
  }
  rid <- first_nonempty(
    run_id,
    if (is.data.frame(metadata) && "run_id" %in% names(metadata)) metadata$run_id else "",
    if (is.data.frame(metadata_public) && "run_id" %in% names(metadata_public)) metadata_public$run_id else ""
  )
  phash <- first_nonempty(
    params_hash,
    if (is.data.frame(metadata) && "params_hash" %in% names(metadata)) metadata$params_hash else "",
    if (is.data.frame(metadata_public) && "params_hash" %in% names(metadata_public)) metadata_public$params_hash else "",
    tryCatch(stpd_multitrack_source_params_hash(ds$params_effective), error = function(e) "")
  )
  list(run_id = rid, params_hash = phash)
}

stpd_multitrack_preview_time <- function(dat, index, default = NA_real_) {
  if (!is.data.frame(dat) || !("timestamp_sec" %in% names(dat)) ||
      !is.finite(index) || index < 1L || index > nrow(dat)) return(default)
  stpd_multitrack_preview_num(dat$timestamp_sec[index], default)
}

stpd_multitrack_preview_candidate_rows <- function(
    train, dat, shadow, compatibility, policy, run_id, params_hash) {
  candidates <- as.data.frame(shadow$candidates, stringsAsFactors = FALSE)
  if (nrow(candidates) == 0L) return(stpd_multitrack_preview_empty_intervals())
  required <- c(
    "candidate_id", "final_label", "start_isi", "end_isi",
    "semantic_track", "selected_within_track"
  )
  missing <- setdiff(required, names(candidates))
  if (length(missing) > 0L) {
    stpd_multitrack_preview_abort(
      "candidate_schema_missing",
      paste0(
        "Phase 1A candidate ledger is missing: ", paste(missing, collapse = ", ")
      )
    )
  }
  candidates <- candidates[
    as.character(candidates$semantic_track) %in%
      c("event", "state", "gap", "review"),
    , drop = FALSE
  ]
  if (nrow(candidates) == 0L) return(stpd_multitrack_preview_empty_intervals())

  parent_table <- as.data.frame(
    compatibility$state_parents %||% data.frame(),
    stringsAsFactors = FALSE
  )
  rows <- vector("list", nrow(candidates))
  for (i in seq_len(nrow(candidates))) {
    candidate <- candidates[i, , drop = FALSE]
    track <- stpd_multitrack_preview_chr(candidate$semantic_track)
    label <- stpd_multitrack_shadow_normalize_label(candidate$final_label)
    start <- stpd_multitrack_preview_int(candidate$start_isi)
    end <- stpd_multitrack_preview_int(candidate$end_isi)
    if (!is.finite(start) || !is.finite(end) || start < 2L || end < start ||
        end > nrow(dat)) {
      stpd_multitrack_preview_abort(
        "invalid_interval_geometry",
        paste0(
          "Invalid ", track, " candidate geometry in train '", train,
          "': ", start, "-", end, "."
        )
      )
    }
    key <- stpd_multitrack_compatibility_candidate_key(candidate)
    interval_id <- stpd_multitrack_preview_interval_id(
      train, "candidate", key, start, end
    )
    selected <- stpd_multitrack_preview_lgl(candidate$selected_within_track)
    split_kind <- ""
    active <- selected && track %in% c("event", "gap", "review")
    activity <- if (active) {
      "selected_track_winner"
    } else if (selected && identical(track, "state")) {
      "selected_state_pending_compatibility"
    } else {
      "not_selected_within_track"
    }

    if (identical(track, "state") && selected) {
      parent_key <- if ("state_candidate_key" %in% names(parent_table)) {
        as.character(parent_table$state_candidate_key)
      } else character(nrow(parent_table))
      hit <- which(parent_key == key)
      if (length(hit) != 1L) {
        stpd_multitrack_preview_abort(
          "state_parent_resolution_failed",
          paste0(
            "Selected State candidate in train '", train,
            "' does not resolve to exactly one Phase 1B state parent: ", key
          )
        )
      }
      split_kind <- stpd_multitrack_preview_chr(
        parent_table$split_kind[hit], "none"
      )
      parent_selection_preserved <- stpd_multitrack_preview_lgl(
        parent_table$selected_within_track_preserved[hit]
      )
      parent_consumed <- stpd_multitrack_preview_lgl(
        parent_table$parent_consumed_in_provisional_policy[hit]
      )
      if (!parent_selection_preserved ||
          !identical(parent_consumed, !identical(split_kind, "none"))) {
        stpd_multitrack_preview_abort(
          "state_parent_lineage_invalid",
          paste0(
            "Phase 1B State-parent lineage is inconsistent in train '",
            train, "': ", key, "."
          )
        )
      }
      active <- identical(split_kind, "none")
      activity <- if (active) {
        "selected_unsplit_state_parent"
      } else {
        paste0("inactive_split_state_parent__", split_kind)
      }
    }

    review <- identical(track, "review")
    is_gap <- identical(track, "gap") && identical(label, "pause")
    gap_semantics <- if (is_gap) {
      stpd_multitrack_preview_chr(candidate$gap_semantics, "canonical_pause")
    } else {
      ""
    }
    rows[[i]] <- data.frame(
      schema_version = policy$schema_version,
      policy_hash = policy$policy_hash,
      run_id = run_id,
      params_hash = params_hash,
      authoritative = FALSE,
      train = train,
      interval_id = interval_id,
      interval_kind = "candidate",
      candidate_id = stpd_multitrack_preview_chr(candidate$candidate_id),
      candidate_key = key,
      root_candidate_id = stpd_multitrack_preview_chr(candidate$candidate_id),
      parent_interval_id = "",
      candidate_layer = stpd_multitrack_preview_chr(candidate$candidate_layer),
      candidate_source = stpd_multitrack_preview_chr(candidate$candidate_source),
      semantic_track = track,
      label = label,
      gap_semantics = gap_semantics,
      hard_for_event = if (is_gap) stpd_multitrack_preview_lgl(
        candidate$hard_for_event, gap_semantics != "ambiguous_gap"
      ) else FALSE,
      hard_for_state_direct_support = if (is_gap) stpd_multitrack_preview_lgl(
        candidate$hard_for_state_direct_support,
        gap_semantics != "ambiguous_gap"
      ) else FALSE,
      envelope_bridge_eligible = if (is_gap) stpd_multitrack_preview_lgl(
        candidate$envelope_bridge_eligible,
        gap_semantics == "contextual_interburst_pause"
      ) else FALSE,
      candidate_decision = if (is_gap) stpd_multitrack_preview_chr(
        candidate$pause_candidate_decision, "accepted"
      ) else "",
      candidate_reason_code = if (is_gap) stpd_multitrack_preview_chr(
        candidate$pause_candidate_reason_code, "legacy_pause_candidate"
      ) else "",
      start_isi = start,
      end_isi = end,
      n_isi = as.integer(end - start + 1L),
      start_time_sec = stpd_multitrack_preview_time(dat, start - 1L),
      end_time_sec = stpd_multitrack_preview_time(dat, end),
      selected_within_track = selected,
      active_in_preview = active,
      activity_status = activity,
      track_selection_status = stpd_multitrack_preview_chr(
        candidate$track_selection_status
      ),
      review_target_track = stpd_multitrack_preview_chr(
        candidate$review_target_track
      ),
      review_target_label = stpd_multitrack_preview_chr(
        candidate$review_target_label
      ),
      review_promotion_required = stpd_multitrack_preview_lgl(
        candidate$review_promotion_required
      ),
      review_policy_status = stpd_multitrack_preview_chr(
        candidate$review_policy_status
      ),
      eligible_for_primary_event_metrics =
        active && identical(track, "event") && !review,
      eligible_for_state_split =
        active && track %in% c("event", "gap") && !review,
      eligible_for_hfs_dominance =
        active && identical(track, "event") && !review,
      split_kind = split_kind,
      gate_evaluated = FALSE,
      provisional_gate_pass = FALSE,
      provisional_gate_status = "",
      source_track_policy_version = stpd_multitrack_preview_chr(
        shadow$policy_version
      ),
      stringsAsFactors = FALSE
    )
  }
  dplyr::bind_rows(stpd_multitrack_preview_empty_intervals(), rows)
}

stpd_multitrack_preview_fragment_rows <- function(
    train, dat, shadow, compatibility, candidate_intervals, policy,
    run_id, params_hash) {
  fragments <- as.data.frame(
    compatibility$state_fragments %||% data.frame(),
    stringsAsFactors = FALSE
  )
  if (nrow(fragments) == 0L || !("split_kind" %in% names(fragments))) {
    return(stpd_multitrack_preview_empty_intervals())
  }
  fragments <- fragments[
    as.character(fragments$split_kind) != "none",
    , drop = FALSE
  ]
  if (nrow(fragments) == 0L) return(stpd_multitrack_preview_empty_intervals())

  rows <- vector("list", nrow(fragments))
  for (i in seq_len(nrow(fragments))) {
    fragment <- fragments[i, , drop = FALSE]
    start <- stpd_multitrack_preview_int(fragment$start_isi)
    end <- stpd_multitrack_preview_int(fragment$end_isi)
    if (!is.finite(start) || !is.finite(end) || start < 2L || end < start ||
        end > nrow(dat)) {
      stpd_multitrack_preview_abort(
        "invalid_fragment_geometry",
        paste0(
          "Invalid State fragment geometry in train '", train, "': ",
          start, "-", end, "."
        )
      )
    }
    root_key <- stpd_multitrack_preview_chr(fragment$root_candidate_key)
    parent_hit <- which(
      candidate_intervals$semantic_track == "state" &
        candidate_intervals$candidate_key == root_key
    )
    if (length(parent_hit) != 1L) {
      stpd_multitrack_preview_abort(
        "fragment_parent_resolution_failed",
        paste0(
          "State fragment in train '", train,
          "' does not resolve to exactly one public parent: ", root_key
        )
      )
    }
    parent <- candidate_intervals[parent_hit, , drop = FALSE]
    fragment_split_kind <- stpd_multitrack_preview_chr(fragment$split_kind)
    if (!identical(parent$semantic_track, "state") ||
        !isTRUE(parent$selected_within_track) ||
        isTRUE(parent$active_in_preview) ||
        !nzchar(fragment_split_kind) || identical(fragment_split_kind, "none") ||
        !identical(parent$split_kind, fragment_split_kind) ||
        !stpd_multitrack_preview_lgl(
          fragment$root_phase1a_selection_preserved
        ) ||
        !stpd_multitrack_preview_lgl(fragment$provisional_fragment_only)) {
      stpd_multitrack_preview_abort(
        "state_fragment_lineage_invalid",
        paste0(
          "State fragment does not descend from a selected consumed parent in train '",
          train, "': ", root_key, "."
        )
      )
    }
    fragment_key <- paste0(root_key, "|fragment|", start, "|", end)
    fragment_id <- stpd_multitrack_preview_chr(
      fragment$fragment_candidate_id
    )
    interval_id <- stpd_multitrack_preview_interval_id(
      train, "state_fragment", fragment_key, start, end
    )
    evaluated <- stpd_multitrack_preview_lgl(fragment$gate_evaluated)
    gate_pass <- stpd_multitrack_preview_lgl(
      fragment$provisional_gate_pass
    )
    active <- evaluated && gate_pass
    status <- if (!evaluated) {
      "inactive_fragment_regate_unresolved"
    } else if (!gate_pass) {
      "inactive_fragment_regate_failed"
    } else {
      "active_fragment_regate_passed"
    }
    rows[[i]] <- data.frame(
      schema_version = policy$schema_version,
      policy_hash = policy$policy_hash,
      run_id = run_id,
      params_hash = params_hash,
      authoritative = FALSE,
      train = train,
      interval_id = interval_id,
      interval_kind = "state_fragment",
      candidate_id = fragment_id,
      candidate_key = fragment_key,
      root_candidate_id = stpd_multitrack_preview_chr(
        fragment$root_candidate_id
      ),
      parent_interval_id = candidate_intervals$interval_id[parent_hit],
      candidate_layer = "phase1b_state_fragment",
      candidate_source = "multitrack_compatibility_shadow",
      semantic_track = "state",
      label = stpd_multitrack_shadow_normalize_label(fragment$state_label),
      gap_semantics = "",
      hard_for_event = FALSE,
      hard_for_state_direct_support = FALSE,
      envelope_bridge_eligible = FALSE,
      candidate_decision = "",
      candidate_reason_code = "",
      start_isi = start,
      end_isi = end,
      n_isi = as.integer(end - start + 1L),
      start_time_sec = stpd_multitrack_preview_time(dat, start - 1L),
      end_time_sec = stpd_multitrack_preview_time(dat, end),
      selected_within_track = TRUE,
      active_in_preview = active,
      activity_status = status,
      track_selection_status =
        "derived_from_selected_state_parent_after_compatibility_split",
      review_target_track = "",
      review_target_label = "",
      review_promotion_required = FALSE,
      review_policy_status = "",
      eligible_for_primary_event_metrics = FALSE,
      eligible_for_state_split = FALSE,
      eligible_for_hfs_dominance = FALSE,
      split_kind = fragment_split_kind,
      gate_evaluated = evaluated,
      provisional_gate_pass = gate_pass,
      provisional_gate_status = stpd_multitrack_preview_chr(
        fragment$provisional_gate_status
      ),
      source_track_policy_version = stpd_multitrack_preview_chr(
        compatibility$policy_version
      ),
      stringsAsFactors = FALSE
    )
  }
  dplyr::bind_rows(stpd_multitrack_preview_empty_intervals(), rows)
}

stpd_multitrack_preview_find_interval <- function(
    intervals, candidate_key = "", candidate_id = "", aliases = NULL,
    context = "relationship endpoint") {
  key <- stpd_multitrack_preview_chr(candidate_key)
  id <- stpd_multitrack_preview_chr(candidate_id)
  original_key <- key
  alias_id <- ""
  if (nzchar(key) && !is.null(aliases) && key %in% names(aliases)) {
    key <- unname(aliases[[key]])
    alias_ids <- attr(aliases, "candidate_ids", exact = TRUE)
    if (!is.null(alias_ids) && original_key %in% names(alias_ids)) {
      alias_id <- stpd_multitrack_preview_chr(alias_ids[[original_key]])
    }
  }
  hit <- integer()
  if (nzchar(key)) {
    hit <- which(intervals$candidate_key == key)
    if (nzchar(id) && length(hit) > 0L) {
      allowed <- intervals$candidate_id[hit] == id
      if (nzchar(alias_id)) allowed <- allowed | id == alias_id
      hit <- hit[allowed]
    }
  } else if (nzchar(id)) {
    hit <- which(intervals$candidate_id == id)
  }
  if (length(hit) != 1L) {
    stpd_multitrack_preview_abort(
      "relationship_foreign_key_failed",
      paste0(
        "Could not resolve exactly one ", context, " (key='", key,
        "', id='", id, "')."
      )
    )
  }
  hit
}

stpd_multitrack_preview_fragment_aliases <- function(compatibility) {
  fragments <- as.data.frame(
    compatibility$state_fragments %||% data.frame(),
    stringsAsFactors = FALSE
  )
  if (nrow(fragments) == 0L ||
      !all(c("split_kind", "root_candidate_key", "start_isi", "end_isi") %in%
        names(fragments))) return(character())
  fragments <- fragments[as.character(fragments$split_kind) == "none", , drop = FALSE]
  if (nrow(fragments) == 0L) return(character())
  keys <- paste0(
    fragments$root_candidate_key, "|fragment|",
    as.integer(fragments$start_isi), "|", as.integer(fragments$end_isi)
  )
  values <- as.character(fragments$root_candidate_key)
  out <- stats::setNames(values, keys)
  attr(out, "candidate_ids") <- stats::setNames(
    as.character(fragments$fragment_candidate_id), keys
  )
  out
}

stpd_multitrack_preview_relationship_rows <- function(
    train, compatibility, intervals, policy, run_id, params_hash) {
  source <- as.data.frame(
    compatibility$relationships %||% data.frame(),
    stringsAsFactors = FALSE
  )
  if (nrow(source) == 0L) return(stpd_multitrack_preview_empty_relationships())
  required <- c(
    "relationship_type", "compatibility_rule", "source_candidate_id",
    "source_candidate_key", "target_candidate_id", "target_candidate_key"
  )
  missing <- setdiff(required, names(source))
  if (length(missing) > 0L) {
    stpd_multitrack_preview_abort(
      "relationship_schema_missing",
      paste0("Phase 1B relationships are missing: ", paste(missing, collapse = ", "))
    )
  }
  aliases <- stpd_multitrack_preview_fragment_aliases(compatibility)
  rows <- vector("list", nrow(source))
  for (i in seq_len(nrow(source))) {
    relationship <- source[i, , drop = FALSE]
    source_hit <- stpd_multitrack_preview_find_interval(
      intervals,
      relationship$source_candidate_key,
      relationship$source_candidate_id,
      aliases = aliases,
      context = "relationship source"
    )
    target_hit <- stpd_multitrack_preview_find_interval(
      intervals,
      relationship$target_candidate_key,
      relationship$target_candidate_id,
      aliases = aliases,
      context = "relationship target"
    )
    source_interval <- intervals[source_hit, , drop = FALSE]
    target_interval <- intervals[target_hit, , drop = FALSE]
    overlap_start <- max(source_interval$start_isi, target_interval$start_isi)
    overlap_end <- min(source_interval$end_isi, target_interval$end_isi)
    overlap_n <- as.integer(max(0L, overlap_end - overlap_start + 1L))
    if (overlap_n <= 0L) {
      stpd_multitrack_preview_abort(
        "relationship_geometry_invalid",
        paste0(
          "Relationship endpoints do not overlap in train '", train,
          "': ", source_interval$interval_id, " -> ",
          target_interval$interval_id, "."
        )
      )
    }
    union_n <- source_interval$n_isi + target_interval$n_isi - overlap_n
    type <- stpd_multitrack_preview_chr(relationship$relationship_type)
    rule <- stpd_multitrack_preview_chr(relationship$compatibility_rule)
    relationship_id <- stpd_multitrack_preview_relationship_id(
      train, type, rule, source_interval$interval_id,
      target_interval$interval_id, overlap_start, overlap_end
    )
    rows[[i]] <- data.frame(
      schema_version = policy$schema_version,
      policy_hash = policy$policy_hash,
      run_id = run_id,
      params_hash = params_hash,
      authoritative = FALSE,
      train = train,
      relationship_id = relationship_id,
      relationship_type = type,
      compatibility_rule = rule,
      source_interval_id = source_interval$interval_id,
      target_interval_id = target_interval$interval_id,
      source_candidate_id = source_interval$candidate_id,
      target_candidate_id = target_interval$candidate_id,
      source_shadow_candidate_id = stpd_multitrack_preview_chr(
        relationship$source_candidate_id
      ),
      target_shadow_candidate_id = stpd_multitrack_preview_chr(
        relationship$target_candidate_id
      ),
      source_track = source_interval$semantic_track,
      target_track = target_interval$semantic_track,
      overlap_start_isi = as.integer(overlap_start),
      overlap_end_isi = as.integer(overlap_end),
      overlap_isi_n = overlap_n,
      source_overlap_fraction = overlap_n / source_interval$n_isi,
      target_overlap_fraction = overlap_n / target_interval$n_isi,
      intersection_over_union = overlap_n / union_n,
      policy_role = stpd_multitrack_preview_chr(relationship$policy_role),
      track_policy_version = stpd_multitrack_preview_chr(
        relationship$track_policy_version,
        stpd_multitrack_preview_chr(compatibility$policy_version)
      ),
      non_destructive = stpd_multitrack_preview_lgl(
        relationship$non_destructive, TRUE
      ),
      stringsAsFactors = FALSE
    )
  }
  out <- dplyr::bind_rows(stpd_multitrack_preview_empty_relationships(), rows)
  if (anyDuplicated(out$relationship_id)) {
    stpd_multitrack_preview_abort(
      "duplicate_relationship_id",
      paste0("Duplicate public relationship_id in train '", train, "'.")
    )
  }
  out
}

stpd_multitrack_preview_split_ids <- function(x) {
  text <- stpd_multitrack_preview_chr(x)
  if (!nzchar(text)) return(character())
  values <- trimws(unlist(strsplit(text, ";", fixed = TRUE), use.names = FALSE))
  values[nzchar(values)]
}

stpd_multitrack_preview_hfs_rows <- function(
    train, compatibility, intervals, policy, run_id, params_hash) {
  evidence <- as.data.frame(
    compatibility$hfs_dominance %||% data.frame(),
    stringsAsFactors = FALSE
  )
  if (nrow(evidence) == 0L) {
    return(stpd_multitrack_preview_empty_hfs_dominance())
  }
  aliases <- stpd_multitrack_preview_fragment_aliases(compatibility)
  rows <- vector("list", nrow(evidence))
  for (i in seq_len(nrow(evidence))) {
    row <- evidence[i, , drop = FALSE]
    fragment_key <- paste0(
      stpd_multitrack_preview_chr(row$root_hfs_candidate_key),
      "|fragment|", stpd_multitrack_preview_int(row$start_isi), "|",
      stpd_multitrack_preview_int(row$end_isi)
    )
    state_hit <- stpd_multitrack_preview_find_interval(
      intervals,
      candidate_key = fragment_key,
      candidate_id = stpd_multitrack_preview_chr(
        row$hfs_fragment_candidate_id
      ),
      aliases = aliases,
      context = "HFS dominance State interval"
    )
    keys <- stpd_multitrack_preview_split_ids(
      row$contributor_candidate_keys
    )
    ids <- stpd_multitrack_preview_split_ids(
      row$contributor_candidate_ids
    )
    if (length(keys) != length(ids) || anyDuplicated(keys) ||
        anyDuplicated(ids)) {
      stpd_multitrack_preview_abort(
        "hfs_dominance_contributor_identity_mismatch",
        paste0(
          "HFS dominance contributor keys and IDs are not a one-to-one set in train '",
          train, "'."
        )
      )
    }
    contributor_hits <- integer()
    n_contributors <- length(keys)
    if (n_contributors > 0L) {
      for (j in seq_len(n_contributors)) {
        hit <- stpd_multitrack_preview_find_interval(
          intervals,
          candidate_key = keys[j],
          candidate_id = "",
          aliases = aliases,
          context = "HFS dominance Event contributor"
        )
        if (!identical(intervals$semantic_track[hit], "event") ||
            !isTRUE(intervals$active_in_preview[hit]) ||
            !isTRUE(intervals$eligible_for_hfs_dominance[hit])) {
          stpd_multitrack_preview_abort(
            "hfs_dominance_contributor_ineligible",
            paste0(
              "HFS dominance evidence in train '", train,
              "' contains a non-active or non-Event contributor."
            )
          )
        }
        contributor_hits <- c(contributor_hits, hit)
      }
      resolved_ids <- as.character(intervals$candidate_id[contributor_hits])
      if (anyDuplicated(contributor_hits) || !setequal(ids, resolved_ids)) {
        stpd_multitrack_preview_abort(
          "hfs_dominance_contributor_identity_mismatch",
          paste0(
            "HFS dominance contributor IDs do not identify the supplied keys in train '",
            train, "'."
          )
        )
      }
    }
    prefix <- data.frame(
      schema_version = policy$schema_version,
      policy_hash = policy$policy_hash,
      run_id = run_id,
      params_hash = params_hash,
      authoritative = FALSE,
      train = train,
      hfs_interval_id = intervals$interval_id[state_hit],
      contributor_interval_ids = paste(
        intervals$interval_id[contributor_hits], collapse = ";"
      ),
      stringsAsFactors = FALSE
    )
    rows[[i]] <- dplyr::bind_cols(prefix, row)
  }
  dplyr::bind_rows(stpd_multitrack_preview_empty_hfs_dominance(), rows)
}

stpd_multitrack_preview_assert_unique_active_tracks <- function(intervals) {
  active <- intervals[intervals$active_in_preview, , drop = FALSE]
  if (nrow(active) <= 1L) return(invisible(TRUE))
  groups <- split(
    seq_len(nrow(active)),
    paste(active$train, active$semantic_track, sep = "\u001f")
  )
  for (indices in groups) {
    group <- active[indices, , drop = FALSE]
    ord <- order(group$start_isi, group$end_isi, group$interval_id, method = "radix")
    group <- group[ord, , drop = FALSE]
    running_end <- -Inf
    running_id <- ""
    for (i in seq_len(nrow(group))) {
      if (group$start_isi[i] <= running_end) {
        stpd_multitrack_preview_abort(
          "active_same_track_overlap",
          paste0(
            "Active ", group$semantic_track[i], " intervals overlap in train '",
            group$train[i], "': ", running_id, " and ",
            group$interval_id[i], "."
          )
        )
      }
      if (group$end_isi[i] > running_end) {
        running_end <- group$end_isi[i]
        running_id <- group$interval_id[i]
      }
    }
  }
  invisible(TRUE)
}

stpd_multitrack_preview_per_isi_rows <- function(
    train, dat, intervals, policy, run_id, params_hash) {
  n <- nrow(dat)
  if (n == 0L) return(stpd_multitrack_preview_empty_per_isi())
  timestamp <- if ("timestamp_sec" %in% names(dat)) {
    suppressWarnings(as.numeric(dat$timestamp_sec))
  } else rep(NA_real_, n)
  isi <- if ("ISI_sec" %in% names(dat)) {
    suppressWarnings(as.numeric(dat$ISI_sec))
  } else rep(NA_real_, n)
  out <- data.frame(
    schema_version = rep(policy$schema_version, n),
    policy_hash = rep(policy$policy_hash, n),
    run_id = rep(run_id, n),
    params_hash = rep(params_hash, n),
    authoritative = rep(FALSE, n),
    train = rep(train, n),
    isi_index = seq_len(n),
    timestamp_sec = timestamp,
    ISI_sec = isi,
    pattern_auto_event_preview = rep("", n),
    pattern_auto_event_preview_interval_id = rep("", n),
    pattern_auto_state_preview = rep("", n),
    pattern_auto_state_preview_interval_id = rep("", n),
    pattern_auto_gap_preview = rep("", n),
    pattern_auto_gap_preview_interval_id = rep("", n),
    pattern_auto_review_preview = rep("", n),
    pattern_auto_review_preview_interval_id = rep("", n),
    review_candidate_count = integer(n),
    stringsAsFactors = FALSE
  )
  train_intervals <- intervals[intervals$train == train, , drop = FALSE]
  reviews <- train_intervals[train_intervals$semantic_track == "review", , drop = FALSE]
  if (nrow(reviews) > 0L) {
    for (i in seq_len(nrow(reviews))) {
      idx <- seq.int(reviews$start_isi[i], reviews$end_isi[i])
      out$review_candidate_count[idx] <- out$review_candidate_count[idx] + 1L
    }
  }
  active <- train_intervals[train_intervals$active_in_preview, , drop = FALSE]
  if (nrow(active) > 0L) {
    for (i in seq_len(nrow(active))) {
      track <- active$semantic_track[i]
      label_column <- paste0("pattern_auto_", track, "_preview")
      id_column <- paste0(label_column, "_interval_id")
      idx <- seq.int(active$start_isi[i], active$end_isi[i])
      # Same-track overlap is checked before this routine; no first-writer
      # arbitration is permitted in the public preview projection.
      out[[label_column]][idx] <- active$label[i]
      out[[id_column]][idx] <- active$interval_id[i]
    }
  }
  # The first spike has no preceding ISI and therefore cannot receive a track
  # label even if malformed upstream input attempted to cover it.
  label_columns <- grep(
    "^pattern_auto_.*_preview(_interval_id)?$", names(out), value = TRUE
  )
  out[1L, label_columns] <- ""
  out$review_candidate_count[1L] <- 0L
  out
}

stpd_multitrack_preview_assert_d012 <- function(intervals) {
  review <- intervals[intervals$semantic_track == "review", , drop = FALSE]
  if (nrow(review) == 0L) return(invisible(TRUE))
  okay <- review$label == "possible_burst" &
    review$review_target_track == "event" &
    review$review_target_label == "burst" &
    review$review_promotion_required &
    !review$eligible_for_primary_event_metrics &
    !review$eligible_for_state_split &
    !review$eligible_for_hfs_dominance
  okay[is.na(okay)] <- FALSE
  if (!all(okay)) {
    stpd_multitrack_preview_abort(
      "d012_review_isolation_failed",
      paste(
        "Review candidates must remain possible_burst, target Event/Burst,",
        "require promotion, and remain ineligible for primary metrics, State",
        "splitting, and HFS dominance."
      )
    )
  }
  invisible(TRUE)
}

stpd_multitrack_preview_assert_shadow_provenance <- function(
    shadow, compatibility, params, train) {
  expected_policy_hash <- stpd_multitrack_policy_hash(params)
  shadow_policy_hash <- stpd_multitrack_preview_chr(
    shadow$multitrack_policy_hash
  )
  compatibility_policy_hash <- stpd_multitrack_preview_chr(
    compatibility$multitrack_policy_hash
  )
  if (!nzchar(expected_policy_hash) ||
      !identical(shadow_policy_hash, expected_policy_hash) ||
      !identical(compatibility_policy_hash, expected_policy_hash)) {
    stpd_multitrack_preview_abort(
      "shadow_policy_hash_mismatch",
      paste0(
        "Phase 1A/1B policy provenance is stale or inconsistent in train '",
        train, "'."
      )
    )
  }

  expected_source_hash <- stpd_multitrack_source_params_hash(params)
  shadow_source_hash <- stpd_multitrack_preview_chr(
    shadow$source_params_hash
  )
  compatibility_source_hash <- stpd_multitrack_preview_chr(
    compatibility$source_params_hash
  )
  if (!nzchar(expected_source_hash) ||
      !identical(shadow_source_hash, expected_source_hash) ||
      !identical(compatibility_source_hash, expected_source_hash)) {
    stpd_multitrack_preview_abort(
      "shadow_source_params_hash_mismatch",
      paste0(
        "Phase 1A/1B detector-parameter provenance is stale or inconsistent ",
        "in train '", train, "'."
      )
    )
  }
  invisible(TRUE)
}

stpd_multitrack_preview_build <- function(
    ds, params, selected_trains, run_id, params_hash, policy,
    materialize_per_isi = TRUE) {
  if (is.null(ds) || !is.list(ds) || is.null(ds$trains) ||
      !is.list(ds$trains)) {
    stpd_multitrack_preview_abort(
      "dataset_schema_missing", "Dataset must contain a named trains list."
    )
  }
  expected_source_hash <- stpd_multitrack_source_params_hash(params)
  if (!nzchar(expected_source_hash) ||
      !identical(stpd_multitrack_preview_chr(params_hash), expected_source_hash)) {
    stpd_multitrack_preview_abort(
      "public_params_hash_mismatch",
      paste(
        "The public preview params_hash does not identify the effective",
        "detector parameters that generated its Phase 1 evidence."
      )
    )
  }
  train_names <- names(ds$trains) %||% character()
  if (is.null(selected_trains)) {
    trains <- train_names
  } else {
    trains <- unique(as.character(selected_trains))
    trains <- trains[!is.na(trains) & nzchar(trains)]
    unknown <- setdiff(trains, train_names)
    if (length(unknown) > 0L) {
      stpd_multitrack_preview_abort(
        "selected_train_missing",
        paste0("Unknown selected train(s): ", paste(unknown, collapse = ", "))
      )
    }
  }
  trains <- sort(unique(trains), method = "radix")
  interval_parts <- list()
  contexts <- list()
  shadow_versions <- character()
  compatibility_versions <- character()

  for (train in trains) {
    dat <- ds$trains[[train]]
    if (!is.data.frame(dat)) {
      stpd_multitrack_preview_abort(
        "train_schema_invalid",
        paste0("Train '", train, "' is not a data frame.")
      )
    }
    shadow <- attr(dat, "multitrack_shadow", exact = TRUE)
    compatibility <- attr(
      dat, "multitrack_compatibility_shadow", exact = TRUE
    )
    if (is.null(shadow) && nrow(dat) == 0L) {
      shadow <- stpd_multitrack_shadow_select(
        data.frame(), patterns = character(), params = params
      )
    }
    if (is.null(compatibility) && nrow(dat) == 0L) {
      compatibility <- stpd_multitrack_compatibility_shadow(
        shadow, params = params
      )
    }
    if (!inherits(shadow, "stpd_multitrack_shadow") ||
        !inherits(
          compatibility, "stpd_multitrack_compatibility_shadow"
        )) {
      stpd_multitrack_preview_abort(
        "shadow_product_missing",
        paste0(
          "Train '", train,
          "' does not contain both Phase 1 multi-track shadow products."
        )
      )
    }
    stpd_multitrack_preview_assert_shadow_provenance(
      shadow, compatibility, params, train
    )
    phase1_invariants <- as.data.frame(
      compatibility$invariants %||% data.frame(), stringsAsFactors = FALSE
    )
    if (nrow(phase1_invariants) > 0L &&
        "severity" %in% names(phase1_invariants) &&
        any(tolower(as.character(phase1_invariants$severity)) == "violation")) {
      messages <- if ("message" %in% names(phase1_invariants)) {
        paste(unique(as.character(phase1_invariants$message)), collapse = "; ")
      } else {
        "Phase 1B compatibility invariant violation."
      }
      stpd_multitrack_preview_abort(
        "phase1b_invariant_violation",
        paste0("Train '", train, "': ", messages)
      )
    }
    candidate_intervals <- stpd_multitrack_preview_candidate_rows(
      train, dat, shadow, compatibility, policy, run_id, params_hash
    )
    fragment_intervals <- stpd_multitrack_preview_fragment_rows(
      train, dat, shadow, compatibility, candidate_intervals, policy,
      run_id, params_hash
    )
    interval_parts[[train]] <- dplyr::bind_rows(
      candidate_intervals, fragment_intervals
    )
    contexts[[train]] <- list(
      dat = dat, shadow = shadow, compatibility = compatibility
    )
    shadow_versions <- c(shadow_versions, shadow$policy_version %||% "")
    compatibility_versions <- c(
      compatibility_versions, compatibility$policy_version %||% ""
    )
  }

  # A named list passed as a second bind_rows() argument becomes a nested
  # list-column.  Flatten the per-train parts explicitly so public columns keep
  # their values and IDs instead of being replaced by NA placeholders.
  intervals <- dplyr::bind_rows(c(
    list(stpd_multitrack_preview_empty_intervals()),
    unname(interval_parts)
  ))
  if (nrow(intervals) > 0L) {
    ord <- order(
      intervals$train,
      match(intervals$semantic_track, c("event", "state", "gap", "review")),
      intervals$start_isi, intervals$end_isi, intervals$interval_kind,
      intervals$interval_id, na.last = TRUE, method = "radix"
    )
    intervals <- intervals[ord, , drop = FALSE]
    rownames(intervals) <- NULL
  }
  if (anyDuplicated(intervals$interval_id)) {
    stpd_multitrack_preview_abort(
      "duplicate_interval_id",
      "Public multi-track interval IDs are not unique."
    )
  }
  stpd_multitrack_preview_assert_d012(intervals)
  stpd_multitrack_preview_assert_unique_active_tracks(intervals)

  relationship_parts <- list()
  dominance_parts <- list()
  per_isi_parts <- list()
  for (train in trains) {
    context <- contexts[[train]]
    train_intervals <- intervals[intervals$train == train, , drop = FALSE]
    relationship_parts[[train]] <- stpd_multitrack_preview_relationship_rows(
      train, context$compatibility, train_intervals, policy,
      run_id, params_hash
    )
    dominance_parts[[train]] <- stpd_multitrack_preview_hfs_rows(
      train, context$compatibility, train_intervals, policy,
      run_id, params_hash
    )
    per_isi_parts[[train]] <- if (isTRUE(materialize_per_isi)) {
      stpd_multitrack_preview_per_isi_rows(
        train, context$dat, intervals, policy, run_id, params_hash
      )
    } else {
      stpd_multitrack_preview_empty_per_isi()
    }
  }
  relationships <- dplyr::bind_rows(c(
    list(stpd_multitrack_preview_empty_relationships()),
    unname(relationship_parts)
  ))
  hfs <- dplyr::bind_rows(c(
    list(stpd_multitrack_preview_empty_hfs_dominance()),
    unname(dominance_parts)
  ))
  per_isi <- dplyr::bind_rows(c(
    list(stpd_multitrack_preview_empty_per_isi()),
    unname(per_isi_parts)
  ))
  if (nrow(relationships) > 0L) {
    non_destructive <- as.logical(relationships$non_destructive)
    if (any(is.na(non_destructive)) || !all(non_destructive)) {
      stpd_multitrack_preview_abort(
        "destructive_relationship_not_allowed",
        "Public Preview relationships must all be explicitly non-destructive."
      )
    }
  }
  if (nrow(hfs) > 0L) {
    preserved <- as.logical(hfs$state_selected_within_track_preserved)
    destructive <- as.logical(hfs$destructive_action_applied)
    if (any(is.na(preserved)) || !all(preserved) ||
        any(is.na(destructive)) || any(destructive)) {
      stpd_multitrack_preview_abort(
        "destructive_hfs_dominance_not_allowed",
        paste(
          "HFS dominance is evidence only: State selection must be preserved",
          "and no destructive action may be applied."
        )
      )
    }
  }
  if (nrow(relationships) > 0L) {
    valid_source <- relationships$source_interval_id %in% intervals$interval_id
    valid_target <- relationships$target_interval_id %in% intervals$interval_id
    if (!all(valid_source & valid_target)) {
      stpd_multitrack_preview_abort(
        "relationship_foreign_key_failed",
        "At least one relationship foreign key is absent from intervals."
      )
    }
  }
  if (nrow(per_isi) > 0L) {
    first <- !duplicated(per_isi$train)
    label_cols <- grep(
      "^pattern_auto_.*_preview(_interval_id)?$",
      names(per_isi), value = TRUE
    )
    if (any(vapply(label_cols, function(column) {
      any(nzchar(as.character(per_isi[[column]][first])))
    }, logical(1))) || any(per_isi$review_candidate_count[first] != 0L)) {
      stpd_multitrack_preview_abort(
        "first_isi_not_empty",
        "The first row of every train must have empty preview labels and IDs."
      )
    }
  }

  invariant_rows <- list(
    stpd_multitrack_preview_invariant_row(
      policy, run_id, params_hash, "interval_id_unique",
      message = "Every interval_id is unique across all selected trains."
    ),
    stpd_multitrack_preview_invariant_row(
      policy, run_id, params_hash, "relationship_foreign_keys",
      message = "Every relationship endpoint resolves to intervals.interval_id."
    ),
    stpd_multitrack_preview_invariant_row(
      policy, run_id, params_hash, "active_within_track_non_overlap",
      message = "No active intervals overlap within a train and semantic track."
    ),
    stpd_multitrack_preview_invariant_row(
      policy, run_id, params_hash, "per_isi_first_row_empty",
      message = "The first spike row has no preceding-ISI preview assignment."
    ),
    stpd_multitrack_preview_invariant_row(
      policy, run_id, params_hash, "d012_review_isolation",
      message = paste(
        "All Review candidates target Event/Burst but remain ineligible for",
        "primary Event metrics, State splitting, and HFS dominance until promotion."
      )
    ),
    stpd_multitrack_preview_invariant_row(
      policy, run_id, params_hash, "dominance_evidence_non_destructive",
      message = paste(
        "Every relationship is non-destructive and HFS dominance remains",
        "evidence that preserves State-track selection."
      )
    )
  )
  invariants <- dplyr::bind_rows(
    stpd_multitrack_preview_empty_invariants(), invariant_rows
  )
  tables <- list(
    intervals = intervals,
    relationships = relationships,
    per_isi = per_isi,
    hfs_dominance_evidence = hfs,
    invariants = invariants
  )
  stpd_multitrack_preview_finalize(
    policy = policy,
    run_id = run_id,
    params_hash = params_hash,
    selected_trains = trains,
    tables = tables,
    source_shadow_versions = shadow_versions,
    source_compatibility_versions = compatibility_versions,
    label_blind = isTRUE((params$meta %||% list())$label_blind)
  )
}

stpd_multitrack_preview_materialize <- function(
    ds, params, selected_trains = NULL, run_id = NULL,
    params_hash = NULL) {
  # The disabled contract is absolute: a valid scalar FALSE does not attach a
  # Preview object, even when another dormant Preview field is malformed.
  raw_block <- tryCatch(
    stpd_multitrack_preview_block(params),
    error = function(e) e
  )
  if (is.list(raw_block) && !inherits(raw_block, "error")) {
    raw_enabled <- raw_block$enabled
    if (length(raw_enabled) == 1L && is.logical(raw_enabled) &&
        !is.na(raw_enabled) && !isTRUE(raw_enabled)) {
      return(NULL)
    }
  }
  identity <- stpd_multitrack_preview_run_identity(
    ds, run_id = run_id, params_hash = params_hash
  )
  fallback_policy <- list(
    enabled = TRUE,
    schema_version = stpd_multitrack_preview_schema_version(),
    authoritative = FALSE,
    parameter_path = stpd_multitrack_preview_parameter_path(),
    policy_hash = tryCatch(
      stpd_multitrack_preview_policy_hash(params),
      error = function(e) ""
    ),
    policy_hash_algorithm = "SHA-256"
  )
  policy <- tryCatch(
    stpd_multitrack_preview_policy(params),
    error = function(e) e
  )
  if (inherits(policy, "error")) {
    return(stpd_multitrack_preview_failed(
      fallback_policy,
      identity$run_id,
      identity$params_hash,
      as.character(selected_trains %||% character()),
      "invalid_preview_policy",
      conditionMessage(policy),
      label_blind = isTRUE((params$meta %||% list())$label_blind)
    ))
  }
  if (!isTRUE(policy$enabled)) return(NULL)
  tryCatch(
    stpd_multitrack_preview_build(
      ds = ds,
      params = params,
      selected_trains = selected_trains,
      run_id = identity$run_id,
      params_hash = identity$params_hash,
      policy = policy
    ),
    error = function(e) {
      code <- if (inherits(e, "stpd_multitrack_preview_error")) {
        stpd_multitrack_preview_chr(e$code, "preview_materialization_error")
      } else {
        "preview_materialization_error"
      }
      stpd_multitrack_preview_failed(
        policy,
        identity$run_id,
        identity$params_hash,
        as.character(selected_trains %||% character()),
        code,
        conditionMessage(e),
        label_blind = isTRUE((params$meta %||% list())$label_blind)
      )
    }
  )
}

stpd_multitrack_preview_strip <- function(ds) {
  if (is.null(ds) || !is.list(ds)) return(ds)
  if (is.list(ds$results) && "multitrack_preview" %in% names(ds$results)) {
    ds$results$multitrack_preview <- NULL
  }
  if (is.list(ds$trains) && length(ds$trains) > 0L) {
    for (train in names(ds$trains)) {
      dat <- ds$trains[[train]]
      if (!is.data.frame(dat)) next
      preview_columns <- grep(
        "^pattern_auto_.*_preview($|_)", names(dat),
        value = TRUE, perl = TRUE
      )
      if (length(preview_columns) > 0L) dat[preview_columns] <- NULL
      ds$trains[[train]] <- dat
    }
  }
  ds
}

stpd_multitrack_preview_attach <- function(
    ds, params, selected_trains = NULL, run_id = NULL,
    params_hash = NULL, target_trains = NULL) {
  out <- stpd_multitrack_preview_strip(ds)
  if (is.null(selected_trains) && !is.null(target_trains)) {
    selected_trains <- target_trains
  }
  preview <- stpd_multitrack_preview_materialize(
    out,
    params = params,
    selected_trains = selected_trains,
    run_id = run_id,
    params_hash = params_hash
  )
  if (is.null(preview)) return(out)
  if (is.null(out$results) || !is.list(out$results)) out$results <- list()
  out$results$multitrack_preview <- preview
  out
}

stpd_multitrack_preview_export_filenames <- function() {
  c(
    unname(stpd_multitrack_preview_table_export_filenames()),
    "Multitrack_preview_manifest.csv",
    "Multitrack_preview.rds"
  )
}

stpd_multitrack_preview_cleanup_exports <- function(out_dir) {
  if (!dir.exists(out_dir)) return(invisible(character()))
  stale <- file.path(out_dir, stpd_multitrack_preview_export_filenames())
  stale <- stale[file.exists(stale)]
  if (length(stale) > 0L) {
    unlink(stale, recursive = TRUE, force = TRUE)
  }
  remaining <- file.path(out_dir, stpd_multitrack_preview_export_filenames())
  remaining <- remaining[file.exists(remaining)]
  if (length(remaining) > 0L) {
    stop(
      paste0(
        "Could not remove stale multi-track Preview artifacts: ",
        paste(basename(remaining), collapse = ", "), "."
      ),
      call. = FALSE
    )
  }
  invisible(stale)
}

stpd_multitrack_preview_export_abort <- function(code, message) {
  condition <- structure(
    list(
      message = as.character(message)[1],
      call = NULL,
      code = as.character(code)[1]
    ),
    class = c(
      "stpd_multitrack_preview_export_validation_error", "error", "condition"
    )
  )
  stop(condition)
}

stpd_multitrack_preview_selected_train_values <- function(x) {
  text <- stpd_multitrack_preview_chr(x)
  if (!nzchar(text)) return(character())
  values <- trimws(unlist(strsplit(text, ";", fixed = TRUE), use.names = FALSE))
  values[nzchar(values)]
}

stpd_multitrack_preview_validate_metadata_for_export <- function(
    metadata, tables) {
  expected_types <- stpd_multitrack_preview_metadata_types()
  actual_types <- if (is.data.frame(metadata)) {
    vapply(metadata, typeof, character(1))
  } else character()
  if (nrow(metadata) != 1L ||
      !identical(names(metadata), names(expected_types)) ||
      !identical(unname(actual_types), unname(expected_types))) {
    stpd_multitrack_preview_export_abort(
      "preview_metadata_schema_invalid",
      "Preview metadata must contain exactly one row with the fixed public schema and types."
    )
  }

  scalar_chr <- function(name) as.character(metadata[[name]][1])
  scalar_lgl <- function(name) as.logical(metadata[[name]][1])
  status <- scalar_chr("materialization_status")
  selected <- stpd_multitrack_preview_selected_train_values(
    metadata$selected_trains[1]
  )
  table_counts <- c(
    intervals_n = nrow(tables$intervals),
    relationships_n = nrow(tables$relationships),
    per_isi_n = nrow(tables$per_isi),
    hfs_dominance_evidence_n = nrow(tables$hfs_dominance_evidence),
    invariants_n = nrow(tables$invariants)
  )
  recorded_counts <- as.integer(unlist(
    metadata[names(table_counts)], use.names = FALSE
  ))
  fixed_semantics <-
    identical(scalar_chr("schema_version"),
              stpd_multitrack_preview_schema_version()) &&
    grepl("^[0-9a-f]{64}$", scalar_chr("policy_hash")) &&
    identical(scalar_chr("policy_hash_algorithm"), "SHA-256") &&
    identical(scalar_chr("policy_parameter_path"),
              stpd_multitrack_preview_parameter_path()) &&
    nzchar(scalar_chr("run_id")) &&
    grepl("^[0-9a-f]{64}$", scalar_chr("params_hash")) &&
    identical(scalar_lgl("enabled"), TRUE) &&
    identical(scalar_lgl("authoritative"), FALSE) &&
    !is.na(scalar_lgl("label_blind")) &&
    identical(
      scalar_chr("detection_mode"),
      if (isTRUE(scalar_lgl("label_blind"))) "label_blind" else "manual_aware"
    ) &&
    identical(scalar_lgl("automatic_tracks_only"), TRUE) &&
    identical(scalar_lgl("manual_final_migration_pending"), TRUE) &&
    identical(scalar_chr("canonical_typed_artifact"),
              "Multitrack_preview.rds") &&
    identical(
      scalar_chr("csv_serialization_role"),
      "human_readable_schema_described_by_manifest"
    ) &&
    identical(scalar_lgl("legacy_projection_changed"), FALSE) &&
    identical(as.integer(metadata$selected_train_n[1]),
              as.integer(length(selected))) &&
    !anyDuplicated(selected) &&
    identical(recorded_counts, as.integer(table_counts))
  status_semantics <- if (identical(status, "materialized")) {
    !nzchar(scalar_chr("failure_code")) &&
      !nzchar(scalar_chr("failure_message"))
  } else if (identical(status, "failed_closed")) {
    nzchar(scalar_chr("failure_code")) && nzchar(scalar_chr("failure_message"))
  } else FALSE
  if (!isTRUE(fixed_semantics) || !isTRUE(status_semantics)) {
    stpd_multitrack_preview_export_abort(
      "preview_metadata_semantics_invalid",
      paste(
        "Persisted Preview metadata does not match its fixed non-authoritative",
        "policy, status, selected-train, or table-count semantics."
      )
    )
  }
  invisible(selected)
}

stpd_multitrack_preview_validate_per_isi_for_export <- function(
    intervals, per_isi, selected_trains, parent = NULL) {
  if (nrow(per_isi) == 0L) return(invisible(TRUE))
  if (anyNA(per_isi$train) || any(!nzchar(per_isi$train)) ||
      anyNA(per_isi$isi_index) || any(per_isi$isi_index < 1L) ||
      anyNA(per_isi$review_candidate_count) ||
      any(per_isi$review_candidate_count < 0L)) {
    stpd_multitrack_preview_export_abort(
      "preview_per_isi_geometry_invalid",
      "Persisted Preview per-ISI rows contain invalid train, index, or review-count values."
    )
  }
  row_key <- paste(per_isi$train, per_isi$isi_index, sep = "\u001f")
  if (anyDuplicated(row_key)) {
    stpd_multitrack_preview_export_abort(
      "preview_per_isi_geometry_invalid",
      "Persisted Preview per-ISI (train, isi_index) keys are not unique."
    )
  }
  observed_trains <- unique(as.character(per_isi$train))
  if (length(selected_trains) > 0L &&
      (!all(observed_trains %in% selected_trains) ||
       !all(selected_trains %in% observed_trains))) {
    stpd_multitrack_preview_export_abort(
      "preview_selected_train_scope_invalid",
      "Persisted Preview per-ISI rows do not match the selected-train scope."
    )
  }
  for (train in observed_trains) {
    rows <- which(per_isi$train == train)
    if (!identical(per_isi$isi_index[rows], seq_len(length(rows)))) {
      stpd_multitrack_preview_export_abort(
        "preview_per_isi_geometry_invalid",
        paste0("Persisted Preview per-ISI rows are not contiguous and ordered for train '",
               train, "'.")
      )
    }
  }

  tracks <- c("event", "state", "gap", "review")
  expected_labels <- stats::setNames(
    replicate(length(tracks), rep("", nrow(per_isi)), simplify = FALSE),
    tracks
  )
  expected_ids <- expected_labels
  expected_review_count <- integer(nrow(per_isi))
  if (nrow(intervals) > 0L) {
    for (i in seq_len(nrow(intervals))) {
      span_key <- paste(
        intervals$train[i],
        seq.int(intervals$start_isi[i], intervals$end_isi[i]),
        sep = "\u001f"
      )
      hits <- match(span_key, row_key)
      if (anyNA(hits)) {
        stpd_multitrack_preview_export_abort(
          "preview_per_isi_interval_coverage_invalid",
          paste0("Interval '", intervals$interval_id[i],
                 "' is not fully represented by persisted per-ISI rows.")
        )
      }
      track <- as.character(intervals$semantic_track[i])
      if (identical(track, "review")) {
        expected_review_count[hits] <- expected_review_count[hits] + 1L
      }
      if (isTRUE(intervals$active_in_preview[i])) {
        if (!track %in% tracks) {
          stpd_multitrack_preview_export_abort(
            "preview_per_isi_projection_invalid",
            paste0("Active interval '", intervals$interval_id[i],
                   "' has an unsupported semantic track.")
          )
        }
        expected_labels[[track]][hits] <- as.character(intervals$label[i])
        expected_ids[[track]][hits] <- as.character(intervals$interval_id[i])
      }
    }
  }
  first <- !duplicated(per_isi$train)
  expected_review_count[first] <- 0L
  for (track in tracks) {
    expected_labels[[track]][first] <- ""
    expected_ids[[track]][first] <- ""
    label_column <- paste0("pattern_auto_", track, "_preview")
    id_column <- paste0(label_column, "_interval_id")
    actual_label <- as.character(per_isi[[label_column]])
    actual_id <- as.character(per_isi[[id_column]])
    if (anyNA(actual_label) || anyNA(actual_id) ||
        !identical(actual_label, expected_labels[[track]]) ||
        !identical(actual_id, expected_ids[[track]])) {
      stpd_multitrack_preview_export_abort(
        "preview_per_isi_projection_invalid",
        paste0("Persisted Preview ", track,
               " labels or interval foreign keys do not match active intervals.")
      )
    }
  }
  if (!identical(as.integer(per_isi$review_candidate_count),
                 expected_review_count)) {
    stpd_multitrack_preview_export_abort(
      "preview_per_isi_review_count_invalid",
      "Persisted Preview review_candidate_count does not match Review interval coverage."
    )
  }

  if (is.list(parent) && is.list(parent$trains)) {
    for (train in selected_trains) {
      dat <- parent$trains[[train]]
      rows <- which(per_isi$train == train)
      if (!is.data.frame(dat) || nrow(dat) != length(rows)) {
        stpd_multitrack_preview_export_abort(
          "preview_parent_train_geometry_mismatch",
          paste0("Preview row count does not match parent train '", train, "'.")
        )
      }
      parent_timestamp <- if ("timestamp_sec" %in% names(dat)) {
        suppressWarnings(as.numeric(dat$timestamp_sec))
      } else rep(NA_real_, nrow(dat))
      parent_isi <- if ("ISI_sec" %in% names(dat)) {
        suppressWarnings(as.numeric(dat$ISI_sec))
      } else rep(NA_real_, nrow(dat))
      if (!identical(per_isi$timestamp_sec[rows], parent_timestamp) ||
          !identical(per_isi$ISI_sec[rows], parent_isi)) {
        stpd_multitrack_preview_export_abort(
          "preview_parent_train_payload_mismatch",
          paste0("Preview timestamps or ISIs do not match parent train '", train, "'.")
        )
      }
    }
  }
  invisible(TRUE)
}

stpd_multitrack_preview_validate_parent_for_export <- function(
    preview, parent, selected_trains) {
  if (!is.list(parent)) return(invisible(TRUE))
  metadata <- preview$metadata
  results <- parent$results %||% list()
  parent_metadata_sources <- Filter(
    function(x) is.data.frame(x) && nrow(x) == 1L,
    list(results$run_metadata_public, results$run_metadata)
  )
  for (parent_metadata in parent_metadata_sources) {
    compare_if_present <- function(field, expected) {
      if (!field %in% names(parent_metadata)) return(TRUE)
      identical(as.character(parent_metadata[[field]][1]), as.character(expected))
    }
    expected_run_id <- as.character(metadata$run_id[1])
    expected_params_hash <- as.character(metadata$params_hash[1])
    if (!compare_if_present("run_id", expected_run_id) ||
        !compare_if_present("params_hash", expected_params_hash)) {
      stpd_multitrack_preview_export_abort(
        "preview_parent_identity_mismatch",
        "Persisted Preview run_id or params_hash does not match its parent result."
      )
    }
    if ("label_blind" %in% names(parent_metadata) &&
        !identical(as.logical(parent_metadata$label_blind[1]),
                   as.logical(metadata$label_blind[1]))) {
      stpd_multitrack_preview_export_abort(
        "preview_parent_detection_mode_mismatch",
        "Persisted Preview label_blind does not match its parent result."
      )
    }
    if ("detection_mode" %in% names(parent_metadata) &&
        !identical(as.character(parent_metadata$detection_mode[1]),
                   as.character(metadata$detection_mode[1]))) {
      stpd_multitrack_preview_export_abort(
        "preview_parent_detection_mode_mismatch",
        "Persisted Preview detection_mode does not match its parent result."
      )
    }
    if ("selected_trains" %in% names(parent_metadata)) {
      parent_selected <- stpd_multitrack_preview_selected_train_values(
        parent_metadata$selected_trains[1]
      )
      if (!identical(parent_selected, selected_trains)) {
        stpd_multitrack_preview_export_abort(
          "preview_parent_selected_train_mismatch",
          "Persisted Preview selected-train scope does not match its parent result."
        )
      }
    }
  }
  if (!is.null(parent$params_effective) && is.list(parent$params_effective)) {
    expected_params_hash <- stpd_params_hash(parent$params_effective)
    expected_policy_hash <- stpd_multitrack_preview_policy_hash(
      parent$params_effective
    )
    if (!identical(as.character(metadata$params_hash[1]), expected_params_hash) ||
        !identical(as.character(metadata$policy_hash[1]), expected_policy_hash)) {
      stpd_multitrack_preview_export_abort(
        "preview_parent_parameter_hash_mismatch",
        "Persisted Preview parameter or policy hash does not match params_effective."
      )
    }
  }
  if (is.list(parent$trains)) {
    if (!all(selected_trains %in% names(parent$trains))) {
      stpd_multitrack_preview_export_abort(
        "preview_parent_selected_train_mismatch",
        "At least one persisted Preview train is absent from its parent result."
      )
    }
    public_train_fields <- c(
      as.character(preview$intervals$train),
      as.character(preview$relationships$train),
      as.character(preview$per_isi$train),
      as.character(preview$hfs_dominance_evidence$train)
    )
    public_train_fields <- unique(public_train_fields[nzchar(public_train_fields)])
    if (!all(public_train_fields %in% selected_trains)) {
      stpd_multitrack_preview_export_abort(
        "preview_parent_selected_train_mismatch",
        "Persisted Preview tables contain a train outside the parent run scope."
      )
    }
  }
  # A normal detector result retains both params_effective and the Phase 1
  # train attributes. Re-materializing from those parent-owned sources binds
  # every public semantic value to the run, rather than trusting a persisted
  # Preview whose tables and manifest may have been altered together.
  if (!is.null(parent$params_effective) && is.list(parent$params_effective) &&
      is.list(parent$trains)) {
    source_parent <- stpd_multitrack_preview_strip(parent)
    rebuilt <- stpd_multitrack_preview_materialize(
      source_parent,
      params = parent$params_effective,
      selected_trains = selected_trains,
      run_id = as.character(metadata$run_id[1]),
      params_hash = as.character(metadata$params_hash[1])
    )
    required <- c(
      "metadata", "intervals", "relationships", "per_isi",
      "hfs_dominance_evidence", "invariants", "table_manifest"
    )
    if (is.null(rebuilt) || !all(required %in% names(rebuilt)) ||
        !all(vapply(required, function(name) {
          identical(preview[[name]], rebuilt[[name]])
        }, logical(1)))) {
      stpd_multitrack_preview_export_abort(
        "preview_parent_rematerialization_mismatch",
        paste(
          "Persisted Preview tables do not exactly match deterministic",
          "re-materialization from the parent run."
        )
      )
    }
  }
  invisible(TRUE)
}

stpd_multitrack_preview_validate_public_tables_for_export <- function(
    tables, metadata, selected_trains = character(), parent = NULL) {
  prototypes <- list(
    intervals = stpd_multitrack_preview_empty_intervals(),
    relationships = stpd_multitrack_preview_empty_relationships(),
    per_isi = stpd_multitrack_preview_empty_per_isi(),
    hfs_dominance_evidence = stpd_multitrack_preview_empty_hfs_dominance(),
    invariants = stpd_multitrack_preview_empty_invariants()
  )
  for (name in names(prototypes)) {
    actual <- tables[[name]]
    expected <- prototypes[[name]]
    same_names <- identical(names(actual), names(expected))
    same_types <- same_names && identical(
      vapply(actual, typeof, character(1)),
      vapply(expected, typeof, character(1))
    )
    if (!same_names || !same_types) {
      stpd_multitrack_preview_export_abort(
        "preview_public_table_schema_invalid",
        paste0("Persisted Preview table '", name,
               "' does not match its fixed public schema and types.")
      )
    }
  }

  identity <- list(
    schema_version = as.character(metadata$schema_version[1]),
    policy_hash = as.character(metadata$policy_hash[1]),
    run_id = as.character(metadata$run_id[1]),
    params_hash = as.character(metadata$params_hash[1])
  )
  for (name in names(tables)) {
    table <- tables[[name]]
    if (nrow(table) == 0L) next
    required <- c(names(identity), "authoritative")
    if (!all(required %in% names(table))) {
      stpd_multitrack_preview_export_abort(
        "preview_table_identity_missing",
        paste0("Persisted Preview table '", name,
               "' is missing public identity columns.")
      )
    }
    matches <- all(vapply(names(identity), function(field) {
      identical(as.character(table[[field]]),
                rep(identity[[field]], nrow(table)))
    }, logical(1))) && identical(
      as.logical(table$authoritative), rep(FALSE, nrow(table))
    )
    if (!matches) {
      stpd_multitrack_preview_export_abort(
        "preview_table_identity_mismatch",
        paste0("Persisted Preview table '", name,
               "' does not match Preview metadata identity.")
      )
    }
  }

  status <- as.character(metadata$materialization_status %||% "")[1]
  if (identical(status, "failed_closed")) {
    nonempty_science <- vapply(
      tables[c(
        "intervals", "relationships", "per_isi", "hfs_dominance_evidence"
      )], nrow, integer(1)
    ) > 0L
    failed_status <- as.character(tables$invariants$status)
    if (any(nonempty_science) || nrow(tables$invariants) == 0L ||
        any(is.na(failed_status)) ||
        !all(failed_status == "failed_closed")) {
      stpd_multitrack_preview_export_abort(
        "preview_failed_closed_payload_invalid",
        paste(
          "A failed-closed Preview may contain only its failed invariant audit,",
          "not public scientific tables."
        )
      )
    }
    return(invisible(TRUE))
  }
  if (!identical(status, "materialized")) {
    stpd_multitrack_preview_export_abort(
      "preview_materialization_status_invalid",
      paste0("Unsupported persisted Preview materialization_status '",
             status, "'.")
    )
  }

  intervals <- tables$intervals
  relationships <- tables$relationships
  per_isi <- tables$per_isi
  hfs <- tables$hfs_dominance_evidence
  invariants <- tables$invariants
  if (anyDuplicated(intervals$interval_id)) {
    stpd_multitrack_preview_export_abort(
      "preview_interval_identity_invalid",
      "Persisted Preview interval_id values are not unique."
    )
  }
  if (nrow(intervals) > 0L) {
    expected_interval_ids <- vapply(seq_len(nrow(intervals)), function(i) {
      stpd_multitrack_preview_interval_id(
        intervals$train[i], intervals$interval_kind[i],
        intervals$candidate_key[i], intervals$start_isi[i],
        intervals$end_isi[i]
      )
    }, character(1))
    if (!identical(as.character(intervals$interval_id),
                   expected_interval_ids)) {
      stpd_multitrack_preview_export_abort(
        "preview_interval_identity_invalid",
        "Persisted Preview interval IDs do not match their semantic payloads."
      )
    }
  }
  tryCatch(
    {
      stpd_multitrack_preview_assert_d012(intervals)
      stpd_multitrack_preview_assert_unique_active_tracks(intervals)
    },
    error = function(e) {
      stpd_multitrack_preview_export_abort(
        "preview_semantic_invariant_failed", conditionMessage(e)
      )
    }
  )

  if (anyDuplicated(relationships$relationship_id)) {
    stpd_multitrack_preview_export_abort(
      "preview_relationship_identity_invalid",
      "Persisted Preview relationship_id values are not unique."
    )
  }
  if (nrow(relationships) > 0L) {
    expected_relationship_ids <- vapply(
      seq_len(nrow(relationships)), function(i) {
        stpd_multitrack_preview_relationship_id(
          relationships$train[i], relationships$relationship_type[i],
          relationships$compatibility_rule[i],
          relationships$source_interval_id[i],
          relationships$target_interval_id[i],
          relationships$overlap_start_isi[i],
          relationships$overlap_end_isi[i]
        )
      }, character(1)
    )
    valid_fk <- relationships$source_interval_id %in% intervals$interval_id &
      relationships$target_interval_id %in% intervals$interval_id
    if (any(is.na(valid_fk)) || !all(valid_fk) ||
        !identical(as.character(relationships$relationship_id),
                   expected_relationship_ids) ||
        any(is.na(relationships$non_destructive)) ||
        !all(relationships$non_destructive)) {
      stpd_multitrack_preview_export_abort(
        "preview_relationship_invariant_failed",
        paste(
          "Persisted Preview relationships have broken identity/FK fields or",
          "contain a destructive action."
        )
      )
    }
  }

  if (nrow(hfs) > 0L) {
    state_ids <- as.character(hfs$hfs_interval_id)
    contributor_ids <- unique(unlist(lapply(
      hfs$contributor_interval_ids,
      stpd_multitrack_preview_split_ids
    ), use.names = FALSE))
    contributor_ids <- contributor_ids[nzchar(contributor_ids)]
    valid_contributors <- intervals$interval_id %in% contributor_ids
    contributors_ok <- if (length(contributor_ids) == 0L) TRUE else {
      isTRUE(all(contributor_ids %in% intervals$interval_id)) &&
        isTRUE(all(intervals$semantic_track[valid_contributors] == "event")) &&
        isTRUE(all(intervals$active_in_preview[valid_contributors])) &&
        isTRUE(all(intervals$eligible_for_hfs_dominance[valid_contributors]))
    }
    if (!all(state_ids %in% intervals$interval_id) || !contributors_ok ||
        any(is.na(hfs$state_selected_within_track_preserved)) ||
        !all(hfs$state_selected_within_track_preserved) ||
        any(is.na(hfs$destructive_action_applied)) ||
        any(hfs$destructive_action_applied)) {
      stpd_multitrack_preview_export_abort(
        "preview_hfs_evidence_invariant_failed",
        paste(
          "Persisted HFS evidence has broken interval references, ineligible",
          "contributors, or a destructive State action."
        )
      )
    }
  }

  stpd_multitrack_preview_validate_per_isi_for_export(
    intervals, per_isi, selected_trains = selected_trains, parent = parent
  )

  expected_invariants <- c(
    "interval_id_unique", "relationship_foreign_keys",
    "active_within_track_non_overlap", "per_isi_first_row_empty",
    "d012_review_isolation", "dominance_evidence_non_destructive"
  )
  invariant_status <- as.character(invariants$status)
  if (!setequal(as.character(invariants$check_name), expected_invariants) ||
      any(is.na(invariant_status)) || any(invariant_status != "pass")) {
    stpd_multitrack_preview_export_abort(
      "preview_invariant_audit_invalid",
      "Persisted Preview invariant audit is incomplete or not passing."
    )
  }
  invisible(TRUE)
}

stpd_multitrack_preview_validate_for_export <- function(preview, parent = NULL) {
  required <- c(
    "metadata", "intervals", "relationships", "per_isi",
    "hfs_dominance_evidence", "invariants", "table_manifest"
  )
  if (!is.list(preview)) {
    stpd_multitrack_preview_export_abort(
      "preview_not_a_list", "The persisted multi-track Preview is not a list."
    )
  }
  missing <- setdiff(required, names(preview))
  extra <- setdiff(names(preview), required)
  if (length(missing) > 0L || length(extra) > 0L ||
      !identical(names(preview), required)) {
    stpd_multitrack_preview_export_abort(
      "preview_component_schema_invalid",
      paste0(
        "The persisted multi-track Preview component set/order is not fixed; ",
        "missing: ", paste(missing, collapse = ", "),
        "; extra: ", paste(extra, collapse = ", ")
      )
    )
  }
  tables <- list(
    metadata = preview$metadata,
    intervals = preview$intervals,
    relationships = preview$relationships,
    per_isi = preview$per_isi,
    hfs_dominance_evidence = preview$hfs_dominance_evidence,
    invariants = preview$invariants
  )
  invalid_tables <- names(tables)[!vapply(tables, is.data.frame, logical(1))]
  if (length(invalid_tables) > 0L || !is.data.frame(preview$table_manifest)) {
    stpd_multitrack_preview_export_abort(
      "preview_table_schema_invalid",
      paste0(
        "Every persisted Preview table and its manifest must be a data.frame; ",
        "invalid: ",
        paste(c(invalid_tables, if (!is.data.frame(preview$table_manifest)) {
          "table_manifest"
        } else character()), collapse = ", ")
      )
    )
  }
  manifest <- preview$table_manifest
  manifest_required <- c(
    "schema_version", "policy_hash", "run_id", "params_hash",
    "authoritative", "table_name", "file_name", "row_count",
    "column_count", "column_types", "table_sha256"
  )
  missing_manifest <- setdiff(manifest_required, names(manifest))
  extra_manifest <- setdiff(names(manifest), manifest_required)
  expected_manifest_types <- c(
    "character", "character", "character", "character", "logical",
    "character", "character", "integer", "integer", "character",
    "character"
  )
  if (length(missing_manifest) > 0L || length(extra_manifest) > 0L ||
      !identical(names(manifest), manifest_required) ||
      !identical(unname(vapply(manifest, typeof, character(1))),
                 expected_manifest_types)) {
    stpd_multitrack_preview_export_abort(
      "preview_manifest_schema_invalid",
      paste0(
        "Preview manifest does not match the fixed schema/types; missing: ",
        paste(missing_manifest, collapse = ", "),
        "; extra: ", paste(extra_manifest, collapse = ", ")
      )
    )
  }
  fixed_files <- stpd_multitrack_preview_table_export_filenames()
  expected_tables <- names(fixed_files)
  manifest_tables <- as.character(manifest$table_name)
  if (nrow(manifest) != length(expected_tables) ||
      anyDuplicated(manifest_tables) ||
      !identical(manifest_tables, expected_tables)) {
    stpd_multitrack_preview_export_abort(
      "preview_manifest_table_identity_invalid",
      "Preview manifest table_name rows must exactly match the fixed table set."
    )
  }
  if (!identical(as.character(manifest$file_name), unname(fixed_files))) {
    stpd_multitrack_preview_export_abort(
      "preview_manifest_filename_invalid",
      paste(
        "Preview manifest file_name values do not match the fixed safe export",
        "mapping. Persisted file names are never used as write paths."
      )
    )
  }
  expected_rows <- vapply(tables, nrow, integer(1))
  expected_columns <- vapply(tables, ncol, integer(1))
  expected_types <- vapply(
    tables, stpd_multitrack_preview_column_types, character(1)
  )
  expected_hashes <- vapply(
    tables, stpd_multitrack_preview_table_hash, character(1)
  )
  if (!identical(as.integer(manifest$row_count), unname(expected_rows)) ||
      !identical(as.integer(manifest$column_count), unname(expected_columns)) ||
      !identical(as.character(manifest$column_types), unname(expected_types)) ||
      !identical(as.character(manifest$table_sha256), unname(expected_hashes))) {
    stpd_multitrack_preview_export_abort(
      "preview_manifest_payload_mismatch",
      paste(
        "Preview manifest row counts, column counts, column types, or table",
        "hashes do not match the persisted tables."
      )
    )
  }
  metadata <- preview$metadata
  selected_trains <- stpd_multitrack_preview_validate_metadata_for_export(
    metadata, tables
  )
  identity_matches <-
    identical(as.character(manifest$schema_version),
              rep(as.character(metadata$schema_version[1]), nrow(manifest))) &&
    identical(as.character(manifest$policy_hash),
              rep(as.character(metadata$policy_hash[1]), nrow(manifest))) &&
    identical(as.character(manifest$run_id),
              rep(as.character(metadata$run_id[1]), nrow(manifest))) &&
    identical(as.character(manifest$params_hash),
              rep(as.character(metadata$params_hash[1]), nrow(manifest))) &&
    identical(as.logical(manifest$authoritative), rep(FALSE, nrow(manifest)))
  if (!identity_matches) {
    stpd_multitrack_preview_export_abort(
      "preview_manifest_identity_mismatch",
      "Preview manifest identity fields do not match Preview metadata."
    )
  }
  stpd_multitrack_preview_validate_parent_for_export(
    preview, parent = parent, selected_trains = selected_trains
  )
  stpd_multitrack_preview_validate_public_tables_for_export(
    tables, metadata, selected_trains = selected_trains, parent = parent
  )
  list(tables = tables, manifest = manifest, fixed_files = fixed_files)
}

stpd_multitrack_preview_export_result <- function(
    paths = character(), status, code = "", message = "") {
  out <- as.character(paths)
  names(out) <- names(paths)
  attr(out, "multitrack_preview_export_status") <- as.character(status)[1]
  attr(out, "multitrack_preview_export_code") <- as.character(code)[1]
  attr(out, "multitrack_preview_export_message") <- as.character(message)[1]
  out
}

stpd_multitrack_preview_append_export_warning <- function(
    out_dir, status, code, message) {
  warning_file <- file.path(out_dir, "Methodological_warnings.txt")
  if (!file.exists(warning_file)) return(invisible(FALSE))
  line <- paste0(
    "component=multitrack_preview; status=", status,
    "; code=", code,
    "; legacy_export_action=continue; message=",
    gsub("[\r\n]+", " ", as.character(message)[1], perl = TRUE)
  )
  cat("\n", line, "\n", file = warning_file, append = TRUE, sep = "")
  invisible(TRUE)
}

stpd_write_multitrack_preview_strict <- function(preview, out_dir, parent = NULL) {
  validated <- stpd_multitrack_preview_validate_for_export(
    preview, parent = parent
  )
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(out_dir)) {
    stop("Could not create the multi-track Preview export directory.", call. = FALSE)
  }
  staging_dir <- tempfile(".stpd_multitrack_preview_", tmpdir = out_dir)
  if (!dir.create(staging_dir, recursive = FALSE, showWarnings = FALSE)) {
    stop("Could not create the multi-track Preview staging directory.", call. = FALSE)
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
  manifest_path <- file.path(staging_dir, "Multitrack_preview_manifest.csv")
  write_csv_safe(
    validated$manifest, manifest_path,
    row.names = FALSE, fileEncoding = "UTF-8"
  )
  paths["manifest"] <- manifest_path
  rds_path <- file.path(staging_dir, "Multitrack_preview.rds")
  saveRDS(preview, rds_path, version = 2)
  paths["rds"] <- rds_path

  expected_names <- stpd_multitrack_preview_export_filenames()
  if (!setequal(basename(paths), expected_names) ||
      !all(file.exists(paths))) {
    stop("The staged multi-track Preview artifact set is incomplete.", call. = FALSE)
  }
  stpd_multitrack_preview_cleanup_exports(out_dir)
  # Data/RDS are committed before the manifest. A consumer can therefore use
  # the manifest as the completion marker for a fully published Preview set.
  commit_order <- c(
    setdiff(names(paths), c("manifest", "rds")), "rds", "manifest"
  )
  paths <- paths[commit_order]
  final_paths <- file.path(out_dir, basename(paths))
  committed <- file.rename(paths, final_paths)
  if (length(committed) != length(paths) || !all(committed)) {
    stop("Could not commit the complete multi-track Preview artifact set.",
         call. = FALSE)
  }
  names(final_paths) <- names(paths)
  stpd_multitrack_preview_export_result(final_paths, status = "written")
}

stpd_write_multitrack_preview <- function(ds, out_dir) {
  preview <- if (is.list(ds) && is.list(ds$results)) {
    ds$results$multitrack_preview %||% NULL
  } else NULL
  if (is.null(preview)) {
    # Reusing an export directory must not leak a Preview from an earlier ON
    # run into a later OFF bundle. Delete only the fixed Preview artifact set.
    cleanup_error <- tryCatch(
      {
        stpd_multitrack_preview_cleanup_exports(out_dir)
        NULL
      },
      error = function(e) e
    )
    if (!is.null(cleanup_error)) {
      message <- conditionMessage(cleanup_error)
      tryCatch(
        stpd_multitrack_preview_append_export_warning(
          out_dir, status = "cleanup_failed",
          code = "multitrack_preview_cleanup_failed", message = message
        ),
        error = function(e) NULL
      )
      tryCatch(
        warning(
          paste0(
            "[multitrack_preview_export_cleanup_failed] ", message,
            " Legacy export continues, but stale Preview paths may remain."
          ),
          call. = FALSE
        ),
        error = function(e) NULL
      )
      return(invisible(stpd_multitrack_preview_export_result(
        status = "cleanup_failed",
        code = "multitrack_preview_cleanup_failed", message = message
      )))
    }
    return(invisible(stpd_multitrack_preview_export_result(
      status = "not_present"
    )))
  }
  tryCatch(
    stpd_write_multitrack_preview_strict(preview, out_dir, parent = ds),
    error = function(e) {
      cleanup_error <- tryCatch(
        {
          stpd_multitrack_preview_cleanup_exports(out_dir)
          NULL
        },
        error = function(cleanup_condition) cleanup_condition
      )
      invalid <- inherits(
        e, "stpd_multitrack_preview_export_validation_error"
      )
      status <- if (invalid) {
        "skipped_invalid_persisted_preview"
      } else {
        "skipped_preview_write_error"
      }
      code <- if (invalid) {
        stpd_multitrack_preview_chr(
          e$code, "multitrack_preview_export_validation_failed"
        )
      } else {
        "multitrack_preview_export_write_failed"
      }
      message <- conditionMessage(e)
      if (!is.null(cleanup_error)) {
        status <- "cleanup_failed"
        code <- "multitrack_preview_cleanup_failed"
        message <- paste(
          message, "Cleanup also failed:", conditionMessage(cleanup_error)
        )
      }
      tryCatch(
        stpd_multitrack_preview_append_export_warning(
          out_dir, status = status, code = code, message = message
        ),
        error = function(log_error) NULL
      )
      # A user may intentionally promote warnings to errors. The Preview export
      # must still never abort the already completed legacy export in that mode.
      tryCatch(
        warning(
          paste0(
            "[multitrack_preview_export_skipped] ", message,
            " Legacy export continues without Preview artifacts."
          ),
          call. = FALSE
        ),
        error = function(warning_as_error) NULL
      )
      invisible(stpd_multitrack_preview_export_result(
        status = status, code = code, message = message
      ))
    }
  )
}
