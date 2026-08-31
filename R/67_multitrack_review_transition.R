# Phase 2B: auditable Review -> Event transitions.
#
# This layer is intentionally post-detection.  It consumes the immutable
# Phase 2A public Preview, appends reviewer decisions, and materializes a
# separate multi-track final product.  It never writes legacy train labels or
# changes the automatic Preview.

stpd_multitrack_review_schema_version <- function() {
  "stpd_multitrack_review_transition_v1"
}

stpd_multitrack_review_conflict_policy_version <- function() {
  "d014_d021_event_state_coexistence_v2"
}

stpd_multitrack_review_result_key <- function() {
  "multitrack_review"
}

stpd_multitrack_review_abort <- function(code, message) {
  condition <- structure(
    list(message = as.character(message)[1], call = NULL, code = as.character(code)[1]),
    class = c("stpd_multitrack_review_error", "error", "condition")
  )
  stop(condition)
}

stpd_multitrack_review_chr <- function(x, default = "") {
  value <- as.character(x %||% default)
  if (length(value) == 0L || is.na(value[1])) default else value[1]
}

stpd_multitrack_review_int <- function(x, default = NA_integer_) {
  value <- suppressWarnings(as.integer(x %||% default))
  if (length(value) == 0L || is.na(value[1])) default else value[1]
}

stpd_multitrack_review_sha256 <- function(x, serialize = TRUE) {
  if (!requireNamespace("digest", quietly = TRUE)) {
    stpd_multitrack_review_abort(
      "digest_dependency_missing",
      "The digest package is required for Phase 2B provenance hashes."
    )
  }
  digest::digest(x, algo = "sha256", serialize = serialize)
}

stpd_multitrack_review_normalize_table <- function(x, sort_rows = TRUE) {
  if (is.null(x) || !is.data.frame(x)) return(data.frame())
  out <- x
  out[] <- lapply(out, function(value) {
    if (is.factor(value)) as.character(value) else value
  })
  if (isTRUE(sort_rows) && nrow(out) > 1L && ncol(out) > 0L) {
    keys <- lapply(out, function(value) {
      if (is.list(value)) vapply(value, stpd_multitrack_review_sha256, character(1))
      else if (is.numeric(value)) {
        result <- sprintf("%+.17e", as.numeric(value))
        result[is.na(value)] <- "<NA>"
        result
      } else {
        result <- as.character(value)
        result[is.na(result)] <- "<NA>"
        result
      }
    })
    ord <- do.call(order, c(keys, list(na.last = TRUE, method = "radix")))
    out <- out[ord, , drop = FALSE]
  }
  rownames(out) <- NULL
  out
}

stpd_multitrack_review_table_hash <- function(x) {
  stpd_multitrack_review_sha256(stpd_multitrack_review_normalize_table(x))
}

stpd_multitrack_review_empty_history <- function() {
  data.frame(
    schema_version = character(),
    transition_sequence = integer(),
    transition_id = character(),
    operation_id = character(),
    transition_action = character(),
    transition_status = character(),
    train = character(),
    source_review_interval_id = character(),
    source_candidate_id = character(),
    source_candidate_key = character(),
    source_candidate_layer = character(),
    source_candidate_source = character(),
    source_start_isi = integer(),
    source_end_isi = integer(),
    source_row_sha256 = character(),
    review_target_track = character(),
    review_target_label = character(),
    proposed_effect = character(),
    target_event_interval_id = character(),
    parent_preview_schema_version = character(),
    parent_preview_policy_hash = character(),
    parent_preview_run_id = character(),
    parent_preview_params_hash = character(),
    parent_preview_manifest_sha256 = character(),
    precondition_sha256 = character(),
    previous_transition_id = character(),
    previous_transition_sha256 = character(),
    conflict_policy_version = character(),
    reviewer = character(),
    reason = character(),
    server_time_utc = character(),
    transition_sha256 = character(),
    stringsAsFactors = FALSE
  )
}

stpd_multitrack_review_empty_current <- function() {
  data.frame(
    schema_version = character(),
    train = character(),
    source_review_interval_id = character(),
    current_status = character(),
    current_transition_id = character(),
    current_transition_sha256 = character(),
    source_row_sha256 = character(),
    proposed_effect = character(),
    target_event_interval_id = character(),
    reviewer = character(),
    reason = character(),
    server_time_utc = character(),
    stringsAsFactors = FALSE
  )
}

stpd_multitrack_review_empty_manual_intervals <- function() {
  data.frame(
    schema_version = character(),
    run_id = character(),
    params_hash = character(),
    authoritative = logical(),
    train = character(),
    source_review_interval_id = character(),
    source_transition_id = character(),
    manual_interval_id = character(),
    semantic_track = character(),
    label = character(),
    start_isi = integer(),
    end_isi = integer(),
    n_isi = integer(),
    start_time_sec = double(),
    end_time_sec = double(),
    action_effect = character(),
    linked_event_interval_id = character(),
    creates_new_event = logical(),
    stringsAsFactors = FALSE
  )
}

stpd_multitrack_review_empty_final_intervals <- function() {
  data.frame(
    schema_version = character(),
    run_id = character(),
    params_hash = character(),
    authoritative = logical(),
    train = character(),
    final_interval_id = character(),
    semantic_track = character(),
    label = character(),
    start_isi = integer(),
    end_isi = integer(),
    n_isi = integer(),
    start_time_sec = double(),
    end_time_sec = double(),
    final_source = character(),
    source_preview_interval_id = character(),
    source_review_interval_id = character(),
    source_transition_id = character(),
    stringsAsFactors = FALSE
  )
}

stpd_multitrack_review_empty_links <- function() {
  data.frame(
    schema_version = character(),
    run_id = character(),
    params_hash = character(),
    authoritative = logical(),
    train = character(),
    relationship_id = character(),
    relationship_type = character(),
    source_review_interval_id = character(),
    source_transition_id = character(),
    target_final_interval_id = character(),
    action_effect = character(),
    non_destructive = logical(),
    stringsAsFactors = FALSE
  )
}

stpd_multitrack_review_empty_per_isi <- function() {
  data.frame(
    schema_version = character(),
    run_id = character(),
    params_hash = character(),
    authoritative = logical(),
    train = character(),
    isi_index = integer(),
    timestamp_sec = double(),
    ISI_sec = double(),
    pattern_final_event = character(),
    pattern_final_event_interval_id = character(),
    pattern_final_state = character(),
    pattern_final_state_interval_id = character(),
    pattern_final_gap = character(),
    pattern_final_gap_interval_id = character(),
    pattern_final_review = character(),
    pattern_final_review_interval_id = character(),
    stringsAsFactors = FALSE
  )
}

stpd_multitrack_review_empty_invariants <- function() {
  data.frame(
    schema_version = character(),
    run_id = character(),
    params_hash = character(),
    check_name = character(),
    status = character(),
    message = character(),
    stringsAsFactors = FALSE
  )
}

stpd_multitrack_review_empty_metadata <- function() {
  data.frame(
    schema_version = character(),
    conflict_policy_version = character(),
    lifecycle_status = character(),
    authoritative = logical(),
    run_id = character(),
    params_hash = character(),
    parent_preview_schema_version = character(),
    parent_preview_policy_hash = character(),
    parent_preview_manifest_sha256 = character(),
    transition_history_sha256 = character(),
    product_sha256 = character(),
    transition_n = integer(),
    confirmed_n = integer(),
    manual_interval_n = integer(),
    final_interval_n = integer(),
    final_relationship_n = integer(),
    final_per_isi_n = integer(),
    legacy_projection_changed = logical(),
    automatic_preview_changed = logical(),
    stale_reason = character(),
    stringsAsFactors = FALSE
  )
}

stpd_multitrack_review_empty_manifest <- function() {
  data.frame(
    schema_version = character(),
    run_id = character(),
    params_hash = character(),
    authoritative = logical(),
    table_name = character(),
    file_name = character(),
    row_count = integer(),
    column_count = integer(),
    column_types = character(),
    table_sha256 = character(),
    stringsAsFactors = FALSE
  )
}

stpd_multitrack_review_column_types <- function(x) {
  paste(paste(names(x), vapply(x, typeof, character(1)), sep = ":"), collapse = ";")
}

stpd_multitrack_review_preview_manifest_hash <- function(preview) {
  stpd_multitrack_review_table_hash(preview$table_manifest %||% data.frame())
}

stpd_multitrack_review_source_row_hash <- function(row) {
  keep <- setdiff(names(row), character())
  stpd_multitrack_review_table_hash(row[, keep, drop = FALSE])
}

stpd_phase2b_has_state <- function(ds) {
  key <- stpd_multitrack_review_result_key()
  is.list(ds) && is.list(ds$results) && key %in% names(ds$results) &&
    !is.null(ds$results[[key]])
}

stpd_multitrack_review_product_raw <- function(ds) {
  if (!stpd_phase2b_has_state(ds)) return(NULL)
  ds$results[[stpd_multitrack_review_result_key()]]
}

stpd_multitrack_review_product <- function(ds) {
  product <- stpd_multitrack_review_product_raw(ds)
  if (is.null(product)) return(NULL)
  lifecycle <- stpd_multitrack_review_chr(
    (product$metadata %||% data.frame())$lifecycle_status
  )
  preview <- if (identical(lifecycle, "active")) {
    stpd_multitrack_review_require_preview(ds)
  } else {
    NULL
  }
  stpd_multitrack_review_validate_product(
    ds, preview = preview, enforce_current_manual_veto = TRUE
  )
  product
}

stpd_multitrack_review_history_raw <- function(ds) {
  product <- stpd_multitrack_review_product_raw(ds)
  if (is.null(product)) return(stpd_multitrack_review_empty_history())
  history <- product$transition_history %||% stpd_multitrack_review_empty_history()
  history
}

stpd_multitrack_review_history <- function(ds) {
  product <- stpd_multitrack_review_product(ds)
  if (is.null(product)) return(stpd_multitrack_review_empty_history())
  stpd_multitrack_review_validate_history(product$transition_history)
}

stpd_multitrack_review_table_prototypes <- function() {
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

stpd_multitrack_review_validate_table_schema <- function(table, prototype, name) {
  if (!is.data.frame(table) ||
      !identical(class(table), "data.frame") ||
      length(setdiff(
        names(attributes(table)), c("names", "class", "row.names")
      )) > 0L ||
      !identical(names(table), names(prototype)) ||
      !identical(vapply(table, typeof, character(1)),
                 vapply(prototype, typeof, character(1)))) {
    stpd_multitrack_review_abort(
      "review_product_table_schema_invalid",
      paste0("Phase 2B table '", name, "' does not match its fixed names/types.")
    )
  }
  invisible(TRUE)
}

stpd_multitrack_review_require_preview <- function(ds) {
  if (is.null(ds) || !is.list(ds) || !is.list(ds$trains)) {
    stpd_multitrack_review_abort(
      "dataset_schema_invalid", "A dataset with a named trains list is required."
    )
  }
  preview <- (ds$results %||% list())$multitrack_preview %||% NULL
  if (is.null(preview)) {
    stpd_multitrack_review_abort(
      "parent_preview_missing",
      "Phase 2B requires a persisted, materialized Phase 2A public Preview."
    )
  }
  if (is.null(ds$params_effective) || !is.list(ds$params_effective)) {
    stpd_multitrack_review_abort(
      "parent_effective_params_missing",
      paste(
        "Phase 2B requires the immutable params_effective payload so the",
        "automatic Preview can be deterministically re-materialized."
      )
    )
  }
  validation <- tryCatch(
    stpd_multitrack_preview_validate_for_export(preview, parent = ds),
    error = function(e) e
  )
  if (inherits(validation, "error")) {
    code <- stpd_multitrack_review_chr(validation$code, "parent_preview_invalid")
    stpd_multitrack_review_abort(
      paste0("parent_", code),
      paste0("The parent automatic Preview is invalid: ", conditionMessage(validation))
    )
  }
  metadata <- preview$metadata
  if (nrow(metadata) != 1L ||
      !identical(stpd_multitrack_review_chr(metadata$materialization_status), "materialized") ||
      isTRUE(metadata$authoritative[1])) {
    stpd_multitrack_review_abort(
      "parent_preview_not_materialized",
      "The parent Preview must be materialized and non-authoritative."
    )
  }
  preview
}

stpd_multitrack_review_normalize_requests <- function(requests) {
  required <- c("train", "source_review_interval_id")
  if (!is.data.frame(requests) || !identical(names(requests), required)) {
    stpd_multitrack_review_abort(
      "request_schema_invalid",
      paste(
        "requests must be a data.frame with exactly two columns in order:",
        "train, source_review_interval_id. Geometry and bare candidate IDs are not accepted."
      )
    )
  }
  out <- data.frame(
    train = trimws(as.character(requests$train)),
    source_review_interval_id = trimws(as.character(requests$source_review_interval_id)),
    stringsAsFactors = FALSE
  )
  if (nrow(out) == 0L || anyNA(out) || any(!nzchar(out$train)) ||
      any(!nzchar(out$source_review_interval_id))) {
    stpd_multitrack_review_abort(
      "request_identity_missing", "Every request must contain a non-empty train and Review interval ID."
    )
  }
  if (anyDuplicated(out)) {
    stpd_multitrack_review_abort(
      "duplicate_request_identity", "A Review interval may appear only once in one atomic request."
    )
  }
  ord <- order(out$train, out$source_review_interval_id, method = "radix")
  out <- out[ord, , drop = FALSE]
  rownames(out) <- NULL
  out
}

stpd_multitrack_review_history_payload <- function(history) {
  if (nrow(history) == 0L) return(stpd_multitrack_review_empty_history())
  history[order(history$transition_sequence, method = "radix"), , drop = FALSE]
}

stpd_multitrack_review_transition_hash <- function(row) {
  payload <- row[, setdiff(names(row), "transition_sha256"), drop = FALSE]
  stpd_multitrack_review_table_hash(payload)
}

stpd_multitrack_review_validate_history <- function(history) {
  prototype <- stpd_multitrack_review_empty_history()
  if (is.null(history)) return(prototype)
  if (!is.data.frame(history)) {
    stpd_multitrack_review_abort(
      "transition_history_schema_invalid",
      "The append-only Phase 2B transition history must be a data frame."
    )
  }
  if (!identical(class(history), "data.frame") ||
      length(setdiff(
        names(attributes(history)), c("names", "class", "row.names")
      )) > 0L) {
    stpd_multitrack_review_abort(
      "transition_history_schema_invalid",
      "The transition history class and attributes must be a plain canonical data.frame."
    )
  }
  if (nrow(history) == 0L) {
    if (!identical(names(history), names(prototype)) ||
        !identical(vapply(history, typeof, character(1)),
                   vapply(prototype, typeof, character(1)))) {
      stpd_multitrack_review_abort(
        "transition_history_schema_invalid",
        "The empty transition history does not match its fixed schema and types."
      )
    }
    return(history)
  }
  expected_names <- names(prototype)
  expected_types <- vapply(prototype, typeof, character(1))
  actual_types <- vapply(history, typeof, character(1))
  if (!identical(names(history), expected_names) ||
      !identical(actual_types, expected_types)) {
    stpd_multitrack_review_abort(
      "transition_history_schema_invalid",
      "The append-only Phase 2B transition history does not match its fixed schema and types."
    )
  }
  if (anyNA(history)) {
    stpd_multitrack_review_abort(
      "transition_history_domain_invalid",
      "The append-only Phase 2B transition history must not contain missing values."
    )
  }
  history <- history[order(history$transition_sequence, method = "radix"), , drop = FALSE]
  if (!identical(as.integer(history$transition_sequence), seq_len(nrow(history))) ||
      anyDuplicated(history$transition_id) || anyDuplicated(history$transition_sha256)) {
    stpd_multitrack_review_abort(
      "transition_history_sequence_invalid",
      "Transition sequence numbers and transition identities must be unique and contiguous."
    )
  }
  hash_columns <- c(
    "source_row_sha256", "parent_preview_policy_hash",
    "parent_preview_params_hash", "parent_preview_manifest_sha256",
    "precondition_sha256", "transition_sha256"
  )
  valid_hash <- vapply(hash_columns, function(name) {
    all(grepl("^[0-9a-f]{64}$", history[[name]]))
  }, logical(1))
  domain_ok <- all(history$schema_version == stpd_multitrack_review_schema_version()) &&
    all(history$transition_action %in% c("confirm", "revoke")) &&
    all(history$transition_status == "applied") &&
    all(history$review_target_track == "event") &&
    all(history$review_target_label == "burst") &&
    all(history$proposed_effect %in% c("create_event", "link_existing_event")) &&
    all(history$conflict_policy_version == stpd_multitrack_review_conflict_policy_version()) &&
    all(nzchar(history$transition_id)) && all(nzchar(history$operation_id)) &&
    all(nzchar(history$train)) && all(nzchar(history$source_review_interval_id)) &&
    all(nzchar(history$source_candidate_id)) &&
    all(nzchar(history$source_candidate_key)) &&
    all(nzchar(history$source_candidate_layer)) &&
    all(nzchar(history$target_event_interval_id)) &&
    all(nzchar(history$parent_preview_schema_version)) &&
    all(nzchar(history$parent_preview_run_id)) &&
    all(nzchar(history$reviewer)) && all(nzchar(history$reason)) &&
    all(nzchar(history$server_time_utc)) &&
    all(is.finite(history$source_start_isi)) &&
    all(is.finite(history$source_end_isi)) &&
    all(history$source_start_isi >= 1L) &&
    all(history$source_start_isi <= history$source_end_isi) && all(valid_hash)
  if (!domain_ok) {
    stpd_multitrack_review_abort(
      "transition_history_domain_invalid",
      "At least one transition value violates the fixed Phase 2B domain contract."
    )
  }
  for (i in seq_len(nrow(history))) {
    expected_transition_id <- paste0(
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
    expected_previous_id <- if (i == 1L) "" else history$transition_id[i - 1L]
    expected_previous_hash <- if (i == 1L) "" else history$transition_sha256[i - 1L]
    if (!identical(history$transition_id[i], expected_transition_id) ||
        !identical(history$previous_transition_id[i], expected_previous_id) ||
        !identical(history$previous_transition_sha256[i], expected_previous_hash) ||
        !identical(history$transition_sha256[i],
                   stpd_multitrack_review_transition_hash(history[i, , drop = FALSE]))) {
      stpd_multitrack_review_abort(
        "transition_history_hash_chain_invalid",
        paste0("Transition history hash-chain validation failed at sequence ", i, ".")
      )
    }
  }

  # The state machine resets at an immutable parent Preview boundary. This
  # permits explicit reconfirmation after a detector rerun while preventing a
  # duplicate confirm inside one run.
  state <- new.env(parent = emptyenv())
  source_payload <- new.env(parent = emptyenv())
  for (i in seq_len(nrow(history))) {
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
      stpd_multitrack_review_abort(
        "transition_history_state_machine_invalid",
        "A confirm/revoke sequence is invalid within one immutable parent Preview."
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
      stpd_multitrack_review_abort(
        "transition_history_source_payload_changed",
        "A Review source changes immutable identity, geometry, or effect within one parent Preview."
      )
    }
    assign(key, payload, envir = source_payload)
    assign(key, if (action == "confirm") "confirmed" else "revoked",
           envir = state)
  }

  for (operation in unique(history$operation_id)) {
    idx <- which(history$operation_id == operation)
    if (!identical(idx, seq.int(min(idx), max(idx)))) {
      stpd_multitrack_review_abort(
        "transition_history_operation_not_contiguous",
        "Rows belonging to one atomic operation must be contiguous."
      )
    }
    shared_fields <- c(
      "transition_action", "precondition_sha256", "reviewer", "reason",
      "server_time_utc", "parent_preview_schema_version",
      "parent_preview_policy_hash", "parent_preview_run_id",
      "parent_preview_params_hash", "parent_preview_manifest_sha256",
      "conflict_policy_version"
    )
    if (any(vapply(shared_fields, function(field) {
      length(unique(as.character(history[[field]][idx]))) != 1L
    }, logical(1)))) {
      stpd_multitrack_review_abort(
        "transition_history_operation_payload_invalid",
        "Rows belonging to one atomic operation do not share one audit/precondition payload."
      )
    }
    confirmed <- idx[history$transition_action[idx] == "confirm"]
    if (length(confirmed) > 1L) {
      for (a in seq_len(length(confirmed) - 1L)) {
        for (b in seq.int(a + 1L, length(confirmed))) {
          i <- confirmed[a]
          j <- confirmed[b]
          if (identical(history$train[i], history$train[j]) &&
              stpd_multitrack_review_span_overlap(
                history$source_start_isi[i], history$source_end_isi[i],
                history$source_start_isi[j], history$source_end_isi[j]
              )) {
            stpd_multitrack_review_abort(
              "transition_history_operation_overlap_invalid",
              "One atomic confirmation operation contains overlapping Review intervals."
            )
          }
        }
      }
    }
  }
  rownames(history) <- NULL
  history
}

stpd_multitrack_review_current_from_history <- function(history, preview = NULL) {
  history <- stpd_multitrack_review_validate_history(history)
  if (nrow(history) == 0L) return(stpd_multitrack_review_empty_current())
  if (!is.null(preview)) {
    meta <- preview$metadata
    history <- history[
      history$parent_preview_schema_version ==
        stpd_multitrack_review_chr(meta$schema_version) &
        history$parent_preview_policy_hash ==
          stpd_multitrack_review_chr(meta$policy_hash) &
        history$parent_preview_run_id == stpd_multitrack_review_chr(meta$run_id) &
        history$parent_preview_params_hash ==
          stpd_multitrack_review_chr(meta$params_hash) &
        history$parent_preview_manifest_sha256 ==
          stpd_multitrack_review_preview_manifest_hash(preview),
      , drop = FALSE
    ]
  }
  if (nrow(history) == 0L) return(stpd_multitrack_review_empty_current())
  key <- paste(history$train, history$source_review_interval_id, sep = "\r")
  last <- !duplicated(key, fromLast = TRUE)
  latest <- history[last, , drop = FALSE]
  latest <- latest[order(latest$train, latest$source_review_interval_id, method = "radix"), , drop = FALSE]
  data.frame(
    schema_version = latest$schema_version,
    train = latest$train,
    source_review_interval_id = latest$source_review_interval_id,
    current_status = ifelse(latest$transition_action == "confirm", "confirmed", "revoked"),
    current_transition_id = latest$transition_id,
    current_transition_sha256 = latest$transition_sha256,
    source_row_sha256 = latest$source_row_sha256,
    proposed_effect = latest$proposed_effect,
    target_event_interval_id = latest$target_event_interval_id,
    reviewer = latest$reviewer,
    reason = latest$reason,
    server_time_utc = latest$server_time_utc,
    stringsAsFactors = FALSE
  )
}

stpd_multitrack_review_burst_coexisting_states <- function() {
  c(
    "high_frequency_spiking",
    "high_frequency_tonic",
    "high_frequency_irregular_state",
    "hf_unresolved",
    "tonic"
  )
}

stpd_multitrack_review_span_overlap <- function(a_start, a_end, b_start, b_end) {
  is.finite(a_start) && is.finite(a_end) && is.finite(b_start) && is.finite(b_end) &&
    max(a_start, b_start) <= min(a_end, b_end)
}

stpd_multitrack_review_manual_veto_payload <- function(dat, start, end) {
  empty <- data.frame(
    isi_index = integer(),
    pattern_manual_negative = character(),
    pattern_manual_legacy_negative = character(),
    stringsAsFactors = FALSE
  )
  if (!is.data.frame(dat) || !is.finite(start) || !is.finite(end) ||
      end < start || nrow(dat) == 0L) {
    return(empty)
  }
  first <- max(1L, as.integer(start))
  last <- min(nrow(dat), as.integer(end))
  if (last < first) return(empty)
  idx <- seq.int(first, last)
  canonical <- function(column) {
    value <- if (column %in% names(dat)) {
      as.character(dat[[column]][idx])
    } else {
      rep("", length(idx))
    }
    value[is.na(value)] <- ""
    tolower(trimws(value))
  }
  negative <- canonical("pattern_manual_negative")
  legacy <- canonical("pattern_manual")
  legacy_tokens <- c(
    "not_burst", "hard_negative_burst", "not burst", "not-burst"
  )
  # pattern_manual is only a compatibility escape hatch for the four legacy
  # negative tokens. Positive manual labels and newer negative vocabulary must
  # not acquire new meaning through this column.
  legacy[!(legacy %in% legacy_tokens)] <- ""
  data.frame(
    isi_index = as.integer(idx),
    pattern_manual_negative = negative,
    pattern_manual_legacy_negative = legacy,
    stringsAsFactors = FALSE
  )
}

stpd_multitrack_review_manual_veto <- function(dat, start, end) {
  payload <- stpd_multitrack_review_manual_veto_payload(dat, start, end)
  if (nrow(payload) == 0L) return(FALSE)
  negative_tokens <- c(
    "not_burst", "hard_negative_burst", "not burst", "not-burst",
    "hard_negative"
  )
  any(payload$pattern_manual_negative %in% negative_tokens) ||
    any(nzchar(payload$pattern_manual_legacy_negative))
}

stpd_multitrack_review_effect_interval_id <- function(run_id, train, source_review_interval_id) {
  paste0(
    "mtr_event_",
    substr(stpd_multitrack_review_sha256(
      paste(stpd_multitrack_review_schema_version(), run_id, train,
            source_review_interval_id, sep = "|"), serialize = FALSE
    ), 1L, 24L)
  )
}

stpd_multitrack_review_relationship_id <- function(run_id, train, source_id, target_id) {
  paste0(
    "mtr_link_",
    substr(stpd_multitrack_review_sha256(
      paste(run_id, train, source_id, target_id, sep = "|"), serialize = FALSE
    ), 1L, 24L)
  )
}

stpd_multitrack_review_validate_history_against_preview <- function(
    ds, preview, history) {
  history <- stpd_multitrack_review_validate_history(history)
  if (nrow(history) == 0L) return(invisible(TRUE))
  metadata <- preview$metadata
  run_id <- stpd_multitrack_review_chr(metadata$run_id)
  params_hash <- stpd_multitrack_review_chr(metadata$params_hash)
  current <- history[
    history$parent_preview_schema_version ==
      stpd_multitrack_review_chr(metadata$schema_version) &
      history$parent_preview_policy_hash ==
        stpd_multitrack_review_chr(metadata$policy_hash) &
      history$parent_preview_run_id == run_id &
      history$parent_preview_params_hash == params_hash &
      history$parent_preview_manifest_sha256 ==
        stpd_multitrack_review_preview_manifest_hash(preview),
    , drop = FALSE
  ]
  if (nrow(current) == 0L) return(invisible(TRUE))
  current <- current[order(current$transition_sequence, method = "radix"), , drop = FALSE]
  manifest_hash <- stpd_multitrack_review_preview_manifest_hash(preview)
  if (!all(current$parent_preview_schema_version ==
           stpd_multitrack_review_chr(metadata$schema_version)) ||
      !all(current$parent_preview_policy_hash ==
           stpd_multitrack_review_chr(metadata$policy_hash)) ||
      !all(current$parent_preview_manifest_sha256 == manifest_hash)) {
    stpd_multitrack_review_abort(
      "transition_parent_preview_mismatch",
      "A current-run transition is not bound to the exact parent Preview identity."
    )
  }

  intervals <- preview$intervals
  active <- intervals[intervals$active_in_preview %in% TRUE, , drop = FALSE]
  automatic_events <- active[active$semantic_track == "event", , drop = FALSE]
  automatic_states <- active[active$semantic_track == "state", , drop = FALSE]
  automatic_gaps <- active[active$semantic_track == "gap", , drop = FALSE]

  # One atomic operation may not confirm overlapping Review intervals.
  confirm_rows <- current[current$transition_action == "confirm", , drop = FALSE]
  if (nrow(confirm_rows) > 1L) {
    for (operation in unique(confirm_rows$operation_id)) {
      part <- confirm_rows[confirm_rows$operation_id == operation, , drop = FALSE]
      if (nrow(part) <= 1L) next
      for (i in seq_len(nrow(part) - 1L)) {
        for (j in seq.int(i + 1L, nrow(part))) {
          if (identical(part$train[i], part$train[j]) &&
              stpd_multitrack_review_span_overlap(
                part$source_start_isi[i], part$source_end_isi[i],
                part$source_start_isi[j], part$source_end_isi[j]
              )) {
            stpd_multitrack_review_abort(
              "transition_batch_overlap_invalid",
              "Persisted confirmations from one atomic operation overlap."
            )
          }
        }
      }
    }
  }

  state <- list()
  active_effect <- list()
  created_events <- data.frame(
    train = character(), interval_id = character(), label = character(),
    start_isi = integer(), end_isi = integer(), source_key = character(),
    stringsAsFactors = FALSE
  )
  for (i in seq_len(nrow(current))) {
    transition <- current[i, , drop = FALSE]
    hit <- which(
      intervals$train == transition$train &
        intervals$interval_id == transition$source_review_interval_id
    )
    if (length(hit) != 1L) {
      stpd_multitrack_review_abort(
        "transition_source_interval_missing",
        "A transition source does not resolve uniquely in the parent Preview."
      )
    }
    source <- intervals[hit, , drop = FALSE]
    source_ok <- source$semantic_track[1] == "review" &&
      source$label[1] == "possible_burst" && isTRUE(source$active_in_preview[1]) &&
      source$review_target_track[1] == "event" &&
      source$review_target_label[1] == "burst" &&
      isTRUE(source$review_promotion_required[1]) &&
      identical(transition$source_row_sha256,
                stpd_multitrack_review_source_row_hash(source)) &&
      identical(transition$source_candidate_id, as.character(source$candidate_id)) &&
      identical(transition$source_candidate_key, as.character(source$candidate_key)) &&
      identical(transition$source_candidate_layer, as.character(source$candidate_layer)) &&
      identical(transition$source_candidate_source, as.character(source$candidate_source)) &&
      identical(transition$source_start_isi, as.integer(source$start_isi)) &&
      identical(transition$source_end_isi, as.integer(source$end_isi))
    if (!source_ok) {
      stpd_multitrack_review_abort(
        "transition_source_semantics_mismatch",
        "A transition's source identity, geometry, or candidate provenance differs from its parent Review row."
      )
    }

    key <- paste(transition$train, transition$source_review_interval_id, sep = "\r")
    status <- state[[key]] %||% "unreviewed"
    if (transition$transition_action == "confirm") {
      if (identical(status, "confirmed")) {
        stpd_multitrack_review_abort(
          "transition_state_machine_invalid",
          "A Review interval was confirmed twice without an intervening revoke."
        )
      }
      start <- transition$source_start_isi
      end <- transition$source_end_isi
      existing <- data.frame(
        train = c(as.character(automatic_events$train), as.character(created_events$train)),
        interval_id = c(as.character(automatic_events$interval_id), as.character(created_events$interval_id)),
        label = c(as.character(automatic_events$label), as.character(created_events$label)),
        start_isi = c(as.integer(automatic_events$start_isi), as.integer(created_events$start_isi)),
        end_isi = c(as.integer(automatic_events$end_isi), as.integer(created_events$end_isi)),
        stringsAsFactors = FALSE
      )
      overlap <- which(existing$train == transition$train & vapply(
        seq_len(nrow(existing)), function(j) {
          stpd_multitrack_review_span_overlap(
            start, end, existing$start_isi[j], existing$end_isi[j]
          )
        }, logical(1)
      ))
      if (length(overlap) == 0L) {
        expected_effect <- "create_event"
        expected_target <- stpd_multitrack_review_effect_interval_id(
          run_id, transition$train, transition$source_review_interval_id
        )
      } else {
        exact <- overlap[
          existing$start_isi[overlap] == start & existing$end_isi[overlap] == end &
            existing$label[overlap] == "burst"
        ]
        if (length(exact) != length(overlap) || length(exact) == 0L) {
          stpd_multitrack_review_abort(
            "transition_effect_event_conflict",
            "A persisted confirmation conflicts with the Event track under D-015."
          )
        }
        expected_effect <- "link_existing_event"
        expected_target <- sort(unique(existing$interval_id[exact]), method = "radix")[1]
      }
      gap_conflict <- any(automatic_gaps$train == transition$train & vapply(
        seq_len(nrow(automatic_gaps)), function(j) {
          stpd_multitrack_review_span_overlap(
            start, end, automatic_gaps$start_isi[j], automatic_gaps$end_isi[j]
          )
        }, logical(1)
      ))
      state_hits <- which(automatic_states$train == transition$train & vapply(
        seq_len(nrow(automatic_states)), function(j) {
          stpd_multitrack_review_span_overlap(
            start, end, automatic_states$start_isi[j], automatic_states$end_isi[j]
          )
        }, logical(1)
      ))
      state_conflict <- any(
        !automatic_states$label[state_hits] %in%
          stpd_multitrack_review_burst_coexisting_states()
      )
      if (gap_conflict || state_conflict ||
          !identical(transition$proposed_effect, expected_effect) ||
          !identical(transition$target_event_interval_id, expected_target)) {
        stpd_multitrack_review_abort(
          "transition_effect_semantics_mismatch",
          "A persisted confirmation effect/target does not replay under D-015."
        )
      }
      if (expected_effect == "create_event") {
        created_events <- dplyr::bind_rows(created_events, data.frame(
          train = transition$train,
          interval_id = expected_target,
          label = "burst",
          start_isi = as.integer(start),
          end_isi = as.integer(end),
          source_key = key,
          stringsAsFactors = FALSE
        ))
      }
      state[[key]] <- "confirmed"
      active_effect[[key]] <- c(effect = expected_effect, target = expected_target)
    } else {
      if (!identical(status, "confirmed") || is.null(active_effect[[key]]) ||
          !identical(transition$proposed_effect, unname(active_effect[[key]]["effect"])) ||
          !identical(transition$target_event_interval_id, unname(active_effect[[key]]["target"]))) {
        stpd_multitrack_review_abort(
          "transition_state_machine_invalid",
          "A revoke does not identify the currently confirmed Review effect."
        )
      }
      if (active_effect[[key]]["effect"] == "create_event") {
        created_events <- created_events[created_events$source_key != key, , drop = FALSE]
      }
      state[[key]] <- "revoked"
      active_effect[[key]] <- NULL
    }
  }
  invisible(TRUE)
}

stpd_multitrack_review_preflight_rows <- function(ds, preview, requests, action, history) {
  intervals <- preview$intervals
  active <- intervals[intervals$active_in_preview %in% TRUE, , drop = FALSE]
  current <- stpd_multitrack_review_current_from_history(history, preview = preview)
  current_key <- paste(current$train, current$source_review_interval_id, sep = "\r")

  auto_events <- active[active$semantic_track == "event", , drop = FALSE]
  auto_states <- active[active$semantic_track == "state", , drop = FALSE]
  auto_gaps <- active[active$semantic_track == "gap", , drop = FALSE]

  confirmed <- current[current$current_status == "confirmed", , drop = FALSE]
  latest_history <- history[history$transition_id %in% confirmed$current_transition_id, , drop = FALSE]
  created <- latest_history[latest_history$transition_action == "confirm" &
                              latest_history$proposed_effect == "create_event", , drop = FALSE]
  existing_events <- data.frame(
    train = c(as.character(auto_events$train), as.character(created$train)),
    interval_id = c(as.character(auto_events$interval_id),
                    as.character(created$target_event_interval_id)),
    label = c(as.character(auto_events$label), rep("burst", nrow(created))),
    start_isi = c(as.integer(auto_events$start_isi), as.integer(created$source_start_isi)),
    end_isi = c(as.integer(auto_events$end_isi), as.integer(created$source_end_isi)),
    source = c(rep("automatic_preview", nrow(auto_events)),
               rep("phase2b_confirmed", nrow(created))),
    stringsAsFactors = FALSE
  )

  rows <- vector("list", nrow(requests))
  for (i in seq_len(nrow(requests))) {
    train <- requests$train[i]
    source_id <- requests$source_review_interval_id[i]
    hit <- which(intervals$train == train & intervals$interval_id == source_id)
    code <- ""
    message <- ""
    eligible <- TRUE
    source <- NULL
    if (length(hit) != 1L) {
      eligible <- FALSE
      code <- if (length(hit) == 0L) "source_review_interval_missing" else "source_review_interval_ambiguous"
      message <- "The train-qualified Review interval identity did not resolve uniquely."
    } else {
      source <- intervals[hit, , drop = FALSE]
      d012_ok <- identical(stpd_multitrack_review_chr(source$interval_kind), "candidate") &&
        identical(stpd_multitrack_review_chr(source$semantic_track), "review") &&
        identical(stpd_multitrack_review_chr(source$label), "possible_burst") &&
        isTRUE(source$active_in_preview[1]) &&
        identical(stpd_multitrack_review_chr(source$review_target_track), "event") &&
        identical(stpd_multitrack_review_chr(source$review_target_label), "burst") &&
        isTRUE(source$review_promotion_required[1])
      if (!d012_ok) {
        eligible <- FALSE
        code <- "source_review_interval_ineligible"
        message <- "Only an active D-012 possible_burst Review interval targeting Event/Burst is eligible."
      }
    }

    status_hit <- match(paste(train, source_id, sep = "\r"), current_key)
    existing_status <- if (is.na(status_hit)) "unreviewed" else current$current_status[status_hit]
    if (eligible && action == "confirm" && existing_status == "confirmed") {
      eligible <- FALSE
      code <- "source_review_interval_already_confirmed"
      message <- "This Review interval is already confirmed."
    }
    if (eligible && action == "revoke" && existing_status != "confirmed") {
      eligible <- FALSE
      code <- "source_review_interval_not_confirmed"
      message <- "Only a currently confirmed Review interval can be revoked."
    }

    start <- if (is.null(source)) NA_integer_ else as.integer(source$start_isi[1])
    end <- if (is.null(source)) NA_integer_ else as.integer(source$end_isi[1])
    dat <- ds$trains[[train]] %||% NULL
    manual_negative_payload <-
      stpd_multitrack_review_manual_veto_payload(dat, start, end)
    manual_negative_hash <-
      stpd_multitrack_review_sha256(manual_negative_payload)
    veto <- is.finite(start) && is.finite(end) &&
      stpd_multitrack_review_manual_veto(dat, start, end)
    if (eligible && action == "confirm" && veto) {
      eligible <- FALSE
      code <- "manual_not_burst_veto"
      message <- "An overlapping manual not_burst label blocks confirmation."
    }

    effect <- ""
    target <- ""
    if (eligible && action == "confirm") {
      overlap_events <- which(existing_events$train == train & vapply(
        seq_len(nrow(existing_events)), function(j) {
          stpd_multitrack_review_span_overlap(
            start, end, existing_events$start_isi[j], existing_events$end_isi[j]
          )
        }, logical(1)
      ))
      if (length(overlap_events) > 0L) {
        exact_burst <- overlap_events[
          existing_events$start_isi[overlap_events] == start &
            existing_events$end_isi[overlap_events] == end &
            existing_events$label[overlap_events] == "burst"
        ]
        if (length(exact_burst) == length(overlap_events) && length(exact_burst) > 0L) {
          target_ids <- sort(unique(existing_events$interval_id[exact_burst]), method = "radix")
          target <- target_ids[1]
          effect <- "link_existing_event"
        } else {
          eligible <- FALSE
          code <- "event_track_conflict"
          message <- "A partial, nested, or different-label Event overlap blocks confirmation."
        }
      } else {
        effect <- "create_event"
        target <- stpd_multitrack_review_effect_interval_id(
          stpd_multitrack_review_chr(preview$metadata$run_id), train, source_id
        )
      }

      if (eligible) {
        gap_overlap <- any(auto_gaps$train == train & vapply(
          seq_len(nrow(auto_gaps)), function(j) {
            stpd_multitrack_review_span_overlap(
              start, end, auto_gaps$start_isi[j], auto_gaps$end_isi[j]
            )
          }, logical(1)
        ))
        if (gap_overlap) {
          eligible <- FALSE
          code <- "gap_track_conflict"
          message <- "An active Pause/Gap interval blocks confirmation."
        }
      }

      if (eligible) {
        state_hits <- which(auto_states$train == train & vapply(
          seq_len(nrow(auto_states)), function(j) {
            stpd_multitrack_review_span_overlap(
              start, end, auto_states$start_isi[j], auto_states$end_isi[j]
            )
          }, logical(1)
        ))
        blocked_state <- state_hits[
          !(auto_states$label[state_hits] %in%
              stpd_multitrack_review_burst_coexisting_states())
        ]
        if (length(blocked_state) > 0L) {
          eligible <- FALSE
          code <- "state_track_split_regate_required"
          message <- "An unrecognized State overlap requires an explicit coexistence or split-and-re-gate rule."
        }
      }
    }
    if (eligible && action == "revoke") {
      effect <- current$proposed_effect[status_hit]
      target <- current$target_event_interval_id[status_hit]
    }

    rows[[i]] <- data.frame(
      train = train,
      source_review_interval_id = source_id,
      action = action,
      eligible = isTRUE(eligible),
      failure_code = code,
      failure_message = message,
      existing_status = existing_status,
      source_row_sha256 = if (is.null(source)) "" else stpd_multitrack_review_source_row_hash(source),
      source_candidate_id = if (is.null(source)) "" else stpd_multitrack_review_chr(source$candidate_id),
      source_candidate_key = if (is.null(source)) "" else stpd_multitrack_review_chr(source$candidate_key),
      source_candidate_layer = if (is.null(source)) "" else stpd_multitrack_review_chr(source$candidate_layer),
      source_candidate_source = if (is.null(source)) "" else stpd_multitrack_review_chr(source$candidate_source),
      start_isi = start,
      end_isi = end,
      manual_negative_sha256 = manual_negative_hash,
      manual_not_burst_veto = isTRUE(veto),
      proposed_effect = effect,
      target_event_interval_id = target,
      stringsAsFactors = FALSE
    )
  }
  decisions <- dplyr::bind_rows(rows)

  if (action == "confirm" && nrow(decisions) > 1L) {
    conflict <- rep(FALSE, nrow(decisions))
    for (i in seq_len(nrow(decisions) - 1L)) {
      for (j in seq.int(i + 1L, nrow(decisions))) {
        if (identical(decisions$train[i], decisions$train[j]) &&
            stpd_multitrack_review_span_overlap(
              decisions$start_isi[i], decisions$end_isi[i],
              decisions$start_isi[j], decisions$end_isi[j]
            )) {
          conflict[c(i, j)] <- TRUE
        }
      }
    }
    if (any(conflict)) {
      decisions$eligible[conflict] <- FALSE
      decisions$failure_code[conflict] <- "requested_promotions_overlap"
      decisions$failure_message[conflict] <-
        "Two Review intervals in the same atomic confirmation request overlap."
    }
  }
  decisions
}

#' Preflight a Phase 2B Review transition
#'
#' @param ds Dataset containing a materialized Phase 2A Preview.
#' @param requests A data frame with exactly `train` and
#'   `source_review_interval_id` columns.
#' @param action Either `"confirm"` or `"revoke"`.
#' @return A preflight object. Pass its `precondition_sha256` unchanged to the
#'   corresponding mutation call.
#' @export
stpd_multitrack_review_preflight <- function(ds, requests, action = c("confirm", "revoke")) {
  action <- match.arg(action)
  requests <- stpd_multitrack_review_normalize_requests(requests)
  preview <- stpd_multitrack_review_require_preview(ds)
  stpd_multitrack_review_validate_product(
    ds, preview = preview,
    enforce_current_manual_veto = !identical(action, "revoke")
  )
  history <- stpd_multitrack_review_validate_history(
    stpd_multitrack_review_history_raw(ds)
  )
  decisions <- stpd_multitrack_review_preflight_rows(ds, preview, requests, action, history)
  metadata <- preview$metadata
  parent_manifest_hash <- stpd_multitrack_review_preview_manifest_hash(preview)
  history_hash <- stpd_multitrack_review_table_hash(history)
  payload <- list(
    schema_version = stpd_multitrack_review_schema_version(),
    conflict_policy_version = stpd_multitrack_review_conflict_policy_version(),
    action = action,
    parent_preview_schema_version = stpd_multitrack_review_chr(metadata$schema_version),
    parent_preview_policy_hash = stpd_multitrack_review_chr(metadata$policy_hash),
    parent_preview_run_id = stpd_multitrack_review_chr(metadata$run_id),
    parent_preview_params_hash = stpd_multitrack_review_chr(metadata$params_hash),
    parent_preview_manifest_sha256 = parent_manifest_hash,
    transition_history_sha256 = history_hash,
    decisions = stpd_multitrack_review_normalize_table(decisions)
  )
  out <- list(
    schema_version = stpd_multitrack_review_schema_version(),
    conflict_policy_version = stpd_multitrack_review_conflict_policy_version(),
    action = action,
    eligible = nrow(decisions) > 0L && all(decisions$eligible),
    decisions = decisions,
    parent_preview_run_id = payload$parent_preview_run_id,
    parent_preview_params_hash = payload$parent_preview_params_hash,
    parent_preview_manifest_sha256 = parent_manifest_hash,
    transition_history_sha256 = history_hash,
    precondition_sha256 = stpd_multitrack_review_sha256(payload)
  )
  class(out) <- c("stpd_multitrack_review_preflight", "list")
  out
}

stpd_multitrack_review_time_at <- function(dat, index) {
  if (!is.data.frame(dat) || !("timestamp_sec" %in% names(dat)) ||
      !is.finite(index) || index < 1L || index > nrow(dat)) return(NA_real_)
  suppressWarnings(as.numeric(dat$timestamp_sec[index]))
}

stpd_multitrack_review_manual_rows <- function(ds, preview, history, current) {
  confirmed <- current[current$current_status == "confirmed", , drop = FALSE]
  if (nrow(confirmed) == 0L) return(stpd_multitrack_review_empty_manual_intervals())
  latest <- history[match(confirmed$current_transition_id, history$transition_id), , drop = FALSE]
  rows <- vector("list", nrow(latest))
  for (i in seq_len(nrow(latest))) {
    row <- latest[i, , drop = FALSE]
    dat <- ds$trains[[row$train]]
    start <- as.integer(row$source_start_isi)
    end <- as.integer(row$source_end_isi)
    rows[[i]] <- data.frame(
      schema_version = stpd_multitrack_review_schema_version(),
      run_id = stpd_multitrack_review_chr(preview$metadata$run_id),
      params_hash = stpd_multitrack_review_chr(preview$metadata$params_hash),
      authoritative = TRUE,
      train = row$train,
      source_review_interval_id = row$source_review_interval_id,
      source_transition_id = row$transition_id,
      manual_interval_id = if (row$proposed_effect == "create_event") {
        row$target_event_interval_id
      } else {
        paste0("mtr_confirmation_", substr(row$transition_sha256, 1L, 24L))
      },
      semantic_track = "event",
      label = "burst",
      start_isi = start,
      end_isi = end,
      n_isi = as.integer(end - start + 1L),
      start_time_sec = stpd_multitrack_review_time_at(dat, start - 1L),
      end_time_sec = stpd_multitrack_review_time_at(dat, end),
      action_effect = row$proposed_effect,
      linked_event_interval_id = row$target_event_interval_id,
      creates_new_event = identical(row$proposed_effect, "create_event"),
      stringsAsFactors = FALSE
    )
  }
  out <- dplyr::bind_rows(stpd_multitrack_review_empty_manual_intervals(), rows)
  out[order(out$train, out$start_isi, out$end_isi, out$source_review_interval_id,
            method = "radix"), , drop = FALSE]
}

stpd_multitrack_review_final_interval_rows <- function(preview, current, manual) {
  confirmed_ids <- current$source_review_interval_id[current$current_status == "confirmed"]
  automatic <- preview$intervals[
    preview$intervals$active_in_preview &
      !(preview$intervals$semantic_track == "review" &
          preview$intervals$interval_id %in% confirmed_ids),
    , drop = FALSE
  ]
  automatic_rows <- if (nrow(automatic) == 0L) {
    stpd_multitrack_review_empty_final_intervals()
  } else {
    data.frame(
      schema_version = stpd_multitrack_review_schema_version(),
      run_id = as.character(automatic$run_id),
      params_hash = as.character(automatic$params_hash),
      authoritative = TRUE,
      train = as.character(automatic$train),
      final_interval_id = as.character(automatic$interval_id),
      semantic_track = as.character(automatic$semantic_track),
      label = as.character(automatic$label),
      start_isi = as.integer(automatic$start_isi),
      end_isi = as.integer(automatic$end_isi),
      n_isi = as.integer(automatic$n_isi),
      start_time_sec = as.numeric(automatic$start_time_sec),
      end_time_sec = as.numeric(automatic$end_time_sec),
      final_source = "automatic_preview",
      source_preview_interval_id = as.character(automatic$interval_id),
      source_review_interval_id = "",
      source_transition_id = "",
      stringsAsFactors = FALSE
    )
  }
  created <- manual[manual$creates_new_event, , drop = FALSE]
  created_rows <- if (nrow(created) == 0L) {
    stpd_multitrack_review_empty_final_intervals()
  } else {
    data.frame(
      schema_version = created$schema_version,
      run_id = created$run_id,
      params_hash = created$params_hash,
      authoritative = TRUE,
      train = created$train,
      final_interval_id = created$linked_event_interval_id,
      semantic_track = "event",
      label = "burst",
      start_isi = created$start_isi,
      end_isi = created$end_isi,
      n_isi = created$n_isi,
      start_time_sec = created$start_time_sec,
      end_time_sec = created$end_time_sec,
      final_source = "phase2b_review_confirmation",
      source_preview_interval_id = "",
      source_review_interval_id = created$source_review_interval_id,
      source_transition_id = created$source_transition_id,
      stringsAsFactors = FALSE
    )
  }
  out <- dplyr::bind_rows(
    stpd_multitrack_review_empty_final_intervals(), automatic_rows, created_rows
  )
  if (anyDuplicated(out$final_interval_id)) {
    stpd_multitrack_review_abort(
      "duplicate_final_interval_id", "Final multi-track interval IDs must be unique."
    )
  }
  for (train in unique(out$train)) {
    for (track in unique(out$semantic_track[out$train == train])) {
      part <- out[out$train == train & out$semantic_track == track, , drop = FALSE]
      if (nrow(part) <= 1L) next
      part <- part[order(part$start_isi, part$end_isi, method = "radix"), , drop = FALSE]
      if (any(part$start_isi[-1L] <= part$end_isi[-nrow(part)])) {
        stpd_multitrack_review_abort(
          "final_within_track_overlap",
          paste0("Final intervals overlap within train '", train, "' and track '", track, "'.")
        )
      }
    }
  }
  out <- out[order(
    out$train, match(out$semantic_track, c("event", "state", "gap", "review")),
    out$start_isi, out$end_isi, out$final_interval_id,
    method = "radix", na.last = TRUE
  ), , drop = FALSE]
  rownames(out) <- NULL
  out
}

stpd_multitrack_review_link_rows <- function(preview, manual) {
  if (nrow(manual) == 0L) return(stpd_multitrack_review_empty_links())
  rows <- lapply(seq_len(nrow(manual)), function(i) {
    row <- manual[i, , drop = FALSE]
    data.frame(
      schema_version = stpd_multitrack_review_schema_version(),
      run_id = row$run_id,
      params_hash = row$params_hash,
      authoritative = TRUE,
      train = row$train,
      relationship_id = stpd_multitrack_review_relationship_id(
        row$run_id, row$train, row$source_review_interval_id,
        row$linked_event_interval_id
      ),
      relationship_type = "review_confirmation_to_event",
      source_review_interval_id = row$source_review_interval_id,
      source_transition_id = row$source_transition_id,
      target_final_interval_id = row$linked_event_interval_id,
      action_effect = row$action_effect,
      non_destructive = TRUE,
      stringsAsFactors = FALSE
    )
  })
  dplyr::bind_rows(stpd_multitrack_review_empty_links(), rows)
}

stpd_multitrack_review_per_isi_rows <- function(ds, preview, final_intervals) {
  rows <- list()
  selected <- unlist(strsplit(stpd_multitrack_review_chr(preview$metadata$selected_trains), ";", fixed = TRUE))
  selected <- sort(unique(selected[nzchar(selected)]), method = "radix")
  for (train in selected) {
    dat <- ds$trains[[train]]
    if (!is.data.frame(dat)) {
      stpd_multitrack_review_abort(
        "final_per_isi_train_missing", paste0("Train '", train, "' is missing."
        )
      )
    }
    n <- nrow(dat)
    table <- data.frame(
      schema_version = rep(stpd_multitrack_review_schema_version(), n),
      run_id = rep(stpd_multitrack_review_chr(preview$metadata$run_id), n),
      params_hash = rep(stpd_multitrack_review_chr(preview$metadata$params_hash), n),
      authoritative = rep(TRUE, n),
      train = rep(train, n),
      isi_index = seq_len(n),
      timestamp_sec = suppressWarnings(as.numeric(dat$timestamp_sec)),
      ISI_sec = suppressWarnings(as.numeric(dat$ISI_sec)),
      pattern_final_event = rep("", n),
      pattern_final_event_interval_id = rep("", n),
      pattern_final_state = rep("", n),
      pattern_final_state_interval_id = rep("", n),
      pattern_final_gap = rep("", n),
      pattern_final_gap_interval_id = rep("", n),
      pattern_final_review = rep("", n),
      pattern_final_review_interval_id = rep("", n),
      stringsAsFactors = FALSE
    )
    part <- final_intervals[final_intervals$train == train, , drop = FALSE]
    for (i in seq_len(nrow(part))) {
      start <- max(1L, as.integer(part$start_isi[i]))
      end <- min(n, as.integer(part$end_isi[i]))
      if (!is.finite(start) || !is.finite(end) || start > end) {
        stpd_multitrack_review_abort(
          "final_interval_geometry_invalid", "A final interval has invalid per-ISI geometry."
        )
      }
      track <- as.character(part$semantic_track[i])
      label_col <- paste0("pattern_final_", track)
      id_col <- paste0(label_col, "_interval_id")
      if (!(label_col %in% names(table)) || any(nzchar(table[[id_col]][start:end]))) {
        stpd_multitrack_review_abort(
          "final_per_isi_track_conflict", "Final per-ISI projection is not unique within a track."
        )
      }
      table[[label_col]][start:end] <- as.character(part$label[i])
      table[[id_col]][start:end] <- as.character(part$final_interval_id[i])
    }
    if (n > 0L) {
      projection_cols <- grep("^pattern_final_", names(table), value = TRUE)
      table[1L, projection_cols] <- ""
    }
    rows[[train]] <- table
  }
  dplyr::bind_rows(c(
    list(stpd_multitrack_review_empty_per_isi()), unname(rows)
  ))
}

stpd_multitrack_review_manifest_filenames <- function() {
  c(
    metadata = "Multitrack_review_metadata.csv",
    transition_history = "Multitrack_review_transition_history.csv",
    current_decisions = "Multitrack_review_current_decisions.csv",
    manual_intervals = "Multitrack_review_manual_intervals.csv",
    final_intervals = "Multitrack_review_final_intervals.csv",
    final_relationships = "Multitrack_review_final_relationships.csv",
    final_per_isi = "Multitrack_review_final_per_isi.csv",
    invariants = "Multitrack_review_invariants.csv"
  )
}

stpd_multitrack_review_manifest <- function(
    tables, run_id, params_hash, authoritative = TRUE) {
  files <- stpd_multitrack_review_manifest_filenames()
  data.frame(
    schema_version = rep(stpd_multitrack_review_schema_version(), length(files)),
    run_id = rep(run_id, length(files)),
    params_hash = rep(params_hash, length(files)),
    authoritative = rep(isTRUE(authoritative), length(files)),
    table_name = names(files),
    file_name = unname(files),
    row_count = vapply(names(files), function(name) as.integer(nrow(tables[[name]])), integer(1)),
    column_count = vapply(names(files), function(name) as.integer(ncol(tables[[name]])), integer(1)),
    column_types = vapply(names(files), function(name) stpd_multitrack_review_column_types(tables[[name]]), character(1)),
    table_sha256 = vapply(names(files), function(name) stpd_multitrack_review_table_hash(tables[[name]]), character(1)),
    stringsAsFactors = FALSE
  )
}

stpd_multitrack_review_build_product <- function(
    ds, preview, history, enforce_current_manual_veto = TRUE) {
  history <- stpd_multitrack_review_validate_history(history)
  stpd_multitrack_review_validate_history_against_preview(ds, preview, history)
  preview_metadata <- preview$metadata
  current_parent_history <- history[
    history$parent_preview_schema_version ==
      stpd_multitrack_review_chr(preview_metadata$schema_version) &
      history$parent_preview_policy_hash ==
        stpd_multitrack_review_chr(preview_metadata$policy_hash) &
      history$parent_preview_run_id ==
        stpd_multitrack_review_chr(preview_metadata$run_id) &
      history$parent_preview_params_hash ==
        stpd_multitrack_review_chr(preview_metadata$params_hash) &
      history$parent_preview_manifest_sha256 ==
        stpd_multitrack_review_preview_manifest_hash(preview),
    , drop = FALSE
  ]
  terminal_parent_matches <- nrow(history) > 0L &&
    identical(
      history$parent_preview_schema_version[nrow(history)],
      stpd_multitrack_review_chr(preview_metadata$schema_version)
    ) &&
    identical(
      history$parent_preview_policy_hash[nrow(history)],
      stpd_multitrack_review_chr(preview_metadata$policy_hash)
    ) &&
    identical(
      history$parent_preview_run_id[nrow(history)],
      stpd_multitrack_review_chr(preview_metadata$run_id)
    ) &&
    identical(
      history$parent_preview_params_hash[nrow(history)],
      stpd_multitrack_review_chr(preview_metadata$params_hash)
    ) &&
    identical(
      history$parent_preview_manifest_sha256[nrow(history)],
      stpd_multitrack_review_preview_manifest_hash(preview)
    )
  if (nrow(current_parent_history) == 0L || !terminal_parent_matches) {
    stpd_multitrack_review_abort(
      "active_review_parent_history_missing",
      "An authoritative Phase 2B product requires its terminal transition to be bound to the exact current parent Preview."
    )
  }
  current <- stpd_multitrack_review_current_from_history(history, preview = preview)
  intervals <- preview$intervals
  confirmed <- current[current$current_status == "confirmed", , drop = FALSE]
  if (nrow(confirmed) > 0L) {
    for (i in seq_len(nrow(confirmed))) {
      hit <- which(
        intervals$train == confirmed$train[i] &
          intervals$interval_id == confirmed$source_review_interval_id[i]
      )
      if (length(hit) != 1L ||
          !identical(
            confirmed$source_row_sha256[i],
            stpd_multitrack_review_source_row_hash(intervals[hit, , drop = FALSE])
          )) {
        stpd_multitrack_review_abort(
          "confirmed_source_provenance_stale",
          "A confirmed Review source no longer matches its immutable parent Preview row."
        )
      }
      source <- intervals[hit, , drop = FALSE]
      if (isTRUE(enforce_current_manual_veto) && stpd_multitrack_review_manual_veto(
        ds$trains[[confirmed$train[i]]],
        as.integer(source$start_isi[1]), as.integer(source$end_isi[1])
      )) {
        stpd_multitrack_review_abort(
          "confirmed_source_now_vetoed",
          "A new not_burst veto makes the Phase 2B final product stale; explicitly revoke or resolve it."
        )
      }
    }
  }

  manual <- stpd_multitrack_review_manual_rows(ds, preview, history, current)
  final_intervals <- stpd_multitrack_review_final_interval_rows(preview, current, manual)
  links <- stpd_multitrack_review_link_rows(preview, manual)
  if (nrow(links) > 0L) {
    for (i in seq_len(nrow(links))) {
      source <- preview$intervals[
        preview$intervals$train == links$train[i] &
          preview$intervals$interval_id == links$source_review_interval_id[i],
        , drop = FALSE
      ]
      target <- final_intervals[
        final_intervals$train == links$train[i] &
          final_intervals$final_interval_id == links$target_final_interval_id[i],
        , drop = FALSE
      ]
      link_ok <- nrow(source) == 1L && nrow(target) == 1L &&
        target$semantic_track[1] == "event" && target$label[1] == "burst" &&
        source$start_isi[1] == target$start_isi[1] &&
        source$end_isi[1] == target$end_isi[1] &&
        isTRUE(links$non_destructive[i])
      if (!link_ok) {
        stpd_multitrack_review_abort(
          "final_relationship_foreign_key_invalid",
          "A Review confirmation link does not resolve to an exact-span final Event/Burst."
        )
      }
    }
  }
  per_isi <- stpd_multitrack_review_per_isi_rows(ds, preview, final_intervals)
  run_id <- stpd_multitrack_review_chr(preview$metadata$run_id)
  params_hash <- stpd_multitrack_review_chr(preview$metadata$params_hash)
  invariants <- data.frame(
    schema_version = rep(stpd_multitrack_review_schema_version(), 5L),
    run_id = rep(run_id, 5L),
    params_hash = rep(params_hash, 5L),
    check_name = c(
      "source_review_rows_immutable",
      "within_track_final_non_overlap",
      "exact_span_event_deduplicated",
      "automatic_preview_unchanged",
      "legacy_projection_unchanged"
    ),
    status = rep("pass", 5L),
    message = c(
      "Every applied decision retains the SHA-256 of its immutable Phase 2A Review row.",
      "Final intervals do not overlap within train and semantic track.",
      "An exact same-span automatic Burst is linked and not duplicated.",
      "Phase 2B materialization does not mutate the automatic Preview.",
      "Phase 2B materialization does not mutate legacy train labels or events."
    ),
    stringsAsFactors = FALSE
  )
  history_hash <- stpd_multitrack_review_table_hash(history)
  parent_manifest_hash <- stpd_multitrack_review_preview_manifest_hash(preview)
  product_payload <- list(
    transition_history = history,
    current_decisions = current,
    manual_intervals = manual,
    final_intervals = final_intervals,
    final_relationships = links,
    final_per_isi = per_isi,
    invariants = invariants
  )
  product_hash <- stpd_multitrack_review_sha256(product_payload)
  metadata <- data.frame(
    schema_version = stpd_multitrack_review_schema_version(),
    conflict_policy_version = stpd_multitrack_review_conflict_policy_version(),
    lifecycle_status = "active",
    authoritative = TRUE,
    run_id = run_id,
    params_hash = params_hash,
    parent_preview_schema_version = stpd_multitrack_review_chr(preview$metadata$schema_version),
    parent_preview_policy_hash = stpd_multitrack_review_chr(preview$metadata$policy_hash),
    parent_preview_manifest_sha256 = parent_manifest_hash,
    transition_history_sha256 = history_hash,
    product_sha256 = product_hash,
    transition_n = as.integer(nrow(history)),
    confirmed_n = as.integer(sum(current$current_status == "confirmed")),
    manual_interval_n = as.integer(nrow(manual)),
    final_interval_n = as.integer(nrow(final_intervals)),
    final_relationship_n = as.integer(nrow(links)),
    final_per_isi_n = as.integer(nrow(per_isi)),
    legacy_projection_changed = FALSE,
    automatic_preview_changed = FALSE,
    stale_reason = "",
    stringsAsFactors = FALSE
  )
  tables <- c(list(metadata = metadata), product_payload)
  manifest <- stpd_multitrack_review_manifest(
    tables, run_id, params_hash, authoritative = TRUE
  )
  structure(
    c(tables, list(table_manifest = manifest)),
    class = c("stpd_multitrack_review_product", "list")
  )
}

stpd_multitrack_review_new_operation_id <- local({
  last_stamp <- ""
  occurrence <- 0L
  function(at = Sys.time()) {
    stamp <- gsub("[^0-9]", "", format(at, "%Y%m%d%H%M%OS6", tz = "UTC"))
    if (identical(stamp, last_stamp)) occurrence <<- occurrence + 1L
    else {
      last_stamp <<- stamp
      occurrence <<- 1L
    }
    paste0("mtr_op_", stamp, "_p", Sys.getpid(), "_n", sprintf("%02d", occurrence))
  }
})

stpd_multitrack_review_existing_operation <- function(
    ds, operation_id, action, requests, expected_precondition_sha256,
    reviewer, reason) {
  history <- stpd_multitrack_review_history_raw(ds)
  existing <- history[history$operation_id == operation_id, , drop = FALSE]
  if (nrow(existing) == 0L) return(NULL)
  expected_requests <- requests[order(requests$train, requests$source_review_interval_id), , drop = FALSE]
  actual_requests <- existing[, c("train", "source_review_interval_id"), drop = FALSE]
  actual_requests <- actual_requests[order(actual_requests$train, actual_requests$source_review_interval_id), , drop = FALSE]
  rownames(expected_requests) <- NULL
  rownames(actual_requests) <- NULL
  same <- identical(expected_requests, actual_requests) &&
    all(existing$transition_action == action) &&
    all(existing$precondition_sha256 == expected_precondition_sha256) &&
    all(existing$reviewer == reviewer) && all(existing$reason == reason)
  if (!same) {
    stpd_multitrack_review_abort(
      "operation_id_payload_conflict",
      "The supplied operation_id already identifies a different immutable transition payload."
    )
  }
  existing
}

#' Apply an atomic Phase 2B Review transition
#'
#' @param ds Dataset containing a materialized public Preview.
#' @param requests Two-column train-qualified Review request table.
#' @param action Either `"confirm"` or `"revoke"`.
#' @param expected_precondition_sha256 Exact hash returned by preflight.
#' @param reviewer Non-empty reviewer identity.
#' @param reason Non-empty scientific/audit reason.
#' @param operation_id Optional idempotency key.
#' @return A list containing the updated `dataset`, appended `transitions`, the
#'   current `product`, and the verified `preflight`.
#' @export
stpd_multitrack_review_apply <- function(
    ds, requests, action = c("confirm", "revoke"),
    expected_precondition_sha256, reviewer, reason, operation_id = NULL) {
  action <- match.arg(action)
  requests <- stpd_multitrack_review_normalize_requests(requests)
  reviewer <- trimws(stpd_multitrack_review_chr(reviewer))
  reason <- trimws(stpd_multitrack_review_chr(reason))
  expected <- tolower(trimws(stpd_multitrack_review_chr(expected_precondition_sha256)))
  if (!grepl("^[0-9a-f]{64}$", expected)) {
    stpd_multitrack_review_abort(
      "precondition_hash_invalid", "A 64-character SHA-256 precondition hash is required."
    )
  }
  if (!nzchar(reviewer) || !nzchar(reason)) {
    stpd_multitrack_review_abort(
      "reviewer_or_reason_missing", "Both reviewer and reason are required for an auditable transition."
    )
  }
  operation_id <- trimws(stpd_multitrack_review_chr(operation_id))
  if (!nzchar(operation_id)) operation_id <- stpd_multitrack_review_new_operation_id()

  verified_preview <- stpd_multitrack_review_require_preview(ds)
  stpd_multitrack_review_validate_product(
    ds, preview = verified_preview,
    enforce_current_manual_veto = !identical(action, "revoke")
  )

  existing_operation <- stpd_multitrack_review_existing_operation(
    ds, operation_id, action, requests, expected, reviewer, reason
  )
  if (!is.null(existing_operation)) {
    preview_metadata <- verified_preview$metadata
    if (!all(existing_operation$parent_preview_schema_version ==
             stpd_multitrack_review_chr(preview_metadata$schema_version)) ||
        !all(existing_operation$parent_preview_policy_hash ==
             stpd_multitrack_review_chr(preview_metadata$policy_hash)) ||
        !all(existing_operation$parent_preview_run_id ==
             stpd_multitrack_review_chr(preview_metadata$run_id)) ||
        !all(existing_operation$parent_preview_params_hash ==
             stpd_multitrack_review_chr(preview_metadata$params_hash)) ||
        !all(existing_operation$parent_preview_manifest_sha256 ==
             stpd_multitrack_review_preview_manifest_hash(verified_preview))) {
      stpd_multitrack_review_abort(
        "operation_id_parent_conflict",
        "The operation_id belongs to a different automatic Preview run."
      )
    }
    replay_ds <- ds
    if (stpd_multitrack_auto_result_key() %in% names(replay_ds$results)) {
      if (exists("stpd_event_regime_strip", mode = "function")) {
        replay_ds <- stpd_event_regime_strip(replay_ds)
      }
      replay_ds <- stpd_multitrack_gate_b_attach(replay_ds)
      if (exists("stpd_event_regime_attach", mode = "function")) {
        replay_ds <- stpd_event_regime_attach(replay_ds)
      }
    }
    return(list(
      dataset = replay_ds,
      transitions = existing_operation,
      product = stpd_multitrack_review_product_raw(replay_ds),
      preflight = NULL,
      idempotent_replay = TRUE
    ))
  }

  preflight <- stpd_multitrack_review_preflight(ds, requests, action = action)
  if (!identical(preflight$precondition_sha256, expected)) {
    stpd_multitrack_review_abort(
      "stale_precondition",
      "The Phase 2B precondition changed; rerun preflight before applying the atomic transition."
    )
  }
  if (!isTRUE(preflight$eligible)) {
    failure <- preflight$decisions[!preflight$decisions$eligible, , drop = FALSE]
    code <- stpd_multitrack_review_chr(failure$failure_code, "preflight_ineligible")
    message <- paste(unique(failure$failure_message), collapse = "; ")
    stpd_multitrack_review_abort(code, message)
  }

  preview <- verified_preview
  history <- stpd_multitrack_review_validate_history(
    stpd_multitrack_review_history_raw(ds)
  )
  previous_id <- if (nrow(history) == 0L) "" else history$transition_id[nrow(history)]
  previous_hash <- if (nrow(history) == 0L) "" else history$transition_sha256[nrow(history)]
  at <- Sys.time()
  time_chr <- format(at, "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC")
  appended <- vector("list", nrow(preflight$decisions))
  for (i in seq_len(nrow(preflight$decisions))) {
    decision <- preflight$decisions[i, , drop = FALSE]
    sequence <- nrow(history) + i
    transition_id <- paste0(
      "mtr_transition_",
      substr(stpd_multitrack_review_sha256(
        paste(operation_id, decision$train, decision$source_review_interval_id,
              action, sep = "|"), serialize = FALSE
      ), 1L, 24L)
    )
    row <- data.frame(
      schema_version = stpd_multitrack_review_schema_version(),
      transition_sequence = as.integer(sequence),
      transition_id = transition_id,
      operation_id = operation_id,
      transition_action = action,
      transition_status = "applied",
      train = decision$train,
      source_review_interval_id = decision$source_review_interval_id,
      source_candidate_id = decision$source_candidate_id,
      source_candidate_key = decision$source_candidate_key,
      source_candidate_layer = decision$source_candidate_layer,
      source_candidate_source = decision$source_candidate_source,
      source_start_isi = as.integer(decision$start_isi),
      source_end_isi = as.integer(decision$end_isi),
      source_row_sha256 = decision$source_row_sha256,
      review_target_track = "event",
      review_target_label = "burst",
      proposed_effect = decision$proposed_effect,
      target_event_interval_id = decision$target_event_interval_id,
      parent_preview_schema_version = stpd_multitrack_review_chr(preview$metadata$schema_version),
      parent_preview_policy_hash = stpd_multitrack_review_chr(preview$metadata$policy_hash),
      parent_preview_run_id = stpd_multitrack_review_chr(preview$metadata$run_id),
      parent_preview_params_hash = stpd_multitrack_review_chr(preview$metadata$params_hash),
      parent_preview_manifest_sha256 = stpd_multitrack_review_preview_manifest_hash(preview),
      precondition_sha256 = expected,
      previous_transition_id = previous_id,
      previous_transition_sha256 = previous_hash,
      conflict_policy_version = stpd_multitrack_review_conflict_policy_version(),
      reviewer = reviewer,
      reason = reason,
      server_time_utc = time_chr,
      transition_sha256 = "",
      stringsAsFactors = FALSE
    )
    row$transition_sha256 <- stpd_multitrack_review_transition_hash(row)
    appended[[i]] <- row
    previous_id <- row$transition_id
    previous_hash <- row$transition_sha256
  }
  appended <- dplyr::bind_rows(stpd_multitrack_review_empty_history(), appended)
  new_history <- dplyr::bind_rows(history, appended)
  new_history <- stpd_multitrack_review_validate_history(new_history)

  # Build everything in a copy before changing the caller-visible dataset. Any
  # failure leaves the original object and its history untouched.
  out <- ds
  product <- stpd_multitrack_review_build_product(out, preview, new_history)
  if (is.null(out$results) || !is.list(out$results)) out$results <- list()
  out$results[[stpd_multitrack_review_result_key()]] <- product
  if (stpd_multitrack_auto_result_key() %in% names(out$results)) {
    if (exists("stpd_event_regime_strip", mode = "function")) {
      out <- stpd_event_regime_strip(out)
    }
    out <- stpd_multitrack_gate_b_attach(out)
    if (exists("stpd_event_regime_attach", mode = "function")) {
      out <- stpd_event_regime_attach(out)
    }
  }
  list(
    dataset = out,
    transitions = appended,
    product = product,
    preflight = preflight,
    idempotent_replay = FALSE
  )
}

#' Confirm one or more Review candidates as Event/Burst decisions
#' @export
stpd_multitrack_review_confirm <- function(
    ds, requests, expected_precondition_sha256, reviewer, reason,
    operation_id = NULL) {
  stpd_multitrack_review_apply(
    ds, requests, action = "confirm",
    expected_precondition_sha256 = expected_precondition_sha256,
    reviewer = reviewer, reason = reason, operation_id = operation_id
  )
}

#' Revoke one or more Phase 2B Review confirmations
#' @export
stpd_multitrack_review_revoke <- function(
    ds, requests, expected_precondition_sha256, reviewer, reason,
    operation_id = NULL) {
  stpd_multitrack_review_apply(
    ds, requests, action = "revoke",
    expected_precondition_sha256 = expected_precondition_sha256,
    reviewer = reviewer, reason = reason, operation_id = operation_id
  )
}

stpd_multitrack_review_stale_product <- function(product, reason) {
  history <- product$transition_history %||% stpd_multitrack_review_empty_history()
  old_metadata <- product$metadata %||% stpd_multitrack_review_empty_metadata()
  run_id <- if (nrow(old_metadata) > 0L) stpd_multitrack_review_chr(old_metadata$run_id) else ""
  params_hash <- if (nrow(old_metadata) > 0L) stpd_multitrack_review_chr(old_metadata$params_hash) else ""
  metadata <- data.frame(
    schema_version = stpd_multitrack_review_schema_version(),
    conflict_policy_version = stpd_multitrack_review_conflict_policy_version(),
    lifecycle_status = "stale_after_detector_rerun",
    authoritative = FALSE,
    run_id = run_id,
    params_hash = params_hash,
    parent_preview_schema_version = if (nrow(old_metadata) > 0L) stpd_multitrack_review_chr(old_metadata$parent_preview_schema_version) else "",
    parent_preview_policy_hash = if (nrow(old_metadata) > 0L) stpd_multitrack_review_chr(old_metadata$parent_preview_policy_hash) else "",
    parent_preview_manifest_sha256 = if (nrow(old_metadata) > 0L) stpd_multitrack_review_chr(old_metadata$parent_preview_manifest_sha256) else "",
    transition_history_sha256 = stpd_multitrack_review_table_hash(history),
    product_sha256 = "",
    transition_n = as.integer(nrow(history)),
    confirmed_n = 0L,
    manual_interval_n = 0L,
    final_interval_n = 0L,
    final_relationship_n = 0L,
    final_per_isi_n = 0L,
    legacy_projection_changed = FALSE,
    automatic_preview_changed = FALSE,
    stale_reason = stpd_multitrack_review_chr(reason, "detector_rerun_requires_reconfirmation"),
    stringsAsFactors = FALSE
  )
  tables <- list(
    metadata = metadata,
    transition_history = history,
    current_decisions = stpd_multitrack_review_empty_current(),
    manual_intervals = stpd_multitrack_review_empty_manual_intervals(),
    final_intervals = stpd_multitrack_review_empty_final_intervals(),
    final_relationships = stpd_multitrack_review_empty_links(),
    final_per_isi = stpd_multitrack_review_empty_per_isi(),
    invariants = stpd_multitrack_review_empty_invariants()
  )
  tables$metadata$product_sha256 <- stpd_multitrack_review_sha256(tables[-1L])
  tables$table_manifest <- stpd_multitrack_review_manifest(
    tables, run_id, params_hash, authoritative = FALSE
  )
  structure(tables, class = c("stpd_multitrack_review_product", "list"))
}

stpd_multitrack_review_validate_product <- function(
    ds, preview = NULL, enforce_current_manual_veto = TRUE) {
  if (!stpd_phase2b_has_state(ds)) return(invisible(TRUE))
  product <- stpd_multitrack_review_product_raw(ds)
  prototypes <- stpd_multitrack_review_table_prototypes()
  required <- c(names(prototypes), "table_manifest")
  if (!is.list(product) ||
      !identical(class(product), c("stpd_multitrack_review_product", "list")) ||
      length(setdiff(names(attributes(product)), c("names", "class"))) > 0L ||
      !identical(names(product), required)) {
    stpd_multitrack_review_abort(
      "review_product_component_schema_invalid",
      "The Phase 2B product component set/order is not fixed."
    )
  }
  for (name in names(prototypes)) {
    stpd_multitrack_review_validate_table_schema(
      product[[name]], prototypes[[name]], name
    )
  }
  stpd_multitrack_review_validate_table_schema(
    product$table_manifest, stpd_multitrack_review_empty_manifest(),
    "table_manifest"
  )
  history <- stpd_multitrack_review_validate_history(product$transition_history)
  if (!identical(history, product$transition_history)) {
    stpd_multitrack_review_abort(
      "review_product_history_order_invalid",
      "Persisted transition history must remain in canonical append order."
    )
  }
  metadata <- product$metadata
  lifecycle <- if (nrow(metadata) == 1L) {
    as.character(metadata$lifecycle_status[1])
  } else {
    ""
  }
  expected_authoritative <- identical(lifecycle, "active")
  metadata_hash_fields <- if (nrow(metadata) == 1L) {
    as.character(unlist(metadata[c(
      "params_hash", "parent_preview_policy_hash",
      "parent_preview_manifest_sha256", "transition_history_sha256",
      "product_sha256"
    )], use.names = FALSE))
  } else {
    character()
  }
  if (nrow(metadata) != 1L ||
      !identical(metadata$schema_version[1], stpd_multitrack_review_schema_version()) ||
      !identical(metadata$conflict_policy_version[1],
                 stpd_multitrack_review_conflict_policy_version()) ||
      !identical(metadata$authoritative[1], expected_authoritative) ||
      !identical(metadata$legacy_projection_changed[1], FALSE) ||
      !identical(metadata$automatic_preview_changed[1], FALSE) ||
      !nzchar(metadata$run_id[1]) ||
      !nzchar(metadata$parent_preview_schema_version[1]) ||
      length(metadata_hash_fields) != 5L || anyNA(metadata_hash_fields) ||
      !all(grepl("^[0-9a-f]{64}$", metadata_hash_fields)) ||
      !(lifecycle %in% c("active", "stale_after_detector_rerun"))) {
    stpd_multitrack_review_abort(
      "review_product_metadata_invalid",
      "Phase 2B metadata violates its fixed identity or lifecycle contract."
    )
  }
  if (!identical(
    metadata$transition_history_sha256[1],
    stpd_multitrack_review_table_hash(history)
  )) {
    stpd_multitrack_review_abort(
      "review_product_history_hash_mismatch",
      "Phase 2B metadata does not identify the persisted transition history."
    )
  }
  tables <- product[names(prototypes)]
  files <- stpd_multitrack_review_manifest_filenames()
  manifest <- product$table_manifest
  if (nrow(manifest) != length(files) ||
      !identical(as.character(manifest$table_name), names(files)) ||
      !identical(as.character(manifest$file_name), unname(files)) ||
      anyDuplicated(manifest$table_name) || anyDuplicated(manifest$file_name)) {
    stpd_multitrack_review_abort(
      "review_product_manifest_identity_invalid",
      "The Phase 2B manifest does not match the code-owned table/file mapping."
    )
  }
  expected_rows <- vapply(tables, nrow, integer(1))
  expected_cols <- vapply(tables, ncol, integer(1))
  expected_types <- vapply(tables, stpd_multitrack_review_column_types, character(1))
  expected_hashes <- vapply(tables, stpd_multitrack_review_table_hash, character(1))
  if (!identical(as.integer(manifest$row_count), unname(expected_rows)) ||
      !identical(as.integer(manifest$column_count), unname(expected_cols)) ||
      !identical(as.character(manifest$column_types), unname(expected_types)) ||
      !identical(as.character(manifest$table_sha256), unname(expected_hashes)) ||
      !all(manifest$schema_version == metadata$schema_version[1]) ||
      !all(manifest$run_id == metadata$run_id[1]) ||
      !all(manifest$params_hash == metadata$params_hash[1]) ||
      !identical(
        as.logical(manifest$authoritative),
        rep(expected_authoritative, nrow(manifest))
      )) {
    stpd_multitrack_review_abort(
      "review_product_manifest_payload_mismatch",
      "The Phase 2B manifest is stale or inconsistent with its persisted tables."
    )
  }
  payload_hash <- stpd_multitrack_review_sha256(tables[-1L])
  if (!identical(metadata$product_sha256[1], payload_hash)) {
    stpd_multitrack_review_abort(
      "review_product_hash_mismatch",
      "The Phase 2B product hash does not match its scientific/audit tables."
    )
  }

  if (identical(lifecycle, "active")) {
    if (is.null(preview)) preview <- stpd_multitrack_review_require_preview(ds)
    if (!identical(metadata$run_id[1], stpd_multitrack_review_chr(preview$metadata$run_id)) ||
        !identical(metadata$params_hash[1], stpd_multitrack_review_chr(preview$metadata$params_hash)) ||
        !identical(metadata$parent_preview_manifest_sha256[1],
                   stpd_multitrack_review_preview_manifest_hash(preview))) {
      stpd_multitrack_review_abort(
        "review_product_parent_identity_mismatch",
        "The active Phase 2B product is not bound to the current automatic Preview."
      )
    }
    rebuilt <- stpd_multitrack_review_build_product(
      ds, preview, history,
      enforce_current_manual_veto = enforce_current_manual_veto
    )
  } else {
    if (nrow(history) > 0L) {
      terminal <- history[nrow(history), , drop = FALSE]
      terminal_parent_matches <-
        identical(
          terminal$parent_preview_schema_version,
          metadata$parent_preview_schema_version[1]
        ) &&
        identical(
          terminal$parent_preview_policy_hash,
          metadata$parent_preview_policy_hash[1]
        ) &&
        identical(terminal$parent_preview_run_id, metadata$run_id[1]) &&
        identical(
          terminal$parent_preview_params_hash, metadata$params_hash[1]
        ) &&
        identical(
          terminal$parent_preview_manifest_sha256,
          metadata$parent_preview_manifest_sha256[1]
        )
      if (!terminal_parent_matches) {
        stpd_multitrack_review_abort(
          "stale_review_parent_identity_mismatch",
          "Stale metadata must identify the terminal immutable parent Preview that was invalidated by the rerun."
        )
      }
    }
    if (nrow(product$current_decisions) != 0L ||
        nrow(product$manual_intervals) != 0L ||
        nrow(product$final_intervals) != 0L ||
        nrow(product$final_relationships) != 0L ||
        nrow(product$final_per_isi) != 0L ||
        nrow(product$invariants) != 0L) {
      stpd_multitrack_review_abort(
        "stale_review_product_contains_active_tables",
        "A stale-after-rerun Phase 2B archive must not contain active/final tables."
      )
    }
    rebuilt <- stpd_multitrack_review_stale_product(
      product, metadata$stale_reason[1]
    )
  }
  if (!all(vapply(required, function(name) {
    identical(product[[name]], rebuilt[[name]])
  }, logical(1)))) {
    stpd_multitrack_review_abort(
      "review_product_deterministic_rebuild_mismatch",
      "The persisted Phase 2B product does not match deterministic history-based reconstruction."
    )
  }
  invisible(TRUE)
}

stpd_multitrack_review_strip_for_rerun <- function(ds) {
  # Strict no-op when Phase 2B has never been used. This preserves every legacy
  # result shape and checksum in automatic-only workflows.
  if (!stpd_phase2b_has_state(ds)) return(ds)
  detached <- stpd_multitrack_review_detach_for_rerun(ds)
  stpd_multitrack_review_restore_archive(detached$dataset, detached$archive)
}

stpd_multitrack_review_detach_for_rerun <- function(
    ds, reason = "detector_rerun_requires_review_source_revalidation") {
  # The detector must not observe active decisions or stale transition history.
  # Validate first, retain only an explicitly non-authoritative archive, and
  # remove the complete product from the working dataset until detection ends.
  if (!stpd_phase2b_has_state(ds)) {
    return(list(dataset = ds, archive = NULL))
  }
  stpd_multitrack_review_validate_product(
    ds, enforce_current_manual_veto = FALSE
  )
  key <- stpd_multitrack_review_result_key()
  product <- ds$results[[key]]
  lifecycle <- stpd_multitrack_review_chr(
    (product$metadata %||% data.frame())$lifecycle_status
  )
  archive <- if (identical(lifecycle, "stale_after_detector_rerun")) {
    product
  } else {
    stpd_multitrack_review_stale_product(product, reason)
  }
  out <- ds
  out$results[[key]] <- NULL
  list(dataset = out, archive = archive)
}

stpd_multitrack_review_restore_archive <- function(ds, archive) {
  # A NULL archive is an exact no-op, preserving the automatic-only result
  # shape and the frozen scientific hashes.
  if (is.null(archive)) return(ds)
  if (stpd_phase2b_has_state(ds)) {
    stpd_multitrack_review_abort(
      "review_archive_restore_collision",
      "A detector result already contains a Phase 2B product; refusing to overwrite it with archived history."
    )
  }
  out <- ds
  if (is.null(out$results) || !is.list(out$results)) out$results <- list()
  out$results[[stpd_multitrack_review_result_key()]] <- archive
  stpd_multitrack_review_validate_product(
    out, enforce_current_manual_veto = FALSE
  )
  out
}

stpd_multitrack_review_strip_label_blind <- function(ds) {
  if (!stpd_phase2b_has_state(ds)) return(ds)
  out <- ds
  out$results[[stpd_multitrack_review_result_key()]] <- NULL
  out
}

stpd_multitrack_review_state_payload <- function(ds) {
  product <- stpd_multitrack_review_product_raw(ds)
  if (is.null(product)) return(NULL)
  fields <- c(
    "metadata", "transition_history", "current_decisions",
    "manual_intervals", "final_intervals", "final_relationships",
    "final_per_isi", "invariants", "table_manifest"
  )
  payload <- lapply(fields, function(name) {
    stpd_multitrack_review_normalize_table(product[[name]] %||% data.frame())
  })
  names(payload) <- fields
  payload
}
