# Gate-B reviewed multi-track product.
#
# This module is deliberately separate from the Phase 2B v1 product.  The v1
# transition chain remains immutable.  Exact, current-parent transitions are
# imported with their legacy SHA-256 and replayed over the canonical v2 AUTO
# product.  A non-exact migration never modifies scientific tables and is
# represented as a typed requires-readjudication product.

stpd_multitrack_final_schema_version <- function() "stpd_multitrack_final_v2"
stpd_multitrack_final_result_key <- function() "multitrack_final"

stpd_multitrack_final_abort <- function(code, message) {
  stop(structure(
    list(message = as.character(message)[1], call = NULL,
         code = as.character(code)[1]),
    class = c("stpd_multitrack_final_error", "error", "condition")
  ))
}

stpd_multitrack_final_empty_history <- function() {
  data.frame(
    schema_version = character(), migration_sequence = integer(),
    migration_id = character(), legacy_transition_id = character(),
    legacy_transition_sha256 = character(), transition_action = character(),
    train = character(), review_candidate_id = character(),
    source_interval_id = character(), source_candidate_id = character(),
    source_candidate_key = character(), start_isi = integer(), end_isi = integer(),
    target_track = character(), target_label = character(),
    reviewer = character(), reason = character(), server_time_utc = character(),
    stringsAsFactors = FALSE
  )
}

stpd_multitrack_final_empty_decisions <- function() {
  data.frame(
    schema_version = character(), train = character(),
    review_candidate_id = character(), current_status = character(),
    current_migration_id = character(), legacy_transition_id = character(),
    legacy_transition_sha256 = character(), reviewer = character(),
    reason = character(), server_time_utc = character(),
    stringsAsFactors = FALSE
  )
}

stpd_multitrack_final_empty_links <- function() {
  data.frame(
    schema_version = character(), run_id = character(), params_hash = character(),
    authoritative = logical(), train = character(), link_id = character(),
    review_candidate_id = character(), event_id = character(),
    migration_id = character(), legacy_transition_id = character(),
    relationship_type = character(), exact_span = logical(),
    creates_new_event = logical(), non_destructive = logical(),
    stringsAsFactors = FALSE
  )
}

stpd_multitrack_final_empty_invariants <- function() {
  data.frame(
    schema_version = character(), run_id = character(), params_hash = character(),
    check_name = character(), status = character(), message = character(),
    stringsAsFactors = FALSE
  )
}

stpd_multitrack_final_empty_metadata <- function() {
  data.frame(
    schema_version = character(), run_id = character(), params_hash = character(),
    authoritative = logical(), authority_scope = character(),
    intended_target_scope = character(), biological_ground_truth = logical(),
    promotion_gate = character(), promotion_status = character(),
    materialization_status = character(), migration_status = character(),
    failure_code = character(), failure_message = character(),
    parent_auto_schema_version = character(), parent_auto_policy_hash = character(),
    parent_auto_product_sha256 = character(), parent_auto_manifest_sha256 = character(),
    legacy_history_sha256 = character(), migration_history_sha256 = character(),
    events_n = integer(), states_n = integer(), gaps_n = integer(),
    review_candidates_n = integer(), state_event_relationships_n = integer(),
    review_event_links_n = integer(), per_isi_n = integer(),
    product_sha256 = character(), stringsAsFactors = FALSE
  )
}

stpd_multitrack_final_empty_manifest <- function() {
  data.frame(
    schema_version = character(), run_id = character(), params_hash = character(),
    table_name = character(), row_count = integer(), column_count = integer(),
    column_types = character(), table_sha256 = character(),
    stringsAsFactors = FALSE
  )
}

stpd_multitrack_final_bind <- function(prototype, rows) {
  if (length(rows) == 0L) return(prototype)
  dplyr::bind_rows(c(list(prototype), rows))
}

stpd_multitrack_final_auto_manifest_hash <- function(auto) {
  stpd_multitrack_auto_hash(auto$manifest %||% data.frame())
}

stpd_multitrack_final_auto_parent <- function(ds) {
  auto <- (ds$results %||% list())[[stpd_multitrack_auto_result_key()]] %||% NULL
  if (is.null(auto)) stpd_multitrack_final_abort(
    "parent_auto_missing", "A v2 FINAL product requires an attached AUTO parent."
  )
  # Validate the stored canonical artifact itself. Downstream Preview/Review
  # products are intentionally excluded from AUTO rematerialization inputs and
  # must not make an already-verified parent appear different.
  stpd_multitrack_auto_validate(auto, parent = NULL)
  auto
}

stpd_multitrack_final_history <- function(ds, auto) {
  product <- stpd_multitrack_review_product_raw(ds)
  if (is.null(product)) {
    return(list(status = "not_required", reason = "", legacy = data.frame(),
                migrated = stpd_multitrack_final_empty_history()))
  }
  metadata <- product$metadata %||% data.frame()
  lifecycle <- stpd_multitrack_review_chr(metadata$lifecycle_status)
  history <- product$transition_history %||% stpd_multitrack_review_empty_history()
  if (!identical(lifecycle, "active")) {
    return(list(
      status = "requires_readjudication",
      reason = "legacy_review_parent_is_not_current",
      legacy = history, migrated = stpd_multitrack_final_empty_history()
    ))
  }
  checked <- tryCatch({
    stpd_multitrack_review_validate_product(
      ds, enforce_current_manual_veto = TRUE
    )
    history
  }, error = function(e) e)
  if (inherits(checked, "condition")) {
    return(list(
      status = "requires_readjudication",
      reason = paste0("legacy_review_validation_failed:",
                      stpd_multitrack_review_chr(checked$code, "unknown")),
      legacy = history, migrated = stpd_multitrack_final_empty_history()
    ))
  }
  if (nrow(history) == 0L) {
    return(list(status = "not_required", reason = "", legacy = history,
                migrated = stpd_multitrack_final_empty_history()))
  }
  review <- auto$review_candidates
  rows <- vector("list", nrow(history))
  for (i in seq_len(nrow(history))) {
    old <- history[i, , drop = FALSE]
    hit <- which(
      review$train == old$train &
        review$source_interval_id == old$source_review_interval_id &
        review$source_candidate_id == old$source_candidate_id &
        review$source_candidate_key == old$source_candidate_key &
        review$start_isi == old$source_start_isi &
        review$end_isi == old$source_end_isi &
        review$target_track == old$review_target_track &
        review$target_label == old$review_target_label
    )
    if (length(hit) != 1L ||
        !identical(old$transition_status, "applied") ||
        !old$transition_action %in% c("confirm", "revoke")) {
      return(list(
        status = "requires_readjudication",
        reason = "legacy_transition_has_no_exact_auto_v2_source",
        legacy = history, migrated = stpd_multitrack_final_empty_history()
      ))
    }
    source <- review[hit, , drop = FALSE]
    rows[[i]] <- data.frame(
      schema_version = stpd_multitrack_final_schema_version(),
      migration_sequence = as.integer(i),
      migration_id = paste0("mtf_migration_", substr(
        stpd_multitrack_auto_hash(c(old$transition_sha256, source$review_candidate_id)),
        1L, 24L
      )),
      legacy_transition_id = old$transition_id,
      legacy_transition_sha256 = old$transition_sha256,
      transition_action = old$transition_action, train = old$train,
      review_candidate_id = source$review_candidate_id,
      source_interval_id = source$source_interval_id,
      source_candidate_id = source$source_candidate_id,
      source_candidate_key = source$source_candidate_key,
      start_isi = as.integer(source$start_isi), end_isi = as.integer(source$end_isi),
      target_track = source$target_track, target_label = source$target_label,
      reviewer = old$reviewer, reason = old$reason,
      server_time_utc = old$server_time_utc, stringsAsFactors = FALSE
    )
  }
  list(status = "migrated_exact", reason = "", legacy = history,
       migrated = stpd_multitrack_final_bind(
         stpd_multitrack_final_empty_history(), rows
       ))
}

stpd_multitrack_final_current <- function(history) {
  if (nrow(history) == 0L) return(stpd_multitrack_final_empty_decisions())
  keys <- paste(history$train, history$review_candidate_id, sep = "\u001f")
  terminal <- history[!duplicated(keys, fromLast = TRUE), , drop = FALSE]
  terminal <- terminal[order(terminal$train, terminal$review_candidate_id,
                             method = "radix"), , drop = FALSE]
  data.frame(
    schema_version = stpd_multitrack_final_schema_version(),
    train = terminal$train, review_candidate_id = terminal$review_candidate_id,
    current_status = ifelse(terminal$transition_action == "confirm",
                            "confirmed", "revoked"),
    current_migration_id = terminal$migration_id,
    legacy_transition_id = terminal$legacy_transition_id,
    legacy_transition_sha256 = terminal$legacy_transition_sha256,
    reviewer = terminal$reviewer, reason = terminal$reason,
    server_time_utc = terminal$server_time_utc, stringsAsFactors = FALSE
  )
}

stpd_multitrack_final_event_from_review <- function(auto, source, decision) {
  event_id <- stpd_multitrack_auto_id(
    "final_event", auto$metadata$run_id, source$train,
    source$review_candidate_id, decision$current_migration_id
  )
  data.frame(
    schema_version = stpd_multitrack_auto_schema_version(),
    policy_hash = auto$metadata$policy_hash, run_id = auto$metadata$run_id,
    params_hash = auto$metadata$params_hash, authoritative = FALSE,
    train = source$train, event_id = event_id,
    source_interval_id = source$source_interval_id,
    source_candidate_id = source$source_candidate_id,
    source_candidate_key = source$source_candidate_key,
    source_label = "possible_burst", event_family = "burst",
    extent_class = "unresolved", frequency_class = "unresolved",
    provenance_source_interval_ids = source$source_interval_id,
    provenance_source_candidate_ids = source$source_candidate_id,
    provenance_source_candidate_keys = source$source_candidate_key,
    provenance_source_labels = "possible_burst",
    provenance_candidate_layers = source$candidate_layer,
    provenance_candidate_sources = source$candidate_source,
    extent_evidence_labels = "", frequency_evidence_labels = "",
    extent_conflict = FALSE,
    modifier_diagnostic = "review_confirmed_without_subtype_inference",
    start_isi = as.integer(source$start_isi), end_isi = as.integer(source$end_isi),
    n_isi = as.integer(source$n_isi), n_spikes = as.integer(source$n_isi + 1L),
    start_time_sec = as.numeric(source$start_time_sec),
    end_time_sec = as.numeric(source$end_time_sec),
    candidate_layer = "review_adjudication",
    candidate_source = "legacy_phase2b_exact_migration",
    stringsAsFactors = FALSE
  )
}

stpd_multitrack_final_per_isi_source <- function(auto) {
  source <- auto$per_isi
  event_hit <- match(source$event_id, auto$events$event_id)
  state_hit <- match(source$state_id, auto$states$state_id)
  gap_hit <- match(source$gap_id, auto$gaps$gap_id)
  review_hit <- match(
    source$review_candidate_id, auto$review_candidates$review_candidate_id
  )
  data.frame(
    train = source$train, isi_index = source$isi_index,
    timestamp_sec = source$timestamp_sec, ISI_sec = source$ISI_sec,
    pattern_auto_event_preview_interval_id = ifelse(
      nzchar(source$event_id), auto$events$source_interval_id[event_hit], ""
    ),
    pattern_auto_state_preview_interval_id = ifelse(
      nzchar(source$state_id), auto$states$source_interval_id[state_hit], ""
    ),
    pattern_auto_gap_preview_interval_id = ifelse(
      nzchar(source$gap_id), auto$gaps$source_interval_id[gap_hit], ""
    ),
    pattern_auto_review_preview_interval_id = ifelse(
      nzchar(source$review_candidate_id),
      auto$review_candidates$source_interval_id[review_hit], ""
    ),
    review_candidate_count = source$review_candidate_count,
    stringsAsFactors = FALSE
  )
}

stpd_multitrack_final_replay <- function(auto, history) {
  current <- stpd_multitrack_final_current(history)
  events <- auto$events
  links <- list()
  confirmed <- current[current$current_status == "confirmed", , drop = FALSE]
  for (i in seq_len(nrow(confirmed))) {
    decision <- confirmed[i, , drop = FALSE]
    source <- auto$review_candidates[
      auto$review_candidates$train == decision$train &
        auto$review_candidates$review_candidate_id == decision$review_candidate_id,
      , drop = FALSE
    ]
    if (nrow(source) != 1L) {
      stpd_multitrack_final_abort(
        "confirmed_review_source_missing",
        "A confirmed v2 Review decision has no unique AUTO source."
      )
    }
    overlap <- events[
      events$train == source$train &
        pmax(events$start_isi, source$start_isi) <=
          pmin(events$end_isi, source$end_isi), , drop = FALSE
    ]
    exact <- overlap[
      overlap$start_isi == source$start_isi &
        overlap$end_isi == source$end_isi, , drop = FALSE
    ]
    if (nrow(overlap) > nrow(exact) || nrow(exact) > 1L) {
      stpd_multitrack_final_abort(
        "review_event_same_track_conflict",
        "A confirmed Review interval overlaps a non-identical canonical Event."
      )
    }
    creates <- nrow(exact) == 0L
    if (creates) {
      added <- stpd_multitrack_final_event_from_review(auto, source, decision)
      events <- dplyr::bind_rows(events, added)
      event_id <- added$event_id
    } else {
      event_id <- exact$event_id
    }
    links[[length(links) + 1L]] <- data.frame(
      schema_version = stpd_multitrack_final_schema_version(),
      run_id = auto$metadata$run_id, params_hash = auto$metadata$params_hash,
      authoritative = FALSE, train = source$train,
      link_id = paste0("mtf_link_", substr(stpd_multitrack_auto_hash(c(
        source$review_candidate_id, event_id, decision$current_migration_id
      )), 1L, 24L)),
      review_candidate_id = source$review_candidate_id, event_id = event_id,
      migration_id = decision$current_migration_id,
      legacy_transition_id = decision$legacy_transition_id,
      relationship_type = "review_confirmation_to_event",
      exact_span = TRUE, creates_new_event = creates,
      non_destructive = TRUE, stringsAsFactors = FALSE
    )
  }
  events <- events[order(events$train, events$start_isi, events$end_isi,
                         events$event_id, method = "radix"), , drop = FALSE]
  rownames(events) <- NULL
  stpd_multitrack_auto_assert_same_track_non_overlap(events, "event_id")
  relationships <- stpd_multitrack_auto_relationships(
    events, auto$states, auto$metadata$policy_hash,
    auto$metadata$run_id, auto$metadata$params_hash
  )
  per_isi_source <- stpd_multitrack_final_per_isi_source(auto)
  link_table <- stpd_multitrack_final_bind(
    stpd_multitrack_final_empty_links(), links
  )
  created <- link_table[link_table$creates_new_event, , drop = FALSE]
  if (nrow(created) > 0L) {
    for (i in seq_len(nrow(created))) {
      review_row <- auto$review_candidates[
        auto$review_candidates$review_candidate_id ==
          created$review_candidate_id[i] &
          auto$review_candidates$train == created$train[i], , drop = FALSE
      ]
      hit <- per_isi_source$train == created$train[i] &
        per_isi_source$isi_index >= review_row$start_isi[1] &
        per_isi_source$isi_index <= review_row$end_isi[1]
      per_isi_source$pattern_auto_event_preview_interval_id[hit] <-
        review_row$source_interval_id[1]
    }
  }
  per_isi <- stpd_multitrack_auto_per_isi(
    per_isi_source,
    events, auto$states, auto$gaps, auto$review_candidates,
    auto$metadata$policy_hash, auto$metadata$run_id, auto$metadata$params_hash
  )
  replay <- list(
    current_decisions = current, events = events, states = auto$states,
    gaps = auto$gaps, review_candidates = auto$review_candidates,
    state_event_relationships = relationships,
    review_event_links = link_table, per_isi = per_isi
  )
  for (name in c(
    "events", "states", "gaps", "review_candidates",
    "state_event_relationships", "review_event_links", "per_isi"
  )) if (nrow(replay[[name]]) > 0L && "authoritative" %in% names(replay[[name]])) {
    replay[[name]]$authoritative <- FALSE
  }
  replay
}

stpd_multitrack_final_manifest <- function(tables, run_id, params_hash) {
  data.frame(
    schema_version = stpd_multitrack_final_schema_version(), run_id = run_id,
    params_hash = params_hash, table_name = names(tables),
    row_count = vapply(tables, nrow, integer(1)),
    column_count = vapply(tables, ncol, integer(1)),
    column_types = vapply(tables, function(x) paste(
      paste(names(x), vapply(x, typeof, character(1)), sep = ":"),
      collapse = ";"
    ), character(1)),
    table_sha256 = vapply(tables, stpd_multitrack_auto_hash, character(1)),
    stringsAsFactors = FALSE
  )
}

stpd_multitrack_final_failure <- function(
    auto, migration, materialization_status = "failed_closed",
    failure_message = "Legacy Review decisions require explicit readjudication.") {
  empty <- list(
    migration_history = stpd_multitrack_final_empty_history(),
    current_decisions = stpd_multitrack_final_empty_decisions(),
    events = stpd_multitrack_auto_empty_events(),
    states = stpd_multitrack_auto_empty_states(),
    gaps = stpd_multitrack_auto_empty_gaps(),
    review_candidates = stpd_multitrack_auto_empty_review(),
    state_event_relationships = stpd_multitrack_auto_empty_relationships(),
    review_event_links = stpd_multitrack_final_empty_links(),
    per_isi = stpd_multitrack_auto_empty_per_isi(),
    invariants = stpd_multitrack_final_empty_invariants()
  )
  product_hash <- stpd_multitrack_auto_hash(empty)
  metadata <- data.frame(
    schema_version = stpd_multitrack_final_schema_version(),
    run_id = auto$metadata$run_id, params_hash = auto$metadata$params_hash,
    authoritative = FALSE, authority_scope = "none_preview",
    intended_target_scope = "reviewed_prediction_record",
    biological_ground_truth = FALSE, promotion_gate = "gate_b",
    promotion_status = "pending", materialization_status = materialization_status,
    migration_status = migration$status, failure_code = migration$reason,
    failure_message = failure_message,
    parent_auto_schema_version = auto$metadata$schema_version,
    parent_auto_policy_hash = auto$metadata$policy_hash,
    parent_auto_product_sha256 = auto$metadata$product_sha256,
    parent_auto_manifest_sha256 = stpd_multitrack_final_auto_manifest_hash(auto),
    legacy_history_sha256 = stpd_multitrack_auto_hash(migration$legacy),
    migration_history_sha256 = stpd_multitrack_auto_hash(empty$migration_history),
    events_n = 0L, states_n = 0L, gaps_n = 0L, review_candidates_n = 0L,
    state_event_relationships_n = 0L, review_event_links_n = 0L,
    per_isi_n = 0L, product_sha256 = product_hash,
    stringsAsFactors = FALSE
  )
  tables <- c(list(metadata = metadata), empty)
  structure(c(tables, list(manifest = stpd_multitrack_final_manifest(
    tables, metadata$run_id, metadata$params_hash
  ))), class = c("stpd_multitrack_final_product", "list"))
}

stpd_multitrack_final_build <- function(ds) {
  auto <- stpd_multitrack_final_auto_parent(ds)
  if (!identical(auto$metadata$materialization_status, "materialized")) {
    migration <- list(
      status = "not_attempted",
      reason = paste0("parent_auto_", auto$metadata$materialization_status),
      legacy = data.frame(), migrated = stpd_multitrack_final_empty_history()
    )
    return(stpd_multitrack_final_failure(
      auto, migration, materialization_status = "not_available",
      failure_message = paste(
        "Reviewed v2 materialization was not attempted because its canonical",
        "AUTO parent is unavailable."
      )
    ))
  }
  migration <- stpd_multitrack_final_history(ds, auto)
  if (identical(migration$status, "requires_readjudication")) {
    return(stpd_multitrack_final_failure(auto, migration))
  }
  replay <- stpd_multitrack_final_replay(auto, migration$migrated)
  invariants <- data.frame(
    schema_version = rep(stpd_multitrack_final_schema_version(), 7L),
    run_id = rep(auto$metadata$run_id, 7L),
    params_hash = rep(auto$metadata$params_hash, 7L),
    check_name = c(
      "exact_auto_parent_binding", "legacy_hash_chain_preserved",
      "exact_review_source_migration", "same_track_event_non_overlap",
      "complete_state_event_relationships", "full_per_isi_closure",
      "legacy_projection_unchanged"
    ), status = rep("pass", 7L),
    message = c(
      "FINAL binds the exact canonical AUTO product and manifest.",
      "Every migrated row retains the immutable legacy transition SHA-256.",
      "Only one-to-one train/source/geometry matches are replayed.",
      "Confirmed reviews cannot create overlapping Event intervals.",
      "All final Event x State overlaps are deterministically recomputed.",
      "The final per-ISI table is rebuilt and bidirectionally validated.",
      "Legacy pattern_auto and events remain a parallel compatibility projection."
    ), stringsAsFactors = FALSE
  )
  payload <- c(list(migration_history = migration$migrated), replay,
               list(invariants = invariants))
  product_hash <- stpd_multitrack_auto_hash(payload)
  metadata <- data.frame(
    schema_version = stpd_multitrack_final_schema_version(),
    run_id = auto$metadata$run_id, params_hash = auto$metadata$params_hash,
    authoritative = FALSE, authority_scope = "none_preview",
    intended_target_scope = "reviewed_prediction_record",
    biological_ground_truth = FALSE, promotion_gate = "gate_b",
    promotion_status = "pending", materialization_status = "materialized",
    migration_status = migration$status, failure_code = "", failure_message = "",
    parent_auto_schema_version = auto$metadata$schema_version,
    parent_auto_policy_hash = auto$metadata$policy_hash,
    parent_auto_product_sha256 = auto$metadata$product_sha256,
    parent_auto_manifest_sha256 = stpd_multitrack_final_auto_manifest_hash(auto),
    legacy_history_sha256 = stpd_multitrack_auto_hash(migration$legacy),
    migration_history_sha256 = stpd_multitrack_auto_hash(migration$migrated),
    events_n = as.integer(nrow(replay$events)),
    states_n = as.integer(nrow(replay$states)), gaps_n = as.integer(nrow(replay$gaps)),
    review_candidates_n = as.integer(nrow(replay$review_candidates)),
    state_event_relationships_n = as.integer(nrow(replay$state_event_relationships)),
    review_event_links_n = as.integer(nrow(replay$review_event_links)),
    per_isi_n = as.integer(nrow(replay$per_isi)), product_sha256 = product_hash,
    stringsAsFactors = FALSE
  )
  tables <- c(list(metadata = metadata), payload)
  product <- structure(c(tables, list(manifest = stpd_multitrack_final_manifest(
    tables, metadata$run_id, metadata$params_hash
  ))), class = c("stpd_multitrack_final_product", "list"))
  stpd_multitrack_final_validate(product, parent = ds)
  product
}

stpd_multitrack_final_validate <- function(product, parent = NULL) {
  required <- c(
    "metadata", "migration_history", "current_decisions", "events", "states",
    "gaps", "review_candidates", "state_event_relationships",
    "review_event_links", "per_isi", "invariants", "manifest"
  )
  if (!inherits(product, "stpd_multitrack_final_product") ||
      !identical(names(product), required)) {
    stpd_multitrack_final_abort(
      "final_product_schema_invalid", "The v2 FINAL product shape is invalid."
    )
  }
  metadata <- product$metadata
  prototypes <- list(
    metadata = stpd_multitrack_final_empty_metadata(),
    migration_history = stpd_multitrack_final_empty_history(),
    current_decisions = stpd_multitrack_final_empty_decisions(),
    events = stpd_multitrack_auto_empty_events(),
    states = stpd_multitrack_auto_empty_states(),
    gaps = stpd_multitrack_auto_empty_gaps(),
    review_candidates = stpd_multitrack_auto_empty_review(),
    state_event_relationships = stpd_multitrack_auto_empty_relationships(),
    review_event_links = stpd_multitrack_final_empty_links(),
    per_isi = stpd_multitrack_auto_empty_per_isi(),
    invariants = stpd_multitrack_final_empty_invariants(),
    manifest = stpd_multitrack_final_empty_manifest()
  )
  schema_ok <- vapply(names(prototypes), function(name) {
    table <- product[[name]]
    prototype <- prototypes[[name]]
    is.data.frame(table) && identical(class(table), "data.frame") &&
      identical(names(table), names(prototype)) &&
      identical(vapply(table, typeof, character(1)),
                vapply(prototype, typeof, character(1)))
  }, logical(1))
  if (!all(schema_ok)) stpd_multitrack_final_abort(
    "final_table_schema_invalid",
    paste0("FINAL fixed table schema failed for: ",
           paste(names(schema_ok)[!schema_ok], collapse = ", "), ".")
  )
  pending_authority <- nrow(metadata) == 1L &&
    !isTRUE(metadata$authoritative) && metadata$authority_scope == "none_preview" &&
    metadata$promotion_status == "pending"
  passed_authority <- nrow(metadata) == 1L &&
    isTRUE(metadata$authoritative) &&
    metadata$authority_scope == "reviewed_prediction_record" &&
    metadata$promotion_status == "passed"
  if (nrow(metadata) != 1L ||
      !identical(metadata$schema_version, stpd_multitrack_final_schema_version()) ||
      !(pending_authority || passed_authority) ||
      isTRUE(metadata$biological_ground_truth)) {
    stpd_multitrack_final_abort(
      "final_authority_contract_invalid", "Gate-B-pending FINAL authority is invalid."
    )
  }
  payload_names <- setdiff(required, c("metadata", "manifest"))
  if (!identical(metadata$product_sha256,
                 stpd_multitrack_auto_hash(product[payload_names]))) {
    stpd_multitrack_final_abort(
      "final_product_hash_invalid", "The v2 FINAL payload hash is invalid."
    )
  }
  tables <- product[setdiff(required, "manifest")]
  expected_manifest <- stpd_multitrack_final_manifest(
    tables, metadata$run_id, metadata$params_hash
  )
  if (!identical(product$manifest, expected_manifest)) {
    stpd_multitrack_final_abort(
      "final_manifest_invalid", "The v2 FINAL manifest is invalid."
    )
  }
  if (identical(metadata$materialization_status, "materialized")) {
    authority_tables <- c(
      "events", "states", "gaps", "review_candidates",
      "state_event_relationships", "review_event_links", "per_isi"
    )
    if (any(vapply(product[authority_tables], function(table) {
      nrow(table) > 0L && !all(table$authoritative == metadata$authoritative)
    }, logical(1)))) {
      stpd_multitrack_final_abort(
        "final_table_authority_invalid",
        "FINAL table authority flags disagree with product authority."
      )
    }
    count_ok <- identical(metadata$events_n, as.integer(nrow(product$events))) &&
      identical(metadata$states_n, as.integer(nrow(product$states))) &&
      identical(metadata$gaps_n, as.integer(nrow(product$gaps))) &&
      identical(metadata$review_candidates_n,
                as.integer(nrow(product$review_candidates))) &&
      identical(metadata$state_event_relationships_n,
                as.integer(nrow(product$state_event_relationships))) &&
      identical(metadata$review_event_links_n,
                as.integer(nrow(product$review_event_links))) &&
      identical(metadata$per_isi_n, as.integer(nrow(product$per_isi)))
    if (!count_ok) stpd_multitrack_final_abort(
      "final_metadata_count_invalid", "FINAL metadata counts do not match tables."
    )
    stpd_multitrack_auto_assert_same_track_non_overlap(product$events, "event_id")
    expected_relationships <- stpd_multitrack_auto_relationships(
      product$events, product$states, metadata$parent_auto_policy_hash,
      metadata$run_id, metadata$params_hash
    )
    if (passed_authority && nrow(expected_relationships) > 0L) {
      expected_relationships$authoritative <- TRUE
    }
    if (!identical(product$state_event_relationships, expected_relationships)) {
      stpd_multitrack_final_abort(
        "final_relationship_closure_invalid",
        "FINAL Event x State relationships are incomplete or altered."
      )
    }
    expected_per_isi <- stpd_multitrack_auto_per_isi(
      stpd_multitrack_final_per_isi_source(product),
      product$events, product$states, product$gaps,
      product$review_candidates, metadata$parent_auto_policy_hash,
      metadata$run_id, metadata$params_hash
    )
    if (passed_authority && nrow(expected_per_isi) > 0L) {
      expected_per_isi$authoritative <- TRUE
    }
    if (!identical(product$per_isi, expected_per_isi)) {
      stpd_multitrack_final_abort(
        "final_per_isi_closure_invalid",
        "FINAL per-ISI assignments do not exactly close over its interval tables."
      )
    }
    interval_projection_ok <- function(table, id_col, per_id_col) {
      if (nrow(table) == 0L) return(TRUE)
      all(vapply(seq_len(nrow(table)), function(i) {
        expected <- product$per_isi$train == table$train[i] &
          product$per_isi$isi_index >= table$start_isi[i] &
          product$per_isi$isi_index <= table$end_isi[i]
        identical(
          which(product$per_isi[[per_id_col]] == table[[id_col]][i]),
          which(expected)
        )
      }, logical(1)))
    }
    if (!interval_projection_ok(product$events, "event_id", "event_id") ||
        !interval_projection_ok(product$states, "state_id", "state_id") ||
        !interval_projection_ok(product$gaps, "gap_id", "gap_id")) {
      stpd_multitrack_final_abort(
        "final_per_isi_closure_invalid",
        "A FINAL interval is not projected over exactly its complete ISI span."
      )
    }
    if (nrow(product$review_event_links) > 0L) {
      for (i in seq_len(nrow(product$review_event_links))) {
        link <- product$review_event_links[i, , drop = FALSE]
        review <- product$review_candidates[
          product$review_candidates$train == link$train &
            product$review_candidates$review_candidate_id ==
              link$review_candidate_id, , drop = FALSE
        ]
        event <- product$events[
          product$events$train == link$train &
            product$events$event_id == link$event_id, , drop = FALSE
        ]
        decision <- product$current_decisions[
          product$current_decisions$train == link$train &
            product$current_decisions$review_candidate_id ==
              link$review_candidate_id &
            product$current_decisions$current_status == "confirmed",
          , drop = FALSE
        ]
        ok <- nrow(review) == 1L && nrow(event) == 1L && nrow(decision) == 1L &&
          review$start_isi == event$start_isi &&
          review$end_isi == event$end_isi &&
          link$migration_id == decision$current_migration_id &&
          isTRUE(link$exact_span) && isTRUE(link$non_destructive)
        if (!ok) stpd_multitrack_final_abort(
          "final_review_event_link_invalid",
          "A FINAL Review-to-Event link violates exact geometry or provenance."
        )
      }
    }
  }
  if (!is.null(parent)) {
    auto <- stpd_multitrack_final_auto_parent(parent)
    if (!identical(metadata$parent_auto_schema_version,
                   auto$metadata$schema_version) ||
        !identical(metadata$parent_auto_policy_hash, auto$metadata$policy_hash) ||
        !identical(metadata$parent_auto_product_sha256,
                   auto$metadata$product_sha256) ||
        !identical(metadata$parent_auto_manifest_sha256,
                   stpd_multitrack_final_auto_manifest_hash(auto))) {
      stpd_multitrack_final_abort(
        "final_parent_binding_invalid", "FINAL does not bind its exact AUTO parent."
      )
    }
    if (identical(metadata$materialization_status, "materialized")) {
      migration <- stpd_multitrack_final_history(parent, auto)
      if (!identical(metadata$migration_status, migration$status)) {
        stpd_multitrack_final_abort(
          "final_migration_status_invalid",
          "FINAL migration status differs from its current legacy history."
        )
      }
      replay <- stpd_multitrack_final_replay(auto, migration$migrated)
      if (passed_authority) {
        replay <- stpd_multitrack_gate_b_promote_final_tables(replay)
      }
      expected <- c(list(migration_history = migration$migrated), replay)
      actual <- product[names(expected)]
      if (!all(vapply(names(expected), function(name) {
        identical(actual[[name]], expected[[name]])
      }, logical(1)))) {
        stpd_multitrack_final_abort(
          "final_deterministic_replay_invalid",
          "FINAL differs from exact AUTO plus append-only Review replay."
        )
      }
    }
  }
  invisible(TRUE)
}

stpd_multitrack_final_attach <- function(ds) {
  legacy_auto <- lapply(ds$trains %||% list(), function(x) x$pattern_auto %||% NULL)
  legacy_events <- (ds$results %||% list())$events %||% NULL
  product <- stpd_multitrack_final_build(ds)
  out <- ds
  if (is.null(out$results) || !is.list(out$results)) out$results <- list()
  out$results[[stpd_multitrack_final_result_key()]] <- product
  if (!identical(legacy_auto, lapply(out$trains %||% list(),
                                    function(x) x$pattern_auto %||% NULL)) ||
      !identical(legacy_events, (out$results %||% list())$events %||% NULL)) {
    stpd_multitrack_final_abort(
      "final_legacy_projection_mutated",
      "Attaching v2 FINAL changed legacy labels or events."
    )
  }
  out
}

stpd_multitrack_final_strip <- function(ds) {
  if (is.list(ds) && is.list(ds$results) &&
      stpd_multitrack_final_result_key() %in% names(ds$results)) {
    ds$results[[stpd_multitrack_final_result_key()]] <- NULL
  }
  ds
}

#' Access the Gate-B reviewed multi-track product
#'
#' @param ds A detector dataset or a `stpd_multitrack_final_product`.
#' @return A strictly validated reviewed product, authoritative only after
#' Gate B passes and exact migration/materialization succeeds.
#' @export
stpd_multitrack_final <- function(ds) {
  product <- if (inherits(ds, "stpd_multitrack_final_product")) ds else
    (ds$results %||% list())[[stpd_multitrack_final_result_key()]] %||% NULL
  if (is.null(product)) stpd_multitrack_final_abort(
    "final_product_missing", "No v2 reviewed multi-track product is attached."
  )
  stpd_multitrack_final_validate(
    product, parent = if (inherits(ds, "stpd_multitrack_final_product")) NULL else ds
  )
  product
}
