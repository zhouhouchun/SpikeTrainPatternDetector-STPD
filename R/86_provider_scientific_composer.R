# Provider-independent scientific relationship and Regime composer -----------

STPD_PROVIDER_COMPOSER_VERSION <- "stpd_provider_scientific_composer_v1"
STPD_PROVIDER_COMPOSER_DECISION_VERSION <-
  "stpd_provider_scientific_composer_decision_v1"

stpd_provider_composer_abort <- function(code, message, value = NULL) {
  condition <- structure(
    list(message = as.character(message)[1L], call = NULL,
         code = as.character(code)[1L], offending_value = value),
    class = c(as.character(code)[1L], "stpd_provider_composer_error",
              "error", "condition")
  )
  stop(condition)
}

stpd_provider_composer_policy <- function() {
  list(
    schema_version = STPD_PROVIDER_COMPOSER_VERSION,
    input_scope = "one_explicit_provider_run_only",
    source_modes = c("auto", "adjudicated"),
    child_policy = "preserve_selected_event_state_gap_geometry_exactly",
    relationships = c(
      "hfs_subtype_of_broad_hfs_exact_support",
      "burst_embedded_in_carrier_state_non_destructive",
      "pause_interrupted_broad_hfs_segments"
    ),
    conflicts = c(
      "canonical_pause_overlaps_broad_hfs_direct_support",
      "hfs_subtype_parent_missing_after_adjudication"
    ),
    recurrent_bursting = "three_selected_burst_events_post_selection",
    recurrent_pausing = "five_selected_pause_gaps_post_selection",
    recurrent_long_pause_route =
      "unavailable_without_explicit_high_specificity_evidence",
    window_policy = stpd_event_regime_window_policy(),
    expansion = "left_to_right_one_pass_fixed_anchor_no_recursive_parent_input",
    authoritative = FALSE,
    detection_recomposition_performed = FALSE,
    automatic_provider_fusion = FALSE
  )
}

stpd_provider_composer_policy_hash <- function() {
  stpd_provider_hash_domain(
    "stpd-provider-scientific-composer-policy-v1",
    stpd_provider_composer_policy()
  )
}

stpd_provider_composer_empty <- function() {
  list(
    metadata = data.frame(
      schema_version = character(), policy_hash = character(),
      provider_run_id = character(), source_mode = character(),
      source_bundle_sha256 = character(), source_product_sha256 = character(),
      decision_id = character(), information_access = character(),
      authority_scope = character(), performance_use = character(),
      authoritative = logical(), biological_ground_truth = logical(),
      detection_recomposition_performed = logical(),
      automatic_provider_fusion = logical(), one_pass_nonrecursive = logical(),
      materialization_status = character(), children_n = integer(),
      relationships_n = integer(), regimes_n = integer(),
      memberships_n = integer(), conflicts_n = integer(),
      product_sha256 = character(), stringsAsFactors = FALSE
    ),
    decision = data.frame(
      schema_version = character(), policy_hash = character(),
      source_bundle_sha256 = character(), provider_run_id = character(),
      source_mode = character(), source_product_sha256 = character(),
      adjudication_product_sha256 = character(), approval_status = character(),
      scientific_owner = character(), rationale = character(),
      decided_utc = character(), decision_id = character(),
      stringsAsFactors = FALSE
    ),
    children = data.frame(
      schema_version = character(), provider_run_id = character(),
      child_id = character(), source_entity_id = character(),
      source_candidate_id = character(), review_decision_id = character(),
      train_key = character(), child_domain = character(),
      child_class = character(), canonical_start_isi = integer(),
      canonical_end_isi = integer(), canonical_start_spike = integer(),
      canonical_end_spike = integer(), canonical_start_time_sec = double(),
      canonical_end_time_sec = double(), canonical_interval_closure = character(),
      geometry_origin = character(), selection_status = character(),
      non_destructive = logical(), stringsAsFactors = FALSE
    ),
    relationships = data.frame(
      schema_version = character(), relationship_id = character(),
      relationship_type = character(), provider_run_id = character(),
      train_key = character(), from_child_id = character(),
      to_child_id = character(), mediator_child_id = character(),
      overlap_start_isi = integer(), overlap_end_isi = integer(),
      overlap_isi_n = integer(), scientific_status = character(),
      non_destructive = logical(), stringsAsFactors = FALSE
    ),
    regimes = data.frame(
      schema_version = character(), policy_hash = character(),
      provider_run_id = character(), regime_id = character(),
      train_key = character(), regime_class = character(),
      trigger_route = character(), candidate_status = character(),
      authoritative = logical(), biological_ground_truth = logical(),
      canonical_start_isi = integer(), canonical_end_isi = integer(),
      canonical_start_time_sec = double(), canonical_end_time_sec = double(),
      envelope_n_isi = integer(), direct_support_isi_n = integer(),
      interruption_isi_n = integer(), interruption_fraction = double(),
      child_n = integer(), trigger_child_n = integer(),
      temporal_budget_pass = logical(), one_pass_nonrecursive = logical(),
      stringsAsFactors = FALSE
    ),
    memberships = data.frame(
      schema_version = character(), regime_id = character(),
      provider_run_id = character(), train_key = character(),
      regime_class = character(), child_id = character(),
      child_domain = character(), child_order = integer(),
      trigger_contributor = logical(), non_destructive = logical(),
      stringsAsFactors = FALSE
    ),
    conflicts = data.frame(
      schema_version = character(), conflict_id = character(),
      provider_run_id = character(), train_key = character(),
      conflict_type = character(), primary_child_id = character(),
      secondary_child_id = character(), scientific_status = character(),
      message = character(), stringsAsFactors = FALSE
    ),
    invariants = data.frame(
      schema_version = character(), check_name = character(),
      status = character(), message = character(), stringsAsFactors = FALSE
    ),
    manifest = data.frame(
      schema_version = character(), table_name = character(),
      row_count = integer(), column_count = integer(),
      column_types = character(), table_sha256 = character(),
      stringsAsFactors = FALSE
    )
  )
}

stpd_provider_composer_bind <- function(prototype, rows) {
  if (!length(rows)) return(prototype)
  dplyr::bind_rows(c(list(prototype), rows))
}

stpd_provider_composer_run <- function(provider_bundle, provider_run_id) {
  hit <- which(provider_bundle$provider_runs$provider_run_id == provider_run_id)
  if (length(hit) != 1L) {
    stpd_provider_composer_abort(
      "composer_provider_run_not_found",
      "The explicit provider_run_id does not identify one provider run."
    )
  }
  run <- provider_bundle$provider_runs[hit, , drop = FALSE]
  if (!identical(run$run_status[[1L]], "complete")) {
    stpd_provider_composer_abort(
      "composer_provider_run_incomplete",
      "Only a complete provider run can enter scientific composition."
    )
  }
  run
}

stpd_provider_composer_text <- function(x, name, max_bytes = 4096L) {
  if (!is.character(x) || length(x) != 1L || is.object(x) || is.na(x) ||
      !nzchar(x) || nchar(x, type = "bytes") > max_bytes) {
    stpd_provider_composer_abort(
      "composer_decision_invalid",
      paste0(name, " must be one bounded non-empty plain character scalar.")
    )
  }
  enc2utf8(x)
}

stpd_provider_composer_decision_id <- function(row) {
  fields <- setdiff(names(row), "decision_id")
  stpd_provider_hash_domain(
    "stpd-provider-scientific-composer-decision-v1",
    as.list(row[1L, fields, drop = FALSE])
  )
}

#' Record an explicit scientific-composer source decision
#'
#' @param provider_bundle A validated immutable provider bundle v1.
#' @param provider_run_id Exactly one provider run to compose.
#' @param source_mode Either `auto` or `adjudicated`.
#' @param adjudication The matching adjudication v2 product when requested.
#' @param scientific_owner A scientific-owner pseudonym.
#' @param rationale A non-empty rationale for this descriptive composition.
#' @param decided_utc Explicit RFC3339 UTC audit time.
#' @return A hashed one-row scientific decision record.
#' @export
stpd_provider_composer_decision <- function(
    provider_bundle, provider_run_id, source_mode = c("auto", "adjudicated"),
    adjudication = NULL, scientific_owner, rationale, decided_utc) {
  bundle_before <- serialize(provider_bundle, NULL, version = 3L)
  stpd_validate_provider_bundle(provider_bundle)
  provider_run_id <- stpd_provider_composer_text(
    provider_run_id, "provider_run_id", 64L
  )
  source_mode <- match.arg(source_mode)
  owner <- stpd_provider_composer_text(
    scientific_owner, "scientific_owner", 256L
  )
  rationale <- stpd_provider_composer_text(rationale, "rationale", 4096L)
  decided_utc <- stpd_provider_composer_text(decided_utc, "decided_utc", 64L)
  if (!grepl(
    "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}(\\.[0-9]+)?Z$",
    decided_utc
  ) || is.na(suppressWarnings(as.POSIXct(
    sub("Z$", "", decided_utc), format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"
  )))) {
    stpd_provider_composer_abort(
      "composer_decision_invalid", "decided_utc must be valid RFC3339 UTC."
    )
  }
  run <- stpd_provider_composer_run(provider_bundle, provider_run_id)
  bundle_sha <- stpd_provider_adjudication_source_sha256(provider_bundle)
  adjudication_sha <- NA_character_
  if (source_mode == "auto") {
    if (!identical(run$output_role[[1L]], "automatic_prediction") ||
        !identical(run$authority_scope[[1L]],
                   "automatic_prediction_record")) {
      stpd_provider_composer_abort(
        "composer_authority_invalid",
        "AUTO composition cannot promote candidate-support or calibration output."
      )
    }
    source_sha <- run$normalized_output_sha256[[1L]]
  } else {
    if (is.null(adjudication)) {
      stpd_provider_composer_abort(
        "composer_adjudication_required",
        "adjudicated source_mode requires the matching Phase C product."
      )
    }
    stpd_validate_provider_adjudication(provider_bundle, adjudication)
    adjudication_sha <- adjudication$product_sha256
    source_sha <- adjudication_sha
  }
  row <- data.frame(
    schema_version = STPD_PROVIDER_COMPOSER_DECISION_VERSION,
    policy_hash = stpd_provider_composer_policy_hash(),
    source_bundle_sha256 = bundle_sha, provider_run_id = provider_run_id,
    source_mode = source_mode, source_product_sha256 = source_sha,
    adjudication_product_sha256 = adjudication_sha,
    approval_status = "approved_for_descriptive_composition",
    scientific_owner = owner, rationale = rationale, decided_utc = decided_utc,
    decision_id = NA_character_, stringsAsFactors = FALSE
  )
  row$decision_id <- stpd_provider_composer_decision_id(row)
  if (!identical(bundle_before, serialize(provider_bundle, NULL, version = 3L))) {
    stpd_provider_composer_abort(
      "composer_parent_mutated", "Decision creation mutated provider AUTO."
    )
  }
  row
}

stpd_provider_composer_validate_decision <- function(
    provider_bundle, decision, adjudication = NULL) {
  prototype <- stpd_provider_composer_empty()$decision
  if (!is.data.frame(decision) || nrow(decision) != 1L ||
      !identical(names(decision), names(prototype)) ||
      any(vapply(names(prototype), function(name) {
        !identical(typeof(decision[[name]]), typeof(prototype[[name]]))
      }, logical(1)))) {
    stpd_provider_composer_abort(
      "composer_decision_invalid", "Invalid scientific decision schema."
    )
  }
  if (!identical(decision$schema_version[[1L]],
                 STPD_PROVIDER_COMPOSER_DECISION_VERSION) ||
      !identical(decision$policy_hash[[1L]],
                 stpd_provider_composer_policy_hash()) ||
      !identical(decision$source_bundle_sha256[[1L]],
                 stpd_provider_adjudication_source_sha256(provider_bundle)) ||
      !identical(decision$approval_status[[1L]],
                 "approved_for_descriptive_composition") ||
      !identical(decision$decision_id[[1L]],
                 stpd_provider_composer_decision_id(decision))) {
    stpd_provider_composer_abort(
      "composer_decision_invalid", "Scientific decision identity is invalid."
    )
  }
  if (!identical(decision$approval_status[[1L]],
                 "approved_for_descriptive_composition") ||
      any(!nzchar(c(decision$scientific_owner[[1L]],
                    decision$rationale[[1L]]))) ||
      !grepl(
        "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}(\\.[0-9]+)?Z$",
        decision$decided_utc[[1L]]
      ) || is.na(suppressWarnings(as.POSIXct(
        sub("Z$", "", decision$decided_utc[[1L]]),
        format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"
      )))) {
    stpd_provider_composer_abort(
      "composer_decision_invalid", "Scientific decision audit fields are invalid."
    )
  }
  run <- stpd_provider_composer_run(
    provider_bundle, decision$provider_run_id[[1L]]
  )
  mode <- decision$source_mode[[1L]]
  if (!(mode %in% c("auto", "adjudicated"))) {
    stpd_provider_composer_abort(
      "composer_decision_invalid", "Unknown composer source_mode."
    )
  }
  if (mode == "auto") {
    if (!is.na(decision$adjudication_product_sha256[[1L]]) ||
        !identical(decision$source_product_sha256[[1L]],
                   run$normalized_output_sha256[[1L]]) ||
        !identical(run$output_role[[1L]], "automatic_prediction") ||
        !identical(run$authority_scope[[1L]],
                   "automatic_prediction_record")) {
      stpd_provider_composer_abort(
        "composer_authority_invalid", "AUTO decision authority is invalid."
      )
    }
  } else {
    if (is.null(adjudication)) {
      stpd_provider_composer_abort(
        "composer_adjudication_required", "The adjudication parent is missing."
      )
    }
    stpd_validate_provider_adjudication(provider_bundle, adjudication)
    if (!identical(decision$adjudication_product_sha256[[1L]],
                   adjudication$product_sha256) ||
        !identical(decision$source_product_sha256[[1L]],
                   adjudication$product_sha256)) {
      stpd_provider_composer_abort(
        "composer_source_stale", "The adjudication decision parent is stale."
      )
    }
  }
  invisible(run)
}

stpd_provider_composer_candidate_rows <- function(candidates, source_sha) {
  if (!nrow(candidates)) return(stpd_provider_composer_empty()$children)
  rows <- vector("list", nrow(candidates))
  for (i in seq_len(nrow(candidates))) {
    x <- candidates[i, , drop = FALSE]
    payload <- list(
      source_product_sha256 = source_sha,
      source_entity_id = x$candidate_id[[1L]],
      train_key = x$train_key[[1L]], semantic_track = x$semantic_track[[1L]],
      proposed_label = x$proposed_label[[1L]],
      canonical_start_isi = x$canonical_start_isi[[1L]],
      canonical_end_isi = x$canonical_end_isi[[1L]],
      canonical_start_time_sec = x$canonical_start_time_sec[[1L]],
      canonical_end_time_sec = x$canonical_end_time_sec[[1L]]
    )
    rows[[i]] <- data.frame(
      schema_version = STPD_PROVIDER_COMPOSER_VERSION,
      provider_run_id = x$provider_run_id[[1L]],
      child_id = stpd_provider_hash_domain(
        "stpd-provider-composer-child-v1", payload
      ),
      source_entity_id = x$candidate_id[[1L]],
      source_candidate_id = x$candidate_id[[1L]],
      review_decision_id = NA_character_, train_key = x$train_key[[1L]],
      child_domain = x$semantic_track[[1L]],
      child_class = x$proposed_label[[1L]],
      canonical_start_isi = x$canonical_start_isi[[1L]],
      canonical_end_isi = x$canonical_end_isi[[1L]],
      canonical_start_spike = x$canonical_start_spike[[1L]],
      canonical_end_spike = x$canonical_end_spike[[1L]],
      canonical_start_time_sec = x$canonical_start_time_sec[[1L]],
      canonical_end_time_sec = x$canonical_end_time_sec[[1L]],
      canonical_interval_closure = x$canonical_interval_closure[[1L]],
      geometry_origin = "provider_auto",
      selection_status = "selected_positive_auto", non_destructive = TRUE,
      stringsAsFactors = FALSE
    )
  }
  stpd_provider_composer_bind(stpd_provider_composer_empty()$children, rows)
}

stpd_provider_composer_review_rows <- function(intervals, source_sha) {
  if (!nrow(intervals)) return(stpd_provider_composer_empty()$children)
  rows <- vector("list", nrow(intervals))
  for (i in seq_len(nrow(intervals))) {
    x <- intervals[i, , drop = FALSE]
    payload <- list(
      source_product_sha256 = source_sha,
      source_entity_id = x$adjudicated_interval_id[[1L]],
      train_key = x$train_key[[1L]], semantic_track = x$semantic_track[[1L]],
      proposed_label = x$proposed_label[[1L]],
      canonical_start_isi = x$canonical_start_isi[[1L]],
      canonical_end_isi = x$canonical_end_isi[[1L]],
      canonical_start_time_sec = x$canonical_start_time_sec[[1L]],
      canonical_end_time_sec = x$canonical_end_time_sec[[1L]]
    )
    rows[[i]] <- data.frame(
      schema_version = STPD_PROVIDER_COMPOSER_VERSION,
      provider_run_id = x$provider_run_id[[1L]],
      child_id = stpd_provider_hash_domain(
        "stpd-provider-composer-child-v1", payload
      ),
      source_entity_id = x$adjudicated_interval_id[[1L]],
      source_candidate_id = x$source_record_id[[1L]],
      review_decision_id = x$decision_id[[1L]], train_key = x$train_key[[1L]],
      child_domain = x$semantic_track[[1L]],
      child_class = x$proposed_label[[1L]],
      canonical_start_isi = x$canonical_start_isi[[1L]],
      canonical_end_isi = x$canonical_end_isi[[1L]],
      canonical_start_spike = x$canonical_start_spike[[1L]],
      canonical_end_spike = x$canonical_end_spike[[1L]],
      canonical_start_time_sec = x$canonical_start_time_sec[[1L]],
      canonical_end_time_sec = x$canonical_end_time_sec[[1L]],
      canonical_interval_closure = x$canonical_interval_closure[[1L]],
      geometry_origin = paste0("adjudicated_", x$geometry_origin[[1L]]),
      selection_status = "selected_by_adjudication", non_destructive = TRUE,
      stringsAsFactors = FALSE
    )
  }
  stpd_provider_composer_bind(stpd_provider_composer_empty()$children, rows)
}

stpd_provider_composer_children <- function(
    provider_bundle, decision, adjudication = NULL) {
  run_id <- decision$provider_run_id[[1L]]
  run <- stpd_provider_composer_run(provider_bundle, run_id)
  candidates <- provider_bundle$candidate_intervals
  candidates <- candidates[
    candidates$provider_run_id == run_id &
      candidates$provider_decision == "positive", , drop = FALSE
  ]
  mode <- decision$source_mode[[1L]]
  if (mode == "auto") {
    out <- stpd_provider_composer_candidate_rows(
      candidates, decision$source_product_sha256[[1L]]
    )
  } else {
    current <- adjudication$current_decisions
    current <- current[current$provider_run_id == run_id, , drop = FALSE]
    reviewed_ids <- current$source_record_id
    if (identical(run$output_role[[1L]], "automatic_prediction")) {
      candidates <- candidates[!candidates$candidate_id %in% reviewed_ids,
                               , drop = FALSE]
      base <- stpd_provider_composer_candidate_rows(
        candidates, decision$source_product_sha256[[1L]]
      )
    } else {
      base <- stpd_provider_composer_empty()$children
    }
    accepted_ids <- current$adjudicated_interval_id[
      current$effect %in% c("accepted", "adjusted") &
        !is.na(current$adjudicated_interval_id)
    ]
    intervals <- adjudication$adjudicated_intervals[
      adjudication$adjudicated_intervals$adjudicated_interval_id %in%
        accepted_ids &
        adjudication$adjudicated_intervals$provider_run_id == run_id,
      , drop = FALSE
    ]
    reviewed <- stpd_provider_composer_review_rows(
      intervals, decision$source_product_sha256[[1L]]
    )
    out <- dplyr::bind_rows(base, reviewed)
  }
  if (nrow(out)) {
    out <- out[order(out$train_key, out$child_domain,
                     out$canonical_start_isi, out$canonical_end_isi,
                     out$child_class, out$child_id, method = "radix"),
               , drop = FALSE]
    rownames(out) <- NULL
  }
  out
}

stpd_provider_composer_overlap <- function(a, b) {
  start <- max(a$canonical_start_isi[[1L]], b$canonical_start_isi[[1L]])
  end <- min(a$canonical_end_isi[[1L]], b$canonical_end_isi[[1L]])
  if (start <= end) c(start = start, end = end, n = end - start + 1L) else
    c(start = NA_integer_, end = NA_integer_, n = 0L)
}

stpd_provider_composer_relationship_row <- function(
    type, from, to, mediator = NA_character_, overlap = c(
      start = NA_integer_, end = NA_integer_, n = 0L
    )) {
  payload <- list(
    type = type, provider_run_id = from$provider_run_id[[1L]],
    train_key = from$train_key[[1L]], from = from$child_id[[1L]],
    to = to$child_id[[1L]], mediator = mediator,
    overlap = as.list(overlap)
  )
  data.frame(
    schema_version = STPD_PROVIDER_COMPOSER_VERSION,
    relationship_id = stpd_provider_hash_domain(
      "stpd-provider-composer-relationship-v1", payload
    ),
    relationship_type = type,
    provider_run_id = from$provider_run_id[[1L]],
    train_key = from$train_key[[1L]], from_child_id = from$child_id[[1L]],
    to_child_id = to$child_id[[1L]], mediator_child_id = mediator,
    overlap_start_isi = as.integer(overlap[["start"]]),
    overlap_end_isi = as.integer(overlap[["end"]]),
    overlap_isi_n = as.integer(overlap[["n"]]),
    scientific_status = "descriptive_relationship",
    non_destructive = TRUE, stringsAsFactors = FALSE
  )
}

stpd_provider_composer_conflict_row <- function(type, primary, secondary,
                                                message) {
  payload <- list(
    type = type, provider_run_id = primary$provider_run_id[[1L]],
    train_key = primary$train_key[[1L]], primary = primary$child_id[[1L]],
    secondary = secondary$child_id[[1L]]
  )
  data.frame(
    schema_version = STPD_PROVIDER_COMPOSER_VERSION,
    conflict_id = stpd_provider_hash_domain(
      "stpd-provider-composer-conflict-v1", payload
    ),
    provider_run_id = primary$provider_run_id[[1L]],
    train_key = primary$train_key[[1L]], conflict_type = type,
    primary_child_id = primary$child_id[[1L]],
    secondary_child_id = secondary$child_id[[1L]],
    scientific_status = "requires_review", message = message,
    stringsAsFactors = FALSE
  )
}

stpd_provider_composer_relationships <- function(children) {
  rel <- list()
  conflicts <- list()
  states <- children[children$child_domain == "state", , drop = FALSE]
  broad <- states[states$child_class == "broad_hfs", , drop = FALSE]
  subtypes <- states[states$child_class %in% c("hft", "hf_irregular"),
                     , drop = FALSE]
  for (i in seq_len(nrow(subtypes))) {
    x <- subtypes[i, , drop = FALSE]
    hit <- which(
      broad$train_key == x$train_key[[1L]] &
        broad$canonical_start_isi == x$canonical_start_isi[[1L]] &
        broad$canonical_end_isi == x$canonical_end_isi[[1L]]
    )
    if (length(hit) == 1L) {
      rel[[length(rel) + 1L]] <- stpd_provider_composer_relationship_row(
        "hfs_subtype_of_broad_hfs", x, broad[hit, , drop = FALSE],
        overlap = stpd_provider_composer_overlap(x, broad[hit, , drop = FALSE])
      )
    } else {
      conflicts[[length(conflicts) + 1L]] <-
        stpd_provider_composer_conflict_row(
          "hfs_subtype_parent_missing", x, x,
          "An HFT/HF-irregular child has no exact selected Broad-HFS parent."
        )
    }
  }
  events <- children[
    children$child_domain == "event" &
      children$child_class %in% c("burst", "long_burst"), , drop = FALSE
  ]
  carriers <- states[states$child_class %in% c("broad_hfs", "tonic"),
                     , drop = FALSE]
  for (i in seq_len(nrow(events))) {
    for (j in seq_len(nrow(carriers))) {
      if (events$train_key[[i]] != carriers$train_key[[j]]) next
      overlap <- stpd_provider_composer_overlap(
        events[i, , drop = FALSE], carriers[j, , drop = FALSE]
      )
      if (overlap[["n"]] > 0L) {
        rel[[length(rel) + 1L]] <- stpd_provider_composer_relationship_row(
          "burst_embedded_in_carrier_state", events[i, , drop = FALSE],
          carriers[j, , drop = FALSE], overlap = overlap
        )
      }
    }
  }
  gaps <- children[
    children$child_domain == "gap" & children$child_class == "pause",
    , drop = FALSE
  ]
  for (i in seq_len(nrow(gaps))) {
    gap <- gaps[i, , drop = FALSE]
    same <- broad[broad$train_key == gap$train_key[[1L]], , drop = FALSE]
    overlapping <- which(
      same$canonical_start_isi <= gap$canonical_end_isi[[1L]] &
        same$canonical_end_isi >= gap$canonical_start_isi[[1L]]
    )
    if (length(overlapping)) {
      for (j in overlapping) {
        conflicts[[length(conflicts) + 1L]] <-
          stpd_provider_composer_conflict_row(
            "canonical_pause_overlaps_broad_hfs_direct_support",
            gap, same[j, , drop = FALSE],
            "Pause and Broad-HFS direct support overlap; neither child was altered."
          )
      }
      next
    }
    pre <- which(same$canonical_end_isi < gap$canonical_start_isi[[1L]])
    post <- which(same$canonical_start_isi > gap$canonical_end_isi[[1L]])
    if (length(pre) && length(post)) {
      pre <- pre[[which.max(same$canonical_end_isi[pre])]]
      post <- post[[which.min(same$canonical_start_isi[post])]]
      rel[[length(rel) + 1L]] <- stpd_provider_composer_relationship_row(
        "pause_interrupted_broad_hfs", same[pre, , drop = FALSE],
        same[post, , drop = FALSE], mediator = gap$child_id[[1L]]
      )
    }
  }
  relationships <- stpd_provider_composer_bind(
    stpd_provider_composer_empty()$relationships, rel
  )
  conflicts <- stpd_provider_composer_bind(
    stpd_provider_composer_empty()$conflicts, conflicts
  )
  if (nrow(relationships)) {
    relationships <- relationships[order(
      relationships$train_key, relationships$relationship_type,
      relationships$relationship_id, method = "radix"
    ), , drop = FALSE]
    rownames(relationships) <- NULL
  }
  if (nrow(conflicts)) {
    conflicts <- conflicts[order(
      conflicts$train_key, conflicts$conflict_type, conflicts$conflict_id,
      method = "radix"
    ), , drop = FALSE]
    rownames(conflicts) <- NULL
  }
  list(relationships = relationships, conflicts = conflicts)
}

stpd_provider_composer_regime_input <- function(children) {
  out <- data.frame(
    train = children$train_key, start_isi = children$canonical_start_isi,
    end_isi = children$canonical_end_isi, .child_id = children$child_id,
    .child_domain = children$child_domain,
    .support_start_time_sec = children$canonical_start_time_sec,
    .support_end_time_sec = children$canonical_end_time_sec,
    .support_duration_sec = pmax(
      0, children$canonical_end_time_sec - children$canonical_start_time_sec
    ), stringsAsFactors = FALSE
  )
  out
}

stpd_provider_composer_regime_rows <- function(selected, regime_class,
                                               trigger_route, trigger_n,
                                               provider_run_id, policy_hash) {
  budget <- stpd_event_regime_window_budget(selected)
  if (!isTRUE(budget$pass)) {
    stpd_provider_composer_abort(
      "composer_temporal_budget_failed",
      "A Regime cannot materialize outside the frozen interruption budget."
    )
  }
  child_ids <- as.character(selected$.child_id)
  regime_id <- stpd_provider_hash_domain(
    "stpd-provider-composer-regime-v1",
    list(policy_hash = policy_hash, provider_run_id = provider_run_id,
         train_key = selected$train[[1L]], regime_class = regime_class,
         trigger_route = trigger_route,
         child_ids = sort(child_ids, method = "radix"))
  )
  regime <- data.frame(
    schema_version = STPD_PROVIDER_COMPOSER_VERSION, policy_hash = policy_hash,
    provider_run_id = provider_run_id, regime_id = regime_id,
    train_key = selected$train[[1L]], regime_class = regime_class,
    trigger_route = trigger_route,
    candidate_status = "unvalidated_descriptive_candidate",
    authoritative = FALSE, biological_ground_truth = FALSE,
    canonical_start_isi = min(selected$start_isi),
    canonical_end_isi = max(selected$end_isi),
    canonical_start_time_sec = budget$start_time_sec,
    canonical_end_time_sec = budget$end_time_sec,
    envelope_n_isi = budget$envelope_n_isi,
    direct_support_isi_n = budget$direct_support_isi_n,
    interruption_isi_n = budget$interruption_isi_n,
    interruption_fraction = budget$interruption_isi_fraction,
    child_n = as.integer(nrow(selected)), trigger_child_n = as.integer(trigger_n),
    temporal_budget_pass = TRUE, one_pass_nonrecursive = TRUE,
    stringsAsFactors = FALSE
  )
  membership <- lapply(seq_len(nrow(selected)), function(i) data.frame(
    schema_version = STPD_PROVIDER_COMPOSER_VERSION, regime_id = regime_id,
    provider_run_id = provider_run_id, train_key = selected$train[[i]],
    regime_class = regime_class, child_id = selected$.child_id[[i]],
    child_domain = selected$.child_domain[[i]], child_order = as.integer(i),
    trigger_contributor = i <= trigger_n, non_destructive = TRUE,
    stringsAsFactors = FALSE
  ))
  list(regime = regime, memberships = membership)
}

stpd_provider_composer_windows <- function(children, child_domain,
                                           child_classes, trigger_n,
                                           regime_class, trigger_route,
                                           provider_run_id, policy_hash) {
  eligible <- children[
    children$child_domain == child_domain &
      children$child_class %in% child_classes, , drop = FALSE
  ]
  rows <- list()
  memberships <- list()
  if (!nrow(eligible)) return(list(regimes = rows, memberships = memberships))
  xall <- stpd_provider_composer_regime_input(eligible)
  groups <- split(xall, xall$train, drop = TRUE)
  groups <- groups[order(names(groups), method = "radix")]
  for (x in groups) {
    x <- x[order(x$start_isi, x$end_isi, x$.child_id,
                 method = "radix"), , drop = FALSE]
    i <- 1L
    while (i + trigger_n - 1L <= nrow(x)) {
      end <- i + trigger_n - 1L
      selected <- x[i:end, , drop = FALSE]
      anchor <- stpd_event_regime_window_budget(selected)
      if (!isTRUE(anchor$pass)) {
        i <- i + 1L
        next
      }
      while (end < nrow(x) && stpd_event_regime_can_expand(
        selected, x[end + 1L, , drop = FALSE], anchor
      )) {
        end <- end + 1L
        selected <- x[i:end, , drop = FALSE]
      }
      materialized <- stpd_provider_composer_regime_rows(
        selected, regime_class, trigger_route, trigger_n,
        provider_run_id, policy_hash
      )
      rows[[length(rows) + 1L]] <- materialized$regime
      memberships <- c(memberships, materialized$memberships)
      i <- end + 1L
    }
  }
  list(regimes = rows, memberships = memberships)
}

stpd_provider_composer_regimes <- function(children, provider_run_id,
                                           policy_hash) {
  burst <- stpd_provider_composer_windows(
    children, "event", c("burst", "long_burst"), 3L,
    "recurrent_bursting", "three_selected_burst_events_post_selection",
    provider_run_id, policy_hash
  )
  pause <- stpd_provider_composer_windows(
    children, "gap", "pause", 5L,
    "recurrent_pausing", "five_selected_pause_gaps_post_selection",
    provider_run_id, policy_hash
  )
  regimes <- stpd_provider_composer_bind(
    stpd_provider_composer_empty()$regimes,
    c(burst$regimes, pause$regimes)
  )
  memberships <- stpd_provider_composer_bind(
    stpd_provider_composer_empty()$memberships,
    c(burst$memberships, pause$memberships)
  )
  if (nrow(regimes)) {
    regimes <- regimes[order(regimes$train_key, regimes$canonical_start_isi,
                             regimes$regime_class, regimes$regime_id,
                             method = "radix"), , drop = FALSE]
    rownames(regimes) <- NULL
  }
  if (nrow(memberships)) {
    memberships <- memberships[order(
      memberships$train_key, memberships$regime_id, memberships$child_order,
      method = "radix"
    ), , drop = FALSE]
    rownames(memberships) <- NULL
  }
  list(regimes = regimes, memberships = memberships)
}

stpd_provider_composer_manifest <- function(tables) {
  data.frame(
    schema_version = STPD_PROVIDER_COMPOSER_VERSION,
    table_name = names(tables),
    row_count = vapply(tables, nrow, integer(1)),
    column_count = vapply(tables, ncol, integer(1)),
    column_types = vapply(tables, function(x) paste(
      paste(names(x), vapply(x, typeof, character(1)), sep = ":"),
      collapse = ";"
    ), character(1)),
    table_sha256 = vapply(tables, function(x) stpd_provider_hash_domain(
      "stpd-provider-composer-table-v1", x
    ), character(1)), stringsAsFactors = FALSE
  )
}

stpd_provider_composer_build <- function(provider_bundle, decision,
                                         adjudication = NULL) {
  run <- stpd_provider_composer_validate_decision(
    provider_bundle, decision, adjudication
  )
  children <- stpd_provider_composer_children(
    provider_bundle, decision, adjudication
  )
  related <- stpd_provider_composer_relationships(children)
  composed <- stpd_provider_composer_regimes(
    children, decision$provider_run_id[[1L]], decision$policy_hash[[1L]]
  )
  conflicts_n <- nrow(related$conflicts)
  invariants <- data.frame(
    schema_version = rep(STPD_PROVIDER_COMPOSER_VERSION, 7L),
    check_name = c(
      "single_provider_run", "children_preserved", "no_detection_recomposition",
      "no_provider_fusion", "orthogonal_event_state_gap",
      "one_pass_nonrecursive", "long_pause_route_evidence_gated"
    ),
    status = rep("pass", 7L),
    message = c(
      "Exactly one explicit provider run is selected.",
      "Selected child identities and geometries are retained without retyping.",
      "The composer adds relationships and Regimes but never reruns detection.",
      "No cross-provider deduplication, voting, or score fusion occurs.",
      "Burst Event, carrier State, and Pause Gap remain separate objects.",
      "Only Event/Gap children enter bounded one-pass Regime aggregation.",
      "Three-long-Pause recurrence is disabled without typed specificity evidence."
    ), stringsAsFactors = FALSE
  )
  source_mode <- decision$source_mode[[1L]]
  information <- if (source_mode == "auto") {
    run$information_access[[1L]]
  } else "manual_aware"
  authority <- if (source_mode == "auto") {
    run$authority_scope[[1L]]
  } else "adjudicated_prediction_record"
  performance <- if (source_mode == "auto" && information == "label_blind") {
    "provider_declared_label_blind_only"
  } else "adjudicated_agreement_only"
  metadata_without_hash <- data.frame(
    schema_version = STPD_PROVIDER_COMPOSER_VERSION,
    policy_hash = decision$policy_hash[[1L]],
    provider_run_id = decision$provider_run_id[[1L]], source_mode = source_mode,
    source_bundle_sha256 = decision$source_bundle_sha256[[1L]],
    source_product_sha256 = decision$source_product_sha256[[1L]],
    decision_id = decision$decision_id[[1L]], information_access = information,
    authority_scope = authority, performance_use = performance,
    authoritative = FALSE, biological_ground_truth = FALSE,
    detection_recomposition_performed = FALSE,
    automatic_provider_fusion = FALSE, one_pass_nonrecursive = TRUE,
    materialization_status = if (conflicts_n) {
      "materialized_with_scientific_conflicts"
    } else "materialized_descriptive",
    children_n = as.integer(nrow(children)),
    relationships_n = as.integer(nrow(related$relationships)),
    regimes_n = as.integer(nrow(composed$regimes)),
    memberships_n = as.integer(nrow(composed$memberships)),
    conflicts_n = as.integer(conflicts_n), product_sha256 = NA_character_,
    stringsAsFactors = FALSE
  )
  payload <- list(
    decision = decision, children = children,
    relationships = related$relationships, regimes = composed$regimes,
    memberships = composed$memberships, conflicts = related$conflicts,
    invariants = invariants
  )
  metadata_without_hash$product_sha256 <- stpd_provider_hash_domain(
    "stpd-provider-scientific-composer-product-v1",
    list(metadata = metadata_without_hash[
      setdiff(names(metadata_without_hash), "product_sha256")
    ], payload = payload)
  )
  tables <- c(list(metadata = metadata_without_hash), payload)
  structure(
    c(tables, list(manifest = stpd_provider_composer_manifest(tables))),
    class = c("stpd_provider_scientific_composition_v1", "list")
  )
}

#' Compose provider-independent scientific relationships and Regimes
#'
#' @param provider_bundle A validated immutable provider bundle v1.
#' @param decision An explicit record from [stpd_provider_composer_decision()].
#' @param adjudication The matching Phase C descendant when selected.
#' @return A descriptive, non-authoritative composition product.
#' @export
stpd_compose_provider_science <- function(provider_bundle, decision,
                                          adjudication = NULL) {
  bundle_before <- serialize(provider_bundle, NULL, version = 3L)
  adjudication_before <- if (is.null(adjudication)) NULL else
    serialize(adjudication, NULL, version = 3L)
  stpd_validate_provider_bundle(provider_bundle)
  out <- stpd_provider_composer_build(
    provider_bundle, decision, adjudication
  )
  stpd_validate_provider_composition(
    provider_bundle, out, adjudication, rematerialize = FALSE
  )
  if (!identical(bundle_before, serialize(provider_bundle, NULL, version = 3L)) ||
      (!is.null(adjudication) && !identical(
        adjudication_before, serialize(adjudication, NULL, version = 3L)
      ))) {
    stpd_provider_composer_abort(
      "composer_parent_mutated", "Composition mutated an immutable parent."
    )
  }
  out
}

#' Validate a provider scientific composition
#'
#' @param provider_bundle The exact provider bundle parent.
#' @param product A provider composition v1 product.
#' @param adjudication The exact Phase C parent when selected.
#' @param rematerialize Rebuild and compare every table when `TRUE`.
#' @return Invisibly `TRUE`, otherwise a typed fail-closed error.
#' @export
stpd_validate_provider_composition <- function(
    provider_bundle, product, adjudication = NULL, rematerialize = TRUE) {
  stpd_validate_provider_bundle(provider_bundle)
  prototypes <- stpd_provider_composer_empty()
  if (!inherits(product, "stpd_provider_scientific_composition_v1") ||
      !identical(names(product), names(prototypes))) {
    stpd_provider_composer_abort(
      "composer_product_invalid", "Composition tables are incomplete."
    )
  }
  for (name in names(prototypes)) {
    x <- product[[name]]
    p <- prototypes[[name]]
    if (!is.data.frame(x) || !identical(names(x), names(p)) ||
        any(vapply(names(p), function(column) {
          !identical(typeof(x[[column]]), typeof(p[[column]]))
        }, logical(1)))) {
      stpd_provider_composer_abort(
        "composer_product_invalid",
        paste0("Invalid composition table: ", name, ".")
      )
    }
  }
  stpd_provider_composer_validate_decision(
    provider_bundle, product$decision, adjudication
  )
  metadata <- product$metadata
  if (nrow(metadata) != 1L ||
      !identical(metadata$schema_version[[1L]], STPD_PROVIDER_COMPOSER_VERSION) ||
      !identical(metadata$policy_hash[[1L]],
                 stpd_provider_composer_policy_hash()) ||
      isTRUE(metadata$authoritative[[1L]]) ||
      isTRUE(metadata$biological_ground_truth[[1L]]) ||
      isTRUE(metadata$detection_recomposition_performed[[1L]]) ||
      isTRUE(metadata$automatic_provider_fusion[[1L]]) ||
      !isTRUE(metadata$one_pass_nonrecursive[[1L]])) {
    stpd_provider_composer_abort(
      "composer_authority_invalid", "Composition authority invariants failed."
    )
  }
  children <- product$children
  if (anyDuplicated(children$child_id) ||
      any(children$provider_run_id != metadata$provider_run_id[[1L]]) ||
      any(!children$child_domain %in% c("event", "state", "gap")) ||
      any(!children$non_destructive)) {
    stpd_provider_composer_abort(
      "composer_children_invalid", "Selected children are invalid."
    )
  }
  child_ids <- children$child_id
  relationships <- product$relationships
  relationship_refs <- c(
    relationships$from_child_id, relationships$to_child_id,
    relationships$mediator_child_id[!is.na(relationships$mediator_child_id)]
  )
  if (anyDuplicated(relationships$relationship_id) ||
      any(relationships$provider_run_id != metadata$provider_run_id[[1L]]) ||
      any(!relationship_refs %in% child_ids) ||
      any(!relationships$non_destructive)) {
    stpd_provider_composer_abort(
      "composer_relationship_invalid", "Relationship closure failed."
    )
  }
  memberships <- product$memberships
  if (any(!memberships$child_id %in% child_ids) ||
      any(memberships$provider_run_id != metadata$provider_run_id[[1L]]) ||
      any(!memberships$regime_id %in% product$regimes$regime_id) ||
      any(!memberships$child_domain %in% c("event", "gap")) ||
      any(!memberships$non_destructive)) {
    stpd_provider_composer_abort(
      "composer_recursive_regime_forbidden", "Regime membership closure failed."
    )
  }
  conflicts <- product$conflicts
  if (anyDuplicated(conflicts$conflict_id) ||
      any(conflicts$provider_run_id != metadata$provider_run_id[[1L]]) ||
      any(!conflicts$primary_child_id %in% child_ids) ||
      any(!conflicts$secondary_child_id %in% child_ids) ||
      any(conflicts$scientific_status != "requires_review")) {
    stpd_provider_composer_abort(
      "composer_conflict_invalid", "Scientific-conflict closure failed."
    )
  }
  if (nrow(product$regimes) &&
      (any(product$regimes$provider_run_id !=
             metadata$provider_run_id[[1L]]) ||
       any(product$regimes$authoritative) ||
       any(product$regimes$biological_ground_truth) ||
       any(!product$regimes$one_pass_nonrecursive) ||
       any(!product$regimes$temporal_budget_pass))) {
    stpd_provider_composer_abort(
      "composer_regime_invalid", "Regime authority or budget is invalid."
    )
  }
  expected_counts <- c(
    children_n = nrow(children), relationships_n = nrow(relationships),
    regimes_n = nrow(product$regimes), memberships_n = nrow(memberships),
    conflicts_n = nrow(product$conflicts)
  )
  observed_counts <- unlist(metadata[1L, names(expected_counts)], use.names = TRUE)
  if (!identical(as.integer(observed_counts), as.integer(expected_counts))) {
    stpd_provider_composer_abort(
      "composer_product_invalid", "Composition metadata counts are stale."
    )
  }
  payload <- product[c(
    "decision", "children", "relationships", "regimes", "memberships",
    "conflicts", "invariants"
  )]
  expected_hash <- stpd_provider_hash_domain(
    "stpd-provider-scientific-composer-product-v1",
    list(metadata = metadata[setdiff(names(metadata), "product_sha256")],
         payload = payload)
  )
  if (!identical(metadata$product_sha256[[1L]], expected_hash)) {
    stpd_provider_composer_abort(
      "composer_product_invalid", "Composition product hash is stale."
    )
  }
  tables <- product[setdiff(names(product), "manifest")]
  if (!identical(product$manifest,
                 stpd_provider_composer_manifest(tables))) {
    stpd_provider_composer_abort(
      "composer_manifest_invalid", "Composition manifest is stale."
    )
  }
  if (isTRUE(rematerialize)) {
    expected <- stpd_provider_composer_build(
      provider_bundle, product$decision, adjudication
    )
    for (name in names(product)) {
      if (!identical(product[[name]], expected[[name]])) {
        stpd_provider_composer_abort(
          "composer_product_invalid",
          paste0("Deterministic rematerialization mismatch in ", name, ".")
        )
      }
    }
  }
  invisible(TRUE)
}
