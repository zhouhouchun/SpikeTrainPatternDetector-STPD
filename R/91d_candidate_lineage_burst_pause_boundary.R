# Gate 1B final Event-boundary observation ---------------------------------
#
# This module observes application of active canonical Pause boundaries to the
# ordinary post-refractory Burst branch.  The boundary source is the closed
# post-ownership contextual-Pause ledger; raw Pause proposals have no boundary
# authority.  The independent hard-threshold Burst root remains outside this
# versioned hook family.

stpd_candidate_lineage_pause_boundary_candidate_hash <- function(row, stage) {
  stpd_candidate_lineage_candidate_source_hash(
    as.data.frame(row, stringsAsFactors = FALSE, check.names = FALSE),
    paste0("canonical_pause_boundary_", as.character(stage)[1L])
  )
}

stpd_candidate_lineage_pause_boundary_raw_pause_hash <- function(row) {
  stpd_candidate_lineage_candidate_source_hash(
    as.data.frame(row, stringsAsFactors = FALSE, check.names = FALSE),
    "canonical_pause_final_event_source"
  )
}

stpd_candidate_lineage_pause_boundary_source_payload <- function(context) {
  boundaries <- context$boundaries
  raw <- context$raw_pause_candidates
  if (!nrow(boundaries)) {
    return(stpd_candidate_lineage_empty_hook_payload(
      "burst_pause_boundary_sources_v1"
    ))
  }
  semantics <- as.character(raw$gap_semantics %||% "")
  hard <- as.logical(raw$hard_for_event %||% FALSE)
  action <- as.character(
    raw$action %||% raw$decision_action %||% "accept"
  )
  eligible <- semantics %in% c(
    "canonical_pause", "contextual_interburst_pause"
  ) & hard & action == "accept"
  eligible[is.na(eligible)] <- FALSE
  rows <- vector("list", nrow(boundaries))
  for (i in seq_len(nrow(boundaries))) {
    start_row <- as.integer(boundaries$start_isi[[i]])
    end_row <- as.integer(boundaries$end_isi[[i]])
    source <- which(
      eligible & as.integer(raw$start_isi) == start_row &
        as.integer(raw$end_isi) == end_row &
        semantics == as.character(boundaries$boundary_kind[[i]])
    )
    if (!length(source)) return(NULL)
    source_hashes <- vapply(source, function(j) {
      stpd_candidate_lineage_pause_boundary_raw_pause_hash(
        raw[j, , drop = FALSE]
      )
    }, character(1))
    source_ids <- stpd_candidate_lineage_candidate_column(
      raw[source, , drop = FALSE], "candidate_id", "character"
    )
    rows[[i]] <- data.frame(
      train = context$train, boundary_ordinal = as.integer(i),
      detector_start_row = start_row, detector_end_row = end_row,
      start_isi = as.integer(start_row - 1L),
      end_isi = as.integer(end_row - 1L),
      boundary_kind = as.character(boundaries$boundary_kind[[i]]),
      source_pause_candidate_n = as.integer(length(source)),
      source_pause_ordinals = paste(as.integer(source), collapse = ";"),
      source_pause_candidate_ids = paste(source_ids, collapse = ";"),
      source_pause_payload_sha256 = stpd_threshold_first_hash_domain(
        "stpd-canonical-pause-boundary-source-set-v1",
        data.frame(
          source_ordinal = as.integer(source),
          source_candidate_id = source_ids,
          source_payload_sha256 = source_hashes,
          stringsAsFactors = FALSE, check.names = FALSE
        )
      ),
      stringsAsFactors = FALSE, check.names = FALSE
    )
  }
  out <- dplyr::bind_rows(rows)
  out[, names(stpd_candidate_lineage_observation_hook_schema(
    "burst_pause_boundary_sources_v1"
  )), drop = FALSE]
}

stpd_candidate_lineage_pause_boundary_upstream_is_closed <- function(
    train_collector, input_candidates, raw_pause_candidates, boundaries) {
  refractory_context <- if (exists(
      "burst_refractory_context", envir = train_collector,
      inherits = FALSE)) {
    train_collector$burst_refractory_context
  } else NULL
  contextual_context <- if (exists(
      "contextual_pause_context", envir = train_collector,
      inherits = FALSE)) {
    train_collector$contextual_pause_context
  } else NULL
  refractory_output <-
    train_collector$observations$burst_refractory_output_v1
  refractory_receipt <-
    train_collector$observations$burst_refractory_receipt_v1
  contextual_receipt <-
    train_collector$observations$contextual_pause_receipt_v1
  expected_boundaries <- if (is.data.frame(raw_pause_candidates)) {
    stpd_event_core_final_event_boundaries(raw_pause_candidates)
  } else NULL
  if (is.null(refractory_context) ||
      !is.data.frame(refractory_context$scientific_out) ||
      is.null(contextual_context) ||
      !is.data.frame(contextual_context$scientific_out) ||
      is.null(refractory_output) || is.null(refractory_receipt) ||
      is.null(contextual_receipt) ||
      !stpd_candidate_lineage_refractory_payload_is_valid(
        train_collector, "burst_refractory_output_v1", refractory_output
      ) ||
      !stpd_candidate_lineage_refractory_payload_is_valid(
        train_collector, "burst_refractory_receipt_v1", refractory_receipt
      ) ||
      !stpd_candidate_lineage_contextual_pause_payload_is_valid(
        train_collector, "contextual_pause_receipt_v1", contextual_receipt
      ) ||
      !identical(input_candidates, refractory_context$scientific_out) ||
      !identical(raw_pause_candidates, contextual_context$scientific_out) ||
      !identical(boundaries, expected_boundaries) ||
      nrow(input_candidates) != nrow(refractory_output)) {
    return(FALSE)
  }
  if (!nrow(input_candidates)) return(TRUE)
  ids <- stpd_candidate_lineage_candidate_column(
    input_candidates, "candidate_id", "character"
  )
  starts <- stpd_candidate_lineage_candidate_column(
    input_candidates, "start_isi", "integer"
  )
  ends <- stpd_candidate_lineage_candidate_column(
    input_candidates, "end_isi", "integer"
  )
  hashes <- vapply(seq_len(nrow(input_candidates)), function(i) {
    stpd_candidate_lineage_refractory_candidate_hash(
      input_candidates[i, , drop = FALSE], "refractory_policy_output"
    )
  }, character(1))
  "train" %in% names(input_candidates) &&
    !anyNA(input_candidates$train) &&
    all(input_candidates$train == train_collector$train) &&
    identical(ids, refractory_output$output_candidate_id) &&
    identical(starts, refractory_output$detector_start_row) &&
    identical(ends, refractory_output$detector_end_row) &&
    identical(hashes, refractory_output$output_payload_sha256)
}

stpd_candidate_lineage_pause_boundary_entry <- function(context) {
  n <- nrow(context$input_candidates)
  boundary_n <- nrow(context$boundaries)
  applicability <- if (!n) {
    "not_applicable_empty_observed_refractory_output"
  } else if (!boundary_n) {
    "applied_no_active_event_boundaries"
  } else {
    "applied"
  }
  data.frame(
    train = context$train, burst_pipeline_id = "final",
    applicability_status = applicability,
    input_candidate_n = as.integer(n),
    raw_pause_candidate_n = as.integer(nrow(context$raw_pause_candidates)),
    canonical_boundary_n = as.integer(boundary_n),
    evidence_origin = "live_input_output_with_deterministic_branch_replay",
    input_scope = "observed_post_refractory_union_descendants",
    upstream_burst_status = "refractory_observation_closed",
    upstream_pause_status = if (isTRUE(
      context$contextual_pause_observation_closed
    )) "post_ownership_contextual_pause_observation_closed" else
      "post_ownership_contextual_pause_observation_unclosed",
    upstream_universe_status = "candidate_universe_unavailable",
    input_payload_sha256 = stpd_threshold_first_hash_domain(
      "stpd-canonical-pause-boundary-input-v1", context$input_candidates
    ),
    raw_pause_payload_sha256 = stpd_threshold_first_hash_domain(
      "stpd-canonical-pause-raw-output-v1", context$raw_pause_candidates
    ),
    boundary_payload_sha256 = stpd_threshold_first_hash_domain(
      "stpd-canonical-pause-hard-boundaries-v1", context$boundaries
    ),
    stringsAsFactors = FALSE, check.names = FALSE
  )
}

stpd_candidate_lineage_pause_boundary_replay <- function(context) {
  if (is.null(context) || !is.list(context) ||
      !is.character(context$train) || length(context$train) != 1L ||
      is.na(context$train) || !nzchar(context$train) ||
      !is.data.frame(context$input_candidates) ||
      !is.data.frame(context$scientific_out) ||
      !is.data.frame(context$raw_pause_candidates) ||
      !is.data.frame(context$boundaries)) {
    return(NULL)
  }
  if (nrow(context$raw_pause_candidates) &&
      (!("train" %in% names(context$raw_pause_candidates)) ||
       anyNA(context$raw_pause_candidates$train) ||
       !all(context$raw_pause_candidates$train == context$train))) {
    return(NULL)
  }
  expected_boundaries <- stpd_event_core_final_event_boundaries(
    context$raw_pause_candidates
  )
  expected_out <- stpd_event_core_apply_hard_boundaries_to_burst_candidates(
    context$input_candidates, expected_boundaries
  )
  if (!identical(expected_boundaries, context$boundaries) ||
      !identical(expected_out, context$scientific_out)) {
    return(NULL)
  }
  entry <- stpd_candidate_lineage_pause_boundary_entry(context)
  sources <- stpd_candidate_lineage_pause_boundary_source_payload(context)
  if (is.null(sources)) return(NULL)
  n <- nrow(context$input_candidates)
  if (!n) {
    attempts <- stpd_candidate_lineage_empty_hook_payload(
      "burst_pause_boundary_attempts_v1"
    )
    output <- stpd_candidate_lineage_empty_hook_payload(
      "burst_pause_boundary_output_v1"
    )
  } else {
    attempts <- vector("list", n)
    outputs <- vector("list", n)
    any_changed <- !identical(
      context$input_candidates, context$scientific_out
    )
    for (i in seq_len(n)) {
      source <- context$input_candidates[i, , drop = FALSE]
      target <- context$scientific_out[i, , drop = FALSE]
      start_row <- stpd_candidate_lineage_candidate_column(
        source, "start_isi", "integer"
      )[[1L]]
      end_row <- stpd_candidate_lineage_candidate_column(
        source, "end_isi", "integer"
      )[[1L]]
      geometry <- is.finite(start_row) && is.finite(end_row)
      overlap <- if (geometry && nrow(context$boundaries)) {
        which(
          as.integer(context$boundaries$start_isi) <= end_row &
            as.integer(context$boundaries$end_isi) >= start_row
        )
      } else integer()
      blocked <- length(overlap) > 0L
      overlap_payload <- if (length(overlap)) {
        sources[overlap, , drop = FALSE]
      } else {
        stpd_candidate_lineage_empty_hook_payload(
          "burst_pause_boundary_sources_v1"
        )
      }
      overlap_semantics <- if (
        length(overlap) && "boundary_kind" %in% names(sources)
      ) {
        as.character(sources$boundary_kind[overlap])
      } else {
        character()
      }
      canonical_pause_only <- length(overlap_semantics) > 0L &&
        all(overlap_semantics == "canonical_pause", na.rm = TRUE)
      source_id <- stpd_candidate_lineage_candidate_column(
        source, "candidate_id", "character"
      )[[1L]]
      output_id <- stpd_candidate_lineage_candidate_column(
        target, "candidate_id", "character"
      )[[1L]]
      source_hash <- stpd_candidate_lineage_refractory_candidate_hash(
        source, "refractory_policy_output"
      )
      output_hash <- stpd_candidate_lineage_pause_boundary_candidate_hash(
        target, "output"
      )
      attempts[[i]] <- data.frame(
        train = context$train, attempt_ordinal = as.integer(i),
        source_input_ordinal = as.integer(i),
        source_candidate_id = source_id,
        source_payload_sha256 = source_hash,
        geometry_available = geometry,
        detector_start_row = start_row, detector_end_row = end_row,
        overlapping_boundary_n = as.integer(length(overlap)),
        overlapping_boundary_ordinals = paste(overlap, collapse = ";"),
        overlapping_boundary_sha256 = stpd_threshold_first_hash_domain(
          "stpd-canonical-pause-overlap-set-v1", overlap_payload
        ),
        blocked = blocked,
        terminal_status = if (blocked) {
          "rejected_without_redetection"
        } else "retained_without_redetection",
        terminal_reason = if (!geometry) {
          "invalid_geometry_not_blocked_by_scientific_policy"
        } else if (blocked && canonical_pause_only) {
          "canonical_pause_overlap"
        } else if (blocked) {
          "active_event_boundary_overlap"
        } else {
          "no_active_event_boundary_overlap"
        },
        redetection_performed = FALSE,
        output_ordinal = as.integer(i), output_candidate_id = output_id,
        output_payload_sha256 = output_hash,
        stringsAsFactors = FALSE, check.names = FALSE
      )
      relation <- if (blocked && canonical_pause_only) {
        "rejected_by_canonical_pause_boundary"
      } else if (blocked) {
        "rejected_by_active_event_boundary"
      } else if (any_changed && !identical(source, target)) {
        "metadata_updated"
      } else {
        "retained"
      }
      outputs[[i]] <- data.frame(
        train = context$train, output_ordinal = as.integer(i),
        source_input_ordinal = as.integer(i), relation = relation,
        source_candidate_id = source_id,
        source_payload_sha256 = source_hash,
        output_candidate_id = output_id,
        output_payload_sha256 = output_hash,
        detector_start_row = stpd_candidate_lineage_candidate_column(
          target, "start_isi", "integer"
        )[[1L]],
        detector_end_row = stpd_candidate_lineage_candidate_column(
          target, "end_isi", "integer"
        )[[1L]],
        start_isi = as.integer(stpd_candidate_lineage_candidate_column(
          target, "start_isi", "integer"
        )[[1L]] - 1L),
        end_isi = as.integer(stpd_candidate_lineage_candidate_column(
          target, "end_isi", "integer"
        )[[1L]] - 1L),
        final_label = stpd_candidate_lineage_candidate_column(
          target, "final_label", "character"
        )[[1L]],
        action = stpd_candidate_lineage_candidate_column(
          target, "action", "character"
        )[[1L]],
        priority = stpd_candidate_lineage_candidate_column(
          target, "priority", "double"
        )[[1L]],
        score = stpd_candidate_lineage_candidate_column(
          target, "score", "double"
        )[[1L]],
        hard_boundary_conflict = stpd_candidate_lineage_candidate_column(
          target, "hard_boundary_conflict", "logical"
        )[[1L]],
        hard_boundary_conflict_kind = stpd_candidate_lineage_candidate_column(
          target, "hard_boundary_conflict_kind", "character"
        )[[1L]],
        hard_boundary_decision = stpd_candidate_lineage_candidate_column(
          target, "hard_boundary_decision", "character"
        )[[1L]],
        stringsAsFactors = FALSE, check.names = FALSE
      )
    }
    attempts <- dplyr::bind_rows(attempts)
    output <- dplyr::bind_rows(outputs)
    attempts <- attempts[, names(
      stpd_candidate_lineage_observation_hook_schema(
        "burst_pause_boundary_attempts_v1"
      )
    ), drop = FALSE]
    output <- output[, names(stpd_candidate_lineage_observation_hook_schema(
      "burst_pause_boundary_output_v1"
    )), drop = FALSE]
  }
  blocked_n <- if (nrow(attempts)) sum(attempts$blocked) else 0L
  invalid_n <- if (nrow(attempts)) sum(!attempts$geometry_available) else 0L
  applicability <- if (!n) {
    "not_applicable_empty_observed_refractory_output"
  } else if (!nrow(context$boundaries)) {
    "applied_no_active_event_boundaries"
  } else if (blocked_n) {
    "applied_with_conflicts"
  } else {
    "applied_without_conflicts"
  }
  receipt <- data.frame(
    train = context$train, burst_pipeline_id = "final",
    applicability_status = applicability,
    input_candidate_n = as.integer(n),
    canonical_boundary_n = as.integer(nrow(context$boundaries)),
    candidate_attempt_n = as.integer(nrow(attempts)),
    output_candidate_n = as.integer(nrow(output)),
    blocked_candidate_n = as.integer(blocked_n),
    retained_candidate_n = as.integer(nrow(output) - blocked_n),
    invalid_geometry_n = as.integer(invalid_n), redetection_n = 0L,
    replay_exhausted = TRUE,
    evidence_origin = "live_input_output_with_deterministic_branch_replay",
    input_scope = "observed_post_refractory_union_descendants",
    upstream_burst_status = "refractory_observation_closed",
    upstream_pause_status = if (isTRUE(
      context$contextual_pause_observation_closed
    )) "post_ownership_contextual_pause_observation_closed" else
      "post_ownership_contextual_pause_observation_unclosed",
    upstream_universe_status = "candidate_universe_unavailable",
    coverage_status = if (!n) {
      paste0(
        "hard_boundary_not_applicable_empty_",
        "observed_refractory_output"
      )
    } else {
      "hard_boundary_decisions_complete_over_observed_refractory_output"
    },
    entry_payload_sha256 = stpd_threshold_first_hash_domain(
      "stpd-canonical-pause-boundary-entry-v1", entry
    ),
    source_payload_sha256 = stpd_threshold_first_hash_domain(
      "stpd-canonical-pause-boundary-sources-v1", sources
    ),
    attempt_payload_sha256 = stpd_threshold_first_hash_domain(
      "stpd-canonical-pause-boundary-attempts-v1", attempts
    ),
    output_payload_sha256 = stpd_threshold_first_hash_domain(
      "stpd-canonical-pause-boundary-output-v1", output
    ),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  list(
    entry = entry, sources = sources, attempts = attempts,
    output = output, receipt = receipt
  )
}

stpd_candidate_lineage_capture_burst_pause_boundary <- function(
    train_collector, train, input_candidates, scientific_out,
    raw_pause_candidates, boundaries) {
  if (is.null(train_collector)) return(invisible(NULL))
  train <- as.character(train)
  if (length(train) != 1L || is.na(train) || !nzchar(train)) {
    stpd_candidate_lineage_abort(
      "collector_train_scope_invalid",
      "Pause-boundary observation requires one non-empty active train.",
      field = "train"
    )
  }
  stpd_candidate_lineage_validate_train_collector(
    train_collector, require_state = "open", train = train,
    validate_observations = FALSE
  )
  input_candidates <- as.data.frame(
    input_candidates %||% data.frame(), stringsAsFactors = FALSE,
    check.names = FALSE
  )
  raw_pause_candidates <- as.data.frame(
    raw_pause_candidates %||% data.frame(), stringsAsFactors = FALSE,
    check.names = FALSE
  )
  boundaries <- as.data.frame(
    boundaries %||% data.frame(), stringsAsFactors = FALSE,
    check.names = FALSE
  )
  if (!stpd_candidate_lineage_pause_boundary_upstream_is_closed(
      train_collector, input_candidates,
      raw_pause_candidates, boundaries)) {
    stpd_candidate_lineage_abort(
      "collector_pause_boundary_upstream_not_closed",
      paste(
        "Pause-boundary input does not close to the exact observed",
        "post-refractory Burst output and post-ownership contextual Pause."
      )
    )
  }
  context <- list(
    train = train, input_candidates = input_candidates,
    scientific_out = as.data.frame(
      scientific_out %||% data.frame(), stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    raw_pause_candidates = raw_pause_candidates,
    boundaries = boundaries,
    contextual_pause_observation_closed = TRUE
  )
  replay <- stpd_candidate_lineage_pause_boundary_replay(context)
  if (is.null(replay)) {
    stpd_candidate_lineage_abort(
      "collector_pause_boundary_scientific_replay_failed",
      "Canonical Pause hard-boundary output does not replay exactly."
    )
  }
  train_collector$burst_pause_boundary_context <- context
  stpd_candidate_lineage_collector_capture(
    train_collector, "burst_pause_boundary_entry_v1", replay$entry,
    validate_payload = FALSE
  )
  stpd_candidate_lineage_collector_capture(
    train_collector, "burst_pause_boundary_sources_v1", replay$sources,
    validate_payload = FALSE
  )
  stpd_candidate_lineage_collector_capture(
    train_collector, "burst_pause_boundary_attempts_v1", replay$attempts,
    validate_payload = FALSE
  )
  stpd_candidate_lineage_collector_capture(
    train_collector, "burst_pause_boundary_output_v1", replay$output,
    validate_payload = FALSE
  )
  stpd_candidate_lineage_collector_capture(
    train_collector, "burst_pause_boundary_receipt_v1", replay$receipt,
    validate_payload = FALSE
  )
  invisible(replay)
}

stpd_candidate_lineage_pause_boundary_payload_is_valid <- function(
    train_collector, hook_id, payload) {
  context <- if (exists(
      "burst_pause_boundary_context", envir = train_collector,
      inherits = FALSE)) {
    train_collector$burst_pause_boundary_context
  } else NULL
  if (is.null(context) || !is.list(context) ||
      !is.character(context$train) || length(context$train) != 1L ||
      is.na(context$train) || !nzchar(context$train) ||
      !identical(context$train, train_collector$train) ||
      !("train" %in% names(payload)) || anyNA(payload$train) ||
      !all(payload$train == train_collector$train) ||
      !stpd_candidate_lineage_pause_boundary_upstream_is_closed(
        train_collector, context$input_candidates,
        context$raw_pause_candidates, context$boundaries
      )) {
    return(FALSE)
  }
  replay <- stpd_candidate_lineage_pause_boundary_replay(context)
  if (is.null(replay)) return(FALSE)
  hook_map <- c(
    burst_pause_boundary_entry_v1 = "entry",
    burst_pause_boundary_sources_v1 = "sources",
    burst_pause_boundary_attempts_v1 = "attempts",
    burst_pause_boundary_output_v1 = "output",
    burst_pause_boundary_receipt_v1 = "receipt"
  )
  key <- unname(hook_map[[hook_id]])
  if (is.null(key) || !identical(payload, replay[[key]])) return(FALSE)
  prior <- switch(
    hook_id,
    burst_pause_boundary_entry_v1 = character(),
    burst_pause_boundary_sources_v1 = "burst_pause_boundary_entry_v1",
    burst_pause_boundary_attempts_v1 = c(
      "burst_pause_boundary_entry_v1",
      "burst_pause_boundary_sources_v1"
    ),
    burst_pause_boundary_output_v1 = c(
      "burst_pause_boundary_entry_v1",
      "burst_pause_boundary_sources_v1",
      "burst_pause_boundary_attempts_v1"
    ),
    burst_pause_boundary_receipt_v1 = names(hook_map)[1:4],
    character()
  )
  if (!length(prior)) return(TRUE)
  all(vapply(prior, function(prior_hook) {
    prior_key <- unname(hook_map[[prior_hook]])
    stored <- train_collector$observations[[prior_hook]]
    !is.null(stored) && identical(stored, replay[[prior_key]])
  }, logical(1)))
}
