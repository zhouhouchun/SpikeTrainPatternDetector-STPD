# Threshold-first Gate 1B Burst refractory observation -------------------
#
# This module binds the exact live input/output of the first downstream Burst
# transformation after the two-route union, then deterministically replays its
# internal branches for audit.  It is not a live branch trace.  The resulting
# sidecar never participates in scientific selection or public products.

stpd_candidate_lineage_refractory_action <- function(params) {
  action <- as.character(
    (params$detector %||% list())$refractory_suspect_action %||%
      (params$burst %||% list())$refractory_suspect_action %||%
      "demote_to_possible"
  )[1L]
  action <- tolower(trimws(action))
  action <- gsub("-", "_", action, fixed = TRUE)
  if (action %in% c(
      "demote", "demote_burst", "demote_to_possible_burst", "review")) {
    action <- "demote_to_possible"
  }
  if (action %in% c("split", "split_candidate", "split_at_refractory")) {
    action <- "split_at_suspect"
  }
  if (action %in% c(
      "exclude_suspect", "exclude_suspect_isi", "reevaluate_fragments")) {
    action <- "exclude_suspect_isi_and_reevaluate"
  }
  if (action %in% c("exclude", "reject", "drop", "reject_candidate")) {
    action <- "exclude_candidate"
  }
  if (action %in% c("multiunit", "mark_multiunit")) {
    action <- "mark_multiunit_contamination"
  }
  valid <- c(
    "warn_only", "demote_to_possible", "split_at_suspect",
    "exclude_suspect_isi_and_reevaluate", "exclude_candidate",
    "mark_multiunit_contamination"
  )
  if (!(action %in% valid)) "warn_only" else action
}

stpd_candidate_lineage_refractory_candidate_hash <- function(row, route) {
  stpd_candidate_lineage_candidate_source_hash(
    as.data.frame(row, stringsAsFactors = FALSE, check.names = FALSE), route
  )
}

stpd_candidate_lineage_refractory_union_closure <- function(
    input_candidates, union_post, train = NULL) {
  input_candidates <- as.data.frame(
    input_candidates %||% data.frame(), stringsAsFactors = FALSE,
    check.names = FALSE
  )
  if (is.null(union_post) || !is.data.frame(union_post) ||
      nrow(input_candidates) != nrow(union_post)) {
    return(NULL)
  }
  if (!is.null(train)) {
    train <- as.character(train)
    if (length(train) != 1L || is.na(train) || !nzchar(train)) return(NULL)
  }
  if (!nrow(input_candidates)) return(character())
  if (!is.null(train) &&
      (!("train" %in% names(input_candidates)) ||
       !("train" %in% names(union_post)) ||
       anyNA(input_candidates$train) || anyNA(union_post$train) ||
       !all(as.character(input_candidates$train) == train) ||
       !all(as.character(union_post$train) == train))) {
    return(NULL)
  }
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
      input_candidates[i, , drop = FALSE], union_post$source_route[[i]]
    )
  }, character(1))
  closed <- identical(ids, union_post$source_candidate_id) &&
    identical(starts, union_post$detector_start_row) &&
    identical(ends, union_post$detector_end_row) &&
    identical(hashes, union_post$source_payload_sha256)
  if (!closed) NULL else hashes
}

stpd_candidate_lineage_refractory_classification_changed <- function(
    source, target) {
  fields <- intersect(c("final_label", "class"),
                      union(names(source), names(target)))
  if (!length(fields)) return(FALSE)
  any(vapply(fields, function(field) {
    source_value <- if (field %in% names(source)) {
      as.character(source[[field]])[[1L]]
    } else NA_character_
    target_value <- if (field %in% names(target)) {
      as.character(target[[field]])[[1L]]
    } else NA_character_
    !identical(source_value, target_value)
  }, logical(1)))
}

stpd_candidate_lineage_refractory_entry_from_context <- function(context) {
  input <- context$input_candidates
  n <- nrow(input)
  has_field <- "refractory_suspect_n" %in% names(input)
  suspect <- if (has_field && n) {
    suspect_n <- suppressWarnings(as.numeric(input$refractory_suspect_n))
    applied <- if ("refractory_suspect_policy_applied" %in% names(input)) {
      as.logical(input$refractory_suspect_policy_applied)
    } else rep(FALSE, n)
    applied[is.na(applied)] <- FALSE
    is.finite(suspect_n) & suspect_n > 0 & !applied
  } else logical(n)
  applicability <- if (!n) {
    "not_applicable_empty_input"
  } else if (!has_field) {
    "not_applicable_no_suspect_field"
  } else if (!any(suspect)) {
    "applied_no_suspects"
  } else {
    "applied"
  }
  min_isi <- suppressWarnings(as.numeric(context$min_isi_sec))[1L]
  threshold <- stpd_event_core_refractory_suspect_threshold(
    context$params, min_isi
  )
  min_spikes <- max(
    3L,
    stpd_event_core_int((context$vp %||% list())$min_spikes %||% 3L, 3L)
  )
  data.frame(
    train = context$train, burst_pipeline_id = "final",
    applicability_status = applicability,
    input_candidate_n = as.integer(n),
    normalized_policy_action = stpd_candidate_lineage_refractory_action(
      context$params
    ),
    min_isi_sec = min_isi,
    refractory_threshold_sec = as.numeric(threshold),
    min_spikes = as.integer(min_spikes),
    context_available = is.data.frame(context$dat) &&
      is.list(context$params) && is.list(context$vp),
    input_payload_sha256 = stpd_threshold_first_hash_domain(
      "stpd-burst-refractory-live-input-v1", input
    ),
    stringsAsFactors = FALSE, check.names = FALSE
  )
}

stpd_candidate_lineage_refractory_output_row <- function(
    context, output_ordinal, source_ordinal, relation) {
  input <- context$input_candidates
  output <- context$scientific_out
  source <- input[source_ordinal, , drop = FALSE]
  target <- output[output_ordinal, , drop = FALSE]
  start_row <- stpd_candidate_lineage_candidate_column(
    target, "start_isi", "integer"
  )[[1L]]
  end_row <- stpd_candidate_lineage_candidate_column(
    target, "end_isi", "integer"
  )[[1L]]
  data.frame(
    train = context$train, output_ordinal = as.integer(output_ordinal),
    source_input_ordinal = as.integer(source_ordinal),
    relation = as.character(relation)[1L],
    source_candidate_id = stpd_candidate_lineage_candidate_column(
      source, "candidate_id", "character"
    )[[1L]],
    source_payload_sha256 = context$source_hashes[[source_ordinal]],
    output_candidate_id = stpd_candidate_lineage_candidate_column(
      target, "candidate_id", "character"
    )[[1L]],
    output_payload_sha256 =
      stpd_candidate_lineage_refractory_candidate_hash(
        target, "refractory_policy_output"
      ),
    detector_start_row = start_row, detector_end_row = end_row,
    start_isi = as.integer(start_row - 1L),
    end_isi = as.integer(end_row - 1L),
    final_label = stpd_candidate_lineage_candidate_column(
      target, "final_label", "character"
    )[[1L]],
    gate_status = stpd_candidate_lineage_candidate_column(
      target, "gate_status", "character"
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
    stringsAsFactors = FALSE, check.names = FALSE
  )
}

stpd_candidate_lineage_refractory_attempt_row <- function(
    context, attempt_ordinal, source_ordinal, attempt_kind,
    fragment_ordinal = 0L, start_row = NA_integer_, end_row = NA_integer_,
    suspect_candidate = FALSE, terminal_status, terminal_reason,
    output_ordinal = NA_integer_) {
  input <- context$input_candidates
  source <- input[source_ordinal, , drop = FALSE]
  geometry <- is.finite(start_row) && is.finite(end_row) &&
    start_row >= 2L && end_row >= start_row
  output_id <- ""
  output_hash <- ""
  if (is.finite(output_ordinal) && output_ordinal >= 1L &&
      output_ordinal <= nrow(context$scientific_out)) {
    target <- context$scientific_out[output_ordinal, , drop = FALSE]
    output_id <- stpd_candidate_lineage_candidate_column(
      target, "candidate_id", "character"
    )[[1L]]
    output_hash <- stpd_candidate_lineage_refractory_candidate_hash(
      target, "refractory_policy_output"
    )
  }
  data.frame(
    train = context$train, attempt_ordinal = as.integer(attempt_ordinal),
    source_input_ordinal = as.integer(source_ordinal),
    source_candidate_id = stpd_candidate_lineage_candidate_column(
      source, "candidate_id", "character"
    )[[1L]],
    source_payload_sha256 = context$source_hashes[[source_ordinal]],
    attempt_kind = as.character(attempt_kind)[1L],
    fragment_ordinal = as.integer(fragment_ordinal),
    proposed_detector_start_row = if (geometry) as.integer(start_row) else
      NA_integer_,
    proposed_detector_end_row = if (geometry) as.integer(end_row) else
      NA_integer_,
    proposed_start_isi = if (geometry) as.integer(start_row - 1L) else
      NA_integer_,
    proposed_end_isi = if (geometry) as.integer(end_row - 1L) else
      NA_integer_,
    geometry_available = geometry,
    suspect_candidate = as.logical(suspect_candidate)[1L],
    terminal_status = as.character(terminal_status)[1L],
    terminal_reason = as.character(terminal_reason)[1L],
    output_ordinal = if (is.finite(output_ordinal)) {
      as.integer(output_ordinal)
    } else NA_integer_,
    output_candidate_id = output_id,
    output_payload_sha256 = output_hash,
    stringsAsFactors = FALSE, check.names = FALSE
  )
}

stpd_candidate_lineage_refractory_replay <- function(context) {
  if (is.null(context) || !is.list(context) ||
      !is.data.frame(context$input_candidates) ||
      !is.data.frame(context$scientific_out) ||
      !is.character(context$source_hashes) ||
      length(context$source_hashes) != nrow(context$input_candidates)) {
    return(NULL)
  }
  input <- context$input_candidates
  scientific_out <- context$scientific_out
  expected_scientific <- stpd_event_core_apply_refractory_suspect_policy(
    input, context$params, dat = context$dat, vp = context$vp,
    min_isi_sec = context$min_isi_sec
  )
  if (!identical(expected_scientific, scientific_out)) return(NULL)

  entry <- stpd_candidate_lineage_refractory_entry_from_context(context)
  n <- nrow(input)
  if (!n) {
    attempts <- stpd_candidate_lineage_empty_hook_payload(
      "burst_refractory_attempts_v1"
    )
    output <- stpd_candidate_lineage_empty_hook_payload(
      "burst_refractory_output_v1"
    )
    receipt <- data.frame(
      train = context$train, burst_pipeline_id = "final",
      applicability_status = entry$applicability_status,
      normalized_policy_action = entry$normalized_policy_action,
      input_candidate_n = 0L, suspect_candidate_n = 0L,
      candidate_attempt_n = 0L, fragment_attempt_n = 0L,
      fragment_discarded_n = 0L, output_candidate_n = 0L,
      retained_output_n = 0L, retyped_output_n = 0L,
      rejected_parent_n = 0L, fragment_output_n = 0L,
      replay_exhausted = TRUE,
      evidence_origin =
        "deterministic_replay_from_live_post_union_input_output",
      input_scope = "observed_post_union_candidates",
      upstream_universe_status = "candidate_universe_unavailable",
      coverage_status = paste0(
        "refractory_not_applicable_empty_",
        "observed_post_union_inputs"
      ),
      input_payload_sha256 = entry$input_payload_sha256,
      attempt_payload_sha256 = stpd_threshold_first_hash_domain(
        "stpd-burst-refractory-attempts-v1", attempts
      ),
      output_payload_sha256 = stpd_threshold_first_hash_domain(
        "stpd-burst-refractory-output-v1", output
      ),
      stringsAsFactors = FALSE, check.names = FALSE
    )
    return(list(
      entry = entry, attempts = attempts, output = output, receipt = receipt
    ))
  }

  has_field <- "refractory_suspect_n" %in% names(input)
  suspect_n <- if (has_field) {
    suppressWarnings(as.numeric(input$refractory_suspect_n))
  } else rep(0, n)
  already <- if ("refractory_suspect_policy_applied" %in% names(input)) {
    as.logical(input$refractory_suspect_policy_applied)
  } else rep(FALSE, n)
  already[is.na(already)] <- FALSE
  suspect <- has_field & is.finite(suspect_n) & suspect_n > 0 & !already
  action <- entry$normalized_policy_action[[1L]]
  split_action <- action %in% c(
    "split_at_suspect", "exclude_suspect_isi_and_reevaluate"
  )
  split_context <- split_action && is.data.frame(context$dat)
  attempts <- list()
  outputs <- list()
  attempt_ordinal <- 0L
  output_ordinal <- 0L

  add_output <- function(source_ordinal, relation) {
    output_ordinal <<- output_ordinal + 1L
    outputs[[length(outputs) + 1L]] <<-
      stpd_candidate_lineage_refractory_output_row(
        context, output_ordinal, source_ordinal, relation
      )
    output_ordinal
  }
  add_attempt <- function(source_ordinal, kind, fragment = 0L,
                          start = NA_integer_, end = NA_integer_,
                          terminal_status, terminal_reason,
                          output = NA_integer_) {
    attempt_ordinal <<- attempt_ordinal + 1L
    attempts[[length(attempts) + 1L]] <<-
      stpd_candidate_lineage_refractory_attempt_row(
        context, attempt_ordinal, source_ordinal, kind, fragment,
        start, end, suspect[[source_ordinal]], terminal_status,
        terminal_reason, output
      )
  }

  if (!has_field) {
    for (i in seq_len(n)) {
      out_i <- add_output(i, "retained")
      s <- stpd_candidate_lineage_candidate_column(
        input[i, , drop = FALSE], "start_isi", "integer"
      )[[1L]]
      e <- stpd_candidate_lineage_candidate_column(
        input[i, , drop = FALSE], "end_isi", "integer"
      )[[1L]]
      add_attempt(
        i, "candidate", start = s, end = e,
        terminal_status = "output_emitted",
        terminal_reason = "not_applicable_no_suspect_field", output = out_i
      )
    }
  } else if (!split_action || !split_context || !any(suspect)) {
    for (i in seq_len(n)) {
      source <- input[i, , drop = FALSE]
      target <- scientific_out[i, , drop = FALSE]
      relation <- if (!isTRUE(suspect[[i]])) {
        "retained"
      } else if (stpd_candidate_lineage_refractory_classification_changed(
          source, target)) {
        "retyped"
      } else {
        "metadata_updated"
      }
      out_i <- add_output(i, relation)
      s <- stpd_candidate_lineage_candidate_column(
        source, "start_isi", "integer"
      )[[1L]]
      e <- stpd_candidate_lineage_candidate_column(
        source, "end_isi", "integer"
      )[[1L]]
      reason <- if (!isTRUE(suspect[[i]])) {
        if (isTRUE(already[[i]])) {
          "not_applicable_policy_already_applied"
        } else {
          "not_applicable_no_refractory_suspect"
        }
      } else if (split_action && !split_context) {
        paste0(action, "_context_unavailable_demoted_to_possible")
      } else {
        observed_action <- stpd_candidate_lineage_candidate_column(
          scientific_out[i, , drop = FALSE],
          "refractory_suspect_action", "character"
        )[[1L]]
        if (nzchar(observed_action)) observed_action else action
      }
      add_attempt(
        i, "candidate", start = s, end = e,
        terminal_status = "output_emitted", terminal_reason = reason,
        output = out_i
      )
    }
  } else {
    min_isi <- entry$min_isi_sec[[1L]]
    threshold <- entry$refractory_threshold_sec[[1L]]
    min_spikes <- entry$min_spikes[[1L]]
    isi <- suppressWarnings(as.numeric(context$dat$ISI_sec))
    for (i in seq_len(n)) {
      source <- input[i, , drop = FALSE]
      s <- stpd_candidate_lineage_candidate_column(
        source, "start_isi", "integer"
      )[[1L]]
      e <- stpd_candidate_lineage_candidate_column(
        source, "end_isi", "integer"
      )[[1L]]
      if (!isTRUE(suspect[[i]])) {
        out_i <- add_output(i, "retained")
        add_attempt(
          i, "candidate", start = s, end = e,
          terminal_status = "output_emitted",
          terminal_reason = if (isTRUE(already[[i]])) {
            "not_applicable_policy_already_applied"
          } else "not_applicable_no_refractory_suspect",
          output = out_i
        )
        next
      }
      parent_output <- add_output(i, "rejected_parent")
      add_attempt(
        i, "candidate", start = s, end = e,
        terminal_status = "output_emitted",
        terminal_reason = paste0(action, "_parent_rejected"),
        output = parent_output
      )
      if (!is.finite(s) || !is.finite(e) || s > e || s < 2L ||
          e > nrow(context$dat)) {
        add_attempt(
          i, "fragment", terminal_status = "fragment_discarded",
          terminal_reason = "invalid_parent_geometry"
        )
        next
      }
      idx <- seq.int(s, e)
      split_mask <- is_refractory_suspect_isi(
        isi[idx], min_isi_sec = min_isi,
        refractory_suspect_sec = threshold
      )
      keep_idx <- idx[!split_mask]
      if (!length(keep_idx)) {
        add_attempt(
          i, "fragment", terminal_status = "fragment_discarded",
          terminal_reason = "no_non_suspect_support"
        )
        next
      }
      groups <- split(
        keep_idx, cumsum(c(TRUE, diff(keep_idx) != 1L))
      )
      fragment_ordinal <- 0L
      for (group in groups) {
        fragment_ordinal <- fragment_ordinal + 1L
        ns <- min(group)
        ne <- max(group)
        if (length(group) + 1L < min_spikes) {
          add_attempt(
            i, "fragment", fragment_ordinal, ns, ne,
            terminal_status = "fragment_discarded",
            terminal_reason = "undersized_fragment"
          )
          next
        }
        metrics <- stpd_event_core_span_metrics(
          context$dat, ns, ne, context$params, context$vp,
          min_isi_sec = min_isi,
          train = stpd_candidate_lineage_candidate_column(
            source, "train", "character"
          )[[1L]],
          label = "refractory_split_fragment"
        )
        if (is.null(metrics) || !nrow(metrics)) {
          add_attempt(
            i, "fragment", fragment_ordinal, ns, ne,
            terminal_status = "fragment_discarded",
            terminal_reason = "fragment_metrics_unavailable"
          )
          next
        }
        child_output <- add_output(i, "fragment_of_rejected_parent")
        add_attempt(
          i, "fragment", fragment_ordinal, ns, ne,
          terminal_status = "fragment_emitted",
          terminal_reason = "fragment_recomputed_after_excluding_suspect_isi",
          output = child_output
        )
      }
    }
  }

  if (output_ordinal != nrow(scientific_out)) return(NULL)
  attempts <- dplyr::bind_rows(attempts)
  output <- dplyr::bind_rows(outputs)
  attempts <- attempts[, names(stpd_candidate_lineage_observation_hook_schema(
    "burst_refractory_attempts_v1"
  )), drop = FALSE]
  output <- output[, names(stpd_candidate_lineage_observation_hook_schema(
    "burst_refractory_output_v1"
  )), drop = FALSE]
  relations <- output$relation
  receipt <- data.frame(
    train = context$train, burst_pipeline_id = "final",
    applicability_status = entry$applicability_status,
    normalized_policy_action = action,
    input_candidate_n = as.integer(n),
    suspect_candidate_n = as.integer(sum(suspect)),
    candidate_attempt_n = as.integer(sum(
      attempts$attempt_kind == "candidate"
    )),
    fragment_attempt_n = as.integer(sum(
      attempts$attempt_kind == "fragment"
    )),
    fragment_discarded_n = as.integer(sum(
      attempts$terminal_status == "fragment_discarded"
    )),
    output_candidate_n = as.integer(nrow(output)),
    retained_output_n = as.integer(sum(
      relations %in% c("retained", "metadata_updated")
    )),
    retyped_output_n = as.integer(sum(relations == "retyped")),
    rejected_parent_n = as.integer(sum(relations == "rejected_parent")),
    fragment_output_n = as.integer(sum(
      relations == "fragment_of_rejected_parent"
    )),
    replay_exhausted = TRUE,
    evidence_origin =
      "deterministic_replay_from_live_post_union_input_output",
    input_scope = "observed_post_union_candidates",
    upstream_universe_status = "candidate_universe_unavailable",
    coverage_status = if (!has_field) {
      paste0(
        "refractory_not_applicable_no_suspect_field_",
        "over_observed_post_union_inputs"
      )
    } else {
      "refractory_replay_complete_over_observed_post_union_inputs"
    },
    input_payload_sha256 = entry$input_payload_sha256,
    attempt_payload_sha256 = stpd_threshold_first_hash_domain(
      "stpd-burst-refractory-attempts-v1", attempts
    ),
    output_payload_sha256 = stpd_threshold_first_hash_domain(
      "stpd-burst-refractory-output-v1", output
    ),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  list(entry = entry, attempts = attempts, output = output, receipt = receipt)
}

stpd_candidate_lineage_capture_burst_refractory <- function(
    train_collector, train, input_candidates, scientific_out,
    dat, params, vp, min_isi_sec) {
  if (is.null(train_collector)) return(invisible(NULL))
  train <- as.character(train)
  if (length(train) != 1L || is.na(train) || !nzchar(train)) {
    stpd_candidate_lineage_abort(
      "collector_train_scope_invalid",
      "Refractory observation requires one non-empty active train.",
      field = "train"
    )
  }
  stpd_candidate_lineage_validate_train_collector(
    train_collector, require_state = "open", train = train,
    validate_observations = FALSE
  )
  union_post <- train_collector$observations$burst_union_post_cap_v1
  source_hashes <- stpd_candidate_lineage_refractory_union_closure(
    input_candidates, union_post, train = train
  )
  if (is.null(source_hashes)) {
    stpd_candidate_lineage_abort(
      "collector_refractory_union_not_closed",
      "Refractory input does not close to the observed Burst union output."
    )
  }
  context <- list(
    train = train,
    input_candidates = as.data.frame(
      input_candidates %||% data.frame(), stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    scientific_out = as.data.frame(
      scientific_out %||% data.frame(), stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    source_hashes = source_hashes,
    dat = dat, params = params, vp = vp,
    min_isi_sec = suppressWarnings(as.numeric(min_isi_sec))[1L]
  )
  replay <- stpd_candidate_lineage_refractory_replay(context)
  if (is.null(replay)) {
    stpd_candidate_lineage_abort(
      "collector_refractory_scientific_replay_failed",
      "Refractory scientific output does not replay from its live union input."
    )
  }
  train_collector$burst_refractory_context <- context
  stpd_candidate_lineage_collector_capture(
    train_collector, "burst_refractory_entry_v1", replay$entry,
    validate_payload = FALSE
  )
  stpd_candidate_lineage_collector_capture(
    train_collector, "burst_refractory_attempts_v1", replay$attempts,
    validate_payload = FALSE
  )
  stpd_candidate_lineage_collector_capture(
    train_collector, "burst_refractory_output_v1", replay$output,
    validate_payload = FALSE
  )
  stpd_candidate_lineage_collector_capture(
    train_collector, "burst_refractory_receipt_v1", replay$receipt,
    validate_payload = FALSE
  )
  invisible(replay)
}

stpd_candidate_lineage_refractory_payload_is_valid <- function(
    train_collector, hook_id, payload) {
  context <- if (exists(
      "burst_refractory_context", envir = train_collector,
      inherits = FALSE)) {
    train_collector$burst_refractory_context
  } else NULL
  if (is.null(context) || !is.list(context) ||
      !is.character(context$train) || length(context$train) != 1L ||
      is.na(context$train) || !nzchar(context$train) ||
      !identical(context$train, train_collector$train) ||
      !("train" %in% names(payload)) || anyNA(payload$train) ||
      !all(payload$train == train_collector$train)) {
    return(FALSE)
  }
  union_post <- train_collector$observations$burst_union_post_cap_v1
  union_receipt <- train_collector$observations$burst_union_rank_receipt_v1
  closed_hashes <- stpd_candidate_lineage_refractory_union_closure(
    context$input_candidates, union_post, train = train_collector$train
  )
  union_valid <- !is.null(closed_hashes) &&
    identical(closed_hashes, context$source_hashes) &&
    is.data.frame(union_receipt) && nrow(union_receipt) == 1L &&
    identical(union_receipt$train[[1L]], train_collector$train) &&
    identical(union_receipt$burst_pipeline_id[[1L]], "final") &&
    identical(union_receipt$retained_n[[1L]],
              as.integer(nrow(union_post))) &&
    isTRUE(union_receipt$ranking_exhausted[[1L]]) &&
    identical(union_receipt$rank_scope_status[[1L]],
              "complete_over_observed_inputs") &&
    identical(union_receipt$overall_universe_status[[1L]],
              "candidate_universe_unavailable") &&
    identical(
      union_receipt$post_cap_payload_sha256[[1L]],
      stpd_threshold_first_hash_domain("stpd-burst-union-post-cap-v1",
                                       union_post)
    )
  if (!union_valid) return(FALSE)
  replay <- stpd_candidate_lineage_refractory_replay(context)
  if (is.null(replay)) return(FALSE)
  expected <- switch(
    hook_id,
    burst_refractory_entry_v1 = replay$entry,
    burst_refractory_attempts_v1 = replay$attempts,
    burst_refractory_output_v1 = replay$output,
    burst_refractory_receipt_v1 = replay$receipt,
    NULL
  )
  !is.null(expected) && identical(payload, expected)
}
