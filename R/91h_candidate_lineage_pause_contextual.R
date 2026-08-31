# Gate 1B contextual/inter-burst Pause observation ------------------------
# Observer-only replay of the post-ownership Pause proposal call in the primary
# hf_protected pipeline.  Both begin and capture are bound to the same raw-Pause
# boundary snapshot; the later active Event boundary is not an input here.
# Nothing in this file selects or materializes a Gap.

stpd_candidate_lineage_contextual_pause_hooks <- function() c(
  contextual_pause_entry_v1 = "entry",
  contextual_pause_generic_input_v1 = "generic_input",
  contextual_pause_ownership_support_v1 = "ownership_support",
  contextual_pause_generic_downgrade_attempts_v1 = "downgrade_attempts",
  contextual_pause_generic_post_downgrade_v1 = "generic_post",
  contextual_pause_interburst_pair_attempts_v1 = "pair_attempts",
  contextual_pause_interburst_candidates_v1 = "interburst",
  contextual_pause_combined_output_v1 = "combined",
  contextual_pause_receipt_v1 = "receipt"
)

stpd_candidate_lineage_contextual_pause_hash <- function(x, stage) {
  stpd_threshold_first_hash_domain(paste0("stpd-contextual-pause-", stage, "-v1"), x)
}

stpd_candidate_lineage_contextual_pause_candidate_rows <- function(
    x, train, route, hook_id = NULL) {
  route_one <- as.character(route)[1L]
  hook <- hook_id %||% if (identical(route_one, "generic_input")) {
    "contextual_pause_generic_input_v1"
  } else if (identical(route_one, "generic_post")) {
    "contextual_pause_generic_post_downgrade_v1"
  } else if (identical(route_one, "interburst")) {
    "contextual_pause_interburst_candidates_v1"
  } else {
    "contextual_pause_combined_output_v1"
  }
  schema <- stpd_candidate_lineage_observation_hook_schema(hook)
  if (is.null(x) || !nrow(x)) {
    return(stpd_candidate_lineage_empty_hook_payload(hook))
  }
  source_routes <- rep_len(as.character(route), nrow(x))
  rows <- lapply(seq_len(nrow(x)), function(i) {
    z <- x[i, , drop = FALSE]
    source_route <- source_routes[[i]]
    action <- stpd_candidate_lineage_candidate_column(z, "action", "character")[[1L]]
    decision_action <- stpd_candidate_lineage_candidate_column(
      z, "decision_action", "character")[[1L]]
    pause_decision <- stpd_candidate_lineage_candidate_column(
      z, "pause_candidate_decision", "character")[[1L]]
    rejected <- identical(action, "reject") || identical(decision_action, "reject") ||
      identical(pause_decision, "blocked_by_frozen_burst_support")
    is_input <- identical(source_route, "generic_input")
    data.frame(
      train = train, output_ordinal = as.integer(i), route = source_route,
      candidate_id = stpd_candidate_lineage_candidate_column(z, "candidate_id", "character")[[1L]],
      detector_start_row = stpd_candidate_lineage_candidate_column(z, "start_isi", "integer")[[1L]],
      detector_end_row = stpd_candidate_lineage_candidate_column(z, "end_isi", "integer")[[1L]],
      final_label = stpd_candidate_lineage_candidate_column(z, "final_label", "character")[[1L]],
      action = action,
      gap_semantics = stpd_candidate_lineage_candidate_column(z, "gap_semantics", "character")[[1L]],
      hard_for_event = stpd_candidate_lineage_candidate_column(z, "hard_for_event", "logical")[[1L]],
      hard_for_state_direct_support = stpd_candidate_lineage_candidate_column(
        z, "hard_for_state_direct_support", "logical")[[1L]],
      envelope_bridge_eligible = stpd_candidate_lineage_candidate_column(
        z, "envelope_bridge_eligible", "logical")[[1L]],
      pause_candidate_decision = stpd_candidate_lineage_candidate_column(
        z, "pause_candidate_decision", "character")[[1L]],
      pause_candidate_reason_code = stpd_candidate_lineage_candidate_column(
        z, "pause_candidate_reason_code", "character")[[1L]],
      candidate_payload_sha256 = stpd_candidate_lineage_contextual_pause_hash(
        z, paste0(source_route, "-candidate")),
      same_geometry_route_n = 1L,
      ontology_track = "gap",
      product_status = if (is_input) "upstream_proposal_input" else if (rejected) {
        "rejected_proposal_ledger"
      } else "preselection_proposal",
      selection_eligibility = if (is_input) "not_evaluated_pre_ownership" else if (rejected) {
        "not_selectable_action_reject"
      } else "eligible_for_downstream_preselection",
      observation_visibility = if (is_input || rejected) "internal_diagnostic" else
        "proposal_ledger",
      scientific_role = if (rejected) "rejected_gap_proposal_diagnostic" else
        "proposal_not_final_selection",
      stringsAsFactors = FALSE, check.names = FALSE
    )
  })
  out <- dplyr::bind_rows(rows)
  if (identical(hook, "contextual_pause_combined_output_v1")) {
    key <- paste(out$detector_start_row, out$detector_end_row, sep = ":")
    out$same_geometry_route_n <- as.integer(vapply(seq_along(key), function(i) {
      length(unique(out$route[key == key[[i]]]))
    }, integer(1)))
  }
  out[, names(schema), drop = FALSE]
}

stpd_candidate_lineage_contextual_pause_upstream <- function(shard, context) {
  raw_closed <- stpd_candidate_lineage_pause_raw_outputs_are_closed(
    shard, context$raw_candidates, context$boundaries,
    science_context = context
  )
  raw_receipt <- shard$observations$pause_raw_receipt_v1
  raw_receipt_hash <- if (is.null(raw_receipt)) "" else
    stpd_candidate_lineage_contextual_pause_hash(raw_receipt, "upstream-raw-receipt")
  patterns <- as.character(context$patterns %||% character())
  burst_requested <- any(c("burst", "long_burst") %in% patterns)
  route_final <- identical(context$burst_route, "final")
  if (!raw_closed) return(list(
    ok = FALSE, ownership = "raw_pause_not_closed",
    raw_receipt_sha256 = raw_receipt_hash,
    ownership_receipt_sha256 = ""
  ))
  if (!burst_requested) {
    return(list(
      ok = nrow(context$burst_support) == 0L,
      ownership = "not_required_burst_not_requested",
      raw_receipt_sha256 = raw_receipt_hash,
      ownership_receipt_sha256 = ""
    ))
  }
  if (!route_final) return(list(
    ok = TRUE, ownership = "unavailable_alternate_burst_route",
    raw_receipt_sha256 = raw_receipt_hash,
    ownership_receipt_sha256 = ""
  ))
  receipt <- shard$observations$burst_pause_ownership_receipt_v1
  owner_context <- if (exists("burst_pause_ownership_context", shard, inherits = FALSE))
    get("burst_pause_ownership_context", shard, inherits = FALSE) else NULL
  ok <- !is.null(receipt) && !is.null(owner_context) &&
    stpd_candidate_lineage_pause_ownership_payload_is_valid(
      shard, "burst_pause_ownership_receipt_v1", receipt
    ) && identical(owner_context$scientific_out, context$burst_support)
  list(
    ok = isTRUE(ok),
    ownership = if (ok) "ownership_observation_closed" else
      "ownership_observation_not_closed",
    raw_receipt_sha256 = raw_receipt_hash,
    ownership_receipt_sha256 = if (ok) {
      stpd_candidate_lineage_contextual_pause_hash(
        receipt, "upstream-ownership-receipt")
    } else ""
  )
}

stpd_candidate_lineage_contextual_pause_entry_row <- function(
    context, requested, applicability_status, upstream, scientific_role) {
  data.frame(
    train = context$train,
    root_id = "post_ownership_pause_proposal_root",
    pause_pattern_requested = requested,
    applicability_status = applicability_status,
    burst_route = context$burst_route,
    upstream_raw_status = "raw_pause_observation_closed",
    upstream_ownership_status = upstream$ownership,
    scientific_role = scientific_role,
    publication_authority = FALSE,
    candidate_universe_status = "unavailable",
    final_gap_selection_status = "not_observed",
    recurrent_pause_state_status = "not_evaluated_post_detection_only",
    hfs_direct_support_materialization_status = "not_observed",
    hfs_envelope_status = "not_observed",
    manual_override_status = "not_observed",
    performance_validation_status = "not_evaluated",
    threshold_optimization_status = "not_performed",
    scientific_result_influence = "none_observer_only",
    stringsAsFactors = FALSE, check.names = FALSE
  )
}

stpd_candidate_lineage_contextual_pause_route_collision_n <- function(x) {
  if (is.null(x) || !nrow(x)) return(0L)
  key <- paste(x$detector_start_row, x$detector_end_row, sep = ":")
  split_route <- split(as.character(x$route), key)
  as.integer(sum(vapply(split_route, function(z) length(unique(z)) > 1L,
                        logical(1))))
}

stpd_candidate_lineage_contextual_pause_receipt_row <- function(
    context, upstream, entry, generic_input, ownership_support,
    downgrade_attempts, generic_post, pair_attempts, interburst, combined,
    scan_exhausted, coverage_status, generic_coverage_status,
    pair_coverage_status, combined_replay_status,
    scientific_output_validation_status, scientific_role) {
  validated_science <- scientific_output_validation_status %in% c(
    "validated_exact_live_scientific_output", "validated_exact_typed_empty"
  )
  data.frame(
    train = context$train,
    root_id = "post_ownership_pause_proposal_root",
    generic_input_n = as.integer(nrow(generic_input)),
    ownership_support_n = as.integer(nrow(ownership_support)),
    generic_downgraded_n = as.integer(sum(downgrade_attempts$downgraded)),
    interburst_pair_attempt_n = as.integer(nrow(pair_attempts)),
    interburst_candidate_n = as.integer(nrow(interburst)),
    combined_output_n = as.integer(nrow(combined)),
    route_collision_n = stpd_candidate_lineage_contextual_pause_route_collision_n(combined),
    scan_exhausted = isTRUE(scan_exhausted),
    coverage_status = coverage_status,
    audit_capture_cap_applied = FALSE,
    deduplication_applied = FALSE,
    generic_downgrade_coverage_status = generic_coverage_status,
    interburst_pair_coverage_status = pair_coverage_status,
    combined_replay_status = combined_replay_status,
    candidate_universe_status = "unavailable",
    final_gap_selection_status = "not_observed",
    recurrent_pause_state_status = "not_evaluated_post_detection_only",
    hfs_direct_support_materialization_status = "not_observed",
    hfs_envelope_status = "not_observed",
    manual_override_status = "not_observed",
    performance_validation_status = "not_evaluated",
    threshold_optimization_status = "not_performed",
    scientific_output_validation_status = scientific_output_validation_status,
    publication_authority = FALSE,
    scientific_result_influence = "none_observer_only",
    scientific_role = scientific_role,
    upstream_raw_receipt_sha256 = as.character(
      upstream$raw_receipt_sha256 %||% "")[[1L]],
    upstream_ownership_receipt_sha256 = as.character(
      upstream$ownership_receipt_sha256 %||% "")[[1L]],
    entry_payload_sha256 = stpd_candidate_lineage_contextual_pause_hash(
      entry, "entry"),
    generic_input_payload_sha256 = stpd_candidate_lineage_contextual_pause_hash(
      generic_input, "generic-input"),
    ownership_support_payload_sha256 = stpd_candidate_lineage_contextual_pause_hash(
      ownership_support, "ownership-support-table"),
    downgrade_attempt_payload_sha256 = stpd_candidate_lineage_contextual_pause_hash(
      downgrade_attempts, "downgrade-attempts"),
    generic_post_payload_sha256 = stpd_candidate_lineage_contextual_pause_hash(
      generic_post, "generic-post"),
    pair_attempt_payload_sha256 = stpd_candidate_lineage_contextual_pause_hash(
      pair_attempts, "pair-attempts"),
    interburst_payload_sha256 = stpd_candidate_lineage_contextual_pause_hash(
      interburst, "interburst"),
    combined_payload_sha256 = stpd_candidate_lineage_contextual_pause_hash(
      combined, "combined-output"),
    scientific_output_sha256 = if (validated_science) {
      stpd_candidate_lineage_contextual_pause_hash(
        context$scientific_out, "scientific-output")
    } else "",
    stringsAsFactors = FALSE, check.names = FALSE
  )
}

stpd_candidate_lineage_contextual_pause_replay <- function(context, shard) {
  patterns <- as.character(context$patterns %||% character())
  requested <- "pause" %in% patterns
  burst_requested <- any(c("burst", "long_burst") %in% patterns)
  route_final <- identical(context$burst_route, "final")
  n <- nrow(context$dat)
  applicable <- requested && n > 2L && is.finite(context$vp$pause_thr) &&
    context$vp$pause_thr > 0
  upstream <- stpd_candidate_lineage_contextual_pause_upstream(shard, context)
  if (!isTRUE(upstream$ok)) return(NULL)

  unavailable_alternate <- burst_requested && !route_final
  if (!isTRUE(applicable) || unavailable_alternate) {
    if (!unavailable_alternate && !identical(context$scientific_out, data.frame())) return(NULL)
    applicability_status <- if (unavailable_alternate) {
      "invoked_but_lineage_unavailable_alternate_burst_route"
    } else if (!requested) {
      "not_invoked_pause_pattern_not_requested"
    } else "invoked_invalid_or_insufficient_input"
    scientific_role <- if (unavailable_alternate) {
      "proposal_lineage_unavailable"
    } else "proposal_not_final_selection"
    entry <- stpd_candidate_lineage_contextual_pause_entry_row(
      context, requested, applicability_status, upstream, scientific_role
    )
    empty <- function(h) stpd_candidate_lineage_empty_hook_payload(h)
    generic_input <- empty("contextual_pause_generic_input_v1")
    ownership_support <- empty("contextual_pause_ownership_support_v1")
    downgrade_attempts <- empty("contextual_pause_generic_downgrade_attempts_v1")
    generic_post <- empty("contextual_pause_generic_post_downgrade_v1")
    pair_attempts <- empty("contextual_pause_interburst_pair_attempts_v1")
    interburst <- empty("contextual_pause_interburst_candidates_v1")
    combined <- empty("contextual_pause_combined_output_v1")
    receipt <- stpd_candidate_lineage_contextual_pause_receipt_row(
      context, upstream, entry, generic_input, ownership_support,
      downgrade_attempts, generic_post, pair_attempts, interburst, combined,
      scan_exhausted = FALSE,
      coverage_status = if (unavailable_alternate) {
        "lineage_unavailable_alternate_burst_route"
      } else "typed_noop_not_applicable",
      generic_coverage_status = if (unavailable_alternate) {
        "unavailable_alternate_burst_route"
      } else "not_evaluated_not_applicable",
      pair_coverage_status = if (unavailable_alternate) {
        "unavailable_alternate_burst_route"
      } else "not_evaluated_not_applicable",
      combined_replay_status = if (unavailable_alternate) {
        "unavailable_alternate_burst_route"
      } else "exact_typed_empty_replay",
      scientific_output_validation_status = if (unavailable_alternate) {
        "not_validated_lineage_unavailable_alternate_burst_route"
      } else "validated_exact_typed_empty",
      scientific_role = scientific_role
    )
    return(list(
      entry = entry,
      generic_input = generic_input,
      ownership_support = ownership_support,
      downgrade_attempts = downgrade_attempts,
      generic_post = generic_post,
      pair_attempts = pair_attempts,
      interburst = interburst,
      combined = combined, receipt = receipt
    ))
  }

  generic <- context$raw_candidates
  support <- context$burst_support
  support_labels <- as.character(support$final_label %||% "")
  support_selected <- if ("selected_for_auto" %in% names(support)) {
    z <- as.logical(support$selected_for_auto); z[is.na(z)] <- FALSE; z
  } else rep(TRUE, nrow(support))
  support_keep <- support_selected & support_labels %in% c("burst", "long_burst")
  owned <- support[support_keep, , drop = FALSE]
  ownership_support <- if (!nrow(owned)) {
    stpd_candidate_lineage_empty_hook_payload("contextual_pause_ownership_support_v1")
  } else dplyr::bind_rows(lapply(seq_len(nrow(owned)), function(i) {
    z <- owned[i, , drop = FALSE]
    data.frame(
      train = context$train, support_ordinal = as.integer(i),
      candidate_id = stpd_candidate_lineage_candidate_column(z, "candidate_id", "character")[[1L]],
      detector_start_row = stpd_candidate_lineage_candidate_column(z, "start_isi", "integer")[[1L]],
      detector_end_row = stpd_candidate_lineage_candidate_column(z, "end_isi", "integer")[[1L]],
      candidate_payload_sha256 = stpd_candidate_lineage_contextual_pause_hash(z, "ownership-support"),
      ontology_track = "support",
      selection_eligibility = "not_a_pause_candidate_support_only",
      observation_visibility = "internal_diagnostic",
      scientific_role = "burst_ownership_support_only",
      stringsAsFactors = FALSE, check.names = FALSE
    )
  }))
  attempts <- vector("list", nrow(generic))
  if (nrow(generic)) for (i in seq_len(nrow(generic))) {
    before <- generic[i, , drop = FALSE]
    s <- as.integer(generic$start_isi[i]); e <- as.integer(generic$end_isi[i])
    hit <- if (nrow(owned)) which(as.integer(owned$start_isi) <= e &
      as.integer(owned$end_isi) >= s) else integer()
    blocked <- length(hit) > 0L
    if (blocked) {
      generic$gap_semantics[i] <- "ambiguous_gap"
      generic$hard_for_event[i] <- FALSE
      generic$hard_for_state_direct_support[i] <- FALSE
      generic$envelope_bridge_eligible[i] <- TRUE
      generic$pause_candidate_decision[i] <- "blocked_by_frozen_burst_support"
      generic$pause_candidate_reason_code[i] <-
        "generic_pause_overlaps_frozen_burst_bridge_or_boundary"
      if ("action" %in% names(generic)) generic$action[i] <- "reject"
      if ("decision_action" %in% names(generic)) generic$decision_action[i] <- "reject"
    }
    after <- generic[i, , drop = FALSE]
    before_character <- function(field) stpd_candidate_lineage_candidate_column(
      before, field, "character")[[1L]]
    after_character <- function(field) stpd_candidate_lineage_candidate_column(
      after, field, "character")[[1L]]
    before_logical <- function(field) stpd_candidate_lineage_candidate_column(
      before, field, "logical")[[1L]]
    after_logical <- function(field) stpd_candidate_lineage_candidate_column(
      after, field, "logical")[[1L]]
    attempts[[i]] <- data.frame(
      train = context$train, generic_ordinal = as.integer(i),
      candidate_id = before_character("candidate_id"),
      detector_start_row = s, detector_end_row = e,
      source_payload_sha256 = stpd_candidate_lineage_contextual_pause_hash(
        before, "generic_input-candidate"),
      output_payload_sha256 = stpd_candidate_lineage_contextual_pause_hash(
        after, "generic_post-candidate"),
      overlapping_support_n = as.integer(length(hit)),
      overlapping_support_ordinals = paste(hit, collapse = ";"),
      overlapping_support_candidate_ids = if (length(hit)) {
        paste(ownership_support$candidate_id[hit], collapse = ";")
      } else "",
      overlapping_support_payload_sha256 = if (length(hit)) {
        paste(ownership_support$candidate_payload_sha256[hit], collapse = ";")
      } else "",
      before_action = before_character("action"),
      after_action = after_character("action"),
      before_decision_action = before_character("decision_action"),
      after_decision_action = after_character("decision_action"),
      before_gap_semantics = before_character("gap_semantics"),
      after_gap_semantics = after_character("gap_semantics"),
      before_hard_for_event = before_logical("hard_for_event"),
      after_hard_for_event = after_logical("hard_for_event"),
      before_hard_for_state_direct_support = before_logical(
        "hard_for_state_direct_support"),
      after_hard_for_state_direct_support = after_logical(
        "hard_for_state_direct_support"),
      before_envelope_bridge_eligible = before_logical("envelope_bridge_eligible"),
      after_envelope_bridge_eligible = after_logical("envelope_bridge_eligible"),
      before_pause_candidate_decision = before_character("pause_candidate_decision"),
      after_pause_candidate_decision = after_character("pause_candidate_decision"),
      before_pause_candidate_reason_code = before_character(
        "pause_candidate_reason_code"),
      after_pause_candidate_reason_code = after_character(
        "pause_candidate_reason_code"),
      downgraded = blocked,
      terminal_reason = if (blocked) "overlaps_frozen_burst_support" else
        "no_burst_ownership_conflict",
      selection_eligibility = if (blocked) "not_selectable_action_reject" else
        "eligible_for_downstream_preselection",
      observation_visibility = "internal_diagnostic",
      scientific_role = "proposal_transform_not_final_selection",
      stringsAsFactors = FALSE, check.names = FALSE
    )
  }
  downgrade_attempts <- if (length(attempts)) dplyr::bind_rows(attempts) else
    stpd_candidate_lineage_empty_hook_payload("contextual_pause_generic_downgrade_attempts_v1")

  pair_attempts <- list(); emitted <- list(); candidate_counter <- 0L
  bursts <- support
  labels <- as.character(bursts$final_label %||% bursts$class %||% ""); labels[is.na(labels)] <- ""
  keep <- labels %in% c("burst", "long_burst")
  if ("decision_action" %in% names(bursts)) {
    a <- as.character(bursts$decision_action); a[is.na(a)] <- ""; keep <- keep & a == "accept"
  }
  bursts <- bursts[keep, , drop = FALSE]
  if (nrow(bursts) >= 2L) {
    selected <- if ("selected_for_auto" %in% names(bursts)) {
      z <- as.logical(bursts$selected_for_auto); z[is.na(z)] <- FALSE; z
    } else rep(FALSE, nrow(bursts))
    if (!any(selected)) {
      bursts <- stpd_event_core_weighted_select(bursts, locked = NULL,
                                                patterns = c("burst", "long_burst"))
      selected <- as.logical(bursts$selected_for_auto); selected[is.na(selected)] <- FALSE
    }
    bursts <- bursts[selected, , drop = FALSE]
  }
  if (nrow(bursts) >= 2L) {
    starts <- as.integer(bursts$start_isi); ends <- as.integer(bursts$end_isi)
    ok <- is.finite(starts) & is.finite(ends) & starts >= 2L & ends <= n & starts <= ends
    bursts <- bursts[ok, , drop = FALSE]; starts <- starts[ok]; ends <- ends[ok]
  }
  if (nrow(bursts) >= 2L) {
    ord <- order(starts, ends); bursts <- bursts[ord, , drop = FALSE]
    starts <- starts[ord]; ends <- ends[ord]
    isi <- suppressWarnings(as.numeric(context$dat$ISI_sec))
    artifact <- is_artifact_isi(isi, context$min_isi_sec)
    valid <- is.finite(isi) & !artifact; if (length(valid)) valid[1L] <- FALSE
    pp <- context$params$pause %||% list(); eg <- context$params$event_grammar %||% list()
    local_factor <- max(1, stpd_event_core_num(eg$pause_relative_local_factor %||%
      pp$relative_local_factor %||% 1.55, 1.55)); burst_factor <- max(1.5, local_factor)
    bp <- context$params$burst %||% list()
    merge_n <- max(1L, stpd_event_core_int(bp$merge_gap_max_n %||% 2L, 2L))
    max_gap_n <- max(merge_n, max(1L, stpd_event_core_int(
      bp$event_core_max_bridge_isi_count %||% merge_n, merge_n)))
    for (ii in seq_len(nrow(bursts) - 1L)) {
      gap0 <- stpd_event_core_safe_seq(ends[ii] + 1L, starts[ii + 1L] - 1L)
      reason <- ""; candidate_isi <- NA_integer_; candidate_value <- NA_real_
      med <- NA_real_; bridge <- NA_real_; contrast <- NA_real_
      threshold <- NA_real_; emitted_id <- ""; emitted_hash <- ""
      gap <- integer(); valid_gap_n <- NA_integer_
      candidate_selection_rule <- "not_evaluated"
      threshold_comparator <- "not_evaluated"
      strict_threshold_pass <- NA
      if (length(gap0) < 1L) reason <- "empty_gap"
      else if (length(gap0) > max_gap_n) reason <- "gap_exceeds_count_cap"
      if (!nzchar(reason)) {
        gap <- gap0[gap0 >= 2L & gap0 <= n & valid[gap0]]
        valid_gap_n <- as.integer(length(gap))
        if (!length(gap)) reason <- "no_valid_gap_isi"
      }
      if (!nzchar(reason)) {
        candidate_isi <- gap[which.max(isi[gap])]; candidate_value <- isi[candidate_isi]
        candidate_selection_rule <- "which.max_first_valid_isi_on_tie"
        left <- isi[stpd_event_core_safe_seq(starts[ii], ends[ii])]
        right <- isi[stpd_event_core_safe_seq(starts[ii + 1L], ends[ii + 1L])]
        flank <- valid_isi_values(c(left, right), context$min_isi_sec)
        med <- if (length(flank)) stats::median(flank, na.rm = TRUE) else NA_real_
        bridge <- stpd_event_core_num(context$vp$bridge_high, NA_real_)
        if (!is.finite(bridge) || bridge <= 0) reason <- "invalid_bridge_upper"
      }
      if (!nzchar(reason)) {
        contrast <- if (is.finite(med) && med > 0) burst_factor * med else NA_real_
        threshold <- max(c(bridge, contrast, context$min_isi_sec), na.rm = TRUE)
        threshold_comparator <- "strict_greater_than"
        strict_threshold_pass <- is.finite(threshold) && candidate_value > threshold
        if (!isTRUE(strict_threshold_pass))
          reason <- "fails_strict_context_threshold"
      }
      if (!nzchar(reason)) {
        candidate_counter <- candidate_counter + 1L
        row <- stpd_event_core_candidate_from_run(
          context$dat, candidate_isi, candidate_isi, context$params, context$vp,
          context$min_isi_sec, context$train, "event_core_pause_gap",
          "event_core_pause_gap", "pause", "event_core_pause_gap",
          "interburst_structural_non_bridge_gap", "accept",
          candidate_value / threshold, 300,
          list(candidate_id = paste0("event_core_interburst_pause_", candidate_counter),
            pause_threshold_sec = context$vp$pause_thr,
            pause_base_threshold_sec = context$vp$pause_thr,
            pause_effective_threshold_sec = threshold,
            pause_local_median_sec = med, pause_global_median_sec = NA_real_,
            pause_relative_local_factor = burst_factor,
            pause_relative_global_factor = NA_real_,
            pause_context_kind = "between_consecutive_burst_events",
            pause_context_rule = "non_bridge_gap_with_flanking_burst_contrast",
            pause_context_gap_isi_n = length(gap), pause_context_gap_isi_cap = max_gap_n,
            pause_context_burst_median_sec = med,
            pause_context_burst_factor = burst_factor,
            pause_context_bridge_upper_sec = bridge,
            pause_context_is_below_generic_pause_threshold = is.finite(context$vp$pause_thr) && candidate_value < context$vp$pause_thr,
            pause_context_left_burst_candidate_id = as.character(bursts$candidate_id[ii] %||% ""),
            pause_context_right_burst_candidate_id = as.character(bursts$candidate_id[ii + 1L] %||% ""),
            gap_semantics = "contextual_interburst_pause", hard_for_event = TRUE,
            hard_for_state_direct_support = TRUE, envelope_bridge_eligible = TRUE,
            pause_candidate_decision = "accepted",
            pause_candidate_reason_code = "two_independent_burst_cores_with_non_bridge_gap"))
        if (is.null(row) || !nrow(row)) {
          reason <- "candidate_construction_failed"
        } else {
          emitted_id <- as.character(row$candidate_id[1L])
          emitted_hash <- stpd_candidate_lineage_contextual_pause_hash(
            row, "interburst-candidate")
          emitted[[length(emitted) + 1L]] <- row; reason <- "candidate_emitted"
        }
      }
      pair_attempts[[length(pair_attempts) + 1L]] <- data.frame(
        train = context$train, pair_ordinal = as.integer(ii),
        left_candidate_id = as.character(bursts$candidate_id[ii] %||% ""),
        right_candidate_id = as.character(bursts$candidate_id[ii + 1L] %||% ""),
        left_candidate_payload_sha256 = stpd_candidate_lineage_contextual_pause_hash(
          bursts[ii, , drop = FALSE], "interburst-left-source"),
        right_candidate_payload_sha256 = stpd_candidate_lineage_contextual_pause_hash(
          bursts[ii + 1L, , drop = FALSE], "interburst-right-source"),
        raw_gap_isi_n = as.integer(length(gap0)), valid_gap_isi_n = valid_gap_n,
        gap_isi_cap = as.integer(max_gap_n), candidate_isi = as.integer(candidate_isi),
        candidate_value_sec = as.numeric(candidate_value),
        flank_median_sec = as.numeric(med), bridge_upper_sec = as.numeric(bridge),
        burst_factor = as.numeric(burst_factor),
        contrast_threshold_sec = as.numeric(contrast),
        minimum_isi_sec = as.numeric(context$min_isi_sec),
        threshold_sec = as.numeric(threshold),
        threshold_comparator = threshold_comparator,
        candidate_selection_rule = candidate_selection_rule,
        strict_threshold_pass = as.logical(strict_threshold_pass),
        terminal_reason = reason, emitted_candidate_id = emitted_id,
        emitted_output_payload_sha256 = emitted_hash,
        scientific_role = "proposal_attempt_not_final_selection",
        stringsAsFactors = FALSE, check.names = FALSE)
    }
  }
  pair_payload <- if (length(pair_attempts)) dplyr::bind_rows(pair_attempts) else
    stpd_candidate_lineage_empty_hook_payload("contextual_pause_interburst_pair_attempts_v1")
  interburst <- if (length(emitted)) dplyr::bind_rows(emitted) else data.frame()
  combined <- if (!nrow(generic)) interburst else if (!nrow(interburst)) generic else
    dplyr::bind_rows(generic, interburst)
  if (!requested || !applicable) combined <- data.frame()
  if (!identical(combined, context$scientific_out)) return(NULL)
  entry <- stpd_candidate_lineage_contextual_pause_entry_row(
    context, requested, "invoked_contextual_pause_scan", upstream,
    "proposal_not_final_selection"
  )
  generic_input <- stpd_candidate_lineage_contextual_pause_candidate_rows(
    context$raw_candidates, context$train, "generic_input")
  generic_post <- stpd_candidate_lineage_contextual_pause_candidate_rows(
    generic, context$train, "generic_post")
  inter_payload <- stpd_candidate_lineage_contextual_pause_candidate_rows(
    interburst, context$train, "interburst")
  combined_payload <- dplyr::bind_rows(generic_post, inter_payload)
  if (nrow(combined_payload)) {
    combined_payload$output_ordinal <- as.integer(seq_len(nrow(combined_payload)))
    geometry_key <- paste(combined_payload$detector_start_row,
                          combined_payload$detector_end_row, sep = ":")
    combined_payload$same_geometry_route_n <- as.integer(vapply(
      seq_along(geometry_key), function(i) {
        length(unique(combined_payload$route[geometry_key == geometry_key[[i]]]))
      }, integer(1)
    ))
  } else {
    combined_payload <- stpd_candidate_lineage_empty_hook_payload(
      "contextual_pause_combined_output_v1")
  }
  combined_payload <- combined_payload[, names(
    stpd_candidate_lineage_observation_hook_schema(
      "contextual_pause_combined_output_v1")), drop = FALSE]
  receipt <- stpd_candidate_lineage_contextual_pause_receipt_row(
    context, upstream, entry, generic_input, ownership_support,
    downgrade_attempts, generic_post, pair_payload, inter_payload,
    combined_payload, scan_exhausted = TRUE,
    coverage_status = "complete_post_ownership_pause_proposal_replay",
    generic_coverage_status =
      "complete_over_supplied_raw_proposals_and_frozen_support",
    pair_coverage_status = if (nrow(pair_payload)) {
      "complete_over_valid_sorted_frozen_ordinary_burst_support"
    } else "complete_no_valid_adjacent_pair",
    combined_replay_status = "exact_generic_first_append_only_replay",
    scientific_output_validation_status = "validated_exact_live_scientific_output",
    scientific_role = "proposal_not_final_selection"
  )
  list(entry = entry, generic_input = generic_input, ownership_support = ownership_support,
       downgrade_attempts = downgrade_attempts, generic_post = generic_post,
       pair_attempts = pair_payload, interburst = inter_payload,
       combined = combined_payload, receipt = receipt)
}

stpd_candidate_lineage_begin_pause_contextual <- function(
    shard, train, dat, params, vp, min_isi_sec, patterns, raw_candidates,
    boundaries, burst_support, burst_route) {
  if (is.null(shard)) return(invisible(NULL))
  stpd_candidate_lineage_validate_train_collector(
    shard, "open", train, validate_observations = FALSE
  )
  x <- unserialize(serialize(list(train = as.character(train), dat = dat,
    params = params, vp = vp, min_isi_sec = as.numeric(min_isi_sec)[1L],
    patterns = as.character(patterns %||% character()), raw_candidates = raw_candidates,
    boundaries = boundaries, burst_support = burst_support,
    burst_route = as.character(burst_route)[1L]), NULL, version = 3L))
  if (!stpd_candidate_lineage_pause_raw_outputs_are_closed(
      shard, x$raw_candidates, x$boundaries, science_context = x))
    stpd_candidate_lineage_abort("collector_contextual_pause_raw_not_closed",
      paste(
        "Contextual Pause begin does not close to the raw canonical-Pause",
        "proposal and its raw boundary snapshot."
      ))
  nm <- "contextual_pause_begin_context"
  if (exists(nm, shard, inherits = FALSE)) {
    if (!bindingIsLocked(nm, shard) || !identical(get(nm, shard), x))
      stpd_candidate_lineage_abort("collector_contextual_pause_begin_conflict",
        "Contextual Pause begin context conflicts with its first capture.")
  } else { assign(nm, x, shard); lockBinding(nm, shard) }
  invisible(x)
}

stpd_candidate_lineage_capture_pause_contextual <- function(
    shard, train, dat, params, vp, min_isi_sec, patterns, raw_candidates,
    boundaries, burst_support, burst_route, scientific_out) {
  if (is.null(shard)) return(invisible(NULL))
  nm <- "contextual_pause_begin_context"
  if (!exists(nm, shard, inherits = FALSE) || !bindingIsLocked(nm, shard))
    stpd_candidate_lineage_abort("collector_contextual_pause_begin_missing",
      "Contextual Pause capture requires locked live begin context.")
  supplied <- list(train = as.character(train), dat = dat, params = params, vp = vp,
    min_isi_sec = as.numeric(min_isi_sec)[1L], patterns = as.character(patterns %||% character()),
    raw_candidates = raw_candidates, boundaries = boundaries,
    burst_support = burst_support, burst_route = as.character(burst_route)[1L])
  begin <- get(nm, shard)
  if (!identical(begin, unserialize(serialize(supplied, NULL, version = 3L))))
    stpd_candidate_lineage_abort("collector_contextual_pause_begin_conflict",
      paste(
        "Contextual Pause capture differs from its locked begin context;",
        "begin and capture must use the same raw-Pause boundary snapshot."
      ))
  context <- begin; context$scientific_out <- as.data.frame(scientific_out %||% data.frame(),
    stringsAsFactors = FALSE, check.names = FALSE)
  context <- unserialize(serialize(context, NULL, version = 3L))
  replay <- stpd_candidate_lineage_contextual_pause_replay(context, shard)
  if (is.null(replay)) stpd_candidate_lineage_abort(
    "collector_contextual_pause_replay_failed", "Contextual Pause stages did not replay exactly.")
  replay_copy <- unserialize(serialize(replay, NULL, version = 3L))
  if (exists("contextual_pause_context", shard, inherits = FALSE)) {
    if (!bindingIsLocked("contextual_pause_context", shard) ||
        !identical(get("contextual_pause_context", shard), context) ||
        !exists("contextual_pause_replay_cache", shard, inherits = FALSE) ||
        !bindingIsLocked("contextual_pause_replay_cache", shard) ||
        !identical(get("contextual_pause_replay_cache", shard), replay_copy))
      stpd_candidate_lineage_abort("collector_contextual_pause_context_conflict",
        "Contextual Pause context conflicts with its first capture.")
  } else {
    assign("contextual_pause_context", context, shard)
    lockBinding("contextual_pause_context", shard)
    assign("contextual_pause_replay_cache", replay_copy, shard)
    lockBinding("contextual_pause_replay_cache", shard)
  }
  hooks <- stpd_candidate_lineage_contextual_pause_hooks()
  for (h in names(hooks)) {
    stpd_candidate_lineage_collector_capture(
      shard, h, replay[[hooks[[h]]]], validate_payload = FALSE
    )
  }
  invisible(replay)
}

stpd_candidate_lineage_contextual_pause_payload_is_valid <- function(shard, hook, payload) {
  if (!exists("contextual_pause_begin_context", shard, inherits = FALSE) ||
      !bindingIsLocked("contextual_pause_begin_context", shard) ||
      !exists("contextual_pause_context", shard, inherits = FALSE) ||
      !bindingIsLocked("contextual_pause_context", shard) ||
      !exists("contextual_pause_replay_cache", shard, inherits = FALSE) ||
      !bindingIsLocked("contextual_pause_replay_cache", shard)) return(FALSE)
  replay <- get("contextual_pause_replay_cache", shard)
  context <- get("contextual_pause_context", shard)
  begin <- get("contextual_pause_begin_context", shard)
  fields <- names(begin)
  if (!identical(context[fields], begin)) return(FALSE)
  fresh <- stpd_candidate_lineage_contextual_pause_replay(context, shard)
  if (is.null(fresh) || !identical(fresh, replay)) return(FALSE)
  map <- stpd_candidate_lineage_contextual_pause_hooks(); key <- unname(map[[hook]])
  if (is.null(key) || !identical(payload, replay[[key]])) return(FALSE)
  pos <- match(hook, names(map)); prior <- if (pos <= 1L) character() else names(map)[seq_len(pos - 1L)]
  all(vapply(prior, function(h) identical(shard$observations[[h]], replay[[map[[h]]]]), logical(1)))
}
