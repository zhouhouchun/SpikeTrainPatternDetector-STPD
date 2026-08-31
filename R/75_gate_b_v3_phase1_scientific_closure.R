# Gate B v3 phase-1 scientific closure.
#
# This file is deliberately collated immediately after the phase-1 contract
# implementation.  It wraps the structural validator with deterministic
# scientific recomputation from the canonical per-ISI spine.  The wrapper is
# additive: the original schema/identity/hash checks still run first.

.stpd_gate_b_v3_validate_product_prototype_structural <-
  stpd_gate_b_v3_validate_product_prototype

stpd_gate_b_v3_same_nullable <- function(observed, expected) {
  identical(is.na(observed), is.na(expected)) &&
    identical(unname(observed[!is.na(observed)]),
              unname(expected[!is.na(expected)]))
}

stpd_gate_b_v3_interval_mask <- function(spine, train, start_isi, end_isi) {
  spine$train == train & spine$isi_index >= start_isi &
    spine$isi_index <= end_isi
}

stpd_gate_b_v3_registry_code <- function(tables, registry_entry_id) {
  index <- match(registry_entry_id, tables$contract_registries$registry_entry_id)
  if (anyNA(index)) {
    stop("Gate B v3 scientific closure references an unknown registry entry.",
         call. = FALSE)
  }
  tables$contract_registries$code[index]
}

stpd_gate_b_v3_domain_id <- function(tables, entity_domain) {
  hit <- tables$entity_domain_registry$entity_domain == entity_domain &
    tables$entity_domain_registry$active
  if (sum(hit) != 1L) {
    stop("Gate B v3 scientific closure cannot resolve an entity domain: ",
         entity_domain, call. = FALSE)
  }
  tables$entity_domain_registry$entity_domain_id[hit]
}

stpd_gate_b_v3_bound_threshold_value <- function(
    tables, consumer_domain, consumer_id, path) {
  domain_id <- stpd_gate_b_v3_domain_id(tables, consumer_domain)
  binding <- tables$threshold_instance_bindings[
    tables$threshold_instance_bindings$consumer_domain_id == domain_id &
      tables$threshold_instance_bindings$consumer_id == consumer_id &
      tables$threshold_instance_bindings$path == path,,drop=FALSE]
  if (nrow(binding) != 1L) {
    stop("Gate B v3 scientific consumer does not resolve one exact threshold binding: ",
         consumer_domain, "/", path, call.=FALSE)
  }
  instance <- tables$threshold_instances[
    tables$threshold_instances$threshold_instance_id ==
      binding$threshold_instance_id[[1L]],,drop=FALSE]
  if (nrow(instance) != 1L || instance$path[[1L]] != path ||
      instance$value_type[[1L]] != "double" ||
      !is.finite(instance$value_num[[1L]]) ||
      !is.na(instance$value_chr[[1L]])) {
    stop("Gate B v3 scientific threshold binding does not resolve one finite double instance.",
         call.=FALSE)
  }
  instance$value_num[[1L]]
}

stpd_gate_b_v3_exact_bound_paths <- function(
    tables, consumer_domain, consumer_id, required_paths) {
  domain_id <- stpd_gate_b_v3_domain_id(tables, consumer_domain)
  rows <- tables$threshold_instance_bindings[
    tables$threshold_instance_bindings$consumer_domain_id == domain_id &
      tables$threshold_instance_bindings$consumer_id == consumer_id,,
    drop = FALSE]
  observed <- sort(unique(rows$path), method = "radix")
  expected <- sort(unique(required_paths), method = "radix")
  if (!identical(observed, expected) || nrow(rows) != length(expected)) {
    stop("Gate B v3 scientific consumer threshold bindings are not the exact required set: ",
         consumer_domain, call. = FALSE)
  }
  values <- vapply(expected, function(path) {
    stpd_gate_b_v3_bound_threshold_value(
      tables, consumer_domain, consumer_id, path)
  }, numeric(1))
  stats::setNames(values, expected)
}

stpd_gate_b_v3_scalar_row_payload <- function(row, excluded) {
  fields <- setdiff(names(row), excluded)
  stats::setNames(lapply(row[fields], function(value) value[[1L]]), fields)
}

stpd_gate_b_v3_validate_state_axis_science <- function(tables) {
  axis <- tables$state_axis_evidence
  if (nrow(axis) == 0L) return(invisible(TRUE))
  spine <- tables$per_isi
  candidates <- tables$state_candidates
  episodes <- tables$state_episodes
  segments <- tables$state_segments
  canonical <- stpd_gate_b_v3_canonical_json
  hash <- stpd_gate_b_v3_hash_raw

  for (i in seq_len(nrow(axis))) {
    row <- axis[i, , drop = FALSE]
    candidate <- candidates[
      candidates$state_candidate_id == row$state_candidate_id[[1L]],,
      drop = FALSE]
    if (nrow(candidate) != 1L ||
        candidate$train[[1L]] != row$train[[1L]] ||
        candidate$start_isi[[1L]] != row$start_isi[[1L]] ||
        candidate$end_isi[[1L]] != row$end_isi[[1L]]) {
      stop("Gate B v3 State-axis evidence is not bound to one exact candidate.",
           call. = FALSE)
    }
    mask <- stpd_gate_b_v3_interval_mask(
      spine, row$train[[1L]], row$start_isi[[1L]], row$end_isi[[1L]])
    values <- spine$isi_sec[mask]
    if (length(values) == 0L || any(!is.finite(values)) || any(values <= 0)) {
      stop("Gate B v3 State-axis estimator received invalid ISIs.",
           call. = FALSE)
    }
    expected_n <- as.integer(length(values))
    expected_duration <- stpd_gate_b_v3_sum_binary64(values)
    expected_frequency <- expected_n / expected_duration
    expected_regularity <- if (length(values) < 2L) {
      NA_real_
    } else {
      pairs <- 2 * abs(diff(values)) / (values[-length(values)] + values[-1L])
      stats::median(pairs)
    }
    frequency_code <- stpd_gate_b_v3_registry_code(
      tables, row$frequency_estimator_registry_id[[1L]])
    regularity_code <- stpd_gate_b_v3_registry_code(
      tables, row$regularity_estimator_registry_id[[1L]])
    if (frequency_code != "state_frequency_rate_v1" ||
        regularity_code != "state_regularity_median_cv2_v1") {
      stop("Gate B v3 phase-1 State-axis estimator is unsupported.",
           call. = FALSE)
    }
    state_paths <- c(
      "state.frequency.high_enter_hz",
      "state.frequency.high_exit_hz",
      "state.regularity.regular_upper_cv2",
      "state.regularity.irregular_lower_cv2",
      "state.state_min_valid_isi",
      "state.state_min_duration_sec")
    bounds <- stpd_gate_b_v3_exact_bound_paths(
      tables, "state_axis_evidence", row$state_axis_evidence_id[[1L]],
      state_paths)
    high_enter <- bounds[["state.frequency.high_enter_hz"]]
    high_exit <- bounds[["state.frequency.high_exit_hz"]]
    regular_upper <- bounds[["state.regularity.regular_upper_cv2"]]
    irregular_lower <- bounds[["state.regularity.irregular_lower_cv2"]]
    min_valid <- bounds[["state.state_min_valid_isi"]]
    min_duration <- bounds[["state.state_min_duration_sec"]]
    if (!(high_exit < high_enter) || !(regular_upper < irregular_lower) ||
        min_valid < 2 || min_valid != floor(min_valid) || min_duration <= 0) {
      stop("Gate B v3 State-axis thresholds violate the frozen relational contract.",
           call. = FALSE)
    }

    insufficient <- expected_n < min_valid || expected_duration < min_duration ||
      !is.finite(expected_frequency) || !is.finite(expected_regularity)
    expected_frequency_class <- if (insufficient) {
      "unresolved"
    } else if (expected_frequency >= high_enter) {
      "high"
    } else if (expected_frequency <= high_exit) {
      "non_high"
    } else {
      "unresolved"
    }
    expected_regularity_class <- if (insufficient) {
      "unresolved"
    } else if (expected_regularity <= regular_upper) {
      "regular"
    } else if (expected_regularity >= irregular_lower) {
      "irregular"
    } else {
      "unresolved"
    }
    expected_frequency_status <- if (insufficient) "insufficient" else
      if (expected_frequency_class == "unresolved") "gray_zone" else "pass"
    expected_regularity_status <- if (insufficient) "insufficient" else
      if (expected_regularity_class == "unresolved") "gray_zone" else "pass"
    expected_frequency_bound <- if (expected_frequency_class == "non_high")
      high_exit else high_enter
    expected_regularity_bound <- if (expected_regularity_class == "regular")
      regular_upper else irregular_lower
    expected_payload_hash <- hash(
      "stpd-state-axis-evidence-payload-v1",
      canonical(stpd_gate_b_v3_scalar_row_payload(row, c(
        "schema_version", "detection_root_id", "state_axis_evidence_id",
        "state_candidate_id", "evidence_payload_hash"))))
    if (!identical(row$n_valid_isi[[1L]], expected_n) ||
        !identical(row$effective_duration_sec[[1L]], expected_duration) ||
        !identical(row$frequency_stat[[1L]], expected_frequency) ||
        !identical(row$regularity_stat[[1L]], expected_regularity) ||
        !identical(row$frequency_pass_bound[[1L]], expected_frequency_bound) ||
        !identical(row$regularity_pass_bound[[1L]], expected_regularity_bound) ||
        !identical(row$frequency_gray_margin[[1L]], high_enter - high_exit) ||
        !identical(row$regularity_gray_margin[[1L]],
                   irregular_lower - regular_upper) ||
        !identical(row$frequency_evaluable[[1L]], !insufficient) ||
        !identical(row$regularity_evaluable[[1L]], !insufficient) ||
        !identical(row$frequency_evidence_status[[1L]],
                   expected_frequency_status) ||
        !identical(row$regularity_evidence_status[[1L]],
                   expected_regularity_status) ||
        !identical(is.na(row$insufficient_reason_registry_id[[1L]]),
                   !insufficient) ||
        !identical(row$evidence_payload_hash[[1L]], expected_payload_hash)) {
      stop("Gate B v3 State-axis statistics differ from deterministic spine recomputation.",
           call. = FALSE)
    }

    direct <- segments[
      segments$state_axis_evidence_id == row$state_axis_evidence_id[[1L]] &
        segments$segment_role == "direct_support",, drop = FALSE]
    axes_pass <- expected_frequency_class != "unresolved" &&
      expected_regularity_class != "unresolved"
    if (candidate$candidate_decision[[1L]] == "accepted_direct_support" &&
        (!axes_pass || nrow(direct) != 1L)) {
      stop("Gate B v3 accepted State candidate lacks exactly one passing direct-support projection.",
           call. = FALSE)
    }
    if (candidate$candidate_decision[[1L]] == "abstained" && axes_pass) {
      stop("Gate B v3 State candidate abstains despite two decisive orthogonal axes.",
           call. = FALSE)
    }
    if (nrow(direct) == 0L) next
    episode_rows <- episodes[
      episodes$state_episode_id %in% direct$state_episode_id,, drop = FALSE]
    for (j in seq_len(nrow(episode_rows))) {
      episode <- episode_rows[j, , drop = FALSE]
      expected_class <- paste(episode$state_frequency_class[[1L]],
                              episode$state_regularity_class[[1L]], sep = "\u001f")
      class_map <- c(
        "high\u001firregular" = "high_frequency_irregular_state",
        "high\u001fregular" = "high_frequency_tonic",
        "non_high\u001fregular" = "tonic"
      )
      if (episode$state_frequency_class[[1L]] != expected_frequency_class ||
          episode$state_regularity_class[[1L]] != expected_regularity_class ||
          row$frequency_evidence_status[[1L]] != "pass" ||
          row$regularity_evidence_status[[1L]] != "pass" ||
          is.na(class_map[[expected_class]]) ||
          episode$state_class[[1L]] != class_map[[expected_class]]) {
        stop("Gate B v3 State classification is inconsistent with recomputed orthogonal axes.",
             call. = FALSE)
      }
    }
  }
  invisible(TRUE)
}

stpd_gate_b_v3_expected_projection <- function(spine, intervals, id_column) {
  expected <- rep(NA_character_, nrow(spine))
  if (nrow(intervals) == 0L) return(expected)
  for (i in seq_len(nrow(intervals))) {
    hit <- stpd_gate_b_v3_interval_mask(
      spine, intervals$train[[i]], intervals$start_isi[[i]],
      intervals$end_isi[[i]])
    if (any(!is.na(expected[hit]))) {
      stop("Gate B v3 accepted intervals overlap on a single-value track.",
           call. = FALSE)
    }
    expected[hit] <- intervals[[id_column]][[i]]
  }
  expected
}

stpd_gate_b_v3_validate_projection_science <- function(tables) {
  spine <- tables$per_isi
  expected_event <- stpd_gate_b_v3_expected_projection(
    spine, tables$events, "event_id")
  expected_gap <- stpd_gate_b_v3_expected_projection(
    spine, tables$gaps, "gap_id")
  if (any(!is.na(expected_event) & !is.na(expected_gap))) {
    stop("Gate B v3 Event and canonical Gap supports overlap.", call. = FALSE)
  }
  if (any(!is.na(expected_gap) & spine$state_episode_membership)) {
    stop("Gate B v3 canonical Gap occurs inside State support.", call. = FALSE)
  }
  expected_boundary <- rep(NA_character_, nrow(spine))
  boundaries <- tables$boundary_evidence
  if (nrow(boundaries) > 0L) {
    for (row_index in seq_len(nrow(spine))) {
      hit <- boundaries$train == spine$train[[row_index]] &
        boundaries$start_isi <= spine$isi_index[[row_index]] &
        boundaries$end_isi >= spine$isi_index[[row_index]]
      if (!any(hit)) next
      candidates <- boundaries[hit, , drop = FALSE]
      best <- candidates$boundary_priority == max(candidates$boundary_priority)
      if (sum(best) != 1L) {
        stop("Gate B v3 controlling-boundary precedence is not unique.",
             call. = FALSE)
      }
      expected_boundary[[row_index]] <- candidates$boundary_id[best]
    }
  }
  if (!stpd_gate_b_v3_same_nullable(spine$event_id, expected_event) ||
      !stpd_gate_b_v3_same_nullable(spine$gap_id, expected_gap) ||
      !stpd_gate_b_v3_same_nullable(spine$controlling_boundary_id,
                                    expected_boundary)) {
    stop("Gate B v3 per-ISI Event/Gap/boundary projection is not exact.",
         call. = FALSE)
  }
  accepted_candidates <- tables$event_candidates[
    tables$event_candidates$candidate_decision == "accepted_event",,
    drop=FALSE]
  accepted_key <- paste(accepted_candidates$train,
    accepted_candidates$start_isi,accepted_candidates$end_isi,sep="\u001f")
  event_key <- paste(tables$events$train,tables$events$start_isi,
    tables$events$end_isi,sep="\u001f")
  if (!setequal(accepted_key,event_key) || anyDuplicated(accepted_key) ||
      anyDuplicated(event_key)) {
    stop("Gate B v3 accepted Event-candidate projection is incomplete or ambiguous.",
         call.=FALSE)
  }
  if (nrow(tables$events)>0L) {
    accepted_candidates <- accepted_candidates[
      match(event_key,accepted_key),,drop=FALSE]
    if (any(accepted_candidates$candidate_class != "burst_candidate" |
            tables$events$event_class != "burst_event")) {
      stop("Gate B v3 Event class is inconsistent with its accepted candidate.",
           call.=FALSE)
    }
  }
  invisible(TRUE)
}

stpd_gate_b_v3_expected_event_state_relationships <- function(tables) {
  events <- tables$events
  episodes <- tables$state_episodes
  segments <- tables$state_segments
  spine <- tables$per_isi
  rows <- list()
  for (i in seq_len(nrow(events))) for (j in seq_len(nrow(episodes))) {
    event <- events[i, , drop = FALSE]
    episode <- episodes[j, , drop = FALSE]
    if (event$train[[1L]] != episode$train[[1L]]) next
    event_mask <- stpd_gate_b_v3_interval_mask(
      spine, event$train[[1L]], event$start_isi[[1L]], event$end_isi[[1L]])
    episode_mask <- stpd_gate_b_v3_interval_mask(
      spine, episode$train[[1L]], episode$start_isi[[1L]], episode$end_isi[[1L]])
    overlap <- event_mask & episode_mask
    if (!any(overlap)) next
    episode_segments <- segments[
      segments$state_episode_id == episode$state_episode_id[[1L]],,
      drop = FALSE]
    direct_mask <- rep(FALSE, nrow(spine))
    connector_mask <- rep(FALSE, nrow(spine))
    for (k in seq_len(nrow(episode_segments))) {
      segment_mask <- stpd_gate_b_v3_interval_mask(
        spine, episode_segments$train[[k]],
        episode_segments$start_isi[[k]], episode_segments$end_isi[[k]])
      if (episode_segments$segment_role[[k]] == "direct_support") {
        direct_mask <- direct_mask | segment_mask
      } else {
        connector_mask <- connector_mask | segment_mask
      }
    }
    direct_overlap <- overlap & direct_mask
    connector_overlap <- overlap & connector_mask
    sum_sec <- function(mask) {
      if (!any(mask)) return(0)
      stpd_gate_b_v3_sum_binary64(spine$isi_sec[mask])
    }
    episode_n <- sum(overlap)
    direct_n <- sum(direct_overlap)
    connector_n <- sum(connector_overlap)
    episode_sec <- sum_sec(overlap)
    direct_sec <- sum_sec(direct_overlap)
    connector_sec <- sum_sec(connector_overlap)
    rows[[length(rows) + 1L]] <- data.frame(
      event_id = event$event_id[[1L]],
      state_episode_id = episode$state_episode_id[[1L]],
      episode_overlap_n_isi = as.integer(episode_n),
      direct_support_overlap_n_isi = as.integer(direct_n),
      connector_overlap_n_isi = as.integer(connector_n),
      episode_overlap_sec = episode_sec,
      direct_support_overlap_sec = direct_sec,
      connector_overlap_sec = connector_sec,
      event_covered_by_episode_time_fraction = episode_sec / event$duration_sec[[1L]],
      event_covered_by_direct_support_time_fraction = direct_sec / event$duration_sec[[1L]],
      state_envelope_covered_by_event_time_fraction = episode_sec / episode$envelope_duration_sec[[1L]],
      state_direct_support_covered_by_event_time_fraction = direct_sec / episode$direct_support_duration_sec[[1L]],
      event_covered_by_episode_isi_fraction = episode_n / event$n_isi[[1L]],
      event_covered_by_direct_support_isi_fraction = direct_n / event$n_isi[[1L]],
      state_envelope_covered_by_event_isi_fraction = episode_n / episode$n_isi[[1L]],
      state_direct_support_covered_by_event_isi_fraction = direct_n / episode$direct_support_n_isi[[1L]],
      stringsAsFactors = FALSE)
  }
  if (length(rows) == 0L) return(NULL)
  do.call(rbind, rows)
}

stpd_gate_b_v3_validate_relationship_science <- function(tables) {
  observed <- tables$event_state_relationships
  expected <- stpd_gate_b_v3_expected_event_state_relationships(tables)
  expected_key <- if (is.null(expected)) character() else paste(
    expected$event_id, expected$state_episode_id, sep = "\u001f")
  observed_key <- if (nrow(observed) == 0L) character() else paste(
    observed$event_id, observed$state_episode_id, sep = "\u001f")
  if (!setequal(expected_key, observed_key) || anyDuplicated(observed_key)) {
    stop("Gate B v3 Event-State relationship expected set is incomplete.",
         call. = FALSE)
  }
  if (!is.null(expected)) {
    numeric_fields <- setdiff(names(expected), c("event_id", "state_episode_id"))
    expected <- expected[match(observed_key, expected_key), , drop = FALSE]
    for (field in numeric_fields) {
      if (!identical(unname(observed[[field]]), unname(expected[[field]]))) {
        stop("Gate B v3 Event-State relationship geometry differs from the canonical spine: ",
             field, call. = FALSE)
      }
    }
  }

  if (nrow(observed) > 0L) {
    event_class <- tables$events$event_class[
      match(observed$event_id, tables$events$event_id)]
    state_class <- tables$state_episodes$state_class[
      match(observed$state_episode_id, tables$state_episodes$state_episode_id)]
    incompatible <- event_class == "burst_event" &
      state_class %in% c("high_frequency_tonic", "tonic")
    if (any(incompatible)) {
      stop("Gate B v3 Burst may not coexist directly with HFT/tonic State.",
           call. = FALSE)
    }
  }

  # A Burst may overlap the immutable parent candidate of a regular State,
  # but the materialized State must be split/re-detected and represented by
  # one explicit interruption relationship.  The candidate is the only
  # generation-before object from which this expected set can be rebuilt.
  parent_candidates <- tables$state_candidates[
    tables$state_candidates$candidate_decision == "accepted_direct_support",,
    drop = FALSE]
  axis <- tables$state_axis_evidence
  expected_interruptions <- character()
  if (nrow(parent_candidates) > 0L && nrow(tables$events) > 0L) {
    for (i in seq_len(nrow(tables$events))) {
      event <- tables$events[i, , drop = FALSE]
      for (j in seq_len(nrow(parent_candidates))) {
        parent <- parent_candidates[j, , drop = FALSE]
        if (event$train[[1L]] != parent$train[[1L]] ||
            event$end_isi[[1L]] < parent$start_isi[[1L]] ||
            event$start_isi[[1L]] > parent$end_isi[[1L]]) next
        evidence <- axis[
          axis$state_axis_evidence_id == parent$state_axis_evidence_id[[1L]],,
          drop = FALSE]
        if (nrow(evidence) != 1L) next
        is_regular <- evidence$regularity_stat[[1L]] <=
          evidence$regularity_pass_bound[[1L]]
        if (isTRUE(is_regular)) {
          expected_interruptions <- c(expected_interruptions, paste(
            event$event_id[[1L]], parent$state_candidate_id[[1L]],
            sep = "\u001f"))
        }
      }
    }
  }
  interruptions <- tables$event_interrupted_state_relationships
  observed_interruptions <- if (nrow(interruptions) == 0L) character() else
    paste(interruptions$event_id, interruptions$parent_state_candidate_id,
          sep = "\u001f")
  if (!setequal(expected_interruptions, observed_interruptions) ||
      anyDuplicated(observed_interruptions)) {
    stop("Gate B v3 Burst-versus-regular-State interruption closure is invalid.",
         call. = FALSE)
  }
  links <- tables$state_episode_links
  if (nrow(links) > 0L) {
    pre_class <- tables$state_episodes$state_class[
      match(links$pre_state_episode_id, tables$state_episodes$state_episode_id)]
    post_class <- tables$state_episodes$state_class[
      match(links$post_state_episode_id, tables$state_episodes$state_episode_id)]
    if (any(pre_class != "high_frequency_irregular_state" |
            post_class != "high_frequency_irregular_state")) {
      stop("Gate B v3 pause-interrupted HF links require HF-irregular States on both sides.",
           call. = FALSE)
    }
  }
  invisible(TRUE)
}

stpd_gate_b_v3_applicable_threshold_value <- function(tables, path, train) {
  rows <- tables$threshold_instances[tables$threshold_instances$path == path,,
                                     drop = FALSE]
  if (nrow(rows) == 0L) {
    stop("Gate B v3 required threshold instance is absent: ", path,
         call. = FALSE)
  }
  applicable <- rows$application_scope == "global" |
    (rows$application_scope == "train" & !is.na(rows$train) &
       rows$train == train)
  rows <- rows[applicable,,drop = FALSE]
  if (nrow(rows) != 1L || rows$value_type[[1L]] != "double" ||
      !is.finite(rows$value_num[[1L]]) || !is.na(rows$value_chr[[1L]])) {
    stop("Gate B v3 threshold scope does not resolve one finite value: ", path,
         call. = FALSE)
  }
  rows$value_num[[1L]]
}

stpd_gate_b_v3_context_epoch_for_isi <- function(tables, train, isi_index) {
  epochs <- tables$scientific_context_epochs
  if (nrow(epochs) == 0L) return(NA_character_)
  hit <- epochs$train == train & epochs$start_isi <= isi_index &
    epochs$end_isi >= isi_index & !epochs$hard_boundary
  if (sum(hit) > 1L) {
    stop("Gate B v3 Pause candidate belongs to multiple scientific context epochs.",
         call. = FALSE)
  }
  if (sum(hit) == 0L) NA_character_ else epochs$context_epoch_id[hit]
}

stpd_gate_b_v3_pause_qc_status <- function(tables, candidate) {
  record <- tables$evidence_records[
    tables$evidence_records$evidence_id == candidate$qc_evidence_id[[1L]],,
    drop = FALSE]
  if (nrow(record) != 1L ||
      stpd_gate_b_v3_registry_code(
        tables, record$evidence_schema_registry_id[[1L]]) !=
        "qc_eligibility_v1") {
    stop("Gate B v3 Pause candidate lacks exact QC-eligibility evidence.",
         call. = FALSE)
  }
  payload <- jsonlite::fromJSON(record$evidence_json[[1L]],
                                simplifyVector = TRUE)
  exact_names <- c("hard_boundary_clear", "isi_index", "qc_eligibility",
                   "train")
  if (!identical(sort(names(payload), method = "radix"),
                 sort(exact_names, method = "radix")) ||
      !identical(payload$train, candidate$train[[1L]]) ||
      !identical(as.integer(payload$isi_index), candidate$start_isi[[1L]]) ||
      !is.logical(payload$hard_boundary_clear) ||
      length(payload$hard_boundary_clear) != 1L ||
      !payload$qc_eligibility %in% c(
        "eligible", "timestamp_only_uncertain", "ineligible")) {
    stop("Gate B v3 Pause QC evidence payload is not canonical.",
         call. = FALSE)
  }
  expected_json <- stpd_gate_b_v3_canonical_json(payload)
  expected_source <- stpd_gate_b_v3_hash_raw(
    "stpd-qc-eligibility-source-v1", expected_json)
  expected_json_hash <- stpd_gate_b_v3_hash_raw(
    "stpd-evidence-json-v1", stpd_gate_b_v3_canonical_json(list(
      evidence_schema_registry_id=record$evidence_schema_registry_id[[1L]],
      evidence_json=payload)))
  expected_id <- stpd_gate_b_v3_entity_id("evidence", list(
    detection_root_id=candidate$detection_root_id[[1L]],
    evidence_type_registry_id=record$evidence_type_registry_id[[1L]],
    evidence_schema_registry_id=record$evidence_schema_registry_id[[1L]],
    source_bytes_sha256=expected_source,
    evidence_json_hash=expected_json_hash), stpd_gate_b_v3_phase1_bundle())
  if (!identical(record$evidence_json[[1L]], expected_json) ||
      !identical(record$source_bytes_sha256[[1L]], expected_source) ||
      !identical(record$evidence_json_hash[[1L]], expected_json_hash) ||
      !identical(record$evidence_id[[1L]], expected_id)) {
    stop("Gate B v3 Pause QC evidence identity is not content-addressed.",
         call. = FALSE)
  }
  list(status = payload$qc_eligibility,
       hard_boundary_clear = isTRUE(payload$hard_boundary_clear))
}

stpd_gate_b_v3_validate_gap_science <- function(tables) {
  candidates <- tables$gap_candidates
  gaps <- tables$gaps
  spine <- tables$per_isi
  candidate_mask <- logical(nrow(spine))
  for (train in unique(spine$train)) {
    train_mask <- spine$train == train
    seed <- stpd_gate_b_v3_applicable_threshold_value(
      tables, "pause.scan_seed_sec", train)
    if (!is.finite(seed) || seed <= 0) {
      stop("Gate B v3 Pause scan seed must be finite and positive.",
           call. = FALSE)
    }
    candidate_mask[train_mask] <- is.finite(spine$isi_sec[train_mask]) &
      spine$isi_sec[train_mask] > 0 & spine$isi_sec[train_mask] >= seed
  }
  expected_spine <- spine[candidate_mask,,drop = FALSE]
  expected_key <- paste(expected_spine$train, expected_spine$isi_index,
                        sep = "\u001f")
  observed_key <- paste(candidates$train, candidates$start_isi,
                        sep = "\u001f")
  if (!setequal(expected_key, observed_key) || anyDuplicated(observed_key) ||
      (nrow(candidates) > 0L && any(candidates$start_isi != candidates$end_isi))) {
    stop("Gate B v3 complete single-ISI Gap-candidate universe is missing, duplicated, or noncanonical.",
         call. = FALSE)
  }
  if (nrow(candidates) > 0L) {
    ordered_spine <- expected_spine[match(observed_key, expected_key), ,
                                    drop = FALSE]
    if (anyNA(ordered_spine$isi_index) ||
        !identical(candidates$n_isi, rep(1L, nrow(candidates))) ||
        !identical(candidates$start_time_sec,
                   ordered_spine$isi_start_time_sec) ||
        !identical(candidates$end_time_sec,
                   ordered_spine$isi_end_time_sec) ||
        !identical(candidates$duration_sec, ordered_spine$isi_sec)) {
      stop("Gate B v3 Gap-candidate geometry differs from the canonical spine.",
           call. = FALSE)
    }
    if (any(stpd_gate_b_v3_registry_code(
        tables,candidates$local_estimator_registry_id) !=
        "pause_local_window_median_v1")) {
      stop("Gate B v3 phase-1 Gap estimator is unsupported.",call.=FALSE)
    }
    if (any(stpd_gate_b_v3_registry_code(
        tables,candidates$multiple_testing_method_registry_id) !=
        "benjamini_hochberg_v1") ||
        any(stpd_gate_b_v3_registry_code(
          tables,candidates$gap_policy_registry_id) !=
          "pause_local_empirical_bh_v1")) {
      stop("Gate B v3 phase-1 Gap policy or multiple-testing method is unsupported.",
           call. = FALSE)
    }
    # Reconstruct the complete multiple-testing families from immutable parent
    # coordinates.  Never let a submitted family id choose its own BH universe:
    # otherwise one complete family can be split into smaller families, re-sealed,
    # and assigned artificially favourable q values.
    family_keys <- vapply(seq_len(nrow(candidates)), function(i) {
      stpd_gate_b_v3_hash_raw(
        "stpd-gap-multiple-testing-family-key-v1",
        stpd_gate_b_v3_canonical_json(list(
          detection_root_id = candidates$detection_root_id[[i]],
          train = candidates$train[[i]],
          gap_policy_registry_id = candidates$gap_policy_registry_id[[i]],
          scientific_context_epoch_id = if (
            is.na(candidates$scientific_context_epoch_id[[i]])) NULL else
              candidates$scientific_context_epoch_id[[i]]
        )))
    }, character(1))
    families <- split(seq_len(nrow(candidates)), family_keys)
    for (indices in families) {
      family <- candidates[indices,,drop=FALSE]
      family_epoch <- unique(family$scientific_context_epoch_id)
      expected_family_id <- stpd_gate_b_v3_entity_id(
        "multiple_testing_family",list(
          detection_root_id=family$detection_root_id[[1L]],
          train=family$train[[1L]],
          gap_policy_registry_id=family$gap_policy_registry_id[[1L]],
          scientific_context_epoch_id=if (is.na(family_epoch[[1L]])) NULL else
            family_epoch[[1L]],
          sorted_gap_candidate_ids=as.list(sort(
            family$gap_candidate_id,method="radix"))),
        stpd_gate_b_v3_phase1_bundle())
      if (length(unique(family$train))!=1L ||
          length(unique(family$gap_policy_registry_id))!=1L ||
          length(unique(family$detection_root_id))!=1L ||
          length(family_epoch)!=1L ||
          length(unique(family$multiple_testing_family_id))!=1L ||
          any(family$multiple_testing_family_id!=expected_family_id)) {
        stop("Gate B v3 Gap multiple-testing family identity is incomplete.",
             call.=FALSE)
      }
      expected_p <- rep(NA_real_,nrow(family))
      expected_reference <- rep(NA_real_,nrow(family))
      expected_ratio <- rep(NA_real_,nrow(family))
      expected_valid_n <- integer(nrow(family))
      expected_window_start <- rep(NA_integer_,nrow(family))
      expected_window_end <- rep(NA_integer_,nrow(family))
      expected_qc <- vector("list", nrow(family))
      for (j in seq_len(nrow(family))) {
        row <- family[j,,drop = FALSE]
        expected_epoch <- stpd_gate_b_v3_context_epoch_for_isi(
          tables, row$train[[1L]], row$start_isi[[1L]])
        if (!stpd_gate_b_v3_same_nullable(
            row$scientific_context_epoch_id, expected_epoch)) {
          stop("Gate B v3 Gap candidate context epoch is not reconstructed from the parent spine.",
               call. = FALSE)
        }
        paths <- c("pause.scan_seed_sec", "pause.absolute_threshold_sec",
                   "pause.local_ratio_min", "pause.local_window_isi",
                   "pause.guard_band_isi", "pause.local_min_valid_isi",
                   "pause.score_alpha")
        bounds <- stpd_gate_b_v3_exact_bound_paths(
          tables, "gap_candidate", row$gap_candidate_id[[1L]], paths)
        window <- bounds[["pause.local_window_isi"]]
        guard <- bounds[["pause.guard_band_isi"]]
        min_local <- bounds[["pause.local_min_valid_isi"]]
        if (window != floor(window) || guard != floor(guard) ||
            min_local != floor(min_local) || window < 1 || guard < 0 ||
            guard >= window || min_local < 1 ||
            row$guard_band_n_isi[[1L]] != as.integer(guard)) {
          stop("Gate B v3 Pause local-window thresholds are invalid.",
               call. = FALSE)
        }
        train_rows <- spine$train == row$train[[1L]]
        distance <- abs(spine$isi_index - row$start_isi[[1L]])
        reference_mask <- train_rows & distance <= window & distance > guard &
          is.finite(spine$isi_sec) & spine$isi_sec > 0
        if (!is.na(expected_epoch)) {
          epoch <- tables$scientific_context_epochs[
            tables$scientific_context_epochs$context_epoch_id == expected_epoch,,
            drop = FALSE]
          reference_mask <- reference_mask & spine$isi_index >= epoch$start_isi[[1L]] &
            spine$isi_index <= epoch$end_isi[[1L]]
        }
        reference_indices <- which(reference_mask)
        if (length(reference_indices) > 0L &&
            nrow(tables$boundary_evidence) > 0L) {
          boundary_rows <- tables$boundary_evidence[
            tables$boundary_evidence$train == row$train[[1L]] &
              (tables$boundary_evidence$hard_for_state |
                 tables$boundary_evidence$hard_for_episode_link),,
            drop = FALSE]
          own_gap <- tables$gaps$gap_id[
            tables$gaps$gap_candidate_id == row$gap_candidate_id[[1L]]]
          if (length(own_gap) == 1L) {
            boundary_rows <- boundary_rows[
              is.na(boundary_rows$source_gap_id) |
                boundary_rows$source_gap_id != own_gap[[1L]],,
              drop = FALSE]
          }
          clear <- vapply(reference_indices, function(index) {
            lo <- min(spine$isi_index[[index]], row$start_isi[[1L]])
            hi <- max(spine$isi_index[[index]], row$start_isi[[1L]])
            !any(!is.na(boundary_rows$start_isi) &
                   !is.na(boundary_rows$end_isi) &
                   boundary_rows$start_isi <= hi &
                   boundary_rows$end_isi >= lo)
          }, logical(1))
          reference_indices <- reference_indices[clear]
        }
        reference <- spine$isi_sec[reference_indices]
        expected_valid_n[[j]] <- length(reference)
        expected_qc[[j]] <- stpd_gate_b_v3_pause_qc_status(tables, row)
        if (length(reference) < min_local ||
            expected_qc[[j]]$status == "ineligible" ||
            !expected_qc[[j]]$hard_boundary_clear ||
            isTRUE(row$boundary_censored[[1L]])) next
        expected_reference[[j]] <- stats::median(reference)
        expected_ratio[[j]] <- family$duration_sec[[j]]/
          expected_reference[[j]]
        expected_p[[j]] <- (1+sum(reference>=family$duration_sec[[j]]))/
          (length(reference)+1)
        expected_window_start[[j]] <- min(spine$isi_index[reference_indices])
        expected_window_end[[j]] <- max(spine$isi_index[reference_indices])
      }
      if (!identical(family$valid_local_n_isi,expected_valid_n) ||
          !stpd_gate_b_v3_same_nullable(
            family$local_reference_sec,expected_reference) ||
          !stpd_gate_b_v3_same_nullable(family$local_ratio,expected_ratio) ||
          !stpd_gate_b_v3_same_nullable(family$raw_p_value,expected_p) ||
          !stpd_gate_b_v3_same_nullable(
            family$reference_window_start_isi,expected_window_start) ||
          !stpd_gate_b_v3_same_nullable(
            family$reference_window_end_isi,expected_window_end)) {
        stop("Gate B v3 Gap local evidence differs from leave-one-out recomputation.",
             call.=FALSE)
      }
      q_order <- order(family$gap_candidate_id,method="radix")
      expected_q <- rep(NA_real_,nrow(family))
      evaluable <- !is.na(expected_p)
      if (any(evaluable)) {
        eval_order <- q_order[evaluable[q_order]]
        expected_q[eval_order] <- stats::p.adjust(
          expected_p[eval_order], method="BH")
      }
      if (!stpd_gate_b_v3_same_nullable(
          family$adjusted_q_value,expected_q)) {
        stop("Gate B v3 Gap BH values differ from complete-family recomputation.",
             call.=FALSE)
      }
      evidence_rows <- tables$evidence_records[
        tables$evidence_records$evidence_id %in% family$evidence_id,,
        drop=FALSE]
      if (length(unique(family$evidence_id)) != 1L ||
          nrow(evidence_rows) != 1L) {
        stop("Gate B v3 Gap family evidence record is absent or ambiguous.",
             call. = FALSE)
      }
      evidence_order <- order(family$gap_candidate_id,method="radix")
      expected_payload <- list(
        candidate_ids=as.list(family$gap_candidate_id[evidence_order]),
        raw_p_values=as.list(expected_p[evidence_order]),
        adjusted_q_values=as.list(expected_q[evidence_order]),
        method_registry_id=family$multiple_testing_method_registry_id[[1L]])
      expected_evidence_json <- stpd_gate_b_v3_canonical_json(expected_payload)
      expected_evidence_json_hash <- stpd_gate_b_v3_hash_raw(
        "stpd-evidence-json-v1", stpd_gate_b_v3_canonical_json(list(
          evidence_schema_registry_id=
            evidence_rows$evidence_schema_registry_id[[1L]],
          evidence_json=expected_payload)))
      expected_source_hash <- stpd_gate_b_v3_hash_raw(
        "stpd-gap-empirical-family-source-v1", expected_evidence_json)
      expected_evidence_id <- stpd_gate_b_v3_entity_id("evidence", list(
        detection_root_id=family$detection_root_id[[1L]],
        evidence_type_registry_id=evidence_rows$evidence_type_registry_id[[1L]],
        evidence_schema_registry_id=
          evidence_rows$evidence_schema_registry_id[[1L]],
        source_bytes_sha256=expected_source_hash,
        evidence_json_hash=expected_evidence_json_hash),
        stpd_gate_b_v3_phase1_bundle())
      expected_estimator_id <- unique(family$local_estimator_registry_id)
      if (evidence_rows$evidence_json[[1L]] != expected_evidence_json ||
          evidence_rows$evidence_json_hash[[1L]] !=
            expected_evidence_json_hash ||
          evidence_rows$source_bytes_sha256[[1L]] != expected_source_hash ||
          evidence_rows$evidence_id[[1L]] != expected_evidence_id ||
          stpd_gate_b_v3_registry_code(
            tables, evidence_rows$evidence_schema_registry_id[[1L]]) !=
            "gap_empirical_family_v1" ||
          length(expected_estimator_id) != 1L ||
          evidence_rows$created_by_registry_entry_id[[1L]] !=
            expected_estimator_id[[1L]] ||
          any(family$evidence_id != expected_evidence_id)) {
        stop("Gate B v3 Gap family evidence payload is incomplete.",call.=FALSE)
      }
      for (j in seq_len(nrow(family))) {
        absolute_threshold <- stpd_gate_b_v3_bound_threshold_value(
          tables,"gap_candidate",family$gap_candidate_id[[j]],
          "pause.absolute_threshold_sec")
        alpha_threshold <- stpd_gate_b_v3_bound_threshold_value(
          tables,"gap_candidate",family$gap_candidate_id[[j]],
          "pause.score_alpha")
        ratio_threshold <- stpd_gate_b_v3_bound_threshold_value(
          tables,"gap_candidate",family$gap_candidate_id[[j]],
          "pause.local_ratio_min")
        expected_decision <- if (isTRUE(family$boundary_censored[[j]]) ||
            expected_qc[[j]]$status == "ineligible" ||
            !expected_qc[[j]]$hard_boundary_clear ||
            is.na(expected_q[[j]])) "abstain" else if (
              family$duration_sec[[j]]>=absolute_threshold &&
                expected_ratio[[j]]>=ratio_threshold &&
                expected_q[[j]]<=alpha_threshold) "accepted_pause" else "rejected"
        expected_reason <- if (expected_decision == "accepted_pause") {
          "accepted_evidence_pass_v1"
        } else if (expected_decision == "rejected") {
          "rejected_pause_threshold_v1"
        } else if (isTRUE(family$boundary_censored[[j]]) ||
                   expected_qc[[j]]$status == "ineligible" ||
                   !expected_qc[[j]]$hard_boundary_clear) {
          "abstained_pause_qc_v1"
        } else {
          "abstained_pause_insufficient_v1"
        }
        observed_reason <- stpd_gate_b_v3_registry_code(
          tables, family$decision_reason_registry_id[[j]])
        if (family$decision[[j]]!=expected_decision ||
            observed_reason != expected_reason) {
          stop("Gate B v3 Gap decision differs from frozen threshold evidence.",
               call.=FALSE)
        }
      }
    }
    complete <- !is.na(candidates$raw_p_value) &
      !is.na(candidates$adjusted_q_value)
    if (any(candidates$raw_p_value[complete] < 0 |
            candidates$raw_p_value[complete] > 1 |
            candidates$adjusted_q_value[complete] <
              candidates$raw_p_value[complete] |
            candidates$adjusted_q_value[complete] > 1)) {
      stop("Gate B v3 Gap multiple-testing values are impossible.",
           call. = FALSE)
    }
  }
  abstained <- candidates[candidates$decision == "abstain",,drop = FALSE]
  abstention_rows <- tables$gap_abstentions
  if (nrow(abstention_rows) != nrow(abstained) ||
      anyDuplicated(abstention_rows$gap_candidate_id) ||
      !setequal(abstention_rows$gap_candidate_id,
                abstained$gap_candidate_id)) {
    stop("Gate B v3 Gap-abstention expected set is incomplete.",
         call. = FALSE)
  }
  if (nrow(abstained) > 0L) {
    abstained <- abstained[match(abstention_rows$gap_candidate_id,
                                 abstained$gap_candidate_id),,drop = FALSE]
    expected_ids <- vapply(seq_len(nrow(abstention_rows)), function(i) {
      stpd_gate_b_v3_entity_id("gap_abstention", list(
        detection_root_id=abstained$detection_root_id[[i]],
        gap_candidate_id=abstained$gap_candidate_id[[i]],
        evidence_id=abstained$evidence_id[[i]]),
        stpd_gate_b_v3_phase1_bundle())
    }, character(1))
    if (!identical(abstention_rows$gap_abstention_id, expected_ids) ||
        !identical(abstention_rows$train, abstained$train) ||
        !identical(abstention_rows$start_isi, abstained$start_isi) ||
        !identical(abstention_rows$end_isi, abstained$end_isi) ||
        !identical(abstention_rows$reason_registry_id,
                   abstained$decision_reason_registry_id) ||
        !identical(abstention_rows$evidence_id, abstained$evidence_id)) {
      stop("Gate B v3 Gap-abstention projection is not canonical.",
           call. = FALSE)
    }
  }
  accepted <- candidates[candidates$decision == "accepted_pause",,
                         drop = FALSE]
  if (nrow(accepted) != nrow(gaps) ||
      anyDuplicated(gaps$gap_candidate_id) ||
      !setequal(accepted$gap_candidate_id, gaps$gap_candidate_id)) {
    stop("Gate B v3 accepted Pause candidate projection is incomplete.",
         call. = FALSE)
  }
  if (nrow(gaps) > 0L) {
    accepted <- accepted[match(gaps$gap_candidate_id,
                               accepted$gap_candidate_id),, drop = FALSE]
    fields <- c("train", "start_isi", "end_isi", "n_isi",
                "start_time_sec", "end_time_sec", "duration_sec",
                "valid_local_n_isi", "local_reference_sec", "local_ratio",
                "raw_p_value", "adjusted_q_value",
                "multiple_testing_method_registry_id")
    for (field in fields) {
      if (!stpd_gate_b_v3_same_nullable(gaps[[field]], accepted[[field]])) {
        stop("Gate B v3 accepted Pause differs from its Gap candidate: ",
             field, call. = FALSE)
      }
    }
    for (i in seq_len(nrow(gaps))) {
      candidate <- accepted[i,,drop = FALSE]
      qc <- stpd_gate_b_v3_pause_qc_status(tables, candidate)
      absolute_threshold <- stpd_gate_b_v3_bound_threshold_value(
        tables, "gap_candidate", candidate$gap_candidate_id[[1L]],
        "pause.absolute_threshold_sec")
      expected_surprise <- -log10(max(
        candidate$adjusted_q_value[[1L]],
        2.2250738585072014e-308))
      if (gaps$gap_class[[i]] != "canonical_pause" ||
          gaps$gap_semantics[[i]] != "predicted_statistical_pause" ||
          gaps$gap_evidence_status[[i]] != "pass" ||
          gaps$n_spikes[[i]] != 2L ||
          !identical(gaps$absolute_threshold_sec[[i]], absolute_threshold) ||
          !identical(gaps$surprise_score[[i]], expected_surprise) ||
          gaps$qc_eligibility[[i]] != qc$status ||
          gaps$gap_evidence_id[[i]] != candidate$evidence_id[[1L]]) {
        stop("Gate B v3 accepted Pause evidence is not the exact canonical projection.",
             call. = FALSE)
      }
    }
  }
  invisible(TRUE)
}

stpd_gate_b_v3_validate_patient_holdout <- function(tables) {
  memberships <- tables$partition_memberships
  if (nrow(memberships) == 0L) return(invisible(TRUE))
  manifest <- tables$normalized_input_manifest
  for (i in seq_len(nrow(memberships))) {
    source <- manifest$normalized_timestamp_bytes_sha256 ==
      memberships$source_input_hash[[i]]
    if (sum(source) != 1L ||
        !stpd_gate_b_v3_same_nullable(
          memberships$patient_group_id_hash[i],
          manifest$patient_group_id_hash[source]) ||
        !identical(memberships$session_group_id_hash[[i]],
                   manifest$session_group_id_hash[source][[1L]])) {
      stop("Gate B v3 partition membership is not bound to its normalized patient/session source row.",
           call. = FALSE)
    }
  }
  relevant <- memberships$group_role %in%
    c("development", "calibration", "validation")
  crosses_roles <- function(groups, roles) {
    grouped <- split(roles, groups)
    any(vapply(grouped, function(value) {
      "validation" %in% value &&
        any(value %in% c("development", "calibration"))
    }, logical(1)))
  }
  session_crossed <- crosses_roles(
    memberships$session_group_id_hash[relevant],
    memberships$group_role[relevant])
  patient_rows <- relevant & !is.na(memberships$patient_group_id_hash)
  patient_crossed <- if (any(patient_rows)) crosses_roles(
    memberships$patient_group_id_hash[patient_rows],
    memberships$group_role[patient_rows]) else FALSE
  if (session_crossed || patient_crossed) {
    stop("Gate B v3 held-out validation shares a patient or session with development/calibration.",
         call. = FALSE)
  }
  invisible(TRUE)
}

stpd_gate_b_v3_structure_first_paths <- function() {
  c(
    event_min="event_core.min_spikes",
    event_max="event_core.classic_max_spikes",
    artifact_floor="detector.artifact_min_valid_isi_sec",
    enabled="spiketrainpattern.burst.structure_first_enabled",
    min_n="spiketrainpattern.burst.structure_first_min_isi_count",
    max_n="spiketrainpattern.burst.structure_first_max_isi_count",
    contrast="spiketrainpattern.burst.structure_first_contrast_min",
    geom="spiketrainpattern.burst.structure_first_geom_contrast_min",
    quantile="spiketrainpattern.burst.structure_first_compactness_quantile",
    background="spiketrainpattern.burst.structure_first_background_fraction",
    min_train="spiketrainpattern.burst.structure_first_min_train_valid_isi",
    tail="spiketrainpattern.burst.structure_first_max_internal_tail_ratio",
    endpoint="spiketrainpattern.burst.structure_first_allow_endpoint",
    endpoint_canonical="spiketrainpattern.burst.allow_one_sided_as_canonical")
}

stpd_gate_b_v3_structure_first_rule_id <- function(tables) {
  rows <- tables$contract_registries[
    tables$contract_registries$registry_domain == "event_candidate_rule" &
      tables$contract_registries$code ==
        "structure_first_local_flank_contrast_v1" &
      tables$contract_registries$active,,drop=FALSE]
  if (nrow(rows) != 1L) {
    stop("Gate B v3 cannot resolve the frozen structure-first Event rule.",
         call.=FALSE)
  }
  rows$registry_entry_id[[1L]]
}

stpd_gate_b_v3_structure_first_params <- function(tables, train) {
  paths <- stpd_gate_b_v3_structure_first_paths()
  value <- vapply(paths, function(path) {
    stpd_gate_b_v3_applicable_threshold_value(tables,path,train)
  }, numeric(1))
  params <- default_params_sec()
  params$event_core$min_spikes <- as.integer(value[["event_min"]])
  params$event_core$classic_max_spikes <- as.integer(value[["event_max"]])
  params$spiketrainpattern$burst$structure_first_enabled <-
    identical(value[["enabled"]],1)
  params$spiketrainpattern$burst$structure_first_min_isi_count <-
    as.integer(value[["min_n"]])
  params$spiketrainpattern$burst$structure_first_max_isi_count <-
    as.integer(value[["max_n"]])
  params$spiketrainpattern$burst$structure_first_contrast_min <-
    value[["contrast"]]
  params$spiketrainpattern$burst$structure_first_geom_contrast_min <-
    value[["geom"]]
  params$spiketrainpattern$burst$structure_first_compactness_quantile <-
    value[["quantile"]]
  params$spiketrainpattern$burst$structure_first_background_fraction <-
    value[["background"]]
  params$spiketrainpattern$burst$structure_first_min_train_valid_isi <-
    as.integer(value[["min_train"]])
  params$spiketrainpattern$burst$structure_first_max_internal_tail_ratio <-
    value[["tail"]]
  params$spiketrainpattern$burst$structure_first_allow_endpoint <-
    identical(value[["endpoint"]],1)
  params$spiketrainpattern$burst$allow_one_sided_as_canonical <-
    identical(value[["endpoint_canonical"]],1)
  list(params=params,min_isi_sec=value[["artifact_floor"]])
}

stpd_gate_b_v3_structure_first_expected_candidates <- function(tables) {
  spine <- tables$per_isi
  rule_id <- stpd_gate_b_v3_structure_first_rule_id(tables)
  bundle <- stpd_gate_b_v3_phase1_bundle()
  columns <- c(
    "event_candidate_id","train","candidate_generation_rule_registry_id",
    "source_candidate_key_hash","candidate_class","start_isi","end_isi",
    "n_isi","n_spikes","start_time_sec","end_time_sec","duration_sec",
    "candidate_decision")
  rows <- list()
  for (train in sort(unique(spine$train),method="radix")) {
    train_spine <- spine[spine$train == train,,drop=FALSE]
    train_spine <- train_spine[order(train_spine$isi_index,method="radix"),,
                               drop=FALSE]
    n <- nrow(train_spine)
    if (n == 0L || !identical(train_spine$isi_index,seq_len(n))) {
      stop("Gate B v3 structure-first replay requires a contiguous per-ISI spine.",
           call.=FALSE)
    }
    dat <- data.frame(
      idx=seq_len(n+1L),
      timestamp_sec=c(train_spine$isi_start_time_sec[[1L]],
                      train_spine$isi_end_time_sec),
      ISI_sec=c(NA_real_,train_spine$isi_sec),
      pattern_manual=rep("",n+1L),
      pattern_manual_negative=rep("",n+1L),
      pattern_auto=rep("",n+1L),
      stringsAsFactors=FALSE)
    frozen <- stpd_gate_b_v3_structure_first_params(tables,train)
    vp <- stpd_event_core_params_impl(
      dat,frozen$params,min_isi_sec=frozen$min_isi_sec)
    generated <- stpd_event_core_structure_first_burst_candidates(
      dat,frozen$params,vp,min_isi_sec=frozen$min_isi_sec,train=train)
    if (nrow(generated) == 0L) next
    for (i in seq_len(nrow(generated))) {
      start_isi <- as.integer(generated$start_isi[[i]])-1L
      end_isi <- as.integer(generated$end_isi[[i]])-1L
      if (start_isi < 1L || end_isi > n || start_isi > end_isi) {
        stop("Gate B v3 production structure-first generator returned invalid geometry.",
             call.=FALSE)
      }
      source_hash <- stpd_gate_b_v3_hash_raw(
        "stpd-structure-first-source-candidate-key-v1",
        stpd_gate_b_v3_canonical_json(list(
          train=train,start_isi=start_isi,end_isi=end_isi,
          candidate_generation_rule_registry_id=rule_id)))
      candidate_id <- stpd_gate_b_v3_entity_id("event_candidate",list(
        detection_root_id=train_spine$detection_root_id[[1L]],train=train,
        start_isi=start_isi,end_isi=end_isi,
        candidate_generation_rule_registry_id=rule_id,
        source_candidate_key_hash=source_hash),bundle)
      decision <- switch(as.character(generated$final_label[[i]]),
        burst="accepted_event", long_burst="accepted_event",
        high_frequency_burst="accepted_event",
        possible_burst="review_candidate", reject="rejected",
        stop("Gate B v3 production structure-first generator returned an unknown decision.",
             call.=FALSE))
      selected <- train_spine$isi_index >= start_isi &
        train_spine$isi_index <= end_isi
      rows[[length(rows)+1L]] <- data.frame(
        event_candidate_id=candidate_id,train=train,
        candidate_generation_rule_registry_id=rule_id,
        source_candidate_key_hash=source_hash,candidate_class="burst_candidate",
        start_isi=start_isi,end_isi=end_isi,
        n_isi=as.integer(end_isi-start_isi+1L),
        n_spikes=as.integer(end_isi-start_isi+2L),
        start_time_sec=train_spine$isi_start_time_sec[train_spine$isi_index==start_isi],
        end_time_sec=train_spine$isi_end_time_sec[train_spine$isi_index==end_isi],
        duration_sec=stpd_gate_b_v3_sum_binary64(train_spine$isi_sec[selected]),
        candidate_decision=decision,stringsAsFactors=FALSE)
    }
  }
  if (length(rows) == 0L) {
    observed <- tables$event_candidates[FALSE,columns,drop=FALSE]
    return(observed)
  }
  expected <- do.call(rbind,rows)
  expected <- expected[order(expected$train,expected$start_isi,
                             expected$end_isi,expected$event_candidate_id,
                             method="radix"),columns,drop=FALSE]
  rownames(expected) <- NULL
  expected
}

stpd_gate_b_v3_validate_structure_first_event_contract <- function(tables) {
  candidates <- tables$event_candidates
  columns <- c(
    "event_candidate_id","train","candidate_generation_rule_registry_id",
    "source_candidate_key_hash","candidate_class","start_isi","end_isi",
    "n_isi","n_spikes","start_time_sec","end_time_sec","duration_sec",
    "candidate_decision")
  observed <- candidates[order(candidates$train,candidates$start_isi,
                               candidates$end_isi,candidates$event_candidate_id,
                               method="radix"),columns,drop=FALSE]
  rownames(observed) <- NULL
  expected <- stpd_gate_b_v3_structure_first_expected_candidates(tables)
  if (!identical(observed,expected)) {
    stop("Gate B v3 structure-first Event candidate universe differs from parent replay.",
         call.=FALSE)
  }
  if (nrow(candidates) == 0L) return(invisible(TRUE))
  paths <- stpd_gate_b_v3_structure_first_paths()
  for (i in seq_len(nrow(candidates))) {
    candidate <- candidates[i,,drop=FALSE]
    rule <- stpd_gate_b_v3_registry_code(
      tables,candidate$candidate_generation_rule_registry_id[[1L]])
    if (!identical(rule,"structure_first_local_flank_contrast_v1")) {
      stop("Gate B v3 Event candidate is not generated by the frozen structure-first contrast rule.",
           call.=FALSE)
    }
    bounds <- stpd_gate_b_v3_exact_bound_paths(
      tables,"event_candidate",candidate$event_candidate_id[[1L]],
      unname(paths))
    value <- setNames(unname(bounds[unname(paths)]),names(paths))
    binary <- function(x) is.finite(x) && x %in% c(0,1)
    whole <- function(x) is.finite(x) && x==floor(x)
    if (!whole(value[["event_min"]]) || value[["event_min"]] < 2 ||
        !whole(value[["event_max"]]) ||
        value[["event_max"]] < value[["event_min"]] ||
        !is.finite(value[["artifact_floor"]]) || value[["artifact_floor"]] <= 0 ||
        !binary(value[["enabled"]]) || value[["enabled"]] != 1 ||
        !whole(value[["min_n"]]) || !whole(value[["max_n"]]) ||
        value[["min_n"]] < 3 || value[["max_n"]] < value[["min_n"]] ||
        !is.finite(value[["contrast"]]) || value[["contrast"]] < 1 ||
        !is.finite(value[["geom"]]) || value[["geom"]] < 1 ||
        !is.finite(value[["quantile"]]) || value[["quantile"]] < .5 ||
        value[["quantile"]] > 1 ||
        !is.finite(value[["background"]]) || value[["background"]] <= 0 ||
        value[["background"]] > 1 ||
        !whole(value[["min_train"]]) || value[["min_train"]] < 1 ||
        !is.finite(value[["tail"]]) || value[["tail"]] < 1 ||
        !binary(value[["endpoint"]]) ||
        !binary(value[["endpoint_canonical"]])) {
      stop("Gate B v3 structure-first Burst parameter contract is invalid.",
           call.=FALSE)
    }
  }
  invisible(TRUE)
}

stpd_gate_b_v3_validate_threshold_binding_completeness <- function(tables) {
  state_paths <- c(
    "state.frequency.high_enter_hz", "state.frequency.high_exit_hz",
    "state.regularity.regular_upper_cv2",
    "state.regularity.irregular_lower_cv2",
    "state.state_min_valid_isi", "state.state_min_duration_sec")
  event_candidate_paths <- c(
    "event_core.min_spikes",
    "event_core.classic_max_spikes",
    "detector.artifact_min_valid_isi_sec",
    "spiketrainpattern.burst.structure_first_enabled",
    "spiketrainpattern.burst.structure_first_min_isi_count",
    "spiketrainpattern.burst.structure_first_max_isi_count",
    "spiketrainpattern.burst.structure_first_contrast_min",
    "spiketrainpattern.burst.structure_first_geom_contrast_min",
    "spiketrainpattern.burst.structure_first_compactness_quantile",
    "spiketrainpattern.burst.structure_first_background_fraction",
    "spiketrainpattern.burst.structure_first_min_train_valid_isi",
    "spiketrainpattern.burst.structure_first_max_internal_tail_ratio",
    "spiketrainpattern.burst.structure_first_allow_endpoint",
    "spiketrainpattern.burst.allow_one_sided_as_canonical")
  event_paths <- "event.selector.minimum_score"
  pause_paths <- c(
    "pause.scan_seed_sec", "pause.absolute_threshold_sec",
    "pause.local_ratio_min", "pause.local_window_isi",
    "pause.guard_band_isi", "pause.local_min_valid_isi",
    "pause.score_alpha")
  required <- list(
    state_candidate=list(ids=tables$state_candidates$state_candidate_id,
                         paths=state_paths),
    state_axis_evidence=list(ids=tables$state_axis_evidence$state_axis_evidence_id,
                             paths=state_paths),
    state_episode=list(ids=tables$state_episodes$state_episode_id,
                       paths=state_paths),
    event_candidate=list(ids=tables$event_candidates$event_candidate_id,
                         paths=event_candidate_paths),
    event=list(ids=tables$events$event_id,paths=event_paths),
    gap_candidate=list(ids=tables$gap_candidates$gap_candidate_id,
                       paths=pause_paths),
    gap=list(ids=tables$gaps$gap_id,paths=pause_paths))
  for (domain in names(required)) {
    ids <- unique(required[[domain]]$ids)
    for (id in ids) {
      stpd_gate_b_v3_exact_bound_paths(
        tables, domain, id, required[[domain]]$paths)
    }
  }
  modifiers <- tables$event_modifier_evidence
  if (nrow(modifiers) > 0L) {
    for (i in seq_len(nrow(modifiers))) {
      paths <- if (modifiers$modifier_domain[[i]] == "extent") c(
        "event.extent.long_min_spikes",
        "event.extent.prolonged_min_spikes") else c(
          "event.frequency.high_enter_hz",
          "event.frequency.high_exit_hz")
      stpd_gate_b_v3_exact_bound_paths(
        tables, "event_modifier_evidence",
        modifiers$modifier_evidence_id[[i]], paths)
    }
  }
  invisible(TRUE)
}

stpd_gate_b_v3_validate_entity_evidence_science <- function(tables) {
  records <- tables$evidence_records
  registry <- tables$contract_registries
  schema_code <- function(record) registry$code[match(
    record$evidence_schema_registry_id,registry$registry_entry_id)]

  for (i in seq_len(nrow(tables$state_episodes))) {
    episode <- tables$state_episodes[i,,drop=FALSE]
    record <- records[records$evidence_id==
      episode$classification_evidence_id[[1L]],,drop=FALSE]
    if (nrow(record)!=1L || schema_code(record)!="state_axis_v1") {
      stop("Gate B v3 State episode lacks canonical orthogonal-axis evidence.",
           call.=FALSE)
    }
    direct <- tables$state_segments[
      tables$state_segments$state_episode_id==episode$state_episode_id[[1L]] &
        tables$state_segments$segment_role=="direct_support",,drop=FALSE]
    if (nrow(direct)==0L) {
      stop("Gate B v3 State episode has no direct-support evidence.",call.=FALSE)
    }
    direct_values <- list()
    pair_values <- list()
    for (j in seq_len(nrow(direct))) {
      hit <- stpd_gate_b_v3_interval_mask(tables$per_isi,direct$train[[j]],
        direct$start_isi[[j]],direct$end_isi[[j]])
      values <- tables$per_isi$isi_sec[hit]
      direct_values[[j]] <- values
      if (length(values)>1L) pair_values[[j]] <-
        2*abs(diff(values))/(values[-length(values)]+values[-1L])
    }
    values <- unlist(direct_values,use.names=FALSE)
    pairs <- unlist(pair_values,use.names=FALSE)
    expected_frequency <- length(values)/stpd_gate_b_v3_sum_binary64(values)
    expected_regularity <- if (length(pairs)==0L) NA_real_ else
      stats::median(pairs)
    domain_id <- stpd_gate_b_v3_domain_id(tables,"state_episode")
    threshold_ids <- sort(tables$threshold_instance_bindings$threshold_instance_id[
      tables$threshold_instance_bindings$consumer_domain_id==domain_id &
        tables$threshold_instance_bindings$consumer_id==
          episode$state_episode_id[[1L]]],method="radix")
    expected_json <- stpd_gate_b_v3_canonical_json(list(
      frequency_stat=expected_frequency,
      frequency_status=episode$frequency_evidence_status[[1L]],
      regularity_stat=expected_regularity,
      regularity_status=episode$regularity_evidence_status[[1L]],
      threshold_instance_ids=as.list(threshold_ids)))
    if (record$evidence_json[[1L]]!=expected_json) {
      stop("Gate B v3 State classification evidence differs from direct-support recomputation.",
           call.=FALSE)
    }
  }

  accepted <- tables$event_candidates[
    tables$event_candidates$candidate_decision=="accepted_event",,
    drop=FALSE]
  for (i in seq_len(nrow(tables$events))) {
    event <- tables$events[i,,drop=FALSE]
    key <- accepted$train==event$train[[1L]] &
      accepted$start_isi==event$start_isi[[1L]] &
      accepted$end_isi==event$end_isi[[1L]]
    candidate <- accepted[key,,drop=FALSE]
    record <- records[records$evidence_id==event$base_event_evidence_id[[1L]],,
                      drop=FALSE]
    if (nrow(candidate)!=1L || nrow(record)!=1L ||
        candidate$base_event_evidence_id[[1L]]!=record$evidence_id[[1L]] ||
        schema_code(record)!="entity_support_v1") {
      stop("Gate B v3 accepted Event lacks canonical source-candidate evidence.",
           call.=FALSE)
    }
    expected_json <- stpd_gate_b_v3_canonical_json(list(
      entity_domain_id=stpd_gate_b_v3_domain_id(tables,"event_candidate"),
      entity_id=candidate$event_candidate_id[[1L]],
      support_rows=as.list(seq.int(event$start_isi[[1L]],
                                   event$end_isi[[1L]]))))
    if (record$evidence_json[[1L]]!=expected_json) {
      stop("Gate B v3 Event source-candidate evidence payload is invalid.",
           call.=FALSE)
    }
  }

  for (i in seq_len(nrow(tables$event_modifier_evidence))) {
    modifier <- tables$event_modifier_evidence[i,,drop=FALSE]
    event <- tables$events[
      tables$events$event_id == modifier$event_id[[1L]],,drop = FALSE]
    record <- records[records$evidence_id==modifier$evidence_id[[1L]],,
                      drop=FALSE]
    if (nrow(event) != 1L || nrow(record)!=1L ||
        schema_code(record)!="event_modifier_v1") {
      stop("Gate B v3 Event modifier evidence payload is invalid.",call.=FALSE)
    }
    rate <- event$n_isi[[1L]] / event$duration_sec[[1L]]
    if (modifier$modifier_domain[[1L]] == "extent") {
      paths <- c("event.extent.long_min_spikes",
                 "event.extent.prolonged_min_spikes")
      bounds <- stpd_gate_b_v3_exact_bound_paths(
        tables, "event_modifier_evidence",
        modifier$modifier_evidence_id[[1L]], paths)
      long_min <- bounds[[paths[[1L]]]]
      prolonged_min <- bounds[[paths[[2L]]]]
      if (long_min != floor(long_min) || prolonged_min != floor(prolonged_min) ||
          long_min < 2 || prolonged_min <= long_min) {
        stop("Gate B v3 Event extent thresholds are invalid.", call. = FALSE)
      }
      expected_stat <- as.double(event$n_spikes[[1L]])
      expected_value <- if (expected_stat >= prolonged_min) "prolonged" else
        if (expected_stat >= long_min) "long" else "classic"
      expected_bound <- if (expected_value == "prolonged") prolonged_min else
        long_min
      expected_margin <- 0
      estimator_code <- "event_extent_n_spikes_v1"
      threshold_path <- if (expected_value == "prolonged") paths[[2L]] else
        paths[[1L]]
      expected_status <- "pass"
    } else if (modifier$modifier_domain[[1L]] == "frequency") {
      paths <- c("event.frequency.high_enter_hz",
                 "event.frequency.high_exit_hz")
      bounds <- stpd_gate_b_v3_exact_bound_paths(
        tables, "event_modifier_evidence",
        modifier$modifier_evidence_id[[1L]], paths)
      high_enter <- bounds[[paths[[1L]]]]
      high_exit <- bounds[[paths[[2L]]]]
      if (!(high_exit < high_enter) || high_exit < 0) {
        stop("Gate B v3 Event frequency thresholds are invalid.", call. = FALSE)
      }
      expected_stat <- rate
      expected_value <- if (rate >= high_enter) "high" else
        if (rate <= high_exit) "non_high" else "unresolved"
      expected_bound <- if (expected_value == "non_high") high_exit else
        high_enter
      expected_margin <- high_enter - high_exit
      estimator_code <- "event_frequency_rate_hz_v1"
      threshold_path <- if (expected_value == "non_high") paths[[2L]] else
        paths[[1L]]
      expected_status <- if (expected_value == "unresolved") "gray_zone" else
        "pass"
    } else {
      stop("Gate B v3 Event modifier domain is unsupported.", call. = FALSE)
    }
    if (stpd_gate_b_v3_registry_code(
        tables, modifier$estimator_registry_id[[1L]]) != estimator_code) {
      stop("Gate B v3 Event modifier uses the wrong frozen estimator.",
           call. = FALSE)
    }
    domain_id <- stpd_gate_b_v3_domain_id(tables, "event_modifier_evidence")
    binding <- tables$threshold_instance_bindings[
      tables$threshold_instance_bindings$consumer_domain_id == domain_id &
        tables$threshold_instance_bindings$consumer_id ==
          modifier$modifier_evidence_id[[1L]] &
        tables$threshold_instance_bindings$path == threshold_path,,
      drop = FALSE]
    expected_threshold_id <- if (nrow(binding) == 1L)
      binding$threshold_instance_id[[1L]] else NA_character_
    payload <- list(
      event_id=modifier$event_id[[1L]],
      modifier_domain=modifier$modifier_domain[[1L]],
      modifier_value=expected_value,
      evidence_status=expected_status,
      estimator_registry_id=modifier$estimator_registry_id[[1L]],
      statistic_value=expected_stat,
      pass_bound=expected_bound,
      gray_margin=expected_margin,
      threshold_instance_id=expected_threshold_id)
    expected_json <- stpd_gate_b_v3_canonical_json(payload)
    expected_source_hash <- stpd_gate_b_v3_hash_raw(
      "stpd-event-modifier-source-v1", expected_json)
    expected_json_hash <- stpd_gate_b_v3_hash_raw(
      "stpd-evidence-json-v1", stpd_gate_b_v3_canonical_json(list(
        evidence_schema_registry_id=record$evidence_schema_registry_id[[1L]],
        evidence_json=payload)))
    expected_record_id <- stpd_gate_b_v3_entity_id("evidence", list(
      detection_root_id=modifier$detection_root_id[[1L]],
      evidence_type_registry_id=record$evidence_type_registry_id[[1L]],
      evidence_schema_registry_id=record$evidence_schema_registry_id[[1L]],
      source_bytes_sha256=expected_source_hash,
      evidence_json_hash=expected_json_hash), stpd_gate_b_v3_phase1_bundle())
    expected_modifier_id <- stpd_gate_b_v3_entity_id(
      "event_modifier_evidence", list(
        detection_root_id=modifier$detection_root_id[[1L]],
        event_id=modifier$event_id[[1L]],
        modifier_domain=modifier$modifier_domain[[1L]],
        evidence_id=expected_record_id), stpd_gate_b_v3_phase1_bundle())
    event_value <- if (modifier$modifier_domain[[1L]] == "extent")
      event$extent_modifier[[1L]] else event$frequency_modifier[[1L]]
    event_status <- if (modifier$modifier_domain[[1L]] == "extent")
      event$extent_evidence_status[[1L]] else
        event$frequency_evidence_status[[1L]]
    if (nrow(binding) != 1L ||
        !identical(modifier$modifier_value[[1L]], expected_value) ||
        !identical(modifier$evidence_status[[1L]], expected_status) ||
        !identical(modifier$statistic_value[[1L]], expected_stat) ||
        !identical(modifier$pass_bound[[1L]], expected_bound) ||
        !identical(modifier$gray_margin[[1L]], expected_margin) ||
        !identical(modifier$threshold_instance_id[[1L]], expected_threshold_id) ||
        !identical(record$evidence_json[[1L]], expected_json) ||
        !identical(record$source_bytes_sha256[[1L]], expected_source_hash) ||
        !identical(record$evidence_json_hash[[1L]], expected_json_hash) ||
        !identical(record$evidence_id[[1L]], expected_record_id) ||
        !identical(record$created_by_registry_entry_id[[1L]],
                   modifier$estimator_registry_id[[1L]]) ||
        !identical(modifier$evidence_id[[1L]], expected_record_id) ||
        !identical(modifier$modifier_evidence_id[[1L]], expected_modifier_id) ||
        !identical(event_value, expected_value) ||
        !identical(event_status, expected_status)) {
      stop("Gate B v3 Event modifier statistics or content identity differ from deterministic recomputation.",
           call. = FALSE)
    }
  }
  invisible(TRUE)
}

stpd_gate_b_v3_validate_threshold_evidence_science <- function(tables) {
  instances <- tables$threshold_instances
  source <- tables$threshold_instance_source_manifest
  policies <- tables$threshold_policies
  evidence <- tables$evidence_records
  registry <- tables$contract_registries
  expected_payload <- list(statistics_schema = "threshold_instance_source_v1",
                           statistics = list())
  expected_json <- stpd_gate_b_v3_canonical_json(expected_payload)
  for (i in seq_len(nrow(instances))) {
    instance <- instances[i, , drop = FALSE]
    keys <- source$path == instance$path[[1L]] &
      source$application_scope == instance$application_scope[[1L]] &
      source$source_scope_key_hash == instance$source_scope_key_hash[[1L]]
    if (sum(keys) != 1L) {
      stop("Gate B v3 threshold instance does not resolve one source-manifest row.",
           call. = FALSE)
    }
    source_row <- source[keys, , drop = FALSE]
    policy <- policies[policies$threshold_policy_id ==
                         instance$threshold_policy_id[[1L]],, drop = FALSE]
    record <- evidence[evidence$evidence_id ==
                         instance$instance_evidence_id[[1L]],, drop = FALSE]
    if (nrow(policy) != 1L || nrow(record) != 1L) {
      stop("Gate B v3 threshold evidence FK closure is incomplete.",
           call. = FALSE)
    }
    algorithm_index <- match(policy$threshold_algorithm_registry_id[[1L]],
                             registry$registry_entry_id)
    if (is.na(algorithm_index) ||
        registry$registry_domain[[algorithm_index]] != "threshold_algorithm") {
      stop("Gate B v3 threshold policy uses an invalid algorithm registry row.",
           call. = FALSE)
    }
    algorithm_code <- registry$code[[algorithm_index]]
    # Phase 1 implements and validates the fixed/preregistered evidence
    # contract.  Adaptive algorithms remain typed unsupported until their
    # exact statistics schema and recomputation backend are implemented.
    if (algorithm_code != "fixed_preregistered_v1") {
      stop("Gate B v3 adaptive threshold evidence is not implemented in phase 1.",
           call. = FALSE)
    }
    if (!is.na(source_row$derivation_statistics_schema_registry_id[[1L]]) ||
        !is.na(source_row$derivation_statistics_json[[1L]]) ||
        !is.na(source_row$application_statistics_schema_registry_id[[1L]]) ||
        !is.na(source_row$application_statistics_json[[1L]])) {
      stop("Gate B v3 fixed threshold evidence may not contain adaptive statistics.",
           call. = FALSE)
    }
    evidence_schema_code <- registry$code[
      match(record$evidence_schema_registry_id, registry$registry_entry_id)]
    if (!identical(record$source_bytes_sha256[[1L]],
                   source_row$instance_evidence_source_hash[[1L]]) ||
        !identical(record$created_by_registry_entry_id[[1L]],
                   policy$threshold_algorithm_registry_id[[1L]]) ||
        !identical(evidence_schema_code[[1L]], "application_statistics_v1") ||
        !identical(record$evidence_json[[1L]], expected_json)) {
      stop("Gate B v3 threshold instance is not bound to its exact evidence payload.",
           call. = FALSE)
    }
  }
  invisible(TRUE)
}

#' Validate a complete Gate B v3 canonical product prototype
#' @inheritParams .stpd_gate_b_v3_validate_product_prototype_structural
#' @return Invisible TRUE; otherwise fails closed.
#' @export
stpd_gate_b_v3_validate_product_prototype <- function(
    tables, bundle = stpd_gate_b_v3_phase1_bundle()) {
  .stpd_gate_b_v3_validate_product_prototype_structural(tables, bundle)
  stpd_gate_b_v3_validate_state_axis_science(tables)
  stpd_gate_b_v3_validate_projection_science(tables)
  stpd_gate_b_v3_validate_relationship_science(tables)
  stpd_gate_b_v3_validate_gap_science(tables)
  stpd_gate_b_v3_validate_patient_holdout(tables)
  stpd_gate_b_v3_validate_threshold_evidence_science(tables)
  stpd_gate_b_v3_validate_structure_first_event_contract(tables)
  stpd_gate_b_v3_validate_threshold_binding_completeness(tables)
  stpd_gate_b_v3_validate_entity_evidence_science(tables)
  invisible(TRUE)
}
