# Gate 1B final Gap arbitration/materialization observation ----------------
#
# This is an observer-only root around the existing protected weighted-
# interval arbitration and legacy pattern_auto projection. It does not add a
# threshold, candidate, veto, State, or product label.

stpd_candidate_lineage_gap_final_hooks <- function() c(
  gap_final_entry_v1 = "entry",
  gap_final_candidate_input_v1 = "candidate_input",
  gap_final_arbitration_v1 = "arbitration",
  gap_final_materialized_output_v1 = "materialized",
  gap_final_receipt_v1 = "receipt"
)

stpd_candidate_lineage_gap_final_hash <- function(x, stage) {
  stpd_threshold_first_hash_domain(paste0("stpd-gap-final-", stage, "-v1"), x)
}

stpd_candidate_lineage_gap_final_action <- function(x) {
  if ("action" %in% names(x)) as.character(x$action) else if (
    "decision_action" %in% names(x)) as.character(x$decision_action) else
      rep("accept", nrow(x))
}

stpd_candidate_lineage_gap_final_match_contextual <- function(pre_audit, combined) {
  out <- rep(NA_integer_, nrow(pre_audit))
  if (!nrow(pre_audit) || is.null(combined) || !nrow(combined)) return(out)
  ids <- as.character(pre_audit$candidate_id %||% rep("", nrow(pre_audit)))
  starts <- suppressWarnings(as.integer(pre_audit$start_isi))
  ends <- suppressWarnings(as.integer(pre_audit$end_isi))
  used <- rep(FALSE, nrow(pre_audit))
  for (j in seq_len(nrow(combined))) {
    hit <- which(!used & ids == as.character(combined$candidate_id[[j]]) &
      starts == as.integer(combined$detector_start_row[[j]]) &
      ends == as.integer(combined$detector_end_row[[j]]))
    if (length(hit)) {
      out[[hit[[1L]]]] <- as.integer(j)
      used[[hit[[1L]]]] <- TRUE
    }
  }
  out
}

stpd_candidate_lineage_gap_final_binding_equal <- function(pre_audit, combined, map) {
  if (!nrow(combined)) return(TRUE)
  if (nrow(combined) != sum(!is.na(map)) || anyDuplicated(map[!is.na(map)]))
    return(FALSE)
  fields <- c(
    candidate_id = "candidate_id", detector_start_row = "start_isi",
    detector_end_row = "end_isi", final_label = "final_label",
    action = "action", gap_semantics = "gap_semantics",
    hard_for_event = "hard_for_event",
    hard_for_state_direct_support = "hard_for_state_direct_support",
    envelope_bridge_eligible = "envelope_bridge_eligible",
    pause_candidate_decision = "pause_candidate_decision",
    pause_candidate_reason_code = "pause_candidate_reason_code"
  )
  all(vapply(seq_len(nrow(combined)), function(j) {
    i <- which(map == j)
    if (length(i) != 1L) return(FALSE)
    z <- pre_audit[i, , drop = FALSE]
    all(vapply(names(fields), function(target) {
      source <- fields[[target]]
      expected <- combined[[target]][[j]]
      actual <- if (is.integer(expected)) {
        stpd_candidate_lineage_candidate_column(z, source, "integer")[[1L]]
      } else if (is.logical(expected)) {
        stpd_candidate_lineage_candidate_column(z, source, "logical")[[1L]]
      } else {
        stpd_candidate_lineage_candidate_column(z, source, "character")[[1L]]
      }
      identical(actual, expected)
    }, logical(1)))
  }, logical(1)))
}

stpd_candidate_lineage_gap_final_replay_selection <- function(audit, patterns) {
  replay <- audit
  replay$selected_for_auto <- FALSE
  replay$selection_status <- "not_selected"
  n <- nrow(replay)
  if (!n) return(list(audit = replay, value = numeric(), predecessor = integer()))
  lab <- as.character(replay$final_label %||% rep("", n)); lab[is.na(lab)] <- ""
  keep <- nzchar(lab) & !(lab %in% c("reject", "profile"))
  action <- stpd_candidate_lineage_gap_final_action(replay)
  action[is.na(action) | !nzchar(action)] <- "accept"
  rejected <- action %in% c("reject", "abstain", "blocked")
  keep <- keep & !rejected
  replay$selection_status[rejected] <- paste0(
    "not_selectable_candidate_action__", action[rejected])
  if (!is.null(patterns)) {
    allowed <- as.character(patterns)
    if (any(c("burst", "long_burst") %in% allowed))
      allowed <- unique(c(allowed, "possible_burst"))
    keep <- keep & lab %in% allowed
  }
  starts <- suppressWarnings(as.integer(replay$start_isi))
  ends <- suppressWarnings(as.integer(replay$end_isi))
  valid <- keep & is.finite(starts) & is.finite(ends) & starts <= ends
  valid[is.na(valid)] <- FALSE
  values <- rep(NA_real_, n)
  predecessor <- rep(NA_integer_, n)
  idx <- which(valid)
  if (!length(idx)) return(list(
    audit = replay, value = values, predecessor = predecessor))
  values[idx] <- vapply(idx, function(i) stpd_event_core_candidate_value(
    replay[i, , drop = FALSE]), numeric(1))
  ord <- order(ends[idx], starts[idx])
  pool_idx <- idx[ord]
  s <- starts[pool_idx]; e <- ends[pool_idx]; val <- values[pool_idx]
  p <- integer(length(pool_idx))
  for (j in seq_along(pool_idx)) {
    ok <- which(e < s[[j]])
    p[[j]] <- if (!length(ok)) 0L else max(ok)
    predecessor[[pool_idx[[j]]]] <- as.integer(p[[j]])
  }
  dp <- numeric(length(pool_idx) + 1L); take <- logical(length(pool_idx))
  for (j in seq_along(pool_idx)) {
    incl <- val[[j]] + dp[[p[[j]] + 1L]]; excl <- dp[[j]]
    if (incl > excl) {
      dp[[j + 1L]] <- incl; take[[j]] <- TRUE
    } else {
      dp[[j + 1L]] <- excl; take[[j]] <- FALSE
    }
  }
  chosen <- integer(); j <- length(pool_idx)
  while (j >= 1L) {
    incl <- val[[j]] + dp[[p[[j]] + 1L]]
    if (take[[j]] && incl >= dp[[j]]) {
      chosen <- c(chosen, j); j <- p[[j]]
    } else j <- j - 1L
  }
  if (length(chosen)) {
    selected <- pool_idx[chosen]
    replay$selected_for_auto[selected] <- TRUE
    replay$selection_status[selected] <-
      "selected_by_event_core_weighted_interval_grammar"
  }
  list(audit = replay, value = values, predecessor = predecessor)
}

stpd_candidate_lineage_gap_final_prevalidation_pattern <- function(
    audit, n_interval_rows, patterns, fill_others, isi, min_isi_sec) {
  pat <- rep("", n_interval_rows)
  score <- rep(NA_real_, n_interval_rows)
  selected <- which(as.logical(audit$selected_for_auto %||% FALSE))
  if (length(selected)) {
    selected <- selected[order(as.integer(audit$start_isi[selected]))]
    for (i in selected) {
      lab <- as.character(audit$final_label[[i]] %||% "")
      s <- suppressWarnings(as.integer(audit$start_isi[[i]]))
      e <- suppressWarnings(as.integer(audit$end_isi[[i]]))
      if (!nzchar(lab) || lab %in% c("reject", "profile") ||
          !is.finite(s) || !is.finite(e) || e < s || s < 2L ||
          e > n_interval_rows) next
      idx <- s:e; idx <- idx[pat[idx] == ""]
      if (!length(idx)) next
      pat[idx] <- lab
      score[idx] <- suppressWarnings(as.numeric(audit$score[[i]] %||% NA_real_))
    }
  }
  if ("others" %in% patterns && isTRUE(fill_others)) {
    art <- is_artifact_isi(isi, min_isi_sec)
    fill <- which(seq_len(n_interval_rows) >= 2L & is.finite(isi) & !art & pat == "")
    pat[fill] <- "others"
  }
  list(pattern = pat, score = score)
}

stpd_candidate_lineage_gap_final_replay <- function(context, shard) {
  contextual <- if (exists("contextual_pause_replay_cache", shard, inherits = FALSE))
    get("contextual_pause_replay_cache", shard, inherits = FALSE) else NULL
  if (is.null(contextual) || is.null(contextual$combined) ||
      is.null(contextual$receipt)) return(NULL)
  combined <- contextual$combined
  map <- stpd_candidate_lineage_gap_final_match_contextual(
    context$pre_audit, combined)
  # HFS protection may intentionally mutate a Gap row. Geometry/ordinal binding
  # remains exact even in that case; require one bound row per proposal.
  exact_binding <- stpd_candidate_lineage_gap_final_binding_equal(
    context$pre_audit, combined, map)
  selection <- stpd_candidate_lineage_gap_final_replay_selection(
    context$protected_audit, context$patterns)
  candidate_rows <- lapply(seq_len(nrow(context$protected_audit)), function(i) {
    z <- context$protected_audit[i, , drop = FALSE]
    action <- stpd_candidate_lineage_gap_final_action(z)[[1L]]
    lab <- as.character(z$final_label[[1L]] %||% "")
    s <- suppressWarnings(as.integer(z$start_isi[[1L]]))
    e <- suppressWarnings(as.integer(z$end_isi[[1L]]))
    selectable <- is.finite(selection$value[[i]])
    cj <- map[[i]]
    data.frame(
      train = context$train, input_ordinal = as.integer(i),
      candidate_id = as.character(z$candidate_id[[1L]] %||% ""),
      final_label = lab, detector_start_row = s, detector_end_row = e,
      action = action, score = suppressWarnings(as.numeric(z$score[[1L]] %||% NA_real_)),
      n_isi = suppressWarnings(as.numeric(z$n_isi[[1L]] %||% NA_real_)),
      arbitration_value = selection$value[[i]], selectable = selectable,
      input_terminal_reason = if (selectable) "eligible_for_weighted_arbitration" else
        as.character(selection$audit$selection_status[[i]]),
      is_contextual_gap_proposal = !is.na(cj),
      contextual_output_ordinal = if (is.na(cj)) NA_integer_ else as.integer(cj),
      candidate_payload_sha256 = stpd_candidate_lineage_gap_final_hash(z, "candidate-input"),
      contextual_payload_sha256 = if (is.na(cj)) "" else
        as.character(combined$candidate_payload_sha256[[cj]]),
      stringsAsFactors = FALSE, check.names = FALSE)
  })
  candidate_input <- if (!length(candidate_rows))
    stpd_candidate_lineage_empty_hook_payload("gap_final_candidate_input_v1") else
      dplyr::bind_rows(candidate_rows)
  scientific_audit <- context$scientific_audit
  scientific_selected <- as.logical(scientific_audit$selected_for_auto %||% FALSE)
  scientific_status <- as.character(scientific_audit$selection_status %||% "")
  expected_selected <- as.logical(selection$audit$selected_for_auto)
  expected_status <- as.character(selection$audit$selection_status)
  arbitration <- candidate_input[, c(
    "train", "input_ordinal", "candidate_id", "final_label",
    "detector_start_row", "detector_end_row", "arbitration_value"), drop = FALSE]
  arbitration$predecessor_pool_ordinal <- as.integer(selection$predecessor)
  arbitration$selected_expected <- expected_selected
  arbitration$selected_scientific <- scientific_selected
  arbitration$selection_status_expected <- expected_status
  arbitration$selection_status_scientific <- scientific_status
  arbitration$replay_equal <- expected_selected == scientific_selected &
    expected_status == scientific_status
  arbitration$is_gap_candidate <- candidate_input$is_contextual_gap_proposal
  arbitration$candidate_payload_sha256 <- candidate_input$candidate_payload_sha256
  arbitration <- arbitration[, names(stpd_candidate_lineage_observation_hook_schema(
    "gap_final_arbitration_v1")), drop = FALSE]
  pre <- stpd_candidate_lineage_gap_final_prevalidation_pattern(
    selection$audit, context$n_interval_rows, context$patterns,
    context$fill_others, context$isi, context$min_isi_sec)
  selected_gap <- which(expected_selected & candidate_input$is_contextual_gap_proposal &
    candidate_input$final_label == "pause")
  materialized_rows <- list(); ordinal <- 0L
  for (i in selected_gap) {
    s <- candidate_input$detector_start_row[[i]]
    e <- candidate_input$detector_end_row[[i]]
    if (!is.finite(s) || !is.finite(e) || e < s) next
    for (isi_i in s:e) {
      ordinal <- ordinal + 1L
      final <- as.character(context$final_pattern_auto[[isi_i]] %||% "")
      retained <- identical(final, "pause")
      materialized_rows[[ordinal]] <- data.frame(
        train = context$train, materialized_ordinal = ordinal,
        input_ordinal = as.integer(i), candidate_id = candidate_input$candidate_id[[i]],
        isi_index = as.integer(isi_i), expected_prevalidation_label = pre$pattern[[isi_i]],
        final_pattern_auto = final, retained_as_pause = retained,
        materialization_status = if (retained) "retained_as_final_pause_support" else
          "removed_or_reassigned_by_post_validation",
        candidate_payload_sha256 = candidate_input$candidate_payload_sha256[[i]],
        stringsAsFactors = FALSE, check.names = FALSE)
    }
  }
  materialized <- if (!length(materialized_rows))
    stpd_candidate_lineage_empty_hook_payload("gap_final_materialized_output_v1") else
      dplyr::bind_rows(materialized_rows)
  upstream_hash <- stpd_candidate_lineage_gap_final_hash(
    contextual$receipt, "upstream-contextual-receipt")
  entry <- data.frame(
    train = context$train, root_id = "final_gap_arbitration_materialization_root",
    pause_pattern_requested = "pause" %in% context$patterns,
    applicability_status = if ("pause" %in% context$patterns) "applicable" else
      "not_applicable_pause_not_requested",
    audit_candidate_n = as.integer(nrow(context$protected_audit)),
    contextual_gap_proposal_n = as.integer(nrow(combined)),
    exact_candidate_input_binding = isTRUE(exact_binding),
    upstream_contextual_receipt_sha256 = upstream_hash,
    candidate_input_sha256 = stpd_candidate_lineage_gap_final_hash(
      candidate_input, "candidate-input-table"),
    arbitration_status = "observed_replay_required",
    materialization_status = "observed_post_validation",
    recurrent_pause_state_status = "not_evaluated_post_detection_only",
    scientific_result_influence = "none_observer_only", publication_authority = FALSE,
    stringsAsFactors = FALSE, check.names = FALSE)
  audit_equal <- identical(selection$audit, scientific_audit)
  arbitration_equal <- nrow(arbitration) == 0L || all(arbitration$replay_equal)
  retained_n <- if (!nrow(materialized)) 0L else sum(materialized$retained_as_pause)
  receipt <- data.frame(
    train = context$train, root_id = "final_gap_arbitration_materialization_root",
    audit_candidate_n = as.integer(nrow(candidate_input)),
    contextual_gap_proposal_n = as.integer(nrow(combined)),
    selectable_candidate_n = as.integer(sum(candidate_input$selectable)),
    selected_candidate_n = as.integer(sum(expected_selected)),
    selected_gap_candidate_n = as.integer(length(selected_gap)),
    expected_gap_support_isi_n = as.integer(nrow(materialized)),
    retained_gap_support_isi_n = as.integer(retained_n),
    exact_candidate_input_binding = isTRUE(exact_binding),
    arbitration_replay_status = if (arbitration_equal) "validated_exact" else "mismatch",
    scientific_audit_identity_status = if (audit_equal) "validated_exact" else "mismatch",
    materialized_gap_identity_status = if (!nrow(materialized) ||
      all(materialized$expected_prevalidation_label == "pause"))
      "validated_exact_prevalidation_identity" else "mismatch",
    post_validation_removal_n = as.integer(nrow(materialized) - retained_n),
    collector_off_equivalence_status = "observer_guarded_no_collector_branch",
    recurrent_pause_state_status = "not_evaluated_post_detection_only",
    publication_authority = FALSE, scientific_result_influence = "none_observer_only",
    upstream_contextual_receipt_sha256 = upstream_hash,
    entry_payload_sha256 = stpd_candidate_lineage_gap_final_hash(entry, "entry"),
    candidate_input_payload_sha256 = stpd_candidate_lineage_gap_final_hash(
      candidate_input, "candidate-input"),
    arbitration_payload_sha256 = stpd_candidate_lineage_gap_final_hash(
      arbitration, "arbitration"),
    materialized_output_payload_sha256 = stpd_candidate_lineage_gap_final_hash(
      materialized, "materialized"),
    scientific_audit_sha256 = stpd_candidate_lineage_gap_final_hash(
      scientific_audit, "scientific-audit"),
    scientific_pattern_auto_sha256 = stpd_candidate_lineage_gap_final_hash(
      context$final_pattern_auto, "scientific-pattern-auto"),
    stringsAsFactors = FALSE, check.names = FALSE)
  if (!isTRUE(exact_binding) || !audit_equal || !arbitration_equal ||
      !identical(receipt$materialized_gap_identity_status[[1L]],
                 "validated_exact_prevalidation_identity")) return(NULL)
  list(entry = entry, candidate_input = candidate_input,
       arbitration = arbitration, materialized = materialized, receipt = receipt)
}

stpd_candidate_lineage_begin_gap_final <- function(
    shard, train, pre_audit, protected_audit, patterns, dat, params,
    min_isi_sec) {
  if (is.null(shard)) return(invisible(NULL))
  stpd_candidate_lineage_validate_train_collector(
    shard, "open", train, validate_observations = FALSE
  )
  if (!exists("contextual_pause_replay_cache", shard, inherits = FALSE) ||
      !bindingIsLocked("contextual_pause_replay_cache", shard))
    stpd_candidate_lineage_abort("collector_gap_final_contextual_not_closed",
      "Final Gap begin requires a closed contextual Pause proposal root.")
  x <- unserialize(serialize(list(
    train = as.character(train)[1L], pre_audit = pre_audit,
    protected_audit = protected_audit, patterns = as.character(patterns),
    n_interval_rows = as.integer(nrow(dat)),
    isi = suppressWarnings(as.numeric(dat$ISI_sec)),
    fill_others = isTRUE(params$detector$fill_others_auto %||% FALSE),
    min_isi_sec = as.numeric(min_isi_sec)[1L]), NULL, version = 3L))
  nm <- "gap_final_begin_context"
  if (exists(nm, shard, inherits = FALSE)) {
    if (!bindingIsLocked(nm, shard) || !identical(get(nm, shard), x))
      stpd_candidate_lineage_abort("collector_gap_final_begin_conflict",
        "Final Gap begin context conflicts with its first capture.")
  } else { assign(nm, x, shard); lockBinding(nm, shard) }
  invisible(x)
}

stpd_candidate_lineage_capture_gap_final <- function(
    shard, train, scientific_audit, final_pattern_auto) {
  if (is.null(shard)) return(invisible(NULL))
  nm <- "gap_final_begin_context"
  if (!exists(nm, shard, inherits = FALSE) || !bindingIsLocked(nm, shard))
    stpd_candidate_lineage_abort("collector_gap_final_begin_missing",
      "Final Gap capture requires its locked live begin context.")
  context <- get(nm, shard)
  if (!identical(context$train, as.character(train)[1L]))
    stpd_candidate_lineage_abort("collector_gap_final_train_conflict",
      "Final Gap capture train differs from its begin context.")
  context$scientific_audit <- unserialize(serialize(
    scientific_audit, NULL, version = 3L))
  context$final_pattern_auto <- as.character(final_pattern_auto)
  replay <- stpd_candidate_lineage_gap_final_replay(context, shard)
  if (is.null(replay)) stpd_candidate_lineage_abort(
    "collector_gap_final_replay_failed",
    "Final Gap arbitration or materialization did not replay exactly.")
  assign("gap_final_context", context, shard); lockBinding("gap_final_context", shard)
  assign("gap_final_replay_cache", replay, shard); lockBinding("gap_final_replay_cache", shard)
  hooks <- stpd_candidate_lineage_gap_final_hooks()
  for (h in names(hooks))
    stpd_candidate_lineage_collector_capture(shard, h, replay[[hooks[[h]]]])
  invisible(replay)
}

stpd_candidate_lineage_gap_final_payload_is_valid <- function(shard, hook, payload) {
  if (!exists("gap_final_context", shard, inherits = FALSE) ||
      !bindingIsLocked("gap_final_context", shard) ||
      !exists("gap_final_replay_cache", shard, inherits = FALSE) ||
      !bindingIsLocked("gap_final_replay_cache", shard)) return(FALSE)
  fresh <- stpd_candidate_lineage_gap_final_replay(
    get("gap_final_context", shard, inherits = FALSE), shard)
  cached <- get("gap_final_replay_cache", shard, inherits = FALSE)
  if (is.null(fresh) || !identical(fresh, cached)) return(FALSE)
  map <- stpd_candidate_lineage_gap_final_hooks(); key <- unname(map[[hook]])
  if (is.null(key) || !identical(payload, cached[[key]])) return(FALSE)
  pos <- match(hook, names(map))
  prior <- if (pos <= 1L) character() else names(map)[seq_len(pos - 1L)]
  all(vapply(prior, function(h)
    identical(shard$observations[[h]], cached[[map[[h]]]]), logical(1)))
}
