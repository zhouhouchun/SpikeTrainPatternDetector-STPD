# Gate 1B parallel hard-threshold Burst root -------------------------------
#
# This observer is bound only to the primary hf_protected callsite.  It
# replays the explicit per-train hard-threshold Burst generator, its internal
# refractory transformation, and the following active Event-boundary
# application.  The boundary source must close to the post-ownership
# contextual-Pause receipt; raw Pause proposals have no authority here.  The
# observed rows remain proposals: they are never fed back into scientific
# selection and never become publication authority.

stpd_candidate_lineage_hard_threshold_track <- function(label) {
  label <- as.character(label)
  track <- rep("diagnostic", length(label))
  track[label %in% c("burst", "long_burst")] <- "event"
  track[label == "possible_burst"] <- "review"
  track
}

stpd_candidate_lineage_hard_threshold_config <- function(context) {
  n <- nrow(context$dat)
  train <- as.character(context$train)[1L]
  range <- if (n > 1L && nzchar(train)) {
    get_train_burst_range(context$params$burst %||% list(), train = train)
  } else NULL
  range_present <- !is.null(range)
  mode <- if (range_present) {
    as.character(stpd_train_isi_threshold_mode(range))[1L]
  } else "not_configured"
  hard <- range_present && isTRUE(stpd_train_isi_threshold_is_hard(range))
  bmax <- if (range_present) {
    suppressWarnings(as.numeric(range_value(range, "high_sec", NA_real_)))[1L]
  } else NA_real_
  applicable <- n > 1L && nzchar(train) && hard &&
    is.finite(bmax) && bmax > 0
  bridge_high <- if (applicable) {
    max(c(
      bmax,
      stpd_event_core_num(context$vp$bridge_high, bmax),
      bmax * 1.25
    ), na.rm = TRUE)
  } else NA_real_
  min_core <- max(
    1L,
    stpd_event_core_int(context$vp$min_seed_isi_n %||% 2L, 2L)
  )
  min_spikes <- max(
    2L,
    stpd_event_core_int(context$vp$min_spikes %||% 3L, 3L)
  )
  source <- if (range_present) {
    as.character(range$source %||% "ui_isi_profile_threshold_line")[1L]
  } else ""
  if (is.na(source)) source <- ""
  patterns <- as.character(context$patterns %||% character())
  patterns <- patterns[!is.na(patterns)]
  burst_requested <- any(c("burst", "long_burst") %in% patterns)
  pause_requested <- "pause" %in% patterns
  applicability <- if (n <= 1L) {
    "not_reached_insufficient_input"
  } else if (!nzchar(train)) {
    "not_applicable_empty_train"
  } else if (!range_present) {
    "invoked_no_train_range"
  } else if (!hard) {
    "invoked_non_hard_threshold_range"
  } else if (!is.finite(bmax) || bmax <= 0) {
    "invoked_invalid_hard_upper"
  } else {
    "invoked_hard_threshold_scan"
  }
  list(
    range = range, range_present = range_present, threshold_mode = mode,
    hard = hard, bmax = bmax, bridge_high = bridge_high,
    min_core = as.integer(min_core), min_spikes = as.integer(min_spikes),
    source = source, applicable = applicable,
    burst_requested = burst_requested, pause_requested = pause_requested,
    applicability = applicability
  )
}

stpd_candidate_lineage_hard_threshold_support <- function(context, config) {
  n <- nrow(context$dat)
  rows <- if (n >= 2L) seq.int(2L, n) else integer()
  if (!length(rows)) {
    return(stpd_candidate_lineage_empty_hook_payload(
      "burst_hard_threshold_support_v1"
    ))
  }
  isi <- suppressWarnings(as.numeric(context$dat$ISI_sec))
  artifact <- is_artifact_isi(isi, context$min_isi_sec)
  valid <- is.finite(isi) & !artifact
  if (length(valid)) valid[[1L]] <- FALSE
  seed <- rep(FALSE, n)
  bridge <- rep(FALSE, n)
  if (isTRUE(config$applicable)) {
    seed <- valid & isi <= config$bmax
    bridge <- valid & isi <= config$bridge_high
  }
  data.frame(
    train = rep(context$train, length(rows)),
    detector_row_index = as.integer(rows),
    isi_index = as.integer(seq_along(rows)),
    isi_sec = as.numeric(isi[rows]),
    artifact_isi = as.logical(artifact[rows]),
    valid_isi = as.logical(valid[rows]),
    seed_band_member = as.logical(seed[rows]),
    bridge_band_member = as.logical(bridge[rows]),
    hard_burst_seed_upper_sec = rep(as.numeric(config$bmax), length(rows)),
    hard_burst_bridge_upper_sec = rep(
      as.numeric(config$bridge_high), length(rows)
    ),
    stringsAsFactors = FALSE, check.names = FALSE
  )
}

stpd_candidate_lineage_hard_threshold_candidate_payload <- function(
    candidates, source_runs, train) {
  if (!nrow(candidates)) {
    return(stpd_candidate_lineage_empty_hook_payload(
      "burst_hard_threshold_pre_refractory_candidates_v1"
    ))
  }
  n <- nrow(candidates)
  hashes <- vapply(seq_len(n), function(i) {
    stpd_candidate_lineage_candidate_source_hash(
      candidates[i, , drop = FALSE], "hard_threshold_pre_refractory"
    )
  }, character(1))
  start <- stpd_candidate_lineage_candidate_column(
    candidates, "start_isi", "integer"
  )
  end <- stpd_candidate_lineage_candidate_column(
    candidates, "end_isi", "integer"
  )
  labels <- stpd_candidate_lineage_candidate_column(
    candidates, "final_label", "character"
  )
  data.frame(
    train = rep(train, n), candidate_ordinal = as.integer(seq_len(n)),
    source_run_ordinal = as.integer(source_runs),
    candidate_id = stpd_candidate_lineage_candidate_column(
      candidates, "candidate_id", "character"
    ),
    candidate_payload_sha256 = hashes,
    detector_start_row = start, detector_end_row = end,
    start_isi = as.integer(start - 1L), end_isi = as.integer(end - 1L),
    candidate_layer = stpd_candidate_lineage_candidate_column(
      candidates, "candidate_layer", "character"
    ),
    candidate_class = stpd_candidate_lineage_candidate_column(
      candidates, "candidate_class", "character"
    ),
    final_label = labels,
    gate_status = stpd_candidate_lineage_candidate_column(
      candidates, "gate_status", "character"
    ),
    action = stpd_candidate_lineage_candidate_column(
      candidates, "action", "character"
    ),
    priority = stpd_candidate_lineage_candidate_column(
      candidates, "priority", "double"
    ),
    score = stpd_candidate_lineage_candidate_column(
      candidates, "score", "double"
    ),
    hard_burst_core_isi_count = stpd_candidate_lineage_candidate_column(
      candidates, "hard_burst_core_isi_count", "integer"
    ),
    hard_burst_seed_upper_sec = stpd_candidate_lineage_candidate_column(
      candidates, "hard_burst_seed_upper_sec", "double"
    ),
    hard_burst_bridge_upper_sec = stpd_candidate_lineage_candidate_column(
      candidates, "hard_burst_bridge_upper_sec", "double"
    ),
    hard_threshold_source = stpd_candidate_lineage_candidate_column(
      candidates, "hard_threshold_source", "character"
    ),
    proposal_track = stpd_candidate_lineage_hard_threshold_track(labels),
    event_materialization_status = rep(
      "proposal_not_final_selection", n
    ),
    stringsAsFactors = FALSE, check.names = FALSE
  )
}

stpd_candidate_lineage_hard_threshold_generate <- function(context, config) {
  support <- stpd_candidate_lineage_hard_threshold_support(context, config)
  if (!isTRUE(config$applicable)) {
    return(list(
      support = support,
      runs = stpd_candidate_lineage_empty_hook_payload(
        "burst_hard_threshold_runs_v1"
      ),
      raw_candidates = data.frame(),
      candidates = stpd_candidate_lineage_empty_hook_payload(
        "burst_hard_threshold_pre_refractory_candidates_v1"
      )
    ))
  }
  dat <- context$dat
  n <- nrow(dat)
  isi <- suppressWarnings(as.numeric(dat$ISI_sec))
  artifact <- is_artifact_isi(isi, context$min_isi_sec)
  valid <- is.finite(isi) & !artifact
  valid[[1L]] <- FALSE
  seed_flag <- valid & isi <= config$bmax
  bridge_flag <- valid & isi <= config$bridge_high
  raw_runs <- stpd_event_core_bool_runs(bridge_flag)
  if (!nrow(raw_runs)) {
    return(list(
      support = support,
      runs = stpd_candidate_lineage_empty_hook_payload(
        "burst_hard_threshold_runs_v1"
      ),
      raw_candidates = data.frame(),
      candidates = stpd_candidate_lineage_empty_hook_payload(
        "burst_hard_threshold_pre_refractory_candidates_v1"
      )
    ))
  }

  run_rows <- vector("list", nrow(raw_runs))
  emitted <- list()
  emitted_source_run <- integer()
  counter <- 0L
  emitted_n <- 0L
  for (rr in seq_len(nrow(raw_runs))) {
    start <- as.integer(raw_runs$start_isi[[rr]])
    end <- as.integer(raw_runs$end_isi[[rr]])
    idx <- stpd_event_core_safe_seq(start, end)
    core_n <- if (length(idx)) {
      as.integer(sum(seed_flag[idx], na.rm = TRUE))
    } else 0L
    n_spikes <- as.integer(end - start + 2L)
    core_gate <- if (!length(idx)) {
      "not_evaluated"
    } else if (core_n >= config$min_core) "pass" else "fail"
    spike_gate <- if (!length(idx) || core_gate == "fail") {
      "not_evaluated"
    } else if (n_spikes >= config$min_spikes) "pass" else "fail"
    construction <- FALSE
    candidate_counter <- NA_integer_
    candidate_id <- ""
    terminal_status <- "run_rejected"
    terminal_reason <- if (!length(idx)) {
      "empty_safe_sequence"
    } else if (core_gate == "fail") {
      "insufficient_hard_threshold_core"
    } else if (spike_gate == "fail") {
      "insufficient_spike_count"
    } else ""
    emitted_ordinal <- NA_integer_
    candidate_hash <- ""

    if (length(idx) && core_gate == "pass" && spike_gate == "pass") {
      construction <- TRUE
      counter <- counter + 1L
      candidate_counter <- as.integer(counter)
      candidate_id <- paste0("hard_isi_threshold_", counter)
      size_label <- stpd_event_core_burst_subtype_from_spike_count(
        n_spikes, context$vp, review_label = "possible_burst"
      )
      review_required <- identical(size_label, "possible_burst")
      score <- 30 + core_n + 0.05 * n_spikes -
        0.25 * sum(bridge_flag[idx] & !seed_flag[idx], na.rm = TRUE)
      row <- stpd_event_core_candidate_from_run(
        dat, start, end, context$params, context$vp,
        context$min_isi_sec, context$train,
        "isi_profile_hard_threshold_burst",
        "isi_profile_hard_threshold_burst", size_label,
        if (review_required) {
          "isi_profile_hard_threshold_prolonged_review"
        } else "isi_profile_hard_threshold_burst_pass",
        if (review_required) {
          "hard_threshold_structure_exceeds_long_burst_spike_count_range"
        } else "hard_threshold_direct_seed_bridge_without_flank_contrast_gate",
        if (review_required) "demote_to_possible" else "accept",
        score,
        if (identical(size_label, "burst")) {
          1450
        } else if (identical(size_label, "long_burst")) 1360 else 120,
        list(
          threshold_mode = "hard_threshold", hard_threshold = TRUE,
          hard_threshold_pattern = "burst",
          hard_burst_seed_upper_sec = config$bmax,
          hard_burst_bridge_upper_sec = config$bridge_high,
          hard_burst_core_isi_count = as.integer(core_n),
          hard_threshold_source = config$source,
          candidate_id = candidate_id
        )
      )
      if (!is.null(row) && nrow(row) > 0L) {
        emitted_n <- emitted_n + 1L
        emitted[[emitted_n]] <- row
        emitted_source_run[[emitted_n]] <- as.integer(rr)
        emitted_ordinal <- as.integer(emitted_n)
        candidate_hash <- stpd_candidate_lineage_candidate_source_hash(
          row, "hard_threshold_pre_refractory"
        )
        terminal_status <- "candidate_emitted"
        terminal_reason <- "hard_threshold_run_emitted"
      } else {
        terminal_status <- "construction_failed"
        terminal_reason <- "candidate_metrics_unavailable"
      }
    }
    run_rows[[rr]] <- data.frame(
      train = context$train, run_ordinal = as.integer(rr),
      detector_start_row = start, detector_end_row = end,
      start_isi = as.integer(start - 1L), end_isi = as.integer(end - 1L),
      bridge_isi_count = as.integer(length(idx)),
      core_isi_count = core_n, n_spikes = n_spikes,
      min_core_isi_count = config$min_core,
      min_spikes = config$min_spikes,
      core_count_gate = core_gate, spike_count_gate = spike_gate,
      construction_attempted = construction,
      candidate_counter = candidate_counter, candidate_id = candidate_id,
      terminal_status = terminal_status, terminal_reason = terminal_reason,
      emitted_candidate_ordinal = emitted_ordinal,
      candidate_payload_sha256 = candidate_hash,
      stringsAsFactors = FALSE, check.names = FALSE
    )
  }
  runs <- dplyr::bind_rows(run_rows)
  raw <- if (length(emitted)) dplyr::bind_rows(emitted) else data.frame()
  candidates <- stpd_candidate_lineage_hard_threshold_candidate_payload(
    raw, emitted_source_run, context$train
  )
  list(
    support = support, runs = runs, raw_candidates = raw,
    candidates = candidates
  )
}

stpd_candidate_lineage_hard_threshold_pre_boundary_payload <- function(
    replay, candidates, train) {
  output <- replay$output
  if (!nrow(output)) {
    return(stpd_candidate_lineage_empty_hook_payload(
      "burst_hard_threshold_pre_boundary_output_v1"
    ))
  }
  source_run <- candidates$source_run_ordinal[output$source_input_ordinal]
  labels <- output$final_label
  data.frame(
    train = rep(train, nrow(output)), output_ordinal = output$output_ordinal,
    source_input_ordinal = output$source_input_ordinal,
    source_run_ordinal = as.integer(source_run), relation = output$relation,
    source_candidate_id = output$source_candidate_id,
    source_payload_sha256 = output$source_payload_sha256,
    output_candidate_id = output$output_candidate_id,
    output_payload_sha256 = output$output_payload_sha256,
    detector_start_row = output$detector_start_row,
    detector_end_row = output$detector_end_row,
    start_isi = output$start_isi, end_isi = output$end_isi,
    final_label = labels, gate_status = output$gate_status,
    action = output$action, priority = output$priority, score = output$score,
    proposal_track = stpd_candidate_lineage_hard_threshold_track(labels),
    event_materialization_status = rep(
      "proposal_not_final_selection", nrow(output)
    ),
    stringsAsFactors = FALSE, check.names = FALSE
  )
}

stpd_candidate_lineage_hard_threshold_post_boundary_payload <- function(
    boundary_replay, pre_boundary, train) {
  output <- boundary_replay$output
  if (!nrow(output)) {
    return(stpd_candidate_lineage_empty_hook_payload(
      "burst_hard_threshold_post_boundary_output_v1"
    ))
  }
  source_run <- pre_boundary$source_run_ordinal[output$source_input_ordinal]
  labels <- output$final_label
  data.frame(
    train = rep(train, nrow(output)), output_ordinal = output$output_ordinal,
    source_input_ordinal = output$source_input_ordinal,
    source_run_ordinal = as.integer(source_run), relation = output$relation,
    source_candidate_id = output$source_candidate_id,
    source_payload_sha256 = output$source_payload_sha256,
    output_candidate_id = output$output_candidate_id,
    output_payload_sha256 = output$output_payload_sha256,
    detector_start_row = output$detector_start_row,
    detector_end_row = output$detector_end_row,
    start_isi = output$start_isi, end_isi = output$end_isi,
    final_label = labels, action = output$action,
    priority = output$priority, score = output$score,
    hard_boundary_conflict = output$hard_boundary_conflict,
    hard_boundary_conflict_kind = output$hard_boundary_conflict_kind,
    hard_boundary_decision = output$hard_boundary_decision,
    proposal_track = stpd_candidate_lineage_hard_threshold_track(labels),
    event_materialization_status = rep(
      "proposal_not_final_selection", nrow(output)
    ),
    stringsAsFactors = FALSE, check.names = FALSE
  )
}

stpd_candidate_lineage_hard_threshold_entry <- function(context, config) {
  n <- nrow(context$dat)
  data.frame(
    train = context$train, root_id = "hard_threshold_burst_parallel_root",
    applicability_status = config$applicability,
    generator_invoked = n > 1L,
    burst_family_requested = config$burst_requested,
    generation_gated_by_requested_patterns = FALSE,
    downstream_pattern_eligibility_status =
      "not_observed_in_this_root_patch",
    n_interval_rows = as.integer(n),
    n_physical_isi = as.integer(max(0L, n - 1L)),
    min_isi_sec = as.numeric(context$min_isi_sec),
    train_range_present = config$range_present,
    threshold_mode = config$threshold_mode,
    hard_threshold_applicable = config$applicable,
    hard_threshold_source = config$source,
    hard_burst_seed_upper_sec = as.numeric(config$bmax),
    hard_burst_bridge_upper_sec = as.numeric(config$bridge_high),
    min_core_isi_count = config$min_core, min_spikes = config$min_spikes,
    shared_vp_configuration = TRUE,
    root_independence_scope =
      "parallel_candidate_root_with_shared_dat_params_vp",
    input_data_sha256 = stpd_threshold_first_hash_domain(
      "stpd-hard-threshold-root-input-data-v1", context$dat
    ),
    params_payload_sha256 = stpd_threshold_first_hash_domain(
      "stpd-hard-threshold-root-effective-params-v1", context$params
    ),
    vp_payload_sha256 = stpd_threshold_first_hash_domain(
      "stpd-hard-threshold-root-resolved-vp-v1", context$vp
    ),
    stringsAsFactors = FALSE, check.names = FALSE
  )
}

stpd_candidate_lineage_hard_threshold_replay <- function(context) {
  required <- c(
    "train", "dat", "params", "vp", "min_isi_sec", "patterns",
    "scientific_pre_boundary", "scientific_post_boundary",
    "raw_pause_candidates", "boundaries"
  )
  if (is.null(context) || !is.list(context) ||
      !all(required %in% names(context)) ||
      !is.character(context$train) || length(context$train) != 1L ||
      is.na(context$train) || !nzchar(context$train) ||
      !is.data.frame(context$dat) || !is.list(context$params) ||
      !is.list(context$vp) || !is.data.frame(context$scientific_pre_boundary) ||
      !is.data.frame(context$scientific_post_boundary) ||
      !is.data.frame(context$raw_pause_candidates) ||
      !is.data.frame(context$boundaries)) {
    return(NULL)
  }
  config <- stpd_candidate_lineage_hard_threshold_config(context)
  generated <- stpd_candidate_lineage_hard_threshold_generate(context, config)
  source_hashes <- if (nrow(generated$raw_candidates)) {
    generated$candidates$candidate_payload_sha256
  } else character()
  refractory_context <- list(
    train = context$train,
    input_candidates = generated$raw_candidates,
    scientific_out = context$scientific_pre_boundary,
    source_hashes = source_hashes,
    dat = context$dat, params = context$params, vp = context$vp,
    min_isi_sec = context$min_isi_sec
  )
  refractory <- stpd_candidate_lineage_refractory_replay(
    refractory_context
  )
  if (is.null(refractory)) return(NULL)
  pre_boundary <- stpd_candidate_lineage_hard_threshold_pre_boundary_payload(
    refractory, generated$candidates, context$train
  )

  boundary_context <- list(
    train = context$train,
    input_candidates = context$scientific_pre_boundary,
    scientific_out = context$scientific_post_boundary,
    raw_pause_candidates = context$raw_pause_candidates,
    boundaries = context$boundaries,
    contextual_pause_observation_closed = TRUE
  )
  boundary <- stpd_candidate_lineage_pause_boundary_replay(boundary_context)
  if (is.null(boundary)) return(NULL)
  boundary_sources <- boundary$sources[, names(
    stpd_candidate_lineage_observation_hook_schema(
      "burst_hard_threshold_boundary_sources_v1"
    )
  ), drop = FALSE]
  boundary_attempts <- boundary$attempts[, names(
    stpd_candidate_lineage_observation_hook_schema(
      "burst_hard_threshold_boundary_attempts_v1"
    )
  ), drop = FALSE]
  post_boundary <-
    stpd_candidate_lineage_hard_threshold_post_boundary_payload(
      boundary, pre_boundary, context$train
    )
  entry <- stpd_candidate_lineage_hard_threshold_entry(context, config)
  support <- generated$support
  runs <- generated$runs
  candidates <- generated$candidates
  refractory_attempts <- refractory$attempts[, names(
    stpd_candidate_lineage_observation_hook_schema(
      "burst_hard_threshold_refractory_attempts_v1"
    )
  ), drop = FALSE]
  core_rejected <- if (nrow(runs)) {
    sum(runs$terminal_reason == "insufficient_hard_threshold_core")
  } else 0L
  spike_rejected <- if (nrow(runs)) {
    sum(runs$terminal_reason == "insufficient_spike_count")
  } else 0L
  construction_failed <- if (nrow(runs)) {
    sum(runs$terminal_status == "construction_failed")
  } else 0L
  blocked <- if (nrow(boundary_attempts)) {
    sum(boundary_attempts$blocked)
  } else 0L
  coverage <- if (!isTRUE(config$applicable)) {
    paste0("hard_threshold_", config$applicability)
  } else if (!nrow(runs)) {
    "hard_threshold_run_scan_complete_no_bridge_runs"
  } else {
    "hard_threshold_run_scan_complete_to_post_boundary_output"
  }
  receipt <- data.frame(
    train = context$train, root_id = "hard_threshold_burst_parallel_root",
    applicability_status = config$applicability,
    generator_invoked = nrow(context$dat) > 1L,
    burst_family_requested = config$burst_requested,
    generation_gated_by_requested_patterns = FALSE,
    downstream_pattern_eligibility_status =
      "not_observed_in_this_root_patch",
    pause_pattern_requested = config$pause_requested,
    support_row_n = as.integer(nrow(support)),
    valid_isi_n = as.integer(sum(support$valid_isi)),
    seed_member_n = as.integer(sum(support$seed_band_member)),
    bridge_member_n = as.integer(sum(support$bridge_band_member)),
    bridge_run_n = as.integer(nrow(runs)),
    core_rejected_run_n = as.integer(core_rejected),
    spike_rejected_run_n = as.integer(spike_rejected),
    construction_failed_run_n = as.integer(construction_failed),
    pre_refractory_candidate_n = as.integer(nrow(candidates)),
    refractory_attempt_n = as.integer(nrow(refractory_attempts)),
    pre_boundary_output_n = as.integer(nrow(pre_boundary)),
    canonical_boundary_n = as.integer(nrow(context$boundaries)),
    boundary_attempt_n = as.integer(nrow(boundary_attempts)),
    boundary_blocked_n = as.integer(blocked),
    post_boundary_output_n = as.integer(nrow(post_boundary)),
    run_scan_exhausted = isTRUE(config$applicable),
    cap_applied = FALSE, deduplication_applied = FALSE,
    scientific_order = "detector_row_order_no_cap_no_dedup",
    evidence_origin =
      "live_primary_callsite_with_deterministic_full_root_replay",
    input_scope =
      "primary_hf_protected_hard_threshold_root_to_active_event_boundary",
    root_independence_scope =
      "parallel_candidate_root_with_shared_dat_params_vp",
    upstream_pause_status = if (!isTRUE(config$pause_requested)) {
      "pause_pattern_not_requested"
    } else if (isTRUE(context$contextual_pause_observation_closed)) {
      "post_ownership_contextual_pause_observation_closed"
    } else {
      "post_ownership_contextual_pause_observation_unclosed"
    },
    upstream_universe_status = "candidate_universe_unavailable",
    output_role =
      "event_review_or_diagnostic_proposals_not_final_materialization",
    coverage_status = coverage,
    entry_payload_sha256 = stpd_threshold_first_hash_domain(
      "stpd-hard-threshold-entry-v1", entry
    ),
    support_payload_sha256 = stpd_threshold_first_hash_domain(
      "stpd-hard-threshold-support-v1", support
    ),
    run_payload_sha256 = stpd_threshold_first_hash_domain(
      "stpd-hard-threshold-runs-v1", runs
    ),
    pre_refractory_payload_sha256 = stpd_threshold_first_hash_domain(
      "stpd-hard-threshold-pre-refractory-v1", candidates
    ),
    refractory_attempt_payload_sha256 = stpd_threshold_first_hash_domain(
      "stpd-hard-threshold-refractory-attempts-v1", refractory_attempts
    ),
    pre_boundary_payload_sha256 = stpd_threshold_first_hash_domain(
      "stpd-hard-threshold-pre-boundary-v1", pre_boundary
    ),
    boundary_source_payload_sha256 = stpd_threshold_first_hash_domain(
      "stpd-hard-threshold-boundary-sources-v1", boundary_sources
    ),
    boundary_attempt_payload_sha256 = stpd_threshold_first_hash_domain(
      "stpd-hard-threshold-boundary-attempts-v1", boundary_attempts
    ),
    post_boundary_payload_sha256 = stpd_threshold_first_hash_domain(
      "stpd-hard-threshold-post-boundary-v1", post_boundary
    ),
    scientific_pre_boundary_sha256 = stpd_threshold_first_hash_domain(
      "stpd-hard-threshold-scientific-pre-boundary-v1",
      context$scientific_pre_boundary
    ),
    scientific_post_boundary_sha256 = stpd_threshold_first_hash_domain(
      "stpd-hard-threshold-scientific-post-boundary-v1",
      context$scientific_post_boundary
    ),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  list(
    entry = entry, support = support, runs = runs,
    pre_refractory_candidates = candidates,
    refractory_attempts = refractory_attempts,
    pre_boundary_output = pre_boundary,
    boundary_sources = boundary_sources,
    boundary_attempts = boundary_attempts,
    post_boundary_output = post_boundary, receipt = receipt
  )
}

stpd_candidate_lineage_hard_threshold_begin_fields <- function() {
  c(
    "train", "dat", "params", "vp", "min_isi_sec", "patterns",
    "raw_pause_candidates", "boundaries"
  )
}

stpd_candidate_lineage_hard_threshold_pause_upstream_is_closed <- function(
    train_collector, context) {
  context_name <- "contextual_pause_context"
  contextual <- if (exists(
      context_name, envir = train_collector, inherits = FALSE) &&
      bindingIsLocked(context_name, train_collector)) {
    get(context_name, envir = train_collector, inherits = FALSE)
  } else NULL
  receipt <- train_collector$observations$contextual_pause_receipt_v1
  if (is.null(contextual) || !is.list(contextual) ||
      !is.data.frame(contextual$scientific_out) ||
      !is.data.frame(receipt) || nrow(receipt) != 1L ||
      !stpd_candidate_lineage_contextual_pause_payload_is_valid(
        train_collector, "contextual_pause_receipt_v1", receipt
      )) {
    return(FALSE)
  }
  expected_boundaries <- stpd_event_core_final_event_boundaries(
    contextual$scientific_out
  )
  identical(context$train, contextual$train) &&
    identical(context$dat, contextual$dat) &&
    identical(context$params, contextual$params) &&
    identical(context$vp, contextual$vp) &&
    identical(context$min_isi_sec, contextual$min_isi_sec) &&
    identical(
      as.character(context$patterns %||% character()),
      as.character(contextual$patterns %||% character())
    ) &&
    identical(context$raw_pause_candidates, contextual$scientific_out) &&
    identical(context$boundaries, expected_boundaries)
}

stpd_candidate_lineage_hard_threshold_live_entry_is_closed <- function(
    train_collector, context, require_begin = TRUE) {
  entry <- train_collector$observations$hf_protected_impl_entry_v1
  outer_closed <- is.data.frame(entry) && nrow(entry) == 1L &&
    identical(entry$train[[1L]], train_collector$train) &&
    identical(entry$train[[1L]], context$train) &&
    identical(entry$n_interval_rows[[1L]], as.integer(nrow(context$dat))) &&
    identical(entry$min_isi_sec[[1L]], as.numeric(context$min_isi_sec)) &&
    stpd_candidate_lineage_hard_threshold_pause_upstream_is_closed(
      train_collector, context
    )
  if (!isTRUE(outer_closed) || !isTRUE(require_begin)) return(outer_closed)
  begin_name <- "burst_hard_threshold_begin_context"
  if (!exists(begin_name, envir = train_collector, inherits = FALSE) ||
      !bindingIsLocked(begin_name, train_collector)) {
    return(FALSE)
  }
  fields <- stpd_candidate_lineage_hard_threshold_begin_fields()
  all(fields %in% names(context)) && identical(
    get(begin_name, envir = train_collector, inherits = FALSE),
    context[fields]
  )
}

stpd_candidate_lineage_begin_burst_hard_threshold_root <- function(
    train_collector, train, dat, params, vp, min_isi_sec, patterns,
    raw_pause_candidates, boundaries) {
  if (is.null(train_collector)) return(invisible(NULL))
  train <- as.character(train)
  if (length(train) != 1L || is.na(train) || !nzchar(train)) {
    stpd_candidate_lineage_abort(
      "collector_train_scope_invalid",
      "Hard-threshold Burst-root observation requires one active train.",
      field = "train"
    )
  }
  stpd_candidate_lineage_validate_train_collector(
    train_collector, require_state = "open", train = train,
    validate_observations = FALSE
  )
  begin_context <- unserialize(serialize(list(
    train = train,
    dat = as.data.frame(dat, stringsAsFactors = FALSE, check.names = FALSE),
    params = params, vp = vp,
    min_isi_sec = suppressWarnings(as.numeric(min_isi_sec))[[1L]],
    patterns = as.character(patterns %||% character()),
    raw_pause_candidates = as.data.frame(
      raw_pause_candidates %||% data.frame(), stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    boundaries = as.data.frame(
      boundaries %||% data.frame(), stringsAsFactors = FALSE,
      check.names = FALSE
    )
  ), NULL, version = 3L))
  if (!stpd_candidate_lineage_hard_threshold_live_entry_is_closed(
      train_collector, begin_context, require_begin = FALSE)) {
    stpd_candidate_lineage_abort(
      "collector_hard_threshold_entry_not_closed",
      "Hard-threshold root does not close to the primary detector entry."
    )
  }
  begin_name <- "burst_hard_threshold_begin_context"
  if (exists(begin_name, envir = train_collector, inherits = FALSE)) {
    if (!bindingIsLocked(begin_name, train_collector) ||
        !identical(
          get(begin_name, envir = train_collector, inherits = FALSE),
          begin_context
        )) {
      stpd_candidate_lineage_abort(
        "collector_hard_threshold_begin_context_conflict",
        "Hard-threshold root begin-context is absent or inconsistent."
      )
    }
  } else {
    assign(begin_name, begin_context, envir = train_collector)
    lockBinding(begin_name, train_collector)
  }
  invisible(begin_context)
}

stpd_candidate_lineage_capture_burst_hard_threshold_root <- function(
    train_collector, train, dat, params, vp, min_isi_sec, patterns,
    scientific_pre_boundary, scientific_post_boundary,
    raw_pause_candidates, boundaries) {
  if (is.null(train_collector)) return(invisible(NULL))
  train <- as.character(train)
  if (length(train) != 1L || is.na(train) || !nzchar(train)) {
    stpd_candidate_lineage_abort(
      "collector_train_scope_invalid",
      "Hard-threshold Burst-root observation requires one active train.",
      field = "train"
    )
  }
  stpd_candidate_lineage_validate_train_collector(
    train_collector, require_state = "open", train = train,
    validate_observations = FALSE
  )
  begin_name <- "burst_hard_threshold_begin_context"
  if (!exists(begin_name, envir = train_collector, inherits = FALSE) ||
      !bindingIsLocked(begin_name, train_collector)) {
    stpd_candidate_lineage_abort(
      "collector_hard_threshold_begin_context_missing",
      paste(
        "Hard-threshold root capture requires the locked context established",
        "immediately before the primary scientific call."
      )
    )
  }
  supplied_begin <- unserialize(serialize(list(
    train = train,
    dat = as.data.frame(dat, stringsAsFactors = FALSE, check.names = FALSE),
    params = params, vp = vp,
    min_isi_sec = suppressWarnings(as.numeric(min_isi_sec))[[1L]],
    patterns = as.character(patterns %||% character()),
    raw_pause_candidates = as.data.frame(
      raw_pause_candidates %||% data.frame(), stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    boundaries = as.data.frame(
      boundaries %||% data.frame(), stringsAsFactors = FALSE,
      check.names = FALSE
    )
  ), NULL, version = 3L))
  begin_context <- get(begin_name, envir = train_collector, inherits = FALSE)
  if (!identical(begin_context, supplied_begin)) {
    stpd_candidate_lineage_abort(
      "collector_hard_threshold_begin_context_conflict",
      "Hard-threshold root capture differs from its locked live begin-context."
    )
  }
  context <- begin_context
  context$scientific_pre_boundary <- as.data.frame(
    scientific_pre_boundary %||% data.frame(), stringsAsFactors = FALSE,
    check.names = FALSE
  )
  context$scientific_post_boundary <- as.data.frame(
    scientific_post_boundary %||% data.frame(), stringsAsFactors = FALSE,
    check.names = FALSE
  )
  context$contextual_pause_observation_closed <- TRUE
  if (!stpd_candidate_lineage_hard_threshold_live_entry_is_closed(
      train_collector, context)) {
    stpd_candidate_lineage_abort(
      "collector_hard_threshold_entry_not_closed",
      "Hard-threshold root does not close to the primary detector entry."
    )
  }
  replay <- stpd_candidate_lineage_hard_threshold_replay(context)
  if (is.null(replay)) {
    stpd_candidate_lineage_abort(
      "collector_hard_threshold_scientific_replay_failed",
      paste(
        "Hard-threshold root, refractory output, or active Event-boundary",
        "output does not replay exactly from contextual Pause."
      )
    )
  }
  context_name <- "burst_hard_threshold_context"
  replay_name <- "burst_hard_threshold_replay_cache"
  if (exists(context_name, envir = train_collector, inherits = FALSE)) {
    if (!identical(get(context_name, envir = train_collector), context)) {
      stpd_candidate_lineage_abort(
        "collector_hard_threshold_context_conflict",
        "Hard-threshold root context was already captured with other bytes."
      )
    }
    if (!exists(replay_name, envir = train_collector, inherits = FALSE) ||
        !bindingIsLocked(replay_name, train_collector) ||
        !identical(get(replay_name, envir = train_collector), replay)) {
      stpd_candidate_lineage_abort(
        "collector_hard_threshold_replay_cache_conflict",
        "Hard-threshold root replay cache is absent or inconsistent."
      )
    }
  } else {
    assign(context_name, context, envir = train_collector)
    lockBinding(context_name, train_collector)
    assign(
      replay_name,
      unserialize(serialize(replay, NULL, version = 3L)),
      envir = train_collector
    )
    lockBinding(replay_name, train_collector)
  }
  hook_map <- c(
    burst_hard_threshold_entry_v1 = "entry",
    burst_hard_threshold_support_v1 = "support",
    burst_hard_threshold_runs_v1 = "runs",
    burst_hard_threshold_pre_refractory_candidates_v1 =
      "pre_refractory_candidates",
    burst_hard_threshold_refractory_attempts_v1 = "refractory_attempts",
    burst_hard_threshold_pre_boundary_output_v1 = "pre_boundary_output",
    burst_hard_threshold_boundary_sources_v1 = "boundary_sources",
    burst_hard_threshold_boundary_attempts_v1 = "boundary_attempts",
    burst_hard_threshold_post_boundary_output_v1 = "post_boundary_output",
    burst_hard_threshold_receipt_v1 = "receipt"
  )
  for (hook in names(hook_map)) {
    stpd_candidate_lineage_collector_capture(
      train_collector, hook, replay[[hook_map[[hook]]]]
    )
  }
  invisible(replay)
}

stpd_candidate_lineage_hard_threshold_payload_is_valid <- function(
    train_collector, hook_id, payload) {
  context_name <- "burst_hard_threshold_context"
  replay_name <- "burst_hard_threshold_replay_cache"
  if (!exists(context_name, envir = train_collector, inherits = FALSE) ||
      !bindingIsLocked(context_name, train_collector) ||
      !exists(replay_name, envir = train_collector, inherits = FALSE) ||
      !bindingIsLocked(replay_name, train_collector)) {
    return(FALSE)
  }
  context <- get(context_name, envir = train_collector, inherits = FALSE)
  if (is.null(context) || !is.list(context) ||
      !identical(context$train, train_collector$train) ||
      !("train" %in% names(payload)) || anyNA(payload$train) ||
      !all(payload$train == train_collector$train) ||
      !stpd_candidate_lineage_hard_threshold_live_entry_is_closed(
        train_collector, context
      )) {
    return(FALSE)
  }
  replay <- get(replay_name, envir = train_collector, inherits = FALSE)
  if (is.null(replay) || !is.list(replay)) return(FALSE)
  hook_map <- c(
    burst_hard_threshold_entry_v1 = "entry",
    burst_hard_threshold_support_v1 = "support",
    burst_hard_threshold_runs_v1 = "runs",
    burst_hard_threshold_pre_refractory_candidates_v1 =
      "pre_refractory_candidates",
    burst_hard_threshold_refractory_attempts_v1 = "refractory_attempts",
    burst_hard_threshold_pre_boundary_output_v1 = "pre_boundary_output",
    burst_hard_threshold_boundary_sources_v1 = "boundary_sources",
    burst_hard_threshold_boundary_attempts_v1 = "boundary_attempts",
    burst_hard_threshold_post_boundary_output_v1 = "post_boundary_output",
    burst_hard_threshold_receipt_v1 = "receipt"
  )
  key <- unname(hook_map[[hook_id]])
  if (is.null(key) || !identical(payload, replay[[key]])) return(FALSE)
  position <- match(hook_id, names(hook_map))
  prior <- if (is.na(position) || position <= 1L) {
    character()
  } else names(hook_map)[seq_len(position - 1L)]
  if (!length(prior)) return(TRUE)
  all(vapply(prior, function(prior_hook) {
    prior_key <- unname(hook_map[[prior_hook]])
    stored <- train_collector$observations[[prior_hook]]
    !is.null(stored) && identical(stored, replay[[prior_key]])
  }, logical(1)))
}
