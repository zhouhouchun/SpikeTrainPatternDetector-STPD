# Threshold-first Gate 1B raw canonical Pause observation -----------------
#
# This module observes the primary hf_protected raw canonical-Pause generator
# and its deterministic hard-boundary projection.  It deliberately stops
# before Burst ownership, contextual/inter-burst Pause generation, arbitration,
# or final materialization.

stpd_candidate_lineage_pause_raw_hooks <- function() {
  c(
    pause_raw_entry_v1 = "entry",
    pause_raw_support_v1 = "support",
    pause_raw_runs_v1 = "runs",
    pause_raw_output_v1 = "output",
    pause_boundary_projection_sources_v1 = "boundary_sources",
    pause_boundary_projection_output_v1 = "boundary_output",
    pause_raw_receipt_v1 = "receipt"
  )
}

stpd_candidate_lineage_pause_raw_begin_fields <- function() {
  c("train", "dat", "params", "vp", "min_isi_sec", "patterns")
}

stpd_candidate_lineage_pause_raw_hash <- function(row, stage) {
  stpd_candidate_lineage_candidate_source_hash(
    as.data.frame(row, stringsAsFactors = FALSE, check.names = FALSE),
    paste0("pause_raw_canonical_", stage)
  )
}

stpd_candidate_lineage_pause_raw_empty <- function(hook) {
  stpd_candidate_lineage_empty_hook_payload(hook)
}

stpd_candidate_lineage_pause_raw_config <- function(context) {
  n <- nrow(context$dat)
  patterns <- as.character(context$patterns %||% character())
  patterns <- patterns[!is.na(patterns)]
  requested <- "pause" %in% patterns
  threshold <- suppressWarnings(as.numeric(context$vp$pause_thr %||% NA_real_))[1L]
  strong_threshold <- suppressWarnings(as.numeric(
    context$vp$pause_strong_thr %||% threshold
  ))[1L]
  applicable <- requested && n > 2L && is.finite(threshold) && threshold > 0 &&
    is.finite(strong_threshold) && strong_threshold > 0
  status <- if (!requested) {
    "not_invoked_pause_pattern_not_requested"
  } else if (n <= 2L) {
    "invoked_insufficient_input"
  } else if (!is.finite(threshold) || threshold <= 0) {
    "invoked_invalid_pause_threshold"
  } else {
    "invoked_raw_canonical_pause_scan"
  }
  list(
    requested = requested, applicable = applicable,
    applicability = status, pause_threshold = threshold,
    pause_strong_threshold = strong_threshold,
    pause_strong_active = isTRUE(context$vp$pause_strong_active %||% FALSE),
    pause_strong_status = as.character(
      context$vp$pause_strong_status %||% "unresolved_q95_relative_fallback"
    )[1L]
  )
}

stpd_candidate_lineage_pause_raw_derive <- function(context, config) {
  dat <- context$dat
  n <- nrow(dat)
  if (!isTRUE(config$applicable)) {
    return(list(
      support = stpd_candidate_lineage_pause_raw_empty("pause_raw_support_v1"),
      runs = stpd_candidate_lineage_pause_raw_empty("pause_raw_runs_v1"),
      raw_candidates = data.frame()
    ))
  }
  isi <- suppressWarnings(as.numeric(dat$ISI_sec))
  artifact <- is_artifact_isi(isi, context$min_isi_sec)
  valid <- is.finite(isi) & !artifact
  if (length(valid)) valid[[1L]] <- FALSE
  vals <- stpd_event_core_valid_train_isis(
    isi, valid, context$min_isi_sec
  )
  pause_source_mode <- tolower(as.character(
    context$vp$pause_threshold_source_mode %||% "auto"
  )[1L])
  frozen_pause_threshold <- pause_source_mode %in%
    c("user", "manual", "default")
  base_floor <- stpd_event_core_pause_global_floor(
    isi, valid, context$vp, context$min_isi_sec
  )
  tonic_bounds <- stpd_event_core_tonic_adaptive_bounds(
    isi, valid, context$vp, context$min_isi_sec
  )
  tonic_guard_enabled <- length(vals) >= 10L
  tonic_guard <- if (tonic_guard_enabled &&
      is.finite(tonic_bounds$upper) && tonic_bounds$upper > 0) {
    tonic_bounds$upper * 1.15
  } else NA_real_
  effective_floor <- base_floor
  if (!frozen_pause_threshold && is.finite(tonic_guard)) {
    effective_floor <- max(base_floor, tonic_guard)
  }
  pp <- context$params$pause %||% list()
  eg <- context$params$event_grammar %||% list()
  local_factor <- max(1, stpd_event_core_num(
    eg$pause_relative_local_factor %||% pp$relative_local_factor %||% 1.55,
    1.55
  ))
  global_factor <- max(1, stpd_event_core_num(
    eg$pause_relative_global_factor %||% pp$relative_global_factor %||% 1.25,
    1.25
  ))
  global_median <- if (length(vals)) stats::median(vals, na.rm = TRUE) else NA_real_
  base_long <- valid & isi >= effective_floor
  base_long[is.na(base_long)] <- FALSE
  flag <- base_long
  local_median <- rep(NA_real_, n)
  local_gate <- rep("not_evaluated", n)
  global_gate <- rep("not_evaluated", n)
  relative_gate_applied <- FALSE
  if (relative_gate_applied) {
    long_idx <- which(base_long)
    for (ii in which(flag)) {
      loc <- get_local_median(
        isi, ii, exclude_idx = long_idx, min_isi_sec = context$min_isi_sec
      )
      if (!is.finite(loc)) {
        loc <- get_local_median(
          isi, ii, exclude_idx = ii, min_isi_sec = context$min_isi_sec
        )
      }
      local_median[[ii]] <- loc
      local_ok <- !is.finite(loc) || isi[[ii]] >= loc * local_factor
      global_ok <- !is.finite(global_median) ||
        isi[[ii]] >= global_median * global_factor
      local_gate[[ii]] <- if (local_ok) "pass" else "fail"
      global_gate[[ii]] <- if (global_ok) "pass" else "fail"
      flag[[ii]] <- local_ok && global_ok
    }
  }
  if (length(flag)) flag[[1L]] <- FALSE
  rows <- if (n >= 2L) seq.int(2L, n) else integer()
  support <- data.frame(
    train = rep(context$train, length(rows)),
    detector_row_index = as.integer(rows),
    isi_index = as.integer(seq_along(rows)),
    isi_sec = as.numeric(isi[rows]), artifact_isi = as.logical(artifact[rows]),
    valid_isi = as.logical(valid[rows]), base_long_member = as.logical(base_long[rows]),
    local_median_sec = as.numeric(local_median[rows]),
    global_median_sec = rep(as.numeric(global_median), length(rows)),
    local_contrast_gate = as.character(local_gate[rows]),
    global_contrast_gate = as.character(global_gate[rows]),
    final_pause_flag = as.logical(flag[rows]),
    pause_base_threshold_sec = rep(as.numeric(base_floor), length(rows)),
    pause_tonic_guard_threshold_sec = rep(as.numeric(tonic_guard), length(rows)),
    pause_effective_threshold_sec = rep(as.numeric(effective_floor), length(rows)),
    pause_relative_local_factor = rep(as.numeric(local_factor), length(rows)),
    pause_relative_global_factor = rep(as.numeric(global_factor), length(rows)),
    relative_gate_applied = rep(relative_gate_applied, length(rows)),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  runs0 <- stpd_event_core_bool_runs(flag)
  if (!nrow(runs0)) {
    return(list(
      support = support,
      runs = stpd_candidate_lineage_pause_raw_empty("pause_raw_runs_v1"),
      raw_candidates = data.frame()
    ))
  }
  run_results <- lapply(seq_len(nrow(runs0)), function(rr) {
    s <- as.integer(runs0$start_isi[[rr]])
    e <- as.integer(runs0$end_isi[[rr]])
    run_vals <- isi[stpd_event_core_safe_seq(s, e)]
    loc_vals <- vapply(stpd_event_core_safe_seq(s, e), function(ii) {
      get_local_median(
        isi, ii, exclude_idx = which(base_long),
        min_isi_sec = context$min_isi_sec
      )
    }, numeric(1))
    loc_med <- if (length(loc_vals)) {
      stats::median(loc_vals[is.finite(loc_vals)], na.rm = TRUE)
    } else NA_real_
    if (!is.finite(loc_med)) loc_med <- NA_real_
    score <- if (length(run_vals)) {
      max(run_vals, na.rm = TRUE) / effective_floor
    } else 1
    candidate_id <- paste0("event_core_pause_", rr)
    automatic_entry <- !frozen_pause_threshold
    candidate <- stpd_event_core_candidate_from_run(
      dat, s, e, context$params, context$vp, context$min_isi_sec,
      context$train, "event_core_pause_gap", "event_core_pause_gap", "pause",
      "event_core_pause_pass",
      if (automatic_entry) "automatic_pooled_q90_pause_gap" else
        "resolved_one_sided_pause_gap",
      "accept",
      score, 300,
      list(
        candidate_id = candidate_id,
        pause_threshold_sec = context$vp$pause_thr,
        pause_entry_threshold_sec = context$vp$pause_thr,
        pause_strong_threshold_sec = config$pause_strong_threshold,
        pause_strong_tail_active = config$pause_strong_active,
        pause_strong_tail_status = config$pause_strong_status,
        pause_base_threshold_sec = base_floor,
        pause_effective_threshold_sec = effective_floor,
        pause_tonic_guard_threshold_sec = tonic_guard,
        pause_local_median_sec = loc_med,
        pause_global_median_sec = global_median,
        pause_relative_local_factor = local_factor,
        pause_relative_global_factor = global_factor,
        gap_semantics = "canonical_pause",
        hard_for_event = TRUE,
        hard_for_state_direct_support = TRUE,
        envelope_bridge_eligible = FALSE,
        pause_candidate_decision = "accepted",
        pause_candidate_reason_code =
          if (automatic_entry && isTRUE(config$pause_strong_active)) {
            "automatic_stable_strong_tail_threshold"
          } else if (automatic_entry) {
            "automatic_q90_entry_strong_tail_unresolved"
          } else {
            "resolved_one_sided_pause_threshold"
          }
      )
    )
    emitted <- !is.null(candidate) && nrow(candidate) > 0L
    list(
      run = data.frame(
        train = context$train, run_ordinal = as.integer(rr),
        detector_start_row = s, detector_end_row = e,
        start_isi = as.integer(s - 1L), end_isi = as.integer(e - 1L),
        run_isi_n = as.integer(e - s + 1L), score = as.numeric(score),
        candidate_counter = as.integer(rr), candidate_id = candidate_id,
        construction_attempted = TRUE, candidate_emitted = emitted,
        terminal_status = if (emitted) {
          "candidate_emitted"
        } else "construction_failed",
        terminal_reason = if (emitted) {
          "raw_canonical_pause_run_emitted"
        } else "candidate_metrics_unavailable",
        # Filled only after reconstructed candidates close exactly to the
        # live scientific output; failures remain NA.
        output_candidate_ordinal = NA_integer_,
        candidate_payload_sha256 = if (emitted) {
          stpd_candidate_lineage_pause_raw_hash(candidate, "output")
        } else "",
        stringsAsFactors = FALSE, check.names = FALSE
      ),
      candidate = if (emitted) candidate else NULL
    )
  })
  candidate_rows <- lapply(run_results, `[[`, "candidate")
  candidate_rows <- candidate_rows[!vapply(candidate_rows, is.null, logical(1))]
  list(
    support = support,
    runs = dplyr::bind_rows(lapply(run_results, `[[`, "run")),
    raw_candidates = if (length(candidate_rows)) {
      dplyr::bind_rows(candidate_rows)
    } else data.frame()
  )
}

stpd_candidate_lineage_pause_raw_output <- function(context) {
  x <- context$scientific_raw_output
  if (!nrow(x)) return(stpd_candidate_lineage_pause_raw_empty("pause_raw_output_v1"))
  rows <- lapply(seq_len(nrow(x)), function(i) {
    row <- x[i, , drop = FALSE]
    s <- stpd_candidate_lineage_candidate_column(row, "start_isi", "integer")[[1L]]
    e <- stpd_candidate_lineage_candidate_column(row, "end_isi", "integer")[[1L]]
    data.frame(
      train = context$train, output_ordinal = as.integer(i),
      candidate_id = stpd_candidate_lineage_candidate_column(
        row, "candidate_id", "character"
      )[[1L]],
      candidate_payload_sha256 = stpd_candidate_lineage_pause_raw_hash(row, "output"),
      detector_start_row = s, detector_end_row = e,
      start_isi = as.integer(s - 1L), end_isi = as.integer(e - 1L),
      final_label = stpd_candidate_lineage_candidate_column(
        row, "final_label", "character"
      )[[1L]],
      action = stpd_candidate_lineage_candidate_column(row, "action", "character")[[1L]],
      gap_semantics = stpd_candidate_lineage_candidate_column(
        row, "gap_semantics", "character"
      )[[1L]],
      hard_for_event = stpd_candidate_lineage_candidate_column(
        row, "hard_for_event", "logical"
      )[[1L]],
      score = stpd_candidate_lineage_candidate_column(row, "score", "double")[[1L]],
      intended_semantic_track = "gap",
      scientific_role = "raw_generic_pause_anchor_proposal",
      selection_status = "not_observed_at_this_root",
      final_contextual_pause_status = "not_observed_in_this_patch",
      stringsAsFactors = FALSE, check.names = FALSE
    )
  })
  dplyr::bind_rows(rows)
}

stpd_candidate_lineage_pause_boundary_projection <- function(context, output) {
  raw <- context$scientific_raw_output
  if (!nrow(raw)) {
    return(list(
      sources = stpd_candidate_lineage_pause_raw_empty(
        "pause_boundary_projection_sources_v1"
      ),
      output = stpd_candidate_lineage_pause_raw_empty(
        "pause_boundary_projection_output_v1"
      )
    ))
  }
  semantics <- as.character(raw$gap_semantics %||% "")
  hard <- as.logical(raw$hard_for_event %||% FALSE)
  action <- as.character(raw$action %||% raw$decision_action %||% "accept")
  keep <- semantics == "canonical_pause" & hard & action == "accept"
  keep[is.na(keep)] <- FALSE
  sources <- lapply(seq_len(nrow(raw)), function(i) {
    data.frame(
      train = context$train, source_ordinal = as.integer(i),
      source_candidate_id = output$candidate_id[[i]],
      source_payload_sha256 = output$candidate_payload_sha256[[i]],
      detector_start_row = as.integer(raw$start_isi[[i]]),
      detector_end_row = as.integer(raw$end_isi[[i]]),
      gap_semantics = semantics[[i]], hard_for_event = hard[[i]],
      action = action[[i]], projected = keep[[i]],
      projection_reason = if (keep[[i]]) "canonical_pause_hard_accept" else if (
        semantics[[i]] != "canonical_pause") {
        "excluded_noncanonical_semantics"
      } else if (!isTRUE(hard[[i]])) {
        "excluded_not_hard_for_event"
      } else "excluded_nonaccept_action",
      boundary_ordinal = NA_integer_,
      stringsAsFactors = FALSE, check.names = FALSE
    )
  })
  sources <- dplyr::bind_rows(sources)
  boundaries <- context$scientific_boundaries
  if (!nrow(boundaries)) {
    boundary_output <- stpd_candidate_lineage_pause_raw_empty(
      "pause_boundary_projection_output_v1"
    )
  } else {
    boundary_rows <- lapply(seq_len(nrow(boundaries)), function(i) {
      s <- as.integer(boundaries$start_isi[[i]])
      e <- as.integer(boundaries$end_isi[[i]])
      src <- which(keep & as.integer(raw$start_isi) == s &
        as.integer(raw$end_isi) == e)
      sources$boundary_ordinal[src] <<- as.integer(i)
      ids <- output$candidate_id[src]
      hashes <- output$candidate_payload_sha256[src]
      data.frame(
        train = context$train, boundary_ordinal = as.integer(i),
        detector_start_row = s, detector_end_row = e,
        start_isi = as.integer(s - 1L), end_isi = as.integer(e - 1L),
        boundary_kind = as.character(boundaries$boundary_kind[[i]]),
        source_candidate_n = as.integer(length(src)),
        source_ordinals = paste(src, collapse = ";"),
        source_candidate_ids = paste(ids, collapse = ";"),
        source_payload_sha256 = stpd_threshold_first_hash_domain(
          "stpd-pause-raw-boundary-source-set-v1",
          data.frame(
            source_ordinal = as.integer(src), candidate_id = as.character(ids),
            payload_sha256 = as.character(hashes),
            stringsAsFactors = FALSE, check.names = FALSE
          )
        ),
        semantic_role = "canonical_pause_boundary_control",
        candidate_status = "not_a_candidate", selectable = FALSE,
        materialization_status = "not_applicable",
        stringsAsFactors = FALSE, check.names = FALSE
      )
    })
    boundary_output <- dplyr::bind_rows(boundary_rows)
  }
  list(sources = sources, output = boundary_output)
}

stpd_candidate_lineage_pause_raw_replay <- function(context) {
  required <- c(
    stpd_candidate_lineage_pause_raw_begin_fields(),
    "scientific_raw_output", "scientific_boundaries"
  )
  if (is.null(context) || !is.list(context) ||
      !all(required %in% names(context)) ||
      !is.character(context$train) || length(context$train) != 1L ||
      is.na(context$train) || !nzchar(context$train) ||
      !is.data.frame(context$dat) || !is.list(context$params) ||
      !is.list(context$vp) || !is.data.frame(context$scientific_raw_output) ||
      !is.data.frame(context$scientific_boundaries)) return(NULL)
  config <- stpd_candidate_lineage_pause_raw_config(context)
  expected_raw <- if (isTRUE(config$requested)) {
    stpd_event_core_detect_pause(
      context$dat, context$params, context$vp, context$min_isi_sec,
      context$train, burst_candidates = NULL
    )
  } else data.frame()
  expected_boundaries <- stpd_event_core_pause_hard_boundaries(expected_raw)
  if (!identical(expected_raw, context$scientific_raw_output) ||
      !identical(expected_boundaries, context$scientific_boundaries)) return(NULL)
  derived <- stpd_candidate_lineage_pause_raw_derive(context, config)
  if (!identical(
      derived$raw_candidates, context$scientific_raw_output
    )) return(NULL)
  output <- stpd_candidate_lineage_pause_raw_output(context)
  projection <- stpd_candidate_lineage_pause_boundary_projection(context, output)
  if (nrow(derived$runs)) {
    emitted <- which(derived$runs$candidate_emitted)
    if (length(emitted) != nrow(output)) return(NULL)
    if (!identical(
        derived$runs$candidate_id[emitted], output$candidate_id) ||
        !identical(
          derived$runs$detector_start_row[emitted],
          output$detector_start_row
        ) ||
        !identical(
          derived$runs$detector_end_row[emitted], output$detector_end_row
        ) ||
        !identical(
          derived$runs$candidate_payload_sha256[emitted],
          output$candidate_payload_sha256
        )) return(NULL)
    derived$runs$output_candidate_ordinal[emitted] <- seq_along(emitted)
  }
  entry <- data.frame(
    train = context$train, root_id = "raw_canonical_pause_parallel_root",
    applicability_status = config$applicability,
    pause_pattern_requested = config$requested,
    generator_invoked = config$requested,
    n_interval_rows = as.integer(nrow(context$dat)),
    n_physical_isi = as.integer(max(0L, nrow(context$dat) - 1L)),
    min_isi_sec = as.numeric(context$min_isi_sec),
    pause_threshold_sec = as.numeric(config$pause_threshold),
    generation_gated_by_requested_patterns = TRUE,
    manual_label_gate_status =
      "manual_labels_not_a_direct_pause_acceptance_gate",
    resolved_parameter_provenance_status = "bound_in_effective_params_and_vp",
    final_contextual_pause_status = "not_observed_in_this_patch",
    input_data_sha256 = stpd_threshold_first_hash_domain(
      "stpd-pause-raw-input-data-v1", context$dat
    ),
    params_payload_sha256 = stpd_threshold_first_hash_domain(
      "stpd-pause-raw-effective-params-v1", context$params
    ),
    vp_payload_sha256 = stpd_threshold_first_hash_domain(
      "stpd-pause-raw-resolved-vp-v1", context$vp
    ), stringsAsFactors = FALSE, check.names = FALSE
  )
  receipt <- data.frame(
    train = context$train, root_id = "raw_canonical_pause_parallel_root",
    applicability_status = config$applicability,
    pause_pattern_requested = config$requested,
    generator_invoked = config$requested,
    support_row_n = as.integer(nrow(derived$support)),
    flagged_isi_n = as.integer(sum(derived$support$final_pause_flag)),
    run_n = as.integer(nrow(derived$runs)),
    construction_failed_run_n = as.integer(sum(
      derived$runs$terminal_status == "construction_failed"
    )),
    raw_output_n = as.integer(nrow(output)),
    projected_source_n = as.integer(sum(projection$sources$projected)),
    boundary_output_n = as.integer(nrow(projection$output)),
    boundary_sort_applied = sum(projection$sources$projected) > 0L,
    boundary_deduplication_applied =
      sum(projection$sources$projected) > nrow(projection$output),
    scan_exhausted = config$applicable,
    cap_applied = FALSE,
    raw_candidate_semantic_track = "gap",
    boundary_semantic_role = "noncandidate_control_sidecar",
    context_adaptive_pause_status = "not_observed_in_this_patch",
    gap_selection_status = "not_observed_in_this_patch",
    recurrent_pause_evaluation_status =
      "not_evaluated_post_detection_only",
    publication_authority = FALSE,
    upstream_universe_status = "candidate_universe_unavailable",
    output_role =
      "canonical_pause_proposals_and_event_boundary_sources_not_final_selection",
    coverage_status = if (!config$requested) {
      "not_invoked_pause_pattern_not_requested"
    } else if (!config$applicable) {
      config$applicability
    } else "raw_canonical_pause_scan_and_boundary_projection_complete",
    entry_payload_sha256 = stpd_threshold_first_hash_domain(
      "stpd-pause-raw-entry-v1", entry
    ),
    support_payload_sha256 = stpd_threshold_first_hash_domain(
      "stpd-pause-raw-support-v1", derived$support
    ),
    run_payload_sha256 = stpd_threshold_first_hash_domain(
      "stpd-pause-raw-runs-v1", derived$runs
    ),
    raw_output_payload_sha256 = stpd_threshold_first_hash_domain(
      "stpd-pause-raw-output-v1", output
    ),
    boundary_source_payload_sha256 = stpd_threshold_first_hash_domain(
      "stpd-pause-raw-boundary-sources-v1", projection$sources
    ),
    boundary_output_payload_sha256 = stpd_threshold_first_hash_domain(
      "stpd-pause-raw-boundary-output-v1", projection$output
    ),
    scientific_raw_output_sha256 = stpd_threshold_first_hash_domain(
      "stpd-pause-raw-scientific-output-v1", expected_raw
    ),
    scientific_boundary_output_sha256 = stpd_threshold_first_hash_domain(
      "stpd-pause-raw-scientific-boundaries-v1", expected_boundaries
    ), stringsAsFactors = FALSE, check.names = FALSE
  )
  list(
    entry = entry, support = derived$support, runs = derived$runs,
    output = output, boundary_sources = projection$sources,
    boundary_output = projection$output, receipt = receipt
  )
}

stpd_candidate_lineage_begin_pause_raw_canonical <- function(
    train_collector, train, dat, params, vp, min_isi_sec, patterns) {
  if (is.null(train_collector)) return(invisible(NULL))
  train <- as.character(train)
  if (length(train) != 1L || is.na(train) || !nzchar(train)) {
    stpd_candidate_lineage_abort(
      "collector_train_scope_invalid",
      "Raw canonical-Pause observation requires one active train.",
      field = "train"
    )
  }
  stpd_candidate_lineage_validate_train_collector(
    train_collector, require_state = "open", train = train,
    validate_observations = FALSE
  )
  begin <- unserialize(serialize(list(
    train = train,
    dat = as.data.frame(dat, stringsAsFactors = FALSE, check.names = FALSE),
    params = params, vp = vp,
    min_isi_sec = suppressWarnings(as.numeric(min_isi_sec))[[1L]],
    patterns = as.character(patterns %||% character())
  ), NULL, version = 3L))
  entry <- train_collector$observations$hf_protected_impl_entry_v1
  if (!is.data.frame(entry) || nrow(entry) != 1L ||
      !identical(entry$train[[1L]], train) ||
      !identical(entry$n_interval_rows[[1L]], as.integer(nrow(begin$dat))) ||
      !identical(entry$min_isi_sec[[1L]], as.numeric(begin$min_isi_sec))) {
    stpd_candidate_lineage_abort(
      "collector_pause_raw_entry_not_closed",
      "Raw canonical-Pause root does not close to the primary detector entry."
    )
  }
  name <- "pause_raw_canonical_begin_context"
  if (exists(name, envir = train_collector, inherits = FALSE)) {
    if (!bindingIsLocked(name, train_collector) ||
        !identical(get(name, envir = train_collector), begin)) {
      stpd_candidate_lineage_abort(
        "collector_pause_raw_begin_context_conflict",
        "Raw canonical-Pause begin-context is absent or inconsistent."
      )
    }
  } else {
    assign(name, begin, envir = train_collector)
    lockBinding(name, train_collector)
  }
  invisible(begin)
}

stpd_candidate_lineage_capture_pause_raw_canonical <- function(
    train_collector, train, dat, params, vp, min_isi_sec, patterns,
    scientific_raw_output, scientific_boundaries) {
  if (is.null(train_collector)) return(invisible(NULL))
  train <- as.character(train)
  stpd_candidate_lineage_validate_train_collector(
    train_collector, require_state = "open", train = train,
    validate_observations = FALSE
  )
  begin_name <- "pause_raw_canonical_begin_context"
  if (!exists(begin_name, envir = train_collector, inherits = FALSE) ||
      !bindingIsLocked(begin_name, train_collector)) {
    stpd_candidate_lineage_abort(
      "collector_pause_raw_begin_context_missing",
      "Raw canonical-Pause capture requires its locked live begin-context."
    )
  }
  supplied <- unserialize(serialize(list(
    train = train,
    dat = as.data.frame(dat, stringsAsFactors = FALSE, check.names = FALSE),
    params = params, vp = vp,
    min_isi_sec = suppressWarnings(as.numeric(min_isi_sec))[[1L]],
    patterns = as.character(patterns %||% character())
  ), NULL, version = 3L))
  begin <- get(begin_name, envir = train_collector, inherits = FALSE)
  if (!identical(begin, supplied)) {
    stpd_candidate_lineage_abort(
      "collector_pause_raw_begin_context_conflict",
      "Raw canonical-Pause capture differs from its locked live begin-context."
    )
  }
  context <- begin
  context$scientific_raw_output <- as.data.frame(
    scientific_raw_output %||% data.frame(), stringsAsFactors = FALSE,
    check.names = FALSE
  )
  context$scientific_boundaries <- as.data.frame(
    scientific_boundaries %||% data.frame(), stringsAsFactors = FALSE,
    check.names = FALSE
  )
  replay <- stpd_candidate_lineage_pause_raw_replay(context)
  if (is.null(replay)) {
    stpd_candidate_lineage_abort(
      "collector_pause_raw_scientific_replay_failed",
      "Raw canonical-Pause output or boundary projection did not replay exactly."
    )
  }
  context_name <- "pause_raw_canonical_context"
  replay_name <- "pause_raw_canonical_replay_cache"
  if (exists(context_name, envir = train_collector, inherits = FALSE)) {
    if (!bindingIsLocked(context_name, train_collector) ||
        !identical(get(context_name, envir = train_collector), context) ||
        !exists(replay_name, envir = train_collector, inherits = FALSE) ||
        !bindingIsLocked(replay_name, train_collector) ||
        !identical(get(replay_name, envir = train_collector), replay)) {
      stpd_candidate_lineage_abort(
        "collector_pause_raw_context_conflict",
        "Raw canonical-Pause context or replay cache is inconsistent."
      )
    }
  } else {
    assign(context_name, context, envir = train_collector)
    lockBinding(context_name, train_collector)
    assign(replay_name, unserialize(serialize(replay, NULL, version = 3L)),
           envir = train_collector)
    lockBinding(replay_name, train_collector)
  }
  hooks <- stpd_candidate_lineage_pause_raw_hooks()
  for (hook in names(hooks)) {
    stpd_candidate_lineage_collector_capture(
      train_collector, hook, replay[[hooks[[hook]]]]
    )
  }
  invisible(replay)
}

stpd_candidate_lineage_pause_raw_payload_is_valid <- function(
    train_collector, hook_id, payload) {
  context_name <- "pause_raw_canonical_context"
  replay_name <- "pause_raw_canonical_replay_cache"
  if (!exists(context_name, envir = train_collector, inherits = FALSE) ||
      !bindingIsLocked(context_name, train_collector) ||
      !exists(replay_name, envir = train_collector, inherits = FALSE) ||
      !bindingIsLocked(replay_name, train_collector)) return(FALSE)
  context <- get(context_name, envir = train_collector, inherits = FALSE)
  replay <- get(replay_name, envir = train_collector, inherits = FALSE)
  begin <- get("pause_raw_canonical_begin_context", envir = train_collector,
               inherits = FALSE)
  fields <- stpd_candidate_lineage_pause_raw_begin_fields()
  if (!identical(context[fields], begin) ||
      !identical(context$train, train_collector$train) ||
      !("train" %in% names(payload)) || anyNA(payload$train) ||
      !all(payload$train == train_collector$train)) return(FALSE)
  hooks <- stpd_candidate_lineage_pause_raw_hooks()
  key <- unname(hooks[[hook_id]])
  if (is.null(key) || !identical(payload, replay[[key]])) return(FALSE)
  pos <- match(hook_id, names(hooks))
  prior <- if (is.na(pos) || pos <= 1L) character() else names(hooks)[seq_len(pos - 1L)]
  if (!length(prior)) return(TRUE)
  all(vapply(prior, function(h) {
    stored <- train_collector$observations[[h]]
    !is.null(stored) && identical(stored, replay[[hooks[[h]]]])
  }, logical(1)))
}

stpd_candidate_lineage_pause_raw_outputs_are_closed <- function(
    train_collector, raw_candidates, boundaries, science_context = NULL) {
  context_name <- "pause_raw_canonical_context"
  replay_name <- "pause_raw_canonical_replay_cache"
  receipt <- train_collector$observations$pause_raw_receipt_v1
  if (!exists(context_name, envir = train_collector, inherits = FALSE) ||
      !bindingIsLocked(context_name, train_collector) ||
      !exists(replay_name, envir = train_collector, inherits = FALSE) ||
      !bindingIsLocked(replay_name, train_collector) ||
      is.null(receipt) ||
      !stpd_candidate_lineage_pause_raw_payload_is_valid(
        train_collector, "pause_raw_receipt_v1", receipt
      )) return(FALSE)
  context <- get(context_name, envir = train_collector, inherits = FALSE)
  science_closed <- is.null(science_context) || (
    is.list(science_context) &&
    identical(context$train, science_context$train) &&
    identical(context$dat, science_context$dat) &&
    identical(context$params, science_context$params) &&
    identical(context$vp, science_context$vp) &&
    identical(context$min_isi_sec, science_context$min_isi_sec) &&
    (!("patterns" %in% names(science_context)) || identical(
      context$patterns,
      as.character(science_context$patterns %||% character())
    ))
  )
  isTRUE(science_closed) && identical(
    context$scientific_raw_output,
    as.data.frame(
      raw_candidates %||% data.frame(), stringsAsFactors = FALSE,
      check.names = FALSE
    )
  ) && identical(
    context$scientific_boundaries,
    as.data.frame(
      boundaries %||% data.frame(), stringsAsFactors = FALSE,
      check.names = FALSE
    )
  )
}
