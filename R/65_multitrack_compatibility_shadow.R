# Non-authoritative Phase 1B compatibility analysis for the multi-track shadow.
#
# This module consumes the Phase 1A pre-protection selection. It never mutates
# Phase 1A candidates, the legacy candidate audit, AUTO labels, or exports. Its
# derived fragments and decisions are explicitly provisional audit products.

stpd_multitrack_compatibility_policy <- function(params = NULL) {
  block <- stpd_multitrack_policy_block(params)
  list(
    policy_version = stpd_multitrack_fixed_policy_value(
      block, "phase1b_policy_version", "phase1b_compatibility_shadow_v1"
    ),
    hfs_dominance_group_floor = stpd_multitrack_compatibility_int(block$hfs_dominance_group_floor),
    hfs_dominance_group_fraction = stpd_multitrack_compatibility_num(block$hfs_dominance_group_fraction),
    hfs_dominance_distributed_coverage_min = stpd_multitrack_compatibility_num(block$hfs_dominance_distributed_coverage_min),
    hfs_dominance_majority_coverage_min = stpd_multitrack_compatibility_num(block$hfs_dominance_majority_coverage_min),
    tonic_fragment_min_spikes_floor = stpd_multitrack_compatibility_int(block$tonic_fragment_min_spikes_floor),
    tonic_fragment_mm_relax_lv_max = stpd_multitrack_compatibility_num(block$tonic_fragment_mm_relax_lv_max),
    tonic_fragment_mm_relax_cv_max = stpd_multitrack_compatibility_num(block$tonic_fragment_mm_relax_cv_max),
    tonic_fragment_mm_relaxed_max = stpd_multitrack_compatibility_num(block$tonic_fragment_mm_relaxed_max),
    tonic_fragment_seed_fraction_max = stpd_multitrack_compatibility_num(block$tonic_fragment_seed_fraction_max),
    hft_fragment_min_spikes_floor = stpd_multitrack_compatibility_int(block$hft_fragment_min_spikes_floor),
    hft_fragment_low_quantile = stpd_multitrack_compatibility_num(block$hft_fragment_low_quantile),
    hft_fragment_high_quantile = stpd_multitrack_compatibility_num(block$hft_fragment_high_quantile),
    hfs_fragment_min_spikes_floor = stpd_multitrack_compatibility_int(block$hfs_fragment_min_spikes_floor),
    hfs_fragment_tolerated_gap_fallback_min_sec = stpd_multitrack_compatibility_num(block$hfs_fragment_tolerated_gap_fallback_min_sec),
    hfs_fragment_tolerated_gap_fallback_max_sec = stpd_multitrack_compatibility_num(block$hfs_fragment_tolerated_gap_fallback_max_sec),
    hfs_fragment_tolerated_gap_bridge_multiplier = stpd_multitrack_compatibility_num(block$hfs_fragment_tolerated_gap_bridge_multiplier),
    hfs_fragment_tolerated_gap_q90_multiplier = stpd_multitrack_compatibility_num(block$hfs_fragment_tolerated_gap_q90_multiplier),
    hfs_fragment_short_fraction_lower_bound = stpd_multitrack_compatibility_num(block$hfs_fragment_short_fraction_lower_bound),
    hfs_fragment_allowed_large_fraction_floor = stpd_multitrack_compatibility_num(block$hfs_fragment_allowed_large_fraction_floor),
    hfs_fragment_max_consecutive_large_floor = stpd_multitrack_compatibility_int(block$hfs_fragment_max_consecutive_large_floor),
    hfs_fragment_q80_probability = stpd_multitrack_compatibility_num(block$hfs_fragment_q80_probability),
    hfs_fragment_q90_probability = stpd_multitrack_compatibility_num(block$hfs_fragment_q90_probability),
    hfs_fragment_short_fraction_min_floor = stpd_multitrack_compatibility_num(block$hfs_fragment_short_fraction_min_floor),
    hfs_fragment_short_fraction_relaxation = stpd_multitrack_compatibility_num(block$hfs_fragment_short_fraction_relaxation),
    hfs_fragment_q90_short_fraction_min_floor = stpd_multitrack_compatibility_num(block$hfs_fragment_q90_short_fraction_min_floor),
    hfs_fragment_q90_short_fraction_relaxation = stpd_multitrack_compatibility_num(block$hfs_fragment_q90_short_fraction_relaxation),
    hfs_fragment_bridge_fraction_min_floor = stpd_multitrack_compatibility_num(block$hfs_fragment_bridge_fraction_min_floor),
    hfs_fragment_tolerated_gap_fraction_min = stpd_multitrack_compatibility_num(block$hfs_fragment_tolerated_gap_fraction_min),
    variable_hfs_cv_min = stpd_multitrack_compatibility_num(block$variable_hfs_cv_min),
    variable_hfs_lv_min = stpd_multitrack_compatibility_num(block$variable_hfs_lv_min),
    variable_hfs_large_fraction_min = stpd_multitrack_compatibility_num(block$variable_hfs_large_fraction_min),
    variable_hfs_mm_audit_min = stpd_multitrack_compatibility_num(block$variable_hfs_mm_audit_min),
    packet_like_multi_group_min = stpd_multitrack_compatibility_int(block$packet_like_multi_group_min),
    packet_like_multi_group_coverage_min = stpd_multitrack_compatibility_num(block$packet_like_multi_group_coverage_min),
    packet_like_single_group_min = stpd_multitrack_compatibility_int(block$packet_like_single_group_min),
    packet_like_single_group_coverage_min = stpd_multitrack_compatibility_num(block$packet_like_single_group_coverage_min),
    packet_neighbor_max_gap_isi = stpd_multitrack_compatibility_int(block$packet_neighbor_max_gap_isi),
    tonic_hft_burst_rule = stpd_multitrack_fixed_policy_value(
      block, "tonic_hft_burst_rule", "non_destructive_event_overlay"
    ),
    hfs_burst_rule = stpd_multitrack_fixed_policy_value(
      block, "hfs_burst_rule", "overlay_with_dominance_evidence_only"
    ),
    packet_rule = stpd_multitrack_fixed_policy_value(
      block, "packet_rule", "diagnostic_annotation_only"
    ),
    variable_hfs_mm_rule = stpd_multitrack_fixed_policy_value(
      block, "variable_hfs_mm_rule", "audit_only_not_boolean_gate"
    )
  )
}

stpd_multitrack_compatibility_empty_tables <- function() {
  list(
    state_parents = data.frame(
      state_candidate_id = character(),
      state_candidate_key = character(),
      state_label = character(),
      start_isi = integer(),
      end_isi = integer(),
      selected_within_track_preserved = logical(),
      parent_consumed_in_provisional_policy = logical(),
      provisional_resolution_status = character(),
      split_kind = character(),
      fragment_count = integer(),
      overlay_alternative_recorded = logical(),
      overlay_alternative_computed = logical(),
      stringsAsFactors = FALSE
    ),
    state_fragments = data.frame(
      fragment_candidate_id = character(),
      root_candidate_id = character(),
      root_candidate_key = character(),
      parent_candidate_id = character(),
      parent_candidate_key = character(),
      state_label = character(),
      start_isi = integer(),
      end_isi = integer(),
      n_isi = integer(),
      n_spikes = integer(),
      fragment_index = integer(),
      split_kind = character(),
      split_boundary_candidate_ids = character(),
      gate_evaluated = logical(),
      gate_fidelity = character(),
      provisional_gate_pass = logical(),
      provisional_gate_status = character(),
      failed_checks = character(),
      n_valid_isi = integer(),
      min_spikes_required = integer(),
      CV = double(),
      LV = double(),
      MM = double(),
      hf_spiking_large_fraction = double(),
      root_phase1a_selection_preserved = logical(),
      provisional_fragment_only = logical(),
      stringsAsFactors = FALSE
    ),
    relationships = data.frame(
      relationship_type = character(),
      compatibility_rule = character(),
      source_candidate_id = character(),
      source_candidate_key = character(),
      target_candidate_id = character(),
      target_candidate_key = character(),
      overlap_start_isi = integer(),
      overlap_end_isi = integer(),
      policy_role = character(),
      track_policy_version = character(),
      non_destructive = logical(),
      stringsAsFactors = FALSE
    ),
    overlays = data.frame(
      state_candidate_id = character(),
      state_candidate_key = character(),
      state_label = character(),
      event_candidate_id = character(),
      event_candidate_key = character(),
      compatibility_rule = character(),
      policy_role = character(),
      would_retain_continuous_parent = logical(),
      changes_phase1a_selection = logical(),
      stringsAsFactors = FALSE
    ),
    hfs_dominance = data.frame(
      hfs_fragment_candidate_id = character(),
      root_hfs_candidate_id = character(),
      root_hfs_candidate_key = character(),
      start_isi = integer(),
      end_isi = integer(),
      n_isi = integer(),
      selected_event_candidate_count = integer(),
      selected_event_group_count = integer(),
      selected_event_covered_isi_n = integer(),
      selected_event_coverage = double(),
      contributor_candidate_ids = character(),
      contributor_candidate_keys = character(),
      merged_event_group_spans = character(),
      group_floor_threshold = integer(),
      group_fraction_threshold = double(),
      effective_min_group_count = integer(),
      distributed_coverage_threshold = double(),
      majority_coverage_threshold = double(),
      distributed_dominance_flag = logical(),
      majority_dominance_flag = logical(),
      threshold_dominance_flag = logical(),
      provisional_dominance_flag = logical(),
      fragment_gate_evaluated = logical(),
      fragment_provisional_gate_pass = logical(),
      dominance_evidence_applicable = logical(),
      provisional_state_status = character(),
      state_selected_within_track_preserved = logical(),
      destructive_action_applied = logical(),
      CV = double(),
      LV = double(),
      MM = double(),
      hf_spiking_large_fraction = double(),
      variable_by_cv = logical(),
      variable_by_lv = logical(),
      variable_by_large_fraction = logical(),
      variable_by_mm_audit_only = logical(),
      variable_hfs_gate_without_mm = logical(),
      legacy_variable_hfs_gate_with_mm = logical(),
      mm_used_in_boolean_gate = logical(),
      packet_like_annotation = logical(),
      packet_neighbor_annotation = logical(),
      packet_neighbor_source_fragment_ids = character(),
      packet_annotations_destructive = logical(),
      stringsAsFactors = FALSE
    ),
    invariants = data.frame(
      invariant = character(),
      severity = character(),
      state_candidate_id = character(),
      other_candidate_id = character(),
      message = character(),
      stringsAsFactors = FALSE
    )
  )
}

stpd_multitrack_compatibility_num <- function(x, default = NA_real_) {
  out <- suppressWarnings(as.numeric(x))[1]
  if (length(out) == 0L || !is.finite(out)) default else out
}

stpd_multitrack_compatibility_int <- function(x, default = NA_integer_) {
  out <- suppressWarnings(as.integer(x))[1]
  if (length(out) == 0L || !is.finite(out)) default else out
}

stpd_multitrack_compatibility_text <- function(x, default = "") {
  out <- as.character(x)[1]
  if (length(out) == 0L || is.na(out) || !nzchar(out)) default else out
}

stpd_multitrack_compatibility_candidate_key <- function(row) {
  id <- stpd_multitrack_compatibility_text(row$candidate_id, "anonymous_candidate")
  layer <- stpd_multitrack_compatibility_text(row$candidate_layer, "unknown_layer")
  label <- stpd_multitrack_compatibility_text(row$final_label, "unknown_label")
  start <- stpd_multitrack_compatibility_int(row$start_isi)
  end <- stpd_multitrack_compatibility_int(row$end_isi)
  paste(id, layer, label, start, end, sep = "|")
}

stpd_multitrack_compatibility_fragment_id <- function(root_key, start_isi, end_isi) {
  key <- paste(root_key, as.integer(start_isi), as.integer(end_isi), sep = "|")
  paste0("phase1b_state_child_", digest::digest(key, algo = "xxhash64", serialize = FALSE))
}

stpd_multitrack_compatibility_interval_overlap <- function(a_start, a_end, b_start, b_end) {
  a_start <- stpd_multitrack_compatibility_int(a_start)
  a_end <- stpd_multitrack_compatibility_int(a_end)
  b_start <- stpd_multitrack_compatibility_int(b_start)
  b_end <- stpd_multitrack_compatibility_int(b_end)
  if (any(!is.finite(c(a_start, a_end, b_start, b_end)))) return(0L)
  as.integer(max(0L, min(a_end, b_end) - max(a_start, b_start) + 1L))
}

stpd_multitrack_compatibility_subtract_intervals <- function(span_start, span_end, cuts) {
  span_start <- stpd_multitrack_compatibility_int(span_start)
  span_end <- stpd_multitrack_compatibility_int(span_end)
  if (!is.finite(span_start) || !is.finite(span_end) || span_end < span_start) {
    return(data.frame(start_isi = integer(), end_isi = integer()))
  }
  if (is.null(cuts) || nrow(cuts) == 0L) {
    return(data.frame(start_isi = span_start, end_isi = span_end))
  }

  starts <- pmax(span_start, suppressWarnings(as.integer(cuts$start_isi)))
  ends <- pmin(span_end, suppressWarnings(as.integer(cuts$end_isi)))
  ok <- is.finite(starts) & is.finite(ends) & ends >= starts
  starts <- starts[ok]
  ends <- ends[ok]
  if (length(starts) == 0L) {
    return(data.frame(start_isi = span_start, end_isi = span_end))
  }

  ord <- order(starts, ends, method = "radix")
  starts <- starts[ord]
  ends <- ends[ord]
  merged_start <- integer()
  merged_end <- integer()
  cur_start <- starts[1]
  cur_end <- ends[1]
  if (length(starts) > 1L) {
    for (i in 2:length(starts)) {
      if (starts[i] <= cur_end + 1L) {
        cur_end <- max(cur_end, ends[i])
      } else {
        merged_start <- c(merged_start, cur_start)
        merged_end <- c(merged_end, cur_end)
        cur_start <- starts[i]
        cur_end <- ends[i]
      }
    }
  }
  merged_start <- c(merged_start, cur_start)
  merged_end <- c(merged_end, cur_end)

  fragments <- list()
  cursor <- span_start
  for (i in seq_along(merged_start)) {
    if (cursor < merged_start[i]) {
      fragments[[length(fragments) + 1L]] <- data.frame(
        start_isi = cursor,
        end_isi = merged_start[i] - 1L
      )
    }
    cursor <- max(cursor, merged_end[i] + 1L)
  }
  if (cursor <= span_end) {
    fragments[[length(fragments) + 1L]] <- data.frame(start_isi = cursor, end_isi = span_end)
  }
  if (length(fragments) == 0L) {
    return(data.frame(start_isi = integer(), end_isi = integer()))
  }
  out <- dplyr::bind_rows(fragments)
  out$start_isi <- as.integer(out$start_isi)
  out$end_isi <- as.integer(out$end_isi)
  out
}

stpd_multitrack_compatibility_selected <- function(phase1a_shadow) {
  candidates <- as.data.frame(phase1a_shadow$candidates, stringsAsFactors = FALSE)
  if (nrow(candidates) == 0L || !("selected_within_track" %in% names(candidates))) {
    return(candidates[FALSE, , drop = FALSE])
  }
  selected <- as.logical(candidates$selected_within_track)
  selected[is.na(selected)] <- FALSE
  out <- candidates[selected, , drop = FALSE]
  if (nrow(out) > 0L) {
    ord <- stpd_multitrack_shadow_canonical_order(out, include_track = TRUE)
    out <- out[ord, , drop = FALSE]
    rownames(out) <- NULL
  }
  out
}

stpd_multitrack_compatibility_join_ids <- function(x) {
  x <- as.character(x)
  x <- sort(unique(x[!is.na(x) & nzchar(x)]), method = "radix")
  paste(x, collapse = ";")
}

stpd_multitrack_compatibility_interval_union <- function(events, span_start, span_end) {
  span_start <- stpd_multitrack_compatibility_int(span_start)
  span_end <- stpd_multitrack_compatibility_int(span_end)
  empty <- list(
    event_count = 0L, group_count = 0L, covered_n = 0L, coverage = 0,
    contributor_candidate_ids = "", contributor_candidate_keys = "", group_spans = ""
  )
  if (!is.finite(span_start) || !is.finite(span_end) || span_end < span_start ||
      is.null(events) || nrow(events) == 0L) return(empty)

  starts <- pmax(span_start, suppressWarnings(as.integer(events$start_isi)))
  ends <- pmin(span_end, suppressWarnings(as.integer(events$end_isi)))
  ok <- is.finite(starts) & is.finite(ends) & ends >= starts
  if (!any(ok)) return(empty)
  events <- events[ok, , drop = FALSE]
  starts <- starts[ok]
  ends <- ends[ok]
  ids <- as.character(events$candidate_id)
  keys <- as.character(events$.candidate_key)
  ord <- order(starts, ends, keys, method = "radix")
  starts <- starts[ord]
  ends <- ends[ord]

  group_starts <- integer()
  group_ends <- integer()
  cur_start <- starts[1]
  cur_end <- ends[1]
  if (length(starts) > 1L) {
    for (i in 2:length(starts)) {
      # Adjacent selected spans are one physical group for dominance evidence.
      if (starts[i] <= cur_end + 1L) {
        cur_end <- max(cur_end, ends[i])
      } else {
        group_starts <- c(group_starts, cur_start)
        group_ends <- c(group_ends, cur_end)
        cur_start <- starts[i]
        cur_end <- ends[i]
      }
    }
  }
  group_starts <- c(group_starts, cur_start)
  group_ends <- c(group_ends, cur_end)
  covered_n <- sum(group_ends - group_starts + 1L)
  span_n <- span_end - span_start + 1L
  list(
    event_count = as.integer(nrow(events)),
    group_count = as.integer(length(group_starts)),
    covered_n = as.integer(covered_n),
    coverage = covered_n / max(1L, span_n),
    contributor_candidate_ids = stpd_multitrack_compatibility_join_ids(ids),
    contributor_candidate_keys = stpd_multitrack_compatibility_join_ids(keys),
    group_spans = paste0(group_starts, "-", group_ends, collapse = ";")
  )
}

stpd_multitrack_compatibility_fragment_regate <- function(
    label, start_isi, end_isi, dat = NULL, params = NULL, variable_params = NULL,
    min_isi_sec = 0.001, parent = NULL, policy = NULL) {
  result <- list(
    gate_evaluated = FALSE,
    gate_fidelity = "not_evaluated__raw_isi_or_effective_gate_parameters_unavailable",
    provisional_gate_pass = NA,
    provisional_gate_status = "not_evaluated",
    failed_checks = "",
    n_valid_isi = NA_integer_,
    min_spikes_required = NA_integer_,
    CV = stpd_multitrack_compatibility_num(parent$CV),
    LV = stpd_multitrack_compatibility_num(parent$LV),
    MM = stpd_multitrack_compatibility_num(parent$MM),
    large_fraction = stpd_multitrack_compatibility_num(parent$hf_spiking_large_fraction)
  )
  label <- stpd_multitrack_compatibility_text(label)
  start_isi <- stpd_multitrack_compatibility_int(start_isi)
  end_isi <- stpd_multitrack_compatibility_int(end_isi)
  if (is.null(dat) || is.null(variable_params) || !("ISI_sec" %in% names(dat)) ||
      !is.finite(start_isi) || !is.finite(end_isi) || start_isi < 2L ||
      end_isi < start_isi || end_isi > nrow(dat)) return(result)

  vp <- variable_params
  params <- params %||% list()
  policy <- policy %||% stpd_multitrack_compatibility_policy(params)
  required_fields <- switch(label,
    tonic = c(
      "tonic_min_spikes", "tonic_min", "tonic_max", "tonic_burst_overlap_guard",
      "tonic_burst_overlap_guard_factor", "tonic_burst_overlap_lower_quantile",
      "tonic_burst_overlap_low_fraction_max", "tonic_burst_overlap_reference_quantile",
      "tonic_burst_overlap_ref", "tonic_lv_max", "tonic_mm_max", "tonic_mm_min",
      "seed_low", "seed_high", "pause_thr"
    ),
    high_frequency_tonic = c(
      "hf_tonic_min_spikes", "hf_tonic_high_max", "hf_tonic_floor",
      "hf_tonic_low_tail_max", "seed_low", "seed_high",
      "hf_tonic_burst_core_veto", "hf_tonic_core_veto_min_isi_n",
      "hf_tonic_cv_max", "hf_tonic_lv_max", "hf_tonic_mm_max"
    ),
    high_frequency_spiking = c(
      "hf_spiking_min_spikes", "hf_spiking_min_duration",
      "hf_spiking_short_upper", "hf_spiking_q80_max", "hf_spiking_q90_max",
      "hf_spiking_epoch_bridge", "hf_spiking_short_fraction_min",
      "hf_spiking_allowed_large_frac", "hf_spiking_max_consec_large"
    ),
    character()
  )
  missing_fields <- required_fields[!vapply(required_fields, function(name) {
    value <- vp[[name]]
    !is.null(value) && length(value) > 0L && !all(is.na(value))
  }, logical(1))]
  if (length(missing_fields) > 0L) {
    result$gate_fidelity <- "not_evaluated__missing_required_variable_params"
    result$provisional_gate_status <- "not_evaluated__missing_required_variable_params"
    result$failed_checks <- paste0("missing_variable_params:", paste(missing_fields, collapse = ","))
    return(result)
  }
  isi <- suppressWarnings(as.numeric(dat$ISI_sec))
  art <- is_artifact_isi(isi, min_isi_sec)
  idx <- start_isi:end_isi
  vals <- isi[idx]
  vals <- vals[is.finite(vals) & !art[idx]]
  if (length(vals) == 0L) {
    result$gate_evaluated <- TRUE
    result$gate_fidelity <- "provisional_feature_recheck_not_full_candidate_regeneration"
    result$provisional_gate_pass <- FALSE
    result$provisional_gate_status <- "failed__no_valid_isi"
    result$failed_checks <- "no_valid_isi"
    result$n_valid_isi <- 0L
    return(result)
  }

  result$gate_evaluated <- TRUE
  result$gate_fidelity <- "provisional_feature_recheck_not_full_candidate_regeneration"
  result$n_valid_isi <- as.integer(length(vals))
  result$CV <- stpd_event_core_cv(vals)
  result$LV <- stpd_event_core_lv(vals)
  result$MM <- stpd_event_core_mm(vals)
  n_spikes <- end_isi - start_isi + 2L
  failed <- character()

  if (identical(label, "tonic")) {
    min_spikes <- max(
      policy$tonic_fragment_min_spikes_floor,
      stpd_multitrack_compatibility_int(vp$tonic_min_spikes)
    )
    result$min_spikes_required <- min_spikes
    if (n_spikes < min_spikes) failed <- c(failed, "min_spikes")
    valid_all <- is.finite(isi) & !art
    if (length(valid_all) > 0L) valid_all[1] <- FALSE
    bounds <- stpd_event_core_tonic_adaptive_bounds(isi, valid_all, vp, min_isi_sec)
    overlap_p <- list(
      burst_overlap_guard = vp$tonic_burst_overlap_guard,
      burst_overlap_guard_factor = vp$tonic_burst_overlap_guard_factor,
      burst_overlap_lower_quantile = vp$tonic_burst_overlap_lower_quantile,
      burst_overlap_low_fraction_max = vp$tonic_burst_overlap_low_fraction_max,
      burst_overlap_reference_quantile = vp$tonic_burst_overlap_reference_quantile
    )
    # Audit the old overlap guard without allowing an Event-like ISI pattern to
    # reject State support (D-021).
    invisible(stpd_tonic_burst_overlap_ok(
      vals, vp$tonic_burst_overlap_ref, p = overlap_p,
      min_isi_sec = min_isi_sec
    ))
    if (is.finite(result$LV) && result$LV > vp$tonic_lv_max) failed <- c(failed, "lv_max")
    # Canonical Tonic parameters are authoritative. The historical fragment
    # policy fields are fallback aliases for saved pre-migration configurations.
    mm_vp <- vp
    mm_vp$tonic_mm_relax_lv_max <- stpd_first_nonnull(
      mm_vp$tonic_mm_relax_lv_max,
      policy$tonic_fragment_mm_relax_lv_max
    )
    mm_vp$tonic_mm_relax_cv_max <- stpd_first_nonnull(
      mm_vp$tonic_mm_relax_cv_max,
      policy$tonic_fragment_mm_relax_cv_max
    )
    mm_vp$tonic_mm_relaxed_max <- stpd_first_nonnull(
      mm_vp$tonic_mm_relaxed_max,
      policy$tonic_fragment_mm_relaxed_max
    )
    mm_gate <- stpd_tonic_mm_gate(
      mm_vp, cv = result$CV, lv = result$LV
    )
    mm_max <- mm_gate$effective_max
    if (is.finite(result$MM) && result$MM > mm_max) failed <- c(failed, "mm_max")
    if (is.finite(result$MM) && result$MM < vp$tonic_mm_min) failed <- c(failed, "mm_min")
    seed_frac <- mean(vals >= vp$seed_low & vals <= vp$seed_high, na.rm = TRUE)
    # The adaptive bounds are intentionally computed for audit even though this
    # fragment pass does not rerun state-run construction.
    if (!is.finite(bounds$lower) || !is.finite(bounds$upper)) failed <- c(failed, "adaptive_bounds")
  } else if (identical(label, "high_frequency_tonic")) {
    min_spikes <- max(
      policy$hft_fragment_min_spikes_floor,
      stpd_multitrack_compatibility_int(vp$hf_tonic_min_spikes)
    )
    result$min_spikes_required <- min_spikes
    if (n_spikes < min_spikes) failed <- c(failed, "min_spikes")
    high_max <- stpd_multitrack_compatibility_num(vp$hf_tonic_high_max)
    floor <- stpd_multitrack_compatibility_num(vp$hf_tonic_floor)
    core_vals <- vals[!is.finite(high_max) | vals <= high_max]
    if (length(core_vals) == 0L) {
      failed <- c(failed, "no_hf_tonic_core")
    } else {
      q10 <- stpd_event_core_quantile(core_vals, policy$hft_fragment_low_quantile)
      q90 <- stpd_event_core_quantile(core_vals, policy$hft_fragment_high_quantile)
      low_tail <- if (is.finite(floor)) mean(core_vals < floor, na.rm = TRUE) else 0
      if (is.finite(high_max) && (!is.finite(q90) || q90 > high_max)) failed <- c(failed, "q90_max")
      if (is.finite(floor) && !((is.finite(q10) && q10 >= floor) ||
          low_tail <= vp$hf_tonic_low_tail_max)) failed <- c(failed, "low_tail")
    }
    # A Burst-like core is an Event-axis observation, not a State rejection
    # rule.  It may be retained in audit features, but it cannot fail the HFT
    # fragment gate or alter State geometry (D-021).
    if (is.finite(result$CV) && result$CV > vp$hf_tonic_cv_max) failed <- c(failed, "cv_max")
    if (is.finite(result$LV) && result$LV > vp$hf_tonic_lv_max) failed <- c(failed, "lv_max")
    if (is.finite(result$MM) && result$MM > vp$hf_tonic_mm_max) failed <- c(failed, "mm_max")
  } else if (identical(label, "high_frequency_spiking")) {
    hp <- params$highfreq %||% list()
    min_spikes <- stpd_multitrack_compatibility_int(
      vp$hf_spiking_min_spikes %||% hp$spiking_min_spikes
    )
    min_spikes <- max(policy$hfs_fragment_min_spikes_floor, min_spikes)
    result$min_spikes_required <- min_spikes
    if (n_spikes < min_spikes) failed <- c(failed, "min_spikes")
    min_duration <- stpd_multitrack_compatibility_num(vp$hf_spiking_min_duration, 0)
    if (min_duration > 0 && "timestamp_sec" %in% names(dat)) {
      duration <- suppressWarnings(as.numeric(dat$timestamp_sec[end_isi]) -
        as.numeric(dat$timestamp_sec[start_isi - 1L]))
      if (!is.finite(duration) || duration < min_duration) failed <- c(failed, "min_duration")
    }
    short_upper <- stpd_multitrack_compatibility_num(
      vp$hf_spiking_short_upper %||% vp$hf_spiking_q90_max
    )
    q80_max <- stpd_multitrack_compatibility_num(
      vp$hf_spiking_q80_max %||% vp$hf_spiking_q90_max
    )
    q90_max <- stpd_multitrack_compatibility_num(vp$hf_spiking_q90_max)
    bridge <- stpd_multitrack_compatibility_num(vp$hf_spiking_epoch_bridge)
    tolerated_gap <- stpd_multitrack_compatibility_num(hp$spiking_tolerated_gap_ISI_sec)
    if (!is.finite(tolerated_gap) || tolerated_gap <= 0) {
      tolerated_gap <- max(
        policy$hfs_fragment_tolerated_gap_fallback_min_sec,
        min(
          policy$hfs_fragment_tolerated_gap_fallback_max_sec,
          max(
            policy$hfs_fragment_tolerated_gap_bridge_multiplier * bridge,
            policy$hfs_fragment_tolerated_gap_q90_multiplier * q90_max
          )
        )
      )
    }
    short_fraction_min <- min(max(stpd_multitrack_compatibility_num(
      vp$hf_spiking_short_fraction_min
    ), policy$hfs_fragment_short_fraction_lower_bound), 1)
    allowed_large <- max(
      stpd_multitrack_compatibility_num(vp$hf_spiking_allowed_large_frac),
      policy$hfs_fragment_allowed_large_fraction_floor
    )
    max_consec_large <- max(
      policy$hfs_fragment_max_consecutive_large_floor,
      stpd_multitrack_compatibility_int(vp$hf_spiking_max_consec_large)
    )
    q80 <- stpd_event_core_quantile(vals, policy$hfs_fragment_q80_probability)
    q90 <- stpd_event_core_quantile(vals, policy$hfs_fragment_q90_probability)
    short_frac <- mean(vals <= short_upper, na.rm = TRUE)
    q90_short_frac <- mean(vals <= q90_max, na.rm = TRUE)
    bridge_frac <- mean(vals <= bridge, na.rm = TRUE)
    large_flag <- vals > bridge
    result$large_fraction <- mean(large_flag, na.rm = TRUE)
    strict_q90 <- is.finite(q90) && q90 <= q90_max
    robust_q80 <- is.finite(q80) && q80 <= q80_max
    majority <- short_frac >= max(
      policy$hfs_fragment_short_fraction_min_floor,
      short_fraction_min - policy$hfs_fragment_short_fraction_relaxation
    ) || q90_short_frac >= max(
      policy$hfs_fragment_q90_short_fraction_min_floor,
      short_fraction_min - policy$hfs_fragment_q90_short_fraction_relaxation
    ) || bridge_frac >= max(
      policy$hfs_fragment_bridge_fraction_min_floor, short_fraction_min
    )
    if (!(strict_q90 || (robust_q80 && majority))) failed <- c(failed, "compactness")
    if (mean(vals <= tolerated_gap, na.rm = TRUE) <
        policy$hfs_fragment_tolerated_gap_fraction_min) {
      failed <- c(failed, "tolerated_gap")
    }
    if (result$large_fraction > allowed_large) failed <- c(failed, "large_fraction")
    if (stpd_event_core_max_consecutive_true(large_flag) > max_consec_large) {
      failed <- c(failed, "max_consecutive_large")
    }
  } else {
    result$gate_evaluated <- FALSE
    result$gate_fidelity <- "not_evaluated__unsupported_state_label"
    result$provisional_gate_status <- "not_evaluated__unsupported_state_label"
    return(result)
  }

  failed <- unique(failed)
  result$provisional_gate_pass <- length(failed) == 0L
  result$failed_checks <- paste(failed, collapse = ";")
  result$provisional_gate_status <- if (result$provisional_gate_pass) {
    "provisional_pass__feature_recheck"
  } else {
    paste0("provisional_fail__", result$failed_checks)
  }
  result
}

stpd_multitrack_compatibility_shadow <- function(
    phase1a_shadow, dat = NULL, params = NULL, variable_params = NULL,
    min_isi_sec = 0.001) {
  if (inherits(phase1a_shadow, "stpd_multitrack_compatibility_shadow")) {
    return(phase1a_shadow)
  }
  if (is.null(phase1a_shadow) || !is.list(phase1a_shadow) ||
      is.null(phase1a_shadow$candidates)) {
    stop("phase1a_shadow must contain the Phase 1A candidate ledger", call. = FALSE)
  }

  policy <- stpd_multitrack_compatibility_policy(params)
  empty_tables <- stpd_multitrack_compatibility_empty_tables()
  selected <- stpd_multitrack_compatibility_selected(phase1a_shadow)
  if (nrow(selected) > 0L) {
    selected$.candidate_key <- vapply(seq_len(nrow(selected)), function(i) {
      stpd_multitrack_compatibility_candidate_key(selected[i, , drop = FALSE])
    }, character(1))
  } else {
    selected$.candidate_key <- character()
  }
  # Reuse the Phase 1A normalizer so a label cannot enter one semantic track
  # and then silently disappear from compatibility processing solely because
  # it used spaces or hyphens rather than underscores.
  labels <- stpd_multitrack_shadow_normalize_label(
    selected$final_label %||% character(nrow(selected))
  )
  tracks <- as.character(selected$semantic_track %||% character(nrow(selected)))
  states <- selected[tracks == "state", , drop = FALSE]
  events <- selected[tracks == "event" & labels %in% c(
    "burst", "long_burst", "high_frequency_burst", "hf_burst"
  ), , drop = FALSE]
  gaps <- selected[tracks == "gap" & labels == "pause", , drop = FALSE]
  state_hard <- as.logical(gaps$hard_for_state_direct_support %||% TRUE)
  state_hard[is.na(state_hard)] <- FALSE
  state_split_gaps <- gaps[state_hard, , drop = FALSE]
  reviews <- selected[tracks == "review", , drop = FALSE]

  relationship_rows <- list()
  parent_rows <- list()
  fragment_rows <- list()
  overlay_rows <- list()
  invariant_rows <- list()

  add_relationship <- function(type, rule, source, target_id, target_key,
                               overlap_start, overlap_end, role = "active_shadow") {
    relationship_rows[[length(relationship_rows) + 1L]] <<- data.frame(
      relationship_type = type,
      compatibility_rule = rule,
      source_candidate_id = stpd_multitrack_compatibility_text(source$candidate_id),
      source_candidate_key = stpd_multitrack_compatibility_text(source$.candidate_key),
      target_candidate_id = as.character(target_id),
      target_candidate_key = as.character(target_key),
      overlap_start_isi = as.integer(overlap_start),
      overlap_end_isi = as.integer(overlap_end),
      policy_role = role,
      track_policy_version = policy$policy_version,
      non_destructive = TRUE,
      stringsAsFactors = FALSE
    )
  }

  if (nrow(states) > 0L) {
    for (i in seq_len(nrow(states))) {
      parent <- states[i, , drop = FALSE]
      # Use the same canonical label normalizer as Phase 1A. A State label that
      # contains spaces or hyphens must not enter the State track successfully
      # and then silently miss its compatibility rules here.
      label <- stpd_multitrack_shadow_normalize_label(
        stpd_multitrack_compatibility_text(parent$final_label)
      )
      start <- stpd_multitrack_compatibility_int(parent$start_isi)
      end <- stpd_multitrack_compatibility_int(parent$end_isi)
      parent_id <- stpd_multitrack_compatibility_text(parent$candidate_id)
      parent_key <- stpd_multitrack_compatibility_text(parent$.candidate_key)
      if (!is.finite(start) || !is.finite(end) || end < start) next

      pause_overlap <- state_split_gaps[
        vapply(seq_len(nrow(state_split_gaps)), function(j) {
          stpd_multitrack_compatibility_interval_overlap(
            start, end, state_split_gaps$start_isi[j],
            state_split_gaps$end_isi[j]
          ) > 0L
        }, logical(1)),
        , drop = FALSE
      ]
      burst_overlap <- events[
        vapply(seq_len(nrow(events)), function(j) {
          stpd_multitrack_compatibility_interval_overlap(
            start, end, events$start_isi[j], events$end_isi[j]
          ) > 0L
        }, logical(1)),
        , drop = FALSE
      ]

      # A canonical Pause is a hard boundary for direct State support. Burst is
      # an orthogonal Event and must never be used to split or reject a State.
      split_cuts <- if (label %in% c(
        "high_frequency_spiking", "high_frequency_tonic", "tonic"
      )) {
        pause_overlap
      } else {
        selected[FALSE, , drop = FALSE]
      }
      fragments <- stpd_multitrack_compatibility_subtract_intervals(start, end, split_cuts)
      split_kind <- if (nrow(pause_overlap) > 0L && label %in% c(
        "high_frequency_spiking", "high_frequency_tonic", "tonic"
      )) {
        "pause_boundary"
      } else {
        "none"
      }
      parent_rows[[length(parent_rows) + 1L]] <- data.frame(
        state_candidate_id = parent_id,
        state_candidate_key = parent_key,
        state_label = label,
        start_isi = start,
        end_isi = end,
        selected_within_track_preserved = TRUE,
        parent_consumed_in_provisional_policy = split_kind != "none",
        provisional_resolution_status = if (split_kind == "none") {
          "parent_retained_in_shadow"
        } else {
          paste0("parent_consumed_by_", split_kind, "__phase1a_selection_unchanged")
        },
        split_kind = split_kind,
        fragment_count = as.integer(nrow(fragments)),
        overlay_alternative_recorded = nrow(burst_overlap) > 0L,
        overlay_alternative_computed = nrow(burst_overlap) > 0L,
        stringsAsFactors = FALSE
      )

      if (nrow(pause_overlap) > 0L && label %in% c(
        "high_frequency_spiking", "high_frequency_tonic", "tonic"
      )) {
        for (j in seq_len(nrow(pause_overlap))) {
          os <- max(start, as.integer(pause_overlap$start_isi[j]))
          oe <- min(end, as.integer(pause_overlap$end_isi[j]))
          add_relationship(
            "gap_boundary", "state_pause_direct_support_split",
            pause_overlap[j, , drop = FALSE],
            parent_id, parent_key, os, oe
          )
        }
      }
      if (nrow(burst_overlap) > 0L &&
          label %in% c("high_frequency_tonic", "tonic")) {
        for (j in seq_len(nrow(burst_overlap))) {
          os <- max(start, as.integer(burst_overlap$start_isi[j]))
          oe <- min(end, as.integer(burst_overlap$end_isi[j]))
          add_relationship(
            "event_overlay_within_state", "burst_state_non_destructive_overlay",
            burst_overlap[j, , drop = FALSE],
            parent_id, parent_key, os, oe
          )
          overlay_rows[[length(overlay_rows) + 1L]] <- data.frame(
            state_candidate_id = parent_id,
            state_candidate_key = parent_key,
            state_label = label,
            event_candidate_id = stpd_multitrack_compatibility_text(burst_overlap$candidate_id[j]),
            event_candidate_key = stpd_multitrack_compatibility_text(burst_overlap$.candidate_key[j]),
            compatibility_rule = "burst_state_non_destructive_overlay",
            policy_role = "active_shadow",
            would_retain_continuous_parent = TRUE,
            changes_phase1a_selection = FALSE,
            stringsAsFactors = FALSE
          )
        }
      }

      if (nrow(fragments) > 0L) {
        for (j in seq_len(nrow(fragments))) {
          fs <- as.integer(fragments$start_isi[j])
          fe <- as.integer(fragments$end_isi[j])
          fragment_id <- stpd_multitrack_compatibility_fragment_id(parent_key, fs, fe)
          regate <- stpd_multitrack_compatibility_fragment_regate(
            label, fs, fe, dat = dat, params = params,
            variable_params = variable_params, min_isi_sec = min_isi_sec,
            parent = parent, policy = policy
          )
          # Pause changes the geometry of direct support, not the evidence by
          # which an already accepted Broad-HF episode was selected.  Requiring
          # every Pause-created child to independently meet the parent minimum
          # spike count creates a deterministic false-split failure (one valid
          # parent can be deleted solely because it contains a Pause).  Every
          # accepted State parent therefore retains its episode evidence while
          # its Pause-free direct-support children inherit acceptance.  Burst
          # overlays never participate in this split.
          if (label %in% c(
              "high_frequency_spiking", "high_frequency_tonic", "tonic"
            ) && identical(split_kind, "pause_boundary")) {
            regate$gate_evaluated <- TRUE
            regate$gate_fidelity <-
              "parent_episode_acceptance_inherited_after_canonical_pause_split"
            regate$provisional_gate_pass <- TRUE
            regate$provisional_gate_status <-
              "passed__parent_episode_acceptance_inherited"
            regate$failed_checks <- ""
          }
          cut_ids <- if (nrow(split_cuts) > 0L) {
            stpd_multitrack_compatibility_join_ids(split_cuts$candidate_id)
          } else ""
          fragment_rows[[length(fragment_rows) + 1L]] <- data.frame(
            fragment_candidate_id = fragment_id,
            root_candidate_id = parent_id,
            root_candidate_key = parent_key,
            parent_candidate_id = parent_id,
            parent_candidate_key = parent_key,
            state_label = label,
            start_isi = fs,
            end_isi = fe,
            n_isi = as.integer(fe - fs + 1L),
            n_spikes = as.integer(fe - fs + 2L),
            fragment_index = as.integer(j),
            split_kind = split_kind,
            split_boundary_candidate_ids = cut_ids,
            gate_evaluated = isTRUE(regate$gate_evaluated),
            gate_fidelity = as.character(regate$gate_fidelity),
            provisional_gate_pass = as.logical(regate$provisional_gate_pass),
            provisional_gate_status = as.character(regate$provisional_gate_status),
            failed_checks = as.character(regate$failed_checks),
            n_valid_isi = as.integer(regate$n_valid_isi),
            min_spikes_required = as.integer(regate$min_spikes_required),
            CV = as.numeric(regate$CV),
            LV = as.numeric(regate$LV),
            MM = as.numeric(regate$MM),
            hf_spiking_large_fraction = as.numeric(regate$large_fraction),
            root_phase1a_selection_preserved = TRUE,
            provisional_fragment_only = TRUE,
            stringsAsFactors = FALSE
          )
          if (split_kind != "none") {
            relationship_rows[[length(relationship_rows) + 1L]] <- data.frame(
              relationship_type = "state_split_child",
              compatibility_rule = if (split_kind == "pause_boundary") {
                "state_pause_direct_support_split"
              } else {
                "state_support_split"
              },
              source_candidate_id = parent_id,
              source_candidate_key = parent_key,
              target_candidate_id = fragment_id,
              target_candidate_key = paste0(parent_key, "|fragment|", fs, "|", fe),
              overlap_start_isi = fs,
              overlap_end_isi = fe,
              policy_role = "active_shadow",
              track_policy_version = policy$policy_version,
              non_destructive = TRUE,
              stringsAsFactors = FALSE
            )
          }
        }
      }
    }
  }

  state_parents <- dplyr::bind_rows(empty_tables$state_parents, parent_rows)
  state_fragments <- dplyr::bind_rows(empty_tables$state_fragments, fragment_rows)

  # HFS Burst coexistence is always represented as an overlay. The following
  # dominance flag is separate evidence and never removes the HFS candidate.
  dominance_rows <- list()
  hfs_fragments <- if (nrow(state_fragments) > 0L) {
    state_fragments[state_fragments$state_label == "high_frequency_spiking", , drop = FALSE]
  } else state_fragments
  if (nrow(hfs_fragments) > 0L) {
    for (i in seq_len(nrow(hfs_fragments))) {
      fragment <- hfs_fragments[i, , drop = FALSE]
      fs <- as.integer(fragment$start_isi)
      fe <- as.integer(fragment$end_isi)
      overlapping_events <- events[
        vapply(seq_len(nrow(events)), function(j) {
          stpd_multitrack_compatibility_interval_overlap(
            fs, fe, events$start_isi[j], events$end_isi[j]
          ) > 0L
        }, logical(1)),
        , drop = FALSE
      ]
      stats <- stpd_multitrack_compatibility_interval_union(overlapping_events, fs, fe)
      span_n <- fe - fs + 1L
      min_groups <- max(
        policy$hfs_dominance_group_floor,
        as.integer(ceiling(policy$hfs_dominance_group_fraction * span_n))
      )
      distributed <- stats$group_count >= min_groups &&
        stats$coverage >= policy$hfs_dominance_distributed_coverage_min
      majority <- stats$coverage >= policy$hfs_dominance_majority_coverage_min
      dominance <- isTRUE(distributed) || isTRUE(majority)
      cv <- stpd_multitrack_compatibility_num(fragment$CV)
      lv <- stpd_multitrack_compatibility_num(fragment$LV)
      mm <- stpd_multitrack_compatibility_num(fragment$MM)
      large <- stpd_multitrack_compatibility_num(fragment$hf_spiking_large_fraction)
      variable_cv <- is.finite(cv) && cv >= policy$variable_hfs_cv_min
      variable_lv <- is.finite(lv) && lv >= policy$variable_hfs_lv_min
      variable_large <- is.finite(large) &&
        large >= policy$variable_hfs_large_fraction_min
      variable_mm_audit <- is.finite(mm) &&
        mm >= policy$variable_hfs_mm_audit_min
      variable_without_mm <- variable_cv || variable_lv || variable_large
      legacy_variable_with_mm <- variable_without_mm || variable_mm_audit
      packet_like <- variable_without_mm &&
        ((stats$group_count >= policy$packet_like_multi_group_min &&
            stats$coverage >= policy$packet_like_multi_group_coverage_min) ||
          (stats$group_count >= policy$packet_like_single_group_min &&
            stats$coverage >= policy$packet_like_single_group_coverage_min))
      fragment_gate_evaluated <- isTRUE(fragment$gate_evaluated)
      fragment_gate_pass <- as.logical(fragment$provisional_gate_pass)[1]
      fragment_split_kind <- stpd_multitrack_compatibility_text(
        fragment$split_kind, "none"
      )
      # Burst coverage is descriptive episode context only. It may be computed
      # for both the accepted parent and Pause-bounded direct-support children,
      # but it never governs State retention or deletion.
      dominance_applicable <- if (identical(fragment_split_kind, "none")) {
        TRUE
      } else {
        fragment_gate_evaluated && isTRUE(fragment_gate_pass)
      }
      dominance_rows[[length(dominance_rows) + 1L]] <- data.frame(
        hfs_fragment_candidate_id = as.character(fragment$fragment_candidate_id),
        root_hfs_candidate_id = as.character(fragment$root_candidate_id),
        root_hfs_candidate_key = as.character(fragment$root_candidate_key),
        start_isi = fs,
        end_isi = fe,
        n_isi = as.integer(span_n),
        selected_event_candidate_count = as.integer(stats$event_count),
        selected_event_group_count = as.integer(stats$group_count),
        selected_event_covered_isi_n = as.integer(stats$covered_n),
        selected_event_coverage = as.numeric(stats$coverage),
        contributor_candidate_ids = as.character(stats$contributor_candidate_ids),
        contributor_candidate_keys = as.character(stats$contributor_candidate_keys),
        merged_event_group_spans = as.character(stats$group_spans),
        group_floor_threshold = as.integer(policy$hfs_dominance_group_floor),
        group_fraction_threshold = as.numeric(policy$hfs_dominance_group_fraction),
        effective_min_group_count = as.integer(min_groups),
        distributed_coverage_threshold = as.numeric(policy$hfs_dominance_distributed_coverage_min),
        majority_coverage_threshold = as.numeric(policy$hfs_dominance_majority_coverage_min),
        distributed_dominance_flag = isTRUE(distributed),
        majority_dominance_flag = isTRUE(majority),
        threshold_dominance_flag = isTRUE(dominance),
        provisional_dominance_flag = isTRUE(dominance) && dominance_applicable,
        fragment_gate_evaluated = fragment_gate_evaluated,
        fragment_provisional_gate_pass = fragment_gate_pass,
        dominance_evidence_applicable = dominance_applicable,
        provisional_state_status = if (!dominance_applicable && !fragment_gate_evaluated) {
          "fragment_regate_not_evaluated__dominance_evidence_not_applicable"
        } else if (!dominance_applicable) {
          "fragment_failed_provisional_regate__dominance_evidence_not_applicable"
        } else if (dominance) {
          "retain_hfs__burst_rich_diagnostic_only"
        } else {
          "retain_hfs_with_event_overlay"
        },
        state_selected_within_track_preserved = TRUE,
        destructive_action_applied = FALSE,
        CV = cv,
        LV = lv,
        MM = mm,
        hf_spiking_large_fraction = large,
        variable_by_cv = variable_cv,
        variable_by_lv = variable_lv,
        variable_by_large_fraction = variable_large,
        variable_by_mm_audit_only = variable_mm_audit,
        variable_hfs_gate_without_mm = variable_without_mm,
        legacy_variable_hfs_gate_with_mm = legacy_variable_with_mm,
        mm_used_in_boolean_gate = FALSE,
        packet_like_annotation = isTRUE(packet_like),
        packet_neighbor_annotation = FALSE,
        packet_neighbor_source_fragment_ids = "",
        packet_annotations_destructive = FALSE,
        stringsAsFactors = FALSE
      )
      if (nrow(overlapping_events) > 0L) {
        for (j in seq_len(nrow(overlapping_events))) {
          os <- max(fs, as.integer(overlapping_events$start_isi[j]))
          oe <- min(fe, as.integer(overlapping_events$end_isi[j]))
          add_relationship(
            "event_overlay_within_state", "hfs_burst_overlay",
            overlapping_events[j, , drop = FALSE],
            fragment$fragment_candidate_id,
            paste0(fragment$root_candidate_key, "|fragment|", fs, "|", fe),
            os, oe
          )
          overlay_rows[[length(overlay_rows) + 1L]] <- data.frame(
            state_candidate_id = as.character(fragment$root_candidate_id),
            state_candidate_key = as.character(fragment$root_candidate_key),
            state_label = "high_frequency_spiking",
            event_candidate_id = stpd_multitrack_compatibility_text(overlapping_events$candidate_id[j]),
            event_candidate_key = stpd_multitrack_compatibility_text(overlapping_events$.candidate_key[j]),
            compatibility_rule = "hfs_burst_overlay",
            policy_role = "active_shadow",
            would_retain_continuous_parent = TRUE,
            changes_phase1a_selection = FALSE,
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  hfs_dominance <- dplyr::bind_rows(empty_tables$hfs_dominance, dominance_rows)

  # Packet-neighbor remains a geometry-only annotation. It is never consulted
  # by dominance or any provisional state selection field.
  if (nrow(hfs_dominance) > 1L && any(hfs_dominance$packet_like_annotation)) {
    packet_sources <- which(hfs_dominance$packet_like_annotation)
    for (i in seq_len(nrow(hfs_dominance))) {
      if (!isTRUE(hfs_dominance$variable_hfs_gate_without_mm[i])) next
      neighbor_sources <- integer()
      for (j in packet_sources) {
        if (i == j) next
        a_start <- hfs_dominance$start_isi[i]
        a_end <- hfs_dominance$end_isi[i]
        b_start <- hfs_dominance$start_isi[j]
        b_end <- hfs_dominance$end_isi[j]
        gap_n <- if (b_end < a_start) {
          a_start - b_end - 1L
        } else if (a_end < b_start) {
          b_start - a_end - 1L
        } else {
          0L
        }
        if (gap_n >= 0L && gap_n <= policy$packet_neighbor_max_gap_isi) {
          neighbor_sources <- c(neighbor_sources, j)
        }
      }
      if (length(neighbor_sources) > 0L) {
        hfs_dominance$packet_neighbor_annotation[i] <- TRUE
        hfs_dominance$packet_neighbor_source_fragment_ids[i] <-
          stpd_multitrack_compatibility_join_ids(
            hfs_dominance$hfs_fragment_candidate_id[neighbor_sources]
          )
      }
    }
  }

  # Review selections are epistemic overlays and never split a biological state.
  if (nrow(reviews) > 0L && nrow(state_parents) > 0L) {
    for (i in seq_len(nrow(reviews))) {
      for (j in seq_len(nrow(state_parents))) {
        overlap_n <- stpd_multitrack_compatibility_interval_overlap(
          reviews$start_isi[i], reviews$end_isi[i],
          state_parents$start_isi[j], state_parents$end_isi[j]
        )
        if (overlap_n <= 0L) next
        os <- max(as.integer(reviews$start_isi[i]), state_parents$start_isi[j])
        oe <- min(as.integer(reviews$end_isi[i]), state_parents$end_isi[j])
        add_relationship(
          "review_overlay", "review_never_splits_state", reviews[i, , drop = FALSE],
          state_parents$state_candidate_id[j], state_parents$state_candidate_key[j],
          os, oe
        )
      }
    }
  }

  # A selected Pause and canonical Burst cannot share an ISI physically. Record
  # any violation without attempting cross-track deletion.
  if (nrow(gaps) > 0L && nrow(events) > 0L) {
    for (i in seq_len(nrow(gaps))) {
      for (j in seq_len(nrow(events))) {
        if (stpd_multitrack_compatibility_interval_overlap(
          gaps$start_isi[i], gaps$end_isi[i], events$start_isi[j], events$end_isi[j]
        ) <= 0L) next
        invariant_rows[[length(invariant_rows) + 1L]] <- data.frame(
          invariant = "selected_pause_must_not_overlap_selected_canonical_burst",
          severity = "violation",
          state_candidate_id = stpd_multitrack_compatibility_text(gaps$candidate_id[i]),
          other_candidate_id = stpd_multitrack_compatibility_text(events$candidate_id[j]),
          message = "Selected Pause and canonical Burst overlap; recorded without destructive arbitration.",
          stringsAsFactors = FALSE
        )
      }
    }
  }

  relationships <- dplyr::bind_rows(empty_tables$relationships, relationship_rows)
  overlays <- dplyr::bind_rows(empty_tables$overlays, overlay_rows)
  invariants <- dplyr::bind_rows(empty_tables$invariants, invariant_rows)
  for (name in c("state_parents", "state_fragments", "relationships", "overlays", "hfs_dominance", "invariants")) {
    obj <- get(name)
    if (nrow(obj) > 0L) {
      sort_columns <- intersect(
        c("state_candidate_key", "root_candidate_key", "start_isi", "end_isi",
          "relationship_type", "source_candidate_key", "target_candidate_key",
          "event_candidate_key", "invariant", "other_candidate_id"),
        names(obj)
      )
      if (length(sort_columns) > 0L) {
        ord_args <- lapply(sort_columns, function(column) obj[[column]])
        ord <- do.call(order, c(ord_args, list(na.last = TRUE, method = "radix")))
        obj <- obj[ord, , drop = FALSE]
        rownames(obj) <- NULL
      }
    }
    assign(name, obj)
  }

  structure(
    list(
      policy_version = policy$policy_version,
      multitrack_policy_parameter_path = stpd_multitrack_policy_parameter_path(),
      multitrack_policy_hash = stpd_multitrack_policy_hash(params),
      multitrack_policy_hash_algorithm = "SHA-256",
      source_params_hash = stpd_multitrack_source_params_hash(params),
      authoritative = FALSE,
      source_policy_version = as.character(phase1a_shadow$policy_version %||% ""),
      candidate_source = as.character(phase1a_shadow$candidate_source %||% ""),
      legacy_projection_changed = FALSE,
      legacy_candidate_audit_changed = FALSE,
      public_exports_changed = FALSE,
      provisional_policy = policy,
      state_parents = state_parents,
      state_fragments = state_fragments,
      relationships = relationships,
      overlays = overlays,
      hfs_dominance = hfs_dominance,
      invariants = invariants
    ),
    class = c("stpd_multitrack_compatibility_shadow", "list")
  )
}
