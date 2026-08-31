# Gate 1B Tonic-State observation ----------------------------------------
#
# Observer-only closure of canonical Tonic and legacy HFT State evidence.
# It records frequency and regularity on separate axes, proves that Burst is
# a non-destructive Event overlay, audits sparse-evidence abstention, and
# independently reruns the canonical generator on every Pause-created child.
# The observer never changes candidates, thresholds, selection, or products.

stpd_candidate_lineage_tonic_state_hooks <- function() c(
  tonic_state_entry_v1 = "entry",
  tonic_state_candidates_v1 = "candidates",
  tonic_state_frequency_regularity_v1 = "frequency_regularity",
  tonic_state_fragment_redetection_v1 = "fragment_redetection",
  tonic_state_receipt_v1 = "receipt"
)

stpd_candidate_lineage_tonic_state_hash <- function(x, stage) {
  stpd_threshold_first_hash_domain(paste0("stpd-tonic-state-", stage, "-v1"), x)
}

stpd_candidate_lineage_tonic_state_num <- function(row, name, fallback = NA_real_) {
  if (!(name %in% names(row))) return(as.numeric(fallback))
  value <- suppressWarnings(as.numeric(row[[name]][[1L]]))
  if (length(value) && is.finite(value[[1L]])) value[[1L]] else as.numeric(fallback)
}

stpd_candidate_lineage_tonic_state_candidate_id <- function(row, ordinal) {
  value <- if ("candidate_id" %in% names(row)) as.character(row$candidate_id[[1L]]) else ""
  if (!length(value) || is.na(value) || !nzchar(value)) paste0("tonic_state_", ordinal) else value
}

stpd_candidate_lineage_tonic_state_fragments <- function(start, end, boundaries) {
  if (!is.finite(start) || !is.finite(end) || end < start) return(data.frame(
    start_isi = integer(), end_isi = integer(), stringsAsFactors = FALSE))
  cuts <- boundaries[FALSE, , drop = FALSE]
  if (nrow(boundaries)) {
    keep <- as.integer(boundaries$start_isi) <= end &
      as.integer(boundaries$end_isi) >= start
    keep[is.na(keep)] <- FALSE
    cuts <- boundaries[keep, , drop = FALSE]
  }
  stpd_multitrack_compatibility_subtract_intervals(start, end, cuts)
}

stpd_candidate_lineage_tonic_state_redetect <- function(
    dat, state_class, start, end, params, vp, min_isi_sec, train) {
  # Slice to one boundary-isolated child plus its preceding timestamp. This is
  # the same complete generator call as a whole-train mask, but avoids an
  # O(number_of_candidates x whole_train_length) observer cost.
  first_row <- max(1L, as.integer(start) - 1L)
  last_row <- min(nrow(dat), as.integer(end))
  isolated <- dat[seq.int(first_row, last_row), , drop = FALSE]
  isolated$ISI_sec[[1L]] <- NA_real_
  regenerated <- if (identical(state_class, "tonic")) {
    stpd_event_core_detect_tonic(isolated, params, vp, min_isi_sec, train)
  } else {
    stpd_event_core_detect_hf_tonic(isolated, params, vp, min_isi_sec, train)
  }
  if (is.null(regenerated) || !is.data.frame(regenerated)) regenerated <- data.frame()
  if (nrow(regenerated)) {
    offset <- first_row - 1L
    regenerated$start_isi <- as.integer(regenerated$start_isi) + offset
    regenerated$end_isi <- as.integer(regenerated$end_isi) + offset
    regenerated <- regenerated[
      as.integer(regenerated$start_isi) >= start &
        as.integer(regenerated$end_isi) <= end,
      , drop = FALSE
    ]
  }
  regenerated
}

stpd_candidate_lineage_tonic_state_replay <- function(context) {
  tonic_requested <- "tonic" %in% context$patterns
  hft_requested <- "high_frequency_tonic" %in% context$patterns
  scientific <- dplyr::bind_rows(context$scientific_tonic, context$scientific_hft)
  state_class <- c(
    rep("tonic", nrow(context$scientific_tonic)),
    rep("high_frequency_tonic", nrow(context$scientific_hft))
  )
  generators <- c(
    rep("stpd_event_core_detect_tonic", nrow(context$scientific_tonic)),
    rep("stpd_event_core_detect_hf_tonic", nrow(context$scientific_hft))
  )
  burst <- context$scientific_burst
  candidate_rows <- list()
  evidence_rows <- list()
  fragment_rows <- list()
  fragment_ordinal <- 0L

  for (i in seq_len(nrow(scientific))) {
    row <- scientific[i, , drop = FALSE]
    start <- suppressWarnings(as.integer(row$start_isi[[1L]]))
    end <- suppressWarnings(as.integer(row$end_isi[[1L]]))
    candidate_id <- stpd_candidate_lineage_tonic_state_candidate_id(row, i)
    indices <- if (is.finite(start) && is.finite(end) && end >= start) seq.int(start, end) else integer()
    indices <- indices[indices >= 1L & indices <= nrow(context$dat)]
    values <- suppressWarnings(as.numeric(context$dat$ISI_sec[indices]))
    artifact <- is_artifact_isi(values, context$min_isi_sec)
    values <- values[is.finite(values) & !artifact]
    burst_overlap <- 0L
    if (nrow(burst) && is.finite(start) && is.finite(end)) {
      hit <- as.integer(burst$start_isi) <= end & as.integer(burst$end_isi) >= start
      hit[is.na(hit)] <- FALSE
      burst_overlap <- sum(hit)
    }
    candidate_rows[[i]] <- data.frame(
      train = context$train, candidate_ordinal = as.integer(i),
      candidate_id = candidate_id, state_class = state_class[[i]],
      generator = generators[[i]], start_isi = as.integer(start),
      end_isi = as.integer(end), n_spikes = as.integer(end - start + 2L),
      burst_overlap_n = as.integer(burst_overlap), burst_veto_applied = FALSE,
      candidate_payload_sha256 = stpd_candidate_lineage_tonic_state_hash(row, "candidate"),
      stringsAsFactors = FALSE, check.names = FALSE
    )
    median_isi <- if (length(values)) stats::median(values) else NA_real_
    lower <- if (identical(state_class[[i]], "tonic")) {
      stpd_candidate_lineage_tonic_state_num(row, "tonic_adaptive_lower_sec", context$vp$tonic_min)
    } else context$vp$hf_tonic_floor
    upper <- if (identical(state_class[[i]], "tonic")) {
      stpd_candidate_lineage_tonic_state_num(row, "tonic_adaptive_upper_sec", context$vp$tonic_max)
    } else context$vp$hf_tonic_high_max
    evidence_rows[[i]] <- data.frame(
      train = context$train, candidate_ordinal = as.integer(i),
      candidate_id = candidate_id, state_class = state_class[[i]],
      n_valid_isi = as.integer(length(values)), median_isi_sec = as.numeric(median_isi),
      median_frequency_hz = if (is.finite(median_isi) && median_isi > 0) 1 / median_isi else NA_real_,
      CV = as.numeric(stpd_event_core_cv(values)), LV = as.numeric(stpd_event_core_lv(values)),
      MM = as.numeric(stpd_event_core_mm(values)),
      lower_frequency_bound_sec = as.numeric(lower), upper_frequency_bound_sec = as.numeric(upper),
      regularity_evidence_status = if (length(values) >= 2L) "computed_independent_axis" else "insufficient",
      frequency_evidence_status = if (length(values)) "computed_independent_axis" else "insufficient",
      stringsAsFactors = FALSE, check.names = FALSE
    )

    fragments <- stpd_candidate_lineage_tonic_state_fragments(
      start, end, context$hard_boundaries)
    split <- nrow(fragments) > 1L || (nrow(fragments) == 1L &&
      (fragments$start_isi[[1L]] != start || fragments$end_isi[[1L]] != end))
    for (j in seq_len(nrow(fragments))) {
      fs <- as.integer(fragments$start_isi[[j]])
      fe <- as.integer(fragments$end_isi[[j]])
      regenerated <- stpd_candidate_lineage_tonic_state_redetect(
        context$dat, state_class[[i]], fs, fe, context$params, context$vp,
        context$min_isi_sec, context$train)
      fragment_ordinal <- fragment_ordinal + 1L
      required <- if (identical(state_class[[i]], "tonic")) {
        context$vp$tonic_min_spikes
      } else context$vp$hf_tonic_min_spikes
      n_spikes <- fe - fs + 2L
      status <- if (nrow(regenerated)) {
        "accepted_by_full_generator_redetection"
      } else if (n_spikes < required) {
        "abstained_sparse_evidence"
      } else {
        "abstained_failed_frequency_or_regularity"
      }
      fragment_rows[[fragment_ordinal]] <- data.frame(
        train = context$train, fragment_ordinal = as.integer(fragment_ordinal),
        parent_candidate_id = candidate_id, state_class = state_class[[i]],
        start_isi = fs, end_isi = fe, n_spikes = as.integer(n_spikes),
        created_by_canonical_pause = isTRUE(split), redetection_performed = TRUE,
        regenerated_candidate_n = as.integer(nrow(regenerated)),
        fragment_accepted = nrow(regenerated) > 0L, terminal_status = status,
        regenerated_geometry_sha256 = stpd_candidate_lineage_tonic_state_hash(
          if (nrow(regenerated)) regenerated[, intersect(c("candidate_id", "start_isi", "end_isi", "final_label"), names(regenerated)), drop = FALSE] else data.frame(),
          "regenerated-geometry"),
        stringsAsFactors = FALSE, check.names = FALSE
      )
    }
  }

  candidates <- if (length(candidate_rows)) dplyr::bind_rows(candidate_rows) else
    stpd_candidate_lineage_empty_hook_payload("tonic_state_candidates_v1")
  frequency_regularity <- if (length(evidence_rows)) dplyr::bind_rows(evidence_rows) else
    stpd_candidate_lineage_empty_hook_payload("tonic_state_frequency_regularity_v1")
  fragment_redetection <- if (length(fragment_rows)) dplyr::bind_rows(fragment_rows) else
    stpd_candidate_lineage_empty_hook_payload("tonic_state_fragment_redetection_v1")

  isi <- suppressWarnings(as.numeric(context$dat$ISI_sec))
  valid <- is.finite(isi) & !is_artifact_isi(isi, context$min_isi_sec)
  if (length(valid)) valid[[1L]] <- FALSE
  tonic_sparse <- tonic_requested && sum(valid) + 1L < context$vp$tonic_min_spikes
  hft_sparse <- hft_requested && sum(valid) + 1L < context$vp$hf_tonic_min_spikes
  sparse_applicable <- tonic_sparse || hft_sparse
  sparse_violation <- (tonic_sparse && nrow(context$scientific_tonic) > 0L) ||
    (hft_sparse && nrow(context$scientific_hft) > 0L)
  sparse_status <- if (!sparse_applicable) "not_applicable" else if (!sparse_violation) "validated_abstained" else "mismatch"
  split_rows <- fragment_redetection$created_by_canonical_pause %||% logical()
  fragment_status <- if (!length(split_rows) || !any(split_rows)) {
    "not_applicable_no_pause_split"
  } else if (all(fragment_redetection$redetection_performed[split_rows])) {
    "validated_full_generator_redetection"
  } else "mismatch"
  burst_status <- if (!nrow(candidates) || all(!candidates$burst_veto_applied)) {
    "validated_non_destructive_event_overlay"
  } else "mismatch"
  entry <- data.frame(
    train = context$train, root_id = "tonic_state_root",
    tonic_requested = tonic_requested, hft_requested = hft_requested,
    generator_invoked = tonic_requested || hft_requested,
    n_interval_rows = as.integer(nrow(context$dat)), n_valid_isi = as.integer(sum(valid)),
    min_isi_sec = context$min_isi_sec,
    canonical_pause_boundary_n = as.integer(nrow(context$hard_boundaries)),
    tonic_min_spikes = as.integer(context$vp$tonic_min_spikes),
    hft_min_spikes = as.integer(context$vp$hf_tonic_min_spikes),
    frequency_axis = "candidate_local_isi_distribution",
    regularity_axes = "CV|LV|MM", burst_to_state_veto_allowed = FALSE,
    input_data_sha256 = context$input_data_sha256,
    params_payload_sha256 = context$params_payload_sha256,
    vp_payload_sha256 = context$vp_payload_sha256,
    hard_boundary_payload_sha256 = context$hard_boundary_payload_sha256,
    stringsAsFactors = FALSE, check.names = FALSE
  )
  receipt <- data.frame(
    train = context$train, root_id = "tonic_state_root",
    tonic_candidate_n = as.integer(nrow(context$scientific_tonic)),
    hft_candidate_n = as.integer(nrow(context$scientific_hft)),
    burst_overlapping_candidate_n = as.integer(sum(candidates$burst_overlap_n > 0L)),
    sparse_case_applicable = sparse_applicable,
    sparse_evidence_abstention_status = sparse_status,
    burst_overlay_status = burst_status, fragment_redetection_status = fragment_status,
    scientific_result_influence = "none_observer_only", publication_authority = FALSE,
    entry_payload_sha256 = stpd_candidate_lineage_tonic_state_hash(entry, "entry"),
    candidate_payload_sha256 = stpd_candidate_lineage_tonic_state_hash(candidates, "candidates"),
    frequency_regularity_payload_sha256 = stpd_candidate_lineage_tonic_state_hash(frequency_regularity, "frequency-regularity"),
    fragment_redetection_payload_sha256 = stpd_candidate_lineage_tonic_state_hash(fragment_redetection, "fragment-redetection"),
    scientific_tonic_output_sha256 = stpd_candidate_lineage_tonic_state_hash(context$scientific_tonic, "scientific-tonic"),
    scientific_hft_output_sha256 = stpd_candidate_lineage_tonic_state_hash(context$scientific_hft, "scientific-hft"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  if (identical(sparse_status, "mismatch") || identical(burst_status, "mismatch") ||
      identical(fragment_status, "mismatch")) return(NULL)
  list(entry = entry, candidates = candidates,
       frequency_regularity = frequency_regularity,
       fragment_redetection = fragment_redetection, receipt = receipt)
}

stpd_candidate_lineage_begin_tonic_state <- function(
    shard, train, dat, params, vp, min_isi_sec, patterns, hard_boundaries) {
  if (is.null(shard)) return(invisible(NULL))
  stpd_candidate_lineage_validate_train_collector(
    shard, "open", train, validate_observations = FALSE
  )
  context <- unserialize(serialize(list(
    train = as.character(train)[1L], dat = dat, params = params, vp = vp,
    min_isi_sec = as.numeric(min_isi_sec)[1L], patterns = as.character(patterns),
    hard_boundaries = as.data.frame(hard_boundaries, stringsAsFactors = FALSE),
    input_data_sha256 = stpd_candidate_lineage_tonic_state_hash(dat, "input"),
    params_payload_sha256 = stpd_candidate_lineage_tonic_state_hash(params, "params"),
    vp_payload_sha256 = stpd_candidate_lineage_tonic_state_hash(vp, "vp"),
    hard_boundary_payload_sha256 = stpd_candidate_lineage_tonic_state_hash(hard_boundaries, "boundaries")
  ), NULL, version = 3L))
  name <- "tonic_state_begin_context"
  if (exists(name, shard, inherits = FALSE)) {
    if (!bindingIsLocked(name, shard) || !identical(get(name, shard), context))
      stpd_candidate_lineage_abort("collector_tonic_state_begin_conflict",
        "Tonic-State begin context conflicts with its first capture.")
  } else {
    assign(name, context, shard); lockBinding(name, shard)
  }
  invisible(context)
}

stpd_candidate_lineage_capture_tonic_state <- function(
    shard, train, scientific_tonic, scientific_hft, scientific_burst) {
  if (is.null(shard)) return(invisible(NULL))
  name <- "tonic_state_begin_context"
  if (!exists(name, shard, inherits = FALSE) || !bindingIsLocked(name, shard))
    stpd_candidate_lineage_abort("collector_tonic_state_begin_missing",
      "Tonic-State capture requires its locked live begin context.")
  context <- get(name, shard)
  if (!identical(context$train, as.character(train)[1L]))
    stpd_candidate_lineage_abort("collector_tonic_state_train_conflict",
      "Tonic-State capture train differs from its begin context.")
  context$scientific_tonic <- unserialize(serialize(scientific_tonic, NULL, version = 3L))
  context$scientific_hft <- unserialize(serialize(scientific_hft, NULL, version = 3L))
  context$scientific_burst <- unserialize(serialize(scientific_burst, NULL, version = 3L))
  replay <- stpd_candidate_lineage_tonic_state_replay(context)
  if (is.null(replay)) stpd_candidate_lineage_abort(
    "collector_tonic_state_replay_failed",
    "Tonic-State frequency, regularity, sparse, Burst-overlay, or fragment-redetection closure failed.")
  assign("tonic_state_context", context, shard); lockBinding("tonic_state_context", shard)
  assign("tonic_state_replay_cache", replay, shard); lockBinding("tonic_state_replay_cache", shard)
  hooks <- stpd_candidate_lineage_tonic_state_hooks()
  for (hook in names(hooks))
    stpd_candidate_lineage_collector_capture(shard, hook, replay[[hooks[[hook]]]])
  invisible(replay)
}

stpd_candidate_lineage_tonic_state_payload_is_valid <- function(shard, hook, payload) {
  if (!exists("tonic_state_context", shard, inherits = FALSE) ||
      !bindingIsLocked("tonic_state_context", shard) ||
      !exists("tonic_state_replay_cache", shard, inherits = FALSE) ||
      !bindingIsLocked("tonic_state_replay_cache", shard)) return(FALSE)
  fresh <- stpd_candidate_lineage_tonic_state_replay(get("tonic_state_context", shard))
  cached <- get("tonic_state_replay_cache", shard)
  if (is.null(fresh) || !identical(fresh, cached)) return(FALSE)
  map <- stpd_candidate_lineage_tonic_state_hooks()
  key <- unname(map[[hook]])
  if (is.null(key) || !identical(payload, cached[[key]])) return(FALSE)
  position <- match(hook, names(map))
  prior <- if (position <= 1L) character() else names(map)[seq_len(position - 1L)]
  all(vapply(prior, function(previous) identical(
    shard$observations[[previous]], cached[[map[[previous]]]]), logical(1)))
}
