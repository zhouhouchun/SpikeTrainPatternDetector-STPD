# Gate 1B Broad-HFS State observation -------------------------------------
#
# Observer-only closure of the unique Broad-HFS parent generator. The
# observer records the accepted episode envelope separately from direct
# support and tolerated connectors. It never changes detector candidates,
# thresholds, selection, subtype classification, or scientific output.

stpd_candidate_lineage_broad_hfs_hooks <- function() c(
  hfs_state_entry_v1 = "entry",
  hfs_state_candidates_v1 = "candidates",
  hfs_state_support_roles_v1 = "support_roles",
  hfs_state_parent_selection_v1 = "parent_selection",
  hfs_state_receipt_v1 = "receipt"
)

stpd_candidate_lineage_broad_hfs_hash <- function(x, stage) {
  stpd_threshold_first_hash_domain(paste0("stpd-broad-hfs-", stage, "-v1"), x)
}

stpd_candidate_lineage_broad_hfs_boundary_mask <- function(n, boundaries) {
  out <- rep(FALSE, n)
  if (is.null(boundaries) || !is.data.frame(boundaries) || !nrow(boundaries))
    return(out)
  for (i in seq_len(nrow(boundaries))) {
    start <- suppressWarnings(as.integer(boundaries$start_isi[[i]]))
    end <- suppressWarnings(as.integer(boundaries$end_isi[[i]]))
    if (!is.finite(start) || !is.finite(end) || end < start) next
    index <- seq.int(max(1L, start), min(n, end))
    if (length(index)) out[index] <- TRUE
  }
  out
}

stpd_candidate_lineage_broad_hfs_replay <- function(context) {
  requested <- any(c(
    "high_frequency_spiking", "high_frequency_tonic",
    "high_frequency_irregular_state"
  ) %in% context$patterns)
  hfs <- context$scientific_hfs
  selected_ids <- if (nrow(context$scientific_selected_hfs)) {
    as.character(context$scientific_selected_hfs$candidate_id)
  } else character()
  selected_ids <- sort(unique(selected_ids), method = "radix")
  # Generation boundaries prove that Broad-HFS parent discovery was not
  # pre-cut by Pause.  Materialization boundaries are a separate, later input:
  # they remove Pause from direct State support while preserving membership in
  # the already accepted parent envelope.
  generation_boundary <- stpd_candidate_lineage_broad_hfs_boundary_mask(
    context$n_interval_rows, context$hard_boundaries)
  materialization_boundary <- stpd_candidate_lineage_broad_hfs_boundary_mask(
    context$n_interval_rows, context$materialization_state_boundaries)

  candidate_rows <- list()
  role_rows <- list()
  role_ordinal <- 0L
  for (i in seq_len(nrow(hfs))) {
    row <- hfs[i, , drop = FALSE]
    candidate_id <- as.character(row$candidate_id[[1L]] %||% "")
    start <- suppressWarnings(as.integer(row$start_isi[[1L]]))
    end <- suppressWarnings(as.integer(row$end_isi[[1L]]))
    direct_max <- suppressWarnings(as.numeric(
      row$hf_spiking_pattern_max_ISI_sec[[1L]] %||% NA_real_))
    bridge_max <- suppressWarnings(as.numeric(
      row$hf_spiking_epoch_bridge_sec[[1L]] %||% NA_real_))
    tolerated_max <- suppressWarnings(as.numeric(
      row$hf_spiking_tolerated_gap_sec[[1L]] %||% NA_real_))
    indices <- if (is.finite(start) && is.finite(end) && end >= start) {
      seq.int(start, end)
    } else integer()
    indices <- indices[indices >= 1L & indices <= context$n_interval_rows]
    direct_n <- 0L
    connector_n <- 0L
    moderate_n <- 0L
    artifact_n <- 0L
    boundary_n <- 0L
    for (index in indices) {
      value <- context$isi[[index]]
      artifact <- isTRUE(is_artifact_isi(value, context$min_isi_sec))
      excluded_pause <- materialization_boundary[[index]]
      direct <- is.finite(value) && !artifact &&
        !generation_boundary[[index]] && !excluded_pause &&
        (!is.finite(bridge_max) || value <= bridge_max) &&
        (!is.finite(direct_max) || value <= direct_max)
      connector_kind <- if (excluded_pause) {
        "excluded_pause_direct_support"
      } else if (direct) {
        "none"
      } else if (artifact && is.finite(value)) {
        "transparent_artifact"
      } else {
        "supra_direct_support"
      }
      role <- if (excluded_pause) {
        "excluded_pause_direct_support"
      } else if (direct) {
        "direct_support"
      } else {
        "tolerated_connector"
      }
      if (excluded_pause) {
        boundary_n <- boundary_n + 1L
      } else if (direct) {
        direct_n <- direct_n + 1L
      } else {
        connector_n <- connector_n + 1L
      }
      if (identical(connector_kind, "supra_direct_support"))
        moderate_n <- moderate_n + 1L
      if (identical(connector_kind, "transparent_artifact"))
        artifact_n <- artifact_n + 1L
      role_ordinal <- role_ordinal + 1L
      role_rows[[role_ordinal]] <- data.frame(
        train = context$train, role_ordinal = as.integer(role_ordinal),
        candidate_ordinal = as.integer(i), candidate_id = candidate_id,
        isi_index = as.integer(index), isi_sec = as.numeric(value),
        support_role = role, connector_kind = connector_kind,
        canonical_pause_boundary = as.logical(excluded_pause),
        active_state_support = isTRUE(direct), episode_membership = TRUE,
        stringsAsFactors = FALSE, check.names = FALSE
      )
    }
    reported_connector_n <- suppressWarnings(as.integer(
      row$hf_spiking_connector_count[[1L]] %||% NA_integer_))
    candidate_rows[[i]] <- data.frame(
      train = context$train, candidate_ordinal = as.integer(i),
      candidate_id = candidate_id, start_isi = as.integer(start),
      end_isi = as.integer(end), envelope_isi_n = as.integer(length(indices)),
      direct_support_isi_n = as.integer(direct_n),
      tolerated_connector_isi_n = as.integer(connector_n),
      moderate_connector_isi_n = as.integer(moderate_n),
      transparent_artifact_connector_isi_n = as.integer(artifact_n),
      canonical_pause_intrusion_isi_n = as.integer(boundary_n),
      reported_connector_isi_n = as.integer(reported_connector_n),
      selected_broad_parent = candidate_id %in% selected_ids,
      state_family = "broad_high_frequency_state",
      parent_state_class = "high_frequency_spiking",
      subtype_status = "deferred_same_frozen_direct_support",
      candidate_payload_sha256 =
        stpd_candidate_lineage_broad_hfs_hash(row, "candidate"),
      stringsAsFactors = FALSE, check.names = FALSE
    )
  }
  candidates <- if (length(candidate_rows)) dplyr::bind_rows(candidate_rows) else
    stpd_candidate_lineage_empty_hook_payload("hfs_state_candidates_v1")
  support_roles <- if (length(role_rows)) dplyr::bind_rows(role_rows) else
    stpd_candidate_lineage_empty_hook_payload("hfs_state_support_roles_v1")
  parent_selection <- if (nrow(candidates)) {
    data.frame(
      train = candidates$train, candidate_ordinal = candidates$candidate_ordinal,
      candidate_id = candidates$candidate_id,
      selected_before_nested_review = candidates$selected_broad_parent,
      selected_after_nested_review = candidates$candidate_id %in% selected_ids,
      parent_signature_unchanged = rep(
        identical(context$provisional_signature, context$final_signature),
        nrow(candidates)),
      burst_veto_applied = rep(FALSE, nrow(candidates)),
      subtype_changes_parent_geometry = rep(FALSE, nrow(candidates)),
      candidate_payload_sha256 = candidates$candidate_payload_sha256,
      stringsAsFactors = FALSE, check.names = FALSE
    )
  } else stpd_candidate_lineage_empty_hook_payload(
    "hfs_state_parent_selection_v1")

  exact_connector <- !nrow(candidates) || all(
    is.na(candidates$reported_connector_isi_n) |
      candidates$reported_connector_isi_n ==
        candidates$moderate_connector_isi_n +
          candidates$canonical_pause_intrusion_isi_n)
  envelope_closed <- !nrow(candidates) || all(
    candidates$envelope_isi_n == candidates$direct_support_isi_n +
      candidates$tolerated_connector_isi_n +
        candidates$canonical_pause_intrusion_isi_n)
  pause_excluded <- !nrow(support_roles) || all(
    !support_roles$canonical_pause_boundary |
      (support_roles$support_role == "excluded_pause_direct_support" &
         !support_roles$active_state_support &
         support_roles$episode_membership)
  )
  signature_equal <- identical(
    context$provisional_signature, context$final_signature)
  entry <- data.frame(
    train = context$train, root_id = "broad_hfs_state_root",
    broad_hfs_requested = requested, generator_invoked = requested,
    n_interval_rows = context$n_interval_rows,
    min_isi_sec = context$min_isi_sec,
    hard_boundary_n = as.integer(nrow(context$hard_boundaries)),
    unique_parent_generator = "stpd_event_core_detect_hf_spiking",
    subtype_generation_status = "deferred_same_frozen_direct_support",
    burst_to_state_veto_allowed = FALSE,
    input_data_sha256 = context$input_data_sha256,
    params_payload_sha256 = context$params_payload_sha256,
    vp_payload_sha256 = context$vp_payload_sha256,
    hard_boundary_payload_sha256 = context$hard_boundary_payload_sha256,
    stringsAsFactors = FALSE, check.names = FALSE
  )
  receipt <- data.frame(
    train = context$train, root_id = "broad_hfs_state_root",
    candidate_n = as.integer(nrow(candidates)),
    selected_parent_n = as.integer(sum(candidates$selected_broad_parent)),
    direct_support_isi_n = as.integer(sum(candidates$direct_support_isi_n)),
    tolerated_connector_isi_n =
      as.integer(sum(candidates$tolerated_connector_isi_n)),
    canonical_pause_intrusion_isi_n =
      as.integer(sum(candidates$canonical_pause_intrusion_isi_n)),
    connector_identity_status = if (exact_connector)
      "validated_exact" else "mismatch",
    support_envelope_closure_status = if (envelope_closed)
      "validated_separate" else "mismatch",
    parent_signature_status = if (signature_equal)
      "validated_unchanged" else "mismatch",
    burst_overlay_policy = "orthogonal_event_non_veto",
    pause_direct_support_policy = if (pause_excluded)
      "pause_excluded_from_direct_support_preserved_in_episode_envelope" else
      "mismatch",
    subtype_policy = "hft_hfi_descriptive_on_frozen_parent_support",
    scientific_result_influence = "none_observer_only",
    publication_authority = FALSE,
    entry_payload_sha256 = stpd_candidate_lineage_broad_hfs_hash(entry, "entry"),
    candidate_payload_sha256 = stpd_candidate_lineage_broad_hfs_hash(
      candidates, "candidates"),
    support_role_payload_sha256 = stpd_candidate_lineage_broad_hfs_hash(
      support_roles, "support-roles"),
    parent_selection_payload_sha256 = stpd_candidate_lineage_broad_hfs_hash(
      parent_selection, "parent-selection"),
    scientific_hfs_output_sha256 = stpd_candidate_lineage_broad_hfs_hash(
      hfs, "scientific-output"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  if (!exact_connector || !envelope_closed || !pause_excluded ||
      !signature_equal) return(NULL)
  list(entry = entry, candidates = candidates, support_roles = support_roles,
       parent_selection = parent_selection, receipt = receipt)
}

stpd_candidate_lineage_begin_broad_hfs_state <- function(
    shard, train, dat, params, vp, min_isi_sec, patterns, hard_boundaries,
    materialization_state_boundaries = data.frame()) {
  if (is.null(shard)) return(invisible(NULL))
  stpd_candidate_lineage_validate_train_collector(
    shard, "open", train, validate_observations = FALSE
  )
  context <- unserialize(serialize(list(
    train = as.character(train)[1L],
    min_isi_sec = as.numeric(min_isi_sec)[1L], patterns = as.character(patterns),
    n_interval_rows = as.integer(nrow(dat)),
    isi = suppressWarnings(as.numeric(dat$ISI_sec)),
    hard_boundaries = as.data.frame(hard_boundaries,
      stringsAsFactors = FALSE),
    materialization_state_boundaries = as.data.frame(
      materialization_state_boundaries, stringsAsFactors = FALSE),
    input_data_sha256 = stpd_candidate_lineage_broad_hfs_hash(dat, "input"),
    params_payload_sha256 = stpd_candidate_lineage_broad_hfs_hash(
      params, "params"),
    vp_payload_sha256 = stpd_candidate_lineage_broad_hfs_hash(vp, "vp"),
    hard_boundary_payload_sha256 = stpd_candidate_lineage_broad_hfs_hash(
      hard_boundaries, "boundaries"),
    materialization_boundary_payload_sha256 =
      stpd_candidate_lineage_broad_hfs_hash(
        materialization_state_boundaries, "materialization-boundaries"
      )), NULL, version = 3L))
  name <- "broad_hfs_begin_context"
  if (exists(name, shard, inherits = FALSE)) {
    if (!bindingIsLocked(name, shard) || !identical(get(name, shard), context))
      stpd_candidate_lineage_abort("collector_broad_hfs_begin_conflict",
        "Broad-HFS begin context conflicts with its first capture.")
  } else {
    assign(name, context, shard); lockBinding(name, shard)
  }
  invisible(context)
}

stpd_candidate_lineage_capture_broad_hfs_state <- function(
    shard, train, scientific_hfs, scientific_selected_hfs,
    provisional_signature, final_signature) {
  if (is.null(shard)) return(invisible(NULL))
  name <- "broad_hfs_begin_context"
  if (!exists(name, shard, inherits = FALSE) || !bindingIsLocked(name, shard))
    stpd_candidate_lineage_abort("collector_broad_hfs_begin_missing",
      "Broad-HFS capture requires its locked live begin context.")
  context <- get(name, shard)
  if (!identical(context$train, as.character(train)[1L]))
    stpd_candidate_lineage_abort("collector_broad_hfs_train_conflict",
      "Broad-HFS capture train differs from its begin context.")
  context$scientific_hfs <- unserialize(serialize(
    scientific_hfs, NULL, version = 3L))
  context$scientific_selected_hfs <- unserialize(serialize(
    scientific_selected_hfs, NULL, version = 3L))
  context$provisional_signature <- as.character(provisional_signature)[1L]
  context$final_signature <- as.character(final_signature)[1L]
  replay <- stpd_candidate_lineage_broad_hfs_replay(context)
  if (is.null(replay)) stpd_candidate_lineage_abort(
    "collector_broad_hfs_replay_failed",
    "Broad-HFS support, connector, Pause, or parent closure failed.")
  assign("broad_hfs_context", context, shard)
  lockBinding("broad_hfs_context", shard)
  assign("broad_hfs_replay_cache", replay, shard)
  lockBinding("broad_hfs_replay_cache", shard)
  hooks <- stpd_candidate_lineage_broad_hfs_hooks()
  for (hook in names(hooks))
    stpd_candidate_lineage_collector_capture(shard, hook, replay[[hooks[[hook]]]])
  invisible(replay)
}

stpd_candidate_lineage_broad_hfs_payload_is_valid <- function(
    shard, hook, payload) {
  if (!exists("broad_hfs_context", shard, inherits = FALSE) ||
      !bindingIsLocked("broad_hfs_context", shard) ||
      !exists("broad_hfs_replay_cache", shard, inherits = FALSE) ||
      !bindingIsLocked("broad_hfs_replay_cache", shard)) return(FALSE)
  fresh <- stpd_candidate_lineage_broad_hfs_replay(
    get("broad_hfs_context", shard, inherits = FALSE))
  cached <- get("broad_hfs_replay_cache", shard, inherits = FALSE)
  if (is.null(fresh) || !identical(fresh, cached)) return(FALSE)
  map <- stpd_candidate_lineage_broad_hfs_hooks()
  key <- unname(map[[hook]])
  if (is.null(key) || !identical(payload, cached[[key]])) return(FALSE)
  position <- match(hook, names(map))
  prior <- if (position <= 1L) character() else names(map)[seq_len(position - 1L)]
  all(vapply(prior, function(previous) identical(
    shard$observations[[previous]], cached[[map[[previous]]]]), logical(1)))
}
