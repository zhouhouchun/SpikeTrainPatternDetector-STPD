# Gate 1B nested-HFS Review observation ----------------------------------
#
# Observer-only closure of locally accelerated proposals inside an already
# frozen Broad-HFS State. These proposals remain Review evidence and cannot
# create, split, reshape, or veto the parent State or self-promote to Event.

stpd_candidate_lineage_nested_hfs_review_hooks <- function() c(
  nested_hfs_review_entry_v1 = "entry",
  nested_hfs_review_candidates_v1 = "candidates",
  nested_hfs_review_local_contrast_v1 = "local_contrast",
  nested_hfs_review_parent_invariance_v1 = "parent_invariance",
  nested_hfs_review_receipt_v1 = "receipt"
)

stpd_candidate_lineage_nested_hfs_review_hash <- function(x, stage) {
  stpd_threshold_first_hash_domain(paste0("stpd-nested-hfs-review-", stage, "-v1"), x)
}

stpd_candidate_lineage_nested_hfs_review_replay <- function(context) {
  raw <- context$scientific_raw
  standardized <- context$scientific_standardized
  shadow_candidates <- as.data.frame(
    context$scientific_shadow$candidates %||% data.frame(),
    stringsAsFactors = FALSE
  )
  nested_shadow <- if (nrow(shadow_candidates) &&
      "candidate_layer" %in% names(shadow_candidates)) {
    shadow_candidates[
      as.character(shadow_candidates$candidate_layer) ==
        "nested_hfs_local_rate_contrast",
      , drop = FALSE
    ]
  } else data.frame()
  requested <- nrow(context$selected_parents) > 0L &&
    any(c("burst", "long_burst") %in% context$patterns)
  candidate_rows <- list()
  contrast_rows <- list()
  for (i in seq_len(nrow(standardized))) {
    row <- standardized[i, , drop = FALSE]
    candidate_id <- as.character(row$candidate_id[[1L]] %||% "")
    shadow_index <- if (nrow(nested_shadow)) {
      which(as.character(nested_shadow$candidate_id) == candidate_id)
    } else integer()
    shadow <- if (length(shadow_index)) nested_shadow[shadow_index[[1L]], , drop = FALSE] else data.frame()
    raw_index <- if (nrow(raw)) which(
      as.character(raw$candidate_id) == candidate_id |
        (as.integer(raw$start_isi) == as.integer(row$start_isi[[1L]]) &
           as.integer(raw$end_isi) == as.integer(row$end_isi[[1L]]) &
           as.character(raw$parent_hfs_candidate_id) ==
             as.character(row$parent_hfs_candidate_id[[1L]]))
    ) else integer()
    raw_row <- if (length(raw_index)) raw[raw_index[[1L]], , drop = FALSE] else row
    candidate_rows[[i]] <- data.frame(
      train = context$train, candidate_ordinal = as.integer(i),
      candidate_id = candidate_id,
      parent_hfs_candidate_id = as.character(row$parent_hfs_candidate_id[[1L]] %||% ""),
      start_isi = as.integer(row$start_isi[[1L]]),
      end_isi = as.integer(row$end_isi[[1L]]),
      n_spikes = as.integer(row$n_spikes[[1L]]),
      final_label = as.character(row$final_label[[1L]]),
      action = as.character(row$action[[1L]]),
      review_only = isTRUE(row$review_only[[1L]]),
      canonical_eligible = isTRUE(row$canonical_eligible[[1L]]),
      semantic_track = if (nrow(shadow)) as.character(shadow$semantic_track[[1L]]) else "",
      review_promotion_required = if (nrow(shadow)) isTRUE(shadow$review_promotion_required[[1L]]) else FALSE,
      candidate_payload_sha256 = stpd_candidate_lineage_nested_hfs_review_hash(row, "candidate"),
      stringsAsFactors = FALSE, check.names = FALSE
    )
    num <- function(name) suppressWarnings(as.numeric(raw_row[[name]][[1L]] %||% NA_real_))
    contrast_rows[[i]] <- data.frame(
      train = context$train, candidate_ordinal = as.integer(i),
      candidate_id = candidate_id,
      seed_pair_local_rank = num("seed_pair_local_rank"),
      seed_left_ratio = num("seed_left_ratio"),
      seed_right_ratio = num("seed_right_ratio"),
      seed_geom_ratio = num("seed_geom_ratio"),
      final_left_ratio = num("final_left_ratio"),
      final_right_ratio = num("final_right_ratio"),
      final_geom_ratio = num("final_geom_ratio"),
      evidence_strength = as.character(raw_row$review_evidence_strength[[1L]] %||% ""),
      absolute_pattern_threshold_used = isTRUE(raw_row$absolute_pattern_threshold_used[[1L]]),
      local_hfs_background_only = as.character(raw_row$candidate_source[[1L]] %||% "") ==
        "accepted_broad_hfs_local_context",
      stringsAsFactors = FALSE, check.names = FALSE
    )
  }
  candidates <- if (length(candidate_rows)) dplyr::bind_rows(candidate_rows) else
    stpd_candidate_lineage_empty_hook_payload("nested_hfs_review_candidates_v1")
  local_contrast <- if (length(contrast_rows)) dplyr::bind_rows(contrast_rows) else
    stpd_candidate_lineage_empty_hook_payload("nested_hfs_review_local_contrast_v1")
  signature_equal <- identical(
    context$provisional_signature, context$final_signature)
  parent_invariance <- data.frame(
    train = context$train,
    provisional_parent_signature = context$provisional_signature,
    final_parent_signature = context$final_signature,
    parent_signature_unchanged = signature_equal,
    review_can_veto_state = FALSE, review_can_reshape_state = FALSE,
    review_can_create_state = FALSE,
    stringsAsFactors = FALSE, check.names = FALSE
  )
  entry <- data.frame(
    train = context$train, root_id = "nested_hfs_review_root",
    nested_review_requested = requested, generator_invoked = requested,
    selected_broad_hfs_parent_n = as.integer(nrow(context$selected_parents)),
    hard_boundary_n = as.integer(nrow(context$hard_boundaries)),
    min_isi_sec = context$min_isi_sec, proposal_semantic_track = "review",
    automatic_event_promotion_allowed = FALSE,
    input_data_sha256 = context$input_data_sha256,
    selected_parent_payload_sha256 = context$selected_parent_payload_sha256,
    settings_payload_sha256 = context$settings_payload_sha256,
    hard_boundary_payload_sha256 = context$hard_boundary_payload_sha256,
    stringsAsFactors = FALSE, check.names = FALSE
  )
  review_identity <- !nrow(candidates) || all(
    candidates$final_label == "possible_burst" &
      candidates$action == "demote_to_possible" & candidates$review_only &
      !candidates$canonical_eligible & candidates$semantic_track == "review")
  contrast_valid <- !nrow(local_contrast) || all(
    is.finite(local_contrast$seed_pair_local_rank) &
      is.finite(local_contrast$seed_left_ratio) &
      is.finite(local_contrast$seed_right_ratio) &
      is.finite(local_contrast$seed_geom_ratio) &
      !local_contrast$absolute_pattern_threshold_used &
      local_contrast$local_hfs_background_only)
  promotion_separate <- !nrow(candidates) || all(candidates$review_promotion_required)
  receipt <- data.frame(
    train = context$train, root_id = "nested_hfs_review_root",
    raw_candidate_n = as.integer(nrow(raw)),
    standardized_candidate_n = as.integer(nrow(standardized)),
    review_track_candidate_n = as.integer(nrow(nested_shadow)),
    review_identity_status = if (review_identity) "validated_review_not_event" else "mismatch",
    local_contrast_status = if (contrast_valid) "validated_local_hfs_background" else "mismatch",
    parent_invariance_status = if (signature_equal) "validated_no_state_effect" else "mismatch",
    promotion_authority_status = if (promotion_separate) "separate_review_transition_required" else "mismatch",
    scientific_result_influence = "none_review_side_channel_only",
    publication_authority = FALSE,
    entry_payload_sha256 = stpd_candidate_lineage_nested_hfs_review_hash(entry, "entry"),
    candidate_payload_sha256 = stpd_candidate_lineage_nested_hfs_review_hash(candidates, "candidates"),
    local_contrast_payload_sha256 = stpd_candidate_lineage_nested_hfs_review_hash(local_contrast, "local-contrast"),
    parent_invariance_payload_sha256 = stpd_candidate_lineage_nested_hfs_review_hash(parent_invariance, "parent-invariance"),
    scientific_raw_output_sha256 = stpd_candidate_lineage_nested_hfs_review_hash(raw, "scientific-raw"),
    scientific_standardized_output_sha256 = stpd_candidate_lineage_nested_hfs_review_hash(standardized, "scientific-standardized"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  counts_equal <- nrow(raw) == nrow(standardized) &&
    nrow(standardized) == nrow(nested_shadow)
  if (!review_identity || !contrast_valid || !signature_equal ||
      !promotion_separate || !counts_equal) return(NULL)
  list(entry = entry, candidates = candidates, local_contrast = local_contrast,
       parent_invariance = parent_invariance, receipt = receipt)
}

stpd_candidate_lineage_begin_nested_hfs_review <- function(
    shard, train, dat, params, vp, min_isi_sec, patterns,
    selected_parents, hard_boundaries) {
  if (is.null(shard)) return(invisible(NULL))
  stpd_candidate_lineage_validate_train_collector(
    shard, "open", train, validate_observations = FALSE
  )
  settings <- stpd_nested_hfs_detector_settings(params, vp)
  context <- unserialize(serialize(list(
    train = as.character(train)[1L], patterns = as.character(patterns),
    min_isi_sec = as.numeric(min_isi_sec)[1L], selected_parents = selected_parents,
    hard_boundaries = as.data.frame(hard_boundaries, stringsAsFactors = FALSE),
    input_data_sha256 = stpd_candidate_lineage_nested_hfs_review_hash(dat, "input"),
    selected_parent_payload_sha256 = stpd_candidate_lineage_nested_hfs_review_hash(selected_parents, "parents"),
    settings_payload_sha256 = stpd_candidate_lineage_nested_hfs_review_hash(settings, "settings"),
    hard_boundary_payload_sha256 = stpd_candidate_lineage_nested_hfs_review_hash(hard_boundaries, "boundaries")
  ), NULL, version = 3L))
  name <- "nested_hfs_review_begin_context"
  if (exists(name, shard, inherits = FALSE)) {
    if (!bindingIsLocked(name, shard) || !identical(get(name, shard), context))
      stpd_candidate_lineage_abort("collector_nested_hfs_review_begin_conflict",
        "Nested-HFS Review begin context conflicts with its first capture.")
  } else {
    assign(name, context, shard); lockBinding(name, shard)
  }
  invisible(context)
}

stpd_candidate_lineage_capture_nested_hfs_review <- function(
    shard, train, scientific_raw, scientific_standardized, scientific_shadow,
    provisional_signature, final_signature) {
  if (is.null(shard)) return(invisible(NULL))
  name <- "nested_hfs_review_begin_context"
  if (!exists(name, shard, inherits = FALSE) || !bindingIsLocked(name, shard))
    stpd_candidate_lineage_abort("collector_nested_hfs_review_begin_missing",
      "Nested-HFS Review capture requires its locked live begin context.")
  context <- get(name, shard)
  if (!identical(context$train, as.character(train)[1L]))
    stpd_candidate_lineage_abort("collector_nested_hfs_review_train_conflict",
      "Nested-HFS Review capture train differs from its begin context.")
  context$scientific_raw <- unserialize(serialize(scientific_raw, NULL, version = 3L))
  context$scientific_standardized <- unserialize(serialize(scientific_standardized, NULL, version = 3L))
  context$scientific_shadow <- unserialize(serialize(scientific_shadow, NULL, version = 3L))
  context$provisional_signature <- as.character(provisional_signature)[1L]
  context$final_signature <- as.character(final_signature)[1L]
  replay <- stpd_candidate_lineage_nested_hfs_review_replay(context)
  if (is.null(replay)) stpd_candidate_lineage_abort(
    "collector_nested_hfs_review_replay_failed",
    "Nested-HFS Review identity, local contrast, parent invariance, or promotion closure failed.")
  assign("nested_hfs_review_context", context, shard); lockBinding("nested_hfs_review_context", shard)
  assign("nested_hfs_review_replay_cache", replay, shard); lockBinding("nested_hfs_review_replay_cache", shard)
  hooks <- stpd_candidate_lineage_nested_hfs_review_hooks()
  for (hook in names(hooks))
    stpd_candidate_lineage_collector_capture(shard, hook, replay[[hooks[[hook]]]])
  invisible(replay)
}

stpd_candidate_lineage_nested_hfs_review_payload_is_valid <- function(
    shard, hook, payload) {
  if (!exists("nested_hfs_review_context", shard, inherits = FALSE) ||
      !bindingIsLocked("nested_hfs_review_context", shard) ||
      !exists("nested_hfs_review_replay_cache", shard, inherits = FALSE) ||
      !bindingIsLocked("nested_hfs_review_replay_cache", shard)) return(FALSE)
  fresh <- stpd_candidate_lineage_nested_hfs_review_replay(
    get("nested_hfs_review_context", shard))
  cached <- get("nested_hfs_review_replay_cache", shard)
  if (is.null(fresh) || !identical(fresh, cached)) return(FALSE)
  map <- stpd_candidate_lineage_nested_hfs_review_hooks()
  key <- unname(map[[hook]])
  if (is.null(key) || !identical(payload, cached[[key]])) return(FALSE)
  position <- match(hook, names(map))
  prior <- if (position <= 1L) character() else names(map)[seq_len(position - 1L)]
  all(vapply(prior, function(previous) identical(
    shard$observations[[previous]], cached[[map[[previous]]]]), logical(1)))
}
