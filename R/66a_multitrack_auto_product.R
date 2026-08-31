# Gate-B-pending automatic multi-track candidate product.
#
# The product is a canonical candidate schema, not yet an authoritative
# prediction record and not biological ground truth. It is materialized from the intact
# Phase 1A/1B multi-track evidence attached to each train before the legacy HFS
# protection and global single-label selector run.  The legacy pattern_auto and
# events products remain an unchanged, parallel compatibility path.

stpd_multitrack_auto_schema_version <- function() {
  "stpd_multitrack_auto_v3"
}

stpd_multitrack_auto_result_key <- function() {
  "multitrack_auto"
}

stpd_multitrack_auto_abort <- function(code, message) {
  condition <- structure(
    list(
      message = as.character(message)[1],
      call = NULL,
      code = as.character(code)[1]
    ),
    class = c("stpd_multitrack_auto_error", "error", "condition")
  )
  stop(condition)
}

stpd_multitrack_auto_chr <- function(x, default = "") {
  value <- as.character(x %||% default)
  if (length(value) == 0L || is.na(value[1])) default else value[1]
}

stpd_multitrack_auto_lgl <- function(x, default = FALSE) {
  value <- suppressWarnings(as.logical(x %||% default))
  if (length(value) == 0L || is.na(value[1])) default else value[1]
}

stpd_multitrack_auto_int <- function(x, default = NA_integer_) {
  value <- suppressWarnings(as.integer(x %||% default))
  if (length(value) == 0L || is.na(value[1])) default else value[1]
}

stpd_multitrack_auto_num <- function(x, default = NA_real_) {
  value <- suppressWarnings(as.numeric(x %||% default))
  if (length(value) == 0L || is.na(value[1])) default else value[1]
}

stpd_multitrack_auto_hash <- function(x) {
  normalized <- if (exists("stpd_params_hash_normalize", mode = "function")) {
    stpd_params_hash_normalize(x)
  } else {
    x
  }
  digest::digest(normalized, algo = "sha256", serialize = TRUE)
}

stpd_multitrack_auto_policy_hash <- function(params) {
  stpd_multitrack_auto_hash(list(
    schema_version = stpd_multitrack_auto_schema_version(),
    source_multitrack_policy_hash = stpd_multitrack_policy_hash(params),
    canonical_event_mapping = c(
      burst = "family=burst|extent=classic|frequency=unresolved",
      long_burst = "family=burst|extent=long|frequency=unresolved",
      prolonged_burst = "family=burst|extent=prolonged|frequency=unresolved",
      high_frequency_burst =
        "family=burst|extent=unresolved|frequency=high_frequency",
      hf_burst =
        "family=burst|extent=unresolved|frequency=high_frequency"
    ),
    same_geometry_evidence_rule = paste(
      "one_canonical_event|merge_all_event_labels|",
      "explicit_extent_or_unresolved_conflict|explicit_hfb_frequency"
    ),
    hfs_burst_rule = "non_destructive_state_event_coexistence",
    state_hierarchy_rule = paste(
      "broad_hfs_unique_parent|hft_hfi_are_descriptive_subtypes|",
      "tonic_outside_broad_hfs|subtype_never_changes_parent_geometry"
    ),
    state_subtype_rule = paste(
      "median_adjacent_cv2_on_frozen_direct_support|regular<=0.5|",
      "irregular>=0.8|otherwise_unresolved"
    ),
    pause_rule = paste(
      "canonical_pause_excluded_from_direct_support|",
      "accepted_hfs_episode_envelope_preserved|",
      "pause_children_inherit_parent_acceptance|",
      "downstream_detector_reentry_forbidden"
    ),
    dominance_rule = "burst_rich_review_flag_only",
    promotion_contract = paste(
      "gate_b_runtime_attested|automatic_prediction_record_after_pass|",
      "biological_ground_truth=false|detector_performance_gate_c"
    )
  ))
}

stpd_multitrack_auto_id <- function(kind, run_id, train, source_id, ...) {
  payload <- paste(
    stpd_multitrack_auto_schema_version(), kind, run_id, train, source_id,
    paste(..., collapse = "\u001f"), sep = "\u001f"
  )
  paste0(
    "mta_", kind, "_",
    digest::digest(payload, algo = "sha256", serialize = FALSE)
  )
}

stpd_multitrack_auto_empty_events <- function() {
  data.frame(
    schema_version = character(), policy_hash = character(),
    run_id = character(), params_hash = character(), authoritative = logical(),
    train = character(), event_id = character(), source_interval_id = character(),
    source_candidate_id = character(), source_candidate_key = character(),
    source_label = character(), event_family = character(),
    extent_class = character(), frequency_class = character(),
    provenance_source_interval_ids = character(),
    provenance_source_candidate_ids = character(),
    provenance_source_candidate_keys = character(),
    provenance_source_labels = character(),
    provenance_candidate_layers = character(),
    provenance_candidate_sources = character(),
    extent_evidence_labels = character(),
    frequency_evidence_labels = character(),
    extent_conflict = logical(), modifier_diagnostic = character(),
    start_isi = integer(), end_isi = integer(), n_isi = integer(),
    n_spikes = integer(), start_time_sec = double(), end_time_sec = double(),
    candidate_layer = character(), candidate_source = character(),
    stringsAsFactors = FALSE
  )
}

stpd_multitrack_auto_empty_states <- function() {
  data.frame(
    schema_version = character(), policy_hash = character(),
    run_id = character(), params_hash = character(), authoritative = logical(),
    train = character(), state_id = character(), source_interval_id = character(),
    source_candidate_id = character(), source_candidate_key = character(),
    source_label = character(), state_class = character(),
    state_family = character(), state_frequency_class = character(),
    state_regularity_class = character(), state_subtype = character(),
    subtype_status = character(), subtype_n_valid_isi = integer(),
    subtype_effective_duration_sec = double(),
    subtype_frequency_hz = double(), subtype_median_cv2 = double(),
    subtype_rule = character(),
    interval_kind = character(), parent_source_interval_id = character(),
    split_kind = character(), state_episode_id = character(),
    episode_source_interval_id = character(), episode_start_isi = integer(),
    episode_end_isi = integer(), episode_n_isi = integer(),
    support_fragment_index = integer(), support_fragment_n = integer(),
    episode_direct_support_isi_n = integer(), episode_pause_isi_n = integer(),
    support_acceptance_basis = character(),
    start_isi = integer(), end_isi = integer(),
    n_isi = integer(), n_spikes = integer(), start_time_sec = double(),
    end_time_sec = double(), candidate_layer = character(),
    candidate_source = character(), stringsAsFactors = FALSE
  )
}

stpd_multitrack_auto_empty_gaps <- function() {
  data.frame(
    schema_version = character(), policy_hash = character(),
    run_id = character(), params_hash = character(), authoritative = logical(),
    train = character(), gap_id = character(), source_interval_id = character(),
    source_candidate_id = character(), source_candidate_key = character(),
    source_label = character(), gap_class = character(),
    gap_semantics = character(), hard_for_event = logical(),
    hard_for_state_direct_support = logical(),
    envelope_bridge_eligible = logical(), candidate_decision = character(),
    candidate_reason_code = character(),
    start_isi = integer(), end_isi = integer(), n_isi = integer(),
    start_time_sec = double(), end_time_sec = double(),
    candidate_layer = character(), candidate_source = character(),
    stringsAsFactors = FALSE
  )
}

stpd_multitrack_auto_empty_review <- function() {
  data.frame(
    schema_version = character(), policy_hash = character(),
    run_id = character(), params_hash = character(), authoritative = logical(),
    train = character(), review_candidate_id = character(),
    source_interval_id = character(), source_candidate_id = character(),
    source_candidate_key = character(), review_label = character(),
    target_track = character(), target_label = character(),
    promotion_required = logical(), selected_within_track = logical(),
    active_in_review_track = logical(), review_policy_status = character(),
    start_isi = integer(), end_isi = integer(), n_isi = integer(),
    start_time_sec = double(), end_time_sec = double(),
    candidate_layer = character(), candidate_source = character(),
    stringsAsFactors = FALSE
  )
}

stpd_multitrack_auto_empty_relationships <- function() {
  data.frame(
    schema_version = character(), policy_hash = character(),
    run_id = character(), params_hash = character(), authoritative = logical(),
    train = character(), relationship_id = character(), event_id = character(),
    state_id = character(), relationship_type = character(),
    compatibility_rule = character(), overlap_start_isi = integer(),
    overlap_end_isi = integer(), overlap_isi_n = integer(),
    event_overlap_fraction = double(), state_overlap_fraction = double(),
    intersection_over_union = double(), non_destructive = logical(),
    stringsAsFactors = FALSE
  )
}

stpd_multitrack_auto_empty_per_isi <- function() {
  data.frame(
    schema_version = character(), policy_hash = character(),
    run_id = character(), params_hash = character(), authoritative = logical(),
    train = character(), isi_index = integer(), timestamp_sec = double(),
    ISI_sec = double(), event_id = character(), event_family = character(),
    event_extent_class = character(), event_frequency_class = character(),
    state_id = character(), state_class = character(),
    state_family = character(), state_frequency_class = character(),
    state_regularity_class = character(), state_subtype = character(),
    gap_id = character(),
    gap_class = character(), state_episode_id = character(),
    state_episode_class = character(), state_support_role = character(),
    review_candidate_id = character(),
    review_label = character(), review_candidate_count = integer(),
    stringsAsFactors = FALSE
  )
}

stpd_multitrack_auto_empty_hfs_context <- function() {
  data.frame(
    schema_version = character(), policy_hash = character(),
    run_id = character(), params_hash = character(), authoritative = logical(),
    train = character(), state_id = character(), hfs_episode_id = character(),
    source_hfs_interval_id = character(),
    start_isi = integer(), end_isi = integer(), n_isi = integer(),
    selected_burst_event_n = integer(), selected_burst_group_n = integer(),
    burst_covered_isi_n = integer(), burst_isi_coverage = double(),
    contributor_event_ids = character(), burst_rich_review_flag = logical(),
    burst_rich_review_description = character(),
    packet_like_review_flag = logical(), packet_neighbor_review_flag = logical(),
    state_preserved = logical(), destructive_action_applied = logical(),
    stringsAsFactors = FALSE
  )
}

stpd_multitrack_auto_empty_invariants <- function() {
  data.frame(
    schema_version = character(), policy_hash = character(),
    run_id = character(), params_hash = character(), authoritative = logical(),
    invariant_id = character(), check_name = character(), status = character(),
    severity = character(), message = character(), stringsAsFactors = FALSE
  )
}

stpd_multitrack_auto_bind <- function(empty, rows) {
  if (length(rows) == 0L) return(empty)
  dplyr::bind_rows(c(list(empty), rows))
}

stpd_multitrack_auto_interval_overlap <- function(a_start, a_end, b_start, b_end) {
  as.integer(max(0L, min(a_end, b_end) - max(a_start, b_start) + 1L))
}

stpd_multitrack_auto_join_values <- function(x) {
  value <- as.character(x)
  value <- value[!is.na(value) & nzchar(value)]
  paste(sort(unique(value), method = "radix"), collapse = ";")
}

stpd_multitrack_auto_event_evidence_modifiers <- function(labels) {
  labels <- stpd_multitrack_shadow_normalize_label(labels)
  labels <- sort(unique(labels[!is.na(labels) & nzchar(labels)]), method = "radix")
  allowed <- c(
    "burst", "long_burst", "prolonged_burst",
    "high_frequency_burst", "hf_burst"
  )
  if (length(labels) == 0L || !all(labels %in% allowed)) {
    stpd_multitrack_auto_abort(
      "unsupported_event_label",
      "Canonical Event evidence contains no supported burst-family label."
    )
  }
  extent_map <- c(
    burst = "classic", long_burst = "long",
    prolonged_burst = "prolonged"
  )
  extent_labels <- intersect(names(extent_map), labels)
  extent_values <- unique(unname(extent_map[extent_labels]))
  extent_conflict <- length(extent_values) > 1L
  extent <- if (length(extent_values) == 1L) extent_values else "unresolved"
  frequency_labels <- intersect(
    c("high_frequency_burst", "hf_burst"), labels
  )
  diagnostic <- if (extent_conflict) {
    paste0(
      "conflicting_extent_evidence:",
      stpd_multitrack_auto_join_values(extent_labels)
    )
  } else {
    ""
  }
  list(
    family = "burst",
    extent = extent,
    frequency = if (length(frequency_labels) > 0L) {
      "high_frequency"
    } else {
      "unresolved"
    },
    extent_labels = stpd_multitrack_auto_join_values(extent_labels),
    frequency_labels = stpd_multitrack_auto_join_values(frequency_labels),
    extent_conflict = extent_conflict,
    diagnostic = diagnostic
  )
}

stpd_multitrack_auto_boundary_rows <- function(dat, gaps, min_isi_sec = 0.0009) {
  rows <- list()
  if (nrow(gaps) > 0L) {
    semantics <- as.character(gaps$gap_semantics %||% "canonical_pause")
    semantics[is.na(semantics) | !nzchar(semantics)] <- "canonical_pause"
    rows[[length(rows) + 1L]] <- data.frame(
      start_isi = as.integer(gaps$start_isi),
      end_isi = as.integer(gaps$end_isi),
      boundary_kind = "pause",
      gap_semantics = semantics,
      hard_for_event = as.logical(gaps$hard_for_event %||% TRUE),
      hard_for_state_direct_support = as.logical(
        gaps$hard_for_state_direct_support %||% TRUE
      ),
      stringsAsFactors = FALSE
    )
  }
  isi <- suppressWarnings(as.numeric(dat$ISI_sec %||% rep(NA_real_, nrow(dat))))
  invalid <- which(seq_along(isi) > 1L &
    (!is.finite(isi) | is_artifact_isi(isi, min_isi_sec)))
  if (length(invalid) > 0L) {
    rows[[length(rows) + 1L]] <- data.frame(
      start_isi = as.integer(invalid), end_isi = as.integer(invalid),
      boundary_kind = "invalid_support", gap_semantics = "",
      hard_for_event = TRUE, hard_for_state_direct_support = TRUE,
      stringsAsFactors = FALSE
    )
  }
  if (length(rows) == 0L) {
    return(data.frame(
      start_isi = integer(), end_isi = integer(),
      boundary_kind = character(), gap_semantics = character(),
      hard_for_event = logical(),
      hard_for_state_direct_support = logical(), stringsAsFactors = FALSE
    ))
  }
  dplyr::bind_rows(rows)
}

stpd_multitrack_auto_mask_boundaries <- function(dat, boundaries) {
  out <- dat
  if (nrow(boundaries) == 0L || !("ISI_sec" %in% names(out))) return(out)
  for (i in seq_len(nrow(boundaries))) {
    idx <- seq.int(
      max(2L, as.integer(boundaries$start_isi[i])),
      min(nrow(out), as.integer(boundaries$end_isi[i]))
    )
    if (length(idx) > 0L) out$ISI_sec[idx] <- NA_real_
  }
  out
}

stpd_multitrack_auto_redetect_selected <- function(
    dat, params, train, semantic_track, label = "", boundaries = NULL) {
  min_isi <- stpd_multitrack_auto_num(
    (params$detector %||% list())$min_valid_isi_sec, 0.0009
  )
  boundaries <- boundaries %||% data.frame(
    start_isi = integer(), end_isi = integer()
  )
  masked <- stpd_multitrack_auto_mask_boundaries(dat, boundaries)
  vp <- stpd_event_grammar_params_impl(
    dat, params, min_isi_sec = min_isi, train = train
  )
  rows <- if (identical(semantic_track, "event")) {
    list(
      stpd_event_grammar_detect_burst_events(
        masked, params, vp, min_isi_sec = min_isi, train = train
      ),
      stpd_event_core_detect_hard_isi_thresholds(
        masked, params, vp, min_isi_sec = min_isi, train = train
      )
    )
  } else if (identical(label, "high_frequency_spiking")) {
    list(stpd_event_core_detect_hf_spiking(
      masked, params, vp, min_isi_sec = min_isi, train = train
    ))
  } else if (identical(label, "high_frequency_tonic")) {
    list(stpd_event_core_detect_hf_tonic(
      masked, params, vp, min_isi_sec = min_isi, train = train
    ))
  } else if (identical(label, "tonic")) {
    list(stpd_event_core_detect_tonic(
      masked, params, vp, min_isi_sec = min_isi, train = train
    ))
  } else {
    list(data.frame())
  }
  rows <- rows[vapply(rows, function(x) is.data.frame(x) && nrow(x) > 0L,
                       logical(1))]
  if (length(rows) == 0L) return(data.frame())
  candidates <- dplyr::bind_rows(rows)
  shadow <- stpd_multitrack_shadow_select(
    candidates,
    patterns = params$detector$patterns_to_run %||% stpd_default_patterns_to_run(),
    params = params
  )
  selected <- as.data.frame(shadow$candidates, stringsAsFactors = FALSE)
  if (nrow(selected) == 0L) return(selected)
  keep <- as.logical(selected$selected_within_track)
  keep[is.na(keep)] <- FALSE
  normalized <- stpd_multitrack_shadow_normalize_label(selected$final_label)
  keep <- keep & as.character(selected$semantic_track) == semantic_track
  if (identical(semantic_track, "event")) {
    keep <- keep & normalized %in% c(
      "burst", "long_burst", "prolonged_burst",
      "high_frequency_burst", "hf_burst"
    )
  } else {
    keep <- keep & normalized == label
  }
  selected[keep, , drop = FALSE]
}

stpd_multitrack_auto_redetected_interval <- function(
    parent, candidate, train, semantic_track, split_kind, child_index, dat) {
  start <- as.integer(candidate$start_isi)
  end <- as.integer(candidate$end_isi)
  label <- stpd_multitrack_shadow_normalize_label(candidate$final_label)
  candidate_key <- paste(
    as.character(parent$candidate_key), "gate_b_redetection", label,
    start, end, child_index, sep = "|"
  )
  kind <- paste0(semantic_track, "_redetected_fragment")
  interval_id <- stpd_multitrack_preview_interval_id(
    train, kind, candidate_key, start, end
  )
  out <- parent
  out$interval_id <- interval_id
  out$interval_kind <- kind
  out$candidate_id <- paste0(
    "gate_b_", semantic_track, "_child_",
    digest::digest(candidate_key, algo = "xxhash64", serialize = FALSE)
  )
  out$candidate_key <- candidate_key
  out$root_candidate_id <- as.character(parent$root_candidate_id)
  out$parent_interval_id <- as.character(parent$interval_id)
  out$candidate_layer <- as.character(candidate$candidate_layer %||%
                                        paste0("gate_b_", semantic_track,
                                               "_redetection"))
  out$candidate_source <- "gate_b_full_detector_redetection"
  out$semantic_track <- semantic_track
  out$label <- label
  out$start_isi <- start
  out$end_isi <- end
  out$n_isi <- as.integer(end - start + 1L)
  out$start_time_sec <- stpd_multitrack_preview_time(dat, start - 1L)
  out$end_time_sec <- stpd_multitrack_preview_time(dat, end)
  out$selected_within_track <- TRUE
  out$active_in_preview <- TRUE
  out$activity_status <- "active_gate_b_residual_redetection_passed"
  out$track_selection_status <- "selected_after_full_detector_redetection"
  out$review_target_track <- ""
  out$review_target_label <- ""
  out$review_promotion_required <- FALSE
  out$review_policy_status <- ""
  out$eligible_for_primary_event_metrics <- identical(semantic_track, "event")
  out$eligible_for_state_split <- identical(semantic_track, "event")
  out$eligible_for_hfs_dominance <- identical(semantic_track, "event")
  out$split_kind <- split_kind
  out$gate_evaluated <- TRUE
  out$provisional_gate_pass <- TRUE
  out$provisional_gate_status <- "full_detector_redetection_pass"
  out$source_track_policy_version <- "gate_b_residual_redetection_v1"
  out
}

stpd_multitrack_auto_inherited_residual_interval <- function(
    parent, train, label, start_isi, end_isi, split_kind, child_index, dat) {
  candidate <- data.frame(
    start_isi = as.integer(start_isi), end_isi = as.integer(end_isi),
    final_label = as.character(label),
    candidate_layer = as.character(parent$candidate_layer),
    candidate_source = "deterministic_parent_support_subtraction",
    stringsAsFactors = FALSE
  )
  out <- stpd_multitrack_auto_redetected_interval(
    parent, candidate, train, "state", split_kind, child_index, dat
  )
  # This is geometry-only subtraction of invalid/QC support from an already
  # accepted parent.  It is deliberately not represented as detector output.
  out$interval_kind <- "fragment"
  out$parent_interval_id <- ""
  out$candidate_layer <- as.character(parent$candidate_layer)
  out$candidate_source <- "deterministic_parent_support_subtraction"
  out$activity_status <- "active_parent_support_after_boundary_subtraction"
  out$track_selection_status <-
    "selected_without_downstream_detector_reentry"
  out$provisional_gate_status <-
    "passed__parent_evidence_preserved_boundary_subtracted"
  out$source_track_policy_version <- "gate_b_deterministic_subtraction_v1"
  out
}

stpd_multitrack_auto_normalize_source <- function(
    source, ds, params, selected_trains, policy, run_id, params_hash) {
  intervals <- source$intervals
  appended <- list()
  for (train in selected_trains) {
    dat <- ds$trains[[train]]
    train_rows <- intervals$train == train
    gaps <- intervals[
      train_rows & intervals$semantic_track == "gap" &
        intervals$active_in_preview, , drop = FALSE
    ]
    min_isi <- stpd_multitrack_auto_num(
      (params$detector %||% list())$min_valid_isi_sec, 0.0009
    )
    hard_boundaries <- stpd_multitrack_auto_boundary_rows(
      dat, gaps, min_isi_sec = min_isi
    )
    event_hard_boundaries <- hard_boundaries[
      as.logical(hard_boundaries$hard_for_event), , drop = FALSE
    ]
    state_hard_boundaries <- hard_boundaries[
      as.logical(hard_boundaries$hard_for_state_direct_support), , drop = FALSE
    ]

    event_roots <- intervals[
      train_rows & intervals$semantic_track == "event" &
        intervals$interval_kind == "candidate" &
        intervals$selected_within_track, , drop = FALSE
    ]
    if (nrow(event_roots) > 0L) {
      geometry <- paste(event_roots$start_isi, event_roots$end_isi, sep = "-")
      for (key in unique(geometry)) {
        roots <- event_roots[geometry == key, , drop = FALSE]
        parent <- roots[order(roots$interval_id, method = "radix")[1L], , drop = FALSE]
        cuts <- event_hard_boundaries[
          event_hard_boundaries$start_isi <= parent$end_isi &
            event_hard_boundaries$end_isi >= parent$start_isi, , drop = FALSE
        ]
        if (nrow(cuts) == 0L) next
        # Event candidates must already respect canonical Pause/QC boundaries
        # upstream.  Re-running the detector here would make the final Event
        # differ from the immutable evidence used to resolve Pause ownership.
        # Fail closed and preserve the original ledger for diagnosis.
        stpd_multitrack_auto_abort(
          "event_crosses_upstream_hard_boundary",
          paste0(
            "Event candidate ", as.character(parent$candidate_id),
            " crosses an upstream hard boundary in train '", train,
            "'. Candidate generation must be repaired; downstream redetection ",
            "is forbidden."
          )
        )
      }
    }

    state_roots <- intervals[
      train_rows & intervals$semantic_track == "state" &
        intervals$interval_kind == "candidate" &
        intervals$selected_within_track, , drop = FALSE
    ]
    for (i in seq_len(nrow(state_roots))) {
      parent <- state_roots[i, , drop = FALSE]
      label <- stpd_multitrack_shadow_normalize_label(parent$label)
      cuts <- state_hard_boundaries
      cuts <- cuts[
        cuts$start_isi <= parent$end_isi &
          cuts$end_isi >= parent$start_isi, , drop = FALSE
      ]
      if (nrow(cuts) == 0L) next

      # A canonical Pause removes direct State support but does not revoke the
      # evidence that accepted the surrounding Broad-HF episode.  Requiring
      # each Pause-created child to pass the parent minimum-spike gate causes a
      # valid episode to disappear whenever its support is split into shorter
      # pieces.  Preserve the episode lineage and activate its existing direct-
      # support children by inheritance.  Invalid/artifact support remains a
      # strict boundary and continues through full detector redetection below.
      pause_only_state <- label %in% c(
        "high_frequency_spiking", "high_frequency_tonic", "tonic"
      ) && nrow(cuts) > 0L && all(as.character(cuts$boundary_kind) == "pause")
      if (pause_only_state) {
        parent_hit <- intervals$interval_id == parent$interval_id
        child_hit <- intervals$parent_interval_id == parent$interval_id &
          intervals$semantic_track == "state" &
          stpd_multitrack_shadow_normalize_label(intervals$label) == label &
          intervals$split_kind == "pause_boundary"
        intervals$active_in_preview[parent_hit] <- FALSE
        intervals$activity_status[parent_hit] <-
          "inactive_episode_parent_direct_support_represented_by_children"
        if (any(child_hit)) {
          intervals$active_in_preview[child_hit] <- TRUE
          intervals$interval_kind[child_hit] <-
            "state_inherited_support_fragment"
          intervals$activity_status[child_hit] <-
            "active_direct_support_inherited_from_accepted_parent_episode"
          intervals$track_selection_status[child_hit] <-
            "selected_parent_episode_pause_excluded_from_direct_support"
          intervals$candidate_source[child_hit] <-
            "accepted_parent_episode_support_inheritance"
          intervals$gate_evaluated[child_hit] <- TRUE
          intervals$provisional_gate_pass[child_hit] <- TRUE
          intervals$provisional_gate_status[child_hit] <-
            "passed__parent_episode_acceptance_inherited"
        }
        next
      }
      descendants <- intervals$interval_id == parent$interval_id |
        intervals$parent_interval_id == parent$interval_id
      intervals$active_in_preview[descendants] <- FALSE
      intervals$activity_status[descendants] <-
        "inactive_parent_replaced_by_deterministic_support_subtraction"
      residuals <- stpd_multitrack_compatibility_subtract_intervals(
        parent$start_isi, parent$end_isi, cuts
      )
      split_kind <- paste(
        sort(unique(as.character(cuts$boundary_kind)), method = "radix"),
        collapse = "+"
      )
      if (nrow(residuals) > 0L) {
        for (j in seq_len(nrow(residuals))) {
          appended[[length(appended) + 1L]] <-
            stpd_multitrack_auto_inherited_residual_interval(
              parent, train, label,
              residuals$start_isi[j], residuals$end_isi[j],
              split_kind, j, dat
            )
        }
      }
    }
  }
  intervals <- dplyr::bind_rows(
    intervals,
    if (length(appended) == 0L) stpd_multitrack_preview_empty_intervals()
    else dplyr::bind_rows(appended)
  )
  if (nrow(intervals) > 0L) {
    ord <- order(
      intervals$train,
      match(intervals$semantic_track, c("event", "state", "gap", "review")),
      intervals$start_isi, intervals$end_isi, intervals$interval_kind,
      intervals$interval_id, na.last = TRUE, method = "radix"
    )
    intervals <- intervals[ord, , drop = FALSE]
    rownames(intervals) <- NULL
  }
  stpd_multitrack_preview_assert_unique_active_tracks(intervals)
  source$intervals <- intervals
  per_isi <- lapply(selected_trains, function(train) {
    stpd_multitrack_preview_per_isi_rows(
      train, ds$trains[[train]], intervals, policy, run_id, params_hash
    )
  })
  source$per_isi <- dplyr::bind_rows(c(
    list(stpd_multitrack_preview_empty_per_isi()), per_isi
  ))
  source
}

stpd_multitrack_auto_source_preview <- function(
    ds, params, selected_trains, run_id, params_hash) {
  # This private Preview-shaped materialization reuses the strict Phase 2A
  # lineage and compatibility checks, but is independent of preview.enabled and
  # is never persisted as the public Preview product.
  policy <- list(
    enabled = TRUE,
    schema_version = stpd_multitrack_preview_schema_version(),
    authoritative = FALSE,
    parameter_path = "internal.multitrack_auto.source_materialization",
    policy_hash = stpd_multitrack_auto_policy_hash(params),
    policy_hash_algorithm = "SHA-256"
  )
  # Phase 1B deliberately records several cross-track hard-boundary conflicts
  # without mutating its historical Preview contract. Gate B repairs exactly
  # those typed conflicts in a private copy, then re-runs the original detector
  # on residual support. Unknown violations remain fail-closed.
  source_ds <- ds
  repairable <- c(
    "selected_pause_must_not_overlap_selected_canonical_burst",
    "tonic_hft_must_not_overlap_selected_pause"
  )
  for (train in selected_trains) {
    compatibility <- attr(
      source_ds$trains[[train]], "multitrack_compatibility_shadow", exact = TRUE
    )
    conflicts <- as.data.frame(
      (compatibility %||% list())$invariants %||% data.frame(),
      stringsAsFactors = FALSE
    )
    violation <- if (nrow(conflicts) > 0L &&
                     all(c("invariant", "severity") %in% names(conflicts))) {
      tolower(as.character(conflicts$severity)) == "violation"
    } else {
      rep(FALSE, nrow(conflicts))
    }
    unknown <- conflicts$invariant[violation &
      !as.character(conflicts$invariant) %in% repairable]
    if (length(unknown) > 0L) {
      stpd_multitrack_auto_abort(
        "phase1b_unrepairable_invariant",
        paste0(
          "Train '", train, "' contains unrecognized Phase 1B violation(s): ",
          paste(sort(unique(unknown), method = "radix"), collapse = ";"), "."
        )
      )
    }
    if (any(violation)) {
      compatibility$invariants <- conflicts[!violation, , drop = FALSE]
      attr(source_ds$trains[[train]], "multitrack_compatibility_shadow") <-
        compatibility
    }
  }
  source <- stpd_multitrack_preview_build(
    ds = source_ds,
    params = params,
    selected_trains = selected_trains,
    run_id = run_id,
    params_hash = params_hash,
    policy = policy,
    materialize_per_isi = FALSE
  )
  if (!inherits(source, "stpd_multitrack_public_preview") ||
      nrow(source$metadata) != 1L ||
      !identical(
        stpd_multitrack_auto_chr(source$metadata$materialization_status),
        "materialized"
      )) {
    stpd_multitrack_auto_abort(
      "source_materialization_failed",
      "The strict Phase 1A/1B source materialization did not complete."
    )
  }
  stpd_multitrack_auto_normalize_source(
    source, ds, params, selected_trains, policy, run_id, params_hash
  )
}

stpd_multitrack_auto_events <- function(intervals, policy_hash, run_id, params_hash) {
  evidence <- intervals[intervals$semantic_track == "event", , drop = FALSE]
  active <- evidence[evidence$active_in_preview, , drop = FALSE]
  if (nrow(active) == 0L) return(stpd_multitrack_auto_empty_events())
  geometry_key <- paste(
    active$train, active$start_isi, active$end_isi, sep = "\u001f"
  )
  geometry_keys <- sort(unique(geometry_key), method = "radix")
  rows <- lapply(geometry_keys, function(key) {
    active_hits <- which(geometry_key == key)
    primary_candidates <- active[active_hits, , drop = FALSE]
    primary_candidates <- primary_candidates[order(
      primary_candidates$interval_id, method = "radix"
    ), , drop = FALSE]
    row <- primary_candidates[1L, , drop = FALSE]
    same_geometry <- evidence$train == row$train &
      evidence$start_isi == row$start_isi & evidence$end_isi == row$end_isi
    same_geometry[is.na(same_geometry)] <- FALSE
    provenance <- evidence[same_geometry, , drop = FALSE]
    labels <- stpd_multitrack_shadow_normalize_label(provenance$label)
    modifier <- stpd_multitrack_auto_event_evidence_modifiers(labels)
    geometry_id <- paste(row$start_isi, row$end_isi, sep = "-")
    data.frame(
      schema_version = stpd_multitrack_auto_schema_version(),
      policy_hash = policy_hash, run_id = run_id, params_hash = params_hash,
      authoritative = FALSE, train = as.character(row$train),
      event_id = stpd_multitrack_auto_id(
        "event", run_id, row$train, geometry_id
      ),
      source_interval_id = as.character(row$interval_id),
      source_candidate_id = as.character(row$candidate_id),
      source_candidate_key = as.character(row$candidate_key),
      source_label = stpd_multitrack_shadow_normalize_label(row$label),
      event_family = modifier$family,
      extent_class = modifier$extent,
      frequency_class = modifier$frequency,
      provenance_source_interval_ids = stpd_multitrack_auto_join_values(
        provenance$interval_id
      ),
      provenance_source_candidate_ids = stpd_multitrack_auto_join_values(
        provenance$candidate_id
      ),
      provenance_source_candidate_keys = stpd_multitrack_auto_join_values(
        provenance$candidate_key
      ),
      provenance_source_labels = stpd_multitrack_auto_join_values(labels),
      provenance_candidate_layers = stpd_multitrack_auto_join_values(
        provenance$candidate_layer
      ),
      provenance_candidate_sources = stpd_multitrack_auto_join_values(
        provenance$candidate_source
      ),
      extent_evidence_labels = modifier$extent_labels,
      frequency_evidence_labels = modifier$frequency_labels,
      extent_conflict = modifier$extent_conflict,
      modifier_diagnostic = modifier$diagnostic,
      start_isi = as.integer(row$start_isi), end_isi = as.integer(row$end_isi),
      n_isi = as.integer(row$n_isi), n_spikes = as.integer(row$n_isi + 1L),
      start_time_sec = as.numeric(row$start_time_sec),
      end_time_sec = as.numeric(row$end_time_sec),
      candidate_layer = as.character(row$candidate_layer),
      candidate_source = as.character(row$candidate_source),
      stringsAsFactors = FALSE
    )
  })
  stpd_multitrack_auto_bind(stpd_multitrack_auto_empty_events(), rows)
}

stpd_multitrack_auto_state_parent_class <- function(source_label) {
  label <- stpd_multitrack_shadow_normalize_label(source_label)
  ifelse(
    label %in% c(
      "high_frequency_spiking", "high_frequency_tonic",
      "high_frequency_irregular_state"
    ),
    "high_frequency_spiking",
    label
  )
}

stpd_multitrack_auto_hf_subtype <- function(
    dat, fragments, min_isi_sec = 0.001) {
  unresolved <- function(status, n = 0L, duration = NA_real_,
                         frequency = NA_real_, regularity = NA_real_) {
    list(
      frequency_class = "high", regularity_class = "unresolved",
      subtype = "hf_unresolved", status = status,
      n_valid_isi = as.integer(n), effective_duration_sec = as.numeric(duration),
      frequency_hz = as.numeric(frequency), median_cv2 = as.numeric(regularity),
      rule = "state_frequency_rate_and_median_adjacent_cv2_v1"
    )
  }
  if (is.null(dat) || !is.data.frame(dat) || !("ISI_sec" %in% names(dat)) ||
      is.null(fragments) || nrow(fragments) == 0L) {
    return(unresolved("source_support_unavailable"))
  }

  isi <- suppressWarnings(as.numeric(dat$ISI_sec))
  values <- numeric()
  adjacent_cv2 <- numeric()
  fragments <- fragments[order(
    fragments$start_isi, fragments$end_isi, method = "radix"
  ), , drop = FALSE]
  for (i in seq_len(nrow(fragments))) {
    index <- seq.int(fragments$start_isi[i], fragments$end_isi[i])
    index <- index[index >= 1L & index <= length(isi)]
    part <- isi[index]
    valid <- is.finite(part) & part > 0 &
      !is_artifact_isi(part, min_isi_sec)
    part <- part[valid]
    values <- c(values, part)
    if (length(part) >= 2L) {
      denominator <- part[-length(part)] + part[-1L]
      pair <- 2 * abs(diff(part)) / denominator
      adjacent_cv2 <- c(
        adjacent_cv2,
        pair[is.finite(pair) & denominator > 0]
      )
    }
  }
  n_valid <- length(values)
  duration <- if (n_valid > 0L) sum(values) else NA_real_
  frequency <- if (is.finite(duration) && duration > 0) {
    n_valid / duration
  } else {
    NA_real_
  }
  regularity <- if (length(adjacent_cv2) > 0L) {
    stats::median(adjacent_cv2)
  } else {
    NA_real_
  }
  if (n_valid < 2L || !is.finite(duration) || duration < 0.001 ||
      !is.finite(regularity)) {
    return(unresolved(
      "insufficient_direct_support", n_valid, duration, frequency, regularity
    ))
  }
  if (regularity <= 0.5) {
    regularity_class <- "regular"
    subtype <- "high_frequency_tonic"
    status <- "classified"
  } else if (regularity >= 0.8) {
    regularity_class <- "irregular"
    subtype <- "high_frequency_irregular_state"
    status <- "classified"
  } else {
    regularity_class <- "unresolved"
    subtype <- "hf_unresolved"
    status <- "regularity_gray_zone"
  }
  list(
    frequency_class = "high", regularity_class = regularity_class,
    subtype = subtype, status = status, n_valid_isi = as.integer(n_valid),
    effective_duration_sec = as.numeric(duration),
    frequency_hz = as.numeric(frequency), median_cv2 = as.numeric(regularity),
    rule = "state_frequency_rate_and_median_adjacent_cv2_v1"
  )
}

stpd_multitrack_auto_states <- function(
    active, all_intervals, policy_hash, run_id, params_hash,
    ds = NULL, params = NULL) {
  source <- active[active$semantic_track == "state", , drop = FALSE]
  rows <- lapply(seq_len(nrow(source)), function(i) {
    row <- source[i, , drop = FALSE]
    source_label <- stpd_multitrack_shadow_normalize_label(row$label)
    state_class <- stpd_multitrack_auto_state_parent_class(source_label)
    is_hfs <- identical(state_class, "high_frequency_spiking")
    data.frame(
      schema_version = stpd_multitrack_auto_schema_version(),
      policy_hash = policy_hash, run_id = run_id, params_hash = params_hash,
      authoritative = FALSE, train = as.character(row$train),
      state_id = stpd_multitrack_auto_id(
        "state", run_id, row$train, row$interval_id
      ),
      source_interval_id = as.character(row$interval_id),
      source_candidate_id = as.character(row$candidate_id),
      source_candidate_key = as.character(row$candidate_key),
      source_label = source_label,
      state_class = state_class,
      state_family = if (is_hfs) "broad_high_frequency_state" else "tonic",
      state_frequency_class = if (is_hfs) "high" else "non_high",
      state_regularity_class = if (is_hfs) "unresolved" else "regular",
      state_subtype = if (is_hfs) "hf_unresolved" else "tonic",
      subtype_status = if (is_hfs) "pending" else "classified_by_tonic_detector",
      subtype_n_valid_isi = NA_integer_,
      subtype_effective_duration_sec = NA_real_,
      subtype_frequency_hz = NA_real_, subtype_median_cv2 = NA_real_,
      subtype_rule = if (is_hfs) {
        "state_frequency_rate_and_median_adjacent_cv2_v1"
      } else {
        "tonic_detector_support_v1"
      },
      interval_kind = as.character(row$interval_kind),
      parent_source_interval_id = as.character(row$parent_interval_id),
      split_kind = as.character(row$split_kind),
      state_episode_id = "", episode_source_interval_id = "",
      episode_start_isi = NA_integer_, episode_end_isi = NA_integer_,
      episode_n_isi = NA_integer_, support_fragment_index = NA_integer_,
      support_fragment_n = NA_integer_,
      episode_direct_support_isi_n = NA_integer_,
      episode_pause_isi_n = NA_integer_, support_acceptance_basis = "",
      start_isi = as.integer(row$start_isi), end_isi = as.integer(row$end_isi),
      n_isi = as.integer(row$n_isi), n_spikes = as.integer(row$n_isi + 1L),
      start_time_sec = as.numeric(row$start_time_sec),
      end_time_sec = as.numeric(row$end_time_sec),
      candidate_layer = as.character(row$candidate_layer),
      candidate_source = as.character(row$candidate_source),
      stringsAsFactors = FALSE
    )
  })
  out <- stpd_multitrack_auto_bind(stpd_multitrack_auto_empty_states(), rows)
  if (nrow(out) == 0L) return(out)

  hfs <- which(out$state_class == "high_frequency_spiking")
  if (length(hfs) == 0L) return(out)
  active_gaps <- all_intervals[
    all_intervals$semantic_track == "gap" & all_intervals$active_in_preview &
      as.logical(all_intervals$hard_for_state_direct_support %||% TRUE),
    , drop = FALSE
  ]
  for (i in hfs) {
    inherited <- identical(
      out$interval_kind[i], "state_inherited_support_fragment"
    ) && nzchar(out$parent_source_interval_id[i])
    source_id <- if (inherited) {
      out$parent_source_interval_id[i]
    } else {
      out$source_interval_id[i]
    }
    root_hit <- which(all_intervals$interval_id == source_id)
    root <- if (length(root_hit) == 1L) {
      all_intervals[root_hit, , drop = FALSE]
    } else {
      NULL
    }
    episode_start <- if (is.null(root)) out$start_isi[i] else root$start_isi
    episode_end <- if (is.null(root)) out$end_isi[i] else root$end_isi
    out$episode_source_interval_id[i] <- source_id
    out$state_episode_id[i] <- stpd_multitrack_auto_id(
      "state_episode", run_id, out$train[i], source_id
    )
    out$episode_start_isi[i] <- as.integer(episode_start)
    out$episode_end_isi[i] <- as.integer(episode_end)
    out$episode_n_isi[i] <- as.integer(episode_end - episode_start + 1L)
    out$support_acceptance_basis[i] <- if (inherited) {
      "inherited_from_accepted_parent_episode"
    } else {
      "direct_detector_acceptance"
    }
  }

  episode_groups <- split(hfs, out$state_episode_id[hfs])
  for (indices in episode_groups) {
    indices <- indices[order(
      out$start_isi[indices], out$end_isi[indices],
      out$state_id[indices], method = "radix"
    )]
    support_n <- sum(out$n_isi[indices])
    episode_start <- out$episode_start_isi[indices[1L]]
    episode_end <- out$episode_end_isi[indices[1L]]
    train <- out$train[indices[1L]]
    pause_n <- 0L
    if (nrow(active_gaps) > 0L) {
      gap_hit <- active_gaps$train == train &
        active_gaps$start_isi <= episode_end &
        active_gaps$end_isi >= episode_start &
        stpd_multitrack_shadow_normalize_label(active_gaps$label) == "pause"
      if (any(gap_hit)) {
        pause_n <- sum(pmax(
          0L,
          pmin(episode_end, active_gaps$end_isi[gap_hit]) -
            pmax(episode_start, active_gaps$start_isi[gap_hit]) + 1L
        ))
      }
    }
    out$support_fragment_index[indices] <- seq_along(indices)
    out$support_fragment_n[indices] <- length(indices)
    out$episode_direct_support_isi_n[indices] <- as.integer(support_n)
    out$episode_pause_isi_n[indices] <- as.integer(pause_n)
    train_dat <- (ds$trains %||% list())[[train]]
    min_isi_sec <- stpd_multitrack_auto_num(
      (params$detector %||% list())$min_valid_isi_sec, 0.001
    )
    subtype <- stpd_multitrack_auto_hf_subtype(
      train_dat, out[indices, , drop = FALSE], min_isi_sec = min_isi_sec
    )
    out$state_frequency_class[indices] <- subtype$frequency_class
    out$state_regularity_class[indices] <- subtype$regularity_class
    out$state_subtype[indices] <- subtype$subtype
    out$subtype_status[indices] <- subtype$status
    out$subtype_n_valid_isi[indices] <- subtype$n_valid_isi
    out$subtype_effective_duration_sec[indices] <-
      subtype$effective_duration_sec
    out$subtype_frequency_hz[indices] <- subtype$frequency_hz
    out$subtype_median_cv2[indices] <- subtype$median_cv2
    out$subtype_rule[indices] <- subtype$rule
  }
  out
}

stpd_multitrack_auto_gaps <- function(active, policy_hash, run_id, params_hash) {
  source <- active[active$semantic_track == "gap", , drop = FALSE]
  rows <- lapply(seq_len(nrow(source)), function(i) {
    row <- source[i, , drop = FALSE]
    label <- stpd_multitrack_shadow_normalize_label(row$label)
    if (!identical(label, "pause")) {
      stpd_multitrack_auto_abort(
        "unsupported_gap_label",
        paste0("Unsupported active Gap label: '", label, "'.")
      )
    }
    data.frame(
      schema_version = stpd_multitrack_auto_schema_version(),
      policy_hash = policy_hash, run_id = run_id, params_hash = params_hash,
      authoritative = FALSE, train = as.character(row$train),
      gap_id = stpd_multitrack_auto_id("gap", run_id, row$train, row$interval_id),
      source_interval_id = as.character(row$interval_id),
      source_candidate_id = as.character(row$candidate_id),
      source_candidate_key = as.character(row$candidate_key),
      source_label = label, gap_class = "pause",
      gap_semantics = stpd_multitrack_auto_chr(
        row$gap_semantics, "canonical_pause"
      ),
      hard_for_event = stpd_multitrack_auto_lgl(row$hard_for_event, TRUE),
      hard_for_state_direct_support = stpd_multitrack_auto_lgl(
        row$hard_for_state_direct_support, TRUE
      ),
      envelope_bridge_eligible = stpd_multitrack_auto_lgl(
        row$envelope_bridge_eligible, FALSE
      ),
      candidate_decision = stpd_multitrack_auto_chr(
        row$candidate_decision, "accepted"
      ),
      candidate_reason_code = stpd_multitrack_auto_chr(
        row$candidate_reason_code, "legacy_pause_candidate"
      ),
      start_isi = as.integer(row$start_isi), end_isi = as.integer(row$end_isi),
      n_isi = as.integer(row$n_isi),
      start_time_sec = as.numeric(row$start_time_sec),
      end_time_sec = as.numeric(row$end_time_sec),
      candidate_layer = as.character(row$candidate_layer),
      candidate_source = as.character(row$candidate_source),
      stringsAsFactors = FALSE
    )
  })
  stpd_multitrack_auto_bind(stpd_multitrack_auto_empty_gaps(), rows)
}

stpd_multitrack_auto_review_candidates <- function(
    intervals, policy_hash, run_id, params_hash) {
  source <- intervals[intervals$semantic_track == "review", , drop = FALSE]
  rows <- lapply(seq_len(nrow(source)), function(i) {
    row <- source[i, , drop = FALSE]
    label <- stpd_multitrack_shadow_normalize_label(row$label)
    if (!identical(label, "possible_burst")) {
      stpd_multitrack_auto_abort(
        "unsupported_review_label",
        paste0("Unsupported Review label: '", label, "'.")
      )
    }
    data.frame(
      schema_version = stpd_multitrack_auto_schema_version(),
      policy_hash = policy_hash, run_id = run_id, params_hash = params_hash,
      authoritative = FALSE, train = as.character(row$train),
      review_candidate_id = stpd_multitrack_auto_id(
        "review", run_id, row$train, row$interval_id
      ),
      source_interval_id = as.character(row$interval_id),
      source_candidate_id = as.character(row$candidate_id),
      source_candidate_key = as.character(row$candidate_key),
      review_label = label,
      target_track = as.character(row$review_target_track),
      target_label = as.character(row$review_target_label),
      promotion_required = as.logical(row$review_promotion_required),
      selected_within_track = as.logical(row$selected_within_track),
      active_in_review_track = as.logical(row$active_in_preview),
      review_policy_status = as.character(row$review_policy_status),
      start_isi = as.integer(row$start_isi), end_isi = as.integer(row$end_isi),
      n_isi = as.integer(row$n_isi),
      start_time_sec = as.numeric(row$start_time_sec),
      end_time_sec = as.numeric(row$end_time_sec),
      candidate_layer = as.character(row$candidate_layer),
      candidate_source = as.character(row$candidate_source),
      stringsAsFactors = FALSE
    )
  })
  stpd_multitrack_auto_bind(stpd_multitrack_auto_empty_review(), rows)
}

stpd_multitrack_auto_assert_disjoint <- function(a, b, label_a, label_b) {
  if (nrow(a) == 0L || nrow(b) == 0L) return(invisible(TRUE))
  for (train in intersect(unique(a$train), unique(b$train))) {
    aa <- a[a$train == train, , drop = FALSE]
    bb <- b[b$train == train, , drop = FALSE]
    for (i in seq_len(nrow(aa))) {
      for (j in seq_len(nrow(bb))) {
        if (stpd_multitrack_auto_interval_overlap(
          aa$start_isi[i], aa$end_isi[i], bb$start_isi[j], bb$end_isi[j]
        ) > 0L) {
          stpd_multitrack_auto_abort(
            "cross_track_overlap_forbidden",
            paste0(label_a, " and ", label_b, " share an ISI in train '",
                   train, "'.")
          )
        }
      }
    }
  }
  invisible(TRUE)
}

stpd_multitrack_auto_relationships <- function(
    events, states, policy_hash, run_id, params_hash) {
  rows <- list()
  if (nrow(events) == 0L || nrow(states) == 0L) {
    return(stpd_multitrack_auto_empty_relationships())
  }
  for (train in intersect(unique(events$train), unique(states$train))) {
    ee <- events[events$train == train, , drop = FALSE]
    ss <- states[states$train == train, , drop = FALSE]
    for (i in seq_len(nrow(ee))) {
      for (j in seq_len(nrow(ss))) {
        overlap_n <- stpd_multitrack_auto_interval_overlap(
          ee$start_isi[i], ee$end_isi[i], ss$start_isi[j], ss$end_isi[j]
        )
        if (overlap_n <= 0L) next
        overlap_start <- max(ee$start_isi[i], ss$start_isi[j])
        overlap_end <- min(ee$end_isi[i], ss$end_isi[j])
        relation <- if (ee$start_isi[i] >= ss$start_isi[j] &&
                        ee$end_isi[i] <= ss$end_isi[j]) {
          "event_contained_in_state"
        } else if (ss$start_isi[j] >= ee$start_isi[i] &&
                   ss$end_isi[j] <= ee$end_isi[i]) {
          "event_contains_state"
        } else if (ee$start_isi[i] < ss$start_isi[j]) {
          "event_crosses_state_start"
        } else {
          "event_crosses_state_end"
        }
        union_n <- ee$n_isi[i] + ss$n_isi[j] - overlap_n
        rows[[length(rows) + 1L]] <- data.frame(
          schema_version = stpd_multitrack_auto_schema_version(),
          policy_hash = policy_hash, run_id = run_id, params_hash = params_hash,
          authoritative = FALSE, train = train,
          relationship_id = stpd_multitrack_auto_id(
            "relation", run_id, train,
            paste(ee$event_id[i], ss$state_id[j], sep = "\u001f"),
            overlap_start, overlap_end
          ),
          event_id = ee$event_id[i], state_id = ss$state_id[j],
          relationship_type = relation,
          compatibility_rule = if (identical(
            ss$state_class[j], "high_frequency_spiking"
          )) {
            "hfs_burst_non_destructive_coexistence"
          } else {
            "burst_state_non_destructive_coexistence"
          },
          overlap_start_isi = as.integer(overlap_start),
          overlap_end_isi = as.integer(overlap_end),
          overlap_isi_n = as.integer(overlap_n),
          event_overlap_fraction = overlap_n / ee$n_isi[i],
          state_overlap_fraction = overlap_n / ss$n_isi[j],
          intersection_over_union = overlap_n / union_n,
          non_destructive = TRUE,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  stpd_multitrack_auto_bind(
    stpd_multitrack_auto_empty_relationships(), rows
  )
}

stpd_multitrack_auto_map_ids <- function(ids, mapping, context) {
  ids <- as.character(ids)
  out <- rep("", length(ids))
  present <- !is.na(ids) & nzchar(ids)
  if (any(present)) {
    out[present] <- unname(mapping[ids[present]])
    if (anyNA(out[present]) || any(!nzchar(out[present]))) {
      stpd_multitrack_auto_abort(
        "projection_foreign_key_failed",
        paste0("Could not resolve ", context, " source interval IDs.")
      )
    }
  }
  out
}

stpd_multitrack_auto_event_source_map <- function(events) {
  if (nrow(events) == 0L) return(character())
  source_ids <- unlist(lapply(
    events$provenance_source_interval_ids,
    stpd_multitrack_auto_split_ids
  ), use.names = FALSE)
  event_ids <- unlist(Map(
    function(ids, event_id) {
      rep(event_id, length(stpd_multitrack_auto_split_ids(ids)))
    },
    events$provenance_source_interval_ids, events$event_id
  ), use.names = FALSE)
  if (length(source_ids) != length(event_ids) || anyDuplicated(source_ids)) {
    stpd_multitrack_auto_abort(
      "event_provenance_foreign_key_ambiguous",
      "A source Event interval resolves to more than one canonical Event."
    )
  }
  stats::setNames(event_ids, source_ids)
}

stpd_multitrack_auto_per_isi <- function(
    source, events, states, gaps, review, policy_hash, run_id, params_hash) {
  if (nrow(source) == 0L) return(stpd_multitrack_auto_empty_per_isi())
  event_map <- stpd_multitrack_auto_event_source_map(events)
  state_map <- stats::setNames(states$state_id, states$source_interval_id)
  gap_map <- stats::setNames(gaps$gap_id, gaps$source_interval_id)
  active_review <- review[review$active_in_review_track, , drop = FALSE]
  review_map <- stats::setNames(
    active_review$review_candidate_id, active_review$source_interval_id
  )
  event_id <- stpd_multitrack_auto_map_ids(
    source$pattern_auto_event_preview_interval_id, event_map, "Event"
  )
  state_id <- stpd_multitrack_auto_map_ids(
    source$pattern_auto_state_preview_interval_id, state_map, "State"
  )
  gap_id <- stpd_multitrack_auto_map_ids(
    source$pattern_auto_gap_preview_interval_id, gap_map, "Gap"
  )
  review_id <- stpd_multitrack_auto_map_ids(
    source$pattern_auto_review_preview_interval_id, review_map, "Review"
  )
  event_hit <- match(event_id, events$event_id)
  state_hit <- match(state_id, states$state_id)
  gap_hit <- match(gap_id, gaps$gap_id)
  review_hit <- match(review_id, active_review$review_candidate_id)
  state_episode_id <- rep("", nrow(source))
  state_episode_class <- rep("", nrow(source))
  state_support_role <- rep("", nrow(source))
  hfs_states <- states[
    states$state_class == "high_frequency_spiking" &
      nzchar(states$state_episode_id), , drop = FALSE
  ]
  if (nrow(hfs_states) > 0L) {
    episode_rows <- hfs_states[!duplicated(hfs_states$state_episode_id), , drop = FALSE]
    for (i in seq_len(nrow(episode_rows))) {
      hit <- source$train == episode_rows$train[i] &
        source$isi_index >= episode_rows$episode_start_isi[i] &
        source$isi_index <= episode_rows$episode_end_isi[i]
      if (any(nzchar(state_episode_id[hit]))) {
        stpd_multitrack_auto_abort(
          "state_episode_overlap",
          "Broad-HF episode envelopes overlap within one train."
        )
      }
      state_episode_id[hit] <- episode_rows$state_episode_id[i]
      state_episode_class[hit] <- "broad_high_frequency_state"
    }
    hard_gap <- rep(FALSE, nrow(source))
    gap_known <- nzchar(gap_id) & !is.na(gap_hit)
    if (any(gap_known)) {
      hard_gap[gap_known] <- as.logical(
        gaps$hard_for_state_direct_support[gap_hit[gap_known]] %||% TRUE
      )
      hard_gap[is.na(hard_gap)] <- FALSE
    }
    direct <- nzchar(state_episode_id) & nzchar(state_id)
    pause <- nzchar(state_episode_id) & hard_gap
    state_support_role[direct] <- "direct_support"
    state_support_role[pause] <- "canonical_pause_gap"
    other <- nzchar(state_episode_id) & !direct & !pause
    state_support_role[other] <- "excluded_or_unresolved_support"
  }
  data.frame(
    schema_version = rep(stpd_multitrack_auto_schema_version(), nrow(source)),
    policy_hash = rep(policy_hash, nrow(source)),
    run_id = rep(run_id, nrow(source)), params_hash = rep(params_hash, nrow(source)),
    authoritative = rep(FALSE, nrow(source)), train = as.character(source$train),
    isi_index = as.integer(source$isi_index),
    timestamp_sec = as.numeric(source$timestamp_sec), ISI_sec = as.numeric(source$ISI_sec),
    event_id = event_id,
    event_family = ifelse(nzchar(event_id), events$event_family[event_hit], ""),
    event_extent_class = ifelse(
      nzchar(event_id), events$extent_class[event_hit], ""
    ),
    event_frequency_class = ifelse(
      nzchar(event_id), events$frequency_class[event_hit], ""
    ),
    state_id = state_id,
    state_class = ifelse(nzchar(state_id), states$state_class[state_hit], ""),
    state_family = ifelse(
      nzchar(state_id), states$state_family[state_hit], ""
    ),
    state_frequency_class = ifelse(
      nzchar(state_id), states$state_frequency_class[state_hit], ""
    ),
    state_regularity_class = ifelse(
      nzchar(state_id), states$state_regularity_class[state_hit], ""
    ),
    state_subtype = ifelse(
      nzchar(state_id), states$state_subtype[state_hit], ""
    ),
    gap_id = gap_id,
    gap_class = ifelse(nzchar(gap_id), gaps$gap_class[gap_hit], ""),
    state_episode_id = state_episode_id,
    state_episode_class = state_episode_class,
    state_support_role = state_support_role,
    review_candidate_id = review_id,
    review_label = ifelse(
      nzchar(review_id), active_review$review_label[review_hit], ""
    ),
    review_candidate_count = as.integer(source$review_candidate_count),
    stringsAsFactors = FALSE
  )
}

stpd_multitrack_auto_split_ids <- function(x) {
  text <- stpd_multitrack_auto_chr(x)
  if (!nzchar(text)) return(character())
  values <- trimws(unlist(strsplit(text, ";", fixed = TRUE), use.names = FALSE))
  values[!is.na(values) & nzchar(values)]
}

stpd_multitrack_auto_hfs_context <- function(
    states, events, evidence, policy_hash, run_id, params_hash) {
  hfs <- states[states$state_class == "high_frequency_spiking", , drop = FALSE]
  if (nrow(hfs) == 0L) return(stpd_multitrack_auto_empty_hfs_context())
  rows <- lapply(seq_len(nrow(hfs)), function(i) {
    state <- hfs[i, , drop = FALSE]
    evidence_source_ids <- c(
      as.character(state$source_interval_id),
      as.character(state$parent_source_interval_id)
    )
    evidence_source_ids <- evidence_source_ids[nzchar(evidence_source_ids)]
    hit <- which(evidence$hfs_interval_id %in% evidence_source_ids)
    if (length(hit) > 1L) {
      stpd_multitrack_auto_abort(
        "duplicate_hfs_context",
        paste0("HFS State '", state$state_id, "' has duplicate evidence rows.")
      )
    }
    row <- if (length(hit) == 1L) evidence[hit, , drop = FALSE] else NULL
    overlapping <- events$train == state$train &
      events$start_isi <= state$end_isi & events$end_isi >= state$start_isi
    overlapping[is.na(overlapping)] <- FALSE
    contributor_ids <- sort(
      unique(as.character(events$event_id[overlapping])), method = "radix"
    )
    contributor_events <- events[
      match(contributor_ids, events$event_id), , drop = FALSE
    ]
    contributor_stats <- stpd_multitrack_auto_hfs_union_stats(
      contributor_events, state
    )
    burst_rich <- if (is.null(row)) FALSE else {
      stpd_multitrack_auto_lgl(row$threshold_dominance_flag)
    }
    description <- if (burst_rich) {
      paste(
        "Burst-rich HFS evidence exceeds a provisional review threshold;",
        "the HFS State is retained and no destructive arbitration is applied."
      )
    } else {
      paste(
        "HFS State retained; burst-rich provisional review threshold was not met",
        "or no dominance evidence was available."
      )
    }
    data.frame(
      schema_version = stpd_multitrack_auto_schema_version(),
      policy_hash = policy_hash, run_id = run_id, params_hash = params_hash,
      authoritative = FALSE, train = state$train, state_id = state$state_id,
      hfs_episode_id = state$state_episode_id,
      source_hfs_interval_id = state$source_interval_id,
      start_isi = as.integer(state$start_isi), end_isi = as.integer(state$end_isi),
      n_isi = as.integer(state$n_isi),
      selected_burst_event_n = contributor_stats$event_n,
      selected_burst_group_n = contributor_stats$group_n,
      burst_covered_isi_n = contributor_stats$covered_n,
      burst_isi_coverage = contributor_stats$coverage,
      contributor_event_ids = paste(contributor_ids, collapse = ";"),
      burst_rich_review_flag = burst_rich,
      burst_rich_review_description = description,
      packet_like_review_flag = if (is.null(row)) FALSE else {
        stpd_multitrack_auto_lgl(row$packet_like_annotation)
      },
      packet_neighbor_review_flag = if (is.null(row)) FALSE else {
        stpd_multitrack_auto_lgl(row$packet_neighbor_annotation)
      },
      state_preserved = TRUE, destructive_action_applied = FALSE,
      stringsAsFactors = FALSE
    )
  })
  stpd_multitrack_auto_bind(stpd_multitrack_auto_empty_hfs_context(), rows)
}

stpd_multitrack_auto_invariant_row <- function(
    policy_hash, run_id, params_hash, check_name, message,
    status = "pass", severity = "information") {
  data.frame(
    schema_version = stpd_multitrack_auto_schema_version(),
    policy_hash = policy_hash, run_id = run_id, params_hash = params_hash,
    authoritative = FALSE,
    invariant_id = stpd_multitrack_auto_id(
      "invariant", run_id, "", check_name
    ),
    check_name = check_name, status = status, severity = severity,
    message = message, stringsAsFactors = FALSE
  )
}

stpd_multitrack_auto_manifest_filenames <- function() {
  c(
    metadata = "Multitrack_auto_candidate_metadata.csv",
    events = "Multitrack_auto_candidate_events.csv",
    states = "Multitrack_auto_candidate_states.csv",
    gaps = "Multitrack_auto_candidate_gaps.csv",
    review_candidates = "Multitrack_auto_candidate_review_candidates.csv",
    state_event_relationships =
      "Multitrack_auto_candidate_state_event_relationships.csv",
    per_isi = "Multitrack_auto_candidate_per_isi.csv",
    hfs_context = "Multitrack_auto_candidate_hfs_context.csv",
    invariants = "Multitrack_auto_candidate_invariants.csv"
  )
}

stpd_multitrack_auto_column_types <- function(x) {
  paste0(names(x), ":", vapply(x, typeof, character(1)), collapse = ";")
}

stpd_multitrack_auto_manifest <- function(tables, policy_hash, run_id, params_hash) {
  files <- stpd_multitrack_auto_manifest_filenames()
  authority <- if ("metadata" %in% names(tables) &&
                   nrow(tables$metadata) == 1L &&
                   "authoritative" %in% names(tables$metadata)) {
    isTRUE(tables$metadata$authoritative[1])
  } else {
    FALSE
  }
  data.frame(
    schema_version = rep(stpd_multitrack_auto_schema_version(), length(files)),
    policy_hash = rep(policy_hash, length(files)), run_id = rep(run_id, length(files)),
    params_hash = rep(params_hash, length(files)),
    authoritative = rep(authority, length(files)),
    table_name = names(files), file_name = unname(files),
    row_count = vapply(names(files), function(name) {
      as.integer(nrow(tables[[name]]))
    }, integer(1)),
    column_count = vapply(names(files), function(name) {
      as.integer(ncol(tables[[name]]))
    }, integer(1)),
    column_types = vapply(names(files), function(name) {
      stpd_multitrack_auto_column_types(tables[[name]])
    }, character(1)),
    table_sha256 = vapply(names(files), function(name) {
      stpd_multitrack_auto_hash(tables[[name]])
    }, character(1)),
    stringsAsFactors = FALSE
  )
}

stpd_multitrack_auto_metadata <- function(
    policy_hash, run_id, params_hash, selected_trains, params, ds, tables,
    product_sha256, materialization_status = "materialized",
    failure_code = "", failure_message = "",
    source_multitrack_evidence_available = TRUE) {
  collapse_versions <- function(x) {
    x <- sort(unique(as.character(x)), method = "radix")
    paste(x[!is.na(x) & nzchar(x)], collapse = ";")
  }
  shadow_versions <- unlist(lapply(selected_trains, function(train) {
    attr(tables$source_trains[[train]], "multitrack_shadow", exact = TRUE)$policy_version %||% ""
  }), use.names = FALSE)
  compatibility_versions <- unlist(lapply(selected_trains, function(train) {
    attr(
      tables$source_trains[[train]],
      "multitrack_compatibility_shadow", exact = TRUE
    )$policy_version %||% ""
  }), use.names = FALSE)
  recorded_input_hash <- stpd_multitrack_auto_chr(
    (ds$meta %||% list())$input_sha256
  )
  input_hash_valid <- grepl("^[0-9a-f]{64}$", recorded_input_hash)
  input_sha256 <- if (input_hash_valid) {
    recorded_input_hash
  } else {
    core_input <- lapply(selected_trains, function(train) {
      dat <- ds$trains[[train]]
      columns <- intersect(c("idx", "timestamp_sec", "ISI_sec"), names(dat))
      dat[, columns, drop = FALSE]
    })
    names(core_input) <- selected_trains
    stpd_multitrack_auto_hash(core_input)
  }
  threshold_table <- (params$event_grammar %||% list())$threshold_table %||%
    (ds$results %||% list())$threshold_table %||% data.frame()
  data.frame(
    schema_version = stpd_multitrack_auto_schema_version(),
    policy_hash = policy_hash, policy_hash_algorithm = "SHA-256",
    run_id = run_id, params_hash = params_hash, authoritative = FALSE,
    authority_scope = "none_preview",
    intended_target_scope = "automatic_prediction_record",
    promotion_gate = "gate_b", promotion_status = "pending",
    canonical_candidate_schema = TRUE,
    biological_ground_truth = FALSE, legacy_single_track_parallel = TRUE,
    materialization_status = materialization_status,
    failure_code = failure_code, failure_message = failure_message,
    source_multitrack_evidence_available =
      isTRUE(source_multitrack_evidence_available),
    label_blind = isTRUE((params$meta %||% list())$label_blind),
    label_blind_execution = isTRUE((params$meta %||% list())$label_blind),
    detection_mode = if (isTRUE((params$meta %||% list())$label_blind)) {
      "label_blind"
    } else {
      "manual_aware"
    },
    detector_performance_eligible = FALSE,
    detector_performance_block_reason = "gate_b_pending",
    event_pause_conflict_policy = paste0(
      "canonical_pause_excluded_from_direct_support__",
      "state_episode_parent_acceptance_inherited__",
      "downstream_detector_reentry_forbidden"
    ),
    per_isi_materialization = if (identical(
      materialization_status, "materialized"
    )) {
      "full_selected_train_scope"
    } else {
      "not_materialized_due_to_product_status"
    },
    per_isi_materialization_reason = if (identical(
      materialization_status, "materialized"
    )) {
      "interval_projection_with_parent_and_bidirectional_validation"
    } else {
      materialization_status
    },
    input_sha256 = input_sha256,
    input_sha256_source = if (input_hash_valid) {
      "dataset_meta.input_sha256"
    } else {
      "selected_train_core_columns"
    },
    threshold_table_sha256 = stpd_multitrack_auto_hash(threshold_table),
    threshold_table_sha256_source =
      "effective_params.event_grammar.threshold_table_or_result_fallback",
    selected_train_n = as.integer(length(selected_trains)),
    selected_trains = paste(selected_trains, collapse = ";"),
    source_shadow_policy_versions = collapse_versions(shadow_versions),
    source_compatibility_policy_versions = collapse_versions(compatibility_versions),
    canonical_typed_artifact = "Multitrack_auto_candidate.rds",
    events_n = as.integer(nrow(tables$events)),
    states_n = as.integer(nrow(tables$states)),
    gaps_n = as.integer(nrow(tables$gaps)),
    review_candidates_n = as.integer(nrow(tables$review_candidates)),
    state_event_relationships_n = as.integer(nrow(tables$state_event_relationships)),
    per_isi_n = as.integer(nrow(tables$per_isi)),
    hfs_context_n = as.integer(nrow(tables$hfs_context)),
    invariants_n = as.integer(nrow(tables$invariants)),
    product_sha256 = product_sha256,
    stringsAsFactors = FALSE
  )
}

stpd_multitrack_auto_source_evidence_issue <- function(ds, selected_trains) {
  unavailable <- vapply(selected_trains, function(train) {
    dat <- ds$trains[[train]]
    shadow <- attr(dat, "multitrack_shadow", exact = TRUE)
    compatibility <- attr(
      dat, "multitrack_compatibility_shadow", exact = TRUE
    )
    !inherits(shadow, "stpd_multitrack_shadow") ||
      !inherits(compatibility, "stpd_multitrack_compatibility_shadow")
  }, logical(1))
  missing <- selected_trains[unavailable]
  if (length(missing) == 0L) return("")
  paste0(
    "The selected detector pipeline did not emit intact Phase 1A/1B evidence ",
    "for train(s): ", paste(missing, collapse = ";"), "."
  )
}

stpd_multitrack_auto_failure_product <- function(
    ds, params, selected_trains, run_id, params_hash, policy_hash,
    materialization_status, failure_code, failure_message,
    source_multitrack_evidence_available) {
  payload <- list(
    events = stpd_multitrack_auto_empty_events(),
    states = stpd_multitrack_auto_empty_states(),
    gaps = stpd_multitrack_auto_empty_gaps(),
    review_candidates = stpd_multitrack_auto_empty_review(),
    state_event_relationships = stpd_multitrack_auto_empty_relationships(),
    per_isi = stpd_multitrack_auto_empty_per_isi(),
    hfs_context = stpd_multitrack_auto_empty_hfs_context(),
    invariants = stpd_multitrack_auto_bind(
      stpd_multitrack_auto_empty_invariants(),
      list(stpd_multitrack_auto_invariant_row(
        policy_hash, run_id, params_hash,
        check_name = failure_code,
        message = failure_message,
        status = materialization_status,
        severity = if (identical(materialization_status, "not_available")) {
          "warning"
        } else {
          "error"
        }
      ))
    )
  )
  product_sha256 <- stpd_multitrack_auto_hash(payload)
  metadata_tables <- c(payload, list(
    source_trains = ds$trains[selected_trains]
  ))
  metadata <- stpd_multitrack_auto_metadata(
    policy_hash, run_id, params_hash, selected_trains, params, ds,
    metadata_tables, product_sha256,
    materialization_status = materialization_status,
    failure_code = failure_code,
    failure_message = failure_message,
    source_multitrack_evidence_available =
      source_multitrack_evidence_available
  )
  tables <- c(list(metadata = metadata), payload)
  manifest <- stpd_multitrack_auto_manifest(
    tables, policy_hash, run_id, params_hash
  )
  product <- structure(
    c(tables, list(manifest = manifest)),
    class = c("stpd_multitrack_auto_product", "list")
  )
  stpd_multitrack_auto_validate(
    product, parent = ds, rematerialize_parent = FALSE
  )
}

stpd_multitrack_auto_build <- function(
    ds, params, selected_trains = NULL, run_id = NULL, params_hash = NULL) {
  run_identity <- stpd_multitrack_preview_run_identity(
    ds, run_id = run_id, params_hash = params_hash
  )
  run_id <- run_identity$run_id
  params_hash <- run_identity$params_hash
  if (!nzchar(run_id) || !grepl("^[0-9a-f]{64}$", params_hash)) {
    stpd_multitrack_auto_abort(
      "run_identity_invalid",
      "Canonical automatic multi-track materialization requires run_id and SHA-256 params_hash."
    )
  }
  train_names <- names(ds$trains %||% list()) %||% character()
  selected_trains <- sort(unique(as.character(
    selected_trains %||% train_names
  )), method = "radix")
  selected_trains <- selected_trains[
    !is.na(selected_trains) & nzchar(selected_trains)
  ]
  if (length(selected_trains) == 0L ||
      !all(selected_trains %in% train_names)) {
    stpd_multitrack_auto_abort(
      "selected_train_scope_invalid",
      "Canonical automatic multi-track scope contains no valid selected trains."
    )
  }
  policy_hash <- stpd_multitrack_auto_policy_hash(params)
  evidence_issue <- stpd_multitrack_auto_source_evidence_issue(
    ds, selected_trains
  )
  if (nzchar(evidence_issue)) {
    return(stpd_multitrack_auto_failure_product(
      ds, params, selected_trains, run_id, params_hash, policy_hash,
      materialization_status = "not_available",
      failure_code = "source_multitrack_evidence_unavailable",
      failure_message = evidence_issue,
      source_multitrack_evidence_available = FALSE
    ))
  }
  candidate <- tryCatch({
    source <- stpd_multitrack_auto_source_preview(
      ds, params, selected_trains, run_id, params_hash
    )
    intervals <- source$intervals
  active <- intervals[intervals$active_in_preview, , drop = FALSE]
  events <- stpd_multitrack_auto_events(intervals, policy_hash, run_id, params_hash)
  states <- stpd_multitrack_auto_states(
    active, intervals, policy_hash, run_id, params_hash,
    ds = ds, params = params
  )
  gaps <- stpd_multitrack_auto_gaps(active, policy_hash, run_id, params_hash)
  review <- stpd_multitrack_auto_review_candidates(
    intervals, policy_hash, run_id, params_hash
  )
  stpd_multitrack_auto_assert_disjoint(states, gaps, "State", "Pause Gap")
  stpd_multitrack_auto_assert_disjoint(events, gaps, "Event", "Pause Gap")
  relationships <- stpd_multitrack_auto_relationships(
    events, states, policy_hash, run_id, params_hash
  )
  per_isi <- stpd_multitrack_auto_per_isi(
    source$per_isi, events, states, gaps, review,
    policy_hash, run_id, params_hash
  )
  hfs_context <- stpd_multitrack_auto_hfs_context(
    states, events, source$hfs_dominance_evidence,
    policy_hash, run_id, params_hash
  )
  invariant_messages <- c(
    canonical_event_identity =
      paste(
        "Each selected physical burst geometry is represented by one canonical",
        "Event row; same-geometry subtype evidence is merged into orthogonal",
        "extent and frequency modifiers without duplicate counting."
      ),
    same_track_non_overlap =
      "Canonical Event, State, and Gap intervals do not overlap within their own train and track.",
    pause_disjoint_from_state_and_event =
      paste(
        "Canonical Pause is excluded from direct State support while an",
        "accepted State episode envelope and its parent evidence are",
        "preserved. Product materialization never re-enters a detector."
      ),
    burst_state_non_destructive_coexistence =
      "A canonical Burst may overlap any State through an explicit non-destructive State-Event relationship.",
    hfs_dominance_review_only =
      "Burst-rich and packet evidence is review context only and never deletes an HFS State.",
    per_isi_materialization_complete = paste(
      "The typed per-ISI table covers every row of every selected train and",
      "is validated bidirectionally against the canonical interval tables."
    ),
    legacy_single_track_parallel =
      "The canonical product is additive and leaves legacy pattern_auto and events unchanged."
  )
  invariant_rows <- lapply(names(invariant_messages), function(name) {
    stpd_multitrack_auto_invariant_row(
      policy_hash, run_id, params_hash, name, unname(invariant_messages[name])
    )
  })
  invariants <- stpd_multitrack_auto_bind(
    stpd_multitrack_auto_empty_invariants(), invariant_rows
  )
  payload <- list(
    events = events, states = states, gaps = gaps,
    review_candidates = review,
    state_event_relationships = relationships,
    per_isi = per_isi, hfs_context = hfs_context, invariants = invariants
  )
  product_sha256 <- stpd_multitrack_auto_hash(payload)
  metadata_tables <- c(payload, list(
    source_trains = ds$trains[selected_trains]
  ))
  metadata <- stpd_multitrack_auto_metadata(
    policy_hash, run_id, params_hash, selected_trains, params, ds,
    metadata_tables, product_sha256
  )
  tables <- c(list(metadata = metadata), payload)
  manifest <- stpd_multitrack_auto_manifest(
    tables, policy_hash, run_id, params_hash
  )
  product <- structure(
    c(tables, list(manifest = manifest)),
    class = c("stpd_multitrack_auto_product", "list")
  )
    stpd_multitrack_auto_validate(
      product, parent = ds, rematerialize_parent = FALSE
    )
  }, error = function(e) e)
  if (inherits(candidate, "condition")) {
    code <- stpd_multitrack_auto_chr(
      candidate$code, "auto_candidate_materialization_error"
    )
    return(stpd_multitrack_auto_failure_product(
      ds, params, selected_trains, run_id, params_hash, policy_hash,
      materialization_status = "failed_closed",
      failure_code = code,
      failure_message = conditionMessage(candidate),
      source_multitrack_evidence_available = TRUE
    ))
  }
  candidate
}

stpd_multitrack_auto_assert_same_track_non_overlap <- function(table, id_column) {
  if (nrow(table) <= 1L) return(invisible(TRUE))
  groups <- split(seq_len(nrow(table)), table$train)
  for (indices in groups) {
    part <- table[indices, , drop = FALSE]
    part <- part[order(
      part$start_isi, part$end_isi, part[[id_column]], method = "radix"
    ), , drop = FALSE]
    if (nrow(part) > 1L && any(
      part$start_isi[-1L] <= part$end_isi[-nrow(part)]
    )) {
      stpd_multitrack_auto_abort(
        "same_track_overlap",
        paste0("Canonical ", id_column, " intervals overlap within a train.")
      )
    }
  }
  invisible(TRUE)
}

stpd_multitrack_auto_metadata_types <- function() {
  c(
    schema_version = "character", policy_hash = "character",
    policy_hash_algorithm = "character", run_id = "character",
    params_hash = "character", authoritative = "logical",
    authority_scope = "character", intended_target_scope = "character",
    promotion_gate = "character", promotion_status = "character",
    canonical_candidate_schema = "logical",
    biological_ground_truth = "logical",
    legacy_single_track_parallel = "logical",
    materialization_status = "character", failure_code = "character",
    failure_message = "character",
    source_multitrack_evidence_available = "logical",
    label_blind = "logical", label_blind_execution = "logical",
    detection_mode = "character",
    detector_performance_eligible = "logical",
    detector_performance_block_reason = "character",
    event_pause_conflict_policy = "character",
    per_isi_materialization = "character",
    per_isi_materialization_reason = "character",
    input_sha256 = "character", input_sha256_source = "character",
    threshold_table_sha256 = "character",
    threshold_table_sha256_source = "character",
    selected_train_n = "integer", selected_trains = "character",
    source_shadow_policy_versions = "character",
    source_compatibility_policy_versions = "character",
    canonical_typed_artifact = "character", events_n = "integer",
    states_n = "integer", gaps_n = "integer",
    review_candidates_n = "integer",
    state_event_relationships_n = "integer", per_isi_n = "integer",
    hfs_context_n = "integer", invariants_n = "integer",
    product_sha256 = "character"
  )
}

stpd_multitrack_auto_manifest_types <- function() {
  c(
    schema_version = "character", policy_hash = "character",
    run_id = "character", params_hash = "character",
    authoritative = "logical", table_name = "character",
    file_name = "character", row_count = "integer",
    column_count = "integer", column_types = "character",
    table_sha256 = "character"
  )
}

stpd_multitrack_auto_fixed_schemas <- function() {
  list(
    metadata = stpd_multitrack_auto_metadata_types(),
    events = vapply(stpd_multitrack_auto_empty_events(), typeof, character(1)),
    states = vapply(stpd_multitrack_auto_empty_states(), typeof, character(1)),
    gaps = vapply(stpd_multitrack_auto_empty_gaps(), typeof, character(1)),
    review_candidates = vapply(
      stpd_multitrack_auto_empty_review(), typeof, character(1)
    ),
    state_event_relationships = vapply(
      stpd_multitrack_auto_empty_relationships(), typeof, character(1)
    ),
    per_isi = vapply(
      stpd_multitrack_auto_empty_per_isi(), typeof, character(1)
    ),
    hfs_context = vapply(
      stpd_multitrack_auto_empty_hfs_context(), typeof, character(1)
    ),
    invariants = vapply(
      stpd_multitrack_auto_empty_invariants(), typeof, character(1)
    ),
    manifest = stpd_multitrack_auto_manifest_types()
  )
}

stpd_multitrack_auto_assert_fixed_schemas <- function(product) {
  schemas <- stpd_multitrack_auto_fixed_schemas()
  for (name in names(schemas)) {
    actual <- vapply(product[[name]], typeof, character(1))
    if (!identical(names(actual), names(schemas[[name]])) ||
        !identical(unname(actual), unname(schemas[[name]]))) {
      stpd_multitrack_auto_abort(
        "fixed_table_schema_invalid",
        paste0("Automatic candidate table '", name,
               "' does not match its fixed columns and types.")
      )
    }
  }
  invisible(TRUE)
}

stpd_multitrack_auto_assert_interval_geometry <- function(
    table, id_column, table_name, require_n_spikes = FALSE) {
  if (nrow(table) == 0L) return(invisible(TRUE))
  id <- as.character(table[[id_column]])
  start <- as.integer(table$start_isi)
  end <- as.integer(table$end_isi)
  n_isi <- as.integer(table$n_isi)
  start_time <- as.numeric(table$start_time_sec)
  end_time <- as.numeric(table$end_time_sec)
  okay <- !is.na(id) & nzchar(id) & !is.na(table$train) &
    nzchar(as.character(table$train)) & is.finite(start) & is.finite(end) &
    start >= 2L & end >= start & n_isi == end - start + 1L &
    is.finite(start_time) & is.finite(end_time) & end_time >= start_time
  if (isTRUE(require_n_spikes)) {
    okay <- okay & as.integer(table$n_spikes) == n_isi + 1L
  }
  okay[is.na(okay)] <- FALSE
  if (!all(okay)) {
    stpd_multitrack_auto_abort(
      "interval_geometry_invalid",
      paste0("Automatic candidate ", table_name,
             " contains invalid interval geometry or counts.")
    )
  }
  invisible(TRUE)
}

stpd_multitrack_auto_expected_event_modifiers <- function(provenance_labels) {
  rows <- lapply(as.character(provenance_labels), function(value) {
    labels <- stpd_multitrack_auto_split_ids(value)
    modifier <- stpd_multitrack_auto_event_evidence_modifiers(labels)
    data.frame(
      event_family = modifier$family,
      extent_class = modifier$extent,
      frequency_class = modifier$frequency,
      extent_evidence_labels = modifier$extent_labels,
      frequency_evidence_labels = modifier$frequency_labels,
      extent_conflict = modifier$extent_conflict,
      modifier_diagnostic = modifier$diagnostic,
      stringsAsFactors = FALSE
    )
  })
  dplyr::bind_rows(rows)
}

stpd_multitrack_auto_expected_relationship_type <- function(event, state) {
  if (event$start_isi >= state$start_isi && event$end_isi <= state$end_isi) {
    "event_contained_in_state"
  } else if (state$start_isi >= event$start_isi &&
             state$end_isi <= event$end_isi) {
    "event_contains_state"
  } else if (event$start_isi < state$start_isi) {
    "event_crosses_state_start"
  } else {
    "event_crosses_state_end"
  }
}

stpd_multitrack_auto_assert_relationship_geometry <- function(product) {
  relationships <- product$state_event_relationships
  if (nrow(relationships) == 0L) return(invisible(TRUE))
  tolerance <- 1e-12
  for (i in seq_len(nrow(relationships))) {
    relation <- relationships[i, , drop = FALSE]
    event_hit <- match(relation$event_id, product$events$event_id)
    state_hit <- match(relation$state_id, product$states$state_id)
    if (is.na(event_hit) || is.na(state_hit)) {
      stpd_multitrack_auto_abort(
        "state_event_relationship_invalid",
        "A State-Event relationship has an unresolved endpoint."
      )
    }
    event <- product$events[event_hit, , drop = FALSE]
    state <- product$states[state_hit, , drop = FALSE]
    overlap_start <- max(event$start_isi, state$start_isi)
    overlap_end <- min(event$end_isi, state$end_isi)
    overlap_n <- overlap_end - overlap_start + 1L
    union_n <- event$n_isi + state$n_isi - overlap_n
    expected_type <- stpd_multitrack_auto_expected_relationship_type(
      event, state
    )
    numeric_ok <- is.finite(overlap_n) && overlap_n > 0L && union_n > 0L &&
      abs(relation$event_overlap_fraction - overlap_n / event$n_isi) <= tolerance &&
      abs(relation$state_overlap_fraction - overlap_n / state$n_isi) <= tolerance &&
      abs(relation$intersection_over_union - overlap_n / union_n) <= tolerance
    expected_rule <- if (identical(
      as.character(state$state_class), "high_frequency_spiking"
    )) {
      "hfs_burst_non_destructive_coexistence"
    } else {
      "burst_state_non_destructive_coexistence"
    }
    fixed_ok <- identical(as.character(relation$train), as.character(event$train)) &&
      identical(as.character(relation$train), as.character(state$train)) &&
      identical(as.integer(relation$overlap_start_isi), as.integer(overlap_start)) &&
      identical(as.integer(relation$overlap_end_isi), as.integer(overlap_end)) &&
      identical(as.integer(relation$overlap_isi_n), as.integer(overlap_n)) &&
      identical(as.character(relation$relationship_type), expected_type) &&
      identical(as.character(relation$compatibility_rule), expected_rule) &&
      isTRUE(relation$non_destructive)
    if (!isTRUE(numeric_ok) || !isTRUE(fixed_ok)) {
      stpd_multitrack_auto_abort(
        "state_event_relationship_geometry_invalid",
        "A State-Event relationship disagrees with endpoint geometry or fractions."
      )
    }
  }
  invisible(TRUE)
}

stpd_multitrack_auto_hfs_union_stats <- function(events, state) {
  if (nrow(events) == 0L) {
    return(list(event_n = 0L, group_n = 0L, covered_n = 0L, coverage = 0))
  }
  starts <- pmax(as.integer(state$start_isi), as.integer(events$start_isi))
  ends <- pmin(as.integer(state$end_isi), as.integer(events$end_isi))
  keep <- ends >= starts
  starts <- starts[keep]
  ends <- ends[keep]
  if (length(starts) == 0L) {
    return(list(event_n = 0L, group_n = 0L, covered_n = 0L, coverage = 0))
  }
  order_index <- order(starts, ends, method = "radix")
  starts <- starts[order_index]
  ends <- ends[order_index]
  group_start <- starts[1]
  group_end <- ends[1]
  group_n <- 0L
  covered_n <- 0L
  if (length(starts) > 1L) {
    for (j in 2:length(starts)) {
      if (starts[j] <= group_end + 1L) {
        group_end <- max(group_end, ends[j])
      } else {
        group_n <- group_n + 1L
        covered_n <- covered_n + group_end - group_start + 1L
        group_start <- starts[j]
        group_end <- ends[j]
      }
    }
  }
  group_n <- group_n + 1L
  covered_n <- covered_n + group_end - group_start + 1L
  list(
    event_n = as.integer(length(starts)), group_n = as.integer(group_n),
    covered_n = as.integer(covered_n),
    coverage = covered_n / as.integer(state$n_isi)
  )
}

stpd_multitrack_auto_assert_hfs_context_closure <- function(product) {
  context <- product$hfs_context
  hfs_state_ids <- product$states$state_id[
    product$states$state_class == "high_frequency_spiking"
  ]
  if (anyDuplicated(context$state_id) ||
      anyDuplicated(context$source_hfs_interval_id) ||
      !setequal(as.character(context$state_id), as.character(hfs_state_ids))) {
    stpd_multitrack_auto_abort(
      "hfs_context_closure_invalid",
      "HFS context must contain exactly one row for every canonical HFS State."
    )
  }
  if (nrow(context) == 0L) return(invisible(TRUE))
  tolerance <- 1e-12
  for (i in seq_len(nrow(context))) {
    row <- context[i, , drop = FALSE]
    state_hit <- match(row$state_id, product$states$state_id)
    if (is.na(state_hit)) {
      stpd_multitrack_auto_abort(
        "hfs_context_invalid", "HFS context has an unresolved State endpoint."
      )
    }
    state <- product$states[state_hit, , drop = FALSE]
    contributor_ids <- stpd_multitrack_auto_split_ids(
      row$contributor_event_ids
    )
    if (anyDuplicated(contributor_ids)) {
      stpd_multitrack_auto_abort(
        "hfs_context_invalid", "HFS context repeats a contributor Event ID."
      )
    }
    event_hits <- match(contributor_ids, product$events$event_id)
    if (anyNA(event_hits)) {
      stpd_multitrack_auto_abort(
        "hfs_context_invalid", "HFS context has an unresolved Event contributor."
      )
    }
    contributors <- product$events[event_hits, , drop = FALSE]
    relation_ids <- product$state_event_relationships$event_id[
      product$state_event_relationships$state_id == row$state_id
    ]
    stats <- stpd_multitrack_auto_hfs_union_stats(contributors, state)
    fixed_ok <- identical(as.character(state$state_class), "high_frequency_spiking") &&
      identical(as.character(row$train), as.character(state$train)) &&
      identical(
        as.character(row$hfs_episode_id),
        as.character(state$state_episode_id)
      ) &&
      identical(
        as.character(row$source_hfs_interval_id),
        as.character(state$source_interval_id)
      ) && identical(as.integer(row$start_isi), as.integer(state$start_isi)) &&
      identical(as.integer(row$end_isi), as.integer(state$end_isi)) &&
      identical(as.integer(row$n_isi), as.integer(state$n_isi)) &&
      setequal(contributor_ids, relation_ids) &&
      identical(as.integer(row$selected_burst_event_n), stats$event_n) &&
      identical(as.integer(row$selected_burst_group_n), stats$group_n) &&
      identical(as.integer(row$burst_covered_isi_n), stats$covered_n) &&
      isTRUE(row$state_preserved) && !isTRUE(row$destructive_action_applied) &&
      !is.na(row$burst_rich_review_flag) &&
      !is.na(row$packet_like_review_flag) &&
      !is.na(row$packet_neighbor_review_flag)
    numeric_ok <- is.finite(row$burst_isi_coverage) &&
      abs(row$burst_isi_coverage - stats$coverage) <= tolerance
    if (!isTRUE(fixed_ok) || !isTRUE(numeric_ok)) {
      stpd_multitrack_auto_abort(
        "hfs_context_closure_invalid",
        "HFS context counts, contributors, or geometry do not close."
      )
    }
  }
  invisible(TRUE)
}

stpd_multitrack_auto_assert_state_episode_closure <- function(product) {
  states <- product$states
  per_isi <- product$per_isi
  hfs <- states[states$state_class == "high_frequency_spiking", , drop = FALSE]
  other <- states[states$state_class != "high_frequency_spiking", , drop = FALSE]
  episode_columns <- c(
    "state_episode_id", "episode_source_interval_id",
    "support_acceptance_basis"
  )
  if (nrow(other) > 0L && any(vapply(episode_columns, function(name) {
    any(nzchar(as.character(other[[name]])))
  }, logical(1)))) {
    stpd_multitrack_auto_abort(
      "state_episode_semantics_invalid",
      "Only Broad-HF direct support may carry an HFS episode identity."
    )
  }
  if (nrow(hfs) == 0L) {
    if (any(nzchar(per_isi$state_episode_id)) ||
        any(nzchar(per_isi$state_episode_class)) ||
        any(nzchar(per_isi$state_support_role))) {
      stpd_multitrack_auto_abort(
        "state_episode_projection_invalid",
        "Per-ISI episode fields are non-empty without a Broad-HF State."
      )
    }
    return(invisible(TRUE))
  }
  required <- nzchar(hfs$state_episode_id) &
    nzchar(hfs$episode_source_interval_id) &
    hfs$episode_start_isi >= 2L &
    hfs$episode_end_isi >= hfs$episode_start_isi &
    hfs$episode_n_isi == hfs$episode_end_isi - hfs$episode_start_isi + 1L &
    hfs$support_fragment_index >= 1L & hfs$support_fragment_n >= 1L &
    hfs$episode_direct_support_isi_n >= hfs$n_isi &
    hfs$episode_pause_isi_n >= 0L &
    hfs$support_acceptance_basis %in% c(
      "direct_detector_acceptance",
      "inherited_from_accepted_parent_episode"
    )
  required[is.na(required)] <- FALSE
  if (!all(required)) {
    stpd_multitrack_auto_abort(
      "state_episode_semantics_invalid",
      "Broad-HF State rows contain incomplete episode/support semantics."
    )
  }

  groups <- split(seq_len(nrow(hfs)), hfs$state_episode_id)
  for (indices in groups) {
    part <- hfs[indices, , drop = FALSE]
    part <- part[order(
      part$start_isi, part$end_isi, part$state_id, method = "radix"
    ), , drop = FALSE]
    expected_id <- stpd_multitrack_auto_id(
      "state_episode", product$metadata$run_id, part$train[1L],
      part$episode_source_interval_id[1L]
    )
    shared_columns <- c(
      "train", "state_episode_id", "episode_source_interval_id",
      "episode_start_isi", "episode_end_isi", "episode_n_isi",
      "support_fragment_n", "episode_direct_support_isi_n",
      "episode_pause_isi_n", "state_family", "state_frequency_class",
      "state_regularity_class", "state_subtype", "subtype_status",
      "subtype_n_valid_isi", "subtype_effective_duration_sec",
      "subtype_frequency_hz", "subtype_median_cv2", "subtype_rule"
    )
    shared <- all(vapply(shared_columns, function(name) {
      length(unique(part[[name]])) == 1L
    }, logical(1)))
    support_n <- sum(part$n_isi)
    episode_n <- part$episode_n_isi[1L]
    pause_n <- part$episode_pause_isi_n[1L]
    inherited <- part$support_acceptance_basis ==
      "inherited_from_accepted_parent_episode"
    lineage_ok <- all(
      (!inherited & part$episode_source_interval_id == part$source_interval_id) |
        (inherited &
           part$interval_kind == "state_inherited_support_fragment" &
           part$episode_source_interval_id == part$parent_source_interval_id)
    )
    if (!shared || !identical(part$state_episode_id[1L], expected_id) ||
        !identical(as.integer(part$support_fragment_index),
                   seq_len(nrow(part))) ||
        !all(part$support_fragment_n == nrow(part)) ||
        !all(part$episode_direct_support_isi_n == support_n) ||
        support_n + pause_n != episode_n || !lineage_ok) {
      stpd_multitrack_auto_abort(
        "state_episode_closure_invalid",
        "Broad-HF episode geometry, support fragments, Pause exclusion, or lineage does not close."
      )
    }

    inside <- per_isi$train == part$train[1L] &
      per_isi$isi_index >= part$episode_start_isi[1L] &
      per_isi$isi_index <= part$episode_end_isi[1L]
    assigned <- per_isi$state_episode_id == part$state_episode_id[1L]
    assigned[is.na(assigned)] <- FALSE
    direct <- assigned & nzchar(per_isi$state_id)
    pause <- assigned & nzchar(per_isi$gap_id)
    if (!identical(which(assigned), which(inside)) ||
        !all(per_isi$state_episode_class[inside] ==
               "broad_high_frequency_state") ||
        !all(per_isi$state_support_role[direct] == "direct_support") ||
        !all(per_isi$state_family[direct] == part$state_family[1L]) ||
        !all(per_isi$state_frequency_class[direct] ==
               part$state_frequency_class[1L]) ||
        !all(per_isi$state_regularity_class[direct] ==
               part$state_regularity_class[1L]) ||
        !all(per_isi$state_subtype[direct] == part$state_subtype[1L]) ||
        !all(per_isi$state_support_role[pause] == "canonical_pause_gap") ||
        any(nzchar(per_isi$state_subtype[pause])) ||
        any(per_isi$state_support_role[assigned & !direct & !pause] !=
              "excluded_or_unresolved_support") ||
        sum(direct) != support_n || sum(pause) != pause_n) {
      stpd_multitrack_auto_abort(
        "state_episode_projection_invalid",
        "Per-ISI Broad-HF episode, direct-support, and Pause roles do not close."
      )
    }
  }
  invisible(TRUE)
}

stpd_multitrack_auto_assert_per_isi_closure <- function(product, parent = NULL) {
  per_isi <- product$per_isi
  selected_trains <- stpd_multitrack_preview_selected_train_values(
    product$metadata$selected_trains[1]
  )
  materialized <- identical(
    stpd_multitrack_auto_chr(product$metadata$materialization_status),
    "materialized"
  )
  if (!materialized) {
    if (nrow(per_isi) != 0L) {
      stpd_multitrack_auto_abort(
        "per_isi_failed_product_nonempty",
        "A non-materialized automatic product cannot contain per-ISI rows."
      )
    }
    return(invisible(TRUE))
  }

  row_key <- paste(per_isi$train, per_isi$isi_index, sep = "\u001f")
  if (anyDuplicated(row_key) || any(per_isi$isi_index < 1L) ||
      any(!per_isi$train %in% selected_trains)) {
    stpd_multitrack_auto_abort(
      "per_isi_key_invalid",
      "The per-ISI projection contains duplicate, invalid, or out-of-scope keys."
    )
  }
  represented <- sort(unique(as.character(per_isi$train)), method = "radix")
  for (train in represented) {
    part <- per_isi[per_isi$train == train, , drop = FALSE]
    part <- part[order(part$isi_index, method = "radix"), , drop = FALSE]
    if (!identical(as.integer(part$isi_index), seq_len(nrow(part)))) {
      stpd_multitrack_auto_abort(
        "per_isi_index_not_contiguous",
        paste0("Per-ISI rows are not contiguous for train '", train, "'.")
      )
    }
  }

  interval_contract <- list(
    events = c(id = "event_id", family = "event_family",
               extent_class = "event_extent_class",
               frequency_class = "event_frequency_class"),
    states = c(
      id = "state_id", state_class = "state_class",
      state_family = "state_family",
      state_frequency_class = "state_frequency_class",
      state_regularity_class = "state_regularity_class",
      state_subtype = "state_subtype"
    ),
    gaps = c(id = "gap_id", gap_class = "gap_class")
  )
  for (table_name in names(interval_contract)) {
    table <- product[[table_name]]
    contract <- interval_contract[[table_name]]
    id_column <- unname(contract[["id"]])
    if (nrow(table) == 0L) next
    for (i in seq_len(nrow(table))) {
      interval <- table[i, , drop = FALSE]
      inside <- per_isi$train == interval$train &
        per_isi$isi_index >= interval$start_isi &
        per_isi$isi_index <= interval$end_isi
      assigned <- as.character(per_isi[[id_column]]) ==
        as.character(interval[[id_column]])
      assigned[is.na(assigned)] <- FALSE
      if (!identical(which(assigned), which(inside))) {
        stpd_multitrack_auto_abort(
          "per_isi_interval_closure_invalid",
          paste0("Per-ISI rows do not reconstruct ", table_name,
                 " interval '", interval[[id_column]], "'.")
        )
      }
      value_contract <- contract[names(contract) != "id"]
      for (source_column in names(value_contract)) {
        projection_column <- unname(value_contract[[source_column]])
        if (!all(as.character(per_isi[[projection_column]][inside]) ==
                 as.character(interval[[source_column]]))) {
          stpd_multitrack_auto_abort(
            "per_isi_label_projection_invalid",
            paste0("Per-ISI ", projection_column,
                   " disagrees with its canonical interval.")
          )
        }
      }
    }
  }

  empty_fk <- function(id, table_ids) {
    !nzchar(as.character(id)) | as.character(id) %in% as.character(table_ids)
  }
  foreign_keys_ok <- empty_fk(per_isi$event_id, product$events$event_id) &
    empty_fk(per_isi$state_id, product$states$state_id) &
    empty_fk(per_isi$gap_id, product$gaps$gap_id) &
    empty_fk(per_isi$state_episode_id, product$states$state_episode_id) &
    empty_fk(
      per_isi$review_candidate_id,
      product$review_candidates$review_candidate_id
    )
  if (!all(foreign_keys_ok) ||
      any(nzchar(per_isi$gap_id) &
            (nzchar(per_isi$event_id) | nzchar(per_isi$state_id)))) {
    stpd_multitrack_auto_abort(
      "per_isi_track_ownership_invalid",
      paste(
        "Per-ISI foreign keys must resolve, and a Pause Gap cannot co-own",
        "support with an Event or State."
      )
    )
  }

  reviews <- product$review_candidates
  active_reviews <- reviews[reviews$active_in_review_track, , drop = FALSE]
  expected_review_count <- integer(nrow(per_isi))
  if (nrow(reviews) > 0L) {
    for (i in seq_len(nrow(reviews))) {
      hit <- per_isi$train == reviews$train[i] &
        per_isi$isi_index >= reviews$start_isi[i] &
        per_isi$isi_index <= reviews$end_isi[i]
      expected_review_count[hit] <- expected_review_count[hit] + 1L
    }
  }
  if (!identical(as.integer(per_isi$review_candidate_count),
                 as.integer(expected_review_count))) {
    stpd_multitrack_auto_abort(
      "per_isi_review_count_invalid",
      "Per-ISI Review-candidate counts do not match Review interval coverage."
    )
  }
  if (nrow(active_reviews) > 0L) {
    for (i in seq_len(nrow(active_reviews))) {
      review <- active_reviews[i, , drop = FALSE]
      inside <- per_isi$train == review$train &
        per_isi$isi_index >= review$start_isi &
        per_isi$isi_index <= review$end_isi
      assigned <- per_isi$review_candidate_id == review$review_candidate_id
      assigned[is.na(assigned)] <- FALSE
      if (!identical(which(assigned), which(inside)) ||
          !all(per_isi$review_label[inside] == review$review_label)) {
        stpd_multitrack_auto_abort(
          "per_isi_review_projection_invalid",
          "Per-ISI active Review projection does not match Review geometry."
        )
      }
    }
  }

  if (nrow(per_isi) > 0L) {
    first <- per_isi$isi_index == 1L
    assignment_columns <- c(
      "event_id", "event_family", "event_extent_class",
      "event_frequency_class", "state_id", "state_class", "state_family",
      "state_frequency_class", "state_regularity_class", "state_subtype", "gap_id",
      "gap_class", "state_episode_id", "state_episode_class",
      "state_support_role", "review_candidate_id", "review_label"
    )
    if (any(vapply(assignment_columns, function(name) {
      any(nzchar(as.character(per_isi[[name]][first])))
    }, logical(1))) || any(per_isi$review_candidate_count[first] != 0L)) {
      stpd_multitrack_auto_abort(
        "first_isi_assignment_invalid",
        "The first spike row cannot carry an ISI-track assignment."
      )
    }
  }

  if (!is.null(parent)) {
    parent_trains <- names(parent$trains %||% list()) %||% character()
    if (!all(selected_trains %in% parent_trains)) {
      stpd_multitrack_auto_abort(
        "per_isi_parent_scope_invalid",
        "A selected train is missing from the parent dataset."
      )
    }
    expected_n <- sum(vapply(
      selected_trains, function(train) nrow(parent$trains[[train]]), integer(1)
    ))
    if (nrow(per_isi) != expected_n) {
      stpd_multitrack_auto_abort(
        "per_isi_parent_row_count_invalid",
        "The per-ISI table does not cover every parent row in selected scope."
      )
    }
    for (train in selected_trains) {
      dat <- parent$trains[[train]]
      part <- per_isi[per_isi$train == train, , drop = FALSE]
      part <- part[order(part$isi_index, method = "radix"), , drop = FALSE]
      timestamp <- if ("timestamp_sec" %in% names(dat)) {
        suppressWarnings(as.numeric(dat$timestamp_sec))
      } else rep(NA_real_, nrow(dat))
      isi <- if ("ISI_sec" %in% names(dat)) {
        suppressWarnings(as.numeric(dat$ISI_sec))
      } else rep(NA_real_, nrow(dat))
      if (!identical(as.numeric(part$timestamp_sec), timestamp) ||
          !identical(as.numeric(part$ISI_sec), isi)) {
        stpd_multitrack_auto_abort(
          "per_isi_parent_values_invalid",
          paste0("Per-ISI raw values differ from parent train '", train, "'.")
        )
      }
    }
  }
  invisible(TRUE)
}

stpd_multitrack_auto_validate <- function(
    product, parent = NULL, rematerialize_parent = TRUE) {
  required <- c(
    "metadata", "events", "states", "gaps", "review_candidates",
    "state_event_relationships", "per_isi", "hfs_context", "invariants",
    "manifest"
  )
  if (!is.list(product) || !all(required %in% names(product)) ||
      !all(vapply(product[required], is.data.frame, logical(1)))) {
    stpd_multitrack_auto_abort(
      "product_schema_invalid",
      "Canonical automatic multi-track product is missing required typed tables."
    )
  }
  stpd_multitrack_auto_assert_fixed_schemas(product)
  metadata <- product$metadata
  pending_authority <- nrow(metadata) == 1L &&
    !stpd_multitrack_auto_lgl(metadata$authoritative) &&
    identical(stpd_multitrack_auto_chr(metadata$authority_scope), "none_preview") &&
    identical(stpd_multitrack_auto_chr(metadata$promotion_status), "pending") &&
    identical(
      stpd_multitrack_auto_chr(metadata$detector_performance_block_reason),
      "gate_b_pending"
    )
  passed_authority <- nrow(metadata) == 1L &&
    stpd_multitrack_auto_lgl(metadata$authoritative) &&
    identical(
      stpd_multitrack_auto_chr(metadata$authority_scope),
      "automatic_prediction_record"
    ) &&
    identical(stpd_multitrack_auto_chr(metadata$promotion_status), "passed") &&
    identical(
      stpd_multitrack_auto_chr(metadata$detector_performance_block_reason),
      "gate_c_pending"
    )
  if (nrow(metadata) != 1L ||
      !identical(stpd_multitrack_auto_chr(metadata$schema_version),
                 stpd_multitrack_auto_schema_version()) ||
      !(pending_authority || passed_authority) ||
      !identical(stpd_multitrack_auto_chr(metadata$intended_target_scope),
                 "automatic_prediction_record") ||
      !identical(stpd_multitrack_auto_chr(metadata$promotion_gate), "gate_b") ||
      !identical(
        stpd_multitrack_auto_lgl(metadata$canonical_candidate_schema), TRUE
      ) ||
      !identical(stpd_multitrack_auto_lgl(metadata$biological_ground_truth), FALSE) ||
      !identical(stpd_multitrack_auto_lgl(metadata$legacy_single_track_parallel), TRUE) ||
      is.na(as.logical(metadata$label_blind[1])) ||
      !identical(
        stpd_multitrack_auto_chr(metadata$detection_mode),
        if (isTRUE(as.logical(metadata$label_blind[1]))) {
          "label_blind"
        } else {
          "manual_aware"
        }
      ) ||
      !identical(
        stpd_multitrack_auto_lgl(metadata$label_blind_execution),
        isTRUE(as.logical(metadata$label_blind[1]))
      ) ||
      !identical(
        stpd_multitrack_auto_lgl(metadata$detector_performance_eligible), FALSE
      ) ||
      !identical(
        stpd_multitrack_auto_chr(metadata$event_pause_conflict_policy),
        paste0(
          "canonical_pause_excluded_from_direct_support__",
          "state_episode_parent_acceptance_inherited__",
          "downstream_detector_reentry_forbidden"
        )
      )) {
    stpd_multitrack_auto_abort(
      "metadata_semantics_invalid",
      "Automatic multi-track candidate metadata has invalid promotion semantics."
    )
  }
  materialization_status <- stpd_multitrack_auto_chr(
    metadata$materialization_status
  )
  failure_code <- stpd_multitrack_auto_chr(metadata$failure_code)
  failure_message <- stpd_multitrack_auto_chr(metadata$failure_message)
  evidence_available <- stpd_multitrack_auto_lgl(
    metadata$source_multitrack_evidence_available
  )
  status_semantics <- if (identical(materialization_status, "materialized")) {
    evidence_available && !nzchar(failure_code) && !nzchar(failure_message)
  } else if (identical(materialization_status, "not_available")) {
    !evidence_available &&
      identical(failure_code, "source_multitrack_evidence_unavailable") &&
      nzchar(failure_message)
  } else if (identical(materialization_status, "failed_closed")) {
    evidence_available && nzchar(failure_code) && nzchar(failure_message)
  } else {
    FALSE
  }
  if (!isTRUE(status_semantics)) {
    stpd_multitrack_auto_abort(
      "materialization_status_invalid",
      "Automatic multi-track candidate status and failure fields disagree."
    )
  }
  expected_per_isi_status <- if (identical(
    materialization_status, "materialized"
  )) {
    c(
      materialization = "full_selected_train_scope",
      reason = "interval_projection_with_parent_and_bidirectional_validation"
    )
  } else {
    c(
      materialization = "not_materialized_due_to_product_status",
      reason = materialization_status
    )
  }
  if (!identical(
        stpd_multitrack_auto_chr(metadata$per_isi_materialization),
        unname(expected_per_isi_status[["materialization"]])
      ) || !identical(
        stpd_multitrack_auto_chr(metadata$per_isi_materialization_reason),
        unname(expected_per_isi_status[["reason"]])
      )) {
    stpd_multitrack_auto_abort(
      "per_isi_metadata_contract_invalid",
      "Per-ISI materialization metadata disagrees with product status."
    )
  }
  if (!identical(materialization_status, "materialized")) {
    must_be_empty <- c(
      "events", "states", "gaps", "review_candidates",
      "state_event_relationships", "per_isi", "hfs_context"
    )
    if (any(vapply(product[must_be_empty], nrow, integer(1)) != 0L) ||
        nrow(product$invariants) < 1L ||
        !any(product$invariants$check_name == failure_code &
               product$invariants$status == materialization_status)) {
      stpd_multitrack_auto_abort(
        "failed_product_payload_invalid",
        "Unavailable or failed-closed candidates must contain only their failure invariant."
      )
    }
  }
  run_id <- stpd_multitrack_auto_chr(metadata$run_id)
  params_hash <- stpd_multitrack_auto_chr(metadata$params_hash)
  policy_hash <- stpd_multitrack_auto_chr(metadata$policy_hash)
  if (!nzchar(run_id) || !grepl("^[0-9a-f]{64}$", params_hash) ||
      !grepl("^[0-9a-f]{64}$", policy_hash)) {
    stpd_multitrack_auto_abort(
      "metadata_identity_invalid", "Canonical automatic identity is incomplete."
    )
  }
  count_columns <- c(
    events_n = "events", states_n = "states", gaps_n = "gaps",
    review_candidates_n = "review_candidates",
    state_event_relationships_n = "state_event_relationships",
    per_isi_n = "per_isi", hfs_context_n = "hfs_context",
    invariants_n = "invariants"
  )
  recorded_counts <- as.integer(unlist(
    metadata[names(count_columns)], use.names = FALSE
  ))
  expected_counts <- as.integer(vapply(
    unname(count_columns), function(name) nrow(product[[name]]), integer(1)
  ))
  selected_trains <- stpd_multitrack_preview_selected_train_values(
    metadata$selected_trains[1]
  )
  data_trains <- unique(unlist(lapply(
    product[c(
      "events", "states", "gaps", "review_candidates",
      "state_event_relationships", "per_isi", "hfs_context"
    )], function(table) as.character(table$train)
  ), use.names = FALSE))
  data_trains <- data_trains[!is.na(data_trains) & nzchar(data_trains)]
  metadata_counts_ok <- identical(recorded_counts, expected_counts) &&
    identical(as.integer(metadata$selected_train_n),
              as.integer(length(selected_trains))) &&
    !anyDuplicated(selected_trains) &&
    all(data_trains %in% selected_trains) &&
    identical(
      stpd_multitrack_auto_chr(metadata$canonical_typed_artifact),
      "Multitrack_auto_candidate.rds"
    ) && grepl("^[0-9a-f]{64}$", metadata$input_sha256) &&
    grepl("^[0-9a-f]{64}$", metadata$threshold_table_sha256)
  if (!isTRUE(metadata_counts_ok)) {
    stpd_multitrack_auto_abort(
      "metadata_counts_invalid",
      "Automatic candidate metadata counts, hashes, or selected-train scope disagree with its tables."
    )
  }
  table_names <- setdiff(required, "manifest")
  for (name in table_names) {
    table <- product[[name]]
    if (nrow(table) == 0L) next
    if (!all(table$schema_version == stpd_multitrack_auto_schema_version()) ||
        !all(table$run_id == run_id) || !all(table$params_hash == params_hash) ||
        !all(table$policy_hash == policy_hash) ||
        !all(table$authoritative %in%
             stpd_multitrack_auto_lgl(metadata$authoritative))) {
      stpd_multitrack_auto_abort(
        "table_identity_invalid",
        paste0("Canonical table '", name, "' does not match product identity.")
      )
    }
  }
  event_geometry_key <- paste(
    product$events$train, product$events$start_isi,
    product$events$end_isi, sep = "\u001f"
  )
  if (anyDuplicated(product$events$event_id) ||
      anyDuplicated(event_geometry_key) ||
      anyDuplicated(product$states$state_id) ||
      anyDuplicated(product$gaps$gap_id) ||
      anyDuplicated(product$review_candidates$review_candidate_id) ||
      anyDuplicated(product$state_event_relationships$relationship_id)) {
    stpd_multitrack_auto_abort(
      "canonical_id_duplicate", "Canonical automatic IDs must be unique per table."
    )
  }
  stpd_multitrack_auto_assert_interval_geometry(
    product$events, "event_id", "Events", require_n_spikes = TRUE
  )
  stpd_multitrack_auto_assert_interval_geometry(
    product$states, "state_id", "States", require_n_spikes = TRUE
  )
  stpd_multitrack_auto_assert_interval_geometry(
    product$gaps, "gap_id", "Gaps"
  )
  stpd_multitrack_auto_assert_interval_geometry(
    product$review_candidates, "review_candidate_id", "Review candidates"
  )
  if (nrow(product$events) > 0L) {
    allowed_labels <- c(
      "burst", "long_burst", "prolonged_burst",
      "high_frequency_burst", "hf_burst"
    )
    expected_modifiers <- stpd_multitrack_auto_expected_event_modifiers(
      product$events$provenance_source_labels
    )
    provenance_columns <- c(
      "provenance_source_interval_ids", "provenance_source_candidate_ids",
      "provenance_source_candidate_keys", "provenance_source_labels",
      "provenance_candidate_layers", "provenance_candidate_sources"
    )
    provenance_is_canonical <- vapply(provenance_columns, function(name) {
      values <- as.character(product$events[[name]])
      expected <- vapply(values, function(value) {
        stpd_multitrack_auto_join_values(
          stpd_multitrack_auto_split_ids(value)
        )
      }, character(1))
      identical(values, unname(expected))
    }, logical(1))
    primary_is_provenance <- vapply(seq_len(nrow(product$events)), function(i) {
      event <- product$events[i, , drop = FALSE]
      required <- c(
        event$source_interval_id %in% stpd_multitrack_auto_split_ids(
          event$provenance_source_interval_ids
        ),
        event$source_candidate_id %in% stpd_multitrack_auto_split_ids(
          event$provenance_source_candidate_ids
        ),
        event$source_candidate_key %in% stpd_multitrack_auto_split_ids(
          event$provenance_source_candidate_keys
        ),
        event$source_label %in% stpd_multitrack_auto_split_ids(
          event$provenance_source_labels
        )
      )
      if (nzchar(event$candidate_layer)) {
        required <- c(required, event$candidate_layer %in%
          stpd_multitrack_auto_split_ids(event$provenance_candidate_layers))
      }
      if (nzchar(event$candidate_source)) {
        required <- c(required, event$candidate_source %in%
          stpd_multitrack_auto_split_ids(event$provenance_candidate_sources))
      }
      all(required)
    }, logical(1))
    expected_event_ids <- mapply(
      function(train, start, end) {
        stpd_multitrack_auto_id(
          "event", run_id, train, paste(start, end, sep = "-")
        )
      },
      product$events$train, product$events$start_isi, product$events$end_isi,
      USE.NAMES = FALSE
    )
    event_ok <- all(provenance_is_canonical) & primary_is_provenance &
      product$events$source_label %in% allowed_labels &
      product$events$event_id == expected_event_ids &
      product$events$event_family == expected_modifiers$event_family &
      product$events$extent_class == expected_modifiers$extent_class &
      product$events$frequency_class == expected_modifiers$frequency_class &
      product$events$extent_evidence_labels ==
        expected_modifiers$extent_evidence_labels &
      product$events$frequency_evidence_labels ==
        expected_modifiers$frequency_evidence_labels &
      product$events$extent_conflict == expected_modifiers$extent_conflict &
      product$events$modifier_diagnostic ==
        expected_modifiers$modifier_diagnostic
    event_ok[is.na(event_ok)] <- FALSE
    if (!all(event_ok)) {
      stpd_multitrack_auto_abort(
        "event_family_invalid",
        paste(
          "Every Event must have canonical same-geometry provenance and map",
          "exactly to one family plus unresolved-or-explicit modifiers."
        )
      )
    }
  }
  if (nrow(product$states) > 0L) {
    states <- product$states
    expected_parent <- stpd_multitrack_auto_state_parent_class(
      states$source_label
    )
    base_kind <- states$interval_kind %in% c("candidate", "fragment")
    derived_kind <- states$interval_kind %in% c(
      "state_redetected_fragment", "state_inherited_support_fragment"
    ) & nzchar(states$parent_source_interval_id) & nzchar(states$split_kind)
    hfs <- states$state_class == "high_frequency_spiking"
    tonic <- states$state_class == "tonic"
    expected_subtype <- ifelse(
      states$state_regularity_class == "regular", "high_frequency_tonic",
      ifelse(
        states$state_regularity_class == "irregular",
        "high_frequency_irregular_state", "hf_unresolved"
      )
    )
    hfs_semantics <- hfs &
      states$state_family == "broad_high_frequency_state" &
      states$state_frequency_class == "high" &
      states$state_regularity_class %in% c(
        "regular", "irregular", "unresolved"
      ) & states$state_subtype == expected_subtype &
      states$subtype_status %in% c(
        "classified", "regularity_gray_zone", "insufficient_direct_support",
        "source_support_unavailable"
      ) & states$subtype_n_valid_isi >= 0L &
      states$subtype_rule ==
        "state_frequency_rate_and_median_adjacent_cv2_v1"
    tonic_semantics <- tonic & states$source_label == "tonic" &
      states$state_family == "tonic" &
      states$state_frequency_class == "non_high" &
      states$state_regularity_class == "regular" &
      states$state_subtype == "tonic" &
      states$subtype_status == "classified_by_tonic_detector" &
      states$subtype_rule == "tonic_detector_support_v1"
    state_ok <- states$source_label %in% c(
      "high_frequency_spiking", "high_frequency_tonic",
      "high_frequency_irregular_state", "tonic"
    ) & states$state_class == expected_parent & (base_kind | derived_kind) &
      (hfs_semantics | tonic_semantics)
    state_ok[is.na(state_ok)] <- FALSE
    if (!all(state_ok)) {
      stpd_multitrack_auto_abort(
        "state_semantics_invalid", "State labels or interval kinds are invalid."
      )
    }
  }
  if (nrow(product$gaps) > 0L) {
    hard_state_flag <- as.logical(
      product$gaps$hard_for_state_direct_support
    )
    gap_ok <- product$gaps$source_label == "pause" &
      product$gaps$gap_class == "pause" &
      product$gaps$gap_semantics %in% c(
        "canonical_pause", "contextual_interburst_pause"
      ) & product$gaps$candidate_decision == "accepted" &
      !is.na(hard_state_flag)
    gap_ok[is.na(gap_ok)] <- FALSE
    if (!all(gap_ok)) {
      stpd_multitrack_auto_abort(
        "gap_semantics_invalid",
        "Active Gap rows must be accepted canonical or contextual Pause evidence."
      )
    }
  }
  stpd_multitrack_auto_assert_same_track_non_overlap(product$events, "event_id")
  stpd_multitrack_auto_assert_same_track_non_overlap(product$states, "state_id")
  stpd_multitrack_auto_assert_same_track_non_overlap(product$gaps, "gap_id")
  stpd_multitrack_auto_assert_disjoint(
    product$states,
    product$gaps[product$gaps$hard_for_state_direct_support, , drop = FALSE],
    "State", "Pause Gap"
  )
  stpd_multitrack_auto_assert_disjoint(
    product$events,
    product$gaps[product$gaps$hard_for_event, , drop = FALSE],
    "Event", "Pause Gap"
  )
  relationships <- product$state_event_relationships
  if (nrow(relationships) > 0L) {
    event_hit <- match(relationships$event_id, product$events$event_id)
    state_hit <- match(relationships$state_id, product$states$state_id)
    if (anyNA(event_hit) || anyNA(state_hit) ||
        !all(relationships$non_destructive %in% TRUE)) {
      stpd_multitrack_auto_abort(
        "state_event_relationship_invalid",
        "State-Event relationships must resolve and be non-destructive."
      )
    }
  }
  expected_relationships <- stpd_multitrack_auto_relationships(
    product$events, product$states, policy_hash, run_id, params_hash
  )
  if (passed_authority && nrow(expected_relationships) > 0L) {
    expected_relationships$authoritative <- TRUE
  }
  if (!identical(relationships, expected_relationships)) {
    stpd_multitrack_auto_abort(
      "state_event_relationship_closure_invalid",
      paste(
        "State-Event relationships must be the complete deterministic set of",
        "all and only canonical Event x State overlaps."
      )
    )
  }
  stpd_multitrack_auto_assert_relationship_geometry(product)
  stpd_multitrack_auto_assert_per_isi_closure(product, parent = parent)
  stpd_multitrack_auto_assert_state_episode_closure(product)
  if (nrow(product$hfs_context) > 0L) {
    state_hit <- match(product$hfs_context$state_id, product$states$state_id)
    if (anyNA(state_hit) ||
        !all(product$states$state_class[state_hit] == "high_frequency_spiking") ||
        !all(product$hfs_context$state_preserved %in% TRUE) ||
        any(product$hfs_context$destructive_action_applied %in% TRUE)) {
      stpd_multitrack_auto_abort(
        "hfs_context_destructive",
        "HFS context must preserve every referenced HFS State."
      )
    }
  }
  stpd_multitrack_auto_assert_hfs_context_closure(product)
  manifest <- product$manifest
  files <- stpd_multitrack_auto_manifest_filenames()
  if (nrow(manifest) != length(files) ||
      !identical(as.character(manifest$table_name), names(files))) {
    stpd_multitrack_auto_abort(
      "manifest_schema_invalid", "Canonical automatic manifest is incomplete."
    )
  }
  for (i in seq_len(nrow(manifest))) {
    name <- manifest$table_name[i]
    table <- product[[name]]
    if (!identical(as.integer(manifest$row_count[i]), as.integer(nrow(table))) ||
        !identical(as.integer(manifest$column_count[i]), as.integer(ncol(table))) ||
        !identical(as.character(manifest$column_types[i]),
                   stpd_multitrack_auto_column_types(table)) ||
        !identical(as.character(manifest$table_sha256[i]),
                   stpd_multitrack_auto_hash(table))) {
      stpd_multitrack_auto_abort(
        "manifest_hash_invalid",
        paste0("Canonical table '", name, "' does not match its manifest."))
    }
  }
  payload <- product[c(
    "events", "states", "gaps", "review_candidates",
    "state_event_relationships", "per_isi", "hfs_context", "invariants"
  )]
  if (!identical(
    stpd_multitrack_auto_chr(metadata$product_sha256),
    stpd_multitrack_auto_hash(payload)
  )) {
    stpd_multitrack_auto_abort(
      "product_hash_invalid", "Canonical automatic product SHA-256 is stale."
    )
  }
  parent_binding_verified <- FALSE
  if (!is.null(parent)) {
    parent_identity <- stpd_multitrack_preview_run_identity(parent)
    if (!identical(parent_identity$run_id, run_id) ||
        !identical(parent_identity$params_hash, params_hash)) {
      stpd_multitrack_auto_abort(
        "parent_identity_mismatch",
        "Canonical automatic product does not belong to its parent detector result."
      )
    }
    if (isTRUE(rematerialize_parent)) {
      effective_params <- parent$params_effective %||% parent$params_last %||% NULL
      if (is.null(effective_params) || !is.list(effective_params)) {
        stpd_multitrack_auto_abort(
          "parent_effective_params_missing",
          paste(
            "Parent-bound validation requires the effective parameters so the",
            "canonical product can be deterministically rematerialized."
          )
        )
      }
      selected <- stpd_multitrack_preview_selected_train_values(
        metadata$selected_trains[1]
      )
      rebuilt <- stpd_multitrack_auto_build(
        parent,
        params = effective_params,
        selected_trains = selected,
        run_id = run_id,
        params_hash = params_hash
      )
      if (passed_authority &&
          exists("stpd_multitrack_gate_b_promote_auto_product", mode = "function")) {
        rebuilt <- stpd_multitrack_gate_b_promote_auto_product(rebuilt)
      }
      compare_names <- c(
        "metadata", "events", "states", "gaps", "review_candidates",
        "state_event_relationships", "per_isi", "hfs_context", "invariants",
        "manifest"
      )
      differences <- compare_names[!vapply(compare_names, function(name) {
        identical(product[[name]], rebuilt[[name]])
      }, logical(1))]
      if (length(differences) > 0L) {
        stpd_multitrack_auto_abort(
          "parent_rematerialization_mismatch",
          paste0(
            "Persisted canonical product differs from deterministic Phase 1A/1B ",
            "rematerialization in table(s): ", paste(differences, collapse = ", "),
            "."
          )
        )
      }
      parent_binding_verified <- TRUE
    }
  }
  attr(product, "parent_binding_verified") <- parent_binding_verified
  product
}

stpd_multitrack_auto_strip <- function(ds) {
  if (is.list(ds) && is.list(ds$results) &&
      stpd_multitrack_auto_result_key() %in% names(ds$results)) {
    ds$results[[stpd_multitrack_auto_result_key()]] <- NULL
  }
  ds
}

stpd_multitrack_auto_attach <- function(
    ds, params, selected_trains = NULL, run_id = NULL,
    params_hash = NULL, target_trains = NULL) {
  if (is.null(selected_trains) && !is.null(target_trains)) {
    selected_trains <- target_trains
  }
  legacy_auto <- lapply(ds$trains %||% list(), function(dat) {
    if (is.data.frame(dat) && "pattern_auto" %in% names(dat)) {
      dat$pattern_auto
    } else {
      NULL
    }
  })
  legacy_events <- (ds$results %||% list())$events %||% NULL
  product <- stpd_multitrack_auto_build(
    ds, params, selected_trains, run_id, params_hash
  )
  out <- ds
  if (is.null(out$results) || !is.list(out$results)) out$results <- list()
  out$results[[stpd_multitrack_auto_result_key()]] <- product
  current_auto <- lapply(out$trains %||% list(), function(dat) {
    if (is.data.frame(dat) && "pattern_auto" %in% names(dat)) {
      dat$pattern_auto
    } else {
      NULL
    }
  })
  current_events <- (out$results %||% list())$events %||% NULL
  if (!identical(legacy_auto, current_auto) ||
      !identical(legacy_events, current_events)) {
    stpd_multitrack_auto_abort(
      "legacy_parallel_mutated",
      "Automatic multi-track attachment changed legacy pattern_auto or events."
    )
  }
  out
}

#' Access the automatic multi-track candidate product
#'
#' @param ds A detector dataset or a `stpd_multitrack_auto_product` object.
#' @return A strictly validated automatic multi-track product. Successful
#' Gate B runs carry automatic-prediction-record authority; Gate C remains
#' required for performance eligibility.
#' @export
stpd_multitrack_auto <- function(ds) {
  product <- if (inherits(ds, "stpd_multitrack_auto_product")) {
    ds
  } else if (is.list(ds) && is.list(ds$results)) {
    ds$results[[stpd_multitrack_auto_result_key()]] %||% NULL
  } else {
    NULL
  }
  if (is.null(product)) {
    stpd_multitrack_auto_abort(
      "product_missing",
      "Dataset does not contain results$multitrack_auto. Run the detector again."
    )
  }
  validated <- stpd_multitrack_auto_validate(
    product,
    parent = if (inherits(ds, "stpd_multitrack_auto_product")) NULL else ds,
    rematerialize_parent = !identical(
      stpd_multitrack_auto_chr(product$metadata$promotion_status), "passed"
    )
  )
  if (!inherits(ds, "stpd_multitrack_auto_product") &&
      identical(stpd_multitrack_auto_chr(product$metadata$promotion_status),
                "passed")) {
    gate <- (ds$results %||% list())$multitrack_gate_b %||% NULL
    bound <- is.list(gate) && is.data.frame(gate$metadata) &&
      nrow(gate$metadata) == 1L &&
      identical(gate$metadata$auto_product_sha256,
                product$metadata$product_sha256)
    attr(validated, "parent_binding_verified") <- isTRUE(bound)
  }
  validated
}

stpd_multitrack_auto_export_filenames <- function(include_status = TRUE) {
  files <- c(
    unname(stpd_multitrack_auto_manifest_filenames()),
    "Multitrack_auto_candidate_manifest.csv",
    "Multitrack_auto_candidate.rds"
  )
  if (isTRUE(include_status)) {
    files <- c(files, "Multitrack_auto_candidate_export_status.csv")
  }
  unique(files)
}

stpd_multitrack_auto_path_exists <- function(paths) {
  paths <- as.character(paths)
  link_target <- suppressWarnings(Sys.readlink(paths))
  is_link <- !is.na(link_target) & nzchar(link_target)
  file.exists(paths) | is_link
}

stpd_multitrack_auto_cleanup_exports <- function(out_dir) {
  out_dir <- as.character(out_dir)[1]
  if (is.na(out_dir) || !nzchar(out_dir) || !dir.exists(out_dir)) {
    return(invisible(character()))
  }
  paths <- file.path(
    out_dir, stpd_multitrack_auto_export_filenames(include_status = TRUE)
  )
  existing <- paths[stpd_multitrack_auto_path_exists(paths)]
  if (length(existing) > 0L) {
    unlink(existing, recursive = FALSE, force = TRUE)
  }
  remaining <- paths[stpd_multitrack_auto_path_exists(paths)]
  if (length(remaining) > 0L) {
    stpd_multitrack_auto_abort(
      "candidate_export_cleanup_failed",
      paste0(
        "Could not remove stale automatic-candidate export file(s): ",
        paste(basename(remaining), collapse = ", "), "."
      )
    )
  }
  invisible(existing)
}

stpd_multitrack_auto_assert_exports_absent <- function(
    out_dir, include_status = FALSE) {
  paths <- file.path(
    out_dir,
    stpd_multitrack_auto_export_filenames(include_status = include_status)
  )
  remaining <- paths[stpd_multitrack_auto_path_exists(paths)]
  if (length(remaining) > 0L) {
    stpd_multitrack_auto_abort(
      "candidate_export_cleanup_failed",
      paste0(
        "Automatic-candidate cleanup postcondition failed for: ",
        paste(basename(remaining), collapse = ", "), "."
      )
    )
  }
  invisible(TRUE)
}

stpd_multitrack_auto_export_status <- function(
    success, status, status_code, source_error_code = "",
    technical_detail = "", warning = !isTRUE(success)) {
  data.frame(
    schema_version = stpd_multitrack_auto_schema_version(),
    candidate_artifacts_written = isTRUE(success),
    export_status = as.character(status)[1],
    warning = isTRUE(warning),
    status_code = as.character(status_code)[1],
    source_error_code = as.character(source_error_code)[1],
    technical_detail = as.character(technical_detail)[1],
    legacy_preview_review_export_continues = TRUE,
    stringsAsFactors = FALSE
  )
}

stpd_multitrack_auto_write_export_status <- function(status, out_dir) {
  status_path <- file.path(
    out_dir, "Multitrack_auto_candidate_export_status.csv"
  )
  tryCatch(
    write_csv_safe(
      status, status_path, row.names = FALSE, fileEncoding = "UTF-8"
    ),
    error = function(e) {
      if (stpd_multitrack_auto_path_exists(status_path)) {
        unlink(status_path, force = TRUE)
      }
      if (stpd_multitrack_auto_path_exists(status_path)) {
        stpd_multitrack_auto_abort(
          "candidate_export_cleanup_failed",
          "A failed candidate export-status write left an unsafe stale file."
        )
      }
      stpd_multitrack_auto_abort(
        "candidate_export_status_write_failed",
        paste0("Could not write candidate export status: ", conditionMessage(e))
      )
    }
  )
  invisible(status_path)
}

stpd_write_multitrack_auto_fail_soft <- function(ds, out_dir) {
  out_dir <- as.character(out_dir)[1]
  stpd_multitrack_auto_cleanup_exports(out_dir)
  # Do not trust the cleanup implementation alone: this explicit coordinator
  # postcondition also protects against mocked/no-op or partially failing
  # cleanup paths.
  stpd_multitrack_auto_assert_exports_absent(out_dir, include_status = TRUE)
  product_present <- is.list(ds) && is.list(ds$results) &&
    !is.null(ds$results[[stpd_multitrack_auto_result_key()]])
  if (!isTRUE(product_present)) {
    status <- stpd_multitrack_auto_export_status(
      FALSE, "not_present", "candidate_product_not_present", warning = FALSE
    )
    stpd_multitrack_auto_write_export_status(status, out_dir)
    return(invisible(status))
  }
  result <- tryCatch(
    stpd_write_multitrack_auto(ds, out_dir),
    error = function(e) e
  )
  if (inherits(result, "condition")) {
    stpd_multitrack_auto_cleanup_exports(out_dir)
    stpd_multitrack_auto_assert_exports_absent(
      out_dir, include_status = TRUE
    )
    code <- stpd_multitrack_auto_chr(
      result$code, "candidate_export_failed"
    )
    text <- conditionMessage(result)
    status <- stpd_multitrack_auto_export_status(
      FALSE, "omit", "candidate_export_omitted", code, text
    )
    stpd_multitrack_auto_write_export_status(status, out_dir)
    message(
      "Warning: automatic multi-track candidate export failed; ",
      "legacy, Preview, and Review export will continue. ", text
    )
    return(invisible(status))
  }
  status <- stpd_multitrack_auto_export_status(
    TRUE, "include", "candidate_export_included"
  )
  stpd_multitrack_auto_write_export_status(status, out_dir)
  invisible(status)
}

#' Write the automatic multi-track candidate product
#'
#' @param ds A detector dataset or a `stpd_multitrack_auto_product` object.
#' @param out_dir Existing or new output directory.
#' @return Invisibly, a named character vector of written artifact paths.
#' @export
stpd_write_multitrack_auto <- function(ds, out_dir) {
  out_dir <- as.character(out_dir)[1]
  if (is.na(out_dir) || !nzchar(out_dir)) {
    stpd_multitrack_auto_abort(
      "output_directory_invalid", "out_dir must be one non-empty path."
    )
  }
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(out_dir)) {
    stpd_multitrack_auto_abort(
      "output_directory_unavailable", "Could not create the output directory."
    )
  }
  stpd_multitrack_auto_cleanup_exports(out_dir)
  stpd_multitrack_auto_assert_exports_absent(out_dir, include_status = TRUE)
  tryCatch({
    product <- stpd_multitrack_auto(ds)
    files <- stpd_multitrack_auto_manifest_filenames()
    paths <- stats::setNames(file.path(out_dir, unname(files)), names(files))
    for (name in names(files)) {
      write_csv_safe(
        product[[name]], paths[[name]], row.names = FALSE,
        fileEncoding = "UTF-8"
      )
    }
    manifest_path <- file.path(out_dir, "Multitrack_auto_candidate_manifest.csv")
    write_csv_safe(
      product$manifest, manifest_path, row.names = FALSE, fileEncoding = "UTF-8"
    )
    rds_path <- file.path(out_dir, "Multitrack_auto_candidate.rds")
    saveRDS(product, rds_path, version = 3)
    result <- c(paths, manifest = manifest_path, rds = rds_path)
    invisible(result)
  }, error = function(e) {
    stpd_multitrack_auto_cleanup_exports(out_dir)
    stop(e)
  })
}
