# Threshold-first Gate 1 candidate-lineage contracts ----------------------
#
# Diagnostic-only contracts. This module neither calls the detector nor
# changes scientific candidate selection, labels, thresholds, or RNG state.

STPD_CANDIDATE_LINEAGE_SCHEMA_VERSION <- "stpd_candidate_lineage_v3"
STPD_CANDIDATE_LINEAGE_AUDIT_LEVELS <- c("off", "summary", "full")
STPD_CANDIDATE_LINEAGE_UNAVAILABLE_REASONS <- c(
  "incomplete_early_stop", "pipeline_not_instrumented",
  "candidate_universe_unavailable", "adapter_mapping_unavailable",
  "legacy_audit_unavailable", "diagnostic_only"
)
STPD_CANDIDATE_LINEAGE_RELATIONS <- c(
  "derive", "merge", "split", "trim", "retype", "supersede"
)
STPD_CANDIDATE_LINEAGE_STAGES <- c(
  "raw_threshold_support_proposed",
  "seed_run_formed",
  "structure_candidate_proposed",
  "bridge_or_boundary_expanded",
  "burst_pause_ownership_resolved",
  "candidate_gate_adjudicated",
  "within_track_selected",
  "post_size_isi_validated",
  "multitrack_materialized"
)
STPD_CANDIDATE_LINEAGE_DECISIONS <- c(
  "proposed", "retained", "modified", "selected", "materialized",
  "rejected", "superseded", "truncated_by_cap", "not_applicable"
)
STPD_CANDIDATE_LINEAGE_REMOVALS <- c(
  "rejected", "superseded", "truncated_by_cap"
)
STPD_CANDIDATE_LINEAGE_TERMINALS <- c(
  STPD_CANDIDATE_LINEAGE_REMOVALS, "materialized"
)

stpd_candidate_lineage_reason_taxonomy <- function() {
  data.frame(
    reason_code = c(
      "not_yet_created", "threshold_support", "seed_formed",
      "structure_supported", "boundary_expanded", "ownership_resolved",
      "gate_passed", "within_track_winner", "post_size_isi_passed",
      "finalized", "continues", "terminal_previously_reached",
      "threshold_failed", "below_min_size", "ownership_lost",
      "overlap_lost", "post_size_isi_failed", "model_inadequate",
      "candidate_cap_rank", "merged_into_child", "split_at_pause",
      "trimmed_into_child", "retyped_into_child", "superseded_by_child",
      "no_candidate_proposed", "all_candidate_lineages_terminated",
      "geometry_support_lost"
    ),
    reason_precedence = as.integer(seq_len(27L)),
    reason_category = c(
      rep("lifecycle", 12L), rep("rejection", 6L), "cap",
      rep("supersession", 5L), rep("truth_evaluation", 3L)
    ),
    terminal_eligible = c(
      rep(FALSE, 12L), rep(TRUE, 12L), rep(FALSE, 3L)
    ),
    stringsAsFactors = FALSE
  )
}

stpd_candidate_lineage_product_ontology <- function() {
  data.frame(
    final_product_type = c(
      "Event", "Gap", "State", "Review", "Diagnostic"
    ),
    semantic_track = c(
      "event", "gap", "state", "review", "diagnostic"
    ),
    stringsAsFactors = FALSE
  )
}

stpd_candidate_lineage_publication_product_types <- function() {
  c("Event", "Gap", "State")
}

stpd_candidate_lineage_candidate_classes <- function() {
  c(
    "burst", "long_burst", "pause", "tonic", "broad_hfs", "hft",
    "hf_irregular", "unclassified"
  )
}

stpd_candidate_lineage_source_final_labels <- function() {
  c(
    stpd_candidate_lineage_candidate_classes(), "reject", "possible_burst",
    "possible_tonic", "high_frequency_spiking", "high_frequency_tonic",
    "high_frequency_irregular_state"
  )
}

stpd_candidate_lineage_raw_route_flags <- function(x) {
  raw_fields <- c(
    "raw_candidate_layer", "raw_candidate_class", "raw_final_label",
    "raw_candidate_source"
  )
  normalize_route_token <- function(value) {
    value <- tolower(trimws(as.character(value)))
    value <- gsub("[^a-z0-9]+", "_", value, perl = TRUE)
    value <- gsub("^_+|_+$", "", value, perl = TRUE)
    # A provider version suffix changes provenance, not ontology routing.
    gsub("_(v)?[0-9]+(_[0-9]+)*$", "", value, perl = TRUE)
  }
  raw <- as.data.frame(
    lapply(x[raw_fields], normalize_route_token),
    stringsAsFactors = FALSE
  )
  profile <- apply(raw, 1L, function(values) any(
    grepl("(^|_)profile($|_)", values, perl = TRUE) |
      values %in% c(
        "seed_bridge_thresholds", "event_grammar_params",
        "manual_examples", "manual_calibration"
      )
  ))
  composite <- apply(raw, 1L, function(values) any(
    grepl("(^|_)composite($|_)", values, perl = TRUE) |
      grepl("(^|_)recurrent_(burst[^_]*|paus[^_]*)($|_)",
            values, perl = TRUE) |
      grepl("(^|_)pause_dominant($|_)", values, perl = TRUE)
  ))
  other <- apply(raw, 1L, function(values) any(
    grepl("(^|_)(other|others)($|_)", values, perl = TRUE)
  ))
  list(profile = profile, composite = composite, other = other)
}

stpd_candidate_lineage_source_label_family <- function(label) {
  out <- rep(NA_character_, length(label))
  out[label %in% c("burst", "long_burst", "possible_burst")] <- "burst"
  out[label == "pause"] <- "pause"
  out[label %in% c("tonic", "possible_tonic")] <- "tonic"
  out[label %in% c(
    "broad_hfs", "hft", "hf_irregular", "high_frequency_spiking",
    "high_frequency_tonic", "high_frequency_irregular_state"
  )] <- "broad_hfs"
  out[label == "unclassified"] <- "unclassified"
  out
}

stpd_candidate_lineage_class_track_contract <- function() {
  primary <- data.frame(
    candidate_class = c(
      "burst", "long_burst", "pause", "tonic", "broad_hfs", "hft",
      "hf_irregular"
    ),
    semantic_track = c(
      "event", "event", "gap", "state", "state", "state", "state"
    ), stringsAsFactors = FALSE
  )
  secondary <- do.call(rbind, lapply(
    primary$candidate_class,
    function(candidate_class) data.frame(
      candidate_class = candidate_class,
      semantic_track = c("review", "diagnostic"),
      stringsAsFactors = FALSE
    )
  ))
  out <- rbind(
    primary, secondary,
    data.frame(
      candidate_class = "unclassified", semantic_track = "diagnostic",
      stringsAsFactors = FALSE
    )
  )
  rownames(out) <- NULL
  out
}

stpd_candidate_lineage_class_family_contract <- function() {
  data.frame(
    candidate_class = stpd_candidate_lineage_candidate_classes(),
    family_id = c(
      "burst", "burst", "pause", "tonic", "broad_hfs", "broad_hfs",
      "broad_hfs", "unclassified"
    ), stringsAsFactors = FALSE
  )
}

stpd_candidate_lineage_source_adapter_schema <- function() {
  c(
    registry_version = "character", registry_source = "character",
    provider_id = "character", raw_candidate_layer = "character",
    raw_candidate_class = "character", raw_final_label = "character",
    raw_candidate_source = "character",
    source_candidate_class = "character", source_final_label = "character",
    candidate_class = "character", semantic_track = "character",
    family_id = "character"
  )
}

stpd_candidate_lineage_empty_source_adapters <- function() {
  stpd_candidate_lineage_empty_from_schema(
    stpd_candidate_lineage_source_adapter_schema()
  )
}

stpd_candidate_lineage_builtin_source_registry <- function() {
  primary_class <- c(
    "burst", "long_burst", "pause", "tonic", "broad_hfs", "hft",
    "hf_irregular"
  )
  primary_track <- c(
    "event", "event", "gap", "state", "state", "state", "state"
  )
  primary_family <- c(
    "burst", "burst", "pause", "tonic", "broad_hfs", "broad_hfs",
    "broad_hfs"
  )
  primary <- data.frame(
    registry_version = "stpd_source_registry_v1",
    registry_source = "builtin", provider_id = "train_adaptive",
    raw_candidate_layer = "candidate",
    raw_candidate_class = primary_class,
    raw_final_label = primary_class,
    raw_candidate_source = "candidate_diagnostic_audit",
    source_candidate_class = primary_class,
    source_final_label = primary_class,
    candidate_class = primary_class, semantic_track = primary_track,
    family_id = primary_family,
    stringsAsFactors = FALSE
  )
  review <- do.call(rbind, lapply(
    c("burst", "tonic"), function(candidate_class) data.frame(
      registry_version = "stpd_source_registry_v1",
      registry_source = "builtin", provider_id = "train_adaptive",
      raw_candidate_layer = "review",
      raw_candidate_class = paste0("possible_", candidate_class),
      raw_final_label = candidate_class,
      raw_candidate_source = "candidate_diagnostic_audit",
      source_candidate_class = candidate_class,
      source_final_label = candidate_class,
      candidate_class = candidate_class, semantic_track = "review",
      family_id = if (candidate_class == "burst") "burst" else "tonic",
      stringsAsFactors = FALSE
    )
  ))
  diagnostic <- do.call(rbind, lapply(
    c(primary_class, "unclassified"), function(candidate_class) {
      raw_class <- if (candidate_class == "unclassified") "other" else
        candidate_class
      data.frame(
        registry_version = "stpd_source_registry_v1",
        registry_source = "builtin", provider_id = "train_adaptive",
        raw_candidate_layer = "diagnostic",
        raw_candidate_class = raw_class,
        raw_final_label = raw_class,
        raw_candidate_source = "candidate_diagnostic_audit",
        source_candidate_class = candidate_class,
        source_final_label = candidate_class,
        candidate_class = candidate_class, semantic_track = "diagnostic",
        family_id = switch(
          candidate_class, burst = "burst", long_burst = "burst",
          pause = "pause", tonic = "tonic", broad_hfs = "broad_hfs",
          hft = "broad_hfs", hf_irregular = "broad_hfs",
          unclassified = "unclassified"
        ),
        stringsAsFactors = FALSE
      )
    }
  ))
  diagnostic <- rbind(
    diagnostic,
    transform(
      diagnostic[diagnostic$candidate_class == "unclassified", , drop = FALSE],
      raw_candidate_class = "others", raw_final_label = "others"
    )
  )
  # manual_examples/manual_calibration are parameter metadata and do not
  # create geometry nodes. Interactive adjudication is a native review route;
  # directly imported intervals require provider_id=external_data_import and
  # an explicit external adapter.
  external_algorithm <- do.call(rbind, lapply(
    c("mean_isi", "logisi_newbd"), function(provider_id) data.frame(
      registry_version = "stpd_source_registry_v1",
      registry_source = "builtin", provider_id = provider_id,
      raw_candidate_layer = "candidate", raw_candidate_class = "burst",
      raw_final_label = "burst", raw_candidate_source = provider_id,
      source_candidate_class = "burst", source_final_label = "burst",
      candidate_class = "burst", semantic_track = "event",
      family_id = "burst",
      stringsAsFactors = FALSE
    )
  ))
  native_row <- function(
      layer, raw_class, raw_label, source_class, source_label,
      candidate_class, track, family, raw_source = "") {
    data.frame(
      registry_version = "stpd_source_registry_v1",
      registry_source = "builtin", provider_id = "native_stpd",
      raw_candidate_layer = layer, raw_candidate_class = raw_class,
      raw_final_label = raw_label, raw_candidate_source = raw_source,
      source_candidate_class = source_class,
      source_final_label = source_label, candidate_class = candidate_class,
      semantic_track = track, family_id = family,
      stringsAsFactors = FALSE
    )
  }
  native <- do.call(rbind, list(
    native_row("event_core_hf_tonic_state", "event_core_hf_tonic",
               "reject", "hft", "reject", "hft", "diagnostic",
               "broad_hfs"),
    native_row("event_core_hf_tonic_state", "event_core_hf_tonic",
               "high_frequency_tonic", "hft", "high_frequency_tonic",
               "hft", "state", "broad_hfs"),
    native_row("event_core_hf_irregular_state", "event_core_hf_irregular",
               "high_frequency_irregular_state", "hf_irregular",
               "high_frequency_irregular_state", "hf_irregular", "state",
               "broad_hfs"),
    native_row("event_core_pause_gap", "event_core_pause_gap", "pause",
               "pause", "pause", "pause", "gap", "pause"),
    native_row("event_core_tonic_state", "event_core_tonic", "reject",
               "tonic", "reject", "tonic", "diagnostic", "tonic"),
    native_row("event_core_tonic_state", "event_core_tonic", "tonic",
               "tonic", "tonic", "tonic", "state", "tonic"),
    native_row("event_grammar_hf_spiking_state",
               "event_grammar_long_hf_spiking_epoch",
               "high_frequency_spiking", "broad_hfs",
               "high_frequency_spiking", "broad_hfs", "state",
               "broad_hfs"),
    native_row("seed_bridge_hf_spiking_epoch",
               "seed_bridge_hf_spiking_epoch", "high_frequency_spiking",
               "broad_hfs", "high_frequency_spiking", "broad_hfs",
               "state", "broad_hfs"),
    native_row("seed_bridge_hf_tonic_state", "seed_bridge_hf_tonic_state",
               "high_frequency_tonic", "hft", "high_frequency_tonic",
               "hft", "state", "broad_hfs"),
    native_row("nested_hfs_local_rate_contrast",
               "nested_hfs_local_rate_review_proposal", "possible_burst",
               "burst", "possible_burst", "burst", "review", "burst",
               "accepted_broad_hfs_local_context"),
    native_row("review_adjudication", "possible_burst", "burst", "burst",
               "burst", "burst", "event", "burst",
               "legacy_phase2b_exact_migration"),
    native_row("tonic_review_candidates", "possible_tonic",
               "possible_tonic", "tonic", "possible_tonic", "tonic",
               "review", "tonic", "near_lower_single_isi"),
    native_row("tonic_review_candidates", "possible_tonic",
               "possible_tonic", "tonic", "possible_tonic", "tonic",
               "review", "tonic", "local_regular_subwindow"),
    native_row("structure_first_burst_screen",
               "structure_first_two_sided_classic_burst", "burst",
               "burst", "burst", "burst", "event", "burst"),
    native_row("structure_first_burst_screen",
               "structure_first_two_sided_classic_burst", "reject",
               "burst", "reject", "burst", "diagnostic", "burst"),
    native_row("structure_first_burst_screen",
               "structure_first_endpoint_classic_burst", "burst",
               "burst", "burst", "burst", "event", "burst"),
    native_row("structure_first_burst_screen",
               "structure_first_endpoint_classic_burst", "possible_burst",
               "burst", "possible_burst", "burst", "review", "burst"),
    native_row("structure_first_burst_screen",
               "structure_first_endpoint_classic_burst", "reject",
               "burst", "reject", "burst", "diagnostic", "burst")
  ))
  burst_sources <- data.frame(
    layer = c(
      "event_grammar_burst_event", "event_grammar_burst_episode",
      "event_grammar_single_expanded_bridge_burst",
      "isi_profile_hard_threshold_burst", "event_core_burst_bridge_merge",
      "seed_bridge_burst_seed_bridge", "dataset_isi_seed_centered_burst"
    ),
    raw_class = c(
      "event_grammar_seed_centered_burst",
      "event_grammar_dense_short_isi_episode",
      "event_grammar_single_expanded_bridge_burst",
      "isi_profile_hard_threshold_burst", "event_core_burst_bridge_merge",
      "seed_bridge_burst_seed_bridge", "dataset_isi_seed_centered_burst"
    ), stringsAsFactors = FALSE
  )
  burst_label_rows <- list()
  for (i in seq_len(nrow(burst_sources))) {
    labels <- c("burst", "long_burst")
    if (burst_sources$layer[i] %in% c(
      "event_grammar_burst_event", "event_grammar_burst_episode",
      "isi_profile_hard_threshold_burst", "seed_bridge_burst_seed_bridge",
      "dataset_isi_seed_centered_burst"
    )) labels <- c(labels, "possible_burst")
    if (burst_sources$layer[i] %in% c(
      "event_grammar_burst_event", "seed_bridge_burst_seed_bridge",
      "dataset_isi_seed_centered_burst"
    )) labels <- c(labels, "reject")
    for (label in labels) {
      candidate_class <- if (label == "long_burst") "long_burst" else "burst"
      track <- if (label == "possible_burst") "review" else if (
        label == "reject"
      ) "diagnostic" else "event"
      burst_label_rows[[length(burst_label_rows) + 1L]] <- native_row(
        burst_sources$layer[i], burst_sources$raw_class[i], label, "burst",
        label, candidate_class, track, "burst"
      )
    }
  }
  native_burst <- do.call(rbind, burst_label_rows)
  out <- rbind(primary, review, diagnostic, external_algorithm, native,
               native_burst)
  key <- do.call(paste, c(out[c(
    "provider_id", "raw_candidate_layer", "raw_candidate_class",
    "raw_final_label", "raw_candidate_source"
  )], sep = "\r"))
  out <- out[order(key, method = "radix"), , drop = FALSE]
  rownames(out) <- NULL
  out
}

stpd_candidate_lineage_validate_source_adapters <- function(adapters) {
  stpd_candidate_lineage_assert_external_schema(
    adapters, stpd_candidate_lineage_source_adapter_schema(),
    "source_adapter"
  )
  if (!nrow(adapters)) return(invisible(TRUE))
  stpd_candidate_lineage_require_text(
    adapters,
    setdiff(names(stpd_candidate_lineage_source_adapter_schema()),
            "raw_candidate_source"),
    "source_adapter"
  )
  if (anyNA(adapters$raw_candidate_source)) {
    stpd_candidate_lineage_abort(
      "source_adapter_invalid",
      "raw_candidate_source may be empty but cannot be missing."
    )
  }
  raw_route <- stpd_candidate_lineage_raw_route_flags(adapters)
  if (any(raw_route$profile)) {
    stpd_candidate_lineage_abort(
      "source_adapter_forbidden_route",
      paste(
        "Profile/threshold/manual-calibration metadata belongs in a",
        "summary table, not candidate lineage."
      )
    )
  }
  if (any(raw_route$composite)) {
    stpd_candidate_lineage_abort(
      "source_adapter_forbidden_route",
      "Composite recurrent states are post-product relationships, not primary candidates."
    )
  }
  if (any(adapters$registry_version != "stpd_source_registry_v1") ||
      any(adapters$registry_source != "external")) {
    stpd_candidate_lineage_abort(
      "source_adapter_invalid",
      "Adapter rows must be explicit external stpd_source_registry_v1 mappings."
    )
  }
  if (any(tolower(trimws(adapters$provider_id)) == "manual")) {
    stpd_candidate_lineage_abort(
      "source_adapter_forbidden_route",
      paste(
        "provider_id=manual is reserved for non-geometric calibration",
        "metadata; direct intervals require external_data_import."
      )
    )
  }
  contract <- stpd_candidate_lineage_class_track_contract()
  observed <- paste(adapters$candidate_class, adapters$semantic_track,
                    sep = "\r")
  allowed <- paste(contract$candidate_class, contract$semantic_track,
                   sep = "\r")
  if (any(!(observed %in% allowed)) ||
      any(!(adapters$source_candidate_class %in%
              stpd_candidate_lineage_candidate_classes())) ||
      any(!(adapters$source_final_label %in%
              stpd_candidate_lineage_source_final_labels()))) {
    stpd_candidate_lineage_abort(
      "source_adapter_invalid",
      "Adapter canonical class, label, or track is outside the frozen ontology."
    )
  }
  family_contract <- stpd_candidate_lineage_class_family_contract()
  expected_family <- family_contract$family_id[match(
    adapters$candidate_class, family_contract$candidate_class
  )]
  if (anyNA(expected_family) || any(adapters$family_id != expected_family)) {
    stpd_candidate_lineage_abort(
      "source_adapter_invalid",
      "Adapter canonical class and biological family disagree."
    )
  }
  expected_source_family <- family_contract$family_id[match(
    adapters$source_candidate_class, family_contract$candidate_class
  )]
  expected_label_family <- stpd_candidate_lineage_source_label_family(
    adapters$source_final_label
  )
  label_has_family <- !is.na(expected_label_family)
  if (anyNA(expected_source_family) ||
      any(adapters$family_id != expected_source_family) ||
      any(label_has_family &
          adapters$family_id != expected_label_family)) {
    stpd_candidate_lineage_abort(
      "source_adapter_cross_family",
      paste(
        "An adapter cannot silently change biological family;",
        "cross-family changes require a typed retype edge."
      )
    )
  }
  if (any(adapters$source_final_label == "reject" &
          adapters$semantic_track != "diagnostic") ||
      any(adapters$source_final_label %in%
            c("possible_burst", "possible_tonic") &
          adapters$semantic_track != "review")) {
    stpd_candidate_lineage_abort(
      "source_adapter_label_track_invalid",
      "reject is diagnostic-only and possible_* labels are review-only."
    )
  }
  if (any(raw_route$other &
          (adapters$candidate_class != "unclassified" |
           adapters$semantic_track != "diagnostic"))) {
    stpd_candidate_lineage_abort(
      "source_adapter_invalid",
      "Raw other/others may map only to diagnostic/unclassified."
    )
  }
  key_fields <- c(
    "provider_id", "raw_candidate_layer", "raw_candidate_class",
    "raw_final_label", "raw_candidate_source"
  )
  if (anyDuplicated(adapters[key_fields])) {
    stpd_candidate_lineage_abort(
      "source_adapter_duplicate", "Adapter registry keys must be unique."
    )
  }
  adapter_key <- do.call(paste, c(adapters[key_fields], sep = "\r"))
  builtin <- stpd_candidate_lineage_builtin_source_registry()
  builtin_key <- do.call(paste, c(builtin[key_fields], sep = "\r"))
  if (any(adapter_key %in% builtin_key)) {
    stpd_candidate_lineage_abort(
      "source_adapter_builtin_override",
      "An adapter cannot shadow an exact built-in source-registry key."
    )
  }
  invisible(TRUE)
}

stpd_candidate_lineage_source_registry <- function(
    source_adapters = stpd_candidate_lineage_empty_source_adapters()) {
  stpd_candidate_lineage_validate_source_adapters(source_adapters)
  out <- rbind(stpd_candidate_lineage_builtin_source_registry(),
               source_adapters)
  key <- do.call(paste, c(out[c(
    "provider_id", "raw_candidate_layer", "raw_candidate_class",
    "raw_final_label", "raw_candidate_source"
  )], sep = "\r"))
  if (anyDuplicated(key)) {
    stpd_candidate_lineage_abort(
      "source_adapter_duplicate", "Combined source registry keys must be unique."
    )
  }
  out <- out[order(key, method = "radix"), , drop = FALSE]
  rownames(out) <- NULL
  out
}

stpd_candidate_lineage_stage_decision_reason_contract <- function() {
  primary_reason <- c(
    raw_threshold_support_proposed = "threshold_support",
    seed_run_formed = "seed_formed",
    structure_candidate_proposed = "structure_supported",
    bridge_or_boundary_expanded = "boundary_expanded",
    burst_pause_ownership_resolved = "ownership_resolved",
    candidate_gate_adjudicated = "gate_passed",
    within_track_selected = "within_track_winner",
    post_size_isi_validated = "post_size_isi_passed",
    multitrack_materialized = "finalized"
  )
  rows <- list()
  add <- function(stage, decision, reason) {
    rows[[length(rows) + 1L]] <<- data.frame(
      stage = stage, decision = decision, reason_code = reason,
      stringsAsFactors = FALSE
    )
  }
  for (stage in STPD_CANDIDATE_LINEAGE_STAGES) {
    add(stage, "not_applicable", "not_yet_created")
    add(stage, "not_applicable", "terminal_previously_reached")
  }
  for (stage in STPD_CANDIDATE_LINEAGE_STAGES[1:8]) {
    add(stage, "proposed", primary_reason[[stage]])
  }
  for (stage in STPD_CANDIDATE_LINEAGE_STAGES[2:8]) {
    add(stage, "retained", primary_reason[[stage]])
  }
  add("bridge_or_boundary_expanded", "modified", "boundary_expanded")
  add("burst_pause_ownership_resolved", "modified", "ownership_resolved")
  add("within_track_selected", "selected", "within_track_winner")
  add("multitrack_materialized", "materialized", "finalized")
  add("raw_threshold_support_proposed", "rejected", "threshold_failed")
  for (stage in STPD_CANDIDATE_LINEAGE_STAGES[c(2, 3, 6, 8)]) {
    add(stage, "rejected", "below_min_size")
  }
  add("burst_pause_ownership_resolved", "rejected", "ownership_lost")
  add("within_track_selected", "rejected", "overlap_lost")
  add("post_size_isi_validated", "rejected", "post_size_isi_failed")
  for (stage in STPD_CANDIDATE_LINEAGE_STAGES[c(1, 3, 6)]) {
    add(stage, "rejected", "model_inadequate")
  }
  add("bridge_or_boundary_expanded", "superseded", "merged_into_child")
  add("burst_pause_ownership_resolved", "superseded", "split_at_pause")
  for (stage in STPD_CANDIDATE_LINEAGE_STAGES[c(4, 5)]) {
    add(stage, "superseded", "trimmed_into_child")
  }
  for (stage in STPD_CANDIDATE_LINEAGE_STAGES[c(5, 7)]) {
    add(stage, "superseded", "retyped_into_child")
  }
  for (stage in STPD_CANDIDATE_LINEAGE_STAGES[2:9]) {
    add(stage, "superseded", "superseded_by_child")
  }
  for (stage in STPD_CANDIDATE_LINEAGE_STAGES[1:8]) {
    add(stage, "truncated_by_cap", "candidate_cap_rank")
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

stpd_candidate_lineage_abort <- function(code, message, field = NA_character_) {
  condition <- structure(
    list(
      message = as.character(message)[1L], call = NULL,
      code = as.character(code)[1L], field = as.character(field)[1L]
    ),
    class = c(
      paste0("stpd_candidate_lineage_", as.character(code)[1L]),
      "stpd_candidate_lineage_error", "error", "condition"
    )
  )
  stop(condition)
}

stpd_candidate_lineage_schema <- function() {
  list(
    candidate_nodes = c(
      schema_version = "character", candidate_node_id = "character",
      run_id = "character", params_hash = "character",
      dataset_id = "character", train_id = "character",
      group_id = "character", fold_id = "character",
      analysis_block_id = "character", family_id = "character",
      semantic_track = "character", node_role = "character",
      candidate_class = "character", node_origin = "character",
      source_candidate_class = "character",
      source_final_label = "character",
      raw_candidate_layer = "character",
      raw_candidate_class = "character", raw_final_label = "character",
      raw_candidate_source = "character",
      source_candidate_id = "character", source_payload_sha256 = "character",
      provider_id = "character", source_mode = "character",
      created_stage = "character", created_stage_order = "integer",
      # rank is non-scientific display/source order only; selection and
      # truncation use candidate_stage_events$cap_rank under a typed policy.
      created_sequence = "integer", rank = "integer",
      original_start_isi = "integer", original_end_isi = "integer",
      current_start_isi = "integer", current_end_isi = "integer",
      threshold_lower_sec = "double", threshold_upper_sec = "double",
      stability_status = "character", model_adequacy_status = "character",
      gray_zone = "logical", direct_isi_count = "integer",
      bridge_isi_count = "integer", borrowed_isi_count = "integer",
      direct_fraction = "double", bridge_fraction = "double",
      borrowed_fraction = "double", terminal_decision = "character",
      node_first_failure_stage = "character",
      node_first_failure_reason = "character",
      final_product_type = "character", final_product_id = "character",
      product_payload_sha256 = "character",
      direct_support_sha256 = "character",
      direct_support_isi_indices = "character",
      direct_support_isi_count = "integer",
      runtime_ms = "double", candidate_count = "integer"
    ),
    candidate_lineage_edges = c(
      schema_version = "character", lineage_edge_id = "character",
      run_id = "character", params_hash = "character",
      dataset_id = "character", train_id = "character",
      group_id = "character", fold_id = "character",
      analysis_block_id = "character",
      operation_id = "character", operation_sequence = "integer",
      parent_candidate_node_id = "character",
      child_candidate_node_id = "character", relation = "character",
      stage = "character", stage_order = "integer",
      reason_code = "character"
    ),
    candidate_stage_events = c(
      schema_version = "character", stage_event_id = "character",
      run_id = "character", params_hash = "character",
      dataset_id = "character", train_id = "character",
      group_id = "character", fold_id = "character",
      analysis_block_id = "character",
      stage_scope_id = "character", candidate_node_id = "character",
      stage = "character", stage_order = "integer",
      decision = "character", reason_code = "character",
      reason_precedence = "integer", source_decision_code = "character",
      current_start_isi = "integer", current_end_isi = "integer",
      overlap_winner_candidate_node_id = "character",
      final_product_type = "character", final_product_id = "character",
      runtime_ms = "double", candidate_count = "integer",
      candidate_cap = "integer", cap_scope_member = "logical",
      cap_rank = "integer", cap_policy_id = "character",
      universe_status = "character"
    )
  )
}

stpd_candidate_lineage_authoritative_product_schema <- function() {
  c(
    run_id = "character", params_hash = "character",
    dataset_id = "character", train_id = "character",
    group_id = "character", fold_id = "character",
    analysis_block_id = "character", final_product_type = "character",
    final_product_id = "character", semantic_track = "character",
    candidate_class = "character", start_isi = "integer",
    end_isi = "integer", product_payload_sha256 = "character",
    direct_support_sha256 = "character",
    direct_support_isi_indices = "character",
    direct_support_isi_count = "integer"
  )
}

stpd_candidate_lineage_cap_policy_schema <- function() {
  c(
    stage_scope_id = "character", run_id = "character",
    params_hash = "character", dataset_id = "character",
    train_id = "character", group_id = "character", fold_id = "character",
    analysis_block_id = "character", family_id = "character",
    semantic_track = "character", stage = "character",
    stage_order = "integer", candidate_cap = "integer",
    policy_id = "character", policy_status = "character"
  )
}

stpd_candidate_lineage_empty_authoritative_products <- function() {
  stpd_candidate_lineage_empty_from_schema(
    stpd_candidate_lineage_authoritative_product_schema()
  )
}

stpd_candidate_lineage_empty_cap_policies <- function() {
  stpd_candidate_lineage_empty_from_schema(
    stpd_candidate_lineage_cap_policy_schema()
  )
}

stpd_candidate_lineage_canonical_manifest_rows <- function(x, key_fields) {
  if (!nrow(x)) return(x)
  order_args <- c(unname(x[key_fields]), list(method = "radix"))
  out <- x[do.call(order, order_args), , drop = FALSE]
  rownames(out) <- NULL
  out
}

stpd_candidate_lineage_canonical_cap_manifest <- function(x) {
  stpd_candidate_lineage_canonical_manifest_rows(x, "stage_scope_id")
}

stpd_candidate_lineage_canonical_product_manifest <- function(x) {
  stpd_candidate_lineage_canonical_manifest_rows(
    x, c(
      "run_id", "params_hash", "dataset_id", "train_id", "group_id",
      "fold_id", "analysis_block_id", "final_product_type",
      "final_product_id"
    )
  )
}

stpd_candidate_lineage_assert_external_schema <- function(x, schema, name) {
  if (!is.data.frame(x) || !identical(names(x), names(schema))) {
    stpd_candidate_lineage_abort(
      paste0(name, "_schema_invalid"), paste0(name, " violates its frozen schema.")
    )
  }
  actual <- vapply(x, stpd_candidate_lineage_column_type, character(1))
  if (!identical(unname(actual), unname(schema))) {
    stpd_candidate_lineage_abort(
      paste0(name, "_schema_invalid"), paste0(name, " types violate the frozen schema.")
    )
  }
  invisible(TRUE)
}

stpd_candidate_lineage_validate_cap_policies <- function(policies) {
  stpd_candidate_lineage_assert_external_schema(
    policies, stpd_candidate_lineage_cap_policy_schema(), "cap_policy"
  )
  if (!nrow(policies)) return(invisible(TRUE))
  stpd_candidate_lineage_require_text(policies, c(
    "stage_scope_id", "run_id", "params_hash", "dataset_id", "train_id",
    "group_id", "fold_id", "analysis_block_id", "family_id",
    "semantic_track", "stage", "policy_id", "policy_status"
  ), "cap_policy")
  expected_order <- match(policies$stage, STPD_CANDIDATE_LINEAGE_STAGES)
  if (anyNA(expected_order) || any(policies$stage_order != expected_order) ||
      anyNA(policies$candidate_cap) || any(policies$candidate_cap < 1L) ||
      any(!(policies$policy_status %in%
              c("applied_complete", "incomplete_early_stop"))) ||
      anyDuplicated(policies$stage_scope_id)) {
    stpd_candidate_lineage_abort(
      "cap_policy_invalid", "Cap policy values are invalid or duplicated."
    )
  }
  expected_ids <- vapply(seq_len(nrow(policies)), function(i) {
    stpd_candidate_lineage_cap_scope_id(
      policies$run_id[i], policies$params_hash[i], policies$dataset_id[i],
      policies$train_id[i], policies$group_id[i], policies$fold_id[i],
      policies$analysis_block_id[i], policies$family_id[i],
      policies$semantic_track[i], policies$stage[i], policies$policy_id[i]
    )
  }, character(1))
  if (!identical(policies$stage_scope_id, expected_ids)) {
    stpd_candidate_lineage_abort(
      "cap_policy_identity_mismatch",
      "stage_scope_id is not the deterministic external cap-policy identity."
    )
  }
  invisible(TRUE)
}

stpd_candidate_lineage_truth_schema <- function() {
  c(
    truth_id = "character", run_id = "character",
    params_hash = "character",
    dataset_id = "character", train_id = "character",
    group_id = "character", fold_id = "character",
    analysis_block_id = "character", family_id = "character",
    truth_start_isi = "integer", truth_end_isi = "integer"
  )
}

stpd_candidate_lineage_empty_from_schema <- function(schema) {
  columns <- lapply(unname(schema), function(type) {
    switch(
      type, character = character(), integer = integer(), double = double(),
      logical = logical(), stpd_candidate_lineage_abort(
        "schema_invalid", paste0("Unsupported schema type: ", type, ".")
      )
    )
  })
  names(columns) <- names(schema)
  as.data.frame(columns, stringsAsFactors = FALSE, check.names = FALSE)
}

stpd_candidate_lineage_empty_table <- function(table_name) {
  schema <- stpd_candidate_lineage_schema()[[table_name]]
  if (is.null(schema)) {
    stpd_candidate_lineage_abort(
      "table_unknown", paste0("Unknown lineage table: ", table_name, ".")
    )
  }
  stpd_candidate_lineage_empty_from_schema(schema)
}

stpd_candidate_lineage_empty_bundle <- function() {
  list(
    candidate_nodes = stpd_candidate_lineage_empty_table("candidate_nodes"),
    candidate_lineage_edges = stpd_candidate_lineage_empty_table(
      "candidate_lineage_edges"
    ),
    candidate_stage_events = stpd_candidate_lineage_empty_table(
      "candidate_stage_events"
    )
  )
}

stpd_candidate_lineage_sha256_id <- function(prefix, ...) {
  fields <- list(...)
  if (!is.character(prefix) || length(prefix) != 1L || is.na(prefix) ||
      !grepl("^[a-z][a-z0-9_]*_$", prefix)) {
    stpd_candidate_lineage_abort(
      "id_prefix_invalid", "ID prefix must be lower-snake-case ending in underscore."
    )
  }
  if (!length(fields) || is.null(names(fields)) || anyNA(names(fields)) ||
      any(!nzchar(names(fields))) || anyDuplicated(names(fields))) {
    stpd_candidate_lineage_abort(
      "id_payload_invalid", "ID fields require unique non-empty names."
    )
  }
  domain <- paste0(
    "stpd-candidate-lineage-v3-",
    sub("_$", "", gsub("_", "-", prefix, fixed = TRUE)), "-id"
  )
  paste0(prefix, stpd_threshold_first_hash_domain(domain, fields))
}

stpd_candidate_lineage_encode_direct_support <- function(isi_indices) {
  if (!is.integer(isi_indices) || !length(isi_indices) ||
      anyNA(isi_indices) || any(isi_indices < 1L) ||
      anyDuplicated(isi_indices) || is.unsorted(isi_indices, strictly = TRUE)) {
    stpd_candidate_lineage_abort(
      "direct_support_invalid",
      "Direct support must be a non-empty strictly increasing positive integer vector."
    )
  }
  paste(isi_indices, collapse = ",")
}

stpd_candidate_lineage_decode_direct_support <- function(encoded) {
  if (!is.character(encoded) || length(encoded) != 1L || is.na(encoded) ||
      !grepl("^[1-9][0-9]*(,[1-9][0-9]*)*$", encoded)) {
    stpd_candidate_lineage_abort(
      "direct_support_invalid",
      "Encoded direct support is not canonical positive integer CSV."
    )
  }
  values_double <- as.double(strsplit(encoded, ",", fixed = TRUE)[[1L]])
  if (any(!is.finite(values_double)) ||
      any(values_double > .Machine$integer.max)) {
    stpd_candidate_lineage_abort(
      "direct_support_invalid", "Direct-support ISI indices exceed integer range."
    )
  }
  values <- as.integer(values_double)
  if (!identical(stpd_candidate_lineage_encode_direct_support(values), encoded)) {
    stpd_candidate_lineage_abort(
      "direct_support_invalid", "Direct-support encoding is not canonical."
    )
  }
  values
}

stpd_candidate_lineage_direct_support_sha256 <- function(isi_indices) {
  encoded <- stpd_candidate_lineage_encode_direct_support(isi_indices)
  stpd_threshold_first_hash_domain(
    "stpd-candidate-lineage-direct-support-v1",
    list(encoded_isi_indices = encoded)
  )
}

stpd_candidate_lineage_node_id <- function(
    run_id, params_hash, dataset_id, train_id, group_id, fold_id,
    analysis_block_id,
    source_candidate_id, source_payload_sha256, family_id, semantic_track,
    node_role, candidate_class, node_origin,
    source_candidate_class, source_final_label,
    raw_candidate_layer, raw_candidate_class, raw_final_label,
    raw_candidate_source, provider_id, source_mode,
    original_start_isi, original_end_isi,
    current_start_isi, current_end_isi, created_stage,
    threshold_lower_sec, threshold_upper_sec,
    stability_status, model_adequacy_status, gray_zone,
    direct_isi_count, bridge_isi_count, borrowed_isi_count,
    direct_fraction, bridge_fraction, borrowed_fraction,
    final_product_type = NA_character_, final_product_id = NA_character_,
    product_payload_sha256 = NA_character_,
    direct_support_sha256 = NA_character_,
    direct_support_isi_indices = NA_character_,
    direct_support_isi_count = NA_integer_) {
  # runtime_ms is intentionally excluded: identity describes scientific
  # provenance and creation geometry, not caller ordering or machine time.
  stpd_candidate_lineage_sha256_id(
    "candidate_", run_id = run_id, params_hash = params_hash,
    dataset_id = dataset_id, train_id = train_id,
    group_id = group_id, fold_id = fold_id,
    analysis_block_id = analysis_block_id,
    source_candidate_id = source_candidate_id,
    source_payload_sha256 = source_payload_sha256, family_id = family_id,
    semantic_track = semantic_track, node_role = node_role,
    candidate_class = candidate_class, node_origin = node_origin,
    source_candidate_class = source_candidate_class,
    source_final_label = source_final_label,
    raw_candidate_layer = raw_candidate_layer,
    raw_candidate_class = raw_candidate_class,
    raw_final_label = raw_final_label,
    raw_candidate_source = raw_candidate_source, provider_id = provider_id,
    source_mode = source_mode,
    original_start_isi = as.integer(original_start_isi),
    original_end_isi = as.integer(original_end_isi),
    current_start_isi = as.integer(current_start_isi),
    current_end_isi = as.integer(current_end_isi), created_stage = created_stage,
    threshold_lower_sec = threshold_lower_sec,
    threshold_upper_sec = threshold_upper_sec,
    stability_status = stability_status,
    model_adequacy_status = model_adequacy_status, gray_zone = gray_zone,
    direct_isi_count = as.integer(direct_isi_count),
    bridge_isi_count = as.integer(bridge_isi_count),
    borrowed_isi_count = as.integer(borrowed_isi_count),
    direct_fraction = direct_fraction, bridge_fraction = bridge_fraction,
    borrowed_fraction = borrowed_fraction,
    final_product_type = final_product_type,
    final_product_id = final_product_id,
    product_payload_sha256 = product_payload_sha256,
    direct_support_sha256 = direct_support_sha256,
    direct_support_isi_indices = direct_support_isi_indices,
    direct_support_isi_count = as.integer(direct_support_isi_count)
  )
}

stpd_candidate_lineage_operation_id <- function(
    run_id, params_hash, dataset_id, train_id, group_id, fold_id,
    analysis_block_id, stage, relation, parent_candidate_node_ids,
    child_candidate_node_ids, reason_code) {
  parents <- sort(unique(parent_candidate_node_ids), method = "radix")
  children <- sort(unique(child_candidate_node_ids), method = "radix")
  stpd_candidate_lineage_sha256_id(
    "operation_", run_id = run_id, params_hash = params_hash,
    dataset_id = dataset_id, train_id = train_id, group_id = group_id,
    fold_id = fold_id, analysis_block_id = analysis_block_id,
    stage = stage, relation = relation,
    parent_candidate_node_ids = parents,
    child_candidate_node_ids = children, reason_code = reason_code
  )
}

stpd_candidate_lineage_edge_id <- function(
    run_id, params_hash, dataset_id, train_id, group_id, fold_id,
    analysis_block_id, operation_id,
    parent_candidate_node_id, child_candidate_node_id,
    relation, stage, reason_code) {
  stpd_candidate_lineage_sha256_id(
    "edge_", run_id = run_id, params_hash = params_hash,
    dataset_id = dataset_id, train_id = train_id,
    group_id = group_id, fold_id = fold_id,
    analysis_block_id = analysis_block_id, operation_id = operation_id,
    parent_candidate_node_id = parent_candidate_node_id,
    child_candidate_node_id = child_candidate_node_id, relation = relation,
    stage = stage, reason_code = reason_code
  )
}

stpd_candidate_lineage_stage_event_id <- function(
    run_id, params_hash, stage_scope_id, candidate_node_id, stage, decision,
    reason_code, source_decision_code, current_start_isi, current_end_isi,
    overlap_winner_candidate_node_id = NA_character_,
    final_product_type = NA_character_, final_product_id = NA_character_,
    candidate_count = 0L, candidate_cap = 0L, cap_scope_member = FALSE,
    cap_rank = 0L, cap_policy_id = "", universe_status = "not_applicable") {
  stpd_candidate_lineage_sha256_id(
    "stage_event_", run_id = run_id, params_hash = params_hash,
    stage_scope_id = stage_scope_id,
    candidate_node_id = candidate_node_id, stage = stage,
    decision = decision, reason_code = reason_code,
    source_decision_code = source_decision_code,
    current_start_isi = as.integer(current_start_isi),
    current_end_isi = as.integer(current_end_isi),
    overlap_winner_candidate_node_id = overlap_winner_candidate_node_id,
    final_product_type = final_product_type, final_product_id = final_product_id,
    candidate_count = as.integer(candidate_count),
    candidate_cap = as.integer(candidate_cap),
    cap_scope_member = cap_scope_member, cap_rank = as.integer(cap_rank),
    cap_policy_id = cap_policy_id,
    universe_status = universe_status
  )
}

stpd_candidate_lineage_cap_scope_id <- function(
    run_id, params_hash, dataset_id, train_id, group_id, fold_id,
    analysis_block_id, family_id, semantic_track, stage, policy_id) {
  stpd_candidate_lineage_sha256_id(
    "stage_scope_", run_id = run_id, params_hash = params_hash,
    dataset_id = dataset_id, train_id = train_id, group_id = group_id,
    fold_id = fold_id, analysis_block_id = analysis_block_id,
    family_id = family_id, semantic_track = semantic_track,
    stage = stage, policy_id = policy_id
  )
}

stpd_candidate_lineage_column_type <- function(x) {
  if (is.integer(x)) return("integer")
  if (is.double(x)) return("double")
  if (is.character(x)) return("character")
  if (is.logical(x)) return("logical")
  class(x)[1L]
}

stpd_candidate_lineage_assert_table <- function(x, table_name) {
  schema <- stpd_candidate_lineage_schema()[[table_name]]
  if (!is.data.frame(x) || is.null(schema)) {
    stpd_candidate_lineage_abort(
      "table_invalid", paste0(table_name, " must be a data frame."), table_name
    )
  }
  if (!identical(names(x), names(schema))) {
    stpd_candidate_lineage_abort(
      "schema_mismatch", paste0(table_name, " columns violate the frozen schema."),
      table_name
    )
  }
  actual <- vapply(x, stpd_candidate_lineage_column_type, character(1))
  if (!identical(unname(actual), unname(schema))) {
    stpd_candidate_lineage_abort(
      "type_mismatch", paste0(table_name, " types violate the frozen schema."),
      table_name
    )
  }
  if (nrow(x) && (anyNA(x$schema_version) || !all(
    x$schema_version == STPD_CANDIDATE_LINEAGE_SCHEMA_VERSION
  ))) {
    stpd_candidate_lineage_abort(
      "schema_version_unsupported", "Unsupported candidate-lineage schema."
    )
  }
  invisible(TRUE)
}

stpd_candidate_lineage_require_text <- function(x, fields, table_name) {
  for (field in fields) {
    if (anyNA(x[[field]]) || any(!nzchar(x[[field]]))) {
      stpd_candidate_lineage_abort(
        "required_field_invalid",
        paste0(table_name, "$", field, " must be non-empty and non-missing."),
        field
      )
    }
  }
}

stpd_candidate_lineage_scope_fields <- function() {
  c(
    "run_id", "params_hash", "dataset_id", "train_id", "group_id",
    "fold_id", "analysis_block_id"
  )
}

stpd_candidate_lineage_expected_created_sequence <- function(nodes, edges) {
  expected <- integer(nrow(nodes))
  if (!nrow(nodes)) return(expected)
  scope_stage <- interaction(
    nodes$run_id, nodes$params_hash, nodes$dataset_id, nodes$train_id,
    nodes$group_id, nodes$fold_id, nodes$analysis_block_id,
    nodes$created_stage_order, drop = TRUE, lex.order = TRUE
  )
  origin_order <- match(nodes$node_origin, c("root", "derived", "final_product"))
  for (indices in split(seq_len(nrow(nodes)), scope_stage)) {
    ids <- nodes$candidate_node_id[indices]
    local_edges <- edges[
      edges$parent_candidate_node_id %in% ids &
        edges$child_candidate_node_id %in% ids, , drop = FALSE
    ]
    incoming <- setNames(integer(length(ids)), ids)
    if (nrow(local_edges)) {
      incoming <- table(factor(
        local_edges$child_candidate_node_id, levels = ids
      ))
      incoming <- as.integer(incoming)
      names(incoming) <- ids
    }
    remaining <- ids
    sequence_value <- 1L
    while (length(remaining)) {
      ready <- remaining[incoming[remaining] == 0L]
      if (!length(ready)) {
        stpd_candidate_lineage_abort(
          "cycle_detected", "Same-stage candidate lineage must be a DAG."
        )
      }
      ready_rows <- match(ready, nodes$candidate_node_id)
      ready <- ready[order(
        origin_order[ready_rows], ready, method = "radix"
      )]
      for (node_id in ready) {
        node_row <- match(node_id, nodes$candidate_node_id)
        expected[node_row] <- sequence_value
        sequence_value <- sequence_value + 1L
        children <- local_edges$child_candidate_node_id[
          local_edges$parent_candidate_node_id == node_id
        ]
        for (child_id in unique(children)) {
          incoming[child_id] <- incoming[child_id] - sum(
            local_edges$parent_candidate_node_id == node_id &
              local_edges$child_candidate_node_id == child_id
          )
        }
      }
      remaining <- setdiff(remaining, ready)
    }
  }
  as.integer(expected)
}

stpd_candidate_lineage_validate_nodes <- function(
    nodes,
    source_adapters = stpd_candidate_lineage_empty_source_adapters()) {
  stpd_candidate_lineage_assert_table(nodes, "candidate_nodes")
  if (!nrow(nodes)) return(invisible(TRUE))
  stpd_candidate_lineage_require_text(nodes, c(
    "candidate_node_id", "run_id", "params_hash", "dataset_id", "train_id",
    "group_id", "fold_id", "analysis_block_id", "family_id",
    "semantic_track", "node_role", "candidate_class", "node_origin",
    "source_candidate_class", "source_final_label",
    "raw_candidate_layer", "raw_candidate_class", "raw_final_label",
    "source_candidate_id", "source_payload_sha256",
    "provider_id", "source_mode", "created_stage",
    "stability_status", "model_adequacy_status", "terminal_decision"
  ), "candidate_nodes")
  if (anyNA(nodes$raw_candidate_source)) {
    stpd_candidate_lineage_abort(
      "required_field_invalid",
      "candidate_nodes$raw_candidate_source may be empty but cannot be missing.",
      "raw_candidate_source"
    )
  }
  if (anyDuplicated(nodes$candidate_node_id)) {
    stpd_candidate_lineage_abort(
      "primary_key_duplicate", "candidate_node_id must be unique."
    )
  }
  if (any(!grepl("^[0-9a-f]{64}$", nodes$params_hash)) ||
      any(!grepl("^[0-9a-f]{64}$", nodes$source_payload_sha256))) {
    stpd_candidate_lineage_abort(
      "hash_invalid", "params_hash and source_payload_sha256 must be SHA-256 hex."
    )
  }
  expected_stage <- match(nodes$created_stage, STPD_CANDIDATE_LINEAGE_STAGES)
  if (anyNA(expected_stage) || any(nodes$created_stage_order != expected_stage)) {
    stpd_candidate_lineage_abort(
      "stage_order_invalid", "created_stage_order violates the frozen stage order."
    )
  }
  if (anyNA(nodes$created_sequence) || any(nodes$created_sequence < 1L) ||
      anyNA(nodes$rank) || any(nodes$rank < 1L)) {
    stpd_candidate_lineage_abort(
      "rank_invalid", "created_sequence and rank must be positive integers."
    )
  }
  # rank is non-scientific provider/presentation order only. It is excluded
  # from node identity and never determines a cap; cap_rank lives on a
  # specific stage event and policy.
  run_hashes <- split(nodes$params_hash, nodes$run_id)
  if (any(vapply(run_hashes, function(x) length(unique(x)) != 1L,
                 logical(1)))) {
    stpd_candidate_lineage_abort(
      "params_scope_invalid", "One run_id must have exactly one params_hash."
    )
  }
  if (any(!(nodes$semantic_track %in%
              c("event", "gap", "state", "review", "diagnostic"))) ||
      any(!(nodes$node_role %in% c("candidate", "final_product"))) ||
      any(!(nodes$node_origin %in% c("root", "derived", "final_product"))) ||
      any(!(nodes$candidate_class %in%
              stpd_candidate_lineage_candidate_classes()))) {
    stpd_candidate_lineage_abort(
      "enum_invalid",
      "semantic_track, node role/origin, or candidate class is invalid."
    )
  }
  raw_route <- stpd_candidate_lineage_raw_route_flags(nodes)
  if (any(raw_route$profile)) {
    stpd_candidate_lineage_abort(
      "profile_node_forbidden",
      paste(
        "Train profiles and manual calibration/examples are metadata",
        "summaries and cannot enter interval lineage nodes."
      )
    )
  }
  if (any(raw_route$composite)) {
    stpd_candidate_lineage_abort(
      "composite_node_forbidden",
      "Composite recurrent states are created after final products and cannot enter the nine-stage candidate lineage."
    )
  }
  class_track <- stpd_candidate_lineage_class_track_contract()
  observed_class_track <- paste(
    nodes$candidate_class, nodes$semantic_track, sep = "\r"
  )
  allowed_class_track <- paste(
    class_track$candidate_class, class_track$semantic_track, sep = "\r"
  )
  if (any(!(observed_class_track %in% allowed_class_track))) {
    stpd_candidate_lineage_abort(
      "candidate_class_track_invalid",
      "candidate_class and semantic_track violate the frozen ontology."
    )
  }
  if (any(raw_route$other & (nodes$candidate_class != "unclassified" |
                             nodes$semantic_track != "diagnostic"))) {
    stpd_candidate_lineage_abort(
      "other_candidate_forbidden",
      "Raw other is per-ISI filler and may enter lineage only as diagnostic/unclassified."
    )
  }
  registry <- stpd_candidate_lineage_source_registry(source_adapters)
  key_fields <- c(
    "provider_id", "raw_candidate_layer", "raw_candidate_class",
    "raw_final_label", "raw_candidate_source"
  )
  node_key <- do.call(paste, c(nodes[key_fields], sep = "\r"))
  registry_key <- do.call(paste, c(registry[key_fields], sep = "\r"))
  registry_match <- match(node_key, registry_key)
  if (anyNA(registry_match)) {
    stpd_candidate_lineage_abort(
      "adapter_mapping_missing",
      "Every raw provider/layer/class/label/source tuple requires an explicit mapping."
    )
  }
  mapped_fields <- c(
    "source_candidate_class", "source_final_label", "candidate_class",
    "semantic_track", "family_id"
  )
  for (field in mapped_fields) {
    if (any(nodes[[field]] != registry[[field]][registry_match])) {
      stpd_candidate_lineage_abort(
        "adapter_mapping_mismatch",
        paste0("Node ", field, " disagrees with the explicit source registry.")
      )
    }
  }
  expected_source_family <- stpd_candidate_lineage_class_family_contract()
  expected_source_family <- expected_source_family$family_id[match(
    nodes$source_candidate_class,
    stpd_candidate_lineage_class_family_contract()$candidate_class
  )]
  expected_label_family <- stpd_candidate_lineage_source_label_family(
    nodes$source_final_label
  )
  label_has_family <- !is.na(expected_label_family)
  if (anyNA(expected_source_family) ||
      any(nodes$family_id != expected_source_family) ||
      any(label_has_family & nodes$family_id != expected_label_family)) {
    stpd_candidate_lineage_abort(
      "source_family_invalid",
      "Source class/label cannot silently cross biological family."
    )
  }
  if (any(nodes$source_final_label == "reject" &
          nodes$semantic_track != "diagnostic") ||
      any(nodes$source_final_label %in%
            c("possible_burst", "possible_tonic") &
          nodes$semantic_track != "review")) {
    stpd_candidate_lineage_abort(
      "source_label_track_invalid",
      "reject is diagnostic-only and possible_* labels are review-only."
    )
  }
  geometry <- c(
    "original_start_isi", "original_end_isi", "current_start_isi",
    "current_end_isi"
  )
  if (anyNA(nodes[geometry]) || any(nodes$original_start_isi < 1L) ||
      any(nodes$current_start_isi < 1L) ||
      any(nodes$original_end_isi < nodes$original_start_isi) ||
      any(nodes$current_end_isi < nodes$current_start_isi) ||
      any(pmin(nodes$original_end_isi, nodes$current_end_isi) <
            pmax(nodes$original_start_isi, nodes$current_start_isi))) {
    stpd_candidate_lineage_abort("geometry_invalid", "Candidate geometry is invalid.")
  }
  counts <- c(
    "direct_isi_count", "bridge_isi_count", "borrowed_isi_count",
    "candidate_count"
  )
  if (anyNA(nodes[counts]) || any(as.matrix(nodes[counts]) < 0L)) {
    stpd_candidate_lineage_abort("count_invalid", "Candidate counts are invalid.")
  }
  fractions <- c("direct_fraction", "bridge_fraction", "borrowed_fraction")
  matrix <- as.matrix(nodes[fractions])
  span <- nodes$current_end_isi - nodes$current_start_isi + 1L
  support_counts <- as.matrix(nodes[c(
    "direct_isi_count", "bridge_isi_count", "borrowed_isi_count"
  )])
  expected_fractions <- support_counts / span
  tolerance <- sqrt(.Machine$double.eps)
  if (anyNA(matrix) || any(!is.finite(matrix)) || any(matrix < 0) ||
      any(matrix > 1) || any(rowSums(support_counts) > span) ||
      any(abs(matrix - expected_fractions) > tolerance)) {
    stpd_candidate_lineage_abort(
      "support_audit_invalid",
      paste(
        "Direct/bridge/borrowed support counts must partition no more than",
        "the current envelope, and each fraction must equal count/span."
      )
    )
  }
  if (any(
    !is.na(nodes$threshold_lower_sec) & !is.na(nodes$threshold_upper_sec) &
      nodes$threshold_lower_sec > nodes$threshold_upper_sec
  ) || any(!is.na(nodes$threshold_lower_sec) &
            !is.finite(nodes$threshold_lower_sec)) ||
      any(!is.na(nodes$threshold_upper_sec) &
            !is.finite(nodes$threshold_upper_sec)) ||
      any(!is.na(nodes$threshold_lower_sec) & nodes$threshold_lower_sec < 0) ||
      any(!is.na(nodes$threshold_upper_sec) & nodes$threshold_upper_sec < 0)) {
    stpd_candidate_lineage_abort("threshold_invalid", "Threshold bounds are invalid.")
  }
  if (anyNA(nodes$gray_zone) || anyNA(nodes$runtime_ms) ||
      any(!is.finite(nodes$runtime_ms)) || any(nodes$runtime_ms < 0)) {
    stpd_candidate_lineage_abort(
      "diagnostic_value_invalid", "Node diagnostic values are invalid."
    )
  }
  if (any(!(nodes$terminal_decision %in% STPD_CANDIDATE_LINEAGE_TERMINALS))) {
    stpd_candidate_lineage_abort(
      "terminal_decision_invalid", "terminal_decision is invalid."
    )
  }
  final <- nodes$node_role == "final_product"
  if (any(final)) {
    ontology <- stpd_candidate_lineage_product_ontology()
    expected_track <- ontology$semantic_track[match(
      nodes$final_product_type[final], ontology$final_product_type
    )]
    if (anyNA(expected_track) ||
        any(nodes$semantic_track[final] != expected_track)) {
      stpd_candidate_lineage_abort(
        "product_ontology_invalid",
        "final_product_type and semantic_track violate the frozen ontology."
      )
    }
  }
  if (any((nodes$node_origin == "final_product") != final) ||
      any(final & nodes$created_stage != "multitrack_materialized") ||
      any(!final & nodes$node_origin == "final_product")) {
    stpd_candidate_lineage_abort(
      "node_origin_invalid",
      "node_origin must agree with role and final-product creation stage."
    )
  }
  expected_ids <- vapply(seq_len(nrow(nodes)), function(i) {
    stpd_candidate_lineage_node_id(
      nodes$run_id[i], nodes$params_hash[i], nodes$dataset_id[i],
      nodes$train_id[i], nodes$group_id[i], nodes$fold_id[i],
      nodes$analysis_block_id[i],
      nodes$source_candidate_id[i], nodes$source_payload_sha256[i],
      nodes$family_id[i], nodes$semantic_track[i], nodes$node_role[i],
      nodes$candidate_class[i], nodes$node_origin[i],
      nodes$source_candidate_class[i], nodes$source_final_label[i],
      nodes$raw_candidate_layer[i], nodes$raw_candidate_class[i],
      nodes$raw_final_label[i], nodes$raw_candidate_source[i],
      nodes$provider_id[i], nodes$source_mode[i],
      nodes$original_start_isi[i], nodes$original_end_isi[i],
      nodes$current_start_isi[i], nodes$current_end_isi[i],
      nodes$created_stage[i], nodes$threshold_lower_sec[i],
      nodes$threshold_upper_sec[i], nodes$stability_status[i],
      nodes$model_adequacy_status[i], nodes$gray_zone[i],
      nodes$direct_isi_count[i], nodes$bridge_isi_count[i],
      nodes$borrowed_isi_count[i], nodes$direct_fraction[i],
      nodes$bridge_fraction[i], nodes$borrowed_fraction[i],
      nodes$final_product_type[i], nodes$final_product_id[i],
      nodes$product_payload_sha256[i], nodes$direct_support_sha256[i],
      nodes$direct_support_isi_indices[i],
      nodes$direct_support_isi_count[i]
    )
  }, character(1))
  if (!identical(nodes$candidate_node_id, expected_ids)) {
    stpd_candidate_lineage_abort(
      "identity_mismatch", "candidate_node_id is not its deterministic identity."
    )
  }
  product_present <- !is.na(nodes$final_product_id) & nzchar(nodes$final_product_id) &
    !is.na(nodes$final_product_type) & nzchar(nodes$final_product_type)
  product_hash_present <- !is.na(nodes$product_payload_sha256) &
    grepl("^[0-9a-f]{64}$", nodes$product_payload_sha256) &
    !is.na(nodes$direct_support_sha256) &
    grepl("^[0-9a-f]{64}$", nodes$direct_support_sha256) &
    !is.na(nodes$direct_support_isi_indices) &
    nzchar(nodes$direct_support_isi_indices) &
    !is.na(nodes$direct_support_isi_count) &
    nodes$direct_support_isi_count > 0L &
    nodes$direct_support_isi_count <=
      nodes$current_end_isi - nodes$current_start_isi + 1L
  any_product_field <- !is.na(nodes$final_product_type) |
    !is.na(nodes$final_product_id) |
    !is.na(nodes$product_payload_sha256) |
    !is.na(nodes$direct_support_sha256) |
    !is.na(nodes$direct_support_isi_indices) |
    !is.na(nodes$direct_support_isi_count)
  if (any(final != product_present) ||
      any(final != product_hash_present) ||
      any(!final & any_product_field) ||
      any(nodes$terminal_decision[final] != "materialized") ||
      any(nodes$terminal_decision[!final] == "materialized")) {
    stpd_candidate_lineage_abort(
      "final_product_invalid",
      "Only final_product nodes may materialize and name final products."
    )
  }
  if (anyDuplicated(paste(
    nodes$run_id[final], nodes$params_hash[final], nodes$dataset_id[final],
    nodes$train_id[final], nodes$group_id[final], nodes$fold_id[final],
    nodes$analysis_block_id[final], nodes$final_product_type[final],
    nodes$final_product_id[final], sep = "\r"
  ))) {
    stpd_candidate_lineage_abort(
      "final_product_duplicate", "Each final product requires one final_product node."
    )
  }
  if (any(final)) {
    replayed_support <- lapply(
      nodes$direct_support_isi_indices[final],
      stpd_candidate_lineage_decode_direct_support
    )
    replayed_count <- vapply(replayed_support, length, integer(1))
    replayed_hash <- vapply(
      replayed_support, stpd_candidate_lineage_direct_support_sha256,
      character(1)
    )
    inside <- vapply(seq_along(replayed_support), function(i) all(
      replayed_support[[i]] >= nodes$current_start_isi[final][i] &
        replayed_support[[i]] <= nodes$current_end_isi[final][i]
    ), logical(1))
    if (any(replayed_count != nodes$direct_support_isi_count[final]) ||
        any(replayed_hash != nodes$direct_support_sha256[final]) ||
        any(!inside)) {
      stpd_candidate_lineage_abort(
        "direct_support_replay_mismatch",
        "Final-product direct support must replay to its frozen hash/count inside the envelope."
      )
    }
    ontology <- stpd_candidate_lineage_product_ontology()
    expected_track <- ontology$semantic_track[match(
      nodes$final_product_type[final], ontology$final_product_type
    )]
    if (anyNA(expected_track) ||
        any(nodes$semantic_track[final] != expected_track)) {
      stpd_candidate_lineage_abort(
        "product_ontology_invalid",
        "final_product_type and semantic_track violate the frozen ontology."
      )
    }
  }
  invisible(TRUE)
}

stpd_candidate_lineage_node_lookup <- function(nodes, ids) {
  nodes[match(ids, nodes$candidate_node_id), , drop = FALSE]
}

stpd_candidate_lineage_validate_edges <- function(edges, nodes, events) {
  stpd_candidate_lineage_assert_table(edges, "candidate_lineage_edges")
  expected_created_sequence <- stpd_candidate_lineage_expected_created_sequence(
    nodes, edges
  )
  if (!identical(nodes$created_sequence, expected_created_sequence)) {
    stpd_candidate_lineage_abort(
      "creation_sequence_noncanonical",
      "created_sequence must be the frozen topological/radix order within scope and stage."
    )
  }
  terminal_relation <- c(
    merged_into_child = "merge", split_at_pause = "split",
    trimmed_into_child = "trim", retyped_into_child = "retype",
    superseded_by_child = "supersede"
  )
  transforming_terminal <- events$reason_code %in% names(terminal_relation) &
    events$decision == "superseded"
  if (any(transforming_terminal)) {
    terminal_rows <- events[transforming_terminal, , drop = FALSE]
    has_outbound <- vapply(seq_len(nrow(terminal_rows)), function(i) {
      any(
        edges$parent_candidate_node_id ==
          terminal_rows$candidate_node_id[i] &
        edges$stage == terminal_rows$stage[i] &
        edges$reason_code == terminal_rows$reason_code[i] &
        edges$relation == terminal_relation[[terminal_rows$reason_code[i]]]
      )
    }, logical(1))
    if (any(!has_outbound)) {
      stpd_candidate_lineage_abort(
        "terminal_edge_missing",
        paste(
          "Every transforming terminal reason requires a matching",
          "same-stage typed outbound edge."
        )
      )
    }
  }
  if (!nrow(edges)) {
    if (any(nodes$node_origin %in% c("derived", "final_product"))) {
      stpd_candidate_lineage_abort(
        "lineage_parent_missing",
        "Derived/final nodes require inbound lineage; native roots may start at any stage."
      )
    }
    return(invisible(TRUE))
  }
  stpd_candidate_lineage_require_text(edges, c(
    "lineage_edge_id", "run_id", "params_hash", "dataset_id", "train_id",
    "group_id", "fold_id", "analysis_block_id", "operation_id",
    "parent_candidate_node_id",
    "child_candidate_node_id", "relation", "stage", "reason_code"
  ), "candidate_lineage_edges")
  if (anyDuplicated(edges$lineage_edge_id) || anyDuplicated(edges[c(
    "run_id", "dataset_id", "train_id", "analysis_block_id", "operation_id",
    "parent_candidate_node_id", "child_candidate_node_id", "relation"
  )])) {
    stpd_candidate_lineage_abort(
      "primary_key_duplicate", "Lineage edge identities or meanings are duplicated."
    )
  }
  if (any(!(edges$relation %in% STPD_CANDIDATE_LINEAGE_RELATIONS))) {
    stpd_candidate_lineage_abort("enum_invalid", "Lineage relation is invalid.")
  }
  taxonomy <- stpd_candidate_lineage_reason_taxonomy()
  if (any(!(edges$reason_code %in% taxonomy$reason_code))) {
    stpd_candidate_lineage_abort(
      "reason_taxonomy_invalid", "Edge reason_code is not in the frozen taxonomy."
    )
  }
  relation_reasons <- list(
    derive = "continues",
    merge = "merged_into_child", split = "split_at_pause",
    trim = "trimmed_into_child", retype = "retyped_into_child",
    supersede = "superseded_by_child"
  )
  incompatible_reason <- vapply(seq_len(nrow(edges)), function(i) {
    !(edges$reason_code[i] %in% relation_reasons[[edges$relation[i]]])
  }, logical(1))
  if (any(incompatible_reason)) {
    stpd_candidate_lineage_abort(
      "operation_reason_invalid", "Edge relation and typed reason_code disagree."
    )
  }
  expected_stage <- match(edges$stage, STPD_CANDIDATE_LINEAGE_STAGES)
  if (anyNA(expected_stage) || any(edges$stage_order != expected_stage) ||
      anyNA(edges$operation_sequence) || any(edges$operation_sequence < 1L)) {
    stpd_candidate_lineage_abort(
      "stage_order_invalid", "Edge stage or operation sequence is invalid."
    )
  }
  if (any(!(edges$parent_candidate_node_id %in% nodes$candidate_node_id)) ||
      any(!(edges$child_candidate_node_id %in% nodes$candidate_node_id))) {
    stpd_candidate_lineage_abort(
      "foreign_key_invalid", "Every lineage endpoint must reference candidate_nodes."
    )
  }
  if (any(edges$parent_candidate_node_id == edges$child_candidate_node_id)) {
    stpd_candidate_lineage_abort("self_edge", "A lineage edge cannot be reflexive.")
  }
  parent <- stpd_candidate_lineage_node_lookup(
    nodes, edges$parent_candidate_node_id
  )
  child <- stpd_candidate_lineage_node_lookup(nodes, edges$child_candidate_node_id)
  scope <- stpd_candidate_lineage_scope_fields()
  for (field in scope) {
    if (any(edges[[field]] != parent[[field]]) ||
        any(edges[[field]] != child[[field]])) {
      stpd_candidate_lineage_abort(
        "scope_mismatch", "Edge, parent, and child must share one run/train/block scope."
      )
    }
  }
  before <- parent$created_stage_order < child$created_stage_order
  same_stage_before <- parent$created_stage_order == child$created_stage_order &
    parent$created_sequence < child$created_sequence
  if (any(!(before | same_stage_before)) ||
      any(edges$stage_order != child$created_stage_order)) {
    stpd_candidate_lineage_abort(
      "lineage_time_invalid",
      "Parents must precede children by stage or canonical same-stage creation sequence."
    )
  }
  expected_ids <- vapply(seq_len(nrow(edges)), function(i) {
    stpd_candidate_lineage_edge_id(
      edges$run_id[i], edges$params_hash[i], edges$dataset_id[i],
      edges$train_id[i], edges$group_id[i], edges$fold_id[i],
      edges$analysis_block_id[i], edges$operation_id[i],
      edges$parent_candidate_node_id[i],
      edges$child_candidate_node_id[i], edges$relation[i], edges$stage[i],
      edges$reason_code[i]
    )
  }, character(1))
  if (!identical(edges$lineage_edge_id, expected_ids)) {
    stpd_candidate_lineage_abort(
      "identity_mismatch", "lineage_edge_id is not its deterministic identity."
    )
  }
  operation_key <- paste(
    edges$run_id, edges$params_hash, edges$dataset_id, edges$train_id,
    edges$group_id, edges$fold_id, edges$analysis_block_id,
    edges$operation_id, sep = "\r"
  )
  operations <- split(seq_len(nrow(edges)), operation_key)
  for (indices in operations) {
    operation <- edges[indices, , drop = FALSE]
    if (length(unique(operation$relation)) != 1L ||
        length(unique(operation$stage)) != 1L ||
        length(unique(operation$operation_sequence)) != 1L) {
      stpd_candidate_lineage_abort(
        "operation_inconsistent", "One operation must have one relation, stage, and sequence."
      )
    }
    parents <- unique(operation$parent_candidate_node_id)
    children <- unique(operation$child_candidate_node_id)
    relation <- operation$relation[1L]
    expected_operation_id <- stpd_candidate_lineage_operation_id(
      operation$run_id[1L], operation$params_hash[1L],
      operation$dataset_id[1L], operation$train_id[1L],
      operation$group_id[1L], operation$fold_id[1L],
      operation$analysis_block_id[1L], operation$stage[1L], relation,
      parents, children, operation$reason_code[1L]
    )
    if (!identical(operation$operation_id[1L], expected_operation_id)) {
      stpd_candidate_lineage_abort(
        "operation_identity_mismatch",
        "operation_id must bind full scope, stage, relation, sorted endpoints, and reason."
      )
    }
    expected_cardinality <- switch(
      relation, merge = c(2L, 1L), split = c(1L, 2L), c(1L, 1L)
    )
    if (length(parents) < expected_cardinality[1L] ||
        length(children) < expected_cardinality[2L] ||
        (relation != "merge" && length(parents) != 1L) ||
        (relation != "split" && length(children) != 1L)) {
      stpd_candidate_lineage_abort(
        "operation_cardinality_invalid",
        paste0("Invalid ", relation, " parent/child cardinality.")
      )
    }
    p <- stpd_candidate_lineage_node_lookup(nodes, parents)
    ch <- stpd_candidate_lineage_node_lookup(nodes, children)
    operation_stage_order <- operation$stage_order[1L]
    parent_event <- events[
      events$candidate_node_id %in% parents &
        events$stage_order == operation_stage_order, , drop = FALSE
    ]
    child_event <- events[
      events$candidate_node_id %in% children &
        events$stage_order == operation_stage_order, , drop = FALSE
    ]
    parent_created_here <- p$created_stage_order == operation_stage_order
    earlier_parent_ids <- parents[!parent_created_here]
    prior_parent_event <- events[
      events$candidate_node_id %in% earlier_parent_ids &
        events$stage_order == operation_stage_order - 1L, , drop = FALSE
    ]
    live_decisions <- c("proposed", "retained", "modified", "selected")
    if (nrow(parent_event) != length(parents) ||
        nrow(child_event) != length(children) ||
        nrow(prior_parent_event) != sum(!parent_created_here) ||
        any(!(prior_parent_event$decision %in% live_decisions)) ||
        any(child_event$decision == "not_applicable")) {
      stpd_candidate_lineage_abort(
        "operation_event_invalid",
        "Operation parents must be live immediately before child creation."
      )
    }
    if (relation == "derive") {
      if (any(!(parent_event$decision %in% live_decisions))) {
        stpd_candidate_lineage_abort(
          "operation_event_invalid", "A derive parent must remain live at derivation."
        )
      }
    } else if (any(parent_event$decision != "superseded")) {
      stpd_candidate_lineage_abort(
        "operation_event_invalid",
        "A transforming parent must be superseded at child creation."
      )
    } else if (any(parent_event$reason_code != operation$reason_code[1L])) {
      stpd_candidate_lineage_abort(
        "operation_reason_invalid",
        "A transform edge reason must equal its parent's same-stage terminal reason."
      )
    }
    p_start <- parent_event$current_start_isi
    p_end <- parent_event$current_end_isi
    ch_start <- child_event$current_start_isi
    ch_end <- child_event$current_end_isi
    same_type <- length(unique(c(p$family_id, ch$family_id))) == 1L &&
      length(unique(c(p$semantic_track, ch$semantic_track))) == 1L &&
      length(unique(c(p$node_role, ch$node_role))) == 1L &&
      length(unique(c(p$candidate_class, ch$candidate_class))) == 1L
    if (relation %in% c("merge", "split", "trim") && !same_type) {
      stpd_candidate_lineage_abort(
        "operation_type_invalid", paste0(relation, " must preserve family, track, and class.")
      )
    }
    if (relation == "merge" && !(
      ch_start[1L] == min(p_start) && ch_end[1L] == max(p_end)
    )) {
      stpd_candidate_lineage_abort(
        "operation_geometry_invalid", "A merge child must span its parent envelope."
      )
    }
    if (relation == "split") {
      order_children <- order(ch_start)
      ordered_start <- ch_start[order_children]
      ordered_end <- ch_end[order_children]
      inside <- all(ordered_start >= p_start[1L]) &&
        all(ordered_end <= p_end[1L])
      nonoverlap <- all(
        head(ordered_end, -1L) < tail(ordered_start, -1L)
      )
      if (!inside || !nonoverlap) {
        stpd_candidate_lineage_abort(
          "operation_geometry_invalid", "Split children must be non-overlapping subsets."
        )
      }
    }
    if (relation == "trim") {
      inside <- ch_start >= p_start & ch_end <= p_end
      changed <- ch_start != p_start | ch_end != p_end
      if (!inside || !changed) {
        stpd_candidate_lineage_abort(
          "operation_geometry_invalid", "A trim child must be a strict parent subset."
        )
      }
    }
    if (relation == "retype") {
      same_geometry <- ch_start == p_start && ch_end == p_end
      changed_type <- ch$family_id != p$family_id |
        ch$semantic_track != p$semantic_track |
        ch$candidate_class != p$candidate_class
      if (!same_geometry || !changed_type) {
        stpd_candidate_lineage_abort(
          "operation_type_invalid", "Retype preserves geometry and changes type."
        )
      }
    }
    if (relation == "supersede" && p$terminal_decision != "superseded") {
      stpd_candidate_lineage_abort(
        "supersede_inconsistent", "A supersede parent must terminate as superseded."
      )
    }
    if (relation %in% c("derive", "supersede")) {
      overlap <- min(p_end, ch_end) - max(p_start, ch_start) + 1L
      if (overlap <= 0L) {
        stpd_candidate_lineage_abort(
          "operation_geometry_invalid",
          paste0(relation, " requires overlapping parent and child support.")
        )
      }
    }
  }
  operation_rows <- unique(edges[c(
    stpd_candidate_lineage_scope_fields(), "stage_order", "operation_id",
    "operation_sequence"
  )])
  operation_scope_stage <- interaction(
    operation_rows$run_id, operation_rows$params_hash,
    operation_rows$dataset_id, operation_rows$train_id,
    operation_rows$group_id, operation_rows$fold_id,
    operation_rows$analysis_block_id, operation_rows$stage_order,
    drop = TRUE, lex.order = TRUE
  )
  expected_operation_sequence <- integer(nrow(operation_rows))
  for (indices in split(seq_len(nrow(operation_rows)), operation_scope_stage)) {
    radix <- order(operation_rows$operation_id[indices], method = "radix")
    expected_operation_sequence[indices[radix]] <- seq_along(indices)
  }
  if (!identical(operation_rows$operation_sequence,
                 as.integer(expected_operation_sequence))) {
    stpd_candidate_lineage_abort(
      "operation_sequence_noncanonical",
      "operation_sequence must be operation_id radix order within scope and stage."
    )
  }
  adjacency <- split(edges$child_candidate_node_id,
                     edges$parent_candidate_node_id)
  state <- setNames(integer(nrow(nodes)), nodes$candidate_node_id)
  visit <- function(node_id) {
    if (state[[node_id]] == 1L) {
      stpd_candidate_lineage_abort(
        "cycle_detected", "Candidate lineage must be a DAG."
      )
    }
    if (state[[node_id]] == 2L) return(invisible(NULL))
    state[[node_id]] <<- 1L
    children <- adjacency[[node_id]]
    if (is.null(children)) children <- character()
    for (child_id in children) visit(child_id)
    state[[node_id]] <<- 2L
    invisible(NULL)
  }
  for (node_id in nodes$candidate_node_id) visit(node_id)
  final_ids <- nodes$candidate_node_id[nodes$node_role == "final_product"]
  incoming <- split(edges$parent_candidate_node_id, edges$child_candidate_node_id)
  required_inbound <- nodes$candidate_node_id[
    nodes$node_origin %in% c("derived", "final_product")
  ]
  forbidden_inbound <- nodes$candidate_node_id[nodes$node_origin == "root"]
  if (any(!(required_inbound %in% names(incoming))) ||
      any(forbidden_inbound %in% names(incoming))) {
    stpd_candidate_lineage_abort(
      "lineage_parent_missing",
      "Derived/final nodes require inbound lineage; native roots never do, at any stage."
    )
  }
  for (final_id in final_ids) {
    frontier <- incoming[[final_id]]
    ancestors <- character()
    while (length(frontier)) {
      current <- setdiff(unique(frontier), ancestors)
      if (!length(current)) break
      ancestors <- c(ancestors, current)
      frontier <- unlist(incoming[current], use.names = FALSE)
    }
    ancestor_rows <- stpd_candidate_lineage_node_lookup(nodes, ancestors)
    if (!length(ancestors) || !any(ancestor_rows$node_role == "candidate")) {
      stpd_candidate_lineage_abort(
        "final_product_orphan",
        "Every final_product node needs at least one candidate ancestor in the DAG."
      )
    }
  }
  invisible(TRUE)
}

stpd_candidate_lineage_validate_events <- function(
    events, nodes,
    expected_cap_scopes = stpd_candidate_lineage_empty_cap_policies(),
    validation_mode = c("publication_authoritative", "diagnostic_unavailable")) {
  validation_mode <- match.arg(validation_mode)
  stpd_candidate_lineage_validate_cap_policies(expected_cap_scopes)
  stpd_candidate_lineage_assert_table(events, "candidate_stage_events")
  if (!nrow(events) && nrow(nodes)) {
    stpd_candidate_lineage_abort(
      "stage_log_incomplete", "Full audit requires nine events for every node."
    )
  }
  if (!nrow(events)) return(invisible(TRUE))
  stpd_candidate_lineage_require_text(events, c(
    "stage_event_id", "run_id", "params_hash", "dataset_id", "train_id",
    "group_id", "fold_id", "analysis_block_id", "stage_scope_id",
    "candidate_node_id", "stage",
    "decision", "reason_code", "source_decision_code", "universe_status"
  ), "candidate_stage_events")
  if (anyDuplicated(events$stage_event_id) || anyDuplicated(events[c(
    "candidate_node_id", "stage"
  )])) {
    stpd_candidate_lineage_abort(
      "primary_key_duplicate", "Stage-event IDs and candidate-stage pairs must be unique."
    )
  }
  if (any(!(events$candidate_node_id %in% nodes$candidate_node_id))) {
    stpd_candidate_lineage_abort(
      "foreign_key_invalid", "Every stage event must reference candidate_nodes."
    )
  }
  node <- stpd_candidate_lineage_node_lookup(nodes, events$candidate_node_id)
  for (field in stpd_candidate_lineage_scope_fields()) {
    if (any(events[[field]] != node[[field]])) {
      stpd_candidate_lineage_abort(
        "scope_mismatch", "Stage event and node scopes must match."
      )
    }
  }
  winner <- !is.na(events$overlap_winner_candidate_node_id) &
    nzchar(events$overlap_winner_candidate_node_id)
  if (any(winner &
          !(events$overlap_winner_candidate_node_id %in% nodes$candidate_node_id))) {
    stpd_candidate_lineage_abort(
      "foreign_key_invalid", "An overlap winner must reference candidate_nodes."
    )
  }
  if (any(winner)) {
    winner_node <- stpd_candidate_lineage_node_lookup(
      nodes, events$overlap_winner_candidate_node_id[winner]
    )
    for (field in stpd_candidate_lineage_scope_fields()) {
      if (any(events[[field]][winner] != winner_node[[field]])) {
        stpd_candidate_lineage_abort(
          "scope_mismatch", "An overlap winner must share the event scope."
        )
      }
    }
  }
  overlap_lost <- events$reason_code == "overlap_lost"
  if (any(overlap_lost != winner) || any(winner & events$decision != "rejected")) {
    stpd_candidate_lineage_abort(
      "overlap_winner_invalid",
      "Exactly overlap_lost rejection events must name an overlap winner."
    )
  }
  if (any(winner)) {
    winner_events <- events[match(
      paste(events$overlap_winner_candidate_node_id[winner],
            events$stage[winner], sep = "\r"),
      paste(events$candidate_node_id, events$stage, sep = "\r")
    ), , drop = FALSE]
    loser_nodes <- node[winner, , drop = FALSE]
    winner_nodes <- stpd_candidate_lineage_node_lookup(
      nodes, events$overlap_winner_candidate_node_id[winner]
    )
    overlaps <- pmin(events$current_end_isi[winner],
                     winner_events$current_end_isi) >=
      pmax(events$current_start_isi[winner], winner_events$current_start_isi)
    if (anyNA(winner_events$candidate_node_id) ||
        any(winner_nodes$family_id != loser_nodes$family_id) ||
        any(winner_nodes$semantic_track != loser_nodes$semantic_track) ||
        any(!(winner_events$decision %in%
                c("proposed", "retained", "modified", "selected"))) ||
        any(!overlaps)) {
      stpd_candidate_lineage_abort(
        "overlap_winner_invalid",
        "An overlap winner must be same-scope, same-family/track, live at the same stage, and geometrically overlapping."
      )
    }
  }
  expected_stage <- match(events$stage, STPD_CANDIDATE_LINEAGE_STAGES)
  if (anyNA(expected_stage) || any(events$stage_order != expected_stage)) {
    stpd_candidate_lineage_abort(
      "stage_order_invalid", "Stage events violate the frozen stage order."
    )
  }
  scope_mapping <- unique(events[c(
    "stage_scope_id", "run_id", "params_hash", "dataset_id", "train_id",
    "group_id", "fold_id", "analysis_block_id", "stage"
  )])
  if (anyDuplicated(scope_mapping$stage_scope_id)) {
    stpd_candidate_lineage_abort(
      "stage_scope_invalid",
      "stage_scope_id must map to exactly one run/train/block/stage scope."
    )
  }
  if (any(!(events$decision %in% STPD_CANDIDATE_LINEAGE_DECISIONS)) ||
      any(!(events$universe_status %in%
              c("complete", "incomplete_early_stop", "not_applicable")))) {
    stpd_candidate_lineage_abort(
      "enum_invalid", "Stage decision or universe_status is invalid."
    )
  }
  taxonomy <- stpd_candidate_lineage_reason_taxonomy()
  reason_match <- match(events$reason_code, taxonomy$reason_code)
  if (anyNA(reason_match) ||
      any(events$reason_precedence != taxonomy$reason_precedence[reason_match])) {
    stpd_candidate_lineage_abort(
      "reason_taxonomy_invalid", "Reason code or precedence is not frozen taxonomy."
    )
  }
  allowed <- stpd_candidate_lineage_stage_decision_reason_contract()
  observed_key <- paste(
    events$stage, events$decision, events$reason_code, sep = "\r"
  )
  allowed_key <- paste(
    allowed$stage, allowed$decision, allowed$reason_code, sep = "\r"
  )
  if (any(!(observed_key %in% allowed_key))) {
    stpd_candidate_lineage_abort(
      "stage_decision_reason_invalid",
      "A stage/decision/reason combination violates the frozen matrix."
    )
  }
  expected_ids <- vapply(seq_len(nrow(events)), function(i) {
    stpd_candidate_lineage_stage_event_id(
      events$run_id[i], events$params_hash[i], events$stage_scope_id[i],
      events$candidate_node_id[i], events$stage[i], events$decision[i],
      events$reason_code[i], events$source_decision_code[i],
      events$current_start_isi[i], events$current_end_isi[i],
      events$overlap_winner_candidate_node_id[i],
      events$final_product_type[i], events$final_product_id[i],
      events$candidate_count[i], events$candidate_cap[i],
      events$cap_scope_member[i], events$cap_rank[i],
      events$cap_policy_id[i],
      events$universe_status[i]
    )
  }, character(1))
  if (!identical(events$stage_event_id, expected_ids)) {
    stpd_candidate_lineage_abort(
      "identity_mismatch", "stage_event_id is not its deterministic identity."
    )
  }
  if (anyNA(events$current_start_isi) || anyNA(events$current_end_isi) ||
      any(events$current_start_isi < 1L) ||
      any(events$current_end_isi < events$current_start_isi) ||
      anyNA(events$runtime_ms) || any(!is.finite(events$runtime_ms)) ||
      any(events$runtime_ms < 0) || anyNA(events$candidate_count) ||
      any(events$candidate_count < 0L) || anyNA(events$candidate_cap) ||
      any(events$candidate_cap < 0L) || anyNA(events$cap_scope_member) ||
      anyNA(events$cap_rank) || any(events$cap_rank < 0L)) {
    stpd_candidate_lineage_abort(
      "stage_value_invalid", "Stage geometry, runtime, count, cap, or membership is invalid."
    )
  }
  materialized_event <- events$decision == "materialized"
  event_product_present <- !is.na(events$final_product_id) &
    nzchar(events$final_product_id) & !is.na(events$final_product_type) &
    nzchar(events$final_product_type)
  if (any(materialized_event != event_product_present)) {
    stpd_candidate_lineage_abort(
      "final_product_invalid",
      "Exactly materialized stage events must name a final product."
    )
  }
  if (validation_mode == "publication_authoritative" &&
      (any(events$universe_status == "incomplete_early_stop") ||
       any(expected_cap_scopes$policy_status == "incomplete_early_stop"))) {
    stpd_candidate_lineage_abort(
      "universe_incomplete",
      "Incomplete early-stop lineage is unavailable for authoritative use."
    )
  }
  by_node <- split(events, events$candidate_node_id)
  for (node_id in nodes$candidate_node_id) {
    current <- by_node[[node_id]]
    current <- current[order(current$stage_order), , drop = FALSE]
    node_row <- nodes[nodes$candidate_node_id == node_id, , drop = FALSE]
    active_geometry <- current$stage_order >= node_row$created_stage_order
    if (length(unique(current$current_start_isi[active_geometry])) != 1L ||
        length(unique(current$current_end_isi[active_geometry])) != 1L) {
      stpd_candidate_lineage_abort(
        "silent_geometry_mutation",
        "One node cannot change geometry; geometry changes require a typed child edge."
      )
    }
    if (nrow(current) != length(STPD_CANDIDATE_LINEAGE_STAGES) ||
        !identical(current$stage, STPD_CANDIDATE_LINEAGE_STAGES)) {
      stpd_candidate_lineage_abort(
        "stage_log_incomplete", "Every node requires exactly nine ordered stage events."
      )
    }
    before_creation <- current$stage_order < node_row$created_stage_order
    if (any(current$decision[before_creation] != "not_applicable") ||
        any(current$reason_code[before_creation] != "not_yet_created")) {
      stpd_candidate_lineage_abort(
        "pre_creation_activity",
        "Stages before node creation must explicitly be not_applicable/not_yet_created."
      )
    }
    if (current$decision[node_row$created_stage_order] == "not_applicable") {
      stpd_candidate_lineage_abort(
        "creation_event_missing", "The created stage cannot be not_applicable."
      )
    }
    terminal <- which(current$decision %in% STPD_CANDIDATE_LINEAGE_TERMINALS)
    if (length(terminal) != 1L || terminal < node_row$created_stage_order) {
      stpd_candidate_lineage_abort(
        "terminal_event_invalid", "Every created node requires one later terminal decision."
      )
    }
    after_terminal <- current$stage_order > terminal
    if (any(current$decision[after_terminal] != "not_applicable") ||
        any(current$reason_code[after_terminal] !=
              "terminal_previously_reached")) {
      stpd_candidate_lineage_abort(
        "post_terminal_activity",
        "Stages after termination must be not_applicable/terminal_previously_reached."
      )
    }
    between <- current$stage_order >= node_row$created_stage_order &
      current$stage_order < terminal
    if (any(current$decision[between] == "not_applicable")) {
      stpd_candidate_lineage_abort(
        "active_stage_missing", "A live node cannot skip an intermediate stage."
      )
    }
    terminal_event <- current[terminal, , drop = FALSE]
    if (terminal_event$current_start_isi != node_row$current_start_isi ||
        terminal_event$current_end_isi != node_row$current_end_isi) {
      stpd_candidate_lineage_abort(
        "terminal_geometry_mismatch",
        "A node's current geometry must equal its terminal-event geometry."
      )
    }
    taxonomy_row <- taxonomy[match(
      terminal_event$reason_code, taxonomy$reason_code
    ), , drop = FALSE]
    if (terminal_event$decision %in% STPD_CANDIDATE_LINEAGE_REMOVALS &&
        !taxonomy_row$terminal_eligible) {
      stpd_candidate_lineage_abort(
        "terminal_reason_invalid", "A terminal removal requires a terminal reason."
      )
    }
    if (terminal_event$decision == "truncated_by_cap" &&
        terminal_event$reason_code != "candidate_cap_rank") {
      stpd_candidate_lineage_abort(
        "cap_reason_invalid", "Cap truncation requires candidate_cap_rank."
      )
    }
    if (terminal_event$decision == "materialized" &&
        (node_row$node_role != "final_product" ||
           terminal_event$stage != "multitrack_materialized")) {
      stpd_candidate_lineage_abort(
        "materialization_stage_invalid",
        "Only a final_product node materializes at the final stage."
      )
    }
  }
  cap_rows <- events$cap_scope_member | events$candidate_count > 0L |
    events$candidate_cap > 0L | events$cap_rank > 0L |
    events$decision == "truncated_by_cap"
  if (any(cap_rows & !events$cap_scope_member) ||
      any(events$cap_scope_member & events$candidate_cap <= 0L) ||
      any(events$cap_scope_member &
            (is.na(events$cap_policy_id) | !nzchar(events$cap_policy_id))) ||
      any(events$cap_scope_member &
            !(events$universe_status %in%
                c("complete", "incomplete_early_stop"))) ||
      any(!events$cap_scope_member & events$universe_status != "not_applicable") ||
      any(!events$cap_scope_member & events$cap_rank != 0L) ||
      any(!events$cap_scope_member &
            !is.na(events$cap_policy_id) & nzchar(events$cap_policy_id))) {
    stpd_candidate_lineage_abort(
      "cap_scope_invalid", "Cap fields must be confined to complete explicit cap-scope members."
    )
  }
  cap_groups <- split(
    events[events$cap_scope_member, , drop = FALSE],
    events$stage_scope_id[events$cap_scope_member]
  )
  actual_scope_ids <- names(cap_groups)[vapply(cap_groups, nrow, integer(1)) > 0L]
  complete_scope_ids <- expected_cap_scopes$stage_scope_id[
    expected_cap_scopes$policy_status == "applied_complete"
  ]
  if (!all(complete_scope_ids %in% actual_scope_ids) ||
      !all(actual_scope_ids %in% expected_cap_scopes$stage_scope_id)) {
    stpd_candidate_lineage_abort(
      "cap_policy_log_mismatch",
      "Actual cap logs must exactly match externally expected cap scopes."
    )
  }
  for (group in cap_groups) {
    if (!nrow(group)) next
    group_nodes <- stpd_candidate_lineage_node_lookup(
      nodes, group$candidate_node_id
    )
    expected_policy <- expected_cap_scopes[
      expected_cap_scopes$stage_scope_id == group$stage_scope_id[1L], ,
      drop = FALSE
    ]
    incomplete_policy <- nrow(expected_policy) == 1L &&
      expected_policy$policy_status == "incomplete_early_stop"
    policy_scope_fields <- c(
      stpd_candidate_lineage_scope_fields(), "family_id", "semantic_track"
    )
    policy_scope_matches <- nrow(expected_policy) == 1L && all(vapply(
      policy_scope_fields,
      function(field) identical(
        expected_policy[[field]][1L], group_nodes[[field]][1L]
      ), logical(1)
    ))
    if (nrow(expected_policy) != 1L ||
        !policy_scope_matches ||
        length(unique(group$cap_policy_id)) != 1L ||
        group$cap_policy_id[1L] != expected_policy$policy_id ||
        group$candidate_cap[1L] != expected_policy$candidate_cap ||
        group$stage[1L] != expected_policy$stage ||
        group$stage_order[1L] != expected_policy$stage_order ||
        any(group$universe_status != if (incomplete_policy) {
          "incomplete_early_stop"
        } else "complete")) {
      stpd_candidate_lineage_abort(
        "cap_policy_log_mismatch",
        "Cap log values disagree with the external expected policy."
      )
    }
    if (!incomplete_policy && (
      length(unique(group$stage)) != 1L ||
        length(unique(group$candidate_count)) != 1L ||
        length(unique(group$candidate_cap)) != 1L ||
        nrow(group) != group$candidate_count[1L]
    )) {
      stpd_candidate_lineage_abort(
        "cap_log_incomplete",
        "A complete cap scope must close over its candidate universe."
      )
    }
    if (length(unique(group_nodes$family_id)) != 1L ||
        length(unique(group_nodes$semantic_track)) != 1L ||
        any(group_nodes$node_role != "candidate")) {
      stpd_candidate_lineage_abort(
        "cap_scope_invalid",
        "A cap scope must contain one candidate family and semantic track."
      )
    }
    stage_order <- group$stage_order[1L]
    stage_events <- events[
      events$stage_order == stage_order &
        events$decision != "not_applicable", , drop = FALSE
    ]
    stage_nodes <- stpd_candidate_lineage_node_lookup(
      nodes, stage_events$candidate_node_id
    )
    stage_context <- stage_nodes$run_id == group_nodes$run_id[1L] &
      stage_nodes$params_hash == group_nodes$params_hash[1L] &
      stage_nodes$dataset_id == group_nodes$dataset_id[1L] &
      stage_nodes$train_id == group_nodes$train_id[1L] &
      stage_nodes$group_id == group_nodes$group_id[1L] &
      stage_nodes$fold_id == group_nodes$fold_id[1L] &
      stage_nodes$analysis_block_id == group_nodes$analysis_block_id[1L] &
      stage_nodes$family_id == group_nodes$family_id[1L] &
      stage_nodes$semantic_track == group_nodes$semantic_track[1L] &
      stage_nodes$node_role == "candidate"
    expected_universe <- stage_events$candidate_node_id[stage_context]
    if (!incomplete_policy && !identical(
      sort(unique(group$candidate_node_id), method = "radix"),
      sort(unique(expected_universe), method = "radix")
    )) {
      stpd_candidate_lineage_abort(
        "cap_universe_mismatch",
        "Cap membership must equal the preceding live scoped universe."
      )
    }
    if (incomplete_policy) next
    expected_truncated <- max(
      group$candidate_count[1L] - group$candidate_cap[1L], 0L
    )
    if (sum(group$decision == "truncated_by_cap") != expected_truncated) {
      stpd_candidate_lineage_abort(
        "cap_log_incomplete", "Cap truncation count does not close its universe."
      )
    }
    if (!identical(sort(group$cap_rank), seq_len(nrow(group))) || any(
      (group$decision == "truncated_by_cap") !=
        (group$cap_rank > group$candidate_cap[1L])
    )) {
      stpd_candidate_lineage_abort(
        "cap_rank_invalid",
        "Stage-local cap_rank must be a complete permutation and determine truncation."
      )
    }
  }
  invisible(TRUE)
}

stpd_candidate_lineage_derive_node_terminal <- function(events, node_id) {
  current <- events[events$candidate_node_id == node_id, , drop = FALSE]
  current <- current[order(current$stage_order), , drop = FALSE]
  terminal <- which(current$decision %in% STPD_CANDIDATE_LINEAGE_TERMINALS)
  if (length(terminal) != 1L) {
    stpd_candidate_lineage_abort(
      "terminal_event_invalid", "Cannot derive one node terminal event."
    )
  }
  event <- current[terminal, , drop = FALSE]
  removed <- event$decision %in% STPD_CANDIDATE_LINEAGE_REMOVALS
  list(
    terminal_decision = event$decision,
    node_first_failure_stage = if (removed) event$stage else NA_character_,
    node_first_failure_reason = if (removed) event$reason_code else NA_character_,
    final_product_type = if (event$decision == "materialized") {
      event$final_product_type
    } else NA_character_,
    final_product_id = if (event$decision == "materialized") {
      event$final_product_id
    } else NA_character_
  )
}

stpd_candidate_lineage_sort_bundle <- function(bundle) {
  nodes <- bundle$candidate_nodes
  if (nrow(nodes)) {
    nodes <- nodes[order(
      nodes$run_id, nodes$dataset_id, nodes$train_id,
      nodes$analysis_block_id, nodes$created_stage_order,
      nodes$created_sequence, nodes$candidate_node_id, method = "radix"
    ), , drop = FALSE]
  }
  edges <- bundle$candidate_lineage_edges
  if (nrow(edges)) {
    edges <- edges[order(
      edges$run_id, edges$dataset_id, edges$train_id,
      edges$analysis_block_id, edges$operation_sequence,
      edges$operation_id, edges$lineage_edge_id, method = "radix"
    ), , drop = FALSE]
  }
  events <- bundle$candidate_stage_events
  if (nrow(events)) {
    events <- events[order(
      events$run_id, events$dataset_id, events$train_id,
      events$analysis_block_id, events$candidate_node_id,
      events$stage_order, events$stage_event_id, method = "radix"
    ), , drop = FALSE]
  }
  rownames(nodes) <- rownames(edges) <- rownames(events) <- NULL
  list(
    candidate_nodes = nodes, candidate_lineage_edges = edges,
    candidate_stage_events = events
  )
}

stpd_candidate_lineage_validate_bundle <- function(
    bundle, audit_level = "full",
    expected_cap_scopes = NULL,
    validation_mode = c("publication_authoritative", "diagnostic_unavailable"),
    source_adapters = stpd_candidate_lineage_empty_source_adapters()) {
  validation_mode <- match.arg(validation_mode)
  if (!is.character(audit_level) || length(audit_level) != 1L ||
      is.na(audit_level) ||
      !(audit_level %in% STPD_CANDIDATE_LINEAGE_AUDIT_LEVELS)) {
    stpd_candidate_lineage_abort(
      "audit_level_invalid", "audit_level must be off, summary, or full."
    )
  }
  if (validation_mode == "publication_authoritative" &&
      audit_level != "full") {
    stpd_candidate_lineage_abort(
      "publication_authority_requires_full_audit",
      paste(
        "Publication-authoritative lineage requires a full audit;",
        "off and summary materializations are diagnostic-unavailable only."
      )
    )
  }
  required <- c(
    "candidate_nodes", "candidate_lineage_edges", "candidate_stage_events"
  )
  if (!is.list(bundle) || !identical(names(bundle), required)) {
    stpd_candidate_lineage_abort(
      "bundle_invalid", "Lineage bundle fields are invalid or out of order."
    )
  }
  for (name in required) stpd_candidate_lineage_assert_table(bundle[[name]], name)
  if (audit_level == "full" && is.null(expected_cap_scopes)) {
    stpd_candidate_lineage_abort(
      "cap_policy_manifest_required",
      "A full audit requires an explicit typed cap-policy manifest, including typed empty."
    )
  }
  if (is.null(expected_cap_scopes)) {
    expected_cap_scopes <- stpd_candidate_lineage_empty_cap_policies()
  }
  if (audit_level == "off") {
    if (any(vapply(bundle, nrow, integer(1)) != 0L)) {
      stpd_candidate_lineage_abort(
        "audit_materialization_invalid", "off requires three typed empty tables."
      )
    }
    return(invisible(TRUE))
  }
  stpd_candidate_lineage_validate_nodes(bundle$candidate_nodes, source_adapters)
  if (audit_level == "summary") {
    if (nrow(bundle$candidate_lineage_edges) ||
        nrow(bundle$candidate_stage_events)) {
      stpd_candidate_lineage_abort(
        "audit_materialization_invalid", "summary materializes nodes only."
      )
    }
    return(invisible(TRUE))
  }
  stpd_candidate_lineage_validate_events(
    bundle$candidate_stage_events, bundle$candidate_nodes,
    expected_cap_scopes, validation_mode
  )
  stpd_candidate_lineage_validate_edges(
    bundle$candidate_lineage_edges, bundle$candidate_nodes,
    bundle$candidate_stage_events
  )
  for (i in seq_len(nrow(bundle$candidate_nodes))) {
    derived <- stpd_candidate_lineage_derive_node_terminal(
      bundle$candidate_stage_events, bundle$candidate_nodes$candidate_node_id[i]
    )
    fields <- names(derived)
    actual <- lapply(fields, function(field) bundle$candidate_nodes[[field]][i])
    names(actual) <- fields
    if (!identical(actual, derived)) {
      stpd_candidate_lineage_abort(
        "derived_field_mismatch",
        "Node terminal/failure/product fields must derive from its stage log."
      )
    }
  }
  invisible(TRUE)
}

stpd_candidate_lineage_canonicalize_sequences <- function(nodes, edges) {
  if (nrow(nodes)) {
    nodes$created_sequence <- stpd_candidate_lineage_expected_created_sequence(
      nodes, edges
    )
  }
  if (nrow(edges)) {
    operation_rows <- unique(edges[c(
      stpd_candidate_lineage_scope_fields(), "stage_order", "operation_id"
    )])
    operation_scope_stage <- interaction(
      operation_rows$run_id, operation_rows$params_hash,
      operation_rows$dataset_id, operation_rows$train_id,
      operation_rows$group_id, operation_rows$fold_id,
      operation_rows$analysis_block_id, operation_rows$stage_order,
      drop = TRUE, lex.order = TRUE
    )
    operation_rows$canonical_sequence <- 0L
    for (indices in split(seq_len(nrow(operation_rows)), operation_scope_stage)) {
      radix <- order(operation_rows$operation_id[indices], method = "radix")
      operation_rows$canonical_sequence[indices[radix]] <- seq_along(indices)
    }
    operation_key <- paste(
      operation_rows$run_id, operation_rows$params_hash,
      operation_rows$dataset_id, operation_rows$train_id,
      operation_rows$group_id, operation_rows$fold_id,
      operation_rows$analysis_block_id, operation_rows$stage_order,
      operation_rows$operation_id, sep = "\r"
    )
    edge_key <- paste(
      edges$run_id, edges$params_hash, edges$dataset_id, edges$train_id,
      edges$group_id, edges$fold_id, edges$analysis_block_id,
      edges$stage_order, edges$operation_id, sep = "\r"
    )
    edges$operation_sequence <- as.integer(
      operation_rows$canonical_sequence[match(edge_key, operation_key)]
    )
  }
  list(candidate_nodes = nodes, candidate_lineage_edges = edges)
}

stpd_candidate_lineage_build <- function(
    candidate_nodes, candidate_lineage_edges, candidate_stage_events,
    audit_level = "full",
    expected_cap_scopes = NULL,
    validation_mode = c("publication_authoritative", "diagnostic_unavailable"),
    source_adapters = stpd_candidate_lineage_empty_source_adapters()) {
  validation_mode <- match.arg(validation_mode)
  raw <- list(
    candidate_nodes = candidate_nodes,
    candidate_lineage_edges = candidate_lineage_edges,
    candidate_stage_events = candidate_stage_events
  )
  for (name in names(raw)) stpd_candidate_lineage_assert_table(raw[[name]], name)
  canonical <- stpd_candidate_lineage_canonicalize_sequences(
    candidate_nodes, candidate_lineage_edges
  )
  candidate_nodes <- canonical$candidate_nodes
  candidate_lineage_edges <- canonical$candidate_lineage_edges
  if (nrow(candidate_nodes)) {
    for (i in seq_len(nrow(candidate_nodes))) {
      derived <- stpd_candidate_lineage_derive_node_terminal(
        candidate_stage_events, candidate_nodes$candidate_node_id[i]
      )
      for (field in names(derived)) candidate_nodes[[field]][i] <- derived[[field]]
    }
  }
  full <- stpd_candidate_lineage_sort_bundle(list(
    candidate_nodes = candidate_nodes,
    candidate_lineage_edges = candidate_lineage_edges,
    candidate_stage_events = candidate_stage_events
  ))
  stpd_candidate_lineage_validate_bundle(
    full, "full", expected_cap_scopes, validation_mode, source_adapters
  )
  out <- switch(
    audit_level,
    off = stpd_candidate_lineage_empty_bundle(),
    summary = list(
      candidate_nodes = full$candidate_nodes,
      candidate_lineage_edges = stpd_candidate_lineage_empty_table(
        "candidate_lineage_edges"
      ),
      candidate_stage_events = stpd_candidate_lineage_empty_table(
        "candidate_stage_events"
      )
    ),
    full = full,
    stpd_candidate_lineage_abort(
      "audit_level_invalid", "audit_level must be off, summary, or full."
    )
  )
  stpd_candidate_lineage_validate_bundle(
    out, audit_level,
    if (audit_level == "full") expected_cap_scopes else
      stpd_candidate_lineage_empty_cap_policies(), validation_mode,
    source_adapters
  )
  out
}

stpd_candidate_lineage_validate_authoritative_products <- function(
    bundle, authoritative_products,
    expected_cap_scopes = NULL,
    source_adapters = stpd_candidate_lineage_empty_source_adapters()) {
  stpd_candidate_lineage_validate_bundle(
    bundle, "full", expected_cap_scopes,
    source_adapters = source_adapters
  )
  schema <- stpd_candidate_lineage_authoritative_product_schema()
  stpd_candidate_lineage_assert_external_schema(
    authoritative_products, schema, "authoritative_product"
  )
  if (nrow(authoritative_products)) {
    stpd_candidate_lineage_require_text(
      authoritative_products, names(schema)[schema == "character"],
      "authoritative_product"
    )
    if (any(!grepl("^[0-9a-f]{64}$", authoritative_products$params_hash)) ||
        any(!grepl("^[0-9a-f]{64}$",
                  authoritative_products$product_payload_sha256)) ||
        any(!grepl("^[0-9a-f]{64}$",
                  authoritative_products$direct_support_sha256)) ||
        anyNA(authoritative_products$start_isi) ||
        anyNA(authoritative_products$end_isi) ||
        anyNA(authoritative_products$direct_support_isi_count) ||
        any(authoritative_products$direct_support_isi_count < 1L) ||
        any(authoritative_products$start_isi < 1L) ||
        any(authoritative_products$end_isi <
              authoritative_products$start_isi)) {
      stpd_candidate_lineage_abort(
        "authoritative_product_invalid",
        "Authoritative product hashes or geometry are invalid."
      )
    }
    replayed_support <- lapply(
      authoritative_products$direct_support_isi_indices,
      stpd_candidate_lineage_decode_direct_support
    )
    replayed_count <- vapply(replayed_support, length, integer(1))
    replayed_hash <- vapply(
      replayed_support, stpd_candidate_lineage_direct_support_sha256,
      character(1)
    )
    inside <- vapply(seq_along(replayed_support), function(i) all(
      replayed_support[[i]] >= authoritative_products$start_isi[i] &
        replayed_support[[i]] <= authoritative_products$end_isi[i]
    ), logical(1))
    if (any(replayed_count !=
            authoritative_products$direct_support_isi_count) ||
        any(replayed_hash != authoritative_products$direct_support_sha256) ||
        any(!inside)) {
      stpd_candidate_lineage_abort(
        "authoritative_product_support_mismatch",
        "Authoritative direct-support indices do not replay to hash/count within the product envelope."
      )
    }
    ontology <- stpd_candidate_lineage_product_ontology()
    expected_track <- ontology$semantic_track[match(
      authoritative_products$final_product_type,
      ontology$final_product_type
    )]
    if (anyNA(expected_track) ||
        any(authoritative_products$semantic_track != expected_track)) {
      stpd_candidate_lineage_abort(
        "product_ontology_invalid",
        "Authoritative product type and track violate the frozen ontology."
      )
    }
    if (any(!(authoritative_products$final_product_type %in%
                stpd_candidate_lineage_publication_product_types()))) {
      stpd_candidate_lineage_abort(
        "authoritative_product_invalid",
        "The publication manifest contains only Event, Gap, and State products."
      )
    }
  }
  final <- bundle$candidate_nodes[
    bundle$candidate_nodes$node_role == "final_product" &
      bundle$candidate_nodes$final_product_type %in%
        stpd_candidate_lineage_publication_product_types(), , drop = FALSE
  ]
  observed <- data.frame(
    run_id = final$run_id, params_hash = final$params_hash,
    dataset_id = final$dataset_id, train_id = final$train_id,
    group_id = final$group_id, fold_id = final$fold_id,
    analysis_block_id = final$analysis_block_id,
    final_product_type = final$final_product_type,
    final_product_id = final$final_product_id,
    semantic_track = final$semantic_track,
    candidate_class = final$candidate_class,
    start_isi = final$current_start_isi, end_isi = final$current_end_isi,
    product_payload_sha256 = final$product_payload_sha256,
    direct_support_sha256 = final$direct_support_sha256,
    direct_support_isi_indices = final$direct_support_isi_indices,
    direct_support_isi_count = final$direct_support_isi_count,
    stringsAsFactors = FALSE, check.names = FALSE
  )
  sort_products <- function(x) {
    if (nrow(x)) x <- x[order(
      x$run_id, x$dataset_id, x$train_id, x$analysis_block_id,
      x$final_product_type, x$final_product_id, method = "radix"
    ), , drop = FALSE]
    rownames(x) <- NULL
    x
  }
  if (!identical(sort_products(observed),
                 sort_products(authoritative_products))) {
    stpd_candidate_lineage_abort(
      "authoritative_product_mismatch",
      "Lineage final products do not exactly match authoritative products."
    )
  }
  invisible(TRUE)
}

stpd_candidate_lineage_validate_truth <- function(truth) {
  schema <- stpd_candidate_lineage_truth_schema()
  if (!is.data.frame(truth) || !identical(names(truth), names(schema))) {
    stpd_candidate_lineage_abort(
      "truth_schema_invalid", "Truth support violates its frozen schema."
    )
  }
  actual <- vapply(truth, stpd_candidate_lineage_column_type, character(1))
  if (!identical(unname(actual), unname(schema))) {
    stpd_candidate_lineage_abort(
      "truth_schema_invalid", "Truth support types violate its frozen schema."
    )
  }
  if (nrow(truth)) {
    stpd_candidate_lineage_require_text(truth, c(
      "truth_id", "run_id", "params_hash", "dataset_id", "train_id",
      "group_id", "fold_id", "analysis_block_id", "family_id"
    ), "truth_support")
    if (any(!grepl("^[0-9a-f]{64}$", truth$params_hash)) ||
        anyDuplicated(truth$truth_id) || anyDuplicated(truth[c(
          "truth_id", "run_id", "params_hash", "dataset_id", "train_id",
          "group_id", "fold_id", "analysis_block_id", "family_id"
        )]) || anyNA(truth$truth_start_isi) ||
        anyNA(truth$truth_end_isi) || any(truth$truth_start_isi < 1L) ||
        any(truth$truth_end_isi < truth$truth_start_isi)) {
      stpd_candidate_lineage_abort(
        "truth_value_invalid", "Truth IDs or geometry are invalid."
      )
    }
  }
  invisible(TRUE)
}

stpd_candidate_lineage_truth_node_first_failure <- function(
    truth, bundle, expected_cap_scopes = NULL,
    source_adapters = stpd_candidate_lineage_empty_source_adapters()) {
  stpd_candidate_lineage_validate_truth(truth)
  stpd_candidate_lineage_validate_bundle(
    bundle, "full", expected_cap_scopes,
    source_adapters = source_adapters
  )
  pair_schema <- c(
    truth_id = "character", candidate_node_id = "character",
    overlap_isi_count = "integer", node_first_failure_stage = "character",
    node_first_failure_reason = "character"
  )
  summary_schema <- c(
    truth_id = "character", entry_stage = "character",
    truth_first_failure_stage = "character",
    truth_first_failure_reason = "character",
    last_live_candidate_node_ids = "character"
  )
  nodes <- bundle$candidate_nodes
  events <- bundle$candidate_stage_events
  edges <- bundle$candidate_lineage_edges
  taxonomy <- stpd_candidate_lineage_reason_taxonomy()
  live_decisions <- c(
    "proposed", "retained", "modified", "selected", "materialized"
  )
  overlap_count <- function(event_rows, truth_row) {
    if (!nrow(event_rows)) return(integer())
    overlap <- as.integer(pmax(
      0L, pmin(event_rows$current_end_isi, truth_row$truth_end_isi) -
        pmax(event_rows$current_start_isi, truth_row$truth_start_isi) + 1L
    ))
    node_rows <- nodes[match(
      event_rows$candidate_node_id, nodes$candidate_node_id
    ), , drop = FALSE]
    final_stage <- event_rows$stage == "multitrack_materialized" &
      node_rows$node_role == "final_product"
    if (any(final_stage)) {
      final_indices <- which(final_stage)
      overlap[final_indices] <- vapply(final_indices, function(index) {
        support <- stpd_candidate_lineage_decode_direct_support(
          node_rows$direct_support_isi_indices[index]
        )
        as.integer(sum(
          support >= truth_row$truth_start_isi &
            support <= truth_row$truth_end_isi
        ))
      }, integer(1))
    }
    overlap
  }
  choose_failure <- function(stage_events, overlap) {
    materialized <- stage_events$decision == "materialized"
    if (any(materialized) && !any(materialized & overlap > 0L)) {
      return("geometry_support_lost")
    }
    removal <- stage_events$decision %in% STPD_CANDIDATE_LINEAGE_REMOVALS &
      overlap > 0L
    if (any(removal)) {
      reasons <- stage_events$reason_code[removal]
      precedence <- taxonomy$reason_precedence[match(
        reasons, taxonomy$reason_code
      )]
      return(reasons[order(precedence, reasons, method = "radix")][1L])
    }
    if (any(stage_events$decision %in% live_decisions)) {
      return("geometry_support_lost")
    }
    "all_candidate_lineages_terminated"
  }
  pair_rows <- list()
  summary_rows <- list()
  for (i in seq_len(nrow(truth))) {
    truth_row <- truth[i, , drop = FALSE]
    scoped <- nodes$run_id == truth_row$run_id &
      nodes$params_hash == truth_row$params_hash &
      nodes$dataset_id == truth_row$dataset_id &
      nodes$train_id == truth_row$train_id &
      nodes$group_id == truth_row$group_id &
      nodes$fold_id == truth_row$fold_id &
      nodes$analysis_block_id == truth_row$analysis_block_id &
      nodes$family_id == truth_row$family_id
    scoped_ids <- nodes$candidate_node_id[scoped]
    evidence_ids <- character()
    frontier <- character()
    entry_order <- NA_integer_
    failure_order <- NA_integer_
    failure_reason <- NA_character_
    last_live <- character()
    for (stage_order in seq_along(STPD_CANDIDATE_LINEAGE_STAGES)) {
      # Each stage admits newly created, truth-overlapping target-family nodes.
      # This includes later native roots, same-family merge products, and typed
      # cross-family retype children; foreign-family ancestors remain excluded.
      new_ids <- nodes$candidate_node_id[
        scoped & nodes$created_stage_order == stage_order
      ]
      new_events <- events[
        events$candidate_node_id %in% new_ids &
          events$stage_order == stage_order &
          events$decision != "not_applicable", , drop = FALSE
      ]
      new_overlap <- overlap_count(new_events, truth_row)
      entrants <- new_events$candidate_node_id[new_overlap > 0L]
      if (length(entrants)) {
        if (is.na(entry_order)) entry_order <- stage_order
        # A later target-family entry makes an earlier empty frontier
        # provisional rather than irreversible.
        failure_order <- NA_integer_
        failure_reason <- NA_character_
      }
      relevant <- unique(c(frontier, entrants))
      repeat {
        stage_children <- edges$child_candidate_node_id[
          edges$stage_order == stage_order &
            edges$parent_candidate_node_id %in% relevant &
            edges$child_candidate_node_id %in% scoped_ids
        ]
        expanded <- unique(c(relevant, stage_children))
        if (setequal(expanded, relevant)) break
        relevant <- expanded
      }
      if (!length(relevant)) next
      stage_frontier <- events[
        events$candidate_node_id %in% relevant &
          events$stage_order == stage_order &
          events$decision != "not_applicable", , drop = FALSE
      ]
      current_overlap <- overlap_count(stage_frontier, truth_row)
      evidence_ids <- unique(c(
        evidence_ids,
        stage_frontier$candidate_node_id[current_overlap > 0L]
      ))
      live <- stage_frontier$decision %in% live_decisions &
        current_overlap > 0L
      if (any(live)) {
        frontier <- sort(unique(
          stage_frontier$candidate_node_id[live]
        ), method = "radix")
        last_live <- frontier
      } else {
        failure_order <- stage_order
        failure_reason <- choose_failure(stage_frontier, current_overlap)
        frontier <- character()
      }
    }
    for (node_id in sort(evidence_ids, method = "radix")) {
      node_row <- nodes[nodes$candidate_node_id == node_id, , drop = FALSE]
      node_events <- events[
        events$candidate_node_id == node_id &
          events$decision != "not_applicable", , drop = FALSE
      ]
      pair_rows[[length(pair_rows) + 1L]] <- data.frame(
        truth_id = truth_row$truth_id, candidate_node_id = node_id,
        overlap_isi_count = as.integer(max(overlap_count(
          node_events, truth_row
        ))),
        node_first_failure_stage = node_row$node_first_failure_stage,
        node_first_failure_reason = node_row$node_first_failure_reason,
        stringsAsFactors = FALSE, check.names = FALSE
      )
    }
    if (is.na(entry_order)) {
      failure_order <- 1L
      failure_reason <- "no_candidate_proposed"
    }
    summary_rows[[length(summary_rows) + 1L]] <- data.frame(
      truth_id = truth_row$truth_id,
      entry_stage = if (is.na(entry_order)) NA_character_ else
        STPD_CANDIDATE_LINEAGE_STAGES[entry_order],
      truth_first_failure_stage = if (is.na(failure_order)) NA_character_ else
        STPD_CANDIDATE_LINEAGE_STAGES[failure_order],
      truth_first_failure_reason = failure_reason,
      last_live_candidate_node_ids = paste(last_live, collapse = ";"),
      stringsAsFactors = FALSE, check.names = FALSE
    )
  }
  pair <- if (length(pair_rows)) do.call(rbind, pair_rows) else
    stpd_candidate_lineage_empty_from_schema(pair_schema)
  summary <- if (length(summary_rows)) do.call(rbind, summary_rows) else
    stpd_candidate_lineage_empty_from_schema(summary_schema)
  if (nrow(pair)) pair <- pair[order(
    pair$truth_id, pair$candidate_node_id, method = "radix"
  ), , drop = FALSE]
  if (nrow(summary)) summary <- summary[order(
    summary$truth_id, method = "radix"
  ), , drop = FALSE]
  rownames(pair) <- rownames(summary) <- NULL
  list(truth_node_evidence = pair, truth_first_failure = summary)
}

stpd_candidate_lineage_attach <- function(
    scientific_result, audit_bundle, authoritative_products = NULL,
    expected_cap_scopes = NULL,
    validation_mode = c("publication_authoritative", "diagnostic_unavailable"),
    source_adapters = stpd_candidate_lineage_empty_source_adapters(),
    unavailable_reason = NULL) {
  validation_mode <- match.arg(validation_mode)
  stpd_candidate_lineage_validate_source_adapters(source_adapters)
  scientific_names <- if (is.list(scientific_result)) {
    names(scientific_result)
  } else NULL
  if (!is.list(scientific_result) || is.data.frame(scientific_result) ||
      is.null(scientific_names) ||
      length(scientific_names) != length(scientific_result) ||
      anyNA(scientific_names) || any(!nzchar(scientific_names)) ||
      anyDuplicated(scientific_names)) {
    stpd_candidate_lineage_abort(
      "scientific_result_invalid",
      "scientific_result must be a uniquely and non-empty named list."
    )
  }
  if ("candidate_lineage_audit" %in% scientific_names) {
    stpd_candidate_lineage_abort(
      "audit_already_attached",
      "An existing candidate_lineage_audit cannot be silently overwritten."
    )
  }
  level <- if (all(vapply(audit_bundle, nrow, integer(1)) == 0L)) {
    "off"
  } else if (!nrow(audit_bundle$candidate_lineage_edges) &&
             !nrow(audit_bundle$candidate_stage_events)) {
    "summary"
  } else "full"
  if (validation_mode == "publication_authoritative" && level != "full") {
    stpd_candidate_lineage_abort(
      "publication_authority_requires_full_audit",
      paste(
        "Publication-authoritative attachment requires a full audit;",
        "off and summary materializations must be attached as",
        "diagnostic_unavailable with an explicit unavailable_reason."
      )
    )
  }
  if (is.null(expected_cap_scopes)) {
    if (level == "full") {
      stpd_candidate_lineage_abort(
        "cap_policy_manifest_required",
        "A full attached audit requires an explicit typed cap-policy manifest."
      )
    }
    expected_cap_scopes <- stpd_candidate_lineage_empty_cap_policies()
  }
  if (validation_mode == "publication_authoritative" &&
      is.null(authoritative_products)) {
    stpd_candidate_lineage_abort(
      "authoritative_products_required",
      "Publication attachment requires an explicit authoritative-product manifest."
    )
  }
  stpd_candidate_lineage_validate_bundle(
    audit_bundle, level,
    if (level == "full") expected_cap_scopes else
      stpd_candidate_lineage_empty_cap_policies(), validation_mode,
    source_adapters
  )
  if (!is.null(authoritative_products)) {
    if (level == "full") {
      stpd_candidate_lineage_validate_authoritative_products(
        audit_bundle, authoritative_products, expected_cap_scopes,
        source_adapters
      )
    } else {
      stpd_candidate_lineage_assert_external_schema(
        authoritative_products,
        stpd_candidate_lineage_authoritative_product_schema(),
        "authoritative_product"
      )
      if (nrow(authoritative_products)) {
        stpd_candidate_lineage_abort(
          "authoritative_product_requires_full_audit",
          "A non-empty authoritative-product manifest requires full audit."
        )
      }
    }
  }
  if (is.null(authoritative_products)) {
    authoritative_products <- stpd_candidate_lineage_empty_authoritative_products()
  }
  cap_hash <- stpd_threshold_first_hash_domain(
    "stpd-candidate-lineage-cap-manifest-v1",
    stpd_candidate_lineage_canonical_cap_manifest(expected_cap_scopes)
  )
  product_hash <- stpd_threshold_first_hash_domain(
    "stpd-candidate-lineage-product-manifest-v1",
    stpd_candidate_lineage_canonical_product_manifest(authoritative_products)
  )
  source_registry_hash <- stpd_threshold_first_hash_domain(
    "stpd-candidate-lineage-source-registry-v1",
    stpd_candidate_lineage_source_registry(source_adapters)
  )
  incomplete <- any(expected_cap_scopes$policy_status == "incomplete_early_stop")
  if (validation_mode == "publication_authoritative") {
    if (!is.null(unavailable_reason) &&
        !(length(unavailable_reason) == 1L && is.na(unavailable_reason))) {
      stpd_candidate_lineage_abort(
        "unavailable_reason_invalid",
        "Publication-authoritative evidence cannot declare an unavailable reason."
      )
    }
    effective_unavailable_reason <- NA_character_
    universe_complete <- TRUE
  } else if (incomplete) {
    if (!is.null(unavailable_reason) &&
        !identical(unavailable_reason, "incomplete_early_stop")) {
      stpd_candidate_lineage_abort(
        "unavailable_reason_invalid",
        "Incomplete early-stop cap evidence has the frozen unavailable reason incomplete_early_stop."
      )
    }
    effective_unavailable_reason <- "incomplete_early_stop"
    universe_complete <- FALSE
  } else {
    if (is.null(unavailable_reason) || length(unavailable_reason) != 1L ||
        is.na(unavailable_reason) ||
        !(unavailable_reason %in%
            setdiff(STPD_CANDIDATE_LINEAGE_UNAVAILABLE_REASONS,
                    "incomplete_early_stop"))) {
      stpd_candidate_lineage_abort(
        "unavailable_reason_required",
        "Diagnostic-unavailable evidence with a complete cap log requires one explicit frozen reason."
      )
    }
    effective_unavailable_reason <- unavailable_reason
    universe_complete <- FALSE
  }
  evidence_status_hash <- stpd_threshold_first_hash_domain(
    "stpd-candidate-lineage-evidence-status-v1",
    list(
      schema_version = STPD_CANDIDATE_LINEAGE_SCHEMA_VERSION,
      audit_level = level, evidence_authority = validation_mode,
      universe_complete = universe_complete,
      unavailable_reason = effective_unavailable_reason
    )
  )
  envelope <- list(
    metadata = list(
      schema_version = STPD_CANDIDATE_LINEAGE_SCHEMA_VERSION,
      audit_level = level,
      evidence_authority = validation_mode,
      universe_complete = universe_complete,
      unavailable_reason = effective_unavailable_reason,
      cap_manifest_sha256 = cap_hash,
      product_manifest_sha256 = product_hash,
      source_registry_sha256 = source_registry_hash,
      evidence_status_sha256 = evidence_status_hash
    ),
    expected_cap_scopes = expected_cap_scopes,
    authoritative_products = authoritative_products,
    source_adapter_registry = source_adapters,
    bundle = audit_bundle
  )
  stpd_candidate_lineage_validate_envelope(envelope)
  out <- scientific_result
  out$candidate_lineage_audit <- envelope
  out
}

stpd_candidate_lineage_validate_envelope <- function(envelope) {
  required <- c(
    "metadata", "expected_cap_scopes", "authoritative_products",
    "source_adapter_registry", "bundle"
  )
  if (!is.list(envelope) || !identical(names(envelope), required) ||
      !is.list(envelope$metadata)) {
    stpd_candidate_lineage_abort(
      "audit_envelope_invalid", "The audit envelope shape is invalid."
    )
  }
  metadata_names <- c(
    "schema_version", "audit_level", "evidence_authority",
    "universe_complete", "unavailable_reason", "cap_manifest_sha256",
    "product_manifest_sha256", "source_registry_sha256",
    "evidence_status_sha256"
  )
  if (!identical(names(envelope$metadata), metadata_names) ||
      envelope$metadata$schema_version != STPD_CANDIDATE_LINEAGE_SCHEMA_VERSION ||
      !(envelope$metadata$audit_level %in% STPD_CANDIDATE_LINEAGE_AUDIT_LEVELS) ||
      !(envelope$metadata$evidence_authority %in%
          c("publication_authoritative", "diagnostic_unavailable"))) {
    stpd_candidate_lineage_abort(
      "audit_envelope_invalid", "Audit-envelope metadata is invalid."
    )
  }
  stpd_candidate_lineage_validate_cap_policies(envelope$expected_cap_scopes)
  stpd_candidate_lineage_validate_source_adapters(
    envelope$source_adapter_registry
  )
  cap_hash <- stpd_threshold_first_hash_domain(
    "stpd-candidate-lineage-cap-manifest-v1",
    stpd_candidate_lineage_canonical_cap_manifest(
      envelope$expected_cap_scopes
    )
  )
  product_hash <- stpd_threshold_first_hash_domain(
    "stpd-candidate-lineage-product-manifest-v1",
    stpd_candidate_lineage_canonical_product_manifest(
      envelope$authoritative_products
    )
  )
  source_registry_hash <- stpd_threshold_first_hash_domain(
    "stpd-candidate-lineage-source-registry-v1",
    stpd_candidate_lineage_source_registry(envelope$source_adapter_registry)
  )
  evidence_status_hash <- stpd_threshold_first_hash_domain(
    "stpd-candidate-lineage-evidence-status-v1",
    list(
      schema_version = envelope$metadata$schema_version,
      audit_level = envelope$metadata$audit_level,
      evidence_authority = envelope$metadata$evidence_authority,
      universe_complete = envelope$metadata$universe_complete,
      unavailable_reason = envelope$metadata$unavailable_reason
    )
  )
  if (!identical(envelope$metadata$cap_manifest_sha256, cap_hash) ||
      !identical(envelope$metadata$product_manifest_sha256, product_hash) ||
      !identical(envelope$metadata$source_registry_sha256,
                 source_registry_hash) ||
      !identical(envelope$metadata$evidence_status_sha256,
                 evidence_status_hash)) {
    stpd_candidate_lineage_abort(
      "audit_manifest_hash_mismatch", "An audit manifest hash does not match its payload."
    )
  }
  incomplete <- any(
    envelope$expected_cap_scopes$policy_status == "incomplete_early_stop"
  )
  publication <- envelope$metadata$evidence_authority ==
    "publication_authoritative"
  reason <- envelope$metadata$unavailable_reason
  valid_diagnostic_reason <- length(reason) == 1L && !is.na(reason) &&
    reason %in% STPD_CANDIDATE_LINEAGE_UNAVAILABLE_REASONS
  invalid_authority <- if (publication) {
    envelope$metadata$audit_level != "full" || incomplete ||
      !identical(envelope$metadata$universe_complete, TRUE) ||
      !(length(reason) == 1L && is.na(reason))
  } else {
    !identical(envelope$metadata$universe_complete, FALSE) ||
      !valid_diagnostic_reason ||
      (incomplete && reason != "incomplete_early_stop") ||
      (!incomplete && reason == "incomplete_early_stop")
  }
  if (invalid_authority) {
    stpd_candidate_lineage_abort(
      "audit_evidence_authority_invalid", "Universe authority is misrepresented."
    )
  }
  stpd_candidate_lineage_validate_bundle(
    envelope$bundle, envelope$metadata$audit_level,
    if (envelope$metadata$audit_level == "full") {
      envelope$expected_cap_scopes
    } else NULL,
    envelope$metadata$evidence_authority,
    envelope$source_adapter_registry
  )
  if (envelope$metadata$evidence_authority == "publication_authoritative") {
    if (envelope$metadata$audit_level == "full") {
      stpd_candidate_lineage_validate_authoritative_products(
        envelope$bundle, envelope$authoritative_products,
        envelope$expected_cap_scopes, envelope$source_adapter_registry
      )
    } else {
      stpd_candidate_lineage_assert_external_schema(
        envelope$authoritative_products,
        stpd_candidate_lineage_authoritative_product_schema(),
        "authoritative_product"
      )
      if (nrow(envelope$authoritative_products)) {
        stpd_candidate_lineage_abort(
          "authoritative_product_requires_full_audit",
          "A non-empty authoritative-product manifest requires full audit."
        )
      }
    }
  } else {
    stpd_candidate_lineage_assert_external_schema(
      envelope$authoritative_products,
      stpd_candidate_lineage_authoritative_product_schema(),
      "authoritative_product"
    )
  }
  invisible(TRUE)
}

stpd_candidate_lineage_scientific_projection <- function(x) {
  if (!is.list(x)) return(x)
  if (is.null(names(x))) return(x)
  x[setdiff(names(x), "candidate_lineage_audit")]
}

stpd_candidate_lineage_scientific_projection_hash <- function(x) {
  stpd_threshold_first_hash_domain(
    "stpd-candidate-lineage-scientific-projection-v1",
    stpd_candidate_lineage_scientific_projection(x)
  )
}
