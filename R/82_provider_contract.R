# Phase A: versioned multi-provider contracts ---------------------------------

STPD_PROVIDER_BUNDLE_VERSION <- "stpd_provider_bundle_v1"
STPD_PROVIDER_COORDINATE_VERSION <- "stpd_train_row_isi_v1"
STPD_PROVIDER_ONTOLOGY_VERSION <- "stpd_provider_ontology_v1"

stpd_provider_contract_versions <- function() {
  c(
    bundle = STPD_PROVIDER_BUNDLE_VERSION,
    coordinate = STPD_PROVIDER_COORDINATE_VERSION,
    ontology = STPD_PROVIDER_ONTOLOGY_VERSION
  )
}

stpd_provider_contract_field <- function(name, type, nullable = FALSE,
                                         enum = NA_character_) {
  data.frame(
    name = as.character(name), type = as.character(type),
    nullable = as.logical(nullable), enum = as.character(enum),
    stringsAsFactors = FALSE
  )
}

stpd_provider_contract_fields <- function(...) {
  do.call(rbind, list(...))
}

stpd_provider_contract_table_specs_build <- function() {
  f <- stpd_provider_contract_field
  fs <- stpd_provider_contract_fields
  list(
    provider_runs = list(
      primary_key = "provider_run_id",
      fields = fs(
        f("schema_version", "character"),
        f("provider_run_id", "character"),
        f("provider_key", "character"),
        f("provider_kind", "character", enum = "provider_kind"),
        f("provider_version", "character"),
        f("adapter_key", "character"),
        f("adapter_version", "character"),
        f("output_role", "character", enum = "output_role"),
        f("authority_scope", "character", enum = "authority_scope"),
        f("generation_mode", "character", enum = "generation_mode"),
        f("information_access", "character", enum = "information_access"),
        f("capability_profile", "character", enum = "capability_profile"),
        f("run_status", "character", enum = "run_status"),
        f("dataset_snapshot_sha256", "character"),
        f("input_artifact_sha256", "character"),
        f("timestamp_spine_sha256", "character"),
        f("params_sha256", "character"),
        f("calibration_bundle_id", "character", TRUE),
        f("raw_output_sha256", "character"),
        f("normalized_output_sha256", "character"),
        f("provider_code_sha256", "character"),
        f("adapter_code_sha256", "character"),
        f("contract_sha256", "character"),
        f("created_utc", "character")
      )
    ),
    candidate_intervals = list(
      primary_key = "candidate_id",
      fields = fs(
        f("schema_version", "character"),
        f("candidate_id", "character"),
        f("provider_run_id", "character"),
        f("source_record_key", "character"),
        f("source_record_sha256", "character"),
        f("normalization_audit_id", "character"),
        f("train_key", "character"),
        f("train_timestamp_sha256", "character"),
        f("record_kind", "character", enum = "record_kind"),
        f("provider_decision", "character", enum = "provider_decision"),
        f("semantic_track", "character", enum = "candidate_semantic_track"),
        f("proposed_label", "character", enum = "proposed_label"),
        f("canonical_start_isi", "integer"),
        f("canonical_end_isi", "integer"),
        f("canonical_start_spike", "integer"),
        f("canonical_end_spike", "integer"),
        f("canonical_start_time_sec", "double"),
        f("canonical_end_time_sec", "double"),
        f("canonical_interval_closure", "character",
          enum = "canonical_interval_closure"),
        f("score_name", "character", TRUE),
        f("score_value", "double", TRUE),
        f("score_direction", "character", enum = "score_direction"),
        f("uncertainty_kind", "character", enum = "uncertainty_kind"),
        f("uncertainty_lower", "double", TRUE),
        f("uncertainty_upper", "double", TRUE),
        f("evidence_manifest_sha256", "character", TRUE)
      )
    ),
    thresholds = list(
      primary_key = "threshold_id",
      fields = fs(
        f("schema_version", "character"),
        f("threshold_id", "character"),
        f("provider_run_id", "character"),
        f("calibration_bundle_id", "character", TRUE),
        f("threshold_name", "character"),
        f("parameter_path", "character", TRUE),
        f("threshold_value", "double"),
        f("threshold_unit", "character", enum = "threshold_unit"),
        f("threshold_role", "character", enum = "threshold_role"),
        f("scope_type", "character", enum = "scope_type"),
        f("scope_sha256", "character"),
        f("semantic_track", "character", enum = "threshold_semantic_track"),
        f("target_label", "character", enum = "threshold_target_label"),
        f("source_kind", "character", enum = "source_kind"),
        f("source_record_sha256", "character", TRUE),
        f("comparison_operator", "character", enum = "comparison_operator"),
        f("evidence_manifest_sha256", "character", TRUE)
      )
    ),
    normalization_audit = list(
      primary_key = "normalization_audit_id",
      fields = fs(
        f("schema_version", "character"),
        f("normalization_audit_id", "character"),
        f("provider_run_id", "character"),
        f("source_record_key", "character"),
        f("source_record_sha256", "character"),
        f("train_key", "character"),
        f("source_coordinate_convention", "character",
          enum = "source_coordinate_convention"),
        f("source_index_base", "character", enum = "source_index_base"),
        f("source_interval_closure", "character",
          enum = "source_interval_closure"),
        f("source_time_unit", "character", enum = "source_time_unit"),
        f("source_start_index", "integer", TRUE),
        f("source_end_index", "integer", TRUE),
        f("source_start_time", "double", TRUE),
        f("source_end_time", "double", TRUE),
        f("transformation", "character", enum = "transformation"),
        f("tolerance_sec", "double"),
        f("canonical_start_isi", "integer", TRUE),
        f("canonical_end_isi", "integer", TRUE),
        f("canonical_start_spike", "integer", TRUE),
        f("canonical_end_spike", "integer", TRUE),
        f("canonical_start_time_sec", "double", TRUE),
        f("canonical_end_time_sec", "double", TRUE),
        f("ambiguity_detected", "logical"),
        f("normalization_status", "character", enum = "normalization_status"),
        f("error_code", "character", TRUE),
        f("error_detail", "character", TRUE)
      )
    ),
    calibration_lineage = list(
      primary_key = "calibration_lineage_id",
      fields = fs(
        f("schema_version", "character"),
        f("calibration_lineage_id", "character"),
        f("calibration_bundle_id", "character"),
        f("calibration_provider_run_id", "character"),
        f("consumer_provider_run_id", "character"),
        f("calibration_source_kind", "character",
          enum = "calibration_source_kind"),
        f("calibration_authority_scope", "character",
          enum = "calibration_authority_scope"),
        f("calibration_dataset_snapshot_sha256", "character"),
        f("parameter_bundle_sha256", "character"),
        f("threshold_set_sha256", "character"),
        f("grouping_key", "character"),
        f("calibration_group_set_sha256", "character", TRUE),
        f("evaluation_group_set_sha256", "character", TRUE),
        f("group_overlap_count", "integer"),
        f("labels_used", "logical"),
        f("reference_annotations_used", "logical"),
        f("group_separation_status", "character",
          enum = "group_separation_status"),
        f("performance_use", "character", enum = "performance_use")
      )
    )
  )
}

# Public contract accessors always build a fresh object.  This is intentional:
# packages such as data.table can mutate a returned data frame by reference, so
# sharing a cached contract object would let caller code alter provenance.
stpd_provider_contract_table_specs <- function() {
  stpd_provider_contract_table_specs_build()
}

stpd_provider_contract_enums <- function() {
  list(
    provider_kind = c("native_stpd", "mean_isi", "logisi_newbd",
                      "external_data_import"),
    output_role = c("automatic_prediction", "candidate_support",
                    "calibration_output"),
    authority_scope = c("automatic_prediction_record", "support_evidence_only",
                        "calibration_parameter_only", "none_candidate"),
    generation_mode = c("native_only", "external_only", "support_to_native"),
    information_access = c("label_blind", "manual_aware", "unknown"),
    capability_profile = c("native_composed_v1", "burst_interval_threshold_v1",
                           "external_interval_v1", "external_per_isi_v1"),
    run_status = c("complete", "rejected"),
    record_kind = c("automatic_assertion", "candidate", "support"),
    provider_decision = c("positive", "negative", "indeterminate"),
    candidate_semantic_track = c("event", "state", "gap"),
    proposed_label = c("burst", "long_burst", "broad_hfs", "hft",
                       "hf_irregular", "tonic", "pause"),
    canonical_interval_closure = "closed",
    score_direction = c("higher_is_stronger", "lower_is_stronger",
                        "not_applicable"),
    uncertainty_kind = c("none", "confidence_interval", "credible_interval",
                         "provider_interval"),
    threshold_unit = c("sec", "ms", "hz", "ratio", "count", "probability",
                       "dimensionless", "log10_sec"),
    threshold_role = c("reported", "calibration_output", "detector_input",
                       "effective_parameter"),
    scope_type = c("dataset", "recording_group", "train", "provider_run"),
    threshold_semantic_track = c("event", "state", "gap", "not_applicable"),
    threshold_target_label = c("burst", "long_burst", "broad_hfs", "hft",
                               "hf_irregular", "tonic", "pause",
                               "not_applicable"),
    source_kind = c("provider_default", "unsupervised_data_derived",
                    "manual_calibration", "external_declared",
                    "calibration_bundle"),
    comparison_operator = c("lt", "le", "gt", "ge", "eq", "range_lower",
                            "range_upper"),
    source_coordinate_convention = c("train_row_isi_index",
                                     "diff_timestamp_isi_index",
                                     "spike_index_span", "spike_time_span",
                                     "per_isi_diff_index"),
    source_index_base = c("zero", "one", "not_applicable"),
    source_interval_closure = c("closed", "left_closed_right_open",
                                "left_open_right_closed", "open", "point"),
    source_time_unit = c("s", "ms", "us", "not_applicable"),
    transformation = c("identity_train_row_one_based",
                       "diff_one_based_plus_one", "diff_zero_based_plus_two",
                       "spike_one_based_span_to_isi",
                       "spike_zero_based_span_to_isi",
                       "exact_spike_time_span_to_isi",
                       "per_isi_one_based_runs_plus_one",
                       "per_isi_zero_based_runs_plus_two"),
    normalization_status = c("normalized", "rejected"),
    calibration_source_kind = c("native_unsupervised", "mean_isi",
                                "logisi_newbd", "manual_examples",
                                "external_parameters"),
    calibration_authority_scope = "calibration_parameter_only",
    group_separation_status = c("not_required", "verified_disjoint",
                                "overlap_rejected", "unverifiable_rejected"),
    performance_use = c("label_blind_detector_performance",
                        "heldout_detector_performance_only",
                        "adjudicated_agreement_only", "not_eligible")
  )
}

stpd_provider_contract_errors <- function() {
  c(
    "schema_version_unsupported", "required_field_missing", "unknown_column",
    "type_invalid", "nullability_violation", "enum_invalid",
    "primary_key_duplicate", "foreign_key_missing", "sha256_invalid",
    "hash_mismatch", "id_mismatch", "resource_limit_exceeded",
    "provider_not_allowlisted", "adapter_not_allowlisted",
    "provider_capability_mismatch", "provider_mode_role_invalid",
    "provider_authority_invalid", "executable_import_forbidden",
    "provider_nondeterministic_output", "dataset_snapshot_mismatch",
    "train_not_found", "train_timestamp_hash_mismatch",
    "timestamp_non_monotonic", "timestamp_duplicate",
    "source_record_hash_mismatch", "coordinate_convention_missing",
    "index_base_missing", "interval_closure_missing", "time_unit_missing",
    "time_unit_unsupported", "coordinate_non_finite",
    "coordinate_non_integer", "coordinate_reversed", "coordinate_empty",
    "coordinate_out_of_range", "time_alignment_missing",
    "time_alignment_ambiguous", "time_tolerance_invalid",
    "coordinate_roundtrip_mismatch", "label_unknown", "label_track_mismatch",
    "score_contract_invalid", "uncertainty_contract_invalid",
    "threshold_non_finite", "threshold_unit_invalid",
    "threshold_scope_invalid", "threshold_operator_invalid",
    "calibration_bundle_missing", "calibration_bundle_hash_mismatch",
    "calibration_lineage_invalid", "calibration_group_overlap",
    "calibration_group_unverifiable", "label_blind_leakage",
    "adjudication_in_provider_bundle", "reference_in_provider_bundle",
    "authority_role_conflict", "estimand_authority_mismatch",
    "review_action_unsupported", "review_parent_not_found",
    "review_precondition_mismatch", "review_source_stale",
    "review_bounds_invalid", "review_hard_boundary_crossed"
  )
}

stpd_provider_contract_allowlist <- function() {
  data.frame(
    provider_key = c(
      "native_stpd", "native_stpd",
      rep("mean_isi", 3L), rep("logisi_newbd", 3L),
      rep("external_data_import", 4L)
    ),
    provider_kind = c(
      "native_stpd", "native_stpd",
      rep("mean_isi", 3L), rep("logisi_newbd", 3L),
      rep("external_data_import", 4L)
    ),
    adapter_key = c(
      "native_stpd_postcomposer_v1", "native_stpd_postcomposer_v1",
      rep("mean_isi_v1", 3L), rep("logisi_newbd_v1", 3L),
      rep("external_interval_v1", 2L), rep("external_per_isi_v1", 2L)
    ),
    adapter_version = rep("1.0.0", 12L),
    capability_profile = c(
      "native_composed_v1", "native_composed_v1",
      rep("burst_interval_threshold_v1", 6L),
      rep("external_interval_v1", 2L), rep("external_per_isi_v1", 2L)
    ),
    output_role = c(
      "automatic_prediction", "automatic_prediction",
      "automatic_prediction", "candidate_support", "calibration_output",
      "automatic_prediction", "candidate_support", "calibration_output",
      "automatic_prediction", "candidate_support",
      "automatic_prediction", "candidate_support"
    ),
    generation_mode = c(
      "native_only", "support_to_native",
      "external_only", "external_only", "support_to_native",
      "external_only", "external_only", "support_to_native",
      rep("external_only", 4L)
    ),
    stringsAsFactors = FALSE
  )
}

stpd_provider_identity_contract_build <- function() {
  list(
    canonical_json = list(
      encoding = "UTF-8",
      domain_separator = "NUL",
      auto_unbox = TRUE,
      null = "null",
      na = "null",
      digits = "NA",
      pretty = FALSE,
      factors = "character",
      named_atomic_vectors = "named JSON objects"
    ),
    payload_fields = list(
      provider_run = c(
        "schema_version", "provider_key", "provider_kind", "provider_version",
        "adapter_key", "adapter_version", "output_role", "authority_scope",
        "generation_mode", "information_access", "capability_profile",
        "run_status", "dataset_snapshot_sha256", "input_artifact_sha256",
        "timestamp_spine_sha256", "params_sha256", "calibration_bundle_id",
        "provider_code_sha256", "adapter_code_sha256", "contract_sha256"
      ),
      normalization_audit = c(
        "provider_run_id", "source_record_key", "source_record_sha256",
        "train_key", "source_coordinate_convention", "source_index_base",
        "source_interval_closure", "source_time_unit", "source_start_index",
        "source_end_index", "source_start_time", "source_end_time",
        "transformation", "tolerance_sec", "canonical_start_isi",
        "canonical_end_isi", "canonical_start_spike", "canonical_end_spike",
        "canonical_start_time_sec", "canonical_end_time_sec",
        "ambiguity_detected", "normalization_status", "error_code"
      ),
      candidate = c(
        "provider_run_id", "source_record_sha256", "normalization_audit_id",
        "train_key", "train_timestamp_sha256", "record_kind",
        "provider_decision", "semantic_track", "proposed_label",
        "canonical_start_isi", "canonical_end_isi", "canonical_start_spike",
        "canonical_end_spike", "canonical_start_time_sec",
        "canonical_end_time_sec", "canonical_interval_closure", "score_name",
        "score_value", "score_direction", "uncertainty_kind",
        "uncertainty_lower", "uncertainty_upper", "evidence_manifest_sha256"
      ),
      threshold = c(
        "provider_run_id", "threshold_name", "parameter_path",
        "threshold_value", "threshold_unit", "threshold_role", "scope_type",
        "scope_sha256", "semantic_track", "target_label", "source_kind",
        "source_record_sha256", "comparison_operator",
        "evidence_manifest_sha256"
      ),
      threshold_set = "sorted_full_threshold_ids",
      calibration_bundle = c(
        "calibration_provider_run_id", "threshold_set_sha256",
        "parameter_bundle_sha256"
      ),
      calibration_lineage = setdiff(
        stpd_provider_contract_table_specs()$calibration_lineage$fields$name,
        "calibration_lineage_id"
      ),
      normalized_output = c(
        "candidate_ids", "threshold_ids", "normalization_audit_ids"
      ),
      contract = "complete_registry_except_runtime_values"
    ),
    hash_dependencies = list(
      contract = character(),
      calibration_provider_run = "contract",
      calibration_threshold = "calibration_provider_run",
      threshold_set = "calibration_threshold",
      calibration_bundle = c("calibration_provider_run", "threshold_set"),
      native_consumer_run = c("contract", "calibration_bundle"),
      native_consumer_threshold = "native_consumer_run",
      native_consumer_audit = "native_consumer_run",
      native_consumer_candidate = c(
        "native_consumer_run", "native_consumer_audit"
      ),
      native_consumer_normalized_output = c(
        "native_consumer_threshold", "native_consumer_audit",
        "native_consumer_candidate"
      ),
      provider_output_run = "contract",
      normalization_audit = "provider_output_run",
      candidate = c("provider_output_run", "normalization_audit"),
      provider_output_threshold = "provider_output_run",
      provider_normalized_output = c(
        "candidate", "provider_output_threshold", "normalization_audit"
      ),
      calibration_normalized_output = "calibration_threshold",
      calibration_lineage = c(
        "calibration_provider_run", "calibration_bundle",
        "native_consumer_run", "threshold_set"
      )
    )
  )
}

stpd_provider_identity_contract <- function() {
  stpd_provider_identity_contract_build()
}

stpd_provider_contract_registry_build <- function() {
  authority <- c(
    automatic_prediction = "automatic_prediction_record",
    candidate_support = "support_evidence_only",
    calibration_output = "calibration_parameter_only"
  )
  track_labels <- list(
    event = c("burst", "long_burst"),
    state = c("broad_hfs", "hft", "hf_irregular", "tonic"),
    gap = "pause"
  )
  coordinate_formulas <- data.frame(
    source_coordinate_convention = c(
      "train_row_isi_index", "diff_timestamp_isi_index",
      "diff_timestamp_isi_index", "spike_index_span", "spike_index_span",
      "per_isi_diff_index", "per_isi_diff_index", "spike_time_span"
    ),
    source_index_base = c("one", "one", "zero", "one", "zero", "one",
                          "zero", "not_applicable"),
    transformation = c(
      "identity_train_row_one_based", "diff_one_based_plus_one",
      "diff_zero_based_plus_two", "spike_one_based_span_to_isi",
      "spike_zero_based_span_to_isi", "per_isi_one_based_runs_plus_one",
      "per_isi_zero_based_runs_plus_two", "exact_spike_time_span_to_isi"
    ),
    canonical_start_formula = c("a", "a+1", "a+2", "p+1", "p+2", "a+1",
                                "a+2", "p+1"),
    canonical_end_formula = c("b", "b+1", "b+2", "q", "q+1", "b+1",
                              "b+2", "q"),
    stringsAsFactors = FALSE
  )
  integer_closure_adjustments <- data.frame(
    source_interval_closure = c("closed", "left_closed_right_open",
                                "left_open_right_closed", "open"),
    start_adjustment = c(0L, 0L, 1L, 1L),
    end_adjustment = c(0L, -1L, 0L, -1L),
    stringsAsFactors = FALSE
  )
  capability_labels <- rbind(
    data.frame(
      capability_profile = "native_composed_v1",
      semantic_track = rep(names(track_labels), lengths(track_labels)),
      proposed_label = unname(unlist(track_labels, use.names = FALSE)),
      stringsAsFactors = FALSE
    ),
    data.frame(
      capability_profile = "burst_interval_threshold_v1",
      semantic_track = "event", proposed_label = "burst",
      stringsAsFactors = FALSE
    ),
    do.call(rbind, lapply(c("external_interval_v1", "external_per_isi_v1"),
                         function(profile) {
      data.frame(
        capability_profile = profile,
        semantic_track = rep(names(track_labels), lengths(track_labels)),
        proposed_label = unname(unlist(track_labels, use.names = FALSE)),
        stringsAsFactors = FALSE
      )
    }))
  )
  coordinate_source_contract <- data.frame(
    source_coordinate_convention = coordinate_formulas$source_coordinate_convention,
    source_index_base = coordinate_formulas$source_index_base,
    transformation = coordinate_formulas$transformation,
    source_field_group = ifelse(
      coordinate_formulas$source_coordinate_convention == "spike_time_span",
      "time", "index"
    ),
    allowed_time_units = ifelse(
      coordinate_formulas$source_coordinate_convention == "spike_time_span",
      "s|ms|us", "not_applicable"
    ),
    allowed_closures = ifelse(
      coordinate_formulas$source_coordinate_convention == "spike_time_span",
      "closed", "closed|left_closed_right_open|left_open_right_closed|open"
    ),
    stringsAsFactors = FALSE
  )
  list(
    versions = stpd_provider_contract_versions(),
    tables = stpd_provider_contract_table_specs(),
    enums = stpd_provider_contract_enums(),
    provider_allowlist = stpd_provider_contract_allowlist(),
    adapter_allowlist = unique(stpd_provider_contract_allowlist()$adapter_key),
    authority_by_output_role = authority,
    track_labels = track_labels,
    capability_labels = capability_labels,
    subtype_parent_labels = c(hft = "broad_hfs", hf_irregular = "broad_hfs"),
    calibration_sources_by_provider_kind = list(
      mean_isi = c("mean_isi", "manual_examples"),
      logisi_newbd = c("logisi_newbd", "manual_examples")
    ),
    calibration_threshold_source_kind = c(
      mean_isi = "unsupervised_data_derived",
      logisi_newbd = "unsupervised_data_derived",
      manual_examples = "manual_calibration"
    ),
    phase_a_authority = c(
      canonical_group_membership_verified = FALSE,
      detector_performance_claims_enabled = FALSE
    ),
    coordinate_formulas = coordinate_formulas,
    coordinate_source_contract = coordinate_source_contract,
    integer_closure_adjustments = integer_closure_adjustments,
    canonical_geometry = c(
      start_spike = "canonical_start_isi-1",
      end_spike = "canonical_end_isi",
      start_time_sec = "timestamp[canonical_start_isi-1]",
      end_time_sec = "timestamp[canonical_end_isi]",
      n_isi = "canonical_end_isi-canonical_start_isi+1",
      n_spikes = "canonical_end_isi-canonical_start_isi+2"
    ),
    hash_domains = c(
      contract = "stpd-provider-contract-v1",
      provider_run = "stpd-provider-run-v1",
      normalization_audit = "stpd-normalization-audit-v1",
      candidate = "stpd-candidate-interval-v1",
      threshold = "stpd-threshold-v1",
      threshold_set = "stpd-threshold-set-v1",
      calibration_bundle = "stpd-calibration-bundle-v1",
      calibration_lineage = "stpd-calibration-lineage-v1",
      normalized_output = "stpd-provider-normalized-output-v1"
    ),
    identity_contract = stpd_provider_identity_contract(),
    error_codes = stpd_provider_contract_errors(),
    resource_limits = c(max_rows_per_table = 1000000L,
                        max_total_rows = 3000000L)
  )
}

stpd_provider_contract_registry <- function() {
  stpd_provider_contract_registry_build()
}

stpd_provider_empty_column <- function(type) {
  switch(type,
         character = character(), integer = integer(), double = double(),
         logical = logical(), stop("Unknown provider field type.", call. = FALSE))
}

stpd_provider_bundle_prototypes_build <- function() {
  specs <- stpd_provider_contract_table_specs()
  lapply(specs, function(spec) {
    values <- lapply(spec$fields$type, stpd_provider_empty_column)
    names(values) <- spec$fields$name
    as.data.frame(values, optional = TRUE, stringsAsFactors = FALSE)
  })
}

stpd_provider_bundle_prototypes <- function() {
  stpd_provider_bundle_prototypes_build()
}

# Hot-path helpers cache only immutable field metadata.  They never return a
# cached data frame: each audit-row request allocates a new plain data frame, so
# by-reference caller mutations cannot alter future rows or the public contract.
stpd_provider_normalization_audit_fields_cached <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      cache <<- stpd_provider_identity_contract_build()$
        payload_fields$normalization_audit
    }
    cache[]
  }
})

stpd_provider_normalization_audit_prototype <- local({
  field_names <- NULL
  field_types <- NULL
  function() {
    if (is.null(field_names)) {
      fields <- stpd_provider_contract_table_specs_build()$
        normalization_audit$fields
      field_names <<- fields$name
      field_types <<- fields$type
    }
    values <- lapply(field_types, stpd_provider_empty_column)
    names(values) <- field_names
    as.data.frame(values, optional = TRUE, stringsAsFactors = FALSE)
  }
})

stpd_provider_canonicalize <- function(x) {
  if (is.data.frame(x)) {
    out <- lapply(x, stpd_provider_canonicalize)
    names(out) <- names(x)
    return(out)
  }
  if (is.list(x)) {
    out <- lapply(x, stpd_provider_canonicalize)
    names(out) <- names(x)
    return(out)
  }
  if (is.factor(x)) x <- as.character(x)
  if (!is.null(names(x))) {
    out <- lapply(seq_along(x), function(i) {
      stpd_provider_canonicalize(unname(x[[i]]))
    })
    names(out) <- names(x)
    return(out)
  }
  if (length(x) == 1L && is.na(x)) return(NULL)
  unname(x)
}

stpd_provider_hash_domain <- function(domain, payload) {
  json <- jsonlite::toJSON(
    stpd_provider_canonicalize(payload), auto_unbox = TRUE,
    null = "null", na = "null", digits = NA, pretty = FALSE
  )
  bytes <- c(charToRaw(enc2utf8(as.character(domain)[1L])), as.raw(0L),
             charToRaw(enc2utf8(as.character(json))))
  digest::digest(bytes, algo = "sha256", serialize = FALSE)
}

stpd_provider_contract_hash <- function() {
  registry <- stpd_provider_contract_registry()
  stpd_provider_hash_domain(registry$hash_domains[["contract"]], registry)
}

stpd_provider_one_row <- function(x, fields = NULL) {
  if (is.data.frame(x)) {
    if (nrow(x) != 1L) stop("Expected exactly one row.", call. = FALSE)
    x <- as.list(x[1L, , drop = FALSE])
  }
  if (!is.list(x) || is.null(names(x))) {
    stop("Expected one named provider row.", call. = FALSE)
  }
  if (!is.null(fields)) x <- x[fields]
  x
}

stpd_provider_run_id_v1 <- function(row) {
  identity <- stpd_provider_identity_contract()
  fields <- identity$payload_fields$provider_run
  stpd_provider_hash_domain("stpd-provider-run-v1",
                            stpd_provider_one_row(row, fields))
}

stpd_provider_normalization_audit_id_v1 <- function(row) {
  fields <- stpd_provider_normalization_audit_fields_cached()
  stpd_provider_hash_domain("stpd-normalization-audit-v1",
                            stpd_provider_one_row(row, fields))
}

stpd_provider_candidate_id_v1 <- function(row) {
  fields <- stpd_provider_identity_contract()$payload_fields$candidate
  stpd_provider_hash_domain("stpd-candidate-interval-v1",
                            stpd_provider_one_row(row, fields))
}

stpd_provider_row_ids_v1 <- function(rows, fields, domain) {
  if (!is.data.frame(rows)) stop("Expected provider rows.", call. = FALSE)
  if (!nrow(rows)) return(character())
  columns <- rows[fields]
  vapply(seq_len(nrow(rows)), function(i) {
    payload <- lapply(columns, `[[`, i)
    names(payload) <- fields
    stpd_provider_hash_domain(domain, payload)
  }, character(1))
}

stpd_provider_candidate_ids_v1 <- function(rows) {
  fields <- stpd_provider_identity_contract()$payload_fields$candidate
  stpd_provider_row_ids_v1(rows, fields, "stpd-candidate-interval-v1")
}

stpd_provider_threshold_id_v1 <- function(row) {
  fields <- stpd_provider_identity_contract()$payload_fields$threshold
  stpd_provider_hash_domain("stpd-threshold-v1",
                            stpd_provider_one_row(row, fields))
}

stpd_provider_threshold_set_sha256_v1 <- function(threshold_ids) {
  ids <- sort(as.character(threshold_ids), method = "radix")
  stpd_provider_hash_domain("stpd-threshold-set-v1", unname(ids))
}

stpd_provider_calibration_bundle_id_v1 <- function(calibration_provider_run_id,
                                                   threshold_set_sha256,
                                                   parameter_bundle_sha256) {
  stpd_provider_hash_domain("stpd-calibration-bundle-v1", list(
    calibration_provider_run_id = as.character(calibration_provider_run_id)[1L],
    threshold_set_sha256 = as.character(threshold_set_sha256)[1L],
    parameter_bundle_sha256 = as.character(parameter_bundle_sha256)[1L]
  ))
}

stpd_provider_calibration_lineage_id_v1 <- function(row) {
  fields <- stpd_provider_identity_contract()$payload_fields$calibration_lineage
  stpd_provider_hash_domain("stpd-calibration-lineage-v1",
                            stpd_provider_one_row(row, fields))
}

stpd_provider_normalized_output_sha256_v1 <- function(bundle, provider_run_id) {
  identity <- stpd_provider_identity_contract()
  ids <- list(
    candidate_ids = sort(as.character(bundle$candidate_intervals$candidate_id[
      bundle$candidate_intervals$provider_run_id == provider_run_id
    ]), method = "radix"),
    threshold_ids = sort(as.character(bundle$thresholds$threshold_id[
      bundle$thresholds$provider_run_id == provider_run_id
    ]), method = "radix"),
    normalization_audit_ids = sort(as.character(
      bundle$normalization_audit$normalization_audit_id[
        bundle$normalization_audit$provider_run_id == provider_run_id
      ]
    ), method = "radix")
  )
  if (!identical(names(ids), identity$payload_fields$normalized_output)) {
    stop("Normalized-output preimage does not match the identity contract.",
         call. = FALSE)
  }
  stpd_provider_hash_domain("stpd-provider-normalized-output-v1", ids)
}

stpd_provider_contract_abort <- function(code, message, table = NA_character_,
                                         row = NA_integer_, column = NA_character_,
                                         provider_run_id = NA_character_,
                                         source_record_key = NA_character_,
                                         offending_value = NULL) {
  condition <- structure(
    list(
      message = as.character(message)[1L], call = NULL, code = as.character(code)[1L],
      table = as.character(table)[1L], row = as.integer(row)[1L],
      column = as.character(column)[1L],
      provider_run_id = as.character(provider_run_id)[1L],
      source_record_key = as.character(source_record_key)[1L],
      offending_value = offending_value
    ),
    class = c(as.character(code)[1L], "stpd_provider_contract_error",
              "error", "condition")
  )
  stop(condition)
}

stpd_provider_contract_fail <- function(code, table, row = NA_integer_,
                                        column = NA_character_, value = NULL,
                                        message = code, provider_run_id = NA_character_,
                                        source_record_key = NA_character_) {
  stpd_provider_contract_abort(
    code, message, table, row, column, provider_run_id, source_record_key, value
  )
}

stpd_provider_validate_structure <- function(bundle, registry) {
  expected_tables <- names(registry$tables)
  if (!is.list(bundle) || is.null(names(bundle))) {
    stpd_provider_contract_fail("type_invalid", "bundle", message =
      "Provider bundle must be a named list.")
  }
  missing_tables <- setdiff(expected_tables, names(bundle))
  if (length(missing_tables)) {
    stpd_provider_contract_fail(
      "required_field_missing", "bundle", column = missing_tables[[1L]],
      message = "Provider bundle is missing a required table."
    )
  }
  extra_tables <- setdiff(names(bundle), expected_tables)
  if (length(extra_tables)) {
    stpd_provider_contract_fail(
      "unknown_column", "bundle", column = extra_tables[[1L]],
      message = "Provider bundle contains an unknown table."
    )
  }
  total_rows <- 0L
  for (table in expected_tables) {
    value <- bundle[[table]]
    if (!is.data.frame(value)) {
      stpd_provider_contract_fail("type_invalid", table,
                                  message = "Provider table must be a data.frame.")
    }
    total_rows <- total_rows + nrow(value)
    if (nrow(value) > registry$resource_limits[["max_rows_per_table"]]) {
      stpd_provider_contract_fail("resource_limit_exceeded", table)
    }
    fields <- registry$tables[[table]]$fields
    missing <- setdiff(fields$name, names(value))
    if (length(missing)) {
      stpd_provider_contract_fail("required_field_missing", table,
                                  column = missing[[1L]])
    }
    extra <- setdiff(names(value), fields$name)
    if (length(extra)) {
      stpd_provider_contract_fail("unknown_column", table, column = extra[[1L]])
    }
    if (!identical(names(value), fields$name)) {
      stpd_provider_contract_fail(
        "unknown_column", table,
        message = "Provider table columns must use the frozen order."
      )
    }
    for (j in seq_len(nrow(fields))) {
      column <- fields$name[[j]]
      expected_type <- fields$type[[j]]
      actual <- value[[column]]
      type_ok <- switch(
        expected_type,
        character = is.character(actual), integer = is.integer(actual),
        double = is.double(actual), logical = is.logical(actual), FALSE
      )
      if (!type_ok) {
        stpd_provider_contract_fail("type_invalid", table, column = column,
                                    value = typeof(actual))
      }
      if (!fields$nullable[[j]] && anyNA(actual)) {
        bad <- which(is.na(actual))[[1L]]
        stpd_provider_contract_fail("nullability_violation", table, bad,
                                    column, NA)
      }
      if (!fields$nullable[[j]] && is.character(actual) && any(!nzchar(actual))) {
        bad <- which(!nzchar(actual))[[1L]]
        stpd_provider_contract_fail("nullability_violation", table, bad,
                                    column, actual[[bad]])
      }
      enum_key <- fields$enum[[j]]
      if (!is.na(enum_key) && length(actual)) {
        invalid <- which(!is.na(actual) & !(actual %in% registry$enums[[enum_key]]))
        if (length(invalid)) {
          bad <- invalid[[1L]]
          code <- if (identical(column, "authority_scope") &&
                      identical(actual[[bad]], "reviewed_prediction_record")) {
            "adjudication_in_provider_bundle"
          } else if (identical(column, "authority_scope") &&
                     identical(actual[[bad]], "evaluation_reference_record")) {
            "reference_in_provider_bundle"
          } else if (column %in% c("proposed_label", "target_label")) {
            "label_unknown"
          } else if (identical(column, "threshold_unit")) {
            "threshold_unit_invalid"
          } else if (identical(column, "comparison_operator")) {
            "threshold_operator_invalid"
          } else {
            "enum_invalid"
          }
          stpd_provider_contract_fail(code, table, bad, column, actual[[bad]])
        }
      }
    }
    if (nrow(value) && any(value$schema_version != STPD_PROVIDER_BUNDLE_VERSION)) {
      bad <- which(value$schema_version != STPD_PROVIDER_BUNDLE_VERSION)[[1L]]
      stpd_provider_contract_fail("schema_version_unsupported", table, bad,
                                  "schema_version", value$schema_version[[bad]])
    }
  }
  if (total_rows > registry$resource_limits[["max_total_rows"]]) {
    stpd_provider_contract_fail("resource_limit_exceeded", "bundle")
  }
  invisible(TRUE)
}

stpd_provider_validate_sha_and_keys <- function(bundle, registry) {
  sha_pattern <- "^[0-9a-f]{64}$"
  id_columns <- list(
    provider_runs = c("provider_run_id", "dataset_snapshot_sha256",
                      "input_artifact_sha256", "timestamp_spine_sha256",
                      "params_sha256", "calibration_bundle_id",
                      "raw_output_sha256", "normalized_output_sha256",
                      "provider_code_sha256", "adapter_code_sha256",
                      "contract_sha256"),
    candidate_intervals = c("candidate_id", "provider_run_id",
                            "source_record_sha256", "normalization_audit_id",
                            "train_timestamp_sha256", "evidence_manifest_sha256"),
    thresholds = c("threshold_id", "provider_run_id", "calibration_bundle_id",
                   "scope_sha256", "source_record_sha256",
                   "evidence_manifest_sha256"),
    normalization_audit = c("normalization_audit_id", "provider_run_id",
                            "source_record_sha256"),
    calibration_lineage = c("calibration_lineage_id", "calibration_bundle_id",
                            "calibration_provider_run_id", "consumer_provider_run_id",
                            "calibration_dataset_snapshot_sha256",
                            "parameter_bundle_sha256", "threshold_set_sha256",
                            "calibration_group_set_sha256",
                            "evaluation_group_set_sha256")
  )
  for (table in names(id_columns)) {
    dat <- bundle[[table]]
    for (column in id_columns[[table]]) {
      values <- dat[[column]]
      bad <- which(!is.na(values) & !grepl(sha_pattern, values))
      if (length(bad)) {
        i <- bad[[1L]]
        stpd_provider_contract_fail("sha256_invalid", table, i, column, values[[i]])
      }
    }
    pk <- registry$tables[[table]]$primary_key
    if (nrow(dat) && anyDuplicated(dat[[pk]])) {
      i <- which(duplicated(dat[[pk]]) | duplicated(dat[[pk]], fromLast = TRUE))[[1L]]
      if (identical(table, "provider_runs")) {
        same <- dat[dat[[pk]] == dat[[pk]][[i]], , drop = FALSE]
        outputs <- unique(paste(same$raw_output_sha256,
                                same$normalized_output_sha256, sep = ":"))
        if (length(outputs) > 1L) {
          stpd_provider_contract_fail("provider_nondeterministic_output", table, i,
                                      pk, dat[[pk]][[i]])
        }
      }
      stpd_provider_contract_fail("primary_key_duplicate", table, i, pk,
                                  dat[[pk]][[i]])
    }
  }
  invisible(TRUE)
}

stpd_provider_validate_runs <- function(bundle, registry) {
  runs <- bundle$provider_runs
  if (!nrow(runs)) return(invisible(TRUE))
  expected_authority <- registry$authority_by_output_role[runs$output_role]
  bad_authority <- which(runs$run_status == "complete" &
                           runs$authority_scope != unname(expected_authority))
  if (length(bad_authority)) {
    i <- bad_authority[[1L]]
    forbidden <- runs$authority_scope[[i]]
    code <- if (identical(forbidden, "reviewed_prediction_record")) {
      "adjudication_in_provider_bundle"
    } else if (identical(forbidden, "evaluation_reference_record")) {
      "reference_in_provider_bundle"
    } else "authority_role_conflict"
    stpd_provider_contract_fail(code, "provider_runs", i, "authority_scope",
                                forbidden, provider_run_id = runs$provider_run_id[[i]])
  }
  rejected_bad <- which(runs$run_status == "rejected" &
                          runs$authority_scope != "none_candidate")
  if (length(rejected_bad)) {
    i <- rejected_bad[[1L]]
    stpd_provider_contract_fail("provider_authority_invalid", "provider_runs", i,
                                "authority_scope", runs$authority_scope[[i]])
  }
  if (any(runs$provider_version == "latest" | runs$adapter_version == "latest")) {
    i <- which(runs$provider_version == "latest" |
                 runs$adapter_version == "latest")[[1L]]
    stpd_provider_contract_fail("provider_capability_mismatch", "provider_runs", i,
                                message = "Provider and adapter versions must be fixed.")
  }
  allow <- registry$provider_allowlist
  for (i in seq_len(nrow(runs))) {
    row <- runs[i, , drop = FALSE]
    if (!(row$provider_key %in% allow$provider_key)) {
      stpd_provider_contract_fail("provider_not_allowlisted", "provider_runs", i,
                                  "provider_key", row$provider_key)
    }
    if (!(row$adapter_key %in% registry$adapter_allowlist)) {
      stpd_provider_contract_fail("adapter_not_allowlisted", "provider_runs", i,
                                  "adapter_key", row$adapter_key)
    }
    identity_match <- allow$provider_key == row$provider_key &
      allow$provider_kind == row$provider_kind & allow$adapter_key == row$adapter_key &
      allow$adapter_version == row$adapter_version
    if (!any(identity_match)) {
      stpd_provider_contract_fail("provider_capability_mismatch", "provider_runs", i,
                                  provider_run_id = row$provider_run_id)
    }
    capability_match <- identity_match &
      allow$capability_profile == row$capability_profile
    if (!any(capability_match)) {
      stpd_provider_contract_fail("provider_capability_mismatch", "provider_runs", i,
                                  "capability_profile", row$capability_profile)
    }
    full_match <- capability_match & allow$output_role == row$output_role &
      allow$generation_mode == row$generation_mode
    if (!any(full_match)) {
      stpd_provider_contract_fail("provider_mode_role_invalid", "provider_runs", i,
                                  provider_run_id = row$provider_run_id)
    }
    if (row$generation_mode == "support_to_native" &&
        row$output_role == "automatic_prediction" &&
        is.na(row$calibration_bundle_id)) {
      stpd_provider_contract_fail("calibration_bundle_missing", "provider_runs", i,
                                  "calibration_bundle_id")
    }
    expected_id <- stpd_provider_run_id_v1(row)
    if (!identical(row$provider_run_id, expected_id)) {
      stpd_provider_contract_fail("id_mismatch", "provider_runs", i,
                                  "provider_run_id", row$provider_run_id)
    }
    if (!identical(row$contract_sha256, stpd_provider_contract_hash())) {
      stpd_provider_contract_fail("hash_mismatch", "provider_runs", i,
                                  "contract_sha256", row$contract_sha256)
    }
    utc_shape <- grepl(
      "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$",
      row$created_utc
    )
    parsed_utc <- suppressWarnings(as.POSIXct(
      row$created_utc, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"
    ))
    utc_roundtrip <- if (length(parsed_utc) == 1L && !is.na(parsed_utc)) {
      identical(format(parsed_utc, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
                row$created_utc)
    } else {
      FALSE
    }
    if (!utc_shape || !utc_roundtrip) {
      stpd_provider_contract_fail("type_invalid", "provider_runs", i,
                                  "created_utc", row$created_utc)
    }
  }
  invisible(TRUE)
}

stpd_provider_validate_foreign_keys <- function(bundle) {
  run_ids <- bundle$provider_runs$provider_run_id
  audit_ids <- bundle$normalization_audit$normalization_audit_id
  checks <- list(
    candidate_intervals = list(
      provider_run_id = run_ids, normalization_audit_id = audit_ids
    ),
    thresholds = list(provider_run_id = run_ids),
    normalization_audit = list(provider_run_id = run_ids),
    calibration_lineage = list(
      calibration_provider_run_id = run_ids, consumer_provider_run_id = run_ids
    )
  )
  for (table in names(checks)) {
    dat <- bundle[[table]]
    for (column in names(checks[[table]])) {
      bad <- which(!(dat[[column]] %in% checks[[table]][[column]]))
      if (length(bad)) {
        i <- bad[[1L]]
        stpd_provider_contract_fail("foreign_key_missing", table, i, column,
                                    dat[[column]][[i]])
      }
    }
  }
  invisible(TRUE)
}

stpd_provider_validate_audit <- function(bundle, registry) {
  audit <- bundle$normalization_audit
  if (!nrow(audit)) return(invisible(TRUE))
  canonical <- c("canonical_start_isi", "canonical_end_isi",
                 "canonical_start_spike", "canonical_end_spike",
                 "canonical_start_time_sec", "canonical_end_time_sec")
  for (i in seq_len(nrow(audit))) {
    row <- audit[i, , drop = FALSE]
    source_contract <- registry$coordinate_source_contract
    contract_match <- source_contract$source_coordinate_convention ==
      row$source_coordinate_convention &
      source_contract$source_index_base == row$source_index_base &
      source_contract$transformation == row$transformation
    if (sum(contract_match) != 1L) {
      stpd_provider_contract_fail(
        "coordinate_roundtrip_mismatch", "normalization_audit", i,
        "transformation", row$transformation,
        message = paste(
          "Coordinate convention, index base, and transformation must match",
          "one frozen coordinate contract row."
        )
      )
    }
    coordinate_contract <- source_contract[which(contract_match), , drop = FALSE]
    allowed_closures <- strsplit(
      coordinate_contract$allowed_closures, "|", fixed = TRUE
    )[[1L]]
    if (!(row$source_interval_closure %in% allowed_closures)) {
      stpd_provider_contract_fail(
        "interval_closure_missing", "normalization_audit", i,
        "source_interval_closure", row$source_interval_closure
      )
    }
    allowed_units <- strsplit(
      coordinate_contract$allowed_time_units, "|", fixed = TRUE
    )[[1L]]
    if (!(row$source_time_unit %in% allowed_units)) {
      stpd_provider_contract_fail(
        "time_unit_unsupported", "normalization_audit", i,
        "source_time_unit", row$source_time_unit
      )
    }
    if (!is.finite(row$tolerance_sec) || row$tolerance_sec < 0) {
      stpd_provider_contract_fail("time_tolerance_invalid", "normalization_audit",
                                  i, "tolerance_sec", row$tolerance_sec)
    }
    if (isTRUE(row$ambiguity_detected) && row$normalization_status != "rejected") {
      stpd_provider_contract_fail("time_alignment_ambiguous", "normalization_audit",
                                  i, "ambiguity_detected", TRUE)
    }
    canonical_missing <- vapply(row[canonical], is.na, logical(1))
    if (row$normalization_status == "normalized") {
      if (any(canonical_missing) || !is.na(row$error_code) ||
          !is.na(row$error_detail)) {
        stpd_provider_contract_fail("coordinate_roundtrip_mismatch",
                                    "normalization_audit", i)
      }
      source_is_index <- coordinate_contract$source_field_group == "index"
      index_present <- !is.na(row$source_start_index) &&
        !is.na(row$source_end_index)
      time_present <- is.finite(row$source_start_time) &&
        is.finite(row$source_end_time)
      forbidden_index <- !is.na(row$source_start_index) ||
        !is.na(row$source_end_index)
      forbidden_time <- !is.na(row$source_start_time) ||
        !is.na(row$source_end_time)
      source_fields_ok <- if (source_is_index) {
        index_present && !forbidden_time && row$tolerance_sec == 0
      } else {
        time_present && !forbidden_index
      }
      if (!source_fields_ok) {
        stpd_provider_contract_fail(
          "coordinate_roundtrip_mismatch", "normalization_audit", i,
          message = paste(
            "Normalized source coordinates must use exactly the field group",
            "and tolerance permitted by the frozen coordinate contract."
          )
        )
      }
      geometry_ok <- row$canonical_start_isi >= 2L &&
        row$canonical_end_isi >= row$canonical_start_isi &&
        row$canonical_start_spike == row$canonical_start_isi - 1L &&
        row$canonical_end_spike == row$canonical_end_isi &&
        is.finite(row$canonical_start_time_sec) &&
        is.finite(row$canonical_end_time_sec) &&
        row$canonical_end_time_sec >= row$canonical_start_time_sec
      if (!geometry_ok) {
        stpd_provider_contract_fail(
          "coordinate_roundtrip_mismatch", "normalization_audit", i
        )
      }
      if (source_is_index) {
        lower_bound <- if (row$source_index_base == "zero") 0L else 1L
        if (row$source_start_index < lower_bound ||
            row$source_end_index < lower_bound) {
          stpd_provider_contract_fail(
            "coordinate_out_of_range", "normalization_audit", i
          )
        }
        adjustment <- registry$integer_closure_adjustments[
          registry$integer_closure_adjustments$source_interval_closure ==
            row$source_interval_closure, , drop = FALSE
        ]
        source_start <- row$source_start_index + adjustment$start_adjustment
        source_end <- row$source_end_index + adjustment$end_adjustment
        if (source_end < source_start) {
          stpd_provider_contract_fail(
            "coordinate_empty", "normalization_audit", i
          )
        }
        expected <- switch(
          row$transformation,
          identity_train_row_one_based = c(source_start, source_end),
          diff_one_based_plus_one = c(source_start + 1L, source_end + 1L),
          diff_zero_based_plus_two = c(source_start + 2L, source_end + 2L),
          spike_one_based_span_to_isi = c(source_start + 1L, source_end),
          spike_zero_based_span_to_isi = c(source_start + 2L, source_end + 1L),
          per_isi_one_based_runs_plus_one = c(
            source_start + 1L, source_end + 1L
          ),
          per_isi_zero_based_runs_plus_two = c(
            source_start + 2L, source_end + 2L
          ),
          NULL
        )
        if (is.null(expected) ||
            !identical(as.integer(expected), c(
              row$canonical_start_isi, row$canonical_end_isi
            ))) {
          stpd_provider_contract_fail(
            "coordinate_roundtrip_mismatch", "normalization_audit", i
          )
        }
      } else {
        unit_factor <- c(s = 1, ms = 1e-3, us = 1e-6)[[row$source_time_unit]]
        expected_time <- c(
          row$source_start_time * unit_factor,
          row$source_end_time * unit_factor
        )
        if (expected_time[[2L]] < expected_time[[1L]] ||
            any(abs(expected_time - c(
              row$canonical_start_time_sec, row$canonical_end_time_sec
            )) > row$tolerance_sec)) {
          stpd_provider_contract_fail(
            "coordinate_roundtrip_mismatch", "normalization_audit", i
          )
        }
      }
    } else {
      if (any(!canonical_missing) || is.na(row$error_code) || !nzchar(row$error_code)) {
        stpd_provider_contract_fail("coordinate_roundtrip_mismatch",
                                    "normalization_audit", i)
      }
      if (!(row$error_code %in% stpd_provider_contract_errors())) {
        stpd_provider_contract_fail("enum_invalid", "normalization_audit", i,
                                    "error_code", row$error_code)
      }
    }
    if (!identical(row$normalization_audit_id,
                   stpd_provider_normalization_audit_id_v1(row))) {
      stpd_provider_contract_fail("id_mismatch", "normalization_audit", i,
                                  "normalization_audit_id",
                                  row$normalization_audit_id)
    }
  }
  keys <- paste(audit$provider_run_id, audit$source_record_key, sep = "\r")
  if (anyDuplicated(keys)) {
    i <- which(duplicated(keys) | duplicated(keys, fromLast = TRUE))[[1L]]
    stpd_provider_contract_fail("primary_key_duplicate", "normalization_audit", i,
                                "source_record_key", audit$source_record_key[[i]])
  }
  invisible(TRUE)
}

stpd_provider_validate_candidates <- function(bundle, registry) {
  candidates <- bundle$candidate_intervals
  if (!nrow(candidates)) return(invisible(TRUE))
  audit <- bundle$normalization_audit
  n <- nrow(candidates)
  run_rows <- match(
    candidates$provider_run_id, bundle$provider_runs$provider_run_id
  )
  audit_rows <- match(
    candidates$normalization_audit_id, audit$normalization_audit_id
  )
  provider_scope <- bundle$provider_runs$authority_scope[run_rows]
  provider_capability <- bundle$provider_runs$capability_profile[run_rows]
  failure_code <- rep(NA_character_, n)
  failure_field <- rep(NA_character_, n)
  mark_failure <- function(mask, code, field = NA_character_) {
    rows <- which(is.na(failure_code) & !is.na(mask) & mask)
    if (length(rows)) {
      failure_code[rows] <<- code
      failure_field[rows] <<- field
    }
  }
  mark_failure(
    provider_scope == "calibration_parameter_only" |
      (provider_scope == "automatic_prediction_record" &
         candidates$record_kind != "automatic_assertion") |
      (provider_scope == "support_evidence_only" &
         candidates$record_kind == "automatic_assertion"),
    "authority_role_conflict", "record_kind"
  )
  allowed_track_labels <- unlist(lapply(
    names(registry$track_labels), function(track) {
      paste(track, registry$track_labels[[track]], sep = "\r")
    }
  ), use.names = FALSE)
  mark_failure(
    !(paste(candidates$semantic_track, candidates$proposed_label,
            sep = "\r") %in% allowed_track_labels),
    "label_track_mismatch", "proposed_label"
  )
  capability_allowed <- registry$capability_labels
  allowed_capabilities <- paste(
    capability_allowed$capability_profile,
    capability_allowed$semantic_track,
    capability_allowed$proposed_label,
    sep = "\r"
  )
  mark_failure(
    !(paste(
      provider_capability, candidates$semantic_track,
      candidates$proposed_label, sep = "\r"
    ) %in% allowed_capabilities),
    "provider_capability_mismatch", "proposed_label"
  )
  geometry_ok <- candidates$canonical_start_isi >= 2L &
    candidates$canonical_end_isi >= candidates$canonical_start_isi &
    candidates$canonical_start_spike == candidates$canonical_start_isi - 1L &
    candidates$canonical_end_spike == candidates$canonical_end_isi &
    is.finite(candidates$canonical_start_time_sec) &
    is.finite(candidates$canonical_end_time_sec) &
    candidates$canonical_end_time_sec >= candidates$canonical_start_time_sec
  mark_failure(!geometry_ok, "coordinate_roundtrip_mismatch")
  score_missing <- is.na(candidates$score_name) &
    is.na(candidates$score_value)
  score_present <- !is.na(candidates$score_name) &
    nzchar(candidates$score_name) & is.finite(candidates$score_value)
  mark_failure(
    (!score_missing & !score_present) |
      (score_missing & candidates$score_direction != "not_applicable") |
      (score_present & candidates$score_direction == "not_applicable"),
    "score_contract_invalid"
  )
  uncertainty_missing <- is.na(candidates$uncertainty_lower) &
    is.na(candidates$uncertainty_upper)
  uncertainty_valid <- is.finite(candidates$uncertainty_lower) &
    is.finite(candidates$uncertainty_upper) &
    candidates$uncertainty_lower <= candidates$uncertainty_upper
  mark_failure(
    (candidates$uncertainty_kind == "none" & !uncertainty_missing) |
      (candidates$uncertainty_kind != "none" & !uncertainty_valid),
    "uncertainty_contract_invalid"
  )
  audit_found <- !is.na(audit_rows)
  audit_normalized <- rep(FALSE, n)
  audit_normalized[audit_found] <-
    audit$normalization_status[audit_rows[audit_found]] == "normalized"
  mark_failure(
    !audit_found | !audit_normalized,
    "coordinate_roundtrip_mismatch", "normalization_audit_id"
  )
  comparable <- c(
    "provider_run_id", "source_record_key", "source_record_sha256",
    "train_key", "canonical_start_isi", "canonical_end_isi",
    "canonical_start_spike", "canonical_end_spike",
    "canonical_start_time_sec", "canonical_end_time_sec"
  )
  for (column in comparable) {
    mismatch <- rep(FALSE, n)
    mismatch[audit_found] <- candidates[[column]][audit_found] !=
      audit[[column]][audit_rows[audit_found]]
    mark_failure(
      mismatch, "coordinate_roundtrip_mismatch", column
    )
  }
  expected_candidate_ids <- stpd_provider_candidate_ids_v1(candidates)
  mark_failure(
    candidates$candidate_id != expected_candidate_ids,
    "id_mismatch", "candidate_id"
  )
  failed_rows <- which(!is.na(failure_code))
  if (length(failed_rows)) {
    i <- failed_rows[[1L]]
    message <- if (failure_code[[i]] == "provider_capability_mismatch") {
      "Provider capability does not authorize this track/label."
    } else {
      failure_code[[i]]
    }
    offending_value <- if (is.na(failure_field[[i]])) {
      NULL
    } else {
      candidates[[failure_field[[i]]]][[i]]
    }
    stpd_provider_contract_fail(
      failure_code[[i]], "candidate_intervals", i,
      failure_field[[i]], offending_value,
      provider_run_id = candidates$provider_run_id[[i]], message = message
    )
  }
  unique_key <- paste(
    candidates$provider_run_id, candidates$source_record_sha256,
    candidates$semantic_track, candidates$proposed_label,
    candidates$canonical_start_isi, candidates$canonical_end_isi,
    candidates$provider_decision, sep = "\r"
  )
  if (anyDuplicated(unique_key)) {
    i <- which(duplicated(unique_key) |
                 duplicated(unique_key, fromLast = TRUE))[[1L]]
    stpd_provider_contract_fail("primary_key_duplicate", "candidate_intervals", i,
                                message = "Duplicate normalized provider record.")
  }
  run_train_key <- paste(
    candidates$provider_run_id, candidates$train_key, sep = "\r"
  )
  run_train_spine_key <- paste(
    run_train_key, candidates$train_timestamp_sha256, sep = "\r"
  )
  pair_rows <- !duplicated(run_train_spine_key)
  repeated_run_train <- unique(
    run_train_key[pair_rows][duplicated(run_train_key[pair_rows])]
  )
  if (length(repeated_run_train)) {
    i <- which(run_train_key == repeated_run_train[[1L]])[[1L]]
    stpd_provider_contract_fail(
      "train_timestamp_hash_mismatch", "candidate_intervals", i,
      "train_timestamp_sha256",
      message = paste(
        "One provider run/train cannot mix multiple timestamp spines."
      )
    )
  }
  positive_subtypes <- which(
    candidates$provider_decision == "positive" &
      candidates$proposed_label %in% names(registry$subtype_parent_labels)
  )
  if (length(positive_subtypes)) {
    geometry_key <- paste(
      candidates$provider_run_id, candidates$train_key,
      candidates$train_timestamp_sha256,
      candidates$canonical_start_isi, candidates$canonical_end_isi,
      candidates$canonical_start_spike, candidates$canonical_end_spike,
      format(candidates$canonical_start_time_sec,
             digits = 17, scientific = TRUE),
      format(candidates$canonical_end_time_sec,
             digits = 17, scientific = TRUE),
      sep = "\r"
    )
    parent_rows <- which(
      candidates$semantic_track == "state" &
        candidates$proposed_label == "broad_hfs" &
        candidates$provider_decision == "positive"
    )
    parent_keys <- geometry_key[parent_rows]
    duplicated_parent_keys <- unique(parent_keys[duplicated(parent_keys)])
    unique_parent_keys <- unique(parent_keys)
    subtype_keys <- geometry_key[positive_subtypes]
    invalid_parent <- !(subtype_keys %in% unique_parent_keys) |
      subtype_keys %in% duplicated_parent_keys
    if (any(invalid_parent)) {
      i <- positive_subtypes[which(invalid_parent)[[1L]]]
      stpd_provider_contract_fail(
        "provider_capability_mismatch", "candidate_intervals", i,
        "proposed_label", candidates$proposed_label[[i]],
        message = paste(
          "A positive HFS subtype requires exactly one positive Broad-HFS",
          "parent on the same frozen support."
        )
      )
    }
    sibling_key <- paste(
      candidates$provider_run_id, candidates$train_key,
      candidates$canonical_start_isi, candidates$canonical_end_isi,
      sep = "\r"
    )
    hft_keys <- unique(sibling_key[
      candidates$provider_decision == "positive" &
        candidates$proposed_label == "hft"
    ])
    hfi_keys <- unique(sibling_key[
      candidates$provider_decision == "positive" &
        candidates$proposed_label == "hf_irregular"
    ])
    conflicting_keys <- intersect(hft_keys, hfi_keys)
    invalid_sibling <- sibling_key[positive_subtypes] %in% conflicting_keys
    if (any(invalid_sibling)) {
      i <- positive_subtypes[which(invalid_sibling)[[1L]]]
      stpd_provider_contract_fail(
        "provider_capability_mismatch", "candidate_intervals", i,
        "proposed_label", candidates$proposed_label[[i]],
        message = paste(
          "One Broad-HFS support cannot have both HFT and HF-irregular",
          "positive subtypes."
        )
      )
    }
  }
  invisible(TRUE)
}

stpd_provider_validate_thresholds <- function(bundle, registry) {
  thresholds <- bundle$thresholds
  if (!nrow(thresholds)) return(invisible(TRUE))
  runs <- bundle$provider_runs
  for (i in seq_len(nrow(thresholds))) {
    row <- thresholds[i, , drop = FALSE]
    if (!is.finite(row$threshold_value)) {
      stpd_provider_contract_fail("threshold_non_finite", "thresholds", i,
                                  "threshold_value", row$threshold_value)
    }
    if (row$threshold_role %in% c("detector_input", "effective_parameter") &&
        (is.na(row$parameter_path) || !nzchar(row$parameter_path))) {
      stpd_provider_contract_fail("calibration_lineage_invalid", "thresholds", i,
                                  "parameter_path")
    }
    if (row$semantic_track == "not_applicable") {
      if (row$target_label != "not_applicable") {
        stpd_provider_contract_fail("label_track_mismatch", "thresholds", i,
                                    "target_label", row$target_label)
      }
    } else if (!(row$target_label %in% registry$track_labels[[row$semantic_track]])) {
      stpd_provider_contract_fail("label_track_mismatch", "thresholds", i,
                                  "target_label", row$target_label)
    }
    producer <- runs[runs$provider_run_id == row$provider_run_id, , drop = FALSE]
    if (row$semantic_track != "not_applicable") {
      capability_allowed <- registry$capability_labels
      capability_match <- capability_allowed$capability_profile ==
        producer$capability_profile &
        capability_allowed$semantic_track == row$semantic_track &
        capability_allowed$proposed_label == row$target_label
      if (!any(capability_match)) {
        stpd_provider_contract_fail(
          "provider_capability_mismatch", "thresholds", i,
          "target_label", row$target_label,
          provider_run_id = row$provider_run_id,
          message = "Provider capability does not authorize this threshold label."
        )
      }
    }
    if (row$threshold_role == "calibration_output" &&
        producer$authority_scope != "calibration_parameter_only") {
      stpd_provider_contract_fail("authority_role_conflict", "thresholds", i,
                                  "threshold_role", row$threshold_role)
    }
    if (row$source_kind == "manual_calibration" &&
        (is.na(row$calibration_bundle_id) ||
         !(row$calibration_bundle_id %in%
             bundle$calibration_lineage$calibration_bundle_id))) {
      stpd_provider_contract_fail("calibration_lineage_invalid", "thresholds", i,
                                  "calibration_bundle_id")
    }
    record_derived <- row$source_kind %in% c(
      "unsupervised_data_derived", "manual_calibration", "external_declared"
    )
    if (record_derived && is.na(row$source_record_sha256)) {
      stpd_provider_contract_fail(
        "source_record_hash_mismatch", "thresholds", i,
        "source_record_sha256",
        message = "Record-derived thresholds require a source-record hash."
      )
    }
    if (row$source_kind == "calibration_bundle" &&
        is.na(row$calibration_bundle_id)) {
      stpd_provider_contract_fail(
        "calibration_bundle_missing", "thresholds", i,
        "calibration_bundle_id"
      )
    }
    calibrated_effective <- row$threshold_role %in%
      c("detector_input", "effective_parameter") &&
      producer$generation_mode == "support_to_native"
    if (calibrated_effective &&
        (row$source_kind != "calibration_bundle" ||
         !identical(row$calibration_bundle_id, producer$calibration_bundle_id))) {
      stpd_provider_contract_fail(
        "calibration_lineage_invalid", "thresholds", i,
        "calibration_bundle_id",
        message = paste(
          "A calibrated native detector input/effective parameter must resolve",
          "to that consumer run's calibration bundle."
        )
      )
    }
    if (!is.na(row$calibration_bundle_id) &&
        !(row$calibration_bundle_id %in%
            bundle$calibration_lineage$calibration_bundle_id)) {
      stpd_provider_contract_fail("calibration_bundle_missing", "thresholds", i,
                                  "calibration_bundle_id",
                                  row$calibration_bundle_id)
    }
    if (row$threshold_role == "calibration_output" &&
        !is.na(row$calibration_bundle_id)) {
      matching_lineage <- bundle$calibration_lineage[
        bundle$calibration_lineage$calibration_bundle_id ==
          row$calibration_bundle_id &
          bundle$calibration_lineage$calibration_provider_run_id ==
          row$provider_run_id, , drop = FALSE
      ]
      if (nrow(matching_lineage) != 1L) {
        stpd_provider_contract_fail(
          "calibration_lineage_invalid", "thresholds", i,
          "calibration_bundle_id"
        )
      }
      expected_source <- unname(
        registry$calibration_threshold_source_kind[
          matching_lineage$calibration_source_kind
        ]
      )
      if (length(expected_source) != 1L || is.na(expected_source) ||
          row$source_kind != expected_source) {
        stpd_provider_contract_fail(
          "calibration_lineage_invalid", "thresholds", i, "source_kind",
          row$source_kind,
          message = paste(
            "Calibration threshold provenance does not match the declared",
            "calibration source."
          )
        )
      }
    }
    if (!identical(row$threshold_id, stpd_provider_threshold_id_v1(row))) {
      stpd_provider_contract_fail("id_mismatch", "thresholds", i,
                                  "threshold_id", row$threshold_id)
    }
  }
  unique_key <- paste(
    thresholds$provider_run_id, thresholds$threshold_name,
    thresholds$threshold_role, thresholds$scope_type, thresholds$scope_sha256,
    thresholds$semantic_track, thresholds$target_label, sep = "\r"
  )
  if (anyDuplicated(unique_key)) {
    i <- which(duplicated(unique_key) |
                 duplicated(unique_key, fromLast = TRUE))[[1L]]
    stpd_provider_contract_fail("primary_key_duplicate", "thresholds", i,
                                message = "Duplicate scoped threshold record.")
  }
  invisible(TRUE)
}

stpd_provider_validate_lineage <- function(bundle, registry) {
  lineage <- bundle$calibration_lineage
  runs <- bundle$provider_runs
  thresholds <- bundle$thresholds
  consumers <- runs$run_status == "complete" &
    runs$output_role == "automatic_prediction" &
    runs$generation_mode == "support_to_native"
  if (any(consumers)) {
    for (run_id in runs$provider_run_id[consumers]) {
      bundle_id <- runs$calibration_bundle_id[runs$provider_run_id == run_id]
      matching <- lineage$consumer_provider_run_id == run_id &
        lineage$calibration_bundle_id == bundle_id
      if (sum(matching) != 1L) {
        stpd_provider_contract_fail("calibration_bundle_missing",
                                    "provider_runs",
                                    which(runs$provider_run_id == run_id)[[1L]],
                                    "calibration_bundle_id", bundle_id,
                                    provider_run_id = run_id)
      }
    }
  }
  referenced_run_bundles <- which(!is.na(runs$calibration_bundle_id))
  for (i in referenced_run_bundles) {
    if (!(runs$calibration_bundle_id[[i]] %in% lineage$calibration_bundle_id)) {
      stpd_provider_contract_fail("calibration_bundle_missing", "provider_runs", i,
                                  "calibration_bundle_id",
                                  runs$calibration_bundle_id[[i]],
                                  provider_run_id = runs$provider_run_id[[i]])
    }
  }
  if (!nrow(lineage)) return(invisible(TRUE))
  for (i in seq_len(nrow(lineage))) {
    row <- lineage[i, , drop = FALSE]
    producer <- runs[runs$provider_run_id == row$calibration_provider_run_id,
                     , drop = FALSE]
    consumer <- runs[runs$provider_run_id == row$consumer_provider_run_id,
                     , drop = FALSE]
    if (identical(row$calibration_provider_run_id,
                  row$consumer_provider_run_id) ||
        producer$output_role != "calibration_output" ||
        producer$authority_scope != "calibration_parameter_only" ||
        consumer$provider_key != "native_stpd" ||
        consumer$provider_kind != "native_stpd" ||
        consumer$output_role != "automatic_prediction" ||
        consumer$authority_scope != "automatic_prediction_record" ||
        consumer$generation_mode != "support_to_native" ||
        !identical(consumer$calibration_bundle_id, row$calibration_bundle_id)) {
      stpd_provider_contract_fail("calibration_lineage_invalid",
                                  "calibration_lineage", i)
    }
    if (!identical(row$calibration_dataset_snapshot_sha256,
                   producer$dataset_snapshot_sha256) ||
        !identical(row$parameter_bundle_sha256, consumer$params_sha256)) {
      stpd_provider_contract_fail("calibration_lineage_invalid",
                                  "calibration_lineage", i,
                                  message = paste(
                                    "Calibration dataset or consumer parameter",
                                    "hash does not match lineage."
                                  ))
    }
    allowed_sources <- registry$calibration_sources_by_provider_kind[[
      producer$provider_kind
    ]]
    if (is.null(allowed_sources) ||
        !(row$calibration_source_kind %in% allowed_sources)) {
      stpd_provider_contract_fail(
        "calibration_lineage_invalid", "calibration_lineage", i,
        "calibration_source_kind", row$calibration_source_kind,
        message = paste(
          "Calibration source kind is not authorized for the actual producer",
          "provider kind."
        )
      )
    }
    if (row$reference_annotations_used && !row$labels_used) {
      stpd_provider_contract_fail(
        "label_blind_leakage", "calibration_lineage", i,
        "reference_annotations_used",
        message = "Reference annotations are labels and imply labels_used=TRUE."
      )
    }
    if (row$calibration_source_kind == "manual_examples" &&
        (!row$labels_used || producer$information_access != "manual_aware")) {
      stpd_provider_contract_fail(
        "label_blind_leakage", "calibration_lineage", i,
        "calibration_source_kind", row$calibration_source_kind,
        message = paste(
          "Manual examples require a manual-aware calibration run and",
          "labels_used=TRUE."
        )
      )
    }
    if (row$calibration_source_kind %in% c("mean_isi", "logisi_newbd") &&
        (row$labels_used || row$reference_annotations_used ||
         producer$information_access != "label_blind")) {
      stpd_provider_contract_fail(
        "label_blind_leakage", "calibration_lineage", i,
        "calibration_source_kind", row$calibration_source_kind,
        message = paste(
          "A label-blind algorithmic calibration source cannot carry",
          "manual/reference access. Use manual_examples explicitly."
        )
      )
    }
    expected_consumer_access <- if (
      row$labels_used || row$reference_annotations_used ||
        producer$information_access == "manual_aware"
    ) {
      "manual_aware"
    } else if (producer$information_access == "unknown") {
      "unknown"
    } else {
      "label_blind"
    }
    if (consumer$information_access != expected_consumer_access) {
      stpd_provider_contract_fail(
        "label_blind_leakage", "calibration_lineage", i,
        "consumer_provider_run_id", row$consumer_provider_run_id,
        message = paste(
          "Calibration information access must propagate to the native",
          "consumer without downgrade or silent promotion."
        )
      )
    }
    producer_thresholds <- thresholds[
      thresholds$provider_run_id == row$calibration_provider_run_id &
        thresholds$threshold_role == "calibration_output", , drop = FALSE
    ]
    if (!nrow(producer_thresholds)) {
      stpd_provider_contract_fail("calibration_lineage_invalid",
                                  "calibration_lineage", i,
                                  message = "Calibration bundle has no output threshold.")
    }
    expected_set <- stpd_provider_threshold_set_sha256_v1(
      producer_thresholds$threshold_id
    )
    if (!identical(row$threshold_set_sha256, expected_set)) {
      stpd_provider_contract_fail("calibration_bundle_hash_mismatch",
                                  "calibration_lineage", i,
                                  "threshold_set_sha256")
    }
    expected_bundle <- stpd_provider_calibration_bundle_id_v1(
      row$calibration_provider_run_id, row$threshold_set_sha256,
      row$parameter_bundle_sha256
    )
    if (!identical(row$calibration_bundle_id, expected_bundle)) {
      stpd_provider_contract_fail("calibration_bundle_hash_mismatch",
                                  "calibration_lineage", i,
                                  "calibration_bundle_id")
    }
    if ((nrow(producer_thresholds) &&
         any(producer_thresholds$calibration_bundle_id !=
             row$calibration_bundle_id, na.rm = TRUE)) ||
        any(is.na(producer_thresholds$calibration_bundle_id))) {
      stpd_provider_contract_fail("calibration_bundle_hash_mismatch",
                                  "calibration_lineage", i,
                                  "calibration_bundle_id")
    }
    if (row$group_overlap_count < 0L) {
      stpd_provider_contract_fail("calibration_lineage_invalid",
                                  "calibration_lineage", i,
                                  "group_overlap_count", row$group_overlap_count)
    }
    if (row$group_separation_status == "not_required" &&
        (!is.na(row$calibration_group_set_sha256) ||
         !is.na(row$evaluation_group_set_sha256) ||
         row$group_overlap_count != 0L)) {
      stpd_provider_contract_fail(
        "calibration_lineage_invalid", "calibration_lineage", i,
        "group_separation_status",
        message = paste(
          "not_required separation cannot carry evaluation group claims."
        )
      )
    }
    if (row$group_separation_status == "verified_disjoint" &&
        (is.na(row$calibration_group_set_sha256) ||
         is.na(row$evaluation_group_set_sha256) ||
         identical(row$calibration_group_set_sha256,
                   row$evaluation_group_set_sha256) ||
         row$group_overlap_count != 0L)) {
      stpd_provider_contract_fail(
        "calibration_group_unverifiable", "calibration_lineage", i
      )
    }
    if (row$group_overlap_count > 0L) {
      recorded_rejection <- row$group_separation_status == "overlap_rejected" &&
        row$performance_use == "not_eligible" &&
        consumer$run_status == "rejected"
      if (!recorded_rejection) {
        stpd_provider_contract_fail("calibration_group_overlap",
                                    "calibration_lineage", i,
                                    "group_overlap_count", row$group_overlap_count)
      }
    }
    if (row$group_separation_status == "unverifiable_rejected") {
      recorded_rejection <- row$performance_use == "not_eligible" &&
        consumer$run_status == "rejected"
      if (!recorded_rejection) {
        stpd_provider_contract_fail("calibration_group_unverifiable",
                                    "calibration_lineage", i)
      }
    }
    supervised_heldout <- (row$labels_used || row$reference_annotations_used) &&
      row$performance_use == "heldout_detector_performance_only"
    if (supervised_heldout &&
        (row$group_separation_status != "verified_disjoint" ||
         is.na(row$calibration_group_set_sha256) ||
         is.na(row$evaluation_group_set_sha256))) {
      stpd_provider_contract_fail("calibration_group_unverifiable",
                                  "calibration_lineage", i)
    }
    if (row$performance_use == "label_blind_detector_performance" &&
        (row$labels_used || row$reference_annotations_used)) {
      stpd_provider_contract_fail("label_blind_leakage", "calibration_lineage", i)
    }
    if (row$performance_use == "label_blind_detector_performance" &&
        consumer$information_access != "label_blind") {
      stpd_provider_contract_fail("estimand_authority_mismatch",
                                  "calibration_lineage", i,
                                  "performance_use", row$performance_use)
    }
    if (row$group_separation_status %in%
        c("overlap_rejected", "unverifiable_rejected") &&
        row$performance_use != "not_eligible") {
      stpd_provider_contract_fail("estimand_authority_mismatch",
                                  "calibration_lineage", i,
                                  "performance_use", row$performance_use)
    }
    if (!isTRUE(registry$phase_a_authority[[
      "detector_performance_claims_enabled"
    ]]) && row$performance_use != "not_eligible") {
      stpd_provider_contract_fail(
        "estimand_authority_mismatch", "calibration_lineage", i,
        "performance_use", row$performance_use,
        message = paste(
          "Phase A records provenance only. Detector-performance authority",
          "requires Phase B canonical group-set artifact verification."
        )
      )
    }
    if (!identical(row$calibration_lineage_id,
                   stpd_provider_calibration_lineage_id_v1(row))) {
      stpd_provider_contract_fail("id_mismatch", "calibration_lineage", i,
                                  "calibration_lineage_id",
                                  row$calibration_lineage_id)
    }
  }
  unique_key <- paste(lineage$calibration_bundle_id,
                      lineage$consumer_provider_run_id, sep = "\r")
  if (anyDuplicated(unique_key)) {
    i <- which(duplicated(unique_key) |
                 duplicated(unique_key, fromLast = TRUE))[[1L]]
    stpd_provider_contract_fail("primary_key_duplicate", "calibration_lineage",
                                i)
  }
  invisible(TRUE)
}

stpd_provider_validate_outputs <- function(bundle) {
  runs <- bundle$provider_runs
  for (i in seq_len(nrow(runs))) {
    run_id <- runs$provider_run_id[[i]]
    has_candidates <- any(bundle$candidate_intervals$provider_run_id == run_id)
    has_thresholds <- any(bundle$thresholds$provider_run_id == run_id)
    if (runs$run_status[[i]] == "rejected" &&
        (has_candidates || has_thresholds)) {
      stpd_provider_contract_fail("provider_authority_invalid", "provider_runs", i,
                                  provider_run_id = run_id,
                                  message = "Rejected runs cannot emit normalized output.")
    }
    expected <- stpd_provider_normalized_output_sha256_v1(bundle, run_id)
    if (!identical(runs$normalized_output_sha256[[i]], expected)) {
      stpd_provider_contract_fail("hash_mismatch", "provider_runs", i,
                                  "normalized_output_sha256",
                                  runs$normalized_output_sha256[[i]],
                                  provider_run_id = run_id)
    }
  }
  invisible(TRUE)
}

stpd_validate_provider_bundle <- function(bundle) {
  registry <- stpd_provider_contract_registry()
  before <- serialize(bundle, NULL, version = 3L)
  stpd_provider_validate_structure(bundle, registry)
  stpd_provider_validate_sha_and_keys(bundle, registry)
  stpd_provider_validate_runs(bundle, registry)
  stpd_provider_validate_foreign_keys(bundle)
  stpd_provider_validate_audit(bundle, registry)
  stpd_provider_validate_candidates(bundle, registry)
  stpd_provider_validate_thresholds(bundle, registry)
  stpd_provider_validate_lineage(bundle, registry)
  stpd_provider_validate_outputs(bundle)
  after <- serialize(bundle, NULL, version = 3L)
  if (!identical(before, after)) {
    stpd_provider_contract_fail("hash_mismatch", "bundle",
                                message = "Validation mutated the provider bundle.")
  }
  invisible(TRUE)
}
