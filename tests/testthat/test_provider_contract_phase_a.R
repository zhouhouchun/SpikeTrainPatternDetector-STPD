provider_contract_sha <- function(x) {
  paste(rep(x, 64L), collapse = "")
}

provider_contract_internal <- function(name) {
  getFromNamespace(name, "SpikeTrainPatternDetector")
}

provider_contract_expect_error <- function(expr, code, table = NULL,
                                           column = NULL) {
  error <- tryCatch(force(expr), error = function(e) e)
  expect_s3_class(error, "stpd_provider_contract_error")
  expect_true(inherits(error, code))
  expect_identical(error$code, code)
  expect_true(all(c(
    "code", "table", "row", "column", "provider_run_id",
    "source_record_key", "offending_value", "message"
  ) %in% names(error)))
  if (!is.null(table)) expect_identical(error$table, table)
  if (!is.null(column)) expect_identical(error$column, column)
  invisible(error)
}

provider_contract_expected_schema <- function() {
  list(
    provider_runs = c(
      schema_version = "character", provider_run_id = "character",
      provider_key = "character", provider_kind = "character",
      provider_version = "character", adapter_key = "character",
      adapter_version = "character", output_role = "character",
      authority_scope = "character", generation_mode = "character",
      information_access = "character", capability_profile = "character",
      run_status = "character", dataset_snapshot_sha256 = "character",
      input_artifact_sha256 = "character", timestamp_spine_sha256 = "character",
      params_sha256 = "character", calibration_bundle_id = "character",
      raw_output_sha256 = "character", normalized_output_sha256 = "character",
      provider_code_sha256 = "character", adapter_code_sha256 = "character",
      contract_sha256 = "character", created_utc = "character"
    ),
    candidate_intervals = c(
      schema_version = "character", candidate_id = "character",
      provider_run_id = "character", source_record_key = "character",
      source_record_sha256 = "character", normalization_audit_id = "character",
      train_key = "character", train_timestamp_sha256 = "character",
      record_kind = "character", provider_decision = "character",
      semantic_track = "character", proposed_label = "character",
      canonical_start_isi = "integer", canonical_end_isi = "integer",
      canonical_start_spike = "integer", canonical_end_spike = "integer",
      canonical_start_time_sec = "double", canonical_end_time_sec = "double",
      canonical_interval_closure = "character", score_name = "character",
      score_value = "double", score_direction = "character",
      uncertainty_kind = "character", uncertainty_lower = "double",
      uncertainty_upper = "double", evidence_manifest_sha256 = "character"
    ),
    thresholds = c(
      schema_version = "character", threshold_id = "character",
      provider_run_id = "character", calibration_bundle_id = "character",
      threshold_name = "character", parameter_path = "character",
      threshold_value = "double", threshold_unit = "character",
      threshold_role = "character", scope_type = "character",
      scope_sha256 = "character", semantic_track = "character",
      target_label = "character", source_kind = "character",
      source_record_sha256 = "character", comparison_operator = "character",
      evidence_manifest_sha256 = "character"
    ),
    normalization_audit = c(
      schema_version = "character", normalization_audit_id = "character",
      provider_run_id = "character", source_record_key = "character",
      source_record_sha256 = "character", train_key = "character",
      source_coordinate_convention = "character", source_index_base = "character",
      source_interval_closure = "character", source_time_unit = "character",
      source_start_index = "integer", source_end_index = "integer",
      source_start_time = "double", source_end_time = "double",
      transformation = "character", tolerance_sec = "double",
      canonical_start_isi = "integer", canonical_end_isi = "integer",
      canonical_start_spike = "integer", canonical_end_spike = "integer",
      canonical_start_time_sec = "double", canonical_end_time_sec = "double",
      ambiguity_detected = "logical", normalization_status = "character",
      error_code = "character", error_detail = "character"
    ),
    calibration_lineage = c(
      schema_version = "character", calibration_lineage_id = "character",
      calibration_bundle_id = "character",
      calibration_provider_run_id = "character",
      consumer_provider_run_id = "character",
      calibration_source_kind = "character",
      calibration_authority_scope = "character",
      calibration_dataset_snapshot_sha256 = "character",
      parameter_bundle_sha256 = "character", threshold_set_sha256 = "character",
      grouping_key = "character", calibration_group_set_sha256 = "character",
      evaluation_group_set_sha256 = "character", group_overlap_count = "integer",
      labels_used = "logical", reference_annotations_used = "logical",
      group_separation_status = "character", performance_use = "character"
    )
  )
}

provider_contract_expected_enums <- function() {
  list(
    provider_kind = c(
      "native_stpd", "mean_isi", "logisi_newbd", "external_data_import"
    ),
    output_role = c(
      "automatic_prediction", "candidate_support", "calibration_output"
    ),
    authority_scope = c(
      "automatic_prediction_record", "support_evidence_only",
      "calibration_parameter_only", "none_candidate"
    ),
    generation_mode = c("native_only", "external_only", "support_to_native"),
    information_access = c("label_blind", "manual_aware", "unknown"),
    capability_profile = c(
      "native_composed_v1", "burst_interval_threshold_v1",
      "external_interval_v1", "external_per_isi_v1"
    ),
    run_status = c("complete", "rejected"),
    record_kind = c("automatic_assertion", "candidate", "support"),
    provider_decision = c("positive", "negative", "indeterminate"),
    candidate_semantic_track = c("event", "state", "gap"),
    proposed_label = c(
      "burst", "long_burst", "broad_hfs", "hft", "hf_irregular", "tonic",
      "pause"
    ),
    canonical_interval_closure = "closed",
    score_direction = c(
      "higher_is_stronger", "lower_is_stronger", "not_applicable"
    ),
    uncertainty_kind = c(
      "none", "confidence_interval", "credible_interval", "provider_interval"
    ),
    threshold_unit = c(
      "sec", "ms", "hz", "ratio", "count", "probability", "dimensionless",
      "log10_sec"
    ),
    threshold_role = c(
      "reported", "calibration_output", "detector_input", "effective_parameter"
    ),
    scope_type = c("dataset", "recording_group", "train", "provider_run"),
    threshold_semantic_track = c("event", "state", "gap", "not_applicable"),
    threshold_target_label = c(
      "burst", "long_burst", "broad_hfs", "hft", "hf_irregular", "tonic",
      "pause", "not_applicable"
    ),
    source_kind = c(
      "provider_default", "unsupervised_data_derived", "manual_calibration",
      "external_declared", "calibration_bundle"
    ),
    comparison_operator = c(
      "lt", "le", "gt", "ge", "eq", "range_lower", "range_upper"
    ),
    source_coordinate_convention = c(
      "train_row_isi_index", "diff_timestamp_isi_index", "spike_index_span",
      "spike_time_span", "per_isi_diff_index"
    ),
    source_index_base = c("zero", "one", "not_applicable"),
    source_interval_closure = c(
      "closed", "left_closed_right_open", "left_open_right_closed", "open",
      "point"
    ),
    source_time_unit = c("s", "ms", "us", "not_applicable"),
    transformation = c(
      "identity_train_row_one_based", "diff_one_based_plus_one",
      "diff_zero_based_plus_two", "spike_one_based_span_to_isi",
      "spike_zero_based_span_to_isi", "exact_spike_time_span_to_isi",
      "per_isi_one_based_runs_plus_one", "per_isi_zero_based_runs_plus_two"
    ),
    normalization_status = c("normalized", "rejected"),
    calibration_source_kind = c(
      "native_unsupervised", "mean_isi", "logisi_newbd", "manual_examples",
      "external_parameters"
    ),
    calibration_authority_scope = "calibration_parameter_only",
    group_separation_status = c(
      "not_required", "verified_disjoint", "overlap_rejected",
      "unverifiable_rejected"
    ),
    performance_use = c(
      "label_blind_detector_performance", "heldout_detector_performance_only",
      "adjudicated_agreement_only", "not_eligible"
    )
  )
}

provider_contract_expected_coordinate_formulas <- function() {
  data.frame(
    source_coordinate_convention = c(
      "train_row_isi_index", "diff_timestamp_isi_index",
      "diff_timestamp_isi_index", "spike_index_span", "spike_index_span",
      "per_isi_diff_index", "per_isi_diff_index", "spike_time_span"
    ),
    source_index_base = c(
      "one", "one", "zero", "one", "zero", "one", "zero",
      "not_applicable"
    ),
    transformation = c(
      "identity_train_row_one_based", "diff_one_based_plus_one",
      "diff_zero_based_plus_two", "spike_one_based_span_to_isi",
      "spike_zero_based_span_to_isi", "per_isi_one_based_runs_plus_one",
      "per_isi_zero_based_runs_plus_two", "exact_spike_time_span_to_isi"
    ),
    canonical_start_formula = c(
      "a", "a+1", "a+2", "p+1", "p+2", "a+1", "a+2", "p+1"
    ),
    canonical_end_formula = c(
      "b", "b+1", "b+2", "q", "q+1", "b+1", "b+2", "q"
    ),
    stringsAsFactors = FALSE
  )
}

provider_contract_expected_closure_adjustments <- function() {
  data.frame(
    source_interval_closure = c(
      "closed", "left_closed_right_open", "left_open_right_closed", "open"
    ),
    start_adjustment = c(0L, 0L, 1L, 1L),
    end_adjustment = c(0L, -1L, 0L, -1L),
    stringsAsFactors = FALSE
  )
}

provider_contract_expected_canonical_geometry <- function() {
  c(
    start_spike = "canonical_start_isi-1",
    end_spike = "canonical_end_isi",
    start_time_sec = "timestamp[canonical_start_isi-1]",
    end_time_sec = "timestamp[canonical_end_isi]",
    n_isi = "canonical_end_isi-canonical_start_isi+1",
    n_spikes = "canonical_end_isi-canonical_start_isi+2"
  )
}

provider_contract_empty_bundle <- function() {
  stpd_provider_bundle_prototypes()
}

provider_contract_make_native_bundle <- function() {
  version <- stpd_provider_contract_versions()[["bundle"]]
  prototypes <- stpd_provider_bundle_prototypes()
  run_id <- provider_contract_internal("stpd_provider_run_id_v1")
  audit_id <- provider_contract_internal("stpd_provider_normalization_audit_id_v1")
  candidate_id <- provider_contract_internal("stpd_provider_candidate_id_v1")
  output_hash <- provider_contract_internal(
    "stpd_provider_normalized_output_sha256_v1"
  )

  run <- prototypes$provider_runs[0L, , drop = FALSE]
  run[1L, ] <- list(
    version, NA_character_, "native_stpd", "native_stpd", "1.2.2",
    "native_stpd_postcomposer_v1", "1.0.0", "automatic_prediction",
    "automatic_prediction_record", "native_only", "label_blind",
    "native_composed_v1", "complete", provider_contract_sha("a"),
    provider_contract_sha("b"), provider_contract_sha("c"),
    provider_contract_sha("d"), NA_character_, provider_contract_sha("e"),
    provider_contract_sha("f"), provider_contract_sha("1"),
    provider_contract_sha("2"), stpd_provider_contract_hash(),
    "2026-08-27T00:00:00Z"
  )
  run$provider_run_id <- run_id(run)

  audit <- prototypes$normalization_audit[0L, , drop = FALSE]
  audit[1L, ] <- list(
    version, NA_character_, run$provider_run_id, "native-record-1",
    provider_contract_sha("3"), "Train_01", "diff_timestamp_isi_index", "one",
    "closed", "not_applicable", 1L, 2L, NA_real_, NA_real_,
    "diff_one_based_plus_one",
    0, 2L, 3L, 1L, 3L, 0.10, 0.20, FALSE, "normalized", NA_character_,
    NA_character_
  )
  audit$normalization_audit_id <- audit_id(audit)

  candidate <- prototypes$candidate_intervals[0L, , drop = FALSE]
  candidate[1L, ] <- list(
    version, NA_character_, run$provider_run_id, "native-record-1",
    provider_contract_sha("3"), audit$normalization_audit_id, "Train_01",
    provider_contract_sha("4"), "automatic_assertion", "positive", "event",
    "burst", 2L, 3L, 1L, 3L, 0.10, 0.20, "closed", "native_score", 1,
    "higher_is_stronger", "none", NA_real_, NA_real_, NA_character_
  )
  candidate$candidate_id <- candidate_id(candidate)

  bundle <- list(
    provider_runs = run,
    candidate_intervals = candidate,
    thresholds = prototypes$thresholds,
    normalization_audit = audit,
    calibration_lineage = prototypes$calibration_lineage
  )
  bundle$provider_runs$normalized_output_sha256 <- output_hash(
    bundle, run$provider_run_id
  )
  bundle
}

provider_contract_make_meanisi_bundle <- function() {
  bundle <- provider_contract_make_native_bundle()
  run_id <- provider_contract_internal("stpd_provider_run_id_v1")
  audit_id <- provider_contract_internal("stpd_provider_normalization_audit_id_v1")
  candidate_id <- provider_contract_internal("stpd_provider_candidate_id_v1")
  output_hash <- provider_contract_internal(
    "stpd_provider_normalized_output_sha256_v1"
  )
  bundle$provider_runs$provider_key <- "mean_isi"
  bundle$provider_runs$provider_kind <- "mean_isi"
  bundle$provider_runs$provider_version <- "1.0.0"
  bundle$provider_runs$adapter_key <- "mean_isi_v1"
  bundle$provider_runs$generation_mode <- "external_only"
  bundle$provider_runs$capability_profile <- "burst_interval_threshold_v1"
  bundle$provider_runs$provider_run_id <- run_id(bundle$provider_runs)
  bundle$normalization_audit$provider_run_id <-
    bundle$provider_runs$provider_run_id
  bundle$normalization_audit$normalization_audit_id <- audit_id(
    bundle$normalization_audit
  )
  bundle$candidate_intervals$provider_run_id <-
    bundle$provider_runs$provider_run_id
  bundle$candidate_intervals$normalization_audit_id <-
    bundle$normalization_audit$normalization_audit_id
  bundle$candidate_intervals$candidate_id <- candidate_id(
    bundle$candidate_intervals
  )
  bundle$provider_runs$normalized_output_sha256 <- output_hash(
    bundle, bundle$provider_runs$provider_run_id
  )
  bundle
}

provider_contract_make_calibration_bundle <- function(
    calibration_source_kind = "manual_examples",
    producer_information_access = "manual_aware",
    threshold_source_kind = "manual_calibration",
    consumer_information_access = "manual_aware",
    labels_used = TRUE, reference_annotations_used = FALSE,
    group_overlap_count = 0L,
    group_separation_status = "not_required",
    performance_use = "not_eligible",
    calibration_group_set_sha256 = NA_character_,
    evaluation_group_set_sha256 = NA_character_) {
  version <- stpd_provider_contract_versions()[["bundle"]]
  prototypes <- stpd_provider_bundle_prototypes()
  run_id <- provider_contract_internal("stpd_provider_run_id_v1")
  threshold_id <- provider_contract_internal("stpd_provider_threshold_id_v1")
  threshold_set <- provider_contract_internal(
    "stpd_provider_threshold_set_sha256_v1"
  )
  calibration_id <- provider_contract_internal(
    "stpd_provider_calibration_bundle_id_v1"
  )
  lineage_id <- provider_contract_internal(
    "stpd_provider_calibration_lineage_id_v1"
  )
  output_hash <- provider_contract_internal(
    "stpd_provider_normalized_output_sha256_v1"
  )
  contract_hash <- stpd_provider_contract_hash()
  parameter_bundle_hash <- provider_contract_sha("c")

  producer <- prototypes$provider_runs[0L, , drop = FALSE]
  producer[1L, ] <- list(
    version, NA_character_, "mean_isi", "mean_isi", "1.0.0",
    "mean_isi_v1", "1.0.0", "calibration_output",
    "calibration_parameter_only", "support_to_native",
    producer_information_access,
    "burst_interval_threshold_v1", "complete", provider_contract_sha("d"),
    provider_contract_sha("e"), provider_contract_sha("f"),
    provider_contract_sha("1"), NA_character_, provider_contract_sha("2"),
    provider_contract_sha("3"), provider_contract_sha("4"),
    provider_contract_sha("5"), contract_hash, "2026-08-27T00:00:00Z"
  )
  producer$provider_run_id <- run_id(producer)

  threshold <- prototypes$thresholds[0L, , drop = FALSE]
  threshold[1L, ] <- list(
    version, NA_character_, producer$provider_run_id, NA_character_,
    "burst_seed_isi_upper", NA_character_, 0.02, "sec", "calibration_output",
    "dataset", provider_contract_sha("6"), "event", "burst",
    threshold_source_kind, provider_contract_sha("7"), "le",
    provider_contract_sha("8")
  )
  threshold$threshold_id <- threshold_id(threshold)
  threshold_set_hash <- threshold_set(threshold$threshold_id)
  bundle_id <- calibration_id(
    producer$provider_run_id, threshold_set_hash, parameter_bundle_hash
  )
  threshold$calibration_bundle_id <- bundle_id

  consumer <- prototypes$provider_runs[0L, , drop = FALSE]
  consumer[1L, ] <- list(
    version, NA_character_, "native_stpd", "native_stpd", "1.2.2",
    "native_stpd_postcomposer_v1", "1.0.0", "automatic_prediction",
    "automatic_prediction_record", "support_to_native",
    consumer_information_access, "native_composed_v1", "complete",
    provider_contract_sha("d"), provider_contract_sha("9"),
    provider_contract_sha("0"), parameter_bundle_hash, bundle_id,
    provider_contract_sha("a"), provider_contract_sha("b"),
    provider_contract_sha("c"), provider_contract_sha("d"), contract_hash,
    "2026-08-27T00:00:01Z"
  )
  consumer$provider_run_id <- run_id(consumer)

  lineage <- prototypes$calibration_lineage[0L, , drop = FALSE]
  lineage[1L, ] <- list(
    version, NA_character_, bundle_id, producer$provider_run_id,
    consumer$provider_run_id, calibration_source_kind,
    "calibration_parameter_only",
    producer$dataset_snapshot_sha256, parameter_bundle_hash,
    threshold_set_hash, "recording_group", calibration_group_set_sha256,
    evaluation_group_set_sha256, as.integer(group_overlap_count),
    isTRUE(labels_used), isTRUE(reference_annotations_used),
    group_separation_status, performance_use
  )
  lineage$calibration_lineage_id <- lineage_id(lineage)

  bundle <- list(
    provider_runs = rbind(producer, consumer),
    candidate_intervals = prototypes$candidate_intervals,
    thresholds = threshold,
    normalization_audit = prototypes$normalization_audit,
    calibration_lineage = lineage
  )
  bundle$provider_runs$normalized_output_sha256 <- vapply(
    bundle$provider_runs$provider_run_id,
    function(id) output_hash(bundle, id), character(1)
  )
  bundle
}

test_that("provider bundle v1 exposes exactly five frozen typed prototypes", {
  expect_identical(stpd_provider_contract_versions(), c(
    bundle = "stpd_provider_bundle_v1",
    coordinate = "stpd_train_row_isi_v1",
    ontology = "stpd_provider_ontology_v1"
  ))

  prototypes <- stpd_provider_bundle_prototypes()
  expected <- provider_contract_expected_schema()
  expect_identical(names(prototypes), names(expected))
  expect_identical(vapply(prototypes, nrow, integer(1)), setNames(
    rep(0L, 5L), names(expected)
  ))
  expect_identical(vapply(prototypes, ncol, integer(1)), c(
    provider_runs = 24L, candidate_intervals = 26L, thresholds = 17L,
    normalization_audit = 26L, calibration_lineage = 18L
  ))

  for (table in names(expected)) {
    expect_identical(names(prototypes[[table]]), names(expected[[table]]))
    expect_identical(
      vapply(prototypes[[table]], typeof, character(1)),
      expected[[table]],
      info = table
    )
  }
  expect_match(stpd_provider_contract_hash(), "^[0-9a-f]{64}$")
  expect_identical(
    stpd_provider_contract_hash(),
    "29a2a660ee594494c6687433db1448825bf5ac5f518f36344bdf81f65ee01a6b"
  )
  contract_doc <- readLines(
    testthat::test_path("..", "..", "docs", "provider-bundle-v1-contract.md"),
    warn = FALSE
  )
  marker <- grep("^The frozen Phase-A contract SHA-256 is$", contract_doc)
  expect_length(marker, 1L)
  documented_hash <- gsub("[^0-9a-f]", "", contract_doc[marker + 1L])
  expect_identical(documented_hash, stpd_provider_contract_hash())
})

test_that("provider registry freezes allowlists enums authority ontology and coordinates", {
  registry <- stpd_provider_contract_registry()
  expect_true(all(c(
    "provider_allowlist", "adapter_allowlist", "enums",
    "authority_by_output_role", "coordinate_formulas",
    "integer_closure_adjustments", "canonical_geometry", "track_labels",
    "capability_labels", "subtype_parent_labels",
    "calibration_sources_by_provider_kind",
    "calibration_threshold_source_kind", "phase_a_authority",
    "coordinate_source_contract", "identity_contract", "error_codes"
  ) %in% names(registry)))

  allowlist <- registry$provider_allowlist
  expect_s3_class(allowlist, "data.frame")
  expect_true(all(c(
    "provider_key", "provider_kind", "adapter_key", "capability_profile",
    "output_role", "generation_mode"
  ) %in% names(allowlist)))
  expect_setequal(unique(allowlist$provider_key), c(
    "native_stpd", "mean_isi", "logisi_newbd", "external_data_import"
  ))
  expect_setequal(registry$adapter_allowlist, c(
    "native_stpd_postcomposer_v1", "mean_isi_v1", "logisi_newbd_v1",
    "external_interval_v1", "external_per_isi_v1"
  ))
  expect_true(any(
    allowlist$provider_key == "native_stpd" &
      allowlist$adapter_key == "native_stpd_postcomposer_v1" &
      allowlist$output_role == "automatic_prediction" &
      allowlist$generation_mode == "native_only"
  ))
  expect_identical(registry$enums, provider_contract_expected_enums())
  expect_identical(registry$authority_by_output_role, c(
    automatic_prediction = "automatic_prediction_record",
    candidate_support = "support_evidence_only",
    calibration_output = "calibration_parameter_only"
  ))
  expect_identical(registry$track_labels, list(
    event = c("burst", "long_burst"),
    state = c("broad_hfs", "hft", "hf_irregular", "tonic"),
    gap = "pause"
  ))
  mean_log_capability <- registry$capability_labels[
    registry$capability_labels$capability_profile ==
      "burst_interval_threshold_v1", , drop = FALSE
  ]
  expect_identical(mean_log_capability$semantic_track, "event")
  expect_identical(mean_log_capability$proposed_label, "burst")
  expect_identical(registry$subtype_parent_labels, c(
    hft = "broad_hfs", hf_irregular = "broad_hfs"
  ))
  expect_identical(
    registry$calibration_sources_by_provider_kind$mean_isi,
    c("mean_isi", "manual_examples")
  )
  expect_false(unname(registry$phase_a_authority[[
    "detector_performance_claims_enabled"
  ]]))
  expect_identical(
    registry$coordinate_formulas,
    provider_contract_expected_coordinate_formulas()
  )
  expect_identical(
    registry$integer_closure_adjustments,
    provider_contract_expected_closure_adjustments()
  )
  expect_identical(
    registry$canonical_geometry,
    provider_contract_expected_canonical_geometry()
  )
  expect_true(all(c(
    "schema_version_unsupported", "required_field_missing", "unknown_column",
    "type_invalid", "nullability_violation", "enum_invalid",
    "primary_key_duplicate", "foreign_key_missing", "sha256_invalid",
    "hash_mismatch", "id_mismatch", "provider_not_allowlisted",
    "adapter_not_allowlisted", "provider_capability_mismatch",
    "provider_mode_role_invalid", "provider_authority_invalid",
    "provider_nondeterministic_output", "label_track_mismatch",
    "score_contract_invalid", "uncertainty_contract_invalid",
    "calibration_bundle_missing", "calibration_bundle_hash_mismatch",
    "calibration_lineage_invalid", "calibration_group_overlap",
    "calibration_group_unverifiable", "label_blind_leakage",
    "adjudication_in_provider_bundle", "reference_in_provider_bundle",
    "authority_role_conflict", "estimand_authority_mismatch"
  ) %in% registry$error_codes))
  expect_false(any(vapply(allowlist, is.function, logical(1))))
})

test_that("contract hash freezes identity preimages canonical JSON and a DAG", {
  identity <- stpd_provider_contract_registry()$identity_contract
  expect_true(all(c(
    "canonical_json", "payload_fields", "hash_dependencies"
  ) %in% names(identity)))
  expect_true(all(c(
    "schema_version", "provider_key", "provider_kind", "provider_version",
    "adapter_key", "adapter_version", "output_role", "authority_scope",
    "generation_mode", "information_access", "capability_profile",
    "run_status", "dataset_snapshot_sha256", "input_artifact_sha256",
    "timestamp_spine_sha256", "params_sha256", "calibration_bundle_id",
    "provider_code_sha256", "adapter_code_sha256", "contract_sha256"
  ) %in% identity$payload_fields$provider_run))
  expect_true(all(c(
    "provider_run", "normalization_audit", "candidate", "threshold",
    "threshold_set", "calibration_bundle", "calibration_lineage",
    "normalized_output", "contract"
  ) %in% names(identity$payload_fields)))
  expect_identical(identity$payload_fields$normalized_output, c(
    "candidate_ids", "threshold_ids", "normalization_audit_ids"
  ))

  dependencies <- identity$hash_dependencies
  visiting <- character()
  visited <- character()
  visit <- function(node) {
    if (node %in% visiting) return(FALSE)
    if (node %in% visited) return(TRUE)
    visiting <<- c(visiting, node)
    for (parent in dependencies[[node]]) {
      if (!visit(parent)) return(FALSE)
    }
    visiting <<- setdiff(visiting, node)
    visited <<- c(visited, node)
    TRUE
  }
  expect_true(all(vapply(names(dependencies), visit, logical(1))))

  bundle <- provider_contract_make_native_bundle()
  run <- bundle$provider_runs$provider_run_id
  exact_preimage <- list(
    candidate_ids = sort(bundle$candidate_intervals$candidate_id),
    threshold_ids = sort(bundle$thresholds$threshold_id),
    normalization_audit_ids = sort(
      bundle$normalization_audit$normalization_audit_id
    )
  )
  expected <- provider_contract_internal("stpd_provider_hash_domain")(
    "stpd-provider-normalized-output-v1", exact_preimage
  )
  actual <- provider_contract_internal(
    "stpd_provider_normalized_output_sha256_v1"
  )(bundle, run)
  expect_identical(actual, expected)
})

test_that("a minimal automatic provider bundle validates without mutation", {
  bundle <- provider_contract_make_native_bundle()
  before <- serialize(bundle, NULL, version = 3)
  expect_identical(stpd_validate_provider_bundle(bundle), TRUE)
  expect_identical(serialize(bundle, NULL, version = 3), before)
  expect_identical(stpd_validate_provider_bundle(bundle), TRUE)
})

test_that("schema violations fail closed with structured typed conditions", {
  bundle <- provider_contract_make_native_bundle()

  missing <- bundle
  missing$provider_runs$provider_key <- NULL
  provider_contract_expect_error(
    stpd_validate_provider_bundle(missing),
    "required_field_missing", "provider_runs", "provider_key"
  )

  extra <- bundle
  extra$provider_runs$unexpected <- "x"
  provider_contract_expect_error(
    stpd_validate_provider_bundle(extra),
    "unknown_column", "provider_runs", "unexpected"
  )

  wrong_type <- bundle
  wrong_type$candidate_intervals$canonical_start_isi <- 2
  provider_contract_expect_error(
    stpd_validate_provider_bundle(wrong_type),
    "type_invalid", "candidate_intervals", "canonical_start_isi"
  )

  missing_value <- bundle
  missing_value$provider_runs$provider_key <- NA_character_
  provider_contract_expect_error(
    stpd_validate_provider_bundle(missing_value),
    "nullability_violation", "provider_runs", "provider_key"
  )

  wrong_version <- bundle
  wrong_version$provider_runs$schema_version <- "stpd_provider_bundle_v2"
  provider_contract_expect_error(
    stpd_validate_provider_bundle(wrong_version),
    "schema_version_unsupported", "provider_runs", "schema_version"
  )
})

test_that("provider keys and allowlisted capability combinations fail closed", {
  bundle <- provider_contract_make_native_bundle()

  unknown_provider <- bundle
  unknown_provider$provider_runs$provider_key <- "custom_plugin"
  provider_contract_expect_error(
    stpd_validate_provider_bundle(unknown_provider),
    "provider_not_allowlisted", "provider_runs", "provider_key"
  )

  unknown_adapter <- bundle
  unknown_adapter$provider_runs$adapter_key <- "dynamic_executable_adapter"
  provider_contract_expect_error(
    stpd_validate_provider_bundle(unknown_adapter),
    "adapter_not_allowlisted", "provider_runs", "adapter_key"
  )

  wrong_capability <- bundle
  wrong_capability$provider_runs$capability_profile <- "external_interval_v1"
  provider_contract_expect_error(
    stpd_validate_provider_bundle(wrong_capability),
    "provider_capability_mismatch", "provider_runs", "capability_profile"
  )

  wrong_mode <- bundle
  wrong_mode$provider_runs$generation_mode <- "external_only"
  provider_contract_expect_error(
    stpd_validate_provider_bundle(wrong_mode),
    "provider_mode_role_invalid", "provider_runs"
  )

  invalid_utc <- bundle
  invalid_utc$provider_runs$created_utc <- "2026-99-99T00:00:00Z"
  provider_contract_expect_error(
    stpd_validate_provider_bundle(invalid_utc),
    "type_invalid", "provider_runs", "created_utc"
  )
})

test_that("Mean-ISI and LogISI capability cannot assert non-Burst semantics", {
  mean_bundle <- provider_contract_make_meanisi_bundle()
  expect_identical(stpd_validate_provider_bundle(mean_bundle), TRUE)

  tonic <- mean_bundle
  tonic$candidate_intervals$semantic_track <- "state"
  tonic$candidate_intervals$proposed_label <- "tonic"
  provider_contract_expect_error(
    stpd_validate_provider_bundle(tonic),
    "provider_capability_mismatch", "candidate_intervals", "proposed_label"
  )

  long_burst <- mean_bundle
  long_burst$candidate_intervals$proposed_label <- "long_burst"
  provider_contract_expect_error(
    stpd_validate_provider_bundle(long_burst),
    "provider_capability_mismatch", "candidate_intervals", "proposed_label"
  )

  threshold <- provider_contract_make_calibration_bundle()
  threshold$thresholds$semantic_track <- "state"
  threshold$thresholds$target_label <- "tonic"
  provider_contract_expect_error(
    stpd_validate_provider_bundle(threshold),
    "provider_capability_mismatch", "thresholds", "target_label"
  )
})

test_that("provider bundle primary and foreign keys cannot be forged", {
  bundle <- provider_contract_make_native_bundle()

  duplicate <- bundle
  duplicate$candidate_intervals <- rbind(
    duplicate$candidate_intervals, duplicate$candidate_intervals
  )
  provider_contract_expect_error(
    stpd_validate_provider_bundle(duplicate),
    "primary_key_duplicate", "candidate_intervals", "candidate_id"
  )

  orphan <- bundle
  orphan$candidate_intervals$provider_run_id <- provider_contract_sha("9")
  provider_contract_expect_error(
    stpd_validate_provider_bundle(orphan),
    "foreign_key_missing", "candidate_intervals", "provider_run_id"
  )
})

test_that("provider authority cannot be promoted to review or reference truth", {
  bundle <- provider_contract_make_native_bundle()

  reviewed <- bundle
  reviewed$provider_runs$authority_scope <- "reviewed_prediction_record"
  provider_contract_expect_error(
    stpd_validate_provider_bundle(reviewed),
    "adjudication_in_provider_bundle", "provider_runs", "authority_scope"
  )

  reference <- bundle
  reference$provider_runs$authority_scope <- "evaluation_reference_record"
  provider_contract_expect_error(
    stpd_validate_provider_bundle(reference),
    "reference_in_provider_bundle", "provider_runs", "authority_scope"
  )

  wrong_role <- bundle
  wrong_role$provider_runs$authority_scope <- "support_evidence_only"
  provider_contract_expect_error(
    stpd_validate_provider_bundle(wrong_role),
    "authority_role_conflict", "provider_runs", "authority_scope"
  )

  rejected_with_prediction_authority <- bundle
  rejected_with_prediction_authority$provider_runs$run_status <- "rejected"
  provider_contract_expect_error(
    stpd_validate_provider_bundle(rejected_with_prediction_authority),
    "provider_authority_invalid", "provider_runs", "authority_scope"
  )
})

test_that("candidate ontology geometry score and uncertainty are fail closed", {
  bundle <- provider_contract_make_native_bundle()

  wrong_track <- bundle
  wrong_track$candidate_intervals$semantic_track <- "state"
  provider_contract_expect_error(
    stpd_validate_provider_bundle(wrong_track),
    "label_track_mismatch", "candidate_intervals", "proposed_label"
  )

  wrong_geometry <- bundle
  wrong_geometry$candidate_intervals$canonical_start_spike <- 2L
  provider_contract_expect_error(
    stpd_validate_provider_bundle(wrong_geometry),
    "coordinate_roundtrip_mismatch", "candidate_intervals"
  )

  incomplete_score <- bundle
  incomplete_score$candidate_intervals$score_name <- NA_character_
  provider_contract_expect_error(
    stpd_validate_provider_bundle(incomplete_score),
    "score_contract_invalid", "candidate_intervals"
  )

  invalid_uncertainty <- bundle
  invalid_uncertainty$candidate_intervals$uncertainty_kind <-
    "confidence_interval"
  provider_contract_expect_error(
    stpd_validate_provider_bundle(invalid_uncertainty),
    "uncertainty_contract_invalid", "candidate_intervals"
  )
})

test_that("HFT and HF-irregular remain subtypes of one frozen Broad-HFS support", {
  candidate_id <- provider_contract_internal("stpd_provider_candidate_id_v1")
  output_hash <- provider_contract_internal(
    "stpd_provider_normalized_output_sha256_v1"
  )
  lone_subtype <- provider_contract_make_native_bundle()
  lone_subtype$candidate_intervals$semantic_track <- "state"
  lone_subtype$candidate_intervals$proposed_label <- "hft"
  lone_subtype$candidate_intervals$candidate_id <- candidate_id(
    lone_subtype$candidate_intervals
  )
  provider_contract_expect_error(
    stpd_validate_provider_bundle(lone_subtype),
    "provider_capability_mismatch", "candidate_intervals", "proposed_label"
  )

  valid <- provider_contract_make_native_bundle()
  parent <- valid$candidate_intervals
  parent$semantic_track <- "state"
  parent$proposed_label <- "broad_hfs"
  parent$candidate_id <- candidate_id(parent)
  subtype <- parent
  subtype$proposed_label <- "hft"
  subtype$candidate_id <- candidate_id(subtype)
  valid$candidate_intervals <- rbind(parent, subtype)
  valid$provider_runs$normalized_output_sha256 <- output_hash(
    valid, valid$provider_runs$provider_run_id
  )
  expect_identical(stpd_validate_provider_bundle(valid), TRUE)

  mixed_spine <- valid
  mixed_spine$candidate_intervals$train_timestamp_sha256[[2L]] <-
    provider_contract_sha("9")
  mixed_spine$candidate_intervals$candidate_id[[2L]] <- candidate_id(
    mixed_spine$candidate_intervals[2L, , drop = FALSE]
  )
  provider_contract_expect_error(
    stpd_validate_provider_bundle(mixed_spine),
    "train_timestamp_hash_mismatch", "candidate_intervals",
    "train_timestamp_sha256"
  )

  conflicting <- valid
  sibling <- parent
  sibling$proposed_label <- "hf_irregular"
  sibling$candidate_id <- candidate_id(sibling)
  conflicting$candidate_intervals <- rbind(
    conflicting$candidate_intervals, sibling
  )
  provider_contract_expect_error(
    stpd_validate_provider_bundle(conflicting),
    "provider_capability_mismatch", "candidate_intervals", "proposed_label"
  )
})

test_that("normalization audit status is mutually exclusive", {
  bundle <- provider_contract_make_native_bundle()

  ambiguous <- bundle
  ambiguous$normalization_audit$ambiguity_detected <- TRUE
  provider_contract_expect_error(
    stpd_validate_provider_bundle(ambiguous),
    "time_alignment_ambiguous", "normalization_audit",
    "ambiguity_detected"
  )

  rejected_with_geometry <- bundle
  rejected_with_geometry$normalization_audit$normalization_status <- "rejected"
  rejected_with_geometry$normalization_audit$error_code <-
    "coordinate_out_of_range"
  rejected_with_geometry$normalization_audit$error_detail <- "fixture"
  provider_contract_expect_error(
    stpd_validate_provider_bundle(rejected_with_geometry),
    "coordinate_roundtrip_mismatch", "normalization_audit"
  )

  wrong_matrix <- bundle
  wrong_matrix$normalization_audit$source_coordinate_convention <-
    "spike_time_span"
  wrong_matrix$normalization_audit$source_index_base <- "not_applicable"
  wrong_matrix$normalization_audit$source_time_unit <- "s"
  provider_contract_expect_error(
    stpd_validate_provider_bundle(wrong_matrix),
    "coordinate_roundtrip_mismatch", "normalization_audit", "transformation"
  )

  wrong_source_fields <- bundle
  wrong_source_fields$normalization_audit$source_coordinate_convention <-
    "spike_time_span"
  wrong_source_fields$normalization_audit$source_index_base <- "not_applicable"
  wrong_source_fields$normalization_audit$source_time_unit <- "s"
  wrong_source_fields$normalization_audit$transformation <-
    "exact_spike_time_span_to_isi"
  provider_contract_expect_error(
    stpd_validate_provider_bundle(wrong_source_fields),
    "coordinate_roundtrip_mismatch", "normalization_audit"
  )

  wrong_audit_geometry <- bundle
  wrong_audit_geometry$normalization_audit$canonical_start_spike <- 2L
  provider_contract_expect_error(
    stpd_validate_provider_bundle(wrong_audit_geometry),
    "coordinate_roundtrip_mismatch", "normalization_audit"
  )
})

test_that("entity IDs and hashes are deterministic full SHA-256 values", {
  bundle <- provider_contract_make_native_bundle()
  sha_columns <- unique(unlist(lapply(bundle, function(tab) {
    names(tab)[endsWith(names(tab), "_sha256")]
  }), use.names = FALSE))
  id_columns <- c(
    "provider_run_id", "candidate_id", "normalization_audit_id",
    "calibration_bundle_id", "calibration_lineage_id"
  )
  for (table in names(bundle)) {
    columns <- intersect(c(sha_columns, id_columns), names(bundle[[table]]))
    for (column in columns) {
      values <- bundle[[table]][[column]]
      values <- values[!is.na(values)]
      expect_true(all(grepl("^[0-9a-f]{64}$", values)),
                  info = paste(table, column))
    }
  }

  run_id <- provider_contract_internal("stpd_provider_run_id_v1")
  first <- bundle$provider_runs
  second <- first
  second$created_utc <- "2030-01-01T00:00:00Z"
  second$raw_output_sha256 <- provider_contract_sha("9")
  second$normalized_output_sha256 <- provider_contract_sha("8")
  expect_identical(run_id(first), run_id(second))

  changed_identity <- first
  changed_identity$params_sha256 <- provider_contract_sha("7")
  expect_false(identical(run_id(first), run_id(changed_identity)))

  candidate_id <- provider_contract_internal("stpd_provider_candidate_id_v1")
  same_geometry_other_provider <- bundle$candidate_intervals
  same_geometry_other_provider$provider_run_id <- provider_contract_sha("9")
  expect_false(identical(
    candidate_id(bundle$candidate_intervals),
    candidate_id(same_geometry_other_provider)
  ))

  invalid <- bundle
  invalid$provider_runs$input_artifact_sha256 <- paste(rep("a", 63L), collapse = "")
  provider_contract_expect_error(
    stpd_validate_provider_bundle(invalid),
    "sha256_invalid", "provider_runs", "input_artifact_sha256"
  )
})

test_that("calibration lineage binds a separate producer and consumer", {
  bundle <- provider_contract_make_calibration_bundle()
  expect_identical(stpd_validate_provider_bundle(bundle), TRUE)
  lineage <- bundle$calibration_lineage
  expect_false(identical(
    lineage$calibration_provider_run_id,
    lineage$consumer_provider_run_id
  ))
  expect_identical(
    bundle$thresholds$calibration_bundle_id,
    lineage$calibration_bundle_id
  )
  expect_identical(lineage$group_overlap_count, 0L)
  expect_identical(lineage$group_separation_status, "not_required")
  expect_identical(lineage$performance_use, "not_eligible")

  label_blind <- provider_contract_make_calibration_bundle(
    calibration_source_kind = "mean_isi",
    producer_information_access = "label_blind",
    threshold_source_kind = "unsupervised_data_derived",
    consumer_information_access = "label_blind",
    labels_used = FALSE,
    reference_annotations_used = FALSE
  )
  expect_identical(stpd_validate_provider_bundle(label_blind), TRUE)
})

test_that("manual calibration cannot leak into label-blind or overlapping evaluation", {
  leaked <- provider_contract_make_calibration_bundle(
    consumer_information_access = "label_blind"
  )
  provider_contract_expect_error(
    stpd_validate_provider_bundle(leaked),
    "label_blind_leakage", "calibration_lineage"
  )

  overlapping <- provider_contract_make_calibration_bundle(
    group_overlap_count = 1L,
    group_separation_status = "overlap_rejected",
    performance_use = "not_eligible"
  )
  provider_contract_expect_error(
    stpd_validate_provider_bundle(overlapping),
    "calibration_group_overlap", "calibration_lineage", "group_overlap_count"
  )

  unverifiable <- provider_contract_make_calibration_bundle(
    calibration_group_set_sha256 = NA_character_,
    evaluation_group_set_sha256 = NA_character_,
    group_separation_status = "unverifiable_rejected"
  )
  provider_contract_expect_error(
    stpd_validate_provider_bundle(unverifiable),
    "calibration_group_unverifiable", "calibration_lineage"
  )

  laundered <- provider_contract_make_calibration_bundle(
    consumer_information_access = "label_blind",
    labels_used = FALSE,
    reference_annotations_used = FALSE
  )
  provider_contract_expect_error(
    stpd_validate_provider_bundle(laundered),
    "label_blind_leakage", "calibration_lineage"
  )

  reference_without_labels <- provider_contract_make_calibration_bundle(
    labels_used = FALSE,
    reference_annotations_used = TRUE
  )
  provider_contract_expect_error(
    stpd_validate_provider_bundle(reference_without_labels),
    "label_blind_leakage", "calibration_lineage",
    "reference_annotations_used"
  )

  premature_performance <- provider_contract_make_calibration_bundle(
    group_separation_status = "verified_disjoint",
    performance_use = "heldout_detector_performance_only",
    calibration_group_set_sha256 = provider_contract_sha("a"),
    evaluation_group_set_sha256 = provider_contract_sha("b")
  )
  provider_contract_expect_error(
    stpd_validate_provider_bundle(premature_performance),
    "estimand_authority_mismatch", "calibration_lineage", "performance_use"
  )
})

test_that("calibration source consumer and threshold provenance are bound", {
  source_mismatch <- provider_contract_make_calibration_bundle()
  source_mismatch$calibration_lineage$calibration_source_kind <- "logisi_newbd"
  provider_contract_expect_error(
    stpd_validate_provider_bundle(source_mismatch),
    "calibration_lineage_invalid"
  )

  missing_source_record <- provider_contract_make_calibration_bundle()
  missing_source_record$thresholds$source_record_sha256 <- NA_character_
  provider_contract_expect_error(
    stpd_validate_provider_bundle(missing_source_record),
    "source_record_hash_mismatch", "thresholds", "source_record_sha256"
  )

  missing_bundle <- provider_contract_make_calibration_bundle()
  missing_bundle$thresholds$source_kind <- "calibration_bundle"
  missing_bundle$thresholds$calibration_bundle_id <- NA_character_
  provider_contract_expect_error(
    stpd_validate_provider_bundle(missing_bundle),
    "calibration_bundle_missing", "thresholds", "calibration_bundle_id"
  )

  wrong_consumer <- provider_contract_make_calibration_bundle()
  run_id <- provider_contract_internal("stpd_provider_run_id_v1")
  lineage_id <- provider_contract_internal(
    "stpd_provider_calibration_lineage_id_v1"
  )
  consumer_row <- 2L
  wrong_consumer$provider_runs$provider_key[[consumer_row]] <- "mean_isi"
  wrong_consumer$provider_runs$provider_kind[[consumer_row]] <- "mean_isi"
  wrong_consumer$provider_runs$provider_version[[consumer_row]] <- "1.0.0"
  wrong_consumer$provider_runs$adapter_key[[consumer_row]] <- "mean_isi_v1"
  wrong_consumer$provider_runs$output_role[[consumer_row]] <-
    "calibration_output"
  wrong_consumer$provider_runs$authority_scope[[consumer_row]] <-
    "calibration_parameter_only"
  wrong_consumer$provider_runs$capability_profile[[consumer_row]] <-
    "burst_interval_threshold_v1"
  wrong_consumer$provider_runs$provider_run_id[[consumer_row]] <- run_id(
    wrong_consumer$provider_runs[consumer_row, , drop = FALSE]
  )
  wrong_consumer$calibration_lineage$consumer_provider_run_id <-
    wrong_consumer$provider_runs$provider_run_id[[consumer_row]]
  wrong_consumer$calibration_lineage$calibration_lineage_id <- lineage_id(
    wrong_consumer$calibration_lineage
  )
  provider_contract_expect_error(
    stpd_validate_provider_bundle(wrong_consumer),
    "calibration_lineage_invalid", "calibration_lineage"
  )
})

test_that("coordinate formulas freeze canonical boundary examples without an adapter", {
  formulas <- stpd_provider_contract_registry()$coordinate_formulas
  expect_identical(formulas, provider_contract_expected_coordinate_formulas())

  # These literal examples prevent an off-by-one reinterpretation by later
  # adapters.  They intentionally do not call a coordinate normalizer in Phase A.
  examples <- data.frame(
    convention = c(
      "diff_one_based", "diff_zero_based", "spike_one_based",
      "spike_zero_based", "per_isi_one_based", "per_isi_zero_based"
    ),
    source_start = c(1L, 0L, 1L, 0L, 1L, 0L),
    source_end = c(4L, 3L, 5L, 4L, 4L, 3L),
    canonical_start_isi = rep(2L, 6L),
    canonical_end_isi = rep(5L, 6L),
    canonical_start_spike = rep(1L, 6L),
    canonical_end_spike = rep(5L, 6L),
    stringsAsFactors = FALSE
  )
  expect_identical(examples$canonical_start_isi, rep(2L, 6L))
  expect_identical(examples$canonical_end_isi, rep(5L, 6L))
  expect_identical(examples$canonical_start_spike,
                   examples$canonical_start_isi - 1L)
  expect_identical(examples$canonical_end_spike,
                   examples$canonical_end_isi)

  closures <- data.frame(
    closure = c(
      "closed", "left_closed_right_open", "left_open_right_closed", "open"
    ),
    source = c("[1,4]", "[1,4)", "(1,4]", "(1,4)"),
    closed_start = c(1L, 1L, 2L, 2L),
    closed_end = c(4L, 3L, 4L, 3L),
    stringsAsFactors = FALSE
  )
  expect_identical(closures$closed_start, c(1L, 1L, 2L, 2L))
  expect_identical(closures$closed_end, c(4L, 3L, 4L, 3L))

  timestamps <- c(0.10, 0.14, 0.20, 0.31, 0.50)
  expect_identical(c(start_isi = 3L, end_isi = 5L), c(
    start_isi = match(0.14, timestamps) + 1L,
    end_isi = match(0.50, timestamps)
  ))
})
