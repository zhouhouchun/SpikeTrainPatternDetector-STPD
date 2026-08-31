#!/usr/bin/env Rscript

contract_path <- "docs/gate-b-v3-normative-data-identity-release-contract.md"
plan_path <- "docs/gate-b-v3-implementation-and-acceptance-plan.md"
out_path <- "inst/config/gate_b_v3_phase1_schema_bundle.json"

sha256_file <- function(path) digest::digest(path, algo = "sha256", file = TRUE)
read_exact_utf8 <- function(path) {
  rawToChar(readBin(path, what = "raw", n = file.info(path)$size))
}
`%||%` <- function(x, y) if (is.null(x)) y else x

lines <- readLines(contract_path, warn = FALSE, encoding = "UTF-8")
blocks <- list()
inside <- FALSE
for (i in seq_along(lines)) {
  if (identical(lines[[i]], "```text")) {
    inside <- TRUE
    start <- i + 1L
  } else if (inside && identical(lines[[i]], "```")) {
    blocks[[length(blocks) + 1L]] <- list(
      start_line = start,
      text = paste(lines[start:(i - 1L)], collapse = "\n")
    )
    inside <- FALSE
  }
}

parse_fields <- function(text) {
  tokens <- trimws(unlist(strsplit(gsub("[\r\n]+", " ", text), ",")))
  tokens <- tokens[nzchar(tokens)]
  match <- regexec(
    "^([A-Za-z][A-Za-z0-9_]*):(chr|int|dbl|lgl|json|lst)([!?])$",
    tokens
  )
  parts <- regmatches(tokens, match)
  ok <- lengths(parts) == 4L
  if (!all(ok)) {
    stop("Unparsed schema token(s): ", paste(tokens[!ok], collapse = " | "))
  }
  lapply(parts, function(x) list(
    name = x[[2L]], type = x[[3L]], nullable = identical(x[[4L]], "?")
  ))
}

block_names <- c(
  `9` = "product_status_envelope",
  `10` = "materialized_product_identity",
  `11` = "state_candidates",
  `12` = "state_episodes",
  `13` = "state_segments",
  `14` = "per_isi",
  `15` = "event_candidates",
  `16` = "events",
  `17` = "gaps",
  `18` = "boundary_evidence",
  `19` = "state_abstentions",
  `20` = "state_axis_evidence",
  `21` = "gap_candidates",
  `22` = "gap_abstentions",
  `23` = "state_connector_decisions",
  `24` = "event_modifier_evidence",
  `25` = "review_candidates",
  `26` = "event_state_relationships",
  `27` = "event_interrupted_state_relationships",
  `28` = "state_episode_links",
  `29` = "entity_evidence_edges",
  `30` = "threshold_policies",
  `31` = "partition_memberships",
  `32` = "threshold_instances",
  `33` = "threshold_instance_bindings",
  `34` = "diagnostic_records",
  `35` = "adjudication_transitions",
  `36` = "contract_registries",
  `37` = "entity_domain_registry",
  `38` = "evidence_records",
  `39` = "lineage_records",
  `40` = "scientific_context",
  `41` = "scientific_context_epochs",
  `42` = "reference_annotations",
  `43` = "reference_display_protocols",
  `44` = "reference_adjudications",
  `45` = "reference_project_manifest",
  `46` = "reference_manifest",
  `50` = "canonical_manifest",
  `51` = "artifact_manifest",
  `54` = "release_attestation",
  `55` = "gate_b_approval_manifest",
  `56` = "gate_b_evidence_records",
  `57` = "fixture_expected_results",
  `58` = "test_result_bundle",
  `59` = "signature_envelope",
  `61` = "consumer_response",
  `65` = "migration_records"
)

tables <- list()
for (idx in names(block_names)) {
  name <- unname(block_names[[idx]])
  block <- blocks[[as.integer(idx)]]
  tables[[name]] <- list(
    source_block_start = as.integer(block$start_line),
    columns = parse_fields(block$text)
  )
}

leaf_text <- blocks[[48L]]$text
leaf_lines <- strsplit(leaf_text, "\n", fixed = TRUE)[[1L]]
headers <- grep("^[a-z][a-z0-9_]+:$", leaf_lines)
for (j in seq_along(headers)) {
  first <- headers[[j]]
  last <- if (j < length(headers)) headers[[j + 1L]] - 1L else length(leaf_lines)
  name <- sub(":$", "", leaf_lines[[first]])
  field_lines <- leaf_lines[(first + 1L):last]
  field_lines <- field_lines[grepl(
    "[A-Za-z][A-Za-z0-9_]*:(chr|int|dbl|lgl|json|lst)[!?]", field_lines
  )]
  tables[[name]] <- list(
    source_block_start = as.integer(blocks[[48L]]$start_line + first - 1L),
    columns = parse_fields(paste(field_lines, collapse = " "))
  )
}

pk <- list(
  product_status_envelope = "product_kind",
  materialized_product_identity = c("product_kind", "detection_root_id", "product_hash"),
  state_candidates = "state_candidate_id",
  state_episodes = "state_episode_id",
  state_segments = "state_segment_id",
  per_isi = c("detection_root_id", "train", "isi_index"),
  event_candidates = "event_candidate_id",
  events = "event_id",
  gaps = "gap_id",
  boundary_evidence = "boundary_id",
  state_abstentions = "abstention_id",
  state_axis_evidence = "state_axis_evidence_id",
  gap_candidates = "gap_candidate_id",
  gap_abstentions = "gap_abstention_id",
  state_connector_decisions = "connector_decision_id",
  event_modifier_evidence = "modifier_evidence_id",
  review_candidates = "review_candidate_id",
  event_state_relationships = "relationship_id",
  event_interrupted_state_relationships = "interruption_relationship_id",
  state_episode_links = "episode_link_id",
  entity_evidence_edges = c("entity_domain_id", "entity_id", "edge_index"),
  threshold_policies = "threshold_policy_id",
  partition_memberships = c("partition_id", "group_id_hash"),
  threshold_instances = "threshold_instance_id",
  threshold_instance_bindings = "binding_id",
  diagnostic_records = "diagnostic_id",
  adjudication_transitions = "transition_id",
  contract_registries = "registry_entry_id",
  entity_domain_registry = "entity_domain_id",
  evidence_records = "evidence_id",
  lineage_records = c("lineage_id", "edge_index"),
  scientific_context = c("detection_root_id", "train"),
  scientific_context_epochs = "context_epoch_id",
  reference_annotations = "annotation_id",
  reference_display_protocols = "display_protocol_id",
  reference_adjudications = "adjudication_id",
  reference_project_manifest = "annotation_project_id",
  reference_manifest = "table_name",
  canonical_manifest = "table_name",
  artifact_manifest = "artifact_path",
  release_attestation = "release_version",
  gate_b_approval_manifest = "approval_payload_sha256",
  gate_b_evidence_records = "evidence_record_id",
  fixture_expected_results = c("contract_id", "fixture_id", "repository_relative_test_path", "test_name"),
  test_result_bundle = "stable_test_id",
  signature_envelope = c("signature_domain", "payload_sha256", "key_id", "release_sequence"),
  consumer_response = c("product_kind", "status_code"),
  migration_records = "migration_id",
  normalized_input_manifest = "train",
  requested_params_manifest = "path",
  partition_membership_source_manifest = c("partition_id", "group_id_hash"),
  threshold_policy_source_manifest = c("path", "threshold_source_class", "adaptation_unit", "policy_parameter_hash", "partition_id"),
  threshold_instance_source_manifest = c("policy_source_row_hash", "path", "application_scope", "source_scope_key_hash"),
  source_manifest = "repository_relative_path",
  native_manifest = "repository_relative_path",
  toolchain_manifest = "toolchain_id",
  detector_policy_manifest = "policy_path",
  label_blind_contract_manifest = "contract_rule_registry_id",
  qc_input_manifest = "train"
)

sort_keys <- list(
  product_status_envelope = "product_kind",
  materialized_product_identity = c("product_kind", "detection_root_id", "product_hash"),
  state_candidates = c("train", "start_isi", "end_isi", "state_candidate_id"),
  state_episodes = c("train", "start_isi", "end_isi", "state_class", "state_episode_id"),
  state_segments = c("train", "state_episode_id", "segment_index"),
  per_isi = c("train", "isi_index"),
  event_candidates = c("train", "start_isi", "end_isi", "event_candidate_id"),
  events = c("train", "start_isi", "end_isi", "event_id"),
  gaps = c("train", "start_isi", "end_isi", "gap_id"),
  boundary_evidence = c("train", "start_isi", "end_isi", "boundary_domain", "boundary_id"),
  state_abstentions = c("train", "start_isi", "end_isi", "abstention_id"),
  state_axis_evidence = c("train", "start_isi", "end_isi", "state_candidate_id"),
  gap_candidates = c("train", "start_isi", "end_isi", "gap_candidate_id"),
  gap_abstentions = c("train", "start_isi", "end_isi", "gap_abstention_id"),
  state_connector_decisions = c("train", "gap_start_isi", "gap_end_isi", "pre_direct_candidate_id", "post_direct_candidate_id", "connector_decision_id"),
  event_modifier_evidence = c("event_id", "modifier_domain"),
  review_candidates = c("train", "start_isi", "end_isi", "review_candidate_id"),
  event_state_relationships = c("train", "event_id", "state_episode_id"),
  event_interrupted_state_relationships = c("train", "event_id", "parent_state_candidate_id"),
  state_episode_links = c("train", "pre_state_episode_id", "gap_id", "post_state_episode_id"),
  entity_evidence_edges = c("entity_domain_id", "entity_id", "edge_index"),
  threshold_policies = c("path", "threshold_policy_id"),
  partition_memberships = c("partition_id", "group_id_hash"),
  threshold_instances = c("path", "application_scope", "application_scope_key_hash", "threshold_instance_id"),
  threshold_instance_bindings = c("consumer_domain_id", "consumer_id", "path", "binding_role_registry_id"),
  diagnostic_records = c("severity", "diagnostic_code_registry_id", "entity_domain_id", "entity_id", "diagnostic_id"),
  adjudication_transitions = "sequence_no",
  contract_registries = "registry_entry_id",
  entity_domain_registry = "entity_domain_id",
  evidence_records = "evidence_id",
  lineage_records = c("lineage_id", "edge_index"),
  scientific_context = "train",
  scientific_context_epochs = c("train", "start_isi", "end_isi", "epoch_domain"),
  reference_annotations = c("annotation_project_id", "annotator_pseudonym", "annotation_round", "annotation_pass", "train", "annotation_domain", "start_isi", "end_isi", "annotation_id"),
  reference_display_protocols = c("protocol_version", "display_protocol_id"),
  reference_adjudications = c("annotation_project_id", "train", "annotation_domain", "start_isi", "end_isi", "adjudication_id"),
  reference_project_manifest = "annotation_project_id",
  reference_manifest = "table_name",
  canonical_manifest = "table_name",
  artifact_manifest = "artifact_path",
  release_attestation = "release_version",
  gate_b_approval_manifest = "approval_payload_sha256",
  gate_b_evidence_records = c("contract_id", "fixture_id", "repository_relative_test_path", "test_name"),
  fixture_expected_results = c("contract_id", "fixture_id", "repository_relative_test_path", "test_name"),
  test_result_bundle = c("contract_id", "fixture_id", "repository_relative_test_path", "test_name", "stable_test_id"),
  signature_envelope = c("signature_domain", "payload_sha256", "key_id", "release_sequence"),
  consumer_response = c("product_kind", "status_code"),
  migration_records = c("source_product_hash", "source_entity_domain_id", "source_entity_id", "target_detection_root_id", "target_entity_domain_id", "target_entity_id", "mapping_status"),
  normalized_input_manifest = "train",
  requested_params_manifest = "path",
  partition_membership_source_manifest = c("partition_id", "group_id_hash"),
  threshold_policy_source_manifest = c("path", "threshold_source_class", "adaptation_unit", "policy_parameter_hash", "partition_id"),
  threshold_instance_source_manifest = c("policy_source_row_hash", "path", "application_scope", "source_scope_key_hash"),
  source_manifest = "repository_relative_path",
  native_manifest = "repository_relative_path",
  toolchain_manifest = "toolchain_id",
  detector_policy_manifest = "policy_path",
  label_blind_contract_manifest = "contract_rule_registry_id",
  qc_input_manifest = "train"
)

if (!setequal(names(tables), names(pk)) || !setequal(names(tables), names(sort_keys))) {
  stop("Every Gate B v3 table must have an explicit PK and sort key.")
}

unique_extra <- list(
  state_candidates = c("detection_root_id", "train", "start_isi", "end_isi", "candidate_generation_rule_registry_id", "source_candidate_key_hash"),
  state_episodes = c("detection_root_id", "train", "state_class", "start_isi", "end_isi"),
  state_segments = c("state_episode_id", "segment_index"),
  event_candidates = c("detection_root_id", "train", "start_isi", "end_isi", "candidate_generation_rule_registry_id", "source_candidate_key_hash"),
  events = c("detection_root_id", "train", "event_class", "start_isi", "end_isi"),
  state_abstentions = "state_candidate_id",
  state_axis_evidence = c("state_candidate_id", "start_isi", "end_isi"),
  gap_candidates = c("detection_root_id", "train", "start_isi", "end_isi", "gap_policy_registry_id"),
  gap_abstentions = "gap_candidate_id",
  state_connector_decisions = c("pre_direct_candidate_id", "gap_start_isi", "gap_end_isi", "post_direct_candidate_id"),
  event_modifier_evidence = c("event_id", "modifier_domain"),
  event_state_relationships = c("event_id", "state_episode_id"),
  event_interrupted_state_relationships = c("event_id", "parent_state_candidate_id"),
  state_episode_links = c("pre_state_episode_id", "gap_id", "post_state_episode_id"),
  entity_evidence_edges = c("entity_domain_id", "entity_id", "evidence_role_registry_id", "source_domain_id", "source_row_key_json", "source_product_hash"),
  threshold_policies = c("path", "threshold_source_class", "adaptation_unit", "policy_parameter_hash", "partition_id"),
  threshold_instances = c("threshold_policy_id", "application_scope", "application_scope_key_hash"),
  threshold_instance_bindings = c("consumer_domain_id", "consumer_id", "path", "binding_role_registry_id"),
  adjudication_transitions = c("detection_root_id", "sequence_no"),
  contract_registries = c("registry_domain", "code", "code_version"),
  entity_domain_registry = "entity_domain",
  lineage_records = c("child_domain_id", "child_id", "parent_domain_id", "parent_id", "lineage_action_registry_id"),
  scientific_context_epochs = c("train", "epoch_domain", "start_isi", "end_isi"),
  reference_annotations = c("annotation_project_id", "annotator_pseudonym", "annotation_round", "annotation_pass", "train", "annotation_domain", "start_isi", "end_isi", "edge_left_annotation_id", "edge_right_annotation_id"),
  reference_display_protocols = "protocol_version",
  gate_b_evidence_records = c("contract_id", "fixture_id", "repository_relative_test_path", "test_name"),
  test_result_bundle = c("contract_id", "fixture_id", "repository_relative_test_path", "test_name"),
  migration_records = c("source_product_hash", "source_entity_domain_id", "source_entity_id", "target_detection_root_id", "target_entity_domain_id", "target_entity_id", "mapping_status")
)

enums <- list(
  product_kind = c("auto", "final"),
  state_frequency_class = c("high", "non_high", "unresolved"),
  state_regularity_class = c("regular", "irregular", "unresolved"),
  evidence_status = c("pass", "fail", "gray_zone", "insufficient"),
  state_class = c("high_frequency_irregular_state", "high_frequency_tonic", "tonic"),
  segment_role = c("direct_support", "tolerated_connector"),
  product_status = c("not_present", "materializing", "candidate_pending", "gate_b_authoritative", "failed_closed", "requires_redetection"),
  gate_b_status = c("pending", "attested", "failed"),
  gate_c_status = "pending",
  reference_standard_status = c("none", "independent_reference_available", "adjudicated_reference_available"),
  authority_scope = c("none_candidate", "automatic_prediction_record", "reviewed_prediction_record"),
  integrity_level = c("none", "detached_internal_integrity", "parent_bound_recomputed", "externally_anchored_product"),
  failure_stage = c("input_normalization", "materialization", "validation", "replay", "migration", "resource_budget", "export", "attestation", "cancelled"),
  candidate_status = c("pending_review", "review_confirmed", "review_rejected"),
  state_candidate_decision = c("accepted_direct_support", "abstained", "rejected", "interrupted_parent"),
  event_candidate_decision = c("accepted_event", "review_candidate", "rejected"),
  record_status = c("predicted", "review_confirmed", "review_rejected"),
  event_class = "burst_event",
  event_candidate_class = "burst_candidate",
  extent_modifier = c("classic", "long", "prolonged", "unresolved"),
  frequency_modifier = c("high", "non_high", "unresolved"),
  gap_class = "canonical_pause",
  gap_semantics = "predicted_statistical_pause",
  boundary_domain = c("predicted_pause", "qc", "algorithmic", "acquisition_boundary"),
  boundary_geometry = c("isi_span", "recording_edge"),
  edge_side = c("none", "left", "right", "both"),
  gap_decision = c("accepted_pause", "rejected", "abstain"),
  qc_eligibility = c("eligible", "timestamp_only_uncertain", "ineligible"),
  equivalence_status = c("equivalent", "not_equivalent", "insufficient"),
  connector_decision = c("connect", "do_not_connect", "abstain"),
  modifier_domain = c("extent", "frequency"),
  candidate_domain = c("event", "state", "gap", "episode_link"),
  event_state_relation_type = "event_overlaps_state_episode",
  episode_link_relation_type = "pause_interrupted_hf",
  link_status = c("algorithmic_candidate", "review_confirmed", "review_rejected"),
  threshold_source_class = c("fixed_preregistered", "development_trained", "per_train_unsupervised", "batch_transductive"),
  adaptation_unit = c("none", "train", "session", "dataset_batch"),
  label_access = c("none", "development_reference_only"),
  application_scope = c("global", "train", "session", "dataset_batch"),
  requested_value_type = c("double", "integer", "logical", "string", "null"),
  threshold_value_type = c("double", "string"),
  severity = c("info", "warning", "error"),
  action_type = c("event_candidate_accept", "review_candidate_reject", "event_projection_reject", "state_projection_reject", "state_split", "gap_projection_reject", "episode_link_confirm", "episode_link_reject", "compensate"),
  registry_domain = strsplit("classification_rule|prediction_class|state_candidate_rule|event_candidate_rule|segment_rule|gap_policy|boundary_class|boundary_rule|reason_code|failure_code|diagnostic_code|diagnostic_detail_schema|transition_payload_schema|threshold_algorithm|operational_policy|label_blind_rule|input_allowlist_schema|input_source_domain|binding_role|lineage_action|evidence_role|evidence_type|evidence_schema|estimator|equivalence_method|multiple_testing_method|episode_aggregation|event_selector|qc_status|reference_label|reference_reason|species|brain_region|brain_subregion|acquisition_condition|anesthesia_status|medication_status|task_status|timebase_provenance|spike_sorting_method|sorting_qc_status|isolation_metric_name|units|nonstationarity_status|context_epoch_status", "\\|", fixed = FALSE)[[1]],
  group_role = c("development", "calibration", "validation", "excluded"),
  context_completeness = c("complete", "partial", "minimal_timestamp_only"),
  rater_role = c("primary", "secondary", "repeat_rater", "adjudicator"),
  confidence_code = c("high", "medium", "low", "not_rateable"),
  adjudication_status = c("resolved", "uncertain", "unscorable"),
  time_scale_policy = c("fixed_absolute", "fixed_relative_to_candidate"),
  waveform_display_policy = c("hidden", "qc_only", "visible"),
  epoch_domain = c("condition", "nonstationarity", "sorting_qc", "acquisition"),
  artifact_role = c("canonical_json", "interchange_csv", "typed_rds", "identity", "manifest", "diagnostic_log"),
  evidence_record_status = c("pass", "fail", "error", "skip"),
  consumer_status = c("ok", "not_present", "pending_not_official", "failed_closed", "requires_redetection", "unsupported_transition_for_schema", "unknown_schema", "integrity_insufficient", "attestation_invalid", "product_anchor_missing", "version_conflict", "corrupt_product"),
  migration_status = c("redetected_new_identity", "transition_imported_exact", "archive_only", "requires_readjudication", "requires_redetection", "rejected_tampered")
)

enum_by_field <- c(
  product_kind="product_kind", product_status="product_status", gate_b_status="gate_b_status", gate_c_status="gate_c_status", reference_standard_status="reference_standard_status", authority_scope="authority_scope", integrity_level="integrity_level", failure_stage="failure_stage",
  state_frequency_class="state_frequency_class", state_regularity_class="state_regularity_class", frequency_evidence_status="evidence_status", regularity_evidence_status="evidence_status", state_class="state_class", segment_role="segment_role", record_status="record_status", event_class="event_class", extent_modifier="extent_modifier", frequency_modifier="frequency_modifier", gap_class="gap_class", gap_semantics="gap_semantics", gap_evidence_status="evidence_status", qc_eligibility="qc_eligibility", boundary_domain="boundary_domain", boundary_geometry="boundary_geometry", edge_side="edge_side", decision="gap_decision", modifier_domain="modifier_domain", evidence_status="evidence_status", candidate_domain="candidate_domain", candidate_status="candidate_status", link_status="link_status", equivalence_status="equivalence_status", threshold_source_class="threshold_source_class", adaptation_unit="adaptation_unit", label_access="label_access", application_scope="application_scope", severity="severity", action_type="action_type", registry_domain="registry_domain", group_role="group_role", epoch_domain="epoch_domain", context_completeness="context_completeness", rater_role="rater_role", confidence_code="confidence_code", adjudication_status="adjudication_status", time_scale_policy="time_scale_policy", waveform_display_policy="waveform_display_policy", artifact_role="artifact_role", status="evidence_record_status", status_code="consumer_status", mapping_status="migration_status"
)

enum_by_table_field <- list(
  state_candidates = c(candidate_decision = "state_candidate_decision"),
  event_candidates = c(candidate_decision = "event_candidate_decision",
                       candidate_class = "event_candidate_class"),
  event_state_relationships = c(relation_type = "event_state_relation_type"),
  state_episode_links = c(relation_type = "episode_link_relation_type"),
  requested_params_manifest = c(value_type = "requested_value_type"),
  detector_policy_manifest = c(value_type = "requested_value_type"),
  threshold_instances = c(value_type = "threshold_value_type"),
  threshold_instance_source_manifest = c(value_type = "threshold_value_type")
)

constant_by_field <- c(
  schema_version="stpd_multitrack_v3_1",
  status_schema="stpd_multitrack_v3_status_1",
  identity_schema="stpd_multitrack_v3_identity_1",
  manifest_schema="stpd_multitrack_v3_manifest_1",
  artifact_manifest_schema="stpd_multitrack_v3_artifact_manifest_1",
  response_schema="stpd_multitrack_v3_response_1",
  evidence_record_schema="stpd_gate_b_v3_evidence_record_1",
  reference_schema="stpd_multitrack_v3_reference_1",
  migration_schema="stpd_multitrack_v3_migration_1",
  approval_schema="stpd_gate_b_v3_approval_1",
  signature_schema="stpd_gate_b_v3_signature_1",
  test_bundle_schema="stpd_gate_b_v3_test_bundle_1",
  release_attestation_schema="stpd_gate_b_v3_release_attestation_1"
)

constant_by_table_field <- list(
  event_candidates = c(candidate_class="burst_candidate"),
  state_episode_links = c(biological_ground_truth=FALSE),
  product_status_envelope = c(gate_c_status="pending",
    detector_performance_eligible=FALSE, biological_ground_truth=FALSE)
)

for (name in names(tables)) {
  # The per-table semantic schema and the canonical serialization envelope are
  # deliberately different identities (contract section 2.5/8.3).
  tables[[name]]$table_schema <- paste0("stpd_multitrack_v3_", name, "_1")
  tables[[name]]$canonical_envelope_schema <- "stpd_canonical_table_v1"
  tables[[name]]$primary_key <- as.list(pk[[name]])
  tables[[name]]$unique_keys <- list(as.list(pk[[name]]))
  if (!is.null(unique_extra[[name]])) {
    tables[[name]]$unique_keys <- c(tables[[name]]$unique_keys,
                                    list(as.list(unique_extra[[name]])))
  }
  tables[[name]]$sort_key <- as.list(sort_keys[[name]])
  tables[[name]]$columns <- lapply(tables[[name]]$columns, function(field) {
    enum_name <- unname(enum_by_field[field$name])
    if (length(enum_name) == 0L || is.na(enum_name)) enum_name <- "none"
    if (identical(field$name, "decision")) {
      enum_name <- if (identical(name, "state_connector_decisions")) {
        "connector_decision"
      } else "gap_decision"
    }
    table_override <- enum_by_table_field[[name]][field$name]
    if (length(table_override) == 1L && !is.na(table_override)) {
      enum_name <- unname(table_override)
    }
    field$enum <- unname(enum_name)
    field$default_policy <- if (isTRUE(field$nullable)) {
      paste0("typed_na_", field$type)
    } else "no_implicit_default"
    constant <- unname(constant_by_field[field$name])
    if (length(constant) == 0L) constant <- NA_character_
    table_constant <- unname(constant_by_table_field[[name]][field$name])
    if (length(table_constant) == 1L && !is.na(table_constant)) {
      constant <- table_constant
    }
    field$constant <- constant
    field
  })
}

generated_id_domains <- c(
  state_candidates="state_candidate", state_axis_evidence="state_axis_evidence",
  state_episodes="state_episode", state_segments="state_segment",
  event_candidates="event_candidate", events="event",
  event_modifier_evidence="event_modifier_evidence", gap_candidates="gap_candidate",
  gaps="gap", boundary_evidence="boundary", state_abstentions="state_abstention",
  gap_abstentions="gap_abstention", state_connector_decisions="connector_decision",
  review_candidates="review_candidate", event_state_relationships="event_state_relationship",
  event_interrupted_state_relationships="interrupted_state_relationship",
  state_episode_links="episode_link", scientific_context_epochs="scientific_context_epoch",
  evidence_records="evidence", lineage_records="lineage", entity_domain_registry="entity_domain",
  threshold_policies="threshold_policy", threshold_instances="threshold_instance",
  threshold_instance_bindings="threshold_binding", diagnostic_records="diagnostic",
  migration_records="migration", adjudication_transitions="transition"
)
generated_prefixes <- c(
  state_candidate="sc_", state_axis_evidence="sx_", state_episode="se_",
  state_segment="ss_", event_candidate="ec_", event="ev_",
  event_modifier_evidence="me_", gap_candidate="gc_", gap="gp_",
  boundary="bd_", state_abstention="sa_", gap_abstention="ga_",
  connector_decision="cd_", review_candidate="rv_",
  event_state_relationship="er_", interrupted_state_relationship="ir_",
  episode_link="lk_", scientific_context_epoch="ce_", evidence="ed_",
  lineage="ln_", entity_domain="dm_", threshold_policy="tp_",
  threshold_instance="ti_", threshold_binding="tb_", diagnostic="dg_",
  migration="mg_", transition="tx_"
)
target_by_field <- c(
  state_candidate_id="state_candidates", source_state_candidate_id="state_candidates",
  parent_state_candidate_id="state_candidates", pre_direct_candidate_id="state_candidates",
  post_direct_candidate_id="state_candidates", state_axis_evidence_id="state_axis_evidence",
  parent_state_axis_evidence_id="state_axis_evidence", classification_evidence_id="evidence_records",
  state_episode_id="state_episodes", pre_state_episode_id="state_episodes",
  post_state_episode_id="state_episodes", state_segment_id="state_segments",
  active_state_segment_id="state_segments", event_candidate_id="event_candidates",
  event_id="events", gap_candidate_id="gap_candidates", gap_id="gaps",
  source_gap_id="gaps", controlling_boundary_id="boundary_evidence",
  scientific_context_epoch_id="scientific_context_epochs",
  connector_decision_id="state_connector_decisions", modifier_evidence_id="event_modifier_evidence",
  extent_modifier_evidence_id="event_modifier_evidence", frequency_modifier_evidence_id="event_modifier_evidence",
  review_candidate_id="review_candidates", relationship_id="event_state_relationships",
  interruption_relationship_id="event_interrupted_state_relationships",
  episode_link_id="state_episode_links", threshold_policy_id="threshold_policies",
  threshold_instance_id="threshold_instances", instance_evidence_id="evidence_records",
  evidence_id="evidence_records", base_event_evidence_id="evidence_records",
  gap_evidence_id="evidence_records", qc_evidence_id="evidence_records",
  link_evidence_id="evidence_records", source_evidence_id="evidence_records",
  lineage_id="lineage_records", partition_id="partition_memberships",
  annotation_project_id="reference_project_manifest", display_protocol_id="reference_display_protocols",
  repeat_of_annotation_id="reference_annotations", supersedes_annotation_id="reference_annotations",
  edge_left_annotation_id="reference_annotations", edge_right_annotation_id="reference_annotations",
  transition_id="adjudication_transitions", compensates_transition_id="adjudication_transitions"
)
registry_domain_by_field <- c(
  classification_rule_registry_id="classification_rule",
  segment_rule_registry_id="segment_rule", decision_reason_registry_id="reason_code",
  gap_policy_registry_id="gap_policy", boundary_class_registry_id="boundary_class",
  reason_registry_id="reason_code", qc_status_registry_id="qc_status",
  boundary_rule_registry_id="boundary_rule", frequency_estimator_registry_id="estimator",
  regularity_estimator_registry_id="estimator", episode_aggregation_registry_id="episode_aggregation",
  insufficient_reason_registry_id="reason_code", local_estimator_registry_id="estimator",
  multiple_testing_method_registry_id="multiple_testing_method", equivalence_method_registry_id="equivalence_method",
  estimator_registry_id="estimator", suggested_class_registry_id="prediction_class",
  link_policy_registry_id="operational_policy", evidence_role_registry_id="evidence_role",
  threshold_algorithm_registry_id="threshold_algorithm", binding_role_registry_id="binding_role",
  diagnostic_code_registry_id="diagnostic_code", detail_schema_registry_id="diagnostic_detail_schema",
  payload_schema_registry_id="transition_payload_schema", evidence_type_registry_id="evidence_type",
  evidence_schema_registry_id="evidence_schema",
  lineage_action_registry_id="lineage_action", species_registry_id="species",
  brain_region_registry_id="brain_region", brain_subregion_registry_id="brain_subregion",
  acquisition_condition_registry_id="acquisition_condition", anesthesia_status_registry_id="anesthesia_status",
  medication_status_registry_id="medication_status", task_status_registry_id="task_status",
  timebase_provenance_registry_id="timebase_provenance", spike_sorting_method_registry_id="spike_sorting_method",
  sorting_qc_status_registry_id="sorting_qc_status", isolation_metric_name_registry_id="isolation_metric_name",
  isolation_metric_units_registry_id="units", nonstationarity_status_registry_id="nonstationarity_status",
  epoch_status_registry_id="context_epoch_status", reference_class_registry_id="reference_label",
  uncertainty_reason_registry_id="reference_reason", adjudicated_class_registry_id="reference_label",
  derivation_statistics_schema_registry_id="evidence_schema", application_statistics_schema_registry_id="evidence_schema",
  contract_rule_registry_id="label_blind_rule", allowlist_schema_registry_id="input_allowlist_schema",
  source_domain_registry_id="input_source_domain", governing_registry_entry_id="operational_policy",
  sort_contract_registry_id="operational_policy", failure_code_registry_id="failure_code",
  requires_redetection_reason_registry_id="reason_code"
)

# A same-named FK can have different semantic domains in different tables.
registry_domain_by_table_field <- list(
  state_candidates = c(candidate_generation_rule_registry_id="state_candidate_rule"),
  event_candidates = c(candidate_generation_rule_registry_id="event_candidate_rule")
)
external_ids <- c(
  detection_root_id="^dr_[0-9a-f]{64}$", execution_id="^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$",
  native_build_id="^[A-Za-z0-9._:-]+$", toolchain_id="^[0-9a-f]{64}$",
  key_id="^[A-Za-z0-9._:-]+$", stable_test_id="^[0-9a-f]{64}$",
  contract_id="^GB3-[A-Z0-9-]+$", fixture_id="^[a-z0-9_:-]+$"
)
polymorphic_ids <- c("entity_id","consumer_id","target_id","child_id","parent_id",
                     "source_entity_id","target_entity_id","reference_record_id")

for (table_name in names(tables)) {
  fields <- vapply(tables[[table_name]]$columns, `[[`, character(1), "name")
  tables[[table_name]]$foreign_keys <- list()
  tables[[table_name]]$id_resolution <- list()
  own_domain <- unname(generated_id_domains[table_name])
  if (length(own_domain) == 0L || is.na(own_domain)) own_domain <- NULL
  own_pk <- unlist(tables[[table_name]]$primary_key, use.names=FALSE)
  if (!is.null(own_domain) && length(own_pk) == 1L) {
    tables[[table_name]]$id_resolution[[own_pk]] <- list(
      resolution="generated_entity", domain=own_domain,
      prefix=unname(generated_prefixes[own_domain])
    )
  }
  for (field in fields[grepl("_id$", fields)]) {
    if (!is.null(tables[[table_name]]$id_resolution[[field]])) next
    target_value <- unname(target_by_field[field])
    registry_value <- unname(registry_domain_by_table_field[[table_name]][field])
    if (length(registry_value) == 0L || is.na(registry_value)) {
      registry_value <- unname(registry_domain_by_field[field])
    }
    external_value <- unname(external_ids[field])
    self_generated_special <- field %in% c("annotation_id","adjudication_id",
      "display_protocol_id","annotation_project_id") &&
      field %in% unlist(pk[[table_name]],use.names=FALSE)
    if (self_generated_special) {
      special <- list(
        annotation_id=list(domain="reference_annotation",prefix="ra_"),
        adjudication_id=list(domain="reference_adjudication",prefix="rj_"),
        display_protocol_id=list(domain="reference_display",prefix="rd_"),
        annotation_project_id=list(domain="reference_project",prefix="rp_")
      )[[field]]
      tables[[table_name]]$id_resolution[[field]] <- c(
        list(resolution="generated_special_contract_id"),special)
    } else if (length(target_value) == 1L && !is.na(target_value)) {
      target <- target_value
      target_pk <- unlist(pk[[target]], use.names=FALSE)
      tables[[table_name]]$foreign_keys[[length(tables[[table_name]]$foreign_keys)+1L]] <- list(
        columns=field, target_table=target, target_columns=target_pk[[1]],
        expected_registry_domain="none"
      )
      tables[[table_name]]$id_resolution[[field]] <- list(
        resolution="foreign_key", target_table=target, target_column=target_pk[[1]]
      )
    } else if (identical(field,"created_by_registry_entry_id")) {
      allowed_creator_domains <- c("classification_rule","state_candidate_rule",
        "segment_rule","gap_policy","boundary_rule","threshold_algorithm",
        "event_candidate_rule","event_selector","equivalence_method","estimator","episode_aggregation")
      tables[[table_name]]$foreign_keys[[length(tables[[table_name]]$foreign_keys)+1L]] <- list(
        columns=field,target_table="contract_registries",
        target_columns="registry_entry_id",
        expected_registry_domain="creator_domain_allowlist",
        allowed_registry_domains=as.list(allowed_creator_domains)
      )
      tables[[table_name]]$id_resolution[[field]] <- list(
        resolution="registry_foreign_key_allowlist",
        allowed_registry_domains=as.list(allowed_creator_domains)
      )
    } else if (length(registry_value) == 1L && !is.na(registry_value)) {
      domain <- registry_value
      tables[[table_name]]$foreign_keys[[length(tables[[table_name]]$foreign_keys)+1L]] <- list(
        columns=field, target_table="contract_registries", target_columns="registry_entry_id",
        expected_registry_domain=domain
      )
      tables[[table_name]]$id_resolution[[field]] <- list(
        resolution="registry_foreign_key", expected_registry_domain=domain
      )
    } else if (length(external_value) == 1L && !is.na(external_value)) {
      tables[[table_name]]$id_resolution[[field]] <- list(
        resolution="external_audit_identifier", pattern=external_value
      )
    } else if (field %in% polymorphic_ids || grepl("_domain_id$", field)) {
      tables[[table_name]]$id_resolution[[field]] <- list(
        resolution="polymorphic_entity_domain", resolver_table="entity_domain_registry"
      )
    } else if (field %in% c("multiple_testing_family_id")) {
      tables[[table_name]]$id_resolution[[field]] <- list(
        resolution="generated_entity", domain="multiple_testing_family", prefix="mf_"
      )
    } else if (field %in% c("registry_entry_id", "annotation_id",
                            "adjudication_id", "evidence_record_id")) {
      special <- list(
        registry_entry_id=list(domain="registry_entry",prefix="reg_"),
        annotation_id=list(domain="reference_annotation",prefix="ra_"),
        adjudication_id=list(domain="reference_adjudication",prefix="rj_"),
        evidence_record_id=list(domain="gate_b_evidence_record",prefix="")
      )[[field]]
      tables[[table_name]]$id_resolution[[field]] <- c(
        list(resolution="generated_special_contract_id"), special
      )
    } else if (field %in% c("target_detection_root_id")) {
      tables[[table_name]]$id_resolution[[field]] <- list(
        resolution="external_audit_identifier", pattern="^dr_[0-9a-f]{64}$"
      )
    } else if (field %in% c("compiler_id", "BLAS_id", "LAPACK_id")) {
      tables[[table_name]]$id_resolution[[field]] <- list(
        resolution="signed_toolchain_identifier", pattern="^[A-Za-z0-9._:+-]+$"
      )
    } else {
      tables[[table_name]]$id_resolution[[field]] <- list(
        resolution="declared_non_scientific_row_key", contract_clause="table_primary_or_external_manifest"
      )
    }
  }
}

# Row/cross-table conditions are part of the executable schema, not prose in a
# validator.  The validator implements these named rules and the schema lint
# rejects an unknown or missing rule identifier.
for (table_name in names(tables)) tables[[table_name]]$row_constraints <- list()
for (table_name in names(tables)) {
  fields <- vapply(tables[[table_name]]$columns, `[[`, character(1), "name")
  if (all(c("detection_root_id","source_context_hash") %in% fields)) {
    tables[[table_name]]$row_constraints <- c(
      tables[[table_name]]$row_constraints,
      list("detection_root_source_context_v1")
    )
  }
}
tables$partition_memberships$id_resolution$partition_id <- list(
  resolution="generated_group",domain="partition",prefix="pt_")
tables$lineage_records$id_resolution$lineage_id <- list(
  resolution="generated_group",domain="lineage",prefix="ln_")
tables$state_candidates$row_constraints <- c(tables$state_candidates$row_constraints,list(
  "state_candidate_decision_domain_v1", "geometry_count_duration_closure_v1"))
tables$event_candidates$row_constraints <- c(tables$event_candidates$row_constraints,list(
  "event_candidate_decision_domain_v1", "event_candidate_class_v1",
  "geometry_count_duration_closure_v1"))
tables$state_episodes$row_constraints <- c(tables$state_episodes$row_constraints,
  list("state_axis_tuple_v1"))
tables$state_segments$row_constraints <- c(tables$state_segments$row_constraints,list(
  "segment_role_conditional_fk_v1", "episode_partition_v1"))
tables$per_isi$row_constraints <- c(tables$per_isi$row_constraints,list("per_isi_role_closure_v1"))
tables$events$row_constraints <- c(tables$events$row_constraints,list(
  "event_modifier_evidence_cardinality_v1", "geometry_count_duration_closure_v1"))
tables$event_modifier_evidence$row_constraints <- c(
  tables$event_modifier_evidence$row_constraints,
  list("event_modifier_domain_value_v1"))
tables$gaps$row_constraints <- c(tables$gaps$row_constraints,list(
  "canonical_pause_hard_boundary_v1", "geometry_count_duration_closure_v1"))
tables$threshold_policies$row_constraints <- c(tables$threshold_policies$row_constraints,list(
  "threshold_permission_matrix_v1"))
tables$threshold_instances$row_constraints <- c(tables$threshold_instances$row_constraints,list(
  "threshold_value_xor_v1", "threshold_scope_key_v1",
  "threshold_policy_instance_match_v1"))
tables$product_status_envelope$row_constraints <- c(tables$product_status_envelope$row_constraints,list(
  "product_status_matrix_v1", "failure_reason_presence_v1",
  "attestation_presence_v1"))
tables$reference_annotations$row_constraints <- c(tables$reference_annotations$row_constraints,list(
  "reference_blinding_v1"))
tables$event_state_relationships$row_constraints <- c(
  tables$event_state_relationships$row_constraints,
  list("event_state_expected_set_closure_v1"))
tables$state_episode_links$row_constraints <- c(
  tables$state_episode_links$row_constraints,
  list("episode_link_expected_set_closure_v1","episode_link_truth_v1"))
tables$label_blind_contract_manifest$row_constraints <- c(
  tables$label_blind_contract_manifest$row_constraints,
  list("label_blind_manifest_action_v1"))

# Contract-required composite FK.  The single-column state_episode_id FK is
# removed so that train/class cannot be coordinated to a different episode.
tables$state_segments$foreign_keys <- Filter(function(fk) {
  !identical(unlist(fk$columns, use.names=FALSE), "state_episode_id")
}, tables$state_segments$foreign_keys)
tables$state_segments$foreign_keys[[length(tables$state_segments$foreign_keys)+1L]] <- list(
  columns=as.list(c("state_episode_id","train","state_class")),
  target_table="state_episodes",
  target_columns=as.list(c("state_episode_id","train","state_class")),
  expected_registry_domain="none"
)

section_lines <- function(start_heading, end_heading) {
  start_idx <- which(lines == start_heading)
  end_idx <- which(lines == end_heading)
  if (length(start_idx) != 1L || length(end_idx) != 1L ||
      end_idx <= start_idx + 1L) {
    stop(
      "Expected exactly one ordered section boundary: ",
      start_heading, " -> ", end_heading
    )
  }
  lines[(start_idx + 1L):(end_idx - 1L)]
}

inventory_lines <- section_lines(
  "### 8.4 Canonical product inventory",
  "### 8.5 三种完整性保证"
)
inventory <- list()
for (line in inventory_lines) {
  m <- regexec("^\\| `([^`]+)` \\| ([^|]+) \\| ([^|]+) \\| ([^|]+) \\|$", line)
  p <- regmatches(line, m)[[1L]]
  if (length(p) == 5L) inventory[[p[[2L]]]] <- list(
    auto = trimws(p[[3L]]), final = trimws(p[[4L]]),
    zero_rows = trimws(p[[5L]])
  )
}

contract_lines <- section_lines(
  "## 16. 稳定合同 ID 与最低证据",
  "## 17. 冻结条件"
)
contract_ids <- sub("^\\| `([^`]+)`.*$", "\\1", contract_lines)
contract_ids <- contract_ids[grepl("^GB3-", contract_ids)]

schema_constants <- list(
  schema_version = "stpd_multitrack_v3_1",
  status_schema = "stpd_multitrack_v3_status_1",
  identity_schema = "stpd_multitrack_v3_identity_1",
  auto_product_kind = "auto",
  final_product_kind = "final",
  auto_schema = "stpd_multitrack_auto_v3",
  final_schema = "stpd_multitrack_final_v3",
  canonical_table_schema = "stpd_canonical_table_v1",
  manifest_schema = "stpd_multitrack_v3_manifest_1",
  artifact_manifest_schema = "stpd_multitrack_v3_artifact_manifest_1",
  response_schema = "stpd_multitrack_v3_response_1",
  evidence_record_schema = "stpd_gate_b_v3_evidence_record_1",
  reference_schema = "stpd_multitrack_v3_reference_1",
  migration_schema = "stpd_multitrack_v3_migration_1",
  approval_schema = "stpd_gate_b_v3_approval_1",
  signature_schema = "stpd_gate_b_v3_signature_1",
  test_bundle_schema = "stpd_gate_b_v3_test_bundle_1",
  release_attestation_schema = "stpd_gate_b_v3_release_attestation_1"
)

id_payload_registry <- list(
  state_candidate=list(prefix="sc_", fields=c("detection_root_id","train","start_isi","end_isi","candidate_generation_rule_registry_id","source_candidate_key_hash")),
  state_axis_evidence=list(prefix="sx_", fields=c("detection_root_id","state_candidate_id","start_isi","end_isi","evidence_payload_hash")),
  state_episode=list(prefix="se_", fields=c("detection_root_id","train","state_class","start_isi","end_isi","classification_evidence_id")),
  state_segment=list(prefix="ss_", fields=c("detection_root_id","state_episode_id","segment_index","segment_role","start_isi","end_isi","source_state_candidate_id","state_axis_evidence_id","connector_decision_id")),
  event_candidate=list(prefix="ec_", fields=c("detection_root_id","train","start_isi","end_isi","candidate_generation_rule_registry_id","source_candidate_key_hash")),
  event=list(prefix="ev_", fields=c("detection_root_id","train","event_class","start_isi","end_isi","base_event_evidence_id")),
  event_modifier_evidence=list(prefix="me_", fields=c("detection_root_id","event_id","modifier_domain","evidence_id")),
  gap_candidate=list(prefix="gc_", fields=c("detection_root_id","train","start_isi","end_isi","gap_policy_registry_id")),
  gap=list(prefix="gp_", fields=c("detection_root_id","gap_candidate_id")),
  boundary=list(prefix="bd_", fields=c("detection_root_id","train","boundary_domain","boundary_geometry","edge_side","start_isi","end_isi","reason_registry_id","evidence_id")),
  state_abstention=list(prefix="sa_", fields=c("detection_root_id","state_candidate_id","start_isi","end_isi","state_axis_evidence_id")),
  gap_abstention=list(prefix="ga_", fields=c("detection_root_id","gap_candidate_id","evidence_id")),
  connector_decision=list(prefix="cd_", fields=c("detection_root_id","pre_direct_candidate_id","gap_start_isi","gap_end_isi","post_direct_candidate_id","evidence_id")),
  review_candidate=list(prefix="rv_", fields=c("detection_root_id","candidate_domain","suggested_class_registry_id","train","start_isi","end_isi","evidence_id")),
  event_state_relationship=list(prefix="er_", fields=c("detection_root_id","event_id","state_episode_id")),
  interrupted_state_relationship=list(prefix="ir_", fields=c("detection_root_id","event_id","parent_state_candidate_id","lineage_closure_hash")),
  episode_link=list(prefix="lk_", fields=c("detection_root_id","pre_state_episode_id","gap_id","post_state_episode_id","link_policy_registry_id","link_evidence_id")),
  scientific_context_epoch=list(prefix="ce_", fields=c("detection_root_id","train","epoch_domain","start_isi","end_isi","source_evidence_id")),
  evidence=list(prefix="ed_", fields=c("detection_root_id","evidence_type_registry_id","evidence_schema_registry_id","source_bytes_sha256","evidence_json_hash")),
  lineage=list(prefix="ln_", fields=c("detection_root_id","child_domain_id","child_id","parent_edge_set_hash")),
  entity_domain=list(prefix="dm_", fields=c("entity_domain","target_table","primary_key_columns_json","train_column","product_scope")),
  partition=list(prefix="pt_", fields=c("partition_version","sorted_membership_rows_without_partition_id")),
  multiple_testing_family=list(prefix="mf_", fields=c("detection_root_id","train","gap_policy_registry_id","scientific_context_epoch_id","sorted_gap_candidate_ids")),
  threshold_policy=list(prefix="tp_", fields=c("detection_root_id","path","threshold_source_class","adaptation_unit","label_access","policy_parameter_hash","partition_id")),
  threshold_instance=list(prefix="ti_", fields=c("detection_root_id","threshold_policy_id","path","application_scope","application_scope_key_hash","effective_value_hash")),
  threshold_binding=list(prefix="tb_", fields=c("detection_root_id","consumer_domain_id","consumer_id","path","threshold_instance_id","binding_role_registry_id")),
  diagnostic=list(prefix="dg_", fields=c("detection_root_id","diagnostic_code_registry_id","entity_domain_id","entity_id","train","start_isi","end_isi","detail_schema_registry_id","detail_json_hash")),
  migration=list(prefix="mg_", fields=c("source_product_hash","target_detection_root_id","mapping_status","source_entity_domain_id","source_entity_id","target_entity_domain_id","target_entity_id")),
  transition=list(prefix="tx_", fields=c("detection_root_id","parent_auto_product_hash","sequence_no","previous_transition_hash","action_type","target_domain_id","target_id","payload_schema_registry_id","payload_json_hash"))
)

status_matrix <- list(
  list(product_kind="any", product_status="not_present", deployment_context="any", execution="na", root="na", product_hash="na", gate_b="pending", authority="none_candidate", authoritative=FALSE, integrity="none", release_attestation="na", product_attestation="na", canonical_tables="forbidden"),
  list(product_kind="any", product_status="materializing", deployment_context="any", execution="required", root="optional", product_hash="optional", gate_b="pending", authority="none_candidate", authoritative=FALSE, integrity="none", release_attestation="na", product_attestation="na", canonical_tables="staging_only"),
  list(product_kind="any", product_status="candidate_pending", deployment_context="any", execution="required", root="required", product_hash="required", gate_b="pending", authority="none_candidate", authoritative=FALSE, integrity="detached_or_parent_bound", release_attestation="na", product_attestation="na", canonical_tables="required_inventory"),
  list(product_kind="auto", product_status="gate_b_authoritative", deployment_context="live", execution="required", root="required", product_hash="required", gate_b="attested", authority="automatic_prediction_record", authoritative=TRUE, integrity="parent_bound_recomputed", release_attestation="required", product_attestation="na", canonical_tables="required_inventory"),
  list(product_kind="final", product_status="gate_b_authoritative", deployment_context="live", execution="required", root="required", product_hash="required", gate_b="attested", authority="reviewed_prediction_record", authoritative=TRUE, integrity="parent_bound_recomputed", release_attestation="required", product_attestation="na", canonical_tables="required_inventory"),
  list(product_kind="auto", product_status="gate_b_authoritative", deployment_context="export", execution="required", root="required", product_hash="required", gate_b="attested", authority="automatic_prediction_record", authoritative=TRUE, integrity="externally_anchored_product", release_attestation="required", product_attestation="required", canonical_tables="required_inventory"),
  list(product_kind="final", product_status="gate_b_authoritative", deployment_context="export", execution="required", root="required", product_hash="required", gate_b="attested", authority="reviewed_prediction_record", authoritative=TRUE, integrity="externally_anchored_product", release_attestation="required", product_attestation="required", canonical_tables="required_inventory"),
  list(product_kind="any", product_status="failed_closed", deployment_context="any", execution="optional", root="optional", product_hash="na", gate_b="failed", authority="none_candidate", authoritative=FALSE, integrity="none", release_attestation="na", product_attestation="na", canonical_tables="forbidden"),
  list(product_kind="any", product_status="requires_redetection", deployment_context="any", execution="optional", root="na", product_hash="na", gate_b="pending", authority="none_candidate", authoritative=FALSE, integrity="none", release_attestation="na", product_attestation="na", canonical_tables="forbidden")
)
status_matrix <- lapply(status_matrix, function(row) c(row, list(
  gate_c = "pending",
  detector_performance_eligible = FALSE,
  biological_ground_truth = FALSE
)))

hash_dag <- list(
  dag_schema="stpd_gate_b_v3_hash_dag_1",
  nodes=c(
    "normalized_input_rows","normalized_input_manifest_hash",
    "requested_parameter_rows","requested_params_hash",
    "partition_membership_rows","partition_membership_manifest_hash",
    "threshold_policy_source_rows","threshold_policy_source_manifest_hash",
    "threshold_instance_source_rows","threshold_instance_source_manifest_hash",
    "qc_input_rows","qc_input_manifest_hash","registry_semantic_rows",
    "registry_manifest_hash","source_manifest_hash","native_manifest_hash",
    "toolchain_manifest_hash","detector_policy_hash","label_blind_contract_hash",
    "package_version","code_identity_hash","source_context_hash","detection_root_id",
    "product_kind","parent_auto_product_hash","contract_sha256",
    "schema_contract_sha256",
    "candidate_evidence_rows","candidate_universe_hash","entity_ids",
    "evidence_records","lineage_records","transition_rows","history_head_hash",
    "canonical_tables","canonical_table_envelopes","canonical_table_hashes",
    "canonical_manifest_core_hash","product_context_hash",
    "product_hash","metadata_envelope","status_core_hash",
    "fixture_expected_result_hashes","fixture_manifest_sha256",
    "test_manifest_sha256","test_result_bundle_hash",
    "gate_b_evidence_table_sha256","required_contract_id_set_sha256",
    "environment_manifest_sha256","all_required_contracts_passed",
    "approval_manifest_hash","artifact_manifest_hash",
    "release_attestation_schema","release_version","R_version","toolchain_id",
    "package_build_manifest_sha256","installed_manifest_sha256",
    "release_attestation_core_hash","embedded_release_signature",
    "release_attestation_hash","release_sequence","product_anchor_hash",
    "product_attestation_hash","tarball_sha256",
    "distribution_payload_sha256","distribution_signature"
  ),
  edges=list(
    c("normalized_input_rows","normalized_input_manifest_hash"),
    c("requested_parameter_rows","requested_params_hash"),
    c("partition_membership_rows","partition_membership_manifest_hash"),
    c("threshold_policy_source_rows","threshold_policy_source_manifest_hash"),
    c("threshold_instance_source_rows","threshold_instance_source_manifest_hash"),
    c("qc_input_rows","qc_input_manifest_hash"),
    c("registry_semantic_rows","registry_manifest_hash"),
    c("source_manifest_hash","code_identity_hash"),
    c("native_manifest_hash","code_identity_hash"),
    c("toolchain_manifest_hash","code_identity_hash"),
    c("package_version","code_identity_hash"),
    c("normalized_input_manifest_hash","source_context_hash"),
    c("requested_params_hash","source_context_hash"),
    c("partition_membership_manifest_hash","source_context_hash"),
    c("threshold_policy_source_manifest_hash","source_context_hash"),
    c("threshold_instance_source_manifest_hash","source_context_hash"),
    c("qc_input_manifest_hash","source_context_hash"),
    c("code_identity_hash","source_context_hash"),
    c("detector_policy_hash","source_context_hash"),
    c("label_blind_contract_hash","source_context_hash"),
    c("source_context_hash","detection_root_id"),
    c("detection_root_id","candidate_evidence_rows"),
    c("candidate_evidence_rows","candidate_universe_hash"),
    c("candidate_universe_hash","evidence_records"),
    c("evidence_records","entity_ids"),
    c("entity_ids","lineage_records"),
    c("entity_ids","canonical_tables"),
    c("transition_rows","history_head_hash"),
    c("history_head_hash","canonical_tables"),
    c("evidence_records","canonical_tables"),
    c("lineage_records","canonical_tables"),
    c("canonical_tables","canonical_table_envelopes"),
    c("canonical_table_envelopes","canonical_table_hashes"),
    c("canonical_table_hashes","canonical_manifest_core_hash"),
    c("product_kind","product_context_hash"),
    c("detection_root_id","product_context_hash"),
    c("parent_auto_product_hash","product_context_hash"),
    c("history_head_hash","product_context_hash"),
    c("canonical_manifest_core_hash","product_context_hash"),
    c("contract_sha256","product_context_hash"),
    c("schema_contract_sha256","product_context_hash"),
    c("product_context_hash","product_hash"),
    c("product_kind","product_hash"),
    c("detection_root_id","product_hash"),
    c("source_context_hash","product_hash"),
    c("parent_auto_product_hash","product_hash"),
    c("history_head_hash","product_hash"),
    c("canonical_manifest_core_hash","product_hash"),
    c("contract_sha256","product_hash"),
    c("schema_contract_sha256","product_hash"),
    c("product_hash","metadata_envelope"),
    c("metadata_envelope","status_core_hash"),
    c("product_hash","status_core_hash"),
    c("fixture_expected_result_hashes","fixture_manifest_sha256"),
    c("fixture_manifest_sha256","gate_b_evidence_table_sha256"),
    c("test_result_bundle_hash","gate_b_evidence_table_sha256"),
    c("source_manifest_hash","gate_b_evidence_table_sha256"),
    c("test_manifest_sha256","gate_b_evidence_table_sha256"),
    c("contract_sha256","gate_b_evidence_table_sha256"),
    c("schema_contract_sha256","gate_b_evidence_table_sha256"),
    c("environment_manifest_sha256","gate_b_evidence_table_sha256"),
    c("gate_b_evidence_table_sha256","all_required_contracts_passed"),
    c("test_result_bundle_hash","all_required_contracts_passed"),
    c("required_contract_id_set_sha256","all_required_contracts_passed"),
    c("source_manifest_hash","package_build_manifest_sha256"),
    c("native_manifest_hash","package_build_manifest_sha256"),
    c("toolchain_manifest_hash","package_build_manifest_sha256"),
    c("package_version","package_build_manifest_sha256"),
    c("package_build_manifest_sha256","installed_manifest_sha256"),
    c("product_hash","artifact_manifest_hash"),
    c("release_attestation_schema","release_attestation_core_hash"),
    c("release_version","release_attestation_core_hash"),
    c("source_manifest_hash","release_attestation_core_hash"),
    c("native_manifest_hash","release_attestation_core_hash"),
    c("toolchain_manifest_hash","release_attestation_core_hash"),
    c("contract_sha256","release_attestation_core_hash"),
    c("schema_contract_sha256","release_attestation_core_hash"),
    c("fixture_manifest_sha256","release_attestation_core_hash"),
    c("test_manifest_sha256","release_attestation_core_hash"),
    c("test_result_bundle_hash","release_attestation_core_hash"),
    c("gate_b_evidence_table_sha256","release_attestation_core_hash"),
    c("required_contract_id_set_sha256","release_attestation_core_hash"),
    c("environment_manifest_sha256","release_attestation_core_hash"),
    c("R_version","release_attestation_core_hash"),
    c("toolchain_id","release_attestation_core_hash"),
    c("package_build_manifest_sha256","release_attestation_core_hash"),
    c("installed_manifest_sha256","release_attestation_core_hash"),
    c("approval_manifest_hash","release_attestation_core_hash"),
    c("all_required_contracts_passed","release_attestation_core_hash"),
    c("release_attestation_core_hash","embedded_release_signature"),
    c("release_attestation_core_hash","release_attestation_hash"),
    c("embedded_release_signature","release_attestation_hash"),
    c("product_hash","product_anchor_hash"),
    c("detection_root_id","product_anchor_hash"),
    c("product_kind","product_anchor_hash"),
    c("release_attestation_hash","product_anchor_hash"),
    c("normalized_input_manifest_hash","product_anchor_hash"),
    c("artifact_manifest_hash","product_anchor_hash"),
    c("status_core_hash","product_anchor_hash"),
    c("release_sequence","product_anchor_hash"),
    c("product_anchor_hash","product_attestation_hash"),
    c("package_build_manifest_sha256","tarball_sha256"),
    c("installed_manifest_sha256","tarball_sha256"),
    c("release_attestation_hash","tarball_sha256"),
    c("tarball_sha256","distribution_payload_sha256"),
    c("release_attestation_hash","distribution_payload_sha256"),
    c("release_version","distribution_payload_sha256"),
    c("distribution_payload_sha256","distribution_signature")
  ),
  forbidden_back_edges=list(
    c("product_hash","canonical_tables"),c("release_attestation_hash","product_hash"),
    c("artifact_manifest_hash","product_hash"),c("metadata_envelope","product_hash"),
    c("embedded_release_signature","release_attestation_core_hash"),
    c("tarball_sha256","release_attestation_hash"),
    c("distribution_signature","distribution_payload_sha256")
  )
)
hash_dag$contract_sha256 <- digest::digest(
  jsonlite::toJSON(hash_dag,auto_unbox=TRUE,null="null",digits=NA,
                   pretty=FALSE),
  algo="sha256",serialize=FALSE
)

bundle <- list(
  bundle_schema = "stpd_gate_b_v3_phase1_schema_bundle_1",
  plan_sha256 = sha256_file(plan_path),
  normative_contract_sha256 = sha256_file(contract_path),
  frozen_sources = list(
    plan_utf8 = read_exact_utf8(plan_path),
    normative_contract_utf8 = read_exact_utf8(contract_path)
  ),
  generated_from_frozen_contract = TRUE,
  schema_constants = schema_constants,
  enums = enums,
  tables = tables,
  id_payload_registry = id_payload_registry,
  status_matrix = status_matrix,
  hash_dag = hash_dag,
  product_inventory = inventory,
  required_contract_ids = as.list(contract_ids)
)

dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(bundle, out_path, auto_unbox = TRUE, pretty = TRUE,
                     null = "null", digits = NA)
cat(out_path, "\n")
