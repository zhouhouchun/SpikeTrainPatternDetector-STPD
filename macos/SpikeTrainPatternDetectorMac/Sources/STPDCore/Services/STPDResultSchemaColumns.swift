import Foundation

/// The single ordered-column acceptance authority for the current result-package schema.
///
/// Table builders retain local projection lists beside their row materialization code, but every
/// constructed table, manifest definition, and read-back table must match these exact names and
/// positions or fail closed. Diagnostic variants intentionally share their public-table layout.
/// Moving the projection lists here safely requires a future typed row-projection refactor; merely
/// deleting the local lists would risk silently pairing values with the wrong columns.
enum STPDResultSchemaColumns {
    static func columns(for table: STPDResultTable) -> [String] {
        switch table {
        case .runMetadata:
            return runMetadata
        case .parametersReport:
            return parametersReport
        case .resolvedParameters:
            return resolvedParameters
        case .candidateLedger, .candidateLedgerDiagnostic:
            return candidateLedger
        case .candidateFeatures, .candidateFeaturesDiagnostic:
            return candidateFeatures
        case .finalDecisions, .finalDecisionsDiagnostic:
            return finalDecisions
        case .eventsFinal:
            return eventsFinal
        case .isiLabelsFinal:
            return isiLabelsFinal
        case .candidateDiagnosticAudit:
            return candidateDiagnosticAudit
        case .resultConsistencyCheck:
            return resultConsistencyCheck
        case .manualAnnotations:
            return manualAnnotations
        case .manualAnnotationImportApprovals:
            return manualAnnotationImportApprovals
        case .reviewStatus:
            return reviewStatus
        case .hfsBurstArbitrationAudit:
            return hfsBurstArbitrationAudit
        case .taskEvents:
            return taskEvents
        case .dataQualityQC:
            return dataQualityQC
        }
    }

    private static let runMetadata = [
        "run_id", "settings_digest", "dataset_digest", "result_schema_version",
        "detector_version", "build_identifier", "build_identifier_kind",
        "build_reproducibility_attested", "owner_name", "owner_email", "source_mode",
        "dataset_name", "dataset_source", "task_event_source_digest", "train_count",
        "spike_count", "task_event_count", "candidate_count",
        "diagnostic_candidate_count", "final_event_count", "final_isi_count",
    ]

    private static let parametersReport = [
        "run_id", "settings_digest", "parameter_key", "requested_value",
    ]

    private static let resolvedParameters = [
        "run_id", "settings_digest", "scope_type", "scope_id", "scope_name",
        "parameter_key", "requested_value", "adaptive_value", "effective_value",
        "source", "resolution_mode", "resolution_note", "histogram_value",
        "default_value",
    ]

    private static let candidateLedger = [
        "run_id", "settings_digest", "candidate_uid", "source_candidate_id",
        "train_id", "train_name", "candidate_layer", "candidate_class", "final_label",
        "gate_status", "action", "selected_for_auto", "selection_status",
        "start_isi_index", "end_isi_index", "start_spike_ordinal", "end_spike_ordinal",
        "n_isi", "n_valid_isi", "n_spikes", "anchor_family", "anchor_lock_level",
    ]

    private static let candidateFeatures = [
        "run_id", "settings_digest", "candidate_uid", "source_candidate_id",
        "duration_sec", "intra_q10_sec", "intra_q40_sec", "intra_q50_sec",
        "intra_q90_sec", "intra_q95_sec", "max_intra_isi_sec", "mean_intra_isi_sec",
        "cv", "cv2", "lv", "pre_gap_sec", "post_gap_sec", "pre_ratio_q90",
        "post_ratio_q90", "edge_contrast_min_q90", "edge_contrast_geom_q90", "score",
        "anchor_band_lower_sec", "anchor_band_upper_sec", "anchor_band_source",
        "anchor_band_semantics", "anchor_band_ordered", "anchor_contrast_min_required",
        "anchor_contrast_geom_required", "refractory_suspect_count",
        "refractory_suspect_action", "profile_seed_low_percentile",
        "profile_seed_high_percentile", "profile_seed_band_fraction",
        "profile_seed_run_count", "profile_max_seed_run_length", "profile_median_isi_sec",
        "profile_q10_isi_sec", "profile_q25_isi_sec", "profile_q90_isi_sec",
        "profile_pause_fraction", "profile_phenotype_prior", "profile_bridge_upper_sec",
        "profile_boundary_floor_sec", "profile_boundary_floor_hard",
        "profile_burst_contrast_s", "profile_possible_contrast_s", "hf_q80_sec",
        "hf_q80_max_sec", "hf_q90_max_sec", "hf_short_upper_sec", "hf_epoch_bridge_sec",
        "hf_tolerated_gap_sec", "hf_pattern_max_isi_sec", "hf_pause_break_sec",
        "hf_short_fraction", "hf_q90_short_fraction", "hf_bridge_fraction",
        "hf_large_fraction", "hf_tolerated_fraction", "hf_max_consecutive_large_isi",
        "hf_min_spikes_required", "hf_acceptance_route", "hf_burst_dominated",
        "hf_embedded_burst_count", "hf_embedded_burst_group_count",
        "hf_embedded_burst_coverage", "hf_burst_packet_like",
        "hf_burst_packet_neighbor", "suppressed_by_hf_state",
        "suppressed_original_label", "hf_suppressor_candidate_uid",
        "state_regularity_score", "state_burst_seed_fraction", "state_low_tail_fraction",
        "state_local_stability_score", "state_core_burst_run_length",
        "state_continuity_authority_frozen", "state_continuity_merge_terminal",
        "state_train_percentile_median", "state_local_percentile_median",
        "state_local_percentile_q90", "state_local_robust_z_median",
        "state_local_robust_z_abs_q80", "state_local_robust_z_q10",
        "burst_seed_run_start_isi", "burst_seed_run_end_isi",
        "burst_seed_band_lower_sec", "burst_seed_band_upper_sec",
        "burst_bridge_band_upper_sec", "burst_contrast_required",
        "burst_possible_contrast_required", "burst_required_gap_sec",
        "burst_possible_required_gap_sec", "burst_boundary_floor_sec",
        "burst_boundary_floor_hard", "burst_strict_boundary_pass",
        "burst_possible_boundary_pass", "burst_bridge_count_pass",
        "burst_bridge_fraction_pass", "burst_q90_bridge_pass",
        "burst_size_label_before_review", "threshold_mode", "hard_threshold",
        "hard_threshold_pattern", "hard_burst_seed_upper_sec",
        "hard_burst_bridge_upper_sec", "hard_burst_core_isi_count",
        "hard_threshold_source", "local_background_q75_sec",
        "local_compression_q90_ratio", "event_local_median_sec",
        "event_local_percentile_median", "event_local_percentile_q90",
        "event_local_robust_z_median", "event_local_robust_z_abs_q80",
        "event_local_robust_z_q10",
    ]

    private static let finalDecisions = [
        "run_id", "settings_digest", "candidate_uid", "source_candidate_id",
        "final_label", "gate_status", "action", "priority", "selected_for_auto",
        "selection_status", "semantic_track", "event_track_class", "audit_family",
        "audit_subtype", "audit_final_class", "audit_review_status",
        "audit_review_required", "audit_confidence_tier", "audit_uncertainty_reason",
        "audit_long_burst_definition_status", "failure_reason",
        "candidate_diagnostic_class", "state_tonic_subtype",
        "state_high_frequency_subtype", "decision_path",
    ]

    private static let eventsFinal = [
        "run_id", "settings_digest", "event_uid", "source_event_ids", "train_id",
        "train_name", "final_label", "final_subtype", "state_tonic_subtype",
        "state_high_frequency_subtypes", "semantic_track", "event_track_class",
        "lock_level", "authority_origin", "start_isi_index", "end_isi_index",
        "start_spike_ordinal", "end_spike_ordinal", "raw_start_sec", "raw_end_sec",
        "aligned_start_sec", "aligned_end_sec", "duration_sec", "score", "priority",
        "source_candidate_uids", "unresolved_source_candidate_ids",
        "automatic_support_isi_indices", "review_evidence_uids",
        "review_evidence_present", "review_changed_projection",
        "audit_recommended_subtype", "audit_review_status", "decision_path",
    ]

    private static let isiLabelsFinal = [
        "run_id", "settings_digest", "isi_uid", "train_id", "train_name", "isi_index",
        "left_spike_array_index", "right_spike_array_index", "left_spike_ordinal",
        "right_spike_ordinal", "timestamp_sec", "aligned_timestamp_sec", "isi_sec",
        "auto_pattern", "auto_subtype", "auto_source_candidate_id",
        "auto_candidate_uid", "final_pattern", "final_subtype", "final_source",
        "manual_veto_suppressed", "review_note", "review_evidence_uids",
        "review_evidence_present", "review_changed_projection", "isi_qc_class",
        "artifact_floor_status", "qc_refractory_suspect", "qc_artifact_threshold_sec",
        "qc_refractory_threshold_sec", "train_qc_warning_level",
        "train_qc_warning_message", "train_qc_duration_sec", "train_qc_firing_rate_hz",
        "train_qc_raw_min_isi_sec", "train_qc_min_valid_isi_sec",
        "train_qc_artifact_min_isi_sec", "train_qc_median_isi_sec",
        "train_qc_max_isi_sec", "train_qc_duplicate_timestamp_count",
        "train_qc_zero_or_negative_isi_count",
        "train_qc_zero_or_negative_timestamp_step_count", "train_qc_input_was_unsorted",
        "train_qc_input_nonmonotonic_step_count",
        "train_qc_input_duplicate_timestamp_step_count",
        "train_qc_input_zero_or_negative_step_count",
        "train_qc_dropped_duplicate_timestamp_count",
        "train_qc_duplicate_timestamp_policy", "train_qc_artifact_isi_count",
        "train_qc_artifact_fraction", "train_qc_refractory_suspect_isi_count",
        "train_qc_refractory_suspect_fraction", "train_qc_valid_isi_count",
        "train_qc_percentile_status",
    ]

    private static let candidateDiagnosticAudit = [
        "run_id", "settings_digest", "candidate_uid", "stage_id",
        "source_candidate_id", "stage_name", "stage_ordinal", "evidence_kind",
        "status", "details", "event_uid", "automatic_source_id",
        "source_support_isi_indices", "source_semantic_track",
        "source_event_track_class", "source_label", "source_lock_level",
        "source_state_tonic_subtype", "source_score", "source_priority",
        "source_decision_path",
    ]

    private static let resultConsistencyCheck = [
        "run_id", "settings_digest", "check_id", "status", "severity", "details",
    ]

    private static let manualAnnotations = [
        "run_id", "settings_digest", "annotation_id", "annotation_semantic_digest",
        "source_annotation_uuid", "authority_source", "import_approval_id", "train_id",
        "label", "polarity", "start_sec", "end_sec", "start_isi_index",
        "end_isi_index", "start_spike_array_index", "end_spike_array_index",
        "start_spike_ordinal", "end_spike_ordinal", "linked_isi_uids", "link_scope",
        "note", "annotator", "annotator_identity_source", "created_at", "updated_at",
        "created_at_unix_sec", "updated_at_unix_sec",
    ]

    private static let manualAnnotationImportApprovals = [
        "run_id", "settings_digest", "approval_id", "source_file_sha256",
        "approved_dataset_digest", "approved_run_id", "approved_settings_digest",
        "approver", "approver_identity_assurance", "approved_at",
        "approved_at_unix_sec", "source_schema_version", "source_run_id",
        "source_review_state", "annotation_ids",
        "annotation_source_semantic_digests", "annotation_count",
    ]

    private static let reviewStatus = [
        "run_id", "settings_digest", "candidate_uid", "review_uid",
        "source_candidate_id", "status", "reviewer", "reviewed_run_id",
        "linked_isi_uids", "link_scope", "note", "reviewed_at",
        "reviewed_at_unix_sec",
    ]

    private static let hfsBurstArbitrationAudit: [String] = {
        let base = [
            "run_id", "settings_digest", "audit_row_id", "hfs_candidate_uid",
            "hfs_root_lineage_uid", "hfs_root_reference_kind",
            "strongest_burst_candidate_uid", "strongest_long_burst_candidate_uid",
            "unresolved_candidate_ids", "package_final_event_count",
            "package_final_event_subtypes", "package_final_projection_differs_from_audit",
        ]
        return base + HFSBurstArbitrationAuditRow.csvHeader.map { "audit_\($0)" }
    }()

    private static let taskEvents = [
        "run_id", "settings_digest", "task_event_uid", "source_event_id", "event_name",
        "event_time_sec", "source_column", "source_event_index", "trial_id", "source",
    ]

    private static let dataQualityQC = [
        "run_id", "settings_digest", "dataset_digest", "train_id", "train_name",
        "spike_count", "raw_isi_count", "valid_isi_count", "artifact_isi_count",
        "artifact_fraction", "refractory_suspect_isi_count",
        "refractory_suspect_fraction", "zero_or_negative_isi_count",
        "duplicate_timestamp_count", "dropped_duplicate_timestamp_count",
        "input_was_unsorted", "input_nonmonotonic_step_count",
        "duplicate_timestamp_policy", "artifact_threshold_sec",
        "refractory_suspect_threshold_sec", "firing_rate_hz", "duration_sec",
        "raw_min_isi_sec", "min_valid_isi_sec", "artifact_min_isi_sec",
        "median_isi_sec", "max_isi_sec", "warning_level", "warning_message",
        "percentile_status",
    ]
}
