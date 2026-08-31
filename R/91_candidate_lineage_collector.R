# Threshold-first Gate 1B run-local collector -----------------------------
#
# This module provides run-local lifecycle, plumbing, and immutable direct
# observation buffers. Observations remain internal sidecars: they are not
# scientific candidate nodes, lineage edges, stage decisions, q values, or
# authoritative products until a later gate assembles and validates them.

STPD_CANDIDATE_LINEAGE_COLLECTOR_VERSION <-
  "stpd_candidate_lineage_collector_v1"
STPD_CANDIDATE_LINEAGE_REQUEST_RECEIPT_VERSION <-
  "stpd_candidate_lineage_request_receipt_v1"
STPD_CANDIDATE_LINEAGE_COLLECTOR_STATES <- c("open", "sealed", "aborted")
STPD_CANDIDATE_LINEAGE_TRAIN_STATES <-
  c("not_started", "open", "completed", "aborted", "disabled")
STPD_CANDIDATE_LINEAGE_OBSERVATION_VERSION <-
  "stpd_candidate_lineage_observation_v1"
STPD_CANDIDATE_LINEAGE_COLLECTOR_INSTRUMENTATION_STATUSES <- c(
  "pipeline_not_instrumented", "direct_partial", "direct_complete"
)

stpd_candidate_lineage_observation_hook_registry <- function() {
  data.frame(
    hook_id = c(
      "hf_protected_impl_entry_v1",
      "pause_raw_entry_v1",
      "pause_raw_support_v1",
      "pause_raw_runs_v1",
      "pause_raw_output_v1",
      "pause_boundary_projection_sources_v1",
      "pause_boundary_projection_output_v1",
      "pause_raw_receipt_v1",
      "burst_dispatch_final_route_v1",
      "burst_structure_first_pre_cap_v1",
      "burst_structure_first_post_cap_v1",
      "burst_structure_first_scan_receipt_v1",
      "burst_detector_final_entry_v1",
      "burst_raw_threshold_support_v1",
      "burst_seed_runs_v1",
      "burst_threshold_stage4_entry_v1",
      "burst_threshold_stage4_episode_roots_v1",
      "burst_threshold_stage4_attempts_v1",
      "burst_threshold_stage4_candidates_v1",
      "burst_threshold_stage4_receipt_v1",
      "burst_union_pre_cap_v1",
      "burst_union_post_cap_v1",
      "burst_union_rank_receipt_v1",
      "burst_refractory_entry_v1",
      "burst_refractory_attempts_v1",
      "burst_refractory_output_v1",
      "burst_refractory_receipt_v1",
      "burst_pause_ownership_entry_v1",
      "burst_pause_ownership_eligibility_v1",
      "burst_pause_ownership_selection_v1",
      "burst_pause_ownership_attempts_v1",
      "burst_pause_ownership_output_v1",
      "burst_pause_ownership_receipt_v1",
      "contextual_pause_entry_v1",
      "contextual_pause_generic_input_v1",
      "contextual_pause_ownership_support_v1",
      "contextual_pause_generic_downgrade_attempts_v1",
      "contextual_pause_generic_post_downgrade_v1",
      "contextual_pause_interburst_pair_attempts_v1",
      "contextual_pause_interburst_candidates_v1",
      "contextual_pause_combined_output_v1",
      "contextual_pause_receipt_v1",
      "burst_pause_boundary_entry_v1",
      "burst_pause_boundary_sources_v1",
      "burst_pause_boundary_attempts_v1",
      "burst_pause_boundary_output_v1",
      "burst_pause_boundary_receipt_v1",
      "burst_hard_threshold_entry_v1",
      "burst_hard_threshold_support_v1",
      "burst_hard_threshold_runs_v1",
      "burst_hard_threshold_pre_refractory_candidates_v1",
      "burst_hard_threshold_refractory_attempts_v1",
      "burst_hard_threshold_pre_boundary_output_v1",
      "burst_hard_threshold_boundary_sources_v1",
      "burst_hard_threshold_boundary_attempts_v1",
      "burst_hard_threshold_post_boundary_output_v1",
      "burst_hard_threshold_receipt_v1",
      "gap_final_entry_v1",
      "gap_final_candidate_input_v1",
      "gap_final_arbitration_v1",
      "gap_final_materialized_output_v1",
      "gap_final_receipt_v1",
      "hfs_state_entry_v1",
      "hfs_state_candidates_v1",
      "hfs_state_support_roles_v1",
      "hfs_state_parent_selection_v1",
      "hfs_state_receipt_v1",
      "tonic_state_entry_v1",
      "tonic_state_candidates_v1",
      "tonic_state_frequency_regularity_v1",
      "tonic_state_fragment_redetection_v1",
      "tonic_state_receipt_v1",
      "nested_hfs_review_entry_v1",
      "nested_hfs_review_candidates_v1",
      "nested_hfs_review_local_contrast_v1",
      "nested_hfs_review_parent_invariance_v1",
      "nested_hfs_review_receipt_v1",
      "candidate_universe_release_manifest_v1",
      "candidate_universe_release_products_v1",
      "candidate_universe_release_receipt_v1"
    ),
    pipeline_id = rep("hf_protected", 80L),
    stringsAsFactors = FALSE, check.names = FALSE
  )
}

stpd_candidate_lineage_observation_hook_schema <- function(hook_id) {
  switch(
    hook_id,
    hf_protected_impl_entry_v1 = c(
      train = "character", n_interval_rows = "integer",
      min_isi_sec = "double"
    ),
    pause_raw_entry_v1 = c(
      train = "character", root_id = "character",
      applicability_status = "character",
      pause_pattern_requested = "logical", generator_invoked = "logical",
      n_interval_rows = "integer", n_physical_isi = "integer",
      min_isi_sec = "double", pause_threshold_sec = "double",
      generation_gated_by_requested_patterns = "logical",
      manual_label_gate_status = "character",
      resolved_parameter_provenance_status = "character",
      final_contextual_pause_status = "character",
      input_data_sha256 = "character", params_payload_sha256 = "character",
      vp_payload_sha256 = "character"
    ),
    pause_raw_support_v1 = c(
      train = "character", detector_row_index = "integer",
      isi_index = "integer", isi_sec = "double",
      artifact_isi = "logical", valid_isi = "logical",
      base_long_member = "logical", local_median_sec = "double",
      global_median_sec = "double", local_contrast_gate = "character",
      global_contrast_gate = "character", final_pause_flag = "logical",
      pause_base_threshold_sec = "double",
      pause_tonic_guard_threshold_sec = "double",
      pause_effective_threshold_sec = "double",
      pause_relative_local_factor = "double",
      pause_relative_global_factor = "double",
      relative_gate_applied = "logical"
    ),
    pause_raw_runs_v1 = c(
      train = "character", run_ordinal = "integer",
      detector_start_row = "integer", detector_end_row = "integer",
      start_isi = "integer", end_isi = "integer", run_isi_n = "integer",
      score = "double", candidate_counter = "integer",
      candidate_id = "character", construction_attempted = "logical",
      candidate_emitted = "logical", terminal_status = "character",
      terminal_reason = "character", output_candidate_ordinal = "integer",
      candidate_payload_sha256 = "character"
    ),
    pause_raw_output_v1 = c(
      train = "character", output_ordinal = "integer",
      candidate_id = "character", candidate_payload_sha256 = "character",
      detector_start_row = "integer", detector_end_row = "integer",
      start_isi = "integer", end_isi = "integer",
      final_label = "character", action = "character",
      gap_semantics = "character", hard_for_event = "logical",
      score = "double", intended_semantic_track = "character",
      scientific_role = "character", selection_status = "character",
      final_contextual_pause_status = "character"
    ),
    pause_boundary_projection_sources_v1 = c(
      train = "character", source_ordinal = "integer",
      source_candidate_id = "character", source_payload_sha256 = "character",
      detector_start_row = "integer", detector_end_row = "integer",
      gap_semantics = "character", hard_for_event = "logical",
      action = "character", projected = "logical",
      projection_reason = "character", boundary_ordinal = "integer"
    ),
    pause_boundary_projection_output_v1 = c(
      train = "character", boundary_ordinal = "integer",
      detector_start_row = "integer", detector_end_row = "integer",
      start_isi = "integer", end_isi = "integer",
      boundary_kind = "character", source_candidate_n = "integer",
      source_ordinals = "character", source_candidate_ids = "character",
      source_payload_sha256 = "character", semantic_role = "character",
      candidate_status = "character", selectable = "logical",
      materialization_status = "character"
    ),
    pause_raw_receipt_v1 = c(
      train = "character", root_id = "character",
      applicability_status = "character",
      pause_pattern_requested = "logical", generator_invoked = "logical",
      support_row_n = "integer", flagged_isi_n = "integer",
      run_n = "integer", construction_failed_run_n = "integer",
      raw_output_n = "integer", projected_source_n = "integer",
      boundary_output_n = "integer", boundary_sort_applied = "logical",
      boundary_deduplication_applied = "logical",
      scan_exhausted = "logical", cap_applied = "logical",
      raw_candidate_semantic_track = "character",
      boundary_semantic_role = "character",
      context_adaptive_pause_status = "character",
      gap_selection_status = "character",
      recurrent_pause_evaluation_status = "character",
      publication_authority = "logical",
      upstream_universe_status = "character", output_role = "character",
      coverage_status = "character", entry_payload_sha256 = "character",
      support_payload_sha256 = "character", run_payload_sha256 = "character",
      raw_output_payload_sha256 = "character",
      boundary_source_payload_sha256 = "character",
      boundary_output_payload_sha256 = "character",
      scientific_raw_output_sha256 = "character",
      scientific_boundary_output_sha256 = "character"
    ),
    contextual_pause_entry_v1 = c(
      train = "character", root_id = "character",
      pause_pattern_requested = "logical", applicability_status = "character",
      burst_route = "character", upstream_raw_status = "character",
      upstream_ownership_status = "character", scientific_role = "character",
      publication_authority = "logical", candidate_universe_status = "character",
      final_gap_selection_status = "character",
      recurrent_pause_state_status = "character",
      hfs_direct_support_materialization_status = "character",
      hfs_envelope_status = "character", manual_override_status = "character",
      performance_validation_status = "character",
      threshold_optimization_status = "character",
      scientific_result_influence = "character"
    ),
    contextual_pause_generic_input_v1 = c(
      train = "character", output_ordinal = "integer", route = "character",
      candidate_id = "character", detector_start_row = "integer",
      detector_end_row = "integer", final_label = "character",
      action = "character", gap_semantics = "character",
      hard_for_event = "logical", hard_for_state_direct_support = "logical",
      envelope_bridge_eligible = "logical", pause_candidate_decision = "character",
      pause_candidate_reason_code = "character",
      candidate_payload_sha256 = "character", same_geometry_route_n = "integer",
      ontology_track = "character", product_status = "character",
      selection_eligibility = "character", observation_visibility = "character",
      scientific_role = "character"
    ),
    contextual_pause_ownership_support_v1 = c(
      train = "character", support_ordinal = "integer", candidate_id = "character",
      detector_start_row = "integer", detector_end_row = "integer",
      candidate_payload_sha256 = "character", ontology_track = "character",
      selection_eligibility = "character", observation_visibility = "character",
      scientific_role = "character"
    ),
    contextual_pause_generic_downgrade_attempts_v1 = c(
      train = "character", generic_ordinal = "integer", candidate_id = "character",
      detector_start_row = "integer", detector_end_row = "integer",
      source_payload_sha256 = "character", output_payload_sha256 = "character",
      overlapping_support_n = "integer", overlapping_support_ordinals = "character",
      overlapping_support_candidate_ids = "character",
      overlapping_support_payload_sha256 = "character",
      before_action = "character", after_action = "character",
      before_decision_action = "character", after_decision_action = "character",
      before_gap_semantics = "character", after_gap_semantics = "character",
      before_hard_for_event = "logical", after_hard_for_event = "logical",
      before_hard_for_state_direct_support = "logical",
      after_hard_for_state_direct_support = "logical",
      before_envelope_bridge_eligible = "logical",
      after_envelope_bridge_eligible = "logical",
      before_pause_candidate_decision = "character",
      after_pause_candidate_decision = "character",
      before_pause_candidate_reason_code = "character",
      after_pause_candidate_reason_code = "character",
      downgraded = "logical", terminal_reason = "character",
      selection_eligibility = "character", observation_visibility = "character",
      scientific_role = "character"
    ),
    contextual_pause_generic_post_downgrade_v1 = c(
      train = "character", output_ordinal = "integer", route = "character",
      candidate_id = "character", detector_start_row = "integer",
      detector_end_row = "integer", final_label = "character",
      action = "character", gap_semantics = "character",
      hard_for_event = "logical", hard_for_state_direct_support = "logical",
      envelope_bridge_eligible = "logical", pause_candidate_decision = "character",
      pause_candidate_reason_code = "character",
      candidate_payload_sha256 = "character", same_geometry_route_n = "integer",
      ontology_track = "character", product_status = "character",
      selection_eligibility = "character", observation_visibility = "character",
      scientific_role = "character"
    ),
    contextual_pause_interburst_pair_attempts_v1 = c(
      train = "character", pair_ordinal = "integer",
      left_candidate_id = "character", right_candidate_id = "character",
      left_candidate_payload_sha256 = "character",
      right_candidate_payload_sha256 = "character",
      raw_gap_isi_n = "integer", valid_gap_isi_n = "integer",
      gap_isi_cap = "integer", candidate_isi = "integer",
      candidate_value_sec = "double", flank_median_sec = "double",
      bridge_upper_sec = "double", burst_factor = "double",
      contrast_threshold_sec = "double", minimum_isi_sec = "double",
      threshold_sec = "double", threshold_comparator = "character",
      candidate_selection_rule = "character",
      strict_threshold_pass = "logical",
      terminal_reason = "character", emitted_candidate_id = "character",
      emitted_output_payload_sha256 = "character",
      scientific_role = "character"
    ),
    contextual_pause_interburst_candidates_v1 = c(
      train = "character", output_ordinal = "integer", route = "character",
      candidate_id = "character", detector_start_row = "integer",
      detector_end_row = "integer", final_label = "character",
      action = "character", gap_semantics = "character",
      hard_for_event = "logical", hard_for_state_direct_support = "logical",
      envelope_bridge_eligible = "logical", pause_candidate_decision = "character",
      pause_candidate_reason_code = "character",
      candidate_payload_sha256 = "character", same_geometry_route_n = "integer",
      ontology_track = "character", product_status = "character",
      selection_eligibility = "character", observation_visibility = "character",
      scientific_role = "character"
    ),
    contextual_pause_combined_output_v1 = c(
      train = "character", output_ordinal = "integer", route = "character",
      candidate_id = "character", detector_start_row = "integer",
      detector_end_row = "integer", final_label = "character",
      action = "character", gap_semantics = "character",
      hard_for_event = "logical", hard_for_state_direct_support = "logical",
      envelope_bridge_eligible = "logical", pause_candidate_decision = "character",
      pause_candidate_reason_code = "character",
      candidate_payload_sha256 = "character", same_geometry_route_n = "integer",
      ontology_track = "character", product_status = "character",
      selection_eligibility = "character", observation_visibility = "character",
      scientific_role = "character"
    ),
    contextual_pause_receipt_v1 = c(
      train = "character", root_id = "character", generic_input_n = "integer",
      ownership_support_n = "integer", generic_downgraded_n = "integer",
      interburst_pair_attempt_n = "integer", interburst_candidate_n = "integer",
      combined_output_n = "integer", route_collision_n = "integer",
      scan_exhausted = "logical", coverage_status = "character",
      audit_capture_cap_applied = "logical", deduplication_applied = "logical",
      generic_downgrade_coverage_status = "character",
      interburst_pair_coverage_status = "character",
      combined_replay_status = "character",
      candidate_universe_status = "character", final_gap_selection_status = "character",
      recurrent_pause_state_status = "character",
      hfs_direct_support_materialization_status = "character",
      hfs_envelope_status = "character", manual_override_status = "character",
      performance_validation_status = "character",
      threshold_optimization_status = "character",
      scientific_output_validation_status = "character",
      publication_authority = "logical", scientific_result_influence = "character",
      scientific_role = "character",
      upstream_raw_receipt_sha256 = "character",
      upstream_ownership_receipt_sha256 = "character",
      entry_payload_sha256 = "character", generic_input_payload_sha256 = "character",
      ownership_support_payload_sha256 = "character",
      downgrade_attempt_payload_sha256 = "character",
      generic_post_payload_sha256 = "character",
      pair_attempt_payload_sha256 = "character",
      interburst_payload_sha256 = "character",
      combined_payload_sha256 = "character", scientific_output_sha256 = "character"
    ),
    gap_final_entry_v1 = c(
      train = "character", root_id = "character",
      pause_pattern_requested = "logical", applicability_status = "character",
      audit_candidate_n = "integer", contextual_gap_proposal_n = "integer",
      exact_candidate_input_binding = "logical",
      upstream_contextual_receipt_sha256 = "character",
      candidate_input_sha256 = "character", arbitration_status = "character",
      materialization_status = "character", recurrent_pause_state_status = "character",
      scientific_result_influence = "character", publication_authority = "logical"
    ),
    gap_final_candidate_input_v1 = c(
      train = "character", input_ordinal = "integer", candidate_id = "character",
      final_label = "character", detector_start_row = "integer",
      detector_end_row = "integer", action = "character",
      score = "double", n_isi = "double", arbitration_value = "double",
      selectable = "logical", input_terminal_reason = "character",
      is_contextual_gap_proposal = "logical", contextual_output_ordinal = "integer",
      candidate_payload_sha256 = "character", contextual_payload_sha256 = "character"
    ),
    gap_final_arbitration_v1 = c(
      train = "character", input_ordinal = "integer", candidate_id = "character",
      final_label = "character", detector_start_row = "integer",
      detector_end_row = "integer", arbitration_value = "double",
      predecessor_pool_ordinal = "integer", selected_expected = "logical",
      selected_scientific = "logical", selection_status_expected = "character",
      selection_status_scientific = "character", replay_equal = "logical",
      is_gap_candidate = "logical", candidate_payload_sha256 = "character"
    ),
    gap_final_materialized_output_v1 = c(
      train = "character", materialized_ordinal = "integer",
      input_ordinal = "integer", candidate_id = "character",
      isi_index = "integer", expected_prevalidation_label = "character",
      final_pattern_auto = "character", retained_as_pause = "logical",
      materialization_status = "character", candidate_payload_sha256 = "character"
    ),
    gap_final_receipt_v1 = c(
      train = "character", root_id = "character", audit_candidate_n = "integer",
      contextual_gap_proposal_n = "integer", selectable_candidate_n = "integer",
      selected_candidate_n = "integer", selected_gap_candidate_n = "integer",
      expected_gap_support_isi_n = "integer", retained_gap_support_isi_n = "integer",
      exact_candidate_input_binding = "logical", arbitration_replay_status = "character",
      scientific_audit_identity_status = "character",
      materialized_gap_identity_status = "character",
      post_validation_removal_n = "integer", collector_off_equivalence_status = "character",
      recurrent_pause_state_status = "character", publication_authority = "logical",
      scientific_result_influence = "character",
      upstream_contextual_receipt_sha256 = "character",
      entry_payload_sha256 = "character", candidate_input_payload_sha256 = "character",
      arbitration_payload_sha256 = "character", materialized_output_payload_sha256 = "character",
      scientific_audit_sha256 = "character", scientific_pattern_auto_sha256 = "character"
    ),
    hfs_state_entry_v1 = c(
      train = "character", root_id = "character",
      broad_hfs_requested = "logical", generator_invoked = "logical",
      n_interval_rows = "integer", min_isi_sec = "double",
      hard_boundary_n = "integer", unique_parent_generator = "character",
      subtype_generation_status = "character",
      burst_to_state_veto_allowed = "logical",
      input_data_sha256 = "character", params_payload_sha256 = "character",
      vp_payload_sha256 = "character",
      hard_boundary_payload_sha256 = "character"
    ),
    hfs_state_candidates_v1 = c(
      train = "character", candidate_ordinal = "integer",
      candidate_id = "character", start_isi = "integer", end_isi = "integer",
      envelope_isi_n = "integer", direct_support_isi_n = "integer",
      tolerated_connector_isi_n = "integer",
      moderate_connector_isi_n = "integer",
      transparent_artifact_connector_isi_n = "integer",
      canonical_pause_intrusion_isi_n = "integer",
      reported_connector_isi_n = "integer",
      selected_broad_parent = "logical", state_family = "character",
      parent_state_class = "character", subtype_status = "character",
      candidate_payload_sha256 = "character"
    ),
    hfs_state_support_roles_v1 = c(
      train = "character", role_ordinal = "integer",
      candidate_ordinal = "integer", candidate_id = "character",
      isi_index = "integer", isi_sec = "double", support_role = "character",
      connector_kind = "character", canonical_pause_boundary = "logical",
      active_state_support = "logical", episode_membership = "logical"
    ),
    hfs_state_parent_selection_v1 = c(
      train = "character", candidate_ordinal = "integer",
      candidate_id = "character", selected_before_nested_review = "logical",
      selected_after_nested_review = "logical",
      parent_signature_unchanged = "logical", burst_veto_applied = "logical",
      subtype_changes_parent_geometry = "logical",
      candidate_payload_sha256 = "character"
    ),
    hfs_state_receipt_v1 = c(
      train = "character", root_id = "character", candidate_n = "integer",
      selected_parent_n = "integer", direct_support_isi_n = "integer",
      tolerated_connector_isi_n = "integer",
      canonical_pause_intrusion_isi_n = "integer",
      connector_identity_status = "character",
      support_envelope_closure_status = "character",
      parent_signature_status = "character", burst_overlay_policy = "character",
      pause_direct_support_policy = "character", subtype_policy = "character",
      scientific_result_influence = "character", publication_authority = "logical",
      entry_payload_sha256 = "character", candidate_payload_sha256 = "character",
      support_role_payload_sha256 = "character",
      parent_selection_payload_sha256 = "character",
      scientific_hfs_output_sha256 = "character"
    ),
    tonic_state_entry_v1 = c(
      train = "character", root_id = "character",
      tonic_requested = "logical", hft_requested = "logical",
      generator_invoked = "logical", n_interval_rows = "integer",
      n_valid_isi = "integer", min_isi_sec = "double",
      canonical_pause_boundary_n = "integer",
      tonic_min_spikes = "integer", hft_min_spikes = "integer",
      frequency_axis = "character", regularity_axes = "character",
      burst_to_state_veto_allowed = "logical",
      input_data_sha256 = "character", params_payload_sha256 = "character",
      vp_payload_sha256 = "character",
      hard_boundary_payload_sha256 = "character"
    ),
    tonic_state_candidates_v1 = c(
      train = "character", candidate_ordinal = "integer",
      candidate_id = "character", state_class = "character",
      generator = "character", start_isi = "integer", end_isi = "integer",
      n_spikes = "integer", burst_overlap_n = "integer",
      burst_veto_applied = "logical", candidate_payload_sha256 = "character"
    ),
    tonic_state_frequency_regularity_v1 = c(
      train = "character", candidate_ordinal = "integer",
      candidate_id = "character", state_class = "character",
      n_valid_isi = "integer", median_isi_sec = "double",
      median_frequency_hz = "double", CV = "double", LV = "double",
      MM = "double", lower_frequency_bound_sec = "double",
      upper_frequency_bound_sec = "double", regularity_evidence_status = "character",
      frequency_evidence_status = "character"
    ),
    tonic_state_fragment_redetection_v1 = c(
      train = "character", fragment_ordinal = "integer",
      parent_candidate_id = "character", state_class = "character",
      start_isi = "integer", end_isi = "integer", n_spikes = "integer",
      created_by_canonical_pause = "logical", redetection_performed = "logical",
      regenerated_candidate_n = "integer", fragment_accepted = "logical",
      terminal_status = "character", regenerated_geometry_sha256 = "character"
    ),
    tonic_state_receipt_v1 = c(
      train = "character", root_id = "character", tonic_candidate_n = "integer",
      hft_candidate_n = "integer", burst_overlapping_candidate_n = "integer",
      sparse_case_applicable = "logical", sparse_evidence_abstention_status = "character",
      burst_overlay_status = "character", fragment_redetection_status = "character",
      scientific_result_influence = "character", publication_authority = "logical",
      entry_payload_sha256 = "character", candidate_payload_sha256 = "character",
      frequency_regularity_payload_sha256 = "character",
      fragment_redetection_payload_sha256 = "character",
      scientific_tonic_output_sha256 = "character",
      scientific_hft_output_sha256 = "character"
    ),
    nested_hfs_review_entry_v1 = c(
      train = "character", root_id = "character",
      nested_review_requested = "logical", generator_invoked = "logical",
      selected_broad_hfs_parent_n = "integer", hard_boundary_n = "integer",
      min_isi_sec = "double", proposal_semantic_track = "character",
      automatic_event_promotion_allowed = "logical",
      input_data_sha256 = "character", selected_parent_payload_sha256 = "character",
      settings_payload_sha256 = "character", hard_boundary_payload_sha256 = "character"
    ),
    nested_hfs_review_candidates_v1 = c(
      train = "character", candidate_ordinal = "integer",
      candidate_id = "character", parent_hfs_candidate_id = "character",
      start_isi = "integer", end_isi = "integer", n_spikes = "integer",
      final_label = "character", action = "character",
      review_only = "logical", canonical_eligible = "logical",
      semantic_track = "character", review_promotion_required = "logical",
      candidate_payload_sha256 = "character"
    ),
    nested_hfs_review_local_contrast_v1 = c(
      train = "character", candidate_ordinal = "integer",
      candidate_id = "character", seed_pair_local_rank = "double",
      seed_left_ratio = "double", seed_right_ratio = "double",
      seed_geom_ratio = "double", final_left_ratio = "double",
      final_right_ratio = "double", final_geom_ratio = "double",
      evidence_strength = "character", absolute_pattern_threshold_used = "logical",
      local_hfs_background_only = "logical"
    ),
    nested_hfs_review_parent_invariance_v1 = c(
      train = "character", provisional_parent_signature = "character",
      final_parent_signature = "character", parent_signature_unchanged = "logical",
      review_can_veto_state = "logical", review_can_reshape_state = "logical",
      review_can_create_state = "logical"
    ),
    nested_hfs_review_receipt_v1 = c(
      train = "character", root_id = "character", raw_candidate_n = "integer",
      standardized_candidate_n = "integer", review_track_candidate_n = "integer",
      review_identity_status = "character", local_contrast_status = "character",
      parent_invariance_status = "character", promotion_authority_status = "character",
      scientific_result_influence = "character", publication_authority = "logical",
      entry_payload_sha256 = "character", candidate_payload_sha256 = "character",
      local_contrast_payload_sha256 = "character",
      parent_invariance_payload_sha256 = "character",
      scientific_raw_output_sha256 = "character",
      scientific_standardized_output_sha256 = "character"
    ),
    candidate_universe_release_manifest_v1 = c(
      train = "character", root_id = "character",
      expected_prior_hook_n = "integer", observed_prior_hook_n = "integer",
      release_hook_n = "integer", max_observation_hook_n = "integer",
      prior_observation_record_n = "integer", root_receipt_n = "integer",
      prior_hook_order_status = "character", root_closure_status = "character",
      incomplete_root_ids = "character",
      cap_or_early_stop_root_ids = "character",
      prior_hook_ids_sha256 = "character", prior_index_sha256 = "character",
      root_audit_sha256 = "character",
      all_family_roots_closed = "logical"
    ),
    candidate_universe_release_products_v1 = c(
      train = "character", audit_candidate_n = "integer",
      pattern_auto_labeled_isi_n = "integer",
      auto_score_nonmissing_n = "integer", shadow_candidate_n = "integer",
      nested_review_candidate_n = "integer", tonic_review_candidate_n = "integer",
      audit_sha256 = "character", pattern_auto_sha256 = "character",
      auto_score_sha256 = "character",
      shadow_candidates_sha256 = "character", nested_review_sha256 = "character",
      tonic_review_sha256 = "character",
      multitrack_shadow_sha256 = "character",
      multitrack_compatibility_shadow_sha256 = "character",
      event_grammar_params_sha256 = "character",
      final_train_product_sha256 = "character",
      final_hfs_parent_signature = "character",
      binding_status = "character"
    ),
    candidate_universe_release_receipt_v1 = c(
      train = "character", root_id = "character",
      observation_universe_status = "character",
      all_family_roots_status = "character",
      final_products_binding_status = "character",
      resource_budget_status = "character",
      canonical_adapter_status = "character",
      max_observation_hook_n = "integer",
      detector_candidate_cap = "integer",
      nested_review_candidate_cap = "integer",
      resource_scope_n = "integer", resource_cap_triggered_n = "integer",
      release_detector_invocation_n = "integer",
      release_scientific_candidate_n = "integer",
      publication_authority = "logical",
      scientific_result_influence = "character",
      manifest_payload_sha256 = "character",
      products_payload_sha256 = "character",
      resource_audit_sha256 = "character"
    ),
    burst_dispatch_final_route_v1 = c(
      train = "character", burst_pipeline_id = "character",
      max_candidates = "integer"
    ),
    burst_structure_first_pre_cap_v1 = c(
      train = "character", proposal_ordinal = "integer",
      source_candidate_id = "character",
      source_payload_sha256 = "character",
      detector_start_row = "integer", detector_end_row = "integer",
      start_isi = "integer", end_isi = "integer",
      candidate_layer = "character", candidate_class = "character",
      final_label = "character", gate_status = "character",
      action = "character", priority = "double", score = "double",
      intra_q90_sec = "double", max_intra_isi_sec = "double",
      pre_gap_sec = "double", post_gap_sec = "double",
      pre_ratio_q90 = "double", post_ratio_q90 = "double",
      edge_contrast_geom_q90 = "double",
      strict_boundary_pass = "logical",
      one_sided_boundary_pass = "logical",
      possible_boundary_pass = "logical",
      compactness_gate_active = "logical",
      q90_compact_pass = "logical", max_intra_strict_pass = "logical",
      tolerated_tail_pass = "logical", manual_negative_veto = "logical",
      max_candidates = "integer", deterministic_cap_rank = "integer",
      retained_by_cap = "logical", post_cap_ordinal = "integer",
      cap_decision = "character"
    ),
    burst_structure_first_post_cap_v1 = c(
      train = "character", post_cap_ordinal = "integer",
      proposal_ordinal = "integer", source_candidate_id = "character",
      source_payload_sha256 = "character",
      detector_start_row = "integer", detector_end_row = "integer",
      start_isi = "integer", end_isi = "integer"
    ),
    burst_structure_first_scan_receipt_v1 = c(
      train = "character", burst_pipeline_id = "character",
      applicability_status = "character", n_interval_rows = "integer",
      n_physical_isi = "integer", min_isi_sec = "double",
      structure_first_enabled = "logical",
      min_isi_n = "integer", max_isi_n = "integer",
      contrast_min = "double", geom_contrast_min = "double",
      compactness_quantile = "double", background_fraction = "double",
      min_train_valid_isi = "integer",
      max_internal_tail_ratio = "double", allow_endpoint = "logical",
      endpoint_as_canonical = "logical", valid_isi_n = "integer",
      train_compactness_quantile_sec = "double",
      compact_upper_sec = "double", compactness_gate_active = "logical",
      expected_window_n = "integer", visited_window_n = "integer",
      valid_window_n = "integer", pre_cap_proposal_n = "integer",
      retained_n = "integer", truncated_n = "integer",
      max_candidates = "integer", scan_exhausted = "logical",
      structure_cap_status = "character",
      input_support_sha256 = "character",
      pre_cap_payload_sha256 = "character",
      post_cap_payload_sha256 = "character"
    ),
    burst_detector_final_entry_v1 = c(
      train = "character", burst_pipeline_id = "character"
    ),
    burst_raw_threshold_support_v1 = c(
      train = "character", detector_row_index = "integer",
      isi_index = "integer", isi_sec = "double",
      artifact_isi = "logical", valid_isi = "logical",
      seed_band_member = "logical",
      native_bridge_extension_eligible = "logical",
      min_valid_isi_sec = "double", seed_low_sec = "double",
      seed_high_sec = "double", native_bridge_high_sec = "double",
      min_seed_isi_count = "integer"
    ),
    burst_seed_runs_v1 = c(
      train = "character", run_ordinal = "integer",
      detector_start_row = "integer", detector_end_row = "integer",
      start_isi = "integer", end_isi = "integer",
      seed_isi_count = "integer", min_seed_isi_count = "integer",
      min_seed_pass = "logical"
    ),
    burst_threshold_stage4_entry_v1 = c(
      train = "character", burst_pipeline_id = "character",
      applicability_status = "character", n_interval_rows = "integer",
      min_isi_sec = "double", max_candidates = "integer",
      max_expand = "integer", min_spikes = "integer",
      min_seed_isi_count = "integer",
      native_bridge_high_sec = "double",
      candidate_bridge_high_sec = "double", episode_upper_sec = "double",
      borrowed_run_max = "integer", episode_min_isi = "integer",
      single_connector_enabled = "logical",
      connector_side_seed_min = "integer"
    ),
    burst_threshold_stage4_attempts_v1 = c(
      train = "character", attempt_ordinal = "integer",
      subroute = "character", route_attempt_ordinal = "integer",
      source_root_type = "character", source_root_ordinal = "integer",
      source_root_start_row = "integer", source_root_end_row = "integer",
      source_root_start_isi = "integer", source_root_end_isi = "integer",
      source_root_sha256 = "character",
      proposed_start_row = "integer", proposed_end_row = "integer",
      proposed_start_isi = "integer", proposed_end_isi = "integer",
      geometry_available = "logical", geometry_key = "character",
      duplicate_domain = "character", geometry_claimed = "logical",
      duplicate_of_attempt_ordinal = "integer",
      rows_before_attempt = "integer", max_candidates = "integer",
      terminal_status = "character", terminal_reason = "character",
      candidate_append_ordinal = "integer", candidate_id = "character",
      candidate_source_sha256 = "character"
    ),
    burst_threshold_stage4_episode_roots_v1 = c(
      train = "character", root_ordinal = "integer",
      detector_start_row = "integer", detector_end_row = "integer",
      start_isi = "integer", end_isi = "integer",
      support_sha256 = "character"
    ),
    burst_threshold_stage4_candidates_v1 = c(
      train = "character", candidate_append_ordinal = "integer",
      source_attempt_ordinal = "integer", subroute = "character",
      candidate_id = "character", source_payload_sha256 = "character",
      candidate_observation_sha256 = "character",
      detector_start_row = "integer", detector_end_row = "integer",
      start_isi = "integer", end_isi = "integer",
      candidate_layer = "character", candidate_class = "character",
      final_label = "character", gate_status = "character",
      action = "character", priority = "double", score = "double"
    ),
    burst_threshold_stage4_receipt_v1 = c(
      train = "character", burst_pipeline_id = "character",
      applicability_status = "character", n_interval_rows = "integer",
      max_candidates = "integer", max_expand = "integer",
      candidate_bridge_high_sec = "double", episode_upper_sec = "double",
      borrowed_run_max = "integer", single_connector_enabled = "logical",
      seed_root_n = "integer", seed_eligible_root_n = "integer",
      episode_root_n = "integer", observed_attempt_n = "integer",
      emitted_candidate_n = "integer", duplicate_n = "integer",
      filter_rejected_n = "integer", seed_attempt_n = "integer",
      dense_attempt_n = "integer", connector_attempt_n = "integer",
      seed_candidate_n = "integer", dense_candidate_n = "integer",
      connector_candidate_n = "integer", seed_status = "character",
      dense_status = "character", connector_status = "character",
      cap_check_triggered = "logical", stop_subroute = "character",
      stop_source_root_ordinal = "integer", stop_rows_n = "integer",
      stage4_search_exhausted = "logical", coverage_status = "character",
      proposal_intrusion_mask_sha256 = "character",
      episode_support_sha256 = "character",
      attempt_payload_sha256 = "character",
      candidate_payload_sha256 = "character"
    ),
    burst_union_pre_cap_v1 = c(
      train = "character", union_ordinal = "integer",
      source_route = "character", source_ordinal = "integer",
      source_candidate_id = "character",
      source_payload_sha256 = "character",
      detector_start_row = "integer", detector_end_row = "integer",
      start_isi = "integer", end_isi = "integer",
      candidate_layer = "character", candidate_class = "character",
      final_label = "character", strict_boundary_pass = "logical",
      strict_structure_anchor = "logical",
      priority = "double", score = "double", max_candidates = "integer",
      deterministic_cap_rank = "integer", retained_by_cap = "logical",
      post_cap_ordinal = "integer", cap_decision = "character"
    ),
    burst_union_post_cap_v1 = c(
      train = "character", post_cap_ordinal = "integer",
      union_ordinal = "integer", source_route = "character",
      source_ordinal = "integer", source_candidate_id = "character",
      source_payload_sha256 = "character",
      detector_start_row = "integer", detector_end_row = "integer",
      start_isi = "integer", end_isi = "integer"
    ),
    burst_union_rank_receipt_v1 = c(
      train = "character", burst_pipeline_id = "character",
      structure_input_n = "integer", threshold_input_n = "integer",
      observed_input_n = "integer", retained_n = "integer",
      truncated_n = "integer", max_candidates = "integer",
      ranking_exhausted = "logical", rank_scope_status = "character",
      structure_upstream_status = "character",
      threshold_upstream_status = "character",
      overall_universe_status = "character",
      union_cap_status = "character", route_order = "character",
      pre_cap_payload_sha256 = "character",
      post_cap_payload_sha256 = "character"
    ),
    burst_refractory_entry_v1 = c(
      train = "character", burst_pipeline_id = "character",
      applicability_status = "character", input_candidate_n = "integer",
      normalized_policy_action = "character", min_isi_sec = "double",
      refractory_threshold_sec = "double", min_spikes = "integer",
      context_available = "logical", input_payload_sha256 = "character"
    ),
    burst_refractory_attempts_v1 = c(
      train = "character", attempt_ordinal = "integer",
      source_input_ordinal = "integer", source_candidate_id = "character",
      source_payload_sha256 = "character", attempt_kind = "character",
      fragment_ordinal = "integer", proposed_detector_start_row = "integer",
      proposed_detector_end_row = "integer", proposed_start_isi = "integer",
      proposed_end_isi = "integer", geometry_available = "logical",
      suspect_candidate = "logical", terminal_status = "character",
      terminal_reason = "character", output_ordinal = "integer",
      output_candidate_id = "character", output_payload_sha256 = "character"
    ),
    burst_refractory_output_v1 = c(
      train = "character", output_ordinal = "integer",
      source_input_ordinal = "integer", relation = "character",
      source_candidate_id = "character", source_payload_sha256 = "character",
      output_candidate_id = "character", output_payload_sha256 = "character",
      detector_start_row = "integer", detector_end_row = "integer",
      start_isi = "integer", end_isi = "integer",
      final_label = "character", gate_status = "character",
      action = "character", priority = "double", score = "double"
    ),
    burst_refractory_receipt_v1 = c(
      train = "character", burst_pipeline_id = "character",
      applicability_status = "character", normalized_policy_action = "character",
      input_candidate_n = "integer", suspect_candidate_n = "integer",
      candidate_attempt_n = "integer", fragment_attempt_n = "integer",
      fragment_discarded_n = "integer", output_candidate_n = "integer",
      retained_output_n = "integer", retyped_output_n = "integer",
      rejected_parent_n = "integer", fragment_output_n = "integer",
      replay_exhausted = "logical", evidence_origin = "character",
      input_scope = "character", upstream_universe_status = "character",
      coverage_status = "character",
      input_payload_sha256 = "character", attempt_payload_sha256 = "character",
      output_payload_sha256 = "character"
    ),
    burst_pause_boundary_entry_v1 = c(
      train = "character", burst_pipeline_id = "character",
      applicability_status = "character", input_candidate_n = "integer",
      raw_pause_candidate_n = "integer", canonical_boundary_n = "integer",
      evidence_origin = "character", input_scope = "character",
      upstream_burst_status = "character",
      upstream_pause_status = "character",
      upstream_universe_status = "character",
      input_payload_sha256 = "character",
      raw_pause_payload_sha256 = "character",
      boundary_payload_sha256 = "character"
    ),
    burst_pause_boundary_sources_v1 = c(
      train = "character", boundary_ordinal = "integer",
      detector_start_row = "integer", detector_end_row = "integer",
      start_isi = "integer", end_isi = "integer",
      boundary_kind = "character", source_pause_candidate_n = "integer",
      source_pause_ordinals = "character",
      source_pause_candidate_ids = "character",
      source_pause_payload_sha256 = "character"
    ),
    burst_pause_boundary_attempts_v1 = c(
      train = "character", attempt_ordinal = "integer",
      source_input_ordinal = "integer", source_candidate_id = "character",
      source_payload_sha256 = "character", geometry_available = "logical",
      detector_start_row = "integer", detector_end_row = "integer",
      overlapping_boundary_n = "integer",
      overlapping_boundary_ordinals = "character",
      overlapping_boundary_sha256 = "character", blocked = "logical",
      terminal_status = "character", terminal_reason = "character",
      redetection_performed = "logical", output_ordinal = "integer",
      output_candidate_id = "character", output_payload_sha256 = "character"
    ),
    burst_pause_boundary_output_v1 = c(
      train = "character", output_ordinal = "integer",
      source_input_ordinal = "integer", relation = "character",
      source_candidate_id = "character", source_payload_sha256 = "character",
      output_candidate_id = "character", output_payload_sha256 = "character",
      detector_start_row = "integer", detector_end_row = "integer",
      start_isi = "integer", end_isi = "integer",
      final_label = "character", action = "character",
      priority = "double", score = "double",
      hard_boundary_conflict = "logical",
      hard_boundary_conflict_kind = "character",
      hard_boundary_decision = "character"
    ),
    burst_pause_boundary_receipt_v1 = c(
      train = "character", burst_pipeline_id = "character",
      applicability_status = "character", input_candidate_n = "integer",
      canonical_boundary_n = "integer", candidate_attempt_n = "integer",
      output_candidate_n = "integer", blocked_candidate_n = "integer",
      retained_candidate_n = "integer", invalid_geometry_n = "integer",
      redetection_n = "integer", replay_exhausted = "logical",
      evidence_origin = "character", input_scope = "character",
      upstream_burst_status = "character",
      upstream_pause_status = "character",
      upstream_universe_status = "character", coverage_status = "character",
      entry_payload_sha256 = "character",
      source_payload_sha256 = "character",
      attempt_payload_sha256 = "character",
      output_payload_sha256 = "character"
    ),
    burst_pause_ownership_entry_v1 = c(
      train = "character", burst_pipeline_id = "character",
      applicability_status = "character", input_candidate_n = "integer",
      eligible_candidate_n = "integer", selected_atom_n = "integer",
      canonical_boundary_n = "integer", bridge_upper_sec = "double",
      bridge_ceiling_sec = "double", bridge_factor = "double",
      seed_low_sec = "double", seed_high_sec = "double",
      min_seed_isi_n = "integer", max_gap_n = "integer",
      max_bridge_n = "double", classic_max_spikes = "integer",
      max_bridge_fraction = "double", evidence_origin = "character",
      input_scope = "character", upstream_boundary_status = "character",
      upstream_pause_status = "character",
      upstream_universe_status = "character", output_role = "character",
      input_payload_sha256 = "character",
      boundary_payload_sha256 = "character"
    ),
    burst_pause_ownership_eligibility_v1 = c(
      train = "character", input_ordinal = "integer",
      source_candidate_id = "character", source_payload_sha256 = "character",
      detector_start_row = "integer", detector_end_row = "integer",
      final_label = "character", action = "character",
      eligible = "logical", eligibility_reason = "character",
      boundary_output_ordinal = "integer",
      boundary_output_payload_sha256 = "character"
    ),
    burst_pause_ownership_selection_v1 = c(
      train = "character", accepted_pool_ordinal = "integer",
      source_input_ordinal = "integer", source_candidate_id = "character",
      source_payload_sha256 = "character", detector_start_row = "integer",
      detector_end_row = "integer", selector_end_start_rank = "integer",
      candidate_value = "double", selected_atom = "logical",
      selection_status = "character", atom_ordinal = "integer"
    ),
    burst_pause_ownership_attempts_v1 = c(
      train = "character", attempt_ordinal = "integer",
      left_atom_ordinal = "integer", right_atom_ordinal = "integer",
      left_source_input_ordinal = "integer",
      right_source_input_ordinal = "integer",
      left_candidate_id = "character", right_candidate_id = "character",
      left_payload_sha256 = "character", right_payload_sha256 = "character",
      proposed_detector_start_row = "integer",
      proposed_detector_end_row = "integer",
      overlapping_boundary_ordinals = "character",
      spanning_witness_n = "integer", spanning_witness_input_ordinals = "character",
      spanning_witness_candidate_ids = "character",
      spanning_witness_payload_sha256 = "character",
      gap_start_row = "integer", gap_end_row = "integer",
      gap_isi_n = "integer", gap_isi_sec = "double",
      gap_valid = "logical", core_isi_n = "integer",
      bridge_isi_n = "integer", bridge_fraction = "double",
      expanded_bridge_isi_n = "integer", q90_sec = "double",
      boundary_gate = "character", witness_gate = "character",
      gap_shape_gate = "character", gap_validity_gate = "character",
      support_gate = "character", ceiling_gate = "character",
      core_gate = "character", bridge_count_gate = "character",
      bridge_fraction_gate = "character",
      expanded_bridge_gate = "character", q90_gate = "character",
      construction_gate = "character",
      terminal_status = "character", terminal_reason = "character",
      redetection_performed = "logical", output_ordinal = "integer",
      output_candidate_id = "character", output_payload_sha256 = "character"
    ),
    burst_pause_ownership_output_v1 = c(
      train = "character", output_ordinal = "integer", relation = "character",
      source_input_ordinals = "character", source_candidate_ids = "character",
      source_payload_sha256 = "character",
      witness_input_ordinals = "character", witness_candidate_ids = "character",
      witness_payload_sha256 = "character", output_candidate_id = "character",
      output_payload_sha256 = "character", detector_start_row = "integer",
      detector_end_row = "integer", start_isi = "integer", end_isi = "integer",
      final_label = "character", action = "character",
      selected_for_auto = "logical", selection_status = "character",
      ownership_role = "character", event_materialization_status = "character"
    ),
    burst_pause_ownership_receipt_v1 = c(
      train = "character", burst_pipeline_id = "character",
      applicability_status = "character", input_candidate_n = "integer",
      eligible_candidate_n = "integer", selector_winner_n = "integer",
      selector_loser_n = "integer", pair_attempt_n = "integer",
      successful_merge_n = "integer", failed_merge_n = "integer",
      retained_atom_n = "integer", lone_tail_n = "integer",
      output_support_n = "integer", redetection_n = "integer",
      one_pass_control_path_exhausted = "logical", cap_applied = "logical",
      evidence_origin = "character", input_scope = "character",
      upstream_boundary_status = "character",
      upstream_pause_status = "character",
      upstream_universe_status = "character", output_role = "character",
      coverage_status = "character", entry_payload_sha256 = "character",
      eligibility_payload_sha256 = "character",
      selection_payload_sha256 = "character",
      attempt_payload_sha256 = "character", output_payload_sha256 = "character",
      scientific_output_sha256 = "character"
    ),
    burst_hard_threshold_entry_v1 = c(
      train = "character", root_id = "character",
      applicability_status = "character", generator_invoked = "logical",
      burst_family_requested = "logical",
      generation_gated_by_requested_patterns = "logical",
      downstream_pattern_eligibility_status = "character",
      n_interval_rows = "integer", n_physical_isi = "integer",
      min_isi_sec = "double", train_range_present = "logical",
      threshold_mode = "character", hard_threshold_applicable = "logical",
      hard_threshold_source = "character",
      hard_burst_seed_upper_sec = "double",
      hard_burst_bridge_upper_sec = "double",
      min_core_isi_count = "integer", min_spikes = "integer",
      shared_vp_configuration = "logical",
      root_independence_scope = "character",
      input_data_sha256 = "character",
      params_payload_sha256 = "character", vp_payload_sha256 = "character"
    ),
    burst_hard_threshold_support_v1 = c(
      train = "character", detector_row_index = "integer",
      isi_index = "integer", isi_sec = "double",
      artifact_isi = "logical", valid_isi = "logical",
      seed_band_member = "logical", bridge_band_member = "logical",
      hard_burst_seed_upper_sec = "double",
      hard_burst_bridge_upper_sec = "double"
    ),
    burst_hard_threshold_runs_v1 = c(
      train = "character", run_ordinal = "integer",
      detector_start_row = "integer", detector_end_row = "integer",
      start_isi = "integer", end_isi = "integer",
      bridge_isi_count = "integer", core_isi_count = "integer",
      n_spikes = "integer", min_core_isi_count = "integer",
      min_spikes = "integer", core_count_gate = "character",
      spike_count_gate = "character", construction_attempted = "logical",
      candidate_counter = "integer", candidate_id = "character",
      terminal_status = "character", terminal_reason = "character",
      emitted_candidate_ordinal = "integer",
      candidate_payload_sha256 = "character"
    ),
    burst_hard_threshold_pre_refractory_candidates_v1 = c(
      train = "character", candidate_ordinal = "integer",
      source_run_ordinal = "integer", candidate_id = "character",
      candidate_payload_sha256 = "character",
      detector_start_row = "integer", detector_end_row = "integer",
      start_isi = "integer", end_isi = "integer",
      candidate_layer = "character", candidate_class = "character",
      final_label = "character", gate_status = "character",
      action = "character", priority = "double", score = "double",
      hard_burst_core_isi_count = "integer",
      hard_burst_seed_upper_sec = "double",
      hard_burst_bridge_upper_sec = "double",
      hard_threshold_source = "character", proposal_track = "character",
      event_materialization_status = "character"
    ),
    burst_hard_threshold_refractory_attempts_v1 = c(
      train = "character", attempt_ordinal = "integer",
      source_input_ordinal = "integer", source_candidate_id = "character",
      source_payload_sha256 = "character", attempt_kind = "character",
      fragment_ordinal = "integer", proposed_detector_start_row = "integer",
      proposed_detector_end_row = "integer", proposed_start_isi = "integer",
      proposed_end_isi = "integer", geometry_available = "logical",
      suspect_candidate = "logical", terminal_status = "character",
      terminal_reason = "character", output_ordinal = "integer",
      output_candidate_id = "character", output_payload_sha256 = "character"
    ),
    burst_hard_threshold_pre_boundary_output_v1 = c(
      train = "character", output_ordinal = "integer",
      source_input_ordinal = "integer", source_run_ordinal = "integer",
      relation = "character", source_candidate_id = "character",
      source_payload_sha256 = "character", output_candidate_id = "character",
      output_payload_sha256 = "character", detector_start_row = "integer",
      detector_end_row = "integer", start_isi = "integer",
      end_isi = "integer", final_label = "character",
      gate_status = "character", action = "character",
      priority = "double", score = "double", proposal_track = "character",
      event_materialization_status = "character"
    ),
    burst_hard_threshold_boundary_sources_v1 = c(
      train = "character", boundary_ordinal = "integer",
      detector_start_row = "integer", detector_end_row = "integer",
      start_isi = "integer", end_isi = "integer",
      boundary_kind = "character", source_pause_candidate_n = "integer",
      source_pause_ordinals = "character",
      source_pause_candidate_ids = "character",
      source_pause_payload_sha256 = "character"
    ),
    burst_hard_threshold_boundary_attempts_v1 = c(
      train = "character", attempt_ordinal = "integer",
      source_input_ordinal = "integer", source_candidate_id = "character",
      source_payload_sha256 = "character", geometry_available = "logical",
      detector_start_row = "integer", detector_end_row = "integer",
      overlapping_boundary_n = "integer",
      overlapping_boundary_ordinals = "character",
      overlapping_boundary_sha256 = "character", blocked = "logical",
      terminal_status = "character", terminal_reason = "character",
      redetection_performed = "logical", output_ordinal = "integer",
      output_candidate_id = "character", output_payload_sha256 = "character"
    ),
    burst_hard_threshold_post_boundary_output_v1 = c(
      train = "character", output_ordinal = "integer",
      source_input_ordinal = "integer", source_run_ordinal = "integer",
      relation = "character", source_candidate_id = "character",
      source_payload_sha256 = "character", output_candidate_id = "character",
      output_payload_sha256 = "character", detector_start_row = "integer",
      detector_end_row = "integer", start_isi = "integer",
      end_isi = "integer", final_label = "character",
      action = "character", priority = "double", score = "double",
      hard_boundary_conflict = "logical",
      hard_boundary_conflict_kind = "character",
      hard_boundary_decision = "character", proposal_track = "character",
      event_materialization_status = "character"
    ),
    burst_hard_threshold_receipt_v1 = c(
      train = "character", root_id = "character",
      applicability_status = "character", generator_invoked = "logical",
      burst_family_requested = "logical",
      generation_gated_by_requested_patterns = "logical",
      downstream_pattern_eligibility_status = "character",
      pause_pattern_requested = "logical",
      support_row_n = "integer", valid_isi_n = "integer",
      seed_member_n = "integer", bridge_member_n = "integer",
      bridge_run_n = "integer", core_rejected_run_n = "integer",
      spike_rejected_run_n = "integer", construction_failed_run_n = "integer",
      pre_refractory_candidate_n = "integer",
      refractory_attempt_n = "integer", pre_boundary_output_n = "integer",
      canonical_boundary_n = "integer", boundary_attempt_n = "integer",
      boundary_blocked_n = "integer", post_boundary_output_n = "integer",
      run_scan_exhausted = "logical", cap_applied = "logical",
      deduplication_applied = "logical", scientific_order = "character",
      evidence_origin = "character", input_scope = "character",
      root_independence_scope = "character",
      upstream_pause_status = "character",
      upstream_universe_status = "character", output_role = "character",
      coverage_status = "character", entry_payload_sha256 = "character",
      support_payload_sha256 = "character", run_payload_sha256 = "character",
      pre_refractory_payload_sha256 = "character",
      refractory_attempt_payload_sha256 = "character",
      pre_boundary_payload_sha256 = "character",
      boundary_source_payload_sha256 = "character",
      boundary_attempt_payload_sha256 = "character",
      post_boundary_payload_sha256 = "character",
      scientific_pre_boundary_sha256 = "character",
      scientific_post_boundary_sha256 = "character"
    ),
    NULL
  )
}

stpd_candidate_lineage_empty_hook_payload <- function(hook_id) {
  schema <- stpd_candidate_lineage_observation_hook_schema(hook_id)
  if (is.null(schema)) {
    stpd_candidate_lineage_abort(
      "collector_hook_unregistered",
      "Cannot construct an empty payload for an unregistered hook.",
      field = "hook_id"
    )
  }
  constructors <- list(
    character = character, integer = integer,
    double = double, logical = logical
  )
  columns <- lapply(unname(schema), function(type) constructors[[type]]())
  names(columns) <- names(schema)
  as.data.frame(
    columns, stringsAsFactors = FALSE, check.names = FALSE,
    optional = TRUE
  )
}

stpd_candidate_lineage_candidate_source_hash <- function(candidate_row,
                                                          source_route) {
  row <- as.data.frame(
    candidate_row, stringsAsFactors = FALSE, check.names = FALSE
  )
  # bind_rows() widens the two candidate routes with route-specific columns.
  # Hash the observed source row in a representation that is invariant to
  # those all-missing widening columns, while retaining every observed value
  # (including priority, score, labels and gate evidence).  Sorting columns
  # also makes the hash independent of route-specific column insertion order.
  observed <- vapply(row, function(column) {
    length(column) == 1L && !is.na(column[[1L]])
  }, logical(1))
  row <- row[, observed, drop = FALSE]
  if (ncol(row)) {
    row <- row[, sort(names(row), method = "radix"), drop = FALSE]
  }
  rownames(row) <- NULL
  stpd_threshold_first_hash_domain(
    "stpd-burst-live-source-candidate-v1",
    list(source_route = as.character(source_route)[1L], candidate = row)
  )
}

stpd_candidate_lineage_candidate_column <- function(
    candidates, field, type = c("character", "integer", "double", "logical"),
    default = NULL) {
  type <- match.arg(type)
  n <- nrow(candidates)
  if (is.null(default)) {
    default <- switch(
      type, character = "", integer = NA_integer_,
      double = NA_real_, logical = FALSE
    )
  }
  value <- if (field %in% names(candidates)) candidates[[field]] else
    rep(default, n)
  switch(
    type, character = as.character(value), integer = as.integer(value),
    double = as.numeric(value), logical = as.logical(value)
  )
}

stpd_candidate_lineage_structure_pre_payload <- function(
    candidates, train, max_candidates) {
  if (!nrow(candidates)) {
    return(stpd_candidate_lineage_empty_hook_payload(
      "burst_structure_first_pre_cap_v1"
    ))
  }
  n <- nrow(candidates)
  proposal_ordinal <- as.integer(seq_len(n))
  detector_start <- stpd_candidate_lineage_candidate_column(
    candidates, "start_isi", "integer"
  )
  detector_end <- stpd_candidate_lineage_candidate_column(
    candidates, "end_isi", "integer"
  )
  priority <- stpd_candidate_lineage_candidate_column(
    candidates, "priority", "double"
  )
  score <- stpd_candidate_lineage_candidate_column(
    candidates, "score", "double"
  )
  ranked <- order(
    -priority, -score, detector_start, detector_end, proposal_ordinal,
    method = "radix", na.last = TRUE
  )
  deterministic_cap_rank <- integer(n)
  deterministic_cap_rank[ranked] <- seq_len(n)
  max_candidates <- max(1L, as.integer(max_candidates)[1L])
  retained_index <- if (n > max_candidates) {
    head(ranked, max_candidates)
  } else {
    seq_len(n)
  }
  retained_by_cap <- seq_len(n) %in% retained_index
  scientific_order <- retained_index[order(
    detector_start[retained_index], detector_end[retained_index],
    -priority[retained_index], -score[retained_index]
  )]
  post_cap_ordinal <- rep(NA_integer_, n)
  post_cap_ordinal[scientific_order] <- seq_along(scientific_order)
  source_hash <- vapply(
    seq_len(n),
    function(i) stpd_candidate_lineage_candidate_source_hash(
      candidates[i, , drop = FALSE], "structure_first"
    ),
    character(1)
  )
  data.frame(
    train = rep(as.character(train)[1L], n),
    proposal_ordinal = proposal_ordinal,
    source_candidate_id = stpd_candidate_lineage_candidate_column(
      candidates, "candidate_id", "character"
    ),
    source_payload_sha256 = source_hash,
    detector_start_row = detector_start,
    detector_end_row = detector_end,
    start_isi = as.integer(detector_start - 1L),
    end_isi = as.integer(detector_end - 1L),
    candidate_layer = stpd_candidate_lineage_candidate_column(
      candidates, "candidate_layer", "character"
    ),
    candidate_class = stpd_candidate_lineage_candidate_column(
      candidates, "candidate_class", "character"
    ),
    final_label = stpd_candidate_lineage_candidate_column(
      candidates, "final_label", "character"
    ),
    gate_status = stpd_candidate_lineage_candidate_column(
      candidates, "gate_status", "character"
    ),
    action = stpd_candidate_lineage_candidate_column(
      candidates, "action", "character"
    ),
    priority = priority, score = score,
    intra_q90_sec = stpd_candidate_lineage_candidate_column(
      candidates, "intra_q90_sec", "double"
    ),
    max_intra_isi_sec = stpd_candidate_lineage_candidate_column(
      candidates, "max_intra_ISI_sec", "double"
    ),
    pre_gap_sec = stpd_candidate_lineage_candidate_column(
      candidates, "pre_gap_sec", "double"
    ),
    post_gap_sec = stpd_candidate_lineage_candidate_column(
      candidates, "post_gap_sec", "double"
    ),
    pre_ratio_q90 = stpd_candidate_lineage_candidate_column(
      candidates, "pre_ratio_q90", "double"
    ),
    post_ratio_q90 = stpd_candidate_lineage_candidate_column(
      candidates, "post_ratio_q90", "double"
    ),
    edge_contrast_geom_q90 = stpd_candidate_lineage_candidate_column(
      candidates, "edge_contrast_geom_q90", "double"
    ),
    strict_boundary_pass = stpd_candidate_lineage_candidate_column(
      candidates, "strict_boundary_pass", "logical"
    ),
    one_sided_boundary_pass = stpd_candidate_lineage_candidate_column(
      candidates, "one_sided_boundary_pass", "logical"
    ),
    possible_boundary_pass = stpd_candidate_lineage_candidate_column(
      candidates, "possible_boundary_pass", "logical"
    ),
    compactness_gate_active = stpd_candidate_lineage_candidate_column(
      candidates, "structure_first_compactness_gate_active", "logical"
    ),
    q90_compact_pass = stpd_candidate_lineage_candidate_column(
      candidates, "structure_first_q90_compact_pass", "logical"
    ),
    max_intra_strict_pass = stpd_candidate_lineage_candidate_column(
      candidates, "structure_first_max_intra_strict_pass", "logical"
    ),
    tolerated_tail_pass = stpd_candidate_lineage_candidate_column(
      candidates, "structure_first_tolerated_tail_pass", "logical"
    ),
    manual_negative_veto = stpd_candidate_lineage_candidate_column(
      candidates, "manual_negative_veto", "logical"
    ),
    max_candidates = rep(max_candidates, n),
    deterministic_cap_rank = as.integer(deterministic_cap_rank),
    retained_by_cap = retained_by_cap,
    post_cap_ordinal = as.integer(post_cap_ordinal),
    cap_decision = ifelse(
      retained_by_cap, "retained", "truncated_by_cap"
    ),
    stringsAsFactors = FALSE, check.names = FALSE
  )
}

stpd_candidate_lineage_structure_post_payload <- function(
    candidates, pre_payload, train) {
  if (!nrow(candidates)) {
    return(stpd_candidate_lineage_empty_hook_payload(
      "burst_structure_first_post_cap_v1"
    ))
  }
  hashes <- vapply(
    seq_len(nrow(candidates)),
    function(i) stpd_candidate_lineage_candidate_source_hash(
      candidates[i, , drop = FALSE], "structure_first"
    ),
    character(1)
  )
  ids <- stpd_candidate_lineage_candidate_column(
    candidates, "candidate_id", "character"
  )
  detector_start <- stpd_candidate_lineage_candidate_column(
    candidates, "start_isi", "integer"
  )
  detector_end <- stpd_candidate_lineage_candidate_column(
    candidates, "end_isi", "integer"
  )
  key <- paste(ids, hashes, detector_start, detector_end, sep = "\r")
  pre_key <- paste(
    pre_payload$source_candidate_id, pre_payload$source_payload_sha256,
    pre_payload$detector_start_row, pre_payload$detector_end_row, sep = "\r"
  )
  source_row <- match(key, pre_key)
  if (anyNA(source_row) || anyDuplicated(source_row)) {
    stpd_candidate_lineage_abort(
      "collector_structure_post_not_closed",
      "Structure-first post-cap candidates do not close to the pre-cap set."
    )
  }
  data.frame(
    train = rep(as.character(train)[1L], length(source_row)),
    post_cap_ordinal = as.integer(seq_along(source_row)),
    proposal_ordinal = pre_payload$proposal_ordinal[source_row],
    source_candidate_id = ids,
    source_payload_sha256 = hashes,
    detector_start_row = detector_start,
    detector_end_row = detector_end,
    start_isi = as.integer(detector_start - 1L),
    end_isi = as.integer(detector_end - 1L),
    stringsAsFactors = FALSE, check.names = FALSE
  )
}

stpd_candidate_lineage_capture_structure_first <- function(
    train_collector, dat, train, min_isi_sec, settings = NULL,
    context = NULL, pre_cap_candidates = data.frame(),
    post_cap_candidates = data.frame(), applicability_status,
    expected_window_n = 0L, visited_window_n = 0L,
    valid_window_n = 0L, scan_exhausted = FALSE,
    max_candidates = 1L) {
  if (is.null(train_collector)) return(invisible(NULL))
  max_candidates <- max(1L, as.integer(max_candidates)[1L])
  pre <- stpd_candidate_lineage_structure_pre_payload(
    pre_cap_candidates, train, max_candidates
  )
  post <- stpd_candidate_lineage_structure_post_payload(
    post_cap_candidates, pre, train
  )
  stpd_candidate_lineage_collector_capture(
    train_collector, "burst_structure_first_pre_cap_v1", pre
  )
  stpd_candidate_lineage_collector_capture(
    train_collector, "burst_structure_first_post_cap_v1", post
  )

  n <- nrow(dat)
  isi <- suppressWarnings(as.numeric(dat$ISI_sec))
  art <- is_artifact_isi(isi, min_isi_sec)
  valid <- is.finite(isi) & !art
  if (length(valid)) valid[1L] <- FALSE
  support_rows <- if (n >= 2L) seq.int(2L, n) else integer()
  input_support <- data.frame(
    detector_row_index = as.integer(support_rows),
    isi_index = as.integer(seq_along(support_rows)),
    isi_sec = as.numeric(isi[support_rows]),
    artifact_isi = as.logical(art[support_rows]),
    valid_isi = as.logical(valid[support_rows]),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  setting_value <- function(field, type, default) {
    value <- if (!is.null(settings) && !is.null(settings[[field]])) {
      settings[[field]][1L]
    } else default
    switch(
      type, integer = as.integer(value), double = as.numeric(value),
      logical = as.logical(value)
    )
  }
  context_value <- function(field, type, default) {
    value <- if (!is.null(context) && !is.null(context[[field]])) {
      context[[field]][1L]
    } else default
    switch(
      type, integer = as.integer(value), double = as.numeric(value),
      logical = as.logical(value)
    )
  }
  truncated_n <- nrow(pre) - nrow(post)
  cap_status <- if (!isTRUE(scan_exhausted)) {
    "not_applicable"
  } else if (!nrow(pre)) {
    "not_applied_empty"
  } else if (truncated_n > 0L) {
    "applied_complete"
  } else {
    "not_applied_within_limit"
  }
  receipt <- data.frame(
    train = as.character(train)[1L], burst_pipeline_id = "final",
    applicability_status = as.character(applicability_status)[1L],
    n_interval_rows = as.integer(n),
    n_physical_isi = as.integer(max(0L, n - 1L)),
    min_isi_sec = as.numeric(min_isi_sec)[1L],
    structure_first_enabled = setting_value(
      "enabled", "logical", FALSE
    ),
    min_isi_n = setting_value("min_isi_n", "integer", NA_integer_),
    max_isi_n = setting_value("max_isi_n", "integer", NA_integer_),
    contrast_min = setting_value("contrast_min", "double", NA_real_),
    geom_contrast_min = setting_value(
      "geom_contrast_min", "double", NA_real_
    ),
    compactness_quantile = setting_value(
      "compactness_quantile", "double", NA_real_
    ),
    background_fraction = setting_value(
      "background_fraction", "double", NA_real_
    ),
    min_train_valid_isi = setting_value(
      "min_train_valid_isi", "integer", NA_integer_
    ),
    max_internal_tail_ratio = setting_value(
      "max_internal_tail_ratio", "double", NA_real_
    ),
    allow_endpoint = setting_value(
      "allow_endpoint", "logical", FALSE
    ),
    endpoint_as_canonical = setting_value(
      "endpoint_as_canonical", "logical", FALSE
    ),
    valid_isi_n = context_value("valid_isi_n", "integer", 0L),
    train_compactness_quantile_sec = context_value(
      "train_compactness_quantile_sec", "double", NA_real_
    ),
    compact_upper_sec = context_value(
      "compact_upper_sec", "double", NA_real_
    ),
    compactness_gate_active = context_value(
      "compactness_gate_active", "logical", FALSE
    ),
    expected_window_n = as.integer(expected_window_n)[1L],
    visited_window_n = as.integer(visited_window_n)[1L],
    valid_window_n = as.integer(valid_window_n)[1L],
    pre_cap_proposal_n = as.integer(nrow(pre)),
    retained_n = as.integer(nrow(post)),
    truncated_n = as.integer(truncated_n),
    max_candidates = max_candidates,
    scan_exhausted = as.logical(scan_exhausted)[1L],
    structure_cap_status = cap_status,
    input_support_sha256 = stpd_threshold_first_hash_domain(
      "stpd-structure-first-input-support-v1", input_support
    ),
    pre_cap_payload_sha256 = stpd_threshold_first_hash_domain(
      "stpd-structure-first-pre-cap-v1", pre
    ),
    post_cap_payload_sha256 = stpd_threshold_first_hash_domain(
      "stpd-structure-first-post-cap-v1", post
    ),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  stpd_candidate_lineage_collector_capture(
    train_collector, "burst_structure_first_scan_receipt_v1", receipt
  )
  invisible(list(pre = pre, post = post, receipt = receipt))
}

stpd_candidate_lineage_union_pre_payload <- function(
    structure_first, threshold_centred, train, max_candidates) {
  if (is.null(structure_first)) structure_first <- data.frame()
  if (is.null(threshold_centred)) threshold_centred <- data.frame()
  structure_n <- nrow(structure_first)
  threshold_n <- nrow(threshold_centred)
  candidates <- dplyr::bind_rows(structure_first, threshold_centred)
  if (!nrow(candidates)) {
    return(stpd_candidate_lineage_empty_hook_payload(
      "burst_union_pre_cap_v1"
    ))
  }
  n <- nrow(candidates)
  union_ordinal <- as.integer(seq_len(n))
  source_route <- c(
    rep("structure_first", structure_n),
    rep("threshold_centred", threshold_n)
  )
  source_ordinal <- as.integer(c(
    seq_len(structure_n), seq_len(threshold_n)
  ))
  layer <- stpd_candidate_lineage_candidate_column(
    candidates, "candidate_layer", "character"
  )
  final_label <- stpd_candidate_lineage_candidate_column(
    candidates, "final_label", "character"
  )
  strict_boundary <- stpd_candidate_lineage_candidate_column(
    candidates, "strict_boundary_pass", "logical"
  )
  # bind_rows() creates an explicit NA for threshold-centred candidates that
  # do not carry the structure-first-only boundary flag.  The scientific
  # union cap treats that widened NA as FALSE; the observation must replay the
  # same ranking semantics rather than preserving an indeterminate value.
  strict_boundary[is.na(strict_boundary)] <- FALSE
  strict_structure_anchor <- layer == "structure_first_burst_screen" &
    strict_boundary &
    final_label %in% c("burst", "long_burst", "possible_burst")
  priority <- stpd_candidate_lineage_candidate_column(
    candidates, "priority", "double"
  )
  priority_rank <- priority
  priority_rank[!is.finite(priority_rank)] <- -Inf
  score <- stpd_candidate_lineage_candidate_column(
    candidates, "score", "double"
  )
  score_rank <- score
  score_rank[!is.finite(score_rank)] <- -Inf
  detector_start <- stpd_candidate_lineage_candidate_column(
    candidates, "start_isi", "integer"
  )
  start_rank <- detector_start
  start_rank[!is.finite(start_rank)] <- .Machine$integer.max
  detector_end <- stpd_candidate_lineage_candidate_column(
    candidates, "end_isi", "integer"
  )
  end_rank <- detector_end
  end_rank[!is.finite(end_rank)] <- .Machine$integer.max
  candidate_id <- stpd_candidate_lineage_candidate_column(
    candidates, "candidate_id", "character"
  )
  ranked <- order(
    -as.integer(strict_structure_anchor), -priority_rank, -score_rank,
    start_rank, end_rank, layer, candidate_id, union_ordinal,
    method = "radix", na.last = TRUE
  )
  deterministic_cap_rank <- integer(n)
  deterministic_cap_rank[ranked] <- seq_len(n)
  max_candidates <- max(1L, as.integer(max_candidates)[1L])
  retained_index <- if (n > max_candidates) {
    sort(head(ranked, max_candidates))
  } else {
    seq_len(n)
  }
  retained_by_cap <- seq_len(n) %in% retained_index
  post_cap_ordinal <- rep(NA_integer_, n)
  post_cap_ordinal[retained_index] <- seq_along(retained_index)
  structure_hash <- if (structure_n) vapply(
    seq_len(structure_n),
    function(i) stpd_candidate_lineage_candidate_source_hash(
      structure_first[i, , drop = FALSE], "structure_first"
    ), character(1)
  ) else character()
  threshold_hash <- if (threshold_n) vapply(
    seq_len(threshold_n),
    function(i) stpd_candidate_lineage_candidate_source_hash(
      threshold_centred[i, , drop = FALSE], "threshold_centred"
    ), character(1)
  ) else character()
  source_hash <- c(structure_hash, threshold_hash)
  data.frame(
    train = rep(as.character(train)[1L], n),
    union_ordinal = union_ordinal, source_route = source_route,
    source_ordinal = source_ordinal,
    source_candidate_id = candidate_id,
    source_payload_sha256 = source_hash,
    detector_start_row = detector_start,
    detector_end_row = detector_end,
    start_isi = as.integer(detector_start - 1L),
    end_isi = as.integer(detector_end - 1L),
    candidate_layer = layer,
    candidate_class = stpd_candidate_lineage_candidate_column(
      candidates, "candidate_class", "character"
    ),
    final_label = final_label, strict_boundary_pass = strict_boundary,
    strict_structure_anchor = strict_structure_anchor,
    priority = priority, score = score,
    max_candidates = rep(max_candidates, n),
    deterministic_cap_rank = as.integer(deterministic_cap_rank),
    retained_by_cap = retained_by_cap,
    post_cap_ordinal = as.integer(post_cap_ordinal),
    cap_decision = ifelse(
      retained_by_cap, "retained", "truncated_by_cap"
    ),
    stringsAsFactors = FALSE, check.names = FALSE
  )
}

stpd_candidate_lineage_union_post_payload <- function(
    candidates, pre_payload, train) {
  if (!nrow(candidates)) {
    return(stpd_candidate_lineage_empty_hook_payload(
      "burst_union_post_cap_v1"
    ))
  }
  # The union helper restores original input order after ranking. Its route and
  # source ordinal are therefore determined by the selected pre-cap rows.
  selected <- which(pre_payload$retained_by_cap)
  expected_n <- length(selected)
  if (nrow(candidates) != expected_n) {
    stpd_candidate_lineage_abort(
      "collector_union_post_not_closed",
      "Union post-cap row count does not close to retained membership."
    )
  }
  observed_id <- stpd_candidate_lineage_candidate_column(
    candidates, "candidate_id", "character"
  )
  observed_start <- stpd_candidate_lineage_candidate_column(
    candidates, "start_isi", "integer"
  )
  observed_end <- stpd_candidate_lineage_candidate_column(
    candidates, "end_isi", "integer"
  )
  observed_hash <- vapply(
    seq_len(expected_n),
    function(i) stpd_candidate_lineage_candidate_source_hash(
      candidates[i, , drop = FALSE],
      pre_payload$source_route[selected[[i]]]
    ),
    character(1)
  )
  closed <- identical(observed_id, pre_payload$source_candidate_id[selected]) &&
    identical(observed_hash, pre_payload$source_payload_sha256[selected]) &&
    identical(observed_start, pre_payload$detector_start_row[selected]) &&
    identical(observed_end, pre_payload$detector_end_row[selected])
  if (!closed) {
    stpd_candidate_lineage_abort(
      "collector_union_post_not_closed",
      "Union post-cap candidates do not close to the ranked pre-cap set."
    )
  }
  data.frame(
    train = rep(as.character(train)[1L], expected_n),
    post_cap_ordinal = as.integer(seq_len(expected_n)),
    union_ordinal = pre_payload$union_ordinal[selected],
    source_route = pre_payload$source_route[selected],
    source_ordinal = pre_payload$source_ordinal[selected],
    source_candidate_id = observed_id,
    source_payload_sha256 = observed_hash,
    detector_start_row = observed_start,
    detector_end_row = observed_end,
    start_isi = as.integer(observed_start - 1L),
    end_isi = as.integer(observed_end - 1L),
    stringsAsFactors = FALSE, check.names = FALSE
  )
}

stpd_candidate_lineage_capture_burst_union <- function(
    train_collector, structure_first, threshold_centred, scientific_out,
    train, max_candidates) {
  if (is.null(train_collector)) return(invisible(NULL))
  if (is.null(structure_first)) structure_first <- data.frame()
  if (is.null(threshold_centred)) threshold_centred <- data.frame()
  max_candidates <- max(1L, as.integer(max_candidates)[1L])
  pre <- stpd_candidate_lineage_union_pre_payload(
    structure_first, threshold_centred, train, max_candidates
  )
  post <- stpd_candidate_lineage_union_post_payload(
    scientific_out, pre, train
  )
  stpd_candidate_lineage_collector_capture(
    train_collector, "burst_union_pre_cap_v1", pre
  )
  stpd_candidate_lineage_collector_capture(
    train_collector, "burst_union_post_cap_v1", post
  )
  structure_receipt <- train_collector$observations$
    burst_structure_first_scan_receipt_v1
  structure_status <- if (!is.null(structure_receipt) &&
      nrow(structure_receipt) == 1L &&
      isTRUE(structure_receipt$scan_exhausted[[1L]])) {
    "stage3_scan_complete"
  } else {
    "stage3_not_applicable_or_unavailable"
  }
  stage4_receipt <- train_collector$observations$
    burst_threshold_stage4_receipt_v1
  threshold_status <- if (!is.null(stage4_receipt) &&
      nrow(stage4_receipt) == 1L &&
      stage4_receipt$coverage_status[[1L]] %in% c(
        "stage4_search_complete", "stage4_incomplete_early_stop",
        "stage4_not_applicable_short_train"
      )) {
    stage4_receipt$coverage_status[[1L]]
  } else {
    "stage4_observation_unavailable"
  }
  truncated_n <- nrow(pre) - nrow(post)
  union_cap_status <- if (!nrow(pre)) {
    "not_applied_empty"
  } else if (truncated_n > 0L) {
    "applied_complete_over_observed_inputs"
  } else {
    "not_applied_within_limit"
  }
  receipt <- data.frame(
    train = as.character(train)[1L], burst_pipeline_id = "final",
    structure_input_n = as.integer(nrow(structure_first)),
    threshold_input_n = as.integer(nrow(threshold_centred)),
    observed_input_n = as.integer(nrow(pre)),
    retained_n = as.integer(nrow(post)),
    truncated_n = as.integer(truncated_n),
    max_candidates = max_candidates,
    ranking_exhausted = TRUE,
    rank_scope_status = "complete_over_observed_inputs",
    structure_upstream_status = structure_status,
    threshold_upstream_status = threshold_status,
    overall_universe_status = "candidate_universe_unavailable",
    union_cap_status = union_cap_status,
    route_order = "structure_first_then_threshold_centred",
    pre_cap_payload_sha256 = stpd_threshold_first_hash_domain(
      "stpd-burst-union-pre-cap-v1", pre
    ),
    post_cap_payload_sha256 = stpd_threshold_first_hash_domain(
      "stpd-burst-union-post-cap-v1", post
    ),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  stpd_candidate_lineage_collector_capture(
    train_collector, "burst_union_rank_receipt_v1", receipt
  )
  invisible(list(pre = pre, post = post, receipt = receipt))
}

stpd_candidate_lineage_empty_observation_index <- function() {
  data.frame(
    observation_version = character(), observation_id = character(),
    hook_id = character(), payload_sha256 = character(), record_n = integer(),
    capture_status = character(), stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

stpd_candidate_lineage_observation_payload_is_flat <- function(payload) {
  plain_atomic <- function(value) {
    is.atomic(value) && is.null(attributes(value))
  }
  column_is_stable <- function(column) {
    attrs <- names(attributes(column)) %||% character()
    if (!is.null(names(column))) return(FALSE)
    if (is.factor(column)) {
      return(all(attrs %in% c("levels", "class")) &&
        plain_atomic(attr(column, "levels", exact = TRUE)) &&
        plain_atomic(attr(column, "class", exact = TRUE)))
    }
    if (inherits(column, "Date")) {
      return(identical(attrs, "class") &&
        plain_atomic(attr(column, "class", exact = TRUE)))
    }
    if (inherits(column, "POSIXct")) {
      tzone <- attr(column, "tzone", exact = TRUE)
      return(all(attrs %in% c("class", "tzone")) &&
        plain_atomic(attr(column, "class", exact = TRUE)) &&
        (is.null(tzone) || plain_atomic(tzone)))
    }
    if (inherits(column, "difftime")) {
      return(all(attrs %in% c("class", "units")) &&
        plain_atomic(attr(column, "class", exact = TRUE)) &&
        plain_atomic(attr(column, "units", exact = TRUE)))
    }
    is.atomic(column) && !is.object(column) && !length(attrs)
  }
  payload_attributes <- attributes(payload)
  payload_attrs <- names(payload_attributes) %||% character()
  payload_names <- payload_attributes$names
  payload_rows <- payload_attributes$row.names
  payload_class <- payload_attributes$class
  is.data.frame(payload) && identical(class(payload), "data.frame") &&
    all(payload_attrs %in% c("names", "row.names", "class")) &&
    plain_atomic(payload_names) && identical(payload_names, names(payload)) &&
    (is.integer(payload_rows) || is.character(payload_rows)) &&
    is.null(attributes(payload_rows)) &&
    plain_atomic(payload_class) && identical(payload_class, "data.frame") &&
    !is.null(names(payload)) &&
    !anyNA(names(payload)) && !any(!nzchar(names(payload))) &&
    !anyDuplicated(names(payload)) &&
    all(vapply(payload, column_is_stable, logical(1)))
}

stpd_candidate_lineage_expected_structure_post <- function(pre) {
  expected <- stpd_candidate_lineage_empty_hook_payload(
    "burst_structure_first_post_cap_v1"
  )
  if (!nrow(pre)) return(expected)
  selected <- which(pre$retained_by_cap)
  if (!length(selected)) return(expected)
  selected <- selected[order(pre$post_cap_ordinal[selected])]
  data.frame(
    train = pre$train[selected],
    post_cap_ordinal = as.integer(seq_along(selected)),
    proposal_ordinal = pre$proposal_ordinal[selected],
    source_candidate_id = pre$source_candidate_id[selected],
    source_payload_sha256 = pre$source_payload_sha256[selected],
    detector_start_row = pre$detector_start_row[selected],
    detector_end_row = pre$detector_end_row[selected],
    start_isi = pre$start_isi[selected], end_isi = pre$end_isi[selected],
    stringsAsFactors = FALSE, check.names = FALSE
  )
}

stpd_candidate_lineage_expected_union_post <- function(pre) {
  expected <- stpd_candidate_lineage_empty_hook_payload(
    "burst_union_post_cap_v1"
  )
  if (!nrow(pre)) return(expected)
  selected <- which(pre$retained_by_cap)
  if (!length(selected)) return(expected)
  selected <- selected[order(pre$post_cap_ordinal[selected])]
  data.frame(
    train = pre$train[selected],
    post_cap_ordinal = as.integer(seq_along(selected)),
    union_ordinal = pre$union_ordinal[selected],
    source_route = pre$source_route[selected],
    source_ordinal = pre$source_ordinal[selected],
    source_candidate_id = pre$source_candidate_id[selected],
    source_payload_sha256 = pre$source_payload_sha256[selected],
    detector_start_row = pre$detector_start_row[selected],
    detector_end_row = pre$detector_end_row[selected],
    start_isi = pre$start_isi[selected], end_isi = pre$end_isi[selected],
    stringsAsFactors = FALSE, check.names = FALSE
  )
}

stpd_candidate_lineage_structure_pre_is_valid <- function(payload, train) {
  if (!nrow(payload)) return(TRUE)
  n <- nrow(payload)
  k <- unique(payload$max_candidates)
  required_complete <- !anyNA(payload[c(
    "train", "proposal_ordinal", "source_candidate_id",
    "source_payload_sha256", "detector_start_row", "detector_end_row",
    "start_isi", "end_isi", "candidate_layer", "candidate_class",
    "final_label", "gate_status", "action", "priority", "score",
    "strict_boundary_pass", "one_sided_boundary_pass",
    "possible_boundary_pass", "compactness_gate_active",
    "q90_compact_pass", "max_intra_strict_pass",
    "tolerated_tail_pass", "manual_negative_veto", "max_candidates",
    "deterministic_cap_rank", "retained_by_cap", "cap_decision"
  )])
  rank_order <- order(
    -payload$priority, -payload$score, payload$detector_start_row,
    payload$detector_end_row, payload$proposal_ordinal,
    method = "radix", na.last = TRUE
  )
  expected_rank <- integer(n)
  expected_rank[rank_order] <- seq_len(n)
  expected_retained <- expected_rank <= k[[1L]]
  retained <- which(expected_retained)
  scientific_order <- retained[order(
    payload$detector_start_row[retained],
    payload$detector_end_row[retained],
    -payload$priority[retained], -payload$score[retained]
  )]
  expected_post <- rep(NA_integer_, n)
  expected_post[scientific_order] <- seq_along(scientific_order)
  required_complete && length(k) == 1L && k[[1L]] >= 1L &&
    all(payload$train == train) &&
    identical(payload$proposal_ordinal, as.integer(seq_len(n))) &&
    all(nzchar(payload$source_candidate_id)) &&
    all(grepl("^[0-9a-f]{64}$", payload$source_payload_sha256)) &&
    all(payload$detector_start_row >= 2L) &&
    all(payload$detector_end_row >= payload$detector_start_row) &&
    identical(payload$detector_start_row, payload$start_isi + 1L) &&
    identical(payload$detector_end_row, payload$end_isi + 1L) &&
    all(payload$candidate_layer == "structure_first_burst_screen") &&
    identical(payload$deterministic_cap_rank, as.integer(expected_rank)) &&
    identical(payload$retained_by_cap, expected_retained) &&
    identical(payload$post_cap_ordinal, as.integer(expected_post)) &&
    identical(
      payload$cap_decision,
      ifelse(expected_retained, "retained", "truncated_by_cap")
    )
}

stpd_candidate_lineage_union_pre_is_valid <- function(
    payload, train, structure_pre, structure_post, stage4_candidates) {
  if (!nrow(payload)) {
    return(!nrow(structure_post) && !nrow(stage4_candidates))
  }
  n <- nrow(payload)
  k <- unique(payload$max_candidates)
  complete <- !anyNA(payload[c(
    "train", "union_ordinal", "source_route", "source_ordinal",
    "source_candidate_id", "source_payload_sha256",
    "detector_start_row", "detector_end_row", "start_isi", "end_isi",
    "candidate_layer", "candidate_class", "final_label",
    "strict_boundary_pass",
    "strict_structure_anchor", "priority", "score", "max_candidates",
    "deterministic_cap_rank", "retained_by_cap", "cap_decision"
  )])
  structure_n <- sum(payload$source_route == "structure_first")
  threshold_n <- sum(payload$source_route == "threshold_centred")
  expected_route <- c(
    rep("structure_first", structure_n),
    rep("threshold_centred", threshold_n)
  )
  expected_source_ordinal <- as.integer(c(
    seq_len(structure_n), seq_len(threshold_n)
  ))
  priority_rank <- payload$priority
  priority_rank[!is.finite(priority_rank)] <- -Inf
  score_rank <- payload$score
  score_rank[!is.finite(score_rank)] <- -Inf
  start_rank <- payload$detector_start_row
  start_rank[!is.finite(start_rank)] <- .Machine$integer.max
  end_rank <- payload$detector_end_row
  end_rank[!is.finite(end_rank)] <- .Machine$integer.max
  rank_order <- order(
    -as.integer(payload$strict_structure_anchor),
    -priority_rank, -score_rank, start_rank, end_rank,
    payload$candidate_layer, payload$source_candidate_id,
    payload$union_ordinal, method = "radix", na.last = TRUE
  )
  expected_rank <- integer(n)
  expected_rank[rank_order] <- seq_len(n)
  expected_retained <- expected_rank <= k[[1L]]
  selected <- which(expected_retained)
  expected_post <- rep(NA_integer_, n)
  expected_post[selected] <- seq_along(selected)
  structure_closed <- structure_n == nrow(structure_post)
  threshold_closed <- threshold_n == nrow(stage4_candidates)
  expected_anchor <- payload$candidate_layer ==
    "structure_first_burst_screen" & payload$strict_boundary_pass &
    payload$final_label %in% c("burst", "long_burst", "possible_burst")
  if (structure_closed && structure_n) {
    rows <- seq_len(structure_n)
    structure_closed <-
      identical(
        payload$source_candidate_id[rows],
        structure_post$source_candidate_id
      ) &&
      identical(
        payload$source_payload_sha256[rows],
        structure_post$source_payload_sha256
      ) &&
      identical(
        payload$detector_start_row[rows],
        structure_post$detector_start_row
      ) &&
      identical(
        payload$detector_end_row[rows],
        structure_post$detector_end_row
      )
    if (structure_closed) {
      proposal_rows <- match(
        structure_post$proposal_ordinal, structure_pre$proposal_ordinal
      )
      structure_closed <- !anyNA(proposal_rows)
      if (structure_closed) {
        structure_closed <- identical(
          payload$strict_boundary_pass[rows],
          structure_pre$strict_boundary_pass[proposal_rows]
        )
      }
    }
  }
  if (threshold_closed && threshold_n) {
    rows <- seq.int(structure_n + 1L, n)
    threshold_closed <-
      identical(
        payload$source_candidate_id[rows],
        stage4_candidates$candidate_id
      ) &&
      identical(
        payload$source_payload_sha256[rows],
        stage4_candidates$source_payload_sha256
      ) &&
      identical(
        payload$detector_start_row[rows],
        stage4_candidates$detector_start_row
      ) &&
      identical(
        payload$detector_end_row[rows],
        stage4_candidates$detector_end_row
      ) &&
      identical(
        payload$candidate_layer[rows],
        stage4_candidates$candidate_layer
      ) &&
      identical(
        payload$candidate_class[rows],
        stage4_candidates$candidate_class
      ) &&
      identical(payload$final_label[rows], stage4_candidates$final_label) &&
      identical(payload$priority[rows], stage4_candidates$priority) &&
      identical(payload$score[rows], stage4_candidates$score)
  }
  complete && length(k) == 1L && k[[1L]] >= 1L &&
    all(payload$train == train) &&
    identical(payload$union_ordinal, as.integer(seq_len(n))) &&
    all(payload$source_route %in% c(
      "structure_first", "threshold_centred"
    )) &&
    identical(payload$source_route, expected_route) &&
    identical(payload$source_ordinal, expected_source_ordinal) &&
    all(nzchar(payload$source_candidate_id)) &&
    all(grepl("^[0-9a-f]{64}$", payload$source_payload_sha256)) &&
    all(payload$detector_start_row >= 2L) &&
    all(payload$detector_end_row >= payload$detector_start_row) &&
    identical(payload$detector_start_row, payload$start_isi + 1L) &&
    identical(payload$detector_end_row, payload$end_isi + 1L) &&
    structure_closed && threshold_closed &&
    identical(payload$strict_structure_anchor, expected_anchor) &&
    identical(payload$deterministic_cap_rank, as.integer(expected_rank)) &&
    identical(payload$retained_by_cap, expected_retained) &&
    identical(payload$post_cap_ordinal, as.integer(expected_post)) &&
    identical(
      payload$cap_decision,
      ifelse(expected_retained, "retained", "truncated_by_cap")
    )
}

stpd_candidate_lineage_validate_hook_payload <- function(
    train_collector, hook_id, payload) {
  registry <- stpd_candidate_lineage_observation_hook_registry()
  hook_row <- match(hook_id, registry$hook_id)
  if (is.na(hook_row)) {
    stpd_candidate_lineage_abort(
      "collector_hook_unregistered",
      "Direct observations require a registered detector hook.",
      field = "hook_id"
    )
  }
  expected_pipeline <- registry$pipeline_id[[hook_row]]
  if (!identical(train_collector$pipeline_id, expected_pipeline)) {
    stpd_candidate_lineage_abort(
      "collector_hook_pipeline_mismatch",
      "Direct-observation hook does not belong to the entered pipeline."
    )
  }
  schema <- stpd_candidate_lineage_observation_hook_schema(hook_id)
  actual_types <- vapply(payload, typeof, character(1))
  if (is.null(schema) || !identical(names(payload), names(schema)) ||
      !identical(actual_types, schema) ||
      !all(vapply(payload, function(column) {
        is.null(attributes(column))
      }, logical(1)))) {
    stpd_candidate_lineage_abort(
      "collector_hook_payload_schema_invalid",
      "Direct-observation payload does not match its frozen hook schema."
    )
  }
  if (identical(hook_id, "hf_protected_impl_entry_v1")) {
    valid <- nrow(payload) == 1L &&
      identical(payload$train[[1L]], train_collector$train) &&
      !is.na(payload$n_interval_rows[[1L]]) &&
      payload$n_interval_rows[[1L]] >= 0L &&
      is.finite(payload$min_isi_sec[[1L]]) &&
      payload$min_isi_sec[[1L]] >= 0
    if (!valid) {
      stpd_candidate_lineage_abort(
        "collector_hook_payload_schema_invalid",
        "hf_protected entry observation does not match its frozen schema."
      )
    }
  } else if (hook_id %in% c(
      "pause_raw_entry_v1",
      "pause_raw_support_v1",
      "pause_raw_runs_v1",
      "pause_raw_output_v1",
      "pause_boundary_projection_sources_v1",
      "pause_boundary_projection_output_v1",
      "pause_raw_receipt_v1")) {
    outer_entry <-
      train_collector$observations$hf_protected_impl_entry_v1
    valid <- !is.null(outer_entry) && nrow(outer_entry) == 1L &&
      stpd_candidate_lineage_pause_raw_payload_is_valid(
        train_collector, hook_id, payload
      )
    if (!valid) {
      stpd_candidate_lineage_abort(
        "collector_hook_payload_schema_invalid",
        paste(
          "Raw canonical-Pause observation does not replay from the exact",
          "primary hf_protected callsite and scientific output."
        )
      )
    }
  } else if (hook_id %in% names(stpd_candidate_lineage_contextual_pause_hooks())) {
    valid <- stpd_candidate_lineage_contextual_pause_payload_is_valid(
      train_collector, hook_id, payload
    )
    if (!valid) {
      stpd_candidate_lineage_abort(
        "collector_hook_payload_schema_invalid",
        "Contextual Pause observation does not close to its locked live replay."
      )
    }
  } else if (hook_id %in% names(stpd_candidate_lineage_gap_final_hooks())) {
    valid <- stpd_candidate_lineage_gap_final_payload_is_valid(
      train_collector, hook_id, payload
    )
    if (!valid) {
      stpd_candidate_lineage_abort(
        "collector_hook_payload_schema_invalid",
        "Final Gap observation does not close to its locked live replay."
      )
    }
  } else if (hook_id %in% names(stpd_candidate_lineage_broad_hfs_hooks())) {
    valid <- stpd_candidate_lineage_broad_hfs_payload_is_valid(
      train_collector, hook_id, payload
    )
    if (!valid) {
      stpd_candidate_lineage_abort(
        "collector_hook_payload_schema_invalid",
        "Broad-HFS State observation does not close to its locked live replay."
      )
    }
  } else if (hook_id %in% names(stpd_candidate_lineage_tonic_state_hooks())) {
    valid <- stpd_candidate_lineage_tonic_state_payload_is_valid(
      train_collector, hook_id, payload
    )
    if (!valid) {
      stpd_candidate_lineage_abort(
        "collector_hook_payload_schema_invalid",
        "Tonic State observation does not close to its locked live replay."
      )
    }
  } else if (hook_id %in% names(stpd_candidate_lineage_nested_hfs_review_hooks())) {
    valid <- stpd_candidate_lineage_nested_hfs_review_payload_is_valid(
      train_collector, hook_id, payload
    )
    if (!valid) {
      stpd_candidate_lineage_abort(
        "collector_hook_payload_schema_invalid",
        "Nested-HFS Review observation does not close to its locked live replay."
      )
    }
  } else if (hook_id %in% names(
      stpd_candidate_lineage_complete_universe_release_hooks())) {
    valid <- stpd_candidate_lineage_complete_universe_release_payload_is_valid(
      train_collector, hook_id, payload
    )
    if (!valid) {
      stpd_candidate_lineage_abort(
        "collector_hook_payload_schema_invalid",
        "Complete candidate-universe release does not close to its locked live replay."
      )
    }
  } else if (identical(hook_id, "burst_dispatch_final_route_v1")) {
    outer_entry <- train_collector$observations$hf_protected_impl_entry_v1
    valid <- !is.null(outer_entry) && nrow(payload) == 1L &&
      identical(payload$train[[1L]], train_collector$train) &&
      identical(payload$burst_pipeline_id[[1L]], "final") &&
      !is.na(payload$max_candidates[[1L]]) &&
      payload$max_candidates[[1L]] >= 1L
    if (!valid) {
      stpd_candidate_lineage_abort(
        "collector_hook_payload_schema_invalid",
        "Burst dispatch route must bind directly to the selected final pipeline."
      )
    }
  } else if (identical(
      hook_id, "burst_structure_first_pre_cap_v1")) {
    dispatch <- train_collector$observations$burst_dispatch_final_route_v1
    valid <- !is.null(dispatch) &&
      stpd_candidate_lineage_structure_pre_is_valid(
        payload, train_collector$train
      )
    if (!valid) {
      stpd_candidate_lineage_abort(
        "collector_hook_payload_schema_invalid",
        "Structure-first pre-cap observation violates its frozen invariants."
      )
    }
  } else if (identical(
      hook_id, "burst_structure_first_post_cap_v1")) {
    pre <- train_collector$observations$burst_structure_first_pre_cap_v1
    expected <- if (is.null(pre)) NULL else
      stpd_candidate_lineage_expected_structure_post(pre)
    if (is.null(expected) || !identical(payload, expected)) {
      stpd_candidate_lineage_abort(
        "collector_hook_payload_schema_invalid",
        "Structure-first post-cap observation does not replay from pre-cap rank."
      )
    }
  } else if (identical(
      hook_id, "burst_structure_first_scan_receipt_v1")) {
    entry <- train_collector$observations$hf_protected_impl_entry_v1
    dispatch <- train_collector$observations$burst_dispatch_final_route_v1
    pre <- train_collector$observations$burst_structure_first_pre_cap_v1
    post <- train_collector$observations$burst_structure_first_post_cap_v1
    status_allowed <- c(
      "not_applicable_short_train", "not_applied_disabled",
      "not_applied_invalid_settings", "scan_exhausted_zero",
      "scan_exhausted_with_proposals"
    )
    pre_n <- if (is.null(pre)) -1L else nrow(pre)
    post_n <- if (is.null(post)) -1L else nrow(post)
    n_interval_rows <- payload$n_interval_rows[[1L]]
    enabled <- isTRUE(payload$structure_first_enabled[[1L]])
    settings_available <- !is.na(payload$min_isi_n[[1L]]) &&
      !is.na(payload$max_isi_n[[1L]])
    scan_settings_valid <- settings_available &&
      payload$min_isi_n[[1L]] >= 1L &&
      payload$max_isi_n[[1L]] >= payload$min_isi_n[[1L]]
    expected_window_n <- if (n_interval_rows > 2L && enabled &&
        scan_settings_valid) {
      as.integer(sum(vapply(
        seq.int(payload$min_isi_n[[1L]], payload$max_isi_n[[1L]]),
        function(w) max(0L, n_interval_rows - as.integer(w)),
        integer(1)
      )))
    } else {
      0L
    }
    expected_status <- if (n_interval_rows <= 2L) {
      "not_applicable_short_train"
    } else if (!enabled) {
      "not_applied_disabled"
    } else if (!scan_settings_valid) {
      "not_applied_invalid_settings"
    } else if (pre_n > 0L) {
      "scan_exhausted_with_proposals"
    } else {
      "scan_exhausted_zero"
    }
    expected_scan_exhausted <- expected_status %in% c(
      "scan_exhausted_zero", "scan_exhausted_with_proposals"
    )
    expected_cap_status <- if (pre_n < 0L || post_n < 0L) {
      "invalid_missing_dependency"
    } else if (!expected_scan_exhausted) {
      "not_applicable"
    } else if (!pre_n) {
      "not_applied_empty"
    } else if (pre_n > post_n) {
      "applied_complete"
    } else {
      "not_applied_within_limit"
    }
    counts_valid <- nrow(payload) == 1L && !anyNA(payload[c(
      "train", "burst_pipeline_id", "applicability_status",
      "n_interval_rows", "n_physical_isi", "min_isi_sec",
      "structure_first_enabled",
      "valid_isi_n", "expected_window_n", "visited_window_n",
      "valid_window_n", "pre_cap_proposal_n", "retained_n",
      "truncated_n", "max_candidates", "scan_exhausted",
      "structure_cap_status", "input_support_sha256",
      "pre_cap_payload_sha256", "post_cap_payload_sha256"
    )]) && payload$n_interval_rows[[1L]] >= 0L &&
      payload$n_physical_isi[[1L]] ==
        max(0L, payload$n_interval_rows[[1L]] - 1L) &&
      payload$valid_isi_n[[1L]] >= 0L &&
      payload$expected_window_n[[1L]] >= 0L &&
      payload$visited_window_n[[1L]] >= 0L &&
      payload$valid_window_n[[1L]] >= 0L &&
      payload$valid_window_n[[1L]] <= payload$visited_window_n[[1L]] &&
      payload$pre_cap_proposal_n[[1L]] == pre_n &&
      payload$retained_n[[1L]] == post_n &&
      payload$truncated_n[[1L]] == pre_n - post_n &&
      payload$max_candidates[[1L]] >= 1L
    dispatch_k <- if (is.null(dispatch) || !nrow(dispatch)) {
      NA_integer_
    } else {
      dispatch$max_candidates[[1L]]
    }
    rank_k_closed <- !is.na(dispatch_k) &&
      identical(payload$max_candidates[[1L]], dispatch_k) &&
      post_n == min(pre_n, dispatch_k) &&
      payload$truncated_n[[1L]] == max(0L, pre_n - dispatch_k)
    if (rank_k_closed && pre_n > 0L) {
      rank_k_closed <- length(unique(pre$max_candidates)) == 1L &&
        identical(pre$max_candidates[[1L]], dispatch_k)
    }
    raw <- train_collector$observations$burst_raw_threshold_support_v1
    input_hash_replays <- TRUE
    valid_support_replays <- TRUE
    valid_windows_replay <- TRUE
    if (!is.null(raw) && nrow(raw) &&
        payload$n_interval_rows[[1L]] > 2L) {
      replay_support <- data.frame(
        detector_row_index = raw$detector_row_index,
        isi_index = raw$isi_index, isi_sec = raw$isi_sec,
        artifact_isi = raw$artifact_isi, valid_isi = raw$valid_isi,
        stringsAsFactors = FALSE, check.names = FALSE
      )
      input_hash_replays <- identical(
        payload$input_support_sha256[[1L]],
        stpd_threshold_first_hash_domain(
          "stpd-structure-first-input-support-v1", replay_support
        )
      )
      if (expected_scan_exhausted) {
        valid_support <- rep(FALSE, n_interval_rows)
        valid_support[raw$detector_row_index] <- raw$valid_isi
        valid_support_replays <- identical(
          payload$valid_isi_n[[1L]], as.integer(sum(valid_support))
        )
        replay_valid_window_n <- as.integer(sum(vapply(
          seq.int(payload$min_isi_n[[1L]], payload$max_isi_n[[1L]]),
          function(w) {
            max_start <- n_interval_rows - as.integer(w) + 1L
            if (max_start < 2L) return(0L)
            as.integer(sum(vapply(
              seq.int(2L, max_start),
              function(s) all(valid_support[
                seq.int(s, s + as.integer(w) - 1L)
              ]), logical(1)
            )))
          }, integer(1)
        )))
        valid_windows_replay <- identical(
          payload$valid_window_n[[1L]], replay_valid_window_n
        )
      }
    }
    hash_valid <- input_hash_replays &&
      all(grepl("^[0-9a-f]{64}$", c(
      payload$input_support_sha256,
      payload$pre_cap_payload_sha256,
      payload$post_cap_payload_sha256
    ))) && identical(
      payload$pre_cap_payload_sha256[[1L]],
      stpd_threshold_first_hash_domain(
        "stpd-structure-first-pre-cap-v1", pre
      )
    ) && identical(
      payload$post_cap_payload_sha256[[1L]],
      stpd_threshold_first_hash_domain(
        "stpd-structure-first-post-cap-v1", post
      )
    )
    scan_valid <- pre_n >= 0L && post_n >= 0L &&
      identical(payload$applicability_status[[1L]], expected_status) &&
      identical(payload$scan_exhausted[[1L]], expected_scan_exhausted) &&
      identical(payload$expected_window_n[[1L]], expected_window_n) &&
      identical(
        payload$visited_window_n[[1L]],
        if (expected_scan_exhausted) expected_window_n else 0L
      ) &&
      (expected_scan_exhausted ||
        (payload$valid_window_n[[1L]] == 0L && !pre_n && !post_n)) &&
      valid_support_replays && valid_windows_replay
    valid <- !is.null(entry) && !is.null(dispatch) && !is.null(pre) &&
      !is.null(post) && nrow(payload) == 1L &&
      identical(payload$train[[1L]], train_collector$train) &&
      identical(payload$burst_pipeline_id[[1L]], "final") &&
      identical(payload$n_interval_rows[[1L]],
                entry$n_interval_rows[[1L]]) &&
      identical(payload$min_isi_sec[[1L]], entry$min_isi_sec[[1L]]) &&
      payload$applicability_status[[1L]] %in% status_allowed &&
      counts_valid && rank_k_closed && hash_valid && scan_valid &&
      identical(payload$structure_cap_status[[1L]], expected_cap_status)
    if (!valid) {
      stpd_candidate_lineage_abort(
        "collector_hook_payload_schema_invalid",
        "Structure-first scan receipt is incomplete or not rank-closed."
      )
    }
  } else if (identical(hook_id, "burst_detector_final_entry_v1")) {
    outer_entry <- train_collector$observations$hf_protected_impl_entry_v1
    valid <- !is.null(outer_entry) && nrow(payload) == 1L &&
      identical(payload$train[[1L]], train_collector$train) &&
      identical(payload$burst_pipeline_id[[1L]], "final")
    if (!valid) {
      stpd_candidate_lineage_abort(
        "collector_hook_payload_schema_invalid",
        paste(
          "Burst detector entry must be captured directly from the final",
          "inner pipeline after the hf_protected entry."
        )
      )
    }
  } else if (identical(hook_id, "burst_raw_threshold_support_v1")) {
    entry <- train_collector$observations$hf_protected_impl_entry_v1
    burst_entry <-
      train_collector$observations$burst_detector_final_entry_v1
    valid <- if (is.null(entry) || is.null(burst_entry)) FALSE else if (!nrow(payload)) {
      entry$n_interval_rows[[1L]] <= 2L
    } else {
      thresholds_finite <- all(is.finite(c(
        payload$min_valid_isi_sec, payload$seed_low_sec,
        payload$seed_high_sec, payload$native_bridge_high_sec
      )))
      logical_complete <- !anyNA(payload$artifact_isi) &&
        !anyNA(payload$valid_isi) && !anyNA(payload$seed_band_member) &&
        !anyNA(payload$native_bridge_extension_eligible)
      constant_contract <- length(unique(payload$min_valid_isi_sec)) == 1L &&
        length(unique(payload$seed_low_sec)) == 1L &&
        length(unique(payload$seed_high_sec)) == 1L &&
        length(unique(payload$native_bridge_high_sec)) == 1L &&
        length(unique(payload$min_seed_isi_count)) == 1L
      thresholds_ordered <- thresholds_finite &&
        all(payload$min_valid_isi_sec >= 0) &&
        all(payload$seed_low_sec <= payload$seed_high_sec) &&
        all(payload$seed_high_sec <= payload$native_bridge_high_sec) &&
        all(payload$min_seed_isi_count >= 1L)
      artifact_expected <- is_artifact_isi(
        payload$isi_sec, entry$min_isi_sec[[1L]]
      )
      valid_expected <- is.finite(payload$isi_sec) & !artifact_expected
      seed_expected <- payload$valid_isi & is.finite(payload$isi_sec) &
        payload$isi_sec >= payload$seed_low_sec &
        payload$isi_sec <= payload$seed_high_sec
      native_bridge_expected <- payload$valid_isi &
        is.finite(payload$isi_sec) &
        payload$isi_sec <= payload$native_bridge_high_sec
      all(payload$train == train_collector$train) &&
        nrow(payload) == entry$n_interval_rows[[1L]] - 1L &&
        identical(
          payload$detector_row_index,
          seq.int(2L, entry$n_interval_rows[[1L]])
        ) &&
        identical(payload$isi_index, seq_len(nrow(payload))) &&
        logical_complete && constant_contract && thresholds_ordered &&
        all(payload$min_valid_isi_sec == entry$min_isi_sec[[1L]]) &&
        identical(payload$artifact_isi, artifact_expected) &&
        identical(payload$valid_isi, valid_expected) &&
        identical(payload$seed_band_member, seed_expected) &&
        identical(
          payload$native_bridge_extension_eligible,
          native_bridge_expected
        )
    }
    if (!valid) {
      stpd_candidate_lineage_abort(
        "collector_hook_payload_schema_invalid",
        "Burst raw-threshold observation violates its frozen invariants."
      )
    }
  } else if (identical(hook_id, "burst_seed_runs_v1")) {
    raw <- train_collector$observations$burst_raw_threshold_support_v1
    expected <- stpd_candidate_lineage_empty_hook_payload(
      "burst_seed_runs_v1"
    )
    if (!is.null(raw) && nrow(raw)) {
      raw_runs <- stpd_event_grammar_bool_runs(raw$seed_band_member)
      if (nrow(raw_runs)) {
        counts <- as.integer(
          raw_runs$end_isi - raw_runs$start_isi + 1L
        )
        min_count <- as.integer(raw$min_seed_isi_count[[1L]])
        expected <- data.frame(
          train = rep(train_collector$train, nrow(raw_runs)),
          run_ordinal = seq_len(nrow(raw_runs)),
          detector_start_row = as.integer(raw_runs$start_isi + 1L),
          detector_end_row = as.integer(raw_runs$end_isi + 1L),
          start_isi = as.integer(raw_runs$start_isi),
          end_isi = as.integer(raw_runs$end_isi),
          seed_isi_count = counts,
          min_seed_isi_count = rep(min_count, nrow(raw_runs)),
          min_seed_pass = counts >= min_count,
          stringsAsFactors = FALSE, check.names = FALSE
        )
      }
    }
    valid <- !is.null(raw) && identical(payload, expected)
    if (!valid) {
      stpd_candidate_lineage_abort(
        "collector_hook_payload_schema_invalid",
        "Burst seed-run observation violates its frozen invariants."
      )
    }
  } else if (identical(
      hook_id, "burst_threshold_stage4_entry_v1")) {
    outer_entry <- train_collector$observations$hf_protected_impl_entry_v1
    dispatch <- train_collector$observations$burst_dispatch_final_route_v1
    context <- if (exists(
        "stage4_replay_context", envir = train_collector,
        inherits = FALSE)) {
      train_collector$stage4_replay_context
    } else NULL
    valid <- if (is.null(outer_entry)) {
      FALSE
    } else if (!nrow(payload)) {
      outer_entry$n_interval_rows[[1L]] <= 2L
    } else if (is.null(context)) {
      FALSE
    } else {
      expected <- stpd_candidate_lineage_stage4_entry_from_context(context)
      identical(payload, expected) &&
        identical(
          payload$n_interval_rows[[1L]],
          outer_entry$n_interval_rows[[1L]]
        ) &&
        identical(payload$min_isi_sec[[1L]],
                  outer_entry$min_isi_sec[[1L]]) &&
        (is.null(dispatch) || identical(
          payload$max_candidates[[1L]],
          dispatch$max_candidates[[1L]]
        ))
    }
    if (!valid) {
      stpd_candidate_lineage_abort(
        "collector_hook_payload_schema_invalid",
        "Burst Stage-4 entry does not close to its live detector context."
      )
    }
  } else if (identical(
      hook_id, "burst_threshold_stage4_episode_roots_v1")) {
    raw <- train_collector$observations$burst_raw_threshold_support_v1
    valid <- !is.null(raw) &&
      stpd_candidate_lineage_stage4_episode_roots_are_valid(
        payload, train_collector$train, raw
      )
    if (!valid) {
      stpd_candidate_lineage_abort(
        "collector_hook_payload_schema_invalid",
        "Burst Stage-4 episode roots violate their observed support."
      )
    }
  } else if (identical(
      hook_id, "burst_threshold_stage4_attempts_v1")) {
    dispatch <- train_collector$observations$burst_dispatch_final_route_v1
    raw <- train_collector$observations$burst_raw_threshold_support_v1
    seed_runs <- train_collector$observations$burst_seed_runs_v1
    episode_roots <- train_collector$observations$
      burst_threshold_stage4_episode_roots_v1
    stage4_entry <- train_collector$observations$
      burst_threshold_stage4_entry_v1
    context <- if (exists(
        "stage4_replay_context", envir = train_collector,
        inherits = FALSE)) {
      train_collector$stage4_replay_context
    } else NULL
    scientific_out <- if (exists(
        "stage4_scientific_out", envir = train_collector,
        inherits = FALSE)) {
      train_collector$stage4_scientific_out
    } else NULL
    valid <- !is.null(raw) && !is.null(seed_runs) &&
      !is.null(episode_roots) && !is.null(stage4_entry) &&
      stpd_candidate_lineage_stage4_attempts_are_valid(
        payload, train_collector$train, dispatch, raw, seed_runs,
        episode_roots, stage4_entry, context, scientific_out
      )
    if (!valid) {
      stpd_candidate_lineage_abort(
        "collector_hook_payload_schema_invalid",
        "Burst Stage-4 attempts violate ordered source/geometry closure."
      )
    }
  } else if (identical(
      hook_id, "burst_threshold_stage4_candidates_v1")) {
    attempts <- train_collector$observations$
      burst_threshold_stage4_attempts_v1
    scientific_out <- if (exists(
        "stage4_scientific_out", envir = train_collector,
        inherits = FALSE)) {
      train_collector$stage4_scientific_out
    } else NULL
    valid <- !is.null(attempts) &&
      stpd_candidate_lineage_stage4_candidates_are_valid(
        payload, train_collector$train, attempts, scientific_out
      )
    if (!valid) {
      stpd_candidate_lineage_abort(
        "collector_hook_payload_schema_invalid",
        "Burst Stage-4 candidates do not close to emitted attempts."
      )
    }
  } else if (identical(
      hook_id, "burst_threshold_stage4_receipt_v1")) {
    entry <- train_collector$observations$hf_protected_impl_entry_v1
    dispatch <- train_collector$observations$burst_dispatch_final_route_v1
    raw <- train_collector$observations$burst_raw_threshold_support_v1
    seed_runs <- train_collector$observations$burst_seed_runs_v1
    episode_roots <- train_collector$observations$
      burst_threshold_stage4_episode_roots_v1
    attempts <- train_collector$observations$
      burst_threshold_stage4_attempts_v1
    candidates <- train_collector$observations$
      burst_threshold_stage4_candidates_v1
    stage4_entry <- train_collector$observations$
      burst_threshold_stage4_entry_v1
    context <- if (exists(
        "stage4_replay_context", envir = train_collector,
        inherits = FALSE)) {
      train_collector$stage4_replay_context
    } else NULL
    scientific_out <- if (exists(
        "stage4_scientific_out", envir = train_collector,
        inherits = FALSE)) {
      train_collector$stage4_scientific_out
    } else NULL
    valid <- stpd_candidate_lineage_stage4_receipt_is_valid(
      payload, train_collector$train, entry, dispatch, raw, seed_runs,
      episode_roots, attempts, candidates, stage4_entry, context,
      scientific_out
    )
    if (!valid) {
      stpd_candidate_lineage_abort(
        "collector_hook_payload_schema_invalid",
        "Burst Stage-4 receipt is not count/hash/coverage closed."
      )
    }
  } else if (identical(hook_id, "burst_union_pre_cap_v1")) {
    dispatch <- train_collector$observations$burst_dispatch_final_route_v1
    structure_pre <-
      train_collector$observations$burst_structure_first_pre_cap_v1
    structure_post <-
      train_collector$observations$burst_structure_first_post_cap_v1
    final_entry <-
      train_collector$observations$burst_detector_final_entry_v1
    seed_runs <- train_collector$observations$burst_seed_runs_v1
    stage4_candidates <- train_collector$observations$
      burst_threshold_stage4_candidates_v1
    stage4_receipt <- train_collector$observations$
      burst_threshold_stage4_receipt_v1
    valid <- !is.null(dispatch) && !is.null(structure_pre) &&
      !is.null(structure_post) && !is.null(final_entry) &&
      !is.null(seed_runs) && !is.null(stage4_candidates) &&
      !is.null(stage4_receipt) &&
      stpd_candidate_lineage_union_pre_is_valid(
        payload, train_collector$train, structure_pre, structure_post,
        stage4_candidates
      )
    if (!valid) {
      stpd_candidate_lineage_abort(
        "collector_hook_payload_schema_invalid",
        "Burst union pre-cap observation violates its frozen invariants."
      )
    }
  } else if (identical(hook_id, "burst_union_post_cap_v1")) {
    pre <- train_collector$observations$burst_union_pre_cap_v1
    expected <- if (is.null(pre)) NULL else
      stpd_candidate_lineage_expected_union_post(pre)
    if (is.null(expected) || !identical(payload, expected)) {
      stpd_candidate_lineage_abort(
        "collector_hook_payload_schema_invalid",
        "Burst union post-cap observation does not replay from pre-cap rank."
      )
    }
  } else if (identical(hook_id, "burst_union_rank_receipt_v1")) {
    pre <- train_collector$observations$burst_union_pre_cap_v1
    post <- train_collector$observations$burst_union_post_cap_v1
    dispatch <- train_collector$observations$burst_dispatch_final_route_v1
    structure_post <-
      train_collector$observations$burst_structure_first_post_cap_v1
    stage4_receipt <- train_collector$observations$
      burst_threshold_stage4_receipt_v1
    expected_cap_status <- if (is.null(pre) || !nrow(pre)) {
      "not_applied_empty"
    } else if (!is.null(post) && nrow(pre) > nrow(post)) {
      "applied_complete_over_observed_inputs"
    } else {
      "not_applied_within_limit"
    }
    hashes_valid <- !is.null(pre) && !is.null(post) &&
      all(grepl("^[0-9a-f]{64}$", c(
        payload$pre_cap_payload_sha256,
        payload$post_cap_payload_sha256
      ))) && identical(
        payload$pre_cap_payload_sha256[[1L]],
        stpd_threshold_first_hash_domain(
          "stpd-burst-union-pre-cap-v1", pre
        )
      ) && identical(
        payload$post_cap_payload_sha256[[1L]],
        stpd_threshold_first_hash_domain(
          "stpd-burst-union-post-cap-v1", post
        )
      )
    dispatch_k <- if (is.null(dispatch) || !nrow(dispatch)) {
      NA_integer_
    } else {
      dispatch$max_candidates[[1L]]
    }
    rank_k_closed <- !is.na(dispatch_k) &&
      identical(payload$max_candidates[[1L]], dispatch_k) &&
      payload$retained_n[[1L]] == min(nrow(pre), dispatch_k) &&
      payload$truncated_n[[1L]] == max(0L, nrow(pre) - dispatch_k)
    if (rank_k_closed && nrow(pre)) {
      rank_k_closed <- length(unique(pre$max_candidates)) == 1L &&
        identical(pre$max_candidates[[1L]], dispatch_k)
    }
    expected_threshold_status <- if (is.null(stage4_receipt) ||
        nrow(stage4_receipt) != 1L) {
      "invalid_stage4_receipt"
    } else {
      stage4_receipt$coverage_status[[1L]]
    }
    valid <- !is.null(pre) && !is.null(post) && !is.null(dispatch) &&
      !is.null(structure_post) && !is.null(stage4_receipt) &&
      nrow(payload) == 1L &&
      !anyNA(payload) &&
      identical(payload$train[[1L]], train_collector$train) &&
      identical(payload$burst_pipeline_id[[1L]], "final") &&
      payload$structure_input_n[[1L]] == nrow(structure_post) &&
      payload$threshold_input_n[[1L]] ==
        sum(pre$source_route == "threshold_centred") &&
      payload$observed_input_n[[1L]] == nrow(pre) &&
      payload$retained_n[[1L]] == nrow(post) &&
      payload$truncated_n[[1L]] == nrow(pre) - nrow(post) &&
      payload$max_candidates[[1L]] >= 1L && rank_k_closed &&
      isTRUE(payload$ranking_exhausted[[1L]]) &&
      identical(payload$rank_scope_status[[1L]],
                "complete_over_observed_inputs") &&
      payload$structure_upstream_status[[1L]] %in% c(
        "stage3_scan_complete",
        "stage3_not_applicable_or_unavailable"
      ) && expected_threshold_status %in% c(
        "stage4_search_complete", "stage4_incomplete_early_stop",
        "stage4_not_applicable_short_train"
      ) && identical(payload$threshold_upstream_status[[1L]],
                    expected_threshold_status) &&
      identical(payload$overall_universe_status[[1L]],
                "candidate_universe_unavailable") &&
      identical(payload$union_cap_status[[1L]], expected_cap_status) &&
      identical(payload$route_order[[1L]],
                "structure_first_then_threshold_centred") && hashes_valid
    if (!valid) {
      stpd_candidate_lineage_abort(
        "collector_hook_payload_schema_invalid",
        paste(
          "Burst union receipt must remain complete only over observed",
          "inputs and unavailable for the intended candidate universe."
        )
      )
    }
  } else if (hook_id %in% c(
      "burst_refractory_entry_v1",
      "burst_refractory_attempts_v1",
      "burst_refractory_output_v1",
      "burst_refractory_receipt_v1")) {
    union_post <- train_collector$observations$burst_union_post_cap_v1
    union_receipt <- train_collector$observations$burst_union_rank_receipt_v1
    valid <- !is.null(union_post) && !is.null(union_receipt) &&
      nrow(union_receipt) == 1L &&
      stpd_candidate_lineage_refractory_payload_is_valid(
        train_collector, hook_id, payload
      )
    if (!valid) {
      stpd_candidate_lineage_abort(
        "collector_hook_payload_schema_invalid",
        paste(
          "Burst refractory observation does not independently replay",
          "from the exact post-union scientific candidates."
        )
      )
    }
  } else if (hook_id %in% c(
      "burst_pause_boundary_entry_v1",
      "burst_pause_boundary_sources_v1",
      "burst_pause_boundary_attempts_v1",
      "burst_pause_boundary_output_v1",
      "burst_pause_boundary_receipt_v1")) {
    refractory_output <-
      train_collector$observations$burst_refractory_output_v1
    refractory_receipt <-
      train_collector$observations$burst_refractory_receipt_v1
    contextual_receipt <-
      train_collector$observations$contextual_pause_receipt_v1
    valid <- !is.null(refractory_output) &&
      !is.null(refractory_receipt) &&
      !is.null(contextual_receipt) &&
      stpd_candidate_lineage_pause_boundary_payload_is_valid(
        train_collector, hook_id, payload
      )
    if (!valid) {
      stpd_candidate_lineage_abort(
        "collector_hook_payload_schema_invalid",
        paste(
          "Canonical Pause hard-boundary observation does not replay",
          paste0(
            "from the exact post-refractory Burst output and closed ",
            "post-ownership contextual Pause."
          )
        )
      )
    }
  } else if (hook_id %in% c(
      "burst_pause_ownership_entry_v1",
      "burst_pause_ownership_eligibility_v1",
      "burst_pause_ownership_selection_v1",
      "burst_pause_ownership_attempts_v1",
      "burst_pause_ownership_output_v1",
      "burst_pause_ownership_receipt_v1")) {
    valid <- stpd_candidate_lineage_pause_ownership_payload_is_valid(
      train_collector, hook_id, payload
    )
    if (!valid) {
      stpd_candidate_lineage_abort(
        "collector_hook_payload_schema_invalid",
        paste(
          "Burst--Pause ownership observation does not replay from",
          "the exact ordinary post-refractory Burst output."
        )
      )
    }
  } else if (hook_id %in% c(
      "burst_hard_threshold_entry_v1",
      "burst_hard_threshold_support_v1",
      "burst_hard_threshold_runs_v1",
      "burst_hard_threshold_pre_refractory_candidates_v1",
      "burst_hard_threshold_refractory_attempts_v1",
      "burst_hard_threshold_pre_boundary_output_v1",
      "burst_hard_threshold_boundary_sources_v1",
      "burst_hard_threshold_boundary_attempts_v1",
      "burst_hard_threshold_post_boundary_output_v1",
      "burst_hard_threshold_receipt_v1")) {
    entry <- train_collector$observations$hf_protected_impl_entry_v1
    valid <- !is.null(entry) && nrow(entry) == 1L &&
      stpd_candidate_lineage_hard_threshold_payload_is_valid(
        train_collector, hook_id, payload
      )
    if (!valid) {
      stpd_candidate_lineage_abort(
        "collector_hook_payload_schema_invalid",
        paste(
          "Hard-threshold Burst-root observation does not replay from",
          "the exact primary hf_protected callsite and science context."
        )
      )
    }
  }
  invisible(TRUE)
}

stpd_candidate_lineage_validate_observation_storage <- function(
    train_collector, validate_payload_semantics = TRUE) {
  index <- train_collector$observation_index
  payloads <- train_collector$observations
  expected_types <- c(
    observation_version = "character", observation_id = "character",
    hook_id = "character", payload_sha256 = "character",
    record_n = "integer", capture_status = "character"
  )
  actual_types <- vapply(index, typeof, character(1))
  payload_names <- names(payloads) %||% character()
  invalid_index <- !identical(actual_types, expected_types) ||
    anyNA(index) || anyDuplicated(index$hook_id) ||
    any(index$observation_version !=
          STPD_CANDIDATE_LINEAGE_OBSERVATION_VERSION) ||
    any(!grepl("^[0-9a-f]{64}$", index$observation_id)) ||
    any(!grepl("^[a-z][a-z0-9_]*_v[0-9]+$", index$hook_id)) ||
    any(!grepl("^[0-9a-f]{64}$", index$payload_sha256)) ||
    any(index$record_n < 0L) ||
    any(index$capture_status != "direct_partial") ||
    !identical(payload_names, index$hook_id)
  if (invalid_index) {
    stpd_candidate_lineage_abort(
      "collector_observation_index_invalid",
      "Direct-observation index metadata is invalid or inconsistent."
    )
  }
  if (!nrow(index)) {
    if (!identical(train_collector$instrumentation_status,
                   "pipeline_not_instrumented")) {
      stpd_candidate_lineage_abort(
        "collector_observation_status_invalid",
        "A shard without direct observations cannot claim instrumentation."
      )
    }
    return(invisible(TRUE))
  }
  if (!isTRUE(train_collector$pipeline_entered) ||
      !(train_collector$instrumentation_status %in%
          c("direct_partial", "direct_complete"))) {
    stpd_candidate_lineage_abort(
      "collector_observation_status_invalid",
      "Direct observations require an entered, instrumented pipeline."
    )
  }
  if (identical(train_collector$instrumentation_status, "direct_complete")) {
    expected_hooks <- stpd_candidate_lineage_observation_hook_registry()$hook_id
    if (!identical(index$hook_id, expected_hooks) ||
        !identical(payload_names, expected_hooks)) {
      stpd_candidate_lineage_abort(
        "collector_observation_status_invalid",
        paste(
          "Direct-complete status requires every frozen observation hook",
          "exactly once and in registry order."
        )
      )
    }
  }
  for (i in seq_len(nrow(index))) {
    hook_id <- index$hook_id[[i]]
    payload <- payloads[[hook_id]]
    if (!stpd_candidate_lineage_observation_payload_is_flat(payload) ||
        !identical(as.integer(nrow(payload)), index$record_n[[i]])) {
      stpd_candidate_lineage_abort(
        "collector_observation_payload_invalid",
        "A direct-observation payload is not a valid flat typed table."
      )
    }
    if (isTRUE(validate_payload_semantics)) {
      stpd_candidate_lineage_validate_hook_payload(
        train_collector, hook_id, payload
      )
    }
    payload_hash <- stpd_threshold_first_hash_domain(
      "stpd-candidate-lineage-observation-payload-v1", payload
    )
    observation_id <- stpd_threshold_first_hash_domain(
      "stpd-candidate-lineage-observation-id-v1",
      list(
        run_id = train_collector$run_id,
        params_hash = train_collector$params_hash,
        dataset_id = train_collector$dataset_id,
        train = train_collector$train,
        pipeline_id = train_collector$pipeline_id,
        hook_id = hook_id,
        payload_sha256 = payload_hash
      )
    )
    if (!identical(payload_hash, index$payload_sha256[[i]]) ||
        !identical(observation_id, index$observation_id[[i]])) {
      stpd_candidate_lineage_abort(
        "collector_observation_hash_mismatch",
        "Direct-observation payload or binding hash does not replay."
      )
    }
  }
  invisible(TRUE)
}

stpd_candidate_lineage_resolve_audit_level <- function(
    audit_level = NULL, params = NULL) {
  if (!is.null(params)) {
    if (!is.list(params) || is.null(names(params)) || anyNA(names(params)) ||
        any(!nzchar(names(params))) || anyDuplicated(names(params))) {
      stpd_candidate_lineage_abort(
        "audit_config_invalid",
        "params must be a uniquely named list when resolving lineage audit level."
      )
    }
    configured <- stpd_threshold_first_exact_legacy_get(
      params, "threshold_first", "params"
    )
    if (!is.null(configured)) {
      stpd_candidate_lineage_abort(
        "run_config_not_activated",
        paste(
          "params$threshold_first is a frozen dormant contract and cannot be",
          "partially activated by Gate 1B collector plumbing; use the independent",
          "runtime audit_level argument."
        ),
        field = "params$threshold_first"
      )
    }
  }

  if (!is.null(audit_level)) {
    return(stpd_threshold_first_choice(
      audit_level, STPD_CANDIDATE_LINEAGE_AUDIT_LEVELS, "audit_level"
    ))
  }

  # Do not reuse the legacy collect_diagnostics migration here. The old
  # diagnostic switch and the new candidate-lineage collector are orthogonal;
  # default legacy detector calls must remain collector-off.
  "off"
}

stpd_candidate_lineage_is_collector <- function(x) {
  is.environment(x) && inherits(x, "stpd_candidate_lineage_collector")
}

stpd_candidate_lineage_is_train_collector <- function(x) {
  is.environment(x) && inherits(x, "stpd_candidate_lineage_train_collector")
}

stpd_candidate_lineage_assert_text <- function(x, field, allow_empty = FALSE) {
  if (!is.character(x) || length(x) != 1L || is.na(x) ||
      (!allow_empty && !nzchar(x))) {
    stpd_candidate_lineage_abort(
      "collector_field_invalid",
      paste0(field, " must be one ", if (allow_empty) "character" else
        "non-empty character", " value."),
      field = field
    )
  }
  x
}

stpd_candidate_lineage_validate_target_trains <- function(target_trains) {
  if (!is.character(target_trains) || anyNA(target_trains) ||
      any(!nzchar(target_trains)) || anyDuplicated(target_trains)) {
    stpd_candidate_lineage_abort(
      "collector_train_scope_invalid",
      "target_trains must contain unique, non-missing, non-empty train IDs.",
      field = "target_trains"
    )
  }
  target_trains
}

stpd_candidate_lineage_collector_new <- function(audit_level = "off") {
  audit_level <- stpd_candidate_lineage_resolve_audit_level(audit_level)
  collector <- new.env(parent = emptyenv())
  collector$collector_version <- STPD_CANDIDATE_LINEAGE_COLLECTOR_VERSION
  collector$requested_audit_level <- audit_level
  collector$state <- "open"
  collector$bound <- FALSE
  collector$run_id <- NA_character_
  collector$params_hash <- NA_character_
  collector$dataset_id <- NA_character_
  collector$target_trains <- character()
  collector$train_collectors <- list()
  collector$instrumentation_status <- "pipeline_not_instrumented"
  class(collector) <- c(
    "stpd_candidate_lineage_collector", "environment"
  )
  collector
}

stpd_candidate_lineage_validate_collector <- function(
    collector, require_state = NULL, require_bound = NULL) {
  if (!stpd_candidate_lineage_is_collector(collector)) {
    stpd_candidate_lineage_abort(
      "collector_invalid", "collector is not a candidate-lineage collector."
    )
  }
  required_fields <- c(
    "collector_version", "requested_audit_level", "state", "bound",
    "run_id", "params_hash", "dataset_id", "target_trains",
    "train_collectors", "instrumentation_status"
  )
  if (!all(vapply(required_fields, exists, logical(1), envir = collector,
                  inherits = FALSE))) {
    stpd_candidate_lineage_abort(
      "collector_invalid", "collector is missing required lifecycle fields."
    )
  }
  if (!identical(
    collector$collector_version, STPD_CANDIDATE_LINEAGE_COLLECTOR_VERSION
  ) || !(collector$requested_audit_level %in%
         STPD_CANDIDATE_LINEAGE_AUDIT_LEVELS) ||
      !(collector$state %in% STPD_CANDIDATE_LINEAGE_COLLECTOR_STATES) ||
      !is.logical(collector$bound) || length(collector$bound) != 1L ||
      is.na(collector$bound) || !is.list(collector$train_collectors) ||
      !(collector$instrumentation_status %in%
          STPD_CANDIDATE_LINEAGE_COLLECTOR_INSTRUMENTATION_STATUSES)) {
    stpd_candidate_lineage_abort(
      "collector_invalid", "collector lifecycle metadata is invalid."
    )
  }
  if (!is.null(require_state) && !identical(collector$state, require_state)) {
    stpd_candidate_lineage_abort(
      "collector_state_invalid",
      paste0("collector state must be '", require_state, "'.")
    )
  }
  if (!is.null(require_bound) && !identical(collector$bound, require_bound)) {
    stpd_candidate_lineage_abort(
      "collector_binding_invalid",
      paste0("collector bound state must be ", require_bound, ".")
    )
  }
  invisible(TRUE)
}

stpd_candidate_lineage_collector_bind_run <- function(
    collector, run_id, params_hash, dataset_id, target_trains) {
  stpd_candidate_lineage_validate_collector(
    collector, require_state = "open"
  )
  run_id <- stpd_candidate_lineage_assert_text(run_id, "run_id")
  params_hash <- stpd_candidate_lineage_assert_text(
    params_hash, "params_hash"
  )
  dataset_id <- stpd_candidate_lineage_assert_text(dataset_id, "dataset_id")
  target_trains <- stpd_candidate_lineage_validate_target_trains(target_trains)
  if (isTRUE(collector$bound)) {
    same <- identical(collector$run_id, run_id) &&
      identical(collector$params_hash, params_hash) &&
      identical(collector$dataset_id, dataset_id) &&
      identical(collector$target_trains, target_trains)
    if (!same) {
      stpd_candidate_lineage_abort(
        "collector_rebind_forbidden",
        "A bound collector cannot be rebound to another run or train scope."
      )
    }
    return(invisible(collector))
  }
  collector$run_id <- run_id
  collector$params_hash <- params_hash
  collector$dataset_id <- dataset_id
  collector$target_trains <- target_trains
  collector$bound <- TRUE
  invisible(collector)
}

stpd_candidate_lineage_collector_begin_train <- function(collector, train) {
  if (is.null(collector)) return(NULL)
  stpd_candidate_lineage_validate_collector(
    collector, require_state = "open", require_bound = TRUE
  )
  if (identical(collector$requested_audit_level, "off")) return(NULL)
  train <- stpd_candidate_lineage_assert_text(train, "train")
  if (!(train %in% collector$target_trains)) {
    stpd_candidate_lineage_abort(
      "collector_train_scope_invalid",
      paste0("Train '", train, "' is outside the collector run scope."),
      field = "train"
    )
  }
  if (!is.null(collector$train_collectors[[train]])) {
    stpd_candidate_lineage_abort(
      "collector_train_duplicate",
      paste0("Train '", train, "' already has a collector shard."),
      field = "train"
    )
  }
  shard <- new.env(parent = emptyenv())
  shard$collector_version <- STPD_CANDIDATE_LINEAGE_COLLECTOR_VERSION
  shard$run_id <- collector$run_id
  shard$params_hash <- collector$params_hash
  shard$dataset_id <- collector$dataset_id
  shard$train <- train
  shard$requested_audit_level <- collector$requested_audit_level
  shard$state <- "open"
  shard$pipeline_id <- NA_character_
  shard$pipeline_entered <- FALSE
  shard$instrumentation_status <- "pipeline_not_instrumented"
  shard$observation_index <- stpd_candidate_lineage_empty_observation_index()
  shard$observations <- list()
  class(shard) <- c(
    "stpd_candidate_lineage_train_collector", "environment"
  )
  collector$train_collectors[[train]] <- shard
  shard
}

stpd_candidate_lineage_validate_train_collector <- function(
    train_collector, require_state = NULL, train = NULL,
    validate_observations = TRUE, validate_observation_semantics = TRUE) {
  if (!stpd_candidate_lineage_is_train_collector(train_collector)) {
    stpd_candidate_lineage_abort(
      "train_collector_invalid",
      "train_collector is not a candidate-lineage train shard."
    )
  }
  required <- c(
    "collector_version", "run_id", "params_hash", "dataset_id", "train",
    "requested_audit_level", "state", "pipeline_id", "pipeline_entered",
    "instrumentation_status", "observation_index", "observations"
  )
  if (!all(vapply(required, exists, logical(1), envir = train_collector,
                  inherits = FALSE)) ||
      !identical(train_collector$collector_version,
                 STPD_CANDIDATE_LINEAGE_COLLECTOR_VERSION) ||
      !(train_collector$requested_audit_level %in%
        STPD_CANDIDATE_LINEAGE_AUDIT_LEVELS) ||
      !(train_collector$state %in% STPD_CANDIDATE_LINEAGE_TRAIN_STATES) ||
      !(train_collector$instrumentation_status %in%
          STPD_CANDIDATE_LINEAGE_COLLECTOR_INSTRUMENTATION_STATUSES) ||
      !is.logical(train_collector$pipeline_entered) ||
      length(train_collector$pipeline_entered) != 1L ||
      is.na(train_collector$pipeline_entered) ||
      !is.data.frame(train_collector$observation_index) ||
      !identical(names(train_collector$observation_index),
                 names(stpd_candidate_lineage_empty_observation_index())) ||
      !is.list(train_collector$observations)) {
    stpd_candidate_lineage_abort(
      "train_collector_invalid", "train collector metadata is invalid."
    )
  }
  # Full observation replay is intentionally reserved for snapshots, final
  # closure, and explicit validation. During sequential capture, the new hook
  # is validated against its frozen schema and upstream observations below;
  # replaying every previously validated hook before every append makes the
  # collector quadratic and caused tiny-train regression tests to take hours.
  if (isTRUE(validate_observations)) {
    stpd_candidate_lineage_validate_observation_storage(
      train_collector,
      validate_payload_semantics = validate_observation_semantics
    )
  }
  if (!is.null(require_state) &&
      !identical(train_collector$state, require_state)) {
    stpd_candidate_lineage_abort(
      "train_collector_state_invalid",
      paste0("train collector state must be '", require_state, "'.")
    )
  }
  if (!is.null(train) && !identical(train_collector$train, train)) {
    stpd_candidate_lineage_abort(
      "collector_train_scope_invalid",
      "train collector does not match the active train.", field = "train"
    )
  }
  invisible(TRUE)
}

stpd_candidate_lineage_collector_note_pipeline <- function(
    train_collector, pipeline_id, train = NULL) {
  if (is.null(train_collector)) return(invisible(NULL))
  stpd_candidate_lineage_validate_train_collector(
    train_collector, require_state = "open", train = train
  )
  pipeline_id <- stpd_candidate_lineage_assert_text(
    pipeline_id, "pipeline_id"
  )
  if (isTRUE(train_collector$pipeline_entered) &&
      !identical(train_collector$pipeline_id, pipeline_id)) {
    stpd_candidate_lineage_abort(
      "collector_pipeline_conflict",
      "A train collector cannot enter two detector pipelines."
    )
  }
  train_collector$pipeline_id <- pipeline_id
  train_collector$pipeline_entered <- TRUE
  invisible(train_collector)
}

stpd_candidate_lineage_collector_capture <- function(
    train_collector, hook_id, payload, validate_payload = TRUE) {
  if (is.null(train_collector)) return(invisible(NULL))
  stpd_candidate_lineage_validate_train_collector(
    train_collector, require_state = "open", validate_observations = FALSE
  )
  if (!isTRUE(train_collector$pipeline_entered)) {
    stpd_candidate_lineage_abort(
      "collector_pipeline_not_entered",
      "Direct observations require an entered detector pipeline."
    )
  }
  hook_id <- stpd_candidate_lineage_assert_text(hook_id, "hook_id")
  if (!grepl("^[a-z][a-z0-9_]*_v[0-9]+$", hook_id)) {
    stpd_candidate_lineage_abort(
      "collector_hook_invalid",
      "hook_id must be a stable lower-case versioned identifier.",
      field = "hook_id"
    )
  }
  if (!stpd_candidate_lineage_observation_payload_is_flat(payload)) {
    stpd_candidate_lineage_abort(
      "collector_payload_invalid",
      paste(
        "Observation payload must be a uniquely named flat data frame",
        "containing only stable typed columns."
      )
    )
  }
  if (isTRUE(validate_payload)) {
    stpd_candidate_lineage_validate_hook_payload(
      train_collector, hook_id, payload
    )
  }
  payload_copy <- unserialize(serialize(payload, NULL, version = 3L))
  payload_hash <- stpd_threshold_first_hash_domain(
    "stpd-candidate-lineage-observation-payload-v1", payload_copy
  )
  observation_id <- stpd_threshold_first_hash_domain(
    "stpd-candidate-lineage-observation-id-v1",
    list(
      run_id = train_collector$run_id,
      params_hash = train_collector$params_hash,
      dataset_id = train_collector$dataset_id,
      train = train_collector$train,
      pipeline_id = train_collector$pipeline_id,
      hook_id = hook_id,
      payload_sha256 = payload_hash
    )
  )
  existing <- match(hook_id, train_collector$observation_index$hook_id)
  if (!is.na(existing)) {
    if (!identical(
      train_collector$observation_index$observation_id[[existing]],
      observation_id
    )) {
      stpd_candidate_lineage_abort(
        "collector_hook_conflict",
        "One train hook cannot capture two different payloads."
      )
    }
    return(invisible(observation_id))
  }
  row <- data.frame(
    observation_version = STPD_CANDIDATE_LINEAGE_OBSERVATION_VERSION,
    observation_id = observation_id, hook_id = hook_id,
    payload_sha256 = payload_hash, record_n = as.integer(nrow(payload_copy)),
    capture_status = "direct_partial", stringsAsFactors = FALSE,
    check.names = FALSE
  )
  train_collector$observation_index <- rbind(
    train_collector$observation_index, row
  )
  rownames(train_collector$observation_index) <- NULL
  train_collector$observations[[hook_id]] <- payload_copy
  train_collector$instrumentation_status <- "direct_partial"
  invisible(observation_id)
}

stpd_candidate_lineage_collector_observation_snapshot <- function(
    train_collector) {
  # Snapshot is a read-only export. Verify the complete index, flat schemas,
  # record counts, and cryptographic payload/observation hashes here. The
  # substantially more expensive scientific cross-hook replay remains
  # mandatory at train closure and in explicit payload validation.
  stpd_candidate_lineage_validate_train_collector(
    train_collector, validate_observation_semantics = FALSE
  )
  unserialize(serialize(list(
    instrumentation_status = train_collector$instrumentation_status,
    observation_index = train_collector$observation_index,
    observations = train_collector$observations
  ), NULL, version = 3L))
}

stpd_candidate_lineage_collector_end_train <- function(
    collector, train_collector) {
  if (is.null(collector) && is.null(train_collector)) return(invisible(NULL))
  stpd_candidate_lineage_validate_collector(
    collector, require_state = "open", require_bound = TRUE
  )
  stpd_candidate_lineage_validate_train_collector(
    train_collector, require_state = "open"
  )
  train <- train_collector$train
  stored <- collector$train_collectors[[train]]
  if (!identical(stored, train_collector) ||
      !identical(train_collector$run_id, collector$run_id) ||
      !identical(train_collector$params_hash, collector$params_hash) ||
      !identical(train_collector$dataset_id, collector$dataset_id)) {
    stpd_candidate_lineage_abort(
      "collector_cross_run_shard",
      "The train collector does not belong to this run collector."
    )
  }
  if (!isTRUE(train_collector$pipeline_entered)) {
    stpd_candidate_lineage_abort(
      "collector_pipeline_not_entered",
      "A train collector cannot complete before detector dispatch is entered."
    )
  }
  shards <- collector$train_collectors
  for (shard in shards) {
    stpd_candidate_lineage_validate_train_collector(shard)
  }
  completed_statuses <- unique(vapply(
    shards,
    function(shard) {
      if (identical(shard, train_collector) ||
          identical(shard$state, "completed")) {
        shard$instrumentation_status
      } else NA_character_
    },
    character(1)
  ))
  completed_statuses <- completed_statuses[!is.na(completed_statuses)]
  next_instrumentation_status <- collector$instrumentation_status
  if (length(completed_statuses) == 1L) {
    next_instrumentation_status <- completed_statuses[[1L]]
  } else if (length(completed_statuses) > 1L) {
    next_instrumentation_status <- "direct_partial"
  }
  train_collector$state <- "completed"
  collector$instrumentation_status <- next_instrumentation_status
  invisible(train_collector)
}

stpd_candidate_lineage_collector_abort <- function(collector) {
  if (is.null(collector)) return(invisible(NULL))
  stpd_candidate_lineage_validate_collector(collector)
  if (identical(collector$state, "aborted")) return(invisible(collector))
  if (identical(collector$state, "sealed")) {
    stpd_candidate_lineage_abort(
      "collector_state_invalid", "A sealed collector cannot be aborted."
    )
  }
  for (shard in collector$train_collectors) {
    if (stpd_candidate_lineage_is_train_collector(shard) &&
        identical(shard$state, "open")) shard$state <- "aborted"
  }
  collector$state <- "aborted"
  invisible(collector)
}

stpd_candidate_lineage_collector_snapshot <- function(collector) {
  stpd_candidate_lineage_validate_collector(collector)
  trains <- collector$target_trains
  rows <- lapply(trains, function(train) {
    shard <- collector$train_collectors[[train]]
    if (is.null(shard)) {
      state <- if (identical(collector$requested_audit_level, "off")) {
        "disabled"
      } else "not_started"
      pipeline_id <- NA_character_
      pipeline_entered <- FALSE
    } else {
      stpd_candidate_lineage_validate_train_collector(shard, train = train)
      state <- shard$state
      pipeline_id <- shard$pipeline_id
      pipeline_entered <- shard$pipeline_entered
    }
    data.frame(
      collector_version = STPD_CANDIDATE_LINEAGE_COLLECTOR_VERSION,
      requested_audit_level = collector$requested_audit_level,
      collector_state = collector$state,
      run_id = collector$run_id,
      params_hash = collector$params_hash,
      dataset_id = collector$dataset_id,
      train = train,
      train_state = state,
      pipeline_id = pipeline_id,
      pipeline_entered = pipeline_entered,
      instrumentation_status = if (is.null(shard)) {
        collector$instrumentation_status
      } else shard$instrumentation_status,
      stringsAsFactors = FALSE, check.names = FALSE
    )
  })
  train_receipts <- if (length(rows)) do.call(rbind, rows) else data.frame(
    collector_version = character(), requested_audit_level = character(),
    collector_state = character(), run_id = character(),
    params_hash = character(), dataset_id = character(), train = character(),
    train_state = character(), pipeline_id = character(),
    pipeline_entered = logical(), instrumentation_status = character(),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  rownames(train_receipts) <- NULL
  list(
    receipt_version = STPD_CANDIDATE_LINEAGE_REQUEST_RECEIPT_VERSION,
    collector_version = collector$collector_version,
    requested_audit_level = collector$requested_audit_level,
    collector_state = collector$state,
    bound = collector$bound,
    run_id = collector$run_id,
    params_hash = collector$params_hash,
    dataset_id = collector$dataset_id,
    target_trains = collector$target_trains,
    instrumentation_status = collector$instrumentation_status,
    train_receipts = train_receipts
  )
}

stpd_candidate_lineage_strip <- function(scientific_result) {
  if (!is.list(scientific_result) || is.data.frame(scientific_result)) {
    stpd_candidate_lineage_abort(
      "scientific_result_invalid",
      "Candidate-lineage stripping requires a named scientific result list."
    )
  }
  scientific_result$candidate_lineage_audit <- NULL
  scientific_result
}

stpd_candidate_lineage_collector_finalize_unavailable <- function(
    scientific_result, collector) {
  if (is.null(collector)) return(scientific_result)
  stpd_candidate_lineage_validate_collector(
    collector, require_state = "open", require_bound = TRUE
  )
  if (identical(collector$requested_audit_level, "off")) {
    collector$state <- "sealed"
    return(scientific_result)
  }
  missing <- setdiff(collector$target_trains, names(collector$train_collectors))
  incomplete <- vapply(
    collector$train_collectors,
    function(shard) !stpd_candidate_lineage_is_train_collector(shard) ||
      !identical(shard$state, "completed") ||
      !isTRUE(shard$pipeline_entered),
    logical(1)
  )
  if (length(missing) || any(incomplete)) {
    stpd_candidate_lineage_abort(
      "collector_train_scope_incomplete",
      "All requested trains must complete detector dispatch before finalization."
    )
  }
  train_statuses <- unique(vapply(
    collector$train_collectors,
    function(shard) shard$instrumentation_status,
    character(1)
  ))
  if (length(train_statuses) != 1L ||
      !identical(collector$instrumentation_status, train_statuses[[1L]])) {
    stpd_candidate_lineage_abort(
      "collector_instrumentation_mixed",
      paste(
        "All requested trains must have the same directly observed",
        "instrumentation coverage before live attachment."
      )
    )
  }
  # Build a sealed immutable receipt while the live collector remains open.
  # Only seal the collector itself after both R90 and the stricter live guard
  # accept the complete attachment, so a failed assembly remains abortable.
  receipt <- stpd_candidate_lineage_collector_snapshot(collector)
  receipt$collector_state <- "sealed"
  receipt$train_receipts$collector_state[] <- "sealed"
  unavailable_reason <- if (identical(
    collector$instrumentation_status, "pipeline_not_instrumented"
  )) {
    "pipeline_not_instrumented"
  } else if (identical(collector$instrumentation_status, "direct_complete")) {
    "adapter_mapping_unavailable"
  } else {
    "candidate_universe_unavailable"
  }
  out <- stpd_candidate_lineage_live_attach(
    scientific_result,
    stpd_candidate_lineage_empty_bundle(),
    authoritative_products =
      stpd_candidate_lineage_empty_authoritative_products(),
    expected_cap_scopes = stpd_candidate_lineage_empty_cap_policies(),
    validation_mode = "diagnostic_unavailable",
    unavailable_reason = unavailable_reason,
    receipt = receipt,
    expected_run_id = collector$run_id,
    expected_params_hash = collector$params_hash,
    expected_dataset_id = collector$dataset_id,
    expected_target_trains = collector$target_trains
  )
  collector$state <- "sealed"
  out
}
