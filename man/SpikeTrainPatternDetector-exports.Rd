
\name{SpikeTrainPatternDetector-exports}
\alias{apply_schema_defaults}
\alias{build_candidate_ledger}
\alias{build_event_audit}
\alias{build_spike_dataset}
\alias{build_trains_from_annot}
\alias{build_trains_from_raw}
\alias{collapse_duplicate_timestamps_train}
\alias{collapse_duplicate_timestamps_trains}
\alias{compute_candidate_feature_table}
\alias{compute_candidate_features}
\alias{compute_isi_percentiles}
\alias{compute_local_median_cache}
\alias{default_params}
\alias{default_params_sec}
\alias{detect_burst_candidates}
\alias{detect_pause_candidates}
\alias{detect_tonic_candidates}
\alias{export_detection_results}
\alias{export_detection_results_simple}
\alias{export_results}
\alias{final_classify_candidate}
\alias{final_classify_candidates}
\alias{generate_candidates}
\alias{interval_best_overlap}
\alias{launch_spike_detector}
\alias{run_detector}
\alias{run_detector_dataset}
\alias{run_detector_dataset_internal}
\alias{run_detector_one_train}
\alias{run_detector_train}
\alias{run_qc}
\alias{scan_short_isi_runs}
\alias{scan_structure_candidates}
\alias{schema_params_from_input}
\alias{schema_ui_controls}
\alias{stpd_architecture_notes}
\alias{stpd_analyze_state_dynamics}
\alias{stpd_build_transition_model_data}
\alias{stpd_build_logisih_pasquale}
\alias{stpd_basic_sensitivity_paths}
\alias{stpd_candidate_states_gmm}
\alias{stpd_candidate_states_rule_based}
\alias{stpd_compute_features}
\alias{stpd_csv_input_policy_notes}
\alias{stpd_decode_hsmm}
\alias{stpd_detect}
\alias{stpd_detect_logisi_newBD_pasquale}
\alias{stpd_detect_misi_bursts_article}
\alias{stpd_detect_train_dispatch}
\alias{stpd_enhance_candidate_features_eventness}
\alias{stpd_estimate_logisi_threshold_pasquale}
\alias{stpd_estimate_misi_threshold}
\alias{stpd_event_iou}
\alias{stpd_event_level_metrics}
\alias{stpd_event_level_match_table}
\alias{stpd_event_level_validation}
\alias{stpd_event_level_validation_export}
\alias{stpd_event_level_validation_report}
\alias{stpd_event_aligned_activity}
\alias{stpd_event_aligned_bin_table}
\alias{stpd_event_aligned_raster_plot}
\alias{stpd_event_aligned_psth_plot}
\alias{stpd_event_aligned_population_plot}
\alias{stpd_event_aligned_heatmap_plot}
\alias{stpd_event_aligned_correlation_plot}
\alias{stpd_event_aligned_correlogram_plot}
\alias{stpd_events_apply_metric_mode}
\alias{stpd_export_results}
\alias{stpd_extract_task_events_from_raw}
\alias{stpd_normalize_task_events}
\alias{stpd_extract_events_by_source}
\alias{stpd_fit_transition_statistical_model}
\alias{stpd_generate_candidates}
\alias{stpd_golden_test_dataset}
\alias{stpd_hsmm_heldout_likelihood}
\alias{stpd_holdout_split_by_train}
\alias{stpd_key_parameter_schema}
\alias{stpd_label_agreement}
\alias{stpd_logisi_support_dataset}
\alias{stpd_logisi_support_export}
\alias{stpd_make_isi_state_space_features}
\alias{stpd_make_logisi_phase_portrait}
\alias{stpd_make_recurrence_plot}
\alias{stpd_manual_events}
\alias{stpd_match_events}
\alias{stpd_match_events_greedy}
\alias{stpd_merge_isi_ranges}
\alias{stpd_metric_mode_normalize}
\alias{stpd_motif_frequency}
\alias{stpd_misi_support_dataset}
\alias{stpd_misi_support_export}
\alias{stpd_product_schema_defaults}
\alias{stpd_productize_params}
\alias{stpd_public_parameter_table}
\alias{stpd_parameter_deprecation_notes}
\alias{stpd_parameter_contract}
\alias{stpd_parameter_registry}
\alias{stpd_parameter_sensitivity_export}
\alias{stpd_parameter_sensitivity_scan}
\alias{stpd_parameter_schema}
\alias{stpd_parameter_schema_full}
\alias{stpd_predicted_events}
\alias{stpd_recommend_family_subtype}
\alias{stpd_result_consistency_check}
\alias{stpd_rqa_metrics}
\alias{stpd_run_isi_state_diffusion_map}
\alias{stpd_run_isi_state_isomap}
\alias{stpd_run_isi_state_isomap_sweep}
\alias{stpd_run_isi_state_pca}
\alias{stpd_run_isi_state_phate}
\alias{stpd_run_isi_state_tsne}
\alias{stpd_run_isi_state_umap}
\alias{stpd_method_readme}
\alias{stpd_params_hash}
\alias{stpd_parameter_report}
\alias{stpd_compute_candidate_features}
\alias{stpd_generate_candidate_events}
\alias{stpd_detect_patterns}
\alias{stpd_run_detector}
\alias{stpd_schema_coverage_report}
\alias{stpd_scientific_validation_report}
\alias{stpd_scientific_validation_summary}
\alias{stpd_split_trains_by_manual_events}
\alias{stpd_state_bootstrap_metrics}
\alias{stpd_state_dwell_times}
\alias{stpd_state_sequence}
\alias{stpd_state_space_pattern_labels}
\alias{stpd_state_surrogate_controls}
\alias{stpd_state_transition_matrix}
\alias{stpd_state_transition_table}
\alias{stpd_train_pipeline_default}
\alias{stpd_train_pipeline_registry}
\alias{stpd_transition_entropy}
\alias{stpd_validate_params}
\alias{stpd_write_scientific_validation_exports}
\alias{validate_dataset_quality_impl}
\title{SpikeTrainPatternDetector exported functions}
\description{
Public functions exported by SpikeTrainPatternDetector.  The package provides
raw spike-train data import, quality control, event-grammar based detection,
manual review, candidate audit, support methods and export utilities.
}
\details{
The event-grammar detector separates diagnostic candidate windows from public selected
candidate ledgers.  Rejected/profile/unwritten diagnostic windows remain in
\code{diagnostic candidate audit}; public tables such as \code{Candidate_ledger},
\code{Eventness_audit} and final classification exports contain selected public
candidates only.  For raw CSV import, high-confidence derived analysis files are
blocked by default.  Use \code{allow_derived_csv = TRUE} only for intentional
overrides.
Task/behavior timestamp columns named \code{Event} or \code{Event_*} are parsed
as a separate task-event annotation layer rather than spike trains.  These task
events are available for original-time raster overlays, peri-event neural
manifold annotation and sliceTCA trial alignment, but they are not used as core
burst/pause/tonic detector inputs.
The event-aligned activity helpers use this separate task-event layer to build
peri-event rasters, neuron-level PSTHs, population mean firing-rate summaries,
baseline z-scored heatmaps, spike-count correlations and cross-correlograms.
These outputs are downstream explanatory analyses and do not change the core
event grammar.
}
\usage{
stpd_csv_input_policy_notes()
}
\value{
Function-specific return values vary.  See package README files and Shiny UI
help text for workflow-level documentation.  \code{stpd_csv_input_policy_notes} returns a data frame describing CSV input and audit separation policy.  \code{stpd_public_parameter_table} returns the productized, version-neutral parameter table.
}
