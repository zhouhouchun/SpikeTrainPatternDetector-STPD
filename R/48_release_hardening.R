# ============================================================
# event grammar release hardening layer
# ------------------------------------------------------------
# This layer does not change the validated event grammar detector.
# It tightens public API boundary semantics, documents input-file guardrails
# in machine-readable helpers, and reduces R CMD check NSE/global binding
# warnings for tidy-evaluation style data-frame columns.
# ============================================================

# Broaden neutral rejection_reason handling for external API users.
# Valid public candidates should normally leave rejection_reason blank.  However,
# if an external caller provides natural-language neutral values, do not treat
# them as active reject diagnostics.
stpd_event_grammar_rejection_reason_active <- function(x) {
  y <- stpd_event_grammar_chr_lower(x)
  if (length(y) == 0) return(FALSE)
  y <- gsub("\\s+", "_", y)
  neutral <- !nzchar(y) | y %in% c(
    "na", "nan", "n_a", "none", "no", "null", "ok", "okay", "pass", "passed",
    "accept", "accepted", "selected", "kept", "keep", "written", "valid", "public",
    "no_rejection", "no_reject", "not_rejected", "not_a_rejection", "not_rejection",
    "not_applicable", "not_apply", "not_applicable_to_public_candidate", "n/a"
  )
  any(!neutral, na.rm = TRUE)
}

# Human-readable notes for users and downstream wrappers.  Keep this helper
# intentionally simple so it can be printed from notebooks, tests, or Shiny.
stpd_csv_input_policy_notes <- function() {
  data.frame(
    topic = c(
      "raw_spike_csv",
      "derived_csv_hard_block",
      "override",
      "rejection_reason",
      "audit_separation",
      "duplicate_timestamps"
    ),
    policy = c(
      "Raw CSV files should contain spike timestamp columns. Each numeric column is interpreted as one spike train after NA removal and sorting.",
      "High-confidence derived outputs such as Sliding_*, ISI_base, tonic_summary, threshold/threshould, Candidate_ledger, Eventness_audit, Events_final, event grammar_candidate, summary/result/features/audit files are blocked by default.",
      "Use allow_derived_csv=TRUE only when intentionally bypassing the derived-table guard. This is not recommended for routine batch processing.",
      "rejection_reason is reserved for diagnostic/rejected rows. Public selected candidates should leave it blank; neutral strings such as 'no rejection' and 'not applicable' are also accepted.",
      "candidate_diagnostic_audit keeps all diagnostic/rejected windows. Candidate_ledger, candidate_features, Eventness_audit and Final_classification contain only selected public candidates.",
      "Exact duplicate timestamps should be reported. For formal statistics, consider duplicate_policy='collapse_exact' or report duplicate impact on ISI, burst/pause and LV/CV/MM metrics."
    ),
    stringsAsFactors = FALSE
  )
}

# CRAN/R CMD check NSE binding hints.  These are column names used in dplyr,
# plotly/DT tables and audit data frames.  They do not affect runtime behavior.
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    ".", ".data", "value", "label", "pattern", "source", "train", "side", "structure",
    "hemisphere", "trajectory", "recording_depth", "event_id", "candidate_id",
    "candidate_layer", "candidate_class", "raw_candidate_class", "final_candidate_class",
    "final_class", "final_label", "pattern_auto", "pattern_manual", "pattern_final",
    "pattern_manual_negative", "source_label", "start_isi", "end_isi", "start_spike",
    "end_spike", "start_time_sec", "end_time_sec", "duration_sec", "duration_ms",
    "n_spikes", "n_isi", "isi_sec", "ISI_sec", "isi_ms", "ISI_ms", "isi_index",
    "left_time_sec", "right_time_sec", "mid_time_sec", "timestamp_sec", "x", "y",
    "x0", "x1", "x_mid", "y0", "y1", "count", "fraction", "density", "bin_left",
    "bin_right", "bin_mid", "raw_count", "raw_fraction", "balanced_fraction",
    "seed_band_lower_sec", "seed_band_upper_sec", "bridge_band_upper_sec",
    "boundary_floor_sec", "seed_band_fraction", "seed_high_percentile_in_train",
    "seed_low_percentile_in_train", "seed_run_count", "max_seed_run_length",
    "core_isi_count", "bridge_isi_count", "bridge_fraction", "seed_purity",
    "intra_q50_sec", "intra_q90_sec", "intra_q95_sec", "max_intra_ISI_sec",
    "pre_gap_sec", "post_gap_sec", "pre_ratio_q90", "post_ratio_q90",
    "min_flank_ratio_q90", "burst_contrast_score", "burst_contrast_required",
    "boundary_type", "selection_status", "gate_status", "decision_path", "action",
    "policy_action", "rejection_reason", "written_to_auto", "selected_for_auto",
    "review_required", "confidence_tier", "score", "priority", "eventness_score",
    "eventness_zone", "family", "subtype", "recommended_final_class", "metric",
    "manual_negative_veto", "artifact", "artifact_flag", "refractory_suspect",
    "duplicate_timestamp", "valid", "n_valid_ISI", "median_ISI", "q10_ISI", "q25_ISI",
    "q90_ISI", "pause_fraction", "phenotype_prior", "threshold", "threshold_sec",
    "threshold_ms", "field", "user", "manual", "histogram", "default", "effective",
    "unit", "method", "status", "message",
    "Spike train recording item name", "spike_train", "Spike_train", "Fragment_number",
    "Start_time", "End_time", "Max_ISI", "Median_ISI", "Mean_ISI", "MM", "CV", "LV",
    "q05", "q10", "q40", "q50", "q90", "q95", "q99", "min", "max",
    "train_id", "dataset", "dataset_id", "filename", "file", "path", "row_id", "col", "name",
    "n", "idx", "index", "left", "right", "type", "mode", "class", "raw", "balanced",
    "lower", "upper", "lower_sec", "upper_sec", "seed_lower", "seed_upper", "bridge_upper",
    "warning", "severity", "topic", "policy", "time",
	    "After_LV", "audit_action", "audit_base_final_label", "audit_final_label",
	    "audit_from_label", "audit_id", "audit_reason", "audit_source", "audit_time",
	    "audit_to_label", "auto_label", "auto_score", "bridge_class", "bridge_score",
	    "auto_label_original", "auto_pattern_majority",
	    "bridge_start_isi", "candidate_source", "category", "context_post_ISI",
    "context_post_ISI_sec", "context_pre_ISI", "context_pre_ISI_sec",
    "contrast_geom_ctx_q", "contrast_geom_q", "contrast_min_ctx_q",
    "contrast_min_q", "contrast_pct_ctx_q", "contrast_pct_q", "core_max_ISI",
    "core_max_ISI_sec", "core_median_ISI", "core_median_ISI_sec",
    "core_min_ISI", "core_min_ISI_sec", "core_q_ISI", "core_q_ISI_sec",
    "core_q90_ISI", "current_value", "direction", "duration",
    "edge_contrast_geom_q", "edge_contrast_min_q", "end_spike_idx",
    "end_time", "F1", "failure_count", "inter_event_interval",
    "inter_event_interval_sec", "interval_type", "is_artifact", "ISI",
    "ISI index", "isi_end_align_sec", "isi_end_sec", "isi_end_timestamp_sec",
    "isi_mid_sec", "ISI_pct", "ISI_rank_n", "isi_start_align_sec",
    "isi_start_sec", "isi_start_timestamp_sec", "isi_values_sec", "keep_seed",
	    "label_source", "label_source_html", "left_seed_id", "manual_hint", "manual_label", "manual_negative_label", "max_ISI", "max_ISI_sec",
    "mean_ISI", "mean_ISI_sec", "median_ISI_sec", "mid_align_sec", "min_ISI",
    "min_ISI_sec", "n_flank", "n_flank_ctx", "next_start", "nm_id",
    "parameter", "pattern_audit_final", "pattern_audit_final_chr", "pattern_audit_final_html",
    "pattern_auto_chr", "pattern_final_chr", "pattern_final_html", "pattern_manual_chr",
    "post_ISI", "post_ISI_sec", "post_ratio_q", "pre_ISI", "pre_ISI_sec",
    "Pre_LV", "pre_ratio_q", "pre_score", "precision", "pred_label",
    "prediction", "reason", "recall", "reject_reason", "relative_change",
    "relative_change_pct", "required_value", "right_seed_id", "section",
    "seed_decision", "seed_id", "seed_score", "seed_source", "Spike i time",
    "Spike i+1 time", "spike_i_time_sec", "start_spike_idx", "start_time",
    "structure_class", "structure_id", "structure_score", "support",
    "time_align_sec", "timestamp_left_sec", "timestamp_right_sec",
		    "cap_x0", "cap_x1", "depth_y", "display_index", "dot_body_size",
		    "dot_inner_size", "dot_inner_x", "dot_inner_y", "dot_shadow_size",
		    "dot_shadow_x", "dot_shadow_y", "dot_side_size", "dot_side_x",
		    "dot_side_y", "electrode_x", "group", "highlight_x", "hjust",
		    "label_hjust", "label_x", "lane_y", "left_rim_x", "legend_name",
		    "n_spikes_window", "raster_label_x", "raster_x0", "raster_x1",
		    "right_rim_x", "shadow_x", "spike_time_rel",
	    "spike_time_sec", "window_end_sec", "window_start_sec",
	    "possible_burst_subtype", "possible_burst_subtype_html", "tonic_like", "train_label", "train_label_html", "train_order", "truth", "uncertainty_reason", "uncertainty_reason_html", "user_override_from",
	    "user_override_id", "user_override_label", "user_override_reason",
	    "user_override_source", "user_override_time", "user_override_to",
	    "user_promoted_possible_burst", "n_user_promoted_isi", "value_sec",
    # Shiny server module installers evaluate in the live server environment.
    "input", "session", "aligned_data", "apply_manual_selection",
    "collapse_duplicate_spikes_for_dataset", "detector_notify_error",
    "current_dataset", "current_param_for_tables", "current_train_metadata", "current_trains",
    "detector_event_counts", "displayed_train_names",
    "format_detector_before_after", "get_dataset", "metadata_filtered_train_names",
    "min_valid_isi_sec", "normalize_dataset", "pattern_isi_gate_patterns",
    "pool_dataset_ids", "qc_isi_unit", "read_params_from_ui",
    "refractory_suspect_sec", "refresh_xrange_slider", "safe_ui_value",
    "selected_points", "selection_from_cache", "selection_time_isi_indices",
    "set_dataset", "threshold_unit_factor_from_sec",
	    "threshold_unit_factor_to_sec", "track_step", "unit_factor",
	    "update_current_dataset_trains", "update_xrange_slider_input",
	    "sync_xrange_length_inputs", "xrange_window_width", "current_xrange_window",
	    "same_xrange_numeric", "apply_xrange_window_length", "xend", "yend"
	  ))
	}
