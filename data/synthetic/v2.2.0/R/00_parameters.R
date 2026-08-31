parameters <- list(
  dataset_id = "clean_synthetic_mechanism_benchmark_v2_2_0",
  dataset_version = "2.2.0",
  n_templates = 20L,
  development_templates = sprintf("TPL_%03d", 1:5),
  holdout_templates = sprintf("TPL_%03d", 6:20),
  template_seed_base = 2026082700L,
  sample_blinding_seed = 2026082791L,
  phenotype_null_seed = 2026082777L,
  quota_seed = 2026082711L,
  calibration_selection_seed = 2026082780L,
  rng_contract = list(
    namespace = "clean_benchmark_component_rng_v1",
    hash_algorithm = "xxhash32",
    key_fields = c("Template_ID", "Component", "Component_Slot", "Replicate"),
    streams = c("schedule", "background", "burst", "pause", "tonic", "hfs", "nested_burst", "point_process"),
    version_is_not_in_seed_key = TRUE,
    local_component_hash_algorithm = "sha256"
  ),
  anchor_B_s = 0.100,
  scale_factors = c(1, 4, 10),
  workbook_blind_batches = list(Batch_A = 4, Batch_B = 10, Batch_C = 1),
  stimulation_enabled = FALSE,
  observation_noise_enabled = FALSE,
  drift_enabled = FALSE,
  renewal_family = "shifted_gamma",
  shared_refractory_B = 0.020,
  frozen_per_template_quota = list(
    Standalone_Burst = 3L,
    Burst_in_HFS = 2L,
    Broad_HFS_total = 3L,
    Tonic_generic = 1L,
    Tonic_STN_like = 1L,
    Canonical_Pause = 2L,
    Contextual_Separator_secondary = 1L
  ),
  complex_pause_templates = sprintf("TPL_%03d", c(1, 3, 6, 8, 10, 12, 14, 16, 18, 20)),
  four_spike_assignments = data.frame(
    Template_ID = sprintf("TPL_%03d", c(1, 2, 3, 4, 5, 6, 8, 10, 12, 14, 16, 18)),
    Context = c("standalone", "hfs", "hfs", "standalone", "hfs", "standalone",
                "hfs", "standalone", "hfs", "standalone", "hfs", "standalone"),
    Strength = c("weak", "weak", "moderate", "strong", "strong", "moderate",
                 "moderate", "weak", "strong", "strong", "weak", "moderate"),
    stringsAsFactors = FALSE
  ),
  background = list(
    mean_isi_B = c(0.55, 0.80),
    gamma_shape = c(2.0, 5.5),
    boundary_spikes = c(2L, 4L),
    between_macro_insertion_probability = 0.55
  ),
  tonic = list(
    # The intended stratum is frozen before generation.  It is not the final
    # phenotype label, which is calculated later without detector output.
    intended_stratum_by_template = c(rep("eligible", 12L), rep("ambiguous", 5L), rep("no_evidence", 3L)),
    phenotype_score_thresholds = c(no_evidence_upper = 0.45, eligible_lower = 0.65),
    phenotype_score_weights = c(stability = 0.40, duration = 0.20, local_separation = 0.20, subtype_band = 0.20),
    flank_isis_each_side = 4L,
    regimes = list(
      eligible = list(boundary_spikes = c(12L, 18L),
                      generic_stress = list(mean_isi_B = c(0.75, 1.10), gamma_shape = c(12, 30), cv_qc_max = 0.40, cv2_qc_max = 0.50, lv_qc_max = 0.35),
                      stn_like_empirical = list(mean_isi_B = c(0.35, 0.55), gamma_shape = c(20, 80), cv_qc_max = 0.30, cv2_qc_max = 0.40, lv_qc_max = 0.20)),
      ambiguous = list(boundary_spikes = c(9L, 13L),
                       generic_stress = list(mean_isi_B = c(0.55, 0.85), gamma_shape = c(4, 10), cv_qc_max = 0.65, cv2_qc_max = 0.75, lv_qc_max = 0.65),
                       stn_like_empirical = list(mean_isi_B = c(0.24, 0.38), gamma_shape = c(8, 25), cv_qc_max = 0.50, cv2_qc_max = 0.60, lv_qc_max = 0.45)),
      no_evidence = list(boundary_spikes = c(8L, 12L),
                         generic_stress = list(mean_isi_B = c(0.55, 0.80), gamma_shape = c(2.5, 6), cv_qc_max = 0.85, cv2_qc_max = 0.95, lv_qc_max = 0.85),
                         stn_like_empirical = list(mean_isi_B = c(0.18, 0.32), gamma_shape = c(6, 15), cv_qc_max = 0.70, cv2_qc_max = 0.80, lv_qc_max = 0.65))
    ),
    generic_stress = list(
      mean_isi_B = c(0.72, 1.18),
      gamma_shape = c(4.0, 12.0),
      cv_qc_max = 0.55,
      cv2_qc_max = 0.70,
      lv_qc_max = 0.55
    ),
    stn_like_empirical = list(
      mean_isi_B = c(0.20, 0.55),
      gamma_shape = c(20.0, 80.0),
      cv_qc_max = 0.30,
      cv2_qc_max = 0.40,
      lv_qc_max = 0.20,
      source = "frozen_summary_of_manually_labeled_PD_STN_tonic_ISIs",
      source_isi_n = 77L,
      source_contiguous_run_n = 25L,
      source_median_isi_s = 0.031962,
      source_q05_isi_s = 0.0160964,
      source_q95_isi_s = 0.0690200,
      source_median_run_cv = 0.0874550,
      source_median_run_lv = 0.0094980,
      source_sha256 = "a70de1ae859b70de39a544b426990b73159c480ea4aa9e3467dfa8ba1d3aa5d5"
    )
  ),
  broad_hfs = list(
    mean_isi_B = c(0.18, 0.32),
    boundary_spikes = c(20L, 35L),
    minimum_duration_B = 3.0,
    direct_support_max_isi_B = 0.45,
    tolerated_interruption_max_isi_B = 0.75,
    maximum_tolerated_interruption_fraction = 0.10,
    maximum_consecutive_tolerated_interruptions = 1L,
    regularity_regimes = list(
      regular = c(6.0, 12.0),
      intermediate = c(2.4, 5.0),
      irregular = c(0.90, 1.80)
    )
  ),
  burst_pulse = list(
    pulse_duration_B = c(0.12, 0.90),
    standard_realized_spikes = 5:9,
    boundary_realized_spikes = 4L,
    maximum_realized_spikes = 10L,
    pre_buffer_B = c(0.45, 0.85),
    post_buffer_B = c(0.45, 0.85),
    standalone_baseline_mean_isi_B = c(0.50, 0.85),
    standalone_baseline_shape = c(1.8, 5.0),
    pulse_strength_strata = list(
      weak = c(4.0, 5.5),
      moderate = c(5.5, 8.0),
      strong = c(8.0, 12.0)
    )
  ),
  canonical_pause = list(gap_B = c(2.8, 5.0), interval_count = 1L),
  complex_pause = list(
    isi_count = c(2L, 3L),
    component_gap_B = c(1.6, 3.2),
    minimum_total_duration_B = 4.5,
    maximum_total_duration_B = 8.5
  ),
  contextual_separator = list(
    gap_B = c(0.45, 0.65),
    local_median_multiplier = 3.5,
    canonical_pause_lower_B = 2.8,
    primary_event_truth = "none",
    secondary_label = "Contextual_Separator"
  ),
  composite_hfs_regime_templates = sprintf("TPL_%03d", c(1, 3, 6, 8, 10, 12, 14, 16, 18, 20)),
  hfs_boundary_audit = list(
    flank_isis_each_side = 3L,
    ambiguous_contrast = 1.15,
    clear_contrast = 1.60
  ),
  phenotype_audit = list(
    minimum_window_isis = 3L,
    maximum_window_isis = 9L,
    flank_isis_each_side = 4L,
    null_surrogates = 1000L,
    ambiguous_quantile = 0.90,
    clear_quantile = 0.975,
    development_only_threshold_fit = TRUE
  ),
  acceptance_contract = list(
    maximum_attempts_per_run = 10000L,
    reject_on_detector_output = FALSE,
    reject_on_visual_clarity = FALSE,
    permitted_rejection_reasons = c(
      "numerical_failure",
      "refractory_violation",
      "frozen_spike_count_contract_failure",
      "contextual_separator_contract_failure",
      "state_duration_contract_failure",
      "complex_pause_contract_failure"
    )
  )
)
