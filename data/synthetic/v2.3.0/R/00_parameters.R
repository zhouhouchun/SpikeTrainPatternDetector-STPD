parameters <- list(
  dataset_id = "clean_synthetic_mechanism_benchmark_v2_3_0",
  dataset_version = "2.3.0",
  supplement_role = "clean_overlap_supplement",
  n_templates = 20L,
  development_templates = sprintf("TPL_%03d", 1:5),
  holdout_templates = sprintf("TPL_%03d", 6:20),
  sample_blinding_seed = 2026082791L,
  phenotype_null_seed = 2026082777L,
  quota_seed = 2026082711L,
  rng_contract = list(
    namespace = "clean_benchmark_component_rng_v1",
    hash_algorithm = "xxhash32",
    key_fields = c("Template_ID", "Component", "Component_Slot", "Replicate"),
    component_types = c("schedule", "background", "burst", "pause", "tonic", "hfs",
                        "hfs_with_nested_burst", "contextual_separator", "borderline_gap"),
    streams = c("hfs", "nested_burst", "hfs_total", "point_process"),
    pulse_streams = list(
      standalone = c("hfs", "nested_burst", "point_process"),
      hfs = c("hfs", "nested_burst", "hfs_total", "point_process")
    ),
    simple_component_stream = "point_process",
    attempt_seed_key_fields = c("Base_Stream_Seed", "Stream", "Attempt"),
    attempt_one_uses_base_stream_seed = TRUE,
    attempt_seed_derivation = "attempt_1=base_stream_seed;attempt_2_plus=xxhash32(namespace|attempt|base_stream_seed|stream|attempt)",
    seed_modulus = 2147483000L,
    version_is_not_in_seed_key = TRUE,
    local_component_hash_algorithm = "sha256"
  ),
  generation_parameter_precedence = list(
    tonic_mean_isi = "tonic.rate_overlap_mean_isi_B[subtype][rate_overlap_stratum]",
    broad_hfs_mean_isi = "broad_hfs.rate_overlap_strata[rate_overlap_stratum]",
    tonic_regularity_and_qc = "tonic.regimes[intended_stratum][subtype]",
    broad_hfs_regularity = "broad_hfs.regularity_regimes[regime]"
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
    Contextual_Separator_secondary = 1L,
    Borderline_Gap_secondary = 1L
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
    # Rate-overlap strata are frozen independently of the phenotype stratum.
    # They create marginal Tonic/HFS overlap while retaining Tonic regularity.
    rate_overlap_stratum_by_template = data.frame(
      Template_ID = sprintf("TPL_%03d", 1:20),
      generic_stress = rep(c("core", "boundary", "deep"), length.out = 20L),
      stn_like_empirical = rep(c("boundary", "deep", "core"), length.out = 20L),
      stringsAsFactors = FALSE
    ),
    rate_overlap_mean_isi_B = list(
      generic_stress = list(core = c(0.70, 0.98), boundary = c(0.46, 0.68), deep = c(0.34, 0.52)),
      stn_like_empirical = list(core = c(0.48, 0.66), boundary = c(0.38, 0.56), deep = c(0.32, 0.48))
    ),
    regimes = list(
      eligible = list(boundary_spikes = c(12L, 18L),
                      generic_stress = list(gamma_shape = c(12, 30), cv_qc_max = 0.40, cv2_qc_max = 0.50, lv_qc_max = 0.35),
                      stn_like_empirical = list(gamma_shape = c(20, 80), cv_qc_max = 0.30, cv2_qc_max = 0.40, lv_qc_max = 0.20)),
      ambiguous = list(boundary_spikes = c(9L, 13L),
                       generic_stress = list(gamma_shape = c(4, 10), cv_qc_max = 0.65, cv2_qc_max = 0.75, lv_qc_max = 0.65),
                       stn_like_empirical = list(gamma_shape = c(8, 25), cv_qc_max = 0.50, cv2_qc_max = 0.60, lv_qc_max = 0.45)),
      no_evidence = list(boundary_spikes = c(8L, 12L),
                         generic_stress = list(gamma_shape = c(2.5, 6), cv_qc_max = 0.85, cv2_qc_max = 0.95, lv_qc_max = 0.85),
                         stn_like_empirical = list(gamma_shape = c(6, 15), cv_qc_max = 0.70, cv2_qc_max = 0.80, lv_qc_max = 0.65))
    ),
    stn_like_empirical = list(
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
    rate_overlap_strata = list(
      core = c(0.18, 0.30),
      boundary = c(0.26, 0.38),
      deep = c(0.32, 0.44)
    ),
    # A Latin-square rotation assigns one core, one boundary and one deep HFS
    # state to every template without confounding overlap stratum and regularity.
    rate_overlap_latin_square = c("core", "boundary", "deep"),
    boundary_spikes = c(20L, 35L),
    minimum_duration_B = 3.0,
    direct_support_max_isi_B = 0.65,
    legacy_v2_2_direct_support_max_isi_B = 0.45,
    support_contract_version = "v2.3_slow_overlap_direct_support_0.65B",
    tolerated_interruption_max_isi_B = 0.85,
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
    pre_buffer_B = c(2.2, 3.5),
    post_buffer_B = c(2.2, 3.5),
    minimum_observed_context_shoulders_each_side = 3L,
    standalone_baseline_mean_isi_B = c(0.50, 0.85),
    standalone_baseline_shape = c(1.8, 5.0),
    pulse_contrast_strata = list(
      weak = c(2.2, 3.0),
      moderate = c(3.0, 4.0),
      strong = c(4.0, 5.5)
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
  borderline_gap = list(
    gap_B = c(0.72, 1.55),
    interval_count = 1L,
    primary_event_truth = "none",
    secondary_label = "Borderline_Gap",
    canonical_pause_lower_B = 2.8,
    boundary_spikes_are_observed = TRUE
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
  dev_holdout_single_isi_baseline = list(
    enabled = TRUE,
    bootstrap_replicates = 1000L,
    bootstrap_seed = 2026082917L,
    threshold_tie_break = "median_threshold_among_maximum_development_template_equal_balanced_accuracy",
    threshold_fit_split = "development",
    evaluation_split = "holdout",
    scale = "dimensionless_anchor_template_only"
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
      "borderline_gap_contract_failure",
      "state_duration_contract_failure",
      "complex_pause_contract_failure"
    )
  )
)
