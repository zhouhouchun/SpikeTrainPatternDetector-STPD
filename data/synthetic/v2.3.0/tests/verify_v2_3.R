#!/usr/bin/env Rscript
options(stringsAsFactors = FALSE, warn = 1)
script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
root <- dirname(dirname(normalizePath(gsub("~\\+~", " ", sub("^--file=", "", script_arg)), mustWork = TRUE)))
required <- c("digest", "jsonlite", "readxl")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Missing R packages: ", paste(missing, collapse = ", "), call. = FALSE)
assert <- function(x, message) if (!isTRUE(x)) stop(message, call. = FALSE)
tol <- 2e-11

p <- jsonlite::read_json(file.path(root, "metadata", "generator_parameters.json"), simplifyVector = TRUE)
spikes_u <- read.csv(file.path(root, "tables", "dimensionless_spike_templates.csv"))
intervals_u <- read.csv(file.path(root, "tables", "dimensionless_interval_templates.csv"))
events_u <- read.csv(file.path(root, "tables", "dimensionless_primary_event_episodes.csv"))
states_u <- read.csv(file.path(root, "tables", "dimensionless_state_envelopes.csv"))
tonic_u <- read.csv(file.path(root, "tables", "dimensionless_tonic_phenotype_evidence.csv"))
regimes_u <- read.csv(file.path(root, "tables", "dimensionless_composite_hfs_regimes.csv"))
runs_u <- read.csv(file.path(root, "tables", "dimensionless_generator_runs.csv"))
borderline_u <- read.csv(file.path(root, "tables", "dimensionless_borderline_gaps.csv"))
contextual_u <- read.csv(file.path(root, "tables", "dimensionless_contextual_separators.csv"))
contextual_links_u <- read.csv(file.path(root, "truth", "contextual_separator_links.csv"))
quota <- read.csv(file.path(root, "metadata", "frozen_burst_quota.csv"))
key <- read.csv(file.path(root, "truth", "sample_template_scale_key.csv"))
intervals <- read.csv(file.path(root, "truth", "interval_truth_multitrack.csv"))
events <- read.csv(file.path(root, "truth", "primary_event_episodes.csv"))
states <- read.csv(file.path(root, "truth", "state_envelopes.csv"))
detector <- read.csv(file.path(root, "detector_inputs", "spike_timestamps_blinded.csv"), check.names = FALSE)
calibration <- read.csv(file.path(root, "calibration", "calibration_same_episode_ids_all_scales.csv"))
baseline <- read.csv(file.path(root, "qc", "single_isi_dev_holdout_baseline.csv"))
holdout_provenance <- jsonlite::read_json(file.path(root, "metadata", "holdout_audit_provenance.json"),
                                          simplifyVector = TRUE)
phenotypes_u <- read.csv(file.path(root, "tables", "dimensionless_observable_phenotype.csv"))
phenotype_thresholds <- read.csv(file.path(root, "metadata", "phenotype_thresholds.csv"))
burst_episode_audit <- read.csv(file.path(root, "truth", "burst_observable_episode_audit.csv"))
observable_burst_episodes <- read.csv(file.path(root, "truth", "observable_burst_positive_episodes.csv"))
null_burst_excursions <- read.csv(file.path(root, "truth", "null_model_burst_like_excursions.csv"))
sensitivity_masks <- read.csv(file.path(root, "truth", "phenotype_sensitivity_masks.csv"))
template_manifest <- read.csv(file.path(root, "metadata", "template_manifest.csv"))

assert(identical(p$dataset_version, "2.3.0"), "Version is not v2.3.0.")
assert(identical(p$supplement_role, "clean_overlap_supplement"), "Supplement role is missing.")
assert(length(unique(spikes_u$Template_ID)) == 20L, "Expected 20 mother templates.")
assert(nrow(key) == 60L && all(table(key$Template_ID) == 3L), "Expected 60 paired projections.")
assert(identical(sort(unique(key$Scale_Factor)), c(1L, 4L, 10L)), "Scale factors are not 1/4/10.")
assert(identical(names(detector), c("Sample_ID", "Spike_Index", "Time_s")), "Detector input leaks truth columns.")
assert(!any(grepl("TPL|Scale|Class|C1|C2|C3", detector$Sample_ID)), "Sample IDs leak template/scale.")
for (sample_id in unique(detector$Sample_ID)) {
  x <- detector[detector$Sample_ID == sample_id, ]
  assert(identical(x$Spike_Index, seq_len(nrow(x))), paste("Spike index failure", sample_id))
  assert(x$Time_s[1] == 0 && all(is.finite(x$Time_s)) && all(diff(x$Time_s) > 0),
         paste("Timestamp integrity failure", sample_id))
}
for (template_id in unique(key$Template_ID)) {
  k <- key[key$Template_ID == template_id, ]; k <- k[order(k$Scale_Factor), ]
  v <- lapply(k$Sample_ID, function(id) detector$Time_s[detector$Sample_ID == id])
  assert(isTRUE(all.equal(v[[2]], 4 * v[[1]], tolerance = tol)), paste("4x mismatch", template_id))
  assert(isTRUE(all.equal(v[[3]], 10 * v[[1]], tolerance = tol)), paste("10x mismatch", template_id))
}
for (x in list(intervals, events, states)) {
  for (u in grep("_u$", names(x), value = TRUE)) {
    s <- sub("_u$", "_s", u)
    if (s %in% names(x)) {
      keep <- is.finite(x[[u]]) & is.finite(x[[s]])
      assert(all(abs(x[[s]][keep] - x[[u]][keep] * x$B_s[keep]) < tol), paste("Scale mismatch", s))
    }
  }
}
assert(all(intervals_u$State_Label %in% c("none", "Tonic", "Broad_HFS")), "Unexpected primary State label.")
assert(all(intervals_u$Event_Label %in% c("none", "Burst", "Pause")), "Unexpected primary Event label.")

burst <- events_u[events_u$Event_Label == "Burst", ]
assert(nrow(burst) == 100L && sum(burst$Event_Context == "burst_in_hfs") == 40L, "Burst quotas failed.")
assert(all(burst$N_Realized_Spikes == burst$Frozen_Target_Spikes), "Burst spike target failed.")
assert(all(burst$N_Realized_Spikes <= 10L) && sum(burst$N_Realized_Spikes == 4L) == 12L, "Burst size contract failed.")
assert(all(with(quota[quota$Target_Spikes == 4L, ], table(Context, Strength)) == 2L),
       "Each context x strength cell must contain two four-spike Bursts.")
for (context in c("standalone", "hfs")) for (strength in c("weak", "moderate", "strong")) {
  tab <- table(factor(quota$Target_Spikes[quota$Context == context & quota$Strength == strength & quota$Target_Spikes > 4L],
                      levels = 5:9))
  assert(all(tab >= ifelse(context == "standalone", 3L, 2L)), "Standard Burst spike-count quota is underfilled.")
}
source(file.path(root, "R", "00_parameters.R"), local = .GlobalEnv)
assert(is.null(parameters$template_seed_base) && is.null(parameters$calibration_selection_seed),
       "Inactive legacy top-level seeds remain in the publication parameter contract.")
assert(is.null(parameters$broad_hfs$mean_isi_B),
       "Broad HFS has more than one active mean-ISI parameter source.")
assert(all(vapply(parameters$tonic$regimes, function(regime) {
  all(vapply(regime[c("generic_stress", "stn_like_empirical")],
             function(subtype) is.null(subtype$mean_isi_B), logical(1)))
}, logical(1))) && is.null(parameters$tonic$generic_stress) &&
         is.null(parameters$tonic$stn_like_empirical$mean_isi_B),
       "Tonic has more than one active mean-ISI parameter source.")
assert(identical(parameters$generation_parameter_precedence$tonic_mean_isi,
                 "tonic.rate_overlap_mean_isi_B[subtype][rate_overlap_stratum]") &&
         identical(parameters$generation_parameter_precedence$broad_hfs_mean_isi,
                   "broad_hfs.rate_overlap_strata[rate_overlap_stratum]"),
       "Mean-ISI parameter precedence is not uniquely declared.")
for (strength in names(parameters$burst_pulse$pulse_contrast_strata)) {
  range <- parameters$burst_pulse$pulse_contrast_strata[[strength]]
  z <- burst$Target_Mean_ISI_Contrast[burst$Pulse_Strength_Stratum == strength]
  assert(all(z >= range[1] - tol & z <= range[2] + tol), paste("Burst contrast range failed", strength))
}
assert(!any(burst$Pulse_Refractory_Limited), "A Burst pulse entered the refractory-limited region.")
min_flank <- parameters$burst_pulse$minimum_observed_context_shoulders_each_side
linked_left <- contextual_links_u$Left_Burst_Event_ID
linked_right <- contextual_links_u$Right_Burst_Event_ID
burst_runs <- runs_u[match(burst$Run_ID, runs_u$Run_ID), ]
assert(all(burst$Latent_Start_u >= burst_runs$Start_u - tol & burst$Latent_End_u <= burst_runs$End_u + tol &
             burst$Latent_End_u >= burst$Latent_Start_u), "A latent Burst window lies outside its retained generator run.")
linked_bursts <- burst$Event_ID %in% c(linked_left, linked_right)
assert(all(burst$Latent_Window_Truncated[linked_bursts]) && !any(burst$Latent_Window_Truncated[!linked_bursts]),
       "Latent-window truncation audit does not match the contextual macro contract.")
direct <- burst$Event_Context == "standalone_burst" & !burst$Event_ID %in% c(linked_left, linked_right)
assert(all(burst$Observed_Left_Context_ISIs[direct] >= min_flank & burst$Observed_Right_Context_ISIs[direct] >= min_flank),
       "Direct standalone Burst flank contract failed.")
assert(all(burst$Observed_Left_Context_ISIs[burst$Event_ID %in% linked_left] >= min_flank &
             burst$Observed_Right_Context_ISIs[burst$Event_ID %in% linked_left] == 0L), "Left contextual Burst flank failed.")
assert(all(burst$Observed_Left_Context_ISIs[burst$Event_ID %in% linked_right] == 0L &
             burst$Observed_Right_Context_ISIs[burst$Event_ID %in% linked_right] >= min_flank), "Right contextual Burst flank failed.")
in_hfs <- burst$Event_Context == "burst_in_hfs"
assert(all(burst$Observed_Left_Context_ISIs[in_hfs] >= min_flank & burst$Observed_Right_Context_ISIs[in_hfs] >= min_flank),
       "HFS-internal Burst flank contract failed.")
injected_evidence <- phenotypes_u[phenotypes_u$Candidate_Origin == "injected_rate_pulse", , drop = FALSE]
null_evidence <- phenotypes_u[phenotypes_u$Candidate_Origin == "null_model_burst_like_excursion", , drop = FALSE]
assert(nrow(injected_evidence) == nrow(burst), "Every injected Burst must have one phenotype audit row.")
expected_parent_state <- burst$State_Envelope_ID[match(injected_evidence$Mechanism_Event_ID, burst$Event_ID)]
assert(identical(as.character(injected_evidence$State_Envelope_ID), as.character(expected_parent_state)),
       "Burst phenotype evidence lost or changed its parent State envelope.")
assert(all(injected_evidence$Flank_Source_Contract == "same_generator_run_non_event_intervals_only"),
       "Injected Burst evidence used a flank outside its generator-run contract.")
left_evidence <- injected_evidence[injected_evidence$Mechanism_Event_ID %in% linked_left, , drop = FALSE]
right_evidence <- injected_evidence[injected_evidence$Mechanism_Event_ID %in% linked_right, , drop = FALSE]
two_sided_evidence <- injected_evidence[!injected_evidence$Mechanism_Event_ID %in% c(linked_left, linked_right), , drop = FALSE]
assert(all(left_evidence$Flank_Pattern == "left_only" & left_evidence$Left_Flank_N_Used >= min_flank &
             left_evidence$Right_Flank_N_Used == 0L), "Left contextual Burst phenotype flank is contaminated.")
assert(all(right_evidence$Flank_Pattern == "right_only" & right_evidence$Left_Flank_N_Used == 0L &
             right_evidence$Right_Flank_N_Used >= min_flank), "Right contextual Burst phenotype flank is contaminated.")
assert(all(two_sided_evidence$Flank_Pattern == "both_sides" &
             two_sided_evidence$Left_Flank_N_Used >= min_flank &
             two_sided_evidence$Right_Flank_N_Used >= min_flank), "A two-sided Burst phenotype flank is incomplete.")
assert(all(phenotype_thresholds$Calibration_Split == "development_only" &
             !phenotype_thresholds$Detector_Output_Used), "Phenotype threshold fitting leaked holdout or detector output.")
injected_thresholds <- phenotype_thresholds$Threshold_Purpose == "injected_support"
assert(all(c("both_sides", "left_only", "right_only") %in%
             phenotype_thresholds$Phenotype_Null_Flank_Pattern[injected_thresholds]),
       "Contextual one-sided Burst null thresholds are missing.")
assert(nrow(null_evidence) == 20L && all(null_evidence$Mechanism_Event_ID == "none"),
       "Natural HFS null-excursion audit is incomplete or promoted to mechanism truth.")
assert(all(burst_episode_audit$Primary_Observable_Burst_Positive ==
             (burst_episode_audit$Candidate_Origin == "injected_rate_pulse" &
                burst_episode_audit$Phenotype_Label == "clear_burst_like")),
       "Primary observable Burst episode flag mixes mechanism origins.")
assert(nrow(observable_burst_episodes) == 3L * sum(injected_evidence$Phenotype_Label == "clear_burst_like") &&
         all(observable_burst_episodes$Candidate_Origin == "injected_rate_pulse" &
               observable_burst_episodes$Phenotype_Label == "clear_burst_like" &
               observable_burst_episodes$Primary_Observable_Burst_Positive),
       "Observable Burst episode export contains a null excursion or non-clear injected event.")
assert(nrow(null_burst_excursions) == 3L * nrow(null_evidence) &&
         all(null_burst_excursions$Candidate_Origin == "null_model_burst_like_excursion" &
               !null_burst_excursions$Primary_Observable_Burst_Positive),
       "Null-model Burst-like excursions are not isolated as a secondary audit.")
expected_observable_burst <- intervals$Phenotype_Candidate_Origin == "injected_rate_pulse" &
  intervals$Phenotype_Label == "clear_burst_like"
expected_null_clear <- intervals$Phenotype_Candidate_Origin == "null_model_burst_like_excursion" &
  intervals$Phenotype_Label == "clear_burst_like"
assert(identical(sensitivity_masks$Burst_Observable_Clear_Positive, expected_observable_burst) &&
         identical(sensitivity_masks$Burst_Null_Model_Clear_Excursion, expected_null_clear) &&
         !any(sensitivity_masks$Burst_Observable_Clear_Positive & sensitivity_masks$Burst_Null_Model_Clear_Excursion),
       "Burst observable and null-model masks are not origin-separated.")

pause <- events_u[events_u$Event_Label == "Pause", ]
assert(sum(pause$Pause_Subtype == "canonical") == 40L && sum(pause$Pause_Subtype == "complex_multi_gap") == 10L,
       "Pause quota failed.")
canonical <- pause[pause$Pause_Subtype == "canonical", ]
assert(all(canonical$N_Realized_ISIs == 1L & canonical$N_Realized_Spikes == 2L), "Canonical Pause boundary contract failed.")
assert(all(canonical$Realized_Duration_u >= 2.8 - tol & canonical$Realized_Duration_u <= 5 + tol), "Canonical Pause range failed.")
assert(nrow(borderline_u) == 20L && all(borderline_u$N_ISIs == 1L & borderline_u$N_Boundary_Spikes == 2L),
       "Borderline-gap boundary contract failed.")
assert(all(borderline_u$Duration_u >= parameters$borderline_gap$gap_B[1] - tol &
             borderline_u$Duration_u <= parameters$borderline_gap$gap_B[2] + tol &
             borderline_u$Duration_u < parameters$canonical_pause$gap_B[1]), "Borderline-gap range failed.")
assert(all(borderline_u$Primary_Event_Label == "none" & !borderline_u$Canonical_Pause_Truth),
       "Borderline gap leaked into primary Pause truth.")
assert(all(contextual_u$Primary_Event_Label == "none"), "Contextual separator leaked into primary Pause truth.")
secondary <- intervals_u$Secondary_Event_Label != "none"
assert(!any(intervals_u$Event_Label[secondary] == "Pause"), "Secondary gap is also labeled primary Pause.")
assert(all(intervals_u$Product_Level_Mapping[secondary] == "Pause"), "Secondary product mapping is missing.")
explicit_gap_runs <- runs_u$Renewal_Family == "explicit_gap"
assert(all(is.na(runs_u$Refractory_u[explicit_gap_runs])),
       "An explicit-gap mechanism incorrectly carries a renewal refractory parameter.")

hfs <- states_u[states_u$State_Label == "Broad_HFS", ]
assert(nrow(hfs) == 60L && all(hfs$N_Boundary_Spikes >= 20L & hfs$N_Boundary_Spikes <= 35L), "HFS count failed.")
assert(all(hfs$Duration_u >= 3 - tol), "HFS duration failed.")
for (template_id in unique(hfs$Template_ID)) {
  assert(identical(sort(unique(hfs$HFS_Rate_Overlap_Stratum[hfs$Template_ID == template_id])),
                   sort(c("core", "boundary", "deep"))), paste("HFS overlap quota failed", template_id))
  assert(identical(sort(unique(hfs$Regularity_Regime[hfs$Template_ID == template_id])),
                   sort(c("regular", "intermediate", "irregular"))), paste("HFS regularity quota failed", template_id))
}
for (state_id in hfs$State_Envelope_ID) {
  x <- intervals_u[intervals_u$State_Envelope_ID == state_id, ]
  tolerated <- x$HFS_Interruption_Role == "tolerated_short_interruption"
  rr <- rle(tolerated)
  assert(mean(tolerated) <= .10 + tol, paste("Too many HFS interruptions", state_id))
  assert(!any(rr$values & rr$lengths > 1L), paste("Consecutive HFS interruptions", state_id))
}
nested <- intervals_u$State_Label == "Broad_HFS" & intervals_u$Event_Label == "Burst"
assert(all(intervals_u$HFS_Interruption_Role[nested] == "nested_burst_event"), "Nested Burst role failed.")
assert(!any(intervals_u$Direct_HFS_Support[nested]), "Nested Burst leaked into direct HFS support.")
assert(all(intervals_u$HFS_Support_Contract_Version[intervals_u$State_Label == "Broad_HFS"] ==
             parameters$broad_hfs$support_contract_version) &&
         all(intervals_u$HFS_Support_Contract_Version[intervals_u$State_Label != "Broad_HFS"] == "not_applicable") &&
         all(hfs$HFS_Support_Contract_Version == parameters$broad_hfs$support_contract_version),
       "The v2.3 HFS support contract is not versioned on truth rows.")
expected_direct_hfs <- intervals_u$State_Label == "Broad_HFS" & !nested &
  intervals_u$ISI_u <= parameters$broad_hfs$direct_support_max_isi_B
expected_legacy_hfs <- intervals_u$State_Label == "Broad_HFS" & !nested &
  intervals_u$ISI_u <= parameters$broad_hfs$legacy_v2_2_direct_support_max_isi_B
assert(identical(intervals_u$Direct_HFS_Support, expected_direct_hfs),
       "Current v2.3 direct HFS support does not match its versioned contract.")
assert(identical(intervals_u$Legacy_V2_2_Direct_HFS_Support_Audit, expected_legacy_hfs) &&
         !any(intervals_u$Legacy_V2_2_Direct_HFS_Support_Audit[nested]),
       "The legacy v2.2-style HFS support audit mask is invalid.")
assert(identical(sensitivity_masks$Broad_HFS_Direct_Support_v2_3, intervals$Direct_HFS_Support) &&
         identical(sensitivity_masks$Broad_HFS_Legacy_v2_2_Direct_Support_Audit,
                   intervals$Legacy_V2_2_Direct_HFS_Support_Audit),
       "Scaled HFS support sensitivity masks are inconsistent with interval truth.")
for (template_id in unique(runs_u$Template_ID)) {
  z <- runs_u[runs_u$Template_ID == template_id, , drop = FALSE]
  z <- z[order(z$Run_Ordinal), , drop = FALSE]
  if (nrow(z) > 1L) {
    same_primary_state <- z$State_Label[-nrow(z)] != "none" &
      z$State_Label[-nrow(z)] == z$State_Label[-1L]
    assert(!any(same_primary_state), paste("Unobservable same-State episode boundary", template_id))
  }
}
assert(sum(template_manifest$Forced_Same_State_Boundary_Separators) > 0L,
       "The frozen schedule did not audit any forced same-State separator.")
assert(nrow(regimes_u) == 10L && !any(regimes_u$Continuous_HFS_State), "Composite Regime contract failed.")

assert(nrow(tonic_u) == 40L && all(tonic_u$Tonic_Phenotype %in% c("eligible", "ambiguous", "no_evidence")),
       "Tonic phenotype audit missing.")
assert(all(is.finite(tonic_u$Tonic_Phenotype_Evidence_Score)) && all(!tonic_u$Detector_Output_Used),
       "Tonic evidence is invalid or detector-dependent.")
assert(all(c("core", "boundary", "deep") %in% tonic_u$Tonic_Rate_Overlap_Stratum), "Tonic overlap strata are incomplete.")
strict_states <- read.csv(file.path(root, "truth", "strict_mechanism_estimand_states.csv"))
observable_states <- read.csv(file.path(root, "truth", "observable_phenotype_estimand_states.csv"))
assert(sum(strict_states$State_Label == "Tonic") == 120L, "Strict Tonic estimand must include all scales.")
assert(sum(observable_states$State_Label == "Tonic") == 3L * sum(tonic_u$Tonic_Phenotype == "eligible"),
       "Observable Tonic estimand mismatch.")

assert(!anyDuplicated(runs_u[, c("Template_ID", "Component_Type", "Component_Slot")]), "Component RNG keys are duplicated.")
assert(!anyDuplicated(runs_u$RNG_Seed), "Stored component RNG seeds are duplicated.")
for (i in seq_len(nrow(runs_u))) {
  x <- intervals_u$ISI_u[intervals_u$Run_ID == runs_u$Run_ID[i]]
  h <- digest::digest(paste(sprintf("%.10f", x), collapse = "|"), algo = "sha256", serialize = FALSE)
  assert(identical(h, runs_u$Local_ISI_SHA256[i]), paste("Local component hash mismatch", runs_u$Run_ID[i]))
}

# Counterfactual component isolation. Absolute downstream offsets may move;
# unrelated component-local ISI hashes must remain identical.
source(file.path(root, "R", "10_point_process.R"), local = .GlobalEnv)
source(file.path(root, "R", "20_template_generation.R"), local = .GlobalEnv)
assert(setequal(unique(unlist(parameters$rng_contract$pulse_streams, use.names = FALSE)),
                parameters$rng_contract$streams), "Allowed and context-used RNG streams differ.")
registry_streams <- function(registry) {
  entries <- strsplit(registry, ";", fixed = TRUE)[[1L]]
  sub("=.*$", "", entries)
}
for (i in seq_len(nrow(runs_u))) {
  expected <- if (runs_u$Component_Type[i] == "burst") {
    parameters$rng_contract$pulse_streams$standalone
  } else if (runs_u$Component_Type[i] == "hfs_with_nested_burst") {
    parameters$rng_contract$pulse_streams$hfs
  } else {
    parameters$rng_contract$simple_component_stream
  }
  assert(identical(registry_streams(runs_u$RNG_Stream_Registry[i]), expected),
         paste("Registered RNG streams do not match actual component use", runs_u$Run_ID[i]))
}
overflow_probe_attempts <- c(1L, 2L, 9999L, parameters$acceptance_contract$maximum_attempts_per_run)
for (stream in parameters$rng_contract$streams) {
  seeds <- vapply(overflow_probe_attempts, function(attempt)
    derive_stream_attempt_seed(parameters$rng_contract$seed_modulus, stream, attempt), integer(1))
  seeds_again <- vapply(overflow_probe_attempts, function(attempt)
    derive_stream_attempt_seed(parameters$rng_contract$seed_modulus, stream, attempt), integer(1))
  assert(identical(seeds, seeds_again) && all(is.finite(seeds)) &&
           all(seeds >= 1L & seeds <= parameters$rng_contract$seed_modulus),
         paste("Attempt-seed derivation is unsafe or non-deterministic", stream))
}
q <- make_frozen_burst_quota_table()
base <- generate_template_v23(6L, q)
parameters$tonic$rate_overlap_mean_isi_B$generic_stress$deep <- c(0.40, 0.50)
changed_tonic <- generate_template_v23(6L, q)
cols <- c("Component_Type", "Component_Slot", "Local_ISI_SHA256")
assert(identical(base$runs[base$runs$Component_Type != "tonic", cols],
                 changed_tonic$runs[changed_tonic$runs$Component_Type != "tonic", cols]),
       "Tonic perturbation changed another component sequence.")
source(file.path(root, "R", "00_parameters.R"), local = .GlobalEnv)
parameters$broad_hfs$rate_overlap_strata$core <- c(0.20, 0.27)
changed_hfs <- generate_template_v23(6L, q)
assert(identical(base$runs[!base$runs$Component_Type %in% c("hfs", "hfs_with_nested_burst"), cols],
                 changed_hfs$runs[!changed_hfs$runs$Component_Type %in% c("hfs", "hfs_with_nested_burst"), cols]),
       "HFS perturbation changed a non-HFS component sequence.")
source(file.path(root, "R", "00_parameters.R"), local = .GlobalEnv)
parameters$borderline_gap$gap_B <- c(0.80, 0.90)
changed_gap <- generate_template_v23(6L, q)
assert(identical(base$runs[base$runs$Component_Type != "borderline_gap", cols],
                 changed_gap$runs[changed_gap$runs$Component_Type != "borderline_gap", cols]),
       "Borderline-gap perturbation changed another component sequence.")

assert(all(table(calibration$Scale_Factor, calibration$Calibration_Mode) == 10L), "Calibration counts failed.")
for (mode in unique(calibration$Calibration_Mode)) {
  sets <- lapply(c(1L, 4L, 10L), function(scale) sort(calibration$Episode_ID[
    calibration$Calibration_Mode == mode & calibration$Scale_Factor == scale]))
  assert(identical(sets[[1]], sets[[2]]) && identical(sets[[1]], sets[[3]]), paste("Calibration ID mismatch", mode))
}
assert(all(baseline$Threshold_Fit_Split == "development_templates_1_to_5" &
             baseline$Evaluation_Split == "holdout_templates_6_to_20" &
             baseline$Acceptance_Target == "none_descriptive_audit_only"), "Baseline provenance failed.")
assert(all(is.finite(baseline$Threshold_u)) && all(baseline$Holdout_Template_Cluster_Bootstrap_CI_Lower <=
                                                   baseline$Holdout_Template_Cluster_Bootstrap_CI_Upper),
       "Single-ISI baseline values are invalid.")
assert(!holdout_provenance$holdout_performance_metrics_used_for_parameter_or_model_selection &&
         holdout_provenance$prespecified_structural_integrity_checks_applied_to_all_templates &&
         !holdout_provenance$detector_output_used,
       "Holdout performance protection and universal structural checks are not distinguished.")

xlsx_path <- file.path(root, "detector_inputs", "spike_timestamps_blinded.xlsx")
expected_sheets <- names(unlist(p$workbook_blind_batches, use.names = TRUE))
assert(identical(readxl::excel_sheets(xlsx_path), expected_sheets), "Unexpected workbook sheets.")
xlsx <- do.call(rbind, lapply(expected_sheets, function(sheet) as.data.frame(readxl::read_xlsx(xlsx_path, sheet = sheet))))
xlsx <- xlsx[order(xlsx$Sample_ID, xlsx$Spike_Index), ]; detector2 <- detector[order(detector$Sample_ID, detector$Spike_Index), ]
assert(identical(as.character(xlsx$Sample_ID), as.character(detector2$Sample_ID)), "XLSX sample mismatch.")
assert(isTRUE(all.equal(xlsx$Time_s, detector2$Time_s, tolerance = 0)), "XLSX timestamps mismatch.")
required_figures <- file.path(root, "qc", "figures", c("all_dimensionless_templates_multitrack.png",
  "paired_scale_multitrack_overview.png", "phenotype_evidence_audit.png", "dimensionless_isi_overlap_ecdf.png"))
assert(all(file.exists(required_figures) & file.info(required_figures)$size > 0), "A required local R figure is missing.")
manifest_path <- file.path(root, "metadata", "reproduction_manifest.json")
manifest <- jsonlite::read_json(manifest_path, simplifyVector = TRUE)
generator_path <- file.path(root, manifest$generator_file)
assert(identical(digest::digest(file = generator_path, algo = "sha256"), manifest$generator_sha256),
       "Generator SHA-256 does not match the reproduction manifest.")
assert(identical(sort(names(manifest$module_sha256)), sort(manifest$module_files)),
       "Manifest module-hash keys are not portable module basenames.")
for (module_name in manifest$module_files) {
  assert(identical(digest::digest(file = file.path(root, "R", module_name), algo = "sha256"),
                   unname(manifest$module_sha256[[module_name]])), paste("Module SHA-256 mismatch", module_name))
}
assert(!grepl("/Users/|/private/|/var/", paste(readLines(manifest_path, warn = FALSE), collapse = "")),
       "Reproduction manifest leaks an absolute local path.")
assert(identical(manifest$hfs_support_contract_version, parameters$broad_hfs$support_contract_version) &&
         manifest$hfs_direct_support_max_isi_B == parameters$broad_hfs$direct_support_max_isi_B &&
         manifest$legacy_v2_2_direct_support_audit_max_isi_B ==
           parameters$broad_hfs$legacy_v2_2_direct_support_max_isi_B &&
         isTRUE(manifest$cross_version_direct_support_f1_requires_common_mask),
       "The reproduction manifest omits the versioned HFS support contract.")
assert(!manifest$holdout_performance_metrics_used_for_parameter_or_model_selection &&
         manifest$prespecified_structural_integrity_checks_applied_to_all_templates,
       "The manifest conflates structural integrity checks with holdout performance selection.")
checksums <- read.csv(file.path(root, "metadata", "canonical_output_checksums_sha256.csv"))
for (i in seq_len(nrow(checksums))) assert(digest::digest(file = file.path(root, checksums$File[i]), algo = "sha256") == checksums$SHA256[i],
                                           paste("Checksum mismatch", checksums$File[i]))
release_lock_path <- file.path(root, "RELEASE_LOCK_SHA256.csv")
assert(file.exists(release_lock_path), "The immutable release hash lock is missing.")
release_lock <- read.csv(release_lock_path, check.names = FALSE)
assert(identical(names(release_lock), c("File", "SHA256", "Purpose")) && nrow(release_lock) > 0L &&
         !anyDuplicated(release_lock$File), "The release hash lock schema or file keys are invalid.")
assert(!any(grepl("^/|(^|/)\\.\\.(/|$)", release_lock$File)), "The release hash lock contains a non-portable path.")
required_lock_entries <- c(
  "generate_benchmark_spike_trains.R", "R/00_parameters.R", "R/10_point_process.R",
  "R/20_template_generation.R", "R/30_phenotype_audit.R", "R/40_exports_and_figures.R",
  "tests/verify_v2_3.R", "run_reproduction.sh", "metadata/generator_parameters.json",
  "detector_inputs/spike_timestamps_blinded.csv", "detector_inputs/spike_timestamps_blinded.xlsx",
  "truth/interval_truth_multitrack.csv", "truth/primary_event_episodes.csv",
  "truth/state_envelopes.csv", "truth/phenotype_sensitivity_masks.csv",
  "truth/observable_burst_positive_episodes.csv", "truth/null_model_burst_like_excursions.csv",
  "qc/figures/all_dimensionless_templates_multitrack.png",
  "qc/figures/paired_scale_multitrack_overview.png", "qc/figures/dimensionless_isi_overlap_ecdf.png")
assert(all(required_lock_entries %in% release_lock$File), "The release hash lock omits a required source or artifact.")
for (i in seq_len(nrow(release_lock))) {
  locked_path <- file.path(root, release_lock$File[i])
  assert(file.exists(locked_path), paste("Release-locked file is missing", release_lock$File[i]))
  assert(identical(digest::digest(file = locked_path, algo = "sha256"), release_lock$SHA256[i]),
         paste("Release-lock drift", release_lock$File[i]))
}
generator_text <- paste(readLines(file.path(root, "generate_benchmark_spike_trains.R"), warn = FALSE), collapse = "\n")
assert(!grepl("RELEASE_LOCK_SHA256[.]csv", generator_text), "The generator must never rewrite the release lock.")
cat("All v2.3 ontology, overlap, Burst flank/contrast, phenotype, RNG-isolation, scale, calibration, XLSX, checksum and release-lock checks passed.\n")
