#!/usr/bin/env Rscript

# Frozen v2.3 clean-overlap supplement generator.
# All timestamps, workbooks and figures are produced locally by this R program.

options(stringsAsFactors = FALSE, warn = 1)
RNGkind("Mersenne-Twister", "Inversion", "Rejection")
script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_arg) != 1L) stop("Run with Rscript.", call. = FALSE)
script_path <- normalizePath(gsub("~\\+~", " ", sub("^--file=", "", script_arg)), mustWork = TRUE)
output_dir <- dirname(script_path)

required_packages <- c("ggplot2", "jsonlite", "yaml", "digest", "openxlsx", "readxl")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) stop("Missing R packages: ", paste(missing_packages, collapse = ", "), call. = FALSE)
module_files <- file.path(output_dir, "R", c("00_parameters.R", "10_point_process.R", "20_template_generation.R",
                                               "30_phenotype_audit.R", "40_exports_and_figures.R"))
if (!all(file.exists(module_files))) stop("One or more generator modules are missing.", call. = FALSE)
invisible(lapply(module_files, source, local = .GlobalEnv))

for (subdir in c("detector_inputs", "truth", "metadata", "qc", "qc/figures", "tests", "tables", "calibration"))
  dir.create(file.path(output_dir, subdir), recursive = TRUE, showWarnings = FALSE)
bind_nonempty <- function(items) {
  items <- items[vapply(items, function(x) is.data.frame(x) && nrow(x) > 0L, logical(1))]
  if (!length(items)) data.frame() else do.call(rbind, items)
}

burst_quota <- make_frozen_burst_quota_table()
templates <- lapply(seq_len(parameters$n_templates), generate_template_v23, burst_quota = burst_quota)
spikes_u <- bind_nonempty(lapply(templates, `[[`, "spikes"))
intervals_raw_u <- bind_nonempty(lapply(templates, `[[`, "intervals"))
events_u <- bind_nonempty(lapply(templates, `[[`, "events"))
states_u <- bind_nonempty(lapply(templates, `[[`, "states"))
runs_u <- bind_nonempty(lapply(templates, `[[`, "runs"))
separators_u <- bind_nonempty(lapply(templates, `[[`, "separators"))
borderline_u <- bind_nonempty(lapply(templates, `[[`, "borderline"))
contextual_links_u <- bind_nonempty(lapply(templates, `[[`, "contextual"))
transitions_u <- bind_nonempty(lapply(templates, `[[`, "transitions"))
regimes_u <- bind_nonempty(lapply(templates, `[[`, "regimes"))
template_manifest <- bind_nonempty(lapply(templates, `[[`, "quota"))

phenotype_thresholds <- fit_phenotype_thresholds(states_u, runs_u, events_u)
phenotype_result <- audit_phenotypes(intervals_raw_u, events_u, states_u, runs_u, phenotype_thresholds)
intervals_u <- phenotype_result$intervals; phenotypes_u <- phenotype_result$evidence
tonic_phenotypes_u <- audit_tonic_phenotypes(intervals_u, states_u)
states_u <- audit_hfs_boundaries(intervals_u, states_u)
if (nrow(tonic_phenotypes_u)) {
  tonic_match <- match(states_u$State_Envelope_ID, tonic_phenotypes_u$State_Envelope_ID)
  states_u$Tonic_Phenotype_Evidence_Score <- tonic_phenotypes_u$Tonic_Phenotype_Evidence_Score[tonic_match]
  states_u$Tonic_Phenotype <- tonic_phenotypes_u$Tonic_Phenotype[tonic_match]
  states_u$Eligibility_Reason <- tonic_phenotypes_u$Eligibility_Reason[tonic_match]
  states_u$Tonic_Phenotype[states_u$State_Label != "Tonic"] <- "not_applicable"
  states_u$Eligibility_Reason[states_u$State_Label != "Tonic"] <- "not_applicable"
  intervals_u$Tonic_Phenotype <- "not_applicable"
  intervals_u$Tonic_Phenotype_Evidence_Score <- NA_real_
  intervals_u$Tonic_Eligibility_Reason <- "not_applicable"
  for (i in seq_len(nrow(tonic_phenotypes_u))) {
    idx <- intervals_u$State_Envelope_ID == tonic_phenotypes_u$State_Envelope_ID[i]
    intervals_u$Tonic_Phenotype[idx] <- tonic_phenotypes_u$Tonic_Phenotype[i]
    intervals_u$Tonic_Phenotype_Evidence_Score[idx] <- tonic_phenotypes_u$Tonic_Phenotype_Evidence_Score[i]
    intervals_u$Tonic_Eligibility_Reason[idx] <- tonic_phenotypes_u$Eligibility_Reason[i]
  }
}
event_pheno <- phenotypes_u[phenotypes_u$Mechanism_Event_ID != "none", ]
events_u$Phenotype_Evidence_ID <- event_pheno$Phenotype_Evidence_ID[match(events_u$Event_ID, event_pheno$Mechanism_Event_ID)]
events_u$Phenotype_Label <- event_pheno$Phenotype_Label[match(events_u$Event_ID, event_pheno$Mechanism_Event_ID)]
events_u$Phenotype_Evidence_Score <- event_pheno$Evidence_Score[match(events_u$Event_ID, event_pheno$Mechanism_Event_ID)]
events_u$Phenotype_Evaluation_Eligibility <- event_pheno$Phenotype_Estimand_Eligibility[match(events_u$Event_ID, event_pheno$Mechanism_Event_ID)]
events_u$Phenotype_Evidence_ID[is.na(events_u$Phenotype_Evidence_ID)] <- "none"
events_u$Phenotype_Label[is.na(events_u$Phenotype_Label)] <- "not_applicable"
events_u$Phenotype_Evaluation_Eligibility[is.na(events_u$Phenotype_Evaluation_Eligibility)] <- "not_applicable"

single_isi_baseline <- make_dev_holdout_single_isi_baseline(intervals_u)

sample_key <- make_sample_key_v23(template_manifest)
spikes <- scale_table_v23(spikes_u, sample_key, "Time_u")
intervals <- scale_table_v23(intervals_u, sample_key, c("Start_u", "End_u", "ISI_u"))
events <- scale_table_v23(events_u, sample_key, c("Latent_Start_u", "Latent_End_u", "Latent_Duration_u",
                                                  "Realized_Start_u", "Realized_End_u", "Realized_Duration_u"))
states <- scale_table_v23(states_u, sample_key, c("Start_u", "End_u", "Duration_u", "Mean_ISI_Parameter_u",
                                                  "Refractory_u", "Observed_Mean_ISI_u"))
runs <- scale_table_v23(runs_u, sample_key, c("Start_u", "End_u", "Duration_u", "Mean_ISI_Parameter_u",
                                              "Refractory_u", "Observed_Mean_ISI_u"))
separators <- scale_table_v23(separators_u, sample_key, c("Start_u", "End_u", "Duration_u"))
borderline <- scale_table_v23(borderline_u, sample_key, c("Start_u", "End_u", "Duration_u"))
contextual_links <- scale_table_v23(contextual_links_u, sample_key,
                                    c("Left_Local_Median_ISI_u", "Right_Local_Median_ISI_u", "Separator_ISI_u"))
transitions <- scale_table_v23(transitions_u, sample_key, "Boundary_u")
regimes <- scale_table_v23(regimes_u, sample_key, c("Start_u", "End_u"))
tonic_phenotypes <- scale_table_v23(tonic_phenotypes_u, sample_key,
                                    c("Duration_u", "Median_ISI_u", "Local_Left_Median_ISI_u", "Local_Right_Median_ISI_u"))
phenotypes <- scale_table_v23(phenotypes_u, sample_key,
                              c("Start_u", "End_u", "Duration_u", "Internal_Median_ISI_u", "Flank_Median_ISI_u"))

detector_input <- spikes[, c("Sample_ID", "Spike_Index", "Time_s")]
detector_input <- detector_input[order(detector_input$Sample_ID, detector_input$Spike_Index), ]; rownames(detector_input) <- NULL
calibration <- select_calibration_v23(events_u, states_u, sample_key)
contextual_calibration <- merge(separators_u[separators_u$Template_ID %in% parameters$development_templates,
                                             c("Template_ID", "Contextual_Separator_ID", "Start_u", "End_u", "Duration_u")],
                                sample_key, by = "Template_ID", sort = FALSE)
contextual_calibration$Start_s <- contextual_calibration$Start_u * contextual_calibration$B_s
contextual_calibration$End_s <- contextual_calibration$End_u * contextual_calibration$B_s
contextual_calibration$Duration_s <- contextual_calibration$Duration_u * contextual_calibration$B_s

write.csv(detector_input, file.path(output_dir, "detector_inputs", "spike_timestamps_blinded.csv"), row.names = FALSE)
write_detector_workbook_v23(detector_input, sample_key, file.path(output_dir, "detector_inputs", "spike_timestamps_blinded.xlsx"))
write.csv(sample_key, file.path(output_dir, "truth", "sample_template_scale_key.csv"), row.names = FALSE)
write.csv(intervals, file.path(output_dir, "truth", "interval_truth_multitrack.csv"), row.names = FALSE, na = "")
write.csv(events, file.path(output_dir, "truth", "primary_event_episodes.csv"), row.names = FALSE, na = "")
write.csv(states, file.path(output_dir, "truth", "state_envelopes.csv"), row.names = FALSE, na = "")
write.csv(regimes, file.path(output_dir, "truth", "composite_hfs_regimes.csv"), row.names = FALSE, na = "")
write.csv(separators, file.path(output_dir, "truth", "contextual_separator_secondary_episodes.csv"), row.names = FALSE, na = "")
write.csv(borderline, file.path(output_dir, "truth", "borderline_gap_secondary_episodes.csv"), row.names = FALSE, na = "")
write.csv(contextual_links, file.path(output_dir, "truth", "contextual_separator_links.csv"), row.names = FALSE, na = "")
write.csv(transitions, file.path(output_dir, "truth", "transition_boundaries.csv"), row.names = FALSE, na = "")
write.csv(runs, file.path(output_dir, "truth", "generator_provenance.csv"), row.names = FALSE, na = "")
write.csv(phenotypes, file.path(output_dir, "truth", "observable_phenotype.csv"), row.names = FALSE, na = "")
burst_observable_episode_audit <- phenotypes
burst_observable_episode_audit$Scoring_Role <- ifelse(
  burst_observable_episode_audit$Candidate_Origin == "injected_rate_pulse" &
    burst_observable_episode_audit$Phenotype_Label == "clear_burst_like", "observable_primary_positive",
  ifelse(burst_observable_episode_audit$Candidate_Origin == "injected_rate_pulse" &
           burst_observable_episode_audit$Phenotype_Label == "ambiguous_burst_like", "ambiguous_sensitivity_only",
  ifelse(burst_observable_episode_audit$Candidate_Origin == "injected_rate_pulse", "strict_only_no_independent_evidence",
  ifelse(burst_observable_episode_audit$Phenotype_Label == "clear_burst_like", "null_model_clear_excursion_secondary_audit",
  ifelse(burst_observable_episode_audit$Phenotype_Label == "ambiguous_burst_like", "null_model_ambiguous_excursion_secondary_audit",
         "null_model_no_excursion")))))
burst_observable_episode_audit$Primary_Observable_Burst_Positive <-
  burst_observable_episode_audit$Scoring_Role == "observable_primary_positive"
write.csv(burst_observable_episode_audit, file.path(output_dir, "truth", "burst_observable_episode_audit.csv"),
          row.names = FALSE, na = "")
write.csv(burst_observable_episode_audit[burst_observable_episode_audit$Primary_Observable_Burst_Positive, , drop = FALSE],
          file.path(output_dir, "truth", "observable_burst_positive_episodes.csv"), row.names = FALSE, na = "")
write.csv(burst_observable_episode_audit[
  burst_observable_episode_audit$Candidate_Origin == "null_model_burst_like_excursion", , drop = FALSE],
  file.path(output_dir, "truth", "null_model_burst_like_excursions.csv"), row.names = FALSE, na = "")
write.csv(tonic_phenotypes, file.path(output_dir, "truth", "tonic_phenotype_evidence.csv"), row.names = FALSE, na = "")
write.csv(events, file.path(output_dir, "truth", "strict_mechanism_estimand_episodes.csv"), row.names = FALSE, na = "")
write.csv(phenotypes, file.path(output_dir, "truth", "phenotype_estimand_regions.csv"), row.names = FALSE, na = "")
write.csv(states, file.path(output_dir, "truth", "strict_mechanism_estimand_states.csv"), row.names = FALSE, na = "")
observable_states <- states[states$State_Label == "Broad_HFS" |
                              (states$State_Label == "Tonic" & states$Tonic_Phenotype == "eligible"), , drop = FALSE]
write.csv(observable_states, file.path(output_dir, "truth", "observable_phenotype_estimand_states.csv"), row.names = FALSE, na = "")
sensitivity_masks <- intervals[, c("Sample_ID", "Template_ID", "Scale_Factor", "B_s", "Interval_Index",
                                    "Start_s", "End_s", "ISI_s")]
sensitivity_masks$Burst_Strict_Mechanism_Positive <- intervals$Event_Label == "Burst"
sensitivity_masks$Burst_Observable_Clear_Positive <- intervals$Phenotype_Candidate_Origin == "injected_rate_pulse" &
  intervals$Phenotype_Label == "clear_burst_like"
sensitivity_masks$Burst_Ambiguous_Mask <- intervals$Phenotype_Candidate_Origin == "injected_rate_pulse" &
  intervals$Phenotype_Label == "ambiguous_burst_like"
sensitivity_masks$Burst_Ambiguous_As_Positive <- intervals$Phenotype_Candidate_Origin == "injected_rate_pulse" &
  intervals$Phenotype_Label %in% c("clear_burst_like", "ambiguous_burst_like")
sensitivity_masks$Burst_Null_Model_Clear_Excursion <- intervals$Phenotype_Candidate_Origin == "null_model_burst_like_excursion" &
  intervals$Phenotype_Label == "clear_burst_like"
sensitivity_masks$Burst_Null_Model_Ambiguous_Excursion <- intervals$Phenotype_Candidate_Origin == "null_model_burst_like_excursion" &
  intervals$Phenotype_Label == "ambiguous_burst_like"
sensitivity_masks$Tonic_Strict_Mechanism_Positive <- intervals$State_Label == "Tonic"
sensitivity_masks$Tonic_Observable_Eligible_Positive <- intervals$State_Label == "Tonic" & intervals$Tonic_Phenotype == "eligible"
sensitivity_masks$Tonic_Ambiguous_Mask <- intervals$State_Label == "Tonic" & intervals$Tonic_Phenotype == "ambiguous"
sensitivity_masks$Tonic_Ambiguous_As_Positive <- intervals$State_Label == "Tonic" & intervals$Tonic_Phenotype %in% c("eligible", "ambiguous")
sensitivity_masks$Broad_HFS_Strict_Positive <- intervals$State_Label == "Broad_HFS"
sensitivity_masks$Broad_HFS_Direct_Support_v2_3 <- intervals$Direct_HFS_Support
sensitivity_masks$Broad_HFS_Legacy_v2_2_Direct_Support_Audit <-
  intervals$Legacy_V2_2_Direct_HFS_Support_Audit
sensitivity_masks$Pause_Primary_Strict_Positive <- intervals$Event_Label == "Pause"
sensitivity_masks$Secondary_Pause_Semantic_Mask <- intervals$Secondary_Event_Label != "none"
write.csv(sensitivity_masks, file.path(output_dir, "truth", "phenotype_sensitivity_masks.csv"), row.names = FALSE, na = "")
write.csv(calibration, file.path(output_dir, "calibration", "calibration_same_episode_ids_all_scales.csv"), row.names = FALSE)
write.csv(contextual_calibration, file.path(output_dir, "calibration", "contextual_separator_secondary_only.csv"), row.names = FALSE)

write.csv(spikes_u, file.path(output_dir, "tables", "dimensionless_spike_templates.csv"), row.names = FALSE)
write.csv(intervals_u, file.path(output_dir, "tables", "dimensionless_interval_templates.csv"), row.names = FALSE, na = "")
write.csv(events_u, file.path(output_dir, "tables", "dimensionless_primary_event_episodes.csv"), row.names = FALSE, na = "")
write.csv(states_u, file.path(output_dir, "tables", "dimensionless_state_envelopes.csv"), row.names = FALSE, na = "")
write.csv(runs_u, file.path(output_dir, "tables", "dimensionless_generator_runs.csv"), row.names = FALSE, na = "")
write.csv(regimes_u, file.path(output_dir, "tables", "dimensionless_composite_hfs_regimes.csv"), row.names = FALSE, na = "")
write.csv(tonic_phenotypes_u, file.path(output_dir, "tables", "dimensionless_tonic_phenotype_evidence.csv"), row.names = FALSE, na = "")
write.csv(separators_u, file.path(output_dir, "tables", "dimensionless_contextual_separators.csv"), row.names = FALSE, na = "")
write.csv(borderline_u, file.path(output_dir, "tables", "dimensionless_borderline_gaps.csv"), row.names = FALSE, na = "")
write.csv(phenotypes_u, file.path(output_dir, "tables", "dimensionless_observable_phenotype.csv"), row.names = FALSE, na = "")

write.csv(template_manifest, file.path(output_dir, "metadata", "template_manifest.csv"), row.names = FALSE)
write.csv(burst_quota, file.path(output_dir, "metadata", "frozen_burst_quota.csv"), row.names = FALSE)
tonic_overlap_quota <- parameters$tonic$rate_overlap_stratum_by_template
hfs_overlap_quota <- states_u[states_u$State_Label == "Broad_HFS",
                              c("Template_ID", "State_Envelope_ID", "Regularity_Regime",
                                "HFS_Rate_Overlap_Stratum", "Contains_Injected_Burst")]
tonic_overlap_long <- do.call(rbind, lapply(c("generic_stress", "stn_like_empirical"), function(subtype) {
  data.frame(Template_ID = tonic_overlap_quota$Template_ID, Component = "Tonic", Subtype = subtype,
             Regularity_Regime = subtype,
             Rate_Overlap_Stratum = tonic_overlap_quota[[subtype]], Contains_Injected_Burst = FALSE,
             stringsAsFactors = FALSE)
}))
hfs_overlap_long <- data.frame(Template_ID = hfs_overlap_quota$Template_ID, Component = "Broad_HFS",
                               Subtype = "not_applicable", Regularity_Regime = hfs_overlap_quota$Regularity_Regime,
                               Rate_Overlap_Stratum = hfs_overlap_quota$HFS_Rate_Overlap_Stratum,
                               Contains_Injected_Burst = hfs_overlap_quota$Contains_Injected_Burst,
                               stringsAsFactors = FALSE)
frozen_overlap_quota <- rbind(tonic_overlap_long, hfs_overlap_long)
frozen_overlap_quota$Split <- ifelse(frozen_overlap_quota$Template_ID %in% parameters$development_templates,
                                     "development", "holdout")
write.csv(frozen_overlap_quota, file.path(output_dir, "metadata", "frozen_overlap_quota.csv"), row.names = FALSE)
write.csv(phenotype_thresholds, file.path(output_dir, "metadata", "phenotype_thresholds.csv"), row.names = FALSE)
rng_registry <- runs_u[, c("Template_ID", "Run_ID", "Component_Type", "Component_Slot", "RNG_Seed",
                           "RNG_Stream_Registry", "Local_ISI_SHA256", "N_ISIs", "Duration_u")]
write.csv(rng_registry, file.path(output_dir, "metadata", "component_rng_registry.csv"), row.names = FALSE)
stn_summary <- data.frame(Source_SHA256 = parameters$tonic$stn_like_empirical$source_sha256,
                          Labeled_Tonic_ISIs = parameters$tonic$stn_like_empirical$source_isi_n,
                          Contiguous_Runs = parameters$tonic$stn_like_empirical$source_contiguous_run_n,
                          ISI_q05_s = parameters$tonic$stn_like_empirical$source_q05_isi_s,
                          ISI_median_s = parameters$tonic$stn_like_empirical$source_median_isi_s,
                          ISI_q95_s = parameters$tonic$stn_like_empirical$source_q95_isi_s,
                          Median_Rate_Hz = 1 / parameters$tonic$stn_like_empirical$source_median_isi_s,
                          stringsAsFactors = FALSE)
write.csv(stn_summary, file.path(output_dir, "metadata", "stn_like_tonic_empirical_summary.csv"), row.names = FALSE)
jsonlite::write_json(parameters, file.path(output_dir, "metadata", "generator_parameters.json"), auto_unbox = TRUE, pretty = TRUE, digits = 15)
yaml::write_yaml(parameters, file.path(output_dir, "metadata", "generator_parameters.yaml"))
flatten_parameters <- function(x, prefix = "") {
  rows <- list()
  for (nm in names(x)) {
    key <- if (nzchar(prefix)) paste(prefix, nm, sep = ".") else nm; value <- x[[nm]]
    if (is.list(value) && !is.data.frame(value)) rows <- c(rows, flatten_parameters(value, key))
    else rows[[length(rows) + 1L]] <- data.frame(Parameter = key, Value = paste(value, collapse = " | "), stringsAsFactors = FALSE)
  }
  rows
}
write.csv(do.call(rbind, flatten_parameters(parameters)), file.path(output_dir, "metadata", "generator_parameters.csv"), row.names = FALSE)

distribution_summary <- make_distribution_summary_v23(intervals_u, events_u, states_u, separators_u, borderline_u)
write.csv(distribution_summary, file.path(output_dir, "qc", "distribution_summary.csv"), row.names = FALSE, na = "")
write.csv(phenotypes_u, file.path(output_dir, "qc", "phenotype_audit.csv"), row.names = FALSE, na = "")
write.csv(tonic_phenotypes_u, file.path(output_dir, "qc", "tonic_phenotype_audit.csv"), row.names = FALSE, na = "")
hfs_boundary_audit <- states_u[states_u$State_Label == "Broad_HFS",
                               c("Template_ID", "State_Envelope_ID", "Regularity_Regime", "HFS_Rate_Overlap_Stratum",
                                 "Contains_Injected_Burst",
                                 "Start_Boundary_Observability", "End_Boundary_Observability",
                                 "Start_Boundary_Contrast", "End_Boundary_Contrast",
                                 "Start_Hard_Cut_Reason", "End_Hard_Cut_Reason", "Composite_HFS_Regime_ID")]
write.csv(hfs_boundary_audit, file.path(output_dir, "qc", "hfs_boundary_observability.csv"), row.names = FALSE, na = "")
burst_balance <- as.data.frame(table(burst_quota$Split, burst_quota$Context, burst_quota$Strength), stringsAsFactors = FALSE)
names(burst_balance) <- c("Split", "Context", "Strength", "N")
write.csv(burst_balance, file.path(output_dir, "qc", "burst_strength_context_balance.csv"), row.names = FALSE)
hfs_coverage <- as.data.frame(table(burst_quota$Split[burst_quota$Context == "hfs"],
                                    burst_quota$HFS_Regime[burst_quota$Context == "hfs"],
                                    burst_quota$Strength[burst_quota$Context == "hfs"]), stringsAsFactors = FALSE)
names(hfs_coverage) <- c("Split", "HFS_Regime", "Strength", "N")
write.csv(hfs_coverage, file.path(output_dir, "qc", "hfs_burst_regularity_strength_coverage.csv"), row.names = FALSE)
context_strength_counts <- table(burst_quota$Context, burst_quota$Strength)
remediation <- data.frame(Check = c("Standalone_Burst_strength_min_cell_count",
                                     "Standalone_Burst_strength_max_cell_count",
                                     "HFS_Burst_strength_min_cell_count",
                                     "HFS_Burst_strength_max_cell_count",
                                     "HFS_Burst_development", "HFS_Burst_holdout",
                                     "Four_spike_Burst", "Contextual_as_primary_Pause", "Complex_Pause",
                                     "Direct_target_boundaries", "Paired_scale_templates"),
                          Value = c(min(context_strength_counts["standalone", ]), max(context_strength_counts["standalone", ]),
                                    min(context_strength_counts["hfs", ]), max(context_strength_counts["hfs", ]),
                                    sum(burst_quota$Context == "hfs" & burst_quota$Split == "development"),
                                    sum(burst_quota$Context == "hfs" & burst_quota$Split == "holdout"),
                                    sum(burst_quota$Is_Four_Spike_Boundary),
                                    sum(events_u$Pause_Subtype == "contextual_interburst_gap_separator"),
                                    sum(events_u$Pause_Subtype == "complex_multi_gap"),
                                    sum(transitions_u$Direct_Target_Boundary), parameters$n_templates), stringsAsFactors = FALSE)
write.csv(remediation, file.path(output_dir, "qc", "v2_3_audit_summary.csv"), row.names = FALSE)
structural <- data.frame(Metric = c("Minimum_observed_ISI_u", "Configured_refractory_u", "Refractory_violations",
                                    "Burst_over_10_spikes", "Exact_4_spike_Burst", "Primary_contextual_Pause_rows"),
                         Value = c(min(intervals_u$ISI_u), parameters$shared_refractory_B,
                                   sum(intervals_u$ISI_u < parameters$shared_refractory_B - 1e-12),
                                   sum(events_u$Event_Label == "Burst" & events_u$N_Realized_Spikes > 10),
                                   sum(events_u$Event_Label == "Burst" & events_u$N_Realized_Spikes == 4),
                                   sum(intervals_u$Event_Label == "Pause" & intervals_u$Contextual_Separator_Label != "none")))
write.csv(structural, file.path(output_dir, "qc", "structural_contract_audit.csv"), row.names = FALSE)
write.csv(single_isi_baseline, file.path(output_dir, "qc", "single_isi_dev_holdout_baseline.csv"), row.names = FALSE)

split_for <- function(template_id) ifelse(template_id %in% parameters$development_templates, "development", "holdout")
tonic_coverage <- as.data.frame(table(
  Split = split_for(tonic_phenotypes_u$Template_ID), Subtype = tonic_phenotypes_u$Tonic_Subtype,
  Rate_Overlap_Stratum = tonic_phenotypes_u$Tonic_Rate_Overlap_Stratum,
  Intended_Difficulty = tonic_phenotypes_u$Tonic_Intended_Stratum,
  Realized_Phenotype = tonic_phenotypes_u$Tonic_Phenotype), stringsAsFactors = FALSE)
burst_events <- events_u[events_u$Event_Label == "Burst", , drop = FALSE]
burst_coverage <- as.data.frame(table(
  Split = split_for(burst_events$Template_ID), Context = burst_events$Event_Context,
  Strength = burst_events$Pulse_Strength_Stratum,
  HFS_Regularity = ifelse(burst_events$Event_Context == "burst_in_hfs",
                          states_u$Regularity_Regime[match(burst_events$State_Envelope_ID, states_u$State_Envelope_ID)],
                          "not_applicable"),
  Phenotype = burst_events$Phenotype_Label), stringsAsFactors = FALSE)
burst_spikecount_coverage <- as.data.frame(table(
  Split = split_for(burst_events$Template_ID), Context = burst_events$Event_Context,
  Strength = burst_events$Pulse_Strength_Stratum, N_Realized_Spikes = burst_events$N_Realized_Spikes,
  Phenotype = burst_events$Phenotype_Label), stringsAsFactors = FALSE)
hfs_states_for_coverage <- states_u[states_u$State_Label == "Broad_HFS", , drop = FALSE]
hfs_coverage_detail <- as.data.frame(table(
  Split = split_for(hfs_overlap_quota$Template_ID),
  Rate_Overlap_Stratum = hfs_overlap_quota$HFS_Rate_Overlap_Stratum,
  Regularity = hfs_states_for_coverage$Regularity_Regime,
  Contains_Injected_Burst = hfs_states_for_coverage$Contains_Injected_Burst,
  Start_Boundary_Observability = hfs_states_for_coverage$Start_Boundary_Observability,
  End_Boundary_Observability = hfs_states_for_coverage$End_Boundary_Observability), stringsAsFactors = FALSE)
pause_coverage <- as.data.frame(table(
  Split = c(split_for(events_u$Template_ID[events_u$Event_Label == "Pause"]),
            split_for(borderline_u$Template_ID)),
  Truth_Level = c(paste0("primary_", events_u$Pause_Subtype[events_u$Event_Label == "Pause"]),
                  rep("secondary_borderline_gap", nrow(borderline_u)))), stringsAsFactors = FALSE)
tonic_coverage$Audit_Domain <- "Tonic"
burst_coverage$Audit_Domain <- "Burst"
hfs_coverage_detail$Audit_Domain <- "Broad_HFS"
pause_coverage$Audit_Domain <- "Pause_and_secondary_gap"
write.csv(tonic_coverage, file.path(output_dir, "qc", "tonic_phenotype_coverage_by_split.csv"), row.names = FALSE)
write.csv(burst_coverage, file.path(output_dir, "qc", "burst_phenotype_coverage_by_split.csv"), row.names = FALSE)
write.csv(burst_spikecount_coverage, file.path(output_dir, "qc", "burst_context_strength_spikecount_phenotype.csv"), row.names = FALSE)
write.csv(hfs_coverage_detail, file.path(output_dir, "qc", "hfs_overlap_coverage_by_split.csv"), row.names = FALSE)
write.csv(pause_coverage, file.path(output_dir, "qc", "pause_gap_coverage_by_split.csv"), row.names = FALSE)

overlap_contract <- data.frame(
  Check = c("Tonic_HFS_same_renewal_family", "Tonic_HFS_same_refractory",
            "Canonical_pause_lower_unchanged", "Borderline_gap_below_canonical",
            "Borderline_gap_is_secondary_only", "Each_template_has_all_HFS_overlap_strata",
            "Detector_output_used_for_acceptance"),
  Value = c(parameters$renewal_family == "shifted_gamma",
            all(states_u$Refractory_u[states_u$State_Label %in% c("Tonic", "Broad_HFS")] == parameters$shared_refractory_B),
            parameters$canonical_pause$gap_B[1] == 2.8,
            max(borderline_u$Duration_u) < parameters$canonical_pause$gap_B[1],
            all(borderline_u$Primary_Event_Label == "none"),
            all(vapply(split(states_u$HFS_Rate_Overlap_Stratum[states_u$State_Label == "Broad_HFS"],
                             states_u$Template_ID[states_u$State_Label == "Broad_HFS"]),
                       function(z) identical(sort(unique(z)), sort(c("core", "boundary", "deep"))), logical(1))),
            FALSE), stringsAsFactors = FALSE)
write.csv(overlap_contract, file.path(output_dir, "qc", "overlap_contract_audit.csv"), row.names = FALSE)
holdout_audit_provenance <- list(
  dataset_version = parameters$dataset_version,
  threshold_fit_templates = parameters$development_templates,
  holdout_evaluation_templates = parameters$holdout_templates,
  threshold_fit_statistic = "template_equal_balanced_accuracy",
  threshold_tie_break = parameters$dev_holdout_single_isi_baseline$threshold_tie_break,
  bootstrap_cluster = "Template_ID",
  bootstrap_replicates = parameters$dev_holdout_single_isi_baseline$bootstrap_replicates,
  scale_use = "dimensionless_mother_templates_only",
  detector_output_used = FALSE,
  holdout_performance_metrics_used_for_parameter_or_model_selection = FALSE,
  prespecified_structural_integrity_checks_applied_to_all_templates = TRUE,
  changing_generator_after_holdout_review_requires_new_version_and_new_holdout = TRUE)
jsonlite::write_json(holdout_audit_provenance, file.path(output_dir, "metadata", "holdout_audit_provenance.json"),
                     auto_unbox = TRUE, pretty = TRUE)
make_plots_v23(intervals, spikes, phenotypes_u, burst_quota, output_dir)

canonical_files <- c("detector_inputs/spike_timestamps_blinded.csv", "detector_inputs/spike_timestamps_blinded.xlsx",
                     "truth/interval_truth_multitrack.csv", "truth/primary_event_episodes.csv", "truth/state_envelopes.csv",
                     "truth/composite_hfs_regimes.csv", "truth/tonic_phenotype_evidence.csv",
                     "truth/contextual_separator_secondary_episodes.csv", "truth/borderline_gap_secondary_episodes.csv",
                     "truth/observable_phenotype.csv", "truth/burst_observable_episode_audit.csv",
                     "truth/observable_burst_positive_episodes.csv", "truth/null_model_burst_like_excursions.csv",
                     "truth/phenotype_sensitivity_masks.csv",
                     "truth/sample_template_scale_key.csv", "metadata/frozen_burst_quota.csv", "metadata/generator_parameters.json",
                     "metadata/phenotype_thresholds.csv", "calibration/calibration_same_episode_ids_all_scales.csv",
                     "metadata/component_rng_registry.csv", "metadata/frozen_overlap_quota.csv",
                     "metadata/holdout_audit_provenance.json",
                     "qc/v2_3_audit_summary.csv", "qc/overlap_contract_audit.csv",
                     "qc/single_isi_dev_holdout_baseline.csv", "tables/dimensionless_spike_templates.csv",
                     "tables/dimensionless_interval_templates.csv", "tables/dimensionless_borderline_gaps.csv")
checksums <- data.frame(File = canonical_files,
                        SHA256 = vapply(canonical_files, function(path) digest::digest(file = file.path(output_dir, path), algo = "sha256"), character(1)))
write.csv(checksums, file.path(output_dir, "metadata", "canonical_output_checksums_sha256.csv"), row.names = FALSE)
module_hashes <- vapply(module_files, function(x) digest::digest(file = x, algo = "sha256"), character(1))
names(module_hashes) <- basename(module_files)
manifest <- list(dataset_id = parameters$dataset_id, dataset_version = parameters$dataset_version,
                 created_utc = "2026-08-29T00:00:00Z", generator_file = basename(script_path),
                 generator_sha256 = digest::digest(file = script_path, algo = "sha256"),
                 module_files = basename(module_files),
                 module_sha256 = as.list(module_hashes),
                 r_version = R.version.string, rng_kind = RNGkind(),
                 package_versions = as.list(vapply(required_packages, function(pkg) as.character(utils::packageVersion(pkg)), character(1))),
                 n_dimensionless_templates = parameters$n_templates, n_scaled_samples = nrow(sample_key),
                 exact_scale_factors = parameters$scale_factors, scales_are_paired_not_independent = TRUE,
                 same_episode_ids_used_for_calibration_across_scales = TRUE,
                 component_rng_isolated = TRUE, rng_seed_key_excludes_dataset_version = TRUE,
                 tonic_strict_and_observable_estimands_exported = TRUE,
                 burst_observable_primary_is_injected_clear_only = TRUE,
                 null_model_burst_excursions_are_secondary_audit_only = TRUE,
                 canonical_pause_hard_cuts_hfs_state = TRUE, composite_hfs_regime_is_not_continuous_state = TRUE,
                 hfs_support_contract_version = parameters$broad_hfs$support_contract_version,
                 hfs_direct_support_max_isi_B = parameters$broad_hfs$direct_support_max_isi_B,
                 legacy_v2_2_direct_support_audit_max_isi_B = parameters$broad_hfs$legacy_v2_2_direct_support_max_isi_B,
                 cross_version_direct_support_f1_requires_common_mask = TRUE,
                 detector_input_columns = names(detector_input), detector_output_used_for_generation_or_selection = FALSE,
                 contextual_separator_is_primary_pause_truth = FALSE, stimulation_enabled = FALSE,
                 borderline_gap_is_primary_pause_truth = FALSE,
                 supplement_role = parameters$supplement_role,
                 holdout_was_not_used_for_threshold_fit = TRUE,
                 holdout_performance_metrics_used_for_parameter_or_model_selection = FALSE,
                 prespecified_structural_integrity_checks_applied_to_all_templates = TRUE,
                 single_isi_baseline_is_descriptive_not_acceptance = TRUE,
                 observation_noise_enabled = FALSE, drift_enabled = FALSE)
jsonlite::write_json(manifest, file.path(output_dir, "metadata", "reproduction_manifest.json"), auto_unbox = TRUE, pretty = TRUE, digits = 15)

cat("Generated", parameters$n_templates, "dimensionless templates and", nrow(sample_key), "exact paired projections.\n")
cat("Primary Burst episodes:", sum(events_u$Event_Label == "Burst"), "| HFS-internal:", sum(events_u$Event_Context == "burst_in_hfs"), "\n")
cat("Contextual separators are secondary truth only:", nrow(separators_u), "\n")
cat("Borderline gaps are secondary truth only:", nrow(borderline_u), "\n")
cat("Output:", output_dir, "\n")
