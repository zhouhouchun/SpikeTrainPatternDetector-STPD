#!/usr/bin/env Rscript

# Frozen v2.2 clean synthetic mechanism benchmark generator.
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
templates <- lapply(seq_len(parameters$n_templates), generate_template_v22, burst_quota = burst_quota)
spikes_u <- bind_nonempty(lapply(templates, `[[`, "spikes"))
intervals_raw_u <- bind_nonempty(lapply(templates, `[[`, "intervals"))
events_u <- bind_nonempty(lapply(templates, `[[`, "events"))
states_u <- bind_nonempty(lapply(templates, `[[`, "states"))
runs_u <- bind_nonempty(lapply(templates, `[[`, "runs"))
separators_u <- bind_nonempty(lapply(templates, `[[`, "separators"))
contextual_links_u <- bind_nonempty(lapply(templates, `[[`, "contextual"))
transitions_u <- bind_nonempty(lapply(templates, `[[`, "transitions"))
regimes_u <- bind_nonempty(lapply(templates, `[[`, "regimes"))
template_manifest <- bind_nonempty(lapply(templates, `[[`, "quota"))

phenotype_thresholds <- fit_phenotype_thresholds(states_u, runs_u)
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

sample_key <- make_sample_key_v22(template_manifest)
spikes <- scale_table_v22(spikes_u, sample_key, "Time_u")
intervals <- scale_table_v22(intervals_u, sample_key, c("Start_u", "End_u", "ISI_u"))
events <- scale_table_v22(events_u, sample_key, c("Latent_Start_u", "Latent_End_u", "Latent_Duration_u",
                                                  "Realized_Start_u", "Realized_End_u", "Realized_Duration_u"))
states <- scale_table_v22(states_u, sample_key, c("Start_u", "End_u", "Duration_u", "Mean_ISI_Parameter_u",
                                                  "Refractory_u", "Observed_Mean_ISI_u"))
runs <- scale_table_v22(runs_u, sample_key, c("Start_u", "End_u", "Duration_u", "Mean_ISI_Parameter_u",
                                              "Refractory_u", "Observed_Mean_ISI_u"))
separators <- scale_table_v22(separators_u, sample_key, c("Start_u", "End_u", "Duration_u"))
contextual_links <- scale_table_v22(contextual_links_u, sample_key,
                                    c("Left_Local_Median_ISI_u", "Right_Local_Median_ISI_u", "Separator_ISI_u"))
transitions <- scale_table_v22(transitions_u, sample_key, "Boundary_u")
regimes <- scale_table_v22(regimes_u, sample_key, c("Start_u", "End_u"))
tonic_phenotypes <- scale_table_v22(tonic_phenotypes_u, sample_key,
                                    c("Duration_u", "Median_ISI_u", "Local_Left_Median_ISI_u", "Local_Right_Median_ISI_u"))
phenotypes <- scale_table_v22(phenotypes_u, sample_key,
                              c("Start_u", "End_u", "Duration_u", "Internal_Median_ISI_u", "Flank_Median_ISI_u"))

detector_input <- spikes[, c("Sample_ID", "Spike_Index", "Time_s")]
detector_input <- detector_input[order(detector_input$Sample_ID, detector_input$Spike_Index), ]; rownames(detector_input) <- NULL
calibration <- select_calibration_v22(events_u, states_u, sample_key)
contextual_calibration <- merge(separators_u[separators_u$Template_ID %in% parameters$development_templates,
                                             c("Template_ID", "Contextual_Separator_ID", "Start_u", "End_u", "Duration_u")],
                                sample_key, by = "Template_ID", sort = FALSE)
contextual_calibration$Start_s <- contextual_calibration$Start_u * contextual_calibration$B_s
contextual_calibration$End_s <- contextual_calibration$End_u * contextual_calibration$B_s
contextual_calibration$Duration_s <- contextual_calibration$Duration_u * contextual_calibration$B_s

write.csv(detector_input, file.path(output_dir, "detector_inputs", "spike_timestamps_blinded.csv"), row.names = FALSE)
write_detector_workbook_v22(detector_input, sample_key, file.path(output_dir, "detector_inputs", "spike_timestamps_blinded.xlsx"))
write.csv(sample_key, file.path(output_dir, "truth", "sample_template_scale_key.csv"), row.names = FALSE)
write.csv(intervals, file.path(output_dir, "truth", "interval_truth_multitrack.csv"), row.names = FALSE, na = "")
write.csv(events, file.path(output_dir, "truth", "primary_event_episodes.csv"), row.names = FALSE, na = "")
write.csv(states, file.path(output_dir, "truth", "state_envelopes.csv"), row.names = FALSE, na = "")
write.csv(regimes, file.path(output_dir, "truth", "composite_hfs_regimes.csv"), row.names = FALSE, na = "")
write.csv(separators, file.path(output_dir, "truth", "contextual_separator_secondary_episodes.csv"), row.names = FALSE, na = "")
write.csv(contextual_links, file.path(output_dir, "truth", "contextual_separator_links.csv"), row.names = FALSE, na = "")
write.csv(transitions, file.path(output_dir, "truth", "transition_boundaries.csv"), row.names = FALSE, na = "")
write.csv(runs, file.path(output_dir, "truth", "generator_provenance.csv"), row.names = FALSE, na = "")
write.csv(phenotypes, file.path(output_dir, "truth", "observable_phenotype.csv"), row.names = FALSE, na = "")
write.csv(tonic_phenotypes, file.path(output_dir, "truth", "tonic_phenotype_evidence.csv"), row.names = FALSE, na = "")
write.csv(events, file.path(output_dir, "truth", "strict_mechanism_estimand_episodes.csv"), row.names = FALSE, na = "")
write.csv(phenotypes, file.path(output_dir, "truth", "phenotype_estimand_regions.csv"), row.names = FALSE, na = "")
write.csv(states, file.path(output_dir, "truth", "strict_mechanism_estimand_states.csv"), row.names = FALSE, na = "")
observable_states <- states[states$State_Label == "Broad_HFS" |
                              (states$State_Label == "Tonic" & states$Tonic_Phenotype == "eligible"), , drop = FALSE]
write.csv(observable_states, file.path(output_dir, "truth", "observable_phenotype_estimand_states.csv"), row.names = FALSE, na = "")
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
write.csv(phenotypes_u, file.path(output_dir, "tables", "dimensionless_observable_phenotype.csv"), row.names = FALSE, na = "")

write.csv(template_manifest, file.path(output_dir, "metadata", "template_manifest.csv"), row.names = FALSE)
write.csv(burst_quota, file.path(output_dir, "metadata", "frozen_burst_quota.csv"), row.names = FALSE)
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

distribution_summary <- make_distribution_summary_v22(intervals_u, events_u, states_u, separators_u)
write.csv(distribution_summary, file.path(output_dir, "qc", "distribution_summary.csv"), row.names = FALSE, na = "")
write.csv(phenotypes_u, file.path(output_dir, "qc", "phenotype_audit.csv"), row.names = FALSE, na = "")
write.csv(tonic_phenotypes_u, file.path(output_dir, "qc", "tonic_phenotype_audit.csv"), row.names = FALSE, na = "")
hfs_boundary_audit <- states_u[states_u$State_Label == "Broad_HFS",
                               c("Template_ID", "State_Envelope_ID", "Regularity_Regime", "Contains_Injected_Burst",
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
fisher_result <- fisher.test(table(burst_quota$Context, burst_quota$Strength))
remediation <- data.frame(Check = c("Burst_strength_context_Fisher_p", "HFS_Burst_development", "HFS_Burst_holdout",
                                     "Four_spike_Burst", "Contextual_as_primary_Pause", "Complex_Pause",
                                     "Direct_target_boundaries", "Paired_scale_templates"),
                          Value = c(fisher_result$p.value,
                                    sum(burst_quota$Context == "hfs" & burst_quota$Split == "development"),
                                    sum(burst_quota$Context == "hfs" & burst_quota$Split == "holdout"),
                                    sum(burst_quota$Is_Four_Spike_Boundary),
                                    sum(events_u$Pause_Subtype == "contextual_interburst_gap_separator"),
                                    sum(events_u$Pause_Subtype == "complex_multi_gap"),
                                    sum(transitions_u$Direct_Target_Boundary), parameters$n_templates), stringsAsFactors = FALSE)
write.csv(remediation, file.path(output_dir, "qc", "v2_2_audit_remediation_summary.csv"), row.names = FALSE)
structural <- data.frame(Metric = c("Minimum_observed_ISI_u", "Configured_refractory_u", "Refractory_violations",
                                    "Burst_over_10_spikes", "Exact_4_spike_Burst", "Primary_contextual_Pause_rows"),
                         Value = c(min(intervals_u$ISI_u), parameters$shared_refractory_B,
                                   sum(intervals_u$ISI_u < parameters$shared_refractory_B - 1e-12),
                                   sum(events_u$Event_Label == "Burst" & events_u$N_Realized_Spikes > 10),
                                   sum(events_u$Event_Label == "Burst" & events_u$N_Realized_Spikes == 4),
                                   sum(intervals_u$Event_Label == "Pause" & intervals_u$Contextual_Separator_Label != "none")))
write.csv(structural, file.path(output_dir, "qc", "structural_contract_audit.csv"), row.names = FALSE)
make_plots_v22(intervals, spikes, phenotypes_u, burst_quota, output_dir)

canonical_files <- c("detector_inputs/spike_timestamps_blinded.csv", "detector_inputs/spike_timestamps_blinded.xlsx",
                     "truth/interval_truth_multitrack.csv", "truth/primary_event_episodes.csv", "truth/state_envelopes.csv",
                     "truth/composite_hfs_regimes.csv", "truth/tonic_phenotype_evidence.csv",
                     "truth/contextual_separator_secondary_episodes.csv", "truth/observable_phenotype.csv",
                     "truth/sample_template_scale_key.csv", "metadata/frozen_burst_quota.csv", "metadata/generator_parameters.json",
                     "metadata/phenotype_thresholds.csv", "calibration/calibration_same_episode_ids_all_scales.csv",
                     "metadata/component_rng_registry.csv", "qc/v2_2_audit_remediation_summary.csv", "tables/dimensionless_spike_templates.csv",
                     "tables/dimensionless_interval_templates.csv")
checksums <- data.frame(File = canonical_files,
                        SHA256 = vapply(canonical_files, function(path) digest::digest(file = file.path(output_dir, path), algo = "sha256"), character(1)))
write.csv(checksums, file.path(output_dir, "metadata", "canonical_output_checksums_sha256.csv"), row.names = FALSE)
module_hashes <- vapply(module_files, function(x) digest::digest(file = x, algo = "sha256"), character(1))
names(module_hashes) <- basename(module_files)
manifest <- list(dataset_id = parameters$dataset_id, dataset_version = parameters$dataset_version,
                 created_utc = "2026-08-28T00:00:00Z", generator_file = basename(script_path),
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
                 canonical_pause_hard_cuts_hfs_state = TRUE, composite_hfs_regime_is_not_continuous_state = TRUE,
                 detector_input_columns = names(detector_input), detector_output_used_for_generation_or_selection = FALSE,
                 contextual_separator_is_primary_pause_truth = FALSE, stimulation_enabled = FALSE,
                 observation_noise_enabled = FALSE, drift_enabled = FALSE)
jsonlite::write_json(manifest, file.path(output_dir, "metadata", "reproduction_manifest.json"), auto_unbox = TRUE, pretty = TRUE, digits = 15)

cat("Generated", parameters$n_templates, "dimensionless templates and", nrow(sample_key), "exact paired projections.\n")
cat("Primary Burst episodes:", sum(events_u$Event_Label == "Burst"), "| HFS-internal:", sum(events_u$Event_Context == "burst_in_hfs"), "\n")
cat("Contextual separators are secondary truth only:", nrow(separators_u), "\n")
cat("Output:", output_dir, "\n")
