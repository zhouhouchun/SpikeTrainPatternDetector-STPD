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
quota <- read.csv(file.path(root, "metadata", "frozen_burst_quota.csv"))
key <- read.csv(file.path(root, "truth", "sample_template_scale_key.csv"))
intervals <- read.csv(file.path(root, "truth", "interval_truth_multitrack.csv"))
detector <- read.csv(file.path(root, "detector_inputs", "spike_timestamps_blinded.csv"), check.names = FALSE)
calibration <- read.csv(file.path(root, "calibration", "calibration_same_episode_ids_all_scales.csv"))

assert(identical(p$dataset_version, "2.2.0"), "Version is not v2.2.0.")
assert(length(unique(spikes_u$Template_ID)) == 20L, "Expected 20 mother templates.")
assert(nrow(key) == 60L && all(table(key$Template_ID) == 3L), "Expected 60 paired projections.")
assert(identical(sort(unique(key$Scale_Factor)), c(1L, 4L, 10L)), "Scale factors are not 1/4/10.")
assert(identical(names(detector), c("Sample_ID", "Spike_Index", "Time_s")), "Detector input leaks truth columns.")
assert(!any(grepl("TPL|Scale|Class|C1|C2|C3", detector$Sample_ID)), "Sample IDs leak template/scale.")
for (template_id in unique(key$Template_ID)) {
  k <- key[key$Template_ID == template_id, ]; k <- k[order(k$Scale_Factor), ]
  v <- lapply(k$Sample_ID, function(id) detector$Time_s[detector$Sample_ID == id])
  assert(isTRUE(all.equal(v[[2]], 4 * v[[1]], tolerance = tol)), paste("4x mismatch", template_id))
  assert(isTRUE(all.equal(v[[3]], 10 * v[[1]], tolerance = tol)), paste("10x mismatch", template_id))
}
assert(all(abs(intervals$ISI_s - intervals$ISI_u * intervals$B_s) < tol), "ISI scaling mismatch.")

burst <- events_u[events_u$Event_Label == "Burst", ]
assert(nrow(burst) == 100L && sum(burst$Event_Context == "burst_in_hfs") == 40L, "Burst quotas failed.")
assert(all(burst$N_Realized_Spikes == burst$Frozen_Target_Spikes), "Burst spike target failed.")
assert(all(burst$N_Realized_Spikes <= 10L) && sum(burst$N_Realized_Spikes == 4L) == 12L, "Burst size contract failed.")
assert(abs(fisher.test(table(quota$Context, quota$Strength))$p.value - 1) < tol, "Burst context/strength confounding remains.")
pause <- events_u[events_u$Event_Label == "Pause", ]
assert(sum(pause$Pause_Subtype == "canonical") == 40L && sum(pause$Pause_Subtype == "complex_multi_gap") == 10L,
       "Pause quota failed.")
assert(!any(pause$Pause_Subtype == "contextual_interburst_gap_separator"), "Contextual separator leaked into Pause.")

hfs <- states_u[states_u$State_Label == "Broad_HFS", ]
assert(nrow(hfs) == 60L && all(hfs$N_Boundary_Spikes >= 20L & hfs$N_Boundary_Spikes <= 35L), "HFS count contract failed.")
assert(all(hfs$Duration_u >= 3 - tol), "HFS duration contract failed.")
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

assert(nrow(regimes_u) == 10L && !any(regimes_u$Continuous_HFS_State), "Composite Regime contract failed.")
assert(all(regimes_u$Left_State_Envelope_ID != regimes_u$Right_State_Envelope_ID), "Pause did not split HFS states.")
for (i in seq_len(nrow(regimes_u))) {
  e <- pause[pause$Event_ID == regimes_u$Canonical_Pause_Event_ID[i], ]
  assert(nrow(e) == 1L && e$Pause_Subtype == "canonical", "Regime connector is not canonical Pause.")
}

assert(nrow(tonic_u) == 40L && all(tonic_u$Tonic_Phenotype %in% c("eligible", "ambiguous", "no_evidence")),
       "Tonic phenotype audit missing.")
assert(all(is.finite(tonic_u$Tonic_Phenotype_Evidence_Score)) && all(!tonic_u$Detector_Output_Used),
       "Tonic evidence is invalid or detector-dependent.")
strict_states <- read.csv(file.path(root, "truth", "strict_mechanism_estimand_states.csv"))
observable_states <- read.csv(file.path(root, "truth", "observable_phenotype_estimand_states.csv"))
assert(sum(strict_states$State_Label == "Tonic") == 120L, "Strict Tonic estimand must include all scales.")
assert(sum(observable_states$State_Label == "Tonic") == 3L * sum(tonic_u$Tonic_Phenotype == "eligible"),
       "Observable Tonic estimand does not match eligible states.")

assert(!anyDuplicated(runs_u[, c("Template_ID", "Component_Type", "Component_Slot")]), "Component RNG keys are duplicated.")
for (i in seq_len(nrow(runs_u))) {
  x <- intervals_u$ISI_u[intervals_u$Run_ID == runs_u$Run_ID[i]]
  h <- digest::digest(paste(formatC(x, digits = 12, format = "fg"), collapse = "|"), algo = "sha256", serialize = FALSE)
  assert(identical(h, runs_u$Local_ISI_SHA256[i]), paste("Local component hash mismatch", runs_u$Run_ID[i]))
}

# Counterfactual isolation test: perturb only Tonic parameters.  Every non-Tonic
# local component must retain the same ISI hash; absolute downstream offsets may move.
source(file.path(root, "R", "00_parameters.R"), local = .GlobalEnv)
source(file.path(root, "R", "10_point_process.R"), local = .GlobalEnv)
source(file.path(root, "R", "20_template_generation.R"), local = .GlobalEnv)
q <- make_frozen_burst_quota_table()
base <- generate_template_v22(6L, q)
parameters$tonic$regimes$eligible$generic_stress$mean_isi_B <- c(0.90, 1.20)
changed <- generate_template_v22(6L, q)
b <- base$runs[base$runs$Component_Type != "tonic", c("Component_Type", "Component_Slot", "Local_ISI_SHA256")]
c <- changed$runs[changed$runs$Component_Type != "tonic", c("Component_Type", "Component_Slot", "Local_ISI_SHA256")]
assert(identical(b, c), "Tonic perturbation changed another component local timestamp sequence.")

assert(all(table(calibration$Scale_Factor, calibration$Calibration_Mode) == 10L), "Calibration counts failed.")
for (mode in unique(calibration$Calibration_Mode)) {
  sets <- lapply(c(1L, 4L, 10L), function(scale) sort(calibration$Episode_ID[
    calibration$Calibration_Mode == mode & calibration$Scale_Factor == scale]))
  assert(identical(sets[[1]], sets[[2]]) && identical(sets[[1]], sets[[3]]), paste("Calibration ID mismatch", mode))
}
xlsx_path <- file.path(root, "detector_inputs", "spike_timestamps_blinded.xlsx")
expected_sheets <- names(unlist(p$workbook_blind_batches, use.names = TRUE))
assert(identical(readxl::excel_sheets(xlsx_path), expected_sheets), "Unexpected workbook sheets.")
xlsx <- do.call(rbind, lapply(expected_sheets, function(sheet) as.data.frame(readxl::read_xlsx(xlsx_path, sheet = sheet))))
xlsx <- xlsx[order(xlsx$Sample_ID, xlsx$Spike_Index), ]; detector2 <- detector[order(detector$Sample_ID, detector$Spike_Index), ]
assert(identical(as.character(xlsx$Sample_ID), as.character(detector2$Sample_ID)), "XLSX sample mismatch.")
assert(isTRUE(all.equal(xlsx$Time_s, detector2$Time_s, tolerance = 0)), "XLSX timestamps mismatch.")
checksums <- read.csv(file.path(root, "metadata", "canonical_output_checksums_sha256.csv"))
for (i in seq_len(nrow(checksums))) assert(digest::digest(file = file.path(root, checksums$File[i]), algo = "sha256") == checksums$SHA256[i],
                                           paste("Checksum mismatch", checksums$File[i]))
cat("All v2.2 mechanism, phenotype, RNG-isolation, scale, calibration, XLSX and checksum checks passed.\n")
