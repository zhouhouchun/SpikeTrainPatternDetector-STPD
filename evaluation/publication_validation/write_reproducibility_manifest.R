#!/usr/bin/env Rscript

command <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", command, value = TRUE)
if (length(file_arg) != 1L) {
  stop("Unable to resolve the manifest script path.", call. = FALSE)
}
script_path <- normalizePath(sub("^--file=", "", file_arg), mustWork = TRUE)
repo_default <- normalizePath(
  file.path(dirname(script_path), "..", ".."), mustWork = TRUE
)
repo <- normalizePath(
  Sys.getenv("STPD_REPO_ROOT", unset = repo_default), mustWork = TRUE
)
source(file.path(
  repo, "evaluation", "publication_validation",
  "publication_validation_helpers.R"
), local = FALSE)
root <- path.expand(Sys.getenv(
  "STPD_VALIDATION_OUTPUT_ROOT",
  unset = file.path(repo, "test-results", "publication_validation")
))
detector_r_files <- sort(list.files(
  file.path(repo, "R"), pattern = "[.]R$", recursive = TRUE,
  full.names = FALSE
), method = "radix")
detector_r_files <- file.path("R", detector_r_files)
parameter_config_files <- sort(list.files(
  file.path(repo, "inst", "config"), pattern = "[.]ya?ml$",
  recursive = TRUE, full.names = FALSE
), method = "radix")
parameter_config_files <- file.path("inst", "config", parameter_config_files)
source_files <- unique(c(
  "DESCRIPTION", "NAMESPACE", detector_r_files, parameter_config_files,
  "evaluation/publication_validation/publication_validation_helpers.R",
  "evaluation/publication_validation/run_simulation_repeated_validation.R",
  "evaluation/publication_validation/run_real_leave_group_out_validation.R",
  "evaluation/publication_validation/build_publication_validation_report.R",
  "evaluation/publication_validation/write_reproducibility_manifest.R",
  "test-results/clean_synthetic_validation/run_v2_stage_c_score.R"
))
simulator_files <- c(
  "detector_inputs/spike_times_blinded.csv",
  "ground_truth/interval_labels.csv",
  "ground_truth/episode_labels.csv"
)
simulator_configured <- trimws(Sys.getenv("STPD_SIM_DATA_ROOT", unset = ""))
if (!nzchar(simulator_configured)) {
  stop(
    "Set STPD_SIM_DATA_ROOT to the explicit frozen simulator input root; fallback is disabled.",
    call. = FALSE
  )
}
simulator_root <- normalizePath(path.expand(simulator_configured), mustWork = TRUE)
resolve_simulation_result <- function(env_var) {
  path <- stpd_pub_resolve_frozen_result_dir(
    env_var = env_var,
    required_files = c(
      "pooled_observed_metrics.csv", "publication_validation_result.rds"
    ),
    configured_path = Sys.getenv(env_var, unset = "")
  )
  if (!any(file.exists(file.path(
      path, c("cluster_bootstrap_95ci.csv", "group_cluster_bootstrap_95ci.csv")
  )))) {
    stop("Frozen simulation result lacks a cluster-bootstrap CI table: ",
         env_var, ".", call. = FALSE)
  }
  path
}
simulation_dirs <- c(
  simulation_short = resolve_simulation_result(
    "STPD_SIM_SHORT_VALIDATION_DIR"
  ),
  simulation_medium = resolve_simulation_result(
    "STPD_SIM_MEDIUM_VALIDATION_DIR"
  ),
  simulation_long = resolve_simulation_result(
    "STPD_SIM_LONG_VALIDATION_DIR"
  )
)
real_bundle <- stpd_pub_validate_real_result_bundle(
  configured_path = Sys.getenv("STPD_REAL_VALIDATION_DIR", unset = "")
)
manual_contract <- stpd_pub_resolve_authoritative_workbook(
  repo = repo,
  configured_path = Sys.getenv("STPD_MANUAL_REFERENCE_XLSX", unset = "")
)
manual_path <- manual_contract$path
raw_path <- Sys.getenv("STPD_REAL_SPIKE_CSV", unset = "")
if (!nzchar(raw_path) || !file.exists(raw_path)) {
  stop("Set STPD_REAL_SPIKE_CSV.", call. = FALSE)
}
manual_path <- normalizePath(path.expand(manual_path), mustWork = TRUE)
raw_path <- normalizePath(path.expand(raw_path), mustWork = TRUE)
inventory_directory <- function(path, label_prefix) {
  files <- sort(list.files(
    path, recursive = TRUE, full.names = TRUE, all.files = FALSE,
    include.dirs = FALSE
  ), method = "radix")
  files <- files[file.exists(files) & !dir.exists(files)]
  if (!length(files)) {
    stop("Frozen result directory is empty: ", path, ".", call. = FALSE)
  }
  prefix <- paste0(normalizePath(path, mustWork = TRUE), .Platform$file.sep)
  relative <- substring(normalizePath(files, mustWork = TRUE), nchar(prefix) + 1L)
  list(
    paths = files,
    labels = paste0(label_prefix, "/", relative)
  )
}
simulation_inventory <- lapply(names(simulation_dirs), function(name) {
  inventory_directory(
    unname(simulation_dirs[[name]]),
    paste0("validation_output/", name)
  )
})
real_inventory <- inventory_directory(
  real_bundle$path, "validation_output/real_frozen"
)
report_required <- c(
  "publication_performance_table.csv", "publication_protocol_table.csv",
  "real_explicit_positive_sensitivity.csv", "real_primary_macro_f1.csv",
  "real_endpoint_scope.csv", "artifact.json", "publication_validation_report.md",
  "publication_validation_report.html"
)
report_dir <- stpd_pub_resolve_frozen_result_dir(
  env_var = "STPD_VALIDATION_OUTPUT_ROOT/report",
  required_files = report_required,
  configured_path = file.path(root, "report")
)
report_inventory <- inventory_directory(
  report_dir, "validation_output/report"
)
report_generated_here <- basename(report_inventory$paths) %in% c(
  "reproducibility_manifest_sha256.csv", "R_sessionInfo.txt"
)
report_inventory$paths <- report_inventory$paths[!report_generated_here]
report_inventory$labels <- report_inventory$labels[!report_generated_here]
paths <- c(
  file.path(repo, source_files),
  file.path(simulator_root, simulator_files), manual_path, raw_path,
  unlist(lapply(simulation_inventory, `[[`, "paths"), use.names = FALSE),
  real_inventory$paths, report_inventory$paths
)
labels <- c(
  source_files,
  paste0("external_simulator/", simulator_files),
  paste0("external_manual/", basename(manual_path)),
  paste0("external_raw/", basename(raw_path)),
  unlist(lapply(simulation_inventory, `[[`, "labels"), use.names = FALSE),
  real_inventory$labels, report_inventory$labels
)
if (any(!file.exists(paths))) {
  stop(
    "Reproducibility manifest input is missing: ",
    paste(labels[!file.exists(paths)], collapse = " | "), call. = FALSE
  )
}
if (anyDuplicated(labels) || anyDuplicated(normalizePath(paths, mustWork = TRUE))) {
  stop("Reproducibility manifest inventory contains duplicate paths or labels.",
       call. = FALSE)
}
manifest <- data.frame(
  relative_path = labels,
  bytes = as.numeric(file.info(paths)$size),
  sha256 = vapply(paths, digest::digest, character(1), file = TRUE, algo = "sha256",
                  serialize = FALSE),
  stringsAsFactors = FALSE
)
write.csv(manifest, file.path(root, "report", "reproducibility_manifest_sha256.csv"),
          row.names = FALSE)
capture.output(sessionInfo(), file = file.path(root, "report", "R_sessionInfo.txt"))
cat("Wrote", nrow(manifest), "SHA-256 entries.\n")
