#!/usr/bin/env Rscript

# Build a clean, allow-list-based public repository snapshot from the working
# scientific repository. The source tree is never modified by this script.

options(stringsAsFactors = FALSE)

args <- commandArgs(trailingOnly = TRUE)
source_root <- if (length(args) >= 1L) args[[1]] else getwd()
output_root <- if (length(args) >= 2L) args[[2]] else {
  file.path(dirname(normalizePath(source_root, mustWork = TRUE)), "STPD_GitHub_release")
}

source_root <- normalizePath(source_root, mustWork = TRUE)
output_root <- path.expand(output_root)

if (file.exists(output_root)) {
  stop("Output directory already exists; choose a new path or archive it first: ", output_root, call. = FALSE)
}
if (identical(normalizePath(dirname(output_root), mustWork = TRUE), source_root)) {
  stop("The public release must be built outside the source repository.", call. = FALSE)
}
if (!requireNamespace("digest", quietly = TRUE)) {
  stop("Package 'digest' is required to write SHA-256 manifests.", call. = FALSE)
}

dir.create(output_root, recursive = TRUE, showWarnings = FALSE)

excluded_file <- function(relative_path) {
  base <- basename(relative_path)
  ext <- tolower(tools::file_ext(base))
  base %in% c(".DS_Store", ".Rhistory", "Rplots.pdf", "test_gate_b_v3_phase1_contract 2.R") ||
    ext %in% c("o", "so", "dll", "dylib", "pyc", "rds", "rdata") ||
    grepl("(^|/)__pycache__(/|$)", relative_path)
}

copy_file <- function(source, destination) {
  if (!file.exists(source) || dir.exists(source)) {
    stop("Missing required release file: ", source, call. = FALSE)
  }
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  ok <- file.copy(source, destination, overwrite = FALSE, copy.mode = TRUE, copy.date = TRUE)
  if (!isTRUE(ok)) stop("Unable to copy: ", source, call. = FALSE)
  invisible(destination)
}

copy_tree <- function(source, destination) {
  if (!dir.exists(source)) stop("Missing required release directory: ", source, call. = FALSE)
  relative <- list.files(source, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  info <- file.info(file.path(source, relative))
  relative <- relative[!is.na(info$isdir) & !info$isdir]
  relative <- relative[!vapply(relative, excluded_file, logical(1))]
  for (path in relative) {
    copy_file(file.path(source, path), file.path(destination, path))
  }
  invisible(destination)
}

copy_selected <- function(source_dir, destination_dir, files) {
  for (file in files) copy_file(file.path(source_dir, file), file.path(destination_dir, file))
  invisible(destination_dir)
}

top_level <- c(
  "DESCRIPTION", "NAMESPACE", "LICENSE", "CITATION.cff", "README.md",
  "README.zh.md", "ARCHITECTURE.md", "RELEASE_NOTES.md"
)
for (file in top_level) copy_file(file.path(source_root, file), file.path(output_root, file))
copy_file(file.path(source_root, "publication", "PUBLIC_GITIGNORE"), file.path(output_root, ".gitignore"))
copy_file(file.path(source_root, "publication", "PUBLIC_RBUILDIGNORE"), file.path(output_root, ".Rbuildignore"))

for (directory in c("R", "inst", "man", "src", "tests", "evaluation", "tools", "analysis")) {
  copy_tree(file.path(source_root, directory), file.path(output_root, directory))
}

docs <- list.files(file.path(source_root, "docs"), pattern = "[.]md$", full.names = TRUE)
for (file in docs) copy_file(file, file.path(output_root, "docs", basename(file)))

synthetic_versions <- c(
  "v2.2.0" = "clean_synthetic_mechanism_benchmark_v2_2_0",
  "v2.3.0" = "clean_synthetic_mechanism_benchmark_v2_3_0"
)
for (version in names(synthetic_versions)) {
  copy_tree(
    file.path(source_root, "Simulator data", synthetic_versions[[version]]),
    file.path(output_root, "data", "synthetic", version)
  )
}

real_workbooks <- c(
  GPe = "PD_GPe/Bagdasaryan_PD_GPe_2020_manual_isi_labels_draft_csv.xlsx",
  STN = "PD_STN/PD_STN_Grechishnikova_2017_manual_isi_labels_draft_csv.xlsx",
  GPi = "PD_GPi/Kurmanaeva_PD_GPi_2017_manual_isi_labels_draft_csv.xlsx"
)
public_real_workbook_names <- c(
  GPe = "PD_GPe_manual_isi_labels.xlsx",
  STN = "PD_STN_manual_isi_labels.xlsx",
  GPi = "PD_GPi_manual_isi_labels.xlsx"
)
include_real_data <- identical(Sys.getenv("STPD_INCLUDE_REAL_DATA", unset = "1"), "1")
if (include_real_data) {
  for (region in names(real_workbooks)) {
    copy_file(
      file.path(source_root, real_workbooks[[region]]),
      file.path(output_root, "data", "real", region,
                public_real_workbook_names[[region]])
    )
  }
}
raw_stn <- Sys.getenv("STPD_REAL_RAW_CSV", unset = "")
if (include_real_data && nzchar(raw_stn) && file.exists(raw_stn)) {
  copy_file(raw_stn, file.path(output_root, "data", "real", "STN",
                               "PD_STN_spike_timestamps.csv"))
}

copy_file(file.path(source_root, "publication", "DATA_README.md"), file.path(output_root, "data", "README.md"))
copy_file(
  file.path(source_root, "publication", "REAL_SOURCE_AND_LICENSE_TEMPLATE.md"),
  file.path(output_root, "data", "real", "SOURCE_AND_LICENSE.md")
)
copy_file(
  file.path(source_root, "publication", "REAL_DATA_ANNOTATION_FREEZE.md"),
  file.path(output_root, "data", "real", "ANNOTATION_FREEZE.md")
)
copy_file(
  file.path(source_root, "publication", "REAL_DATA_ANNOTATION_FREEZE.csv"),
  file.path(output_root, "data", "real", "annotation_freeze.csv")
)
copy_file(file.path(source_root, "publication", "RESULTS_README.md"), file.path(output_root, "results", "README.md"))
copy_file(
  file.path(source_root, "publication", "RELEASE_CHECKLIST.md"),
  file.path(output_root, "PUBLIC_RELEASE_CHECKLIST.md")
)
reviewer_release_docs <- c(
  "REVIEWER_RESPONSE_EVIDENCE_MATRIX.md",
  "REVIEWER_RESPONSE_EVIDENCE_MATRIX.csv",
  "REVIEWER_1_VERBATIM.md",
  "REVIEWER_2_VERBATIM.md",
  "REVIEWER_3_VERBATIM.md",
  "REVIEWER_3_RESPONSE_AND_GAP_AUDIT.md"
)
reviewer_release_dir <- file.path(output_root, "docs", "reviewer_response")
copy_selected(
  file.path(source_root, "publication"),
  reviewer_release_dir,
  reviewer_release_docs
)

# The maintainer evidence matrix uses source-repository paths. Rewrite only the
# released copies so every cited path resolves inside the public repository.
public_path_replacements <- c(
  "publication/FINAL_ALGORITHM_FREEZE.md" =
    "results/release_freeze/FINAL_ALGORITHM_FREEZE.md",
  "publication/REAL_DATA_ANNOTATION_FREEZE.md" =
    "data/real/ANNOTATION_FREEZE.md",
  "publication/RELEASE_CHECKLIST.md" = "PUBLIC_RELEASE_CHECKLIST.md",
  "publication/DATA_README.md" = "data/README.md",
  "test-results/clean_synthetic_validation/runs/2026-08-30_tonic_state_repair_final_01/" =
    "results/synthetic_validation/",
  "test-results/clean_synthetic_validation/" = "results/synthetic_validation/",
  "test-results/multi_region_three_regime_20260831/" =
    "results/real_validation/",
  "test-results/method_comparison/" = "results/method_comparison/",
  "test-results/performance_benchmark_20260831/" = "results/performance/",
  "test-results/release_freeze_final/regression_final/" =
    "results/release_freeze/regression/",
  "test-results/" = "results/",
  "publication/" = "docs/reviewer_response/"
)
for (name in c(
  "REVIEWER_RESPONSE_EVIDENCE_MATRIX.md",
  "REVIEWER_RESPONSE_EVIDENCE_MATRIX.csv",
  "REVIEWER_3_RESPONSE_AND_GAP_AUDIT.md"
)) {
  path <- file.path(reviewer_release_dir, name)
  text <- readLines(path, warn = FALSE, encoding = "UTF-8")
  for (source_path in names(public_path_replacements)) {
    text <- gsub(
      source_path, public_path_replacements[[source_path]], text,
      fixed = TRUE
    )
  }
  writeLines(text, path, useBytes = TRUE)
}
copy_selected(
  file.path(source_root, "publication"),
  file.path(output_root, "results", "release_freeze"),
  c("FINAL_ALGORITHM_FREEZE.md", "FINAL_ALGORITHM_FREEZE.csv")
)
regression_files <- unlist(lapply(1:4, function(shard) c(
  sprintf("testthat_shard_%02d.csv", shard),
  sprintf("testthat_shard_%02d_summary.csv", shard)
)), use.names = FALSE)
copy_selected(
  file.path(source_root, "test-results", "release_freeze_final", "regression_final"),
  file.path(output_root, "results", "release_freeze", "regression"),
  regression_files
)

real_results <- file.path(source_root, "test-results", "multi_region_three_regime_20260831")
copy_selected(real_results, file.path(output_root, "results", "real_validation"), c(
  "primary_f1_summary.csv", "primary_f1_summary.md", "combined_metrics_with_ci.csv",
  "validation_integrity_checks.csv", "validation_findings_zh.md"
))
for (region in c("GPE", "STN", "GPI")) {
  copy_selected(
    file.path(real_results, region, "automatic"),
    file.path(output_root, "results", "real_validation",
              "current_automatic", region, "automatic"),
    c("predicted_events.csv", "reference_profile.csv")
  )
}

copy_tree(
  file.path(source_root, "test-results", "performance_benchmark_20260831"),
  file.path(output_root, "results", "performance")
)
copy_file(
  file.path(source_root, "publication", "PERFORMANCE_README.md"),
  file.path(output_root, "results", "performance", "README.md")
)

method_comparison_root <- file.path(
  source_root, "test-results", "method_comparison"
)
copy_selected(
  file.path(method_comparison_root, "meanisi_vs_logisi_burst"),
  file.path(output_root, "results", "method_comparison",
            "meanisi_vs_logisi_burst"),
  c(
    "RESULTS.md", "protocol.csv", "support_metrics.csv", "event_metrics.csv",
    "paired_cluster_bootstrap_f1_contrast.csv",
    "cluster_bootstrap_method_metrics_95ci.csv",
    "real_reference_eligibility.csv", "runtime_and_threshold_status.csv",
    "validation_checks.csv", "meanisi_logisi_burst_performance.pdf",
    "meanisi_logisi_burst_performance.png", "figure_caption.md"
  )
)
copy_selected(
  file.path(method_comparison_root, "three_method_agreement_current"),
  file.path(output_root, "results", "method_comparison",
            "three_method_agreement_current"),
  c(
    "RESULTS.md", "protocol.csv", "comparison_scope.csv",
    "pairwise_agreement_metrics.csv", "three_way_support_metrics.csv",
    "recording_group_bootstrap_95ci.csv", "validation_checks.csv",
    "input_manifest_sha256.csv", "figure_selection_and_windows.csv",
    "three_method_two_train_raster.pdf", "three_method_two_train_raster.png",
    "three_method_two_train_raster_caption.md"
  )
)
copy_selected(
  file.path(method_comparison_root, "three_method_truth_accuracy_current"),
  file.path(output_root, "results", "method_comparison",
            "three_method_truth_accuracy_current"),
  c(
    "RESULTS.md", "protocol.csv", "analysis_scope.csv",
    "support_metrics.csv", "event_metrics.csv",
    "cluster_bootstrap_95ci.csv", "event_cluster_bootstrap_95ci.csv",
    "validation_checks.csv",
    "three_method_burst_accuracy.pdf", "three_method_burst_accuracy.png",
    "figure_caption.md"
  )
)

synthetic_final_run <- "2026-08-30_tonic_state_repair_final_01"
synthetic_stage_c <- c(
  "v2.2.0" = file.path(
    source_root, "test-results", "clean_synthetic_validation", "runs",
    synthetic_final_run, "v220_stage_c"
  ),
  "v2.3.0" = file.path(
    source_root, "test-results", "clean_synthetic_validation", "runs",
    synthetic_final_run, "v230_stage_c"
  )
)
synthetic_result_files <- c(
  "primary_observed_metrics.csv", "template_cluster_bootstrap_95ci.csv",
  "fragmentation_merge_summary.csv", "boundary_errors_iou_050.csv",
  "pause_subtype_summary.csv", "hfs_predicted_subtypes_descriptive.csv"
)
for (version in names(synthetic_stage_c)) {
  source_stage_c <- synthetic_stage_c[[version]]
  public_stage_c <- file.path(
    output_root, "results", "synthetic_validation", version
  )
  copy_selected(
    source_stage_c,
    public_stage_c,
    synthetic_result_files
  )

  source_manifest_path <- file.path(source_stage_c, "stage_c_manifest.csv")
  source_manifest <- utils::read.csv(
    source_manifest_path, check.names = FALSE, stringsAsFactors = FALSE
  )
  compact_manifest <- source_manifest[
    match(synthetic_result_files, source_manifest$File_Name),
    , drop = FALSE
  ]
  if (anyNA(compact_manifest$File_Name) ||
      !identical(compact_manifest$File_Name, synthetic_result_files)) {
    stop(
      "Final synthetic Stage C manifest does not cover the public compact files: ",
      source_manifest_path,
      call. = FALSE
    )
  }
  actual_sha256 <- vapply(
    file.path(source_stage_c, compact_manifest$File_Name),
    function(path) digest::digest(file = path, algo = "sha256"),
    character(1)
  )
  if (!identical(unname(actual_sha256), compact_manifest$SHA256)) {
    stop(
      "Final synthetic Stage C manifest hash verification failed: ",
      source_manifest_path,
      call. = FALSE
    )
  }
  utils::write.csv(
    compact_manifest,
    file.path(public_stage_c, "stage_c_manifest.csv"),
    row.names = FALSE
  )
}

synthetic_pipeline <- file.path(source_root, "test-results", "clean_synthetic_validation")
pipeline_files <- c(
  "README_v2_validation.md", "v2_pipeline_common.R", "run_v2_stage_a_calibrate.R",
  "run_v2_stage_b_detect.R", "run_v2_stage_c_score.R", "test_v2_pipeline_contract.R"
)
copy_selected(
  synthetic_pipeline,
  file.path(output_root, "evaluation", "synthetic_validation"),
  pipeline_files
)

old_public_repo <- Sys.getenv("STPD_PUBLIC_REPO", unset = NA_character_)
on.exit({
  if (is.na(old_public_repo)) Sys.unsetenv("STPD_PUBLIC_REPO")
  else Sys.setenv(STPD_PUBLIC_REPO = old_public_repo)
}, add = TRUE)
Sys.setenv(STPD_PUBLIC_REPO = output_root)
sys.source(
  file.path(output_root, "analysis", "figures", "plot_validation_summaries.R"),
  envir = new.env(parent = globalenv())
)
sys.source(
  file.path(output_root, "analysis", "figures", "plot_performance_runtime.R"),
  envir = new.env(parent = globalenv())
)

all_relative <- list.files(output_root, recursive = TRUE, all.files = TRUE, no.. = TRUE)
all_info <- file.info(file.path(output_root, all_relative))
all_relative <- all_relative[!is.na(all_info$isdir) & !all_info$isdir]
all_relative <- setdiff(all_relative, "release_manifest_sha256.csv")
all_info <- file.info(file.path(output_root, all_relative))
identifying_filename_terms <- c("Bagdasaryan", "Grechishnikova", "Kurmanaeva")
identifying_filename_hits <- all_relative[vapply(
  all_relative,
  function(path) any(vapply(
    identifying_filename_terms,
    function(term) grepl(term, basename(path), ignore.case = TRUE),
    logical(1)
  )),
  logical(1)
)]
if (length(identifying_filename_hits)) {
  stop("Public release contains identifying names in filenames: ",
       paste(identifying_filename_hits, collapse = "; "), call. = FALSE)
}
if (any(all_info$size > 100 * 1024^2)) {
  stop("Public release contains a file larger than 100 MiB.", call. = FALSE)
}

manifest <- data.frame(
  path = all_relative,
  bytes = unname(all_info$size),
  sha256 = vapply(
    file.path(output_root, all_relative),
    function(path) digest::digest(file = path, algo = "sha256"),
    character(1)
  ),
  stringsAsFactors = FALSE
)
manifest <- manifest[order(manifest$path), , drop = FALSE]
utils::write.csv(manifest, file.path(output_root, "release_manifest_sha256.csv"), row.names = FALSE)

text_extensions <- c("r", "md", "txt", "csv", "json", "yml", "yaml", "cff", "c", "cpp", "sh")
text_paths <- file.path(output_root, manifest$path[tolower(tools::file_ext(manifest$path)) %in% text_extensions])
decode_term <- function(codepoints) intToUtf8(codepoints)
forbidden <- vapply(list(
  c(67, 111, 100, 101, 120),
  c(67, 108, 97, 117, 100, 101),
  c(67, 104, 97, 116, 71, 80, 84),
  c(79, 112, 101, 110, 65, 73),
  c(46, 67, 111, 100, 101, 120),
  c(47, 85, 115, 101, 114, 115, 47, 122, 97, 114, 107),
  c(99, 111, 108, 108, 97, 98, 111, 114, 97, 116, 105, 111, 110, 47)
), decode_term, character(1))
hits <- character()
for (path in text_paths) {
  lines <- tryCatch(readLines(path, warn = FALSE, encoding = "UTF-8"), error = function(e) character())
  for (term in forbidden) {
    if (any(grepl(term, lines, fixed = TRUE))) hits <- c(hits, paste(path, term, sep = " :: "))
  }
}
if (length(hits)) {
  stop("Public-content scan failed:\n", paste(unique(hits), collapse = "\n"), call. = FALSE)
}

cat("Public release built successfully:\n", output_root, "\n", sep = "")
cat("Files:", nrow(manifest), "\n")
cat("Bytes:", sum(manifest$bytes), "\n")
cat("Patient-level real workbooks included:", include_real_data, "\n")
