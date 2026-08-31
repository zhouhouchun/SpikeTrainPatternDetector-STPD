#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 1L && args[1L] %in% c("-h", "--help")) {
  cat(paste(
    "Usage:",
    "  Rscript evaluation/run_detection_evaluation.R predicted.csv reference.csv output_dir [class_col] [iou_thresholds]",
    "",
    "Example:",
    "  Rscript evaluation/run_detection_evaluation.R predicted.csv reference.csv results pattern 0.25,0.50,0.75",
    sep = "\n"
  ))
  quit(status = 0L)
}
if (length(args) < 3L || length(args) > 5L) {
  stop("Expected predicted.csv, reference.csv, output_dir, and optional class_col / comma-separated IoU thresholds.", call. = FALSE)
}

command <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", command, value = TRUE)
if (length(file_arg) != 1L) stop("Unable to resolve the evaluation script path.", call. = FALSE)
script_path <- normalizePath(sub("^--file=", "", file_arg), mustWork = TRUE)
project_root <- dirname(dirname(script_path))

evaluator <- NULL
if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(project_root, quiet = TRUE)
  evaluator <- get("stpd_evaluate_detection_results", envir = .GlobalEnv, inherits = TRUE)
} else if (requireNamespace("SpikeTrainPatternDetector", quietly = TRUE)) {
  evaluator <- getExportedValue("SpikeTrainPatternDetector", "stpd_evaluate_detection_results")
} else {
  stop("Install pkgload for a source checkout, or install SpikeTrainPatternDetector first.", call. = FALSE)
}

predicted_path <- normalizePath(args[1L], mustWork = TRUE)
reference_path <- normalizePath(args[2L], mustWork = TRUE)
output_dir <- args[3L]
class_col <- if (length(args) >= 4L) args[4L] else "pattern"
iou_thresholds <- if (length(args) >= 5L) {
  suppressWarnings(as.numeric(strsplit(args[5L], ",", fixed = TRUE)[[1L]]))
} else {
  c(0.25, 0.50, 0.75)
}

predicted <- utils::read.csv(predicted_path, stringsAsFactors = FALSE, check.names = FALSE)
reference <- utils::read.csv(reference_path, stringsAsFactors = FALSE, check.names = FALSE)
result <- evaluator(
  predicted = predicted,
  reference = reference,
  class_col = class_col,
  iou_thresholds = iou_thresholds
)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
write_output <- function(x, name) {
  utils::write.csv(as.data.frame(x), file.path(output_dir, name), row.names = FALSE, na = "")
}
write_output(result$protocol, "evaluation_protocol.csv")
write_output(result$event_metrics, "event_metrics.csv")
write_output(result$matches, "event_matches.csv")
write_output(result$isi_metrics, "isi_metrics.csv")
write_output(result$isi_metrics_by_train, "isi_metrics_by_train.csv")
saveRDS(result, file.path(output_dir, "detection_evaluation.rds"), version = 3)

cat(sprintf(
  "Evaluation complete: %d protocol level(s), %d event-metric row(s), %d matched-event row(s).\nOutput: %s\n",
  nrow(result$protocol), nrow(result$event_metrics), nrow(result$matches),
  normalizePath(output_dir, mustWork = TRUE)
))
