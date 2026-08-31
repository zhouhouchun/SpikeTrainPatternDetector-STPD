#!/usr/bin/env Rscript

# Reproducibility and arithmetic checks for the direct Mean-ISI versus
# LogISI/newBD Burst comparison.

options(stringsAsFactors = FALSE)

command <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", command, value = TRUE)
if (length(file_arg) != 1L) stop("Unable to resolve script path.", call. = FALSE)
script_path <- normalizePath(sub("^--file=", "", file_arg), mustWork = TRUE)
repo_default <- normalizePath(file.path(dirname(script_path), "..", ".."))
repo <- normalizePath(
  Sys.getenv("STPD_REPO_ROOT", unset = repo_default), mustWork = TRUE
)
out_dir <- file.path(repo, "test-results", "method_comparison",
                     "meanisi_vs_logisi_burst")
support <- read.csv(file.path(out_dir, "support_metrics.csv"),
                    check.names = FALSE, stringsAsFactors = FALSE)
events <- read.csv(file.path(out_dir, "event_metrics.csv"),
                   check.names = FALSE, stringsAsFactors = FALSE)
counts <- read.csv(file.path(out_dir, "support_counts_by_train.csv"),
                   check.names = FALSE, stringsAsFactors = FALSE)
contrast <- read.csv(file.path(out_dir, "paired_cluster_bootstrap_f1_contrast.csv"),
                     check.names = FALSE, stringsAsFactors = FALSE)
protocol <- read.csv(file.path(out_dir, "protocol.csv"),
                     check.names = FALSE, stringsAsFactors = FALSE)

tolerance <- 1e-12
checks <- list()
add_check <- function(name, pass, detail) {
  checks[[length(checks) + 1L]] <<- data.frame(
    check = name, pass = isTRUE(pass), detail = as.character(detail),
    stringsAsFactors = FALSE
  )
}

add_check(
  "support_counts_partition_scored_intervals",
  all(counts$tp + counts$fp + counts$fn + counts$tn == counts$scored_isi_n),
  "tp + fp + fn + tn equals scored_isi_n for every train/method/variant"
)
add_check(
  "support_truth_denominator",
  all(counts$tp + counts$fn == counts$truth_burst_isi_n),
  "tp + fn equals truth_burst_isi_n"
)
add_check(
  "support_prediction_denominator",
  all(counts$tp + counts$fp == counts$predicted_burst_isi_n),
  "tp + fp equals predicted_burst_isi_n"
)
add_check(
  "support_metric_bounds",
  all(unlist(support[c("precision", "recall", "specificity", "f1",
                       "accuracy", "balanced_accuracy")]) >= 0 &
        unlist(support[c("precision", "recall", "specificity", "f1",
                         "accuracy", "balanced_accuracy")]) <= 1,
      na.rm = TRUE),
  "all probability metrics lie in [0, 1]"
)
recomputed_precision <- with(support, tp / (tp + fp))
recomputed_recall <- with(support, tp / (tp + fn))
recomputed_f1 <- with(support,
  2 * recomputed_precision * recomputed_recall /
    (recomputed_precision + recomputed_recall))
add_check(
  "support_metric_formulae",
  all(abs(support$precision - recomputed_precision) < tolerance, na.rm = TRUE) &&
    all(abs(support$recall - recomputed_recall) < tolerance, na.rm = TRUE) &&
    all(abs(support$f1 - recomputed_f1) < tolerance, na.rm = TRUE),
  "pooled precision, recall, and F1 recompute from saved counts"
)
add_check(
  "event_count_identities",
  all(events$tp + events$fp == events$predicted_event_n) &&
    all(events$tp + events$fn == events$truth_event_n),
  "event TP/FP/FN reconcile with predicted and truth event counts"
)
add_check(
  "event_metric_bounds",
  all(unlist(events[c("precision", "recall", "f1", "mean_matched_iou",
                      "fragmentation_rate", "merge_rate")]) >= 0 &
        unlist(events[c("precision", "recall", "f1", "mean_matched_iou",
                        "fragmentation_rate", "merge_rate")]) <= 1,
      na.rm = TRUE),
  "all event-level rates lie in [0, 1]"
)
synthetic <- support[support$dataset == "synthetic_v2.3_holdout", , drop = FALSE]
add_check(
  "synthetic_scales_separate",
  setequal(unique(synthetic$scale), c("1x", "4x", "10x")) &&
    all(synthetic$train_n == 15L) && all(synthetic$cluster_n == 15L),
  "each scale contains the 15 holdout Template_ID clusters and is reported separately"
)
add_check(
  "real_regions_separate",
  setequal(unique(support$region[support$dataset != "synthetic_v2.3_holdout"]),
           c("GPe", "STN", "GPi")),
  "GPe, STN, and GPi are not pooled into one biological sample"
)
add_check(
  "bootstrap_complete",
  all(contrast$bootstrap_n == 1000L) &&
    all(contrast$ci_low <= contrast$ci_high),
  "1,000 paired cluster-bootstrap replicates are present per comparison"
)
add_check(
  "label_blind_protocol",
  identical(protocol$value[protocol$item == "threshold_learning_from_truth"],
            "none; both methods are label-blind"),
  "no truth-derived threshold is declared"
)
add_check(
  "publication_figure_present",
  all(file.exists(file.path(out_dir, c(
    "meanisi_logisi_burst_performance.pdf",
    "meanisi_logisi_burst_performance.png",
    "cluster_bootstrap_method_metrics_95ci.csv"
  )))),
  "R-generated PDF/PNG and plotted confidence-interval table exist"
)

checks <- do.call(rbind, checks)
write.csv(checks, file.path(out_dir, "validation_checks.csv"), row.names = FALSE)
if (any(!checks$pass)) {
  stop("Method-comparison validation failed: ",
       paste(checks$check[!checks$pass], collapse = "; "), call. = FALSE)
}
message("All ", nrow(checks), " method-comparison checks passed.")
