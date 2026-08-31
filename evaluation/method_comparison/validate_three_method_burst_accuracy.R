#!/usr/bin/env Rscript

# Validate the frozen unified three-method Burst accuracy result contract.

options(stringsAsFactors = FALSE, warn = 1)

command <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", command, value = TRUE)
if (length(file_arg) != 1L) stop("Unable to resolve script path.", call. = FALSE)
script_path <- normalizePath(sub("^--file=", "", file_arg), mustWork = TRUE)
repo <- normalizePath(Sys.getenv(
  "STPD_REPO_ROOT", unset = file.path(dirname(script_path), "..", "..")
), mustWork = TRUE)

candidates <- c(
  Sys.getenv("STPD_THREE_METHOD_RESULT_DIR", unset = ""),
  file.path(repo, "test-results", "method_comparison",
            "three_method_truth_accuracy_current"),
  file.path(repo, "results", "method_comparison",
            "three_method_truth_accuracy_current")
)
candidates <- candidates[nzchar(candidates) & dir.exists(candidates)]
if (!length(candidates)) stop("Three-method result directory is unavailable.",
                              call. = FALSE)
out_dir <- normalizePath(candidates[[1L]], mustWork = TRUE)

required <- c(
  "analysis_scope.csv", "support_metrics.csv", "event_metrics.csv",
  "cluster_bootstrap_95ci.csv", "event_cluster_bootstrap_95ci.csv",
  "protocol.csv", "validation_checks.csv", "RESULTS.md", "figure_caption.md"
)
missing <- required[!file.exists(file.path(out_dir, required))]
if (length(missing)) stop("Missing result files: ", paste(missing, collapse = ", "),
                          call. = FALSE)

read_result <- function(name) read.csv(
  file.path(out_dir, name), check.names = FALSE, stringsAsFactors = FALSE
)
scope <- read_result("analysis_scope.csv")
support <- read_result("support_metrics.csv")
event <- read_result("event_metrics.csv")
support_ci <- read_result("cluster_bootstrap_95ci.csv")
event_ci <- read_result("event_cluster_bootstrap_95ci.csv")
protocol <- read_result("protocol.csv")
checks <- read_result("validation_checks.csv")

fail <- character()
expect <- function(value, label) {
  if (!isTRUE(value)) fail <<- c(fail, label)
}
ratio <- function(a, b) ifelse(b > 0, a / b, NA_real_)
f1 <- function(p, r) ifelse(p + r > 0, 2 * p * r / (p + r), NA_real_)
near <- function(a, b, tolerance = 1e-12) {
  identical(is.na(a), is.na(b)) && all(abs(a[!is.na(a)] - b[!is.na(b)]) <= tolerance)
}

methods <- c("Mean-ISI", "LogISI/newBD", "STPD")
expect(setequal(unique(support$method), methods), "support method set")
expect(setequal(unique(event$method), methods), "event method set")
expect(setequal(unique(event$iou_threshold), c(.10, .25, .50)),
       "event IoU threshold set")
expect(all(checks$pass), "embedded validation checks")

support_p <- ratio(support$tp, support$tp + support$fp)
support_r <- ratio(support$tp, support$tp + support$fn)
event_p <- ratio(event$tp, event$tp + event$fp)
event_r <- ratio(event$tp, event$tp + event$fn)
expect(near(support$precision, support_p) && near(support$recall, support_r) &&
         near(support$f1, f1(support_p, support_r)), "support metric identities")
expect(near(event$precision, event_p) && near(event$recall, event_r) &&
         near(event$f1, f1(event_p, event_r)), "event metric identities")

scope_key <- c("dataset", "region", "estimand", "scale")
check_scope <- function(metrics) all(vapply(seq_len(nrow(metrics)), function(ii) {
  z <- metrics[ii, ]
  keep <- rep(TRUE, nrow(scope))
  for (name in scope_key) keep <- keep & scope[[name]] == z[[name]]
  q <- scope[keep, , drop = FALSE]
  nrow(q) == 1L && q$train_n == z$train_n && q$cluster_n == z$cluster_n
}, logical(1)))
expect(check_scope(support), "support scope counts")
expect(check_scope(event), "event scope counts")
expect(all(scope$cluster_unit == ifelse(scope$region == "synthetic",
                                       "Template_ID", "Group_ID")),
       "scope cluster units")
expect(all(grepl("event_reference_eligible", scope$eligibility_rule[
  scope$region != "synthetic"], fixed = TRUE)), "real eligibility definition")

real_expected <- data.frame(
  region = c("GPe", "STN", "GPi"), train_n = c(13L, 23L, 21L),
  cluster_n = c(13L, 13L, 13L), stringsAsFactors = FALSE
)
real_scope <- unique(scope[scope$region != "synthetic",
                           c("region", "train_n", "cluster_n")])
real_scope <- real_scope[match(real_expected$region, real_scope$region), ]
expect(identical(as.character(real_scope$region), real_expected$region) &&
         identical(as.integer(real_scope$train_n), real_expected$train_n) &&
         identical(as.integer(real_scope$cluster_n), real_expected$cluster_n),
       "frozen real train/Group_ID counts")
expect(all(scope$train_n[scope$region == "synthetic"] == 15L) &&
         all(scope$cluster_n[scope$region == "synthetic"] == 15L),
       "frozen synthetic holdout Template_ID counts")

check_ci <- function(ci, metrics, event_level = FALSE) {
  expected_n <- nrow(metrics) * 3L
  rows_ok <- nrow(ci) == expected_n &&
    setequal(unique(ci$metric), c("precision", "recall", "f1")) &&
    all(ci$bootstrap_n == 1000L) && all(ci$ci_low <= ci$ci_high) &&
    all(ci$bootstrap_unit == ifelse(ci$region == "synthetic",
                                   "Template_ID", "Group_ID"))
  values_ok <- all(vapply(seq_len(nrow(ci)), function(ii) {
    z <- ci[ii, ]
    keep <- metrics$dataset == z$dataset & metrics$region == z$region &
      metrics$estimand == z$estimand & metrics$scale == z$scale &
      metrics$method == z$method
    if (event_level) {
      keep <- keep & abs(metrics$iou_threshold - z$iou_threshold) < 1e-12
    }
    q <- metrics[keep, , drop = FALSE]
    nrow(q) == 1L && near(z$observed, q[[z$metric]][[1L]])
  }, logical(1)))
  rows_ok && values_ok
}
expect(check_ci(support_ci, support), "support cluster-bootstrap result contract")
expect(check_ci(event_ci, event, TRUE), "event cluster-bootstrap result contract")
expect(setequal(unique(event_ci$iou_threshold), c(.10, .25, .50)),
       "event CI IoU threshold set")

protocol_value <- stats::setNames(protocol$value, protocol$item)
expect(identical(unname(protocol_value[["bootstrap_units"]]),
                 "real=Group_ID; synthetic=Template_ID"), "protocol bootstrap units")
expect(grepl("event_reference_eligible", protocol_value[["real_eligibility"]],
             fixed = TRUE), "protocol eligibility")
expect(grepl("event precision/recall/F1", protocol_value[["bootstrap_intervals"]],
             fixed = TRUE), "protocol event intervals")

if (length(fail)) {
  stop("Three-method result validation failed: ", paste(fail, collapse = "; "),
       call. = FALSE)
}
message("Three-method Burst accuracy validation passed: ", out_dir)
