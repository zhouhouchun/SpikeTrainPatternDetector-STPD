#!/usr/bin/env Rscript

# Arithmetic, scope, snapshot, and artifact checks for the current three-method
# Burst agreement audit.

options(stringsAsFactors = FALSE)

command <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", command, value = TRUE)
if (length(file_arg) != 1L) stop("Unable to resolve script path.", call. = FALSE)
script_path <- normalizePath(sub("^--file=", "", file_arg), mustWork = TRUE)
repo_default <- normalizePath(file.path(dirname(script_path), "..", ".."))
repo <- normalizePath(
  Sys.getenv("STPD_REPO_ROOT", unset = repo_default), mustWork = TRUE
)
first_existing <- function(paths) {
  found <- paths[file.exists(paths) | dir.exists(paths)]
  if (!length(found)) stop("Required input is unavailable: ", paths[[1L]], call. = FALSE)
  found[[1L]]
}
out_dir <- file.path(repo, "test-results", "method_comparison",
                     "three_method_agreement_current")
pair_train <- read.csv(file.path(out_dir, "pairwise_agreement_by_train.csv"),
                       check.names = FALSE, stringsAsFactors = FALSE)
pair_metrics <- read.csv(file.path(out_dir, "pairwise_agreement_metrics.csv"),
                         check.names = FALSE, stringsAsFactors = FALSE)
three_train <- read.csv(file.path(out_dir, "three_way_support_by_train.csv"),
                        check.names = FALSE, stringsAsFactors = FALSE)
three_metrics <- read.csv(file.path(out_dir, "three_way_support_metrics.csv"),
                          check.names = FALSE, stringsAsFactors = FALSE)
scope <- read.csv(file.path(out_dir, "comparison_scope.csv"),
                  check.names = FALSE, stringsAsFactors = FALSE)
ci <- read.csv(file.path(out_dir, "recording_group_bootstrap_95ci.csv"),
               check.names = FALSE, stringsAsFactors = FALSE)
protocol <- read.csv(file.path(out_dir, "protocol.csv"),
                     check.names = FALSE, stringsAsFactors = FALSE)

checks <- list()
add_check <- function(name, pass, detail) {
  checks[[length(checks) + 1L]] <<- data.frame(
    check = name, pass = isTRUE(pass), detail = as.character(detail),
    stringsAsFactors = FALSE
  )
}

add_check(
  "scope_region_counts",
  identical(as.integer(table(factor(scope$region,
    levels = c("GPE", "STN", "GPI")))), c(13L, 23L, 21L)),
  "current event-reference-eligible scope is GPe=13, STN=23, GPi=21 trains"
)
add_check(
  "event_match_cardinality",
  all(pair_train$matched_event_n <= pair_train$a_event_n) &&
    all(pair_train$matched_event_n <= pair_train$b_event_n),
  "one-to-one matches never exceed either method event count"
)
add_check(
  "support_intersection_bounds",
  all(pair_train$overlapping_isi_n <= pair_train$a_isi_n) &&
    all(pair_train$overlapping_isi_n <= pair_train$b_isi_n) &&
    all(pair_train$union_isi_n >= pair_train$a_isi_n) &&
    all(pair_train$union_isi_n >= pair_train$b_isi_n),
  "pairwise ISI intersection and union satisfy set bounds"
)
add_check(
  "pairwise_metric_bounds",
  all(unlist(pair_metrics[c(
    "a_event_recovery", "b_event_recovery", "event_f1",
    "mean_matched_iou", "a_isi_coverage", "b_isi_coverage",
    "isi_jaccard", "isi_dice"
  )]) >= 0 & unlist(pair_metrics[c(
    "a_event_recovery", "b_event_recovery", "event_f1",
    "mean_matched_iou", "a_isi_coverage", "b_isi_coverage",
    "isi_jaccard", "isi_dice"
  )]) <= 1, na.rm = TRUE),
  "all agreement rates lie in [0, 1]"
)
add_check(
  "three_way_set_bounds",
  all(three_train$three_way_intersection_isi_n <= three_train$meanisi_isi_n) &&
    all(three_train$three_way_intersection_isi_n <= three_train$logisi_isi_n) &&
    all(three_train$three_way_intersection_isi_n <= three_train$stpd_isi_n) &&
    all(three_train$three_way_union_isi_n >= three_train$meanisi_isi_n) &&
    all(three_train$three_way_union_isi_n >= three_train$logisi_isi_n) &&
    all(three_train$three_way_union_isi_n >= three_train$stpd_isi_n),
  "three-way intersection is no larger than any set and union is no smaller"
)
recomputed_three <- do.call(rbind, lapply(split(three_train, three_train$region),
  function(z) data.frame(
    region = z$region[[1L]],
    common = sum(z$three_way_intersection_isi_n),
    union = sum(z$three_way_union_isi_n),
    jaccard = sum(z$three_way_intersection_isi_n) /
      sum(z$three_way_union_isi_n), stringsAsFactors = FALSE
  )))
matched_three <- merge(three_metrics, recomputed_three, by = "region")
add_check(
  "three_way_pooled_recompute",
  all(matched_three$three_way_intersection_isi_n == matched_three$common) &&
    all(matched_three$three_way_union_isi_n == matched_three$union) &&
    all(abs(matched_three$three_way_jaccard - matched_three$jaccard) < 1e-12),
  "three-way pooled totals and Jaccard recompute from saved train rows"
)
add_check(
  "bootstrap_complete",
  all(ci$bootstrap_n == 1000L) && all(ci$ci_low <= ci$ci_high),
  "1,000 recording-group bootstrap replicates are summarized"
)
add_check(
  "agreement_not_accuracy",
  identical(protocol$value[protocol$item == "purpose"],
    "descriptive three-method Burst agreement; not truth accuracy"),
  "protocol explicitly separates agreement from truth-referenced accuracy"
)
add_check(
  "detectors_label_blind",
  identical(protocol$value[protocol$item == "manual_labels_used_by_detectors"],
            "no"),
  "manual labels do not enter any detector"
)

validation_root <- first_existing(c(
  file.path(repo, "test-results", "multi_region_three_regime_20260831"),
  file.path(repo, "results", "real_validation", "current_automatic")
))
stpd_outputs <- file.path(validation_root, c("GPE", "STN", "GPI"),
                          "automatic", "predicted_events.csv")
r_files <- list.files(file.path(repo, "R"), pattern = "[.]R$", full.names = TRUE)
snapshot_current <- max(file.info(r_files)$mtime) <= min(file.info(stpd_outputs)$mtime)
add_check(
  "stpd_snapshot_not_stale",
  snapshot_current,
  "no detector R source file is newer than the reused automatic STPD outputs"
)
add_check(
  "publication_figure_present",
  all(file.exists(file.path(out_dir, c(
    "three_method_two_train_raster.pdf",
    "three_method_two_train_raster.png",
    "figure_selection_and_windows.csv"
  )))),
  "R-generated PDF/PNG and deterministic figure-selection audit exist"
)

checks <- do.call(rbind, checks)
write.csv(checks, file.path(out_dir, "validation_checks.csv"), row.names = FALSE)
if (any(!checks$pass)) {
  stop("Three-method validation failed: ",
       paste(checks$check[!checks$pass], collapse = "; "), call. = FALSE)
}
message("All ", nrow(checks), " three-method agreement checks passed.")
