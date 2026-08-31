#!/usr/bin/env Rscript

# Unified truth-referenced Burst accuracy for Mean-ISI, LogISI/newBD, and STPD.
# The support-method outputs and the current zero-example STPD outputs are
# rescored against the same frozen records, score masks, and event matcher.

options(stringsAsFactors = FALSE, warn = 1)

args <- commandArgs(trailingOnly = TRUE)
n_bootstrap <- if (length(args)) as.integer(args[[1L]]) else 1000L
stopifnot(is.finite(n_bootstrap), n_bootstrap >= 200L)

command <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", command, value = TRUE)
if (length(file_arg) != 1L) stop("Unable to resolve script path.", call. = FALSE)
script_path <- normalizePath(sub("^--file=", "", file_arg), mustWork = TRUE)
repo <- normalizePath(Sys.getenv(
  "STPD_REPO_ROOT", unset = file.path(dirname(script_path), "..", "..")
), mustWork = TRUE)
pkgload::load_all(repo, quiet = TRUE)

first_existing <- function(paths) {
  hit <- paths[file.exists(paths) | dir.exists(paths)]
  if (!length(hit)) stop("Required input unavailable: ", paths[[1L]], call. = FALSE)
  normalizePath(hit[[1L]], mustWork = TRUE)
}
ratio <- function(a, b) if (is.finite(b) && b > 0) a / b else NA_real_
f1 <- function(p, r) if (is.finite(p) && is.finite(r) && p + r > 0) 2*p*r/(p+r) else NA_real_

out_dir <- file.path(repo, "test-results", "method_comparison",
                     "three_method_truth_accuracy_current")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
two_dir <- file.path(repo, "test-results", "method_comparison",
                     "meanisi_vs_logisi_burst")
validation_root <- first_existing(c(
  file.path(repo, "test-results", "multi_region_three_regime_20260831"),
  file.path(repo, "results", "real_validation", "current_automatic")
))
synthetic_prediction_root <- first_existing(c(
  file.path(repo, "test-results", "clean_synthetic_validation", "runs",
            "2026-08-30_tonic_state_repair_final_01", "v230_stage_b"),
  file.path(repo, "results", "synthetic_validation", "current_automatic")
))

support_old <- read.csv(file.path(two_dir, "support_counts_by_train.csv"),
                        check.names = FALSE)
event_old <- read.csv(file.path(two_dir, "event_counts_by_train.csv"),
                      check.names = FALSE)
support_old <- support_old[support_old$variant == "article_default", , drop = FALSE]
event_old <- event_old[event_old$variant == "article_default", , drop = FALSE]
support_old$configuration <- "article_default"
event_old$configuration <- "article_default"

runs_from_mask <- function(mask, keep, train) {
  mask <- as.logical(mask) & as.logical(keep)
  mask[is.na(mask)] <- FALSE
  index <- which(mask)
  if (!length(index)) return(data.frame(
    train = character(), pattern = character(), start_isi = integer(), end_isi = integer()
  ))
  data.frame(
    train = train, pattern = "burst",
    start_isi = index[c(TRUE, diff(index) > 1L)],
    end_isi = index[c(diff(index) > 1L, TRUE)], stringsAsFactors = FALSE
  )
}

mask_from_events <- function(events, n_isi) {
  mask <- rep(FALSE, n_isi)
  if (!is.data.frame(events) || !nrow(events)) return(mask)
  for (ii in seq_len(nrow(events))) {
    # STPD product coordinates identify right-spike positions; workbook and
    # synthetic truth rows identify the corresponding ISIs starting at one.
    first <- max(1L, as.integer(events$start_isi[[ii]]) - 1L)
    last <- min(n_isi, as.integer(events$end_isi[[ii]]) - 1L)
    if (is.finite(first) && is.finite(last) && last >= first) {
      mask[seq.int(first, last)] <- TRUE
    }
  }
  mask
}

event_overlap_counts <- function(pred, truth) {
  overlap <- matrix(FALSE, nrow(pred), nrow(truth))
  if (nrow(pred) && nrow(truth)) {
    for (ii in seq_len(nrow(pred))) {
      overlap[ii, ] <- pmax(0L, pmin(pred$end_isi[[ii]], truth$end_isi) -
        pmax(pred$start_isi[[ii]], truth$start_isi) + 1L) > 0L
    }
  }
  list(
    fragmented_truth_n = if (nrow(truth) && nrow(pred)) sum(colSums(overlap) > 1L) else 0L,
    merged_prediction_n = if (nrow(truth) && nrow(pred)) sum(rowSums(overlap) > 1L) else 0L
  )
}

score_record <- function(record, prediction, iou_grid = c(.10, .25, .50)) {
  keep <- as.logical(record$score_keep); keep[is.na(keep)] <- FALSE
  truth <- as.logical(record$truth_burst); truth[is.na(truth)] <- FALSE
  prediction <- as.logical(prediction); prediction[is.na(prediction)] <- FALSE
  stopifnot(length(keep) == length(truth), length(truth) == length(prediction))
  support <- data.frame(
    tp = sum(keep & truth & prediction), fp = sum(keep & !truth & prediction),
    fn = sum(keep & truth & !prediction), tn = sum(keep & !truth & !prediction),
    scored_isi_n = sum(keep), truth_burst_isi_n = sum(keep & truth),
    predicted_burst_isi_n = sum(keep & prediction)
  )
  pred <- runs_from_mask(prediction, keep, record$train)
  ref <- runs_from_mask(truth, keep, record$train)
  overlap <- event_overlap_counts(pred, ref)
  event <- do.call(rbind, lapply(iou_grid, function(threshold) {
    matches <- stpd_match_events_optimal(pred, ref, class_col = "pattern",
                                         iou_min = threshold)
    data.frame(
      iou_threshold = threshold, tp = nrow(matches),
      fp = nrow(pred) - nrow(matches), fn = nrow(ref) - nrow(matches),
      truth_event_n = nrow(ref), predicted_event_n = nrow(pred),
      matched_iou_sum = if (nrow(matches)) sum(matches$iou) else 0,
      matched_iou_n = nrow(matches),
      fragmented_truth_n = overlap$fragmented_truth_n,
      merged_prediction_n = overlap$merged_prediction_n
    )
  }))
  list(support = support, event = event)
}

read_synthetic <- function() {
  root <- first_existing(c(
    file.path(repo, "Simulator data", "clean_synthetic_mechanism_benchmark_v2_3_0"),
    file.path(repo, "data", "synthetic", "v2.3.0")
  ))
  spikes <- read.csv(file.path(root, "detector_inputs", "spike_timestamps_blinded.csv"),
                     check.names = FALSE)
  truth <- read.csv(file.path(root, "truth", "interval_truth_multitrack.csv"),
                    check.names = FALSE)
  truth <- truth[tolower(truth$Split) == "holdout", , drop = FALSE]
  spike_split <- split(spikes, spikes$Sample_ID)
  truth_split <- split(truth, truth$Sample_ID)
  records <- list()
  for (sample in sort(unique(truth$Sample_ID), method = "radix")) {
    s <- spike_split[[sample]][order(spike_split[[sample]]$Spike_Index), , drop = FALSE]
    z <- truth_split[[sample]][order(truth_split[[sample]]$Interval_Index), , drop = FALSE]
    stopifnot(nrow(s) == nrow(z) + 1L)
    common <- list(
      train = sample, cluster = as.character(z$Template_ID[[1L]]),
      dataset = "synthetic_v2.3_holdout", region = "synthetic",
      scale = paste0(z$Scale_Factor[[1L]], "x"), n_isi = nrow(z)
    )
    strict <- common
    strict$estimand <- "strict_mechanism"
    strict$score_keep <- rep(TRUE, nrow(z))
    strict$truth_burst <- z$Event_Label == "Burst"
    phenotype <- common
    phenotype$estimand <- "observable_phenotype_primary"
    phenotype$score_keep <- z$Phenotype_Label != "ambiguous_burst_like"
    phenotype$truth_burst <- z$Phenotype_Label == "clear_burst_like"
    records[[length(records) + 1L]] <- strict
    records[[length(records) + 1L]] <- phenotype
  }
  records
}

read_real <- function() {
  workbooks <- c(
    GPe = first_existing(c(
      file.path(repo, "PD_GPe", "Bagdasaryan_PD_GPe_2020_manual_isi_labels_draft_csv.xlsx"),
      file.path(repo, "data", "real", "GPe", "PD_GPe_manual_isi_labels.xlsx"),
      file.path(repo, "data", "real", "GPe", "Bagdasaryan_PD_GPe_2020_manual_isi_labels_draft_csv.xlsx")
    )),
    STN = first_existing(c(
      file.path(repo, "PD_STN", "PD_STN_Grechishnikova_2017_manual_isi_labels_draft_csv.xlsx"),
      file.path(repo, "data", "real", "STN", "PD_STN_manual_isi_labels.xlsx"),
      file.path(repo, "data", "real", "STN", "PD_STN_Grechishnikova_2017_manual_isi_labels_draft_csv.xlsx")
    )),
    GPi = first_existing(c(
      file.path(repo, "PD_GPi", "Kurmanaeva_PD_GPi_2017_manual_isi_labels_draft_csv.xlsx"),
      file.path(repo, "data", "real", "GPi", "PD_GPi_manual_isi_labels.xlsx"),
      file.path(repo, "data", "real", "GPi", "Kurmanaeva_PD_GPi_2017_manual_isi_labels_draft_csv.xlsx")
    ))
  )
  eligible <- unique(support_old[support_old$region != "synthetic", c("dataset", "train")])
  records <- list()
  for (region in names(workbooks)) {
    for (sheet in readxl::excel_sheets(workbooks[[region]])) {
      z <- as.data.frame(readxl::read_excel(workbooks[[region]], sheet = sheet))
      z <- z[order(as.integer(z$isi_index)), , drop = FALSE]
      train <- as.character(z$train_id[[1L]])
      dataset <- paste0("real_", region)
      if (!any(eligible$dataset == dataset & eligible$train == train)) next
      reviewed <- tolower(trimws(as.character(z$review_status))) == "manually_labeled"
      label <- tolower(trimws(as.character(z$event_pattern)))
      records[[length(records) + 1L]] <- list(
        train = train, cluster = train, dataset = dataset, region = region,
        scale = "observed", estimand = "reviewed_manual_reference",
        score_keep = reviewed, truth_burst = reviewed & !is.na(label) & label == "burst",
        n_isi = nrow(z)
      )
    }
  }
  records
}

synthetic_events <- do.call(rbind, lapply(
  list.files(synthetic_prediction_root, pattern = "^predicted_episodes_.*[.]csv$",
             full.names = TRUE), read.csv, check.names = FALSE
))
synthetic_events <- synthetic_events[
  synthetic_events$Axis == "event" & synthetic_events$Pattern == "burst", , drop = FALSE
]
real_events <- list()
region_dirs <- c(GPe = "GPE", STN = "STN", GPi = "GPI")
for (region in names(region_dirs)) {
  z <- read.csv(file.path(validation_root, region_dirs[[region]], "automatic",
                          "predicted_events.csv"),
                check.names = FALSE)
  if ("repeat_id" %in% names(z)) z <- z[z$repeat_id == min(z$repeat_id), , drop = FALSE]
  z <- z[z$Axis == "event" & z$Pattern == "burst", , drop = FALSE]
  z$region <- region
  real_events[[region]] <- z
}

records <- c(read_synthetic(), read_real())
stpd_support <- list(); stpd_event <- list()
for (record in records) {
  if (record$region == "synthetic") {
    events <- synthetic_events[synthetic_events$Train_ID == record$train, , drop = FALSE]
  } else {
    events <- real_events[[record$region]]
    events <- events[events$Train_ID == record$train, , drop = FALSE]
  }
  scored <- score_record(record, mask_from_events(events, record$n_isi))
  base <- data.frame(
    dataset = record$dataset, region = record$region, estimand = record$estimand,
    scale = record$scale, cluster = record$cluster, train = record$train,
    method = "STPD", variant = "current_automatic",
    configuration = "current_automatic", stringsAsFactors = FALSE
  )
  stpd_support[[length(stpd_support) + 1L]] <- cbind(base, scored$support)
  stpd_event[[length(stpd_event) + 1L]] <- cbind(base, scored$event)
}

support_old <- support_old[, intersect(names(support_old), names(stpd_support[[1L]])), drop = FALSE]
event_old <- event_old[, intersect(names(event_old), names(stpd_event[[1L]])), drop = FALSE]
support <- rbind(support_old, do.call(rbind, stpd_support))
event <- rbind(event_old, do.call(rbind, stpd_event))

group_cols <- c("dataset", "region", "estimand", "scale", "method", "configuration")
aggregate_support <- function(z) {
  totals <- colSums(z[c("tp", "fp", "fn", "tn", "scored_isi_n",
                        "truth_burst_isi_n", "predicted_burst_isi_n")])
  p <- ratio(totals[["tp"]], totals[["tp"]] + totals[["fp"]])
  r <- ratio(totals[["tp"]], totals[["tp"]] + totals[["fn"]])
  data.frame(z[1L, group_cols, drop = FALSE], train_n = length(unique(z$train)),
    cluster_n = length(unique(z$cluster)), t(totals), precision = p, recall = r,
    f1 = f1(p, r), stringsAsFactors = FALSE)
}
support_metrics <- do.call(rbind, lapply(split(
  support, interaction(support[group_cols], drop = TRUE, lex.order = TRUE)
), aggregate_support))

event_group <- c(group_cols, "iou_threshold")
event_metrics <- do.call(rbind, lapply(split(
  event, interaction(event[event_group], drop = TRUE, lex.order = TRUE)
), function(z) {
  tp <- sum(z$tp); fp <- sum(z$fp); fn <- sum(z$fn)
  p <- ratio(tp, tp + fp); r <- ratio(tp, tp + fn)
  data.frame(z[1L, event_group, drop = FALSE], train_n = length(unique(z$train)),
    cluster_n = length(unique(z$cluster)), tp = tp, fp = fp, fn = fn,
    truth_event_n = sum(z$truth_event_n), predicted_event_n = sum(z$predicted_event_n),
    precision = p, recall = r, f1 = f1(p, r),
    mean_matched_iou = ratio(sum(z$matched_iou_sum), sum(z$matched_iou_n)),
    fragmentation_rate = ratio(sum(z$fragmented_truth_n), sum(z$truth_event_n)),
    merge_rate = ratio(sum(z$merged_prediction_n), sum(z$predicted_event_n)),
    stringsAsFactors = FALSE)
}))

bootstrap_groups <- unique(support[c("dataset", "region", "estimand", "scale")])
draw_rows <- list()
for (gg in seq_len(nrow(bootstrap_groups))) {
  group <- bootstrap_groups[gg, , drop = FALSE]
  keep <- rep(TRUE, nrow(support))
  for (name in names(group)) keep <- keep & support[[name]] == group[[name]]
  z <- support[keep, , drop = FALSE]
  clusters <- sort(unique(z$cluster), method = "radix")
  set.seed(271000L + gg)
  for (bb in seq_len(n_bootstrap)) {
    sampled <- sample(clusters, length(clusters), replace = TRUE)
    q <- do.call(rbind, lapply(seq_along(sampled), function(ii) {
      part <- z[z$cluster == sampled[[ii]], , drop = FALSE]
      part$cluster <- paste0(part$cluster, "__", ii); part
    }))
    for (method in c("Mean-ISI", "LogISI/newBD", "STPD")) {
      m <- q[q$method == method, , drop = FALSE]
      tp <- sum(m$tp); fp <- sum(m$fp); fn <- sum(m$fn)
      p <- ratio(tp, tp + fp); r <- ratio(tp, tp + fn)
      draw_rows[[length(draw_rows) + 1L]] <- data.frame(
        group, bootstrap_replicate = bb, method = method,
        precision = p, recall = r, f1 = f1(p, r), stringsAsFactors = FALSE)
    }
  }
}
draws <- do.call(rbind, draw_rows)
ci <- do.call(rbind, lapply(split(
  draws, interaction(draws[c("dataset", "region", "estimand", "scale", "method")],
                     drop = TRUE, lex.order = TRUE)
), function(z) do.call(rbind, lapply(c("precision", "recall", "f1"), function(metric) {
  values <- z[[metric]][is.finite(z[[metric]])]
  observed <- support_metrics[
    support_metrics$dataset == z$dataset[[1L]] &
      support_metrics$region == z$region[[1L]] &
      support_metrics$estimand == z$estimand[[1L]] &
      support_metrics$scale == z$scale[[1L]] &
      support_metrics$method == z$method[[1L]], , drop = FALSE]
  bounds <- quantile(values, c(.025, .975), names = FALSE, type = 6)
  data.frame(z[1L, c("dataset", "region", "estimand", "scale", "method")],
    metric = metric, observed = observed[[metric]][[1L]],
    ci_low = bounds[[1L]], ci_high = bounds[[2L]], bootstrap_n = length(values),
    bootstrap_unit = if (z$region[[1L]] == "synthetic") "Template_ID" else "train")
}))))

write.csv(support, file.path(out_dir, "support_counts_by_train.csv"), row.names = FALSE)
write.csv(event, file.path(out_dir, "event_counts_by_train.csv"), row.names = FALSE)
write.csv(support_metrics, file.path(out_dir, "support_metrics.csv"), row.names = FALSE)
write.csv(event_metrics, file.path(out_dir, "event_metrics.csv"), row.names = FALSE)
write.csv(ci, file.path(out_dir, "cluster_bootstrap_95ci.csv"), row.names = FALSE)
saveRDS(draws, file.path(out_dir, "cluster_bootstrap_draws.rds"))

protocol <- data.frame(item = c(
  "purpose", "methods", "truth_used_for_thresholds", "real_score_mask",
  "synthetic_release_split", "event_matching", "event_iou_thresholds",
  "bootstrap_units", "coordinate_conversion"
), value = c(
  "unified truth-referenced three-method Burst accuracy",
  "Mean-ISI article default; LogISI/newBD article default; STPD current automatic zero-example",
  "none", "review_status == manually_labeled; same eligible trains for all methods",
  "v2.3.0 holdout", stpd_event_matching_rule(), "0.10;0.25;0.50",
  "real=train; synthetic=Template_ID",
  "STPD right-spike product coordinates converted to one-based ISI truth rows by subtracting one"
), stringsAsFactors = FALSE)
write.csv(protocol, file.path(out_dir, "protocol.csv"), row.names = FALSE)

headline_s <- support_metrics[
  support_metrics$estimand %in% c("reviewed_manual_reference", "strict_mechanism"), ]
headline_e <- event_metrics[
  event_metrics$estimand %in% c("reviewed_manual_reference", "strict_mechanism") &
    abs(event_metrics$iou_threshold - .25) < 1e-12, ]
fmt <- function(x) ifelse(is.finite(x), sprintf("%.3f", x), "NA")
lines <- c(
  "# Unified three-method Burst accuracy", "",
  "All three automatic methods are rescored on identical truth records and scoring rules. No truth label is used to estimate a detector threshold.", "",
  "## ISI-support metrics", "",
  "| Dataset | Region | Scale | Method | Precision | Recall | F1 |",
  "|---|---|---|---|---:|---:|---:|"
)
for (ii in seq_len(nrow(headline_s))) {
  z <- headline_s[ii, ]
  lines <- c(lines, sprintf("| %s | %s | %s | %s | %s | %s | %s |",
    z$dataset, z$region, z$scale, z$method, fmt(z$precision), fmt(z$recall), fmt(z$f1)))
}
lines <- c(lines, "", "## Event metrics (IoU >= 0.25)", "",
  "| Dataset | Region | Scale | Method | Precision | Recall | F1 | Mean matched IoU |",
  "|---|---|---|---|---:|---:|---:|---:|")
for (ii in seq_len(nrow(headline_e))) {
  z <- headline_e[ii, ]
  lines <- c(lines, sprintf("| %s | %s | %s | %s | %s | %s | %s | %s |",
    z$dataset, z$region, z$scale, z$method, fmt(z$precision), fmt(z$recall),
    fmt(z$f1), fmt(z$mean_matched_iou)))
}
lines <- c(lines, "", "## Safeguards", "",
  "- Real labels remain draft; estimates are exploratory until annotation freeze.",
  "- GPe, STN, and GPi are not pooled as independent patients.",
  "- Synthetic scales are reported separately and share Template_ID bootstrap clusters.")
writeLines(lines, file.path(out_dir, "RESULTS.md"), useBytes = TRUE)

checks <- data.frame(
  check = c("three_methods", "same_truth_denominators", "support_identities",
            "event_identities", "metric_bounds", "bootstrap_complete"),
  pass = c(
    setequal(unique(support$method), c("Mean-ISI", "LogISI/newBD", "STPD")),
    all(vapply(split(support, interaction(support[c("dataset", "estimand", "scale", "train")], drop=TRUE)),
      function(z) length(unique(z$truth_burst_isi_n)) == 1L && length(unique(z$scored_isi_n)) == 1L, logical(1))),
    all(support$tp + support$fp == support$predicted_burst_isi_n) &&
      all(support$tp + support$fn == support$truth_burst_isi_n),
    all(event$tp + event$fp == event$predicted_event_n) &&
      all(event$tp + event$fn == event$truth_event_n),
    all(unlist(support_metrics[c("precision", "recall", "f1")]) >= 0 &
          unlist(support_metrics[c("precision", "recall", "f1")]) <= 1, na.rm = TRUE),
    all(ci$bootstrap_n == n_bootstrap) && all(ci$ci_low <= ci$ci_high)
  ), stringsAsFactors = FALSE
)
write.csv(checks, file.path(out_dir, "validation_checks.csv"), row.names = FALSE)
if (any(!checks$pass)) stop("Unified comparison validation failed: ",
  paste(checks$check[!checks$pass], collapse = "; "), call. = FALSE)

manifest_files <- list.files(out_dir, full.names = TRUE)
manifest <- data.frame(file = basename(manifest_files), sha256 = vapply(
  manifest_files, digest::digest, character(1), algo = "sha256", serialize = FALSE,
  file = TRUE
))
write.csv(manifest, file.path(out_dir, "manifest_checksums.csv"), row.names = FALSE)
message("Unified three-method accuracy complete: ", out_dir)
