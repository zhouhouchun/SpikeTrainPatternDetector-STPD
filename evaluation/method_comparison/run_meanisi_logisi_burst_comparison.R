#!/usr/bin/env Rscript

# Direct Burst-performance comparison of the Mean-ISI and Pasquale LogISI/newBD
# support methods. This script evaluates the methods against frozen synthetic
# truth and reviewed manual real-data intervals. It does not import either
# method into STPD and never uses reference labels to choose a threshold.

options(stringsAsFactors = FALSE, warn = 1)

args <- commandArgs(trailingOnly = TRUE)
workers <- if (length(args) >= 1L) as.integer(args[[1L]]) else 3L
n_bootstrap <- if (length(args) >= 2L) as.integer(args[[2L]]) else 1000L
stopifnot(is.finite(workers), workers >= 1L,
          is.finite(n_bootstrap), n_bootstrap >= 200L)

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
                     "meanisi_vs_logisi_burst")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

pkgload::load_all(repo, quiet = TRUE)

num1 <- function(x, default = NA_real_) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) && is.finite(x[[1L]])) x[[1L]] else default
}

chr1 <- function(x, default = "") {
  x <- as.character(x)
  if (length(x) && !is.na(x[[1L]])) x[[1L]] else default
}

safe_ratio <- function(numerator, denominator) {
  if (is.finite(denominator) && denominator > 0) numerator / denominator else NA_real_
}

f1_score <- function(precision, recall) {
  if (is.finite(precision) && is.finite(recall) && precision + recall > 0) {
    2 * precision * recall / (precision + recall)
  } else {
    NA_real_
  }
}

mcc_score <- function(tp, fp, fn, tn) {
  denominator <- sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
  if (is.finite(denominator) && denominator > 0) {
    (tp * tn - fp * fn) / denominator
  } else {
    NA_real_
  }
}

runs_from_mask <- function(mask, keep = rep(TRUE, length(mask)), train = "") {
  mask <- as.logical(mask) & as.logical(keep)
  mask[is.na(mask)] <- FALSE
  index <- which(mask)
  if (!length(index)) {
    return(data.frame(train = character(), pattern = character(),
                      start_isi = integer(), end_isi = integer()))
  }
  starts <- index[c(TRUE, diff(index) > 1L)]
  ends <- index[c(diff(index) > 1L, TRUE)]
  data.frame(
    train = rep(as.character(train), length(starts)),
    pattern = rep("burst", length(starts)),
    start_isi = as.integer(starts), end_isi = as.integer(ends),
    stringsAsFactors = FALSE
  )
}

mask_from_bursts <- function(bursts, n_isi) {
  out <- rep(FALSE, n_isi)
  if (!is.data.frame(bursts) || !nrow(bursts)) return(out)
  for (ii in seq_len(nrow(bursts))) {
    first <- max(1L, as.integer(bursts$start_isi[[ii]]))
    last <- min(n_isi, as.integer(bursts$end_isi[[ii]]))
    if (is.finite(first) && is.finite(last) && last >= first) {
      out[seq.int(first, last)] <- TRUE
    }
  }
  out
}

event_overlap_counts <- function(pred, truth) {
  if (!nrow(truth)) {
    return(list(fragmented_truth_n = 0L, merged_prediction_n = 0L,
                truth_event_with_overlap_n = 0L,
                prediction_event_with_overlap_n = 0L))
  }
  overlap <- matrix(FALSE, nrow = nrow(pred), ncol = nrow(truth))
  if (nrow(pred)) {
    for (ii in seq_len(nrow(pred))) {
      overlap[ii, ] <- pmax(
        0L,
        pmin(pred$end_isi[[ii]], truth$end_isi) -
          pmax(pred$start_isi[[ii]], truth$start_isi) + 1L
      ) > 0L
    }
  }
  list(
    fragmented_truth_n = if (nrow(pred)) sum(colSums(overlap) > 1L) else 0L,
    merged_prediction_n = if (nrow(pred)) sum(rowSums(overlap) > 1L) else 0L,
    truth_event_with_overlap_n = if (nrow(pred)) sum(colSums(overlap) > 0L) else 0L,
    prediction_event_with_overlap_n = if (nrow(pred)) sum(rowSums(overlap) > 0L) else 0L
  )
}

score_record <- function(record, prediction_mask, iou_thresholds = c(.10, .25, .50)) {
  keep <- as.logical(record$score_keep)
  truth <- as.logical(record$truth_burst)
  prediction <- as.logical(prediction_mask)
  stopifnot(length(keep) == length(truth), length(truth) == length(prediction))
  keep[is.na(keep)] <- FALSE
  truth[is.na(truth)] <- FALSE
  prediction[is.na(prediction)] <- FALSE

  support <- data.frame(
    tp = sum(keep & truth & prediction),
    fp = sum(keep & !truth & prediction),
    fn = sum(keep & truth & !prediction),
    tn = sum(keep & !truth & !prediction),
    scored_isi_n = sum(keep), truth_burst_isi_n = sum(keep & truth),
    predicted_burst_isi_n = sum(keep & prediction),
    stringsAsFactors = FALSE
  )

  pred_events <- runs_from_mask(prediction, keep, record$train)
  truth_events <- runs_from_mask(truth, keep, record$train)
  overlap_counts <- event_overlap_counts(pred_events, truth_events)
  event_rows <- lapply(iou_thresholds, function(iou_min) {
    matches <- stpd_match_events_optimal(
      pred_events, truth_events, class_col = "pattern", iou_min = iou_min
    )
    tp <- nrow(matches)
    fp <- nrow(pred_events) - tp
    fn <- nrow(truth_events) - tp
    start_error <- end_error <- numeric()
    if (tp) {
      start_error <- pred_events$start_isi[matches$pred_index] -
        truth_events$start_isi[matches$truth_index]
      end_error <- pred_events$end_isi[matches$pred_index] -
        truth_events$end_isi[matches$truth_index]
    }
    data.frame(
      iou_threshold = iou_min, tp = tp, fp = fp, fn = fn,
      truth_event_n = nrow(truth_events), predicted_event_n = nrow(pred_events),
      matched_iou_sum = if (tp) sum(matches$iou) else 0,
      matched_iou_n = tp,
      median_matched_iou = if (tp) median(matches$iou) else NA_real_,
      onset_error_isi_median = if (length(start_error)) median(start_error) else NA_real_,
      offset_error_isi_median = if (length(end_error)) median(end_error) else NA_real_,
      fragmented_truth_n = overlap_counts$fragmented_truth_n,
      merged_prediction_n = overlap_counts$merged_prediction_n,
      stringsAsFactors = FALSE
    )
  })
  list(support = support, events = do.call(rbind, event_rows))
}

detect_one <- function(record, method, variant) {
  started <- proc.time()[["elapsed"]]
  if (identical(method, "Mean-ISI")) {
    min_spikes <- if (identical(variant, "article_default")) 3L else 4L
    result <- stpd_detect_misi_bursts_article(
      record$timestamp_sec, min_valid_isi_sec = 0.001,
      min_isi_count = 2L, max_isi_count = Inf, max_windows = 2000000L,
      min_spikes = min_spikes, min_duration_sec = 0,
      collapse_exact_duplicates = FALSE
    )
    threshold <- num1(result$threshold$threshold_sec)
    status <- chr1(result$threshold$threshold_status)
  } else {
    min_spikes <- if (identical(variant, "article_default")) 5L else 4L
    result <- stpd_detect_logisi_newBD_pasquale(
      record$timestamp_sec, min_valid_isi_sec = 0.001,
      min_num_spikes = min_spikes, core_reference_sec = 0.100,
      max_reasonable_threshold_sec = 1.0, fallback_ch = TRUE,
      fallback_maxISI_sec = 0.100, multiple_core_mode = "split_by_core",
      void_threshold = 0.7, intraburst_peak_window_ms = 100
    )
    threshold <- num1(result$threshold$threshold_sec)
    status <- chr1(result$threshold$threshold_status)
  }
  elapsed <- proc.time()[["elapsed"]] - started
  list(
    mask = mask_from_bursts(result$bursts, length(record$truth_burst)),
    threshold_sec = threshold, threshold_status = status,
    method_warning = chr1(result$method_warning),
    detected_event_n = if (is.data.frame(result$bursts)) nrow(result$bursts) else 0L,
    elapsed_sec = elapsed, min_spikes = min_spikes
  )
}

read_synthetic_records <- function() {
  root <- first_existing(c(
    file.path(repo, "Simulator data",
              "clean_synthetic_mechanism_benchmark_v2_3_0"),
    file.path(repo, "data", "synthetic", "v2.3.0")
  ))
  spikes <- read.csv(file.path(root, "detector_inputs",
                               "spike_timestamps_blinded.csv"),
                     check.names = FALSE, stringsAsFactors = FALSE)
  truth <- read.csv(file.path(root, "truth", "interval_truth_multitrack.csv"),
                    check.names = FALSE, stringsAsFactors = FALSE)
  truth <- truth[tolower(as.character(truth$Split)) == "holdout", , drop = FALSE]
  samples <- sort(unique(as.character(truth$Sample_ID)), method = "radix")
  spike_split <- split(spikes, as.character(spikes$Sample_ID))
  truth_split <- split(truth, as.character(truth$Sample_ID))

  records <- list()
  for (sample in samples) {
    spike_part <- spike_split[[sample]]
    truth_part <- truth_split[[sample]]
    spike_part <- spike_part[order(spike_part$Spike_Index), , drop = FALSE]
    truth_part <- truth_part[order(truth_part$Interval_Index), , drop = FALSE]
    stopifnot(nrow(spike_part) == nrow(truth_part) + 1L)
    common <- list(
      train = sample,
      cluster = as.character(truth_part$Template_ID[[1L]]),
      dataset = "synthetic_v2.3_holdout",
      region = "synthetic",
      scale = paste0(as.character(truth_part$Scale_Factor[[1L]]), "x"),
      timestamp_sec = as.numeric(spike_part$Time_s)
    )
    strict <- common
    strict$estimand <- "strict_mechanism"
    strict$score_keep <- rep(TRUE, nrow(truth_part))
    strict$truth_burst <- as.character(truth_part$Event_Label) == "Burst"
    records[[length(records) + 1L]] <- strict

    phenotype <- common
    phenotype$estimand <- "observable_phenotype_primary"
    phenotype$score_keep <-
      as.character(truth_part$Phenotype_Label) != "ambiguous_burst_like"
    phenotype$truth_burst <-
      as.character(truth_part$Phenotype_Label) == "clear_burst_like"
    records[[length(records) + 1L]] <- phenotype
  }
  records
}

read_real_records <- function() {
  workbooks <- c(
    GPe = first_existing(c(
      file.path(repo, "PD_GPe",
        "Bagdasaryan_PD_GPe_2020_manual_isi_labels_draft_csv.xlsx"),
      file.path(repo, "data", "real", "GPe",
        "Bagdasaryan_PD_GPe_2020_manual_isi_labels_draft_csv.xlsx")
    )),
    STN = first_existing(c(
      file.path(repo, "PD_STN",
        "PD_STN_Grechishnikova_2017_manual_isi_labels_draft_csv.xlsx"),
      file.path(repo, "data", "real", "STN",
        "PD_STN_Grechishnikova_2017_manual_isi_labels_draft_csv.xlsx")
    )),
    GPi = first_existing(c(
      file.path(repo, "PD_GPi",
        "Kurmanaeva_PD_GPi_2017_manual_isi_labels_draft_csv.xlsx"),
      file.path(repo, "data", "real", "GPi",
        "Kurmanaeva_PD_GPi_2017_manual_isi_labels_draft_csv.xlsx")
    ))
  )
  records <- list()
  eligibility <- list()
  for (region in names(workbooks)) {
    path <- workbooks[[region]]
    for (sheet in readxl::excel_sheets(path)) {
      z <- as.data.frame(readxl::read_excel(path, sheet = sheet),
                         stringsAsFactors = FALSE)
      z <- z[order(as.integer(z$isi_index)), , drop = FALSE]
      reviewed <- tolower(trimws(as.character(z$review_status))) ==
        "manually_labeled"
      event_label <- tolower(trimws(as.character(z$event_pattern)))
      event_nonblank <- reviewed & !is.na(event_label) & nzchar(event_label)
      eligible <- sum(event_nonblank) >= 10L
      train <- as.character(z$train_id[[1L]])
      eligibility[[length(eligibility) + 1L]] <- data.frame(
        dataset = paste0("real_", region), region = region, train = train,
        isi_n = nrow(z), reviewed_isi_n = sum(reviewed),
        reviewed_fraction = mean(reviewed),
        event_reference_isi_n = sum(event_nonblank),
        burst_reference_isi_n = sum(reviewed & event_label == "burst", na.rm = TRUE),
        reference_eligible = eligible, stringsAsFactors = FALSE
      )
      if (!eligible) next
      timestamps <- c(as.numeric(z$left_timestamp_us[[1L]]),
                      as.numeric(z$right_timestamp_us)) / 1e6
      record <- list(
        train = train, cluster = train,
        dataset = paste0("real_", region), region = region,
        scale = "observed", estimand = "reviewed_manual_reference",
        timestamp_sec = timestamps,
        score_keep = reviewed,
        truth_burst = reviewed & !is.na(event_label) & event_label == "burst"
      )
      records[[length(records) + 1L]] <- record
    }
  }
  list(records = records, eligibility = do.call(rbind, eligibility))
}

synthetic_records <- read_synthetic_records()
real_bundle <- read_real_records()

# Detection is identical for the two synthetic estimands. Run once per unique
# train/method/variant and reuse the prediction mask for both truth definitions.
all_records <- c(synthetic_records, real_bundle$records)
detect_key <- vapply(all_records, function(x) paste(x$dataset, x$train, sep = "\r"), character(1))
unique_index <- !duplicated(detect_key)
detection_records <- all_records[unique_index]

tasks <- expand.grid(
  record_index = seq_along(detection_records),
  method = c("Mean-ISI", "LogISI/newBD"),
  variant = c("article_default", "harmonized_4_spikes"),
  stringsAsFactors = FALSE
)

run_task <- function(ii) {
  task <- tasks[ii, , drop = FALSE]
  record <- detection_records[[task$record_index]]
  detected <- detect_one(record, task$method, task$variant)
  list(
    key = paste(record$dataset, record$train, sep = "\r"),
    method = task$method, variant = task$variant,
    detected = detected,
    status = data.frame(
      dataset = record$dataset, region = record$region, scale = record$scale,
      train = record$train, method = task$method, variant = task$variant,
      min_spikes = detected$min_spikes,
      threshold_sec = detected$threshold_sec,
      threshold_status = detected$threshold_status,
      method_warning = detected$method_warning,
      detected_event_n = detected$detected_event_n,
      elapsed_sec = detected$elapsed_sec,
      stringsAsFactors = FALSE
    )
  )
}

message("Running ", nrow(tasks), " method/train configurations with ",
        workers, " worker(s)...")
task_results <- if (.Platform$OS.type != "windows" && workers > 1L) {
  parallel::mclapply(seq_len(nrow(tasks)), run_task, mc.cores = workers,
                     mc.preschedule = FALSE)
} else {
  lapply(seq_len(nrow(tasks)), run_task)
}

prediction_lookup <- new.env(parent = emptyenv())
for (item in task_results) {
  assign(paste(item$key, item$method, item$variant, sep = "\r"),
         item$detected, envir = prediction_lookup)
}
runtime_status <- do.call(rbind, lapply(task_results, `[[`, "status"))

support_rows <- list()
event_rows <- list()
for (record in all_records) {
  for (method in c("Mean-ISI", "LogISI/newBD")) {
    for (variant in c("article_default", "harmonized_4_spikes")) {
      lookup_key <- paste(record$dataset, record$train, method, variant, sep = "\r")
      detected <- get(lookup_key, envir = prediction_lookup, inherits = FALSE)
      scored <- score_record(record, detected$mask)
      base <- data.frame(
        dataset = record$dataset, region = record$region,
        estimand = record$estimand, scale = record$scale,
        cluster = record$cluster, train = record$train,
        method = method, variant = variant, stringsAsFactors = FALSE
      )
      support_rows[[length(support_rows) + 1L]] <- cbind(base, scored$support)
      event_rows[[length(event_rows) + 1L]] <- cbind(base, scored$events)
    }
  }
}
support_by_train <- do.call(rbind, support_rows)
event_by_train <- do.call(rbind, event_rows)

group_columns <- c("dataset", "region", "estimand", "scale", "method", "variant")
group_key <- interaction(support_by_train[group_columns], drop = TRUE, lex.order = TRUE)
support_metrics <- do.call(rbind, lapply(split(support_by_train, group_key), function(z) {
  totals <- colSums(z[c("tp", "fp", "fn", "tn", "scored_isi_n",
                        "truth_burst_isi_n", "predicted_burst_isi_n")])
  precision <- safe_ratio(totals[["tp"]], totals[["tp"]] + totals[["fp"]])
  recall <- safe_ratio(totals[["tp"]], totals[["tp"]] + totals[["fn"]])
  specificity <- safe_ratio(totals[["tn"]], totals[["tn"]] + totals[["fp"]])
  data.frame(
    z[1L, group_columns, drop = FALSE],
    train_n = length(unique(z$train)), cluster_n = length(unique(z$cluster)),
    t(totals), precision = precision, recall = recall,
    specificity = specificity, f1 = f1_score(precision, recall),
    accuracy = safe_ratio(totals[["tp"]] + totals[["tn"]],
                          totals[["tp"]] + totals[["fp"]] + totals[["fn"]] + totals[["tn"]]),
    balanced_accuracy = if (is.finite(recall) && is.finite(specificity))
      mean(c(recall, specificity)) else NA_real_,
    mcc = mcc_score(totals[["tp"]], totals[["fp"]], totals[["fn"]], totals[["tn"]]),
    stringsAsFactors = FALSE
  )
}))

event_group_columns <- c(group_columns, "iou_threshold")
event_key <- interaction(event_by_train[event_group_columns], drop = TRUE, lex.order = TRUE)
event_metrics <- do.call(rbind, lapply(split(event_by_train, event_key), function(z) {
  tp <- sum(z$tp); fp <- sum(z$fp); fn <- sum(z$fn)
  precision <- safe_ratio(tp, tp + fp)
  recall <- safe_ratio(tp, tp + fn)
  data.frame(
    z[1L, event_group_columns, drop = FALSE],
    train_n = length(unique(z$train)), cluster_n = length(unique(z$cluster)),
    tp = tp, fp = fp, fn = fn,
    truth_event_n = sum(z$truth_event_n),
    predicted_event_n = sum(z$predicted_event_n),
    precision = precision, recall = recall, f1 = f1_score(precision, recall),
    mean_matched_iou = safe_ratio(sum(z$matched_iou_sum), sum(z$matched_iou_n)),
    fragmentation_rate = safe_ratio(sum(z$fragmented_truth_n), sum(z$truth_event_n)),
    merge_rate = safe_ratio(sum(z$merged_prediction_n), sum(z$predicted_event_n)),
    stringsAsFactors = FALSE
  )
}))

bootstrap_support_contrast <- function(z, n, seed) {
  clusters <- sort(unique(z$cluster), method = "radix")
  methods <- c("Mean-ISI", "LogISI/newBD")
  set.seed(seed)
  rows <- vector("list", n)
  for (bb in seq_len(n)) {
    sampled <- sample(clusters, length(clusters), replace = TRUE)
    parts <- lapply(seq_along(sampled), function(ii) {
      q <- z[z$cluster == sampled[[ii]], , drop = FALSE]
      q$cluster <- paste0(q$cluster, "__", ii)
      q
    })
    q <- do.call(rbind, parts)
    values <- lapply(methods, function(method) {
      m <- q[q$method == method, , drop = FALSE]
      tp <- sum(m$tp); fp <- sum(m$fp); fn <- sum(m$fn)
      precision <- safe_ratio(tp, tp + fp)
      recall <- safe_ratio(tp, tp + fn)
      c(precision = precision, recall = recall,
        f1 = f1_score(precision, recall))
    })
    rows[[bb]] <- data.frame(
      bootstrap_replicate = bb,
      meanisi_precision = values[[1L]][["precision"]],
      logisi_precision = values[[2L]][["precision"]],
      meanisi_recall = values[[1L]][["recall"]],
      logisi_recall = values[[2L]][["recall"]],
      meanisi_f1 = values[[1L]][["f1"]],
      logisi_f1 = values[[2L]][["f1"]],
      f1_difference_meanisi_minus_logisi =
        values[[1L]][["f1"]] - values[[2L]][["f1"]],
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

contrast_groups <- unique(support_by_train[c(
  "dataset", "region", "estimand", "scale", "variant"
)])
contrast_draws <- list()
contrast_summary <- list()
for (ii in seq_len(nrow(contrast_groups))) {
  group <- contrast_groups[ii, , drop = FALSE]
  keep <- rep(TRUE, nrow(support_by_train))
  for (name in names(group)) keep <- keep & support_by_train[[name]] == group[[name]]
  z <- support_by_train[keep, , drop = FALSE]
  draws <- bootstrap_support_contrast(z, n_bootstrap, 93000L + ii)
  draws <- cbind(group[rep(1L, nrow(draws)), , drop = FALSE], draws)
  contrast_draws[[ii]] <- draws
  difference <- draws$f1_difference_meanisi_minus_logisi
  ci <- quantile(difference[is.finite(difference)], c(.025, .975),
                 names = FALSE, type = 6)
  contrast_summary[[ii]] <- data.frame(
    group,
    f1_difference_meanisi_minus_logisi = mean(difference, na.rm = TRUE),
    ci_low = ci[[1L]], ci_high = ci[[2L]],
    probability_meanisi_f1_greater = mean(difference > 0, na.rm = TRUE),
    bootstrap_n = n_bootstrap, bootstrap_unit = "template_or_train",
    stringsAsFactors = FALSE
  )
}
contrast_draws <- do.call(rbind, contrast_draws)
contrast_summary <- do.call(rbind, contrast_summary)

write.csv(support_by_train, file.path(out_dir, "support_counts_by_train.csv"), row.names = FALSE)
write.csv(event_by_train, file.path(out_dir, "event_counts_by_train.csv"), row.names = FALSE)
write.csv(support_metrics, file.path(out_dir, "support_metrics.csv"), row.names = FALSE)
write.csv(event_metrics, file.path(out_dir, "event_metrics.csv"), row.names = FALSE)
write.csv(contrast_summary, file.path(out_dir, "paired_cluster_bootstrap_f1_contrast.csv"), row.names = FALSE)
saveRDS(contrast_draws, file.path(out_dir, "paired_cluster_bootstrap_draws.rds"))
write.csv(runtime_status, file.path(out_dir, "runtime_and_threshold_status.csv"), row.names = FALSE)
write.csv(real_bundle$eligibility, file.path(out_dir, "real_reference_eligibility.csv"), row.names = FALSE)

protocol <- data.frame(
  item = c(
    "comparison", "synthetic_release", "synthetic_split", "synthetic_cluster_unit",
    "real_regions", "real_score_mask", "real_blank_label_policy",
    "real_train_eligibility", "threshold_learning_from_truth",
    "primary_method_defaults", "sensitivity_variant", "event_matching",
    "event_iou_thresholds", "bootstrap_replicates"
  ),
  value = c(
    "Mean-ISI versus Pasquale LogISI/newBD Burst detection",
    "v2.3.0", "holdout only", "Template_ID",
    "GPe;STN;GPi", "review_status == manually_labeled",
    "reviewed blank event_pattern is negative/other",
    "at least 10 reviewed nonblank event-reference ISIs",
    "none; both methods are label-blind",
    "Mean-ISI >=3 spikes; LogISI/newBD >=5 spikes",
    "both methods >=4 spikes",
    stpd_event_matching_rule(), "0.10;0.25;0.50", as.character(n_bootstrap)
  ), stringsAsFactors = FALSE
)
write.csv(protocol, file.path(out_dir, "protocol.csv"), row.names = FALSE)

headline_support <- support_metrics[
  support_metrics$variant == "article_default", , drop = FALSE
]
headline_events <- event_metrics[
  event_metrics$variant == "article_default" &
    abs(event_metrics$iou_threshold - 0.25) < 1e-12, , drop = FALSE
]

fmt <- function(x) ifelse(is.finite(x), sprintf("%.3f", x), "NA")
summary_lines <- c(
  "# Mean-ISI versus LogISI/newBD Burst comparison",
  "",
  "This is a direct truth-referenced comparison. Neither method is imported into STPD, and no reference label is used to estimate a threshold.",
  "",
  "Primary article-style defaults: Mean-ISI requires at least 3 spikes; LogISI/newBD requires at least 5 spikes. A harmonized >=4-spike sensitivity analysis is also exported.",
  "",
  "## ISI-support headline metrics",
  "",
  "| Dataset | Region | Estimand | Scale | Method | Precision | Recall | F1 | Accuracy | Balanced accuracy |",
  "|---|---|---|---|---|---:|---:|---:|---:|---:|"
)
for (ii in seq_len(nrow(headline_support))) {
  z <- headline_support[ii, ]
  summary_lines <- c(summary_lines, paste0(
    "| ", z$dataset, " | ", z$region, " | ", z$estimand, " | ", z$scale,
    " | ", z$method, " | ", fmt(z$precision), " | ", fmt(z$recall),
    " | ", fmt(z$f1), " | ", fmt(z$accuracy), " | ",
    fmt(z$balanced_accuracy), " |"
  ))
}
summary_lines <- c(summary_lines, "", "## Event-level headline metrics (IoU >= 0.25)", "",
  "| Dataset | Region | Estimand | Scale | Method | Precision | Recall | F1 | Mean matched IoU | Fragmentation | Merge |",
  "|---|---|---|---|---|---:|---:|---:|---:|---:|---:|")
for (ii in seq_len(nrow(headline_events))) {
  z <- headline_events[ii, ]
  summary_lines <- c(summary_lines, paste0(
    "| ", z$dataset, " | ", z$region, " | ", z$estimand, " | ", z$scale,
    " | ", z$method, " | ", fmt(z$precision), " | ", fmt(z$recall),
    " | ", fmt(z$f1), " | ", fmt(z$mean_matched_iou), " | ",
    fmt(z$fragmentation_rate), " | ", fmt(z$merge_rate), " |"
  ))
}
summary_lines <- c(summary_lines, "", "## Interpretation safeguards", "",
  "- Accuracy is secondary because non-Burst ISIs are much more frequent and can inflate it.",
  "- The primary discrimination metrics are precision, recall, F1, balanced accuracy, and event-level IoU performance.",
  "- The three synthetic scales are repeated projections of the same templates; Template_ID, not Sample_ID, is the bootstrap unit.",
  "- Real workbooks remain manual drafts. Real-data estimates are exploratory until annotation sealing and provenance confirmation.",
  "- Results at 1x, 4x, and 10x are kept separate; they are not treated as independent biological replicates."
)
writeLines(summary_lines, file.path(out_dir, "RESULTS.md"), useBytes = TRUE)

manifest_files <- list.files(out_dir, full.names = TRUE)
manifest_files <- manifest_files[
  !basename(manifest_files) %in% c("manifest_checksums.csv")
]
manifest <- data.frame(
  file = basename(manifest_files),
  sha256 = vapply(manifest_files, function(path) digest::digest(
    file = path, algo = "sha256", serialize = FALSE
  ), character(1)),
  stringsAsFactors = FALSE
)
write.csv(manifest, file.path(out_dir, "manifest_checksums.csv"), row.names = FALSE)

message("Completed. Results: ", out_dir)
