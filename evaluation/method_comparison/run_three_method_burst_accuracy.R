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
f1 <- function(p, r) {
  if (!is.finite(p) || !is.finite(r)) return(NA_real_)
  if (p + r > 0) 2*p*r/(p+r) else 0
}

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

region_dirs <- c(GPe = "GPE", STN = "STN", GPi = "GPI")
read_real_reference_scope <- function() {
  rows <- lapply(names(region_dirs), function(region) {
    path <- file.path(validation_root, region_dirs[[region]], "automatic",
                      "reference_profile.csv")
    z <- read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
    required <- c("Train_ID", "Group_ID", "event_reference_eligible")
    if (!all(required %in% names(z))) {
      stop("Real reference profile schema is incomplete: ", path, call. = FALSE)
    }
    z$Train_ID <- as.character(z$Train_ID)
    z$Group_ID <- as.character(z$Group_ID)
    if (any(!nzchar(z$Train_ID)) || any(!nzchar(z$Group_ID)) ||
        anyDuplicated(z$Train_ID)) {
      stop("Real reference profile has invalid Train_ID/Group_ID values: ",
           path, call. = FALSE)
    }
    z <- z[as.logical(z$event_reference_eligible), , drop = FALSE]
    data.frame(
      dataset = paste0("real_", region), region = region,
      train = z$Train_ID, cluster = z$Group_ID,
      eligibility_source = paste0(region_dirs[[region]],
                                  "/automatic/reference_profile.csv"),
      eligibility_rule = "event_reference_eligible == TRUE",
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

real_reference_scope <- read_real_reference_scope()

validate_real_input_scope <- function(x, label) {
  for (dataset in unique(real_reference_scope$dataset)) {
    expected <- sort(unique(real_reference_scope$train[
      real_reference_scope$dataset == dataset]), method = "radix")
    methods <- unique(x$method[x$dataset == dataset])
    if (!length(methods)) {
      stop(label, " has no rows for ", dataset, ".", call. = FALSE)
    }
    for (method in methods) {
      observed <- sort(unique(x$train[
        x$dataset == dataset & x$method == method]), method = "radix")
      if (!identical(observed, expected)) {
        stop(label, " eligibility differs from the frozen reference profile for ",
             dataset, " / ", method, ".", call. = FALSE)
      }
    }
  }
  invisible(TRUE)
}

assign_real_group_id <- function(x) {
  real <- x$region != "synthetic"
  key <- paste(real_reference_scope$dataset, real_reference_scope$train, sep = "\r")
  index <- match(paste(x$dataset[real], x$train[real], sep = "\r"), key)
  if (anyNA(index)) {
    stop("A real-data comparison row lacks an authoritative Group_ID.",
         call. = FALSE)
  }
  x$cluster[real] <- real_reference_scope$cluster[index]
  x
}

support_old <- read.csv(file.path(two_dir, "support_counts_by_train.csv"),
                        check.names = FALSE)
event_old <- read.csv(file.path(two_dir, "event_counts_by_train.csv"),
                      check.names = FALSE)
support_old <- support_old[support_old$variant == "article_default", , drop = FALSE]
event_old <- event_old[event_old$variant == "article_default", , drop = FALSE]
support_old$configuration <- "article_default"
event_old$configuration <- "article_default"
validate_real_input_scope(support_old, "Support-count input")
validate_real_input_scope(event_old, "Event-count input")
support_old <- assign_real_group_id(support_old)
event_old <- assign_real_group_id(event_old)

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
  records <- list()
  for (region in names(workbooks)) {
    for (sheet in readxl::excel_sheets(workbooks[[region]])) {
      z <- as.data.frame(readxl::read_excel(workbooks[[region]], sheet = sheet))
      z <- z[order(as.integer(z$isi_index)), , drop = FALSE]
      train <- as.character(z$train_id[[1L]])
      dataset <- paste0("real_", region)
      scope <- real_reference_scope[
        real_reference_scope$dataset == dataset &
          real_reference_scope$train == train, , drop = FALSE]
      if (!nrow(scope)) next
      if (nrow(scope) != 1L) {
        stop("Non-unique real Train_ID to Group_ID mapping.", call. = FALSE)
      }
      reviewed <- tolower(trimws(as.character(z$review_status))) == "manually_labeled"
      label <- tolower(trimws(as.character(z$event_pattern)))
      records[[length(records) + 1L]] <- list(
        train = train, cluster = scope$cluster[[1L]], dataset = dataset, region = region,
        scale = "observed", estimand = "reviewed_manual_reference",
        score_keep = reviewed, truth_burst = reviewed & !is.na(label) & label == "burst",
        n_isi = nrow(z)
      )
    }
  }
  observed <- sort(vapply(records, `[[`, character(1), "train"), method = "radix")
  expected <- sort(real_reference_scope$train, method = "radix")
  if (!identical(observed, expected)) {
    stop("Workbook scope differs from the frozen event-reference-eligible scope.",
         call. = FALSE)
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

bootstrap_keys <- c("dataset", "region", "estimand", "scale")
bootstrap_groups <- unique(support[bootstrap_keys])
filter_group <- function(x, group) {
  keep <- rep(TRUE, nrow(x))
  for (name in names(group)) keep <- keep & x[[name]] == group[[name]]
  x[keep, , drop = FALSE]
}
resample_cluster_rows <- function(x, sampled) {
  do.call(rbind, lapply(seq_along(sampled), function(ii) {
    part <- x[x$cluster == sampled[[ii]], , drop = FALSE]
    part$cluster <- paste0(part$cluster, "__bootstrap_", ii)
    part
  }))
}

support_draw_rows <- list()
event_draw_rows <- list()
method_order <- c("Mean-ISI", "LogISI/newBD", "STPD")
for (gg in seq_len(nrow(bootstrap_groups))) {
  group <- bootstrap_groups[gg, , drop = FALSE]
  z_support <- filter_group(support, group)
  z_event <- filter_group(event, group)
  clusters <- sort(unique(z_support$cluster), method = "radix")
  if (!setequal(clusters, unique(z_event$cluster))) {
    stop("Support and event rows use different bootstrap clusters.", call. = FALSE)
  }
  set.seed(271000L + gg)
  for (bb in seq_len(n_bootstrap)) {
    sampled <- sample(clusters, length(clusters), replace = TRUE)
    q_support <- resample_cluster_rows(z_support, sampled)
    q_event <- resample_cluster_rows(z_event, sampled)
    for (method in method_order) {
      m <- q_support[q_support$method == method, , drop = FALSE]
      tp <- sum(m$tp); fp <- sum(m$fp); fn <- sum(m$fn)
      p <- ratio(tp, tp + fp); r <- ratio(tp, tp + fn)
      support_draw_rows[[length(support_draw_rows) + 1L]] <- data.frame(
        group, bootstrap_replicate = bb, method = method,
        precision = p, recall = r, f1 = f1(p, r), stringsAsFactors = FALSE)
      for (threshold in sort(unique(q_event$iou_threshold))) {
        e <- q_event[q_event$method == method &
                       abs(q_event$iou_threshold - threshold) < 1e-12,
                     , drop = FALSE]
        tp <- sum(e$tp); fp <- sum(e$fp); fn <- sum(e$fn)
        p <- ratio(tp, tp + fp); r <- ratio(tp, tp + fn)
        event_draw_rows[[length(event_draw_rows) + 1L]] <- data.frame(
          group, bootstrap_replicate = bb, method = method,
          iou_threshold = threshold, precision = p, recall = r,
          f1 = f1(p, r), stringsAsFactors = FALSE)
      }
    }
  }
}
draws <- do.call(rbind, support_draw_rows)
event_draws <- do.call(rbind, event_draw_rows)

ci <- do.call(rbind, lapply(split(
  draws, interaction(draws[c(bootstrap_keys, "method")],
                     drop = TRUE, lex.order = TRUE)
), function(z) do.call(rbind, lapply(c("precision", "recall", "f1"), function(metric) {
  values <- z[[metric]][is.finite(z[[metric]])]
  observed <- filter_group(support_metrics, z[1L, bootstrap_keys, drop = FALSE])
  observed <- observed[observed$method == z$method[[1L]], , drop = FALSE]
  if (nrow(observed) != 1L) stop("Non-unique support point estimate.", call. = FALSE)
  bounds <- quantile(values, c(.025, .975), names = FALSE, type = 6)
  data.frame(z[1L, c(bootstrap_keys, "method")], metric = metric,
    observed = observed[[metric]][[1L]], ci_low = bounds[[1L]],
    ci_high = bounds[[2L]], bootstrap_n = length(values),
    bootstrap_unit = if (z$region[[1L]] == "synthetic") "Template_ID" else "Group_ID",
    stringsAsFactors = FALSE)
}))))

event_ci <- do.call(rbind, lapply(split(
  event_draws, interaction(
    event_draws[c(bootstrap_keys, "method", "iou_threshold")],
    drop = TRUE, lex.order = TRUE)
), function(z) do.call(rbind, lapply(c("precision", "recall", "f1"), function(metric) {
  observed <- filter_group(event_metrics, z[1L, bootstrap_keys, drop = FALSE])
  observed <- observed[
    observed$method == z$method[[1L]] &
      abs(observed$iou_threshold - z$iou_threshold[[1L]]) < 1e-12,
    , drop = FALSE]
  if (nrow(observed) != 1L) stop("Non-unique event point estimate.", call. = FALSE)
  values <- z[[metric]][is.finite(z[[metric]])]
  bounds <- quantile(values, c(.025, .975), names = FALSE, type = 6)
  data.frame(z[1L, c(bootstrap_keys, "method", "iou_threshold")],
    metric = metric, observed = observed[[metric]][[1L]],
    ci_low = bounds[[1L]], ci_high = bounds[[2L]],
    bootstrap_n = length(values),
    bootstrap_unit = if (z$region[[1L]] == "synthetic") "Template_ID" else "Group_ID",
    stringsAsFactors = FALSE)
}))))

scope_groups <- unique(support[bootstrap_keys])
analysis_scope <- do.call(rbind, lapply(seq_len(nrow(scope_groups)), function(ii) {
  group <- scope_groups[ii, , drop = FALSE]
  z <- filter_group(support, group)
  synthetic <- group$region[[1L]] == "synthetic"
  data.frame(
    group,
    eligibility_rule = if (synthetic) {
      "v2.3.0 Split == holdout"
    } else {
      "event_reference_eligible == TRUE in automatic/reference_profile.csv"
    },
    score_mask = if (synthetic && group$estimand[[1L]] == "strict_mechanism") {
      "all holdout ISIs"
    } else if (synthetic) {
      "Phenotype_Label != ambiguous_burst_like"
    } else {
      "review_status == manually_labeled"
    },
    cluster_unit = if (synthetic) "Template_ID" else "Group_ID",
    train_n = length(unique(z$train)), cluster_n = length(unique(z$cluster)),
    stringsAsFactors = FALSE
  )
}))

write.csv(support, file.path(out_dir, "support_counts_by_train.csv"), row.names = FALSE)
write.csv(event, file.path(out_dir, "event_counts_by_train.csv"), row.names = FALSE)
write.csv(support_metrics, file.path(out_dir, "support_metrics.csv"), row.names = FALSE)
write.csv(event_metrics, file.path(out_dir, "event_metrics.csv"), row.names = FALSE)
write.csv(ci, file.path(out_dir, "cluster_bootstrap_95ci.csv"), row.names = FALSE)
write.csv(event_ci, file.path(out_dir, "event_cluster_bootstrap_95ci.csv"),
          row.names = FALSE)
write.csv(analysis_scope, file.path(out_dir, "analysis_scope.csv"), row.names = FALSE)
saveRDS(draws, file.path(out_dir, "cluster_bootstrap_draws.rds"))
saveRDS(event_draws, file.path(out_dir, "event_cluster_bootstrap_draws.rds"))

protocol <- data.frame(item = c(
  "purpose", "methods", "truth_used_for_thresholds", "real_score_mask",
  "real_eligibility", "real_cluster_source", "synthetic_release_split",
  "event_matching", "event_iou_thresholds", "bootstrap_units",
  "bootstrap_resampling", "bootstrap_intervals", "coordinate_conversion"
), value = c(
  "unified truth-referenced three-method Burst accuracy",
  "Mean-ISI article default; LogISI/newBD article default; STPD current automatic zero-example",
  "none", "review_status == manually_labeled; same eligible trains for all methods",
  "event_reference_eligible == TRUE in each region automatic/reference_profile.csv; exact train set required for every method",
  "Group_ID read from each region automatic/reference_profile.csv",
  "v2.3.0 holdout", stpd_event_matching_rule(), "0.10;0.25;0.50",
  "real=Group_ID; synthetic=Template_ID",
  "sample clusters with replacement within dataset x region x estimand x scale; keep all member trains; recompute pooled counts per replicate",
  sprintf("percentile 95%% intervals (quantile type 6), %s replicates, for ISI-support and event precision/recall/F1",
          format(n_bootstrap, big.mark = ",", scientific = FALSE)),
  "STPD right-spike product coordinates converted to one-based ISI truth rows by subtracting one"
), stringsAsFactors = FALSE)
write.csv(protocol, file.path(out_dir, "protocol.csv"), row.names = FALSE)

headline_s <- support_metrics[
  support_metrics$estimand %in% c("reviewed_manual_reference", "strict_mechanism"), ]
headline_e <- event_metrics[
  event_metrics$estimand %in% c("reviewed_manual_reference", "strict_mechanism") &
    abs(event_metrics$iou_threshold - .25) < 1e-12, ]
fmt <- function(x) ifelse(is.finite(x), sprintf("%.3f", x), "NA")
f1_with_ci <- function(z, ci_table, event_threshold = NULL) {
  keep <- ci_table$dataset == z$dataset & ci_table$region == z$region &
    ci_table$estimand == z$estimand & ci_table$scale == z$scale &
    ci_table$method == z$method & ci_table$metric == "f1"
  if (!is.null(event_threshold)) {
    keep <- keep & abs(ci_table$iou_threshold - event_threshold) < 1e-12
  }
  q <- ci_table[keep, , drop = FALSE]
  if (nrow(q) != 1L) stop("Missing F1 interval for RESULTS.md.", call. = FALSE)
  sprintf("%s [%s, %s]", fmt(z$f1), fmt(q$ci_low), fmt(q$ci_high))
}
lines <- c(
  "# Unified three-method Burst accuracy", "",
  "All three automatic methods are rescored on identical truth records and scoring rules. No truth label is used to estimate a detector threshold.", "",
  "## ISI-support metrics", "",
  "| Dataset | Region | Scale | Method | Precision | Recall | F1 [95% cluster-bootstrap CI] |",
  "|---|---|---|---|---:|---:|---:|"
)
for (ii in seq_len(nrow(headline_s))) {
  z <- headline_s[ii, ]
  lines <- c(lines, sprintf("| %s | %s | %s | %s | %s | %s | %s |",
    z$dataset, z$region, z$scale, z$method, fmt(z$precision), fmt(z$recall),
    f1_with_ci(z, ci)))
}
lines <- c(lines, "", "## Event metrics (IoU >= 0.25)", "",
  "| Dataset | Region | Scale | Method | Precision | Recall | F1 [95% cluster-bootstrap CI] | Mean matched IoU |",
  "|---|---|---|---|---:|---:|---:|---:|")
for (ii in seq_len(nrow(headline_e))) {
  z <- headline_e[ii, ]
  lines <- c(lines, sprintf("| %s | %s | %s | %s | %s | %s | %s | %s |",
    z$dataset, z$region, z$scale, z$method, fmt(z$precision), fmt(z$recall),
    f1_with_ci(z, event_ci, z$iou_threshold), fmt(z$mean_matched_iou)))
}
lines <- c(lines, "",
  "All precision, recall, and F1 intervals are available in `cluster_bootstrap_95ci.csv` and `event_cluster_bootstrap_95ci.csv`.",
  "", "## Safeguards", "",
  "- Real estimates are exploratory comparisons on the frozen reviewed reference, not clinical performance estimates.",
  "- GPe, STN, and GPi are not pooled as independent patients.",
  "- Real resampling uses recording Group_ID; all member trains remain together.",
  "- Synthetic scales are reported separately and share Template_ID bootstrap clusters.")
writeLines(lines, file.path(out_dir, "RESULTS.md"), useBytes = TRUE)

ci_observed_matches <- function(ci_table, metric_table, event_level = FALSE) {
  all(vapply(seq_len(nrow(ci_table)), function(ii) {
    z <- ci_table[ii, ]
    keep <- metric_table$dataset == z$dataset & metric_table$region == z$region &
      metric_table$estimand == z$estimand & metric_table$scale == z$scale &
      metric_table$method == z$method
    if (event_level) {
      keep <- keep & abs(metric_table$iou_threshold - z$iou_threshold) < 1e-12
    }
    q <- metric_table[keep, , drop = FALSE]
    nrow(q) == 1L && isTRUE(all.equal(z$observed, q[[z$metric]][[1L]],
                                     tolerance = 1e-14))
  }, logical(1)))
}
real_scope_exact <- function(x) {
  all(vapply(split(x[x$region != "synthetic", ],
                   interaction(x[x$region != "synthetic", c("dataset", "method")],
                               drop = TRUE, lex.order = TRUE)), function(z) {
    expected <- sort(real_reference_scope$train[
      real_reference_scope$dataset == z$dataset[[1L]]], method = "radix")
    identical(sort(unique(z$train), method = "radix"), expected)
  }, logical(1)))
}
unit_is_correct <- function(x) all(
  x$bootstrap_unit == ifelse(x$region == "synthetic", "Template_ID", "Group_ID")
)
checks <- data.frame(
  check = c("three_methods", "same_truth_denominators", "support_identities",
            "event_identities", "metric_bounds", "real_eligibility_exact",
            "cluster_assignment_consistent", "support_bootstrap_complete",
            "event_bootstrap_complete", "bootstrap_units",
            "bootstrap_observed_values"),
  pass = c(
    setequal(unique(support$method), c("Mean-ISI", "LogISI/newBD", "STPD")),
    all(vapply(split(support, interaction(support[c("dataset", "estimand", "scale", "train")], drop=TRUE)),
      function(z) length(unique(z$truth_burst_isi_n)) == 1L && length(unique(z$scored_isi_n)) == 1L, logical(1))),
    all(support$tp + support$fp == support$predicted_burst_isi_n) &&
      all(support$tp + support$fn == support$truth_burst_isi_n),
    all(event$tp + event$fp == event$predicted_event_n) &&
      all(event$tp + event$fn == event$truth_event_n),
    all(unlist(support_metrics[c("precision", "recall", "f1")]) >= 0 &
          unlist(support_metrics[c("precision", "recall", "f1")]) <= 1, na.rm = TRUE) &&
      all(unlist(event_metrics[c("precision", "recall", "f1")]) >= 0 &
            unlist(event_metrics[c("precision", "recall", "f1")]) <= 1, na.rm = TRUE),
    real_scope_exact(support) && real_scope_exact(event),
    all(vapply(split(rbind(
      unique(support[c("dataset", "train", "cluster")]),
      unique(event[c("dataset", "train", "cluster")])
    ), interaction(rbind(
      unique(support[c("dataset", "train", "cluster")]),
      unique(event[c("dataset", "train", "cluster")])
    )[c("dataset", "train")], drop = TRUE)),
    function(z) length(unique(z$cluster)) == 1L, logical(1))),
    nrow(ci) == nrow(support_metrics) * 3L &&
      all(ci$bootstrap_n == n_bootstrap) && all(ci$ci_low <= ci$ci_high),
    nrow(event_ci) == nrow(event_metrics) * 3L &&
      all(event_ci$bootstrap_n == n_bootstrap) &&
      all(event_ci$ci_low <= event_ci$ci_high) &&
      setequal(unique(event_ci$iou_threshold), c(.10, .25, .50)),
    unit_is_correct(ci) && unit_is_correct(event_ci),
    ci_observed_matches(ci, support_metrics) &&
      ci_observed_matches(event_ci, event_metrics, event_level = TRUE)
  ), stringsAsFactors = FALSE
)
write.csv(checks, file.path(out_dir, "validation_checks.csv"), row.names = FALSE)
if (any(!checks$pass)) stop("Unified comparison validation failed: ",
  paste(checks$check[!checks$pass], collapse = "; "), call. = FALSE)

manifest_files <- list.files(out_dir, full.names = TRUE)
manifest_files <- manifest_files[basename(manifest_files) != "manifest_checksums.csv"]
manifest <- data.frame(file = basename(manifest_files), sha256 = vapply(
  manifest_files, digest::digest, character(1), algo = "sha256", serialize = FALSE,
  file = TRUE
))
write.csv(manifest, file.path(out_dir, "manifest_checksums.csv"), row.names = FALSE)
message("Unified three-method accuracy complete: ", out_dir)
