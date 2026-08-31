#!/usr/bin/env Rscript

# Current three-method Burst agreement audit.
#
# Mean-ISI and Pasquale LogISI/newBD are run directly on the latest canonical
# GPe/STN/GPi workbook timestamps. Their event/support geometry is then compared
# with the already completed current-code, zero-manual-example STPD automatic
# run. Manual labels select event-reference-eligible trains only; they do not
# enter any of the three detectors or the agreement calculations.

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
validation_root <- first_existing(c(
  file.path(repo, "test-results", "multi_region_three_regime_20260831"),
  file.path(repo, "results", "real_validation", "current_automatic")
))
out_dir <- file.path(repo, "test-results", "method_comparison",
                     "three_method_agreement_current")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

pkgload::load_all(repo, quiet = TRUE)

workbooks <- c(
  GPE = first_existing(c(
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
  GPI = first_existing(c(
    file.path(repo, "PD_GPi",
      "Kurmanaeva_PD_GPi_2017_manual_isi_labels_draft_csv.xlsx"),
    file.path(repo, "data", "real", "GPi",
      "Kurmanaeva_PD_GPi_2017_manual_isi_labels_draft_csv.xlsx")
  ))
)

read_region <- function(region) {
  path <- normalizePath(workbooks[[region]], mustWork = TRUE)
  profile_path <- file.path(validation_root, region, "automatic",
                            "reference_profile.csv")
  stpd_path <- file.path(validation_root, region, "automatic",
                         "predicted_events.csv")
  stopifnot(file.exists(profile_path), file.exists(stpd_path))
  profile <- read.csv(profile_path, check.names = FALSE,
                      stringsAsFactors = FALSE)
  profile <- profile[as.logical(profile$event_reference_eligible), , drop = FALSE]
  event_eligible <- as.character(profile$Train_ID)

  sheets <- readxl::excel_sheets(path)
  trains <- list()
  for (sheet in sheets) {
    z <- as.data.frame(readxl::read_excel(path, sheet = sheet),
                       stringsAsFactors = FALSE)
    z <- z[order(as.integer(z$isi_index), method = "radix"), , drop = FALSE]
    train <- as.character(z$train_id[[1L]])
    if (!train %in% event_eligible) next
    timestamps <- c(as.numeric(z$left_timestamp_us[[1L]]),
                    as.numeric(z$right_timestamp_us)) / 1e6
    trains[[train]] <- list(
      region = region, train = train,
      group = as.character(profile$Group_ID[match(train, profile$Train_ID)]),
      timestamp_sec = timestamps, n_isi = nrow(z)
    )
  }
  stpd <- read.csv(stpd_path, check.names = FALSE, stringsAsFactors = FALSE)
  stpd <- stpd[
    as.character(stpd$Train_ID) %in% names(trains) &
      as.character(stpd$Axis) == "event" &
      as.character(stpd$Pattern) == "burst", , drop = FALSE
  ]
  stpd_events <- data.frame(
    region = region, train = as.character(stpd$Train_ID), method = "STPD",
    start_isi = as.integer(stpd$start_isi), end_isi = as.integer(stpd$end_isi),
    stringsAsFactors = FALSE
  )
  list(trains = trains, stpd_events = stpd_events,
       workbook = path, profile_path = profile_path, stpd_path = stpd_path)
}

regions <- lapply(names(workbooks), read_region)
names(regions) <- names(workbooks)
records <- unlist(lapply(regions, `[[`, "trains"), recursive = FALSE)

as_method_events <- function(result, record, method) {
  bursts <- result$bursts
  if (!is.data.frame(bursts) || !nrow(bursts)) {
    return(data.frame(region = character(), train = character(),
                      method = character(), start_isi = integer(),
                      end_isi = integer()))
  }
  # Support methods use diff-index coordinates; the STPD product uses the
  # inclusive right-spike coordinate, hence the +1 conversion.
  data.frame(
    region = record$region, train = record$train, method = method,
    start_isi = as.integer(bursts$start_isi) + 1L,
    end_isi = as.integer(bursts$end_isi) + 1L,
    stringsAsFactors = FALSE
  )
}

run_record <- function(ii) {
  record <- records[[ii]]
  started_mean <- proc.time()[["elapsed"]]
  mean_result <- stpd_detect_misi_bursts_article(
    record$timestamp_sec, min_valid_isi_sec = 0.001,
    min_isi_count = 2L, max_isi_count = Inf, max_windows = 2000000L,
    min_spikes = 3L, min_duration_sec = 0,
    collapse_exact_duplicates = FALSE
  )
  mean_elapsed <- proc.time()[["elapsed"]] - started_mean

  started_log <- proc.time()[["elapsed"]]
  log_result <- stpd_detect_logisi_newBD_pasquale(
    record$timestamp_sec, min_valid_isi_sec = 0.001, min_num_spikes = 5L,
    core_reference_sec = 0.100, max_reasonable_threshold_sec = 1.0,
    fallback_ch = TRUE, fallback_maxISI_sec = 0.100,
    multiple_core_mode = "split_by_core", void_threshold = 0.7,
    intraburst_peak_window_ms = 100
  )
  log_elapsed <- proc.time()[["elapsed"]] - started_log

  events <- rbind(
    as_method_events(mean_result, record, "Mean-ISI"),
    as_method_events(log_result, record, "LogISI/newBD")
  )
  status <- rbind(
    data.frame(
      region = record$region, train = record$train, group = record$group,
      method = "Mean-ISI", threshold_sec = suppressWarnings(as.numeric(
        mean_result$threshold$threshold_sec[[1L]]
      )), threshold_status = as.character(
        mean_result$threshold$threshold_status[[1L]]
      ), method_warning = as.character(mean_result$method_warning),
      event_n = nrow(mean_result$bursts), elapsed_sec = mean_elapsed,
      stringsAsFactors = FALSE
    ),
    data.frame(
      region = record$region, train = record$train, group = record$group,
      method = "LogISI/newBD", threshold_sec = suppressWarnings(as.numeric(
        log_result$threshold$threshold_sec[[1L]]
      )), threshold_status = as.character(
        log_result$threshold$threshold_status[[1L]]
      ), method_warning = as.character(log_result$method_warning),
      event_n = nrow(log_result$bursts), elapsed_sec = log_elapsed,
      stringsAsFactors = FALSE
    )
  )
  list(events = events, status = status)
}

message("Running Mean-ISI and LogISI/newBD on ", length(records),
        " current real-data trains...")
method_runs <- if (.Platform$OS.type != "windows" && workers > 1L) {
  parallel::mclapply(seq_along(records), run_record, mc.cores = workers,
                     mc.preschedule = FALSE)
} else {
  lapply(seq_along(records), run_record)
}

events <- do.call(rbind, lapply(method_runs, `[[`, "events"))
status <- do.call(rbind, lapply(method_runs, `[[`, "status"))
events <- rbind(events, do.call(rbind, lapply(regions, `[[`, "stpd_events")))
events <- events[order(events$region, events$train, events$method,
                       events$start_isi, method = "radix"), , drop = FALSE]

record_table <- do.call(rbind, lapply(records, function(x) data.frame(
  region = x$region, train = x$train, group = x$group, n_isi = x$n_isi,
  stringsAsFactors = FALSE
)))

index_set <- function(z) {
  if (!nrow(z)) return(integer())
  sort(unique(unlist(Map(seq.int, as.integer(z$start_isi),
                         as.integer(z$end_isi)), use.names = FALSE)))
}

empty_event_table <- function() {
  data.frame(train = character(), pattern = character(),
             start_isi = integer(), end_isi = integer())
}

pair_score <- function(region, train, group, method_a, method_b,
                       iou_min = 0.25) {
  a <- events[events$region == region & events$train == train &
                events$method == method_a, , drop = FALSE]
  b <- events[events$region == region & events$train == train &
                events$method == method_b, , drop = FALSE]
  ae <- if (nrow(a)) data.frame(train = train, pattern = "burst",
    start_isi = a$start_isi, end_isi = a$end_isi) else empty_event_table()
  be <- if (nrow(b)) data.frame(train = train, pattern = "burst",
    start_isi = b$start_isi, end_isi = b$end_isi) else empty_event_table()
  matches <- stpd_match_events_optimal(ae, be, class_col = "pattern",
                                       iou_min = iou_min)
  a_index <- index_set(a)
  b_index <- index_set(b)
  overlap <- length(intersect(a_index, b_index))
  union <- length(union(a_index, b_index))
  matched <- nrow(matches)
  a_recovery <- if (nrow(ae)) matched / nrow(ae) else NA_real_
  b_recovery <- if (nrow(be)) matched / nrow(be) else NA_real_
  data.frame(
    region = region, train = train, group = group,
    method_a = method_a, method_b = method_b, iou_threshold = iou_min,
    a_event_n = nrow(ae), b_event_n = nrow(be), matched_event_n = matched,
    a_event_recovery = a_recovery, b_event_recovery = b_recovery,
    event_f1 = if (is.finite(a_recovery) && is.finite(b_recovery) &&
                   a_recovery + b_recovery > 0)
      2 * a_recovery * b_recovery / (a_recovery + b_recovery) else NA_real_,
    mean_matched_iou = if (matched) mean(matches$iou) else NA_real_,
    a_isi_n = length(a_index), b_isi_n = length(b_index),
    overlapping_isi_n = overlap, union_isi_n = union,
    a_isi_coverage = if (length(a_index)) overlap / length(a_index) else NA_real_,
    b_isi_coverage = if (length(b_index)) overlap / length(b_index) else NA_real_,
    isi_jaccard = if (union) overlap / union else NA_real_,
    isi_dice = if (length(a_index) + length(b_index) > 0)
      2 * overlap / (length(a_index) + length(b_index)) else NA_real_,
    stringsAsFactors = FALSE
  )
}

pairs <- list(
  c("Mean-ISI", "LogISI/newBD"),
  c("Mean-ISI", "STPD"),
  c("LogISI/newBD", "STPD")
)
iou_grid <- c(.10, .25, .50)
pair_rows <- list()
for (ii in seq_len(nrow(record_table))) {
  record <- record_table[ii, ]
  for (pair in pairs) {
    for (iou_min in iou_grid) {
      pair_rows[[length(pair_rows) + 1L]] <- pair_score(
        record$region, record$train, record$group,
        pair[[1L]], pair[[2L]], iou_min
      )
    }
  }
}
pair_by_train <- do.call(rbind, pair_rows)

pool_pair <- function(z) {
  matched <- sum(z$matched_event_n)
  a_n <- sum(z$a_event_n); b_n <- sum(z$b_event_n)
  a_rec <- if (a_n) matched / a_n else NA_real_
  b_rec <- if (b_n) matched / b_n else NA_real_
  overlap <- sum(z$overlapping_isi_n)
  union <- sum(z$union_isi_n)
  data.frame(
    train_n = length(unique(z$train)), group_n = length(unique(z$group)),
    a_event_n = a_n, b_event_n = b_n, matched_event_n = matched,
    a_event_recovery = a_rec, b_event_recovery = b_rec,
    event_f1 = if (is.finite(a_rec) && is.finite(b_rec) && a_rec + b_rec > 0)
      2 * a_rec * b_rec / (a_rec + b_rec) else NA_real_,
    mean_matched_iou = if (matched)
      weighted.mean(z$mean_matched_iou, z$matched_event_n, na.rm = TRUE) else NA_real_,
    a_isi_n = sum(z$a_isi_n), b_isi_n = sum(z$b_isi_n),
    overlapping_isi_n = overlap, union_isi_n = union,
    a_isi_coverage = if (sum(z$a_isi_n)) overlap / sum(z$a_isi_n) else NA_real_,
    b_isi_coverage = if (sum(z$b_isi_n)) overlap / sum(z$b_isi_n) else NA_real_,
    isi_jaccard = if (union) overlap / union else NA_real_,
    isi_dice = if (sum(z$a_isi_n) + sum(z$b_isi_n) > 0)
      2 * overlap / (sum(z$a_isi_n) + sum(z$b_isi_n)) else NA_real_,
    stringsAsFactors = FALSE
  )
}

pool_key <- interaction(pair_by_train[c("region", "method_a", "method_b",
                                        "iou_threshold")],
                        drop = TRUE, lex.order = TRUE)
pair_metrics <- do.call(rbind, lapply(split(pair_by_train, pool_key), function(z) {
  cbind(z[1L, c("region", "method_a", "method_b", "iou_threshold"),
          drop = FALSE], pool_pair(z))
}))

three_way_rows <- lapply(seq_len(nrow(record_table)), function(ii) {
  record <- record_table[ii, ]
  sets <- lapply(c("Mean-ISI", "LogISI/newBD", "STPD"), function(method) {
    index_set(events[events$region == record$region &
                       events$train == record$train &
                       events$method == method, , drop = FALSE])
  })
  common <- Reduce(intersect, sets)
  any_set <- Reduce(union, sets)
  data.frame(
    region = record$region, train = record$train, group = record$group,
    meanisi_isi_n = length(sets[[1L]]), logisi_isi_n = length(sets[[2L]]),
    stpd_isi_n = length(sets[[3L]]), three_way_intersection_isi_n = length(common),
    three_way_union_isi_n = length(any_set),
    three_way_jaccard = if (length(any_set)) length(common) / length(any_set) else NA_real_,
    stringsAsFactors = FALSE
  )
})
three_way_by_train <- do.call(rbind, three_way_rows)
three_way_metrics <- do.call(rbind, lapply(
  split(three_way_by_train, three_way_by_train$region), function(z) {
    common <- sum(z$three_way_intersection_isi_n)
    union <- sum(z$three_way_union_isi_n)
    data.frame(
      region = z$region[[1L]], train_n = nrow(z), group_n = length(unique(z$group)),
      meanisi_isi_n = sum(z$meanisi_isi_n), logisi_isi_n = sum(z$logisi_isi_n),
      stpd_isi_n = sum(z$stpd_isi_n), three_way_intersection_isi_n = common,
      three_way_union_isi_n = union,
      three_way_jaccard = if (union) common / union else NA_real_,
      stringsAsFactors = FALSE
    )
  }
))

bootstrap_pair <- function(z, n, seed) {
  groups <- sort(unique(z$group), method = "radix")
  set.seed(seed)
  rows <- vector("list", n)
  for (bb in seq_len(n)) {
    sampled <- sample(groups, length(groups), replace = TRUE)
    q <- do.call(rbind, lapply(seq_along(sampled), function(jj) {
      part <- z[z$group == sampled[[jj]], , drop = FALSE]
      part$group <- paste0(part$group, "__", jj)
      part
    }))
    pooled <- pool_pair(q)
    rows[[bb]] <- data.frame(
      bootstrap_replicate = bb, event_f1 = pooled$event_f1,
      mean_matched_iou = pooled$mean_matched_iou,
      isi_jaccard = pooled$isi_jaccard, isi_dice = pooled$isi_dice,
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

headline <- pair_by_train[abs(pair_by_train$iou_threshold - .25) < 1e-12, ]
bootstrap_groups <- unique(headline[c("region", "method_a", "method_b")])
bootstrap_summary <- list()
bootstrap_draws <- list()
for (ii in seq_len(nrow(bootstrap_groups))) {
  group <- bootstrap_groups[ii, ]
  z <- headline[headline$region == group$region &
                  headline$method_a == group$method_a &
                  headline$method_b == group$method_b, , drop = FALSE]
  draws <- bootstrap_pair(z, n_bootstrap, 84000L + ii)
  draws <- cbind(group[rep(1L, nrow(draws)), , drop = FALSE], draws)
  bootstrap_draws[[ii]] <- draws
  for (metric in c("event_f1", "mean_matched_iou", "isi_jaccard", "isi_dice")) {
    values <- draws[[metric]]
    values <- values[is.finite(values)]
    ci <- if (length(values)) quantile(values, c(.025, .975), names = FALSE,
                                      type = 6) else c(NA_real_, NA_real_)
    bootstrap_summary[[length(bootstrap_summary) + 1L]] <- data.frame(
      group, metric = metric,
      bootstrap_mean = if (length(values)) mean(values) else NA_real_,
      ci_low = ci[[1L]], ci_high = ci[[2L]], bootstrap_n = length(values),
      bootstrap_unit = "recording_group", stringsAsFactors = FALSE
    )
  }
}
bootstrap_draws <- do.call(rbind, bootstrap_draws)
bootstrap_summary <- do.call(rbind, bootstrap_summary)

write.csv(events, file.path(out_dir, "three_method_burst_events.csv"), row.names = FALSE)
write.csv(status, file.path(out_dir, "support_method_runtime_and_status.csv"), row.names = FALSE)
write.csv(record_table, file.path(out_dir, "comparison_scope.csv"), row.names = FALSE)
write.csv(pair_by_train, file.path(out_dir, "pairwise_agreement_by_train.csv"), row.names = FALSE)
write.csv(pair_metrics, file.path(out_dir, "pairwise_agreement_metrics.csv"), row.names = FALSE)
write.csv(three_way_by_train, file.path(out_dir, "three_way_support_by_train.csv"), row.names = FALSE)
write.csv(three_way_metrics, file.path(out_dir, "three_way_support_metrics.csv"), row.names = FALSE)
write.csv(bootstrap_summary, file.path(out_dir, "recording_group_bootstrap_95ci.csv"), row.names = FALSE)
saveRDS(bootstrap_draws, file.path(out_dir, "recording_group_bootstrap_draws.rds"))

protocol <- data.frame(
  item = c(
    "purpose", "regions", "train_scope", "manual_labels_used_by_detectors",
    "stpd_regime", "meanisi_min_spikes", "logisi_min_spikes",
    "event_matching", "iou_thresholds", "bootstrap_unit",
    "stpd_source_snapshot"
  ),
  value = c(
    "descriptive three-method Burst agreement; not truth accuracy",
    "GPe;STN;GPi", "event-reference-eligible trains",
    "no", "automatic; zero manual examples", "3", "5",
    stpd_event_matching_rule(), "0.10;0.25;0.50", "recording Group_ID",
    "test-results/multi_region_three_regime_20260831/*/automatic"
  ), stringsAsFactors = FALSE
)
write.csv(protocol, file.path(out_dir, "protocol.csv"), row.names = FALSE)

headline_metrics <- pair_metrics[
  abs(pair_metrics$iou_threshold - .25) < 1e-12, , drop = FALSE
]
fmt <- function(x) ifelse(is.finite(x), sprintf("%.3f", x), "NA")
lines <- c(
  "# Current Mean-ISI / LogISI-newBD / STPD Burst agreement",
  "",
  "This analysis measures agreement among method outputs. It is distinct from truth-referenced accuracy.",
  "",
  "| Region | Pair | Event F1 (IoU >= 0.25) | Mean matched IoU | ISI Jaccard | ISI Dice |",
  "|---|---|---:|---:|---:|---:|"
)
for (ii in seq_len(nrow(headline_metrics))) {
  z <- headline_metrics[ii, ]
  lines <- c(lines, paste0(
    "| ", z$region, " | ", z$method_a, " vs ", z$method_b,
    " | ", fmt(z$event_f1), " | ", fmt(z$mean_matched_iou),
    " | ", fmt(z$isi_jaccard), " | ", fmt(z$isi_dice), " |"
  ))
}
lines <- c(lines, "", "## Safeguards", "",
  "- Agreement is not sensitivity, specificity, or accuracy.",
  "- Manual labels determine the eligible train scope only; no label enters detection.",
  "- STPD is the current automatic zero-manual-example run; support methods use their article-style defaults.",
  "- GPe, STN, and GPi are reported separately and are not pooled as independent patients."
)
writeLines(lines, file.path(out_dir, "RESULTS.md"), useBytes = TRUE)

input_files <- c(unname(workbooks),
  file.path(validation_root, names(workbooks), "automatic", "predicted_events.csv"))
input_manifest <- data.frame(
  file = substring(
    normalizePath(input_files, mustWork = TRUE), nchar(repo) + 2L
  ),
  sha256 = vapply(input_files, function(path) digest::digest(
    file = path, algo = "sha256", serialize = FALSE
  ), character(1)), stringsAsFactors = FALSE
)
write.csv(input_manifest, file.path(out_dir, "input_manifest_sha256.csv"), row.names = FALSE)

message("Completed. Results: ", out_dir)
