#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
dataset_path <- if (length(args) >= 1 && nzchar(args[[1]])) {
  args[[1]]
} else {
  "/Users/zark/Desktop/SPIKE_TRAIN_V3_spike_matrix.csv"
}
out_dir <- if (length(args) >= 2 && nzchar(args[[2]])) {
  args[[2]]
} else {
  file.path(getwd(), "reproducibility", "spike_train_v3_detection")
}

if (!file.exists(dataset_path)) {
  stop("Dataset not found: ", dataset_path, call. = FALSE)
}
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

if (requireNamespace("pkgload", quietly = TRUE) && file.exists(file.path(getwd(), "DESCRIPTION"))) {
  pkgload::load_all(".", quiet = TRUE)
} else {
  suppressPackageStartupMessages(library(SpikeTrainPatternDetector))
}

write_table <- function(x, path) {
  utils::write.csv(x, path, row.names = FALSE, fileEncoding = "UTF-8")
}

label_counts <- function(x) {
  x <- as.character(x)
  x[is.na(x) | !nzchar(x)] <- "blank"
  as.data.frame(table(pattern = x), stringsAsFactors = FALSE)
}

dataset <- build_spike_dataset(dataset_path, mode = "raw", unit_in = "s")
params <- default_params()
params$detector$patterns_to_run <- stpd_resolve_patterns_to_run(
  c("burst", "long_burst", "tonic", "high_frequency_tonic", "high_frequency_spiking", "pause"),
  strict_subset = FALSE
)

detected <- stpd_detect(
  dataset,
  params,
  selected_trains = names(dataset$trains),
  lock_manual = TRUE,
  collect_diagnostics = TRUE
)

per_train <- do.call(rbind, lapply(names(detected$trains), function(tr) {
  cc <- label_counts(detected$trains[[tr]]$pattern_auto)
  data.frame(train = tr, cc, stringsAsFactors = FALSE)
}))
names(per_train)[names(per_train) == "Freq"] <- "n_isi"

events <- detected$results$events %||% data.frame()
event_counts <- if (nrow(events) > 0 && "pattern" %in% names(events)) {
  as.data.frame(table(pattern = as.character(events$pattern)), stringsAsFactors = FALSE)
} else {
  data.frame(pattern = character(), Freq = integer(), stringsAsFactors = FALSE)
}
names(event_counts)[names(event_counts) == "Freq"] <- "n_events"

audit <- detected$results$candidate_diagnostic_audit %||% data.frame()
selected_counts <- if (nrow(audit) > 0 && all(c("final_label", "selected_for_auto") %in% names(audit))) {
  sel <- audit[as.logical(audit$selected_for_auto %||% FALSE), , drop = FALSE]
  as.data.frame(table(pattern = as.character(sel$final_label)), stringsAsFactors = FALSE)
} else {
  data.frame(pattern = character(), Freq = integer(), stringsAsFactors = FALSE)
}
names(selected_counts)[names(selected_counts) == "Freq"] <- "n_selected_candidates"

tonic_pause_cols <- intersect(
  c("train", "pattern", "start_isi", "end_isi", "n_isi", "n_spikes",
    "start_sec", "end_sec", "duration"),
  names(events)
)
tonic_pause_events <- if (nrow(events) > 0 && "pattern" %in% names(events)) {
  events[events$pattern %in% c("tonic", "pause"), tonic_pause_cols, drop = FALSE]
} else {
  data.frame()
}

write_table(per_train, file.path(out_dir, "auto_label_counts_by_train.csv"))
write_table(event_counts, file.path(out_dir, "event_counts.csv"))
write_table(selected_counts, file.path(out_dir, "selected_candidate_counts.csv"))
write_table(tonic_pause_events, file.path(out_dir, "tonic_pause_events.csv"))

expected_train_counts <- data.frame(
  train = c("Train_1_Time_s", "Train_1_Time_s", "Train_1_Time_s",
            "Train_2_Time_s", "Train_2_Time_s", "Train_2_Time_s"),
  pattern = c("burst", "tonic", "pause", "burst", "tonic", "pause"),
  expected_n_isi = c(38L, 30L, 5L, 38L, 40L, 1L),
  stringsAsFactors = FALSE
)

observed <- merge(expected_train_counts, per_train, by = c("train", "pattern"), all.x = TRUE)
observed$n_isi[is.na(observed$n_isi)] <- 0L
observed$pass <- observed$n_isi == observed$expected_n_isi
write_table(observed, file.path(out_dir, "expected_count_checks.csv"))

expected_events <- data.frame(
  pattern = c("burst", "tonic", "pause", "possible_burst"),
  expected_n_events = c(33L, 17L, 23L, 2L),
  stringsAsFactors = FALSE
)
observed_events <- merge(expected_events, event_counts, by = "pattern", all.x = TRUE)
observed_events$n_events[is.na(observed_events$n_events)] <- 0L
observed_events$pass <- observed_events$n_events == observed_events$expected_n_events
write_table(observed_events, file.path(out_dir, "expected_event_count_checks.csv"))

summary_lines <- c(
  paste0("Dataset: ", dataset_path),
  paste0("Output directory: ", out_dir),
  paste0("Patterns to run: ", paste(params$detector$patterns_to_run, collapse = ", ")),
  "",
  "Expected Train 1 / Train 2 ISI-label checks:",
  capture.output(print(observed, row.names = FALSE)),
  "",
  "Expected event-table checks:",
  capture.output(print(observed_events, row.names = FALSE)),
  "",
  "Session info:",
  capture.output(utils::sessionInfo())
)
writeLines(summary_lines, file.path(out_dir, "run_summary.txt"), useBytes = TRUE)

if (!all(observed$pass) || !all(observed_events$pass)) {
  stop("Reproducibility checks failed. See: ", out_dir, call. = FALSE)
}

cat("Reproducibility checks passed.\n")
cat("Output directory:", out_dir, "\n")
