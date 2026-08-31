#!/usr/bin/env Rscript

# Strict two-phase validation of the dataset-adaptive, label-blind Pause path.
# Phase A writes and hashes predictions before Phase B opens the reference file.

options(stringsAsFactors = FALSE)
suppressPackageStartupMessages({
  library(pkgload)
  library(readxl)
  library(digest)
})

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(script_arg)) sub("^--file=", "", script_arg[[1]]) else NA_character_
repo_default <- if (is.character(script_path) && !is.na(script_path)) {
  file.path(dirname(normalizePath(script_path, mustWork = TRUE)), "..", "..")
} else {
  getwd()
}
repo <- normalizePath(Sys.getenv("STPD_REPO", unset = repo_default), mustWork = TRUE)
raw_path <- normalizePath(Sys.getenv(
  "STPD_REAL_RAW_CSV",
  unset = file.path(repo, "data", "real", "STN", "Grechishnikova_STN_2017.csv")
), mustWork = TRUE)
truth_path <- normalizePath(Sys.getenv(
  "STPD_REAL_TRUTH_XLSX",
  unset = file.path(
    repo, "data", "real", "STN",
    "PD_STN_Grechishnikova_2017_manual_isi_labels_draft_csv.xlsx"
  )
), mustWork = TRUE)
out_dir <- Sys.getenv(
  "STPD_REAL_ZERO_SHOT_OUT",
  unset = file.path(
    repo, "results", "reruns",
    "real_zero_shot_pause"
  )
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

pkgload::load_all(repo, quiet = TRUE)
source(file.path(
  repo, "evaluation", "publication_validation",
  "publication_validation_helpers.R"
))

# Phase A: only raw timestamps are available to the detector.
raw <- utils::read.csv(raw_path, check.names = FALSE)
spikes <- lapply(raw, function(x) {
  x <- suppressWarnings(as.numeric(x))
  sort(unique(x[is.finite(x)]))
})
trains <- stpd_pub_make_trains(spikes, names(spikes))
ds <- SpikeTrainPatternDetector:::make_dataset(
  name = "Grechishnikova_2017_zero_shot",
  source = "raw_timestamps_only_label_blind",
  trains = trains,
  unit_in = "s"
)
params <- default_params_sec()
params$event_grammar$threshold_source_mode <- "auto"
params$engine$threshold_source_mode <- "auto"
params$event_grammar$effective_bands <- NULL
params$event_grammar$manual_event_table <- NULL
params$event_grammar$manual_suggest <- NULL
params$event_core$use_manual_isi_calibration <- FALSE

started <- Sys.time()
detected <- stpd_detect(
  ds,
  params = params,
  selected_trains = names(spikes),
  lock_manual = FALSE,
  collect_diagnostics = FALSE,
  label_blind = TRUE,
  audit_level = "off"
)
elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
prediction <- stpd_pub_extract_predictions(detected, names(spikes))

prediction_path <- file.path(
  out_dir, "predicted_intervals_label_blind.csv"
)
saveRDS(
  detected,
  file.path(out_dir, "detector_result_label_blind.rds"),
  version = 3
)
utils::write.csv(prediction$interval, prediction_path, row.names = FALSE)
utils::write.csv(
  prediction$events,
  file.path(out_dir, "predicted_events_label_blind.csv"),
  row.names = FALSE
)
utils::write.csv(
  detected$results$threshold_table,
  file.path(out_dir, "resolved_threshold_table_label_blind.csv"),
  row.names = FALSE
)
prediction_hash <- digest::digest(
  prediction_path, algo = "sha256", file = TRUE
)
utils::write.csv(data.frame(
  label_blind = TRUE,
  manual_calibration_examples = 0L,
  threshold_source_mode = "auto_histogram",
  truth_loaded_after_prediction = TRUE,
  elapsed_seconds = elapsed,
  prediction_sha256_before_scoring = prediction_hash
), file.path(out_dir, "zero_shot_protocol.csv"), row.names = FALSE)

# Phase B: the reference workbook is opened only after predictions are frozen.
sheets <- readxl::excel_sheets(truth_path)
book <- do.call(rbind, lapply(sheets, function(sheet) {
  z <- as.data.frame(readxl::read_excel(truth_path, sheet = sheet))
  z$sheet <- sheet
  z
}))
book$Train_ID <- as.character(book$train_id)
book$Right_Spike_Index <- as.integer(book$isi_index) + 1L
book$ISI_s <- as.numeric(book$isi_us) / 1e6
book$Truth <- ifelse(
  !is.na(book$event_pattern) &
    tolower(trimws(as.character(book$event_pattern))) == "pause",
  "pause", "other"
)

pred_event <- prediction$interval[
  as.character(prediction$interval$Axis) == "event",
  c("Train_ID", "Right_Spike_Index", "Prediction"),
  drop = FALSE
]
joined <- merge(
  book[c("Train_ID", "Right_Spike_Index", "ISI_s", "Truth")],
  pred_event,
  by = c("Train_ID", "Right_Spike_Index"),
  all.x = TRUE,
  sort = FALSE
)
joined$Prediction[is.na(joined$Prediction)] <- "other"
joined$Prediction <- ifelse(
  joined$Prediction == "pause", "pause", "other"
)
tp <- sum(joined$Truth == "pause" & joined$Prediction == "pause")
fp <- sum(joined$Truth != "pause" & joined$Prediction == "pause")
fn <- sum(joined$Truth == "pause" & joined$Prediction != "pause")
support <- data.frame(
  truth_n = sum(joined$Truth == "pause"),
  predicted_n = sum(joined$Prediction == "pause"),
  tp = tp,
  fp = fp,
  fn = fn,
  precision = tp / (tp + fp),
  recall = tp / (tp + fn),
  F1 = 2 * tp / (2 * tp + fp + fn)
)

truth_event_interval <- data.frame(
  Train_ID = joined$Train_ID,
  Right_Spike_Index = joined$Right_Spike_Index,
  Axis = "event",
  Truth = joined$Truth
)
truth_events <- stpd_pub_axis_runs(
  truth_event_interval, value_col = "Truth", axes = "event"
)
truth_events <- truth_events[truth_events$Pattern == "pause", ]
pred_events <- prediction$events[
  prediction$events$Axis == "event" &
    prediction$events$Pattern == "pause",
]
to_eval <- function(x) data.frame(
  train = x$Train_ID,
  pattern = "pause",
  start_isi = x$start_isi,
  end_isi = x$end_isi
)
event_eval <- stpd_evaluate_detection_results(
  to_eval(pred_events),
  to_eval(truth_events),
  iou_thresholds = c(0.25, 0.50)
)

joined$band <- ifelse(joined$ISI_s >= 0.050, ">=50ms", "<50ms")
pause_truth <- joined[joined$Truth == "pause", ]
band_recall <- do.call(rbind, lapply(
  split(pause_truth, pause_truth$band),
  function(z) data.frame(
    band = z$band[1L],
    truth_n = nrow(z),
    detected_n = sum(z$Prediction == "pause"),
    recall = mean(z$Prediction == "pause")
  )
))

utils::write.csv(
  joined,
  file.path(out_dir, "scored_pause_intervals.csv"),
  row.names = FALSE
)
utils::write.csv(
  support,
  file.path(out_dir, "pause_support_metrics.csv"),
  row.names = FALSE
)
utils::write.csv(
  event_eval$event_metrics,
  file.path(out_dir, "pause_event_metrics.csv"),
  row.names = FALSE
)
utils::write.csv(
  band_recall,
  file.path(out_dir, "pause_recall_by_50ms_band.csv"),
  row.names = FALSE
)
prediction_hash_after <- digest::digest(
  prediction_path, algo = "sha256", file = TRUE
)
stopifnot(identical(prediction_hash, prediction_hash_after))

cat("elapsed_seconds", elapsed, "\n")
print(support, row.names = FALSE)
print(event_eval$event_metrics, row.names = FALSE)
print(band_recall, row.names = FALSE)
