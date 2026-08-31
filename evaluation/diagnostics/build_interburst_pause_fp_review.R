#!/usr/bin/env Rscript

# Review the Pause ISIs introduced by the structural inter-burst rule. This is
# strictly post-detection: it compares two frozen LOGO prediction tables and
# joins the manual workbook only for review; it never changes detector inputs.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3L) {
  stop(
    paste(
      "Usage:",
      "Rscript evaluation/diagnostics/build_interburst_pause_fp_review.R",
      "<baseline_validation_dir> <structural_validation_dir> <output_dir>"
    ),
    call. = FALSE
  )
}

baseline_dir <- normalizePath(args[[1L]], mustWork = TRUE)
structural_dir <- normalizePath(args[[2L]], mustWork = TRUE)
output_dir <- args[[3L]]
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

read_predictions <- function(dir) {
  path <- file.path(dir, "heldout_interval_predictions.csv")
  if (!file.exists(path)) stop("Missing prediction table: ", path, call. = FALSE)
  x <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  x[x$Axis == "event", , drop = FALSE]
}

baseline <- read_predictions(baseline_dir)
structural <- read_predictions(structural_dir)
key <- c("Train_ID", "Right_Spike_Index", "Axis", "fold_id", "Truth")
baseline <- baseline[, c(key, "Prediction"), drop = FALSE]
structural <- structural[, c(key, "Prediction", "ISI_s", "Group_ID"), drop = FALSE]
names(baseline)[names(baseline) == "Prediction"] <- "baseline_prediction"
names(structural)[names(structural) == "Prediction"] <- "structural_prediction"
joined <- merge(baseline, structural, by = key, all = FALSE, sort = FALSE)

# These are the only Pause FP rows attributable to the new rule: not Pause in
# the frozen baseline, Pause after the structural rule, and not Pause under the
# event-axis reference policy (blank event labels are intentionally other).
added <- joined[
  joined$baseline_prediction != "pause" &
    joined$structural_prediction == "pause" &
    joined$Truth != "pause",
  , drop = FALSE
]

command <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", command, value = TRUE)
script_path <- if (length(file_arg) == 1L) {
  normalizePath(sub("^--file=", "", file_arg), mustWork = TRUE)
} else {
  normalizePath("evaluation/diagnostics/build_interburst_pause_fp_review.R", mustWork = TRUE)
}
repo <- normalizePath(file.path(dirname(script_path), "..", ".."), mustWork = TRUE)
xlsx_candidates <- c(
  Sys.getenv("STPD_MANUAL_REFERENCE_XLSX", unset = ""),
  file.path(repo, "PD_STN", "PD_STN_Grechishnikova_2017_manual_isi_labels_draft_csv.xlsx"),
  file.path(repo, "PD_STN_Grechishnikova_2017_manual_isi_labels_draft_csv.xlsx")
)
xlsx_candidates <- xlsx_candidates[nzchar(xlsx_candidates)]
xlsx <- xlsx_candidates[file.exists(xlsx_candidates)][1L]
if (length(xlsx) == 0L || is.na(xlsx)) {
  stop(
    "Manual workbook not found; set STPD_MANUAL_REFERENCE_XLSX.",
    call. = FALSE
  )
}
if (!requireNamespace("readxl", quietly = TRUE)) stop("Package readxl is required.", call. = FALSE)
book <- do.call(rbind, lapply(readxl::excel_sheets(xlsx), function(sheet) {
  as.data.frame(readxl::read_excel(xlsx, sheet = sheet), stringsAsFactors = FALSE)
}))
book$train_id <- as.character(book$train_id)
book$Right_Spike_Index <- suppressWarnings(as.integer(book$isi_index)) + 1L
book$left_time_ms <- suppressWarnings(as.numeric(book$left_timestamp_us)) / 1000
book$right_time_ms <- suppressWarnings(as.numeric(book$right_timestamp_us)) / 1000
book$isi_ms <- suppressWarnings(as.numeric(book$isi_us)) / 1000
manual <- book[, c(
  "train_id", "Right_Spike_Index", "left_time_ms", "right_time_ms", "isi_ms",
  "event_pattern", "state_pattern", "review_status", "authority_status"
), drop = FALSE]
names(manual)[1L] <- "Train_ID"
review <- merge(added, manual, by = c("Train_ID", "Right_Spike_Index"), all.x = TRUE,
                sort = FALSE)

event_label <- trimws(as.character(review$event_pattern))
state_label <- trimws(as.character(review$state_pattern))
event_label[is.na(event_label) | !nzchar(event_label)] <- "(blank → other)"
state_label[is.na(state_label) | !nzchar(state_label)] <- "(none)"
review$manual_event_label <- event_label
review$manual_state_label <- state_label
review$reference_interpretation <- ifelse(
  review$Truth == "burst", "manual Burst: event-axis conflict",
  "event label blank: evaluated as other"
)

# Show local event-axis context, which is what matters for deciding whether the
# rule caught a biologically plausible burst separator or split one Burst.
context_map <- structural[, c(
  "Train_ID", "Right_Spike_Index", "structural_prediction", "Truth"
), drop = FALSE]
names(context_map)[names(context_map) == "structural_prediction"] <- "Prediction"
context_map <- split(context_map, context_map$Train_ID)
context_for_row <- function(train, index) {
  z <- context_map[[train]]
  z <- z[order(z$Right_Spike_Index), , drop = FALSE]
  take <- z[z$Right_Spike_Index >= index - 2L & z$Right_Spike_Index <= index + 2L, , drop = FALSE]
  paste(sprintf("%s:%s/%s", take$Right_Spike_Index, take$Truth, take$Prediction), collapse = " | ")
}
review$event_context_plus_minus_2_isi <- mapply(
  context_for_row, review$Train_ID, review$Right_Spike_Index, USE.NAMES = FALSE
)

by_train <- aggregate(
  Right_Spike_Index ~ Train_ID + Group_ID,
  data = review,
  FUN = length
)
names(by_train)[names(by_train) == "Right_Spike_Index"] <- "new_pause_fp_n"
by_train <- by_train[order(-by_train$new_pause_fp_n, by_train$Train_ID), , drop = FALSE]
review <- merge(review, by_train, by = c("Train_ID", "Group_ID"), all.x = TRUE, sort = FALSE)
review <- review[order(
  review$Truth != "burst", -review$new_pause_fp_n,
  review$Train_ID, review$Right_Spike_Index
), , drop = FALSE]
review$rank_within_train <- ave(
  review$Right_Spike_Index, review$Train_ID,
  FUN = function(x) seq_along(x)
)

representative <- do.call(rbind, lapply(split(review, review$Train_ID), function(z) {
  z[seq_len(min(4L, nrow(z))), , drop = FALSE]
}))
representative <- representative[order(
  representative$Truth != "burst", -representative$new_pause_fp_n,
  representative$Train_ID, representative$Right_Spike_Index
), , drop = FALSE]

write.csv(by_train, file.path(output_dir, "new_interburst_pause_false_positives_by_train.csv"),
          row.names = FALSE, na = "")
write.csv(review, file.path(output_dir, "new_interburst_pause_false_positives_all.csv"),
          row.names = FALSE, na = "")
write.csv(representative,
          file.path(output_dir, "new_interburst_pause_false_positives_representative.csv"),
          row.names = FALSE, na = "")
cat(sprintf("Wrote %d newly introduced Pause false-positive ISIs to %s\n",
            nrow(review), normalizePath(output_dir, mustWork = TRUE)))
