#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE, warn = 1)
command <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", command, value = TRUE)
if (length(file_arg) != 1L) {
  stop("Unable to resolve the validation script path.", call. = FALSE)
}
script_path <- normalizePath(sub("^--file=", "", file_arg), mustWork = TRUE)
repo_default <- normalizePath(
  file.path(dirname(script_path), "..", ".."), mustWork = TRUE
)
repo <- normalizePath(
  Sys.getenv("STPD_REPO_ROOT", unset = repo_default), mustWork = TRUE
)
args <- commandArgs(trailingOnly = TRUE)
workers <- if (length(args) >= 1L) as.integer(args[[1L]]) else 4L
analysis_variant <- if (length(args) >= 2L) as.character(args[[2L]]) else "all_trains"
output_tag <- if (length(args) >= 3L) as.character(args[[3L]]) else ""
state_selection_variant <- Sys.getenv(
  "STPD_STATE_SELECTION_VARIANT", unset = "broad_hfs_parent"
)
bounded_borrowing_variant <- Sys.getenv(
  "STPD_BOUNDED_BORROWING_VARIANT", unset = "enabled"
)
stopifnot(workers >= 1L)
if (!analysis_variant %in% c("all_trains", "reference_eligible")) {
  stop("analysis_variant must be all_trains or reference_eligible.", call. = FALSE)
}
if (!state_selection_variant %in% c(
    "broad_hfs_parent", "legacy_peer_ablation")) {
  stop(
    "STPD_STATE_SELECTION_VARIANT must be broad_hfs_parent or legacy_peer_ablation.",
    call. = FALSE
  )
}
if (!bounded_borrowing_variant %in% c("enabled", "disabled_ablation")) {
  stop(
    "STPD_BOUNDED_BORROWING_VARIANT must be enabled or disabled_ablation.",
    call. = FALSE
  )
}
out_name <- if (identical(analysis_variant, "reference_eligible")) {
  "real_grechishnikova_2017_reference_eligible"
} else {
  "real_grechishnikova_2017"
}
if (nzchar(output_tag)) {
  if (!grepl("^[A-Za-z0-9_-]+$", output_tag)) {
    stop("output_tag may contain only letters, numbers, underscore, and hyphen.",
         call. = FALSE)
  }
  out_name <- paste(out_name, output_tag, sep = "_")
}
validation_output_root <- path.expand(Sys.getenv(
  "STPD_VALIDATION_OUTPUT_ROOT",
  unset = file.path(repo, "test-results", "publication_validation")
))
out_dir <- file.path(validation_output_root, out_name)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
pkgload::load_all(repo, quiet = TRUE)
if (identical(state_selection_variant, "legacy_peer_ablation")) {
  # Controlled ablation only: reproduce the previous peer-State arbitration
  # while keeping the input, calibration, candidate generators and evaluation
  # identical.  This mode is never a primary/publication result.
  package_namespace <- asNamespace("SpikeTrainPatternDetector")
  legacy_peer_selector <- function(selection_pool, patterns = NULL) {
    stpd_event_core_weighted_select(
      selection_pool, locked = NULL, patterns = patterns
    )
  }
  environment(legacy_peer_selector) <- package_namespace
  assignInNamespace(
    "stpd_multitrack_shadow_select_state_pool", legacy_peer_selector,
    ns = "SpikeTrainPatternDetector"
  )
}
source(file.path(repo, "evaluation", "publication_validation",
                 "publication_validation_helpers.R"), local = FALSE)

workbook_contract <- stpd_pub_resolve_authoritative_workbook(
  repo = repo,
  configured_path = Sys.getenv("STPD_MANUAL_REFERENCE_XLSX", unset = "")
)
xlsx <- workbook_contract$path
workbook_sha256 <- workbook_contract$sha256
raw_path <- Sys.getenv("STPD_REAL_SPIKE_CSV", unset = "")
if (!nzchar(raw_path) || !file.exists(raw_path)) {
  stop("Set STPD_REAL_SPIKE_CSV to the raw timestamp CSV.", call. = FALSE)
}
raw_path <- normalizePath(path.expand(raw_path), mustWork = TRUE)
raw <- read.csv(raw_path, check.names = FALSE, stringsAsFactors = FALSE)
sheet_names <- readxl::excel_sheets(xlsx)
rows <- lapply(sheet_names, function(sheet) {
  z <- as.data.frame(readxl::read_excel(xlsx, sheet = sheet), stringsAsFactors = FALSE)
  z$sheet <- sheet
  z
})
book <- do.call(rbind, rows)
book$train_id <- as.character(book$train_id)
book$Group_ID <- sub("_[Ff]on.*$", "", book$train_id)
book$Patient_ID <- "Grechishnikova_2017_patient_1"
book$Hemisphere <- ifelse(grepl("^LT", book$train_id), "left_STN",
                          ifelse(grepl("^RT", book$train_id), "right_STN", NA_character_))
if (anyNA(book$Hemisphere)) {
  stop("Unable to resolve left/right STN from one or more train IDs.", call. = FALSE)
}
reference_profile <- do.call(rbind, lapply(split(book, book$train_id), function(z) {
  event_labeled <- !is.na(z$event_pattern) & nzchar(trimws(as.character(z$event_pattern)))
  raw_state_labeled <- !is.na(z$state_pattern) &
    nzchar(trimws(as.character(z$state_pattern)))
  tonic_like_review <- raw_state_labeled &
    stpd_pub_normalize_pattern(z$state_pattern) == "tonic"
  state_labeled <- raw_state_labeled & !tonic_like_review
  explicit_other <- !is.na(z$other_pattern) &
    nzchar(trimws(as.character(z$other_pattern)))
  labeled <- event_labeled | raw_state_labeled
  # A few isolated positive State marks can still be used as calibration
  # examples, but they are not enough to treat every blank ISI in that train as
  # a reviewed State negative.  Ten labelled State ISIs is the prespecified
  # minimum for State-axis evaluation; Event eligibility is assessed
  # independently.  Within an eligible axis, blank rows remain `other`.
  min_axis_reference_isi_n <- 10L
  data.frame(
    Patient_ID = z$Patient_ID[[1L]], Hemisphere = z$Hemisphere[[1L]],
    Group_ID = z$Group_ID[[1L]], train_id = z$train_id[[1L]], isi_n = nrow(z),
    manually_labeled_n = sum(labeled), manual_coverage = mean(labeled),
    event_labeled_n = sum(event_labeled), state_labeled_n = sum(state_labeled),
    tonic_like_review_n = sum(tonic_like_review),
    explicit_other_n = sum(explicit_other),
    event_reference_eligible = sum(event_labeled) >= min_axis_reference_isi_n,
    state_reference_eligible = sum(state_labeled) >= min_axis_reference_isi_n,
    coexistence_reference_eligible =
      sum(event_labeled) >= min_axis_reference_isi_n &&
      sum(state_labeled) >= min_axis_reference_isi_n,
    reference_eligible = sum(event_labeled) >= min_axis_reference_isi_n ||
      sum(state_labeled) >= min_axis_reference_isi_n,
    exclusion_reason = if (sum(event_labeled) >= min_axis_reference_isi_n ||
        sum(state_labeled) >= min_axis_reference_isi_n) "" else
      "fewer_than_10_manual_intervals_on_each_axis",
    stringsAsFactors = FALSE)
}))
reference_profile <- reference_profile[order(reference_profile$train_id, method = "radix"), ]
book_all <- book
write.csv(reference_profile, file.path(out_dir, "reference_eligibility_by_train.csv"),
          row.names = FALSE)
axis_reference_eligibility <- data.frame(
  Train_ID = reference_profile$train_id,
  event_reference_eligible = reference_profile$event_reference_eligible,
  state_reference_eligible = reference_profile$state_reference_eligible,
  coexistence_reference_eligible =
    reference_profile$coexistence_reference_eligible,
  stringsAsFactors = FALSE
)
write.csv(
  axis_reference_eligibility,
  file.path(out_dir, "axis_reference_eligibility.csv"), row.names = FALSE
)
if (identical(analysis_variant, "reference_eligible")) {
  eligible_trains <- reference_profile$train_id[reference_profile$reference_eligible]
  book <- book[book$train_id %in% eligible_trains, , drop = FALSE]
}
book$Right_Spike_Index <- as.integer(book$isi_index) + 1L
book$ISI_s <- as.numeric(book$isi_us) / 1e6
spikes <- lapply(names(raw), function(nm) {
  x <- as.numeric(raw[[nm]]); x[is.finite(x)]
})
names(spikes) <- names(raw)
spikes <- spikes[names(spikes) %in% unique(book$train_id)]
stopifnot(setequal(unique(book$train_id), names(spikes)))

interval_parts <- list()
event_rows <- !is.na(book$event_pattern) & nzchar(trimws(as.character(book$event_pattern)))
state_rows <- !is.na(book$state_pattern) & nzchar(trimws(as.character(book$state_pattern)))
other_rows <- !event_rows & !state_rows
base_cols <- c("train_id", "Group_ID", "Right_Spike_Index", "ISI_s", "review_status")
add_axis <- function(index, axis, values) {
  z <- book[index, base_cols, drop = FALSE]
  names(z)[1L] <- "Train_ID"
  z$Axis <- axis
  z$Pattern <- stpd_pub_normalize_pattern(values[index])
  z
}
interval_parts[[1L]] <- add_axis(event_rows, "event", book$event_pattern)
interval_parts[[2L]] <- add_axis(state_rows, "state", book$state_pattern)
interval_parts[[3L]] <- add_axis(other_rows, "other", rep("other", nrow(book)))
intervals_raw <- do.call(rbind, interval_parts)
intervals_raw <- intervals_raw[order(
  intervals_raw$Train_ID, intervals_raw$Right_Spike_Index,
  intervals_raw$Axis, method = "radix"
), , drop = FALSE]
tonic_reference <- stpd_pub_partition_tonic_reference(
  intervals_raw, tonic_reference_role = "tonic_like_review"
)
all_tonic_rows <- !is.na(book_all$state_pattern) &
  stpd_pub_normalize_pattern(book_all$state_pattern) == "tonic"
tonic_like_source <- book_all[
  all_tonic_rows,
  c("train_id", "Group_ID", "isi_index", "isi_us", "review_status"),
  drop = FALSE
]
names(tonic_like_source)[c(1L, 3L, 4L)] <-
  c("Train_ID", "Right_Spike_Index", "ISI_s")
tonic_like_source$Right_Spike_Index <-
  as.integer(tonic_like_source$Right_Spike_Index) + 1L
tonic_like_source$ISI_s <- as.numeric(tonic_like_source$ISI_s) / 1e6
tonic_like_source$Axis <- "state"
tonic_like_source$Pattern <- "tonic"
tonic_like_partition <- stpd_pub_partition_tonic_reference(
  tonic_like_source, tonic_reference_role = "tonic_like_review"
)
tonic_like_review <- stpd_pub_tonic_like_review_audit(
  tonic_like_partition$review_intervals
)
intervals <- tonic_reference$formal_intervals
episodes <- stpd_pub_contiguous_episodes(intervals)
intervals <- stpd_pub_assign_episode_ids(intervals, episodes)
groups <- sort(unique(book$Group_ID), method = "radix")
train_group <- unique(book[c("train_id", "Group_ID")])
names(train_group)[1L] <- "Train_ID"

# Verify that the reference workbook is an exact interval rendering of the raw
# timestamps before using it in any performance calculation.
alignment <- do.call(rbind, lapply(unique(book$train_id), function(train) {
  z <- book[book$train_id == train, , drop = FALSE]
  x <- spikes[[train]]
  data.frame(Train_ID = train, Group_ID = z$Group_ID[1], raw_spike_n = length(x),
    workbook_isi_n = nrow(z), expected_isi_n = length(x) - 1L,
    max_left_error_sec = max(abs(x[-length(x)] - as.numeric(z$left_timestamp_us) / 1e6)),
    max_right_error_sec = max(abs(x[-1L] - as.numeric(z$right_timestamp_us) / 1e6)),
    index_duplicate_n = sum(duplicated(z$isi_index)),
    manually_labeled_n = sum(z$review_status == "manually_labeled"),
    unreviewed_blank_n = sum(z$review_status == "unreviewed" &
      (is.na(z$state_pattern) | !nzchar(as.character(z$state_pattern))) &
      (is.na(z$event_pattern) | !nzchar(as.character(z$event_pattern)))), stringsAsFactors = FALSE)
}))
write.csv(alignment, file.path(out_dir, "reference_alignment_audit.csv"), row.names = FALSE)
if (any(alignment$raw_spike_n - 1L != alignment$workbook_isi_n) ||
    any(alignment$max_left_error_sec != 0) || any(alignment$max_right_error_sec != 0) ||
    any(alignment$index_duplicate_n != 0L)) {
  stop("Real reference workbook is not an exact rendering of the raw timestamps.", call. = FALSE)
}

run_fold <- function(ii) {
  heldout_group <- groups[[ii]]
  validation <- sort(train_group$Train_ID[train_group$Group_ID == heldout_group], method = "radix")
  calibration <- sort(setdiff(train_group$Train_ID, validation), method = "radix")
  result <- stpd_pub_run_split(
    spikes = spikes, intervals = intervals, episodes = episodes,
    calibration_trains = calibration, validation_trains = validation,
    repeat_id = ii, seed = 910000L + ii,
    dataset_name = paste0("Grechishnikova_2017_LOGO_", heldout_group),
    bounded_borrowing = identical(bounded_borrowing_variant, "enabled"),
    reference_eligibility = axis_reference_eligibility,
    tonic_reference_role = "tonic_like_review",
    state_review_exclusions = tonic_like_review$intervals)
  result$split$heldout_group <- heldout_group
  result$runtime$heldout_group <- heldout_group
  result$runtime$state_selection_variant <- state_selection_variant
  result$runtime$bounded_borrowing_variant <- bounded_borrowing_variant
  result
}

cat("Running", length(groups), "leave-one-group-out folds with", workers, "workers.\n")
results <- if (.Platform$OS.type == "unix" && workers > 1L) {
  parallel::mclapply(seq_along(groups), run_fold, mc.cores = workers,
                     mc.preschedule = FALSE, mc.set.seed = FALSE)
} else lapply(seq_along(groups), run_fold)
failed <- vapply(results, inherits, logical(1), what = "try-error")
if (any(failed)) {
  stop("One or more real-data leave-one-group-out folds failed: ",
       paste(as.character(results[failed]), collapse = " | "), call. = FALSE)
}

counts <- do.call(rbind, lapply(results, `[[`, "counts"))
splits <- do.call(rbind, lapply(results, `[[`, "split"))
selected <- do.call(rbind, Map(function(x, rr) transform(x$selected, fold_id = rr),
                               results, seq_along(results)))
parameters <- do.call(rbind, Map(function(x, rr) transform(x$parameter_manifest, fold_id = rr),
                                 results, seq_along(results)))
runtimes <- do.call(rbind, lapply(results, `[[`, "runtime"))
candidate_provenance_columns <- c(
  "multitrack_schema_version", "multitrack_policy_hash",
  "multitrack_product_sha256"
)
if (!all(candidate_provenance_columns %in% names(runtimes)) ||
    nrow(runtimes) != length(groups) ||
    any(is.na(runtimes$multitrack_schema_version) |
          runtimes$multitrack_schema_version != "stpd_multitrack_auto_v3") ||
    any(is.na(runtimes$multitrack_policy_hash) |
          !grepl("^[0-9a-f]{64}$", runtimes$multitrack_policy_hash)) ||
    any(is.na(runtimes$multitrack_product_sha256) |
          !grepl("^[0-9a-f]{64}$", runtimes$multitrack_product_sha256))) {
  stop("Per-fold Gate B v3 candidate provenance was not collected completely.",
       call. = FALSE)
}
heldout_interval_predictions <- do.call(rbind, Map(function(x, fold_id) {
  transform(x$joined, fold_id = fold_id)
}, results, seq_along(results)))
heldout_predicted_events <- do.call(rbind, Map(function(x, fold_id) {
  transform(x$predicted_events, fold_id = fold_id)
}, results, seq_along(results)))
heldout_truth_events <- do.call(rbind, Map(function(x, fold_id) {
  transform(x$truth_events, fold_id = fold_id)
}, results, seq_along(results)))

# Additive, detector-independent Broad-HFS evaluation.  The primary interval
# metrics below remain cluster-bootstrap compatible; this table adds the
# prespecified IoU grid and explicit reference-event fragmentation diagnostics.
broad_predicted <- heldout_predicted_events[
  heldout_predicted_events$Axis == "state_hf_family" &
    heldout_predicted_events$Pattern == "high_frequency_spiking",
  , drop = FALSE
]
broad_reference <- heldout_truth_events[
  heldout_truth_events$Axis == "state_hf_family" &
    heldout_truth_events$Pattern == "high_frequency_spiking",
  , drop = FALSE
]
broad_predicted_eval <- data.frame(
  train = broad_predicted$Train_ID, pattern = broad_predicted$Pattern,
  start_isi = broad_predicted$start_isi, end_isi = broad_predicted$end_isi,
  stringsAsFactors = FALSE
)
broad_reference_eval <- data.frame(
  train = broad_reference$Train_ID, pattern = broad_reference$Pattern,
  start_isi = broad_reference$start_isi, end_isi = broad_reference$end_isi,
  stringsAsFactors = FALSE
)
broad_hfs_evaluation <- stpd_evaluate_detection_results(
  broad_predicted_eval, broad_reference_eval,
  class_col = "pattern", iou_thresholds = c(0.25, 0.50, 0.75)
)
confirmatory_evaluation <- stpd_evaluate_detection_results(
  stpd_pub_confirmatory_evaluation_events(heldout_predicted_events),
  stpd_pub_confirmatory_evaluation_events(heldout_truth_events),
  class_col = "pattern", iou_thresholds = c(0.25, 0.50, 0.75)
)

# Main analysis follows the prespecified rule: every blank/unreviewed ISI is
# treated as other.  The sensitivity table below reports recall only on rows
# with an explicit manual label, because this draft contains no reviewed
# negative/other rows from which specificity could be estimated honestly.
bootstrap <- stpd_pub_bootstrap(counts, n_bootstrap = 2000L, seed = 920001L)
fold_metrics <- do.call(rbind, lapply(split(counts, counts$repeat_id), function(z) {
  m <- stpd_pub_metric_from_counts(z); m$fold_id <- unique(z$repeat_id); m
}))

explicit_recall <- do.call(rbind, lapply(results, function(x) {
  z <- x$joined
  train_rows <- book[c("train_id", "Right_Spike_Index", "review_status")]
  names(train_rows)[1L] <- "Train_ID"
  z <- merge(z, train_rows, by = c("Train_ID", "Right_Spike_Index"), all.x = TRUE)
  z <- z[z$review_status == "manually_labeled" & z$Truth != "other", , drop = FALSE]
  if (!nrow(z)) return(data.frame())
  do.call(rbind, lapply(split(z, paste(z$Axis, z$Truth, sep = "\r")), function(q) {
    data.frame(fold_id = unique(x$runtime$repeat_id), axis = q$Axis[1], label = q$Truth[1],
      explicit_positive_n = nrow(q), detected_n = sum(q$Prediction == q$Truth),
      recall = mean(q$Prediction == q$Truth), stringsAsFactors = FALSE)
  }))
}))

write.csv(counts, file.path(out_dir, "cluster_counts.csv"), row.names = FALSE)
write.csv(splits, file.path(out_dir, "leave_one_group_out_splits.csv"), row.names = FALSE)
write.csv(selected, file.path(out_dir, "sampled_10_per_pattern_by_fold.csv"), row.names = FALSE)
write.csv(parameters, file.path(out_dir, "learned_parameters_by_fold.csv"), row.names = FALSE)
write.csv(runtimes, file.path(out_dir, "runtime_and_candidate_status.csv"), row.names = FALSE)
write.csv(heldout_interval_predictions,
          file.path(out_dir, "heldout_interval_predictions.csv"), row.names = FALSE)
write.csv(heldout_predicted_events,
          file.path(out_dir, "heldout_predicted_events.csv"), row.names = FALSE)
write.csv(heldout_truth_events,
          file.path(out_dir, "heldout_truth_events.csv"), row.names = FALSE)
write.csv(fold_metrics, file.path(out_dir, "metrics_by_heldout_group.csv"), row.names = FALSE)
write.csv(bootstrap$observed, file.path(out_dir, "pooled_observed_metrics.csv"), row.names = FALSE)
write.csv(bootstrap$summary, file.path(out_dir, "group_cluster_bootstrap_95ci.csv"), row.names = FALSE)
write.csv(explicit_recall, file.path(out_dir, "explicit_manual_positive_recall_sensitivity.csv"), row.names = FALSE)
write.csv(tonic_like_review$intervals,
          file.path(out_dir, "tonic_like_review_intervals.csv"),
          row.names = FALSE)
write.csv(tonic_like_review$episodes,
          file.path(out_dir, "tonic_like_review_episodes.csv"),
          row.names = FALSE)
write.csv(tonic_like_review$summary,
          file.path(out_dir, "tonic_like_review_summary.csv"),
          row.names = FALSE)
write.csv(broad_hfs_evaluation$event_metrics,
          file.path(out_dir, "broad_hfs_event_iou_metrics.csv"), row.names = FALSE)
write.csv(broad_hfs_evaluation$isi_metrics,
          file.path(out_dir, "broad_hfs_support_metrics.csv"), row.names = FALSE)
write.csv(broad_hfs_evaluation$fragmentation_by_reference,
          file.path(out_dir, "broad_hfs_fragmentation_by_reference.csv"), row.names = FALSE)
write.csv(broad_hfs_evaluation$fragmentation_metrics,
          file.path(out_dir, "broad_hfs_fragmentation_metrics.csv"), row.names = FALSE)
write.csv(confirmatory_evaluation$event_metrics,
          file.path(out_dir, "confirmatory_event_iou_metrics.csv"), row.names = FALSE)
write.csv(confirmatory_evaluation$matches,
          file.path(out_dir, "confirmatory_event_matches.csv"), row.names = FALSE)
write.csv(confirmatory_evaluation$isi_metrics,
          file.path(out_dir, "confirmatory_isi_support_metrics.csv"), row.names = FALSE)
write.csv(confirmatory_evaluation$isi_metrics_by_train,
          file.path(out_dir, "confirmatory_isi_support_metrics_by_train.csv"), row.names = FALSE)
write.csv(confirmatory_evaluation$fragmentation_by_reference,
          file.path(out_dir, "confirmatory_fragmentation_by_reference.csv"), row.names = FALSE)
write.csv(confirmatory_evaluation$fragmentation_metrics,
          file.path(out_dir, "confirmatory_fragmentation_metrics.csv"), row.names = FALSE)
saveRDS(bootstrap$bootstrap, file.path(out_dir, "group_cluster_bootstrap_draws.rds"), version = 3)

state_profile_label <- stpd_pub_normalize_pattern(book$state_pattern)
state_profile_label[state_profile_label == "tonic"] <- "tonic_like_review"
label_profile <- rbind(
  data.frame(axis = "event", label = stpd_pub_normalize_pattern(book$event_pattern)),
  data.frame(axis = ifelse(
    state_profile_label == "tonic_like_review", "tonic_like_review", "state"
  ), label = state_profile_label))
label_profile <- as.data.frame(table(label_profile$axis, label_profile$label), stringsAsFactors = FALSE)
names(label_profile) <- c("axis", "label", "interval_n")
write.csv(label_profile, file.path(out_dir, "reference_label_profile.csv"), row.names = FALSE)

quality <- data.frame(
  check = c("reference_authority", "workbook_sha256",
    "patient_n", "hemisphere_n", "recording_group_n",
    "train_n", "isi_n", "manual_labeled_n",
    "blank_as_other_n", "multi_axis_interval_n", "group_overlap_max", "raw_alignment_max_sec",
    "event_reference_eligible_train_n", "state_reference_eligible_train_n",
    "coexistence_reference_eligible_train_n", "axis_reference_min_labeled_isi_n",
    "examples_each_pattern_min", "examples_each_pattern_max", "label_blind_all",
    "multitrack_authoritative_any", "state_selection_variant",
    "bounded_borrowing_variant", "bootstrap_n", "event_iou_min",
    "explicit_reviewed_negative_n"),
  value = c(unique(book$authority_status),
    workbook_sha256,
    length(unique(book$Patient_ID)),
    length(unique(book$Hemisphere)), length(groups), length(spikes), nrow(book),
    sum(book$review_status == "manually_labeled"), sum(book$review_status == "unreviewed"),
    sum(event_rows & state_rows),
    max(vapply(split(splits, splits$repeat_id), function(z)
      length(intersect(z$Group_ID[z$Role == "calibration"], z$Group_ID[z$Role == "validation"])), integer(1))),
    max(alignment$max_left_error_sec, alignment$max_right_error_sec),
    sum(reference_profile$event_reference_eligible),
    sum(reference_profile$state_reference_eligible),
    sum(reference_profile$coexistence_reference_eligible), 10L,
    min(table(selected$fold_id, selected$Pattern)), max(table(selected$fold_id, selected$Pattern)),
    all(runtimes$label_blind), any(runtimes$multitrack_authoritative),
    state_selection_variant, bounded_borrowing_variant, 2000L, 0.25,
    sum(book$review_status == "manually_labeled" & !event_rows & !state_rows)),
  stringsAsFactors = FALSE)
write.csv(quality, file.path(out_dir, "data_quality_and_protocol_checks.csv"), row.names = FALSE)
write.csv(unique(book[c("Patient_ID", "Hemisphere", "Group_ID", "train_id")]),
          file.path(out_dir, "real_data_scope.csv"), row.names = FALSE)
saveRDS(list(counts = counts, splits = splits, selected = selected, parameters = parameters,
             runtimes = runtimes, fold_metrics = fold_metrics,
             bootstrap_summary = bootstrap$summary, explicit_recall = explicit_recall,
             tonic_like_review = tonic_like_review,
             broad_hfs_evaluation = broad_hfs_evaluation,
             confirmatory_evaluation = confirmatory_evaluation,
             quality = quality), file.path(out_dir, "publication_validation_result.rds"), version = 3)

cat("Completed", length(groups), "leave-one-group-out folds;",
    sum(runtimes$elapsed_seconds), "detector-seconds.\n")
print(bootstrap$summary[bootstrap$summary$metric == "F1" &
  bootstrap$summary$label != "other", ], row.names = FALSE)
