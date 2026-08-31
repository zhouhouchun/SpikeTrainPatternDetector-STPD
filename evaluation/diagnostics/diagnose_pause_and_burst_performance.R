#!/usr/bin/env Rscript

# Diagnose Pause and Burst performance from frozen validation outputs.
#
# This script does not change detector parameters or predictions.  It makes the
# error decomposition used in the accompanying technical note reproducible:
# (1) real-data Pause errors by final label, adaptive threshold and local
#     event context; and
# (2) the distinction between the manual-10 transfer experiment and the
#     simulator-informed stress harness for Burst.

options(stringsAsFactors = FALSE, warn = 1)

args <- commandArgs(trailingOnly = TRUE)
command <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", command, value = TRUE)
script_path <- if (length(file_arg) == 1L) {
  normalizePath(sub("^--file=", "", file_arg), mustWork = TRUE)
} else {
  normalizePath(
    "evaluation/diagnostics/diagnose_pause_and_burst_performance.R",
    mustWork = TRUE
  )
}
repo_default <- normalizePath(
  file.path(dirname(script_path), "..", ".."), mustWork = TRUE
)
repo <- if (length(args) >= 1L) {
  normalizePath(args[[1L]], mustWork = TRUE)
} else {
  normalizePath(Sys.getenv("STPD_REPO_ROOT", unset = repo_default), mustWork = TRUE)
}
out_dir <- if (length(args) >= 2L) args[[2L]] else
  file.path(repo, "test-results", "metric_diagnostics", "pause_burst_20260824")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

real_dir <- file.path(
  repo, "test-results", "publication_validation",
  "real_grechishnikova_2017_reference_eligible_interburst_pause_structural_20260824"
)
sim_dir <- file.path(repo, "test-results", "simulation_60")
raw_path <- Sys.getenv("STPD_REAL_SPIKE_CSV", unset = "")
if (!nzchar(raw_path)) {
  stop("Set STPD_REAL_SPIKE_CSV to the raw timestamp CSV.", call. = FALSE)
}
raw_path <- normalizePath(path.expand(raw_path), mustWork = TRUE)

need <- c(
  file.path(real_dir, "heldout_interval_predictions.csv"),
  file.path(real_dir, "heldout_truth_events.csv"),
  file.path(real_dir, "learned_parameters_by_fold.csv"),
  file.path(real_dir, "pooled_observed_metrics.csv"),
  file.path(sim_dir, "class_1", "simulator_informed", "detected_dataset.rds"),
  file.path(sim_dir, "manual10_learned", "class_1", "manual10_learned_detected_dataset.rds"),
  file.path(sim_dir, "manual10_learned", "class_1", "train_split.csv"),
  file.path(repo, "Simulator data", "ground_truth", "interval_labels.csv"),
  file.path(sim_dir, "manual10_learned", "separate_class_interval_metrics.csv"),
  raw_path
)
if (!all(file.exists(need))) {
  stop("Required frozen validation artifact is missing: ",
       paste(need[!file.exists(need)], collapse = " | "), call. = FALSE)
}

normalize_label <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x[is.na(x) | !nzchar(x) | x %in% c("others", "unlabeled", "none")] <- "other"
  x[x %in% c("possible_burst", "long_burst")] <- "burst"
  x
}

intervals <- read.csv(file.path(real_dir, "heldout_interval_predictions.csv"),
                      check.names = FALSE, stringsAsFactors = FALSE)
truth_events <- read.csv(file.path(real_dir, "heldout_truth_events.csv"),
                         check.names = FALSE, stringsAsFactors = FALSE)
parameters <- read.csv(file.path(real_dir, "learned_parameters_by_fold.csv"),
                       check.names = FALSE, stringsAsFactors = FALSE)
pooled <- read.csv(file.path(real_dir, "pooled_observed_metrics.csv"),
                   check.names = FALSE, stringsAsFactors = FALSE)

event_intervals <- intervals[intervals$Axis == "event", , drop = FALSE]
event_intervals$Truth <- normalize_label(event_intervals$Truth)
event_intervals$Prediction <- normalize_label(event_intervals$Prediction)
truth_events$Pattern <- normalize_label(truth_events$Pattern)

# Train q90 is an unavoidable lower bound for the generic Pause route because
# stpd_event_core_pause_global_floor() uses max(pause threshold, train q90).
raw <- read.csv(raw_path, check.names = FALSE, stringsAsFactors = FALSE)
train_q90 <- data.frame(Train_ID = names(raw), q90_train_sec = NA_real_)
for (ii in seq_along(raw)) {
  times <- suppressWarnings(as.numeric(raw[[ii]]))
  times <- times[is.finite(times)]
  isi <- diff(times)
  isi <- isi[is.finite(isi) & isi >= 0.001]
  train_q90$q90_train_sec[ii] <- if (length(isi)) {
    as.numeric(stats::quantile(isi, 0.90, names = FALSE, type = 7))
  } else NA_real_
}
pause_parameter <- parameters[parameters$Parameter == "pause.min_isi_sec",
                              c("fold_id", "Value"), drop = FALSE]
names(pause_parameter)[2L] <- "pause_min_parameter_sec"
pause_rows <- event_intervals[event_intervals$Truth == "pause", , drop = FALSE]
pause_rows <- merge(pause_rows, train_q90, by = "Train_ID", all.x = TRUE, sort = FALSE)
pause_rows <- merge(pause_rows, pause_parameter, by = "fold_id", all.x = TRUE, sort = FALSE)
pause_rows$generic_pause_floor_lower_bound_sec <- pmax(
  pause_rows$q90_train_sec, pause_rows$pause_min_parameter_sec
)
pause_rows$below_generic_floor_lower_bound <-
  pause_rows$ISI_s < pause_rows$generic_pause_floor_lower_bound_sec

pause_interval_summary <- do.call(rbind, lapply(
  sort(unique(pause_rows$Prediction)),
  function(prediction) {
    z <- pause_rows[pause_rows$Prediction == prediction, , drop = FALSE]
    data.frame(
      prediction = prediction,
      interval_n = nrow(z),
      median_isi_sec = stats::median(z$ISI_s, na.rm = TRUE),
      median_train_q90_sec = stats::median(z$q90_train_sec, na.rm = TRUE),
      below_train_q90_n = sum(z$ISI_s < z$q90_train_sec, na.rm = TRUE),
      below_train_q90_fraction = mean(z$ISI_s < z$q90_train_sec, na.rm = TRUE),
      below_generic_floor_lower_bound_n = sum(z$below_generic_floor_lower_bound, na.rm = TRUE),
      below_generic_floor_lower_bound_fraction = mean(z$below_generic_floor_lower_bound, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }
))
write.csv(pause_interval_summary,
          file.path(out_dir, "real_pause_interval_threshold_decomposition.csv"),
          row.names = FALSE)

# Event coverage answers whether a low interval recall is mostly complete
# absence of a Pause candidate or partial coverage of a multi-ISI reference.
pause_events <- truth_events[
  truth_events$Axis == "event" & truth_events$Pattern == "pause", , drop = FALSE
]
pause_event_coverage <- do.call(rbind, lapply(seq_len(nrow(pause_events)), function(ii) {
  event <- pause_events[ii, , drop = FALSE]
  z <- event_intervals[
    event_intervals$Train_ID == event$Train_ID &
      event_intervals$fold_id == event$fold_id &
      event_intervals$Right_Spike_Index >= event$start_isi &
      event_intervals$Right_Spike_Index <= event$end_isi,
    , drop = FALSE
  ]
  n_pause <- sum(z$Prediction == "pause")
  n_burst <- sum(z$Prediction == "burst")
  category <- if (n_pause == 0L) {
    if (n_burst > 0L) "none__burst_conflict" else "none__no_pause_candidate"
  } else if (n_pause < nrow(z)) {
    "partial_pause_coverage"
  } else {
    "full_pause_coverage"
  }
  data.frame(
    Train_ID = event$Train_ID, fold_id = event$fold_id,
    start_isi = event$start_isi, end_isi = event$end_isi,
    reference_isi_n = nrow(z), predicted_pause_isi_n = n_pause,
    predicted_burst_isi_n = n_burst,
    pause_coverage = n_pause / nrow(z), category = category,
    stringsAsFactors = FALSE
  )
}))
write.csv(pause_event_coverage,
          file.path(out_dir, "real_pause_event_coverage.csv"), row.names = FALSE)

# Context is based only on the independent manual reference, never on detector
# predictions.  A local inter-burst pause is bounded by immediately adjacent
# true Burst episodes with no more than four unlabelled ISIs on either side.
all_event_truth <- truth_events[truth_events$Axis == "event", , drop = FALSE]
pause_truth_context <- do.call(rbind, lapply(seq_len(nrow(pause_events)), function(ii) {
  event <- pause_events[ii, , drop = FALSE]
  z <- all_event_truth[
    all_event_truth$Train_ID == event$Train_ID & all_event_truth$fold_id == event$fold_id,
    , drop = FALSE
  ]
  z <- z[order(z$start_isi, z$end_isi), , drop = FALSE]
  position <- which(z$Pattern == "pause" & z$start_isi == event$start_isi & z$end_isi == event$end_isi)
  left <- if (length(position) == 1L && position > 1L) z[position - 1L, , drop = FALSE] else NULL
  right <- if (length(position) == 1L && position < nrow(z)) z[position + 1L, , drop = FALSE] else NULL
  left_pattern <- if (is.null(left)) "none" else left$Pattern
  right_pattern <- if (is.null(right)) "none" else right$Pattern
  left_gap <- if (is.null(left)) NA_integer_ else event$start_isi - left$end_isi - 1L
  right_gap <- if (is.null(right)) NA_integer_ else right$start_isi - event$end_isi - 1L
  context <- if (identical(left_pattern, "burst") && identical(right_pattern, "burst") &&
                 is.finite(left_gap) && is.finite(right_gap) && left_gap <= 4L && right_gap <= 4L) {
    "true_local_interburst"
  } else if (identical(left_pattern, "burst") || identical(right_pattern, "burst")) {
    "one_or_remote_burst_flank"
  } else {
    "nonburst_context"
  }
  coverage <- pause_event_coverage[
    pause_event_coverage$Train_ID == event$Train_ID &
      pause_event_coverage$fold_id == event$fold_id &
      pause_event_coverage$start_isi == event$start_isi &
      pause_event_coverage$end_isi == event$end_isi,
    , drop = FALSE
  ]
  data.frame(
    Train_ID = event$Train_ID, fold_id = event$fold_id,
    context = context, reference_isi_n = coverage$reference_isi_n,
    pause_coverage = coverage$pause_coverage,
    any_predicted_pause = coverage$predicted_pause_isi_n > 0L,
    stringsAsFactors = FALSE
  )
}))
pause_context_summary <- do.call(rbind, lapply(split(pause_truth_context, pause_truth_context$context), function(z) {
  data.frame(
    context = z$context[[1L]], event_n = nrow(z), reference_isi_n = sum(z$reference_isi_n),
    no_predicted_pause_event_n = sum(!z$any_predicted_pause),
    any_predicted_pause_event_n = sum(z$any_predicted_pause),
    mean_pause_coverage = mean(z$pause_coverage), stringsAsFactors = FALSE
  )
}))
write.csv(pause_context_summary,
          file.path(out_dir, "real_pause_reference_context.csv"), row.names = FALSE)

# A direct Burst-on-Pause conflict is rare in the frozen real-data run.
confusion <- as.data.frame.matrix(table(event_intervals$Truth, event_intervals$Prediction))
confusion$Truth <- rownames(confusion)
rownames(confusion) <- NULL
write.csv(confusion, file.path(out_dir, "real_event_interval_confusion.csv"), row.names = FALSE)

# Simulator-informed versus manual-10 experiment.  The former is a stress
# harness, not a valid user-workflow estimate, because automatic generation
# merges immediately adjacent same-pattern runs into much longer truth events.
sim_informed <- readRDS(file.path(sim_dir, "class_1", "simulator_informed", "detected_dataset.rds"))
manual10 <- readRDS(file.path(sim_dir, "manual10_learned", "class_1", "manual10_learned_detected_dataset.rds"))
split <- read.csv(file.path(sim_dir, "manual10_learned", "class_1", "train_split.csv"), stringsAsFactors = FALSE)
validation_trains <- split$Train_ID[split$Role == "validation"]
truth_sim <- read.csv(file.path(repo, "Simulator data", "ground_truth", "interval_labels.csv"), stringsAsFactors = FALSE)
truth_sim <- truth_sim[truth_sim$Class == 1L & truth_sim$Train_ID %in% validation_trains, , drop = FALSE]
truth_sim$Pattern <- normalize_label(truth_sim$Pattern)

prediction_from_dataset <- function(dataset, train_ids) {
  do.call(rbind, lapply(train_ids, function(train_id) {
    dat <- dataset$trains[[train_id]]
    data.frame(
      Train_ID = train_id, Right_Spike_Index = seq_len(nrow(dat))[-1L],
      prediction = normalize_label(dat$pattern_auto[-1L]), stringsAsFactors = FALSE
    )
  }))
}
sim_informed_prediction <- prediction_from_dataset(sim_informed, validation_trains)
manual10_prediction <- prediction_from_dataset(manual10, validation_trains)
sim_joined <- merge(truth_sim[, c("Train_ID", "Right_Spike_Index", "Pattern", "Episode")],
                    sim_informed_prediction, by = c("Train_ID", "Right_Spike_Index"), sort = FALSE)
names(sim_joined)[names(sim_joined) == "prediction"] <- "simulator_informed_prediction"
sim_joined <- merge(sim_joined, manual10_prediction,
                    by = c("Train_ID", "Right_Spike_Index"), sort = FALSE)
names(sim_joined)[names(sim_joined) == "prediction"] <- "manual10_prediction"

burst_runs <- aggregate(
  cbind(reference_isi_n = rep(1L, nrow(sim_joined)),
        simulator_informed_burst_isi_n = sim_joined$simulator_informed_prediction == "burst",
        manual10_burst_isi_n = sim_joined$manual10_prediction == "burst") ~ Train_ID + Episode + Pattern,
  data = sim_joined, FUN = sum
)
burst_runs <- burst_runs[burst_runs$Pattern == "burst", , drop = FALSE]
burst_runs$truth_run_length_bin <- cut(
  burst_runs$reference_isi_n,
  c(-Inf, 3, 4, 5, 6, 8, 12, 20, 30, Inf), right = TRUE
)
sim_burst_length_summary <- do.call(rbind, lapply(split(burst_runs, burst_runs$truth_run_length_bin), function(z) {
  data.frame(
    truth_run_length_bin = as.character(z$truth_run_length_bin[[1L]]),
    truth_episode_n = nrow(z), reference_isi_n = sum(z$reference_isi_n),
    simulator_informed_burst_isi_n = sum(z$simulator_informed_burst_isi_n),
    manual10_burst_isi_n = sum(z$manual10_burst_isi_n),
    simulator_informed_full_episode_n = sum(z$simulator_informed_burst_isi_n == z$reference_isi_n),
    manual10_full_episode_n = sum(z$manual10_burst_isi_n == z$reference_isi_n),
    stringsAsFactors = FALSE
  )
}))
write.csv(sim_burst_length_summary,
          file.path(out_dir, "simulation_class1_burst_run_length_decomposition.csv"), row.names = FALSE)

event_params <- attr(sim_informed$trains[[validation_trains[[1L]]]], "event_grammar_params")
manual_params <- attr(manual10$trains[[validation_trains[[1L]]]], "event_grammar_params")
parameter_path_comparison <- data.frame(
  parameter = c("burst_seed_upper_sec", "burst_bridge_upper_sec", "classic_min_spikes",
                "classic_max_spikes", "long_max_spikes", "prolonged_max_spikes"),
  simulator_informed = c(event_params$seed_high, event_params$bridge_high,
                         event_params$min_spikes, event_params$classic_max_spikes,
                         event_params$long_max_spikes, event_params$prolonged_max_spikes),
  manual10_learned = c(manual_params$seed_high, manual_params$bridge_high,
                       manual_params$min_spikes, manual_params$classic_max_spikes,
                       manual_params$long_max_spikes, manual_params$prolonged_max_spikes),
  stringsAsFactors = FALSE
)
write.csv(parameter_path_comparison,
          file.path(out_dir, "simulation_parameter_path_comparison.csv"), row.names = FALSE)

manual10_metrics <- read.csv(file.path(sim_dir, "manual10_learned", "separate_class_interval_metrics.csv"),
                             stringsAsFactors = FALSE)
manual10_metrics <- manual10_metrics[
  manual10_metrics$Mode == "manual10_learned" &
    manual10_metrics$Label %in% c("burst", "pause"), , drop = FALSE
]
write.csv(manual10_metrics,
          file.path(out_dir, "simulation_manual10_heldout_metrics.csv"), row.names = FALSE)

simulator_informed_metrics <- read.csv(file.path(sim_dir, "class_1", "simulator_informed",
                                                  "interval_metrics_overall.csv"), stringsAsFactors = FALSE)
simulator_informed_metrics <- simulator_informed_metrics[
  simulator_informed_metrics$Label %in% c("burst", "pause"), , drop = FALSE
]
write.csv(simulator_informed_metrics,
          file.path(out_dir, "simulation_informed_stress_metrics.csv"), row.names = FALSE)

# Compact cross-cut summary for human reading and report construction.
metric_row <- function(scope, metric, value, denominator = NA_real_, note = "") {
  data.frame(scope = scope, metric = metric, value = value,
             denominator = denominator, note = note, stringsAsFactors = FALSE)
}
real_burst_interval <- pooled[
  pooled$level == "interval" & pooled$axis == "event" & pooled$label == "burst", , drop = FALSE
]
real_burst_event <- pooled[
  pooled$level == "event" & pooled$axis == "event" & pooled$label == "burst", , drop = FALSE
]
real_pause_interval <- pooled[
  pooled$level == "interval" & pooled$axis == "event" & pooled$label == "pause", , drop = FALSE
]
real_pause_event <- pooled[
  pooled$level == "event" & pooled$axis == "event" & pooled$label == "pause", , drop = FALSE
]
summary_rows <- rbind(
  metric_row("real_event", "burst_interval_F1", real_burst_interval$F1),
  metric_row("real_event", "burst_interval_recall", real_burst_interval$recall),
  metric_row("real_event", "burst_event_F1", real_burst_event$F1),
  metric_row("real_event", "burst_event_recall", real_burst_event$recall),
  metric_row("real_event", "pause_interval_F1", real_pause_interval$F1),
  metric_row("real_event", "pause_interval_recall", real_pause_interval$recall),
  metric_row("real_event", "pause_event_F1", real_pause_event$F1),
  metric_row("real_event", "pause_event_recall", real_pause_event$recall),
  metric_row("real_pause", "false_negative_to_other", sum(pause_rows$Prediction == "other"),
             nrow(pause_rows), "Manual Pause ISIs not called Pause"),
  metric_row("real_pause", "false_negative_to_burst", sum(pause_rows$Prediction == "burst"),
             nrow(pause_rows), "Direct Burst/Pause conflict"),
  metric_row("real_pause", "false_negative_below_generic_floor_lower_bound",
             sum(pause_rows$Prediction != "pause" & pause_rows$below_generic_floor_lower_bound),
             sum(pause_rows$Prediction != "pause"),
             "Lower bound excludes tonic guard and local/global contrast"),
  metric_row("real_pause", "no_pause_candidate_event", sum(pause_event_coverage$category == "none__no_pause_candidate"),
             nrow(pause_event_coverage)),
  metric_row("real_pause", "partial_pause_event", sum(pause_event_coverage$category == "partial_pause_coverage"),
             nrow(pause_event_coverage)),
  metric_row("simulator", "class1_informed_burst_F1",
             simulator_informed_metrics$F1[simulator_informed_metrics$Label == "burst"],
             NA_real_, "Stress harness; not a manual-parameter workflow"),
  metric_row("simulator", "class1_manual10_burst_F1",
             manual10_metrics$F1[manual10_metrics$Class == 1 & manual10_metrics$Label == "burst"],
             NA_real_, "10 calibration trains; disjoint 10-train validation"),
  metric_row("simulator", "class2_manual10_burst_F1",
             manual10_metrics$F1[manual10_metrics$Class == 2 & manual10_metrics$Label == "burst"],
             NA_real_, "10 calibration trains; disjoint 10-train validation"),
  metric_row("simulator", "class3_manual10_burst_F1",
             manual10_metrics$F1[manual10_metrics$Class == 3 & manual10_metrics$Label == "burst"],
             NA_real_, "10 calibration trains; disjoint 10-train validation")
)
write.csv(summary_rows, file.path(out_dir, "diagnostic_summary.csv"), row.names = FALSE)

writeLines(c(
  "# Pause and Burst diagnostic outputs",
  "",
  "Generated from frozen real LOGO validation outputs and independent simulation artifacts.",
  "The script does not tune parameters, use held-out labels for detection, or rewrite predictions.",
  "",
  "Key definitions:",
  "- Real-data final labels are evaluated on the Event axis.",
  "- Generic Pause floor lower bound = max(fold pause.min_isi_sec, raw train q90 ISI).",
  "- A true local inter-burst Pause is flanked by true Burst episodes within four unlabelled ISIs.",
  "- The simulator-informed class-1 run is a stress harness, not a user-workflow validation,",
  "  because automatically adjacent same-label generator runs are merged into a single truth episode."
), file.path(out_dir, "README.md"))

message("Wrote diagnostic outputs to: ", normalizePath(out_dir))
