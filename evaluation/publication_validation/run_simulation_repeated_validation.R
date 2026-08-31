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
if (length(args) < 1L || !args[[1L]] %in% c("1", "2", "3")) {
  stop("Usage: Rscript run_simulation_repeated_validation.R <class 1|2|3> [repeats=20] [workers=4] [output_tag]",
       call. = FALSE)
}
class_id <- as.integer(args[[1L]])
n_repeats <- if (length(args) >= 2L) as.integer(args[[2L]]) else 20L
workers <- if (length(args) >= 3L) as.integer(args[[3L]]) else 4L
output_tag <- if (length(args) >= 4L) as.character(args[[4L]]) else ""
bounded_borrowing_variant <- Sys.getenv(
  "STPD_BOUNDED_BORROWING_VARIANT", unset = "enabled"
)
stopifnot(n_repeats >= 2L, workers >= 1L)
if (nzchar(output_tag) && !grepl("^[A-Za-z0-9_-]+$", output_tag)) {
  stop("output_tag may contain only letters, numbers, underscore, and hyphen.",
       call. = FALSE)
}
if (!bounded_borrowing_variant %in% c("enabled", "disabled_ablation")) {
  stop(
    "STPD_BOUNDED_BORROWING_VARIANT must be enabled or disabled_ablation.",
    call. = FALSE
  )
}

simulation_data_root <- Sys.getenv(
  "STPD_SIM_DATA_ROOT", unset = file.path(repo, "Simulator data")
)
simulation_data_root <- normalizePath(path.expand(simulation_data_root), mustWork = TRUE)
validation_output_root <- Sys.getenv(
  "STPD_VALIDATION_OUTPUT_ROOT",
  unset = file.path(repo, "test-results", "publication_validation")
)
validation_output_root <- path.expand(validation_output_root)
out_name <- paste0("class_", class_id)
if (nzchar(output_tag)) out_name <- paste(out_name, output_tag, sep = "_")
out_dir <- file.path(validation_output_root, "simulation", out_name)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
pkgload::load_all(repo, quiet = TRUE)
source(file.path(repo, "evaluation", "publication_validation",
                 "publication_validation_helpers.R"), local = FALSE)

input <- read.csv(file.path(simulation_data_root, "detector_inputs", "spike_times_blinded.csv"),
                  check.names = FALSE, stringsAsFactors = FALSE)
truth_i <- read.csv(file.path(simulation_data_root, "ground_truth", "interval_labels.csv"),
                    check.names = FALSE, stringsAsFactors = FALSE)
input <- input[input$Class == class_id, , drop = FALSE]
truth_i <- truth_i[truth_i$Class == class_id, , drop = FALSE]
input$Train_ID <- as.character(input$Train_ID)
truth_i$Train_ID <- as.character(truth_i$Train_ID)
trains <- sort(unique(input$Train_ID), method = "radix")
stopifnot(length(trains) == 20L)

spikes <- lapply(split(input, input$Train_ID), function(z) sort(as.numeric(z$Time_s)))
intervals <- data.frame(
  Train_ID = truth_i$Train_ID, Group_ID = truth_i$Train_ID,
  Axis = ifelse(truth_i$Pattern %in% c("burst", "pause"), "event",
         ifelse(truth_i$Pattern %in% c("tonic", "high_frequency_spiking"), "state", "other")),
  Pattern = stpd_pub_normalize_pattern(truth_i$Pattern),
  Right_Spike_Index = as.integer(truth_i$Right_Spike_Index),
  ISI_s = as.numeric(truth_i$ISI_s), stringsAsFactors = FALSE
)
episodes <- stpd_pub_contiguous_episodes(intervals)
intervals <- stpd_pub_assign_episode_ids(intervals, episodes)
if (anyDuplicated(intervals[c("Train_ID", "Right_Spike_Index")])) {
  stop("Simulation interval truth contains duplicate train/index keys.", call. = FALSE)
}

split_for_repeat <- function(rr) {
  set.seed(860000L + class_id * 10000L + rr)
  calibration <- sort(sample(trains, 10L, replace = FALSE), method = "radix")
  validation <- sort(setdiff(trains, calibration), method = "radix")
  list(calibration = calibration, validation = validation)
}

run_one <- function(rr) {
  split <- split_for_repeat(rr)
  stpd_pub_run_split(
    spikes = spikes, intervals = intervals, episodes = episodes,
    calibration_trains = split$calibration, validation_trains = split$validation,
    repeat_id = rr, seed = 870000L + class_id * 10000L + rr,
    dataset_name = paste0("sim_class_", class_id, "_repeat_", sprintf("%02d", rr)),
    bounded_borrowing = identical(bounded_borrowing_variant, "enabled"))
}

cat("Running simulation class", class_id, "with", n_repeats,
    "independent 10/10 holdouts and", workers, "workers.\n")
results <- if (.Platform$OS.type == "unix" && workers > 1L) {
  parallel::mclapply(seq_len(n_repeats), run_one, mc.cores = workers,
                     mc.preschedule = FALSE, mc.set.seed = FALSE)
} else lapply(seq_len(n_repeats), run_one)
failed <- vapply(results, inherits, logical(1), what = "try-error")
if (any(failed)) stop("One or more repeated simulation runs failed.", call. = FALSE)

counts <- do.call(rbind, lapply(results, `[[`, "counts"))
splits <- do.call(rbind, lapply(results, `[[`, "split"))
selected <- do.call(rbind, Map(function(x, rr) transform(x$selected, repeat_id = rr),
                               results, seq_along(results)))
parameters <- do.call(rbind, Map(function(x, rr) transform(x$parameter_manifest, repeat_id = rr),
                                 results, seq_along(results)))
runtimes <- do.call(rbind, lapply(results, `[[`, "runtime"))

repeat_scoped_events <- function(field) {
  do.call(rbind, Map(function(result, rr) {
    z <- as.data.frame(result[[field]], stringsAsFactors = FALSE)
    if (!nrow(z)) return(z)
    z$Train_ID <- paste0("repeat_", sprintf("%02d", rr), "::", z$Train_ID)
    z
  }, results, seq_along(results)))
}
confirmatory_truth <- stpd_pub_confirmatory_evaluation_events(
  repeat_scoped_events("truth_events")
)
confirmatory_prediction <- stpd_pub_confirmatory_evaluation_events(
  repeat_scoped_events("predicted_events")
)
confirmatory_evaluation <- stpd_evaluate_detection_results(
  confirmatory_prediction, confirmatory_truth,
  iou_thresholds = c(0.25, 0.50, 0.75)
)

per_repeat <- do.call(rbind, lapply(split(counts, counts$repeat_id), function(z) {
  m <- stpd_pub_metric_from_counts(z)
  m$repeat_id <- unique(z$repeat_id)
  m
}))
bootstrap <- stpd_pub_bootstrap(counts, n_bootstrap = 1000L,
                                seed = 880000L + class_id)
repeat_summary <- do.call(rbind, lapply(split(per_repeat,
  paste(per_repeat$level, per_repeat$axis, per_repeat$label, sep = "\r")), function(z) {
    data.frame(level = z$level[1], axis = z$axis[1], label = z$label[1],
      repeat_n = length(unique(z$repeat_id)), mean_precision = mean(z$precision, na.rm = TRUE),
      mean_recall = mean(z$recall, na.rm = TRUE), mean_F1 = mean(z$F1, na.rm = TRUE),
      sd_F1 = stats::sd(z$F1, na.rm = TRUE),
      q025_F1 = stats::quantile(z$F1, .025, na.rm = TRUE, names = FALSE, type = 6),
      q50_F1 = stats::quantile(z$F1, .5, na.rm = TRUE, names = FALSE, type = 6),
      q975_F1 = stats::quantile(z$F1, .975, na.rm = TRUE, names = FALSE, type = 6))
  }))

write.csv(counts, file.path(out_dir, "cluster_counts.csv"), row.names = FALSE)
write.csv(splits, file.path(out_dir, "repeated_train_splits.csv"), row.names = FALSE)
write.csv(selected, file.path(out_dir, "sampled_10_per_pattern.csv"), row.names = FALSE)
write.csv(parameters, file.path(out_dir, "learned_parameters_by_repeat.csv"), row.names = FALSE)
write.csv(runtimes, file.path(out_dir, "runtime_and_candidate_status.csv"), row.names = FALSE)
write.csv(per_repeat, file.path(out_dir, "metrics_by_repeat.csv"), row.names = FALSE)
write.csv(repeat_summary, file.path(out_dir, "repeated_split_summary.csv"), row.names = FALSE)
write.csv(bootstrap$observed, file.path(out_dir, "pooled_observed_metrics.csv"), row.names = FALSE)
write.csv(bootstrap$summary, file.path(out_dir, "cluster_bootstrap_95ci.csv"), row.names = FALSE)
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
saveRDS(bootstrap$bootstrap, file.path(out_dir, "cluster_bootstrap_draws.rds"), version = 3)

quality <- data.frame(
  check = c("class", "train_n", "repeat_n", "calibration_train_n_each",
    "validation_train_n_each", "train_overlap_max", "examples_each_pattern_min",
    "examples_each_pattern_max", "label_blind_all", "multitrack_authoritative_any",
    "bootstrap_n", "event_iou_min", "simulation_data_root"),
  value = c(class_id, length(trains), n_repeats,
    paste(range(table(splits$repeat_id[splits$Role == "calibration"])), collapse = ".."),
    paste(range(table(splits$repeat_id[splits$Role == "validation"])), collapse = ".."),
    max(vapply(split(splits, splits$repeat_id), function(z)
      length(intersect(z$Train_ID[z$Role == "calibration"], z$Train_ID[z$Role == "validation"])), integer(1))),
    min(table(selected$repeat_id, selected$Pattern)), max(table(selected$repeat_id, selected$Pattern)),
    all(runtimes$label_blind), any(runtimes$multitrack_authoritative), 1000L, 0.25,
    simulation_data_root),
  stringsAsFactors = FALSE)
write.csv(quality, file.path(out_dir, "data_quality_and_protocol_checks.csv"), row.names = FALSE)

saveRDS(list(class_id = class_id, counts = counts, splits = splits, selected = selected,
             parameters = parameters, runtimes = runtimes, per_repeat = per_repeat,
             repeat_summary = repeat_summary, bootstrap_summary = bootstrap$summary,
             confirmatory_evaluation = confirmatory_evaluation,
             quality = quality),
        file.path(out_dir, "publication_validation_result.rds"), version = 3)
cat("Completed class", class_id, ":", n_repeats, "repeats;",
    sum(runtimes$elapsed_seconds), "detector-seconds.\n")
print(bootstrap$summary[bootstrap$summary$metric == "F1" &
  bootstrap$summary$label != "other", ], row.names = FALSE)
