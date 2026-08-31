#!/usr/bin/env Rscript

# Reproducible STPD runtime benchmark.
# Author: Zhou Houchun
#
# The timed region contains detector execution only. Package loading, CSV import,
# data normalization, warm-up, result serialization, and figure rendering are
# measured separately or excluded. No reference labels are opened by this script.

options(stringsAsFactors = FALSE)

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(script_arg)) {
  sub("^--file=", "", script_arg[[1]])
} else {
  file.path(getwd(), "evaluation", "performance", "benchmark_detector_runtime.R")
}
repo_default <- file.path(dirname(normalizePath(script_path, mustWork = TRUE)), "..", "..")
repo <- normalizePath(Sys.getenv("STPD_REPO", unset = repo_default), mustWork = TRUE)

input_default <- file.path(
  repo, "data", "real", "STN", "Grechishnikova_STN_2017.csv"
)
input_path <- normalizePath(
  Sys.getenv("STPD_BENCHMARK_INPUT", unset = input_default),
  mustWork = TRUE
)
output_dir <- Sys.getenv(
  "STPD_BENCHMARK_OUT",
  unset = file.path(repo, "results", "performance", "repeated_runtime")
)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

parse_positive_integer <- function(value, name) {
  out <- suppressWarnings(as.integer(value))
  if (length(out) != 1L || is.na(out) || out < 1L) {
    stop(name, " must be a positive integer.", call. = FALSE)
  }
  out
}

repeats <- parse_positive_integer(
  Sys.getenv("STPD_BENCHMARK_REPEATS", unset = "3"),
  "STPD_BENCHMARK_REPEATS"
)
profile <- match.arg(
  Sys.getenv("STPD_BENCHMARK_PROFILE", unset = "core"),
  c("core", "app_default", "audit_summary")
)
warmup_enabled <- tolower(Sys.getenv("STPD_BENCHMARK_WARMUP", unset = "true")) %in%
  c("1", "true", "yes")
subset_spec <- strsplit(
  Sys.getenv("STPD_BENCHMARK_SUBSETS", unset = "1,5,all"),
  ",", fixed = TRUE
)[[1]]
subset_spec <- trimws(subset_spec)

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("Package 'pkgload' is required to benchmark the source tree.", call. = FALSE)
}
if (!requireNamespace("digest", quietly = TRUE)) {
  stop("Package 'digest' is required for input provenance.", call. = FALSE)
}
suppressPackageStartupMessages(pkgload::load_all(repo, quiet = TRUE))
source(file.path(
  repo, "evaluation", "publication_validation",
  "publication_validation_helpers.R"
))

normalize_csv <- function(path) {
  raw <- utils::read.csv(path, check.names = FALSE)
  spikes <- lapply(raw, function(x) {
    x <- suppressWarnings(as.numeric(x))
    sort(unique(x[is.finite(x)]))
  })
  spikes[vapply(spikes, length, integer(1)) >= 2L]
}

import_timing <- system.time(spikes <- normalize_csv(input_path))
if (!length(spikes)) stop("The benchmark input has no valid spike trains.", call. = FALSE)

spike_counts <- vapply(spikes, length, integer(1))
# A deterministic descending workload order makes every prefix reproducible and
# ensures that larger subset sizes include the smaller stress workloads.
train_order <- names(sort(spike_counts, decreasing = TRUE))
spikes <- spikes[train_order]
spike_counts <- spike_counts[train_order]

resolve_subset_size <- function(value) {
  if (tolower(value) == "all") return(length(spikes))
  out <- parse_positive_integer(value, "STPD_BENCHMARK_SUBSETS entry")
  min(out, length(spikes))
}
subset_sizes <- sort(unique(vapply(subset_spec, resolve_subset_size, integer(1))))

trains <- stpd_pub_make_trains(spikes, names(spikes))
dataset <- SpikeTrainPatternDetector:::make_dataset(
  name = "STPD_runtime_benchmark",
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

collect_diagnostics <- !identical(profile, "core")
audit_level <- if (identical(profile, "audit_summary")) "summary" else "off"

run_detector <- function(selected) {
  stpd_detect(
    dataset,
    params = params,
    selected_trains = selected,
    lock_manual = FALSE,
    collect_diagnostics = collect_diagnostics,
    label_blind = TRUE,
    audit_level = audit_level
  )
}

warmup_seconds <- 0
if (warmup_enabled) {
  warmup_seconds <- unname(system.time(
    invisible(run_detector(train_order[[1]]))
  )[["elapsed"]])
}

input_sha256 <- digest::digest(input_path, algo = "sha256", file = TRUE)
run_started_utc <- format(Sys.time(), tz = "UTC", usetz = TRUE)
run_rows <- list()

for (n_train in subset_sizes) {
  selected <- train_order[seq_len(n_train)]
  for (repeat_id in seq_len(repeats)) {
    invisible(gc(reset = TRUE))
    timing <- system.time(result <- run_detector(selected))
    heap <- gc()
    peak_r_heap_mib <- sum(heap[, 7L], na.rm = TRUE)
    run_rows[[length(run_rows) + 1L]] <- data.frame(
      profile = profile,
      repeat_id = repeat_id,
      train_n = n_train,
      spike_n = sum(spike_counts[selected]),
      isi_n = sum(pmax(spike_counts[selected] - 1L, 0L)),
      elapsed_seconds = unname(timing[["elapsed"]]),
      user_seconds = unname(timing[["user.self"]]),
      system_seconds = unname(timing[["sys.self"]]),
      seconds_per_train = unname(timing[["elapsed"]]) / n_train,
      seconds_per_1000_isi = unname(timing[["elapsed"]]) /
        max(sum(pmax(spike_counts[selected] - 1L, 0L)), 1L) * 1000,
      isi_per_second = sum(pmax(spike_counts[selected] - 1L, 0L)) /
        max(unname(timing[["elapsed"]]), .Machine$double.eps),
      approximate_peak_r_heap_mib = peak_r_heap_mib,
      result_object_mib = as.numeric(utils::object.size(result)) / 1024^2,
      collect_diagnostics = collect_diagnostics,
      audit_level = audit_level,
      label_blind = TRUE,
      manual_example_n = 0L,
      threshold_source_mode = "auto",
      input_sha256 = input_sha256,
      stringsAsFactors = FALSE
    )
    utils::write.csv(
      do.call(rbind, run_rows),
      file.path(output_dir, "detector_runtime_runs.csv"),
      row.names = FALSE
    )
    rm(result)
    invisible(gc())
  }
}

runs <- do.call(rbind, run_rows)
summary_rows <- do.call(rbind, lapply(
  split(runs, interaction(runs$profile, runs$train_n, drop = TRUE)),
  function(x) data.frame(
    profile = x$profile[[1]],
    train_n = x$train_n[[1]],
    spike_n = x$spike_n[[1]],
    isi_n = x$isi_n[[1]],
    repeat_n = nrow(x),
    elapsed_median_seconds = stats::median(x$elapsed_seconds),
    elapsed_q25_seconds = unname(stats::quantile(x$elapsed_seconds, 0.25)),
    elapsed_q75_seconds = unname(stats::quantile(x$elapsed_seconds, 0.75)),
    elapsed_min_seconds = min(x$elapsed_seconds),
    elapsed_max_seconds = max(x$elapsed_seconds),
    seconds_per_1000_isi_median = stats::median(x$seconds_per_1000_isi),
    isi_per_second_median = stats::median(x$isi_per_second),
    approximate_peak_r_heap_mib_max = max(x$approximate_peak_r_heap_mib),
    result_object_mib_median = stats::median(x$result_object_mib),
    stringsAsFactors = FALSE
  )
))
rownames(summary_rows) <- NULL
utils::write.csv(
  summary_rows,
  file.path(output_dir, "detector_runtime_summary.csv"),
  row.names = FALSE
)

utils::write.csv(data.frame(
  train = train_order,
  rank = seq_along(train_order),
  spike_n = unname(spike_counts[train_order]),
  isi_n = pmax(unname(spike_counts[train_order]) - 1L, 0L),
  stringsAsFactors = FALSE
), file.path(output_dir, "benchmark_train_workload.csv"), row.names = FALSE)

cpu_model <- tryCatch({
  value <- suppressWarnings(system2(
    "sysctl", c("-n", "machdep.cpu.brand_string"), stdout = TRUE, stderr = FALSE
  ))
  if (length(value)) value[[1]] else NA_character_
}, error = function(e) NA_character_)
environment <- data.frame(
  run_started_utc = run_started_utc,
  r_version = R.version.string,
  platform = R.version$platform,
  os = paste(Sys.info()[c("sysname", "release", "machine")], collapse = " "),
  cpu_model = cpu_model,
  logical_cores = parallel::detectCores(logical = TRUE),
  physical_cores = parallel::detectCores(logical = FALSE),
  package_version = as.character(utils::packageVersion("SpikeTrainPatternDetector")),
  input_path_public = file.path("data", "real", "STN", basename(input_path)),
  input_sha256 = input_sha256,
  input_train_n = length(spikes),
  input_spike_n = sum(spike_counts),
  input_isi_n = sum(pmax(spike_counts - 1L, 0L)),
  csv_import_and_normalize_seconds = unname(import_timing[["elapsed"]]),
  warmup_enabled = warmup_enabled,
  warmup_seconds_excluded = warmup_seconds,
  benchmark_profile = profile,
  repeats = repeats,
  subset_sizes = paste(subset_sizes, collapse = ";"),
  stringsAsFactors = FALSE
)
utils::write.csv(
  environment,
  file.path(output_dir, "benchmark_environment.csv"),
  row.names = FALSE
)

cat("Runtime benchmark completed.\n")
cat("Output:", normalizePath(output_dir, mustWork = TRUE), "\n")
print(summary_rows, row.names = FALSE)
