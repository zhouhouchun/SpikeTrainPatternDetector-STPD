#!/usr/bin/env Rscript

# Three-regime validation for independently annotated PD GPe/STN/GPi data.
#
# Regimes:
#   automatic     - dataset-adaptive label-blind detection; zero manual examples
#   partial_known - five-fold group CV; up to 10 training episodes per pattern
#   full_params   - all reference episodes parameterize the detector; resubstitution
#
# The full_params regime is an adaptation upper bound, not independent evidence.

options(stringsAsFactors = FALSE, warn = 1)
suppressPackageStartupMessages({
  library(pkgload)
  library(readxl)
  library(digest)
})

args <- commandArgs(trailingOnly = TRUE)
region <- if (length(args) >= 1L) toupper(args[[1L]]) else "STN"
regime <- if (length(args) >= 2L) tolower(args[[2L]]) else "automatic"
workers <- if (length(args) >= 3L) as.integer(args[[3L]]) else 2L
if (!region %in% c("GPE", "STN", "GPI")) {
  stop("region must be GPE, STN, or GPI.", call. = FALSE)
}
if (!regime %in% c("automatic", "partial_known", "full_params")) {
  stop(
    "regime must be automatic, partial_known, or full_params.",
    call. = FALSE
  )
}
stopifnot(is.finite(workers), workers >= 1L)

command <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", command, value = TRUE)
if (length(file_arg) != 1L) {
  stop("Unable to resolve validation script path.", call. = FALSE)
}
script_path <- normalizePath(sub("^--file=", "", file_arg), mustWork = TRUE)
repo_default <- normalizePath(file.path(dirname(script_path), "..", ".."))
repo <- normalizePath(
  Sys.getenv("STPD_REPO_ROOT", unset = repo_default), mustWork = TRUE
)
output_root <- path.expand(Sys.getenv(
  "STPD_MULTI_REGION_OUTPUT_ROOT",
  unset = file.path(repo, "test-results", "multi_region_three_regime_20260831")
))
out_dir <- file.path(output_root, region, regime)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

pkgload::load_all(repo, quiet = TRUE)
source(file.path(
  repo, "evaluation", "publication_validation",
  "publication_validation_helpers.R"
), local = FALSE)

specs <- list(
  GPE = list(
    workbook = file.path(
      repo, "PD_GPe", "Bagdasaryan_PD_GPe_2020_manual_isi_labels_draft_csv.xlsx"
    ),
    dataset = "Bagdasaryan_PD_GPe_2020",
    tonic_reference_role = "tonic_like_review"
  ),
  STN = list(
    workbook = file.path(
      repo, "PD_STN", "PD_STN_Grechishnikova_2017_manual_isi_labels_draft_csv.xlsx"
    ),
    dataset = "Grechishnikova_PD_STN_2017",
    tonic_reference_role = "tonic_like_review"
  ),
  GPI = list(
    workbook = file.path(
      repo, "PD_GPi", "Kurmanaeva_PD_GPi_2017_manual_isi_labels_draft_csv.xlsx"
    ),
    dataset = "Kurmanaeva_PD_GPi_2017",
    tonic_reference_role = "formal_state"
  )
)
spec <- specs[[region]]
spec$workbook <- normalizePath(spec$workbook, mustWork = TRUE)

resolve_group <- function(train, region) {
  train <- as.character(train)
  if (region == "STN") {
    return(sub("_[Ff]on.*$", "", train))
  }
  if (region == "GPI") {
    out <- sub(" \\(flag [12]\\)$", "", train)
    out <- sub("_[12]-[12]$", "", out)
    out <- sub("_1_[12]$", "_1", out)
    return(out)
  }
  train
}

read_reference <- function(path, region, tonic_reference_role) {
  sheets <- readxl::excel_sheets(path)
  rows <- lapply(sheets, function(sheet) {
    z <- as.data.frame(
      readxl::read_excel(path, sheet = sheet), stringsAsFactors = FALSE
    )
    z$sheet <- sheet
    z
  })
  book <- do.call(rbind, rows)
  required <- c(
    "train_id", "isi_index", "left_timestamp_us", "right_timestamp_us",
    "isi_us", "state_pattern", "event_pattern", "review_status"
  )
  if (!all(required %in% names(book))) {
    stop("Reference workbook schema is incomplete.", call. = FALSE)
  }
  book$train_id <- as.character(book$train_id)
  book$Group_ID <- resolve_group(book$train_id, region)
  book$Right_Spike_Index <- as.integer(book$isi_index) + 1L
  book$ISI_s <- as.numeric(book$isi_us) / 1e6

  by_train <- split(book, book$train_id)
  alignment <- do.call(rbind, lapply(by_train, function(z) {
    z <- z[order(as.integer(z$isi_index), method = "radix"), , drop = FALSE]
    sequential_index <- identical(
      as.integer(z$isi_index), seq_len(nrow(z))
    )
    linked <- nrow(z) <= 1L || all(
      as.numeric(z$right_timestamp_us[-nrow(z)]) ==
        as.numeric(z$left_timestamp_us[-1L])
    )
    interval_exact <- all(
      as.numeric(z$right_timestamp_us) - as.numeric(z$left_timestamp_us) ==
        as.numeric(z$isi_us)
    )
    data.frame(
      Train_ID = as.character(z$train_id[1L]),
      Group_ID = as.character(z$Group_ID[1L]),
      isi_n = nrow(z), sequential_index = sequential_index,
      timestamp_chain_exact = linked, isi_difference_exact = interval_exact,
      manually_labeled_n = sum(z$review_status == "manually_labeled"),
      unreviewed_n = sum(z$review_status != "manually_labeled"),
      stringsAsFactors = FALSE
    )
  }))
  if (any(!alignment$sequential_index) ||
      any(!alignment$timestamp_chain_exact) ||
      any(!alignment$isi_difference_exact)) {
    stop("Workbook timestamps fail exact reconstruction checks.", call. = FALSE)
  }
  spikes <- lapply(by_train, function(z) {
    z <- z[order(as.integer(z$isi_index), method = "radix"), , drop = FALSE]
    as.numeric(c(z$left_timestamp_us[1L], z$right_timestamp_us)) / 1e6
  })

  event_rows <- !is.na(book$event_pattern) &
    nzchar(trimws(as.character(book$event_pattern)))
  state_rows <- !is.na(book$state_pattern) &
    nzchar(trimws(as.character(book$state_pattern)))
  other_rows <- !event_rows & !state_rows
  base_cols <- c(
    "train_id", "Group_ID", "Right_Spike_Index", "ISI_s", "review_status"
  )
  add_axis <- function(index, axis, values) {
    z <- book[index, base_cols, drop = FALSE]
    names(z)[1L] <- "Train_ID"
    z$Axis <- axis
    z$Pattern <- stpd_pub_normalize_pattern(values[index])
    z
  }
  intervals_raw <- rbind(
    add_axis(event_rows, "event", book$event_pattern),
    add_axis(state_rows, "state", book$state_pattern),
    add_axis(other_rows, "other", rep("other", nrow(book)))
  )
  intervals_raw <- intervals_raw[order(
    intervals_raw$Train_ID, intervals_raw$Right_Spike_Index,
    intervals_raw$Axis, method = "radix"
  ), , drop = FALSE]
  tonic_partition <- stpd_pub_partition_tonic_reference(
    intervals_raw, tonic_reference_role = tonic_reference_role
  )
  intervals <- tonic_partition$formal_intervals
  episodes <- stpd_pub_contiguous_episodes(intervals)
  intervals <- stpd_pub_assign_episode_ids(intervals, episodes)

  profile <- do.call(rbind, lapply(by_train, function(z) {
    event_n <- sum(
      !is.na(z$event_pattern) & nzchar(trimws(as.character(z$event_pattern)))
    )
    state_label <- stpd_pub_normalize_pattern(z$state_pattern)
    state_n <- sum(
      !is.na(z$state_pattern) & nzchar(trimws(as.character(z$state_pattern))) &
        !(tonic_reference_role == "tonic_like_review" & state_label == "tonic")
    )
    data.frame(
      Train_ID = as.character(z$train_id[1L]),
      Group_ID = as.character(z$Group_ID[1L]), isi_n = nrow(z),
      manually_labeled_n = sum(z$review_status == "manually_labeled"),
      manual_coverage = mean(z$review_status == "manually_labeled"),
      event_labeled_n = event_n, state_labeled_n = state_n,
      event_reference_eligible = event_n >= 10L,
      state_reference_eligible = state_n >= 10L,
      coexistence_reference_eligible = event_n >= 10L && state_n >= 10L,
      stringsAsFactors = FALSE
    )
  }))
  eligibility <- profile[c(
    "Train_ID", "event_reference_eligible", "state_reference_eligible",
    "coexistence_reference_eligible"
  )]
  # `review_intervals` is an exclusion mask used by the scoring helpers.  The
  # partition helper also returns a descriptive Tonic copy for formal-State
  # datasets, but that copy must not be passed as an exclusion or the valid
  # GPi Tonic truth is silently removed from State scoring.
  review_intervals <- if (identical(tonic_reference_role, "tonic_like_review")) {
    tonic_partition$review_intervals
  } else {
    tonic_partition$review_intervals[FALSE, , drop = FALSE]
  }
  list(
    book = book, spikes = spikes, intervals = intervals,
    episodes = episodes, alignment = alignment, profile = profile,
    eligibility = eligibility, state_review_exclusions = review_intervals
  )
}

reference <- read_reference(
  spec$workbook, region, spec$tonic_reference_role
)
train_ids <- sort(names(reference$spikes), method = "radix")
group_map <- stats::setNames(
  reference$profile$Group_ID, reference$profile$Train_ID
)

detect_with_params <- function(params, validation_trains, dataset_name) {
  pool <- stpd_pub_make_trains(reference$spikes, validation_trains)
  ds <- SpikeTrainPatternDetector:::make_dataset(
    name = dataset_name, source = "canonical_workbook_timestamps_only",
    trains = pool, unit_in = "s"
  )
  started <- Sys.time()
  detected <- stpd_detect(
    ds, params = params, selected_trains = validation_trains,
    lock_manual = FALSE, collect_diagnostics = FALSE,
    label_blind = TRUE, audit_level = "off"
  )
  list(
    detected = detected,
    elapsed_seconds = as.numeric(difftime(Sys.time(), started, units = "secs"))
  )
}

score_detection <- function(detected, validation_trains, repeat_id) {
  prediction <- stpd_pub_extract_predictions(detected, validation_trains)
  truth <- stpd_pub_truth_axes(
    reference$intervals, validation_trains,
    state_review_exclusions = reference$state_review_exclusions
  )
  truth <- stpd_pub_filter_axis_reference(truth, reference$eligibility)
  prediction$interval <- stpd_pub_filter_axis_reference(
    prediction$interval, reference$eligibility
  )
  prediction$events <- stpd_pub_filter_axis_reference(
    prediction$events, reference$eligibility
  )
  joined <- merge(
    truth, prediction$interval,
    by = c("Train_ID", "Right_Spike_Index", "Axis"),
    all.x = TRUE, sort = FALSE
  )
  joined$Prediction[is.na(joined$Prediction)] <- "other"
  truth_events <- stpd_pub_truth_events(
    reference$intervals, validation_trains,
    state_review_exclusions = reference$state_review_exclusions
  )
  truth_events <- stpd_pub_filter_axis_reference(
    truth_events, reference$eligibility
  )
  counts <- rbind(
    stpd_pub_counts(joined, repeat_id, group_map),
    stpd_pub_event_counts(
      prediction$events, truth_events, repeat_id, group_map, iou_min = 0.25
    )
  )
  list(
    counts = counts, joined = joined,
    predicted_events = prediction$events, truth_events = truth_events,
    candidate_metadata = prediction$candidate_metadata
  )
}

calibration_patterns <- function() {
  out <- c("burst", "pause", "high_frequency_spiking", "other")
  if (spec$tonic_reference_role == "formal_state" &&
      any(reference$episodes$Pattern == "tonic")) {
    out <- c(out, "tonic")
  }
  out
}

sample_partial <- function(calibration_trains, seed) {
  pool <- reference$episodes[
    reference$episodes$Train_ID %in% calibration_trains, , drop = FALSE
  ]
  parts <- lapply(seq_along(calibration_patterns()), function(ii) {
    pattern <- calibration_patterns()[ii]
    available <- sum(pool$Pattern == pattern)
    n_each <- min(10L, available)
    if (n_each < 2L) {
      stop(
        "Fewer than two training episodes for ", pattern, ".",
        call. = FALSE
      )
    }
    stpd_pub_balanced_sample(
      pool, patterns = pattern, n_each = n_each,
      seed = as.integer(seed) + ii * 10007L
    )
  })
  do.call(rbind, parts)
}

run_automatic <- function() {
  params <- default_params_sec()
  params$event_grammar$threshold_source_mode <- "auto"
  params$engine$threshold_source_mode <- "auto"
  params$event_grammar$effective_bands <- NULL
  params$event_grammar$manual_event_table <- NULL
  params$event_grammar$manual_suggest <- NULL
  params$event_core$use_manual_isi_calibration <- FALSE
  run <- detect_with_params(
    params, train_ids, paste0(spec$dataset, "_automatic")
  )
  scored <- score_detection(run$detected, train_ids, 1L)
  list(
    results = list(scored), detected = list(run$detected), selected = data.frame(),
    manifests = data.frame(),
    splits = data.frame(
      repeat_id = 1L, Role = "validation", Train_ID = train_ids,
      Group_ID = unname(group_map[train_ids])
    ),
    runtimes = data.frame(
      repeat_id = 1L, elapsed_seconds = run$elapsed_seconds,
      label_blind = TRUE, manual_example_n = 0L,
      threshold_source_mode = "auto_histogram"
    )
  )
}

run_partial <- function() {
  groups <- sort(unique(unname(group_map[train_ids])), method = "radix")
  set.seed(731000L + match(region, c("GPE", "STN", "GPI")))
  shuffled <- sample(groups, length(groups), replace = FALSE)
  fold_map <- stats::setNames(
    rep(seq_len(min(5L, length(groups))), length.out = length(groups)),
    shuffled
  )
  fold_ids <- sort(unique(as.integer(fold_map)), method = "radix")
  run_fold <- function(fold_id) {
    validation_groups <- names(fold_map)[fold_map == fold_id]
    validation <- train_ids[unname(group_map[train_ids]) %in% validation_groups]
    calibration <- setdiff(train_ids, validation)
    selected <- sample_partial(calibration, seed = 910000L + fold_id)
    learned <- stpd_pub_learn_params(
      reference$spikes, calibration, selected, reference$intervals,
      dataset_name = paste0(spec$dataset, "_partial_fold_", fold_id),
      bounded_borrowing = TRUE,
      tonic_reference_role = spec$tonic_reference_role
    )
    run <- detect_with_params(
      learned$params, validation,
      paste0(spec$dataset, "_partial_fold_", fold_id)
    )
    list(
      scored = score_detection(run$detected, validation, fold_id),
      detected = run$detected, selected = selected,
      manifest = learned$manifest,
      split = data.frame(
        repeat_id = fold_id,
        Role = c(rep("calibration", length(calibration)),
                 rep("validation", length(validation))),
        Train_ID = c(calibration, validation),
        Group_ID = unname(group_map[c(calibration, validation)])
      ),
      runtime = data.frame(
        repeat_id = fold_id, elapsed_seconds = run$elapsed_seconds,
        label_blind = TRUE, manual_example_n = nrow(selected),
        threshold_source_mode = "balanced_partial_known"
      )
    )
  }
  folds <- if (.Platform$OS.type == "unix" && workers > 1L) {
    parallel::mclapply(
      fold_ids, run_fold, mc.cores = workers,
      mc.preschedule = FALSE, mc.set.seed = FALSE
    )
  } else {
    lapply(fold_ids, run_fold)
  }
  if (any(vapply(folds, inherits, logical(1), what = "try-error"))) {
    stop("One or more partial-known folds failed.", call. = FALSE)
  }
  list(
    results = lapply(folds, `[[`, "scored"),
    detected = lapply(folds, `[[`, "detected"),
    selected = do.call(rbind, Map(
      function(x, id) transform(x$selected, repeat_id = id),
      folds, fold_ids
    )),
    manifests = do.call(rbind, Map(
      function(x, id) transform(x$manifest, repeat_id = id),
      folds, fold_ids
    )),
    splits = do.call(rbind, lapply(folds, `[[`, "split")),
    runtimes = do.call(rbind, lapply(folds, `[[`, "runtime"))
  )
}

run_full <- function() {
  patterns <- calibration_patterns()
  selected <- reference$episodes[
    reference$episodes$Pattern %in% patterns, , drop = FALSE
  ]
  selected$Calibration_Role <- ifelse(
    selected$Pattern == "other", "negative_control", "parameter_estimation"
  )
  learned <- stpd_pub_learn_params(
    reference$spikes, train_ids, selected, reference$intervals,
    dataset_name = paste0(spec$dataset, "_full_parameters"),
    bounded_borrowing = TRUE,
    tonic_reference_role = spec$tonic_reference_role
  )
  run <- detect_with_params(
    learned$params, train_ids, paste0(spec$dataset, "_full_parameters")
  )
  scored <- score_detection(run$detected, train_ids, 1L)
  list(
    results = list(scored), detected = list(run$detected),
    selected = transform(selected, repeat_id = 1L),
    manifests = transform(learned$manifest, repeat_id = 1L),
    splits = data.frame(
      repeat_id = 1L, Role = "calibration_and_resubstitution",
      Train_ID = train_ids, Group_ID = unname(group_map[train_ids])
    ),
    runtimes = data.frame(
      repeat_id = 1L, elapsed_seconds = run$elapsed_seconds,
      label_blind = TRUE, manual_example_n = nrow(selected),
      threshold_source_mode = "full_reference_parameterized"
    )
  )
}

cat("Running", region, regime, "with", length(train_ids), "trains.\n")
bundle <- switch(
  regime,
  automatic = run_automatic(),
  partial_known = run_partial(),
  full_params = run_full()
)
counts <- do.call(rbind, lapply(bundle$results, `[[`, "counts"))
joined <- do.call(rbind, lapply(bundle$results, `[[`, "joined"))
predicted_events <- do.call(rbind, lapply(
  seq_along(bundle$results), function(ii) transform(
    bundle$results[[ii]]$predicted_events, repeat_id = ii
  )
))
truth_events <- do.call(rbind, lapply(
  seq_along(bundle$results), function(ii) transform(
    bundle$results[[ii]]$truth_events, repeat_id = ii
  )
))
metrics <- stpd_pub_metric_from_counts(counts)
bootstrap <- stpd_pub_bootstrap(
  counts, n_bootstrap = 1000L,
  seed = 20260831L + match(region, c("GPE", "STN", "GPI")) * 100L +
    match(regime, c("automatic", "partial_known", "full_params"))
)

write.csv(metrics, file.path(out_dir, "pooled_observed_metrics.csv"), row.names = FALSE)
write.csv(bootstrap$summary, file.path(out_dir, "cluster_bootstrap_95ci.csv"), row.names = FALSE)
saveRDS(bootstrap$bootstrap, file.path(out_dir, "cluster_bootstrap_draws.rds"), version = 3)
write.csv(counts, file.path(out_dir, "counts_by_train.csv"), row.names = FALSE)
write.csv(joined, file.path(out_dir, "interval_predictions.csv"), row.names = FALSE)
write.csv(predicted_events, file.path(out_dir, "predicted_events.csv"), row.names = FALSE)
write.csv(truth_events, file.path(out_dir, "truth_events.csv"), row.names = FALSE)
write.csv(bundle$selected, file.path(out_dir, "calibration_examples.csv"), row.names = FALSE)
write.csv(bundle$manifests, file.path(out_dir, "parameter_manifest.csv"), row.names = FALSE)
write.csv(bundle$splits, file.path(out_dir, "splits.csv"), row.names = FALSE)
write.csv(bundle$runtimes, file.path(out_dir, "runtime.csv"), row.names = FALSE)
write.csv(reference$alignment, file.path(out_dir, "reference_alignment_audit.csv"), row.names = FALSE)
write.csv(reference$profile, file.path(out_dir, "reference_profile.csv"), row.names = FALSE)
write.csv(data.frame(
  region = region, dataset = spec$dataset, regime = regime,
  workbook = spec$workbook,
  workbook_sha256 = digest::digest(spec$workbook, algo = "sha256", file = TRUE),
  train_n = length(train_ids), group_n = length(unique(group_map[train_ids])),
  isi_n = nrow(reference$book),
  manual_example_n = sum(bundle$runtimes$manual_example_n),
  validation_label_visible_to_detector = FALSE,
  tonic_reference_role = spec$tonic_reference_role,
  interpretation = if (regime == "automatic") {
    "zero_manual_example_dataset_adaptive"
  } else if (regime == "partial_known") {
    "five_fold_group_heldout_validation"
  } else {
    "full_parameter_resubstitution_upper_bound_not_independent_validation"
  },
  stringsAsFactors = FALSE
), file.path(out_dir, "protocol.csv"), row.names = FALSE)
saveRDS(bundle$detected, file.path(out_dir, "detector_results.rds"), version = 3)

cat("Completed", region, regime, "\n")
print(metrics[
  (metrics$level == "interval" & metrics$axis %in% c("event", "state_hf_family", "state_strict")) |
    (metrics$level == "event" & metrics$axis %in% c("event", "state_hf_family", "state_strict")),
], row.names = FALSE)
