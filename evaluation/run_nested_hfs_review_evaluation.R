#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE, warn = 1)

# Additive, review-only evaluation of locally accelerated Burst proposals that
# were generated inside a frozen Broad-HFS parent.  This script reads already
# frozen detector_result.rds files and writes to a separate output directory.
# It never reruns detection and never writes into the canonical validation tree.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 1L && args[[1L]] %in% c("-h", "--help")) {
  cat(paste(
    "Usage:",
    "  Rscript evaluation/run_nested_hfs_review_evaluation.R [synthetic_release_dir] [truth_csv] [output_dir]",
    "",
    "truth_csv and output_dir have repository-local defaults. If synthetic_release_dir is omitted, set STPD_SYNTHETIC_RELEASE_DIR.",
    sep = "\n"
  ))
  quit(status = 0L)
}
if (length(args) > 3L) {
  stop("Expected at most synthetic_release_dir, truth_csv, and output_dir.",
       call. = FALSE)
}

command <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", command, value = TRUE)
if (length(file_arg) != 1L) {
  stop("Unable to resolve the evaluation script path.", call. = FALSE)
}
script_path <- normalizePath(sub("^--file=", "", file_arg), mustWork = TRUE)
reviewer_root <- dirname(dirname(script_path))
default_release <- Sys.getenv("STPD_SYNTHETIC_RELEASE_DIR", unset = "")
default_truth <- file.path(
  reviewer_root, "Simulator data",
  "clean_synthetic_mechanism_benchmark_v1_0_0", "ground_truth",
  "interval_truth_dual_track.csv"
)
default_output <- file.path(
  reviewer_root, "test-results", "publication_validation",
  "nested_hfs_review"
)

release_input <- if (length(args) >= 1L) args[[1L]] else default_release
if (!nzchar(release_input)) {
  stop(
    "Pass synthetic_release_dir or set STPD_SYNTHETIC_RELEASE_DIR.",
    call. = FALSE
  )
}
release_dir <- normalizePath(
  path.expand(release_input),
  mustWork = TRUE
)
truth_path <- normalizePath(
  if (length(args) >= 2L) path.expand(args[[2L]]) else default_truth,
  mustWork = TRUE
)
output_dir <- path.expand(if (length(args) >= 3L) args[[3L]] else default_output)

path_inside <- function(path, parent) {
  path <- normalizePath(path, mustWork = FALSE)
  parent <- normalizePath(parent, mustWork = TRUE)
  identical(path, parent) || startsWith(path, paste0(parent, .Platform$file.sep))
}
if (path_inside(output_dir, release_dir)) {
  stop("Review-only output must not be inside the frozen canonical release.",
       call. = FALSE)
}

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("pkgload is required to load the source checkout.", call. = FALSE)
}
if (!requireNamespace("digest", quietly = TRUE)) {
  stop("digest is required for provenance verification.", call. = FALSE)
}
pkgload::load_all(reviewer_root, quiet = TRUE)
evaluator <- get("stpd_evaluate_detection_results", envir = .GlobalEnv,
                 inherits = TRUE)

read_csv <- function(path) {
  utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
}
write_csv <- function(x, name) {
  utils::write.csv(
    as.data.frame(x, stringsAsFactors = FALSE),
    file.path(output_dir, name), row.names = FALSE, na = ""
  )
}
sha256 <- function(path) {
  digest::digest(file = path, algo = "sha256")
}
release_manifest <- function() {
  files <- sort(
    list.files(release_dir, recursive = TRUE, full.names = TRUE),
    method = "radix"
  )
  files <- files[file.info(files)$isdir %in% FALSE]
  data.frame(
    relative_path = substring(files, nchar(release_dir) + 2L),
    sha256 = vapply(files, sha256, character(1)),
    stringsAsFactors = FALSE
  )
}

canonical_before <- release_manifest()
scale_levels <- c(1L, 4L, 10L)
detector_paths <- file.path(
  release_dir, "detection", paste0("scale_", scale_levels),
  "detector_result.rds"
)
if (any(!file.exists(detector_paths))) {
  stop("Frozen release is missing one or more detector_result.rds files.",
       call. = FALSE)
}

key_path <- file.path(
  dirname(dirname(truth_path)), "ground_truth", "sample_template_scale_key.csv"
)
if (!file.exists(key_path)) {
  key_path <- file.path(dirname(truth_path), "sample_template_scale_key.csv")
}
if (!file.exists(key_path)) {
  stop("Missing sample_template_scale_key.csv beside the truth source.",
       call. = FALSE)
}
sample_key <- read_csv(key_path)
truth_interval <- read_csv(truth_path)

required_key <- c("Sample_ID", "Template_ID", "Scale_Factor", "Split")
required_truth <- c(
  "Sample_ID", "Template_ID", "Scale_Factor", "Split",
  "Right_Spike_Index", "Event_Label", "Event_ID", "State_Label"
)
if (!all(required_key %in% names(sample_key))) {
  stop("Sample key is missing required columns.", call. = FALSE)
}
if (!all(required_truth %in% names(truth_interval))) {
  stop("Interval truth is missing required columns.", call. = FALSE)
}
if (anyDuplicated(sample_key[c("Sample_ID", "Scale_Factor")])) {
  stop("Sample key is not unique by Sample_ID and Scale_Factor.",
       call. = FALSE)
}

extract_candidates <- function(scale_factor, path) {
  detected <- readRDS(path)
  if (is.null(detected$trains) || !length(detected$trains)) {
    stop("Frozen detector result has no trains at scale ", scale_factor,
         call. = FALSE)
  }
  train_names <- names(detected$trains)
  if (is.null(train_names) || any(!nzchar(train_names))) {
    stop("Frozen detector train names are missing at scale ", scale_factor,
         call. = FALSE)
  }
  rows <- lapply(seq_along(detected$trains), function(index) {
    candidate <- attr(
      detected$trains[[index]],
      "nested_hfs_burst_review_candidates", exact = TRUE
    )
    if (!is.data.frame(candidate) || !nrow(candidate)) return(NULL)
    required <- c(
      "candidate_id", "candidate_layer", "final_label", "start_isi",
      "end_isi", "n_isi", "n_spikes", "parent_hfs_candidate_id",
      "review_evidence_strength", "nested_in_hfs",
      "canonical_eligible", "review_only"
    )
    if (!all(required %in% names(candidate))) {
      stop("Nested-HFS candidate schema is incomplete at scale ",
           scale_factor, ", train ", train_names[[index]], call. = FALSE)
    }
    data.frame(
      Sample_ID = train_names[[index]],
      candidate[, required, drop = FALSE],
      stringsAsFactors = FALSE, check.names = FALSE
    )
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (!length(rows)) {
    return(data.frame(
      Sample_ID = character(), candidate_id = character(),
      candidate_layer = character(), final_label = character(),
      start_isi = integer(), end_isi = integer(), n_isi = integer(),
      n_spikes = integer(), parent_hfs_candidate_id = character(),
      review_evidence_strength = character(), nested_in_hfs = logical(),
      canonical_eligible = logical(), review_only = logical(),
      stringsAsFactors = FALSE
    ))
  }
  out <- do.call(rbind, rows)
  out$Scale_Factor <- as.integer(scale_factor)
  lookup <- sample_key[
    as.numeric(sample_key$Scale_Factor) == scale_factor &
      as.character(sample_key$Split) == "holdout",
    c("Sample_ID", "Template_ID"), drop = FALSE
  ]
  out <- merge(out, lookup, by = "Sample_ID", all.x = TRUE, sort = FALSE)
  if (anyNA(out$Template_ID)) {
    stop("A review candidate cannot be mapped to a held-out template at scale ",
         scale_factor, call. = FALSE)
  }
  out
}

candidate_all <- do.call(rbind, Map(
  extract_candidates, scale_levels, detector_paths
))
rownames(candidate_all) <- NULL
if (nrow(candidate_all)) {
  valid_candidate <-
    candidate_all$candidate_layer == "nested_hfs_local_rate_contrast" &
    candidate_all$final_label == "possible_burst" &
    as.logical(candidate_all$nested_in_hfs) &
    as.logical(candidate_all$review_only) &
    !as.logical(candidate_all$canonical_eligible)
  valid_candidate[is.na(valid_candidate)] <- FALSE
  if (!all(valid_candidate)) {
    stop("Frozen candidate table contains non-review or canonical rows.",
         call. = FALSE)
  }
  geometry_n <- as.integer(candidate_all$end_isi) -
    as.integer(candidate_all$start_isi) + 1L
  if (any(geometry_n != as.integer(candidate_all$n_isi)) ||
      any(as.integer(candidate_all$n_spikes) != geometry_n + 1L) ||
      any(as.integer(candidate_all$start_isi) < 2L)) {
    stop("Nested-HFS candidate geometry is internally inconsistent.",
         call. = FALSE)
  }
  candidate_key <- paste(
    candidate_all$Scale_Factor, candidate_all$Sample_ID,
    candidate_all$candidate_id, sep = "\r"
  )
  if (anyDuplicated(candidate_key)) {
    stop("Nested-HFS candidate identifiers are duplicated within a train.",
         call. = FALSE)
  }
}

burst_rows <- truth_interval[
  as.character(truth_interval$Split) == "holdout" &
    tolower(as.character(truth_interval$Event_Label)) == "burst",
  , drop = FALSE
]
truth_groups <- split(
  seq_len(nrow(burst_rows)),
  paste(
    burst_rows$Scale_Factor, burst_rows$Sample_ID, burst_rows$Event_ID,
    sep = "\r"
  )
)
truth_burst <- do.call(rbind, lapply(truth_groups, function(index) {
  z <- burst_rows[index, , drop = FALSE]
  nested <- tolower(as.character(z$State_Label)) == "broad_hfs"
  data.frame(
    Sample_ID = as.character(z$Sample_ID[[1L]]),
    Template_ID = as.character(z$Template_ID[[1L]]),
    Scale_Factor = as.integer(z$Scale_Factor[[1L]]),
    Event_ID = as.character(z$Event_ID[[1L]]),
    start_isi = min(as.integer(z$Right_Spike_Index)),
    end_isi = max(as.integer(z$Right_Spike_Index)),
    n_isi = nrow(z), n_spikes = nrow(z) + 1L,
    broad_hfs_overlap_isi_n = sum(nested),
    fully_nested_in_broad_hfs = all(nested),
    stringsAsFactors = FALSE
  )
}))
truth_nested <- truth_burst[truth_burst$fully_nested_in_broad_hfs, , drop = FALSE]
if (!nrow(truth_nested)) {
  stop("The held-out benchmark has no fully nested Burst truth.",
       call. = FALSE)
}
if (any(truth_burst$broad_hfs_overlap_isi_n > 0L &
        !truth_burst$fully_nested_in_broad_hfs)) {
  stop("Partially nested truth events require an explicit scoring policy.",
       call. = FALSE)
}

evaluation_specs <- data.frame(
  target = c("all_nested_burst", "exact_burst3"),
  candidate_rule = c("all_review_candidates", "n_isi_equals_2"),
  truth_rule = c(
    "burst_event_fully_nested_in_broad_hfs",
    "burst_event_fully_nested_in_broad_hfs_and_n_isi_equals_2"
  ),
  stringsAsFactors = FALSE
)

event_metric_rows <- list()
isi_metric_rows <- list()
match_rows <- list()
fragment_rows <- list()
fragment_metric_rows <- list()
count_rows <- list()
result_objects <- list()

as_evaluator_table <- function(x, pattern) {
  data.frame(
    train = as.character(x$Sample_ID), pattern = rep(pattern, nrow(x)),
    start_isi = as.integer(x$start_isi), end_isi = as.integer(x$end_isi),
    stringsAsFactors = FALSE
  )
}

for (scale_factor in scale_levels) {
  for (spec_index in seq_len(nrow(evaluation_specs))) {
    target <- evaluation_specs$target[[spec_index]]
    candidate <- candidate_all[candidate_all$Scale_Factor == scale_factor,
                               , drop = FALSE]
    reference <- truth_nested[truth_nested$Scale_Factor == scale_factor,
                              , drop = FALSE]
    if (target == "exact_burst3") {
      candidate <- candidate[as.integer(candidate$n_isi) == 2L, , drop = FALSE]
      reference <- reference[as.integer(reference$n_isi) == 2L, , drop = FALSE]
    }
    pattern <- paste0("nested_hfs_review__", target)
    predicted_eval <- as_evaluator_table(candidate, pattern)
    reference_eval <- as_evaluator_table(reference, pattern)
    result <- evaluator(
      predicted = predicted_eval, reference = reference_eval,
      class_col = "pattern", iou_thresholds = c(0.25, 0.50, 0.75)
    )
    result_key <- paste0("scale_", scale_factor, "__", target)
    result_objects[[result_key]] <- result

    add_context <- function(x) {
      x <- as.data.frame(x, stringsAsFactors = FALSE)
      if (!nrow(x)) return(x)
      x$Scale_Factor <- scale_factor
      x$target <- target
      x$analysis_role <- "review_only_noncanonical"
      x$confirmatory_estimable <- nrow(reference) > 0L
      x
    }
    event_metric_rows[[result_key]] <- add_context(result$event_metrics)
    isi_metric_rows[[result_key]] <- add_context(result$isi_metrics)
    match_rows[[result_key]] <- add_context(result$matches)
    fragment_rows[[result_key]] <- add_context(
      result$fragmentation_by_reference
    )
    fragment_metric_rows[[result_key]] <- add_context(
      result$fragmentation_metrics
    )
    count_rows[[result_key]] <- data.frame(
      Scale_Factor = scale_factor, target = target,
      candidate_n = nrow(candidate), truth_n = nrow(reference),
      candidate_isi_n = sum(as.integer(candidate$n_isi)),
      truth_isi_n = sum(as.integer(reference$n_isi)),
      candidate_train_n = length(unique(candidate$Sample_ID)),
      truth_train_n = length(unique(reference$Sample_ID)),
      confirmatory_estimable = nrow(reference) > 0L,
      interpretation = if (nrow(reference) > 0L) {
        "heldout_review_proposal_agreement_estimable"
      } else {
        "not_estimable_no_reference_truth_events"
      },
      stringsAsFactors = FALSE
    )
  }
}

bind_rows_base <- function(rows) {
  rows <- rows[vapply(rows, nrow, integer(1)) > 0L]
  if (!length(rows)) data.frame() else do.call(rbind, rows)
}
event_metrics <- bind_rows_base(event_metric_rows)
isi_metrics <- bind_rows_base(isi_metric_rows)
event_matches <- bind_rows_base(match_rows)
fragmentation <- bind_rows_base(fragment_rows)
fragmentation_metrics <- bind_rows_base(fragment_metric_rows)
counts <- do.call(rbind, count_rows)
rownames(counts) <- NULL

candidate_output <- candidate_all
candidate_output$target_all_nested_burst <- TRUE
candidate_output$target_exact_burst3 <- as.integer(candidate_output$n_isi) == 2L
truth_output <- truth_nested
truth_output$target_all_nested_burst <- TRUE
truth_output$target_exact_burst3 <- as.integer(truth_output$n_isi) == 2L

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
write_csv(evaluation_specs, "evaluation_target_definitions.csv")
write_csv(counts, "candidate_truth_counts.csv")
write_csv(event_metrics, "event_iou_metrics_025_050_075.csv")
write_csv(isi_metrics, "isi_support_metrics.csv")
write_csv(event_matches, "event_matches_025_050_075.csv")
write_csv(fragmentation, "fragmentation_by_reference.csv")
write_csv(fragmentation_metrics, "fragmentation_metrics.csv")
write_csv(candidate_output, "heldout_nested_hfs_review_candidates.csv")
write_csv(truth_output, "heldout_nested_hfs_truth_events.csv")

source_manifest <- rbind(
  data.frame(
    role = "synthetic_interval_truth", path = truth_path,
    sha256 = sha256(truth_path), stringsAsFactors = FALSE
  ),
  data.frame(
    role = "synthetic_sample_key", path = key_path,
    sha256 = sha256(key_path), stringsAsFactors = FALSE
  ),
  data.frame(
    role = paste0("frozen_detector_result_scale_", scale_levels),
    path = detector_paths,
    sha256 = vapply(detector_paths, sha256, character(1)),
    stringsAsFactors = FALSE
  ),
  data.frame(
    role = c("evaluation_script", "official_result_evaluator_source"),
    path = c(script_path, file.path(reviewer_root, "R", "28_validation.R")),
    sha256 = vapply(
      c(script_path, file.path(reviewer_root, "R", "28_validation.R")),
      sha256, character(1)
    ),
    stringsAsFactors = FALSE
  )
)
write_csv(source_manifest, "source_sha256.csv")
write_csv(canonical_before, "frozen_release_sha256_before.csv")

run_metadata <- data.frame(
  schema_version = "stpd_nested_hfs_review_evaluation_v1",
  analysis_role = "review_only_noncanonical",
  detector_rerun = FALSE,
  canonical_metrics_eligible = FALSE,
  scale_calibration_policy = "each_scale_already_calibrated_and_detected_separately",
  truth_access_policy = "truth_read_only_after_frozen_detection_artifacts",
  coordinate_system = "inclusive_right_spike_index_isi_intervals",
  matching_rule = unique(vapply(
    result_objects, function(x) x$matching_rule, character(1)
  )),
  synthetic_negative_reference_status = "exhaustive_generator_truth",
  real_negative_reference_status = "none_explicitly_reviewed_not_evaluated_here",
  stringsAsFactors = FALSE
)
write_csv(run_metadata, "run_metadata.csv")
saveRDS(
  list(
    metadata = run_metadata, target_definitions = evaluation_specs,
    counts = counts, event_metrics = event_metrics,
    isi_metrics = isi_metrics, matches = event_matches,
    fragmentation_by_reference = fragmentation,
    fragmentation_metrics = fragmentation_metrics,
    candidates = candidate_output, truth = truth_output,
    evaluations = result_objects, source_manifest = source_manifest
  ),
  file.path(output_dir, "nested_hfs_review_evaluation.rds"), version = 3
)

readme <- c(
  "# Nested-HFS Burst review-only evaluation",
  "",
  "This directory is an additive, non-canonical evaluation artifact. It reads",
  "the already frozen synthetic `detector_result.rds` files and evaluates only",
  "the `nested_hfs_local_rate_contrast` proposals stored on the Review track.",
  "It does not rerun detection and does not replace or enter canonical Burst,",
  "Pause, Tonic, or Broad-HFS metrics.",
  "",
  "## Targets",
  "",
  "- `all_nested_burst`: every review proposal versus every held-out Burst Event",
  "  fully contained in generator Broad-HFS truth.",
  "- `exact_burst3`: only proposals and truth events containing exactly two ISIs",
  "  (three spikes). The current frozen benchmark contains no exact Burst3 truth;",
  "  therefore its recall is not estimable. This is a benchmark-coverage finding,",
  "  not evidence that the detector fails to detect Burst3.",
  "",
  "The three time scales are evaluated separately with the official",
  "`stpd_evaluate_detection_results()` function. Event IoU at 0.25/0.50/0.75,",
  "ISI support, candidate/truth counts, and fragmentation remain separate.",
  "",
  "## Real-data boundary",
  "",
  "The current real reference has no explicitly reviewed negative intervals",
  "(`explicit_reviewed_negative_n = 0`). A later real-data nested-HFS analysis",
  "may report positive-reference recall and reviewer workload/candidate burden,",
  "but must not present precision, specificity, or F1 as confirmatory biological",
  "performance until explicit reviewed negatives are available. The frozen real",
  "LOGO artifact also does not serialize these review candidates, so such an",
  "analysis requires a label-blind rerun that exports the Review-track table.",
  "",
  "## Reproduce",
  "",
  "From the Reviewer repository root:",
  "",
  "```bash",
  "Rscript evaluation/run_nested_hfs_review_evaluation.R",
  "```",
  "",
  "Input SHA-256 values and a complete before/after hash audit of the frozen",
  "release are included. A changed release hash causes the script to fail."
)
writeLines(readme, file.path(output_dir, "README.md"), useBytes = TRUE)

canonical_after <- release_manifest()
if (!identical(canonical_before, canonical_after)) {
  stop("Frozen canonical release changed during review-only evaluation.",
       call. = FALSE)
}
write_csv(canonical_after, "frozen_release_sha256_after.csv")
write_csv(
  data.frame(
    check = c(
      "frozen_release_unchanged", "detector_rerun",
      "canonical_metrics_written", "all_outputs_outside_release"
    ),
    value = c(TRUE, FALSE, FALSE, !path_inside(output_dir, release_dir)),
    stringsAsFactors = FALSE
  ),
  "isolation_audit.csv"
)

cat("Nested-HFS review-only evaluation complete.\n")
cat("Output:", normalizePath(output_dir, mustWork = TRUE), "\n")
print(counts, row.names = FALSE)
