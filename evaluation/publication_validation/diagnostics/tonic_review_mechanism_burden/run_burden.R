#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
repo_root <- if (length(args)) normalizePath(args[1L], mustWork = TRUE) else getwd()
if (!file.exists(file.path(repo_root, "DESCRIPTION"))) {
  stop("Run from the Reviewer repository or pass its path as the first argument.")
}
pkgload::load_all(repo_root, quiet = TRUE)

review_fun <- getFromNamespace(
  "stpd_tonic_review_candidates", "SpikeTrainPatternDetector"
)
detect_tonic <- getFromNamespace(
  "stpd_event_core_detect_tonic", "SpikeTrainPatternDetector"
)

make_train <- function(isi) {
  timestamp <- c(0, cumsum(isi))
  data.frame(
    idx = seq_along(timestamp), timestamp_sec = timestamp,
    ISI_sec = c(NA_real_, isi), pattern_manual = "", pattern_auto = "",
    stringsAsFactors = FALSE
  )
}

make_vp <- function(lower, upper, dmin, lv_max) {
  list(
    tonic_min = lower, tonic_max = upper,
    tonic_threshold_source_mode = "user",
    tonic_bridge_upper = upper, tonic_connector_max_n = 0L,
    tonic_min_spikes = 3L, tonic_min_duration = dmin,
    tonic_lv_max = lv_max, tonic_mm_min = 0.85, tonic_mm_max = 1.25,
    tonic_burst_overlap_ref = NA_real_, tonic_burst_overlap_guard = FALSE,
    tonic_burst_overlap_guard_factor = 1.15,
    tonic_burst_overlap_lower_quantile = 0.10,
    tonic_burst_overlap_low_fraction_max = 0.05,
    tonic_burst_overlap_reference_quantile = 0.95,
    tonic_short_regular_route_enabled = FALSE,
    seed_low = 0, seed_high = lower
  )
}

cases <- list(
  fold7_near_lower = list(
    mechanism = "near_lower_single_isi",
    dat = make_train(c(
      0.080, 0.040631, 0.027115, 0.007888,
      0.021364, 0.024362, 0.020090, 0.022185, 0.028429,
      0.009162, 0.020665, 0.039933, 0.011462, 0.028964, 0.080
    )),
    vp = make_vp(
      0.0203323246, 0.0474788274, 0.05283305, 0.0412484649351685
    ),
    target_start = 6L, target_end = 10L
  ),
  fold8_failed_parent = list(
    mechanism = "local_regular_subwindow",
    dat = make_train(c(
      0.080, 0.022021, 0.031552, 0.026252, 0.053737,
      0.032333, 0.040056, 0.037961, 0.042192, 0.029252, 0.080
    )),
    vp = make_vp(
      0.0160569276, 0.0544407456, 0.0457712, 0.082493017038585
    ),
    target_start = 7L, target_end = 11L
  ),
  pure_hfs = list(
    mechanism = "negative_control_pure_hfs",
    dat = make_train(rep(0.008, 20L)),
    vp = make_vp(0.020, 0.050, 0.040, 0.050),
    target_start = NA_integer_, target_end = NA_integer_
  ),
  random_oscillation = list(
    mechanism = "negative_control_random_oscillation",
    dat = make_train(rep(c(0.0205, 0.0495), 10L)),
    vp = make_vp(0.020, 0.050, 0.040, 0.050),
    target_start = NA_integer_, target_end = NA_integer_
  ),
  stable_canonical_tonic = list(
    mechanism = "negative_control_canonical_full_run",
    dat = make_train(rep(0.030, 20L)),
    vp = make_vp(0.020, 0.050, 0.040, 0.050),
    target_start = NA_integer_, target_end = NA_integer_
  )
)

params <- SpikeTrainPatternDetector::default_params()
params$detector$patterns_to_run <- "tonic"
summary_rows <- list()
detail_rows <- list()
for (case_name in names(cases)) {
  case <- cases[[case_name]]
  canonical <- detect_tonic(
    case$dat, params, case$vp, min_isi_sec = 0.001, train = case_name
  )
  input_hash_before <- digest::digest(case$dat, algo = "sha256")
  review <- review_fun(
    case$dat, params, case$vp, min_isi_sec = 0.001, train = case_name,
    canonical_tonic = canonical
  )
  input_hash_after <- digest::digest(case$dat, algo = "sha256")
  has_target <- is.finite(case$target_start) && is.finite(case$target_end)
  target_hit <- if (has_target && nrow(review)) {
    review$start_isi <= case$target_start & review$end_isi >= case$target_end
  } else logical(nrow(review))
  if (nrow(review)) {
    detail <- review
    detail$case <- case_name
    detail$intended_target_covered <- target_hit
    detail_rows[[length(detail_rows) + 1L]] <- detail
  }
  summary_rows[[length(summary_rows) + 1L]] <- data.frame(
    case = case_name, intended_mechanism = case$mechanism,
    input_isi_n = nrow(case$dat) - 1L,
    canonical_tonic_candidate_n = nrow(canonical),
    review_candidate_n = nrow(review),
    intended_target_candidate_n = sum(target_hit),
    extra_review_candidate_n = nrow(review) - sum(target_hit),
    proposed_isi_n_total = if (nrow(review)) sum(review$n_isi) else 0L,
    input_object_unchanged = identical(input_hash_before, input_hash_after),
    all_review_only = nrow(review) == 0L || all(review$review_only),
    any_canonical_eligible = nrow(review) > 0L && any(review$canonical_eligible),
    stringsAsFactors = FALSE
  )
}

summary_out <- dplyr::bind_rows(summary_rows)
detail_out <- if (length(detail_rows)) dplyr::bind_rows(detail_rows) else data.frame()
output_dir <- file.path(
  repo_root, "test-results", "publication_validation", "diagnostics",
  "tonic_review_mechanism_burden"
)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
utils::write.csv(
  summary_out, file.path(output_dir, "mechanism_burden_summary.csv"),
  row.names = FALSE, na = ""
)
utils::write.csv(
  detail_out, file.path(output_dir, "review_candidate_details.csv"),
  row.names = FALSE, na = ""
)
manifest <- data.frame(
  file = c("mechanism_burden_summary.csv", "review_candidate_details.csv"),
  sha256 = vapply(
    c("mechanism_burden_summary.csv", "review_candidate_details.csv"),
    function(name) digest::digest(
      file = file.path(output_dir, name), algo = "sha256"
    ),
    character(1)
  ),
  stringsAsFactors = FALSE
)
utils::write.csv(
  manifest, file.path(output_dir, "reproducibility_manifest.csv"),
  row.names = FALSE, na = ""
)
print(summary_out, row.names = FALSE)
