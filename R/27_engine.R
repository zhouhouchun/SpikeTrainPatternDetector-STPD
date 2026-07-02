# Canonical detector engine.
# This is a clean pipeline wrapper around the reference detector. The goal is
# not to change detector semantics, but to make QC, candidate generation,
# feature extraction, final classification, event audit and export consistently
# derived from one run object.

stpd_engine_prepare_params <- function(params = default_params_sec(), strict = FALSE) {
  params <- apply_schema_defaults(params)
  issues <- stpd_validate_params(params, strict = strict)
  params$meta <- params$meta %||% list()
  params$meta$params_hash <- stpd_params_hash_flat(params)
  attr(params, "validation_issues") <- issues
  params
}

stpd_detect <- function(ds, params = default_params_sec(), selected_trains = NULL,
                        lock_manual = TRUE, collect_diagnostics = TRUE,
                        strict_params = FALSE, progress_callback = NULL) {
  if (is.null(ds) || is.null(ds$trains)) stop("stpd_detect(): ds must be a dataset with a trains list.", call. = FALSE)
  params <- stpd_engine_prepare_params(params, strict = strict_params)
  run_id <- paste0("stpd_run_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  out <- run_detector_dataset_internal(
    ds,
    params = params,
    selected_trains = selected_trains,
    lock_manual = lock_manual,
    collect_diagnostics = collect_diagnostics,
    progress_callback = progress_callback
  )
  # Candidate ledger and event audit should remain distinct. Rebuild both from
  # the post-policy detector outputs in case an older reference path omitted one.
  stpd_call_progress(progress_callback, "public_ledgers", detail = "Synchronizing public candidate and event audits")
  cand_rebuilt <- FALSE
  cand <- out$results$candidate_ledger %||% NULL
  if (is.null(cand) || !is.data.frame(cand)) {
    cand <- tryCatch(build_candidate_ledger(out, params = params, selected_trains = selected_trains, run_id = run_id, params_hash = params$meta$params_hash), error = function(e) NULL)
    cand_rebuilt <- !is.null(cand)
  }
  if (!is.null(cand)) out$results$candidate_ledger <- cand
  evt <- out$results$event_audit %||% out$results$event_ledger %||% NULL
  if (is.null(evt) || !is.data.frame(evt)) {
    evt <- tryCatch(build_event_audit(out, params = params, selected_trains = selected_trains, run_id = run_id, params_hash = params$meta$params_hash), error = function(e) NULL)
  }
  if (!is.null(evt)) out$results$event_audit <- evt

  stpd_call_progress(progress_callback, "public_features", detail = "Synchronizing public candidate features")
  feats <- out$results$candidate_features %||% out$results$candidate_features_internal %||% NULL
  feature_missing <- is.null(feats) || !is.data.frame(feats) ||
    (nrow(out$results$candidate_ledger %||% data.frame()) > 0 && nrow(feats) == 0)
  if (isTRUE(cand_rebuilt) || isTRUE(feature_missing)) {
    feats <- tryCatch(compute_candidate_feature_table(out, candidates = out$results$candidate_ledger, params = params, selected_trains = selected_trains), error = function(e) tibble::tibble())
  }
  out$results$candidate_features <- feats

  stpd_call_progress(progress_callback, "public_final", detail = "Synchronizing public final decisions")
  dec <- out$results$final_decisions %||% out$results$final_classification_audit %||% out$results$final_decisions_internal %||% NULL
  decision_missing <- is.null(dec) || !is.data.frame(dec) ||
    (nrow(feats %||% data.frame()) > 0 && nrow(dec) == 0)
  if (isTRUE(cand_rebuilt) || isTRUE(feature_missing) || isTRUE(decision_missing)) {
    dec <- tryCatch(final_classify_candidates(feats, params = params), error = function(e) tibble::tibble())
  }
  out$results$final_decisions <- dec
  out$results$eventness_audit <- dec
  out$results$final_classification_audit <- dec
  stpd_call_progress(progress_callback, "distributional_evidence", detail = "Computing distributional evidence and firing phenotype summaries")
  out <- stpd_add_distributional_results(out, params = params, selected_trains = selected_trains, candidates = feats)
  out$results$parameter_validation <- attr(params, "validation_issues")
  out$results$parameter_report <- stpd_parameter_report_flat(params)
  out$results$run_metadata_public <- data.frame(
    run_id = run_id,
    params_hash = params$meta$params_hash,
    selected_trains = paste(selected_trains %||% names(out$trains), collapse = ";"),
    candidate_count = nrow(out$results$candidate_ledger %||% data.frame()),
    event_count = nrow(out$results$events %||% data.frame()),
    feature_count = nrow(feats),
    stringsAsFactors = FALSE
  )
  stpd_call_progress(progress_callback, "public_reports", detail = "Computing consistency and validation summaries")
  out$results$result_consistency <- tryCatch(stpd_result_consistency_check(out), error = function(e) data.frame(severity = "error", component = "result_consistency", issue = "consistency check failed", detail = conditionMessage(e), stringsAsFactors = FALSE))
  out$results$scientific_validation_summary <- tryCatch(stpd_scientific_validation_summary(out, params), error = function(e) data.frame(item = "scientific_validation_summary", value = paste0("failed: ", conditionMessage(e)), stringsAsFactors = FALSE))
  out$results$event_level_validation_strict <- tryCatch(stpd_event_level_validation(out, params, selected_trains = selected_trains, metric_mode = "strict_high_confidence"), error = function(e) data.frame(split = "all", metric_mode = "strict_high_confidence", pattern = NA_character_, note = paste0("failed: ", conditionMessage(e)), stringsAsFactors = FALSE))
  out$results$event_level_validation_candidate_family <- tryCatch(stpd_event_level_validation(out, params, selected_trains = selected_trains, metric_mode = "candidate_family"), error = function(e) data.frame(split = "all", metric_mode = "candidate_family", pattern = NA_character_, note = paste0("failed: ", conditionMessage(e)), stringsAsFactors = FALSE))
  stpd_call_progress(progress_callback, "public_complete", detail = "Public detector outputs are ready")
  out
}

stpd_generate_candidates <- function(ds, params = default_params_sec(), selected_trains = NULL) {
  out <- stpd_detect(ds, params, selected_trains = selected_trains, collect_diagnostics = TRUE)
  out$results$candidate_ledger %||% tibble::tibble()
}

stpd_compute_features <- function(ds, candidates = NULL, params = default_params_sec(), selected_trains = NULL) {
  compute_candidate_feature_table(ds, candidates = candidates, params = params, selected_trains = selected_trains)
}

stpd_export_results <- function(ds, params = default_params_sec(), out_dir, dataset_name = "dataset", time_unit = "ms") {
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  export_detection_results(ds, params = params, out_dir = out_dir, dataset_name = dataset_name, time_unit = time_unit)
  if (!is.null(ds$results$parameter_report)) write_csv_safe(ds$results$parameter_report, file.path(out_dir, "Parameters_report.csv"))
  if (!is.null(ds$results$parameter_validation)) write_csv_safe(ds$results$parameter_validation, file.path(out_dir, "Parameter_validation.csv"))
  if (!is.null(ds$results$run_metadata_public)) write_csv_safe(ds$results$run_metadata_public, file.path(out_dir, "Detector_run_metadata.csv"))
  if (!is.null(ds$results$result_consistency)) write_csv_safe(ds$results$result_consistency, file.path(out_dir, "Result_consistency_check.csv"))
  if (!is.null(ds$results$scientific_validation_summary)) write_csv_safe(ds$results$scientific_validation_summary, file.path(out_dir, "Scientific_validation_summary.csv"))
  if (!is.null(ds$results$event_level_validation_strict)) write_csv_safe(ds$results$event_level_validation_strict, file.path(out_dir, "Event_level_validation_strict.csv"))
  if (!is.null(ds$results$event_level_validation_candidate_family)) write_csv_safe(ds$results$event_level_validation_candidate_family, file.path(out_dir, "Event_level_validation_candidate_family.csv"))
  if (!is.null(ds$results$eventness_audit)) write_csv_safe(ds$results$eventness_audit, file.path(out_dir, "Eventness_audit.csv"))
  if (!is.null(ds$results$event_distribution_evidence)) write_csv_safe(ds$results$event_distribution_evidence, file.path(out_dir, "Event_distribution_evidence.csv"))
  if (!is.null(ds$results$train_distribution_features)) write_csv_safe(ds$results$train_distribution_features, file.path(out_dir, "Train_distribution_features.csv"))
  if (!is.null(ds$results$spike_count_pmf)) write_csv_safe(ds$results$spike_count_pmf, file.path(out_dir, "Spike_count_PMF.csv"))
  readme <- if (exists("stpd_method_readme", mode = "function")) stpd_method_readme() else c("SpikeTrainPatternDetector result package")
  writeLines(readme, con = file.path(out_dir, "README_results.txt"), useBytes = TRUE)
  invisible(out_dir)
}
