# ============================================================
# Version-neutral internal API layer
# ------------------------------------------------------------
# Purpose:
#   New detector code should call these functions and read these result fields.
#   Compatibility behavior is routed through semantic implementation functions
#   while detector internals continue moving toward the stable API.
# ============================================================

stpd_threshold_patterns <- function() {
  stpd_threshold_pattern_names_impl()
}

stpd_threshold_pattern_label <- function(pattern) {
  stpd_threshold_pattern_label_impl(pattern)
}

stpd_threshold_pattern_color <- function(pattern, source = "manual") {
  stpd_threshold_pattern_color_impl(pattern, source = source)
}

stpd_resolve_thresholds_for_dataset <- function(trains, params, min_isi_sec = 0.001, bin_width_sec = 0.005) {
  params <- stpd_productize_params(params, prefer = "canonical")
  stpd_resolve_thresholds_for_dataset_impl(trains, params, min_isi_sec = min_isi_sec, bin_width_sec = bin_width_sec)
}

stpd_attach_thresholds_to_params <- function(params, ds = NULL, min_isi_sec = NULL, bin_width_sec = NULL) {
  params <- stpd_productize_params(params, prefer = "canonical")
  stpd_attach_thresholds_to_params_impl(params, ds = ds, min_isi_sec = min_isi_sec, bin_width_sec = bin_width_sec)
}

stpd_event_grammar_params <- function(dat, params, min_isi_sec = 0.001, train = "") {
  params <- stpd_productize_params(params, prefer = "canonical")
  stpd_event_grammar_params_impl(dat, params, min_isi_sec = min_isi_sec, train = train)
}

stpd_event_core_params <- function(dat, params, min_isi_sec = 0.001) {
  params <- stpd_productize_params(params, prefer = "canonical")
  stpd_event_core_params_impl(dat, params, min_isi_sec = min_isi_sec)
}

stpd_train_pipeline_registry <- function() {
  list(
    near_miss_augmented = stpd_detect_train_near_miss_augmented,
    arbitrated = stpd_detect_train_arbitrated,
    seed_bridge_classicity = stpd_detect_train_seed_bridge_classicity,
    event_grammar_core = stpd_detect_train_event_grammar_core,
    threshold_resolved = stpd_detect_train_threshold_resolved,
    hf_protected = stpd_detect_train_hf_protected
  )
}

stpd_train_pipeline_default <- function(params = default_params_sec()) {
  pp <- effective_params_for_detector(params)
  requested <- as.character(
    (pp$detector %||% list())$train_pipeline %||%
      ((pp$spiketrainpattern %||% list())$engine %||% list())$train_pipeline %||%
      "hf_protected"
  )[1]
  registry <- stpd_train_pipeline_registry()
  if (!nzchar(requested) || !(requested %in% names(registry))) "hf_protected" else requested
}

stpd_detect_train_dispatch <- function(dat, params, min_isi_sec = 0.001, train = "", lock_manual = TRUE,
                                       pipeline = NULL,
                                       candidate_lineage_collector = NULL) {
  # This exported dispatch boundary must not silently execute one legacy route
  # when a complete but still-dormant threshold-first run contract is present.
  stpd_candidate_lineage_resolve_audit_level(NULL, params)
  params <- effective_params_for_detector(params)
  registry <- stpd_train_pipeline_registry()
  pipeline <- as.character(pipeline %||% stpd_train_pipeline_default(params))[1]
  if (!nzchar(pipeline) || !(pipeline %in% names(registry))) {
    stop("Unknown train detector pipeline: ", pipeline, call. = FALSE)
  }
  stpd_candidate_lineage_collector_note_pipeline(
    candidate_lineage_collector, pipeline, train = train
  )
  if (!is.null(candidate_lineage_collector) &&
      identical(pipeline, "hf_protected")) {
    return(registry[[pipeline]](
      dat, params, min_isi_sec = min_isi_sec, train = train,
      lock_manual = lock_manual,
      candidate_lineage_collector = candidate_lineage_collector
    ))
  }
  registry[[pipeline]](
    dat, params, min_isi_sec = min_isi_sec, train = train,
    lock_manual = lock_manual
  )
}

run_detector_one_train <- function(dat, params, min_isi_sec = 0.001, train = "", lock_manual = TRUE,
                                   candidate_lineage_collector = NULL) {
  # The full threshold-first run config remains dormant. In particular, a
  # single-train call must not silently consume only its audit field while
  # ignoring engine, threshold-source, and manual-policy semantics.
  stpd_candidate_lineage_resolve_audit_level(NULL, params)
  stpd_detect_train_dispatch(
    dat, params, min_isi_sec = min_isi_sec, train = train,
    lock_manual = lock_manual,
    candidate_lineage_collector = candidate_lineage_collector
  )
}

stpd_detect_train_event_grammar <- function(dat, params, min_isi_sec = 0.001, train = "", lock_manual = TRUE) {
  stpd_candidate_lineage_resolve_audit_level(NULL, params)
  params <- stpd_productize_params(params, prefer = "canonical")
  stpd_detect_train_hf_protected(dat, params, min_isi_sec = min_isi_sec, train = train, lock_manual = lock_manual)
}

stpd_detect_train_core <- function(dat, params, min_isi_sec = 0.001, train = "", lock_manual = TRUE) {
  params <- stpd_productize_params(params, prefer = "canonical")
  run_detector_one_train(dat, params, min_isi_sec = min_isi_sec, train = train, lock_manual = lock_manual)
}

stpd_detect_dataset_core <- function(ds, params, selected_trains = NULL, lock_manual = TRUE, collect_diagnostics = TRUE,
                                     progress_callback = NULL, label_blind = FALSE,
                                     audit_level = NULL) {
  # Keep the exported dataset-core entry point on the same provenance and
  # label-isolation path as stpd_detect(); it is an API alias, not a bypass.
  args <- list(
    ds, params = params, selected_trains = selected_trains,
    lock_manual = lock_manual, collect_diagnostics = collect_diagnostics,
    progress_callback = progress_callback, label_blind = label_blind
  )
  if (!is.null(audit_level)) args$audit_level <- audit_level
  do.call(stpd_detect, args)
}

stpd_empty_candidate_ledger <- function() {
  stpd_event_grammar_empty_candidate_ledger()
}

stpd_empty_candidate_features <- function() {
  stpd_event_grammar_empty_candidate_features()
}

stpd_empty_final_decisions <- function(features = NULL) {
  stpd_event_grammar_empty_final_decisions(features)
}

stpd_review_reject_labels <- function() {
  stpd_event_grammar_review_reject_labels()
}

stpd_is_reject_like <- function(x) {
  stpd_event_grammar_is_reject_like(x)
}

stpd_candidate_audit_to_ledger <- function(ds, params, selected_trains = NULL, run_id = NULL, params_hash = NULL) {
  params <- stpd_productize_params(params, prefer = "canonical")
  stpd_candidate_diagnostic_audit_to_ledger(ds, params, selected_trains = selected_trains,
                                            run_id = run_id, params_hash = params_hash)
}

stpd_public_params_only <- function(params) {
  pp <- stpd_productize_params(params, prefer = "canonical")
  list(
    spiketrainpattern = pp$spiketrainpattern,
    metadata = list(
      parameter_schema = "spiketrainpattern",
      params_hash = (pp$meta %||% list())$params_hash %||% NA_character_
    )
  )
}

stpd_canonicalize_result_names <- function(ds, drop_legacy = TRUE) {
  if (is.null(ds) || is.null(ds$results)) return(ds)
  res <- ds$results
  res$run_metadata_public <- res$run_metadata_public %||% res$run_metadata
  res$result_consistency <- res$result_consistency %||% res$consistency_audit
  if (isTRUE(drop_legacy)) {
    versioned <- grepl("(^v[0-9]|_v[0-9]|v[0-9].*_|parameters_report$)", names(res), ignore.case = TRUE)
    res[names(res)[versioned]] <- NULL
  }
  ds$results <- res
  if (!is.null(ds$params_last)) {
    # Keep an immutable, full effective-parameter payload for hash verification
    # before presenting the reduced canonical public view in `params_last`.
    if (is.null(ds$params_effective)) ds$params_effective <- ds$params_last
    ds$params_last <- stpd_public_params_only(ds$params_last)
  }
  if (!is.null(ds$trains) && length(ds$trains) > 0) {
    for (nm in names(ds$trains)) {
      tr <- ds$trains[[nm]]
      if (isTRUE(drop_legacy)) {
        attrs <- names(attributes(tr))
        for (legacy_attr in attrs[grepl("^v[0-9]", attrs, ignore.case = TRUE)]) {
          attr(tr, legacy_attr) <- NULL
        }
      }
      ds$trains[[nm]] <- tr
    }
  }
  ds
}

if (exists("run_detector_dataset_internal", mode = "function") && !exists("run_detector_dataset_internal_productized", mode = "function")) {
  run_detector_dataset_internal_productized <- run_detector_dataset_internal
  run_detector_dataset_internal <- function(ds, params, selected_trains = NULL, lock_manual = TRUE, collect_diagnostics = TRUE,
                                            progress_callback = NULL,
                                            audit_level = NULL,
                                            candidate_lineage_collector = NULL) {
    if (is.list(ds) && !is.data.frame(ds) && !is.null(ds$trains)) {
      ds <- stpd_candidate_lineage_strip(ds)
    }
    if (is.null(candidate_lineage_collector)) {
      resolved_audit_level <- stpd_candidate_lineage_resolve_audit_level(
        audit_level, params
      )
    } else {
      stpd_candidate_lineage_validate_collector(
        candidate_lineage_collector, require_state = "open"
      )
      resolved_audit_level <- stpd_candidate_lineage_resolve_audit_level(
        candidate_lineage_collector$requested_audit_level, params
      )
      if (!is.null(audit_level)) {
        explicit_level <- stpd_candidate_lineage_resolve_audit_level(
          audit_level, params
        )
        if (!identical(explicit_level, resolved_audit_level)) {
          stpd_candidate_lineage_abort(
            "collector_audit_level_mismatch",
            "The supplied collector and audit_level request do not match."
          )
        }
      }
    }
    owns_collector <- is.null(candidate_lineage_collector) &&
      !identical(resolved_audit_level, "off")
    if (owns_collector) {
      candidate_lineage_collector <- stpd_candidate_lineage_collector_new(
        resolved_audit_level
      )
    }
    completed <- FALSE
    if (owns_collector) {
      on.exit({
        if (!completed) {
          stpd_candidate_lineage_collector_abort(candidate_lineage_collector)
        }
      }, add = TRUE)
    }
    args <- list(
      ds, params = params, selected_trains = selected_trains,
      lock_manual = lock_manual,
      collect_diagnostics = collect_diagnostics,
      progress_callback = progress_callback
    )
    if (!identical(resolved_audit_level, "off")) {
      args$audit_level <- resolved_audit_level
    }
    if (!is.null(candidate_lineage_collector)) {
      args$candidate_lineage_collector <- candidate_lineage_collector
    }
    out <- do.call(run_detector_dataset_internal_productized, args)
    out <- stpd_canonicalize_result_names(out)
    if (owns_collector) {
      out <- stpd_candidate_lineage_collector_finalize_unavailable(
        out, candidate_lineage_collector
      )
      completed <- TRUE
    }
    out
  }
}

if (exists("stpd_detect", mode = "function") && !exists("stpd_detect_productized", mode = "function")) {
  stpd_detect_productized <- stpd_detect
  stpd_detect <- function(ds, params = default_params_sec(), selected_trains = NULL,
                          lock_manual = TRUE, collect_diagnostics = TRUE,
                          strict_params = FALSE, progress_callback = NULL,
                          label_blind = FALSE, audit_level = NULL) {
    args <- list(
      ds, params = params, selected_trains = selected_trains,
      lock_manual = lock_manual,
      collect_diagnostics = collect_diagnostics,
      strict_params = strict_params,
      progress_callback = progress_callback,
      label_blind = label_blind
    )
    if (!is.null(audit_level)) args$audit_level <- audit_level
    out <- do.call(stpd_detect_productized, args)
    stpd_canonicalize_result_names(out)
  }
}
