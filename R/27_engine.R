# Canonical detector engine.
# This is a clean pipeline wrapper around the reference detector. The goal is
# not to change detector semantics, but to make QC, candidate generation,
# feature extraction, final classification, event audit and export consistently
# derived from one run object.

stpd_engine_prepare_params <- function(params = default_params_sec(), strict = FALSE) {
  params <- apply_schema_defaults(params)
  issues <- stpd_validate_params(params, strict = strict)
  relation_issues <- stpd_validate_param_relations(params)
  relation_errors <- relation_issues[relation_issues$severity == "error", , drop = FALSE]
  # Threshold freezing may replace the seed/bridge values with empirical bands
  # whose ordering is intentionally resolved from the data.  Validate the
  # user-supplied configuration before freezing, but do not reject a valid
  # frozen run merely because the resolved band is narrower than its prior.
  has_resolved_thresholds <- is.list(params$event_grammar) &&
    (!is.null(params$event_grammar$threshold_table) ||
       !is.null(params$event_grammar$effective_bands))
  if (nrow(relation_errors) > 0L) {
    relation_errors <- relation_errors[
      !relation_errors$issue_code %in% c(
        "pause_max_above_bridge",
        if (has_resolved_thresholds) "burst_seed_upper_above_bridge" else "__never__"
      ),
      , drop = FALSE
    ]
  }
  if (nrow(relation_errors) > 0L) {
    stop(
      paste0(
        "Cross-parameter validation failed: ",
        paste(
          paste0(
            relation_errors$path, " [", relation_errors$issue_code, "] ",
            relation_errors$issue
          ),
          collapse = "; "
        )
      ),
      call. = FALSE
    )
  }
  params$meta <- params$meta %||% list()
  params$meta$params_hash <- stpd_params_hash_flat(params)
  attr(params, "validation_issues") <- issues
  params
}

stpd_label_blind_is_manual_derived <- function(x) {
  if (is.null(x) || !is.list(x)) return(FALSE)

  nms <- names(x) %||% character()
  provenance_fields <- grepl("source|method|provenance|derived", nms, ignore.case = TRUE)
  provenance <- if (any(provenance_fields)) {
    unlist(x[provenance_fields], recursive = TRUE, use.names = FALSE)
  } else {
    character()
  }
  provenance <- as.character(provenance)
  provenance[is.na(provenance)] <- ""

  nested_manual <- any(vapply(
    x,
    function(value) is.list(value) && stpd_label_blind_is_manual_derived(value),
    logical(1)
  ))

  any(grepl("manual", provenance, ignore.case = TRUE)) ||
    any(grepl("^n_manual", nms, ignore.case = TRUE)) ||
    nested_manual
}

stpd_label_blind_filter_train_settings <- function(settings) {
  if (is.null(settings) || !is.list(settings) || length(settings) == 0) return(settings)
  manual_derived <- vapply(settings, stpd_label_blind_is_manual_derived, logical(1))
  settings[!manual_derived]
}

stpd_label_blind_strip_legacy_adjudication <- function(ds) {
  # A label-blind detector copy must not retain any post-detection review
  # projection.  Several legacy review helpers keep immutable automatic labels
  # alongside derived overrides/audits; those derived fields are useful for a
  # reviewer, but they are not valid detector input or automatic-scoring input.
  review_exact <- c(
    "pattern_auto_original",
    "pattern_manual_before_user_override",
    "pattern_manual_negative_before_user_override"
  )
  review_prefix <- c("pattern_user_override", "pattern_audit_")

  if (is.list(ds$trains)) {
    ds$trains <- lapply(ds$trains, function(dat) {
      if (!is.data.frame(dat)) return(dat)
      drop <- names(dat) %in% review_exact
      for (prefix in review_prefix) {
        drop <- drop | startsWith(names(dat), prefix)
      }
      if (any(drop)) dat <- dat[, !drop, drop = FALSE]
      dat
    })
  }

  legacy_result_names <- c(
    "final_audit_summary", "final_audit_events", "final_audit_history",
    "final_audit_event_history", "final_audit_policy",
    "possible_burst_promotion_audit", "possible_burst_promotion_summary"
  )
  if (is.list(ds$results)) {
    ds$results[intersect(legacy_result_names, names(ds$results))] <- NULL
  }
  ds
}

stpd_label_blind_dataset_copy <- function(ds) {
  blinded <- if (exists("stpd_multitrack_auto_strip", mode = "function")) {
    stpd_multitrack_auto_strip(ds)
  } else {
    ds
  }
  blinded <- stpd_label_blind_strip_legacy_adjudication(blinded)
  # Phase 2B review history and derived final products are a separate,
  # authoritative post-detection layer.  They are intentionally absent from a
  # label-blind result, not restored after automatic detection.
  if (exists("stpd_multitrack_review_strip_label_blind", mode = "function")) {
    blinded <- stpd_multitrack_review_strip_label_blind(blinded)
  }
  blinded$trains <- lapply(blinded$trains, function(dat) {
    if (!is.data.frame(dat)) return(dat)
    dat$pattern_manual <- rep("", nrow(dat))
    dat$pattern_manual_negative <- rep("", nrow(dat))
    dat
  })

  # Preserve explicit UI/user thresholds, but remove train-specific ranges whose
  # provenance explicitly says they were learned from manual annotations.
  if (is.list(blinded$train_settings)) {
    setting_names <- c(
      "burst_isi_ranges", "tonic_isi_ranges", "pause_isi_ranges",
      "highfreq_isi_ranges", "isi_thresholds"
    )
    for (nm in intersect(setting_names, names(blinded$train_settings))) {
      blinded$train_settings[[nm]] <-
        stpd_label_blind_filter_train_settings(blinded$train_settings[[nm]])
    }
  }
  blinded
}

stpd_label_blind_restore_manual_columns <- function(out, original_ds) {
  if (is.null(out$trains) || is.null(original_ds$trains)) return(out)
  for (train in intersect(names(out$trains), names(original_ds$trains))) {
    original <- original_ds$trains[[train]]
    detected <- out$trains[[train]]
    if (!is.data.frame(original) || !is.data.frame(detected)) next
    for (column in c("pattern_manual", "pattern_manual_negative")) {
      if (column %in% names(original)) {
        detected[[column]] <- original[[column]]
      } else if (column %in% names(detected)) {
        detected[[column]] <- NULL
      }
    }
    out$trains[[train]] <- detected
  }
  out
}

stpd_label_blind_prepare_params <- function(params) {
  blinded <- params

  # A params object can be reused after a previous manually calibrated run.
  # Invalidate cached threshold resolution whenever it contains manual evidence;
  # the detector will rebuild it from the blinded dataset before detection.
  eg <- blinded$event_grammar %||% list()
  threshold_table <- eg$threshold_table %||% NULL
  manual_resolution <- FALSE
  if (is.data.frame(threshold_table)) {
    if ("source" %in% names(threshold_table)) {
      src <- as.character(threshold_table$source)
      src[is.na(src)] <- ""
      manual_resolution <- any(grepl("manual", src, ignore.case = TRUE))
    }
    if ("manual_sec" %in% names(threshold_table)) {
      manual_resolution <- manual_resolution ||
        any(is.finite(suppressWarnings(as.numeric(threshold_table$manual_sec))))
    }
  }
  manual_resolution <- manual_resolution ||
    stpd_label_blind_is_manual_derived(eg$effective_bands %||% list()) ||
    (is.data.frame(eg$manual_event_table) && nrow(eg$manual_event_table) > 0)

  eg$manual_event_table <- NULL
  eg$manual_suggest <- NULL
  if (isTRUE(manual_resolution)) {
    eg$threshold_table <- NULL
    eg$effective_bands <- NULL
  }
  blinded$event_grammar <- eg

  # Remove only ranges with explicit manual provenance. Frozen thresholds from a
  # separate calibration set and explicit user thresholds remain valid inputs.
  for (family in intersect(c("burst", "tonic", "pause", "highfreq"), names(blinded))) {
    if (is.list(blinded[[family]]) && "adaptive_train_ranges" %in% names(blinded[[family]])) {
      blinded[[family]]$adaptive_train_ranges <-
        stpd_label_blind_filter_train_settings(blinded[[family]]$adaptive_train_ranges)
    }
  }
  if (is.list(blinded$detector) && "train_isi_thresholds" %in% names(blinded$detector)) {
    blinded$detector$train_isi_thresholds <-
      stpd_label_blind_filter_train_settings(blinded$detector$train_isi_thresholds)
  }
  blinded
}

stpd_runtime_package_version <- function() {
  tryCatch(
    as.character(utils::packageVersion("SpikeTrainPatternDetector")),
    error = function(e) NA_character_
  )
}

stpd_runtime_code_revision_info <- function() {
  revision <- Sys.getenv("STPD_GIT_COMMIT", unset = "")
  if (nzchar(revision)) {
    return(list(revision = revision, source = "STPD_GIT_COMMIT", dirty = NA))
  }
  if (!nzchar(revision)) {
    description <- tryCatch(
      utils::packageDescription("SpikeTrainPatternDetector"),
      error = function(e) NULL
    )
    if (!is.null(description)) {
      revision <- as.character(
        description[["GithubSHA1"]] %||%
          description[["RemoteSha"]] %||%
          ""
      )[1]
    }
  }
  if (!is.na(revision) && nzchar(revision)) {
    return(list(revision = revision, source = "package_description_remote_sha", dirty = FALSE))
  }

  source_dir <- Sys.getenv("STPD_SOURCE_DIR", unset = "")
  candidates <- unique(c(source_dir[nzchar(source_dir)], getwd()))
  for (candidate in candidates) {
    sha <- tryCatch(
      suppressWarnings(system2(
        "git", c("-C", shQuote(candidate), "rev-parse", "HEAD"),
        stdout = TRUE, stderr = FALSE
      )),
      error = function(e) character()
    )
    sha <- as.character(sha)[1]
    if (length(sha) == 0 || is.na(sha) || !grepl("^[0-9a-fA-F]{40}$", sha)) next
    status <- tryCatch(
      suppressWarnings(system2(
        "git", c("-C", shQuote(candidate), "status", "--porcelain", "--untracked-files=normal"),
        stdout = TRUE, stderr = FALSE
      )),
      error = function(e) character()
    )
    return(list(
      revision = sha,
      source = if (nzchar(source_dir) && identical(candidate, source_dir)) "STPD_SOURCE_DIR_git" else "working_directory_git",
      dirty = length(status) > 0L
    ))
  }
  list(revision = NA_character_, source = "unavailable", dirty = NA)
}

stpd_runtime_code_revision <- function() {
  stpd_runtime_code_revision_info()$revision
}

stpd_runtime_dependency_versions <- function() {
  packages <- c(
    "shiny", "plotly", "dplyr", "tidyr", "purrr", "DT",
    "digest", "tibble", "yaml"
  )
  versions <- vapply(packages, function(package) {
    tryCatch(as.character(utils::packageVersion(package)), error = function(e) NA_character_)
  }, character(1))
  paste0(packages, "=", versions, collapse = ";")
}

stpd_runtime_threshold_sources <- function(out) {
  threshold_table <- (out$results %||% list())$threshold_table %||% data.frame()
  if (!is.data.frame(threshold_table) || nrow(threshold_table) == 0) return(NA_character_)
  source_columns <- intersect(
    c("source", "selected_source", "resolved_source", "final_source"),
    names(threshold_table)
  )
  if (length(source_columns) == 0) return(NA_character_)
  sources <- unique(unlist(threshold_table[source_columns], use.names = FALSE))
  sources <- sort(as.character(sources[!is.na(sources) & nzchar(as.character(sources))]))
  if (length(sources) == 0) NA_character_ else paste(sources, collapse = ";")
}

stpd_input_manual_interval_counts <- function(ds) {
  count_column <- function(column) {
    sum(vapply(ds$trains %||% list(), function(dat) {
      if (!is.data.frame(dat) || !(column %in% names(dat))) return(0L)
      value <- as.character(dat[[column]])
      value[is.na(value)] <- ""
      sum(nzchar(value))
    }, integer(1)))
  }
  c(
    manual_positive_interval_n = count_column("pattern_manual"),
    manual_negative_interval_n = count_column("pattern_manual_negative")
  )
}

stpd_new_run_id <- local({
  last_stamp <- ""
  occurrence <- 0L
  function(at = Sys.time()) {
    stamp <- format(at, "%Y%m%d_%H%M%OS6", tz = "UTC")
    stamp <- gsub("[^0-9_]", "", stamp)
    if (identical(stamp, last_stamp)) {
      occurrence <<- occurrence + 1L
    } else {
      last_stamp <<- stamp
      occurrence <<- 1L
    }
    paste0(
      "stpd_run_", stamp,
      "_p", Sys.getpid(),
      "_n", sprintf("%02d", occurrence)
    )
  }
})

stpd_runtime_structure_first_execution <- function(params, train_pipeline = NULL) {
  train_pipeline <- as.character(
    train_pipeline %||%
      tryCatch(stpd_train_pipeline_default(params), error = function(e) "unknown")
  )[1]
  if (is.na(train_pipeline) || !nzchar(train_pipeline)) train_pipeline <- "unknown"

  configured <- isTRUE(
    (((params$spiketrainpattern %||% list())$burst %||% list())$structure_first_enabled %||% TRUE)
  )
  patterns <- as.character(
    (params$detector %||% list())$patterns_to_run %||% stpd_default_patterns_to_run()
  )
  patterns <- patterns[!is.na(patterns) & nzchar(patterns)]
  burst_family_requested <- any(c("burst", "long_burst") %in% patterns)

  event_grammar_enabled <- isTRUE((params$event_grammar %||% list())$enabled %||% TRUE)
  event_core_enabled <- tryCatch(
    stpd_event_core_is_enabled(params),
    error = function(e) isTRUE((params$event_core %||% list())$enabled %||% TRUE)
  )
  structure_capable_route <- switch(
    train_pipeline,
    event_grammar_core = isTRUE(event_core_enabled),
    threshold_resolved = isTRUE(event_grammar_enabled) || isTRUE(event_core_enabled),
    hf_protected = isTRUE(event_grammar_enabled) || isTRUE(event_core_enabled),
    FALSE
  )
  executed <- configured && burst_family_requested && structure_capable_route

  if (executed) {
    initial_burst_screen <- "structure_first_local_flank_separation"
    stage_order <- "structure_first;threshold_centred_fallback;explicit_hard_override;state_arbitration"
  } else if (!burst_family_requested) {
    initial_burst_screen <- "not_run_burst_family_disabled"
    stage_order <- "burst_family_not_requested;state_arbitration"
  } else if (structure_capable_route) {
    initial_burst_screen <- "seed_band_threshold_centred"
    stage_order <- "threshold_centred;explicit_hard_override;state_arbitration"
  } else if (train_pipeline %in% c("event_grammar_core", "threshold_resolved", "hf_protected")) {
    initial_burst_screen <- "fallback_seed_bridge_classicity"
    stage_order <- "seed_bridge_classicity_fallback;state_arbitration"
  } else {
    initial_burst_screen <- paste0("pipeline_specific__", train_pipeline)
    stage_order <- paste0("pipeline_specific__", train_pipeline)
  }

  list(
    configured = configured,
    executed = executed,
    burst_family_requested = burst_family_requested,
    structure_capable_route = structure_capable_route,
    initial_burst_screen = initial_burst_screen,
    stage_order = stage_order
  )
}

stpd_detect <- function(ds, params = default_params_sec(), selected_trains = NULL,
                        lock_manual = TRUE, collect_diagnostics = TRUE,
                        strict_params = FALSE, progress_callback = NULL,
                        label_blind = FALSE, audit_level = NULL) {
  if (is.null(ds) || is.null(ds$trains)) stop("stpd_detect(): ds must be a dataset with a trains list.", call. = FALSE)
  resolved_audit_level <- stpd_candidate_lineage_resolve_audit_level(
    audit_level, params
  )
  candidate_lineage_collector <- if (
    identical(resolved_audit_level, "off")
  ) NULL else stpd_candidate_lineage_collector_new(resolved_audit_level)
  lineage_complete <- FALSE
  if (!is.null(candidate_lineage_collector)) {
    on.exit({
      if (!lineage_complete) {
        stpd_candidate_lineage_collector_abort(
          candidate_lineage_collector
        )
      }
    }, add = TRUE)
  }
  ds <- stpd_candidate_lineage_strip(ds)
  label_blind <- isTRUE(label_blind)
  detector_ds <- if (exists("stpd_multitrack_auto_strip", mode = "function")) {
    stpd_multitrack_auto_strip(ds)
  } else {
    ds
  }
  detector_ds <- stpd_multitrack_final_strip(detector_ds)
  detector_ds <- stpd_multitrack_gate_b_strip(detector_ds)
  if (exists("stpd_event_regime_strip", mode = "function")) {
    detector_ds <- stpd_event_regime_strip(detector_ds)
  }
  review_archive <- NULL
  detector_params <- params
  effective_lock_manual <- lock_manual
  if (label_blind) {
    detector_ds <- stpd_label_blind_dataset_copy(detector_ds)
    detector_params <- stpd_label_blind_prepare_params(params)
    effective_lock_manual <- FALSE
  } else if (exists("stpd_multitrack_review_detach_for_rerun", mode = "function")) {
    # Keep the entire post-detection adjudication namespace outside every
    # automatic stage, including the public ledgers, evidence summaries, and
    # validation reports computed after the internal detector returns. The
    # immutable history is restored only as a non-authoritative stale archive
    # immediately before returning the fully computed result.
    detached_review <- stpd_multitrack_review_detach_for_rerun(detector_ds)
    detector_ds <- detached_review$dataset
    review_archive <- detached_review$archive
  }
  params <- stpd_engine_prepare_params(detector_params, strict = strict_params)
  run_started_at <- Sys.time()
  run_id <- stpd_new_run_id(run_started_at)
  params$meta <- params$meta %||% list()
  params$meta$run_id <- run_id
  params$meta$label_blind <- label_blind
  detector_args <- list(
    detector_ds,
    params = params,
    selected_trains = selected_trains,
    lock_manual = effective_lock_manual,
    collect_diagnostics = collect_diagnostics,
    progress_callback = progress_callback
  )
  if (!identical(resolved_audit_level, "off")) {
    detector_args$audit_level <- resolved_audit_level
  }
  if (!is.null(candidate_lineage_collector)) {
    detector_args$candidate_lineage_collector <-
      candidate_lineage_collector
  }
  out <- do.call(run_detector_dataset_internal, detector_args)
  effective_hash <- as.character(
    (out$results$run_metadata %||% data.frame())$params_hash %||%
      (params$meta %||% list())$params_hash %||% ""
  )[1]
  if (!is.na(effective_hash) && nzchar(effective_hash)) {
    params$meta$params_hash <- effective_hash
  }
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
  distributional_refresh_needed <- isTRUE(cand_rebuilt) ||
    isTRUE(feature_missing) ||
    !stpd_distributional_results_complete(out)
  if (distributional_refresh_needed) {
    stpd_call_progress(progress_callback, "distributional_evidence", detail = "Computing distributional evidence and firing phenotype summaries")
    out <- stpd_add_distributional_results(
      out,
      params = params,
      selected_trains = selected_trains,
      candidates = feats
    )
  }
  out$results$parameter_validation <- attr(params, "validation_issues")
  out$results$parameter_report <- stpd_parameter_report_flat_with_policy_hash(params)
  out$results$label_blind <- label_blind
  out$results$detection_mode <- if (label_blind) "label_blind" else "manual_aware"
  target_trains <- intersect(
    as.character(selected_trains %||% names(out$trains)),
    names(out$trains)
  )
  manual_counts <- stpd_input_manual_interval_counts(ds)
  sys_info <- Sys.info()
  revision_info <- stpd_runtime_code_revision_info()
  threshold_mode <- as.character(
    ((params$spiketrainpattern %||% list())$engine %||% list())$threshold_source_mode %||%
      (params$event_grammar %||% list())$threshold_source_mode %||%
      "auto"
  )[1]
  train_pipeline <- tryCatch(
    stpd_train_pipeline_default(params),
    error = function(e) as.character((params$detector %||% list())$train_pipeline %||% "unknown")[1]
  )
  burst_detector_pipeline <- tryCatch(
    stpd_event_grammar_burst_detector_default(params),
    error = function(e) as.character((params$event_grammar %||% list())$burst_detector_pipeline %||% "unknown")[1]
  )
  structure_first <- stpd_runtime_structure_first_execution(
    params, train_pipeline = train_pipeline
  )
  multitrack_policy <- tryCatch(
    stpd_multitrack_policy_block(params),
    error = function(e) list()
  )
  multitrack_policy_hash <- tryCatch(
    stpd_multitrack_policy_hash(params),
    error = function(e) NA_character_
  )
  multitrack_auto_metadata <- tryCatch(
    stpd_multitrack_auto(out)$metadata,
    error = function(e) data.frame()
  )
  if (!is.data.frame(multitrack_auto_metadata) ||
      nrow(multitrack_auto_metadata) != 1L) {
    stop(
      "A successful detector run must contain one valid results$multitrack_auto product.",
      call. = FALSE
    )
  }
  out$results$run_metadata_public <- data.frame(
    run_id = run_id,
    run_started_at_utc = format(run_started_at, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    params_hash = params$meta$params_hash,
    params_hash_algorithm = "SHA-256",
    package_version = stpd_runtime_package_version(),
    code_revision = as.character(revision_info$revision %||% NA_character_),
    code_revision_source = as.character(revision_info$source %||% "unavailable"),
    code_revision_dirty = as.logical(revision_info$dirty %||% NA),
    r_version = R.version.string,
    platform = R.version$platform,
    operating_system = paste(
      as.character(sys_info[["sysname"]] %||% ""),
      as.character(sys_info[["release"]] %||% "")
    ),
    dependency_versions = stpd_runtime_dependency_versions(),
    dataset_name = as.character((ds$meta %||% list())$display_name %||% "dataset"),
    dataset_source = as.character((ds$meta %||% list())$source %||% "unknown"),
    input_file_name = as.character((ds$meta %||% list())$input_file_name %||% NA_character_),
    input_size_bytes = suppressWarnings(as.numeric((ds$meta %||% list())$input_size_bytes %||% NA_real_)),
    input_sha256 = as.character((ds$meta %||% list())$input_sha256 %||% NA_character_),
    input_parser_mode = as.character((ds$meta %||% list())$input_parser_mode %||% NA_character_),
    input_unit = as.character((ds$meta %||% list())$input_unit %||% (ds$meta %||% list())$unit_in %||% NA_character_),
    input_header = as.logical((ds$meta %||% list())$input_header %||% NA),
    input_duplicate_policy = as.character((ds$meta %||% list())$input_duplicate_policy %||% NA_character_),
    total_train_n = length(out$trains),
    selected_train_n = length(target_trains),
    selected_trains = paste(target_trains, collapse = ";"),
    detection_mode = out$results$detection_mode,
    label_blind = label_blind,
    lock_manual_requested = isTRUE(lock_manual),
    lock_manual_effective = isTRUE(effective_lock_manual),
    manual_positive_interval_n_at_input = unname(manual_counts[["manual_positive_interval_n"]]),
    manual_negative_interval_n_at_input = unname(manual_counts[["manual_negative_interval_n"]]),
    threshold_source_mode = threshold_mode,
    threshold_sources_resolved = stpd_runtime_threshold_sources(out),
    dataset_thresholds_frozen = isTRUE((params$detector %||% list())$freeze_dataset_thresholds),
    train_pipeline = train_pipeline,
    burst_detector_pipeline = burst_detector_pipeline,
    initial_burst_screen = structure_first$initial_burst_screen,
    burst_candidate_stage_order = structure_first$stage_order,
    structure_first_configured = structure_first$configured,
    structure_first_executed = structure_first$executed,
    # Backward-compatible field now carries effective run semantics. The
    # configured parameter value is available separately above.
    structure_first_enabled = structure_first$executed,
    burst_family_requested = structure_first$burst_family_requested,
    multitrack_shadow_authoritative = FALSE,
    multitrack_auto_schema_version = as.character(
      multitrack_auto_metadata$schema_version[1]
    ),
    multitrack_auto_authoritative = as.logical(
      multitrack_auto_metadata$authoritative[1]
    ),
    multitrack_auto_authority_scope = as.character(
      multitrack_auto_metadata$authority_scope[1]
    ),
    multitrack_auto_intended_target_scope = as.character(
      multitrack_auto_metadata$intended_target_scope[1]
    ),
    multitrack_auto_promotion_gate = as.character(
      multitrack_auto_metadata$promotion_gate[1]
    ),
    multitrack_auto_promotion_status = as.character(
      multitrack_auto_metadata$promotion_status[1]
    ),
    multitrack_auto_canonical_candidate_schema = as.logical(
      multitrack_auto_metadata$canonical_candidate_schema[1]
    ),
    multitrack_auto_materialization_status = as.character(
      multitrack_auto_metadata$materialization_status[1]
    ),
    multitrack_auto_failure_code = as.character(
      multitrack_auto_metadata$failure_code[1]
    ),
    multitrack_auto_source_multitrack_evidence_available = as.logical(
      multitrack_auto_metadata$source_multitrack_evidence_available[1]
    ),
    multitrack_auto_per_isi_materialization = as.character(
      multitrack_auto_metadata$per_isi_materialization[1]
    ),
    multitrack_auto_detector_performance_eligible = as.logical(
      multitrack_auto_metadata$detector_performance_eligible[1]
    ),
    multitrack_auto_detector_performance_block_reason = as.character(
      multitrack_auto_metadata$detector_performance_block_reason[1]
    ),
    multitrack_auto_biological_ground_truth = as.logical(
      multitrack_auto_metadata$biological_ground_truth[1]
    ),
    multitrack_auto_legacy_single_track_parallel = as.logical(
      multitrack_auto_metadata$legacy_single_track_parallel[1]
    ),
    multitrack_auto_product_sha256 = as.character(
      multitrack_auto_metadata$product_sha256[1]
    ),
    multitrack_phase1a_policy_version = as.character(
      multitrack_policy$phase1a_policy_version %||% NA_character_
    ),
    multitrack_track_ontology_version = as.character(
      multitrack_policy$phase1a_track_ontology_version %||% NA_character_
    ),
    multitrack_phase1b_policy_version = as.character(
      multitrack_policy$phase1b_policy_version %||% NA_character_
    ),
    multitrack_policy_hash = as.character(multitrack_policy_hash),
    multitrack_policy_hash_algorithm = "SHA-256",
    multitrack_policy_parameter_path = stpd_multitrack_policy_parameter_path(),
    diagnostics_collected = isTRUE(collect_diagnostics),
    detector_deterministic = TRUE,
    random_seed = NA_character_,
    candidate_count = nrow(out$results$candidate_ledger %||% data.frame()),
    event_count = nrow(out$results$events %||% data.frame()),
    feature_count = nrow(feats),
    stringsAsFactors = FALSE
  )
  # Keep the stable detector metadata schema independent of the opt-in Preview.
  # Preview provenance and table hashes live in the nested Preview metadata and
  # manifest, which are exported as their own artifacts. Mirroring them here
  # would reshape the legacy Detector_run_metadata.csv only when the flag is ON.
  if (label_blind) out <- stpd_label_blind_restore_manual_columns(out, ds)
  stpd_call_progress(progress_callback, "public_reports", detail = "Computing consistency and validation summaries")
  out$results$result_consistency <- tryCatch(stpd_result_consistency_check(out), error = function(e) data.frame(severity = "error", component = "result_consistency", issue = "consistency check failed", detail = conditionMessage(e), stringsAsFactors = FALSE))
  out$results$scientific_validation_summary <- tryCatch(stpd_scientific_validation_summary(out, params), error = function(e) data.frame(item = "scientific_validation_summary", value = paste0("failed: ", conditionMessage(e)), stringsAsFactors = FALSE))
  out$results$event_level_validation_strict <- tryCatch(stpd_event_level_validation(out, params, selected_trains = selected_trains, metric_mode = "strict_high_confidence"), error = function(e) data.frame(split = "all", metric_mode = "strict_high_confidence", pattern = NA_character_, note = paste0("failed: ", conditionMessage(e)), stringsAsFactors = FALSE))
  out$results$event_level_validation_candidate_family <- tryCatch(stpd_event_level_validation(out, params, selected_trains = selected_trains, metric_mode = "candidate_family"), error = function(e) data.frame(split = "all", metric_mode = "candidate_family", pattern = NA_character_, note = paste0("failed: ", conditionMessage(e)), stringsAsFactors = FALSE))
  if (!label_blind &&
      exists("stpd_multitrack_review_restore_archive", mode = "function")) {
    out <- stpd_multitrack_review_restore_archive(out, review_archive)
  }
  out <- stpd_multitrack_gate_b_attach(out)
  if (exists("stpd_event_regime_attach", mode = "function")) {
    out <- stpd_event_regime_attach(out)
  }
  if (!is.null(candidate_lineage_collector)) {
    out <- stpd_candidate_lineage_collector_finalize_unavailable(
      out, candidate_lineage_collector
    )
    lineage_complete <- TRUE
  }
  stpd_call_progress(progress_callback, "public_complete", detail = "Public detector outputs are ready")
  out
}

stpd_generate_candidates <- function(ds, params = default_params_sec(), selected_trains = NULL,
                                     label_blind = FALSE) {
  out <- stpd_detect(
    ds, params, selected_trains = selected_trains,
    collect_diagnostics = TRUE, label_blind = label_blind
  )
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
  if (!is.null(ds$results$threshold_table)) write_csv_safe(ds$results$threshold_table, file.path(out_dir, "Detector_threshold_table.csv"))
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
