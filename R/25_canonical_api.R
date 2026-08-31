# schema additional canonical API layer.
# These names are stable user-facing wrappers around legacy-compatible internals.
# They make detector, QC, candidate generation, feature extraction and export
# callable without referring to historical suffixes.

build_spike_dataset <- function(path, mode = c("raw", "labeled"), unit_in = c("s", "ms"), header = TRUE, name = NULL, duplicate_policy = c("error_keep", "warn_keep", "collapse_exact")) {
  mode <- match.arg(mode); unit_in <- match.arg(unit_in); duplicate_policy <- match.arg(duplicate_policy)
  trains <- if (mode == "raw") build_trains_from_raw(path, header = header, unit_in = unit_in, duplicate_policy = duplicate_policy) else build_trains_from_annot(path, unit_in = unit_in, duplicate_policy = duplicate_policy)
  task_events <- if (mode == "raw") {
    tryCatch(stpd_extract_task_events_from_raw(path, header = header, unit_in = unit_in), error = function(e) stpd_empty_task_events())
  } else {
    tryCatch(stpd_extract_task_events_from_raw(path, header = TRUE, unit_in = unit_in), error = function(e) stpd_empty_task_events())
  }
  make_dataset(name = name %||% tools::file_path_sans_ext(basename(path)), source = mode, trains = trains, unit_in = unit_in, task_events = task_events)
}

run_qc <- function(ds, params = default_params(), min_isi_sec = NULL) {
  if (is.null(ds) || is.null(ds$trains)) return(data.frame())
  p <- apply_schema_defaults(params)
  min_isi <- min_isi_sec %||% p$detector$min_valid_isi_sec %||% 0.0009
  validate_dataset_quality(ds$trains, min_isi_sec = min_isi, unit_hint = ds$meta$unit_in %||% "s", refractory_suspect_sec = p$detector$refractory_suspect_sec %||% 0.0010)
}

run_detector <- function(ds, params = default_params(), selected_trains = NULL, lock_manual = TRUE, collect_diagnostics = TRUE,
                         progress_callback = NULL, label_blind = FALSE,
                         audit_level = NULL) {
  # engine.2: canonical API now goes through the consolidated detector engine.
  args <- list(
    ds, params = params, selected_trains = selected_trains,
    lock_manual = lock_manual, collect_diagnostics = collect_diagnostics,
    progress_callback = progress_callback, label_blind = label_blind
  )
  if (!is.null(audit_level)) args$audit_level <- audit_level
  do.call(stpd_detect, args)
}

export_results <- function(ds, params = default_params(), out_dir, dataset_name = "dataset", time_unit = "ms") {
  # engine.2: export through consolidated exporter so validation/consistency reports are included.
  stpd_export_results(ds, params = params, out_dir = out_dir,
                      dataset_name = dataset_name, time_unit = time_unit)
}

generate_candidates <- function(ds, params = default_params(), selected_trains = NULL, collect_diagnostics = TRUE,
                                label_blind = FALSE) {
  out <- run_detector(
    ds, params = params, selected_trains = selected_trains,
    lock_manual = TRUE, collect_diagnostics = collect_diagnostics,
    label_blind = label_blind
  )
  out$results$candidate_ledger %||% data.frame()
}

compute_candidate_features <- function(ds, candidates = NULL, params = default_params(), selected_trains = NULL) {
  compute_candidate_feature_table(ds, candidates = candidates, params = params, selected_trains = selected_trains)
}

detect_burst_candidates <- function(dat, params = default_params(), train = "", min_isi_sec = NULL) {
  p <- effective_params_for_detector(apply_schema_defaults(params))
  min_isi <- min_isi_sec %||% p$detector$min_valid_isi_sec %||% 0.0009
  burst_p <- effective_burst_params(p)
  detect_burst_train(dat, burst_p, min_isi_sec = min_isi, train = train)
}

detect_tonic_candidates <- function(dat, occupied_idx = integer(), params = default_params(), train = "", min_isi_sec = NULL) {
  p <- effective_params_for_detector(apply_schema_defaults(params))
  min_isi <- min_isi_sec %||% p$detector$min_valid_isi_sec %||% 0.0009
  n <- nrow(dat)
  occupied <- rep(FALSE, n)
  if (length(occupied_idx) > 0L) {
    if (is.logical(occupied_idx)) {
      if (length(occupied_idx) == 1L) {
        occupied[] <- isTRUE(occupied_idx)
      } else if (length(occupied_idx) == n) {
        occupied <- as.logical(occupied_idx)
        occupied[is.na(occupied)] <- FALSE
      } else {
        stop("Logical occupied_idx must have length 1 or nrow(dat).",
             call. = FALSE)
      }
    } else {
      raw_idx <- suppressWarnings(as.numeric(occupied_idx))
      invalid <- !is.finite(raw_idx) |
        abs(raw_idx - round(raw_idx)) >= sqrt(.Machine$double.eps) |
        raw_idx < 1 | raw_idx > n
      if (any(invalid)) {
        stop("occupied_idx contains a non-integer or out-of-range index.",
             call. = FALSE)
      }
      idx <- as.integer(raw_idx)
      occupied[idx] <- TRUE
    }
  }
  # The public candidate API must use the same State generator and the same
  # calibration-frozen duration/short-support contract as the formal detector.
  # Occupied support is an explicit caller exclusion, not a Burst-to-State
  # veto; it is applied only during interval selection and cannot shift the
  # train-level Tonic band.
  vp <- stpd_event_grammar_params_impl(
    dat, p, min_isi_sec = min_isi, train = train
  )
  raw <- stpd_event_core_detect_tonic(
    dat, p, vp, min_isi_sec = min_isi, train = train
  )
  core <- stpd_event_core_weighted_select(
    raw, locked = occupied, patterns = "tonic"
  )
  if (nrow(core) > 0L) {
    selected <- as.logical(core$selected_for_auto)
    selected[is.na(selected)] <- FALSE
    core <- core[selected, , drop = FALSE]
  }
  # Preserve the established public return contract (two interval columns for
  # an empty result; six columns for detected candidates) while routing the
  # scientific decision through the current event-core implementation.
  if (nrow(core) == 0L) {
    return(data.frame(start_isi = integer(), end_isi = integer()))
  }
  data.frame(
    start_isi = as.integer(core$start_isi),
    end_isi = as.integer(core$end_isi),
    tonic_burst_overlap_ref_sec =
      as.numeric(core$tonic_burst_overlap_ref_sec),
    tonic_burst_overlap_guard =
      as.logical(core$tonic_burst_overlap_guard),
    tonic_burst_overlap_veto_applied = FALSE,
    tonic_anti_burst_veto_applied = FALSE,
    stringsAsFactors = FALSE
  )
}

detect_pause_candidates <- function(dat, occupied_idx = integer(), params = default_params(), current_labels = NULL, train = "", min_isi_sec = NULL) {
  p <- effective_params_for_detector(apply_schema_defaults(params))
  min_isi <- min_isi_sec %||% p$detector$min_valid_isi_sec %||% 0.0009
  pause_p <- effective_pause_params(p)
  detect_pause_train(dat, occupied_idx = occupied_idx, p = pause_p, tonic_p = p$tonic, min_isi_sec = min_isi, current_labels = current_labels, train = train)
}
