# Auto-generated modular extraction from Spike Train Pattern Detector reference script.
# Do not edit generated function names blindly; use git for version history.

# ============================================================
# Governance helpers: parameter report, stationarity QC,
# and migration roadmap. These helpers do not change detector labels.
# ============================================================

flatten_params <- function(x, prefix = "") {
  if (is.null(x)) return(data.frame(path = character(), value = character(), value_type = character(), stringsAsFactors = FALSE))
  rows <- list()
  add_row <- function(path, val) {
    if (is.list(val) && !is.data.frame(val)) {
      # Non-scalar lists such as train-specific ranges are summarized; they are
      # not expanded recursively because their keys may be arbitrary train names.
      return(data.frame(path = path, value = paste0("<list:", length(val), ">"), value_type = "list", stringsAsFactors = FALSE))
    }
    if (is.data.frame(val)) return(data.frame(path = path, value = paste0("<data.frame:", nrow(val), "x", ncol(val), ">"), value_type = "data.frame", stringsAsFactors = FALSE))
    vv <- val
    if (length(vv) == 0) vv <- NA
    if (length(vv) > 1) {
      out <- paste(as.character(vv), collapse = ";")
      typ <- paste0(class(vv)[1], "[]")
    } else {
      out <- ifelse(is.na(vv), "NA", as.character(vv))
      typ <- class(vv)[1]
    }
    data.frame(path = path, value = out, value_type = typ, stringsAsFactors = FALSE)
  }
  walk <- function(obj, pref) {
    if (is.list(obj) && !is.data.frame(obj)) {
      nms <- names(obj)
      if (is.null(nms)) nms <- as.character(seq_along(obj))
      for (ii in seq_along(obj)) {
        nm <- nms[ii]
        path <- if (nzchar(pref)) paste0(pref, ".", nm) else nm
        val <- obj[[ii]]
        if (is.list(val) && !is.data.frame(val) && length(val) > 0 && !any(grepl("adaptive_train_ranges|train_.*ranges", path))) {
          walk(val, path)
        } else {
          rows[[length(rows) + 1L]] <<- add_row(path, val)
        }
      }
    } else {
      rows[[length(rows) + 1L]] <<- add_row(pref, obj)
    }
  }
  walk(x, prefix)
  if (length(rows) == 0) return(data.frame(path = character(), value = character(), value_type = character(), stringsAsFactors = FALSE))
  dplyr::bind_rows(rows)
}

parameter_report_table <- function(params, defaults = default_params_sec(), preset_params = NULL) {
  cur <- flatten_params(effective_params_for_detector(params))
  def <- flatten_params(effective_params_for_detector(defaults))
  names(def)[names(def) == "value"] <- "default_value"
  names(def)[names(def) == "value_type"] <- "default_value_type"
  out <- dplyr::full_join(cur, def, by = "path")
  if (is.null(preset_params)) {
    preset_name <- tryCatch(as.character(params$detector$preset_name %||% "balanced_single_unit"), error = function(e) "balanced_single_unit")
    preset_params <- apply_preset_to_params(defaults, preset_name)
  }
  pre <- flatten_params(effective_params_for_detector(preset_params))
  names(pre)[names(pre) == "value"] <- "preset_value"
  names(pre)[names(pre) == "value_type"] <- "preset_value_type"
  out <- dplyr::left_join(out, pre[, c("path", "preset_value", "preset_value_type"), drop = FALSE], by = "path")
  out$value[is.na(out$value)] <- "NA"
  out$default_value[is.na(out$default_value)] <- "NA"
  out$preset_value[is.na(out$preset_value)] <- "NA"
  out$differs_from_default <- out$value != out$default_value
  out$changed_from_default <- out$differs_from_default
  out$differs_from_preset <- out$value != out$preset_value
  out$parameter_group <- sub("\\..*$", "", out$path)
  reg <- stpd_parameter_registry(defaults)
  meta_cols <- intersect(names(reg), c("path", "label", "scientific_note", "registry_scope", "ui_level", "section", "section_order", "ui_order", "help_text", "control_type", "advanced", "expert_only"))
  out <- dplyr::left_join(out, reg[, meta_cols, drop = FALSE], by = "path")
  out$ui_level[is.na(out$ui_level) | !nzchar(out$ui_level)] <- "unclassified"
  out$section[is.na(out$section) | !nzchar(out$section)] <- out$parameter_group[is.na(out$section) | !nzchar(out$section)]
  out$method_reporting_priority <- dplyr::case_when(
    out$ui_level == "basic" ~ "high",
    grepl("refractory|possible_burst|tonic_like|local_compression|long_burst|pause.global|pause.exclude|patterns_to_run|min_valid_isi|preset_name", out$path) ~ "high",
    out$differs_from_default ~ "modified",
    TRUE ~ "default"
  )
  out <- out[order(suppressWarnings(as.numeric(out$section_order)), suppressWarnings(as.numeric(out$ui_order)), out$parameter_group, out$path), , drop = FALSE]
  out
}

stationarity_train_qc <- function(dat, train = "", min_isi_sec = 0.001, n_bins = 8L, min_valid_isi = 60L,
                                            warn_ratio = 3.0, error_ratio = 6.0) {
  if (is.null(dat) || nrow(dat) < 3 || !("ISI_sec" %in% names(dat))) {
    return(data.frame(train = train, stationarity_status = "unavailable", stationarity_drift_ratio = NA_real_,
                      stationarity_log_median_range = NA_real_, stationarity_n_bins = 0L,
                      stationarity_warning = "insufficient spikes", stringsAsFactors = FALSE))
  }
  isi <- suppressWarnings(as.numeric(dat$ISI_sec))
  ts <- suppressWarnings(as.numeric(dat$timestamp_sec))
  valid_idx <- which(is.finite(isi) & isi >= min_isi_sec)
  valid_idx <- valid_idx[valid_idx > 1L]
  if (length(valid_idx) < min_valid_isi) {
    return(data.frame(train = train, stationarity_status = "low_n", stationarity_drift_ratio = NA_real_,
                      stationarity_log_median_range = NA_real_, stationarity_n_bins = 0L,
                      stationarity_warning = paste0("<", min_valid_isi, " valid ISIs; stationarity check weak"), stringsAsFactors = FALSE))
  }
  n_bins <- max(3L, min(as.integer(n_bins), length(valid_idx)))
  # Bin by time, not by fixed ISI count, to flag slow regime shifts.
  t_valid <- ts[valid_idx]
  if (!all(is.finite(t_valid)) || length(unique(t_valid)) < 3) {
    bins <- cut(seq_along(valid_idx), breaks = n_bins, labels = FALSE, include.lowest = TRUE)
  } else {
    bins <- cut(t_valid, breaks = n_bins, labels = FALSE, include.lowest = TRUE)
  }
  meds <- tapply(isi[valid_idx], bins, function(x) stats::median(x[is.finite(x)], na.rm = TRUE))
  meds <- as.numeric(meds[is.finite(meds) & meds > 0])
  if (length(meds) < 3) {
    return(data.frame(train = train, stationarity_status = "unavailable", stationarity_drift_ratio = NA_real_,
                      stationarity_log_median_range = NA_real_, stationarity_n_bins = length(meds),
                      stationarity_warning = "too few valid temporal bins", stringsAsFactors = FALSE))
  }
  ratio <- max(meds, na.rm = TRUE) / max(min(meds, na.rm = TRUE), .Machine$double.eps)
  log_range <- diff(range(log10(meds), na.rm = TRUE))
  # Stationarity drift is an advisory risk flag. High drift is reported as
  # warning_high_drift instead of error so Data QC reserves ERROR for data
  # integrity issues such as invalid duration, non-positive ISIs or timestamp
  # corruption.
  status <- if (ratio >= error_ratio) "warning_high_drift" else if (ratio >= warn_ratio) "warning" else "ok"
  warn <- if (status == "ok") "" else paste0("sliding median ISI drift ratio=", signif(ratio, 4), "; pause/global thresholds may be state-dependent")
  data.frame(train = train, stationarity_status = status, stationarity_drift_ratio = ratio,
             stationarity_log_median_range = log_range, stationarity_n_bins = length(meds),
             stationarity_warning = warn, stringsAsFactors = FALSE)
}

stationarity_qc <- function(trains, min_isi_sec = 0.001, n_bins = 8L, min_valid_isi = 60L,
                                      warn_ratio = 3.0, error_ratio = 6.0) {
  if (is.null(trains) || length(trains) == 0) return(data.frame())
  dplyr::bind_rows(lapply(names(trains), function(tr) {
    stationarity_train_qc(trains[[tr]], train = tr, min_isi_sec = min_isi_sec, n_bins = n_bins,
                                    min_valid_isi = min_valid_isi, warn_ratio = warn_ratio, error_ratio = error_ratio)
  }))
}

development_roadmap <- function() {
  data.frame(
    stage = c("freeze_reference", "modular_engine", "unit_tests", "validation_framework", "performance_core"),
    recommendation = c(
      "Freeze this single-file script as a reference prototype; stop adding detection classes here.",
      "Move IO/QC/features/candidates/final_classification/ledger/evaluation/export/UI into separate package modules.",
      "Create golden tests for boundary burst, refractory policies, HF packets, pause boundaries, ledgers, and exports.",
      "Add held-out train/dataset evaluation, event-level IoU, and threshold sensitivity reports before publication use.",
      "After modularization, move ISI percentiles, rolling medians, candidate scans, and interval joins to Rcpp/data.table."
    ),
    scientific_rationale = c(
      "The current script is valuable as a behavior reference, not as a maintainable production core.",
      "Separation of concerns prevents UI/API/export drift from changing scientific labels silently.",
      "Regression tests protect biologically meaningful edge cases from future patches.",
      "Interactive threshold tuning can overfit manual labels without held-out evidence.",
      "Performance optimization is safest once feature computation and classification boundaries are explicit."
    ),
    stringsAsFactors = FALSE
  )
}

overfit_warning_report <- function(ds, params = NULL) {
  params <- params %||% (ds$params_last %||% default_params_sec())
  has_manual <- FALSE
  n_manual <- 0L
  if (!is.null(ds) && !is.null(ds$trains)) {
    for (tr in names(ds$trains)) {
      pm <- as.character(ds$trains[[tr]]$pattern_manual %||% "")
      n_manual <- n_manual + sum(pm != "", na.rm = TRUE)
    }
    has_manual <- n_manual > 0
  }
  learned_ranges_used <- any(c(length(params$burst$adaptive_train_ranges %||% list()),
                               length(params$tonic$adaptive_train_ranges %||% list()),
                               length(params$pause$adaptive_train_ranges %||% list())) > 0)
  data.frame(
    item = c("manual_labels_present", "n_manual_labeled_ISI", "learned_train_specific_ranges_used", "recommended_interpretation", "publication_warning"),
    value = c(as.character(has_manual), as.character(n_manual), as.character(learned_ranges_used),
              "Use current metrics as calibration feedback unless held-out train/dataset evaluation is performed.",
              "Report strict high-confidence, review-candidate, and burst-family metrics separately; do not merge possible_burst into burst silently."),
    stringsAsFactors = FALSE
  )
}

write_governance_exports <- function(ds, params = default_params_sec(), out_dir) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  if (!is.null(ds$results$candidate_features) && nrow(ds$results$candidate_features) > 0) {
    write_csv_safe(ds$results$candidate_features, file.path(out_dir, "Candidate_features_audit.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  }
  if (!is.null(ds$results$final_classification_audit) && nrow(ds$results$final_classification_audit) > 0) {
    write_csv_safe(ds$results$final_classification_audit, file.path(out_dir, "Final_classification_audit.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  }
  if (!is.null(ds$results$event_distribution_evidence) && nrow(ds$results$event_distribution_evidence) > 0) {
    write_csv_safe(ds$results$event_distribution_evidence, file.path(out_dir, "Event_distribution_evidence.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  }
  if (!is.null(ds$results$train_distribution_features) && nrow(ds$results$train_distribution_features) > 0) {
    write_csv_safe(ds$results$train_distribution_features, file.path(out_dir, "Train_distribution_features.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  }
  if (!is.null(ds$results$spike_count_pmf) && nrow(ds$results$spike_count_pmf) > 0) {
    write_csv_safe(ds$results$spike_count_pmf, file.path(out_dir, "Spike_count_PMF.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  }
  if (!is.null(ds$results$validation_guidance) && nrow(ds$results$validation_guidance) > 0) {
    write_csv_safe(ds$results$validation_guidance, file.path(out_dir, "Validation_guidance.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  }
  if (!is.null(ds$results$consistency_audit) && nrow(ds$results$consistency_audit) > 0) {
    write_csv_safe(ds$results$consistency_audit, file.path(out_dir, "Semantic_consistency_report.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  }
  if (!is.null(ds$results$governance_summary) && nrow(ds$results$governance_summary) > 0) {
    write_csv_safe(ds$results$governance_summary, file.path(out_dir, "Parameter_governance_summary.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  } else {
    write_csv_safe(params_governance_summary(params), file.path(out_dir, "Parameter_governance_summary.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  }
  pr <- if (!is.null(ds$results$parameters_report) && nrow(ds$results$parameters_report) > 0) ds$results$parameters_report else parameter_report_table(params)
  write_csv_safe(pr, file.path(out_dir, "Parameters_report.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write_csv_safe(stpd_schema_coverage_report(default_params_sec()), file.path(out_dir, "Parameter_schema_coverage.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write_csv_safe(stpd_parameter_schema(scope = "all", params = default_params_sec()), file.path(out_dir, "Parameter_schema_all.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  if (!is.null(ds$results$stationarity_qc) && nrow(ds$results$stationarity_qc) > 0) {
    write_csv_safe(ds$results$stationarity_qc, file.path(out_dir, "Stationarity_QC.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  } else if (!is.null(ds$quality) && nrow(ds$quality) > 0 && any(grepl("stationarity_", names(ds$quality)))) {
    cols <- c("train", grep("stationarity_", names(ds$quality), value = TRUE))
    write_csv_safe(ds$quality[, intersect(cols, names(ds$quality)), drop = FALSE], file.path(out_dir, "Stationarity_QC.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  }
  write_csv_safe(development_roadmap(), file.path(out_dir, "Development_roadmap.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  if (!is.null(ds$results$overfit_warning_report) && nrow(ds$results$overfit_warning_report) > 0) {
    write_csv_safe(ds$results$overfit_warning_report, file.path(out_dir, "Overfit_warning_report.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  } else {
    write_csv_safe(overfit_warning_report(ds, params), file.path(out_dir, "Overfit_warning_report.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  }
  writeLines(stpd_methodological_warning(as_vector = TRUE), file.path(out_dir, "Methodological_warnings.txt"), useBytes = TRUE)
  write_csv_safe(preset_catalog(), file.path(out_dir, "Preset_catalog.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  invisible(TRUE)
}
