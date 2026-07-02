# ============================================================
# event grammar public API / audit integrity fixes
# ------------------------------------------------------------
# This module connects the event grammar candidate audit to the public
# candidate-ledger -> candidate-features -> eventness/final-decisions pipeline.
# It also provides fixed empty-table schemas and protects batch raw-CSV loading
# from common derived/summary CSV files.
# ============================================================

stpd_event_grammar_empty_candidate_ledger <- function() {
  tibble::tibble(
    candidate_id = character(), run_id = character(), params_hash = character(),
    train = character(), start_isi = integer(), end_isi = integer(),
    start_time_sec = double(), end_time_sec = double(),
    candidate_source = character(), raw_candidate_class = character(),
    final_candidate_class = character(), final_label_majority = character(),
    written_to_auto = logical(), visible_in_raster = logical(), selected_for_auto = logical(),
    score = double(), priority = double(), possible_burst_subtype = character(),
    uncertainty_reason = character(), policy_action = character(), rejection_reason = character(),
    refractory_suspect_n = double(), pause_local_median_sec = double(),
    pause_effective_threshold_sec = double(), pause_global_median_sec = double(),
    pause_global_threshold_sec = double(), gate_status = character(), decision_path = character(),
    action = character(), selection_status = character(), candidate_audit_source = character()
  )
}

stpd_event_grammar_empty_candidate_features <- function() {
  tibble::tibble(
    candidate_id = character(), run_id = character(), params_hash = character(), train = character(),
    start_isi = integer(), end_isi = integer(), n_isi = integer(), n_spikes = integer(),
    start_time_sec = double(), end_time_sec = double(), duration_sec = double(),
    candidate_source = character(), raw_candidate_class = character(), final_candidate_class = character(),
    written_to_auto = logical(), review_required = logical(), uncertainty_reason = character(),
    biological_warning = character(), mean_ISI_sec = double(), median_ISI_sec = double(),
    q90_ISI_sec = double(), min_ISI_sec = double(), max_ISI_sec = double(),
    mean_ISI_pct = double(), max_ISI_pct = double(), short_ISI_fraction_35pct = double(),
    pre_ISI_sec = double(), post_ISI_sec = double(), pre_core_ratio = double(),
    post_core_ratio = double(), edge_contrast_min = double(), edge_contrast_geom = double(),
    LV = double(), CV = double(), MM = double(), refractory_suspect_n = double(),
    policy_action = character(), rejection_reason = character()
  )
}

stpd_event_grammar_add_eventness_schema_cols <- function(features) {
  out <- tibble::as_tibble(features %||% stpd_event_grammar_empty_candidate_features())
  numeric_cols <- c(
    "q10_ISI_sec", "q50_ISI_sec", "q90_q10_ratio", "distant_context_median_sec",
    "context_contrast", "return_to_baseline_score", "eventness_score",
    "eventness_edge_component", "eventness_context_component", "regularity_score",
    "internal_outlier_count", "internal_outlier_fraction", "internal_outlier_max_ratio"
  )
  logical_cols <- c(
    "medium_eventness_review", "long_burst_context_pass", "long_burst_short_fraction_pass",
    "long_burst_duration_pass", "long_burst_internal_outlier_pass"
  )
  character_cols <- c("eventness_zone", "long_burst_definition_status", "eventness_audit_note")
  for (nm in numeric_cols) if (!(nm %in% names(out))) out[[nm]] <- numeric()
  for (nm in logical_cols) if (!(nm %in% names(out))) out[[nm]] <- logical()
  for (nm in character_cols) if (!(nm %in% names(out))) out[[nm]] <- character()
  out
}

stpd_event_grammar_empty_final_decisions <- function(features = NULL) {
  base <- stpd_event_grammar_add_eventness_schema_cols(features %||% stpd_event_grammar_empty_candidate_features())
  add_chr <- c(
    "final_class", "confidence_tier", "decision_reason", "biological_warning",
    "recommended_family", "recommended_subtype", "recommended_final_class", "decision_path",
    "recommendation_confidence_tier", "recommended_uncertainty_reason",
    "recommended_reporting_layer", "final_classification_note", "recommended_interpretation_note"
  )
  add_lgl <- c("review_required", "recommended_review_required")
  for (nm in add_chr) if (!(nm %in% names(base))) base[[nm]] <- character()
  for (nm in add_lgl) if (!(nm %in% names(base))) base[[nm]] <- logical()
  base
}

stpd_event_grammar_mode_nonempty <- function(x) {
  x <- as.character(x); x[is.na(x)] <- ""; x <- x[nzchar(x)]
  if (length(x) == 0) return("")
  names(sort(table(x), decreasing = TRUE))[1]
}

stpd_event_grammar_row_chr <- function(row, names_vec, default = "") {
  for (nm in names_vec) {
    if (nm %in% names(row)) {
      v <- row[[nm]][1]
      if (!is.null(v) && length(v) > 0 && !is.na(v)) return(as.character(v))
    }
  }
  default
}

stpd_event_grammar_row_num <- function(row, names_vec, default = NA_real_) {
  for (nm in names_vec) {
    if (nm %in% names(row)) {
      v <- suppressWarnings(as.numeric(row[[nm]][1]))
      if (is.finite(v)) return(v)
    }
  }
  default
}

stpd_event_grammar_row_logical <- function(row, names_vec, default = FALSE) {
  for (nm in names_vec) {
    if (nm %in% names(row)) {
      v <- row[[nm]][1]
      if (is.logical(v) && !is.na(v)) return(isTRUE(v))
      if (!is.na(v)) return(tolower(as.character(v)) %in% c("true", "1", "yes", "y"))
    }
  }
  default
}

stpd_candidate_diagnostic_audit_to_ledger <- function(ds, params, selected_trains = NULL, run_id = NULL, params_hash = NULL) {
  if (is.null(ds) || is.null(ds$trains) || is.null(ds$results)) return(stpd_event_grammar_empty_candidate_ledger())
  audit <- ds$results$candidate_diagnostic_audit %||% data.frame()
  if (is.null(audit) || nrow(audit) == 0) return(stpd_event_grammar_empty_candidate_ledger())
  audit <- as.data.frame(audit)
  run_id <- run_id %||% paste0("run_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  params_hash <- params_hash %||% compute_params_hash(params)
  trains <- selected_trains %||% names(ds$trains)
  trains <- intersect(trains, names(ds$trains))
  rows <- list()
  for (ii in seq_len(nrow(audit))) {
    r <- audit[ii, , drop = FALSE]
    tr <- stpd_event_grammar_row_chr(r, "train")
    if (!tr %in% trains || !tr %in% names(ds$trains)) next
    dat <- ds$trains[[tr]]
    s0 <- as.integer(stpd_event_grammar_row_num(r, "start_isi", NA_real_))
    e0 <- as.integer(stpd_event_grammar_row_num(r, "end_isi", NA_real_))
    if (!is.finite(s0) || !is.finite(e0) || s0 < 2L || e0 > nrow(dat) || e0 < s0) next
    layer <- stpd_event_grammar_row_chr(r, c("candidate_layer", "candidate_source", "source"), "candidate_diagnostic")
    cls <- stpd_event_grammar_row_chr(r, c("candidate_class", "raw_candidate_class", "class", "final_label"), "candidate")
    final_lab <- stpd_event_grammar_row_chr(r, c("final_label", "class", "final_candidate_class"), cls)
    gate <- stpd_event_grammar_row_chr(r, c("gate_status", "status"), "")
    decision <- stpd_event_grammar_row_chr(r, c("decision_path", "decision", "rejection_reason", "reject_reason"), "")
    action <- stpd_event_grammar_row_chr(r, c("action", "policy_action"), "")
    selection <- stpd_event_grammar_row_chr(r, "selection_status", "")
    selected <- stpd_event_grammar_row_logical(r, "selected_for_auto", FALSE)
    lab_ok <- nzchar(final_lab) && !(final_lab %in% c("reject", "profile", "unlabeled", "not_selected"))
    auto_vals <- as.character(dat$pattern_auto[s0:e0]); auto_vals[is.na(auto_vals)] <- ""
    visible <- selected && lab_ok && any(auto_vals == final_lab, na.rm = TRUE)
    auto_major <- stpd_event_grammar_mode_nonempty(auto_vals)
    final_vals <- tryCatch(
      compute_final_pattern(dat$pattern_manual, dat$pattern_auto, dat$ISI_sec,
                            auto_others = FALSE,
                            min_isi_sec = params$detector$min_valid_isi_sec %||% 0.0009)[s0:e0],
      error = function(e) rep("", e0 - s0 + 1L)
    )
    final_major <- stpd_event_grammar_mode_nonempty(final_vals)
    rejection <- stpd_event_grammar_row_chr(r, c("rejection_reason", "reject_reason"), "")
    if (!nzchar(rejection) && (identical(action, "reject") || identical(final_lab, "reject") || !selected)) rejection <- decision
    rows[[length(rows) + 1L]] <- tibble::tibble(
      candidate_id = stpd_event_grammar_row_chr(r, "candidate_id", paste0(run_id, ":audit:", tr, ":", ii)),
      run_id = run_id,
      params_hash = params_hash,
      train = tr,
      start_isi = s0,
      end_isi = e0,
      start_time_sec = stpd_event_grammar_row_num(r, "start_time_sec", suppressWarnings(as.numeric(dat$timestamp_sec[s0 - 1L]))),
      end_time_sec = stpd_event_grammar_row_num(r, "end_time_sec", suppressWarnings(as.numeric(dat$timestamp_sec[e0]))),
      candidate_source = layer,
      raw_candidate_class = cls,
      final_candidate_class = if (lab_ok) final_lab else cls,
      final_label_majority = if (nzchar(final_major)) final_major else "unlabeled",
      written_to_auto = selected && lab_ok,
      visible_in_raster = visible,
      selected_for_auto = selected,
      score = stpd_event_grammar_row_num(r, c("score", "candidate_score"), NA_real_),
      priority = stpd_event_grammar_row_num(r, "priority", NA_real_),
      possible_burst_subtype = if (final_lab == "possible_burst" || cls == "possible_burst") paste(c(gate, decision), collapse = ";") else "",
      uncertainty_reason = paste(c(gate, decision), collapse = ";"),
      policy_action = action,
      rejection_reason = rejection,
      refractory_suspect_n = stpd_event_grammar_row_num(r, "refractory_suspect_n", NA_real_),
      pause_local_median_sec = stpd_event_grammar_row_num(r, "pause_local_median_sec", NA_real_),
      pause_effective_threshold_sec = stpd_event_grammar_row_num(r, "pause_effective_threshold_sec", NA_real_),
      pause_global_median_sec = stpd_event_grammar_row_num(r, "pause_global_median_sec", NA_real_),
      pause_global_threshold_sec = stpd_event_grammar_row_num(r, "pause_global_threshold_sec", NA_real_),
      gate_status = gate,
      decision_path = decision,
      action = action,
      selection_status = selection,
      candidate_audit_source = "candidate_diagnostic_audit"
    )
  }
  if (length(rows) == 0) stpd_event_grammar_empty_candidate_ledger() else dplyr::bind_rows(rows) %>% dplyr::arrange(train, start_isi, end_isi, candidate_source)
}
