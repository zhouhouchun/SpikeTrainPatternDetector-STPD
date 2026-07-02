# ============================================================
# Candidate-audit review fixes
# ------------------------------------------------------------
# This module fixes the separation between diagnostic audits and the
# public candidate-ledger/eventness/final-decision pipeline.
#
# Design rule:
#   - candidate_diagnostic_audit keeps all selected, rejected and diagnostic windows.
#   - candidate_ledger / candidate_features / final_decisions receive only
#     selected, biologically labelled candidates that were accepted for AUTO.
# Rejected/profile/unlabeled windows must not propagate into downstream
# eventness statistics or final classification audits.
# ============================================================

stpd_event_grammar_review_reject_labels <- function() {
  c("", "reject", "rejected", "profile", "unlabeled", "not_selected",
    "none", "na", "nan", "candidate", "diagnostic", "background",
    "manual_lock", "manual_lock_trimmed", "blocked_by_manual_label",
    "auto_write_blocked_by_manual_lock")
}

stpd_event_grammar_is_reject_like <- function(x) {
  x <- tolower(trimws(as.character(x %||% "")))
  if (length(x) == 0) return(FALSE)
  any(!is.na(x) & nzchar(x) & (
    x %in% stpd_event_grammar_review_reject_labels() |
      grepl("reject|not[_ -]?selected|diagnostic|profile|manual[_ -]?lock|blocked[_ -]?by[_ -]?manual|auto[_ -]?write[_ -]?blocked", x)
  ))
}

stpd_event_grammar_is_selected_like <- function(selected_bool, selection = "", action = "", written = FALSE, visible = FALSE) {
  if (isTRUE(selected_bool) || isTRUE(written) || isTRUE(visible)) return(TRUE)
  txt <- tolower(trimws(as.character(c(selection, action))))
  any(txt %in% c("selected", "accepted", "accept", "keep", "kept", "write", "written", "selected_for_auto", "final", "final_selected", "auto", "auto_written"))
}

# Robust schema helper. The older implementation assigned numeric()/logical()/character()
# to missing columns; this crashes for non-empty tibbles. Fill with NA vectors of
# the correct row count instead.
stpd_event_grammar_add_eventness_schema_cols <- function(features) {
  out <- tibble::as_tibble(features %||% stpd_event_grammar_empty_candidate_features())
  n <- nrow(out)
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
  for (nm in numeric_cols) if (!(nm %in% names(out))) out[[nm]] <- if (n == 0) numeric() else rep(NA_real_, n)
  for (nm in logical_cols) if (!(nm %in% names(out))) out[[nm]] <- if (n == 0) logical() else rep(NA, n)
  for (nm in character_cols) if (!(nm %in% names(out))) out[[nm]] <- if (n == 0) character() else rep(NA_character_, n)
  out
}

stpd_event_grammar_empty_final_decisions <- function(features = NULL) {
  base <- stpd_event_grammar_add_eventness_schema_cols(features %||% stpd_event_grammar_empty_candidate_features())
  n <- nrow(base)
  add_chr <- c(
    "final_class", "confidence_tier", "decision_reason", "biological_warning",
    "recommended_family", "recommended_subtype", "recommended_final_class", "decision_path",
    "recommendation_confidence_tier", "recommended_uncertainty_reason",
    "recommended_reporting_layer", "final_classification_note", "recommended_interpretation_note"
  )
  add_lgl <- c("review_required", "recommended_review_required")
  for (nm in add_chr) if (!(nm %in% names(base))) base[[nm]] <- if (n == 0) character() else rep(NA_character_, n)
  for (nm in add_lgl) if (!(nm %in% names(base))) base[[nm]] <- if (n == 0) logical() else rep(NA, n)
  base
}

stpd_event_grammar_has_candidate_audit <- function(ds) {
  audit <- NULL
  if (!is.null(ds) && !is.null(ds$results)) audit <- ds$results$candidate_diagnostic_audit
  !is.null(audit) && nrow(as.data.frame(audit)) > 0
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
    cls <- stpd_event_grammar_row_chr(r, c("candidate_class", "raw_candidate_class", "class"), "candidate")
    final_lab <- stpd_event_grammar_row_chr(r, c("final_label", "final_candidate_class", "class"), cls)
    gate <- stpd_event_grammar_row_chr(r, c("gate_status", "status"), "")
    decision <- stpd_event_grammar_row_chr(r, c("decision_path", "decision", "rejection_reason", "reject_reason"), "")
    action <- stpd_event_grammar_row_chr(r, c("action", "policy_action"), "")
    selection <- stpd_event_grammar_row_chr(r, "selection_status", "")

    auto_vals <- as.character(dat$pattern_auto[s0:e0]); auto_vals[is.na(auto_vals)] <- ""
    auto_major <- stpd_event_grammar_mode_nonempty(auto_vals)
    if ((!nzchar(final_lab) || stpd_event_grammar_is_reject_like(final_lab)) && nzchar(auto_major)) final_lab <- auto_major

    written_flag <- stpd_event_grammar_row_logical(r, c("written_to_auto", "auto_written"), FALSE)
    visible_flag <- nzchar(final_lab) && length(auto_vals) > 0L && all(nzchar(auto_vals) & auto_vals == final_lab)
    selected_flag <- stpd_event_grammar_row_logical(r, c("selected_for_auto", "selected"), FALSE)
    selected <- stpd_event_grammar_is_selected_like(selected_flag, selection = selection, action = action, written = written_flag, visible = visible_flag)

    reject_like <- stpd_event_grammar_is_reject_like(final_lab) ||
      stpd_event_grammar_is_reject_like(action) || stpd_event_grammar_is_reject_like(selection) ||
      stpd_event_grammar_is_reject_like(gate) || stpd_event_grammar_is_reject_like(decision)
    lab_ok <- nzchar(final_lab) && !(tolower(trimws(final_lab)) %in% stpd_event_grammar_review_reject_labels()) && !stpd_event_grammar_is_reject_like(final_lab)

    # Critical Codex-review fix: rejected/profile/unlabeled/diagnostic windows remain
    # in candidate_diagnostic_audit only. They are not public candidate-ledger rows and must
    # not reach eventness/final-decision statistics.
    if (!isTRUE(selected) || !isTRUE(lab_ok) || isTRUE(reject_like) || !isTRUE(visible_flag)) next

    final_vals <- tryCatch(
      compute_final_pattern(dat$pattern_manual, dat$pattern_auto, dat$ISI_sec,
                            auto_others = FALSE,
                            min_isi_sec = params$detector$min_valid_isi_sec %||% 0.0009)[s0:e0],
      error = function(e) rep("", e0 - s0 + 1L)
    )
    final_major <- stpd_event_grammar_mode_nonempty(final_vals)
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
      final_candidate_class = final_lab,
      final_label_majority = if (nzchar(final_major)) final_major else "unlabeled",
      written_to_auto = TRUE,
      visible_in_raster = TRUE,
      selected_for_auto = TRUE,
      score = stpd_event_grammar_row_num(r, c("score", "candidate_score"), NA_real_),
      priority = stpd_event_grammar_row_num(r, "priority", NA_real_),
      possible_burst_subtype = if (final_lab == "possible_burst" || cls == "possible_burst") paste(c(gate, decision), collapse = ";") else "",
      uncertainty_reason = paste(c(gate, decision), collapse = ";"),
      policy_action = action,
      rejection_reason = "",
      refractory_suspect_n = stpd_event_grammar_row_num(r, "refractory_suspect_n", NA_real_),
      pause_local_median_sec = stpd_event_grammar_row_num(r, "pause_local_median_sec", NA_real_),
      pause_effective_threshold_sec = stpd_event_grammar_row_num(r, "pause_effective_threshold_sec", NA_real_),
      pause_global_median_sec = stpd_event_grammar_row_num(r, "pause_global_median_sec", NA_real_),
      pause_global_threshold_sec = stpd_event_grammar_row_num(r, "pause_global_threshold_sec", NA_real_),
      gate_status = gate,
      decision_path = decision,
      action = action,
      selection_status = selection,
      candidate_audit_source = "candidate_diagnostic_audit_selected_only"
    )
  }
  if (length(rows) == 0) stpd_event_grammar_empty_candidate_ledger() else dplyr::bind_rows(rows) %>% dplyr::arrange(train, start_isi, end_isi, candidate_source)
}

# -------------------------------------------------------------------------
# Raw CSV guardrails. Filename clues now warn unless paired with suspicious
# schema/data shape. This avoids rejecting legitimate raw files whose names
# contain words such as "event", while still blocking derived threshold/summary
# tables such as BURST_ISI_threshould.csv unless explicitly allowed.
# -------------------------------------------------------------------------

stpd_event_grammar_derived_csv_filename <- function(path) {
  nm <- tolower(basename(as.character(path)))
  pat <- "(^|[_ -])(sliding|summary|threshold|threshould|thresholds|thresh|candidate|candidates|ledger|eventness|event_audit|events_final|audit|feature|features|metric|metrics|isi_base|tonic_summary|misi|logisi|support|output|outputs|result|results|qc|validation|manual_vs_detector|stationarity|duplicate|artifact|parameter|parameters|near_miss|burst_candidate|burst_candidates)([_ .-]|$)"
  grepl(pat, nm, perl = TRUE)
}

stpd_event_grammar_derived_csv_schema_score <- function(df) {
  if (is.null(df) || ncol(df) == 0) return(0L)
  cn <- tolower(colnames(df))
  derived_exact <- c("train", "pattern", "label", "final_label", "candidate_id", "event_id", "start_isi", "end_isi", "start_time_sec", "end_time_sec", "duration_sec", "score", "threshold", "threshould", "mean_isi", "median_isi", "q90_isi", "cv", "lv", "mm", "count", "n_spikes", "source", "status", "decision")
  hit <- cn %in% derived_exact | grepl("pattern|candidate|event|threshold|threshould|audit|score|duration|mean|median|quantile|q[0-9]+|cv$|lv$|mm$|count|fraction|metric|status|decision|source|family|summary|result", cn)
  as.integer(sum(hit, na.rm = TRUE))
}

stpd_event_grammar_derived_csv_schema <- function(df) {
  if (is.null(df) || ncol(df) == 0) return(FALSE)
  cn <- tolower(colnames(df))
  hit_n <- stpd_event_grammar_derived_csv_schema_score(df)
  hit_frac <- hit_n / max(length(cn), 1L)
  hit_n >= 3L && hit_frac >= 0.30
}

stpd_event_grammar_plausible_raw_timestamp_shape <- function(df) {
  if (is.null(df) || ncol(df) == 0) return(FALSE)
  numeric_cols <- 0L
  plausible_cols <- 0L
  for (j in seq_len(ncol(df))) {
    x <- suppressWarnings(as.numeric(df[[j]]))
    x <- x[is.finite(x)]
    if (length(x) == 0) next
    numeric_cols <- numeric_cols + 1L
    # Raw spike columns are usually a reasonably long list of non-negative
    # timestamps. We do not require monotonic input because downstream code sorts,
    # but very tiny threshold tables with a handful of rows should not pass.
    if (length(x) >= 20L && all(x >= 0, na.rm = TRUE) && length(unique(x)) >= min(10L, length(x))) {
      plausible_cols <- plausible_cols + 1L
    }
  }
  plausible_cols > 0L && plausible_cols >= max(1L, ceiling(numeric_cols * 0.25))
}
