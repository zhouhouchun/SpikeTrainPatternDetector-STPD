# ============================================================
# Public classification review fixes
# ------------------------------------------------------------
# Public API boundary hardening for final classification:
#   - Diagnostic/rejected/profile/not-selected windows must remain in
#     candidate_diagnostic_audit only.
#   - candidate_ledger -> candidate_features -> final_decisions must never
#     classify rows whose reject semantics appear in raw_candidate_class,
#     action, decision_path or rejection_reason, even if final_candidate_class
#     is accidentally set to a biological label.
#   - final_classify_candidate() and final_classify_candidates() now both
#     return zero-row schema tables for rejected diagnostic inputs.
# ============================================================

stpd_event_grammar_empty_single_final_classification <- function() {
  data.frame(
    final_class = character(), confidence_tier = character(), review_required = logical(),
    decision_reason = character(), biological_warning = character(),
    recommended_family = character(), recommended_subtype = character(),
    recommended_final_class = character(), decision_path = character(),
    eventness_zone = character(), recommended_review_required = logical(),
    recommendation_confidence_tier = character(), recommended_uncertainty_reason = character(),
    long_burst_definition_status = character(),
    stringsAsFactors = FALSE
  )
}

stpd_event_grammar_chr_lower <- function(x) {
  x <- as.character(x %||% "")
  x[is.na(x)] <- ""
  tolower(trimws(x))
}

stpd_event_grammar_text_is_reject_semantic <- function(x, empty_is_reject = FALSE) {
  y <- stpd_event_grammar_chr_lower(x)
  if (length(y) == 0) return(FALSE)
  y <- gsub("\\s+", "_", y)
  empty_bad <- empty_is_reject & (!nzchar(y) | y %in% c("na", "nan", "n_a", "null"))
  explicit_bad <- y %in% c(
    "reject", "rejected", "profile", "diagnostic", "diagnostics",
    "not_selected", "not-selected", "notselected", "unselected",
    "unlabeled", "unlabelled", "background", "candidate_only",
    "audit_only", "review_only", "unwritten", "not_written",
    "blocked", "veto", "vetoed", "discard", "discarded",
    "drop", "dropped", "remove", "removed", "failed", "failure",
    "manual_lock", "manual_lock_trimmed", "blocked_by_manual_label",
    "auto_write_blocked_by_manual_lock"
  )
  regex_bad <- grepl(
    "(^|[_ -])(reject|rejected|profile|diagnostic|not[_ -]?selected|unselected|unlabeled|unlabelled|unwritten|not[_ -]?written|blocked|blocked[_ -]?by[_ -]?manual|manual[_ -]?lock|auto[_ -]?write[_ -]?blocked[_ -]?by[_ -]?manual|veto|vetoed|discard|discarded|dropped|removed|failed|failure)([_ -]|$)",
    y
  )
  any(empty_bad | explicit_bad | regex_bad, na.rm = TRUE)
}

stpd_event_grammar_rejection_reason_active <- function(x) {
  y <- stpd_event_grammar_chr_lower(x)
  if (length(y) == 0) return(FALSE)
  neutral <- !nzchar(y) | y %in% c(
    "na", "nan", "n_a", "none", "no", "null", "ok", "pass", "passed",
    "accept", "accepted", "selected", "kept", "keep", "written",
    "no_rejection", "not_applicable", "n/a"
  )
  any(!neutral, na.rm = TRUE)
}

stpd_event_grammar_logical_false_present <- function(x) {
  if (is.null(x)) return(rep(FALSE, 0L))
  if (is.logical(x)) return(!is.na(x) & x == FALSE)
  y <- stpd_event_grammar_chr_lower(x)
  y %in% c("false", "f", "0", "no", "n")
}

stpd_event_grammar_public_candidate_keep_mask <- function(tbl) {
  f <- tibble::as_tibble(tbl %||% data.frame())
  n <- nrow(f)
  if (n == 0) return(logical(0))
  keep <- rep(TRUE, n)

  # Explicit public-output flags.  If present and FALSE, the row is diagnostic.
  if ("written_to_auto" %in% names(f)) {
    keep <- keep & !stpd_event_grammar_logical_false_present(f$written_to_auto)
  }
  if ("selected_for_auto" %in% names(f)) {
    keep <- keep & !stpd_event_grammar_logical_false_present(f$selected_for_auto)
  }
  if ("visible_in_raster" %in% names(f)) {
    keep <- keep & !stpd_event_grammar_logical_false_present(f$visible_in_raster)
  }

  # Class-like columns: reject/profile/unlabeled semantics are not public final
  # candidates.  Empty raw/class fields are ignored so older feature tables that
  # lack those columns can still be classified if another valid class exists.
  class_cols <- intersect(
    c("final_candidate_class", "raw_candidate_class", "candidate_class", "class", "final_label", "final_class"),
    names(f)
  )
  for (nm in class_cols) {
    x <- f[[nm]]
    bad <- vapply(x, function(v) stpd_event_grammar_text_is_reject_semantic(v, empty_is_reject = FALSE), logical(1))
    keep <- keep & !bad
  }

  # Action/path/status columns can carry reject semantics even when the final
  # candidate class has accidentally been set to burst/HF/tonic.
  semantic_cols <- intersect(
    c("action", "policy_action", "decision_path", "gate_status", "selection_status", "status"),
    names(f)
  )
  for (nm in semantic_cols) {
    x <- f[[nm]]
    bad <- vapply(x, function(v) stpd_event_grammar_text_is_reject_semantic(v, empty_is_reject = FALSE), logical(1))
    keep <- keep & !bad
  }

  # rejection_reason is stricter: any non-neutral reason means the row is audit
  # diagnostic, not a public final-classification candidate.
  if ("rejection_reason" %in% names(f)) {
    bad_reason <- vapply(f$rejection_reason, stpd_event_grammar_rejection_reason_active, logical(1))
    keep <- keep & !bad_reason
  }

  keep
}

stpd_event_grammar_filter_public_candidate_features <- function(features) {
  f <- tibble::as_tibble(features %||% stpd_event_grammar_empty_candidate_features())
  if (nrow(f) == 0) return(f)
  f[stpd_event_grammar_public_candidate_keep_mask(f), , drop = FALSE]
}

build_candidate_ledger_internal <- function(ds, params, selected_trains = NULL, run_id = NULL, params_hash = NULL) {
  led <- if (stpd_event_grammar_has_candidate_audit(ds)) {
    stpd_candidate_diagnostic_audit_to_ledger(ds, params, selected_trains = selected_trains, run_id = run_id, params_hash = params_hash)
  } else {
    build_candidate_ledger_from_result_tables(ds, params, selected_trains = selected_trains, run_id = run_id, params_hash = params_hash)
  }
  if (is.null(led) || nrow(led) == 0) return(stpd_event_grammar_empty_candidate_ledger())
  led <- tibble::as_tibble(led)
  led <- led[stpd_event_grammar_public_candidate_keep_mask(led), , drop = FALSE]
  if (nrow(led) == 0) stpd_event_grammar_empty_candidate_ledger() else led
}

# Keep the public alias synchronized because it is assigned earlier in Collate.
build_candidate_ledger <- build_candidate_ledger_internal

compute_candidate_features_internal <- function(ds, ledger = NULL, params = NULL, selected_trains = NULL) {
  if (!is.null(ledger) && nrow(ledger) > 0) {
    ledger <- tibble::as_tibble(ledger)
    ledger <- ledger[stpd_event_grammar_public_candidate_keep_mask(ledger), , drop = FALSE]
  }
  out <- compute_candidate_features_core(ds, ledger = ledger, params = params, selected_trains = selected_trains)
  if (is.null(out) || nrow(out) == 0) return(stpd_event_grammar_empty_candidate_features())
  out <- tibble::as_tibble(out)
  out <- out[stpd_event_grammar_public_candidate_keep_mask(out), , drop = FALSE]
  if (nrow(out) == 0) stpd_event_grammar_empty_candidate_features() else out
}

stpd_enhance_candidate_features_eventness <- function(ds, features, params = default_params_sec(), selected_trains = NULL) {
  if (is.null(features) || nrow(features) == 0 || is.null(ds) || is.null(ds$trains)) {
    return(stpd_event_grammar_add_eventness_schema_cols(features))
  }
  f <- stpd_event_grammar_filter_public_candidate_features(features)
  if (nrow(f) == 0) return(stpd_event_grammar_add_eventness_schema_cols(f))
  out <- stpd_enhance_candidate_features_eventness_core(ds, f, params = params, selected_trains = selected_trains)
  if (is.null(out) || nrow(out) == 0) return(stpd_event_grammar_add_eventness_schema_cols(f))
  stpd_event_grammar_add_eventness_schema_cols(out)
}

final_classify_candidate <- function(feature, params = default_params_sec(), preserve_detector_class = TRUE) {
  f <- tibble::as_tibble(feature %||% data.frame())
  if (nrow(f) == 0) return(stpd_event_grammar_empty_single_final_classification())
  if (!stpd_event_grammar_public_candidate_keep_mask(f)[1]) return(stpd_event_grammar_empty_single_final_classification())
  out <- final_classify_candidate_core(f[1, , drop = FALSE], params = params, preserve_detector_class = preserve_detector_class)
  if (is.null(out) || nrow(out) == 0) return(stpd_event_grammar_empty_single_final_classification())
  if ("final_class" %in% names(out)) {
    bad <- vapply(out$final_class, function(v) stpd_event_grammar_text_is_reject_semantic(v, empty_is_reject = FALSE), logical(1))
    out <- out[!bad, , drop = FALSE]
  }
  if (is.null(out) || nrow(out) == 0) stpd_event_grammar_empty_single_final_classification() else out
}

final_classify_candidates <- function(features, params = default_params_sec(), preserve_detector_class = TRUE) {
  f0 <- tibble::as_tibble(features %||% stpd_event_grammar_empty_candidate_features())
  if (nrow(f0) == 0) return(stpd_event_grammar_empty_final_decisions(f0))

  f <- stpd_event_grammar_filter_public_candidate_features(f0)
  if (nrow(f) == 0) return(stpd_event_grammar_empty_final_decisions(f0[0, , drop = FALSE]))

  out <- final_classify_candidates_core(f, params = params, preserve_detector_class = preserve_detector_class)
  if (is.null(out) || nrow(out) == 0) return(stpd_event_grammar_empty_final_decisions(f[0, , drop = FALSE]))
  out <- tibble::as_tibble(out)
  out <- out[stpd_event_grammar_public_candidate_keep_mask(out), , drop = FALSE]
  if ("final_class" %in% names(out)) {
    bad_final <- vapply(out$final_class, function(v) stpd_event_grammar_text_is_reject_semantic(v, empty_is_reject = FALSE), logical(1))
    out <- out[!bad_final, , drop = FALSE]
  }
  if (nrow(out) == 0) stpd_event_grammar_empty_final_decisions(f[0, , drop = FALSE]) else out
}

stpd_result_consistency_check <- function(ds) {
  out <- stpd_result_consistency_check_core(ds)
  issues <- list()
  add <- function(severity, component, issue, detail = "") {
    issues[[length(issues) + 1L]] <<- data.frame(severity = severity, component = component, issue = issue, detail = detail, stringsAsFactors = FALSE)
  }
  if (!is.null(ds) && !is.null(ds$results)) {
    for (nm in c("candidate_ledger", "candidate_features", "eventness_audit", "final_decisions", "final_classification_audit")) {
      tab <- ds$results[[nm]]
      if (!is.null(tab) && nrow(tab) > 0) {
        keep <- stpd_event_grammar_public_candidate_keep_mask(tab)
        if (any(!keep, na.rm = TRUE)) {
          add("error", nm, "reject/diagnostic rows in public table", "Rows carrying reject/profile/not-selected/action/path/rejection_reason semantics must remain in candidate_diagnostic_audit only.")
        }
        if ("final_class" %in% names(tab)) {
          bad_final <- vapply(tab$final_class, function(v) stpd_event_grammar_text_is_reject_semantic(v, empty_is_reject = FALSE), logical(1))
          if (any(bad_final, na.rm = TRUE)) {
            add("error", nm, "reject final_class in public table", "final_class must be a biological/public class, not reject/profile/diagnostic.")
          }
        }
      }
    }
    dec <- ds$results$final_decisions %||% ds$results$final_classification_audit %||% data.frame()
    if (!is.null(dec) && nrow(dec) > 0) {
      if ("final_class" %in% names(dec) && all(is.na(dec$final_class) | !nzchar(as.character(dec$final_class)))) {
        add("error", "final_decisions", "all final classes are empty", "This usually indicates rejected diagnostic rows leaked into the public classification table or final classification was called on invalid inputs.")
      }
      if ("selection_status" %in% names(dec)) {
        ss <- tolower(trimws(as.character(dec$selection_status)))
        if (any(grepl("reject|not[_ -]?selected|diagnostic|profile", ss), na.rm = TRUE)) {
          add("error", "final_decisions", "diagnostic selection statuses in final decisions", "Rejected/profile/not-selected rows must remain in candidate_diagnostic_audit only.")
        }
      }
    }
  }
  if (length(issues) > 0) {
    new <- dplyr::bind_rows(issues)
    if (is.null(out) || nrow(out) == 0 || ("severity" %in% names(out) && nrow(out) == 1 && out$severity[1] == "ok")) return(new)
    return(dplyr::bind_rows(out[out$severity != "ok", , drop = FALSE], new))
  }
  out
}
