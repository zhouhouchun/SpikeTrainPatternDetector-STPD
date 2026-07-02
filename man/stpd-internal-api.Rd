\name{stpd-internal-api}
\alias{stpd_threshold_patterns}
\alias{stpd_threshold_pattern_label}
\alias{stpd_threshold_pattern_color}
\alias{stpd_resolve_thresholds_for_dataset}
\alias{stpd_attach_thresholds_to_params}
\alias{stpd_event_grammar_params}
\alias{stpd_event_core_params}
\alias{stpd_detect_train_event_grammar}
\alias{stpd_detect_train_core}
\alias{stpd_detect_dataset_core}
\alias{stpd_empty_candidate_ledger}
\alias{stpd_empty_candidate_features}
\alias{stpd_empty_final_decisions}
\alias{stpd_review_reject_labels}
\alias{stpd_is_reject_like}
\alias{stpd_candidate_audit_to_ledger}
\alias{stpd_canonicalize_result_names}
\alias{stpd_public_params_only}
\title{Version-neutral detector internals}
\usage{
stpd_threshold_patterns()
stpd_threshold_pattern_label(pattern)
stpd_threshold_pattern_color(pattern, source = "manual")
stpd_resolve_thresholds_for_dataset(trains, params, min_isi_sec = 0.001,
  bin_width_sec = 0.005)
stpd_attach_thresholds_to_params(params, ds = NULL, min_isi_sec = NULL,
  bin_width_sec = NULL)
stpd_event_grammar_params(dat, params, min_isi_sec = 0.001, train = "")
stpd_event_core_params(dat, params, min_isi_sec = 0.001)
stpd_detect_train_event_grammar(dat, params, min_isi_sec = 0.001,
  train = "", lock_manual = TRUE)
stpd_detect_train_core(dat, params, min_isi_sec = 0.001,
  train = "", lock_manual = TRUE)
stpd_detect_dataset_core(ds, params, selected_trains = NULL,
  lock_manual = TRUE, collect_diagnostics = TRUE, progress_callback = NULL)
stpd_empty_candidate_ledger()
stpd_empty_candidate_features()
stpd_empty_final_decisions(features = NULL)
stpd_review_reject_labels()
stpd_is_reject_like(x)
stpd_candidate_audit_to_ledger(ds, params, selected_trains = NULL,
  run_id = NULL, params_hash = NULL)
stpd_canonicalize_result_names(ds, drop_legacy = TRUE)
stpd_public_params_only(params)
}
\arguments{
  \item{pattern}{Pattern name.}
  \item{source}{Threshold or label source.}
  \item{trains}{Named list of train data frames.}
  \item{params}{Detector parameter list, preferably using \code{params$spiketrainpattern}.}
  \item{min_isi_sec}{Minimum valid ISI in seconds.}
  \item{bin_width_sec}{Histogram bin width in seconds.}
  \item{ds}{Detector dataset object.}
  \item{dat}{Single train data frame.}
  \item{train}{Train name.}
  \item{lock_manual}{Whether manual labels prevent AUTO labels on the same ISIs.}
  \item{selected_trains}{Optional selected train names.}
  \item{collect_diagnostics}{Whether to collect diagnostic candidate tables.}
  \item{progress_callback}{Optional callback for detector progress reporting.}
  \item{features}{Candidate feature table.}
  \item{x}{Character vector or object to classify as reject-like.}
  \item{run_id}{Optional run identifier.}
  \item{params_hash}{Optional parameter hash.}
  \item{drop_legacy}{Whether to remove version-marked result fields and train attributes after stable aliases are created.}
}
\description{
Version-neutral wrappers for the active spike-train pattern detector internals.
New code should prefer these names over versioned implementation names.
}
\details{
\code{stpd_canonicalize_result_names()} adds stable result aliases such as
\code{threshold_table}, \code{candidate_diagnostic_audit},
\code{candidate_features}, \code{final_decisions}, \code{eventness_audit}, and
\code{run_metadata_public}. Version-marked fields are removed from normal detector
results.
}
\value{
The threshold helpers return threshold metadata, the detector helpers return
train or dataset objects, and the empty-table helpers return schema-preserving
zero-row data frames.
}
