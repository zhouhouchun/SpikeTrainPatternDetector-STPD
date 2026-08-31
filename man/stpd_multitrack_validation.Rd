\name{stpd_multitrack_validation}
\alias{stpd_multitrack_validation}
\title{Validate Multi-Track Intervals Against Track-Aware Truth}
\usage{
stpd_multitrack_validation(
  ds,
  truth_intervals,
  prediction_source = NULL,
  truth_role = NULL,
  selected_trains = NULL,
  iou_threshold = 0.5
)
}
\arguments{
  \item{ds}{A detector dataset containing the requested validated multi-track artifact.}
  \item{truth_intervals}{A long-form data frame with exactly the columns
  \code{truth_interval_id}, \code{train}, \code{semantic_track}, \code{label},
  \code{start_isi}, and \code{end_isi}. The first four columns must be character
  and the last two integer. Event, State, and Gap truth are evaluated separately;
  within-track overlaps are rejected and cross-track overlaps are allowed.}
  \item{prediction_source}{Required source. Use \code{"automatic_v2"} or
  \code{"reviewed_v2"} for Gate B records. The legacy
  \code{"automatic_preview"} and \code{"phase2b_final"} sources remain
  available only for explicit migration comparisons. No fallback is performed.}
  \item{truth_role}{For automatic Preview scoring, this must be
  \code{"independent_reference"}. Phase 2B final agreement accepts either
  \code{"independent_reference"} or \code{"adjudicated_reference"}.}
  \item{selected_trains}{Optional character vector defining an explicit subset
  of trains already present in the validated source artifact.}
  \item{iou_threshold}{Inclusive interval-IoU threshold in \eqn{(0,1]}.}
}
\value{
A fixed nine-component object containing \code{metadata},
\code{primary_predictions}, \code{truth_intervals}, \code{matches},
\code{metrics_by_label}, \code{metrics_by_track}, \code{coverage_metrics},
\code{review_candidates}, and \code{review_summary}. The metadata estimand is
the literal \code{"detector_performance"} for automatic sources and
\code{"adjudicated_agreement"} for reviewed sources.
}
\description{
This pure validation interface preserves the semantic separation of Event,
State, and Gap tracks. Review candidates are never included in primary
precision, recall, or F1. Automatic detector performance requires a strictly
validated, authoritative, label-blind Gate B AUTO record and independent truth.
A Gate B FINAL record must be current and authoritative and is always labelled
as adjudicated agreement, even when compared with an independent reference.
}
\details{
Matching is performed independently within each train, semantic track, and
label by ordered dynamic programming. The objective first maximizes the number
of matches, then total IoU, with deterministic tie-breaking. Metrics never pool
Event, State, and Gap. V1 reports raw counts without interval-level confidence
intervals and supports explicit train subsets; split-table and cluster-bootstrap
inference are deferred.

The estimands have deliberately narrow interpretations. Detector performance from
a label-blind Preview is not evidence of external generalization unless the data and
reference are genuinely independent. Phase 2B adjudicated agreement is not
label-blind detector accuracy and does not by itself establish biological validity.
Review target coverage is not Review precision, and interval rows are not independent
sampling units.
Neither estimand by itself establishes biological validity beyond the evaluated
reference and scope.

The selected train scope declares the set for which annotation is complete, including
fully annotated trains with zero positive truth intervals. The function does not infer
annotation completeness or silently narrow scope from truth rows. Predictions in
every selected train enter the denominators and, when unmatched, count as false
positives. Pass \code{selected_trains} explicitly when validating an annotated subset.
}
