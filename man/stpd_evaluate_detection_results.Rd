\name{stpd_evaluate_detection_results}
\alias{stpd_evaluate_detection_results}
\title{Evaluate detected events independently of the detector}
\usage{
stpd_evaluate_detection_results(
  predicted,
  reference,
  class_col = "pattern",
  iou_thresholds = c(0.25, 0.5, 0.75)
)
}
\arguments{
\item{predicted}{Event table containing \code{train}, the class column,
\code{start_isi}, and \code{end_isi}.}
\item{reference}{Reference event table with the same required columns.}
\item{class_col}{Name of the event-class column.}
\item{iou_thresholds}{Event IoU thresholds. The defaults represent approximate
detection, substantial overlap, and high overlap.}
}
\value{
A \code{stpd_detection_evaluation} list containing the fixed evaluation
protocol, event metrics, matched-event boundary details, pooled ISI metrics,
train-level ISI metrics, per-reference fragmentation details, and summarized
false-split metrics.
}
\description{
Compares fixed prediction and reference event tables using deterministic
one-to-one matching. This is a result-evaluation function: it does not execute
or modify the detector and cannot pass reference labels back to detection.
}
\details{
Event matching uses inclusive right-spike-index ISI intervals. Event discovery
and event-boundary accuracy are intentionally reported separately. For example,
a prediction covering three of the four ISIs in a five-spike reference Burst
has IoU 0.75 when it is a contained contiguous interval.

Fragmentation is independent of one-to-one IoU matching. For each reference
event, the evaluator counts distinct prediction rows of the same train and
class having any positive ISI overlap. Two or more such rows constitute a
false split. The all-reference rate includes missed references in its
denominator; the detected-reference rate is additionally reported and is
undefined when no reference event has any overlapping prediction.
}
