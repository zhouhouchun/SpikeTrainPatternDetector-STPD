\name{stpd-final-audit}
\alias{stpd_apply_final_audit}
\alias{stpd_audit_final_labels}
\alias{stpd_clear_final_audit}
\alias{stpd_final_audit_events}
\alias{stpd_final_audit_event_history}
\alias{stpd_final_audit_history}
\alias{stpd_final_audit_summary}
\alias{stpd_promote_possible_labels}
\title{Final audit label layer for downstream spike-train analyses}
\usage{
stpd_apply_final_audit(ds, selected_trains = NULL,
  promote_possible = FALSE, promotion_map = NULL,
  min_isi_sec = 0.001, audit_id = NULL,
  reason = "final_audit_rebuild", user = NA_character_)

stpd_audit_final_labels(dat, min_isi_sec = 0.001,
  auto_others = FALSE, prefer_stored = TRUE,
  promote_possible = FALSE, promotion_map = NULL)

stpd_clear_final_audit(ds, selected_trains = NULL)

stpd_final_audit_summary(ds)

stpd_final_audit_events(ds)

stpd_final_audit_history(ds)

stpd_final_audit_event_history(ds)

stpd_promote_possible_labels(labels, promote_possible = TRUE,
  promotion_map = NULL)
}
\arguments{
  \item{ds}{A SpikeTrainPatternDetector dataset object containing a named \code{trains} list.}
  \item{selected_trains}{Optional train names to audit. When omitted, all trains are used.}
  \item{promote_possible}{Whether \code{possible_*} labels should be promoted to their corresponding real event labels in the final audit layer.}
  \item{promotion_map}{Optional named character vector mapping possible labels to final labels.}
  \item{min_isi_sec}{Minimum valid inter-spike interval in seconds.}
  \item{audit_id}{Optional identifier for the audit operation.}
  \item{reason}{Short reason stored in audit metadata.}
  \item{user}{Optional user name stored in audit metadata.}
  \item{dat}{A single spike-train data frame.}
  \item{auto_others}{Whether unlabeled valid intervals should be filled as \code{others} for display.}
  \item{prefer_stored}{Whether to use an existing \code{pattern_audit_final} column when it is present.}
  \item{labels}{Character vector of event labels.}
}
\description{
The final audit layer is an explicit downstream-analysis label layer. It
preserves the original AUTO and MANUAL labels, writes \code{pattern_audit_final}
and related audit columns, and optionally promotes review labels such as
\code{possible_burst} to real labels such as \code{burst}.
}
\details{
This layer is intended for analyses that should consume the investigator's final
review policy, including neural manifold summaries, state trajectories, event
tables, and machine-learning feature tables. It does not overwrite
\code{pattern_auto} or \code{pattern_manual}.
}
\value{
\code{stpd_apply_final_audit()} returns a list with \code{dataset},
\code{summary}, \code{events}, and \code{policy}. \code{stpd_audit_final_labels()}
and \code{stpd_promote_possible_labels()} return character vectors.
\code{stpd_clear_final_audit()} returns the updated dataset.
\code{stpd_final_audit_summary()} and \code{stpd_final_audit_events()} return
the current audit-state data frames. \code{stpd_final_audit_history()} and
\code{stpd_final_audit_event_history()} return append-only audit-history data
frames.
}
\examples{
dat <- data.frame(
  idx = 1:4,
  timestamp_sec = c(0, 0.01, 0.02, 0.5),
  ISI_sec = c(NA, 0.01, 0.01, 0.48),
  pattern_manual = "",
  pattern_auto = c("", "possible_burst", "possible_burst", "pause")
)
ds <- list(trains = list(train_1 = dat), results = list())
res <- stpd_apply_final_audit(ds, promote_possible = TRUE)
stpd_audit_final_labels(res$dataset$trains$train_1)
}
