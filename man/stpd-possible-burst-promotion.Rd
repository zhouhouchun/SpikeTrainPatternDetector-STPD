\name{stpd-possible-burst-promotion}
\alias{stpd-possible-burst-promotion}
\alias{stpd_possible_burst_promotion_preview}
\alias{stpd_promote_possible_burst}
\alias{stpd_revert_possible_burst_promotions}
\title{Controlled promotion of possible_burst review candidates}
\description{
Preview, promote, and revert user-reviewed \code{possible_burst} candidates.
The promotion is written to the user-review/manual layer so that final labels
become \code{burst}, while the automatic detector label remains auditable.
}
\usage{
stpd_possible_burst_promotion_preview(
  ds,
  selected_trains = NULL,
  overwrite_manual = FALSE
)

stpd_promote_possible_burst(
  ds,
  selected_trains = NULL,
  overwrite_manual = FALSE,
  reason = "user_promoted_possible_burst",
  user = NA_character_,
  audit_id = NULL
)

stpd_revert_possible_burst_promotions(
  ds,
  selected_trains = NULL,
  protect_manual_edits = TRUE,
  reason = NULL
)
}
\arguments{
\item{ds}{A SpikeTrainPatternDetector dataset.}
\item{selected_trains}{Optional character vector of train names. Defaults to all trains.}
\item{overwrite_manual}{Logical; whether existing manual or manual-negative labels may be overwritten. The default protects them.}
\item{reason}{Audit reason for promotion, or an optional reason filter for reversion.}
\item{user}{Optional user/operator name recorded in the audit table.}
\item{audit_id}{Optional stable operation identifier.}
\item{protect_manual_edits}{Logical; when reverting, keep rows whose manual label was changed after the promotion.}
}
\value{
\code{stpd_possible_burst_promotion_preview()} returns a list with
\code{summary}, \code{events}, \code{labels}, and total eligible counts.
\code{stpd_promote_possible_burst()} and
\code{stpd_revert_possible_burst_promotions()} return lists containing the
modified \code{dataset}, \code{summary}, and \code{audit}.
}
\details{
The functions implement a review-layer workflow:
\code{pattern_auto} is preserved, \code{pattern_auto_original} stores the
algorithmic label, and \code{pattern_user_override_*} columns record the user
decision. The current final-label machinery sees the promoted interval through
\code{pattern_manual = "burst"}.
}
\seealso{
\code{\link{stpd_event_level_validation_report}},
\code{\link{stpd_scientific_validation_report}}
}
