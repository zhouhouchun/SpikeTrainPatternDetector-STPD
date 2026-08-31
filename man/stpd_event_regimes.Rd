\name{stpd_event_regimes}
\alias{stpd_event_regimes}
\title{Access descriptive recurrent parent-State candidates}
\usage{
stpd_event_regimes(ds)
}
\arguments{
  \item{ds}{A detector dataset containing \code{results$event_regimes},
  or an \code{stpd_event_regime_product}.}
}
\value{A strictly validated descriptive recurrent parent-State product.}
\description{
Returns the non-authoritative parent-State layer that summarizes recurrent
Burst or Pause Events while preserving every canonical child Event or Gap.
}
\details{
The product is derived only after canonical FINAL materialization. The
three-long-Pause and five-ordinary-accepted-Pause rules therefore aggregate
already accepted Pause entities; they do not discover, accept, reject, or
retype an individual Pause. Likewise, recurrent Bursting references canonical Burst
Events without merging their cores across interruptions.

The resulting \code{recurrent_bursting_state} and
\code{recurrent_pause_state} rows are descriptive, unvalidated parent-State
candidates. Tonic and Broad-HFS remain independent carrier States and may
overlap these parent States. The product is not
biological ground truth and must not be used for performance claims without
an independent reference annotation.
}
