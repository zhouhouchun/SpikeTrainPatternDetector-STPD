\name{stpd_multitrack_final}
\alias{stpd_multitrack_final}
\title{Access the Gate-B reviewed multi-track product}
\usage{
stpd_multitrack_final(ds)
}
\arguments{
  \item{ds}{A detector dataset containing \code{results$multitrack_final},
  or a \code{stpd_multitrack_final_product}.}
}
\value{A strictly validated \code{stpd_multitrack_final_product}.}
\description{
The v2 FINAL product binds one exact \code{multitrack_auto} parent and replays
only one-to-one, train-qualified, geometry-identical decisions imported from
the immutable Phase 2B transition history. Each imported row retains its
legacy transition SHA-256. A stale or ambiguous legacy history produces a
typed \code{requires_readjudication} product with empty scientific tables;
it is never silently replayed.
}
\details{
Confirmed Review candidates create or link to one canonical Burst Event.
They never mutate State or Gap intervals. Event--State relationships are
fully recomputed, so an HFS State and an overlapping Burst Event coexist
non-destructively. The per-ISI table is fully materialized and checked in both
directions against the interval products.

Before the independent checker passes, the product remains non-authoritative.
After all twelve Gate B checks pass, an exactly materialized product receives
\code{authority_scope = "reviewed_prediction_record"}. A product requiring
readjudication remains non-authoritative. Neither state is biological ground
truth or eligible for detector-performance claims.
}
