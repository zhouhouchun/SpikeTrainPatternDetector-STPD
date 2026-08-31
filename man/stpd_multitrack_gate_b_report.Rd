\name{stpd_multitrack_gate_b_report}
\alias{stpd_multitrack_gate_b_report}
\title{Access the Gate B multi-track runtime attestation}
\usage{
stpd_multitrack_gate_b_report(ds)
}
\arguments{
  \item{ds}{A detector dataset containing a Gate B report.}
}
\value{A validated \code{stpd_multitrack_gate_b_report}.}
\description{
Returns the twelve-condition Gate B runtime attestation and the explicit
legacy-v2 count and ISI-duration difference audit. Passing Gate B establishes
representation authority only. It never sets biological ground truth and it
does not make detector-performance estimates eligible; those remain blocked
until Gate C.
}
\details{
The legacy single-label output is retained as
\code{legacy_lossy_projection} during the declared dual-write migration
period. Differences are recorded rather than treated as equality failures.
Persisted review identities are either migrated exactly with their legacy
transition SHA-256 or marked \code{requires_readjudication}.
}
