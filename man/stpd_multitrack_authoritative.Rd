\name{stpd_multitrack_authoritative}
\alias{stpd_multitrack_authoritative}
\alias{stpd_multitrack_authoritative_per_isi}
\title{Access the authoritative Gate B multi-track record}
\usage{
stpd_multitrack_authoritative(
  ds,
  source = c("preferred", "automatic", "reviewed")
)

stpd_multitrack_authoritative_per_isi(
  ds,
  source = c("preferred", "automatic", "reviewed")
)
}
\arguments{
\item{ds}{A detector dataset with a passing Gate B report.}
\item{source}{Choose the preferred, automatic, or reviewed v2 record.}
}
\value{
\code{stpd_multitrack_authoritative()} returns the validated record and its
Gate B attestation. \code{stpd_multitrack_authoritative_per_isi()} returns the
independent Event, State, Gap, and Review assignments for every ISI.
}
\description{
This is the supported read boundary for the promoted v2 representation. It
does not flatten overlapping Event and State evidence into the lossy legacy
single-label projection. Neither record is biological ground truth; Gate C is
required before detector-performance claims.
}
