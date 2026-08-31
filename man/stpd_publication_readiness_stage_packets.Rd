\name{stpd_publication_readiness_stage_packets}
\alias{stpd_publication_readiness_stage_packets}
\title{Load Publication-Readiness Stage Packets}
\usage{
stpd_publication_readiness_stage_packets(path = NULL)
}
\arguments{
  \item{path}{Optional path to an explicit stage-packet JSON file. By default,
  the installed packet registry is used.}
}
\value{
A validated list of twelve ordered minimum framework packets.
}
\description{
Loads the governance-only packet registry and verifies that every packet binds
exactly to its framework stage's required outputs, stop checks, and deferred
work. Packet review does not imply scientific implementation.
}
