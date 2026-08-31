\name{stpd_publication_readiness_framework}
\alias{stpd_publication_readiness_framework}
\title{Load the STPD Publication-Readiness Framework}
\usage{
stpd_publication_readiness_framework(path = NULL)
}
\arguments{
  \item{path}{Optional path to an explicit framework JSON file. By default,
  the installed framework is used.}
}
\value{
A validated list describing the twelve serial, non-authoritative framework
stages. When its state is \code{FRAMEWORK_COMPLETE}, packet and review-receipt
hash closure is also required.
}
\description{
Loads and validates the governance-only publication-readiness framework. The
result does not grant scientific, Gate B, release, or publication authority.
}
