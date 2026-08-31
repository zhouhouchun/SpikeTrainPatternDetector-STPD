\name{stpd_publication_readiness_review_receipts}
\alias{stpd_publication_readiness_review_receipts}
\title{Load Hash-Bound Publication-Readiness Review Receipts}
\usage{
stpd_publication_readiness_review_receipts(
  path = NULL,
  verify_source_reviews = c("auto", "always", "never")
)
}
\arguments{
  \item{path}{Optional path to an explicit review-receipt JSON file.}
  \item{verify_source_reviews}{Whether source-tree Markdown review records are
  checked when available (\code{"auto"}), always required
  (\code{"always"}), or not read (\code{"never"}). Framework and packet
  hashes are always verified.}
}
\value{
A validated list containing the twelve bounded framework-review receipts.
}
\description{
Verifies the installed receipt manifest against the framework and packet byte
hashes and the exact output/check/deferred bindings. The receipts explicitly
grant no scientific, release, or publication authority.
}
