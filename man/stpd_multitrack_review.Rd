\name{stpd_multitrack_review}
\alias{stpd_multitrack_review_preflight}
\alias{stpd_multitrack_review_apply}
\alias{stpd_multitrack_review_confirm}
\alias{stpd_multitrack_review_revoke}
\alias{stpd_multitrack_review_history}
\alias{stpd_multitrack_review_product}
\alias{stpd_write_multitrack_review}
\title{Apply and Export Auditable Multi-Track Review Decisions}
\usage{
stpd_multitrack_review_preflight(
  ds,
  requests,
  action = c("confirm", "revoke")
)

stpd_multitrack_review_apply(
  ds,
  requests,
  action = c("confirm", "revoke"),
  expected_precondition_sha256,
  reviewer,
  reason,
  operation_id = NULL
)

stpd_multitrack_review_confirm(
  ds,
  requests,
  expected_precondition_sha256,
  reviewer,
  reason,
  operation_id = NULL
)

stpd_multitrack_review_revoke(
  ds,
  requests,
  expected_precondition_sha256,
  reviewer,
  reason,
  operation_id = NULL
)

stpd_multitrack_review_history(ds)

stpd_multitrack_review_product(ds)

stpd_write_multitrack_review(ds, out_dir)
}
\arguments{
  \item{ds}{A detector dataset containing a strictly validated Phase 2A Preview
  and, when applicable, a Phase 2B review product.}
  \item{requests}{A data frame with exactly \code{train} and
  \code{source_review_interval_id}.}
  \item{action}{The immutable transition action, \code{"confirm"} or
  \code{"revoke"}.}
  \item{expected_precondition_sha256}{The unchanged SHA-256 returned by the
  matching preflight call.}
  \item{reviewer}{A non-empty reviewer identity.}
  \item{reason}{A non-empty scientific or audit reason.}
  \item{operation_id}{Optional idempotency key.}
  \item{out_dir}{Directory shared with the legacy and Preview export bundle.}
}
\value{
\code{stpd_multitrack_review_preflight()} returns eligibility decisions and a
precondition hash. Mutation helpers return the updated dataset, appended
transitions, validated product, and preflight evidence. Accessors return validated
history or product data. The writer returns an invisible status-bearing named
character vector of written paths.
}
\description{
These functions implement the Phase 2B train-qualified Review-to-Event workflow.
Confirmation preserves the source Review hypothesis and creates or links one
Event/Burst decision in a separate manual/final layer. Transition history is
append-only and bound to the exact parent Preview.
}
\details{
Always run preflight immediately before apply, confirm, or revoke, and pass the
returned precondition hash unchanged. Multi-request operations are atomic. An
exact-span existing Burst is linked rather than duplicated; Pause, unresolved
tonic/HF-tonic overlap, ambiguous Event overlap, and manual not-burst vetoes fail
closed. HFS can coexist with the confirmed Burst. The writer validates the complete
product before publishing fixed-path CSV and canonical typed RDS artifacts.
}
