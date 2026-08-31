\name{provider_adjudication_v2}
\alias{stpd_new_provider_adjudication}
\alias{stpd_provider_review_request}
\alias{stpd_apply_provider_adjudication}
\alias{stpd_replay_provider_adjudication}
\alias{stpd_validate_provider_adjudication}
\alias{stpd_write_provider_adjudication}
\title{Provider-agnostic append-only adjudication v2}
\usage{
stpd_new_provider_adjudication(provider_bundle)

stpd_provider_review_request(
  adjudication, action, provider_run_id, source_record_id, operation_id,
  reviewer_id, reason, decided_utc, adjusted_start_isi = NULL,
  adjusted_end_isi = NULL, target_decision_id = NULL
)

stpd_apply_provider_adjudication(
  provider_bundle, adjudication, request, dataset = NULL
)

stpd_replay_provider_adjudication(provider_bundle, history)

stpd_validate_provider_adjudication(provider_bundle, adjudication)

stpd_write_provider_adjudication(provider_bundle, adjudication, out_dir)
}
\arguments{
  \item{provider_bundle}{A validated immutable provider bundle v1.}
  \item{adjudication}{A provider adjudication v2 descendant.}
  \item{action}{One supported v2 action.}
  \item{provider_run_id, source_record_id}{The immutable provider target.}
  \item{operation_id}{A caller-generated idempotency key.}
  \item{reviewer_id}{A reviewer pseudonym, not direct identity.}
  \item{reason}{A non-empty audit reason.}
  \item{decided_utc}{An explicit RFC3339 UTC audit time.}
  \item{adjusted_start_isi, adjusted_end_isi}{Canonical bounds supplied only
  for \code{adjust_bounds}.}
  \item{target_decision_id}{The current decision supplied only for
  \code{void_prior_decision}.}
  \item{request}{An exact ordered
  \code{stpd_provider_adjudication_request_v2} list.}
  \item{dataset}{The immutable source dataset, required by
  \code{adjust_bounds}.}
  \item{history}{A v2 append-only history table.}
  \item{out_dir}{A new export directory; existing paths fail closed.}
}
\value{
The constructor, apply, and replay functions return a separately hashed v2
product. Validation returns \code{TRUE} invisibly. Export returns the normalized
output directory invisibly.
}
\description{
Reviews normalized results from any provider without modifying provider AUTO
bytes or hashes. Supported actions are accept, reject, boundary adjustment, and
append-only compensation. Replay verifies the parent, request, source,
transition, and product hash chains.
}
\details{
Boundary adjustment inherits the source train, semantic track, and label. It
requires the exact source timestamp train and cannot cross its canonical
acquisition boundary. Split, merge, relabel, and create are intentionally not
supported in this contract version.
}
