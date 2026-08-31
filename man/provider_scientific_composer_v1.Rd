\name{provider_scientific_composer_v1}
\alias{stpd_provider_composer_decision}
\alias{stpd_compose_provider_science}
\alias{stpd_validate_provider_composition}
\title{Provider-independent scientific composition v1}
\usage{
stpd_provider_composer_decision(
  provider_bundle, provider_run_id, source_mode = c("auto", "adjudicated"),
  adjudication = NULL, scientific_owner, rationale, decided_utc
)

stpd_compose_provider_science(
  provider_bundle, decision, adjudication = NULL
)

stpd_validate_provider_composition(
  provider_bundle, product, adjudication = NULL, rematerialize = TRUE
)
}
\arguments{
  \item{provider_bundle}{A validated immutable provider bundle v1.}
  \item{provider_run_id}{Exactly one provider run selected for composition.}
  \item{source_mode}{Either \code{"auto"} or \code{"adjudicated"}.}
  \item{adjudication}{The exact Phase C adjudication v2 descendant when
  \code{source_mode = "adjudicated"}.}
  \item{scientific_owner}{A bounded scientific-owner pseudonym.}
  \item{rationale}{A non-empty audit rationale.}
  \item{decided_utc}{An explicit RFC3339 UTC audit time.}
  \item{decision}{A hashed one-row record returned by
  \code{stpd_provider_composer_decision()}.}
  \item{product}{A provider scientific composition v1 product.}
  \item{rematerialize}{Whether validation must deterministically rebuild and
  compare the complete product.}
}
\value{
The decision function returns a hashed one-row decision. The composition
function returns a descriptive \code{stpd_provider_scientific_composition_v1}
product. Validation returns \code{TRUE} invisibly or fails closed.
}
\description{
Adds provider-independent Event/State/Gap relationships and descriptive
recurrent Regimes for one explicitly selected provider run without rerunning
detection, fusing providers, modifying parent artifacts, or replacing child
geometry.
}
\details{
AUTO composition is restricted to automatic-prediction authority.
Candidate-support intervals require explicit Phase C acceptance. Burst Events
may overlap Tonic or Broad-HFS States. HFT and HF-irregular require exact-support
Broad-HFS parents. Canonical Pause/Broad-HFS overlap is reported as a conflict,
not silently resolved. Recurrent Bursting requires three selected Burst Events;
Recurrent Pausing requires five selected Pause Gaps. Regimes are one-pass,
nonrecursive, descriptive, and never biological ground truth.
}
