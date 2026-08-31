\name{stpd_import_provider_batch}
\alias{stpd_import_provider_batch}
\title{Import an Allowlisted Provider Result as an Auditable Bundle}
\usage{
stpd_import_provider_batch(dataset, request, artifact = NULL)
}
\arguments{
  \item{dataset}{An STPD dataset. Only the selected trains and their raw
  timestamp spines are exposed to the provider adapter.}
  \item{request}{An unclassed named list following
  \code{stpd_provider_import_request_v1}. Its fields, in order, are
  \code{schema_version}, \code{adapter_key}, \code{adapter_version},
  \code{output_role}, \code{generation_mode},
  \code{selected_train_keys}, and \code{parameters}.}
  \item{artifact}{For an external adapter, an unclassed raw vector containing
  its exact UTF-8 JSON artifact. For the internal Mean-ISI and LogISI/newBD
  adapters, this must be \code{NULL}.}
}
\value{
A new, validated five-table provider bundle. The input \code{dataset} is not
modified. A deterministically identifiable record-local normalization failure,
provider-level unresolved threshold, or semantic failure returns a bundle whose
provider run is \code{"rejected"}, with every available audit row but no
candidate or threshold rows. The audit table may be empty when the provider
fails before producing source records. Systemic schema, identity, coverage,
executable input, or resource failures raise a typed
\code{stpd_provider_contract_error} and return no partial object.
}
\description{
Runs one frozen provider adapter against a selected timestamp spine and imports
its result through the coordinate-normalization, provenance, authority, and
atomic-publication contracts. Provider assertions remain separate records;
this function does not fuse providers, apply manual edits, or promote support
evidence to biological ground truth.
}
\details{
Phase B2 enables the internal Mean-ISI and LogISI/newBD adapters and the fixed
external interval and binary per-ISI adapters. Each enabled adapter can emit an
\code{automatic_prediction} or \code{candidate_support} bundle through the
\code{external_only} generation mode. The Native STPD adapter and calibration
routes remain disabled until their explicit scientific authority gates pass.

The external schemas accept only fixed coordinate-profile identifiers and
pure JSON data. They do not accept R objects, code, coordinate formulas, or a
caller assertion that validation has already occurred. Mean-ISI and
LogISI/newBD run directly on each selected train's raw timestamp spine and do
not read manual labels, review state, or native candidate ledgers.

See \file{docs/provider-adapters-phase-b2.md} in the source distribution for
the exact request, artifact, route, parameter, and rejection contracts.
}
\examples{
\dontrun{
request <- list(
  schema_version = "stpd_provider_import_request_v1",
  adapter_key = "mean_isi_v1",
  adapter_version = "1.0.0",
  output_role = "candidate_support",
  generation_mode = "external_only",
  selected_train_keys = "train_1",
  parameters = list(min_spikes = 4L)
)
support_bundle <- stpd_import_provider_batch(dataset, request)
}
}
