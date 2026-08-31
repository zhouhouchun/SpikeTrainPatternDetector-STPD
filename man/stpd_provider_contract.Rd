\name{stpd_provider_contract}
\alias{stpd_provider_contract_versions}
\alias{stpd_provider_contract_registry}
\alias{stpd_provider_bundle_prototypes}
\alias{stpd_provider_contract_hash}
\alias{stpd_validate_provider_bundle}
\title{Inspect and validate the STPD provider-bundle v1 contract}
\usage{
stpd_provider_contract_versions()

stpd_provider_contract_registry()

stpd_provider_bundle_prototypes()

stpd_provider_contract_hash()

stpd_validate_provider_bundle(bundle)
}
\arguments{
  \item{bundle}{A named list containing the five provider-bundle v1 tables.}
}
\value{
The version function returns three named version strings. The registry returns
the frozen schema, allowlist, ontology, coordinate, authority, hashing, and
error-code declarations, including capability-specific labels, calibration
taint, identity preimages, and the hash DAG. The prototype function returns five zero-row typed
data frames. The hash function returns a full lowercase SHA-256 contract hash.
The validator returns \code{invisible(TRUE)} or raises a typed
\code{stpd_provider_contract_error}; it never mutates \code{bundle}.
}
\description{
Phase A defines a provider-neutral interoperability boundary for native STPD,
Mean-ISI, LogISI/newBD, and allowlisted pure-data external results. Provider
automatic output, calibration, later human adjudication, and independent
evaluation reference truth remain separate authorities. This API declares and
validates the contract only; it does not execute adapters or imported code.
Mean-ISI and LogISI/newBD are restricted to Burst semantics. HFT and
HF-irregular require an equal-geometry Broad-HFS parent. Phase A records
calibration provenance but does not authorize detector-performance claims.
}
