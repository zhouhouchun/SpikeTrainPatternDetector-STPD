\name{stpd_apply_support_threshold_to_params}
\alias{stpd_apply_support_threshold_to_params}
\title{Import a support-method Burst threshold into detector parameters}
\usage{
stpd_apply_support_threshold_to_params(
  params,
  support,
  method = c("mean_isi", "logisi"),
  activate = TRUE,
  aggregation = c("median")
)
}
\arguments{
  \item{params}{A SpikeTrainPatternDetector parameter list.}
  \item{support}{A result returned by \code{stpd_misi_support_dataset()} or
  \code{stpd_logisi_support_dataset()}.}
  \item{method}{The support method that produced the threshold evidence.}
  \item{activate}{If \code{TRUE}, update the active Burst seed/bridge bands and
  select the user threshold source. If \code{FALSE}, store the method evidence
  without changing the active detector bands.}
  \item{aggregation}{The across-train aggregation rule. Currently the robust
  median is used.}
}
\value{A parameter list with deterministic support-method provenance.}
\description{
This function provides the explicit bridge between the Mean-ISI or LogISI
support layers and the editable detector parameter store. Only resolved,
positive per-train thresholds are aggregated. Importing does not write AUTO
labels and does not alter tonic, pause, or high-frequency parameters. After an
active import, detection must be rerun before AUTO results change.
}
