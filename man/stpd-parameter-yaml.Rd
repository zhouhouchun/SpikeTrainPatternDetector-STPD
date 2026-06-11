\name{stpd-parameter-yaml}
\alias{stpd_write_params_yaml}
\alias{stpd_read_params_yaml}
\alias{stpd_parameter_yaml_bundle}
\alias{stpd_parameter_yaml_roundtrip_report}
\alias{stpd_parameter_validation_summary}
\title{YAML parameter import, export, validation, and round-trip checks}
\usage{
stpd_write_params_yaml(params, file, source = "api",
  baseline = default_params_sec())
stpd_read_params_yaml(file, baseline = default_params_sec(),
  strict = FALSE)
stpd_parameter_yaml_bundle(params, source = "api",
  baseline = default_params_sec(), include_validation = TRUE)
stpd_parameter_yaml_roundtrip_report(params,
  source = "roundtrip_probe")
stpd_parameter_validation_summary(params, issues = NULL)
}
\arguments{
  \item{params}{Detector parameter list.}
  \item{file}{YAML file path to read or write.}
  \item{source}{Short label describing where the parameter YAML came from.}
  \item{baseline}{Default parameter tree used when importing partial YAML.}
  \item{strict}{If \code{TRUE}, stop when contract validation reports errors.}
  \item{include_validation}{Whether to include validation summary metadata in the YAML bundle.}
  \item{issues}{Optional validation table from \code{stpd_validate_params()}.}
}
\description{
Helpers for exporting SpikeTrainPatternDetector parameters to a durable YAML
configuration file, importing YAML back into a detector parameter tree, and
checking whether write/read round-trips preserve the parameter hash.
}
\details{
The YAML format stores a \code{format} field, schema metadata, a parameter hash,
and a \code{parameters} mapping. Partial YAML files are overlaid on
\code{baseline}; imported parameters are coerced according to
\code{stpd_parameter_contract()} and validated with \code{stpd_validate_params()}.
The Shiny parameter panel uses these helpers for parameter-file import/export,
UI round-trip, and validation display.
}
\value{
\code{stpd_write_params_yaml()} invisibly returns the YAML bundle it wrote.
\code{stpd_read_params_yaml()} returns a list with \code{params},
\code{validation}, \code{summary}, \code{params_hash}, and \code{metadata}.
\code{stpd_parameter_yaml_bundle()} returns the serializable bundle.
\code{stpd_parameter_yaml_roundtrip_report()} and
\code{stpd_parameter_validation_summary()} return data frames.
}
