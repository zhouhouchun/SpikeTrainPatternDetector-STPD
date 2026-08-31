\name{launch_pause_error_reviewer}
\alias{launch_pause_error_reviewer}
\title{Launch the Interactive Pause Error Reviewer}
\usage{
launch_pause_error_reviewer(
  repo = getwd(),
  raw_spike_csv,
  real_validation_dir = NULL,
  manual10_dir = NULL,
  browser = TRUE,
  host = "127.0.0.1",
  port = NULL
)
}
\arguments{
  \item{repo}{An existing repository root. It supplies the default validation
  artifact directories when \code{real_validation_dir} or
  \code{manual10_dir} is \code{NULL}.}
  \item{raw_spike_csv}{A required CSV containing real-data spike timestamps,
  with one spike train per column. Column names must match the train identifiers
  in the frozen real-data prediction table.}
  \item{real_validation_dir}{An existing directory containing the frozen
  \file{heldout_interval_predictions.csv} real-data table. If \code{NULL}, the
  function uses the publication-validation path below \code{repo}.}
  \item{manual10_dir}{An existing directory containing the frozen manual-10
  simulation validation subdirectories. If \code{NULL}, the function uses the
  simulation-validation path below \code{repo}.}
  \item{browser}{Passed to the \code{launch.browser} argument of
  \code{shiny::runApp()}.}
  \item{host}{Local server host passed to \code{shiny::runApp()}.}
  \item{port}{Optional local server port passed to \code{shiny::runApp()}.
  \code{NULL} lets Shiny select its configured default.}
}
\value{
The value returned by \code{shiny::runApp()} after the application exits.
}
\description{
Launches a read-only Shiny application for examining Pause false negatives and
false positives in frozen real-data and simulation validation results. The app
aligns raw spike timestamps, reference labels, and held-out STPD predictions;
it does not rerun detection or modify the supplied artifacts.
}
