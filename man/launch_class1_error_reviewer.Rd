\name{launch_class1_error_reviewer}
\alias{launch_class1_error_reviewer}
\title{Launch the Interactive Class-1 Simulation Error Reviewer}
\usage{
launch_class1_error_reviewer(
  scale_runs_rds,
  interval_truth_csv,
  browser = TRUE,
  host = "127.0.0.1",
  port = 7319L
)
}
\arguments{
  \item{scale_runs_rds}{Path to a frozen Class-1 diagnostic RDS bundle. The
  native-timescale run is selected from the bundle.}
  \item{interval_truth_csv}{Path to the simulator interval ground-truth CSV.}
  \item{browser}{Whether to open a browser automatically.}
  \item{host}{Local server host passed to \code{shiny::runApp()}.}
  \item{port}{Local server port passed to \code{shiny::runApp()}.}
}
\value{
The value returned by \code{shiny::runApp()} after the application exits.
}
\description{
Launches a read-only Shiny application for reviewing false-positive and
false-negative Burst, Pause, and Broad-HFS labels in a frozen Class-1
simulation split. Reference and prediction lanes share the same timestamps;
the function does not rerun detection or modify the supplied artifacts.
}
