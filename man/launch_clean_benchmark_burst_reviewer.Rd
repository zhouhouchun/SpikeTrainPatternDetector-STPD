\name{launch_clean_benchmark_burst_reviewer}
\alias{launch_clean_benchmark_burst_reviewer}
\title{Launch the Clean-Benchmark Burst Error Reviewer}
\usage{
launch_clean_benchmark_burst_reviewer(
  validation_dir,
  benchmark_dir,
  browser = TRUE,
  host = "127.0.0.1",
  port = 7321L
)
}
\arguments{
  \item{validation_dir}{Directory containing the \code{scoring} output from
  the clean-benchmark validation workflow.}
  \item{benchmark_dir}{Root directory of the clean synthetic mechanism
  benchmark containing \code{detector_inputs} and \code{ground_truth}.}
  \item{browser}{Whether to open a browser automatically.}
  \item{host}{Local server host passed to \code{shiny::runApp()}.}
  \item{port}{Local server port passed to \code{shiny::runApp()}.}
}
\value{
The value returned by \code{shiny::runApp()} after the application exits.
}
\description{
Launches a read-only Shiny application for inspecting Burst false-positive,
false-negative, and imperfect-boundary episodes in the frozen three-scale
clean mechanism benchmark. Event and State tracks are displayed separately.
}
