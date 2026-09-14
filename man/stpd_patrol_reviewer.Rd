\name{stpd_patrol_reviewer_bundle_from_dataset}
\alias{stpd_patrol_reviewer_bundle_from_dataset}
\alias{launch_multi_method_patrol_reviewer}
\title{Build and review a multi-method patrol bundle}
\usage{
stpd_patrol_reviewer_bundle_from_dataset(
  ds,
  params = default_params_sec(),
  region_label = NULL,
  selected_trains = NULL,
  methods = c("mean_isi", "logisi_newbd", "poisson_surprise",
    "robust_gaussian_surprise"),
  strict = FALSE
)

launch_multi_method_patrol_reviewer(
  bundle,
  browser = TRUE,
  host = "127.0.0.1",
  port = 7336L,
  annotation_dir = NULL
)
}
\arguments{
  \item{ds}{A non-empty STPD dataset.}
  \item{params}{Detector parameters used for the current STPD prediction and auxiliary methods.}
  \item{region_label}{Optional display label for the dataset.}
  \item{selected_trains}{Optional character vector of train identifiers.}
  \item{methods}{Auxiliary support methods to include in the patrol bundle.}
  \item{strict}{Whether auxiliary-method failures should stop bundle construction.}
  \item{bundle}{A validated reviewer bundle or the path to its RDS file.}
  \item{browser}{Whether to open the system browser.}
  \item{host}{Local interface on which to serve the reviewer.}
  \item{port}{Local port on which to serve the reviewer.}
  \item{annotation_dir}{Directory used for append-only annotation history and current annotations.}
}
\value{
\code{stpd_patrol_reviewer_bundle_from_dataset()} returns a validated reviewer
bundle. \code{launch_multi_method_patrol_reviewer()} returns the value from
\code{shiny::runApp()} after the application exits.
}
\description{
Constructs aligned evidence from STPD, Mean-ISI, LogISI/newBD, Poisson Surprise
and Robust Gaussian Surprise, and opens the same auditable manual annotation
interface used by the integrated STPD review page.
}
