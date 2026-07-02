\name{stpd-slicetca}
\alias{stpd_slicetca_backend_status}
\alias{stpd_install_slicetca_backend}
\alias{stpd_make_slicetca_trial_tensor}
\alias{stpd_run_slicetca_backend}
\title{Optional sliceTCA backend for trial-aligned neural tensors}
\usage{
stpd_slicetca_backend_status(initialize = FALSE)

stpd_install_slicetca_backend(envname = "stpd-slicetca",
  packages = stpd_slicetca_backend_packages(),
  method = c("virtualenv", "conda"), conda = "auto")

stpd_make_slicetca_trial_tensor(trains, selected_trains = NULL,
  trial_events, event_time_col, trial_id_col = NULL, condition_col = NULL,
  pre_sec = 0.5, post_sec = 1, bin_sec = 0.02,
  time_origin = c("raw", "aligned"),
  transform = c("sqrt_count", "log1p_rate", "rate", "count"),
  scaling = c("zscore", "robust", "none"), smoothing_sigma_bins = 0,
  label_source = c("audit_final", "final", "manual_priority", "auto", "manual"),
  min_isi_sec = 0.001, auto_others = FALSE)

stpd_run_slicetca_backend(tensor_res, ranks = c(2L, 0L, 2L),
  run_python = FALSE, seed = 1L, max_iter = 1000L,
  learning_rate = 0.005, positive = FALSE, apply_invariance = TRUE)
}
\arguments{
  \item{envname}{Python virtual environment or conda environment name.}
  \item{packages}{Python modules to install. Defaults to \code{numpy}, \code{torch}, and \code{slicetca}.}
  \item{method}{Installation backend used by \pkg{reticulate}.}
  \item{conda}{Conda executable passed to \pkg{reticulate}.}
  \item{initialize}{Whether checking module availability may initialize Python. The default avoids locking \pkg{reticulate} to the wrong Python before installation.}
  \item{trains}{Named list of spike-train data frames containing \code{timestamp_sec}.}
  \item{selected_trains}{Optional neuron/train names to include.}
  \item{trial_events}{Data frame containing trial or movement event times.}
  \item{event_time_col}{Column in \code{trial_events} with event time in seconds.}
  \item{trial_id_col}{Optional trial identifier column.}
  \item{condition_col}{Optional stimulus, behavior, or condition label column.}
  \item{pre_sec, post_sec}{Seconds before and after each event to include.}
  \item{bin_sec}{Tensor time-bin width in seconds.}
  \item{time_origin}{Use raw timestamps or align each train to its first spike before tensor construction.}
  \item{transform}{Count/rate transform applied before scaling.}
  \item{scaling}{Neuron-wise tensor scaling.}
  \item{smoothing_sigma_bins}{Optional Gaussian smoothing width in bins.}
  \item{label_source}{Pattern label source used for event-state annotation.}
  \item{min_isi_sec}{Minimum valid ISI in seconds for event-state labels.}
  \item{auto_others}{Whether unlabeled automatic states may be assigned to others.}
  \item{tensor_res}{Result from \code{stpd_make_slicetca_trial_tensor}.}
  \item{ranks}{Three integers: trial-slicing, neuron-slicing, and time-slicing component counts.}
  \item{run_python}{Whether to call the official Python \code{slicetca} backend.}
  \item{seed}{Random seed passed to the backend and embedding helpers.}
  \item{max_iter}{Maximum sliceTCA optimization iterations.}
  \item{learning_rate}{Python backend learning rate.}
  \item{positive}{Whether to request non-negative components.}
  \item{apply_invariance}{Whether to run the official invariance transform when available.}
}
\description{
Build a trial-by-neuron-by-time tensor from selected spike trains and optionally fit the official Python \code{slicetca} implementation through \pkg{reticulate}.
}
\details{
The Python backend is intentionally optional. The R package does not vendor
\code{torch} or the full Python numerical stack, because those binary
dependencies are large and platform-specific. Instead,
\code{stpd_install_slicetca_backend()} creates a dedicated environment and
installs the official \code{slicetca} package plus its Python dependencies.

The tensor builder also annotates each trial-time bin with burst, pause, tonic,
high-frequency spiking, others, or unlabeled event-state summaries from the
detector layer. This makes it possible to compare sliceTCA-derived trial-time
coordinates with spike-train event grammar states.
}
\value{
\code{stpd_slicetca_backend_status()} returns a diagnostic data frame.
\code{stpd_install_slicetca_backend()} returns the backend status after
installation. \code{stpd_make_slicetca_trial_tensor()} returns a list containing
the tensor, raw counts, trial metadata, relative time bins and event-state
annotations. \code{stpd_run_slicetca_backend()} returns diagnostics,
reconstruction metrics, optional Python components/model objects, and trial-time
3D embeddings for visualization.
}
\examples{
\dontrun{
stpd_install_slicetca_backend()
stpd_slicetca_backend_status()

tensor <- stpd_make_slicetca_trial_tensor(
  trains = my_trains,
  trial_events = movement_table,
  event_time_col = "movement_onset_sec",
  condition_col = "movement_type",
  pre_sec = 0.5,
  post_sec = 1.0,
  bin_sec = 0.02
)

fit <- stpd_run_slicetca_backend(
  tensor,
  ranks = c(2, 0, 2),
  run_python = TRUE,
  max_iter = 1000
)
}
}
