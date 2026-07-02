\name{stpd-distributional-evidence}
\alias{stpd_add_distributional_results}
\alias{stpd_event_distribution_evidence}
\alias{stpd_logisi_moments}
\alias{stpd_spike_count_pmf}
\alias{stpd_train_distribution_features}
\title{Distributional evidence and firing-phenotype summaries}
\usage{
stpd_add_distributional_results(ds, params = default_params_sec(),
  selected_trains = NULL, candidates = NULL)

stpd_event_distribution_evidence(ds, candidates = NULL,
  selected_trains = NULL, min_isi_sec = 0.001,
  burst_threshold_sec = NA_real_, pause_threshold_sec = NA_real_,
  flank_n = 10L, refractory_sec = 0.005)

stpd_train_distribution_features(ds_or_trains, selected_trains = NULL,
  min_isi_sec = 0.001,
  burst_thresholds_sec = c(0.005, 0.010, 0.020),
  pause_thresholds_sec = c(0.100, 0.200, 0.500),
  count_windows_sec = c(0.020, 0.050, 0.100),
  refractory_sec = 0.005)

stpd_spike_count_pmf(ds_or_trains, selected_trains = NULL,
  windows_sec = c(0.020, 0.050, 0.100))

stpd_logisi_moments(isi_sec, min_isi_sec = 0.001)
}
\arguments{
  \item{ds}{A SpikeTrainPatternDetector dataset object containing a named \code{trains} list.}
  \item{params}{Detector parameter list. The minimum valid ISI is read from \code{params$detector}.}
  \item{selected_trains}{Optional train names. When omitted, all trains are used.}
  \item{candidates}{Optional candidate table. When omitted, \code{candidate_features} or \code{candidate_ledger} is used.}
  \item{min_isi_sec}{Minimum valid inter-spike interval in seconds.}
  \item{burst_threshold_sec}{Optional fixed short-ISI threshold for event-level burst evidence. When missing, a train-adaptive lower ECDF threshold is used.}
  \item{pause_threshold_sec}{Optional fixed long-ISI threshold for event-level pause evidence. When missing, a train-adaptive upper ECDF threshold is used.}
  \item{flank_n}{Number of pre/post ISIs used as local flank context.}
  \item{refractory_sec}{Refractory-period constant used in the refractory-adjusted LV-style index.}
  \item{ds_or_trains}{Either a dataset object or a named list of train data frames.}
  \item{burst_thresholds_sec}{Fixed short-ISI thresholds used for train-level CDF-mass summaries.}
  \item{pause_thresholds_sec}{Fixed long-ISI thresholds used for train-level tail-mass summaries.}
  \item{count_windows_sec}{Window widths for spike-count PMF and Fano-factor summaries.}
  \item{windows_sec}{Window widths for \code{stpd_spike_count_pmf()}.}
  \item{isi_sec}{Numeric vector of inter-spike intervals in seconds.}
}
\description{
Adds a distributional support layer for spike-train candidates and trains. The
layer quantifies ISI CDF/tail mass, logISI distributional distances, local
CV/CV2/LV/LvR, spike-count PMF, Fano factor and logISI higher moments.
}
\details{
This is an audit/support layer, not a replacement for the event grammar. It is
designed to help users judge whether a candidate event has distributional
support and to summarize train-level firing phenotype. The helper
\code{stpd_add_distributional_results()} stores three public result tables:
\code{event_distribution_evidence}, \code{train_distribution_features}, and
\code{spike_count_pmf}. These tables do not overwrite \code{pattern_auto},
\code{pattern_manual}, \code{pattern_audit_final}, or event boundaries.
}
\value{
\code{stpd_add_distributional_results()} returns the dataset with additional
result tables. The remaining functions return data frames or tibbles with
distributional evidence, train-level features, spike-count PMF rows, or logISI
moment summaries.
}
\examples{
isi <- c(rep(0.30, 6), rep(0.025, 5), rep(0.30, 6))
ts <- c(0, cumsum(isi))
dat <- data.frame(
  idx = seq_along(ts),
  timestamp_sec = ts,
  ISI_sec = c(NA, isi),
  pattern_manual = "",
  pattern_auto = ""
)
ds <- list(trains = list(train_1 = dat), results = list())
stpd_train_distribution_features(ds)
stpd_spike_count_pmf(ds)
}
