\name{stpd_detect_poisson_surprise}
\alias{stpd_detect_poisson_surprise}
\alias{stpd_detect_poisson_surprise_dataframe}
\alias{stpd_poisson_surprise_support_dataset}
\alias{stpd_poisson_surprise_support_export}
\title{Poisson-surprise Burst support detection}
\usage{
stpd_detect_poisson_surprise(
  spike_times,
  surprise_threshold = 10,
  log_base = c("e", "10"),
  min_spikes = 3L,
  seed_isi_factor = 0.5,
  seed_isi_sec = NULL,
  rejection_isi_factor = 2,
  rejection_isi_sec = NULL,
  max_failed_additions = 10L,
  rate_method = c("mean_isi", "recording_window"),
  recording_start_sec = NULL,
  recording_end_sec = NULL,
  time_unit = c("s", "ms"),
  sort_policy = c("error", "sort"),
  duplicate_policy = c("error", "drop"),
  return_candidates = TRUE,
  improvement_tolerance = sqrt(.Machine$double.eps)
)

stpd_detect_poisson_surprise_dataframe(data, ...)

stpd_poisson_surprise_support_dataset(
  ds,
  selected_trains = NULL,
  surprise_threshold = 10,
  log_base = c("e", "10"),
  min_spikes = 3L,
  seed_isi_factor = 0.5,
  seed_isi_sec = NULL,
  rejection_isi_factor = 2,
  rejection_isi_sec = NULL,
  max_failed_additions = 10L,
  rate_method = "mean_isi",
  recording_start_sec = NULL,
  recording_end_sec = NULL,
  improvement_tolerance = sqrt(.Machine$double.eps)
)

stpd_poisson_surprise_support_export(support, out_dir)
}
\arguments{
  \item{spike_times}{Spike timestamps for one analysis epoch.}
  \item{surprise_threshold}{Minimum optimized surprise. Report this value together with \code{log_base}.}
  \item{log_base}{\code{"e"} for \eqn{-\ln(P)} or \code{"10"} for \eqn{-\log_{10}(P)}.}
  \item{min_spikes}{Minimum seed and final Burst size; the default is three spikes.}
  \item{seed_isi_factor}{Seed ISI threshold relative to the train-wide mean ISI.}
  \item{seed_isi_sec}{Optional absolute seed threshold in seconds.}
  \item{rejection_isi_factor}{Extension stopping threshold relative to mean ISI.}
  \item{rejection_isi_sec}{Optional absolute extension stopping threshold.}
  \item{max_failed_additions}{Consecutive non-improving additions allowed.}
  \item{rate_method}{Baseline rate estimated from mean ISI or a known recording window.}
  \item{recording_start_sec, recording_end_sec}{Known acquisition boundaries when using \code{"recording_window"}.}
  \item{time_unit}{Input timestamp unit.}
  \item{sort_policy, duplicate_policy}{Explicit timestamp quality-control policies.}
  \item{return_candidates}{Retain optimized candidates below the acceptance threshold.}
  \item{improvement_tolerance}{Numerical tolerance for surprise improvement.}
  \item{data}{A data frame with one spike train per column.}
  \item{ds}{An STPD dataset containing one or more timestamp trains.}
  \item{selected_trains}{Optional train names to process.}
  \item{support}{A result returned by \code{stpd_poisson_surprise_support_dataset}.}
  \item{out_dir}{Directory for auditable CSV output tables.}
  \item{...}{Arguments passed to \code{stpd_detect_poisson_surprise}.}
}
\value{An auditable result containing a standardized Burst feature table, rejected candidates, spike and ISI membership, parameters, baseline-rate information, inter-Burst timing, and QC records. The export function invisibly returns the normalized output directory.}
\description{Implements a classical Legendy--Salcman/pCLAMP-style Poisson-surprise method as an auxiliary Burst support method. Its output is evidence for STPD review and does not alter the native STPD grammar or become ground truth.}
\details{The Burst feature table follows the Mean-ISI/LogISI support-table conventions for event boundaries, spike and ISI counts, duration, within-Burst ISI summaries, firing rate, and adjacent ISIs. Inter-Burst timing is reported without ambiguity: \code{inter_burst_gap_sec} is the preceding Burst end to current Burst start, whereas \code{inter_burst_onset_interval_sec} is the preceding Burst start to current Burst start.}
