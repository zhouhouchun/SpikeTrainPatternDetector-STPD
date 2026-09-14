\name{stpd_rgs_support_dataset}
\alias{stpd_rgs_fit}
\alias{stpd_rgs_default_parameters}
\alias{stpd_rgs_support_dataset}
\alias{stpd_rgs_support_export}
\title{Robust Gaussian Surprise Burst and Pause support}
\usage{
stpd_rgs_default_parameters()

stpd_rgs_fit(ds, selected_trains = NULL, groups = NULL,
  params = stpd_rgs_default_parameters())

stpd_rgs_support_dataset(ds, selected_trains = NULL, groups = NULL,
  params = stpd_rgs_default_parameters(), fit = NULL)

stpd_rgs_support_export(support, out_dir,
  include_full_analysis_rds = FALSE)
}
\arguments{
  \item{ds}{An STPD dataset containing timestamp trains.}
  \item{selected_trains}{Optional train names to include.}
  \item{groups}{A scalar group, a named train-to-group vector, or a data frame containing train and reference-group columns.}
  \item{params}{RGS parameters.}
  \item{fit}{Optional reference fit returned by \code{stpd_rgs_fit}; when supplied, the selected data are predicted without refitting the reference distribution.}
  \item{support}{An RGS auxiliary support result.}
  \item{out_dir}{Directory for auditable output tables.}
  \item{include_full_analysis_rds}{Whether to additionally export the complete analysis object, including spike timestamps. The default compact RDS omits raw timestamps.}
}
\value{\code{stpd_rgs_default_parameters} returns the declared parameter list. \code{stpd_rgs_fit} returns a frozen RGS reference fit. \code{stpd_rgs_support_dataset} returns Burst and Pause events, rejected candidates, per-ISI support, reference diagnostics, thresholds, summaries, and QC. The export function invisibly returns the normalized output directory.}
\description{Runs a Ko-style Robust Gaussian Surprise analysis as an auxiliary evidence provider. RGS outputs never overwrite STPD AUTO labels or detector arbitration.}
\details{The same-data convenience mode estimates the reference distribution from the selected trains. For held-out use, first fit calibration trains with \code{stpd_rgs_fit}, then pass that fit to \code{stpd_rgs_support_dataset}. Reference groups should be declared before analysis; unrelated nuclei or cohorts should not be pooled by convenience. The parameters \code{candidate_count_p} and \code{familywise_alpha} separately control estimation of the Bonferroni test count and the final family-wise significance criterion. Scientific diagnostics report \code{estimable}, \code{estimable_with_warning}, or \code{not_estimable}; publication mode fails closed for a non-estimable reference.}
