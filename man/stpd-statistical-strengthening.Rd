\name{stpd-statistical-strengthening}
\alias{stpd_event_level_metrics_ci}
\alias{stpd_event_level_cluster_bootstrap}
\alias{stpd_freeze_thresholds_for_trains}
\alias{stpd_score_calibration}
\alias{stpd_fit_score_calibrator}
\alias{stpd_predict_score_calibrator}
\alias{stpd_score_calibration_frozen}
\alias{stpd_boundary_tolerance_sensitivity}
\alias{stpd_inter_rater_reliability}
\alias{stpd_manual_label_uncertainty_report}
\alias{stpd_ambiguous_manual_labels}
\alias{stpd_train_validation_strata}
\alias{stpd_stratified_train_splits}
\alias{stpd_repeated_train_holdout_validation}
\alias{stpd_nested_tune_params}
\alias{stpd_parameter_sensitivity_adjustment}
\alias{stpd_detector_surrogate_false_alarm}
\title{Statistical strengthening helpers for event-level validation}
\description{
Adds independent threshold freezing, event-level confidence intervals, train-cluster
bootstrap intervals, score calibration tables, and surrogate false-alarm/FDR
summaries to spike-train detector validation workflows.
}
\usage{
stpd_event_level_metrics_ci(metrics, conf_level = 0.95)

stpd_event_level_cluster_bootstrap(pred, truth, class_col = "pattern",
  iou_min = 0.25, cluster_col = "train", n_bootstrap = 200L,
  seed = NULL, conf_level = 0.95)

stpd_freeze_thresholds_for_trains(ds, params = default_params_sec(),
  calibration_trains = NULL, min_isi_sec = NULL, bin_width_sec = NULL,
  freeze_scope = "calibration")

stpd_score_calibration(pred, truth, score_col = "auto_score",
  class_col = "pattern", iou_min = 0.25, n_bins = 10L,
  conf_level = 0.95)

stpd_fit_score_calibrator(pred, truth, score_col = "auto_score",
  class_col = "pattern", iou_min = 0.25,
  method = c("platt", "isotonic"))

stpd_predict_score_calibrator(calibrator, pred)

stpd_score_calibration_frozen(pred_cal, truth_cal, pred_val,
  truth_val = NULL, score_col = "auto_score", class_col = "pattern",
  iou_min = 0.25, method = c("platt", "isotonic"), n_bins = 10L,
  conf_level = 0.95)

stpd_boundary_tolerance_sensitivity(pred, truth,
  iou_grid = c(0.10, 0.25, 0.50), class_col = "pattern",
  ambiguous_labels = stpd_ambiguous_manual_labels(),
  exclude_ambiguous = TRUE, conf_level = 0.95)

stpd_inter_rater_reliability(rater_events, rater_col = "rater",
  class_col = "pattern", iou_min = 0.25,
  ambiguous_labels = stpd_ambiguous_manual_labels(),
  exclude_ambiguous = TRUE)

stpd_manual_label_uncertainty_report(pred = NULL, truth = NULL,
  rater_events = NULL, iou_grid = c(0.10, 0.25, 0.50),
  iou_min = 0.25, ambiguous_labels = stpd_ambiguous_manual_labels(),
  exclude_ambiguous = TRUE, conf_level = 0.95)

stpd_train_validation_strata(ds, params = default_params_sec(),
  selected_trains = NULL, metadata = NULL, strata_cols = NULL,
  metric_mode = c("strict_high_confidence", "candidate_family"))

stpd_stratified_train_splits(strata_table, validation_fraction = 0.25,
  n_repeats = 50L, seed = 1L, min_validation_trains = 1L)

stpd_repeated_train_holdout_validation(ds, params = default_params_sec(),
  selected_trains = NULL, metadata = NULL, strata_cols = NULL,
  n_repeats = 50L, validation_fraction = 0.25, seed = 1L,
  nested_tuning = FALSE, tuning_paths = NULL, max_tuning_params = 4L,
  relative_step = 0.25, iou_min = 0.25,
  metric_mode = c("strict_high_confidence", "candidate_family"),
  use_learned_ranges = TRUE, threshold_freeze = c("calibration", "none"),
  collect_diagnostics = FALSE)

stpd_nested_tune_params(ds, params, calibration_trains, paths = NULL,
  max_params = 4L, relative_step = 0.25,
  inner_validation_fraction = 0.25, seed = 1L, iou_min = 0.25,
  metric_mode = c("strict_high_confidence", "candidate_family"),
  use_learned_ranges = TRUE, collect_diagnostics = FALSE,
  selection_metric = c("macro_F1", "macro_recall", "macro_precision"))

stpd_parameter_sensitivity_adjustment(summary, train_metrics,
  baseline_variant_id = "baseline_current", alpha = 0.05,
  effect_threshold = 0.02, p_adjust_method = "BH",
  n_permutations = 999L, seed = 1L)

stpd_detector_surrogate_false_alarm(ds, params = default_params_sec(),
  selected_trains = NULL, n_surrogates = 99L,
  methods = c("isi_permutation", "renewal"),
  metric_mode = c("strict_high_confidence", "candidate_family"),
  seed = NULL, block_length = 10L, collect_diagnostics = FALSE)
}
\arguments{
\item{metrics}{A data frame returned by \code{stpd_event_level_metrics()} or a compatible table with true-positive, false-positive and false-negative counts.}
\item{conf_level}{Confidence level for Wilson or bootstrap intervals.}
\item{pred}{Predicted event table.}
\item{truth}{Manual/ground-truth event table.}
\item{class_col}{Column containing event class labels.}
\item{iou_min}{Minimum event IoU used to count a same-label match.}
\item{cluster_col}{Cluster column for bootstrap resampling. The default is train-level resampling.}
\item{n_bootstrap}{Number of bootstrap replicates.}
\item{seed}{Optional random seed.}
\item{ds}{SpikeTrainPatternDetector dataset object.}
\item{params}{Detector parameter list.}
\item{calibration_trains}{Train names used to resolve/freeze dataset thresholds or to define the inner tuning calibration pool.}
\item{min_isi_sec}{Optional minimum valid ISI for threshold resolution.}
\item{bin_width_sec}{Optional histogram bin width for threshold resolution.}
\item{freeze_scope}{Text label stored in threshold-resolution metadata.}
\item{score_col}{Predicted-event score column used for calibration.}
\item{method}{Score calibration method: Platt logistic scaling or isotonic regression.}
\item{n_bins}{Number of score bins. Quantile bins are used when possible.}
\item{calibrator}{A frozen score calibrator returned by \code{stpd_fit_score_calibrator()}.}
\item{pred_cal, truth_cal}{Calibration-split predicted and manual event tables.}
\item{pred_val, truth_val}{Validation-split predicted and manual event tables.}
\item{iou_grid}{IoU thresholds used for boundary tolerance sensitivity.}
\item{ambiguous_labels}{Manual labels treated as ambiguous/uncertain.}
\item{exclude_ambiguous}{Whether ambiguous labels are excluded from primary validation metrics.}
\item{rater_events}{Event table or named list of event tables containing manual labels from multiple raters.}
\item{rater_col}{Column containing rater identifiers.}
\item{metadata}{Optional train-level metadata table with a \code{train} column.}
\item{strata_cols}{Columns used to build stratified train-level holdout strata.}
\item{strata_table}{Output from \code{stpd_train_validation_strata()}.}
\item{validation_fraction}{Fraction of labeled trains assigned to validation.}
\item{n_repeats}{Number of repeated holdout splits.}
\item{min_validation_trains}{Minimum validation trains per repeated split when possible.}
\item{nested_tuning}{Whether parameter variants are selected inside each outer calibration split.}
\item{tuning_paths, paths}{Parameter paths considered during nested tuning.}
\item{max_tuning_params, max_params}{Maximum number of parameter paths considered.}
\item{relative_step}{Relative perturbation size for numeric parameter variants.}
\item{use_learned_ranges}{Whether learned train ranges are retained during validation.}
\item{selection_metric}{Metric used to select nested-tuning variants.}
\item{inner_validation_fraction}{Fraction of outer calibration trains held out for inner validation during nested tuning.}
\item{threshold_freeze}{Whether validation reports freeze thresholds on calibration trains or leave threshold behavior unchanged.}
\item{summary}{Parameter-sensitivity summary table.}
\item{train_metrics}{Train-paired metric table used for sign-flip tests.}
\item{baseline_variant_id}{Variant id used as the no-change baseline.}
\item{alpha}{False-discovery-rate threshold.}
\item{effect_threshold}{Minimum macro-F1 effect size needed to call a parameter sensitive.}
\item{p_adjust_method}{Multiple-comparison adjustment method, typically \code{"BH"}.}
\item{n_permutations}{Number of random sign-flip permutations when exact enumeration is not used.}
\item{selected_trains}{Train names included in surrogate false-alarm analysis.}
\item{n_surrogates}{Number of surrogate datasets per surrogate method.}
\item{methods}{Surrogate generation methods: \code{"isi_permutation"}, \code{"renewal"}, or \code{"block_isi_shuffle"}.}
\item{metric_mode}{Event-label normalization mode used before counting events.}
\item{block_length}{ISI block length used by \code{"block_isi_shuffle"}.}
\item{collect_diagnostics}{Whether detector diagnostics are collected during repeated surrogate runs.}
}
\details{
\code{stpd_freeze_thresholds_for_trains()} resolves threshold tables on the
specified calibration trains and then disables automatic dataset re-resolution
inside \code{stpd_detect()}, preventing validation trains from influencing
threshold choice.

\code{stpd_event_level_metrics_ci()} adds Wilson score intervals for precision
and recall. F1 bounds are reported as plug-in bounds from the precision and
recall intervals.

\code{stpd_event_level_cluster_bootstrap()} resamples whole trains with
replacement. This targets uncertainty from train-to-train heterogeneity rather
than treating all events as independent.

\code{stpd_score_calibration()} bins detector scores and reports empirical
precision with Wilson intervals. Brier score is reported only when scores are
already bounded in \eqn{[0, 1]}; otherwise scores are treated as raw ranks.

\code{stpd_score_calibration_frozen()} fits a Platt or isotonic calibrator on
calibration predictions only, then applies that frozen mapping to validation
predictions and reports reliability bins, expected calibration error, maximum
calibration error and Brier score.

\code{stpd_boundary_tolerance_sensitivity()} reports validation metrics over
multiple IoU thresholds. \code{stpd_inter_rater_reliability()} computes pairwise
event-level agreement and Cohen-style label kappa for multiple manual raters.
Ambiguous manual labels are excluded from primary metrics by default.

\code{stpd_repeated_train_holdout_validation()} repeats stratified train-level
holdout splits and can run nested parameter tuning inside the outer calibration
split. \code{stpd_parameter_sensitivity_adjustment()} adds train-paired
sign-flip p-values, Benjamini-Hochberg q-values and robust-parameter flags to
parameter-sensitivity scans.

\code{stpd_detector_surrogate_false_alarm()} creates ISI-based surrogate
datasets, runs the detector with observed thresholds frozen, and estimates
surrogate false-alarm counts, empirical count p-values, Benjamini-Hochberg
adjusted q-values, and a detector-level FDR estimate. The FDR estimate is
reported only when the observed detector event count is greater than zero.
}
\value{
\code{stpd_event_level_metrics_ci()} returns the input metrics with interval
columns added. \code{stpd_event_level_cluster_bootstrap()} returns a list with
observed metrics, bootstrap replicate metrics, and interval summaries.
\code{stpd_freeze_thresholds_for_trains()} returns a parameter list containing
frozen threshold tables and threshold metadata. \code{stpd_score_calibration()}
and \code{stpd_detector_surrogate_false_alarm()} return lists of summary tables
and diagnostic event/count tables.
}
