# Burst method-comparison analyses

Author: Zhou Houchun

This directory contains three complementary analyses. They must not be
interpreted as the same estimand.

## Unified three-method truth-referenced accuracy

`run_three_method_burst_accuracy.R` evaluates Mean-ISI, LogISI/newBD and the
current zero-manual-example STPD run against exactly the same truth rows,
eligibility masks and event matcher. Synthetic v2.3 holdout results are kept
separate at 1x, 4x and 10x and bootstrapped by `Template_ID`; GPe, STN and GPi
results use the frozen reference-eligible trains and recording-group
bootstrap. No truth label is used to choose a detector threshold.

Primary outputs include ISI-support precision/recall/F1 and event
precision/recall/F1 at IoU 0.10, 0.25 and 0.50, with fragmentation and merge
diagnostics. Run the frozen 1,000-replicate analysis with:

```sh
Rscript evaluation/method_comparison/run_three_method_burst_accuracy.R 1000
Rscript evaluation/method_comparison/build_three_method_burst_accuracy_figure.R
```

## Direct truth-referenced performance

`run_meanisi_logisi_burst_comparison.R` compares Mean-ISI with Pasquale
LogISI/newBD against:

- the frozen v2.3 synthetic holdout truth, reported separately at 1x, 4x and
  10x and clustered by `Template_ID`; and
- reviewed manual Burst intervals in event-reference-eligible GPe, STN and GPi
  trains.

Primary outputs are ISI-support precision, recall, specificity, F1, balanced
accuracy and MCC, plus event precision, recall and F1 at IoU thresholds 0.10,
0.25 and 0.50. Ordinary accuracy is retained only as a secondary measure
because the abundance of non-Burst ISIs can make it misleading.

The primary comparison preserves the article-style event-size rules
(Mean-ISI: at least 3 spikes; LogISI/newBD: at least 5 spikes). A separate
harmonized sensitivity analysis requires at least 4 spikes for both methods.
Neither method uses truth labels to choose a threshold.

Run:

```sh
Rscript evaluation/method_comparison/run_meanisi_logisi_burst_comparison.R 3 1000
Rscript evaluation/method_comparison/build_meanisi_logisi_burst_report.R
Rscript evaluation/method_comparison/validate_meanisi_logisi_burst_comparison.R
```

## Three-method output agreement

`run_three_method_agreement_current.R` compares Burst output geometry among
Mean-ISI, LogISI/newBD and the current zero-manual-example STPD automatic run.
It reports pairwise event agreement, pairwise ISI overlap, three-way support
intersection and recording-group bootstrap intervals. This is a descriptive
agreement analysis, not an accuracy analysis.

Manual references are used only to define the event-reference-eligible train
scope. They do not enter any detector or any agreement calculation.

Run:

```sh
Rscript evaluation/method_comparison/run_three_method_agreement_current.R 3 1000
Rscript evaluation/method_comparison/build_three_method_agreement_figure.R
Rscript evaluation/method_comparison/validate_three_method_agreement_current.R
```

Results are written to:

- `test-results/method_comparison/three_method_truth_accuracy_current/`
- `test-results/method_comparison/meanisi_vs_logisi_burst/`
- `test-results/method_comparison/three_method_agreement_current/`
