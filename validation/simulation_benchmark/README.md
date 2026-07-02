# Simulation Benchmark Artifacts

This directory contains a curated synthetic validation package for SpikeTrainPatternDetector.
It is intended for reviewers who need to inspect the simulated spike-train input, simulator ground truth, STPD outputs, and summary benchmark tables without receiving private manuscript drafts or bulky local intermediates.

## What Is Included

- `source/`: simulator snapshot and standalone scoring templates.
- `dataset/detector_inputs/`: detector-visible spike-train input tables.
- `dataset/ground_truth/`: simulator-generated interval and episode labels used only for scoring.
- `dataset/metadata/`: simulation configuration, seed table, label counts, quality audit, and warning notes.
- `stpd_results/`: curated STPD output tables, ground-truth comparison tables, class metrics, confusion matrices, and automatic-versus-simulator-informed threshold summaries.
- `figures/`: compact PNG figures for the dataset and STPD benchmark summaries.
- `simulator_validation_summary/`: synthetic-only simulator validation summaries and figures from the larger simulator validation run.

Bulky binary objects, TIFF/PDF publication exports, full intermediate candidate diagnostics, local archives, manuscript-review notes, and real-data calibration tables are intentionally excluded.

## Dataset Summary

The curated benchmark uses 30 synthetic spike trains of approximately 30 seconds each.
The stimulation module is disabled in this dataset; `external_stimulus_table_input.csv`, `stimulus_table_audit.csv`, and `stimulus_response_table.csv` are kept as schema-compatible empty tables.

Reference labels include:

- `Burst`
- `Pause`
- `Tonic`
- `high_frequency_tonic`
- `high_frequency_spiking`
- `Noisy`

The detector should only use files under `dataset/detector_inputs/` as input. Files under `dataset/ground_truth/` are reference labels for scoring and audit.

## Main Result Tables

- `stpd_results/simulation_aggregate_metrics_table.csv`
- `stpd_results/simulation_class_metrics_table.csv`
- `stpd_results/ground_truth_interval_evaluation_summary.csv`
- `stpd_results/automatic_vs_oracle_threshold_evaluation_summary.csv`
- `stpd_results/automatic_vs_oracle_threshold_class_metrics.csv`

The crosswalk between simulator labels and STPD reporting labels is in:

- `stpd_results/ground_truth_to_scored_label_crosswalk.csv`

For interval-level auditing, inspect:

- `stpd_results/ground_truth_interval_evaluation_joined.csv`
- `stpd_results/ISI_labels_final.csv`
- `stpd_results/Events_final.csv`

## Interpretation Notes

STPD is a candidate-event generation and review platform, not an unbiased final biological truth classifier.
The raw `exact_original_labels` evaluation is conservative because the current detector does not emit a `Noisy` class.
The `stpd_comparable_burst_family` evaluation is the fairest interval-level comparison against the current STPD label vocabulary because it treats STPD `burst`, `possible_burst`, and `long_burst` as a burst family.

Simulator-informed or oracle-threshold summaries should be interpreted as a ceiling or sensitivity analysis, not as the fully automatic default detector performance.

## Exclusions

This public package deliberately excludes:

- private manuscript drafts and reviewer notes;
- local absolute-path workspaces;
- `.rds` run objects;
- large TIFF/PDF figure exports;
- real patient data or clinical calibration outputs.

The larger local simulator workspace remains outside this repository.
