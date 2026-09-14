# Stage 10 framework review — real validation and comparators

Date: 2026-08-30
Scope: evaluation protocol only; no new real-data metrics.

## Decision

PASS. The framework defines recording-group leave-one-group-out evaluation,
held-out exclusion from threshold calibration, track-specific denominators and
a common event/support metric implementation for STPD and comparators.

## Stop-check review

- Predictions are generated label-blind within every held-out fold.
- Mean-ISI, log-ISI, Max-Interval or imported/manual-threshold workflows receive
  the same calibration/evaluation separation and matching rule.
- STPD's manual-assisted compatibility is reported separately from its AUTO
  mode; imported comparator thresholds cannot be presented as AUTO accuracy.
- Support recall, event IoU, boundary error and split/merge diagnostics use one
  frozen evaluator.
- Recording-group bootstrap quantifies within-dataset uncertainty only; one
  patient cannot establish patient-level external generalization.

The updated-workbook rerun and final comparator numbers remain deferred.
P0/P1/P2 framework defects found: 0/0/0. Stage 11 may begin.
