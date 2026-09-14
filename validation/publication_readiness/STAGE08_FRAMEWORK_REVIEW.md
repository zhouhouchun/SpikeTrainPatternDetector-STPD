# Stage 08 framework review — synthetic validation pipeline

Date: 2026-08-30
Scope: estimand and execution contract only; no new performance values.

## Decision

PASS. The pipeline separates calibration from evaluation by template/group,
scores each physical scale separately, and defines both ISI-support and episode
estimands with cluster-aware uncertainty and explicit boundary diagnostics.

## Stop-check review

- Calibration and evaluation groups are disjoint and manifest-bound.
- Primary results use label-blind detector execution; oracle/known-parameter
  runs are reported only as separate workflow sensitivity analyses.
- Precision, recall and F1 are supplemented by IoU, onset/offset error,
  fragmentation, false split and prediction merge.
- Template-cluster bootstrap respects paired 1x/4x/10x realizations.
- Perturbation/OOD results cannot silently replace the frozen primary endpoint.

Full reruns and final confidence intervals remain deferred. P0/P1/P2 framework
defects found: 0/0/0. Stage 9 may begin.
