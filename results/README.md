# Validation result snapshots

The `results/` directory contains compact, table-first outputs needed to audit
the reported summaries. Large serialized detector objects, intermediate HTML
dependencies, and exploratory reruns are intentionally excluded.

## Real validation

`results/real_validation/` contains the combined GPe/STN/GPi comparison of:

1. automatic detection without manual examples;
2. partial-known five-fold recording-group holdout calibration;
3. full-parameter same-data resubstitution, reported only as an adaptation
   upper bound and not as independent validation.

Confidence intervals use recording-group cluster bootstrap resampling. The
scope and eligibility rules are recorded in the accompanying protocol and
summary files.

## Synthetic validation

`results/synthetic_validation/v2.2.0/` and `v2.3.0/` contain the frozen scoring
tables for the paired 1x, 4x, and 10x benchmark projections. Report ISI-support
metrics, episode metrics across IoU thresholds, boundary errors,
fragmentation, and prediction merging together; no single score is sufficient
to characterize an event detector.

## Computational performance

`results/performance/` contains full-dataset runtimes for the automatic and
full-parameter workflows in GPe, STN and GPi, together with repeated core and
app-default measurements on a fixed high-workload train. Hardware, R version,
input hashes, workload sizes, warm-up policy, elapsed time, normalized
throughput, approximate R heap usage and result size are retained. Runtime is
reported separately from detection accuracy.
