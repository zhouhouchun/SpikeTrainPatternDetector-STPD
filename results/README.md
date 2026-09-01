# Validation result snapshots

The `results/` directory contains compact, table-first outputs needed to audit
the reported summaries. Large serialized detector objects, intermediate HTML
dependencies, and exploratory reruns are intentionally excluded.

## Real validation

`results/real_validation/` contains the combined GPe/STN/GPi comparison of:

1. automatic detection without manual examples;
2. partial-known five-fold recording-group holdout calibration;
3. full-parameter same-data resubstitution, reported only as a sensitivity
   analysis. It is neither independent validation nor a guaranteed monotone
   performance upper bound.

Confidence intervals use recording-group cluster bootstrap resampling. The
scope and eligibility rules are recorded in the accompanying protocol and
summary files.

## Synthetic validation

`results/synthetic_validation/v2.2.0/` and `v2.3.0/` contain the frozen scoring
tables from the final `2026-08-30_tonic_state_repair_final_01` scoring run for
the paired 1x, 4x, and 10x benchmark projections. Each directory's
`stage_c_manifest.csv` lists and authenticates only the compact tables included
in the public release. Report ISI-support
metrics, episode metrics across IoU thresholds, boundary errors,
fragmentation, and prediction merging together; no single score is sufficient
to characterize an event detector.

## Burst method comparison

`results/method_comparison/three_method_truth_accuracy_current/` contains the
unified automatic Mean-ISI, LogISI/newBD, and STPD comparison. The three
methods use identical eligible records, truth masks, and event matching.
Biological recordings are resampled by recording `Group_ID`; synthetic data
are resampled by `Template_ID`. ISI-support and event precision, recall, and F1
all include 1,000-replicate cluster-bootstrap intervals, and
`analysis_scope.csv` records the exact train and cluster counts.

## Computational performance

`results/performance/` contains full-dataset runtimes for the automatic and
full-parameter workflows in GPe, STN and GPi, together with repeated core and
app-default measurements on a fixed high-workload train. Hardware, R version,
input hashes, workload sizes, warm-up policy, elapsed time, normalized
throughput, approximate R heap usage and result size are retained. Runtime is
reported separately from detection accuracy.
