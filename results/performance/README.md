# Computational performance results

Author: Zhou Houchun

These results address the peer-review request for computational workload and
runtime reporting. Timing is reported separately from detector accuracy.

## Full real-data runs

The frozen automatic, label-blind runs used the core profile: optional
diagnostics disabled, candidate-lineage auditing off, no manual examples, and
browser rendering excluded.

| Region | Trains | ISIs | Elapsed time | Seconds / 1,000 ISIs |
|---|---:|---:|---:|---:|
| GPe | 16 | 28,635 | 588.0 s (9.80 min) | 20.53 |
| STN | 23 | 16,705 | 567.3 s (9.46 min) | 33.96 |
| GPi | 23 | 19,819 | 494.3 s (8.24 min) | 24.94 |

Each row is one full-dataset execution and is therefore descriptive; it does
not provide a runtime confidence interval. Variation across regions shows that
runtime depends on candidate composition as well as the number of ISIs.

## Repeated high-workload single-train benchmark

The longest STN train contained 1,436 spikes (1,435 ISIs). After one excluded
warm-up run, three timed repetitions were run on an Apple M1 Max with R 4.5.2.

| Profile | Median elapsed | Q1–Q3 | Range | Approx. peak R heap | Result size |
|---|---:|---:|---:|---:|---:|
| Core | 34.01 s | 33.94–35.13 s | 33.87–36.26 s | 418.1 MiB | 14.44 MiB |
| App default, browser excluded | 43.93 s | 43.76–45.10 s | 43.59–46.27 s | 419.1 MiB | 19.09 MiB |

The default app-side detector call was 29.2% slower than the core call in this
small repeated benchmark because it materialized optional diagnostics. This
comparison excludes plotting, browser transfer, and reactive UI rendering, so
it must not be presented as end-to-end Shiny latency.

The peak R heap estimate is obtained from R's garbage-collector accounting; it
is not operating-system resident memory. Exact run rows, workload order,
environment metadata, and input SHA-256 hashes are included in this directory.

## Reproduction

See `evaluation/performance/README.md` and run
`evaluation/performance/benchmark_detector_runtime.R`. Figures in
`analysis/figures/generated/` are produced by
`analysis/figures/plot_performance_runtime.R`.
