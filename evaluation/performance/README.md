# STPD computational performance benchmark

Author: Zhou Houchun

This directory provides the reproducible computational-performance analysis
requested during peer review. Runtime is not a detection-accuracy metric and is
reported separately from the biological validation results.

## What is timed

`benchmark_detector_runtime.R` times the public `stpd_detect()` call. Package
loading, CSV import, normalization, warm-up, serialization, Shiny rendering,
and figure generation are outside the timed region. The benchmark is label
blind and uses no manual examples.

Three profiles are available:

- `core`: automatic detection with optional diagnostics disabled and candidate
  lineage auditing off;
- `app_default`: automatic detection with the Shiny detector's default
  diagnostics enabled and candidate-lineage auditing off;
- `audit_summary`: diagnostics enabled together with summary candidate-lineage
  auditing. This is a trace-heavy expert mode, not the default Shiny path.

The output reports elapsed, user and system time; train, spike and ISI counts;
time per train and per 1,000 ISIs; throughput; result size; and approximate peak
R heap usage. The R heap value is not operating-system resident memory and must
be identified as an approximation.

## Formal execution

From the repository root:

```bash
STPD_BENCHMARK_PROFILE=core \
STPD_BENCHMARK_REPEATS=3 \
STPD_BENCHMARK_SUBSETS=1,5,all \
Rscript evaluation/performance/benchmark_detector_runtime.R
```

For the default Shiny detector path, excluding browser rendering:

```bash
STPD_BENCHMARK_PROFILE=app_default \
STPD_BENCHMARK_REPEATS=3 \
STPD_BENCHMARK_SUBSETS=1,5,all \
STPD_BENCHMARK_OUT=results/performance/repeated_runtime_app_default \
Rscript evaluation/performance/benchmark_detector_runtime.R
```

The default label-blind input is
`data/derived/runtime/PD_STN_public_runtime_timestamps.csv`. It is reconstructed
from the timestamp columns of the public STN workbook by
`prepare_public_runtime_input.R`; annotation columns are not read into the
benchmark input. If the derived CSV is absent, the benchmark creates it before
timing begins. A different wide timestamp CSV can be supplied with
`STPD_BENCHMARK_INPUT=/path/to/timestamps.csv`.

The frozen August 2026 runtime tables retain the SHA-256 of the original raw
CSV bytes. The public derived CSV has different serialization bytes but was
verified to reconstruct the same 23 train names up to column order and all
16,728 normalized spike timestamps exactly. Its own SHA-256 is recorded in
`data/derived/runtime/PD_STN_public_runtime_timestamps_metadata.csv`.

`summarize_three_region_runtime.R` consolidates the already frozen GPe, STN and
GPi full-dataset runs. Those runs have one timing observation per dataset and
therefore support descriptive runtime reporting only, not a confidence
interval. The repeated benchmark supplies run-to-run variability.

## Interpretation limits

Runtime depends on hardware, R version, train composition, candidate density,
diagnostic level, and background system load. Seconds per 1,000 ISIs are
provided for normalization, but should not be interpreted as proof of linear
asymptotic complexity. The three time-scaled synthetic datasets share the same
template structure and are not independent runtime replicates.
