# Publication validation workflows

This directory contains the portable scripts used for repeated synthetic
holdouts, real-data leave-one-group-out (LOGO) validation, reporting, and
prespecified diagnostic traces. Core detection code remains in `R/`; generated
results remain under the ignored `test-results/` tree.

## Data boundary

Raw recordings, manual workbooks, and generated validation outputs are not
committed. Supply them explicitly with environment variables:

- `STPD_REPO_ROOT`: optional source checkout; normally inferred from the script.
- `STPD_SIM_DATA_ROOT`: simulator dataset containing `detector_inputs/` and
  `ground_truth/`.
- `STPD_MANUAL_REFERENCE_XLSX`: real-data manual reference workbook.
- `STPD_REAL_SPIKE_CSV`: raw spike timestamp CSV, one train per column.
- `STPD_VALIDATION_OUTPUT_ROOT`: generated result root; defaults to
  `test-results/publication_validation`.
- `STPD_REAL_VALIDATION_DIR`: required explicit frozen tagged real-validation
  directory used by the report, reproducibility manifest, and diagnostics.
- `STPD_SIM_SHORT_VALIDATION_DIR`, `STPD_SIM_MEDIUM_VALIDATION_DIR`, and
  `STPD_SIM_LONG_VALIDATION_DIR`: required explicit frozen simulation-result
  directories used by the legacy combined report and manifest builders.

These paths affect only input/output discovery. Calibration and held-out labels
remain separated by the validation scripts.

## Main commands

From the repository root:

```sh
STPD_SIM_DATA_ROOT=/path/to/simulator_data \
Rscript evaluation/publication_validation/run_simulation_repeated_validation.R \
  1 20 4 release

STPD_MANUAL_REFERENCE_XLSX=/path/to/manual.xlsx \
STPD_REAL_SPIKE_CSV=/path/to/spike_timestamps.csv \
Rscript evaluation/publication_validation/run_real_leave_group_out_validation.R \
  4 reference_eligible release
```

Run classes 1, 2, and 3 separately. Never concatenate their timestamps or
score them as one run. `publication_validation_helpers.R` supplies the common
label-blind calibration, matching, and clustered-bootstrap routines.

Report and manifest builders write under `STPD_VALIDATION_OUTPUT_ROOT`, but
their result inputs must be supplied explicitly. They never fall back silently
to historical canonical result directories:

```sh
STPD_SIM_SHORT_VALIDATION_DIR=/frozen/results/short \
STPD_SIM_MEDIUM_VALIDATION_DIR=/frozen/results/medium \
STPD_SIM_LONG_VALIDATION_DIR=/frozen/results/long \
STPD_REAL_VALIDATION_DIR=/frozen/results/real_tagged \
Rscript evaluation/publication_validation/build_publication_validation_report.R

STPD_SIM_DATA_ROOT=/frozen/simulator/input \
STPD_SIM_SHORT_VALIDATION_DIR=/frozen/results/short \
STPD_SIM_MEDIUM_VALIDATION_DIR=/frozen/results/medium \
STPD_SIM_LONG_VALIDATION_DIR=/frozen/results/long \
STPD_REAL_VALIDATION_DIR=/frozen/results/real_tagged \
STPD_REAL_SPIKE_CSV=/frozen/real/spike_timestamps.csv \
Rscript evaluation/publication_validation/write_reproducibility_manifest.R
```

The report builder preserves its original generic directory contract. A dated
release must either use those canonical directory names or be copied into a
separate immutable report workspace; the script must not be presented as
having rebuilt a newer tagged release unless its source inventory matches.

## Interpretation

- The real reference is one patient with bilateral STN recordings, not an
  independent multi-patient cohort.
- Blank labels are not explicit biological negatives; axis eligibility and the
  blank-as-other convention must be reported.
- `possible_burst` and `possible_tonic` outputs are review-only and excluded
  from canonical performance metrics.
- Short real-data Tonic labels are retained as `tonic_like_review` descriptive
  fragments. They are excluded from calibration, balanced sampling, and formal
  F1; real confirmatory endpoints are Burst, Pause, and Broad HFS.
- Confirmatory Tonic reporting is restricted to the synthetic
  `phenotype / eligible_primary` estimand after Stage C has been rerun.
- Local diagnostic recovery is not an independent estimate of sensitivity.
