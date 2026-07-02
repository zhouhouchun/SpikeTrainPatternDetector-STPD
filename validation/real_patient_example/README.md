# Real Patient Example Dataset

This directory contains a curated, de-identified real STN spike-train example for smoke testing and reviewer reproducibility checks.

## Included Files

- `STN_2017_full_example.csv`: raw wide-format spike timestamp table. Columns are anonymized as `train_01` through `train_23`; each numeric value is a spike timestamp in seconds. Blank cells represent trains with fewer spikes.
- `input_spike_counts_by_train.csv`: per-train input summary generated from the anonymized CSV.
- `file_inventory_sha256.csv`: SHA256 checksums for the included data and summary files.

## De-Identification And Scope

The local source CSV was reviewed for obvious direct identifiers before this curated copy was made. No direct identifiers or local absolute paths were detected in the raw timestamp table by the repository-preparation scan.

For additional caution, the original source filename and technical train names were not copied into this public-ready folder. The spike timestamps are preserved, but column names are replaced with neutral train identifiers.

This example has no manual ground-truth labels. It is intended to demonstrate that STPD can load and process a real multi-train timestamp dataset; it should not be interpreted as a public clinical metadata release or a population-level validation dataset.

Before pushing this directory to a remote repository, confirm that the applicable ethics approval, consent language, and data-sharing policy permit sharing a de-identified single-patient electrophysiology timestamp example.

## Excluded Local Artifacts

The local `Results` folder is deliberately excluded. It contains generated audit outputs, binary `.nex`/`.rds` objects, local absolute paths, run timestamps, and run-user metadata that are not needed for reproducibility review.

Regenerate detector outputs from `STN_2017_full_example.csv` when needed instead of committing local intermediate results.

## Example Use

```r
library(SpikeTrainPatternDetector)

csv_path <- "validation/real_patient_example/STN_2017_full_example.csv"
ds <- build_spike_dataset(csv_path, mode = "raw", unit_in = "s")
params <- default_params()
ds_detected <- stpd_detect(ds, params)
```
