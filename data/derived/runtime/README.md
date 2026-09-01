# Derived label-blind runtime input

`PD_STN_public_runtime_timestamps.csv` is a wide timestamp table derived from
the public STN workbook. Each column is one spike train and values are seconds.
Only `train_id`, `isi_index`, `left_timestamp_us`, `right_timestamp_us`, and
`isi_us` are used during reconstruction; manual label columns are excluded.

Recreate the file from the repository root with:

```bash
Rscript evaluation/performance/prepare_public_runtime_input.R
```

The accompanying metadata CSV records the source workbook SHA-256, derived
CSV SHA-256, train counts, timestamp ranges, and worksheet-to-train mapping.
