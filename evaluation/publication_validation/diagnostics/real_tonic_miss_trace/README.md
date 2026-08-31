# Real-data Tonic miss trace

This directory contains a diagnostic rerun of two prespecified held-out Tonic
episodes from the Grechishnikova 2017 reference workbook. The script is stored
with publication-validation workflows because it reruns the detector; generated
tables should be written under the ignored `test-results/` tree.

The trace uses the same reference-eligible LOGO protocol as the real validation,
including ten unique calibration episodes per pattern, the corrected
singleton-group sampler, label-blind held-out detection, bounded borrowing, and
the calibration-frozen Tonic band.

Run from the repository root:

```sh
STPD_MANUAL_REFERENCE_XLSX=/path/to/manual.xlsx \
STPD_REAL_SPIKE_CSV=/path/to/spike_timestamps.csv \
Rscript evaluation/publication_validation/diagnostics/real_tonic_miss_trace/run_trace.R \
  test-results/publication_validation/diagnostics/real_tonic_miss_trace
```

The optional first argument selects another output directory. By default, CSV
files are regenerated below `test-results/publication_validation/diagnostics`.

## Prespecified targets

- Fold 7, `RT1D00.82`, right-spike ISIs 67--70.
- Fold 8, `RT2D00.44`, right-spike ISIs 80--84.

The indices above are detector/right-spike indices. They correspond to workbook
`isi_index` values 66--69 and 79--83, respectively.

## Outputs

- `target_gate_summary.csv`: frozen thresholds, target/run statistics, gate
  outcomes, and the first failing stage.
- `target_per_isi_trace.csv`: target plus six-ISI flanks, truth labels, band
  membership, review proposal support, selected State support, and product State.
- `candidate_stage_trace.csv`: raw Tonic generator, review-only proposal,
  public candidate ledger, multi-track shadow, and automatic-product State evidence.
- `tonic_review_candidates.csv`: every review-only proposal in each target
  train, including target-overlap geometry and the total per-train review burden.
- `calibration_examples.csv`: the exact ten-per-pattern examples used in each
  fold.
- `learned_parameters.csv`: fold-specific frozen parameter manifest.
- `reproducibility_manifest.csv`: SHA-256 hashes of the input workbook, raw
  timestamps, detector core, review proposer, validation helper, and this script.

## Diagnostic conclusion

Neither miss is caused by Broad HFS competition or product materialization:
there is no overlapping raw Tonic candidate, selected HFS parent, or product
State at either target.

- Fold 7 is split at ISI 69: 20.090 ms is 0.242 ms (1.19%) below the frozen
  20.332 ms lower bound. The remaining band runs are too short even though the
  complete manually marked segment passes LV, MM, and duration checks.
- Fold 8 is absorbed into the maximal in-band run 76--84. The target 80--84
  alone passes the frozen band, LV, MM, support, and duration checks, but the
  full run includes preceding manually labelled Broad-HFS ISIs and fails LV
  (0.1004 > 0.0825) and MM (1.5336 > 1.25).

These observations do not justify relaxing the canonical frozen thresholds.
They motivate a separately evaluated `possible_tonic` review proposal for
near-boundary segments and regular subruns inside a rejected maximal band run.
