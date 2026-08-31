# Clean synthetic benchmark v2 validation

This runner evaluates the frozen v2.3.0 benchmark with three
separate R processes. Do not source two stages in one process. Stage B is the
only detector process and is blind to the template/scale/split key and all
truth, phenotype, and generator files.

## Stage contract

- **A — development-only calibration.** Reads the blinded workbook, the
  calibration CSV, and the protected split key solely to select exactly five
  development samples per batch. The parameter learner receives only those
  five spike trains. A writes one frozen parameter RDS and one sanitized CSV
  containing the 15 opaque holdout `Sample_ID`/spike-time series per batch.
  Neither artifact contains Template, scale, split, phenotype, or truth labels.
- **B — blinded detection.** Reads only the Stage-A manifest, the selected
  batch's frozen parameter RDS, and its sanitized 15-sample input CSV. It never
  opens the original workbook or benchmark directory. Accepted Pause output is
  classified only by `gaps$gap_semantics`: `canonical_pause` versus
  `contextual_interburst_pause`. Each batch has independent files and a
  batch-specific manifest.
- **C — protected scoring.** First verifies all Stage-B hashes, then reads the
  key and truth. It scores only the 15 holdout templates at all three paired
  scales (45 samples). Every exported prediction axis must exactly cover every
  held-out inclusive right-spike-index key; missing exports fail rather than
  becoming `other`.

## Commands

Run the structural and lightweight leakage contracts first:

```sh
cd /path/to/SpikeTrainPatternDetector-STPD
Rscript data/synthetic/v2.3.0/tests/verify_v2_3.R
Rscript evaluation/synthetic_validation/test_v2_pipeline_contract.R
```

Run Stage A in its own process:

```sh
STPD_V2_STAGE_A_OUT=/private/tmp/stpd_clean_benchmark_v2_stage_a_20260827 \
Rscript evaluation/synthetic_validation/run_v2_stage_a_calibrate.R
```

Run all three Stage-B batches sequentially:

```sh
STPD_V2_STAGE_A=/private/tmp/stpd_clean_benchmark_v2_stage_a_20260827 \
STPD_V2_STAGE_B_OUT=/private/tmp/stpd_clean_benchmark_v2_stage_b_20260827 \
Rscript evaluation/synthetic_validation/run_v2_stage_b_detect.R
```

For independent parallel processes, run the same Stage-B command three times,
setting exactly one of these environment values in each process:

```sh
STPD_V2_BATCH=Batch_A
STPD_V2_BATCH=Batch_B
STPD_V2_BATCH=Batch_C
```

All three processes may share `STPD_V2_STAGE_B_OUT`; their prediction and
manifest filenames are batch-specific. After all three manifests exist, run
Stage C in a fresh process:

```sh
STPD_V2_STAGE_B=/private/tmp/stpd_clean_benchmark_v2_stage_b_20260827 \
STPD_V2_STAGE_C_OUT=/private/tmp/stpd_clean_benchmark_v2_stage_c_20260827 \
Rscript evaluation/synthetic_validation/run_v2_stage_c_score.R
```

Set `STPD_V2_BOOTSTRAP_N` to change the default 2,000 paired-template cluster
bootstrap replicates.

## Scoring outputs

Results are separated by batch/scale and target: Burst, canonical Pause,
contextual inter-burst separator, Tonic, and Broad HFS. HFS regularity subtypes
are descriptive only. The scorer produces inclusive ISI-support
precision/recall/F1, episode IoU at 0.25/0.50/0.75, boundary error,
fragmentation/merge diagnostics, and paired Template-ID cluster bootstrap CIs.

Burst has two mechanism variants. `strict_raw` is the primary audit and counts
all predicted support outside injected Burst truth as false positive, including
natural HFS excursions. `protocol_attributable` applies the pre-frozen v2
protocol and routes independent null-excursion support out of the ordinary FP
domain; the routed support and episode counts are exported separately. The
phenotype estimand reports primary ambiguity exclusion plus ambiguous-negative
and ambiguous-positive sensitivity bounds.

These scripts create no detector results until Stage A/B are explicitly run.
