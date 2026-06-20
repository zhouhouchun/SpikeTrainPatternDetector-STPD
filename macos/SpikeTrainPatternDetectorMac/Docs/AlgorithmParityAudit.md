# Algorithm Parity Audit

This document records the original R detection flow before each native Swift
migration step. The goal is to keep the macOS app aligned with the source
algorithm rather than only matching the visible Shiny UI.

## Source Of Truth

Original R entry points:

- `R/27_engine.R`: `stpd_detect()` is the public detection wrapper.
- `R/49_spiketrainpattern_product_schema.R`: product-level pre-detection QC,
  optional dataset threshold freezing, and run output schema.
- `R/16_semantic_consistency.R`: dataset-level loop, diagnostics, event tables,
  candidate ledger, event audit, and final decision objects.
- `R/50_internal_api.R`: stable internal API. The original default train
  pipeline is `hf_protected`.
- `R/42_hfspiking_protection_and_manual_coexistence.R`: active per-train
  `hf_protected` detector, HFS protection, and the active candidate value
  function used by weighted interval selection.

## Original Detection Flow

The original detector does not run a single threshold rule. It runs a layered
candidate grammar and then performs a global non-overlap selection.

1. Product pre-detection QC checks the requested trains and can stop the run
   when QC errors are configured as fatal.
2. Dataset/train adaptive thresholds are resolved. When threshold freezing is
   enabled, the resolved threshold table is attached to the parameter object and
   reused during train detection.
3. For every selected train, the active `hf_protected` train pipeline runs:
   - ensure ISI percentiles and train profile rows;
   - add classic anchor candidates when the classic anchor layer is available;
   - add hard-threshold ISI candidates;
   - add final event-grammar burst candidates;
   - add high-frequency-spiking, high-frequency-tonic, tonic, and pause
     candidates;
   - protect accepted long HFS states from embedded fragment candidates;
   - select non-overlapping candidates with weighted interval selection;
   - write selected candidates into `pattern_auto`.
4. Dataset-level code derives event intervals from final labels and builds:
   - `events`;
   - `candidate_diagnostic_audit`;
   - `candidate_ledger`;
   - `event_audit`;
   - `candidate_features`;
   - `final_decisions`;
   - validation and run metadata.

## Current Swift Mapping

Native Swift files already covering part of the original design:

- `ClassicAnchorDetectionPipeline.swift`: dataset/train loop and current native
  detector composition.
- `TrainAdaptiveBandResolver.swift`: native train-adaptive band resolver.
- `ClassicAnchorDetector.swift`: native classic anchor burst candidates.
- `PauseDetector.swift`: native pause candidates.
- `StatePatternDetector.swift`: native tonic, HF tonic, and HFS candidates.
- `ClassicAnchorCandidateArbitrator.swift`: final non-overlap selection.

Known parity gaps still to close:

- The detector page UI is still not the full original Shiny detector surface.
- Swift currently has native QC UI, but the detection run does not yet mirror
  the original product-level fatal QC gate and threshold-table output schema.
- Swift has classic anchor burst candidates, but not the full final
  event-grammar burst detector from `R/40_burst_consistency_optimization.R`.
- Swift HFS protection is still much simpler than the original suppression
  layer in `R/42_hfspiking_protection_and_manual_coexistence.R`.
- Swift pause and state detector defaults need a second pass against the exact
  R parameter defaults and guards.
- Swift candidate output needs a stricter split between diagnostic audit rows
  and public selected candidate ledger rows.

## Migration Rule

Before migrating a detector block, compare these R details first:

1. candidate generation conditions;
2. candidate metric fields;
3. candidate priority and value function;
4. non-overlap or suppression rules;
5. public output rows versus diagnostic-only rows;
6. regression tests on the bundled sample and `PD_STN.csv` subset.

## Completed In This Pass

The first parity correction is candidate arbitration. The original R pipeline
uses `stpd_event_core_weighted_select()` to choose the highest-value compatible
set of candidate intervals. The previous Swift implementation used a greedy
priority sort, which can reject two lower-priority non-overlapping candidates
even when their combined value is higher than one broad overlapping candidate.

Swift now mirrors the R weighted interval selection structure:

- group by train;
- keep valid candidate intervals;
- sort by `end_isi`, then `start_isi`;
- compute the previous compatible interval index;
- dynamic-program the maximum total candidate value;
- mark selected rows with
  `selected_by_event_core_weighted_interval_grammar`.
