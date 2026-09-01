# Tonic State repair and threshold-preview validation record

Date: 2026-08-30

## Frozen purpose

This stage addressed a bounded diagnostic question: recover physiologically
regular Tonic State at separable Broad-HFS edges without converting every
HFS-adjacent regular fragment into Tonic. Burst and Pause remain Event tracks;
Broad HFS and Tonic remain State tracks. The stage did not redefine Burst,
Pause, or the Broad-HFS parent estimand.

The threshold preview is explicitly exploratory. It reruns selected trains in
memory and reports expected Event/State changes, but it is non-authoritative,
does not adopt a threshold, and does not overwrite a formal detector result.

## Implemented detector contract

1. An independent Tonic candidate may cut a Broad-HFS parent only at a
   separable edge transition. Internal or covering conflicts remain HFS-side
   vetoes. Residual HFS support must pass the frozen HFS gate again.
2. The ordinary Tonic MM ceiling remains 1.25. Strongly regular candidates may
   use the prespecified 1.40 fallback. A ceiling above 1.40 requires at least
   10 calibration Tonic episodes from at least four recording groups, complete
   full-sample and group-LOGO q95 evidence, and remains capped at 1.50.
3. The relaxed MM route retains hard guards of CV <= 0.30 and LV <= 0.15. An
   invalid or incomplete contract fails closed to 1.40.
4. The short-Tonic route remains evidence-bound. It is not globally enabled by
   lowering the minimum spike count.

Primary implementation:

- `R/42_hfspiking_protection_and_manual_coexistence.R`
- `R/07_manual_param_estimation.R`
- `R/38_event_grammar_core.R`
- `R/26_parameter_registry.R`
- `R/49_spiketrainpattern_product_schema.R`
- `R/65_multitrack_compatibility_shadow.R`

## Threshold-preview contract

The Tonic CV/LV/MM preview now reports, per parameter variant:

- numeric direction and scientific direction (`relaxed` or `tightened`);
- configured value, detector-applied value, contract status, and whether the
  variant was actually run;
- Tonic and Broad-HFS State-episode changes;
- direct-support ISI changes;
- Tonic-Broad-HFS overlap-resolution changes;
- a dry-run authority flag, baseline parameter hash, and context SHA-256.

MM variants above 1.40 without a valid frozen calibration contract are marked
`invalid_contract`; they are not plotted as if the detector had used them, and
the expected runtime fallback to 1.40 is recorded. Changing data, formal run
state, learned ranges, or UI parameters invalidates the old preview. Detailed
tables are visible in the UI and exported as separate CSV files.

Primary implementation:

- `R/53_parameter_delta_preview.R`
- `R/54_parameter_sensitivity_validation.R`
- `R/18_ui.R`
- `R/19_server.R`

## Synthetic validation

Frozen inputs were v2.2.0 and v2.3.0. The 1x, 4x, and 10x scales were run and
reported separately. Their identical dimensionless structures mean that the
three scales are robustness transformations, not independent biological
replicates. Confidence intervals use 2,000 template-cluster bootstrap draws.

### v2.2.0, strict mechanism truth

- Tonic episode F1 at IoU >= 0.50: 0.5366 -> 0.5714
  (TP 11 -> 12; FP remained 0; FN 19 -> 18).
- Tonic direct-support F1: 0.6108 -> 0.6525
  (precision 0.9941, recall 0.4856; 95% CI 0.3811-0.8171).
- Burst and Pause strict metrics were unchanged.
- Broad-HFS direct-support F1 changed by approximately -0.00039; this is a
  one-ISI tradeoff, not a material State regression.

### v2.3.0, strict mechanism truth

- Tonic episode F1 at IoU >= 0.50: 0.2857 -> 0.5366
  (TP 5 -> 11; FP remained 0; FN 25 -> 19; 95% CI 0.2857-0.7234).
- Tonic direct-support F1: 0.3462 -> 0.5904
  (precision 0.9423, recall 0.4298; 95% CI 0.3358-0.7593).
- Burst and Pause strict metrics were unchanged.
- Broad-HFS direct-support F1 improved from approximately 0.7973 to 0.8004;
  episode F1 at IoU >= 0.50 was unchanged.

Result roots:

- `test-results/clean_synthetic_validation/runs/2026-08-30_tonic_state_repair_final_01/v220_stage_c`
- `test-results/clean_synthetic_validation/runs/2026-08-30_tonic_state_repair_final_01/v230_stage_c`

## Real-data LOGO validation status

The authoritative manual-reference workbook SHA-256 was
`42d246192b9051b82fc979e696619d804051503a21ca65183fd59abaa781d30e`.
The aligned raw spike CSV SHA-256 was
`25d1bd25cbf8d7f5fa11f8af1f23c815222f3c043298d586464cfc2577971953`.

Current reference scope: one patient, bilateral STN, 13 recording groups, 23
trains, and 16,705 ISIs. After the short manually labelled Tonic fragments are
partitioned to `tonic_like_review`, all 23 trains remain reference-eligible on
at least one formal axis and six trains are State-reference-eligible. The 77
Tonic-like ISIs are retained descriptively and are not recoded to `other`.

The first rerun tag (`tonic_edge_mm_20260830_r1`) was invalid because the
runtime and test-results helper copies were not synchronized. The subsequent
`tonic_edge_mm_wired_20260830_r2` run fixed that software wiring but still used
10 real Tonic-labelled fragments per calibration fold. Under the clarified
scientific estimand, those fragments are not sustained Tonic States. Therefore
both r1 and r2 are historical diagnostic runs, not final real-data validation,
and their F1 values or prediction hashes must not be cited as confirmatory
results.

The current real LOGO entry point now fails closed to
`tonic_reference_role=tonic_like_review`: calibration sampling contains Burst,
Pause, Broad HFS, and Other only; Tonic thresholds use predeclared defaults;
formal metrics contain Burst, Pause, and Broad HFS only. A new tagged real LOGO
run is mandatory after the detector/HFS source is frozen. The report and
reproducibility manifest must bind that explicit tagged directory and reject an
old canonical-directory fallback.

The short manually labelled Tonic fragments are retained only as a
`tonic_like_review` descriptive audit. Their historical direct-support F1
(0.0545) and episode F1 (0.0488) are not confirmatory Tonic-State endpoints and
must not be used to tune CV/LV/MM, duration, short-route, or State/HFS edge
rules.

Historical diagnostic result root, not valid for final citation:

- `test-results/publication_validation/real_PD_STN_reference_eligible_tonic_edge_mm_wired_20260830_r2`

No final real-result root is declared in this document until the new tagged
LOGO run passes the tonic-like calibration and confirmatory-endpoint gates.

## Decision and limitations

The detector repair remains a candidate implementation. The previously written
v2.2.0/v2.3.0 Tonic numbers above are strict-mechanism diagnostics generated
before the Stage-C `phenotype / eligible_primary / tonic` scoring rows were
added. They do not by themselves establish the confirmatory Tonic endpoint.
Stage C must be rerun from frozen, label-blind Stage-B predictions; only the
phenotype-eligible Tonic support and episode rows may be cited as confirmatory.
Strict-mechanism Tonic remains a secondary sensitivity analysis.

The real manual Tonic fragments are too short to represent a typical sustained
Tonic State (77 ISIs in 25 episodes; median 3 ISIs and 0.0936 s), so their poor
historical match is expected and is not evidence for further detector
relaxation.

The real-data result must not be described as external patient validation:
there is one patient, sparse Tonic truth, only six State-reference-eligible
trains, and no independent second annotator. LOGO prevents recording-group
leakage but cannot estimate patient-level generalization.

Formal Tonic-State validation therefore requires a newly materialized result
using sustained phenotype-eligible synthetic episodes. Real short fragments
remain available for qualitative review only. The preview may still expose the
consequences of alternative thresholds, but it cannot promote a real short
fragment into calibration evidence. Global lowering of Tonic minimum spikes or
unconditional CV/LV/MM widening is not justified by the current evidence.

## Verification

Focused and broad regression suites covering Tonic, HFS, Burst/HFS nesting,
Pause/HFS coexistence, multitrack products, parameter contracts, schema/YAML,
publication helpers, delta preview, and sensitivity preview passed. Both
publication helper mirrors parse and are byte-identical; `git diff --check`
passes.
