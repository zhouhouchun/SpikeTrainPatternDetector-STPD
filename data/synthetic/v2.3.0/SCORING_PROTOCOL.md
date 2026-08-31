# Scoring protocol v2.3.0

## Statistical unit and scale reporting

Report 1x, 4x, and 10x separately. Use `Template_ID` as the paired comparison and cluster-bootstrap unit. The three exact projections of one template are repeated numerical tests and must never be counted as three independent recordings.

For a strict scale-equivariance test, freeze the detector and parameter contract before running the three blinded batches, reset any mutable detector state between batches, and do not manually tune absolute-time thresholds per sheet. A separately recalibrated per-scale workflow may be reported as a distinct estimand, but it is not the same test as frozen scale equivariance.

## Primary estimands

Report both primary estimands without selecting the better result:

1. **Strict mechanism:** every injected primary Burst/Pause Event and every generated Tonic/Broad HFS State.
2. **Observable phenotype:** clear injected-pulse Burst-like support and phenotype-eligible Tonic States under the frozen timestamp-only audit; Broad HFS State truth remains explicit. HFS null-model excursions are a separate secondary audit, not primary observable Burst positives.

Always disclose eligible/clear, ambiguous, and no-evidence counts by development/holdout split, Burst context and strength, Tonic subtype and overlap stratum, and HFS overlap/regularity stratum. Phenotype classes are specific to the pre-specified audit.

## Sensitivity analyses

Use `truth/phenotype_sensitivity_masks.csv` to report at least:

- ambiguous regions excluded from the primary observable analysis;
- ambiguous regions treated as positive;
- strict mechanism positives irrespective of phenotype evidence.

Contextual separators and borderline gaps are secondary semantic targets. Score each subtype separately and exclude both from the primary Pause macro-average. A product-level prediction of Pause on these intervals may be reported in the secondary analysis, but it does not become a primary Pause true positive.

## Metrics

For each primary target report:

- ISI-support precision, recall, and F1;
- episode/state IoU precision, recall, and F1 at pre-specified IoU thresholds;
- onset and offset boundary error in both seconds and units of `B`;
- fragmentation/false-split and merge rates;
- coverage of ambiguous and no-evidence truth.

Background and all non-target primary regions are explicit negatives. Predictions in Background count as false positives. For Broad HFS, report direct support, tolerated interruption, nested Burst support, outer State envelope, and boundary observability separately. The v2.3 direct-support mask uses the explicitly versioned slow-overlap contract; the exported v2.2-style `<=0.45B` mask is audit-only. Do not directly compare v2.2 and v2.3 direct-support F1 under different support definitions. Use a common legacy mask or compare the outer State envelopes. Canonical Pause divides left and right HFS States even when both belong to one `Composite_HFS_Regime_ID`.

## Development, holdout, and baselines

Templates 1–5 may be used for calibration and threshold fitting. Templates 6–20 are holdout and their performance metrics must not influence parameter choice, phenotype thresholds, model selection, or sample filtering. Pre-specified ontology and structural-integrity checks apply to all templates, including holdout. Scale copies of one calibration episode use the same `Episode_ID`.

The single-ISI baseline in `qc/single_isi_dev_holdout_baseline.csv` is fitted on development templates and evaluated on holdout templates. It is a descriptive lower-complexity comparator, not generator truth, detector calibration, or an acceptance target.

Primary release comparisons may contrast a frozen detector on the separately frozen v2.2 and v2.3 benchmarks, separately calibrated workflows using the same development-ID policy, and generator-only overlap/phenotype audits. Because v2.3 uses revised semantic RNG keys, this is not a component-for-component paired redraw; do not attribute an individual timestamp difference to only one generator change. No increase in STPD F1 is required for generator acceptance.
