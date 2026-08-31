# Data dictionary v2.3.0

## Detector input and scale key

- `detector_inputs/spike_timestamps_blinded.csv` and `.xlsx`: detector-visible data containing only `Sample_ID`, `Spike_Index`, and `Time_s`.
- `truth/sample_template_scale_key.csv`: withheld mapping from blinded `Sample_ID` to `Template_ID`, scale factor, `B_s`, split, and recording duration.

## Primary and multitrack truth

- `truth/interval_truth_multitrack.csv`: one row per observed ISI. It contains primary State/Event labels; secondary-gap fields; Tonic/HFS overlap strata; versioned direct HFS, legacy audit support, interruption, and nested-Burst roles; Burst contrast; phenotype evidence; and exact dimensionless/scaled coordinates.
- `truth/primary_event_episodes.csv`: primary Burst and Pause episodes. Burst rows include latent and realized boundaries, frozen spike target, target mean-ISI contrast, refractory-limitation flag, `Latent_Window_Truncated`, and observed left/right context shoulder counts.
- `truth/state_envelopes.csv`: Tonic and Broad HFS State envelopes, renewal parameters, CV/CV2/LV, overlap strata, Tonic phenotype audit, and HFS boundary observability.
- `truth/composite_hfs_regimes.csv`: higher-order links between two different HFS States across canonical Pause. `Continuous_HFS_State` is always false.
- `truth/generator_provenance.csv`: run-level generator component, RNG stream, parameter, duration, and local hash provenance.

## Secondary gap semantics

- `truth/contextual_separator_secondary_episodes.csv`: contextual inter-Burst gap/separator episodes. `Primary_Event_Label` is `none`.
- `truth/contextual_separator_links.csv`: left Burst, separator, and right Burst relationship with local medians.
- `truth/borderline_gap_secondary_episodes.csv`: one-ISI Tonic-tail-overlap gaps. `Canonical_Pause_Truth` is false and `Primary_Event_Label` is `none`.

Both secondary files retain `Product_Level_Mapping=Pause` and `Secondary_Scoring_Role=secondary_target_excluded_from_primary_pause`.

## Estimands and phenotype evidence

- `truth/strict_mechanism_estimand_episodes.csv`: every injected primary Event at all scales.
- `truth/strict_mechanism_estimand_states.csv`: every generated Tonic and Broad HFS State at all scales.
- `truth/observable_phenotype_estimand_states.csv`: all Broad HFS States plus phenotype-eligible Tonic States.
- `truth/observable_phenotype.csv` and `truth/phenotype_estimand_regions.csv`: Burst evidence for injected pulses and null-model HFS excursions, including the context/window-specific threshold-set provenance.
- `truth/burst_observable_episode_audit.csv`: all injected-pulse and null-model Burst-like candidates with an explicit `Scoring_Role` and primary-observable flag.
- `truth/observable_burst_positive_episodes.csv`: only clear candidates whose origin is `injected_rate_pulse`; this is the primary observable Burst episode table.
- `truth/null_model_burst_like_excursions.csv`: spontaneous candidates from non-injected HFS States; these are secondary phenotype audits and never Burst mechanism truth or primary observable positives.
- `truth/tonic_phenotype_evidence.csv`: continuous Tonic evidence features, overlap stratum, phenotype label, and eligibility reason.
- `truth/phenotype_sensitivity_masks.csv`: interval masks for strict Burst/Tonic truth, observable clear/eligible truth, ambiguous regions, ambiguous-as-positive sensitivity analyses, Broad HFS, primary Pause, and secondary Pause semantics.

`no_burst_evidence`, `ambiguous_burst_like`, and Tonic eligibility classes are operational audit labels, not universal biological observability labels.

## Frozen metadata, calibration, and QC

- `metadata/frozen_overlap_quota.csv`: pre-generation Tonic/HFS overlap assignments by split, subtype, regularity, and nested-Burst status.
- `metadata/frozen_burst_quota.csv`: Burst context, contrast stratum, HFS regularity, and realized spike-count quota.
- `metadata/phenotype_thresholds.csv`: development-only null thresholds, context/window match, surrogate count, and detector-use flag.
- `metadata/holdout_audit_provenance.json`: development and holdout IDs, threshold statistic and tie-break, bootstrap contract, and explicit confirmation that holdout results were not used for generator acceptance.
- `metadata/component_rng_registry.csv`: component keys, RNG stream registry, and local ISI SHA-256.
- `metadata/generator_parameters.{json,yaml,csv}`: complete publication parameters.
- `metadata/reproduction_manifest.json` and `metadata/canonical_output_checksums_sha256.csv`: versioned run provenance and regenerated output hashes.
- `RELEASE_LOCK_SHA256.csv`: immutable release hashes for source, parameters, key truth, detector inputs, and figures. It is not generated or overwritten by the reproduction script.
- `calibration/calibration_same_episode_ids_all_scales.csv`: identical calibration Episode IDs projected across all three scales.
- `qc/single_isi_dev_holdout_baseline.csv`: development-fitted, holdout-evaluated descriptive single-ISI baseline; it is not an acceptance criterion.
- `qc/*coverage_by_split.csv` and `qc/overlap_contract_audit.csv`: split-specific quota, phenotype, and ontology audits.
- `qc/burst_context_strength_spikecount_phenotype.csv`: Burst phenotype coverage jointly stratified by split, context, contrast strength, and realized spike count.
- `qc/figures/*`: figures rendered locally by the included R/ggplot2 code.

`Template_ID` is the pairing and bootstrap cluster. `Sample_ID` is blinded. Columns ending in `_u` are dimensionless; corresponding `_s` columns are exact scaled seconds.
