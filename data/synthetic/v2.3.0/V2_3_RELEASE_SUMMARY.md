# v2.3.0 release summary

v2.3.0 is a frozen clean-overlap supplement. It is distributed separately from v2.2.0 and does not overwrite the v2.2 timestamps, truth, parameters, or results.

## Release contents

The release contains 20 dimensionless mother templates and 60 exact paired 1x/4x/10x projections. Each scale batch contains 3,210 spikes. At the mother-template level, individual trains contain 140–174 spikes and end at `61.353–75.805B`; at the anchor scale this is approximately 6.135–7.580 s.

| Truth object | Dimensionless count | Design note |
|---|---:|---|
| Burst Events | 100 | 60 standalone; 40 nested in HFS |
| Primary Pause Events | 50 | 40 canonical; 10 complex multi-gap |
| Tonic States | 40 | two subtypes; all retained in strict truth |
| Broad HFS States | 60 | three per template |
| Contextual separators | 20 | secondary only |
| Borderline gaps | 20 | secondary only |
| Composite HFS Regimes | 10 | never continuous HFS States |

Canonical Pause remains one observed `[2.8, 5.0]B` gap bounded by two observed spikes. Realized borderline gaps are `0.7580–1.4891B`, below the canonical range, and contextual separators are `0.4804–0.6485B`. Neither secondary subtype is a primary Pause positive.

## Burst contract and audit

Realized Burst size is 4–9 spikes; 12 Events contain exactly four spikes and none exceeds 10. Realized duration is `0.1013–0.8732B`. Target mean-ISI contrast falls within the frozen weak (`2.2–3.0`), moderate (`3.0–4.0`), and strong (`4.0–5.5`) ranges. No pulse is marked refractory-limited.

Development-only, context- and window-matched phenotype thresholds classify the 100 injected Bursts as follows:

| Context | Clear burst-like | Ambiguous burst-like | No burst evidence |
|---|---:|---:|---:|
| Standalone | 58 | 2 | 0 |
| Burst in HFS | 36 | 3 | 1 |
| Total | 94 | 5 | 1 |

The separate scan of 20 non-injected HFS States yields 3 clear, 0 ambiguous, and 17 no-evidence null-model excursions. These are audit categories under the frozen shifted-gamma null, not additional mechanism or primary observable Burst truth. The primary observable Burst table therefore contains only the 94 clear injected pulses.

## Tonic and HFS audit

Tonic timestamp-only phenotype results are:

| Subtype | Eligible | Ambiguous | No evidence |
|---|---:|---:|---:|
| `generic_stress` | 10 | 7 | 3 |
| `stn_like_empirical` | 14 | 6 | 0 |
| Total | 24 | 13 | 3 |

All 40 Tonic States remain in strict mechanism truth. The 24 eligible States form the primary observable Tonic estimand; ambiguous and no-evidence States remain available through the strict and sensitivity masks.

Every template contains one `core`, one `boundary`, and one `deep` Broad HFS State, and one State in each regularity regime. Across HFS intervals, 1,448 are v2.3 direct support, 51 are tolerated short interruptions, and 227 are nested Burst support; 1,260 intervals also satisfy the exported v2.2-style `<=0.45B` legacy audit mask. HFS boundary observability classifies 24 States as clear/hard-cut, 28 as ambiguous/edge, and 8 as having no evidence at one or more boundaries. Four macro boundaries that would otherwise place two separately scored States of the same primary type directly adjacent receive an explicit Background separator, avoiding an unobservable truth split.

## Development–holdout single-ISI audit

Thresholds in the following table were selected using templates 1–5 and evaluated without refitting on templates 6–20. Values are holdout template-equal balanced accuracy with a template-cluster bootstrap interval. They are descriptive diagnostics, not acceptance targets or detector results.

| Comparison | Holdout balanced accuracy | Cluster interval |
|---|---:|---:|
| Burst vs Broad HFS direct | 0.807 | 0.792–0.824 |
| Broad HFS direct vs Tonic strict | 0.785 | 0.754–0.814 |
| Broad HFS direct vs Tonic observable | 0.829 | 0.792–0.862 |
| Tonic strict vs all primary Pause | 0.958 | 0.911–0.993 |
| Tonic strict vs canonical Pause | 1.000 | 1.000–1.000 |
| Tonic strict vs complex-Pause internal ISI | 0.865 | 0.731–0.967 |
| Tonic strict vs borderline gap | 0.763 | 0.625–0.887 |
| Tonic observable vs borderline gap | 0.771 | 0.635–0.897 |

Canonical Pause remains intentionally distinct. The overlap challenge is supplied by Tonic/HFS strata, complex-Pause internal ISIs, and separately scored borderline gaps rather than by relabeling short gaps as canonical Pause.

## Reproducibility and scope

The release exports component-keyed RNG provenance and CSV-round-trip-stable local ISI hashes. Counterfactual acceptance checks isolate Tonic, HFS, and borderline-gap changes from unrelated component-local sequences. All scale projections are exact and all calibration copies use the same Episode IDs. The v2.3 semantic RNG-key layout differs from v2.2, so this supplement is not a component-for-component paired redraw of v2.2.

No STPD output was used for generation or acceptance. No stimulation, slow drift, observation noise, missing or false spikes, timestamp jitter, or sorting error is present. All figures in `qc/figures/` are rendered locally by the included R/ggplot2 code.

`tests/verify_v2_3.R` validates ontology, overlap strata, Burst context/contrast and origin separation, primary-versus-secondary gap semantics, versioned HFS support, observable State boundaries, phenotype thresholds, RNG isolation, exact scaling, calibration IDs, detector-input blinding, XLSX integrity, figures, canonical checksums, and the immutable release hash lock.
