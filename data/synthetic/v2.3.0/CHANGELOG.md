# Changelog

## 2.3.0 — 2026-08-29

- Preserved v2.2.0 as a separate frozen clean mechanism release; v2.3.0 is an additive clean-overlap supplement.
- Added pre-frozen `core`, `boundary`, and `deep` Tonic/HFS rate-overlap strata while retaining the shared shifted-gamma family and refractory term.
- Rotated all three HFS overlap strata across regularity regimes within every template.
- Replaced Burst multiplier bands with explicit weak/moderate/strong target mean-ISI contrast bands and expanded context buffers.
- Added observed Burst shoulder counts and a contract requiring usable local context around direct standalone and HFS-internal Bursts.
- Clipped contextual-macro latent pulse windows to their retained generator runs and exported `Latent_Window_Truncated`, preventing latent support from extending into the explicit separator.
- Kept canonical Pause unchanged at one observed `[2.8, 5.0]B` gap and retained complex multi-gap Pause as primary truth.
- Added one pre-frozen Tonic-tail-overlap `Borderline_Gap` per template as secondary truth only; contextual separators remain secondary only.
- Added a scheduler integrity rule that inserts an audited Background separator when two separately scored States with the same primary label would otherwise touch at an unobservable boundary.
- Added explicit strict, observable, ambiguous, and ambiguous-as-positive interval sensitivity masks.
- Made injected-Burst phenotype thresholds context- and window-size-matched, with every threshold fitted on development templates only.
- Matched injected-Burst nulls by usable one- versus two-sided flank pattern, restricted evidence flanks to non-event ISIs in the same generator run, and separated natural HFS null excursions from primary observable Burst positives.
- Versioned the expanded v2.3 HFS direct-support contract and exported a v2.2-style legacy audit mask; direct-support F1 is not compared across versions under different definitions.
- Added an immutable release hash lock that the generator cannot rewrite.
- Clarified that revised v2.3 semantic RNG keys make this a separately frozen supplement, not a component-for-component redraw of v2.2.
- Added a development-fitted, holdout-evaluated single-ISI separability baseline with template-cluster bootstrap intervals; it has no acceptance target.
- Added split-specific overlap, phenotype, gap, and ontology audits and a locally rendered R/ggplot2 ISI-overlap figure.
- Kept stimulation, slow drift, and all observation/sorting noise disabled; no STPD output is used by the generator or acceptance suite.

## 2.2.0 — 2026-08-28

- Preserved v2.1 as a separate frozen release.
- Added component-keyed random streams and component-local ISI hashes.
- Added counterfactual RNG-isolation acceptance test.
- Added Tonic continuous evidence, eligibility classes/reasons and parallel State estimands.
- Added HFS interruption roles and boundary observability audit.
- Added canonical-Pause hard cuts and Composite HFS Regime hierarchy.
- Kept exact paired 1x/4x/10x projections, Burst quotas and contextual-secondary semantics.
