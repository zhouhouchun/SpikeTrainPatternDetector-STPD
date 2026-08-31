# v2.3 clean-overlap audit response

v2.2.0 remains frozen and unchanged. v2.3.0 responds to the finding that several v2.2 primary supports, especially Tonic versus Pause and eligible Tonic versus direct HFS, were separable by a low-complexity single-ISI oracle. The response is a separate clean-overlap supplement rather than a retrospective alteration of the original mechanism benchmark.

## Targeted remediation

- **Tonic/HFS overlap:** both States retain the shifted-gamma family and common refractory term. Pre-frozen `core`, `boundary`, and `deep` rate strata increase overlap while regularity, duration, local context, and State identity remain available structural evidence.
- **Burst context:** weak/moderate/strong generation is defined by target local mean-ISI contrast. Direct standalone and HFS-internal Bursts retain observed shoulders on both sides; the contextual macro retains the outer shoulders and its explicit separator boundary.
- **Burst phenotype audit:** injected-event null thresholds are matched to standalone versus HFS context, realized event-window size, and usable flank pattern. Evidence flanks are restricted to non-event intervals in the same generator run. Every threshold is fitted using development templates only. A separate maximum-window HFS null remains available for natural null excursions, which are never promoted to primary observable Burst positives.
- **Pause semantics:** canonical Pause is not shortened and remains `[2.8, 5.0]B`. Complex Pause remains primary. Contextual separators and new `[0.72, 1.55]B` borderline gaps are secondary targets only and are excluded from the primary Pause macro-average.
- **Estimand transparency:** strict-mechanism, observable clear/eligible, ambiguous, and ambiguous-as-positive interval masks are exported. Ambiguous regions are retained rather than removed from the release.
- **Holdout protection:** templates 1–5 fit audit thresholds; templates 6–20 performance metrics only describe frozen generalization and detector performance. Pre-specified structural-integrity checks apply to every template and are not performance selection. The development-only single-ISI baseline is a diagnostic comparator, not an acceptance criterion.

## Interpretation boundary

Phenotype classes are evidence categories under the pre-specified audit, not declarations that a pattern is universally visible or invisible. Lower single-ISI separability is evidence that overlap increased; it is not itself proof that the resulting patterns are biologically realistic or that a detector must improve.

The supplement deliberately does not add stimulation, nonstationary drift, timestamp jitter, missing or false spikes, unit merge/split errors, or other acquisition effects. Those perturbations require a separately versioned realistic stress benchmark so that their failure modes remain attributable.

No STPD prediction is read during generation, null fitting, phenotype labeling, rejection, visual review, or acceptance. The generator is accepted by frozen ontology, overlap, context, split, RNG-isolation, scale, integrity, and reproducibility contracts—not by improvement in STPD F1. All QC figures are rendered locally by the included R/ggplot2 scripts.

The v2.3 HFS direct-support definition is explicitly versioned because the slow-overlap stratum extends beyond the v2.2 support ceiling. Cross-version direct-support comparisons require one common mask. In addition, v2.3 uses revised semantic RNG keys and must be treated as a separately frozen related benchmark rather than a component-level paired redraw of v2.2.
