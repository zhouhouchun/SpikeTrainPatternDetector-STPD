# STPD multi-provider and auditable-review architecture plan

## Status and scope

- Status: implementation plan approved after two independent reviews. Phase 0,
  Phase A, Phase B1, the enabled Phase B2 adapter subset, headless Phase C
  adjudication v2, the Phase D scientific composer, and Phase E provider
  workbench/scoring/UI implementation and release regression are complete;
  Phase F is the next implementation boundary. Native and calibration routes
  remain explicitly gated rather than being counted as Phase B2 deliverables.
- Scientific owner: user.
- Implementation owner: Zhou Houchun.
- Scope: organize STPD as a multi-algorithm, parameter-calibrated, human-in-the-loop workbench without changing the frozen native scientific output during infrastructure phases A--C.
- Non-goals for the first release: dynamic executable plugins, ensemble voting, cross-provider score ranking, automatic provider-result fusion, or promotion of recurrent regimes to authoritative biological States.

### Frozen implementation checkpoint

- Phase-0 pre-provider baseline commit: `7b54932`.
- Architecture-plan commit: `65a4985`.
- Phase-A provider-contract commit: `9b3399b`; contract SHA-256:
  `29a2a660ee594494c6687433db1448825bf5ac5f518f36344bdf81f65ee01a6b`.
- Phase-B1 normalization protocol SHA-256:
  `5b0cc6ca2fe6bdf2bb7bd2bddb066632ea3b0646b1096be87c77bec292a26877`.
- Phase-B1 standard snapshot SHA-256:
  `dceced8315cdaaf5b9bed7450c49ab5ada83d0d737de2331e4c8767ea7c3f994`.
- Phase-B1 standard interval-coverage SHA-256:
  `e05615a412fccfefd26cc38d7d9fb5b356260b72a88b0e512729f4fb404e0daa`.
- Phase-B2 frozen source leaves: R82
  `34073b3bb9cc0fdef5d2712bcfb34dec74c78caed71deed3c4b1209eb0e11708`,
  R83 `fc3901462cbd5b418ef9baae5e6a19603f7e944456f91af9563777ed856be8d8`,
  and R84
  `4e64ecacd8f9ec78e3446f5faa0c5c7faa5903f53ab435d93b093418228c7858`.
- Phase-B2 enables the automatic-prediction and candidate-support routes for
  Mean-ISI, LogISI/newBD, external interval, and external per-ISI providers.
  Native STPD and both support-to-native calibration routes remain disabled.
- Phase-C adjudication source leaf `R/85_provider_adjudication.R` SHA-256:
  `7be6958083e3a13607e5a0e40f7a03b559699976bec64f7864a8d8fc9d534252`.
  The v2 contract SHA-256 is
  `62523137fa3052d2428b277264bc1bdc67f2644c3ff3d780a5306ce89cb2f0da`;
  its focused test leaf SHA-256 is
  `c16417c625e59118b4db51b20e3a6b4767fadc8df7234ff47b3e5d61781c4254`.
  All 113 sorted test files passed in three fresh-R shards; the source package
  built and `R CMD check --no-tests --no-manual` completed with only the known
  Apple-Clang/R-header warning.
- Phase-D scientific-composer source leaf
  `R/86_provider_scientific_composer.R` SHA-256:
  `e6826569e88fffdaa7ca3c9ca38cb7fc51ea0c6d704d52ab37a7b5ac41d2f37b`.
  Its v1 contract SHA-256 is
  `0f420df2fcfaa9b7c74a99a3f9224bfcc581cabd5af04623c725cb3264b199a9`;
  its focused test leaf SHA-256 is
  `677ac4e1dceaa03d6dd413a55f85cf40782b0db60dbc257a97ac5df76108371e`.
  All 114 sorted test files passed across three fresh-R shards (the six files
  interrupted only by a denied user-cache write were rerun with an isolated
  temporary cache); the source package built and
  `R CMD check --no-tests --no-manual` completed with only the known
  Apple-Clang/R-header warning.
- Phase-E workbench source leaves: `R/87_provider_workbench_phase_e.R`
  `fc8629072eec070c4b07a18c2f862c3166f093a8c60cddd2a40fe1def8a9145a`
  and `R/88_provider_scoring_export_phase_e.R`
  `844a6ac6c8de4f3776a88e886eb0c16c6c486e73f7dfd110983e8d68a324255b`.
  The Phase-E normative contract SHA-256 is
  `bddb9611451ee8d9c1d5fdd74ace0632a30adfdf4ea434e9862329479a3b957c`;
  its focused test leaf SHA-256 is
  `9ec1ce67194922522e5e6a2dd8bc8dfeb182a1be4b8f9bac32af18395376e513`.
  All 115 sorted test files completed across three fresh-R shards. The only
  initial failures were the intentionally frozen static-UI count and one exact
  translation key after adding the Phase-E panel; both contracts were updated
  and their affected i18n/UI/Phase-E tests passed on the final rerun. The source
  package built, and `R CMD check --no-tests --no-manual` completed with only
  the known Apple-Clang/R-header warning.

Both final independent reviews returned GO with no P0/P1 finding. The scientific
review additionally confirmed that Phase B1 introduces no Burst, Pause, Tonic,
HFS, ISI-threshold, regularity, contrast, or minimum-spike decision. The
software/interoperability review confirmed deterministic normalization and
fail-closed transaction semantics. Its non-blocking Phase-B2 requirements are:

1. expose only a batch-level importer, not free caller-supplied coordinate specs
   or flags claiming that data were already validated;
2. derive each raw source record and its coordinate declaration in the same
   allowlisted adapter;
3. bind `adapter_code_sha256` to adapter code and all B1 protocol/normalizer
   dependencies;
4. add stricter adapter-level artifact byte limits and a scaling/complexity
   regression before public import is enabled.

## Product definition

STPD is not only an event grammar. Its product architecture has five distinct roles:

1. **Detector provider**: consumes spike-train input and emits a provider-owned automatic assertion, candidate, or evidence artifact.
2. **Calibration provider**: derives a frozen parameter bundle from Mean-ISI, LogISI/newBD, manual examples, or another declared source.
3. **Adjudication layer**: records append-only human decisions over one immutable provider result and materializes a separate reviewed product.
4. **Composer**: represents Event, State, Gap, Review, and descriptive Regime objects and their relationships without treating one track as a global label winner.
5. **Reference store**: contains sealed evaluation reference data. It is physically and semantically separate from provider output and reviewed predictions.

Provider output, calibrated parameters, reviewed output, and reference truth are never interchangeable.

## Required scientific semantics

### Event and State

- Burst is an Event; Broad HFS, HFT/HF-irregular subtypes, and Tonic are States; Pause is a Gap.
- A Burst label does not itself cut Tonic or Broad-HFS support. State boundaries require independent label-blind frequency/regularity/support evidence, a canonical Pause direct-support boundary, or QC/acquisition failure.
- Removing the Event track must not change native State geometry.
- Recurrent Bursting and Recurrent Pausing are derived only after their child Events/Gaps have been accepted. Children are retained.
- Recurrent products remain a separate, descriptive, non-authoritative Regime layer until independently validated.

### HFS and Pause geometry

- Canonical Pause never belongs to Broad-HFS direct support.
- Disjoint direct-support segments may be represented within a higher episode envelope or by an explicit relationship, according to one versioned composer policy.
- Direct-support and episode-envelope metrics are always reported separately; Pause duration never contributes to HFS direct-support performance.

### Manual work and validation

- A human may revise the result of any provider, including native STPD, Mean-ISI, LogISI/newBD, or a strictly imported external result.
- Human revision creates an adjudicated/manual-aware product and never overwrites provider AUTO output.
- Algorithm-assisted annotation is disclosed as such and cannot serve as independent detector truth for the same run.
- Detector performance and adjudicated agreement are different estimands and fail closed when their authority requirements are mixed.

## Versioned provider bundle

Use small normalized tables rather than one unbounded wide table:

1. `provider_runs`: provider and adapter identity/version; declared capabilities and output role; input, parameter, calibration, raw-artifact, and dataset-snapshot hashes; generation mode; label-blind/manual-aware status.
2. `candidate_intervals`: provider-run-qualified record ID; record kind; provider decision; train; semantic track; proposed label; canonical geometry; original-row identity; uncertainty metadata.
3. `thresholds`: named value, unit, scope, source, and calibration-bundle binding.
4. `normalization_audit`: original coordinate convention, index base, interval closure, unit, transformation, tolerance, ambiguity and typed status.
5. Provider-specific evidence attachments referenced by a manifest, not embedded as unconstrained core JSON.

Different-provider scores are non-comparable by default. Identical geometry never causes automatic deduplication or fusion.

## Coordinate contract to freeze before implementation

- Canonical ISI index is the existing train-row ISI index; row 1 normally has no preceding ISI.
- Mean-ISI/LogISI `diff(timestamp)` coordinates require an explicit adapter mapping to canonical train-row coordinates.
- Every input declares `coordinate_convention`, `index_base`, `interval_closure`, and time unit.
- Original spike, ISI, and time coordinates are preserved alongside canonical geometry.
- Unit omission, out-of-range coordinates, non-monotonic or duplicate timestamps, ambiguous time alignment, or dataset/train/timestamp hash mismatch fail closed.
- Time boundaries are never silently snapped to the nearest spike.

## Supported first-release modes

1. `native-only`: current STPD native result, exposed through a read-only adapter after the existing native composer.
2. `external-only`: imported structured predictions remain that provider's automatic assertions and may be reviewed without STPD reclassification.
3. `support-to-native`: a calibration-provider run produces a frozen parameter snapshot, which is then consumed by a separately identified native STPD run.

Mean-ISI and LogISI/newBD remain support providers by default. Selecting either as a primary algorithm is an explicit source-selection action. Automatic ensemble/fusion is deferred.

## Adjudication v2

Do not modify the existing Phase-2B v1 hash contract. Create a provider-agnostic v2 product keyed by `provider_run_id + source_record_id`.

First supported actions:

- `accept_as_is`
- `reject`
- `adjust_bounds`
- `void_prior_decision` as append-only compensation

Boundary adjustment creates a new derived interval with lineage to the immutable source geometry. It cannot cross train, dataset snapshot, or QC/acquisition hard boundaries. Unsupported `split`, `merge`, `relabel`, and `create` actions return stable typed errors until separately versioned.

## Implementation phases and stop gates

### Phase 0 -- pre-provider scientific baseline

1. Complete or remove incomplete current nested-HFS changes by applying the accepted scientific rules rather than leaving mixed behavior.
2. Keep Event-Regime as a descriptive post-FINAL product and reconcile all normative documentation with current Event/State and HFS/Pause decisions.
3. Resolve every full-regression failure, run source installation and `R CMD check`, and freeze the canonical scientific payload hash.
4. Commit a clean `pre-provider` baseline before changing provider/review architecture.

Stop gate: no unresolved semantic contradiction, no failing regression, no untracked scientific source, and a reproducible canonical native baseline.

The reconciliation is additive: the historical v3.1 contract bytes remain
unchanged, while `gate-b-v3-2-semantic-overlay-contract.md` records the active
scientific supersession and its migration boundary.

### Phase A -- contracts and prototypes only

Freeze the provider bundle, coordinate mapping, authority model, calibration lineage, adjudication v2 actions, scientific hash scope, and compatibility rules. Add schema prototypes and contract tests without changing native detection.

Stop gate: contracts, prototypes, registry, tests, and documentation agree byte-for-byte on versioned enums and fields.

### Phase B1 -- strict normalization

Implement pure interval, time, and per-ISI normalizers with resource bounds and typed fail-closed errors. No UI and no composer integration.

Stop gate: correct round-trip where mathematically reversible; otherwise raw artifact preservation plus deterministic normalized geometry and complete conversion audit.

### Phase B2 -- allowlisted adapters

Implement explicit adapters for Native STPD, Mean-ISI, LogISI/newBD, and pure-data external import. Do not execute code from imported files and do not create a global mutable plugin registry.

Stop gate: native canonical scientific hash unchanged; each provider can coexist on identical geometry with independent IDs and provenance.

### Phase C -- adjudication v2

Implement accept, reject, boundary adjustment, compensation, stale detection, replay, and export. Preserve Phase-2B v1 as a read-only compatibility adapter.

Stop gate: provider AUTO bytes and hashes remain unchanged; full history is deterministic and tamper-evident; label-blind detection is invariant.

### Phase D -- scientific composer migration

Introduce any new provider-independent Event/State/Gap relationships and Regime composition under a new policy/schema version. Do not send the already-composed native product through the composer a second time.

Stop gate: paired old/new scientific comparison, explicit user decision record, no recursive regime expansion, and preservation of all child Events/Gaps.

### Phase E -- UI, export, and scoring

After headless APIs stabilize, allow the user to select a provider result as a review backdrop, retain all other provider runs, and display AUTO versus adjudicated differences and authority. Refactor scoring to consume normalized prediction bundles rather than adding provider-specific switches.

Stop gate: provider, track, label, and estimand are never pooled implicitly; failures cannot leave a seemingly complete manifest.

Implemented by `R/87_provider_workbench_phase_e.R`,
`R/88_provider_scoring_export_phase_e.R`, and the Shiny provider-workbench
module in `R/51_server_modules.R`. The normative Phase E product and authority
contract is `docs/provider-workbench-scoring-v1-contract.md`.

### Phase F -- release freeze

Run full regression, source install, `R CMD check`, deterministic reruns, perturbation and scaling checks, and regenerate frozen simulated and real-data reports with versioned input/parameter/result hashes.

Implemented on 2026-08-28. The frozen evidence is generated by
`test-results/release_freeze_phase_f/phase_f_input_audit.R`,
`run_phase_f_perturbation.R`, `build_phase_f_report.R`, and
`finalize_phase_f_release.R`. The release uses synthetic mechanism benchmark
v2.1.0 (20 paired templates, protected templates 6--20, independently scored
1x/4x/10x projections, 2,000 paired-template bootstrap draws) and the latest
single-patient bilateral-STN reference (23 Event-eligible trains in 13
recording groups; 7 State-eligible trains; 2,000 recording-group bootstrap
draws). The latter remains a single-rater draft and is not described as a
biological gold standard or external patient-level validation.

The simulation calibration/scoring rerun and all real-data scientific outputs
are deterministic; elapsed-time and run-container metadata are explicitly
excluded from scientific equality. All 115 test files pass, including the
10,000-ISI import and 50,000-candidate materialization checks. The only skipped
test is an explicitly opt-in legacy frozen-baseline rerun superseded here by
the current complete LOGO validation. Package build, isolated source install,
`R CMD check`, input/result hashes, and the portable technical report are
recorded in `test-results/release_freeze_phase_f/phase_f_release_manifest.json`.

Release interpretation is bounded: Burst, Pause, and Broad HFS are the primary
reported families; HFT/HF-irregular remain descriptive Broad-HFS subtypes, and
Tonic remains exploratory because its current synthetic and real reference
performance is weak. The defensible claim is an auditable, calibratable,
provider-compatible candidate detector with internal validation, not universal
automatic classification or cross-patient biological generalization.

## Minimum acceptance suite

- Native thresholds, candidates, Event/State/Gap scientific payload and canonical hash are unchanged through phases A--C.
- Adapter attachment never mutates the source native dataset.
- Mean-ISI/LogISI first and last boundaries map correctly from `diff(timestamp)` to canonical row-based ISI coordinates.
- Interval/time/per-ISI imports reject ambiguous units, closure, index base, alignment, or identity.
- Equal IDs or geometry from different providers remain distinct.
- Provider raw artifact and AUTO hashes remain unchanged after every review action.
- Accept/reject/adjust/compensation history replays deterministically; stale parent or precondition fails closed.
- Unsupported actions return typed errors.
- Manual/reference data cannot enter label-blind detection or held-out calibration.
- Calibration and held-out groups are disjoint whenever supervised/manual parameter learning is used.
- Removing Event output does not change State geometry.
- Canonical Pause is excluded from HFS direct support while higher-level episode context remains auditable.
- Recurrence consumes completed child entities once, preserves them, and never recursively grows from its own parents.
- Detector-performance and adjudicated-agreement authority cannot be mixed.
- Legacy APIs remain readable and are labelled as lossy where appropriate.
