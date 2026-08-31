# STPD provider scientific composer v1 contract

Status: Phase D headless contract and implementation.

## Scientific decision record

- Decision: approved for implementation as a descriptive, non-authoritative
  provider-independent composition layer.
- Scientific owner: project scientific owner (pseudonymous runtime identity is
  required in every materialization record).
- Approval date: 2026-08-27.
- Approved scope: one explicitly selected provider run; Event, State, and Gap
  children remain separate; accepted recurrent child patterns may create an
  additional Regime parent.
- Explicit exclusions: cross-provider fusion, detector reruns, replacement of
  native composition, recursive Regime input, and biological-ground-truth
  authority.

Every materialization additionally requires a hashed one-row decision created
by `stpd_provider_composer_decision()`. It binds the selected provider run,
AUTO/adjudicated source, exact source hashes, scientific-owner pseudonym,
rationale, approval status, and explicit RFC3339 UTC time.

## Input and authority

The composer consumes exactly one selected `provider_run_id` from a validated
provider bundle v1. Equal geometries from other runs are not pooled,
deduplicated, voted, ranked, or fused.

AUTO input is permitted only for an `automatic_prediction` run with
`automatic_prediction_record` authority. `candidate_support` cannot be promoted
through AUTO composition. It enters only after an explicit Phase C accept or
boundary-adjustment decision. An adjudicated product is manual-aware and is
eligible only for adjudicated-agreement analyses, never label-blind detector
performance or independent truth.

The product is always descriptive, non-authoritative, and not biological ground
truth. Composition does not run a detector, recalculate thresholds, or alter
provider/adjudication parents.

## Child and relationship semantics

Selected children retain their source identity, Event/State/Gap domain, label,
and canonical ISI/spike/time geometry exactly. The composer adds only typed
relationships, conflicts, Regimes, and membership records.

- Burst and Long Burst remain Events. Overlap with Tonic or Broad HFS creates a
  non-destructive `burst_embedded_in_carrier_state` relationship; it does not
  cut or extend either child.
- Broad HFS, HFT, HF-irregular, and Tonic remain States. HFT/HF-irregular is a
  subtype only when an exact-support selected Broad-HFS parent exists. A missing
  exact parent is retained and raised as a review conflict.
- Pause remains a Gap. A Pause strictly between separate Broad-HFS direct
  support segments creates `pause_interrupted_broad_hfs`. A Pause overlapping
  Broad-HFS direct support raises
  `canonical_pause_overlaps_broad_hfs_direct_support`; neither child is silently
  trimmed or deleted.

## Recurrent Regimes

Recurrence is evaluated only after provider selection or human adjudication.
The first release creates:

- `recurrent_bursting` after at least three selected Burst/Long-Burst Events;
- `recurrent_pausing` after at least five selected Pause Gaps.

The three-high-specificity-long-Pause route is disabled because provider bundle
v1 has no typed evidence field that can prove that route. It may be introduced
only under a separately versioned evidence contract.

The existing frozen R81 cadence/interruption budget is reused. Expansion is a
left-to-right, one-pass operation with a frozen initial anchor. Regime objects
never enter the input set, so a Regime cannot recursively create or enlarge
another Regime. All child Events/Gaps remain present and auditable.

## Paired legacy/new comparison

| Property | Legacy native R81 | Provider composer R86 |
|---|---|---|
| Source | already-composed native FINAL | one explicit normalized provider run or its Phase C descendant |
| Detector/composer invocation | native path invokes R81 once | no detector invocation and no native recomposition |
| Children | canonical native FINAL children | exact selected provider/adjudicated children |
| Recurrence budget | frozen R81 one-pass cadence budget | the identical frozen R81 budget helper |
| Recurrent Burst | three canonical Burst Events | three selected Burst/Long-Burst Events |
| Recurrent Pause | typed 3-long or 5-ordinary route | 5 selected Pause Gaps; 3-long disabled without typed evidence |
| State/Event overlap | native product semantics | explicit non-destructive relationship |
| Pause/HFS contradiction | native upstream policy | explicit review conflict; no silent geometry edit |
| Authority | descriptive Event-Regime product | descriptive provider composition; never truth |

R81 is not changed or called by R86. The paired regression asserts that R86
shares only the frozen window policy, preserves source geometry exactly, does
not mutate either parent, and records `detection_recomposition_performed=FALSE`
and `automatic_provider_fusion=FALSE`.

## Product and validation

The product contains `metadata`, `decision`, `children`, `relationships`,
`regimes`, `memberships`, `conflicts`, `invariants`, and `manifest` tables. IDs,
table hashes, the product hash, exact parent hashes, closure across child and
Regime references, provider-run isolation, authority, and deterministic full
rematerialization are validated fail closed.

This is a headless Phase D boundary. UI selection, export integration, and
normalized scoring belong to Phase E.
