# Threshold-first Gate 1B Tonic State closure

- Date: 2026-08-30
- Decision: GO for the bounded Tonic-State observer root
- Root ID: `tonic_state_root`
- Scientific detector behavior: unchanged
- Public-product authority: unchanged and non-authoritative
- Publication authority: `FALSE`

## Closed scope

This stage observes the active canonical Tonic and legacy HFT State calls in
the `hf_protected` pipeline. It locks the train data, effective parameters,
resolved train parameters, requested patterns and accepted canonical Pause
boundaries before the scientific calls. The observer closes only after the
Broad-HFS observer, preserving the serial Gate 1B dependency.

Five typed, hash-linked payloads are recorded:

1. Tonic/HFT generator entry and minimum evidence requirements;
2. emitted State candidates and overlapping canonical Burst Events;
3. separate frequency and CV/LV/MM evidence axes;
4. complete canonical-generator redetection of every Pause-created child; and
5. a fail-closed root receipt.

The patch is observer-only. It does not add, delete, extend, split, rank or
relabel any Tonic, HFT, HFS, Burst or Pause candidate. Thresholds, AUTO/FINAL
labels, multitrack products and exports are unchanged.

## Frozen scientific semantics

Tonic is a State, not an Event. HFT remains a high-frequency regular State
description under the broader HFS family. Frequency evidence is recorded from
the candidate-local ISI distribution; regularity evidence is recorded
separately as CV, LV and MM. No universal absolute ISI value is introduced by
this stage.

Canonical Burst is an orthogonal Event overlay. For every already emitted
Tonic/HFT State candidate, Burst overlap is recorded without applying a State
veto or changing State geometry. This closes the downstream Event/State
compatibility rule. It does not claim that all biologically valid Tonic
envelopes are already generated; candidate-entry recall remains a later
performance-validation question.

When the whole train has fewer valid spikes than the requested State's frozen
minimum, emission must abstain. The receipt distinguishes this explicit sparse
case from ordinary non-detection caused by frequency or regularity evidence.

Canonical Pause is a genuine direct-support boundary. Every child produced by
subtracting a canonical Pause is sent through the complete corresponding
generator (`stpd_event_core_detect_tonic` or
`stpd_event_core_detect_hf_tonic`) on a boundary-isolated slice. Acceptance is
therefore independently regenerated; it is not inferred from the parent
candidate and not reduced to a feature-only provisional check. The slice plus
preceding timestamp preserves duration calculations while avoiding repeated
whole-train scans.

## Authority boundary

These tables are immutable Gate 1B observations, not a public v3 prediction
product. They do not establish biological truth, Tonic/HFT sensitivity,
external validity, subtype accuracy or release authority. The older
compatibility shadow may still display inherited provisional parent evidence;
the new observer records the stricter independent-redetection audit without
silently changing that legacy product.

## Review checks

| Required check | Outcome |
|---|---|
| Frequency and regularity stored on separate axes | PASS |
| CV, LV and MM recomputed from candidate-local evidence | PASS |
| Burst overlap applies no downstream State veto | PASS |
| Sparse evidence produces explicit abstention | PASS |
| Canonical Pause children use complete generator redetection | PASS |
| Redetection geometry and decisions are hash-linked | PASS |
| Collector-on/off scientific output identical | PASS |
| All five payloads mutation-rejected | PASS |
| Publication authority remains false | PASS |

## Verification

| Check | Outcome |
|---|---|
| Tonic-State focused suite | PASS (36 expectations) |
| Active-entry ordering suite | PASS (33 expectations) |
| Broad-HFS observer suite | PASS (31 expectations) |
| Burst-stage lineage suite | PASS (38 expectations) |
| State/Event boundary suite | PASS (69 expectations) |
| Tonic frozen-band, Review and Burst-overlay suites | PASS (80 + 39 + 21 expectations) |
| Multitrack AUTO product suite | PASS (184 expectations) |
| HFS/Pause coexistence suite | PASS (35 expectations) |
| Clean source build | PASS |
| `R CMD check --no-tests --no-manual --no-build-vignettes` | `Status: OK` |
| R parse, DESCRIPTION/Collate and `git diff --check` | PASS |

The first build attempt correctly rejected unrelated untracked auxiliary R
files and a duplicate non-portable test filename present in the user's working
tree. No user file was changed or removed. A clean temporary source copy that
excluded only those unrelated untracked files compiled, loaded, unloaded and
checked with `Status: OK`.

## Frozen implementation hashes

- `R/91k_candidate_lineage_tonic_state.R`:
  `a5e4c22fdff105cd809d9c9a4d957290084c5c2b1818bd8fe21a9db9a7171238`
- `R/91_candidate_lineage_collector.R`:
  `778cc2a05ab3af2270291b0e2d60ed9472d06b20776965c98ce880462183f45c`
- `R/42_hfspiking_protection_and_manual_coexistence.R`:
  `42e9a0ab95487a119b5958d3ecf90b98e90c6eacbea9104ca6969a4bf090f12f`
- focused test:
  `bd4fa62f8deeae702a8afc3d43c6709c712c74bb4abae8e0122a8ea4d795fc75`

## Next bounded stage

Close the `nested_hfs_review` root. It must remain Review rather than Event,
use contrast against the local HFS background, never veto or reshape the
Broad-HFS parent, and require a separate explicit authority before any future
promotion to a canonical Burst Event.
