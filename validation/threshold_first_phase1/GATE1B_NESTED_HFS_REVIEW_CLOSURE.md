# Threshold-first Gate 1B nested-HFS Review closure

- Date: 2026-08-30
- Decision: GO for the bounded nested-HFS Review observer root
- Root ID: `nested_hfs_review_root`
- Scientific detector behavior: unchanged
- Public-product authority: unchanged and non-authoritative
- Publication authority: `FALSE`

## Closed scope

This observer binds directly to the existing local-rate proposal generator
inside already selected Broad-HFS parents. It freezes the input train, selected
parent payload, dimensionless detector settings and hard boundaries before the
scientific call, then closes after Tonic State so the Gate 1B dependency order
is explicit.

Five immutable payloads record entry, proposal identity, local HFS contrast,
parent-State invariance and a hash-linked receipt. No candidate, threshold,
selection, AUTO/FINAL label or exported product is changed.

## Frozen scientific semantics

An HFS-local acceleration is `possible_burst` Review evidence, not a canonical
Burst Event. Every proposal is `review_only`, `canonical_eligible = FALSE`, and
`action = demote_to_possible`. The multitrack projection must place it on the
Review track and require a separate Review-to-Event transition.

The evidence is relative to the frozen parent HFS support: local pair rank,
left/right HFS background ratios, geometric contrast and final-flank ratios.
No absolute pattern threshold is used. Homogeneous HFS emits no proposal merely
because a mathematical minimum exists.

The selected Broad-HFS parent signature must be identical before and after the
Review proposal enters the shadow pool. Review cannot create, veto, split,
extend or otherwise reshape the State. Future Event promotion requires an
independent, auditable authority; the proposal cannot self-promote.

## Review checks

| Required check | Outcome |
|---|---|
| Review identity, not Event | PASS |
| Local frozen-HFS background contrast | PASS |
| No absolute Burst threshold | PASS |
| Homogeneous HFS abstains | PASS |
| Parent HFS signature unchanged | PASS |
| Review cannot veto/reshape/create State | PASS |
| Separate promotion authority required | PASS |
| All five payloads mutation-rejected | PASS |
| Collector-off scientific output identical | PASS |
| Publication authority remains false | PASS |

## Verification

| Check | Outcome |
|---|---|
| Nested-HFS observer focused suite | PASS (33 expectations) |
| Existing nested local-rate suite | PASS (62 expectations) |
| Review-to-Event transition suite | PASS (209 expectations) |
| Legacy promotion guard suite | PASS (38 expectations) |
| Active-entry ordering suite | PASS (33 expectations) |
| Clean source build | PASS |
| `R CMD check --no-tests --no-manual --no-build-vignettes` | `Status: OK` |
| R parse, DESCRIPTION/Collate and `git diff --check` | PASS |

Unrelated untracked auxiliary files were excluded only from the temporary
clean-source check. No user file was modified or removed.

## Frozen implementation hashes

- `R/91l_candidate_lineage_nested_hfs_review.R`:
  `72db917b8c7a367bcc58dba99877fdfc1b3af7ac15223ac0f88aaa261293cbfe`
- `R/91_candidate_lineage_collector.R`:
  `0aaf675b3795c3f98f8f589e7e84706ee473e408f7c349e2387b4f807f983043`
- `R/42_hfspiking_protection_and_manual_coexistence.R`:
  `c038a054b45bf329f9a3d35d82518e1fe5da55b80dce222ff268633fca0be12f`
- focused test:
  `0b717e8afcb9e10723ab1cfc6adbb62ecbdb902b048980fc632e40a44daf4819`

## Next bounded stage

Close `complete_candidate_universe_release`, the final Gate 1B root. It must
prove complete-universe coverage across all earlier roots, deterministic
lineage/decision identity, all-or-nothing publication, and explicit separation
of observation completeness from scientific or publication authority.
