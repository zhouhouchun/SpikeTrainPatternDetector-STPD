# Threshold-first Gate 1B final Gap arbitration/materialization closure

- Date: 2026-08-30
- Decision: GO for the bounded final-Gap observer root
- Root ID: `final_gap_arbitration_materialization_root`
- Scientific detector behavior: unchanged
- Recurrent Pause State: not evaluated
- Publication authority: `FALSE`

## Closed scope

This stage closes direct observation of the existing final Gap path in the
primary `hf_protected` detector. It binds every contextual Pause proposal to
the exact protected candidate ledger entering the legacy weighted-interval
grammar, independently replays that grammar, and records the selected Gap
support after the existing `pattern_auto` projection and post-validation.

The patch is observer-only. It adds no ISI threshold, proposal, veto,
deduplication rule, recurrence rule, Event, State, or public product. It does
not change Burst ownership, HFS protection, candidate values, interval
selection, label projection, or post-validation.

## Five typed hooks

1. final-Gap entry and upstream contextual receipt binding;
2. exact protected candidate input, including arbitration value and the
   contextual proposal ordinal;
3. independent weighted-interval arbitration replay;
4. per-ISI materialized selected-Gap output; and
5. a hash-linked closure receipt.

The receipt reports candidate and selected counts, exact input binding,
arbitration equality, scientific audit identity, final materialized Pause
support, post-validation removals, collector-off status, and fixed authority
limits.

## Scientific semantics and replay

The final Gap remains an orthogonal Gap proposal before the legacy
single-label projection. A proposal rejected after frozen Burst ownership is
not selectable. All other candidate families remain in the same global
weighted-interval arbitration; therefore conflicts and priorities are replayed
over the complete protected ledger, not over Pause rows in isolation.

Independent replay reproduces the existing allowed-label filter,
reject/abstain/blocked filter, interval validity checks, deterministic
end/start ordering, predecessor relation, dynamic-programming recurrence,
tie behavior, backtracking, and selection statuses. The replayed full audit is
required to be byte-identical to the scientific audit.

Selected Gap candidates are then projected independently to their expected
pre-validation ISI support. Every selected Gap ISI is bound to its final
scientific `pattern_auto` value and classified as retained Pause support or as
removed/reassigned by the existing post-validation contract. No removed Gap is
silently counted as materialized support.

## Authority boundary

This closure does not construct `Recurrent Pause State`; it only makes the
validated child Gap Events available for that later post-detection stage. It
does not materialize Broad-HFS direct support/envelopes, Tonic State, nested-HFS
Review, a complete candidate universe, performance estimates, manual-review
authority, or publication authority.

The fixed authority fields are:

- scientific influence: `none_observer_only`;
- recurrent Pause State: `not_evaluated_post_detection_only`;
- collector-off path: observer branch not entered;
- publication authority: `FALSE`.

## Review checks

| Required check | Outcome |
|---|---|
| Exact candidate input binding | PASS |
| Independent arbitration replay | PASS |
| Full scientific audit identity | PASS |
| Materialized Gap identity | PASS |
| Frozen Burst/rejected-Gap conflict priority | PASS |
| Collector-off byte equivalence | PASS |
| Payload mutation rejection | PASS for all five hooks |

The active deterministic fixture produced one contextual Gap proposal, one
selected Gap candidate, one expected Gap-support ISI and one retained final
Pause-support ISI, with zero post-validation removals.

## Verification

| Check | Outcome |
|---|---|
| Final-Gap focused suite | PASS (24 expectations) |
| Contextual Pause lineage suite | PASS |
| Candidate-lineage active-entry suite | PASS |
| Burst-stage lineage suite | PASS |
| Adaptive inter-burst Pause suite | PASS |
| HFS--Pause coexistence suite | PASS |
| Product-hardening suite | PASS |
| Installed-package final-Gap smoke | `INSTALLED_GAP_FINAL_OK` |
| Clean source build | PASS |
| `R CMD check --no-tests --no-manual --no-build-vignettes` | `Status: OK` |
| R parse, DESCRIPTION/Collate, `git diff --check` | PASS |

The source-only test runner used `pkgload::load_all(..., compile = FALSE)` and
therefore emitted its expected local DLL warning. The clean installed-package
check compiled and loaded the native library successfully; this warning is not
a package or detector failure.

## Frozen implementation hashes

- `R/91i_candidate_lineage_gap_final.R`:
  `fceb257945485ee719929f1969796d27164b07d54dc30f9657332114be7e9597`
- `R/91_candidate_lineage_collector.R`:
  `043dfbf10e7920435aa0976fe4ebe3b97b0e78ecc13fd65cc60334b452727f3b`
- `R/42_hfspiking_protection_and_manual_coexistence.R`:
  `07633b0ae4729ec59e2f7a1019d40cd520168971b9fa119b7abef0f096c7b97f`
- focused test:
  `cfe0d9e615bf8463984d379a39a8f505e00f1b19495fa6d883e8303035aacc38`

## Next bounded stage

Implement `Recurrent Pause State` strictly after final child Gap detection.
Preserve every child Pause Event, apply the pre-agreed `three long or five
ordinary` trigger only post detection, bind recurrence windows and interruption
budgets, and prohibit recursive parent-State growth.
