# Threshold-first Gate 1B complete candidate-universe release closure

- Date: 2026-08-30
- Decision: GO for the bounded internal direct-observation release
- Root ID: `complete_candidate_universe_release`
- Scientific detector behavior: unchanged
- Canonical public lineage adapter: not materialized
- Publication authority: `FALSE`
- Independent recheck remediation: complete

## Closed scope

The final Gate 1B observer releases one train-local internal observation
receipt only when all 77 preceding frozen hooks exist exactly once and in the
actual `hf_protected` call order, and when 13 root-level receipts independently
close their applicable scan/replay paths. Three final hooks bind that root set
to the exact returned train product, bind every active candidate-cap scope, and
issue a hash-linked receipt. The complete registry therefore contains 80 hooks.

This release does not assemble the canonical public candidate-lineage bundle.
A fully observed train ends as `direct_complete`, but final live attachment
remains `diagnostic_unavailable` with reason `adapter_mapping_unavailable`.
Observation completeness is not scientific validation and is not publication
authority.

## Frozen release semantics

The manifest records the exact preceding hook order, record count, hook-list
hash, index hash, root-audit hash and explicit cap/early-stop accounting. Any
missing, duplicated or reordered hook fails before release. Stage-4 early stop,
observer-side capture truncation, a non-terminal replay, or an unobserved
Nested-HFS cap fails closed. Structure-first and union caps are allowed only
when their full pre-cap universes were already captured and hash-bound.

The product binding records counts and hashes for the final scientific audit,
`pattern_auto`, `auto_score`, the complete multitrack shadow, compatibility
shadow, event-grammar parameters, nested-HFS Review candidates, Tonic Review
candidates, the frozen Broad-HFS parent signature, and the complete data frame
with all attributes that is returned to the caller. It only observes products
already produced by the detector.

The resource receipt freezes the 80-hook ceiling and five active resource
scopes: dispatch, structure-first, Stage 4, union ranking, and nested-HFS
Review. Cap values must equal their effective detector settings. The release
itself invokes the detector zero times and emits zero scientific candidates. It
explicitly records
`publication_authority = FALSE` and `scientific_result_influence =
none_observer_release_only`.

The three release hooks are one transaction. An error after hook 1, 2 or 3
removes the hash-replayed cache and restores the preceding observation index,
payload list and `direct_partial` status byte-for-byte. The same shard can then
retry safely; `direct_complete` is assigned only after all 80 hooks and storage
validation succeed.

Runs using a deliberately partial pattern or alternate-pipeline scope remain
`direct_partial`; they neither fail nor claim complete coverage. Direct calls
to the release with missing roots fail closed.

## Review checks

| Required check | Outcome |
|---|---|
| All preceding family roots present once | PASS |
| Actual frozen call order equals registry order | PASS |
| Thirteen root receipts close applicable paths | PASS |
| Stage-4 early stop/capture truncation fails closed | PASS |
| Final train products hash-bound | PASS |
| Full returned product and all attributes hash-bound | PASS |
| Five active resource cap scopes frozen | PASS |
| Release invokes no detector | PASS |
| Release emits no scientific candidate | PASS |
| Missing-root release fails before mutation | PASS |
| False `direct_complete` rejected | PASS |
| Publication-authority tamper rejected | PASS |
| Product hash and resource-audit hash tamper rejected | PASS |
| Fault after release hook 1, 2 or 3 rolls back | PASS |
| Same-shard retry reaches 80/80 | PASS |
| Partial/alternate pipelines remain partial | PASS |
| Collector-off scientific output byte-identical | PASS |
| Canonical adapter remains unavailable | PASS |

## Verification

| Check | Outcome |
|---|---|
| Active-entry and complete-release suite | PASS |
| Burst Stage 1/2 full-chain assertions | PASS; alternate-pipeline contract retested separately |
| Tonic-State focused suite | PASS |
| Broad-HFS focused suite | PASS |
| Nested-HFS Review focused suite | PASS |
| Collector contract suite | PASS |
| Live-guard suite | PASS |
| Final-Gap focused suite | PASS |
| Publication-readiness governance suite | PASS |
| Clean source build | PASS |
| `R CMD check --no-tests --no-manual --no-build-vignettes` | `Status: OK` |
| R parse, DESCRIPTION/Collate and `git diff --check` | PASS |

The broad plumbing and contextual-Pause batches were stopped after their
already completed sub-suites passed because repeated full detector replay made
the remaining non-focused cases disproportionately slow. Their directly
affected focused suites and the complete production entry path passed.

## Frozen implementation hashes

- `R/91m_candidate_lineage_complete_universe_release.R`:
  `32aee45f7dc4f6e034dd3bae7bb0682155c9c369c275713e39f03a950878fea1`
- `R/91_candidate_lineage_collector.R`:
  `dafb4aa8a7feb43f2a174b214dd0009b5f04936f3860f6f30640b43446d7d764`
- `R/42_hfspiking_protection_and_manual_coexistence.R`:
  `c0ab0617e6dba7fb3c87a4fab4322cd56df6befebd0895e8ab8c6218fd483ba7`
- active-entry focused test:
  `1775cee60c3579f6e0d5db83b2fcfc63945fab5aea6c0ffea38cfb58bc9b7e17`
- root/resource audit focused test:
  `e6aaabd00f372bd577ba2bcafbd40df09c4a89540e1146ebe5cfa9717ec659ae`

## Gate boundary

All six planned Gate 1B roots now have bounded implementation and closure
records. This does not complete the 12 publication-readiness scientific
stages. The next work item is PR-01 evidence-freeze execution: assemble one
immutable evidence manifest from the closed detector, simulation benchmark and
eligible real-data reference set, then review it before PR-02 begins.
