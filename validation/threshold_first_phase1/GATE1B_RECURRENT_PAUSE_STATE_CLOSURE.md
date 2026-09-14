# Threshold-first Gate 1B Recurrent Pause State closure

- Date: 2026-08-30
- Decision: GO for the bounded recurrent parent-State layer
- Parent State: `recurrent_pause_state`
- Canonical child Pause authority: unchanged
- Publication authority: `FALSE`

## Closed scope

This stage materializes a descriptive parent State only after canonical FINAL
Pause detection has completed. It never discovers a Pause, changes a Pause
decision, or replaces a child Pause. Every parent membership binds an already
accepted canonical FINAL Gap by its identifier, train and exact ISI support.

For compatibility, the public result key remains `event_regimes` and the
legacy grouping label remains `recurrent_pausing`. The normative ontology is
now explicit in every parent row:

- `semantic_domain = state`;
- `parent_state_class = recurrent_pause_state`;
- `candidate_layer = post_final_recurrent_parent_state`; and
- `candidate_source = canonical_final_child_entities`.

## Frozen trigger semantics

A Recurrent Pause State candidate is triggered by either:

1. at least three upstream high-specificity long Pause children; or
2. at least five upstream ordinary accepted Pause children.

These routes are a logical OR and are budgeted independently. A mixed count
such as two long plus three ordinary Pauses is not a trigger. The count rule is
applied only to already detected Pause entities; it is not an online Pause
detection rule and does not use an absolute ISI threshold.

The minimum trigger anchor may include additional adjacent accepted Pause
children only under its frozen local cadence and finite interruption budget.
Expansion is one-pass, left-to-right and nonrecursive: accepted parent States
are never used as children for another parent, and a canonical child cannot
feed more than one parent of the same class.

## Event/State and coexistence semantics

Canonical Pause Gaps remain the direct-support child entities. The new State
is an outer recurrence envelope with explicit memberships; interruption ISIs
inside the envelope are not relabelled as Pause. Tonic and Broad HFS remain
independent carrier States and can overlap the recurrence envelope without
being deleted, retyped or used to create Pause support.

Only typed QC, acquisition or artifact hard boundaries split recurrence.
Pause itself is not a parent-recurrence boundary. Every source child and the
canonical FINAL parent product are hash-bound, and validation includes
deterministic rematerialization.

## Authority boundary

The product is a descriptive, unvalidated parent-State candidate layer. It is
not biological ground truth, does not enter canonical direct-support State
labels, and cannot support publication performance claims without independent
State-level reference annotation. Individual Pause performance continues to
be evaluated from canonical FINAL Gap support.

## Review checks

| Required check | Outcome |
|---|---|
| Runs only after FINAL Pause detection | PASS |
| Three-long trigger | PASS |
| Five-ordinary trigger | PASS |
| Mixed 2-long + 3-ordinary rejected | PASS |
| Child Pause decisions/support preserved | PASS |
| Explicit State-domain ontology | PASS |
| Finite interruption budget | PASS |
| One-pass nonrecursive expansion | PASS |
| Multiplicative time-scale invariance | PASS |
| Parent hash binding and rematerialization | PASS |
| Carrier Tonic/Broad-HFS non-destructive | PASS |
| Absolute ISI threshold absent | PASS |

## Verification

| Check | Outcome |
|---|---|
| Recurrent parent-State focused suite | PASS (68 expectations) |
| Provider scientific composer compatibility suite | PASS (57 expectations) |
| State/Event boundary suite | PASS (69 expectations) |
| Publication HFS semantics suite | PASS (141 expectations) |
| FINAL product suite | PASS |
| Pre/post canonical per-ISI scientific projection hash | IDENTICAL (`2c954eda...36ce`) |
| Clean source build | PASS |
| `R CMD check --no-tests --no-manual --no-build-vignettes` | `Status: OK` |
| R parse, documentation schema and `git diff --check` | PASS |

The source-only runner emitted its expected local DLL warning under
`pkgload::load_all(..., compile = FALSE)`. The clean source package compiled,
loaded and unloaded successfully during `R CMD check`.

## Frozen implementation hashes

- `R/81_event_regime_product.R`:
  `70aa5555b07be1b3ef839bc677c605c1e53c41d5c111a896ec2990bac7b8ee33`
- `man/stpd_event_regimes.Rd`:
  `191d30b13e34447d2505db1807d68af8ae0cb3ccae3a91272603ddd535315dcb`
- focused test:
  `ceca65047fde5fff061c437e505bcd783daa29b5b7ab2f186dd7ff053962308b`

## Next bounded stage

Review and close the Broad HFS State root. The next stage must distinguish
direct HFS support from its episode envelope, keep HFT and HF-irregular as
descriptive subtypes on one Broad-HFS parent support, permit independent Burst
Events to coexist, and prevent canonical Pause support from being swallowed.
