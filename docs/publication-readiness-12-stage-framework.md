# STPD 12-stage publication-readiness framework

## Scope

This is a sequencing and review framework, not a detector result, scientific
validation result, Gate B approval, Gate C approval, or publication claim. It
keeps the twelve agreed delivery stages finite and prevents a later stage from
starting before the preceding framework review passes.

The executable authority is
`inst/config/publication_readiness_framework_v1.json`, validated by
`R/93_publication_readiness_framework.R`. Every stage initially remains
non-authoritative. A stage framework may be marked reviewed only after its
declared outputs and stop checks exist and the focused tests, parse checks,
regression boundary and overclaim review pass.

Version 1.1 additionally requires
`inst/config/publication_readiness_review_receipts_v1.json`. A completed
framework is accepted only when the framework and packet file hashes match the
receipt, every stage output/check/deferred ID matches exactly, all twelve
review-record hashes are present, and every authority flag remains false.

## Frozen stage order

1. Gate 1B remaining roots.
2. Gate B v3 contract freeze and approval.
3. State direct-support, connector and hard-boundary ledger.
4. AUTO v3 multi-track product.
5. FINAL v3 and review transitions.
6. Evidence, export, UI, migration and signing.
7. Final synthetic benchmark freeze.
8. Synthetic validation pipeline.
9. Real-reference freeze and eligibility.
10. Real-data validation and comparators.
11. Reports, performance and software release.
12. Manuscript, reviewer response and reproducibility archive.

Dependencies are serial because the user requested review after every stage.
Implementation work may be prepared separately, but it cannot be marked active
or accepted in this framework until the previous stage review passes.

## Stage 1 boundary

Stage 1 registers six remaining Gate 1B roots in order:

1. final Gap arbitration and materialization;
2. recurrent Pause State;
3. Broad-HFS State;
4. Tonic State;
5. nested-HFS Review; and
6. complete candidate-universe/release closure.

Registration does not close any root. Each remains
`registered_pending_implementation`, has scientific closure `pending`, and has
publication authority `FALSE`. The already closed contextual/inter-burst Pause
proposal receipt is only the dependency of the first remaining root.

## Review rule

For each stage:

1. implement only its declared framework outputs;
2. run focused contract tests and relevant regressions;
3. verify parse, package load and diff cleanliness;
4. audit scientific and publication authority wording;
5. record unresolved non-blocking debt; and
6. only then advance `current_stage_id`.

Scientific implementation details, detector tuning, final performance claims
and release promotion remain separate work and cannot be inferred from a
reviewed framework skeleton.

## Current framework status

- Stage 1 framework review: passed on 2026-08-30.
- Stage 1 scientific completion: still partial; the six registered roots remain
  pending implementation and retain no publication authority.
- Stage 2 framework review: passed on 2026-08-30. The existing 59-table,
  27-contract Gate B v3 candidate bundle is bound to its frozen schemas,
  fixtures and external-approval boundary; it remains non-authoritative.
- Stage 3 framework review: passed on 2026-08-30. Direct support, tolerated
  connectors, hard boundaries and split/redetection lineage remain separate,
  typed and non-authoritative.
- Stage 4 framework review: passed on 2026-08-30. AUTO v3 keeps Event,
  State and Gap tracks distinct, provides deterministic per-ISI projection and
  binds every product to its parent/hash DAG.
- Stage 5 framework review: passed on 2026-08-30. FINAL v3 is defined as a
  deterministic replay of append-only, parent-bound review transitions while
  AUTO evidence remains immutable.
- Stage 6 framework review: passed on 2026-08-30. Evidence coverage gates an
  atomic generation export; candidate/official consumers are version-separated
  and release signing remains external.
- Stage 7 framework review: passed on 2026-08-30. The final synthetic freeze
  must bind input bytes, detector-blind phenotype eligibility, paired template
  identities and immutable grouped splits.
- Stage 8 framework review: passed on 2026-08-30. Synthetic validation is
  grouped, label-blind and reports support, event, boundary and split/merge
  estimands with template-cluster uncertainty.
- Stage 9 framework review: passed on 2026-08-30. Real-reference freezing is
  byte-addressed and track-specific, with annotation coverage and limitations
  carried into every estimand.
- Stage 10 framework review: passed on 2026-08-30. Real validation uses
  recording-group LOGO, track-specific eligibility, fair comparator calibration
  and separates agreement from generalization claims.
- Stage 11 framework review: passed on 2026-08-30. Reports must resolve every
  number to frozen result bytes, while runtime/resource and clean-package gates
  bind the exact detector hash evaluated.
- Stage 12 framework review: passed on 2026-08-30. Manuscript claims, reviewer
  responses and archive entries must resolve to frozen evidence and explicit
  limitations.
- Framework status: all 12 minimum frameworks reviewed. This does **not** mean
  the twelve scientific/release stages are implemented or publication-ready;
  the deferred work listed in each packet remains mandatory.
- Independent adversarial recheck: the initial permissive packet validation,
  missing receipt closure and missing package documentation were identified and
  remediated in framework version 1.1. Full scientific/release tests remain
  separate gates.
