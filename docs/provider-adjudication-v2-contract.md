# STPD provider adjudication v2 contract

Status: Phase C headless contract and implementation. This product is a
manual-aware descendant of, and never a mutation of, provider bundle v1.

## Identity and authority

Each review target is the exact pair `provider_run_id + candidate_id`. The
candidate ID is used as `source_record_id`; its immutable candidate hash,
source-record key, train timestamp hash, and provider dataset snapshot remain in
the lineage. Provider `raw_output_sha256`, `normalized_output_sha256`, all AUTO
tables, and the provider-bundle contract hash are never rewritten.

The adjudication product is not an independent biological reference and is not
eligible for label-blind detector-performance claims. Comparisons involving it
must be named algorithm-assisted or adjudicated agreement.
The product therefore freezes `information_access=manual_aware`,
`authority_scope=adjudicated_prediction_record`, and
`performance_use=adjudicated_agreement_only`; legacy Phase-2B compatibility is
explicitly `phase2b_v1_read_only`.

## Supported actions

- `accept_as_is` retains a positive source interval without changing geometry;
- `reject` records rejection and materializes no interval;
- `adjust_bounds` creates a separately identified derived interval while
  inheriting train, semantic track, label, and source lineage;
- `void_prior_decision` is append-only compensation and removes the current
  projection without deleting its history row.

A second substantive decision on the same source is forbidden until the current
decision is voided. `split`, `merge`, `relabel`, `create`, and every unknown action
fail with `review_action_unsupported` until separately versioned.

`adjust_bounds` requires the exact source dataset. The implementation verifies
the selected train timestamp SHA-256, canonical train-row coordinate range, and
overlap with the immutable source interval. This B2 protocol has no segmented
QC/acquisition boundary table, so the verified timestamp train is the hard
acquisition segment. A future segmented-boundary context must be introduced as a
new contract before cross-segment or segment-aware expansion is allowed.

## Concurrency, replay, and tamper evidence

Every request contains the expected parent product SHA-256. A stale parent fails
with `review_precondition_mismatch`. `operation_id` is idempotent only for the
identical normalized request; reuse with different bytes fails closed.
`stpd_provider_review_request()` constructs the exact ordered request so callers
do not need to reproduce the wire schema manually.

History is append-only and contains a strict sequence, request hash, parent hash,
source hash, prior/compensated decision, reviewer pseudonym, reason, UTC audit
time, resolved canonical ISI/spike/time geometry, and transition hash. Derived
interval identity excludes reviewer, reason, and audit time; the full product
hash includes them. Deterministic replay
rebuilds current decisions, materialized intervals, lineage, and product hash.
Any mismatch fails closed. Review validation never reruns detection.

## Export and compatibility

Export writes an RDS product, four canonical CSV projections, and a JSON file
manifest into a new directory. Existing paths are never overwritten.

Legacy Phase-2B v1 products remain readable through their existing public
accessors and keep their historical hash contract. They are not silently mapped
into v2. Without an exact source/run/geometry equivalence proof they require
readjudication.
