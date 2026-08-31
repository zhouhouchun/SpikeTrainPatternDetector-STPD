# STPD provider workbench and scoring v1 contract

Status: Phase E implementation and Shiny integration.

## Purpose and scope

Phase E exposes the already frozen provider, adjudication, and scientific
composer products as one auditable workbench. It does not add a detector, tune
thresholds, or fuse algorithms. The workbench retains every imported provider
run in its catalog while selecting exactly one run and exactly one source mode
(`auto` or `adjudicated`) for a view.

The Shiny page can import strictly validated STPD RDS products, select a single
run, append Phase C accept/reject/boundary-adjustment/compensation decisions,
materialize the Phase D composition, inspect AUTO-versus-adjudicated changes,
score one explicit reference, and export a sealed archive. Imported RDS files
must be trusted STPD products; every object is nevertheless revalidated against
its exact schema, parent hashes, semantics, and deterministic replay contract.

## Review view

`stpd_provider_review_view()` produces a non-authoritative view with:

- one-row metadata including provider run, source mode, authority, information
  access, performance use, exact parent hashes, counts, and estimand notice;
- the complete provider catalog, with exactly one selected run;
- provider records and selected intervals only from the selected run;
- an explicit status for every positive AUTO record, including rejection,
  accepted-as-is, boundary adjustment, retained unreviewed AUTO, or absence
  without acceptance;
- provider-independent Event/State/Gap relationships, descriptive Regimes,
  memberships, and conflicts from the Phase D composer; and
- per-table and whole-product SHA-256 manifests.

Equal geometry from different runs is never deduplicated. Candidate-support
output cannot enter AUTO composition; it enters only through an explicit Phase
C acceptance or adjustment. Building or validating a view must not mutate its
provider, adjudication, or composition parents.

## Reference product and ontology

`stpd_provider_reference_bundle()` seals a separate reference product. Its
ordered typed interval table uses canonical ISI coordinates and supports:

- Event: `burst`, `long_burst`;
- State: `broad_hfs`, `hft`, `hf_irregular`, `tonic`;
- Gap: `pause`.

Intervals cannot overlap within one train/track/label. The reference declares
its dataset snapshot, authority, access to predictions, provider blinding,
annotation provenance, and RFC3339 UTC audit time. These declarations are
validated semantically as well as cryptographically; the reference is never
read from a provider-output table.

## Normalized scoring and estimand gates

`stpd_score_provider_view()` consumes only the normalized selected-interval
view and the sealed reference. It has no provider-specific scoring switch.
Matching is performed independently for each provider run, semantic track, and
label by ordered dynamic programming that maximizes match count and then total
IoU. The product reports:

- interval/event precision, recall, and F1 by track and label;
- ISI-support coverage by track and label;
- matched boundaries and IoU;
- overlap count, best IoU, fragmentation, and false-split status for each
  reference interval.

`provider_pooling` and `label_pooling` are always false. Broad HFS and its HFT
or HF-irregular subtypes are evaluated separately when corresponding reference
labels exist.

The two estimands fail closed:

1. `detector_performance` requires label-blind AUTO with
   `automatic_prediction_record` authority plus an independent reference that
   was blinded to predictions and provider output.
2. `adjudicated_agreement` requires an adjudicated source whose declared use is
   `adjudicated_agreement_only`.

Both construction and later validation recheck these gates. Recomputing table
or product hashes cannot convert manual-aware output into detector performance.

## Shiny adjudication boundary

The UI appends Phase C decisions and never edits provider AUTO bytes. Accept,
reject, and compensation use only the immutable provider record and exact
adjudication parent. Boundary adjustment additionally requires the exact source
dataset to be loaded as the current dataset; Phase C rechecks snapshot, train,
QC, and canonical-coordinate boundaries before accepting it. Any successful
decision invalidates the current composition, view, and score until the user
explicitly rematerializes them.

## Transactional export

`stpd_write_provider_workbench()` validates every parent and optional score
before creating a staging directory. It writes the provider bundle,
adjudication when applicable, composition, workbench view, reference and score
when applicable, plus normalized CSV tables. A completion manifest containing
the finalized payload hashes is written only after all other files exist and
are hashed. The staging directory is then renamed atomically to a new output
directory. Existing destinations fail closed, and any pre-finalization failure
removes the staging directory rather than leaving a seemingly complete export.

The Shiny download wraps that finalized directory in a ZIP without changing
the inner completion contract.
