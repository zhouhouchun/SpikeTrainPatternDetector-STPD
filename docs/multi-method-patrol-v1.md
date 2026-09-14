# Multi-method patrol v1

## Purpose

The patrol layer turns independent STPD, Mean-ISI, LogISI/newBD, Poisson
Surprise, and Robust Gaussian Surprise outputs into a deterministic review
queue. It is a quality-control and adjudication aid, not a new biological
classifier.

The layer never changes a detector result and never assigns a final label.
Every source interval remains independently identifiable. Manual acceptance,
rejection, or boundary correction must occur later through the versioned STPD
adjudication product.

## Evidence families

Method counts and evidence-family counts are both reported. Mean-ISI and
LogISI/newBD belong to the same `isi_threshold` family; Poisson Surprise and
RGS belong to the same `surprise` family; STPD is `native_grammar`. This avoids
treating correlated implementations as independent votes.

Agreement is descriptive. It is never majority voting and does not establish
ground truth.

## Candidate construction

Intervals are grouped only when train, semantic track, and canonical target
family match. By default, closed ISI intervals must overlap or touch; an
explicit non-negative `max_gap_isi` can broaden the review component. Original
boundaries are retained in the evidence and pairwise tables.

Native STPD ledgers already use the canonical train-row ISI coordinate. The
four support modules use one-based `diff(timestamp)` indices, which are mapped
exactly to train-row ISI coordinates by adding one. No nearest-time snapping is
used.

Each component is classified as one of:

- `cross_family_agreement`;
- `within_family_agreement`;
- `single_method_only`;
- `boundary_disagreement`;
- `semantic_conflict`.

The queue prioritizes semantic conflicts, boundary disagreement, and
auxiliary-only candidates. Transitive components with disproportionate span
are marked `chain_expansion_warning` rather than silently trusted.

## Event/state semantics

Burst evidence inside Broad HFS is recorded as `nested_hfs_context`; it is not
a conflict because Event and State may coexist. Burst evidence overlapping a
Pause gap is marked `semantic_conflict` and requires review. The context table
is never used to erase either source record.

## Public entry points

- `stpd_patrol_evidence_from_provider_bundle()`
- `stpd_patrol_evidence_from_native_events()` supports frozen legacy/native
  event ledgers without rewriting them as provider bundles.
- `stpd_patrol_evidence_from_support()`
- `stpd_patrol_bind_evidence()`
- `stpd_build_multi_method_patrol()`
- `stpd_run_multi_method_patrol()` runs the four existing auxiliary methods and
  constructs the same patrol product; an optional native provider bundle adds
  immutable STPD evidence. Frozen legacy STPD event ledgers can instead be
  supplied through `native_events`.

The result contains source evidence, union components, membership, pairwise
boundary/IoU diagnostics, a prioritized review queue, and a compact summary.
It deliberately contains no `final_label` field.

Each requested method also receives a `complete` or `rejected` status. By
default, one failed auxiliary method is excluded without discarding evidence
from successful methods; its error remains visible. `strict=TRUE` instead stops
the orchestration at the first rejected method.

## Interactive review

`launch_multi_method_patrol_reviewer()` displays the same raw timestamps on
parallel manual-reference, STPD, Mean-ISI, LogISI/newBD, Poisson Surprise, and
RGS tracks. The selected union component is shaded in both the raster and the
log-scale ISI profile. Filters expose semantic conflicts, boundary disagreement,
single-method candidates, and agreement cases; pan, zoom, a global range slider,
and per-method visibility are available.

The annotation panel can accept a candidate under a chosen label, reject it as
`other`, or retain it as `uncertain`. Closed ISI boundaries can be entered
directly or set by clicking points in the local ISI plot; the draft is drawn on
a separate track before saving. It also supports free intervals outside the
candidate set, even on trains with no candidate under the active filters. Other
operations include editing a saved interval, erasing it through a tombstone action,
splitting one interval, merging multiple same-label intervals, and undoing the
last transaction through compensating records. Saved manual intervals appear on
their own raster track and in a selectable per-train table. Split and merge are
written atomically as one transaction. Every operation appends hash-chained rows to
`review_annotations/patrol_annotation_history.csv`, while
`patrol_annotation_current.csv` contains the latest action per annotation entity.
Both tables can be downloaded from the page. These annotations are separate
manual-aware products: source outputs and the original manual-reference table
remain immutable, and the saved records must not be reported as label-blind
detector performance.

The main STPD Shiny application also exposes the same editor under
**多算法纠察与人工标注**. It can construct a reviewer bundle directly from the
current STPD dataset with `stpd_patrol_reviewer_bundle_from_dataset()`, or load a
previously frozen reviewer-bundle RDS. The current-dataset route recomputes the
declared auxiliary support methods and includes the current label-blind STPD
prediction; it never converts multi-method agreement into an automatic final
label. One app session is bound to one bundle and one annotation directory so
that audit chains from different datasets cannot be mixed.
