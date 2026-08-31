# Multi-track v2 scientific contract

## Status and scope

This document defines the normative scientific semantics for the next canonical
multi-track result produced by SpikeTrainPatternDetector (STPD). It is a product
contract, not a claim that the detector has already achieved biological validity.

The contract resolves one central ambiguity: a sustained firing regime and a
localized event are different biological dimensions. In particular, a canonical
Burst event may occur inside a high-frequency-spiking (HFS) state without either
annotation deleting the other.

The unpublished macOS implementation may be used as design evidence, but it is not
the authority for this contract. The rules below are derived independently from the
event/state distinction, explicit interval geometry, auditable review, and valid
performance estimation.

The key words **MUST**, **MUST NOT**, **SHOULD**, **SHOULD NOT**, and **MAY** are
normative.

## Scientific interpretation boundary

STPD labels are operational descriptions of extracellular spike-train patterns.
They MUST NOT be presented as direct measurements of membrane potential, ionic
mechanism, synaptic mechanism, or a latent cellular state that cannot be inferred
from spike timestamps alone.

A Burst is a localized event whose internal discharge structure is distinct from
its local context. HFS is a sustained high-frequency firing regime. Short ISIs alone
are insufficient to equate the two:

- homogeneous HFS can contain no Burst events;
- an HFS state can contain one or more locally distinct Burst events;
- a high-frequency Burst can occur outside an HFS state; and
- recurrent Burst packets do not establish HFS unless the HFS state criteria also
  pass independently.

## Canonical biological and epistemic tracks

The canonical v2 representation consists of four separate tracks. QC and diagnostic
evidence remain separate from all four.

| Track | Meaning | Canonical values |
|---|---|---|
| `state` | Sustained observed firing regime | `high_frequency_spiking`, `high_frequency_tonic`, `tonic` |
| `event` | Localized biological episode | `event_family = burst`, with modifiers defined below |
| `gap` | A physical interruption or boundary interval | `pause` |
| `review` | Epistemic hypothesis or unresolved decision | `possible_burst` and explicit review states |

Absence of an active interval in a track means that the track is unclassified at
that location. It MUST NOT be silently converted to `others` unless a separately
versioned policy explicitly requests such a display projection.

Within one train, active canonical intervals MUST NOT overlap within the same
`state`, `event`, or `gap` track. Cross-track overlap is permitted only by the
compatibility rules in this document. Multiple Review hypotheses MAY overlap, but
they remain hypotheses and MUST NOT be counted as accepted biological events.

Competing candidates, rejected candidates, profiles, feature rows, and uncertainty
evidence belong in diagnostic/audit tables. They are not additional biological
tracks.

## Canonical Burst identity and modifiers

`burst`, `long_burst`, and `high_frequency_burst` MUST NOT be stored as duplicate
peer events describing the same physical episode. One physical episode has one
stable `event_id` and one `event_family`:

```text
event_family = burst
```

Its scientifically relevant subclasses are modifiers of that event:

```text
extent_class    = classic | long | prolonged | unresolved
frequency_class = ordinary | high_frequency | unresolved
```

`extent_class` refers to the declared structural/spike-extent definition; it MUST
NOT be described as a duration class unless the active definition actually uses
duration. `frequency_class` is determined from the event's own internal frequency,
compactness, and event-boundary/context evidence.

The display labels `long_burst` and `high_frequency_burst` MAY be derived from these
fields. They MUST NOT create another `event_id` or increase Burst counts. `hf_burst`
MAY be accepted as a legacy input alias, but it MUST NOT be emitted as a second
canonical subtype.

"Burst in HFS" and `frequency_class = high_frequency` are different statements:

- Burst-in-HFS is a geometric relationship between an Event and a State.
- High-frequency Burst is an intrinsic Event modifier.

Neither statement implies the other.

## Interval coordinate and boundary convention

The canonical support unit is the ISI row. For spike timestamps `t`, ISI row `i`
represents:

```text
ISI i = (t[i - 1], t[i]]
```

An interval with inclusive indices `[start_isi, end_isi]` owns the union of those
ISI rows and therefore has:

```text
n_isi = end_isi - start_isi + 1
start_time_sec = t[start_isi - 1]
end_time_sec   = t[end_isi]
```

An interval has `n_isi + 1` boundary spikes. Adjacent intervals may use the same
spike as a boundary marker, but they MUST NOT share an ISI within one canonical
track. Consequently, `n_spikes` is not additive across adjacent intervals and MUST
NOT be summed to estimate total unique spikes without de-duplication.

Coverage, overlap, occupancy, and IoU MUST use ISI support sets or the corresponding
time-union geometry. Event-flank ISIs used to establish contrast do not belong to
the Burst interval unless they independently satisfy the event boundary rule.

## Cross-track compatibility matrix

| State or Gap | Event or Review | Rule | Required resolution |
|---|---|---|---|
| HFS | canonical Burst of any modifier | **Allowed** | Retain both complete intervals and create an Event-State relationship. The Burst MUST NOT split or deselect HFS. |
| HFS | `possible_burst` | **Allowed as Review only** | It cannot enter Event counts, split State, or change HFS selection before confirmation. |
| HFS | Pause | **Forbidden on the same ISI** | Pause owns the Gap support, splits HFS, and each residual HFS child is re-gated. |
| HFS | HFT or tonic | **Forbidden within State** | Select at most one canonical State; preserve losing candidates as diagnostics. |
| HFT or tonic | canonical Burst | **Not co-owned in the observed-State contract** | The Burst cuts the State support; every residual child is independently re-gated and parent lineage is preserved. A future latent-background track would require a new contract. |
| any State | Pause | **Forbidden on the same ISI** | Pause splits the State and residual children are independently re-gated. |
| Pause | canonical Burst | **Forbidden on the same ISI** | A candidate spanning Pause is invalid as one event. Any residual pieces must independently pass Event detection; priority alone cannot choose the winner. |
| Pause | `possible_burst` | Review overlap MAY be retained | Confirmation is blocked until the Gap conflict is resolved. |
| canonical Burst | another Burst subtype row on the same support | **Forbidden duplication** | Represent one Event with modifiers. |
| accepted biological interval | artifact or invalid ISI | **Forbidden** | Artifact/invalid support creates a QC/unknown boundary and cannot be counted as accepted biology. |
| any biological track | Review hypothesis | **Allowed epistemically** | Review remains non-biological until an auditable transition creates or links an accepted interval. |

## HFS-specific decision rules

HFS acceptance MUST be determined from HFS evidence only, including its declared
minimum extent, sustained high-frequency support, ISI distribution, tolerated gaps,
and hard breaks. Event labels MUST NOT act as a circular veto on an HFS candidate
that independently passes the State gate.

In particular:

1. A selected canonical Burst inside HFS is a non-destructive overlay.
2. Burst count, Burst group count, and Burst coverage MUST NOT delete HFS.
3. Existing "Burst-dominated HFS", packet-like, and packet-neighbor calculations MAY
   be retained as continuous diagnostic evidence, but MUST NOT become State selection
   gates without a separately versioned and independently validated scientific
   decision.
4. Continuous summaries such as `burst_event_rate`, `burst_occupancy`, and a
   `packetization_index` are preferred to an unvalidated binary dominance label.
5. A homogeneous HFS epoch is a required negative-control case: it SHOULD produce no
   Burst merely because its absolute ISIs are short.
6. Burst detection inside HFS MUST still require the declared local structure,
   context contrast, boundary separation, and artifact checks. Its background
   reference SHOULD be appropriate to the local HFS regime rather than a global
   low-rate baseline.
7. If a packetized record fails HFS's own sustained-state criteria, HFS is absent
   because the State gate failed—not because Burst events overruled it.

## Pause and hard-boundary rules

Pause is an observed long-ISI Gap, not a competing score label. A selected Pause
owns its ISI rows and forms a hard biological boundary for every State and Event.

- A State crossing Pause MUST be split around the full Pause support.
- Each State child MUST independently pass all minimum-size and feature gates.
- A Burst crossing Pause MUST be invalidated as one continuous Event; residual
  candidates, if any, MUST be independently detected and validated.
- Adjacency to Pause is allowed. Sharing Pause's ISI support is not.
- Artifact or invalid-data gaps follow the same non-co-ownership principle, with an
  explicit QC reason rather than a biological Pause label when appropriate.

## Event-State relationships

Cross-track coexistence MUST be materialized explicitly rather than inferred only
from colors in a plot or from a lossy per-ISI label. Each relationship has stable
foreign keys to one Event and one State and records its overlap geometry.

The canonical relationship types are:

```text
event_contained_in_state
event_crosses_state_start
event_crosses_state_end
event_contains_state
event_partially_overlaps_state
```

Disjoint Event-State pairs SHOULD NOT be materialized because doing so is
combinatorial and adds no information. For every materialized relationship, export:

```text
relationship_id
train
event_id
state_id
relationship_type
overlap_start_isi
overlap_end_isi
overlap_isi_n
event_overlap_fraction
state_overlap_fraction
intersection_over_union
```

An Event crossing an HFS boundary MUST NOT be clipped merely to manufacture a
contained relationship. Both original geometries remain intact and the relationship
describes the actual partial overlap.

All automatic relationships MUST be re-materialized in the reviewed/final product.
Relationships created by newly confirmed Events MUST be computed against the current
final State and Gap tracks. A final relationship table containing only
Review-to-Event provenance links is incomplete.

## Counting and derived summaries

Accepted Event counts are counts of unique active `event_id` values. Review rows,
candidate rows, modifier rows, per-ISI rows, and relationship rows MUST NOT increase
that count.

The minimum recommended summaries are:

```text
burst_event_n
high_frequency_burst_event_n
hfs_epoch_n
burst_overlapping_hfs_n
burst_contained_in_hfs_n
hfs_exposure_sec
burst_rate_within_hfs_per_sec
hfs_burst_occupancy
proportion_hfs_epochs_with_at_least_one_burst
```

`high_frequency_burst_event_n` is a subset of `burst_event_n`; it is not added to it.
`hfs_burst_occupancy` MUST be calculated from the union of `Burst ∩ HFS` support
divided by HFS exposure. Overlapping candidate lengths MUST NOT be summed.

For an Event crossing a State boundary, a categorical count MAY use the Event-onset
State if that convention is declared. Exposure-based summaries MUST use the true
overlap fractions. Onset attribution and exposure attribution MUST have different,
explicit names.

State and Event counts or durations MUST NOT be pooled into one denominator merely
because the tracks overlap.

## Review and manual-edit semantics

Review is an epistemic track. `possible_burst` is not an accepted Event and is
ineligible for primary Event counts and metrics until an auditable confirmation.

All manual operations MUST be track-scoped:

- adding, deleting, or resizing a Burst leaves HFS geometry unchanged and recomputes
  relationships;
- adding, deleting, or resizing HFS leaves Burst geometry unchanged and recomputes
  relationships;
- deleting HFS does not delete contained Bursts;
- deleting a Burst does not delete the containing HFS;
- confirming `possible_burst` creates one canonical Burst or links the exact existing
  Event without duplication;
- confirming Pause atomically splits conflicting States and re-gates all children;
- `not_burst` applies only to Event/Burst; a State veto requires an explicit
  track-specific annotation such as `not_hfs`; and
- an edit that creates a forbidden same-track or Gap conflict fails atomically rather
  than silently clipping, merging, ranking, or overwriting intervals.

AUTO, MANUAL, and FINAL products MUST remain separately reconstructable. Every
transition records the reviewer, reason, server time, before/after identities,
parent run and parameter hashes, and a deterministic source-row hash. Manual/final
state MUST NOT enter label-blind threshold estimation, candidate generation, or
automatic scoring.

## Canonical export contract

The normalized v2 product SHOULD include at least:

```text
events
states
gaps
state_event_relationships
review_history
per_isi_multitrack
qc
thresholds
run_metadata
```

The canonical typed artifact is authoritative over human-readable CSV
serializations. Every table MUST include schema/run/parameter identity and stable
foreign keys where applicable.

The per-ISI product MAY have both Event and State populated on the same row, for
example:

```text
pattern_event_family = burst
pattern_event_id     = event_123
pattern_state        = high_frequency_spiking
pattern_state_id     = state_456
```

Existing `pattern_auto` and `pattern_final` MAY remain only as deterministic,
explicitly named `legacy_lossy_projection` outputs. They MUST NOT determine v2
selection, counts, validation, or scientific summaries. The projection MUST record
that coincident labels were collapsed and which precedence rule was used.

## Validation truth contract

Detector performance requires an independently prepared, annotation-complete,
track-aware reference. At least two experts SHOULD annotate Event and State tracks
independently while algorithm proposals are hidden, followed by a recorded
adjudication step. Algorithm-assisted review MAY be studied separately, but its
agreement MUST NOT be relabelled as unbiased detector performance.

Truth SHOULD be normalized into:

```text
state_truth
event_truth
state_event_relation_truth
```

Truth records SHOULD include annotator identity, confidence, boundary uncertainty,
adjudication provenance, train/session/subject grouping, and the exact coordinate
convention. Within-track canonical truth overlap is invalid; cross-track Event-State
overlap is expected.

Required biological scenarios include:

1. homogeneous HFS with no Burst;
2. HFS with one embedded Burst;
3. HFS with multiple embedded Bursts while HFS remains valid;
4. packetized Bursts whose raw ISIs fail the HFS State gate;
5. a Burst crossing an HFS start or end;
6. Pause splitting HFS;
7. artifact/invalid support inside a suspected HFS epoch;
8. Burst interacting with tonic/HFT fragment re-gating;
9. high-frequency Burst outside HFS; and
10. a non-high-frequency Burst inside HFS.

Automatic detector validation MUST use label-blind predictions and thresholds that
were not tuned on the evaluation reference. Reviewed/final products estimate
`adjudicated_agreement`, not unbiased detector performance.

## Validation metrics

Event, State, Gap, and Review estimands MUST remain separate.

### Event family

Primary Burst detection uses one-to-one optimal matching at the `event_family`
level. Report event precision, recall, F1, false Events per minute, IoU across a
declared threshold grid, onset/offset absolute error, and split/merge rates.

Short Events are sensitive to a one-ISI boundary shift, so a single IoU threshold is
insufficient. Spike/ISI support precision and recall SHOULD also be reported.

### Event modifiers

`extent_class` and `frequency_class` are evaluated only among successfully matched
Burst-family pairs. Modifier disagreement MUST NOT be converted into an additional
family-level false positive and false negative. Report confusion matrices and
conditional accuracy/sensitivity for each modifier.

### State and Gap

Report time- or ISI-weighted precision, recall, F1/Jaccard, epoch-level recall,
boundary error, and duration bias separately for each State and Gap class.

### Coexistence and safety estimands

At minimum, report:

```text
nested_burst_recall
hfs_with_burst_state_recall
predicted_state_event_relationship_precision
predicted_state_event_relationship_recall
relationship_type_accuracy
homogeneous_hfs_false_bursts_per_minute
truth_hfs_erroneously_split_or_suppressed_fraction
```

The same Event and HFS State may each be a true positive on its own track. They MUST
NOT be pooled into one micro count.

### Review and uncertainty

Review candidates are summarized by target coverage, workload, decision rate,
review time, and promotion outcome. They are excluded from primary Event
precision/recall/F1.

Confidence intervals SHOULD resample the highest independent unit available
(subject, then session, then train), not individual ISIs or Events. If the number of
independent groups is insufficient, report raw counts and per-group distributions
rather than a misleading interval-level confidence interval.

## Authority scope and biological ground truth

`authority_scope` and `biological_ground_truth` describe different properties and
MUST be stored separately.

Recommended `authority_scope` values are:

| `authority_scope` | Meaning |
|---|---|
| `none_preview` | Experimental/non-canonical proposal; not a final product |
| `automatic_prediction_record` | Canonical immutable record of what the automatic algorithm predicted |
| `reviewed_prediction_record` | Canonical immutable record after auditable human review |
| `independent_reference` | Independently prepared reference used for validation |
| `legacy_lossy_projection` | Compatibility view that collapses multiple tracks |

For every automatic or reviewed prediction product:

```text
biological_ground_truth = FALSE
```

An authoritative prediction record is authoritative only as a record of the stated
algorithm/review process. It does not become biological truth because its schema is
canonical, its hashes are valid, or a reviewer confirmed it.

`biological_ground_truth = TRUE` MAY appear only on an explicitly governed reference
artifact whose truth role, annotators, independence/blinding, adjudication, and
provenance are declared. An algorithm-assisted adjudicated reference MUST disclose
that assistance and MUST NOT be used to estimate unbiased performance for the same
algorithm.

## Promotion gates

### Gate A: freeze the scientific specification

Before implementation, the project owner accepts this ontology, compatibility
matrix, interval convention, modifier model, counting rules, and authority
distinction. Changes after acceptance require a new policy/schema version.

### Gate B: promote v2 to the authoritative prediction representation

All of the following MUST pass before an output receives
`authority_scope = automatic_prediction_record` or
`reviewed_prediction_record`:

1. Every allowed and forbidden combination has deterministic regression tests.
2. Homogeneous HFS produces no Burst solely because its ISIs are short.
3. A valid embedded Burst remains visible without changing a valid HFS interval.
4. Pause and invalid support split all conflicting States and invalidate crossing
   Events; every residual child is re-gated.
5. One physical Burst has one Event ID; modifiers never increase Event count.
6. Automatic and manually created Event-State relationships are fully materialized
   in FINAL, with validated foreign keys and geometry.
7. Per-ISI projections exactly reproduce interval tables without same-track overlap.
8. AUTO, FINAL, diagnostic, QC, thresholds, parameters, input, and review history have
   deterministic versioned hashes.
9. Label-blind execution removes all manual/final information before calibration
   and detection.
10. A failed v2 invariant fails the v2 product closed. It MUST NOT silently fall back
    to the single-label legacy result while retaining a v2 authority claim.
11. Legacy and v2 outputs are dual-written during a declared migration period, and
    count/duration differences are explicitly audited.
12. A schema migrator or explicit non-migratable status exists for persisted review
    histories and interval identities.

Passing Gate B establishes representation authority only.

### Gate C: enable v2 by default and support biological-performance claims

All of the following are additionally required:

1. An independently annotated real dataset contains adequate examples of nested
   HFS-Burst coexistence and the required negative controls.
2. Thresholds are frozen without using the evaluation reference.
3. Event, State, Gap, modifiers, and coexistence relationships are evaluated with the
   metrics in this contract.
4. Performance is reported per train/session/subject with appropriate uncertainty.
5. Homogeneous-HFS false Burst rate and erroneous HFS suppression are explicitly
   acceptable under predeclared criteria.
6. Failure cases, abstentions, data-quality dependence, and lack of external
   generalization are stated honestly.

Passing Gate C permits default activation and appropriately bounded biological
claims. It still does not make each prediction biological ground truth.

## Post-promotion runtime safety

After promotion, every run MUST continue to validate:

- schema and policy version;
- interval geometry and within-track non-overlap;
- allowed cross-track relationships;
- relationship and lineage foreign keys;
- modifier/event identity uniqueness;
- count and per-ISI reconstruction consistency;
- parent run, input, parameter, threshold, and review hashes; and
- declared `authority_scope` and `biological_ground_truth` values.

An invariant failure MUST preserve the detector/audit evidence, mark the v2 product
invalid, and prevent formal v2 export. It MUST NOT rewrite or delete a previous valid
artifact.

## Migration hazards that must remain visible

1. The legacy single-label detector can suppress embedded Burst candidates or reject
   HFS using Burst-density evidence. Canonical v2 MUST be derived from the intact
   pre-protection candidate evidence, not reconstructed from legacy `pattern_auto`.
2. Existing peer labels for `burst`, `long_burst`, `high_frequency_burst`, and
   `hf_burst` can duplicate events unless normalized to one family plus modifiers.
3. Existing downstream plots, analyses, and exports may assume exactly one label per
   ISI; they require explicit v2 readers and must not silently consume the lossy
   projection.
4. A final relationship table that retains only Review-to-Event links loses automatic
   HFS-Burst relationships and is not a complete v2 final product.
5. Strict full-label validation can penalize a correct Burst boundary twice when only
   a modifier is wrong; family matching and modifier evaluation must be separated.
6. Re-keying Event or State intervals can invalidate append-only review history; the
   migration must preserve an auditable identity map.
7. Making Preview authoritative before these corrections would freeze known semantic
   defects into the public schema.
8. Independent biological validation remains outstanding; deterministic software
   behavior and simulation regression are not substitutes.

## Final normative decisions

1. HFS and canonical Burst MAY coexist over the same ISIs.
2. Burst MUST NOT split or delete HFS.
3. HFS MUST be accepted or rejected from State evidence, not Burst density.
4. Burst-rich and packetized HFS evidence remains continuous and diagnostic unless a
   later independently validated contract states otherwise.
5. Pause is a hard Gap boundary and cannot co-own ISIs with State or Event.
6. Long and high-frequency Burst are modifiers of one canonical Burst Event.
7. Review hypotheses are not biological Events before an auditable transition.
8. Canonical counts use unique interval IDs and union geometry, never duplicate
   subtype or per-ISI rows.
9. Representation authority and biological ground truth are separate declarations.
10. The current Preview MUST NOT be made authoritative merely by changing its flag;
    v2 first passes Gate B, and default biological use additionally passes Gate C.
