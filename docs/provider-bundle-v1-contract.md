# STPD provider bundle v1 contract

## Status and boundary

This document freezes the Phase-A interchange contract. Its three version tokens are
`stpd_provider_bundle_v1`, `stpd_train_row_isi_v1`, and
`stpd_provider_ontology_v1`. A bundle consists of exactly five typed tables:
`provider_runs`, `candidate_intervals`, `thresholds`, `normalization_audit`, and
`calibration_lineage`. Extra columns, implicit defaults, free-form enum values, and
lossy coordinate conversion fail closed.

The frozen Phase-A contract SHA-256 is
`29a2a660ee594494c6687433db1448825bf5ac5f518f36344bdf81f65ee01a6b`.

The provider bundle keeps four authorities distinct: immutable provider AUTO
output, calibration evidence, a separately materialized adjudicated result, and a
sealed reference store. It is an interoperability boundary for a multi-algorithm,
human-in-the-loop workbench; it is not merely an input to the STPD grammar.

Notation below is `chr` (character), `int` (integer), `dbl` (double), and `lgl`
(logical). `required` means non-`NA`, scalar-valued, and non-empty for `chr`.
`conditional` means `NA` is allowed only under the stated invariant. SHA-256 values
are lowercase 64-hex strings. All foreign keys are checked within the submitted
bundle; dangling references are invalid.

## Fixed schemas

### `provider_runs` (24 columns)

Primary key: `provider_run_id`.

| # | field | type | nullability | meaning / invariant |
|---:|---|---|---|---|
| 1 | `schema_version` | chr | required | Exactly `stpd_provider_bundle_v1`. |
| 2 | `provider_run_id` | chr | required | Primary key; deterministic run identity. |
| 3 | `provider_key` | chr | required | Registered provider implementation key. |
| 4 | `provider_kind` | chr | required | Provider family enum. |
| 5 | `provider_version` | chr | required | Provider scientific version. |
| 6 | `adapter_key` | chr | required | Registered, allowlisted adapter key. |
| 7 | `adapter_version` | chr | required | Adapter contract/code version. |
| 8 | `output_role` | chr | required | What the provider emitted. |
| 9 | `authority_scope` | chr | required | Maximum authority of that output. |
| 10 | `generation_mode` | chr | required | Declared execution mode. |
| 11 | `information_access` | chr | required | Information available during generation. |
| 12 | `capability_profile` | chr | required | Registered input/output capability. |
| 13 | `run_status` | chr | required | `complete` or fail-closed `rejected`. |
| 14 | `dataset_snapshot_sha256` | chr | required | Canonical dataset/QC snapshot hash. |
| 15 | `input_artifact_sha256` | chr | required | Exact provider input-artifact hash. |
| 16 | `timestamp_spine_sha256` | chr | required | Canonical ordered timestamp-spine hash. |
| 17 | `params_sha256` | chr | required | Complete typed effective-parameter hash. |
| 18 | `calibration_bundle_id` | chr | conditional | Required for a `support_to_native` automatic-prediction consumer; otherwise may be `NA`. |
| 19 | `raw_output_sha256` | chr | required | Exact raw output, including a typed rejection artifact. |
| 20 | `normalized_output_sha256` | chr | required | Canonical normalized output for this run. |
| 21 | `provider_code_sha256` | chr | required | Provider code fingerprint. |
| 22 | `adapter_code_sha256` | chr | required | Adapter code fingerprint. |
| 23 | `contract_sha256` | chr | required | Hash of the frozen schemas, registries, and canonicalization rules. |
| 24 | `created_utc` | chr | required | RFC-3339 UTC audit time; excluded from scientific identity. |

### `candidate_intervals` (26 columns)

Primary key: `candidate_id`. Foreign keys:
`provider_run_id -> provider_runs.provider_run_id` and
`normalization_audit_id -> normalization_audit.normalization_audit_id`.

| # | field | type | nullability | meaning / invariant |
|---:|---|---|---|---|
| 1 | `schema_version` | chr | required | Bundle schema version. |
| 2 | `candidate_id` | chr | required | Provider-run-qualified primary key. |
| 3 | `provider_run_id` | chr | required | Provider-run foreign key. |
| 4 | `source_record_key` | chr | required | Immutable provider-owned source identity. |
| 5 | `source_record_sha256` | chr | required | Canonical hash of the source record. |
| 6 | `normalization_audit_id` | chr | required | Successful normalization-audit foreign key. |
| 7 | `train_key` | chr | required | Dataset-snapshot-qualified train identity. |
| 8 | `train_timestamp_sha256` | chr | required | Hash of this train's ordered timestamps. |
| 9 | `record_kind` | chr | required | Assertion/candidate/support enum. |
| 10 | `provider_decision` | chr | required | Positive/negative/indeterminate provider decision. |
| 11 | `semantic_track` | chr | required | `event`, `state`, or `gap`. |
| 12 | `proposed_label` | chr | required | Ontology label compatible with the track. |
| 13 | `canonical_start_isi` | int | required | First included train-row ISI index. |
| 14 | `canonical_end_isi` | int | required | Last included train-row ISI index. |
| 15 | `canonical_start_spike` | int | required | First supporting spike-row index. |
| 16 | `canonical_end_spike` | int | required | Last supporting spike-row index. |
| 17 | `canonical_start_time_sec` | dbl | required | Exact first supporting spike time in seconds. |
| 18 | `canonical_end_time_sec` | dbl | required | Exact last supporting spike time in seconds. |
| 19 | `canonical_interval_closure` | chr | required | Exactly `closed`. |
| 20 | `score_name` | chr | conditional | `NA` only when no provider score exists. |
| 21 | `score_value` | dbl | conditional | Present iff `score_name` is present; finite. |
| 22 | `score_direction` | chr | required | `not_applicable` iff the score pair is absent. |
| 23 | `uncertainty_kind` | chr | required | Typed uncertainty enum; use `none`, not a blank value. |
| 24 | `uncertainty_lower` | dbl | conditional | Both bounds absent for `none`; otherwise both finite and ordered. |
| 25 | `uncertainty_upper` | dbl | conditional | Same group invariant as `uncertainty_lower`. |
| 26 | `evidence_manifest_sha256` | chr | conditional | Hash of a separate typed evidence manifest; `NA` when none exists. |

The candidate and its audit row must agree exactly on provider run, source record,
train, and canonical geometry. Equal geometry from different provider runs remains
different evidence and is never deduplicated or fused automatically.

### `thresholds` (17 columns)

Primary key: `threshold_id`. Foreign key:
`provider_run_id -> provider_runs.provider_run_id`.

| # | field | type | nullability | meaning / invariant |
|---:|---|---|---|---|
| 1 | `schema_version` | chr | required | Bundle schema version. |
| 2 | `threshold_id` | chr | required | Primary key. |
| 3 | `provider_run_id` | chr | required | Producing/declaring provider-run foreign key. |
| 4 | `calibration_bundle_id` | chr | conditional | Required when bound to calibration; otherwise `NA`. |
| 5 | `threshold_name` | chr | required | Stable provider-facing name. |
| 6 | `parameter_path` | chr | conditional | Required for detector input/effective parameter; otherwise `NA`. |
| 7 | `threshold_value` | dbl | required | Finite typed value. |
| 8 | `threshold_unit` | chr | required | Fixed unit enum. |
| 9 | `threshold_role` | chr | required | Reported, calibrated, detector-input, or effective role. |
| 10 | `scope_type` | chr | required | Dataset/group/train/run scope enum. |
| 11 | `scope_sha256` | chr | required | Canonical identity of the complete scope set. |
| 12 | `semantic_track` | chr | required | `event`, `state`, `gap`, or `not_applicable`. |
| 13 | `target_label` | chr | required | Track-compatible ontology label; exactly `not_applicable` when the track is. |
| 14 | `source_kind` | chr | required | Threshold provenance enum. |
| 15 | `source_record_sha256` | chr | conditional | Required for unsupervised-data-derived, manual-calibration, or externally declared values. |
| 16 | `comparison_operator` | chr | required | Fixed comparison enum. |
| 17 | `evidence_manifest_sha256` | chr | conditional | Separate evidence-manifest hash, or `NA`. |

`calibration_bundle_id`, when present anywhere in the bundle, must resolve to a
consistent `calibration_lineage` bundle and parameter/threshold hashes. It is not a
license to read sealed evaluation truth.

A `calibration_bundle` threshold source must carry a resolving
`calibration_bundle_id`. A detector-input/effective-parameter threshold emitted by
a `support_to_native` consumer must use that source kind and the same bundle as the
consumer run. Calibration-output threshold provenance must agree with the lineage:
`mean_isi` or `logisi_newbd` uses `unsupervised_data_derived`, while
`manual_examples` uses `manual_calibration`.

### `normalization_audit` (26 columns)

Primary key: `normalization_audit_id`. Foreign key:
`provider_run_id -> provider_runs.provider_run_id`.

| # | field | type | nullability | meaning / invariant |
|---:|---|---|---|---|
| 1 | `schema_version` | chr | required | Bundle schema version. |
| 2 | `normalization_audit_id` | chr | required | Primary key. |
| 3 | `provider_run_id` | chr | required | Provider-run foreign key. |
| 4 | `source_record_key` | chr | required | Original provider record identity. |
| 5 | `source_record_sha256` | chr | required | Original record hash. |
| 6 | `train_key` | chr | required | Declared target train. |
| 7 | `source_coordinate_convention` | chr | required | Fixed coordinate-convention enum. |
| 8 | `source_index_base` | chr | required | `zero`, `one`, or `not_applicable`. |
| 9 | `source_interval_closure` | chr | required | Explicit source closure enum. |
| 10 | `source_time_unit` | chr | required | Explicit unit, including `not_applicable`. |
| 11 | `source_start_index` | int | conditional | Required by index-based conventions; otherwise `NA`. |
| 12 | `source_end_index` | int | conditional | Same group invariant as `source_start_index`. |
| 13 | `source_start_time` | dbl | conditional | Required by time-based conventions; otherwise `NA`. |
| 14 | `source_end_time` | dbl | conditional | Same group invariant as `source_start_time`. |
| 15 | `transformation` | chr | required | Exact allowlisted mapping. |
| 16 | `tolerance_sec` | dbl | required | Finite and non-negative; never enables nearest-spike snapping. |
| 17 | `canonical_start_isi` | int | conditional | Required only when status is `normalized`. |
| 18 | `canonical_end_isi` | int | conditional | Required only when status is `normalized`. |
| 19 | `canonical_start_spike` | int | conditional | Required only when status is `normalized`. |
| 20 | `canonical_end_spike` | int | conditional | Required only when status is `normalized`. |
| 21 | `canonical_start_time_sec` | dbl | conditional | Required only when status is `normalized`. |
| 22 | `canonical_end_time_sec` | dbl | conditional | Required only when status is `normalized`. |
| 23 | `ambiguity_detected` | lgl | required | True forces rejection. |
| 24 | `normalization_status` | chr | required | `normalized` or `rejected`. |
| 25 | `error_code` | chr | conditional | Stable typed code required only for `rejected`. |
| 26 | `error_detail` | chr | conditional | Optional human diagnostic; never used for branching or identity. |

A normalized row has complete canonical geometry, `ambiguity_detected = FALSE`,
and no error fields. A rejected row has no canonical geometry and a typed
`error_code`. Rejected records cannot appear in `candidate_intervals`.

### `calibration_lineage` (18 columns)

Primary key: `calibration_lineage_id`. Foreign keys:
`calibration_provider_run_id -> provider_runs.provider_run_id` and
`consumer_provider_run_id -> provider_runs.provider_run_id`.
The tuple `(calibration_bundle_id, consumer_provider_run_id)` is unique.

| # | field | type | nullability | meaning / invariant |
|---:|---|---|---|---|
| 1 | `schema_version` | chr | required | Bundle schema version. |
| 2 | `calibration_lineage_id` | chr | required | Primary key. |
| 3 | `calibration_bundle_id` | chr | required | Frozen parameter-bundle identity. |
| 4 | `calibration_provider_run_id` | chr | required | Calibration producer foreign key. |
| 5 | `consumer_provider_run_id` | chr | required | Detector consumer foreign key. |
| 6 | `calibration_source_kind` | chr | required | Fixed source-kind enum. |
| 7 | `calibration_authority_scope` | chr | required | Maximum scientific authority of calibration. |
| 8 | `calibration_dataset_snapshot_sha256` | chr | required | Dataset used for calibration. |
| 9 | `parameter_bundle_sha256` | chr | required | Frozen effective parameter payload. |
| 10 | `threshold_set_sha256` | chr | required | Canonically sorted bound threshold set. |
| 11 | `grouping_key` | chr | required | Declared grouping/partition variable, or the registered literal for global calibration. |
| 12 | `calibration_group_set_sha256` | chr | conditional | Required when calibration groups are defined. |
| 13 | `evaluation_group_set_sha256` | chr | conditional | Required for held-out performance. |
| 14 | `group_overlap_count` | int | required | Non-negative; zero is mandatory for held-out claims. |
| 15 | `labels_used` | lgl | required | Whether any labels informed calibration. |
| 16 | `reference_annotations_used` | lgl | required | Whether sealed/manual reference annotations informed calibration. |
| 17 | `group_separation_status` | chr | required | Typed separation result. |
| 18 | `performance_use` | chr | required | Permitted estimand/use enum. |

The consumer is exactly an allowlisted `native_stpd` automatic-prediction run with
automatic-prediction authority and `support_to_native` generation. A calibration
producer cannot be substituted as its own consumer.

Calibration source identity is fail closed. `mean_isi` is produced by the
Mean-ISI provider and `logisi_newbd` by the LogISI/newBD provider. Manual examples
may be processed by either explicitly declared calibration algorithm, but require
`information_access=manual_aware`, `labels_used=TRUE`, and manual-calibration
threshold provenance. `reference_annotations_used=TRUE` implies
`labels_used=TRUE`; the reference flag is reserved for actual access to sealed
evaluation annotations, not ordinary training examples. Manual or unknown access
is tainted through the frozen bundle to the native consumer and cannot be
downgraded to label-blind.

Phase A records calibration provenance but grants no performance authority:
`performance_use` must be `not_eligible`. Other enum values are reserved for a
later validator. Phase B1 may enable them only after reading canonical group-set
artifacts, verifying their hashes and dataset/group identities, and recomputing a
zero intersection. Caller-supplied set hashes and `group_overlap_count` are not
proof of disjointness.

## Versioned registries and allowlist

All values are case-sensitive.

- `provider_kind`: `native_stpd`, `mean_isi`, `logisi_newbd`,
  `external_data_import`.
- `output_role`: `automatic_prediction`, `candidate_support`,
  `calibration_output`.
- `authority_scope`: `automatic_prediction_record`, `support_evidence_only`,
  `calibration_parameter_only`, `none_candidate`.
- `generation_mode`: `native_only`, `external_only`, `support_to_native`.
- `information_access`: `label_blind`, `manual_aware`, `unknown`.
- `capability_profile`: `native_composed_v1`,
  `burst_interval_threshold_v1`, `external_interval_v1`,
  `external_per_isi_v1`.
- `run_status`: `complete`, `rejected`.
- candidate `record_kind`: `automatic_assertion`, `candidate`, `support`;
  `provider_decision`: `positive`, `negative`, `indeterminate`;
  `semantic_track`: `event`, `state`, `gap`.
- ontology: Event = `burst`, `long_burst`; State = `broad_hfs`, `hft`,
  `hf_irregular`, `tonic`; Gap = `pause`.

Capability further restricts this global ontology for both candidates and labelled
thresholds:

| capability | allowed track / label |
|---|---|
| `native_composed_v1` | every registered Event, State, and Gap label |
| `burst_interval_threshold_v1` | exactly `event / burst` |
| `external_interval_v1` | every explicitly registered Event, State, and Gap label |
| `external_per_isi_v1` | every explicitly registered Event, State, and Gap label |

Thus Mean-ISI and LogISI/newBD cannot claim `long_burst`, HFS, Tonic, or Pause.
External data still requires an explicit ontology mapping; it is not a free-form
label channel.

`hft` and `hf_irregular` are mutually exclusive subtypes of `broad_hfs`, not
independent top-level States. Every positive subtype row requires exactly one
positive Broad-HFS parent from the same provider run, train, and train-timestamp
hash with exactly the same canonical ISI, spike, and time geometry. One
provider-run/train pair cannot mix timestamp-spine hashes. An adapter receiving a
subtype-only external record must materialize the implied parent transparently and
preserve its source evidence.
- `score_direction`: `higher_is_stronger`, `lower_is_stronger`,
  `not_applicable`; `uncertainty_kind`: `none`, `confidence_interval`,
  `credible_interval`, `provider_interval`.
- threshold `semantic_track`: `event`, `state`, `gap`, `not_applicable`;
  `target_label`: the ontology above plus `not_applicable`; `threshold_unit`:
  `sec`, `ms`, `hz`, `ratio`, `count`,
  `probability`, `dimensionless`, `log10_sec`; `threshold_role`: `reported`,
  `calibration_output`, `detector_input`, `effective_parameter`; `scope_type`:
  `dataset`, `recording_group`, `train`, `provider_run`; `source_kind`:
  `provider_default`, `unsupervised_data_derived`, `manual_calibration`,
  `external_declared`, `calibration_bundle`; `comparison_operator`: `lt`, `le`,
  `gt`, `ge`, `eq`, `range_lower`, `range_upper`.
- normalization `source_coordinate_convention`: `train_row_isi_index`,
  `diff_timestamp_isi_index`, `spike_index_span`, `spike_time_span`,
  `per_isi_diff_index`; `source_index_base`: `zero`, `one`, `not_applicable`;
  `source_interval_closure`: `closed`, `left_closed_right_open`,
  `left_open_right_closed`, `open`, `point`; `source_time_unit`: `s`, `ms`,
  `us`, `not_applicable`; `normalization_status`: `normalized`, `rejected`.
- `transformation`: `identity_train_row_one_based`, `diff_one_based_plus_one`,
  `diff_zero_based_plus_two`, `spike_one_based_span_to_isi`,
  `spike_zero_based_span_to_isi`, `exact_spike_time_span_to_isi`,
  `per_isi_one_based_runs_plus_one`, `per_isi_zero_based_runs_plus_two`.
- calibration `calibration_source_kind`: `native_unsupervised`, `mean_isi`,
  `logisi_newbd`, `manual_examples`, `external_parameters`;
  `calibration_authority_scope`: exactly `calibration_parameter_only`;
  `group_separation_status`: `not_required`, `verified_disjoint`,
  `overlap_rejected`, `unverifiable_rejected`; `performance_use`:
  `label_blind_detector_performance`, `heldout_detector_performance_only`,
  `adjudicated_agreement_only`, `not_eligible`.

In Phase A, `native_unsupervised` and `external_parameters` are reserved enum
values without an allowlisted calibration-output route. Actual calibration
lineage accepts `mean_isi`, `logisi_newbd`, or the explicitly manual-aware
`manual_examples` route described above. Although the performance-use enum is
versioned now, only `not_eligible` is operational in Phase A.

For a complete run, authority is a function of output role:
`automatic_prediction -> automatic_prediction_record`, `candidate_support ->
support_evidence_only`, and `calibration_output -> calibration_parameter_only`.
A rejected run has `none_candidate` authority and emits no normalized candidate or
threshold rows. `manual_aware` or `unknown` information access is never silently
reported as label-blind detector performance.

The Phase-A adapter allowlist is the following closed set; every adapter version is
`1.0.0`:

| provider / adapter | capability | allowed role / generation mode |
|---|---|---|
| `native_stpd` / `native_stpd_postcomposer_v1` | `native_composed_v1` | `automatic_prediction` / `native_only` |
| `native_stpd` / `native_stpd_postcomposer_v1` | `native_composed_v1` | `automatic_prediction` / `support_to_native` |
| `mean_isi` / `mean_isi_v1` | `burst_interval_threshold_v1` | `automatic_prediction` / `external_only` |
| `mean_isi` / `mean_isi_v1` | `burst_interval_threshold_v1` | `candidate_support` / `external_only` |
| `mean_isi` / `mean_isi_v1` | `burst_interval_threshold_v1` | `calibration_output` / `support_to_native` |
| `logisi_newbd` / `logisi_newbd_v1` | `burst_interval_threshold_v1` | `automatic_prediction` / `external_only` |
| `logisi_newbd` / `logisi_newbd_v1` | `burst_interval_threshold_v1` | `candidate_support` / `external_only` |
| `logisi_newbd` / `logisi_newbd_v1` | `burst_interval_threshold_v1` | `calibration_output` / `support_to_native` |
| `external_data_import` / `external_interval_v1` | `external_interval_v1` | `automatic_prediction` or `candidate_support` / `external_only` |
| `external_data_import` / `external_per_isi_v1` | `external_per_isi_v1` | `automatic_prediction` or `candidate_support` / `external_only` |

Imported files are pure data, never executable code. A registered provider,
capability, role, or mode mismatch is rejected; no runtime plugin registration or
fallback to a generic adapter is permitted. Versions such as `latest` are invalid.

Mean-ISI and LogISI/newBD are `candidate_support`/
`support_evidence_only` by default. They become primary algorithms only through an
explicit run declared as `automatic_prediction`/
`automatic_prediction_record`; that declaration does not make their scores
comparable to STPD scores. `support_to_native` is two runs linked by
`calibration_lineage`: calibration provider -> frozen bundle -> native consumer.

## Canonical coordinate mapping

For strictly increasing timestamps `t[1],...,t[n]`, canonical train-row ISI row
`r` is defined for `r = 2,...,n` as `t[r] - t[r-1]`. A closed canonical ISI
interval `[s,e]` therefore has:

- ISI rows `s:e`;
- supporting spike rows `(s-1):e`;
- times `t[s-1]` through `t[e]`;
- `n_isi = e-s+1` and `n_spikes = e-s+2`.

The frozen mappings are:

- one-based `diff(timestamp)[a:b]` -> canonical ISI `[a+1,b+1]`;
- zero-based diff/per-ISI `[a:b]` -> canonical ISI `[a+2,b+2]`;
- one-based spike span `[p,q]` -> canonical ISI `[p+1,q]`;
- zero-based spike span `[p,q]` -> canonical ISI `[p+2,q+1]`;
- time spans -> canonical rows only when both converted boundaries match unique
  timestamps exactly under the declared unit and tolerance.

The convention/base/transformation triple must match one registry row. Every
index convention uses its declared zero/one base, index fields only,
`source_time_unit=not_applicable`, and `tolerance_sec=0`. `spike_time_span` uses
`source_index_base=not_applicable`, time fields only, one of `s`, `ms`, or `us`,
and `exact_spike_time_span_to_isi`. A normalized audit also independently obeys
the canonical ISI/spike geometry above; a syntactically valid audit ID cannot make
an inconsistent conversion valid.

The index formulas above are for closed source intervals. Before applying them,
integer closures adjust `(start,end)` by `(0,0)` for `closed`, `(0,-1)` for
`left_closed_right_open`, `(1,0)` for `left_open_right_closed`, and `(1,-1)` for
`open`. Phase A accepts those closures for index coordinates and only `closed` for
time spans. `point` remains a reserved enum and is not authorized by the Phase-A
coordinate matrix; a single index is represented as a closed `[a,a]` interval.

Closure conversion must preserve the same mathematical support; an empty result
is rejected. Missing units or bases, out-of-range or reversed coordinates,
non-monotonic/duplicate timestamps, non-unique time matches, or dataset/train/
timestamp hash mismatch fail closed. `tolerance_sec` may verify an explicitly
declared exact mapping but may not select a nearest spike or break a tie.

## Hash DAG and canonicalization

Use domain-separated SHA-256 over fixed-field-order typed JSON: UTF-8 domain bytes,
one NUL byte, then compact canonical JSON bytes. Never use locale-dependent text,
incoming row order, creation timestamps, or abbreviated hashes.

The registry itself contains the ordered preimage field list for every entity,
the JSON settings (`UTF-8`, NUL domain separator, `auto_unbox`, explicit JSON
`null` for `NA`, full numeric precision, compact output, named-vector handling),
and the explicit dependency DAG. ID functions consume those same registered
field lists; changing a preimage or encoding rule therefore changes
`contract_sha256`.

1. `contract_sha256 = H("stpd-provider-contract-v1", registry)` binds the five
   schemas, enums, allowlist, capability/ontology and calibration-source maps,
   coordinate rules, identity preimages, canonicalization parameters, dependency
   DAG, hash domains, errors, authority locks, and limits.
2. Raw bytes produce `input_artifact_sha256` and `raw_output_sha256`; canonical
   dataset and train timestamp spines produce their corresponding snapshot hashes.
3. `provider_run_id = H("stpd-provider-run-v1", ...)` binds
   `schema_version`; provider/adapter keys and versions; output role, authority,
   mode, access, capability, and run status; dataset/input/timestamp/parameter
   hashes; nullable calibration bundle; provider/adapter code hashes; and contract
   hash. It excludes post-run output hashes and `created_utc`.
4. `normalization_audit_id = H("stpd-normalization-audit-v1", ...)` binds every
   audit field except `schema_version`, its own ID, and non-authoritative
   `error_detail`. `source_record_sha256` binds the exact provider-owned source
   record.
5. `candidate_id = H("stpd-candidate-interval-v1", ...)` binds every candidate
   field except `schema_version`, its own ID, and `source_record_key` (the immutable
   source hash is included). `threshold_id = H("stpd-threshold-v1", ...)` binds
   every threshold field except `schema_version`, its own ID, and
   `calibration_bundle_id`; that bundle is independently verified through lineage.
6. `threshold_set_sha256` hashes sorted threshold IDs.
   `calibration_bundle_id = H("stpd-calibration-bundle-v1", calibration provider
   run ID, threshold-set hash, parameter-bundle hash)`.
   `calibration_lineage_id = H("stpd-calibration-lineage-v1", ...)` binds all
   lineage fields except its own ID, including the consumer and permitted use.
7. `normalized_output_sha256 = H("stpd-provider-normalized-output-v1", ...)`
   binds the exact registered JSON keys `candidate_ids`, `threshold_ids`, and
   `normalization_audit_ids`, each containing the sorted IDs for one provider run.
   It includes every threshold role emitted by that run, not only reported
   thresholds. A later bundle manifest may bind all five sorted table hashes,
   including lineage, without creating a self-reference.

The registered DAG is acyclic and distinguishes each use of the run and threshold
domains:
`contract -> calibration-provider run -> calibration thresholds -> threshold set
-> calibration bundle -> native consumer run`. Provider-output runs then bind
their normalization audits, candidates/all provider-output thresholds, and
normalized-output hashes; native-consumer and calibration-run normalized-output
variants bind all IDs emitted under their respective run. Calibration lineage
binds producer, bundle, consumer, threshold set, and group metadata. Tests check
the DAG for cycles, compare the registered normalized-output preimage with the
executable hash function, and freeze the full contract hash.

The same deterministic identity with different raw or normalized bytes is a
nondeterministic-output error, not a new silent result. Human review never changes
any provider hash; adjudication v2 creates a separately hashed descendant.

## Typed fail-closed errors

Errors are structured conditions with a stable `code`, table, field/row key, and
non-authoritative detail. Phase A reserves these error families; implementations
must use the exact code registry and may not branch on prose:

- schema/identity: `schema_version_unsupported`, `required_field_missing`,
  `unknown_column`, `type_invalid`, `nullability_violation`, `enum_invalid`,
  `primary_key_duplicate`, `foreign_key_missing`, `sha256_invalid`,
  `hash_mismatch`, `id_mismatch`, `resource_limit_exceeded`;
- provider/API: `provider_not_allowlisted`, `adapter_not_allowlisted`,
  `provider_capability_mismatch`, `provider_mode_role_invalid`,
  `provider_authority_invalid`, `executable_import_forbidden`,
  `provider_nondeterministic_output`;
- dataset/train: `dataset_snapshot_mismatch`, `train_not_found`,
  `train_timestamp_hash_mismatch`, `timestamp_non_monotonic`,
  `timestamp_duplicate`, `source_record_hash_mismatch`;
- coordinates: `coordinate_convention_missing`, `index_base_missing`,
  `interval_closure_missing`, `time_unit_missing`, `time_unit_unsupported`,
  `coordinate_non_finite`, `coordinate_non_integer`, `coordinate_reversed`,
  `coordinate_empty`, `coordinate_out_of_range`, `time_alignment_missing`,
  `time_alignment_ambiguous`, `time_tolerance_invalid`,
  `coordinate_roundtrip_mismatch`;
- candidate/threshold: `label_unknown`, `label_track_mismatch`,
  `score_contract_invalid`, `uncertainty_contract_invalid`,
  `threshold_non_finite`, `threshold_unit_invalid`,
  `threshold_scope_invalid`, `threshold_operator_invalid`;
- calibration/leakage: `calibration_bundle_missing`,
  `calibration_bundle_hash_mismatch`, `calibration_lineage_invalid`,
  `calibration_group_overlap`, `calibration_group_unverifiable`,
  `label_blind_leakage`;
- authority/estimand: `adjudication_in_provider_bundle`,
  `reference_in_provider_bundle`, `authority_role_conflict`,
  `estimand_authority_mismatch`;
- reserved adjudication-v2 boundary: `review_action_unsupported`,
  `review_parent_not_found`, `review_precondition_mismatch`,
  `review_source_stale`, `review_bounds_invalid`,
  `review_hard_boundary_crossed`.

## Phase-A non-goals

Phase A adds contracts, typed prototypes, a closed registry, validators, tests, and
documentation only. It does **not** change native detection or composition; run
the native product through the composer again; implement adapters or
normalization; execute imported code; fuse/deduplicate/vote across providers;
harmonize scores; add UI; alter Phase-2B-v1 review hashes; expose reference truth;
or implement split, merge, relabel, or create review actions. Those require their
later gated phases and, for scientific composition, a new policy/schema plus an
explicit scientific decision.
