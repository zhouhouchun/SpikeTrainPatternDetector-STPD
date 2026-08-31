# STPD provider adapters: Phase B2 contract

## Status and normative dependencies

This document freezes the first allowlisted provider-import boundary. The
registry identifier and function are exactly
`stpd_provider_adapter_registry_v1` and
`stpd_provider_adapter_registry_v1()`. It is normative for Phase B2 and does
not alter either preceding freeze:

- provider-bundle v1 SHA-256:
  `29a2a660ee594494c6687433db1448825bf5ac5f518f36344bdf81f65ee01a6b`;
- provider-adapter protocol v1 SHA-256:
  `5b0cc6ca2fe6bdf2bb7bd2bddb066632ea3b0646b1096be87c77bec292a26877`.

Phase B2 adds adapters and materialization only. It does not change Burst,
Pause, Tonic, Broad-HFS, HFT, HF-irregular, or native composer decisions.
Native STPD import is deliberately disabled in this version because the current
native product has not passed its Gate-B authority stop gate.

## Only public entry point

The exact public function is:

```r
stpd_import_provider_batch <- function(dataset, request, artifact = NULL)
```

There is no `...` argument and no public single-record importer. `request` is an
unclassed named list whose names and order are exactly:

1. `schema_version` -- exactly `stpd_provider_import_request_v1`;
2. `adapter_key`;
3. `adapter_version` -- exactly `1.0.0` in this registry;
4. `output_role`;
5. `generation_mode`;
6. `selected_train_keys` -- a non-empty, unique, unclassed character vector;
7. `parameters` -- an unclassed named list governed by the selected adapter.

Missing, additional, duplicated, reordered, classed, executable, or
unsupported fields fail closed. A version such as `latest` is never resolved.
The importer derives provider kind, capability, authority, information-access
status, coordinate declaration, artifact hashes, code hashes, run status, and
all IDs. The caller cannot override them outside the versioned artifact. For an
external import, the adapter reads its explicitly declared provider-code hash
and information-access status from the exact hashed raw artifact.

The public request never accepts `coordinate_specs`, a coordinate formula,
`source_index_base`, interval closure, time unit, transformation,
`source_time_resolution_sec`, `train_index`, `.spine_prevalidated`,
`.coordinate_spec_prevalidated`, or any flag asserting prior validation. B1
normalization helpers remain internal and unexported.

`artifact` has three adapter-specific meanings:

- Mean-ISI and LogISI/newBD: exactly `NULL`; the provider runs internally over
  the strict selected B1 timestamp spine;
- external adapters: an unclassed raw vector containing one UTF-8 JSON object;
- Native STPD: reserved for a validated promoted native product, but rejected
  while the native routes below remain disabled.

The importer returns a new provider bundle and never attaches it to, or mutates,
`dataset`.

## Frozen adapter and route registries

The adapter registry has exactly the columns `adapter_key`, `adapter_version`,
`provider_key`, `provider_kind`, and `capability_profile`, and exactly these
rows:

| `adapter_key` | `adapter_version` | `provider_key` | `provider_kind` | `capability_profile` |
|---|---|---|---|---|
| `native_stpd_postcomposer_v1` | `1.0.0` | `native_stpd` | `native_stpd` | `native_composed_v1` |
| `mean_isi_v1` | `1.0.0` | `mean_isi` | `mean_isi` | `burst_interval_threshold_v1` |
| `logisi_newbd_v1` | `1.0.0` | `logisi_newbd` | `logisi_newbd` | `burst_interval_threshold_v1` |
| `external_interval_v1` | `1.0.0` | `external_data_import` | `external_data_import` | `external_interval_v1` |
| `external_per_isi_v1` | `1.0.0` | `external_data_import` | `external_data_import` | `external_per_isi_v1` |

The route registry has exactly the columns `adapter_key`, `output_role`,
`generation_mode`, `enabled`, and `gate_code`:

| adapter | role | mode | enabled | gate code |
|---|---|---|---:|---|
| Native | `automatic_prediction` | `native_only` | false | `native_gate_b_pending` |
| Native | `automatic_prediction` | `support_to_native` | false | `native_gate_b_pending` |
| Mean-ISI | `automatic_prediction` | `external_only` | true | `NA` |
| Mean-ISI | `candidate_support` | `external_only` | true | `NA` |
| Mean-ISI | `calibration_output` | `support_to_native` | false | `calibration_disabled_phase_b2` |
| LogISI/newBD | `automatic_prediction` | `external_only` | true | `NA` |
| LogISI/newBD | `candidate_support` | `external_only` | true | `NA` |
| LogISI/newBD | `calibration_output` | `support_to_native` | false | `calibration_disabled_phase_b2` |
| external interval | `automatic_prediction` | `external_only` | true | `NA` |
| external interval | `candidate_support` | `external_only` | true | `NA` |
| external per-ISI | `automatic_prediction` | `external_only` | true | `NA` |
| external per-ISI | `candidate_support` | `external_only` | true | `NA` |

These routes are a strict operational subset of the Phase-A allowlist. A route
being present in Phase A does not enable it in B2. There is no generic adapter,
fallback adapter, runtime registration, or mutable plugin registry.

## Internal Mean-ISI and LogISI/newBD adapters

Both providers receive only `spine$timestamps[[train_key]]` from a strictly
validated B1 spine. They run once per selected train through the train-level
functions `stpd_detect_misi_bursts_article()` and
`stpd_detect_logisi_newBD_pasquale()`. The dataset-level support wrappers are
forbidden because they read native candidate ledgers. Manual labels, reference
labels, review products, native candidates, and other `dataset$results` content
must not affect either provider byte.

Both adapters use the fixed `diff_isi_one_closed_v1` profile. Their only allowed
candidate label is `event / burst`; they cannot emit Long Burst, HFS, Tonic, or
Pause. An unresolved threshold in any selected train rejects the whole run. A
Mean-ISI search reporting `truncated_at_*` also rejects the whole run; incomplete
search is never reported as a complete all-negative prediction.

Mean-ISI `parameters` accepts only the following keys; omitted keys take the
shown defaults:

| key | default | constraint |
|---|---:|---|
| `min_valid_isi_sec` | `0.001` | finite and greater than zero |
| `min_isi_count` | `2L` | integer, 2--1,000,000 |
| `max_isi_count` | `NULL` | `NULL` or integer from `min_isi_count` to 1,000,000 |
| `max_windows` | `250000L` | integer, 1--250,000 |
| `min_spikes` | `3L` | integer, 3--1,000,000 |
| `min_duration_sec` | `0` | finite and non-negative |

`collapse_exact_duplicates` is fixed to `FALSE` and is not a request field.

LogISI/newBD `parameters` accepts only:

| key | default | constraint |
|---|---:|---|
| `min_valid_isi_sec` | `0.001` | finite and greater than zero |
| `min_num_spikes` | `5L` | integer, 3--1,000,000 |
| `core_reference_sec` | `0.100` | finite and greater than zero |
| `max_reasonable_threshold_sec` | `1.0` | finite and greater than zero |
| `fallback_ch` | `TRUE` | one logical scalar |
| `fallback_maxISI_sec` | `0.100` | finite and greater than zero |
| `multiple_core_mode` | `split_by_core` | `split_by_core` or `join_loose_window` |
| `bin_width_log10` | `0.05` | finite and greater than zero |
| `lowess_span` | `0.20` | finite in (0,1] |
| `min_peak_distance` | `3L` | integer, 1--1,000,000 |
| `intraburst_peak_window_ms` | `100` | finite and greater than zero |
| `void_threshold` | `0.7` | finite in [0,1] |
| `valley_selection` | `max_void` | `max_void` or `first_eligible` |

The adapter passes only these explicit arguments; the provider's internal `...`
is not exposed at the B2 boundary. Canonicalized effective parameters, including
all defaults, determine `params_sha256`.

Each Mean-ISI or LogISI/newBD threshold row has a train-scoped
`scope_sha256` under domain `stpd-provider-threshold-scope-train-v1`. Its
exact ordered payload is `{dataset_snapshot_sha256, train_key,
train_timestamp_sha256}`. Thus two otherwise identical timestamp vectors with
different canonical train keys remain distinct threshold scopes.

## External raw JSON artifacts

External artifacts are exact raw bytes, not R objects or file paths. They must
be non-empty UTF-8 JSON without NUL bytes. Object fields use the exact order
below; unknown, missing, duplicated, or reordered fields fail closed.
`provider_code_sha256` is a required lowercase 64-hex external declaration; B2
records it as declared evidence and does not claim that STPD verified the
external source code. Before it enters `provider_runs`, the declaration is
domain-wrapped with
`stpd-external-unverified-provider-code-declaration-v1`; it is never promoted
to a verified internal-code fingerprint. `provider_version` is non-empty and
cannot be `latest`. Although the field remains explicit in both artifact
schemas, `information_access` is exactly `unknown` in external v1: an imported
artifact cannot self-certify itself as label-blind or manual-aware. For both
external adapters, request `parameters` is exactly an empty plain list.
The importer nevertheless derives, rather than accepts, the effective parameter
object used for `params_sha256`. Its exact ordered fields are
`{artifact_schema_version, coordinate_profile_id, semantic_track,
target_label}`. For the interval adapter the final two fields are JSON/R `null`;
for the per-ISI adapter they contain the artifact-wide channel values. These
derived fields bind schema, coordinate profile, and channel semantics to run
identity without making them caller-supplied request parameters.

### Interval artifact

Top-level fields, in order, are:

```text
schema_version, provider_version, provider_code_sha256,
information_access, coordinate_profile_id, records
```

`schema_version` is exactly `stpd_external_interval_artifact_v1`. Each element
of `records` has exactly these fields in order:

```text
source_record_key, train_key, source_start, source_end,
semantic_track, proposed_label, provider_decision,
score_name, score_value, score_direction,
uncertainty_kind, uncertainty_lower, uncertainty_upper,
evidence_manifest_sha256
```

JSON `null` is used for an absent score, uncertainty bound, or evidence hash.
Index profiles require integer `source_start` and `source_end`; time profiles
require finite numeric values. All positive, negative, and indeterminate
interval assertions are normalized and materialized as provider candidates.

### Binary per-ISI artifact

Top-level fields, in order, are:

```text
schema_version, provider_version, provider_code_sha256,
information_access, coordinate_profile_id, semantic_track, target_label,
records
```

`schema_version` is exactly `stpd_external_per_isi_artifact_v1`.
`semantic_track` and `target_label` define one channel for the entire artifact.
Every record has exactly these fields in order:

```text
source_record_key, train_key, diff_index, provider_decision,
score_name, score_value, score_direction,
uncertainty_kind, uncertainty_lower, uncertainty_upper,
evidence_manifest_sha256
```

`provider_decision` is exactly `positive` or `negative`; `indeterminate` is
forbidden in this binary schema. The run must contain exactly one closed record
for each of the `n-1` diff positions of every selected train, with one base and
transformation throughout. Missing, duplicate, extra, or mixed-position input
rejects the whole batch. All positive and negative positions are retained in
`candidate_intervals` as the complete binary provider assertion channel; every
position also has its own normalization audit.

## Fixed coordinate profiles

The external artifact supplies only one allowlisted profile ID. It cannot
supply a coordinate formula. The adapter expands the ID to the frozen B1 tuple:

| profile ID | convention | base | unit | transformation |
|---|---|---|---|---|
| `train_row_isi_one_closed_v1` | train-row ISI | one | n/a | identity train-row one-based |
| `diff_isi_one_closed_v1` | diff ISI | one | n/a | diff one-based plus one |
| `diff_isi_zero_closed_v1` | diff ISI | zero | n/a | diff zero-based plus two |
| `spike_span_one_closed_v1` | spike span | one | n/a | spike one-based span to ISI |
| `spike_span_zero_closed_v1` | spike span | zero | n/a | spike zero-based span to ISI |
| `spike_time_s_closed_v1` | spike-time span | n/a | s | exact spike-time span to ISI |
| `spike_time_ms_closed_v1` | spike-time span | n/a | ms | exact spike-time span to ISI |
| `spike_time_us_closed_v1` | spike-time span | n/a | us | exact spike-time span to ISI |
| `per_isi_one_closed_v1` | per-ISI diff | one | n/a | per-ISI one-based plus one |
| `per_isi_zero_closed_v1` | per-ISI diff | zero | n/a | per-ISI zero-based plus two |

Every profile is closed. Interval artifacts cannot use a per-ISI profile and
per-ISI artifacts cannot use an interval profile. Time profiles pass
`source_time_resolution_sec=0`; after unit conversion, each endpoint must match
one unique B1 spine timestamp under the frozen conversion envelope. There is no
nearest-spike snapping.

## Authority, record kind, and label materialization

For complete runs the mapping is fixed:

| `output_role` | `authority_scope` | `record_kind` |
|---|---|---|
| `automatic_prediction` | `automatic_prediction_record` | `automatic_assertion` |
| `candidate_support` | `support_evidence_only` | `support` |
| `calibration_output` | `calibration_parameter_only` | no candidate rows |

A rejected run has `authority_scope=none_candidate` and no candidate or threshold
rows. Mean-ISI and LogISI/newBD always set `information_access=label_blind`
because their only input is the B1 timestamp spine. External v1 requires
`information_access=unknown`; it is never upgraded to label-blind based on an
artifact producer's declaration.

External labels must obey the Phase-A Event/State/Gap ontology. An external
positive `hft` or `hf_irregular` row requires exactly one positive `broad_hfs`
parent with the same provider run, train, timestamp hash, ISI bounds, spike
bounds, and time bounds. If an exact explicit parent is absent, the adapter adds
one implied parent which reuses the subtype source record, source hash,
normalization audit, geometry, and record kind and has no invented score or
uncertainty. The paired rows sharing source, audit, and geometry make that
derivation deterministic; the raw artifact is never changed. An exact explicit
parent is reused rather than duplicated. HFT and HF-irregular cannot both be
positive for one parent geometry.

Identical geometry from different providers remains separate. It is never
deduplicated, fused, voted, ranked, or used to overwrite another provider.

## Atomic rejection and two-pass run identity

The importer is a transaction over the complete selected spine:

1. validate request, route, dependency manifest, artifact bytes, parameters,
   selected spine, and provider output;
2. derive the input, parameter, raw-output, provider-code, and adapter-code
   hashes and calculate a dedicated preflight ID with domain
   `stpd-provider-import-preflight-v1` over adapter key/version, role, mode,
   dataset snapshot, input-artifact hash, and parameter hash;
3. normalize every record under that preflight ID and evaluate complete
   coverage, provider threshold resolution, capability, subtype-parent, and
   materialization invariants;
4. derive final status (`complete` only if every invariant passed, otherwise
   `rejected`), calculate the status-qualified Phase-A `provider_run_id`, and
   rerun all B1 normalization under that final ID;
5. materialize and validate the five-table provider bundle only after the final
   pass.

If a rejected run can be identified deterministically, it contains one rejected
`provider_runs` row, every available final-ID-bound audit row, and zero
candidates and zero thresholds. The audit table may be empty when an internal
provider cannot resolve a threshold before producing any source record. A
systemic error occurring before a trustworthy run identity exists raises a
typed error and returns no object. No path returns a mixture of complete and
rejected records, and no failure publishes a partial bundle.

The same run ID with different raw or normalized bytes is
`provider_nondeterministic_output` and fails closed.

## Artifact, resource, and executable-code limits

The B2 limits are:

- at most 1,000 selected trains;
- at most 2,000,000 total selected timestamps;
- at most 250,000 source records;
- at most 250,000 materialized candidate rows after adding required implied
  Broad-HFS parents;
- at most 64 MiB (`64 * 1024 * 1024` bytes) per external raw JSON artifact;
- at most 250,000 Mean-ISI windows per train and 1,000,000 windows over the
  complete selected run;
- at most 128 MiB (`128 * 1024 * 1024` bytes by R `object.size`) for a parsed
  external artifact or an in-memory internal provider raw-output object.

All B1 limits also apply; where limits differ, the stricter limit wins. Raw byte
length is checked before JSON parsing. Parsed expansion, record count, field
count, scalar-byte, timestamp, and output-table limits are checked before
materialization. Resource exhaustion rejects rather than silently truncating or
returning a seemingly complete prediction.

The frozen scaling regression exercises the public importer with one complete
10,000-position per-ISI channel and requires 10,000 normalization-audit rows,
10,000 provider-candidate rows, an unchanged input dataset, a valid final
bundle, and elapsed time below 180 seconds. A second regression materializes
50,000 positive HFT rows plus their 50,000 required Broad-HFS parents and also
requires completion below 180 seconds with unique candidate IDs. These are
acceptance bounds, not biological-performance claims.

The boundary accepts no RDS, RData, archive, connection, expression, formula,
function, environment, external pointer, S3/S4 object, classed list, list column,
or imported executable code. It performs no download and loads no provider code,
package, script, plugin, or shared library named by an artifact.

## Dependency manifest and hash DAG

The installed dependency manifest schema is exactly
`stpd_provider_adapter_dependency_manifest_v1`. Its ordered root fields are:

```text
schema_version, provider_contract_sha256,
provider_adapter_protocol_sha256, adapters
```

Each adapter entry has exactly:

```text
adapter_key, adapter_version, adapter_code_sha256,
provider_code_sha256, dependency_files
```

`dependency_files` is a non-empty JSON array. Each element has exactly the
ordered fields `{path, sha256}`; paths are unique repository-relative UTF-8
paths and the array is UTF-8-path sorted. Absolute paths, `..`, symlinks,
runtime-loaded files, mtimes, host names, and session state are excluded. Each
leaf is SHA-256 after deterministic CRLF-to-LF conversion, with no other
encoding or whitespace normalization. The manifest generator and source-tree
acceptance test recompute every leaf. Runtime reconstructs the sorted dependency
closure and domain-hashes it with `stpd-provider-adapter-code-v1`; a supplied
derived adapter hash is never trusted by itself.

The acyclic identity graph is:

```text
Phase-A contract hash ----\
B1 protocol hash ----------+--> full sorted dependency_files
adapter/normalizer files --/                 |
                                             +--> adapter_code_sha256

provider algorithm-file subset ----------------> provider_code_sha256

adapter_code_sha256 + provider_code_sha256 + dataset/spine/input/parameters
    + role/mode/status + Phase-A contract hash --> provider_run_id

provider_run_id + final audits/candidates/thresholds --> normalized_output_sha256
```

Mean-ISI and LogISI provider algorithm files are deliberately members of both
closures: the full adapter dependency closure and the smaller provider-code
subset. Internal hash domains are `stpd-provider-adapter-code-v1`,
`stpd-provider-code-v1`,
`stpd-provider-parameters-v1`, and `stpd-provider-input-artifact-v1`.
External `raw_output_sha256` is SHA-256 of the exact supplied raw JSON bytes;
internal provider raw output uses frozen canonical JSON. The manifest file,
golden hashes, tests, and this document are not included in their own dependency
preimage, preventing a self-reference cycle. Runtime hashes must match the
installed manifest; a mismatch is fail closed.

The exact B2 payloads are:

- adapter code: `{provider_contract_sha256,
  provider_adapter_protocol_sha256, adapter_key, adapter_version,
  dependency_files}` under `stpd-provider-adapter-code-v1`;
- parameters: the adapter's fully default-expanded canonical parameter object
  under `stpd-provider-parameters-v1`;
- internal input artifact: `{adapter_key, adapter_version, output_role,
  generation_mode, dataset_snapshot_sha256, timestamp_spine_sha256,
  train_manifest, parameters}` under `stpd-provider-input-artifact-v1`;
- external input artifact and raw output: SHA-256 of the same exact raw bytes;
- external provider code: `{provider_version,
  declared_provider_code_sha256}` under
  `stpd-external-unverified-provider-code-declaration-v1`.

The resulting provider and adapter code hashes then enter the unchanged
Phase-A provider-run payload. Neither `raw_output_sha256` nor
`normalized_output_sha256` is retroactively inserted into the run-ID preimage;
the existing nondeterminism check binds those outputs to the stable run ID.

## Support-to-native boundary

The Mean-ISI and LogISI calibration routes and both Native routes are disabled
in this B2 registry. When a later version enables support-to-native, it must be
two distinct provider runs joined by calibration lineage. The only information
passed from the support provider to Native STPD is the complete frozen effective
parameter bundle and its hash. Candidate bounds, event geometry, scores, labels,
and provider decisions are never passed into Native detection. The native
consumer runs the native detector/composer exactly once on its own B1 spine.
Manual-example parameters remain manual-aware and propagate that taint to the
consumer; they cannot be reported as label-blind.

## Native stop gate and current non-goals

`native_stpd_postcomposer_v1` remains disabled with
`native_gate_b_pending`. Enabling it requires a new registry/hash freeze and all
of the following evidence:

- a signed, parent-bound native automatic product with
  `authoritative=TRUE`, `authority_scope=automatic_prediction_record`, and
  `promotion_status=passed`;
- exact selected-train and timestamp-spine identity;
- rejection of pending Preview, reviewed/final products, legacy
  `pattern_auto`, candidate ledgers, and descriptive Regime products;
- read-only projection with zero composer or detector calls and no mutation of
  either dataset or native product bytes;
- an unchanged fresh 30-train native scientific payload SHA-256 of
  `9af37b989edc848c9d7a5c9eedc160ae080eed131b53c9846010c402fd23a545`.

Until that gate passes, B2 infrastructure and the enabled Mean-ISI, LogISI, and
external adapters may be tested and released without claiming Native adapter or
full Gate-B completion.

Current non-goals are dynamic plugins, additional external formats, imported
code, automatic provider fusion or voting, cross-provider score comparison,
adjudication UI, recurrent-regime composition, sealed-reference access,
performance claims, and enabling any disabled calibration or Native route.
