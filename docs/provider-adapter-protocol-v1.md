# STPD provider adapter protocol v1

## Purpose

This Phase-B1 companion protocol binds actual spike timestamps to the frozen
provider-bundle v1 declarations. It does not change the Phase-A five-table schema
or its contract hash. It is an internal adapter boundary; the supported public
entry point is the unified provider importer introduced in Phase B2. The normalizer is
not a second detector and never assigns a biological label.

STPD is therefore organized as a multi-algorithm and human-in-the-loop workbench:
native STPD, Mean-ISI, LogISI/newBD, and allowlisted pure-data results can retain
their own AUTO output, can calibrate a separate native run, and can later be
reviewed without overwriting either the provider result or sealed reference data.

The architecture has two independent freezes. Provider-bundle v1 remains the
Phase-A schema/authority freeze with contract SHA-256
`29a2a660ee594494c6687433db1448825bf5ac5f518f36344bdf81f65ee01a6b`.
This document and `stpd_provider_adapter_protocol_v1()` are the Phase-B1
timestamp/normalization freeze. Changing B1 must change its companion protocol
hash; it must not silently change the Phase-A contract hash.

## Hash primitive and frozen identities

Except for a timestamp leaf, every identity in this protocol is

`SHA256(UTF8(domain) || 0x00 || UTF8(canonical_json(payload)))`.

The canonical JSON rules are inherited verbatim from provider-bundle v1: field
and array order are preserved, encoding is UTF-8, output is compact,
`auto_unbox=TRUE`, `NULL` and typed `NA` serialize as JSON `null`, floating-point
digits are not rounded (`digits=NA`), and factors are not accepted at the B1
pure-data boundary. The companion protocol itself uses domain
`stpd-provider-adapter-protocol-v1`. Its frozen hash is
`5b0cc6ca2fe6bdf2bb7bd2bddb066632ea3b0646b1096be87c77bec292a26877`.

The absent acquisition-context sentinel is domain
`stpd-provider-acquisition-context-none-v1` with payload
`{"status":"not_supplied"}` and hash
`7d8bf6bf4dac8e6ba54c19ba576dbd21d3a54b82b9c22117d6055f6540439051`.
The corresponding QC-policy domain is `stpd-provider-qc-policy-none-v1`, with
hash `babd38b1109e862fc3a986e1b473cd3c0f395a19e01b4a785773aa5639d23bbe`.
Supplied acquisition/QC hashes are bound verbatim rather than rewrapped; the
upstream producer is responsible for validating their content contract.

## Timestamp identity

For each train, timestamps are read from `timestamp_sec` in original spike-row
order. They must be base numeric, finite IEEE-754 binary64 seconds, must not contain
negative zero, and must be strictly increasing and unique. The implementation must
not sort, deduplicate, drop, round, or otherwise repair them.

`train_timestamp_sha256` is SHA-256 over the exact binary64 values written
little-endian with eight bytes per value and no R serialization or header. This is
the same leaf encoding already used by the Gate-B label-blind input manifest.

`timestamp_spine_sha256` uses domain `stpd-dataset-timestamp-spine-v1` over
radix-sorted rows containing protocol version, coordinate version, exact NFC UTF-8
train key, spike count, seconds unit, and train leaf hash. `dataset_snapshot_sha256`
separately binds the protocol, timestamp spine, acquisition-context hash, and QC
policy hash. Manual/reference labels, detector/review output, `created_at`, and
mutable train settings are excluded.

The exact leaf goldens for `double()`, `[0]`, and `[0,.1,.2,.3]` are respectively
`e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855`,
`af5570f5a1810b7af78caf4bc70a660f0df51e42baf91d4de5b2328de0e83dfc`,
and `1f8ca26fda2a620f4272dfe8264145b694be8cac4a09125b5fccc32450cbccd1`.
For the one-train `t1/[0,.1,.2,.3]` fixture, the spine hash is
`34f09f001cb4848ad14623f73ba01dd403dee599f2d9b4cf0c3ddeaba63f44e5`
and the default-context snapshot is
`dceced8315cdaaf5b9bed7450c49ab5ada83d0d737de2331e4c8767ea7c3f994`.

At least one train must be selected for a run. A selected zero- or one-spike
train has a valid spine identity but contains no normalizable ISI interval.

## Canonical coordinates

Canonical row `r` (`2 <= r <= n`) is the ISI between spike rows `r-1` and `r`.
Canonical interval `[s,e]` includes ISI rows `s:e`, spike rows `(s-1):e`, and times
`timestamp_sec[s-1]` through `timestamp_sec[e]`.

The mappings are frozen by provider-bundle v1:

| source | canonical ISI rows |
|---|---|
| one-based train-row ISI `[a,b]` | `[a,b]` |
| one-based `diff(timestamp)[a:b]` | `[a+1,b+1]` |
| zero-based diff/per-ISI `[a,b]` | `[a+2,b+2]` |
| one-based spike span `[p,q]` | `[p+1,q]` |
| zero-based spike span `[p,q]` | `[p+2,q+1]` |

Integer closure is resolved before mapping: closed leaves both bounds unchanged;
left-closed/right-open subtracts one from the end; left-open/right-closed adds one
to the start; open applies both adjustments. Empty support is rejected. A single
closed diff/per-ISI element is valid; a single spike contains no ISI and is rejected.
Biological minimum-spike rules belong to the detector/provider, not normalization.

For `per_isi_diff_index`, Phase B1 accepts only one closed singleton source row
per diff position. A batch-level coverage manifest is mandatory, one index base and
transformation must be used throughout the run, and every covered train must have
exactly `n-1` unique positions. Missing, duplicate, or extra positions reject the
batch. A later adapter may RLE adjacent positive positions only after preserving
all constituent source-row hashes in evidence lineage.

## Time spans

Only a closed `spike_time_span` is accepted. Units are explicit `s`, `ms`, or `us`.
Each converted boundary must match exactly one real spine timestamp inside the
derived tolerance set; zero matches is `time_alignment_missing`, more than one is
`time_alignment_ambiguous`. The implementation never chooses the nearest spike.

An exact seconds source has tolerance exactly zero and therefore requires a
bit-exact boundary. For `ms` or `us`, the unit-conversion envelope is
`min(1e-12, 8 * .Machine$double.eps * max(1, abs(start_sec), abs(end_sec)))`.
The 1-ps ceiling prevents tolerance from expanding with a very large absolute
time origin; a lower-precision artifact must declare its real serialization
resolution. Final tolerance is
`max(conversion_envelope, source_time_resolution_sec / 2)`. It must remain below
half the local adjacent-spike gap at both matched boundaries. Resolution is one
batch-level input and cannot be tuned record by record.

## Transaction and audit semantics

Systemic schema, identity, source-key, spine, executable-input, or resource errors
raise a typed error and return no partial object. Parseable record-local coordinate
errors produce a deterministic rejected audit row with all canonical fields `NA`.
Every record is audited.

Every batch, including an empty interval result, must carry an explicit
processed-train coverage manifest. It must equal the complete selected timestamp
spine, not merely be a subset. A provider that processes a subset must first build
a new subset spine and therefore obtains a different snapshot identity. This
binds all-negative trains to run identity. An empty record list without coverage
is a systemic error. Per-ISI coverage must equal the same processed-train manifest.

`coverage_sha256` uses domain `stpd-provider-coverage-manifest-v1` and the ordered
payload `{input_mode,rows}`. `input_mode` is `interval` or `per_isi`; rows are
UTF-8-bytewise train-sorted objects with exact field order
`{train_key,n_spikes,train_timestamp_sha256}`. The standard interval fixture for
`t1/[0,.1,.2,.3]` has coverage hash
`e05615a412fccfefd26cc38d7d9fb5b356260b72a88b0e512729f4fb404e0daa`.

`atomic_accept` is true only when every record normalized. If any record is
rejected, normalized geometry is empty even for individually successful rows.
Phase B2 must materialize the provider run as rejected and must publish no
candidate or threshold rows. Recovering valid records requires a new filtered raw
artifact and a new provider run. This prevents incomplete input from masquerading
as a valid all-negative result.

Raw artifact bytes and their SHA-256 remain outside this normalizer. Each source
record additionally receives a deterministic hash under domain
`stpd-provider-source-record-v1` over the original ordered
typed fields. The payload is `{"fields":[...]}` and every field has ordered keys
`name,type,is_na,value`; allowed types are `null`, `logical`, `integer`, `double`,
and `character`. A real `NULL`, a typed `NA`, and an empty string are distinct.
Normalized output is sorted only after raw record identity/order has been
preserved by the artifact.

The standard source record `x=1L,y=1.0,z="burst"`, `x=NULL`, and
`x=NA_integer_` have frozen hashes
`86b263d9afdda2eb5b0a11e8d17cffd823955b921059d4c0e0fa8773ca5292c1`,
`496661ba1cb2dee3c287017a59f77b7f4da8fe9a2e86e1bb9f087c349313a9fc`,
and `7dda0b9ab80b028c397be20a683c381f10abb1c74f24cc6dd6653ea91a73c33d`.
The standard successful, rejected, and microsecond-conversion normalization-audit
IDs are respectively
`0c8173d0c0b9639f1eb50e1c967b055cf9929745cb1f51e8c441693b24d5620f`,
`58b2bfd0e75b36fbe6cce39316e5444a75557de88c45637100d714fe66b0fb8f`,
and `d7419541570d91b79fca36df9049b74c74b8acf93fbd1f305cd30e2f4b99602c`.
`error_detail` is diagnostic and excluded from that identity.

The raw source-record hash intentionally does not contain the adapter's coordinate
declaration. That declaration, transformation, resulting geometry, tolerance, and
raw source hash are jointly bound by `normalization_audit_id` under domain
`stpd-normalization-audit-v1`. The provider run separately binds
`adapter_code_sha256`. Consequently, B2 must derive the source record and its
coordinate spec in the same allowlisted pure function; the supported importer may
not accept a free caller-supplied spec for an unrelated raw record.

Human modification is downstream of this boundary. External/Mean-ISI/LogISI
AUTO output remains immutable; accepting, rejecting, or adjusting it creates a
separate adjudicated product. A support-to-native workflow is two runs: a provider
calibration run creates frozen parameters, followed by a separately identified
native STPD run. Only the frozen parameter bundle crosses that boundary. Provider
candidate geometry is never sent through, merged into, or composed by the native
STPD detector/composer, including in support-to-native mode.

## Resource limits

- at most 100,000 trains;
- at most 10,000,000 timestamps;
- at most 1,000,000 source records;
- at most 1,024 scalar fields in one source record;
- at most 5,000,000 source fields in one batch;
- at most 65,536 UTF-8 bytes per scalar character field.

These are hard ceilings. B2 adapters may impose lower limits but cannot raise them.
Imported executable code, environments, language objects, list columns, RDS/RData
plugins, and implicit provider discovery are forbidden.
