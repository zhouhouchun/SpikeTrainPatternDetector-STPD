# Scientific Data Import Contract

## Status and scope

This document is the approved product and scientific contract for the native
macOS import path. It describes the target behavior. A rule is not considered
implemented until its code and acceptance tests are present; the current CSV
reader remains a legacy path while the canonical path is built in parallel.

The supported scientific input is timestamp data. The importer must not claim
to identify waveform artifacts, amplifier saturation, acquisition dropout, or
stimulus blanking when those signals are not present in the source data.

This work does not change the R implementation, detector thresholds,
candidate generation, labels, arbitration, or result metrics. The R code is a
scientific reference, not a specification to copy mechanically.

## Data sources and bounded ingestion

- Accept CSV and XLSX tables. The import UI must say which formats are
  supported and must explain the expected column structure.
- A source file is expected to be about 5 MB or smaller. Before activation, the
  exact byte cap and the separate XLSX limits for decompressed XML, cells,
  shared strings, rows, columns, and attributes must be documented and tested.
- Never truncate rows, cells, strings, or attributes silently. A limit breach
  is a source-identified blocking error, with sheet/cell/row location whenever
  that location exists.
- CSV and XLSX inputs with the same confirmed scientific meaning must produce
  the same canonical dataset identity.
- CSV uses deterministic RFC 4180 quoting, including commas and embedded
  newlines inside quoted fields. Those fields must have the same meaning as the
  equivalent XLSX cells.
- An XLSX import requires the user to select the worksheet explicitly. The app
  must not silently choose the active or first sheet.
- XLSX numeric timestamp cells are allowed only when their raw workbook value
  uniquely converts to one signed microsecond tick. Values that would require
  rounding, formula cells, error cells, Boolean cells, date-formatted cells,
  non-finite values, and merged cells in the data region are blocking errors.
  If adjacent microsecond ticks cannot be distinguished at the numeric cell's
  stored precision and magnitude, the cell is rejected.
- A truly blank timestamp cell means that no timestamp occurs in that row and
  column. Text markers such as `NA`, `null`, `NaN`, and `Inf` are invalid
  timestamp tokens, not blanks.

## Dataset activity mode

Every imported file or import batch has exactly one explicitly confirmed mode:

1. `putative_single_unit` — timestamp evidence treated as one putative neuron
   per spike-train column;
2. `intentional_multi_unit` — deliberately pooled multi-unit timestamps;
3. `unknown_or_uncertain` — provenance is insufficient to choose either mode.

Columns in one file cannot silently use different modes. If mixed material is
encountered, it must be split into separate import batches.

Only a valid, confirmed putative-single-unit dataset may receive the current
detector's authoritative biological interpretation. Multi-unit and unknown
modes are placeholders that support timestamp inspection, raster display, and
timestamp-integrity QC only. They must not inherit single-unit refractory-
period assumptions or thresholds. Every biological detector surface reports
**not evaluated** together with the reason; it must not report zero, absence,
or a negative biological finding.

Because the input has no waveforms, “putative single unit” is a user-confirmed
data provenance statement, not a spike-sorting proof made by the app.

## Exact timestamp representation

The import UI requires an explicit source unit: seconds (`s`) or milliseconds
(`ms`). There is no silent default and no unit inference from decimal places.

Canonical time is a signed 64-bit integer count of microseconds. CSV/text
parsing and unit conversion are exact and checked; they must not pass through
binary floating point, apply an epsilon, or round to the nearest tick. XLSX
numeric cells are stored by the format as binary64. Their decoder first gives
priority to an exact canonical whole-microsecond projection. Only when no exact
projection exists may it recognize an Excel serialization residue that is
exactly one binary64 ULP from exactly one whole-microsecond projection. If an
adjacent tick also qualifies, the residue exceeds one ULP, the value lies
outside the signed range, or it is otherwise off the microsecond grid, import
is blocked. This bounded inverse is not general nearest-tick rounding; the
source snapshot binding continues to attest the unchanged workbook bytes.

Timestamp text accepts an ASCII decimal mantissa with an optional sign,
fraction, and base-10 scientific exponent. Extra trailing zeros are accepted
only when the scaled value is still an exact whole number of microseconds.
Locale-specific separators, embedded whitespace, unit suffixes, and non-ASCII
digits or signs are invalid. Displayed decimal places are a formatting rule,
not evidence that the source value was precise.

Canonical display and export use:

- seconds: exactly 6 fractional digits, for example `12.500001`;
- milliseconds: exactly 3 fractional digits, for example `12500.001`.

The display unit does not change scientific identity. Signed zero is
canonicalized to zero.

## Group-local time basis and negative timestamps

Every event-scope group explicitly confirms one time basis:

- `recording_elapsed` — all spike and event ticks are non-negative elapsed
  recording time;
- `event_relative` — signed source ticks are interpreted relative to one
  selected event occurrence in the same group.

Negative source timestamps are permitted only for event-relative data within a
confirmed event-scope group. The user must select one unambiguous event
occurrence in that same group as the origin. Rebasing is exact checked integer
arithmetic:

```text
canonical_tick = source_tick - selected_origin_tick
```

Overflow, a missing origin, an origin in another group, or an occurrence that
cannot be uniquely identified is a blocking error. The source values and
selected offset remain provenance. Rebasing applies to every spike tick and
every event tick in that group. Different groups may confirm different origins.

Without an explicit compatible clock transform, groups with different time
bases or origins cannot be combined for absolute-time overlays, synchrony
analysis, population binning, timestamp concatenation, or cross-group ISIs.

## Event-scope groups

A file is partitioned into explicit contiguous groups. Each group contains one
or more spike-train columns followed by zero or more event columns (`S+ E*`).
An event applies only to the spike trains in its own group.

Example:

```text
unit_A | unit_B | event_stimulus | event_reward | unit_C | unit_D | event_light
|---------------- group alpha -----------------| |-------- group beta --------|
```

Here, `event_stimulus` and `event_reward` can describe `unit_A` and `unit_B`;
`event_light` can describe `unit_C` and `unit_D`. A row is not a trial and does
not create cross-column event pairing. One group may contain multiple event
columns and each event column may contain multiple occurrences.

Header names and column patterns may generate a staging suggestion only. The
confirmed manifest is authoritative. An event column cannot begin a group or
appear without a preceding spike-train column in that group. A headerless file
can be imported only as one all-spike group; event columns require headers and
explicit confirmation.

A manifest draft is transaction-bound to the exact staged source facts the user
reviewed: selected source or worksheet, ordered headers, raw cells, blanks,
multiplicity, and row count. Applying that draft to different source facts is a
blocking error. Suggestions are excluded from this binding, and the binding is
review safety rather than scientific identity.

Every bounded file reader binds staging to the SHA-256 of the exact owned byte
snapshot it parsed. XLSX additionally binds the selected worksheet name,
`sheetID`, relationship ID, and normalized part path. Therefore, a byte-different
file or a different worksheet invalidates an earlier draft even when the visible
table happens to be identical. Such sources may still normalize to equal
scientific data. The byte digest and worksheet coordinates remain provenance and
must not enter canonical scientific identity. Manually constructed, unbound
staging is a compatibility/testing state and is not proof of a reviewed file.

Repeated display headers remain distinct definitions until the user assigns
stable, unambiguous semantic IDs. A confirmed event definition remains part of
scientific identity even when it contains no occurrence; that condition is a
warning, and the empty definition cannot be selected as a time origin.

An event type is a reusable semantic category. An event definition is the
confirmed event column in one group. An event occurrence is one timestamp plus
its attached attributes. None of these constructs a temporal scope: event
definitions and occurrences never create a RecordingSegment or a Trial. The
manifest separately and explicitly confirms exactly one dataset-global
RecordingSegment, so that segment is already represented. This slice still
creates no Trial entities: Trial is explicitly not applicable for
`continuous_untrialed`, while `trialized` and `unknown_or_uncertain` remain
blocked by the missing Trial contract. Temporal scopes are never inferred from
rows, events, source order, or group membership. The projection remains
shadow-only and cannot activate detection or authoritative export.

## Event occurrences and structured attributes

Within an event column, a numeric timestamp starts an occurrence. Zero or more
immediately following, contiguous text cells of the form `@key=value` attach to
that occurrence. The next timestamp starts another occurrence; a blank cell
ends the attribute block.

Example:

```text
event_stimulus
12.500000
@label=light_on
@condition=drug
@intensity=5
@intensity.unit=mW
15.000000
@label=light_off
```

Parsing rules:

- split an attribute at the first `=`;
- ASCII `=` is the unescaped separator and is not allowed inside an attribute
  key; keys remain case-sensitive Unicode NFC and are not silently trimmed;
- an attribute before any timestamp, or after a blank, is an orphan and blocks
  import;
- a scalar key may appear only once in one occurrence;
- attributes never spill into another column or group;
- spike-train columns contain timestamps or blanks only;
- the `.unit` suffix is reserved: `@x.unit=...` declares a staging-time unit
  suggestion for attribute `x`; a declaration without `@x=...` in the same
  occurrence is an error.

The same occurrence and attribute grammar applies to CSV and XLSX. CSV quoting
must preserve an attribute value containing a comma or newline without turning
it into a new column or occurrence.

The allowed scalar types are string, integer, exact decimal, and Boolean.
Exact decimals must not be represented by binary floating point. Strings are
normalized to Unicode NFC for canonical comparison while the raw text remains
provenance. Canonical Boolean values are `true` and `false`.

## Attribute role, type, unit, and empty values

Every newly discovered key is retained in staging so the format remains
extensible. It begins with unconfirmed type and role. Before import can be
confirmed, the user must explicitly confirm its scalar type and classify it as
either:

- **Scientific** — may distinguish occurrences and enters scientific identity;
- **Presentation** — display-only, excluded from scientific identity, and
  cannot be used to distinguish an event origin.

The UI may apply a role to many checked keys at once. The confirmed manifest
still stores the role separately for every key; the batch gesture is provenance
only. There is no silent role or type default, and an observed value must not
silently cause a previously confirmed key to change type.

An inline declaration such as `@intensity.unit=mW` is only an import
suggestion. The unit confirmed in the manifest is the sole authority. Conflicts
between inline declarations, or between an inline declaration and the
confirmed manifest, block import. No automatic unit conversion is allowed.
An unknown unit can be retained as opaque display metadata, but cannot be used
in scientific computation until an explicit unit mapping exists.

The current detector consumes neither Scientific nor Presentation event
attributes. Any future scientific consumer must explicitly declare the keys,
types, units, missing-value behavior, and identity consequences it accepts.

Missing and explicitly empty are different states. Integer, exact-decimal, and
Boolean attributes cannot be empty. A string accepts `@key=` only if its
definition explicitly enables **Allow Empty String**; the value then means
“present but empty.” It is never converted to missing, zero, false, or unknown.

## Ordering, duplicate timestamps, and timestamp QC

Raw multiplicity is always retained in both canonical raw data and provenance.

- Input order is preserved in provenance. A non-monotonic timestamp column is
  never silently sorted: the user must either approve an audited stable sort or
  cancel and repair the source. Scientific identity uses the resulting final
  canonical sequence, not the operation history.
- Putative single-unit duplicates require an explicit, audited analysis-view
  policy. A requested exact-duplicate collapse is virtual and run-derived: it
  never removes a timestamp from canonical raw data or changes canonical
  dataset identity. The original duplicate count remains visible in audit.
- Multi-unit and unknown inputs preserve multiplicity in every current view.
  Any future merge policy must be explicit and cannot borrow single-unit
  assumptions.
- Event occurrences at the same canonical tick may be distinct only when their
  confirmed Scientific attributes differ. Same tick plus identical Scientific
  attributes is ambiguous multiplicity and cannot serve as a unique origin.
- Presentation attributes never resolve scientific ambiguity.

Short ISIs are described causally neutrally as values below the configured
minimum valid ISI. Timestamp-only input cannot establish why they occurred.
For putative single-unit data they participate in refractory/data-validity QC;
for multi-unit data the corresponding biological rule is deferred.

Timestamp QC reports, without inventing waveform-level causes:

- invalid or non-numeric tokens and explicit missing-value markers;
- source ordering and any explicitly approved stable sort;
- duplicate ticks and their zero ISIs;
- positive adjacent ISIs computed from the final ordered sequence;
- negative ticks lacking a valid group-local event-relative basis;
- positive ISIs below the user-confirmed minimum-valid-ISI floor;
- for putative single-unit data only, refractory/isolation-suspect ISIs as a
  separate, explicitly non-causal finding.

Timestamp-integrity findings, below-minimum-ISI findings, and putative-single-
unit refractory/isolation-suspect findings remain separate categories. None of
them, from timestamps alone, proves an acquisition artifact or spike-sorting
failure.

## Canonical identity and provenance

The first canonical implementation is deliberately a shadow projection. It may
be constructed only from a source-bound prepared import that passes the
independent replay validator. On success, validation constructs a deterministic,
in-memory, shadow-only `CanonicalScientificDatasetFingerprint`: a content digest
of the confirmed scientific dataset under the schema contract
`canonical_microsecond_single_recording_segment_event_scope_dataset`. That fingerprint is **not** an
authority receipt — it does not confirm or activate a dataset, is not a persisted
manifest identity, and grants no detector, review, result-package, or export
authority. This slice still creates no legacy `SpikeDataset` adapter, detector
input, result package, export, active-dataset standing, or source-bound manifest
digest.

Spike-train identity is dataset-global. The canonical dataset owns a single
ordered spike-train registry keyed by `ScientificSpikeTrainID`, and each
EventScopeGroup references its members by that identity rather than redefining
them. Under the current strict partition, every spike train belongs to exactly
one group: the reference union equals the registry and every registry entry is
referenced exactly once. `EventScopeGroup` expresses Unit/Event applicability
only; it is not a Unit namespace. Exactly one dataset-global `RecordingSegment` is
represented; `Trial` entities and numeric segment bounds are not.

Scientific identity is derived from the confirmed semantic manifest and
canonical data, including:

- stable semantic IDs and membership for groups, the global spike-train
  registry, and event definitions;
- the single dataset-global RecordingSegment: its explicitly confirmed semantic
  ID, recording regime, imported-excerpt coverage, and observation-bounds
  availability;
- activity mode and confirmed scientific settings;
- each group's confirmed time basis and selected origin occurrence semantics;
- exact final canonical spike and event ticks and retained canonical
  multiplicity, including every exact duplicate timestamp;
- event occurrences and their association with definitions;
- typed canonical Scientific attribute values and their confirmed units.

The schema contract digest binds the exact byte codec — SHA-256, both domain
strings, the primitive encoding rules, every field tag and token, the traversal
order, the canonical-ordering rules (UTF-8 semantic-ID ordering; occurrence
ordering by tick then Scientific attributes; attribute-key ordering with a
scalar-type tie-break of string < integer < exact_decimal < boolean; string,
integer, and exact-decimal payloads as canonical text and Boolean payloads as one
byte; the not-applicable/dimensionless/specified unit branches; canonical
event-relative origin at tick zero with a unique group-local matching occurrence;
and retained exact duplicate event-occurrence multiplicity), and these structural
facts. Any codec or canonical-ordering change requires a schema-contract update;
fixed schema and byte-transcript goldens enforce that coordination. Digest
ordering follows stable semantic IDs, not source column order. Duplicate display
names require explicit disambiguation before confirmation.

The following remain provenance and do not by themselves change scientific
identity: CSV versus XLSX, source filename and bytes, sheet/cell/row address,
raw lexeme, source time unit, display time unit, UI batch-selection gestures,
Presentation attributes, original source order, source origin offset, and
sort/virtual-collapse/rebase operation history. Raw input and every normalization
decision must remain inspectable. A normalization that changes the final
canonical object changes identity through that object, not through the gesture
or history record itself.

Changing canonical data, activity mode, a Scientific attribute definition or
value, an attribute's role between Scientific and Presentation, a confirmed
scientific unit, or another scientific setting changes identity and invalidates
dependent results. Moving an attribute across the Scientific/Presentation
boundary is therefore an identity-bearing change, because it adds or removes a
Scientific attribute from the canonical dataset. A presentation-only change —
one confined to a Presentation-role definition or its display, never crossing
that boundary — does not change canonical scientific identity, though it may
change a source-bound confirmation-record digest.

An identity-bearing change invalidates stale detector runs, reviews,
authority-bearing annotations, and caches. Builder, writer, reader, and sealed
validator must use one shared canonical encoder; any identity or validation
divergence blocks activation.

Only after canonical identity is established may an explicit compatibility
adapter project integer ticks to the current detector's `Double` seconds. That
derived projection is neither canonical storage nor identity and is subject to
the detector-projection regression gate below.

New capability and schema names must describe function, not release order. Do
not encode an ordinal release or project stage in a new identifier. Suitable
names include `canonical_dataset_digest`,
`canonical_scientific_settings_digest`, and
`canonical_microsecond_event_scope_result`. Historical ordinal identifiers may
be recognized only inside a read-only compatibility adapter.

## Recording segment (single dataset-global temporal scope)

One import batch represents exactly one dataset-global `RecordingSegment`. All spike trains and
EventScopeGroups belong to that one segment; the canonical dataset carries a single non-optional
`ConfirmedRecordingSegment` and no redundant membership array. Shared segment membership does **not**
override group-local time-basis/origin incompatibility — without an explicit clock transform,
cross-group ISIs, synchronization, population binning, timestamp concatenation, and absolute-time
overlays remain prohibited.

The segment carries four explicitly user-confirmed fields, all unresolved until confirmed (never
silently defaulted):

- an explicit **segment semantic ID** (enters scientific identity; a suggestion is never silently
  accepted);
- **recording regime**: `continuous_untrialed`, `trialized`, or `unknown_or_uncertain`. A contiguous
  excerpt of a continuous recording is `continuous_untrialed`; concatenated or explicitly
  trial-bounded material is never silently called continuous;
- **imported-excerpt coverage**: `all_spike_trains_full_imported_excerpt` (every included spike-train
  stream was continuously observable/valid throughout the same imported excerpt — this does not
  require any train to fire at the edges, does not require a nonempty train, and does not prove
  single-unit isolation), `not_all_spike_trains_full_imported_excerpt`, or `unknown_or_uncertain`;
- **observation-bounds availability**: `unknown_or_unavailable` (the only supported value in this
  slice, but still explicitly confirmed).

This slice creates no Trial entities and no numeric segment bounds. Recording bounds are never
inferred from first/last spikes, min/max timestamps, rows, empty cells, ordinary event occurrences,
EventScopeGroups, or source order. Repeated stimulus/reward/event occurrences never create a Trial.
Because exact acquisition start/end are unknown, first/last-spike exterior time stays censored: the
app produces no authoritative recording-wide firing rate, state occupancy, leading/trailing-edge
Pause, complete recording-relative episode duration, or claim that a state reached the acquisition
boundary. This censors complete recording-relative or boundary-censored episode-duration claims, not
a candidate episode's internally supported duration measured wholly within the imported data.
Unavailable bounds are never represented as zero or as the timestamp extrema. Existing
negative-timestamp rules are unchanged: `recording_elapsed` remains non-negative, `event_relative`
still requires the explicitly selected same-group origin, and the segment grants no new negative-time
permission. The four fields are scientific identity and enter the canonical byte codec under the
schema contract `canonical_microsecond_single_recording_segment_event_scope_dataset`.

## Confirmation record and analysis readiness

An explicit coordinator confirmation action (the production `Confirm & Save` control) produces an
immutable, in-memory `ConfirmedScientificImportManifest`. Validation never auto-confirms; the confirmation exists only
after the explicit action, and it creates no active dataset and no authority. The record binds the
exact validated source transaction, the
resolved user decisions (activity mode, dataset-global spike-train identities, EventScopeGroup
definitions and membership, group time bases and event-relative origins, event and attribute
definitions, source time unit, timestamp ordering decisions, requested duplicate policies, and
Presentation decisions), the confirmed dataset-global RecordingSegment (reached through the sealed
validated import, never copied into a second truth), and the existing
`CanonicalScientificDatasetFingerprint`. It records an explicit temporal scope of one dataset-global
recording segment with unavailable acquisition bounds and no Trial entities.

The confirmation record is **not** an authority receipt. It confirms only what source and user
decisions were reviewed and which canonical fingerprint resulted. It does not confirm that the
analysis contract is complete and exposes no positive permission (no `authority`, `permitsDetection`,
or `permitsExport`). Scientific identity remains solely the canonical fingerprint — the confirmation
record adds no second scientific dataset digest, and its source binding and provenance never change
canonical scientific identity when the resulting canonical scientific data are equivalent. A
confirmation may additionally be persisted as a receipt-only record and later restored by full replay
(see *Confirmed-manifest persistence* below); the live base confirmation remains in-memory, and
persistence grants no analysis, detector, dataset, or export authority.

Construction is atomic and non-forgeable: a confirmation is built only from the sealed pairing of the
non-forgeable validated import and its shadow projection, reusing the fingerprint already produced for
that transaction. Confirmation is an explicit action — a clean validation never becomes a confirmation
on its own — and reruns no independent validation, restaging, renormalization, or spike-proportional
hashing pass. `collapseExact` remains only a requested future run-derived analysis-view policy and
never removes timestamps from canonical raw data.

A separate `ScientificAnalysisReadinessAssessment` reports stable functional blocking reasons. It is
deny-only and fail-closed: it never grants readiness, only enumerates blockers, and any blocker means
the import remains shadow-only. It always reports that observation bounds are unavailable and that the
run contract, the detector-consumer closure, and the authoritative-export closure are unavailable. It
also reports that confirmed-manifest persistence is unavailable for a bare in-memory confirmation; a
verified persisted wrapper (see below) removes exactly that one blocker and no other, and never grants
readiness. It never reports the recording segment as
unrepresented, because exactly one dataset-global segment is now confirmed. Regime adds Trial blockers by matrix: `continuous_untrialed` makes
Trial explicitly not applicable (no Trial blocker); `trialized` reports Trial entities not
represented; `unknown_or_uncertain` reports the regime unknown/uncertain and the Trial contract
unavailable. Coverage `not_all_spike_trains_full_imported_excerpt` reports partial coverage,
`unknown_or_uncertain` reports coverage unknown/uncertain, and `all_spike_trains_full_imported_excerpt`
adds no coverage blocker but grants no positive permission. For `intentional_multi_unit` and
`unknown_or_uncertain` activity modes it additionally reports that authoritative biological pattern
detection is not defined for that mode — a `not_evaluated` state, never zero, absent, negative, or "no
pattern detected". Even a putative-single-unit, all-coverage, continuous import remains shadow-only
because the remaining contracts are incomplete.

## Confirmed-manifest persistence (receipt-only)

A confirmed manifest may be persisted and later restored, but only by re-reading the original source
and replaying the complete canonical import chain. This removes exactly one readiness blocker
(confirmed-manifest persistence unavailable) and grants no detector, active-dataset, review,
result-package, or export authority. A file that merely exists or decodes is never sufficient to
restore confirmation.

Persistence is **receipt-only**. The stored receipt carries a functional persistence schema-contract
ID and digest, the canonical fingerprint identity triple (`schemaContractID`, `schemaContractDigest`,
`datasetDigest`), the exact `StagedSourceTransactionBinding`, the header rule (derived from the sealed
validated source — see below — never caller-supplied), the source
time unit, the activity mode, all four confirmed RecordingSegment fields, every group / spike-train /
event-definition / event-type identity and source-column membership, each group time basis and any
event-relative origin source-cell reference, spike/event ordering and duplicate decisions, every
attribute key/type/role/unit/empty-string decision (including Presentation-role definitions), and a
deterministic `confirmationRecordDigest`. It deliberately contains **no** source bytes, raw cells,
raw lexemes, header text, timestamps, event values, Presentation values, `PreparedScientificImport`,
`CanonicalScientificDataset`, normalization provenance, source path/filename/mtime/inode/bookmark, UI
UUID, wall-clock time, or app build. `encodedByteCount` is never an identity or authority field. The
canonical fingerprint remains the only scientific identity; the confirmation-record digest identifies
only the source-bound saved record and is never a second scientific identity.

The header rule is **derived**, never supplied. It has exactly ONE authoritative source: the sealed,
independently validated base confirmation's source columns
(`validatedImport.preparedImport.provenance.resolvedPlan.source.columns`), inspected only for optional
header *presence* (`header != nil`) and never for header *text*. Every column carrying a header yields
`firstRecordIsHeader`; no column carrying a header yields `headerless`; an empty column collection or
mixed header presence is a fail-closed typed error (`noSourceColumns` / `mixedHeaderPresence`) that is
never defaulted to headerless. An explicitly present empty or whitespace-only header still counts as
present. The rule is never inferred from the file extension, transport type, column names or contents,
UI state, receipt contents, first-row values, majority voting, or the first column alone. There is no
independent caller-provided header rule anywhere in the trust chain: `project(from:)` derives it,
`save(baseConfirmation:)` derives the whole receipt from the base before creating any directory,
temporary file, or final artifact, and `verified`/`restoreVerify` re-derive it from the base and
compare the entire expected receipt against the decoded on-disk record — so a headerful base can never
be projected, saved, or verified as headerless (a contradictory save is unrepresentable at the public
API). `receipt.headerRule` remains encoded and is still needed after a restart to choose how the
reselected source is staged, but it is an UNTRUSTED replay decision until a full replay produces a
fresh sealed base whose base-derived rule and complete receipt the store confirms exactly.
Header-rule / header-presence metadata is not directly encoded as a canonical scientific-identity
field, but the applied header rule always participates in the confirmation-record receipt, so changing
the applied rule always changes the confirmation-record digest. Changing the applied rule can also
change which first-record values enter the canonical scientific dataset; when the canonical data
differ, the canonical dataset fingerprint MUST change. The fingerprint stays unchanged only when the
two interpretations happen to produce semantically equivalent canonical scientific data — so a
confirmation-record mismatch with an unchanged fingerprint is possible, but it is not guaranteed for
every header-rule change. The header rule is thus a confirmation-record decision and never a directly
hashed scientific-identity field, yet it is not merely Presentation metadata: it can move records into
or out of the dataset and thereby change the fingerprint. The persistence schema transcript
binds this derivation as a structural fact
(`header_rule=derived_from_sealed_validated_source_header_presence` plus the empty/mixed fail-closed
rule), so the persistence schema-contract, record, and byte-transcript goldens move with it while the
functional persistence schema ID and every canonical dataset fingerprint golden stay fixed.

Three distinct states keep the boundary safe: the live in-memory `ConfirmedScientificImportManifest`;
the untrusted decoded receipt (which can neither enter readiness nor create a confirmation); and a
sealed `PersistedConfirmedScientificImportManifest` with a private initializer whose mint
(`verified(…)`) is `internal` to STPDCore and is additionally gated by an opaque
`DurableRecordCapability` whose own initializer is file-private to the store's source file. Only the
store — after completing its durability barrier on its OWN derived final record — can construct that
capability, so the compiler, not merely convention, prevents any other type in any module from
promoting a receipt to standing. The store's arbitrary-root initializer and its fault-injection seam
are `internal` (test-only via `@testable`); the sole public production factory is the fixed
Application Support store, which fails closed when that directory cannot be resolved. The durable
store, `ConfirmedScientificImportManifestStore`, is the single caller of the mint and the SOLE
production source of persistence standing: it mints only after reading back its OWN derived final path
(never a caller-supplied URL) and matching it against a freshly produced base confirmation. A generic bounded
read of an arbitrary file (`ConfirmedScientificImportManifestPersistence.readUntrusted`) returns only
a bare, untrusted receipt that removes no readiness blocker, so there is no public or package-callable
sequence — `project`/`encode` → arbitrary file → read → wrapper — that yields standing. Standing is
obtainable only by `save` (write → durability barrier → readback of the derived path → match) or
`restoreVerify` (re-read the derived path → match against a full replay).

A dedicated deterministic codec and strict bounded reader (not `Codable`) bind the field tags/order,
token vocabulary, integer/string/count encoding, collection ordering, size/count limits, and
record-digest domain separation. The reader fails closed on unknown/missing/duplicate/out-of-order
fields, unknown tokens, invalid UTF-8, invalid identifiers/enum values, count/length overflow,
truncated or trailing bytes, schema-ID/digest mismatch, record-digest mismatch, source-binding
defects, noncanonical collection order, and files larger than 8 MiB. It additionally rejects receipt
metadata graphs the resolver could never have produced, via a single shared invariant checker that
projection (debug assertion) and decoding (hard rejection) both use so the persisted contract cannot
drift from the resolver's live contract: an empty group collection or a group with no spike train; a
spike-train identity or group identity reused anywhere in the dataset; an event-definition identity
reused **within** one group — while reuse of the same event-definition ID **across** groups is
legitimate and accepted, matching the resolver's group-local scoping (e.g. two groups may each define
`stimulus`); source columns that do not form exactly one contiguous `1..N` partition in group order
(rejecting gaps, duplicates, interleaving, and unassigned columns); a headerless source that is not
exactly one group with no event definitions on recording-elapsed time; an event-relative origin that
does not reference an event definition in the same group; an exact-duplicate collapse request for any
activity mode other than putative-single-unit; and an invalid attribute combination such as
`allowExplicitEmptyString` on a non-string type. Because these graph facts are bound into the schema
transcript, the schema-contract and record-digest goldens moved in lockstep when the checker was
tightened. The filesystem read opens and pins an `O_NOFOLLOW` descriptor, rejects symlinks and non-regular
files, and reads in bounded chunks stopping at 8 MiB + 1, never calling unbounded `Data(contentsOf:)`.
Fixed schema, byte-transcript, and record-digest goldens plus an independent SHA-256 oracle pin it.
The existing canonical dataset fingerprint schema, encoder, and goldens are unchanged.

The store is app-managed (not a sidecar), laid out as
`…/confirmed-scientific-import-manifests/by-source/<source-sha256>/<record-digest>.stpdimportmanifest`,
with path components derived only from validated lowercase SHA-256 values — never filenames or
semantic IDs. Durable storage is resolved with no temporary-directory fallback; if it is unavailable
every operation fails closed. The store's durability and interprocess locking are rooted at a FIXED
trusted anchor — a directory that lives **outside** every store-owned directory (in production,
Application Support; the store root and its `by-source`/per-source subtree all sit beneath it) — opened
once by path and thereafter traversed only by descriptor. Every filesystem step below that anchor is
performed on descriptors opened with `O_NOFOLLOW` and traversed anchored
(`openat`/`mkdirat`/`fstatat`/`linkat`), so a symlink substituted anywhere in the store subtree — the
root subtree, the per-source directory, a temporary file, or a final record — is rejected as an unsafe
path object rather than followed. Writes are coordinated within a store instance and across separate
instances and processes by a **store-global** advisory lock: a single `flock` taken on the fixed
anchor descriptor and held for the entire save/restore critical section (durability, readback, and
identity verification), never a separate lock file. Because the anchor is namespace-stable, the lock
cannot split into distinct old/new critical sections if the `by-source` subtree is replaced beneath an
in-flight operation (an ABA); the deliberate trade-off is that all manifest persistence for one store
root is serialized — acceptable, because these records are small and correctness is preferred over
per-source concurrency. The lock is acquired **nonblocking** against a finite monotonic deadline and
honors task cancellation (never an unbounded blocking wait). Saves use unique `O_EXCL` temporary files
and reject an oversized or noncanonical encoding before any final artifact is published. For every
target — newly written, already existing (idempotent), or concurrently appeared — the full durability
barrier runs before standing is minted: the final path is verified as a regular non-symlink file, the
file is `fsync`ed, then the **complete** directory chain (per-source directory, `by-source`, every
store-root component, and the fixed trusted anchor) is `fsync`ed child-to-parent — always the whole chain, not only directories
created this attempt, because an earlier failed attempt may have left a visible-but-not-durable link —
with every error propagated, and finally the path is read back through the bounded reader and matched.
If the post-publish removal of the temporary name fails, the mint is aborted and the already-published
final record is reported as **visible and retryable** — not as already crash-durable, because its
directory barrier has not run — so a later retry makes it durable rather than the store claiming to be
clean. Any failure in the readback, durability barrier, or path/identity revalidation fails closed by
**minting no persistence standing**; it makes no promise that a final link was never published, since
an interrupted attempt's visible link is exactly what a later retry's whole-chain barrier makes
durable. A retry after a synchronization failure re-runs the barrier rather than skipping it because
the leaf directory or target now exists. `restoreVerify` runs this same barrier under the same
store-global lock, so a restore can never observe or mint from the window after a concurrent writer has
published a link but before that writer's durability barrier completes. Identical bytes are an idempotent success;
different bytes at the same target are a blocker and never overwrite. Multiple saved records for one
source are allowed (bounded to 128; exceeding the limit is a clear error that deletes nothing) and are
never latest-wins or auto-selected. Historical receipts are immutable and never auto-deleted, even when
the current form/source becomes invalid. Discovery enumerates entries incrementally (never
materializing the directory) and counts every entry against a deterministic entry budget. It opens each
candidate no-follow and **non-blocking** and rejects any non-regular object, then charges the **actual
bytes read** against a cumulative byte-work budget, so a candidate that grows after its size probe
cannot bypass the budget. A file whose stat size already exceeds the per-file cap is charged and
rejected **without being read**, so no multi-gigabyte slurp is possible; and a read that fails after a
valid prefix is counted **unreadable** rather than mistaken for a clean end-of-file that could list a
truncated record. Exceeding either budget fails clearly rather than returning a partial list. A
budget-exhaustion, lock-timeout, or unreadable-store condition reaches the UI distinctly even when zero
records are returned, and a file that cannot be read, or whose filename/record-digest/source-SHA
disagree, is counted as unreadable — never silently reported as "no saved records".

`Confirm & Save` appears only after one clean validated preparation and traverses decision metadata
only — it never restages, renormalizes, revalidates, reprojects, refingerprints, or scans spike
timestamps. `Restore & Verify` requires the user to reselect the source; the app then discovers saved
records under that exact SHA (discovery never auto-restores), requires explicit selection even for a
single record, and is permitted only from a clean, Stage-B-absent state so that no prior shadow can
coexist with a restored confirmation and a failure leaves the previous transaction unchanged. It reads
the saved record as an untrusted bare receipt to drive the replay, applies the saved worksheet and
header decision into local values, reconstructs the manifest draft, runs exactly one full chain (stage
→ resolve → normalize → validate → project → fingerprint → confirm) into local values, and then has
the store re-read its derived final record and match it against the replayed base. Only on complete
equality does it commit atomically — installing the receipt's header and worksheet selections together
with the restored base confirmation and persisted wrapper as one consistent restored transaction. A
saved-record selection cannot change underneath an in-flight restore: the rows are disabled while
persistence work is active, the coordinator guards the selection setter, and each restore captures its
record digest at the start so a later selection change cannot install a different record. Distinct failures are reported: source unavailable/unverified, source-binding
mismatch, corrupt/unsupported receipt, replay divergence (canonical fingerprint mismatch), and
confirmation-record mismatch. A source-unavailable record is never called scientifically stale; an
unchanged fingerprint with a differing confirmation-record digest is a confirmation-record mismatch,
not a changed dataset. A presentation-only change may change a confirmation-record digest without
changing the canonical fingerprint, whereas moving an attribute across the Scientific/Presentation
boundary changes the canonical fingerprint. The coordinator generation barrier discards any
save/restore completion once source, form, worksheet, header, preflight, validation, or transaction
state changes, and each save/restore carries an operation identifier so a stale completion mutates no
state belonging to a newer one; a new validation generation always clears the in-memory persisted
wrapper. Because `STPDCore` does not depend on `STPDTabularIO`, the header rule is a Core-owned type
mapped at the app boundary only for *staging*; it is never a persisted authority the app supplies. On
`Confirm & Save` the coordinator passes only the sealed base to the store (the store derives the
rule), and it may defensively compare the current UI header decision with the base-derived rule to
detect stale UI, but that comparison is never the source of the persisted rule. On `Restore & Verify`
the coordinator uses `receipt.headerRule` solely to stage the reselected source locally and then calls
`restoreVerify` with no header parameter, letting the store derive the authoritative rule from the
replayed base.

## Transactional import and compatibility boundary

Import is a transaction:

```text
read bounded source
    -> parse into staging
    -> validate and show source-identified findings
    -> collect explicit user decisions
    -> confirm and persist the manifest
    -> atomically install the canonical dataset
```

Until the final install succeeds, the currently active dataset and all of its
analysis state, source binding, and UI selections remain unchanged. Cancel,
parse failure, validation failure, or confirmation failure must not partially
replace or clear active state. A successful identity-changing install replaces
the dataset atomically and invalidates its dependent state in the same
transaction.

The new canonical path is built in parallel and remains inactive until its
acceptance gates pass and a controlled activation decision is made. It must not
write canonical staging data under a legacy identity. Existing result packages
are read-only compatibility inputs: they may be opened through a compatibility
adapter, but must not be silently rewritten, migrated, or rebound to a new
identity.

## Offline boundary and deferred capabilities

The authoritative workflow covered here is offline: the complete bounded input
is available before parsing, confirmation, QC, and detection.

A future online workflow may load an explicitly identified preset from prior
experiments, use a user-specified initial interval for recalibration, and then
monitor incoming ISIs with a separately validated real-time rule set. Its
buffering, clock, latency, missing-data, recalibration, and uncertainty
contracts are not yet defined, so it is not part of this implementation.

Multi-unit detector semantics, per-event scientific consumers, and unit
conversion maps are also deferred. Mixed-mode files and per-column activity
modes are unsupported under this contract; known heterogeneous data must be
split into separate datasets or import batches.

## Blocking acceptance criteria

Activation is blocked if any of the following remains possible:

- implicit source-unit, activity-mode, attribute-role, type, unit, or empty
  value decisions;
- canonical timestamp parsing or source-unit conversion through general binary-
  floating-point rounding, range overflow, or locale-dependent canonical
  formatting; the sole XLSX exception is the unique one-ULP serialization
  inverse defined above;
- cross-group event leakage or row-based trial inference;
- ambiguous event-origin selection;
- CSV/XLSX semantic inequality for equivalent confirmed input;
- silent truncation or an unbounded XLSX expansion path;
- authoritative detector output for multi-unit, unknown, or invalid putative
  single-unit data;
- activation without a detector-projection regression gate: clean on-grid
  putative-single-unit fixtures must preserve ordered ISIs, candidates, labels,
  and arbitration when canonical integer ticks are projected into the current
  detector; actual threshold predicates and numeric margins are compared where
  feasible, and every deliberate floating-boundary difference is enumerated
  and scientifically adjudicated rather than excused by a new capability or
  identity name;
- mutation of the active dataset after any failed or cancelled import;
- silent rewriting of historical result packages;
- a new ordinal release label used in place of a functional name.
