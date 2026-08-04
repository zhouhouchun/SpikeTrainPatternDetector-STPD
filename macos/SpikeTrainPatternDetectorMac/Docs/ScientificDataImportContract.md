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

Canonical time is a signed 64-bit integer count of microseconds. Parsing and
unit conversion must be exact and checked; it must not pass through binary
floating point, apply an epsilon, or round to the nearest tick. A value outside
the signed range or off the microsecond grid is a blocking error.

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

Repeated display headers remain distinct definitions until the user assigns
stable, unambiguous semantic IDs. A confirmed event definition remains part of
scientific identity even when it contains no occurrence; that condition is a
warning, and the empty definition cannot be selected as a time origin.

An event type is a reusable semantic category. An event definition is the
confirmed event column in one group. An event occurrence is one timestamp plus
its attached attributes. None of these constructs a trial; trial semantics
require a future explicit contract.

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

Raw multiplicity is always retained in provenance.

- Input order is preserved in provenance. A non-monotonic timestamp column is
  never silently sorted: the user must either approve an audited stable sort or
  cancel and repair the source. Scientific identity uses the resulting final
  canonical sequence, not the operation history.
- Putative single-unit duplicates are unresolved until the user makes an
  explicit, audited choice. Unresolved duplicates block authoritative results.
  Exact duplicate collapse is allowed only as a non-default normalization.
- Multi-unit and unknown inputs preserve multiplicity. Any future merge policy
  must be explicit and cannot borrow single-unit assumptions.
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

Scientific identity is derived from the confirmed semantic manifest and
canonical data, including:

- stable semantic IDs and membership for groups, spike trains, and event
  definitions;
- activity mode and confirmed scientific settings;
- each group's confirmed time basis and selected origin occurrence semantics;
- exact final canonical spike and event ticks and retained canonical
  multiplicity;
- event occurrences and their association with definitions;
- typed canonical Scientific attribute values and their confirmed units.

Digest ordering follows stable semantic IDs, not source column order. Duplicate
display names require explicit disambiguation before confirmation.

The following remain provenance and do not by themselves change scientific
identity: CSV versus XLSX, source filename and bytes, sheet/cell/row address,
raw lexeme, source time unit, display time unit, UI batch-selection gestures,
Presentation attributes, original order and multiplicity, source origin offset,
and sort/collapse/rebase operation history. Raw input and every normalization
decision must remain inspectable. A normalization that changes the final
canonical object changes identity through that object, not through the gesture
or history record itself.

Changing canonical data, activity mode, a Scientific attribute definition or
value, a confirmed scientific unit, or another scientific setting changes
identity and invalidates dependent results. A presentation-only change does
not.

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
- canonical timestamp parsing or source-unit conversion through binary floating
  point, rounding, range overflow, or locale-dependent canonical formatting;
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
