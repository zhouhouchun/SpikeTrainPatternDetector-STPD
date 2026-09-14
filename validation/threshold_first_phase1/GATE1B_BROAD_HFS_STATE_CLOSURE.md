# Threshold-first Gate 1B Broad-HFS State closure

- Date: 2026-08-30
- Decision: GO for the bounded Broad-HFS observer root
- Root ID: `broad_hfs_state_root`
- Scientific detector behavior: unchanged
- Public-product authority: unchanged and non-authoritative
- Publication authority: `FALSE`

## Closed scope

This stage directly observes the active `hf_protected` Broad-HFS parent path.
It locks the detector input, effective parameters, train-resolved parameters,
requested patterns and already accepted canonical Pause boundaries before the
scientific HFS call. It closes only after the final Gap root, preserving the
serial Gate 1B dependency while retaining the earlier scientific call order.

The observer records five typed tables:

1. unique Broad-HFS parent-generator entry;
2. candidate episode envelopes and selected-parent status;
3. per-ISI direct-support versus tolerated-connector roles;
4. pre/post nested-Review parent selection; and
5. a hash-linked closure receipt.

The patch is observer-only. It does not add or alter any HFS, Burst, Pause,
Tonic or Review candidate, threshold, score, selector, subtype decision,
legacy label, multitrack result, or exported prediction.

## Frozen scientific semantics

`stpd_event_core_detect_hf_spiking` is the only Broad-HFS parent-support
generator. Requests for HFT or HF-irregular invoke this same parent generator;
they do not create an independent State geometry. HFT/HF-irregular remain
descriptive regularity subtypes calculated later on the frozen parent direct
support.

Every observed ISI inside a candidate envelope has one role:

- `direct_support`: active Broad-HFS State support; or
- `tolerated_connector`: episode membership without active State support.

The latter distinguishes supra-direct-support connectors from transparent
artifact connectors. Candidate closure requires
`envelope = direct support + tolerated connectors`. The independently derived
supra-direct-support connector count must equal the detector's reported
connector count.

Accepted canonical Pause boundaries are excluded before HFS support-run
generation. No emitted candidate may contain a canonical Pause boundary.
Pause therefore cannot become active HFS support or a tolerated connector in
this root. A later composite relationship may relate State episodes across a
Pause without including Pause in direct support.

Burst is an orthogonal Event overlay. The observer proves that nested-HFS
Review proposals leave the selected Broad-HFS parent signature unchanged;
neither Burst nor Review may veto, split, create or expand the parent State.

## Authority boundary

The per-ISI role ledger is direct observation evidence for Gate 1B; it is not
yet an authoritative public AUTO/FINAL v3 product. The existing scientific and
multitrack products remain unchanged. Performance, biological truth, subtype
accuracy, release promotion and publication authority remain unavailable.

## Review checks

| Required check | Outcome |
|---|---|
| One Broad-HFS parent before subtype | PASS |
| HFT/HF-irregular use the same parent generator | PASS |
| Direct support and connector roles separated | PASS |
| Connector identity equals scientific detector audit | PASS |
| Canonical Pause excluded from candidate support/envelope | PASS |
| Burst overlay applies no State veto | PASS |
| Nested Review leaves parent signature unchanged | PASS |
| Collector-on/off scientific output identical | PASS |
| All five payloads mutation-rejected | PASS |
| Publication authority remains false | PASS |

## Verification

| Check | Outcome |
|---|---|
| Broad-HFS focused suite | PASS (31 expectations) |
| Active-entry ordering suite | PASS (33 expectations) |
| Burst-stage lineage suite | PASS (38 expectations) |
| Final-Gap suite | PASS (24 expectations) |
| HFS/Pause coexistence suite | PASS (35 expectations) |
| Multitrack AUTO product suite | PASS (184 expectations) |
| Clean source build | PASS |
| `R CMD check --no-tests --no-manual --no-build-vignettes` | `Status: OK` |
| R parse, DESCRIPTION/Collate and `git diff --check` | PASS |

The source-only runner emitted its expected local DLL warning under
`pkgload::load_all(..., compile = FALSE)`. The clean source package compiled,
loaded and unloaded successfully during `R CMD check`.

## Frozen implementation hashes

- `R/91j_candidate_lineage_broad_hfs_state.R`:
  `47b591bb98814194e7cbb841d31a0ff880a281f9a2972b739c1ce83a2e6dd447`
- `R/91_candidate_lineage_collector.R`:
  `5c63f386e70317f08a7e5a94249d181c156d4764db547bb44c9c3ff78954f6bd`
- `R/42_hfspiking_protection_and_manual_coexistence.R`:
  `65b6029d14101a5cbff62f84447c42f708659b444e9f47b1bcab4bd5ca3e56a3`
- focused test:
  `6b5a10cd4c812f98cb5bc09601202e291472b45e26d91c031598d714afc9c034`

## Next bounded stage

Close the Tonic State root. The next stage must keep Tonic as a State, use
frequency and regularity evidence without a universal absolute ISI threshold,
allow sparse evidence to abstain, prevent a small number of Burst Events from
automatically cutting Tonic, and require full redetection when a genuine State
boundary is applied.
