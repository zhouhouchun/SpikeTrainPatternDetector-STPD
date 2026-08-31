\name{stpd_multitrack_auto}
\alias{stpd_multitrack_auto}
\alias{stpd_write_multitrack_auto}
\title{Access and Export the Automatic Multi-Track Product}
\usage{
stpd_multitrack_auto(ds)

stpd_write_multitrack_auto(ds, out_dir)
}
\arguments{
  \item{ds}{A detector dataset containing \code{results$multitrack_auto}, or
  an object returned by \code{stpd_multitrack_auto()}.}
  \item{out_dir}{Directory in which to write the typed RDS, CSV tables, and
  manifest.}
}
\value{
\code{stpd_multitrack_auto()} returns a strictly validated
\code{stpd_multitrack_auto_product}. \code{stpd_write_multitrack_auto()}
invisibly returns a named character vector of written paths. When a parent
dataset is supplied, validation deterministically rematerializes every table
from the parent Phase 1A/1B evidence; a detached product receives internal
schema, geometry, foreign-key, and hash validation only.
}
\description{
Access or export the detector's automatic Event, State, Gap, and Review
representation. A successful Gate B attestation promotes it to the authoritative
automatic prediction representation; it is never declared biological ground truth.
}
\details{
The product is materialized on every successful detection run from the intact
Phase 1A/1B multi-track candidate evidence and does not depend on the optional
public Preview flag. A canonical burst is represented by one Event row with
extent and frequency classes. Exact same-train, same-ISI-geometry Event subtype
evidence is merged into that row rather than counted twice: explicit
\code{burst}, \code{long_burst}, or \code{prolonged_burst} evidence supplies the
extent class, while explicit high-frequency-burst evidence supplies the
frequency class. Conflicting explicit extent evidence is retained as
\code{extent_class = "unresolved"} with a diagnostic. Ordinary frequency is not
inferred from the absence of HFB evidence, so otherwise the frequency class is
\code{"unresolved"}.

The version-3 State hierarchy has one formal Broad-HFS parent:
\code{state_class = "high_frequency_spiking"} and
\code{state_family = "broad_high_frequency_state"}. The parent geometry is
generated once and frozen before subtype classification. On that same direct
support, adjacent-CV2 evidence classifies the descriptive
\code{state_subtype} as \code{"high_frequency_tonic"},
\code{"high_frequency_irregular_state"}, or \code{"hf_unresolved"}. A subtype
never creates, enlarges, splits, deletes, or out-scores its Broad-HFS parent.
Legacy HFT candidates remain audit evidence only. Ordinary tonic is a separate
State selected only outside accepted Broad-HFS support. Because the available
real reference labels Broad HFS rather than its regularity subtypes, subtype
fields are descriptive and are not independent performance endpoints.

Burst and every State interval may coexist through a non-destructive
relationship; Burst evidence cannot veto or split Broad HFS, HFT-subtyped HFS,
HF-irregular-subtyped HFS, or tonic. Pause and invalid ISI support are hard
direct-support boundaries. Canonical Pause is excluded from State support while
an explicit Broad-HFS episode envelope and episode identifier may retain the
relationship between HFS fragments on either side. Burst-rich and packet
evidence is retained only as review context and cannot delete HFS. Legacy
\code{pattern_auto} and event tables remain
an unchanged parallel compatibility representation. Before Gate B passes, its
metadata remains \code{authoritative = FALSE},
\code{authority_scope = "none_preview"},
\code{promotion_gate = "gate_b"}, and
\code{promotion_status = "pending"}; the intended future target scope is
\code{automatic_prediction_record}. After all twelve Gate B checks pass, the
scope becomes \code{automatic_prediction_record} and the promotion status becomes
\code{passed}. In both states, \code{detector_performance_eligible} is false.
Passing Gate B alone does not establish biological performance or default
fitness; those claims require the separate Gate C evidence programme. The Gate
B candidate now materializes a complete per-ISI projection for every selected
train. It preserves raw timestamps and ISIs, permits Event and State foreign
keys on the same row, forbids Gap co-ownership with either, and is validated in
both directions against the canonical interval tables and its parent dataset.
Its metadata therefore records
\code{per_isi_materialization = "full_selected_train_scope"}. Pipelines that do not
emit Phase 1A/1B evidence still complete the legacy run and produce a typed
candidate with \code{materialization_status = "not_available"} and empty
scientific tables. Candidate filenames use the
\code{Multitrack_auto_candidate_*} prefix so they cannot be mistaken for a
promoted prediction record. Direct calls to
\code{stpd_write_multitrack_auto()} are strict. The legacy result-export path
handles a damaged pending candidate fail-soft: candidate-only files are
omitted, an explicit candidate export-status CSV is written, and legacy,
Preview, and Review exports continue. The coordinator is also run for legacy
datasets without this product: it removes any stale candidate files from a
reused output directory and records \code{export_status = "not_present"}.
Failure to prove that stale candidate files were removed is a fatal export
error, because continuing would create an ambiguous mixed-run bundle.
}
