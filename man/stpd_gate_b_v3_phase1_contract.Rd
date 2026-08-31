\name{stpd_gate_b_v3_phase1_contract}
\alias{stpd_gate_b_v3_phase1_bundle}
\alias{stpd_gate_b_v3_phase1_schema_lint}
\alias{stpd_gate_b_v3_empty_table}
\alias{stpd_gate_b_v3_validate_table_prototype}
\alias{stpd_gate_b_v3_validate_product_prototype}
\alias{stpd_gate_b_v3_canonical_table_bytes}
\alias{stpd_gate_b_v3_label_blind_input}
\alias{stpd_gate_b_v3_state_axis_decision}
\alias{stpd_gate_b_v3_pause_attainability}
\alias{stpd_gate_b_v3_compatibility_matrix}
\alias{stpd_gate_b_v3_compatibility_action}
\alias{stpd_gate_b_v3_complexity_contract}
\alias{stpd_gate_b_v3_resource_budget_check}
\alias{stpd_gate_b_v3_resource_staging_probe}
\alias{stpd_gate_b_v3_validate_transition_payload}
\alias{stpd_gate_b_v3_validate_threshold_permissions}
\alias{stpd_gate_b_v3_validate_redacted_artifacts}
\alias{stpd_gate_b_v3_phase1_registry}
\alias{stpd_gate_b_v3_run_phase1_fixture}
\alias{stpd_gate_b_v3_phase1_fixture_spec}
\alias{stpd_gate_b_v3_phase1_halt_report}
\title{Gate B v3 phase-1 executable contract prototypes}
\usage{
stpd_gate_b_v3_phase1_bundle(path = NULL, verify_frozen = TRUE)

stpd_gate_b_v3_phase1_schema_lint(bundle = stpd_gate_b_v3_phase1_bundle())

stpd_gate_b_v3_empty_table(table_name, bundle = stpd_gate_b_v3_phase1_bundle())

stpd_gate_b_v3_validate_table_prototype(
  x, table_name, bundle = stpd_gate_b_v3_phase1_bundle(),
  tables = NULL, require_sorted = TRUE
)

stpd_gate_b_v3_validate_product_prototype(
  tables, bundle = stpd_gate_b_v3_phase1_bundle()
)

stpd_gate_b_v3_canonical_table_bytes(
  x, table_name, bundle = stpd_gate_b_v3_phase1_bundle()
)

stpd_gate_b_v3_label_blind_input(ds, acquisition_context = NULL)

stpd_gate_b_v3_state_axis_decision(frequency, regularity)

stpd_gate_b_v3_pause_attainability(valid_local_n_isi, family_size, alpha)

stpd_gate_b_v3_compatibility_matrix()

stpd_gate_b_v3_compatibility_action(
  candidate_domain, candidate_class, overlap_domain, overlap_class
)

stpd_gate_b_v3_complexity_contract()

stpd_gate_b_v3_resource_budget_check(counters, cancelled = FALSE)

stpd_gate_b_v3_resource_staging_probe(
  root, operations = list(), cancelled = FALSE
)

stpd_gate_b_v3_validate_transition_payload(action_type, payload)

stpd_gate_b_v3_validate_threshold_permissions(
  policies, instances, memberships
)

stpd_gate_b_v3_validate_redacted_artifacts(object)

stpd_gate_b_v3_phase1_registry(verify_materialized = TRUE)

stpd_gate_b_v3_run_phase1_fixture(fixture_id)

stpd_gate_b_v3_phase1_fixture_spec(verify_materialized = TRUE)

stpd_gate_b_v3_phase1_halt_report()
}
\arguments{
  \item{path}{Optional schema-bundle path.}
  \item{verify_frozen}{Verify available frozen plan and contract bytes.}
  \item{table_name}{A table name in the frozen executable bundle.}
  \item{bundle}{An optional preloaded schema bundle.}
  \item{tables}{For a table prototype, optional named related tables for
  foreign-key validation; for a complete product, the exact named set of all
  59 canonical and signed-support tables.}
  \item{require_sorted}{Require the frozen canonical row order.}
  \item{counters}{Untrusted caller-supplied resource data. Direct checks are
  disabled because an R object cannot prove how counters were measured; use
  the controlled staging writer instead.}
  \item{cancelled}{Whether cancellation was observed at a declared boundary.}
  \item{root}{Existing directory for an isolated staging probe.}
  \item{operations}{A declarative list of controlled-writer operations. Each
  operation uses an exact typed schema; arbitrary callback functions and
  caller-supplied write counters are rejected.}
  \item{action_type}{One of the nine frozen FINAL transition actions.}
  \item{payload}{Named action-specific transition payload.}
  \item{policies,instances,memberships}{Canonical threshold provenance and
  partition-membership tables.}
  \item{object}{Nested diagnostic, log, or artifact metadata to inspect for
  raw identifiers or paths.}
  \item{x}{A data frame to validate or serialize.}
  \item{ds}{A dataset with a named \code{trains} list.}
  \item{acquisition_context}{Optional allowlisted acquisition and QC metadata.}
  \item{frequency}{One of \code{high}, \code{non_high}, or \code{unresolved}.}
  \item{regularity}{One of \code{regular}, \code{irregular}, or \code{unresolved}.}
  \item{valid_local_n_isi}{Number of valid local reference intervals.}
  \item{family_size}{Number of Pause candidates in the adjustment family.}
  \item{alpha}{Operational adjusted-score threshold.}
  \item{fixture_id}{A stable phase-1 fixture identifier.}
  \item{verify_materialized}{Compare live registry or fixture rows with the
  installed canonical JSON artifact.}
  \item{candidate_domain,candidate_class,overlap_domain,overlap_class}{A frozen compatibility tuple.}
}
\value{
Depending on the function, a schema bundle, exact empty table, canonical raw
bytes, label-blind input, decision record, registry, fixture specification, or
non-authoritative phase-1 halt report.
}
\description{
Executable prototypes for the frozen Gate B v3 phase-1 contract. These helpers
cannot grant Gate B authority, create signed release evidence, or make a
biological-ground-truth claim. Pause BH values are operational adjusted scores;
the helpers explicitly make no false-discovery-rate control claim. Canonical
double inputs reject negative zero, UTC fields use validated Gregorian
RFC3339 timestamps, and complete products enforce the AUTO/FINAL parent and
history-presence matrix. Label-blind reconstruction strips caller classes and
attributes before any container accessor is used, then copies only normalized
timestamps and the explicit acquisition/QC allowlist. Current-product
entity-evidence edges bind their canonical source key, complete typed source
row, source domain, and deterministic edge index; detached phase-1 validation
continues to reject unverified cross-product provenance. The staging probe
executes only declarative controlled-writer operations and performs conservative
row/byte/resource preflight before creating staging or invoking a serializer;
it then measures actual written bytes and peak RSS, verifies the complete staged
artifact set, and removes its private generation before returning. Portable
audit metadata uses the active
\code{portable_audit_metadata_v1} registry schema: all string fields have closed
typed value domains and free text is forbidden.
}
