\name{stpd_threshold_first_phase0}
\alias{stpd_threshold_first_run_config_enums}
\alias{stpd_threshold_first_run_config}
\alias{stpd_threshold_first_validate_run_config}
\alias{stpd_threshold_first_migrate_legacy_config}
\alias{stpd_threshold_first_numeric_contract}
\alias{stpd_threshold_first_robust_log_reference}
\alias{stpd_threshold_first_log_residual}
\alias{stpd_threshold_first_empirical_tail_p}
\alias{stpd_threshold_first_bh_adjust}
\alias{stpd_threshold_first_run_surprise}
\alias{stpd_threshold_first_local_reference_indices}
\alias{stpd_threshold_first_local_ratio}
\alias{stpd_threshold_first_adjust_tail_by_family}
\alias{stpd_threshold_first_q_zone}
\alias{stpd_threshold_first_qc_floor_status}
\alias{stpd_threshold_first_bridge_ratio}
\alias{stpd_threshold_first_resolution_action}
\alias{stpd_threshold_first_input_policy_contract}
\alias{stpd_threshold_first_input_provenance}
\alias{stpd_threshold_first_validate_input_provenance}
\alias{stpd_threshold_first_normalize_train_view}
\alias{stpd_threshold_first_validate_train_view}
\alias{stpd_threshold_first_label_free_view}
\alias{stpd_threshold_first_scientific_train_projection}
\alias{stpd_threshold_first_train_view_hash}
\alias{stpd_threshold_first_train_view_audit_hash}
\alias{stpd_threshold_first_scientific_params}
\alias{stpd_threshold_first_validate_scientific_params}
\alias{stpd_threshold_first_scientific_hash}
\alias{stpd_threshold_first_provider_interface_contract}
\alias{stpd_threshold_first_provider_identity}
\alias{stpd_threshold_first_validate_provider_identity}
\alias{stpd_threshold_first_map_provider_status}
\alias{stpd_threshold_first_validate_provider_status}
\alias{stpd_threshold_first_phase0_manifest}
\title{Dormant threshold-first Phase 0 contracts}
\description{
Versioned, fail-closed contracts for a future threshold-first detector path.
They define orthogonal run modes, label-free input normalization, exact
numerical primitives, scientific identity, and provider provenance.  They do
not invoke or modify the active detector.
}
\details{
The default and migrated engine remains \code{legacy}.  Contrast-based
four-spike support is restricted to the initial seed proposer, and the 3.5
ratio applies only to a strict single bridge; neither rule is a universal
final-Burst gate.  Local-window and FDR defaults are shadow-development
preregistrations rather than universal neurophysiological constants.

The normalized train view retains a complete raw-to-normalized audit map.
Scientific hashing uses a separate projection containing canonical
timestamps, recomputed ISIs, acquisition and analysis blocks, QC masks,
effective parameters, provider/fold identity, and RNG identity.  Raw row
numbers, UI fields, log timestamps, and audit materialization level are
excluded from the scientific projection.

Provider evidence is mapped at provider-by-train-by-pattern-by-scope
granularity.  Positive support is existential and may have partial coverage;
positive support with no scope coverage is a contract error.  Only a resolved,
explicit negative with complete coverage and adequate model status is negative
evidence.  Unsupported, unresolved, and error states remain abstentions.
Provider-assisted performance requires recorded information access and
group-separation provenance.

In Phase 0, the scientific-parameter object is explicitly a caller-supplied
contract payload whose completeness is unverified until an active engine
parameter resolver is wired.  Its hash is not yet proof that every effective
detector parameter was captured.

BH family completeness remains unverified until the candidate-lineage gate;
the helper alone cannot prove that no candidates were prefiltered.  The bridge
helper consumes, but does not infer, an upstream two-sided direct-support mask
and requires at least two contiguous direct-support ISIs per flank.
}
\value{
Contract constructors return versioned lists or normalized train views;
validators return \code{invisible(TRUE)} or raise a typed
\code{stpd_threshold_first_error}; numerical helpers return deterministic
indices, scores, classifications, or mapped status/outcome pairs; hash
functions return lowercase SHA-256 strings.
}
