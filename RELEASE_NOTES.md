# Spike Train Pattern Detector 1.2.1

## Schema-first parameter YAML

- Added `inst/config/parameters.yml` as the canonical source for runtime defaults, product defaults, key UI schema rows, and eventness-audit schema rows.
- `default_params_sec()`, `stpd_product_schema_defaults()`, `stpd_key_parameter_schema()`, and the eventness schema extension now materialize from the YAML source.
- Added regression tests that verify YAML loading and ensure key schema defaults stay synchronized with materialized default parameters.
- Synchronized two older schema/default drifts: `detector.patterns_to_run` now includes `others`, and `burst.long_burst_edge_min` is explicit in runtime defaults.
- Expanded the YAML file with a full `parameter_contract` covering all materialized default parameters.
- `stpd_parameter_schema(scope = "all")`, `stpd_parameter_registry()`, and `stpd_validate_params()` now use the YAML contract for type, range, and choice validation.
- Replaced the hand-written Shiny `Detector / Parameters` tab with `stpd_contract_ui_controls()`, a grouped UI generated from `parameter_contract`.
- Added regression coverage that verifies contract UI generation and `contract_param_` input write-back to nested parameters.
- Added parameter YAML import/export helpers and Shiny controls for UI round-trip, contract validation, and YAML hash round-trip checks.
- Added UI metadata to `parameter_contract` (`ui_level`, ordering, section, help, visibility, and control type), split generated parameters into Basic / Advanced / Expert layers, and made validation/report tables carry the same metadata.
- Refined the Basic layer with biology-facing labels/help, workflow ordering, and a Shiny preview of UI-visible parameter changes before detector reruns.
- Added `stpd_parameter_delta_preview()` and Shiny controls for selected-train dry-run event diffs between current parameters and a baseline, including pattern count deltas and added/removed/label-changed event rows without mutating formal results.
- Added changed-event row click-through to the aligned raster, dry-run delta event overlays on the raster and ISI temporal profile, and ZIP export of `Parameter_delta_preview_summary.csv`, `Parameter_delta_preview_counts.csv`, and `Parameter_delta_preview_events.csv`.
- Added event-level MANUAL-vs-detector IoU validation reports and Basic-parameter sensitivity scanning with metric curves, boundary-error/label-confusion match tables, and exports for `Parameter_sensitivity_summary.csv`, `Event_level_validation_metrics.csv`, and `Manual_detector_event_matches.csv`.

## Productized detection safeguards

- Promoted the product hardening release to `1.2.1`.
- Added `R/50_internal_api.R`, a version-neutral internal API layer for new detector code.
- Added stable result aliases: `threshold_table`, `candidate_diagnostic_audit`, `candidate_features`, `final_decisions`, `eventness_audit`, and `run_metadata_public`.
- Removed historical versioned exports from `NAMESPACE`; normal detector results now drop legacy versioned result fields and train attributes after stable aliases are created.
- Updated regression tests to prefer stable result fields over historical versioned fields.
- Pre-detection QC now runs before any pattern labels are generated. By default, data-integrity errors such as exact duplicate timestamps, zero-or-negative ISIs, and hard artifact ISIs stop detection before burst, pause, tonic, or high-frequency evidence is computed.
- Added `params$spiketrainpattern$engine$stop_on_qc_error`, `freeze_dataset_thresholds`, and `honor_manual_lock_for_auto` as public policy controls.
- Dataset/manual/histogram thresholds are resolved once at the dataset entry point and then frozen for all selected trains in that run. The resolved table is stored in `results$threshold_table`.
- Pre-detection QC output is stored in `results$pre_detection_quality`.
- With `lock_manual = TRUE`, AUTO labels are not written onto manually labeled ISIs; manual labels remain final-label dominant while unlocked intervals remain available for AUTO diagnostic review.
- Added regression coverage for product parameter mirroring, dataset-scope threshold freezing, manual-lock behavior, and pre-detection QC failure policy.

This release consolidates the user interface for publication-oriented use.
Artifact, refractory-suspect, duplicate timestamp, and detector-family settings are controlled in dedicated UI sections and are not duplicated in generated parameter panels. The schema remains available for reporting, hashing, and reproducible export.


## post-overlap post-overlap stability patch

- Clarified that manual examples calibrate train-specific ranges; they are not a supervised classifier.
- Added post-overlap minimum-size enforcement for AUTO events. Final visible/exported AUTO fragments now must still satisfy pattern-specific minimum spike counts after burst/HF/tonic/pause priority resolution.
- In particular, `high_frequency_spiking` fragments shorter than `hf_spiking_min_spikes` are removed from AUTO labels and logged in `Posthoc_fragment_audit.csv`.
- Added a Shiny audit table under `Events / outputs -> Post-overlap minimum-size enforcement`.

## Physiology-gated arbitration

- Added the physiology-gated arbitration module.
- Existing Structure-Seed-Bridge burst output is now treated as a burst-candidate layer when event-level arbitration is enabled.
- Added canonical burst hard gates: immediate pre/post edge ratio, local context compression, edge return-to-baseline, absolute core-q90 ceiling with strict/+5%/+10% fuzzy zones, max internal bridge ISI, and internal coherence vetoes.
- Added independent high-frequency tonic/spiking candidates before final arbitration to prevent weak burst candidates from fragmenting sustained HF states.
- Added manual `not_burst` hard-negative labels stored in `pattern_manual_negative`.
- Added exportable `Candidate_arbitration_audit.csv` with gate pass/fail status and decision paths.

## UI preservation patch

- Fixed a UI sync bug where clicking `Estimate params` or `Apply estimated to UI` reset user-entered `Pattern-specific Min_ISI / Max_ISI gates` back to zero.
- Pattern-specific ISI gates are now treated as manual hard-gate overrides and are preserved during parameter estimation, estimated-parameter application, and display-unit resync.
- When a new dataset/workspace is loaded, stored pattern-specific gates can still be restored from saved parameters.

## Public audit, CSV input, and parameter namespace policy

The event-grammar detector keeps diagnostic candidate windows and public biological calls separate.
Rejected, profile, unwritten, blocked or not-selected windows stay in `Candidate_diagnostic_audit.csv` and are not propagated into `Candidate_ledger.csv`, `Eventness_audit.csv` or final classification exports.

`rejection_reason` is reserved for diagnostic/rejected rows. Public selected candidates should leave it blank; neutral external strings such as `no rejection`, `not rejected` and `not applicable` are treated as neutral.

Raw CSV import expects spike timestamp columns. High-confidence derived tables such as `Sliding_*`, `ISI_base`, `tonic_summary`, `threshold`/`threshould`, `Candidate_ledger`, `Eventness_audit`, `Events_final`, `diagnostic candidate`, `summary`, `result`, `features` and `audit` files are blocked by default. Use `allow_derived_csv=TRUE` only for intentional overrides.
- Release hardening: added explicit imports for graphics/stats/utils helpers, expanded NSE global-variable declarations, documented duplicate timestamp handling and CSV guardrails.


## Product parameter namespace

Public configuration should use `params$spiketrainpattern`. Compatibility handling for older saved workspaces is resolved by `stpd_productize_params()` before detection and should not be used as a public parameter namespace.
