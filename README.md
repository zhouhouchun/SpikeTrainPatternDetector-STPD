# Spike Train Pattern Detector

Spike Train Pattern Detector is a candidate-event generation and audit-oriented analysis platform for spike-train firing pattern review.

It supports data quality control, interactive annotation, burst/tonic/pause/high-frequency candidate generation, eventness auditing, Mean-ISI and Pasquale logISIH/newBD support analyses, and reproducible export reports.

Event-level validation can compare detector events against MANUAL labels by IoU, report precision/recall/F1, expose boundary errors and label confusions, and scan Basic-layer parameter perturbations. The sensitivity export writes `Parameter_sensitivity_summary.csv`, `Event_level_validation_metrics.csv`, and `Manual_detector_event_matches.csv` so parameter choices can be documented in methods records.

This tool is designed for candidate generation and scientific review, not as an unbiased final ground-truth classifier.

## Simulation Benchmark Artifacts

Curated synthetic validation artifacts are available under `validation/simulation_benchmark/`.
This folder contains detector-visible simulated spike trains, simulator ground truth, selected STPD benchmark outputs, compact figures, and synthetic-only simulator validation summaries.

The package intentionally excludes manuscript drafts, reviewer notes, bulky run objects, large TIFF/PDF exports, and real patient calibration tables.

## Public audit, CSV input, and parameter namespace policy

The event-grammar detector keeps diagnostic candidate windows and public biological calls separate.
Rejected, profile, unwritten, blocked or not-selected windows stay in `Candidate_diagnostic_audit.csv` and are not propagated into `Candidate_ledger.csv`, `Eventness_audit.csv` or final classification exports.

`rejection_reason` is reserved for diagnostic/rejected rows. Public selected candidates should leave it blank; neutral external strings such as `no rejection`, `not rejected` and `not applicable` are treated as neutral.

Raw CSV import expects spike timestamp columns. High-confidence derived tables such as `Sliding_*`, `ISI_base`, `tonic_summary`, `threshold`/`threshould`, `Candidate_ledger`, `Eventness_audit`, `Events_final`, `diagnostic candidate`, `summary`, `result`, `features` and `audit` files are blocked by default. Use `allow_derived_csv=TRUE` only for intentional overrides.

Exact duplicate timestamps are reported by QC. For formal analyses, either use `duplicate_policy="collapse_exact"` or report how duplicates affect ISI, burst/pause, LV/CV/MM and high-frequency metrics.


## Product parameter namespace

Public configuration should use `params$spiketrainpattern`. Compatibility handling for older saved workspaces is resolved by `stpd_productize_params()` before detection and should not be used as a public parameter namespace.

Parameter defaults and contract metadata are YAML-backed. The canonical source is `inst/config/parameters.yml`, which feeds `default_params_sec()`, `stpd_product_schema_defaults()`, `stpd_key_parameter_schema()`, `stpd_parameter_contract()`, the eventness-audit schema rows, and contract-based validation.

The main Shiny detector-parameter tab is generated from the same contract. Specialized workflow controls remain hand-built where they need custom interactions, while scalar detector parameters use the contract-driven `contract_param_` namespace. Contract metadata now separates parameters into Basic, Advanced, and Expert layers; the Shiny page defaults to a biology-friendly Basic layer ordered as QC, burst seed/bridge/contrast, HF spiking, tonic, pause, and arbitration. The tab can export the current UI parameters to YAML, import YAML back into both dedicated and generated controls, show level-aware contract validation issues, preview UI-visible parameter changes before rerunning the detector, run a local dry-run event-difference preview on selected trains, jump from changed-event rows to the aligned raster window, overlay dry-run changed events on the raster and ISI temporal profile, export the preview CSV bundle, and run a YAML hash round-trip check. The Scientific validation tab adds event-level Basic-parameter sensitivity scanning against MANUAL labels.


## Version-neutral internal API

New detector code should use the version-neutral internal API added in `R/50_internal_api.R`, including `stpd_detect_dataset_core()`, `stpd_detect_train_core()`, `stpd_detect_train_event_grammar()`, `stpd_event_grammar_params()`, `stpd_resolve_thresholds_for_dataset()`, and `stpd_candidate_audit_to_ledger()`.

New result consumers should read stable fields such as `results$threshold_table`, `results$candidate_diagnostic_audit`, `results$candidate_features`, `results$final_decisions`, `results$eventness_audit`, and `results$run_metadata_public`. Historical versioned fields are no longer exported in normal detector results; they are used only as private migration sources inside the compatibility layer.


## Productized detection safeguards

The detector runs pre-detection QC before any pattern labels are generated. By default, data-integrity errors such as exact duplicate timestamps, zero-or-negative ISIs, and hard artifact ISIs stop detection before burst, pause, tonic, or high-frequency evidence is computed. For diagnostic continuation, set `params$spiketrainpattern$engine$stop_on_qc_error = FALSE`; for formal analyses, collapse exact duplicates at import or report duplicate prevalence and sensitivity analyses.

Dataset/manual/histogram thresholds are resolved once at the dataset entry point and then frozen for all selected trains in that run. The resolved threshold table is stored in `results$threshold_table`, and the pre-detection QC table is stored in `results$pre_detection_quality`, so exported analyses can audit both the data-integrity gate and the effective threshold policy.

The public `lock_manual` meaning is strict: with `lock_manual = TRUE`, manual labels remain final-label dominant and AUTO labels are not written onto manually labeled ISIs. AUTO evidence can still be generated on unlocked intervals for diagnostic review.
