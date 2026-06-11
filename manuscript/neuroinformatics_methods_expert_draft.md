# 2. Methods

## 2.1 Software implementation and design principles

SpikeTrainPatternDetector was implemented as an R/Shiny software framework for auditable spike-train event review. The detector is intended to generate interpretable candidate events and validation artifacts from spike timestamp data, rather than to serve as an unsupervised biological ground-truth classifier. This distinction guided the method design: every automatic label is linked to a candidate interval, an effective threshold table, a parameter hash, and an audit trail, and literature-linked support methods are kept separate from the project-defined event grammar.

The package is written primarily in R, with Shiny and Plotly used for interactive review and a native C backend used to accelerate repeated low-level operations. In the current implementation, the native layer accelerates per-train ISI percentile calculation, local-median cache calculation, short-ISI run scanning, structural candidate scanning, and interval-overlap calculations. The public analysis workflow is exposed through version-neutral entry points such as `build_spike_dataset()`, `stpd_detect()`, `stpd_export_results()`, and validation functions. Detector parameters are materialized from a YAML-backed parameter contract, which defines default values, parameter namespaces, user-interface metadata, and validation constraints. At runtime, public product parameters are translated into the internal detector namespaces used by the event grammar, allowing backward-compatible saved settings to be resolved while preserving a stable public parameter schema.

The software follows five methodological principles. First, raw spike timing integrity is checked before any pattern evidence is computed. Second, candidate generation and final public calls are separated, so rejected or diagnostic windows cannot be mistaken for accepted biological events. Third, threshold resolution is performed at the dataset entry point and frozen for the selected run, making event calls reproducible from the exported threshold table and parameter hash. Fourth, the main detector is described as a physiologically motivated event grammar, not as a strict implementation of a single published burst definition. Fifth, manual annotations, benchmark labels, and parameter sensitivity analyses are treated as necessary evidence for performance claims.

## 2.2 Input data model

The primary input is a table of spike timestamps. In raw CSV mode, each non-empty numeric column is interpreted as one spike train. Timestamps may be supplied in seconds or milliseconds and are converted internally to seconds. The importer removes missing values, sorts timestamps within each train, records whether the input order was non-monotonic, and recomputes all inter-spike intervals (ISIs) from timestamp differences. Thus, for a train with ordered timestamps

`t_1 < t_2 < ... < t_n`,

the detector defines

`\Delta_i = t_i - t_{i-1}`, for `i = 2, ..., n`,

with `\Delta_1` undefined. All event candidates are represented as contiguous ISI-indexed intervals `[a, b]`, where `2 <= a <= b <= n`. This convention makes event boundaries explicit and allows manual and automatic events to be compared by interval overlap.

In annotated input mode, timestamp columns and manual-label columns can be imported together. External ISI columns, if present, are retained for quality-control checks but are not treated as authoritative; timestamp-derived ISIs are used for detection. This prevents a mismatch between timestamp order and supplied ISI values from silently affecting the detector.

The importer also includes guardrails against accidental re-analysis of derived outputs. Files or columns whose names strongly resemble previously exported detector products, including candidate ledgers, eventness audits, final-event tables, diagnostic candidate tables, summary tables, and threshold tables, are blocked by default in raw import mode. This reduces the risk of recursively treating detector output as raw spike data.

## 2.3 Pre-detection quality control

Quality control is performed before burst, pause, tonic, or high-frequency evidence is computed. For each train, the QC table reports spike count, recording duration, firing rate, raw minimum ISI, minimum valid ISI, duplicate timestamps, zero or negative timestamp steps, zero or negative ISIs, hard artifact ISIs, refractory-suspect ISIs, timestamp-ISI mismatch status, and stationarity diagnostics. The hard artifact threshold is a minimum valid ISI threshold. The default value used by the product schema is 0.0009 s. A second refractory-suspect threshold, default 0.001 s, flags intervals that are above the hard artifact cutoff but remain suspicious for single-unit refractory physiology.

By default, exact duplicate timestamps, zero or negative ISIs, and hard artifact ISIs stop detection before pattern labels are generated. For exploratory diagnosis, users may explicitly allow continuation after QC errors, but formal analyses should either collapse exact duplicates at import or report their prevalence and the results of sensitivity analyses. This policy is biologically important because very short intervals can disproportionately influence burst evidence, local variability metrics, high-frequency state detection, and histogram-based threshold estimates. The detector therefore treats timestamp validity as an upstream scientific condition, not as a downstream cosmetic warning.

Stationarity is handled as an interpretive warning rather than a hard error. Nonstationary firing can make global pause thresholds, train-level ISI percentiles, and tonic-state assumptions less reliable. The package therefore exports stationarity diagnostics and warning messages so that analyses depending on global thresholds can be interpreted in context.

## 2.4 Parameter materialization and threshold resolution

Detector parameters are stored in a YAML-backed contract and materialized at runtime. The public parameter namespace is `spiketrainpattern`, while internal namespaces such as `event_core`, `event_grammar`, `highfreq`, `tonic`, `pause`, and `classification` are derived from the product schema before detection. The detector records a parameter hash and exports parameter reports so that each run can be reproduced and audited.

Thresholds used by the event grammar are resolved once for the selected dataset and then frozen for all selected trains. For each pattern family, candidate threshold values can come from four sources: explicit user settings, manual-label-derived summaries, histogram-derived suggestions, or defaults. The active source policy is resolved before the train-level detector is called. The exported threshold table records, for each pattern and threshold field, the user value, manual-derived value, histogram-derived value, default value, effective value, and selected source.

For a given pattern family `p`, the detector resolves lower seed bounds, upper seed bounds, bridge bounds, and, where relevant, contrast requirements. After source selection, monotonicity and geometry constraints are applied: lower bounds must be non-negative, upper bounds must exceed lower bounds, and bridge bounds must not be lower than seed upper bounds. Additional safeguards prevent high-frequency tonic thresholds from falling into the extreme burst-core band unless explicitly overridden by the user.

This frozen-threshold design serves two purposes. Computationally, it prevents threshold drift across trains within a single run. Scientifically, it makes the effective threshold policy visible to reviewers and allows identical event calls to be regenerated from the exported parameter and threshold records.

## 2.5 Event-grammar detector

The core detector is a deterministic event grammar over ISI-indexed candidate intervals. It is physiologically motivated but project-defined. It combines compact short-ISI seeds, bridge intervals, flank contrast, state regularity, pause gaps, and high-frequency state evidence into an auditable rule system. It should therefore be interpreted as an operational candidate-generation layer, not as a universal biological definition of bursts, pauses, tonic firing, or high-frequency firing.

For each selected train, the detector computes a set of candidate intervals and an evidence vector for each interval. A candidate `c = [a, b]` has within-candidate ISIs `\Delta_a, ..., \Delta_b`, flanking intervals `\Delta_{a-1}` and `\Delta_{b+1}` when available, spike count `b - a + 2`, duration `t_b - t_{a-1}` when timestamps are available, and summary statistics such as within-candidate quantiles, mean ISI, coefficient of variation, local variation, maximum-to-mean ratio, bridge count, bridge fraction, and manual-negative overlap. Candidates are then assigned provisional labels, priorities, and diagnostic decision paths. A weighted interval selection step chooses a non-overlapping set of public automatic calls from the candidate pool.

### 2.5.1 Burst-family detection

Burst-family detection begins with compact seed runs. An ISI is treated as seed-supporting when it falls between the resolved seed lower and seed upper thresholds. Consecutive seed-supporting intervals form seed runs, and a seed run must contain at least the minimum required number of seed ISIs. Each seed run is expanded left and right through a limited number of bridge intervals. Bridge intervals allow small interruptions inside a candidate while preventing a seed from absorbing an entire high-rate or nonstationary segment. Expansion is constrained by the maximum number of bridge ISIs, the bridge fraction, and the maximum number of expansion steps on either side.

For each expanded candidate, the detector computes an intra-event compactness reference, primarily based on the within-candidate 90th and 95th ISI percentiles. Flank contrast is computed from the pre-event and post-event ISIs relative to the intra-event compactness. Conceptually, for candidate `c`,

`S_pre(c) = \Delta_{a-1} / Q90(c)` and `S_post(c) = \Delta_{b+1} / Q90(c)`,

where `Q90(c)` is the 90th percentile of valid ISIs inside the candidate. Two-sided canonical burst evidence requires both flanks to exceed the resolved contrast threshold. Candidates with partial or edge-limited evidence can be retained as `possible_burst` rather than discarded, preserving ambiguous but biologically plausible intervals for review.

The burst-family detector includes an optimized structural-rescue route for compact short-ISI episodes. This route preserves candidates that show strong compression relative to the train-scale background even when classical two-sided flanks are weak or unavailable. Such candidates are accepted as burst or long_burst only when their structure is sufficiently strong under the configured rules; otherwise, they are retained as possible_burst review candidates. The detector uses a soft q95 bridge policy by default: modest q95 overflow penalizes the candidate score rather than automatically rejecting it, whereas severe overflow can still block acceptance.

Spike-count criteria distinguish classical burst, long_burst, and prolonged burst-like candidates. Default product settings treat 3-10 spikes as classical burst scale and 11-15 spikes as long_burst scale, while longer compact structures are generally demoted to possible_burst unless additional study-specific criteria justify accepting them. This conservative treatment avoids conflating long high-rate epochs with discrete burst events.

### 2.5.2 High-frequency spiking

High-frequency spiking is modeled as a sustained state or epoch, not as a burst-family event. This is a central biological distinction in the detector. A high-frequency spiking candidate is built from sustained support runs in which most ISIs are short, while allowing a limited number of moderate gaps. The default product settings require at least 30 spikes, a short-ISI upper reference of 0.020 s, an epoch-level q90 limit of 0.025 s, a bridge allowance of 0.035 s, a tolerated gap of 0.075 s, and limits on the fraction and number of consecutive larger ISIs.

The detector evaluates median, q80, q90, and q95 ISI summaries, short-ISI fractions, bridge fractions, tolerated-gap fractions, and large-ISI burden. Acceptance can occur through a strict q90 route or through a robust q80/majority route, reflecting the fact that a biologically sustained high-frequency state can contain occasional moderate intervals without ceasing to be high-frequency. Candidate scoring gives long high-frequency states span-aware priority so that they are not fragmented into many low-specificity possible_burst, tonic, or pause candidates. However, putative high-frequency spiking states that are dominated by many embedded burst packets can be rejected, allowing burst-family events to remain visible when the evidence favors packetized bursting rather than a sustained state.

### 2.5.3 High-frequency tonic and tonic states

High-frequency tonic and tonic candidates represent relatively regular firing regimes. High-frequency tonic detection uses a high-frequency ISI upper bound while applying a lower floor to avoid labeling extreme burst-core intervals as tonic-like high-frequency discharge. It also applies regularity checks based on coefficient of variation, local variation, maximum-to-mean ratio, and vetoes against dominant burst-core runs.

Tonic detection uses an adaptive mid-ISI band derived from configured tonic bounds and train-level ISI summaries. A tonic candidate must satisfy spike-count requirements and regularity constraints and must not be dominated by burst-core ISIs. The detector also uses burst-overlap safeguards so that a short-ISI burst sequence embedded within a broader candidate is not misinterpreted as tonic regularity. These tonic and high-frequency tonic labels should be interpreted as operational state annotations whose biological meaning depends on cell type, preparation, spike sorting quality, and experimental context.

### 2.5.4 Pause detection

Pause candidates are long-gap intervals. The pause layer combines an absolute pause threshold with train-level context. The effective pause floor is constrained by the train's upper ISI distribution and by tonic-state guardrails, and candidate long intervals can be further checked against local and global median ISI context. This relative design reduces the chance that ordinary slow firing is mislabeled as pause while preserving long isolated gaps as reviewable events.

Because pause detection is particularly sensitive to nonstationarity and long silent tails, the detector exports pause threshold fields, local median context, global median context, and stationarity warnings. A pause label should therefore be interpreted together with the QC table and threshold table.

## 2.6 Weighted interval selection and manual-label policy

Candidate generation may produce overlapping intervals from different layers. To construct a single public automatic label track, SpikeTrainPatternDetector applies weighted interval selection. Each candidate receives a value determined by its label family, explicit priority, score, and span. The algorithm then selects a non-overlapping subset of intervals that maximizes total value under the configured pattern set. This avoids arbitrary first-come selection and makes competition between burst, possible_burst, high-frequency, tonic, and pause candidates deterministic.

Manual labels are handled separately from automatic evidence. When manual locking is enabled, manually labeled intervals remain final-label dominant and automatic labels are not written over those ISIs. At the same time, automatic evidence can still be generated and audited in diagnostic layers. This design supports semi-supervised workflows: expert annotations are protected, but the detector can still reveal where the algorithm would have proposed candidates and where manual and automatic evidence disagree.

Negative manual labels, such as explicit not-burst labels, can veto candidate acceptance in the corresponding interval. Such vetoes are recorded in the diagnostic audit rather than silently removing the candidate history.

## 2.7 Candidate ledgers and eventness audit

The detector separates public candidate calls from diagnostic candidate windows. Accepted, selected intervals are exported to public event and candidate ledgers. Rejected, profile-only, blocked, suppressed, or not-selected windows remain in the diagnostic candidate audit. This separation is essential for reproducibility: public biological counts are not inflated by internal candidate windows, while rejected candidates remain available for failure-mode inspection.

After public candidates are selected, the software computes a candidate feature table used by the final classification audit and downstream reports. Candidate features include event duration, spike count, within-event ISI quantiles, mean and maximum ISI, coefficient of variation, local variation, maximum-to-mean ratio, flanking ISIs, edge contrast, bridge count, bridge fraction, local context summaries, and state-specific evidence fields.

The eventness audit provides an additional interpretive layer. It estimates whether a candidate is event-like or state-like by combining boundary contrast, context contrast, return-to-baseline evidence, and regularity. Regularity is summarized from coefficient of variation, local variation, and the q90/q10 ISI ratio. Eventness is not used as an independent biological truth measure; it is an audit feature that helps distinguish compact, boundary-delimited events from sustained states. Long_burst candidates receive additional checks for context contrast, short-ISI fraction, duration, and internal outlier burden. Ambiguous eventness zones are explicitly marked for review.

## 2.8 Literature-linked support methods

The main event grammar is project-defined, but SpikeTrainPatternDetector also provides literature-linked support layers for threshold evidence and comparison. These support methods do not overwrite main automatic labels unless a user intentionally uses their outputs in a downstream workflow.

The Mean-ISI support layer follows the threshold principle of Chen et al. (2009). Given valid ISIs `T_i`, the train-level mean ISI is first computed. The mean-ISI threshold is then

`ML = mean({T_i : T_i < mean(T)})`.

Candidate burst windows are identified when the mean ISI of consecutive intervals in the window does not exceed `ML`. The package implementation enumerates consecutive windows, applies spike-count and duration constraints, and merges overlapping windows. The threshold principle is literature-linked; exhaustive enumeration, budget controls, and merge policies are implementation choices and are reported as such.

The logISIH/newBD support layer implements a Pasquale-style threshold-evidence workflow. ISIs are represented as `log10(ISI_ms)`, a logISI histogram is constructed, smoothed, and searched for peaks and valleys, and void-parameter evidence is used to support threshold resolution. If a threshold cannot be resolved reliably, the method reports the failure status rather than silently producing a final biological label. The output is intended for threshold calibration, visualization, and comparison with the event grammar.

## 2.9 Event-level validation against manual labels

When manual labels are available, automatic events are compared with manual events using ISI-indexed interval overlap. For an automatic interval `A = [a_1, a_2]` and a manual interval `M = [m_1, m_2]`, the intersection length and union length are computed over inclusive ISI indices, and the intersection-over-union is

`IoU(A, M) = |A intersect M| / |A union M|`.

Candidate/manual pairs are greedily matched in descending IoU subject to a minimum overlap criterion. From these matches, the package reports precision, recall, F1, false positives, false negatives, boundary errors, and label-confusion summaries. Two evaluation modes are provided. Strict high-confidence evaluation preserves specific labels and is appropriate when manual events are intended as class-specific ground truth. Candidate-family evaluation groups related labels, such as burst and possible_burst, and is appropriate when the goal is to assess retrieval of biologically plausible event families rather than exact subtype assignment.

The package also implements parameter sensitivity scanning. Basic-layer parameters can be perturbed and the full event-level validation recomputed. The exported sensitivity reports document how precision, recall, F1, IoU, boundary error, and confusion structure change under parameter perturbations. These outputs are intended for methods records and to reduce the risk of reporting a narrowly tuned parameter set without robustness evidence.

## 2.10 State-space and population exploratory modules

SpikeTrainPatternDetector includes optional exploratory modules for ISI state-space analysis, neural manifold visualization, event-aligned activity, and slice tensor construction. These modules are not required for the core detector and should not be used as sole evidence that detected events represent biological ground truth.

For single-train state-space analysis, the package computes ISI-derived features including logISI, local median ISI, local mean ISI, coefficient of variation, local variation, CV2, lagged ISI features, and local context summaries. CV is computed as `sd(ISI) / mean(ISI)` for valid positive ISIs. LV is computed as

`mean(3 * (T_i - T_{i+1})^2 / (T_i + T_{i+1})^2)`,

and CV2 is computed as

`mean(2 * |T_i - T_{i+1}| / (T_i + T_{i+1}))`

over adjacent valid ISI pairs. Linear and nonlinear embeddings, including PCA, Isomap, diffusion-map-style embeddings, PHATE, UMAP, and t-SNE, are provided as visualization and hypothesis-generation tools. Their outputs should be reported with parameter settings, random seeds where relevant, and stability or shuffle controls.

For population-level exploration, spike trains can be binned into a time-by-neuron matrix using counts or rates, with optional transformations such as log1p or square-root count transforms. Dimensionality reduction can then be applied and event labels overlaid as annotations rather than as inputs to the embedding. This ordering is important to avoid circular inference. Event-state centroid distances, dispersions, permutation tests, event-triggered trajectories, latent velocity, curvature, and decoding analyses can be used to ask whether event states explain structure in population activity beyond visual separation alone.

The optional sliceTCA workflow constructs trial-by-neuron-by-time tensors from selected spike trains and task events. When the required Python environment is available through reticulate, the R package can call the official Python sliceTCA backend. Otherwise, the package exports tensor diagnostics and reduced summaries. Studies using this module should report tensor dimensions, bin width, event windows, rank settings, optimization parameters, random seed, reconstruction metrics, and backend versions.

## 2.11 Reproducibility outputs and reporting

Each detector run exports enough metadata to reproduce and audit the analysis. Public result fields include the effective threshold table, pre-detection quality table, candidate diagnostic audit, candidate ledger, event table, candidate feature table, final decision audit, eventness audit, parameter report, parameter-validation report, run metadata, and manual-label validation summaries when labels are available. The exported run metadata include the parameter hash and selected trains. Result consistency checks and scientific validation summaries are generated to identify mismatches between event tables, candidate ledgers, and final decision layers.

For publication use, we recommend reporting the package version, repository release tag or DOI, R version, operating system, dependency versions, input unit convention, duplicate handling policy, hard artifact threshold, refractory-suspect threshold, threshold-source policy, full parameter file or parameter hash, number of trains analyzed, number of spikes per train, QC warnings, effective threshold table, and validation mode. If detector outputs are used for biological inference rather than software demonstration, manual-label agreement, inter-rater agreement when available, parameter sensitivity, and comparison with literature-linked support methods should be reported.

## 2.12 Methodological positioning

The method is best understood as an auditable candidate-generation and validation framework. A burst call indicates a compact short-ISI structure with sufficient local contrast under the chosen threshold policy. A pause call indicates a long-gap interval under the chosen pause and context rules. Tonic and high-frequency calls indicate state-like temporal regimes defined by local rate and regularity evidence. These labels can be scientifically useful, but their interpretation depends on preparation, cell type, spike sorting quality, brain region, disease or stimulation state, behavior, and the validation data available for the study. Accordingly, strong claims about detector accuracy or biological mechanism should be supported by manual annotation, synthetic benchmarks, independent physiological or behavioral endpoints, shuffle controls, and parameter-sensitivity analyses.
