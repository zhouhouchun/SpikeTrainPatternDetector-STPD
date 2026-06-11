# SpikeTrainPatternDetector: an auditable R/Shiny framework for spike-train event review and validation

**Article type:** Technology and Code  
**Target journal:** Frontiers in Neuroinformatics  
**Running title:** Auditable spike-train event review  

**Authors:** Houchun Zhou1*  
**Affiliations:**  
1. [Institution, Department, City, Country - to be completed before submission]  

**Correspondence:**  
Houchun Zhou, zhouhouchun@outlook.com

**Keywords:** spike train, burst detection, pause detection, event grammar, neural manifold, dimensionality reduction, Shiny, reproducible neuroinformatics

## Abstract

Spike-train event labeling requires a compromise between physiologically interpretable states and reproducible computational rules. We present SpikeTrainPatternDetector, an R/Shiny framework for quality-controlled spike-train event review rather than an unsupervised ground-truth classifier. The workflow performs pre-detection quality control, applies a literature-inspired and engineering-calibrated event grammar for reviewable burst-family, pause, tonic, and high-frequency candidates, and keeps Mean-ISI and Pasquale logISIH/newBD analyses as separate literature-linked support layers. The software exports traceable candidate ledgers, diagnostic audits, parameter-hashed run metadata, eventness features, threshold tables, and manual-label validation reports based on interval overlap, precision, recall, F1, boundary error, and parameter sensitivity. Optional exploratory modules support ISI state-space visualization, population manifold inspection, and sliceTCA tensor construction, but these modules are not used as evidence of biological ground truth without independent validation. A bundled smoke-test dataset demonstrates the end-to-end workflow on four spike trains with 120 spikes each, yielding 116 reviewable AUTO events and 185 diagnostic candidate-audit rows. The framework is intended to make spike-train pattern analysis inspectable, reproducible, and testable through manual labels, benchmark data, shuffles, sensitivity scans, and held-out decoding analyses.

## 1. Introduction

Action potentials are discrete events, but many neuroscientific questions are asked at the level of firing-pattern states. In basal ganglia physiology, cortical network recordings, and other spike-train settings, investigators often describe intervals of burst firing, pauses, tonic discharge, high-frequency packets, or transitions between these regimes. These descriptions are scientifically useful because they connect single-neuron timing to hypotheses about synaptic drive, circuit state, pathology, stimulation, and behavior. They are also methodologically fragile. The same spike train can support different event labels depending on the minimum ISI considered physiologically valid, whether timestamps contain duplicates or refractory-suspect intervals, whether a global threshold is applied to a nonstationary train, and whether the algorithm is designed to be permissive for candidate review or conservative for final inference.

Several established methods address parts of this problem. The coefficient of variation (CV), local variation (LV), and CV2 summarize complementary aspects of ISI variability, with CV reflecting global dispersion, LV and CV2 emphasizing adjacent-interval changes, and Mean-ISI or logISI approaches providing adaptive burst-threshold support (Holt et al., 1996; Shinomoto et al., 2005; Chen et al., 2009; Pasquale et al., 2010). Population-level methods add another layer: PCA and factor analysis (FA) offer transparent linear baselines, Gaussian-process factor analysis (GPFA) links smoothing and dimensionality reduction in a probabilistic single-trial framework, and nonlinear visualization methods such as Isomap, t-SNE, UMAP, and PHATE can reveal neighborhood structure or continuous transitions while requiring careful validation (Tenenbaum et al., 2000; Yu et al., 2009; Cunningham and Yu, 2014; McInnes et al., 2018; Moon et al., 2019). More recent methods such as CEBRA use behavioral variables and contrastive learning to learn behaviorally relevant latent spaces, while sliceTCA decomposes trial-by-neuron-by-time tensors beyond traditional neural subspaces (Schneider et al., 2023; Pellegrino et al., 2024).

Despite this rich methodological landscape, everyday spike-train review still faces a practical gap. Investigators need software that can import raw timestamp data, block accidental re-analysis of derived outputs, perform pre-detection quality control, generate interpretable candidate labels, preserve manual annotations, expose failure modes, document parameter choices, compare automatic calls with manual labels, and explore how event states relate to low-dimensional neural trajectories. A useful detector for this setting should not pretend that all biological states have universal formulas. It should instead distinguish literature-defined statistics and support methods from project-defined event grammar, make both inspectable, and produce validation artifacts that allow scientific users to judge whether the rules are appropriate for their dataset.

SpikeTrainPatternDetector was developed for this purpose. The package provides an R/Shiny platform for spike-train candidate-event generation and audit-oriented analysis. Its core detector is a literature-inspired and engineering-calibrated event grammar: it uses biologically interpretable concepts such as compact short-ISI seeds, bridge intervals, flank contrast, pause gaps, tonic regularity, and high-frequency episodes, but it does not claim to be a strict reproduction of a single published burst formula. In contrast, its support layers for Mean-ISI and Pasquale logISIH/newBD are implemented as literature-linked threshold-evidence modules. This separation is deliberate. It allows the main workflow to serve as a transparent review engine while keeping classical support methods available for comparison, reporting, and parameter calibration.

Here we describe the software architecture, algorithmic logic, validation outputs, optional exploratory modules, and example use of SpikeTrainPatternDetector. We frame the package as a Technology and Code contribution for neuroinformatics: a reproducible, human-readable, open software implementation designed to support spike-train analysis rather than to replace expert scientific judgment.

## 2. Method

### 2.1 Software availability and implementation environment

SpikeTrainPatternDetector is an R package with a Shiny graphical interface. The local version used for this manuscript draft is version 1.2.1. The package imports Shiny, Plotly, DT, tidyverse-style table tools, YAML, digest, base R statistics and graphics utilities, and a compiled native C backend. Optional suggested dependencies include testthat, ggplot2, nnet, data.table, lme4, phateR, Rtsne, uwot, and reticulate. The package license is MIT with a package LICENSE file.

For final Frontiers submission, the project should be deposited in a stable public repository with a persistent DOI or URI. The manuscript-ready software metadata should be completed as follows:

**Project link:** [GitHub/Zenodo DOI to be inserted before submission]  
**Current package URL:** https://www.spiketrain.studio  
**Operating system:** platform independent for the R/Shiny core, subject to the availability of R package dependencies and native compilation tools.  
**Programming language:** R, C, Shiny, with optional Python backend for sliceTCA through reticulate.  
**License and non-academic restrictions:** MIT license; no additional non-academic use restriction is currently specified beyond dependency licenses.  
**Version reported here:** 1.2.1.

The software is organized into modular R source files rather than a monolithic script. Data import, quality control, detector helpers, event grammar, support methods, validation, state-space analysis, manifold analysis, sliceTCA integration, UI, and server logic are separated. A native C backend accelerates per-train ISI percentile calculation and local-median cache calculation, two operations repeatedly used by visualization, local compression, candidate generation, and pause context estimation.

### 2.2 Data model and input guardrails

The primary input is raw spike timestamp data. In raw CSV mode, each column is interpreted as one spike train, and timestamps can be supplied in seconds or milliseconds. The importer sorts timestamps, computes ISIs from timestamp differences, records whether the input was unsorted, counts exact duplicates, and records zero or negative timestamp steps. In annotated CSV mode, timestamp and annotation columns can be loaded, but timestamp-derived ISIs are treated as authoritative and external ISI columns are retained for quality control rather than trusted blindly.

A key design choice is that derived result files are blocked by default during raw import. Tables with names suggesting previous exports, such as sliding summaries, ISI-base tables, threshold tables, candidate ledgers, eventness audits, final events, diagnostic candidate tables, summary tables, result tables, feature tables, or audit files, are treated as high-confidence derived outputs. Users can override this only intentionally. This guardrail reduces the risk of recursively treating detector output as raw spike data.

### 2.3 Pre-detection quality control

Quality control is performed before pattern labels are generated. The QC table reports, for each train, the number of spikes, recording duration, firing rate, raw minimum ISI, minimum valid ISI, hard artifact interval count, refractory-suspect interval count, exact duplicate timestamps, zero or negative ISIs, input-order problems, timestamp-ISI mismatch status, and stationarity diagnostics. By default, data-integrity errors such as exact duplicate timestamps, zero or negative ISIs, or hard artifact ISIs stop detection before burst, pause, tonic, or high-frequency evidence is computed. For exploratory diagnosis, users can choose to continue after QC warnings, but formal analyses should collapse exact duplicates at import or report duplicate prevalence and sensitivity analyses.

This QC-first design is important because many firing-pattern statistics are highly sensitive to very short intervals. Exact duplicate timestamps create zero ISIs, and a single implausibly short interval can inflate local burst evidence, local variation, high-frequency metrics, or threshold estimates. SpikeTrainPatternDetector therefore treats timestamp validity as an upstream scientific issue rather than as a nuisance handled after detection.

### 2.4 Core event grammar

The core event grammar generates candidate intervals over ISI-indexed spike-train data. The grammar is project-defined: it is inspired by known physiological patterns and established ISI statistics, but the exact seed, bridge, flank, scoring, and arbitration rules are engineering-calibrated rules within this software. We therefore describe the detector as literature-inspired and engineering-calibrated, not as a strict reproduction of a single published formula.

The grammar uses the following conceptual layers.

**Burst-family seeds.** Compact sets of short ISIs form potential burst seeds. A seed is expanded through limited neighboring intervals when they satisfy bridge criteria. The expansion is constrained by the number and fraction of bridge ISIs so that isolated short intervals do not automatically absorb long surrounding regions.

**Bridge intervals.** Bridge intervals allow a burst candidate to include small interruptions without fragmenting a biologically coherent high-frequency episode. The maximum bridge count and bridge fraction are explicit parameters and are recorded in parameter reports.

**Boundary contrast.** Candidate burst-family intervals are evaluated against flanking ISIs. A boundary contrast score compares the pre-event and post-event gaps with the compactness of intra-event ISIs. This prevents short-ISI clusters embedded in uniformly fast tonic firing from being treated exactly like isolated classical bursts.

**Possible bursts and long bursts.** Candidate intervals that show partial burst evidence but fail stricter criteria can be retained as possible_burst candidates. Longer burst-family events can be represented separately when they satisfy duration and structural criteria.

**Pause states.** Long ISIs and pause-like gaps are detected relative to train-specific and dataset-level thresholds. The pause layer is sensitive to stationarity and therefore requires QC warnings and threshold reports to be reviewed carefully.

**Tonic and high-frequency states.** Tonic and high-frequency tonic states describe intervals with relatively regular or sustained firing. High-frequency spiking states capture fast packets that may not satisfy burst-family structure. These rules are useful for state-space exploration and review but should be validated against manual labels or study-specific operational definitions before being used as inferential endpoints.

**Manual-lock arbitration.** When manual labels are present, the public lock_manual mode gives manual labels final-label dominance on manually labeled intervals. AUTO evidence can still be generated for diagnostic review on unlocked intervals, but automatic labels are not written over manually labeled ISIs when manual locking is enabled.

**Table 1. Core event-grammar specification.**

| Rule block | Key parameter(s) and default(s) | Unit/type | Rationale | Audit output and main failure mode |
|---|---|---|---|---|
| QC hard gate | artifact_min_valid_isi_sec = 0.0009 | s | Exclude implausibly short intervals before event evidence is computed | pre_detection_quality; overly strict gates can remove true very short intervals in multiunit/high-rate recordings |
| Refractory-suspect guard | refractory_suspect_isi_sec = 0.001; action = demote_to_possible | s; action | Flag intervals suspicious for single-unit refractory physiology | candidate_diagnostic_audit; true short-latency spikes and multiunit contamination can be difficult to separate |
| Burst seed band | seed_lower_sec = 0.001; seed_upper_sec = 0.010; min_seed_isi_count = 2 | s; ISI count | Detect compact short-ISI cores | candidate_features; high-rate tonic epochs may create false seeds |
| Burst bridge band | bridge_upper_sec = 0.015; max_bridge_isi_count = 4; max_bridge_isi_fraction = 0.6 | s; count; fraction | Avoid fragmenting coherent burst-family events because of small internal interruptions | candidate_diagnostic_audit; excess bridge allowance can over-merge neighboring events |
| Boundary contrast | contrast_min = 2.5; possible_contrast_min = 2.0 | ratio | Distinguish isolated burst-family events from uniformly fast firing | eventness_audit; weak flanks in nonstationary trains can demote true events |
| Local expansion | max_expansion_isi_each_side = 4 | ISI count | Permit limited boundary adjustment around seed windows | candidate_features; expansion can absorb neighboring states if local context is unstable |
| One-sided candidates | allow_one_sided_possible = yes; one_sided_seed_purity_min = 0.65 | logic; fraction | Retain reviewable edge or partial candidates without overclaiming canonical bursts | candidate_diagnostic_audit; can increase possible_burst calls near recording edges |
| High-frequency spiking | min_spikes = 30; short_isi_upper_sec = 0.020; q90_isi_max_sec = 0.025 | spike count; s | Capture sustained fast spiking packets that may not satisfy burst contrast | final_decisions; may overlap with high-rate tonic discharge |
| High-frequency tonic | min_spikes = 6; min_isi_floor_sec = 0.010; max_isi_sec = 0.030 | spike count; s | Represent sustained high-frequency but relatively regular epochs | final_decisions; burst cores embedded in tonic-like segments can be confused |
| Tonic | min_isi_sec = 0.020; max_isi_sec = 0.060; lv_max = 0.5; mm_max = 1.25 | s; dimensionless | Identify relatively regular firing regimes | final_decisions; slowly drifting trains can mimic tonic regularity |
| Pause | min_isi_sec = 0.100; max_isi_sec = 0.150; bridge_upper_sec = 0.150 | s | Detect long-gap intervals | threshold_table; nonstationarity or long silent intervals can shift pause baseline |
| Manual arbitration | honor_manual_lock_for_auto = yes | logic | Preserve expert labels as final-label dominant intervals | event_level_validation; incomplete manual labeling can leave unlabeled intervals for AUTO review |

Algorithm 1. Conceptual event-grammar pseudocode.

1. Import raw or annotated spike timestamps, sort timestamps, recompute ISIs from timestamps, and block derived result files unless explicitly overridden.
2. Run pre-detection QC; stop before event evidence if hard data-integrity errors are present under the active QC policy.
3. Resolve dataset, manual, histogram, and default thresholds once at the dataset entry point, then freeze the effective threshold table for the run.
4. For each selected train, scan ISI-indexed windows for compact seed intervals satisfying the seed band and minimum seed-count rules.
5. Expand seed windows through limited bridge ISIs, subject to maximum bridge count, bridge fraction, and local expansion constraints.
6. Compute within-event compactness, flanking gaps, boundary contrast, bridge burden, local variability, and state-family evidence.
7. Demote, reject, or retain candidates according to tonic-like vetoes, refractory-suspect policy, one-sided edge rules, and candidate-family thresholds.
8. Run pause, tonic, high-frequency tonic, and high-frequency spiking state rules over the same ISI-indexed train.
9. Apply manual-lock arbitration so manual labels remain final-label dominant where present.
10. Export selected candidates to public ledgers and rejected or diagnostic windows to Candidate_diagnostic_audit.csv, preserving parameter hashes and threshold tables for audit.

### 2.5 Literature-linked support layers

SpikeTrainPatternDetector separates its project-defined core grammar from literature-linked support methods.

#### 2.5.1 Mean-ISI support

The Mean-ISI support layer follows the threshold principle described by Chen et al. (2009). For a train with valid ISIs T_i, the global mean ISI is first computed. The Mean-ISI threshold ML is then defined as the mean of the ISIs below the global mean:

ML = mean({T_i : T_i < mean(T)}).

Candidate burst windows are identified when the mean of consecutive ISIs in the window is not larger than ML. The package implementation enumerates consecutive windows, applies spike-count and duration constraints, and merges overlapping candidate windows. The threshold principle is literature-linked; the exhaustive window enumeration, budget controls, and merge rules are implementation choices and should be reported as such.

#### 2.5.2 Pasquale logISIH/newBD support

The logISIH/newBD support layer implements a Pasquale-style logISI threshold-evidence workflow for burst and network-burst support (Pasquale et al., 2010). Internally, ISIs are represented as log10(ISI_ms), matching the convention used by the support method. The support layer computes a logISI histogram, applies smoothing, searches for peaks and valleys, evaluates void-parameter evidence, and exports newBD-style threshold support. The support output is treated as evidence for threshold calibration and visualization rather than as an automatic final label source.

This distinction matters for reproducible reporting. If a study uses the main event grammar, it should report that the detector is a review-oriented grammar. If a study uses Mean-ISI or logISIH/newBD support outputs, it should specify whether those outputs were used for threshold evidence, visual overlay, or formal event calling.

### 2.6 Candidate ledgers, diagnostic audit, and eventness features

The detector keeps diagnostic candidates separate from public biological calls. Rejected, blocked, profile-only, not-selected, or otherwise diagnostic windows remain in Candidate_diagnostic_audit.csv. Selected candidates propagate to Candidate_ledger.csv, Eventness_audit.csv, and final event exports. This separation reduces a common reproducibility problem in which exploratory windows are accidentally interpreted as final biological calls.

For each selected candidate, feature tables report event geometry and ISI context. Features include event duration, spike count, within-event ISI summaries, local variability, flanking contrast, bridge burden, and state-specific scoring evidence. The final classification audit records how candidates passed or failed final decision rules. Parameter reports include the effective values and parameter hash used in the run.

### 2.7 Event-level validation against manual labels

When manual labels are available, SpikeTrainPatternDetector compares AUTO events against manual events using interval overlap. Each candidate/manual pair can be scored by intersection-over-union (IoU) over ISI-indexed intervals. Greedy one-to-one matching is then performed by descending IoU subject to a minimum overlap criterion. The resulting validation outputs include precision, recall, F1, false positives, false negatives, boundary errors, and label confusions. The package supports strict high-confidence evaluation and broader candidate-family evaluation, allowing investigators to distinguish exact label agreement from family-level candidate retrieval.

Parameter sensitivity scanning perturbs Basic-layer detector parameters and recomputes event-level agreement against manual labels. Exports include parameter sensitivity summaries, event-level validation metrics, and manual-detector event matches. These files are intended for methods records, parameter justification, and transparent reporting of how robust conclusions are to threshold choices.

### 2.8 State-space analysis from ISI-derived features

For single-train state-space visualization, the package computes ISI-derived features such as logISI, local median ISI, local mean ISI, CV, LV, CV2, lagged features, and local context summaries. CV is computed as sd(ISI)/mean(ISI) for valid positive ISIs. LV is computed from adjacent intervals as mean(3*(T_i - T_{i+1})^2/(T_i + T_{i+1})^2). CV2 is computed as mean(2*abs(T_i - T_{i+1})/(T_i + T_{i+1})) across adjacent valid ISI pairs. PCA is computed using R's stats::prcomp after the selected features are centered/scaled according to the panel settings. Isomap uses a nearest-neighbor graph, shortest-path geodesic distances, and metric multidimensional scaling. Additional state-dynamics tools include transition matrices, transition entropy, diffusion-map style embeddings, PHATE support through phateR when available, and recurrence quantification analysis with the main diagonal excluded by convention.

### 2.9 Optional Neural Manifold module

The Neural Manifold module is an optional exploratory layer for simultaneously recorded spike trains or sets of spike trains that can be interpreted as a population. It converts selected trains into a time-by-neuron matrix using binned spike counts or rates. Transform options include raw counts, rates, log1p-transformed rates, and the square-root-style count transform sqrt(count + 3/8), which stabilizes Poisson-like count variance in exploratory analyses. Optional smoothing and scaling are applied before dimensionality reduction. This module is not the primary evidence for detector validity in the current software manuscript; it is a hypothesis-generating extension that requires held-out decoding, shuffle controls, and population/behavior data before biological claims are made.

The module includes the following method families.

**PCA.** PCA is used as a transparent linear baseline. It provides loadings, coordinates, and explained variance. PCA is not treated as evidence of a nonlinear manifold; it is used to ask whether a low-dimensional linear projection summarizes major variance.

**Factor analysis.** FA models shared latent variability and private noise, making it more aligned with neural population analysis than plain PCA when the goal is to separate shared structure from neuron-specific noise. The implementation provides an exploratory FA embedding through R's statistical machinery and reports method notes for publication use.

**Smoothed-FA trajectory preview.** The smooth-trajectory option smooths binned population activity and then applies FA-like embedding. It is explicitly reported as a smoothed FA preview, not as a full GPFA expectation-maximization implementation with Gaussian-process latent priors. Publication-grade GPFA claims should use a full GPFA implementation or dedicated probabilistic latent trajectory software.

**Nonlinear embeddings.** Isomap, PHATE, UMAP, and t-SNE are available for visualization. These methods can be useful for revealing candidate structure, local neighborhoods, branches, or continuous transitions, but they should not be used alone as proof that a true neural manifold has been discovered. The module therefore reports trustworthiness, continuity, seed stability guidance, and shuffle-control recommendations.

**Behavior-guided supervised projection.** When a numeric behavior variable is available, the module can compute a lightweight behavior-guided projection proxy. It should not be interpreted as the official CEBRA contrastive-learning model. For final supervised manifold claims, the manuscript should use the official CEBRA package and report held-out behavior decoding and embedding consistency.

**Event-geometry validation.** The module computes event-state centroids and dispersions in 3D latent space, distances between burst and pause regions, permutation p-values, circular time-shift controls, event-triggered 3D trajectories, latent velocity and curvature around event onset, event-label decoding from manifold coordinates, behavior decoding with and without event labels, and sensitivity to bin width, smoothing sigma, and embedding seed. These metrics operationalize a central scientific question: whether burst/pause labels occupy distinct low-dimensional regions or simply decorate a continuous trajectory without predictive value. In the absence of a population recording and behavior-linked validation dataset, these outputs should be reported as exploratory software capabilities rather than as empirical manifold evidence.

### 2.10 sliceTCA integration

The package includes an optional sliceTCA workflow inspired by slice tensor component analysis (Pellegrino et al., 2024). The R layer builds a trial-by-neuron-by-time tensor from selected spike trains, event or trial times, and time windows. Counts can be transformed and scaled, and event annotations such as burst, pause, tonic, or high-frequency states are attached to tensor bins. When reticulate, numpy, torch, and the official Python slicetca package are available, the backend can call the official Python implementation. If the backend is not installed or the user disables Python execution, the module still returns tensor summaries, diagnostics, and PCA-style tensor embeddings for review.

The package does not vendor torch or slicetca binaries inside the R package because those dependencies are large and platform-specific. Instead, it provides installation and backend-status functions so that users can create a dedicated Python environment and verify module availability. This design is more sustainable for distribution and avoids hiding heavy external dependencies inside the R source tree.

## 3. Results

### 3.1 Software workflow and audit artifacts

SpikeTrainPatternDetector implements a reproducible pipeline from raw timestamps to reviewable event labels and validation reports. A typical run proceeds through:

1. raw spike timestamp import or annotated import;
2. QC and optional duplicate-collapse policy;
3. dataset/manual/histogram threshold resolution;
4. core event-grammar candidate generation;
5. support-layer Mean-ISI or logISIH/newBD threshold evidence;
6. candidate feature extraction and final decision audit;
7. event-level manual validation when labels exist;
8. state-space, neural manifold, and optional tensor analyses;
9. export of candidate ledgers, diagnostic audits, parameter reports, validation metrics, and reproducible result packages.

The software stores stable public result fields, including threshold_table, candidate_diagnostic_audit, candidate_ledger, candidate_features, final_decisions, eventness_audit, run_metadata_public, result_consistency, and event-level validation summaries. This organization allows a reviewer to trace a final event label back to the candidate window, feature evidence, threshold policy, and parameter hash that produced it.

### 3.2 Example smoke test on bundled data

To verify the package-level workflow, we ran SpikeTrainPatternDetector version 1.2.1 on the bundled raw CSV file inst/extdata/Grechishnikova_STN_2017_subset.csv. This example is a software smoke test and should not be interpreted as a clinical or disease-physiology result. The dataset contains four spike trains with 120 spikes each. Pre-detection QC generated four train-level QC rows. The detector produced 116 public AUTO candidate events: 67 burst events, 43 pause events, and 6 possible_burst events. The public candidate ledger contained 116 rows, the diagnostic candidate audit contained 185 rows, and the candidate-feature table contained 116 rows.

**Table 2. Smoke-test output summary.**

| Item | Value |
|---|---:|
| Package version | 1.2.1 |
| Input file | inst/extdata/Grechishnikova_STN_2017_subset.csv |
| Number of spike trains | 4 |
| Spikes per train | 120, 120, 120, 120 |
| QC rows | 4 |
| Public AUTO events | 116 |
| Burst events | 67 |
| Pause events | 43 |
| possible_burst events | 6 |
| Candidate ledger rows | 116 |
| Diagnostic candidate-audit rows | 185 |
| Candidate-feature rows | 116 |

The same smoke test also illustrates why QC must precede interpretation. Some trains showed stationarity warnings, and two trains contained refractory-suspect ISIs below 1 ms but above the hard artifact threshold used in the test run. These warnings do not invalidate the software pipeline, but they would need to be reported and examined in a biological analysis.

### 3.3 Validation-oriented outputs

The package exports both strict and candidate-family validation modes. Strict validation is appropriate when manually labeled intervals are intended to define high-confidence ground truth for particular event classes. Candidate-family validation is useful when the main question is whether the detector retrieves intervals that belong to a broader biological family, such as burst-like events, even if final subclass labels differ.

The parameter sensitivity workflow is designed to prevent a common failure in spike-train event analysis: reporting a single parameter set without showing how much the conclusions depend on it. By perturbing Basic-layer parameters and measuring changes in precision, recall, F1, boundary error, and confusion structure against manual labels, investigators can identify fragile thresholds, train-specific failure modes, and whether a parameter set is robust enough for formal reporting.

The current Results section demonstrates that the validation machinery is implemented and exportable, but it does not yet claim detector accuracy on a gold-standard benchmark. A final submission intended to support method performance should add at least one manually annotated or synthetic benchmark dataset with precision, recall, F1, IoU, boundary error, inter-rater agreement when applicable, and comparison against Mean-ISI and logISIH/newBD support calls.

**Table 3. Pre-submission validation evidence recommended before strong method-performance claims.**

| Validation layer | Minimum evidence | Primary outputs | Interpretation |
|---|---|---|---|
| Synthetic benchmark | Injected burst, pause, tonic, and high-frequency states with known boundaries | Precision, recall, F1, IoU, boundary error | Tests whether event grammar recovers known event structure |
| Manual annotation subset | Expert or consensus labels on representative trains | Detector-vs-human metrics; inter-rater agreement if multiple experts are available | Tests biological face validity and label ambiguity |
| Classical support comparison | Mean-ISI and logISIH/newBD support outputs run on the same trains | Agreement, disagreement classes, false-positive and false-negative examples | Shows whether the event grammar adds value beyond support thresholds |
| Artifact stress test | Duplicate timestamps, refractory doublets, long silent intervals, and nonstationary trains | QC stop/demote behavior; false event rate | Tests whether QC-first guardrails prevent predictable failure modes |
| Parameter sensitivity | Perturb Basic-layer seed, bridge, contrast, pause, and high-frequency thresholds | F1/IoU curves; robust parameter regions | Documents whether conclusions depend on narrow threshold choices |

### 3.4 Optional neural manifold and event-state geometry

The Neural Manifold module extends the package from per-train event detection to population-level latent-state exploration, but in this manuscript it is treated as an optional exploratory module. Rather than using event labels as hidden inputs to define the manifold, the primary pipeline constructs population activity vectors from binned spike counts or rates. Event labels are then overlaid and tested as annotations. This ordering is important: if burst or pause labels are used to construct the coordinates, subsequent claims about their separation in the embedding become circular. In the implemented workflow, event-state centroids, dispersions, burst-pause distances, permutation p-values, event-triggered trajectories, velocity, curvature, event-label decoding, and behavior decoding quantify whether event states carry information about the latent population trajectory.

For datasets with behavior variables, the module supports behavior-attached analyses. A scientifically stronger analysis would compare behavior decoding from manifold coordinates alone against behavior decoding from manifold coordinates plus burst/pause labels. If burst/pause labels improve held-out behavior decoding, this supports the claim that event grammar captures behaviorally relevant timing structure not fully represented by the low-dimensional continuous trajectory. If labels do not improve decoding, they may still be descriptive, but their inferential role should be limited.

### 3.5 Tensor analysis with sliceTCA

The sliceTCA module offers a complementary view when the experimental design contains trials, events, or repeated behavioral epochs. The R pipeline constructs a trial-by-neuron-by-time tensor and attaches event-state annotations. The official Python slicetca backend can then be called through reticulate to fit slice tensor components. This is particularly relevant when neural responses vary along trial, neuron, and time modes in ways that are not well captured by a single low-dimensional neural subspace.

In the current package, sliceTCA should be treated as an optional advanced analysis layer rather than part of the core detector-validation claim. The core package can run without Python. When sliceTCA is used in a submitted study, the exact Python environment, torch version, slicetca version, tensor dimensions, bin width, event window, ranks, optimization settings, reconstruction metrics, and random seed should be reported.

## 4. Discussion

SpikeTrainPatternDetector contributes a practical neuroinformatics framework for spike-train review. Its main strength is not that it proposes a universal formula for bursts, pauses, tonic states, or high-frequency firing. Its strength is that it makes the analysis chain explicit. Raw data are checked before detection. Candidate windows are separated from public calls. Manual labels can be protected. Literature support layers are kept distinct from project-defined grammar. Parameters are documented through YAML-backed contracts. Validation outputs can be exported. Neural manifold analyses include event-geometry metrics and shuffle controls rather than relying on visual impressions alone.

This design matches the way spike-train analysis is often conducted in practice. Expert reviewers may recognize meaningful patterns, but a manuscript requires reproducible definitions. Conversely, a purely automatic detector may be reproducible but biologically brittle. SpikeTrainPatternDetector places a reviewable grammar between these extremes: it proposes candidate events, exposes the evidence, and requires validation before strong claims are made.

### 4.1 Biological interpretation

The event grammar should be interpreted as an operational analysis layer. A burst candidate indicates a compact short-ISI structure with sufficient local contrast under the chosen parameter policy. A pause candidate indicates a long interval or pause-like gap under the chosen thresholds and QC context. Tonic and high-frequency labels describe temporal regimes according to local regularity and rate-like evidence. These labels can be biologically meaningful, but their meaning depends on preparation, spike sorting, cell type, brain region, disease state, stimulation state, and behavior. The package therefore encourages manual review, support-method comparison, parameter sensitivity analysis, and, when possible, behavioral validation.

### 4.2 Relationship to classical burst-detection methods

Mean-ISI and logISIH/newBD support methods are important because they provide literature-linked threshold evidence. They also illustrate why no single detector should be treated as universally definitive. Mean-ISI adapts to intrinsic ISI statistics, but it can be affected by nonstationarity and by the distribution of long silent intervals. logISIH/newBD uses histogram structure and void-like separation, but histogram binning, smoothing, and peak-valley selection can influence threshold resolution. SpikeTrainPatternDetector exposes these methods as support layers so that investigators can compare them with the main grammar instead of silently mixing their assumptions.

### 4.3 Manifold interpretation and validation

Low-dimensional neural embeddings are powerful but easy to overinterpret. PCA and FA offer transparent baselines and should usually be reported before nonlinear methods. GPFA is appropriate for single-trial population trajectories when implemented as a full probabilistic model, while the current package's smooth-trajectory option is only an exploratory smoothed FA preview. UMAP, t-SNE, PHATE, and Isomap can reveal useful structure, but changes in seed, bin width, smoothing sigma, neighbor number, or perplexity can change the visualization. The package therefore emphasizes trustworthiness, continuity, seed stability, time-shuffle controls, event-label-shuffle controls, and decoding-based validation.

For studies of movement-related neurons, the most informative analysis is not simply whether a 3D trajectory looks separated into burst and pause regions. A stronger analysis asks whether event-state geometry predicts behavior, whether behavior predicts the latent trajectory, whether burst/pause labels improve held-out behavior decoding beyond coordinates alone, and whether event-triggered velocity or curvature changes survive shuffle controls. This shifts the claim from "the manifold looks interesting" to "event states explain or predict measurable structure in neural-behavioral dynamics."

### 4.4 Limitations

Several limitations should be considered before publication or biological inference.

First, the core event grammar is an engineering-calibrated rule system. It is interpretable and auditable, but it is not a strict formula reproduced from a single paper. Manuscripts using the grammar should explicitly state this.

Second, manual labels remain essential for validation. Without expert labels or independent behavioral/physiological endpoints, detector outputs should be treated as candidate events.

Third, QC warnings can alter interpretation. Refractory-suspect ISIs, duplicate timestamps, nonstationarity, and derived-file import errors can all affect burst and pause evidence.

Fourth, the Neural Manifold and sliceTCA modules broaden the software's exploratory scope. They should be kept secondary unless a submitted study includes real population recordings, behavior variables, held-out decoding, shuffle controls, and embedding-stability analyses. Bin width and smoothing are scientific choices. A 1 ms bin may preserve spike timing, but population manifold estimation usually requires enough observations per bin or smoothing to stabilize the activity matrix. Very small bins can produce sparse, noisy embeddings and should be tested against larger bins and smoothing settings.

Fifth, optional Python-dependent methods require transparent environment reporting. sliceTCA, torch, CEBRA, or full GPFA backends should be documented with package versions, random seeds, and reconstruction or prediction metrics.

### 4.5 Future work

Future development should focus on four directions. First, parameter metadata should be expanded so that every parameter is tagged with method_class, citation keys, formula reference, unit convention, and validation status. Second, benchmark and manual-label validation should be added as first-class evidence, including synthetic ground truth, expert labels, inter-rater agreement, classical-method comparisons, artifact stress tests, and parameter-sensitivity curves. Third, additional golden tests should cover boundary bursts, refractory doublets, high-frequency tonic states, pause-near-burst conflicts, long bursts, candidate-ledger separation, and export consistency. Fourth, publication-grade manifold workflows should integrate full GPFA and official CEBRA backends, with held-out neuron prediction, held-out behavior decoding, and cross-session embedding stability as first-class outputs.

## 5. Conclusion

SpikeTrainPatternDetector provides an auditable R/Shiny framework for spike-train pattern analysis. It combines QC-first data handling, reviewable event-grammar candidate generation, literature-linked Mean-ISI and logISIH/newBD support layers, manual-label protection, validation exports, state-space analysis, population neural manifold tools, and optional sliceTCA integration. The package is best understood as a transparent candidate-generation and validation platform. Its scientific value lies in making spike-train pattern analysis inspectable, reproducible, and testable rather than in claiming universal automatic ground truth.

## Data availability statement

The software includes a bundled example CSV file used for smoke testing: inst/extdata/Grechishnikova_STN_2017_subset.csv. This example is used only to demonstrate the computational workflow and should not be interpreted as a clinical or disease-physiology dataset. Any private patient or laboratory spike-train datasets used in future submissions should be described under the appropriate institutional review, consent, and data-sharing constraints. A public code repository and archival DOI/URI should be inserted before submission.

## Code availability statement

SpikeTrainPatternDetector version 1.2.1 is an R/Shiny package with native C components and optional Python integration through reticulate. The project should be deposited in a public repository before submission, and the final manuscript should include the repository URL, release tag, Zenodo DOI or equivalent persistent identifier, operating-system notes, programming language, installation instructions, and dependency versions.

## Ethics statement

This manuscript draft describes software and a bundled smoke-test dataset. It does not report new human or animal experimental results. If the software is submitted together with patient recordings, intraoperative data, animal experiments, or unpublished laboratory datasets, the final manuscript must include the relevant ethics committee approval, consent statement, protocol identifier, and data-use restrictions.

## Author contributions

HZ designed and implemented the software, developed the algorithmic workflow, performed code-level checks, and prepared the manuscript draft. Additional contributors, if any, should be added according to Frontiers and ICMJE authorship criteria before submission.

## Funding

[Funding information to be inserted. If no specific funding supported the work, state: The author received no specific funding for this work.]

## Conflict of interest

[Conflict-of-interest statement to be completed before submission.]

## Acknowledgments

The author thanks users and reviewers who provided feedback on spike-train review workflows and validation needs. Any additional acknowledgments should be added before submission.

## Generative AI statement

During manuscript preparation, OpenAI Codex was used for drafting assistance, code-audit summarization, and manuscript organization. The author remains responsible for all scientific claims, citations, code behavior, and final submitted content. This statement should be revised to match the final use of AI tools according to Frontiers policy at the time of submission.

## Figure legends

**Figure 1. SpikeTrainPatternDetector workflow.** Raw spike timestamp data are imported, checked by pre-detection QC, passed through the event-grammar detector and literature-linked support layers, reviewed with manual-label protection, validated against manual labels or shuffles, and exported as candidate ledgers, diagnostic audits, eventness features, parameter reports, and validation files.

**Figure 2. Core event grammar and support layers.** The main detector uses project-defined seed, bridge, flank-contrast, pause, tonic, and high-frequency rules to generate reviewable event candidates. Mean-ISI and logISIH/newBD are separate literature-linked support layers that provide threshold evidence but do not define final labels by themselves.

**Figure 3. Candidate audit and event-level validation.** Public selected candidates are stored separately from diagnostic rejected or blocked windows. When manual labels are available, automatic and manual intervals are matched by IoU to compute precision, recall, F1, boundary error, and label-confusion summaries.

**Figure 4. Optional Neural Manifold module.** Simultaneous spike trains are binned into a time-by-neuron population matrix, transformed and optionally smoothed, embedded with PCA, FA, smoothed-FA preview, Isomap, PHATE, UMAP, t-SNE, or behavior-guided projection, and tested with event-state centroid, dispersion, permutation, time-shift, trajectory, velocity, curvature, and decoding metrics.

**Figure 5. Optional sliceTCA workflow.** Trial or event times define a trial-by-neuron-by-time tensor. Event annotations are attached to tensor bins, and the official Python sliceTCA backend can be called through reticulate when numpy, torch, and slicetca are installed.

## References

Chen, L., Deng, Y., Luo, W., Wang, Z., and Zeng, S. (2009). Detection of bursts in neuronal spike trains by the mean inter-spike interval method. Progress in Natural Science 19, 229-235. doi: 10.1016/j.pnsc.2008.05.027

Coifman, R.R., and Lafon, S. (2006). Diffusion maps. Applied and Computational Harmonic Analysis 21, 5-30. doi: 10.1016/j.acha.2006.04.006

Cunningham, J.P., and Yu, B.M. (2014). Dimensionality reduction for large-scale neural recordings. Nature Neuroscience 17, 1500-1509. doi: 10.1038/nn.3776

Holt, G.R., Softky, W.R., Koch, C., and Douglas, R.J. (1996). Comparison of discharge variability in vitro and in vivo in cat visual cortex neurons. Journal of Neurophysiology 75, 1806-1814. doi: 10.1152/jn.1996.75.5.1806

Marwan, N., Romano, M.C., Thiel, M., and Kurths, J. (2007). Recurrence plots for the analysis of complex systems. Physics Reports 438, 237-329. doi: 10.1016/j.physrep.2006.11.001

McInnes, L., Healy, J., and Melville, J. (2018). UMAP: Uniform Manifold Approximation and Projection. Journal of Open Source Software 3, 861. doi: 10.21105/joss.00861

Moon, K.R., van Dijk, D., Wang, Z., Gigante, S., Burkhardt, D.B., Chen, W.S., et al. (2019). Visualizing structure and transitions in high-dimensional biological data. Nature Biotechnology 37, 1482-1492. doi: 10.1038/s41587-019-0336-3

Pasquale, V., Martinoia, S., and Chiappalone, M. (2010). A self-adapting approach for the detection of bursts and network bursts in neuronal cultures. Journal of Computational Neuroscience 29, 213-229. doi: 10.1007/s10827-009-0175-1

Pellegrino, A., Stein, H., and Cayco-Gajic, N.A. (2024). Dimensionality reduction beyond neural subspaces with slice tensor component analysis. Nature Neuroscience 27, 1199-1210. doi: 10.1038/s41593-024-01626-2

Schneider, S., Lee, J.H., and Mathis, M.W. (2023). Learnable latent embeddings for joint behavioural and neural analysis. Nature 617, 360-368. doi: 10.1038/s41586-023-06031-6

Shinomoto, S., Miura, K., and Koyama, S. (2005). A measure of local variation of inter-spike intervals. BioSystems 79, 67-72. doi: 10.1016/j.biosystems.2004.09.023

Tenenbaum, J.B., de Silva, V., and Langford, J.C. (2000). A global geometric framework for nonlinear dimensionality reduction. Science 290, 2319-2323. doi: 10.1126/science.290.5500.2319

van der Maaten, L., and Hinton, G. (2008). Visualizing data using t-SNE. Journal of Machine Learning Research 9, 2579-2605.

Yu, B.M., Cunningham, J.P., Santhanam, G., Ryu, S.I., Shenoy, K.V., and Sahani, M. (2009). Gaussian-process factor analysis for low-dimensional single-trial analysis of neural population activity. Journal of Neurophysiology 102, 614-635. doi: 10.1152/jn.90941.2008
