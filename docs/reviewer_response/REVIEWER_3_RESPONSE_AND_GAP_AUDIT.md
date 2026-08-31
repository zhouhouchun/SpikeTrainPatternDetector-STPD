# Reviewer 3 response and gap audit

Author: Zhou Houchun

Status: evidence audit before manuscript integration. `Complete` means that a
frozen implementation or result exists. It does not mean that the corresponding
manuscript text and line references have already been updated.

## Overall response strategy

We thank the reviewer for the detailed major-revision assessment and for
recognizing the value of retaining QC decisions, threshold sources, candidate
histories and manual interventions. The review correctly separates three
claims that the submitted manuscript partially conflated:

1. the detector's empirical accuracy;
2. the engineering reproducibility and traceability of the software; and
3. the untested possibility that provenance improves human annotation speed or
   agreement.

The revision will claim and evaluate the first two. It will not claim that STPD
improves reviewer agreement, annotation efficiency or usability because no
controlled user study or multi-rater experiment has been performed. The
software contribution will therefore be described as an auditable and
replayable annotation architecture whose human-factor benefits remain to be
tested.

## Major-comment disposition

| ID | Issue | Disposition | Evidence and required action |
|---|---|---|---|
| R3.1 | Provenance benefit not directly evaluated | **Partially addressed; claim narrowed** | Candidate stages now have stable schemas, hashes, deterministic replay and tamper-sensitive receipts, but no controlled reviewer-time/agreement study exists. Report replayability and error localization as engineering properties; state that usability and agreement benefits remain untested. |
| R3.2 | Validation almost entirely synthetic | **Largely addressed with limitation** | Added expert-reference GPe, STN and GPi validation under automatic, held-out partial-known and full-parameter regimes. Current references are single-expert per dataset; no inter-rater agreement claim is permitted. |
| R3.3 | Synthetic benchmark aligned with detector assumptions | **Partially addressed** | v2.2/v2.3 use versioned mechanism-based generators, blinded timestamp inputs, separate truth, shifted-gamma State processes, finite rate-multiplier Burst pulses, explicit transition/overlap strata and no STPD output during generation. However, v2.3 explicitly lacks drift, sorting error, missed/false spikes and timestamp jitter. Describe it as a clean mechanism benchmark, not a realistic acquisition benchmark; observation-noise/OOD testing remains open. |
| R3.4 | Correlated ISIs and absent recording-level uncertainty | **Addressed** | Added per-train outputs and 1,000-replicate clustered bootstrap 95% CIs. Synthetic scales share `Template_ID` clusters; biological results use recording groups. Do not interpret these as patient-population CIs. |
| R3.5 | Event-level validation should be central | **Largely addressed** | Added event precision/recall/F1 at prespecified IoU thresholds, matched IoU, onset/offset error, fragmentation, merge/false-split and event counts. ISI-support metrics remain secondary. Human reviewer workload is not experimentally measured and must not be claimed. |
| R3.6 | Single mutually exclusive label track | **Addressed in implementation** | Event, State, rate/subtype, QC and review/provenance are represented separately; Burst may coexist with Broad HFS; HFT/HF-irregular are subtypes on frozen Broad HFS support; an optional deterministic summary remains for display/export. |
| R3.7 | Oracle threshold analysis | **Addressed** | Oracle/full-parameter analysis is labeled a same-data adaptation upper bound. Added automatic zero-example and held-out partial-known calibration with non-overlapping recording groups. Never describe oracle results as deployable accuracy. |
| R3.8 | Mean-ISI/LogISI comparison uncontrolled | **Addressed** | Added a unified label-blind automatic comparison using identical records, truth masks, event matching, post-processing definitions and clustered CIs. Agreement, automatic accuracy and parameter import are reported separately. |
| R3.9 | Dataset pooling and threshold leakage | **Largely addressed; methods explanation required** | Threshold sources and effective values are exported; train-adaptive evidence is separated from dataset-level priors; held-out calibration freezes parameters from calibration groups only; bounded borrowing is tested. The manuscript must specify which quantities are dataset-level, train-specific and held-out. |
| R3.10 | Small-window statistics unstable | **Partially addressed** | Initial contrast Burst candidates require at least four spikes; low-n routes fail closed or are explicitly typed; candidate tables retain observation counts and missingness; CV/LV/MM are not interchangeable. A concise feature-by-feature minimum-sample table and UI-warning description are still required. |
| R3.11 | Spike-sorting/timestamp-error robustness | **Not yet fully addressed** | QC covers duplicates, nonpositive timestamps, hard artifacts and refractory-suspect ISIs. The clean synthetic benchmark explicitly contains no missed/false spikes, jitter or sorting error. Add a bounded remove/insert/jitter sensitivity experiment or state this as an untested limitation. |
| R3.12 | 0.9/1.0-ms refractory thresholds | **Partially addressed** | Both thresholds and the suspect action are configurable and exported; policies include warn, demote, mark possible multi-unit contamination, split and exclude. The manuscript still needs a precision/rounding guard and must describe the defaults as operational presets rather than universal physiological constants. |
| R3.13 | Stationarity warning undefined | **Implemented; prose pending** | `stationarity_train_qc()` uses eight temporal bins by default, requires 60 valid ISIs, records bin count, drift ratio, log-median range, status and warning; it is advisory and exported. Methods must state decision thresholds, low-n behavior and that warnings do not silently relabel events. |
| R3.14 | Manual-label protection can preserve errors | **Partially addressed** | Strong AUTO/manual conflicts, reviewer identifiers, append-only review transitions and replayable history are supported. The current biological validation does not provide multi-rater disagreement data, and the UI should not be claimed to establish inter-rater adjudication. |
| R3.15 | Formal software testing | **Partially addressed** | Final regression: 141 test files, 999 test blocks, zero failures/errors/warnings and one optional skip; schema, hash, replay and numerical-tolerance cases are tested. Formal coverage percentage, active CI evidence and cross-platform equivalence are not yet available and must not be claimed. |
| R3.16 | Performance/scalability absent | **Partially addressed** | Added repeated core and full-application runtime/memory benchmarks plus GPe/STN/GPi workloads. These quantify current cost but do not establish a full scaling law over candidate density and diagnostic levels. Report the measured workloads and avoid extrapolation. |
| R3.17 | Restrictive input interoperability | **Open/partially addressed** | CSV/RDS and external interval-provider contracts exist, but direct Neo, NWB and SpikeInterface object import is not established. Add a documented long-format converter as the minimum revision; describe Neo/NWB/SpikeInterface as future adapters unless implemented and tested. |
| R3.18 | Formalize provenance | **Largely addressed; terminology must be precise** | Current products include stable candidate/run identifiers, schema versions, policy and parameter hashes, input manifests, parent-child links, deterministic replay, tamper-sensitive receipts and append-only review transitions. Define this as computational provenance/traceability; do not equate it with demonstrated human-factor benefit. |

## Evidence anchors

- Final algorithm and regression freeze: `results/release_freeze/FINAL_ALGORITHM_FREEZE.md`
- Real-reference freeze: `data/real/ANNOTATION_FREEZE.md`
- Multitrack product: `R/66a_multitrack_auto_product.R`
- Candidate lineage and replay: `R/91_candidate_lineage_collector.R` and
  `R/91b_candidate_lineage_burst_stage4.R` through
  `R/91m_candidate_lineage_complete_universe_release.R`
- Review/provider contracts: `R/82_provider_contract.R` through
  `R/88_provider_scoring_export_phase_e.R`
- Stationarity QC: `R/15_governance.R`
- Three-regime validation:
  `results/real_validation/`
- Unified three-method comparison:
  `results/method_comparison/three_method_truth_accuracy_current/`
- Performance evidence: `results/performance/`
- Synthetic v2.3 design:
  `Simulator data/clean_synthetic_mechanism_benchmark_v2_3_0/README.md`

## Minor-comment disposition

| Comment | Disposition |
|---|---|
| Manuscript is long | Move detailed operational equations, parameter contracts and extended schemas to Supplementary Methods; keep the conceptual detector stages in the main text. |
| Distinguish clinical fixture/calibration/reference data | Replace the obsolete terminology with explicit categories: software fixture, simulator-development biological ranges, and expert-reference biological validation datasets. |
| Do not emphasize patterned-vs-unclassified F1 | Remove it from primary claims. Keep `possible_burst`, QC and review status off the biological Event/State truth track. |
| Report event counts | Completed in event-level result tables; add counts to the main manuscript tables. |
| Report HFT predicted interval counts | Report Broad HFS as the primary endpoint and HFT/HF-irregular counts as descriptive subtypes only. |
| Add sensitivity plots | Threshold preview and parameter-sensitivity outputs exist; select only prespecified, interpretable plots for the manuscript. |
| Larger interface figures | Pending manuscript figure replacement; use separate high-resolution panels for candidate review, provenance inspection and conflict resolution. |

## Claims that must be removed or narrowed

- Do not claim general neuronal firing-pattern detection performance.
- Do not claim that provenance improves reviewer agreement, review time or
  annotation accuracy.
- Do not claim inter-rater validation.
- Do not treat 1×, 4× and 10× projections as independent replicates.
- Do not describe full-parameter/same-data adaptation as independent accuracy.
- Do not describe HFT or HF-irregular as independently validated biological
  classes.
- Do not claim cross-platform determinism, formal code coverage or direct
  Neo/NWB/SpikeInterface support until corresponding evidence exists.

## Remaining work specifically exposed by Reviewer 3

1. Add a small, frozen spike remove/insert/jitter sensitivity experiment, or
   explicitly decline it and narrow robustness claims.
2. Add a documented and tested long-format timestamp converter; keep direct
   Neo/NWB/SpikeInterface adapters as future work unless implemented.
3. Produce the feature-level minimum effective sample-size table.
4. Decide whether to add CI/coverage infrastructure; otherwise report only the
   completed regression suite and tested environment.
5. Replace the small UI screenshot with publication-quality R-generated or
   direct software screenshots showing the requested workflow panels.
6. Rewrite the manuscript and response with page/line references after the
   result tables and figures are frozen.
