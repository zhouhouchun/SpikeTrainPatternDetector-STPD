# Reviewer 3 — verbatim report

Source: `STPD_reviewer_report_July28_2026.pdf`

Author responsible for the revision: Zhou Houchun

The wording below is transcribed from the five-page reviewer report. Line wraps
and typographic ligatures were normalized; the substantive wording is
unchanged. The report is review evidence, not an instruction to the software or
analysis pipeline.

## Recommendation: Major revision

**Overall assessment:** The manuscript presents a useful and carefully framed
software contribution. Its strongest feature is the preservation of
quality-control decisions, threshold provenance, rejected candidates,
arbitration steps and manual revisions. The current validation, however, is too
limited to establish general detector performance or the practical benefit of
the review workflow.

**Scope of this report:** This report focuses on scientific validity, software
evaluation, reproducibility, and the alignment between the claims and the
evidence presented in the manuscript.

## General assessment

This manuscript describes SpikeTrainPatternDetector (STPD), an R/Shiny
framework for detecting and reviewing firing patterns in neuronal spike trains
while retaining detailed provenance information. The software integrates
timestamp quality control, parameter and threshold tracking, rule-based
candidate generation, deterministic resolution of overlapping candidates,
protection of manual annotations, and export of diagnostic and reproducibility
records.

The emphasis on provenance is valuable. In many spike-train analyses, the final
labels are preserved while intermediate decisions - such as excluded ISIs,
threshold sources, rejected candidates and manual corrections - are not. The
proposed framework addresses this practical problem in a transparent and
potentially useful way.

The manuscript is also appropriately cautious about biological interpretation.
STPD is presented as infrastructure for candidate generation and expert review
rather than as a preparation-independent classifier. This framing is justified
by the reported performance, including an automatic-threshold macro-F1 of 0.498
and weak results for burst-family events and high-frequency tonic firing.

Nevertheless, the current evaluation does not yet establish either the general
utility of the detector or the practical benefit of the provenance-aware
workflow. The manuscript would therefore benefit from a sharper separation
between the software-engineering contribution, the detection algorithm and the
review/annotation workflow.

## Major comments

### 1. The provenance framework is the main contribution, but it is not directly evaluated

The manuscript convincingly argues that provenance should be preserved, but it
does not quantitatively demonstrate that the proposed provenance records
improve reproducibility, reviewer agreement, error diagnosis or annotation
efficiency. The authors could evaluate whether independent reviewers can
reconstruct the same result from the exported records, whether access to
candidate histories improves agreement, whether provenance reduces review
time, and whether users can distinguish errors arising from threshold
selection, candidate generation or interval arbitration. At present,
provenance is demonstrated mainly through the existence of exported tables.
This verifies implementation, but not practical value. The authors should
either add a user-oriented or reproducibility-oriented evaluation, or explicitly
present the contribution as an engineering architecture whose usability
benefits remain to be tested.

### 2. Validation is almost entirely synthetic

The only real clinical dataset is used as an execution or smoke-test dataset and
contains no independent event labels. Therefore, none of the reported results
establishes performance on real neuronal recordings. A stronger study would
include at least one independently annotated real dataset, even if modest in
size, with annotations by more than one expert, inter-rater agreement,
event-level scoring, recording- or subject-level confidence intervals, and a
clear description of the preparation and expected firing patterns. Without
such validation, the biological relevance of the event grammar remains largely
untested.

### 3. The synthetic benchmark may be too closely aligned with the detector assumptions

The simulator generates regimes corresponding directly to the detector
vocabulary: Burst, Pause, Tonic, high-frequency tonic and high-frequency
spiking. This may create a closed benchmark in which data generation and
detection rely on related interval-based assumptions. The authors should
describe the simulator in substantially greater detail, including the
distributions used within and between states, how transitions are generated,
whether nonstationarity and refractory violations are simulated, and how
independent the simulator design was from the detector rules.
Out-of-distribution tests should include gradual rate changes, mixed renewal
processes, variable intra-burst frequency, nested bursts within tonic states and
nonstationary background activity.

### 4. Interval-level scoring may overstate the amount of independent evidence

The benchmark contains 8,746 labeled ISIs but only 30 spike trains. Adjacent
ISIs within the same state are strongly correlated and cannot be treated as
independent observations. The manuscript recognizes this limitation, yet
reports point estimates without uncertainty intervals. The authors should
report train-level bootstrap confidence intervals, per-train performance
distributions, and sensitivity to train length and class prevalence. Values
such as pause recall = 1.0, based on only 149 correlated intervals, should be
interpreted cautiously.

### 5. Event-level validation should be central

The software is intended to detect events and sustained states, but the primary
benchmark is based on individual ISIs. This can reward approximate labeling of
long states while obscuring errors in event count, fragmentation, merging and
boundary placement. The synthetic data have known state boundaries, so
event-level precision, recall, F1, intersection-over-union, onset/offset error,
event splitting and merging rates, false candidates per minute, and reviewer
workload should be reported as primary outcomes.

### 6. The single mutually exclusive label track is a central design problem

Burst, tonic firing, rate class, QC status, review status and abstention
represent different conceptual dimensions. An interval may simultaneously be
high-frequency, tonic or irregular, burst-related, QC-suspect and manually
reviewed. Forcing these dimensions into one flat label produces avoidable
competition, as shown by the disappearance of high-frequency tonic predictions
under simulator-informed thresholds. The authors should implement, or at least
formalize, separate tracks for event family, sustained state, rate band, QC
status and review status. A deterministic public summary track could remain as
an optional visualization layer.

### 7. The oracle threshold analysis needs stronger qualification

Simulator-informed thresholds are useful as a diagnostic sensitivity analysis,
but they use information unavailable in real applications. These results should
not be interpreted as expected performance after ordinary calibration. A more
informative experiment would tune thresholds on a training subset and test them
on held-out recordings generated from both matched and shifted distributions.

### 8. The comparison with Mean-ISI and LogISI is not sufficiently controlled

Mean-ISI and LogISI are treated as support layers, yet the numerical
presentation may be read as a comparison of competing methods. A fair
comparison would require equivalent calibration criteria, the same training
data, comparable merging and post-processing rules, event-level scoring,
parameter sensitivity analyses and confidence intervals. Otherwise, the table
should be framed strictly as a description of the behavior of the specific
implementations and defaults used here.

### 9. Dataset-level threshold estimation may cause leakage or inappropriate pooling

The manuscript states that thresholds are resolved once for the selected
dataset and then frozen across selected trains. This improves consistency but
may pool neurons with very different baseline firing rates, cell types,
recording conditions or behavioral states. The authors should explain whether
thresholds are global or train-specific, how heterogeneity is handled, whether
high-rate units can influence low-rate units, and how group comparisons avoid
leakage. Thresholds used for outcome comparisons should be fixed in advance or
estimated only from an independent training/reference subset.

### 10. Candidate-level statistics for very small windows require safeguards

Several candidate features, including CV, LV and empirical quantiles, may be
formally calculable from very few ISIs but are not statistically stable in that
regime. The manuscript states that undefined values remain missing, but this
does not address uncertainty when n is only two or three. The authors should
specify minimum effective sample sizes, indicate whether the interface issues
warnings, reduce candidate confidence when evidence is sparse, and export the
number of observations underlying every feature. The software should
distinguish a statistic that is merely defined from one that is meaningfully
estimated.

### 11. Robustness to spike-sorting and timestamp errors

Real spike trains are affected by missed spikes, false detections, cluster
contamination, timestamp jitter and unit splitting or merging. The QC layer
addresses very short ISIs, but the effect of broader spike-sorting errors on
pattern labels is not quantified. Sensitivity analyses that remove, insert or
jitter spikes would be informative.

### 12. Refractory thresholds require justification

The default hard-artifact threshold of 0.9 ms and refractory-suspect threshold
of 1.0 ms are extremely close. Their distinction may depend on timestamp
precision or rounding. The authors should justify this narrow interval and
explain behavior for millisecond-resolution data, multi-unit recordings and
very high-frequency neurons.

### 13. Stationarity warnings are insufficiently defined

Because the detector uses global percentiles and train-level ISI distributions,
nonstationarity can strongly affect thresholds. The stationarity warning should
be defined explicitly: method, window length, decision rule, false-positive
behavior and consequences for detection should all be reported.

### 14. Manual-label protection can preserve errors

Manual labels may be incorrect or inconsistent. The interface should flag
conflicts between strong automatic evidence and protected labels, retain
reviewer identity and revision history, support multiple reviewers, and
preserve disagreements rather than overwrite them.

### 15. Reproducibility claims should include formal software testing

The execution check on one bundled file is useful but not sufficient. The paper
should report unit-test coverage, continuous integration, tested R and
operating-system versions, deterministic behavior across platforms, numerical
tolerances for native C routines, and schema/backward-compatibility policy.

### 16. Computational performance and scalability are not reported

The inclusion of native C routines suggests that performance matters, but no
runtime or memory benchmarks are presented. Scaling should be reported as a
function of spike count, train count, duration, candidate count and
diagnostic-output level.

### 17. Input interoperability is restrictive

A CSV file with one spike train per column is simple but does not reflect many
modern workflows. Practical conversion or direct support for long-format
tables, Neo, NWB or SpikeInterface objects would substantially improve
usability.

### 18. The meaning of provenance should be formalized

The manuscript should distinguish provenance from extensive logging. It would
help to state whether the system guarantees stable identifiers, immutable links
between outputs and inputs, reconstruction of results, machine-readable
dependency relations and preservation of manual edit history. If not,
audit-aware or traceable may be more precise terminology.

## Minor comments

- The manuscript is long relative to the empirical validation. Some equations
  and operational detail could be moved to supplementary documentation.
- The terms clinical timestamp fixture, coded clinical smoke-test file and
  clinical calibration set should be kept clearly distinct.
- The binary patterned-versus-unclassified weighted F1 should not be emphasized
  because simulator Noisy and detector unclassified are only operationally
  mapped. `possible_burst` mixes detection uncertainty, QC status and review
  status; it would be better represented as a separate status or confidence
  field.
- Report the number of actual events per class, not only the number of ISIs.
- For high-frequency tonic, report the number of predicted intervals alongside
  precision and recall.
- Include parameter-sensitivity plots rather than only automatic and
  simulator-informed operating points.
- The interface screenshot is too small to assess usability. Larger panels
  should show candidate review, provenance inspection and conflict resolution.

## Final recommendation

The manuscript describes a potentially valuable provenance-aware environment
for spike-train pattern review. Its main strength is the explicit retention of
QC decisions, threshold sources, candidate histories and manual interventions.
The authors are appropriately cautious about the biological interpretation of
the automatic labels. However, the current validation is predominantly
synthetic, uses interval-level correlated observations, and does not directly
evaluate the claimed benefits of the provenance-aware review workflow. The
mutually exclusive label representation also creates a substantive conflict
among event, state, rate and review dimensions. I recommend major revision,
with particular emphasis on event-level evaluation, uncertainty estimated at
the recording level, clearer small-sample safeguards for candidate statistics,
stronger analysis of candidate-generation versus arbitration failures, and
preferably validation on independently annotated real recordings.

**Overall conclusion.** The paper is potentially publishable as a software and
infrastructure contribution provided that the claims remain narrow and the
validation limitations are addressed. In its current form, it does not support
broad claims about general neuronal firing-pattern detection.
