STPD detector vs simulator ground-truth interval evaluation

Alignment: simulator ground truth uses (Train, Right_Spike_Index); STPD ISI_labels_final uses train and idx. The first spike in each train has no ISI and is excluded.

Evaluation modes:
1. exact_original_labels: simulator labels are used as written; STPD blank labels are unclassified. This penalizes STPD because the current detector does not emit a Noisy class.
2. stpd_comparable_noisy_as_unclassified: simulator Noisy intervals are mapped to unclassified, matching the detector's absence of a Noisy output class.
3. stpd_comparable_burst_family: same as mode 2, but simulator Burst and STPD burst/possible_burst are merged into burst_family.
4. binary_patterned_vs_nonpattern: all non-Noisy simulator intervals are patterned; STPD blank intervals are non-pattern/non-noisy.

The crosswalk used for manuscript reporting is exported as ground_truth_to_scored_label_crosswalk.csv.
For macro-F1 reporting, macro_F1_inclusive counts undefined F1 values caused by zero predictions as 0. This is the primary macro-F1 for manuscript reporting. macro_F1_emitted_only averages only classes emitted by the detector and should be treated as a diagnostic, not a headline result.

Use exact_original_labels for a conservative raw-label audit; use stpd_comparable_burst_family for the fairest interval-level benchmark against the current STPD label vocabulary.
