# Exploratory Tonic local-core burden check

Status: **exploratory and non-independent**. This diagnostic reuses the latest
held-out real-data predictions and labels. It is not an independent validation,
was not used to tune the detector, and must not be reported as a confirmatory
performance result.

Source snapshot:

`real_grechishnikova_2017_reference_eligible_tonicfreeze_samplerfix_20260826`

Reproduce from the repository root:

```sh
STPD_REAL_VALIDATION_DIR=/path/to/frozen_real_validation \
Rscript evaluation/publication_validation/diagnostics/tonic_local_core_burden/tonic_local_core_burden.R
```

The script scans label-blind, exact four-ISI local cores with each held-out
fold's frozen Tonic ISI band, minimum duration and LV limit. A core may contain
at most one ISI below the lower band, none above the upper band, must have
`max(ISI)/mean(ISI) <= 1.25`, and overlapping passing cores are merged. The two
outputs differ only in the lower MM interpretation:

- `intended_min_over_mean`: applies `min(ISI)/mean(ISI) >= 0.85`, matching the
  legacy near-miss implementation and the intended lower-tail regularity gate.
- `current_canonical_max_over_mean`: reproduces the current canonical code,
  which compares `stpd_event_core_mm = max(ISI)/mean(ISI)` with both MM limits.
  For positive ISIs and a lower limit of 0.85, the lower test is mathematically
  non-binding because `max(ISI)/mean(ISI) >= 1`.

This exposes an MM semantic inconsistency, not evidence to change the frozen
algorithm from held-out data. The same current-canonical comparison is also
present in the multitrack compatibility re-gate; the legacy near-miss code
separately computes `min(ISI)/mean(ISI)`.

Headline burden:

- Intended lower-tail semantics: 29 candidates / 125 ISIs; Tonic support
  TP/FP/FN 15/110/45 (P/R/F1 0.120/0.250/0.162); 20 HFS-only candidates.
- Current canonical semantics: 77 candidates / 393 ISIs; Tonic support
  TP/FP/FN 24/369/36 (P/R/F1 0.061/0.400/0.106); 58 HFS-only candidates.

The diagnostic therefore supports, at most, a review-only `possible_tonic`
proposal layer. It does not support automatic threshold relaxation or changes
to canonical Tonic/HFS support.
