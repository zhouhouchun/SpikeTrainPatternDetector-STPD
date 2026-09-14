# Stage 09 framework review — real-reference freeze and eligibility

Date: 2026-08-30
Scope: reference contract only; the user's evolving annotation workbook is not
declared frozen by this review.

## Decision

PASS. The framework requires exact workbook/input hashes, per-train annotation
coverage, per-track eligibility and a limitation record before any real-data
performance rerun can become primary evidence.

## Stop-check review

- Blank/unlabelled ISIs follow the frozen `other` policy only after the final
  workbook version is declared.
- A train may be eligible for Burst but ineligible for Pause/HFS; sparse or
  unreviewed tracks are excluded from that track's primary denominator.
- The biological scope is one patient with bilateral STN recordings, not 13
  independent patients.
- Single-rater labels are agreement references with possible annotation error;
  they cannot support inter-rater reliability claims.
- Any later annotation correction changes the reference hash and invalidates
  downstream result manifests.

Final hashes and eligibility counts remain deferred. P0/P1/P2 framework
defects found: 0/0/0. Stage 10 may begin.
