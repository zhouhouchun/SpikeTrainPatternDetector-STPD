# Stage 07 framework review — synthetic benchmark freeze

Date: 2026-08-30
Scope: freeze contract only; no dataset version is declared final here.

## Decision

PASS. The minimum freeze packet binds input/truth bytes, mechanism and
phenotype eligibility, paired identities across 1x/4x/10x scales, generator
version/seeds and immutable calibration/evaluation group membership.

## Stop-check review

- The three scales are scored separately.
- Scale copies of one template remain one statistical cluster and are not
  treated as independent biological replicates.
- Phenotype eligibility is computed by preregistered detector-blind rules;
  STPD output cannot trigger deletion or resampling.
- Mechanism truth, phenotype truth and generator provenance remain parallel.
- Ambiguous cases remain countable for strict/sensitivity analyses.

The current v2.1/v2.2 choice and final file hashes remain deferred until the
generator work is frozen. P0/P1/P2 framework defects found: 0/0/0. Stage 8 may
begin.
