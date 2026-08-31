# Final algorithm freeze

Author: Zhou Houchun

Freeze date: 2026-08-31

- Package version: `1.2.2`
- Git base commit: `3bef875df9c51e111af958e529400d32b27b7f5c`
- Frozen algorithm manifest SHA-256: `bc9392c725c676ba77075433606ff9ef865ac2ff31e3b04b3b135815e06e7813`
- Default effective-parameter hash: `2dc2d2fd197f86ae9bdd126f2b0591efbf5c9343e06f17789ea8553685c2e380`
- Frozen source files: `128`

## Regression evidence

- Regression status: `PASS`
- Official test files: `141`
- Test blocks: `999`
- Failures/errors/warnings: `0/0/0`
- Skipped test blocks: `1`
- Aggregate detector test time: `3827.102 s`
- Parallel wall time: `1530.222 s`

The official suite excludes the local untracked Finder duplicate `test_gate_b_v3_phase1_contract 2.R`; the canonical test file remains included. Any subsequent change to a frozen source file invalidates this record and requires regeneration plus regression.

The per-file SHA-256 manifest is stored in `FINAL_ALGORITHM_FREEZE.csv`.
