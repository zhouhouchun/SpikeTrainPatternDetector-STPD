# Final algorithm freeze

Author: Zhou Houchun

Freeze date: 2026-08-31

- Package version (`DESCRIPTION`): `1.2.2`
- Historical working-tree base commit: `3bef875df9c51e111af958e529400d32b27b7f5c`
- Frozen algorithm manifest SHA-256: `bc9392c725c676ba77075433606ff9ef865ac2ff31e3b04b3b135815e06e7813`
- Default effective-parameter hash: `2dc2d2fd197f86ae9bdd126f2b0591efbf5c9343e06f17789ea8553685c2e380`
- Frozen source files: `128`
- Verified public reconstruction commit: `9741a81d84f29e8ebfaf8ec4e890a3124588fbcc` (`128/128` manifest paths and SHA-256 values match)

## Release identity

The per-file manifest and its aggregate SHA-256 are the authoritative identity
of the frozen algorithm bytes. The historical base SHA records the checked-out
repository base beneath the working tree from which the manifest was computed;
it does not by itself reconstruct all frozen bytes. Public commit
`9741a81d84f29e8ebfaf8ec4e890a3124588fbcc` was independently verified against
the manifest and provides an exact public reconstruction point.

The immutable historical tag `v1.2.2` remains at
`3c5ce65b4f97472a91d3e7f17fc26515fd00e7ce` and does not identify this freeze.
The corrected evidence tree is designated `v1.2.2-rev1`; the tag is created
only after the attested commit and is never repointed. This record does not
invent a future commit hash.

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
