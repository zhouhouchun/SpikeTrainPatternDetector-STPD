# Review-only Tonic mechanism burden

This diagnostic exercises the two locally diagnosed Tonic miss mechanisms and
three negative controls without running the full real-data LOGO validation.
It calls the separate `tonic_review_candidates` proposer directly and records
both intended-target recovery and extra human-review burden.

The proposer is not an evaluator and does not change canonical labels, candidate
audits, multitrack State selection, or publication metrics. Every emitted row
has `review_only = TRUE` and `canonical_eligible = FALSE`.

Reproduce from the repository root:

```sh
Rscript evaluation/publication_validation/diagnostics/tonic_review_mechanism_burden/run_burden.R
```

Outputs:

- `mechanism_burden_summary.csv`: per-mechanism candidate and excess-review counts.
- `review_candidate_details.csv`: complete proposal evidence and geometry.
- `reproducibility_manifest.csv`: SHA-256 hashes of the two result tables.
