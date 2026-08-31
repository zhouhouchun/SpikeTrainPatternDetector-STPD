# Publication-validation diagnostics

These scripts may rerun the detector and therefore live with publication
validation results rather than in the standalone `evaluation/` module.

- [`real_tonic_miss_trace/`](real_tonic_miss_trace/README.md): label-blind LOGO
  trace of the prespecified fold-7 and fold-8 real-data Tonic misses, from raw
  candidate generation through automatic multi-track materialization.
- [`tonic_review_mechanism_burden/`](tonic_review_mechanism_burden/README.md):
  small, non-LOGO mechanism check for the separate review-only Tonic proposer,
  including pure-HFS, oscillatory, and canonical-Tonic negative controls.
