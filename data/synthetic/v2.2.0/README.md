# Clean synthetic mechanism benchmark v2.2.0

This frozen benchmark contains 20 dimensionless mother templates and their exact 1x, 4x and 10x time projections (60 detector inputs). The three projections are paired numerical scale tests, not 60 independent biological recordings.

Run `./run_reproduction.sh` to regenerate timestamps, XLSX, truth tables, QC tables and local R/ggplot2 figures, then execute the v2.2 acceptance suite. The entry point is `generate_benchmark_spike_trains.R`; the benchmark algorithm is in `R/`. The general Shiny simulator in `reference/` is provenance only and is not executed by this wrapper.

Key v2.2 semantics:

- State truth: `none`, `Tonic`, `Broad_HFS`; Event truth: `none`, `Burst`, `Pause`.
- Tonic has parallel strict-mechanism and observable-phenotype estimands. Eligibility is frozen from timestamps without STPD output.
- HFS direct support, short tolerated interruption and nested Burst are distinct. A canonical Pause hard-cuts the HFS State.
- `Composite_HFS_Regime_ID` may relate two separate HFS States across a canonical Pause; it never means that Pause belongs to a continuous HFS State.
- Contextual separators remain secondary truth and are not primary Pause positives.
- Component-keyed RNG streams make local Burst, Pause, HFS and Background draws invariant to Tonic-only parameter changes.

Detector-ready timestamps are in `detector_inputs/spike_timestamps_blinded.xlsx` and `.csv`. The XLSX sheets are blind scale batches; the truth mapping is intentionally separate in `truth/sample_template_scale_key.csv`.
