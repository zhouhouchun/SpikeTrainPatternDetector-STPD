# Data dictionary v2.2.0

- `detector_inputs/spike_timestamps_blinded.*`: only `Sample_ID`, `Spike_Index`, `Time_s`.
- `truth/interval_truth_multitrack.csv`: one row per observed ISI with State, Event, HFS support/interruption, Tonic phenotype and Regime audit fields.
- `truth/primary_event_episodes.csv`: strict injected Burst and observed Pause episodes with latent and realized Burst boundaries.
- `truth/state_envelopes.csv`: strict Tonic/Broad HFS States and HFS boundary observability.
- `truth/strict_mechanism_estimand_states.csv`: every generated State at all scales.
- `truth/observable_phenotype_estimand_states.csv`: all HFS States plus only phenotype-eligible Tonic States.
- `truth/tonic_phenotype_evidence.csv`: continuous Tonic evidence features, label and reason at all scales.
- `truth/composite_hfs_regimes.csv`: higher-order links across canonical Pause; `Continuous_HFS_State` is always false.
- `truth/contextual_separator_secondary_episodes.csv`: secondary separator truth, never primary Pause.
- `truth/observable_phenotype.csv`: Burst-like evidence including injected pulses and null-model excursions.
- `metadata/component_rng_registry.csv`: component keys, stream registry and local ISI SHA-256.
- `metadata/generator_parameters.{json,yaml,csv}`: publication parameters.
- `metadata/reproduction_manifest.json` and `canonical_output_checksums_sha256.csv`: provenance and frozen output hashes.

`Template_ID` is the pairing/bootstrap cluster. `Sample_ID` is blinded. `_u` columns are dimensionless; `_s` columns are exact scaled seconds.
