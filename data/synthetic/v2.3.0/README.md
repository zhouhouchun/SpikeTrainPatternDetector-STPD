# Clean synthetic mechanism benchmark v2.3.0

v2.3.0 is a frozen **clean-overlap supplement** to v2.2.0. It does not overwrite, replace, or retrospectively relabel the v2.2.0 release. The supplement increases controlled overlap among the four existing targets while preserving the clean mechanism setting.

The release contains 20 dimensionless mother templates and their exact 1x, 4x, and 10x time projections, yielding 60 detector inputs. The three projections of one `Template_ID` have identical spike counts, pattern order, truth IDs, and normalized ISI sequences. They are paired numerical scale tests, not 60 independent biological recordings.

## What v2.3 changes

- Tonic and Broad HFS remain shifted-gamma renewal States with the same refractory term. Pre-frozen `core`, `boundary`, and `deep` rate-overlap strata increase their marginal ISI overlap without defining either class by one absolute ISI threshold.
- Burst remains a finite rate-multiplier pulse. Weak, moderate, and strong target-contrast strata are frozen, and observed context shoulders are retained so that Burst evidence can be audited against its local background.
- Canonical Pause is unchanged: it is a primary Event consisting of one observed gap in `[2.8, 5.0]B`, bounded by two observed spikes. Complex multi-gap Pause also remains primary truth.
- Contextual inter-Burst separators and the new Tonic-tail-overlap borderline gaps are secondary semantic targets only. Neither is a primary Pause positive, although both may map to `Pause` at the product layer.
- Strict-mechanism, observable-phenotype, ambiguous, and ambiguous-as-positive interval masks are exported explicitly.
- Burst phenotype thresholds are fitted only from development templates and are matched by event context, realized window size, and usable flank pattern. Holdout performance metrics are used only for frozen descriptive audit and detector evaluation; the same pre-specified structural-integrity checks are necessarily applied to every template. Natural HFS null excursions remain a separate secondary audit and never enter the primary observable-Burst positives.

No STPD prediction is read during generation, threshold fitting, phenotype qualification, rejection, or acceptance. No stimulation, slow drift, missing or false spikes, timestamp jitter, sorting error, or other observation noise is added. This release is therefore not a realistic acquisition benchmark or a biological gold standard.

## Reproduction and files

Run `./run_reproduction.sh` to regenerate timestamps, XLSX, truth tables, metadata, QC tables, locally rendered R/ggplot2 figures, and the v2.3 acceptance checks. The entry point is `generate_benchmark_spike_trains.R`; the benchmark implementation is in `R/`. The general simulator under `reference/` is provenance only and is not executed by this wrapper. `RELEASE_LOCK_SHA256.csv` is an immutable release anchor: the generator never rewrites it, and the verification suite rejects any source or key-output drift from that lock.

Detector-ready timestamps are in `detector_inputs/spike_timestamps_blinded.xlsx` and `.csv`. The workbook sheets are blinded scale batches; their mapping is stored separately in `truth/sample_template_scale_key.csv`. Primary truth, secondary semantic targets, and sensitivity masks are kept outside the detector input.

v2.3 uses a new semantic RNG-key layout and is not a component-for-component redraw of v2.2. Cross-version results may therefore be compared as separately frozen benchmarks, but individual timestamp changes must not be attributed to only one modified component.
