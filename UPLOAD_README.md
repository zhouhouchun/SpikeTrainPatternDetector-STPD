# STPD code-and-materials upload bundle

Author and repository maintainer: Zhou Houchun

This directory is the upload-ready code and evidence package for the
Neuroinformatics revision of Spike Train Pattern Detector (STPD). It starts
from public tag `v1.2.2-rev1` at commit
`77e53daeead789399db259c54a4e36c40d20e703` and adds only packaging,
documentation, figure-reproduction, and public-input reconstruction materials.
The frozen detector algorithm bytes are unchanged.

## Included

- R package source (`R/`, `src/`, `inst/`, `man/`, `NAMESPACE`, `DESCRIPTION`)
- automated and regression tests (`tests/`)
- synthetic benchmark releases v2.2.0 and v2.3.0, including generator source,
  blinded inputs, multitrack truth, and QC (`data/synthetic/`)
- de-identified GPe, STN, and GPi timestamp/annotation workbooks under
  region-only filenames (`data/real/`)
- automatic, partial-known recording-group-held-out, full-parameter
  sensitivity, three-method burst-comparison, diagnostic, and runtime scripts
  (`evaluation/`)
- compact frozen result tables, bootstrap confidence intervals, structural
  error summaries, runtime metadata, and release-freeze evidence (`results/`)
- R source and generated outputs for all six manuscript figures
  (`analysis/manuscript_figures/`)
- SHA-256 manifests and public-release documentation

## Intentionally excluded

- patient names and direct identifiers
- private clinical records or waveform data not used in the reported analyses
- manuscript drafts, reviewer correspondence, journal submission forms, and
  internal working notes
- large temporary objects, exploratory reruns, browser dependencies, caches,
  and local credentials
- the DBS visualization module, which is outside the reported STPD scope

The clean manuscript and point-by-point response belong in the journal
submission system, not in this public code/data repository.

## Reproduce core materials

Install and launch the package:

```r
remotes::install_github(
  "zhouhouchun/SpikeTrainPatternDetector-STPD@v1.2.2-rev1",
  upgrade = "never"
)
SpikeTrainPatternDetector::run_spike_train_pattern_detector()
```

Recreate the label-blind runtime input and manuscript figures from a local
checkout:

```bash
Rscript evaluation/performance/prepare_public_runtime_input.R
Rscript analysis/manuscript_figures/plot_pattern_schematic.R
Rscript analysis/manuscript_figures/build_manuscript_figures.R
Rscript analysis/manuscript_figures/plot_three_method_structure.R
```

The original full regression evidence is recorded in
`results/release_freeze/`. Re-running every validation analysis is optional for
repository upload because the compact frozen outputs and their identities are
already included.

## Identity and licences

`results/release_freeze/FINAL_ALGORITHM_FREEZE.csv` is authoritative for the
128 frozen algorithm files. `release_manifest_sha256.csv` authenticates the
original public `v1.2.2-rev1` tree. `UPLOAD_MANIFEST_SHA256.csv` authenticates
the complete prepared directory after the additional upload materials were
added.

The MIT licence applies to software. Human-derived workbooks have separate
provenance and use terms in `data/real/SOURCE_AND_LICENSE.md` and
`data/real/DATA_USE_TERMS.md`.

## Recommended GitHub action

Review `UPLOAD_QA_REPORT.md`, then commit the staged additions as a new commit
after `v1.2.2-rev1`. Do not move or rewrite the immutable `v1.2.2` or
`v1.2.2-rev1` tags. A new evidence tag or GitHub Release may be created for the
final revision package after the manuscript and response cite the same commit.
