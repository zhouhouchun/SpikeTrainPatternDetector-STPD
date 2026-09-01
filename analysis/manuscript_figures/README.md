# Reproducible manuscript figures

All six manuscript figures are generated from R scripts in this directory.
The plotting scripts use only committed result tables or fixed schematic
timestamps. No generated image is the primary analytical evidence.

| Manuscript figure | Script | Main input |
|---|---|---|
| Figure 1, pattern schematic | `plot_pattern_schematic.R` | Fixed illustrative timestamps and regimes declared in the script |
| Figure 2, auditable workflow | `build_manuscript_figures.R` | Declared software workflow; no quantitative input |
| Figure 3, primary event evidence | `build_manuscript_figures.R` | Synthetic validation and real-reference result tables under `results/` |
| Figure 4, three-method real burst agreement | `build_manuscript_figures.R` | `results/method_comparison/three_method_truth_accuracy_current/` |
| Figure 5, structural comparison | `plot_three_method_structure.R` | `event_metrics.csv` from the same comparison directory |
| Figure 6, event-error decomposition | `build_manuscript_figures.R` | Frozen fragmentation, merge and boundary-error tables |

Run from the repository root:

```r
source("analysis/manuscript_figures/plot_pattern_schematic.R")
source("analysis/manuscript_figures/build_manuscript_figures.R")
source("analysis/manuscript_figures/plot_three_method_structure.R")
```

Outputs are written to `analysis/manuscript_figures/generated/` as vector PDF
and high-resolution PNG. Figure 5 also includes TIFF and a compact source CSV.
The Figure 1 source timestamps and regime intervals are exported as CSV files.

Figure 1 is schematic. It illustrates known regimes and is not used in any
accuracy estimate. Quantitative figures read frozen evaluation outputs.
