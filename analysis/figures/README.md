# Reproducible scientific figures

All scientific figures distributed with the public repository are produced by
R scripts. The scripts use tabular inputs committed under `results/`; no image
file is treated as primary analytical evidence.

## Figure map

| Output | R script | Input |
|---|---|---|
| `real_validation_interval_f1.*` | `plot_validation_summaries.R` | `results/real_validation/primary_f1_summary.csv` |
| `real_validation_event_f1.*` | `plot_validation_summaries.R` | `results/real_validation/primary_f1_summary.csv` |
| `synthetic_v2_3_f1.*` | `plot_validation_summaries.R` | `results/synthetic_validation/v2.3.0/template_cluster_bootstrap_95ci.csv` |
| `runtime_three_region_automatic.*` | `plot_performance_runtime.R` | `results/performance/real_three_region_runtime.csv` |
| `runtime_one_train_profiles.*` | `plot_performance_runtime.R` | repeated core/app-default runtime rows |
| Synthetic benchmark QC figures | `data/synthetic/v2.3.0/R/40_exports_and_figures.R` | frozen v2.3.0 benchmark tables |

Run from the public repository root:

```r
source("analysis/figures/plot_validation_summaries.R")
source("analysis/figures/plot_performance_runtime.R")
```

The script writes vector PDF and 300-dpi PNG files to
`analysis/figures/generated/`. It uses a color-vision-deficiency-friendly
palette and does not place interpretation text inside the plotting area.
