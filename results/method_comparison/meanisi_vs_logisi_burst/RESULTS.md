# Mean-ISI versus LogISI/newBD Burst comparison

This is a direct truth-referenced comparison. Neither method is imported into STPD, and no reference label is used to estimate a threshold.

Primary article-style defaults: Mean-ISI requires at least 3 spikes; LogISI/newBD requires at least 5 spikes. A harmonized >=4-spike sensitivity analysis is also exported.

## ISI-support headline metrics

| Dataset | Region | Estimand | Scale | Method | Precision | Recall | F1 | Accuracy | Balanced accuracy |
|---|---|---|---|---|---:|---:|---:|---:|---:|
| real_GPe | GPe | reviewed_manual_reference | observed | LogISI/newBD | 0.072 | 0.998 | 0.134 | 0.078 | 0.502 |
| real_GPe | GPe | reviewed_manual_reference | observed | Mean-ISI | 0.086 | 0.622 | 0.151 | 0.501 | 0.557 |
| real_GPi | GPi | reviewed_manual_reference | observed | LogISI/newBD | 0.303 | 0.998 | 0.464 | 0.314 | 0.511 |
| real_GPi | GPi | reviewed_manual_reference | observed | Mean-ISI | 0.331 | 0.498 | 0.398 | 0.551 | 0.535 |
| real_STN | STN | reviewed_manual_reference | observed | LogISI/newBD | 0.564 | 0.965 | 0.712 | 0.578 | 0.544 |
| real_STN | STN | reviewed_manual_reference | observed | Mean-ISI | 0.742 | 0.763 | 0.752 | 0.728 | 0.725 |
| synthetic_v2.3_holdout | synthetic | observable_phenotype_primary | 10x | LogISI/newBD | 0.947 | 0.210 | 0.344 | 0.857 | 0.604 |
| synthetic_v2.3_holdout | synthetic | observable_phenotype_primary | 10x | Mean-ISI | 0.327 | 1.000 | 0.493 | 0.633 | 0.777 |
| synthetic_v2.3_holdout | synthetic | observable_phenotype_primary | 1x | LogISI/newBD | 0.190 | 1.000 | 0.319 | 0.238 | 0.536 |
| synthetic_v2.3_holdout | synthetic | observable_phenotype_primary | 1x | Mean-ISI | 0.327 | 1.000 | 0.493 | 0.633 | 0.777 |
| synthetic_v2.3_holdout | synthetic | observable_phenotype_primary | 4x | LogISI/newBD | 0.427 | 0.931 | 0.585 | 0.764 | 0.830 |
| synthetic_v2.3_holdout | synthetic | observable_phenotype_primary | 4x | Mean-ISI | 0.327 | 1.000 | 0.493 | 0.633 | 0.777 |
| synthetic_v2.3_holdout | synthetic | strict_mechanism | 10x | LogISI/newBD | 0.904 | 0.196 | 0.323 | 0.851 | 0.596 |
| synthetic_v2.3_holdout | synthetic | strict_mechanism | 10x | Mean-ISI | 0.329 | 1.000 | 0.495 | 0.631 | 0.775 |
| synthetic_v2.3_holdout | synthetic | strict_mechanism | 1x | LogISI/newBD | 0.192 | 1.000 | 0.323 | 0.240 | 0.536 |
| synthetic_v2.3_holdout | synthetic | strict_mechanism | 1x | Mean-ISI | 0.329 | 1.000 | 0.495 | 0.631 | 0.775 |
| synthetic_v2.3_holdout | synthetic | strict_mechanism | 4x | LogISI/newBD | 0.429 | 0.928 | 0.586 | 0.763 | 0.827 |
| synthetic_v2.3_holdout | synthetic | strict_mechanism | 4x | Mean-ISI | 0.329 | 1.000 | 0.495 | 0.631 | 0.775 |

## Event-level headline metrics (IoU >= 0.25)

| Dataset | Region | Estimand | Scale | Method | Precision | Recall | F1 | Mean matched IoU | Fragmentation | Merge |
|---|---|---|---|---|---:|---:|---:|---:|---:|---:|
| real_GPe | GPe | reviewed_manual_reference | observed | LogISI/newBD | 0.351 | 0.168 | 0.227 | 0.874 | 0.000 | 0.257 |
| real_GPe | GPe | reviewed_manual_reference | observed | Mean-ISI | 0.167 | 0.524 | 0.253 | 0.787 | 0.010 | 0.020 |
| real_GPi | GPi | reviewed_manual_reference | observed | LogISI/newBD | 0.574 | 0.312 | 0.405 | 0.869 | 0.002 | 0.147 |
| real_GPi | GPi | reviewed_manual_reference | observed | Mean-ISI | 0.464 | 0.468 | 0.466 | 0.794 | 0.027 | 0.023 |
| real_STN | STN | reviewed_manual_reference | observed | LogISI/newBD | 0.708 | 0.578 | 0.636 | 0.746 | 0.023 | 0.201 |
| real_STN | STN | reviewed_manual_reference | observed | Mean-ISI | 0.821 | 0.722 | 0.768 | 0.881 | 0.025 | 0.027 |
| synthetic_v2.3_holdout | synthetic | observable_phenotype_primary | 10x | LogISI/newBD | 1.000 | 0.268 | 0.422 | 0.701 | 0.000 | 0.000 |
| synthetic_v2.3_holdout | synthetic | observable_phenotype_primary | 10x | Mean-ISI | 0.448 | 0.662 | 0.534 | 0.518 | 0.000 | 0.124 |
| synthetic_v2.3_holdout | synthetic | observable_phenotype_primary | 1x | LogISI/newBD | 0.207 | 0.239 | 0.222 | 0.385 | 0.000 | 0.195 |
| synthetic_v2.3_holdout | synthetic | observable_phenotype_primary | 1x | Mean-ISI | 0.448 | 0.662 | 0.534 | 0.518 | 0.000 | 0.124 |
| synthetic_v2.3_holdout | synthetic | observable_phenotype_primary | 4x | LogISI/newBD | 0.567 | 0.775 | 0.655 | 0.803 | 0.000 | 0.031 |
| synthetic_v2.3_holdout | synthetic | observable_phenotype_primary | 4x | Mean-ISI | 0.448 | 0.662 | 0.534 | 0.518 | 0.000 | 0.124 |
| synthetic_v2.3_holdout | synthetic | strict_mechanism | 10x | LogISI/newBD | 0.947 | 0.240 | 0.383 | 0.715 | 0.000 | 0.000 |
| synthetic_v2.3_holdout | synthetic | strict_mechanism | 10x | Mean-ISI | 0.461 | 0.627 | 0.531 | 0.509 | 0.000 | 0.147 |
| synthetic_v2.3_holdout | synthetic | strict_mechanism | 1x | LogISI/newBD | 0.192 | 0.200 | 0.196 | 0.380 | 0.000 | 0.244 |
| synthetic_v2.3_holdout | synthetic | strict_mechanism | 1x | Mean-ISI | 0.461 | 0.627 | 0.531 | 0.509 | 0.000 | 0.147 |
| synthetic_v2.3_holdout | synthetic | strict_mechanism | 4x | LogISI/newBD | 0.589 | 0.747 | 0.659 | 0.809 | 0.000 | 0.032 |
| synthetic_v2.3_holdout | synthetic | strict_mechanism | 4x | Mean-ISI | 0.461 | 0.627 | 0.531 | 0.509 | 0.000 | 0.147 |

## Interpretation safeguards

- Accuracy is secondary because non-Burst ISIs are much more frequent and can inflate it.
- The primary discrimination metrics are precision, recall, F1, balanced accuracy, and event-level IoU performance.
- The three synthetic scales are repeated projections of the same templates; Template_ID, not Sample_ID, is the bootstrap unit.
- Real workbooks remain manual drafts. Real-data estimates are exploratory until annotation sealing and provenance confirmation.
- Results at 1x, 4x, and 10x are kept separate; they are not treated as independent biological replicates.
