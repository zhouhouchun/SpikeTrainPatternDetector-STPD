# Unified three-method Burst accuracy

All three automatic methods are rescored on identical truth records and scoring rules. No truth label is used to estimate a detector threshold.

## ISI-support metrics

| Dataset | Region | Scale | Method | Precision | Recall | F1 [95% cluster-bootstrap CI] |
|---|---|---|---|---:|---:|---:|
| real_GPe | GPe | observed | LogISI/newBD | 0.072 | 0.998 | 0.134 [0.038, 0.287] |
| real_GPe | GPe | observed | Mean-ISI | 0.086 | 0.622 | 0.151 [0.034, 0.341] |
| real_GPe | GPe | observed | STPD | 0.303 | 0.745 | 0.431 [0.174, 0.612] |
| real_GPi | GPi | observed | LogISI/newBD | 0.303 | 0.998 | 0.464 [0.337, 0.603] |
| real_GPi | GPi | observed | Mean-ISI | 0.331 | 0.498 | 0.398 [0.283, 0.529] |
| real_GPi | GPi | observed | STPD | 0.642 | 0.625 | 0.633 [0.564, 0.696] |
| real_STN | STN | observed | LogISI/newBD | 0.564 | 0.965 | 0.712 [0.467, 0.864] |
| real_STN | STN | observed | Mean-ISI | 0.742 | 0.763 | 0.752 [0.565, 0.859] |
| real_STN | STN | observed | STPD | 0.827 | 0.857 | 0.842 [0.679, 0.922] |
| synthetic_v2.3_holdout | synthetic | 10x | LogISI/newBD | 0.904 | 0.196 | 0.323 [0.216, 0.419] |
| synthetic_v2.3_holdout | synthetic | 10x | Mean-ISI | 0.329 | 1.000 | 0.495 [0.477, 0.512] |
| synthetic_v2.3_holdout | synthetic | 10x | STPD | 0.510 | 0.917 | 0.655 [0.628, 0.683] |
| synthetic_v2.3_holdout | synthetic | 1x | LogISI/newBD | 0.192 | 1.000 | 0.323 [0.309, 0.340] |
| synthetic_v2.3_holdout | synthetic | 1x | Mean-ISI | 0.329 | 1.000 | 0.495 [0.477, 0.514] |
| synthetic_v2.3_holdout | synthetic | 1x | STPD | 0.507 | 0.912 | 0.652 [0.624, 0.682] |
| synthetic_v2.3_holdout | synthetic | 4x | LogISI/newBD | 0.429 | 0.928 | 0.586 [0.510, 0.672] |
| synthetic_v2.3_holdout | synthetic | 4x | Mean-ISI | 0.329 | 1.000 | 0.495 [0.476, 0.514] |
| synthetic_v2.3_holdout | synthetic | 4x | STPD | 0.507 | 0.912 | 0.652 [0.620, 0.679] |

## Event metrics (IoU >= 0.25)

| Dataset | Region | Scale | Method | Precision | Recall | F1 [95% cluster-bootstrap CI] | Mean matched IoU |
|---|---|---|---|---:|---:|---:|---:|
| real_GPe | GPe | observed | LogISI/newBD | 0.351 | 0.168 | 0.227 [0.092, 0.381] | 0.874 |
| real_GPe | GPe | observed | Mean-ISI | 0.167 | 0.524 | 0.253 [0.062, 0.471] | 0.787 |
| real_GPe | GPe | observed | STPD | 0.442 | 0.632 | 0.520 [0.231, 0.660] | 0.809 |
| real_GPi | GPi | observed | LogISI/newBD | 0.574 | 0.312 | 0.405 [0.276, 0.519] | 0.869 |
| real_GPi | GPi | observed | Mean-ISI | 0.464 | 0.468 | 0.466 [0.364, 0.575] | 0.794 |
| real_GPi | GPi | observed | STPD | 0.839 | 0.566 | 0.676 [0.597, 0.738] | 0.785 |
| real_STN | STN | observed | LogISI/newBD | 0.708 | 0.578 | 0.636 [0.530, 0.746] | 0.746 |
| real_STN | STN | observed | Mean-ISI | 0.821 | 0.722 | 0.768 [0.634, 0.836] | 0.881 |
| real_STN | STN | observed | STPD | 0.885 | 0.859 | 0.872 [0.762, 0.923] | 0.971 |
| synthetic_v2.3_holdout | synthetic | 10x | LogISI/newBD | 0.947 | 0.240 | 0.383 [0.253, 0.495] | 0.715 |
| synthetic_v2.3_holdout | synthetic | 10x | Mean-ISI | 0.461 | 0.627 | 0.531 [0.462, 0.607] | 0.509 |
| synthetic_v2.3_holdout | synthetic | 10x | STPD | 0.452 | 1.000 | 0.622 [0.591, 0.655] | 0.805 |
| synthetic_v2.3_holdout | synthetic | 1x | LogISI/newBD | 0.192 | 0.200 | 0.196 [0.114, 0.270] | 0.380 |
| synthetic_v2.3_holdout | synthetic | 1x | Mean-ISI | 0.461 | 0.627 | 0.531 [0.457, 0.601] | 0.509 |
| synthetic_v2.3_holdout | synthetic | 1x | STPD | 0.446 | 0.987 | 0.614 [0.579, 0.649] | 0.810 |
| synthetic_v2.3_holdout | synthetic | 4x | LogISI/newBD | 0.589 | 0.747 | 0.659 [0.546, 0.747] | 0.809 |
| synthetic_v2.3_holdout | synthetic | 4x | Mean-ISI | 0.461 | 0.627 | 0.531 [0.463, 0.608] | 0.509 |
| synthetic_v2.3_holdout | synthetic | 4x | STPD | 0.446 | 0.987 | 0.614 [0.581, 0.647] | 0.810 |

All precision, recall, and F1 intervals are available in `cluster_bootstrap_95ci.csv` and `event_cluster_bootstrap_95ci.csv`.

## Safeguards

- Real estimates are exploratory comparisons on the frozen reviewed reference, not clinical performance estimates.
- GPe, STN, and GPi are not pooled as independent patients.
- Real resampling uses recording Group_ID; all member trains remain together.
- Synthetic scales are reported separately and share Template_ID bootstrap clusters.
