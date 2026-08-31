# Current Mean-ISI / LogISI-newBD / STPD Burst agreement

This analysis measures agreement among method outputs. It is distinct from truth-referenced accuracy.

| Region | Pair | Event F1 (IoU >= 0.25) | Mean matched IoU | ISI Jaccard | ISI Dice |
|---|---|---:|---:|---:|---:|
| GPE | LogISI/newBD vs STPD | 0.130 | 0.615 | 0.180 | 0.305 |
| GPE | Mean-ISI vs LogISI/newBD | 0.073 | 0.466 | 0.515 | 0.680 |
| GPE | Mean-ISI vs STPD | 0.280 | 0.613 | 0.113 | 0.203 |
| GPI | LogISI/newBD vs STPD | 0.168 | 0.544 | 0.295 | 0.455 |
| GPI | Mean-ISI vs LogISI/newBD | 0.147 | 0.549 | 0.444 | 0.615 |
| GPI | Mean-ISI vs STPD | 0.388 | 0.649 | 0.197 | 0.329 |
| STN | LogISI/newBD vs STPD | 0.185 | 0.611 | 0.509 | 0.675 |
| STN | Mean-ISI vs LogISI/newBD | 0.223 | 0.570 | 0.510 | 0.675 |
| STN | Mean-ISI vs STPD | 0.750 | 0.836 | 0.566 | 0.722 |

## Safeguards

- Agreement is not sensitivity, specificity, or accuracy.
- Manual labels determine the eligible train scope only; no label enters detection.
- STPD is the current automatic zero-manual-example run; support methods use their article-style defaults.
- GPe, STN, and GPi are reported separately and are not pooled as independent patients.
