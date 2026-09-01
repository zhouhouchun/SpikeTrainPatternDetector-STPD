# PD GPe/STN/GPi three-regime validation

Automatic uses no manual examples. Partial-known is five-fold held-out
recording-group validation with at most ten training episodes per pattern.
Full-parameters is same-data resubstitution reported as a sensitivity analysis,
not a guaranteed performance upper bound.
GPi Tonic is a formal State endpoint; STN Tonic is descriptive review-only.

| Region | Regime | Level | Pattern | Precision | Recall | F1 (95% cluster CI) | Support |
|---|---|---|---|---:|---:|---:|---:|
| GPE | automatic | interval | Burst | 0.294 | 0.745 | 0.422 (0.160–0.609) | 1663 |
| GPE | automatic | interval | Pause | 0.195 | 0.888 | 0.320 (0.216–0.407) | 411 |
| GPE | automatic | interval | Broad HFS | 1.000 | 0.055 | 0.105 (0.065–0.137) | 27669 |
| GPE | automatic | event | Burst | 0.425 | 0.607 | 0.500 (0.218–0.648) | 399 |
| GPE | automatic | event | Pause | 0.204 | 0.835 | 0.328 (0.224–0.430) | 345 |
| GPE | automatic | event | Broad HFS | 0.167 | 0.085 | 0.112 (0.000–0.211) | 71 |
| GPE | partial_known | interval | Burst | 0.200 | 0.663 | 0.308 (0.110–0.503) | 1663 |
| GPE | partial_known | interval | Pause | 0.516 | 0.825 | 0.635 (0.485–0.733) | 411 |
| GPE | partial_known | interval | Broad HFS | 0.998 | 0.895 | 0.944 (0.869–0.979) | 27669 |
| GPE | partial_known | event | Burst | 0.279 | 0.549 | 0.370 (0.149–0.530) | 399 |
| GPE | partial_known | event | Pause | 0.478 | 0.843 | 0.610 (0.464–0.709) | 345 |
| GPE | partial_known | event | Broad HFS | 0.262 | 0.775 | 0.391 (0.189–0.558) | 71 |
| GPE | full_params | interval | Burst | 0.252 | 0.761 | 0.379 (0.145–0.594) | 1663 |
| GPE | full_params | interval | Pause | 0.491 | 0.764 | 0.598 (0.447–0.689) | 411 |
| GPE | full_params | interval | Broad HFS | 0.996 | 0.932 | 0.963 (0.914–0.985) | 27669 |
| GPE | full_params | event | Burst | 0.331 | 0.654 | 0.440 (0.188–0.618) | 399 |
| GPE | full_params | event | Pause | 0.457 | 0.794 | 0.581 (0.431–0.677) | 345 |
| GPE | full_params | event | Broad HFS | 0.309 | 0.845 | 0.453 (0.258–0.616) | 71 |
| STN | automatic | interval | Burst | 0.787 | 0.857 | 0.821 (0.689–0.896) | 7486 |
| STN | automatic | interval | Pause | 0.815 | 0.709 | 0.758 (0.694–0.804) | 2546 |
| STN | automatic | interval | Broad HFS | 0.526 | 0.012 | 0.024 (0.000–0.082) | 4131 |
| STN | automatic | event | Burst | 0.872 | 0.833 | 0.852 (0.757–0.903) | 1845 |
| STN | automatic | event | Pause | 0.799 | 0.774 | 0.786 (0.705–0.842) | 2014 |
| STN | automatic | event | Broad HFS | 0.000 | 0.000 | 0.000 (0.000–0.000) | 35 |
| STN | partial_known | interval | Burst | 0.805 | 0.735 | 0.768 (0.635–0.836) | 7486 |
| STN | partial_known | interval | Pause | 0.674 | 0.701 | 0.687 (0.582–0.767) | 2546 |
| STN | partial_known | interval | Broad HFS | 0.767 | 0.907 | 0.831 (0.402–0.953) | 4131 |
| STN | partial_known | event | Burst | 0.826 | 0.641 | 0.722 (0.612–0.774) | 1845 |
| STN | partial_known | event | Pause | 0.677 | 0.732 | 0.704 (0.592–0.789) | 2014 |
| STN | partial_known | event | Broad HFS | 0.586 | 0.486 | 0.531 (0.267–0.700) | 35 |
| STN | full_params | interval | Burst | 0.821 | 0.735 | 0.775 (0.661–0.840) | 7486 |
| STN | full_params | interval | Pause | 0.758 | 0.806 | 0.781 (0.698–0.830) | 2546 |
| STN | full_params | interval | Broad HFS | 0.786 | 0.890 | 0.835 (0.392–0.962) | 4131 |
| STN | full_params | event | Burst | 0.856 | 0.636 | 0.730 (0.649–0.776) | 1845 |
| STN | full_params | event | Pause | 0.756 | 0.842 | 0.797 (0.705–0.855) | 2014 |
| STN | full_params | event | Broad HFS | 0.465 | 0.571 | 0.513 (0.261–0.640) | 35 |
| GPI | automatic | interval | Burst | 0.589 | 0.625 | 0.606 (0.549–0.656) | 4779 |
| GPI | automatic | interval | Pause | 0.365 | 0.944 | 0.526 (0.434–0.615) | 655 |
| GPI | automatic | interval | Broad HFS | 1.000 | 0.032 | 0.063 (0.012–0.121) | 16036 |
| GPI | automatic | interval | Tonic | 0.071 | 0.236 | 0.109 (0.000–0.235) | 495 |
| GPI | automatic | event | Burst | 0.815 | 0.531 | 0.643 (0.578–0.695) | 1127 |
| GPI | automatic | event | Pause | 0.391 | 0.854 | 0.536 (0.453–0.616) | 512 |
| GPI | automatic | event | Broad HFS | 0.071 | 0.023 | 0.034 (0.000–0.113) | 44 |
| GPI | automatic | event | Tonic | 0.015 | 0.200 | 0.029 (0.000–0.060) | 30 |
| GPI | partial_known | interval | Burst | 0.558 | 0.689 | 0.617 (0.493–0.724) | 4779 |
| GPI | partial_known | interval | Pause | 0.629 | 0.786 | 0.699 (0.566–0.797) | 655 |
| GPI | partial_known | interval | Broad HFS | 0.929 | 0.956 | 0.943 (0.900–0.969) | 16036 |
| GPI | partial_known | interval | Tonic | 0.625 | 0.020 | 0.039 (0.000–0.065) | 495 |
| GPI | partial_known | event | Burst | 0.668 | 0.595 | 0.630 (0.546–0.693) | 1127 |
| GPI | partial_known | event | Pause | 0.583 | 0.783 | 0.668 (0.540–0.777) | 512 |
| GPI | partial_known | event | Broad HFS | 0.295 | 0.750 | 0.423 (0.290–0.600) | 44 |
| GPI | partial_known | event | Tonic | 1.000 | 0.033 | 0.065 (0.000–0.125) | 30 |
| GPI | full_params | interval | Burst | 0.679 | 0.704 | 0.691 (0.597–0.758) | 4779 |
| GPI | full_params | interval | Pause | 0.612 | 0.756 | 0.676 (0.549–0.770) | 655 |
| GPI | full_params | interval | Broad HFS | 0.947 | 0.953 | 0.950 (0.917–0.973) | 16036 |
| GPI | full_params | interval | Tonic | 0.000 | 0.000 | 0.000 (0.000–0.000) | 495 |
| GPI | full_params | event | Burst | 0.723 | 0.614 | 0.664 (0.594–0.710) | 1127 |
| GPI | full_params | event | Pause | 0.555 | 0.750 | 0.638 (0.518–0.742) | 512 |
| GPI | full_params | event | Broad HFS | 0.293 | 0.818 | 0.431 (0.276–0.662) | 44 |
| GPI | full_params | event | Tonic | 0.000 | 0.000 | 0.000 (0.000–0.000) | 30 |
