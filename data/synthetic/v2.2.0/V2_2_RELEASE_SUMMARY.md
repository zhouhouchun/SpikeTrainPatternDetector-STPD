# v2.2.0 release summary

The frozen output contains 20 mother templates, 60 exact paired scale projections, 100 Burst episodes (60 standalone, 40 nested in HFS), 50 primary Pause episodes (40 canonical, 10 complex), 40 Tonic States and 60 Broad HFS States. Burst contains at most 9 realized spikes; 12 episodes contain exactly 4 spikes. Contextual separators are secondary truth only.

At the anchor scale, mother templates contain 125–157 spikes and last 38.845–62.534 B units. Realized Burst duration is 0.1274–0.8301 B, below the 3 B minimum HFS duration.

Tonic timestamp-only phenotype audit:

| Subtype | Eligible | Ambiguous | No evidence |
|---|---:|---:|---:|
| generic_stress | 15 | 2 | 3 |
| stn_like_empirical | 13 | 4 | 3 |
| Total | 28 | 6 | 6 |

All 40 Tonic States remain in strict mechanism truth. The 28 eligible States form the observable Tonic estimand; the other 12 are retained and reported, not deleted.

Across HFS intervals, 1,527 are direct support, 65 are tolerated short interruptions, and 221 are nested Burst support. HFS envelope boundary audit classifies 17 states as clear/hard-cut, 14 as ambiguous/edge, and 29 as having no evidence at one or more boundaries. These difficult boundaries remain in strict mechanism truth; the audit is descriptive and was not optimized against STPD.

Ten templates contain a higher-order Composite HFS Regime. Every linked canonical Pause separates two different HFS State envelope IDs and is excluded from direct HFS support.

`tests/verify_v2_2.R` passed all mechanism, phenotype, RNG-isolation, scale, calibration, XLSX and checksum contracts. Two complete calls to `run_reproduction.sh` produced identical canonical-checksum, XLSX and main-figure SHA-256 hashes.
