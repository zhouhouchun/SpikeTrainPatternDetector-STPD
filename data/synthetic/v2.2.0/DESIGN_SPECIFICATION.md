# Design specification v2.2.0

## Statistical generator

All times are generated in dimensionless units `u=t/B`. Tonic, Background and Broad HFS use shifted-gamma renewal processes with a shared refractory term. Burst is a finite rate-multiplier pulse on that family. Pause is an explicit observed gap. No stimulation, observation error or slow drift is added.

Twenty templates are generated once, then projected by exact multiplication at scale factors 1, 4 and 10. Template IDs 1–5 are development/calibration; 6–20 are protected holdout. Scale copies of one Template_ID cannot be split statistically.

## Truth hierarchy

- `State_Envelope_ID`: continuous Tonic or Broad HFS State.
- `Event_ID`: Burst or canonical/complex Pause.
- `Composite_HFS_Regime_ID`: higher-order relation `HFS State -- canonical Pause -- HFS State`.
- `Direct_HFS_Support`: only direct high-frequency ISIs, excluding nested Burst.
- `HFS_Interruption_Role`: direct, tolerated short interruption, or nested Burst event.

Canonical Pause always has two observed boundary spikes, removes direct HFS support and hard-cuts the continuous HFS State. Contextual separators are separately generated secondary targets.

## Tonic phenotype contract

Each subtype is generated from a pre-frozen intended difficulty stratum. A detector-independent continuous score combines regularity (CV/CV2/LV), state length, local flank separation and subtype-relative rate band. Frozen score thresholds assign `eligible`, `ambiguous`, or `no_evidence`. All generated Tonic States remain in strict mechanism truth; only eligible states enter the primary observable-phenotype estimand. Counts and scores for every state are retained.

## RNG contract

Seeds are derived from `Template_ID × component × component slot × replicate` under a fixed namespace that excludes dataset version. Nested HFS pulses retain distinct HFS, nested-Burst and point-process stream IDs. Component-local ISI hashes are exported. The acceptance test perturbs only Tonic parameters and requires every non-Tonic local hash to remain identical; downstream absolute offsets may change when a preceding duration changes.

No acceptance or rejection rule reads STPD predictions or seeks an F1 increase.
