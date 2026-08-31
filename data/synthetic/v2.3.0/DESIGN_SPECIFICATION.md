# Design specification v2.3.0

## Purpose and version boundary

v2.3.0 is a clean-overlap supplement designed to test whether a detector uses episode duration, regularity, local context, and Event/State structure when marginal ISI distributions overlap. It is released beside the frozen v2.2.0 mechanism benchmark. Results from the two releases must remain identifiable by dataset version.

The supplement retains only four primary targets: `Burst`, `Pause`, `Tonic`, and `Broad_HFS`. It adds no stimulation, slow drift, observation error, sorting error, or additional primary firing mode.

## Dimensionless generator and scale projection

All generation occurs in dimensionless time `u=t/B`, with anchor `B=0.100 s`. Twenty mother templates are generated once and projected by exact multiplication at scale factors 1, 4, and 10. Template IDs 1–5 are the development/calibration split; IDs 6–20 are the protected holdout split. The three projections of one template must remain in the same statistical cluster.

Tonic, Background, and Broad HFS are shifted-gamma renewal processes. Tonic and Broad HFS use the same refractory term, `0.020B`. Burst is a finite rate-multiplier pulse on the corresponding renewal background. Pause and secondary gap mechanisms are explicitly generated observed gaps.

## Tonic–HFS overlap contract

Rate-overlap strata are assigned before timestamp generation and independently of the later Tonic phenotype label.

| State/subtype | `core` mean ISI | `boundary` mean ISI | `deep` mean ISI |
|---|---:|---:|---:|
| Tonic `generic_stress` | `0.70–0.98B` | `0.46–0.68B` | `0.34–0.52B` |
| Tonic `stn_like_empirical` | `0.48–0.66B` | `0.38–0.56B` | `0.32–0.48B` |
| Broad HFS | `0.18–0.30B` | `0.26–0.38B` | `0.32–0.44B` |

Every template contains one Broad HFS State in each overlap stratum. A Latin-square rotation prevents overlap stratum from being fixed to the `regular`, `intermediate`, or `irregular` HFS regime. Tonic overlap strata are also pre-frozen across both subtypes. These ranges are hidden generator parameters, not detector thresholds.

Each Broad HFS State contains 20–35 boundary spikes and lasts at least `3B`. Direct HFS support, tolerated short interruptions, nested Burst support, and the outer HFS State envelope remain separate truth fields. The v2.3 slow-overlap support contract defines direct support through `0.65B` and tolerated interruptions through `0.85B`; its version is exported on HFS rows. A separate `<=0.45B` legacy v2.2-style direct-support mask is audit-only. Direct-support F1 is not compared across versions unless one common mask is applied. Nested Burst does not terminate the HFS State. Canonical Pause does.

## Burst context and contrast contract

Every template contains three standalone Bursts and two Bursts nested in HFS. Realized Burst size is pre-frozen at 4–9 spikes, with 12 four-spike boundary cases. No Burst may exceed 10 spikes.

The target mean-ISI contrast is sampled from pre-frozen ranges:

- weak: `2.2–3.0`;
- moderate: `3.0–4.0`;
- strong: `4.0–5.5`.

Pulse buffers are `2.2–3.5B`. Direct standalone Bursts and HFS-internal Bursts retain at least three observed context ISIs on each side. In the indivisible `Burst–contextual separator–Burst` macro, the separator supplies the inner boundary and at least three observed ISIs are retained on the outer side of each Burst. `Target_Mean_ISI_Contrast`, observed left/right shoulder counts, and refractory-limitation status are exported for audit.

When a contextual macro retains only the outer side of a pulse-generating run, its latent window is clipped to the retained run boundary so it cannot extend into the explicit separator. `Latent_Window_Truncated` records this structural clipping; realized spike support is unchanged.

## Pause and secondary-gap ontology

- **Canonical Pause:** primary `Pause` truth; exactly one observed ISI in `[2.8, 5.0]B`, with two observed boundary spikes. It hard-cuts a continuous HFS State.
- **Complex multi-gap Pause:** primary `Pause` truth; two or three component gaps with a total duration of `4.5–8.5B`.
- **Contextual separator:** secondary truth in `[0.45, 0.65]B`, generated inside an indivisible two-Burst macro and constrained relative to both local Burst medians.
- **Borderline gap:** secondary truth consisting of one observed ISI in `[0.72, 1.55]B`. It probes overlap with the Tonic tail but is never canonical or primary Pause truth.

Both secondary gap types carry `Product_Level_Mapping=Pause` and are scored separately from primary Pause. They must not enter the primary Pause macro-average.

The frozen scheduler also prevents two separately scored States with the same primary label from touching at a macro boundary. If a sampled order would create such an unobservable split, a pre-defined Background generator run is inserted and recorded in the template manifest. Direct boundaries between different targets remain available.

## Truth hierarchy and estimands

- `State_Envelope_ID`: continuous Tonic or Broad HFS State.
- `Event_ID`: primary Burst or primary canonical/complex Pause.
- `Composite_HFS_Regime_ID`: higher-order relation `HFS State -- canonical Pause -- HFS State`; it is never a continuous HFS State.
- `Secondary_Event_ID`: contextual separator or borderline gap.
- `Direct_HFS_Support` and `HFS_Interruption_Role`: direct, tolerated interruption, or nested Burst support.

All injected Events and generated States remain in the strict-mechanism estimand. The observable-phenotype estimand uses only clear injected-pulse Burst regions and phenotype-eligible Tonic States under pre-specified timestamp-only rules. Natural clear or ambiguous HFS null excursions are exported as secondary audit objects and never promoted to mechanism or primary observable Burst truth. Ambiguous regions are retained rather than deleted. `truth/phenotype_sensitivity_masks.csv` provides strict, observable-clear/eligible, ambiguous-excluded, ambiguous-as-positive, and null-excursion audit masks.

Phenotype labels are operational results under the frozen audit, not claims of universal biological observability.

## Development-only fitting and holdout audit

Burst evidence thresholds use 1,000 shifted-gamma null surrogates per threshold set and are fitted only with templates 1–5. Injected-event thresholds are matched by standalone/HFS context, realized window size, and usable flank pattern. The indivisible contextual macro uses a one-sided same-run outer flank, so its null is one-sided as well. A separate maximum-window HFS null is used for spontaneous null excursions. Templates 6–20 do not contribute to threshold selection.

The single-ISI baseline is also fitted on development templates and evaluated on holdout templates. It is a descriptive separability audit, with `Template_ID` as the bootstrap cluster, and has no acceptance target. Holdout performance never selects parameters, models, or samples. Pre-specified structural integrity checks apply to all templates, including holdout; this is distinct from performance-based tuning. Generator acceptance never depends on STPD output or an F1 improvement.

## Randomness and provenance

Seeds are keyed by `Template_ID × component × component slot × replicate` under a fixed namespace. Component-local ISI hashes are exported. Counterfactual tests require a Tonic-only, HFS-only, or borderline-gap-only parameter change to leave unrelated component-local sequences unchanged; downstream absolute offsets may change when a preceding component duration changes.

The semantic component keys were revised for v2.3, so v2.3 is a separately frozen related supplement rather than a component-level paired redraw of v2.2. The release lock records the exact generator, modules, parameters, key truth, detector inputs, and figures.

All release figures are rendered locally by the included R/ggplot2 code.
