# PS/RGS auxiliary support: validation status

This record distinguishes tests that are executable in this repository from
claims that would require an independent reference implementation.

## Scope

The Poisson-surprise (PS) and Robust Gaussian Surprise (RGS) modules are
auxiliary evidence providers. They do not replace STPD AUTO labels, modify
STPD thresholds, or participate in final candidate arbitration.

## Executable checks in this repository

- PS probability calculations are checked against `stats::ppois()` for the
  declared pCLAMP-style count convention (`m - 1` counted events).
- PS tests cover seed formation, extension, front trimming, non-overlap,
  stable zero-event exports, input QC, and event/ISI/spike membership.
- RGS interval probabilities are checked against a direct implementation of
  the Gaussian-sum equation described by Ko et al. (2012).
- RGS tests cover fitted-reference prediction, separation of the candidate
  count threshold from the family-wise acceptance threshold, estimability,
  publication-mode fail-closed behavior, grouping, input QC, compact export,
  and stable empty schemas.
- Existing STPD multitrack integration tests confirm that the new providers do
  not alter the main detector output contract.

## Independent-code comparison boundary

The public comparison repository accompanying Cotterill et al. (2016) was
inspected at commit `80680289f43f82ec272ac110d89d09466b2e943a`.
Its RGS wrapper calls `f.BPsummary()`, but that implementation is not included
in the repository. Its PS script likewise depends on undeclared objects and a
`surprise()` implementation not contained in the repository. Consequently,
those files are not standalone executable reference implementations and are
not used here as an external golden oracle.

The current evidence therefore supports **published-equation parity and
internal behavioral regression**, not byte-for-byte or event-for-event parity
with independently executable PS/RGS software. Any future external parity
claim must name the exact implementation, version, parameters, count
convention, input fixture, and expected event geometry.

## Platform verification

The workflow `.github/workflows/ps-rgs-core-check.yaml` runs package checks and
the focused PS/RGS and multitrack integration tests on Linux, macOS, and
Windows. A local successful run is not described as cross-platform evidence;
the workflow results attached to a pushed commit are the platform evidence.

## References

- Ko D, Wilson CJ, Lobb CJ, Paladini CA. Detection of bursts and pauses in
  spike trains. *Journal of Neuroscience Methods*. 2012;211:145-158.
- Cotterill E, Charlesworth P, Thomas CW, Paulsen O, Eglen SJ. A comparison of
  computational methods for detecting bursts in neuronal spike trains and
  their application to human stem cell-derived neuronal networks.
  *Journal of Neurophysiology*. 2016;116:306-321.
