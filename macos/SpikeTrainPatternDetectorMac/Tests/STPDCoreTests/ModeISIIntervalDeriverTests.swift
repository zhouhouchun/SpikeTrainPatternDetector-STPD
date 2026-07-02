@testable import STPDCore
import Testing

// MARK: - Phase D3 — ModeISIIntervalDeriver (distribution-based initial interval priors)
//
// Structural tests (NOT equivalence with existing bands — D3 derives distribution-first PRIORS from
// ISI shape only, no anchors/detectors). Assert: family ordering, provenance/scope, overlap kinds,
// scale-invariance (proves no fixed absolute-ms cutoff), and safe degenerate handling.

private func d3Train(_ name: String, isis: [Double]) -> SpikeTrain {
    var ts = [0.0]
    for isi in isis { ts.append((ts.last ?? 0) + isi) }
    return SpikeTrain(name: name, timestampsSec: ts)
}

private func d3Derive(_ trains: [SpikeTrain], floor: Double) -> DerivedModeISIIntervals {
    let dataset = SpikeDataset(name: "d3", sourceDescription: "unit-test", trains: trains)
    let distribution = DatasetISIDistributionService.compute(dataset: dataset, minimumValidISISec: floor)
    return ModeISIIntervalDeriver.derive(datasetDistribution: distribution, minimumValidISISec: floor)
}

private func d3Close(_ a: Double?, _ b: Double?, tol: Double = 1e-9) -> Bool {
    switch (a, b) {
    case (nil, nil): return true
    case let (x?, y?): return abs(x - y) <= tol
    default: return false
    }
}

private let d3BurstISIs = [0.003, 0.0032, 0.0035, 0.0030, 0.0033]
private let d3TonicISIs = [0.040, 0.052, 0.045, 0.058, 0.048, 0.055, 0.043, 0.050, 0.047, 0.053]
private let d3PauseISIs = [0.50, 0.55, 0.60]

// 1 — Burst compact mode: a bimodal (short burst + tonic) distribution yields a burst interval whose
// core upper sits below the tonic lower, with a bridge (>= core upper).
@Test
func modeISIIntervalDeriverFindsBurstCompactModeBelowTonic() {
    let derived = d3Derive([d3Train("bimodal", isis: d3BurstISIs + d3TonicISIs)], floor: 0.001)
    let fam = derived.dataset

    #expect(fam.burst != nil)
    #expect(fam.tonic != nil)
    #expect(fam.pause == nil)                                   // no large-ISI tail
    let burst = fam.burst!
    #expect(burst.family == .burst)
    #expect(burst.upperSec < fam.tonic!.lowerSec)              // burst core below tonic
    #expect(burst.bridgeUpperSec != nil)
    #expect(burst.bridgeUpperSec! >= burst.upperSec)          // bridge extends beyond core
    #expect(burst.containsBridge(burst.bridgeUpperSec!))      // bridge helper covers the extension
    #expect(!burst.contains(burst.bridgeUpperSec!) || burst.bridgeUpperSec! == burst.upperSec)
}

// 2 — Tonic central band brackets the central ISI mass and sits above the burst core.
@Test
func modeISIIntervalDeriverTonicBandBracketsCentreAboveBurst() {
    let derived = d3Derive([d3Train("bimodal", isis: d3BurstISIs + d3TonicISIs)], floor: 0.001)
    let fam = derived.dataset

    #expect(fam.tonic != nil)
    let tonic = fam.tonic!
    #expect(tonic.family == .tonic)
    #expect(tonic.contains(0.047))                             // a representative central ISI is inside
    #expect(tonic.lowerSec >= fam.burst!.upperSec)            // tonic lower >= burst upper when burst exists
    #expect(tonic.bridgeUpperSec == nil)                      // tonic has no bridge
}

// 3 — Pause tail: a large-ISI tail yields a pause interval whose lower sits above the tonic upper and
// which covers the large tail values.
@Test
func modeISIIntervalDeriverPauseTailSitsAboveTonic() {
    let derived = d3Derive([d3Train("withPause", isis: d3BurstISIs + d3TonicISIs + d3PauseISIs)], floor: 0.001)
    let fam = derived.dataset

    #expect(fam.pause != nil)
    #expect(fam.tonic != nil)
    let pause = fam.pause!
    #expect(pause.family == .pause)
    #expect(pause.lowerSec >= fam.tonic!.upperSec)            // pause floor above tonic ceiling
    #expect(pause.contains(0.55))                             // covers a large tail ISI (interior)
    #expect(pause.upperSec >= 0.59)                           // reaches the top of the tail
}

// 4 — Provenance: train-local intervals are trainLocalDerived (no propagation); dataset intervals are
// datasetSupported (propagate); a single-outlier pause tail is audit-only (never selects/propagates).
@Test
func modeISIIntervalDeriverProvenanceReflectsScopeAndAmbiguity() {
    let t1 = d3Train("t1", isis: d3BurstISIs + d3TonicISIs)
    let t2 = d3Train("t2", isis: d3BurstISIs + d3TonicISIs)
    let derived = d3Derive([t1, t2], floor: 0.001)

    // Train-local
    let local = derived.perTrain[t1.id]
    #expect(local?.burst?.provenance.origin == .trainLocalDerived)
    #expect(local?.burst?.provenance.mayPropagateToDataset == false)
    #expect(local?.burst?.provenance.maySelectFinalLabel == true)
    #expect(local?.tonic?.scope == .trainLocal)

    // Dataset (pooled)
    #expect(derived.dataset.burst?.provenance.origin == .datasetSupported)
    #expect(derived.dataset.burst?.provenance.mayPropagateToDataset == true)
    #expect(derived.dataset.tonic?.scope == .dataset)

    // Audit-only: a lone far outlier tail is ambiguous → never selects/propagates.
    let audit = d3Train("audit", isis: [0.045, 0.046, 0.044, 0.047, 0.045, 0.043, 5.0])
    let derivedAudit = d3Derive([audit], floor: 0.001)
    let auditPause = derivedAudit.perTrain[audit.id]?.pause
    #expect(auditPause != nil)
    #expect(auditPause?.provenance.isAuditOnly == true)
    #expect(auditPause?.provenance.maySelectFinalLabel == false)
    #expect(auditPause?.provenance.mayPropagateToDataset == false)
}

// 5 — Overlap descriptors are tagged within-train vs cross-train (dataset).
@Test
func modeISIIntervalDeriverEmitsWithinAndCrossTrainOverlaps() {
    let derived = d3Derive([d3Train("bimodal", isis: d3BurstISIs + d3TonicISIs)], floor: 0.001)
    #expect(derived.overlaps.contains { $0.kind == .withinTrainModeOverlap })
    #expect(derived.overlaps.contains { $0.kind == .crossTrainBandOverlap })
}

// 6 — Scale invariance: scaling every ISI (and the floor) by 10 scales every derived bound by 10 and
// leaves support counts unchanged. Proves no fixed absolute-ms cutoff slipped in.
@Test
func modeISIIntervalDeriverIsScaleInvariant() {
    let isis = d3BurstISIs + d3TonicISIs + d3PauseISIs
    let base = d3Derive([d3Train("s1", isis: isis)], floor: 0.001).dataset
    let scaled = d3Derive([d3Train("s10", isis: isis.map { $0 * 10 })], floor: 0.010).dataset

    for family in [\FamilyModeIntervals.burst, \FamilyModeIntervals.tonic, \FamilyModeIntervals.pause] {
        let b = base[keyPath: family]
        let s = scaled[keyPath: family]
        #expect((b == nil) == (s == nil))
        guard let b, let s else { continue }
        #expect(d3Close(b.lowerSec * 10, s.lowerSec, tol: 1e-6))
        #expect(d3Close(b.upperSec * 10, s.upperSec, tol: 1e-6))
        #expect(d3Close(b.bridgeUpperSec.map { $0 * 10 }, s.bridgeUpperSec, tol: 1e-6))
        #expect(b.supportCount == s.supportCount)              // counts are scale-free
    }
}

// 7 — Degenerate inputs are safe: empty dataset, single ISI, and all-identical ISIs never crash and
// yield nil intervals (no distinct mode to derive).
@Test
func modeISIIntervalDeriverHandlesDegenerateDistributions() {
    let empty = d3Derive([], floor: 0.001)
    #expect(empty.perTrain.isEmpty)
    #expect(empty.dataset.burst == nil && empty.dataset.tonic == nil && empty.dataset.pause == nil)
    #expect(empty.overlaps.isEmpty)

    let single = d3Derive([d3Train("one", isis: [0.05])], floor: 0.001).dataset
    #expect(single.burst == nil && single.tonic == nil && single.pause == nil)

    // Near-identical ISIs: no spurious burst/pause modes (a thin central tonic is acceptable).
    let flat = d3Derive([d3Train("flat", isis: [0.05, 0.05, 0.05, 0.05, 0.05])], floor: 0.001).dataset
    #expect(flat.burst == nil && flat.pause == nil)
}

// 8 — ModeISIInterval bridge validity: a bridge below the core upper is invalid; a bridge at/above it
// is valid and only containsBridge (not contains) covers the extension.
@Test
func modeISIIntervalBridgeValidityAndExtendedContains() {
    let valid = ModeISIInterval(
        family: .burst, lowerSec: 0.001, upperSec: 0.010,
        scope: .trainLocal, provenance: .trainLocalDerived(sourceStatistic: "x"),
        bridgeUpperSec: 0.020
    )
    #expect(valid.isValid)
    #expect(valid.contains(0.008))
    #expect(!valid.contains(0.015))          // beyond the core
    #expect(valid.containsBridge(0.015))     // within the bridge
    #expect(!valid.containsBridge(0.025))    // beyond the bridge

    let invalidBridge = ModeISIInterval(
        family: .burst, lowerSec: 0.001, upperSec: 0.010,
        scope: .trainLocal, provenance: .trainLocalDerived(sourceStatistic: "x"),
        bridgeUpperSec: 0.005                // below core upper -> invalid
    )
    #expect(!invalidBridge.isValid)
    #expect(ModeISIInterval.validated(
        family: .burst, lowerSec: 0.001, upperSec: 0.010,
        scope: .trainLocal, provenance: .trainLocalDerived(sourceStatistic: "x"),
        bridgeUpperSec: 0.005) == nil)
}
