import Foundation
import STPDCore
import Testing

// P4.5 — stabilize exact core/bridge ISI membership counts against floating-point epsilon at band edges, with no
// double-counting. The tolerance is a FRACTION of the bound (scale-relative), never a fixed ms.

private func dataset(_ name: String, _ times: [Double]) throws -> SpikeDataset {
    let csv = ([name] + times.map { String(format: "%.6f", $0) }).joined(separator: "\n")
    return try CSVSpikeMatrixParser.parse(contents: csv, datasetName: "p45", sourceDescription: "p45")
}
private func classicBurstTimes() -> [Double] {
    var t = 0.0; var times = [0.0]
    for blk in 0..<5 { for _ in 0..<6 { t += 0.40; times.append(t) }; if blk < 4 { for _ in 0..<4 { t += 0.008; times.append(t) } } }
    return times
}
private func falseBurstTimes() -> [Double] {
    var t = 0.0; var times = [0.0]
    for _ in 0..<5 { for _ in 0..<3 { t += 0.06; times.append(t) }; t += 1.5; times.append(t) }
    return times
}
private func burstProfile(lower: Double, upper: Double, bridge: Double) -> ResolvedThresholdProfile {
    ResolvedThresholdProfile(
        burst: ResolvedFamilyThresholds(lowerSec: lower, upperSec: upper, bridgeUpperSec: bridge, minSpikes: 3),
        hfs: ResolvedFamilyThresholds(lowerSec: 0.003, upperSec: 0.020, minSpikes: 30),
        hfTonic: ResolvedFamilyThresholds(lowerSec: 0.024, upperSec: 0.040, minSpikes: 6),
        tonic: ResolvedFamilyThresholds(lowerSec: 0.20, upperSec: 0.90, minSpikes: 5),
        pause: ResolvedFamilyThresholds(lowerSec: 1.0, upperSec: .infinity), provenance: [])
}

private func cleanBurstCandidate() throws -> (ClassicAnchorCandidate, SpikeTrain, Int) {
    let ds = try dataset("cb", classicBurstTimes())
    let train = ds.trains[0]
    let run = ClassicAnchorDetectionPipeline.run(dataset: ds, bandSettings: TrainAdaptiveBandSettings())
    let cand = try #require((run.result(for: train.id)?.candidates ?? []).first { $0.selectedForAuto && $0.finalLabel == .burst })
    let n = CandidateIntervalEvidence.candidateISIsSec(train: train, startISIIndex: cand.startISIIndex, endISIIndex: cand.endISIIndex).count
    return (cand, train, n)
}

// MARK: - task 4: ISI at upper + epsilon counts as core (not lost, not bridge)

@Test
func p45_isiAtUpperPlusEpsilonCountsAsCore() throws {
    let (cand, train, n) = try cleanBurstCandidate()
    // Place the core upper JUST below the candidate's ISIs (within the band-edge tolerance), bridge well above.
    let q50 = try #require(CandidateIntervalEvidence.build(candidate: cand, train: train, profile: burstProfile(lower: 0.003, upper: 0.012, bridge: 0.020), eventness: nil).q50Sec)
    let profile = burstProfile(lower: 0.003, upper: q50 - 1e-12, bridge: 0.020)

    let strict = CandidateIntervalEvidence.build(candidate: cand, train: train, profile: profile, eventness: nil, bandEdgeRelativeTolerance: 0)
    let tolerant = CandidateIntervalEvidence.build(candidate: cand, train: train, profile: profile, eventness: nil, bandEdgeRelativeTolerance: 1e-9)

    // Strict membership drops the epsilon-above-upper ISIs from core; the tolerant one keeps them as core.
    #expect((tolerant.burstCoreISICount ?? 0) > (strict.burstCoreISICount ?? -1))
    #expect(tolerant.burstCoreISICount == n)                 // all ISIs counted as core under tolerance
    #expect((tolerant.burstBridgeExtensionISICount ?? 0) == 0)   // epsilon-above stays CORE, never bridge
}

// MARK: - task 4: ISI clearly above upper but within bridge counts as bridge extension

@Test
func p45_isiClearlyAboveUpperWithinBridgeCountsAsBridgeExtension() throws {
    let (cand, train, n) = try cleanBurstCandidate()   // ~0.008 s ISIs
    let profile = burstProfile(lower: 0.003, upper: 0.005, bridge: 0.020)   // 0.008 is clearly above 0.005, within 0.020
    let ev = CandidateIntervalEvidence.build(candidate: cand, train: train, profile: profile, eventness: nil)
    #expect(ev.burstCoreISICount == 0)
    #expect(ev.burstBridgeExtensionISICount == n)
}

// MARK: - task 4: no double-counting between core and bridge

@Test
func p45_noDoubleCountingBetweenCoreAndBridge() throws {
    let (cand, train, n) = try cleanBurstCandidate()
    // Sweep the core upper across the candidate's ISI scale; core + bridgeExtension must never exceed the ISI count.
    for upper in [0.004, 0.006, 0.008, 0.0080000000001, 0.010, 0.012] {
        let ev = CandidateIntervalEvidence.build(candidate: cand, train: train, profile: burstProfile(lower: 0.003, upper: upper, bridge: 0.020), eventness: nil)
        let core = ev.burstCoreISICount ?? 0
        let bridge = ev.burstBridgeExtensionISICount ?? 0
        #expect(core + bridge <= n)
        #expect(core >= 0 && bridge >= 0)
    }
}

// MARK: - tasks: ON behavior preserved (false-burst demotes; classic canonical) + default OFF byte-identical

private struct Sel { let canonicalBurst: Int; let possibleBurst: Int; let v2Marker: Int }
private func observe(_ ds: SpikeDataset, on: Bool) -> Sel {
    let run = ClassicAnchorDetectionPipeline.run(dataset: ds, bandSettings: TrainAdaptiveBandSettings(), useAdaptiveV2Canonicalization: on)
    let all = run.result(for: ds.trains[0].id)?.candidates ?? []
    let sel = all.filter(\.selectedForAuto)
    return Sel(canonicalBurst: sel.filter { $0.finalLabel.isCanonicalBurstFamily }.count,
               possibleBurst: sel.filter { $0.finalLabel == .possibleBurst }.count,
               v2Marker: all.filter { $0.decisionPath.contains("adaptive_v2_canonicalization") }.count)
}

@Test
func p45_onBehaviorPreservedFalseBurstDemotesClassicCanonical() throws {
    let cb = observe(try dataset("cb", classicBurstTimes()), on: true)
    #expect(cb.canonicalBurst == 4 && cb.v2Marker == 0)        // clean classic bursts all canonical, no demotion
    let fb = observe(try dataset("fb", falseBurstTimes()), on: true)
    #expect(fb.canonicalBurst == 0 && fb.possibleBurst >= 1 && fb.v2Marker >= 1)   // false-burst still demotes
}

@Test
func p45_defaultOffByteIdentical() throws {
    for times in [classicBurstTimes(), falseBurstTimes()] {
        let ds = try dataset("off", times)
        let train = ds.trains[0]
        func snap(_ on: Bool?) -> [String] {
            let run = on.map { ClassicAnchorDetectionPipeline.run(dataset: ds, bandSettings: TrainAdaptiveBandSettings(), useAdaptiveV2Canonicalization: $0) }
                ?? ClassicAnchorDetectionPipeline.run(dataset: ds, bandSettings: TrainAdaptiveBandSettings())
            return (run.result(for: train.id)?.candidates ?? [])
                .map { "\($0.id)|\($0.finalLabel.rawValue)|\($0.selectedForAuto)|\($0.decisionPath.contains("adaptive_v2_canonicalization"))" }.sorted()
        }
        #expect(snap(nil) == snap(false))
        #expect(!snap(nil).contains { $0.hasSuffix("|true") })
    }
}
