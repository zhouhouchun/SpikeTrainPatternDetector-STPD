import Foundation
import STPDCore
import Testing

// P4 — stabilize Adaptive-v2 ON behavior: a scale-relative quantile tolerance stops a clean classic-burst edge packet
// from being demoted by floating-point epsilon, while the genuine false-burst still demotes. Default stays OFF.

private func dataset(_ name: String, _ times: [Double]) throws -> SpikeDataset {
    let csv = ([name] + times.map { String(format: "%.6f", $0) }).joined(separator: "\n")
    return try CSVSpikeMatrixParser.parse(contents: csv, datasetName: "p4", sourceDescription: "p4")
}
private func falseBurstTimes() -> [Double] {
    var t = 0.0; var times = [0.0]
    for _ in 0..<5 { for _ in 0..<3 { t += 0.06; times.append(t) }; t += 1.5; times.append(t) }
    return times
}
private func classicBurstTimes() -> [Double] {
    var t = 0.0; var times = [0.0]
    for blk in 0..<5 { for _ in 0..<6 { t += 0.40; times.append(t) }; if blk < 4 { for _ in 0..<4 { t += 0.008; times.append(t) } } }
    return times
}
private struct Sel { let canonicalBurst: Int; let possibleBurst: Int; let tonic: Int; let v2Marker: Int }
private func observe(_ ds: SpikeDataset, on: Bool) -> Sel {
    let run = ClassicAnchorDetectionPipeline.run(dataset: ds, bandSettings: TrainAdaptiveBandSettings(), useAdaptiveV2Canonicalization: on)
    let all = run.result(for: ds.trains[0].id)?.candidates ?? []
    let sel = all.filter(\.selectedForAuto)
    return Sel(
        canonicalBurst: sel.filter { $0.finalLabel.isCanonicalBurstFamily }.count,
        possibleBurst: sel.filter { $0.finalLabel == .possibleBurst }.count,
        tonic: sel.filter { $0.finalLabel == .tonic }.count,
        v2Marker: all.filter { $0.decisionPath.contains("adaptive_v2_canonicalization") }.count)
}

// MARK: - the fix: clean classic bursts are no longer demoted by epsilon

@Test
func p4_classicBurstEdgePacketNoLongerDemotedUnderON() throws {
    let ds = try dataset("cb", classicBurstTimes())
    let off = observe(ds, on: false)
    let on = observe(ds, on: true)
    #expect(off.canonicalBurst == 4)
    // After the tolerance fix, ALL real classic bursts remain canonical and nothing is demoted.
    #expect(on.canonicalBurst == 4)
    #expect(on.v2Marker == 0)
    #expect(on.tonic == off.tonic)   // tonics unchanged
}

// MARK: - the tolerance does NOT rescue the genuine false-burst

@Test
func p4_falseBurstStillDemotedUnderON() throws {
    let ds = try dataset("fb", falseBurstTimes())
    let on = observe(ds, on: true)
    #expect(on.canonicalBurst == 0)     // still demoted (q95 is ~10× the burst reference — far beyond the tolerance)
    #expect(on.possibleBurst >= 1)
    #expect(on.v2Marker >= 1)
}

// MARK: - default OFF stays byte-identical

@Test
func p4_defaultOffByteIdentical() throws {
    for times in [classicBurstTimes(), falseBurstTimes()] {
        let ds = try dataset("p4off", times)
        let train = ds.trains[0]
        func snap(_ on: Bool?) -> [String] {
            let run = on.map { ClassicAnchorDetectionPipeline.run(dataset: ds, bandSettings: TrainAdaptiveBandSettings(), useAdaptiveV2Canonicalization: $0) }
                ?? ClassicAnchorDetectionPipeline.run(dataset: ds, bandSettings: TrainAdaptiveBandSettings())
            return (run.result(for: train.id)?.candidates ?? [])
                .map { "\($0.id)|\($0.finalLabel.rawValue)|\($0.selectedForAuto)|\($0.decisionPath.contains("adaptive_v2_canonicalization"))" }.sorted()
        }
        #expect(snap(nil) == snap(false))                                // default == explicit OFF
        #expect(!snap(nil).contains { $0.hasSuffix("|true") })           // no v2 marker when OFF
    }
}

// MARK: - tolerance unit behavior (scale-relative, not fixed-ms)

@Test
func p4_quantileToleranceIsScaleRelative() {
    // Build evidence with a q95 just above the reference (epsilon), and confirm the verdict treats it as compatible.
    let ev = CandidateIntervalEvidence(
        candidateID: "c", route: "t", candidateClass: "t", proposedLabel: .burst, proposedFamily: .burst,
        isiCount: 6, spikeCount: 7,
        q50Sec: 0.008, q75Sec: 0.008, q90Sec: 0.0080000001, q95Sec: 0.0080000001, cv: 0.05, cv2: 0.05, lv: 0.02,
        burstCoreCoverage: 1.0, burstBridgeCoverage: 1.0, tonicCoverage: 0, hfTonicCoverage: 0, pauseCoverage: 0,
        edgeContrast: nil, contextContrast: nil, eventnessScore: nil, returnToBaselineScore: nil,
        softAnchorInvolved: false, hardNumericGateInvolved: false, manualSemanticLabelInvolved: false,
        burstCoreISICount: 6, burstBridgeExtensionISICount: 0)
    let burstRef = ResolvedFamilyDistributionSummary.from(
        family: .burst, inBandISIsSec: [0.008, 0.008, 0.008, 0.008, 0.008], provenance: .adaptive)   // q95 = 0.008
    let v = CanonicalizationVerdictBuilder.verdict(evidence: ev, burstReference: burstRef, tonicReference: nil, hfTonicReference: nil)
    #expect(v.verdict == .canonicalCandidate)                            // epsilon-over is tolerated → canonical

    // A candidate well beyond the tolerance (2× the reference) is NOT rescued.
    let evFar = CandidateIntervalEvidence(
        candidateID: "c2", route: "t", candidateClass: "t", proposedLabel: .burst, proposedFamily: .burst,
        isiCount: 6, spikeCount: 7,
        q50Sec: 0.016, q75Sec: 0.016, q90Sec: 0.016, q95Sec: 0.016, cv: 0.05, cv2: 0.05, lv: 0.02,
        burstCoreCoverage: 1.0, burstBridgeCoverage: 1.0, tonicCoverage: 0, hfTonicCoverage: 0, pauseCoverage: 0,
        edgeContrast: nil, contextContrast: nil, eventnessScore: nil, returnToBaselineScore: nil,
        softAnchorInvolved: false, hardNumericGateInvolved: false, manualSemanticLabelInvolved: false,
        burstCoreISICount: 6, burstBridgeExtensionISICount: 0)
    let vFar = CanonicalizationVerdictBuilder.verdict(evidence: evFar, burstReference: burstRef, tonicReference: nil, hfTonicReference: nil)
    #expect(vFar.verdict != .canonicalCandidate)                         // 2× over → not within tolerance
}
