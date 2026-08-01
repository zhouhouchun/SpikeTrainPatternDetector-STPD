import Foundation
import STPDCore
import Testing

// P5A — the experimental Adaptive-v2 flag threaded through DetectionInputsSignature and the framework run path. The
// RasterDocument state + UI toggle are app-target (build-verified); these cover the STPDCore-testable surface.

private func dataset(_ name: String, _ times: [Double]) throws -> SpikeDataset {
    let csv = ([name] + times.map { String(format: "%.6f", $0) }).joined(separator: "\n")
    return try CSVSpikeMatrixParser.parse(contents: csv, datasetName: "p5a", sourceDescription: "p5a")
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

private func signature(_ flag: Bool? = nil) -> DetectionInputsSignature {
    if let flag {
        return DetectionInputsSignature(datasetID: "d", bandSettings: TrainAdaptiveBandSettings(),
            qualitySettings: SpikeQualitySettings(), stateTuning: StatePatternDetectorTuning(),
            detectorParameters: .defaults, manualThresholdProfile: .automatic, useAdaptiveV2Canonicalization: flag)
    }
    return DetectionInputsSignature(datasetID: "d", bandSettings: TrainAdaptiveBandSettings(),
        qualitySettings: SpikeQualitySettings(), stateTuning: StatePatternDetectorTuning(),
        detectorParameters: .defaults, manualThresholdProfile: .automatic)   // flag omitted ⇒ default false
}

private struct Sel { let canonicalBurst: Int; let possibleBurst: Int; let v2Marker: Int }
private func observe(_ ds: SpikeDataset, on: Bool) -> Sel {
    let run = HybridPatternDetectionFramework.run(dataset: ds, bandSettings: TrainAdaptiveBandSettings(),
                                                  useAdaptiveV2Canonicalization: on)
    let all = run.result(for: ds.trains[0].id)?.candidates ?? []
    let sel = all.filter(\.selectedForAuto)
    return Sel(canonicalBurst: sel.filter { $0.finalLabel.isCanonicalBurstFamily }.count,
               possibleBurst: sel.filter { $0.finalLabel == .possibleBurst }.count,
               v2Marker: all.filter { $0.decisionPath.contains("adaptive_v2_canonicalization") }.count)
}
private func snapshot(_ ds: SpikeDataset, on: Bool?) -> [String] {
    let run = on.map { HybridPatternDetectionFramework.run(dataset: ds, bandSettings: TrainAdaptiveBandSettings(), useAdaptiveV2Canonicalization: $0) }
        ?? HybridPatternDetectionFramework.run(dataset: ds, bandSettings: TrainAdaptiveBandSettings())
    return (run.result(for: ds.trains[0].id)?.candidates ?? [])
        .map { "\($0.id)|\($0.finalLabel.rawValue)|\($0.selectedForAuto)|\($0.selectionStatus)" }.sorted()
}

// MARK: - signature

@Test
func p5a_signatureChangesWhenFlagChanges() {
    #expect(signature(false) != signature(true))
    #expect(signature() == signature(false))   // default (omitted) is OFF
}

// MARK: - framework run path

@Test
func p5a_frameworkDefaultIsOffAndByteIdentical() throws {
    // Default (flag omitted) == explicit OFF for both fixtures — the byte-identical guarantee.
    for times in [classicBurstTimes(), falseBurstTimes()] {
        let ds = try dataset("p5a", times)
        #expect(snapshot(ds, on: nil) == snapshot(ds, on: false))
    }
    // ON visibly differs only where there is a non-canonical burst to demote (the false-burst), not the clean classic.
    let fb = try dataset("fb", falseBurstTimes())
    #expect(snapshot(fb, on: nil) != snapshot(fb, on: true))
    let cb = try dataset("cb", classicBurstTimes())
    #expect(snapshot(cb, on: nil) == snapshot(cb, on: true))   // clean classic bursts unchanged by ON
}

@Test
func p5a_frameworkOnDemotesFalseBurst() throws {
    let ds = try dataset("fb", falseBurstTimes())
    let off = observe(ds, on: false)
    let on = observe(ds, on: true)
    #expect(off.canonicalBurst >= 1 && off.v2Marker == 0)
    #expect(on.canonicalBurst == 0 && on.possibleBurst >= 1 && on.v2Marker >= 1)
}

@Test
func p5a_frameworkOnKeepsClassicBurstCanonical() throws {
    let ds = try dataset("cb", classicBurstTimes())
    let on = observe(ds, on: true)
    #expect(on.canonicalBurst == 4)
    #expect(on.v2Marker == 0)   // clean classic bursts: no demotion
}
