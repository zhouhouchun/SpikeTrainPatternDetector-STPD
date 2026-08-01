import Foundation
import STPDCore
import Testing

// P3B — run Adaptive-v2 canonicalization ON in TESTS ONLY (default stays OFF). Characterizes the behavior changes:
// the motivating false-burst is demoted canonical→possible; real classic bursts mostly stay canonical; tonics survive.

private func dataset(_ name: String, _ times: [Double]) throws -> SpikeDataset {
    let csv = ([name] + times.map { String(format: "%.6f", $0) }).joined(separator: "\n")
    return try CSVSpikeMatrixParser.parse(contents: csv, datasetName: "p3b", sourceDescription: "p3b")
}
private func pauseFlanked(spikes: Int, isi: Double, gap: Double, repeats: Int) -> [Double] {
    var t = 0.0; var times = [0.0]
    for _ in 0..<repeats { for _ in 0..<(spikes - 1) { t += isi; times.append(t) }; t += gap; times.append(t) }
    return times
}
private func classicBurstTimes() -> [Double] {
    var t = 0.0; var times = [0.0]
    for blk in 0..<5 { for _ in 0..<6 { t += 0.40; times.append(t) }; if blk < 4 { for _ in 0..<4 { t += 0.008; times.append(t) } } }
    return times
}

private struct Sel {
    let canonicalBurst: Int     // selected, isCanonicalBurstFamily (.burst/.highFrequencyBurst/.longBurst)
    let possibleBurst: Int      // selected, .possibleBurst
    let tonic: Int
    let v2Marker: Int           // candidates carrying the adaptive_v2_canonicalization demotion marker
}
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

// MARK: - task 2: motivating false-burst is demoted canonical → possible

@Test
func p3b_falseBurstDemotedFromCanonicalToPossible() throws {
    let ds = try dataset("fb", pauseFlanked(spikes: 4, isi: 0.06, gap: 1.5, repeats: 5))
    let off = observe(ds, on: false)
    let on = observe(ds, on: true)
    // OFF: the packet is a canonical burst, no v2 marker.
    #expect(off.canonicalBurst >= 1 && off.v2Marker == 0)
    // ON: every canonical burst is demoted to possible_burst, carrying the v2 marker.
    #expect(on.canonicalBurst == 0)
    #expect(on.possibleBurst >= 1)
    #expect(on.v2Marker >= 1)
}

// MARK: - task 3: real classic bursts mostly remain canonical

@Test
func p3b_realClassicBurstsRemainCanonical() throws {
    let ds = try dataset("cb", classicBurstTimes())
    let off = observe(ds, on: false)
    let on = observe(ds, on: true)
    #expect(off.canonicalBurst == 4)
    // After P4's scale-relative quantile tolerance, ALL real classic bursts remain canonical under ON (the former
    // epsilon-driven edge-packet demotion is fixed).
    #expect(on.canonicalBurst == 4)
}

// MARK: - task 4: tonic candidates survive arbitration after burst demotion

@Test
func p3b_tonicCandidatesSurviveAfterBurstDemotion() throws {
    let ds = try dataset("cb", classicBurstTimes())
    let off = observe(ds, on: false)
    let on = observe(ds, on: true)
    // Demoting a burst does not remove the surrounding tonic candidates — they survive arbitration unchanged.
    #expect(on.tonic == off.tonic)
    #expect(on.tonic >= 1)
}

// MARK: - task 5: which P0 pins flip — recorded as assertions

@Test
func p3b_p0PinAssertionsDoNotFlipButCanonicalityChanges() throws {
    // The P0 pins count `isBurstEventFamily` (which INCLUDES possible_burst) with unchanged lock levels, so demotion
    // to possible_burst does NOT flip any P0 pin assertion. The real change is canonical→possible, captured here.
    let fb = try dataset("fb", pauseFlanked(spikes: 4, isi: 0.06, gap: 1.5, repeats: 5))
    let fbRun = ClassicAnchorDetectionPipeline.run(dataset: fb, bandSettings: TrainAdaptiveBandSettings(), useAdaptiveV2Canonicalization: true)
    let fbSel = (fbRun.result(for: fb.trains[0].id)?.candidates ?? []).filter(\.selectedForAuto)
    // P0#1 assertions (burst>=1, tonic==0, strongCandidateBurst>=1) STILL hold under ON:
    let burstFamily = fbSel.filter { $0.finalLabel.isBurstEventFamily }
    #expect(burstFamily.count >= 1)                                                   // possible_burst still burst-family
    #expect(fbSel.filter { $0.finalLabel == .tonic }.count == 0)
    #expect(burstFamily.filter { $0.anchorLockLevel == .strongCandidate }.count >= 1)
    // ...but the canonical-burst count (the real semantic) drops to 0 — the change the coarse pin can't see.
    #expect(fbSel.filter { $0.finalLabel.isCanonicalBurstFamily }.count == 0)
}

// MARK: - task 7: default remains OFF

@Test
func p3b_defaultRemainsOffAndByteIdentical() throws {
    let ds = try dataset("fb", pauseFlanked(spikes: 4, isi: 0.06, gap: 1.5, repeats: 5))
    let train = ds.trains[0]
    func snap(_ on: Bool) -> [String] {
        let run = ClassicAnchorDetectionPipeline.run(dataset: ds, bandSettings: TrainAdaptiveBandSettings(), useAdaptiveV2Canonicalization: on)
        return (run.result(for: train.id)?.candidates ?? [])
            .map { "\($0.id)|\($0.finalLabel.rawValue)|\($0.selectedForAuto)|\($0.decisionPath.contains("adaptive_v2_canonicalization"))" }.sorted()
    }
    let defaultRun = ClassicAnchorDetectionPipeline.run(dataset: ds, bandSettings: TrainAdaptiveBandSettings())
    let defaultSnap = (defaultRun.result(for: train.id)?.candidates ?? [])
        .map { "\($0.id)|\($0.finalLabel.rawValue)|\($0.selectedForAuto)|\($0.decisionPath.contains("adaptive_v2_canonicalization"))" }.sorted()
    // Default (flag omitted) == explicit OFF, and differs from ON (which demotes).
    #expect(defaultSnap == snap(false))
    #expect(defaultSnap != snap(true))
    // Default has the canonical burst and no v2 marker.
    #expect(observe(ds, on: false).canonicalBurst >= 1)
    #expect(!defaultSnap.contains { $0.hasSuffix("|true") })
}
