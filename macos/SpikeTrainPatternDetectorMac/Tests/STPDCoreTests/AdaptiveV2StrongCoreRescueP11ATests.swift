import Foundation
import STPDCore
import Testing

// ALGO-V2-P11A — strong-core canonical burst rescue (behind useAdaptiveV2Canonicalization, default OFF).
//
// Rule (profile-relative, evidence-based; NO fixed-ms window): a burst candidate with a strong EXACT in-band core
// (`burstCoreISICount` ≥ minStrongBurstCoreISICount, default 3) stays canonical even when its q95 is MODERATELY above the
// resolved eligibility ceiling — operationalized as: the candidate MEDIAN (q50) is still within the ceiling (only the
// upper tail spills over). The intended demotions are untouched by ordering: bridge_without_core (core 0) and the
// weak-support tonic_guard_conflict / pause_boundary_driven_dense_packet verdicts are already returned before the rescue.

// MARK: - constructed-evidence unit cases

private func ev(
    family: IntervalFamily = .burst,
    q50: Double, q75: Double, q90: Double, q95: Double,
    cv: Double? = 0.10, cv2: Double? = 0.10, lv: Double? = 0.05,
    burstCore: Double? = 0.6, burstBridge: Double? = 0.7, tonicCov: Double? = nil,
    edge: Double? = nil, ctx: Double? = nil,
    coreCount: Int?, bridgeExtCount: Int? = 0
) -> CandidateIntervalEvidence {
    CandidateIntervalEvidence(
        candidateID: "c", route: "test", candidateClass: "test", proposedLabel: .burst, proposedFamily: family,
        isiCount: 10, spikeCount: 11,
        q50Sec: q50, q75Sec: q75, q90Sec: q90, q95Sec: q95, cv: cv, cv2: cv2, lv: lv,
        burstCoreCoverage: burstCore, burstBridgeCoverage: burstBridge, tonicCoverage: tonicCov,
        hfTonicCoverage: nil, pauseCoverage: nil,
        edgeContrast: edge, contextContrast: ctx, eventnessScore: nil, returnToBaselineScore: nil,
        softAnchorInvolved: false, hardNumericGateInvolved: false, manualSemanticLabelInvolved: false,
        burstCoreISICount: coreCount, burstBridgeExtensionISICount: bridgeExtCount)
}

@Test
func p11a_strongCoreOverCeilingBecomesStrongCoreRescue() {
    // q95 (0.060) and q90 (0.050) are over the ceiling (0.012), but the MEDIAN (q50 0.010) is within it and the core is
    // strong (3 ≥ 3) ⇒ kept canonical via strong_core_rescue, NOT demoted to possible.
    let e = ev(q50: 0.010, q75: 0.011, q90: 0.050, q95: 0.060, coreCount: 3)
    let v = CanonicalizationVerdictBuilder.verdict(
        evidence: e, burstReference: nil, tonicReference: nil, hfTonicReference: nil, burstEligibilityCeilingSec: 0.012)
    #expect(v.verdict == .strongCoreRescue)
    #expect(v.verdict.rawValue == "strong_core_rescue")
    #expect(v.reasons.contains(.strongBurstCoreRescued))
    #expect(BurstCanonicalizationGate.decide(
        candidate: makeBurstCandidate(), evidence: e, verdict: v) == .keepCanonical)
}

@Test
func p11a_coreZeroBridgeOnlyStillDemotes() {
    // No tight core (core 0, all in the bridge band) ⇒ bridge_without_core, NOT rescued (core 0 < 3).
    let e = ev(q50: 0.010, q75: 0.011, q90: 0.013, q95: 0.014, burstCore: 0.0, burstBridge: 0.8,
               coreCount: 0, bridgeExtCount: 6)
    let v = CanonicalizationVerdictBuilder.verdict(
        evidence: e, burstReference: nil, tonicReference: nil, hfTonicReference: nil, burstEligibilityCeilingSec: 0.012)
    #expect(v.verdict == .bridgeWithoutCore)
    #expect(v.verdict != .strongCoreRescue)
}

@Test
func p11a_coreOnePauseBoundaryStillDemotes() {
    // One core ISI, strong immediate-flank contrast against a quiet background ⇒ pause_boundary_driven_dense_packet,
    // NOT rescued (core 1 < 3; and the pause verdict is returned before the rescue).
    let e = ev(q50: 0.6, q75: 0.6, q90: 0.6, q95: 0.6, burstCore: 0.1, burstBridge: 0.1,
               edge: 0.80, ctx: 0.03, coreCount: 1)
    let v = CanonicalizationVerdictBuilder.verdict(
        evidence: e, burstReference: nil, tonicReference: nil, hfTonicReference: nil, burstEligibilityCeilingSec: 0.012)
    #expect(v.verdict == .pauseBoundaryDrivenDensePacket)
    #expect(v.verdict != .strongCoreRescue)
}

@Test
func p11a_strongCoreButFarAboveCeilingStillDemotes() {
    // The median guard: a strong core (3) but with the MEDIAN itself far above the ceiling (q50 0.090 ≫ ceiling 0.045) is
    // NOT "moderately above" — the bulk is out of band — so it stays possible_burst_candidate, never rescued.
    let e = ev(q50: 0.090, q75: 0.090, q90: 0.090, q95: 0.140, cv: 0.55, cv2: 0.40, lv: 0.30, coreCount: 3)
    let v = CanonicalizationVerdictBuilder.verdict(
        evidence: e, burstReference: nil, tonicReference: nil, hfTonicReference: nil, burstEligibilityCeilingSec: 0.045)
    #expect(v.verdict == .possibleBurstCandidate)
    #expect(v.verdict != .strongCoreRescue)
}

@Test
func p11a_noCeilingDoesNotRescue() {
    // Missing profile/ceiling evidence never rescues (legacy reference path, no ceiling supplied).
    let e = ev(q50: 0.010, q75: 0.011, q90: 0.050, q95: 0.060, coreCount: 5)
    let v = CanonicalizationVerdictBuilder.verdict(
        evidence: e, burstReference: nil, tonicReference: nil, hfTonicReference: nil)
    #expect(v.verdict != .strongCoreRescue)
}

private func makeBurstCandidate() -> ClassicAnchorCandidate {
    // A minimal currently-canonical burst candidate so the gate's "only canonical bursts are demotable" guard passes.
    var t = 0.0; var times = [0.0]
    for _ in 0..<6 { t += 0.008; times.append(t) }
    let train = SpikeTrain(name: "g", timestampsSec: times)
    let ds = SpikeDataset(name: "g", sourceDescription: "g", trains: [train])
    let run = ClassicAnchorDetectionPipeline.run(dataset: ds, bandSettings: TrainAdaptiveBandSettings())
    return (run.result(for: train.id)?.candidates ?? []).first { $0.finalLabel.isCanonicalBurstFamily }
        ?? (run.result(for: train.id)?.candidates ?? [])[0]
}

// MARK: - real-data characterization

private func fixture5x5() throws -> SpikeDataset {
    let url = URL(fileURLWithPath: #filePath).deletingLastPathComponent().appendingPathComponent("Fixtures/tonic_baseline_5x5.csv")
    return try CSVSpikeMatrixParser.parse(contents: try String(contentsOf: url, encoding: .utf8), datasetName: "5x5", sourceDescription: url.path)
}
private func grechishnikova() throws -> SpikeDataset? {
    // Repo file at <repo>/inst/extdata/… — five levels up from Tests/STPDCoreTests/<file>.swift.
    let repo = URL(fileURLWithPath: #filePath)
        .deletingLastPathComponent().deletingLastPathComponent().deletingLastPathComponent()
        .deletingLastPathComponent().deletingLastPathComponent()
    let url = repo.appendingPathComponent("inst/extdata/Grechishnikova_STN_2017_subset.csv")
    guard FileManager.default.fileExists(atPath: url.path) else { return nil }
    return try CSVSpikeMatrixParser.parse(contents: try String(contentsOf: url, encoding: .utf8),
                                          datasetName: "grech", sourceDescription: url.path, allowDerivedCSV: true)
}
private func cands(_ r: ClassicAnchorDetectionRun, _ id: String) -> [ClassicAnchorCandidate] { r.result(for: id)?.candidates ?? [] }
private func selCanon(_ run: ClassicAnchorDetectionRun, _ ds: SpikeDataset) -> Int {
    ds.trains.reduce(0) { $0 + cands(run, $1.id).filter { $0.selectedForAuto && $0.finalLabel.isCanonicalBurstFamily }.count }
}
private func markerCount(_ run: ClassicAnchorDetectionRun, _ ds: SpikeDataset, _ needle: String) -> Int {
    ds.trains.reduce(0) { $0 + cands(run, $1.id).filter { $0.decisionPath.contains(needle) }.count }
}
private func tonicCoverage(_ run: ClassicAnchorDetectionRun, _ ds: SpikeDataset) -> Int {
    ds.trains.reduce(0) { acc, tr in
        var s = Set<Int>()
        for c in cands(run, tr.id) where c.selectedForAuto && c.finalLabel == .tonic {
            for i in c.startISIIndex...max(c.startISIIndex, c.endISIIndex) { s.insert(i) }
        }
        return acc + s.count
    }
}

@Test
func p11a_grechishnikovaCanonicalImprovesWithoutRevertingFalseBursts() throws {
    guard let ds = try grechishnikova() else { return }   // graceful skip if the repo fixture is absent
    let band = TrainAdaptiveBandSettings()
    let off = ClassicAnchorDetectionPipeline.run(dataset: ds, bandSettings: band, useAdaptiveV2Canonicalization: false)
    let on  = ClassicAnchorDetectionPipeline.run(dataset: ds, bandSettings: band, useAdaptiveV2Canonicalization: true)
    let offCanon = selCanon(off, ds)
    let onCanon = selCanon(on, ds)
    // ALGO-V2-FINAL-AUDIT documented OFF=74, pre-P11A ON=54 (a 27% over-demotion). P11A rescues the strong-core casualties.
    #expect(offCanon >= 70)
    #expect(onCanon > 54)            // strictly improved over the pre-P11A ON (54)
    #expect(onCanon < offCanon)      // …but NOT fully reverted — genuine false bursts still demote
    #expect(markerCount(on, ds, "adaptive_v2_canonicalization=strong_core_rescue") >= 1)   // the rescue fired
    // The false-burst demotions are preserved: both bridge_without_core and pause-boundary packets still demote.
    #expect(markerCount(on, ds, "verdict_bridge_without_core") >= 1)
    #expect(markerCount(on, ds, "verdict_pause_boundary_driven_dense_packet") >= 1)
}

@Test
func p11a_fiveByFiveNoTonicLossNoNewFragmentation() throws {
    let ds = try fixture5x5()
    let band = TrainAdaptiveBandSettings()
    let off = ClassicAnchorDetectionPipeline.run(dataset: ds, bandSettings: band, useAdaptiveV2Canonicalization: false)
    let on  = ClassicAnchorDetectionPipeline.run(dataset: ds, bandSettings: band, useAdaptiveV2Canonicalization: true)
    // Tonic coverage is byte-for-byte preserved (the gate only ever touches canonical-burst-family candidates).
    #expect(tonicCoverage(off, ds) == tonicCoverage(on, ds))
    // No new burst fragmentation: selected canonical bursts stay at the P6B level (40 → ≥39; only the genuine marginal
    // [12…13] core=1 demotes — it is below the strong-core threshold so the rescue does not touch it).
    #expect(selCanon(off, ds) == 40)
    #expect(selCanon(on, ds) >= 39)
    // …and the strong-core rescue genuinely fires on the synthetic strong-core packets too.
    #expect(markerCount(on, ds, "adaptive_v2_canonicalization=strong_core_rescue") >= 1)
}
