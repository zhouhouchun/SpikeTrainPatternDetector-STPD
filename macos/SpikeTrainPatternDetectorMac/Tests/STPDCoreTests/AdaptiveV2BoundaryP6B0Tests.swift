import Foundation
import STPDCore
import Testing

// P6B-1 / P6B-2 — FIXED-BEHAVIOR net for the Adaptive-v2 burst recalibration + boundary rescue, on the real 5x5 dataset
// (a repo fixture loaded via #filePath). P6B-0 pinned the bug (real bursts massively over-demoted under ON); P6B-1
// recalibrated burst q-compatibility against the train-specific eligibility ceiling (resolved bridgeUpper) and burst
// SUPPORT against the whole burst band (core + bridge-extension); P6B-2 added BOUNDARY RESCUE — when a burst would still
// be demoted only because a slow boundary ISI inflates q95 above the ceiling, its tight core is kept canonical by
// trimming ≤1 slow edge ISI per side (when the trimmed span re-passes the verdict). These tests assert the combined fix:
//   • P6B-1: q-compat vs ceiling, burst-band support, no bridge_without_core false demotion, clean cores stay canonical;
//   • P6B-2: edge-contaminated bursts are rescued (kept canonical, slow edge excluded) so net selected-canonical loss on
//     the 5x5 falls to 1 (ON=39 vs OFF=40), the single remaining loss being a genuinely near-ceiling non-edge tiny span;
//   • the genuine pause-flanked false-burst is still demoted (pause trains are not over-promoted).
// Behavior changes only behind useAdaptiveV2Canonicalization == true; default stays OFF and byte-identical.

private func fixtureDataset() throws -> SpikeDataset {
    let url = URL(fileURLWithPath: #filePath).deletingLastPathComponent().appendingPathComponent("Fixtures/tonic_baseline_5x5.csv")
    let contents = try String(contentsOf: url, encoding: .utf8)
    return try CSVSpikeMatrixParser.parse(contents: contents, datasetName: "5x5", sourceDescription: url.path)
}
private func runs() throws -> (SpikeDataset, ClassicAnchorDetectionRun, ClassicAnchorDetectionRun) {
    let ds = try fixtureDataset()
    let band = TrainAdaptiveBandSettings()
    return (ds,
            ClassicAnchorDetectionPipeline.run(dataset: ds, bandSettings: band, useAdaptiveV2Canonicalization: false),
            ClassicAnchorDetectionPipeline.run(dataset: ds, bandSettings: band, useAdaptiveV2Canonicalization: true))
}

private func burstTrains(_ ds: SpikeDataset) -> [SpikeTrain] { ds.trains.filter { $0.name.hasPrefix("burst_response") } }
private func pauseTrains(_ ds: SpikeDataset) -> [SpikeTrain] { ds.trains.filter { $0.name.hasPrefix("pause_response") } }
private func cands(_ run: ClassicAnchorDetectionRun, _ id: String) -> [ClassicAnchorCandidate] { run.result(for: id)?.candidates ?? [] }
private func hasV2Marker(_ c: ClassicAnchorCandidate) -> Bool { c.decisionPath.contains("adaptive_v2_canonicalization") }
private func hasTrimMarker(_ c: ClassicAnchorCandidate) -> Bool { c.decisionPath.contains("adaptive_v2_canonicalization=boundary_trim") }
private func hasDemoteMarker(_ c: ClassicAnchorCandidate) -> Bool { c.decisionPath.contains("adaptive_v2_canonicalization=demote_to_possible") }
private func hasStrongCoreMarker(_ c: ClassicAnchorCandidate) -> Bool { c.decisionPath.contains("adaptive_v2_canonicalization=strong_core_rescue") }   // P11A
private func hasCompatibleBoundaryKeptMarker(_ c: ClassicAnchorCandidate) -> Bool { c.decisionPath.contains("adaptive_v2_canonicalization=bcb_compatible_boundary_kept") }   // BURST-BOUNDARY-AUTH
// Is ISI index `i` inside any SELECTED canonical burst span in this run/train?
private func isiInSelectedCanonicalBurst(_ run: ClassicAnchorDetectionRun, _ id: String, _ i: Int) -> Bool {
    cands(run, id).contains { $0.selectedForAuto && $0.finalLabel.isCanonicalBurstFamily && $0.startISIIndex <= i && i <= $0.endISIIndex }
}
private func v2MarkerCount(_ run: ClassicAnchorDetectionRun, _ trains: [SpikeTrain]) -> Int {
    trains.reduce(0) { $0 + cands(run, $1.id).filter(hasV2Marker).count }
}
private func selectedCanonicalBurstCount(_ run: ClassicAnchorDetectionRun, _ trains: [SpikeTrain]) -> Int {
    trains.reduce(0) { $0 + cands(run, $1.id).filter { $0.selectedForAuto && $0.finalLabel.isCanonicalBurstFamily }.count }
}

// Reconstruct the effective burst/tonic profile a train was resolved with (mirrors the gate's resolved bands), so the
// tests can recompute the same P1 evidence + eligibility ceiling the gate used.
private func burstProfile(_ run: ClassicAnchorDetectionRun, _ id: String) -> ResolvedThresholdProfile? {
    guard let res = run.resolutions.first(where: { $0.trainID == id }), let b = res.bands[.burst] else { return nil }
    let t = res.bands[.tonic]; let hf = res.bands[.highFrequencyTonic]
    return ResolvedThresholdProfile(
        burst: ResolvedFamilyThresholds(lowerSec: b.seedLowerSec, upperSec: b.seedUpperSec, bridgeUpperSec: b.bridgeUpperSec, minSpikes: 3),
        hfs: ResolvedFamilyThresholds(lowerSec: 0.003, upperSec: 0.020, minSpikes: 30),
        hfTonic: ResolvedFamilyThresholds(lowerSec: hf?.seedLowerSec ?? 0.024, upperSec: hf?.seedUpperSec ?? 0.040, minSpikes: 6),
        tonic: ResolvedFamilyThresholds(lowerSec: t?.seedLowerSec ?? 0.20, upperSec: t?.seedUpperSec ?? 0.90, minSpikes: 5),
        pause: ResolvedFamilyThresholds(lowerSec: 1.0, upperSec: .infinity), provenance: [])
}
private func evidenceFor(_ c: ClassicAnchorCandidate, _ train: SpikeTrain, _ prof: ResolvedThresholdProfile) -> CandidateIntervalEvidence {
    CandidateIntervalEvidence.build(candidate: c, train: train, profile: prof,
        eventness: ISICandidateEventnessAuditor.makeAudit(for: c, train: train, minValidISISec: 0.001))
}

// MARK: - task 3a: the selected canonical-burst count is preserved MUCH better under ON

@Test
func p6b1_realBurstSelectionMuchBetterPreserved() throws {
    let (ds, off, on) = try runs()
    let bt = burstTrains(ds)
    let offCanon = selectedCanonicalBurstCount(off, bt)
    let onCanon = selectedCanonicalBurstCount(on, bt)
    // P6B-0 pinned a net loss of ≥5 (observed ~14 selected canonical bursts demoted). After the recalibration +
    // re-arbitration the net selected-canonical loss across the burst_response trains is small.
    #expect(offCanon > 0)
    #expect(offCanon - onCanon <= 4)   // was ≥5 in P6B-0; observed 3 here
}

// MARK: - task 3b/3c: no real burst is demoted as bridge_without_core; clean core(+bridge) bursts stay canonical

@Test
func p6b1_noBridgeWithoutCoreFalseDemotions() throws {
    let (ds, _, on) = try runs()
    // The core+bridge support fix means a burst with ≥1 tight-core ISI is never demoted as bridge_without_core; that
    // verdict now fires only for true bridge-only candidates (core == 0), of which there are none among real bursts.
    for train in ds.trains {
        for c in cands(on, train.id) {
            #expect(!c.decisionPath.contains("verdict_bridge_without_core"))
        }
    }
}

@Test
func p6b1_cleanCoreWithinCeilingBurstsRemainCanonical() throws {
    let (ds, off, on) = try runs()
    var checked = 0
    var keptWithBridge = 0
    for train in burstTrains(ds) {
        guard let prof = burstProfile(on, train.id) else { continue }
        let ceiling = prof.burst.bridgeUpperSec ?? prof.burst.upperSec
        let onByID = Dictionary(cands(on, train.id).map { ($0.id, $0) }, uniquingKeysWith: { a, _ in a })
        for c in cands(off, train.id) where c.selectedForAuto && c.finalLabel.isCanonicalBurstFamily {
            let ev = evidenceFor(c, train, prof)
            let core = ev.burstCoreISICount ?? 0
            let bridge = ev.burstBridgeExtensionISICount ?? 0
            let q95 = ev.q95Sec ?? .infinity
            // Unambiguous canonical burst: strong tight core AND q95 comfortably within the eligibility ceiling
            // (0.95× margin keeps reconstruction noise away from the 1.05× decision boundary).
            guard core >= 3, q95 <= ceiling * 0.95 else { continue }
            checked += 1
            let onC = onByID[c.id]
            #expect(onC?.finalLabel.isCanonicalBurstFamily == true)   // stays a canonical burst under ON
            #expect(!(onC.map(hasV2Marker) ?? false))                  // and is not demoted
            if bridge >= 1, onC?.finalLabel.isCanonicalBurstFamily == true { keptWithBridge += 1 }
        }
    }
    #expect(checked >= 3)        // the fixture really contains clean strong-core bursts to protect
    #expect(keptWithBridge >= 1) // …including at least one core+bridge burst the support fix rescues
}

// MARK: - task 3d: pause_response trains are not over-promoted (gate only demotes; the false-burst still demotes)

@Test
func p6b1_pauseTrainsNotOverPromoted() throws {
    let (ds, off, on) = try runs()
    let pt = pauseTrains(ds)
    // The canonicalization gate can only DEMOTE, never promote — so ON cannot add canonical bursts to a pause train.
    #expect(selectedCanonicalBurstCount(on, pt) <= selectedCanonicalBurstCount(off, pt))
    // And pause-train demotions remain a small minority vs the burst trains (P6A observed 0 pause-train demotions).
    let burstMarkers = v2MarkerCount(on, burstTrains(ds))
    let pauseMarkers = v2MarkerCount(on, pt)
    #expect(pauseMarkers * 3 <= burstMarkers)
}

// MARK: - P6B-2: boundary-contaminated bursts are rescued (kept canonical, slow edge excluded)

@Test
func p6b2_edgeContaminatedBurstsRescuedToNearFullPreservation() throws {
    let (ds, off, on) = try runs()
    let bt = burstTrains(ds)
    let offCanon = selectedCanonicalBurstCount(off, bt)
    let onCanon = selectedCanonicalBurstCount(on, bt)
    // P6B-1 left net loss 3; the boundary rescue brings ON within 1 of OFF (only the genuine non-edge marginal remains).
    #expect(offCanon == 40)
    #expect(offCanon - onCanon <= 1)
    #expect(onCanon >= 39)
    // BURST-BOUNDARY-AUTH: the boundary rescue now KEEPS the FULL burst — Adaptive-v2 defers to BCB's vetted compatible
    // extension instead of trimming it (the only 5x5 slow edges reaching V2 are BCB-compatible, since BCB already
    // removed real contaminants at birth). At least one burst carries the compatible-boundary-kept marker and stays canonical.
    let keptCanonical = bt.flatMap { cands(on, $0.id) }.filter { hasCompatibleBoundaryKeptMarker($0) && $0.finalLabel.isCanonicalBurstFamily }
    #expect(keptCanonical.count >= 1)
}

@Test
func p6b2_slowEdgeISICompatibleExtensionKeptInCanonicalBurst() throws {
    // BURST-BOUNDARY-AUTH (reframed from the old slow-edge-EXCLUSION test): when the ONLY reason Adaptive-v2 would demote
    // a burst is a slow boundary ISI that BCB has classified as a COMPATIBLE EXTENSION (within the train-local seed-band
    // upper × the asymmetric ratio, not a contaminant), Adaptive-v2 must NOT re-trim it — the slow edge stays INSIDE the
    // selected canonical burst under ON, exactly as under OFF. (A genuine contaminant edge is trimmed by BCB before V2.)
    let (ds, off, on) = try runs()
    var checkedEdges = 0
    for train in burstTrains(ds) {
        guard let prof = burstProfile(on, train.id) else { continue }
        let ceiling = prof.burst.bridgeUpperSec ?? prof.burst.upperSec
        let edgeCeiling = ceiling * (1 + CanonicalizationVerdictSettings().quantileCompatibilityRelativeTolerance)
        let onByID = Dictionary(cands(on, train.id).map { ($0.id, $0) }, uniquingKeysWith: { a, _ in a })
        for c in cands(off, train.id) where c.selectedForAuto && c.finalLabel.isCanonicalBurstFamily {
            guard let onC = onByID[c.id], hasCompatibleBoundaryKeptMarker(onC) else { continue }
            // Each slow boundary ISI (above the V2 eligibility ceiling) that Adaptive-v2 KEPT (BCB-compatible extension):
            for edge in [c.startISIIndex, c.endISIIndex] {
                guard edge >= 0, edge < train.isiSec.count, let v = train.isiSec[edge], v.isFinite, v > edgeCeiling else { continue }
                checkedEdges += 1
                #expect(isiInSelectedCanonicalBurst(off, train.id, edge))   // OFF: slow edge inside a canonical burst
                #expect(isiInSelectedCanonicalBurst(on, train.id, edge))    // ON: STILL inside a canonical burst (kept, not trimmed)
            }
        }
    }
    #expect(checkedEdges >= 1)   // the fixture really exercises at least one compatible-boundary keep (burst_response_2_s ISI 23)
}

@Test
func p6b2_marginalNonEdgeBurstIsNotForceRescued() throws {
    // The single genuine net loss: burst_response_2_s [12...13] is a 2-ISI span whose q95 is already below ceiling×1.05
    // (no slow boundary ISI to trim) and has no core support — it must stay demoted, NOT be force-rescued by trimming.
    let (ds, _, on) = try runs()
    let train = try #require(burstTrains(ds).first(where: { $0.name == "burst_response_2_s" }))
    // The gated [12...13] candidate (carries an Adaptive-v2 marker) must be the DEMOTE one, never a trim rescue.
    let marginal = try #require(cands(on, train.id).first { $0.startISIIndex == 12 && $0.endISIIndex == 13 && hasV2Marker($0) })
    #expect(hasDemoteMarker(marginal))   // demoted, not trimmed
    #expect(!hasTrimMarker(marginal))
    #expect(!marginal.finalLabel.isCanonicalBurstFamily)
}

@Test
func p6b2_noTrimCreatesBridgeWithoutCore() throws {
    // A boundary trim must never strip the core: no trimmed candidate is a bridge-without-core, and every boundary-trim
    // rescue keeps a canonical burst label.
    let (ds, _, on) = try runs()
    for train in ds.trains {
        for c in cands(on, train.id) where hasTrimMarker(c) {
            #expect(c.finalLabel.isCanonicalBurstFamily)
            #expect(!c.decisionPath.contains("verdict_bridge_without_core"))
        }
    }
}

@Test
func p7a_explanationParsesRealAdaptiveV2Markers() throws {
    // P7A (updated for P11A): the display-only explanation parser classifies REAL detector markers from the 5x5 ON run —
    // boundary-trim rescues read as .boundaryTrim (info), P11A strong-core rescues read as .strongCoreRescue (info, a
    // KEPT outcome), and the genuine demotion reads as .demotedToPossible.
    let (ds, _, on) = try runs()
    var sawKept = false
    var sawStrongCore = false
    var sawDemote = false
    for train in ds.trains {
        for c in cands(on, train.id) {
            let e = AdaptiveV2CanonicalizationExplanation.parse(decisionPath: c.decisionPath)
            if hasTrimMarker(c) {
                #expect(e.kind == .boundaryTrim)
                #expect(e.tone == .info)
                #expect(c.finalLabel.isCanonicalBurstFamily)   // a trim is a KEPT outcome, not a deletion
                sawKept = true
            } else if hasCompatibleBoundaryKeptMarker(c) {
                // BURST-BOUNDARY-AUTH: Adaptive-v2 deferred to BCB's compatible boundary — a KEPT (full) outcome, mapped
                // onto the existing .boundaryTrim kind with nothing trimmed (leftTrimmed/rightTrimmed == false).
                #expect(e.kind == .boundaryTrim)
                #expect(e.leftTrimmed == false && e.rightTrimmed == false)
                #expect(e.tone == .info)
                #expect(c.finalLabel.isCanonicalBurstFamily)
                sawKept = true
            } else if hasStrongCoreMarker(c) {
                #expect(e.kind == .strongCoreRescue)
                #expect(e.tone == .info)
                #expect(c.finalLabel.isCanonicalBurstFamily)   // a strong-core rescue is a KEPT outcome
                sawStrongCore = true
            } else if hasDemoteMarker(c) {
                #expect(e.kind == .demotedToPossible)
                #expect(e.verdict != nil)
                sawDemote = true
            } else {
                #expect(e.kind == .none)
            }
        }
    }
    #expect(sawKept)        // the fixture exercises at least one boundary KEEP (trim OR BCB-compatible-boundary defer)
    #expect(sawStrongCore)  // …at least one P11A strong-core rescue (core ≥3, q95 moderately above ceiling)
    #expect(sawDemote)      // …and at least one demotion (the marginal [12...13])
}

@Test
func p6b2_userBurstGateBypassesAdaptiveV2GateUnderON() throws {
    // Semantic preservation: when the user sets an explicit burst HARD GATE, the automatic Adaptive-v2 gate must bail
    // entirely (parity with demotingTonicRateRegularBurstsAndRearbitrating) — so ON is byte-identical to OFF and no burst
    // is demoted/trimmed against the user's explicit bound.
    let ds = try fixtureDataset()
    let band = TrainAdaptiveBandSettings()
    let userGate = ManualThresholdProfile(
        burst: BurstManualThresholds(seedUpperISI: ManualISIThreshold(mode: .hardGate, valueSec: 0.020)))
    let off = ClassicAnchorDetectionPipeline.run(
        dataset: ds, bandSettings: band, manualThresholdProfile: userGate, useAdaptiveV2Canonicalization: false)
    let on = ClassicAnchorDetectionPipeline.run(
        dataset: ds, bandSettings: band, manualThresholdProfile: userGate, useAdaptiveV2Canonicalization: true)
    func digest(_ run: ClassicAnchorDetectionRun) -> [String] {
        ds.trains.flatMap { t in
            cands(run, t.id).map { "\($0.id)|\($0.finalLabel.rawValue)|\($0.startISIIndex)-\($0.endISIIndex)|\($0.selectedForAuto)|\($0.decisionPath)" }
        }
    }
    #expect(v2MarkerCount(on, ds.trains) == 0)   // the gate bailed: no demote/trim markers under ON
    #expect(digest(on) == digest(off))            // …and ON is byte-identical to OFF with the user gate active
}

// MARK: - diagnostic report (printed; uses the same eligibility ceiling the gate uses)

@Test
func p6b1_printAffectedCandidateReport() throws {
    let (ds, off, on) = try runs()
    let bt = burstTrains(ds)
    let offCanon = selectedCanonicalBurstCount(off, bt)
    let onCanon = selectedCanonicalBurstCount(on, bt)
    print("\n===== P6B-2 affected-candidate report (Adaptive-v2 ON; burst->burst = boundary-trim rescue) =====")
    print(String(format: "burst_response selected-canonical: OFF=%d ON=%d netLoss=%d demotionMarkers=%d",
                 offCanon, onCanon, offCanon - onCanon, v2MarkerCount(on, bt)))
    print("train | span | labelOFF->labelON | q90 | q95 | ceiling | core | bridgeExt | verdict")
    for train in burstTrains(ds) {
        let onByID = Dictionary(cands(on, train.id).map { ($0.id, $0) }, uniquingKeysWith: { a, _ in a })
        guard let prof = burstProfile(on, train.id) else { continue }
        let ceiling = prof.burst.bridgeUpperSec ?? prof.burst.upperSec
        let bRef = ResolvedFamilyDistributionSummary.fromTrain(train: train, family: .burst, lowerSec: prof.burst.lowerSec, upperSec: ceiling, provenance: .adaptive)
        let tRef = ResolvedFamilyDistributionSummary.fromTrain(train: train, family: .tonic, lowerSec: prof.tonic.lowerSec, upperSec: prof.tonic.upperSec, provenance: .adaptive)
        for c in cands(off, train.id) where c.selectedForAuto && c.finalLabel.isCanonicalBurstFamily {
            guard let onC = onByID[c.id], hasV2Marker(onC) else { continue }   // affected = demoted under ON
            let ev = evidenceFor(c, train, prof)
            let v = CanonicalizationVerdictBuilder.verdict(evidence: ev, burstReference: bRef, tonicReference: tRef,
                hfTonicReference: nil, burstEligibilityCeilingSec: ceiling)
            print(String(format: "%@ | [%d…%d] | %@->%@ | %.4f | %.4f | %.4f | %d | %d | %@",
                train.name, c.startISIIndex, c.endISIIndex, c.finalLabel.rawValue, onC.finalLabel.rawValue,
                ev.q90Sec ?? -1, ev.q95Sec ?? -1, ceiling,
                ev.burstCoreISICount ?? -1, ev.burstBridgeExtensionISICount ?? -1, v.verdict.rawValue))
        }
    }
    #expect(true)   // report only
}
