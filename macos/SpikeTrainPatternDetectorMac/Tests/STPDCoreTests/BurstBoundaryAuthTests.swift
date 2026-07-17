import Foundation
import STPDCore
import Testing

// MARK: - BURST-BOUNDARY-AUTH — reconcile BCB-1 / Adaptive-V2 boundary authority + short in-seed onset preservation
//
// Two independent boundary defects, both manifestations of an under-supported or double-applied boundary decision:
//   Case A (burst_response_2_s ISI 23 = 43.9 ms, a compatible TAIL): BCB-1 keeps it, but Adaptive-V2 independently
//     re-trimmed it against a tighter eligibility ceiling. Fix: Adaptive-V2 CONSUMES BCB's structured boundary class and
//     does not re-trim a BCB-compatible extension on a boundary-only failure (still demotes non-boundary failures).
//   Case B (burst_response_4_s ISI 85 = 24.9 ms, an accelerating ONSET): the in-seed internalEdge compares 24.9 to an
//     atypically fast short rest {6.8, 9.1} and mis-flags it. Fix: preserve a short in-seed onset under a CONJUNCTION of
//     scale-free signals — in-band seed, supported rest, strong pause-flank isolation, and NON-extreme boundary/rest-min
//     spread (≤ a DEDICATED dimensionless maximum `coreFirstBoundaryOnsetMaxSpreadToRestMin`, distinct from and oppositely
//     monotone to the structural-rescue minimum). Genuine contaminants (105.4, 164.1 ms: spread 12–14×) still trim.
// No fixed-ms cutoff; no tonic-presence predicate. Tests use the committed ClassicAnchorDetectionPipeline.run (HEAD-safe).

// Self-contained against committed HEAD: uses `ClassicAnchorDetectionPipeline.run` (the committed detection path the app
// framework delegates to) — NOT the uncommitted Hybrid-framework overload. Same app-exact band (5 ms bin) + default
// detector params + Adaptive-V2 toggle, which reproduces the app's selected/public burst output for these targets.
private func bbaAppRun(_ ds: SpikeDataset, adaptiveV2: Bool) -> ClassicAnchorDetectionRun {
    let q = SpikeQualitySettings()
    let band = TrainAdaptiveBandSettings(minValidISISec: max(q.artifactThresholdSec, 1e-9), histogramBinWidthSec: 0.005)
    return ClassicAnchorDetectionPipeline.run(
        dataset: ds, bandSettings: band, qualitySettings: q, refractoryAction: .warnOnly,
        stateTuning: StatePatternDetectorTuning(), detectorParameters: .defaults,
        manualThresholdProfile: .automatic, useAdaptiveV2Canonicalization: adaptiveV2, manualThresholdScope: .allTrains)
}
private func bba5x5() throws -> SpikeDataset {
    let url = URL(fileURLWithPath: #filePath).deletingLastPathComponent().appendingPathComponent("Fixtures/tonic_baseline_5x5.csv")
    return try CSVSpikeMatrixParser.parse(contents: try String(contentsOf: url, encoding: .utf8), datasetName: "5x5", sourceDescription: url.path)
}
private func bbaGrech() throws -> SpikeDataset? {
    let repo = URL(fileURLWithPath: #filePath).deletingLastPathComponent().deletingLastPathComponent()
        .deletingLastPathComponent().deletingLastPathComponent().deletingLastPathComponent()
    let url = repo.appendingPathComponent("inst/extdata/Grechishnikova_STN_2017_subset.csv")
    guard FileManager.default.fileExists(atPath: url.path) else { return nil }
    return try CSVSpikeMatrixParser.parse(contents: try String(contentsOf: url, encoding: .utf8), datasetName: "grech", sourceDescription: url.path, allowDerivedCSV: true)
}
private func bbaSelectedBursts(_ run: ClassicAnchorDetectionRun, _ train: SpikeTrain) -> [ClassicAnchorCandidate] {
    (run.result(for: train.id)?.candidates ?? []).filter { $0.selectedForAuto && $0.finalLabel.isBurstEventFamily }
}
private func bbaCovers(_ c: ClassicAnchorCandidate, _ isi: Int) -> Bool { isi >= c.startISIIndex && isi <= max(c.startISIIndex, c.endISIIndex) }
private func bbaPublicBurstCoversISI(_ run: ClassicAnchorDetectionRun, _ ds: SpikeDataset, _ train: SpikeTrain, _ isi: Int) -> Bool {
    run.eventAnnotations(in: ds, selectedOnly: true, tracks: [.event, .gap, .state])
        .filter { $0.trainID == train.id && $0.displayFamilyName == "Burst" }
        .contains { ($0.coveredISIIndices(in: train)?.contains(isi)) ?? false }
}
private func bbaCumulative(_ isis: [Double]) -> [Double] { var t: [Double] = [0]; for d in isis { t.append((t.last ?? 0) + d) }; return t }
private func bbaSynthBursts(_ isis: [Double], upper: Double) -> [ClassicAnchorCandidate] {
    let train = SpikeTrain(name: "synth", timestampsSec: bbaCumulative(isis))
    let s = ClassicAnchorSettings(minValidISISec: 0.001, burstBandLowerSec: 0.001, burstBandUpperSec: upper,
                                  burstBridgeUpperSec: 0.300, burstBandSource: .structure, burstBandIsStructureDerived: true, burstCoreReferenceUpperSec: upper)
    let sel = ClassicAnchorCandidateArbitrator.arbitrateBySemanticTrack(ClassicAnchorDetector.detect(train: train, settings: s).candidates)
    return sel.filter { $0.selectedForAuto && $0.finalLabel.isBurstEventFamily }
}

// MARK: 1 — app-exact V2-ON: burst_response_2_s selected/public Burst covers ISI 23 (Case A fixed)

@Test
func bba_caseA_appExactV2On_publicBurstCoversISI23() throws {
    let ds = try bba5x5()
    let train = try #require(ds.trains.first { $0.name == "burst_response_2_s" })
    #expect((train.isiSec[23] ?? 0) > 0.040 && (train.isiSec[23] ?? 0) < 0.048)   // ~43.9 ms
    let run = bbaAppRun(ds, adaptiveV2: true)
    let core = try #require(bbaSelectedBursts(run, train).first { bbaCovers($0, 21) && bbaCovers($0, 23) })
    #expect(core.startISIIndex == 21 && core.endISIIndex == 23)
    #expect(bbaPublicBurstCoversISI(run, ds, train, 23))   // the exact selected-annotation layer the pinned label reads
}

// MARK: 2 — V2-OFF still covers ISI 23

@Test
func bba_caseA_appExactV2Off_stillCoversISI23() throws {
    let ds = try bba5x5()
    let train = try #require(ds.trains.first { $0.name == "burst_response_2_s" })
    let run = bbaAppRun(ds, adaptiveV2: false)
    #expect(bbaSelectedBursts(run, train).contains { bbaCovers($0, 21) && bbaCovers($0, 23) })
    #expect(bbaPublicBurstCoversISI(run, ds, train, 23))
}

// MARK: 3 — Adaptive-V2 does not OVERRIDE a BCB-compatible boundary-only decision (structured authority, not substring)

@Test
func bba_caseA_adaptiveV2DefersToBCBCompatibleBoundary() throws {
    let ds = try bba5x5()
    let train = try #require(ds.trains.first { $0.name == "burst_response_2_s" })
    let run = bbaAppRun(ds, adaptiveV2: true)
    let kept = try #require((run.result(for: train.id)?.candidates ?? []).first {
        $0.startISIIndex == 21 && $0.endISIIndex == 23 && $0.candidateLayer == "structure_first_classic_burst_anchor" })
    #expect(kept.finalLabel.isCanonicalBurstFamily)                                            // kept CANONICAL (not demoted)
    #expect(kept.decisionPath.contains("adaptive_v2_canonicalization=bcb_compatible_boundary_kept"))
    #expect(!kept.decisionPath.contains("adaptive_v2_canonicalization=boundary_trim("))         // NOT re-trimmed
    // The trailing 43.9 ms boundary is a BCB compatibleExtension (typed structured result, NOT a decisionPath substring).
    let q = SpikeQualitySettings()
    let s = ClassicAnchorSettings(minValidISISec: max(q.artifactThresholdSec, 1e-9), burstBandUpperSec: 0.035,
                                  burstBandSource: .structure, burstBandIsStructureDerived: true, burstCoreReferenceUpperSec: 0.035)
    #expect(ClassicAnchorDetector.nonSeedBoundaryExtensionClass(of: kept, trailing: true, train: train, settings: s) == .compatibleExtension)
}

// MARK: 4 — a NON-boundary Adaptive-V2 failure is still demoted (the marginal [12...13] core, no slow edge)

@Test
func bba_adaptiveV2StillDemotesNonBoundaryFailure() throws {
    let ds = try bba5x5()
    let train = try #require(ds.trains.first { $0.name == "burst_response_2_s" })
    let cands = bbaAppRun(ds, adaptiveV2: true).result(for: train.id)?.candidates ?? []
    let marginal = try #require(cands.first {
        $0.startISIIndex == 12 && $0.endISIIndex == 13 && $0.decisionPath.contains("adaptive_v2_canonicalization") })
    #expect(marginal.decisionPath.contains("adaptive_v2_canonicalization=demote_to_possible"))
    #expect(!marginal.finalLabel.isCanonicalBurstFamily)
    #expect(!marginal.decisionPath.contains("bcb_compatible_boundary_kept"))
}

// MARK: 5 — app-exact: burst_response_4_s selected/public Burst covers ISI 85 with [85...87] (Case B fixed)

@Test
func bba_caseB_appExact_publicBurstCoversISI85_span85to87() throws {
    let ds = try bba5x5()
    let train = try #require(ds.trains.first { $0.name == "burst_response_4_s" })
    #expect((train.isiSec[85] ?? 0) > 0.022 && (train.isiSec[85] ?? 0) < 0.027)   // ~24.9 ms, in-seed
    for v2 in [false, true] {
        let run = bbaAppRun(ds, adaptiveV2: v2)
        let core = try #require(bbaSelectedBursts(run, train).first { bbaCovers($0, 85) })
        #expect(core.startISIIndex == 85 && core.endISIIndex == 87)
        #expect(core.decisionPath.contains("in_seed_onset_preserved"))
        #expect(bbaPublicBurstCoversISI(run, ds, train, 85))
    }
}

// MARK: 6/7 — genuine in-band contaminants are STILL trimmed (extreme boundary/rest-min spread)

@Test
func bba_contaminant_105_excludedAndRetainedBurstKept() {
    let bursts = bbaSynthBursts([0.5, 0.1054, 0.0443, 0.0085, 0.5], upper: 0.200)
    #expect(!bursts.contains { bbaCovers($0, 2) })                                    // 105.4 (idx 2) excluded
    #expect(bursts.contains { $0.startISIIndex == 3 && $0.endISIIndex == 4 && $0.finalLabel.isCanonicalBurstFamily })
}

@Test
func bba_contaminant_164_excludedAndRetainedBurstKept() {
    let bursts = bbaSynthBursts([0.5, 0.1641, 0.0332, 0.0115, 0.5], upper: 0.200)
    #expect(!bursts.contains { bbaCovers($0, 2) })                                    // 164.1 (idx 2) excluded
    #expect(bursts.contains { $0.startISIIndex == 3 && $0.endISIIndex == 4 && $0.finalLabel.isCanonicalBurstFamily })
}

// MARK: 8 — Grechishnikova EXACT canonical-span ledger pinned (count-only cannot detect span expansion). NO new canonical
//   burst is created; the ONLY deltas vs the pre-slice 8a5fc9d ledger are two SPAN EXTENSIONS on train `…_1_1`, both
//   documented consequences of the two fixes and flagged for explicit user approval (NOT auto-approved as "no change"):
//     Case-B onset (both toggles):  [18…22] → [17…22]  (ISI 17 = 10.0 ms accelerating onset; onset/rest-min 2.70× < 4×)
//     Case-A trailing (V2-ON only): (103,104) → (103,105) (ISI 105 = a BCB-compatible trailing Adaptive-V2 now keeps)

@Test
func bba_grechishnikovaCanonicalSpanLedgerPinned() throws {
    guard let ds = try bbaGrech() else { return }
    func ledger(_ v2: Bool) -> (count: Int, digest: Int, affected: [(Int, Int)]) {
        let run = bbaAppRun(ds, adaptiveV2: v2)
        var count = 0, digest = 0
        var affected: [(Int, Int)] = []
        for tr in ds.trains {
            let spans = (run.result(for: tr.id)?.candidates ?? [])
                .filter { $0.selectedForAuto && $0.finalLabel.isCanonicalBurstFamily }
                .map { ($0.startISIIndex, $0.endISIIndex) }.sorted { $0 < $1 }
            count += spans.count
            for (s, e) in spans { digest = digest &* 1000003 &+ (s &* 131 &+ e) }
            if tr.name == "LT1D01.96_fon_nw_minus_7_08_minus_1_1" { affected = spans }
        }
        return (count, digest, affected)
    }
    let off = ledger(false), on = ledger(true)
    #expect(off.count == 74 && on.count == 74 - 17)                 // canonical COUNT unchanged (no new burst)
    // EXACT span digest across all trains — catches ANY span drift beyond the two documented deltas below.
    #expect(off.digest == 6339587877383112071)
    #expect(on.digest == -55792772906424943)
    // The two documented span extensions on train _1_1 (and nothing else regressed):
    #expect(on.affected.contains { $0.0 == 17 && $0.1 == 22 } && !on.affected.contains { $0.0 == 18 && $0.1 == 22 })
    #expect(on.affected.contains { $0.0 == 103 && $0.1 == 105 } && !on.affected.contains { $0.0 == 103 && $0.1 == 104 })
    #expect(off.affected.contains { $0.0 == 17 && $0.1 == 22 })
}

// MARK: 8b — MONOTONICITY: onset preservation is DECOUPLED from the structural-rescue MINIMUM (Blocker-1 semantics)

@Test
func bba_onsetPreservation_decoupledFromStructuralRescueMinimum() {
    // Raising `burstStructuralRescueStrongCompressionMin` (a `>=` MINIMUM demanding STRONGER rescue) must NOT change onset
    // preservation — the onset rule uses a SEPARATE dimensionless maximum, not this parameter.
    func onsetKept(rescueMin: Double) -> Bool {
        let train = SpikeTrain(name: "syn", timestampsSec: bbaCumulative([0.5, 0.0249, 0.0068, 0.0091, 0.5]))
        let s = ClassicAnchorSettings(minValidISISec: 0.001, burstBandLowerSec: 0.001, burstBandUpperSec: 0.035,
                                      burstBridgeUpperSec: 0.300, burstBandSource: .structure, burstBandIsStructureDerived: true,
                                      burstStructuralRescueStrongCompressionMin: rescueMin, burstCoreReferenceUpperSec: 0.035)
        let sel = ClassicAnchorCandidateArbitrator.arbitrateBySemanticTrack(ClassicAnchorDetector.detect(train: train, settings: s).candidates)
            .filter { $0.selectedForAuto && $0.finalLabel.isBurstEventFamily }
        return sel.contains { bbaCovers($0, 2) }   // onset (24.9 ms) at ISI index 2
    }
    #expect(onsetKept(rescueMin: 4.0) == onsetKept(rescueMin: 15.0))   // identical → decoupled
    #expect(onsetKept(rescueMin: 15.0))                                // a HIGHER rescue minimum does not widen preservation
}

// MARK: 8c — MONOTONICITY: the dedicated onset-spread MAXIMUM is applied as a max (below → keep, above → trim)

@Test
func bba_onsetSpreadMaximum_appliedAsMaximum() {
    // The dedicated onset-spread maximum is a PRIVATE BCB constant (4.0). Prove it is applied as a MAXIMUM behaviorally:
    // an onset at spread 3.5 (≤ max) is kept; at 5.0 (> max) it is trimmed — so a larger max would be MORE permissive.
    let kept = bbaSynthBursts([0.5, 0.028, 0.008, 0.008, 0.5], upper: 0.050)      // onset/rest-min = 28/8 = 3.5 ≤ 4
    #expect(kept.contains { bbaCovers($0, 2) })                                    // onset (idx 2) KEPT
    let trimmed = bbaSynthBursts([0.5, 0.040, 0.008, 0.008, 0.5], upper: 0.050)   // onset/rest-min = 40/8 = 5.0 > 4
    #expect(!trimmed.contains { bbaCovers($0, 2) })                               // onset (idx 2) TRIMMED
    #expect(trimmed.contains { $0.startISIIndex == 3 && $0.endISIIndex == 4 })    // compact core retained
}

// MARK: 9 — pause_response trains gain NO burst false positives

@Test
func bba_pauseResponseNoBurstFalsePositives() throws {
    let ds = try bba5x5()
    for v2 in [false, true] {
        let run = bbaAppRun(ds, adaptiveV2: v2)
        for train in ds.trains where train.name.hasPrefix("pause_response_") {
            #expect(bbaSelectedBursts(run, train).isEmpty, "\(train.name) v2=\(v2) gained a burst")
        }
    }
}

// MARK: 10 — no-tonic fixture: the same onset decision holds (rule is tonic-independent)

@Test
func bba_noTonicFixture_onsetPreservedWithoutTonic() {
    // Four pause-isolated accelerating packets {onset 24, fast 7, fast 9} ms, no tonic baseline anywhere.
    let unit: [Double] = [0.700, 0.024, 0.007, 0.009]
    let isis = unit + unit + unit + unit + [0.700]
    let train = SpikeTrain(name: "no_tonic_onset", timestampsSec: bbaCumulative(isis))
    let ds = SpikeDataset(name: "no_tonic", sourceDescription: "synthetic", trains: [train])
    let run = ClassicAnchorDetectionPipeline.run(dataset: ds, bandSettings: TrainAdaptiveBandSettings(), useAdaptiveV2Canonicalization: true)
    let cands = run.result(for: train.id)?.candidates ?? []
    let bursts = cands.filter { $0.selectedForAuto && $0.finalLabel.isBurstEventFamily }
    #expect(!bursts.isEmpty)
    #expect(!cands.contains { $0.selectedForAuto && ($0.finalLabel == .tonic || $0.finalLabel == .highFrequencyTonic) })
    for onset in [2, 6, 10, 14] {   // each unit's onset ISI
        #expect(bursts.contains { bbaCovers($0, onset) }, "onset ISI \(onset) not preserved in a selected burst")
    }
}

// MARK: 11 — provenance is exact and non-duplicating (onset marker once; compatible-boundary-kept marker once)

@Test
func bba_provenanceExactAndNonDuplicating() throws {
    let ds = try bba5x5()
    let run = bbaAppRun(ds, adaptiveV2: true)
    let b4 = try #require(ds.trains.first { $0.name == "burst_response_4_s" })
    let onset = try #require((run.result(for: b4.id)?.candidates ?? []).first { $0.selectedForAuto && $0.startISIIndex == 85 && $0.endISIIndex == 87 })
    #expect(onset.decisionPath.components(separatedBy: "in_seed_onset_preserved").count - 1 == 1)
    let b2 = try #require(ds.trains.first { $0.name == "burst_response_2_s" })
    let kept = try #require((run.result(for: b2.id)?.candidates ?? []).first { $0.selectedForAuto && $0.startISIIndex == 21 && $0.endISIIndex == 23 })
    #expect(kept.decisionPath.components(separatedBy: "bcb_compatible_boundary_kept").count - 1 == 1)
}

// MARK: 12 — scale-free: whole-compact + tolerated-tail cases preserved (guards a regressed onset rule; no fixed-ms)

@Test
func bba_scaleFree_wholeCompactAndToleratedTailPreserved() {
    let whole = bbaSynthBursts([0.5, 0.031, 0.040, 0.5], upper: 0.050)
    #expect(whole.contains { $0.startISIIndex == 2 && $0.endISIIndex == 3 && $0.finalLabel.isCanonicalBurstFamily })
    let tail = bbaSynthBursts([0.5, 0.026, 0.020, 0.044, 0.5], upper: 0.050)
    #expect(tail.contains { $0.startISIIndex == 2 && $0.endISIIndex == 4 && $0.finalLabel.isCanonicalBurstFamily })
}
