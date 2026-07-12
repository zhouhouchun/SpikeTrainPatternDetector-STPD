import Foundation
@testable import STPDCore   // Blocker 1 test injects a tolerated_internal_tail gate via the internal withDiagnosticOverride
import Testing

// MARK: - BCB-HF — shared burst-boundary consistency for adaptive local-HF packets
//
// An `adaptive_local_hf_burst_packet` is generated with its own local-background core ceiling precisely WHEN the train's
// seed band collapses, so at its birth `ClassicAnchorDetector.detect` stage BCB-1's train-local seed-band reference is
// degenerate and cannot normalize its boundaries. Such a packet could therefore keep a gross incompatible leading ISI —
// several× the train-local seed-band upper — and, once Adaptive-V2 demotes the competing compact core, win possible-burst
// selection covering that contaminant. This slice re-applies the SAME scale-free two-evidence trim at the dataset-seed-
// aware pipeline seam (where a valid seed-band reference exists), before Adaptive-V2 canonicalization and final arbitration.
//
// Ground truth (user scientific position): in burst_response_2_s, ISI 11 ≈ 185.9 ms is NOT a burst ISI; the genuine
// compact core is ISIs 12…13 = [30.9, 40.0] ms. The rule is profile-relative (never a fixed-ms cutoff) and never uses
// tonic presence as burst evidence.

private func bcbHF5x5() throws -> SpikeDataset {
    let url = URL(fileURLWithPath: #filePath).deletingLastPathComponent().appendingPathComponent("Fixtures/tonic_baseline_5x5.csv")
    return try CSVSpikeMatrixParser.parse(contents: try String(contentsOf: url, encoding: .utf8),
                                          datasetName: "5x5", sourceDescription: url.path)
}

/// The on-disk Grechishnikova subset (five directory levels up); graceful nil if absent, exactly like the existing
/// AdaptiveV2/TonicBoundaryRescue Grechishnikova helpers.
private func bcbHFGrech() throws -> SpikeDataset? {
    let repo = URL(fileURLWithPath: #filePath)
        .deletingLastPathComponent().deletingLastPathComponent().deletingLastPathComponent()
        .deletingLastPathComponent().deletingLastPathComponent()
    let url = repo.appendingPathComponent("inst/extdata/Grechishnikova_STN_2017_subset.csv")
    guard FileManager.default.fileExists(atPath: url.path) else { return nil }
    return try CSVSpikeMatrixParser.parse(contents: try String(contentsOf: url, encoding: .utf8),
                                          datasetName: "grech", sourceDescription: url.path, allowDerivedCSV: true)
}

/// The APP-exact detection run (RasterDocument defaults): the framework entry point, adaptive-v2 canonicalization ON,
/// 5 ms histogram bin, automatic manual profile, all-trains scope. This is the runtime path that produced the false burst.
private func bcbHFAppRun(_ ds: SpikeDataset, adaptiveV2: Bool) -> ClassicAnchorDetectionRun {
    let q = SpikeQualitySettings()
    let band = TrainAdaptiveBandSettings(minValidISISec: max(q.artifactThresholdSec, 1e-9), histogramBinWidthSec: 0.005)
    let params = PatternDetectionParameterSettings(classicBurstContrastMin: 3.0, classicBurstFlankPauseContrastMin: 5.0,
                                                   classicBurstMinSpikes: 3, classicBurstMaxSpikes: 9, pauseMinISISecOverride: nil)
    return HybridPatternDetectionFramework.run(dataset: ds, bandSettings: band, qualitySettings: q,
        refractoryAction: .warnOnly, stateTuning: StatePatternDetectorTuning(), detectorParameters: params,
        manualThresholdProfile: .automatic, useAdaptiveV2Canonicalization: adaptiveV2, manualThresholdScope: .allTrains)
}

private func bcbHFSelectedBursts(_ run: ClassicAnchorDetectionRun, _ train: SpikeTrain) -> [ClassicAnchorCandidate] {
    (run.result(for: train.id)?.candidates ?? []).filter { $0.selectedForAuto && $0.finalLabel.isBurstEventFamily }
}

private func bcbHFCovers(_ c: ClassicAnchorCandidate, _ isi: Int) -> Bool {
    isi >= c.startISIIndex && isi <= max(c.startISIIndex, c.endISIIndex)
}

private func cumulative(_ isis: [Double]) -> [Double] {
    var t: [Double] = [0]; for d in isis { t.append((t.last ?? 0) + d) }; return t
}

/// Capture a GENUINE PRE-SEAM adaptive-HF packet spanning ISIs 11…13 from burst_response_2_s. It is produced under a
/// COLLAPSED band (non-structure-derived) with a degenerate `burstBandUpperSec = 0` so BCB-1's birth-time pass early-
/// returns and cannot trim it — reproducing exactly the state in which the packet reaches the pipeline seam untrimmed.
private func bcbHFCapturePreSeamPacket11to13(_ train: SpikeTrain) -> ClassicAnchorCandidate? {
    let collapsed = ClassicAnchorSettings(
        minValidISISec: 0.001, burstBandLowerSec: 0.001, burstBandUpperSec: 0.0,
        burstBridgeUpperSec: 0.060, burstBandSource: .structure, burstBandIsStructureDerived: false)
    return ClassicAnchorDetector.detect(train: train, settings: collapsed).candidates.first {
        $0.candidateLayer == ClassicAnchorDetector.adaptiveLocalHFBurstPacketLayer
            && $0.startISIIndex == 11 && $0.endISIIndex == 13
    }
}

/// The valid seed-aware band the seam supplies for burst_response_2_s (train-local seed-band upper ≈ 35 ms).
private func bcbHFSeedAwareSettings(upperSec: Double = 0.035, burstCoreMinISI: Int = 2) -> ClassicAnchorSettings {
    ClassicAnchorSettings(
        minValidISISec: 0.001, burstBandLowerSec: 0.001, burstBandUpperSec: upperSec,
        burstBridgeUpperSec: 0.060, burstBandSource: .structure, burstBandIsStructureDerived: true,
        burstCoreMinISI: burstCoreMinISI, burstCoreReferenceUpperSec: upperSec)
}

// MARK: E.1 — app-exact V2-ON: ISI 11 (185.9 ms) is NOT covered by any selected burst-family candidate

@Test
func bcbHF_appExactV2On_isi11NotInAnySelectedBurst() throws {
    let ds = try bcbHF5x5()
    let train = try #require(ds.trains.first { $0.name == "burst_response_2_s" })
    #expect(train.isiSec.indices.contains(11))
    #expect((train.isiSec[11] ?? 0) > 0.15 && (train.isiSec[11] ?? 0) < 0.22)   // the ~185.9 ms interval
    let selected = bcbHFSelectedBursts(bcbHFAppRun(ds, adaptiveV2: true), train)
    #expect(!selected.contains { bcbHFCovers($0, 11) })                          // no selected burst covers ISI 11
}

// MARK: E.2 — the compact core ISIs 12…13 = [30.9, 40.0] ms remains a SELECTED burst-family event

@Test
func bcbHF_appExactV2On_compactCore12to13RemainsSelectedBurstFamily() throws {
    let ds = try bcbHF5x5()
    let train = try #require(ds.trains.first { $0.name == "burst_response_2_s" })
    let selected = bcbHFSelectedBursts(bcbHFAppRun(ds, adaptiveV2: true), train)
    let core = selected.first { bcbHFCovers($0, 12) && bcbHFCovers($0, 13) }
    #expect(core != nil)
    #expect(core?.startISIIndex == 12)
    #expect(core?.finalLabel.isBurstEventFamily ?? false)
}

// MARK: Correction 1 — DIRECT resolver test on an UNNORMALIZED (pre-seam) packet actually performing the trim

@Test
func bcbHF_directResolver_trimsGrossLeadingEdgeOnUnnormalizedPacket() throws {
    let ds = try bcbHF5x5()
    let train = try #require(ds.trains.first { $0.name == "burst_response_2_s" })
    // A GENUINE pre-seam packet: span [11…13], adaptive-HF layer, ISIs ≈ [185.9, 30.9, 40.0] ms, NOT yet trimmed.
    let pre = try #require(bcbHFCapturePreSeamPacket11to13(train))
    #expect(pre.startISIIndex == 11 && pre.endISIIndex == 13)
    #expect(pre.candidateLayer == ClassicAnchorDetector.adaptiveLocalHFBurstPacketLayer)
    #expect(!pre.decisionPath.contains("core_first_boundary_trim"))              // proves it is UNNORMALIZED input
    #expect((train.isiSec[11] ?? 0) > 0.18 && (train.isiSec[11] ?? 0) < 0.19)    // 185.9 ms
    #expect((train.isiSec[12] ?? 0) > 0.029 && (train.isiSec[12] ?? 0) < 0.033)  // 30.9 ms
    #expect((train.isiSec[13] ?? 0) > 0.038 && (train.isiSec[13] ?? 0) < 0.042)  // 40.0 ms

    // Pass it directly to the reusable resolver with the valid seed-aware band reference (~35 ms).
    let out = try #require(
        ClassicAnchorDetector.boundaryNormalizedAdaptiveHFBurstPackets([pre], train: train, settings: bcbHFSeedAwareSettings()).first)
    #expect(out.startISIIndex == 12)                                             // gross 185.9 ms leading edge trimmed
    #expect(out.endISIIndex == 13)                                               // compact core 12…13 retained
    #expect(!bcbHFCovers(out, 11))                                               // ISI 11 excluded
    #expect(out.decisionPath.contains("core_first_boundary_trim("))              // trim provenance present
    #expect(out.decisionPath.contains("boundary_ref=train_local_seed_band_upper"))
    #expect(out.decisionPath.contains("\(ClassicAnchorDetector.adaptiveHFBoundaryConsistencyToken)=trimmed"))
    #expect(out.decisionPath.contains("adaptive_local_hf_burst_packet"))         // ORIGINAL provenance preserved
}

// MARK: Blocker 2 — idempotent outcome provenance (Semantic A: a terminal `trimmed` is retained across a no-op pass)

@Test
func bcbHF_directResolver_outcomeProvenanceIsIdempotent_semanticA() throws {
    let ds = try bcbHF5x5()
    let train = try #require(ds.trains.first { $0.name == "burst_response_2_s" })
    let pre = try #require(bcbHFCapturePreSeamPacket11to13(train))
    let once = try #require(
        ClassicAnchorDetector.boundaryNormalizedAdaptiveHFBurstPackets([pre], train: train, settings: bcbHFSeedAwareSettings()).first)
    let twice = try #require(
        ClassicAnchorDetector.boundaryNormalizedAdaptiveHFBurstPackets([once], train: train, settings: bcbHFSeedAwareSettings()).first)
    // Geometry and label are unchanged on the second pass (idempotence is a SEPARATE property from the trim itself).
    #expect(twice.startISIIndex == once.startISIIndex && twice.endISIIndex == once.endISIIndex)
    #expect(twice.finalLabel == once.finalLabel)
    // Parse decisionPath as exact semicolon-delimited tokens: EXACTLY ONE begins with the outcome prefix,
    let prefix = "\(ClassicAnchorDetector.adaptiveHFBoundaryConsistencyToken)="
    let outcomeTokens = twice.decisionPath.split(separator: ";").map(String.init).filter { $0.hasPrefix(prefix) }
    #expect(outcomeTokens.count == 1)
    // …and Semantic A retains the terminal `trimmed` transformation across the second no-op pass (does not erase history).
    #expect(outcomeTokens.first == "\(prefix)trimmed")
    // The b9738f5 trim provenance is still present exactly once (not duplicated) and never lost.
    #expect(twice.decisionPath.components(separatedBy: "core_first_boundary_trim(").count - 1 == 1)
}

// MARK: Blocker 2 (regression) — a later pass with a BLOCKED gross edge strengthens a non-terminal outcome to
// demoted_below_min. This exercises the short-circuit's blockedLeft/blockedRight guard: a blocked edge (dropLeft ==
// dropRight == 0 yet an edge remains) must NOT be short-circuited as a retained non-terminal outcome.

@Test
func bcbHF_idempotence_blockedGrossEdgeStrengthensNonTerminalToDemote() throws {
    let ds = try bcbHF5x5()
    let train = try #require(ds.trains.first { $0.name == "burst_response_2_s" })
    // Reshape a real adaptive-HF packet onto the real-train span [12…14] = {30.9, 40.0, 432.9} ms. The 432.9 ms ISI is a
    // NON-seed trailing neighbour: a COMPATIBLE extension under a wide (150 ms) band, but a GROSS edge under a tight
    // (45 ms) band — where, with burstCoreMinISI = 3, the 3-ISI span cannot be trimmed below the minimum size → demote.
    #expect((train.isiSec[14] ?? 0) > 0.40 && (train.isiSec[14] ?? 0) < 0.45)
    let base = try #require(bcbHFCapturePreSeamPacket11to13(train))
    let reshaped = base.withGeometry(
        startISIIndex: 12, endISIIndex: 14, startSpikeIndex: 12, endSpikeIndex: 15,
        nISI: base.nISI, nValidISI: base.nValidISI, nSpikes: base.nSpikes, durationSec: base.durationSec,
        intraQ10Sec: base.intraQ10Sec, intraQ40Sec: base.intraQ40Sec, intraQ50Sec: base.intraQ50Sec,
        intraQ90Sec: base.intraQ90Sec, intraQ95Sec: base.intraQ95Sec,
        maxIntraISISec: base.maxIntraISISec, meanIntraISISec: base.meanIntraISISec, cv: base.cv, lv: base.lv,
        preGapSec: base.preGapSec, postGapSec: base.postGapSec, preRatioQ90: base.preRatioQ90, postRatioQ90: base.postRatioQ90,
        edgeContrastMinQ90: base.edgeContrastMinQ90, edgeContrastGeomQ90: base.edgeContrastGeomQ90,
        decisionPath: base.decisionPath)
    let wide = bcbHFSeedAwareSettings(upperSec: 0.150, burstCoreMinISI: 3)
    let tight = bcbHFSeedAwareSettings(upperSec: 0.045, burstCoreMinISI: 3)

    // PASS 1 (wide band): 432.9 ms is within 3.0× → kept, a NON-TERMINAL compatible_extension_kept.
    let pass1 = try #require(
        ClassicAnchorDetector.boundaryNormalizedAdaptiveHFBurstPackets([reshaped], train: train, settings: wide).first)
    #expect(pass1.decisionPath.contains("\(ClassicAnchorDetector.adaptiveHFBoundaryConsistencyToken)=compatible_extension_kept"))
    #expect(pass1.finalLabel.isCanonicalBurstFamily)

    // PASS 2 (tight band, burstCoreMinISI = 3): the 432.9 ms edge is now gross but BLOCKED by the min-size floor → demote.
    let pass2 = try #require(
        ClassicAnchorDetector.boundaryNormalizedAdaptiveHFBurstPackets([pass1], train: train, settings: tight).first)
    #expect(pass2.finalLabel == .possibleBurst)
    #expect(pass2.action == "demote_to_possible")
    let prefix = "\(ClassicAnchorDetector.adaptiveHFBoundaryConsistencyToken)="
    let tokens = pass2.decisionPath.split(separator: ";").map(String.init).filter { $0.hasPrefix(prefix) }
    #expect(tokens.count == 1)                                          // exactly one outcome token
    #expect(tokens.first == "\(prefix)demoted_below_min")              // strengthened to the terminal outcome
    #expect(!pass2.decisionPath.contains("compatible_extension_kept")) // the earlier non-terminal outcome is ABSENT
    #expect(pass2.id == pass1.id)                                      // candidate ID preserved
    #expect(pass2.decisionPath.contains("adaptive_local_hf_burst_packet"))          // unrelated provenance preserved
    #expect(pass2.decisionPath.contains("routed_to_possible_burst_below_min_core")) // b9738f5 blocked-trim token preserved
}

// MARK: Blocker 1 — a tolerated_internal_tail gate must NOT exempt a gross opposite-side edge at the adaptive-HF seam

@Test
func bcbHF_seam_toleratedInternalTailDoesNotExemptGrossLeadingEdge() throws {
    let ds = try bcbHF5x5()
    let train = try #require(ds.trains.first { $0.name == "burst_response_2_s" })
    // A pre-seam adaptive-HF packet [11…13] = [185.9, 30.9, 40.0] ms: gross 185.9 ms LEADING edge, a valid compact seed
    // core (30.9 ms), and a compatible 40.0 ms trailing tail. Inject a tolerated_internal_tail gate as if an earlier
    // rescue had tolerated that tail.
    let base = try #require(bcbHFCapturePreSeamPacket11to13(train))
    let tolerated = base.withDiagnosticOverride(gateStatus: base.gateStatus + ";tolerated_internal_tail")
    #expect(tolerated.gateStatus.contains("tolerated_internal_tail"))
    #expect(tolerated.candidateLayer == ClassicAnchorDetector.adaptiveLocalHFBurstPacketLayer)
    #expect(tolerated.startISIIndex == 11 && tolerated.endISIIndex == 13)

    let out = try #require(
        ClassicAnchorDetector.boundaryNormalizedAdaptiveHFBurstPackets([tolerated], train: train, settings: bcbHFSeedAwareSettings()).first)
    #expect(out.startISIIndex == 12)                                   // gross LEADING edge trimmed despite the tail gate
    #expect(out.endISIIndex == 13)                                     // compact core retained…
    #expect((out.startISIIndex...out.endISIIndex).contains(13))        // …and the compatible 40.0 ms trailing tail preserved
    #expect(out.finalLabel.isBurstEventFamily)                         // remains burst-family (minimum size satisfied)
    #expect(out.decisionPath.contains("\(ClassicAnchorDetector.adaptiveHFBoundaryConsistencyToken)=trimmed"))
    #expect(!out.decisionPath.contains("exempt_tolerated_internal_tail"))   // NO whole-candidate exemption at the seam
    // Mirror sanity: the SAME rule that trims the gross leading edge keeps the compatible trailing tail (3.0× trailing),
    // so a tolerated compatible tail is preserved by the two-evidence rule, not by a blanket exemption.
    #expect((train.isiSec[13] ?? 0) > 0.038 && (train.isiSec[13] ?? 0) < 0.042)
}

// MARK: Correction 2 — resolver execution provenance is COMPLETE (one outcome per processed packet)

@Test
func bcbHF_provenance_trimmedOutcomeRecorded() throws {
    let ds = try bcbHF5x5()
    let train = try #require(ds.trains.first { $0.name == "burst_response_2_s" })
    let pre = try #require(bcbHFCapturePreSeamPacket11to13(train))
    let out = try #require(
        ClassicAnchorDetector.boundaryNormalizedAdaptiveHFBurstPackets([pre], train: train, settings: bcbHFSeedAwareSettings()).first)
    #expect(out.decisionPath.contains("\(ClassicAnchorDetector.adaptiveHFBoundaryConsistencyToken)=trimmed"))
}

@Test
func bcbHF_provenance_checkedNoChangeOutcomeRecorded() throws {
    let ds = try bcbHF5x5()
    let train = try #require(ds.trains.first { $0.name == "burst_response_2_s" })
    // A compact adaptive-HF packet [12…13] = [30.9, 40.0] ms (birth-time trimmed under a valid collapse band).
    let compact = try #require(
        ClassicAnchorDetector.detect(train: train, settings: ClassicAnchorSettings(
            minValidISISec: 0.001, burstBandLowerSec: 0.001, burstBandUpperSec: 0.035, burstBridgeUpperSec: 0.060,
            burstBandSource: .structure, burstBandIsStructureDerived: false, burstCoreReferenceUpperSec: 0.035)).candidates.first {
                $0.candidateLayer == ClassicAnchorDetector.adaptiveLocalHFBurstPacketLayer
                    && $0.startISIIndex == 12 && $0.endISIIndex == 13 })
    // With a WIDER band (50 ms) BOTH 30.9 and 40.0 ms are in-band seeds and neither is an edge → NO change.
    let out = try #require(
        ClassicAnchorDetector.boundaryNormalizedAdaptiveHFBurstPackets([compact], train: train, settings: bcbHFSeedAwareSettings(upperSec: 0.050)).first)
    #expect(out.startISIIndex == 12 && out.endISIIndex == 13)                    // geometry unchanged
    #expect(out.finalLabel == compact.finalLabel)                               // label unchanged
    #expect(out.selectedForAuto == compact.selectedForAuto)                     // selection unchanged
    #expect(out.decisionPath.contains("\(ClassicAnchorDetector.adaptiveHFBoundaryConsistencyToken)=checked_no_change"))
}

@Test
func bcbHF_provenance_demotedBelowMinOutcomeRecorded() throws {
    let ds = try bcbHF5x5()
    let train = try #require(ds.trains.first { $0.name == "burst_response_2_s" })
    let pre = try #require(bcbHFCapturePreSeamPacket11to13(train))               // [11…13], 3 ISIs
    // Force minSpanISI = 3 (burstCoreMinISI = 3): the 3-ISI packet cannot be trimmed below the floor, so the gross
    // 185.9 ms leading edge remains but is BLOCKED → demoted to possibleBurst (the established BCB-1 blocked-trim path).
    let out = try #require(
        ClassicAnchorDetector.boundaryNormalizedAdaptiveHFBurstPackets(
            [pre], train: train, settings: bcbHFSeedAwareSettings(burstCoreMinISI: 3)).first)
    #expect(out.finalLabel == .possibleBurst)
    #expect(out.decisionPath.contains("\(ClassicAnchorDetector.adaptiveHFBoundaryConsistencyToken)=demoted_below_min"))
    #expect(out.decisionPath.contains("routed_to_possible_burst_below_min_core"))   // preserved b9738f5 blocked-trim token
}

@Test
func bcbHF_provenance_compatibleExtensionKeptOutcomeRecorded() throws {
    let ds = try bcbHF5x5()
    let train = try #require(ds.trains.first { $0.name == "burst_response_2_s" })
    // [12…13] with a 35 ms band: 30.9 ms is an in-band seed, 40.0 ms is a NON-seed compatible trailing extension
    // (40.0 / 35 = 1.14 < 3.0× trailing) → kept, recorded as a compatible extension.
    let compact = try #require(
        ClassicAnchorDetector.detect(train: train, settings: ClassicAnchorSettings(
            minValidISISec: 0.001, burstBandLowerSec: 0.001, burstBandUpperSec: 0.035, burstBridgeUpperSec: 0.060,
            burstBandSource: .structure, burstBandIsStructureDerived: false, burstCoreReferenceUpperSec: 0.035)).candidates.first {
                $0.candidateLayer == ClassicAnchorDetector.adaptiveLocalHFBurstPacketLayer
                    && $0.startISIIndex == 12 && $0.endISIIndex == 13 })
    let out = try #require(
        ClassicAnchorDetector.boundaryNormalizedAdaptiveHFBurstPackets([compact], train: train, settings: bcbHFSeedAwareSettings()).first)
    #expect(out.startISIIndex == 12 && out.endISIIndex == 13)
    #expect(out.decisionPath.contains("\(ClassicAnchorDetector.adaptiveHFBoundaryConsistencyToken)=compatible_extension_kept"))
    #expect(out.decisionPath.contains("candidate_relative_compatible_extension"))   // preserved b9738f5 token
}

// MARK: Correction 2.4 — NO selected adaptive-HF burst-family packet reaches final output without provenance

@Test
func bcbHF_everySelectedAdaptiveHFPacketCarriesBoundaryProvenance_5x5_v2On() throws {
    let ds = try bcbHF5x5()
    let run = bcbHFAppRun(ds, adaptiveV2: true)
    for train in ds.trains {
        for c in (run.result(for: train.id)?.candidates ?? [])
        where c.selectedForAuto && c.finalLabel.isBurstEventFamily
            && c.candidateLayer == ClassicAnchorDetector.adaptiveLocalHFBurstPacketLayer {
            #expect(c.decisionPath.contains("\(ClassicAnchorDetector.adaptiveHFBoundaryConsistencyToken)="),
                    "selected adaptive-HF packet [\(c.startISIIndex)...\(c.endISIIndex)] on \(train.name) lacks boundary-consistency provenance")
        }
    }
}

// MARK: E.4 — adaptive-v2 OFF characterization is preserved (ISI 11 Other; compact [12…13] canonical selected)

@Test
func bcbHF_appExactV2Off_isi11StaysOther_and12to13Canonical() throws {
    let ds = try bcbHF5x5()
    let train = try #require(ds.trains.first { $0.name == "burst_response_2_s" })
    let selected = bcbHFSelectedBursts(bcbHFAppRun(ds, adaptiveV2: false), train)
    #expect(!selected.contains { bcbHFCovers($0, 11) })                          // ISI 11 remains Other under V2-OFF
    let core = try #require(selected.first { bcbHFCovers($0, 12) && bcbHFCovers($0, 13) })
    #expect(core.startISIIndex == 12)
    #expect(core.finalLabel.isCanonicalBurstFamily)                              // structure-first canonical burst
}

// MARK: Correction 3 — Grechishnikova regression: pin the RETAINED COMPACT CORE + gross-edge exclusion (app-exact V2-ON)

@Test
func bcbHF_grechishnikovaCompactCoreRetained_grossTrailingEdgeExcluded_v2On() throws {
    guard let ds = try bcbHFGrech() else { return }              // graceful skip if the repo fixture is absent
    let run = bcbHFAppRun(ds, adaptiveV2: true)
    // Affected train A — the [92…94] → [92…93] trim. ISI 92 = 9.8 ms, 93 = 4.7 ms (compact core), 94 = 32.5 ms (gross).
    let trainA = try #require(ds.trains.first { $0.name == "LT1D01.96_fon_nw_minus_7_08_minus_1_2" })
    let selA = bcbHFSelectedBursts(run, trainA)
    #expect(selA.contains { bcbHFCovers($0, 92) && bcbHFCovers($0, 93) })        // genuine compact core RETAINED
    #expect(!selA.contains { bcbHFCovers($0, 94) })                             // gross 32.5 ms trailing edge EXCLUDED
    // The retained core carries boundary-consistency provenance.
    let coreA = try #require(selA.first { bcbHFCovers($0, 92) && bcbHFCovers($0, 93)
        && $0.candidateLayer == ClassicAnchorDetector.adaptiveLocalHFBurstPacketLayer })
    #expect(coreA.endISIIndex == 93)
    #expect(coreA.decisionPath.contains("\(ClassicAnchorDetector.adaptiveHFBoundaryConsistencyToken)="))
    // Affected train B — the [98…99] = [4.8, 0.9] ms fast packet demoted; its coverage must NOT be lost entirely.
    let trainB = try #require(ds.trains.first { $0.name == "LT1D01.96_fon_nw_minus_7_08_minus_1_1" })
    let selB = bcbHFSelectedBursts(run, trainB)
    #expect(selB.contains { bcbHFCovers($0, 98) && bcbHFCovers($0, 99) })        // fast core still covered (no burst lost)
}

// MARK: Correction 4 — the ACTUAL selected-annotation layer consumed by RasterDocument / RasterCanvasView

@Test
func bcbHF_appSelectedAnnotations_isi11NotBurstFamily_v2On() throws {
    let ds = try bcbHF5x5()
    let train = try #require(ds.trains.first { $0.name == "burst_response_2_s" })
    let run = bcbHFAppRun(ds, adaptiveV2: true)
    // (1) no selected burst-family CANDIDATE covers ISI 11
    #expect(!bcbHFSelectedBursts(run, train).contains { bcbHFCovers($0, 11) })
    // Build the SAME selected event-annotation set the pinned Auto label reads (selectedOnly, event+gap+state tracks).
    let anns = run.eventAnnotations(in: ds, selectedOnly: true, tracks: [.event, .gap, .state])
        .filter { $0.trainID == train.id }
    func covers(_ a: ClassicAnchorEventAnnotation, _ isi: Int) -> Bool {
        (a.coveredISIIndices(in: train)?.contains(isi)) ?? false
    }
    // (3) no Burst-family annotation covers ISI 11
    #expect(!anns.contains { $0.displayFamilyName == "Burst" && covers($0, 11) })
    // (4)+(5) the selected annotation covering ISIs 12 & 13 is Burst-family, and its Burst display spans ONLY 12…13.
    let core = try #require(anns.first { covers($0, 12) && covers($0, 13) })
    #expect(core.displayFamilyName == "Burst")
    #expect(!covers(core, 11))
    #expect(core.coveredISIIndices(in: train)?.lowerBound == 12)
    // (6) possibleBurst display semantics unchanged: under V2-ON this core annotation IS a possibleBurst, and it still
    //     displays under the "Burst" family (the slice corrected candidate geometry, NOT the possibleBurst display map).
    #expect(core.label == .possibleBurst)
    #expect(core.displayFamilyName == "Burst")
}

// MARK: Characterization guard (weaker than exact parity) — no OBVIOUSLY gross leading ISI survives on a selected burst
//
// NOTE: this is a COARSE characterization guard, NOT a proof of exact rule parity. Production trims a non-seed leading
// edge against the TRAIN-LOCAL SEED-BAND UPPER at the asymmetric 2.0× limit; this test cannot access that per-train
// reference, so it only checks the candidate-relative ratio (leading ISI vs its own span median, at the lenient 3.0×).
// It would miss a 2.0–3.0× contaminant. It exists to catch a [11…13]-scale (≈5×) regression, not to certify the rule.

@Test
func bcbHF_characterizationGuard_noObviouslyGrossLeadingEdgeSurvives_5x5_v2On() throws {
    let ds = try bcbHF5x5()
    let run = bcbHFAppRun(ds, adaptiveV2: true)
    for train in ds.trains where train.name.hasPrefix("burst_response_") {
        for burst in bcbHFSelectedBursts(run, train) {
            let span = (burst.startISIIndex...max(burst.startISIIndex, burst.endISIIndex))
                .compactMap { train.isiSec.indices.contains($0) ? train.isiSec[$0] ?? nil : nil }
            guard let leading = span.first, span.count >= 2 else { continue }
            let sorted = span.sorted()
            let median = sorted[sorted.count / 2]
            #expect(leading <= median * 3.0,
                    "\(train.name) selected burst [\(burst.startISIIndex)...\(burst.endISIIndex)] keeps an obviously gross leading \(leading*1000) ms vs span median \(median*1000) ms")
        }
    }
}

// MARK: E.7 — the boundary rule is independent of tonic presence (no-tonic fixture)

@Test
func bcbHF_boundaryRuleIndependentOfTonicPresence_noTonicFixture() {
    // A NO-TONIC train with four CLEAN compact cores {31, 35, 40} ms (which establish a small train-local seed-band
    // reference) plus a fifth core carrying a genuine gross 190 ms leading contaminant. Every inter-burst gap is a
    // 700 ms pause; there is no regular tonic baseline anywhere. The gross leading edge must be excluded from the
    // selected burst while the compact core is kept — proving the boundary rule uses no tonic evidence.
    let clean: [Double] = [0.031, 0.035, 0.040, 0.700]
    let contaminated: [Double] = [0.190, 0.031, 0.035, 0.040, 0.700]
    let isis = clean + clean + clean + clean + contaminated
    let train = SpikeTrain(name: "no_tonic_gross_edge", timestampsSec: cumulative(isis))
    let ds = SpikeDataset(name: "no_tonic", sourceDescription: "synthetic", trains: [train])
    let run = HybridPatternDetectionFramework.run(dataset: ds, bandSettings: TrainAdaptiveBandSettings(),
                                                  useAdaptiveV2Canonicalization: true)
    let cands = run.result(for: train.id)?.candidates ?? []
    let selectedBursts = cands.filter { $0.selectedForAuto && $0.finalLabel.isBurstEventFamily }
    let selectedTonic = cands.filter { $0.selectedForAuto && ($0.finalLabel == .tonic || $0.finalLabel == .highFrequencyTonic) }
    #expect(!selectedBursts.isEmpty)                                             // the compact cores are detected as bursts
    #expect(selectedTonic.isEmpty)                                              // nothing tonic — no tonic baseline exists
    // The contaminated core is the 5th unit: clean×4 = 16 ISIs, so its gross 190 ms leading edge is ISI 17 and its
    // compact core is ISIs 18…20. The gross edge must be excluded; the compact core must remain in a selected burst.
    #expect(!selectedBursts.contains { bcbHFCovers($0, 17) },
            "gross 190 ms leading edge (ISI 17) must not be inside a selected burst")
    #expect(selectedBursts.contains { bcbHFCovers($0, 18) && bcbHFCovers($0, 20) })  // compact core kept
}
