import Foundation
import STPDCore
import Testing

@Test
func classicAnchorDetectsTwoSidedBurstRun() throws {
    let train = SpikeTrain(
        name: "unit_1",
        timestampsSec: [0, 0.100, 0.106, 0.112, 0.118, 0.205, 0.260, 0.320]
    )
    let settings = ClassicAnchorSettings(
        minValidISISec: 0.0009,
        burstBandLowerSec: 0.001,
        burstBandUpperSec: 0.010,
        burstBandSource: .userPatternISILimit
    )

    let result = ClassicAnchorDetector.detect(train: train, settings: settings)
    let candidate = try #require(result.candidates.first { $0.candidateLayer == "structure_first_classic_burst_anchor" })

    #expect(result.candidates.filter { $0.candidateLayer == "structure_first_classic_burst_anchor" }.count == 1)
    #expect(candidate.candidateLayer == "structure_first_classic_burst_anchor")
    #expect(candidate.candidateClass == "structure_first_two_sided_classic_burst_i")
    #expect(candidate.gateStatus == "structure_first_two_sided_classic_burst_i_pass")
    #expect(candidate.finalLabel == .burst)
    #expect(candidate.anchorLockLevel == .lockedClassic)
    #expect(candidate.startISIIndex == 2)
    #expect(candidate.endISIIndex == 4)
    #expect(candidate.startSpikeIndex == 2)
    #expect(candidate.endSpikeIndex == 5)
    #expect(candidate.nISI == 3)
    #expect(candidate.nSpikes == 4)
    #expect(isClose(candidate.preGapSec, 0.100))
    #expect(isClose(candidate.postGapSec, 0.087))
    #expect(isClose(candidate.intraQ90Sec, 0.006))
    #expect(isClose(candidate.edgeContrastMinQ90, 14.5, tolerance: 1e-10))
    #expect(candidate.anchorBandSource == .structure)
}

@Test
func classicAnchorBridgesSingleInternalISIWithinBurstRun() throws {
    let train = SpikeTrain(
        name: "unit_bridge",
        timestampsSec: [0, 0.100, 0.111, 0.122, 0.1386, 0.144, 0.230]
    )
    let settings = ClassicAnchorSettings(
        minValidISISec: 0.0009,
        burstBandLowerSec: 0.001,
        burstBandUpperSec: 0.015,
        burstBridgeUpperSec: 0.020
    )

    let result = ClassicAnchorDetector.detect(train: train, settings: settings)
    let candidate = try #require(result.candidates.first { $0.finalLabel == .burst })

    #expect(candidate.finalLabel == .burst)
    #expect(candidate.anchorLockLevel == .lockedClassic)
    #expect(candidate.candidateLayer == "structure_first_classic_burst_anchor")
    #expect(candidate.candidateClass == "structure_first_two_sided_classic_burst_i")
    #expect(candidate.gateStatus == "structure_first_two_sided_classic_burst_i_pass")
    #expect(candidate.startISIIndex == 2)
    #expect(candidate.endISIIndex == 5)
    #expect(candidate.nISI == 4)
    #expect(candidate.nSpikes == 5)
    #expect(isClose(candidate.maxIntraISISec, 0.0166))
    #expect((candidate.edgeContrastMinQ90 ?? 0) > candidate.anchorContrastMinRequired)
    #expect(candidate.burstSeedRunStartISI == 2)
    #expect(candidate.burstSeedRunEndISI == 5)
    #expect(isClose(candidate.burstSeedBandLowerSec, settings.minValidISISec))
    #expect(isClose(candidate.burstSeedBandUpperSec, candidate.intraQ90Sec ?? 0))
    #expect(isClose(candidate.burstBridgeBandUpperSec, candidate.intraQ90Sec ?? 0))
    #expect(isClose(candidate.burstContrastRequired, 3.0))
    #expect(isClose(candidate.burstPossibleContrastRequired, 2.0))
    let intraQ90Sec = try #require(candidate.intraQ90Sec)
    #expect(isClose(candidate.burstRequiredGapSec, intraQ90Sec * 3.0))
    #expect(isClose(candidate.burstPossibleRequiredGapSec, intraQ90Sec * 2.0))
    #expect(candidate.burstStrictBoundaryPass == true)
    #expect(candidate.burstPossibleBoundaryPass == true)
    #expect(candidate.burstBridgeCountPass == true)
    #expect(candidate.burstBridgeFractionPass == true)
    #expect(candidate.burstQ90BridgePass == true)
    #expect(candidate.burstSizeLabelBeforeReview == "burst")
}

@Test
func classicAnchorRejectsWeakTwoSidedContrast() throws {
    let train = SpikeTrain(
        name: "unit_weak",
        timestampsSec: [0, 0.011, 0.017, 0.023, 0.029, 0.040, 0.052]
    )
    let settings = ClassicAnchorSettings(
        minValidISISec: 0.0009,
        burstBandLowerSec: 0.001,
        burstBandUpperSec: 0.010,
        burstBandSource: .userPatternISILimit
    )

    let result = ClassicAnchorDetector.detect(train: train, settings: settings)
    let candidate = try #require(result.candidates.first)

    #expect(result.candidates.count == 1)
    #expect(candidate.finalLabel == .reject)
    #expect(candidate.action == "reject")
    #expect(candidate.gateStatus == "event_grammar_reject")
    #expect(candidate.decisionPath.hasPrefix("flank_contrast_fail"))
    #expect(candidate.failureReason.hasPrefix("flank_contrast_fail"))
    #expect(candidate.candidateDiagnosticClass.hasPrefix("rejected__flank_contrast_fail"))
    #expect(!candidate.isEligibleForAutoSelection)
}

@Test
func classicAnchorKeepsBridgeRejectDiagnosticAuditRows() throws {
    let train = SpikeTrain(
        name: "unit_bridge_reject",
        timestampsSec: [0, 0.100, 0.106, 0.112, 0.1286, 0.230]
    )
    let settings = ClassicAnchorSettings(
        minValidISISec: 0.0009,
        burstBandLowerSec: 0.001,
        burstBandUpperSec: 0.010,
        burstBridgeUpperSec: 0.020,
        burstBandSource: .userPatternISILimit,
        burstBridgeMaxCount: 0
    )

    let result = ClassicAnchorDetector.detect(train: train, settings: settings)
    let structureFirst = try #require(result.candidates.first { $0.candidateLayer == "structure_first_classic_burst_anchor" })

    #expect(structureFirst.finalLabel == .burst)
    #expect(structureFirst.isEligibleForAutoSelection)
    #expect(structureFirst.anchorBandSource == .structure)
    #expect(!result.candidates.contains { candidate in
        candidate.finalLabel == .reject &&
            candidate.decisionPath.contains("too_many_bridge_isis")
    })
}

@Test
func hardThresholdBurstCandidateMatchesREventCoreSemantics() throws {
    let train = SpikeTrain(
        name: "unit_hard_threshold",
        timestampsSec: cumulativeTimestamps([
            0.200,
            0.080,
            0.050,
            0.060,
            0.040,
            0.200
        ])
    )
    let settings = ClassicAnchorSettings(
        minValidISISec: 0.001,
        burstBandLowerSec: 0.001,
        burstBandUpperSec: 0.070,
        burstHardThresholdEnabled: true
    )

    let result = ClassicAnchorDetector.detect(train: train, settings: settings)
    let hard = try #require(result.candidates.first { candidate in
        candidate.candidateLayer == "isi_profile_hard_threshold_burst"
    })

    #expect(hard.candidateClass == "isi_profile_hard_threshold_burst")
    #expect(hard.finalLabel == .burst)
    #expect(hard.gateStatus == "isi_profile_hard_threshold_burst_pass")
    #expect(hard.decisionPath == "hard_threshold_direct_seed_bridge_without_flank_contrast_gate")
    #expect(hard.action == "accept")
    #expect(hard.priority == 1_450)
    #expect(isClose(hard.score, 33.0))
    #expect(hard.startISIIndex == 2)
    #expect(hard.endISIIndex == 5)
    #expect(hard.nSpikes == 5)
    #expect(hard.hardThreshold == true)
    #expect(hard.thresholdMode == "hard_threshold")
    #expect(hard.hardThresholdPattern == "burst")
    #expect(isClose(hard.hardBurstSeedUpperSec, 0.070))
    #expect(isClose(hard.hardBurstBridgeUpperSec, 0.0875))
    #expect(hard.hardBurstCoreISICount == 3)
    #expect(hard.hardThresholdSource == "ui_isi_profile_threshold_line")
}

@Test
func classicAnchorStructuralRescueAcceptsCompactClusterWithWeakLocalFlanks() throws {
    let isi = [
        0.450, 0.450, 0.450, 0.450, 0.450,
        0.090,
        0.040, 0.040, 0.040, 0.040, 0.070,
        0.090,
        0.450, 0.450, 0.450
    ]
    var timestamps = [0.0]
    for value in isi {
        timestamps.append((timestamps.last ?? 0) + value)
    }
    let train = SpikeTrain(name: "unit_structural_rescue", timestampsSec: timestamps)
    let settings = ClassicAnchorSettings(
        minValidISISec: 0.0009,
        burstBandLowerSec: 0.001,
        burstBandUpperSec: 0.050,
        burstBridgeUpperSec: 0.080,
        burstBandSource: .userPatternISILimit,
        burstContrastMin: 2.5,
        possibleBurstContrastMin: 2.0
    )

    let result = ClassicAnchorDetector.detect(train: train, settings: settings)
    let rescued = try #require(result.candidates.first { candidate in
        candidate.startISIIndex == 7 &&
            candidate.endISIIndex == 11 &&
            candidate.gateStatus == "event_grammar_structural_burst_rescue_pass"
    })

    #expect(rescued.finalLabel == .burst)
    #expect(rescued.action == "accept")
    #expect(rescued.anchorLockLevel == .lockedClassic)
    #expect(rescued.candidateClass == "event_grammar_seed_centered_burst")
    #expect(rescued.decisionPath == "compact_short_isi_cluster_rescued_by_train_scale_compression__burst")
    #expect((rescued.edgeContrastMinQ90 ?? 0) < rescued.anchorContrastMinRequired)
    #expect(rescued.isEligibleForAutoSelection)
}

@Test
func classicAnchorEpisodeRescuesDenseLowISIPacketOutsideStrictSeedRun() throws {
    let isi = [
        0.200,
        0.009, 0.030, 0.031, 0.029, 0.032,
        0.200, 0.200, 0.200
    ]
    var timestamps = [0.0]
    for value in isi {
        timestamps.append((timestamps.last ?? 0) + value)
    }
    let train = SpikeTrain(name: "unit_episode_rescue", timestampsSec: timestamps)
    let settings = ClassicAnchorSettings(
        minValidISISec: 0.0009,
        burstBandLowerSec: 0.001,
        burstBandUpperSec: 0.010,
        burstBridgeUpperSec: 0.020,
        burstBandSource: .userPatternISILimit,
        burstContrastMin: 2.5,
        possibleBurstContrastMin: 2.0
    )

    let result = ClassicAnchorDetector.detect(train: train, settings: settings)
    #expect(!result.candidates.contains { candidate in
        candidate.candidateLayer == "event_grammar_burst_episode" &&
            candidate.startISIIndex == 2 &&
            candidate.endISIIndex == 6 &&
            candidate.isEligibleForAutoSelection
    })
    #expect(result.candidates.allSatisfy { candidate in
        guard candidate.candidateLayer == "event_grammar_burst_episode" else {
            return true
        }
        guard let seedStart = candidate.burstSeedRunStartISI,
              let seedEnd = candidate.burstSeedRunEndISI else {
            return false
        }
        return seedEnd - seedStart + 1 >= settings.burstEpisodeMinSeedISI
    })
    #expect(!result.candidates.contains { candidate in
        candidate.candidateLayer == "event_grammar_burst_event" &&
            candidate.startISIIndex == 2 &&
            candidate.endISIIndex == 6 &&
            candidate.finalLabel == .burst
    })
}

@Test
func classicAnchorAcceptsStartBoundaryBurstWithSinglePostFlank() throws {
    let train = SpikeTrain(
        name: "unit_start_boundary",
        timestampsSec: [0, 0.005, 0.010, 0.015, 0.100, 0.160]
    )
    let settings = ClassicAnchorSettings(
        minValidISISec: 0.0009,
        burstBandLowerSec: 0.001,
        burstBandUpperSec: 0.010
    )

    let result = ClassicAnchorDetector.detect(train: train, settings: settings)
    let candidate = try #require(result.candidates.first { $0.candidateLayer == "structure_first_classic_burst_anchor" })

    #expect(result.candidates.filter { $0.candidateLayer == "structure_first_classic_burst_anchor" }.count == 1)
    #expect(candidate.finalLabel == .burst)
    #expect(candidate.anchorLockLevel == .lockedClassic)
    #expect(candidate.candidateClass == "structure_first_endpoint_classic_burst_i")
    #expect(candidate.gateStatus == "structure_first_endpoint_classic_burst_i_pass")
    #expect(candidate.startISIIndex == 1)
    #expect(candidate.endISIIndex == 3)
    #expect(candidate.preGapSec == nil)
    #expect(isClose(candidate.postGapSec, 0.085))
    #expect((candidate.edgeContrastMinQ90 ?? 0) > candidate.anchorContrastMinRequired)
}

@Test
func classicAnchorAcceptsEndBoundaryBurstWithSinglePreFlank() throws {
    let train = SpikeTrain(
        name: "unit_end_boundary",
        timestampsSec: [0, 0.060, 0.150, 0.155, 0.160, 0.165]
    )
    let settings = ClassicAnchorSettings(
        minValidISISec: 0.0009,
        burstBandLowerSec: 0.001,
        burstBandUpperSec: 0.010
    )

    let result = ClassicAnchorDetector.detect(train: train, settings: settings)
    let candidate = try #require(result.candidates.first { $0.candidateLayer == "structure_first_classic_burst_anchor" })

    #expect(result.candidates.filter { $0.candidateLayer == "structure_first_classic_burst_anchor" }.count == 1)
    #expect(candidate.finalLabel == .burst)
    #expect(candidate.anchorLockLevel == .lockedClassic)
    #expect(candidate.candidateClass == "structure_first_endpoint_classic_burst_i")
    #expect(candidate.gateStatus == "structure_first_endpoint_classic_burst_i_pass")
    #expect(candidate.startISIIndex == 3)
    #expect(candidate.endISIIndex == 5)
    #expect(isClose(candidate.preGapSec, 0.090))
    #expect(candidate.postGapSec == nil)
    #expect((candidate.edgeContrastMinQ90 ?? 0) > candidate.anchorContrastMinRequired)
}

@Test
func classicAnchorDemotesOnlyMinimalRefractorySuspectBurst() throws {
    let train = SpikeTrain(
        name: "unit_minimal_refractory",
        timestampsSec: [0, 0.100, 0.1011, 0.1022, 0.205]
    )
    let settings = ClassicAnchorSettings(
        minValidISISec: 0.001,
        burstBandLowerSec: 0.001,
        burstBandUpperSec: 0.010,
        refractorySuspectSec: 0.0015,
        refractoryAction: .demoteToPossible
    )

    let result = ClassicAnchorDetector.detect(train: train, settings: settings)
    let candidate = try #require(result.candidates.first {
        $0.startISIIndex == 2 && $0.endISIIndex == 3
    })

    #expect(candidate.nISI == 2)
    #expect(candidate.finalLabel == .possibleBurst)
    #expect(candidate.refractorySuspectCount == 2)
    #expect(candidate.refractorySuspectAction == .demoteToPossible)
    #expect(candidate.anchorLockLevel == .strongCandidate)
}

@Test
func classicAnchorKeepsLongerRefractorySuspectBurstCanonical() throws {
    let train = SpikeTrain(
        name: "unit_refractory",
        timestampsSec: [0, 0.100, 0.1011, 0.1022, 0.1033, 0.205]
    )
    let settings = ClassicAnchorSettings(
        minValidISISec: 0.001,
        burstBandLowerSec: 0.001,
        burstBandUpperSec: 0.010,
        refractorySuspectSec: 0.0015,
        refractoryAction: .demoteToPossible
    )

    let result = ClassicAnchorDetector.detect(train: train, settings: settings)
    let candidate = try #require(result.candidates.first {
        $0.startISIIndex == 2 && $0.endISIIndex == 4
    })

    #expect(candidate.nISI == 3)
    #expect(candidate.finalLabel == .burst)
    #expect(candidate.refractorySuspectCount == 3)
    #expect(candidate.refractorySuspectAction == .warnOnly)
    #expect(candidate.anchorLockLevel == .lockedClassic)
}

@Test
func optionalPDSTNClassicAnchorSubsetMatchesRReference() throws {
    guard let path = ProcessInfo.processInfo.environment["STPD_COMPLEX_CSV"], !path.isEmpty else {
        return
    }

    let csvURL = URL(fileURLWithPath: path)
    let csv = try String(contentsOf: csvURL, encoding: .utf8)
    let dataset = try CSVSpikeMatrixParser.parse(
        contents: csv,
        datasetName: "PD_STN",
        sourceDescription: csvURL.path,
        unit: .seconds,
        duplicatePolicy: .collapseExact
    )

    try assertAnchorCounts(
        dataset: dataset,
        trainName: "LT1D00.732F001-nw-11 (flag 1)",
        settings: ClassicAnchorSettings(
            minValidISISec: 0.001,
            burstBandLowerSec: 0.001218035,
            burstBandUpperSec: 0.035
        ),
        expectedCount: 8,
        expectedLockedCount: 4,
        expectedFirstStartISI: 16,
        expectedFirstEndISI: 18,
        expectedFirstLabel: .burst,
        expectedFirstMinContrast: 3.348,
        contrastTolerance: 0.001
    )

    try assertAnchorCounts(
        dataset: dataset,
        trainName: "RT2D03.535_nw-5 (flag 1)",
        settings: ClassicAnchorSettings(
            minValidISISec: 0.001,
            burstBandLowerSec: 0.001315,
            burstBandUpperSec: 0.015
        ),
        expectedCount: 185,
        expectedLockedCount: 102,
        expectedFirstStartISI: 12,
        expectedFirstEndISI: 17,
        expectedFirstLabel: .burst,
        expectedFirstMinContrast: 2.631,
        contrastTolerance: 0.001
    )
}

private func assertAnchorCounts(
    dataset: SpikeDataset,
    trainName: String,
    settings: ClassicAnchorSettings,
    expectedCount: Int,
    expectedLockedCount: Int,
    expectedFirstStartISI: Int,
    expectedFirstEndISI: Int,
    expectedFirstLabel: ClassicAnchorLabel,
    expectedFirstMinContrast: Double,
    contrastTolerance: Double
) throws {
    let train = try #require(dataset.trains.first { $0.name == trainName })
    let result = ClassicAnchorDetector.detect(train: train, settings: settings)
    let referenceCandidates = result.candidates.filter { candidate in
        candidate.isEligibleForAutoSelection &&
            candidate.candidateClass == "classic_anchor_short_isi_packet"
    }
    let first = try #require(referenceCandidates.first)

    #expect(referenceCandidates.count == expectedCount)
    #expect(referenceCandidates.filter { $0.anchorLockLevel == .lockedClassic }.count == expectedLockedCount)
    #expect(first.startISIIndex == expectedFirstStartISI)
    #expect(first.endISIIndex == expectedFirstEndISI)
    #expect(first.finalLabel == expectedFirstLabel)
    #expect(isClose(first.edgeContrastMinQ90, expectedFirstMinContrast, tolerance: contrastTolerance))
    #expect(result.candidates.contains { $0.finalLabel == .reject && $0.action == "reject" })
}

private func isClose(_ value: Double?, _ expected: Double, tolerance: Double = 1e-12) -> Bool {
    guard let value else {
        return false
    }
    return abs(value - expected) <= tolerance
}

private func cumulativeTimestamps(_ isi: [Double]) -> [Double] {
    var values = [0.0]
    values.reserveCapacity(isi.count + 1)
    for value in isi {
        values.append((values.last ?? 0) + value)
    }
    return values
}
