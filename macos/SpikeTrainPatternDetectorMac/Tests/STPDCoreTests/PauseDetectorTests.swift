import Foundation
import STPDCore
import Testing

@Test
func pauseDetectorDetectsClearLongISIRelativeToLocalAndGlobalBaseline() throws {
    let train = SpikeTrain(
        name: "pause_unit",
        timestampsSec: [0, 0.010, 0.020, 0.030, 0.230, 0.240, 0.250, 0.260]
    )

    let result = PauseDetector.detect(train: train)
    let candidate = try #require(result.candidates.first)

    #expect(result.candidates.count == 1)
    #expect(candidate.candidateLayer == "pause_detector")
    #expect(candidate.finalLabel == .pause)
    #expect(candidate.anchorLockLevel == .lockedClassic)
    #expect(candidate.gateStatus == "pause_strong_long_isi_pass")
    #expect(candidate.startSpikeIndex == 4)
    #expect(candidate.endSpikeIndex == 5)
    #expect(candidate.startISIIndex == 4)
    #expect(candidate.endISIIndex == 4)
    #expect(candidate.nISI == 1)
    #expect(candidate.nSpikes == 2)
    #expect(isClose(candidate.durationSec, 0.200))
    #expect(isClose(candidate.maxIntraISISec, 0.200))
    #expect((candidate.edgeContrastMinQ90 ?? 0) > 10)
    #expect((candidate.edgeContrastGeomQ90 ?? 0) > 2)
}

@Test
func pauseDetectorRejectsLongISIThatFailsGlobalMedianGuard() throws {
    let train = SpikeTrain(
        name: "pause_guard_unit",
        timestampsSec: [0, 0.100, 0.200, 0.300, 0.500, 0.600, 0.700, 0.800]
    )
    let guardedSettings = PauseDetectorSettings(
        seedThresholdSec: 0.010,
        strongThresholdSec: 0.150,
        alpha: 1.2,
        useGlobalMedianGuard: true,
        globalMedianFactor: 2.5,
        eventCoreGapEnabled: false
    )
    let relaxedSettings = PauseDetectorSettings(
        seedThresholdSec: 0.010,
        strongThresholdSec: 0.150,
        alpha: 1.2,
        useGlobalMedianGuard: false,
        globalMedianFactor: 2.5,
        eventCoreGapEnabled: false
    )

    let guarded = PauseDetector.detect(train: train, settings: guardedSettings)
    let relaxed = PauseDetector.detect(train: train, settings: relaxedSettings)

    #expect(guarded.candidates.isEmpty)
    #expect(relaxed.candidates.count == 1)
    #expect(relaxed.candidates.first?.finalLabel == .pause)
}

@Test
func pauseDetectorAddsEventCoreRelativeGapEvidenceBelowClassicGlobalGuard() throws {
    let train = SpikeTrain(
        name: "pause_event_core_unit",
        timestampsSec: [
            0,
            0.050,
            0.100,
            0.150,
            0.200,
            0.250,
            0.370,
            0.420,
            0.470,
            0.520,
            0.570,
            0.620
        ]
    )
    let classicOnlySettings = PauseDetectorSettings(
        alpha: 2.2,
        useGlobalMedianGuard: true,
        globalMedianFactor: 2.5,
        eventCoreGapEnabled: false
    )
    let eventCoreSettings = PauseDetectorSettings(
        alpha: 2.2,
        useGlobalMedianGuard: true,
        globalMedianFactor: 2.5,
        eventCoreGapEnabled: true
    )

    let classicOnly = PauseDetector.detect(train: train, settings: classicOnlySettings)
    let eventCore = PauseDetector.detect(train: train, settings: eventCoreSettings)
    let candidate = try #require(eventCore.candidates.first { $0.candidateLayer == "event_core_pause_gap" })

    #expect(classicOnly.candidates.isEmpty)
    #expect(candidate.finalLabel == .pause)
    #expect(candidate.decisionPath.hasPrefix("relative_long_isi_gap_layer"))
    #expect(candidate.startISIIndex == 6)
    #expect(candidate.endISIIndex == 6)
    #expect(isClose(candidate.maxIntraISISec, 0.120))
    #expect((candidate.edgeContrastMinQ90 ?? 0) > candidate.anchorContrastMinRequired)
    #expect((candidate.edgeContrastGeomQ90 ?? 0) > candidate.anchorContrastGeomRequired)
}

@Test
func pauseDetectorAppliesAntiTonicVetoToStablePauseBlocks() throws {
    let train = SpikeTrain(
        name: "pause_anti_tonic_unit",
        timestampsSec: [0, 0.120, 0.240, 0.360, 0.480]
    )
    let vetoSettings = PauseDetectorSettings(
        seedThresholdSec: 0.100,
        strongThresholdSec: 0.150,
        alpha: 0.8,
        useGlobalMedianGuard: false,
        eventCoreGapEnabled: false,
        antiTonicVeto: true,
        tonicLowerSec: 0.100,
        tonicUpperSec: 0.140,
        tonicLVMax: 0.50
    )
    let noVetoSettings = PauseDetectorSettings(
        seedThresholdSec: 0.100,
        strongThresholdSec: 0.150,
        alpha: 0.8,
        useGlobalMedianGuard: false,
        eventCoreGapEnabled: false,
        antiTonicVeto: false,
        tonicLowerSec: 0.100,
        tonicUpperSec: 0.140,
        tonicLVMax: 0.50
    )

    let vetoed = PauseDetector.detect(train: train, settings: vetoSettings)
    let accepted = PauseDetector.detect(train: train, settings: noVetoSettings)

    #expect(vetoed.candidates.isEmpty)
    #expect(accepted.candidates.count == 1)
    #expect(accepted.candidates.first?.finalLabel == .pause)
}

@Test
func pauseDetectorBridgesSingleBetaQualifiedGapBetweenPauseSeeds() throws {
    let train = SpikeTrain(
        name: "pause_bridge_unit",
        timestampsSec: [0, 0.010, 0.020, 0.220, 0.250, 0.450, 0.460, 0.470]
    )

    let result = PauseDetector.detect(train: train)
    let candidate = try #require(result.candidates.first)

    #expect(result.candidates.count == 1)
    #expect(candidate.finalLabel == .pause)
    #expect(candidate.startISIIndex == 3)
    #expect(candidate.endISIIndex == 5)
    #expect(candidate.nISI == 3)
    #expect(candidate.nSpikes == 4)
    #expect(isClose(candidate.durationSec, 0.430))
}

@Test
func classicAnchorPipelineIncludesPauseCandidates() throws {
    let burstTrain = SpikeTrain(
        name: "pipeline_burst_unit",
        timestampsSec: [0, 0.100, 0.106, 0.112, 0.200]
    )
    let pauseTrain = SpikeTrain(
        name: "pipeline_pause_unit",
        timestampsSec: [0, 0.010, 0.020, 0.030, 0.230, 0.240, 0.250, 0.260]
    )
    let dataset = SpikeDataset(
        name: "synthetic pause pipeline",
        sourceDescription: "unit-test",
        trains: [burstTrain, pauseTrain]
    )

    let run = ClassicAnchorDetectionPipeline.run(
        dataset: dataset,
        bandSettings: TrainAdaptiveBandSettings(minValidISISec: 0.001, histogramBinWidthSec: 0.005)
    )
    let burstResult = try #require(run.result(for: "pipeline_burst_unit"))
    let pauseResult = try #require(run.result(for: "pipeline_pause_unit"))

    #expect(run.candidates.filter { $0.selectedForAuto && $0.finalLabel == .burst }.count >= 1)
    #expect(run.pauseCount >= 1)
    #expect(burstResult.candidates.contains { $0.finalLabel == .burst })
    #expect(pauseResult.candidates.contains { $0.finalLabel == .pause })
    #expect(run.eventAnnotations(in: dataset, tracks: [.gap]).contains { $0.label == .pause })
}

@Test
func classicBurstFlankingISIsCanRemainPauseCandidates() throws {
    let train = SpikeTrain(
        name: "burst_with_pause_flanks",
        timestampsSec: [
            0,
            0.010,
            0.020,
            0.220,
            0.226,
            0.232,
            0.238,
            0.438,
            0.448,
            0.458
        ]
    )
    let dataset = SpikeDataset(
        name: "synthetic burst pause flank arbitration",
        sourceDescription: "unit-test",
        trains: [train]
    )

    let burstResult = ClassicAnchorDetector.detect(
        train: train,
        settings: ClassicAnchorSettings(
            minValidISISec: 0.001,
            burstBandLowerSec: 0.001,
            burstBandUpperSec: 0.010
        )
    )
    let pauseResult = PauseDetector.detect(train: train)
    let burstOnly = ClassicAnchorCandidateArbitrator.arbitrate(burstResult.candidates)
    let flankPauses = ClassicBurstFlankPauseDetector.detect(
        train: train,
        candidates: burstOnly,
        settings: PauseDetectorSettings()
    )
    #expect(flankPauses.count == 2)
    #expect(Set(flankPauses.map(\.startISIIndex)) == [3, 7])
    #expect(flankPauses.allSatisfy { $0.candidateLayer == "classic_burst_flank_pause" })

    let candidates = ClassicAnchorCandidateArbitrator.arbitrate(
        burstResult.candidates + pauseResult.candidates + flankPauses
    )
    let selected = candidates.filter { $0.selectedForAuto }

    let burst = try #require(selected.first {
        $0.finalLabel == .burst &&
            $0.startISIIndex == 4 &&
            $0.endISIIndex == 6
    })
    let prePause = try #require(selected.first {
        $0.finalLabel == .pause &&
            $0.startISIIndex == 3 &&
            $0.endISIIndex == 3
    })
    let postPause = try #require(selected.first {
        $0.finalLabel == .pause &&
            $0.startISIIndex == 7 &&
            $0.endISIIndex == 7
    })

    #expect(isClose(burst.preGapSec, 0.200))
    #expect(isClose(burst.postGapSec, 0.200))
    #expect(prePause.selectionStatus == "selected_by_gap_track_structural_flank_pause")
    #expect(postPause.selectionStatus == "selected_by_gap_track_structural_flank_pause")

    let run = ClassicAnchorDetectionRun(
        bandSettings: TrainAdaptiveBandSettings(),
        qualitySettings: SpikeQualitySettings(),
        resolutions: [],
        results: [
            ClassicAnchorDetectionResult(
                trainID: train.id,
                trainName: train.name,
                candidates: candidates
            )
        ]
    )
    let annotations = run.eventAnnotations(in: dataset, tracks: [.event, .gap])
    #expect(annotations.contains {
        $0.label == .burst &&
            $0.startISISecIndex == 4 &&
            $0.endISISecIndex == 6
    })
    #expect(annotations.contains {
        $0.label == .pause &&
            $0.startISISecIndex == 3 &&
            $0.endISISecIndex == 3
    })
    #expect(annotations.contains {
        $0.label == .pause &&
            $0.startISISecIndex == 7 &&
            $0.endISISecIndex == 7
    })
}

private func isClose(_ value: Double?, _ expected: Double, tolerance: Double = 1e-12) -> Bool {
    guard let value else {
        return false
    }
    return abs(value - expected) <= tolerance
}
