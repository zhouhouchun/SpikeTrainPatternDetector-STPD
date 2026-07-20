import Foundation
@testable import STPDCore
import Testing

@Test
func statePatternDetectorDetectsStableTonicRun() throws {
    let train = SpikeTrain(
        name: "tonic_unit",
        timestampsSec: cumulativeTimestamps(repeating: 0.040, count: 8)
    )

    let result = StatePatternDetector.detect(train: train)
    let tonic = try #require(result.candidates.first { $0.finalLabel == .tonic })

    #expect(tonic.candidateLayer == "event_core_tonic_state")
    #expect(tonic.startISIIndex == 1)
    #expect(tonic.endISIIndex == 8)
    #expect(tonic.startSpikeIndex == 1)
    #expect(tonic.endSpikeIndex == 9)
    #expect(tonic.nSpikes == 9)
    #expect(tonic.nISI == 8)
    #expect(isClose(tonic.anchorBandLowerSec, 0.040))
    #expect(isClose(tonic.anchorBandUpperSec, 0.040))
    #expect(tonic.anchorBandSource == .structure)
    #expect((tonic.lv ?? 1) <= 0.50)
}

@Test
func statePatternDetectorDetectsHighFrequencyTonicRun() throws {
    let train = SpikeTrain(
        name: "hf_tonic_unit",
        timestampsSec: cumulativeTimestamps(repeating: 0.015, count: 7)
    )

    let result = StatePatternDetector.detect(
        train: train,
        settings: StatePatternDetectorSettings(
            highFrequencyTonicFloorSec: 0.010,
            highFrequencyTonicUpperSec: 0.030
        )
    )
    let highFrequencyTonic = try #require(result.candidates.first { $0.finalLabel == .highFrequencyTonic })

    #expect(highFrequencyTonic.candidateLayer == "event_core_hf_tonic_state")
    #expect(highFrequencyTonic.startISIIndex == 1)
    #expect(highFrequencyTonic.endISIIndex == 7)
    #expect(highFrequencyTonic.startSpikeIndex == 1)
    #expect(highFrequencyTonic.endSpikeIndex == 8)
    #expect(highFrequencyTonic.nSpikes == 8)
    #expect(highFrequencyTonic.nISI == 7)
    #expect((highFrequencyTonic.cv ?? 1) <= 0.30)
    #expect((highFrequencyTonic.lv ?? 1) <= 0.35)
}

@Test
func statePatternDetectorDetectsHighFrequencySpikingRun() throws {
    let train = SpikeTrain(
        name: "hfs_unit",
        timestampsSec: cumulativeTimestamps(repeating: 0.010, count: 32)
    )

    let result = StatePatternDetector.detect(train: train)
    let hfs = try #require(result.candidates.first { $0.finalLabel == .highFrequencySpiking })

    #expect(hfs.candidateLayer == "event_grammar_hf_spiking_state")
    #expect(hfs.startISIIndex == 1)
    #expect(hfs.endISIIndex == 32)
    #expect(hfs.startSpikeIndex == 1)
    #expect(hfs.endSpikeIndex == 33)
    #expect(hfs.nSpikes == 33)
    #expect(hfs.nISI == 32)
    #expect((hfs.intraQ90Sec ?? 1) <= 0.025)
}

@Test
func highFrequencySpikingMergesSupportRunsAcrossModerateGapLikeREventGrammar() throws {
    let isi = Array(repeating: 0.010, count: 16) + [0.050] + Array(repeating: 0.010, count: 16)
    let train = SpikeTrain(
        name: "hfs_moderate_gap_unit",
        timestampsSec: cumulativeTimestamps(fromISI: isi)
    )

    let result = StatePatternDetector.detect(train: train)
    let hfs = try #require(result.candidates.first { $0.finalLabel == .highFrequencySpiking })

    #expect(hfs.startISIIndex == 1)
    #expect(hfs.endISIIndex == 33)
    #expect(hfs.nISI == 33)
    #expect(hfs.nSpikes == 34)
    #expect(isClose(hfs.hfSpikingToleratedGapSec, 0.075))
    #expect(isClose(hfs.hfSpikingShortFraction, 32.0 / 33.0))
}

@Test
func highFrequencySpikingKeepsClassicBoundaryCandidatesForRStyleHFProtection() throws {
    let isi = [0.200] + Array(repeating: 0.010, count: 32) + [0.200]
    let train = SpikeTrain(
        name: "classic_boundary_hfs_veto_unit",
        timestampsSec: cumulativeTimestamps(fromISI: isi)
    )

    let result = StatePatternDetector.detect(train: train)
    let hfs = try #require(result.candidates.first { $0.finalLabel == .highFrequencySpiking })

    #expect(hfs.candidateLayer == "event_grammar_hf_spiking_state")
    #expect(hfs.startISIIndex == 2)
    #expect(hfs.endISIIndex == 33)
}

@Test
func classicAnchorPipelineIncludesStateCandidatesAndAutoSelection() throws {
    let tonicTrain = SpikeTrain(
        name: "pipeline_tonic_unit",
        timestampsSec: cumulativeTimestamps(repeating: 0.040, count: 8)
    )
    let hfsTrain = SpikeTrain(
        name: "pipeline_hfs_unit",
        timestampsSec: cumulativeTimestamps(repeating: 0.010, count: 32)
    )
    let dataset = SpikeDataset(
        name: "synthetic state pipeline",
        sourceDescription: "unit-test",
        trains: [tonicTrain, hfsTrain]
    )

    let run = ClassicAnchorDetectionPipeline.run(
        dataset: dataset,
        bandSettings: TrainAdaptiveBandSettings(minValidISISec: 0.001, histogramBinWidthSec: 0.005)
    )

    #expect(run.tonicCount >= 1)
    #expect(run.highFrequencySpikingCount >= 1)
    #expect(run.selectedAutoCount >= 2)
    #expect(run.candidates.contains { $0.finalLabel == .tonic })
    #expect(run.candidates.contains { $0.finalLabel == .highFrequencySpiking })
    #expect(run.eventAnnotations(in: dataset).allSatisfy { annotation in
        run.candidates.first { $0.id == annotation.candidateID }?.selectedForAuto == true
    })
}

@Test
func classicAnchorPipelineAppliesStateTuningOverrides() throws {
    let hfsTrain = SpikeTrain(
        name: "tuned_hfs_unit",
        timestampsSec: cumulativeTimestamps(repeating: 0.010, count: 32)
    )
    let dataset = SpikeDataset(
        name: "synthetic tuned state pipeline",
        sourceDescription: "unit-test",
        trains: [hfsTrain]
    )

    let defaultRun = ClassicAnchorDetectionPipeline.run(
        dataset: dataset,
        bandSettings: TrainAdaptiveBandSettings(minValidISISec: 0.001, histogramBinWidthSec: 0.005)
    )
    let tunedRun = ClassicAnchorDetectionPipeline.run(
        dataset: dataset,
        bandSettings: TrainAdaptiveBandSettings(minValidISISec: 0.001, histogramBinWidthSec: 0.005),
        stateTuning: StatePatternDetectorTuning(highFrequencySpikingMinSpikes: 40)
    )

    #expect(defaultRun.highFrequencySpikingCount >= 1)
    #expect(tunedRun.highFrequencySpikingCount == 0)
}

@Test
func arbitratorBlocksLowerPriorityOverlappingCandidate() throws {
    let burst = testCandidate(
        id: "burst",
        label: .burst,
        startISIIndex: 2,
        endISIIndex: 6,
        priority: 1_375
    )
    let hfs = testCandidate(
        id: "hfs",
        label: .highFrequencySpiking,
        startISIIndex: 3,
        endISIIndex: 7,
        priority: 1_220
    )

    let arbitrated = ClassicAnchorCandidateArbitrator.arbitrate([hfs, burst])
    let arbitratedBurst = try #require(arbitrated.first { $0.id == "burst" })
    let arbitratedHFS = try #require(arbitrated.first { $0.id == "hfs" })

    #expect(arbitratedBurst.selectedForAuto)
    #expect(arbitratedBurst.selectionStatus == "selected_by_event_core_weighted_interval_grammar")
    #expect(!arbitratedHFS.selectedForAuto)
    #expect(arbitratedHFS.selectionStatus == "not_selected")
}

@Test
func legacyHFSProtectionSuppressesCompactBurstKernelsInsideLongHFState() throws {
    var hfs = testCandidate(
        id: "hfs",
        label: .highFrequencySpiking,
        startISIIndex: 1,
        endISIIndex: 32,
        priority: 1_220
    )
    hfs.hfSpikingShortFraction = 0.90
    hfs.hfSpikingBridgeFraction = 0.96
    hfs.hfSpikingQ90MaxSec = 0.025
    let compactBurst = testCandidate(
        id: "compact_burst",
        label: .burst,
        startISIIndex: 10,
        endISIIndex: 12,
        priority: 1_375
    )
    let possibleKernel = testCandidate(
        id: "possible_kernel",
        label: .possibleBurst,
        startISIIndex: 20,
        endISIIndex: 21,
        priority: 520
    )
    let outsideBurst = testCandidate(
        id: "outside_burst",
        label: .burst,
        startISIIndex: 40,
        endISIIndex: 42,
        priority: 1_375
    )

    let protected = HFSpikingProtection.apply(
        to: [hfs, compactBurst, possibleKernel, outsideBurst],
        mode: .legacySingleLabel
    )
    let protectedHFS = try #require(protected.first { $0.id == "hfs" })
    let protectedBurst = try #require(protected.first { $0.id == "compact_burst" })
    let protectedPossible = try #require(protected.first { $0.id == "possible_kernel" })
    let protectedOutside = try #require(protected.first { $0.id == "outside_burst" })
    let arbitrated = ClassicAnchorCandidateArbitrator.arbitrateBySemanticTrack(protected)

    #expect(protectedHFS.action == "accept")
    #expect(protectedBurst.action == "suppress_embedded_burst_for_hf_spiking_state")
    #expect(protectedBurst.gateStatus == "hf_protected_suppressed")
    #expect(protectedBurst.decisionPath.contains("compact_burst_kernel_suppressed_inside_long_hf_spiking_state"))
    #expect(protectedBurst.failureReason == "compact_burst_kernel_suppressed_inside_long_hf_spiking_state")
    #expect(protectedBurst.candidateDiagnosticClass == "rejected__compact_burst_kernel_suppressed_inside_long_hf_spiking_state")
    #expect(protectedBurst.suppressedByHFSpikingState == true)
    #expect(protectedBurst.suppressedOriginalLabel == "burst")
    #expect(protectedBurst.hfSpikingSuppressorID == "hfs")
    #expect(protectedBurst.isEligibleForAutoSelection == false)
    #expect(protectedPossible.action == "accept")
    #expect(protectedPossible.isEligibleForAutoSelection)
    #expect(protectedOutside.action == "accept")
    // The compact burst is suppressed by the legacy protection pass. The
    // generic possible-burst packet remains on the event track, but an
    // overlapping packet does not erase a non-dominated sustained HFS state.
    #expect(arbitrated.first { $0.id == "hfs" }?.selectedForAuto == true)
    #expect(
        arbitrated.first { $0.id == "hfs" }?.selectionStatus ==
            "selected_by_state_track_weighted_interval_grammar__hfs_retained_with_internal_burst_packet_overlay"
    )
    #expect(arbitrated.first { $0.id == "compact_burst" }?.selectedForAuto == false)
    #expect(arbitrated.first { $0.id == "possible_kernel" }?.selectedForAuto == true)
    #expect(arbitrated.first { $0.id == "outside_burst" }?.selectedForAuto == true)
}

@Test
func hfsProtectionRejectsBurstDominatedHFStateWhileKeepingBurstEvents() throws {
    let hfs = testCandidate(
        id: "burst_dominated_hfs",
        label: .highFrequencySpiking,
        startISIIndex: 1,
        endISIIndex: 60,
        priority: 1_220
    )
    let firstBurst = testCandidate(
        id: "first_burst",
        label: .burst,
        startISIIndex: 3,
        endISIIndex: 5,
        priority: 1_375
    )
    let secondBurst = testCandidate(
        id: "second_burst",
        label: .burst,
        startISIIndex: 10,
        endISIIndex: 12,
        priority: 1_375
    )
    let thirdBurst = testCandidate(
        id: "third_burst",
        label: .burst,
        startISIIndex: 17,
        endISIIndex: 19,
        priority: 1_375
    )
    let fourthBurst = testCandidate(
        id: "fourth_burst",
        label: .burst,
        startISIIndex: 24,
        endISIIndex: 26,
        priority: 1_375
    )
    let fifthBurst = testCandidate(
        id: "fifth_burst",
        label: .burst,
        startISIIndex: 31,
        endISIIndex: 33,
        priority: 1_375
    )
    let sixthBurst = testCandidate(
        id: "sixth_burst",
        label: .burst,
        startISIIndex: 38,
        endISIIndex: 40,
        priority: 1_375
    )

    let selectedEvents = [
        firstBurst,
        secondBurst,
        thirdBurst,
        fourthBurst,
        fifthBurst,
        sixthBurst
    ].map {
        $0.withAutoSelection(
            selectedForAuto: true,
            selectionStatus: "selected_by_event_track_weighted_interval_grammar"
        )
    }
    let protected = HFSpikingProtection.apply(
        to: [hfs] + selectedEvents,
        selectedEvents: selectedEvents,
        mode: .multiTrack
    )
    let protectedHFS = try #require(protected.first { $0.id == "burst_dominated_hfs" })
    let arbitrated = ClassicAnchorCandidateArbitrator.arbitrateBySemanticTrack(protected)

    #expect(protectedHFS.action == "reject_burst_dominated_hf_spiking_state")
    #expect(protectedHFS.gateStatus == "hf_protected_reject")
    #expect(protectedHFS.decisionPath.contains("reject_burst_dominated_hf_spiking_state"))
    #expect(protectedHFS.failureReason == "reject_burst_dominated_hf_spiking_state")
    #expect(protectedHFS.candidateDiagnosticClass == "rejected__reject_burst_dominated_hf_spiking_state")
    #expect(protectedHFS.hfSpikingEmbeddedBurstCount == 6)
    #expect(protectedHFS.hfSpikingEmbeddedBurstGroupCount == 6)
    #expect(isClose(protectedHFS.hfSpikingEmbeddedBurstCoverage, 0.30))
    #expect(protectedHFS.hfSpikingBurstDominated == true)
    #expect(protectedHFS.hfSpikingBurstPacketLike == false)
    #expect(protected.filter { $0.finalLabel == .burst }.allSatisfy { $0.action == "accept" })
    #expect(arbitrated.first { $0.id == "burst_dominated_hfs" }?.selectedForAuto == false)
    #expect(arbitrated.first { $0.id == "first_burst" }?.selectedForAuto == true)
    #expect(arbitrated.first { $0.id == "second_burst" }?.selectedForAuto == true)
    #expect(arbitrated.first { $0.id == "third_burst" }?.selectedForAuto == true)
    #expect(arbitrated.first { $0.id == "fourth_burst" }?.selectedForAuto == true)
    #expect(arbitrated.first { $0.id == "fifth_burst" }?.selectedForAuto == true)
    #expect(arbitrated.first { $0.id == "sixth_burst" }?.selectedForAuto == true)
}

@Test
func hfsProtectionCountsAdjacentEmbeddedBurstsAsSeparateRGroups() throws {
    let hfs = testCandidate(
        id: "adjacent_burst_dominated_hfs",
        label: .highFrequencySpiking,
        startISIIndex: 1,
        endISIIndex: 60,
        priority: 1_220
    )
    let bursts = [
        testCandidate(id: "adjacent_burst_1", label: .burst, startISIIndex: 1, endISIIndex: 3, priority: 1_375),
        testCandidate(id: "adjacent_burst_2", label: .burst, startISIIndex: 4, endISIIndex: 6, priority: 1_375),
        testCandidate(id: "adjacent_burst_3", label: .burst, startISIIndex: 7, endISIIndex: 9, priority: 1_375),
        testCandidate(id: "adjacent_burst_4", label: .burst, startISIIndex: 10, endISIIndex: 12, priority: 1_375),
        testCandidate(id: "adjacent_burst_5", label: .burst, startISIIndex: 13, endISIIndex: 15, priority: 1_375),
        testCandidate(id: "adjacent_burst_6", label: .burst, startISIIndex: 16, endISIIndex: 18, priority: 1_375)
    ].map {
        $0.withAutoSelection(
            selectedForAuto: true,
            selectionStatus: "selected_by_event_track_weighted_interval_grammar"
        )
    }

    let protected = HFSpikingProtection.apply(
        to: [hfs] + bursts,
        selectedEvents: bursts,
        mode: .multiTrack
    )
    let protectedHFS = try #require(protected.first { $0.id == "adjacent_burst_dominated_hfs" })

    #expect(protectedHFS.action == "accept")
    #expect(protectedHFS.hfSpikingEmbeddedBurstCount == 6)
    #expect(protectedHFS.hfSpikingEmbeddedBurstGroupCount == 1)
    #expect(isClose(protectedHFS.hfSpikingEmbeddedBurstCoverage, 0.30))
    #expect(protectedHFS.hfSpikingBurstDominated == false)
}

@Test
func legacyHFSProtectionRejectsBurstPacketLikeVariableHFState() throws {
    var hfs = testCandidate(
        id: "packet_like_hfs",
        label: .highFrequencySpiking,
        startISIIndex: 1,
        endISIIndex: 30,
        priority: 1_220
    )
    hfs.hfSpikingLargeFraction = 0.10
    let embeddedPacket = testCandidate(
        id: "embedded_packet",
        label: .longBurst,
        startISIIndex: 5,
        endISIIndex: 10,
        priority: 1_160
    )

    let protected = HFSpikingProtection.apply(
        to: [hfs, embeddedPacket],
        mode: .legacySingleLabel
    )
    let protectedHFS = try #require(protected.first { $0.id == "packet_like_hfs" })
    let protectedBurst = try #require(protected.first { $0.id == "embedded_packet" })

    #expect(protectedHFS.action == "reject_burst_packet_like_hf_spiking_state")
    #expect(protectedHFS.gateStatus == "hf_protected_reject")
    #expect(protectedHFS.failureReason == "reject_burst_packet_like_hf_spiking_state")
    #expect(protectedHFS.candidateDiagnosticClass == "rejected__reject_burst_packet_like_hf_spiking_state")
    #expect(protectedHFS.hfSpikingEmbeddedBurstCount == 1)
    #expect(protectedHFS.hfSpikingEmbeddedBurstGroupCount == 1)
    #expect(isClose(protectedHFS.hfSpikingEmbeddedBurstCoverage, 0.20))
    #expect(protectedHFS.hfSpikingBurstDominated == false)
    #expect(protectedHFS.hfSpikingBurstPacketLike == true)
    #expect(protectedBurst.action == "accept")
}

@Test
func hfsProtectionMultiTrackRetainsPacketLikeHFStateAsBurstOverlay() throws {
    var hfs = testCandidate(
        id: "packet_like_hfs",
        label: .highFrequencySpiking,
        startISIIndex: 1,
        endISIIndex: 30,
        priority: 1_220
    )
    hfs.hfSpikingLargeFraction = 0.10
    let embeddedPacket = testCandidate(
        id: "embedded_packet",
        label: .longBurst,
        startISIIndex: 5,
        endISIIndex: 10,
        priority: 1_160
    )

    let selectedEmbeddedPacket = embeddedPacket.withAutoSelection(
        selectedForAuto: true,
        selectionStatus: "selected_by_event_track_weighted_interval_grammar"
    )
    let protected = HFSpikingProtection.apply(
        to: [hfs, embeddedPacket],
        selectedEvents: [selectedEmbeddedPacket],
        mode: .multiTrack
    )
    let protectedHFS = try #require(protected.first { $0.id == "packet_like_hfs" })
    let protectedBurst = try #require(protected.first { $0.id == "embedded_packet" })
    let arbitrated = ClassicAnchorCandidateArbitrator.arbitrateBySemanticTrack(protected)

    #expect(protectedHFS.action == "accept")
    #expect(protectedHFS.gateStatus == "pass")
    #expect(protectedHFS.hfSpikingEmbeddedBurstCount == 1)
    #expect(protectedHFS.hfSpikingEmbeddedBurstGroupCount == 1)
    #expect(isClose(protectedHFS.hfSpikingEmbeddedBurstCoverage, 0.20))
    #expect(protectedHFS.hfSpikingBurstDominated == false)
    #expect(protectedHFS.hfSpikingBurstPacketLike == true)
    #expect(protectedBurst.finalLabel == .longBurst)
    #expect(protectedBurst.action == "accept")
    #expect(protectedBurst.suppressedOriginalLabel == nil)
    #expect(protectedBurst.suppressedByHFSpikingState != true)
    #expect(arbitrated.first { $0.id == "packet_like_hfs" }?.selectedForAuto == true)
    #expect(arbitrated.first { $0.id == "embedded_packet" }?.selectedForAuto == true)
    #expect(
        arbitrated.first { $0.id == "packet_like_hfs" }?.selectionStatus ==
            "selected_by_state_track_weighted_interval_grammar__hfs_retained_with_internal_burst_packet_overlay"
    )
}

@Test
func hfsProtectionAuditColumnsAreAvailableInFullCSVExport() throws {
    var hfs = testCandidate(
        id: "packet_like_hfs",
        label: .highFrequencySpiking,
        startISIIndex: 1,
        endISIIndex: 30,
        priority: 1_220
    )
    hfs.hfSpikingLargeFraction = 0.10
    let embeddedPacket = testCandidate(
        id: "embedded_packet",
        label: .longBurst,
        startISIIndex: 5,
        endISIIndex: 10,
        priority: 1_160
    )
    let selectedEmbeddedPacket = embeddedPacket.withAutoSelection(
        selectedForAuto: true,
        selectionStatus: "selected_by_event_track_weighted_interval_grammar"
    )
    let protected = HFSpikingProtection.apply(
        to: [hfs, embeddedPacket],
        selectedEvents: [selectedEmbeddedPacket],
        mode: .multiTrack
    )
    let train = SpikeTrain(
        name: "arbitration_train",
        timestampsSec: cumulativeTimestamps(repeating: 0.010, count: 32)
    )
    let dataset = SpikeDataset(
        name: "synthetic hfs audit export",
        sourceDescription: "unit-test",
        trains: [train]
    )
    let run = ClassicAnchorDetectionRun(
        bandSettings: TrainAdaptiveBandSettings(),
        qualitySettings: SpikeQualitySettings(),
        resolutions: [],
        results: [
            ClassicAnchorDetectionResult(
                trainID: train.id,
                trainName: train.name,
                candidates: ClassicAnchorCandidateArbitrator.arbitrate(protected)
            )
        ]
    )
    let exportedAt = try #require(ISO8601DateFormatter().date(from: "2026-06-17T00:00:00Z"))
    let csv = ClassicAnchorEventCSVExporter.csv(
        dataset: dataset,
        run: run,
        annotations: run.eventAnnotations(
            in: dataset,
            selectedOnly: false,
            tracks: Set(ClassicAnchorSemanticTrack.allCases)
        ),
        reviewStatuses: [:],
        includeUnselectedCandidates: true,
        exportedAt: exportedAt
    )

    #expect(csv.contains("hf_spiking_embedded_burst_count"))
    #expect(csv.contains("hf_spiking_burst_packet_like"))
    #expect(csv.contains("hfs_packet_like_but_not_dominated__retain_state_for_mutual_exclusion_arbitration"))
    #expect(csv.contains(",1,1,0.2,false,true,false,"))
}

@Test
func selectedGapDoesNotSplitHigherPriorityTonicState() throws {
    let train = SpikeTrain(
        name: "arbitration_train",
        timestampsSec: cumulativeTimestamps(repeating: 0.020, count: 14)
    )
    let tonic = testCandidate(
        id: "tonic_parent",
        label: .tonic,
        startISIIndex: 1,
        endISIIndex: 10,
        priority: 420
    )
    let selectedGap = testCandidate(
        id: "selected_pause_gap",
        label: .pause,
        startISIIndex: 5,
        endISIIndex: 5,
        priority: 910
    )
    .withAutoSelection(
        selectedForAuto: true,
        selectionStatus: "selected_by_gap_track_monotonic_completion"
    )

    let fragments = StateEventCompatibilityResolver.splitStateCandidates(
        train: train,
        candidates: [tonic, selectedGap],
        selectedEvents: [],
        selectedGaps: [selectedGap]
    )

    #expect(fragments.isEmpty)
}

@Test
func stateSplitDoesNotCutHFSAtSelectedBurstWithoutGap() throws {
    // A selected burst is an overlay inside sustained HFS. Without a selected
    // pause/gap boundary, state splitting must leave the HFS parent intact;
    // burst dominance is evaluated downstream rather than by cutting here.
    let train = SpikeTrain(
        name: "arbitration_train",
        timestampsSec: cumulativeTimestamps(repeating: 0.005, count: 70)
    )
    let settings = StatePatternDetectorSettings(
        highFrequencySpikingMinSpikes: 8,
        highFrequencySpikingMinDurationSec: 0
    )
    let hfs = testCandidate(
        id: "hfs_parent",
        label: .highFrequencySpiking,
        startISIIndex: 1,
        endISIIndex: 60,
        priority: 1_220
    )
    let selectedBurst = testCandidate(
        id: "selected_compact_burst",
        label: .burst,
        startISIIndex: 10,
        endISIIndex: 12,
        priority: 1_375
    )
    .withAutoSelection(
        selectedForAuto: true,
        selectionStatus: "selected_by_event_track_weighted_interval_grammar"
    )

    let fragments = StateEventCompatibilityResolver.splitStateCandidates(
        train: train,
        candidates: [hfs, selectedBurst],
        selectedEvents: [selectedBurst],
        selectedGaps: [],
        settings: settings
    )

    #expect(fragments.isEmpty)
}

@Test
func arbitratorUsesWeightedIntervalSelectionInsteadOfGreedyPriority() throws {
    let broad = testCandidate(
        id: "broad_hf_tonic",
        label: .highFrequencyTonic,
        startISIIndex: 1,
        endISIIndex: 6,
        priority: 560
    )
    let left = testCandidate(
        id: "left_tonic",
        label: .tonic,
        startISIIndex: 1,
        endISIIndex: 3,
        priority: 420
    )
    let right = testCandidate(
        id: "right_tonic",
        label: .tonic,
        startISIIndex: 4,
        endISIIndex: 6,
        priority: 420
    )

    let arbitrated = ClassicAnchorCandidateArbitrator.arbitrateBySemanticTrack([broad, left, right])
    let arbitratedBroad = try #require(arbitrated.first { $0.id == "broad_hf_tonic" })
    let arbitratedLeft = try #require(arbitrated.first { $0.id == "left_tonic" })
    let arbitratedRight = try #require(arbitrated.first { $0.id == "right_tonic" })

    #expect(!arbitratedBroad.selectedForAuto)
    #expect(arbitratedLeft.selectedForAuto)
    #expect(arbitratedRight.selectedForAuto)
    #expect(arbitratedLeft.selectionStatus == "selected_by_state_track_weighted_interval_grammar")
    #expect(arbitratedRight.selectionStatus == "selected_by_state_track_weighted_interval_grammar")
}

@Test
func eventAnnotationsDefaultToAutoSelectedEventCandidates() throws {
    let broad = testCandidate(
        id: "broad_hf_tonic",
        label: .highFrequencyTonic,
        startISIIndex: 1,
        endISIIndex: 6,
        priority: 560
    )
    let left = testCandidate(
        id: "left_tonic",
        label: .tonic,
        startISIIndex: 1,
        endISIIndex: 3,
        priority: 420
    )
    let right = testCandidate(
        id: "right_tonic",
        label: .tonic,
        startISIIndex: 4,
        endISIIndex: 6,
        priority: 420
    )
    let train = SpikeTrain(
        name: "arbitration_train",
        timestampsSec: cumulativeTimestamps(repeating: 0.010, count: 8)
    )
    let dataset = SpikeDataset(
        name: "synthetic arbitration",
        sourceDescription: "unit-test",
        trains: [train]
    )
    let candidates = ClassicAnchorCandidateArbitrator.arbitrateBySemanticTrack([broad, left, right])
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

    let selectedAnnotations = run.eventAnnotations(in: dataset)
    let selectedStateAnnotations = run.eventAnnotations(in: dataset, tracks: [.state])
    let auditAnnotations = run.eventAnnotations(
        in: dataset,
        selectedOnly: false,
        tracks: Set(ClassicAnchorSemanticTrack.allCases)
    )
    let exportedAt = try #require(ISO8601DateFormatter().date(from: "2026-06-17T00:00:00Z"))
    let selectedCSV = ClassicAnchorEventCSVExporter.csv(
        dataset: dataset,
        run: run,
        annotations: selectedAnnotations,
        reviewStatuses: [:],
        exportedAt: exportedAt
    )
    let auditCSV = ClassicAnchorEventCSVExporter.csv(
        dataset: dataset,
        run: run,
        annotations: auditAnnotations,
        reviewStatuses: [:],
        includeUnselectedCandidates: true,
        exportedAt: exportedAt
    )

    #expect(selectedAnnotations.isEmpty)
    #expect(selectedStateAnnotations.map(\.candidateID).sorted() == ["left_tonic", "right_tonic"])
    #expect(auditAnnotations.map(\.candidateID).sorted() == ["broad_hf_tonic", "left_tonic", "right_tonic"])
    #expect(!selectedCSV.contains("broad_hf_tonic"))
    #expect(!selectedCSV.contains("left_tonic"))
    #expect(!selectedCSV.contains("right_tonic"))
    #expect(auditCSV.contains("broad_hf_tonic"))
}

@Test
func eventAnnotationTimeBoundsFollowISISpanEvenWhenSpikeSpanDiffers() throws {
    let train = SpikeTrain(
        name: "arbitration_train",
        timestampsSec: [0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
    )
    let dataset = SpikeDataset(
        name: "synthetic annotation",
        sourceDescription: "unit-test",
        trains: [train]
    )
    let candidate = testCandidate(
        id: "state_with_wide_spike_span",
        label: .tonic,
        startISIIndex: 3,
        endISIIndex: 4,
        startSpikeIndex: 1,
        endSpikeIndex: 6,
        priority: 420
    )
    let run = ClassicAnchorDetectionRun(
        bandSettings: TrainAdaptiveBandSettings(),
        qualitySettings: SpikeQualitySettings(),
        resolutions: [],
        results: [
            ClassicAnchorDetectionResult(
                trainID: train.id,
                trainName: train.name,
                candidates: [candidate]
            )
        ]
    )

    let annotation = try #require(
        run.eventAnnotations(
            in: dataset,
            selectedOnly: false,
            tracks: Set(ClassicAnchorSemanticTrack.allCases)
        ).first
    )
    let coveredISI = try #require(annotation.coveredISIIndices(in: train))

    #expect(Array(coveredISI) == [3, 4])
    #expect(annotation.rawStartSec == train.timestampsSec[2])
    #expect(annotation.rawEndSec == train.timestampsSec[4])
    #expect(annotation.alignedStartSec == train.alignedTimestampsSec[2])
    #expect(annotation.alignedEndSec == train.alignedTimestampsSec[4])
}

private func cumulativeTimestamps(repeating isi: Double, count: Int) -> [Double] {
    var values = [0.0]
    values.reserveCapacity(count + 1)
    for _ in 0..<count {
        values.append((values.last ?? 0) + isi)
    }
    return values
}

private func cumulativeTimestamps(fromISI isiValues: [Double]) -> [Double] {
    var values = [0.0]
    values.reserveCapacity(isiValues.count + 1)
    for isi in isiValues {
        values.append((values.last ?? 0) + isi)
    }
    return values
}

private func testCandidate(
    id: String,
    label: ClassicAnchorLabel,
    startISIIndex: Int,
    endISIIndex: Int,
    startSpikeIndex: Int? = nil,
    endSpikeIndex: Int? = nil,
    priority: Int
) -> ClassicAnchorCandidate {
    ClassicAnchorCandidate(
        id: id,
        trainID: "arbitration_train",
        trainName: "arbitration_train",
        candidateLayer: "unit_test",
        candidateClass: "unit_test",
        finalLabel: label,
        gateStatus: "pass",
        decisionPath: "unit_test",
        action: "accept",
        score: 1,
        priority: priority,
        selectedForAuto: false,
        selectionStatus: "not_selected",
        startISIIndex: startISIIndex,
        endISIIndex: endISIIndex,
        startSpikeIndex: startSpikeIndex ?? startISIIndex,
        endSpikeIndex: endSpikeIndex ?? endISIIndex + 1,
        nISI: endISIIndex - startISIIndex + 1,
        nValidISI: endISIIndex - startISIIndex + 1,
        nSpikes: endISIIndex - startISIIndex + 2,
        durationSec: nil,
        intraQ10Sec: nil,
        intraQ40Sec: nil,
        intraQ50Sec: nil,
        intraQ90Sec: nil,
        intraQ95Sec: nil,
        maxIntraISISec: nil,
        meanIntraISISec: nil,
        cv: nil,
        lv: nil,
        preGapSec: nil,
        postGapSec: nil,
        preRatioQ90: nil,
        postRatioQ90: nil,
        edgeContrastMinQ90: nil,
        edgeContrastGeomQ90: nil,
        anchorFamily: "unit_test",
        anchorLockLevel: .strongCandidate,
        anchorBandLowerSec: 0.001,
        anchorBandUpperSec: 0.010,
        anchorBandSource: .structure,
        anchorContrastMinRequired: 1,
        anchorContrastGeomRequired: 1,
        refractorySuspectCount: 0,
        refractorySuspectAction: nil
    )
}

private func isClose(_ value: Double?, _ expected: Double, tolerance: Double = 1e-12) -> Bool {
    guard let value else {
        return false
    }
    return abs(value - expected) <= tolerance
}
