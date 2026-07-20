import XCTest
@testable import STPDCore

final class Phase1BTests: XCTestCase {
    func testMultiTrackHFSRetainedAsOverlayWhenBurstOverlaps() {
        var hfs = makeCandidate(
            id: "hfs",
            label: .highFrequencySpiking,
            start: 1,
            end: 100,
            priority: 1_040,
            cv: 0.30,
            selected: true
        )
        hfs.hfSpikingBurstPacketLike = false
        let burst = makeCandidate(
            id: "burst",
            label: .burst,
            start: 20,
            end: 24,
            priority: 1_500,
            selected: true
        )

        let protected = HFSpikingProtection.apply(
            to: [hfs, burst],
            selectedEvents: [burst],
            mode: .multiTrack
        )
        let byID = Dictionary(uniqueKeysWithValues: protected.map { ($0.id, $0) })
        let arbitrated = ClassicAnchorCandidateArbitrator.arbitrateBySemanticTrack(protected)
        let finalByID = Dictionary(uniqueKeysWithValues: arbitrated.map { ($0.id, $0) })

        XCTAssertEqual(byID["hfs"]?.finalLabel, .highFrequencySpiking)
        XCTAssertEqual(byID["burst"]?.finalLabel, .burst)
        XCTAssertEqual(byID["burst"]?.action, "accept")
        XCTAssertNil(byID["burst"]?.suppressedOriginalLabel)
        XCTAssertNotEqual(byID["burst"]?.suppressedByHFSpikingState, true)
        XCTAssertEqual(byID["hfs"]?.hfSpikingBurstDominated, false)
        XCTAssertEqual(finalByID["hfs"]?.selectedForAuto, true)
        XCTAssertEqual(finalByID["burst"]?.selectedForAuto, true)
        XCTAssertEqual(
            finalByID["hfs"]?.selectionStatus,
            "selected_by_state_track_weighted_interval_grammar__hfs_retained_with_internal_burst_packet_overlay"
        )
    }

    func testMultiTrackRejectsOnlyEventDerivedBurstDominatedHFS() {
        var hfs = makeCandidate(
            id: "hfs",
            label: .highFrequencySpiking,
            start: 1,
            end: 100,
            priority: 1_040,
            cv: 0.80,
            selected: true
        )
        hfs.hfSpikingLargeFraction = 0.10
        let bursts = (0..<6).map { group in
            makeCandidate(
                id: "burst-\(group)",
                label: .burst,
                start: 5 + group * 15,
                end: 9 + group * 15,
                priority: 1_500,
                selected: true
            )
        }

        let protected = HFSpikingProtection.apply(
            to: [hfs] + bursts,
            selectedEvents: bursts,
            mode: .multiTrack
        )
        let protectedHFS = protected.first { $0.id == "hfs" }

        XCTAssertEqual(protectedHFS?.finalLabel, .reject)
        XCTAssertEqual(
            protectedHFS?.suppressedOriginalLabel,
            ClassicAnchorLabel.highFrequencySpiking.rawValue
        )
        XCTAssertEqual(protectedHFS?.hfSpikingBurstDominated, true)
        XCTAssertEqual(protectedHFS?.hfSpikingEmbeddedBurstGroupCount, 6)
        XCTAssertEqual(
            protectedHFS?.hfSpikingEmbeddedBurstCoverage ?? 0,
            0.30,
            accuracy: 1e-12
        )
    }

    func testPacketLikeButNotDominatedHFSIsRetainedForReview() {
        var hfs = makeCandidate(
            id: "hfs",
            label: .highFrequencySpiking,
            start: 1,
            end: 100,
            priority: 1_040,
            cv: 0.80,
            selected: true
        )
        hfs.hfSpikingLargeFraction = 0.10
        let burst = makeCandidate(
            id: "burst",
            label: .burst,
            start: 20,
            end: 39,
            priority: 1_500,
            selected: true
        )

        let protected = HFSpikingProtection.apply(
            to: [hfs, burst],
            selectedEvents: [burst],
            mode: .multiTrack
        )
        let protectedHFS = protected.first { $0.id == "hfs" }

        XCTAssertEqual(protectedHFS?.finalLabel, .highFrequencySpiking)
        XCTAssertEqual(protectedHFS?.hfSpikingBurstDominated, false)
        XCTAssertEqual(protectedHFS?.hfSpikingBurstPacketLike, true)
        XCTAssertTrue(
            protectedHFS?.decisionPath.contains("retain_state_for_mutual_exclusion_arbitration") == true
        )
        let protectedBurst = protected.first { $0.id == "burst" }
        XCTAssertEqual(protectedBurst?.finalLabel, .burst)
        XCTAssertEqual(protectedBurst?.action, "accept")
        XCTAssertNotEqual(protectedBurst?.suppressedByHFSpikingState, true)
        let final = ClassicAnchorCandidateArbitrator.arbitrateBySemanticTrack(protected)
        XCTAssertEqual(final.first { $0.id == "hfs" }?.selectedForAuto, true)
        XCTAssertEqual(
            final.first { $0.id == "hfs" }?.selectionStatus,
            "selected_by_state_track_weighted_interval_grammar__hfs_retained_with_internal_burst_packet_overlay"
        )
        XCTAssertEqual(final.first { $0.id == "burst" }?.selectedForAuto, true)
    }

    func testMultiTrackFallbackIgnoresRawUnselectedBurstProposals() {
        var hfs = makeCandidate(
            id: "hfs",
            label: .highFrequencySpiking,
            start: 1,
            end: 100,
            priority: 1_040,
            cv: 0.80,
            selected: true
        )
        hfs.hfSpikingLargeFraction = 0.10
        let rawBursts = (0..<6).map { group in
            makeCandidate(
                id: "raw-burst-\(group)",
                label: .burst,
                start: 5 + group * 15,
                end: 9 + group * 15,
                priority: 1_500,
                selected: false
            )
        }

        let protected = HFSpikingProtection.apply(
            to: [hfs] + rawBursts,
            mode: .multiTrack
        )
        let protectedHFS = protected.first { $0.id == "hfs" }

        XCTAssertEqual(protectedHFS?.finalLabel, .highFrequencySpiking)
        XCTAssertEqual(protectedHFS?.hfSpikingBurstDominated, false)
        XCTAssertEqual(protectedHFS?.hfSpikingEmbeddedBurstGroupCount, 0)
        XCTAssertEqual(protectedHFS?.hfSpikingEmbeddedBurstCoverage, 0)
    }

    func testHFSIsSplitBySelectedGapButNotByInternalBurst() {
        let train = makeTrain(
            name: "train-1",
            intervals: Array(repeating: 0.01, count: 29) +
                [0.20] +
                Array(repeating: 0.01, count: 30)
        )
        let hfs = makeCandidate(
            id: "hfs-parent",
            label: .highFrequencySpiking,
            start: 1,
            end: 60,
            priority: 1_040,
            cv: 0.20
        )
        let pause = makeCandidate(
            id: "pause",
            label: .pause,
            start: 30,
            end: 30,
            priority: 900,
            selected: true,
            q50: 0.20
        )
        let burst = makeCandidate(
            id: "burst",
            label: .burst,
            start: 10,
            end: 13,
            priority: 1_500,
            selected: true
        )
        let settings = StatePatternDetectorSettings(
            highFrequencySpikingMinSpikes: 10,
            highFrequencySpikingMinDurationSec: 0,
            highFrequencySpikingInternalPacketizationPolicy: .multiTrackEventOverlay
        )

        let fragments = StateEventCompatibilityResolver.splitStateCandidates(
            train: train,
            candidates: [hfs, pause, burst],
            selectedEvents: [burst],
            selectedGaps: [pause],
            settings: settings
        )

        // Pause/gap boundaries split sustained HFS; an internal burst remains an
        // event overlay and is handled by downstream dominance arbitration.
        XCTAssertEqual(fragments.map(\.startISIIndex), [1, 31])
        XCTAssertEqual(fragments.map(\.endISIIndex), [29, 60])
        XCTAssertTrue(fragments.allSatisfy { $0.finalLabel == .highFrequencySpiking })
    }

    func testTonicIsSplitBySelectedCanonicalEvent() {
        let train = makeTrain(
            name: "train-1",
            intervals: Array(repeating: 0.03, count: 30)
        )
        let tonic = makeCandidate(
            id: "tonic-parent",
            label: .tonic,
            start: 1,
            end: 30,
            priority: 760,
            q50: 0.03
        )
        let burst = makeCandidate(
            id: "burst",
            label: .burst,
            start: 10,
            end: 12,
            priority: 1_500,
            selected: true,
            q50: 0.005
        )

        let fragments = StateEventCompatibilityResolver.splitStateCandidates(
            train: train,
            candidates: [tonic, burst],
            selectedEvents: [burst],
            selectedGaps: [],
            settings: StatePatternDetectorSettings(tonicMinSpikes: 5)
        )

        XCTAssertEqual(fragments.map(\.startISIIndex), [1, 13])
        XCTAssertEqual(fragments.map(\.endISIIndex), [9, 30])
        XCTAssertTrue(fragments.allSatisfy { $0.finalLabel == .tonic })
    }

    func testPossibleBurstDoesNotSplitOrDeleteState() {
        let hfs = makeCandidate(
            id: "hfs",
            label: .highFrequencySpiking,
            start: 1,
            end: 40,
            priority: 1_040
        )
        let possible = makeCandidate(
            id: "possible",
            label: .possibleBurst,
            start: 10,
            end: 14,
            priority: 500
        ).withDiagnosticOverride(
            gateStatus: "review",
            decisionPath: "structural_seed_bridge_expansion_possible_review",
            action: "review",
            selectedForAuto: false,
            selectionStatus: "not_selected"
        )

        let resolved = ClassicAnchorCandidateArbitrator.arbitrateBySemanticTrack([hfs, possible])
        XCTAssertEqual(resolved.first { $0.id == "hfs" }?.selectedForAuto, true)
        XCTAssertEqual(resolved.first { $0.id == "possible" }?.selectedForAuto, true)
    }

    func testStateSplitIsIdempotent() {
        let train = makeTrain(
            name: "train-1",
            intervals: Array(repeating: 0.01, count: 29) +
                [0.20] +
                Array(repeating: 0.01, count: 30)
        )
        let hfs = makeCandidate(
            id: "hfs-parent",
            label: .highFrequencySpiking,
            start: 1,
            end: 60,
            priority: 1_040
        )
        let pause = makeCandidate(
            id: "pause",
            label: .pause,
            start: 30,
            end: 30,
            selected: true,
            q50: 0.20
        )
        let settings = StatePatternDetectorSettings(highFrequencySpikingMinSpikes: 10)

        let first = StateEventCompatibilityResolver.splitStateCandidates(
            train: train,
            candidates: [hfs, pause],
            selectedEvents: [],
            selectedGaps: [pause],
            settings: settings
        )
        let second = StateEventCompatibilityResolver.splitStateCandidates(
            train: train,
            candidates: [hfs, pause] + first,
            selectedEvents: [],
            selectedGaps: [pause],
            settings: settings
        )

        XCTAssertEqual(first.map(\.id), second.map(\.id))
        XCTAssertEqual(first.map(\.startISIIndex), second.map(\.startISIIndex))
        XCTAssertEqual(first.map(\.endISIIndex), second.map(\.endISIIndex))
    }

    func testArbitratorSelectsGapBoundedHFSFragmentsInsteadOfParent() {
        let train = makeTrain(
            name: "train-1",
            intervals: Array(repeating: 0.01, count: 29) +
                [0.20] +
                Array(repeating: 0.01, count: 30)
        )
        let hfs = makeCandidate(
            id: "hfs-parent",
            label: .highFrequencySpiking,
            start: 1,
            end: 60,
            priority: 1_040
        )
        let pause = makeCandidate(
            id: "pause",
            label: .pause,
            start: 30,
            end: 30,
            selected: true,
            q50: 0.20
        )
        let settings = StatePatternDetectorSettings(highFrequencySpikingMinSpikes: 10)
        let fragments = StateEventCompatibilityResolver.splitStateCandidates(
            train: train,
            candidates: [hfs, pause],
            selectedEvents: [],
            selectedGaps: [pause],
            settings: settings
        )

        let result = ClassicAnchorCandidateArbitrator.arbitrateBySemanticTrack(
            [hfs, pause] + fragments
        )
        let selectedHFS = result.filter {
            $0.finalLabel == .highFrequencySpiking && $0.selectedForAuto
        }
        let parent = result.first { $0.id == "hfs-parent" }

        XCTAssertEqual(parent?.selectedForAuto, false)
        XCTAssertEqual(parent?.selectionStatus, "not_selected__state_track_weighted_interval_grammar")
        XCTAssertEqual(selectedHFS.map(\.startISIIndex), [1, 31])
        XCTAssertEqual(selectedHFS.map(\.endISIIndex), [29, 60])
        XCTAssertEqual(result.first { $0.id == "pause" }?.selectedForAuto, true)
    }

    func testPauseCompletionIsNotBlockedBySelectedState() {
        let train = makeTrain(
            name: "train-1",
            intervals: [0.02, 0.10, 0.02, 0.02, 0.15, 0.02]
        )
        let establishedPause = makeCandidate(
            id: "pause-established",
            label: .pause,
            start: 2,
            end: 2,
            selected: true,
            q50: 0.10
        )
        let tonic = makeCandidate(
            id: "tonic",
            label: .tonic,
            start: 1,
            end: 6,
            selected: true
        )
        let completions = PauseMonotonicCompletionDetector.detect(
            train: train,
            candidates: [establishedPause, tonic],
            settings: PauseDetectorSettings(
                minValidISISec: 0.001,
                strongThresholdSec: 0.12
            )
        )

        XCTAssertTrue(completions.contains { $0.startISIIndex == 5 })
        XCTAssertEqual(
            completions.first { $0.startISIIndex == 5 }?.id,
            "train-1-pause-monotonic-floor-5"
        )
    }

    func testPauseCompletionIsBlockedBySelectedBurstFamilyEventCore() {
        let train = makeTrain(
            name: "train-1",
            intervals: [0.02, 0.10, 0.02, 0.02, 0.15, 0.02]
        )
        let establishedPause = makeCandidate(
            id: "pause-established",
            label: .pause,
            start: 2,
            end: 2,
            selected: true,
            q50: 0.10
        )
        let burst = makeCandidate(
            id: "burst",
            label: .burst,
            start: 5,
            end: 5,
            selected: true
        )
        let completions = PauseMonotonicCompletionDetector.detect(
            train: train,
            candidates: [establishedPause, burst],
            settings: PauseDetectorSettings(
                minValidISISec: 0.001,
                strongThresholdSec: 0.12
            )
        )

        XCTAssertFalse(completions.contains { $0.startISIIndex == 5 })

        let burstII = makeCandidate(
            id: "burst-ii",
            label: .possibleBurst,
            start: 5,
            end: 5,
            selected: true
        )
        let burstIICompletions = PauseMonotonicCompletionDetector.detect(
            train: train,
            candidates: [establishedPause, burstII],
            settings: PauseDetectorSettings(
                minValidISISec: 0.001,
                strongThresholdSec: 0.12
            )
        )

        XCTAssertFalse(burstIICompletions.contains { $0.startISIIndex == 5 })
    }

    func testPauseCompletionHonorsExternalQCOrManualBlock() {
        let train = makeTrain(
            name: "train-1",
            intervals: [0.02, 0.10, 0.02, 0.02, 0.15, 0.02]
        )
        let establishedPause = makeCandidate(
            id: "pause-established",
            label: .pause,
            start: 2,
            end: 2,
            selected: true,
            q50: 0.10
        )
        let completions = PauseMonotonicCompletionDetector.detect(
            train: train,
            candidates: [establishedPause],
            settings: PauseDetectorSettings(
                minValidISISec: 0.001,
                strongThresholdSec: 0.12
            ),
            additionalBlockedISIIndices: [5]
        )

        XCTAssertFalse(completions.contains { $0.startISIIndex == 5 })
    }

    func testPauseCompletionDoesNotDuplicateExistingFormalPauseEvidence() {
        let train = makeTrain(
            name: "train-1",
            intervals: [0.02, 0.10, 0.02, 0.02, 0.15, 0.02]
        )
        let establishedPause = makeCandidate(
            id: "pause-established",
            label: .pause,
            start: 2,
            end: 2,
            selected: true,
            q50: 0.10
        )
        let existingPause = makeCandidate(
            id: "pause-existing",
            label: .pause,
            start: 5,
            end: 5,
            selected: false,
            q50: 0.15
        )
        let completions = PauseMonotonicCompletionDetector.detect(
            train: train,
            candidates: [establishedPause, existingPause],
            settings: PauseDetectorSettings(
                minValidISISec: 0.001,
                strongThresholdSec: 0.12
            )
        )

        XCTAssertFalse(completions.contains { $0.startISIIndex == 5 })
    }

    func testPhase1BResolverCompletesPauseBeforeStateSplitting() {
        var intervals = Array(repeating: 0.01, count: 60)
        intervals[19] = 0.10
        intervals[29] = 0.15
        let train = makeTrain(name: "train-1", intervals: intervals)
        let hfs = makeCandidate(
            id: "hfs-parent",
            label: .highFrequencySpiking,
            start: 1,
            end: 60,
            priority: 1_040
        )
        let establishedPause = makeCandidate(
            id: "pause-established",
            label: .pause,
            start: 20,
            end: 20,
            priority: 900,
            q50: 0.10
        )

        let resolved = MultiTrackPhase1BResolver.resolve(
            train: train,
            candidates: [hfs, establishedPause],
            pauseSettings: PauseDetectorSettings(
                minValidISISec: 0.001,
                strongThresholdSec: 0.12
            ),
            stateSettings: StatePatternDetectorSettings(
                highFrequencySpikingMinSpikes: 8,
                highFrequencySpikingMinDurationSec: 0
            ),
            stagePrefix: "test"
        )

        let selectedGaps = resolved.filter {
            $0.selectedForAuto && $0.finalLabel == .pause
        }
        let selectedHFS = resolved.filter {
            $0.selectedForAuto && $0.finalLabel == .highFrequencySpiking
        }
        XCTAssertEqual(selectedGaps.map(\.startISIIndex), [20, 30])
        XCTAssertEqual(selectedHFS.map(\.startISIIndex), [1, 21, 31])
        XCTAssertEqual(selectedHFS.map(\.endISIIndex), [19, 29, 60])
        XCTAssertFalse(
            selectedHFS.contains { $0.startISIIndex <= 30 && $0.endISIIndex >= 30 }
        )
        let parent = resolved.first { $0.id == "hfs-parent" }
        XCTAssertEqual(
            parent?.selectionStatus,
            "not_selected__state_parent_consumed_by_hard_boundary"
        )
    }

    func testGapSplitPrecedesHFSPacketDominanceEvaluation() {
        var intervals = Array(repeating: 0.01, count: 120)
        intervals[59] = 0.20
        let train = makeTrain(name: "train-1", intervals: intervals)
        let hfs = makeCandidate(
            id: "hfs-parent",
            label: .highFrequencySpiking,
            start: 1,
            end: 120,
            priority: 1_040,
            cv: 0.80
        )
        let pause = makeCandidate(
            id: "pause",
            label: .pause,
            start: 60,
            end: 60,
            priority: 900,
            q50: 0.20
        )
        // Seven disjoint burst groups make the left HFS child burst-dominated.
        // The selected pause splits the parent first, so dominance is evaluated
        // per gap-bounded child rather than against the unsplit envelope.
        let burstRanges = [
            2...6, 10...14, 18...22, 26...30,
            34...38, 42...46, 50...54
        ]
        let bursts = burstRanges.enumerated().map { index, range in
            makeCandidate(
                id: "burst-\(index)",
                label: .burst,
                start: range.lowerBound,
                end: range.upperBound,
                priority: 1_500
            )
        }

        let resolved = MultiTrackPhase1BResolver.resolve(
            train: train,
            candidates: [hfs, pause] + bursts,
            pauseSettings: PauseDetectorSettings(
                minValidISISec: 0.001,
                strongThresholdSec: 0.12
            ),
            stateSettings: StatePatternDetectorSettings(
                highFrequencySpikingMinSpikes: 20,
                highFrequencySpikingMinDurationSec: 0,
                highFrequencySpikingInternalPacketizationPolicy: .multiTrackEventOverlay
            ),
            stagePrefix: "test-gap-before-packetization"
        )

        let selectedHFS = resolved.filter {
            $0.selectedForAuto && $0.finalLabel == .highFrequencySpiking
        }
        XCTAssertFalse(selectedHFS.contains { $0.startISIIndex <= 60 && $0.endISIIndex >= 60 })

        let parent = resolved.first { $0.id == "hfs-parent" }
        XCTAssertEqual(parent?.finalLabel, .highFrequencySpiking)
        XCTAssertEqual(
            parent?.selectionStatus,
            "not_selected__state_parent_consumed_by_hard_boundary"
        )
        XCTAssertTrue(
            resolved.contains {
                $0.finalLabel == .reject &&
                    $0.startISIIndex == 1 &&
                    $0.endISIIndex == 59 &&
                    $0.hfSpikingBurstDominated == true
            }
        )
        XCTAssertTrue(
            resolved.contains {
                $0.selectedForAuto &&
                    $0.finalLabel == .highFrequencySpiking &&
                    $0.startISIIndex == 61 &&
                    $0.endISIIndex == 120
            }
        )
    }

    func testStateDetectorDefersInternalPacketizationInMultiTrackMode() {
        var intervals: [Double] = []
        for _ in 0..<35 {
            intervals.append(contentsOf: [0.005, 0.005, 0.020])
        }
        let train = makeTrain(name: "packetized", intervals: intervals)
        let multiTrack = StatePatternDetector.detect(
            train: train,
            settings: StatePatternDetectorSettings(
                highFrequencySpikingMinSpikes: 30,
                highFrequencySpikingInternalPacketizationPolicy: .multiTrackEventOverlay
            )
        ).candidates
        let legacy = StatePatternDetector.detect(
            train: train,
            settings: StatePatternDetectorSettings(
                highFrequencySpikingMinSpikes: 30,
                highFrequencySpikingInternalPacketizationPolicy: .legacyRejectPacketLike
            )
        ).candidates

        let multiHFS = multiTrack.first { $0.finalLabel == .highFrequencySpiking }
        XCTAssertNotNil(multiHFS)
        XCTAssertEqual(multiHFS?.hfSpikingBurstPacketLike, true)
        XCTAssertEqual(multiHFS?.hfSpikingBurstDominated, false)
        XCTAssertNil(legacy.first { $0.finalLabel == .highFrequencySpiking })
        XCTAssertNotNil(
            legacy.first {
                $0.finalLabel == .reject &&
                    $0.decisionPath.contains("reject_hfs_internal_burst_packetization")
            }
        )
    }

    private func makeTrain(name: String, intervals: [Double]) -> SpikeTrain {
        var timestamps = [0.0]
        timestamps.reserveCapacity(intervals.count + 1)
        for interval in intervals {
            timestamps.append((timestamps.last ?? 0) + interval)
        }
        return SpikeTrain(name: name, timestampsSec: timestamps)
    }

    private func makeCandidate(
        id: String,
        label: ClassicAnchorLabel,
        start: Int,
        end: Int,
        layer: String = "test_candidate",
        decisionPath: String = "test",
        action: String = "accept",
        priority: Int = 100,
        cv: Double = 0.10,
        selected: Bool = false,
        q50: Double = 0.01
    ) -> ClassicAnchorCandidate {
        ClassicAnchorCandidate(
            id: id,
            trainID: "train-1",
            trainName: "train-1",
            candidateLayer: layer,
            candidateClass: label.rawValue,
            finalLabel: label,
            gateStatus: "pass",
            decisionPath: decisionPath,
            action: action,
            score: 1,
            priority: priority,
            selectedForAuto: selected,
            selectionStatus: selected ? "selected_for_test" : "not_selected",
            startISIIndex: start,
            endISIIndex: end,
            startSpikeIndex: start,
            endSpikeIndex: end + 1,
            nISI: max(1, end - start + 1),
            nValidISI: max(1, end - start + 1),
            nSpikes: max(2, end - start + 2),
            durationSec: Double(max(1, end - start + 1)) * q50,
            intraQ10Sec: q50,
            intraQ40Sec: q50,
            intraQ50Sec: q50,
            intraQ90Sec: q50,
            intraQ95Sec: q50,
            maxIntraISISec: q50,
            meanIntraISISec: q50,
            cv: cv,
            lv: 0.10,
            preGapSec: nil,
            postGapSec: nil,
            preRatioQ90: nil,
            postRatioQ90: nil,
            edgeContrastMinQ90: nil,
            edgeContrastGeomQ90: nil,
            anchorFamily: label.rawValue,
            anchorLockLevel: .strongCandidate,
            anchorBandLowerSec: 0.001,
            anchorBandUpperSec: 0.20,
            anchorBandSource: .structure,
            anchorContrastMinRequired: 1,
            anchorContrastGeomRequired: 1,
            refractorySuspectCount: 0,
            refractorySuspectAction: nil
        )
    }
}
