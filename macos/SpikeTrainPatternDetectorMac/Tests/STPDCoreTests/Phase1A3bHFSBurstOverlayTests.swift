import XCTest
@testable import STPDCore

final class Phase1A3bHFSBurstOverlayTests: XCTestCase {
    func testPhase1A3bPauseSplitsHFSButInternalBurstDoesNot() {
        let train = makeTrain(
            intervals: intervals(
                count: 60,
                base: 0.01,
                replacing: [(10...13, 0.005), (30...30, 0.20)]
            )
        )
        let hfs = makeCandidate(
            id: "hfs-parent",
            label: .highFrequencySpiking,
            start: 1,
            end: 60,
            priority: 1_040,
            q50: 0.01
        )
        let burst = makeCandidate(
            id: "burst",
            label: .burst,
            start: 10,
            end: 13,
            priority: 1_500,
            selected: true,
            q50: 0.005
        )
        let pause = makeCandidate(
            id: "pause",
            label: .pause,
            start: 30,
            end: 30,
            priority: 900,
            selected: true,
            q50: 0.20,
            pauseBoundaryRole: .canonicalPauseAnchor
        )

        let fragments = StateEventCompatibilityResolver.splitStateCandidates(
            train: train,
            candidates: [hfs, burst, pause],
            selectedEvents: [burst],
            selectedGaps: [pause],
            settings: hfsSettings
        )

        XCTAssertEqual(fragments.map(\.startISIIndex), [1, 31])
        XCTAssertEqual(fragments.map(\.endISIIndex), [29, 60])
        XCTAssertTrue(fragments.allSatisfy { $0.finalLabel == .highFrequencySpiking })
        XCTAssertTrue(
            fragments.contains {
                $0.startISIIndex <= burst.startISIIndex &&
                    $0.endISIIndex >= burst.endISIIndex
            }
        )
    }

    func testPhase1A3bBriefContextualAndUnspecifiedPausesDoNotSplitHFS() {
        let train = makeTrain(
            intervals: intervals(
                count: 60,
                base: 0.01,
                replacing: [(30...30, 0.08)]
            )
        )
        let hfs = makeCandidate(
            id: "hfs-parent",
            label: .highFrequencySpiking,
            start: 1,
            end: 60,
            priority: 1_040,
            q50: 0.01
        )
        let roles: [(String, PauseBoundaryRole?)] = [
            ("brief", .briefStateInterruption),
            ("contextual", .contextualPause),
            ("unspecified", nil),
        ]

        XCTAssertEqual(train.isiSec[30] ?? .nan, 0.08, accuracy: 1e-12)
        for (name, role) in roles {
            let gap = makeCandidate(
                id: "\(name)-pause",
                label: .pause,
                start: 30,
                end: 30,
                priority: 900,
                selected: true,
                q50: 0.08,
                pauseBoundaryRole: role
            )
            XCTAssertEqual(gap.pauseBoundaryRole, role)

            let resolution = StateEventCompatibilityResolver.resolveStateCandidates(
                train: train,
                candidates: [hfs, gap],
                selectedEvents: [],
                selectedGaps: [gap],
                settings: hfsSettings
            )

            XCTAssertTrue(resolution.fragments.isEmpty, name)
            XCTAssertTrue(resolution.consumedStateCandidateIdentities.isEmpty, name)
            XCTAssertTrue(
                resolution.boundaryCandidateIDsByConsumedStateCandidateIdentity.isEmpty,
                name
            )
        }
    }

    func testPhase1A3bBurstAloneDoesNotGenerateHFSFragmentsWithoutTonicEvidence() {
        let train = makeTrain(
            intervals: intervals(
                count: 60,
                base: 0.008,
                replacing: [(20...24, 0.004)]
            )
        )
        let hfs = makeCandidate(
            id: "hfs-parent",
            label: .highFrequencySpiking,
            start: 1,
            end: 60,
            priority: 1_040,
            q50: 0.008
        )
        let burst = makeCandidate(
            id: "burst",
            label: .burst,
            start: 20,
            end: 24,
            priority: 1_500,
            selected: true,
            q50: 0.004
        )

        let fragments = StateEventCompatibilityResolver.splitStateCandidates(
            train: train,
            candidates: [hfs, burst],
            selectedEvents: [burst],
            selectedGaps: [],
            settings: hfsSettings
        )

        XCTAssertTrue(fragments.isEmpty)
    }

    func testPhase1A3bTonicStillSplitsAtSelectedBurst() {
        let train = makeTrain(
            intervals: intervals(
                count: 30,
                base: 0.03,
                replacing: [(10...13, 0.005)]
            )
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
            end: 13,
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

        XCTAssertEqual(fragments.map(\.startISIIndex), [1, 14])
        XCTAssertEqual(fragments.map(\.endISIIndex), [9, 30])
        XCTAssertTrue(fragments.allSatisfy { $0.finalLabel == .tonic })
    }

    func testPhase1A3bHighFrequencyTonicStillSplitsAtSelectedBurst() {
        let train = makeTrain(
            intervals: intervals(
                count: 30,
                base: 0.025,
                replacing: [(10...13, 0.005)]
            )
        )
        let tonic = makeCandidate(
            id: "hf-tonic-parent",
            label: .highFrequencyTonic,
            start: 1,
            end: 30,
            priority: 780,
            q50: 0.025
        )
        let burst = makeCandidate(
            id: "burst",
            label: .burst,
            start: 10,
            end: 13,
            priority: 1_500,
            selected: true,
            q50: 0.005
        )
        let settings = StatePatternDetectorSettings(
            burstSeedUpperSec: 0.01,
            highFrequencyTonicFloorSec: 0.015,
            highFrequencyTonicUpperSec: 0.04,
            highFrequencyTonicMinSpikes: 5
        )

        let fragments = StateEventCompatibilityResolver.splitStateCandidates(
            train: train,
            candidates: [tonic, burst],
            selectedEvents: [burst],
            selectedGaps: [],
            settings: settings
        )

        XCTAssertEqual(fragments.map(\.startISIIndex), [1, 14])
        XCTAssertEqual(fragments.map(\.endISIIndex), [9, 30])
        XCTAssertTrue(fragments.allSatisfy { $0.finalLabel == .highFrequencyTonic })
    }

    func testPhase1A3bUnselectedOrCrossTrainPauseCannotCutOrConsumeHFS() {
        let train = makeTrain(intervals: Array(repeating: 0.008, count: 60))
        let hfs = makeCandidate(
            id: "hfs-parent",
            label: .highFrequencySpiking,
            start: 1,
            end: 60,
            priority: 1_040,
            q50: 0.008
        )
        let rawPause = makeCandidate(
            id: "raw-pause",
            label: .pause,
            start: 10,
            end: 10,
            priority: 900,
            selected: false,
            q50: 0.20,
            pauseBoundaryRole: .canonicalPauseAnchor
        )
        let otherTrainPause = makeCandidate(
            id: "other-train-pause",
            trainID: "other-train",
            label: .pause,
            start: 30,
            end: 30,
            priority: 900,
            selected: true,
            q50: 0.20,
            pauseBoundaryRole: .canonicalPauseAnchor
        )

        let resolution = StateEventCompatibilityResolver.resolveStateCandidates(
            train: train,
            candidates: [hfs, rawPause, otherTrainPause],
            selectedEvents: [],
            selectedGaps: [rawPause, otherTrainPause],
            settings: hfsSettings
        )

        XCTAssertTrue(resolution.fragments.isEmpty)
        XCTAssertTrue(resolution.consumedStateCandidateIdentities.isEmpty)
        XCTAssertTrue(
            resolution.boundaryCandidateIDsByConsumedStateCandidateIdentity.isEmpty
        )
    }

    func testPhase1A3bPauseSplitIsIdempotent() {
        let train = makeTrain(
            intervals: intervals(
                count: 60,
                base: 0.01,
                replacing: [(30...30, 0.20)]
            )
        )
        let hfs = makeCandidate(
            id: "hfs-parent",
            label: .highFrequencySpiking,
            start: 1,
            end: 60,
            priority: 1_040,
            q50: 0.01
        )
        let pause = makeCandidate(
            id: "pause",
            label: .pause,
            start: 30,
            end: 30,
            priority: 900,
            selected: true,
            q50: 0.20,
            pauseBoundaryRole: .canonicalPauseAnchor
        )

        let first = StateEventCompatibilityResolver.splitStateCandidates(
            train: train,
            candidates: [hfs, pause],
            selectedEvents: [],
            selectedGaps: [pause],
            settings: hfsSettings
        )
        let second = StateEventCompatibilityResolver.splitStateCandidates(
            train: train,
            candidates: [hfs, pause] + first,
            selectedEvents: [],
            selectedGaps: [pause],
            settings: hfsSettings
        )

        XCTAssertEqual(first.map(\.id), second.map(\.id))
        XCTAssertEqual(first.map(\.startISIIndex), second.map(\.startISIIndex))
        XCTAssertEqual(first.map(\.endISIIndex), second.map(\.endISIIndex))
    }

    func testPhase1A3bFinalArbitrationRetainsOnlyNonDominatedHFSOverlay() {
        var retainedHFS = makeCandidate(
            id: "hfs-retained",
            label: .highFrequencySpiking,
            start: 1,
            end: 100,
            priority: 1_040,
            q50: 0.008
        )
        retainedHFS.hfSpikingBurstDominated = false
        let retainedBurst = makeCandidate(
            id: "burst-retained",
            label: .burst,
            start: 20,
            end: 24,
            priority: 1_500,
            q50: 0.004
        )
        let retained = ClassicAnchorCandidateArbitrator.arbitrateBySemanticTrack([
            retainedHFS,
            retainedBurst
        ])
        let retainedByID = Dictionary(uniqueKeysWithValues: retained.map { ($0.id, $0) })

        XCTAssertEqual(retainedByID["hfs-retained"]?.selectedForAuto, true)
        XCTAssertEqual(retainedByID["burst-retained"]?.selectedForAuto, true)
        XCTAssertEqual(
            retainedByID["hfs-retained"]?.selectionStatus,
            "selected_by_state_track_weighted_interval_grammar__hfs_retained_with_internal_burst_packet_overlay"
        )

        var dominatedHFS = makeCandidate(
            id: "hfs-dominated",
            label: .highFrequencySpiking,
            start: 1,
            end: 100,
            priority: 1_040,
            q50: 0.008
        )
        dominatedHFS.hfSpikingBurstDominated = true
        let dominatedBurst = makeCandidate(
            id: "burst-dominated",
            label: .burst,
            start: 20,
            end: 24,
            priority: 1_500,
            q50: 0.004
        )
        let dominated = ClassicAnchorCandidateArbitrator.arbitrateBySemanticTrack([
            dominatedHFS,
            dominatedBurst
        ])
        let dominatedByID = Dictionary(uniqueKeysWithValues: dominated.map { ($0.id, $0) })

        XCTAssertEqual(dominatedByID["hfs-dominated"]?.selectedForAuto, false)
        XCTAssertEqual(
            dominatedByID["hfs-dominated"]?.selectionStatus,
            "not_selected__hfs_burst_packet_dominance"
        )
        XCTAssertEqual(dominatedByID["burst-dominated"]?.selectedForAuto, true)
    }

    func testPhase1A3bPipelineRetainsSustainedHFSWithFewBurstOverlays() {
        let burstRanges = [20...23, 50...53, 80...83]
        let train = makeTrain(
            intervals: intervals(
                count: 100,
                base: 0.008,
                replacing: burstRanges.map { ($0, 0.004) }
            )
        )
        let hfs = makeCandidate(
            id: "hfs-parent",
            label: .highFrequencySpiking,
            start: 1,
            end: 100,
            priority: 1_040,
            q50: 0.008
        )
        let bursts = burstRanges.enumerated().map { index, range in
            makeCandidate(
                id: "burst-\(index)",
                label: .burst,
                start: range.lowerBound,
                end: range.upperBound,
                priority: 1_500,
                q50: 0.004
            )
        }

        let resolved = MultiTrackPhase1BResolver.resolve(
            train: train,
            candidates: [hfs] + bursts,
            pauseSettings: PauseDetectorSettings(
                minValidISISec: 0.001,
                strongThresholdSec: 0.12
            ),
            stateSettings: hfsSettings,
            stagePrefix: "phase1a3b-retained"
        )

        let selectedHFS = resolved.filter {
            $0.selectedForAuto && $0.finalLabel == .highFrequencySpiking
        }
        XCTAssertTrue(
            selectedHFS.contains {
                $0.startISIIndex == 1 && $0.endISIIndex == 100
            }
        )
        XCTAssertTrue(bursts.allSatisfy { burst in
            resolved.contains { $0.id == burst.id && $0.selectedForAuto }
        })
    }

    func testPhase1A3bPipelineRejectsBurstDominatedHFS() {
        let burstRanges = (0..<6).map { group in
            (5 + group * 15)...(9 + group * 15)
        }
        let train = makeTrain(
            intervals: intervals(
                count: 100,
                base: 0.008,
                replacing: burstRanges.map { ($0, 0.004) }
            )
        )
        let hfs = makeCandidate(
            id: "hfs-parent",
            label: .highFrequencySpiking,
            start: 1,
            end: 100,
            priority: 1_040,
            q50: 0.008
        )
        let bursts = burstRanges.enumerated().map { group, range in
            makeCandidate(
                id: "burst-\(group)",
                label: .burst,
                start: range.lowerBound,
                end: range.upperBound,
                priority: 1_500,
                q50: 0.004
            )
        }

        let resolved = MultiTrackPhase1BResolver.resolve(
            train: train,
            candidates: [hfs] + bursts,
            pauseSettings: PauseDetectorSettings(
                minValidISISec: 0.001,
                strongThresholdSec: 0.12
            ),
            stateSettings: hfsSettings,
            stagePrefix: "phase1a3b-dominated"
        )
        let parent = resolved.first { $0.id == "hfs-parent" }

        XCTAssertEqual(parent?.finalLabel, .reject)
        XCTAssertEqual(parent?.hfSpikingBurstDominated, true)
        XCTAssertFalse(
            resolved.contains {
                $0.selectedForAuto &&
                    $0.finalLabel == .highFrequencySpiking &&
                    $0.startISIIndex == 1 &&
                    $0.endISIIndex == 100
            }
        )
        XCTAssertTrue(bursts.allSatisfy { burst in
            resolved.contains { $0.id == burst.id && $0.selectedForAuto }
        })
    }

    func testPhase1A3bPipelineRejectsHFSWhenOneBurstOwnsMajorityCoverage() {
        let burstRange = 10...90
        let train = makeTrain(
            intervals: intervals(
                count: 100,
                base: 0.008,
                replacing: [(burstRange, 0.004)]
            )
        )
        let hfs = makeCandidate(
            id: "hfs-majority-parent",
            label: .highFrequencySpiking,
            start: 1,
            end: 100,
            priority: 1_040,
            q50: 0.004
        )
        let burst = makeCandidate(
            id: "majority-burst",
            label: .burst,
            start: burstRange.lowerBound,
            end: burstRange.upperBound,
            priority: 1_500,
            q50: 0.004
        )

        let resolved = MultiTrackPhase1BResolver.resolve(
            train: train,
            candidates: [hfs, burst],
            pauseSettings: PauseDetectorSettings(
                minValidISISec: 0.001,
                strongThresholdSec: 0.12
            ),
            stateSettings: hfsSettings,
            stagePrefix: "phase1a3b-majority"
        )
        let parent = resolved.first { $0.id == hfs.id }

        XCTAssertEqual(parent?.finalLabel, .reject)
        XCTAssertEqual(parent?.hfSpikingBurstDominated, true)
        XCTAssertTrue(
            parent?.hfSpikingAcceptanceRoute?.contains(
                "burst_dominance_majority_coverage"
            ) == true
        )
        XCTAssertTrue(
            parent?.decisionPath.contains(
                "hf_burst_dominance_majority_coverage_min=0.5"
            ) == true
        )
        XCTAssertTrue(
            parent?.decisionPath.contains(
                "hf_burst_dominance_majority_pass=true"
            ) == true
        )
        XCTAssertEqual(
            resolved.first { $0.id == burst.id }?.selectedForAuto,
            true
        )
    }

    func testPhase1A3bPipelinePauseConsumesHFSWhenBothChildrenAreBelowMinimum() {
        let train = makeTrain(
            intervals: intervals(
                count: 20,
                base: 0.008,
                replacing: [(10...10, 0.20)]
            )
        )
        let hfs = makeCandidate(
            id: "hfs-unsplittable-parent",
            label: .highFrequencySpiking,
            start: 1,
            end: 20,
            priority: 1_040,
            q50: 0.008
        )
        let pause = makeCandidate(
            id: "hard-pause",
            label: .pause,
            start: 10,
            end: 10,
            priority: 900,
            q50: 0.20,
            pauseBoundaryRole: .canonicalPauseAnchor
        )
        let settings = StatePatternDetectorSettings(
            highFrequencySpikingMinSpikes: 15,
            highFrequencySpikingMinDurationSec: 0,
            highFrequencySpikingInternalPacketizationPolicy: .multiTrackEventOverlay
        )

        let resolved = MultiTrackPhase1BResolver.resolve(
            train: train,
            candidates: [hfs, pause],
            pauseSettings: PauseDetectorSettings(
                minValidISISec: 0.001,
                strongThresholdSec: 0.12
            ),
            stateSettings: settings,
            stagePrefix: "phase1a3b-unsplittable"
        )
        let parent = resolved.first { $0.id == hfs.id }

        XCTAssertEqual(parent?.selectedForAuto, false)
        XCTAssertEqual(parent?.stateContinuityAuthorityFrozen, true)
        XCTAssertEqual(
            parent?.selectionStatus,
            "not_selected__state_parent_consumed_by_hard_boundary"
        )
        XCTAssertTrue(
            parent?.decisionPath.contains("state_hard_boundary_consumed=true") == true
        )
        XCTAssertTrue(
            resolved.contains {
                $0.selectedForAuto &&
                    $0.finalLabel == .pause &&
                    $0.startISIIndex <= 10 &&
                    $0.endISIIndex >= 10
            }
        )
        XCTAssertFalse(
            resolved.contains {
                $0.selectedForAuto &&
                    $0.finalLabel == .highFrequencySpiking
            }
        )
    }

    func testPhase1A3bPipelinePauseConsumesParentAndSelectsValidHFSChildren() {
        let train = makeTrain(
            intervals: intervals(
                count: 60,
                base: 0.008,
                replacing: [(30...30, 0.20)]
            )
        )
        let hfs = makeCandidate(
            id: "hfs-splittable-parent",
            label: .highFrequencySpiking,
            start: 1,
            end: 60,
            priority: 1_040,
            q50: 0.008
        )
        let pause = makeCandidate(
            id: "hard-pause",
            label: .pause,
            start: 30,
            end: 30,
            priority: 900,
            q50: 0.20,
            pauseBoundaryRole: .canonicalPauseAnchor
        )

        let resolved = MultiTrackPhase1BResolver.resolve(
            train: train,
            candidates: [hfs, pause],
            pauseSettings: PauseDetectorSettings(
                minValidISISec: 0.001,
                strongThresholdSec: 0.12
            ),
            stateSettings: hfsSettings,
            stagePrefix: "phase1a3b-splittable"
        )
        let parent = resolved.first { $0.id == hfs.id }
        let selectedHFSRanges = resolved
            .filter { $0.selectedForAuto && $0.finalLabel == .highFrequencySpiking }
            .map { $0.startISIIndex...$0.endISIIndex }
            .sorted { $0.lowerBound < $1.lowerBound }

        XCTAssertEqual(parent?.selectedForAuto, false)
        XCTAssertEqual(parent?.stateContinuityAuthorityFrozen, true)
        XCTAssertEqual(selectedHFSRanges, [1...29, 31...60])
        XCTAssertTrue(
            resolved.contains {
                $0.selectedForAuto &&
                    $0.finalLabel == .pause &&
                    $0.startISIIndex <= 30 &&
                    $0.endISIIndex >= 30
            }
        )
    }

    func testPhase1A3bPauseCompletionUsesGapAuthorityBeforeSplittingHFS() {
        let train = makeTrain(
            intervals: intervals(
                count: 60,
                base: 0.01,
                replacing: [(20...20, 0.10), (30...30, 0.15)]
            )
        )
        let hfs = makeCandidate(
            id: "hfs-completion-parent",
            label: .highFrequencySpiking,
            start: 1,
            end: 60,
            priority: 1_040,
            q50: 0.01
        )
        let establishedPause = makeCandidate(
            id: "pause-established",
            label: .pause,
            start: 20,
            end: 20,
            priority: 900,
            q50: 0.10,
            pauseBoundaryRole: .briefStateInterruption
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
                highFrequencySpikingMinDurationSec: 0,
                highFrequencySpikingInternalPacketizationPolicy: .multiTrackEventOverlay
            ),
            stagePrefix: "phase1a3b-pause-completion"
        )

        let selectedPauseIndices = resolved.filter {
            $0.selectedForAuto && $0.finalLabel == .pause
        }.reduce(into: Set<Int>()) { indices, candidate in
            let lower = min(candidate.startISIIndex, candidate.endISIIndex)
            let upper = max(candidate.startISIIndex, candidate.endISIIndex)
            indices.formUnion(lower...upper)
        }
        let selectedHFSRanges = resolved
            .filter { $0.selectedForAuto && $0.finalLabel == .highFrequencySpiking }
            .map { $0.startISIIndex...$0.endISIIndex }
            .sorted { $0.lowerBound < $1.lowerBound }
        let parent = resolved.first { $0.id == hfs.id }

        // The brief interruption remains audit-visible but cannot out-rank the HFS state
        // as a selected hard gap. Only the canonical completion at index 30 is selected.
        XCTAssertEqual(selectedPauseIndices, Set([30]))
        XCTAssertEqual(selectedHFSRanges, [1...29, 31...60])
        XCTAssertEqual(
            resolved.first { $0.id == establishedPause.id }?.pauseBoundaryRole,
            .briefStateInterruption
        )
        XCTAssertEqual(
            resolved.first {
                $0.finalLabel == .pause && $0.startISIIndex == 30
            }?.pauseBoundaryRole,
            .canonicalPauseAnchor
        )
        XCTAssertTrue(
            selectedHFSRanges.contains { $0.lowerBound <= 20 && $0.upperBound >= 20 }
        )
        XCTAssertEqual(parent?.selectedForAuto, false)
        XCTAssertEqual(
            parent?.selectionStatus,
            "not_selected__state_parent_consumed_by_hard_boundary"
        )
        XCTAssertTrue(
            parent?.decisionPath.contains("state_hard_boundary_ids=") == true
        )
    }

    func testPhase1A3bPauseSplitPrecedesBurstDominanceOnEachHFSChild() {
        let burstRanges = [
            2...6, 10...14, 18...22, 26...30,
            34...38, 42...46, 50...54
        ]
        let train = makeTrain(
            intervals: intervals(
                count: 120,
                base: 0.01,
                replacing: burstRanges.map { ($0, 0.004) } + [(60...60, 0.20)]
            )
        )
        let hfs = makeCandidate(
            id: "hfs-gap-dominance-parent",
            label: .highFrequencySpiking,
            start: 1,
            end: 120,
            priority: 1_040,
            cv: 0.80,
            q50: 0.01
        )
        let pause = makeCandidate(
            id: "hard-pause",
            label: .pause,
            start: 60,
            end: 60,
            priority: 900,
            q50: 0.20,
            pauseBoundaryRole: .canonicalPauseAnchor
        )
        let bursts = burstRanges.enumerated().map { index, range in
            makeCandidate(
                id: "burst-\(index)",
                label: .burst,
                start: range.lowerBound,
                end: range.upperBound,
                priority: 1_500,
                q50: 0.004
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
            stagePrefix: "phase1a3b-gap-before-dominance"
        )
        let parent = resolved.first { $0.id == hfs.id }
        let leftChild = resolved.first {
            $0.finalLabel == .reject &&
                $0.startISIIndex == 1 &&
                $0.endISIIndex == 59
        }
        let rightChild = resolved.first {
            $0.finalLabel == .highFrequencySpiking &&
                $0.startISIIndex == 61 &&
                $0.endISIIndex == 120
        }

        XCTAssertEqual(parent?.finalLabel, .highFrequencySpiking)
        XCTAssertEqual(parent?.selectedForAuto, false)
        XCTAssertEqual(
            parent?.selectionStatus,
            "not_selected__state_parent_consumed_by_hard_boundary"
        )
        XCTAssertEqual(leftChild?.hfSpikingBurstDominated, true)
        XCTAssertEqual(leftChild?.selectedForAuto, false)
        XCTAssertEqual(rightChild?.hfSpikingBurstDominated, false)
        XCTAssertEqual(rightChild?.selectedForAuto, true)
        XCTAssertTrue(
            resolved.contains {
                $0.selectedForAuto &&
                    $0.finalLabel == .pause &&
                    $0.startISIIndex <= 60 &&
                    $0.endISIIndex >= 60
            }
        )
    }

    func testPhase1A3bRepeatedResolveSplitsTheExistingRightSibling() {
        let train = makeTrain(
            intervals: intervals(
                count: 100,
                base: 0.008,
                replacing: [(60...60, 0.20), (80...80, 0.20)]
            )
        )
        let hfs = makeCandidate(
            id: "repeated-split-parent",
            label: .highFrequencySpiking,
            start: 1,
            end: 100,
            priority: 1_040,
            q50: 0.008
        )
        let firstPause = makeCandidate(
            id: "pause-60",
            label: .pause,
            start: 60,
            end: 60,
            priority: 900,
            q50: 0.20,
            pauseBoundaryRole: .canonicalPauseAnchor
        )
        let secondPause = makeCandidate(
            id: "pause-80",
            label: .pause,
            start: 80,
            end: 80,
            priority: 900,
            q50: 0.20,
            pauseBoundaryRole: .canonicalPauseAnchor
        )

        let first = MultiTrackPhase1BResolver.resolve(
            train: train,
            candidates: [hfs, firstPause],
            pauseSettings: PauseDetectorSettings(
                minValidISISec: 0.001,
                strongThresholdSec: 0.12
            ),
            stateSettings: hfsSettings,
            stagePrefix: "phase1a3b-repeated-first"
        )
        let second = MultiTrackPhase1BResolver.resolve(
            train: train,
            candidates: first + [secondPause],
            pauseSettings: PauseDetectorSettings(
                minValidISISec: 0.001,
                strongThresholdSec: 0.12
            ),
            stateSettings: hfsSettings,
            stagePrefix: "phase1a3b-repeated-second"
        )
        let selectedHFSRanges = second
            .filter { $0.selectedForAuto && $0.finalLabel == .highFrequencySpiking }
            .map { $0.startISIIndex...$0.endISIIndex }
            .sorted { $0.lowerBound < $1.lowerBound }
        let selectedPauseIndices = second.filter {
            $0.selectedForAuto && $0.finalLabel == .pause
        }.reduce(into: Set<Int>()) { indices, candidate in
            let lower = min(candidate.startISIIndex, candidate.endISIIndex)
            let upper = max(candidate.startISIIndex, candidate.endISIIndex)
            indices.formUnion(lower...upper)
        }

        XCTAssertEqual(selectedHFSRanges, [1...59, 61...79, 81...100])
        XCTAssertTrue(selectedPauseIndices.isSuperset(of: [60, 80]))
        XCTAssertFalse(
            second.contains {
                $0.selectedForAuto &&
                    $0.finalLabel == .highFrequencySpiking &&
                    $0.startISIIndex <= 80 &&
                    $0.endISIIndex >= 80
            }
        )
    }

    func testPhase1A3bHardBoundaryConsumptionUsesCompositeIdentity() {
        let train = makeTrain(
            intervals: intervals(
                count: 60,
                base: 0.008,
                replacing: [(30...30, 0.20)]
            )
        )
        let localHFS = makeCandidate(
            id: "shared-state-id",
            label: .highFrequencySpiking,
            start: 1,
            end: 60,
            priority: 1_040,
            q50: 0.008
        )
        let otherTrainHFS = makeCandidate(
            id: "shared-state-id",
            trainID: "other-train",
            label: .highFrequencySpiking,
            start: 1,
            end: 60,
            priority: 1_040,
            q50: 0.008
        )
        let pause = makeCandidate(
            id: "local-pause",
            label: .pause,
            start: 30,
            end: 30,
            priority: 900,
            q50: 0.20,
            pauseBoundaryRole: .canonicalPauseAnchor
        )

        let resolved = MultiTrackPhase1BResolver.resolve(
            train: train,
            candidates: [localHFS, otherTrainHFS, pause],
            pauseSettings: PauseDetectorSettings(
                minValidISISec: 0.001,
                strongThresholdSec: 0.12
            ),
            stateSettings: hfsSettings,
            stagePrefix: "phase1a3b-composite-consumption"
        )
        let local = resolved.first {
            $0.trainID == train.id && $0.id == localHFS.id
        }
        let other = resolved.first {
            $0.trainID == otherTrainHFS.trainID && $0.id == otherTrainHFS.id
        }

        XCTAssertEqual(local?.selectedForAuto, false)
        XCTAssertEqual(local?.stateContinuityAuthorityFrozen, true)
        XCTAssertTrue(local?.decisionPath.contains("state_hard_boundary_consumed=true") == true)
        XCTAssertNotNil(other)
        XCTAssertNotEqual(other?.stateContinuityAuthorityFrozen, true)
        XCTAssertFalse(other?.decisionPath.contains("state_hard_boundary_consumed=true") == true)
    }

    func testPhase1A3bHFProtectionUsesCompositeIdentity() {
        let localHFS = makeCandidate(
            id: "shared-hfs-id",
            label: .highFrequencySpiking,
            start: 1,
            end: 100,
            priority: 1_040,
            q50: 0.008
        )
        let otherTrainHFS = makeCandidate(
            id: "shared-hfs-id",
            trainID: "other-train",
            label: .highFrequencySpiking,
            start: 1,
            end: 100,
            priority: 1_040,
            q50: 0.008
        )
        let localBurst = makeCandidate(
            id: "local-majority-burst",
            label: .burst,
            start: 10,
            end: 70,
            priority: 1_500,
            selected: true,
            q50: 0.004
        )

        let protected = HFSpikingProtection.apply(
            to: [localHFS, otherTrainHFS, localBurst],
            selectedEvents: [localBurst],
            mode: .multiTrack
        )
        let local = protected.first {
            $0.trainID == localHFS.trainID && $0.id == localHFS.id
        }
        let other = protected.first {
            $0.trainID == otherTrainHFS.trainID && $0.id == otherTrainHFS.id
        }

        XCTAssertEqual(local?.finalLabel, .reject)
        XCTAssertEqual(local?.hfSpikingBurstDominated, true)
        XCTAssertEqual(other?.finalLabel, .highFrequencySpiking)
        XCTAssertEqual(other?.hfSpikingBurstDominated, false)
        XCTAssertEqual(other?.trainID, "other-train")
    }

    private var hfsSettings: StatePatternDetectorSettings {
        StatePatternDetectorSettings(
            highFrequencySpikingMinSpikes: 10,
            highFrequencySpikingMinDurationSec: 0,
            highFrequencySpikingInternalPacketizationPolicy: .multiTrackEventOverlay
        )
    }

    /// Builds a train whose raw ISIs agree with the candidate summaries used by
    /// each test. ISI indices are one-based, matching `ClassicAnchorCandidate`.
    private func intervals(
        count: Int,
        base: Double,
        replacing replacements: [(ClosedRange<Int>, Double)]
    ) -> [Double] {
        var values = Array(repeating: base, count: count)
        for (range, replacement) in replacements {
            for isiIndex in range where isiIndex >= 1 && isiIndex <= count {
                values[isiIndex - 1] = replacement
            }
        }
        return values
    }

    private func makeTrain(intervals: [Double]) -> SpikeTrain {
        var timestamps = [0.0]
        timestamps.reserveCapacity(intervals.count + 1)
        for interval in intervals {
            timestamps.append((timestamps.last ?? 0) + interval)
        }
        return SpikeTrain(name: "train-1", timestampsSec: timestamps)
    }

    private func makeCandidate(
        id: String,
        trainID: String = "train-1",
        label: ClassicAnchorLabel,
        start: Int,
        end: Int,
        priority: Int,
        cv: Double = 0.20,
        selected: Bool = false,
        q50: Double,
        pauseBoundaryRole: PauseBoundaryRole? = nil
    ) -> ClassicAnchorCandidate {
        let nISI = max(1, end - start + 1)
        return ClassicAnchorCandidate(
            id: id,
            trainID: trainID,
            trainName: trainID,
            candidateLayer: "phase1a3b_test_candidate",
            candidateClass: label.rawValue,
            finalLabel: label,
            gateStatus: "pass",
            decisionPath: "phase1a3b_test",
            action: "accept",
            score: 1,
            priority: priority,
            selectedForAuto: selected,
            selectionStatus: selected ? "selected_for_test" : "not_selected",
            startISIIndex: start,
            endISIIndex: end,
            startSpikeIndex: start,
            endSpikeIndex: end + 1,
            nISI: nISI,
            nValidISI: nISI,
            nSpikes: nISI + 1,
            durationSec: Double(nISI) * q50,
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
            refractorySuspectAction: nil,
            pauseBoundaryRole: pauseBoundaryRole
        )
    }
}
