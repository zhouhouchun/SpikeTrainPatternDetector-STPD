import Foundation
import Testing
@testable import STPDCore

@Test
func phase1a3aResolverMergesOnlySelectedIrregularTonicFragments() throws {
    let train = phase1a3aMergeTrain(gapISISec: 0.045)
    let settings = phase1a3aSettings()
    var left = phase1a3aCandidate(
        train: train,
        id: "selected-left",
        label: .tonic,
        start: 1,
        end: 28,
        priority: 1_100,
        subtype: "irregular"
    )
    left.cv2 = 0.42
    var right = phase1a3aCandidate(
        train: train,
        id: "selected-right",
        label: .tonic,
        start: 30,
        end: 57,
        priority: 1_100,
        subtype: "irregular"
    )
    right.cv2 = 0.37

    let resolved = MultiTrackPhase1BResolver.resolve(
        train: train,
        candidates: [left, right],
        pauseSettings: PauseDetectorSettings(),
        stateSettings: settings,
        stagePrefix: "phase1a3a"
    )

    let merged = try #require(resolved.first {
        $0.selectedForAuto &&
            $0.finalLabel == .tonic &&
            $0.startISIIndex == 1 &&
            $0.endISIIndex == 57
    })
    #expect(merged.stateTonicSubtype == "irregular")
    #expect(merged.decisionPath.contains("irregular_tonic_micro_gap_merge=true"))
    #expect(merged.decisionPath.contains("merge_fragment_authority=caller_authorized_fragment_ids"))
    #expect(merged.decisionPath.contains("merged_span_qc_complete=true"))
    #expect(merged.decisionPath.contains("manual_tonic_hard_gate_status=not_configured"))
    #expect(merged.decisionPath.contains("fragment_rate_continuity_pass=true"))
    #expect(merged.decisionPath.contains("fragment_median_ratio="))
    #expect(merged.decisionPath.contains("fragment_median_ratio_max=1.35"))
    #expect(merged.decisionPath.contains("fragment_medians_sec="))
    for fragment in [left, right] {
        let auditFragment = try #require(resolved.first { $0.id == fragment.id })
        #expect(!auditFragment.selectedForAuto)
        #expect(!auditFragment.isEligibleForAutoSelection)
        #expect(auditFragment.stateContinuityAuthorityFrozen)
        #expect(auditFragment.selectionStatus ==
            "not_selected__consumed_by_irregular_tonic_micro_gap_merge")
        #expect(auditFragment.decisionPath.contains("state_continuity_consumed=true"))
        #expect(auditFragment.decisionPath.contains("phase1a3a_test"))
        #expect(auditFragment.startISIIndex == fragment.startISIIndex)
        #expect(auditFragment.endISIIndex == fragment.endISIIndex)
        #expect(auditFragment.stateTonicSubtype == fragment.stateTonicSubtype)
        #expect(auditFragment.cv2 == fragment.cv2)
    }

    let rerun = MultiTrackPhase1BResolver.resolve(
        train: train,
        candidates: resolved,
        pauseSettings: PauseDetectorSettings(),
        stateSettings: settings,
        stagePrefix: "phase1a3a_rerun"
    )
    for fragment in [left, right] {
        let auditFragment = try #require(rerun.first { $0.id == fragment.id })
        #expect(auditFragment.selectionStatus ==
            "not_selected__consumed_by_irregular_tonic_micro_gap_merge")
        #expect(auditFragment.stateContinuityAuthorityFrozen)
        #expect(auditFragment.decisionPath.contains("state_continuity_consumed=true"))
        #expect(auditFragment.cv2 == fragment.cv2)
    }
}

@Test
func phase1a3aStateSplitPreservesFrozenAndTerminalContinuityAuthority() throws {
    let train = phase1a3aTrain(isi: Array(repeating: 0.040, count: 12))
    var parent = phase1a3aCandidate(
        train: train,
        id: "continuity-authority-parent",
        label: .tonic,
        start: 1,
        end: 12,
        priority: 1_100,
        subtype: "irregular"
    )
    parent.stateContinuityAuthorityFrozen = true
    parent.stateContinuityMergeTerminal = true

    let rebuilt = try #require(StatePatternDetector.rebuildSplitCandidate(
        train: train,
        parent: parent,
        range: 1...8,
        settings: phase1a3aSettings(),
        splitByCandidateIDs: ["splitter"]
    ))

    #expect(rebuilt.stateContinuityAuthorityFrozen)
    #expect(rebuilt.stateContinuityMergeTerminal)
    #expect(!rebuilt.isEligibleForAutoSelection)
    #expect(rebuilt.decisionPath.contains("parent_candidate_id=\(parent.id)"))
}

@Test
func phase1a3aUnselectedFragmentCannotBeRepromotedByMerge() {
    let train = phase1a3aMergeTrain(gapISISec: 0.045)
    let settings = phase1a3aSettings()
    let left = phase1a3aCandidate(
        train: train,
        id: "authorized-left",
        label: .tonic,
        start: 1,
        end: 28,
        priority: 1_100,
        subtype: "irregular"
    )
    let right = phase1a3aCandidate(
        train: train,
        id: "unselected-right",
        label: .tonic,
        start: 30,
        end: 57,
        priority: 1_100,
        subtype: "irregular"
    )

    let result = StatePatternDetector.mergeIrregularTonicMicroGaps(
        train: train,
        candidates: [left, right],
        selectedEvents: [],
        selectedGaps: [],
        settings: settings,
        authorizedFragmentIDs: [left.id]
    )

    #expect(Set(result.map(\.id)) == Set([left.id, right.id]))
    #expect(!result.contains { $0.decisionPath.contains("irregular_tonic_micro_gap_merge=true") })
}

@Test
func phase1a3aEmptyAuthorizationSetDisablesMerge() {
    let train = phase1a3aMergeTrain(gapISISec: 0.045)
    let fragments = phase1a3aFragments(train: train)

    let result = StatePatternDetector.mergeIrregularTonicMicroGaps(
        train: train,
        candidates: fragments,
        selectedEvents: [],
        selectedGaps: [],
        settings: phase1a3aSettings()
    )

    #expect(result == fragments)
    #expect(!result.contains { $0.decisionPath.contains("irregular_tonic_micro_gap_merge=true") })
}

@Test
func phase1a3aRateContinuityGateRejectsOtherwiseMergeableDistinctRegimes() throws {
    let left = Array(repeating: 0.024, count: 14) + Array(repeating: 0.046, count: 14)
    let right = Array(repeating: 0.036, count: 14) + Array(repeating: 0.069, count: 14)
    let train = phase1a3aTrain(isi: left + [0.045] + right)
    let fragments = [
        phase1a3aCandidate(
            train: train, id: "rate-left", label: .tonic,
            start: 1, end: 28, priority: 1_100, subtype: "irregular"
        ),
        phase1a3aCandidate(
            train: train, id: "rate-right", label: .tonic,
            start: 30, end: 57, priority: 1_100, subtype: "irregular"
        )
    ]
    var permissive = phase1a3aSettings()
    permissive.irregularTonicFragmentMedianRatioMax = 1.60

    let permissiveResult = StatePatternDetector.mergeIrregularTonicMicroGaps(
        train: train,
        candidates: fragments,
        selectedEvents: [],
        selectedGaps: [],
        settings: permissive,
        authorizedFragmentIDs: Set(fragments.map(\.id))
    )
    let conservativeResult = StatePatternDetector.mergeIrregularTonicMicroGaps(
        train: train,
        candidates: fragments,
        selectedEvents: [],
        selectedGaps: [],
        settings: phase1a3aSettings(),
        authorizedFragmentIDs: Set(fragments.map(\.id))
    )

    let permissiveMerge = try #require(permissiveResult.first {
        $0.startISIIndex == 1 && $0.endISIIndex == 57
    })
    #expect(permissiveMerge.decisionPath.contains("fragment_median_ratio=1.5"))
    #expect(conservativeResult == fragments)
}

@Test
func phase1a3aRateContinuityDecisionIsScaleInvariant() {
    func mergedAtScale(_ scale: Double) -> Bool {
        let left = Array(repeating: 0.024 * scale, count: 14) +
            Array(repeating: 0.046 * scale, count: 14)
        let right = Array(repeating: 0.036 * scale, count: 14) +
            Array(repeating: 0.069 * scale, count: 14)
        let train = phase1a3aTrain(isi: left + [0.045 * scale] + right)
        let fragments = [
            phase1a3aCandidate(
                train: train, id: "scaled-left-\(scale)", label: .tonic,
                start: 1, end: 28, priority: 1_100, subtype: "irregular"
            ),
            phase1a3aCandidate(
                train: train, id: "scaled-right-\(scale)", label: .tonic,
                start: 30, end: 57, priority: 1_100, subtype: "irregular"
            )
        ]
        let settings = phase1a3aSettings(scale: scale)
        return StatePatternDetector.mergeIrregularTonicMicroGaps(
            train: train,
            candidates: fragments,
            selectedEvents: [],
            selectedGaps: [],
            settings: settings,
            authorizedFragmentIDs: Set(fragments.map(\.id))
        ).contains { $0.startISIIndex == 1 && $0.endISIIndex == 57 }
    }

    #expect(!mergedAtScale(1))
    #expect(!mergedAtScale(10))
}

@Test
func phase1a3aAcceptedRateContinuityIsScaleInvariant() {
    func mergedAtScale(_ scale: Double) -> Bool {
        let train = phase1a3aTrain(
            isi: Array(repeating: 0.035 * scale, count: 28) +
                [0.040 * scale] +
                Array(repeating: 0.040 * scale, count: 28)
        )
        let fragments = [
            phase1a3aCandidate(
                train: train, id: "accepted-scaled-left-\(scale)", label: .tonic,
                start: 1, end: 28, priority: 1_100, subtype: "irregular"
            ),
            phase1a3aCandidate(
                train: train, id: "accepted-scaled-right-\(scale)", label: .tonic,
                start: 30, end: 57, priority: 1_100, subtype: "irregular"
            )
        ]
        return StatePatternDetector.mergeIrregularTonicMicroGaps(
            train: train,
            candidates: fragments,
            selectedEvents: [],
            selectedGaps: [],
            settings: phase1a3aSettings(scale: scale),
            authorizedFragmentIDs: Set(fragments.map(\.id))
        ).contains {
            $0.startISIIndex == 1 &&
                $0.endISIIndex == 57 &&
                $0.decisionPath.contains("irregular_tonic_micro_gap_merge=true")
        }
    }

    #expect(mergedAtScale(1))
    #expect(mergedAtScale(10))
}

@Test
func phase1a3aDistinctTonicRateRegimesDoNotMerge() {
    let left = (0..<28).map { 0.015 + Double($0) * (0.040 / 27.0) }
    let right = (0..<28).map { 0.035 + Double($0) * (0.070 / 27.0) }
    let train = phase1a3aTrain(isi: left + [0.045] + right)
    let fragments = [
        phase1a3aCandidate(
            train: train, id: "fast-tonic-regime", label: .tonic,
            start: 1, end: 28, priority: 1_100, subtype: "irregular"
        ),
        phase1a3aCandidate(
            train: train, id: "slow-tonic-regime", label: .tonic,
            start: 30, end: 57, priority: 1_100, subtype: "irregular"
        )
    ]

    let result = StatePatternDetector.mergeIrregularTonicMicroGaps(
        train: train,
        candidates: fragments,
        selectedEvents: [],
        selectedGaps: [],
        settings: phase1a3aSettings(),
        authorizedFragmentIDs: Set(fragments.map(\.id))
    )

    #expect(result == fragments)
    #expect(!result.contains { $0.startISIIndex == 1 && $0.endISIIndex == 57 })
}

@Test
func phase1a3aQCInvalidGapDoesNotDisappearDuringMerge() {
    let train = phase1a3aMergeTrain(gapISISec: 0.0005)
    let settings = phase1a3aSettings()
    let fragments = phase1a3aFragments(train: train)

    let result = StatePatternDetector.mergeIrregularTonicMicroGaps(
        train: train,
        candidates: fragments,
        selectedEvents: [],
        selectedGaps: [],
        settings: settings,
        authorizedFragmentIDs: Set(fragments.map(\.id))
    )

    #expect(Set(result.map(\.id)) == Set(fragments.map(\.id)))
    #expect(!result.contains { $0.startISIIndex == 1 && $0.endISIIndex == 57 })
}

@Test
func phase1a3aManualTonicHardGateBlocksOutOfBandGap() {
    let train = phase1a3aMergeTrain(gapISISec: 0.018)
    var settings = phase1a3aSettings()
    settings.manualTonicHardLowerSec = 0.020
    settings.manualTonicHardUpperSec = 0.060
    let fragments = phase1a3aFragments(train: train)

    let result = StatePatternDetector.mergeIrregularTonicMicroGaps(
        train: train,
        candidates: fragments,
        selectedEvents: [],
        selectedGaps: [],
        settings: settings,
        authorizedFragmentIDs: Set(fragments.map(\.id))
    )

    #expect(Set(result.map(\.id)) == Set(fragments.map(\.id)))
    #expect(!result.contains { $0.startISIIndex == 1 && $0.endISIIndex == 57 })
}

@Test
func phase1a3aConfiguredManualTonicHardGateRecordsConfiguredPass() throws {
    let train = phase1a3aMergeTrain(gapISISec: 0.045)
    var settings = phase1a3aSettings()
    settings.manualTonicHardLowerSec = 0.020
    settings.manualTonicHardUpperSec = 0.060
    let fragments = phase1a3aFragments(train: train)

    let result = StatePatternDetector.mergeIrregularTonicMicroGaps(
        train: train,
        candidates: fragments,
        selectedEvents: [],
        selectedGaps: [],
        settings: settings,
        authorizedFragmentIDs: Set(fragments.map(\.id))
    )

    let merged = try #require(result.first {
        $0.startISIIndex == 1 && $0.endISIIndex == 57
    })
    #expect(merged.decisionPath.contains("manual_tonic_hard_gate_status=configured_pass"))
    #expect(!merged.decisionPath.contains("manual_tonic_hard_gate_status=not_configured"))
}

@Test
func phase1a3aInvalidManualTonicHardGateFailsClosed() {
    let train = phase1a3aMergeTrain(gapISISec: 0.045)
    let fragments = phase1a3aFragments(train: train)
    let authorized = Set(fragments.map(\.id))

    var nonFiniteLower = phase1a3aSettings()
    nonFiniteLower.manualTonicHardLowerSec = .nan
    var nonFiniteUpper = phase1a3aSettings()
    nonFiniteUpper.manualTonicHardUpperSec = .infinity
    var inverted = phase1a3aSettings()
    inverted.manualTonicHardLowerSec = 0.060
    inverted.manualTonicHardUpperSec = 0.020

    for settings in [nonFiniteLower, nonFiniteUpper, inverted] {
        let result = StatePatternDetector.mergeIrregularTonicMicroGaps(
            train: train,
            candidates: fragments,
            selectedEvents: [],
            selectedGaps: [],
            settings: settings,
            authorizedFragmentIDs: authorized
        )
        #expect(Set(result.map(\.id)) == Set(fragments.map(\.id)))
        #expect(!result.contains {
            $0.decisionPath.contains("irregular_tonic_micro_gap_merge=true")
        })
    }
}

@Test
func phase1a3aSelectedPauseAndBurstRemainHardMergeBoundaries() {
    let train = phase1a3aMergeTrain(gapISISec: 0.045)
    let settings = phase1a3aSettings()
    let fragments = phase1a3aFragments(train: train)
    let authorized = Set(fragments.map(\.id))
    let pause = phase1a3aCandidate(
        train: train,
        id: "selected-pause",
        label: .pause,
        start: 29,
        end: 29,
        priority: 1_500,
        subtype: nil,
        selected: true
    )
    let burst = phase1a3aCandidate(
        train: train,
        id: "selected-burst",
        label: .burst,
        start: 29,
        end: 29,
        priority: 1_500,
        subtype: nil,
        selected: true
    )

    let pauseBlocked = StatePatternDetector.mergeIrregularTonicMicroGaps(
        train: train,
        candidates: fragments + [pause],
        selectedEvents: [],
        selectedGaps: [pause],
        settings: settings,
        authorizedFragmentIDs: authorized
    )
    let burstBlocked = StatePatternDetector.mergeIrregularTonicMicroGaps(
        train: train,
        candidates: fragments + [burst],
        selectedEvents: [burst],
        selectedGaps: [],
        settings: settings,
        authorizedFragmentIDs: authorized
    )

    #expect(!pauseBlocked.contains { $0.startISIIndex == 1 && $0.endISIIndex == 57 })
    #expect(!burstBlocked.contains { $0.startISIIndex == 1 && $0.endISIIndex == 57 })
    #expect(pauseBlocked.contains { $0.id == pause.id })
    #expect(burstBlocked.contains { $0.id == burst.id })
}

@Test
func phase1a3aSelectedClassicTonicRegimeIsNotAbsorbedAsGap() throws {
    let train = phase1a3aTrain(
        isi: Array(repeating: 0.060, count: 10) +
            Array(repeating: 0.040, count: 4) +
            Array(repeating: 0.060, count: 10)
    )
    let left = phase1a3aCandidate(
        train: train, id: "irregular-left-around-classic", label: .tonic,
        start: 1, end: 10, priority: 1_100, subtype: "irregular"
    )
    let right = phase1a3aCandidate(
        train: train, id: "irregular-right-around-classic", label: .tonic,
        start: 15, end: 24, priority: 1_100, subtype: "irregular"
    )
    let classic = phase1a3aCandidate(
        train: train, id: "selected-classic-between-irregular", label: .tonic,
        start: 11, end: 14, priority: 1_200, subtype: "classic", selected: true
    )
    var settings = phase1a3aSettings()
    settings.tonicMaxBridgeISI = 4
    settings.tonicBridgeFractionMax = 0.25
    let authorized = Set([left.id, right.id])

    let control = StatePatternDetector.mergeIrregularTonicMicroGaps(
        train: train,
        candidates: [left, right],
        selectedEvents: [],
        selectedGaps: [],
        settings: settings,
        authorizedFragmentIDs: authorized
    )
    let blocked = StatePatternDetector.mergeIrregularTonicMicroGaps(
        train: train,
        candidates: [left, classic, right],
        selectedEvents: [],
        selectedGaps: [],
        settings: settings,
        authorizedFragmentIDs: authorized
    )

    #expect(control.contains {
        $0.startISIIndex == 1 &&
            $0.endISIIndex == 24 &&
            $0.decisionPath.contains("irregular_tonic_micro_gap_merge=true")
    })
    #expect(!blocked.contains {
        $0.startISIIndex == 1 &&
            $0.endISIIndex == 24 &&
            $0.decisionPath.contains("irregular_tonic_micro_gap_merge=true")
    })
    #expect(blocked.contains { $0.id == classic.id && $0.selectedForAuto })
    #expect(blocked.contains { $0.id == left.id })
    #expect(blocked.contains { $0.id == right.id })
}

@Test
func phase1a3aSelectedHighFrequencyStatesRemainHardMergeBoundaries() {
    let train = phase1a3aMergeTrain(gapISISec: 0.045)
    let fragments = phase1a3aFragments(train: train)
    let authorized = Set(fragments.map(\.id))

    for (label, id) in [
        (ClassicAnchorLabel.highFrequencySpiking, "selected-hfs-boundary"),
        (ClassicAnchorLabel.highFrequencyTonic, "selected-hf-tonic-boundary")
    ] {
        let boundary = phase1a3aCandidate(
            train: train,
            id: id,
            label: label,
            start: 29,
            end: 29,
            priority: 1_400,
            subtype: nil,
            selected: true
        )
        let result = StatePatternDetector.mergeIrregularTonicMicroGaps(
            train: train,
            candidates: fragments + [boundary],
            selectedEvents: [],
            selectedGaps: [],
            settings: phase1a3aSettings(),
            authorizedFragmentIDs: authorized
        )

        #expect(!result.contains {
            $0.startISIIndex == 1 &&
                $0.endISIIndex == 57 &&
                $0.decisionPath.contains("irregular_tonic_micro_gap_merge=true")
        })
        #expect(result.contains { $0.id == boundary.id && $0.selectedForAuto })
    }
}

@Test
func phase1a3aPhysiologicallyLongUnlabeledGapRejectsAtEveryScale() {
    func rejects(scale: Double) -> Bool {
        let train = phase1a3aTrain(
            isi: Array(repeating: 0.040 * scale, count: 12) +
                [0.180 * scale] +
                Array(repeating: 0.040 * scale, count: 12)
        )
        let fragments = [
            phase1a3aCandidate(
                train: train, id: "long-gap-left-\(scale)", label: .tonic,
                start: 1, end: 12, priority: 1_100, subtype: "irregular"
            ),
            phase1a3aCandidate(
                train: train, id: "long-gap-right-\(scale)", label: .tonic,
                start: 14, end: 25, priority: 1_100, subtype: "irregular"
            )
        ]
        let result = StatePatternDetector.mergeIrregularTonicMicroGaps(
            train: train,
            candidates: fragments,
            selectedEvents: [],
            selectedGaps: [],
            settings: phase1a3aSettings(scale: scale),
            authorizedFragmentIDs: Set(fragments.map(\.id))
        )
        return !result.contains {
            $0.startISIIndex == 1 &&
                $0.endISIIndex == 25 &&
                $0.decisionPath.contains("irregular_tonic_micro_gap_merge=true")
        }
    }

    #expect(rejects(scale: 1))
    #expect(rejects(scale: 10))
}

@Test
func phase1a3aWholeSpanHighJitterFailsRevalidation() {
    let segment = [0.015, 0.060, 0.015, 0.060, 0.015, 0.060, 0.015, 0.060]
    let train = phase1a3aTrain(isi: segment + [0.040] + segment)
    let settings = phase1a3aSettings()
    let left = phase1a3aCandidate(
        train: train,
        id: "jitter-left",
        label: .tonic,
        start: 1,
        end: 8,
        priority: 1_100,
        subtype: "irregular"
    )
    let right = phase1a3aCandidate(
        train: train,
        id: "jitter-right",
        label: .tonic,
        start: 10,
        end: 17,
        priority: 1_100,
        subtype: "irregular"
    )

    let result = StatePatternDetector.mergeIrregularTonicMicroGaps(
        train: train,
        candidates: [left, right],
        selectedEvents: [],
        selectedGaps: [],
        settings: settings,
        authorizedFragmentIDs: [left.id, right.id]
    )

    #expect(Set(result.map(\.id)) == Set([left.id, right.id]))
}

@Test
func phase1a3aClassicTonicAndNonTonicCandidatesAreUnchanged() {
    let train = phase1a3aMergeTrain(gapISISec: 0.045)
    let settings = phase1a3aSettings()
    let classic = phase1a3aCandidate(
        train: train,
        id: "classic-tonic",
        label: .tonic,
        start: 1,
        end: 28,
        priority: 1_100,
        subtype: "classic"
    )
    let burst = phase1a3aCandidate(
        train: train,
        id: "burst-event",
        label: .burst,
        start: 30,
        end: 35,
        priority: 1_500,
        subtype: nil
    )
    let pause = phase1a3aCandidate(
        train: train,
        id: "pause-gap",
        label: .pause,
        start: 40,
        end: 40,
        priority: 1_500,
        subtype: nil
    )
    let input = [classic, burst, pause]

    let result = StatePatternDetector.mergeIrregularTonicMicroGaps(
        train: train,
        candidates: input,
        selectedEvents: [burst],
        selectedGaps: [pause],
        settings: settings,
        authorizedFragmentIDs: Set(input.map(\.id))
    )

    #expect(result == input)
}

@Test
func phase1a3aResolverDoesNotInventTonicForNonTonicNeuron() {
    let train = phase1a3aTrain(isi: Array(repeating: 0.006, count: 40))
    let burst = phase1a3aCandidate(
        train: train,
        id: "selected-burst-only-neuron",
        label: .burst,
        start: 10,
        end: 14,
        priority: 1_500,
        subtype: nil,
        selected: true
    )
    let hfs = phase1a3aCandidate(
        train: train,
        id: "selected-hfs-only-neuron",
        label: .highFrequencySpiking,
        start: 1,
        end: 40,
        priority: 1_200,
        subtype: nil,
        selected: true
    )

    let result = MultiTrackPhase1BResolver.resolve(
        train: train,
        candidates: [burst, hfs],
        pauseSettings: PauseDetectorSettings(),
        stateSettings: phase1a3aSettings(),
        stagePrefix: "phase1a3a_no_tonic"
    )

    #expect(!result.contains { $0.finalLabel == .tonic })
    #expect(!result.contains { $0.decisionPath.contains("irregular_tonic_micro_gap_merge=true") })
}

@Test
func phase1a3aOtherTrainBoundaryCandidatesDoNotBlockMerge() throws {
    let train = phase1a3aMergeTrain(gapISISec: 0.045)
    let otherTrain = phase1a3aTrain(
        isi: Array(repeating: 0.040, count: 57),
        name: "phase1a3a-other-train"
    )
    let fragments = phase1a3aFragments(train: train)
    let otherPause = phase1a3aCandidate(
        train: otherTrain,
        id: "other-train-pause",
        label: .pause,
        start: 29,
        end: 29,
        priority: 1_500,
        subtype: nil,
        selected: true
    )
    let otherBurst = phase1a3aCandidate(
        train: otherTrain,
        id: "other-train-burst",
        label: .burst,
        start: 29,
        end: 29,
        priority: 1_500,
        subtype: nil,
        selected: true
    )
    let otherHFS = phase1a3aCandidate(
        train: otherTrain,
        id: "other-train-hfs",
        label: .highFrequencySpiking,
        start: 29,
        end: 29,
        priority: 1_200,
        subtype: nil
    )

    let result = StatePatternDetector.mergeIrregularTonicMicroGaps(
        train: train,
        candidates: fragments + [otherHFS],
        selectedEvents: [otherBurst],
        selectedGaps: [otherPause],
        settings: phase1a3aSettings(),
        authorizedFragmentIDs: Set(fragments.map(\.id))
    )

    let merged = try #require(result.first {
        $0.trainID == train.id &&
            $0.startISIIndex == 1 &&
            $0.endISIIndex == 57
    })
    #expect(merged.decisionPath.contains("irregular_tonic_micro_gap_merge=true"))
    #expect(result.contains { $0.id == otherHFS.id })
}

@Test
func phase1a3aConsumedChildIDDoesNotRemoveSameIDFromAnotherTrain() throws {
    let train = phase1a3aMergeTrain(gapISISec: 0.045)
    let otherTrain = phase1a3aTrain(
        isi: Array(repeating: 0.040, count: 57),
        name: "phase1a3a-id-collision-train"
    )
    let fragments = phase1a3aFragments(train: train)
    let collidingOtherTrainCandidate = phase1a3aCandidate(
        train: otherTrain,
        id: fragments[0].id,
        label: .pause,
        start: 29,
        end: 29,
        priority: 1_500,
        subtype: nil
    )

    let result = StatePatternDetector.mergeIrregularTonicMicroGaps(
        train: train,
        candidates: fragments + [collidingOtherTrainCandidate],
        selectedEvents: [],
        selectedGaps: [],
        settings: phase1a3aSettings(),
        authorizedFragmentIDs: Set(fragments.map(\.id))
    )

    let merged = try #require(result.first {
        $0.trainID == train.id &&
            $0.startISIIndex == 1 &&
            $0.endISIIndex == 57
    })
    #expect(merged.decisionPath.contains("irregular_tonic_micro_gap_merge=true"))
    #expect(result.contains {
        $0.trainID == otherTrain.id && $0.id == collidingOtherTrainCandidate.id
    })
    #expect(!result.contains {
        $0.trainID == train.id && fragments.map(\.id).contains($0.id)
    })
}

@Test
func phase1a3aThreeFragmentProvenanceAlignsEachGapWithItsCap() throws {
    let train = phase1a3aTrain(
        isi: Array(repeating: 0.035, count: 10) +
            [0.040] +
            Array(repeating: 0.040, count: 10) +
            [0.045] +
            Array(repeating: 0.045, count: 10)
    )
    let fragments = [
        phase1a3aCandidate(
            train: train, id: "three-fragment-left", label: .tonic,
            start: 1, end: 10, priority: 1_100, subtype: "irregular"
        ),
        phase1a3aCandidate(
            train: train, id: "three-fragment-middle", label: .tonic,
            start: 12, end: 21, priority: 1_100, subtype: "irregular"
        ),
        phase1a3aCandidate(
            train: train, id: "three-fragment-right", label: .tonic,
            start: 23, end: 32, priority: 1_100, subtype: "irregular"
        )
    ]

    let result = StatePatternDetector.mergeIrregularTonicMicroGaps(
        train: train,
        candidates: fragments,
        selectedEvents: [],
        selectedGaps: [],
        settings: phase1a3aSettings(),
        authorizedFragmentIDs: Set(fragments.map(\.id))
    )

    let merged = try #require(result.first {
        $0.startISIIndex == 1 && $0.endISIIndex == 32
    })
    let tokens = merged.decisionPath.split(separator: ";").map(String.init)
    let evidenceToken = try #require(tokens.first {
        $0.hasPrefix("micro_gap_boundary_evidence=")
    })
    let evidence = evidenceToken
        .dropFirst("micro_gap_boundary_evidence=".count)
        .split(separator: "|")
        .map(String.init)

    #expect(evidence.count == 2)
    #expect(evidence.allSatisfy { record in
        record.contains("n=") && record.contains("max=") && record.contains("cap=")
    })
    #expect(evidence[0] == "n=1,max=0.04,cap=0.04")
    #expect(evidence[1] == "n=1,max=0.045,cap=0.045")
    #expect(tokens.contains("micro_gap_cap_summary=max_boundary_cap"))
}

@Test
func phase1a3aCumulativeGapBurdenStopsLongerChain() throws {
    let train = phase1a3aTrain(
        isi: Array(repeating: 0.035, count: 10) +
            [0.040] +
            Array(repeating: 0.040, count: 10) +
            [0.045] +
            Array(repeating: 0.045, count: 10)
    )
    let fragments = [
        phase1a3aCandidate(
            train: train, id: "burden-left", label: .tonic,
            start: 1, end: 10, priority: 1_100, subtype: "irregular"
        ),
        phase1a3aCandidate(
            train: train, id: "burden-middle", label: .tonic,
            start: 12, end: 21, priority: 1_100, subtype: "irregular"
        ),
        phase1a3aCandidate(
            train: train, id: "burden-right", label: .tonic,
            start: 23, end: 32, priority: 1_100, subtype: "irregular"
        )
    ]
    var settings = phase1a3aSettings()
    settings.tonicBridgeFractionMax = 0.055

    let result = StatePatternDetector.mergeIrregularTonicMicroGaps(
        train: train,
        candidates: fragments,
        selectedEvents: [],
        selectedGaps: [],
        settings: settings,
        authorizedFragmentIDs: Set(fragments.map(\.id))
    )

    #expect(!result.contains {
        $0.startISIIndex == 1 && $0.endISIIndex == 32
    })
    let partialMerge = try #require(result.first {
        $0.decisionPath.contains("irregular_tonic_micro_gap_merge=true")
    })
    #expect(partialMerge.startISIIndex == 1)
    #expect(partialMerge.endISIIndex == 21)
    #expect(partialMerge.decisionPath.contains("merged_gap_fraction=0.047619"))
}

@Test
func phase1a3aGeneratedMergeIsTerminalAcrossLaterContinuityPasses() throws {
    let train = phase1a3aTrain(
        isi: Array(repeating: 0.035, count: 10) +
            [0.040] +
            Array(repeating: 0.040, count: 10) +
            [0.045] +
            Array(repeating: 0.045, count: 10)
    )
    let fragments = [
        phase1a3aCandidate(
            train: train, id: "terminal-left", label: .tonic,
            start: 1, end: 10, priority: 1_100, subtype: "irregular"
        ),
        phase1a3aCandidate(
            train: train, id: "terminal-middle", label: .tonic,
            start: 12, end: 21, priority: 1_100, subtype: "irregular"
        ),
        phase1a3aCandidate(
            train: train, id: "terminal-right", label: .tonic,
            start: 23, end: 32, priority: 1_100, subtype: "irregular"
        )
    ]
    var settings = phase1a3aSettings()
    settings.tonicBridgeFractionMax = 0.055

    let firstPass = StatePatternDetector.mergeIrregularTonicMicroGaps(
        train: train,
        candidates: fragments,
        selectedEvents: [],
        selectedGaps: [],
        settings: settings,
        authorizedFragmentIDs: Set(fragments.map(\.id))
    )
    let partialMerge = try #require(firstPass.first {
        $0.startISIIndex == 1 &&
            $0.endISIIndex == 21 &&
            $0.decisionPath.contains("irregular_tonic_micro_gap_merge=true")
    })
    #expect(partialMerge.stateContinuityMergeTerminal)

    let secondPass = StatePatternDetector.mergeIrregularTonicMicroGaps(
        train: train,
        candidates: firstPass,
        selectedEvents: [],
        selectedGaps: [],
        settings: settings,
        authorizedFragmentIDs: Set(firstPass.map(\.id))
    )

    #expect(secondPass == firstPass)
    #expect(!secondPass.contains {
        $0.startISIIndex == 1 && $0.endISIIndex == 32
    })
}

@Test
func phase1a3aUnselectedHFSProposalDoesNotBlockAuthorizedMerge() throws {
    let train = phase1a3aMergeTrain(gapISISec: 0.045)
    let fragments = phase1a3aFragments(train: train)
    let hfsProposal = phase1a3aCandidate(
        train: train,
        id: "unselected-hfs-gap-proposal",
        label: .highFrequencySpiking,
        start: 20,
        end: 35,
        priority: 1,
        subtype: nil
    )

    let result = MultiTrackPhase1BResolver.resolve(
        train: train,
        candidates: fragments + [hfsProposal],
        pauseSettings: PauseDetectorSettings(),
        stateSettings: phase1a3aSettings(),
        stagePrefix: "phase1a3a_unselected_hfs"
    )

    let resolvedHFS = try #require(result.first { $0.id == hfsProposal.id })
    #expect(!resolvedHFS.selectedForAuto)
    let merged = try #require(result.first {
        $0.selectedForAuto &&
            $0.startISIIndex == 1 &&
            $0.endISIIndex == 57 &&
            $0.decisionPath.contains("irregular_tonic_micro_gap_merge=true")
    })
    #expect(merged.finalLabel == .tonic)
}

@Test
func phase1a3aResolverDoesNotRepromoteWeightedSelectionLoser() throws {
    let train = phase1a3aMergeTrain(gapISISec: 0.045)
    let left = phase1a3aCandidate(
        train: train,
        id: "selected-irregular-left",
        label: .tonic,
        start: 1,
        end: 28,
        priority: 1_100,
        subtype: "irregular"
    )
    let loser = phase1a3aCandidate(
        train: train,
        id: "weighted-selection-loser",
        label: .tonic,
        start: 30,
        end: 57,
        priority: 500,
        subtype: "irregular"
    )
    let winner = phase1a3aCandidate(
        train: train,
        id: "weighted-selection-winner",
        label: .tonic,
        start: 30,
        end: 57,
        priority: 2_000,
        subtype: "classic"
    )

    let result = MultiTrackPhase1BResolver.resolve(
        train: train,
        candidates: [left, loser, winner],
        pauseSettings: PauseDetectorSettings(),
        stateSettings: phase1a3aSettings(),
        stagePrefix: "phase1a3a_weighted_loser"
    )

    let resolvedLoser = try #require(result.first { $0.id == loser.id })
    let resolvedWinner = try #require(result.first { $0.id == winner.id })
    #expect(!resolvedLoser.selectedForAuto)
    #expect(resolvedWinner.selectedForAuto)
    #expect(!result.contains {
        $0.startISIIndex == 1 &&
            $0.endISIIndex == 57 &&
            $0.decisionPath.contains("irregular_tonic_micro_gap_merge=true")
    })
    #expect(!result.contains {
        $0.decisionPath.contains("merged_child_ids=") &&
            $0.decisionPath.contains(loser.id)
    })
}

@Test
func phase1a3aResolverQualifiesDuplicateCandidateIDsByTrain() throws {
    let targetTrain = phase1a3aMergeTrain(gapISISec: 0.045)
    let otherTrain = phase1a3aTrain(
        isi: Array(repeating: 0.040, count: 57),
        name: "phase1a3a-other-train"
    )
    let selectedOtherTrainCandidate = phase1a3aCandidate(
        train: otherTrain,
        id: "shared-candidate-id",
        label: .tonic,
        start: 1,
        end: 28,
        priority: 2_000,
        subtype: "irregular"
    )
    let targetLeft = phase1a3aCandidate(
        train: targetTrain,
        id: "target-left",
        label: .tonic,
        start: 1,
        end: 28,
        priority: 1_100,
        subtype: "irregular"
    )
    let targetLoserWithSharedID = phase1a3aCandidate(
        train: targetTrain,
        id: "shared-candidate-id",
        label: .tonic,
        start: 30,
        end: 57,
        priority: 500,
        subtype: "irregular"
    )
    let targetWinner = phase1a3aCandidate(
        train: targetTrain,
        id: "target-winner",
        label: .tonic,
        start: 30,
        end: 57,
        priority: 2_000,
        subtype: "classic"
    )

    let result = MultiTrackPhase1BResolver.resolve(
        train: targetTrain,
        candidates: [
            selectedOtherTrainCandidate,
            targetLeft,
            targetLoserWithSharedID,
            targetWinner
        ],
        pauseSettings: PauseDetectorSettings(),
        stateSettings: phase1a3aSettings(),
        stagePrefix: "phase1a3a_cross_train_identity"
    )

    let resolvedOther = try #require(result.first {
        $0.trainID == otherTrain.id && $0.id == selectedOtherTrainCandidate.id
    })
    let resolvedTargetLoser = try #require(result.first {
        $0.trainID == targetTrain.id && $0.id == targetLoserWithSharedID.id
    })
    let resolvedTargetWinner = try #require(result.first {
        $0.trainID == targetTrain.id && $0.id == targetWinner.id
    })
    #expect(resolvedOther.selectedForAuto)
    #expect(!resolvedTargetLoser.selectedForAuto)
    #expect(resolvedTargetLoser.stateContinuityAuthorityFrozen)
    #expect(resolvedTargetWinner.selectedForAuto)
    #expect(!result.contains {
        $0.trainID == targetTrain.id &&
            $0.startISIIndex == 1 &&
            $0.endISIIndex == 57 &&
            $0.decisionPath.contains("irregular_tonic_micro_gap_merge=true")
    })
}

@Test
func phase1a3aFinalArbitrationKeepsPriorStateLosersFrozenAfterMerge() throws {
    let train = phase1a3aMergeTrain(gapISISec: 0.045)
    let fragments = phase1a3aFragments(train: train)
    let leftAlternative = phase1a3aCandidate(
        train: train,
        id: "prior-loser-left",
        label: .tonic,
        start: 1,
        end: 28,
        priority: 1_099,
        subtype: "classic"
    )
    let rightAlternative = phase1a3aCandidate(
        train: train,
        id: "prior-loser-right",
        label: .tonic,
        start: 30,
        end: 57,
        priority: 1_099,
        subtype: "classic"
    )

    let result = MultiTrackPhase1BResolver.resolve(
        train: train,
        candidates: fragments + [leftAlternative, rightAlternative],
        pauseSettings: PauseDetectorSettings(),
        stateSettings: phase1a3aSettings(),
        stagePrefix: "phase1a3a_frozen_losers"
    )

    let merged = try #require(result.first {
        $0.startISIIndex == 1 &&
            $0.endISIIndex == 57 &&
            $0.decisionPath.contains("irregular_tonic_micro_gap_merge=true")
    })
    #expect(merged.selectedForAuto)
    let resolvedLeftAlternative = try #require(result.first { $0.id == leftAlternative.id })
    let resolvedRightAlternative = try #require(result.first { $0.id == rightAlternative.id })
    #expect(!resolvedLeftAlternative.selectedForAuto)
    #expect(!resolvedRightAlternative.selectedForAuto)
    #expect(!result.contains {
        ($0.id == leftAlternative.id || $0.id == rightAlternative.id) && $0.selectedForAuto
    })
}

@Test
func phase1a3aResolverSecondPassCannotRepromoteFrozenStateLosers() throws {
    let train = phase1a3aMergeTrain(gapISISec: 0.045)
    let fragments = phase1a3aFragments(train: train)
    let alternatives = [
        phase1a3aCandidate(
            train: train,
            id: "second-pass-loser-left",
            label: .tonic,
            start: 1,
            end: 28,
            priority: 1_099,
            subtype: "classic"
        ),
        phase1a3aCandidate(
            train: train,
            id: "second-pass-loser-right",
            label: .tonic,
            start: 30,
            end: 57,
            priority: 1_099,
            subtype: "classic"
        )
    ]

    let firstPass = MultiTrackPhase1BResolver.resolve(
        train: train,
        candidates: fragments + alternatives,
        pauseSettings: PauseDetectorSettings(),
        stateSettings: phase1a3aSettings(),
        stagePrefix: "phase1a3a_second_pass_first"
    )
    let secondPass = MultiTrackPhase1BResolver.resolve(
        train: train,
        candidates: firstPass,
        pauseSettings: PauseDetectorSettings(),
        stateSettings: phase1a3aSettings(),
        stagePrefix: "phase1a3a_second_pass_second"
    )

    let merged = try #require(secondPass.first {
        $0.startISIIndex == 1 &&
            $0.endISIIndex == 57 &&
            $0.decisionPath.contains("irregular_tonic_micro_gap_merge=true")
    })
    #expect(merged.selectedForAuto)
    for alternative in alternatives {
        let resolved = try #require(secondPass.first { $0.id == alternative.id })
        #expect(!resolved.selectedForAuto)
        #expect(resolved.stateContinuityAuthorityFrozen)
        #expect(!resolved.isEligibleForAutoSelection)
    }
}

@Test
func phase1a3aForgedMergeProvenanceDoesNotGrantSelectionAuthority() throws {
    let train = phase1a3aMergeTrain(gapISISec: 0.045)
    let fragments = phase1a3aFragments(train: train)
    let stale = phase1a3aCandidate(
        train: train,
        id: "stale-provenance-only-merge",
        label: .tonic,
        start: 1,
        end: 57,
        priority: 1_200,
        subtype: "irregular"
    ).withDiagnosticOverride(
        decisionPath: "phase1a3a_stale;irregular_tonic_micro_gap_merge=true",
        selectedForAuto: false,
        selectionStatus: "not_selected"
    )

    let result = MultiTrackPhase1BResolver.resolve(
        train: train,
        candidates: fragments + [stale],
        pauseSettings: PauseDetectorSettings(),
        stateSettings: phase1a3aSettings(),
        stagePrefix: "phase1a3a_forged_provenance"
    )

    let resolvedStale = try #require(result.first { $0.id == stale.id })
    #expect(!resolvedStale.selectedForAuto)
    #expect(resolvedStale.stateContinuityAuthorityFrozen)
    let selectedMerge = try #require(result.first {
        $0.id != stale.id &&
            $0.selectedForAuto &&
            $0.startISIIndex == 1 &&
            $0.endISIIndex == 57 &&
            $0.decisionPath.contains("merged_child_ids=irregular-left|irregular-right")
    })
    #expect(selectedMerge.finalLabel == .tonic)
}

@Test
func phase1a3aGeneratedIdentityCollisionReplacesStaleCandidateWithoutTrap() throws {
    let train = phase1a3aMergeTrain(gapISISec: 0.045)
    let fragments = phase1a3aFragments(train: train)
    let generatedID = "\(train.id)-irregular-tonic-micro-merge-1-57"
    let stale = phase1a3aCandidate(
        train: train,
        id: generatedID,
        label: .tonic,
        start: 1,
        end: 57,
        priority: 1_200,
        subtype: "irregular"
    ).withDiagnosticOverride(
        decisionPath: "stale_candidate;irregular_tonic_micro_gap_merge=true",
        selectedForAuto: false,
        selectionStatus: "not_selected"
    )

    let result = MultiTrackPhase1BResolver.resolve(
        train: train,
        candidates: fragments + [stale],
        pauseSettings: PauseDetectorSettings(),
        stateSettings: phase1a3aSettings(),
        stagePrefix: "phase1a3a_identity_collision"
    )

    let colliding = result.filter { $0.trainID == train.id && $0.id == generatedID }
    #expect(colliding.count == 1)
    let regenerated = try #require(colliding.first)
    #expect(regenerated.selectedForAuto)
    #expect(regenerated.decisionPath.contains("merged_child_ids=irregular-left|irregular-right"))
    #expect(!regenerated.decisionPath.contains("stale_candidate"))
}

@Test
func phase1a3aConvenienceProjectionPrefersGeneratedMergeOnConsumedIdentityCollision() throws {
    let train = phase1a3aMergeTrain(gapISISec: 0.045)
    let generatedID = "\(train.id)-irregular-tonic-micro-merge-1-57"
    let fragments = [
        phase1a3aCandidate(
            train: train,
            id: generatedID,
            label: .tonic,
            start: 1,
            end: 28,
            priority: 1_100,
            subtype: "irregular"
        ),
        phase1a3aCandidate(
            train: train,
            id: "collision-right",
            label: .tonic,
            start: 30,
            end: 57,
            priority: 1_100,
            subtype: "irregular"
        )
    ]

    let result = StatePatternDetector.mergeIrregularTonicMicroGaps(
        train: train,
        candidates: fragments,
        selectedEvents: [],
        selectedGaps: [],
        settings: phase1a3aSettings(),
        authorizedFragmentIDs: Set(fragments.map(\.id))
    )

    let colliding = result.filter { $0.trainID == train.id && $0.id == generatedID }
    #expect(colliding.count == 1)
    let generated = try #require(colliding.first)
    #expect(generated.startISIIndex == 1)
    #expect(generated.endISIIndex == 57)
    #expect(generated.decisionPath.contains("irregular_tonic_micro_gap_merge=true"))
    #expect(generated.decisionPath.contains("merged_child_ids=\(generatedID)|collision-right"))
    #expect(!result.contains { $0.id == "collision-right" })
}

@Test
func phase1a3aMergeIsIdempotent() throws {
    let train = phase1a3aMergeTrain(gapISISec: 0.045)
    let settings = phase1a3aSettings()
    let fragments = phase1a3aFragments(train: train)
    let firstPass = StatePatternDetector.mergeIrregularTonicMicroGaps(
        train: train,
        candidates: fragments,
        selectedEvents: [],
        selectedGaps: [],
        settings: settings,
        authorizedFragmentIDs: Set(fragments.map(\.id))
    )
    let merged = try #require(firstPass.first { $0.startISIIndex == 1 && $0.endISIIndex == 57 })

    let secondPass = StatePatternDetector.mergeIrregularTonicMicroGaps(
        train: train,
        candidates: firstPass,
        selectedEvents: [],
        selectedGaps: [],
        settings: settings,
        authorizedFragmentIDs: [merged.id]
    )

    #expect(secondPass == firstPass)
    #expect(secondPass.filter { $0.decisionPath.contains("irregular_tonic_micro_gap_merge=true") }.count == 1)
}

private func phase1a3aFragments(train: SpikeTrain) -> [ClassicAnchorCandidate] {
    [
        phase1a3aCandidate(
            train: train,
            id: "irregular-left",
            label: .tonic,
            start: 1,
            end: 28,
            priority: 1_100,
            subtype: "irregular"
        ),
        phase1a3aCandidate(
            train: train,
            id: "irregular-right",
            label: .tonic,
            start: 30,
            end: 57,
            priority: 1_100,
            subtype: "irregular"
        )
    ]
}

private func phase1a3aMergeTrain(gapISISec: Double) -> SpikeTrain {
    let leftMs: [Double] = [
        28.7, 44.5, 38.7, 46.6, 30.6, 34.4, 22.0, 32.1, 22.7, 41.9, 31.7, 55.0, 47.1, 33.5,
        33.9, 22.0, 34.6, 22.0, 28.3, 55.0, 25.0, 30.9, 55.0, 50.9, 23.1, 33.1, 23.2, 39.5
    ]
    let rightMs: [Double] = [
        22.0, 28.5, 22.0, 22.0, 40.5, 33.9, 39.3, 43.3, 42.2, 42.2, 26.0, 55.0, 28.8, 52.8,
        55.0, 42.9, 22.0, 24.1, 33.2, 55.0, 37.6, 33.6, 22.2, 29.5, 30.5, 22.0, 22.8, 46.5
    ]
    return phase1a3aTrain(isi: leftMs.map { $0 / 1000 } + [gapISISec] + rightMs.map { $0 / 1000 })
}

private func phase1a3aTrain(
    isi: [Double],
    name: String = "phase1a3a-train"
) -> SpikeTrain {
    var timestamps = [0.0]
    timestamps.reserveCapacity(isi.count + 1)
    for value in isi {
        timestamps.append((timestamps.last ?? 0) + value)
    }
    return SpikeTrain(name: name, timestampsSec: timestamps)
}

private func phase1a3aSettings(scale: Double = 1) -> StatePatternDetectorSettings {
    StatePatternDetectorSettings(
        minValidISISec: 0.001 * scale,
        burstSeedUpperSec: 0.010 * scale,
        tonicBridgeUpperSec: 0.075 * scale,
        highFrequencySpikingShortUpperSec: 0.020 * scale,
        highFrequencySpikingQ80MaxSec: 0.025 * scale,
        highFrequencySpikingQ90MaxSec: 0.025 * scale,
        highFrequencySpikingEpochBridgeSec: 0.035 * scale,
        highFrequencySpikingToleratedGapSec: 0.075 * scale
    )
}

private func phase1a3aCandidate(
    train: SpikeTrain,
    id: String,
    label: ClassicAnchorLabel,
    start: Int,
    end: Int,
    priority: Int,
    subtype: String?,
    selected: Bool = false
) -> ClassicAnchorCandidate {
    var candidate = ClassicAnchorCandidate(
        id: id,
        trainID: train.id,
        trainName: train.name,
        candidateLayer: "phase1a3a_test",
        candidateClass: label.rawValue,
        finalLabel: label,
        gateStatus: "pass",
        decisionPath: "phase1a3a_test",
        action: "accept",
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
        anchorFamily: label.rawValue,
        anchorLockLevel: .strongCandidate,
        anchorBandLowerSec: 0.001,
        anchorBandUpperSec: 0.075,
        anchorBandSource: .structure,
        anchorContrastMinRequired: 1,
        anchorContrastGeomRequired: 1,
        refractorySuspectCount: 0,
        refractorySuspectAction: nil
    )
    candidate.stateTonicSubtype = subtype
    return candidate
}
