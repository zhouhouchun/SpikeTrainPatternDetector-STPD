import XCTest
@testable import STPDCore

final class GapTrackResolverTests: XCTestCase {
    func testPauseUsesGapTrack() {
        let pause = makeCandidate(
            id: "pause-1",
            label: .pause,
            start: 10,
            end: 10
        )

        XCTAssertEqual(pause.arbitrationTrack, .gap)
        XCTAssertEqual(pause.eventGrammarAudit.recommendedTrack, .gap)
        XCTAssertEqual(pause.eventGrammarAudit.recommendedEventTrackClass, "none")
    }


    func testStructuralPausePriorRemainsDiagnosticOnly() {
        let prior = makeCandidate(
            id: "pause-prior",
            label: .pause,
            start: 9,
            end: 9,
            layer: "classic_burst_flank_pause",
            decisionPath: "role=pause_prior_pool_anchor",
            action: "audit_only",
            lockLevel: .auditOnly
        )

        XCTAssertNil(prior.arbitrationTrack)
        XCTAssertEqual(prior.eventGrammarAudit.recommendedTrack, .diagnostic)
    }

    func testDuplicateGapEvidenceUsesOneDeterministicRepresentative() {
        let structural = makeCandidate(
            id: "pause-structural",
            label: .pause,
            start: 12,
            end: 12,
            layer: "classic_burst_flank_pause",
            priority: 100,
            lockLevel: .lockedClassic
        )
        let completion = makeCandidate(
            id: "pause-completion",
            label: .pause,
            start: 12,
            end: 12,
            layer: "pause_monotonic_completion",
            decisionPath: "pipeline_stage=pause_floor_completion",
            priority: 1,
            lockLevel: .strongCandidate
        )

        let resolution = GapTrackResolver.resolve(
            [completion, structural],
            selectedEvents: []
        )

        XCTAssertEqual(resolution.selectedIDs, ["pause-structural"])
        XCTAssertEqual(
            resolution.selectedStatusByID["pause-structural"],
            "selected_by_gap_track_structural_flank_pause"
        )
        XCTAssertTrue(
            resolution.unselectedStatusByID["pause-completion"]?
                .hasPrefix("not_selected__duplicate_gap_evidence") == true
        )
    }

    func testMonotonicCompletionIsSelectedWithoutEventCompetition() {
        let burst = makeCandidate(
            id: "burst-1",
            label: .burst,
            start: 1,
            end: 4,
            priority: 10_000
        )
        let completion = makeCandidate(
            id: "pause-completion",
            label: .pause,
            start: 12,
            end: 12,
            layer: "pause_monotonic_completion",
            decisionPath: "pipeline_stage=train_adaptive_pause_floor_completion",
            priority: 1
        )

        let result = ClassicAnchorCandidateArbitrator.arbitrateBySemanticTrack([
            burst,
            completion
        ])
        let byID = Dictionary(uniqueKeysWithValues: result.map { ($0.id, $0) })

        XCTAssertEqual(byID["burst-1"]?.selectedForAuto, true)
        XCTAssertEqual(byID["pause-completion"]?.selectedForAuto, true)
        XCTAssertEqual(
            byID["pause-completion"]?.selectionStatus,
            "selected_by_gap_track_monotonic_completion"
        )
    }

    func testGapConflictingWithSelectedBurstCoreIsRejected() {
        let burst = makeCandidate(
            id: "burst-1",
            label: .burst,
            start: 4,
            end: 8,
            priority: 10_000
        )
        let pause = makeCandidate(
            id: "pause-1",
            label: .pause,
            start: 7,
            end: 7,
            layer: "pause_monotonic_completion"
        )

        let result = ClassicAnchorCandidateArbitrator.arbitrateBySemanticTrack([
            burst,
            pause
        ])
        let byID = Dictionary(uniqueKeysWithValues: result.map { ($0.id, $0) })

        XCTAssertEqual(byID["burst-1"]?.selectedForAuto, true)
        XCTAssertEqual(byID["pause-1"]?.selectedForAuto, false)
        XCTAssertEqual(
            byID["pause-1"]?.selectionStatus,
            "not_selected__gap_conflicts_with_selected_event_core"
        )
    }

    func testGapConflictingWithBurstFamilyEventCoreIsRejected() {
        let burstII = makeCandidate(
            id: "burst-ii-1",
            label: .possibleBurst,
            start: 7,
            end: 9,
            priority: 1_200
        )
        let pause = makeCandidate(
            id: "pause-1",
            label: .pause,
            start: 8,
            end: 8,
            layer: "pause_monotonic_completion"
        )

        let result = ClassicAnchorCandidateArbitrator.arbitrateBySemanticTrack([
            burstII,
            pause
        ])
        let byID = Dictionary(uniqueKeysWithValues: result.map { ($0.id, $0) })

        XCTAssertEqual(byID["burst-ii-1"]?.selectedForAuto, true)
        XCTAssertEqual(byID["pause-1"]?.selectedForAuto, false)
        XCTAssertEqual(
            byID["pause-1"]?.selectionStatus,
            "not_selected__gap_conflicts_with_selected_event_core"
        )
    }

    func testStateTrackOverridesOverlappingGap() {
        let pause = makeCandidate(
            id: "pause-1",
            label: .pause,
            start: 10,
            end: 10,
            layer: "pause_monotonic_completion"
        )
        let tonic = makeCandidate(
            id: "tonic-1",
            label: .tonic,
            start: 1,
            end: 20,
            priority: 500
        )

        let result = ClassicAnchorCandidateArbitrator.arbitrateBySemanticTrack([
            pause,
            tonic
        ])
        let byID = Dictionary(uniqueKeysWithValues: result.map { ($0.id, $0) })

        XCTAssertEqual(byID["pause-1"]?.selectedForAuto, false)
        XCTAssertEqual(byID["tonic-1"]?.selectedForAuto, true)
        XCTAssertEqual(
            byID["pause-1"]?.selectionStatus,
            "not_selected__gap_track_overlapped_higher_priority_state_track"
        )
    }

    func testBurstFamilyEventHasPriorityOverOverlappingHFSpiking() {
        let hfs = makeCandidate(
            id: "hfs-1",
            label: .highFrequencySpiking,
            start: 1,
            end: 40,
            priority: 1_000,
            hfsBurstDominated: false
        )
        let burst = makeCandidate(
            id: "burst-1",
            label: .burst,
            start: 10,
            end: 13,
            priority: 1_500
        )

        let result = ClassicAnchorCandidateArbitrator.arbitrateBySemanticTrack([
            hfs,
            burst
        ])
        let byID = Dictionary(uniqueKeysWithValues: result.map { ($0.id, $0) })

        XCTAssertEqual(byID["hfs-1"]?.selectedForAuto, false)
        XCTAssertEqual(byID["burst-1"]?.selectedForAuto, true)
        XCTAssertEqual(
            byID["hfs-1"]?.selectionStatus,
            "not_selected__hfs_state_overlaps_selected_burst_event"
        )
    }

    func testBurstDominatedHFSpikingIsRejectedWhenBurstEventsAreSelected() {
        let hfs = makeCandidate(
            id: "hfs-1",
            label: .highFrequencySpiking,
            start: 1,
            end: 40,
            priority: 1_000,
            hfsBurstDominated: true
        )
        let burst = makeCandidate(
            id: "burst-1",
            label: .burst,
            start: 10,
            end: 13,
            priority: 1_500
        )

        let result = ClassicAnchorCandidateArbitrator.arbitrateBySemanticTrack([
            hfs,
            burst
        ])
        let byID = Dictionary(uniqueKeysWithValues: result.map { ($0.id, $0) })

        XCTAssertEqual(byID["burst-1"]?.selectedForAuto, true)
        XCTAssertEqual(byID["hfs-1"]?.selectedForAuto, false)
        XCTAssertEqual(
            byID["hfs-1"]?.selectionStatus,
            "not_selected__hfs_burst_packet_dominance"
        )
    }

    func testPossibleBurstReviewDoesNotRemoveState() {
        let tonic = makeCandidate(
            id: "tonic-1",
            label: .tonic,
            start: 1,
            end: 20,
            priority: 500
        )
        let possible = makeCandidate(
            id: "possible-1",
            label: .possibleBurst,
            start: 7,
            end: 10,
            layer: "possible_burst_boundary_review",
            decisionPath: "possible_burst_boundary_review",
            action: "review",
            priority: 300,
            lockLevel: .strongCandidate
        )

        let result = ClassicAnchorCandidateArbitrator.arbitrateBySemanticTrack([
            tonic,
            possible
        ])
        let byID = Dictionary(uniqueKeysWithValues: result.map { ($0.id, $0) })

        XCTAssertEqual(byID["tonic-1"]?.selectedForAuto, true)
        XCTAssertEqual(byID["possible-1"]?.selectedForAuto, true)
        XCTAssertEqual(
            byID["tonic-1"]?.selectionStatus,
            "selected_by_state_track_weighted_interval_grammar__possible_burst_review_overlay"
        )
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
        hfsBurstDominated: Bool? = nil,
        lockLevel: ClassicAnchorLockLevel = .lockedClassic
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
            selectedForAuto: false,
            selectionStatus: "not_selected",
            startISIIndex: start,
            endISIIndex: end,
            startSpikeIndex: start,
            endSpikeIndex: end + 1,
            nISI: max(1, end - start + 1),
            nValidISI: max(1, end - start + 1),
            nSpikes: max(2, end - start + 2),
            durationSec: 0.1,
            intraQ10Sec: 0.01,
            intraQ40Sec: 0.01,
            intraQ50Sec: 0.01,
            intraQ90Sec: 0.01,
            intraQ95Sec: 0.01,
            maxIntraISISec: 0.01,
            meanIntraISISec: 0.01,
            cv: 0.1,
            lv: 0.1,
            preGapSec: nil,
            postGapSec: nil,
            preRatioQ90: nil,
            postRatioQ90: nil,
            edgeContrastMinQ90: nil,
            edgeContrastGeomQ90: nil,
            anchorFamily: label.rawValue,
            anchorLockLevel: lockLevel,
            anchorBandLowerSec: 0.001,
            anchorBandUpperSec: 0.1,
            anchorBandSource: .structure,
            anchorContrastMinRequired: 1,
            anchorContrastGeomRequired: 1,
            refractorySuspectCount: 0,
            refractorySuspectAction: nil,
            hfSpikingBurstDominated: hfsBurstDominated
        )
    }
}
