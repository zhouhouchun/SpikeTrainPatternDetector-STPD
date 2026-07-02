@testable import STPDCore
import Testing

// MARK: - Phase 3 characterization net (test-only; NO production changes)
//
// Pins the CURRENT local carving-authority behavior so Phase 3A becomes falsifiable:
//   1. A `.possibleBurst` from BurstLocalCompletionDetector is routed onto the `.event` track and
//      CARVES an overlapping tonic (undesired — Phase 3A will flip this to a review overlay).
//   2. A review/boundary-review `.possibleBurst` OVERLAYS tonic instead of carving it (the target
//      behavior model already present for review-track possibleBursts).
//   3. Phase 2A containment holds: weak / completion-class possibleBurst evidence forms only a
//      train-local burst seed and does NOT enter the dataset aggregate.
//   4. Bridge strictPass canonicalization remains dataset-aggregatable — documented as bounded,
//      by-design behavior (NOT changed in this slice).
//
// Tests 1-2 exercise ClassicAnchorCandidateArbitrator.arbitrateBySemanticTrack directly with
// hand-built candidates (deterministic; targets the exact carving mechanism). Tests 3-4 exercise
// StructuralSeedBandResolver.summarize + StructuralDatasetSeedAggregator.aggregate.

private func p3Train(repeating isi: Double, count: Int) -> SpikeTrain {
    var t = [0.0]
    for _ in 0..<count { t.append((t.last ?? 0) + isi) }
    return SpikeTrain(name: "phase3_train", timestampsSec: t)
}

private func p3Candidate(
    id: String,
    label: ClassicAnchorLabel,
    start: Int,
    end: Int,
    layer: String = "test_candidate",
    gate: String = "pass",
    action: String = "accept",
    decisionPath: String = "test",
    priority: Int = 100,
    selected: Bool = false,
    q: Double = 0.006
) -> ClassicAnchorCandidate {
    ClassicAnchorCandidate(
        id: id, trainID: "train-1", trainName: "train-1",
        candidateLayer: layer, candidateClass: label.rawValue, finalLabel: label,
        gateStatus: gate, decisionPath: decisionPath, action: action,
        score: 1, priority: priority, selectedForAuto: selected,
        selectionStatus: selected ? "selected_for_test" : "not_selected",
        startISIIndex: start, endISIIndex: end, startSpikeIndex: start, endSpikeIndex: end + 1,
        nISI: max(1, end - start + 1), nValidISI: max(1, end - start + 1), nSpikes: max(2, end - start + 2),
        durationSec: Double(max(1, end - start + 1)) * q,
        intraQ10Sec: q, intraQ40Sec: q, intraQ50Sec: q, intraQ90Sec: q, intraQ95Sec: q,
        maxIntraISISec: q, meanIntraISISec: q, cv: 0.10, lv: 0.10,
        preGapSec: nil, postGapSec: nil, preRatioQ90: nil, postRatioQ90: nil,
        edgeContrastMinQ90: nil, edgeContrastGeomQ90: nil,
        anchorFamily: label.rawValue, anchorLockLevel: .strongCandidate,
        anchorBandLowerSec: 0.001, anchorBandUpperSec: 0.20, anchorBandSource: .structure,
        anchorContrastMinRequired: 1, anchorContrastGeomRequired: 1,
        refractorySuspectCount: 0, refractorySuspectAction: nil
    )
}

// 1 — Phase 3A (FIXED): burst-local-completion possibleBurst is a REVIEW OVERLAY, not an
// event-track carve. It overlays an overlapping tonic instead of splitting it.
@Test
func phase3a_burstLocalCompletionPossibleBurstOverlaysTonicNotCarves() {
    // Faithful to BurstLocalCompletionDetector emission (layer/action/gate/decisionPath).
    let completion = p3Candidate(
        id: "completion-1", label: .possibleBurst, start: 5, end: 7,
        layer: "burst_local_completion", gate: "burst_local_completion_pass",
        action: "accept_burst_ii_local_completion",
        decisionPath: "burst_local_completion;possible_burst_structural_definition=local_completion_burst_ii;auto_selected=true",
        priority: 2_665, selected: true
    )
    let tonic = p3Candidate(id: "tonic-1", label: .tonic, start: 2, end: 10, layer: "event_core_tonic_state")

    // Phase 3A routing: completion is now a boundary-review candidate on the REVIEW track.
    #expect(completion.isBurstBoundaryReviewCandidate == true)
    #expect(completion.arbitrationTrack == .review)

    let arbitrated = ClassicAnchorCandidateArbitrator.arbitrateBySemanticTrack([tonic, completion])
    let tonicResult = arbitrated.first { $0.id == "tonic-1" }
    // Tonic is RETAINED; the completion overlays it non-destructively (no event-track carving).
    #expect(tonicResult?.selectedForAuto == true)
    #expect(tonicResult?.selectionStatus.contains("possible_burst_review_overlay") == true)
}

// 2 — DESIRED/SAFE (already present): review-track possibleBurst overlays tonic, does not carve.
@Test
func phase3_reviewTrackPossibleBurstOverlaysTonic() {
    // action contains "review" → isBurstBoundaryReviewCandidate → arbitrationTrack == .review.
    let review = p3Candidate(
        id: "review-1", label: .possibleBurst, start: 5, end: 7,
        layer: "event_grammar_burst_episode", gate: "structural_seed_bridge_expansion_possible_review",
        action: "review_possible",
        decisionPath: "structural_seed_bridge_expansion_possible_review",
        priority: 400, selected: true
    )
    let tonic = p3Candidate(id: "tonic-2", label: .tonic, start: 2, end: 10, layer: "event_core_tonic_state")

    #expect(review.isBurstBoundaryReviewCandidate == true)
    #expect(review.arbitrationTrack == .review)

    let arbitrated = ClassicAnchorCandidateArbitrator.arbitrateBySemanticTrack([tonic, review])
    let tonicResult = arbitrated.first { $0.id == "tonic-2" }
    // Target model: tonic is RETAINED, possibleBurst is a non-destructive review overlay.
    #expect(tonicResult?.selectedForAuto == true)
    #expect(tonicResult?.selectionStatus.contains("possible_burst_review_overlay") == true)
}

// 3 — Phase 2A containment: weak / completion-class possibleBurst forms only a train-local burst
// seed and does NOT enter the dataset aggregate.
@Test
func phase3_completionPossibleBurstDoesNotPropagateAfterPhase2A() {
    // A structurally-supported (weak) possibleBurst that is the ONLY burst evidence → the summarize
    // weak-fallback path fires (usingWeakPossibleBurstSeeds == true).
    let weakPossible = p3Candidate(
        id: "weak-1", label: .possibleBurst, start: 2, end: 5,
        layer: "burst_local_completion", gate: "burst_local_completion_pass",
        action: "accept_burst_ii_local_completion",
        decisionPath: "burst_local_completion;possible_burst_structural_definition=local_completion_burst_ii;auto_selected=true",
        priority: 2_665, selected: true, q: 0.006
    )
    #expect(weakPossible.isWeakBurstStructuralSeedEvidence == true)

    let train = p3Train(repeating: 0.006, count: 12)
    let base = TrainAdaptiveBandResolver.resolve(train: train)
    let summary = StructuralSeedBandResolver.summarize(train: train, resolution: base, candidates: [weakPossible])

    // Train-local band forms (weak fallback), but it is flagged NON-aggregatable by Phase 2A.
    #expect(summary.burstSeedUpperSec != nil)
    #expect(summary.isBurstSeedDatasetAggregatable == false)

    // Containment: it does not reach the dataset aggregate.
    let resolution = StructuralSeedBandResolver.attachingSummary(to: base, summary: summary)
    let aggregate = StructuralDatasetSeedAggregator.aggregate(resolutions: [resolution])
    #expect(aggregate.burstSeedUpperSec == nil)
}

// 4 — Bounded, by-design: a canonical burst seed (as strict bridge expansion can produce when
// flank.strict + seed-purity + q90/q95 pass) REMAINS dataset-aggregatable, while a weak one does
// not. Documents the bounded strictPass canonicalization boundary; NOT changed in this slice.
// (See StructuralBridgeExpansionResolver.swift:238-240 label, :535-539 strictPass gate.)
@Test
func phase3_characterizesBridgeStrictPassCanonicalBypassAsBounded() {
    // Canonical (strong) burst seed — the downstream form a strictPass bridge canonicalization
    // yields; aggregatable by design.
    let canonical = StructuralSeedBandSummary(
        burstAnchorCount: 3, burstSeedUpperSec: 0.008, burstBridgeUpperSec: 0.012
        // isBurstSeedDatasetAggregatable defaults true
    )
    // Weak possible seed — non-aggregatable (Phase 2A).
    let weak = StructuralSeedBandSummary(
        burstAnchorCount: 2, burstSeedUpperSec: 0.030, burstBridgeUpperSec: 0.045,
        isBurstSeedDatasetAggregatable: false
    )
    let train = p3Train(repeating: 0.008, count: 10)
    let base = TrainAdaptiveBandResolver.resolve(train: train)
    let canonicalRes = StructuralSeedBandResolver.attachingSummary(to: base, summary: canonical)
    let weakRes = StructuralSeedBandResolver.attachingSummary(to: base, summary: weak)

    let aggregate = StructuralDatasetSeedAggregator.aggregate(resolutions: [canonicalRes, weakRes])
    // BOUNDED, DOCUMENTED: strong/canonical burst seed propagates; weak does not.
    #expect(aggregate.burstSeedUpperSec == 0.008)
}
