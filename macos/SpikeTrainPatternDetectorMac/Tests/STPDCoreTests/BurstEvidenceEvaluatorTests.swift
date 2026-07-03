@testable import STPDCore
import Testing

// MARK: - Phase D4a — BurstEvidenceEvaluator tests
//
// Advisory evaluation of ONE supplied span vs a burst prior. Structural assertions on outcome
// precedence, two-stage BCB trim, flank/bridge/q95 gates, recommendations, and scale-invariance.

private func d4aTrain(_ name: String, isis: [Double]) -> SpikeTrain {
    var ts = [0.0]
    for isi in isis { ts.append((ts.last ?? 0) + isi) }
    return SpikeTrain(name: name, timestampsSec: ts)
}

private func d4aBurstPrior(lower: Double = 0.001, upper: Double, bridge: Double?) -> ModeISIInterval {
    ModeISIInterval(family: .burst, lowerSec: lower, upperSec: upper, scope: .trainLocal,
                    provenance: .trainLocalDerived(sourceStatistic: "d3"), bridgeUpperSec: bridge)
}

// span over isis[loIdx...hiIdx] (0-based in the isis array) => isiSec indices [loIdx+1 ... hiIdx+1].
private func d4aSpan(_ train: SpikeTrain, _ loIdx: Int, _ hiIdx: Int) -> ISISpan {
    ISISpan(trainID: train.id, startISIIndex: loIdx + 1, endISIIndex: hiIdx + 1, familyHint: .burst)
}

private func d4aSignal(_ verdict: FamilyEvidenceVerdict, _ key: String) -> EvidenceSignal? {
    verdict.signals.first { $0.key == key }
}

private let d4aThresholds = StructuralEvidenceThresholds()

// 1 — confirmed compact burst with two-sided flank contrast (no trim).
@Test
func burstEvidenceConfirmsCompactTwoSidedBurst() {
    let train = d4aTrain("t", isis: [0.05, 0.003, 0.003, 0.003, 0.003, 0.05])
    let v = BurstEvidenceEvaluator.evaluate(
        train: train, span: d4aSpan(train, 1, 4),
        prior: d4aBurstPrior(upper: 0.005, bridge: 0.012), thresholds: d4aThresholds)
    #expect(v.outcome == .confirmed)
    #expect(v.refinedSpan == nil)
    #expect(v.selectionRecommendation == .recommendSelectable)
    #expect(v.carvingRecommendation == .recommendMayCarve)
    #expect(v.priorRecommendation == .recommendTrainLocalOnly)   // never dataset from one span
    #expect(v.reviewRequired == false)
    #expect(d4aSignal(v, "burst.flank_contrast_two_sided")?.status == .pass)
}

// 2 — BCB-1 leading edge trim: a wide leading ISI is removed, compact core retained, => refined.
@Test
func burstEvidenceTrimsLeadingEdgeToRefined() {
    let train = d4aTrain("t", isis: [0.05, 0.02, 0.003, 0.003, 0.003, 0.003, 0.05])
    let v = BurstEvidenceEvaluator.evaluate(
        train: train, span: d4aSpan(train, 1, 5),   // 0.02 edge + 4 core
        prior: d4aBurstPrior(upper: 0.005, bridge: 0.012), thresholds: d4aThresholds)
    #expect(v.outcome == .refined)
    #expect(v.refinedSpan?.startISIIndex == 3)      // advanced past the 0.02 edge (isiSec index 2)
    #expect(v.refinedSpan?.endISIIndex == 6)
    #expect(v.selectionRecommendation == .recommendSelectable)
    let trim = d4aSignal(v, "burst.bcb_leading_edge_trim")
    #expect(trim?.status == .fail)                  // "edge incompatible; trimmed" (not a whole-burst fail)
    #expect((trim?.observedValue ?? 0) > d4aThresholds.leadingCoreRatioMax)
}

// 3 — BCB-1 trailing edge trim.
@Test
func burstEvidenceTrimsTrailingEdgeToRefined() {
    let train = d4aTrain("t", isis: [0.05, 0.003, 0.003, 0.003, 0.003, 0.03, 0.05])
    let v = BurstEvidenceEvaluator.evaluate(
        train: train, span: d4aSpan(train, 1, 5),   // 4 core + 0.03 edge
        prior: d4aBurstPrior(upper: 0.005, bridge: 0.012), thresholds: d4aThresholds)
    #expect(v.outcome == .refined)
    #expect(v.refinedSpan?.startISIIndex == 2)
    #expect(v.refinedSpan?.endISIIndex == 5)        // trailing 0.03 (isiSec index 6) removed
    #expect(d4aSignal(v, "burst.bcb_trailing_edge_trim")?.status == .fail)
}

// 4 — excessive bridge burden demotes to possibleReview (bridge ISIs within core-ratio => not trimmed).
@Test
func burstEvidenceExcessiveBridgeBurdenDemotesToPossibleReview() {
    let train = d4aTrain("t", isis: [0.05, 0.004, 0.004, 0.004, 0.006, 0.006, 0.006, 0.006, 0.006, 0.05])
    let v = BurstEvidenceEvaluator.evaluate(
        train: train, span: d4aSpan(train, 1, 8),   // 3 core + 5 bridge (> burstBridgeMaxCount 4)
        prior: d4aBurstPrior(upper: 0.005, bridge: 0.020), thresholds: d4aThresholds)
    #expect(v.outcome == .possibleReview)
    #expect(v.refinedSpan == nil)                   // nothing trimmed (bridge within ratio)
    #expect(d4aSignal(v, "burst.bridge_burden")?.status == .fail)
    #expect(v.reviewRequired == true)
}

// 5 — one-sided / endpoint support is possibleReview, not confirmed.
@Test
func burstEvidenceOneSidedEndpointIsPossibleReview() {
    // Burst at the very start of the train => no pre-flank.
    let train = d4aTrain("t", isis: [0.003, 0.003, 0.003, 0.003, 0.05])
    let v = BurstEvidenceEvaluator.evaluate(
        train: train, span: d4aSpan(train, 0, 3),
        prior: d4aBurstPrior(upper: 0.005, bridge: 0.012), thresholds: d4aThresholds)
    #expect(v.outcome == .possibleReview)
    #expect(d4aSignal(v, "burst.flank_one_sided")?.status == .pass)
    #expect(d4aSignal(v, "burst.flank_contrast_two_sided")?.status == .fail)
    #expect(v.selectionRecommendation == .recommendReviewOnly)
    #expect(v.carvingRecommendation == .recommendOverlayOnly)
}

// 6 — q95 excess demotes (overflow within core-ratio, so not trimmed, but incompatible with the band).
@Test
func burstEvidenceQ95ExcessDemotesToPossibleReview() {
    let train = d4aTrain("t", isis: [0.05, 0.006, 0.006, 0.006, 0.006, 0.016, 0.017, 0.05])
    let v = BurstEvidenceEvaluator.evaluate(
        train: train, span: d4aSpan(train, 1, 6),   // 4 core + two overflow (<3x core, not trimmed)
        prior: d4aBurstPrior(upper: 0.008, bridge: 0.012), thresholds: d4aThresholds)
    #expect(v.outcome == .possibleReview)
    #expect(v.refinedSpan == nil)                   // 0.016/0.006, 0.017/0.006 < trailingCoreRatioMax 3.0
    #expect(d4aSignal(v, "burst.q95_compatibility")?.status == .fail)
}

// 7 — outcome precedence: wrong family / invalid prior => rejected; too few => insufficient;
// enough ISIs but no compact core => rejected.
@Test
func burstEvidencePrecedenceRejectsVsInsufficient() {
    let train = d4aTrain("t", isis: [0.003, 0.003, 0.003, 0.003])

    // wrong family
    let tonicPrior = ModeISIInterval(family: .tonic, lowerSec: 0.02, upperSec: 0.06,
                                     scope: .trainLocal, provenance: .trainLocalDerived(sourceStatistic: "x"))
    let wrongFamily = BurstEvidenceEvaluator.evaluate(
        train: train, span: d4aSpan(train, 0, 3), prior: tonicPrior, thresholds: d4aThresholds)
    #expect(wrongFamily.outcome == .rejected)

    // invalid prior (lower > upper)
    let invalidPrior = ModeISIInterval(family: .burst, lowerSec: 0.06, upperSec: 0.02,
                                       scope: .trainLocal, provenance: .trainLocalDerived(sourceStatistic: "x"))
    let invalid = BurstEvidenceEvaluator.evaluate(
        train: train, span: d4aSpan(train, 0, 3), prior: invalidPrior, thresholds: d4aThresholds)
    #expect(invalid.outcome == .rejected)

    // too few valid ISIs (1 < burstCoreMinISI 2) => insufficient (NOT rejected)
    let tiny = d4aTrain("s", isis: [0.003])
    let insufficient = BurstEvidenceEvaluator.evaluate(
        train: tiny, span: d4aSpan(tiny, 0, 0),
        prior: d4aBurstPrior(upper: 0.005, bridge: 0.012), thresholds: d4aThresholds)
    #expect(insufficient.outcome == .insufficientEvidence)
    #expect(d4aSignal(insufficient, "burst.insufficient_evidence") != nil)

    // enough ISIs but none in core => rejected
    let noCore = d4aTrain("n", isis: [0.05, 0.05, 0.05])
    let rejectedNoCore = BurstEvidenceEvaluator.evaluate(
        train: noCore, span: d4aSpan(noCore, 0, 2),
        prior: d4aBurstPrior(upper: 0.005, bridge: 0.012), thresholds: d4aThresholds)
    #expect(rejectedNoCore.outcome == .rejected)
    #expect(d4aSignal(rejectedNoCore, "burst.core_compactness")?.status == .fail)
}

// 8 — scale-invariance: ×10 ISIs + prior + floor => identical outcome, recommendations, refinedSpan
// indices, and ratio-valued observed signals.
@Test
func burstEvidenceIsScaleInvariant() {
    let isis: [Double] = [0.05, 0.02, 0.003, 0.003, 0.003, 0.003, 0.05]
    let base = BurstEvidenceEvaluator.evaluate(
        train: d4aTrain("b", isis: isis), span: ISISpan(trainID: "b", startISIIndex: 2, endISIIndex: 6, familyHint: .burst),
        prior: d4aBurstPrior(lower: 0.001, upper: 0.005, bridge: 0.012),
        thresholds: StructuralEvidenceThresholds(minimumValidISISec: 0.001))
    let scaled = BurstEvidenceEvaluator.evaluate(
        train: d4aTrain("b", isis: isis.map { $0 * 10 }), span: ISISpan(trainID: "b", startISIIndex: 2, endISIIndex: 6, familyHint: .burst),
        prior: d4aBurstPrior(lower: 0.010, upper: 0.050, bridge: 0.120),
        thresholds: StructuralEvidenceThresholds(minimumValidISISec: 0.010))

    #expect(base.outcome == scaled.outcome)
    #expect(base.selectionRecommendation == scaled.selectionRecommendation)
    #expect(base.carvingRecommendation == scaled.carvingRecommendation)
    #expect(base.priorRecommendation == scaled.priorRecommendation)
    #expect(base.refinedSpan?.startISIIndex == scaled.refinedSpan?.startISIIndex)
    #expect(base.refinedSpan?.endISIIndex == scaled.refinedSpan?.endISIIndex)
    // ratio-valued observed signals unchanged after scaling.
    for key in ["burst.bcb_leading_edge_trim", "burst.flank_contrast_two_sided", "burst.q95_compatibility"] {
        let a = d4aSignal(base, key)?.observedValue
        let b = d4aSignal(scaled, key)?.observedValue
        switch (a, b) {
        case (nil, nil): #expect(Bool(true))
        case let (x?, y?): #expect(abs(x - y) <= 1e-9)
        default: #expect(Bool(false))
        }
    }
}
