@testable import STPDCore
import Testing

// MARK: - Phase D4b — TonicEvidenceEvaluator tests
//
// Advisory evaluation of ONE supplied span vs a tonic prior. Structural assertions on short-run
// (2-4 ISI) compactness, >=5 ISI CV/CV2/LV, conservative <=1-edge boundary trim, in-prior fraction
// tiers (<50 reject / 50-80 review / >=80 confirm), burst contamination, and scale-invariance.

private func d4bTrain(_ name: String, isis: [Double]) -> SpikeTrain {
    var ts = [0.0]
    for isi in isis { ts.append((ts.last ?? 0) + isi) }
    return SpikeTrain(name: name, timestampsSec: ts)
}

private func d4bTonicPrior(lower: Double, upper: Double) -> ModeISIInterval {
    ModeISIInterval(family: .tonic, lowerSec: lower, upperSec: upper, scope: .trainLocal,
                    provenance: .trainLocalDerived(sourceStatistic: "d3"))
}

// span over isis[loIdx...hiIdx] (0-based) => isiSec indices [loIdx+1 ... hiIdx+1].
private func d4bSpan(_ train: SpikeTrain, _ loIdx: Int, _ hiIdx: Int) -> ISISpan {
    ISISpan(trainID: train.id, startISIIndex: loIdx + 1, endISIIndex: hiIdx + 1, familyHint: .tonic)
}

private func d4bSignal(_ verdict: FamilyEvidenceVerdict, _ key: String) -> EvidenceSignal? {
    verdict.signals.first { $0.key == key }
}

private let d4bThresholds = StructuralEvidenceThresholds()

// 1 — 3-ISI compact in-prior tonic confirms (short-run recall path; no tonicMinSpikes gate).
@Test
func tonicEvidenceConfirmsShortCompactRun() {
    let train = d4bTrain("t", isis: [0.04, 0.045, 0.042])
    let v = TonicEvidenceEvaluator.evaluate(
        train: train, span: d4bSpan(train, 0, 2),
        prior: d4bTonicPrior(lower: 0.02, upper: 0.06), thresholds: d4bThresholds)
    #expect(v.outcome == .confirmed)
    #expect(v.selectionRecommendation == .recommendSelectable)
    #expect(v.carvingRecommendation == .recommendNoCarve)
    #expect(v.priorRecommendation == .recommendTrainLocalOnly)
    #expect(v.refinedSpan == nil)
    #expect(d4bSignal(v, "tonic.short_run_compactness")?.status == .pass)
}

// 2 — 2-ISI compact run is possibleReview, not confirmed (borderline).
@Test
func tonicEvidenceTwoISIRunIsPossibleReview() {
    let train = d4bTrain("t", isis: [0.04, 0.045])
    let v = TonicEvidenceEvaluator.evaluate(
        train: train, span: d4bSpan(train, 0, 1),
        prior: d4bTonicPrior(lower: 0.02, upper: 0.06), thresholds: d4bThresholds)
    #expect(v.outcome == .possibleReview)
    #expect(v.selectionRecommendation == .recommendReviewOnly)
    #expect(v.priorRecommendation == .recommendTrainLocalOnly)
}

// 3 — long regular run confirmed by CV/CV2/LV.
@Test
func tonicEvidenceLongRegularRunConfirms() {
    let train = d4bTrain("t", isis: [0.040, 0.042, 0.041, 0.043, 0.040, 0.042])
    let v = TonicEvidenceEvaluator.evaluate(
        train: train, span: d4bSpan(train, 0, 5),
        prior: d4bTonicPrior(lower: 0.02, upper: 0.06), thresholds: d4bThresholds)
    #expect(v.outcome == .confirmed)
    #expect(d4bSignal(v, "tonic.regularity_cv")?.status == .pass)
    #expect(d4bSignal(v, "tonic.regularity_cv2")?.status == .pass)
    #expect(d4bSignal(v, "tonic.regularity_lv")?.status == .pass)
    #expect(d4bSignal(v, "tonic.short_run_compactness")?.status == .notApplicable)
}

// 4 — long but irregular (classic CV fails) in-prior run is possibleReview, not rejected.
@Test
func tonicEvidenceLongIrregularRunIsPossibleReview() {
    let train = d4bTrain("t", isis: [0.025, 0.065, 0.030, 0.060, 0.028, 0.063])
    let v = TonicEvidenceEvaluator.evaluate(
        train: train, span: d4bSpan(train, 0, 5),
        prior: d4bTonicPrior(lower: 0.02, upper: 0.07), thresholds: d4bThresholds)
    #expect(v.outcome == .possibleReview)
    #expect(d4bSignal(v, "tonic.regularity_cv")?.status == .fail)
    #expect(d4bSignal(v, "tonic.burst_contamination")?.status == .pass)   // not contamination
}

// 5 — interior burst-like packet is rejected as contamination.
@Test
func tonicEvidenceInteriorBurstPacketRejected() {
    let train = d4bTrain("t", isis: [0.04, 0.04, 0.003, 0.003, 0.04, 0.04])
    let v = TonicEvidenceEvaluator.evaluate(
        train: train, span: d4bSpan(train, 0, 5),
        prior: d4bTonicPrior(lower: 0.02, upper: 0.06), thresholds: d4bThresholds)
    #expect(v.outcome == .rejected)
    #expect(d4bSignal(v, "tonic.burst_contamination")?.status == .fail)
    #expect(v.selectionRecommendation == .recommendNotSelectable)
    #expect(v.priorRecommendation == .recommendAuditOnly)
}

// 6 — one out-of-prior edge is trimmed => refined.
@Test
func tonicEvidenceOneEdgeTrimGivesRefined() {
    let train = d4bTrain("t", isis: [0.10, 0.04, 0.045, 0.042, 0.043])
    let v = TonicEvidenceEvaluator.evaluate(
        train: train, span: d4bSpan(train, 0, 4),
        prior: d4bTonicPrior(lower: 0.02, upper: 0.06), thresholds: d4bThresholds)
    #expect(v.outcome == .refined)
    #expect(v.refinedSpan?.startISIIndex == 2)     // 0.10 (isiSec index 1) trimmed
    #expect(v.refinedSpan?.endISIIndex == 5)
    #expect(d4bSignal(v, "tonic.boundary_edge_trim")?.status == .fail)
    #expect(v.selectionRecommendation == .recommendSelectable)
}

// 7 — multiple out-of-prior edges are NOT all trimmed; remaining incompatibility demotes (no
// carve-down-to-tiny-core confirm).
@Test
func tonicEvidenceDoesNotTrimMultipleEdges() {
    let train = d4bTrain("t", isis: [0.10, 0.11, 0.04, 0.045, 0.042])
    let v = TonicEvidenceEvaluator.evaluate(
        train: train, span: d4bSpan(train, 0, 4),
        prior: d4bTonicPrior(lower: 0.02, upper: 0.06), thresholds: d4bThresholds)
    #expect(v.outcome == .possibleReview)          // 3/4 in-prior after ONE edge trim
    #expect(v.refinedSpan != nil)                  // exactly one leading edge trimmed
    #expect(d4bSignal(v, "tonic.prior_compatibility")?.status == .fail)
}

// 8 — retained in-prior fraction < 50% => rejected.
@Test
func tonicEvidenceWholesaleIncompatibleRejected() {
    let train = d4bTrain("t", isis: [0.10, 0.11, 0.12, 0.04])
    let v = TonicEvidenceEvaluator.evaluate(
        train: train, span: d4bSpan(train, 0, 3),
        prior: d4bTonicPrior(lower: 0.02, upper: 0.06), thresholds: d4bThresholds)
    #expect(v.outcome == .rejected)                // after one trim: 1/3 in-prior
    #expect(d4bSignal(v, "tonic.prior_compatibility")?.status == .fail)
}

// 9 — retained in-prior fraction 50-80% => possibleReview (interior incompatibility, no trim).
@Test
func tonicEvidencePartialCompatibilityIsPossibleReview() {
    let train = d4bTrain("t", isis: [0.04, 0.10, 0.045, 0.11, 0.042])
    let v = TonicEvidenceEvaluator.evaluate(
        train: train, span: d4bSpan(train, 0, 4),
        prior: d4bTonicPrior(lower: 0.02, upper: 0.06), thresholds: d4bThresholds)
    #expect(v.outcome == .possibleReview)          // 3/5 = 0.60 in-prior
    #expect(v.refinedSpan == nil)                  // no edges out (first/last in-prior)
    #expect(d4bSignal(v, "tonic.prior_compatibility")?.status == .fail)
}

// 10 — scale-invariance: ISIs + prior + floor ×10 => identical outcome/recommendations/refinedSpan
// indices and ratio-valued signals.
@Test
func tonicEvidenceIsScaleInvariant() {
    let isis: [Double] = [0.10, 0.04, 0.045, 0.042, 0.043]
    let base = TonicEvidenceEvaluator.evaluate(
        train: d4bTrain("b", isis: isis), span: ISISpan(trainID: "b", startISIIndex: 1, endISIIndex: 5, familyHint: .tonic),
        prior: d4bTonicPrior(lower: 0.02, upper: 0.06),
        thresholds: StructuralEvidenceThresholds(minimumValidISISec: 0.001))
    let scaled = TonicEvidenceEvaluator.evaluate(
        train: d4bTrain("b", isis: isis.map { $0 * 10 }), span: ISISpan(trainID: "b", startISIIndex: 1, endISIIndex: 5, familyHint: .tonic),
        prior: d4bTonicPrior(lower: 0.20, upper: 0.60),
        thresholds: StructuralEvidenceThresholds(minimumValidISISec: 0.010))

    #expect(base.outcome == scaled.outcome)
    #expect(base.outcome == .refined)
    #expect(base.selectionRecommendation == scaled.selectionRecommendation)
    #expect(base.priorRecommendation == scaled.priorRecommendation)
    #expect(base.refinedSpan?.startISIIndex == scaled.refinedSpan?.startISIIndex)
    #expect(base.refinedSpan?.endISIIndex == scaled.refinedSpan?.endISIIndex)
    for key in ["tonic.prior_compatibility", "tonic.burst_contamination"] {
        let a = d4bSignal(base, key)?.observedValue
        let b = d4bSignal(scaled, key)?.observedValue
        switch (a, b) {
        case (nil, nil): #expect(Bool(true))
        case let (x?, y?): #expect(abs(x - y) <= 1e-9)
        default: #expect(Bool(false))
        }
    }
}

// 11 — wrong family / invalid prior => rejected; no valid ISI / too few => insufficient.
@Test
func tonicEvidenceEligibilityAndInsufficiency() {
    let train = d4bTrain("t", isis: [0.04, 0.045, 0.042])

    let burstPrior = ModeISIInterval(family: .burst, lowerSec: 0.001, upperSec: 0.01,
                                     scope: .trainLocal, provenance: .trainLocalDerived(sourceStatistic: "x"))
    #expect(TonicEvidenceEvaluator.evaluate(train: train, span: d4bSpan(train, 0, 2), prior: burstPrior, thresholds: d4bThresholds).outcome == .rejected)

    let invalidPrior = ModeISIInterval(family: .tonic, lowerSec: 0.06, upperSec: 0.02,
                                       scope: .trainLocal, provenance: .trainLocalDerived(sourceStatistic: "x"))
    #expect(TonicEvidenceEvaluator.evaluate(train: train, span: d4bSpan(train, 0, 2), prior: invalidPrior, thresholds: d4bThresholds).outcome == .rejected)

    // one valid ISI => too few => insufficient (NOT rejected)
    let tiny = d4bTrain("s", isis: [0.04])
    let tooFew = TonicEvidenceEvaluator.evaluate(
        train: tiny, span: d4bSpan(tiny, 0, 0), prior: d4bTonicPrior(lower: 0.02, upper: 0.06), thresholds: d4bThresholds)
    #expect(tooFew.outcome == .insufficientEvidence)
    #expect(d4bSignal(tooFew, "tonic.insufficient_evidence") != nil)

    // span out of range => no valid ISI => insufficient
    let noISI = TonicEvidenceEvaluator.evaluate(
        train: train, span: ISISpan(trainID: "t", startISIIndex: 20, endISIIndex: 25, familyHint: .tonic),
        prior: d4bTonicPrior(lower: 0.02, upper: 0.06), thresholds: d4bThresholds)
    #expect(noISI.outcome == .insufficientEvidence)
}
