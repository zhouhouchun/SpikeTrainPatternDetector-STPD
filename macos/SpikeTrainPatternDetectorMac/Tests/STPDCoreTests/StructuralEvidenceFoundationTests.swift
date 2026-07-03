@testable import STPDCore
import Testing

// MARK: - Phase D4-0 — structural-evidence foundation tests
//
// Foundation only: span metrics, advisory verdict container, and thresholds. No evaluators, no
// detector wiring, no behavior change.

private func d40Close(_ a: Double?, _ b: Double?, tol: Double = 1e-9) -> Bool {
    switch (a, b) {
    case (nil, nil): return true
    case let (x?, y?): return abs(x - y) <= tol
    default: return false
    }
}

// 1 — quantiles / mean / median computed correctly (type-7), and median == q50.
@Test
func structuralEvidenceMetricsComputesQuantilesAndMean() {
    let m = ISISpanMetrics.compute(orderedValidISISec: [0.01, 0.02, 0.03, 0.04, 0.05])
    #expect(d40Close(m.minSec, 0.01))
    #expect(d40Close(m.maxSec, 0.05))
    #expect(d40Close(m.meanSec, 0.03))
    #expect(d40Close(m.q50Sec, 0.03))
    #expect(d40Close(m.q90Sec, 0.046))     // 0.04 + 0.6*(0.05-0.04)
    #expect(d40Close(m.q95Sec, 0.048))     // 0.04 + 0.8*(0.05-0.04)
    #expect(d40Close(m.q10Sec, 0.014))
    #expect(d40Close(m.q40Sec, 0.026))
    #expect(d40Close(m.medianSec, m.q50Sec))   // clarification 3: identical
    #expect(m.nISI == 5 && m.validISIValuesCount == 5 && m.nSpikes == 6)
}

// 2 — CV/CV2/LV match the shared STPDStatistics exactly (locks the formula) and are nil when < 2.
@Test
func structuralEvidenceCVCV2LVCorrectOrNilWhenInsufficient() {
    let values = [0.01, 0.03]
    let m = ISISpanMetrics.compute(orderedValidISISec: values)
    #expect(d40Close(m.cv, STPDStatistics.coefficientOfVariation(values)))
    #expect(d40Close(m.cv2, STPDStatistics.coefficientOfVariation2(values)))
    #expect(d40Close(m.lv, STPDStatistics.localVariation(values)))
    #expect(d40Close(m.cv2, 1.0))          // 2*|0.03-0.01|/(0.04)
    #expect(d40Close(m.lv, 0.75))          // 3*(0.02)^2/(0.04)^2
    #expect(d40Close(m.cv, 0.7071067811865476, tol: 1e-9))

    let single = ISISpanMetrics.compute(orderedValidISISec: [0.02])
    #expect(single.cv == nil && single.cv2 == nil && single.lv == nil)
}

// 3 — pre/post gap ratios are gap/q90.
@Test
func structuralEvidencePrePostGapRatiosAreCorrect() {
    let m = ISISpanMetrics.compute(
        orderedValidISISec: [0.005, 0.005, 0.005, 0.005, 0.005],
        preNeighborSec: 0.02, postNeighborSec: 0.03
    )
    #expect(d40Close(m.q90Sec, 0.005))
    #expect(d40Close(m.preGapSec, 0.02))
    #expect(d40Close(m.postGapSec, 0.03))
    #expect(d40Close(m.preRatioQ90, 4.0))   // 0.02 / 0.005
    #expect(d40Close(m.postRatioQ90, 6.0))  // 0.03 / 0.005
}

// 4 — edgeContrastGeomQ90 = sqrt(pre*post); edgeContrastMinQ90 = min(pre, post).
@Test
func structuralEvidenceEdgeContrastGeomIsSqrtOfFlankRatios() {
    let m = ISISpanMetrics.compute(
        orderedValidISISec: [0.005, 0.005, 0.005, 0.005, 0.005],
        preNeighborSec: 0.02, postNeighborSec: 0.03
    )
    #expect(d40Close(m.edgeContrastMinQ90, 4.0))
    #expect(d40Close(m.edgeContrastGeomQ90, (24.0).squareRoot()))   // sqrt(4*6)
}

// 5 — insufficient spans do not crash; reasons flagged.
@Test
func structuralEvidenceInsufficientSpansDoNotCrash() {
    let empty = ISISpanMetrics.compute(orderedValidISISec: [])
    #expect(empty.validISIValuesCount == 0 && empty.nSpikes == 0)
    #expect(empty.insufficientEvidenceReason == .noValidISI)
    #expect(empty.q50Sec == nil && empty.meanSec == nil && empty.cv == nil)

    let one = ISISpanMetrics.compute(orderedValidISISec: [0.01])
    #expect(one.insufficientEvidenceReason == .tooFewValidISI)
    #expect(d40Close(one.q50Sec, 0.01))     // single value quantile
    #expect(one.cv == nil && one.cv2 == nil && one.lv == nil)
    #expect(one.nSpikes == 2)
}

// 6 — invalid / artifact / non-finite ISI is flagged and excluded (not silently used).
@Test
func structuralEvidenceInvalidISIIsFlaggedNotSilentlyUsed() {
    // Non-finite via the pure path.
    let mixed = ISISpanMetrics.compute(orderedValidISISec: [0.02, .nan, 0.03])
    #expect(mixed.hasInvalidISI == true)
    #expect(mixed.validISIValuesCount == 2)
    #expect(mixed.nISI == 3)
    #expect(d40Close(mixed.meanSec, 0.025))

    // Sub-floor artifact via a real train span (0.0005 < minimumValidISISec 0.001).
    let train = SpikeTrain(name: "t", timestampsSec: [0.0, 0.0005, 0.0205, 0.0505, 0.0905])
    let span = ISISpan(trainID: "t", startISIIndex: 1, endISIIndex: 4)
    let m = ISISpanMetrics.from(train: train, span: span, thresholds: StructuralEvidenceThresholds())
    #expect(m.hasInvalidISI == true)              // 0.0005 excluded
    #expect(m.validISIValuesCount == 3)           // 0.02, 0.03, 0.04
    #expect(m.nISI == 4)                          // raw span slots
    #expect(d40Close(m.meanSec, 0.03, tol: 1e-9))
}

// 7 — verdict preserves both original and refined span/interval.
@Test
func structuralEvidenceVerdictPreservesOriginalAndRefined() {
    let s1 = ISISpan(trainID: "t", startISIIndex: 1, endISIIndex: 5, familyHint: .burst)
    let s2 = ISISpan(trainID: "t", startISIIndex: 2, endISIIndex: 4)
    let i1 = ModeISIInterval(family: .burst, lowerSec: 0.001, upperSec: 0.010,
                             scope: .trainLocal, provenance: .trainLocalDerived(sourceStatistic: "orig"))
    let i2 = ModeISIInterval(family: .burst, lowerSec: 0.002, upperSec: 0.008,
                             scope: .trainLocal, provenance: .trainLocalDerived(sourceStatistic: "refined"))
    let verdict = FamilyEvidenceVerdict(
        family: .burst, outcome: .refined, originalSpan: s1, refinedSpan: s2,
        originalInterval: i1, refinedInterval: i2
    )
    #expect(verdict.originalSpan == s1)
    #expect(verdict.refinedSpan == s2)
    #expect(verdict.originalInterval == i1)
    #expect(verdict.refinedInterval == i2)
}

// 8 — recommendations are independent advisory axes and imply no final authority. (The type has NO
// isSelected / mayCarve / mayPropagate field — a compile-time guarantee; here we assert independence.)
@Test
func structuralEvidenceRecommendationsDoNotImplyFinalAuthority() {
    let span = ISISpan(trainID: "t", startISIIndex: 1, endISIIndex: 3)
    let verdict = FamilyEvidenceVerdict(
        family: .tonic, outcome: .confirmed, originalSpan: span,
        selectionRecommendation: .recommendSelectable,     // would select…
        carvingRecommendation: .recommendOverlayOnly,      // …but only overlay, not carve…
        priorRecommendation: .recommendTrainLocalOnly,     // …and not propagate to dataset…
        reviewRequired: true                               // …and still flagged for review.
    )
    #expect(verdict.selectionRecommendation == .recommendSelectable)
    #expect(verdict.priorRecommendation == .recommendTrainLocalOnly)   // select ≠ propagate
    #expect(verdict.carvingRecommendation == .recommendOverlayOnly)    // select ≠ carve
    #expect(verdict.reviewRequired == true)                            // independent of selection
}

// 9A — factory-copied thresholds equal the live PUBLIC settings values.
@Test
func structuralEvidenceThresholdsFactoryCopiesPublicSettings() {
    let ca = ClassicAnchorSettings()
    let tca = StructuralEvidenceThresholds.from(classicAnchor: ca)
    #expect(tca.minimumValidISISec == ca.minValidISISec)
    #expect(tca.burstCoreMinISI == ca.burstCoreMinISI)
    #expect(tca.burstBridgeMaxCount == ca.burstBridgeMaxCount)
    #expect(tca.burstBridgeFractionMax == ca.burstBridgeFractionMax)
    #expect(tca.burstContrastMin == ca.burstContrastMin)
    #expect(tca.burstContrastGeomMin == ca.burstContrastGeomMin)

    let st = StatePatternDetectorSettings()
    let tst = StructuralEvidenceThresholds.from(state: st)
    #expect(tst.minimumValidISISec == st.minValidISISec)
    #expect(tst.burstSeedUpperSec == st.burstSeedUpperSec)
    #expect(tst.tonicMinSpikes == st.tonicMinSpikes)
    #expect(tst.tonicCVMax == st.tonicCVMax)
    #expect(tst.tonicCV2Max == st.tonicCV2Max)
    #expect(tst.tonicLVMax == st.tonicLVMax)
    #expect(tst.irregularTonicCVMax == st.irregularTonicCVMax)
    #expect(tst.irregularTonicCV2Max == st.irregularTonicCV2Max)
    #expect(tst.irregularTonicLVMax == st.irregularTonicLVMax)
    #expect(tst.tonicBridgeFractionMax == st.tonicBridgeFractionMax)
    #expect(tst.tonicBurstSeedFractionMax == st.tonicBurstSeedFractionMax)

    let pa = PauseDetectorSettings()
    let tpa = StructuralEvidenceThresholds.from(pause: pa)
    #expect(tpa.minimumValidISISec == pa.minValidISISec)
    #expect(tpa.globalMedianFactor == pa.globalMedianFactor)

    let ql = SpikeQualitySettings()
    let tql = StructuralEvidenceThresholds.from(quality: ql)
    #expect(tql.artifactThresholdSec == ql.artifactThresholdSec)
}

// 9B — D4 calibration defaults mirror the detector INLINE literals and are NOT touched by factories.
@Test
func structuralEvidenceThresholdsCalibrationDefaultsMirrorInlineLiterals() {
    let t = StructuralEvidenceThresholds()
    #expect(t.q95ExcessRatioMax == 1.35)
    #expect(t.leadingCoreRatioMax == 2.0)
    #expect(t.trailingCoreRatioMax == 3.0)
    #expect(t.oneSidedSeedPurityMin == 0.65)
    #expect(t.tonicLocalRatioLow == 0.55)
    #expect(t.tonicLocalRatioHigh == 1.85)
    #expect(t.tonicLocalRobustZMax == 3.0)
    #expect(t.tonicPauseGuardRatio == 1.15)
    #expect(t.pauseLocalPercentileTight == 0.90)
    #expect(t.pauseLocalRobustZTight == 2.5)

    // Factories do NOT source these calibration fields — they stay at the default.
    let fromSettings = StructuralEvidenceThresholds.from(classicAnchor: ClassicAnchorSettings())
    #expect(fromSettings.q95ExcessRatioMax == 1.35)
    #expect(fromSettings.leadingCoreRatioMax == 2.0)
    #expect(fromSettings.trailingCoreRatioMax == 3.0)
}

// 10 — scale-free: ISIs ×10 ⇒ sec metrics ×10; CV/CV2/LV and all ratios unchanged.
@Test
func structuralEvidenceScaleFreeMetricsScaleWithISIs() {
    let base = ISISpanMetrics.compute(
        orderedValidISISec: [0.01, 0.02, 0.03, 0.04, 0.05], preNeighborSec: 0.02, postNeighborSec: 0.03
    )
    let scaled = ISISpanMetrics.compute(
        orderedValidISISec: [0.10, 0.20, 0.30, 0.40, 0.50], preNeighborSec: 0.20, postNeighborSec: 0.30
    )
    // Seconds scale by 10.
    for kp in [\ISISpanMetrics.minSec, \ISISpanMetrics.maxSec, \ISISpanMetrics.meanSec,
               \ISISpanMetrics.q50Sec, \ISISpanMetrics.q90Sec, \ISISpanMetrics.q95Sec,
               \ISISpanMetrics.preGapSec, \ISISpanMetrics.postGapSec] {
        #expect(d40Close(base[keyPath: kp].map { $0 * 10 }, scaled[keyPath: kp], tol: 1e-9))
    }
    // Dimensionless metrics unchanged.
    #expect(d40Close(base.cv, scaled.cv))
    #expect(d40Close(base.cv2, scaled.cv2))
    #expect(d40Close(base.lv, scaled.lv))
    #expect(d40Close(base.preRatioQ90, scaled.preRatioQ90))
    #expect(d40Close(base.postRatioQ90, scaled.postRatioQ90))
    #expect(d40Close(base.edgeContrastGeomQ90, scaled.edgeContrastGeomQ90))
}
