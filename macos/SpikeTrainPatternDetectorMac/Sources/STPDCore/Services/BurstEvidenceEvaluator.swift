import Foundation

// MARK: - Phase D4a — BurstEvidenceEvaluator
//
// Pure, advisory. Evaluates ONE supplied ISI span against a D3 burst `ModeISIInterval` prior and
// returns a `FamilyEvidenceVerdict` with RECOMMENDATIONS only. It does NOT search for candidates,
// build ClassicAnchorCandidate, select labels, carve tonic, or propagate dataset priors.
//
// Two-stage design (correction 2): BCB-1 edge trimming happens FIRST (Stage A) so a boundary ISI the
// evaluator correctly trims cannot first poison q95 / bridge burden / flank contrast, which are then
// recomputed on the RETAINED span (Stage B). Trim guard (correction 3): an edge is trimmed only when it
// is non-core AND its ratio to the core median exceeds the BCB threshold — outside-bridge values that
// do not exceed the core ratio are NOT auto-trimmed; they demote via bridge/q95 signals instead.
public enum BurstEvidenceEvaluator {
    public static func evaluate(
        train: SpikeTrain,
        span: ISISpan,
        prior: ModeISIInterval,
        thresholds: StructuralEvidenceThresholds
    ) -> FamilyEvidenceVerdict {
        let floor = max(0, thresholds.minimumValidISISec)
        let upper = prior.upperSec
        let coreTol = tolerance(upper)

        // --- Eligibility: wrong family / invalid prior => rejected (independent of ISI count).
        guard prior.family == .burst, prior.isValid else {
            let signal = EvidenceSignal(
                key: "burst.eligibility", status: .fail, role: .eligibility,
                message: prior.family != .burst ? "prior family is not burst" : "prior interval is invalid")
            return reject(span: span, prior: prior, signals: [signal],
                          path: prior.family != .burst ? "eligibility=wrong_family" : "eligibility=invalid_prior")
        }

        // --- Ordered valid indexed ISIs in the span (index 0 is the structural placeholder).
        let indexed = orderedValidISIs(train: train, span: span, floor: floor)
        let validCount = indexed.count

        // --- Insufficient evidence: no valid ISI / fewer than the min core count (correction 1).
        if validCount == 0 { return insufficient(span: span, prior: prior, reason: .noValidISI) }
        if validCount < thresholds.burstCoreMinISI { return insufficient(span: span, prior: prior, reason: .tooFewValidISI) }

        // --- Compact core = valid ISIs <= prior.upperSec.
        let coreValues = indexed.filter { $0.value <= upper + coreTol }.map(\.value)
        let coreCount = coreValues.count
        let eligibilitySignal = EvidenceSignal(
            key: "burst.eligibility", status: .pass, role: .eligibility,
            observedValue: Double(validCount), requiredValue: Double(thresholds.burstCoreMinISI),
            message: "burst prior; \(validCount) valid ISIs")
        guard coreCount >= thresholds.burstCoreMinISI else {
            // Enough evidence but not burst-like (no compact core) => rejected (correction 1).
            let signal = EvidenceSignal(
                key: "burst.core_compactness", status: .fail, role: .compactness,
                observedValue: Double(coreCount), requiredValue: Double(thresholds.burstCoreMinISI),
                message: "no compact core inside prior.upperSec")
            return reject(span: span, prior: prior, signals: [eligibilitySignal, signal], path: "core=absent")
        }
        let coreMedian = SortedFiniteSample(coreValues).quantile(0.5) ?? coreValues[0]
        let coreSignal = EvidenceSignal(
            key: "burst.core_compactness", status: .pass, role: .compactness,
            observedValue: Double(coreCount), requiredValue: Double(thresholds.burstCoreMinISI),
            message: "compact core of \(coreCount) ISIs")

        // --- Stage A: BCB-1 edge trim (never trim core; guard = non-core AND ratio-to-core exceeded).
        var startK = 0
        while startK < indexed.count {
            let value = indexed[startK].value
            if value <= upper + coreTol { break }                                   // core: stop
            if coreMedian > 0, value / coreMedian > thresholds.leadingCoreRatioMax { startK += 1 } else { break }
        }
        var endM = 0
        while endM < indexed.count - startK {
            let value = indexed[indexed.count - 1 - endM].value
            if value <= upper + coreTol { break }
            if coreMedian > 0, value / coreMedian > thresholds.trailingCoreRatioMax { endM += 1 } else { break }
        }
        let retained = Array(indexed[startK ..< (indexed.count - endM)])
        // Core is never trimmed (loops break at the first core ISI) and >= 1 core ISI exists, so
        // `retained` is non-empty; guard defensively rather than force-unwrap.
        guard let firstRetained = retained.first, let lastRetained = retained.last else {
            return reject(span: span, prior: prior, signals: [eligibilitySignal, coreSignal], path: "core=trimmed_empty")
        }
        let trimmedLeading = startK > 0
        let trimmedTrailing = endM > 0
        let trimmed = trimmedLeading || trimmedTrailing
        let retainedSpan = ISISpan(
            trainID: span.trainID, startISIIndex: firstRetained.index,
            endISIIndex: lastRetained.index, familyHint: .burst)
        let refinedSpan: ISISpan? = trimmed ? retainedSpan : nil

        let leadingTrimSignal = EvidenceSignal(
            key: "burst.bcb_leading_edge_trim", status: trimmedLeading ? .fail : .pass, role: .boundary,
            observedValue: trimmedLeading ? indexed[0].value / coreMedian : nil,
            requiredValue: thresholds.leadingCoreRatioMax,
            message: trimmedLeading ? "leading edge incompatible with core; trimmed" : "no leading edge trim needed")
        let trailingTrimSignal = EvidenceSignal(
            key: "burst.bcb_trailing_edge_trim", status: trimmedTrailing ? .fail : .pass, role: .boundary,
            observedValue: trimmedTrailing ? indexed[indexed.count - 1].value / coreMedian : nil,
            requiredValue: thresholds.trailingCoreRatioMax,
            message: trimmedTrailing ? "trailing edge incompatible with core; trimmed" : "no trailing edge trim needed")

        // --- Stage B: recompute all downstream gates on the RETAINED span.
        let metricsB = ISISpanMetrics.from(train: train, span: retainedSpan, thresholds: thresholds)
        let retainedValues = retained.map(\.value)

        // Flank contrast (retained-span q90-based).
        let preStrong = (metricsB.preRatioQ90 ?? 0) >= thresholds.burstContrastMin
        let postStrong = (metricsB.postRatioQ90 ?? 0) >= thresholds.burstContrastMin
        let twoSidedStrong = preStrong && postStrong && (metricsB.edgeContrastGeomQ90 ?? 0) >= thresholds.burstContrastGeomMin
        let hasSomeFlank = preStrong || postStrong
        let flankTwoSidedSignal = EvidenceSignal(
            key: "burst.flank_contrast_two_sided", status: twoSidedStrong ? .pass : .fail, role: .boundary,
            observedValue: metricsB.edgeContrastGeomQ90, requiredValue: thresholds.burstContrastGeomMin,
            message: twoSidedStrong ? "two-sided flank contrast" : "insufficient two-sided flank contrast")
        let flankOneSidedSignal = EvidenceSignal(
            key: "burst.flank_one_sided", status: (!twoSidedStrong && hasSomeFlank) ? .pass : .notApplicable,
            role: .boundary, observedValue: preStrong ? metricsB.preRatioQ90 : metricsB.postRatioQ90,
            requiredValue: thresholds.burstContrastMin,
            message: (!twoSidedStrong && hasSomeFlank) ? "one-sided / endpoint flank support only" : "not one-sided")

        // Bridge burden (retained span). Overflow beyond bridge is NOT trimmed here (correction 3) — it
        // makes the bridge incompatible and demotes.
        let hasBridge = prior.bridgeUpperSec != nil
        let bridgeUpper = prior.bridgeUpperSec ?? upper
        let bridgeTol = tolerance(bridgeUpper)
        let bridgeCount = retainedValues.filter { $0 > upper + coreTol && $0 <= bridgeUpper + bridgeTol }.count
        let overflowCount = retainedValues.filter { $0 > bridgeUpper + bridgeTol }.count
        let bridgeFraction = retainedValues.isEmpty ? 0 : Double(bridgeCount) / Double(retainedValues.count)
        let bridgeOK: Bool = {
            guard hasBridge else { return overflowCount == 0 }
            return bridgeCount <= thresholds.burstBridgeMaxCount
                && bridgeFraction <= thresholds.burstBridgeFractionMax
                && overflowCount == 0
        }()
        let bridgeSignal = EvidenceSignal(
            key: "burst.bridge_burden", status: hasBridge ? (bridgeOK ? .pass : .fail) : (bridgeOK ? .notApplicable : .fail),
            role: .priorCompatibility, observedValue: bridgeFraction, requiredValue: thresholds.burstBridgeFractionMax,
            message: bridgeOK ? "bridge burden within limits"
                : (overflowCount > 0 ? "ISIs beyond bridge upper (overflow)" : "excessive bridge burden"))

        // q95 compatibility (retained span q95 vs the bridge/core ceiling).
        let q95Excess: Double? = {
            guard let q95 = metricsB.q95Sec, bridgeUpper > 0 else { return nil }
            return q95 / bridgeUpper
        }()
        let q95OK = (q95Excess ?? 0) <= thresholds.q95ExcessRatioMax
        let q95Signal = EvidenceSignal(
            key: "burst.q95_compatibility", status: q95OK ? .pass : .fail, role: .priorCompatibility,
            observedValue: q95Excess, requiredValue: thresholds.q95ExcessRatioMax,
            message: q95OK ? "q95 within bridge compatibility" : "q95 exceeds bridge compatibility")

        let signals = [eligibilitySignal, coreSignal, leadingTrimSignal, trailingTrimSignal,
                       flankTwoSidedSignal, flankOneSidedSignal, bridgeSignal, q95Signal]

        // --- Classify (after core exists): confirmed / refined / possibleReview.
        if twoSidedStrong && bridgeOK && q95OK {
            let outcome: FamilyEvidenceOutcome = trimmed ? .refined : .confirmed
            return FamilyEvidenceVerdict(
                family: .burst, outcome: outcome, originalSpan: span, refinedSpan: refinedSpan,
                originalInterval: prior, refinedInterval: nil, signals: signals,
                selectionRecommendation: .recommendSelectable, carvingRecommendation: .recommendMayCarve,
                priorRecommendation: .recommendTrainLocalOnly, reviewRequired: false,
                decisionPath: "burst=\(outcome.rawValue);flank=two_sided;bridge=ok;q95=ok" + (trimmed ? ";bcb=trimmed" : ""))
        }

        var reasons: [String] = []
        if !twoSidedStrong { reasons.append(hasSomeFlank ? "flank=one_sided" : "flank=weak") }
        if !bridgeOK { reasons.append("bridge=incompatible") }
        if !q95OK { reasons.append("q95=excess") }
        if trimmed { reasons.append("bcb=trimmed") }
        let carving: CarvingAuthorityRecommendation = (twoSidedStrong || hasSomeFlank) ? .recommendOverlayOnly : .recommendNoCarve
        let priorRec: PriorAuthorityRecommendation = (twoSidedStrong || hasSomeFlank) ? .recommendTrainLocalOnly : .recommendAuditOnly
        return FamilyEvidenceVerdict(
            family: .burst, outcome: .possibleReview, originalSpan: span, refinedSpan: refinedSpan,
            originalInterval: prior, refinedInterval: nil, signals: signals,
            selectionRecommendation: .recommendReviewOnly, carvingRecommendation: carving,
            priorRecommendation: priorRec, reviewRequired: true,
            decisionPath: "burst=possible_review;" + reasons.joined(separator: ";"))
    }

    // MARK: Helpers

    private static func tolerance(_ value: Double) -> Double { max(1e-12, abs(value) * 1e-6) }

    private static func orderedValidISIs(train: SpikeTrain, span: ISISpan, floor: Double) -> [(index: Int, value: Double)] {
        let isi = train.isiSec
        let lo = max(1, span.startISIIndex)
        let hi = min(isi.count - 1, span.endISIIndex)
        guard hi >= lo else { return [] }
        var result: [(index: Int, value: Double)] = []
        for index in lo...hi {
            guard let value = isi[index], value.isFinite, value >= floor else { continue }
            result.append((index, value))
        }
        return result
    }

    private static func reject(span: ISISpan, prior: ModeISIInterval, signals: [EvidenceSignal], path: String) -> FamilyEvidenceVerdict {
        FamilyEvidenceVerdict(
            family: .burst, outcome: .rejected, originalSpan: span, originalInterval: prior, signals: signals,
            selectionRecommendation: .recommendNotSelectable, carvingRecommendation: .recommendNoCarve,
            priorRecommendation: .recommendAuditOnly, reviewRequired: false, decisionPath: "burst=rejected;" + path)
    }

    private static func insufficient(span: ISISpan, prior: ModeISIInterval, reason: InsufficientEvidenceReason) -> FamilyEvidenceVerdict {
        let signal = EvidenceSignal(
            key: "burst.insufficient_evidence", status: .insufficientEvidence, role: .eligibility, message: reason.message)
        return FamilyEvidenceVerdict(
            family: .burst, outcome: .insufficientEvidence, originalSpan: span, originalInterval: prior, signals: [signal],
            selectionRecommendation: .noRecommendation, carvingRecommendation: .noRecommendation,
            priorRecommendation: .noRecommendation, reviewRequired: false, decisionPath: "burst=insufficient;\(reason.rawValue)")
    }
}
