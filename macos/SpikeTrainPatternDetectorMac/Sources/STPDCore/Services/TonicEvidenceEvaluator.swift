import Foundation

// MARK: - Phase D4b — TonicEvidenceEvaluator
//
// Pure, advisory. Evaluates ONE supplied ISI span/run against a D3 tonic `ModeISIInterval` prior and
// returns a `FamilyEvidenceVerdict` with RECOMMENDATIONS only. No tonic candidate search, no
// StatePatternDetector reproduction, no label selection, no carving, no dataset propagation.
//
// Deliberately supports 2-4 ISI SHORT runs (the tonic-recall gap that `detectTonic`'s tonicMinSpikes=5
// hard gate drops) via range-ratio compactness; >=5 ISI runs use CV/CV2/LV (STPDStatistics parity).
// Everything is SCALE-FREE: compactness is relative to the run median, CV/CV2/LV are dimensionless,
// and prior compatibility / burst contamination are expressed RELATIVE TO THE PRIOR (never via an
// absolute burst-seed cutoff). The only absolute is `minimumValidISISec` (QC floor).
public enum TonicEvidenceEvaluator {
    // D4b calibration defaults (documented; no public settings source — see correction-2 tiers).
    private static let inPriorRejectFraction = 0.50     // < 0.50 in-prior => rejected
    private static let inPriorReviewFraction = 0.80     // [0.50, 0.80) => possibleReview; >= 0.80 may confirm
    private static let shortRunConfirmMinISI = 3        // 2-ISI is borderline (possibleReview); 3-4 may confirm
    private static let longRunMinISI = 5                // >= 5 ISIs => CV/CV2/LV path
    private static let burstContaminationMinRun = 2     // >= 2 consecutive sub-tonic ISIs => contamination

    public static func evaluate(
        train: SpikeTrain,
        span: ISISpan,
        prior: ModeISIInterval,
        thresholds: StructuralEvidenceThresholds
    ) -> FamilyEvidenceVerdict {
        let floor = max(0, thresholds.minimumValidISISec)
        let lower = prior.lowerSec
        let upper = prior.upperSec
        let lowerTol = tolerance(lower)
        let upperTol = tolerance(upper)

        // --- Eligibility.
        guard prior.family == .tonic, prior.isValid else {
            let signal = EvidenceSignal(
                key: "tonic.eligibility", status: .fail, role: .eligibility,
                message: prior.family != .tonic ? "prior family is not tonic" : "prior interval is invalid")
            return reject(span: span, prior: prior, refinedSpan: nil, signals: [signal],
                          path: prior.family != .tonic ? "eligibility=wrong_family" : "eligibility=invalid_prior")
        }

        // --- Ordered valid indexed ISIs.
        let indexed = orderedValidISIs(train: train, span: span, floor: floor)
        let validCount = indexed.count
        if validCount == 0 { return insufficient(span: span, prior: prior, reason: .noValidISI) }
        if validCount < 2 { return insufficient(span: span, prior: prior, reason: .tooFewValidISI) }

        func outOfPrior(_ value: Double) -> Bool { value < lower - lowerTol || value > upper + upperTol }

        // --- Stage A: CONSERVATIVE boundary trim — at most ONE out-of-prior edge per side, keeping
        // the retained run >= 2. Remaining out-of-prior ISIs are handled by the fraction tiers below.
        var startK = 0
        if outOfPrior(indexed[0].value), indexed.count - 1 >= 2 { startK = 1 }
        var endM = 0
        let lastIndex = indexed.count - 1
        if lastIndex > startK, outOfPrior(indexed[lastIndex].value), indexed.count - startK - 1 >= 2 { endM = 1 }
        let retained = Array(indexed[startK ..< (indexed.count - endM)])
        guard let firstRetained = retained.first, let lastRetained = retained.last else {
            return insufficient(span: span, prior: prior, reason: .tooFewValidISI)  // defensive; unreachable
        }
        let trimmed = startK > 0 || endM > 0
        let retainedSpan = ISISpan(trainID: span.trainID, startISIIndex: firstRetained.index,
                                   endISIIndex: lastRetained.index, familyHint: .tonic)
        let refinedSpan: ISISpan? = trimmed ? retainedSpan : nil

        let eligibilitySignal = EvidenceSignal(
            key: "tonic.eligibility", status: .pass, role: .eligibility,
            observedValue: Double(validCount), message: "tonic prior; \(validCount) valid ISIs")
        let boundaryTrimSignal = EvidenceSignal(
            key: "tonic.boundary_edge_trim", status: trimmed ? .fail : .pass, role: .boundary,
            message: trimmed ? "out-of-prior edge trimmed (<=1 per side)" : "no boundary edge trim")

        // --- Stage B on the retained run.
        let metricsB = ISISpanMetrics.from(train: train, span: retainedSpan, thresholds: thresholds)
        let values = retained.map(\.value)
        let count = values.count

        // Prior compatibility fraction (both sides).
        let inPriorCount = values.filter { $0 >= lower - lowerTol && $0 <= upper + upperTol }.count
        let inPriorFraction = Double(inPriorCount) / Double(count)
        let priorCompatSignal = EvidenceSignal(
            key: "tonic.prior_compatibility",
            status: inPriorFraction >= inPriorReviewFraction ? .pass : .fail, role: .priorCompatibility,
            observedValue: inPriorFraction, requiredValue: inPriorReviewFraction,
            message: priorCompatibilityMessage(inPriorFraction))

        // Burst contamination (prior-relative: ISIs below the tonic band floor).
        let burstLikeFlags = values.map { $0 < lower - lowerTol }
        let burstLikeFraction = Double(burstLikeFlags.filter { $0 }.count) / Double(count)
        let longestBurstRun = longestTrueRun(burstLikeFlags)
        let contaminated = burstLikeFraction > thresholds.tonicBurstSeedFractionMax || longestBurstRun >= burstContaminationMinRun
        let contaminationSignal = EvidenceSignal(
            key: "tonic.burst_contamination", status: contaminated ? .fail : .pass, role: .contaminationVeto,
            observedValue: burstLikeFraction, requiredValue: thresholds.tonicBurstSeedFractionMax,
            message: contaminated ? "burst-like ISIs below tonic band (contamination)" : "no burst contamination")

        // Regularity (CV/CV2/LV) — long path only; short path marks them notApplicable.
        let isLong = count >= longRunMinISI
        let cvPass = (metricsB.cv ?? .infinity) <= thresholds.tonicCVMax
        let cv2Pass = (metricsB.cv2 ?? .infinity) <= thresholds.tonicCV2Max
        let lvPass = (metricsB.lv ?? .infinity) <= thresholds.tonicLVMax
        let classicRegular = cvPass && cv2Pass && lvPass
        let irregularRegular = (metricsB.cv ?? .infinity) <= thresholds.irregularTonicCVMax
            && (metricsB.cv2 ?? .infinity) <= thresholds.irregularTonicCV2Max
            && (metricsB.lv ?? .infinity) <= thresholds.irregularTonicLVMax
        let cvSignal = regularitySignal("tonic.regularity_cv", metricsB.cv, thresholds.tonicCVMax, isLong: isLong, pass: cvPass)
        let cv2Signal = regularitySignal("tonic.regularity_cv2", metricsB.cv2, thresholds.tonicCV2Max, isLong: isLong, pass: cv2Pass)
        let lvSignal = regularitySignal("tonic.regularity_lv", metricsB.lv, thresholds.tonicLVMax, isLong: isLong, pass: lvPass)

        // Short-run compactness (range-ratio vs the run median).
        let median = metricsB.medianSec ?? 0
        let compact = median > 0 && values.allSatisfy {
            $0 >= median * thresholds.tonicLocalRatioLow - tolerance(median)
            && $0 <= median * thresholds.tonicLocalRatioHigh + tolerance(median)
        }
        let compactSignal = EvidenceSignal(
            key: "tonic.short_run_compactness",
            status: isLong ? .notApplicable : (compact ? .pass : .fail), role: .compactness,
            message: isLong ? "long run (CV path)" : (compact ? "compact short run" : "short run not compact"))

        let signals = [eligibilitySignal, priorCompatSignal, compactSignal, cvSignal, cv2Signal, lvSignal,
                       boundaryTrimSignal, contaminationSignal]

        // --- Classify (precedence: contamination > wholesale-incompat > partial > count/regularity).
        if contaminated {
            return verdict(.rejected, span: span, refinedSpan: refinedSpan, prior: prior, signals: signals,
                           selection: .recommendNotSelectable, carving: .recommendNoCarve,
                           priorRec: .recommendAuditOnly, review: false, path: "tonic=rejected;contamination")
        }
        if inPriorFraction < inPriorRejectFraction {
            return verdict(.rejected, span: span, refinedSpan: refinedSpan, prior: prior, signals: signals,
                           selection: .recommendNotSelectable, carving: .recommendNoCarve,
                           priorRec: .recommendAuditOnly, review: false, path: "tonic=rejected;prior_incompatible")
        }
        if inPriorFraction < inPriorReviewFraction {
            return verdict(.possibleReview, span: span, refinedSpan: refinedSpan, prior: prior, signals: signals,
                           selection: .recommendReviewOnly, carving: .recommendNoCarve,
                           priorRec: .recommendAuditOnly, review: true, path: "tonic=possible_review;prior_partial")
        }
        // inPriorFraction >= 0.80 and uncontaminated.
        if count < shortRunConfirmMinISI {   // 2-ISI borderline
            return verdict(.possibleReview, span: span, refinedSpan: refinedSpan, prior: prior, signals: signals,
                           selection: .recommendReviewOnly, carving: .recommendNoCarve,
                           priorRec: .recommendTrainLocalOnly, review: true, path: "tonic=possible_review;short_run_borderline")
        }
        if isLong {
            if classicRegular {
                return verdict(trimmed ? .refined : .confirmed, span: span, refinedSpan: refinedSpan, prior: prior,
                               signals: signals, selection: .recommendSelectable, carving: .recommendNoCarve,
                               priorRec: .recommendTrainLocalOnly, review: false,
                               path: "tonic=\(trimmed ? "refined" : "confirmed");regularity=classic")
            }
            let insufficientRegularity = !irregularRegular
            return verdict(.possibleReview, span: span, refinedSpan: refinedSpan, prior: prior, signals: signals,
                           selection: .recommendReviewOnly, carving: .recommendNoCarve,
                           priorRec: insufficientRegularity ? .recommendAuditOnly : .recommendTrainLocalOnly, review: true,
                           path: insufficientRegularity ? "tonic=possible_review;regularity=insufficient" : "tonic=possible_review;regularity=irregular")
        }
        // Short run 3-4 ISIs.
        if compact {
            return verdict(trimmed ? .refined : .confirmed, span: span, refinedSpan: refinedSpan, prior: prior,
                           signals: signals, selection: .recommendSelectable, carving: .recommendNoCarve,
                           priorRec: .recommendTrainLocalOnly, review: false,
                           path: "tonic=\(trimmed ? "refined" : "confirmed");short_run=compact")
        }
        return verdict(.possibleReview, span: span, refinedSpan: refinedSpan, prior: prior, signals: signals,
                       selection: .recommendReviewOnly, carving: .recommendNoCarve,
                       priorRec: .recommendTrainLocalOnly, review: true, path: "tonic=possible_review;short_run=not_compact")
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

    private static func longestTrueRun(_ flags: [Bool]) -> Int {
        var best = 0, current = 0
        for flag in flags {
            current = flag ? current + 1 : 0
            best = Swift.max(best, current)
        }
        return best
    }

    private static func priorCompatibilityMessage(_ fraction: Double) -> String {
        if fraction >= inPriorReviewFraction { return "prior-compatible (>= 80%)" }
        if fraction >= inPriorRejectFraction { return "partial prior compatibility (50-80%)" }
        return "wholesale prior incompatibility (< 50%)"
    }

    private static func regularitySignal(_ key: String, _ observed: Double?, _ classic: Double, isLong: Bool, pass: Bool) -> EvidenceSignal {
        EvidenceSignal(
            key: key, status: isLong ? (pass ? .pass : .fail) : .notApplicable, role: .regularity,
            observedValue: observed, requiredValue: classic,
            message: isLong ? (pass ? "within classic threshold" : "exceeds classic threshold") : "short run (compactness path)")
    }

    private static func verdict(
        _ outcome: FamilyEvidenceOutcome, span: ISISpan, refinedSpan: ISISpan?, prior: ModeISIInterval,
        signals: [EvidenceSignal], selection: SelectionAuthorityRecommendation,
        carving: CarvingAuthorityRecommendation, priorRec: PriorAuthorityRecommendation,
        review: Bool, path: String
    ) -> FamilyEvidenceVerdict {
        FamilyEvidenceVerdict(
            family: .tonic, outcome: outcome, originalSpan: span, refinedSpan: refinedSpan,
            originalInterval: prior, refinedInterval: nil, signals: signals,
            selectionRecommendation: selection, carvingRecommendation: carving,
            priorRecommendation: priorRec, reviewRequired: review, decisionPath: path)
    }

    private static func reject(span: ISISpan, prior: ModeISIInterval, refinedSpan: ISISpan?, signals: [EvidenceSignal], path: String) -> FamilyEvidenceVerdict {
        verdict(.rejected, span: span, refinedSpan: refinedSpan, prior: prior, signals: signals,
                selection: .recommendNotSelectable, carving: .recommendNoCarve, priorRec: .recommendAuditOnly,
                review: false, path: "tonic=rejected;" + path)
    }

    private static func insufficient(span: ISISpan, prior: ModeISIInterval, reason: InsufficientEvidenceReason) -> FamilyEvidenceVerdict {
        let signal = EvidenceSignal(
            key: "tonic.insufficient_evidence", status: .insufficientEvidence, role: .eligibility, message: reason.message)
        return FamilyEvidenceVerdict(
            family: .tonic, outcome: .insufficientEvidence, originalSpan: span, originalInterval: prior, signals: [signal],
            selectionRecommendation: .noRecommendation, carvingRecommendation: .noRecommendation,
            priorRecommendation: .noRecommendation, reviewRequired: false, decisionPath: "tonic=insufficient;\(reason.rawValue)")
    }
}
