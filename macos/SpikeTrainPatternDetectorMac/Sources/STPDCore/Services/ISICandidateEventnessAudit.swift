import Foundation

/// Candidate-level **eventness audit** (Phase 2C). Diagnostic, event-vs-state fields computed beside a
/// detected candidate to help a reviewer triage it. Mirrors R's `R/32_eventness_audit.R`
/// (`stpd_eventness_context_indices`, `stpd_regularity_score`, `stpd_eventness_score`,
/// `stpd_eventness_zone`, the per-candidate loop in `stpd_enhance_candidate_features_eventness_core`).
///
/// IMPORTANT — audit only. Like R's eventness layer (`R/32`: "intentionally audit-first: it does not
/// overwrite AUTO labels by default") and the distributional layer (`R/63`: "does not rewrite
/// pattern_auto ... or event boundaries"), no field here may ever enter a comparison that changes which
/// candidates are emitted, selected, labeled, bounded, or annotated. All numeric fields are optional
/// and `nil` when undefined; they are never fabricated. No MM / max-mean. No fixed-ms assumption.
public struct ISICandidateEventnessAudit: Hashable, Sendable {
    public let q10ISISec: Double?
    public let q50ISISec: Double?
    public let q90ISISec: Double?
    public let q90Q10Ratio: Double?
    public let distantContextMedianSec: Double?
    public let contextContrast: Double?
    public let returnToBaselineScore: Double?
    public let eventnessEdgeComponent: Double?
    public let eventnessContextComponent: Double?
    public let eventnessScore: Double?
    public let regularityScore: Double?
    /// `event_like` / `state_like` / `medium_eventness_review` / `unknown`.
    public let eventnessZone: String
    public let mediumEventnessReview: Bool
    /// Neutral review priority derived from the zone (never a relabel):
    /// `review_event_like_candidate` / `review_state_like_candidate` / `review_ambiguous_candidate` /
    /// `insufficient_evidence`.
    public let auditRecommendation: String
    public let auditNote: String

    public init(
        q10ISISec: Double?, q50ISISec: Double?, q90ISISec: Double?, q90Q10Ratio: Double?,
        distantContextMedianSec: Double?, contextContrast: Double?, returnToBaselineScore: Double?,
        eventnessEdgeComponent: Double?, eventnessContextComponent: Double?, eventnessScore: Double?,
        regularityScore: Double?, eventnessZone: String, mediumEventnessReview: Bool,
        auditRecommendation: String, auditNote: String
    ) {
        self.q10ISISec = q10ISISec; self.q50ISISec = q50ISISec; self.q90ISISec = q90ISISec
        self.q90Q10Ratio = q90Q10Ratio; self.distantContextMedianSec = distantContextMedianSec
        self.contextContrast = contextContrast; self.returnToBaselineScore = returnToBaselineScore
        self.eventnessEdgeComponent = eventnessEdgeComponent
        self.eventnessContextComponent = eventnessContextComponent
        self.eventnessScore = eventnessScore; self.regularityScore = regularityScore
        self.eventnessZone = eventnessZone; self.mediumEventnessReview = mediumEventnessReview
        self.auditRecommendation = auditRecommendation; self.auditNote = auditNote
    }

    /// Undefined audit (out-of-range span, or no valid in-span ISI). `note` distinguishes the cause.
    public static func undefined(note: String = "") -> ISICandidateEventnessAudit {
        ISICandidateEventnessAudit(
            q10ISISec: nil, q50ISISec: nil, q90ISISec: nil, q90Q10Ratio: nil,
            distantContextMedianSec: nil, contextContrast: nil, returnToBaselineScore: nil,
            eventnessEdgeComponent: nil, eventnessContextComponent: nil, eventnessScore: nil,
            regularityScore: nil, eventnessZone: "unknown", mediumEventnessReview: false,
            auditRecommendation: "insufficient_evidence", auditNote: note
        )
    }
}

/// Pure builder for `ISICandidateEventnessAudit`. Reuses the Phase 2B `ISITemporalProfileEvidence`
/// edge-contrast minimum and computes the eventness context / scores from the train ISI trace and the
/// candidate's ISI-index span (the same `isiSec` convention as `ClassicAnchorCandidate`). Read-only.
public enum ISICandidateEventnessAuditor {
    public static let defaultContextGap = 2
    public static let defaultContextWindow = 12
    public static let defaultReturnToBaselineTolerance = 1.5
    public static let defaultEdgeRef = 3.0
    public static let defaultContextRef = 3.0
    public static let defaultRegularityCVRef = 0.50
    public static let defaultRegularityLVRef = 0.50
    public static let defaultRegularityQ90Q10Ref = 2.00
    public static let eventLikeThreshold = 0.60
    public static let stateLikeThreshold = 0.45

    /// Core builder. `edgeContrastMin` is the reused Phase 2B `ISITemporalProfileEvidence.edgeContrastMin`.
    public static func makeAudit(
        train: SpikeTrain,
        startISIIndex: Int,
        endISIIndex: Int,
        minValidISISec: Double,
        edgeContrastMin: Double?,
        contextGap: Int = defaultContextGap,
        contextWindow: Int = defaultContextWindow,
        returnToBaselineTolerance: Double = defaultReturnToBaselineTolerance
    ) -> ISICandidateEventnessAudit {
        let isi = train.isiSec
        let n = isi.count
        let minValid = minValidISISec.isFinite ? minValidISISec : 0.001
        let gap = max(0, contextGap)
        let window = max(1, contextWindow)

        func validISI(at index: Int) -> Double? {
            guard index >= 1, index < n, let value = isi[index], value.isFinite, value >= minValid else {
                return nil
            }
            return value
        }

        guard startISIIndex >= 1, endISIIndex <= n - 1, endISIIndex >= startISIIndex else {
            return .undefined()
        }

        var spanVals: [Double] = []
        for i in startISIIndex...endISIIndex {
            if let value = validISI(at: i) { spanVals.append(value) }
        }
        guard !spanVals.isEmpty else {
            return .undefined(note: "no_valid_candidate_ISI")
        }

        let sample = SortedFiniteSample(spanVals)
        let q10 = sample.quantile(0.10)
        let q50 = sample.quantile(0.50)
        let q90 = sample.quantile(0.90)
        let q90Q10Ratio: Double? = {
            guard let q10, q10 > 0, let q90 else { return nil }
            return q90 / q10
        }()

        // Distant context: window ISIs on each side, excluding the core span and the immediate gap.
        var contextVals: [Double] = []
        let leftLow = max(1, startISIIndex - gap - window)
        let leftHigh = startISIIndex - gap - 1
        if leftLow <= leftHigh {
            for i in leftLow...leftHigh { if let value = validISI(at: i) { contextVals.append(value) } }
        }
        let rightLow = endISIIndex + gap + 1
        let rightHigh = min(n - 1, endISIIndex + gap + window)
        if rightLow <= rightHigh {
            for i in rightLow...rightHigh { if let value = validISI(at: i) { contextVals.append(value) } }
        }
        let distantContextMedian = contextVals.isEmpty ? nil : SortedFiniteSample(contextVals).quantile(0.5)

        let contextContrast: Double? = {
            guard let ctxMed = distantContextMedian, let q90, q90 > 0 else { return nil }
            return ctxMed / q90
        }()

        // Return-to-baseline: how close the mean immediate flank ISI is to the distant context median.
        let pre = (startISIIndex >= 2) ? validISI(at: startISIIndex - 1) : nil
        let post = (endISIIndex + 1 <= n - 1) ? validISI(at: endISIIndex + 1) : nil
        var flanks: [Double] = []
        if let pre { flanks.append(pre) }
        if let post { flanks.append(post) }
        let returnToBaselineScore: Double? = {
            guard !flanks.isEmpty, let ctxMed = distantContextMedian, ctxMed > 0 else { return nil }
            let meanFlank = flanks.reduce(0.0, +) / Double(flanks.count)
            guard meanFlank > 0 else { return nil }
            let ratio = meanFlank / ctxMed
            let denom = returnToBaselineTolerance > 1 ? log(returnToBaselineTolerance) : 1.0
            return clamp01(exp(-abs(log(ratio)) / denom))
        }()

        let edgeComponent = clamp01(edgeContrastMin.map { $0 / max(defaultEdgeRef, .ulpOfOne) })
        let contextComponent = clamp01(contextContrast.map { $0 / max(defaultContextRef, .ulpOfOne) })

        // Boundary = min of the available edge / context components; averaged with return score if any.
        let boundary = [edgeComponent, contextComponent].compactMap { $0 }.min()
        let eventnessScore: Double? = {
            guard let boundary else { return nil }
            guard let returnScore = returnToBaselineScore else { return boundary }
            return (boundary + returnScore) / 2.0
        }()

        let cv = STPDStatistics.coefficientOfVariation(spanVals)
        let lv = STPDStatistics.localVariation(spanVals)
        let regularityScore = regularity(cv: cv, lv: lv, q90Q10Ratio: q90Q10Ratio)

        let zone = eventnessZone(eventnessScore)
        let mediumReview = zone == "medium_eventness_review"
        let note =
            "eventness=\(noteValue(eventnessScore)); zone=\(zone); regularity=\(noteValue(regularityScore))"

        return ISICandidateEventnessAudit(
            q10ISISec: q10, q50ISISec: q50, q90ISISec: q90, q90Q10Ratio: q90Q10Ratio,
            distantContextMedianSec: distantContextMedian, contextContrast: contextContrast,
            returnToBaselineScore: returnToBaselineScore,
            eventnessEdgeComponent: edgeComponent, eventnessContextComponent: contextComponent,
            eventnessScore: eventnessScore, regularityScore: regularityScore,
            eventnessZone: zone, mediumEventnessReview: mediumReview,
            auditRecommendation: recommendation(for: zone), auditNote: note
        )
    }

    /// Convenience for a detected candidate: reuses Phase 2B evidence for the edge-contrast minimum.
    public static func makeAudit(
        for candidate: ClassicAnchorCandidate,
        train: SpikeTrain,
        minValidISISec: Double,
        contextGap: Int = defaultContextGap,
        contextWindow: Int = defaultContextWindow,
        returnToBaselineTolerance: Double = defaultReturnToBaselineTolerance
    ) -> ISICandidateEventnessAudit {
        let evidence = ISITemporalProfileEvidenceBuilder.makeEvidence(
            for: candidate, train: train, minValidISISec: minValidISISec
        )
        return makeAudit(
            train: train,
            startISIIndex: candidate.startISIIndex,
            endISIIndex: candidate.endISIIndex,
            minValidISISec: minValidISISec,
            edgeContrastMin: evidence.edgeContrastMin,
            contextGap: contextGap,
            contextWindow: contextWindow,
            returnToBaselineTolerance: returnToBaselineTolerance
        )
    }

    /// Eventness audit for every candidate in a run, keyed by candidate id. Uses the run's dataset-level
    /// `bandSettings.minValidISISec`. A candidate with no matching train maps to `.undefined()`. Pure,
    /// read-only — never mutates the run.
    public static func auditByCandidateID(
        run: ClassicAnchorDetectionRun,
        dataset: SpikeDataset
    ) -> [String: ISICandidateEventnessAudit] {
        let minValid = max(run.bandSettings.minValidISISec, 1e-9)
        let trainsByID = Dictionary(dataset.trains.map { ($0.id, $0) }, uniquingKeysWith: { first, _ in first })
        var result: [String: ISICandidateEventnessAudit] = [:]
        result.reserveCapacity(run.candidates.count)
        for candidate in run.candidates {
            if let train = trainsByID[candidate.trainID] {
                result[candidate.id] = makeAudit(for: candidate, train: train, minValidISISec: minValid)
            } else {
                result[candidate.id] = .undefined()
            }
        }
        return result
    }

    // MARK: - Pure helpers

    static func eventnessZone(_ score: Double?) -> String {
        guard let score, score.isFinite else { return "unknown" }
        if score >= eventLikeThreshold { return "event_like" }
        if score <= stateLikeThreshold { return "state_like" }
        return "medium_eventness_review"
    }

    private static func recommendation(for zone: String) -> String {
        switch zone {
        case "event_like": return "review_event_like_candidate"
        case "state_like": return "review_state_like_candidate"
        case "medium_eventness_review": return "review_ambiguous_candidate"
        default: return "insufficient_evidence"
        }
    }

    /// R `stpd_regularity_score`: mean of `1 - clamp01(cv/0.50)`, `1 - clamp01(lv/0.50)`, and
    /// `1 - clamp01((q90/q10 - 1)/(2.00 - 1))` over the finite parts; nil when none are finite.
    private static func regularity(cv: Double?, lv: Double?, q90Q10Ratio: Double?) -> Double? {
        var parts: [Double] = []
        if let p = clamp01(cv.map { $0 / max(defaultRegularityCVRef, .ulpOfOne) }) { parts.append(1 - p) }
        if let p = clamp01(lv.map { $0 / max(defaultRegularityLVRef, .ulpOfOne) }) { parts.append(1 - p) }
        if let p = clamp01(q90Q10Ratio.map { ($0 - 1) / max(defaultRegularityQ90Q10Ref - 1, .ulpOfOne) }) {
            parts.append(1 - p)
        }
        guard !parts.isEmpty else { return nil }
        return parts.reduce(0.0, +) / Double(parts.count)
    }

    private static func clamp01(_ value: Double?) -> Double? {
        guard let value, value.isFinite else { return nil }
        return Swift.max(0, Swift.min(1, value))
    }

    private static func noteValue(_ value: Double?) -> String {
        guard let value, value.isFinite else { return "NA" }
        return String(format: "%.3f", value)
    }
}
