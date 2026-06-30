import Foundation

/// One near-miss gate row (Phase 2D): "how far is an existing candidate's metric from the threshold it
/// was compared against, and how much would that threshold need to move for the candidate to pass?"
/// Mirrors R's `relax_row` (`R/11_near_miss.R`): `current = threshold`, `required = metric`,
/// `absoluteChange = metric - threshold`, `relativeChange = |metric - threshold| / |threshold|`.
public struct ISINearMissGateRow: Hashable, Sendable {
    /// Gate identifier, e.g. `burst_final_edge_min`.
    public let parameter: String
    /// `final` / `possible_boundary` / `local_compression` / `eventness` / `other`.
    public let category: String
    /// `decrease` for a failed minimum gate (the threshold would need to drop to the metric),
    /// `increase` for a failed maximum gate.
    public let direction: String
    /// The threshold the candidate was compared against.
    public let currentValue: Double
    /// The candidate's metric (the value the threshold would have to reach).
    public let requiredValue: Double
    /// `metric - threshold` (negative for a failed minimum gate, positive for a failed maximum gate).
    public let absoluteChange: Double
    /// `|absoluteChange| / |threshold|` (R `relax_row`: falls back to `|absoluteChange|` when the
    /// threshold is exactly zero — unreachable for the contrast gates, whose thresholds are > 1).
    public let relativeChange: Double
    public let reason: String

    public init(
        parameter: String, category: String, direction: String,
        currentValue: Double, requiredValue: Double, absoluteChange: Double,
        relativeChange: Double, reason: String
    ) {
        self.parameter = parameter; self.category = category; self.direction = direction
        self.currentValue = currentValue; self.requiredValue = requiredValue
        self.absoluteChange = absoluteChange; self.relativeChange = relativeChange; self.reason = reason
    }
}

/// Candidate-level **near-miss audit** (Phase 2D). Diagnostic: it explains how close an existing
/// rejected / unselected / evidence-only candidate was to passing a detector gate, and (reusing Phase
/// 2C eventness) flags event-like unselected candidates for review. It NEVER creates candidates,
/// changes labels/boundaries/selection, or adds annotations — mirroring R's near-miss preview, which is
/// a review aid, not a detector stage.
///
/// IMPORTANT — audit only. No field here may enter a comparison that changes AUTO candidate emission,
/// class, boundary, selected/default status, or raster annotation. Numeric fields are optional and
/// `nil` when undefined; never fabricated. No MM / max-mean.
public struct ISINearMissAudit: Hashable, Sendable {
    public let eligible: Bool
    public let isNearMiss: Bool
    /// `final` / `possible_boundary` / `local_compression` / `eventness` / `other` / `none`.
    public let bestCategory: String
    public let bestParameter: String?
    /// `decrease` / `increase` / `none`.
    public let bestDirection: String
    public let bestCurrentValue: Double?
    public let bestRequiredValue: Double?
    public let bestAbsoluteChange: Double?
    public let bestRelativeChange: Double?
    public let failureCount: Int
    public let nearMissScore: Double?
    public let eventnessScore: Double?
    public let eventnessZone: String
    public let candidateRef: String
    public let reason: String
    public let details: String
    /// All evaluated failed-gate rows (internal detail; not a CSV column).
    public let gateRows: [ISINearMissGateRow]

    public init(
        eligible: Bool, isNearMiss: Bool, bestCategory: String, bestParameter: String?,
        bestDirection: String, bestCurrentValue: Double?, bestRequiredValue: Double?,
        bestAbsoluteChange: Double?, bestRelativeChange: Double?, failureCount: Int,
        nearMissScore: Double?, eventnessScore: Double?, eventnessZone: String,
        candidateRef: String, reason: String, details: String, gateRows: [ISINearMissGateRow]
    ) {
        self.eligible = eligible; self.isNearMiss = isNearMiss; self.bestCategory = bestCategory
        self.bestParameter = bestParameter; self.bestDirection = bestDirection
        self.bestCurrentValue = bestCurrentValue; self.bestRequiredValue = bestRequiredValue
        self.bestAbsoluteChange = bestAbsoluteChange; self.bestRelativeChange = bestRelativeChange
        self.failureCount = failureCount; self.nearMissScore = nearMissScore
        self.eventnessScore = eventnessScore; self.eventnessZone = eventnessZone
        self.candidateRef = candidateRef; self.reason = reason; self.details = details
        self.gateRows = gateRows
    }

    /// Ineligible candidate (an accepted / auto-selected candidate): empty/default near-miss fields.
    public static func ineligible(candidateRef: String) -> ISINearMissAudit {
        ISINearMissAudit(
            eligible: false, isNearMiss: false, bestCategory: "none", bestParameter: nil,
            bestDirection: "none", bestCurrentValue: nil, bestRequiredValue: nil,
            bestAbsoluteChange: nil, bestRelativeChange: nil, failureCount: 0, nearMissScore: nil,
            eventnessScore: nil, eventnessZone: "unknown", candidateRef: candidateRef,
            reason: "not_eligible", details: "", gateRows: []
        )
    }
}

/// Pure builder for `ISINearMissAudit`. Operates only on EXISTING `ClassicAnchorCandidate`s (never mines
/// new spans). Reuses Phase 2C `ISICandidateEventnessAuditor` for eventness and the candidate's own
/// stored gate metrics/thresholds (the values the detector actually compared). Read-only.
public enum ISINearMissAuditor {
    /// R `near_miss_max_relax`: a candidate is a near miss only when its best finite gate
    /// `relativeChange` is within this fraction.
    public static let nearMissMaxRelax = 0.25

    /// A candidate is eligible for near-miss review when it is NOT an accepted auto-selected candidate:
    /// unselected, rejected (label / action / gate status), or evidence-only (structural pause prior).
    public static func isEligible(_ candidate: ClassicAnchorCandidate) -> Bool {
        if !candidate.selectedForAuto { return true }
        if candidate.finalLabel == .reject { return true }
        if candidate.action.lowercased() == "reject" { return true }
        if candidate.gateStatus.lowercased().contains("reject") { return true }
        if candidate.isStructuralPausePriorEvidence { return true }
        return false
    }

    public static func makeAudit(
        for candidate: ClassicAnchorCandidate,
        train: SpikeTrain,
        minValidISISec: Double
    ) -> ISINearMissAudit {
        let candidateRef = "candidate:\(candidate.id)"
        guard isEligible(candidate) else {
            return .ineligible(candidateRef: candidateRef)
        }
        let eventness = ISICandidateEventnessAuditor.makeAudit(
            for: candidate, train: train, minValidISISec: minValidISISec
        )
        return makeEligibleAudit(
            candidateRef: candidateRef,
            edgeContrastMinQ90: candidate.edgeContrastMinQ90,
            edgeContrastGeomQ90: candidate.edgeContrastGeomQ90,
            anchorContrastMinRequired: candidate.anchorContrastMinRequired,
            anchorContrastGeomRequired: candidate.anchorContrastGeomRequired,
            burstPossibleContrastRequired: candidate.burstPossibleContrastRequired,
            failureReason: candidate.failureReason,
            candidateClass: candidate.candidateClass,
            finalLabelRawValue: candidate.finalLabel.rawValue,
            eventnessScore: eventness.eventnessScore,
            eventnessZone: eventness.eventnessZone
        )
    }

    /// Assemble an eligible candidate's near-miss audit from its already-extracted gate metrics /
    /// thresholds and Phase 2C eventness. Pure and self-contained (no `ClassicAnchorCandidate`), so the
    /// gate math is directly testable. Only failed minimum gates produce gate rows; the best gate is the
    /// failed gate with the smallest `relativeChange` (closest to passing).
    public static func makeEligibleAudit(
        candidateRef: String,
        edgeContrastMinQ90: Double?,
        edgeContrastGeomQ90: Double?,
        anchorContrastMinRequired: Double?,
        anchorContrastGeomRequired: Double?,
        burstPossibleContrastRequired: Double?,
        failureReason: String,
        candidateClass: String,
        finalLabelRawValue: String,
        eventnessScore: Double?,
        eventnessZone: String
    ) -> ISINearMissAudit {
        var gateRows: [ISINearMissGateRow] = []
        func addMinGate(_ parameter: String, _ category: String, metric: Double?, threshold: Double?, reason: String) {
            guard let metric, metric.isFinite, let threshold, threshold.isFinite, metric < threshold else { return }
            let absoluteChange = metric - threshold
            let relativeChange = threshold != 0 ? abs(absoluteChange) / abs(threshold) : abs(absoluteChange)
            gateRows.append(ISINearMissGateRow(
                parameter: parameter, category: category, direction: "decrease",
                currentValue: threshold, requiredValue: metric, absoluteChange: absoluteChange,
                relativeChange: relativeChange, reason: reason
            ))
        }

        addMinGate(
            "burst_final_edge_min", "final",
            metric: edgeContrastMinQ90, threshold: anchorContrastMinRequired,
            reason: "edge contrast min below the final burst threshold"
        )
        addMinGate(
            "burst_final_edge_geom", "final",
            metric: edgeContrastGeomQ90, threshold: anchorContrastGeomRequired,
            reason: "edge contrast geom below the final burst threshold"
        )
        addMinGate(
            "possible_burst_edge_min", "possible_boundary",
            metric: edgeContrastMinQ90, threshold: burstPossibleContrastRequired,
            reason: "edge contrast min below the possible-burst threshold"
        )
        // `local_compression_ratio_min` is intentionally skipped: the local-compression threshold is a
        // detector parameter (R `local_compression_local_ratio_min = 2.20`), not carried on the Mac
        // candidate, and there is no stable key=value token in decisionPath to read it from.

        let failureCount = gateRows.count
        let bestGate = gateRows.min { $0.relativeChange < $1.relativeChange }
        let eventnessFlag = eventnessZone == "event_like" || eventnessZone == "medium_eventness_review"

        let bestCategory: String
        let reason: String
        let isNearMiss: Bool
        let nearMissScore: Double?
        if let bestGate {
            bestCategory = bestGate.category
            reason = bestGate.reason
            isNearMiss = bestGate.relativeChange <= nearMissMaxRelax
            nearMissScore = 1 - clamp01(bestGate.relativeChange / nearMissMaxRelax)
        } else if eventnessFlag {
            // Enough evidence for eventness, but no finite gate margin: eventness diagnostic only.
            bestCategory = "eventness"
            reason = "eventness_without_gate_margin"
            isNearMiss = false
            nearMissScore = nil
        } else {
            bestCategory = "none"
            reason = failureReason.isEmpty ? "no_near_miss_gate" : failureReason
            isNearMiss = false
            nearMissScore = nil
        }

        let details =
            "class=\(candidateClass); label=\(finalLabelRawValue); "
            + "failures=\(failureCount); best=\(bestGate?.parameter ?? "none"); "
            + "eventness=\(noteValue(eventnessScore)); zone=\(eventnessZone)"

        return ISINearMissAudit(
            eligible: true,
            isNearMiss: isNearMiss,
            bestCategory: bestCategory,
            bestParameter: bestGate?.parameter,
            bestDirection: bestGate?.direction ?? "none",
            bestCurrentValue: bestGate?.currentValue,
            bestRequiredValue: bestGate?.requiredValue,
            bestAbsoluteChange: bestGate?.absoluteChange,
            bestRelativeChange: bestGate?.relativeChange,
            failureCount: failureCount,
            nearMissScore: nearMissScore,
            eventnessScore: eventnessScore,
            eventnessZone: eventnessZone,
            candidateRef: candidateRef,
            reason: reason,
            details: details,
            gateRows: gateRows
        )
    }

    /// Near-miss audit for every candidate in a run, keyed by candidate id. Uses the run's dataset-level
    /// `bandSettings.minValidISISec`. A candidate with no matching train maps to `.ineligible(...)`.
    /// Pure, read-only — never mutates the run.
    public static func auditByCandidateID(
        run: ClassicAnchorDetectionRun,
        dataset: SpikeDataset
    ) -> [String: ISINearMissAudit] {
        let minValid = max(run.bandSettings.minValidISISec, 1e-9)
        let trainsByID = Dictionary(dataset.trains.map { ($0.id, $0) }, uniquingKeysWith: { first, _ in first })
        var result: [String: ISINearMissAudit] = [:]
        result.reserveCapacity(run.candidates.count)
        for candidate in run.candidates {
            if let train = trainsByID[candidate.trainID] {
                result[candidate.id] = makeAudit(for: candidate, train: train, minValidISISec: minValid)
            } else {
                result[candidate.id] = .ineligible(candidateRef: "candidate:\(candidate.id)")
            }
        }
        return result
    }

    /// The near-miss candidates ranked for human review, mirroring R's
    /// `arrange(failure_count, relative_change, desc(score), ...)` — but using the Phase 2C
    /// `eventnessScore` for the tie-break (handoff §3): lower `failureCount`, lower `bestRelativeChange`,
    /// higher `eventnessScore`, stable candidate id. Only `isNearMiss` candidates are included.
    public static func rankedNearMisses(
        run: ClassicAnchorDetectionRun,
        dataset: SpikeDataset
    ) -> [(candidateID: String, audit: ISINearMissAudit)] {
        auditByCandidateID(run: run, dataset: dataset)
            .filter { $0.value.isNearMiss }
            .map { (candidateID: $0.key, audit: $0.value) }
            .sorted { lhs, rhs in
                if lhs.audit.failureCount != rhs.audit.failureCount {
                    return lhs.audit.failureCount < rhs.audit.failureCount
                }
                let lr = lhs.audit.bestRelativeChange ?? .infinity
                let rr = rhs.audit.bestRelativeChange ?? .infinity
                if lr != rr { return lr < rr }
                let le = lhs.audit.eventnessScore ?? -.infinity
                let re = rhs.audit.eventnessScore ?? -.infinity
                if le != re { return le > re }
                return lhs.candidateID < rhs.candidateID
            }
    }

    // MARK: - Pure helpers

    private static func clamp01(_ value: Double) -> Double {
        guard value.isFinite else { return 0 }
        return Swift.max(0, Swift.min(1, value))
    }

    private static func noteValue(_ value: Double?) -> String {
        guard let value, value.isFinite else { return "NA" }
        return String(format: "%.3f", value)
    }
}
