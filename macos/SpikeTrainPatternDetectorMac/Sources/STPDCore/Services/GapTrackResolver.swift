import Foundation

/// The result of resolving pause candidates on the independent gap track.
///
/// Gap candidates do not compete with burst-family events by score. An eligible
/// pause is selected unless it conflicts with a selected burst-family event core.
/// Duplicate evidence for the exact same ISI span is collapsed deterministically
/// to one representative candidate; all non-identical eligible gap spans may
/// coexist and are projected as a union by the label/annotation layer.
public struct GapTrackResolution: Sendable {
    public let selectedStatusByID: [String: String]
    public let unselectedStatusByID: [String: String]

    public init(
        selectedStatusByID: [String: String],
        unselectedStatusByID: [String: String]
    ) {
        self.selectedStatusByID = selectedStatusByID
        self.unselectedStatusByID = unselectedStatusByID
    }

    public var selectedIDs: Set<String> {
        Set(selectedStatusByID.keys)
    }
}

public enum GapTrackResolver {
    /// Resolve formal pause candidates independently from the event track.
    ///
    /// - Parameters:
    ///   - candidates: Candidates assigned to `.gap`. Evidence-only pause priors
    ///     are ignored even if they are passed accidentally.
    ///   - selectedEvents: Burst-family candidates already selected on the event
    ///     track. A pause span may not overlap a selected event core.
    /// - Returns: Per-candidate selection and rejection statuses.
    public static func resolve(
        _ candidates: [ClassicAnchorCandidate],
        selectedEvents: [ClassicAnchorCandidate]
    ) -> GapTrackResolution {
        guard !candidates.isEmpty else {
            return GapTrackResolution(
                selectedStatusByID: [:],
                unselectedStatusByID: [:]
            )
        }

        let selectedBurstEvents = selectedEvents.filter {
            $0.arbitrationTrack == .event &&
                $0.finalLabel.isBurstEventFamily
        }

        var selectedStatusByID: [String: String] = [:]
        var unselectedStatusByID: [String: String] = [:]
        var eligibleBySpan: [GapSpan: [ClassicAnchorCandidate]] = [:]

        for candidate in candidates {
            guard candidate.finalLabel == .pause,
                  !candidate.isStructuralPausePriorEvidence else {
                unselectedStatusByID[candidate.id] =
                    "not_selected__gap_evidence_only_or_wrong_label"
                continue
            }

            guard candidate.isEligibleForAutoSelection else {
                unselectedStatusByID[candidate.id] =
                    "not_selected__gap_candidate_ineligible"
                continue
            }

            if selectedBurstEvents.contains(where: {
                overlapCount(candidate, $0) > 0
            }) {
                unselectedStatusByID[candidate.id] =
                    "not_selected__gap_conflicts_with_selected_event_core"
                continue
            }

            let span = GapSpan(candidate)
            eligibleBySpan[span, default: []].append(candidate)
        }

        for candidatesForSpan in eligibleBySpan.values {
            guard let representative = candidatesForSpan.sorted(by: isPreferred).first else {
                continue
            }

            selectedStatusByID[representative.id] = selectionStatus(for: representative)

            for duplicate in candidatesForSpan where duplicate.id != representative.id {
                unselectedStatusByID[duplicate.id] =
                    "not_selected__duplicate_gap_evidence__represented_by=\(representative.id)"
            }
        }

        return GapTrackResolution(
            selectedStatusByID: selectedStatusByID,
            unselectedStatusByID: unselectedStatusByID
        )
    }

    private struct GapSpan: Hashable {
        let start: Int
        let end: Int

        init(_ candidate: ClassicAnchorCandidate) {
            start = min(candidate.startISIIndex, candidate.endISIIndex)
            end = max(candidate.startISIIndex, candidate.endISIIndex)
        }
    }

    /// Prefer the most structurally specific evidence when several detectors
    /// describe the exact same gap. Monotonic completion remains auto-selected
    /// when it is the only evidence, but it does not replace a locked structural
    /// pause for the same ISI span.
    private static func isPreferred(
        _ lhs: ClassicAnchorCandidate,
        _ rhs: ClassicAnchorCandidate
    ) -> Bool {
        let lhsRank = preferenceRank(lhs)
        let rhsRank = preferenceRank(rhs)

        if lhsRank != rhsRank {
            return lhsRank > rhsRank
        }
        if lhs.priority != rhs.priority {
            return lhs.priority > rhs.priority
        }

        let lhsScore = lhs.score.isFinite ? lhs.score : -.infinity
        let rhsScore = rhs.score.isFinite ? rhs.score : -.infinity
        if lhsScore != rhsScore {
            return lhsScore > rhsScore
        }
        return lhs.id < rhs.id
    }

    private static func preferenceRank(_ candidate: ClassicAnchorCandidate) -> Int {
        var rank = 0

        switch candidate.anchorLockLevel {
        case .lockedClassic:
            rank += 400
        case .strongCandidate:
            rank += 200
        case .auditOnly:
            break
        }

        if isFormalStructuralFlankPause(candidate) {
            rank += 80
        }
        if isContextualInterburstPause(candidate) {
            rank += 100
        }
        if isMonotonicCompletion(candidate) {
            rank += 40
        }
        return rank
    }

    private static func selectionStatus(for candidate: ClassicAnchorCandidate) -> String {
        if isMonotonicCompletion(candidate) {
            return "selected_by_gap_track_monotonic_completion"
        }
        if isFormalStructuralFlankPause(candidate) {
            return "selected_by_gap_track_structural_flank_pause"
        }
        if isContextualInterburstPause(candidate) {
            return "selected_by_gap_track_contextual_interburst_pause"
        }
        return "selected_by_gap_track_pause_evidence"
    }

    private static func isFormalStructuralFlankPause(
        _ candidate: ClassicAnchorCandidate
    ) -> Bool {
        candidate.candidateLayer == "classic_burst_flank_pause" &&
            !candidate.isStructuralPausePriorEvidence
    }

    private static func isMonotonicCompletion(
        _ candidate: ClassicAnchorCandidate
    ) -> Bool {
        let text = "\(candidate.candidateLayer);\(candidate.decisionPath)".lowercased()
        return text.contains("pause_monotonic_completion") ||
            text.contains("pause_floor_completion") ||
            text.contains("monotonic_pause_completion")
    }

    private static func isContextualInterburstPause(
        _ candidate: ClassicAnchorCandidate
    ) -> Bool {
        candidate.candidateLayer == "contextual_interburst_pause" &&
            candidate.pauseBoundaryRole == .contextualPause
    }

    private static func overlapCount(
        _ lhs: ClassicAnchorCandidate,
        _ rhs: ClassicAnchorCandidate
    ) -> Int {
        let lhsStart = min(lhs.startISIIndex, lhs.endISIIndex)
        let lhsEnd = max(lhs.startISIIndex, lhs.endISIIndex)
        let rhsStart = min(rhs.startISIIndex, rhs.endISIIndex)
        let rhsEnd = max(rhs.startISIIndex, rhs.endISIIndex)
        let start = max(lhsStart, rhsStart)
        let end = min(lhsEnd, rhsEnd)
        return start <= end ? end - start + 1 : 0
    }
}
