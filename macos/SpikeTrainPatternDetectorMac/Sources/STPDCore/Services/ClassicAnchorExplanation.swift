import Foundation

/// One line of a candidate's human-readable label explanation.
public struct ClassicAnchorExplanationLine: Hashable, Sendable {
    public let label: String
    public let value: String
    /// `true` for a failure / needs-review reason (rendered with emphasis).
    public let isFailure: Bool

    public init(label: String, value: String, isFailure: Bool = false) {
        self.label = label
        self.value = value
        self.isFailure = isFailure
    }
}

/// A **read-only** synthesis of *why* a classic-anchor candidate received its label — assembled entirely from the
/// candidate's EXISTING detector provenance (`finalLabel`, `auditRecommendedFamily`, `selectedForAuto`, the
/// adaptive `anchorBand…`, and the already-computed `auditReasonSummary` / `failureReason` from
/// `ClassicAnchorCandidateAudit` / `ClassicAnchorCandidateArbitrator`). It introduces **no new detection logic and
/// no parallel detector** — it only reformats the recorded provenance into a concise headline for the inspector.
/// Manual-threshold mode and review/override status are document-level and are layered on by the view.
public enum ClassicAnchorExplanation {
    /// Ordered headline explanation lines for a focused candidate.
    public static func lines(for candidate: ClassicAnchorCandidate) -> [ClassicAnchorExplanationLine] {
        var lines: [ClassicAnchorExplanationLine] = []

        // 1. The resolved label + how it entered the result.
        let family = candidate.auditRecommendedFamily.isEmpty
            ? readable(candidate.finalLabel.rawValue)
            : readable(candidate.auditRecommendedFamily)
        lines.append(ClassicAnchorExplanationLine(label: "Label", value: family))
        lines.append(ClassicAnchorExplanationLine(
            label: "Selected",
            value: candidate.selectedForAuto ? "Auto-selected" : "Not auto-selected (review)"
        ))

        // 2. The adaptive ISI band it was evaluated against + its spike count.
        lines.append(ClassicAnchorExplanationLine(label: "ISI band", value: bandText(candidate)))
        lines.append(ClassicAnchorExplanationLine(
            label: "Spikes",
            value: "\(candidate.nSpikes) (\(candidate.nValidISI) valid ISI)"
        ))

        // 3. The synthesized "why" (reuses the detector's own `auditReasonSummary`).
        let why = readable(candidate.auditReasonSummary)
        if !why.isEmpty {
            lines.append(ClassicAnchorExplanationLine(label: "Why", value: why))
        }

        // 4. Explicit failure / needs-review reason, when distinct from the "why".
        let failure = readable(candidate.failureReason)
        if !failure.isEmpty, failure != why {
            lines.append(ClassicAnchorExplanationLine(label: "Failure", value: failure, isFailure: true))
        }

        return lines
    }

    /// `lower–upper ms · source`, from the candidate's resolved adaptive anchor band.
    public static func bandText(_ candidate: ClassicAnchorCandidate) -> String {
        "\(milliseconds(candidate.anchorBandLowerSec))–\(milliseconds(candidate.anchorBandUpperSec)) ms"
            + " · \(readable(candidate.anchorBandSource.rawValue))"
    }

    private static func milliseconds(_ seconds: Double) -> String {
        guard seconds.isFinite else { return "—" }
        return String(format: "%.3g", seconds * 1000)
    }

    /// Provenance tokens are snake_case; this is the same readability convention the audit views use.
    private static func readable(_ token: String) -> String {
        token.replacingOccurrences(of: "_", with: " ")
    }
}
