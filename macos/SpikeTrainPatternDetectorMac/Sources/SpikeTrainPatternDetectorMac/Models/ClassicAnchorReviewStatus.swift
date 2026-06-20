import SwiftUI

enum ClassicAnchorReviewStatus: String, CaseIterable, Identifiable {
    case unreviewed
    case accepted
    case rejected
    case needsReview

    var id: String { rawValue }

    var title: String {
        switch self {
        case .unreviewed:
            return "Unreviewed"
        case .accepted:
            return "Accepted"
        case .rejected:
            return "Rejected"
        case .needsReview:
            return "Review"
        }
    }

    var inspectorDescription: String {
        switch self {
        case .unreviewed:
            return "No manual decision has been saved for this candidate yet."
        case .accepted:
            return "Saved as accepted. This manual decision is persisted and included in event export."
        case .rejected:
            return "Saved as rejected. This candidate is removed from final labels and default event export, while retained in the audit table for traceability and undo."
        case .needsReview:
            return "Saved for follow-up review. The decision is persisted and included in export."
        }
    }

    var shortTitle: String {
        switch self {
        case .unreviewed:
            return "Open"
        case .accepted:
            return "Accept"
        case .rejected:
            return "Reject"
        case .needsReview:
            return "Review"
        }
    }

    var systemImage: String {
        switch self {
        case .unreviewed:
            return "circle"
        case .accepted:
            return "checkmark.circle.fill"
        case .rejected:
            return "xmark.circle.fill"
        case .needsReview:
            return "exclamationmark.circle.fill"
        }
    }

    var tint: Color {
        switch self {
        case .unreviewed:
            return .secondary
        case .accepted:
            return .green
        case .rejected:
            return .red
        case .needsReview:
            return .orange
        }
    }

    static func parse(_ rawValue: String) -> ClassicAnchorReviewStatus? {
        let normalized = rawValue.trimmingCharacters(in: .whitespacesAndNewlines)
        return allCases.first { $0.rawValue.caseInsensitiveCompare(normalized) == .orderedSame }
    }
}
