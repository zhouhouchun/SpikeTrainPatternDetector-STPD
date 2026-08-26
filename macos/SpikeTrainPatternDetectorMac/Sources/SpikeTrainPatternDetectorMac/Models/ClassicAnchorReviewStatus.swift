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
            return "未审核"
        case .accepted:
            return "已接受"
        case .rejected:
            return "已拒绝"
        case .needsReview:
            return "待复核"
        }
    }

    var inspectorDescription: String {
        switch self {
        case .unreviewed:
            return "尚未为此候选保存人工决定。"
        case .accepted:
            return "已保存为接受。该人工决定会持久化并纳入事件导出。"
        case .rejected:
            return "已保存为拒绝。该候选会从最终标签和默认事件导出中移除，但仍保留在审计表中，以便追溯和撤销。"
        case .needsReview:
            return "已标记为后续复核。该决定会持久化并纳入导出。"
        }
    }

    var shortTitle: String {
        switch self {
        case .unreviewed:
            return "未处理"
        case .accepted:
            return "接受"
        case .rejected:
            return "拒绝"
        case .needsReview:
            return "复核"
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
