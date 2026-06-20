enum ISIStateSpaceLayoutMode: String, CaseIterable, Identifiable {
    case singleTrain
    case overlay

    var id: String { rawValue }

    var title: String {
        switch self {
        case .singleTrain:
            return "Single"
        case .overlay:
            return "Overlay"
        }
    }
}

enum ISIStateSpaceScaling: String, CaseIterable, Identifiable {
    case robust
    case zScore

    var id: String { rawValue }

    var title: String {
        switch self {
        case .robust:
            return "Robust"
        case .zScore:
            return "Z-score"
        }
    }
}

enum ISIStateSpaceLabelSource: String, CaseIterable, Identifiable {
    case auditFinal
    case final
    case manualPriority
    case auto
    case manual

    var id: String { rawValue }

    var title: String {
        switch self {
        case .auditFinal:
            return "Audit final"
        case .final:
            return "Final"
        case .manualPriority:
            return "Manual priority"
        case .auto:
            return "Auto"
        case .manual:
            return "Manual"
        }
    }
}
