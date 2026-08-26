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

/// Display-only coloring for reduced ISI state-space embeddings. Pattern colors are overlays and
/// never enter PCA, Isomap, detection, thresholding, or exported scientific identity.
enum ISIStateSpaceColorMode: String, CaseIterable, Identifiable {
    case time
    case autoPattern
    case finalPattern
    case train
    case qc

    var id: String { rawValue }

    var title: String {
        switch self {
        case .time: return "时间"
        case .autoPattern: return "自动模式"
        case .finalPattern: return "审核后模式"
        case .train: return "Spike train"
        case .qc: return "QC"
        }
    }

    var isPattern: Bool { self == .autoPattern || self == .finalPattern }
}

/// The mathematical view used by ISI State Space. The default keeps the direct feature axes;
/// alternative methods are read-only scientific visualizations.
enum ISIStateSpaceMethod: String, CaseIterable, Identifiable {
    case featureAxes
    case pca
    case isomap
    case logISIPhasePortrait

    var id: String { rawValue }

    var title: String {
        switch self {
        case .featureAxes: return "特征轴"
        case .pca: return "PCA"
        case .isomap: return "Isomap"
        case .logISIPhasePortrait: return "LogISI"
        }
    }
}

enum ISIStateSpaceIsomapComponentMode: String, CaseIterable, Identifiable {
    case largest
    case error

    var id: String { rawValue }

    var title: String {
        switch self {
        case .largest: return "最大连通分量"
        case .error: return "阻止计算"
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
