/// NM-3B/NM-3C/NM-3D: which embedding the Neural Manifold computes — PCA (default, NM-1B), Isomap (NM-3B), PHATE
/// (NM-3C, diffusion-potential + classical-MDS fallback), FA (NM-3D, maximum-likelihood factor analysis), or
/// GPFA-style (NM-3D, Gaussian-smoothed FA). Pure display state; coordinates always come from the binned
/// population activity (`matrix.scaled`), never from detector/event labels.
enum NeuralManifoldMethod: String, CaseIterable, Identifiable {
    case pca
    case isomap
    case phate
    case fa
    case gpfa

    var id: String { rawValue }

    var title: String {
        switch self {
        case .pca: return "PCA"
        case .isomap: return "Isomap"
        case .phate: return "PHATE"
        case .fa: return "FA"
        case .gpfa: return "GPFA"
        }
    }
}
/// How a disconnected Isomap kNN graph is handled (R `component = c("largest", "error")`); the UI mirror of the
/// pure `ISIStateSpaceIsomapComponentPolicy`.
enum NeuralManifoldIsomapComponentMode: String, CaseIterable, Identifiable {
    case largest
    case error

    var id: String { rawValue }

    var title: String {
        switch self {
        case .largest: return "Largest"
        case .error: return "Error"
        }
    }
}

/// NM-1E/NM-3B: 2-D vs 3-D display mode for the active population-manifold embedding. 3-D shows the
/// selected X/Y/Z axes, while 2-D uses the user-selected X/Y plane. Pure display state — it never changes
/// the underlying embedding result.
enum NeuralManifoldDisplayMode: String, CaseIterable, Identifiable {
    case twoD
    case threeD

    var id: String { rawValue }

    var title: String {
        switch self {
        case .twoD: return "2D"
        case .threeD: return "3D"
        }
    }
}

/// How the Neural Manifold colors embedding points. Annotation/visualization only — never an embedding input. Bins are
/// population-level aggregates, so per-train / QC coloring (offered in ISI State Space) does not apply here;
/// pattern modes color each bin by its dominant detector pattern state.
enum NeuralManifoldColorMode: String, CaseIterable, Identifiable {
    case time
    case autoPattern
    case finalPattern

    var id: String { rawValue }

    var title: String {
        switch self {
        case .time: return "Time"
        case .autoPattern: return "Auto"
        case .finalPattern: return "Final"
        }
    }

    var isPattern: Bool { self != .time }
}
