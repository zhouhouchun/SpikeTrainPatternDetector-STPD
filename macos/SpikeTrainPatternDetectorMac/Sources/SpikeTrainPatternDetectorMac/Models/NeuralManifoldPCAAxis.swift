import STPDCore

/// Display axis for the active population-manifold embedding. User-facing component labels are method-aware
/// (PCA -> PC1/PC2/PC3, Isomap -> Isomap1/Isomap2/Isomap3, PHATE -> PHATE1/PHATE2/PHATE3,
/// FA -> FA1/FA2/FA3, GPFA -> GPFA1/GPFA2/GPFA3), while the shared score coordinates remain NM1/NM2/NM3.
/// `.time` uses the population-bin midpoint as a coordinate. This is a pure display concern; it does not affect
/// the embedding result.
enum NeuralManifoldPCAAxis: String, CaseIterable, Identifiable {
    case nm1
    case nm2
    case nm3
    case time

    var id: String { rawValue }

    /// Default user-facing axis label (PCA method -> PC labels), kept for older call sites.
    var title: String {
        title(for: .pca)
    }

    /// User-facing axis label for the active embedding method.
    func title(for method: NeuralManifoldMethod) -> String {
        let index: String
        switch self {
        case .nm1: index = "1"
        case .nm2: index = "2"
        case .nm3: index = "3"
        case .time: return "Time"
        }
        switch method {
        case .pca: return "PC" + index
        case .isomap: return "Isomap" + index
        case .phate: return "PHATE" + index
        case .fa: return "FA" + index
        case .gpfa: return "GPFA" + index
        }
    }

    /// Underlying manifold coordinate name (NM1–NM3), for diagnostics/tooltips if needed.
    var coordinateLabel: String {
        switch self {
        case .nm1: return "NM1"
        case .nm2: return "NM2"
        case .nm3: return "NM3"
        case .time: return "time"
        }
    }

    /// The selected coordinate value of a PCA score row.
    func value(in score: NeuralPopulationPCAScore) -> Double {
        switch self {
        case .nm1: return score.nm1
        case .nm2: return score.nm2
        case .nm3: return score.nm3
        case .time: return score.midSec
        }
    }
}
