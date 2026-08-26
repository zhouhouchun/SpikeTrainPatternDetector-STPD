enum SpikeTrainSelectionScope: String, Identifiable {
    case raster
    case isiTimeline
    case isiStateSpace
    case neuralManifold

    var id: String { rawValue }

    var title: String {
        switch self {
        case .raster:
            return "棘波序列"
        case .isiTimeline:
            return "ISI 序列"
        case .isiStateSpace:
            return "状态空间序列"
        case .neuralManifold:
            return "神经流形序列"
        }
    }

    var selectorTitle: String {
        switch self {
        case .raster:
            return "Spike Train 选择器"
        case .isiTimeline:
            return "ISI Spike Train 选择器"
        case .isiStateSpace:
            return "ISI 状态空间 Spike Train 选择器"
        case .neuralManifold:
            return "神经流形 Spike Train 选择器"
        }
    }

    var help: String {
        switch self {
        case .raster:
            return "选择前 N 条 spike train 显示；右侧 raster 会显示当前可见 train 名称。"
        case .isiTimeline:
            return "选择 ISI 时间剖面中显示的 spike train；该选择与时间戳图互不影响。"
        case .isiStateSpace:
            return "选择 ISI 状态空间中显示的 spike train；该选择与 raster 和 ISI 时间剖面互不影响。"
        case .neuralManifold:
            return "选择进入群体神经流形计算的 spike train；至少需要两条。"
        }
    }
}
