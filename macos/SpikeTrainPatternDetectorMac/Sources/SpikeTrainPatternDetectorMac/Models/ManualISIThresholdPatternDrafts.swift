import STPDCore

/// View-local text drafts for detector-independent threshold-assisted labeling.
///
/// Each pattern owns a complete draft so changing the segmented picker cannot reinterpret one
/// pattern's values as another pattern's rule. Keeping the raw strings also preserves partially
/// entered values exactly while the reviewer moves between patterns.
struct ManualISIThresholdPatternDraft: Equatable {
    var minimumISIMilliseconds: String
    var maximumISIMilliseconds: String
    var minimumSpikes: String
    var maximumSpikes: String
    var usesBurstEdgeContrast: Bool
    var minimumBurstEdgeContrast: String
    var tonicMetric: ManualISITonicMetric
    var tonicMetricMinimum: String
    var tonicMetricMaximum: String

    static func defaultValue(for pattern: ManualISIThresholdPattern) -> Self {
        switch pattern {
        case .burst:
            Self(
                minimumISIMilliseconds: "",
                maximumISIMilliseconds: "",
                minimumSpikes: "3",
                maximumSpikes: "15",
                usesBurstEdgeContrast: false,
                minimumBurstEdgeContrast: "3.0",
                tonicMetric: .cv2,
                tonicMetricMinimum: "0",
                tonicMetricMaximum: "0.30"
            )
        case .pause:
            Self(
                minimumISIMilliseconds: "",
                maximumISIMilliseconds: "",
                minimumSpikes: "2",
                maximumSpikes: "2",
                usesBurstEdgeContrast: false,
                minimumBurstEdgeContrast: "3.0",
                tonicMetric: .cv2,
                tonicMetricMinimum: "0",
                tonicMetricMaximum: "0.30"
            )
        case .tonic:
            Self(
                minimumISIMilliseconds: "",
                maximumISIMilliseconds: "",
                minimumSpikes: "6",
                maximumSpikes: "",
                usesBurstEdgeContrast: false,
                minimumBurstEdgeContrast: "3.0",
                tonicMetric: .cv2,
                tonicMetricMinimum: "0",
                tonicMetricMaximum: "0.30"
            )
        }
    }
}

struct ManualISIThresholdPatternDrafts: Equatable {
    private var burst = ManualISIThresholdPatternDraft.defaultValue(for: .burst)
    private var pause = ManualISIThresholdPatternDraft.defaultValue(for: .pause)
    private var tonic = ManualISIThresholdPatternDraft.defaultValue(for: .tonic)

    subscript(pattern: ManualISIThresholdPattern) -> ManualISIThresholdPatternDraft {
        get {
            switch pattern {
            case .burst: burst
            case .pause: pause
            case .tonic: tonic
            }
        }
        set {
            switch pattern {
            case .burst: burst = newValue
            case .pause: pause = newValue
            case .tonic: tonic = newValue
            }
        }
    }

    mutating func selectTonicMetric(_ metric: ManualISITonicMetric) {
        tonic.tonicMetric = metric
        switch metric {
        case .mm:
            tonic.minimumSpikes = "3"
            tonic.maximumSpikes = "5"
            tonic.tonicMetricMinimum = "1"
            tonic.tonicMetricMaximum = ""
        case .cv, .cv2, .lv:
            tonic.minimumSpikes = "6"
            tonic.maximumSpikes = ""
            tonic.tonicMetricMinimum = "0"
            tonic.tonicMetricMaximum = "0.30"
        }
    }
}
