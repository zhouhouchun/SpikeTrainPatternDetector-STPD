enum WorkbenchGroup: String, CaseIterable, Identifiable {
    case primaryViews
    case stateAndManifold
    case diagnostics
    case validation
    case configuration
    case outputs

    var id: String { rawValue }

    var title: String {
        switch self {
        case .primaryViews:
            return "主图"
        case .stateAndManifold:
            return "状态与流形"
        case .diagnostics:
            return "诊断"
        case .validation:
            return "验证"
        case .configuration:
            return "参数"
        case .outputs:
            return "输出"
        }
    }
}

enum WorkbenchSection: String, CaseIterable, Identifiable, Hashable {
    case alignedRaster
    case rawRaster
    case dbsTrack
    case isiProfile
    case isiStateSpace
    case stateTrajectory
    case eventAlignedActivity
    case neuralManifold
    case intervalHistogram
    case datasetISIHistogram
    case structuralCandidates
    case seedBridgeDiagnostics
    case thresholdPreview
    case supportMethods
    case manualDetectorReport
    case scientificValidation
    case batchAPI
    case methodAudit
    case detectorParameters
    case adaptiveTrainTuning
    case neuralNetworkModel
    case dataQC
    case eventsOutput

    var id: String { rawValue }

    var title: String {
        switch self {
        case .alignedRaster:
            return "对齐时间戳图"
        case .rawRaster:
            return "原始时间戳图"
        case .dbsTrack:
            return "目标核团深度图"
        case .isiProfile:
            return "ISI 时间剖面"
        case .isiStateSpace:
            return "ISI 状态空间"
        case .stateTrajectory:
            return "State trajectory"
        case .eventAlignedActivity:
            return "事件对齐活动"
        case .neuralManifold:
            return "神经流形"
        case .intervalHistogram:
            return "区间直方图"
        case .datasetISIHistogram:
            return "数据集 ISI 直方图"
        case .structuralCandidates:
            return "结构候选"
        case .seedBridgeDiagnostics:
            return "Seed / Bridge 诊断"
        case .thresholdPreview:
            return "阈值预览"
        case .supportMethods:
            return "支持方法"
        case .manualDetectorReport:
            return "手动标记 vs 检测器报告"
        case .scientificValidation:
            return "科学验证"
        case .batchAPI:
            return "批处理 / API"
        case .methodAudit:
            return "方法 / 审计说明"
        case .detectorParameters:
            return "检测器 / 参数"
        case .adaptiveTrainTuning:
            return "自适应 train 调参"
        case .neuralNetworkModel:
            return "神经网络模型"
        case .dataQC:
            return "数据 QC"
        case .eventsOutput:
            return "事件 / 输出"
        }
    }

    var group: WorkbenchGroup {
        switch self {
        case .alignedRaster, .rawRaster, .dbsTrack, .dataQC:
            return .primaryViews
        case .isiProfile, .isiStateSpace, .stateTrajectory, .eventAlignedActivity, .neuralManifold:
            return .stateAndManifold
        case .intervalHistogram, .datasetISIHistogram, .structuralCandidates, .seedBridgeDiagnostics, .thresholdPreview, .supportMethods:
            return .diagnostics
        case .manualDetectorReport, .scientificValidation:
            return .validation
        case .detectorParameters, .adaptiveTrainTuning, .neuralNetworkModel:
            return .configuration
        case .batchAPI, .methodAudit, .eventsOutput:
            return .outputs
        }
    }

    var systemImage: String {
        switch self {
        case .alignedRaster:
            return "chart.xyaxis.line"
        case .rawRaster:
            return "waveform.path.ecg"
        case .dbsTrack:
            return "point.3.connected.trianglepath.dotted"
        case .isiProfile:
            return "timeline.selection"
        case .isiStateSpace:
            return "square.stack.3d.up"
        case .stateTrajectory:
            return "point.topleft.down.curvedto.point.bottomright.up"
        case .eventAlignedActivity:
            return "align.horizontal.center"
        case .neuralManifold:
            return "rotate.3d"
        case .intervalHistogram, .datasetISIHistogram:
            return "chart.bar.xaxis"
        case .structuralCandidates:
            return "rectangle.connected.to.line.below"
        case .seedBridgeDiagnostics:
            return "stethoscope"
        case .thresholdPreview:
            return "slider.horizontal.3"
        case .supportMethods:
            return "checkmark.seal"
        case .manualDetectorReport:
            return "person.crop.circle.badge.checkmark"
        case .scientificValidation:
            return "checklist.checked"
        case .batchAPI:
            return "square.stack.3d.forward.dottedline"
        case .methodAudit:
            return "doc.text.magnifyingglass"
        case .detectorParameters:
            return "switch.2"
        case .adaptiveTrainTuning:
            return "dial.low"
        case .neuralNetworkModel:
            return "brain"
        case .dataQC:
            return "exclamationmark.shield"
        case .eventsOutput:
            return "tablecells"
        }
    }

    var isLive: Bool {
        switch self {
        case .alignedRaster, .rawRaster, .isiProfile, .isiStateSpace, .datasetISIHistogram, .dataQC, .structuralCandidates, .detectorParameters:
            return true
        default:
            return false
        }
    }

    var sourceLine: Int {
        switch self {
        case .alignedRaster:
            return 2320
        case .rawRaster:
            return 2333
        case .dbsTrack:
            return 2358
        case .isiProfile:
            return 2423
        case .isiStateSpace:
            return 2483
        case .stateTrajectory:
            return 2634
        case .eventAlignedActivity:
            return 2740
        case .neuralManifold:
            return 2823
        case .intervalHistogram:
            return 3019
        case .datasetISIHistogram:
            return 3070
        case .structuralCandidates:
            return 3162
        case .seedBridgeDiagnostics:
            return 3194
        case .thresholdPreview:
            return 3236
        case .supportMethods:
            return 3274
        case .manualDetectorReport:
            return 3367
        case .scientificValidation:
            return 3393
        case .batchAPI:
            return 3440
        case .methodAudit:
            return 3448
        case .detectorParameters:
            return 3487
        case .adaptiveTrainTuning:
            return 3562
        case .neuralNetworkModel:
            return 3609
        case .dataQC:
            return 3662
        case .eventsOutput:
            return 3677
        }
    }

    var migrationStatus: String {
        isLive ? "Live" : "UI shell"
    }

    static func sections(in group: WorkbenchGroup) -> [WorkbenchSection] {
        allCases.filter { $0.group == group }
    }
}
