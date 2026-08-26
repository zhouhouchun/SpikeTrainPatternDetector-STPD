import STPDCore
import SwiftUI

struct RasterWorkspaceView: View {
    @Bindable var document: RasterDocument
    @Environment(\.l10n) private var l10n
    @State private var timeMode: RasterTimeMode
    @State private var maxSpikeTickHeightPx = 50
    @AppStorage("stpd_raster_shows_isi_information_panel") private var showsISIInformationPanel = true

    init(document: RasterDocument, initialTimeMode: RasterTimeMode = .aligned) {
        self.document = document
        _timeMode = State(initialValue: initialTimeMode)
    }

    private var adaptiveBurstBands: [String: AdaptiveBand] {
        guard let run = document.classicAnchorDetectionRun else { return [:] }
        return run.resolutions.reduce(into: [:]) { result, resolution in
            if let band = resolution.burstBand { result[resolution.trainID] = band }
        }
    }

    private var manualPauseLowerSec: Double? {
        guard document.manualPauseMode != .automatic,
              document.manualPauseMinISIMs > 0 else { return nil }
        return document.manualPauseMinISIMs / 1_000
    }

    var body: some View {
        VStack(alignment: .leading, spacing: 0) {
            HeaderView(
                document: document,
                timeMode: $timeMode,
                visibleWindowSeconds: $document.rasterVisibleWindowSeconds,
                visibleWindowUnit: $document.rasterVisibleWindowUnit,
                spikeTickHeightPx: $document.rasterSpikeTickHeightPx,
                maxSpikeTickHeightPx: maxSpikeTickHeightPx
            )
                .padding(.horizontal, 16)
                .padding(.vertical, 12)

            Divider()

            if let dataset = document.dataset {
                manualAnnotationToolbar
                Divider()
                let selectedTrainIDs = document.selectedTrainIDsIncludingFocusedCandidate(for: .raster)
                RasterCanvasView(
                    dataset: dataset,
                    selectedTrainIDs: selectedTrainIDs,
                    timeMode: timeMode,
                    visibleWindowSeconds: document.rasterVisibleWindowSeconds,
                    visibleWindowUnit: document.rasterVisibleWindowUnit,
                    artifactThresholdSeconds: document.qualitySettings.artifactThresholdSec,
                    spikeTickHeightPx: CGFloat(clampedSpikeTickHeightPx),
                    eventAnnotations: document.classicAnchorEventAnnotations,
                    focusedAnnotation: document.focusedClassicAnchorEventAnnotation,
                    focusedTimeRangeOverride: nil,
                    reviewStatuses: document.classicAnchorReviewStatuses,
                    focusRequestID: document.classicAnchorFocusRequestID,
                    adaptiveBurstBands: adaptiveBurstBands,
                    manualPauseLowerSec: manualPauseLowerSec,
                    taskEvents: document.taskEvents,
                    manualAnnotations: document.rasterManualAnnotations,
                    manualAnnotationModeEnabled: document.manualAnnotationModeEnabled,
                    showsISIInformationPanel: showsISIInformationPanel,
                    selectedManualLabel: rasterManualPreviewLabel,
                    manualAnnotationEraseModeEnabled:
                        document.rasterManualAnnotationEditMode == .erase,
                    onApplyManualLabel: { label, trainID, isiIndices in
                        if document.rasterManualAnnotationEditMode == .erase {
                            document.clearRasterManualISILabel(
                                track: document.selectedManualClearTrack,
                                trainID: trainID,
                                isiIndices: isiIndices
                            )
                        } else {
                            document.applyRasterManualISILabel(
                                label,
                                trainID: trainID,
                                isiIndices: isiIndices
                            )
                        }
                    },
                    onPinISI: { document.pinISIDiagnostic($0) }
                )
            } else {
                ContentUnavailableView(
                    l10n.t("尚未加载光栅图"),
                    systemImage: "chart.xyaxis.line",
                    description: Text(l10n.t(document.lastErrorMessage ?? "目前没有可用的 spike train。"))
                )
                .frame(maxWidth: .infinity, maxHeight: .infinity)
            }
        }
        .frame(minWidth: 640)
        .onPreferenceChange(RasterMaxSpikeTickHeightPreferenceKey.self) { maxHeight in
            maxSpikeTickHeightPx = max(4, Int(maxHeight.rounded(.down)))
        }
    }

    private var clampedSpikeTickHeightPx: Int {
        min(max(document.rasterSpikeTickHeightPx, 4), maxSpikeTickHeightPx)
    }

    private var rasterManualPreviewLabel: ManualAnnotationLabel {
        guard document.rasterManualAnnotationEditMode == .erase else {
            return document.selectedManualLabel
        }
        return ManualAnnotationLabel.positiveLabels(for: document.selectedManualClearTrack)
            .first ?? .other
    }

    private var manualAnnotationToolbar: some View {
        Group {
            if l10n.language == .ru {
                manualAnnotationToolbarStacked
            } else {
                ViewThatFits(in: .horizontal) {
                    manualAnnotationToolbarRow
                    manualAnnotationToolbarStacked
                }
            }
        }
        .padding(.horizontal, 16)
        .padding(.vertical, 8)
        .background(document.manualAnnotationModeEnabled ? Color.accentColor.opacity(0.06) : Color.clear)
    }

    private var manualAnnotationToolbarRow: some View {
        HStack(spacing: 12) {
            manualAnnotationToggle
            isiInformationToggle
            manualActionControl
            manualLabelControl
            undoManualEditButton
            Text(manualToolbarGuidance)
                .font(.caption)
                .foregroundStyle(.secondary)
                .lineLimit(1)
                .fixedSize(horizontal: true, vertical: false)
            Spacer(minLength: 8)
            manualAnnotationCount
        }
    }

    private var manualAnnotationToolbarStacked: some View {
        VStack(alignment: .leading, spacing: 7) {
            HStack(spacing: 14) {
                manualAnnotationToggle
                isiInformationToggle
                Spacer(minLength: 8)
                manualAnnotationCount
            }
            HStack(spacing: 16) {
                manualActionControl
                manualLabelControl
                undoManualEditButton
                Spacer(minLength: 0)
            }
            Text(manualToolbarGuidance)
                .font(.caption)
                .foregroundStyle(.secondary)
                .lineLimit(2)
                .fixedSize(horizontal: false, vertical: true)
        }
    }

    private var manualAnnotationToggle: some View {
        Toggle(
            l10n.t("在时间戳图上手工标记"),
            isOn: Binding(
                get: { document.manualAnnotationModeEnabled },
                set: { document.setRasterManualAnnotationModeEnabled($0) }
            )
        )
        .toggleStyle(.switch)
        .lineLimit(1)
        .fixedSize(horizontal: true, vertical: false)
        .help(l10n.t("开启后，在同一条 spike train 的时间戳行内拖动即可标记连续 ISI。"))
    }

    private var isiInformationToggle: some View {
        Toggle(l10n.t("显示 ISI 信息栏"), isOn: $showsISIInformationPanel)
            .toggleStyle(.switch)
            .lineLimit(1)
            .fixedSize(horizontal: true, vertical: false)
            .help(l10n.t("关闭后不再绘制鼠标悬停的 ISI 信息栏，可提升手工标记时的流畅度。"))
    }

    private var manualActionControl: some View {
        HStack(spacing: 8) {
            Text(l10n.t("操作"))
                .lineLimit(1)
                .fixedSize(horizontal: true, vertical: false)
            Picker("", selection: $document.rasterManualAnnotationEditMode) {
                ForEach(RasterManualAnnotationEditMode.allCases, id: \.self) { mode in
                    Text(l10n.t(mode.displayNameZH)).tag(mode)
                }
            }
            .labelsHidden()
            .pickerStyle(.segmented)
            .frame(width: l10n.language == .ru ? 192 : 142)
        }
        .disabled(!document.manualAnnotationModeEnabled)
    }

    @ViewBuilder
    private var manualLabelControl: some View {
        if document.rasterManualAnnotationEditMode == .erase {
            HStack(spacing: 8) {
                Text(l10n.t("清除轨道"))
                    .lineLimit(1)
                    .fixedSize(horizontal: true, vertical: false)
                Picker("", selection: $document.selectedManualClearTrack) {
                    ForEach(ManualAnnotationSemanticTrack.allCases, id: \.self) { track in
                        Text(l10n.t(track.displayNameZH)).tag(track)
                    }
                }
                .labelsHidden()
                .frame(width: l10n.language == .ru ? 205 : 150)
            }
            .disabled(!document.manualAnnotationModeEnabled)
        } else {
            HStack(spacing: 8) {
                Text(l10n.t("模式"))
                    .lineLimit(1)
                    .fixedSize(horizontal: true, vertical: false)
                Picker("", selection: $document.selectedManualLabel) {
                    ForEach(ManualAnnotationLabel.allCases.filter { $0.polarity == .positive }, id: \.self) { label in
                        Text(l10n.t(label.displayNameZH)).tag(label)
                    }
                }
                .labelsHidden()
                .frame(width: l10n.language == .ru ? 220 : 165)
            }
            .disabled(!document.manualAnnotationModeEnabled)
            .help(document.selectedManualLabel.minimumManualAuthoringRequirement(using: l10n))
        }
    }

    private var undoManualEditButton: some View {
        Button {
            document.undoLastManualISIEdit()
        } label: {
            Label(l10n.t("撤销上一步"), systemImage: "arrow.uturn.backward")
        }
        .disabled(!document.canUndoManualISIEdit)
        .keyboardShortcut("z", modifiers: .command)
        .fixedSize(horizontal: true, vertical: false)
    }

    private var manualAnnotationCount: some View {
        Text("\(l10n.t("手工标记")) \(document.rasterManualAnnotations.count)")
            .font(.caption.weight(.semibold))
            .foregroundStyle(.secondary)
            .lineLimit(1)
            .fixedSize(horizontal: true, vertical: false)
    }

    private var manualToolbarGuidance: String {
        guard document.rasterManualAnnotationEditMode != .erase else {
            return l10n.t("在同一行拖动，仅清除所选轨道；共存轨道会保留。")
        }
        let requirement = document.selectedManualLabel.minimumManualAuthoringRequirement(using: l10n)
        switch l10n.language {
        case .zh:
            return "拖动范围必须保持在同一行；\(requirement)；黑色 spike 始终保留可见。"
        case .en:
            return "Keep the drag within one row; \(requirement); black spike marks remain visible."
        case .ru:
            return "Перетаскивайте в пределах одной строки; \(requirement); чёрные спайки остаются видимыми."
        }
    }
}

private struct HeaderView: View {
    @Bindable var document: RasterDocument
    @Environment(\.l10n) private var l10n
    @Binding var timeMode: RasterTimeMode
    @Binding var visibleWindowSeconds: Double
    @Binding var visibleWindowUnit: QualityDisplayUnit
    @Binding var spikeTickHeightPx: Int
    let maxSpikeTickHeightPx: Int
    @State private var headerWidth: CGFloat = 900
    private var rightLabelWidth: CGFloat { l10n.language == .ru ? 138 : 74 }

    var body: some View {
        VStack(alignment: .leading, spacing: 9) {
            titleBlock

            informationControls

            scaleControls
        }
        .frame(maxWidth: .infinity, alignment: .leading)
        .background {
            GeometryReader { proxy in
                Color.clear.preference(key: HeaderWidthPreferenceKey.self, value: proxy.size.width)
            }
        }
        .onPreferenceChange(HeaderWidthPreferenceKey.self) { width in
            headerWidth = width
        }
    }

    private var titleBlock: some View {
        VStack(alignment: .leading, spacing: 3) {
            Text(l10n.t(timeMode == .aligned ? "对齐 spike 光栅图" : "原始 spike 光栅图"))
                .font(.headline)
            Text(l10n.t(document.statusMessage))
                .font(.subheadline)
                .foregroundStyle(document.lastErrorMessage == nil ? Color.secondary : Color.red)
                .lineLimit(2)
        }
    }

    private var informationControls: some View {
        ViewThatFits(in: .horizontal) {
            informationControlsRow
            informationControlsCompact
        }
    }

    private var informationControlsRow: some View {
        HStack(alignment: .center, spacing: 22) {
            timeModeControl
            datasetInlineSummary
            rasterTrainDisplayBlock
        }
        .frame(maxWidth: .infinity, alignment: .leading)
    }

    private var informationControlsCompact: some View {
        VStack(alignment: .leading, spacing: 8) {
            timeModeControl
            datasetSummary
            rasterTrainDisplayBlock
        }
    }

    private var timeModeControl: some View {
        HStack(spacing: 8) {
            Text(l10n.t("时间"))
                .foregroundStyle(.secondary)
                .frame(width: rightLabelWidth, alignment: .leading)
            GlassSegmentedControl(
                options: RasterTimeMode.allCases.map { ($0, l10n.t($0.title)) },
                selection: $timeMode,
                minSegmentWidth: 58
            )
            .frame(width: 180)
        }
    }

    @ViewBuilder
    private var rasterTrainDisplayBlock: some View {
        if let dataset = document.dataset {
            SpikeTrainCountControls(
                document: document,
                dataset: dataset,
                scope: .raster,
                showsTitle: true,
                titleWidth: rightLabelWidth
            )
            .frame(minHeight: 34, alignment: .center)
        }
    }

    private var scaleControls: some View {
        ViewThatFits(in: .horizontal) {
            scaleControlsRow
            compactScaleControls
        }
    }

    private var scaleControlsRow: some View {
        HStack(alignment: .center, spacing: 14) {
            Image(systemName: "magnifyingglass")
                .foregroundStyle(.secondary)
                .frame(height: 34)

            rasterScaleControl(
                title: l10n.t("可见窗口"),
                value: secondsBinding(
                    unit: visibleWindowUnit,
                    minimumSeconds: 0.001,
                    getSeconds: { visibleWindowSeconds },
                    setSeconds: { visibleWindowSeconds = $0; document.rasterVisibleWindowUserLocked = true }
                ),
                unit: $visibleWindowUnit,
                fieldWidth: 86,
                help: l10n.t("设置光栅视口中可见的时间跨度；旁边显示估算的 ISI 分辨率。")
            )

            spikeHeightControl

            rasterResolutionReadout

            resetZoomButton
        }
    }

    private var compactScaleControls: some View {
        VStack(alignment: .leading, spacing: 7) {
            HStack(alignment: .center, spacing: 12) {
                Image(systemName: "magnifyingglass")
                    .foregroundStyle(.secondary)
                    .frame(height: 34)

                rasterScaleControl(
                    title: l10n.t("可见窗口"),
                    value: secondsBinding(
                        unit: visibleWindowUnit,
                        minimumSeconds: 0.001,
                        getSeconds: { visibleWindowSeconds },
                        setSeconds: { visibleWindowSeconds = $0; document.rasterVisibleWindowUserLocked = true }
                    ),
                    unit: $visibleWindowUnit,
                    fieldWidth: 86,
                    help: l10n.t("设置光栅视口中可见的时间跨度；旁边显示估算的 ISI 分辨率。")
                )

                resetZoomButton
            }

            HStack(alignment: .center, spacing: 12) {
                Color.clear
                    .frame(width: 18, height: 1)

                spikeHeightControl

                rasterResolutionReadout
            }
        }
    }

    private var rasterResolutionReadout: some View {
        HStack(alignment: .firstTextBaseline, spacing: 5) {
            Text(rasterResolutionLine)
            if let rasterVisibleLine {
                Text("·")
                Text(rasterVisibleLine)
            }
        }
        .font(.caption)
        .foregroundStyle(.secondary)
        .lineLimit(1)
        .fixedSize(horizontal: true, vertical: false)
        .frame(minHeight: 34, alignment: .center)
    }

    private var resetZoomButton: some View {
        Button {
            document.resetRasterVisibleWindowToStandard()
        } label: {
                Label(l10n.t("重置缩放"), systemImage: "arrow.counterclockwise")
        }
        .liquidGlassButtonStyle()
        .labelStyle(.iconOnly)
        .help(l10n.t("重置缩放"))
        .frame(height: 34)
    }

    @ViewBuilder
    private var datasetSummary: some View {
        if let dataset = document.dataset {
            DatasetHeaderSummary(
                dataset: dataset,
                visibleTrainCount: visibleTrains(in: dataset).count,
                labelWidth: rightLabelWidth
            )
        }
    }

    @ViewBuilder
    private var datasetInlineSummary: some View {
        if let dataset = document.dataset {
            DatasetHeaderInlineSummary(
                dataset: dataset,
                visibleTrainCount: visibleTrains(in: dataset).count,
                labelWidth: rightLabelWidth
            )
        }
    }

    private func rasterScaleControl(
        title: String,
        value: Binding<Double>,
        unit: Binding<QualityDisplayUnit>,
        fieldWidth: CGFloat,
        help: String
    ) -> some View {
        HStack(spacing: 8) {
            Text(title)
                .foregroundStyle(.secondary)
                .lineLimit(1)
                .fixedSize(horizontal: true, vertical: false)
            HStack(spacing: 0) {
                DebouncedDoubleField("0", value: value, width: fieldWidth)
                Spacer()
                    .frame(width: 18)
                GlassSegmentedControl(
                    options: [
                        (.milliseconds, "ms"),
                        (.seconds, "s")
                    ],
                    selection: unit,
                    minSegmentWidth: 30
                )
                .frame(width: 82)
            }
        }
        .help(help)
        .frame(minHeight: 34, alignment: .center)
    }

    private var spikeHeightControl: some View {
        HStack(spacing: 8) {
            Text(l10n.t("Spike 高度"))
                .foregroundStyle(.secondary)
                .lineLimit(1)
                .fixedSize(horizontal: true, vertical: false)
            DebouncedIntField(
                "px",
                value: spikeHeightBinding,
                range: 4...safeMaxSpikeTickHeightPx,
                width: 58
            )
            Text("px")
                .foregroundStyle(.secondary)
            Text("\(l10n.t("最大")) \(safeMaxSpikeTickHeightPx) px")
                .font(.caption)
                .foregroundStyle(.secondary)
                .lineLimit(1)
                .fixedSize(horizontal: true, vertical: false)
        }
        .help(l10n.t("以像素控制每个 spike 标记的垂直高度；最大值随当前轨道高度变化。"))
        .frame(minHeight: 34, alignment: .center)
    }

    private var spikeHeightBinding: Binding<Int> {
        Binding(
            get: {
                min(max(spikeTickHeightPx, 4), safeMaxSpikeTickHeightPx)
            },
            set: { newValue in
                spikeTickHeightPx = min(max(newValue, 4), safeMaxSpikeTickHeightPx)
            }
        )
    }

    private var safeMaxSpikeTickHeightPx: Int {
        max(4, maxSpikeTickHeightPx)
    }

    private func secondsBinding(
        unit: QualityDisplayUnit,
        minimumSeconds: Double,
        getSeconds: @escaping () -> Double,
        setSeconds: @escaping (Double) -> Void
    ) -> Binding<Double> {
        Binding(
            get: {
                max(minimumSeconds, getSeconds()) * unit.scaleFromSeconds
            },
            set: { newValue in
                let seconds = max(minimumSeconds, newValue / unit.scaleFromSeconds)
                setSeconds(seconds)
            }
        )
    }

    private var rasterResolutionLine: String {
        let metrics = estimatedRasterMetrics()
        return "\(l10n.t("ISI 分辨率约为")) \(TimeFormatting.seconds(metrics.isiResolution))"
    }

    private var rasterVisibleLine: String? {
        let metrics = estimatedRasterMetrics()
        var line: String?
        if let visibleDuration = metrics.visibleDuration {
            line = "\(l10n.t("可见")) \(TimeFormatting.seconds(visibleDuration))"
        }
        if metrics.compressedByWidthCap {
            line = [line, l10n.t("细节层级上限")].compactMap(\.self).joined(separator: " · ")
        }
        return line
    }

    private func estimatedRasterMetrics() -> (isiResolution: Double, visibleDuration: Double?, compressedByWidthCap: Bool) {
        let pointsForResolvableISI = 3.0
        let plotViewportWidth = estimatedPlotViewportWidth
        let safeVisibleWindow = max(visibleWindowSeconds, 0.001)

        guard let dataset = document.dataset else {
            return (pointsForResolvableISI * safeVisibleWindow / Double(plotViewportWidth), nil, false)
        }

        let trains = visibleTrains(in: dataset)
        guard !trains.isEmpty else {
            return (pointsForResolvableISI * safeVisibleWindow / Double(plotViewportWidth), nil, false)
        }

        let timeRange = trains.rasterTimeRange(mode: timeMode)
        let duration = max(timeRange.duration, 0.001)
        let requestedPointsPerSecond = max(Double(plotViewportWidth) / safeVisibleWindow, 1)
        let requestedPlotWidth = duration * requestedPointsPerSecond
        let cappedPlotWidth = min(max(requestedPlotWidth, Double(plotViewportWidth)), 80_000)
        let effectivePointsPerSecond = cappedPlotWidth / duration
        let visibleDuration = min(duration, Double(plotViewportWidth) / max(effectivePointsPerSecond, .leastNonzeroMagnitude))
        let isiResolution = pointsForResolvableISI / max(effectivePointsPerSecond, .leastNonzeroMagnitude)

        return (isiResolution, visibleDuration, requestedPlotWidth > 80_000)
    }

    private var estimatedPlotViewportWidth: CGFloat {
        let containerWidth = max(headerWidth, 420)
        let labelWidth = min(max(containerWidth * 0.24, 190), 300)
        return max(containerWidth - labelWidth, 260)
    }

    private func visibleTrains(in dataset: SpikeDataset) -> [SpikeTrain] {
        let selected = document.selectedTrainIDs
        guard !selected.isEmpty else {
            return []
        }
        return dataset.trains.filter { selected.contains($0.id) }
    }
}

private struct DatasetHeaderSummary: View {
    let dataset: SpikeDataset
    let visibleTrainCount: Int
    let labelWidth: CGFloat
    @Environment(\.l10n) private var l10n

    var body: some View {
        HStack(alignment: .top, spacing: 8) {
            Text(l10n.t("数据集"))
                .font(.headline.weight(.semibold))
                .foregroundStyle(.primary)
                .frame(width: labelWidth, alignment: .leading)
                .padding(.top, 1)

            VStack(alignment: .leading, spacing: 3) {
                ViewThatFits(in: .horizontal) {
                    HStack(spacing: 10) {
                        metric(l10n.t("序列"), "\(dataset.trains.count)")
                        metric(l10n.t("Spike"), dataset.totalSpikeCount.formatted())
                        metric(l10n.t("对齐"), TimeFormatting.seconds(dataset.maxAlignedDurationSec))
                        metric(l10n.t("原始"), TimeFormatting.seconds(dataset.rawDurationSec))
                        metric(l10n.t("可见"), "\(visibleTrainCount)")
                    }

                    HStack(spacing: 10) {
                        metric(l10n.t("序列"), "\(dataset.trains.count)")
                        metric(l10n.t("Spike"), dataset.totalSpikeCount.formatted())
                        metric(l10n.t("可见"), "\(visibleTrainCount)")
                    }
                }
                .font(.caption)
                .monospacedDigit()

                ViewThatFits(in: .horizontal) {
                    HStack(spacing: 10) {
                        metric(l10n.t("对齐"), TimeFormatting.seconds(dataset.maxAlignedDurationSec))
                        metric(l10n.t("原始"), TimeFormatting.seconds(dataset.rawDurationSec))
                        sourceText
                    }

                    HStack(spacing: 10) {
                        metric(l10n.t("对齐"), TimeFormatting.seconds(dataset.maxAlignedDurationSec))
                        metric(l10n.t("原始"), TimeFormatting.seconds(dataset.rawDurationSec))
                    }
                }
                .font(.caption)
                .monospacedDigit()
            }
        }
        .frame(maxWidth: .infinity, alignment: .leading)
    }

    private var sourceText: some View {
        Text("\(l10n.t("数据源")) \(sourceDisplayName)")
            .foregroundStyle(.secondary)
            .lineLimit(1)
            .truncationMode(.middle)
            .textSelection(.enabled)
            .help(dataset.sourceDescription)
    }

    private func metric(_ title: String, _ value: String) -> some View {
        HStack(spacing: 3) {
            Text(title)
                .foregroundStyle(.secondary)
            Text(value)
                .foregroundStyle(.primary)
        }
    }

    private var sourceDisplayName: String {
        let parts = dataset.sourceDescription.split(separator: "/", omittingEmptySubsequences: true)
        return parts.last.map(String.init) ?? dataset.sourceDescription
    }
}

/// Compact, one-line dataset evidence for the shared raster-header information
/// row.  The full source detail remains available in the dedicated summary.
private struct DatasetHeaderInlineSummary: View {
    let dataset: SpikeDataset
    let visibleTrainCount: Int
    let labelWidth: CGFloat
    @Environment(\.l10n) private var l10n

    var body: some View {
        HStack(spacing: 8) {
            Text(l10n.t("数据集"))
                .foregroundStyle(.secondary)
                .frame(width: labelWidth, alignment: .leading)

            ViewThatFits(in: .horizontal) {
                metrics(showsDurations: true)
                metrics(showsDurations: false)
            }
        }
        .font(.caption)
        .monospacedDigit()
        .frame(minHeight: 34, alignment: .center)
    }

    private func metrics(showsDurations: Bool) -> some View {
        HStack(spacing: 10) {
            metric(l10n.t("序列"), "\(dataset.trains.count)")
            metric(l10n.t("Spike"), dataset.totalSpikeCount.formatted())
            metric(l10n.t("可见"), "\(visibleTrainCount)")
            if showsDurations {
                metric(l10n.t("对齐"), TimeFormatting.seconds(dataset.maxAlignedDurationSec))
                metric(l10n.t("原始"), TimeFormatting.seconds(dataset.rawDurationSec))
            }
        }
    }

    private func metric(_ title: String, _ value: String) -> some View {
        HStack(spacing: 3) {
            Text(title).foregroundStyle(.secondary)
            Text(value).foregroundStyle(.primary)
        }
    }
}

private struct HeaderWidthPreferenceKey: PreferenceKey {
    static let defaultValue: CGFloat = 900

    static func reduce(value: inout CGFloat, nextValue: () -> CGFloat) {
        value = nextValue()
    }
}
