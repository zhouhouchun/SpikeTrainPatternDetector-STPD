import AppKit
import STPDCore
import SwiftUI

struct ISITimelineView: View {
    @Bindable var document: RasterDocument

    @Environment(\.l10n) private var l10n

    @State private var pageIndex = 0
    @State private var highlightedTrainIDs: Set<String> = []

    private let panesPerPage = 4

    private var selectedTraces: [SpikeISITrace] {
        guard let dataset = document.dataset else {
            return []
        }

        let selected = document.selectedTrainIDsIncludingFocusedCandidate(for: .isiTimeline)
        guard !selected.isEmpty else {
            return []
        }

        return dataset.trains
            .filter { selected.contains($0.id) }
            .map { SpikeISITrace.build(for: $0, settings: document.qualitySettings) }
    }

    private var displayedTraces: [SpikeISITrace] {
        switch document.isiLayoutMode {
        case .separateAxes:
            let start = min(pageIndex * panesPerPage, selectedTraces.count)
            return Array(selectedTraces.dropFirst(start).prefix(panesPerPage))
        case .overlay:
            return selectedTraces
        }
    }

    var body: some View {
        VStack(alignment: .leading, spacing: 0) {
            header
                .padding(.horizontal, 16)
                .padding(.vertical, 12)

            Divider()

            content
        }
        .frame(minWidth: 640)
        .onChange(of: selectedTraceIDs) { _, _ in
            clampPageIndex()
            highlightedTrainIDs.formIntersection(Set(selectedTraceIDs))
        }
        .onChange(of: document.isiLayoutMode) { _, _ in
            clampPageIndex()
            prepareFocusedCandidateView()
        }
        .onChange(of: document.classicAnchorFocusRequestID) { _, _ in
            prepareFocusedCandidateView()
        }
        .onAppear {
            prepareFocusedCandidateView()
        }
    }

    @ViewBuilder
    private var content: some View {
        if document.dataset == nil {
            ContentUnavailableView(
                l10n.t("未加载 ISI 序列"),
                systemImage: "timeline.selection",
                description: Text(document.lastErrorMessage ?? l10n.t("请先加载脉冲序列数据集。"))
            )
            .frame(maxWidth: .infinity, maxHeight: .infinity)
        } else if selectedTraces.isEmpty {
            ContentUnavailableView(
                l10n.t("未选择 ISI 序列"),
                systemImage: "checklist",
                description: Text(l10n.t("使用顶部的 ISI 序列选择器选择一个或多个脉冲序列。"))
            )
            .frame(maxWidth: .infinity, maxHeight: .infinity)
        } else {
            ISITimelineCanvasView(
                traces: displayedTraces,
                timeRangeTraces: selectedTraces,
                layoutMode: document.isiLayoutMode,
                timeMode: document.isiTimeMode,
                displayUnit: document.isiDisplayUnit,
                visibleWindowSeconds: document.isiVisibleWindowSeconds,
                yScale: document.isiYAxisScale,
                lockYAxis: document.isiLockYAxis,
                qualitySettings: document.qualitySettings,
                eventAnnotations: document.classicAnchorPublicEventAnnotations,
                stateAnnotations: document.classicAnchorStateAnnotations,
                focusedAnnotation: document.focusedClassicAnchorEventAnnotation,
                reviewStatuses: document.classicAnchorReviewStatuses,
                focusRequestID: isiFocusScrollKey,
                reference: document.isiTimelineReference,
                referenceTolerance: document.isiTimelineReferenceTolerance,
                thresholdLines: document.isiTimelineShowsThresholdLines
                    ? document.activeManualISIThresholdLines : [],
                onLockReference: { event in
                    document.lockISITimelineReference(
                        trainID: event.trainID,
                        trainName: event.trainName,
                        intervalIndex: event.intervalIndex,
                        isiSec: event.isiSec
                    )
                },
                highlightedTrainIDs: $highlightedTrainIDs
            )
        }
    }

    private var header: some View {
        VStack(alignment: .leading, spacing: 10) {
            HStack(alignment: .center, spacing: 14) {
                VStack(alignment: .leading, spacing: 4) {
                    HStack(alignment: .firstTextBaseline, spacing: 10) {
                        Text(l10n.t("ISI 时间剖面"))
                            .font(.headline)
                        Text(l10n.t("实时"))
                            .font(.caption.weight(.semibold))
                            .foregroundStyle(.secondary)
                            .padding(.horizontal, 8)
                            .padding(.vertical, 3)
                            .background(.quaternary, in: Capsule())
                    }

                    Text(summaryText)
                        .font(.caption)
                        .foregroundStyle(.secondary)
                        .lineLimit(1)
                }

                Spacer(minLength: 16)
                isiTrainDisplayBlock
            }

            ScrollView(.horizontal) {
                controls
                    .fixedSize(horizontal: true, vertical: false)
            }
            .scrollIndicators(.hidden)

            if document.dataset != nil, !selectedTraces.isEmpty {
                ScrollView(.horizontal) {
                    referenceControls
                        .fixedSize(horizontal: true, vertical: false)
                }
                .scrollIndicators(.hidden)
            }

            if document.isiLayoutMode == .overlay, !selectedTraces.isEmpty {
                ISIOverlayLegendStrip(
                    traces: selectedTraces,
                    highlightedTrainIDs: $highlightedTrainIDs
                )
            }
        }
    }

    @ViewBuilder
    private var isiTrainDisplayBlock: some View {
        if let dataset = document.dataset {
            VStack(alignment: .leading, spacing: 7) {
                Text(l10n.t(SpikeTrainSelectionScope.isiTimeline.title))
                    .font(.caption.weight(.semibold))
                    .foregroundStyle(.secondary)

                SpikeTrainCountControls(
                    document: document,
                    dataset: dataset,
                    scope: .isiTimeline,
                    showsTitle: false
                )
            }
            .fixedSize(horizontal: true, vertical: false)
        }
    }

    private var controls: some View {
        HStack(alignment: .center, spacing: 20) {
            controlGroup(l10n.t("布局")) {
                GlassSegmentedControl(
                    options: ISITimelineLayoutMode.allCases.map { ($0, $0.title) },
                    selection: $document.isiLayoutMode,
                    minSegmentWidth: 70
                )
                .frame(width: 176)
            }

            controlGroup(l10n.t("时间")) {
                GlassSegmentedControl(
                    options: RasterTimeMode.allCases.map { ($0, $0.title) },
                    selection: $document.isiTimeMode,
                    minSegmentWidth: 52
                )
                .frame(width: 150)
            }

            visibleWindowControl

            controlGroup(l10n.t("单位"), spacing: 20) {
                GlassSegmentedControl(
                    options: [
                        (.milliseconds, "ms"),
                        (.seconds, "s")
                    ],
                    selection: $document.isiDisplayUnit,
                    minSegmentWidth: 30
                )
                .frame(width: 82)
            }

            controlGroup(l10n.t("Y 轴刻度")) {
                GlassSegmentedControl(
                    options: ISIYAxisScale.allCases.map { ($0, $0.title) },
                    selection: $document.isiYAxisScale,
                    minSegmentWidth: 40
                )
                .frame(width: 128)
            }

            if document.isiLayoutMode == .separateAxes {
                Toggle(l10n.t("锁定 Y 轴"), isOn: $document.isiLockYAxis)
                    .toggleStyle(.checkbox)
                    .help(l10n.t("全部可见的四个 ISI 窗格使用同一共享 Y 轴范围。"))
            }

            pageControls

            Button {
                document.isiVisibleWindowSeconds = 5
                document.isiVisibleWindowUnit = .seconds
                pageIndex = 0
            } label: {
                Label(l10n.t("重置"), systemImage: "arrow.counterclockwise")
            }
            .liquidGlassButtonStyle()
            .labelStyle(.iconOnly)
            .help(l10n.t("重置 ISI 窗口"))

        }
        .font(.subheadline)
    }

    private var referenceControls: some View {
        HStack(alignment: .center, spacing: 16) {
            Image(systemName: "scope")
                .foregroundStyle(.teal)

            if let reference = document.isiTimelineReference {
                VStack(alignment: .leading, spacing: 1) {
                    Text("参考值 \(ISITimelineFormat.time(reference.isiSec, unit: document.isiDisplayUnit))")
                        .font(.caption.weight(.semibold))
                        .monospacedDigit()
                    Text("\(reference.trainName) · interval \(reference.intervalIndex)")
                        .font(.caption2)
                        .foregroundStyle(.secondary)
                        .lineLimit(1)
                        .truncationMode(.middle)
                }
                .frame(maxWidth: 230, alignment: .leading)
                .fixedSize(horizontal: true, vertical: false)

                Button {
                    document.clearISITimelineReference()
                } label: {
                    Label(l10n.t("清除"), systemImage: "xmark.circle")
                }
                .liquidGlassButtonStyle()
                .labelStyle(.iconOnly)
                .help(l10n.t("清除已锁定的 ISI 参考。"))

                Menu {
                    ForEach(ISIReferenceThresholdTarget.allCases) { target in
                        Button(target.menuTitle) {
                            document.copyISITimelineReferenceToManualThreshold(target)
                        }
                    }
                } label: {
                    Label(l10n.t("复制到手动阈值"), systemImage: "arrow.right.doc.on.clipboard")
                }
                .menuStyle(.borderlessButton)
                .fixedSize()
                .help(l10n.t("将参考 ISI 复制到某个手动阈值字段。若该族为自动，则会成为软锚点，在下次检测器运行时生效。"))

                controlGroup(l10n.t("相似 ±")) {
                    GlassSegmentedControl(
                        options: [(0.05, "5%"), (0.10, "10%"), (0.20, "20%")],
                        selection: $document.isiTimelineReferenceTolerance,
                        minSegmentWidth: 40
                    )
                    .frame(width: 138)
                }
                .help(l10n.t("高亮显示数值在参考值此比例范围内的 ISI。"))
            } else {
                Text(l10n.t("点击 ISI 点或线段将其锁定为参考。"))
                    .font(.caption)
                    .foregroundStyle(.secondary)
            }

            Toggle(l10n.t("阈值线"), isOn: $document.isiTimelineShowsThresholdLines)
                .toggleStyle(.checkbox)
                .help(l10n.t("叠加显示活动的手动阈值线（实线 = 硬门限，虚线 = 软锚点）。"))
        }
        .font(.subheadline)
    }

    private func controlGroup<Content: View>(
        _ title: String,
        spacing: CGFloat = 10,
        @ViewBuilder content: () -> Content
    ) -> some View {
        HStack(spacing: spacing) {
            Text(title)
                .foregroundStyle(.secondary)
                .fixedSize(horizontal: true, vertical: false)
            content()
        }
    }

    private var visibleWindowControl: some View {
        HStack(spacing: 12) {
            Text(l10n.t("可见窗口"))
                .foregroundStyle(.secondary)
                .fixedSize(horizontal: true, vertical: false)
            DebouncedDoubleField(
                "0",
                value: visibleWindowBinding,
                width: 76
            )
            GlassSegmentedControl(
                options: [
                    (.milliseconds, "ms"),
                    (.seconds, "s")
                ],
                selection: $document.isiVisibleWindowUnit,
                minSegmentWidth: 30
            )
            .frame(width: 82)
        }
        .help(l10n.t("控制可见 ISI 时间视口的宽度。横向滚动以浏览时间轴。"))
        .padding(.trailing, 24)
    }

    @ViewBuilder
    private var pageControls: some View {
        if document.isiLayoutMode == .separateAxes, totalPages > 1 {
            HStack(spacing: 6) {
                Button {
                    pageIndex = max(0, pageIndex - 1)
                } label: {
                    Image(systemName: "chevron.left")
                }
                .liquidGlassButtonStyle()
                .disabled(pageIndex == 0)

                Text("\(pageIndex + 1) / \(totalPages)")
                    .font(.caption)
                    .foregroundStyle(.secondary)
                    .monospacedDigit()
                    .frame(width: 46)

                Button {
                    pageIndex = min(totalPages - 1, pageIndex + 1)
                } label: {
                    Image(systemName: "chevron.right")
                }
                .liquidGlassButtonStyle()
                .disabled(pageIndex >= totalPages - 1)
            }
            .help(l10n.t("显示下一组（最多四个）ISI 窗格。"))
        }
    }

    private var visibleWindowBinding: Binding<Double> {
        Binding(
            get: {
                max(0.001, document.isiVisibleWindowSeconds) * document.isiVisibleWindowUnit.scaleFromSeconds
            },
            set: { newValue in
                document.isiVisibleWindowSeconds = max(0.001, newValue / document.isiVisibleWindowUnit.scaleFromSeconds)
            }
        )
    }

    private var summaryText: String {
        let eventCount = selectedTraces.reduce(0) { $0 + $1.events.count }
        let pageText = document.isiLayoutMode == .separateAxes && totalPages > 1 ? ", page \(pageIndex + 1)/\(totalPages)" : ""
        return "\(selectedTraces.count) selected, \(eventCount.formatted()) ISI interval(s)\(pageText)"
    }

    private var selectedTraceIDs: [String] {
        selectedTraces.map(\.id)
    }

    private var totalPages: Int {
        max(1, Int(ceil(Double(max(selectedTraces.count, 1)) / Double(panesPerPage))))
    }

    private var isiFocusScrollKey: Int {
        let layoutOffset = document.isiLayoutMode == .overlay ? 50_000 : 0
        return document.classicAnchorFocusRequestID &* 1_000 &+ pageIndex &+ layoutOffset
    }

    private func clampPageIndex() {
        pageIndex = min(max(0, pageIndex), max(0, totalPages - 1))
    }

    private func prepareFocusedCandidateView() {
        guard let annotation = document.focusedClassicAnchorEventAnnotation else {
            return
        }

        highlightedTrainIDs = [annotation.trainID]

        guard document.isiLayoutMode == .separateAxes,
              let traceIndex = selectedTraces.firstIndex(where: { $0.id == annotation.trainID }) else {
            return
        }
        pageIndex = min(max(0, traceIndex / panesPerPage), max(0, totalPages - 1))
    }
}

private struct ISIOverlayLegendStrip: View {
    let traces: [SpikeISITrace]
    @Binding var highlightedTrainIDs: Set<String>

    @Environment(\.l10n) private var l10n

    var body: some View {
        ScrollView(.horizontal) {
            HStack(spacing: 8) {
                ForEach(Array(traces.enumerated()), id: \.element.id) { index, trace in
                    Button {
                        updateHighlight(for: trace.id)
                    } label: {
                        HStack(spacing: 6) {
                            Circle()
                                .fill(displayColor(for: trace.id, index: index))
                                .frame(width: 8, height: 8)
                            Text(trace.trainName)
                                .lineLimit(1)
                                .truncationMode(.middle)
                        }
                        .font(.caption)
                        .padding(.horizontal, 8)
                        .padding(.vertical, 5)
                        .background(.quaternary.opacity(highlightedTrainIDs.contains(trace.id) ? 0.8 : 0.45), in: RoundedRectangle(cornerRadius: 7))
                    }
                    .buttonStyle(.plain)
                    .help(l10n.t("点击聚焦此序列。按住 Command 点击可聚焦多个序列。"))
                }

                if !highlightedTrainIDs.isEmpty {
                    Button("显示全部") {
                        highlightedTrainIDs = []
                    }
                    .font(.caption)
                    .liquidGlassButtonStyle()
                }
            }
            .padding(.vertical, 1)
        }
        .scrollIndicators(.hidden)
    }

    private func updateHighlight(for trainID: String) {
        if NSEvent.modifierFlags.contains(.command) {
            if highlightedTrainIDs.contains(trainID) {
                highlightedTrainIDs.remove(trainID)
            } else {
                highlightedTrainIDs.insert(trainID)
            }
        } else if highlightedTrainIDs == [trainID] {
            highlightedTrainIDs = []
        } else {
            highlightedTrainIDs = [trainID]
        }
    }

    private func displayColor(for trainID: String, index: Int) -> Color {
        if !highlightedTrainIDs.isEmpty, !highlightedTrainIDs.contains(trainID) {
            return .secondary.opacity(0.45)
        }
        return ISIPlotPalette.color(at: index)
    }
}

private struct ISITimelineCanvasView: View {
    let traces: [SpikeISITrace]
    let timeRangeTraces: [SpikeISITrace]
    let layoutMode: ISITimelineLayoutMode
    let timeMode: RasterTimeMode
    let displayUnit: QualityDisplayUnit
    let visibleWindowSeconds: Double
    let yScale: ISIYAxisScale
    let lockYAxis: Bool
    let qualitySettings: SpikeQualitySettings
    let eventAnnotations: [ClassicAnchorEventAnnotation]
    let stateAnnotations: [ClassicAnchorEventAnnotation]
    let focusedAnnotation: ClassicAnchorEventAnnotation?
    let reviewStatuses: [String: ClassicAnchorReviewStatus]
    let focusRequestID: Int
    let reference: ISITimelineReference?
    let referenceTolerance: Double
    let thresholdLines: [ISIManualThresholdLine]
    let onLockReference: (SpikeISIEvent) -> Void
    @Binding var highlightedTrainIDs: Set<String>

    @Environment(\.l10n) private var l10n

    var body: some View {
        GeometryReader { proxy in
            let layout = ISITimelineLayout(
                containerSize: proxy.size,
                traceCount: traces.count,
                timeRange: ISITimelineLayout.timeRange(for: timeRangeTraces, mode: timeMode),
                visibleWindowSeconds: visibleWindowSeconds,
                layoutMode: layoutMode
            )
            let legendEntries = PatternLegendEntry.visibleEntries(
                annotations: eventAnnotations + stateAnnotations,
                trainIDs: Set(traces.map(\.trainID)),
                timeRange: layout.timeRange,
                timeMode: timeMode
            )

            if traces.isEmpty {
                ContentUnavailableView(l10n.t("无可见 ISI 序列"), systemImage: "timeline.selection")
                    .frame(maxWidth: .infinity, maxHeight: .infinity)
            } else {
                ScrollViewReader { verticalProxy in
                    ScrollView(.vertical) {
                        HStack(alignment: .top, spacing: 0) {
                            if layoutMode == .separateAxes {
                                ISIAxisLabelColumn(
                                    traces: traces,
                                    layout: layout,
                                    legendEntries: legendEntries
                                )
                                    .frame(width: layout.labelWidth, height: layout.contentHeight)
                            }

                            ScrollViewReader { horizontalProxy in
                                ScrollView(.horizontal) {
                                    ISITimelinePlotSurface(
                                        traces: traces,
                                        layout: layout,
                                        layoutMode: layoutMode,
                                        timeMode: timeMode,
                                        displayUnit: displayUnit,
                                        yScale: yScale,
                                        lockYAxis: lockYAxis,
                                        qualitySettings: qualitySettings,
                                        eventAnnotations: eventAnnotations,
                                        stateAnnotations: stateAnnotations,
                                        focusedAnnotation: focusedAnnotation,
                                        reviewStatuses: reviewStatuses,
                                        showsInlineLegend: layoutMode == .overlay,
                                        reference: reference,
                                        referenceTolerance: referenceTolerance,
                                        thresholdLines: thresholdLines,
                                        onLockReference: onLockReference,
                                        highlightedTrainIDs: $highlightedTrainIDs
                                    )
                                    .frame(width: layout.plotWidth, height: layout.contentHeight)
                                }
                                .frame(width: layout.plotViewportWidth, height: layout.contentHeight)
                                .onAppear {
                                    scrollToFocusedAnnotation(
                                        verticalProxy: verticalProxy,
                                        horizontalProxy: horizontalProxy,
                                        traces: traces
                                    )
                                }
                                .onChange(of: focusRequestID) { _, _ in
                                    scrollToFocusedAnnotation(
                                        verticalProxy: verticalProxy,
                                        horizontalProxy: horizontalProxy,
                                        traces: traces
                                    )
                                }
                            }
                        }
                        .frame(height: layout.contentHeight, alignment: .top)
                    }
                }
                .background(Color(nsColor: .textBackgroundColor))
            }
        }
    }

    private func scrollToFocusedAnnotation(
        verticalProxy: ScrollViewProxy,
        horizontalProxy: ScrollViewProxy,
        traces: [SpikeISITrace]
    ) {
        guard let focusedAnnotation,
              traces.contains(where: { $0.id == focusedAnnotation.trainID }) else {
            return
        }

        withAnimation(.easeInOut(duration: 0.28)) {
            verticalProxy.scrollTo(ISITimelineFocusScrollID.trace(focusedAnnotation.trainID), anchor: .center)
            horizontalProxy.scrollTo(ISITimelineFocusScrollID.event(focusedAnnotation.id), anchor: .center)
        }
    }
}

private enum ISITimelineFocusScrollID: Hashable {
    case trace(String)
    case event(String)
}

private struct ISIAxisLabelColumn: View {
    let traces: [SpikeISITrace]
    let layout: ISITimelineLayout
    let legendEntries: [PatternLegendEntry]

    var body: some View {
        VStack(spacing: 0) {
            ZStack(alignment: .bottomTrailing) {
                PatternLegendStrip(entries: legendEntries)
                    .padding(.trailing, 8)
                    .padding(.leading, 8)
                    .padding(.bottom, 7)
            }
            .frame(height: ISITimelineLayout.topInset)

            ForEach(traces) { trace in
                VStack(alignment: .trailing, spacing: 4) {
                    Text(trace.trainName)
                        .font(.system(size: traces.count > 3 ? 10 : 11, weight: .medium))
                        .foregroundStyle(.secondary)
                        .lineLimit(1)
                        .truncationMode(.middle)
                    Text("\(trace.events.count.formatted()) ISI")
                        .font(.caption2)
                        .foregroundStyle(.tertiary)
                        .monospacedDigit()
                }
                .frame(maxWidth: .infinity, minHeight: layout.paneHeight, maxHeight: layout.paneHeight, alignment: .trailing)
                .padding(.trailing, 12)
                .id(ISITimelineFocusScrollID.trace(trace.id))
            }

            Spacer()
                .frame(height: ISITimelineLayout.bottomInset)
        }
        .background(Color(nsColor: .controlBackgroundColor))
        .overlay(alignment: .trailing) {
            Rectangle()
                .fill(Color(nsColor: .separatorColor))
                .frame(width: 1)
        }
    }
}

private struct ISITimelinePlotSurface: View {
    private static let isiSegmentLineWidth: CGFloat = 2
    private static let isiMarkerRadius: CGFloat = 2.5
    private static let eventIntervalLineWidth: CGFloat = 3.5
    private static let eventIntervalYOffset: CGFloat = 5
    private static let stateTrackLineWidth: CGFloat = 5.5
    private static let referenceColor = Color(nsColor: .systemTeal)
    private static let manualThresholdColor = Color(nsColor: .systemPurple)

    let traces: [SpikeISITrace]
    let layout: ISITimelineLayout
    let layoutMode: ISITimelineLayoutMode
    let timeMode: RasterTimeMode
    let displayUnit: QualityDisplayUnit
    let yScale: ISIYAxisScale
    let lockYAxis: Bool
    let qualitySettings: SpikeQualitySettings
    let eventAnnotations: [ClassicAnchorEventAnnotation]
    let stateAnnotations: [ClassicAnchorEventAnnotation]
    let focusedAnnotation: ClassicAnchorEventAnnotation?
    let reviewStatuses: [String: ClassicAnchorReviewStatus]
    let showsInlineLegend: Bool
    let reference: ISITimelineReference?
    let referenceTolerance: Double
    let thresholdLines: [ISIManualThresholdLine]
    let onLockReference: (SpikeISIEvent) -> Void
    @Binding var highlightedTrainIDs: Set<String>

    @Environment(\.l10n) private var l10n

    @State private var hoverLocation: CGPoint?

    var body: some View {
        let model = ISITimelinePlotModel(
            traces: traces,
            layout: layout,
            layoutMode: layoutMode,
            timeMode: timeMode,
            displayUnit: displayUnit,
            yScale: yScale,
            lockYAxis: lockYAxis,
            qualitySettings: qualitySettings
        )

        ZStack(alignment: .topLeading) {
            Canvas { context, _ in
                drawBackground(model: model, context: &context)
                drawStateAnnotations(model: model, context: &context)
                drawEventAnnotations(model: model, context: &context)
                drawThresholds(model: model, context: &context)
                drawManualThresholdLines(model: model, context: &context)
                drawReferenceOverlay(model: model, context: &context)
                drawTraces(model: model, context: &context)
                drawSimilarISIHighlights(model: model, context: &context)
                drawXAxis(model: model, context: &context)
            }
            .background(Color(nsColor: .textBackgroundColor))

            if let hoverLocation,
               let target = model.hoverTarget(at: hoverLocation, displayUnit: displayUnit) {
                ISIHoverCard(target: target)
                    .frame(width: 310, alignment: .leading)
                    .position(model.hoverCardPosition(for: hoverLocation))
                    .allowsHitTesting(false)
            }

            if showsInlineLegend {
                VStack {
                    HStack {
                        Spacer(minLength: 0)
                        annotationLegend(model: model)
                    }
                    Spacer(minLength: 0)
                }
                .padding(.top, 8)
                .padding(.trailing, 12)
                .allowsHitTesting(false)
            }

            focusedEventAnchor
        }
        .contentShape(Rectangle())
        .onContinuousHover { phase in
            switch phase {
            case .active(let location):
                hoverLocation = location
            case .ended:
                hoverLocation = nil
            }
        }
        .simultaneousGesture(
            DragGesture(minimumDistance: 0)
                .onEnded { value in
                    guard abs(value.translation.width) < 3,
                          abs(value.translation.height) < 3,
                          let event = model.nearestEvent(at: value.location) else {
                        return
                    }
                    onLockReference(event)
                    if layoutMode == .overlay {
                        updateHighlight(for: event.trainID)
                    }
                }
        )
    }

    @ViewBuilder
    private func annotationLegend(model: ISITimelinePlotModel) -> some View {
        let eventEntries = annotationLegendEntries(
            model: model,
            annotations: annotationsForDrawing(eventAnnotations, track: .event),
            title: l10n.t("事件")
        )
        let stateEntries = annotationLegendEntries(
            model: model,
            annotations: annotationsForDrawing(stateAnnotations, track: .state),
            title: l10n.t("状态层")
        )
        let groups = [eventEntries, stateEntries].filter { !$0.items.isEmpty }
        if !groups.isEmpty {
            HStack(spacing: 12) {
                ForEach(groups, id: \.title) { group in
                    HStack(spacing: 5) {
                        Text(group.title)
                            .foregroundStyle(.tertiary)
                        ForEach(group.items, id: \.name) { entry in
                            HStack(spacing: 4) {
                                Rectangle()
                                    .fill(entry.color.opacity(0.82))
                                    .frame(width: 18, height: 4)
                                Text(entry.name)
                                    .foregroundStyle(.secondary)
                            }
                            .fixedSize(horizontal: true, vertical: false)
                        }
                    }
                    .fixedSize(horizontal: true, vertical: false)
                }
            }
            .font(.caption2)
            .padding(.horizontal, 8)
            .padding(.vertical, 5)
            .background(.thinMaterial, in: RoundedRectangle(cornerRadius: 8))
            .overlay {
                RoundedRectangle(cornerRadius: 8)
                    .stroke(Color(nsColor: .separatorColor).opacity(0.35), lineWidth: 1)
            }
        }
    }

    private func annotationLegendEntries(
        model: ISITimelinePlotModel,
        annotations: [ClassicAnchorEventAnnotation],
        title: String
    ) -> (title: String, items: [(name: String, color: Color)]) {
        guard !annotations.isEmpty else {
            return (title, [])
        }

        let annotationsByTrain = Dictionary(grouping: annotations, by: \.trainID)
        var labels = Set<ClassicAnchorLabel>()
        for (traceIndex, trace) in traces.enumerated() {
            guard let annotations = annotationsByTrain[trace.trainID], !annotations.isEmpty else {
                continue
            }
            let points = model.points(for: trace, traceIndex: traceIndex)
            guard !points.isEmpty else {
                continue
            }
            for annotation in annotations where !pointsForAnnotation(annotation, points: points).isEmpty {
                labels.insert(annotation.visualLabel)
            }
        }

        let items = ClassicAnchorLabel.allCases
            .filter { $0 != .reject }
            .filter { labels.contains($0) }
            .map { label in
                (name: legendName(for: label), color: eventColor(label))
            }
        return (title, items)
    }

    private func annotationsForDrawing(
        _ annotations: [ClassicAnchorEventAnnotation],
        track: ClassicAnchorSemanticTrack
    ) -> [ClassicAnchorEventAnnotation] {
        var combined = annotations.filter { $0.semanticTrack == track }
        if let focusedAnnotation,
           focusedAnnotation.semanticTrack == track,
           (reviewStatuses[focusedAnnotation.candidateID] ?? .unreviewed) != .rejected,
           !combined.contains(where: { $0.id == focusedAnnotation.id }) {
            combined.append(focusedAnnotation)
        }
        return combined
    }

    private func drawBackground(model: ISITimelinePlotModel, context: inout GraphicsContext) {
        let titleText = "\(timeMode.axisTitle), \(TimeFormatting.seconds(layout.timeRange.lowerBound)) to \(TimeFormatting.seconds(layout.timeRange.upperBound))"
        let title = Text(titleText)
            .font(.caption)
            .foregroundStyle(.secondary)
        context.draw(title, at: CGPoint(x: layout.dataRect.minX, y: 18), anchor: .leading)

        for index in traces.indices {
            drawPaneBackground(index: index, model: model, context: &context)
        }

        let yAxis = Text(yScale == .log ? "ISI (log \(displayUnit.rawValue))" : "ISI (\(displayUnit.rawValue))")
            .font(.caption2.weight(.semibold))
            .foregroundStyle(.secondary)
        context.draw(yAxis, at: CGPoint(x: 6, y: layout.plotRect.minY - 16), anchor: .leading)
    }

    private func drawPaneBackground(index: Int, model: ISITimelinePlotModel, context: inout GraphicsContext) {
        let paneRect = layout.paneRect(at: index)
        let paneDataRect = layout.dataRect(for: paneRect)
        let yRange = model.yRange(for: traces[index])
        let yTicks = model.yTicks(for: yRange)

        var border = Path()
        border.addRect(paneDataRect)
        context.stroke(border, with: .color(Color(nsColor: .separatorColor)), lineWidth: 1)

        for tick in yTicks {
            let y = model.yPosition(for: tick, in: paneRect, yRange: yRange)
            let labelY = min(max(y, paneDataRect.minY + 12), paneDataRect.maxY - 12)
            var grid = Path()
            grid.move(to: CGPoint(x: paneDataRect.minX, y: y))
            grid.addLine(to: CGPoint(x: paneDataRect.maxX, y: y))
            context.stroke(grid, with: .color(Color(nsColor: .gridColor).opacity(0.30)), lineWidth: 0.75)

            context.draw(
                Text(ISITimelineFormat.axisYTime(tick, unit: displayUnit))
                    .font(.caption2)
                    .foregroundStyle(.secondary),
                at: CGPoint(x: paneDataRect.minX - 8, y: labelY),
                anchor: .trailing
            )
        }
    }

    private func drawThresholds(model: ISITimelinePlotModel, context: inout GraphicsContext) {
        for index in traces.indices {
            let paneRect = layout.paneRect(at: index)
            let yRange = model.yRange(for: traces[index])
            var labels: [(text: String, color: Color)] = []

            drawThreshold(
                qualitySettings.artifactThresholdSec,
                color: ISIPlotPalette.artifactColor,
                paneRect: paneRect,
                yRange: yRange,
                model: model,
                context: &context
            ) {
                labels.append(("artifact \(formatTime(qualitySettings.artifactThresholdSec))", ISIPlotPalette.artifactColor))
            }

            drawThreshold(
                qualitySettings.refractorySuspectThresholdSec,
                color: .orange,
                paneRect: paneRect,
                yRange: yRange,
                model: model,
                context: &context
            ) {
                labels.append(("refractory \(formatTime(qualitySettings.refractorySuspectThresholdSec))", .orange))
            }

            drawThresholdLabels(labels, paneRect: paneRect, context: &context)
        }
    }

    private func drawStateAnnotations(model: ISITimelinePlotModel, context: inout GraphicsContext) {
        let annotationsToDraw = annotationsForDrawing(stateAnnotations, track: .state)
        guard !annotationsToDraw.isEmpty else {
            return
        }

        let annotationsByTrain = Dictionary(grouping: annotationsToDraw, by: \.trainID)
        for (traceIndex, trace) in traces.enumerated() {
            guard let annotations = annotationsByTrain[trace.trainID], !annotations.isEmpty else {
                continue
            }

            let paneIndex = layoutMode == .separateAxes ? traceIndex : 0
            let paneRect = layout.paneRect(at: paneIndex)
            let paneDataRect = layout.dataRect(for: paneRect)
            let dimmed = layoutMode == .overlay && !highlightedTrainIDs.isEmpty && !highlightedTrainIDs.contains(trace.id)

            for annotation in annotations.sorted(by: { lhs, rhs in
                if lhs.priority == rhs.priority {
                    return lhs.id < rhs.id
                }
                return lhs.priority < rhs.priority
            }) {
                let rawStart = annotationTimeStart(annotation)
                let rawEnd = annotationTimeEnd(annotation)
                let intervalStart = max(min(rawStart, rawEnd), layout.timeRange.lowerBound)
                let intervalEnd = min(max(rawStart, rawEnd), layout.timeRange.upperBound)
                guard intervalEnd >= intervalStart else {
                    continue
                }

                let x0 = layout.xPosition(for: intervalStart)
                let x1 = max(layout.xPosition(for: intervalEnd), x0 + 1)
                let slot = stateTrackSlot(annotation.label)
                let y = max(
                    paneDataRect.minY + 12,
                    paneDataRect.maxY - 10 - CGFloat(slot) * 7
                )

                var path = Path()
                path.move(to: CGPoint(x: x0, y: y))
                path.addLine(to: CGPoint(x: x1, y: y))

                let color = reviewedStateColor(annotation)
                let isFocused = annotation.id == focusedAnnotation?.id
                let hasFocus = focusedAnnotation != nil
                let reviewStatus = reviewStatuses[annotation.candidateID] ?? .unreviewed
                let opacity: Double
                if isFocused {
                    opacity = reviewStatus == .rejected ? 0.22 : 0.72
                } else if reviewStatus == .rejected {
                    opacity = 0.12
                } else if dimmed || hasFocus {
                    opacity = dimmed ? 0.10 : 0.18
                } else {
                    opacity = annotation.lockLevel == .lockedClassic ? 0.48 : 0.34
                }

                if isFocused {
                    context.stroke(
                        path,
                        with: .color(color.opacity(0.22)),
                        style: StrokeStyle(lineWidth: Self.stateTrackLineWidth + 5, lineCap: .round)
                    )
                }

                context.stroke(
                    path,
                    with: .color(color.opacity(opacity)),
                    style: StrokeStyle(lineWidth: Self.stateTrackLineWidth, lineCap: .round)
                )

                if isFocused {
                    let label = Text("状态 \(legendName(for: annotation.label))")
                        .font(.caption2.weight(.semibold))
                        .foregroundStyle(color.opacity(0.88))
                    context.draw(
                        label,
                        at: CGPoint(
                            x: min(max(x0 + 5, paneDataRect.minX + 5), paneDataRect.maxX - 64),
                            y: y - 10
                        ),
                        anchor: .leading
                    )
                }
            }
        }
    }

    private func drawEventAnnotations(model: ISITimelinePlotModel, context: inout GraphicsContext) {
        let annotationsToDraw = annotationsForDrawing(eventAnnotations, track: .event)
        guard !annotationsToDraw.isEmpty else {
            return
        }

        let annotationsByTrain = Dictionary(grouping: annotationsToDraw, by: \.trainID)
        for (traceIndex, trace) in traces.enumerated() {
            guard let annotations = annotationsByTrain[trace.trainID], !annotations.isEmpty else {
                continue
            }

            let paneIndex = layoutMode == .separateAxes ? traceIndex : 0
            let paneRect = layout.paneRect(at: paneIndex)
            let paneDataRect = layout.dataRect(for: paneRect)
            let dimmed = layoutMode == .overlay && !highlightedTrainIDs.isEmpty && !highlightedTrainIDs.contains(trace.id)
            let points = model.points(for: trace, traceIndex: traceIndex)
            guard !points.isEmpty else {
                continue
            }

            for annotation in annotations.sorted(by: { lhs, rhs in
                if lhs.priority == rhs.priority {
                    return lhs.id < rhs.id
                }
                return lhs.priority < rhs.priority
            }) {
                let eventPoints = pointsForAnnotation(annotation, points: points)
                guard !eventPoints.isEmpty else {
                    continue
                }

                var highlightedIntervals = Path()
                for point in eventPoints {
                    let segment = clampedAnnotationSegment(for: point, in: paneDataRect)
                    highlightedIntervals.move(to: segment.start)
                    highlightedIntervals.addLine(to: segment.end)
                }

                let color = reviewedEventColor(annotation)
                let isFocused = annotation.id == focusedAnnotation?.id
                let hasFocus = focusedAnnotation != nil
                let reviewStatus = reviewStatuses[annotation.candidateID] ?? .unreviewed
                let opacity: Double
                let lineWidth: CGFloat
                if isFocused {
                    opacity = reviewStatus == .rejected ? 0.20 : 0.34
                    lineWidth = Self.eventIntervalLineWidth + 2
                } else if reviewStatus == .rejected {
                    opacity = 0.08
                    lineWidth = Self.eventIntervalLineWidth
                } else if dimmed || hasFocus {
                    opacity = dimmed ? 0.07 : 0.11
                    lineWidth = Self.eventIntervalLineWidth
                } else {
                    opacity = annotation.lockLevel == .lockedClassic ? 0.20 : 0.15
                    lineWidth = Self.eventIntervalLineWidth
                }

                if isFocused {
                    context.stroke(
                        highlightedIntervals,
                        with: .color(color.opacity(0.22)),
                        style: StrokeStyle(lineWidth: lineWidth + 2.5, lineCap: .butt, lineJoin: .round)
                    )
                }

                context.stroke(
                    highlightedIntervals,
                    with: .color(color.opacity(opacity)),
                    style: StrokeStyle(lineWidth: lineWidth, lineCap: .butt, lineJoin: .round)
                )

                if isFocused, let labelLocation = eventLabelLocation(for: eventPoints, in: paneDataRect) {
                    let label = Text(annotationLegendName(annotation))
                        .font(.caption2.weight(.semibold))
                        .foregroundStyle(color.opacity(0.95))
                    context.draw(
                        label,
                        at: labelLocation,
                        anchor: .leading
                    )
                }
            }
        }
    }

    private func clampedIntervalSegment(
        for point: ISIPlotPoint,
        in paneDataRect: CGRect
    ) -> (start: CGPoint, end: CGPoint) {
        let y = point.location.y
        let minX = min(point.leftLocation.x, point.rightLocation.x)
        let maxX = max(point.leftLocation.x, point.rightLocation.x)
        if maxX - minX < 1 {
            let center = min(max(point.location.x, paneDataRect.minX + 1), paneDataRect.maxX - 1)
            return (
                CGPoint(x: center - 0.5, y: y),
                CGPoint(x: center + 0.5, y: y)
            )
        }
        return (
            CGPoint(x: max(minX, paneDataRect.minX), y: y),
            CGPoint(x: min(maxX, paneDataRect.maxX), y: y)
        )
    }

    private func clampedAnnotationSegment(
        for point: ISIPlotPoint,
        in paneDataRect: CGRect
    ) -> (start: CGPoint, end: CGPoint) {
        let base = clampedIntervalSegment(for: point, in: paneDataRect)
        let y = min(max(point.location.y + Self.eventIntervalYOffset, paneDataRect.minY + 2), paneDataRect.maxY - 2)
        return (
            CGPoint(x: base.start.x, y: y),
            CGPoint(x: base.end.x, y: y)
        )
    }

    private func pointsForAnnotation(
        _ annotation: ClassicAnchorEventAnnotation,
        points: [ISIPlotPoint]
    ) -> [ISIPlotPoint] {
        let lowerIndex = min(annotation.startISISecIndex, annotation.endISISecIndex)
        let upperIndex = max(annotation.startISISecIndex, annotation.endISISecIndex)
        guard lowerIndex <= upperIndex else {
            return []
        }

        return points
            .filter { point in
                point.event.intervalIndex >= lowerIndex &&
                    point.event.intervalIndex <= upperIndex
            }
            .sorted { lhs, rhs in
                lhs.event.intervalIndex < rhs.event.intervalIndex
            }
    }

    private func eventLabelLocation(for points: [ISIPlotPoint], in paneDataRect: CGRect) -> CGPoint? {
        guard !points.isEmpty else {
            return nil
        }

        let minX = points.map { min($0.leftLocation.x, $0.rightLocation.x) }.min() ?? paneDataRect.minX
        let minY = points.map(\.location.y).min() ?? paneDataRect.minY
        return CGPoint(
            x: min(max(minX + 5, paneDataRect.minX + 4), paneDataRect.maxX - 48),
            y: min(max(minY - 14, paneDataRect.minY + 12), paneDataRect.maxY - 10)
        )
    }

    private func stateTrackSlot(_ label: ClassicAnchorLabel) -> Int {
        switch label {
        case .tonic:
            return 0
        case .highFrequencyTonic:
            return 1
        case .highFrequencySpiking:
            return 2
        default:
            return 0
        }
    }

    private func drawThreshold(
        _ value: Double,
        color: Color,
        paneRect: CGRect,
        yRange: ClosedRange<Double>,
        model: ISITimelinePlotModel,
        context: inout GraphicsContext,
        lineWidth: CGFloat = 1,
        dash: [CGFloat] = [5, 4],
        onDrawn: () -> Void
    ) {
        guard value.isFinite, model.canPlotY(value, yRange: yRange) else {
            return
        }

        let paneDataRect = layout.dataRect(for: paneRect)
        let y = model.yPosition(for: value, in: paneRect, yRange: yRange)
        var path = Path()
        path.move(to: CGPoint(x: paneDataRect.minX, y: y))
        path.addLine(to: CGPoint(x: paneDataRect.maxX, y: y))
        context.stroke(path, with: .color(color.opacity(0.76)), style: StrokeStyle(lineWidth: lineWidth, dash: dash))
        onDrawn()
    }

    /// Phase 2A: overlay the active manual ISI threshold lines (solid = hard gate, dashed = soft
    /// anchor) in each visible pane, mirroring `drawThresholds`' per-pane / per-yRange handling.
    private func drawManualThresholdLines(model: ISITimelinePlotModel, context: inout GraphicsContext) {
        guard !thresholdLines.isEmpty else {
            return
        }
        for index in traces.indices {
            let paneRect = layout.paneRect(at: index)
            let paneDataRect = layout.dataRect(for: paneRect)
            let yRange = model.yRange(for: traces[index])
            for line in thresholdLines {
                guard model.canPlotY(line.isiSec, yRange: yRange) else {
                    continue
                }
                drawThreshold(
                    line.isiSec,
                    color: Self.manualThresholdColor,
                    paneRect: paneRect,
                    yRange: yRange,
                    model: model,
                    context: &context,
                    lineWidth: line.isHardGate ? 1.3 : 1,
                    dash: line.isHardGate ? [] : [4, 3]
                ) {}
                let y = model.yPosition(for: line.isiSec, in: paneRect, yRange: yRange)
                context.draw(
                    Text(line.label)
                        .font(.system(size: 9, weight: .medium))
                        .foregroundStyle(Self.manualThresholdColor.opacity(0.95)),
                    at: CGPoint(x: paneDataRect.minX + 5, y: y - 7),
                    anchor: .leading
                )
            }
        }
    }

    /// Phase 2A: draw the locked reference ISI line plus its similar-ISI tolerance band in every
    /// visible pane, so the reference can be compared against each train's ISIs on its own Y range.
    private func drawReferenceOverlay(model: ISITimelinePlotModel, context: inout GraphicsContext) {
        guard let reference, reference.isiSec.isFinite, reference.isiSec > 0 else {
            return
        }
        let tolerance = max(0, referenceTolerance)
        let low = reference.isiSec * (1 - tolerance)
        let high = reference.isiSec * (1 + tolerance)

        for index in traces.indices {
            let paneRect = layout.paneRect(at: index)
            let paneDataRect = layout.dataRect(for: paneRect)
            let yRange = model.yRange(for: traces[index])

            let bandLow = max(low, yRange.lowerBound)
            let bandHigh = min(high, yRange.upperBound)
            if bandHigh > bandLow {
                let yTop = model.yPosition(for: bandHigh, in: paneRect, yRange: yRange)
                let yBottom = model.yPosition(for: bandLow, in: paneRect, yRange: yRange)
                let bandRect = CGRect(
                    x: paneDataRect.minX,
                    y: min(yTop, yBottom),
                    width: paneDataRect.width,
                    height: abs(yBottom - yTop)
                )
                context.fill(Path(bandRect), with: .color(Self.referenceColor.opacity(0.10)))
            }

            if model.canPlotY(reference.isiSec, yRange: yRange) {
                let y = model.yPosition(for: reference.isiSec, in: paneRect, yRange: yRange)
                var path = Path()
                path.move(to: CGPoint(x: paneDataRect.minX, y: y))
                path.addLine(to: CGPoint(x: paneDataRect.maxX, y: y))
                context.stroke(path, with: .color(Self.referenceColor.opacity(0.92)), style: StrokeStyle(lineWidth: 1.6))
            }
        }
    }

    /// Phase 2A: ring every plotted ISI marker whose value is within the tolerance band of the
    /// locked reference, drawn on top of the traces so the "similar ISIs" stand out.
    private func drawSimilarISIHighlights(model: ISITimelinePlotModel, context: inout GraphicsContext) {
        guard let reference, reference.isiSec.isFinite, reference.isiSec > 0 else {
            return
        }
        let tolerance = max(0, referenceTolerance)
        let low = reference.isiSec * (1 - tolerance)
        let high = reference.isiSec * (1 + tolerance)
        let ringRadius = Self.isiMarkerRadius + 2.5

        for (traceIndex, trace) in traces.enumerated() {
            var rings = Path()
            for point in model.points(for: trace, traceIndex: traceIndex)
            where point.event.isiSec >= low && point.event.isiSec <= high {
                rings.addEllipse(
                    in: CGRect(
                        x: point.location.x - ringRadius,
                        y: point.location.y - ringRadius,
                        width: ringRadius * 2,
                        height: ringRadius * 2
                    )
                )
            }
            context.stroke(rings, with: .color(Self.referenceColor.opacity(0.95)), style: StrokeStyle(lineWidth: 1.5))
        }
    }

    private func drawThresholdLabels(
        _ labels: [(text: String, color: Color)],
        paneRect: CGRect,
        context: inout GraphicsContext
    ) {
        guard !labels.isEmpty else {
            return
        }

        let paneDataRect = layout.dataRect(for: paneRect)
        let startY = paneDataRect.minY + 13
        for (offset, label) in labels.enumerated() {
            let text = Text(label.text)
                .font(.caption2)
                .foregroundStyle(label.color)
            context.draw(
                text,
                at: CGPoint(x: paneDataRect.maxX - 8, y: startY + CGFloat(offset) * 15),
                anchor: .trailing
            )
        }
    }

    private func drawTraces(model: ISITimelinePlotModel, context: inout GraphicsContext) {
        for (traceIndex, trace) in traces.enumerated() {
            let points = model.points(for: trace, traceIndex: traceIndex)
            guard !points.isEmpty else {
                continue
            }

            let baseColor = traceColor(for: trace.id, index: traceIndex)
            let dimmed = layoutMode == .overlay && !highlightedTrainIDs.isEmpty && !highlightedTrainIDs.contains(trace.id)

            var isiSegments = Path()
            let paneIndex = layoutMode == .separateAxes ? traceIndex : 0
            let paneDataRect = layout.dataRect(for: layout.paneRect(at: paneIndex))
            for point in points {
                let segment = clampedIntervalSegment(for: point, in: paneDataRect)
                isiSegments.move(to: segment.start)
                isiSegments.addLine(to: segment.end)
            }
            context.stroke(
                isiSegments,
                with: .color(baseColor.opacity(dimmed ? 0.24 : 0.74)),
                style: StrokeStyle(lineWidth: dimmed ? 1.2 : Self.isiSegmentLineWidth, lineCap: .butt)
            )

            var normalDots = Path()
            var refractoryDots = Path()
            var artifactDots = Path()
            for point in points {
                let dotRadius = Self.isiMarkerRadius
                let rect = CGRect(
                    x: point.location.x - dotRadius,
                    y: point.location.y - dotRadius,
                    width: dotRadius * 2,
                    height: dotRadius * 2
                )
                if point.event.isArtifact {
                    artifactDots.addEllipse(in: rect)
                } else if point.event.isRefractorySuspect {
                    refractoryDots.addEllipse(in: rect)
                } else {
                    normalDots.addEllipse(in: rect)
                }
            }

            context.fill(normalDots, with: .color(baseColor.opacity(dimmed ? 0.26 : 0.58)))
            context.fill(refractoryDots, with: .color((dimmed ? Color.secondary : Color.orange).opacity(dimmed ? 0.28 : 0.82)))
            context.fill(artifactDots, with: .color((dimmed ? Color.secondary : ISIPlotPalette.artifactColor).opacity(dimmed ? 0.30 : 0.95)))
        }
    }

    private func drawXAxis(model: ISITimelinePlotModel, context: inout GraphicsContext) {
        let ticks = layout.timeTicks()
        guard !ticks.isEmpty else {
            return
        }

        let y = layout.plotRect.maxY + 32
        let dataRect = layout.dataRect
        let edgeInset: CGFloat = 18
        let minLabelSpacing: CGFloat = 72
        var lastDrawnX: CGFloat?

        for timestamp in ticks {
            let x = layout.xPosition(for: timestamp)
            guard x >= dataRect.minX - 1, x <= dataRect.maxX + 1 else {
                continue
            }

            let clampedX = min(max(x, dataRect.minX + edgeInset), dataRect.maxX - edgeInset)
            if let lastDrawnX, clampedX - lastDrawnX < minLabelSpacing {
                continue
            }

            let label = Text(ISITimelineFormat.axisTime(timestamp, tickInterval: layout.timeTickInterval))
                .font(.caption2)
                .foregroundStyle(.secondary)
            if clampedX <= dataRect.minX + edgeInset + 1 {
                context.draw(label, at: CGPoint(x: clampedX, y: y), anchor: .leading)
            } else if clampedX >= dataRect.maxX - edgeInset - 1 {
                context.draw(label, at: CGPoint(x: clampedX, y: y), anchor: .trailing)
            } else {
                context.draw(label, at: CGPoint(x: clampedX, y: y), anchor: .center)
            }
            lastDrawnX = clampedX
        }
    }

    private func updateHighlight(for trainID: String) {
        if NSEvent.modifierFlags.contains(.command) {
            if highlightedTrainIDs.contains(trainID) {
                highlightedTrainIDs.remove(trainID)
            } else {
                highlightedTrainIDs.insert(trainID)
            }
        } else if highlightedTrainIDs == [trainID] {
            highlightedTrainIDs = []
        } else {
            highlightedTrainIDs = [trainID]
        }
    }

    private func traceColor(for trainID: String, index: Int) -> Color {
        if layoutMode == .separateAxes || traces.count == 1 {
            return Color(nsColor: .labelColor)
        }
        if layoutMode == .overlay, !highlightedTrainIDs.isEmpty, !highlightedTrainIDs.contains(trainID) {
            return .secondary
        }
        return ISIPlotPalette.color(at: index)
    }

    private func annotationTimeStart(_ annotation: ClassicAnchorEventAnnotation) -> Double {
        switch timeMode {
        case .aligned:
            return annotation.alignedStartSec
        case .raw:
            return annotation.rawStartSec
        }
    }

    private func annotationTimeEnd(_ annotation: ClassicAnchorEventAnnotation) -> Double {
        switch timeMode {
        case .aligned:
            return annotation.alignedEndSec
        case .raw:
            return annotation.rawEndSec
        }
    }

    private func eventColor(_ annotation: ClassicAnchorEventAnnotation) -> Color {
        eventColor(annotation.visualLabel)
    }

    private func eventColor(_ label: ClassicAnchorLabel) -> Color {
        switch label {
        case .burst, .highFrequencyBurst, .longBurst, .possibleBurst:
            return Self.color(hex: 0xD55E00)
        case .tonic:
            return Self.color(hex: 0x009E73)
        case .highFrequencyTonic:
            return Self.color(hex: 0x35B779)
        case .highFrequencySpiking:
            return Self.color(hex: 0xCC79A7)
        case .pause:
            return Self.color(hex: 0x0072B2)
        case .reject, .profile:
            return .secondary
        }
    }

    private func legendName(for label: ClassicAnchorLabel) -> String {
        switch label {
        case .burst:
            return "burst"
        case .highFrequencyBurst:
            return "burst"
        case .longBurst:
            return "burst"
        case .possibleBurst:
            return "burst"
        case .tonic:
            return "tonic"
        case .highFrequencyTonic:
            return "HF tonic"
        case .highFrequencySpiking:
            return "HFS"
        case .pause:
            return "pause"
        case .reject:
            return "reject"
        case .profile:
            return "profile"
        }
    }

    private func annotationLegendName(_ annotation: ClassicAnchorEventAnnotation) -> String {
        annotation.displayFamilyName.lowercased()
    }

    private func reviewedEventColor(_ annotation: ClassicAnchorEventAnnotation) -> Color {
        switch reviewStatuses[annotation.candidateID] ?? .unreviewed {
        case .accepted:
            return .green
        case .rejected:
            return .red
        case .needsReview:
            return .orange
        case .unreviewed:
            return eventColor(annotation)
        }
    }

    private func reviewedStateColor(_ annotation: ClassicAnchorEventAnnotation) -> Color {
        switch reviewStatuses[annotation.candidateID] ?? .unreviewed {
        case .rejected:
            return .red
        case .accepted, .needsReview, .unreviewed:
            return eventColor(annotation)
        }
    }

    private static func color(hex: UInt32) -> Color {
        Color(
            red: Double((hex >> 16) & 0xFF) / 255.0,
            green: Double((hex >> 8) & 0xFF) / 255.0,
            blue: Double(hex & 0xFF) / 255.0
        )
    }

    @ViewBuilder
    private var focusedEventAnchor: some View {
        if let focusedAnnotation {
            let start = annotationTimeStart(focusedAnnotation)
            let end = annotationTimeEnd(focusedAnnotation)
            let center = min(max((start + end) / 2, layout.timeRange.lowerBound), layout.timeRange.upperBound)
            Color.clear
                .frame(width: 1, height: layout.contentHeight)
                .position(x: layout.xPosition(for: center), y: layout.contentHeight / 2)
                .id(ISITimelineFocusScrollID.event(focusedAnnotation.id))
        }
    }

    private func formatTime(_ seconds: Double) -> String {
        ISITimelineFormat.time(seconds, unit: displayUnit)
    }
}

private struct ISITimelineLayout {
    static let topInset: CGFloat = 66
    static let bottomInset: CGFloat = 66
    static let yAxisWidth: CGFloat = 76
    static let horizontalDataInset: CGFloat = 8
    static let minPaneHeight: CGFloat = 148
    static let maxPlotWidth: CGFloat = 80_000

    let labelWidth: CGFloat
    let plotViewportWidth: CGFloat
    let plotWidth: CGFloat
    let contentHeight: CGFloat
    let paneHeight: CGFloat
    let timeRange: RasterTimeRange
    let effectiveVisibleWindowSeconds: Double
    let layoutMode: ISITimelineLayoutMode

    init(
        containerSize: CGSize,
        traceCount: Int,
        timeRange: RasterTimeRange,
        visibleWindowSeconds: Double,
        layoutMode: ISITimelineLayoutMode
    ) {
        let containerWidth = max(containerSize.width, 460)
        let containerHeight = max(containerSize.height, 320)
        let labelWidth = layoutMode == .separateAxes ? min(max(containerWidth * 0.24, 210), 320) : 0
        let plotViewportWidth = max(containerWidth - labelWidth, 300)
        let paneCount = layoutMode == .separateAxes ? max(traceCount, 1) : 1

        let minContentHeight = Self.topInset + Self.bottomInset + Self.minPaneHeight * CGFloat(paneCount)
        let contentHeight = max(containerHeight, minContentHeight)
        let paneHeight = max(Self.minPaneHeight, (contentHeight - Self.topInset - Self.bottomInset) / CGFloat(paneCount))

        let duration = max(timeRange.duration, 0.001)
        let safeVisibleWindow = max(visibleWindowSeconds, 0.001)
        let requestedPointsPerSecond = max(Double(plotViewportWidth) / safeVisibleWindow, 1)
        let requestedPlotWidth = CGFloat(duration * requestedPointsPerSecond) + Self.yAxisWidth + Self.horizontalDataInset * 2
        let cappedPlotWidth = min(max(requestedPlotWidth, plotViewportWidth), Self.maxPlotWidth)
        let effectiveDataWidth = max(cappedPlotWidth - Self.yAxisWidth - Self.horizontalDataInset * 2, 1)
        let effectivePointsPerSecond = Double(effectiveDataWidth) / duration
        let effectiveVisibleWindowSeconds = min(duration, Double(plotViewportWidth - Self.yAxisWidth) / max(effectivePointsPerSecond, .leastNonzeroMagnitude))

        self.labelWidth = labelWidth
        self.plotViewportWidth = plotViewportWidth
        self.plotWidth = cappedPlotWidth
        self.contentHeight = contentHeight
        self.paneHeight = paneHeight
        self.timeRange = timeRange
        self.effectiveVisibleWindowSeconds = max(0.001, effectiveVisibleWindowSeconds)
        self.layoutMode = layoutMode
    }

    var plotRect: CGRect {
        CGRect(
            x: 0,
            y: Self.topInset,
            width: plotWidth,
            height: max(1, contentHeight - Self.topInset - Self.bottomInset)
        )
    }

    var dataRect: CGRect {
        dataRect(for: plotRect)
    }

    func dataRect(for paneRect: CGRect) -> CGRect {
        CGRect(
            x: paneRect.minX + Self.yAxisWidth,
            y: paneRect.minY,
            width: max(1, paneRect.width - Self.yAxisWidth - Self.horizontalDataInset),
            height: paneRect.height
        )
    }

    func paneRect(at index: Int) -> CGRect {
        switch layoutMode {
        case .separateAxes:
            return CGRect(
                x: plotRect.minX,
                y: plotRect.minY + paneHeight * CGFloat(index),
                width: plotRect.width,
                height: paneHeight
            )
        case .overlay:
            return plotRect
        }
    }

    func paneIndex(at location: CGPoint) -> Int? {
        guard plotRect.contains(location) else {
            return nil
        }
        switch layoutMode {
        case .separateAxes:
            let rawIndex = Int(((location.y - plotRect.minY) / paneHeight).rounded(.down))
            return rawIndex >= 0 ? rawIndex : nil
        case .overlay:
            return 0
        }
    }

    func xPosition(for timestamp: Double) -> CGFloat {
        let span = max(timeRange.duration, .leastNonzeroMagnitude)
        let fraction = CGFloat((timestamp - timeRange.lowerBound) / span)
        return dataRect.minX + dataRect.width * fraction
    }

    func timeTicks() -> [Double] {
        let tickInterval = timeTickInterval
        let duration = max(timeRange.duration, 0)
        guard duration > 0 else {
            return [timeRange.lowerBound]
        }

        let epsilon = tickInterval * 1e-6
        var tick = ceil((timeRange.lowerBound - epsilon) / tickInterval) * tickInterval
        var ticks: [Double] = []
        while tick <= timeRange.upperBound + epsilon {
            ticks.append(abs(tick) < epsilon ? 0 : tick)
            tick += tickInterval
            if ticks.count > 10_000 {
                break
            }
        }
        return ticks
    }

    var timeTickInterval: Double {
        Self.niceTimeInterval(for: max(effectiveVisibleWindowSeconds / 5, 1e-9))
    }

    private static func niceTimeInterval(for target: Double) -> Double {
        guard target.isFinite, target > 0 else {
            return 1
        }

        let magnitude = pow(10, floor(log10(target)))
        let normalized = target / magnitude
        let nice: Double
        if normalized <= 1 {
            nice = 1
        } else if normalized <= 2 {
            nice = 2
        } else if normalized <= 5 {
            nice = 5
        } else {
            nice = 10
        }
        return max(nice * magnitude, 1e-9)
    }

    static func timeRange(for traces: [SpikeISITrace], mode: RasterTimeMode) -> RasterTimeRange {
        let events = traces.flatMap(\.events)
        guard !events.isEmpty else {
            return RasterTimeRange(lowerBound: 0, upperBound: 1)
        }

        switch mode {
        case .aligned:
            let upper = events.map(\.alignedRightSpikeTimeSec).filter(\.isFinite).max() ?? 1
            return RasterTimeRange(lowerBound: 0, upperBound: max(upper, 0.001))
        case .raw:
            let lower = events.map(\.leftSpikeTimeSec).filter(\.isFinite).min() ?? 0
            let upper = events.map(\.rightSpikeTimeSec).filter(\.isFinite).max() ?? lower + 1
            return RasterTimeRange(lowerBound: lower, upperBound: max(lower + 0.001, upper))
        }
    }
}

private struct ISITimelinePlotModel {
    let traces: [SpikeISITrace]
    let layout: ISITimelineLayout
    let layoutMode: ISITimelineLayoutMode
    let timeMode: RasterTimeMode
    let displayUnit: QualityDisplayUnit
    let yScale: ISIYAxisScale
    let lockYAxis: Bool
    let qualitySettings: SpikeQualitySettings
    let sharedYRange: ClosedRange<Double>

    init(
        traces: [SpikeISITrace],
        layout: ISITimelineLayout,
        layoutMode: ISITimelineLayoutMode,
        timeMode: RasterTimeMode,
        displayUnit: QualityDisplayUnit,
        yScale: ISIYAxisScale,
        lockYAxis: Bool,
        qualitySettings: SpikeQualitySettings
    ) {
        self.traces = traces
        self.layout = layout
        self.layoutMode = layoutMode
        self.timeMode = timeMode
        self.displayUnit = displayUnit
        self.yScale = yScale
        self.lockYAxis = lockYAxis
        self.qualitySettings = qualitySettings
        self.sharedYRange = Self.yRange(
            for: traces.flatMap(\.events),
            scale: yScale,
            displayUnit: displayUnit,
            qualitySettings: qualitySettings
        )
    }

    func yRange(for trace: SpikeISITrace) -> ClosedRange<Double> {
        if layoutMode == .overlay || lockYAxis {
            return sharedYRange
        }
        return Self.yRange(
            for: trace.events,
            scale: yScale,
            displayUnit: displayUnit,
            qualitySettings: qualitySettings
        )
    }

    func yTicks(for range: ClosedRange<Double>) -> [Double] {
        switch yScale {
        case .linear:
            let upperScaled = max(range.upperBound * displayUnit.scaleFromSeconds, 0)
            let stepScaled = Self.niceLinearTickStep(for: max(upperScaled / 4, .leastNonzeroMagnitude), unit: displayUnit)
            guard stepScaled.isFinite, stepScaled > 0 else {
                return [0]
            }
            let tickCount = max(1, Int((upperScaled / stepScaled).rounded(.up)))
            return (0...tickCount).map { Double($0) * stepScaled / displayUnit.scaleFromSeconds }
        case .log:
            return Self.niceLogTicks(for: range, unit: displayUnit)
        }
    }

    func points(for trace: SpikeISITrace, traceIndex: Int) -> [ISIPlotPoint] {
        let paneIndex = layoutMode == .separateAxes ? traceIndex : 0
        let paneRect = layout.paneRect(at: paneIndex)
        let yRange = yRange(for: trace)

        return trace.events.compactMap { event in
            guard canPlotY(event.isiSec, yRange: yRange) else {
                return nil
            }
            return ISIPlotPoint(
                event: event,
                leftLocation: CGPoint(
                    x: layout.xPosition(for: Self.leftTime(for: event, mode: timeMode)),
                    y: yPosition(for: event.isiSec, in: paneRect, yRange: yRange)
                ),
                rightLocation: CGPoint(
                    x: layout.xPosition(for: Self.time(for: event, mode: timeMode)),
                    y: yPosition(for: event.isiSec, in: paneRect, yRange: yRange)
                )
            )
        }
    }

    func yPosition(for value: Double, in paneRect: CGRect, yRange: ClosedRange<Double>) -> CGFloat {
        let fraction: Double
        switch yScale {
        case .linear:
            let span = max(yRange.upperBound - yRange.lowerBound, .leastNonzeroMagnitude)
            fraction = (value - yRange.lowerBound) / span
        case .log:
            let lower = log10(max(yRange.lowerBound, .leastNonzeroMagnitude))
            let upper = log10(max(yRange.upperBound, yRange.lowerBound * 1.01))
            fraction = (log10(max(value, yRange.lowerBound)) - lower) / max(upper - lower, .leastNonzeroMagnitude)
        }
        return paneRect.maxY - paneRect.height * CGFloat(fraction)
    }

    func canPlotY(_ value: Double, yRange: ClosedRange<Double>) -> Bool {
        guard value.isFinite else {
            return false
        }
        switch yScale {
        case .linear:
            return value >= yRange.lowerBound && value <= yRange.upperBound
        case .log:
            return value > 0 && value >= yRange.lowerBound && value <= yRange.upperBound
        }
    }

    /// The ISI event nearest to a point within the marker/segment hit radius, or `nil`. Shared by the
    /// hover card and the Phase 2A click-to-lock-reference gesture so both resolve the same interval.
    func nearestEvent(at location: CGPoint) -> SpikeISIEvent? {
        guard layout.plotRect.insetBy(dx: 0, dy: -8).contains(location),
              let paneIndex = layout.paneIndex(at: location) else {
            return nil
        }

        let searchable: [(index: Int, trace: SpikeISITrace)]
        switch layoutMode {
        case .separateAxes:
            guard traces.indices.contains(paneIndex) else {
                return nil
            }
            searchable = [(paneIndex, traces[paneIndex])]
        case .overlay:
            searchable = Array(traces.enumerated()).map { ($0.offset, $0.element) }
        }

        var best: (event: SpikeISIEvent, distance: CGFloat)?
        for item in searchable {
            let points = points(for: item.trace, traceIndex: item.index)
            for point in points {
                let markerDistance = hypot(point.location.x - location.x, point.location.y - location.y)
                if markerDistance <= 8, best == nil || markerDistance < best!.distance {
                    best = (point.event, markerDistance)
                }
                let segmentDistance = distanceToSegment(location, point.leftLocation, point.rightLocation)
                if segmentDistance <= 7, best == nil || segmentDistance < best!.distance {
                    best = (point.event, segmentDistance)
                }
            }
        }

        return best?.event
    }

    func hoverTarget(at location: CGPoint, displayUnit: QualityDisplayUnit) -> ISIHoverTarget? {
        guard let event = nearestEvent(at: location) else {
            return nil
        }

        return ISIHoverTarget(
            trainID: event.trainID,
            trainName: event.trainName,
            rows: [
                ("Left", ISITimelineFormat.time(Self.leftTime(for: event, mode: timeMode), unit: displayUnit)),
                ("Right", ISITimelineFormat.time(Self.time(for: event, mode: timeMode), unit: displayUnit)),
                ("ISI", ISITimelineFormat.time(event.isiSec, unit: displayUnit)),
                ("QC", qcLabel(for: event))
            ]
        )
    }

    func hoverCardPosition(for location: CGPoint) -> CGPoint {
        let cardWidth: CGFloat = 310
        let cardHeight: CGFloat = 126
        let margin: CGFloat = 10
        let xCandidate = location.x + cardWidth / 2 + 16
        let x: CGFloat
        if xCandidate + cardWidth / 2 + margin > layout.plotWidth {
            x = max(cardWidth / 2 + margin, location.x - cardWidth / 2 - 16)
        } else {
            x = min(max(cardWidth / 2 + margin, xCandidate), layout.plotWidth - cardWidth / 2 - margin)
        }

        let yCandidate = location.y - cardHeight / 2 - 14
        let y: CGFloat
        if yCandidate - cardHeight / 2 - margin < 0 {
            y = min(layout.contentHeight - cardHeight / 2 - margin, location.y + cardHeight / 2 + 14)
        } else {
            y = min(max(cardHeight / 2 + margin, yCandidate), layout.contentHeight - cardHeight / 2 - margin)
        }
        return CGPoint(x: x, y: y)
    }

    private func qcLabel(for event: SpikeISIEvent) -> String {
        if event.isArtifact {
            return "Artifact"
        }
        if event.isRefractorySuspect {
            return "Refractory suspect"
        }
        return "OK"
    }

    private func distanceToSegment(_ point: CGPoint, _ a: CGPoint, _ b: CGPoint) -> CGFloat {
        let dx = b.x - a.x
        let dy = b.y - a.y
        let lengthSquared = dx * dx + dy * dy
        guard lengthSquared > .leastNonzeroMagnitude else {
            return hypot(point.x - a.x, point.y - a.y)
        }
        let t = min(max(((point.x - a.x) * dx + (point.y - a.y) * dy) / lengthSquared, 0), 1)
        let projection = CGPoint(x: a.x + t * dx, y: a.y + t * dy)
        return hypot(point.x - projection.x, point.y - projection.y)
    }

    static func time(for event: SpikeISIEvent, mode: RasterTimeMode) -> Double {
        switch mode {
        case .aligned:
            return event.alignedRightSpikeTimeSec
        case .raw:
            return event.rightSpikeTimeSec
        }
    }

    static func leftTime(for event: SpikeISIEvent, mode: RasterTimeMode) -> Double {
        switch mode {
        case .aligned:
            return event.alignedLeftSpikeTimeSec
        case .raw:
            return event.leftSpikeTimeSec
        }
    }

    private static func yRange(
        for events: [SpikeISIEvent],
        scale: ISIYAxisScale,
        displayUnit: QualityDisplayUnit,
        qualitySettings: SpikeQualitySettings
    ) -> ClosedRange<Double> {
        let thresholds = [
            qualitySettings.artifactThresholdSec,
            qualitySettings.refractorySuspectThresholdSec
        ].filter { $0.isFinite && $0 > 0 }

        switch scale {
        case .linear:
            let values = events.map(\.isiSec).filter { $0.isFinite && $0 >= 0 } + thresholds
            let rawUpperScaled = max((values.max() ?? 0.001) * displayUnit.scaleFromSeconds, 1)
            let stepScaled = niceLinearTickStep(for: rawUpperScaled / 4, unit: displayUnit)
            let upperScaled = max(stepScaled, (rawUpperScaled / stepScaled).rounded(.up) * stepScaled)
            return 0...(upperScaled / displayUnit.scaleFromSeconds)
        case .log:
            let values = events.map(\.isiSec).filter { $0.isFinite && $0 > 0 } + thresholds
            let minValue = max(values.min() ?? 0.0001, .leastNonzeroMagnitude)
            let maxValue = max(values.max() ?? 0.001, minValue * 1.01)
            return (minValue / 1.15)...(maxValue * 1.15)
        }
    }

    private static func niceLinearTickStep(for target: Double, unit: QualityDisplayUnit) -> Double {
        guard target.isFinite, target > 0 else {
            return unit == .milliseconds ? 1 : 1
        }

        let magnitude = pow(10, floor(log10(target)))
        let normalized = target / magnitude
        let nice: Double
        if normalized <= 1 {
            nice = 1
        } else if normalized <= 2 {
            nice = 2
        } else if normalized <= 2.5 {
            nice = 2.5
        } else if normalized <= 5 {
            nice = 5
        } else {
            nice = 10
        }

        let step = nice * magnitude
        if unit == .milliseconds {
            return max(1, step)
        }
        return max(step, 1e-9)
    }

    private static func niceLogTicks(for range: ClosedRange<Double>, unit: QualityDisplayUnit) -> [Double] {
        let lowerScaled = max(range.lowerBound * unit.scaleFromSeconds, .leastNonzeroMagnitude)
        let upperScaled = max(range.upperBound * unit.scaleFromSeconds, lowerScaled * 1.01)
        guard lowerScaled.isFinite, upperScaled.isFinite else {
            return []
        }

        let startPower = Int(floor(log10(lowerScaled))) - 1
        let endPower = Int(ceil(log10(upperScaled))) + 1
        let mantissas: [Double] = [1, 2, 5]
        let paddingFactor = 1.000001
        var ticks: [Double] = []

        for power in startPower...endPower {
            let magnitude = pow(10, Double(power))
            for mantissa in mantissas {
                let scaled = mantissa * magnitude
                if scaled >= lowerScaled / paddingFactor, scaled <= upperScaled * paddingFactor {
                    ticks.append(scaled / unit.scaleFromSeconds)
                }
            }
        }

        if ticks.count >= 2 {
            return Array(ticks.prefix(8))
        }

        return [
            range.lowerBound,
            sqrt(range.lowerBound * range.upperBound),
            range.upperBound
        ].filter { $0.isFinite && $0 > 0 }
    }
}

private struct ISIPlotPoint {
    let event: SpikeISIEvent
    let leftLocation: CGPoint
    let rightLocation: CGPoint

    var location: CGPoint {
        CGPoint(
            x: (leftLocation.x + rightLocation.x) / 2,
            y: (leftLocation.y + rightLocation.y) / 2
        )
    }
}

private struct ISIHoverTarget {
    let trainID: String
    let trainName: String
    let rows: [(label: String, value: String)]
}

private struct ISIHoverCard: View {
    let target: ISIHoverTarget

    var body: some View {
        VStack(alignment: .leading, spacing: 7) {
            Text("ISI")
                .font(.caption.weight(.semibold))
            Text(target.trainName)
                .font(.caption2)
                .foregroundStyle(.secondary)
                .lineLimit(1)
                .truncationMode(.middle)

            Divider()

            ForEach(target.rows.indices, id: \.self) { index in
                let row = target.rows[index]
                HStack(alignment: .firstTextBaseline, spacing: 8) {
                    Text(row.label)
                        .foregroundStyle(.secondary)
                        .frame(width: 52, alignment: .leading)
                    Text(row.value)
                        .monospacedDigit()
                        .lineLimit(1)
                        .truncationMode(.middle)
                    Spacer(minLength: 0)
                }
            }
        }
        .font(.caption2)
        .padding(10)
        .background(.regularMaterial, in: RoundedRectangle(cornerRadius: 8))
        .overlay {
            RoundedRectangle(cornerRadius: 8)
                .stroke(Color(nsColor: .separatorColor).opacity(0.55), lineWidth: 1)
        }
        .shadow(color: .black.opacity(0.16), radius: 10, y: 4)
    }
}

private enum ISIPlotPalette {
    static let artifactColor = Color(red: 255.0 / 255.0, green: 100.0 / 255.0, blue: 78.0 / 255.0)

    static func color(at index: Int) -> Color {
        let palette: [Color] = [.accentColor, .purple, .green, .cyan, .pink, .indigo, .teal, .brown]
        return palette[index % palette.count]
    }
}

private enum ISITimelineFormat {
    static func axisYTime(_ seconds: Double, unit: QualityDisplayUnit) -> String {
        guard seconds.isFinite else {
            return "NA"
        }

        let scaled = seconds * unit.scaleFromSeconds
        let cleanScaled = abs(scaled) < 1e-9 ? 0 : scaled
        let rounded = cleanScaled.rounded()
        if abs(cleanScaled - rounded) < 1e-6 {
            return "\(Int(rounded)) \(unit.rawValue)"
        }
        if abs(cleanScaled * 10 - (cleanScaled * 10).rounded()) < 1e-6 {
            return "\(String(format: "%.1f", cleanScaled)) \(unit.rawValue)"
        }
        if abs(cleanScaled * 100 - (cleanScaled * 100).rounded()) < 1e-6 {
            return "\(String(format: "%.2f", cleanScaled)) \(unit.rawValue)"
        }
        return "\(String(format: "%.3f", cleanScaled)) \(unit.rawValue)"
    }

    static func axisTime(_ seconds: Double, tickInterval: Double) -> String {
        guard seconds.isFinite else {
            return "NA"
        }

        let cleanSeconds = abs(seconds) < max(tickInterval, 1e-12) * 1e-6 ? 0 : seconds
        if tickInterval < 0.001 {
            return "\(Int((cleanSeconds * 1_000_000).rounded())) us"
        }
        if tickInterval < 1 {
            return "\(Int((cleanSeconds * 1_000).rounded())) ms"
        }

        let roundedSeconds = cleanSeconds.rounded()
        if abs(cleanSeconds - roundedSeconds) < max(tickInterval, 1e-12) * 1e-6 {
            return "\(Int(roundedSeconds)) s"
        }
        return "\(String(format: "%.1f", cleanSeconds)) s"
    }

    static func time(_ seconds: Double, unit: QualityDisplayUnit) -> String {
        guard seconds.isFinite else {
            return "NA"
        }

        let scaled = seconds * unit.scaleFromSeconds
        let suffix = unit.rawValue
        let magnitude = abs(scaled)
        if magnitude < 1e-12 {
            return "0 \(suffix)"
        }
        if magnitude >= 100 {
            return "\(String(format: "%.3f", scaled)) \(suffix)"
        }
        if magnitude >= 1 {
            return "\(String(format: "%.4f", scaled)) \(suffix)"
        }
        return "\(String(format: "%.6f", scaled)) \(suffix)"
    }
}
