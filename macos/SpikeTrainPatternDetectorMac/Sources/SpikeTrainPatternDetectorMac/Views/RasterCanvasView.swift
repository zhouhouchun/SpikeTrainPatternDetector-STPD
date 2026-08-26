import AppKit
import STPDCore
import SwiftUI

struct RasterCanvasView: View {
    let dataset: SpikeDataset
    let selectedTrainIDs: Set<String>
    let timeMode: RasterTimeMode
    let visibleWindowSeconds: Double
    let visibleWindowUnit: QualityDisplayUnit
    let artifactThresholdSeconds: Double
    let spikeTickHeightPx: CGFloat
    let eventAnnotations: [ClassicAnchorEventAnnotation]
    let focusedAnnotation: ClassicAnchorEventAnnotation?
    let focusedTimeRangeOverride: RasterTimeRange?
    let reviewStatuses: [String: ClassicAnchorReviewStatus]
    let focusRequestID: Int
    var adaptiveBurstBands: [String: AdaptiveBand] = [:]
    var manualPauseLowerSec: Double? = nil
    /// External task/stimulus events. They are display-only evidence and never enter detection.
    let taskEvents: [TaskEvent]
    let manualAnnotations: [ManualAnnotation]
    let manualAnnotationModeEnabled: Bool
    let showsISIInformationPanel: Bool
    let selectedManualLabel: ManualAnnotationLabel
    let manualAnnotationEraseModeEnabled: Bool
    let onApplyManualLabel: (ManualAnnotationLabel, String, Set<Int>) -> Void
    var onPinISI: (PinnedISIDiagnostic) -> Void = { _ in }

    private var visibleTrains: [SpikeTrain] {
        guard !selectedTrainIDs.isEmpty else {
            return []
        }
        let filtered = dataset.trains.filter { selectedTrainIDs.contains($0.id) }
        return filtered
    }

    var body: some View {
        GeometryReader { proxy in
            let trains = visibleTrains
            let baseTimeRange = trains.rasterTimeRange(mode: timeMode)
            // Lay out over the full range so the native horizontal ScrollView gives the
            // reviewer free panning to neighboring structures at the chosen zoom; the
            // focused candidate is centered via scrollToFocusedAnnotation, not by pinning
            // (and the window length is no longer overridden per candidate).
            let displayTimeRange = focusedTimeRangeOverride ?? baseTimeRange
            // Identity intentionally excludes focusRequestID: focusing/Center must NOT
            // rebuild the scroll view (which would reset it to the start and animate across
            // every structure). Focus recentres in place via onChange(of: focusRequestID).
            let displayTimeRangeID = "\(timeMode.rawValue)-\(displayTimeRange.lowerBound)-\(displayTimeRange.upperBound)"
            let layout = RasterLayout(
                containerSize: proxy.size,
                trainCount: trains.count,
                trainNames: trains.map(\.name),
                timeRange: displayTimeRange,
                visibleWindowSeconds: visibleWindowSeconds
            )
            let legendEntries = PatternLegendEntry.visibleEntries(
                annotations: eventAnnotations,
                manualAnnotations: manualAnnotations,
                trains: trains,
                trainIDs: Set(trains.map(\.id)),
                timeRange: layout.timeRange,
                timeMode: timeMode
            )

            Group {
                if trains.isEmpty {
                    ContentUnavailableView("没有可见的 spike train", systemImage: "waveform.path.ecg")
                        .frame(maxWidth: .infinity, maxHeight: .infinity)
                } else {
                    ScrollViewReader { verticalProxy in
                        ScrollView(.vertical) {
                            HStack(alignment: .top, spacing: 0) {
                                RasterLabelColumn(
                                    trains: trains,
                                    layout: layout,
                                    legendEntries: legendEntries
                                )
                                .frame(width: layout.labelWidth, height: layout.contentHeight)

                                ScrollViewReader { horizontalProxy in
                                    ScrollView(.horizontal) {
                                        RasterPlotCanvas(
                                            trains: trains,
                                            layout: layout,
                                            timeMode: timeMode,
                                            displayUnit: visibleWindowUnit,
                                            artifactThresholdSeconds: artifactThresholdSeconds,
                                            spikeTickHeightPx: spikeTickHeightPx,
                                            eventAnnotations: eventAnnotations,
                                            focusedAnnotation: focusedAnnotation,
                                            reviewStatuses: reviewStatuses,
                                            adaptiveBurstBands: adaptiveBurstBands,
                                            manualPauseLowerSec: manualPauseLowerSec,
                                            taskEvents: taskEvents,
                                            manualAnnotations: manualAnnotations,
                                            manualAnnotationModeEnabled: manualAnnotationModeEnabled,
                                            showsISIInformationPanel: showsISIInformationPanel,
                                            selectedManualLabel: selectedManualLabel,
                                            manualAnnotationEraseModeEnabled:
                                                manualAnnotationEraseModeEnabled,
                                            onApplyManualLabel: onApplyManualLabel,
                                            onPinISI: onPinISI
                                        )
                                        .frame(width: layout.plotWidth, height: layout.contentHeight)
                                    }
                                    .id(displayTimeRangeID)
                                    // Keep the native horizontal scroller clear of the workbench's
                                    // lower edge without changing the raster's allocated height.
                                    .padding(.bottom, 3)
                                    .frame(width: layout.plotViewportWidth, height: layout.contentHeight)
                                    .onAppear {
                                        scrollToFocusedAnnotation(
                                            verticalProxy: verticalProxy,
                                            horizontalProxy: horizontalProxy,
                                            trains: trains
                                        )
                                    }
                                    .onChange(of: focusRequestID) { _, _ in
                                        scrollToFocusedAnnotation(
                                            verticalProxy: verticalProxy,
                                            horizontalProxy: horizontalProxy,
                                            trains: trains
                                        )
                                    }
                                    .onChange(of: focusedAnnotation?.id) { _, _ in
                                        scrollToFocusedAnnotation(
                                            verticalProxy: verticalProxy,
                                            horizontalProxy: horizontalProxy,
                                            trains: trains
                                        )
                                    }
                                    .onChange(of: timeMode) { _, _ in
                                        scrollToFocusedAnnotation(
                                            verticalProxy: verticalProxy,
                                            horizontalProxy: horizontalProxy,
                                            trains: trains
                                        )
                                    }
                                    .onChange(of: selectedTrainIDs) { _, _ in
                                        scrollToFocusedAnnotation(
                                            verticalProxy: verticalProxy,
                                            horizontalProxy: horizontalProxy,
                                            trains: trains
                                        )
                                    }
                                    .onChange(of: visibleWindowSeconds) { _, _ in
                                        scrollToFocusedAnnotation(
                                            verticalProxy: verticalProxy,
                                            horizontalProxy: horizontalProxy,
                                            trains: trains
                                        )
                                    }
                                    .onChange(of: layout.plotWidth) { _, _ in
                                        scrollToFocusedAnnotation(
                                            verticalProxy: verticalProxy,
                                            horizontalProxy: horizontalProxy,
                                            trains: trains
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
            .preference(key: RasterMaxSpikeTickHeightPreferenceKey.self, value: layout.maxSpikeTickHeight)
        }
    }

    private func scrollToFocusedAnnotation(
        verticalProxy: ScrollViewProxy,
        horizontalProxy: ScrollViewProxy,
        trains: [SpikeTrain]
    ) {
        guard let focusedAnnotation,
              trains.contains(where: { $0.id == focusedAnnotation.trainID }) else {
            return
        }

        let scroll = {
            withAnimation(.easeInOut(duration: 0.18)) {
                verticalProxy.scrollTo(RasterFocusScrollID.train(focusedAnnotation.trainID), anchor: .center)
                horizontalProxy.scrollTo(RasterFocusScrollID.event(focusedAnnotation.id), anchor: .center)
            }
        }
        scroll()
        for delay in [0.08, 0.22, 0.46] {
            DispatchQueue.main.asyncAfter(deadline: .now() + delay) {
                scroll()
            }
        }
    }

}

private enum RasterFocusScrollID: Hashable {
    case train(String)
    case event(String)
}

private struct ManualDragPreview {
    let trainIndex: Int
    let startX: CGFloat
    let currentX: CGFloat
    let resolvedSpikeRange: ClosedRange<Int>?
    let isValid: Bool
}

enum RasterLabelWidthResolver {
    static let minimumWidth: CGFloat = 160
    static let maximumWidth: CGFloat = 280
    private static let minimumPlotViewportWidth: CGFloat = 260
    private static let horizontalPadding: CGFloat = 24

    static func width(
        trainNames: [String],
        fontSize: CGFloat,
        containerWidth: CGFloat
    ) -> CGFloat {
        let font = NSFont.systemFont(ofSize: fontSize)
        let measuredWidth = trainNames
            .map { name in
                (name as NSString).size(withAttributes: [.font: font]).width
            }
            .max() ?? 0
        let desiredWidth = ceil(measuredWidth) + horizontalPadding
        let availableWidth = max(minimumWidth, containerWidth - minimumPlotViewportWidth)
        let upperBound = min(maximumWidth, availableWidth)
        return min(max(desiredWidth, minimumWidth), upperBound)
    }
}

private struct RasterLayout {
    static let topInset: CGFloat = 66
    static let bottomInset: CGFloat = 54
    static let horizontalDataInset: CGFloat = 8
    static let minSpikeTickHeight: CGFloat = 4
    static let minLaneHeight: CGFloat = 56
    static let maxPlotWidth: CGFloat = 80_000

    let labelWidth: CGFloat
    let plotViewportWidth: CGFloat
    let plotWidth: CGFloat
    let contentHeight: CGFloat
    let laneHeight: CGFloat
    let timeRange: RasterTimeRange
    let effectiveVisibleWindowSeconds: Double

    init(
        containerSize: CGSize,
        trainCount: Int,
        trainNames: [String],
        timeRange: RasterTimeRange,
        visibleWindowSeconds: Double
    ) {
        let containerWidth = max(containerSize.width, 420)
        let containerHeight = max(containerSize.height, 280)
        let labelFontSize: CGFloat = trainCount > 12 ? 10 : 11
        let labelWidth = RasterLabelWidthResolver.width(
            trainNames: trainNames,
            fontSize: labelFontSize,
            containerWidth: containerWidth
        )
        let plotViewportWidth = max(containerWidth - labelWidth, 260)

        let trainCount = max(trainCount, 1)
        let minContentHeight = Self.topInset + Self.bottomInset + Self.minLaneHeight * CGFloat(trainCount)
        let contentHeight = max(containerHeight, minContentHeight)
        let laneHeight = max(Self.minLaneHeight, (contentHeight - Self.topInset - Self.bottomInset) / CGFloat(trainCount))

        let duration = max(timeRange.duration, 0.001)
        let safeVisibleWindow = max(visibleWindowSeconds, 0.001)
        let requestedPointsPerSecond = max(Double(plotViewportWidth) / safeVisibleWindow, 1)
        let requestedPlotWidth = CGFloat(duration * requestedPointsPerSecond)
        let cappedPlotWidth = min(max(requestedPlotWidth, plotViewportWidth), Self.maxPlotWidth)
        let effectivePointsPerSecond = Double(cappedPlotWidth) / duration
        let effectiveVisibleWindowSeconds = min(duration, Double(plotViewportWidth) / max(effectivePointsPerSecond, .leastNonzeroMagnitude))

        self.labelWidth = labelWidth
        self.plotViewportWidth = plotViewportWidth
        self.plotWidth = cappedPlotWidth
        self.contentHeight = contentHeight
        self.laneHeight = laneHeight
        self.timeRange = timeRange
        self.effectiveVisibleWindowSeconds = effectiveVisibleWindowSeconds
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
        let inset = min(Self.horizontalDataInset, max(0, plotRect.width / 2 - 1))
        return plotRect.insetBy(dx: inset, dy: 0)
    }

    func laneY(at index: Int) -> CGFloat {
        plotRect.minY + laneHeight * (CGFloat(index) + 0.5)
    }

    func xPosition(for timestamp: Double) -> CGFloat {
        let duration = max(timeRange.duration, .leastNonzeroMagnitude)
        let fraction = CGFloat((timestamp - timeRange.lowerBound) / duration)
        return dataRect.minX + dataRect.width * fraction
    }

    var maxSpikeTickHeight: CGFloat {
        max(Self.minSpikeTickHeight, laneHeight - 8)
    }

    func clampedSpikeTickHeight(_ height: CGFloat) -> CGFloat {
        min(max(height, Self.minSpikeTickHeight), maxSpikeTickHeight)
    }
}

private struct RasterLabelColumn: View {
    let trains: [SpikeTrain]
    let layout: RasterLayout
    let legendEntries: [PatternLegendEntry]

    var body: some View {
        VStack(spacing: 0) {
            ZStack(alignment: .bottomTrailing) {
                PatternLegendStrip(entries: legendEntries)
                    .padding(.trailing, 8)
                    .padding(.leading, 8)
                    .padding(.bottom, 7)
            }
            .frame(height: RasterLayout.topInset)
            ForEach(trains, id: \.id) { train in
                Text(train.name)
                    .font(.system(size: trains.count > 12 ? 10 : 11))
                    .foregroundStyle(.secondary)
                    .lineLimit(1)
                    .truncationMode(.middle)
                    .frame(maxWidth: .infinity, minHeight: layout.laneHeight, maxHeight: layout.laneHeight, alignment: .trailing)
                    .padding(.trailing, 12)
                    .id(RasterFocusScrollID.train(train.id))
            }
            Spacer()
                .frame(height: RasterLayout.bottomInset)
        }
        .background(Color(nsColor: .controlBackgroundColor))
    }
}

private struct RasterPlotCanvas: View {
    private static let artifactColor = Color(red: 255.0 / 255.0, green: 100.0 / 255.0, blue: 78.0 / 255.0)
    private static let patternStripLineWidth: CGFloat = 8.25
    private static let patternStripOpacity = 0.82
    private static let spikeTickLineWidth: CGFloat = 1.65

    let trains: [SpikeTrain]
    let layout: RasterLayout
    let timeMode: RasterTimeMode
    let displayUnit: QualityDisplayUnit
    let artifactThresholdSeconds: Double
    let spikeTickHeightPx: CGFloat
    let eventAnnotations: [ClassicAnchorEventAnnotation]
    let focusedAnnotation: ClassicAnchorEventAnnotation?
    let reviewStatuses: [String: ClassicAnchorReviewStatus]
    let adaptiveBurstBands: [String: AdaptiveBand]
    let manualPauseLowerSec: Double?
    let taskEvents: [TaskEvent]
    let manualAnnotations: [ManualAnnotation]
    let manualAnnotationModeEnabled: Bool
    let showsISIInformationPanel: Bool
    let selectedManualLabel: ManualAnnotationLabel
    let manualAnnotationEraseModeEnabled: Bool
    let onApplyManualLabel: (ManualAnnotationLabel, String, Set<Int>) -> Void
    let onPinISI: (PinnedISIDiagnostic) -> Void
    @Environment(\.l10n) private var l10n
    @State private var hoverLocation: CGPoint?
    @State private var manualDragPreview: ManualDragPreview?

    private static let minimumManualDragWidth: CGFloat = 6

    var body: some View {
        ZStack(alignment: .topLeading) {
            Canvas { context, _ in
                drawPlotBackground(context: &context)
                drawLaneAxis(context: &context)
                drawTaskEvents(context: &context)
                drawEventAnnotations(context: &context)
                drawFocusedAnnotation(context: &context)
                drawManualAnnotations(context: &context)
                drawManualDragPreview(context: &context)
                drawManualDragSpikeHighlight(context: &context)
                drawSpikeTicks(context: &context)
                drawXAxis(context: &context)
            }
            .background(Color(nsColor: .textBackgroundColor))

            if manualAnnotationModeEnabled {
                Color.clear
                    .contentShape(Rectangle())
                    .gesture(manualDragGesture)
                    // In annotation mode this transparent surface owns the drag gesture. Give it the
                    // hover tracker as well, otherwise SwiftUI can fail to deliver the pointer location
                    // to the enclosing ZStack while a reviewer moves between adjacent spikes.
                    .modifier(
                        RasterISIInformationHoverTracking(
                            isEnabled: showsISIInformationPanel,
                            hoverLocation: $hoverLocation
                        )
                    )
            }

            if showsISIInformationPanel,
               let hoverLocation,
               let target = hoverTarget(at: hoverLocation) {
                RasterHoverCard(
                    target: target,
                    highTransparency: manualAnnotationModeEnabled
                )
                    .frame(width: 310, alignment: .leading)
                    .position(hoverCardPosition(for: hoverLocation, rowCount: target.rows.count))
                    .allowsHitTesting(false)
            }

            focusedEventAnchor
        }
        .contentShape(Rectangle())
        .modifier(
            RasterISIInformationHoverTracking(
                isEnabled: showsISIInformationPanel && !manualAnnotationModeEnabled,
                hoverLocation: $hoverLocation
            )
        )
        .onTapGesture {
            guard showsISIInformationPanel,
                  !manualAnnotationModeEnabled,
                  let location = hoverLocation,
                  let diagnostic = pinnedDiagnostic(at: location) else { return }
            onPinISI(diagnostic)
        }
        .onChange(of: manualAnnotationModeEnabled) { _, enabled in
            if !enabled { manualDragPreview = nil }
        }
        .onChange(of: showsISIInformationPanel) { _, isVisible in
            if !isVisible { hoverLocation = nil }
        }
    }

    private func pinnedDiagnostic(at location: CGPoint) -> PinnedISIDiagnostic? {
        guard layout.plotRect.contains(location),
              let trainIndex = trainIndex(at: location),
              trains.indices.contains(trainIndex) else { return nil }
        let train = trains[trainIndex]
        let centerY = layout.laneY(at: trainIndex)
        let tolerance = min(max(layout.laneHeight * 0.34, 10), max(layout.laneHeight / 2 - 2, 10))
        guard abs(location.y - centerY) <= tolerance else { return nil }

        var best: (value: PinnedISIDiagnostic, distance: CGFloat)?
        for rightIndex in train.timestampsSec.indices.dropFirst() {
            let leftTime = train.rasterTimestamp(at: rightIndex - 1, mode: timeMode)
            let rightTime = train.rasterTimestamp(at: rightIndex, mode: timeMode)
            let leftX = layout.xPosition(for: leftTime)
            let rightX = layout.xPosition(for: rightTime)
            let low = min(leftX, rightX)
            let high = max(leftX, rightX)
            guard location.x >= low - 8, location.x <= high + 8,
                  train.isiSec.indices.contains(rightIndex),
                  let isi = train.isiSec[rightIndex], isi.isFinite else { continue }
            let annotation = annotationCoveringISI(train: train, rightIndex: rightIndex)
            let band = adaptiveBurstBands[train.id]
            let diagnosis = PerISIDiagnosticBuilder.diagnose(
                isiSec: isi,
                coveringCandidateLabel: annotation?.displayFamilyName,
                seedLowerSec: band?.seedLowerSec,
                seedUpperSec: band?.seedUpperSec,
                bridgeUpperSec: band?.bridgeUpperSec,
                manualPauseLowerSec: manualPauseLowerSec
            )
            let value = PinnedISIDiagnostic(
                trainID: train.id,
                trainName: train.name,
                isiIndex: rightIndex,
                leftTimestampSec: leftTime,
                rightTimestampSec: rightTime,
                isiSec: isi,
                reviewStatus: annotation.map {
                    (reviewStatuses[$0.candidateID] ?? .unreviewed).title
                },
                diagnostic: diagnosis
            )
            let distance = abs(location.x - (low + high) / 2)
            if best == nil || distance < best!.distance { best = (value, distance) }
        }
        return best?.value
    }

    private func drawPlotBackground(context: inout GraphicsContext) {
        let plotRect = layout.plotRect
        var bottomAxis = Path()
        bottomAxis.move(to: CGPoint(x: plotRect.minX, y: plotRect.maxY))
        bottomAxis.addLine(to: CGPoint(x: plotRect.maxX, y: plotRect.maxY))
        context.stroke(bottomAxis, with: .color(Color(nsColor: .separatorColor).opacity(0.75)), lineWidth: 1)

        let titleText = localizedTimeRangeTitle
        let title = Text(titleText)
            .font(.caption)
            .foregroundStyle(.secondary)
        context.draw(title, at: CGPoint(x: plotRect.minX, y: 18), anchor: .leading)
    }

    private var localizedTimeRangeTitle: String {
        let axis = l10n.t(timeMode.axisTitle)
        let lower = TimeFormatting.seconds(layout.timeRange.lowerBound)
        let upper = TimeFormatting.seconds(layout.timeRange.upperBound)
        switch l10n.language {
        case .zh:
            return "\(axis)，\(lower) 至 \(upper)"
        case .en:
            return "\(axis), \(lower) to \(upper)"
        case .ru:
            return "\(axis): \(lower)–\(upper)"
        }
    }

    private func drawLaneAxis(context: inout GraphicsContext) {
        var lanePath = Path()
        for (index, train) in trains.enumerated() {
            guard let firstIndex = train.timestampsSec.indices.first,
                  let lastIndex = train.timestampsSec.indices.last else {
                continue
            }

            let firstTimestamp = train.rasterTimestamp(at: firstIndex, mode: timeMode)
            let lastTimestamp = train.rasterTimestamp(at: lastIndex, mode: timeMode)
            guard firstTimestamp.isFinite, lastTimestamp.isFinite else {
                continue
            }

            let lowerTimestamp = min(firstTimestamp, lastTimestamp)
            let upperTimestamp = max(firstTimestamp, lastTimestamp)
            guard upperTimestamp >= layout.timeRange.lowerBound,
                  lowerTimestamp <= layout.timeRange.upperBound else {
                continue
            }

            let startTimestamp = max(lowerTimestamp, layout.timeRange.lowerBound)
            let endTimestamp = min(upperTimestamp, layout.timeRange.upperBound)
            let y = layout.laneY(at: index)
            lanePath.move(to: CGPoint(x: layout.xPosition(for: startTimestamp), y: y))
            lanePath.addLine(to: CGPoint(x: layout.xPosition(for: endTimestamp), y: y))
        }
        context.stroke(lanePath, with: .color(Color(nsColor: .separatorColor).opacity(0.15)), lineWidth: 4)
    }

    /// Draw global task/stimulus events only in raw time. In aligned mode every train has its own
    /// origin, so a shared vertical line would be scientifically ambiguous.
    private func drawTaskEvents(context: inout GraphicsContext) {
        guard !taskEvents.isEmpty else { return }
        let plotRect = layout.plotRect

        guard timeMode == .raw else {
            let note = Text("\(taskEvents.count) 个任务事件 · 原始时间（切换到“原始”查看）")
                .font(.caption2)
                .foregroundStyle(Color.red.opacity(0.7))
            context.draw(
                note,
                at: CGPoint(x: plotRect.maxX - 6, y: plotRect.minY + 8),
                anchor: .topTrailing
            )
            return
        }

        let style = StrokeStyle(lineWidth: 1, dash: [4, 3])
        for event in taskEvents {
            let time = event.timeSec
            guard time.isFinite,
                  time >= layout.timeRange.lowerBound,
                  time <= layout.timeRange.upperBound else { continue }
            let x = layout.xPosition(for: time)
            var line = Path()
            line.move(to: CGPoint(x: x, y: plotRect.minY))
            line.addLine(to: CGPoint(x: x, y: plotRect.maxY))
            context.stroke(line, with: .color(Color.red.opacity(0.55)), style: style)
        }
    }

    private func drawEventAnnotations(context: inout GraphicsContext) {
        guard !eventAnnotations.isEmpty else {
            return
        }

        let annotationsByTrain = Dictionary(grouping: eventAnnotations, by: \.trainID)
        for (trainIndex, train) in trains.enumerated() {
            guard let annotations = annotationsByTrain[train.id], !annotations.isEmpty else {
                continue
            }
            let y = layout.laneY(at: trainIndex)
            var patternSegments: [ClassicAnchorLabel: [(Double, Double)]] = [:]
            var focusedSegments: [ClassicAnchorLabel: [(Double, Double)]] = [:]

            for annotation in annotations.sorted(by: { lhs, rhs in
                if lhs.priority == rhs.priority {
                    return lhs.id < rhs.id
                }
                return lhs.priority < rhs.priority
            }) {
                let reviewStatus = reviewStatuses[annotation.candidateID] ?? .unreviewed
                guard reviewStatus != .rejected else {
                    continue
                }

                guard let coveredISIIndices = annotation.coveredISIIndices(in: train) else {
                    continue
                }

                var clippedSegments: [(Double, Double)] = []
                for rightIndex in coveredISIIndices {
                    let leftIndex = rightIndex - 1
                    guard train.timestampsSec.indices.contains(leftIndex),
                          train.timestampsSec.indices.contains(rightIndex) else {
                        continue
                    }

                    let leftTimestamp = train.rasterTimestamp(at: leftIndex, mode: timeMode)
                    let rightTimestamp = train.rasterTimestamp(at: rightIndex, mode: timeMode)
                    guard leftTimestamp.isFinite, rightTimestamp.isFinite else {
                        continue
                    }

                    let intervalStart = min(leftTimestamp, rightTimestamp)
                    let intervalEnd = max(leftTimestamp, rightTimestamp)
                    guard intervalEnd >= layout.timeRange.lowerBound,
                          intervalStart <= layout.timeRange.upperBound else {
                        continue
                    }

                    let clippedStart = max(intervalStart, layout.timeRange.lowerBound)
                    let clippedEnd = min(intervalEnd, layout.timeRange.upperBound)
                    guard clippedEnd >= clippedStart else {
                        continue
                    }

                    clippedSegments.append((clippedStart, clippedEnd))
                }

                guard !clippedSegments.isEmpty else {
                    continue
                }

                let isFocused = annotation.candidateID == focusedAnnotation?.candidateID
                let label = annotation.visualLabel
                patternSegments[label, default: []].append(contentsOf: clippedSegments)
                if isFocused {
                    focusedSegments[label, default: []].append(contentsOf: clippedSegments)
                }
            }

            drawSegmentGroups(
                context: &context,
                groups: patternSegments,
                y: y,
                opacity: Self.patternStripOpacity
            )
            for label in ClassicAnchorLabel.allCases {
                guard let segments = focusedSegments[label], !segments.isEmpty else {
                    continue
                }
                drawAnnotationSegments(
                    context: &context,
                    segments: mergedTimeSegments(segments),
                    y: y,
                    color: patternColor(label),
                    opacity: Self.patternStripOpacity,
                    drawFocusHalo: true
                )
            }
        }
    }

    private func drawSegmentGroups(
        context: inout GraphicsContext,
        groups: [ClassicAnchorLabel: [(Double, Double)]],
        y: CGFloat,
        opacity: Double
    ) {
        for label in ClassicAnchorLabel.allCases {
            guard let segments = groups[label], !segments.isEmpty else {
                continue
            }
            drawAnnotationSegments(
                context: &context,
                segments: mergedTimeSegments(segments),
                y: y,
                color: patternColor(label),
                opacity: opacity,
                drawFocusHalo: false
            )
        }
    }

    private func drawAnnotationSegments(
        context: inout GraphicsContext,
        segments: [(Double, Double)],
        y: CGFloat,
        color: Color,
        opacity: Double,
        drawFocusHalo: Bool
    ) {
        var path = Path()
        for segment in segments {
            let x0 = layout.xPosition(for: segment.0)
            let x1 = layout.xPosition(for: segment.1)
            path.move(to: CGPoint(x: x0, y: y))
            path.addLine(to: CGPoint(x: x1, y: y))
        }

        if drawFocusHalo {
            context.stroke(
                path,
                with: .color(color.opacity(0.20)),
                style: StrokeStyle(lineWidth: Self.patternStripLineWidth + 6, lineCap: .butt)
            )
        }
        context.stroke(
            path,
            with: .color(color.opacity(opacity)),
            style: StrokeStyle(lineWidth: Self.patternStripLineWidth, lineCap: .butt)
        )
    }

    private func mergedTimeSegments(_ segments: [(Double, Double)]) -> [(Double, Double)] {
        let sorted = segments
            .map { (min($0.0, $0.1), max($0.0, $0.1)) }
            .sorted { lhs, rhs in
                if lhs.0 != rhs.0 {
                    return lhs.0 < rhs.0
                }
                return lhs.1 < rhs.1
            }
        guard var current = sorted.first else {
            return []
        }

        var merged: [(Double, Double)] = []
        let tolerance = max(1e-12, layout.timeRange.duration * 1e-9)
        for segment in sorted.dropFirst() {
            if segment.0 <= current.1 + tolerance {
                current.1 = max(current.1, segment.1)
            } else {
                merged.append(current)
                current = segment
            }
        }
        merged.append(current)
        return merged
    }

    private func drawFocusedAnnotation(context: inout GraphicsContext) {
        guard let focusedAnnotation,
              (reviewStatuses[focusedAnnotation.candidateID] ?? .unreviewed) != .rejected,
              let trainIndex = trains.firstIndex(where: { $0.id == focusedAnnotation.trainID }),
              let coveredISIIndices = focusedAnnotation.coveredISIIndices(in: trains[trainIndex]) else {
            return
        }

        let train = trains[trainIndex]
        let y = layout.laneY(at: trainIndex)
        var focusPath = Path()
        var bracketPath = Path()
        var visibleBounds: (minX: CGFloat, maxX: CGFloat)?

        for rightIndex in coveredISIIndices {
            let leftIndex = rightIndex - 1
            guard train.timestampsSec.indices.contains(leftIndex),
                  train.timestampsSec.indices.contains(rightIndex) else {
                continue
            }

            let leftTimestamp = train.rasterTimestamp(at: leftIndex, mode: timeMode)
            let rightTimestamp = train.rasterTimestamp(at: rightIndex, mode: timeMode)
            guard leftTimestamp.isFinite, rightTimestamp.isFinite else {
                continue
            }

            let intervalStart = min(leftTimestamp, rightTimestamp)
            let intervalEnd = max(leftTimestamp, rightTimestamp)
            guard intervalEnd >= layout.timeRange.lowerBound,
                  intervalStart <= layout.timeRange.upperBound else {
                continue
            }

            let clippedStart = max(intervalStart, layout.timeRange.lowerBound)
            let clippedEnd = min(intervalEnd, layout.timeRange.upperBound)
            guard clippedEnd >= clippedStart else {
                continue
            }

            let x0 = layout.xPosition(for: clippedStart)
            let x1 = layout.xPosition(for: clippedEnd)
            focusPath.move(to: CGPoint(x: x0, y: y))
            focusPath.addLine(to: CGPoint(x: x1, y: y))

            let segmentBounds = (min(x0, x1), max(x0, x1))
            if let bounds = visibleBounds {
                visibleBounds = (min(bounds.minX, segmentBounds.0), max(bounds.maxX, segmentBounds.1))
            } else {
                visibleBounds = segmentBounds
            }
        }

        guard let visibleBounds else {
            return
        }

        let color = patternStripColor(focusedAnnotation)
        // Soft two-layer halo (the "shadow area") so only the structure currently under
        // review stands out — kept subtle with low-opacity flat layers and rounded ends.
        context.stroke(
            focusPath,
            with: .color(color.opacity(0.12)),
            style: StrokeStyle(lineWidth: Self.patternStripLineWidth + 14, lineCap: .round)
        )
        context.stroke(
            focusPath,
            with: .color(color.opacity(0.22)),
            style: StrokeStyle(lineWidth: Self.patternStripLineWidth + 8, lineCap: .round)
        )
        context.stroke(
            focusPath,
            with: .color(color.opacity(1.0)),
            style: StrokeStyle(lineWidth: Self.patternStripLineWidth + 2, lineCap: .butt)
        )

        let bracketHalfHeight = min(layout.laneHeight * 0.34, 24)
        bracketPath.move(to: CGPoint(x: visibleBounds.minX, y: y - bracketHalfHeight))
        bracketPath.addLine(to: CGPoint(x: visibleBounds.minX, y: y + bracketHalfHeight))
        bracketPath.move(to: CGPoint(x: visibleBounds.maxX, y: y - bracketHalfHeight))
        bracketPath.addLine(to: CGPoint(x: visibleBounds.maxX, y: y + bracketHalfHeight))
        context.stroke(
            bracketPath,
            with: .color(color.opacity(0.92)),
            style: StrokeStyle(lineWidth: 2, lineCap: .round)
        )
    }

    // MARK: - Detector-independent manual annotation

    private var manualDragGesture: some Gesture {
        DragGesture(minimumDistance: Self.minimumManualDragWidth, coordinateSpace: .local)
            .onChanged { value in
                guard let startIndex = trainIndex(atY: value.startLocation.y),
                      trainIndex(atY: value.location.y) == startIndex else {
                    manualDragPreview = trainIndex(atY: value.startLocation.y).map {
                        ManualDragPreview(
                            trainIndex: $0,
                            startX: value.startLocation.x,
                            currentX: value.location.x,
                            resolvedSpikeRange: nil,
                            isValid: false
                        )
                    }
                    return
                }
                manualDragPreview = makeManualDragPreview(
                    trainIndex: startIndex,
                    startX: value.startLocation.x,
                    currentX: value.location.x
                )
            }
            .onEnded { value in
                defer { manualDragPreview = nil }
                guard abs(value.location.x - value.startLocation.x)
                        >= Self.minimumManualDragWidth,
                      let startIndex = trainIndex(atY: value.startLocation.y),
                      trainIndex(atY: value.location.y) == startIndex,
                      trains.indices.contains(startIndex) else { return }

                let train = trains[startIndex]
                let rawRange = rawTimeRange(
                    train: train,
                    x0: value.startLocation.x,
                    x1: value.location.x
                )
                let candidate = ManualAnnotation(
                    trainID: train.id,
                    label: selectedManualLabel,
                    startSec: rawRange.lowerBound,
                    endSec: rawRange.upperBound
                )
                guard let covered = ManualAnnotationGeometryResolver
                    .resolve(annotation: candidate, in: train)
                    .coveredISIIndices else { return }
                onApplyManualLabel(selectedManualLabel, train.id, Set(covered))
            }
    }

    private func trainIndex(atY y: CGFloat) -> Int? {
        let plot = layout.plotRect
        guard layout.laneHeight > 0, y >= plot.minY, y < plot.maxY else { return nil }
        let index = Int((y - plot.minY) / layout.laneHeight)
        return trains.indices.contains(index) ? index : nil
    }

    private func displayTime(atX x: CGFloat) -> Double {
        let data = layout.dataRect
        let width = max(data.width, .leastNonzeroMagnitude)
        let fraction = Double(max(0, min(1, (x - data.minX) / width)))
        return layout.timeRange.lowerBound + fraction * layout.timeRange.duration
    }

    private func rawTimeRange(train: SpikeTrain, x0: CGFloat, x1: CGFloat) -> ClosedRange<Double> {
        let displayStart = displayTime(atX: min(x0, x1))
        let displayEnd = displayTime(atX: max(x0, x1))
        let first = train.firstTimestampSec ?? 0
        let aligned = timeMode == .aligned
        let rawStart = ManualAnnotationTimeConversion.rawSec(
            displaySec: displayStart,
            firstTimestampSec: first,
            aligned: aligned
        )
        let rawEnd = ManualAnnotationTimeConversion.rawSec(
            displaySec: displayEnd,
            firstTimestampSec: first,
            aligned: aligned
        )
        return min(rawStart, rawEnd)...max(rawStart, rawEnd)
    }

    private func makeManualDragPreview(
        trainIndex: Int,
        startX: CGFloat,
        currentX: CGFloat
    ) -> ManualDragPreview {
        let train = trains[trainIndex]
        let rawRange = rawTimeRange(train: train, x0: startX, x1: currentX)
        let candidate = ManualAnnotation(
            trainID: train.id,
            label: selectedManualLabel,
            startSec: rawRange.lowerBound,
            endSec: rawRange.upperBound
        )
        let geometry = ManualAnnotationGeometryResolver.resolve(
            annotation: candidate,
            in: train
        )
        return ManualDragPreview(
            trainIndex: trainIndex,
            startX: startX,
            currentX: currentX,
            resolvedSpikeRange: geometry.coveredSpikeIndices,
            isValid: geometry.coveredISIIndices != nil
        )
    }

    private func drawManualAnnotations(context: inout GraphicsContext) {
        let grouped = Dictionary(grouping: manualAnnotations, by: \.trainID)
        for (trainIndex, train) in trains.enumerated() {
            guard let annotations = grouped[train.id] else { continue }
            var rangesByLabel: [ManualAnnotationLabel: [ClosedRange<Int>]] = [:]
            for annotation in annotations {
                guard let range = cachedManualISIIndexRange(annotation, in: train) else { continue }
                rangesByLabel[annotation.label, default: []].append(range)
            }

            for label in ManualAnnotationLabel.allCases {
                guard let ranges = rangesByLabel[label], !ranges.isEmpty else { continue }
                let y = manualTrackY(label: label, trainIndex: trainIndex)
                var path = Path()
                for range in mergedManualISIRanges(ranges) {
                    let leftIndex = range.lowerBound - 1
                    let rightIndex = range.upperBound
                    guard train.timestampsSec.indices.contains(leftIndex),
                          train.timestampsSec.indices.contains(rightIndex) else { continue }
                    let left = train.rasterTimestamp(at: leftIndex, mode: timeMode)
                    let right = train.rasterTimestamp(at: rightIndex, mode: timeMode)
                    let lower = max(min(left, right), layout.timeRange.lowerBound)
                    let upper = min(max(left, right), layout.timeRange.upperBound)
                    guard upper >= lower else { continue }
                    path.move(to: CGPoint(x: layout.xPosition(for: lower), y: y))
                    path.addLine(to: CGPoint(x: layout.xPosition(for: upper), y: y))
                }

                context.stroke(
                    path,
                    with: .color(Color(nsColor: .textBackgroundColor)),
                    style: StrokeStyle(lineWidth: 7, lineCap: .butt)
                )
                context.stroke(
                    path,
                    with: .color(manualLabelColor(label).opacity(0.95)),
                    style: StrokeStyle(lineWidth: 4, lineCap: .butt)
                )
            }
        }
    }

    /// Raster labels authored in this app already contain resolved ISI indices.
    /// Reusing that immutable display cache avoids rescanning every timestamp on
    /// every Canvas redraw while horizontal scrolling.  Older/imported entries
    /// without a usable cache retain the established resolver as a safe fallback.
    private func cachedManualISIIndexRange(
        _ annotation: ManualAnnotation,
        in train: SpikeTrain
    ) -> ClosedRange<Int>? {
        guard annotation.trainID == train.id else { return nil }
        if let start = annotation.startISIIndex,
           let end = annotation.endISIIndex,
           start >= 1,
           end >= start,
           end < train.timestampsSec.count {
            return start...end
        }
        return ManualAnnotationGeometryResolver.resolve(annotation: annotation, in: train)
            .coveredISIIndices
    }

    /// Collapsing contiguous or overlapping intervals into one stroked segment
    /// preserves label geometry while eliminating thousands of redundant path
    /// strokes for dense manually reviewed regions.
    private func mergedManualISIRanges(
        _ ranges: [ClosedRange<Int>]
    ) -> [ClosedRange<Int>] {
        let ordered = ranges.sorted {
            if $0.lowerBound != $1.lowerBound { return $0.lowerBound < $1.lowerBound }
            return $0.upperBound < $1.upperBound
        }
        guard var current = ordered.first else { return [] }
        var merged: [ClosedRange<Int>] = []
        for range in ordered.dropFirst() {
            if range.lowerBound <= current.upperBound + 1 {
                current = current.lowerBound...max(current.upperBound, range.upperBound)
            } else {
                merged.append(current)
                current = range
            }
        }
        merged.append(current)
        return merged
    }

    private func drawManualDragPreview(context: inout GraphicsContext) {
        guard let preview = manualDragPreview,
              trains.indices.contains(preview.trainIndex) else { return }
        let y = manualTrackY(label: selectedManualLabel, trainIndex: preview.trainIndex)
        let x0 = max(layout.dataRect.minX, min(preview.startX, preview.currentX))
        let x1 = min(layout.dataRect.maxX, max(preview.startX, preview.currentX))
        guard x1 >= x0 else { return }
        let color = manualAnnotationEraseModeEnabled
            ? Color.red
            : (preview.isValid ? manualLabelColor(selectedManualLabel) : Color.red)

        // Keep the transient drag affordance visually consistent with a committed manual label.
        // A full-lane rectangle obscures spike geometry and can be mistaken for the final mark.
        // Flat line ends mark the exact selected time range without the visual overhang of a
        // rounded stroke cap.
        var rail = Path()
        rail.move(to: CGPoint(x: x0, y: y))
        rail.addLine(to: CGPoint(x: x1, y: y))
        context.stroke(
            rail,
            with: .color(Color(nsColor: .textBackgroundColor).opacity(0.96)),
            style: StrokeStyle(lineWidth: 8, lineCap: .butt)
        )
        context.stroke(
            rail,
            with: .color(color.opacity(preview.isValid ? 0.95 : 0.82)),
            style: StrokeStyle(
                lineWidth: 4,
                lineCap: .butt,
                dash: preview.isValid && !manualAnnotationEraseModeEnabled ? [] : [5, 4]
            )
        )

        let capHalfHeight = min(max(layout.laneHeight * 0.07, 4), 8)
        var caps = Path()
        for x in [x0, x1] {
            caps.move(to: CGPoint(x: x, y: y - capHalfHeight))
            caps.addLine(to: CGPoint(x: x, y: y + capHalfHeight))
        }
        context.stroke(
            caps,
            with: .color(color.opacity(0.88)),
            style: StrokeStyle(lineWidth: 2, lineCap: .butt)
        )
    }

    private func drawManualDragSpikeHighlight(context: inout GraphicsContext) {
        guard let preview = manualDragPreview,
              preview.isValid,
              let range = preview.resolvedSpikeRange,
              trains.indices.contains(preview.trainIndex) else { return }
        let train = trains[preview.trainIndex]
        let y = layout.laneY(at: preview.trainIndex)
        let halfHeight = layout.clampedSpikeTickHeight(spikeTickHeightPx) / 2 + 4
        var path = Path()
        for spikeIndex in range where train.timestampsSec.indices.contains(spikeIndex) {
            let time = train.rasterTimestamp(at: spikeIndex, mode: timeMode)
            guard time >= layout.timeRange.lowerBound,
                  time <= layout.timeRange.upperBound else { continue }
            let x = layout.xPosition(for: time)
            path.move(to: CGPoint(x: x, y: y - halfHeight))
            path.addLine(to: CGPoint(x: x, y: y + halfHeight))
        }
        context.stroke(
            path,
            with: .color((manualAnnotationEraseModeEnabled
                          ? Color.red
                          : manualLabelColor(selectedManualLabel)).opacity(0.34)),
            lineWidth: 5
        )
    }

    private func manualTrackY(label: ManualAnnotationLabel, trainIndex: Int) -> CGFloat {
        let offset: CGFloat = switch label.semanticTrack {
        case .state: 0.22
        case .event: 0.32
        case .other: 0.42
        }
        return layout.laneY(at: trainIndex) + layout.laneHeight * offset
    }

    private func manualLabelColor(_ label: ManualAnnotationLabel) -> Color {
        switch label {
        case .burst: .orange
        case .highFrequencyBurst: .red
        case .longBurst: .pink
        case .tonic: .green
        case .highFrequencyTonic: .teal
        case .highFrequencySpiking: .purple
        case .pause: .blue
        case .other: .gray
        case .notBurst: .secondary
        }
    }

    private func drawSpikeTicks(context: inout GraphicsContext) {
        for (index, train) in trains.enumerated() {
            drawSpikes(for: train, at: index, context: &context)
        }
    }

    private func drawSpikes(for train: SpikeTrain, at trainIndex: Int, context: inout GraphicsContext) {
        let y = layout.laneY(at: trainIndex)
        let tickHalfHeight = layout.clampedSpikeTickHeight(spikeTickHeightPx) / 2
        let y0 = y - tickHalfHeight
        let y1 = y + tickHalfHeight
        let density = Double(train.spikeCount) / Double(max(layout.plotRect.width, 1))

        if density > 6 {
            drawBinnedSpikes(for: train, y0: y0, y1: y1, context: &context)
        } else {
            var normalPath = Path()
            var artifactPath = Path()
            for index in train.timestampsSec.indices {
                let timestamp = train.rasterTimestamp(at: index, mode: timeMode)
                guard timestamp >= layout.timeRange.lowerBound, timestamp <= layout.timeRange.upperBound else {
                    continue
                }
                let x = layout.xPosition(for: timestamp)
                let isArtifact = isArtifactSpike(train: train, index: index)
                if isArtifact {
                    artifactPath.move(to: CGPoint(x: x, y: y0))
                    artifactPath.addLine(to: CGPoint(x: x, y: y1))
                } else {
                    normalPath.move(to: CGPoint(x: x, y: y0))
                    normalPath.addLine(to: CGPoint(x: x, y: y1))
                }
            }
            context.stroke(normalPath, with: .color(.primary), lineWidth: Self.spikeTickLineWidth)
            context.stroke(artifactPath, with: .color(Self.artifactColor), lineWidth: Self.spikeTickLineWidth)
        }
    }

    private func drawBinnedSpikes(for train: SpikeTrain, y0: CGFloat, y1: CGFloat, context: inout GraphicsContext) {
        let dataRect = layout.dataRect
        let binCount = max(1, Int(dataRect.width.rounded(.up)))
        var normalBins = Set<Int>()
        var artifactBins = Set<Int>()
        normalBins.reserveCapacity(min(train.spikeCount, binCount))
        artifactBins.reserveCapacity(min(train.spikeCount, binCount))

        for index in train.timestampsSec.indices {
            let timestamp = train.rasterTimestamp(at: index, mode: timeMode)
            guard timestamp >= layout.timeRange.lowerBound, timestamp <= layout.timeRange.upperBound else {
                continue
            }
            let x = layout.xPosition(for: timestamp)
            let bin = min(max(Int((x - dataRect.minX).rounded(.down)), 0), binCount - 1)
            if isArtifactSpike(train: train, index: index) {
                artifactBins.insert(bin)
            } else {
                normalBins.insert(bin)
            }
        }

        var normalPath = Path()
        for bin in normalBins.sorted() {
            let x = dataRect.minX + CGFloat(bin)
            normalPath.move(to: CGPoint(x: x, y: y0))
            normalPath.addLine(to: CGPoint(x: x, y: y1))
        }
        context.stroke(normalPath, with: .color(.primary), lineWidth: Self.spikeTickLineWidth)

        var artifactPath = Path()
        for bin in artifactBins.sorted() {
            let x = dataRect.minX + CGFloat(bin)
            artifactPath.move(to: CGPoint(x: x, y: y0))
            artifactPath.addLine(to: CGPoint(x: x, y: y1))
        }
        context.stroke(artifactPath, with: .color(Self.artifactColor), lineWidth: Self.spikeTickLineWidth)
    }

    private func isArtifactSpike(train: SpikeTrain, index: Int) -> Bool {
        let previousISI = index > 0 && train.isiSec.indices.contains(index) ? train.isiSec[index] : nil
        let nextIndex = index + 1
        let nextISI = train.timestampsSec.indices.contains(nextIndex) && train.isiSec.indices.contains(nextIndex) ? train.isiSec[nextIndex] : nil

        return isArtifactISI(previousISI) || isArtifactISI(nextISI)
    }

    private func isArtifactISI(_ isi: Double?) -> Bool {
        guard let isi, isi.isFinite else {
            return false
        }
        let threshold = max(0, artifactThresholdSeconds)
        let tolerance = max(1e-12, abs(threshold) * 1e-6)
        return isi < threshold - tolerance
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

    private func patternStripColor(_ annotation: ClassicAnchorEventAnnotation) -> Color {
        patternColor(annotation.visualLabel)
    }

    private func patternColor(_ label: ClassicAnchorLabel) -> Color {
        PatternLegendEntry.color(for: label)
    }

    @ViewBuilder
    private var focusedEventAnchor: some View {
        if let focusedAnnotation,
           (reviewStatuses[focusedAnnotation.candidateID] ?? .unreviewed) != .rejected {
            let start = annotationTimeStart(focusedAnnotation)
            let end = annotationTimeEnd(focusedAnnotation)
            if start.isFinite, end.isFinite {
                let lower = max(min(start, end), layout.timeRange.lowerBound)
                let upper = min(max(start, end), layout.timeRange.upperBound)
                let center = min(max((lower + upper) / 2, layout.timeRange.lowerBound), layout.timeRange.upperBound)
                let centerX = layout.xPosition(for: center)
                let markerWidth: CGFloat = 1
                // Real-frame scroll anchor: a leading-aligned marker whose layout frame is
                // centered at the event's x. ScrollViewReader.scrollTo(anchor: .center) then
                // centers the candidate itself. A .position-based anchor reports the whole
                // plot as its frame, so scrollTo would center the plot midpoint instead and
                // off-screen candidates would drift.
                HStack(spacing: 0) {
                    Color.clear
                        .frame(width: max(0, centerX - markerWidth / 2), height: 1)
                    Color.clear
                        .frame(width: markerWidth, height: layout.contentHeight)
                        .id(RasterFocusScrollID.event(focusedAnnotation.id))
                    Spacer(minLength: 0)
                }
                .frame(width: layout.plotWidth, height: layout.contentHeight, alignment: .topLeading)
                .allowsHitTesting(false)
            }
        }
    }

    private func hoverTarget(at location: CGPoint) -> RasterHoverTarget? {
        guard layout.plotRect.contains(location),
              let trainIndex = trainIndex(at: location),
              trains.indices.contains(trainIndex) else {
            return nil
        }

        let train = trains[trainIndex]
        return isiHoverTarget(at: location, train: train, trainIndex: trainIndex)
    }

    private func trainIndex(at location: CGPoint) -> Int? {
        let rawIndex = Int(((location.y - layout.plotRect.minY) / layout.laneHeight).rounded(.down))
        return trains.indices.contains(rawIndex) ? rawIndex : nil
    }

    private func isiHoverTarget(at location: CGPoint, train: SpikeTrain, trainIndex: Int) -> RasterHoverTarget? {
        let laneCenterY = layout.laneY(at: trainIndex)
        // The user is inspecting the interval between two spikes, not a narrow mathematical center
        // line. Accept the entire visible lane while still excluding the neighbouring train.
        let verticalTolerance = max(0, layout.laneHeight / 2 - 2)
        guard abs(location.y - laneCenterY) <= verticalTolerance else {
            return nil
        }

        var bestTarget: (target: RasterHoverTarget, distance: CGFloat, isArtifact: Bool, intervalWidth: CGFloat)?
        for rightIndex in train.timestampsSec.indices.dropFirst() {
            let leftIndex = rightIndex - 1
            let leftTimestamp = train.rasterTimestamp(at: leftIndex, mode: timeMode)
            let rightTimestamp = train.rasterTimestamp(at: rightIndex, mode: timeMode)
            guard rightTimestamp >= layout.timeRange.lowerBound,
                  leftTimestamp <= layout.timeRange.upperBound else {
                continue
            }

            let leftX = layout.xPosition(for: leftTimestamp)
            let rightX = layout.xPosition(for: rightTimestamp)
            let lowerX = min(leftX, rightX)
            let upperX = max(leftX, rightX)
            let intervalWidth = upperX - lowerX
            guard train.isiSec.indices.contains(rightIndex),
                  let isi = train.isiSec[rightIndex],
                  isi.isFinite else {
                continue
            }

            let isArtifact = isArtifactISI(isi)
            let annotation = annotationCoveringISI(train: train, rightIndex: rightIndex)
            var rows: [(label: String, value: String)] = [
                (l10n.t("左侧时间戳"), formatTime(leftTimestamp)),
                (l10n.t("右侧时间戳"), formatTime(rightTimestamp)),
                (l10n.t("ISI 间隔"), formatTime(isi))
            ]
            if let annotation {
                rows.append((l10n.t("自动检测"), localizedAutomaticResult(annotation)))
                rows.append((l10n.t("自动审核"), l10n.t((reviewStatuses[annotation.candidateID] ?? .unreviewed).title)))
            } else {
                rows.append((l10n.t("自动检测"), l10n.t("未检测到模式")))
            }
            let manualLabels = manualLabelsCoveringISI(train: train, rightIndex: rightIndex)
            rows.append((
                l10n.t("手工标记"),
                manualLabels.isEmpty
                    ? l10n.t("无")
                    : manualLabels.map(localizedManualLabel).joined(separator: " · ")
            ))
            rows.append((l10n.t("低于最小有效 ISI"), isArtifact ? l10n.t("是") : l10n.t("否")))

            let shortISIPadding: CGFloat
            if intervalWidth < 12 {
                shortISIPadding = isArtifact ? 16 : 8
            } else {
                shortISIPadding = 0
            }
            guard location.x >= lowerX - shortISIPadding,
                  location.x <= upperX + shortISIPadding else {
                continue
            }

            let midpoint = (lowerX + upperX) / 2
            let distance = abs(location.x - midpoint)
            let isDuplicateTimestamp = abs(isi) <= 1e-12
            let target = RasterHoverTarget(
                title: isDuplicateTimestamp ? l10n.t("重复时间戳") : l10n.t("ISI 详情"),
                trainName: train.name,
                rows: rows
            )
            if shouldPreferHoverCandidate(
                distance: distance,
                isArtifact: isArtifact,
                intervalWidth: intervalWidth,
                over: bestTarget
            ) {
                bestTarget = (target, distance, isArtifact, intervalWidth)
            }
        }

        return bestTarget?.target
    }

    /// Manual annotations are an independent, user-authored overlay. Showing them alongside — never
    /// instead of — the automatic result lets a reviewer compare the two sources without altering either.
    private func manualLabelsCoveringISI(train: SpikeTrain, rightIndex: Int) -> [ManualAnnotationLabel] {
        manualAnnotations
            .filter { annotation in
                annotation.trainID == train.id
                    && (ManualAnnotationGeometryResolver.resolve(annotation: annotation, in: train)
                        .coveredISIIndices?.contains(rightIndex) ?? false)
            }
            .map(\.label)
            .sorted { lhs, rhs in
                if lhs.semanticTrack == rhs.semanticTrack { return lhs.rawValue < rhs.rawValue }
                return lhs.semanticTrack.rawValue < rhs.semanticTrack.rawValue
            }
    }

    private func localizedAutomaticResult(_ annotation: ClassicAnchorEventAnnotation) -> String {
        let label: String
        switch annotation.label {
        case .burst, .highFrequencyBurst, .longBurst, .possibleBurst:
            label = l10n.t("爆发")
        case .tonic:
            label = l10n.t("强直发放")
        case .highFrequencyTonic:
            label = l10n.t("高频强直发放")
        case .highFrequencySpiking:
            label = l10n.t("高频连续发放")
        case .pause:
            label = l10n.t("暂停")
        case .reject:
            label = l10n.t("拒绝")
        case .profile:
            label = l10n.t("配置")
        }
        return label
    }

    private func localizedManualLabel(_ label: ManualAnnotationLabel) -> String {
        l10n.t(label.displayNameZH)
    }

    private func annotationCoveringISI(train: SpikeTrain, rightIndex: Int) -> ClassicAnchorEventAnnotation? {
        eventAnnotations
            .filter { annotation in
                guard (reviewStatuses[annotation.candidateID] ?? .unreviewed) != .rejected else {
                    return false
                }
                guard annotation.trainID == train.id,
                      let range = annotation.coveredISIIndices(in: train) else {
                    return false
                }
                return range.contains(rightIndex)
            }
            .sorted { lhs, rhs in
                let focusedCandidateID = (focusedAnnotation.flatMap {
                    (reviewStatuses[$0.candidateID] ?? .unreviewed) != .rejected ? $0.candidateID : nil
                })
                if lhs.candidateID == focusedCandidateID {
                    return true
                }
                if rhs.candidateID == focusedCandidateID {
                    return false
                }
                if lhs.priority == rhs.priority {
                    return lhs.id < rhs.id
                }
                return lhs.priority > rhs.priority
            }
            .first
    }

    private func shouldPreferHoverCandidate(
        distance: CGFloat,
        isArtifact: Bool,
        intervalWidth: CGFloat,
        over current: (target: RasterHoverTarget, distance: CGFloat, isArtifact: Bool, intervalWidth: CGFloat)?
    ) -> Bool {
        guard let current else {
            return true
        }

        let artifactHoverPriorityRadius: CGFloat = 16
        if isArtifact != current.isArtifact {
            if isArtifact && distance <= artifactHoverPriorityRadius {
                return true
            }
            if current.isArtifact && current.distance <= artifactHoverPriorityRadius {
                return false
            }
        }

        let tieTolerance: CGFloat = 0.5
        if abs(distance - current.distance) <= tieTolerance,
           intervalWidth != current.intervalWidth {
            return intervalWidth < current.intervalWidth
        }

        return distance < current.distance
    }

    private func hoverCardPosition(for location: CGPoint, rowCount: Int) -> CGPoint {
        let cardWidth: CGFloat = 310
        // Header + train name + divider + rows + padding. This keeps a full automatic/manual
        // comparison card inside the plot instead of positioning it as though it held only 3 rows.
        let cardHeight = min(CGFloat(86 + rowCount * 19), 250)
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

    private func formatTime(_ seconds: Double) -> String {
        guard seconds.isFinite else {
            return "NA"
        }

        let scaled = seconds * displayUnit.scaleFromSeconds
        let suffix = displayUnit.rawValue
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

    private func drawXAxis(context: inout GraphicsContext) {
        let ticks = timeTicks()
        guard let lastIndex = ticks.indices.last else {
            return
        }

        for index in ticks.indices {
            let tick = ticks[index]
            let label = Text(TimeFormatting.seconds(tick.timestamp))
                .font(.caption2)
                .foregroundStyle(.secondary)
            if index == 0 {
                context.draw(label, at: CGPoint(x: tick.x + 4, y: layout.plotRect.maxY + 20), anchor: .leading)
            } else if index == lastIndex {
                context.draw(label, at: CGPoint(x: tick.x - 4, y: layout.plotRect.maxY + 20), anchor: .trailing)
            } else {
                context.draw(label, at: CGPoint(x: tick.x, y: layout.plotRect.maxY + 20), anchor: .center)
            }
        }
    }

    private func timeTicks() -> [(timestamp: Double, x: CGFloat)] {
        let tickInterval = max(layout.effectiveVisibleWindowSeconds / 5, 1e-9)
        let duration = max(layout.timeRange.duration, 0)
        let tickCount = max(0, Int((duration / tickInterval).rounded(.down)))

        return (0...tickCount).map { index in
            let timestamp = layout.timeRange.lowerBound + Double(index) * tickInterval
            return (timestamp, layout.xPosition(for: timestamp))
        }
    }
}

/// Disabling the ISI information panel also removes its continuous-hover
/// tracking path, avoiding repeated hit testing while a manual drag is active.
private struct RasterISIInformationHoverTracking: ViewModifier {
    let isEnabled: Bool
    @Binding var hoverLocation: CGPoint?

    @ViewBuilder
    func body(content: Content) -> some View {
        if isEnabled {
            content.onContinuousHover { phase in
                switch phase {
                case .active(let location):
                    hoverLocation = location
                case .ended:
                    hoverLocation = nil
                }
            }
        } else {
            content
        }
    }
}

private struct RasterHoverTarget {
    let title: String
    let trainName: String
    let rows: [(label: String, value: String)]
}

private struct RasterHoverCard: View {
    let target: RasterHoverTarget
    let highTransparency: Bool

    var body: some View {
        VStack(alignment: .leading, spacing: 7) {
            HStack(spacing: 8) {
                Text(target.title)
                    .font(.caption.weight(.semibold))
                    .foregroundStyle(.primary)
                Spacer(minLength: 8)
            }

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
                        .frame(width: 74, alignment: .leading)
                    Text(row.value)
                        .foregroundStyle(.primary)
                        .monospacedDigit()
                        .lineLimit(1)
                        .truncationMode(.middle)
                    Spacer(minLength: 0)
                }
            }
        }
        .font(.caption2)
        .padding(10)
        .modifier(RasterHoverGlassPanel(highTransparency: highTransparency))
    }
}

private struct RasterHoverGlassPanel: ViewModifier {
    let highTransparency: Bool
    private let shape = RoundedRectangle(cornerRadius: 12, style: .continuous)

    @ViewBuilder
    func body(content: Content) -> some View {
        if #available(macOS 26.0, *) {
            if highTransparency {
                content
                    .background {
                        shape
                            .fill(Color(nsColor: .textBackgroundColor).opacity(0.025))
                            .glassEffect(.clear, in: shape)
                            .mask { RasterHoverLeadingEdgeMask() }
                    }
                    .overlay {
                        shape
                            .stroke(
                                LinearGradient(
                                    colors: [
                                        Color.white.opacity(0.20),
                                        Color(nsColor: .separatorColor).opacity(0.12)
                                    ],
                                    startPoint: .topLeading,
                                    endPoint: .bottomTrailing
                                ),
                                lineWidth: 0.75
                            )
                            .mask { RasterHoverLeadingEdgeMask() }
                    }
            } else {
                content
                    .background {
                        shape
                            .fill(Color(nsColor: .textBackgroundColor).opacity(0.18))
                    }
                    .glassEffect(.regular, in: shape)
                    .overlay {
                        shape
                            .stroke(
                                LinearGradient(
                                    colors: [
                                        Color.white.opacity(0.40),
                                        Color(nsColor: .separatorColor).opacity(0.22)
                                    ],
                                    startPoint: .topLeading,
                                    endPoint: .bottomTrailing
                                ),
                                lineWidth: 1
                            )
                    }
                    .shadow(color: .black.opacity(0.06), radius: 18, y: 5)
            }
        } else {
            if highTransparency {
                content
                    .background {
                        shape
                            .fill(.ultraThinMaterial)
                            .opacity(0.06)
                            .mask { RasterHoverLeadingEdgeMask() }
                    }
                    .overlay {
                        shape
                            .stroke(
                                Color(nsColor: .separatorColor).opacity(0.18),
                                lineWidth: 0.75
                            )
                            .mask { RasterHoverLeadingEdgeMask() }
                    }
            } else {
                content
                    .background {
                        shape
                            .fill(.ultraThinMaterial)
                            .opacity(0.88)
                    }
                    .overlay {
                        shape.stroke(
                            Color(nsColor: .separatorColor).opacity(0.38),
                            lineWidth: 1
                        )
                    }
                    .shadow(color: .black.opacity(0.06), radius: 18, y: 5)
            }
        }
    }
}

/// Masks the already-rendered glass layer, rather than changing its shape. This removes
/// the native leading refraction without introducing a new glass boundary at the fade.
private struct RasterHoverLeadingEdgeMask: View {
    var body: some View {
        LinearGradient(
            stops: [
                .init(color: .clear, location: 0),
                .init(color: .clear, location: 0.05),
                .init(color: .white, location: 0.13),
                .init(color: .white, location: 1)
            ],
            startPoint: .leading,
            endPoint: .trailing
        )
    }
}

struct RasterMaxSpikeTickHeightPreferenceKey: PreferenceKey {
    static let defaultValue: CGFloat = 50

    static func reduce(value: inout CGFloat, nextValue: () -> CGFloat) {
        value = nextValue()
    }
}
