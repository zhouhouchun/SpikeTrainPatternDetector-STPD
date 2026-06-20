import AppKit
import STPDCore
import SwiftUI

struct ISIStateSpaceView: View {
    @Bindable var document: RasterDocument

    @State private var singleTraceIndex = 0
    @State private var highlightedTrainIDs: Set<String> = []
    @State private var showsFeatureTable = true

    private var selectedTraces: [SpikeISIStateTrace] {
        guard let dataset = document.dataset, !document.isiStateSpaceSelectedTrainIDs.isEmpty else {
            return []
        }

        let selected = document.isiStateSpaceSelectedTrainIDs
        return dataset.trains
            .filter { selected.contains($0.id) }
            .map { SpikeISIStateTrace.build(for: $0, settings: document.qualitySettings) }
    }

    private var selectedFeatureTraces: [SpikeISIStateFeatureTrace] {
        SpikeISIStateFeatureTable.build(
            for: selectedTraces,
            options: featureOptions
        )
    }

    private var displayedFeatureTraces: [SpikeISIStateFeatureTrace] {
        switch document.isiStateSpaceLayoutMode {
        case .singleTrain:
            let traces = selectedFeatureTraces
            guard !traces.isEmpty else {
                return []
            }
            let index = min(max(singleTraceIndex, 0), traces.count - 1)
            return [traces[index]]
        case .overlay:
            return selectedFeatureTraces
        }
    }

    private var featureOptions: SpikeISIStateFeatureOptions {
        SpikeISIStateFeatureOptions(
            halfWindowK: document.isiStateSpaceHalfWindowK,
            scaling: document.isiStateSpaceScaling.featureScaling,
            winsorizeExtremeLogISI: document.isiStateSpaceWinsorizeExtremeLogISI,
            breakLongISI: document.isiStateSpaceBreakLongISI,
            breakThresholdSec: document.isiStateSpaceBreakThresholdMs / 1000
        )
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
            clampSingleTraceIndex()
            highlightedTrainIDs.formIntersection(Set(selectedTraceIDs))
        }
        .onChange(of: document.isiStateSpaceLayoutMode) { _, _ in
            clampSingleTraceIndex()
        }
    }

    @ViewBuilder
    private var content: some View {
        if document.dataset == nil {
            ContentUnavailableView(
                "ISI State Space Not Loaded",
                systemImage: "square.stack.3d.up",
                description: Text(document.lastErrorMessage ?? "Load a spike train dataset first.")
            )
            .frame(maxWidth: .infinity, maxHeight: .infinity)
        } else if selectedTraces.isEmpty {
            ContentUnavailableView(
                "No state-space trains selected",
                systemImage: "checklist",
                description: Text("Use the state-space train selector in the header to choose one or more spike trains.")
            )
            .frame(maxWidth: .infinity, maxHeight: .infinity)
        } else {
            VSplitView {
                ISIStateSpaceCanvasView(
                    traces: displayedFeatureTraces,
                    layoutMode: document.isiStateSpaceLayoutMode,
                    timeMode: document.isiStateSpaceTimeMode,
                    displayUnit: document.isiStateSpaceDisplayUnit,
                    axisScale: document.isiStateSpaceAxisScale,
                    qualitySettings: document.qualitySettings,
                    highlightedTrainIDs: $highlightedTrainIDs
                )
                .frame(minHeight: 320)

                if showsFeatureTable {
                    ISIStateSpaceFeatureTableView(
                        traces: displayedFeatureTraces,
                        timeMode: document.isiStateSpaceTimeMode,
                        displayUnit: document.isiStateSpaceDisplayUnit
                    )
                    .frame(minHeight: 170, idealHeight: 230)
                }
            }
        }
    }

    private var header: some View {
        VStack(alignment: .leading, spacing: 10) {
            HStack(alignment: .center, spacing: 14) {
                VStack(alignment: .leading, spacing: 4) {
                    HStack(alignment: .firstTextBaseline, spacing: 10) {
                        Image(systemName: "square.stack.3d.up")
                            .foregroundStyle(.secondary)
                        Text("ISI 状态空间")
                            .font(.headline)
                        Text("Live")
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
                stateSpaceTrainDisplayBlock
            }

            ScrollView(.horizontal) {
                controls
                    .fixedSize(horizontal: true, vertical: false)
            }
            .scrollIndicators(.hidden)

            if document.isiStateSpaceLayoutMode == .overlay, !selectedTraces.isEmpty {
                ISIStateSpaceLegendStrip(
                    traces: selectedFeatureTraces,
                    highlightedTrainIDs: $highlightedTrainIDs
                )
            }
        }
    }

    @ViewBuilder
    private var stateSpaceTrainDisplayBlock: some View {
        if let dataset = document.dataset {
            VStack(alignment: .leading, spacing: 7) {
                Text(SpikeTrainSelectionScope.isiStateSpace.title)
                    .font(.caption.weight(.semibold))
                    .foregroundStyle(.secondary)

                SpikeTrainCountControls(
                    document: document,
                    dataset: dataset,
                    scope: .isiStateSpace,
                    showsTitle: false
                )
            }
            .fixedSize(horizontal: true, vertical: false)
        }
    }

    private var controls: some View {
        HStack(alignment: .center, spacing: 18) {
            controlGroup("View") {
                GlassSegmentedControl(
                    options: ISIStateSpaceLayoutMode.allCases.map { ($0, $0.title) },
                    selection: $document.isiStateSpaceLayoutMode,
                    minSegmentWidth: 58
                )
                .frame(width: 152)
            }

            if document.isiStateSpaceLayoutMode == .singleTrain {
                singleTracePager
            }

            controlGroup("Timestamp") {
                GlassSegmentedControl(
                    options: RasterTimeMode.allCases.map { ($0, $0.title) },
                    selection: $document.isiStateSpaceTimeMode,
                    minSegmentWidth: 52
                )
                .frame(width: 150)
            }

            controlGroup("Axis") {
                GlassSegmentedControl(
                    options: ISIYAxisScale.allCases.map { ($0, $0.title) },
                    selection: $document.isiStateSpaceAxisScale,
                    minSegmentWidth: 42
                )
                .frame(width: 128)
            }

            controlGroup("Unit", spacing: 20) {
                GlassSegmentedControl(
                    options: [
                        (.milliseconds, "ms"),
                        (.seconds, "s")
                    ],
                    selection: $document.isiStateSpaceDisplayUnit,
                    minSegmentWidth: 30
                )
                .frame(width: 82)
            }

            controlGroup("Local k") {
                DebouncedIntField(
                    "k",
                    value: $document.isiStateSpaceHalfWindowK,
                    range: 1...10,
                    width: 46
                )
            }
            .help("Shiny parity: local ISI half-window parameter. It is reserved for the next state-space feature layers.")

            controlGroup("Scaling") {
                GlassSegmentedControl(
                    options: ISIStateSpaceScaling.allCases.map { ($0, $0.title) },
                    selection: $document.isiStateSpaceScaling,
                    minSegmentWidth: 56
                )
                .frame(width: 158)
            }
            .help("Shiny parity parameter for feature-space state extraction.")

            controlGroup("Label source") {
                Picker("", selection: $document.isiStateSpaceLabelSource) {
                    ForEach(ISIStateSpaceLabelSource.allCases) { source in
                        Text(source.title).tag(source)
                    }
                }
                .labelsHidden()
                .frame(width: 150)
            }
            .help("Shiny parity parameter for label-aware state-space views.")

            Toggle("Winsorize logISI", isOn: $document.isiStateSpaceWinsorizeExtremeLogISI)
                .toggleStyle(.checkbox)
                .disabled(document.isiStateSpaceAxisScale != .log)

            Toggle("Break long ISI", isOn: $document.isiStateSpaceBreakLongISI)
                .toggleStyle(.checkbox)

            Toggle("Feature table", isOn: $showsFeatureTable)
                .toggleStyle(.checkbox)

            controlGroup("Break at") {
                DebouncedDoubleField(
                    "150",
                    value: breakThresholdBinding,
                    width: 64,
                    maxFractionDigits: 4
                )
                Text(document.isiStateSpaceDisplayUnit.rawValue)
                    .foregroundStyle(.secondary)
            }

            Button {
                document.isiStateSpaceLayoutMode = .singleTrain
                document.isiStateSpaceAxisScale = .log
                document.isiStateSpaceDisplayUnit = .milliseconds
                document.isiStateSpaceHalfWindowK = 3
                document.isiStateSpaceScaling = .robust
                document.isiStateSpaceLabelSource = .auditFinal
                document.isiStateSpaceWinsorizeExtremeLogISI = true
                document.isiStateSpaceBreakLongISI = true
                document.isiStateSpaceBreakThresholdMs = 150
                singleTraceIndex = 0
                highlightedTrainIDs = []
            } label: {
                Label("Reset", systemImage: "arrow.counterclockwise")
            }
            .liquidGlassButtonStyle()
            .labelStyle(.iconOnly)
            .help("Reset state-space controls")
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

    @ViewBuilder
    private var singleTracePager: some View {
        if selectedTraces.count > 1 {
            HStack(spacing: 6) {
                Button {
                    singleTraceIndex = max(0, singleTraceIndex - 1)
                } label: {
                    Image(systemName: "chevron.left")
                }
                .liquidGlassButtonStyle()
                .disabled(singleTraceIndex == 0)

                Text("\(min(singleTraceIndex + 1, selectedTraces.count)) / \(selectedTraces.count)")
                    .font(.caption)
                    .foregroundStyle(.secondary)
                    .monospacedDigit()
                    .frame(width: 50)

                Button {
                    singleTraceIndex = min(selectedTraces.count - 1, singleTraceIndex + 1)
                } label: {
                    Image(systemName: "chevron.right")
                }
                .liquidGlassButtonStyle()
                .disabled(singleTraceIndex >= selectedTraces.count - 1)
            }
            .help("Step through the selected spike trains one at a time.")
        }
    }

    private var breakThresholdBinding: Binding<Double> {
        Binding(
            get: {
                max(0, document.isiStateSpaceBreakThresholdMs / 1000) * document.isiStateSpaceDisplayUnit.scaleFromSeconds
            },
            set: { newValue in
                document.isiStateSpaceBreakThresholdMs = max(0, newValue / document.isiStateSpaceDisplayUnit.scaleFromSeconds * 1000)
            }
        )
    }

    private var summaryText: String {
        let pointCount = selectedFeatureTraces.reduce(0) { $0 + $1.features.count }
        let singleText = document.isiStateSpaceLayoutMode == .singleTrain && selectedTraces.count > 1 ?
            ", train \(min(singleTraceIndex + 1, selectedTraces.count))/\(selectedTraces.count)" : ""
        return "\(selectedTraces.count) selected, \(pointCount.formatted()) ISI transition point(s)\(singleText)"
    }

    private var selectedTraceIDs: [String] {
        selectedTraces.map(\.id)
    }

    private func clampSingleTraceIndex() {
        guard !selectedTraces.isEmpty else {
            singleTraceIndex = 0
            return
        }
        singleTraceIndex = min(max(0, singleTraceIndex), selectedTraces.count - 1)
    }
}

private struct ISIStateSpaceLegendStrip: View {
    let traces: [SpikeISIStateFeatureTrace]
    @Binding var highlightedTrainIDs: Set<String>

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
                    .help("Click to focus this train. Command-click to focus multiple trains.")
                }

                if !highlightedTrainIDs.isEmpty {
                    Button("Show all") {
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
        return ISIStateSpacePalette.color(at: index)
    }
}

private struct ISIStateSpaceCanvasView: View {
    let traces: [SpikeISIStateFeatureTrace]
    let layoutMode: ISIStateSpaceLayoutMode
    let timeMode: RasterTimeMode
    let displayUnit: QualityDisplayUnit
    let axisScale: ISIYAxisScale
    let qualitySettings: SpikeQualitySettings
    @Binding var highlightedTrainIDs: Set<String>

    @State private var hoverLocation: CGPoint?

    var body: some View {
        GeometryReader { proxy in
            if traces.flatMap(\.features).isEmpty {
                ContentUnavailableView("No state-space points", systemImage: "square.stack.3d.up")
                    .frame(maxWidth: .infinity, maxHeight: .infinity)
            } else {
                let layout = ISIStateSpacePlotLayout(containerSize: proxy.size)
                let model = ISIStateSpacePlotModel(
                    traces: traces,
                    layout: layout,
                    layoutMode: layoutMode,
                    displayUnit: displayUnit,
                    axisScale: axisScale,
                    qualitySettings: qualitySettings,
                    highlightedTrainIDs: highlightedTrainIDs
                )

                ZStack(alignment: .topLeading) {
                    Canvas { context, _ in
                        drawBackground(model: model, context: &context)
                        drawThresholds(model: model, context: &context)
                        drawDiagonal(model: model, context: &context)
                        drawTraces(model: model, context: &context)
                    }
                    .background(Color(nsColor: .textBackgroundColor))

                    if let hoverLocation,
                       let target = model.hoverTarget(at: hoverLocation, timeMode: timeMode) {
                        ISIStateSpaceHoverCard(target: target)
                            .frame(width: 350, alignment: .leading)
                            .position(model.hoverCardPosition(for: hoverLocation))
                            .allowsHitTesting(false)
                    }
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
                            guard layoutMode == .overlay,
                                  abs(value.translation.width) < 3,
                                  abs(value.translation.height) < 3,
                                  let target = model.hoverTarget(at: value.location, timeMode: timeMode) else {
                                return
                            }
                            updateHighlight(for: target.trainID)
                        }
                )
            }
        }
        .background(Color(nsColor: .textBackgroundColor))
    }

    private func drawBackground(model: ISIStateSpacePlotModel, context: inout GraphicsContext) {
        var border = Path()
        border.addRect(model.layout.plotRect)
        context.stroke(border, with: .color(Color(nsColor: .separatorColor).opacity(0.8)), lineWidth: 1)

        let xTicks = model.axisTicks()
        let yTicks = xTicks
        for tick in xTicks {
            let x = model.xPosition(forTransformedValue: tick.value)
            var grid = Path()
            grid.move(to: CGPoint(x: x, y: model.layout.plotRect.minY))
            grid.addLine(to: CGPoint(x: x, y: model.layout.plotRect.maxY))
            context.stroke(grid, with: .color(Color(nsColor: .gridColor).opacity(0.18)), lineWidth: 0.75)
            context.draw(
                Text(tick.label)
                    .font(.caption2)
                    .foregroundStyle(.secondary),
                at: CGPoint(x: x, y: model.layout.plotRect.maxY + 22),
                anchor: .center
            )
        }

        for tick in yTicks {
            let y = model.yPosition(forTransformedValue: tick.value)
            var grid = Path()
            grid.move(to: CGPoint(x: model.layout.plotRect.minX, y: y))
            grid.addLine(to: CGPoint(x: model.layout.plotRect.maxX, y: y))
            context.stroke(grid, with: .color(Color(nsColor: .gridColor).opacity(0.18)), lineWidth: 0.75)
            context.draw(
                Text(tick.label)
                    .font(.caption2)
                    .foregroundStyle(.secondary),
                at: CGPoint(x: model.layout.plotRect.minX - 8, y: y),
                anchor: .trailing
            )
        }

        context.draw(
            Text("ISI\u{1d62} (\(axisUnitLabel))")
                .font(.caption.weight(.semibold))
                .foregroundStyle(.secondary),
            at: CGPoint(x: model.layout.plotRect.midX, y: model.layout.bounds.maxY - 10),
            anchor: .center
        )
        context.draw(
            Text("ISI\u{1d62}\u{208a}\u{2081} (\(axisUnitLabel))")
                .font(.caption.weight(.semibold))
                .foregroundStyle(.secondary),
            at: CGPoint(x: model.layout.yAxisTitleX, y: model.layout.plotRect.midY),
            anchor: .center
        )

        context.draw(
            Text(axisScale == .log ? "logISI phase portrait" : "ISI phase portrait")
                .font(.caption)
                .foregroundStyle(.secondary),
            at: CGPoint(x: model.layout.plotRect.minX, y: model.layout.plotRect.minY - 18),
            anchor: .leading
        )
    }

    private func drawThresholds(model: ISIStateSpacePlotModel, context: inout GraphicsContext) {
        let thresholds: [(value: Double, color: Color)] = [
            (qualitySettings.artifactThresholdSec, ISIStateSpacePalette.artifactColor),
            (qualitySettings.refractorySuspectThresholdSec, .orange)
        ]

        for threshold in thresholds {
            guard let transformed = model.transformedThreshold(for: threshold.value),
                  transformed >= model.axisRange.lowerBound,
                  transformed <= model.axisRange.upperBound else {
                continue
            }

            let x = model.xPosition(forTransformedValue: transformed)
            let y = model.yPosition(forTransformedValue: transformed)
            var vertical = Path()
            vertical.move(to: CGPoint(x: x, y: model.layout.plotRect.minY))
            vertical.addLine(to: CGPoint(x: x, y: model.layout.plotRect.maxY))
            var horizontal = Path()
            horizontal.move(to: CGPoint(x: model.layout.plotRect.minX, y: y))
            horizontal.addLine(to: CGPoint(x: model.layout.plotRect.maxX, y: y))
            let style = StrokeStyle(lineWidth: 1, dash: [5, 4])
            context.stroke(vertical, with: .color(threshold.color.opacity(0.42)), style: style)
            context.stroke(horizontal, with: .color(threshold.color.opacity(0.42)), style: style)
        }
    }

    private func drawDiagonal(model: ISIStateSpacePlotModel, context: inout GraphicsContext) {
        var diagonal = Path()
        diagonal.move(to: CGPoint(x: model.layout.plotRect.minX, y: model.layout.plotRect.maxY))
        diagonal.addLine(to: CGPoint(x: model.layout.plotRect.maxX, y: model.layout.plotRect.minY))
        context.stroke(
            diagonal,
            with: .color(Color.secondary.opacity(0.34)),
            style: StrokeStyle(lineWidth: 1, dash: [6, 5])
        )
    }

    private func drawTraces(model: ISIStateSpacePlotModel, context: inout GraphicsContext) {
        for (traceIndex, trace) in traces.enumerated() {
            let points = model.points(for: trace, traceIndex: traceIndex)
            guard !points.isEmpty else {
                continue
            }

            let baseColor = model.traceColor(for: trace.id, index: traceIndex)
            let dimmed = layoutMode == .overlay && !highlightedTrainIDs.isEmpty && !highlightedTrainIDs.contains(trace.id)

            var line = Path()
            var hasCurrentSubpath = false
            for index in points.indices {
                if index == 0 || model.shouldBreakLine(between: points[index - 1].feature, and: points[index].feature) {
                    line.move(to: points[index].location)
                    hasCurrentSubpath = true
                } else if hasCurrentSubpath {
                    line.addLine(to: points[index].location)
                }
            }
            context.stroke(line, with: .color(baseColor.opacity(dimmed ? 0.22 : 0.44)), lineWidth: dimmed ? 0.8 : 1.15)

            var okDots = Path()
            var refractoryDots = Path()
            var artifactDots = Path()
            var duplicateDots = Path()
            for point in points {
                let size: CGFloat = point.feature.qcStatus == .ok ? 4.6 : 6.0
                let rect = CGRect(
                    x: point.location.x - size / 2,
                    y: point.location.y - size / 2,
                    width: size,
                    height: size
                )
                switch point.feature.qcStatus {
                case .ok:
                    okDots.addEllipse(in: rect)
                case .refractory:
                    refractoryDots.addEllipse(in: rect)
                case .artifact:
                    artifactDots.addEllipse(in: rect)
                case .duplicate:
                    duplicateDots.addEllipse(in: rect)
                }
            }

            context.fill(okDots, with: .color(baseColor.opacity(dimmed ? 0.24 : 0.88)))
            context.fill(refractoryDots, with: .color((dimmed ? Color.secondary : Color.orange).opacity(dimmed ? 0.28 : 0.92)))
            context.fill(artifactDots, with: .color((dimmed ? Color.secondary : ISIStateSpacePalette.artifactColor).opacity(dimmed ? 0.30 : 0.96)))
            context.fill(duplicateDots, with: .color((dimmed ? Color.secondary : ISIStateSpacePalette.duplicateColor).opacity(dimmed ? 0.30 : 0.98)))
        }
    }

    private var axisUnitLabel: String {
        axisScale == .log ? "log \(displayUnit.rawValue)" : displayUnit.rawValue
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
}

private struct ISIStateSpacePlotLayout {
    let bounds: CGRect
    let plotRect: CGRect
    let yAxisTitleX: CGFloat

    init(containerSize: CGSize) {
        let width = max(containerSize.width, 520)
        let height = max(containerSize.height, 380)
        let leftInset: CGFloat = 112
        self.bounds = CGRect(origin: .zero, size: CGSize(width: width, height: height))
        self.plotRect = CGRect(
            x: leftInset,
            y: 46,
            width: max(1, width - leftInset - 32),
            height: max(1, height - 102)
        )
        self.yAxisTitleX = max(42, leftInset - 74)
    }
}

private struct ISIStateSpacePlotModel {
    let traces: [SpikeISIStateFeatureTrace]
    let layout: ISIStateSpacePlotLayout
    let layoutMode: ISIStateSpaceLayoutMode
    let displayUnit: QualityDisplayUnit
    let axisScale: ISIYAxisScale
    let qualitySettings: SpikeQualitySettings
    let highlightedTrainIDs: Set<String>
    let axisRange: ClosedRange<Double>
    let duplicateFloor: Double?

    init(
        traces: [SpikeISIStateFeatureTrace],
        layout: ISIStateSpacePlotLayout,
        layoutMode: ISIStateSpaceLayoutMode,
        displayUnit: QualityDisplayUnit,
        axisScale: ISIYAxisScale,
        qualitySettings: SpikeQualitySettings,
        highlightedTrainIDs: Set<String>
    ) {
        self.traces = traces
        self.layout = layout
        self.layoutMode = layoutMode
        self.displayUnit = displayUnit
        self.axisScale = axisScale
        self.qualitySettings = qualitySettings
        self.highlightedTrainIDs = highlightedTrainIDs

        let features = traces.flatMap(\.features)
        let values = features.flatMap {
            [
                Self.transformedValue(for: $0, side: .left, axisScale: axisScale, displayUnit: displayUnit),
                Self.transformedValue(for: $0, side: .right, axisScale: axisScale, displayUnit: displayUnit)
            ]
        }
        let range = Self.axisRange(
            transformedValues: values,
            axisScale: axisScale,
            duplicateFloorCandidate: features.contains { $0.leftLogWasFloored || $0.rightLogWasFloored } ? values.min() : nil
        )
        self.axisRange = range.range
        self.duplicateFloor = range.duplicateFloor
    }

    func points(for trace: SpikeISIStateFeatureTrace, traceIndex: Int) -> [ISIStateSpacePlotPoint] {
        trace.features.compactMap { feature in
            guard canPlotFeature(feature) else {
                return nil
            }

            return ISIStateSpacePlotPoint(
                feature: feature,
                location: CGPoint(
                    x: xPosition(forTransformedValue: transformedValue(for: feature, side: .left)),
                    y: yPosition(forTransformedValue: transformedValue(for: feature, side: .right))
                )
            )
        }
    }

    func traceColor(for trainID: String, index: Int) -> Color {
        if layoutMode == .overlay, !highlightedTrainIDs.isEmpty, !highlightedTrainIDs.contains(trainID) {
            return .secondary
        }
        return ISIStateSpacePalette.color(at: index)
    }

    enum StateFeatureSide {
        case left
        case right
    }

    func transformedValue(for feature: SpikeISIStateFeature, side: StateFeatureSide) -> Double {
        Self.transformedValue(for: feature, side: side, axisScale: axisScale, displayUnit: displayUnit)
    }

    static func transformedValue(
        for feature: SpikeISIStateFeature,
        side: StateFeatureSide,
        axisScale: ISIYAxisScale,
        displayUnit: QualityDisplayUnit
    ) -> Double {
        switch axisScale {
        case .linear:
            let isiSec = side == .left ? feature.leftISISec : feature.rightISISec
            return max(0, isiSec * displayUnit.scaleFromSeconds)
        case .log:
            let logSeconds = side == .left ? feature.log10LeftISISec : feature.log10RightISISec
            return logSeconds + log10(displayUnit.scaleFromSeconds)
        }
    }

    func transformedThreshold(for isiSec: Double) -> Double? {
        guard isiSec.isFinite, isiSec > 0 else {
            return nil
        }
        switch axisScale {
        case .linear:
            return isiSec * displayUnit.scaleFromSeconds
        case .log:
            return log10(isiSec * displayUnit.scaleFromSeconds)
        }
    }

    func canPlotFeature(_ feature: SpikeISIStateFeature) -> Bool {
        let left = transformedValue(for: feature, side: .left)
        let right = transformedValue(for: feature, side: .right)
        return left.isFinite && right.isFinite &&
            left >= axisRange.lowerBound && left <= axisRange.upperBound &&
            right >= axisRange.lowerBound && right <= axisRange.upperBound
    }

    func xPosition(forTransformedValue value: Double) -> CGFloat {
        let span = max(axisRange.upperBound - axisRange.lowerBound, .leastNonzeroMagnitude)
        let fraction = (value - axisRange.lowerBound) / span
        return layout.plotRect.minX + layout.plotRect.width * CGFloat(fraction)
    }

    func yPosition(forTransformedValue value: Double) -> CGFloat {
        let span = max(axisRange.upperBound - axisRange.lowerBound, .leastNonzeroMagnitude)
        let fraction = (value - axisRange.lowerBound) / span
        return layout.plotRect.maxY - layout.plotRect.height * CGFloat(fraction)
    }

    func shouldBreakLine(between left: SpikeISIStateFeature, and right: SpikeISIStateFeature) -> Bool {
        left.isLongISIBreak || right.isLongISIBreak
    }

    func axisTicks() -> [ISIStateSpaceAxisTick] {
        switch axisScale {
        case .linear:
            return linearTicks()
        case .log:
            return logTicks()
        }
    }

    func hoverTarget(at location: CGPoint, timeMode: RasterTimeMode) -> ISIStateSpaceHoverTarget? {
        guard layout.plotRect.insetBy(dx: -10, dy: -10).contains(location) else {
            return nil
        }

        var best: (feature: SpikeISIStateFeature, distance: CGFloat)?
        for (traceIndex, trace) in traces.enumerated() {
            for plotPoint in points(for: trace, traceIndex: traceIndex) {
                let distance = hypot(plotPoint.location.x - location.x, plotPoint.location.y - location.y)
                if distance <= 10, best == nil || distance < best!.distance {
                    best = (plotPoint.feature, distance)
                }
            }
        }

        guard let feature = best?.feature else {
            return nil
        }
        let point = feature.point

        return ISIStateSpaceHoverTarget(
            trainID: point.trainID,
            trainName: point.trainName,
            rows: [
                ("Left ISI", formatISI(point.leftISISec)),
                ("Right ISI", formatISI(point.rightISISec)),
                ("Spike 1", timestamp(point, position: .first, timeMode: timeMode)),
                ("Spike 2", timestamp(point, position: .middle, timeMode: timeMode)),
                ("Spike 3", timestamp(point, position: .last, timeMode: timeMode)),
                ("Scaled", "\(Self.formatScore(feature.scaledLeft)), \(Self.formatScore(feature.scaledRight))"),
                ("QC", feature.qcStatus.title)
            ]
        )
    }

    func hoverCardPosition(for location: CGPoint) -> CGPoint {
        let cardWidth: CGFloat = 350
        let cardHeight: CGFloat = 178
        let margin: CGFloat = 12
        let xCandidate = location.x + cardWidth / 2 + 16
        let x: CGFloat
        if xCandidate + cardWidth / 2 + margin > layout.bounds.maxX {
            x = max(cardWidth / 2 + margin, location.x - cardWidth / 2 - 16)
        } else {
            x = min(max(cardWidth / 2 + margin, xCandidate), layout.bounds.maxX - cardWidth / 2 - margin)
        }

        let yCandidate = location.y - cardHeight / 2 - 14
        let y: CGFloat
        if yCandidate - cardHeight / 2 - margin < 0 {
            y = min(layout.bounds.maxY - cardHeight / 2 - margin, location.y + cardHeight / 2 + 14)
        } else {
            y = min(max(cardHeight / 2 + margin, yCandidate), layout.bounds.maxY - cardHeight / 2 - margin)
        }
        return CGPoint(x: x, y: y)
    }

    private func linearTicks() -> [ISIStateSpaceAxisTick] {
        let lower = max(0, axisRange.lowerBound)
        let upper = max(axisRange.upperBound, lower + 1)
        let step = Self.niceLinearStep(for: max((upper - lower) / 5, .leastNonzeroMagnitude))
        let start = floor(lower / step) * step
        let end = ceil(upper / step) * step
        var ticks: [ISIStateSpaceAxisTick] = []
        var value = start
        while value <= end + step * 1e-6 {
            if value >= axisRange.lowerBound - step * 1e-6, value <= axisRange.upperBound + step * 1e-6 {
                ticks.append(ISIStateSpaceAxisTick(value: value, label: Self.formatAxisValue(value, unit: displayUnit)))
            }
            value += step
            if ticks.count > 12 {
                break
            }
        }
        return ticks
    }

    private func logTicks() -> [ISIStateSpaceAxisTick] {
        let lower = axisRange.lowerBound
        let upper = axisRange.upperBound
        let mantissas: [Double] = [1, 2, 5]
        let startPower = Int(floor(lower)) - 1
        let endPower = Int(ceil(upper)) + 1
        var ticks: [ISIStateSpaceAxisTick] = []

        if let duplicateFloor, duplicateFloor >= lower - 1e-9, duplicateFloor <= upper + 1e-9 {
            ticks.append(ISIStateSpaceAxisTick(value: duplicateFloor, label: "0 \(displayUnit.rawValue)"))
        }

        for power in startPower...endPower {
            let magnitude = pow(10, Double(power))
            for mantissa in mantissas {
                let scaledValue = mantissa * magnitude
                let transformed = log10(scaledValue)
                guard transformed >= lower - 1e-9, transformed <= upper + 1e-9 else {
                    continue
                }
                ticks.append(
                    ISIStateSpaceAxisTick(
                        value: transformed,
                        label: Self.formatAxisValue(scaledValue, unit: displayUnit)
                    )
                )
            }
        }

        if ticks.count > 8 {
            let stride = Int(ceil(Double(ticks.count) / 8.0))
            ticks = ticks.enumerated().compactMap { index, tick in
                index % stride == 0 ? tick : nil
            }
        }
        return ticks
    }

    private func formatISI(_ seconds: Double) -> String {
        guard seconds.isFinite else {
            return "NA"
        }
        let scaled = seconds * displayUnit.scaleFromSeconds
        let suffix = displayUnit.rawValue
        if abs(scaled) < 1e-12 {
            return "0 \(suffix)"
        }
        if abs(scaled) >= 100 {
            return "\(String(format: "%.3f", scaled)) \(suffix)"
        }
        if abs(scaled) >= 1 {
            return "\(String(format: "%.4f", scaled)) \(suffix)"
        }
        return "\(String(format: "%.6f", scaled)) \(suffix)"
    }

    private enum SpikePosition {
        case first
        case middle
        case last
    }

    private func timestamp(_ point: SpikeISIStatePoint, position: SpikePosition, timeMode: RasterTimeMode) -> String {
        let value: Double
        switch (position, timeMode) {
        case (.first, .aligned):
            value = point.alignedFirstSpikeTimeSec
        case (.middle, .aligned):
            value = point.alignedMiddleSpikeTimeSec
        case (.last, .aligned):
            value = point.alignedLastSpikeTimeSec
        case (.first, .raw):
            value = point.firstSpikeTimeSec
        case (.middle, .raw):
            value = point.middleSpikeTimeSec
        case (.last, .raw):
            value = point.lastSpikeTimeSec
        }
        return TimeFormatting.seconds(value)
    }

    private static func axisRange(
        transformedValues: [Double],
        axisScale: ISIYAxisScale,
        duplicateFloorCandidate: Double?
    ) -> (range: ClosedRange<Double>, duplicateFloor: Double?) {
        let values = transformedValues.filter(\.isFinite).sorted()
        guard !values.isEmpty else {
            return axisScale == .linear ? (0...1, nil) : (-1...1, duplicateFloorCandidate)
        }
        switch axisScale {
        case .linear:
            let maxValue = max(values.filter { $0 >= 0 }.max() ?? 1, 1)
            let step = niceLinearStep(for: maxValue / 5)
            let upper = step * ceil(maxValue / step)
            return (0...max(upper, 1), nil)
        case .log:
            let lower = min(values.first ?? -1, duplicateFloorCandidate ?? values.first ?? -1)
            let upper = max(values.last ?? lower + 1, lower + 0.1)
            let span = max(upper - lower, 0.5)
            return ((lower - span * 0.06)...(upper + span * 0.08), duplicateFloorCandidate)
        }
    }

    private static func percentile(_ sortedValues: [Double], p: Double) -> Double {
        guard !sortedValues.isEmpty else {
            return 0
        }
        let clamped = min(max(p, 0), 1)
        let rawIndex = clamped * Double(sortedValues.count - 1)
        let lowerIndex = Int(floor(rawIndex))
        let upperIndex = Int(ceil(rawIndex))
        guard lowerIndex != upperIndex else {
            return sortedValues[lowerIndex]
        }
        let fraction = rawIndex - Double(lowerIndex)
        return sortedValues[lowerIndex] * (1 - fraction) + sortedValues[upperIndex] * fraction
    }

    private static func niceLinearStep(for target: Double) -> Double {
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
        return max(nice * magnitude, .leastNonzeroMagnitude)
    }

    private static func formatAxisValue(_ scaledValue: Double, unit: QualityDisplayUnit) -> String {
        let clean = abs(scaledValue) < 1e-9 ? 0 : scaledValue
        let rounded = clean.rounded()
        if abs(clean - rounded) < 1e-6 {
            return "\(Int(rounded)) \(unit.rawValue)"
        }
        if abs(clean * 10 - (clean * 10).rounded()) < 1e-6 {
            return "\(String(format: "%.1f", clean)) \(unit.rawValue)"
        }
        if abs(clean * 100 - (clean * 100).rounded()) < 1e-6 {
            return "\(String(format: "%.2f", clean)) \(unit.rawValue)"
        }
        return "\(String(format: "%.3f", clean)) \(unit.rawValue)"
    }

    private static func formatScore(_ value: Double) -> String {
        guard value.isFinite else {
            return "NA"
        }
        return String(format: "%.3f", value)
    }
}

private struct ISIStateSpacePlotPoint {
    let feature: SpikeISIStateFeature
    let location: CGPoint
}

private struct ISIStateSpaceAxisTick {
    let value: Double
    let label: String
}

private struct ISIStateSpaceHoverTarget {
    let trainID: String
    let trainName: String
    let rows: [(label: String, value: String)]
}

private struct ISIStateSpaceFeatureTableView: View {
    let traces: [SpikeISIStateFeatureTrace]
    let timeMode: RasterTimeMode
    let displayUnit: QualityDisplayUnit

    private let rowLimit = 2_000

    var body: some View {
        VStack(alignment: .leading, spacing: 8) {
            HStack(alignment: .firstTextBaseline, spacing: 10) {
                Text("State-space feature table")
                    .font(.headline)
                Text(summaryText)
                    .font(.caption)
                    .foregroundStyle(.secondary)
                Spacer()
            }
            .padding(.horizontal, 12)
            .padding(.top, 10)

            Table(rows) {
                TableColumn("Train") { row in
                    Text(row.feature.trainName)
                        .lineLimit(1)
                        .truncationMode(.middle)
                }
                .width(min: 180, ideal: 240)

                TableColumn("Time") { row in
                    Text(timeString(for: row.feature.point))
                        .monospacedDigit()
                }
                .width(86)

                TableColumn("ISI i") { row in
                    Text(formatISI(row.feature.leftISISec))
                        .monospacedDigit()
                }
                .width(84)

                TableColumn("ISI i+1") { row in
                    Text(formatISI(row.feature.rightISISec))
                        .monospacedDigit()
                }
                .width(88)

                TableColumn("log i") { row in
                    Text(formatNumber(row.feature.log10LeftISISec, digits: 4))
                        .monospacedDigit()
                }
                .width(70)

                TableColumn("log i+1") { row in
                    Text(formatNumber(row.feature.log10RightISISec, digits: 4))
                        .monospacedDigit()
                }
                .width(74)

                TableColumn("Delta") { row in
                    Text(formatNumber(row.feature.deltaLog10ISI, digits: 4))
                        .monospacedDigit()
                }
                .width(70)

                TableColumn("Local MAD") { row in
                    Text(formatNumber(row.feature.localMADLog10ISI, digits: 4))
                        .monospacedDigit()
                }
                .width(86)

                TableColumn("Scaled") { row in
                    Text("\(formatNumber(row.feature.scaledLeft, digits: 3)), \(formatNumber(row.feature.scaledRight, digits: 3))")
                        .monospacedDigit()
                }
                .width(112)

                TableColumn("QC") { row in
                    Text(row.feature.qcStatus.title)
                        .foregroundStyle(qcColor(row.feature.qcStatus))
                }
                .width(130)
            }
        }
        .background(Color(nsColor: .textBackgroundColor))
        .overlay(alignment: .top) {
            Rectangle()
                .fill(Color(nsColor: .separatorColor).opacity(0.8))
                .frame(height: 1)
        }
    }

    private var rows: [ISIStateSpaceFeatureTableRow] {
        Array(allFeatures.prefix(rowLimit)).map(ISIStateSpaceFeatureTableRow.init(feature:))
    }

    private var allFeatures: [SpikeISIStateFeature] {
        traces.flatMap(\.features)
    }

    private var summaryText: String {
        let total = allFeatures.count
        let shown = min(total, rowLimit)
        if total > rowLimit {
            return "Showing \(shown.formatted()) of \(total.formatted()) rows"
        }
        return "\(shown.formatted()) row(s)"
    }

    private func timeString(for point: SpikeISIStatePoint) -> String {
        switch timeMode {
        case .aligned:
            return TimeFormatting.seconds(point.alignedMiddleSpikeTimeSec)
        case .raw:
            return TimeFormatting.seconds(point.middleSpikeTimeSec)
        }
    }

    private func formatISI(_ seconds: Double) -> String {
        guard seconds.isFinite else {
            return "NA"
        }
        let scaled = seconds * displayUnit.scaleFromSeconds
        if abs(scaled) < 1e-12 {
            return "0 \(displayUnit.rawValue)"
        }
        if abs(scaled) >= 100 {
            return "\(String(format: "%.3f", scaled)) \(displayUnit.rawValue)"
        }
        if abs(scaled) >= 1 {
            return "\(String(format: "%.4f", scaled)) \(displayUnit.rawValue)"
        }
        return "\(String(format: "%.6f", scaled)) \(displayUnit.rawValue)"
    }

    private func formatNumber(_ value: Double, digits: Int) -> String {
        guard value.isFinite else {
            return "NA"
        }
        return String(format: "%.\(digits)f", value)
    }

    private func qcColor(_ qcStatus: SpikeISIStatePointQC) -> Color {
        switch qcStatus {
        case .ok:
            return .primary
        case .refractory:
            return .orange
        case .artifact, .duplicate:
            return ISIStateSpacePalette.artifactColor
        }
    }
}

private struct ISIStateSpaceFeatureTableRow: Identifiable {
    let feature: SpikeISIStateFeature

    var id: String {
        feature.id
    }
}

private struct ISIStateSpaceHoverCard: View {
    let target: ISIStateSpaceHoverTarget

    var body: some View {
        VStack(alignment: .leading, spacing: 7) {
            Text("ISI transition")
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
                        .frame(width: 62, alignment: .leading)
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
        .background(.regularMaterial.opacity(0.50), in: RoundedRectangle(cornerRadius: 8))
        .overlay {
            RoundedRectangle(cornerRadius: 8)
                .stroke(Color(nsColor: .separatorColor).opacity(0.32), lineWidth: 1)
        }
        .shadow(color: .black.opacity(0.05), radius: 22, y: 8)
    }
}

private enum ISIStateSpacePalette {
    static let artifactColor = Color(red: 255.0 / 255.0, green: 100.0 / 255.0, blue: 78.0 / 255.0)
    static let duplicateColor = Color(red: 255.0 / 255.0, green: 100.0 / 255.0, blue: 78.0 / 255.0)

    static func color(at index: Int) -> Color {
        let palette: [Color] = [.accentColor, .purple, .green, .cyan, .pink, .indigo, .teal, .brown]
        return palette[index % palette.count]
    }
}

private extension ISIStateSpaceScaling {
    var featureScaling: SpikeISIStateFeatureScaling {
        switch self {
        case .robust:
            return .robust
        case .zScore:
            return .zScore
        }
    }
}
