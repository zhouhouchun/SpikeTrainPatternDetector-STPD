import AppKit
import STPDCore
import SwiftUI

struct ISIStateSpaceView: View {
    @Bindable var document: RasterDocument
    @Environment(\.l10n) private var l10n

    @State private var singleTraceIndex = 0
    @State private var highlightedTrainIDs: Set<String> = []
    @State private var showsFeatureTable = true
    @State private var showsRCompatibleTable = false

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

    /// The raw spike trains backing the currently displayed feature traces — the input the
    /// R-compatible per-valid-ISI feature builder needs (it works on `SpikeTrain`, not feature traces).
    private var displayedTrains: [SpikeTrain] {
        guard let dataset = document.dataset else {
            return []
        }
        let ids = Set(displayedFeatureTraces.map(\.trainID))
        return dataset.trains.filter { ids.contains($0.id) }
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

    /// Raw-time pattern intervals for the PCA scatter Color-by (auto vs reviewed-final source). Built in
    /// raw time because the feature rows carry raw `timeMidSec`. Annotation overlay only; PCA unchanged.
    private var autoPatternIntervals: SpikePatternIntervals {
        SpikePatternIntervals(annotations: document.classicAnchorRawEventAnnotations, useAligned: false)
    }

    private var finalPatternIntervals: SpikePatternIntervals {
        SpikePatternIntervals(annotations: document.classicAnchorPublicEventAnnotations, useAligned: false)
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
                l10n.t("ISI 状态空间未加载"),
                systemImage: "square.stack.3d.up",
                description: Text(document.lastErrorMessage ?? l10n.t("请先加载 spike train 数据。"))
            )
            .frame(maxWidth: .infinity, maxHeight: .infinity)
        } else if selectedTraces.isEmpty {
            ContentUnavailableView(
                l10n.t("尚未选择状态空间序列"),
                systemImage: "checklist",
                description: Text(l10n.t("请使用顶部的状态空间序列选择器选择一条或多条 spike train。"))
            )
            .frame(maxWidth: .infinity, maxHeight: .infinity)
        } else {
            VSplitView {
                // Primary plot: the Method selector chooses what is shown here (single-plot layout). The
                // method panels REPLACE the feature-axes canvas rather than stacking below it. `.featureAxes`
                // is the original feature-axes scatter, so the default page looks/behaves exactly as before.
                switch document.isiStateSpaceMethod {
                case .featureAxes:
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
                case .pca:
                    ISIStatePCAScatterView(
                        trains: displayedTrains,
                        halfWindowK: document.isiStateSpaceHalfWindowK,
                        minValidISISec: document.isiStateSpaceMinValidISISec,
                        winsorize: document.isiStateSpaceWinsorizeExtremeLogISI,
                        scaling: document.isiStateSpaceScaling.pcaScaling,
                        qualitySettings: document.qualitySettings,
                        colorMode: document.isiStateSpaceColorMode,
                        displayUnit: document.isiStateSpaceDisplayUnit,
                        autoIntervals: autoPatternIntervals,
                        finalIntervals: finalPatternIntervals
                    )
                    .frame(minHeight: 320, idealHeight: 420)
                case .isomap:
                    ISIStateIsomapScatterView(
                        trains: displayedTrains,
                        halfWindowK: document.isiStateSpaceHalfWindowK,
                        minValidISISec: document.isiStateSpaceMinValidISISec,
                        winsorize: document.isiStateSpaceWinsorizeExtremeLogISI,
                        scaling: document.isiStateSpaceScaling.pcaScaling,
                        qualitySettings: document.qualitySettings,
                        colorMode: document.isiStateSpaceColorMode,
                        displayUnit: document.isiStateSpaceDisplayUnit,
                        autoIntervals: autoPatternIntervals,
                        finalIntervals: finalPatternIntervals,
                        neighbors: document.isiStateSpaceIsomapNeighbors,
                        componentPolicy: document.isiStateSpaceIsomapComponentMode == .error ? .error : .largest
                    )
                    .frame(minHeight: 320, idealHeight: 420)
                case .logISIPhasePortrait:
                    ISIStateLogISIPhasePortraitView(
                        trains: displayedTrains,
                        minValidISISec: document.isiStateSpaceMinValidISISec,
                        winsorize: document.isiStateSpaceWinsorizeExtremeLogISI,
                        lag: document.isiStateSpacePhasePortraitLag,
                        displayUnit: document.isiStateSpaceDisplayUnit,
                        colorMode: document.isiStateSpaceColorMode,
                        autoIntervals: autoPatternIntervals,
                        finalIntervals: finalPatternIntervals
                    )
                    .frame(minHeight: 320, idealHeight: 420)
                }

                // Feature tables stay below the primary plot, controlled by their existing toggles.
                if showsFeatureTable {
                    ISIStateSpaceFeatureTableView(
                        traces: displayedFeatureTraces,
                        timeMode: document.isiStateSpaceTimeMode,
                        displayUnit: document.isiStateSpaceDisplayUnit
                    )
                    .frame(minHeight: 170, idealHeight: 230)
                }

                if showsRCompatibleTable {
                    ISIRCompatibleFeatureTableView(
                        trains: displayedTrains,
                        halfWindowK: document.isiStateSpaceHalfWindowK,
                        minValidISISec: document.isiStateSpaceMinValidISISec,
                        winsorize: document.isiStateSpaceWinsorizeExtremeLogISI,
                        qualitySettings: document.qualitySettings,
                        displayUnit: document.isiStateSpaceDisplayUnit
                    )
                    .frame(minHeight: 170, idealHeight: 240)
                }
            }
        }
    }

    private var header: some View {
        VStack(alignment: .leading, spacing: 10) {
            HStack(alignment: .center, spacing: 14) {
                VStack(alignment: .leading, spacing: 4) {
                    HStack(alignment: .firstTextBaseline, spacing: 10) {
                        Text(l10n.t("ISI 状态空间"))
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
                Text(l10n.t(SpikeTrainSelectionScope.isiStateSpace.title))
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
            .help("Local ISI half-window (±k intervals) used for the local-context features and the R-compatible feature rows.")

            controlGroup("State-space min ISI") {
                DebouncedDoubleField(
                    "1",
                    value: minValidISIBinding,
                    width: 64,
                    maxFractionDigits: 4
                )
                Text(document.isiStateSpaceDisplayUnit.rawValue)
                    .foregroundStyle(.secondary)
            }
            .help("Minimum valid ISI (R min_isi_sec, default 1 ms / 0.001 s) for the R-compatible feature rows: shorter ISIs are excluded. Independent of the QC artifact threshold, which only flags rows.")

            controlGroup("Scaling") {
                GlassSegmentedControl(
                    options: ISIStateSpaceScaling.allCases.map { ($0, $0.title) },
                    selection: $document.isiStateSpaceScaling,
                    minSegmentWidth: 56
                )
                .frame(width: 158)
            }
            .help("How the per-axis ISI features are scaled (robust median/IQR or z-score) in the state-space plot and feature table.")

            controlGroup("Label source") {
                Picker("", selection: $document.isiStateSpaceLabelSource) {
                    ForEach(ISIStateSpaceLabelSource.allCases) { source in
                        Text(source.title).tag(source)
                    }
                }
                .labelsHidden()
                .frame(width: 150)
                .disabled(true)
            }
            .help("ISI pattern labels are not yet computed on macOS, so this Shiny-parity selector has no effect here. The state-space plot and feature tables are label-free; per-ISI labels are planned for a later phase.")

            Toggle("Winsorize logISI", isOn: $document.isiStateSpaceWinsorizeExtremeLogISI)
                .toggleStyle(.checkbox)
                .disabled(document.isiStateSpaceAxisScale != .log)

            Toggle("Break long ISI", isOn: $document.isiStateSpaceBreakLongISI)
                .toggleStyle(.checkbox)

            Toggle("Feature table", isOn: $showsFeatureTable)
                .toggleStyle(.checkbox)

            Toggle("R-feature rows", isOn: $showsRCompatibleTable)
                .toggleStyle(.checkbox)
                .help("Show the R-compatible one-row-per-valid-ISI state-space feature rows (stpd_make_isi_state_space_features parity): log ISI, lag context, local statistics, CV/LV/CV2, delta and prepost ratio.")

            controlGroup("Method") {
                GlassSegmentedControl(
                    options: ISIStateSpaceMethod.allCases.map { ($0, $0.title) },
                    selection: $document.isiStateSpaceMethod,
                    minSegmentWidth: 44
                )
                .fixedSize(horizontal: true, vertical: false)
            }
            .help("Choose the ISI state-space embedding: Axes (the always-on ISI_i vs ISI_{i+1} feature scatter, the default), PCA (stpd_run_isi_state_pca), Isomap (stpd_run_isi_state_isomap), or the logISI phase portrait (stpd_make_logisi_phase_portrait). All use the same R-compatible feature rows; none changes detection or exports.")

            if document.isiStateSpaceMethod != .featureAxes {
                controlGroup("Color by") {
                    Picker("", selection: $document.isiStateSpaceColorMode) {
                        ForEach(ISIStateSpaceColorMode.allCases) { mode in
                            Text(mode.title).tag(mode)
                        }
                    }
                    .labelsHidden()
                    .pickerStyle(.menu)
                    .frame(width: 96)
                }
                .help("Color the embedding by Time, Auto pattern, Final reviewed pattern, Train, or QC (annotation overlay only; the embedding math is unchanged). The logISI phase portrait has no per-point QC, so QC shows a note there.")
            }

            if document.isiStateSpaceMethod == .isomap {
                controlGroup("Neighbors") {
                    DebouncedIntField("k", value: $document.isiStateSpaceIsomapNeighbors, range: 2...50, width: 46)
                }
                .help("Isomap kNN neighbor count (R n_neighbors). Clamped to [2, n-1]; larger k connects the graph but can shortcut across states.")

                controlGroup("Disconnected") {
                    GlassSegmentedControl(
                        options: ISIStateSpaceIsomapComponentMode.allCases.map { ($0, $0.title) },
                        selection: $document.isiStateSpaceIsomapComponentMode,
                        minSegmentWidth: 52
                    )
                    .fixedSize(horizontal: true, vertical: false)
                }
                .help("How a disconnected kNN graph is handled: Largest embeds the biggest connected component; Error shows a note asking to increase neighbors.")
            }

            if document.isiStateSpaceMethod == .logISIPhasePortrait {
                controlGroup("Lag") {
                    DebouncedIntField("1", value: $document.isiStateSpacePhasePortraitLag, range: 1...10, width: 46)
                }
                .help("logISI phase-portrait transition lag (R lag): plot logISI_i versus logISI_{i+lag}.")
            }

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
                document.isiStateSpaceMinValidISISec = SpikeISIStateSpaceFeatureBuilder.defaultMinValidISISec
                document.isiStateSpaceScaling = .robust
                document.isiStateSpaceLabelSource = .auditFinal
                document.isiStateSpaceWinsorizeExtremeLogISI = true
                document.isiStateSpaceBreakLongISI = true
                document.isiStateSpaceBreakThresholdMs = 150
                document.isiStateSpaceMethod = .featureAxes
                document.isiStateSpaceIsomapNeighbors = 15
                document.isiStateSpaceIsomapComponentMode = .largest
                document.isiStateSpacePhasePortraitLag = 1
                singleTraceIndex = 0
                highlightedTrainIDs = []
            } label: {
                Label(l10n.t("重置"), systemImage: "arrow.counterclockwise")
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

    /// State-space min ISI is stored in seconds (R-compatible); the field shows/edits it in the
    /// selected display unit (ms by default).
    private var minValidISIBinding: Binding<Double> {
        Binding(
            get: {
                max(0, document.isiStateSpaceMinValidISISec) * document.isiStateSpaceDisplayUnit.scaleFromSeconds
            },
            set: { newValue in
                // Guardrail: clamp the stored floor to a tiny positive minimum so it is never exactly 0.
                let seconds = newValue / document.isiStateSpaceDisplayUnit.scaleFromSeconds
                document.isiStateSpaceMinValidISISec = SpikeISIStateSpaceFeatureBuilder.clampedMinValidISISec(seconds)
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
                ContentUnavailableView("无状态空间点", systemImage: "square.stack.3d.up")
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
            title: "ISI transition",
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
    let title: String
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
                Text("状态空间特征表")
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

/// Phase 2A: the R-compatible one-row-per-valid-ISI feature table. Mirrors the column set produced
/// by `stpd_make_isi_state_space_features` (`R/55_state_space_pca.R`) via the pure STPDCore
/// `SpikeISIStateSpaceFeatureBuilder`. Times are absolute (raw) interval midpoints, matching the R
/// builder, which uses the train's timestamps directly. This is an additive, read-only surface and
/// does not touch the existing adjacent-pair feature table or the phase-portrait plot.
private struct ISIRCompatibleFeatureTableView: View {
    let trains: [SpikeTrain]
    let halfWindowK: Int
    let minValidISISec: Double
    let winsorize: Bool
    let qualitySettings: SpikeQualitySettings
    let displayUnit: QualityDisplayUnit

    private let rowLimit = 2_000

    var body: some View {
        VStack(alignment: .leading, spacing: 8) {
            HStack(alignment: .firstTextBaseline, spacing: 10) {
                Text("与 R 兼容的 ISI 特征行")
                    .font(.headline)
                Text(summaryText)
                    .font(.caption)
                    .foregroundStyle(.secondary)
                Spacer()
            }
            .padding(.horizontal, 12)
            .padding(.top, 10)

            Table(rows) {
                Group {
                TableColumn("Train") { (row: ISIRCompatibleFeatureRow) in
                    Text(row.row.trainName)
                        .lineLimit(1)
                        .truncationMode(.middle)
                }
                .width(min: 150, ideal: 200)

                TableColumn("idx") { row in
                    Text("\(row.row.idx)")
                        .monospacedDigit()
                }
                .width(48)

                TableColumn("Time") { row in
                    Text(TimeFormatting.seconds(row.row.timeMidSec))
                        .monospacedDigit()
                }
                .width(86)

                TableColumn("ISI") { row in
                    Text(formatISI(row.row.isiSec))
                        .monospacedDigit()
                }
                .width(96)

                TableColumn("log ISI") { row in
                    Text(formatOptional(row.row.logISI, digits: 4))
                        .monospacedDigit()
                }
                .width(72)

                TableColumn("log feat") { row in
                    Text(formatOptional(row.row.logISIFeature, digits: 4))
                        .monospacedDigit()
                }
                .width(74)

                TableColumn("lag −1") { row in
                    Text(formatOptional(row.row.lag(-1), digits: 4))
                        .monospacedDigit()
                }
                .width(70)

                TableColumn("lag +1") { row in
                    Text(formatOptional(row.row.lag(1), digits: 4))
                        .monospacedDigit()
                }
                .width(70)

                TableColumn("loc med") { row in
                    Text(formatOptional(row.row.localMedianLogISI, digits: 4))
                        .monospacedDigit()
                }
                .width(74)
                }

                Group {
                TableColumn("loc CV") { (row: ISIRCompatibleFeatureRow) in
                    Text(formatOptional(row.row.localCV, digits: 3))
                        .monospacedDigit()
                }
                .width(64)

                TableColumn("loc LV") { row in
                    Text(formatOptional(row.row.localLV, digits: 3))
                        .monospacedDigit()
                }
                .width(64)

                TableColumn("loc CV2") { row in
                    Text(formatOptional(row.row.localCV2, digits: 3))
                        .monospacedDigit()
                }
                .width(68)

                TableColumn("rate Hz") { row in
                    Text(formatOptional(row.row.localRateHz, digits: 2))
                        .monospacedDigit()
                }
                .width(70)

                TableColumn("Δ log") { row in
                    Text(formatOptional(row.row.deltaLogISI, digits: 4))
                        .monospacedDigit()
                }
                .width(70)

                TableColumn("next Δ") { row in
                    Text(formatOptional(row.row.nextDeltaLogISI, digits: 4))
                        .monospacedDigit()
                }
                .width(70)

                TableColumn("prepost") { row in
                    Text(formatOptional(row.row.prepostRatio, digits: 3))
                        .monospacedDigit()
                }
                .width(70)

                TableColumn("QC") { row in
                    Text(row.row.qcStatus.title)
                        .foregroundStyle(qcColor(row.row.qcStatus))
                }
                .width(150)
                }
            }
        }
        .background(Color(nsColor: .textBackgroundColor))
        .overlay(alignment: .top) {
            Rectangle()
                .fill(Color(nsColor: .separatorColor).opacity(0.8))
                .frame(height: 1)
        }
    }

    private var allRows: [SpikeISIStateSpaceRow] {
        // Validity gate = the user-exposed state-space min ISI (R `min_isi_sec`, default 0.001 s). This
        // is independent of the QC artifact threshold: the artifact threshold still drives the per-row
        // `qcStatus` column (via `qualitySettings`), but it does not change which ISIs are emitted, so
        // live inclusion/exclusion matches R/Shiny. Detector and quality-analysis behavior are unchanged.
        return trains.flatMap { train in
            SpikeISIStateSpaceFeatureBuilder.makeRows(
                train: train,
                k: halfWindowK,
                minValidISISec: minValidISISec,
                winsorize: winsorize,
                qualitySettings: qualitySettings
            )
        }
    }

    private var rows: [ISIRCompatibleFeatureRow] {
        Array(allRows.prefix(rowLimit)).map(ISIRCompatibleFeatureRow.init(row:))
    }

    private var summaryText: String {
        let total = allRows.count
        let shown = min(total, rowLimit)
        if total > rowLimit {
            return "Showing \(shown.formatted()) of \(total.formatted()) rows · one row per valid ISI"
        }
        return "\(shown.formatted()) row(s) · one row per valid ISI"
    }

    private func formatISI(_ seconds: Double) -> String {
        guard seconds.isFinite else {
            return "NA"
        }
        let scaled = seconds * displayUnit.scaleFromSeconds
        if abs(scaled) >= 100 {
            return "\(String(format: "%.3f", scaled)) \(displayUnit.rawValue)"
        }
        if abs(scaled) >= 1 {
            return "\(String(format: "%.4f", scaled)) \(displayUnit.rawValue)"
        }
        return "\(String(format: "%.6f", scaled)) \(displayUnit.rawValue)"
    }

    private func formatOptional(_ value: Double?, digits: Int) -> String {
        guard let value, value.isFinite else {
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

private struct ISIRCompatibleFeatureRow: Identifiable {
    let row: SpikeISIStateSpaceRow

    var id: String {
        "\(row.trainID)-\(row.idx)"
    }
}

private struct ISIStateSpaceHoverCard: View {
    let target: ISIStateSpaceHoverTarget

    var body: some View {
        VStack(alignment: .leading, spacing: 7) {
            Text(target.title)
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

/// One PCA scatter point: PC1/PC2 plus the originating row's QC status + train (row context preserved by
/// zipping `ISIStatePCAResult.scores` with the `SpikeISIStateSpaceRow`s they were computed from).
private struct ISIStatePCAScatterPoint {
    let pc1: Double
    let pc2: Double
    let qcStatus: SpikeISIStatePointQC
    let trainName: String
    let trainID: String
    let timeMidSec: Double
    /// Pattern state of the detector annotation covering this ISI's time (auto / reviewed-final source).
    let autoState: SpikePatternState
    let finalState: SpikePatternState
    /// The full source feature row, retained so the hover inspector can show ISI index / value / logISI / QC.
    let row: SpikeISIStateSpaceRow
}

/// View model for the PCA panel: the scatter points (row context preserved) plus the variance and
/// loadings summaries (Step 2).
private struct ISIStatePCAPanelData {
    let points: [ISIStatePCAScatterPoint]
    let variance: [ISIStatePCAVariance]
    let loadings: [ISIStatePCALoading]
}

/// P2B: runs the pure R-compatible PCA on the current feature rows and plots PC1 vs PC2. No 3D, no
/// variance/loadings tables yet (Step 2), no Isomap/labels/state-dynamics.
private struct ISIStatePCAScatterView: View {
    let trains: [SpikeTrain]
    let halfWindowK: Int
    let minValidISISec: Double
    let winsorize: Bool
    let scaling: ISIStatePCAScaling
    let qualitySettings: SpikeQualitySettings
    let colorMode: ISIStateSpaceColorMode
    let displayUnit: QualityDisplayUnit
    /// Raw-time pattern intervals (auto + reviewed-final). Built once in the parent; rows carry raw
    /// `timeMidSec`, so these are queried in raw time regardless of the display timestamp mode.
    let autoIntervals: SpikePatternIntervals
    let finalIntervals: SpikePatternIntervals
    @Environment(\.l10n) private var l10n

    private var rows: [SpikeISIStateSpaceRow] {
        trains.flatMap { train in
            SpikeISIStateSpaceFeatureBuilder.makeRows(
                train: train,
                k: halfWindowK,
                minValidISISec: minValidISISec,
                winsorize: winsorize,
                qualitySettings: qualitySettings
            )
        }
    }

    private var outcome: Result<ISIStatePCAPanelData, ISIStatePCAError> {
        let rows = self.rows
        do {
            let result = try ISIStateSpacePCA.run(rows: rows, scaling: scaling)
            // scores are aligned 1:1 with the input rows, so zip preserves each point's row context.
            let points = zip(result.scores, rows).compactMap { score, row -> ISIStatePCAScatterPoint? in
                guard let pc1 = score.pc1, let pc2 = score.pc2, pc1.isFinite, pc2.isFinite else { return nil }
                return ISIStatePCAScatterPoint(
                    pc1: pc1, pc2: pc2, qcStatus: row.qcStatus, trainName: row.trainName,
                    trainID: row.trainID, timeMidSec: row.timeMidSec,
                    autoState: autoIntervals.state(forTrainID: row.trainID, atSec: row.timeMidSec),
                    finalState: finalIntervals.state(forTrainID: row.trainID, atSec: row.timeMidSec),
                    row: row
                )
            }
            return .success(ISIStatePCAPanelData(
                points: points, variance: result.variance, loadings: result.loadings
            ))
        } catch let error as ISIStatePCAError {
            return .failure(error)
        } catch {
            return .failure(.tooFewRows)
        }
    }

    var body: some View {
        // Compute the PCA once per render and feed the subtitle, variance line, scatter, and loadings.
        let outcome = self.outcome
        return VStack(alignment: .leading, spacing: 8) {
            HStack(alignment: .firstTextBaseline, spacing: 10) {
                Text(l10n.t("主成分（PC1 · PC2）"))
                    .font(.headline)
                Text(subtitle(outcome))
                    .font(.caption)
                    .foregroundStyle(.secondary)
                Spacer()
            }
            .padding(.horizontal, 12)
            .padding(.top, 10)

            if let poolingNote {
                Text(poolingNote)
                    .font(.caption2)
                    .foregroundStyle(.secondary)
                    .padding(.horizontal, 12)
            }

            if case let .success(data) = outcome, !data.variance.isEmpty {
                Text(varianceSummary(data.variance))
                    .font(.caption2)
                    .foregroundStyle(.secondary)
                    .monospacedDigit()
                    .padding(.horizontal, 12)
            }

            content(outcome)
        }
        .background(Color(nsColor: .textBackgroundColor))
        .overlay(alignment: .top) {
            Rectangle()
                .fill(Color(nsColor: .separatorColor).opacity(0.8))
                .frame(height: 1)
        }
    }

    @ViewBuilder
    private func content(_ outcome: Result<ISIStatePCAPanelData, ISIStatePCAError>) -> some View {
        switch outcome {
        case let .success(data) where data.points.count >= 2:
            VStack(alignment: .leading, spacing: 6) {
                HStack(alignment: .top, spacing: 12) {
                    ISIStatePCAScatterCanvas(points: data.points, colorMode: colorMode, displayUnit: displayUnit)
                    ISIStatePCALoadingsSummary(loadings: data.loadings)
                        .frame(width: 196)
                }
                if colorMode.isPattern {
                    patternLegend
                } else if colorMode == .time {
                    ISIStateTimeLegend()
                }
            }
            .padding(.horizontal, 12)
            .padding(.bottom, 12)
        case .success:
            unavailable("Not enough finite PCA points to plot.")
        case let .failure(error):
            unavailable(message(for: error))
        }
    }

    /// Compact pattern-color legend (shown in Auto / Final color modes).
    private var patternLegend: some View {
        HStack(spacing: 10) {
            Text(colorMode == .autoPattern ? l10n.t("自动") : l10n.t("最终"))
                .font(.caption2.weight(.semibold))
                .foregroundStyle(.secondary)
            ForEach(SpikePatternColor.legendStates, id: \.self) { state in
                HStack(spacing: 3) {
                    Circle().fill(SpikePatternColor.color(state)).frame(width: 8, height: 8)
                    Text(l10n.t(state.localizationSourceZH)).font(.caption2).foregroundStyle(.secondary)
                }
            }
        }
    }

    /// Subtle scope clarification (no PCA math change): the panel pools the per-valid-ISI feature
    /// rows across whatever trains are currently displayed, so make the single-train vs pooled
    /// multi-train (overlay) distinction explicit. Driven by the actual count of displayed trains,
    /// which already reflects the layout mode (single-train layout → 1 train; overlay → all selected).
    private var poolingNote: String? {
        switch trains.count {
        case 0:
            return nil
        case 1:
            return "Single-train PCA on the displayed train's ISI rows."
        default:
            return "PCA is computed on pooled ISI rows from the \(trains.count) currently displayed trains."
        }
    }

    private func subtitle(_ outcome: Result<ISIStatePCAPanelData, ISIStatePCAError>) -> String {
        switch outcome {
        case let .success(data):
            return "\(data.points.count) points · \(scaling == .robust ? "robust" : "z-score") scaling"
        case .failure:
            return scaling == .robust ? "robust scaling" : "z-score scaling"
        }
    }

    /// Compact variance line: top components' explained-variance % plus the cumulative through PC3.
    private func varianceSummary(_ variance: [ISIStatePCAVariance]) -> String {
        let top = variance.prefix(3)
        let parts = top.map { entry -> String in
            "\(entry.component) \(percent(entry.variance))"
        }
        let cumulative = top.last?.cumulative
        return parts.joined(separator: " · ") + " · cumulative " + percent(cumulative)
    }

    private func percent(_ ratio: Double?) -> String {
        guard let ratio, ratio.isFinite else { return "—" }
        return (ratio * 100).formatted(.number.precision(.fractionLength(1))) + "%"
    }

    private func message(for error: ISIStatePCAError) -> String {
        switch error {
        case .tooFewRows:
            return "Need at least two ISI feature rows for PCA."
        case .tooFewVaryingFeatures:
            return "Need at least two varying numeric ISI features for PCA."
        }
    }

    private func unavailable(_ text: String) -> some View {
        Text(text)
            .font(.callout)
            .foregroundStyle(.secondary)
            .frame(maxWidth: .infinity, maxHeight: .infinity)
            .padding(20)
    }
}

private struct ISIStatePCAScatterCanvas: View {
    let points: [ISIStatePCAScatterPoint]
    let colorMode: ISIStateSpaceColorMode
    let displayUnit: QualityDisplayUnit
    @Environment(\.l10n) private var l10n
    @State private var hoverLocation: CGPoint?

    /// Stable per-train color map (dataset-independent: by sorted trainID) for the Train color mode.
    private var trainColors: [String: Color] {
        let ids = Array(Set(points.map(\.trainID))).sorted()
        return Dictionary(uniqueKeysWithValues: ids.enumerated().map { ($1, ISIStateSpacePalette.color(at: $0)) })
    }

    var body: some View {
        GeometryReader { proxy in
            // Project once from the GeometryReader size and share it with the Canvas draw, so the hover
            // hit-test maps points to the exact pixels they are drawn at.
            let projection = ISIStateScatterProjection(xs: points.map(\.pc1), ys: points.map(\.pc2), size: proxy.size)
            ZStack(alignment: .topLeading) {
                Canvas { context, _ in
                    guard let projection else { return }
                    draw(projection, into: &context)
                }
                .frame(maxWidth: .infinity, maxHeight: .infinity)

                if let projection, let location = hoverLocation,
                   let index = nearestIndex(at: location, projection: projection) {
                    ISIStateSpaceHoverCard(target: hoverTarget(for: points[index]))
                        .frame(width: 240, alignment: .leading)
                        .position(isiStateSpaceHoverCardPosition(for: location, in: proxy.size, cardWidth: 240, cardHeight: 210))
                        .allowsHitTesting(false)
                }
            }
            .contentShape(Rectangle())
            .onContinuousHover { phase in
                switch phase {
                case .active(let location): hoverLocation = location
                case .ended: hoverLocation = nil
                }
            }
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity)
    }

    private func draw(_ projection: ISIStateScatterProjection, into context: inout GraphicsContext) {
        let timeMin = points.map(\.timeMidSec).min() ?? 0
        let timeMax = points.map(\.timeMidSec).max() ?? 0
        let trainColors = self.trainColors
        let plot = projection.plot

        context.stroke(Path(plot), with: .color(Color(nsColor: .separatorColor).opacity(0.6)), lineWidth: 1)

        let dashed = StrokeStyle(lineWidth: 0.75, dash: [3, 3])
        if projection.xLo <= 0, 0 <= projection.xHi {
            let zx = projection.screen(x: 0, y: projection.yLo).x
            var axis = Path()
            axis.move(to: CGPoint(x: zx, y: plot.minY))
            axis.addLine(to: CGPoint(x: zx, y: plot.maxY))
            context.stroke(axis, with: .color(.secondary.opacity(0.3)), style: dashed)
        }
        if projection.yLo <= 0, 0 <= projection.yHi {
            let zy = projection.screen(x: projection.xLo, y: 0).y
            var axis = Path()
            axis.move(to: CGPoint(x: plot.minX, y: zy))
            axis.addLine(to: CGPoint(x: plot.maxX, y: zy))
            context.stroke(axis, with: .color(.secondary.opacity(0.3)), style: dashed)
        }

        for point in points {
            let p = projection.screen(x: point.pc1, y: point.pc2)
            let rect = CGRect(x: p.x - 2.4, y: p.y - 2.4, width: 4.8, height: 4.8)
            let pointColor = color(for: point, timeMin: timeMin, timeMax: timeMax, trainColors: trainColors)
            context.fill(Path(ellipseIn: rect), with: .color(pointColor.opacity(0.85)))
        }

        context.draw(
            Text("PC1").font(.caption2).foregroundStyle(.secondary),
            at: CGPoint(x: plot.maxX - 14, y: plot.maxY + 10)
        )
        context.draw(
            Text("PC2").font(.caption2).foregroundStyle(.secondary),
            at: CGPoint(x: plot.minX + 12, y: plot.minY + 8)
        )
    }

    private func nearestIndex(at location: CGPoint, projection: ISIStateScatterProjection) -> Int? {
        let positions = points.map { point -> ISIStateSpaceScreenPoint in
            let screen = projection.screen(x: point.pc1, y: point.pc2)
            return ISIStateSpaceScreenPoint(x: Double(screen.x), y: Double(screen.y))
        }
        return ISIStateSpaceHitTesting.nearestIndex(
            positions: positions,
            to: ISIStateSpaceScreenPoint(x: Double(location.x), y: Double(location.y)),
            maxDistance: 12
        )
    }

    private func hoverTarget(for point: ISIStatePCAScatterPoint) -> ISIStateSpaceHoverTarget {
        let row = point.row
        var rows: [(label: String, value: String)] = [
            ("Train ID", row.trainID),
            ("Time", TimeFormatting.seconds(row.timeMidSec)),
            ("ISI #", "\(row.rowNumber)"),
            ("ISI", isiStateSpaceFormatISI(row.isiSec, unit: displayUnit)),
        ]
        if let logISI = row.logISI, logISI.isFinite {
            rows.append(("logISI", String(format: "%.3f", logISI)))
        }
        rows.append(("Auto", l10n.t(point.autoState.localizationSourceZH)))
        rows.append(("Final", l10n.t(point.finalState.localizationSourceZH)))
        rows.append(("QC", point.qcStatus.title))
        rows.append(("Color", colorMode.title))
        return ISIStateSpaceHoverTarget(title: "PCA point", trainID: row.trainID, trainName: row.trainName, rows: rows)
    }

    private func color(
        for point: ISIStatePCAScatterPoint, timeMin: Double, timeMax: Double, trainColors: [String: Color]
    ) -> Color {
        switch colorMode {
        case .time:
            let fraction = timeMax > timeMin ? (point.timeMidSec - timeMin) / (timeMax - timeMin) : 0
            return ISIStateSpacePalette.timeColor(fraction: fraction)
        case .autoPattern:
            return SpikePatternColor.color(point.autoState)
        case .finalPattern:
            return SpikePatternColor.color(point.finalState)
        case .train:
            return trainColors[point.trainID] ?? .accentColor
        case .qc:
            return qcColor(point.qcStatus)
        }
    }

    private func qcColor(_ qcStatus: SpikeISIStatePointQC) -> Color {
        switch qcStatus {
        case .ok:
            return .accentColor
        case .refractory:
            return .orange
        case .artifact, .duplicate:
            return ISIStateSpacePalette.artifactColor
        }
    }
}

/// Compact loadings summary (Step 2): the top features by combined |PC1| + |PC2| magnitude, mirroring R's
/// loadings table ordering (top by absolute PC weight). Kept small and read-only beside the scatter.
private struct ISIStatePCALoadingsSummary: View {
    let loadings: [ISIStatePCALoading]

    private var topLoadings: [ISIStatePCALoading] {
        loadings
            .sorted { combinedMagnitude($0) > combinedMagnitude($1) }
            .prefix(7)
            .map { $0 }
    }

    private func combinedMagnitude(_ loading: ISIStatePCALoading) -> Double {
        abs(loading.pc1 ?? 0) + abs(loading.pc2 ?? 0)
    }

    var body: some View {
        VStack(alignment: .leading, spacing: 4) {
            Text("主要载荷（|PC1|+|PC2|）")
                .font(.caption.weight(.semibold))
                .foregroundStyle(.secondary)
            Grid(alignment: .leading, horizontalSpacing: 8, verticalSpacing: 2) {
                GridRow {
                    Text("特征").foregroundStyle(.secondary)
                    Text("PC1").foregroundStyle(.secondary).gridColumnAlignment(.trailing)
                    Text("PC2").foregroundStyle(.secondary).gridColumnAlignment(.trailing)
                }
                .font(.caption2)
                ForEach(topLoadings, id: \.feature) { loading in
                    GridRow {
                        Text(loading.feature)
                            .lineLimit(1)
                            .truncationMode(.middle)
                        Text(format(loading.pc1)).monospacedDigit()
                        Text(format(loading.pc2)).monospacedDigit()
                    }
                    .font(.caption2)
                }
            }
            Spacer(minLength: 0)
        }
    }

    private func format(_ value: Double?) -> String {
        guard let value, value.isFinite else { return "—" }
        return value.formatted(.number.precision(.fractionLength(2)))
    }
}

// MARK: - P2: shared embedding scatter (Isomap + logISI phase portrait)

/// One point for the generic embedding scatter. `qcStatus` is `nil` when the source method has no per-point
/// QC (the logISI phase portrait) — the canvas then colors QC mode neutrally and the panel shows a note.
/// What an embedding scatter point originated from, retained so the hover inspector can show the right
/// biological context: Isomap points keep their ISI feature row; logISI points keep their phase-portrait
/// transition row (which also carries the current/next ISI). Display only — never an embedding input.
private enum ISIStateScatterSource {
    case isi(SpikeISIStateSpaceRow)
    case phasePortrait(ISIStateSpacePhasePortraitRow)
}

private struct ISIStateScatterDatum {
    let x: Double
    let y: Double
    let trainID: String
    let timeMidSec: Double
    let autoState: SpikePatternState
    let finalState: SpikePatternState
    let qcStatus: SpikeISIStatePointQC?
    let source: ISIStateScatterSource
}

/// Generic 2-D embedding scatter shared by the Isomap and logISI phase-portrait panels. Mirrors the PCA
/// canvas's color-by semantics (time / auto / final / train / qc); `qc` falls back to a neutral color when a
/// point has no QC status (phase portrait). Pure drawing — never an embedding input.
private struct ISIStateEmbeddingScatterCanvas: View {
    let points: [ISIStateScatterDatum]
    let colorMode: ISIStateSpaceColorMode
    let xLabel: String
    let yLabel: String
    /// Hover-card title ("Isomap point" / "logISI transition").
    let title: String
    let displayUnit: QualityDisplayUnit
    @Environment(\.l10n) private var l10n
    @State private var hoverLocation: CGPoint?

    private var trainColors: [String: Color] {
        let ids = Array(Set(points.map(\.trainID))).sorted()
        return Dictionary(uniqueKeysWithValues: ids.enumerated().map { ($1, ISIStateSpacePalette.color(at: $0)) })
    }

    var body: some View {
        GeometryReader { proxy in
            // Project once from the GeometryReader size and share it with the Canvas draw, so the hover
            // hit-test maps points to the exact pixels they are drawn at.
            let projection = ISIStateScatterProjection(xs: points.map(\.x), ys: points.map(\.y), size: proxy.size)
            ZStack(alignment: .topLeading) {
                Canvas { context, _ in
                    guard let projection else { return }
                    draw(projection, into: &context)
                }
                .frame(maxWidth: .infinity, maxHeight: .infinity)

                if let projection, let location = hoverLocation,
                   let index = nearestIndex(at: location, projection: projection) {
                    ISIStateSpaceHoverCard(target: hoverTarget(for: points[index]))
                        .frame(width: 240, alignment: .leading)
                        .position(isiStateSpaceHoverCardPosition(for: location, in: proxy.size, cardWidth: 240, cardHeight: 210))
                        .allowsHitTesting(false)
                }
            }
            .contentShape(Rectangle())
            .onContinuousHover { phase in
                switch phase {
                case .active(let location): hoverLocation = location
                case .ended: hoverLocation = nil
                }
            }
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity)
    }

    private func draw(_ projection: ISIStateScatterProjection, into context: inout GraphicsContext) {
        let timeMin = points.map(\.timeMidSec).min() ?? 0
        let timeMax = points.map(\.timeMidSec).max() ?? 0
        let trainColors = self.trainColors
        let plot = projection.plot

        context.stroke(Path(plot), with: .color(Color(nsColor: .separatorColor).opacity(0.6)), lineWidth: 1)
        let dashed = StrokeStyle(lineWidth: 0.75, dash: [3, 3])
        if projection.xLo <= 0, 0 <= projection.xHi {
            let zx = projection.screen(x: 0, y: projection.yLo).x
            var axis = Path(); axis.move(to: CGPoint(x: zx, y: plot.minY)); axis.addLine(to: CGPoint(x: zx, y: plot.maxY))
            context.stroke(axis, with: .color(.secondary.opacity(0.3)), style: dashed)
        }
        if projection.yLo <= 0, 0 <= projection.yHi {
            let zy = projection.screen(x: projection.xLo, y: 0).y
            var axis = Path(); axis.move(to: CGPoint(x: plot.minX, y: zy)); axis.addLine(to: CGPoint(x: plot.maxX, y: zy))
            context.stroke(axis, with: .color(.secondary.opacity(0.3)), style: dashed)
        }

        for point in points {
            let p = projection.screen(x: point.x, y: point.y)
            let rect = CGRect(x: p.x - 2.4, y: p.y - 2.4, width: 4.8, height: 4.8)
            let color = color(for: point, timeMin: timeMin, timeMax: timeMax, trainColors: trainColors)
            context.fill(Path(ellipseIn: rect), with: .color(color.opacity(0.85)))
        }

        context.draw(Text(xLabel).font(.caption2).foregroundStyle(.secondary), at: CGPoint(x: plot.maxX - 24, y: plot.maxY + 10))
        context.draw(Text(yLabel).font(.caption2).foregroundStyle(.secondary), at: CGPoint(x: plot.minX + 22, y: plot.minY + 8))
    }

    private func nearestIndex(at location: CGPoint, projection: ISIStateScatterProjection) -> Int? {
        let positions = points.map { point -> ISIStateSpaceScreenPoint in
            let screen = projection.screen(x: point.x, y: point.y)
            return ISIStateSpaceScreenPoint(x: Double(screen.x), y: Double(screen.y))
        }
        return ISIStateSpaceHitTesting.nearestIndex(
            positions: positions,
            to: ISIStateSpaceScreenPoint(x: Double(location.x), y: Double(location.y)),
            maxDistance: 12
        )
    }

    /// Build the hover inspector from the point's retained source row. Isomap points (`.isi`) carry per-point
    /// QC and a single ISI; logISI points (`.phasePortrait`) carry the current and next ISI and have no QC.
    private func hoverTarget(for datum: ISIStateScatterDatum) -> ISIStateSpaceHoverTarget {
        var rows: [(label: String, value: String)] = []
        let trainName: String
        let trainID: String
        switch datum.source {
        case let .isi(row):
            trainName = row.trainName
            trainID = row.trainID
            rows.append(("Train ID", row.trainID))
            rows.append(("Time", TimeFormatting.seconds(row.timeMidSec)))
            rows.append(("ISI #", "\(row.rowNumber)"))
            rows.append(("ISI", isiStateSpaceFormatISI(row.isiSec, unit: displayUnit)))
            if let logISI = row.logISI, logISI.isFinite {
                rows.append(("logISI", String(format: "%.3f", logISI)))
            }
        case let .phasePortrait(row):
            trainName = row.train
            trainID = datum.trainID
            rows.append(("Train ID", datum.trainID))
            rows.append(("Time", TimeFormatting.seconds(row.timeMidSec)))
            rows.append(("ISI #", "\(row.rowNumber)"))
            rows.append(("ISI", isiStateSpaceFormatISI(row.isiSec, unit: displayUnit)))
            rows.append(("Next ISI", isiStateSpaceFormatISI(row.nextISISec, unit: displayUnit)))
            rows.append(("logISIᵢ", String(format: "%.3f", row.logISIi)))
            rows.append(("logISIᵢ₊ₗ", String(format: "%.3f", row.logISINext)))
        }
        rows.append(("Auto", l10n.t(datum.autoState.localizationSourceZH)))
        rows.append(("Final", l10n.t(datum.finalState.localizationSourceZH)))
        if let qcStatus = datum.qcStatus {
            rows.append(("QC", qcStatus.title))
        }
        rows.append(("Color", colorMode.title))
        return ISIStateSpaceHoverTarget(title: title, trainID: trainID, trainName: trainName, rows: rows)
    }

    private func color(
        for point: ISIStateScatterDatum, timeMin: Double, timeMax: Double, trainColors: [String: Color]
    ) -> Color {
        switch colorMode {
        case .time:
            let fraction = timeMax > timeMin ? (point.timeMidSec - timeMin) / (timeMax - timeMin) : 0
            return ISIStateSpacePalette.timeColor(fraction: fraction)
        case .autoPattern:
            return SpikePatternColor.color(point.autoState)
        case .finalPattern:
            return SpikePatternColor.color(point.finalState)
        case .train:
            return trainColors[point.trainID] ?? .accentColor
        case .qc:
            guard let qcStatus = point.qcStatus else { return .secondary }   // no per-point QC -> neutral
            switch qcStatus {
            case .ok: return .accentColor
            case .refractory: return .orange
            case .artifact, .duplicate: return ISIStateSpacePalette.artifactColor
            }
        }
    }
}

/// Compact pattern legend reused by the embedding panels (Auto / Final color modes).
private struct ISIStatePatternLegend: View {
    let colorMode: ISIStateSpaceColorMode
    @Environment(\.l10n) private var l10n
    var body: some View {
        HStack(spacing: 10) {
            Text(colorMode == .autoPattern ? l10n.t("自动") : l10n.t("最终"))
                .font(.caption2.weight(.semibold)).foregroundStyle(.secondary)
            ForEach(SpikePatternColor.legendStates, id: \.self) { state in
                HStack(spacing: 3) {
                    Circle().fill(SpikePatternColor.color(state)).frame(width: 8, height: 8)
                    Text(l10n.t(state.localizationSourceZH)).font(.caption2).foregroundStyle(.secondary)
                }
            }
        }
    }
}

/// Compact horizontal Time gradient legend (Early → Late) shown by every embedding panel when Color by = Time.
/// The bar uses the unified `ISIStateSpacePalette` gradient (#399E65 → #EC6965) so it matches the points, and
/// mirrors the neural-manifold time legend's house style. Display-only.
private struct ISIStateTimeLegend: View {
    @Environment(\.l10n) private var l10n
    var body: some View {
        HStack(spacing: 7) {
            Text(l10n.t("时间"))
                .font(.caption2.weight(.semibold))
                .foregroundStyle(.secondary)
            Text(l10n.t("早"))
                .font(.caption2)
                .foregroundStyle(.secondary)
            RoundedRectangle(cornerRadius: 3)
                .fill(
                    LinearGradient(
                        colors: [ISIStateSpacePalette.timeGradientStart, ISIStateSpacePalette.timeGradientEnd],
                        startPoint: .leading,
                        endPoint: .trailing
                    )
                )
                .frame(width: 72, height: 8)
            Text(l10n.t("晚"))
                .font(.caption2)
                .foregroundStyle(.secondary)
        }
    }
}

/// P2: Isomap panel — runs the pure `ISIStateSpaceIsomap` on the current feature rows and plots dim1 vs dim2.
private struct ISIStateIsomapScatterView: View {
    let trains: [SpikeTrain]
    let halfWindowK: Int
    let minValidISISec: Double
    let winsorize: Bool
    let scaling: ISIStatePCAScaling
    let qualitySettings: SpikeQualitySettings
    let colorMode: ISIStateSpaceColorMode
    let displayUnit: QualityDisplayUnit
    let autoIntervals: SpikePatternIntervals
    let finalIntervals: SpikePatternIntervals
    let neighbors: Int
    let componentPolicy: ISIStateSpaceIsomapComponentPolicy

    private var rows: [SpikeISIStateSpaceRow] {
        trains.flatMap {
            SpikeISIStateSpaceFeatureBuilder.makeRows(
                train: $0, k: halfWindowK, minValidISISec: minValidISISec,
                winsorize: winsorize, qualitySettings: qualitySettings
            )
        }
    }

    private var outcome: Result<(points: [ISIStateScatterDatum], diagnostics: ISIStateSpaceIsomapDiagnostics), ISIStateSpaceIsomapError> {
        do {
            let result = try ISIStateSpaceIsomap.run(
                rows: rows, neighborCount: neighbors, scaling: scaling, componentPolicy: componentPolicy
            )
            let points = result.points.compactMap { point -> ISIStateScatterDatum? in
                guard let d1 = point.dim1, let d2 = point.dim2, d1.isFinite, d2.isFinite else { return nil }
                let row = point.row
                return ISIStateScatterDatum(
                    x: d1, y: d2, trainID: row.trainID, timeMidSec: row.timeMidSec,
                    autoState: autoIntervals.state(forTrainID: row.trainID, atSec: row.timeMidSec),
                    finalState: finalIntervals.state(forTrainID: row.trainID, atSec: row.timeMidSec),
                    qcStatus: row.qcStatus,
                    source: .isi(row)
                )
            }
            return .success((points, result.diagnostics))
        } catch let error as ISIStateSpaceIsomapError {
            return .failure(error)
        } catch {
            return .failure(.tooFewRows)
        }
    }

    var body: some View {
        let outcome = self.outcome
        return VStack(alignment: .leading, spacing: 8) {
            HStack(alignment: .firstTextBaseline, spacing: 10) {
                Text("Isomap（维度 1 · 维度 2）").font(.headline)
                Text(subtitle(outcome)).font(.caption).foregroundStyle(.secondary).monospacedDigit()
                Spacer()
            }
            .padding(.horizontal, 12).padding(.top, 10)

            content(outcome)
        }
        .background(Color(nsColor: .textBackgroundColor))
        .overlay(alignment: .top) {
            Rectangle().fill(Color(nsColor: .separatorColor).opacity(0.8)).frame(height: 1)
        }
    }

    @ViewBuilder
    private func content(_ outcome: Result<(points: [ISIStateScatterDatum], diagnostics: ISIStateSpaceIsomapDiagnostics), ISIStateSpaceIsomapError>) -> some View {
        switch outcome {
        case let .success(data) where data.points.count >= 2:
            VStack(alignment: .leading, spacing: 6) {
                if data.diagnostics.componentCount > 1 {
                    note("Graph disconnected (\(data.diagnostics.componentCount) components); embedding the largest \(data.diagnostics.largestComponentCount) of \(data.diagnostics.sampledCount) points. Increase Neighbors to connect.")
                }
                ISIStateEmbeddingScatterCanvas(points: data.points, colorMode: colorMode, xLabel: "Isomap1", yLabel: "Isomap2", title: "Isomap point", displayUnit: displayUnit)
                if colorMode.isPattern { ISIStatePatternLegend(colorMode: colorMode) }
                else if colorMode == .time { ISIStateTimeLegend() }
            }
            .padding(.horizontal, 12).padding(.bottom, 12)
        case .success:
            unavailable("Not enough finite Isomap points to plot.")
        case let .failure(error):
            unavailable(message(for: error))
        }
    }

    private func subtitle(_ outcome: Result<(points: [ISIStateScatterDatum], diagnostics: ISIStateSpaceIsomapDiagnostics), ISIStateSpaceIsomapError>) -> String {
        switch outcome {
        case let .success(data):
            let resid = data.diagnostics.residualVariance.map { String(format: "%.3f", $0) } ?? "—"
            return "\(data.points.count) pts · k=\(data.diagnostics.neighborCount) · residual var \(resid) · \(scaling == .robust ? "robust" : "z-score")"
        case .failure:
            return scaling == .robust ? "robust scaling" : "z-score scaling"
        }
    }

    private func message(for error: ISIStateSpaceIsomapError) -> String {
        switch error {
        case .tooFewRows: return "Need at least five ISI feature rows for Isomap."
        case .tooFewVaryingFeatures: return "Need at least two varying numeric ISI features for Isomap."
        case .disconnectedGraph: return "The Isomap kNN graph is disconnected. Increase Neighbors, or set Disconnected to Largest to embed the biggest component."
        case .componentTooSmall: return "The largest Isomap component has too few points. Increase Neighbors."
        }
    }

    private func note(_ text: String) -> some View {
        Text(text).font(.caption2).foregroundStyle(.secondary).padding(.horizontal, 12)
    }

    private func unavailable(_ text: String) -> some View {
        Text(text).font(.callout).foregroundStyle(.secondary)
            .frame(maxWidth: .infinity, maxHeight: .infinity).padding(20)
    }
}

/// P2: logISI phase-portrait panel — runs the pure `ISIStateSpaceLogISIPhasePortrait` per displayed train and
/// plots logISI_i vs logISI_{i+lag}. Color-by reuses the pattern/time/train semantics; QC is not available
/// per transition row (a note is shown).
private struct ISIStateLogISIPhasePortraitView: View {
    let trains: [SpikeTrain]
    let minValidISISec: Double
    let winsorize: Bool
    let lag: Int
    let displayUnit: QualityDisplayUnit
    let colorMode: ISIStateSpaceColorMode
    let autoIntervals: SpikePatternIntervals
    let finalIntervals: SpikePatternIntervals

    private var data: [ISIStateScatterDatum] {
        trains.flatMap { train -> [ISIStateScatterDatum] in
            let rows = ISIStateSpaceLogISIPhasePortrait.build(
                timestampsSec: train.timestampsSec, train: train.name,
                minValidISISec: minValidISISec, lag: lag, winsorize: winsorize
            )
            return rows.compactMap { row in
                guard row.logISIi.isFinite, row.logISINext.isFinite else { return nil }
                return ISIStateScatterDatum(
                    x: row.logISIi, y: row.logISINext, trainID: train.id, timeMidSec: row.timeMidSec,
                    autoState: autoIntervals.state(forTrainID: train.id, atSec: row.timeMidSec),
                    finalState: finalIntervals.state(forTrainID: train.id, atSec: row.timeMidSec),
                    qcStatus: nil,
                    source: .phasePortrait(row)
                )
            }
        }
    }

    var body: some View {
        let points = data
        return VStack(alignment: .leading, spacing: 8) {
            HStack(alignment: .firstTextBaseline, spacing: 10) {
                Text("logISI 相图").font(.headline)
                Text("\(points.count) transitions · lag \(lag)").font(.caption).foregroundStyle(.secondary).monospacedDigit()
                Spacer()
            }
            .padding(.horizontal, 12).padding(.top, 10)

            if points.count >= 2 {
                VStack(alignment: .leading, spacing: 6) {
                    if colorMode == .qc {
                        Text("逐转移点暂无 QC 着色，因此使用中性色；可选择时间、自动模式、最终模式或 spike train 着色。")
                            .font(.caption2).foregroundStyle(.secondary).padding(.horizontal, 12)
                    }
                    ISIStateEmbeddingScatterCanvas(points: points, colorMode: colorMode, xLabel: "logISIᵢ", yLabel: "logISIᵢ₊ₗ", title: "logISI transition", displayUnit: displayUnit)
                    if colorMode.isPattern { ISIStatePatternLegend(colorMode: colorMode) }
                    else if colorMode == .time { ISIStateTimeLegend() }
                }
                .padding(.horizontal, 12).padding(.bottom, 12)
            } else {
                Text("没有可绘制的有效 logISI 转移。每条序列至少需要 4 个 spike 及连续有效 ISI；可增加所选序列或降低最小有效 ISI。")
                    .font(.callout).foregroundStyle(.secondary)
                    .frame(maxWidth: .infinity, maxHeight: .infinity).padding(20)
            }
        }
        .background(Color(nsColor: .textBackgroundColor))
        .overlay(alignment: .top) {
            Rectangle().fill(Color(nsColor: .separatorColor).opacity(0.8)).frame(height: 1)
        }
    }
}

/// Shared data → screen projection for the embedding scatters (PCA / Isomap / logISI). The exact plot rect,
/// 6% padding span, and axis mapping the canvas draws with, factored out so the hover hit-test projects points
/// to the *same* pixels the draw uses (they can't drift). Pure geometry — never an embedding input.
private struct ISIStateScatterProjection {
    let plot: CGRect
    let xLo: Double
    let xHi: Double
    let yLo: Double
    let yHi: Double

    init?(xs: [Double], ys: [Double], size: CGSize) {
        guard let xMin = xs.min(), let xMax = xs.max(), let yMin = ys.min(), let yMax = ys.max() else { return nil }
        let plot = CGRect(x: 16, y: 12, width: size.width - 28, height: size.height - 34)
        guard plot.width > 1, plot.height > 1 else { return nil }
        func span(_ lo: Double, _ hi: Double) -> (Double, Double) {
            guard hi - lo > 1e-9 else { return (lo - 1, hi + 1) }
            let pad = (hi - lo) * 0.06
            return (lo - pad, hi + pad)
        }
        (xLo, xHi) = span(xMin, xMax)
        (yLo, yHi) = span(yMin, yMax)
        self.plot = plot
    }

    func screen(x: Double, y: Double) -> CGPoint {
        CGPoint(
            x: plot.minX + CGFloat((x - xLo) / (xHi - xLo)) * plot.width,
            y: plot.maxY - CGFloat((y - yLo) / (yHi - yLo)) * plot.height
        )
    }
}

/// ISI value formatter for the hover inspector — mirrors the page's `formatISI` (scales seconds to the active
/// display unit with the unit suffix) so the card matches the feature tables and Axes hover.
private func isiStateSpaceFormatISI(_ seconds: Double, unit: QualityDisplayUnit) -> String {
    guard seconds.isFinite else { return "NA" }
    let scaled = seconds * unit.scaleFromSeconds
    let suffix = unit.rawValue
    if abs(scaled) < 1e-12 { return "0 \(suffix)" }
    if abs(scaled) >= 100 { return "\(String(format: "%.3f", scaled)) \(suffix)" }
    if abs(scaled) >= 1 { return "\(String(format: "%.4f", scaled)) \(suffix)" }
    return "\(String(format: "%.6f", scaled)) \(suffix)"
}

/// Clamps the hover card so it stays inside the canvas, biased above-right of the cursor (falls back
/// below/left near the edges). Mirrors the Axes canvas's `hoverCardPosition`.
private func isiStateSpaceHoverCardPosition(for location: CGPoint, in size: CGSize, cardWidth: CGFloat, cardHeight: CGFloat) -> CGPoint {
    let margin: CGFloat = 12
    let xCandidate = location.x + cardWidth / 2 + 16
    let x: CGFloat
    if xCandidate + cardWidth / 2 + margin > size.width {
        x = max(cardWidth / 2 + margin, location.x - cardWidth / 2 - 16)
    } else {
        x = min(max(cardWidth / 2 + margin, xCandidate), size.width - cardWidth / 2 - margin)
    }
    let yCandidate = location.y - cardHeight / 2 - 14
    let y: CGFloat
    if yCandidate - cardHeight / 2 - margin < 0 {
        y = min(size.height - cardHeight / 2 - margin, location.y + cardHeight / 2 + 14)
    } else {
        y = min(max(cardHeight / 2 + margin, yCandidate), size.height - cardHeight / 2 - margin)
    }
    return CGPoint(x: max(cardWidth / 2 + margin, x), y: max(cardHeight / 2 + margin, y))
}

private enum ISIStateSpacePalette {
    static let artifactColor = Color(red: 255.0 / 255.0, green: 100.0 / 255.0, blue: 78.0 / 255.0)
    static let duplicateColor = Color(red: 255.0 / 255.0, green: 100.0 / 255.0, blue: 78.0 / 255.0)

    /// Unified Time color gradient for the ISI State Space embedding plots (PCA / Isomap / logISI phase
    /// portrait), matching the neural-manifold time gradient: #399E65 (early) → #EC6965 (late). Annotation /
    /// visualization only — never an embedding input.
    static let timeGradientStart = Color(red: Double(0x39) / 255, green: Double(0x9E) / 255, blue: Double(0x65) / 255)
    static let timeGradientEnd = Color(red: Double(0xEC) / 255, green: Double(0x69) / 255, blue: Double(0x65) / 255)

    /// Early → late interpolation of the unified Time gradient (`fraction` clamped to [0, 1]). Shared by every
    /// ISI State Space embedding canvas so all Time coloring is identical.
    static func timeColor(fraction: Double) -> Color {
        let t = max(0, min(1, fraction))
        let start = (r: Double(0x39) / 255, g: Double(0x9E) / 255, b: Double(0x65) / 255)
        let end = (r: Double(0xEC) / 255, g: Double(0x69) / 255, b: Double(0x65) / 255)
        return Color(
            red: start.r + (end.r - start.r) * t,
            green: start.g + (end.g - start.g) * t,
            blue: start.b + (end.b - start.b) * t
        )
    }

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

    /// Map the UI scaling choice to the pure PCA layer's R-compatible scaling.
    var pcaScaling: ISIStatePCAScaling {
        switch self {
        case .robust:
            return .robust
        case .zScore:
            return .zscore
        }
    }
}
