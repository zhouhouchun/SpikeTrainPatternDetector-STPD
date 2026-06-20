import STPDCore
import SwiftUI

private enum DatasetISIHistogramDisplayMode: String, CaseIterable, Hashable {
    case raw
    case balanced
    case overlay

    var title: String {
        switch self {
        case .raw:
            return "Raw"
        case .balanced:
            return "Balanced"
        case .overlay:
            return "Overlay"
        }
    }

    var yAxisTitle: String {
        switch self {
        case .raw:
            return "Count"
        case .balanced, .overlay:
            return "Fraction"
        }
    }
}

private struct DatasetHistogramBand: Identifiable, Hashable {
    let id: String
    let name: String
    let lowerSec: Double
    let upperSec: Double
    let color: Color
    let anchorCount: Int
    let source: String
}

struct DatasetISIHistogramView: View {
    @Bindable var document: RasterDocument

    @State private var displayMode: DatasetISIHistogramDisplayMode = .overlay
    @State private var displayUnit: QualityDisplayUnit = .milliseconds
    @State private var xMaxSec = 0.0
    @State private var logY = false
    @State private var showQC = true
    @State private var showStructuralBands = true

    var body: some View {
        ScrollView {
            VStack(alignment: .leading, spacing: 16) {
                header

                if let dataset = document.dataset {
                    let summary = DatasetISIHistogram.summarize(
                        dataset: dataset,
                        qualitySettings: document.qualitySettings,
                        binWidthSec: max(1e-9, document.detectorHistogramBinWidthMs / 1000),
                        xMaxSec: xMaxSec > 0 ? xMaxSec : nil
                    )
                    let bands = structuralBands(for: summary)

                    controls
                    summaryStrip(summary)
                    DatasetISIHistogramChart(
                        summary: summary,
                        mode: displayMode,
                        displayUnit: displayUnit,
                        logY: logY,
                        showQC: showQC,
                        qualitySettings: document.qualitySettings,
                        bands: showStructuralBands ? bands : []
                    )
                    .frame(minHeight: 430)
                    .background(Color(nsColor: .textBackgroundColor))
                    .clipShape(RoundedRectangle(cornerRadius: 8, style: .continuous))
                    .overlay {
                        RoundedRectangle(cornerRadius: 8, style: .continuous)
                            .stroke(Color(nsColor: .separatorColor).opacity(0.28), lineWidth: 1)
                    }

                    bandTable(bands: bands)
                    trainAuditTable(summary: summary)
                } else {
                    ContentUnavailableView(
                        "No Dataset",
                        systemImage: "chart.bar.xaxis",
                        description: Text(document.lastErrorMessage ?? "Open a raw spike timestamp CSV or load the bundled sample.")
                    )
                    .frame(maxWidth: .infinity, minHeight: 420)
                }
            }
            .padding(18)
            .frame(minWidth: 980, maxWidth: .infinity, alignment: .topLeading)
        }
        .frame(minWidth: 760)
    }

    private var header: some View {
        HStack(alignment: .firstTextBaseline, spacing: 10) {
            Image(systemName: "chart.bar.xaxis")
                .foregroundStyle(.secondary)
            Text("数据集 ISI 直方图")
                .font(.title3.weight(.semibold))
            Text("Live")
                .font(.caption.weight(.semibold))
                .foregroundStyle(.secondary)
                .padding(.horizontal, 8)
                .padding(.vertical, 3)
                .background(.quaternary, in: Capsule())
            Spacer()
            Text("Diagnostic only · structure-first detection")
                .font(.caption)
                .foregroundStyle(.secondary)
        }
    }

    private var controls: some View {
        HStack(alignment: .center, spacing: 18) {
            controlGroup("Mode") {
                GlassSegmentedControl(
                    options: DatasetISIHistogramDisplayMode.allCases.map { ($0, $0.title) },
                    selection: $displayMode,
                    minSegmentWidth: 66
                )
                .frame(width: 242)
            }

            controlGroup("Bin") {
                DebouncedDoubleField("5", value: binWidthBinding, width: 78, maxFractionDigits: 6)
                unitControl
            }

            controlGroup("X max") {
                DebouncedDoubleField("0", value: xMaxBinding, width: 86, maxFractionDigits: 6)
                Text("0 = auto")
                    .font(.caption)
                    .foregroundStyle(.tertiary)
            }

            Toggle("log Y", isOn: $logY)
                .toggleStyle(.checkbox)
            Toggle("QC", isOn: $showQC)
                .toggleStyle(.checkbox)
                .help("Show artifact and refractory-suspect thresholds.")
            Toggle("Structure bands", isOn: $showStructuralBands)
                .toggleStyle(.checkbox)
                .help("Show intervals inferred from the structural detector. These bands do not change histogram counts.")

            Spacer(minLength: 16)
        }
        .font(.subheadline)
        .padding(14)
        .background(.thinMaterial, in: RoundedRectangle(cornerRadius: 8, style: .continuous))
    }

    private func controlGroup<Content: View>(
        _ title: String,
        @ViewBuilder content: () -> Content
    ) -> some View {
        HStack(spacing: 9) {
            Text(title)
                .foregroundStyle(.secondary)
                .fixedSize(horizontal: true, vertical: false)
            content()
        }
    }

    private var unitControl: some View {
        GlassSegmentedControl(
            options: [
                (.milliseconds, "ms"),
                (.seconds, "s")
            ],
            selection: $displayUnit,
            minSegmentWidth: 30
        )
        .frame(width: 84)
    }

    private var binWidthBinding: Binding<Double> {
        Binding(
            get: {
                (document.detectorHistogramBinWidthMs / 1000) * displayUnit.scaleFromSeconds
            },
            set: { value in
                let seconds = max(1e-9, value / displayUnit.scaleFromSeconds)
                document.detectorHistogramBinWidthMs = seconds * 1000
            }
        )
    }

    private var xMaxBinding: Binding<Double> {
        Binding(
            get: {
                xMaxSec * displayUnit.scaleFromSeconds
            },
            set: { value in
                xMaxSec = max(0, value / displayUnit.scaleFromSeconds)
            }
        )
    }

    private func summaryStrip(_ summary: DatasetISIHistogramSummary) -> some View {
        HStack(spacing: 10) {
            histogramMetric("Valid ISI", "\(summary.totalValidISICount.formatted())")
            histogramMetric("Visible", "\(summary.visibleValidISICount.formatted())")
            histogramMetric("Trains", "\(summary.contributingTrainCount)/\(summary.trainRows.count)")
            histogramMetric("Bin", formatTime(summary.binWidthSec))
            histogramMetric("X max", formatTime(summary.xMaxSec))
            histogramMetric("Artifact excluded", "\(summary.artifactExcludedCount.formatted())")
        }
    }

    private func histogramMetric(_ title: String, _ value: String) -> some View {
        VStack(alignment: .leading, spacing: 3) {
            Text(title)
                .font(.caption)
                .foregroundStyle(.secondary)
            Text(value)
                .font(.headline)
                .monospacedDigit()
        }
        .padding(.horizontal, 14)
        .padding(.vertical, 10)
        .frame(minWidth: 120, alignment: .leading)
        .background(Color(nsColor: .controlBackgroundColor).opacity(0.55), in: RoundedRectangle(cornerRadius: 7))
    }

    private func structuralBands(for summary: DatasetISIHistogramSummary) -> [DatasetHistogramBand] {
        guard let run = document.classicAnchorDetectionRun else {
            return []
        }

        let structural = run.datasetStructuralSeedSummary
        var bands: [DatasetHistogramBand] = []
        let minValid = max(0, document.qualitySettings.artifactThresholdSec)

        if let burstUpper = structural.burstSeedUpperSec,
           burstUpper > minValid,
           structural.burstAnchorCount > 0 {
            bands.append(
                DatasetHistogramBand(
                    id: "burst",
                    name: "Burst",
                    lowerSec: minValid,
                    upperSec: burstUpper,
                    color: patternColor(.burst),
                    anchorCount: structural.burstAnchorCount,
                    source: structural.source
                )
            )
        }

        if let lower = structural.tonicSeedLowerSec,
           let upper = structural.tonicSeedUpperSec,
           upper > lower,
           structural.tonicAnchorCount > 0 {
            bands.append(
                DatasetHistogramBand(
                    id: "tonic",
                    name: "Tonic",
                    lowerSec: lower,
                    upperSec: upper,
                    color: patternColor(.tonic),
                    anchorCount: structural.tonicAnchorCount,
                    source: structural.source
                )
            )
        }

        if let lower = structural.pauseSeedLowerSec,
           structural.pauseAnchorCount + structural.pausePoolAnchorCount > 0 {
            let upper = max(lower, structural.pauseSeedUpperSec ?? summary.xMaxSec)
            if upper > lower {
                bands.append(
                    DatasetHistogramBand(
                        id: "pause",
                        name: "Pause",
                        lowerSec: lower,
                        upperSec: upper,
                        color: patternColor(.pause),
                        anchorCount: structural.pauseAnchorCount + structural.pausePoolAnchorCount,
                        source: structural.pausePoolSource
                    )
                )
            }
        }

        return bands
    }

    private func bandTable(bands: [DatasetHistogramBand]) -> some View {
        VStack(alignment: .leading, spacing: 8) {
            sectionTitle("Structure-derived ISI bands")

            if bands.isEmpty {
                Text(document.classicAnchorDetectionRun == nil
                     ? "Run structural detection to show structure-derived ISI bands. Histogram counts remain available before detection."
                     : "No structure-derived dataset bands are available for the current detection run.")
                    .font(.subheadline)
                    .foregroundStyle(.secondary)
                    .padding(14)
                    .frame(maxWidth: .infinity, alignment: .leading)
                    .background(Color(nsColor: .controlBackgroundColor).opacity(0.45), in: RoundedRectangle(cornerRadius: 7))
            } else {
                VStack(spacing: 0) {
                    bandRowHeader
                    ForEach(bands) { band in
                        HStack(spacing: 14) {
                            HStack(spacing: 7) {
                                Capsule()
                                    .fill(band.color)
                                    .frame(width: 28, height: 4)
                                Text(band.name)
                                    .fontWeight(.semibold)
                            }
                            .frame(width: 150, alignment: .leading)
                            Text(formatTime(band.lowerSec))
                                .frame(width: 110, alignment: .leading)
                            Text(formatTime(band.upperSec))
                                .frame(width: 110, alignment: .leading)
                            Text("\(band.anchorCount)")
                                .monospacedDigit()
                                .frame(width: 80, alignment: .leading)
                            Text(band.source)
                                .foregroundStyle(.secondary)
                                .lineLimit(1)
                                .truncationMode(.middle)
                                .frame(maxWidth: .infinity, alignment: .leading)
                        }
                        .font(.subheadline)
                        .padding(.horizontal, 10)
                        .padding(.vertical, 8)
                        .background(Color(nsColor: .textBackgroundColor))
                    }
                }
                .clipShape(RoundedRectangle(cornerRadius: 7))
                .overlay {
                    RoundedRectangle(cornerRadius: 7)
                        .stroke(Color(nsColor: .separatorColor).opacity(0.28), lineWidth: 1)
                }
            }
        }
    }

    private var bandRowHeader: some View {
        HStack(spacing: 14) {
            Text("Pattern").frame(width: 150, alignment: .leading)
            Text("Lower").frame(width: 110, alignment: .leading)
            Text("Upper").frame(width: 110, alignment: .leading)
            Text("Anchors").frame(width: 80, alignment: .leading)
            Text("Source").frame(maxWidth: .infinity, alignment: .leading)
        }
        .font(.caption.weight(.semibold))
        .foregroundStyle(.secondary)
        .padding(.horizontal, 10)
        .padding(.vertical, 8)
        .background(Color(nsColor: .controlBackgroundColor).opacity(0.55))
    }

    private func trainAuditTable(summary: DatasetISIHistogramSummary) -> some View {
        VStack(alignment: .leading, spacing: 8) {
            sectionTitle("Per-train ISI audit")

            ScrollView(.horizontal) {
                VStack(alignment: .leading, spacing: 0) {
                    trainRowHeader
                    ForEach(Array(summary.trainRows.prefix(80))) { row in
                        trainRow(row)
                    }
                }
                .clipShape(RoundedRectangle(cornerRadius: 7))
                .overlay {
                    RoundedRectangle(cornerRadius: 7)
                        .stroke(Color(nsColor: .separatorColor).opacity(0.28), lineWidth: 1)
                }
            }
            .scrollIndicators(.visible)
        }
    }

    private var trainRowHeader: some View {
        HStack(spacing: 12) {
            tableHeader("Train", width: 280)
            tableHeader("Valid", width: 70)
            tableHeader("Visible", width: 70)
            tableHeader("Artifact", width: 74)
            tableHeader("Min", width: 92)
            tableHeader("Q10", width: 92)
            tableHeader("Q25", width: 92)
            tableHeader("Median", width: 92)
            tableHeader("Q75", width: 92)
            tableHeader("Q90", width: 92)
            tableHeader("Max", width: 92)
        }
        .padding(.horizontal, 10)
        .padding(.vertical, 8)
        .background(Color(nsColor: .controlBackgroundColor).opacity(0.55))
    }

    private func trainRow(_ row: DatasetISIHistogramTrainRow) -> some View {
        HStack(spacing: 12) {
            Text(row.trainName)
                .fontWeight(.medium)
                .lineLimit(1)
                .truncationMode(.middle)
                .frame(width: 280, alignment: .leading)
            Text("\(row.validISICount)").frame(width: 70, alignment: .leading)
            Text("\(row.visibleISICount)").frame(width: 70, alignment: .leading)
            Text("\(row.artifactExcludedCount)").frame(width: 74, alignment: .leading)
            tableTime(row.minISISec, width: 92)
            tableTime(row.q10ISISec, width: 92)
            tableTime(row.q25ISISec, width: 92)
            tableTime(row.medianISISec, width: 92)
            tableTime(row.q75ISISec, width: 92)
            tableTime(row.q90ISISec, width: 92)
            tableTime(row.maxISISec, width: 92)
        }
        .font(.subheadline)
        .monospacedDigit()
        .padding(.horizontal, 10)
        .padding(.vertical, 7)
        .background(Color(nsColor: .textBackgroundColor))
    }

    private func tableHeader(_ text: String, width: CGFloat) -> some View {
        Text(text)
            .font(.caption.weight(.semibold))
            .foregroundStyle(.secondary)
            .frame(width: width, alignment: .leading)
    }

    private func tableTime(_ value: Double?, width: CGFloat) -> some View {
        Text(value.map(formatTime) ?? "NA")
            .foregroundStyle(value == nil ? .tertiary : .primary)
            .frame(width: width, alignment: .leading)
    }

    private func sectionTitle(_ title: String) -> some View {
        Text(title)
            .font(.headline)
    }

    private func formatTime(_ seconds: Double) -> String {
        let value = seconds * displayUnit.scaleFromSeconds
        guard value.isFinite else {
            return "NA"
        }
        if displayUnit == .milliseconds {
            if value < 10 {
                return String(format: "%.3f ms", value)
            }
            if value < 100 {
                return String(format: "%.2f ms", value)
            }
            return String(format: "%.1f ms", value)
        }
        if value < 1 {
            return String(format: "%.4f s", value)
        }
        if value < 10 {
            return String(format: "%.3f s", value)
        }
        return String(format: "%.2f s", value)
    }
}

private struct DatasetISIHistogramChart: View {
    let summary: DatasetISIHistogramSummary
    let mode: DatasetISIHistogramDisplayMode
    let displayUnit: QualityDisplayUnit
    let logY: Bool
    let showQC: Bool
    let qualitySettings: SpikeQualitySettings
    let bands: [DatasetHistogramBand]

    @State private var hoverLocation: CGPoint?

    var body: some View {
        GeometryReader { proxy in
            let model = DatasetISIHistogramPlotModel(
                summary: summary,
                mode: mode,
                displayUnit: displayUnit,
                logY: logY,
                size: proxy.size
            )

            ZStack(alignment: .topLeading) {
                Canvas { context, _ in
                    draw(model: model, context: &context)
                }

                if let hoverLocation,
                   let bin = model.bin(at: hoverLocation) {
                    histogramHoverCard(bin: bin)
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
        }
        .frame(minHeight: 420)
    }

    private func draw(model: DatasetISIHistogramPlotModel, context: inout GraphicsContext) {
        drawBackground(model: model, context: &context)
        drawBands(model: model, context: &context)
        drawBars(model: model, context: &context)
        drawBalancedLineIfNeeded(model: model, context: &context)
        if showQC {
            drawQCThresholds(model: model, context: &context)
        }
        drawAxes(model: model, context: &context)
        drawLegend(model: model, context: &context)
    }

    private func drawBackground(model: DatasetISIHistogramPlotModel, context: inout GraphicsContext) {
        context.fill(Path(CGRect(origin: .zero, size: model.size)), with: .color(Color(nsColor: .textBackgroundColor)))

        let title = Text("Dataset ISI distribution")
            .font(.caption.weight(.semibold))
            .foregroundStyle(.secondary)
        context.draw(title, at: CGPoint(x: model.plotRect.minX, y: 20), anchor: .leading)

        for tick in model.yTicks {
            let y = model.yPosition(for: tick)
            var path = Path()
            path.move(to: CGPoint(x: model.plotRect.minX, y: y))
            path.addLine(to: CGPoint(x: model.plotRect.maxX, y: y))
            context.stroke(path, with: .color(Color(nsColor: .gridColor).opacity(0.24)), lineWidth: 0.7)
            context.draw(
                Text(model.yLabel(for: tick))
                    .font(.caption2)
                    .foregroundStyle(.secondary),
                at: CGPoint(x: model.plotRect.minX - 8, y: y),
                anchor: .trailing
            )
        }

        var border = Path()
        border.addRect(model.plotRect)
        context.stroke(border, with: .color(Color(nsColor: .separatorColor).opacity(0.45)), lineWidth: 1)
    }

    private func drawBands(model: DatasetISIHistogramPlotModel, context: inout GraphicsContext) {
        for band in bands {
            let x0 = model.xPosition(for: band.lowerSec)
            let x1 = model.xPosition(for: band.upperSec)
            let rect = CGRect(
                x: min(x0, x1),
                y: model.plotRect.minY,
                width: max(1, abs(x1 - x0)),
                height: model.plotRect.height
            )
            context.fill(Path(rect), with: .color(band.color.opacity(0.08)))
            context.stroke(Path(rect), with: .color(band.color.opacity(0.32)), lineWidth: 1)
            context.draw(
                Text(band.name)
                    .font(.caption2.weight(.semibold))
                    .foregroundStyle(band.color.opacity(0.88)),
                at: CGPoint(x: rect.minX + 5, y: rect.minY + 11),
                anchor: .leading
            )
        }
    }

    private func drawBars(model: DatasetISIHistogramPlotModel, context: inout GraphicsContext) {
        for bin in summary.bins {
            let x = model.xPosition(for: bin.binLeftSec)
            let width = max(1, model.xPosition(for: bin.binRightSec) - x)
            let inset = min(width * 0.16, 2.5)

            switch mode {
            case .raw:
                drawBar(
                    value: Double(bin.rawCount),
                    x: x + inset,
                    width: max(1, width - inset * 2),
                    color: Color(nsColor: .labelColor).opacity(0.64),
                    model: model,
                    context: &context
                )
            case .balanced:
                drawBar(
                    value: bin.trainBalancedFraction,
                    x: x + inset,
                    width: max(1, width - inset * 2),
                    color: Color(red: 0.42, green: 0.36, blue: 0.78).opacity(0.68),
                    model: model,
                    context: &context
                )
            case .overlay:
                drawBar(
                    value: bin.rawFraction,
                    x: x + inset,
                    width: max(1, width - inset * 2),
                    color: Color(nsColor: .labelColor).opacity(0.30),
                    model: model,
                    context: &context
                )
            }
        }
    }

    private func drawBar(
        value: Double,
        x: CGFloat,
        width: CGFloat,
        color: Color,
        model: DatasetISIHistogramPlotModel,
        context: inout GraphicsContext
    ) {
        guard value.isFinite, value > 0 else {
            return
        }
        let y = model.yPosition(for: value)
        let rect = CGRect(
            x: x,
            y: y,
            width: width,
            height: max(1, model.plotRect.maxY - y)
        )
        context.fill(Path(rect), with: .color(color))
    }

    private func drawBalancedLineIfNeeded(model: DatasetISIHistogramPlotModel, context: inout GraphicsContext) {
        guard mode == .overlay else {
            return
        }

        var path = Path()
        var hasPoint = false
        for bin in summary.bins where bin.trainBalancedFraction > 0 {
            let point = CGPoint(
                x: model.xPosition(for: bin.midSec),
                y: model.yPosition(for: bin.trainBalancedFraction)
            )
            if hasPoint {
                path.addLine(to: point)
            } else {
                path.move(to: point)
                hasPoint = true
            }
        }
        context.stroke(path, with: .color(Color(red: 0.42, green: 0.36, blue: 0.78).opacity(0.92)), lineWidth: 1.8)
    }

    private func drawQCThresholds(model: DatasetISIHistogramPlotModel, context: inout GraphicsContext) {
        drawVerticalRule(
            at: qualitySettings.artifactThresholdSec,
            color: .secondary,
            label: "Artifact",
            dash: [4, 4],
            model: model,
            context: &context
        )
        drawVerticalRule(
            at: qualitySettings.refractorySuspectThresholdSec,
            color: .orange,
            label: "Refractory",
            dash: [2, 3],
            model: model,
            context: &context
        )
    }

    private func drawVerticalRule(
        at seconds: Double,
        color: Color,
        label: String,
        dash: [CGFloat],
        model: DatasetISIHistogramPlotModel,
        context: inout GraphicsContext
    ) {
        guard seconds.isFinite, seconds >= 0, seconds <= summary.xMaxSec else {
            return
        }
        let x = model.xPosition(for: seconds)
        var path = Path()
        path.move(to: CGPoint(x: x, y: model.plotRect.minY))
        path.addLine(to: CGPoint(x: x, y: model.plotRect.maxY))
        context.stroke(
            path,
            with: .color(color.opacity(0.52)),
            style: StrokeStyle(lineWidth: 1, dash: dash)
        )
        context.draw(
            Text(label)
                .font(.caption2)
                .foregroundStyle(color.opacity(0.82)),
            at: CGPoint(x: x + 4, y: model.plotRect.maxY - 10),
            anchor: .leading
        )
    }

    private func drawAxes(model: DatasetISIHistogramPlotModel, context: inout GraphicsContext) {
        for tick in model.xTicks {
            let x = model.xPosition(for: tick)
            var path = Path()
            path.move(to: CGPoint(x: x, y: model.plotRect.maxY))
            path.addLine(to: CGPoint(x: x, y: model.plotRect.maxY + 4))
            context.stroke(path, with: .color(Color(nsColor: .separatorColor).opacity(0.55)), lineWidth: 1)
            context.draw(
                Text(model.xLabel(for: tick))
                    .font(.caption2)
                    .foregroundStyle(.secondary),
                at: CGPoint(x: x, y: model.plotRect.maxY + 18),
                anchor: .center
            )
        }

        context.draw(
            Text("ISI (\(displayUnit.rawValue))")
                .font(.caption2.weight(.semibold))
                .foregroundStyle(.secondary),
            at: CGPoint(x: model.plotRect.midX, y: model.size.height - 10),
            anchor: .center
        )
        context.draw(
            Text(mode.yAxisTitle)
                .font(.caption2.weight(.semibold))
                .foregroundStyle(.secondary),
            at: CGPoint(x: 12, y: model.plotRect.minY - 15),
            anchor: .leading
        )
    }

    private func drawLegend(model: DatasetISIHistogramPlotModel, context: inout GraphicsContext) {
        let entries: [(String, Color)] = switch mode {
        case .raw:
            [("Raw pooled count", Color(nsColor: .labelColor).opacity(0.64))]
        case .balanced:
            [("Train-balanced fraction", Color(red: 0.42, green: 0.36, blue: 0.78).opacity(0.82))]
        case .overlay:
            [
                ("Raw pooled fraction", Color(nsColor: .labelColor).opacity(0.42)),
                ("Train-balanced fraction", Color(red: 0.42, green: 0.36, blue: 0.78).opacity(0.92))
            ]
        }

        var x = model.plotRect.maxX
        let y = model.plotRect.minY - 18
        for entry in entries.reversed() {
            let textWidth = CGFloat(entry.0.count) * 5.7 + 30
            x -= textWidth
            var line = Path()
            line.move(to: CGPoint(x: x, y: y))
            line.addLine(to: CGPoint(x: x + 18, y: y))
            context.stroke(line, with: .color(entry.1), lineWidth: 4)
            context.draw(
                Text(entry.0)
                    .font(.caption2.weight(.medium))
                    .foregroundStyle(.secondary),
                at: CGPoint(x: x + 24, y: y),
                anchor: .leading
            )
        }
    }

    private func histogramHoverCard(bin: DatasetISIHistogramBin) -> some View {
        VStack(alignment: .leading, spacing: 6) {
            Text("ISI bin")
                .font(.caption.weight(.semibold))
            Divider()
            hoverRow("Range", "\(formatTime(bin.binLeftSec)) - \(formatTime(bin.binRightSec))")
            hoverRow("Raw count", "\(bin.rawCount)")
            hoverRow("Raw fraction", percent(bin.rawFraction))
            hoverRow("Train-balanced", percent(bin.trainBalancedFraction))
        }
        .font(.caption)
        .padding(10)
        .background(.ultraThinMaterial, in: RoundedRectangle(cornerRadius: 10, style: .continuous))
        .overlay {
            RoundedRectangle(cornerRadius: 10, style: .continuous)
                .stroke(Color(nsColor: .separatorColor).opacity(0.28), lineWidth: 1)
        }
        .shadow(color: .black.opacity(0.10), radius: 18, y: 8)
        .frame(width: 220, alignment: .leading)
    }

    private func hoverRow(_ title: String, _ value: String) -> some View {
        HStack {
            Text(title)
                .foregroundStyle(.secondary)
            Spacer()
            Text(value)
                .monospacedDigit()
        }
    }

    private func formatTime(_ seconds: Double) -> String {
        let value = seconds * displayUnit.scaleFromSeconds
        if displayUnit == .milliseconds {
            if value < 10 { return String(format: "%.3f ms", value) }
            if value < 100 { return String(format: "%.2f ms", value) }
            return String(format: "%.1f ms", value)
        }
        if value < 1 { return String(format: "%.4f s", value) }
        if value < 10 { return String(format: "%.3f s", value) }
        return String(format: "%.2f s", value)
    }

    private func percent(_ value: Double) -> String {
        String(format: "%.2f%%", value * 100)
    }
}

private struct DatasetISIHistogramPlotModel {
    let summary: DatasetISIHistogramSummary
    let mode: DatasetISIHistogramDisplayMode
    let displayUnit: QualityDisplayUnit
    let logY: Bool
    let size: CGSize
    let plotRect: CGRect
    let yMax: Double
    let yMinPositive: Double
    let yTicks: [Double]
    let xTicks: [Double]

    init(
        summary: DatasetISIHistogramSummary,
        mode: DatasetISIHistogramDisplayMode,
        displayUnit: QualityDisplayUnit,
        logY: Bool,
        size: CGSize
    ) {
        self.summary = summary
        self.mode = mode
        self.displayUnit = displayUnit
        self.logY = logY
        self.size = size
        self.plotRect = CGRect(
            x: 64,
            y: 38,
            width: max(1, size.width - 84),
            height: max(1, size.height - 92)
        )

        let values = summary.bins.flatMap { bin -> [Double] in
            switch mode {
            case .raw:
                [Double(bin.rawCount)]
            case .balanced:
                [bin.trainBalancedFraction]
            case .overlay:
                [bin.rawFraction, bin.trainBalancedFraction]
            }
        }.filter { $0.isFinite && $0 > 0 }
        let maximum = values.max() ?? 1
        let resolvedYMax = Self.niceCeil(maximum)
        let resolvedYMinPositive = max((values.min() ?? resolvedYMax) / 2, 1e-9)
        self.yMax = resolvedYMax
        self.yMinPositive = resolvedYMinPositive
        self.yTicks = logY
            ? Self.logTicks(minValue: yMinPositive, maxValue: yMax)
            : Self.linearTicks(maxValue: yMax)
        self.xTicks = Self.linearXTicks(maxValue: summary.xMaxSec)
    }

    func xPosition(for seconds: Double) -> CGFloat {
        guard summary.xMaxSec > 0 else {
            return plotRect.minX
        }
        let fraction = min(max(seconds / summary.xMaxSec, 0), 1)
        return plotRect.minX + plotRect.width * CGFloat(fraction)
    }

    func yPosition(for value: Double) -> CGFloat {
        guard value.isFinite, value > 0 else {
            return plotRect.maxY
        }
        if logY {
            let low = log10(yMinPositive)
            let high = log10(max(yMax, yMinPositive * 10))
            let fraction = (log10(max(value, yMinPositive)) - low) / max(high - low, 1e-9)
            return plotRect.maxY - plotRect.height * CGFloat(min(max(fraction, 0), 1))
        }
        let fraction = value / max(yMax, 1e-9)
        return plotRect.maxY - plotRect.height * CGFloat(min(max(fraction, 0), 1))
    }

    func bin(at location: CGPoint) -> DatasetISIHistogramBin? {
        guard plotRect.contains(location),
              summary.xMaxSec > 0,
              !summary.bins.isEmpty else {
            return nil
        }
        let fraction = Double((location.x - plotRect.minX) / plotRect.width)
        let seconds = min(max(fraction, 0), 1) * summary.xMaxSec
        let index = min(max(Int(floor(seconds / summary.binWidthSec)), 0), summary.bins.count - 1)
        return summary.bins[index]
    }

    func hoverCardPosition(for location: CGPoint) -> CGPoint {
        let x = min(max(location.x + 126, 120), size.width - 120)
        let y = min(max(location.y - 72, 72), size.height - 92)
        return CGPoint(x: x, y: y)
    }

    func yLabel(for value: Double) -> String {
        switch mode {
        case .raw:
            return value >= 10 ? String(format: "%.0f", value) : String(format: "%.1f", value)
        case .balanced, .overlay:
            return String(format: "%.0f%%", value * 100)
        }
    }

    func xLabel(for seconds: Double) -> String {
        let value = seconds * displayUnit.scaleFromSeconds
        if displayUnit == .milliseconds {
            if value < 100 { return String(format: "%.1f", value) }
            return String(format: "%.0f", value)
        }
        if value < 1 { return String(format: "%.3f", value) }
        if value < 10 { return String(format: "%.2f", value) }
        return String(format: "%.1f", value)
    }

    private static func niceCeil(_ value: Double) -> Double {
        guard value.isFinite, value > 0 else {
            return 1
        }
        let exponent = floor(log10(value))
        let scale = pow(10, exponent)
        let normalized = value / scale
        let step: Double
        if normalized <= 1 {
            step = 1
        } else if normalized <= 2 {
            step = 2
        } else if normalized <= 5 {
            step = 5
        } else {
            step = 10
        }
        return step * scale
    }

    private static func linearTicks(maxValue: Double) -> [Double] {
        let upper = max(maxValue, 1e-9)
        return (0...4).map { upper * Double($0) / 4.0 }
    }

    private static func logTicks(minValue: Double, maxValue: Double) -> [Double] {
        let lowPower = Int(floor(log10(max(minValue, 1e-12))))
        let highPower = Int(ceil(log10(max(maxValue, minValue * 10))))
        var ticks: [Double] = []
        for power in lowPower...highPower {
            for multiplier in [1.0, 2.0, 5.0] {
                let value = multiplier * pow(10, Double(power))
                if value >= minValue * 0.95 && value <= maxValue * 1.05 {
                    ticks.append(value)
                }
            }
        }
        return ticks.isEmpty ? [minValue, maxValue] : ticks
    }

    private static func linearXTicks(maxValue: Double) -> [Double] {
        guard maxValue.isFinite, maxValue > 0 else {
            return [0]
        }
        return (0...5).map { maxValue * Double($0) / 5.0 }
    }
}

private func patternColor(_ label: ClassicAnchorLabel) -> Color {
    PatternLegendEntry.color(for: label)
}
