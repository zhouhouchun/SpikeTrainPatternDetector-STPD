import STPDCore
import SwiftUI

/// Standalone view for the distribution-first foundation (D1-D3). It renders a pre-shaped
/// `ISIDistributionFoundationPresentation` (from STPDCore) and depends ONLY on STPDCore + SwiftUI —
/// deliberately NO `RasterDocument` / `WorkbenchSection` dependency, so it compiles and previews
/// without the (currently dirty) Workbench nav files. Navigation wiring is a separate later step.
///
/// This is the distribution-first counterpart to the old QC `DatasetISIHistogramView`, and is clearly
/// labelled as such.
struct ISIDistributionFoundationView: View {
    let presentation: ISIDistributionFoundationPresentation

    /// Selected train for the overlay (UI-2B). `nil` ⇒ pooled-only. Held as local `@State` so the
    /// picker is self-contained — deliberately NOT wired to Workbench selection yet.
    @State private var selectedTrainID: String?

    /// The picked train's overlay detail, if it is still present in the current presentation.
    private var selectedDetail: ISIDistributionTrainDetail? {
        guard let selectedTrainID else { return nil }
        return presentation.perTrainDetails.first { $0.trainID == selectedTrainID }
    }

    /// Picker binding that reflects the EFFECTIVE selection: a stale id (not in the current
    /// presentation — e.g. after the debug host swaps in a different dataset) reads back as `nil`, so the
    /// Picker shows "None (pooled only)" rather than a blank label. This keeps the control consistent
    /// with the overlay, which also falls back to pooled-only via `selectedDetail == nil`.
    private var overlaySelection: Binding<String?> {
        Binding(get: { selectedDetail?.trainID }, set: { selectedTrainID = $0 })
    }

    /// Distinct (non-family) color for the selected-train overlay bars + dashed prior rules.
    private var selectedOverlayColor: Color { .pink }

    /// Convenience for the eventual nav call site (and the preview): shape the presentation from a
    /// dataset, preferring a D2-wired run distribution when available.
    init(dataset: SpikeDataset, runDistribution: DatasetISIDistribution?, minimumValidISISec: Double) {
        self.init(presentation: .from(dataset: dataset, runDistribution: runDistribution, minimumValidISISec: minimumValidISISec))
    }

    init(presentation: ISIDistributionFoundationPresentation) {
        self.presentation = presentation
    }

    var body: some View {
        ScrollView {
            VStack(alignment: .leading, spacing: 20) {
                header
                if presentation.pooledValidISICount == 0 && presentation.perTrain.isEmpty {
                    emptyState
                } else {
                    selectedTrainControl
                    chartSection
                    selectedTrainPriorsSection
                    quantileSection
                    perTrainSection
                    intervalSection
                }
            }
            .padding(20)
            .frame(maxWidth: .infinity, alignment: .topLeading)
        }
    }

    // MARK: Header

    private var header: some View {
        VStack(alignment: .leading, spacing: 6) {
            HStack(alignment: .firstTextBaseline, spacing: 10) {
                Image(systemName: "chart.bar.doc.horizontal")
                    .foregroundStyle(.secondary)
                Text("Distribution-first foundation (D1-D3)")
                    .font(.title3.weight(.semibold))
                badge(sourceText, tint: presentation.source == .detectionRun ? .green : .orange)
                Spacer()
            }
            Text("Distribution-first ISI models (D1/D2) + D3-derived burst/tonic/pause interval priors — separate from the old QC ISI histogram.")
                .font(.callout)
                .foregroundStyle(.secondary)
            HStack(spacing: 14) {
                metric("Dataset", presentation.datasetName)
                metric("Floor", "\(ms(presentation.floorSec)) ms")
                metric("Contributing trains", "\(presentation.contributingTrainCount)")
                metric("Pooled valid ISIs", "\(presentation.pooledValidISICount)")
            }
            .font(.caption)
        }
    }

    private var sourceText: String {
        switch presentation.source {
        case .detectionRun: return "from detection run"
        case .computedFromDataset: return "computed from dataset"
        }
    }

    // MARK: Sections

    private var quantileSection: some View {
        sectionCard("Dataset quantiles (seconds → ms)") {
            Grid(alignment: .leading, horizontalSpacing: 14, verticalSpacing: 6) {
                quantileHeaderRow
                Divider().gridCellColumns(9)
                quantileRow(presentation.pooled)
                if let balanced = presentation.trainBalanced {
                    quantileRow(balanced)
                }
            }
        }
    }

    private var perTrainSection: some View {
        sectionCard("Per-train valid ISIs + key quantiles") {
            Grid(alignment: .leading, horizontalSpacing: 14, verticalSpacing: 6) {
                quantileHeaderRow
                Divider().gridCellColumns(9)
                ForEach(presentation.perTrain, id: \.trainID) { row in
                    quantileRow(row)
                }
            }
        }
    }

    private var intervalSection: some View {
        sectionCard("D3 dataset interval priors") {
            if presentation.datasetIntervals.isEmpty {
                Text("No dataset-scope burst/tonic/pause priors derived.")
                    .foregroundStyle(.secondary)
            } else {
                VStack(alignment: .leading, spacing: 10) {
                    Text("core = central identity band (e.g. tonic q25–q75);  acc = wider acceptance / membership band;  bridge = burst extension.")
                        .font(.caption)
                        .foregroundStyle(.secondary)
                    ForEach(presentation.datasetIntervals, id: \.self) { interval in
                        intervalRow(interval)
                    }
                }
            }
        }
    }

    // MARK: Rows

    private var quantileHeaderRow: some View {
        GridRow {
            cell("Scope", bold: true)
            cell("n", bold: true)
            cell("min", bold: true); cell("q10", bold: true); cell("q25", bold: true)
            cell("q50", bold: true); cell("q75", bold: true); cell("q90", bold: true); cell("max", bold: true)
        }
    }

    private func quantileRow(_ row: ISIDistributionQuantileRow) -> some View {
        GridRow {
            cell(row.label)
            cell("\(row.validISICount)")
            cell(ms(row.minSec)); cell(ms(row.q10Sec)); cell(ms(row.q25Sec))
            cell(ms(row.q50Sec)); cell(ms(row.q75Sec)); cell(ms(row.q90Sec)); cell(ms(row.maxSec))
        }
    }

    private func intervalRow(_ interval: ISIModeIntervalRow) -> some View {
        HStack(alignment: .firstTextBaseline, spacing: 10) {
            Text(familyName(interval.family))
                .font(.body.weight(.semibold))
                .foregroundStyle(familyColor(interval.family))
                .frame(width: 64, alignment: .leading)

            VStack(alignment: .leading, spacing: 2) {
                // Core (identity) interval.
                Text("core  [\(ms(interval.lowerSec)) – \(ms(interval.upperSec)) ms]")
                    .monospacedDigit()
                // Acceptance (wider membership) interval, shown only when it differs from the core.
                if interval.acceptanceLowerSec != nil || interval.acceptanceUpperSec != nil {
                    Text("acc.  [\(ms(interval.acceptanceLowerSec ?? interval.lowerSec)) – \(ms(interval.acceptanceUpperSec ?? interval.upperSec)) ms]")
                        .monospacedDigit()
                        .foregroundStyle(.secondary)
                }
                // Burst bridge/extension upper.
                if let bridge = interval.bridgeUpperSec {
                    Text("bridge → \(ms(bridge)) ms")
                        .font(.caption)
                        .foregroundStyle(.secondary)
                }
            }

            Spacer(minLength: 8)

            badge(interval.scope.rawValue, tint: .indigo)
            badge(interval.provenanceOrigin.rawValue, tint: .purple)
            if interval.mayPropagateToDataset { badge("propagate", tint: .green) }
            if !interval.maySelectFinalLabel { badge("no-select", tint: .gray) }
            if interval.isAuditOnly { badge("audit-only", tint: .gray) }
            if !interval.isValid { badge("invalid", tint: .red) }
        }
    }

    // MARK: Selected-train overlay (UI-2B)

    private var selectedTrainControl: some View {
        HStack(spacing: 10) {
            Text("Overlay train")
                .font(.callout.weight(.medium))
            Picker("Overlay train", selection: overlaySelection) {
                Text("None (pooled only)").tag(String?.none)
                ForEach(presentation.perTrainDetails, id: \.trainID) { detail in
                    Text("\(detail.trainName)  ·  \(detail.validISICount) ISIs")
                        .tag(String?.some(detail.trainID))
                }
            }
            .labelsHidden()
            .frame(maxWidth: 340)
            if let selected = selectedDetail {
                HStack(spacing: 5) {
                    RoundedRectangle(cornerRadius: 2).fill(selectedOverlayColor).frame(width: 12, height: 10)
                    Text("overlaid: \(selected.trainName)")
                        .font(.caption).foregroundStyle(.secondary)
                }
            }
            Spacer()
        }
    }

    private var selectedTrainPriorsSection: some View {
        sectionCard("Selected-train interval priors") {
            if let selected = selectedDetail {
                if selected.intervals.isEmpty {
                    Text("No train-local burst/tonic/pause priors derived for “\(selected.trainName)”.")
                        .foregroundStyle(.secondary)
                } else {
                    VStack(alignment: .leading, spacing: 10) {
                        Text("Train-local priors for “\(selected.trainName)” (\(selected.validISICount) valid ISIs) — the “trainLocal” badge distinguishes these from the dataset priors below.")
                            .font(.caption)
                            .foregroundStyle(.secondary)
                        ForEach(selected.intervals, id: \.self) { interval in
                            intervalRow(interval)
                        }
                    }
                }
            } else {
                Text("Select a train above to overlay its ISI distribution and list its train-local priors.")
                    .foregroundStyle(.secondary)
            }
        }
    }

    // MARK: Chart (UI-2A + UI-2B overlay)

    private var chartSection: some View {
        sectionCard("Pooled ISI distribution (log ISI)") {
            if let histogram = presentation.histogram, histogram.totalCount > 0 {
                VStack(alignment: .leading, spacing: 8) {
                    chartLegend
                    Canvas { context, size in
                        drawFoundationChart(histogram: histogram, datasetIntervals: presentation.datasetIntervals,
                                            selected: selectedDetail, context: &context, size: size)
                    }
                    .frame(height: 240)
                    .background(Color(nsColor: .textBackgroundColor).opacity(0.6),
                                in: RoundedRectangle(cornerRadius: 6))
                }
            } else {
                Text("Not enough ISI data to plot a distribution.")
                    .foregroundStyle(.secondary)
            }
        }
    }

    private var chartLegend: some View {
        HStack(spacing: 14) {
            legendSwatch("burst", familyColor(.burst))
            legendSwatch("tonic", familyColor(.tonic))
            legendSwatch("pause", familyColor(.pause))
            Divider().frame(height: 12)
            Text("dark = core · light = acceptance · mid = bridge")
                .font(.caption2).foregroundStyle(.secondary)
            if selectedDetail != nil {
                Divider().frame(height: 12)
                legendSwatch("selected train", selectedOverlayColor)
                Text("dashed = its priors")
                    .font(.caption2).foregroundStyle(.secondary)
            }
        }
    }

    private func legendSwatch(_ label: String, _ color: Color) -> some View {
        HStack(spacing: 4) {
            RoundedRectangle(cornerRadius: 2).fill(color.opacity(0.5)).frame(width: 12, height: 10)
            Text(label).font(.caption2)
        }
    }

    private func drawFoundationChart(histogram: ISIDistributionHistogram, datasetIntervals: [ISIModeIntervalRow],
                                     selected: ISIDistributionTrainDetail?,
                                     context: inout GraphicsContext, size: CGSize) {
        let plot = CGRect(x: 8, y: 8, width: size.width - 16, height: size.height - 30)
        guard plot.width > 8, plot.height > 8, histogram.maxSec > histogram.minSec else { return }
        let logMin = log10(histogram.minSec)
        let logSpan = max(1e-9, log10(histogram.maxSec) - logMin)

        func x(_ sec: Double) -> CGFloat {
            let clamped = min(max(sec, histogram.minSec), histogram.maxSec)
            return plot.minX + CGFloat((log10(clamped) - logMin) / logSpan) * plot.width
        }
        func yTop(_ count: Int) -> CGFloat {
            let frac = histogram.maxCount > 0 ? CGFloat(count) / CGFloat(histogram.maxCount) : 0
            return plot.maxY - frac * plot.height
        }

        // 1) DATASET interval bands behind the bars: acceptance (lightest) < bridge (mid) < core (darkest).
        for interval in datasetIntervals {
            let color = familyColor(interval.family)
            if interval.acceptanceLowerSec != nil || interval.acceptanceUpperSec != nil {
                let al = interval.acceptanceLowerSec ?? interval.lowerSec
                let au = interval.acceptanceUpperSec ?? interval.upperSec
                fillBand(&context, x(al), x(au), plot, color.opacity(0.10))
            }
            if let bridge = interval.bridgeUpperSec, bridge > interval.upperSec {
                fillBand(&context, x(interval.upperSec), x(bridge), plot, color.opacity(0.16))
            }
            fillBand(&context, x(interval.lowerSec), x(interval.upperSec), plot, color.opacity(0.26))
            verticalRule(&context, x(interval.lowerSec), plot, color.opacity(0.6))
            verticalRule(&context, x(interval.upperSec), plot, color.opacity(0.6))
        }

        // 2) pooled histogram bars (gray).
        for bin in histogram.bins where bin.count > 0 {
            let x0 = x(bin.lowerSec), x1 = x(bin.upperSec)
            let top = yTop(bin.count)
            let rect = CGRect(x: x0 + 0.5, y: top, width: max(1, x1 - x0 - 1), height: plot.maxY - top)
            context.fill(Path(rect), with: .color(Color(nsColor: .labelColor).opacity(0.55)))
        }

        // 3) SELECTED-train overlay (UI-2B): its share of each pooled bin as translucent distinct bars,
        // then its train-local D3 priors as dashed rules (distinct from the dataset FILLED bands).
        if let selected, selected.histogramCounts.count == histogram.bins.count {
            for (index, bin) in histogram.bins.enumerated() {
                let count = selected.histogramCounts[index]
                guard count > 0 else { continue }
                let x0 = x(bin.lowerSec), x1 = x(bin.upperSec)
                let top = yTop(count)
                let rect = CGRect(x: x0 + 0.5, y: top, width: max(1, x1 - x0 - 1), height: plot.maxY - top)
                context.fill(Path(rect), with: .color(selectedOverlayColor.opacity(0.55)))
            }
        }
        if let selected {
            for interval in selected.intervals {
                let color = familyColor(interval.family)
                dashedRule(&context, x(interval.lowerSec), plot, color.opacity(0.9))
                dashedRule(&context, x(interval.upperSec), plot, color.opacity(0.9))
                if let bridge = interval.bridgeUpperSec, bridge > interval.upperSec {
                    dashedRule(&context, x(bridge), plot, color.opacity(0.6))
                }
            }
        }

        // 4) x baseline + log decade ticks / labels.
        context.stroke(Path { $0.move(to: CGPoint(x: plot.minX, y: plot.maxY)); $0.addLine(to: CGPoint(x: plot.maxX, y: plot.maxY)) },
                       with: .color(.secondary.opacity(0.5)), lineWidth: 1)
        for decade in decadeTicks(minSec: histogram.minSec, maxSec: histogram.maxSec) {
            let tx = x(decade)
            context.stroke(Path { $0.move(to: CGPoint(x: tx, y: plot.minY)); $0.addLine(to: CGPoint(x: tx, y: plot.maxY)) },
                           with: .color(.secondary.opacity(0.15)), lineWidth: 1)
            var label = context.resolve(Text(decadeLabel(decade)).font(.caption2))
            label.shading = .color(.secondary)
            context.draw(label, at: CGPoint(x: tx, y: plot.maxY + 11), anchor: .center)
        }
    }

    private func fillBand(_ context: inout GraphicsContext, _ x0: CGFloat, _ x1: CGFloat, _ plot: CGRect, _ color: Color) {
        let rect = CGRect(x: min(x0, x1), y: plot.minY, width: abs(x1 - x0), height: plot.height)
        context.fill(Path(rect), with: .color(color))
    }

    private func verticalRule(_ context: inout GraphicsContext, _ x: CGFloat, _ plot: CGRect, _ color: Color) {
        context.stroke(Path { $0.move(to: CGPoint(x: x, y: plot.minY)); $0.addLine(to: CGPoint(x: x, y: plot.maxY)) },
                       with: .color(color), lineWidth: 1)
    }

    private func dashedRule(_ context: inout GraphicsContext, _ x: CGFloat, _ plot: CGRect, _ color: Color) {
        context.stroke(Path { $0.move(to: CGPoint(x: x, y: plot.minY)); $0.addLine(to: CGPoint(x: x, y: plot.maxY)) },
                       with: .color(color), style: StrokeStyle(lineWidth: 1.5, dash: [4, 3]))
    }

    private func decadeTicks(minSec: Double, maxSec: Double) -> [Double] {
        var ticks: [Double] = []
        var decade = pow(10, (log10(minSec)).rounded(.down))
        while decade <= maxSec * 1.0000001 {
            if decade >= minSec { ticks.append(decade) }
            decade *= 10
        }
        return ticks
    }

    private func decadeLabel(_ sec: Double) -> String {
        sec >= 1 ? "\(Int(sec.rounded()))s" : "\(Int((sec * 1000).rounded()))ms"
    }

    private var emptyState: some View {
        VStack(alignment: .leading, spacing: 6) {
            Text("No valid ISI data")
                .font(.headline)
            Text("Load a dataset (or run detection) to populate the distribution-first foundation.")
                .foregroundStyle(.secondary)
        }
        .padding(.vertical, 12)
    }

    // MARK: Building blocks

    private func sectionCard<Content: View>(_ title: String, @ViewBuilder _ content: () -> Content) -> some View {
        VStack(alignment: .leading, spacing: 10) {
            Text(title).font(.headline)
            content()
        }
        .padding(14)
        .frame(maxWidth: .infinity, alignment: .topLeading)
        .background(.thinMaterial, in: RoundedRectangle(cornerRadius: 8))
    }

    private func cell(_ text: String, bold: Bool = false) -> some View {
        Text(text)
            .font(bold ? .caption.weight(.semibold) : .caption)
            .foregroundStyle(bold ? .secondary : .primary)
            .monospacedDigit()
    }

    private func metric(_ label: String, _ value: String) -> some View {
        HStack(spacing: 4) {
            Text(label + ":").foregroundStyle(.secondary)
            Text(value).fontWeight(.medium)
        }
    }

    private func badge(_ text: String, tint: Color) -> some View {
        Text(text)
            .font(.caption2.weight(.semibold))
            .padding(.horizontal, 7).padding(.vertical, 2)
            .background(tint.opacity(0.16), in: Capsule())
            .foregroundStyle(tint)
    }

    private func ms(_ sec: Double?) -> String {
        guard let sec else { return "—" }
        return String(format: "%.2f", sec * 1000)
    }

    private func familyName(_ family: ISIPatternFamily) -> String {
        switch family {
        case .burst: return "Burst"
        case .tonic: return "Tonic"
        case .pause: return "Pause"
        case .unknown: return "Unknown"
        }
    }

    private func familyColor(_ family: ISIPatternFamily) -> Color {
        // Match existing app/raster color semantics.
        switch family {
        case .burst: return .orange
        case .tonic: return .green
        case .pause: return .blue
        case .unknown: return .gray
        }
    }
}

// MARK: - Preview

private func isiDistributionFoundationPreviewDataset() -> SpikeDataset {
    func train(_ name: String, _ isis: [Double]) -> SpikeTrain {
        var ts = [0.0]
        for isi in isis { ts.append((ts.last ?? 0) + isi) }
        return SpikeTrain(name: name, timestampsSec: ts)
    }
    let burst = [0.003, 0.0032, 0.0035, 0.003, 0.0033]
    let tonic = [0.040, 0.052, 0.045, 0.058, 0.048, 0.055, 0.043, 0.050, 0.047, 0.053]
    // Large-ISI tail so D3 derives a pause interval too (a clear upper gap above the tonic mode).
    let pauseTail = [0.50, 0.60, 0.55, 0.52]
    return SpikeDataset(
        name: "preview", sourceDescription: "fixture",
        trains: [train("burst+tonic+pause", burst + tonic + pauseTail), train("tonic-only", tonic)])
}

#Preview("Distribution-first foundation (D1-D3)") {
    ISIDistributionFoundationView(
        dataset: isiDistributionFoundationPreviewDataset(),
        runDistribution: nil,
        minimumValidISISec: 0.001)
        .frame(width: 660, height: 760)
}
