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
            Text("[\(ms(interval.lowerSec)) – \(ms(interval.upperSec)) ms]")
                .monospacedDigit()
            if let bridge = interval.bridgeUpperSec {
                Text("bridge → \(ms(bridge)) ms")
                    .font(.caption)
                    .foregroundStyle(.secondary)
            }
            badge(interval.scope.rawValue, tint: .blue)
            badge(interval.provenanceOrigin.rawValue, tint: .purple)
            if interval.mayPropagateToDataset { badge("propagate", tint: .green) }
            if !interval.maySelectFinalLabel { badge("no-select", tint: .gray) }
            if interval.isAuditOnly { badge("audit-only", tint: .gray) }
            if !interval.isValid { badge("invalid", tint: .red) }
            Spacer()
        }
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
