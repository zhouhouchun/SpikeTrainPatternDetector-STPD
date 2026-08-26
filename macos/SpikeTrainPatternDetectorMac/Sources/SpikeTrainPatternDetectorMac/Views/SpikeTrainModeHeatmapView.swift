import AppKit
import STPDCore
import SwiftUI

/// Categorical counterpart to the ISI-intensity heatmap. It presents labelled time support rather
/// than a continuous intensity estimate, so the image is always nearest-neighbour and never blends
/// two biological modes into an invented intermediate colour.
struct SpikeTrainModeHeatmapView: View {
    @Bindable var document: RasterDocument
    @Environment(\.l10n) private var l10n
    @State private var timeMode: RasterTimeMode = .aligned
    @State private var source: SpikeTrainModeHeatmapSource = .combined
    @State private var columnCount = 256
    @State private var heatmap = SpikeTrainModeHeatmap.empty
    @State private var renderedImage: NSImage?
    @State private var isRebuilding = false

    private var visibleTrains: [SpikeTrain] {
        guard let dataset = document.dataset else { return [] }
        let selectedIDs = document.selectedTrainIDsIncludingFocusedCandidate(for: .raster)
        return dataset.trains.filter { selectedIDs.contains($0.id) }
    }

    private var automaticSegments: [SpikeTrainModeHeatmapSegment] {
        let annotations = document.classicAnchorStateAnnotations + document.classicAnchorPublicEventAnnotations
        return annotations.compactMap { annotation in
            guard let label = ManualAnnotationLabel(autoLabel: annotation.label) else { return nil }
            return SpikeTrainModeHeatmapSegment(
                trainID: annotation.trainID,
                label: label,
                rawStartSec: annotation.rawStartSec,
                rawEndSec: annotation.rawEndSec
            )
        }
    }

    private var manualSegments: [SpikeTrainModeHeatmapSegment] {
        document.rasterManualAnnotations.map {
            SpikeTrainModeHeatmapSegment(
                trainID: $0.trainID,
                label: $0.label,
                rawStartSec: $0.normalizedStartSec,
                rawEndSec: $0.normalizedEndSec
            )
        }
    }

    private var inputKey: ModeHeatmapInputKey {
        ModeHeatmapInputKey(
            datasetID: document.dataset?.id,
            trainIDs: visibleTrains.map(\.id),
            automaticSignature: signature(for: automaticSegments),
            manualSignature: signature(for: manualSegments),
            timeMode: timeMode,
            source: source,
            columnCount: columnCount
        )
    }

    var body: some View {
        VStack(alignment: .leading, spacing: 0) {
            header
            Divider()
            content
        }
        .frame(minWidth: 640)
        .delayedBackgroundProgress(
            isRebuilding ? l10n.t("正在生成模式热力图…") : nil,
            alignment: .center
        )
        .task(id: inputKey) {
            await rebuild(for: visibleTrains)
        }
    }

    private var header: some View {
        VStack(alignment: .leading, spacing: 9) {
            HStack(alignment: .firstTextBaseline) {
                Text(l10n.t("模式热力图"))
                    .font(.headline)
                Spacer()
                Text("\(visibleTrains.count) \(l10n.t("条"))")
                    .font(.caption.weight(.semibold))
                    .foregroundStyle(.secondary)
            }

            ViewThatFits(in: .horizontal) {
                HStack(spacing: 18) {
                    heatmapTimeControl
                    heatmapSourceControl
                    heatmapResolutionControl
                    heatmapDescription
                    Spacer(minLength: 0)
                }

                VStack(alignment: .leading, spacing: 8) {
                    HStack(spacing: 18) {
                        heatmapTimeControl
                        heatmapSourceControl
                        Spacer(minLength: 0)
                    }
                    HStack(spacing: 18) {
                        heatmapResolutionControl
                        heatmapDescription
                        Spacer(minLength: 0)
                    }
                }

                VStack(alignment: .leading, spacing: 8) {
                    heatmapTimeControl
                    heatmapSourceControl
                    heatmapResolutionControl
                    heatmapDescription
                }
            }
        }
        .padding(.horizontal, 16)
        .padding(.vertical, 12)
    }

    private var heatmapTimeControl: some View {
        HStack(spacing: 8) {
            Text(l10n.t("时间"))
                .foregroundStyle(.secondary)
                .lineLimit(1)
                .fixedSize(horizontal: true, vertical: false)
            GlassSegmentedControl(
                options: RasterTimeMode.allCases.map { ($0, l10n.t($0.title)) },
                selection: $timeMode,
                minSegmentWidth: l10n.language == .ru ? 72 : 58
            )
            .frame(width: l10n.language == .ru ? 214 : 180)
        }
    }

    private var heatmapSourceControl: some View {
        HStack(spacing: 8) {
            Text(l10n.t("来源"))
                .foregroundStyle(.secondary)
                .lineLimit(1)
                .fixedSize(horizontal: true, vertical: false)
            GlassSegmentedControl(
                options: SpikeTrainModeHeatmapSource.allCases.map { ($0, l10n.t($0.title)) },
                selection: $source,
                minSegmentWidth: l10n.language == .ru ? 78 : 54
            )
            .frame(width: l10n.language == .ru ? 278 : 210)
        }
    }

    private var heatmapResolutionControl: some View {
        HStack(spacing: 8) {
            Text(l10n.t("热力图清晰度"))
                .foregroundStyle(.secondary)
                .lineLimit(1)
                .fixedSize(horizontal: true, vertical: false)
            Picker("", selection: $columnCount) {
                Text("128").tag(128)
                Text("256").tag(256)
                Text("384").tag(384)
                Text("512").tag(512)
            }
            .labelsHidden()
            .frame(width: 78)
        }
    }

    private var heatmapDescription: some View {
        Text(l10n.t("状态、事件与其它标记分轨显示；不进行颜色插值。"))
            .font(.caption)
            .foregroundStyle(.secondary)
            .lineLimit(l10n.language == .ru ? 2 : 1)
            .fixedSize(horizontal: false, vertical: true)
    }

    @ViewBuilder
    private var content: some View {
        if document.dataset == nil {
            ContentUnavailableView(
                l10n.t("尚未加载光栅图"),
                systemImage: "rectangle.3.group.fill",
                description: Text(l10n.t("目前没有可用的 spike train。"))
            )
            .frame(maxWidth: .infinity, maxHeight: .infinity)
        } else if visibleTrains.isEmpty {
            ContentUnavailableView(
                l10n.t("没有可见的 spike train"),
                systemImage: "rectangle.3.group.fill",
                description: Text(l10n.t("请先在时间戳图中选择至少一条 spike train。"))
            )
            .frame(maxWidth: .infinity, maxHeight: .infinity)
        } else {
            GeometryReader { proxy in
                let yAxisWidth: CGFloat = 184
                let availablePlotHeight = max(proxy.size.height - 112, 1)
                let plotHeight = preferredPlotHeight(available: availablePlotHeight)
                let viewportWidth = max(proxy.size.width - yAxisWidth - 8, 1)
                let contentWidth = max(viewportWidth, CGFloat(heatmap.columnCount) * 2.25)

                VStack(alignment: .leading, spacing: 10) {
                    HStack(alignment: .top, spacing: 8) {
                        yAxis
                            .frame(width: yAxisWidth, height: plotHeight)
                        ScrollView(.horizontal) {
                            VStack(alignment: .leading, spacing: 6) {
                                plot
                                    .frame(width: contentWidth, height: plotHeight)
                                xAxis
                                    .frame(width: contentWidth)
                            }
                        }
                        .scrollIndicators(.visible)
                        .accessibilityLabel(l10n.t("模式热力图"))
                    }
                    legend
                    Spacer(minLength: 0)
                }
                .padding(.horizontal, 16)
                .padding(.vertical, 14)
            }
        }
    }

    private var plot: some View {
        ZStack {
            Color(nsColor: .textBackgroundColor)
            if let renderedImage {
                Image(nsImage: renderedImage)
                    .resizable()
                    .interpolation(.none)
                    .clipped()
            }
        }
        .clipShape(RoundedRectangle(cornerRadius: 8, style: .continuous))
        .overlay {
            RoundedRectangle(cornerRadius: 8, style: .continuous)
                .stroke(.separator.opacity(0.55), lineWidth: 1)
        }
    }

    private var yAxis: some View {
        HStack(spacing: 6) {
            Text(l10n.t("Spike train"))
                .font(.caption2.weight(.semibold))
                .foregroundStyle(.secondary)
                .fixedSize()
                .rotationEffect(.degrees(-90))
                .frame(width: 13)
            VStack(spacing: 0) {
                // Renderer uses origin=lower, so reverse the labels to keep first selected train
                // at the visual bottom just like the timestamp raster.
                ForEach(Array(visibleTrains.reversed()), id: \.id) { train in
                    VStack(alignment: .trailing, spacing: 1) {
                        Text(train.name)
                            .font(.caption2.monospacedDigit())
                            .lineLimit(1)
                            .truncationMode(.middle)
                        Text(l10n.t("状态 · 事件 · 其它"))
                            .font(.caption2)
                            .foregroundStyle(.tertiary)
                    }
                    .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .trailing)
                }
            }
        }
    }

    private var xAxis: some View {
        VStack(alignment: .leading, spacing: 3) {
            HStack(spacing: 0) {
                ForEach(Array(timeTicks.enumerated()), id: \.offset) { index, tick in
                    Text(TimeFormatting.seconds(tick))
                        .font(.caption2.monospacedDigit())
                        .foregroundStyle(.secondary)
                        .frame(
                            maxWidth: index == 0 || index == timeTicks.count - 1 ? nil : .infinity,
                            alignment: index == 0 ? .leading : index == timeTicks.count - 1 ? .trailing : .center
                        )
                }
            }
            Text(l10n.t(timeMode.axisTitle))
                .font(.caption2.weight(.semibold))
                .foregroundStyle(.secondary)
        }
    }

    private var legend: some View {
        Group {
            if !legendLabels.isEmpty {
                VStack(alignment: .leading, spacing: 6) {
                    Text(l10n.t("模式图例"))
                        .font(.caption.weight(.semibold))
                    FlowLayout(spacing: 10) {
                        ForEach(legendLabels, id: \.self) { label in
                            HStack(spacing: 5) {
                                Rectangle()
                                    .fill(ModeHeatmapPalette.color(for: label))
                                    .frame(width: 14, height: 10)
                                Text(l10n.t(label.displayName))
                                    .font(.caption)
                                    .foregroundStyle(.secondary)
                            }
                        }
                    }
                    Text(l10n.t("手工标记在同一轨道内覆盖自动显示，但不会删除自动检测记录。"))
                        .font(.caption2)
                        .foregroundStyle(.secondary)
                }
            }
        }
    }

    private var legendLabels: [ManualAnnotationLabel] {
        let visibleCodes = Set(
            heatmap.stateCells + heatmap.eventCells + heatmap.otherCells
        )
        return ManualAnnotationLabel.allCases.filter { label in
            label.polarity == .positive &&
                visibleCodes.contains(SpikeTrainModeHeatmap.code(for: label))
        }
    }

    private var timeTicks: [Double] {
        guard heatmap.upperTimeSec > heatmap.lowerTimeSec else { return [] }
        let duration = heatmap.upperTimeSec - heatmap.lowerTimeSec
        return (0...4).map { heatmap.lowerTimeSec + duration * Double($0) / 4 }
    }

    private func preferredPlotHeight(available: CGFloat) -> CGFloat {
        let rowCount = max(visibleTrains.count, 1)
        let preferredTrainHeight: CGFloat = 72
        let trainHeight = min(preferredTrainHeight, available / CGFloat(rowCount))
        return max(trainHeight * CGFloat(rowCount), 1)
    }

    private func signature(for segments: [SpikeTrainModeHeatmapSegment]) -> String {
        segments.map {
            "\($0.trainID)|\($0.label.rawValue)|\($0.rawStartSec)|\($0.rawEndSec)"
        }
        .sorted()
        .joined(separator: ";")
    }

    private func rebuild(for trains: [SpikeTrain]) async {
        isRebuilding = true
        let automatic = automaticSegments
        let manual = manualSegments
        let selectedMode = timeMode
        let selectedSource = source
        let selectedColumns = columnCount
        let result = await Task.detached(priority: .userInitiated) {
            SpikeTrainModeHeatmapBuilder.make(
                trains: trains,
                automaticSegments: automatic,
                manualSegments: manual,
                source: selectedSource,
                timeMode: selectedMode,
                columnCount: selectedColumns
            )
        }.value
        guard !Task.isCancelled else { return }
        heatmap = result
        renderedImage = ModeHeatmapImageRenderer.image(for: result)
        isRebuilding = false
    }
}

private struct ModeHeatmapInputKey: Hashable {
    let datasetID: UUID?
    let trainIDs: [String]
    let automaticSignature: String
    let manualSignature: String
    let timeMode: RasterTimeMode
    let source: SpikeTrainModeHeatmapSource
    let columnCount: Int
}

private enum ModeHeatmapImageRenderer {
    static func image(for heatmap: SpikeTrainModeHeatmap) -> NSImage? {
        guard heatmap.rowCount > 0,
              heatmap.columnCount > 0 else { return nil }
        let width = heatmap.columnCount
        let height = heatmap.rowCount * 3
        let expected = heatmap.rowCount * width
        guard heatmap.stateCells.count == expected,
              heatmap.eventCells.count == expected,
              heatmap.otherCells.count == expected else { return nil }

        var pixels = [UInt8](repeating: 0, count: width * height * 4)
        for outputTrainRow in 0..<heatmap.rowCount {
            let sourceTrainRow = heatmap.rowCount - 1 - outputTrainRow
            let planes = [heatmap.stateCells, heatmap.eventCells, heatmap.otherCells]
            for (track, plane) in planes.enumerated() {
                for column in 0..<width {
                    let code = plane[sourceTrainRow * width + column]
                    guard let label = SpikeTrainModeHeatmap.label(for: code) else { continue }
                    let color = ModeHeatmapPalette.components(for: label)
                    let pixel = ((outputTrainRow * 3 + track) * width + column) * 4
                    pixels[pixel] = UInt8((color.red * 255).rounded())
                    pixels[pixel + 1] = UInt8((color.green * 255).rounded())
                    pixels[pixel + 2] = UInt8((color.blue * 255).rounded())
                    pixels[pixel + 3] = 255
                }
            }
        }

        let provider = CGDataProvider(data: Data(pixels) as CFData)
        guard let provider,
              let cgImage = CGImage(
                width: width,
                height: height,
                bitsPerComponent: 8,
                bitsPerPixel: 32,
                bytesPerRow: width * 4,
                space: CGColorSpaceCreateDeviceRGB(),
                bitmapInfo: CGBitmapInfo(rawValue: CGImageAlphaInfo.last.rawValue),
                provider: provider,
                decode: nil,
                shouldInterpolate: false,
                intent: .defaultIntent
              ) else { return nil }
        return NSImage(cgImage: cgImage, size: NSSize(width: width, height: height))
    }
}

private enum ModeHeatmapPalette {
    static func color(for label: ManualAnnotationLabel) -> Color {
        Color(components(for: label))
    }

    static func components(for label: ManualAnnotationLabel) -> ModeHeatmapColor {
        switch label {
        case .tonic: .init(red: 0.18, green: 0.60, blue: 0.34)
        case .highFrequencyTonic: .init(red: 0.12, green: 0.56, blue: 0.61)
        case .highFrequencySpiking: .init(red: 0.51, green: 0.32, blue: 0.71)
        case .burst: .init(red: 0.92, green: 0.49, blue: 0.14)
        case .highFrequencyBurst: .init(red: 0.86, green: 0.20, blue: 0.22)
        case .longBurst: .init(red: 0.77, green: 0.34, blue: 0.56)
        case .pause: .init(red: 0.20, green: 0.48, blue: 0.80)
        case .other: .init(red: 0.42, green: 0.45, blue: 0.50)
        case .notBurst: .init(red: 0.45, green: 0.45, blue: 0.45)
        }
    }
}

private struct ModeHeatmapColor {
    let red: Double
    let green: Double
    let blue: Double
}

private extension Color {
    init(_ color: ModeHeatmapColor) {
        self.init(red: color.red, green: color.green, blue: color.blue)
    }
}

/// Minimal wrapping layout for a compact categorical legend.
private struct FlowLayout: Layout {
    var spacing: CGFloat

    func sizeThatFits(proposal: ProposedViewSize, subviews: Subviews, cache: inout ()) -> CGSize {
        let maxWidth = proposal.width ?? .greatestFiniteMagnitude
        var cursorX: CGFloat = 0
        var cursorY: CGFloat = 0
        var rowHeight: CGFloat = 0
        for subview in subviews {
            let size = subview.sizeThatFits(.unspecified)
            if cursorX > 0, cursorX + size.width > maxWidth {
                cursorX = 0
                cursorY += rowHeight + spacing
                rowHeight = 0
            }
            cursorX += size.width + (cursorX == 0 ? 0 : spacing)
            rowHeight = max(rowHeight, size.height)
        }
        return CGSize(width: proposal.width ?? cursorX, height: cursorY + rowHeight)
    }

    func placeSubviews(in bounds: CGRect, proposal: ProposedViewSize, subviews: Subviews, cache: inout ()) {
        var cursorX = bounds.minX
        var cursorY = bounds.minY
        var rowHeight: CGFloat = 0
        for subview in subviews {
            let size = subview.sizeThatFits(.unspecified)
            if cursorX > bounds.minX, cursorX + size.width > bounds.maxX {
                cursorX = bounds.minX
                cursorY += rowHeight + spacing
                rowHeight = 0
            }
            subview.place(at: CGPoint(x: cursorX, y: cursorY), proposal: ProposedViewSize(size))
            cursorX += size.width + spacing
            rowHeight = max(rowHeight, size.height)
        }
    }
}
