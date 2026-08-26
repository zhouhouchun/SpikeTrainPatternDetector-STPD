import AppKit
import STPDCore
import SwiftUI

private enum ISIHeatmapPresentationPreset: String, CaseIterable, Identifiable {
    case exploration
    case publication

    var id: String { rawValue }

    var title: String {
        switch self {
        case .exploration: "探索"
        case .publication: "发表"
        }
    }

    var scaleMode: SpikeTrainISIHeatmapScaleMode {
        switch self {
        case .exploration: .explorationRobust
        case .publication: .publicationDatasetFixed
        }
    }

    var palette: ISIHeatmapPalette {
        switch self {
        case .exploration: .jet
        case .publication: .cividis
        }
    }
}

struct SpikeTrainISIHeatmapView: View {
    @Bindable var document: RasterDocument
    @Environment(\.l10n) private var l10n
    @State private var timeMode: RasterTimeMode = .aligned
    @State private var columnCount = 256
    @State private var presentationPreset: ISIHeatmapPresentationPreset = .exploration
    @State private var heatmap = SpikeTrainISIHeatmap.empty
    @State private var renderedImage: NSImage?
    @State private var isRebuilding = false

    private var visibleTrains: [SpikeTrain] {
        guard let dataset = document.dataset else { return [] }
        let selectedIDs = document.selectedTrainIDsIncludingFocusedCandidate(for: .raster)
        return dataset.trains.filter { selectedIDs.contains($0.id) }
    }

    private var inputKey: HeatmapInputKey {
        HeatmapInputKey(
            datasetID: document.dataset?.id,
            trainIDs: visibleTrains.map(\.id),
            timeMode: timeMode,
            columnCount: columnCount,
            presentationPreset: presentationPreset
        )
    }

    var body: some View {
        VStack(alignment: .leading, spacing: 0) {
            header
            Divider()
            heatmapContent
        }
        .frame(minWidth: 640)
        .delayedBackgroundProgress(
            isRebuilding ? l10n.t("正在生成 ISI 热力图…") : nil,
            alignment: .center
        )
        .task(id: inputKey) {
            await rebuildHeatmap(for: visibleTrains)
        }
    }

    private var header: some View {
        VStack(alignment: .leading, spacing: 9) {
            HStack(alignment: .firstTextBaseline) {
                Text(l10n.t("ISI 热力图"))
                    .font(.headline)
                Spacer()
                Text("\(visibleTrains.count) \(l10n.t("条"))")
                    .font(.caption.weight(.semibold))
                    .foregroundStyle(.secondary)
            }

            ViewThatFits(in: .horizontal) {
                HStack(spacing: 18) {
                    heatmapTimeControl
                    heatmapResolutionControl
                    heatmapPresetControl
                    heatmapPresetDescription
                    Spacer(minLength: 0)
                }

                VStack(alignment: .leading, spacing: 8) {
                    HStack(spacing: 18) {
                        heatmapTimeControl
                        heatmapResolutionControl
                        heatmapPresetControl
                        Spacer(minLength: 0)
                    }
                    heatmapPresetDescription
                }

                VStack(alignment: .leading, spacing: 8) {
                    heatmapTimeControl
                    heatmapResolutionControl
                    heatmapPresetControl
                    heatmapPresetDescription
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

    private var heatmapPresetControl: some View {
        HStack(spacing: 8) {
            Text(l10n.t("预设"))
                .foregroundStyle(.secondary)
                .lineLimit(1)
                .fixedSize(horizontal: true, vertical: false)
            GlassSegmentedControl(
                options: ISIHeatmapPresentationPreset.allCases.map { ($0, l10n.t($0.title)) },
                selection: $presentationPreset,
                minSegmentWidth: l10n.language == .ru ? 84 : 58
            )
            .frame(width: l10n.language == .ru ? 208 : 150)
        }
    }

    private var heatmapPresetDescription: some View {
        Text(presetDescription)
            .font(.caption)
            .foregroundStyle(.secondary)
            .lineLimit(l10n.language == .ru ? 2 : 1)
            .fixedSize(horizontal: false, vertical: true)
    }

    @ViewBuilder
    private var heatmapContent: some View {
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
                let yAxisWidth: CGFloat = 169
                let availablePlotHeight = max(proxy.size.height - 68, 1)
                let plotHeight = preferredPlotHeight(available: availablePlotHeight)
                let viewportWidth = max(proxy.size.width - yAxisWidth - 8, 1)
                let contentWidth = heatmapContentWidth(forViewportWidth: viewportWidth)

                VStack(alignment: .leading, spacing: 10) {
                    HStack(alignment: .top, spacing: 8) {
                        heatmapYAxis
                            .frame(width: yAxisWidth, height: plotHeight)

                        ScrollView(.horizontal) {
                            VStack(alignment: .leading, spacing: 6) {
                                heatmapPlot
                                    .frame(width: contentWidth, height: plotHeight)
                                heatmapXAxis
                                    .frame(width: contentWidth)
                            }
                        }
                        .scrollIndicators(.visible)
                        .accessibilityLabel(l10n.t("ISI 热力图"))
                    }

                    legend
                    Spacer(minLength: 0)
                }
                .padding(.horizontal, 16)
                .padding(.vertical, 14)
            }
        }
    }

    private var heatmapPlot: some View {
        ZStack {
            Color(red: 0, green: 0, blue: 0.34)
            if let renderedImage {
                Image(nsImage: renderedImage)
                    .resizable()
                    // This is a time-by-train data grid, not an image with a natural display
                    // aspect ratio. Stretch it onto the plot axes so every time column remains
                    // visible; preserving 256×N aspect ratio would crop raw-time intervals.
                    .interpolation(presentationPreset == .publication ? .none : .high)
                    .clipped()
            }
        }
        .clipShape(RoundedRectangle(cornerRadius: 8, style: .continuous))
        .overlay {
            RoundedRectangle(cornerRadius: 8, style: .continuous)
                .stroke(.separator.opacity(0.55), lineWidth: 1)
        }
    }

    private var heatmapYAxis: some View {
        HStack(spacing: 5) {
            Text(l10n.t("Spike train"))
                .font(.caption2.weight(.semibold))
                .foregroundStyle(.secondary)
                .fixedSize()
                .rotationEffect(.degrees(-90))
                .frame(width: 13)

            GeometryReader { proxy in
                VStack(spacing: 0) {
                    // The raster is plotted with origin=lower; reverse the labels so the first
                    // selected train remains the bottom row in both the raster and heatmap.
                    ForEach(Array(visibleTrains.reversed().enumerated()), id: \.element.id) { _, train in
                        Text(train.name)
                            .font(.caption2.monospacedDigit())
                            .foregroundStyle(.secondary)
                            .lineLimit(1)
                            .truncationMode(.middle)
                            .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .trailing)
                    }
                }
                .frame(height: proxy.size.height)
            }
            .frame(width: 148)
        }
    }

    private var heatmapXAxis: some View {
        VStack(alignment: .leading, spacing: 3) {
            HStack(spacing: 0) {
                ForEach(Array(heatmapTimeTicks.enumerated()), id: \.offset) { index, tick in
                    Text(TimeFormatting.seconds(tick))
                        .font(.caption2.monospacedDigit())
                        .foregroundStyle(.secondary)
                        .frame(
                            maxWidth: index == 0 || index == heatmapTimeTicks.count - 1 ? nil : .infinity,
                            alignment: index == 0 ? .leading : index == heatmapTimeTicks.count - 1 ? .trailing : .center
                        )
                }
            }
            Text(l10n.t(timeMode.axisTitle))
                .font(.caption2.weight(.semibold))
                .foregroundStyle(.secondary)
        }
    }

    private func heatmapContentWidth(forViewportWidth viewportWidth: CGFloat) -> CGFloat {
        // One calculated time bin is kept wide enough for the user to inspect it by horizontal
        // scrolling. It is a display layout choice only; it does not change the ISI grid.
        max(viewportWidth, CGFloat(heatmap.columnCount) * 2.5)
    }

    private func preferredPlotHeight(available: CGFloat) -> CGFloat {
        let rowCount = max(visibleTrains.count, 1)
        let preferredLaneHeight: CGFloat = 72
        // Do not stretch sparse selections merely to fill the inspector. Only compress lanes when
        // the visible train count cannot fit at the preferred height.
        let laneHeight = min(preferredLaneHeight, available / CGFloat(rowCount))
        return max(laneHeight * CGFloat(rowCount), 1)
    }

    private var heatmapTimeTicks: [Double] {
        guard heatmap.upperTimeSec > heatmap.lowerTimeSec else { return [] }
        let duration = heatmap.upperTimeSec - heatmap.lowerTimeSec
        return (0...4).map { heatmap.lowerTimeSec + duration * Double($0) / 4 }
    }

    private var legend: some View {
        VStack(alignment: .leading, spacing: 5) {
            HStack(spacing: 10) {
                Text(l10n.t("颜色范围"))
                    .font(.caption.weight(.semibold))
                Text("log₁₀(Hz)")
                    .font(.caption.monospacedDigit())
                    .foregroundStyle(.secondary)
                Spacer(minLength: 0)
                Text(scaleDescription)
                    .font(.caption)
                    .foregroundStyle(.secondary)
            }

            LinearGradient(
                colors: presentationPreset.palette.legendColors.map(Color.init),
                startPoint: .leading,
                endPoint: .trailing
            )
            .frame(height: 12)
            .clipShape(Capsule())

            if let lower = heatmap.lowerLogHz, let upper = heatmap.upperLogHz {
                HStack {
                    Text(String(format: "%.2f", lower))
                    Spacer()
                    Text(String(format: "%.2f", (lower + upper) / 2))
                    Spacer()
                    Text(String(format: "%.2f", upper))
                }
                .font(.caption2.monospacedDigit())
                .foregroundStyle(.secondary)

                if presentationPreset == .exploration,
                   heatmap.lowClipCount + heatmap.highClipCount > 0 {
                    Text("\(l10n.t("裁剪")) \(heatmap.lowClipCount + heatmap.highClipCount)/\(heatmap.scaleObservationCount) \(l10n.t("个时间格"))")
                        .font(.caption2)
                        .foregroundStyle(.secondary)
                }
            }
        }
    }

    private var presetDescription: String {
        switch presentationPreset {
        case .exploration:
            l10n.t("探索：Jet 色图；当前视图稳健 5–95% 范围。")
        case .publication:
            l10n.t("发表：Cividis 色图；全数据集固定数值范围。")
        }
    }

    private var scaleDescription: String {
        switch heatmap.scaleMode {
        case .explorationRobust:
            l10n.t("当前视图 5–95% 稳健范围")
        case .publicationDatasetFixed:
            l10n.t("全数据集固定范围")
        }
    }

    private func rebuildHeatmap(for trains: [SpikeTrain]) async {
        isRebuilding = true
        let selectedMode = timeMode
        let selectedColumns = columnCount
        let selectedPreset = presentationPreset
        let scaleTrains = document.dataset?.trains ?? trains
        let result = await Task.detached(priority: .userInitiated) {
            SpikeTrainISIHeatmapBuilder.make(
                trains: trains,
                timeMode: selectedMode,
                columnCount: selectedColumns,
                scaleTrains: scaleTrains,
                scaleMode: selectedPreset.scaleMode
            )
        }.value
        guard !Task.isCancelled else { return }
        heatmap = result
        renderedImage = ISIHeatmapImageRenderer.image(for: result, palette: selectedPreset.palette)
        isRebuilding = false
    }
}

private struct HeatmapInputKey: Hashable {
    let datasetID: UUID?
    let trainIDs: [String]
    let timeMode: RasterTimeMode
    let columnCount: Int
    let presentationPreset: ISIHeatmapPresentationPreset
}

private enum ISIHeatmapImageRenderer {
    static func image(for heatmap: SpikeTrainISIHeatmap, palette: ISIHeatmapPalette) -> NSImage? {
        guard heatmap.rowCount > 0,
              heatmap.columnCount > 0,
              heatmap.supportedCells.count == heatmap.rowCount * heatmap.columnCount,
              heatmap.normalizedValues.count == heatmap.rowCount * heatmap.columnCount else {
            return nil
        }

        let width = heatmap.columnCount
        let height = heatmap.rowCount
        var pixels = [UInt8](repeating: 0, count: width * height * 4)
        for outputRow in 0..<height {
            // `origin="lower"`: the first selected train occupies the bottom heatmap row.
            let sourceRow = height - 1 - outputRow
            for column in 0..<width {
                let sourceCell = sourceRow * width + column
                let value = Double(heatmap.normalizedValues[sourceCell])
                let color = palette.color(value)
                let pixel = (outputRow * width + column) * 4
                pixels[pixel] = UInt8((color.red * 255).rounded())
                pixels[pixel + 1] = UInt8((color.green * 255).rounded())
                pixels[pixel + 2] = UInt8((color.blue * 255).rounded())
                pixels[pixel + 3] = heatmap.supportedCells[sourceCell] ? 255 : 0
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
                shouldInterpolate: true,
                intent: .defaultIntent
              ) else {
            return nil
        }
        return NSImage(cgImage: cgImage, size: NSSize(width: width, height: height))
    }
}

private enum ISIHeatmapPalette {
    case jet
    case cividis

    var legendColors: [ColorComponents] {
        stride(from: 0.0, through: 1.0, by: 0.125).map(color)
    }

    func color(_ value: Double) -> ColorComponents {
        let x = min(max(value, 0), 1)
        switch self {
        case .jet:
            return ColorComponents(
                red: channel(1.5 - abs(4 * x - 3)),
                green: channel(1.5 - abs(4 * x - 2)),
                blue: channel(1.5 - abs(4 * x - 1))
            )
        case .cividis:
            return interpolatedCividisColor(at: x)
        }
    }

    private func interpolatedCividisColor(at value: Double) -> ColorComponents {
        // Canonical Cividis samples (every 16 entries of the 256-entry Matplotlib table),
        // linearly interpolated for display. It is a sequential, colour-vision-deficiency-
        // friendly palette with monotonically increasing lightness.
        let stops: [(Double, ColorComponents)] = [
            (0.0000, .init(red: 0.000000, green: 0.135112, blue: 0.304751)),
            (0.0627, .init(red: 0.000000, green: 0.178802, blue: 0.414764)),
            (0.1255, .init(red: 0.103401, green: 0.220406, blue: 0.435790)),
            (0.1882, .init(red: 0.195057, green: 0.264372, blue: 0.425924)),
            (0.2510, .init(red: 0.263738, green: 0.307831, blue: 0.422789)),
            (0.3137, .init(red: 0.324250, green: 0.351289, blue: 0.426250)),
            (0.3765, .init(red: 0.380830, green: 0.395164, blue: 0.435653)),
            (0.4392, .init(red: 0.435168, green: 0.439763, blue: 0.451134)),
            (0.5020, .init(red: 0.488697, green: 0.485318, blue: 0.471008)),
            (0.5647, .init(red: 0.547840, green: 0.531895, blue: 0.471704)),
            (0.6275, .init(red: 0.609105, green: 0.579816, blue: 0.463638)),
            (0.6902, .init(red: 0.671991, green: 0.629316, blue: 0.448018)),
            (0.7529, .init(red: 0.736488, green: 0.680629, blue: 0.424028)),
            (0.8157, .init(red: 0.802667, green: 0.733978, blue: 0.390153)),
            (0.8784, .init(red: 0.870717, green: 0.789572, blue: 0.343333)),
            (0.9412, .init(red: 0.941147, green: 0.847530, blue: 0.275815)),
            (1.0000, .init(red: 0.995737, green: 0.909344, blue: 0.217772))
        ]
        guard let upperIndex = stops.firstIndex(where: { $0.0 >= value }) else {
            return stops[stops.count - 1].1
        }
        guard upperIndex > 0 else { return stops[upperIndex].1 }
        let lower = stops[upperIndex - 1]
        let upper = stops[upperIndex]
        let fraction = (value - lower.0) / (upper.0 - lower.0)
        return ColorComponents(
            red: lower.1.red + (upper.1.red - lower.1.red) * fraction,
            green: lower.1.green + (upper.1.green - lower.1.green) * fraction,
            blue: lower.1.blue + (upper.1.blue - lower.1.blue) * fraction
        )
    }

    private func channel(_ value: Double) -> Double { min(max(value, 0), 1) }
}

private struct ColorComponents {
    let red: Double
    let green: Double
    let blue: Double
}

private extension Color {
    init(_ components: ColorComponents) {
        self.init(red: components.red, green: components.green, blue: components.blue)
    }
}
