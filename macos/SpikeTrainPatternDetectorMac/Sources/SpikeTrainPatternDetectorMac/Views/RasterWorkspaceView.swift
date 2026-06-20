import STPDCore
import SwiftUI

struct RasterWorkspaceView: View {
    @Bindable var document: RasterDocument
    @State private var timeMode: RasterTimeMode
    @State private var maxSpikeTickHeightPx = 50

    init(document: RasterDocument, initialTimeMode: RasterTimeMode = .aligned) {
        self.document = document
        _timeMode = State(initialValue: initialTimeMode)
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
                    focusRequestID: document.classicAnchorFocusRequestID
                )
            } else {
                ContentUnavailableView(
                    "Raster Not Loaded",
                    systemImage: "chart.xyaxis.line",
                    description: Text(document.lastErrorMessage ?? "No spike trains are available yet.")
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
}

private struct HeaderView: View {
    @Bindable var document: RasterDocument
    @Binding var timeMode: RasterTimeMode
    @Binding var visibleWindowSeconds: Double
    @Binding var visibleWindowUnit: QualityDisplayUnit
    @Binding var spikeTickHeightPx: Int
    let maxSpikeTickHeightPx: Int
    @State private var headerWidth: CGFloat = 900
    private let rightLabelWidth: CGFloat = 74

    var body: some View {
        HStack(alignment: .top, spacing: 16) {
            VStack(alignment: .leading, spacing: 9) {
                titleBlock
                scaleControls
            }
            .layoutPriority(1)

            Spacer(minLength: 12)

            headerRightControls
                .frame(width: rightColumnWidth, alignment: .topLeading)
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
            Text("\(timeMode.title) Spike Raster")
                .font(.headline)
            Text(document.statusMessage)
                .font(.subheadline)
                .foregroundStyle(document.lastErrorMessage == nil ? Color.secondary : Color.red)
                .lineLimit(2)
        }
    }

    private var headerRightControls: some View {
        HStack(alignment: .center, spacing: 12) {
            VStack(alignment: .leading, spacing: 8) {
                timeModeControl
                datasetSummary
            }
            .frame(width: timeDatasetBlockWidth, alignment: .leading)

            rasterTrainDisplayBlock
        }
    }

    private var timeModeControl: some View {
        HStack(spacing: 8) {
            Text("Time")
                .foregroundStyle(.secondary)
                .frame(width: rightLabelWidth, alignment: .leading)
            GlassSegmentedControl(
                options: RasterTimeMode.allCases.map { ($0, $0.title) },
                selection: $timeMode,
                minSegmentWidth: 58
            )
            .frame(width: 180)
        }
    }

    @ViewBuilder
    private var rasterTrainDisplayBlock: some View {
        if let dataset = document.dataset {
            VStack(alignment: .leading, spacing: 7) {
                Text(SpikeTrainSelectionScope.raster.title)
                    .font(.caption.weight(.semibold))
                    .foregroundStyle(.secondary)

                SpikeTrainCountControls(
                    document: document,
                    dataset: dataset,
                    scope: .raster,
                    showsTitle: false
                )
            }
        }
    }

    private var scaleControls: some View {
        ViewThatFits(in: .horizontal) {
            scaleControlsRow
            compactScaleControls
        }
    }

    private var scaleControlsRow: some View {
        HStack(alignment: .top, spacing: 18) {
            Image(systemName: "magnifyingglass")
                .foregroundStyle(.secondary)
                .padding(.top, 6)

            rasterScaleControl(
                title: "Visible window",
                value: secondsBinding(
                    unit: visibleWindowUnit,
                    minimumSeconds: 0.001,
                    getSeconds: { visibleWindowSeconds },
                    setSeconds: { visibleWindowSeconds = $0; document.rasterVisibleWindowUserLocked = true }
                ),
                unit: $visibleWindowUnit,
                fieldWidth: 86,
                help: "Requested time span in the visible raster viewport. The estimated ISI resolution is shown beside this control."
            )

            spikeHeightControl

            rasterResolutionReadout

            resetZoomButton
        }
    }

    private var compactScaleControls: some View {
        VStack(alignment: .leading, spacing: 7) {
            HStack(alignment: .top, spacing: 14) {
                Image(systemName: "magnifyingglass")
                    .foregroundStyle(.secondary)
                    .padding(.top, 6)

                rasterScaleControl(
                    title: "Visible window",
                    value: secondsBinding(
                        unit: visibleWindowUnit,
                        minimumSeconds: 0.001,
                        getSeconds: { visibleWindowSeconds },
                        setSeconds: { visibleWindowSeconds = $0; document.rasterVisibleWindowUserLocked = true }
                    ),
                    unit: $visibleWindowUnit,
                    fieldWidth: 86,
                    help: "Requested time span in the visible raster viewport. The estimated ISI resolution is shown beside this control."
                )

                resetZoomButton
            }

            HStack(alignment: .top, spacing: 14) {
                Color.clear
                    .frame(width: 18, height: 1)

                spikeHeightControl

                rasterResolutionReadout
            }
        }
    }

    private var rasterResolutionReadout: some View {
        VStack(alignment: .leading, spacing: 2) {
            Text(rasterResolutionLine)
            if let rasterVisibleLine {
                Text(rasterVisibleLine)
            }
        }
        .font(.caption)
        .foregroundStyle(.secondary)
        .lineLimit(1)
        .fixedSize(horizontal: true, vertical: false)
    }

    private var resetZoomButton: some View {
        Button {
            document.resetRasterVisibleWindowToStandard()
        } label: {
            Label("Reset Zoom", systemImage: "arrow.counterclockwise")
        }
        .liquidGlassButtonStyle()
        .labelStyle(.iconOnly)
        .help("Reset zoom")
        .padding(.top, 1)
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
    }

    private var spikeHeightControl: some View {
        HStack(spacing: 8) {
            Text("Spike height")
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
            Text("max \(safeMaxSpikeTickHeightPx) px")
                .font(.caption)
                .foregroundStyle(.secondary)
                .lineLimit(1)
                .fixedSize(horizontal: true, vertical: false)
        }
        .help("Controls the vertical height of each spike tick in pixels. The maximum follows the current lane height.")
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
        return "ISI resolution ~ \(TimeFormatting.seconds(metrics.isiResolution))"
    }

    private var rasterVisibleLine: String? {
        let metrics = estimatedRasterMetrics()
        var line: String?
        if let visibleDuration = metrics.visibleDuration {
            line = "Visible \(TimeFormatting.seconds(visibleDuration))"
        }
        if metrics.compressedByWidthCap {
            line = [line, "LOD cap"].compactMap(\.self).joined(separator: " · ")
        }
        return line
    }

    private var rightColumnWidth: CGFloat {
        min(max(headerWidth * 0.44, 590), 760)
    }

    private var timeDatasetBlockWidth: CGFloat {
        min(max(rightColumnWidth * 0.50, 330), 400)
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

    var body: some View {
        HStack(alignment: .top, spacing: 8) {
            Text("Dataset")
                .font(.headline.weight(.semibold))
                .foregroundStyle(.primary)
                .frame(width: labelWidth, alignment: .leading)
                .padding(.top, 1)

            VStack(alignment: .leading, spacing: 3) {
                ViewThatFits(in: .horizontal) {
                    HStack(spacing: 10) {
                        metric("Trains", "\(dataset.trains.count)")
                        metric("Spikes", dataset.totalSpikeCount.formatted())
                        metric("Aligned", TimeFormatting.seconds(dataset.maxAlignedDurationSec))
                        metric("Raw", TimeFormatting.seconds(dataset.rawDurationSec))
                        metric("Visible", "\(visibleTrainCount)")
                    }

                    HStack(spacing: 10) {
                        metric("Trains", "\(dataset.trains.count)")
                        metric("Spikes", dataset.totalSpikeCount.formatted())
                        metric("Visible", "\(visibleTrainCount)")
                    }
                }
                .font(.caption)
                .monospacedDigit()

                ViewThatFits(in: .horizontal) {
                    HStack(spacing: 10) {
                        metric("Aligned", TimeFormatting.seconds(dataset.maxAlignedDurationSec))
                        metric("Raw", TimeFormatting.seconds(dataset.rawDurationSec))
                        sourceText
                    }

                    HStack(spacing: 10) {
                        metric("Aligned", TimeFormatting.seconds(dataset.maxAlignedDurationSec))
                        metric("Raw", TimeFormatting.seconds(dataset.rawDurationSec))
                    }
                }
                .font(.caption)
                .monospacedDigit()
            }
        }
        .frame(maxWidth: .infinity, alignment: .leading)
    }

    private var sourceText: some View {
        Text("Source \(sourceDisplayName)")
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

private struct HeaderWidthPreferenceKey: PreferenceKey {
    static let defaultValue: CGFloat = 900

    static func reduce(value: inout CGFloat, nextValue: () -> CGFloat) {
        value = nextValue()
    }
}
