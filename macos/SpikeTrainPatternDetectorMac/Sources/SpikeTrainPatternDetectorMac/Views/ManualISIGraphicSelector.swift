import AppKit
import STPDCore
import SwiftUI

struct ManualISIGraphicRow: Identifiable, Hashable {
    let id: String
    let isiIndex: Int
    let leftSeconds: Double
    let rightSeconds: Double
    let stateLabel: ManualAnnotationLabel?
    let eventLabel: ManualAnnotationLabel?
    let otherLabel: ManualAnnotationLabel?
    let automaticLabels: [ClassicAnchorLabel]
    let isInvalidISI: Bool

    init(
        id: String,
        isiIndex: Int,
        leftSeconds: Double,
        rightSeconds: Double,
        stateLabel: ManualAnnotationLabel?,
        eventLabel: ManualAnnotationLabel?,
        otherLabel: ManualAnnotationLabel?,
        automaticLabels: [ClassicAnchorLabel] = [],
        isInvalidISI: Bool = false
    ) {
        self.id = id
        self.isiIndex = isiIndex
        self.leftSeconds = leftSeconds
        self.rightSeconds = rightSeconds
        self.stateLabel = stateLabel
        self.eventLabel = eventLabel
        self.otherLabel = otherLabel
        self.automaticLabels = automaticLabels
        self.isInvalidISI = isInvalidISI
    }
}

enum ManualISIGraphicSelection {
    static func rowIndex(at time: Double, in rows: [ManualISIGraphicRow]) -> Int? {
        guard !rows.isEmpty, time.isFinite else { return nil }
        if time <= rows[0].rightSeconds { return 0 }
        if time >= rows[rows.count - 1].leftSeconds { return rows.count - 1 }

        var lower = 0
        var upper = rows.count - 1
        while lower < upper {
            let middle = lower + (upper - lower) / 2
            if rows[middle].rightSeconds < time {
                lower = middle + 1
            } else {
                upper = middle
            }
        }
        return lower
    }

    static func selectedIDs(
        in rows: [ManualISIGraphicRow],
        from startTime: Double,
        through endTime: Double
    ) -> Set<String> {
        guard let start = rowIndex(at: startTime, in: rows),
              let end = rowIndex(at: endTime, in: rows) else { return [] }
        return Set(rows[min(start, end)...max(start, end)].map(\.id))
    }

    /// Consecutive selections share one glass surface. This keeps rendering bounded by the
    /// number of disjoint selections rather than the number of selected ISIs.
    static func selectedIndexRanges(
        in rows: [ManualISIGraphicRow],
        selectedRowIDs: Set<String>
    ) -> [ClosedRange<Int>] {
        var ranges: [ClosedRange<Int>] = []
        var rangeStart: Int?
        var previousIndex: Int?

        for (index, row) in rows.enumerated() where selectedRowIDs.contains(row.id) {
            if let previousIndex, index != previousIndex + 1 {
                if let rangeStart { ranges.append(rangeStart...previousIndex) }
                rangeStart = index
            } else if rangeStart == nil {
                rangeStart = index
            }
            previousIndex = index
        }

        if let rangeStart, let previousIndex {
            ranges.append(rangeStart...previousIndex)
        }
        return ranges
    }
}

enum ManualISIGraphicQCGeometry {
    static func involvedSpikeTimes(in rows: [ManualISIGraphicRow]) -> Set<Double> {
        var result: Set<Double> = []
        for row in rows where row.isInvalidISI {
            result.insert(row.leftSeconds)
            result.insert(row.rightSeconds)
        }
        return result
    }
}

enum ManualISIGraphicPatternLane: Hashable {
    case state
    case event
    case other
}

struct ManualISIGraphicPatternRun: Hashable {
    let lane: ManualISIGraphicPatternLane
    let label: ManualAnnotationLabel
    let rowRange: ClosedRange<Int>

    func rowIDs(in rows: [ManualISIGraphicRow]) -> Set<String> {
        guard rows.indices.contains(rowRange.lowerBound),
              rows.indices.contains(rowRange.upperBound) else { return [] }
        return Set(rows[rowRange].map(\.id))
    }

    func isiIndices(in rows: [ManualISIGraphicRow]) -> Set<Int> {
        guard rows.indices.contains(rowRange.lowerBound),
              rows.indices.contains(rowRange.upperBound) else { return [] }
        return Set(rows[rowRange].map(\.isiIndex))
    }
}

/// Coalesces adjacent equal labels before SwiftUI creates glass surfaces. Rendering cost therefore
/// follows the number of visible pattern runs rather than the number of labeled ISIs.
enum ManualISIGraphicPatternGlass {
    static func runs(in rows: [ManualISIGraphicRow]) -> [ManualISIGraphicPatternRun] {
        laneRuns(in: rows, lane: .state, label: \ManualISIGraphicRow.stateLabel)
            + laneRuns(in: rows, lane: .event, label: \ManualISIGraphicRow.eventLabel)
            + laneRuns(in: rows, lane: .other, label: \ManualISIGraphicRow.otherLabel)
    }

    private static func laneRuns(
        in rows: [ManualISIGraphicRow],
        lane: ManualISIGraphicPatternLane,
        label: KeyPath<ManualISIGraphicRow, ManualAnnotationLabel?>
    ) -> [ManualISIGraphicPatternRun] {
        var result: [ManualISIGraphicPatternRun] = []
        var runStart: Int?
        var runLabel: ManualAnnotationLabel?

        for index in rows.indices {
            let nextLabel = rows[index][keyPath: label]
            guard nextLabel != runLabel else { continue }

            if let runStart, let runLabel {
                result.append(ManualISIGraphicPatternRun(
                    lane: lane,
                    label: runLabel,
                    rowRange: runStart...(index - 1)
                ))
            }
            runStart = nextLabel == nil ? nil : index
            runLabel = nextLabel
        }

        if let runStart, let runLabel, let finalIndex = rows.indices.last {
            result.append(ManualISIGraphicPatternRun(
                lane: lane,
                label: runLabel,
                rowRange: runStart...finalIndex
            ))
        }
        return result
    }
}

/// Detector-independent visual selection for manual ISI authoring. The horizontal geometry and
/// selection are derived only from source timestamps. Existing automatic labels may be shown as
/// read-only comparison text, but never affect selection or manual-label decisions.
struct ManualISIGraphicSelector: View {
    @Environment(\.l10n) private var l10n

    let rows: [ManualISIGraphicRow]
    @Binding var selectedRowIDs: Set<String>
    @Binding var showsISIInformationPanel: Bool
    let onFocusedRowIDChange: (String?) -> Void
    let onDeletePatternRun: (ManualISIGraphicPatternRun) -> Void

    @State private var zoom = 1.0
    @State private var dragAnchorTime: Double?
    @State private var dragFocusRowID: String?
    @State private var gesturePatternRun: ManualISIGraphicPatternRun?
    @State private var focusedPatternRun: ManualISIGraphicPatternRun?
    @State private var deleteKeyFocusRequestID: UInt64 = 0
    @State private var hoveredRowID: String?
    @State private var hoverLocation: CGPoint?
    @FocusState private var plotHasKeyboardFocus: Bool

    private let plotHeight: CGFloat = 184
    private let horizontalPadding: CGFloat = 12

    init(
        rows: [ManualISIGraphicRow],
        selectedRowIDs: Binding<Set<String>>,
        showsISIInformationPanel: Binding<Bool> = .constant(true),
        onFocusedRowIDChange: @escaping (String?) -> Void = { _ in },
        onDeletePatternRun: @escaping (ManualISIGraphicPatternRun) -> Void = { _ in }
    ) {
        self.rows = rows
        _selectedRowIDs = selectedRowIDs
        _showsISIInformationPanel = showsISIInformationPanel
        self.onFocusedRowIDChange = onFocusedRowIDChange
        self.onDeletePatternRun = onDeletePatternRun
    }

    var body: some View {
        VStack(alignment: .leading, spacing: 8) {
            HStack(spacing: 12) {
                Label(l10n.t("时间图手工选择"), systemImage: "hand.draw")
                    .font(.headline)
                Text(l10n.t("拖动选择连续 ISI；点击已标记色块后按 Delete 可清除该区块。"))
                    .font(.callout)
                    .foregroundStyle(.secondary)
                Spacer()
                Toggle(l10n.t("显示 ISI 信息栏"), isOn: $showsISIInformationPanel)
                    .toggleStyle(.switch)
                    .controlSize(.small)
                    .help(l10n.t("鼠标悬停在相邻 spike 之间时显示该 ISI 的时间戳、间隔与模式信息"))
                Text(l10n.t("缩放"))
                    .font(.caption)
                    .foregroundStyle(.secondary)
                Slider(value: $zoom, in: 1...24)
                    .frame(width: 150)
                Button(l10n.t("适合窗口")) { zoom = 1 }
                    .disabled(zoom == 1)
            }

            GeometryReader { geometry in
                let baseWidth = max(620, geometry.size.width - 58)
                let contentWidth = min(100_000, baseWidth * zoom)

                HStack(spacing: 8) {
                    laneLabels
                        .frame(width: 50, height: plotHeight)

                    ScrollView(.horizontal) {
                        plot(width: contentWidth)
                            .frame(width: contentWidth, height: plotHeight)
                    }
                    .scrollIndicators(.visible)
                }
            }
            .frame(height: plotHeight)
        }
        .padding(10)
        .background(.quaternary.opacity(0.32), in: RoundedRectangle(cornerRadius: 10))
    }

    private var laneLabels: some View {
        VStack(alignment: .trailing, spacing: 0) {
            Text(ManualAnnotationSemanticTrack.state.displayName(using: l10n)).frame(height: 34)
            Text(ManualAnnotationSemanticTrack.event.displayName(using: l10n)).frame(height: 34)
            Text(ManualAnnotationSemanticTrack.other.displayName(using: l10n)).frame(height: 28)
            Text(l10n.t("Spike")).frame(height: 58)
            Spacer(minLength: 0)
        }
        .font(.caption)
        .foregroundStyle(.secondary)
        .frame(maxWidth: .infinity, alignment: .trailing)
    }

    private func plot(width: CGFloat) -> some View {
        ZStack(alignment: .topLeading) {
            // Every glass surface is deliberately behind the scientific Canvas. Grid lines and
            // especially spike ticks are painted afterwards at their original geometry, so
            // glass optics can never thin, shift, or duplicate them.
            patternGlassOverlay(width: width)
                .allowsHitTesting(false)

            selectionGlassOverlay(width: width)
                .allowsHitTesting(false)

            Canvas { context, size in
                guard let first = rows.first, let last = rows.last else { return }
                let origin = first.leftSeconds
                let span = max(last.rightSeconds - origin, 0.000_001)
                let usableWidth = max(1, size.width - horizontalPadding * 2)
                func x(_ seconds: Double) -> CGFloat {
                    horizontalPadding + CGFloat((seconds - origin) / span) * usableWidth
                }

                for y in [CGFloat(38), 72, 100, 158] {
                    var separator = Path()
                    separator.move(to: CGPoint(x: 0, y: y))
                    separator.addLine(to: CGPoint(x: size.width, y: y))
                    context.stroke(separator, with: .color(.secondary.opacity(0.16)), lineWidth: 1)
                }

                var spikes = Path()
                for row in rows {
                    let spikeX = x(row.leftSeconds)
                    spikes.move(to: CGPoint(x: spikeX, y: 107))
                    spikes.addLine(to: CGPoint(x: spikeX, y: 151))
                }
                let finalX = x(last.rightSeconds)
                spikes.move(to: CGPoint(x: finalX, y: 107))
                spikes.addLine(to: CGPoint(x: finalX, y: 151))
                context.stroke(spikes, with: .color(.primary.opacity(0.86)), lineWidth: 1.2)

                // Selection emphasis is carried by the scientific foreground, not by making
                // the glass more opaque. Every spike inside a selected ISI run is redrawn a
                // little thicker, including the left and right boundary spikes.
                let selectedRanges = ManualISIGraphicSelection.selectedIndexRanges(
                    in: rows,
                    selectedRowIDs: selectedRowIDs
                )
                if !selectedRanges.isEmpty {
                    var selectedSpikes = Path()
                    for range in selectedRanges {
                        for index in range {
                            let selectedX = x(rows[index].leftSeconds)
                            selectedSpikes.move(to: CGPoint(x: selectedX, y: 107))
                            selectedSpikes.addLine(to: CGPoint(x: selectedX, y: 151))
                        }
                        let rightBoundaryX = x(rows[range.upperBound].rightSeconds)
                        selectedSpikes.move(to: CGPoint(x: rightBoundaryX, y: 107))
                        selectedSpikes.addLine(to: CGPoint(x: rightBoundaryX, y: 151))
                    }
                    context.stroke(
                        selectedSpikes,
                        with: .color(.primary.opacity(0.98)),
                        lineWidth: 1.8
                    )
                }

                // An invalid ISI identifies a suspicious interval, not which endpoint spike is
                // erroneous. Draw the interval connector and both involved endpoint spikes red.
                let invalidRows = rows.filter(\.isInvalidISI)
                if !invalidRows.isEmpty {
                    var invalidIntervals = Path()
                    for row in invalidRows {
                        invalidIntervals.move(to: CGPoint(x: x(row.leftSeconds), y: 154))
                        invalidIntervals.addLine(to: CGPoint(x: x(row.rightSeconds), y: 154))
                    }
                    context.stroke(
                        invalidIntervals,
                        with: .color(.red.opacity(0.90)),
                        lineWidth: 1.6
                    )

                    var invalidSpikes = Path()
                    for time in ManualISIGraphicQCGeometry.involvedSpikeTimes(in: rows).sorted() {
                        let spikeX = x(time)
                        invalidSpikes.move(to: CGPoint(x: spikeX, y: 105))
                        invalidSpikes.addLine(to: CGPoint(x: spikeX, y: 153))
                    }
                    context.stroke(
                        invalidSpikes,
                        with: .color(.red),
                        lineWidth: 2.2
                    )
                }

                for tick in 0...4 {
                    let fraction = Double(tick) / 4
                    let tickX = horizontalPadding + CGFloat(fraction) * usableWidth
                    let elapsed = fraction * span
                    context.draw(
                        Text(formatElapsed(elapsed))
                            .font(.caption2.monospacedDigit())
                            .foregroundStyle(.secondary),
                        at: CGPoint(x: tickX, y: 171),
                        anchor: tick == 0 ? .leading : (tick == 4 ? .trailing : .center)
                    )
                }
            }

            if showsISIInformationPanel,
               let hoveredRowID,
               let row = rows.first(where: { $0.id == hoveredRowID }),
               let hoverLocation {
                ManualISIInformationCard(row: row)
                    .frame(width: 268)
                    .position(informationCardPosition(for: hoverLocation, plotWidth: width))
                    .allowsHitTesting(false)
            }
        }
        .background {
            ManualISIDeleteKeyCapture(
                isActive: focusedPatternRun != nil,
                focusRequestID: deleteKeyFocusRequestID,
                onDelete: deleteFocusedPatternRun
            )
        }
        .contentShape(Rectangle())
        .focusable()
        .focused($plotHasKeyboardFocus)
        .focusEffectDisabled()
        .onKeyPress(keys: [.delete, .deleteForward], phases: .down) { _ in
            guard focusedPatternRun != nil else { return .ignored }
            deleteFocusedPatternRun()
            return .handled
        }
        .onContinuousHover { phase in
            guard showsISIInformationPanel else {
                clearHover()
                return
            }
            switch phase {
            case .active(let location):
                hoverLocation = location
                hoveredRowID = row(atX: location.x, width: width)?.id
            case .ended:
                clearHover()
            }
        }
        .gesture(
            DragGesture(minimumDistance: 0)
                .onChanged { value in
                    if gesturePatternRun == nil,
                       dragAnchorTime == nil,
                       let patternRun = patternRun(
                           at: value.startLocation,
                           width: width
                       ) {
                        gesturePatternRun = patternRun
                        focusedPatternRun = patternRun
                        selectedRowIDs = patternRun.rowIDs(in: rows)
                        let centerIndex = patternRun.rowRange.lowerBound
                            + patternRun.rowRange.count / 2
                        dragFocusRowID = rows[centerIndex].id
                        return
                    }
                    if gesturePatternRun != nil { return }

                    focusedPatternRun = nil
                    guard let time = time(atX: value.location.x, width: width),
                          let focusedRow = row(atTime: time) else { return }
                    if dragAnchorTime == nil { dragAnchorTime = time }
                    dragFocusRowID = focusedRow.id
                    selectedRowIDs = ManualISIGraphicSelection.selectedIDs(
                        in: rows,
                        from: dragAnchorTime ?? time,
                        through: time
                    )
                }
                .onEnded { _ in
                    if gesturePatternRun != nil {
                        gesturePatternRun = nil
                        plotHasKeyboardFocus = true
                        deleteKeyFocusRequestID &+= 1
                        onFocusedRowIDChange(dragFocusRowID)
                        dragFocusRowID = nil
                        return
                    }
                    dragAnchorTime = nil
                    onFocusedRowIDChange(dragFocusRowID)
                    dragFocusRowID = nil
                }
        )
        .onChange(of: showsISIInformationPanel) { _, isVisible in
            if !isVisible { clearHover() }
        }
        .onChange(of: rows) { _, updatedRows in
            guard let focusedPatternRun else { return }
            if !ManualISIGraphicPatternGlass.runs(in: updatedRows)
                .contains(focusedPatternRun) {
                self.focusedPatternRun = nil
            }
        }
        .accessibilityLabel(l10n.t("手工 ISI 时间图"))
        .accessibilityHint(l10n.t("拖动以选择连续 ISI；点击已标记色块后按 Delete 清除该区块"))
    }

    @ViewBuilder
    private func patternGlassOverlay(width: CGFloat) -> some View {
        let placements = patternGlassPlacements(width: width)
        if !placements.isEmpty {
            ZStack(alignment: .topLeading) {
                ForEach(Array(placements.enumerated()), id: \.offset) { _, placement in
                    ManualISIPatternGlassSurface(
                        tint: color(for: placement.run.label),
                        isFocused: focusedPatternRun == placement.run
                    )
                        .frame(width: placement.rect.width, height: placement.rect.height)
                        .position(x: placement.rect.midX, y: placement.rect.midY)
                }
            }
            .frame(width: width, height: plotHeight)
            .allowsHitTesting(false)
        }
    }

    private func patternGlassPlacements(width: CGFloat) -> [ManualISIPatternGlassPlacement] {
        guard let first = rows.first, let last = rows.last else { return [] }
        let span = max(last.rightSeconds - first.leftSeconds, 0.000_001)
        let usableWidth = max(1, width - horizontalPadding * 2)
        func x(_ seconds: Double) -> CGFloat {
            horizontalPadding
                + CGFloat((seconds - first.leftSeconds) / span) * usableWidth
        }
        func laneFrame(_ lane: ManualISIGraphicPatternLane) -> (y: CGFloat, height: CGFloat) {
            switch lane {
            case .state: (7, 27)
            case .event: (42, 26)
            case .other: (76, 20)
            }
        }

        return ManualISIGraphicPatternGlass.runs(in: rows).map { run in
            let left = x(rows[run.rowRange.lowerBound].leftSeconds)
            let right = max(left + 1, x(rows[run.rowRange.upperBound].rightSeconds))
            let lane = laneFrame(run.lane)
            return ManualISIPatternGlassPlacement(
                run: run,
                rect: CGRect(x: left, y: lane.y, width: right - left, height: lane.height)
            )
        }
    }

    private func time(atX x: CGFloat, width: CGFloat) -> Double? {
        guard let first = rows.first, let last = rows.last else { return nil }
        let span = max(last.rightSeconds - first.leftSeconds, 0.000_001)
        let usableWidth = max(1, width - horizontalPadding * 2)
        let fraction = min(max(Double((x - horizontalPadding) / usableWidth), 0), 1)
        return first.leftSeconds + fraction * span
    }

    private func row(atX x: CGFloat, width: CGFloat) -> ManualISIGraphicRow? {
        guard let time = time(atX: x, width: width) else { return nil }
        return row(atTime: time)
    }

    private func row(atTime time: Double) -> ManualISIGraphicRow? {
        guard let index = ManualISIGraphicSelection.rowIndex(at: time, in: rows) else {
            return nil
        }
        return rows[index]
    }

    private func patternRun(
        at location: CGPoint,
        width: CGFloat
    ) -> ManualISIGraphicPatternRun? {
        patternGlassPlacements(width: width)
            .first(where: { $0.rect.contains(location) })?
            .run
    }

    @ViewBuilder
    private func selectionGlassOverlay(width: CGFloat) -> some View {
        let rectangles = selectionRectangles(width: width)
        if !rectangles.isEmpty {
            // A system glass/material surface refracts or blurs the raster beneath it. That is
            // inappropriate for scientific spike geometry: it can visually duplicate a tick.
            // Each selected span owns a tightly bounded surface. This mirrors the sidebar
            // slider's body/specular/double-rim language while never creating a full-width
            // compositing layer or resampling the scientific plot behind it.
            ZStack(alignment: .topLeading) {
                ForEach(Array(rectangles.enumerated()), id: \.offset) { _, rectangle in
                    ManualISISelectionGlassSurface()
                    .frame(width: rectangle.width, height: rectangle.height)
                    .position(x: rectangle.midX, y: rectangle.midY)
                }
            }
            .frame(width: width, height: plotHeight)
            .allowsHitTesting(false)
        }
    }

    private func selectionRectangles(width: CGFloat) -> [CGRect] {
        guard let first = rows.first, let last = rows.last else { return [] }
        let span = max(last.rightSeconds - first.leftSeconds, 0.000_001)
        let usableWidth = max(1, width - horizontalPadding * 2)
        func x(_ seconds: Double) -> CGFloat {
            horizontalPadding
                + CGFloat((seconds - first.leftSeconds) / span) * usableWidth
        }

        return ManualISIGraphicSelection.selectedIndexRanges(
            in: rows,
            selectedRowIDs: selectedRowIDs
        ).map { range in
            let left = x(rows[range.lowerBound].leftSeconds)
            let right = max(left + 1, x(rows[range.upperBound].rightSeconds))
            // Selection chrome belongs to the Spike lane. Keeping it out of the state/event/
            // other lanes prevents a wide selection from becoming a large white panel while
            // still marking the exact interval bounded by the two selected spike timestamps.
            return CGRect(x: left, y: 101, width: right - left, height: 56)
        }
    }

    private func informationCardPosition(for location: CGPoint, plotWidth: CGFloat) -> CGPoint {
        let cardWidth: CGFloat = 268
        let cardHeight: CGFloat = 154
        let proposedRight = location.x + 12 + cardWidth / 2
        let x = proposedRight + cardWidth / 2 <= plotWidth
            ? proposedRight
            : location.x - 12 - cardWidth / 2
        return CGPoint(
            x: min(max(cardWidth / 2 + 4, x), plotWidth - cardWidth / 2 - 4),
            y: min(max(cardHeight / 2 + 4, location.y), plotHeight - cardHeight / 2 - 4)
        )
    }

    private func clearHover() {
        hoveredRowID = nil
        hoverLocation = nil
    }

    private func deleteFocusedPatternRun() {
        guard let focusedPatternRun else { return }
        onDeletePatternRun(focusedPatternRun)
        self.focusedPatternRun = nil
        plotHasKeyboardFocus = false
    }

    private func color(for label: ManualAnnotationLabel) -> Color {
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

    private func formatElapsed(_ seconds: Double) -> String {
        if seconds < 1 { return String(format: "%.1f ms", seconds * 1_000) }
        return String(format: "%.3f s", seconds)
    }
}

/// SwiftUI's focus state does not reliably make a gesture-only drawing surface the AppKit first
/// responder. This narrowly scoped bridge requests first-responder status only after a marked
/// pattern block is clicked, handles the two macOS delete key codes, and otherwise stays out of
/// mouse hit testing and command routing.
@MainActor
struct ManualISIDeleteKeyCapture: NSViewRepresentable {
    let isActive: Bool
    let focusRequestID: UInt64
    let onDelete: () -> Void

    func makeCoordinator() -> Coordinator { Coordinator() }

    func makeNSView(context: Context) -> ManualISIDeleteKeyView {
        ManualISIDeleteKeyView(frame: .zero)
    }

    func updateNSView(_ nsView: ManualISIDeleteKeyView, context: Context) {
        nsView.isDeleteEnabled = isActive
        nsView.onDelete = onDelete

        guard isActive else {
            if nsView.window?.firstResponder === nsView {
                nsView.window?.makeFirstResponder(nil)
            }
            return
        }
        guard context.coordinator.lastFocusRequestID != focusRequestID else { return }
        context.coordinator.lastFocusRequestID = focusRequestID

        DispatchQueue.main.async { [weak nsView] in
            guard let nsView, nsView.isDeleteEnabled else { return }
            nsView.window?.makeFirstResponder(nsView)
        }
    }

    static func dismantleNSView(_ nsView: ManualISIDeleteKeyView, coordinator: Coordinator) {
        if nsView.window?.firstResponder === nsView {
            nsView.window?.makeFirstResponder(nil)
        }
        nsView.onDelete = nil
    }

    @MainActor
    final class Coordinator {
        var lastFocusRequestID: UInt64?
    }
}

@MainActor
final class ManualISIDeleteKeyView: NSView {
    var isDeleteEnabled = false
    var onDelete: (() -> Void)?

    override var acceptsFirstResponder: Bool { isDeleteEnabled }

    override func hitTest(_ point: NSPoint) -> NSView? {
        nil
    }

    override func keyDown(with event: NSEvent) {
        guard Self.shouldHandleDelete(
            keyCode: event.keyCode,
            modifierFlags: event.modifierFlags
        ) else {
            super.keyDown(with: event)
            return
        }
        onDelete?()
    }

    static func shouldHandleDelete(
        keyCode: UInt16,
        modifierFlags: NSEvent.ModifierFlags
    ) -> Bool {
        let disallowedModifiers: NSEvent.ModifierFlags = [.command, .control, .option]
        guard modifierFlags.intersection(disallowedModifiers).isEmpty else { return false }
        return keyCode == 51 || keyCode == 117
    }
}

private struct ManualISIPatternGlassPlacement {
    let run: ManualISIGraphicPatternRun
    let rect: CGRect
}

/// Native, rectangular, tinted Liquid Glass for an already-applied manual pattern. It has no
/// exterior shadow; the subtle highlight and depth edge stay inside the exact pattern geometry.
private struct ManualISIPatternGlassSurface: View {
    let tint: Color
    let isFocused: Bool

    private var shape: Rectangle { Rectangle() }

    @ViewBuilder
    var body: some View {
        ZStack {
            // This plate defines the authoritative silhouette. It remains an exact rectangle;
            // the system's optical edge contraction is kept one point inside it so the visible
            // pattern can never appear narrower at the top or wider at the bottom.
            shape.fill(tint.opacity(0.30))

            if #available(macOS 26.0, *) {
                shape
                    .fill(Color.white.opacity(0.001))
                    .glassEffect(.regular.tint(tint.opacity(0.56)), in: shape)
                    .padding(1)
            } else {
                shape
                    .fill(Color.white.opacity(0.001))
                    .background(.ultraThinMaterial, in: shape)
                    .padding(1)
            }
        }
        .clipShape(shape)
        .overlay {
            // A uniform, non-gradient rim makes all four external edges geometrically explicit.
            // Depth comes from the inset native glass, not from unequal top/bottom edge weights.
            shape.strokeBorder(
                isFocused ? Color.accentColor.opacity(0.95) : tint.opacity(0.72),
                lineWidth: isFocused ? 1.8 : 0.8
            )
        }
    }
}

/// A true system Liquid Glass selection surface placed behind the scientific Canvas.
/// The Canvas subsequently redraws every grid line, label, and spike at full fidelity.
private struct ManualISISelectionGlassSurface: View {
    private var shape: Rectangle {
        Rectangle()
    }

    @ViewBuilder
    var body: some View {
        if #available(macOS 26.0, *) {
            glassBody
                .glassEffect(.regular, in: shape)
                .overlay { innerRim }
                .overlay { innerDepthEdge }
        } else {
            glassBody
                .background(.ultraThinMaterial, in: shape)
                .overlay { innerRim }
                .overlay { innerDepthEdge }
        }
    }

    private var glassBody: some View {
        shape.fill(
            LinearGradient(
                colors: [
                    Color.white.opacity(0.030),
                    Color(nsColor: .textBackgroundColor).opacity(0.015),
                    Color.black.opacity(0.012),
                ],
                startPoint: .top,
                endPoint: .bottom
            )
        )
    }

    private var innerRim: some View {
        shape.strokeBorder(
            LinearGradient(
                colors: [
                    Color.white.opacity(0.62),
                    Color(nsColor: .separatorColor).opacity(0.24),
                    Color.black.opacity(0.10),
                ],
                startPoint: .topLeading,
                endPoint: .bottomTrailing
            ),
            lineWidth: 0.85
        )
    }

    private var innerDepthEdge: some View {
        shape
            .strokeBorder(Color.black.opacity(0.11), lineWidth: 2)
            .mask(
                LinearGradient(
                    colors: [Color.clear, Color.clear, Color.black],
                    startPoint: .topLeading,
                    endPoint: .bottomTrailing
                )
            )
    }
}

private struct ManualISIInformationCard: View {
    @Environment(\.l10n) private var l10n

    let row: ManualISIGraphicRow

    var body: some View {
        VStack(alignment: .leading, spacing: 5) {
            Text("ISI #\(row.isiIndex)")
                .font(.caption.weight(.semibold).monospacedDigit())
            informationRow(l10n.t("左侧时间戳"), value: seconds(row.leftSeconds))
            informationRow(l10n.t("右侧时间戳"), value: seconds(row.rightSeconds))
            informationRow(l10n.t("ISI 间隔"), value: milliseconds(row.rightSeconds - row.leftSeconds))
            informationRow("QC", value: l10n.t(row.isInvalidISI ? "绝对无效 ISI" : "通过"))
            informationRow(l10n.t("STPD 检测"), value: automaticResult)
            informationRow(l10n.t("手工标记"), value: manualResult)
        }
        .padding(.horizontal, 10)
        .padding(.vertical, 8)
        .background(.regularMaterial, in: RoundedRectangle(cornerRadius: 9))
        .overlay {
            RoundedRectangle(cornerRadius: 9)
                .stroke(.white.opacity(0.28), lineWidth: 0.7)
        }
        .shadow(color: .black.opacity(0.12), radius: 6, y: 2)
    }

    private func informationRow(_ title: String, value: String) -> some View {
        HStack(alignment: .firstTextBaseline, spacing: 8) {
            Text(title)
                .foregroundStyle(.secondary)
            Spacer(minLength: 4)
            Text(value)
                .multilineTextAlignment(.trailing)
                .lineLimit(1)
        }
        .font(.caption.monospacedDigit())
    }

    private var automaticResult: String {
        guard !row.automaticLabels.isEmpty else { return l10n.t("尚无匹配结果") }
        return row.automaticLabels.map(automaticName).joined(separator: " + ")
    }

    private var manualResult: String {
        let values = [
            row.stateLabel.map { "\(ManualAnnotationSemanticTrack.state.displayName(using: l10n)): \($0.displayName(using: l10n))" },
            row.eventLabel.map { "\(ManualAnnotationSemanticTrack.event.displayName(using: l10n)): \($0.displayName(using: l10n))" },
            row.otherLabel.map { "\(ManualAnnotationSemanticTrack.other.displayName(using: l10n)): \($0.displayName(using: l10n))" },
        ].compactMap { $0 }
        return values.isEmpty ? l10n.t("未标记") : values.joined(separator: " · ")
    }

    private func automaticName(_ label: ClassicAnchorLabel) -> String {
        switch label {
        case .burst: l10n.t("Burst")
        case .highFrequencyBurst: "HFB"
        case .longBurst: l10n.t("长 Burst")
        case .possibleBurst: l10n.t("可能 Burst")
        case .tonic: l10n.t("Tonic")
        case .highFrequencyTonic: l10n.t("高频 Tonic")
        case .highFrequencySpiking: "HFS"
        case .pause: l10n.t("Pause")
        case .reject: l10n.t("拒绝")
        case .profile: l10n.t("配置")
        }
    }

    private func seconds(_ value: Double) -> String {
        value.isFinite ? String(format: "%.6f s", value) : "—"
    }

    private func milliseconds(_ value: Double) -> String {
        value.isFinite ? String(format: "%.3f ms", value * 1_000) : "—"
    }
}

/// SwiftUI `Table` keeps its selection in sync but does not expose a row-scroll anchor on macOS.
/// This bridge performs only the missing imperative edge: after a graph click/drag completes, it
/// locates the table backing this view and centers the focused row in its clip view.
@MainActor
struct ManualISITableCenteringBridge: NSViewRepresentable {
    let focusedRowID: String?
    let requestID: UInt64
    let rowIDs: [String]

    func makeCoordinator() -> Coordinator { Coordinator() }

    func makeNSView(context: Context) -> NSView {
        let view = NSView(frame: .zero)
        view.translatesAutoresizingMaskIntoConstraints = false
        return view
    }

    func updateNSView(_ nsView: NSView, context: Context) {
        guard let focusedRowID,
              let rowIndex = rowIDs.firstIndex(of: focusedRowID) else { return }
        let request = Request(
            requestID: requestID,
            rowID: focusedRowID,
            rowIndex: rowIndex,
            rowCount: rowIDs.count,
            firstRowID: rowIDs.first,
            lastRowID: rowIDs.last
        )
        guard context.coordinator.lastRequest != request else { return }
        context.coordinator.lastRequest = request
        context.coordinator.center(request, relativeTo: nsView)
    }

    struct Request: Equatable, Sendable {
        let requestID: UInt64
        let rowID: String
        let rowIndex: Int
        let rowCount: Int
        let firstRowID: String?
        let lastRowID: String?
    }

    @MainActor
    final class Coordinator {
        var lastRequest: Request?

        func center(_ request: Request, relativeTo marker: NSView, attempt: Int = 0) {
            DispatchQueue.main.async { [weak self, weak marker] in
                guard let self, let marker else { return }
                guard let table = self.nearestTable(to: marker, rowCount: request.rowCount),
                      table.numberOfRows > request.rowIndex,
                      let scrollView = table.enclosingScrollView else {
                    guard attempt < 3 else { return }
                    DispatchQueue.main.asyncAfter(deadline: .now() + 0.04) {
                        self.center(request, relativeTo: marker, attempt: attempt + 1)
                    }
                    return
                }

                table.scrollRowToVisible(request.rowIndex)
                let rowRect = table.rect(ofRow: request.rowIndex)
                let clipView = scrollView.contentView
                let maximumY = max(0, table.bounds.height - clipView.bounds.height)
                let centeredY = min(max(0, rowRect.midY - clipView.bounds.height / 2), maximumY)
                clipView.scroll(to: NSPoint(x: clipView.bounds.origin.x, y: centeredY))
                scrollView.reflectScrolledClipView(clipView)
            }
        }

        private func nearestTable(to marker: NSView, rowCount: Int) -> NSTableView? {
            guard let root = marker.window?.contentView else { return nil }
            let markerFrame = marker.convert(marker.bounds, to: nil)
            return tables(in: root)
                .filter { $0.numberOfRows == rowCount }
                .min { lhs, rhs in
                    distance(from: markerFrame, to: lhs.convert(lhs.bounds, to: nil))
                        < distance(from: markerFrame, to: rhs.convert(rhs.bounds, to: nil))
                }
        }

        private func tables(in view: NSView) -> [NSTableView] {
            var result = view.subviews.compactMap { $0 as? NSTableView }
            for subview in view.subviews {
                result.append(contentsOf: tables(in: subview))
            }
            return result
        }

        private func distance(from lhs: CGRect, to rhs: CGRect) -> CGFloat {
            let dx = lhs.midX - rhs.midX
            let dy = lhs.midY - rhs.midY
            return dx * dx + dy * dy
        }
    }
}
