import Foundation
import STPDCore

/// Which non-destructive annotation layer is rendered in the mode heatmap.
enum SpikeTrainModeHeatmapSource: String, CaseIterable, Identifiable, Sendable {
    case combined
    case automatic
    case manual

    var id: String { rawValue }

    var title: String {
        switch self {
        case .combined: "合并"
        case .automatic: "自动"
        case .manual: "手工"
        }
    }
}

/// A time range already expressed in raw recording seconds. It is display evidence only: this
/// projection never edits a detector candidate, timestamp, ISI, manual annotation, or review.
struct SpikeTrainModeHeatmapSegment: Hashable, Sendable {
    let trainID: String
    let label: ManualAnnotationLabel
    let rawStartSec: Double
    let rawEndSec: Double
}

/// Compact categorical mode grid. State, event and other are intentionally distinct planes:
/// HFS can therefore coexist with an embedded HFB, while a pause remains an event-layer mark
/// rather than being silently blended into a state colour.
struct SpikeTrainModeHeatmap: Equatable {
    let rowCount: Int
    let columnCount: Int
    let lowerTimeSec: Double
    let upperTimeSec: Double
    let stateCells: [UInt8]
    let eventCells: [UInt8]
    let otherCells: [UInt8]

    static let empty = SpikeTrainModeHeatmap(
        rowCount: 0,
        columnCount: 0,
        lowerTimeSec: 0,
        upperTimeSec: 0,
        stateCells: [],
        eventCells: [],
        otherCells: []
    )

    static func code(for label: ManualAnnotationLabel) -> UInt8 {
        switch label {
        case .tonic: 1
        case .highFrequencyTonic: 2
        case .highFrequencySpiking: 3
        case .burst: 4
        case .highFrequencyBurst: 5
        case .longBurst: 6
        case .pause: 7
        case .other: 8
        case .notBurst: 0
        }
    }

    static func label(for code: UInt8) -> ManualAnnotationLabel? {
        switch code {
        case 1: .tonic
        case 2: .highFrequencyTonic
        case 3: .highFrequencySpiking
        case 4: .burst
        case 5: .highFrequencyBurst
        case 6: .longBurst
        case 7: .pause
        case 8: .other
        default: nil
        }
    }
}

enum SpikeTrainModeHeatmapBuilder {
    static func make(
        trains: [SpikeTrain],
        automaticSegments: [SpikeTrainModeHeatmapSegment],
        manualSegments: [SpikeTrainModeHeatmapSegment],
        source: SpikeTrainModeHeatmapSource,
        timeMode: RasterTimeMode,
        columnCount requestedColumnCount: Int
    ) -> SpikeTrainModeHeatmap {
        guard !trains.isEmpty else { return .empty }
        let timeRange = trains.rasterTimeRange(mode: timeMode)
        guard timeRange.duration.isFinite, timeRange.duration > 0 else { return .empty }

        let requestedColumns = min(max(requestedColumnCount, 64), 512)
        let columns = effectiveColumnCount(
            requestedColumns: requestedColumns,
            timeMode: timeMode,
            timeRange: timeRange,
            trains: trains
        )
        let cellCount = trains.count * columns
        var state = Array(repeating: UInt8.zero, count: cellCount)
        var event = Array(repeating: UInt8.zero, count: cellCount)
        var other = Array(repeating: UInt8.zero, count: cellCount)
        let trainRows = Dictionary(uniqueKeysWithValues: trains.enumerated().map { ($0.element.id, $0.offset) })
        let trainByID = Dictionary(uniqueKeysWithValues: trains.map { ($0.id, $0) })

        func apply(_ segments: [SpikeTrainModeHeatmapSegment]) {
            for segment in segments {
                guard segment.label.polarity == .positive,
                      let row = trainRows[segment.trainID],
                      let train = trainByID[segment.trainID],
                      let range = displayRange(segment, train: train, mode: timeMode) else {
                    continue
                }
                let lower = max(min(range.lowerBound, range.upperBound), timeRange.lowerBound)
                let upper = min(max(range.lowerBound, range.upperBound), timeRange.upperBound)
                guard upper > lower else { continue }
                let first = column(for: lower, lowerTime: timeRange.lowerBound, duration: timeRange.duration, count: columns)
                let last = column(for: upper.nextDown, lowerTime: timeRange.lowerBound, duration: timeRange.duration, count: columns)
                let code = SpikeTrainModeHeatmap.code(for: segment.label)
                guard code > 0 else { continue }
                for column in first...max(first, last) {
                    let index = row * columns + column
                    switch segment.label.semanticTrack {
                    case .state: state[index] = code
                    case .event: event[index] = code
                    case .other: other[index] = code
                    }
                }
            }
        }

        // Manual selections are intentionally painted after automatic evidence. That is a visual
        // precedence rule within one semantic track only; it does not erase the detector result.
        switch source {
        case .automatic:
            apply(automaticSegments)
        case .manual:
            apply(manualSegments)
        case .combined:
            apply(automaticSegments)
            apply(manualSegments)
        }

        return SpikeTrainModeHeatmap(
            rowCount: trains.count,
            columnCount: columns,
            lowerTimeSec: timeRange.lowerBound,
            upperTimeSec: timeRange.upperBound,
            stateCells: state,
            eventCells: event,
            otherCells: other
        )
    }

    private static func displayRange(
        _ segment: SpikeTrainModeHeatmapSegment,
        train: SpikeTrain,
        mode: RasterTimeMode
    ) -> ClosedRange<Double>? {
        guard segment.rawStartSec.isFinite, segment.rawEndSec.isFinite else { return nil }
        switch mode {
        case .raw:
            return min(segment.rawStartSec, segment.rawEndSec)...max(segment.rawStartSec, segment.rawEndSec)
        case .aligned:
            guard let first = train.firstTimestampSec else { return nil }
            let start = segment.rawStartSec - first
            let end = segment.rawEndSec - first
            return min(start, end)...max(start, end)
        }
    }

    private static func column(for time: Double, lowerTime: Double, duration: Double, count: Int) -> Int {
        let normalized = (time - lowerTime) / duration
        return min(max(Int((normalized * Double(count)).rounded(.down)), 0), count - 1)
    }

    private static func effectiveColumnCount(
        requestedColumns: Int,
        timeMode: RasterTimeMode,
        timeRange: RasterTimeRange,
        trains: [SpikeTrain]
    ) -> Int {
        guard timeMode == .raw else { return requestedColumns }
        let alignedReferenceDuration = max(trains.map(\.alignedDurationSec).max() ?? 0, 1)
        let secondsPerColumn = alignedReferenceDuration / Double(requestedColumns)
        guard secondsPerColumn.isFinite, secondsPerColumn > 0 else { return requestedColumns }
        let desired = Int((timeRange.duration / secondsPerColumn).rounded(.up))
        let maximumFromBudget = max(64, 1_000_000 / max(trains.count * 3, 1))
        return min(max(desired, requestedColumns), min(8_192, maximumFromBudget))
    }
}
