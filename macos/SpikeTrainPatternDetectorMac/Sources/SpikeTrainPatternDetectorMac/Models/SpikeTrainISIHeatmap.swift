import Foundation
import STPDCore

enum SpikeTrainISIHeatmapScaleMode: String, Hashable, Sendable {
    /// Robust limits calculated from the currently displayed, time-binned cells.
    case explorationRobust
    /// Exact full range from every positive ISI in the loaded dataset.
    case publicationDatasetFixed
}

/// A display-only, ISI-weighted raster projection. Each horizontal pixel represents the locally
/// averaged `log10(1 / ISI)` over its time bin; it is never a detector input and never changes a
/// timestamp, ISI, label, or result-package value.
struct SpikeTrainISIHeatmap: Equatable {
    let rowCount: Int
    let columnCount: Int
    let lowerTimeSec: Double
    let upperTimeSec: Double
    /// True only for a cell covered by a positive, real ISI from that row's own spike train.
    /// Unsupported cells are rendered as background rather than as an invented long ISI.
    let supportedCells: [Bool]
    /// Normalized heat values in top-to-bottom train order, row-major. Unsupported cells are
    /// identified independently by `supportedCells`; zero here is a real lowest-scale value.
    let normalizedValues: [Float]
    let lowerLogHz: Double?
    let upperLogHz: Double?
    let scaleMode: SpikeTrainISIHeatmapScaleMode
    let scaleObservationCount: Int
    let lowClipCount: Int
    let highClipCount: Int

    static let empty = SpikeTrainISIHeatmap(
        rowCount: 0,
        columnCount: 0,
        lowerTimeSec: 0,
        upperTimeSec: 0,
        supportedCells: [],
        normalizedValues: [],
        lowerLogHz: nil,
        upperLogHz: nil,
        scaleMode: .explorationRobust,
        scaleObservationCount: 0,
        lowClipCount: 0,
        highClipCount: 0
    )
}

enum SpikeTrainISIHeatmapBuilder {
    static func make(
        trains: [SpikeTrain],
        timeMode: RasterTimeMode,
        columnCount requestedColumnCount: Int,
        scaleTrains: [SpikeTrain]? = nil,
        scaleMode: SpikeTrainISIHeatmapScaleMode = .explorationRobust
    ) -> SpikeTrainISIHeatmap {
        let requestedColumns = min(max(requestedColumnCount, 32), 512)
        guard !trains.isEmpty else { return .empty }

        let timeRange = trains.rasterTimeRange(mode: timeMode)
        let duration = timeRange.duration
        guard duration.isFinite, duration > 0 else { return .empty }
        let columnCount = effectiveColumnCount(
            requestedColumns: requestedColumns,
            timeMode: timeMode,
            timeRange: timeRange,
            trains: trains
        )

        let rowCount = trains.count
        let cellCount = rowCount * columnCount
        var weightedSum = Array(repeating: 0.0, count: cellCount)
        var coverage = Array(repeating: 0.0, count: cellCount)

        for (row, train) in trains.enumerated() {
            let timestamps = timestamps(for: train, timeMode: timeMode)
            guard timestamps.count > 1 else { continue }

            for index in timestamps.indices.dropFirst() {
                let lower = timestamps[index - 1]
                let upper = timestamps[index]
                let isi = upper - lower
                guard lower.isFinite, upper.isFinite, isi.isFinite, isi > 0 else { continue }

                // `log10(1 / ISI)` is local firing intensity in log Hz: a smaller ISI produces
                // a larger, warmer value without allowing a single tiny interval to dominate.
                let logHz = log10(1.0 / isi)
                guard logHz.isFinite else { continue }
                accumulate(
                    logHz: logHz,
                    interval: lower..<upper,
                    row: row,
                    lowerTime: timeRange.lowerBound,
                    duration: duration,
                    columnCount: columnCount,
                    weightedSum: &weightedSum,
                    coverage: &coverage
                )
            }
        }

        let supported = zip(weightedSum, coverage).compactMap { sum, weight -> Double? in
            guard weight > 0 else { return nil }
            return sum / weight
        }
        guard !supported.isEmpty else {
            return SpikeTrainISIHeatmap(
                rowCount: rowCount,
                columnCount: columnCount,
                lowerTimeSec: timeRange.lowerBound,
                upperTimeSec: timeRange.upperBound,
                supportedCells: Array(repeating: false, count: cellCount),
                normalizedValues: Array(repeating: 0, count: cellCount),
                lowerLogHz: nil,
                upperLogHz: nil,
                scaleMode: scaleMode,
                scaleObservationCount: 0,
                lowClipCount: 0,
                highClipCount: 0
            )
        }

        let scaleValues: [Double]
        switch scaleMode {
        case .explorationRobust:
            scaleValues = supported
        case .publicationDatasetFixed:
            scaleValues = positiveLogHzValues(in: scaleTrains ?? trains)
        }
        let valuesForLimits = scaleValues.isEmpty ? supported : scaleValues
        let sorted = valuesForLimits.sorted()
        let lowerLogHz: Double
        let upperCandidate: Double
        let lowClipCount: Int
        let highClipCount: Int
        switch scaleMode {
        case .explorationRobust:
            lowerLogHz = quantile(sorted, 0.05)
            upperCandidate = quantile(sorted, 0.95)
            lowClipCount = supported.filter { $0 < lowerLogHz }.count
            highClipCount = supported.filter { $0 > upperCandidate }.count
        case .publicationDatasetFixed:
            lowerLogHz = sorted.first ?? 0
            upperCandidate = sorted.last ?? lowerLogHz
            lowClipCount = 0
            highClipCount = 0
        }
        let upperLogHz = max(upperCandidate, lowerLogHz + 0.000_001)

        var normalized = Array(repeating: Float.zero, count: cellCount)
        for cell in normalized.indices where coverage[cell] > 0 {
            let value = weightedSum[cell] / coverage[cell]
            normalized[cell] = Float(min(max((value - lowerLogHz) / (upperLogHz - lowerLogHz), 0), 1))
        }

        return SpikeTrainISIHeatmap(
            rowCount: rowCount,
            columnCount: columnCount,
            lowerTimeSec: timeRange.lowerBound,
            upperTimeSec: timeRange.upperBound,
            supportedCells: coverage.map { $0 > 0 },
            normalizedValues: displayValues(
                normalized,
                supported: coverage.map { $0 > 0 },
                rows: rowCount,
                columns: columnCount,
                scaleMode: scaleMode
            ),
            lowerLogHz: lowerLogHz,
            upperLogHz: upperLogHz,
            scaleMode: scaleMode,
            scaleObservationCount: valuesForLimits.count,
            lowClipCount: lowClipCount,
            highClipCount: highClipCount
        )
    }

    /// In raw time, a shared absolute axis can be much longer than the aligned axis. Preserve the
    /// same seconds-per-column as the selected aligned resolution instead of compressing each
    /// train's ISIs into a handful of broad raw-time bins. The cell budget is display-only and
    /// bounds memory for unusually wide recordings or very large train collections.
    private static func effectiveColumnCount(
        requestedColumns: Int,
        timeMode: RasterTimeMode,
        timeRange: RasterTimeRange,
        trains: [SpikeTrain]
    ) -> Int {
        guard timeMode == .raw else { return requestedColumns }

        let alignedReferenceDuration = max(trains.map(\.alignedDurationSec).max() ?? 0, 1)
        let targetSecondsPerColumn = alignedReferenceDuration / Double(requestedColumns)
        guard targetSecondsPerColumn.isFinite, targetSecondsPerColumn > 0 else {
            return requestedColumns
        }

        let desired = Int((timeRange.duration / targetSecondsPerColumn).rounded(.up))
        let maximumColumnsFromCellBudget = max(32, 1_000_000 / max(trains.count, 1))
        let maximumColumns = min(8_192, maximumColumnsFromCellBudget)
        return min(max(desired, requestedColumns), maximumColumns)
    }

    private static func timestamps(for train: SpikeTrain, timeMode: RasterTimeMode) -> [Double] {
        switch timeMode {
        case .aligned: train.alignedTimestampsSec
        case .raw: train.timestampsSec
        }
    }

    private static func positiveLogHzValues(in trains: [SpikeTrain]) -> [Double] {
        trains.flatMap { train in
            zip(train.timestampsSec, train.timestampsSec.dropFirst()).compactMap { lower, upper in
                let isi = upper - lower
                guard isi.isFinite, isi > 0 else { return nil }
                let value = log10(1.0 / isi)
                return value.isFinite ? value : nil
            }
        }
    }

    private static func accumulate(
        logHz: Double,
        interval: Range<Double>,
        row: Int,
        lowerTime: Double,
        duration: Double,
        columnCount: Int,
        weightedSum: inout [Double],
        coverage: inout [Double]
    ) {
        let clippedLower = max(interval.lowerBound, lowerTime)
        let clippedUpper = min(interval.upperBound, lowerTime + duration)
        guard clippedUpper > clippedLower else { return }

        let unit = duration / Double(columnCount)
        let firstColumn = max(0, min(columnCount - 1, Int(floor((clippedLower - lowerTime) / unit))))
        let lastColumn = max(0, min(columnCount - 1, Int(ceil((clippedUpper - lowerTime) / unit)) - 1))
        guard lastColumn >= firstColumn else { return }

        for column in firstColumn...lastColumn {
            let cellLower = lowerTime + Double(column) * unit
            let cellUpper = cellLower + unit
            let overlap = max(0, min(clippedUpper, cellUpper) - max(clippedLower, cellLower))
            guard overlap > 0 else { continue }
            let cell = row * columnCount + column
            weightedSum[cell] += logHz * overlap
            coverage[cell] += overlap
        }
    }

    private static func quantile(_ sorted: [Double], _ probability: Double) -> Double {
        guard let first = sorted.first else { return 0 }
        guard sorted.count > 1 else { return first }
        let position = min(max(probability, 0), 1) * Double(sorted.count - 1)
        let lower = Int(floor(position))
        let upper = Int(ceil(position))
        guard lower != upper else { return sorted[lower] }
        let fraction = position - Double(lower)
        return sorted[lower] * (1 - fraction) + sorted[upper] * fraction
    }

    /// Publication mode preserves the calculated time cells exactly. Exploration mode applies a
    /// small, within-train-only horizontal smoother for interactive pattern browsing; it never
    /// blends two spike trains or fills regions outside a train's actual timestamp coverage.
    private static func displayValues(
        _ values: [Float],
        supported: [Bool],
        rows: Int,
        columns: Int,
        scaleMode: SpikeTrainISIHeatmapScaleMode
    ) -> [Float] {
        guard rows > 0, columns > 0 else { return values }
        guard scaleMode == .explorationRobust else { return values }
        let horizontalKernel: [(Int, Float)] = [(-2, 1), (-1, 4), (0, 6), (1, 4), (2, 1)]
        var result = Array(repeating: Float.zero, count: values.count)

        for row in 0..<rows {
            for column in 0..<columns {
                let cell = row * columns + column
                guard supported[cell] else { continue }
                var sum: Float = 0
                var weight: Float = 0
                for (offset, kernelWeight) in horizontalKernel {
                    let neighbour = column + offset
                    guard neighbour >= 0, neighbour < columns else { continue }
                    let neighbourCell = row * columns + neighbour
                    guard supported[neighbourCell] else { continue }
                    sum += values[neighbourCell] * kernelWeight
                    weight += kernelWeight
                }
                result[cell] = weight > 0 ? sum / weight : 0
            }
        }
        return result
    }
}
