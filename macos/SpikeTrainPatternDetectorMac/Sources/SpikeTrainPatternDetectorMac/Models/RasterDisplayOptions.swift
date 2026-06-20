import Foundation
import STPDCore

enum RasterTimeMode: String, CaseIterable, Identifiable {
    case aligned
    case raw

    var id: String { rawValue }

    var title: String {
        switch self {
        case .aligned:
            return "Aligned"
        case .raw:
            return "Raw"
        }
    }

    var axisTitle: String {
        switch self {
        case .aligned:
            return "Aligned time"
        case .raw:
            return "Raw timestamp"
        }
    }
}

struct RasterTimeRange {
    let lowerBound: Double
    let upperBound: Double

    var duration: Double {
        max(upperBound - lowerBound, 0)
    }
}

extension SpikeTrain {
    func rasterTimestamp(at index: Int, mode: RasterTimeMode) -> Double {
        switch mode {
        case .aligned:
            return alignedTimestampsSec[index]
        case .raw:
            return timestampsSec[index]
        }
    }
}

extension Collection where Element == SpikeTrain {
    func rasterTimeRange(mode: RasterTimeMode) -> RasterTimeRange {
        switch mode {
        case .aligned:
            let upper = map(\.alignedDurationSec).max() ?? 0
            return RasterTimeRange(lowerBound: 0, upperBound: Swift.max(upper, 1))
        case .raw:
            let starts = compactMap(\.firstTimestampSec)
            let ends = compactMap(\.lastTimestampSec)
            guard let lower = starts.min(), let upper = ends.max() else {
                return RasterTimeRange(lowerBound: 0, upperBound: 1)
            }
            return RasterTimeRange(lowerBound: lower, upperBound: Swift.max(lower + 1, upper))
        }
    }
}
