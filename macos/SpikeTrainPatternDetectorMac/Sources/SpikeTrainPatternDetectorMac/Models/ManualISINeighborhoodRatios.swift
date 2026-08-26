import Foundation

/// Display-only neighborhood ratios for one ISI row in the manual-review table.
///
/// The owner-defined convention is directional and deliberately not symmetrized:
/// `leftMM = previousISI / currentISI` and `rightMM = nextISI / currentISI`.
/// These values never enter detector evidence, canonical identity, or exports.
struct ManualISINeighborhoodRatios: Equatable, Sendable {
    let leftMM: Double?
    let rightMM: Double?

    static func indexedSeconds<RowID: Hashable>(
        _ samples: [(id: RowID, interval: Double)]
    ) -> [RowID: ManualISINeighborhoodRatios] {
        indexed(samples: samples) { neighbor, current in
            guard current.isFinite, current > 0,
                  neighbor.isFinite, neighbor >= 0 else { return nil }
            return neighbor / current
        }
    }

    static func indexedMicroseconds<RowID: Hashable>(
        _ samples: [(id: RowID, interval: Int64)]
    ) -> [RowID: ManualISINeighborhoodRatios] {
        indexed(samples: samples) { neighbor, current in
            guard current > 0, neighbor >= 0 else { return nil }
            return Double(neighbor) / Double(current)
        }
    }

    private static func indexed<RowID: Hashable, Interval>(
        samples: [(id: RowID, interval: Interval)],
        ratio: (Interval, Interval) -> Double?
    ) -> [RowID: ManualISINeighborhoodRatios] {
        var result: [RowID: ManualISINeighborhoodRatios] = [:]
        result.reserveCapacity(samples.count)

        for index in samples.indices {
            let current = samples[index].interval
            let leftMM = index > samples.startIndex
                ? ratio(samples[samples.index(before: index)].interval, current)
                : nil
            let rightMM = samples.index(after: index) < samples.endIndex
                ? ratio(samples[samples.index(after: index)].interval, current)
                : nil
            result[samples[index].id] = ManualISINeighborhoodRatios(
                leftMM: leftMM,
                rightMM: rightMM
            )
        }
        return result
    }
}
