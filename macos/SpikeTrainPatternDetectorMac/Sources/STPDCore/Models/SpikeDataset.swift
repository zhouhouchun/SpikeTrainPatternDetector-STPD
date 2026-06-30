import Foundation

public enum SpikeTimeUnit: String, CaseIterable, Sendable {
    case seconds
    case milliseconds

    public var scaleToSeconds: Double {
        switch self {
        case .seconds:
            return 1.0
        case .milliseconds:
            return 0.001
        }
    }
}

public enum DuplicateTimestampPolicy: String, CaseIterable, Sendable {
    case errorKeep = "error_keep"
    case warnKeep = "warn_keep"
    case collapseExact = "collapse_exact"

    public var title: String {
        switch self {
        case .errorKeep:
            return "Error, keep"
        case .warnKeep:
            return "Warn, keep"
        case .collapseExact:
            return "Collapse exact"
        }
    }
}

public struct SpikeTrain: Identifiable, Hashable, Sendable {
    public let id: String
    public let name: String
    public let timestampsSec: [Double]
    public let alignedTimestampsSec: [Double]
    public let isiSec: [Double?]
    public let duplicateTimestampPolicy: DuplicateTimestampPolicy
    public let inputWasUnsorted: Bool
    public let inputOrderIndices: [Int]
    public let droppedDuplicateTimestampCount: Int
    public let sortedDuplicateTimestampCountBeforePolicy: Int
    public let inputDuplicateTimestampStepCount: Int
    public let inputNonmonotonicStepCount: Int
    public let inputZeroOrNegativeStepCount: Int

    public init(
        name: String,
        timestampsSec rawTimestamps: [Double],
        duplicateTimestampPolicy: DuplicateTimestampPolicy = .errorKeep
    ) {
        let finiteTimestamps = rawTimestamps
            .enumerated()
            .compactMap { index, timestamp -> (inputIndex: Int, timestamp: Double)? in
                guard timestamp.isFinite else {
                    return nil
                }
                return (index + 1, timestamp)
            }

        let inputDiffs = zip(finiteTimestamps, finiteTimestamps.dropFirst()).map { pair in
            pair.1.timestamp - pair.0.timestamp
        }
        let inputNonmonotonicStepCount = inputDiffs.filter { $0.isFinite && $0 < 0 }.count
        let inputDuplicateTimestampStepCount = inputDiffs.filter { $0.isFinite && $0 == 0 }.count
        let inputZeroOrNegativeStepCount = inputDiffs.filter { $0.isFinite && $0 <= 0 }.count

        let sortedPairs = finiteTimestamps.sorted {
            if $0.timestamp == $1.timestamp {
                return $0.inputIndex < $1.inputIndex
            }
            return $0.timestamp < $1.timestamp
        }

        let sortedDiffs = zip(sortedPairs, sortedPairs.dropFirst()).map { pair in
            pair.1.timestamp - pair.0.timestamp
        }
        let sortedDuplicateTimestampCountBeforePolicy = sortedDiffs.filter { $0.isFinite && $0 == 0 }.count

        var droppedDuplicateTimestampCount = 0
        let retainedPairs: [(inputIndex: Int, timestamp: Double)]
        if duplicateTimestampPolicy == .collapseExact {
            var seen = Set<Double>()
            var retained: [(inputIndex: Int, timestamp: Double)] = []
            retained.reserveCapacity(sortedPairs.count)

            for pair in sortedPairs {
                if seen.insert(pair.timestamp).inserted {
                    retained.append(pair)
                } else {
                    droppedDuplicateTimestampCount += 1
                }
            }
            retainedPairs = retained
        } else {
            retainedPairs = sortedPairs
        }

        let sorted = retainedPairs.map(\.timestamp)

        self.id = name
        self.name = name
        self.timestampsSec = sorted
        self.duplicateTimestampPolicy = duplicateTimestampPolicy
        self.inputWasUnsorted = inputNonmonotonicStepCount > 0
        self.inputOrderIndices = retainedPairs.map(\.inputIndex)
        self.droppedDuplicateTimestampCount = droppedDuplicateTimestampCount
        self.sortedDuplicateTimestampCountBeforePolicy = sortedDuplicateTimestampCountBeforePolicy
        self.inputDuplicateTimestampStepCount = inputDuplicateTimestampStepCount
        self.inputNonmonotonicStepCount = inputNonmonotonicStepCount
        self.inputZeroOrNegativeStepCount = inputZeroOrNegativeStepCount

        let first = sorted.first ?? 0
        self.alignedTimestampsSec = sorted.map { $0 - first }

        if sorted.isEmpty {
            self.isiSec = []
        } else {
            var values: [Double?] = [nil]
            values.reserveCapacity(sorted.count)
            for index in sorted.indices.dropFirst() {
                values.append(sorted[index] - sorted[sorted.index(before: index)])
            }
            self.isiSec = values
        }
    }

    private init(
        id: String,
        name: String,
        timestampsSec: [Double],
        alignedTimestampsSec: [Double],
        isiSec: [Double?],
        duplicateTimestampPolicy: DuplicateTimestampPolicy,
        inputWasUnsorted: Bool,
        inputOrderIndices: [Int],
        droppedDuplicateTimestampCount: Int,
        sortedDuplicateTimestampCountBeforePolicy: Int,
        inputDuplicateTimestampStepCount: Int,
        inputNonmonotonicStepCount: Int,
        inputZeroOrNegativeStepCount: Int
    ) {
        self.id = id
        self.name = name
        self.timestampsSec = timestampsSec
        self.alignedTimestampsSec = alignedTimestampsSec
        self.isiSec = isiSec
        self.duplicateTimestampPolicy = duplicateTimestampPolicy
        self.inputWasUnsorted = inputWasUnsorted
        self.inputOrderIndices = inputOrderIndices
        self.droppedDuplicateTimestampCount = droppedDuplicateTimestampCount
        self.sortedDuplicateTimestampCountBeforePolicy = sortedDuplicateTimestampCountBeforePolicy
        self.inputDuplicateTimestampStepCount = inputDuplicateTimestampStepCount
        self.inputNonmonotonicStepCount = inputNonmonotonicStepCount
        self.inputZeroOrNegativeStepCount = inputZeroOrNegativeStepCount
    }

    /// Returns a copy with a different stable identity while preserving the
    /// user-facing `name`, all parsed timestamps, and QC diagnostics. Used to
    /// keep train IDs unique when display names (e.g. duplicate CSV headers)
    /// collide; recomputing via the public initializer would discard the
    /// original duplicate/sort diagnostics, so a field-preserving copy is used.
    func withID(_ newID: String) -> SpikeTrain {
        SpikeTrain(
            id: newID,
            name: name,
            timestampsSec: timestampsSec,
            alignedTimestampsSec: alignedTimestampsSec,
            isiSec: isiSec,
            duplicateTimestampPolicy: duplicateTimestampPolicy,
            inputWasUnsorted: inputWasUnsorted,
            inputOrderIndices: inputOrderIndices,
            droppedDuplicateTimestampCount: droppedDuplicateTimestampCount,
            sortedDuplicateTimestampCountBeforePolicy: sortedDuplicateTimestampCountBeforePolicy,
            inputDuplicateTimestampStepCount: inputDuplicateTimestampStepCount,
            inputNonmonotonicStepCount: inputNonmonotonicStepCount,
            inputZeroOrNegativeStepCount: inputZeroOrNegativeStepCount
        )
    }

    public var spikeCount: Int {
        timestampsSec.count
    }

    public var firstTimestampSec: Double? {
        timestampsSec.first
    }

    public var lastTimestampSec: Double? {
        timestampsSec.last
    }

    public var alignedDurationSec: Double {
        alignedTimestampsSec.last ?? 0
    }

    public var rawDurationSec: Double {
        guard let first = timestampsSec.first, let last = timestampsSec.last else {
            return 0
        }
        return max(0, last - first)
    }
}

public struct SpikeDataset: Identifiable, Hashable, Sendable {
    public let id: UUID
    public let name: String
    public let sourceDescription: String
    public let trains: [SpikeTrain]
    /// External task/stimulus event timestamps (annotation layer only; never spike trains). Defaulted so
    /// existing call sites keep compiling.
    public let taskEvents: [TaskEvent]

    public init(
        name: String,
        sourceDescription: String,
        trains: [SpikeTrain],
        taskEvents: [TaskEvent] = []
    ) {
        self.id = UUID()
        self.name = name
        self.sourceDescription = sourceDescription
        self.trains = SpikeDataset.trainsWithUniqueIDs(trains)
        self.taskEvents = taskEvents
    }

    /// Guarantees unique `SpikeTrain.id` values even when display names (e.g.
    /// duplicate CSV headers) collide, so any downstream
    /// `Dictionary(uniqueKeysWithValues:)` keyed on train ID cannot trap.
    /// The first occurrence keeps its ID; later collisions get a stable `#N`
    /// suffix. Display `name` is never changed.
    private static func trainsWithUniqueIDs(_ trains: [SpikeTrain]) -> [SpikeTrain] {
        var usedIDs = Set<String>()
        usedIDs.reserveCapacity(trains.count)
        var result: [SpikeTrain] = []
        result.reserveCapacity(trains.count)
        for train in trains {
            if !usedIDs.contains(train.id) {
                usedIDs.insert(train.id)
                result.append(train)
                continue
            }
            var suffix = 2
            var disambiguatedID = "\(train.id)#\(suffix)"
            while usedIDs.contains(disambiguatedID) {
                suffix += 1
                disambiguatedID = "\(train.id)#\(suffix)"
            }
            usedIDs.insert(disambiguatedID)
            result.append(train.withID(disambiguatedID))
        }
        return result
    }

    public var totalSpikeCount: Int {
        trains.reduce(0) { $0 + $1.spikeCount }
    }

    public var maxAlignedDurationSec: Double {
        trains.map(\.alignedDurationSec).max() ?? 0
    }

    public var rawTimeRangeSec: ClosedRange<Double>? {
        let starts = trains.compactMap(\.firstTimestampSec)
        let ends = trains.compactMap(\.lastTimestampSec)
        guard let lower = starts.min(), let upper = ends.max() else {
            return nil
        }
        return lower...max(lower, upper)
    }

    public var rawDurationSec: Double {
        guard let range = rawTimeRangeSec else {
            return 0
        }
        return max(0, range.upperBound - range.lowerBound)
    }

    public func applyingDuplicateTimestampPolicy(_ policy: DuplicateTimestampPolicy) -> SpikeDataset {
        SpikeDataset(
            name: name,
            sourceDescription: sourceDescription,
            trains: trains.map { train in
                SpikeTrain(
                    name: train.name,
                    timestampsSec: train.timestampsSec,
                    duplicateTimestampPolicy: policy
                )
            },
            taskEvents: taskEvents
        )
    }
}
