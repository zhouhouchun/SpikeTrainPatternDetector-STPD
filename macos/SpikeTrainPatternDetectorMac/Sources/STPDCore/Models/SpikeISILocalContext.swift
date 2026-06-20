import Foundation

public struct SpikeISILocalContextSettings: Hashable, Sendable {
    public var minValidISISec: Double
    public var halfWindow: Int

    public init(
        minValidISISec: Double = 0.001,
        halfWindow: Int = 11
    ) {
        self.minValidISISec = minValidISISec.isFinite && minValidISISec > 0 ? minValidISISec : 0.001
        self.halfWindow = max(1, halfWindow)
    }
}

public struct SpikeISILocalContextPoint: Identifiable, Hashable, Sendable {
    public let id: String
    public let trainID: String
    public let trainName: String
    public let isiIndex: Int
    public let isiSec: Double
    public let trainPercentile: Double
    public let localPercentile: Double?
    public let localMedianSec: Double?
    public let localRatio: Double?
    public let localMedianLogSec: Double?
    public let localMADLogSec: Double?
    public let localRobustZ: Double?

    public init(
        trainID: String,
        trainName: String,
        isiIndex: Int,
        isiSec: Double,
        trainPercentile: Double,
        localPercentile: Double?,
        localMedianSec: Double?,
        localRatio: Double?,
        localMedianLogSec: Double?,
        localMADLogSec: Double?,
        localRobustZ: Double?
    ) {
        self.id = "\(trainID)-isi-local-context-\(isiIndex)"
        self.trainID = trainID
        self.trainName = trainName
        self.isiIndex = isiIndex
        self.isiSec = isiSec
        self.trainPercentile = min(max(trainPercentile, 0), 1)
        self.localPercentile = localPercentile.map { min(max($0, 0), 1) }
        self.localMedianSec = localMedianSec
        self.localRatio = localRatio
        self.localMedianLogSec = localMedianLogSec
        self.localMADLogSec = localMADLogSec
        self.localRobustZ = localRobustZ
    }
}

public struct SpikeISILocalContextTable: Hashable, Sendable {
    public let trainID: String
    public let trainName: String
    public let points: [SpikeISILocalContextPoint]

    private let pointsByIndex: [Int: SpikeISILocalContextPoint]

    public init(trainID: String, trainName: String, points: [SpikeISILocalContextPoint]) {
        self.trainID = trainID
        self.trainName = trainName
        self.points = points
        self.pointsByIndex = Dictionary(uniqueKeysWithValues: points.map { ($0.isiIndex, $0) })
    }

    public func point(for isiIndex: Int) -> SpikeISILocalContextPoint? {
        pointsByIndex[isiIndex]
    }

    public static func build(
        for train: SpikeTrain,
        settings: SpikeISILocalContextSettings = SpikeISILocalContextSettings()
    ) -> SpikeISILocalContextTable {
        let validPairs = train.isiSec.enumerated().compactMap { index, value -> (index: Int, value: Double)? in
            guard index > 0,
                  let value,
                  value.isFinite,
                  value >= settings.minValidISISec - tolerance(for: settings.minValidISISec) else {
                return nil
            }
            return (index, value)
        }
        let sortedTrainValues = validPairs.map(\.value).sorted()
        let validByIndex = Dictionary(uniqueKeysWithValues: validPairs.map { ($0.index, $0.value) })

        let points = validPairs.map { pair in
            let localValues = localValues(
                around: pair.index,
                valuesByIndex: validByIndex,
                halfWindow: settings.halfWindow,
                excluding: [pair.index]
            )
            let localMedian = median(localValues)
            let localRatio = localMedian.flatMap { median -> Double? in
                guard median.isFinite, median > 0 else {
                    return nil
                }
                return pair.value / median
            }
            let localLogs = localValues.filter { $0.isFinite && $0 > 0 }.map(log)
            let localMedianLog = median(localLogs)
            let localMADLog = localMedianLog.flatMap { medianLog in
                median(localLogs.map { abs($0 - medianLog) })
            }
            let robustZ: Double? = {
                guard let localMedianLog,
                      let localMADLog,
                      pair.value > 0 else {
                    return nil
                }
                let scale = max(localMADLog * 1.4826, 1e-12)
                return (log(pair.value) - localMedianLog) / scale
            }()

            return SpikeISILocalContextPoint(
                trainID: train.id,
                trainName: train.name,
                isiIndex: pair.index,
                isiSec: pair.value,
                trainPercentile: percentileRank(pair.value, in: sortedTrainValues),
                localPercentile: localValues.isEmpty ? nil : percentileRank(pair.value, in: localValues.sorted()),
                localMedianSec: localMedian,
                localRatio: localRatio,
                localMedianLogSec: localMedianLog,
                localMADLogSec: localMADLog,
                localRobustZ: robustZ
            )
        }

        return SpikeISILocalContextTable(
            trainID: train.id,
            trainName: train.name,
            points: points
        )
    }

    private static func localValues(
        around index: Int,
        valuesByIndex: [Int: Double],
        halfWindow: Int,
        excluding excludedIndices: Set<Int>
    ) -> [Double] {
        let lower = max(1, index - halfWindow)
        let upper = index + halfWindow
        guard lower <= upper else {
            return []
        }
        return (lower...upper).compactMap { candidateIndex in
            guard !excludedIndices.contains(candidateIndex) else {
                return nil
            }
            return valuesByIndex[candidateIndex]
        }
    }

    private static func percentileRank(_ value: Double, in sortedValues: [Double]) -> Double {
        guard !sortedValues.isEmpty else {
            return 0
        }
        let count = sortedValues.filter { $0 <= value + tolerance(for: value) }.count
        return Double(count) / Double(sortedValues.count)
    }

    private static func median(_ values: [Double]) -> Double? {
        guard !values.isEmpty else {
            return nil
        }
        let sorted = values.sorted()
        let middle = sorted.count / 2
        if sorted.count.isMultiple(of: 2) {
            return (sorted[middle - 1] + sorted[middle]) / 2
        }
        return sorted[middle]
    }

    private static func tolerance(for value: Double) -> Double {
        max(1e-12, abs(value) * 1e-6)
    }
}
