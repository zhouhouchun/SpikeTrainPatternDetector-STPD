import Foundation

public struct DatasetISIHistogramBin: Identifiable, Hashable, Sendable {
    public let id: Int
    public let binLeftSec: Double
    public let binRightSec: Double
    public let rawCount: Int
    public let rawFraction: Double
    public let trainBalancedFraction: Double

    public init(
        id: Int,
        binLeftSec: Double,
        binRightSec: Double,
        rawCount: Int,
        rawFraction: Double,
        trainBalancedFraction: Double
    ) {
        self.id = id
        self.binLeftSec = binLeftSec
        self.binRightSec = binRightSec
        self.rawCount = rawCount
        self.rawFraction = rawFraction
        self.trainBalancedFraction = trainBalancedFraction
    }

    public var midSec: Double {
        (binLeftSec + binRightSec) / 2
    }
}

public struct DatasetISIHistogramTrainRow: Identifiable, Hashable, Sendable {
    public let id: String
    public let trainName: String
    public let validISICount: Int
    public let visibleISICount: Int
    public let artifactExcludedCount: Int
    public let minISISec: Double?
    public let q10ISISec: Double?
    public let q25ISISec: Double?
    public let medianISISec: Double?
    public let q75ISISec: Double?
    public let q90ISISec: Double?
    public let maxISISec: Double?

    public init(
        id: String,
        trainName: String,
        validISICount: Int,
        visibleISICount: Int,
        artifactExcludedCount: Int,
        minISISec: Double?,
        q10ISISec: Double?,
        q25ISISec: Double?,
        medianISISec: Double?,
        q75ISISec: Double?,
        q90ISISec: Double?,
        maxISISec: Double?
    ) {
        self.id = id
        self.trainName = trainName
        self.validISICount = validISICount
        self.visibleISICount = visibleISICount
        self.artifactExcludedCount = artifactExcludedCount
        self.minISISec = minISISec
        self.q10ISISec = q10ISISec
        self.q25ISISec = q25ISISec
        self.medianISISec = medianISISec
        self.q75ISISec = q75ISISec
        self.q90ISISec = q90ISISec
        self.maxISISec = maxISISec
    }
}

public struct DatasetISIHistogramSummary: Hashable, Sendable {
    public let datasetName: String
    public let sourceDescription: String
    public let binWidthSec: Double
    public let xMaxSec: Double
    public let totalValidISICount: Int
    public let visibleValidISICount: Int
    public let contributingTrainCount: Int
    public let artifactExcludedCount: Int
    public let bins: [DatasetISIHistogramBin]
    public let trainRows: [DatasetISIHistogramTrainRow]

    public init(
        datasetName: String,
        sourceDescription: String,
        binWidthSec: Double,
        xMaxSec: Double,
        totalValidISICount: Int,
        visibleValidISICount: Int,
        contributingTrainCount: Int,
        artifactExcludedCount: Int,
        bins: [DatasetISIHistogramBin],
        trainRows: [DatasetISIHistogramTrainRow]
    ) {
        self.datasetName = datasetName
        self.sourceDescription = sourceDescription
        self.binWidthSec = binWidthSec
        self.xMaxSec = xMaxSec
        self.totalValidISICount = totalValidISICount
        self.visibleValidISICount = visibleValidISICount
        self.contributingTrainCount = contributingTrainCount
        self.artifactExcludedCount = artifactExcludedCount
        self.bins = bins
        self.trainRows = trainRows
    }

    public var maxRawCount: Int {
        bins.map(\.rawCount).max() ?? 0
    }

    public var maxFraction: Double {
        bins.map { max($0.rawFraction, $0.trainBalancedFraction) }.max() ?? 0
    }
}

public enum DatasetISIHistogram {
    public static func summarize(
        dataset: SpikeDataset,
        qualitySettings: SpikeQualitySettings,
        binWidthSec rawBinWidthSec: Double,
        xMaxSec requestedXMaxSec: Double? = nil
    ) -> DatasetISIHistogramSummary {
        let binWidthSec = rawBinWidthSec.isFinite && rawBinWidthSec > 0 ? rawBinWidthSec : 0.005
        let validByTrain = validISIsByTrain(dataset: dataset, artifactThresholdSec: qualitySettings.artifactThresholdSec)
        let allValid = validByTrain.flatMap(\.values)
        let xMaxSec = resolvedXMaxSec(
            requested: requestedXMaxSec,
            values: allValid,
            binWidthSec: binWidthSec
        )
        let binCount = max(1, Int(ceil(xMaxSec / binWidthSec)))
        let effectiveXMaxSec = Double(binCount) * binWidthSec
        var rawCounts = Array(repeating: 0, count: binCount)
        var balancedFractions = Array(repeating: 0.0, count: binCount)

        for value in allValid where value <= effectiveXMaxSec {
            rawCounts[binIndex(for: value, binWidthSec: binWidthSec, binCount: binCount)] += 1
        }

        for train in validByTrain where !train.values.isEmpty {
            var trainCounts = Array(repeating: 0, count: binCount)
            for value in train.values where value <= effectiveXMaxSec {
                trainCounts[binIndex(for: value, binWidthSec: binWidthSec, binCount: binCount)] += 1
            }
            let denominator = Double(train.values.count)
            for index in trainCounts.indices {
                balancedFractions[index] += Double(trainCounts[index]) / denominator
            }
        }

        let contributingTrainCount = validByTrain.filter { !$0.values.isEmpty }.count
        if contributingTrainCount > 0 {
            for index in balancedFractions.indices {
                balancedFractions[index] /= Double(contributingTrainCount)
            }
        }

        let denominator = Double(max(allValid.count, 1))
        let bins = rawCounts.indices.map { index in
            let left = Double(index) * binWidthSec
            let right = Double(index + 1) * binWidthSec
            return DatasetISIHistogramBin(
                id: index,
                binLeftSec: left,
                binRightSec: right,
                rawCount: rawCounts[index],
                rawFraction: Double(rawCounts[index]) / denominator,
                trainBalancedFraction: balancedFractions[index]
            )
        }

        let rows = validByTrain.map { train in
            trainRow(
                train: train,
                xMaxSec: effectiveXMaxSec
            )
        }

        return DatasetISIHistogramSummary(
            datasetName: dataset.name,
            sourceDescription: dataset.sourceDescription,
            binWidthSec: binWidthSec,
            xMaxSec: effectiveXMaxSec,
            totalValidISICount: allValid.count,
            visibleValidISICount: allValid.filter { $0 <= effectiveXMaxSec }.count,
            contributingTrainCount: contributingTrainCount,
            artifactExcludedCount: validByTrain.reduce(0) { $0 + $1.artifactExcludedCount },
            bins: bins,
            trainRows: rows
        )
    }

    private static func validISIsByTrain(
        dataset: SpikeDataset,
        artifactThresholdSec: Double
    ) -> [(id: String, name: String, values: [Double], artifactExcludedCount: Int)] {
        let minValidSec = max(0, artifactThresholdSec)
        return dataset.trains.map { train in
            var values: [Double] = []
            values.reserveCapacity(max(0, train.isiSec.count - 1))
            var artifactExcludedCount = 0

            for isi in train.isiSec.compactMap({ $0 }) where isi.isFinite {
                if isi < minValidSec {
                    artifactExcludedCount += 1
                } else {
                    values.append(isi)
                }
            }

            return (
                id: train.id,
                name: train.name,
                values: values,
                artifactExcludedCount: artifactExcludedCount
            )
        }
    }

    private static func trainRow(
        train: (id: String, name: String, values: [Double], artifactExcludedCount: Int),
        xMaxSec: Double
    ) -> DatasetISIHistogramTrainRow {
        let sample = SortedFiniteSample(train.values)
        return DatasetISIHistogramTrainRow(
            id: train.id,
            trainName: train.name,
            validISICount: train.values.count,
            visibleISICount: train.values.filter { $0 <= xMaxSec }.count,
            artifactExcludedCount: train.artifactExcludedCount,
            minISISec: sample.values.first,
            q10ISISec: sample.quantile(0.10),
            q25ISISec: sample.quantile(0.25),
            medianISISec: sample.quantile(0.50),
            q75ISISec: sample.quantile(0.75),
            q90ISISec: sample.quantile(0.90),
            maxISISec: sample.values.last
        )
    }

    private static func resolvedXMaxSec(
        requested: Double?,
        values: [Double],
        binWidthSec: Double
    ) -> Double {
        if let requested,
           requested.isFinite,
           requested > 0 {
            return max(requested, binWidthSec)
        }

        let sample = SortedFiniteSample(values)
        let quantile = sample.quantile(0.995) ?? sample.values.last ?? binWidthSec
        return max(quantile, binWidthSec)
    }

    private static func binIndex(for value: Double, binWidthSec: Double, binCount: Int) -> Int {
        guard value.isFinite, binWidthSec > 0, binCount > 0 else {
            return 0
        }

        return min(max(Int(floor(value / binWidthSec)), 0), binCount - 1)
    }
}
