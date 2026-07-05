import Foundation

// MARK: - Distribution-first foundation (D1-D3) — UI presentation model
//
// A PURE, view-agnostic data-shaping layer that turns the distribution-first foundation
// (`DatasetISIDistribution` from D1/D2 + `ModeISIIntervalDeriver` priors from D3) into flat rows a
// SwiftUI view can render directly. Lives in STPDCore (not the app target) so it is unit-testable in
// isolation. It does NOT touch detectors, the pipeline, or the old `DatasetISIHistogram` — it is the
// distribution-first counterpart, clearly separate.

/// One quantile summary row (pooled, train-balanced, or a single train). Seconds are raw; the view
/// formats units.
public struct ISIDistributionQuantileRow: Hashable, Sendable {
    public let label: String
    public let trainID: String?
    public let validISICount: Int
    public let minSec: Double?
    public let q10Sec: Double?
    public let q25Sec: Double?
    public let q50Sec: Double?
    public let q75Sec: Double?
    public let q90Sec: Double?
    public let maxSec: Double?
    public let meanSec: Double?

    public init(
        label: String, trainID: String?, validISICount: Int,
        minSec: Double?, q10Sec: Double?, q25Sec: Double?, q50Sec: Double?,
        q75Sec: Double?, q90Sec: Double?, maxSec: Double?, meanSec: Double?
    ) {
        self.label = label; self.trainID = trainID; self.validISICount = validISICount
        self.minSec = minSec; self.q10Sec = q10Sec; self.q25Sec = q25Sec; self.q50Sec = q50Sec
        self.q75Sec = q75Sec; self.q90Sec = q90Sec; self.maxSec = maxSec; self.meanSec = meanSec
    }

    /// Build a row from a D1 `ISIQuantiles` bundle.
    public static func from(label: String, trainID: String?, count: Int, quantiles: ISIQuantiles) -> ISIDistributionQuantileRow {
        ISIDistributionQuantileRow(
            label: label, trainID: trainID, validISICount: count,
            minSec: quantiles.minSec, q10Sec: quantiles.q10, q25Sec: quantiles.q25, q50Sec: quantiles.q50,
            q75Sec: quantiles.q75, q90Sec: quantiles.q90, maxSec: quantiles.maxSec, meanSec: quantiles.meanSec)
    }
}

/// One D3-derived `ModeISIInterval` prior, flattened with its provenance permissions for display.
public struct ISIModeIntervalRow: Hashable, Sendable {
    public let family: ISIPatternFamily
    public let scope: ISIDistributionScope
    /// Core interval bounds (e.g. tonic q25-q75).
    public let lowerSec: Double
    public let upperSec: Double
    public let bridgeUpperSec: Double?
    /// Wider acceptance-band bounds (membership band); nil when equal to the core.
    public let acceptanceLowerSec: Double?
    public let acceptanceUpperSec: Double?
    public let isValid: Bool
    public let provenanceOrigin: IntervalOrigin
    public let sourceStatistic: String
    public let mayPropagateToDataset: Bool
    public let maySelectFinalLabel: Bool
    public let isAuditOnly: Bool

    public init(_ interval: ModeISIInterval) {
        self.family = interval.family
        self.scope = interval.scope
        self.lowerSec = interval.lowerSec
        self.upperSec = interval.upperSec
        self.bridgeUpperSec = interval.bridgeUpperSec
        self.acceptanceLowerSec = interval.acceptanceLowerSec
        self.acceptanceUpperSec = interval.acceptanceUpperSec
        self.isValid = interval.isValid
        self.provenanceOrigin = interval.provenance.origin
        self.sourceStatistic = interval.provenance.sourceStatistic
        self.mayPropagateToDataset = interval.provenance.mayPropagateToDataset
        self.maySelectFinalLabel = interval.provenance.maySelectFinalLabel
        self.isAuditOnly = interval.provenance.isAuditOnly
    }
}

/// One histogram bar over an ISI range `[lowerSec, upperSec)`.
public struct ISIHistogramBin: Hashable, Sendable {
    public let lowerSec: Double
    public let upperSec: Double
    public let count: Int

    public init(lowerSec: Double, upperSec: Double, count: Int) {
        self.lowerSec = lowerSec
        self.upperSec = upperSec
        self.count = count
    }
}

/// A LOG-spaced histogram of pooled valid ISIs — the drawable distribution for the foundation chart
/// (ISIs span burst≈ms to pause≈100s of ms, so a log ISI axis is the natural view).
public struct ISIDistributionHistogram: Hashable, Sendable {
    public let bins: [ISIHistogramBin]
    public let totalCount: Int
    public let maxCount: Int
    public let minSec: Double   // domain lower (first bin lower)
    public let maxSec: Double   // domain upper (last bin upper)

    public init(bins: [ISIHistogramBin], totalCount: Int, maxCount: Int, minSec: Double, maxSec: Double) {
        self.bins = bins
        self.totalCount = totalCount
        self.maxCount = maxCount
        self.minSec = minSec
        self.maxSec = maxSec
    }

    /// Build a log-spaced histogram from valid (finite, positive) ISI values. Returns nil when there
    /// are fewer than two positive values or the range is degenerate (min == max).
    public static func logScale(values: [Double], binCount: Int = 40) -> ISIDistributionHistogram? {
        let positive = values.filter { $0.isFinite && $0 > 0 }
        guard positive.count >= 2, let minSec = positive.min(), let maxSec = positive.max(), maxSec > minSec else {
            return nil
        }
        let bins = max(1, binCount)
        let logMin = log10(minSec)
        let logMax = log10(maxSec)
        let step = (logMax - logMin) / Double(bins)
        var counts = [Int](repeating: 0, count: bins)
        for value in positive {
            let idx = min(bins - 1, max(0, Int((log10(value) - logMin) / step)))
            counts[idx] += 1
        }
        let histBins = (0..<bins).map { index in
            ISIHistogramBin(
                lowerSec: pow(10, logMin + Double(index) * step),
                upperSec: pow(10, logMin + Double(index + 1) * step),
                count: counts[index])
        }
        return ISIDistributionHistogram(
            bins: histBins, totalCount: positive.count, maxCount: counts.max() ?? 0,
            minSec: minSec, maxSec: maxSec)
    }

    /// Bin `values` into THIS histogram's existing (log) bin edges — for overlaying a single train's
    /// distribution on the pooled histogram with aligned bars. Returns one count per bin.
    public func counts(for values: [Double]) -> [Int] {
        let binCount = bins.count
        guard binCount > 0, maxSec > minSec else { return [Int](repeating: 0, count: binCount) }
        let logMin = log10(minSec)
        let step = (log10(maxSec) - logMin) / Double(binCount)
        var result = [Int](repeating: 0, count: binCount)
        for value in values where value.isFinite && value > 0 {
            let idx = min(binCount - 1, max(0, Int((log10(value) - logMin) / step)))
            result[idx] += 1
        }
        return result
    }
}

/// Per-train detail for the selected-train overlay: identity, counts binned onto the pooled histogram,
/// and the train-local D3 interval priors.
public struct ISIDistributionTrainDetail: Hashable, Sendable {
    public let trainID: String
    public let trainName: String
    public let validISICount: Int
    /// Per-train counts aligned to the POOLED histogram bins (same edges → overlay-aligned); empty when
    /// there is no pooled histogram.
    public let histogramCounts: [Int]
    /// Train-local D3 interval priors (burst, tonic, pause — only those present).
    public let intervals: [ISIModeIntervalRow]

    public init(trainID: String, trainName: String, validISICount: Int, histogramCounts: [Int], intervals: [ISIModeIntervalRow]) {
        self.trainID = trainID
        self.trainName = trainName
        self.validISICount = validISICount
        self.histogramCounts = histogramCounts
        self.intervals = intervals
    }
}

/// Everything the "Distribution-first foundation (D1-D3)" view needs, already shaped.
public struct ISIDistributionFoundationPresentation: Hashable, Sendable {
    /// Whether the distribution came from the detection run (D2-wired) or a fallback compute.
    public enum Source: String, Hashable, Sendable {
        case detectionRun          // read from run.datasetISIDistribution (band floor)
        case computedFromDataset   // fallback: DatasetISIDistributionService.compute
    }

    public let datasetName: String
    public let source: Source
    public let floorSec: Double
    public let contributingTrainCount: Int
    public let pooledValidISICount: Int
    public let pooled: ISIDistributionQuantileRow
    public let trainBalanced: ISIDistributionQuantileRow?
    public let perTrain: [ISIDistributionQuantileRow]
    /// Dataset-scope D3 priors (burst, then tonic, then pause — only those present).
    public let datasetIntervals: [ISIModeIntervalRow]
    /// Log-ISI histogram of the pooled valid ISIs (nil when too few / degenerate).
    public let histogram: ISIDistributionHistogram?
    /// Per-train overlay details (counts aligned to the pooled histogram + train-local D3 priors), one
    /// per contributing train, in `perTrain` order.
    public let perTrainDetails: [ISIDistributionTrainDetail]

    public init(
        datasetName: String, source: Source, floorSec: Double,
        contributingTrainCount: Int, pooledValidISICount: Int,
        pooled: ISIDistributionQuantileRow, trainBalanced: ISIDistributionQuantileRow?,
        perTrain: [ISIDistributionQuantileRow], datasetIntervals: [ISIModeIntervalRow],
        histogram: ISIDistributionHistogram? = nil,
        perTrainDetails: [ISIDistributionTrainDetail] = []
    ) {
        self.datasetName = datasetName; self.source = source; self.floorSec = floorSec
        self.contributingTrainCount = contributingTrainCount; self.pooledValidISICount = pooledValidISICount
        self.pooled = pooled; self.trainBalanced = trainBalanced
        self.perTrain = perTrain; self.datasetIntervals = datasetIntervals
        self.histogram = histogram
        self.perTrainDetails = perTrainDetails
    }

    /// Flatten a family-interval bundle into display rows (burst, then tonic, then pause — present only).
    private static func intervalRows(_ families: FamilyModeIntervals?) -> [ISIModeIntervalRow] {
        guard let families else { return [] }
        return [families.burst, families.tonic, families.pause].compactMap { $0 }.map(ISIModeIntervalRow.init)
    }

    /// Shape an already-computed distribution + derived intervals into presentation rows.
    public static func make(
        distribution: DatasetISIDistribution,
        derived: DerivedModeISIIntervals,
        source: Source,
        floorSec: Double
    ) -> ISIDistributionFoundationPresentation {
        let pooled = ISIDistributionQuantileRow.from(
            label: "Pooled", trainID: nil, count: distribution.pooledValidISICount,
            quantiles: distribution.pooledQuantiles)
        let balanced = distribution.trainBalancedQuantiles.map {
            ISIDistributionQuantileRow.from(
                label: "Train-balanced", trainID: nil, count: $0.count, quantiles: $0)
        }
        let perTrain = distribution.trainDistributions.map { train in
            ISIDistributionQuantileRow.from(
                label: train.trainName, trainID: train.trainID, count: train.validISICount,
                quantiles: train.quantiles)
        }
        let intervals = intervalRows(derived.dataset)

        let pooledValues = distribution.trainDistributions.flatMap(\.validISIValuesSec)
        let histogram = ISIDistributionHistogram.logScale(values: pooledValues)

        // Per-train overlay details: bin each train's ISIs onto the pooled histogram edges (so bars align)
        // and carry its train-local D3 priors — answering "does this train have a prior the dataset lacks?"
        let perTrainDetails = distribution.trainDistributions.map { train in
            ISIDistributionTrainDetail(
                trainID: train.trainID, trainName: train.trainName, validISICount: train.validISICount,
                histogramCounts: histogram?.counts(for: train.validISIValuesSec) ?? [],
                intervals: intervalRows(derived.perTrain[train.trainID]))
        }

        return ISIDistributionFoundationPresentation(
            datasetName: distribution.datasetName, source: source, floorSec: floorSec,
            contributingTrainCount: distribution.contributingTrainCount,
            pooledValidISICount: distribution.pooledValidISICount,
            pooled: pooled, trainBalanced: balanced, perTrain: perTrain, datasetIntervals: intervals,
            histogram: histogram, perTrainDetails: perTrainDetails)
    }

    /// End-to-end convenience for the view: prefer the D2-wired `runDistribution`; otherwise compute
    /// from the dataset at `minimumValidISISec`. Then derive D3 priors and shape everything.
    public static func from(
        dataset: SpikeDataset,
        runDistribution: DatasetISIDistribution?,
        minimumValidISISec: Double
    ) -> ISIDistributionFoundationPresentation {
        let distribution = runDistribution
            ?? DatasetISIDistributionService.compute(dataset: dataset, minimumValidISISec: minimumValidISISec)
        let derived = ModeISIIntervalDeriver.derive(datasetDistribution: distribution, minimumValidISISec: minimumValidISISec)
        let source: Source = runDistribution != nil ? .detectionRun : .computedFromDataset
        return make(distribution: distribution, derived: derived, source: source, floorSec: minimumValidISISec)
    }
}
