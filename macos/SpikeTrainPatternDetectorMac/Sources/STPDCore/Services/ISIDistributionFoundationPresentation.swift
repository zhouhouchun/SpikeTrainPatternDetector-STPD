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
    public let lowerSec: Double
    public let upperSec: Double
    public let bridgeUpperSec: Double?
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
        self.isValid = interval.isValid
        self.provenanceOrigin = interval.provenance.origin
        self.sourceStatistic = interval.provenance.sourceStatistic
        self.mayPropagateToDataset = interval.provenance.mayPropagateToDataset
        self.maySelectFinalLabel = interval.provenance.maySelectFinalLabel
        self.isAuditOnly = interval.provenance.isAuditOnly
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

    public init(
        datasetName: String, source: Source, floorSec: Double,
        contributingTrainCount: Int, pooledValidISICount: Int,
        pooled: ISIDistributionQuantileRow, trainBalanced: ISIDistributionQuantileRow?,
        perTrain: [ISIDistributionQuantileRow], datasetIntervals: [ISIModeIntervalRow]
    ) {
        self.datasetName = datasetName; self.source = source; self.floorSec = floorSec
        self.contributingTrainCount = contributingTrainCount; self.pooledValidISICount = pooledValidISICount
        self.pooled = pooled; self.trainBalanced = trainBalanced
        self.perTrain = perTrain; self.datasetIntervals = datasetIntervals
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
        let families = derived.dataset
        let intervals = [families.burst, families.tonic, families.pause]
            .compactMap { $0 }
            .map(ISIModeIntervalRow.init)

        return ISIDistributionFoundationPresentation(
            datasetName: distribution.datasetName, source: source, floorSec: floorSec,
            contributingTrainCount: distribution.contributingTrainCount,
            pooledValidISICount: distribution.pooledValidISICount,
            pooled: pooled, trainBalanced: balanced, perTrain: perTrain, datasetIntervals: intervals)
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
