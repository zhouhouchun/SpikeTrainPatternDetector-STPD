import Foundation

public struct StructuralDatasetSeedSummary: Hashable, Sendable {
    public let trainCount: Int
    public let seededTrainCount: Int
    public let burstAnchorCount: Int
    public let burstSupportWeight: Double
    public let burstSeedUpperSec: Double?
    public let burstBridgeUpperSec: Double?
    public let tonicAnchorCount: Int
    public let tonicSupportWeight: Double
    public let tonicSeedLowerSec: Double?
    public let tonicSeedUpperSec: Double?
    public let pauseAnchorCount: Int
    public let pausePoolAnchorCount: Int
    public let pauseSupportWeight: Double
    public let pausePoolSupportWeight: Double
    public let pauseSeedLowerSec: Double?
    public let pauseSeedUpperSec: Double?
    public let pausePoolSource: String
    public let source: String
    /// True when this dataset summary was aggregated over ALL trains with no
    /// leave-one-out exclusion, i.e. the prior it provides for any train includes
    /// that train's own evidence. Provenance only; no detection/aggregation logic
    /// reads this field, so it does not affect behavior.
    public let isSelfInclusive: Bool

    public init(
        trainCount: Int = 0,
        seededTrainCount: Int = 0,
        burstAnchorCount: Int = 0,
        burstSupportWeight: Double? = nil,
        burstSeedUpperSec: Double? = nil,
        burstBridgeUpperSec: Double? = nil,
        tonicAnchorCount: Int = 0,
        tonicSupportWeight: Double? = nil,
        tonicSeedLowerSec: Double? = nil,
        tonicSeedUpperSec: Double? = nil,
        pauseAnchorCount: Int = 0,
        pausePoolAnchorCount: Int = 0,
        pauseSupportWeight: Double? = nil,
        pausePoolSupportWeight: Double? = nil,
        pauseSeedLowerSec: Double? = nil,
        pauseSeedUpperSec: Double? = nil,
        pausePoolSource: String = "none",
        source: String = "none",
        isSelfInclusive: Bool = false
    ) {
        self.trainCount = max(0, trainCount)
        self.seededTrainCount = max(0, seededTrainCount)
        self.burstAnchorCount = max(0, burstAnchorCount)
        self.burstSupportWeight = Self.cleanWeight(
            burstSupportWeight ?? Self.supportWeight(anchorCount: burstAnchorCount, shrinkage: 4)
        )
        self.burstSeedUpperSec = burstSeedUpperSec.flatMap { $0.isFinite && $0 > 0 ? $0 : nil }
        self.burstBridgeUpperSec = burstBridgeUpperSec.flatMap { $0.isFinite && $0 > 0 ? $0 : nil }
        self.tonicAnchorCount = max(0, tonicAnchorCount)
        self.tonicSupportWeight = Self.cleanWeight(
            tonicSupportWeight ?? Self.supportWeight(anchorCount: tonicAnchorCount, shrinkage: 4)
        )
        self.tonicSeedLowerSec = tonicSeedLowerSec.flatMap { $0.isFinite && $0 > 0 ? $0 : nil }
        self.tonicSeedUpperSec = tonicSeedUpperSec.flatMap { $0.isFinite && $0 > 0 ? $0 : nil }
        self.pauseAnchorCount = max(0, pauseAnchorCount)
        self.pausePoolAnchorCount = max(0, pausePoolAnchorCount)
        self.pauseSupportWeight = Self.cleanWeight(
            pauseSupportWeight ?? Self.supportWeight(
                anchorCount: pauseAnchorCount + pausePoolAnchorCount,
                shrinkage: 3
            )
        )
        self.pausePoolSupportWeight = Self.cleanWeight(
            pausePoolSupportWeight ?? Self.supportWeight(anchorCount: pausePoolAnchorCount, shrinkage: 3)
        )
        self.pauseSeedLowerSec = pauseSeedLowerSec.flatMap { $0.isFinite && $0 > 0 ? $0 : nil }
        self.pauseSeedUpperSec = pauseSeedUpperSec.flatMap { $0.isFinite && $0 > 0 ? $0 : nil }
        self.pausePoolSource = pausePoolSource
        self.source = source
        self.isSelfInclusive = isSelfInclusive
    }

    public static let empty = StructuralDatasetSeedSummary()

    public var hasAnyAnchor: Bool {
        burstAnchorCount > 0 || tonicAnchorCount > 0 || pauseAnchorCount > 0 || pausePoolAnchorCount > 0
    }

    private static func supportWeight(anchorCount: Int, shrinkage: Double) -> Double {
        let count = Double(max(0, anchorCount))
        let lambda = max(shrinkage, 1)
        return count / (count + lambda)
    }

    private static func cleanWeight(_ value: Double) -> Double {
        guard value.isFinite else {
            return 0
        }
        return min(1, max(0, value))
    }
}

public enum StructuralDatasetSeedAggregator {
    public static func aggregate(
        resolutions: [TrainAdaptiveBandResolution],
        excluding excludedTrainID: String? = nil
    ) -> StructuralDatasetSeedSummary {
        // Firewall: only train-local structural evidence is eligible for dataset seed
        // aggregation. Dataset-applied summaries are excluded so derived/applied priors
        // cannot re-enter the aggregate, and the optional target train is excluded for
        // leave-one-out (so a train never contributes to the prior applied back to it).
        let eligible = resolutions.filter { resolution in
            resolution.structuralSeedSummary.origin == .trainLocal
                && resolution.trainID != excludedTrainID
        }
        let selfInclusive = (excludedTrainID == nil)
        let summaries = eligible.map(\.structuralSeedSummary)
        let seededSummaries = summaries.filter(\.hasAnyAnchor)
        guard !seededSummaries.isEmpty else {
            return StructuralDatasetSeedSummary(trainCount: eligible.count, isSelfInclusive: selfInclusive)
        }

        // Phase 2A dataset-aggregation firewall: only bands whose train-local family is
        // dataset-aggregatable feed the dataset quantile merge. Weak-derived burst seeds,
        // structural-window tonic fills, and audit-only pause priors are excluded here and
        // stay train-local. The origin/leave-one-out firewall above is unchanged; this is a
        // per-family strength/conservation gate applied on top of it. Anchor counts and
        // support weights below are intentionally left over all seeded summaries (metadata;
        // strength-weighted merge is a deferred refinement).
        let burstSeededSummaries = seededSummaries.filter(\.isBurstSeedDatasetAggregatable)
        let tonicSeededSummaries = seededSummaries.filter(\.isTonicSeedDatasetAggregatable)
        let pauseSeededSummaries = seededSummaries.filter(\.isPauseSeedDatasetAggregatable)
        let burstSeedUpperValues = burstSeededSummaries.compactMap(\.burstSeedUpperSec)
        let burstBridgeValues = burstSeededSummaries.compactMap(\.burstBridgeUpperSec)
        let tonicLowerValues = tonicSeededSummaries.compactMap(\.tonicSeedLowerSec)
        let tonicUpperValues = tonicSeededSummaries.compactMap(\.tonicSeedUpperSec)
        let pauseLowerValues = pauseSeededSummaries.compactMap(\.pauseSeedLowerSec)
        let pauseUpperValues = pauseSeededSummaries.compactMap(\.pauseSeedUpperSec)
        let ordered = orderedSeedValues(
            burstSeedUpperSec: quantile(burstSeedUpperValues, probability: 0.50),
            burstBridgeUpperSec: quantile(burstBridgeValues, probability: 0.75),
            tonicSeedLowerSec: quantile(tonicLowerValues, probability: 0.20),
            tonicSeedUpperSec: quantile(tonicUpperValues, probability: 0.80),
            pauseSeedLowerSec: quantile(pauseLowerValues, probability: 0.20),
            pauseSeedUpperSec: quantile(pauseUpperValues, probability: 0.80)
        )

        return StructuralDatasetSeedSummary(
            trainCount: eligible.count,
            seededTrainCount: seededSummaries.count,
            burstAnchorCount: seededSummaries.reduce(0) { $0 + $1.burstAnchorCount },
            burstSupportWeight: meanWeight(seededSummaries.map(\.burstSupportWeight)),
            burstSeedUpperSec: ordered.burstSeedUpperSec,
            burstBridgeUpperSec: ordered.burstBridgeUpperSec,
            tonicAnchorCount: seededSummaries.reduce(0) { $0 + $1.tonicAnchorCount },
            tonicSupportWeight: meanWeight(seededSummaries.map(\.tonicSupportWeight)),
            tonicSeedLowerSec: ordered.tonicSeedLowerSec,
            tonicSeedUpperSec: ordered.tonicSeedUpperSec,
            pauseAnchorCount: seededSummaries.reduce(0) { $0 + $1.pauseAnchorCount },
            pausePoolAnchorCount: seededSummaries.reduce(0) { $0 + $1.pausePoolAnchorCount },
            pauseSupportWeight: meanWeight(seededSummaries.map(\.pauseSupportWeight)),
            pausePoolSupportWeight: meanWeight(seededSummaries.map(\.pausePoolSupportWeight)),
            pauseSeedLowerSec: ordered.pauseSeedLowerSec,
            pauseSeedUpperSec: ordered.pauseSeedUpperSec,
            pausePoolSource: combinedPausePoolSource(seededSummaries),
            source: ordered.orderingAdjusted
                ? "structural_dataset_seed_aggregate_with_nonoverlap_ordering_audit"
                : "structural_dataset_seed_aggregate",
            isSelfInclusive: selfInclusive
        )
    }

    private static func orderedSeedValues(
        burstSeedUpperSec: Double?,
        burstBridgeUpperSec: Double?,
        tonicSeedLowerSec: Double?,
        tonicSeedUpperSec: Double?,
        pauseSeedLowerSec: Double?,
        pauseSeedUpperSec: Double?
    ) -> (
        burstSeedUpperSec: Double?,
        burstBridgeUpperSec: Double?,
        tonicSeedLowerSec: Double?,
        tonicSeedUpperSec: Double?,
        pauseSeedLowerSec: Double?,
        pauseSeedUpperSec: Double?,
        orderingAdjusted: Bool
    ) {
        var orderingAdjusted = false
        if let lhs = burstSeedUpperSec, let rhs = tonicSeedLowerSec, lhs > rhs {
            orderingAdjusted = true
        }
        if let lhs = tonicSeedUpperSec, let rhs = pauseSeedLowerSec, lhs > rhs {
            orderingAdjusted = true
        }
        if let lhs = burstSeedUpperSec, let rhs = pauseSeedLowerSec, lhs > rhs {
            orderingAdjusted = true
        }
        if let bridge = burstBridgeUpperSec, let floor = pauseSeedLowerSec, bridge >= floor {
            orderingAdjusted = true
        }

        return (
            burstSeedUpperSec: burstSeedUpperSec,
            burstBridgeUpperSec: burstBridgeUpperSec,
            tonicSeedLowerSec: tonicSeedLowerSec,
            tonicSeedUpperSec: tonicSeedUpperSec,
            pauseSeedLowerSec: pauseSeedLowerSec,
            pauseSeedUpperSec: pauseSeedUpperSec,
            orderingAdjusted: orderingAdjusted
        )
    }

    private static func meanWeight(_ values: [Double]) -> Double? {
        let cleanValues = values.filter(\.isFinite).map { min(1, max(0, $0)) }
        guard !cleanValues.isEmpty else {
            return nil
        }
        return cleanValues.reduce(0, +) / Double(cleanValues.count)
    }

    private static func combinedPausePoolSource(_ summaries: [StructuralSeedBandSummary]) -> String {
        let sources = Set(
            summaries
                .filter { $0.pausePoolAnchorCount > 0 }
                .map(\.pausePoolSource)
                .filter { $0 != "none" }
        )
        guard !sources.isEmpty else {
            return "none"
        }
        if sources.contains("classic_and_possible_burst_flank_pause_pool") ||
            (
                sources.contains("classic_burst_flank_pause_pool") &&
                    sources.contains("possible_burst_flank_pause_prior_pool")
            ) {
            return "classic_and_possible_burst_flank_pause_pool"
        }
        if sources.contains("possible_burst_flank_pause_prior_pool") {
            return "possible_burst_flank_pause_prior_pool"
        }
        if sources.contains("classic_burst_flank_pause_pool") {
            return "classic_burst_flank_pause_pool"
        }
        return "structural_pause_prior_pool"
    }

    private static func quantile(_ values: [Double], probability: Double) -> Double? {
        let sorted = values.filter(\.isFinite).filter { $0 > 0 }.sorted()
        guard !sorted.isEmpty else {
            return nil
        }
        guard sorted.count > 1 else {
            return sorted[0]
        }
        let p = min(max(probability, 0), 1)
        let h = 1 + (Double(sorted.count) - 1) * p
        let lowerIndex = max(0, Int(floor(h)) - 1)
        let upperIndex = max(0, Int(ceil(h)) - 1)
        let lowerValue = sorted[min(lowerIndex, sorted.count - 1)]
        let upperValue = sorted[min(upperIndex, sorted.count - 1)]
        return lowerValue + (h - floor(h)) * (upperValue - lowerValue)
    }
}
