import Foundation

// MARK: - Phase D1 — distribution-first ISI interval foundation (core value types only)
//
// Pure, deterministic value types for the distribution-first detector architecture. NO detector is
// wired to these yet (Phase D5). They are designed so:
//   • Phase D2 can compute them from the existing QC-filtered ISI paths (finite + >= artifact floor),
//     reusing the shared `SortedFiniteSample` quantile convention (type-7 interpolation) so the
//     numbers match `DatasetISIHistogram` / `TrainAdaptiveBandResolver`;
//   • Phase D5 can let tonic/burst/pause detectors consume `ModeISIInterval` instead of recomputing
//     ad-hoc bounds (`tonicStructuralSearchBounds`, `histogramSuggestion`, pause floor).
// Everything is empty/single/non-finite safe: quantiles are `nil` when there is no data; invalid
// intervals are marked, never crash.

// MARK: ISIQuantiles

/// A reusable quantile bundle over a set of ISI values. Quantiles are `nil` when empty. Computed via
/// the shared `SortedFiniteSample` (finite-filtered, sorted, type-7 interpolation).
public struct ISIQuantiles: Hashable, Sendable {
    /// Number of values behind these quantiles. For pooled statistics this is the total valid ISI
    /// count; for train-balanced statistics (see `balancedAverage`) it is the number of contributing
    /// trains — the two are intentionally different so pooled vs balanced are never conflated.
    public let count: Int
    public let minSec: Double?
    public let q005: Double?
    public let q01: Double?
    public let q05: Double?
    public let q10: Double?
    public let q25: Double?
    public let q50: Double?
    public let q75: Double?
    public let q90: Double?
    public let q95: Double?
    public let q99: Double?
    public let maxSec: Double?
    public let meanSec: Double?

    public init(
        count: Int,
        minSec: Double?, q005: Double?, q01: Double?, q05: Double?, q10: Double?, q25: Double?,
        q50: Double?, q75: Double?, q90: Double?, q95: Double?, q99: Double?, maxSec: Double?,
        meanSec: Double?
    ) {
        self.count = max(0, count)
        self.minSec = minSec; self.q005 = q005; self.q01 = q01; self.q05 = q05; self.q10 = q10
        self.q25 = q25; self.q50 = q50; self.q75 = q75; self.q90 = q90; self.q95 = q95
        self.q99 = q99; self.maxSec = maxSec; self.meanSec = meanSec
    }

    /// Empty (no data). All quantiles nil.
    public static let empty = ISIQuantiles(
        count: 0, minSec: nil, q005: nil, q01: nil, q05: nil, q10: nil, q25: nil,
        q50: nil, q75: nil, q90: nil, q95: nil, q99: nil, maxSec: nil, meanSec: nil
    )

    /// Compute quantiles from raw ISI values (non-finite values are dropped by `SortedFiniteSample`).
    public static func compute(from values: [Double]) -> ISIQuantiles {
        let sample = SortedFiniteSample(values)
        guard !sample.isEmpty else { return .empty }
        let finite = values.filter(\.isFinite)
        let mean = finite.isEmpty ? nil : finite.reduce(0, +) / Double(finite.count)
        return ISIQuantiles(
            count: sample.count,
            minSec: sample.quantile(0.0), q005: sample.quantile(0.005), q01: sample.quantile(0.01),
            q05: sample.quantile(0.05), q10: sample.quantile(0.10), q25: sample.quantile(0.25),
            q50: sample.quantile(0.50), q75: sample.quantile(0.75), q90: sample.quantile(0.90),
            q95: sample.quantile(0.95), q99: sample.quantile(0.99), maxSec: sample.quantile(1.0),
            meanSec: mean
        )
    }

    /// Train-balanced average: each train contributes equally (mean of each per-train quantile,
    /// ignoring nils). `count` becomes the number of contributing trains. Returns nil if no train
    /// has data.
    public static func balancedAverage(_ perTrain: [ISIQuantiles]) -> ISIQuantiles? {
        let contributing = perTrain.filter { $0.count > 0 }
        guard !contributing.isEmpty else { return nil }
        func avg(_ kp: KeyPath<ISIQuantiles, Double?>) -> Double? {
            let vals = contributing.compactMap { $0[keyPath: kp] }
            return vals.isEmpty ? nil : vals.reduce(0, +) / Double(vals.count)
        }
        return ISIQuantiles(
            count: contributing.count,
            minSec: avg(\.minSec), q005: avg(\.q005), q01: avg(\.q01), q05: avg(\.q05),
            q10: avg(\.q10), q25: avg(\.q25), q50: avg(\.q50), q75: avg(\.q75), q90: avg(\.q90),
            q95: avg(\.q95), q99: avg(\.q99), maxSec: avg(\.maxSec), meanSec: avg(\.meanSec)
        )
    }
}

// MARK: ISIExclusionSummary

/// Structured breakdown of *why* raw ISI entries were dropped during QC. The four buckets are
/// mutually exclusive and partition every excluded entry, so `totalExcluded` is their exact sum.
/// This is strictly richer than the two aggregate counts historically carried on
/// `TrainISIDistribution` (`excludedNonFiniteCount` / `excludedArtifactCount`), which remain as
/// convenience roll-ups over these buckets.
public struct ISIExclusionSummary: Hashable, Sendable {
    /// Entry was a non-leading nil real ISI slot; the structural leading `isiSec[0]` placeholder is
    /// skipped before counting.
    public let nilCount: Int
    /// Entry was present but non-finite (`NaN` / `±infinity`).
    public let nonFiniteCount: Int
    /// Finite but `<= 0` — a negative or zero ISI is physically impossible (ordering / duplicate ts).
    public let nonPositiveCount: Int
    /// Finite and `> 0` but `<` the artifact floor (strict; the floor value itself is KEPT) — a
    /// refractory-scale artifact ISI.
    public let belowArtifactFloorCount: Int

    /// Total dropped entries across all reasons.
    public var totalExcluded: Int {
        nilCount + nonFiniteCount + nonPositiveCount + belowArtifactFloorCount
    }
    /// Legacy roll-up: the two "not a usable number" reasons (`nil` + non-finite).
    public var nonFiniteOrNilCount: Int { nilCount + nonFiniteCount }
    /// Legacy roll-up: the two "below usable magnitude" reasons (non-positive + below artifact floor).
    public var artifactOrNonPositiveCount: Int { nonPositiveCount + belowArtifactFloorCount }

    public static let none = ISIExclusionSummary()

    public init(
        nilCount: Int = 0,
        nonFiniteCount: Int = 0,
        nonPositiveCount: Int = 0,
        belowArtifactFloorCount: Int = 0
    ) {
        self.nilCount = max(0, nilCount)
        self.nonFiniteCount = max(0, nonFiniteCount)
        self.nonPositiveCount = max(0, nonPositiveCount)
        self.belowArtifactFloorCount = max(0, belowArtifactFloorCount)
    }
}

// MARK: TrainISIDistribution

/// Per-train ISI distribution after QC. `validISIValuesSec` is finite, `>= artifact floor` (inclusive),
/// sorted. The structural leading placeholder (`SpikeTrain.isiSec[0]`) is never counted.
public struct TrainISIDistribution: Hashable, Sendable {
    public let trainID: String
    public let trainName: String
    public let validISIValuesSec: [Double]
    /// Number of real ISI slots considered (index > 0); excludes the structural leading placeholder.
    public let rawISICount: Int
    /// Structured QC-exclusion breakdown (nil / non-finite / non-positive / below artifact floor).
    public let exclusionSummary: ISIExclusionSummary
    /// Total spikes in the source train, if known (ISIs alone cannot recover it once some are excluded).
    public let spikeCount: Int?
    public let quantiles: ISIQuantiles

    public var validISICount: Int { validISIValuesSec.count }
    /// Legacy roll-up: entries dropped as `nil` or non-finite.
    public var excludedNonFiniteCount: Int { exclusionSummary.nonFiniteOrNilCount }
    /// Legacy roll-up: entries dropped as non-positive or below the artifact floor.
    public var excludedArtifactCount: Int { exclusionSummary.artifactOrNonPositiveCount }

    public init(
        trainID: String,
        trainName: String,
        validISIValuesSec: [Double],
        rawISICount: Int? = nil,
        exclusionSummary: ISIExclusionSummary = .none,
        spikeCount: Int? = nil
    ) {
        self.trainID = trainID
        self.trainName = trainName
        let cleaned = validISIValuesSec.filter(\.isFinite).sorted()
        self.validISIValuesSec = cleaned
        self.rawISICount = rawISICount ?? cleaned.count
        self.exclusionSummary = exclusionSummary
        self.spikeCount = spikeCount
        self.quantiles = .compute(from: cleaned)
    }

    /// Phase D2-ready factory: consumes raw ISIs exactly as `SpikeTrain.isiSec` produces them — a
    /// `[Double?]` whose index-0 element is a STRUCTURAL PLACEHOLDER (there is no ISI before the
    /// first spike), not a data-quality drop.
    ///
    /// For any positive `artifactThresholdSec` this keeps exactly the same set as the canonical
    /// valid-ISI extraction in `SpikeQualityAnalyzer`
    /// (`guard index > 0, value.isFinite, value >= artifactThresholdSec`):
    ///   • index 0 is skipped unconditionally (placeholder — NOT an exclusion, NOT in `rawISICount`);
    ///   • an ISI is kept iff finite and `>= artifactThresholdSec` (INCLUSIVE lower bound);
    ///   • only `value < artifactThresholdSec` (strict) is dropped.
    /// It additionally sorts drops into reason buckets and — matching the codebase's own precedence
    /// (a zero-length ISI is a duplicate timestamp; "duplicate > artifact", see `SpikeISITrace`) —
    /// classifies non-positive ISIs (`value <= 0`) as `nonPositive` rather than `belowArtifactFloor`.
    /// That explicit non-positive check is the ONLY difference from the canonical guard, and it changes
    /// the kept set only under a nonsensical negative threshold (which never occurs in practice).
    /// `rawISICount` is the count of real ISI slots (index > 0), so the summary stays exhaustive:
    /// `exclusionSummary.totalExcluded + validISICount == rawISICount`.
    public static func from(
        trainID: String,
        trainName: String,
        rawISISec: [Double?],
        artifactThresholdSec: Double,
        spikeCount: Int? = nil
    ) -> TrainISIDistribution {
        var valid: [Double] = []
        var realSlotCount = 0
        var nilCount = 0
        var nonFinite = 0
        var nonPositive = 0
        var belowFloor = 0
        for (index, entry) in rawISISec.enumerated() {
            // Index 0 is the structural placeholder (no ISI precedes the first spike): it is not a
            // real ISI slot and never counts as an exclusion.
            if index == 0 { continue }
            realSlotCount += 1
            guard let value = entry else { nilCount += 1; continue }
            guard value.isFinite else { nonFinite += 1; continue }
            if value <= 0 { nonPositive += 1; continue }                    // zero/negative == duplicate ts
            if value < artifactThresholdSec { belowFloor += 1; continue }   // inclusive: keep value >= threshold
            valid.append(value)
        }
        return TrainISIDistribution(
            trainID: trainID, trainName: trainName, validISIValuesSec: valid,
            rawISICount: realSlotCount,
            exclusionSummary: ISIExclusionSummary(
                nilCount: nilCount, nonFiniteCount: nonFinite,
                nonPositiveCount: nonPositive, belowArtifactFloorCount: belowFloor
            ),
            spikeCount: spikeCount
        )
    }
}

// MARK: DatasetISIDistribution

/// Dataset-level ISI distribution with an explicit distinction between POOLED statistics (all valid
/// ISIs concatenated — large trains dominate) and TRAIN-BALANCED statistics (each train weighted
/// equally). Both are exposed so callers must choose deliberately.
public struct DatasetISIDistribution: Hashable, Sendable {
    public let datasetName: String
    public let trainDistributions: [TrainISIDistribution]
    /// Quantiles over ALL valid ISIs concatenated. Large trains dominate.
    public let pooledQuantiles: ISIQuantiles
    /// Mean-of-per-train quantiles. Each train contributes equally. `nil` if no train has data.
    public let trainBalancedQuantiles: ISIQuantiles?

    public var pooledValidISICount: Int { pooledQuantiles.count }
    public var contributingTrainCount: Int { trainDistributions.filter { $0.validISICount > 0 }.count }

    public init(datasetName: String, trainDistributions: [TrainISIDistribution]) {
        self.datasetName = datasetName
        self.trainDistributions = trainDistributions
        self.pooledQuantiles = .compute(from: trainDistributions.flatMap(\.validISIValuesSec))
        self.trainBalancedQuantiles = ISIQuantiles.balancedAverage(trainDistributions.map(\.quantiles))
    }
}

// MARK: Families / scope

public enum ISIPatternFamily: String, Hashable, Sendable, CaseIterable {
    case burst, tonic, pause, unknown
}

/// Which distribution scope an interval was derived from.
public enum ISIDistributionScope: String, Hashable, Sendable {
    case trainLocal, dataset, trainBalanced, manual
}

// MARK: IntervalProvenance

public enum IntervalOrigin: String, Hashable, Sendable {
    case trainLocalDerived
    case datasetSupported
    case datasetPropagated
    case manualAdjusted
    case defaultFallback
}

/// First-class provenance for a derived interval, carrying the governance permissions (propagation,
/// selection, audit-only) that Phase 2A/3A established as scattered flags. Detectors in later phases
/// branch on these instead of re-deriving governance ad hoc.
public struct IntervalProvenance: Hashable, Sendable {
    public let origin: IntervalOrigin
    /// The statistic / rule that produced the interval, e.g. "q90", "histogram_peak", "manual_hard_gate".
    public let sourceStatistic: String
    /// May this interval feed the dataset seed aggregate (cross-train propagation)?
    public let mayPropagateToDataset: Bool
    /// May this interval drive a final auto-selected label?
    public let maySelectFinalLabel: Bool
    /// Is this interval purely diagnostic (never selects, never propagates)?
    public let isAuditOnly: Bool

    public init(
        origin: IntervalOrigin,
        sourceStatistic: String,
        mayPropagateToDataset: Bool,
        maySelectFinalLabel: Bool,
        isAuditOnly: Bool
    ) {
        self.origin = origin
        self.sourceStatistic = sourceStatistic
        self.mayPropagateToDataset = mayPropagateToDataset
        self.maySelectFinalLabel = maySelectFinalLabel
        self.isAuditOnly = isAuditOnly
    }

    /// Train-local evidence: usable for this train's own labels, but does NOT propagate to the dataset
    /// aggregate (matches the Phase 2A firewall for weak/train-local priors).
    public static func trainLocalDerived(sourceStatistic: String) -> IntervalProvenance {
        IntervalProvenance(origin: .trainLocalDerived, sourceStatistic: sourceStatistic,
                           mayPropagateToDataset: false, maySelectFinalLabel: true, isAuditOnly: false)
    }

    /// Dataset-supported strong evidence: eligible to propagate to the dataset aggregate.
    public static func datasetSupported(sourceStatistic: String) -> IntervalProvenance {
        IntervalProvenance(origin: .datasetSupported, sourceStatistic: sourceStatistic,
                           mayPropagateToDataset: true, maySelectFinalLabel: true, isAuditOnly: false)
    }

    /// A prior that was propagated FROM the dataset TO this train: it selects labels here but must not
    /// re-enter the aggregate (matches origin == .datasetApplied firewall).
    public static func datasetPropagated(sourceStatistic: String) -> IntervalProvenance {
        IntervalProvenance(origin: .datasetPropagated, sourceStatistic: sourceStatistic,
                           mayPropagateToDataset: false, maySelectFinalLabel: true, isAuditOnly: false)
    }

    /// User-adjusted interval. Propagation defaults to false (governed by manual scope, decided later).
    public static func manualAdjusted(
        sourceStatistic: String,
        mayPropagateToDataset: Bool = false
    ) -> IntervalProvenance {
        IntervalProvenance(origin: .manualAdjusted, sourceStatistic: sourceStatistic,
                           mayPropagateToDataset: mayPropagateToDataset, maySelectFinalLabel: true,
                           isAuditOnly: false)
    }

    /// Default fallback band (no structural evidence). Selects labels but never propagates.
    public static func defaultFallback(sourceStatistic: String = "default") -> IntervalProvenance {
        IntervalProvenance(origin: .defaultFallback, sourceStatistic: sourceStatistic,
                           mayPropagateToDataset: false, maySelectFinalLabel: true, isAuditOnly: false)
    }

    /// Diagnostic-only: never selects a final label, never propagates.
    public static func auditOnly(origin: IntervalOrigin, sourceStatistic: String) -> IntervalProvenance {
        IntervalProvenance(origin: origin, sourceStatistic: sourceStatistic,
                           mayPropagateToDataset: false, maySelectFinalLabel: false, isAuditOnly: true)
    }
}

// MARK: ModeISIInterval

/// A candidate ISI interval for one pattern family, derived from distribution structure, carrying its
/// scope, provenance, support, and validity.
public struct ModeISIInterval: Hashable, Sendable {
    public let family: ISIPatternFamily
    public let lowerSec: Double
    public let upperSec: Double
    /// Optional burst BRIDGE/extension upper bound. `upperSec` is the compact/core/seed upper; when
    /// present, `bridgeUpperSec` (>= `upperSec`) is the wider bridge extent used by burst. Normally
    /// nil for tonic/pause/unknown.
    public let bridgeUpperSec: Double?
    public let supportCount: Int
    public let supportFraction: Double
    public let scope: ISIDistributionScope
    public let provenance: IntervalProvenance
    public let confidence: Double
    /// True iff lower/upper are finite, `lower >= 0`, and `lower <= upper`. Invalid intervals are
    /// kept but explicitly marked (never crash); use `validated(...)` to reject instead.
    public let isValid: Bool

    public init(
        family: ISIPatternFamily,
        lowerSec: Double,
        upperSec: Double,
        supportCount: Int = 0,
        supportFraction: Double = 0,
        scope: ISIDistributionScope,
        provenance: IntervalProvenance,
        confidence: Double = 0,
        bridgeUpperSec: Double? = nil
    ) {
        self.family = family
        self.lowerSec = lowerSec
        self.upperSec = upperSec
        self.bridgeUpperSec = bridgeUpperSec
        self.supportCount = max(0, supportCount)
        self.supportFraction = supportFraction.isFinite ? min(1, max(0, supportFraction)) : 0
        self.scope = scope
        self.provenance = provenance
        self.confidence = confidence.isFinite ? min(1, max(0, confidence)) : 0
        let coreValid = lowerSec.isFinite && upperSec.isFinite && lowerSec >= 0 && lowerSec <= upperSec
        // A bridge, when present, must be finite and extend at or beyond the core upper.
        let bridgeValid = bridgeUpperSec.map { $0.isFinite && $0 >= upperSec } ?? true
        self.isValid = coreValid && bridgeValid
    }

    /// Failable factory: returns nil for an invalid interval (`lower > upper`, non-finite, negative).
    public static func validated(
        family: ISIPatternFamily,
        lowerSec: Double,
        upperSec: Double,
        supportCount: Int = 0,
        supportFraction: Double = 0,
        scope: ISIDistributionScope,
        provenance: IntervalProvenance,
        confidence: Double = 0,
        bridgeUpperSec: Double? = nil
    ) -> ModeISIInterval? {
        let interval = ModeISIInterval(
            family: family, lowerSec: lowerSec, upperSec: upperSec, supportCount: supportCount,
            supportFraction: supportFraction, scope: scope, provenance: provenance, confidence: confidence,
            bridgeUpperSec: bridgeUpperSec
        )
        return interval.isValid ? interval : nil
    }

    /// Does this interval contain a value (inclusive)? Uses the CORE bounds [lowerSec, upperSec] only.
    /// Always false when invalid.
    public func contains(_ valueSec: Double) -> Bool {
        isValid && valueSec.isFinite && valueSec >= lowerSec && valueSec <= upperSec
    }

    /// Does the value fall within the EXTENDED interval [lowerSec, bridgeUpperSec], falling back to the
    /// core upper when there is no bridge? Always false when invalid.
    public func containsBridge(_ valueSec: Double) -> Bool {
        guard isValid, valueSec.isFinite, valueSec >= lowerSec else { return false }
        return valueSec <= (bridgeUpperSec ?? upperSec)
    }
}

// MARK: ISIOverlapDescriptor

public enum ISIOverlapKind: String, Hashable, Sendable {
    /// Two family modes overlap WITHIN the same train (an arbitration fact for that train).
    case withinTrainModeOverlap
    /// Family bands overlap ACROSS different trains at the dataset level (supporting evidence only).
    case crossTrainBandOverlap
}

/// Describes the overlap between two mode intervals, tagging whether it is a within-train fact or a
/// cross-train (dataset) band overlap — the distinction the architecture requires but does not model.
public struct ISIOverlapDescriptor: Hashable, Sendable {
    public let kind: ISIOverlapKind
    public let familyA: ISIPatternFamily
    public let familyB: ISIPatternFamily
    public let overlapLowerSec: Double
    public let overlapUpperSec: Double

    public var overlapWidthSec: Double { max(0, overlapUpperSec - overlapLowerSec) }
    public var hasOverlap: Bool { overlapWidthSec > 0 }

    public init(
        kind: ISIOverlapKind,
        familyA: ISIPatternFamily,
        familyB: ISIPatternFamily,
        overlapLowerSec: Double,
        overlapUpperSec: Double
    ) {
        self.kind = kind
        self.familyA = familyA
        self.familyB = familyB
        self.overlapLowerSec = overlapLowerSec
        self.overlapUpperSec = overlapUpperSec
    }

    /// Compute the overlap of two intervals. Returns a descriptor with `hasOverlap == false` when they
    /// are disjoint or either interval is invalid.
    public static func between(
        _ a: ModeISIInterval,
        _ b: ModeISIInterval,
        kind: ISIOverlapKind
    ) -> ISIOverlapDescriptor {
        guard a.isValid, b.isValid else {
            return ISIOverlapDescriptor(kind: kind, familyA: a.family, familyB: b.family,
                                        overlapLowerSec: 0, overlapUpperSec: 0)
        }
        let lower = max(a.lowerSec, b.lowerSec)
        let upper = min(a.upperSec, b.upperSec)
        return ISIOverlapDescriptor(kind: kind, familyA: a.family, familyB: b.family,
                                    overlapLowerSec: lower, overlapUpperSec: max(lower, upper))
    }
}
