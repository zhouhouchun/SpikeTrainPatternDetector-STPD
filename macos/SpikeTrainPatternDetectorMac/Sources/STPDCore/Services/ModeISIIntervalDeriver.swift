import Foundation

// MARK: - Phase D3 — derive distribution-based initial ISI mode-interval PRIORS
//
// Pure, read-only service. Given the Phase D1/D2 distributions (DatasetISIDistribution + per-train
// TrainISIDistribution), it derives INITIAL burst/tonic/pause `ModeISIInterval` priors from the ISI
// distribution SHAPE alone (no candidates, no anchors, no detectors).
//
// These are PRIORS, not the full structural detector: the existing burst/tonic/pause bands are
// anchor-refined; D3 intentionally derives a complementary distribution-first prior that later phases
// (D4 structural evidence, D5 detector consumption) will refine/consume. D3 does NOT wire detectors,
// does NOT change labels, and is attached to nothing in this slice.
//
// Everything is SCALE-FREE: bounds come from quantiles, geometric-mean valleys, and DIMENSIONLESS
// ratio guards. The only absolute quantity is the caller-supplied `minimumValidISISec` floor — there
// are NO fixed absolute-ms cutoffs. Scaling every ISI by a constant scales every derived bound by the
// same constant (see the scale-invariance test).

// MARK: Result types

/// The burst/tonic/pause interval priors for one scope (a train, or the pooled dataset).
public struct FamilyModeIntervals: Hashable, Sendable {
    public let burst: ModeISIInterval?
    public let tonic: ModeISIInterval?
    public let pause: ModeISIInterval?

    public init(burst: ModeISIInterval? = nil, tonic: ModeISIInterval? = nil, pause: ModeISIInterval? = nil) {
        self.burst = burst
        self.tonic = tonic
        self.pause = pause
    }
}

/// Output of `ModeISIIntervalDeriver.derive`: per-train (train-local) priors, the pooled dataset
/// priors, and the overlap descriptors between family bands (within-train and cross-train).
public struct DerivedModeISIIntervals: Hashable, Sendable {
    public let perTrain: [String: FamilyModeIntervals]
    public let dataset: FamilyModeIntervals
    public let overlaps: [ISIOverlapDescriptor]

    public init(
        perTrain: [String: FamilyModeIntervals],
        dataset: FamilyModeIntervals,
        overlaps: [ISIOverlapDescriptor]
    ) {
        self.perTrain = perTrain
        self.dataset = dataset
        self.overlaps = overlaps
    }
}

// MARK: Deriver

public enum ModeISIIntervalDeriver {
    /// A dimensionless multiplicative jump (2×) that marks a mode boundary. NOT an absolute-ms value —
    /// it is a ratio between adjacent ISIs, so it is invariant to the overall time scale.
    private static let gapRatioThreshold = 2.0
    /// Minimum valid-ISI count before attempting gap-based burst/pause mode detection.
    private static let minSampleForGap = 4

    /// Derive per-train and pooled-dataset burst/tonic/pause interval priors.
    public static func derive(
        datasetDistribution: DatasetISIDistribution,
        minimumValidISISec: Double
    ) -> DerivedModeISIIntervals {
        var perTrain: [String: FamilyModeIntervals] = [:]
        var overlaps: [ISIOverlapDescriptor] = []

        for train in datasetDistribution.trainDistributions {
            let families = deriveFamilies(
                fromValues: train.validISIValuesSec,
                scope: .trainLocal,
                minimumValidISISec: minimumValidISISec
            )
            perTrain[train.trainID] = families
            overlaps.append(contentsOf: adjacentOverlaps(families, kind: .withinTrainModeOverlap))
        }

        // Dataset priors from the POOLED valid ISIs (all trains concatenated). Clearly marked
        // scope=.dataset / provenance=.datasetSupported so callers never conflate them with train-local.
        let pooled = datasetDistribution.trainDistributions.flatMap(\.validISIValuesSec)
        let datasetFamilies = deriveFamilies(
            fromValues: pooled,
            scope: .dataset,
            minimumValidISISec: minimumValidISISec
        )
        overlaps.append(contentsOf: adjacentOverlaps(datasetFamilies, kind: .crossTrainBandOverlap))

        return DerivedModeISIIntervals(perTrain: perTrain, dataset: datasetFamilies, overlaps: overlaps)
    }

    // MARK: Per-scope family derivation

    private static func deriveFamilies(
        fromValues rawValues: [Double],
        scope: ISIDistributionScope,
        minimumValidISISec: Double
    ) -> FamilyModeIntervals {
        let v = rawValues.filter(\.isFinite).sorted()
        let n = v.count
        guard n >= 2 else { return FamilyModeIntervals() }

        let sample = SortedFiniteSample(sortedFiniteValues: v)
        let floor = max(0, minimumValidISISec)
        guard let median = sample.quantile(0.5),
              let q25 = sample.quantile(0.25),
              let q75 = sample.quantile(0.75) else {
            return FamilyModeIntervals()
        }

        // --- Burst: compact short-ISI mode, marked by a large multiplicative gap in the LOWER region.
        var burst: ModeISIInterval?
        var burstBridgeBoundary: Double?    // valley above the burst cluster = the tonic floor
        if n >= minSampleForGap,
           let split = maxRatioSplit(v, where: { v[$0] <= median }),
           split.ratio >= gapRatioThreshold {
            let coreUpper = v[split.index]
            let nextValue = v[split.index + 1]
            let valley = (coreUpper * nextValue).squareRoot()   // geometric-mean valley (scale-free)
            let lower = max(floor, v[0])
            let count = split.index + 1
            burst = ModeISIInterval.validated(
                family: .burst, lowerSec: lower, upperSec: coreUpper,
                supportCount: count, supportFraction: Double(count) / Double(n),
                scope: scope,
                provenance: provenance(for: scope, statistic: "burst_short_mode_gap", auditOnly: false),
                confidence: Double(count) / Double(n),
                bridgeUpperSec: valley
            )
            if burst != nil { burstBridgeBoundary = valley }
        }

        // --- Pause: large-ISI tail, marked by a large multiplicative gap in the UPPER region.
        var pause: ModeISIInterval?
        var pauseFloorBoundary: Double?     // valley below the pause tail = the tonic ceiling
        if n >= minSampleForGap,
           let split = maxRatioSplit(v, where: { v[$0] >= median }),
           split.ratio >= gapRatioThreshold {
            let coreTop = v[split.index]
            let tailStart = v[split.index + 1]
            let valley = (coreTop * tailStart).squareRoot()
            let tailCount = n - (split.index + 1)
            pauseFloorBoundary = valley
            // A single-point tail is an ambiguous outlier → audit-only (never selects/propagates).
            let auditOnly = tailCount <= 1
            pause = ModeISIInterval.validated(
                family: .pause, lowerSec: valley, upperSec: v[n - 1],
                supportCount: tailCount, supportFraction: Double(tailCount) / Double(n),
                scope: scope,
                provenance: provenance(for: scope, statistic: "pause_large_isi_gap", auditOnly: auditOnly),
                confidence: Double(tailCount) / Double(n)
            )
        }

        // --- Tonic: central ISI mass, bounded ABOVE the burst bridge and BELOW the pause floor.
        var tonic: ModeISIInterval?
        let tonicFloor = max(floor, burstBridgeBoundary ?? floor)
        var tonicLower = max(tonicFloor, q25)
        var tonicUpper = max(tonicLower, q75)
        if let pauseFloorBoundary {
            tonicUpper = min(tonicUpper, pauseFloorBoundary)
            tonicLower = min(tonicLower, tonicUpper)
        }
        if tonicUpper > tonicLower {
            let count = v.reduce(into: 0) { acc, value in
                if value >= tonicLower && value <= tonicUpper { acc += 1 }
            }
            // Acceptance band = the wider "could legitimately be tonic" membership band: extend to the
            // neighbouring mode boundaries when present (burst valley below / pause valley above), else
            // to q10/q90. Clamped so the core (q25-q75) is always a subset (core ⊆ acceptance).
            let acceptanceLowerRaw = burstBridgeBoundary ?? sample.quantile(0.10) ?? tonicLower
            let acceptanceUpperRaw = pauseFloorBoundary ?? sample.quantile(0.90) ?? tonicUpper
            let acceptanceLower = Swift.min(acceptanceLowerRaw, tonicLower)
            let acceptanceUpper = Swift.max(acceptanceUpperRaw, tonicUpper)
            tonic = ModeISIInterval.validated(
                family: .tonic, lowerSec: tonicLower, upperSec: tonicUpper,
                supportCount: count, supportFraction: Double(count) / Double(n),
                scope: scope,
                provenance: provenance(for: scope, statistic: "tonic_central_iqr", auditOnly: false),
                confidence: Double(count) / Double(n),
                acceptanceLowerSec: acceptanceLower, acceptanceUpperSec: acceptanceUpper
            )
        }

        return FamilyModeIntervals(burst: burst, tonic: tonic, pause: pause)
    }

    // MARK: Helpers

    /// The adjacent pair (index, index+1) with the largest multiplicative ratio among indices passing
    /// `predicate`. Scale-free: it ranks by ratio, not absolute difference.
    private static func maxRatioSplit(
        _ v: [Double],
        where predicate: (Int) -> Bool
    ) -> (index: Int, ratio: Double)? {
        var best: (index: Int, ratio: Double)?
        for i in 0..<(v.count - 1) {
            guard predicate(i) else { continue }
            let a = v[i], b = v[i + 1]
            guard a > 0, b.isFinite else { continue }
            let ratio = b / a
            if best == nil || ratio > best!.ratio {
                best = (i, ratio)
            }
        }
        return best
    }

    private static func provenance(
        for scope: ISIDistributionScope,
        statistic: String,
        auditOnly: Bool
    ) -> IntervalProvenance {
        let datasetScoped = scope == .dataset || scope == .trainBalanced
        if auditOnly {
            return .auditOnly(origin: datasetScoped ? .datasetSupported : .trainLocalDerived,
                              sourceStatistic: statistic)
        }
        return datasetScoped
            ? .datasetSupported(sourceStatistic: statistic)
            : .trainLocalDerived(sourceStatistic: statistic)
    }

    /// Overlap descriptors between adjacent family bands (burst↔tonic, tonic↔pause) when both exist.
    private static func adjacentOverlaps(
        _ families: FamilyModeIntervals,
        kind: ISIOverlapKind
    ) -> [ISIOverlapDescriptor] {
        var out: [ISIOverlapDescriptor] = []
        if let burst = families.burst, let tonic = families.tonic {
            out.append(.between(burst, tonic, kind: kind))
        }
        if let tonic = families.tonic, let pause = families.pause {
            out.append(.between(tonic, pause, kind: kind))
        }
        return out
    }
}
