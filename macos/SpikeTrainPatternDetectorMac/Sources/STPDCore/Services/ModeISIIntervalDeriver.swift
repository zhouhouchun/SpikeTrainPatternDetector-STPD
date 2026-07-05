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
    /// Minimum ISIs in the central (post-burst, pre-pause) segment before a tonic mode is trusted.
    /// Dimensionless (a count), so the deriver stays scale-free.
    private static let minTonicSegmentCount = 3

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
        // `median` divides the lower (burst) / upper (pause) search regions. Tonic quantiles are taken on
        // the central segment below (NOT the global sample), so global q25/q75 are no longer needed here.
        guard let median = sample.quantile(0.5) else {
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

        // --- Tonic: central ISI mass, RE-CENTERED on the CENTRAL SEGMENT — the valid ISIs strictly
        // between the burst valley (burstBridgeBoundary) and the pause valley (pauseFloorBoundary). Taking
        // quantiles on that segment ALONE (not the global mixture) stops burst ISIs from pulling the core
        // down and the pause tail from inflating it. Scale-free: the boundaries are the geometric-mean
        // valleys already derived, and the statistics are segment quantiles/extrema — no absolute-ms cutoff.
        var tonic: ModeISIInterval?
        let central = v.filter { value in
            (burstBridgeBoundary.map { value > $0 } ?? true) &&   // no burst ⇒ keep all short ISIs
            (pauseFloorBoundary.map { value < $0 } ?? true)       // no pause ⇒ keep all long ISIs
        }   // `v` is sorted ⇒ `central` is sorted
        if central.count >= minTonicSegmentCount,
           let segLo = central.first, let segHi = central.last, segHi > segLo {
            let centralSample = SortedFiniteSample(sortedFiniteValues: central)
            let cLo = centralSample.quantile(0.25) ?? segLo
            let cHi = centralSample.quantile(0.75) ?? segHi
            // CORE = central q25–q75 (robust tonic identity). For a concentrated/regular tonic mode the
            // central IQR can COLLAPSE (q25 == q75 when a majority of ISIs are near-identical); fall back
            // to the segment support [segMin, segMax] so a valid steady tonic still yields a (non-degenerate,
            // since segHi > segLo) prior instead of disappearing. ACCEPTANCE = the observed central-segment
            // support: a membership band that captures every observed tonic ISI yet, by construction, cannot
            // reach into the empty burst/pause gaps. core ⊆ acceptance holds because segLo <= cLo, segHi >= cHi.
            let tonicLower = cHi > cLo ? cLo : segLo
            let tonicUpper = cHi > cLo ? cHi : segHi
            let acceptanceLower = Swift.min(segLo, tonicLower)
            let acceptanceUpper = Swift.max(segHi, tonicUpper)
            let count = central.reduce(into: 0) { acc, value in
                if value >= tonicLower && value <= tonicUpper { acc += 1 }
            }
            tonic = ModeISIInterval.validated(
                family: .tonic, lowerSec: tonicLower, upperSec: tonicUpper,
                supportCount: count, supportFraction: Double(count) / Double(n),
                scope: scope,
                provenance: provenance(for: scope, statistic: "tonic_central_segment_iqr", auditOnly: false),
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
