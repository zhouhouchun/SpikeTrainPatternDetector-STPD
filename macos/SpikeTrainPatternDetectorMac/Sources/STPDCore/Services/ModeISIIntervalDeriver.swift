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
    /// D3-HF: how well the fast/high-frequency mode separates from the slower tonic-like mode in this
    /// scope. Lets downstream consumers (e.g. the TSW magnitude gate) decide whether the burst valley
    /// and the tonic lower bound are trustworthy classic-tonic boundaries or are themselves fast-regime
    /// artifacts. Defaults to `.unavailable` so existing constructions are unaffected.
    public let separation: ModeSeparationEvidence

    public init(
        burst: ModeISIInterval? = nil,
        tonic: ModeISIInterval? = nil,
        pause: ModeISIInterval? = nil,
        separation: ModeSeparationEvidence = .unavailable
    ) {
        self.burst = burst
        self.tonic = tonic
        self.pause = pause
        self.separation = separation
    }
}

/// D3-HF: reliability of the fast/high-frequency → tonic mode boundary in one scope.
/// Scale-free — every underlying comparison is a ratio, a refractory-relative ceiling, or a support count.
public enum ModeBoundaryReliability: String, Hashable, Sendable {
    /// A clear multiplicative valley with adequate support (≥ `minReliableModeSupport`) on BOTH sides.
    case reliable
    /// A valley exists but one side is a single ISI / below `minReliableModeSupport` — a refractory or
    /// outlier artifact, not a trustworthy mode boundary.
    case degenerate
    /// Substantial fast mass but NO clean multiplicative valley — the fast activity blends continuously
    /// into the tonic-like mode, so no boundary can be trusted.
    case notSeparable
    /// COMPUTED and found negligible fast mass below the physiological fast ceiling — a clean slower mode,
    /// nothing to split. (Distinct from `.unavailable`, which means no evidence was computed at all.)
    case noFastMode
    /// No evidence was computed (e.g. an empty / degenerate sample). NOT a claim that there is no fast
    /// mode — the separation simply could not be characterized. Kept distinct from `.noFastMode` so the
    /// debug UI and any downstream consumer never read "unknown" as "confirmed clean".
    case unavailable
}

/// D3-HF: evidence about how the fast/high-frequency mode relates to the slower tonic-like mode.
/// Attached to every `FamilyModeIntervals`. All fields are scale-free (seconds scale with the input,
/// counts and the reliability class are dimensionless).
public struct ModeSeparationEvidence: Hashable, Sendable {
    /// Antimode valley (seconds) between the fast mode and the slower mode, when a reliable/degenerate
    /// valley was found; nil when no valley exists (noFastMode / notSeparable).
    public let fastTonicValleySec: Double?
    /// Count of ISIs on the fast side of the boundary (or, absent a valley, below the fast ceiling).
    public let fastModeSupportCount: Int
    /// Count of ISIs on the tonic side of the boundary (or, absent a valley, above the fast ceiling).
    public let tonicModeSupportCount: Int
    /// The multiplicative gap ratio at the valley (≥ 1); nil when there is no valley.
    public let valleyGapRatio: Double?
    /// Refractory-relative fast/HF ceiling (`fastModeRefractoryMultiple × refractory floor`). ISIs below
    /// this are physiologically in the fast/HF regime regardless of local distribution shape.
    public let fastCeilingSec: Double
    /// Overall reliability of the fast→tonic boundary in this scope.
    public let reliability: ModeBoundaryReliability
    /// True iff the tonic core lower bound sits at/above the physiological fast ceiling — i.e. the tonic
    /// lower is a trustworthy classic-tonic boundary and not itself a fast/HF-regime value.
    public let tonicLowerReliable: Bool

    public init(
        fastTonicValleySec: Double?,
        fastModeSupportCount: Int,
        tonicModeSupportCount: Int,
        valleyGapRatio: Double?,
        fastCeilingSec: Double,
        reliability: ModeBoundaryReliability,
        tonicLowerReliable: Bool
    ) {
        self.fastTonicValleySec = fastTonicValleySec
        self.fastModeSupportCount = fastModeSupportCount
        self.tonicModeSupportCount = tonicModeSupportCount
        self.valleyGapRatio = valleyGapRatio
        self.fastCeilingSec = fastCeilingSec
        self.reliability = reliability
        self.tonicLowerReliable = tonicLowerReliable
    }

    /// No evidence computed (e.g. an empty/degenerate sample). `reliability == .unavailable` marks this as
    /// "not characterized" rather than a computed "no fast mode".
    public static let unavailable = ModeSeparationEvidence(
        fastTonicValleySec: nil,
        fastModeSupportCount: 0,
        tonicModeSupportCount: 0,
        valleyGapRatio: nil,
        fastCeilingSec: 0,
        reliability: .unavailable,
        tonicLowerReliable: false
    )
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
    /// D3-HF: multiple of the refractory floor below which an ISI is physiologically in the fast/HF
    /// regime, independent of local distribution shape. Same multiple the TSW magnitude gate uses.
    private static let fastModeRefractoryMultiple = 15.0
    /// D3-HF: minimum support (a dimensionless count) required on EACH side of a valley before that
    /// valley is a `reliable` mode boundary rather than a `degenerate` refractory/outlier artifact.
    private static let minReliableModeSupport = 2
    /// D3-HF: minimum fraction of ISIs below the fast ceiling for a scope with NO clean valley to count
    /// as `notSeparable` (graded fast mass) rather than `noFastMode` (clean slow mode). Dimensionless.
    private static let notSeparableFastFractionMin = 0.15

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
        var burstSupportCount: Int?         // ISIs on the fast side of the burst valley
        var burstGapRatio: Double?          // multiplicative gap at the burst valley (D3-HF evidence)
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
            if burst != nil {
                burstBridgeBoundary = valley
                burstSupportCount = count
                burstGapRatio = split.ratio
            }
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

        // --- D3-HF: characterize how the fast/high-frequency mode separates from the tonic-like mode.
        let separation = separationEvidence(
            sortedValues: v,
            n: n,
            floor: floor,
            burstValleySec: burstBridgeBoundary,
            burstSupportCount: burstSupportCount,
            burstGapRatio: burstGapRatio,
            tonicLowerSec: tonic?.lowerSec
        )

        return FamilyModeIntervals(burst: burst, tonic: tonic, pause: pause, separation: separation)
    }

    /// D3-HF: derive `ModeSeparationEvidence` for one scope from its sorted valid ISIs and the already-
    /// derived burst valley / tonic lower. Scale-free: the only absolute quantity is `floor`, and the fast
    /// ceiling is a fixed MULTIPLE of it, so scaling every ISI (and the floor) by k scales every seconds
    /// field by k and leaves the counts / reliability class / `tonicLowerReliable` flag unchanged.
    private static func separationEvidence(
        sortedValues v: [Double],
        n: Int,
        floor: Double,
        burstValleySec: Double?,
        burstSupportCount: Int?,
        burstGapRatio: Double?,
        tonicLowerSec: Double?
    ) -> ModeSeparationEvidence {
        // Refractory-relative fast/HF ceiling. ISIs below this are in the fast regime regardless of shape.
        let fastCeiling = fastModeRefractoryMultiple * floor
        let fastMassCount = v.reduce(into: 0) { acc, value in if value < fastCeiling { acc += 1 } }
        // The tonic lower is a trustworthy classic-tonic boundary only if it clears the fast ceiling.
        // Absent a tonic prior there is no reliable tonic lower to trust.
        let tonicLowerReliable = (tonicLowerSec ?? -.infinity) >= fastCeiling

        if let valley = burstValleySec, let fastSupport = burstSupportCount {
            // A concrete multiplicative valley split the fast mode from the rest.
            let tonicSideCount = n - fastSupport
            let reliability: ModeBoundaryReliability
            if fastSupport < minReliableModeSupport || tonicSideCount < minReliableModeSupport {
                reliability = .degenerate   // one side is a single ISI / below support → artifact
            } else {
                reliability = .reliable
            }
            return ModeSeparationEvidence(
                fastTonicValleySec: valley,
                fastModeSupportCount: fastSupport,
                tonicModeSupportCount: tonicSideCount,
                valleyGapRatio: burstGapRatio,
                fastCeilingSec: fastCeiling,
                reliability: reliability,
                tonicLowerReliable: tonicLowerReliable
            )
        }

        // No clean valley. Distinguish "graded fast mass, not separable" from "clean slow mode, no fast".
        let fastFraction = n > 0 ? Double(fastMassCount) / Double(n) : 0
        let reliability: ModeBoundaryReliability =
            fastFraction >= notSeparableFastFractionMin ? .notSeparable : .noFastMode
        return ModeSeparationEvidence(
            fastTonicValleySec: nil,
            fastModeSupportCount: fastMassCount,
            tonicModeSupportCount: n - fastMassCount,
            valleyGapRatio: nil,
            fastCeilingSec: fastCeiling,
            reliability: reliability,
            tonicLowerReliable: tonicLowerReliable
        )
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
