import Foundation

// MARK: - Phase D4-0 — common structural-evidence foundation
//
// Pure data + metric foundation for the D4 evidence layer. D4 is an ADVISORY layer: it evaluates
// SUPPLIED spans/runs/gaps and returns RECOMMENDATIONS only. It does NOT generate candidates, select
// final labels, carve states, propagate dataset priors, or wire detectors. Final authority is decided
// later by D5 / the resolver.
//
// This file (D4-0) provides only the shared vocabulary + span-metric computation. The per-family
// evidence evaluators (burst/tonic/pause) come in later slices (D4a+).

// MARK: Evidence signal vocabulary

public enum EvidenceStatus: String, Hashable, Sendable {
    case pass, fail, notApplicable, insufficientEvidence
}

public enum EvidenceRole: String, Hashable, Sendable {
    case eligibility, compactness, boundary, regularity, contaminationVeto, priorCompatibility, audit
}

/// A single structural-evidence observation. Purely descriptive — carries no authority.
public struct EvidenceSignal: Hashable, Sendable {
    public let key: String
    public let status: EvidenceStatus
    public let role: EvidenceRole
    public let observedValue: Double?
    public let requiredValue: Double?
    public let message: String

    public init(
        key: String,
        status: EvidenceStatus,
        role: EvidenceRole,
        observedValue: Double? = nil,
        requiredValue: Double? = nil,
        message: String = ""
    ) {
        self.key = key
        self.status = status
        self.role = role
        self.observedValue = observedValue
        self.requiredValue = requiredValue
        self.message = message
    }
}

// MARK: Outcomes & recommendations (advisory only)

public enum FamilyEvidenceOutcome: String, Hashable, Sendable {
    case confirmed, refined, possibleReview, rejected, insufficientEvidence
}

/// Advisory recommendation on whether the evidence would support SELECTING a final label. NOT a
/// decision — the resolver (D5) decides. `noRecommendation` means the evidence is silent on selection.
public enum SelectionAuthorityRecommendation: String, Hashable, Sendable {
    case recommendSelectable, recommendReviewOnly, recommendNotSelectable, noRecommendation
}

/// Advisory recommendation on whether the family could CARVE/split an overlapping state. Advisory only.
public enum CarvingAuthorityRecommendation: String, Hashable, Sendable {
    case recommendMayCarve, recommendOverlayOnly, recommendNoCarve, noRecommendation
}

/// Advisory recommendation on dataset-prior propagation. Advisory only.
public enum PriorAuthorityRecommendation: String, Hashable, Sendable {
    case recommendDatasetPropagatable, recommendTrainLocalOnly, recommendAuditOnly, noRecommendation
}

/// Why span metrics could not be fully computed.
public enum InsufficientEvidenceReason: String, Hashable, Sendable {
    case noValidISI
    case tooFewValidISI

    public var message: String {
        switch self {
        case .noValidISI: return "no valid ISI values in span"
        case .tooFewValidISI: return "too few valid ISI values for the requested metrics"
        }
    }
}

// MARK: ISISpan

/// A supplied span of ISIs to evaluate. `startISIIndex`/`endISIIndex` are INCLUSIVE indices into
/// `SpikeTrain.isiSec`, whose index 0 is the structural placeholder (never counted). `familyHint` is
/// a non-authoritative hint about which family the caller is probing.
public struct ISISpan: Hashable, Sendable {
    public let trainID: String
    public let startISIIndex: Int
    public let endISIIndex: Int
    public let familyHint: ISIPatternFamily?

    public init(trainID: String, startISIIndex: Int, endISIIndex: Int, familyHint: ISIPatternFamily? = nil) {
        self.trainID = trainID
        self.startISIIndex = startISIIndex
        self.endISIIndex = endISIIndex
        self.familyHint = familyHint
    }

    /// Raw inclusive ISI-slot count (before validity filtering); 0 when the range is empty.
    public var rawISICount: Int { endISIIndex >= startISIIndex ? endISIIndex - startISIIndex + 1 : 0 }
}

// MARK: ISISpanMetrics

/// Structural metrics for a span. Reuses the shared `STPDStatistics` CV/CV2/LV (exact detector parity)
/// and the shared `SortedFiniteSample` quantiles. Non-finite / sub-floor ISIs are excluded and flagged.
public struct ISISpanMetrics: Hashable, Sendable {
    /// Train-ORDERED valid ISI values in the span. CV2/LV use this order; quantiles use a sorted copy.
    public let values: [Double]
    /// Raw ISI-slot count of the span (including any invalid/excluded slots).
    public let nISI: Int
    /// Spikes touched by the raw span (`nISI + 1` when non-empty).
    public let nSpikes: Int
    public let minSec: Double?
    public let maxSec: Double?
    public let meanSec: Double?
    /// Median; identical to `q50Sec` (both kept for readability).
    public let medianSec: Double?
    public let q10Sec: Double?
    public let q40Sec: Double?
    public let q50Sec: Double?
    public let q90Sec: Double?
    public let q95Sec: Double?
    public let cv: Double?
    public let cv2: Double?
    public let lv: Double?
    /// The valid ISI immediately before / after the span (the flanking "gap"), if any.
    public let preGapSec: Double?
    public let postGapSec: Double?
    /// Flank contrast: pre/post gap divided by the span's q90.
    public let preRatioQ90: Double?
    public let postRatioQ90: Double?
    public let edgeContrastMinQ90: Double?
    public let edgeContrastGeomQ90: Double?
    /// Bridge burden vs a supplied reference (count/fraction of span ISIs `> bridgeReferenceUpperSec`).
    /// nil unless a reference was supplied.
    public let bridgeCount: Int?
    public let bridgeFraction: Double?
    public let validISIValuesCount: Int
    public let hasInvalidISI: Bool
    public let insufficientEvidenceReason: InsufficientEvidenceReason?

    // Full memberwise init (public so callers/tests can build fixtures directly).
    public init(
        values: [Double], nISI: Int, nSpikes: Int,
        minSec: Double?, maxSec: Double?, meanSec: Double?, medianSec: Double?,
        q10Sec: Double?, q40Sec: Double?, q50Sec: Double?, q90Sec: Double?, q95Sec: Double?,
        cv: Double?, cv2: Double?, lv: Double?,
        preGapSec: Double?, postGapSec: Double?, preRatioQ90: Double?, postRatioQ90: Double?,
        edgeContrastMinQ90: Double?, edgeContrastGeomQ90: Double?,
        bridgeCount: Int?, bridgeFraction: Double?,
        validISIValuesCount: Int, hasInvalidISI: Bool, insufficientEvidenceReason: InsufficientEvidenceReason?
    ) {
        self.values = values; self.nISI = nISI; self.nSpikes = nSpikes
        self.minSec = minSec; self.maxSec = maxSec; self.meanSec = meanSec; self.medianSec = medianSec
        self.q10Sec = q10Sec; self.q40Sec = q40Sec; self.q50Sec = q50Sec; self.q90Sec = q90Sec; self.q95Sec = q95Sec
        self.cv = cv; self.cv2 = cv2; self.lv = lv
        self.preGapSec = preGapSec; self.postGapSec = postGapSec
        self.preRatioQ90 = preRatioQ90; self.postRatioQ90 = postRatioQ90
        self.edgeContrastMinQ90 = edgeContrastMinQ90; self.edgeContrastGeomQ90 = edgeContrastGeomQ90
        self.bridgeCount = bridgeCount; self.bridgeFraction = bridgeFraction
        self.validISIValuesCount = validISIValuesCount; self.hasInvalidISI = hasInvalidISI
        self.insufficientEvidenceReason = insufficientEvidenceReason
    }

    /// Compute metrics from an already-ordered set of valid ISI values, plus optional flanking gaps and
    /// a bridge reference. Non-finite values are defensively dropped and flagged.
    public static func compute(
        orderedValidISISec values: [Double],
        preNeighborSec: Double? = nil,
        postNeighborSec: Double? = nil,
        bridgeReferenceUpperSec: Double? = nil
    ) -> ISISpanMetrics {
        let clean = values.filter(\.isFinite)
        return make(
            orderedValid: clean, rawISICount: values.count, hasInvalidISI: clean.count != values.count,
            preGap: preNeighborSec, postGap: postNeighborSec, bridgeReferenceUpperSec: bridgeReferenceUpperSec
        )
    }

    /// Compute metrics for a span of a real `SpikeTrain`, applying the D2 QC (skip the leading
    /// placeholder, keep finite `>= minimumValidISISec`) and flagging any excluded ISI. The flanking
    /// gaps are the nearest valid ISIs just outside the span.
    public static func from(
        train: SpikeTrain,
        span: ISISpan,
        thresholds: StructuralEvidenceThresholds
    ) -> ISISpanMetrics {
        let isi = train.isiSec
        let floor = max(0, thresholds.minimumValidISISec)
        // ISI slots live at indices 1...(count-1); index 0 is the structural placeholder.
        let lo = max(1, span.startISIIndex)
        let hi = min(isi.count - 1, span.endISIIndex)
        var ordered: [Double] = []
        var rawCount = 0
        var hadInvalid = false
        if hi >= lo {
            for index in lo...hi {
                rawCount += 1
                guard let value = isi[index], value.isFinite, value >= floor else { hadInvalid = true; continue }
                ordered.append(value)
            }
        }
        let pre = validISI(in: isi, at: lo - 1, floor: floor)
        let post = validISI(in: isi, at: hi + 1, floor: floor)
        return make(
            orderedValid: ordered, rawISICount: rawCount, hasInvalidISI: hadInvalid,
            preGap: pre, postGap: post, bridgeReferenceUpperSec: nil
        )
    }

    private static func validISI(in isi: [Double?], at index: Int, floor: Double) -> Double? {
        guard index >= 1, index < isi.count, let value = isi[index], value.isFinite, value >= floor else {
            return nil
        }
        return value
    }

    private static func make(
        orderedValid: [Double], rawISICount: Int, hasInvalidISI: Bool,
        preGap: Double?, postGap: Double?, bridgeReferenceUpperSec: Double?
    ) -> ISISpanMetrics {
        let n = orderedValid.count
        let reason: InsufficientEvidenceReason? = n == 0 ? .noValidISI : (n < 2 ? .tooFewValidISI : nil)
        let sample = SortedFiniteSample(sortedFiniteValues: orderedValid.sorted())
        func q(_ p: Double) -> Double? { sample.quantile(p) }
        let q90 = q(0.90)

        func flankRatio(_ gap: Double?) -> Double? {
            guard let gap, let q90, q90 > 0 else { return nil }
            return gap / q90
        }
        let preRatio = flankRatio(preGap)
        let postRatio = flankRatio(postGap)
        let edgeMin: Double? = {
            switch (preRatio, postRatio) {
            case let (a?, b?): return Swift.min(a, b)
            case let (a?, nil): return a
            case let (nil, b?): return b
            default: return nil
            }
        }()
        let edgeGeom: Double? = {
            guard let a = preRatio, let b = postRatio, a >= 0, b >= 0 else { return nil }
            return (a * b).squareRoot()
        }()

        let bridgeCount = bridgeReferenceUpperSec.map { ref in orderedValid.filter { $0 > ref }.count }
        let bridgeFraction: Double? = {
            guard let bridgeCount, n > 0 else { return nil }
            return Double(bridgeCount) / Double(n)
        }()

        return ISISpanMetrics(
            values: orderedValid,
            nISI: rawISICount,
            nSpikes: rawISICount > 0 ? rawISICount + 1 : 0,
            minSec: q(0.0), maxSec: q(1.0), meanSec: STPDStatistics.mean(orderedValid), medianSec: q(0.5),
            q10Sec: q(0.10), q40Sec: q(0.40), q50Sec: q(0.50), q90Sec: q90, q95Sec: q(0.95),
            cv: STPDStatistics.coefficientOfVariation(orderedValid),
            cv2: STPDStatistics.coefficientOfVariation2(orderedValid),
            lv: STPDStatistics.localVariation(orderedValid),
            preGapSec: preGap, postGapSec: postGap,
            preRatioQ90: preRatio, postRatioQ90: postRatio,
            edgeContrastMinQ90: edgeMin, edgeContrastGeomQ90: edgeGeom,
            bridgeCount: bridgeCount, bridgeFraction: bridgeFraction,
            validISIValuesCount: n, hasInvalidISI: hasInvalidISI, insufficientEvidenceReason: reason
        )
    }
}

// MARK: FamilyEvidenceVerdict (advisory container)

/// The advisory result of evaluating one span/interval for one family. Carries recommendations ONLY —
/// there is deliberately NO `isSelected` / `mayCarve` / `mayPropagate` / final-authority field.
public struct FamilyEvidenceVerdict: Hashable, Sendable {
    public let family: ISIPatternFamily
    public let outcome: FamilyEvidenceOutcome
    public let originalSpan: ISISpan
    public let refinedSpan: ISISpan?
    public let originalInterval: ModeISIInterval?
    public let refinedInterval: ModeISIInterval?
    public let signals: [EvidenceSignal]
    public let selectionRecommendation: SelectionAuthorityRecommendation
    public let carvingRecommendation: CarvingAuthorityRecommendation
    public let priorRecommendation: PriorAuthorityRecommendation
    public let reviewRequired: Bool
    public let decisionPath: String

    public init(
        family: ISIPatternFamily,
        outcome: FamilyEvidenceOutcome,
        originalSpan: ISISpan,
        refinedSpan: ISISpan? = nil,
        originalInterval: ModeISIInterval? = nil,
        refinedInterval: ModeISIInterval? = nil,
        signals: [EvidenceSignal] = [],
        selectionRecommendation: SelectionAuthorityRecommendation = .noRecommendation,
        carvingRecommendation: CarvingAuthorityRecommendation = .noRecommendation,
        priorRecommendation: PriorAuthorityRecommendation = .noRecommendation,
        reviewRequired: Bool = false,
        decisionPath: String = ""
    ) {
        self.family = family
        self.outcome = outcome
        self.originalSpan = originalSpan
        self.refinedSpan = refinedSpan
        self.originalInterval = originalInterval
        self.refinedInterval = refinedInterval
        self.signals = signals
        self.selectionRecommendation = selectionRecommendation
        self.carvingRecommendation = carvingRecommendation
        self.priorRecommendation = priorRecommendation
        self.reviewRequired = reviewRequired
        self.decisionPath = decisionPath
    }
}

// MARK: StructuralEvidenceThresholds

/// Threshold set shared by the (future) burst/tonic/pause evidence evaluators.
///
/// Two provenance classes:
///   • FACTORY-COPIED from public settings (`from(classicAnchor:)` / `from(state:)` / `from(pause:)` /
///     `from(quality:)`): floors, burst core/bridge/contrast, tonic CV/CV2/LV, pause global-median.
///   • D4 CALIBRATION DEFAULTS that MIRROR current detector INLINE literals (there is no public settings
///     source): `q95ExcessRatioMax`, `leadingCoreRatioMax`, `trailingCoreRatioMax`, `oneSidedSeedPurityMin`,
///     `tonicLocalRatio*`, `tonicLocalRobustZMax`, `tonicPauseGuardRatio`, `pauseLocalPercentileTight`,
///     `pauseLocalRobustZTight`. The factories do NOT set these — they are documented calibration targets.
public struct StructuralEvidenceThresholds: Hashable, Sendable {
    // Floors (factory-copied)
    public var minimumValidISISec: Double
    public var artifactThresholdSec: Double
    // Burst (factory-copied from ClassicAnchorSettings)
    public var burstCoreMinISI: Int
    public var burstBridgeMaxCount: Int
    public var burstBridgeFractionMax: Double
    public var burstContrastMin: Double
    public var burstContrastGeomMin: Double
    public var burstSeedUpperSec: Double
    // Tonic (factory-copied from StatePatternDetectorSettings)
    public var tonicMinSpikes: Int
    public var tonicCVMax: Double
    public var tonicCV2Max: Double
    public var tonicLVMax: Double
    public var irregularTonicCVMax: Double
    public var irregularTonicCV2Max: Double
    public var irregularTonicLVMax: Double
    public var tonicBridgeFractionMax: Double
    public var tonicBurstSeedFractionMax: Double
    // Pause (factory-copied from PauseDetectorSettings)
    public var globalMedianFactor: Double
    // D4 calibration defaults (mirror detector inline literals; NOT settings-sourced)
    public var q95ExcessRatioMax: Double
    public var leadingCoreRatioMax: Double
    public var trailingCoreRatioMax: Double
    public var oneSidedSeedPurityMin: Double
    public var tonicLocalRatioLow: Double
    public var tonicLocalRatioHigh: Double
    public var tonicLocalRobustZMax: Double
    public var tonicPauseGuardRatio: Double
    public var pauseLocalPercentileTight: Double
    public var pauseLocalRobustZTight: Double

    public init(
        minimumValidISISec: Double = 0.001,
        artifactThresholdSec: Double = 0.0009,
        burstCoreMinISI: Int = 2,
        burstBridgeMaxCount: Int = 4,
        burstBridgeFractionMax: Double = 0.60,
        burstContrastMin: Double = 3.0,
        // Standalone default mirrors ClassicAnchorSettings' "geom defaults to contrast-min" resolution.
        burstContrastGeomMin: Double = 3.0,
        burstSeedUpperSec: Double = 0.010,
        tonicMinSpikes: Int = 5,
        tonicCVMax: Double = 0.30,
        tonicCV2Max: Double = 0.30,
        tonicLVMax: Double = 0.35,
        irregularTonicCVMax: Double = 0.60,
        irregularTonicCV2Max: Double = 0.60,
        irregularTonicLVMax: Double = 0.80,
        tonicBridgeFractionMax: Double = 0.20,
        tonicBurstSeedFractionMax: Double = 0.20,
        globalMedianFactor: Double = 2.5,
        // --- D4 calibration defaults (inline-literal mirrors) ---
        q95ExcessRatioMax: Double = 1.35,
        leadingCoreRatioMax: Double = 2.0,
        trailingCoreRatioMax: Double = 3.0,
        oneSidedSeedPurityMin: Double = 0.65,
        tonicLocalRatioLow: Double = 0.55,
        tonicLocalRatioHigh: Double = 1.85,
        tonicLocalRobustZMax: Double = 3.0,
        tonicPauseGuardRatio: Double = 1.15,
        pauseLocalPercentileTight: Double = 0.90,
        pauseLocalRobustZTight: Double = 2.5
    ) {
        self.minimumValidISISec = minimumValidISISec
        self.artifactThresholdSec = artifactThresholdSec
        self.burstCoreMinISI = burstCoreMinISI
        self.burstBridgeMaxCount = burstBridgeMaxCount
        self.burstBridgeFractionMax = burstBridgeFractionMax
        self.burstContrastMin = burstContrastMin
        self.burstContrastGeomMin = burstContrastGeomMin
        self.burstSeedUpperSec = burstSeedUpperSec
        self.tonicMinSpikes = tonicMinSpikes
        self.tonicCVMax = tonicCVMax
        self.tonicCV2Max = tonicCV2Max
        self.tonicLVMax = tonicLVMax
        self.irregularTonicCVMax = irregularTonicCVMax
        self.irregularTonicCV2Max = irregularTonicCV2Max
        self.irregularTonicLVMax = irregularTonicLVMax
        self.tonicBridgeFractionMax = tonicBridgeFractionMax
        self.tonicBurstSeedFractionMax = tonicBurstSeedFractionMax
        self.globalMedianFactor = globalMedianFactor
        self.q95ExcessRatioMax = q95ExcessRatioMax
        self.leadingCoreRatioMax = leadingCoreRatioMax
        self.trailingCoreRatioMax = trailingCoreRatioMax
        self.oneSidedSeedPurityMin = oneSidedSeedPurityMin
        self.tonicLocalRatioLow = tonicLocalRatioLow
        self.tonicLocalRatioHigh = tonicLocalRatioHigh
        self.tonicLocalRobustZMax = tonicLocalRobustZMax
        self.tonicPauseGuardRatio = tonicPauseGuardRatio
        self.pauseLocalPercentileTight = pauseLocalPercentileTight
        self.pauseLocalRobustZTight = pauseLocalRobustZTight
    }
}

extension StructuralEvidenceThresholds {
    /// Copy the BURST + floor thresholds that are public fields of `ClassicAnchorSettings`.
    /// (`burstContrastGeomMin` is copied from the resolved public field, not guessed.)
    public static func from(classicAnchor s: ClassicAnchorSettings) -> StructuralEvidenceThresholds {
        var t = StructuralEvidenceThresholds()
        t.minimumValidISISec = s.minValidISISec
        t.burstCoreMinISI = s.burstCoreMinISI
        t.burstBridgeMaxCount = s.burstBridgeMaxCount
        t.burstBridgeFractionMax = s.burstBridgeFractionMax
        t.burstContrastMin = s.burstContrastMin
        t.burstContrastGeomMin = s.burstContrastGeomMin
        return t
    }

    /// Copy the TONIC + floor thresholds that are public fields of `StatePatternDetectorSettings`.
    public static func from(state s: StatePatternDetectorSettings) -> StructuralEvidenceThresholds {
        var t = StructuralEvidenceThresholds()
        t.minimumValidISISec = s.minValidISISec
        t.burstSeedUpperSec = s.burstSeedUpperSec
        t.tonicMinSpikes = s.tonicMinSpikes
        t.tonicCVMax = s.tonicCVMax
        t.tonicCV2Max = s.tonicCV2Max
        t.tonicLVMax = s.tonicLVMax
        t.irregularTonicCVMax = s.irregularTonicCVMax
        t.irregularTonicCV2Max = s.irregularTonicCV2Max
        t.irregularTonicLVMax = s.irregularTonicLVMax
        t.tonicBridgeFractionMax = s.tonicBridgeFractionMax
        t.tonicBurstSeedFractionMax = s.tonicBurstSeedFractionMax
        return t
    }

    /// Copy the PAUSE + floor thresholds that are public fields of `PauseDetectorSettings`.
    public static func from(pause s: PauseDetectorSettings) -> StructuralEvidenceThresholds {
        var t = StructuralEvidenceThresholds()
        t.minimumValidISISec = s.minValidISISec
        t.globalMedianFactor = s.globalMedianFactor
        return t
    }

    /// Copy the artifact floor, a public field of `SpikeQualitySettings`.
    public static func from(quality s: SpikeQualitySettings) -> StructuralEvidenceThresholds {
        var t = StructuralEvidenceThresholds()
        t.artifactThresholdSec = s.artifactThresholdSec
        return t
    }
}
