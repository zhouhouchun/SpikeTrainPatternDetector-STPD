import Foundation

// MARK: - TSW-1 — first-stage tonic STRUCTURAL window detector (pure, UNWIRED)
//
// Discovers tonic structure directly from a train's QC-filtered SEQUENTIAL ISI series. It is the missing
// UPSTREAM producer of structural candidates: it consumes NO final labels and does NOT start from a
// global/adaptive tonic ISI band. A distribution-derived burst prior is used ONLY as a POST-window
// contamination VETO (never as a starting membership band).
//
// TSW-1 provides the pure building blocks only — the per-window regularity GATE (short-window compactness
// vs long-window CV/CV2/LV), an adjacent-ratio helper, and the burst-contamination GUARD — plus the
// candidate/output model. The stride-1 seed scan, merge, and boundary-refine emission come in TSW-2/3.
//
// Metric parity: all CV/CV2/LV come from `ISISpanMetrics` / `STPDStatistics` (never hand-rolled). Scale
// freedom: every gate is a ratio or a config-supplied floor; there are NO fixed absolute-ms literals in
// the gate logic (the only absolute quantities are the caller's QC/user-configured floors).
//
// NOTE: the R-style max/mean peakiness ratio is deliberately NOT used (that metric was removed from this
// codebase); short windows use the shipped median range-ratio compactness instead.

// MARK: Output vocabulary

/// How a tonic structural candidate was produced.
public enum TonicWindowSource: String, Hashable, Sendable {
    case seed          // a single passing seed / evaluated window
    case merged        // union of overlapping/adjacent passing seeds (TSW-3)
    case refined       // boundary-refined (expanded/shrunk) span (TSW-3)
}

/// Why a window (or an expansion step) failed — attached to the failing edge as boundary evidence.
public enum TonicWindowBoundaryReason: String, Hashable, Sendable {
    case insufficientData          // fewer than 2 valid ISIs / degenerate median
    case notCompact                // short window: an ISI fell outside median*[low, high]
    case adjacentRatioExceeded     // a single adjacent ISI step jumped more than the allowed ratio
    case cvExceeded                // long window: CV over the max
    case cv2Exceeded               // long window: CV2 over the max
    case lvExceeded                // long window: LV over the max
    case burstContamination        // too many / a run of sub-burst-floor ISIs (post-window veto)

    public var message: String {
        switch self {
        case .insufficientData: return "fewer than two valid ISIs for a regularity test"
        case .notCompact: return "an ISI fell outside the median compactness band"
        case .adjacentRatioExceeded: return "an adjacent ISI step exceeded the allowed ratio"
        case .cvExceeded: return "CV exceeded the tonic maximum"
        case .cv2Exceeded: return "CV2 exceeded the tonic maximum"
        case .lvExceeded: return "LV exceeded the tonic maximum"
        case .burstContamination: return "window contains burst-floor contamination"
        }
    }
}

/// TSW configuration. Reuses `StructuralEvidenceThresholds` for the CV/CV2/LV limits, the compactness
/// ratios (`tonicLocalRatioLow/High`), the QC `minimumValidISISec` floor, and the contamination fraction
/// cap. The extra fields govern the burst-floor fallback and the short/long boundary.
public struct TonicStructuralWindowConfig: Hashable, Sendable {
    public var thresholds: StructuralEvidenceThresholds
    /// QC refractory-suspect floor (seconds) — a QC/user-configured floor, not a fixed cutoff. When no
    /// distribution-derived burst valley is supplied, the burst-contamination floor is this × the
    /// multiplier below.
    public var refractoryFloorSec: Double
    /// Dimensionless multiplier applied to `refractoryFloorSec` to form the fallback burst floor.
    public var refractoryFloorMultiplier: Double
    /// Per-step / short-window local guard: the maximum allowed adjacent ISI ratio (scale-free). Defaults
    /// to the compactness high ratio so the local and whole-window guards agree.
    public var adjacentRatioMax: Double
    /// Minimum valid ISIs at which the gate switches from compactness to CV/CV2/LV.
    public var longWindowMinISI: Int

    public init(
        thresholds: StructuralEvidenceThresholds = StructuralEvidenceThresholds(),
        refractoryFloorSec: Double = 0.001,
        refractoryFloorMultiplier: Double = 3.0,
        adjacentRatioMax: Double? = nil,
        longWindowMinISI: Int = 5
    ) {
        self.thresholds = thresholds
        self.refractoryFloorSec = refractoryFloorSec
        self.refractoryFloorMultiplier = refractoryFloorMultiplier
        self.adjacentRatioMax = adjacentRatioMax ?? thresholds.tonicLocalRatioHigh
        self.longWindowMinISI = longWindowMinISI
    }
}

/// A first-stage tonic STRUCTURAL candidate — a span the sequence-local detector judged tonic-regular and
/// burst-clean. Carries no final-label authority.
public struct TonicStructuralWindowCandidate: Hashable, Sendable {
    public let span: ISISpan
    public let metrics: ISISpanMetrics
    public let source: TonicWindowSource
    public let signals: [EvidenceSignal]
    public let reviewRequired: Bool
    /// Why expansion stopped at the trailing edge (TSW-2/3); nil for a single evaluated window.
    public let boundaryReason: TonicWindowBoundaryReason?
    public let decisionPath: String

    public init(
        span: ISISpan, metrics: ISISpanMetrics, source: TonicWindowSource,
        signals: [EvidenceSignal], reviewRequired: Bool,
        boundaryReason: TonicWindowBoundaryReason?, decisionPath: String
    ) {
        self.span = span; self.metrics = metrics; self.source = source
        self.signals = signals; self.reviewRequired = reviewRequired
        self.boundaryReason = boundaryReason; self.decisionPath = decisionPath
    }

    /// Span → spike mapping per the codebase convention: ISI index `i` is the interval between spikes
    /// `i-1` and `i` (`isiSec[0]` is a placeholder), so the span touches spikes
    /// `[startISIIndex, endISIIndex + 1]`.
    public var startSpikeIndex: Int { span.startISIIndex }
    public var endSpikeIndex: Int { span.endISIIndex + 1 }
}

/// The outcome of evaluating one window: accepted as a candidate, or rejected with a boundary reason.
public enum TonicWindowEvaluation: Hashable, Sendable {
    case accepted(TonicStructuralWindowCandidate)
    case rejected(reason: TonicWindowBoundaryReason, signals: [EvidenceSignal])
}

// MARK: Detector

public enum TonicStructuralWindowDetector {

    /// Largest adjacent ISI ratio in train order: `max_i max(v[i+1]/v[i], v[i]/v[i+1])`. Scale-free.
    /// nil when fewer than two values or any value is non-positive.
    public static func maxAdjacentRatio(_ values: [Double]) -> Double? {
        guard values.count >= 2 else { return nil }
        var worst = 1.0
        for i in 0..<(values.count - 1) {
            let a = values[i], b = values[i + 1]
            guard a > 0, b > 0 else { return nil }
            worst = Swift.max(worst, Swift.max(a / b, b / a))
        }
        return worst
    }

    /// The effective burst-contamination floor + a provenance tag for the decision path. Prefers the
    /// supplied distribution-derived burst valley (D3); otherwise the QC refractory floor × multiplier.
    public static func effectiveBurstFloorSec(
        burstValleySec: Double?, config: TonicStructuralWindowConfig
    ) -> (floorSec: Double, provenance: String) {
        if let valley = burstValleySec, valley.isFinite, valley > 0 {
            return (valley, "d3_valley")
        }
        let fallback = config.refractoryFloorSec * config.refractoryFloorMultiplier
        return (fallback, "refractory_x\(config.refractoryFloorMultiplier)")
    }

    /// Per-window regularity gate. Short windows (< `longWindowMinISI` valid ISIs) use median range-ratio
    /// compactness; longer windows use CV/CV2/LV from `ISISpanMetrics`. Both tiers also apply the local
    /// adjacent-ratio cap. Returns pass/fail, the per-metric signals, the first failing reason, and which
    /// tier was used.
    public static func regularityGate(
        metrics: ISISpanMetrics, config: TonicStructuralWindowConfig
    ) -> (passed: Bool, signals: [EvidenceSignal], failReason: TonicWindowBoundaryReason?, usedLongMetrics: Bool) {
        let vals = metrics.values
        let t = config.thresholds
        guard vals.count >= 2, let median = metrics.medianSec, median > 0 else {
            let signal = EvidenceSignal(
                key: "tonic_window_size", status: .insufficientEvidence, role: .eligibility,
                observedValue: Double(vals.count), requiredValue: 2, message: "need >= 2 valid ISIs")
            return (false, [signal], .insufficientData, false)
        }
        let long = vals.count >= config.longWindowMinISI
        var signals: [EvidenceSignal] = []

        if long {
            let cv = metrics.cv ?? .infinity
            signals.append(EvidenceSignal(key: "tonic_cv", status: cv <= t.tonicCVMax ? .pass : .fail,
                                          role: .regularity, observedValue: metrics.cv, requiredValue: t.tonicCVMax))
            if cv > t.tonicCVMax { return (false, signals, .cvExceeded, true) }
            let cv2 = metrics.cv2 ?? .infinity
            signals.append(EvidenceSignal(key: "tonic_cv2", status: cv2 <= t.tonicCV2Max ? .pass : .fail,
                                          role: .regularity, observedValue: metrics.cv2, requiredValue: t.tonicCV2Max))
            if cv2 > t.tonicCV2Max { return (false, signals, .cv2Exceeded, true) }
            let lv = metrics.lv ?? .infinity
            signals.append(EvidenceSignal(key: "tonic_lv", status: lv <= t.tonicLVMax ? .pass : .fail,
                                          role: .regularity, observedValue: metrics.lv, requiredValue: t.tonicLVMax))
            if lv > t.tonicLVMax { return (false, signals, .lvExceeded, true) }
        } else {
            let low = median * t.tonicLocalRatioLow
            let high = median * t.tonicLocalRatioHigh
            let compact = vals.allSatisfy { $0 >= low && $0 <= high }
            let peakRatio = (vals.map { $0 / median }.max() ?? 1)   // observed worst deviation above the median
            signals.append(EvidenceSignal(key: "tonic_compactness", status: compact ? .pass : .fail,
                                          role: .compactness, observedValue: peakRatio, requiredValue: t.tonicLocalRatioHigh,
                                          message: "each ISI within median*[low, high]"))
            if !compact { return (false, signals, .notCompact, false) }
        }

        if let adj = maxAdjacentRatio(vals) {
            signals.append(EvidenceSignal(key: "tonic_adjacent_ratio", status: adj <= config.adjacentRatioMax ? .pass : .fail,
                                          role: .regularity, observedValue: adj, requiredValue: config.adjacentRatioMax))
            if adj > config.adjacentRatioMax { return (false, signals, .adjacentRatioExceeded, long) }
        }
        return (true, signals, nil, long)
    }

    /// Burst-contamination guard: a POST-window veto (not a starting band). Rejects windows where more
    /// than `tonicBurstSeedFractionMax` of ISIs, OR a run of >= 2 consecutive ISIs, fall below the burst
    /// floor. Returns pass/fail, a single contamination signal, and the floor provenance tag.
    public static func burstContaminationGuard(
        metrics: ISISpanMetrics, burstValleySec: Double?, config: TonicStructuralWindowConfig
    ) -> (passed: Bool, signal: EvidenceSignal, provenance: String) {
        let (floor, provenance) = effectiveBurstFloorSec(burstValleySec: burstValleySec, config: config)
        let vals = metrics.values
        let subFloorFlags = vals.map { $0 < floor }
        let subCount = subFloorFlags.filter { $0 }.count
        let fraction = vals.isEmpty ? 0 : Double(subCount) / Double(vals.count)
        let longestSubRun = maxConsecutiveTrue(subFloorFlags)
        let contaminated = fraction > config.thresholds.tonicBurstSeedFractionMax || longestSubRun >= 2
        let signal = EvidenceSignal(
            key: "burst_contamination", status: contaminated ? .fail : .pass, role: .contaminationVeto,
            observedValue: fraction, requiredValue: config.thresholds.tonicBurstSeedFractionMax,
            message: "burst_floor=\(provenance)")
        return (!contaminated, signal, provenance)
    }

    /// Evaluate one span of a real train end-to-end: compute `ISISpanMetrics` (QC-filtered, parity with
    /// the D3/D4 layers), apply the regularity gate then the burst guard, and return an accepted candidate
    /// or a rejection with its boundary reason.
    public static func evaluateWindow(
        train: SpikeTrain, span: ISISpan, source: TonicWindowSource = .seed,
        burstValleySec: Double?, config: TonicStructuralWindowConfig = TonicStructuralWindowConfig()
    ) -> TonicWindowEvaluation {
        let metrics = ISISpanMetrics.from(train: train, span: span, thresholds: config.thresholds)
        return evaluate(metrics: metrics, span: span, source: source, burstValleySec: burstValleySec, config: config)
    }

    /// Evaluate from already-computed metrics (used by tests and by the TSW-2/3 scan to avoid recomputing).
    public static func evaluate(
        metrics: ISISpanMetrics, span: ISISpan, source: TonicWindowSource = .seed,
        burstValleySec: Double?, config: TonicStructuralWindowConfig = TonicStructuralWindowConfig()
    ) -> TonicWindowEvaluation {
        let gate = regularityGate(metrics: metrics, config: config)
        guard gate.passed else {
            return .rejected(reason: gate.failReason ?? .insufficientData, signals: gate.signals)
        }
        let guardResult = burstContaminationGuard(metrics: metrics, burstValleySec: burstValleySec, config: config)
        let signals = gate.signals + [guardResult.signal]
        guard guardResult.passed else {
            return .rejected(reason: .burstContamination, signals: signals)
        }
        let tier = gate.usedLongMetrics ? "long_cvcv2lv" : "short_compactness"
        let decisionPath = "tsw1/gate=\(tier)/guard=\(guardResult.provenance)/pass"
        let candidate = TonicStructuralWindowCandidate(
            span: span, metrics: metrics, source: source, signals: signals,
            reviewRequired: false, boundaryReason: nil, decisionPath: decisionPath)
        return .accepted(candidate)
    }

    // MARK: Helpers

    private static func maxConsecutiveTrue(_ flags: [Bool]) -> Int {
        var best = 0, current = 0
        for flag in flags {
            if flag { current += 1; best = Swift.max(best, current) } else { current = 0 }
        }
        return best
    }
}
