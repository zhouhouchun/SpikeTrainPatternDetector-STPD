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
    /// The calibrated state-support contract found no valid maximum-consensus core, or a deviation
    /// geometry that cannot automatically support a state.
    case stateSupportInsufficient
    case notCompact                // short window: an ISI fell outside median*[low, high]
    case adjacentRatioExceeded     // a single adjacent ISI step jumped more than the allowed ratio
    case cvExceeded                // long window: CV over the max
    case cv2Exceeded               // long window: CV2 over the max
    case lvExceeded                // long window: LV over the max
    case burstContamination        // too many / a run of sub-burst-floor ISIs (post-window veto)
    case invalidNextISI            // the next ISI failed QC (artifact / sub-floor) — a real edge, not tonic

    public var message: String {
        switch self {
        case .insufficientData: return "fewer than two valid ISIs for a regularity test"
        case .stateSupportInsufficient: return "the state-support core/deviation contract was not satisfied"
        case .notCompact: return "an ISI fell outside the median compactness band"
        case .adjacentRatioExceeded: return "an adjacent ISI step exceeded the allowed ratio"
        case .cvExceeded: return "CV exceeded the tonic maximum"
        case .cv2Exceeded: return "CV2 exceeded the tonic maximum"
        case .lvExceeded: return "LV exceeded the tonic maximum"
        case .burstContamination: return "window contains burst-floor contamination"
        case .invalidNextISI: return "the next ISI failed QC (artifact or sub-floor)"
        }
    }
}

/// TSW-2A — the family/magnitude ROUTE a regularity-passing window is classified into. Regularity alone
/// does not make a window classic tonic; its central magnitude must be compatible with the tonic family.
/// Only `.classicTonic` may be consumed as authoritative classic tonic; the other routes keep the window
/// as structural evidence without contaminating classic tonic. The route is structural evidence here;
/// final-label authority belongs to the consuming detector.
public enum TonicStructuralWindowRoute: String, Hashable, Sendable {
    case classicTonic              // regular AND magnitude-compatible with the tonic family
    case highFrequencyTonic        // regular but too fast for classic tonic (short/moderate run)
    case highFrequencySpiking      // regular but too fast, and long/dense (HFS-like)
    case possibleTonicReview       // just below the tonic floor, or no reliable magnitude guard — review
    case tooFastForClassicTonic    // clearly fast with no trustworthy boundary to sub-type
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
    /// TSW-2A: ratio a classic-tonic window's median must clear above a TRUSTED burst boundary (mirrors
    /// StatePatternDetector's burstSeedUpper*1.25). Dimensionless / scale-free.
    public var classicTonicBurstFloorRatio: Double
    /// TSW-2A: review buffer width just below the classic-tonic floor — median in [floor/ratio, floor)
    /// routes to review rather than a fast family. Dimensionless.
    public var tonicReviewBufferRatio: Double
    /// TSW-2A: a burst valley is trusted as a magnitude boundary only when its support count is at least
    /// this — a single-point / low-support valley is a refractory artifact, not a mode boundary.
    public var minBurstSupportCountForValley: Int
    /// TSW-2A: spike-count tier separating a long/dense high-frequency-spiking run from high-frequency
    /// tonic. A dimensionless COUNT (scale-free).
    public var highFrequencySpikingMinSpikes: Int
    /// TSW-2A: PHYSIOLOGICAL fast/HF exclusion — a classic-tonic window's median must be at least this
    /// multiple of the QC `refractoryFloorSec` (a refractory-RELATIVE guard, NOT an absolute-ms cutoff).
    /// Below it a regular window is too fast for classic tonic regardless of any train-local quantile.
    /// This is what stops a fast-dominated train's low q25 from confirming fast regular windows as classic.
    public var classicTonicMinRefractoryMultiple: Double
    /// Optional shared state-support policy. When present, its core/deviation accounting is emitted as
    /// audit evidence for every evaluated tonic window. The historical compactness/CV path remains
    /// authoritative unless `enforcesStateSupportEligibility` is explicitly enabled by a calibrated run.
    public var stateSupportSettings: StateSupportClassifierSettings?
    /// When true, a window that fails the shared core/deviation support policy cannot become an automatic
    /// tonic candidate. It defaults to false while the policy is calibrated against real recordings, so
    /// callers can inspect the evidence without silently changing labels.
    public var enforcesStateSupportEligibility: Bool

    public init(
        thresholds: StructuralEvidenceThresholds = StructuralEvidenceThresholds(),
        refractoryFloorSec: Double = 0.001,
        refractoryFloorMultiplier: Double = 3.0,
        adjacentRatioMax: Double? = nil,
        longWindowMinISI: Int = 5,
        classicTonicBurstFloorRatio: Double = 1.25,
        tonicReviewBufferRatio: Double = 1.25,
        minBurstSupportCountForValley: Int = 2,
        highFrequencySpikingMinSpikes: Int = 30,
        classicTonicMinRefractoryMultiple: Double = 15.0,
        stateSupportSettings: StateSupportClassifierSettings? = nil,
        enforcesStateSupportEligibility: Bool = false
    ) {
        self.thresholds = thresholds
        self.refractoryFloorSec = refractoryFloorSec
        self.refractoryFloorMultiplier = refractoryFloorMultiplier
        self.adjacentRatioMax = adjacentRatioMax ?? thresholds.tonicLocalRatioHigh
        self.longWindowMinISI = longWindowMinISI
        self.classicTonicBurstFloorRatio = classicTonicBurstFloorRatio
        self.tonicReviewBufferRatio = tonicReviewBufferRatio
        self.minBurstSupportCountForValley = minBurstSupportCountForValley
        self.highFrequencySpikingMinSpikes = highFrequencySpikingMinSpikes
        self.classicTonicMinRefractoryMultiple = classicTonicMinRefractoryMultiple
        self.stateSupportSettings = stateSupportSettings
        self.enforcesStateSupportEligibility = enforcesStateSupportEligibility
    }
}

/// TSW-3 — which edge of a window a refinement acted on.
public enum TonicWindowEdge: String, Hashable, Sendable {
    case low       // the fast/burst side (leading ISIs)
    case high      // the slow/pause side (trailing ISIs)
}

/// TSW-3 — boundary-refinement provenance: how the refined span differs from the raw scan span it came
/// from. All counts are dimensionless. Refinement only ever SHRINKS the fast/slow edges or recovers a
/// SINGLE bounded bridge ISI — it never invents structure, so `originalSpan ⊇ refined` except for the
/// at-most-one bridged ISI.
public struct TonicWindowRefinement: Hashable, Sendable {
    /// The pre-refinement (raw scan) span this candidate was derived from.
    public let originalStartISIIndex: Int
    public let originalEndISIIndex: Int
    /// Leading ISIs trimmed off the LOW (fast/burst) side because they sat below the classic-tonic floor.
    public let lowSideTrimmed: Int
    /// Trailing ISIs trimmed off the HIGH (slow/pause) side because they were pause-like (≥ pause floor).
    public let highSideTrimmed: Int
    /// Bridge ISIs recovered by merging two adjacent runs across a single bounded ISI (0 or 1).
    public let bridgeCount: Int
    /// Which side the bridge extended toward (nil when no bridge).
    public let bridgeSide: TonicWindowEdge?
    /// True when a TRUSTED D3 burst valley (support ≥ min) bounded the low-side/bridge decisions, rather
    /// than a refractory-relative fallback floor.
    public let usedReliableBurstBoundary: Bool

    public init(
        originalStartISIIndex: Int, originalEndISIIndex: Int,
        lowSideTrimmed: Int, highSideTrimmed: Int, bridgeCount: Int,
        bridgeSide: TonicWindowEdge?, usedReliableBurstBoundary: Bool
    ) {
        self.originalStartISIIndex = originalStartISIIndex
        self.originalEndISIIndex = originalEndISIIndex
        self.lowSideTrimmed = lowSideTrimmed
        self.highSideTrimmed = highSideTrimmed
        self.bridgeCount = bridgeCount
        self.bridgeSide = bridgeSide
        self.usedReliableBurstBoundary = usedReliableBurstBoundary
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
    /// TSW-2A family/magnitude route. `.classicTonic` only when the window is regular AND magnitude-
    /// compatible with the tonic family; otherwise a non-classic route (kept as structural evidence).
    public let route: TonicStructuralWindowRoute
    /// TSW-3 boundary-refinement provenance; nil when the span equals the raw scan span (source ≠ .refined).
    public let refinement: TonicWindowRefinement?
    /// TSW-3 the raw scan span this refined candidate came from; nil when unrefined.
    public let originalSpan: ISISpan?

    public init(
        span: ISISpan, metrics: ISISpanMetrics, source: TonicWindowSource,
        signals: [EvidenceSignal], reviewRequired: Bool,
        boundaryReason: TonicWindowBoundaryReason?, decisionPath: String,
        route: TonicStructuralWindowRoute = .classicTonic,
        refinement: TonicWindowRefinement? = nil,
        originalSpan: ISISpan? = nil
    ) {
        self.span = span; self.metrics = metrics; self.source = source
        self.signals = signals; self.reviewRequired = reviewRequired
        self.boundaryReason = boundaryReason; self.decisionPath = decisionPath
        self.route = route
        self.refinement = refinement; self.originalSpan = originalSpan
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

        if let supportSettings = config.stateSupportSettings {
            let support = StateSupportClassifier.analyze(
                vals.enumerated().map { offset, value in
                    StateSupportISIObservation(sourceIndex: offset + 1, valueSec: value)
                },
                settings: supportSettings
            )
            let eligible = support.isEligibleForAutomaticTonic(settings: supportSettings)
            signals += [
                EvidenceSignal(
                    key: "tonic_state_support_n_support", status: .pass, role: .audit,
                    observedValue: Double(support.nSupport),
                    message: "n_raw=\(support.nRaw) n_valid=\(support.nValid)"
                ),
                EvidenceSignal(
                    key: "tonic_state_support_n_core", status: .pass, role: .audit,
                    observedValue: Double(support.nCore),
                    message: "maximum_consensus_core"
                ),
                EvidenceSignal(
                    key: "tonic_state_support_ordinary_deviations", status: eligible ? .pass : .fail,
                    role: .audit, observedValue: Double(support.ordinaryDeviationCount),
                    requiredValue: Double(supportSettings.maximumAutomaticDeviationCount),
                    message: "competing=\(support.competingExcursionCount) invalid=\(support.invalidCount)"
                ),
                EvidenceSignal(
                    key: "tonic_state_support_automatic_eligibility", status: eligible ? .pass : .fail,
                    role: .eligibility, observedValue: Double(support.nCore),
                    requiredValue: Double(support.ordinaryDeviationCount),
                    message: "n_support=\(support.nSupport)"
                )
            ]
            if config.enforcesStateSupportEligibility && !eligible {
                return (false, signals, .stateSupportInsufficient, long)
            }
        }

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

    /// TSW-2A — classic-tonic FAMILY / MAGNITUDE routing (scale-free). A regularity-passing window is
    /// classic tonic only when its central magnitude (median) is compatible with the tonic family, i.e.
    /// SLOWER than the burst/HF region. Evidence order: a TRUSTED train-local D3 burst valley (× ratio) →
    /// a train-local tonic core-lower fallback → otherwise NO reliable guard (route to review). The
    /// refractory floor is only a protective floor-of-floors, never the classifier. Degenerate / low-
    /// support valleys are not trusted. Returns the route, a provenance signal, a trust tag, and the floor.
    /// TSW-2A/3 — the classic-tonic magnitude FLOOR and its reliability, computed from the distribution
    /// evidence ALONE (independent of any particular window's metrics). Shared by the routing gate and the
    /// TSW-3 low-side trim so both use the SAME floor. Reliability classes / `trust`:
    ///  d3_valley           — a trusted burst/HF boundary (support >= min; degenerate valleys excluded)
    ///  d3_tonic_core_lower — a trusted classic-tonic lower (q25 clear of the fast/HF regime)
    ///  ambiguous_fast_q25  — a train-local q25 that itself sits in the fast/HF regime (NOT trusted)
    ///  unavailable         — no distribution guard at all
    public static func classicTonicMagnitudeFloor(
        burstValleySec: Double?,
        burstValleySupportCount: Int?,
        fallbackFloorSec: Double?,
        config: TonicStructuralWindowConfig
    ) -> (floorSec: Double, distReliable: Bool, reliableBurstBoundary: Bool, trust: String) {
        // PHYSIOLOGICAL fast/HF exclusion floor — refractory-relative (scale-free), independent of any
        // train-local quantile. A regular window below this is too fast to be classic tonic, period.
        let physiologicalFloor = config.classicTonicMinRefractoryMultiple * config.refractoryFloorSec
        var distFloor: Double?
        var distReliable = false
        var reliableBurstBoundary = false
        var trust: String
        if let valley = burstValleySec, valley.isFinite, valley > 0,
           (burstValleySupportCount ?? 0) >= config.minBurstSupportCountForValley {
            distFloor = valley * config.classicTonicBurstFloorRatio      // burst boundary → need 25% above
            distReliable = true
            reliableBurstBoundary = true
            trust = "d3_valley"
        } else if let fallback = fallbackFloorSec, fallback.isFinite, fallback > 0 {
            distFloor = fallback
            if fallback >= physiologicalFloor {
                distReliable = true                                      // q25 is clear of the HF regime
                trust = "d3_tonic_core_lower"
            } else {
                distReliable = false                                     // q25 in the HF regime → not trusted
                trust = "ambiguous_fast_q25"
            }
        } else {
            trust = "unavailable"
        }
        let floorSec = distReliable ? Swift.max(physiologicalFloor, distFloor ?? physiologicalFloor) : physiologicalFloor
        return (floorSec, distReliable, reliableBurstBoundary, trust)
    }

    public static func classicTonicMagnitudeGate(
        metrics: ISISpanMetrics,
        burstValleySec: Double?,
        burstValleySupportCount: Int?,
        fallbackFloorSec: Double?,
        config: TonicStructuralWindowConfig
    ) -> (route: TonicStructuralWindowRoute, signal: EvidenceSignal, trust: String, floorSec: Double) {
        let physiologicalFloor = config.classicTonicMinRefractoryMultiple * config.refractoryFloorSec
        let floorInfo = classicTonicMagnitudeFloor(
            burstValleySec: burstValleySec, burstValleySupportCount: burstValleySupportCount,
            fallbackFloorSec: fallbackFloorSec, config: config)

        let median = metrics.medianSec ?? .infinity
        let route: TonicStructuralWindowRoute
        var floor = physiologicalFloor
        if median < physiologicalFloor {
            // Hard physiological exclusion — clearly too fast for classic tonic.
            route = .tooFastForClassicTonic
        } else if floorInfo.distReliable {
            // Above the physiological floor AND with reliable distribution evidence: classic tonic requires
            // magnitude compatibility with BOTH floors.
            floor = floorInfo.floorSec
            if median >= floor {
                route = .classicTonic
            } else if median >= floor / config.tonicReviewBufferRatio {
                route = .possibleTonicReview
            } else {
                route = metrics.nSpikes >= config.highFrequencySpikingMinSpikes
                    ? .highFrequencySpiking : .highFrequencyTonic        // fast vs this train's own boundary
            }
        } else {
            // Above the physiological floor but no RELIABLE distribution evidence (ambiguous fast-dominated
            // q25, or none) → cannot CONFIRM classic tonic; keep as review.
            route = .possibleTonicReview
        }
        let signal = EvidenceSignal(
            key: "tonic_magnitude_floor", status: route == .classicTonic ? .pass : .fail,
            role: .priorCompatibility, observedValue: metrics.medianSec, requiredValue: floor,
            message: "route=\(route.rawValue) floor=\(floorInfo.trust)")
        return (route, signal, floorInfo.trust, floor)
    }

    /// Evaluate one span of a real train end-to-end: compute `ISISpanMetrics` (QC-filtered, parity with
    /// the D3/D4 layers), apply the regularity gate then the burst guard, then TSW-2A family/magnitude
    /// routing; return an accepted candidate (with its route) or a rejection with its boundary reason.
    public static func evaluateWindow(
        train: SpikeTrain, span: ISISpan, source: TonicWindowSource = .seed,
        burstValleySec: Double?, burstValleySupportCount: Int? = nil, fallbackFloorSec: Double? = nil,
        config: TonicStructuralWindowConfig = TonicStructuralWindowConfig()
    ) -> TonicWindowEvaluation {
        let metrics = ISISpanMetrics.from(train: train, span: span, thresholds: config.thresholds)
        return evaluate(metrics: metrics, span: span, source: source, burstValleySec: burstValleySec,
                        burstValleySupportCount: burstValleySupportCount, fallbackFloorSec: fallbackFloorSec, config: config)
    }

    /// Evaluate from already-computed metrics (used by tests and by the TSW-2/3 scan to avoid recomputing).
    public static func evaluate(
        metrics: ISISpanMetrics, span: ISISpan, source: TonicWindowSource = .seed,
        burstValleySec: Double?, burstValleySupportCount: Int? = nil, fallbackFloorSec: Double? = nil,
        config: TonicStructuralWindowConfig = TonicStructuralWindowConfig()
    ) -> TonicWindowEvaluation {
        let gate = regularityGate(metrics: metrics, config: config)
        guard gate.passed else {
            return .rejected(reason: gate.failReason ?? .insufficientData, signals: gate.signals)
        }
        let guardResult = burstContaminationGuard(metrics: metrics, burstValleySec: burstValleySec, config: config)
        guard guardResult.passed else {
            return .rejected(reason: .burstContamination, signals: gate.signals + [guardResult.signal])
        }
        // TSW-2A: family/magnitude routing (regularity + burst-clean is necessary but NOT sufficient for
        // classic tonic). Routing does not change acceptance/expansion — only the emitted family.
        let magnitude = classicTonicMagnitudeGate(
            metrics: metrics, burstValleySec: burstValleySec, burstValleySupportCount: burstValleySupportCount,
            fallbackFloorSec: fallbackFloorSec, config: config)
        let signals = gate.signals + [guardResult.signal, magnitude.signal]
        let tier = gate.usedLongMetrics ? "long_cvcv2lv" : "short_compactness"
        let decisionPath = "tsw1/gate=\(tier)/guard=\(guardResult.provenance)/pass"
            + "/route=\(magnitude.route.rawValue)/floor=\(magnitude.trust)"
        let candidate = TonicStructuralWindowCandidate(
            span: span, metrics: metrics, source: source, signals: signals,
            reviewRequired: false, boundaryReason: nil, decisionPath: decisionPath, route: magnitude.route)
        return .accepted(candidate)
    }

    // MARK: TSW-2 — sequence-level scan

    /// Stride-1 sequence scan: slides a minimum seed window (`max(2, tonicMinSpikes - 1)` ISIs) across the
    /// train's valid ISI slots (indices `1...`), evaluates each seed with the TSW-1 gate + burst guard,
    /// merges overlapping/contiguous PASSING seeds only when the full merged span re-validates, emits
    /// MAXIMAL DISJOINT candidates, and records right-edge boundary provenance. It reads the raw QC ISI
    /// series directly — NOT a global/adaptive tonic band. Deterministic: the result is sorted by
    /// (startISIIndex, endISIIndex) and independent of incidental iteration order.
    public static func scan(
        train: SpikeTrain,
        config: TonicStructuralWindowConfig = TonicStructuralWindowConfig(),
        burstValleySec: Double? = nil,
        minTonicSpikes: Int? = nil,
        burstValleySupportCount: Int? = nil,
        fallbackFloorSec: Double? = nil
    ) -> [TonicStructuralWindowCandidate] {
        let lastValidIndex = train.isiSec.count - 1              // ISI slots live at 1...lastValidIndex
        guard lastValidIndex >= 1 else { return [] }
        let baseMinSpikes = minTonicSpikes ?? config.thresholds.tonicMinSpikes
        let seedISI = Swift.max(2, baseMinSpikes - 1)
        guard lastValidIndex >= seedISI else { return [] }
        let floor = Swift.max(0, config.thresholds.minimumValidISISec)

        // A span is a valid tonic window only when every ISI in it is QC-valid AND it clears the TSW-1
        // regularity gate + burst guard.
        func accepts(_ start: Int, _ end: Int) -> Bool {
            guard spanHasOnlyValidISIs(train, start, end, floor: floor) else { return false }
            let span = ISISpan(trainID: train.id, startISIIndex: start, endISIIndex: end, familyHint: .tonic)
            if case .accepted = evaluateWindow(train: train, span: span, burstValleySec: burstValleySec, config: config) {
                return true
            }
            return false
        }

        // 1) Passing minimum seeds (stride 1).
        var passingStarts: [Int] = []
        for start in 1...(lastValidIndex - seedISI + 1) where accepts(start, start + seedISI - 1) {
            passingStarts.append(start)
        }
        guard !passingStarts.isEmpty else { return [] }

        // 2) From EACH passing start, greedily build the LONGEST valid span by expanding one ISI at a time
        //    and re-validating the FULL span (never across a QC-invalid ISI). Building from every start
        //    independently makes each start's best candidate order-independent — so a later-starting seed
        //    can still form a longer candidate than an earlier one whose forward merge failed.
        var provisional: [(start: Int, end: Int)] = []
        for start in passingStarts {
            var end = start + seedISI - 1
            while end + 1 <= lastValidIndex, accepts(start, end + 1) { end += 1 }
            provisional.append((start: start, end: end))
        }

        // 3) Select MAXIMAL DISJOINT candidates: longest first, ties by earliest start then earliest end;
        //    keep a candidate only if it does not overlap an already-selected one (longer wins).
        provisional.sort { a, b in
            let lengthA = a.end - a.start, lengthB = b.end - b.start
            if lengthA != lengthB { return lengthA > lengthB }
            if a.start != b.start { return a.start < b.start }
            return a.end < b.end
        }
        var selected: [(start: Int, end: Int)] = []
        for candidate in provisional {
            let overlaps = selected.contains { !(candidate.end < $0.start || candidate.start > $0.end) }
            if !overlaps { selected.append(candidate) }
        }

        // 4) Finalize (metrics + boundary provenance + source) and return sorted by (startISIIndex, endISIIndex).
        var candidates: [TonicStructuralWindowCandidate] = []
        for span in selected {
            let source: TonicWindowSource = (span.end - span.start + 1) > seedISI ? .merged : .seed
            if let candidate = finalizeCandidate(
                train: train, startISIIndex: span.start, endISIIndex: span.end, source: source,
                burstValleySec: burstValleySec, burstValleySupportCount: burstValleySupportCount,
                fallbackFloorSec: fallbackFloorSec, config: config, lastValidIndex: lastValidIndex, floor: floor) {
                candidates.append(candidate)
            }
        }
        return candidates.sorted {
            $0.span.startISIIndex != $1.span.startISIIndex
                ? $0.span.startISIIndex < $1.span.startISIIndex
                : $0.span.endISIIndex < $1.span.endISIIndex
        }
    }

    // MARK: TSW-3 — boundary refinement

    /// Refine the raw `scan` candidates by (a) trimming ISIs that are incompatible with classic tonic off
    /// the boundaries — ASYMMETRICALLY: the LOW (fast/burst) side is trimmed against the classic-tonic
    /// magnitude floor, while the HIGH (slow/pause) side is trimmed only for genuinely pause-like ISIs —
    /// and (b) recovering AT MOST ONE bounded bridge ISI to merge two adjacent tonic runs separated by a
    /// single tonic-magnitude-compatible ISI (never a burst-floor or pause-floor ISI, so bursts and pauses
    /// are never swallowed). Refinement only SHRINKS the edges or adds the single bridge — it never invents
    /// structure, so the output stays disjoint and sorted. A candidate whose span is unchanged is returned
    /// verbatim (source preserved); a changed one is re-evaluated for fresh metrics/route and carries TSW-3
    /// provenance (`refinement` + `originalSpan`, source = .refined).
    public static func scanRefined(
        train: SpikeTrain,
        config: TonicStructuralWindowConfig = TonicStructuralWindowConfig(),
        burstValleySec: Double? = nil,
        minTonicSpikes: Int? = nil,
        burstValleySupportCount: Int? = nil,
        fallbackFloorSec: Double? = nil,
        pauseFloorSec: Double? = nil
    ) -> [TonicStructuralWindowCandidate] {
        let base = scan(
            train: train, config: config, burstValleySec: burstValleySec, minTonicSpikes: minTonicSpikes,
            burstValleySupportCount: burstValleySupportCount, fallbackFloorSec: fallbackFloorSec)
        guard !base.isEmpty else { return [] }

        let lastValidIndex = train.isiSec.count - 1
        let floor = Swift.max(0, config.thresholds.minimumValidISISec)
        let baseMinSpikes = minTonicSpikes ?? config.thresholds.tonicMinSpikes
        let seedISI = Swift.max(2, baseMinSpikes - 1)

        // The SAME classic-tonic floor the magnitude gate uses (distribution-only, window-independent), plus
        // the burst-contamination floor. Both bound the trim/bridge so TSW-3 agrees with TSW-2A routing.
        let floorInfo = classicTonicMagnitudeFloor(
            burstValleySec: burstValleySec, burstValleySupportCount: burstValleySupportCount,
            fallbackFloorSec: fallbackFloorSec, config: config)
        let burstFloor = effectiveBurstFloorSec(burstValleySec: burstValleySec, config: config).floorSec

        // 1) Asymmetric edge trim (shrink-only, independent per candidate).
        struct Work { var start: Int; var end: Int; let base: TonicStructuralWindowCandidate
                      var low: Int; var high: Int; var bridge: Int }
        var items: [Work] = base.map {
            Work(start: $0.span.startISIIndex, end: $0.span.endISIIndex, base: $0, low: 0, high: 0, bridge: 0)
        }
        for k in items.indices {
            // LOW side: drop leading ISIs below the classic-tonic magnitude floor (fast/burst contamination),
            // keeping at least a seed's worth of ISIs. GUARD: only trim when the window actually has a tonic
            // core ABOVE the floor to preserve — a uniformly-fast window (every ISI below the floor) has no
            // boundary to clean, so it is left intact for the magnitude gate to route non-classic rather than
            // arbitrarily truncated down to the seed length.
            let hasTonicCore = (items[k].start...items[k].end).contains {
                (validISIValue(train, $0, floor: floor) ?? 0) >= floorInfo.floorSec
            }
            if hasTonicCore {
                while items[k].end - items[k].start + 1 > seedISI,
                      let v = validISIValue(train, items[k].start, floor: floor), v < floorInfo.floorSec {
                    items[k].start += 1; items[k].low += 1
                }
            }
            // HIGH side: drop trailing PAUSE-like ISIs only (asymmetric — a much looser slow-side condition
            // that never trims legitimate slow tonic ISIs). Requires a known pause floor.
            if let pause = pauseFloorSec {
                while items[k].end - items[k].start + 1 > seedISI,
                      let v = validISIValue(train, items[k].end, floor: floor), v >= pause {
                    items[k].end -= 1; items[k].high += 1
                }
            }
        }

        // 2) Bounded single-bridge pass: merge candidate i with i+1 when EXACTLY ONE bounded ISI separates
        //    them and the merged span re-passes the gate/guard with the adjacent-ratio cap relaxed (CV/CV2/
        //    LV kept strict — so only the single junction step is tolerated). The bridge ISI must be neither
        //    burst-floor (fast) nor pause-floor (slow), so bursts/pauses are never bridged over.
        var relaxed = config
        relaxed.adjacentRatioMax = .infinity
        var merged: [Work] = []
        var i = 0
        while i < items.count {
            var cur = items[i]
            if i + 1 < items.count {
                let nxt = items[i + 1]
                let gapIndex = cur.end + 1
                let gapValue = validISIValue(train, gapIndex, floor: floor)
                let bounded = nxt.start == cur.end + 2                        // exactly one ISI between runs
                    && gapValue != nil
                    && (gapValue ?? 0) >= burstFloor                          // not burst (fast) contamination
                    && (pauseFloorSec == nil || (gapValue ?? .infinity) < pauseFloorSec!)   // not a pause ISI
                if bounded {
                    let mergedSpan = ISISpan(trainID: train.id, startISIIndex: cur.start,
                                             endISIIndex: nxt.end, familyHint: .tonic)
                    if case .accepted = evaluateWindow(
                        train: train, span: mergedSpan, burstValleySec: burstValleySec,
                        burstValleySupportCount: burstValleySupportCount, fallbackFloorSec: fallbackFloorSec,
                        config: relaxed) {
                        cur.end = nxt.end
                        cur.bridge = 1
                        merged.append(cur)
                        i += 2                                                // absorb the next candidate
                        continue
                    }
                }
            }
            merged.append(cur)
            i += 1
        }

        // 3) Finalize: unchanged spans return verbatim; changed spans are re-evaluated (bridged spans under
        //    the relaxed-adjacent config so the tolerated junction survives) and carry TSW-3 provenance.
        var out: [TonicStructuralWindowCandidate] = []
        for it in merged {
            let changed = it.start != it.base.span.startISIIndex
                || it.end != it.base.span.endISIIndex || it.bridge > 0
            guard changed else { out.append(it.base); continue }
            let useConfig = it.bridge > 0 ? relaxed : config
            guard let refined = finalizeCandidate(
                train: train, startISIIndex: it.start, endISIIndex: it.end, source: .refined,
                burstValleySec: burstValleySec, burstValleySupportCount: burstValleySupportCount,
                fallbackFloorSec: fallbackFloorSec, config: useConfig,
                lastValidIndex: lastValidIndex, floor: floor) else {
                out.append(it.base)                                          // refinement failed → keep raw
                continue
            }
            let refinement = TonicWindowRefinement(
                originalStartISIIndex: it.base.span.startISIIndex,
                originalEndISIIndex: it.base.span.endISIIndex,
                lowSideTrimmed: it.low, highSideTrimmed: it.high, bridgeCount: it.bridge,
                bridgeSide: it.bridge > 0 ? .high : nil,
                usedReliableBurstBoundary: floorInfo.reliableBurstBoundary)
            var path = refined.decisionPath + "/tsw3=refined"
            if it.low > 0 { path += "/low_trim=\(it.low)" }
            if it.high > 0 { path += "/high_trim=\(it.high)" }
            if it.bridge > 0 { path += "/bridge=\(it.bridge)" }
            out.append(TonicStructuralWindowCandidate(
                span: refined.span, metrics: refined.metrics, source: .refined, signals: refined.signals,
                reviewRequired: refined.reviewRequired, boundaryReason: refined.boundaryReason,
                decisionPath: path, route: refined.route, refinement: refinement,
                originalSpan: it.base.span))
        }
        return out.sorted {
            $0.span.startISIIndex != $1.span.startISIIndex
                ? $0.span.startISIIndex < $1.span.startISIIndex
                : $0.span.endISIIndex < $1.span.endISIIndex
        }
    }

    /// Build the final candidate for `[startISIIndex, endISIIndex]`, attaching right-edge boundary
    /// provenance: if the next ISI exists and the one-ISI-expanded span FAILS, record the failure reason
    /// (the failing ISI is NOT included) and flag `reviewRequired`. A train that simply ends (no next ISI)
    /// never sets `reviewRequired`.
    private static func finalizeCandidate(
        train: SpikeTrain, startISIIndex: Int, endISIIndex: Int, source: TonicWindowSource,
        burstValleySec: Double?, burstValleySupportCount: Int?, fallbackFloorSec: Double?,
        config: TonicStructuralWindowConfig, lastValidIndex: Int, floor: Double
    ) -> TonicStructuralWindowCandidate? {
        let span = ISISpan(trainID: train.id, startISIIndex: startISIIndex, endISIIndex: endISIIndex, familyHint: .tonic)
        guard case let .accepted(base) = evaluateWindow(
            train: train, span: span, source: source, burstValleySec: burstValleySec,
            burstValleySupportCount: burstValleySupportCount, fallbackFloorSec: fallbackFloorSec, config: config) else {
            return nil
        }
        var boundaryReason: TonicWindowBoundaryReason?
        var reviewRequired = false
        var decisionPath = base.decisionPath
        let nextIndex = endISIIndex + 1
        if nextIndex <= lastValidIndex {
            if !isValidISI(train.isiSec, nextIndex, floor: floor) {
                // The next ISI failed QC (artifact / sub-floor) — a real edge. Record it rather than
                // letting the raw span silently cross it.
                boundaryReason = .invalidNextISI
                reviewRequired = true
                decisionPath += "/right_boundary_fail=\(TonicWindowBoundaryReason.invalidNextISI.rawValue)"
            } else {
                let expanded = ISISpan(trainID: train.id, startISIIndex: startISIIndex, endISIIndex: nextIndex, familyHint: .tonic)
                if case let .rejected(reason, _) = evaluateWindow(
                    train: train, span: expanded, burstValleySec: burstValleySec, config: config) {
                    boundaryReason = reason
                    reviewRequired = true
                    decisionPath += "/right_boundary_fail=\(reason.rawValue)"
                }
            }
        }
        return TonicStructuralWindowCandidate(
            span: base.span, metrics: base.metrics, source: source, signals: base.signals,
            reviewRequired: reviewRequired, boundaryReason: boundaryReason, decisionPath: decisionPath,
            route: base.route)
    }

    // MARK: Helpers

    /// True when every ISI slot in `[start, end]` is QC-valid (finite and `>= floor`). A tonic structural
    /// window must be a contiguous run of valid ISIs — it never spans an artifact/sub-floor ISI.
    private static func spanHasOnlyValidISIs(_ train: SpikeTrain, _ start: Int, _ end: Int, floor: Double) -> Bool {
        let isi = train.isiSec
        guard start >= 1, end >= start, end < isi.count else { return false }
        for index in start...end where !isValidISI(isi, index, floor: floor) { return false }
        return true
    }

    private static func isValidISI(_ isi: [Double?], _ index: Int, floor: Double) -> Bool {
        guard index >= 1, index < isi.count, let value = isi[index], value.isFinite, value >= floor else { return false }
        return true
    }

    /// The QC-valid ISI value at `index`, or nil when the slot is a placeholder / sub-floor / non-finite.
    private static func validISIValue(_ train: SpikeTrain, _ index: Int, floor: Double) -> Double? {
        let isi = train.isiSec
        guard index >= 1, index < isi.count, let value = isi[index], value.isFinite, value >= floor else { return nil }
        return value
    }

    private static func maxConsecutiveTrue(_ flags: [Bool]) -> Int {
        var best = 0, current = 0
        for flag in flags {
            if flag { current += 1; best = Swift.max(best, current) } else { current = 0 }
        }
        return best
    }
}
