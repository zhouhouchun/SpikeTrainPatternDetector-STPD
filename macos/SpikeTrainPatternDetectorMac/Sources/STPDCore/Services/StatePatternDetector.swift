import Foundation

public enum HFSpikingInternalPacketizationPolicy: String, CaseIterable, Hashable, Sendable {
    /// Historical single-label behavior: an internally packet-like HFS run is
    /// rejected before event-track arbitration.
    case legacyRejectPacketLike = "legacy_reject_packet_like"

    /// Phase 1B behavior: internal packetization is provisional audit evidence.
    /// Final burst dominance is computed later from selected canonical events.
    case multiTrackEventOverlay = "multi_track_event_overlay"
}

public struct StatePatternDetectorSettings: Hashable, Sendable {
    public var isEnabled: Bool
    public var minValidISISec: Double
    public var burstSeedUpperSec: Double
    public var tonicLowerSec: Double
    public var tonicUpperSec: Double
    public var tonicBridgeUpperSec: Double
    public var tonicMaxBridgeISI: Int
    public var tonicBridgeFractionMax: Double
    public var tonicMinSpikes: Int
    public var tonicCVMax: Double
    public var tonicCV2Max: Double
    public var tonicLVMax: Double
    public var tonicMMMin: Double
    public var tonicMMMax: Double
    public var tonicBurstSeedFractionMax: Double
    public var highFrequencyTonicFloorSec: Double
    public var highFrequencyTonicUpperSec: Double
    public var highFrequencyTonicMinSpikes: Int
    public var highFrequencyTonicLowTailFractionMax: Double
    public var highFrequencyTonicCVMax: Double
    public var highFrequencyTonicCV2Max: Double
    public var highFrequencyTonicLVMax: Double
    public var highFrequencyTonicMMMax: Double
    public var highFrequencyTonicBurstCoreVetoMinISI: Int
    public var highFrequencySpikingShortUpperSec: Double
    public var highFrequencySpikingQ80MaxSec: Double
    public var highFrequencySpikingQ90MaxSec: Double
    public var highFrequencySpikingEpochBridgeSec: Double
    public var highFrequencySpikingPatternMaxISISec: Double?
    public var highFrequencySpikingPauseBreakSec: Double?
    public var highFrequencySpikingHardBreakSec: Double?
    public var highFrequencySpikingToleratedGapSec: Double?
    public var highFrequencySpikingMinSpikes: Int
    public var highFrequencySpikingMinDurationSec: Double
    public var highFrequencySpikingShortFractionMin: Double
    public var highFrequencySpikingAllowedLargeFraction: Double
    public var highFrequencySpikingMaxConsecutiveLargeISI: Int
    public var highFrequencySpikingInternalPacketizationPolicy: HFSpikingInternalPacketizationPolicy
    public var classicBoundaryContrastMin: Double
    public var structuralBurstSupportWeight: Double

    public init(
        isEnabled: Bool = true,
        minValidISISec: Double = 0.001,
        burstSeedUpperSec: Double = 0.010,
        tonicLowerSec: Double = 0,
        tonicUpperSec: Double = 0,
        tonicBridgeUpperSec: Double? = nil,
        tonicMaxBridgeISI: Int = 2,
        tonicBridgeFractionMax: Double = 0.20,
        tonicMinSpikes: Int = 5,
        tonicCVMax: Double = 0.30,
        tonicCV2Max: Double = 0.30,
        tonicLVMax: Double = 0.35,
        tonicMMMin: Double = 0.85,
        tonicMMMax: Double = 1.25,
        tonicBurstSeedFractionMax: Double = 0.20,
        highFrequencyTonicFloorSec: Double = 0,
        highFrequencyTonicUpperSec: Double = 0,
        highFrequencyTonicMinSpikes: Int = 6,
        highFrequencyTonicLowTailFractionMax: Double = 0.05,
        highFrequencyTonicCVMax: Double = 0.30,
        highFrequencyTonicCV2Max: Double = 0.30,
        highFrequencyTonicLVMax: Double = 0.35,
        highFrequencyTonicMMMax: Double = 1.25,
        highFrequencyTonicBurstCoreVetoMinISI: Int = 2,
        highFrequencySpikingShortUpperSec: Double = 0.020,
        highFrequencySpikingQ80MaxSec: Double? = nil,
        highFrequencySpikingQ90MaxSec: Double = 0.025,
        highFrequencySpikingEpochBridgeSec: Double = 0.035,
        highFrequencySpikingPatternMaxISISec: Double? = nil,
        highFrequencySpikingPauseBreakSec: Double? = nil,
        highFrequencySpikingHardBreakSec: Double? = nil,
        highFrequencySpikingToleratedGapSec: Double? = 0.075,
        highFrequencySpikingMinSpikes: Int = 30,
        highFrequencySpikingMinDurationSec: Double = 0,
        highFrequencySpikingShortFractionMin: Double = 0.70,
        highFrequencySpikingAllowedLargeFraction: Double = 0.25,
        highFrequencySpikingMaxConsecutiveLargeISI: Int = 3,
        highFrequencySpikingInternalPacketizationPolicy: HFSpikingInternalPacketizationPolicy = .multiTrackEventOverlay,
        classicBoundaryContrastMin: Double = 3.0,
        structuralBurstSupportWeight: Double = 0
    ) {
        self.isEnabled = isEnabled
        self.minValidISISec = positive(minValidISISec, fallback: 0.001)
        self.burstSeedUpperSec = positive(burstSeedUpperSec, fallback: 0.010)
        self.tonicLowerSec = positive(tonicLowerSec, fallback: self.minValidISISec)
        self.tonicUpperSec = max(self.tonicLowerSec, positive(tonicUpperSec, fallback: self.tonicLowerSec))
        let cleanedTonicBridgeUpper = tonicBridgeUpperSec.flatMap { $0.isFinite && $0 > 0 ? $0 : nil }
        self.tonicBridgeUpperSec = max(self.tonicUpperSec, cleanedTonicBridgeUpper ?? self.tonicUpperSec * 1.20)
        self.tonicMaxBridgeISI = max(0, tonicMaxBridgeISI)
        self.tonicBridgeFractionMax = clampedFraction(tonicBridgeFractionMax, fallback: 0.20)
        self.tonicMinSpikes = max(3, tonicMinSpikes)
        self.tonicCVMax = positive(tonicCVMax, fallback: 0.30)
        self.tonicCV2Max = positive(tonicCV2Max, fallback: 0.30)
        self.tonicLVMax = positive(tonicLVMax, fallback: 0.35)
        self.tonicMMMin = positive(tonicMMMin, fallback: 0.85)
        self.tonicMMMax = max(self.tonicMMMin, positive(tonicMMMax, fallback: 1.25))
        self.tonicBurstSeedFractionMax = clampedFraction(tonicBurstSeedFractionMax, fallback: 0.20)
        self.highFrequencyTonicFloorSec = positive(highFrequencyTonicFloorSec, fallback: self.minValidISISec)
        self.highFrequencyTonicUpperSec = max(
            self.highFrequencyTonicFloorSec,
            positive(highFrequencyTonicUpperSec, fallback: self.highFrequencyTonicFloorSec)
        )
        self.highFrequencyTonicMinSpikes = max(3, highFrequencyTonicMinSpikes)
        self.highFrequencyTonicLowTailFractionMax = clampedFraction(highFrequencyTonicLowTailFractionMax, fallback: 0.05)
        self.highFrequencyTonicCVMax = positive(highFrequencyTonicCVMax, fallback: self.tonicCVMax)
        self.highFrequencyTonicCV2Max = positive(highFrequencyTonicCV2Max, fallback: self.tonicCV2Max)
        self.highFrequencyTonicLVMax = positive(highFrequencyTonicLVMax, fallback: self.tonicLVMax)
        self.highFrequencyTonicMMMax = positive(highFrequencyTonicMMMax, fallback: 1.25)
        self.highFrequencyTonicBurstCoreVetoMinISI = max(1, highFrequencyTonicBurstCoreVetoMinISI)
        self.highFrequencySpikingShortUpperSec = positive(highFrequencySpikingShortUpperSec, fallback: 0.020)
        self.highFrequencySpikingQ90MaxSec = positive(highFrequencySpikingQ90MaxSec, fallback: 0.025)
        let cleanedQ80Max = highFrequencySpikingQ80MaxSec.flatMap { $0.isFinite && $0 > 0 ? $0 : nil }
        self.highFrequencySpikingQ80MaxSec = cleanedQ80Max ?? self.highFrequencySpikingQ90MaxSec
        self.highFrequencySpikingEpochBridgeSec = max(
            self.highFrequencySpikingQ90MaxSec,
            positive(highFrequencySpikingEpochBridgeSec, fallback: 0.035)
        )
        self.highFrequencySpikingPatternMaxISISec = highFrequencySpikingPatternMaxISISec.flatMap { $0.isFinite && $0 > 0 ? $0 : nil }
        self.highFrequencySpikingPauseBreakSec = highFrequencySpikingPauseBreakSec.flatMap { $0.isFinite && $0 > 0 ? $0 : nil }
        self.highFrequencySpikingHardBreakSec = highFrequencySpikingHardBreakSec.flatMap { $0.isFinite && $0 > 0 ? $0 : nil }
        self.highFrequencySpikingToleratedGapSec = highFrequencySpikingToleratedGapSec.flatMap { $0.isFinite && $0 > 0 ? $0 : nil }
        self.highFrequencySpikingMinSpikes = max(3, highFrequencySpikingMinSpikes)
        self.highFrequencySpikingMinDurationSec = max(0, highFrequencySpikingMinDurationSec.isFinite ? highFrequencySpikingMinDurationSec : 0)
        self.highFrequencySpikingShortFractionMin = clampedFraction(highFrequencySpikingShortFractionMin, fallback: 0.70)
        self.highFrequencySpikingAllowedLargeFraction = clampedFraction(highFrequencySpikingAllowedLargeFraction, fallback: 0.25)
        self.highFrequencySpikingMaxConsecutiveLargeISI = max(0, highFrequencySpikingMaxConsecutiveLargeISI)
        self.highFrequencySpikingInternalPacketizationPolicy = highFrequencySpikingInternalPacketizationPolicy
        self.classicBoundaryContrastMin = positive(classicBoundaryContrastMin, fallback: 3.0)
        self.structuralBurstSupportWeight = clampedFraction(structuralBurstSupportWeight, fallback: 0)
    }
}

public struct StatePatternDetectorTuning: Hashable, Sendable {
    public var tonicMinSpikes: Int
    public var tonicCVMax: Double
    public var tonicCV2Max: Double
    public var tonicLVMax: Double
    public var tonicMMMin: Double
    public var tonicMMMax: Double
    public var tonicBurstSeedFractionMax: Double
    public var highFrequencyTonicMinSpikes: Int
    public var highFrequencyTonicLowTailFractionMax: Double
    public var highFrequencyTonicCVMax: Double
    public var highFrequencyTonicCV2Max: Double
    public var highFrequencyTonicLVMax: Double
    public var highFrequencyTonicMMMax: Double
    public var highFrequencySpikingMinSpikes: Int
    public var highFrequencySpikingShortFractionMin: Double
    public var highFrequencySpikingAllowedLargeFraction: Double
    public var highFrequencySpikingMaxConsecutiveLargeISI: Int

    public init(
        tonicMinSpikes: Int = 5,
        tonicCVMax: Double = 0.30,
        tonicCV2Max: Double = 0.30,
        tonicLVMax: Double = 0.35,
        tonicMMMin: Double = 0.85,
        tonicMMMax: Double = 1.25,
        tonicBurstSeedFractionMax: Double = 0.20,
        highFrequencyTonicMinSpikes: Int = 6,
        highFrequencyTonicLowTailFractionMax: Double = 0.05,
        highFrequencyTonicCVMax: Double = 0.30,
        highFrequencyTonicCV2Max: Double = 0.30,
        highFrequencyTonicLVMax: Double = 0.35,
        highFrequencyTonicMMMax: Double = 1.25,
        highFrequencySpikingMinSpikes: Int = 30,
        highFrequencySpikingShortFractionMin: Double = 0.70,
        highFrequencySpikingAllowedLargeFraction: Double = 0.25,
        highFrequencySpikingMaxConsecutiveLargeISI: Int = 3
    ) {
        self.tonicMinSpikes = max(3, tonicMinSpikes)
        self.tonicCVMax = positive(tonicCVMax, fallback: 0.30)
        self.tonicCV2Max = positive(tonicCV2Max, fallback: 0.30)
        self.tonicLVMax = positive(tonicLVMax, fallback: 0.35)
        self.tonicMMMin = positive(tonicMMMin, fallback: 0.85)
        self.tonicMMMax = max(self.tonicMMMin, positive(tonicMMMax, fallback: 1.25))
        self.tonicBurstSeedFractionMax = clampedFraction(tonicBurstSeedFractionMax, fallback: 0.20)
        self.highFrequencyTonicMinSpikes = max(3, highFrequencyTonicMinSpikes)
        self.highFrequencyTonicLowTailFractionMax = clampedFraction(highFrequencyTonicLowTailFractionMax, fallback: 0.05)
        self.highFrequencyTonicCVMax = positive(highFrequencyTonicCVMax, fallback: 0.30)
        self.highFrequencyTonicCV2Max = positive(highFrequencyTonicCV2Max, fallback: 0.30)
        self.highFrequencyTonicLVMax = positive(highFrequencyTonicLVMax, fallback: 0.35)
        self.highFrequencyTonicMMMax = positive(highFrequencyTonicMMMax, fallback: 1.25)
        self.highFrequencySpikingMinSpikes = max(3, highFrequencySpikingMinSpikes)
        self.highFrequencySpikingShortFractionMin = clampedFraction(highFrequencySpikingShortFractionMin, fallback: 0.70)
        self.highFrequencySpikingAllowedLargeFraction = clampedFraction(highFrequencySpikingAllowedLargeFraction, fallback: 0.25)
        self.highFrequencySpikingMaxConsecutiveLargeISI = max(0, highFrequencySpikingMaxConsecutiveLargeISI)
    }
}

public enum StatePatternDetector {
    public static func detect(
        train: SpikeTrain,
        settings: StatePatternDetectorSettings = StatePatternDetectorSettings()
    ) -> ClassicAnchorDetectionResult {
        guard settings.isEnabled else {
            return ClassicAnchorDetectionResult(trainID: train.id, trainName: train.name, candidates: [])
        }

        let localContext = SpikeISILocalContextTable.build(
            for: train,
            settings: SpikeISILocalContextSettings(
                minValidISISec: settings.minValidISISec,
                halfWindow: max(3, settings.tonicMinSpikes)
            )
        )
        let candidates =
            detectHighFrequencySpiking(train: train, settings: settings, localContext: localContext) +
            detectHighFrequencyTonic(train: train, settings: settings, localContext: localContext) +
            detectTonic(train: train, settings: settings, localContext: localContext)

        return ClassicAnchorDetectionResult(
            trainID: train.id,
            trainName: train.name,
            candidates: candidates
        )
    }

    public static func detect(
        dataset: SpikeDataset,
        settings: StatePatternDetectorSettings = StatePatternDetectorSettings()
    ) -> [ClassicAnchorDetectionResult] {
        dataset.trains.map { detect(train: $0, settings: settings) }
    }

    /// Rebuild a state candidate after an event/gap split using the same
    /// label-specific gates as the primary state detector.
    ///
    /// The returned candidate has a deterministic ID derived from the original
    /// root candidate and fragment range, making repeated pipeline passes
    /// idempotent.
    public static func rebuildSplitCandidate(
        train: SpikeTrain,
        parent: ClassicAnchorCandidate,
        range: ClosedRange<Int>,
        settings: StatePatternDetectorSettings,
        splitByCandidateIDs: [String]
    ) -> ClassicAnchorCandidate? {
        let localContext = SpikeISILocalContextTable.build(
            for: train,
            settings: SpikeISILocalContextSettings(
                minValidISISec: settings.minValidISISec,
                halfWindow: max(3, settings.tonicMinSpikes)
            )
        )
        return rebuildSplitCandidate(
            train: train,
            parent: parent,
            range: range,
            settings: settings,
            localContext: localContext,
            splitByCandidateIDs: splitByCandidateIDs
        )
    }

    /// Context-sharing overload used by `StateEventCompatibilityResolver` when
    /// several fragments of the same train are rebuilt in one pass.
    public static func rebuildSplitCandidate(
        train: SpikeTrain,
        parent: ClassicAnchorCandidate,
        range: ClosedRange<Int>,
        settings: StatePatternDetectorSettings,
        localContext: SpikeISILocalContextTable,
        splitByCandidateIDs: [String]
    ) -> ClassicAnchorCandidate? {
        guard parent.finalLabel == .tonic ||
                parent.finalLabel == .highFrequencyTonic ||
                parent.finalLabel == .highFrequencySpiking,
              range.lowerBound > 0,
              range.lowerBound <= range.upperBound,
              range.upperBound < train.isiSec.count,
              let metrics = metrics(
                train: train,
                run: (range.lowerBound, range.upperBound),
                settings: settings,
                localContext: localContext
              ),
              metrics.nValidISI == metrics.nISI else {
            return nil
        }

        let rootID = stateSplitRootID(parent.id)
        let fragmentID = "\(rootID)::state-split::\(range.lowerBound)-\(range.upperBound)"
        let splitIDs = splitByCandidateIDs.sorted().joined(separator: ",")
        let commonExtra = [
            "state_track_split_fragment",
            "parent_candidate_id=\(parent.id)",
            "root_candidate_id=\(rootID)",
            "split_by_candidate_ids=\(splitIDs.isEmpty ? "none" : splitIDs)",
            "fragment_isi=\(range.lowerBound)-\(range.upperBound)",
            "fragment_n_isi=\(metrics.nISI)",
            "fragment_n_spikes=\(metrics.nSpikes)"
        ]

        switch parent.finalLabel {
        case .tonic:
            return rebuildTonicSplitCandidate(
                train: train,
                parent: parent,
                run: (range.lowerBound, range.upperBound),
                metrics: metrics,
                settings: settings,
                id: fragmentID,
                commonExtra: commonExtra
            )

        case .highFrequencyTonic:
            return rebuildHighFrequencyTonicSplitCandidate(
                train: train,
                parent: parent,
                run: (range.lowerBound, range.upperBound),
                metrics: metrics,
                settings: settings,
                id: fragmentID,
                commonExtra: commonExtra
            )

        case .highFrequencySpiking:
            return rebuildHighFrequencySpikingSplitCandidate(
                train: train,
                parent: parent,
                run: (range.lowerBound, range.upperBound),
                metrics: metrics,
                settings: settings,
                id: fragmentID,
                commonExtra: commonExtra
            )

        default:
            return nil
        }
    }

    private struct StateMetrics {
        let values: [Double]
        let nISI: Int
        let nValidISI: Int
        let nSpikes: Int
        let durationSec: Double
        let q10: Double?
        let q40: Double?
        let q50: Double?
        let q80: Double?
        let q90: Double?
        let q95: Double?
        let mean: Double?
        let max: Double?
        let cv: Double?
        let cv2: Double?
        let lv: Double?
        let mm: Double?
        let preGapSec: Double?
        let postGapSec: Double?
        let trainPercentileMedian: Double?
        let localPercentileMedian: Double?
        let localPercentileQ90: Double?
        let localRobustZMedian: Double?
        let localRobustZAbsQ80: Double?
        let localRobustZQ10: Double?
    }

    private struct HFSpikingPacketizationStats {
        let groupCount: Int
        let coveredISI: Int
        let coverage: Double
        let longestGroup: Int
        let burstDominated: Bool
        let packetLike: Bool
    }

    private static func detectHighFrequencySpiking(
        train: SpikeTrain,
        settings: StatePatternDetectorSettings,
        localContext: SpikeISILocalContextTable
    ) -> [ClassicAnchorCandidate] {
        guard train.spikeCount >= settings.highFrequencySpikingMinSpikes else {
            return []
        }

        let supportFlags = train.isiSec.indices.map { index -> Bool in
            guard index > 0,
                  let value = finiteValidISI(train.isiSec[index], settings: settings) else {
                return false
            }
            if let patternMax = settings.highFrequencySpikingPatternMaxISISec,
               value > patternMax + tolerance(for: patternMax) {
                return false
            }
            if let pauseBreak = settings.highFrequencySpikingPauseBreakSec,
               value + tolerance(for: pauseBreak) >= pauseBreak {
                return false
            }
            if value <= settings.highFrequencySpikingEpochBridgeSec + tolerance(for: settings.highFrequencySpikingEpochBridgeSec) {
                return true
            }
            let context = localContext.point(for: index)
            let locallyShort =
                (context?.localPercentile ?? 1) <= 0.25 ||
                (context?.localRobustZ ?? .infinity) <= -1.5
            let toleratedGap = highFrequencySpikingToleratedGapSec(settings: settings)
            return locallyShort && value <= toleratedGap + tolerance(for: toleratedGap)
        }

        var candidates: [ClassicAnchorCandidate] = []
        let supportRuns = mergeHighFrequencySpikingSupportRuns(
            boolRuns(supportFlags),
            train: train,
            settings: settings
        )

        for run in supportRuns {
            guard let metrics = metrics(train: train, run: run, settings: settings, localContext: localContext),
                  metrics.nSpikes >= settings.highFrequencySpikingMinSpikes,
                  metrics.durationSec >= settings.highFrequencySpikingMinDurationSec - tolerance(for: settings.highFrequencySpikingMinDurationSec) else {
                continue
            }

            let shortFraction = fraction(metrics.values) { $0 <= settings.highFrequencySpikingShortUpperSec + tolerance(for: settings.highFrequencySpikingShortUpperSec) }
            let q90ShortFraction = fraction(metrics.values) { $0 <= settings.highFrequencySpikingQ90MaxSec + tolerance(for: settings.highFrequencySpikingQ90MaxSec) }
            let bridgeFraction = fraction(metrics.values) { $0 <= settings.highFrequencySpikingEpochBridgeSec + tolerance(for: settings.highFrequencySpikingEpochBridgeSec) }
            let largeFlags = metrics.values.map { $0 > settings.highFrequencySpikingEpochBridgeSec + tolerance(for: settings.highFrequencySpikingEpochBridgeSec) }
            let largeFraction = mean(largeFlags.map { $0 ? 1 : 0 }) ?? 0
            let maxConsecutiveLarge = maxConsecutiveTrue(largeFlags)
            let toleratedGap = highFrequencySpikingToleratedGapSec(settings: settings)
            let toleratedFraction = fraction(metrics.values) { $0 <= toleratedGap + tolerance(for: toleratedGap) }
            let patternMaxExceeded = settings.highFrequencySpikingPatternMaxISISec.map { patternMax in
                metrics.values.contains { $0 > patternMax + tolerance(for: patternMax) }
            } ?? false
            let strictQ90Pass = (metrics.q90 ?? .infinity) <= settings.highFrequencySpikingQ90MaxSec + tolerance(for: settings.highFrequencySpikingQ90MaxSec)
            let robustQ80Pass = (metrics.q80 ?? .infinity) <= settings.highFrequencySpikingQ80MaxSec + tolerance(for: settings.highFrequencySpikingQ80MaxSec)
            let majorityPass = shortFraction >= max(0.50, settings.highFrequencySpikingShortFractionMin - 0.15) ||
                q90ShortFraction >= max(0.60, settings.highFrequencySpikingShortFractionMin - 0.10) ||
                bridgeFraction >= max(0.75, settings.highFrequencySpikingShortFractionMin)
            let sustainedRunPass = metrics.nSpikes >= max(settings.highFrequencySpikingMinSpikes, 30) &&
                bridgeFraction >= max(0.60, settings.highFrequencySpikingShortFractionMin - 0.10) &&
                toleratedFraction >= 0.90 - 1e-12 &&
                (metrics.q80 ?? .infinity) <= toleratedGap + tolerance(for: toleratedGap)
            let stateCompactPass = strictQ90Pass || (robustQ80Pass && majorityPass) || sustainedRunPass
            let requiredToleratedFraction = sustainedRunPass ? 0.90 : 0.95
            let gapTolerancePass = toleratedFraction >= requiredToleratedFraction - 1e-12
            let allowedLargeEff = max(settings.highFrequencySpikingAllowedLargeFraction, sustainedRunPass ? 0.40 : 0.30)
            let maxConsecutiveLargeEff = max(sustainedRunPass ? 3 : 2, settings.highFrequencySpikingMaxConsecutiveLargeISI)
            let largePass = largeFraction <= allowedLargeEff + 1e-12 &&
                maxConsecutiveLarge <= maxConsecutiveLargeEff
            let packetization = highFrequencySpikingPacketizationStats(
                train: train,
                run: run,
                metrics: metrics,
                settings: settings
            )
            let rejectsInternalPacketization =
                settings.highFrequencySpikingInternalPacketizationPolicy == .legacyRejectPacketLike
            let packetizationPass = !rejectsInternalPacketization || !packetization.packetLike
            if patternMaxExceeded || !stateCompactPass || !gapTolerancePass || !largePass || !packetizationPass {
                var rejectReasons: [String] = []
                if patternMaxExceeded {
                    rejectReasons.append("reject_hfs_pattern_max_exceeded")
                }
                if !stateCompactPass {
                    rejectReasons.append("reject_hfs_compactness_fail")
                }
                if !gapTolerancePass {
                    rejectReasons.append("reject_hfs_tolerated_gap_fraction_fail")
                }
                if !largePass {
                    rejectReasons.append("reject_hfs_large_isi_fraction_or_run_fail")
                }
                if !packetizationPass {
                    rejectReasons.append("reject_hfs_internal_burst_packetization")
                }
                let rejectDecisionPath = stateDecisionPath(
                    base: "reject_hf_spiking_state_candidate",
                    metrics: metrics,
                    extra: rejectReasons + [
                        "short_fraction=\(format(shortFraction))",
                        "q90_short_fraction=\(format(q90ShortFraction))",
                        "bridge_fraction=\(format(bridgeFraction))",
                        "large_fraction=\(format(largeFraction))",
                        "tolerated_fraction=\(format(toleratedFraction))",
                        "required_tolerated_fraction=\(format(requiredToleratedFraction))",
                        "sustained_run_pass=\(sustainedRunPass)",
                        "max_consecutive_large_isi=\(maxConsecutiveLarge)",
                        "self_packet_group_count=\(packetization.groupCount)",
                        "self_packet_coverage=\(format(packetization.coverage))",
                        "self_packet_longest_group=\(packetization.longestGroup)",
                        "self_packet_burst_dominated=\(packetization.burstDominated)",
                        "self_packet_like=\(packetization.packetLike)",
                        "event_state_policy=\(rejectsInternalPacketization ? "legacy_reject_internal_packetization" : "multitrack_defer_packetization_to_selected_events")",
                        "acceptance_route=reject"
                    ]
                )
                var rejected = candidate(
                    train: train,
                    run: run,
                    metrics: metrics,
                    label: .reject,
                    layer: "event_grammar_hf_spiking_state_diagnostic",
                    candidateClass: "rejected_hf_spiking_epoch",
                    gateStatus: "event_grammar_hf_spiking_state_reject",
                    decisionPath: rejectDecisionPath,
                    action: "reject",
                    score: 0,
                    priority: 0,
                    bandLower: settings.minValidISISec,
                    bandUpper: settings.highFrequencySpikingQ90MaxSec,
                    contrastMinRequired: settings.highFrequencySpikingShortFractionMin,
                    contrastGeomRequired: allowedLargeEff,
                    index: candidates.count + 1
                )
                rejected.hfSpikingQ80Sec = metrics.q80
                rejected.hfSpikingQ80MaxSec = settings.highFrequencySpikingQ80MaxSec
                rejected.hfSpikingQ90MaxSec = settings.highFrequencySpikingQ90MaxSec
                rejected.hfSpikingShortUpperSec = settings.highFrequencySpikingShortUpperSec
                rejected.hfSpikingEpochBridgeSec = settings.highFrequencySpikingEpochBridgeSec
                rejected.hfSpikingToleratedGapSec = toleratedGap
                rejected.hfSpikingPatternMaxISISec = settings.highFrequencySpikingPatternMaxISISec
                rejected.hfSpikingPauseBreakSec = settings.highFrequencySpikingPauseBreakSec
                rejected.hfSpikingShortFraction = shortFraction
                rejected.hfSpikingQ90ShortFraction = q90ShortFraction
                rejected.hfSpikingBridgeFraction = bridgeFraction
                rejected.hfSpikingLargeFraction = largeFraction
                rejected.hfSpikingToleratedFraction = toleratedFraction
                rejected.hfSpikingMaxConsecutiveLargeISI = maxConsecutiveLarge
                rejected.hfSpikingMinSpikesRequired = settings.highFrequencySpikingMinSpikes
                rejected.hfSpikingAcceptanceRoute = "reject"
                rejected.hfSpikingEmbeddedBurstCount = packetization.groupCount
                rejected.hfSpikingEmbeddedBurstGroupCount = packetization.groupCount
                rejected.hfSpikingEmbeddedBurstCoverage = packetization.coverage
                rejected.hfSpikingBurstDominated = packetization.burstDominated
                rejected.hfSpikingBurstPacketLike = packetization.packetLike
                candidates.append(rejected)
                continue
            }

            let score = 24 +
                0.08 * Double(metrics.nSpikes) +
                2.5 * shortFraction +
                2.0 * q90ShortFraction +
                1.5 * bridgeFraction -
                2.0 * largeFraction +
                (strictQ90Pass ? 1.0 : 0.4) +
                highFrequencyLocalShortScore(metrics)
            let decisionPath = stateDecisionPath(
                base: "merged_support_run_long_high_frequency_state",
                metrics: metrics,
                extra: [
                    "short_fraction=\(format(shortFraction))",
                    "q90_short_fraction=\(format(q90ShortFraction))",
                    "bridge_fraction=\(format(bridgeFraction))",
                    "large_fraction=\(format(largeFraction))",
                    "tolerated_fraction=\(format(toleratedFraction))",
                    "required_tolerated_fraction=\(format(requiredToleratedFraction))",
                    "sustained_run_pass=\(sustainedRunPass)",
                    "self_packet_group_count=\(packetization.groupCount)",
                    "self_packet_coverage=\(format(packetization.coverage))",
                    "self_packet_longest_group=\(packetization.longestGroup)",
                    "self_packet_burst_dominated=\(packetization.burstDominated)",
                    "self_packet_like=\(packetization.packetLike)",
                    "event_state_policy=\(rejectsInternalPacketization ? "legacy_packetization_guard_pass" : "multitrack_provisional_packetization_audit_then_selected_event_resolution")",
                    "acceptance_route=\(strictQ90Pass ? "strict_q90" : (sustainedRunPass ? "sustained_hfs_run" : "robust_q80_majority_state"))"
                ]
            )
            var hfs = candidate(
                train: train,
                run: run,
                metrics: metrics,
                label: .highFrequencySpiking,
                layer: "event_grammar_hf_spiking_state",
                candidateClass: "event_grammar_long_hf_spiking_epoch",
                gateStatus: "event_grammar_hf_spiking_state_pass",
                decisionPath: decisionPath,
                score: score,
                priority: 1_040,
                bandLower: settings.minValidISISec,
                bandUpper: settings.highFrequencySpikingQ90MaxSec,
                contrastMinRequired: settings.highFrequencySpikingShortFractionMin,
                contrastGeomRequired: allowedLargeEff,
                index: candidates.count + 1
            )
            hfs.hfSpikingQ80Sec = metrics.q80
            hfs.hfSpikingQ80MaxSec = settings.highFrequencySpikingQ80MaxSec
            hfs.hfSpikingQ90MaxSec = settings.highFrequencySpikingQ90MaxSec
            hfs.hfSpikingShortUpperSec = settings.highFrequencySpikingShortUpperSec
            hfs.hfSpikingEpochBridgeSec = settings.highFrequencySpikingEpochBridgeSec
            hfs.hfSpikingToleratedGapSec = toleratedGap
            hfs.hfSpikingPatternMaxISISec = settings.highFrequencySpikingPatternMaxISISec
            hfs.hfSpikingPauseBreakSec = settings.highFrequencySpikingPauseBreakSec
            hfs.hfSpikingShortFraction = shortFraction
            hfs.hfSpikingQ90ShortFraction = q90ShortFraction
            hfs.hfSpikingBridgeFraction = bridgeFraction
            hfs.hfSpikingLargeFraction = largeFraction
            hfs.hfSpikingToleratedFraction = toleratedFraction
            hfs.hfSpikingMaxConsecutiveLargeISI = maxConsecutiveLarge
            hfs.hfSpikingMinSpikesRequired = settings.highFrequencySpikingMinSpikes
            let baseAcceptanceRoute = strictQ90Pass
                ? "strict_q90"
                : (sustainedRunPass ? "sustained_hfs_run" : "robust_q80_majority_state")
            hfs.hfSpikingAcceptanceRoute = rejectsInternalPacketization
                ? baseAcceptanceRoute
                : "\(baseAcceptanceRoute)+provisional_self_packetization"
            hfs.hfSpikingEmbeddedBurstCount = packetization.groupCount
            hfs.hfSpikingEmbeddedBurstGroupCount = packetization.groupCount
            hfs.hfSpikingEmbeddedBurstCoverage = packetization.coverage
            // In multitrack mode, final dominance must come from selected
            // canonical events, not raw short-ISI runs inside this state.
            hfs.hfSpikingBurstDominated = rejectsInternalPacketization
                ? packetization.burstDominated
                : false
            hfs.hfSpikingBurstPacketLike = packetization.packetLike
            candidates.append(hfs)
        }

        return candidates
    }

    private static func highFrequencySpikingPacketizationStats(
        train: SpikeTrain,
        run: (start: Int, end: Int),
        metrics: StateMetrics,
        settings: StatePatternDetectorSettings
    ) -> HFSpikingPacketizationStats {
        let minPacketISI = 2
        var groups: [Int] = []
        var current = 0
        for index in run.start...run.end {
            let inBurstCore = finiteValidISI(train.isiSec[index], settings: settings).map {
                $0 <= settings.burstSeedUpperSec + tolerance(for: settings.burstSeedUpperSec)
            } ?? false
            if inBurstCore {
                current += 1
            } else {
                if current >= minPacketISI {
                    groups.append(current)
                }
                current = 0
            }
        }
        if current >= minPacketISI {
            groups.append(current)
        }

        let covered = groups.reduce(0, +)
        let coverage = Double(covered) / Double(max(1, metrics.nISI))
        let longest = groups.max() ?? 0
        let scaledMinGroups = max(3, Int(ceil(0.035 * Double(max(1, metrics.nISI)))))
        let manySeparatedPackets = groups.count >= scaledMinGroups && coverage >= 0.18
        let compactPacketDominance = groups.count >= 2 &&
            coverage >= 0.30 &&
            longest <= max(8, Int(ceil(0.50 * Double(max(1, metrics.nISI)))))
        let variableContext = (metrics.cv ?? 0) >= 0.55 ||
            (metrics.lv ?? 0) >= 0.40 ||
            (metrics.mm ?? 0) >= 3.0
        let burstDominated = groups.count >= 2 && coverage >= 0.45 && variableContext
        return HFSpikingPacketizationStats(
            groupCount: groups.count,
            coveredISI: covered,
            coverage: coverage,
            longestGroup: longest,
            burstDominated: burstDominated,
            packetLike: burstDominated || manySeparatedPackets || compactPacketDominance
        )
    }

    private static func mergeHighFrequencySpikingSupportRuns(
        _ runs: [(start: Int, end: Int)],
        train: SpikeTrain,
        settings: StatePatternDetectorSettings
    ) -> [(start: Int, end: Int)] {
        guard var current = runs.first else {
            return []
        }

        let toleratedGap = highFrequencySpikingToleratedGapSec(settings: settings)
        let maxGapCount = max(1, settings.highFrequencySpikingMaxConsecutiveLargeISI)
        let hardBreak = highFrequencySpikingHardBreakSec(settings: settings)
        var merged: [(start: Int, end: Int)] = []

        for next in runs.dropFirst() {
            let gapStart = current.end + 1
            let gapEnd = next.start - 1
            if gapStart <= gapEnd,
               canBridgeHighFrequencySpikingGap(
                gapStart...gapEnd,
                train: train,
                settings: settings,
                toleratedGap: toleratedGap,
                hardBreak: hardBreak,
                maxGapCount: maxGapCount
               ) {
                current.end = next.end
            } else if gapStart > gapEnd {
                current.end = max(current.end, next.end)
            } else {
                merged.append(current)
                current = next
            }
        }

        merged.append(current)
        return merged
    }

    private static func canBridgeHighFrequencySpikingGap(
        _ gap: ClosedRange<Int>,
        train: SpikeTrain,
        settings: StatePatternDetectorSettings,
        toleratedGap: Double,
        hardBreak: Double?,
        maxGapCount: Int
    ) -> Bool {
        guard gap.count <= maxGapCount else {
            return false
        }

        for index in gap {
            guard train.isiSec.indices.contains(index),
                  let value = finiteValidISI(train.isiSec[index], settings: settings),
                  value <= toleratedGap + tolerance(for: toleratedGap) else {
                return false
            }
            if let hardBreak,
               value + tolerance(for: hardBreak) >= hardBreak {
                return false
            }
        }
        return true
    }

    private static func highFrequencySpikingToleratedGapSec(settings: StatePatternDetectorSettings) -> Double {
        if let configured = settings.highFrequencySpikingToleratedGapSec {
            if let patternMax = settings.highFrequencySpikingPatternMaxISISec {
                return min(configured, patternMax)
            }
            return configured
        }

        let derived = max(
            0.060,
            min(
                0.120,
                max(
                    2.0 * settings.highFrequencySpikingEpochBridgeSec,
                    2.5 * settings.highFrequencySpikingQ90MaxSec
                )
            )
        )
        if let patternMax = settings.highFrequencySpikingPatternMaxISISec {
            return min(derived, patternMax)
        }
        return derived
    }

    private static func highFrequencySpikingHardBreakSec(settings: StatePatternDetectorSettings) -> Double? {
        [
            settings.highFrequencySpikingPauseBreakSec,
            settings.highFrequencySpikingPatternMaxISISec,
            settings.highFrequencySpikingHardBreakSec
        ]
        .compactMap { $0 }
        .filter { $0.isFinite && $0 > 0 }
        .min()
    }

    private static func mergeTonicSupportRuns(
        _ runs: [(start: Int, end: Int)],
        train: SpikeTrain,
        settings: StatePatternDetectorSettings
    ) -> [(start: Int, end: Int)] {
        guard settings.tonicMaxBridgeISI > 0,
              var current = runs.first else {
            return runs
        }

        var merged: [(start: Int, end: Int)] = []
        for next in runs.dropFirst() {
            let gapStart = current.end + 1
            let gapEnd = next.start - 1
            if gapStart <= gapEnd,
               canBridgeTonicGap(gapStart...gapEnd, train: train, settings: settings) {
                current.end = next.end
            } else if gapStart > gapEnd {
                current.end = max(current.end, next.end)
            } else {
                merged.append(current)
                current = next
            }
        }

        merged.append(current)
        return merged
    }

    private static func canBridgeTonicGap(
        _ gap: ClosedRange<Int>,
        train: SpikeTrain,
        settings: StatePatternDetectorSettings
    ) -> Bool {
        guard gap.count <= settings.tonicMaxBridgeISI else {
            return false
        }

        let bounds = tonicStructuralSearchBounds(train: train, settings: settings)
        let bridgeUpper = max(settings.tonicBridgeUpperSec, bounds.upper)
        for index in gap {
            guard train.isiSec.indices.contains(index),
                  let value = finiteValidISI(train.isiSec[index], settings: settings) else {
                return false
            }
            if value <= settings.burstSeedUpperSec + tolerance(for: settings.burstSeedUpperSec) {
                return false
            }
            if value > bridgeUpper + tolerance(for: bridgeUpper) {
                return false
            }
        }

        return true
    }

    private static func tonicStructuralSearchBounds(
        train: SpikeTrain,
        settings: StatePatternDetectorSettings
    ) -> (lower: Double, upper: Double) {
        let nonBurstValues = train.isiSec.compactMap { value -> Double? in
            guard let value = finiteValidISI(value, settings: settings),
                  value > settings.burstSeedUpperSec + tolerance(for: settings.burstSeedUpperSec) else {
                return nil
            }
            return value
        }
        let sample = SortedFiniteSample(nonBurstValues, positiveOnly: true)
        guard !sample.isEmpty,
              let q10 = sample.quantile(0.10),
              let q20 = sample.quantile(0.20),
              let q50 = sample.quantile(0.50),
              let q80 = sample.quantile(0.80),
              let q90 = sample.quantile(0.90),
              let q95 = sample.quantile(0.95) else {
            let floor = max(
                settings.minValidISISec,
                settings.burstSeedUpperSec * 1.25
            )
            return (floor, floor)
        }

        let moderateTailUpper = min(q90, q50 * 1.75)
        let adaptiveUpper = min(
            q95,
            max(
                q80,
                q50 * 1.35,
                moderateTailUpper
            )
        )
        let lower = max(
            settings.minValidISISec,
            settings.burstSeedUpperSec * 1.25,
            min(q10, q20, adaptiveUpper)
        )
        let upper = max(lower, adaptiveUpper)
        return (lower, upper)
    }

    private static func isTonicStructuralSupport(
        index: Int,
        value: Double,
        bounds: (lower: Double, upper: Double),
        localContext: SpikeISILocalContextTable
    ) -> Bool {
        guard value >= bounds.lower - tolerance(for: bounds.lower),
              value <= bounds.upper + tolerance(for: bounds.upper) else {
            return false
        }
        guard let context = localContext.point(for: index) else {
            return true
        }
        if let ratio = context.localRatio,
           ratio.isFinite,
           (ratio < 0.55 || ratio > 1.85) {
            return false
        }
        if let robustZ = context.localRobustZ,
           robustZ.isFinite,
           abs(robustZ) > 3.0 {
            return false
        }
        return true
    }

    private static func tonicBridgeCount(run: (start: Int, end: Int), strictFlags: [Bool]) -> Int {
        guard run.start <= run.end else {
            return 0
        }

        var count = 0
        for index in run.start...run.end where strictFlags.indices.contains(index) && !strictFlags[index] {
            count += 1
        }
        return count
    }

    private static func detectHighFrequencyTonic(
        train: SpikeTrain,
        settings: StatePatternDetectorSettings,
        localContext: SpikeISILocalContextTable
    ) -> [ClassicAnchorCandidate] {
        let highMax = settings.highFrequencyTonicUpperSec
        let flags = train.isiSec.indices.map { index -> Bool in
            guard index > 0,
                  let value = finiteValidISI(train.isiSec[index], settings: settings) else {
                return false
            }
            return value <= highMax + tolerance(for: highMax)
        }

        var candidates: [ClassicAnchorCandidate] = []
        for run in boolRuns(flags) {
            guard let metrics = metrics(train: train, run: run, settings: settings, localContext: localContext),
                  metrics.nSpikes >= settings.highFrequencyTonicMinSpikes else {
                continue
            }

            let lowTail = fraction(metrics.values) { $0 < settings.highFrequencyTonicFloorSec - tolerance(for: settings.highFrequencyTonicFloorSec) }
            let burstSeedFraction = fraction(metrics.values) {
                $0 <= settings.burstSeedUpperSec + tolerance(for: settings.burstSeedUpperSec)
            }
            let coreRunLength = maxBurstSeedRun(train: train, run: run, settings: settings)
            let effectiveLowTailMax = effectiveHighFrequencyTonicLowTailFractionMax(settings: settings)
            let lowTailPass = (metrics.q10.map { $0 >= settings.highFrequencyTonicFloorSec - tolerance(for: settings.highFrequencyTonicFloorSec) } ?? false) ||
                lowTail <= effectiveLowTailMax + 1e-12
            let burstSeedFractionMax = effectiveHighFrequencyTonicBurstSeedFractionMax(settings: settings)
            let burstSeedFractionPass = burstSeedFraction <= burstSeedFractionMax + 1e-12
            let q90Pass = metrics.q90.map { $0 <= highMax + tolerance(for: highMax) } ?? false
            let burstCoreVetoPass = coreRunLength < settings.highFrequencyTonicBurstCoreVetoMinISI
            let cvPass = metrics.cv.map { $0 <= settings.highFrequencyTonicCVMax + 1e-12 } ?? true
            let cv2Pass = metrics.cv2.map { $0 <= settings.highFrequencyTonicCV2Max + 1e-12 } ?? true
            let lvPass = metrics.lv.map { $0 <= settings.highFrequencyTonicLVMax + 1e-12 } ?? true
            let mmPass = metrics.mm.map { $0 <= settings.highFrequencyTonicMMMax + 1e-12 } ?? true
            let classicBoundaryPass = !hasClassicBoundary(metrics: metrics, settings: settings)
            let regularityScore = mean([
                metrics.cv.map { 1 / (1 + $0) },
                metrics.cv2.map { 1 / (1 + $0) },
                metrics.lv.map { 1 / (1 + $0) }
            ].compactMap { $0 }) ?? 0
            if !q90Pass || !lowTailPass || !burstSeedFractionPass || !burstCoreVetoPass || !cvPass || !cv2Pass || !lvPass || !mmPass || !classicBoundaryPass {
                var rejectReasons: [String] = []
                if !q90Pass {
                    rejectReasons.append("reject_hf_tonic_q90_above_band")
                }
                if !lowTailPass {
                    rejectReasons.append("reject_hf_tonic_low_tail_too_large")
                }
                if !burstSeedFractionPass {
                    rejectReasons.append("reject_hf_tonic_burst_seed_fraction_too_high")
                }
                if !burstCoreVetoPass {
                    rejectReasons.append("reject_hf_tonic_contains_burst_core_run")
                }
                if !cvPass {
                    rejectReasons.append("reject_hf_tonic_cv_unstable")
                }
                if !cv2Pass {
                    rejectReasons.append("reject_hf_tonic_cv2_unstable")
                }
                if !lvPass {
                    rejectReasons.append("reject_hf_tonic_lv_unstable")
                }
                if !mmPass {
                    rejectReasons.append("reject_hf_tonic_mm_outlier")
                }
                if !classicBoundaryPass {
                    rejectReasons.append("reject_hf_tonic_has_classic_burst_boundary")
                }
                let rejectDecisionPath = stateDecisionPath(
                    base: "reject_high_frequency_tonic_state_candidate",
                    metrics: metrics,
                    extra: rejectReasons + [
                        "low_tail_fraction=\(format(lowTail))",
                        "low_tail_fraction_max_effective=\(format(effectiveLowTailMax))",
                        "burst_seed_fraction=\(format(burstSeedFraction))",
                        "burst_seed_fraction_max_effective=\(format(burstSeedFractionMax))",
                        "structural_burst_support_weight=\(format(settings.structuralBurstSupportWeight))",
                        "core_burst_run_length=\(coreRunLength)",
                        "state_burst_packet_guard=hf_tonic_rejects_when_short_tail_forms_burst_seed_occupancy",
                        "regularity_score=\(format(regularityScore))"
                    ]
                )
                candidates.append(
                    candidate(
                        train: train,
                        run: run,
                        metrics: metrics,
                        label: .reject,
                        layer: "event_core_hf_tonic_state_diagnostic",
                        candidateClass: "rejected_event_core_hf_tonic",
                        gateStatus: "event_core_hf_tonic_reject",
                        decisionPath: rejectDecisionPath,
                        action: "reject",
                        score: 0,
                        priority: 0,
                        bandLower: settings.highFrequencyTonicFloorSec,
                        bandUpper: highMax,
                        contrastMinRequired: settings.highFrequencyTonicLVMax,
                        contrastGeomRequired: settings.highFrequencyTonicMMMax,
                        stateRegularityScore: regularityScore,
                        stateBurstSeedFraction: burstSeedFraction,
                        stateLowTailFraction: lowTail,
                        stateCoreBurstRunLength: coreRunLength,
                        index: candidates.count + 1
                    )
                )
                continue
            }

            let score = 5 + (1 - min(lowTail, 1)) + regularityScore + localStabilityScore(metrics)
            let decisionPath = stateDecisionPath(
                base: "stable_high_frequency_tonic_state_above_burst_core_floor",
                metrics: metrics,
                extra: [
                    "low_tail_fraction=\(format(lowTail))",
                    "low_tail_fraction_max_effective=\(format(effectiveLowTailMax))",
                    "burst_seed_fraction=\(format(burstSeedFraction))",
                    "burst_seed_fraction_max_effective=\(format(burstSeedFractionMax))",
                    "structural_burst_support_weight=\(format(settings.structuralBurstSupportWeight))",
                    "core_burst_run_length=\(coreRunLength)",
                    "state_burst_packet_guard=pass",
                    "regularity_score=\(format(regularityScore))"
                ]
            )
            candidates.append(
                candidate(
                    train: train,
                    run: run,
                    metrics: metrics,
                    label: .highFrequencyTonic,
                    layer: "event_core_hf_tonic_state",
                    candidateClass: "event_core_hf_tonic",
                    gateStatus: "event_core_hf_tonic_pass",
                    decisionPath: decisionPath,
                    score: score,
                    priority: 1_120,
                    bandLower: settings.highFrequencyTonicFloorSec,
                    bandUpper: highMax,
                    contrastMinRequired: settings.highFrequencyTonicLVMax,
                    contrastGeomRequired: settings.highFrequencyTonicMMMax,
                    stateRegularityScore: regularityScore,
                    stateBurstSeedFraction: burstSeedFraction,
                    stateLowTailFraction: lowTail,
                    stateCoreBurstRunLength: coreRunLength,
                    index: candidates.count + 1
                )
            )
        }

        return candidates
    }

    private static func detectTonic(
        train: SpikeTrain,
        settings: StatePatternDetectorSettings,
        localContext: SpikeISILocalContextTable
    ) -> [ClassicAnchorCandidate] {
        let bounds = tonicStructuralSearchBounds(train: train, settings: settings)
        let lower = bounds.lower
        let upper = bounds.upper
        let strictFlags = train.isiSec.indices.map { index -> Bool in
            guard index > 0,
                  let value = finiteValidISI(train.isiSec[index], settings: settings) else {
                return false
            }
            return isTonicStructuralSupport(
                index: index,
                value: value,
                bounds: bounds,
                localContext: localContext
            )
        }

        var candidates: [ClassicAnchorCandidate] = []
        var nextCandidateIndex = 1
        let tonicRuns = mergeTonicSupportRuns(
            boolRuns(strictFlags),
            train: train,
            settings: settings
        )
        for run in tonicRuns {
            guard let metrics = metrics(train: train, run: run, settings: settings, localContext: localContext),
                  metrics.nSpikes >= settings.tonicMinSpikes else {
                continue
            }

            let bridgeCount = tonicBridgeCount(run: run, strictFlags: strictFlags)
            let bridgeFraction = Double(bridgeCount) / Double(max(1, metrics.nValidISI))
            let bridgeFractionPass = bridgeFraction <= settings.tonicBridgeFractionMax + 1e-12
            let burstSeedFraction = fraction(metrics.values) { $0 <= settings.burstSeedUpperSec + tolerance(for: settings.burstSeedUpperSec) }
            let effectiveBurstSeedFractionMax = effectiveTonicBurstSeedFractionMax(settings: settings)
            let coreRunLength = maxBurstSeedRun(train: train, run: run, settings: settings)
            let coreRunLimit = tonicBurstCoreRunLimit(settings: settings)
            let cvPass = metrics.cv.map { $0 <= settings.tonicCVMax + 1e-12 } ?? true
            let cv2Pass = metrics.cv2.map { $0 <= settings.tonicCV2Max + 1e-12 } ?? true
            let lvPass = metrics.lv.map { $0 <= settings.tonicLVMax + 1e-12 } ?? true
            let mmPass = metrics.mm.map { $0 >= settings.tonicMMMin - 1e-12 && $0 <= settings.tonicMMMax + 1e-12 } ?? true
            let burstSeedFractionPass = burstSeedFraction <= effectiveBurstSeedFractionMax + 1e-12
            let coreRunPass = coreRunLength <= coreRunLimit
            let regularityScore = mean([
                metrics.cv.map { 1 / (1 + $0) },
                metrics.cv2.map { 1 / (1 + $0) },
                metrics.lv.map { 1 / (1 + $0) }
            ].compactMap { $0 }) ?? 0
            if !cvPass || !cv2Pass || !lvPass || !mmPass || !burstSeedFractionPass || !coreRunPass || !bridgeFractionPass {
                var rejectReasons: [String] = []
                if !cvPass {
                    rejectReasons.append("reject_tonic_cv_unstable")
                }
                if !cv2Pass {
                    rejectReasons.append("reject_tonic_cv2_unstable")
                }
                if !lvPass {
                    rejectReasons.append("reject_tonic_lv_unstable")
                }
                if !mmPass {
                    rejectReasons.append("reject_tonic_mm_outside_band")
                }
                if !burstSeedFractionPass {
                    rejectReasons.append("reject_tonic_burst_seed_fraction_too_high")
                }
                if !coreRunPass {
                    rejectReasons.append("reject_tonic_contains_burst_seed_core_run")
                }
                if !bridgeFractionPass {
                    rejectReasons.append("reject_tonic_bridge_fraction_too_high")
                }
                let rejectDecisionPath = stateDecisionPath(
                    base: "reject_classic_tonic_state_candidate",
                    metrics: metrics,
                    extra: rejectReasons + [
                        "bridge_count=\(bridgeCount)",
                        "bridge_fraction=\(format(bridgeFraction))",
                        "bridge_upper_sec=\(format(settings.tonicBridgeUpperSec))",
                        "structural_search_lower_sec=\(format(lower))",
                        "structural_search_upper_sec=\(format(upper))",
                        "candidate_seed_policy=structure_first_cv_cv2_lv_not_default_isi_band",
                        "bridge_policy=short_nonburst_gap_between_tonic_runs",
                        "burst_seed_fraction=\(format(burstSeedFraction))",
                        "burst_seed_fraction_max_effective=\(format(effectiveBurstSeedFractionMax))",
                        "core_burst_run_length=\(coreRunLength)",
                        "core_burst_run_limit=\(coreRunLimit)",
                        "state_burst_packet_guard=classic_tonic_rejects_consecutive_burst_seed_core",
                        "structural_burst_support_weight=\(format(settings.structuralBurstSupportWeight))",
                        "regularity_score=\(format(regularityScore))"
                    ]
                )
                candidates.append(
                    candidate(
                        train: train,
                        run: run,
                        metrics: metrics,
                        label: .reject,
                        layer: "event_core_tonic_state_diagnostic",
                        candidateClass: "rejected_event_core_tonic",
                        gateStatus: "event_core_tonic_reject",
                        decisionPath: rejectDecisionPath,
                        action: "reject",
                        score: 0,
                        priority: 0,
                        bandLower: lower,
                        bandUpper: upper,
                        contrastMinRequired: settings.tonicLVMax,
                        contrastGeomRequired: settings.tonicMMMax,
                        stateRegularityScore: regularityScore,
                        stateBurstSeedFraction: burstSeedFraction,
                        stateCoreBurstRunLength: coreRunLength,
                        index: nextCandidateIndex
                    )
                )
                nextCandidateIndex += 1
                continue
            }

            let score = 3 + regularityScore + localStabilityScore(metrics)
            let decisionPath = stateDecisionPath(
                base: bridgeCount > 0 ? "stable_mid_isi_tonic_state_with_short_bridge" : "stable_mid_isi_tonic_state",
                metrics: metrics,
                extra: [
                    "bridge_count=\(bridgeCount)",
                    "bridge_fraction=\(format(bridgeFraction))",
                    "bridge_upper_sec=\(format(settings.tonicBridgeUpperSec))",
                    "structural_search_lower_sec=\(format(lower))",
                    "structural_search_upper_sec=\(format(upper))",
                    "candidate_seed_policy=structure_first_cv_cv2_lv_not_default_isi_band",
                    "bridge_policy=short_nonburst_gap_between_tonic_runs",
                    "burst_seed_fraction=\(format(burstSeedFraction))",
                    "burst_seed_fraction_max_effective=\(format(effectiveBurstSeedFractionMax))",
                    "core_burst_run_length=\(coreRunLength)",
                    "core_burst_run_limit=\(coreRunLimit)",
                    "state_burst_packet_guard=pass",
                    "structural_burst_support_weight=\(format(settings.structuralBurstSupportWeight))",
                    "regularity_score=\(format(regularityScore))"
                ]
            )
            candidates.append(
                candidate(
                    train: train,
                    run: run,
                    metrics: metrics,
                    label: .tonic,
                    layer: "event_core_tonic_state",
                    candidateClass: bridgeCount > 0 ? "event_core_tonic_bridge_state" : "event_core_tonic",
                    gateStatus: bridgeCount > 0 ? "event_core_tonic_bridge_pass" : "event_core_tonic_pass",
                    decisionPath: decisionPath,
                    score: score - 0.5 * bridgeFraction,
                    priority: 1_100,
                    bandLower: lower,
                    bandUpper: upper,
                    contrastMinRequired: settings.tonicLVMax,
                    contrastGeomRequired: settings.tonicMMMax,
                    stateRegularityScore: regularityScore,
                    stateBurstSeedFraction: burstSeedFraction,
                    stateCoreBurstRunLength: coreRunLength,
                    index: nextCandidateIndex
                )
            )
            nextCandidateIndex += 1
        }

        let acceptedRanges = candidates
            .filter { $0.finalLabel == .tonic && $0.action == "accept" }
            .map { (start: $0.startISIIndex, end: $0.endISIIndex) }
        candidates.append(
            contentsOf: detectTonicStructuralWindows(
                train: train,
                settings: settings,
                localContext: localContext,
                bounds: bounds,
                occupiedAcceptedRanges: acceptedRanges,
                nextCandidateIndex: &nextCandidateIndex
            )
        )

        return candidates
    }

    /// Secondary tonic seeds are generated from stable windows inside the
    /// train-local non-burst ISI distribution. This keeps tonic structure-driven:
    /// CV/CV2/LV/MM remain the acceptance gates, and default/histogram bands do
    /// not define the tonic search envelope.
    private static func detectTonicStructuralWindows(
        train: SpikeTrain,
        settings: StatePatternDetectorSettings,
        localContext: SpikeISILocalContextTable,
        bounds: (lower: Double, upper: Double),
        occupiedAcceptedRanges: [(start: Int, end: Int)],
        nextCandidateIndex: inout Int
    ) -> [ClassicAnchorCandidate] {
        let windowUpper = tonicStructuralWindowUpper(bounds: bounds, settings: settings)
        let broadFlags = train.isiSec.indices.map { index -> Bool in
            guard index > 0,
                  let value = finiteValidISI(train.isiSec[index], settings: settings) else {
                return false
            }
            return value >= bounds.lower - tolerance(for: bounds.lower) &&
                value <= windowUpper + tolerance(for: windowUpper)
        }
        let structuralWindowMinSpikes = max(settings.tonicMinSpikes, 6)
        let minISI = max(1, structuralWindowMinSpikes - 1)
        let maxWindowISI = max(minISI, min(32, settings.tonicMinSpikes * 4))
        var candidates: [ClassicAnchorCandidate] = []
        var emittedRanges: [(start: Int, end: Int)] = []

        for broadRun in boolRuns(broadFlags) {
            let runLength = broadRun.end - broadRun.start + 1
            guard runLength >= minISI else {
                continue
            }

            let longestWindow = min(maxWindowISI, runLength)
            for length in stride(from: longestWindow, through: minISI, by: -1) {
                var start = broadRun.start
                while start + length - 1 <= broadRun.end {
                    let run = (start: start, end: start + length - 1)
                    defer { start += 1 }

                    if occupiedAcceptedRanges.contains(where: { $0.start <= run.start && $0.end >= run.end }) ||
                        emittedRanges.contains(where: { rangesOverlap($0, run) }) {
                        continue
                    }
                    guard let metrics = metrics(
                        train: train,
                        run: run,
                        settings: settings,
                        localContext: localContext
                    ),
                          metrics.nSpikes >= structuralWindowMinSpikes else {
                        continue
                    }

                    let burstSeedFraction = fraction(metrics.values) {
                        $0 <= settings.burstSeedUpperSec + tolerance(for: settings.burstSeedUpperSec)
                    }
                    let effectiveBurstSeedFractionMax = effectiveTonicBurstSeedFractionMax(settings: settings)
                    let coreRunLength = maxBurstSeedRun(train: train, run: run, settings: settings)
                    let coreRunLimit = tonicBurstCoreRunLimit(settings: settings)
                    let cvPass = metrics.cv.map { $0 <= settings.tonicCVMax + 1e-12 } ?? true
                    let cv2Pass = metrics.cv2.map { $0 <= settings.tonicCV2Max + 1e-12 } ?? true
                    let lvPass = metrics.lv.map { $0 <= settings.tonicLVMax + 1e-12 } ?? true
                    let mmPass = metrics.mm.map {
                        $0 >= settings.tonicMMMin - 1e-12 &&
                            $0 <= settings.tonicMMMax + 1e-12
                    } ?? true
                    let burstSeedFractionPass = burstSeedFraction <= effectiveBurstSeedFractionMax + 1e-12
                    let coreRunPass = coreRunLength <= coreRunLimit
                    guard cvPass, cv2Pass, lvPass, mmPass, burstSeedFractionPass, coreRunPass else {
                        continue
                    }

                    let regularityScore = mean([
                        metrics.cv.map { 1 / (1 + $0) },
                        metrics.cv2.map { 1 / (1 + $0) },
                        metrics.lv.map { 1 / (1 + $0) }
                    ].compactMap { $0 }) ?? 0
                    let score = 3.2 +
                        regularityScore +
                        localStabilityScore(metrics) +
                        0.015 * Double(metrics.nISI)
                    let decisionPath = stateDecisionPath(
                        base: "stable_mid_isi_tonic_state_structural_window",
                        metrics: metrics,
                        extra: [
                            "structural_search_lower_sec=\(format(bounds.lower))",
                            "structural_search_upper_sec=\(format(bounds.upper))",
                            "structural_window_upper_sec=\(format(windowUpper))",
                            "candidate_seed_policy=structure_first_window_cv_cv2_lv_not_default_isi_band",
                            "window_seed_source=broad_nonburst_isi_range",
                            "burst_seed_fraction=\(format(burstSeedFraction))",
                            "burst_seed_fraction_max_effective=\(format(effectiveBurstSeedFractionMax))",
                            "core_burst_run_length=\(coreRunLength)",
                            "core_burst_run_limit=\(coreRunLimit)",
                            "state_burst_packet_guard=pass",
                            "regularity_score=\(format(regularityScore))"
                        ]
                    )
                    candidates.append(
                        candidate(
                            train: train,
                            run: run,
                            metrics: metrics,
                            label: .tonic,
                            layer: "event_core_tonic_state_structural_window",
                            candidateClass: "event_core_tonic_structural_window",
                            gateStatus: "event_core_tonic_structural_window_pass",
                            decisionPath: decisionPath,
                            score: score,
                            priority: 1_095,
                            bandLower: bounds.lower,
                            bandUpper: windowUpper,
                            contrastMinRequired: settings.tonicLVMax,
                            contrastGeomRequired: settings.tonicMMMax,
                            stateRegularityScore: regularityScore,
                            stateBurstSeedFraction: burstSeedFraction,
                            stateCoreBurstRunLength: coreRunLength,
                            index: nextCandidateIndex
                        )
                    )
                    emittedRanges.append(run)
                    nextCandidateIndex += 1
                }
            }
        }

        return candidates
    }

    private static func tonicStructuralWindowUpper(
        bounds: (lower: Double, upper: Double),
        settings: StatePatternDetectorSettings
    ) -> Double {
        let bridgeLimited = max(
            bounds.upper,
            min(settings.tonicBridgeUpperSec * 1.03, bounds.upper * 1.15)
        )
        return max(bounds.upper, bridgeLimited)
    }

    private static func rebuildTonicSplitCandidate(
        train: SpikeTrain,
        parent: ClassicAnchorCandidate,
        run: (start: Int, end: Int),
        metrics: StateMetrics,
        settings: StatePatternDetectorSettings,
        id: String,
        commonExtra: [String]
    ) -> ClassicAnchorCandidate? {
        guard metrics.nSpikes >= settings.tonicMinSpikes else {
            return nil
        }

        let bounds = tonicStructuralSearchBounds(train: train, settings: settings)
        let lower = bounds.lower
        let upper = bounds.upper
        let strictFlags = metrics.values.map {
            $0 >= lower - tolerance(for: lower) &&
                $0 <= upper + tolerance(for: upper)
        }
        let bridgeUpper = max(settings.tonicBridgeUpperSec, upper)
        let supportPass = zip(metrics.values, strictFlags).allSatisfy { value, strict in
            strict || (
                value > settings.burstSeedUpperSec + tolerance(for: settings.burstSeedUpperSec) &&
                    value <= bridgeUpper + tolerance(for: bridgeUpper)
            )
        }
        guard supportPass else {
            return nil
        }

        let bridgeCount = strictFlags.filter { !$0 }.count
        let bridgeFraction = Double(bridgeCount) / Double(max(1, metrics.nValidISI))
        let burstSeedFraction = fraction(metrics.values) {
            $0 <= settings.burstSeedUpperSec + tolerance(for: settings.burstSeedUpperSec)
        }
        let effectiveBurstSeedFractionMax = effectiveTonicBurstSeedFractionMax(settings: settings)
        let coreRunLength = maxBurstSeedRun(train: train, run: run, settings: settings)
        let coreRunLimit = tonicBurstCoreRunLimit(settings: settings)
        let gatesPass =
            bridgeFraction <= settings.tonicBridgeFractionMax + 1e-12 &&
            (metrics.cv.map { $0 <= settings.tonicCVMax + 1e-12 } ?? true) &&
            (metrics.cv2.map { $0 <= settings.tonicCV2Max + 1e-12 } ?? true) &&
            (metrics.lv.map { $0 <= settings.tonicLVMax + 1e-12 } ?? true) &&
            (metrics.mm.map {
                $0 >= settings.tonicMMMin - 1e-12 &&
                    $0 <= settings.tonicMMMax + 1e-12
            } ?? true) &&
            burstSeedFraction <= effectiveBurstSeedFractionMax + 1e-12 &&
            coreRunLength <= coreRunLimit
        guard gatesPass else {
            return nil
        }

        let regularityScore = mean([
            metrics.cv.map { 1 / (1 + $0) },
            metrics.cv2.map { 1 / (1 + $0) },
            metrics.lv.map { 1 / (1 + $0) }
        ].compactMap { $0 }) ?? 0
        let score = 3 + regularityScore + localStabilityScore(metrics) - 0.5 * bridgeFraction
        let decisionPath = stateDecisionPath(
            base: "state_track_gap_event_split_tonic_pass",
            metrics: metrics,
            extra: commonExtra + [
                "bridge_count=\(bridgeCount)",
                "bridge_fraction=\(format(bridgeFraction))",
                "structural_search_lower_sec=\(format(lower))",
                "structural_search_upper_sec=\(format(upper))",
                "burst_seed_fraction=\(format(burstSeedFraction))",
                "core_burst_run_length=\(coreRunLength)",
                "split_fragment_revalidated_with_primary_tonic_gates=true"
            ]
        )
        return candidate(
            train: train,
            run: run,
            metrics: metrics,
            label: .tonic,
            layer: "\(parent.candidateLayer)_state_track_split",
            candidateClass: "\(parent.candidateClass)_state_track_fragment",
            gateStatus: "state_track_split_tonic_pass",
            decisionPath: decisionPath,
            score: score,
            priority: parent.priority,
            bandLower: lower,
            bandUpper: upper,
            contrastMinRequired: settings.tonicLVMax,
            contrastGeomRequired: settings.tonicMMMax,
            stateRegularityScore: regularityScore,
            stateBurstSeedFraction: burstSeedFraction,
            stateCoreBurstRunLength: coreRunLength,
            index: 0,
            id: id
        )
    }

    private static func rebuildHighFrequencyTonicSplitCandidate(
        train: SpikeTrain,
        parent: ClassicAnchorCandidate,
        run: (start: Int, end: Int),
        metrics: StateMetrics,
        settings: StatePatternDetectorSettings,
        id: String,
        commonExtra: [String]
    ) -> ClassicAnchorCandidate? {
        guard metrics.nSpikes >= settings.highFrequencyTonicMinSpikes else {
            return nil
        }

        let highMax = settings.highFrequencyTonicUpperSec
        guard metrics.values.allSatisfy({
            $0 <= highMax + tolerance(for: highMax)
        }) else {
            return nil
        }

        let lowTail = fraction(metrics.values) {
            $0 < settings.highFrequencyTonicFloorSec - tolerance(for: settings.highFrequencyTonicFloorSec)
        }
        let burstSeedFraction = fraction(metrics.values) {
            $0 <= settings.burstSeedUpperSec + tolerance(for: settings.burstSeedUpperSec)
        }
        let coreRunLength = maxBurstSeedRun(train: train, run: run, settings: settings)
        let effectiveLowTailMax = effectiveHighFrequencyTonicLowTailFractionMax(settings: settings)
        let burstSeedFractionMax = effectiveHighFrequencyTonicBurstSeedFractionMax(settings: settings)
        let gatesPass =
            ((metrics.q10.map {
                $0 >= settings.highFrequencyTonicFloorSec - tolerance(for: settings.highFrequencyTonicFloorSec)
            } ?? false) || lowTail <= effectiveLowTailMax + 1e-12) &&
            burstSeedFraction <= burstSeedFractionMax + 1e-12 &&
            (metrics.q90.map { $0 <= highMax + tolerance(for: highMax) } ?? false) &&
            coreRunLength < settings.highFrequencyTonicBurstCoreVetoMinISI &&
            (metrics.cv.map { $0 <= settings.highFrequencyTonicCVMax + 1e-12 } ?? true) &&
            (metrics.cv2.map { $0 <= settings.highFrequencyTonicCV2Max + 1e-12 } ?? true) &&
            (metrics.lv.map { $0 <= settings.highFrequencyTonicLVMax + 1e-12 } ?? true) &&
            (metrics.mm.map { $0 <= settings.highFrequencyTonicMMMax + 1e-12 } ?? true) &&
            !hasClassicBoundary(metrics: metrics, settings: settings)
        guard gatesPass else {
            return nil
        }

        let regularityScore = mean([
            metrics.cv.map { 1 / (1 + $0) },
            metrics.cv2.map { 1 / (1 + $0) },
            metrics.lv.map { 1 / (1 + $0) }
        ].compactMap { $0 }) ?? 0
        let score = 5 + (1 - min(lowTail, 1)) + regularityScore + localStabilityScore(metrics)
        let decisionPath = stateDecisionPath(
            base: "state_track_gap_event_split_hf_tonic_pass",
            metrics: metrics,
            extra: commonExtra + [
                "low_tail_fraction=\(format(lowTail))",
                "burst_seed_fraction=\(format(burstSeedFraction))",
                "core_burst_run_length=\(coreRunLength)",
                "split_fragment_revalidated_with_primary_hf_tonic_gates=true"
            ]
        )
        return candidate(
            train: train,
            run: run,
            metrics: metrics,
            label: .highFrequencyTonic,
            layer: "\(parent.candidateLayer)_state_track_split",
            candidateClass: "\(parent.candidateClass)_state_track_fragment",
            gateStatus: "state_track_split_hf_tonic_pass",
            decisionPath: decisionPath,
            score: score,
            priority: parent.priority,
            bandLower: settings.highFrequencyTonicFloorSec,
            bandUpper: highMax,
            contrastMinRequired: settings.highFrequencyTonicLVMax,
            contrastGeomRequired: settings.highFrequencyTonicMMMax,
            stateRegularityScore: regularityScore,
            stateBurstSeedFraction: burstSeedFraction,
            stateLowTailFraction: lowTail,
            stateCoreBurstRunLength: coreRunLength,
            index: 0,
            id: id
        )
    }

    private static func rebuildHighFrequencySpikingSplitCandidate(
        train: SpikeTrain,
        parent: ClassicAnchorCandidate,
        run: (start: Int, end: Int),
        metrics: StateMetrics,
        settings: StatePatternDetectorSettings,
        id: String,
        commonExtra: [String]
    ) -> ClassicAnchorCandidate? {
        guard metrics.nSpikes >= settings.highFrequencySpikingMinSpikes,
              metrics.durationSec >= settings.highFrequencySpikingMinDurationSec -
                tolerance(for: settings.highFrequencySpikingMinDurationSec) else {
            return nil
        }

        let shortFraction = fraction(metrics.values) {
            $0 <= settings.highFrequencySpikingShortUpperSec +
                tolerance(for: settings.highFrequencySpikingShortUpperSec)
        }
        let q90ShortFraction = fraction(metrics.values) {
            $0 <= settings.highFrequencySpikingQ90MaxSec +
                tolerance(for: settings.highFrequencySpikingQ90MaxSec)
        }
        let bridgeFraction = fraction(metrics.values) {
            $0 <= settings.highFrequencySpikingEpochBridgeSec +
                tolerance(for: settings.highFrequencySpikingEpochBridgeSec)
        }
        let largeFlags = metrics.values.map {
            $0 > settings.highFrequencySpikingEpochBridgeSec +
                tolerance(for: settings.highFrequencySpikingEpochBridgeSec)
        }
        let largeFraction = mean(largeFlags.map { $0 ? 1 : 0 }) ?? 0
        let maxConsecutiveLarge = maxConsecutiveTrue(largeFlags)
        let toleratedGap = highFrequencySpikingToleratedGapSec(settings: settings)
        let toleratedFraction = fraction(metrics.values) {
            $0 <= toleratedGap + tolerance(for: toleratedGap)
        }
        let patternMaxExceeded = settings.highFrequencySpikingPatternMaxISISec.map { patternMax in
            metrics.values.contains { $0 > patternMax + tolerance(for: patternMax) }
        } ?? false
        let strictQ90Pass = (metrics.q90 ?? .infinity) <=
            settings.highFrequencySpikingQ90MaxSec + tolerance(for: settings.highFrequencySpikingQ90MaxSec)
        let robustQ80Pass = (metrics.q80 ?? .infinity) <=
            settings.highFrequencySpikingQ80MaxSec + tolerance(for: settings.highFrequencySpikingQ80MaxSec)
        let majorityPass =
            shortFraction >= max(0.50, settings.highFrequencySpikingShortFractionMin - 0.15) ||
            q90ShortFraction >= max(0.60, settings.highFrequencySpikingShortFractionMin - 0.10) ||
            bridgeFraction >= max(0.75, settings.highFrequencySpikingShortFractionMin)
        let stateCompactPass = strictQ90Pass || (robustQ80Pass && majorityPass)
        let gapTolerancePass = toleratedFraction >= 0.95 - 1e-12
        let allowedLargeEff = max(settings.highFrequencySpikingAllowedLargeFraction, 0.30)
        let maxConsecutiveLargeEff = max(2, settings.highFrequencySpikingMaxConsecutiveLargeISI)
        let largePass = largeFraction <= allowedLargeEff + 1e-12 &&
            maxConsecutiveLarge <= maxConsecutiveLargeEff
        guard !patternMaxExceeded,
              stateCompactPass,
              gapTolerancePass,
              largePass else {
            return nil
        }

        let packetization = highFrequencySpikingPacketizationStats(
            train: train,
            run: run,
            metrics: metrics,
            settings: settings
        )
        let score = 24 +
            0.08 * Double(metrics.nSpikes) +
            2.5 * shortFraction +
            2.0 * q90ShortFraction +
            1.5 * bridgeFraction -
            2.0 * largeFraction +
            (strictQ90Pass ? 1.0 : 0.4) +
            highFrequencyLocalShortScore(metrics)
        let decisionPath = stateDecisionPath(
            base: "state_track_gap_split_hf_spiking_pass",
            metrics: metrics,
            extra: commonExtra + [
                "short_fraction=\(format(shortFraction))",
                "q90_short_fraction=\(format(q90ShortFraction))",
                "bridge_fraction=\(format(bridgeFraction))",
                "large_fraction=\(format(largeFraction))",
                "tolerated_fraction=\(format(toleratedFraction))",
                "provisional_self_packet_group_count=\(packetization.groupCount)",
                "provisional_self_packet_coverage=\(format(packetization.coverage))",
                "split_fragment_revalidated_with_primary_hfs_gates=true",
                "packet_dominance_deferred_to_selected_event_track=true"
            ]
        )
        var rebuilt = candidate(
            train: train,
            run: run,
            metrics: metrics,
            label: .highFrequencySpiking,
            layer: "\(parent.candidateLayer)_state_track_split",
            candidateClass: "\(parent.candidateClass)_state_track_fragment",
            gateStatus: "state_track_split_hf_spiking_pass",
            decisionPath: decisionPath,
            score: score,
            priority: parent.priority,
            bandLower: settings.minValidISISec,
            bandUpper: settings.highFrequencySpikingQ90MaxSec,
            contrastMinRequired: settings.highFrequencySpikingShortFractionMin,
            contrastGeomRequired: allowedLargeEff,
            index: 0,
            id: id
        )
        rebuilt.hfSpikingQ80Sec = metrics.q80
        rebuilt.hfSpikingQ80MaxSec = settings.highFrequencySpikingQ80MaxSec
        rebuilt.hfSpikingQ90MaxSec = settings.highFrequencySpikingQ90MaxSec
        rebuilt.hfSpikingShortUpperSec = settings.highFrequencySpikingShortUpperSec
        rebuilt.hfSpikingEpochBridgeSec = settings.highFrequencySpikingEpochBridgeSec
        rebuilt.hfSpikingToleratedGapSec = toleratedGap
        rebuilt.hfSpikingPatternMaxISISec = settings.highFrequencySpikingPatternMaxISISec
        rebuilt.hfSpikingPauseBreakSec = settings.highFrequencySpikingPauseBreakSec
        rebuilt.hfSpikingShortFraction = shortFraction
        rebuilt.hfSpikingQ90ShortFraction = q90ShortFraction
        rebuilt.hfSpikingBridgeFraction = bridgeFraction
        rebuilt.hfSpikingLargeFraction = largeFraction
        rebuilt.hfSpikingToleratedFraction = toleratedFraction
        rebuilt.hfSpikingMaxConsecutiveLargeISI = maxConsecutiveLarge
        rebuilt.hfSpikingMinSpikesRequired = settings.highFrequencySpikingMinSpikes
        rebuilt.hfSpikingAcceptanceRoute = "state_track_split_fragment+provisional_self_packetization"
        rebuilt.hfSpikingEmbeddedBurstCount = packetization.groupCount
        rebuilt.hfSpikingEmbeddedBurstGroupCount = packetization.groupCount
        rebuilt.hfSpikingEmbeddedBurstCoverage = packetization.coverage
        rebuilt.hfSpikingBurstDominated = false
        rebuilt.hfSpikingBurstPacketLike = packetization.packetLike
        return rebuilt
    }

    private static func stateSplitRootID(_ id: String) -> String {
        id.components(separatedBy: "::state-split::").first ?? id
    }

    private static func candidate(
        train: SpikeTrain,
        run: (start: Int, end: Int),
        metrics: StateMetrics,
        label: ClassicAnchorLabel,
        layer: String,
        candidateClass: String,
        gateStatus: String,
        decisionPath: String,
        action: String = "accept",
        score: Double,
        priority: Int,
        bandLower: Double,
        bandUpper: Double,
        contrastMinRequired: Double,
        contrastGeomRequired: Double,
        stateRegularityScore: Double? = nil,
        stateBurstSeedFraction: Double? = nil,
        stateLowTailFraction: Double? = nil,
        stateCoreBurstRunLength: Int? = nil,
        index: Int,
        id explicitID: String? = nil
    ) -> ClassicAnchorCandidate {
        var candidate = ClassicAnchorCandidate(
            id: explicitID ?? "\(train.id)-\(label.rawValue)-state-\(index)",
            trainID: train.id,
            trainName: train.name,
            candidateLayer: layer,
            candidateClass: candidateClass,
            finalLabel: label,
            gateStatus: gateStatus,
            decisionPath: decisionPath,
            action: action,
            score: score,
            priority: priority,
            selectedForAuto: false,
            selectionStatus: "not_selected",
            startISIIndex: run.start,
            endISIIndex: run.end,
            startSpikeIndex: run.start,
            endSpikeIndex: run.end + 1,
            nISI: metrics.nISI,
            nValidISI: metrics.nValidISI,
            nSpikes: metrics.nSpikes,
            durationSec: metrics.durationSec,
            intraQ10Sec: metrics.q10,
            intraQ40Sec: metrics.q40,
            intraQ50Sec: metrics.q50,
            intraQ90Sec: metrics.q90,
            intraQ95Sec: metrics.q95,
            maxIntraISISec: metrics.max,
            meanIntraISISec: metrics.mean,
            cv: metrics.cv,
            lv: metrics.lv,
            mm: metrics.mm,
            preGapSec: metrics.preGapSec,
            postGapSec: metrics.postGapSec,
            preRatioQ90: nil,
            postRatioQ90: nil,
            edgeContrastMinQ90: metrics.lv,
            edgeContrastGeomQ90: metrics.mm,
            anchorFamily: "state",
            anchorLockLevel: .strongCandidate,
            anchorBandLowerSec: bandLower,
            anchorBandUpperSec: bandUpper,
            anchorBandSource: .structure,
            anchorContrastMinRequired: contrastMinRequired,
            anchorContrastGeomRequired: contrastGeomRequired,
            refractorySuspectCount: 0,
            refractorySuspectAction: nil
        )
        candidate.cv2 = metrics.cv2
        candidate.stateRegularityScore = stateRegularityScore
        candidate.stateBurstSeedFraction = stateBurstSeedFraction
        candidate.stateLowTailFraction = stateLowTailFraction
        candidate.stateLocalStabilityScore = localStabilityScore(metrics)
        candidate.stateCoreBurstRunLength = stateCoreBurstRunLength
        candidate.stateTrainPercentileMedian = metrics.trainPercentileMedian
        candidate.stateLocalPercentileMedian = metrics.localPercentileMedian
        candidate.stateLocalPercentileQ90 = metrics.localPercentileQ90
        candidate.stateLocalRobustZMedian = metrics.localRobustZMedian
        candidate.stateLocalRobustZAbsQ80 = metrics.localRobustZAbsQ80
        candidate.stateLocalRobustZQ10 = metrics.localRobustZQ10
        return candidate
    }

    private static func metrics(
        train: SpikeTrain,
        run: (start: Int, end: Int),
        settings: StatePatternDetectorSettings,
        localContext: SpikeISILocalContextTable
    ) -> StateMetrics? {
        guard run.start > 0,
              run.end >= run.start,
              run.end < train.isiSec.count,
              train.timestampsSec.indices.contains(run.start - 1),
              train.timestampsSec.indices.contains(run.end) else {
            return nil
        }

        let values = (run.start...run.end).compactMap { index in
            finiteValidISI(train.isiSec[index], settings: settings)
        }
        guard !values.isEmpty else {
            return nil
        }
        let contextPoints = (run.start...run.end).compactMap { localContext.point(for: $0) }
        let trainPercentiles = contextPoints.map(\.trainPercentile)
        let localPercentiles = contextPoints.compactMap(\.localPercentile)
        let localRobustZValues = contextPoints.compactMap(\.localRobustZ)
        let localRobustZAbs = localRobustZValues.map(abs)
        let valueSample = SortedFiniteSample(values)
        let trainPercentileSample = SortedFiniteSample(trainPercentiles)
        let localPercentileSample = SortedFiniteSample(localPercentiles)
        let localRobustZSample = SortedFiniteSample(localRobustZValues)
        let localRobustZAbsSample = SortedFiniteSample(localRobustZAbs)

        let duration = train.timestampsSec[run.end] - train.timestampsSec[run.start - 1]
        return StateMetrics(
            values: values,
            nISI: run.end - run.start + 1,
            nValidISI: values.count,
            nSpikes: run.end - run.start + 2,
            durationSec: duration.isFinite ? duration : 0,
            q10: valueSample.quantile(0.10),
            q40: valueSample.quantile(0.40),
            q50: valueSample.quantile(0.50),
            q80: valueSample.quantile(0.80),
            q90: valueSample.quantile(0.90),
            q95: valueSample.quantile(0.95),
            mean: mean(values),
            max: values.max(),
            cv: coefficientOfVariation(values),
            cv2: coefficientOfVariation2(values),
            lv: localVariation(values),
            mm: maxMeanRatio(values),
            preGapSec: finiteValidISI(run.start > 1 ? train.isiSec[run.start - 1] : nil, settings: settings),
            postGapSec: finiteValidISI(run.end < train.isiSec.count - 1 ? train.isiSec[run.end + 1] : nil, settings: settings),
            trainPercentileMedian: trainPercentileSample.quantile(0.50),
            localPercentileMedian: localPercentileSample.quantile(0.50),
            localPercentileQ90: localPercentileSample.quantile(0.90),
            localRobustZMedian: localRobustZSample.quantile(0.50),
            localRobustZAbsQ80: localRobustZAbsSample.quantile(0.80),
            localRobustZQ10: localRobustZSample.quantile(0.10)
        )
    }

    private static func maxBurstSeedRun(
        train: SpikeTrain,
        run: (start: Int, end: Int),
        settings: StatePatternDetectorSettings
    ) -> Int {
        var best = 0
        var current = 0
        for index in run.start...run.end {
            if let value = finiteValidISI(train.isiSec[index], settings: settings),
               value <= settings.burstSeedUpperSec + tolerance(for: settings.burstSeedUpperSec) {
                current += 1
                best = max(best, current)
            } else {
                current = 0
            }
        }
        return best
    }

    private static func hasClassicBoundary(
        metrics: StateMetrics,
        settings: StatePatternDetectorSettings
    ) -> Bool {
        guard let q90 = metrics.q90,
              q90.isFinite,
              q90 > 0,
              let pre = metrics.preGapSec,
              pre.isFinite,
              let post = metrics.postGapSec,
              post.isFinite else {
            return false
        }

        let preRatio = pre / q90
        let postRatio = post / q90
        return preRatio >= settings.classicBoundaryContrastMin &&
            postRatio >= settings.classicBoundaryContrastMin
    }

    private static func effectiveTonicBurstSeedFractionMax(
        settings: StatePatternDetectorSettings
    ) -> Double {
        let support = min(1, max(0, settings.structuralBurstSupportWeight))
        let tightened = settings.tonicBurstSeedFractionMax * (1 - 0.75 * support)
        return max(0.03, tightened)
    }

    private static func effectiveHighFrequencyTonicLowTailFractionMax(
        settings: StatePatternDetectorSettings
    ) -> Double {
        let support = min(1, max(0, settings.structuralBurstSupportWeight))
        let tightened = settings.highFrequencyTonicLowTailFractionMax * (1 - 0.65 * support)
        return max(0.01, tightened)
    }

    private static func effectiveHighFrequencyTonicBurstSeedFractionMax(
        settings: StatePatternDetectorSettings
    ) -> Double {
        let support = min(1, max(0, settings.structuralBurstSupportWeight))
        let base = max(settings.highFrequencyTonicLowTailFractionMax, 0.04)
        return max(0.01, base * (1 - 0.60 * support))
    }

    private static func tonicBurstCoreRunLimit(settings: StatePatternDetectorSettings) -> Int {
        settings.structuralBurstSupportWeight >= 0.50 ? 1 : 2
    }

    private static func localStabilityScore(_ metrics: StateMetrics) -> Double {
        guard let absQ80 = metrics.localRobustZAbsQ80, absQ80.isFinite else {
            return 0
        }
        return min(1.0, 1 / (1 + max(0, absQ80)))
    }

    private static func highFrequencyLocalShortScore(_ metrics: StateMetrics) -> Double {
        guard let q10 = metrics.localRobustZQ10, q10.isFinite else {
            return 0
        }
        return min(0.75, max(0, -q10) * 0.25)
    }

    private static func stateDecisionPath(
        base: String,
        metrics: StateMetrics,
        extra: [String] = []
    ) -> String {
        (
            [base] + extra + [
                "train_percentile_median=\(format(metrics.trainPercentileMedian))",
                "local_percentile_median=\(format(metrics.localPercentileMedian))",
                "local_percentile_q90=\(format(metrics.localPercentileQ90))",
                "local_robust_z_median=\(format(metrics.localRobustZMedian))",
                "local_robust_z_abs_q80=\(format(metrics.localRobustZAbsQ80))",
                "local_robust_z_q10=\(format(metrics.localRobustZQ10))"
            ]
        ).joined(separator: ";")
    }

    private static func boolRuns(_ flag: [Bool]) -> [(start: Int, end: Int)] {
        var runs: [(start: Int, end: Int)] = []
        var start: Int?

        for index in flag.indices {
            if flag[index] {
                if start == nil {
                    start = index
                }
            } else if let openStart = start {
                runs.append((openStart, index - 1))
                start = nil
            }
        }

        if let openStart = start {
            runs.append((openStart, flag.count - 1))
        }

        return runs
    }

    private static func rangesOverlap(
        _ lhs: (start: Int, end: Int),
        _ rhs: (start: Int, end: Int)
    ) -> Bool {
        lhs.start <= rhs.end && rhs.start <= lhs.end
    }

    private static func finiteValidISI(_ value: Double?, settings: StatePatternDetectorSettings) -> Double? {
        guard let value,
              value.isFinite,
              value >= settings.minValidISISec - tolerance(for: settings.minValidISISec) else {
            return nil
        }
        return value
    }

    private static func quantile(_ values: [Double], probability: Double) -> Double? {
        let sorted = values.filter(\.isFinite).sorted()
        guard !sorted.isEmpty else {
            return nil
        }
        guard sorted.count > 1 else {
            return sorted[0]
        }

        let p = min(max(probability, 0), 1)
        let h = (Double(sorted.count) - 1) * p + 1
        let lowerIndex = max(0, Int(floor(h)) - 1)
        let upperIndex = max(0, Int(ceil(h)) - 1)
        let lowerValue = sorted[min(lowerIndex, sorted.count - 1)]
        let upperValue = sorted[min(upperIndex, sorted.count - 1)]
        return lowerValue + (h - floor(h)) * (upperValue - lowerValue)
    }

    private static func fraction(_ values: [Double], predicate: (Double) -> Bool) -> Double {
        guard !values.isEmpty else {
            return 0
        }
        return Double(values.filter(predicate).count) / Double(values.count)
    }

    private static func mean(_ values: [Double]) -> Double? {
        let finite = values.filter(\.isFinite)
        guard !finite.isEmpty else {
            return nil
        }
        return finite.reduce(0, +) / Double(finite.count)
    }

    private static func coefficientOfVariation(_ values: [Double]) -> Double? {
        let finite = values.filter(\.isFinite)
        guard finite.count >= 2,
              let meanValue = mean(finite),
              meanValue > 0 else {
            return nil
        }
        let variance = finite.reduce(0) { partial, value in
            let delta = value - meanValue
            return partial + delta * delta
        } / Double(finite.count)
        return sqrt(variance) / meanValue
    }

    private static func coefficientOfVariation2(_ values: [Double]) -> Double? {
        let finite = values.filter(\.isFinite)
        guard finite.count >= 2 else {
            return nil
        }
        let terms = zip(finite, finite.dropFirst()).compactMap { previous, next -> Double? in
            let denominator = previous + next
            guard denominator > 0 else {
                return nil
            }
            return 2 * abs(next - previous) / denominator
        }
        return mean(terms)
    }

    private static func localVariation(_ values: [Double]) -> Double? {
        let finite = values.filter(\.isFinite)
        guard finite.count >= 2 else {
            return nil
        }
        let terms = zip(finite, finite.dropFirst()).compactMap { previous, next -> Double? in
            let denominator = previous + next
            guard denominator > 0 else {
                return nil
            }
            let numerator = 3 * pow(next - previous, 2)
            return numerator / pow(denominator, 2)
        }
        return mean(terms)
    }

    private static func maxMeanRatio(_ values: [Double]) -> Double? {
        let finite = values.filter(\.isFinite)
        guard let meanValue = mean(finite),
              meanValue > 0,
              let maxValue = finite.max() else {
            return nil
        }
        return maxValue / meanValue
    }

    private static func maxConsecutiveTrue(_ values: [Bool]) -> Int {
        var best = 0
        var current = 0
        for value in values {
            if value {
                current += 1
                best = max(best, current)
            } else {
                current = 0
            }
        }
        return best
    }

    private static func format(_ value: Double?) -> String {
        guard let value, value.isFinite else {
            return "NA"
        }
        return String(format: "%.12g", locale: Locale(identifier: "en_US_POSIX"), value)
    }

    private static func format(_ value: Double) -> String {
        format(Optional(value))
    }

    private static func tolerance(for value: Double) -> Double {
        max(1e-12, abs(value) * 1e-6)
    }
}

public extension StatePatternDetectorSettings {
    init(
        adaptiveResolution resolution: TrainAdaptiveBandResolution,
        qualitySettings: SpikeQualitySettings = SpikeQualitySettings(),
        tuning: StatePatternDetectorTuning = StatePatternDetectorTuning()
    ) {
        let burst = resolution.band(for: .burst).flatMap { band -> AdaptiveBand? in
            band.primarySource == .structure ? band : nil
        }
        let tonic = resolution.band(for: .tonic).flatMap { band -> AdaptiveBand? in
            band.primarySource == .structure ? band : nil
        }
        let highFrequencyTonic = resolution.band(for: .highFrequencyTonic).flatMap { band -> AdaptiveBand? in
            band.primarySource == .structure ? band : nil
        }
        let highFrequencySpiking = resolution.band(for: .highFrequencySpiking)
        let pause = resolution.band(for: .pause).flatMap { band -> AdaptiveBand? in
            band.primarySource == .structure ? band : nil
        }
        let minValidISI = max(resolution.minValidISISec, qualitySettings.artifactThresholdSec)
        let hfsShortUpper = highFrequencySpiking?.seedUpperSec ?? 0.020
        let hfsEpochBridgeRaw = highFrequencySpiking?.bridgeUpperSec ?? 0.035
        let hfsUIQ90 = 0.025
        let hfsUIBridge = 0.035
        let hfsQ80Max = max(hfsShortUpper, hfsUIQ90)
        let hfsQ90Max = max(hfsUIQ90, hfsShortUpper, 0.75 * hfsEpochBridgeRaw)
        let hfsEpochBridge = max(hfsEpochBridgeRaw, hfsUIBridge, hfsQ90Max)
        let hfsHardBreak = max(hfsEpochBridge, 1.5 * hfsQ90Max)
        let tonicLower = tonic?.seedLowerSec ?? minValidISI
        let tonicUpper = tonic?.seedUpperSec ?? minValidISI
        let structuralBurstUpper = burst?.seedUpperSec ?? minValidISI
        let highFrequencyTonicFloor = highFrequencyTonic?.seedLowerSec ?? structuralBurstUpper
        let highFrequencyTonicUpper = highFrequencyTonic?.seedUpperSec ?? max(highFrequencyTonicFloor, tonic?.seedLowerSec ?? highFrequencyTonicFloor)
        let tonicBridgeRaw = max(tonic?.bridgeUpperSec ?? tonicUpper * 1.20, tonicUpper)
        let tonicBridgeUpper = (pause?.seedLowerSec).map {
            max(tonicUpper, min(tonicBridgeRaw, $0))
        } ?? tonicBridgeRaw

        self.init(
            minValidISISec: minValidISI,
            burstSeedUpperSec: burst?.seedUpperSec ?? minValidISI,
            tonicLowerSec: tonicLower,
            tonicUpperSec: tonicUpper,
            tonicBridgeUpperSec: tonicBridgeUpper,
            tonicMinSpikes: tuning.tonicMinSpikes,
            tonicCVMax: tuning.tonicCVMax,
            tonicCV2Max: tuning.tonicCV2Max,
            tonicLVMax: tuning.tonicLVMax,
            tonicMMMin: tuning.tonicMMMin,
            tonicMMMax: tuning.tonicMMMax,
            tonicBurstSeedFractionMax: tuning.tonicBurstSeedFractionMax,
            highFrequencyTonicFloorSec: highFrequencyTonicFloor,
            highFrequencyTonicUpperSec: highFrequencyTonicUpper,
            highFrequencyTonicMinSpikes: tuning.highFrequencyTonicMinSpikes,
            highFrequencyTonicLowTailFractionMax: tuning.highFrequencyTonicLowTailFractionMax,
            highFrequencyTonicCVMax: tuning.highFrequencyTonicCVMax,
            highFrequencyTonicCV2Max: tuning.highFrequencyTonicCV2Max,
            highFrequencyTonicLVMax: tuning.highFrequencyTonicLVMax,
            highFrequencyTonicMMMax: tuning.highFrequencyTonicMMMax,
            highFrequencySpikingShortUpperSec: hfsShortUpper,
            highFrequencySpikingQ80MaxSec: hfsQ80Max,
            highFrequencySpikingQ90MaxSec: hfsQ90Max,
            highFrequencySpikingEpochBridgeSec: hfsEpochBridge,
            highFrequencySpikingPauseBreakSec: pause?.seedLowerSec,
            highFrequencySpikingHardBreakSec: hfsHardBreak,
            highFrequencySpikingToleratedGapSec: 0.075,
            highFrequencySpikingMinSpikes: tuning.highFrequencySpikingMinSpikes,
            highFrequencySpikingShortFractionMin: tuning.highFrequencySpikingShortFractionMin,
            highFrequencySpikingAllowedLargeFraction: tuning.highFrequencySpikingAllowedLargeFraction,
            highFrequencySpikingMaxConsecutiveLargeISI: tuning.highFrequencySpikingMaxConsecutiveLargeISI,
            structuralBurstSupportWeight: resolution.structuralSeedSummary.burstSupportWeight
        )
    }
}

private func positive(_ value: Double, fallback: Double) -> Double {
    value.isFinite && value > 0 ? value : fallback
}

private func clampedFraction(_ value: Double, fallback: Double) -> Double {
    guard value.isFinite else {
        return fallback
    }
    return min(max(value, 0), 1)
}
