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
    /// Relaxed regularity bands for the irregular-tonic subtype (only used after classic
    /// regularity fails and tonic-family structural eligibility passes). Classic tonic
    /// defaults are unchanged; these are strictly wider and are explicit/auditable.
    public var irregularTonicCVMax: Double
    public var irregularTonicCV2Max: Double
    public var irregularTonicLVMax: Double
    public var tonicBurstSeedFractionMax: Double
    public var highFrequencyTonicFloorSec: Double
    public var highFrequencyTonicUpperSec: Double
    public var highFrequencyTonicMinSpikes: Int
    public var highFrequencyTonicLowTailFractionMax: Double
    public var highFrequencyTonicCVMax: Double
    public var highFrequencyTonicCV2Max: Double
    public var highFrequencyTonicLVMax: Double
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
    /// Manual tonic ISI hard-gate band (seconds). When set, the tonic structural search band is
    /// narrowed to this range BEFORE candidate generation, so a manual tonic hard gate actually
    /// constrains which runs can become tonic candidates (the adaptive search band ignores
    /// `tonicLowerSec`/`tonicUpperSec` directly). Left nil by the adaptive path so default
    /// behavior is unchanged; only the manual-threshold resolver sets these.
    public var manualTonicHardLowerSec: Double? = nil
    public var manualTonicHardUpperSec: Double? = nil
    /// TSW-INTEGRATION: when true (the default), the PRIMARY tonic run source is the from-sequence
    /// expandable sliding window (`TonicStructuralWindowDetector.scanRefined`) instead of the adaptive
    /// band-membership run scan. The per-run classic/irregular gates, burst / fast-packet guards, subtype
    /// routing, boundary rescue, and arbitration are UNCHANGED — only the run SEED changes, so a slower /
    /// wider tonic run stays ONE maximal candidate rather than being clipped by the band upper and
    /// fragmented. Set false to restore the legacy band-membership seed (A/B / rollback). Not an init
    /// parameter, so every existing construction defaults to the new primary path.
    public var tonicStructuralWindowPrimary: Bool = true

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
        irregularTonicCVMax: Double = 0.60,
        irregularTonicCV2Max: Double = 0.60,
        irregularTonicLVMax: Double = 0.80,
        tonicBurstSeedFractionMax: Double = 0.20,
        highFrequencyTonicFloorSec: Double = 0,
        highFrequencyTonicUpperSec: Double = 0,
        highFrequencyTonicMinSpikes: Int = 6,
        highFrequencyTonicLowTailFractionMax: Double = 0.05,
        highFrequencyTonicCVMax: Double = 0.30,
        highFrequencyTonicCV2Max: Double = 0.30,
        highFrequencyTonicLVMax: Double = 0.35,
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
        // Irregular bands clamp to be no tighter than the classic bands, so the irregular
        // subtype can only ever be a superset of classic acceptance, never stricter.
        self.irregularTonicCVMax = max(self.tonicCVMax, positive(irregularTonicCVMax, fallback: 0.60))
        self.irregularTonicCV2Max = max(self.tonicCV2Max, positive(irregularTonicCV2Max, fallback: 0.60))
        self.irregularTonicLVMax = max(self.tonicLVMax, positive(irregularTonicLVMax, fallback: 0.80))
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
    public var irregularTonicCVMax: Double
    public var irregularTonicCV2Max: Double
    public var irregularTonicLVMax: Double
    public var tonicBurstSeedFractionMax: Double
    public var highFrequencyTonicMinSpikes: Int
    public var highFrequencyTonicLowTailFractionMax: Double
    public var highFrequencyTonicCVMax: Double
    public var highFrequencyTonicCV2Max: Double
    public var highFrequencyTonicLVMax: Double
    public var highFrequencySpikingMinSpikes: Int
    public var highFrequencySpikingShortFractionMin: Double
    public var highFrequencySpikingAllowedLargeFraction: Double
    public var highFrequencySpikingMaxConsecutiveLargeISI: Int

    public init(
        tonicMinSpikes: Int = 5,
        tonicCVMax: Double = 0.30,
        tonicCV2Max: Double = 0.30,
        tonicLVMax: Double = 0.35,
        irregularTonicCVMax: Double = 0.60,
        irregularTonicCV2Max: Double = 0.60,
        irregularTonicLVMax: Double = 0.80,
        tonicBurstSeedFractionMax: Double = 0.20,
        highFrequencyTonicMinSpikes: Int = 6,
        highFrequencyTonicLowTailFractionMax: Double = 0.05,
        highFrequencyTonicCVMax: Double = 0.30,
        highFrequencyTonicCV2Max: Double = 0.30,
        highFrequencyTonicLVMax: Double = 0.35,
        highFrequencySpikingMinSpikes: Int = 30,
        highFrequencySpikingShortFractionMin: Double = 0.70,
        highFrequencySpikingAllowedLargeFraction: Double = 0.25,
        highFrequencySpikingMaxConsecutiveLargeISI: Int = 3
    ) {
        self.tonicMinSpikes = max(3, tonicMinSpikes)
        self.tonicCVMax = positive(tonicCVMax, fallback: 0.30)
        self.tonicCV2Max = positive(tonicCV2Max, fallback: 0.30)
        self.tonicLVMax = positive(tonicLVMax, fallback: 0.35)
        self.irregularTonicCVMax = max(self.tonicCVMax, positive(irregularTonicCVMax, fallback: 0.60))
        self.irregularTonicCV2Max = max(self.tonicCV2Max, positive(irregularTonicCV2Max, fallback: 0.60))
        self.irregularTonicLVMax = max(self.tonicLVMax, positive(irregularTonicLVMax, fallback: 0.80))
        self.tonicBurstSeedFractionMax = clampedFraction(tonicBurstSeedFractionMax, fallback: 0.20)
        self.highFrequencyTonicMinSpikes = max(3, highFrequencyTonicMinSpikes)
        self.highFrequencyTonicLowTailFractionMax = clampedFraction(highFrequencyTonicLowTailFractionMax, fallback: 0.05)
        self.highFrequencyTonicCVMax = positive(highFrequencyTonicCVMax, fallback: 0.30)
        self.highFrequencyTonicCV2Max = positive(highFrequencyTonicCV2Max, fallback: 0.30)
        self.highFrequencyTonicLVMax = positive(highFrequencyTonicLVMax, fallback: 0.35)
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

    /// Conservatively merge adjacent selectable irregular-tonic state fragments of the same
    /// train across a tiny, non-event, non-pause gap. State-level continuity only — not a
    /// threshold relaxation. The merged span is re-validated through the same tonic-family
    /// structural-eligibility and irregular regularity gates; the child fragments are consumed
    /// (removed) so the merged state is selected deterministically over them.
    ///
    /// A large gap is never bridged just because the neighbors are irregular tonic: selected
    /// pauses, selected burst-family events, and HFS / HF-tonic regions inside the gap block
    /// the merge, and the gap must also satisfy the adaptive micro-gap cap.
    public static func mergeIrregularTonicMicroGaps(
        train: SpikeTrain,
        candidates: [ClassicAnchorCandidate],
        selectedEvents: [ClassicAnchorCandidate],
        selectedGaps: [ClassicAnchorCandidate],
        settings: StatePatternDetectorSettings
    ) -> [ClassicAnchorCandidate] {
        let fragments = candidates
            .filter {
                $0.trainID == train.id &&
                    $0.finalLabel == .tonic &&
                    $0.stateTonicSubtype == "irregular" &&
                    $0.isEligibleForAutoSelection
            }
            .sorted {
                if $0.startISIIndex != $1.startISIIndex {
                    return $0.startISIIndex < $1.startISIIndex
                }
                return $0.endISIIndex < $1.endISIIndex
            }
        guard fragments.count >= 2 else {
            return candidates
        }

        let localContext = SpikeISILocalContextTable.build(
            for: train,
            settings: SpikeISILocalContextSettings(
                minValidISISec: settings.minValidISISec,
                halfWindow: max(3, settings.tonicMinSpikes)
            )
        )
        let bounds = tonicStructuralSearchBounds(train: train, settings: settings)
        let strictFlags = train.isiSec.indices.map { index -> Bool in
            guard index > 0,
                  let value = finiteValidISI(train.isiSec[index], settings: settings) else {
                return false
            }
            return isTonicStructuralSupport(index: index, value: value, bounds: bounds, localContext: localContext)
        }

        var consumed = Set<String>()
        var mergedCandidates: [ClassicAnchorCandidate] = []

        var i = 0
        while i < fragments.count {
            let first = fragments[i]
            var accStart = min(first.startISIIndex, first.endISIIndex)
            var accEnd = max(first.startISIIndex, first.endISIIndex)
            var childIDs = [first.id]
            var gapISIs: [Double] = []
            var bestMerge: ClassicAnchorCandidate?

            var j = i + 1
            while j < fragments.count {
                let next = fragments[j]
                let nextStart = min(next.startISIIndex, next.endISIIndex)
                let nextEnd = max(next.startISIIndex, next.endISIIndex)
                guard nextStart > accEnd else { break }

                let gapLower = accEnd + 1
                let gapUpper = nextStart - 1
                let hasGap = gapLower <= gapUpper
                let gapISIValues = hasGap
                    ? (gapLower...gapUpper).compactMap { idx -> Double? in
                        guard train.isiSec.indices.contains(idx) else { return nil }
                        return finiteValidISI(train.isiSec[idx], settings: settings)
                    }
                    : []
                let gapCount = hasGap ? (gapUpper - gapLower + 1) : 0

                if hasGap, microGapBlockedByBoundary(
                    gapLower: gapLower,
                    gapUpper: gapUpper,
                    selectedEvents: selectedEvents,
                    selectedGaps: selectedGaps,
                    candidates: candidates
                ) {
                    break
                }

                let cap = microGapCap(
                    train: train,
                    leftStart: accStart, leftEnd: accEnd,
                    rightStart: nextStart, rightEnd: nextEnd,
                    settings: settings
                )
                let maxGap = gapISIValues.max() ?? 0
                // Respect tonicMaxBridgeISI exactly: 0 disables bridging entirely (only directly
                // adjacent fragments with no intervening gap may merge), and never allows more
                // intervening ISIs than the setting permits.
                guard gapCount <= settings.tonicMaxBridgeISI,
                      maxGap <= cap + tolerance(for: cap) else {
                    break
                }

                guard let merged = revalidatedIrregularTonicMerge(
                    train: train,
                    start: accStart, end: nextEnd,
                    settings: settings, localContext: localContext,
                    bounds: bounds, strictFlags: strictFlags,
                    childIDs: childIDs + [next.id],
                    gapISIs: gapISIs + gapISIValues,
                    gapCount: gapISIs.count + gapCount,
                    cap: cap
                ) else {
                    break
                }

                bestMerge = merged
                accEnd = nextEnd
                childIDs.append(next.id)
                gapISIs.append(contentsOf: gapISIValues)
                j += 1
            }

            if let merged = bestMerge, childIDs.count >= 2 {
                mergedCandidates.append(merged)
                consumed.formUnion(childIDs)
                i = j
            } else {
                i += 1
            }
        }

        guard !mergedCandidates.isEmpty else {
            return candidates
        }
        return candidates.filter { !consumed.contains($0.id) } + mergedCandidates
    }

    private static func microGapBlockedByBoundary(
        gapLower: Int,
        gapUpper: Int,
        selectedEvents: [ClassicAnchorCandidate],
        selectedGaps: [ClassicAnchorCandidate],
        candidates: [ClassicAnchorCandidate]
    ) -> Bool {
        guard gapLower <= gapUpper else { return false }
        func overlapsGap(_ candidate: ClassicAnchorCandidate) -> Bool {
            let start = min(candidate.startISIIndex, candidate.endISIIndex)
            let end = max(candidate.startISIIndex, candidate.endISIIndex)
            return start <= gapUpper && end >= gapLower
        }
        if selectedGaps.contains(where: { $0.finalLabel == .pause && overlapsGap($0) }) {
            return true
        }
        if selectedEvents.contains(where: { $0.finalLabel.isBurstEventFamily && overlapsGap($0) }) {
            return true
        }
        if candidates.contains(where: {
            ($0.finalLabel == .highFrequencySpiking || $0.finalLabel == .highFrequencyTonic) &&
                $0.isEligibleForAutoSelection && overlapsGap($0)
        }) {
            return true
        }
        return false
    }

    private static func microGapCap(
        train: SpikeTrain,
        leftStart: Int, leftEnd: Int,
        rightStart: Int, rightEnd: Int,
        settings: StatePatternDetectorSettings
    ) -> Double {
        let leftValues = microGapISIValues(train: train, start: leftStart, end: leftEnd, settings: settings)
        let rightValues = microGapISIValues(train: train, start: rightStart, end: rightEnd, settings: settings)
        let pooled = leftValues + rightValues
        let pooledMedian = quantile(pooled, probability: 0.50) ?? 0
        let neighborQ90Cap = max(
            quantile(leftValues, probability: 0.90) ?? 0,
            quantile(rightValues, probability: 0.90) ?? 0
        )
        let caps = [settings.tonicBridgeUpperSec, 1.5 * pooledMedian, neighborQ90Cap]
            .filter { $0.isFinite && $0 > 0 }
        return caps.min() ?? settings.tonicBridgeUpperSec
    }

    private static func microGapISIValues(
        train: SpikeTrain,
        start: Int, end: Int,
        settings: StatePatternDetectorSettings
    ) -> [Double] {
        guard start <= end else { return [] }
        return (start...end).compactMap { idx in
            train.isiSec.indices.contains(idx) ? finiteValidISI(train.isiSec[idx], settings: settings) : nil
        }
    }

    private static func revalidatedIrregularTonicMerge(
        train: SpikeTrain,
        start: Int, end: Int,
        settings: StatePatternDetectorSettings,
        localContext: SpikeISILocalContextTable,
        bounds: (lower: Double, upper: Double),
        strictFlags: [Bool],
        childIDs: [String],
        gapISIs: [Double],
        gapCount: Int,
        cap: Double
    ) -> ClassicAnchorCandidate? {
        let run = (start: start, end: end)
        guard let metrics = metrics(train: train, run: run, settings: settings, localContext: localContext),
              metrics.nSpikes >= settings.tonicMinSpikes else {
            return nil
        }

        let bridgeCount = tonicBridgeCount(run: run, strictFlags: strictFlags)
        let bridgeFraction = Double(bridgeCount) / Double(max(1, metrics.nValidISI))
        let bridgeFractionPass = bridgeFraction <= settings.tonicBridgeFractionMax + 1e-12
        let burstSeedFraction = fraction(metrics.values) {
            $0 <= settings.burstSeedUpperSec + tolerance(for: settings.burstSeedUpperSec)
        }
        let burstSeedFractionPass = burstSeedFraction <= effectiveTonicBurstSeedFractionMax(settings: settings) + 1e-12
        let coreRunLength = maxBurstSeedRun(train: train, run: run, settings: settings)
        let coreRunPass = coreRunLength <= tonicBurstCoreRunLimit(settings: settings)
        let fastPacketCoreUpper = fastPacketCoreUpperSec(settings: settings)
        let fastPacketFraction = fraction(metrics.values) {
            $0 <= fastPacketCoreUpper + tolerance(for: fastPacketCoreUpper)
        }
        let fastPacketPass = fastPacketFraction <= fastPacketCoreOccupancyMax(settings: settings) + 1e-12
        let structuralEligibilityPass = burstSeedFractionPass && coreRunPass && fastPacketPass && bridgeFractionPass
        guard structuralEligibilityPass else { return nil }

        let classicRegularityPass =
            (metrics.cv.map { $0 <= settings.tonicCVMax + 1e-12 } ?? true) &&
            (metrics.cv2.map { $0 <= settings.tonicCV2Max + 1e-12 } ?? true) &&
            (metrics.lv.map { $0 <= settings.tonicLVMax + 1e-12 } ?? true)
        let irregularRegularityPass =
            (metrics.cv.map { $0 <= settings.irregularTonicCVMax + 1e-12 } ?? true) &&
            (metrics.cv2.map { $0 <= settings.irregularTonicCV2Max + 1e-12 } ?? true) &&
            (metrics.lv.map { $0 <= settings.irregularTonicLVMax + 1e-12 } ?? true)
        guard classicRegularityPass || irregularRegularityPass else { return nil }
        let subtype = classicRegularityPass ? "classic" : "irregular"

        let regularityScore = mean([
            metrics.cv.map { 1 / (1 + $0) },
            metrics.cv2.map { 1 / (1 + $0) },
            metrics.lv.map { 1 / (1 + $0) }
        ].compactMap { $0 }) ?? 0
        let maxGap = gapISIs.max() ?? 0
        let decisionPath = stateDecisionPath(
            base: "irregular_tonic_micro_gap_merge_state",
            metrics: metrics,
            extra: [
                "tonic_subtype=\(subtype)",
                "irregular_tonic_micro_gap_merge=true",
                "merge_revalidated=true",
                "merged_fragment_count=\(childIDs.count)",
                "merged_gap_count=\(gapCount)",
                "max_merged_gap_sec=\(format(maxGap))",
                "micro_gap_cap_sec=\(format(cap))",
                "micro_gap_source=adaptive_neighbor_tonic_scale",
                "merged_child_ids=\(childIDs.joined(separator: "|"))",
                "cv=\(format(metrics.cv))",
                "cv2=\(format(metrics.cv2))",
                "lv=\(format(metrics.lv))",
                "max_isi_sec=\(format(metrics.max))",
                "burst_seed_fraction=\(format(burstSeedFraction))",
                "core_burst_run_length=\(coreRunLength)",
                "fast_packet_fraction=\(format(fastPacketFraction))",
                "bridge_fraction=\(format(bridgeFraction))",
                "classic_regularity_pass=\(classicRegularityPass)",
                "irregular_regularity_pass=\(irregularRegularityPass)",
                "tonic_family_structural_eligibility_pass=\(structuralEligibilityPass)"
            ]
        )
        let score = 3 + regularityScore + localStabilityScore(metrics)
        return candidate(
            train: train,
            run: run,
            metrics: metrics,
            label: .tonic,
            layer: "event_core_irregular_tonic_micro_gap_merge",
            candidateClass: "event_core_tonic",
            gateStatus: "event_core_irregular_tonic_micro_gap_merge_pass",
            decisionPath: decisionPath,
            score: score,
            priority: 1_100,
            bandLower: bounds.lower,
            bandUpper: bounds.upper,
            contrastMinRequired: settings.tonicLVMax,
            contrastGeomRequired: settings.tonicCV2Max,
            stateRegularityScore: regularityScore,
            stateBurstSeedFraction: burstSeedFraction,
            stateCoreBurstRunLength: coreRunLength,
            stateTonicSubtype: subtype,
            index: 0,
            id: "\(train.id)-irregular-tonic-micro-merge-\(start)-\(end)"
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
                rejected.stateHighFrequencySubtype = packetization.burstDominated
                    ? "hf_burst_dominant"
                    : "hf_irregular_spiking"
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
            // HF family subtype: a sustained HF state is irregular HF spiking unless it is
            // burst-dominated. finalLabel stays .highFrequencySpiking.
            hfs.stateHighFrequencySubtype = (hfs.hfSpikingBurstDominated == true)
                ? "hf_burst_dominant"
                : "hf_irregular_spiking"
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
            (metrics.cv2 ?? 0) >= 0.55 ||
            (metrics.lv ?? 0) >= 0.40
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
        var lower = max(
            settings.minValidISISec,
            settings.burstSeedUpperSec * 1.25,
            min(q10, q20, adaptiveUpper)
        )
        var upper = max(lower, adaptiveUpper)
        // A manual tonic ISI hard gate narrows the support band BEFORE candidate generation: the
        // lower can only rise and the upper can only fall. If the manual band excludes the run's
        // ISIs entirely the band becomes empty (upper < lower), and isTonicStructuralSupport then
        // flags nothing — so every tonic code path (classic, irregular-merge, split-rebuild) is
        // constrained uniformly. Soft anchors never set these fields, so they can only ever leave
        // the band unchanged here (they never remove a tonic candidate).
        if let manualLower = settings.manualTonicHardLowerSec, manualLower.isFinite, manualLower > 0 {
            lower = max(lower, manualLower)
        }
        if let manualUpper = settings.manualTonicHardUpperSec, manualUpper.isFinite, manualUpper > 0 {
            upper = min(upper, manualUpper)
        }
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

    /// Relative-baseline HF-tonic recall constants (ratio/percentile/count — never fixed ms). A
    /// candidate ISI is "fast relative to the train background" when it is at most this fraction of
    /// the train's own non-burst background q50. This fraction (< 1) IS the candidate-vs-background
    /// separation guard: a uniform classic-tonic train, whose fastest ISIs are only the lower-noise
    /// tail of the same baseline, never seeds the relative route because nothing sits below 0.6x its
    /// own median.
    private static let relativeHighFrequencyTonicBackgroundRatioMax = 0.6
    /// Train-relative "fast" percentile, recorded for audit (the fastest ~35% of the train's ISIs),
    /// the Mac analogue of R/11's percentile HF seed.
    private static let relativeHighFrequencyTonicPercentileCeiling = 0.35
    /// Reliability floor: the relative route stays disabled unless the non-burst background has at
    /// least this many valid ISIs, so the percentile/background estimate is stable. Derived only from
    /// the train's own ISIs (no cross-train leakage).
    private static let relativeHighFrequencyTonicMinBackgroundISI = 40

    /// Train-relative non-burst background used by the HF-tonic relative recall route. Returns nil
    /// (route disabled, behavior unchanged) when the background sample is too small to be reliable.
    private static func relativeHighFrequencyTonicBackground(
        train: SpikeTrain,
        settings: StatePatternDetectorSettings
    ) -> (backgroundQ50: Double, backgroundQ75: Double, percentileCeiling: Double, validCount: Int)? {
        let nonBurst = train.isiSec.compactMap { value -> Double? in
            guard let value = finiteValidISI(value, settings: settings),
                  value > settings.burstSeedUpperSec + tolerance(for: settings.burstSeedUpperSec) else {
                return nil
            }
            return value
        }
        guard nonBurst.count >= relativeHighFrequencyTonicMinBackgroundISI else {
            return nil
        }
        let sample = SortedFiniteSample(nonBurst, positiveOnly: true)
        guard let q50 = sample.quantile(0.50), q50.isFinite, q50 > 0,
              let q75 = sample.quantile(0.75) else {
            return nil
        }
        let allValid = train.isiSec.compactMap { finiteValidISI($0, settings: settings) }
        let percentileCeiling = quantile(allValid, probability: relativeHighFrequencyTonicPercentileCeiling) ?? q50
        return (q50, q75, percentileCeiling, nonBurst.count)
    }

    private static func detectHighFrequencyTonic(
        train: SpikeTrain,
        settings: StatePatternDetectorSettings,
        localContext: SpikeISILocalContextTable
    ) -> [ClassicAnchorCandidate] {
        let highMax = settings.highFrequencyTonicUpperSec
        // Relative-baseline recall: a per-train ceiling derived from the train's own non-burst
        // background (ratios only, never a fixed ms). It is nil when the background sample is too
        // small to be reliable, in which case only the fixed band is used and behavior is unchanged.
        let relativeBackground = relativeHighFrequencyTonicBackground(train: train, settings: settings)
        let relativeSupportCeiling = relativeBackground.map {
            $0.backgroundQ50 * relativeHighFrequencyTonicBackgroundRatioMax
        }
        let fixedFlags = train.isiSec.indices.map { index -> Bool in
            guard index > 0,
                  let value = finiteValidISI(train.isiSec[index], settings: settings) else {
                return false
            }
            return value <= highMax + tolerance(for: highMax)
        }
        // Relatively-fast ISIs: clearly below the background ceiling AND above the burst-core floor,
        // so burst-core packets are not seeded into a tonic state.
        let relativeFlags = train.isiSec.indices.map { index -> Bool in
            guard index > 0,
                  let ceiling = relativeSupportCeiling,
                  let value = finiteValidISI(train.isiSec[index], settings: settings),
                  value > settings.burstSeedUpperSec + tolerance(for: settings.burstSeedUpperSec) else {
                return false
            }
            return value <= ceiling + tolerance(for: ceiling)
        }
        let flags = zip(fixedFlags, relativeFlags).map { $0 || $1 }

        var candidates: [ClassicAnchorCandidate] = []
        for run in boolRuns(flags) {
            guard let metrics = metrics(train: train, run: run, settings: settings, localContext: localContext),
                  metrics.nSpikes >= settings.highFrequencyTonicMinSpikes else {
                continue
            }
            // A run is relative-supported only when it exists/extends because of a relatively-fast ISI
            // the fixed band did not already include (which can only happen when the relative ceiling
            // exceeds the fixed band). Pure fixed-band runs keep the exact previous behavior.
            let relativeSupported = (run.start...run.end).contains { index in
                relativeFlags.indices.contains(index) && relativeFlags[index] && !fixedFlags[index]
            }
            let effectiveHighMax = relativeSupported
                ? max(highMax, relativeSupportCeiling ?? highMax)
                : highMax
            let relativeProvenance: [String] = relativeSupported
                ? [
                    "hf_tonic_recall_route=relative_baseline",
                    "relative_hf_background_q50_sec=\(format(relativeBackground?.backgroundQ50))",
                    "relative_hf_background_q75_sec=\(format(relativeBackground?.backgroundQ75))",
                    "relative_hf_candidate_q90_background_ratio=\(format(relativeBackground.flatMap { bg in metrics.q90.map { $0 / bg.backgroundQ50 } }))",
                    "relative_hf_percentile_ceiling=\(format(relativeBackground?.percentileCeiling))",
                    "relative_hf_support_ceiling_sec=\(format(relativeSupportCeiling))",
                    "relative_hf_valid_isi_n=\(relativeBackground?.validCount ?? 0)"
                ]
                : ["hf_tonic_recall_route=fixed_band"]

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
            let q90Pass = metrics.q90.map { $0 <= effectiveHighMax + tolerance(for: effectiveHighMax) } ?? false
            let burstCoreVetoPass = coreRunLength < settings.highFrequencyTonicBurstCoreVetoMinISI
            let fastPacketCoreUpper = fastPacketCoreUpperSec(settings: settings)
            let fastPacketFraction = fraction(metrics.values) {
                $0 <= fastPacketCoreUpper + tolerance(for: fastPacketCoreUpper)
            }
            let fastPacketOccupancyMax = fastPacketCoreOccupancyMax(settings: settings)
            let fastPacketPass = fastPacketFraction <= fastPacketOccupancyMax + 1e-12
            let cvPass = metrics.cv.map { $0 <= settings.highFrequencyTonicCVMax + 1e-12 } ?? true
            let cv2Pass = metrics.cv2.map { $0 <= settings.highFrequencyTonicCV2Max + 1e-12 } ?? true
            let lvPass = metrics.lv.map { $0 <= settings.highFrequencyTonicLVMax + 1e-12 } ?? true
            let classicBoundaryPass = !hasClassicBoundary(metrics: metrics, settings: settings)
            let regularityScore = mean([
                metrics.cv.map { 1 / (1 + $0) },
                metrics.cv2.map { 1 / (1 + $0) },
                metrics.lv.map { 1 / (1 + $0) }
            ].compactMap { $0 }) ?? 0
            if !q90Pass || !lowTailPass || !burstSeedFractionPass || !burstCoreVetoPass || !fastPacketPass || !cvPass || !cv2Pass || !lvPass || !classicBoundaryPass {
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
                if !fastPacketPass {
                    rejectReasons.append("reject_hf_tonic_fast_packet_core_occupancy")
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
                if !classicBoundaryPass {
                    rejectReasons.append("reject_hf_tonic_has_classic_burst_boundary")
                }
                let rejectDecisionPath = stateDecisionPath(
                    base: "reject_high_frequency_tonic_state_candidate",
                    metrics: metrics,
                    extra: rejectReasons + relativeProvenance + [
                        "low_tail_fraction=\(format(lowTail))",
                        "low_tail_fraction_max_effective=\(format(effectiveLowTailMax))",
                        "burst_seed_fraction=\(format(burstSeedFraction))",
                        "burst_seed_fraction_max_effective=\(format(burstSeedFractionMax))",
                        "structural_burst_support_weight=\(format(settings.structuralBurstSupportWeight))",
                        "core_burst_run_length=\(coreRunLength)",
                        "state_burst_packet_guard=hf_tonic_rejects_when_short_tail_forms_burst_seed_occupancy",
                        "fast_packet_core_upper_sec=\(format(fastPacketCoreUpper))",
                        "fast_packet_fraction=\(format(fastPacketFraction))",
                        "fast_packet_core_occupancy_max=\(format(fastPacketOccupancyMax))",
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
                        bandUpper: effectiveHighMax,
                        contrastMinRequired: settings.highFrequencyTonicLVMax,
                        contrastGeomRequired: settings.highFrequencyTonicCV2Max,
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
                extra: relativeProvenance + [
                    "tonic_subtype=high_frequency",
                    "low_tail_fraction=\(format(lowTail))",
                    "low_tail_fraction_max_effective=\(format(effectiveLowTailMax))",
                    "burst_seed_fraction=\(format(burstSeedFraction))",
                    "burst_seed_fraction_max_effective=\(format(burstSeedFractionMax))",
                    "structural_burst_support_weight=\(format(settings.structuralBurstSupportWeight))",
                    "core_burst_run_length=\(coreRunLength)",
                    "state_burst_packet_guard=pass",
                    "fast_packet_core_upper_sec=\(format(fastPacketCoreUpper))",
                    "fast_packet_fraction=\(format(fastPacketFraction))",
                    "fast_packet_core_occupancy_max=\(format(fastPacketOccupancyMax))",
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
                    bandUpper: effectiveHighMax,
                    contrastMinRequired: settings.highFrequencyTonicLVMax,
                    contrastGeomRequired: settings.highFrequencyTonicCV2Max,
                    stateRegularityScore: regularityScore,
                    stateBurstSeedFraction: burstSeedFraction,
                    stateLowTailFraction: lowTail,
                    stateCoreBurstRunLength: coreRunLength,
                    stateTonicSubtype: "high_frequency",
                    stateHighFrequencySubtype: "hf_tonic_spiking",
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

        // PRIMARY tonic run source. Default (tonicStructuralWindowPrimary): the from-sequence expandable
        // sliding window — seed = tonicMinSpikes-1 ISIs, scan left→right by one ISI, expand while the whole
        // span passes the tonic structural gates, emit the maximal window, resume after it. It is NOT
        // constrained to the adaptive tonic band, so a slower / wider tonic run stays ONE candidate instead
        // of being clipped by the band upper into sub-minSpikes fragments. Legacy band-membership seed kept
        // behind the flag. Either way, the SAME per-run gates / subtype routing / guards below run on the runs.
        let tonicRunSpecs: [(run: (start: Int, end: Int), tsw: TonicStructuralWindowCandidate?, carve: [String])]
        if settings.tonicStructuralWindowPrimary {
            tonicRunSpecs = tonicStructuralWindowRuns(train: train, settings: settings, bounds: bounds)
                .map { ($0.run, $0.window, $0.carve) }
        } else {
            tonicRunSpecs = mergeTonicSupportRuns(boolRuns(strictFlags), train: train, settings: settings)
                .map { ($0, nil, []) }
        }

        for spec in tonicRunSpecs {
            let run = spec.run
            guard let metrics = metrics(train: train, run: run, settings: settings, localContext: localContext),
                  metrics.nSpikes >= settings.tonicMinSpikes else {
                continue
            }
            let tswProvenance = (spec.tsw.map(tonicStructuralWindowProvenance) ?? []) + spec.carve

            // A TSW run is a WHOLE validated regular window (TSW enforced CV/CV2/LV + adjacent-ratio +
            // burst-cleanliness on the full span), so the band-coupled "bridge" audit — which counts ISIs
            // that fall outside the narrow support band — does not apply: every ISI in the window is tonic-
            // supported by construction. Applying it would re-introduce the exact band-clipping the sliding
            // window removes (a clean run whose jitter pokes just outside the band would be spuriously
            // rejected). bridgeCount is 0 for TSW runs; the legacy band path keeps its strict-flag audit.
            let bridgeCount = spec.tsw != nil ? 0 : tonicBridgeCount(run: run, strictFlags: strictFlags)
            let bridgeFraction = Double(bridgeCount) / Double(max(1, metrics.nValidISI))
            let bridgeFractionPass = bridgeFraction <= settings.tonicBridgeFractionMax + 1e-12
            let burstSeedFraction = fraction(metrics.values) { $0 <= settings.burstSeedUpperSec + tolerance(for: settings.burstSeedUpperSec) }
            let effectiveBurstSeedFractionMax = effectiveTonicBurstSeedFractionMax(settings: settings)
            let coreRunLength = maxBurstSeedRun(train: train, run: run, settings: settings)
            let coreRunLimit = tonicBurstCoreRunLimit(settings: settings)
            let cvPass = metrics.cv.map { $0 <= settings.tonicCVMax + 1e-12 } ?? true
            let cv2Pass = metrics.cv2.map { $0 <= settings.tonicCV2Max + 1e-12 } ?? true
            let lvPass = metrics.lv.map { $0 <= settings.tonicLVMax + 1e-12 } ?? true
            let burstSeedFractionPass = burstSeedFraction <= effectiveBurstSeedFractionMax + 1e-12
            let coreRunPass = coreRunLength <= coreRunLimit
            let fastPacketCoreUpper = fastPacketCoreUpperSec(settings: settings)
            let fastPacketFraction = fraction(metrics.values) {
                $0 <= fastPacketCoreUpper + tolerance(for: fastPacketCoreUpper)
            }
            let fastPacketOccupancyMax = fastPacketCoreOccupancyMax(settings: settings)
            let fastPacketPass = fastPacketFraction <= fastPacketOccupancyMax + 1e-12
            let regularityScore = mean([
                metrics.cv.map { 1 / (1 + $0) },
                metrics.cv2.map { 1 / (1 + $0) },
                metrics.lv.map { 1 / (1 + $0) }
            ].compactMap { $0 }) ?? 0
            // Tonic family subtype. finalLabel stays .tonic for classic and irregular; only
            // the auditable subtype differs. Structural eligibility (burst-seed / core-run /
            // fast-packet / bridge guards) is required for BOTH subtypes. Irregular only
            // relaxes the regularity bands AFTER eligibility holds, so it can never become a
            // backdoor for burst / fast-packet / pause-dominated runs.
            let structuralEligibilityPass = burstSeedFractionPass && coreRunPass && fastPacketPass && bridgeFractionPass
            let classicRegularityPass = cvPass && cv2Pass && lvPass
            let irregularCvPass = metrics.cv.map { $0 <= settings.irregularTonicCVMax + 1e-12 } ?? true
            let irregularCv2Pass = metrics.cv2.map { $0 <= settings.irregularTonicCV2Max + 1e-12 } ?? true
            let irregularLvPass = metrics.lv.map { $0 <= settings.irregularTonicLVMax + 1e-12 } ?? true
            let irregularRegularityPass = irregularCvPass && irregularCv2Pass && irregularLvPass
            let tonicSubtype: String?
            if structuralEligibilityPass && classicRegularityPass {
                tonicSubtype = "classic"
            } else if structuralEligibilityPass && irregularRegularityPass {
                tonicSubtype = "irregular"
            } else {
                tonicSubtype = nil
            }
            let subtypeAudit = [
                "cv=\(format(metrics.cv))",
                "cv2=\(format(metrics.cv2))",
                "lv=\(format(metrics.lv))",
                "max_isi_sec=\(format(metrics.max))",
                "classic_regularity_pass=\(classicRegularityPass)",
                "irregular_regularity_pass=\(irregularRegularityPass)",
                "tonic_family_structural_eligibility_pass=\(structuralEligibilityPass)",
                "irregular_cv_max=\(format(settings.irregularTonicCVMax))",
                "irregular_cv2_max=\(format(settings.irregularTonicCV2Max))",
                "irregular_lv_max=\(format(settings.irregularTonicLVMax))"
            ]

            guard let acceptedSubtype = tonicSubtype else {
                var rejectReasons: [String] = []
                if !burstSeedFractionPass {
                    rejectReasons.append("reject_tonic_burst_seed_fraction_too_high")
                }
                if !coreRunPass {
                    rejectReasons.append("reject_tonic_contains_burst_seed_core_run")
                }
                if !fastPacketPass {
                    rejectReasons.append("reject_tonic_fast_packet_core_occupancy")
                }
                if !bridgeFractionPass {
                    rejectReasons.append("reject_tonic_bridge_fraction_too_high")
                }
                // Regularity is reported as unstable only when it exceeds even the relaxed
                // irregular bands; cv/cv2/lv between classic and irregular routes to irregular.
                if !irregularCvPass {
                    rejectReasons.append("reject_tonic_cv_unstable")
                }
                if !irregularCv2Pass {
                    rejectReasons.append("reject_tonic_cv2_unstable")
                }
                if !irregularLvPass {
                    rejectReasons.append("reject_tonic_lv_unstable")
                }
                let rejectDecisionPath = stateDecisionPath(
                    base: "reject_classic_tonic_state_candidate",
                    metrics: metrics,
                    extra: rejectReasons + [
                        "tonic_subtype=rejected",
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
                        "fast_packet_core_upper_sec=\(format(fastPacketCoreUpper))",
                        "fast_packet_fraction=\(format(fastPacketFraction))",
                        "fast_packet_core_occupancy_max=\(format(fastPacketOccupancyMax))",
                        "structural_burst_support_weight=\(format(settings.structuralBurstSupportWeight))",
                        "regularity_score=\(format(regularityScore))"
                    ] + subtypeAudit + tswProvenance
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
                        contrastGeomRequired: settings.tonicCV2Max,
                        stateRegularityScore: regularityScore,
                        stateBurstSeedFraction: burstSeedFraction,
                        stateCoreBurstRunLength: coreRunLength,
                        index: nextCandidateIndex
                    )
                )
                nextCandidateIndex += 1
                continue
            }

            let isIrregular = (acceptedSubtype == "irregular")
            let score = 3 + regularityScore + localStabilityScore(metrics)
            let decisionPath = stateDecisionPath(
                base: bridgeCount > 0 ? "stable_mid_isi_tonic_state_with_short_bridge" : "stable_mid_isi_tonic_state",
                metrics: metrics,
                extra: [
                    "tonic_subtype=\(acceptedSubtype)",
                    isIrregular ? "irregular_tonic" : "classic_tonic",
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
                    "fast_packet_core_upper_sec=\(format(fastPacketCoreUpper))",
                    "fast_packet_fraction=\(format(fastPacketFraction))",
                    "fast_packet_core_occupancy_max=\(format(fastPacketOccupancyMax))",
                    "structural_burst_support_weight=\(format(settings.structuralBurstSupportWeight))",
                    "regularity_score=\(format(regularityScore))"
                ] + subtypeAudit + tswProvenance
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
                    contrastGeomRequired: settings.tonicCV2Max,
                    stateRegularityScore: regularityScore,
                    stateBurstSeedFraction: burstSeedFraction,
                    stateCoreBurstRunLength: coreRunLength,
                    stateTonicSubtype: acceptedSubtype,
                    index: nextCandidateIndex
                )
            )
            nextCandidateIndex += 1
        }

        candidates = tonicBoundaryRescuedCandidates(
            candidates,
            train: train,
            settings: settings,
            localContext: localContext,
            bounds: bounds
        )

        // The legacy band-gated, length-capped (≤32 ISI) structural-window FILL is redundant once the
        // primary run source is itself the uncapped from-sequence sliding window: TSW already emits maximal
        // tonic windows, so this secondary gap-fill would only re-derive shorter, band-clipped copies. Keep
        // it only on the legacy path.
        if !settings.tonicStructuralWindowPrimary {
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
        }

        return candidates
    }

    /// TSW-INTEGRATION primary tonic run generator: the from-sequence expandable sliding window. Reads the
    /// train's QC ISI series directly (NOT the adaptive tonic band) via `TonicStructuralWindowDetector`
    /// and returns maximal tonic structural windows. Burst contamination is vetoed with the SAME burst
    /// boundary the burst detector uses (`burstSeedUpperSec`), and the tonic-core lower (`bounds.lower`,
    /// which already folds in the manual tonic hard gate) is the classic-tonic magnitude / low-side-trim
    /// floor — so burst-core ISIs never seed a tonic window and manual gates still bound it. Expansion stops
    /// at a large / irregular ISI, leaving that ISI for the pause detector. The emitted spans still flow
    /// through every per-run gate + subtype route + boundary rescue + arbitration in `detectTonic`.
    /// A pause-like ISI is at least this multiple of the tonic-window CORE median (scale-free relative gap).
    private static let tonicPauseLikeCoreRatio = 2.0
    /// …AND at least this multiple of the window's own q90 — the discriminator between a genuine bimodal
    /// PAUSE (well beyond the bulk of the run) and an ordinary IRREGULAR-tonic outlier (whose largest beats
    /// sit near q90). Dimensionless, so legitimate irregular tonic is preserved.
    private static let tonicPauseLikeQ90Ratio = 1.5

    private static func tonicStructuralWindowRuns(
        train: SpikeTrain,
        settings: StatePatternDetectorSettings,
        bounds: (lower: Double, upper: Double)
    ) -> [(run: (start: Int, end: Int), window: TonicStructuralWindowCandidate, carve: [String])] {
        // Drive the sliding window with the WIDEST tonic regularity bands (the irregular tier), so a single
        // maximal window can span both classic AND irregular tonic instead of fragmenting a moderately-
        // variable run into classic-only cores. The per-run gate in `detectTonic` then re-assigns the
        // classic-vs-irregular subtype from the span's ACTUAL cv/cv2/lv, so a clean run still routes classic.
        // Magnitude / burst-contamination protection is unchanged (it keys off burstSeedUpperSec, not CV).
        let thresholds = StructuralEvidenceThresholds(
            minimumValidISISec: settings.minValidISISec,
            burstSeedUpperSec: settings.burstSeedUpperSec,
            tonicMinSpikes: settings.tonicMinSpikes,
            tonicCVMax: settings.irregularTonicCVMax,
            tonicCV2Max: settings.irregularTonicCV2Max,
            tonicLVMax: settings.irregularTonicLVMax
        )
        let config = TonicStructuralWindowConfig(
            thresholds: thresholds,
            refractoryFloorSec: settings.minValidISISec
        )
        // Pause floor for the high-side trim / bounded bridge: well above the tonic ceiling, so only a
        // genuine pause (≫ tonic) is excluded. Expansion already stops at large ISIs and the bridge's CV
        // re-check already refuses to span a pause, so this is a conservative secondary safety.
        let pauseFloor = max(settings.tonicBridgeUpperSec, bounds.upper) * 2.0
        let windows = TonicStructuralWindowDetector.scanRefined(
            train: train,
            config: config,
            burstValleySec: settings.burstSeedUpperSec,
            minTonicSpikes: settings.tonicMinSpikes,
            burstValleySupportCount: nil,
            fallbackFloorSec: bounds.lower,
            pauseFloorSec: pauseFloor
        )
        // A manual tonic ISI HARD gate is an explicit magnitude constraint the sliding window (which is
        // magnitude-agnostic above the burst floor) does not otherwise honor. When set, drop any window that
        // strays outside [manualLower, manualUpper] — a hard-gated tonic candidate must lie entirely within
        // the user's band (mirrors the legacy per-ISI band membership, which excludes out-of-band ISIs).
        // No manual gate ⇒ this is a no-op and the maximal windows pass through unchanged.
        let gated: [TonicStructuralWindowCandidate]
        if settings.manualTonicHardLowerSec != nil || settings.manualTonicHardUpperSec != nil {
            let lo = settings.manualTonicHardLowerSec ?? 0
            let hi = settings.manualTonicHardUpperSec ?? .infinity
            gated = windows.filter { window in
                (window.span.startISIIndex...window.span.endISIIndex).allSatisfy { index in
                    guard let value = finiteValidISI(train.isiSec[index], settings: settings) else { return false }
                    return value >= lo - tolerance(for: lo) && value <= hi + tolerance(for: hi)
                }
            }
        } else {
            gated = windows
        }
        // CARVE each window at pause-like OUTLIERS: the TSW-3 bounded bridge can span a single large ISI to
        // merge two tonic runs; when that ISI is actually a pause (bimodal gap) rather than a tonic beat, the
        // merged window would swallow the pause as tonic. Split the window at any such ISI (excluded → left
        // for pause detection), keeping the adjacent true-tonic segments. Pause-like is RELATIVE only.
        let adjacentRatioHigh = thresholds.tonicLocalRatioHigh
        return gated.flatMap { window in
            carveTonicWindowAtPauseLikeOutliers(
                window: window, train: train, settings: settings, adjacentRatioHigh: adjacentRatioHigh)
        }
    }

    /// Split a tonic structural window at internal/boundary PAUSE-LIKE ISIs and return the surviving tonic
    /// segments (each ≥ the seed length) as runs, with carve provenance. An ISI is pause-like only on
    /// RELATIVE evidence (no fixed ms): it is ≥ `tonicPauseLikeCoreRatio`× the window CORE median, ≥
    /// `tonicPauseLikeQ90Ratio`× the window's own q90 (so it is beyond the bulk — a bimodal gap, not an
    /// irregular-tonic tail beat), AND an isolated jump (≥ `adjacentRatioHigh`× BOTH existing neighbors — the
    /// gap signature the bridge relaxed to admit it). Uncarved windows return one run with empty provenance.
    private static func carveTonicWindowAtPauseLikeOutliers(
        window: TonicStructuralWindowCandidate,
        train: SpikeTrain,
        settings: StatePatternDetectorSettings,
        adjacentRatioHigh: Double
    ) -> [(run: (start: Int, end: Int), window: TonicStructuralWindowCandidate, carve: [String])] {
        let start = window.span.startISIIndex
        let end = window.span.endISIIndex
        guard start <= end else { return [] }
        let indices = Array(start...end)
        let values = indices.map { finiteValidISI(train.isiSec[$0], settings: settings) ?? .nan }
        let sample = SortedFiniteSample(values.filter(\.isFinite), positiveOnly: true)
        let whole: [(run: (start: Int, end: Int), window: TonicStructuralWindowCandidate, carve: [String])]
            = [(run: (start, end), window: window, carve: [])]
        guard let coreMedian = sample.quantile(0.5), coreMedian > 0,
              let q90 = sample.quantile(0.90), q90 > 0 else {
            return whole
        }

        func isPauseLike(_ k: Int) -> Bool {
            let v = values[k]
            guard v.isFinite, v > 0 else { return false }
            guard v / coreMedian >= tonicPauseLikeCoreRatio,
                  v / q90 >= tonicPauseLikeQ90Ratio else { return false }
            var jumps: [Double] = []
            if k > 0, values[k - 1].isFinite, values[k - 1] > 0 { jumps.append(v / values[k - 1]) }
            if k < values.count - 1, values[k + 1].isFinite, values[k + 1] > 0 { jumps.append(v / values[k + 1]) }
            return !jumps.isEmpty && jumps.allSatisfy { $0 >= adjacentRatioHigh }
        }

        let pauseLike = indices.indices.filter(isPauseLike)
        guard !pauseLike.isEmpty else { return whole }

        // Provenance uses the WORST (largest relative) carved ISI.
        let worst = pauseLike.max { values[$0] / coreMedian < values[$1] / coreMedian }!
        let carveProvenance = [
            "tonic_pause_like_outlier",
            "split_by_pause_like_isi",
            "pause_ratio_to_tonic_core=\(format(values[worst] / coreMedian))",
            "pause_ratio_to_tonic_q90=\(format(values[worst] / q90))",
            "carved_from_tsw=[\(start)...\(end)]"
        ]
        let seedISI = max(2, settings.tonicMinSpikes - 1)
        let pauseSet = Set(pauseLike)
        var out: [(run: (start: Int, end: Int), window: TonicStructuralWindowCandidate, carve: [String])] = []
        var segStart: Int?
        func flush(_ segEndK: Int) {
            guard let s = segStart else { return }
            let runStart = indices[s], runEnd = indices[segEndK]
            if runEnd - runStart + 1 >= seedISI {
                out.append((run: (runStart, runEnd), window: window, carve: carveProvenance))
            }
            segStart = nil
        }
        for k in indices.indices {
            if pauseSet.contains(k) {
                flush(k - 1)
            } else if segStart == nil {
                segStart = k
            }
        }
        flush(indices.count - 1)
        return out
    }

    /// TSW-INTEGRATION provenance tokens for a tonic run seeded by the sliding window, appended to the
    /// candidate decisionPath: `tonic_structural_window`, `seed_window`/`expanded_window`, the route/source,
    /// `stopped_by=<boundaryReason>` (or `train_end`), and `refined_from=[start...end]` when TSW-3
    /// refinement changed the span.
    private static func tonicStructuralWindowProvenance(_ window: TonicStructuralWindowCandidate) -> [String] {
        var tokens = [
            "tonic_structural_window",
            window.source == .seed ? "seed_window" : "expanded_window",
            "tsw_source=\(window.source.rawValue)",
            "tsw_route=\(window.route.rawValue)",
            "stopped_by=\(window.boundaryReason?.rawValue ?? "train_end")"
        ]
        if let original = window.originalSpan {
            tokens.append("refined_from=[\(original.startISIIndex)...\(original.endISIIndex)]")
        }
        return tokens
    }

    /// Local post-pass for a narrow failure mode in the structure-first tonic detector: a stable tonic run can be
    /// clipped by one or two immediately-adjacent ISIs that are slightly above the train-local structural upper bound,
    /// even though the expanded run still satisfies the tonic regularity gates. This is deliberately NOT a residual
    /// tonic fill; only neighbors of an already-accepted tonic candidate are considered, with a tight high-side
    /// structural allowance and the same burst/fast-packet guards as the primary tonic route.
    private static func tonicBoundaryRescuedCandidates(
        _ candidates: [ClassicAnchorCandidate],
        train: SpikeTrain,
        settings: StatePatternDetectorSettings,
        localContext: SpikeISILocalContextTable,
        bounds: (lower: Double, upper: Double)
    ) -> [ClassicAnchorCandidate] {
        candidates.map { candidate in
            guard candidate.finalLabel == .tonic,
                  candidate.action == "accept",
                  candidate.startISIIndex <= candidate.endISIIndex else {
                return candidate
            }

            var current = candidate
            var rescuedLeft = 0
            var rescuedRight = 0
            var changed = false

            while rescuedLeft < tonicBoundaryRescueMaxISI {
                let nextStart = current.startISIIndex - 1
                guard tonicBoundaryRescueEdgeIsEligible(
                    index: nextStart,
                    reference: current,
                    train: train,
                    settings: settings,
                    bounds: bounds
                ),
                      let expanded = rebuildTonicBoundaryRescueCandidate(
                        train: train,
                        parent: current,
                        original: candidate,
                        run: (nextStart, current.endISIIndex),
                        leftRescued: rescuedLeft + 1,
                        rightRescued: rescuedRight,
                        settings: settings,
                        localContext: localContext,
                        bounds: bounds
                      ) else {
                    break
                }
                current = expanded
                rescuedLeft += 1
                changed = true
            }

            while rescuedRight < tonicBoundaryRescueMaxISI {
                let nextEnd = current.endISIIndex + 1
                guard tonicBoundaryRescueEdgeIsEligible(
                    index: nextEnd,
                    reference: current,
                    train: train,
                    settings: settings,
                    bounds: bounds
                ),
                      let expanded = rebuildTonicBoundaryRescueCandidate(
                        train: train,
                        parent: current,
                        original: candidate,
                        run: (current.startISIIndex, nextEnd),
                        leftRescued: rescuedLeft,
                        rightRescued: rescuedRight + 1,
                        settings: settings,
                        localContext: localContext,
                        bounds: bounds
                      ) else {
                    break
                }
                current = expanded
                rescuedRight += 1
                changed = true
            }

            return changed ? current : candidate
        }
    }

    private static let tonicBoundaryRescueMaxISI = 2
    private static let tonicBoundaryRescueUpperSlack = 0.05
    /// TONIC-BND-1: low-side "stable neighbor" slack. A rescued neighbor must be ≥ the candidate's own q10 × (1 − slack),
    /// i.e. CONSISTENT with the candidate's firing rate — not merely inside the wide tonic band. This is what prevents a
    /// faster pre-burst TRANSITION ISI (≈half the tonic baseline, but still ≥ the band lower) from being swallowed as tonic.
    private static let tonicBoundaryRescueNeighborSlack = 0.10

    private static func tonicBoundaryRescueEdgeIsEligible(
        index: Int,
        reference: ClassicAnchorCandidate,
        train: SpikeTrain,
        settings: StatePatternDetectorSettings,
        bounds: (lower: Double, upper: Double)
    ) -> Bool {
        guard train.isiSec.indices.contains(index),
              let value = finiteValidISI(train.isiSec[index], settings: settings) else {
            return false
        }
        // Never a burst-seed or fast-packet ISI (the burst-side guard).
        let fastPacketCoreUpper = fastPacketCoreUpperSec(settings: settings)
        guard value > settings.burstSeedUpperSec + tolerance(for: settings.burstSeedUpperSec),
              value > fastPacketCoreUpper + tolerance(for: fastPacketCoreUpper) else {
            return false
        }
        // High side: within the train-local tonic structural upper + a tight slack (admits a slightly-slow stable
        // neighbor; a pause-length ISI is far above this and is rejected).
        let highSideCeiling = bounds.upper * (1 + tonicBoundaryRescueUpperSlack)
        guard highSideCeiling.isFinite, highSideCeiling > 0,
              value <= highSideCeiling + tolerance(for: highSideCeiling) else {
            return false
        }
        // Low side: STABLE-NEIGHBOR, candidate-relative (NOT the wide band lower). The neighbor must be ≥ the candidate's
        // own q10 × (1 − slack), so a faster pre-burst transition ISI inside the band is not swallowed. Falls back to the
        // band lower only when the candidate carries no quantiles (should not happen for an accepted tonic candidate).
        let lowSide: Double = {
            if let q10 = reference.intraQ10Sec, q10.isFinite, q10 > 0 {
                return q10 * (1 - tonicBoundaryRescueNeighborSlack)
            }
            return bounds.lower
        }()
        return value >= lowSide - tolerance(for: lowSide)
    }

    private static func rebuildTonicBoundaryRescueCandidate(
        train: SpikeTrain,
        parent: ClassicAnchorCandidate,
        original: ClassicAnchorCandidate,
        run: (start: Int, end: Int),
        leftRescued: Int,
        rightRescued: Int,
        settings: StatePatternDetectorSettings,
        localContext: SpikeISILocalContextTable,
        bounds: (lower: Double, upper: Double)
    ) -> ClassicAnchorCandidate? {
        guard let metrics = metrics(train: train, run: run, settings: settings, localContext: localContext),
              metrics.nSpikes >= settings.tonicMinSpikes else {
            return nil
        }

        let strictFlags = metrics.values.map {
            $0 >= bounds.lower - tolerance(for: bounds.lower) &&
                $0 <= bounds.upper + tolerance(for: bounds.upper)
        }
        let bridgeCount = strictFlags.filter { !$0 }.count
        let bridgeFraction = Double(bridgeCount) / Double(max(1, metrics.nValidISI))
        let burstSeedFraction = fraction(metrics.values) {
            $0 <= settings.burstSeedUpperSec + tolerance(for: settings.burstSeedUpperSec)
        }
        let effectiveBurstSeedFractionMax = effectiveTonicBurstSeedFractionMax(settings: settings)
        let coreRunLength = maxBurstSeedRun(train: train, run: run, settings: settings)
        let coreRunLimit = tonicBurstCoreRunLimit(settings: settings)
        let fastPacketCoreUpper = fastPacketCoreUpperSec(settings: settings)
        let fastPacketFraction = fraction(metrics.values) {
            $0 <= fastPacketCoreUpper + tolerance(for: fastPacketCoreUpper)
        }
        let fastPacketOccupancyMax = fastPacketCoreOccupancyMax(settings: settings)
        let structuralPass =
            bridgeFraction <= settings.tonicBridgeFractionMax + 1e-12 &&
            burstSeedFraction <= effectiveBurstSeedFractionMax + 1e-12 &&
            coreRunLength <= coreRunLimit &&
            fastPacketFraction <= fastPacketOccupancyMax + 1e-12
        guard structuralPass else { return nil }

        let classicRegularityPass =
            (metrics.cv.map { $0 <= settings.tonicCVMax + 1e-12 } ?? true) &&
            (metrics.cv2.map { $0 <= settings.tonicCV2Max + 1e-12 } ?? true) &&
            (metrics.lv.map { $0 <= settings.tonicLVMax + 1e-12 } ?? true)
        let irregularRegularityPass =
            (metrics.cv.map { $0 <= settings.irregularTonicCVMax + 1e-12 } ?? true) &&
            (metrics.cv2.map { $0 <= settings.irregularTonicCV2Max + 1e-12 } ?? true) &&
            (metrics.lv.map { $0 <= settings.irregularTonicLVMax + 1e-12 } ?? true)
        guard classicRegularityPass || irregularRegularityPass else { return nil }
        let subtype = classicRegularityPass ? "classic" : "irregular"

        let regularityScore = mean([
            metrics.cv.map { 1 / (1 + $0) },
            metrics.cv2.map { 1 / (1 + $0) },
            metrics.lv.map { 1 / (1 + $0) }
        ].compactMap { $0 }) ?? 0
        let score = 3 + regularityScore + localStabilityScore(metrics) - 0.5 * bridgeFraction
        let decisionPath = stateDecisionPath(
            base: bridgeCount > 0 ? "stable_mid_isi_tonic_state_boundary_rescue_with_short_bridge" : "stable_mid_isi_tonic_state_boundary_rescue",
            metrics: metrics,
            extra: [
                "tonic_subtype=\(subtype)",
                subtype == "irregular" ? "irregular_tonic" : "classic_tonic",
                "tonic_boundary_rescue=true",
                "original_isi_span=\(original.startISIIndex)-\(original.endISIIndex)",
                "rescued_left_isi=\(leftRescued)",
                "rescued_right_isi=\(rightRescued)",
                "bridge_count=\(bridgeCount)",
                "bridge_fraction=\(format(bridgeFraction))",
                "structural_search_lower_sec=\(format(bounds.lower))",
                "structural_search_upper_sec=\(format(bounds.upper))",
                "boundary_rescue_upper_slack=\(format(tonicBoundaryRescueUpperSlack))",
                "candidate_seed_policy=structure_first_boundary_rescue_cv_cv2_lv_not_default_isi_band",
                "burst_seed_fraction=\(format(burstSeedFraction))",
                "burst_seed_fraction_max_effective=\(format(effectiveBurstSeedFractionMax))",
                "core_burst_run_length=\(coreRunLength)",
                "core_burst_run_limit=\(coreRunLimit)",
                "state_burst_packet_guard=pass",
                "fast_packet_core_upper_sec=\(format(fastPacketCoreUpper))",
                "fast_packet_fraction=\(format(fastPacketFraction))",
                "fast_packet_core_occupancy_max=\(format(fastPacketOccupancyMax))",
                "regularity_score=\(format(regularityScore))"
            ]
        )
        return candidate(
            train: train,
            run: run,
            metrics: metrics,
            label: .tonic,
            layer: "\(original.candidateLayer)_boundary_rescue",
            candidateClass: "\(original.candidateClass)_boundary_rescue",
            gateStatus: "event_core_tonic_boundary_rescue_pass",
            decisionPath: decisionPath,
            score: score,
            priority: parent.priority,
            bandLower: bounds.lower,
            bandUpper: bounds.upper,
            contrastMinRequired: settings.tonicLVMax,
            contrastGeomRequired: settings.tonicCV2Max,
            stateRegularityScore: regularityScore,
            stateBurstSeedFraction: burstSeedFraction,
            stateCoreBurstRunLength: coreRunLength,
            stateTonicSubtype: subtype,
            index: 0,
            id: original.id
        )
    }

    /// Secondary tonic seeds are generated from stable windows inside the
    /// train-local non-burst ISI distribution. This keeps tonic structure-driven:
    /// CV/CV2/LV remain the acceptance gates, and default/histogram bands do
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
                    let burstSeedFractionPass = burstSeedFraction <= effectiveBurstSeedFractionMax + 1e-12
                    let coreRunPass = coreRunLength <= coreRunLimit
                    let fastPacketPass = !fastPacketCoreExcludes(metrics, settings: settings)
                    guard cvPass, cv2Pass, lvPass, burstSeedFractionPass, coreRunPass, fastPacketPass else {
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
                            contrastGeomRequired: settings.tonicCV2Max,
                            stateRegularityScore: regularityScore,
                            stateBurstSeedFraction: burstSeedFraction,
                            stateCoreBurstRunLength: coreRunLength,
                            stateTonicSubtype: "classic",
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
            burstSeedFraction <= effectiveBurstSeedFractionMax + 1e-12 &&
            coreRunLength <= coreRunLimit &&
            !fastPacketCoreExcludes(metrics, settings: settings)
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
            contrastGeomRequired: settings.tonicCV2Max,
            stateRegularityScore: regularityScore,
            stateBurstSeedFraction: burstSeedFraction,
            stateCoreBurstRunLength: coreRunLength,
            stateTonicSubtype: "classic",
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
            !hasClassicBoundary(metrics: metrics, settings: settings) &&
            !fastPacketCoreExcludes(metrics, settings: settings)
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
            contrastGeomRequired: settings.highFrequencyTonicCV2Max,
            stateRegularityScore: regularityScore,
            stateBurstSeedFraction: burstSeedFraction,
            stateLowTailFraction: lowTail,
            stateCoreBurstRunLength: coreRunLength,
            stateTonicSubtype: "high_frequency",
            stateHighFrequencySubtype: "hf_tonic_spiking",
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
        rebuilt.stateHighFrequencySubtype = "hf_irregular_spiking"
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
        stateTonicSubtype: String? = nil,
        stateHighFrequencySubtype: String? = nil,
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
            preGapSec: metrics.preGapSec,
            postGapSec: metrics.postGapSec,
            preRatioQ90: nil,
            postRatioQ90: nil,
            edgeContrastMinQ90: metrics.lv,
            edgeContrastGeomQ90: nil,
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
        candidate.stateTonicSubtype = stateTonicSubtype
        candidate.stateHighFrequencySubtype = stateHighFrequencySubtype
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
            cv: STPDStatistics.coefficientOfVariation(values),
            cv2: STPDStatistics.coefficientOfVariation2(values),
            lv: STPDStatistics.localVariation(values),
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

    /// Adaptive fast structural core upper boundary. Tonic and HF tonic must live ABOVE
    /// this boundary; ISIs at or below it are intra-burst / HFS-like fast structure.
    ///
    /// It is the larger of the intra-burst seed core (`burstSeedUpperSec`) and the short
    /// (faster) sub-half of the high-frequency-spiking band (`highFrequencySpikingShortUpperSec`).
    /// The HFS short structure is resolved independently of the classic burst band, so it
    /// does not collapse toward `minValidISI` when no structural burst band is detected —
    /// this keeps the fast-packet veto robust against an underestimated burst seed upper,
    /// which is exactly the case that let stable short-ISI packets pass as tonic/HF tonic.
    /// Fully train/dataset adaptive: both inputs scale with the recording, so no fixed
    /// millisecond (e.g. 1-10 ms) range is assumed.
    private static func fastPacketCoreUpperSec(settings: StatePatternDetectorSettings) -> Double {
        max(settings.burstSeedUpperSec, 0.5 * settings.highFrequencySpikingShortUpperSec)
    }

    /// Maximum fraction of a candidate's ISIs allowed inside the adaptive fast structural
    /// core before tonic / HF tonic is rejected. A "most ISIs" majority threshold that
    /// tightens with structural burst support (mirroring the burst-seed-fraction veto), but
    /// keeps a majority floor so genuine states merely straddling the core are not
    /// over-rejected. Never relaxes the existing burst-seed-fraction protection: when the
    /// fast core equals the burst seed upper (the non-collapsed case), this looser threshold
    /// rejects only a subset of what the burst-seed veto already rejects.
    private static func fastPacketCoreOccupancyMax(settings: StatePatternDetectorSettings) -> Double {
        let support = min(1, max(0, settings.structuralBurstSupportWeight))
        return max(0.40, 0.50 * (1 - 0.20 * support))
    }

    /// True when a candidate run is dominated by ISIs inside the adaptive fast structural
    /// core and must therefore be excluded from tonic / HF tonic regardless of CV/CV2/LV.
    private static func fastPacketCoreExcludes(
        _ metrics: StateMetrics,
        settings: StatePatternDetectorSettings
    ) -> Bool {
        let coreUpper = fastPacketCoreUpperSec(settings: settings)
        let fraction = fraction(metrics.values) { $0 <= coreUpper + tolerance(for: coreUpper) }
        return fraction > fastPacketCoreOccupancyMax(settings: settings) + 1e-12
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
            irregularTonicCVMax: tuning.irregularTonicCVMax,
            irregularTonicCV2Max: tuning.irregularTonicCV2Max,
            irregularTonicLVMax: tuning.irregularTonicLVMax,
            tonicBurstSeedFractionMax: tuning.tonicBurstSeedFractionMax,
            highFrequencyTonicFloorSec: highFrequencyTonicFloor,
            highFrequencyTonicUpperSec: highFrequencyTonicUpper,
            highFrequencyTonicMinSpikes: tuning.highFrequencyTonicMinSpikes,
            highFrequencyTonicLowTailFractionMax: tuning.highFrequencyTonicLowTailFractionMax,
            highFrequencyTonicCVMax: tuning.highFrequencyTonicCVMax,
            highFrequencyTonicCV2Max: tuning.highFrequencyTonicCV2Max,
            highFrequencyTonicLVMax: tuning.highFrequencyTonicLVMax,
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
