import Foundation

public struct PauseDetectorSettings: Hashable, Sendable {
    public var isEnabled: Bool
    public var minValidISISec: Double
    public var seedThresholdSec: Double
    public var strongThresholdSec: Double
    public var adaptiveLowerSec: Double?
    public var adaptiveUpperSec: Double?
    /// User-confirmed absolute Pause floor. Unlike the adaptive lower bound, this also constrains
    /// contextual/inter-burst Pause evidence that is intentionally allowed below the ordinary
    /// train-adaptive Pause threshold.
    public var manualHardLowerSec: Double?
    public var alpha: Double
    public var beta: Double
    public var contextRelax: Double
    public var contextTight: Double
    public var localWindow: Int
    public var useGlobalMedianGuard: Bool
    public var globalMedianFactor: Double
    public var minDurationSec: Double
    public var minSpikes: Int
    public var eventCoreGapEnabled: Bool
    public var eventCoreLocalFactor: Double
    public var eventCoreGlobalFactor: Double
    public var tonicPauseGuardUpperSec: Double?
    public var antiTonicVeto: Bool
    public var tonicLowerSec: Double
    public var tonicUpperSec: Double
    public var tonicLVMax: Double
    public var structuralPauseSupportWeight: Double
    public var structuralPausePoolSupportWeight: Double
    public var classicBurstFlankPauseContrastMin: Double

    public init(
        isEnabled: Bool = true,
        minValidISISec: Double = 0.001,
        seedThresholdSec: Double = 0.100,
        strongThresholdSec: Double = 0.150,
        adaptiveLowerSec: Double? = nil,
        adaptiveUpperSec: Double? = nil,
        manualHardLowerSec: Double? = nil,
        alpha: Double = 2.2,
        beta: Double = 0.8,
        contextRelax: Double = 0.9,
        contextTight: Double = 1.1,
        localWindow: Int = 11,
        useGlobalMedianGuard: Bool = true,
        globalMedianFactor: Double = 2.5,
        minDurationSec: Double = 0,
        minSpikes: Int = 2,
        eventCoreGapEnabled: Bool = true,
        eventCoreLocalFactor: Double = 1.55,
        eventCoreGlobalFactor: Double = 1.25,
        tonicPauseGuardUpperSec: Double? = nil,
        antiTonicVeto: Bool = true,
        tonicLowerSec: Double = 0,
        tonicUpperSec: Double = 0,
        tonicLVMax: Double = 0.35,
        structuralPauseSupportWeight: Double = 0,
        structuralPausePoolSupportWeight: Double = 0,
        classicBurstFlankPauseContrastMin: Double = 5.0
    ) {
        self.isEnabled = isEnabled
        self.minValidISISec = minValidISISec.isFinite && minValidISISec > 0 ? minValidISISec : 0.001
        self.seedThresholdSec = seedThresholdSec.isFinite && seedThresholdSec > 0 ? seedThresholdSec : 0.100
        self.strongThresholdSec = strongThresholdSec.isFinite && strongThresholdSec > 0 ? strongThresholdSec : 0.150
        self.adaptiveLowerSec = adaptiveLowerSec.flatMap { $0.isFinite && $0 > 0 ? $0 : nil }
        self.adaptiveUpperSec = adaptiveUpperSec.flatMap { $0.isFinite && $0 > 0 ? $0 : nil }
        self.manualHardLowerSec = manualHardLowerSec.flatMap { $0.isFinite && $0 > 0 ? $0 : nil }
        self.alpha = alpha.isFinite && alpha > 0 ? alpha : 2.2
        self.beta = beta.isFinite && beta > 0 ? beta : 0.8
        self.contextRelax = contextRelax.isFinite && contextRelax > 0 ? contextRelax : 0.9
        self.contextTight = contextTight.isFinite && contextTight > 0 ? contextTight : 1.1
        self.localWindow = max(1, localWindow)
        self.useGlobalMedianGuard = useGlobalMedianGuard
        self.globalMedianFactor = globalMedianFactor.isFinite && globalMedianFactor > 0 ? globalMedianFactor : 2.5
        self.minDurationSec = max(0, minDurationSec.isFinite ? minDurationSec : 0)
        self.minSpikes = max(2, minSpikes)
        self.eventCoreGapEnabled = eventCoreGapEnabled
        self.eventCoreLocalFactor = eventCoreLocalFactor.isFinite && eventCoreLocalFactor >= 1 ? eventCoreLocalFactor : 1.55
        self.eventCoreGlobalFactor = eventCoreGlobalFactor.isFinite && eventCoreGlobalFactor >= 1 ? eventCoreGlobalFactor : 1.25
        self.tonicPauseGuardUpperSec = tonicPauseGuardUpperSec.flatMap { $0.isFinite && $0 > 0 ? $0 : nil }
        self.antiTonicVeto = antiTonicVeto
        self.tonicLowerSec = tonicLowerSec.isFinite && tonicLowerSec > 0 ? tonicLowerSec : self.minValidISISec
        let cleanedTonicUpper = tonicUpperSec.isFinite && tonicUpperSec > 0 ? tonicUpperSec : self.tonicLowerSec
        self.tonicUpperSec = max(self.tonicLowerSec, cleanedTonicUpper)
        self.tonicLVMax = tonicLVMax.isFinite && tonicLVMax > 0 ? tonicLVMax : 0.35
        self.structuralPauseSupportWeight = Self.cleanWeight(structuralPauseSupportWeight)
        self.structuralPausePoolSupportWeight = Self.cleanWeight(structuralPausePoolSupportWeight)
        self.classicBurstFlankPauseContrastMin = classicBurstFlankPauseContrastMin.isFinite && classicBurstFlankPauseContrastMin > 0
            ? classicBurstFlankPauseContrastMin
            : 5.0
    }

    public var seedBaselineSec: Double {
        if let adaptiveLowerSec {
            return max(adaptiveLowerSec, minValidISISec)
        }
        return max(seedThresholdSec, minValidISISec)
    }

    public var displayUpperSec: Double {
        max(adaptiveUpperSec ?? strongThresholdSec, strongThresholdSec, seedBaselineSec)
    }

    private static func cleanWeight(_ value: Double) -> Double {
        guard value.isFinite else {
            return 0
        }
        return min(1, max(0, value))
    }
}

public enum PauseDetector {
    public static func detect(
        train: SpikeTrain,
        settings: PauseDetectorSettings = PauseDetectorSettings()
    ) -> ClassicAnchorDetectionResult {
        ClassicAnchorDetectionResult(
            trainID: train.id,
            trainName: train.name,
            candidates: detectPauseCandidates(train: train, settings: settings)
        )
    }

    public static func detect(
        dataset: SpikeDataset,
        settings: PauseDetectorSettings = PauseDetectorSettings()
    ) -> [ClassicAnchorDetectionResult] {
        dataset.trains.map { detect(train: $0, settings: settings) }
    }

    private struct PausePoint {
        let index: Int
        let isiSec: Double
        let localMedianSec: Double
        let effectiveThresholdSec: Double
        let globalThresholdSec: Double?
        let localRatio: Double
        let trainPercentile: Double?
        let localPercentile: Double?
        let localRobustZ: Double?
        let isStrong: Bool
    }

    private static func detectPauseCandidates(
        train: SpikeTrain,
        settings: PauseDetectorSettings
    ) -> [ClassicAnchorCandidate] {
        guard settings.isEnabled, train.spikeCount >= 2 else {
            return []
        }

        let validGlobalValues = validISISec(train.isiSec, settings: settings)
        guard let globalMedian = median(validGlobalValues), globalMedian > 0 else {
            return []
        }
        let localContext = SpikeISILocalContextTable.build(
            for: train,
            settings: SpikeISILocalContextSettings(
                minValidISISec: settings.minValidISISec,
                halfWindow: settings.localWindow
            )
        )
        let globalGuardThreshold = settings.useGlobalMedianGuard ? settings.globalMedianFactor * globalMedian : nil

        var seedFlags = Array(repeating: false, count: train.isiSec.count)
        var pointsByIndex: [Int: PausePoint] = [:]

        for index in train.isiSec.indices where index > 0 {
            guard let isi = finiteValidISI(train.isiSec[index], settings: settings) else {
                continue
            }

            let context = localContext.point(for: index)
            let localMedian = context?.localMedianSec ?? localMedianISI(
                train.isiSec,
                centerIndex: index,
                window: settings.localWindow,
                excluding: [index],
                settings: settings
            ) ?? globalMedian
            guard localMedian.isFinite, localMedian > 0 else {
                continue
            }

            let hasLocalDistributionSupport =
                (context?.localPercentile ?? 0) >= 0.90 ||
                (context?.localRobustZ ?? -.infinity) >= 2.5
            let seedContextFactor = hasLocalDistributionSupport ? settings.contextRelax : settings.contextTight
            let localContextFactor = hasLocalDistributionSupport ? settings.contextRelax : 1
            let effectiveThreshold = max(
                settings.seedBaselineSec * seedContextFactor,
                settings.alpha * localContextFactor * localMedian,
                globalGuardThreshold ?? 0
            )
            let localRatio = context?.localRatio ?? isi / localMedian
            let strongThreshold = max(
                settings.strongThresholdSec,
                settings.alpha * localMedian,
                globalGuardThreshold ?? 0
            )

            pointsByIndex[index] = PausePoint(
                index: index,
                isiSec: isi,
                localMedianSec: localMedian,
                effectiveThresholdSec: effectiveThreshold,
                globalThresholdSec: globalGuardThreshold,
                localRatio: localRatio,
                trainPercentile: context?.trainPercentile,
                localPercentile: context?.localPercentile,
                localRobustZ: context?.localRobustZ,
                isStrong: isi >= strongThreshold - tolerance(for: strongThreshold)
            )

            guard isi >= effectiveThreshold - tolerance(for: effectiveThreshold) else {
                continue
            }

            seedFlags[index] = true
        }

        var candidates: [ClassicAnchorCandidate] = []
        for run in mergedPauseRuns(
            boolRuns(seedFlags),
            pointsByIndex: pointsByIndex,
            settings: settings
        ) {
            guard let candidate = candidate(
                train: train,
                run: run,
                pointsByIndex: pointsByIndex,
                settings: settings,
                candidateIndex: candidates.count + 1
            ) else {
                continue
            }
            candidates.append(candidate)
        }

        for candidate in eventCorePauseCandidates(
            train: train,
            settings: settings,
            validGlobalValues: validGlobalValues,
            globalMedian: globalMedian,
            localContext: localContext,
            existingCandidates: candidates
        ) {
            candidates.append(candidate)
        }

        return candidates
    }

    private static func eventCorePauseCandidates(
        train: SpikeTrain,
        settings: PauseDetectorSettings,
        validGlobalValues: [Double],
        globalMedian: Double,
        localContext: SpikeISILocalContextTable,
        existingCandidates: [ClassicAnchorCandidate]
    ) -> [ClassicAnchorCandidate] {
        guard settings.eventCoreGapEnabled,
              validGlobalValues.count >= 6,
              let q90 = quantile(validGlobalValues, probability: 0.90) else {
            return []
        }

        let structuralSupport = max(
            settings.structuralPauseSupportWeight,
            settings.structuralPausePoolSupportWeight
        )
        let q90Guard = structuralPauseQ90Guard(
            seedBaselineSec: settings.seedBaselineSec,
            trainQ90Sec: q90,
            structuralSupport: structuralSupport
        )
        let pauseFloorBase = max(settings.seedBaselineSec, q90Guard)
        let tonicPauseGuard = settings.tonicPauseGuardUpperSec.map { $0 * 1.15 }
        let pauseFloor = max(pauseFloorBase, tonicPauseGuard ?? pauseFloorBase)
        guard pauseFloor.isFinite, pauseFloor > 0 else {
            return []
        }

        var baseLong = Array(repeating: false, count: train.isiSec.count)
        for index in train.isiSec.indices where index > 0 {
            guard let isi = finiteValidISI(train.isiSec[index], settings: settings) else {
                continue
            }
            baseLong[index] = isi >= pauseFloor - tolerance(for: pauseFloor)
        }

        let baseLongIndices = Set(baseLong.indices.filter { baseLong[$0] })
        guard !baseLongIndices.isEmpty else {
            return []
        }

        var flag = Array(repeating: false, count: train.isiSec.count)
        var pointsByIndex: [Int: PausePoint] = [:]
        let globalThreshold = globalMedian * settings.eventCoreGlobalFactor

        for index in baseLongIndices.sorted() {
            guard let isi = finiteValidISI(train.isiSec[index], settings: settings) else {
                continue
            }
            let localMedian = localMedianISI(
                train.isiSec,
                centerIndex: index,
                window: settings.localWindow,
                excluding: baseLongIndices,
                settings: settings
            ) ?? localMedianISI(
                train.isiSec,
                centerIndex: index,
                window: settings.localWindow,
                excluding: [index],
                settings: settings
            )
            let localOK = localMedian.map { isi >= $0 * settings.eventCoreLocalFactor - tolerance(for: $0 * settings.eventCoreLocalFactor) } ?? true
            let globalOK = isi >= globalThreshold - tolerance(for: globalThreshold)
            guard localOK && globalOK else {
                continue
            }

            let local = localMedian ?? globalMedian
            let context = localContext.point(for: index)
            let strongThreshold = max(settings.strongThresholdSec, pauseFloor)
            pointsByIndex[index] = PausePoint(
                index: index,
                isiSec: isi,
                localMedianSec: local,
                effectiveThresholdSec: pauseFloor,
                globalThresholdSec: globalMedian,
                localRatio: local > 0 ? isi / local : .nan,
                trainPercentile: context?.trainPercentile,
                localPercentile: context?.localPercentile,
                localRobustZ: context?.localRobustZ,
                isStrong: isi >= strongThreshold - tolerance(for: strongThreshold)
            )
            flag[index] = true
        }

        var candidates: [ClassicAnchorCandidate] = []
        for run in boolRuns(flag) {
            guard !overlapsExistingPause(run, existingCandidates: existingCandidates),
                  let candidate = eventCoreCandidate(
                    train: train,
                    run: run,
                    pointsByIndex: pointsByIndex,
                    settings: settings,
                    pauseFloor: pauseFloor,
                    pauseFloorBase: pauseFloorBase,
                    q90Guard: q90Guard,
                    trainQ90: q90,
                    structuralSupport: structuralSupport,
                    tonicPauseGuard: tonicPauseGuard,
                    globalMedian: globalMedian,
                    candidateIndex: candidates.count + 1
                  ) else {
                continue
            }
            candidates.append(candidate)
        }
        return candidates
    }

    private static func overlapsExistingPause(
        _ run: (start: Int, end: Int),
        existingCandidates: [ClassicAnchorCandidate]
    ) -> Bool {
        existingCandidates.contains { candidate in
            guard candidate.finalLabel == .pause else {
                return false
            }
            return max(run.start, candidate.startISIIndex) <= min(run.end, candidate.endISIIndex)
        }
    }

    private static func structuralPauseQ90Guard(
        seedBaselineSec: Double,
        trainQ90Sec: Double,
        structuralSupport: Double
    ) -> Double {
        let baseline = seedBaselineSec.isFinite && seedBaselineSec > 0 ? seedBaselineSec : trainQ90Sec
        guard trainQ90Sec.isFinite,
              trainQ90Sec > baseline,
              baseline > 0 else {
            return max(baseline, trainQ90Sec)
        }

        let support = min(1, max(0, structuralSupport.isFinite ? structuralSupport : 0))
        guard support > 0 else {
            return trainQ90Sec
        }

        let q90Weight = 1 - min(0.70, 0.70 * support)
        return exp(q90Weight * log(trainQ90Sec) + (1 - q90Weight) * log(baseline))
    }

    private static func eventCoreCandidate(
        train: SpikeTrain,
        run: (start: Int, end: Int),
        pointsByIndex: [Int: PausePoint],
        settings: PauseDetectorSettings,
        pauseFloor: Double,
        pauseFloorBase: Double,
        q90Guard: Double,
        trainQ90: Double,
        structuralSupport: Double,
        tonicPauseGuard: Double?,
        globalMedian: Double,
        candidateIndex: Int
    ) -> ClassicAnchorCandidate? {
        let points = (run.start...run.end).compactMap { pointsByIndex[$0] }
        let values = points.map(\.isiSec)
        guard !values.isEmpty,
              train.timestampsSec.indices.contains(run.start - 1),
              train.timestampsSec.indices.contains(run.end) else {
            return nil
        }
        guard !antiTonicVeto(values, settings: settings) else {
            return nil
        }

        let duration = train.timestampsSec[run.end] - train.timestampsSec[run.start - 1]
        let spikeCount = run.end - run.start + 2
        let hasStrong = points.contains { $0.isStrong }
        let passesBasic = hasStrong ||
            duration >= settings.minDurationSec - tolerance(for: settings.minDurationSec) ||
            spikeCount >= settings.minSpikes
        guard passesBasic else {
            return nil
        }

        let sample = SortedFiniteSample(values)
        let q10 = sample.quantile(0.10)
        let q40 = sample.quantile(0.40)
        let q50 = sample.quantile(0.50)
        let q90 = sample.quantile(0.90)
        let q95 = sample.quantile(0.95)
        let meanValue = mean(values)
        let maxValue = values.max()
        let medianLocal = median(points.map(\.localMedianSec))
        let localRatio = ratio(maxValue, over: medianLocal)
        let globalRatio = ratio(maxValue, over: globalMedian)
        let maxTrainPercentile = points.compactMap(\.trainPercentile).max()
        let maxLocalPercentile = points.compactMap(\.localPercentile).max()
        let maxLocalRobustZ = points.compactMap(\.localRobustZ).max()
        let score = ratio(maxValue, over: pauseFloor) ?? 1
        let gateStatus = tonicPauseGuard == nil
            ? "event_core_pause_pass"
            : "event_core_pause_pass_tonic_guarded"
        let pauseBoundaryRole: PauseBoundaryRole = hasStrong
            ? .canonicalPauseAnchor
            : .briefStateInterruption
        let decisionPath = [
            "relative_long_isi_gap_layer",
            "pause_boundary_role=\(pauseBoundaryRole.rawValue)",
            "pause_floor=\(format(pauseFloor))",
            "pause_floor_base=\(format(pauseFloorBase))",
            "train_q90_guard_raw=\(format(trainQ90))",
            "train_q90_guard_effective=\(format(q90Guard))",
            "structural_pause_support_weight=\(format(settings.structuralPauseSupportWeight))",
            "structural_pause_pool_support_weight=\(format(settings.structuralPausePoolSupportWeight))",
            "structural_pause_q90_guard_relaxation=\(format(structuralSupport))",
            "tonic_pause_guard=\(format(tonicPauseGuard))",
            "local_ratio=\(format(localRatio))",
            "global_ratio=\(format(globalRatio))",
            "train_percentile_max=\(format(maxTrainPercentile))",
            "local_percentile_max=\(format(maxLocalPercentile))",
            "local_robust_z_max=\(format(maxLocalRobustZ))"
        ].joined(separator: ";")

        return ClassicAnchorCandidate(
            id: "\(train.id)-event-core-pause-gap-\(candidateIndex)",
            trainID: train.id,
            trainName: train.name,
            candidateLayer: "event_core_pause_gap",
            candidateClass: "event_core_pause_gap",
            finalLabel: .pause,
            gateStatus: gateStatus,
            decisionPath: decisionPath,
            action: "accept",
            score: score,
            priority: hasStrong ? 925 : 890,
            selectedForAuto: false,
            selectionStatus: "not_selected",
            startISIIndex: run.start,
            endISIIndex: run.end,
            startSpikeIndex: run.start,
            endSpikeIndex: run.end + 1,
            nISI: run.end - run.start + 1,
            nValidISI: values.count,
            nSpikes: spikeCount,
            durationSec: duration.isFinite ? duration : nil,
            intraQ10Sec: q10,
            intraQ40Sec: q40,
            intraQ50Sec: q50,
            intraQ90Sec: q90,
            intraQ95Sec: q95,
            maxIntraISISec: maxValue,
            meanIntraISISec: meanValue,
            cv: STPDStatistics.coefficientOfVariation(values),
            lv: STPDStatistics.localVariation(values),
            preGapSec: finiteValidISI(run.start > 1 ? train.isiSec[run.start - 1] : nil, settings: settings),
            postGapSec: finiteValidISI(run.end < train.isiSec.count - 1 ? train.isiSec[run.end + 1] : nil, settings: settings),
            preRatioQ90: nil,
            postRatioQ90: nil,
            edgeContrastMinQ90: localRatio,
            edgeContrastGeomQ90: globalRatio,
            anchorFamily: "pause",
            anchorLockLevel: hasStrong ? .lockedClassic : .strongCandidate,
            anchorBandLowerSec: pauseFloor,
            anchorBandUpperSec: max(settings.displayUpperSec, pauseFloorBase, tonicPauseGuard ?? 0),
            anchorBandSource: .structure,
            anchorContrastMinRequired: settings.eventCoreLocalFactor,
            anchorContrastGeomRequired: settings.eventCoreGlobalFactor,
            refractorySuspectCount: 0,
            refractorySuspectAction: nil,
            pauseBoundaryRole: pauseBoundaryRole
        )
    }

    private static func candidate(
        train: SpikeTrain,
        run: (start: Int, end: Int),
        pointsByIndex: [Int: PausePoint],
        settings: PauseDetectorSettings,
        candidateIndex: Int
    ) -> ClassicAnchorCandidate? {
        let points = (run.start...run.end).compactMap { pointsByIndex[$0] }
        let values = points.map(\.isiSec)
        guard !values.isEmpty,
              train.timestampsSec.indices.contains(run.start - 1),
              train.timestampsSec.indices.contains(run.end) else {
            return nil
        }
        guard !antiTonicVeto(values, settings: settings) else {
            return nil
        }

        let duration = train.timestampsSec[run.end] - train.timestampsSec[run.start - 1]
        let spikeCount = run.end - run.start + 2
        let hasStrong = points.contains { $0.isStrong }
        let passesBasic = hasStrong ||
            duration >= settings.minDurationSec - tolerance(for: settings.minDurationSec) ||
            spikeCount >= settings.minSpikes
        guard passesBasic else {
            return nil
        }

        let sample = SortedFiniteSample(values)
        let q10 = sample.quantile(0.10)
        let q40 = sample.quantile(0.40)
        let q50 = sample.quantile(0.50)
        let q90 = sample.quantile(0.90)
        let q95 = sample.quantile(0.95)
        let meanValue = mean(values)
        let maxValue = values.max()
        let medianLocal = median(points.map(\.localMedianSec))
        let medianThreshold = median(points.map(\.effectiveThresholdSec)) ?? settings.seedBaselineSec
        let localRatio = ratio(maxValue, over: medianLocal)
        let globalRatio = ratio(maxValue, over: points.first?.globalThresholdSec)
        let maxTrainPercentile = points.compactMap(\.trainPercentile).max()
        let maxLocalPercentile = points.compactMap(\.localPercentile).max()
        let maxLocalRobustZ = points.compactMap(\.localRobustZ).max()
        let score = log1p(duration) +
            0.70 * log(max(localRatio ?? 1, 1)) +
            0.30 * log(max(globalRatio ?? 1, 1)) +
            (hasStrong ? 0.25 : 0)
        let pauseBoundaryRole: PauseBoundaryRole = hasStrong
            ? .canonicalPauseAnchor
            : .briefStateInterruption
        let decisionPath = [
            "pause_long_isi_exceeds_local_and_global_baseline_thresholds",
            "pause_boundary_role=\(pauseBoundaryRole.rawValue)",
            "threshold_median=\(format(medianThreshold))",
            "local_ratio=\(format(localRatio))",
            "global_ratio=\(format(globalRatio))",
            "train_percentile_max=\(format(maxTrainPercentile))",
            "local_percentile_max=\(format(maxLocalPercentile))",
            "local_robust_z_max=\(format(maxLocalRobustZ))",
            "context_relax=\(format(settings.contextRelax))",
            "context_tight=\(format(settings.contextTight))"
        ].joined(separator: ";")

        return ClassicAnchorCandidate(
            id: "\(train.id)-classic-anchor-pause-\(candidateIndex)",
            trainID: train.id,
            trainName: train.name,
            candidateLayer: "pause_detector",
            candidateClass: "long_isi_pause",
            finalLabel: .pause,
            gateStatus: hasStrong ? "pause_strong_long_isi_pass" : "pause_seed_long_isi_pass",
            decisionPath: decisionPath,
            action: "accept",
            score: score,
            priority: hasStrong ? 930 : 880,
            selectedForAuto: false,
            selectionStatus: "not_selected",
            startISIIndex: run.start,
            endISIIndex: run.end,
            startSpikeIndex: run.start,
            endSpikeIndex: run.end + 1,
            nISI: run.end - run.start + 1,
            nValidISI: values.count,
            nSpikes: spikeCount,
            durationSec: duration.isFinite ? duration : nil,
            intraQ10Sec: q10,
            intraQ40Sec: q40,
            intraQ50Sec: q50,
            intraQ90Sec: q90,
            intraQ95Sec: q95,
            maxIntraISISec: maxValue,
            meanIntraISISec: meanValue,
            cv: STPDStatistics.coefficientOfVariation(values),
            lv: STPDStatistics.localVariation(values),
            preGapSec: finiteValidISI(run.start > 1 ? train.isiSec[run.start - 1] : nil, settings: settings),
            postGapSec: finiteValidISI(run.end < train.isiSec.count - 1 ? train.isiSec[run.end + 1] : nil, settings: settings),
            preRatioQ90: nil,
            postRatioQ90: nil,
            edgeContrastMinQ90: localRatio,
            edgeContrastGeomQ90: globalRatio,
            anchorFamily: "pause",
            anchorLockLevel: hasStrong ? .lockedClassic : .strongCandidate,
            anchorBandLowerSec: medianThreshold,
            anchorBandUpperSec: settings.displayUpperSec,
            anchorBandSource: .structure,
            anchorContrastMinRequired: settings.alpha,
            anchorContrastGeomRequired: settings.globalMedianFactor,
            refractorySuspectCount: 0,
            refractorySuspectAction: nil,
            pauseBoundaryRole: pauseBoundaryRole
        )
    }

    private static func validISISec(
        _ values: [Double?],
        settings: PauseDetectorSettings
    ) -> [Double] {
        values.enumerated().compactMap { index, value in
            guard index > 0 else {
                return nil
            }
            return finiteValidISI(value, settings: settings)
        }
    }

    private static func finiteValidISI(_ value: Double?, settings: PauseDetectorSettings) -> Double? {
        guard let value,
              value.isFinite,
              value >= settings.minValidISISec - tolerance(for: settings.minValidISISec) else {
            return nil
        }
        return value
    }

    private static func localMedianISI(
        _ values: [Double?],
        centerIndex: Int,
        window: Int,
        excluding excludedIndices: Set<Int>,
        settings: PauseDetectorSettings
    ) -> Double? {
        guard !values.isEmpty else {
            return nil
        }
        let lower = max(1, centerIndex - window)
        let upper = min(values.count - 1, centerIndex + window)
        guard lower <= upper else {
            return nil
        }

        let localValues = (lower...upper).compactMap { index -> Double? in
            guard !excludedIndices.contains(index) else {
                return nil
            }
            return finiteValidISI(values[index], settings: settings)
        }
        return median(localValues)
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

    private static func mergedPauseRuns(
        _ runs: [(start: Int, end: Int)],
        pointsByIndex: [Int: PausePoint],
        settings: PauseDetectorSettings
    ) -> [(start: Int, end: Int)] {
        guard runs.count >= 2 else {
            return runs
        }

        var merged: [(start: Int, end: Int)] = []
        var current = runs[0]

        for next in runs.dropFirst() {
            if canBridgePauseGap(
                current: current,
                next: next,
                pointsByIndex: pointsByIndex,
                settings: settings
            ) {
                current = (current.start, next.end)
            } else {
                merged.append(current)
                current = next
            }
        }

        merged.append(current)
        return merged
    }

    private static func canBridgePauseGap(
        current: (start: Int, end: Int),
        next: (start: Int, end: Int),
        pointsByIndex: [Int: PausePoint],
        settings: PauseDetectorSettings
    ) -> Bool {
        let gapStart = current.end + 1
        let gapEnd = next.start - 1
        guard gapStart == gapEnd,
              let gap = pointsByIndex[gapStart],
              gap.localMedianSec > 0 else {
            return false
        }

        let relativeThreshold = settings.beta * gap.localMedianSec
        guard gap.isiSec >= relativeThreshold - tolerance(for: relativeThreshold) else {
            return false
        }
        if let globalThreshold = gap.globalThresholdSec,
           gap.isiSec < globalThreshold - tolerance(for: globalThreshold) {
            return false
        }

        let mergedValues = (current.start...next.end).compactMap { pointsByIndex[$0]?.isiSec }
        let mergedDuration = mergedValues.reduce(0, +)
        return mergedDuration >= settings.minDurationSec - tolerance(for: settings.minDurationSec)
    }

    private static func median(_ values: [Double]) -> Double? {
        quantile(values, probability: 0.50)
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

    private static func mean(_ values: [Double]) -> Double? {
        let finite = values.filter(\.isFinite)
        guard !finite.isEmpty else {
            return nil
        }
        return finite.reduce(0, +) / Double(finite.count)
    }

    private static func ratio(_ numerator: Double?, over denominator: Double?) -> Double? {
        guard let numerator,
              let denominator,
              numerator.isFinite,
              denominator.isFinite,
              denominator > 0 else {
            return nil
        }
        return numerator / denominator
    }

    private static func format(_ value: Double?) -> String {
        guard let value, value.isFinite else {
            return "NA"
        }
        return String(format: "%.12g", locale: Locale(identifier: "en_US_POSIX"), value)
    }


    private static func antiTonicVeto(_ values: [Double], settings: PauseDetectorSettings) -> Bool {
        guard settings.antiTonicVeto,
              values.count >= 2,
              let lv = STPDStatistics.localVariation(values),
              let meanValue = mean(values) else {
            return false
        }
        return lv <= settings.tonicLVMax + 1e-12 &&
            meanValue >= settings.tonicLowerSec - tolerance(for: settings.tonicLowerSec) &&
            meanValue <= settings.tonicUpperSec + tolerance(for: settings.tonicUpperSec)
    }

    private static func tolerance(for value: Double) -> Double {
        max(1e-12, abs(value) * 1e-6)
    }
}

public extension PauseDetectorSettings {
    init(
        adaptiveResolution resolution: TrainAdaptiveBandResolution,
        qualitySettings: SpikeQualitySettings = SpikeQualitySettings(),
        pauseMinISISecOverride: Double? = nil,
        classicBurstFlankPauseContrastMin: Double = 5.0
    ) {
        let pause = resolution.band(for: .pause).flatMap { band -> AdaptiveBand? in
            band.primarySource == .structure ? band : nil
        }
        let tonic = resolution.band(for: .tonic).flatMap { band -> AdaptiveBand? in
            band.primarySource == .structure ? band : nil
        }
        let overrideLower = pauseMinISISecOverride.flatMap { value in
            value.isFinite && value > 0 ? value : nil
        }
        self.init(
            minValidISISec: max(resolution.minValidISISec, qualitySettings.artifactThresholdSec),
            adaptiveLowerSec: overrideLower ?? pause?.seedLowerSec,
            adaptiveUpperSec: Self.maxPositive(overrideLower, pause?.seedUpperSec),
            tonicPauseGuardUpperSec: tonic?.seedUpperSec,
            tonicLowerSec: tonic?.seedLowerSec ?? max(resolution.minValidISISec, qualitySettings.artifactThresholdSec),
            tonicUpperSec: tonic?.seedUpperSec ?? max(resolution.minValidISISec, qualitySettings.artifactThresholdSec),
            structuralPauseSupportWeight: resolution.structuralSeedSummary.pauseSupportWeight,
            structuralPausePoolSupportWeight: resolution.structuralSeedSummary.pausePoolSupportWeight,
            classicBurstFlankPauseContrastMin: classicBurstFlankPauseContrastMin
        )
    }

    private static func maxPositive(_ lhs: Double?, _ rhs: Double?) -> Double? {
        let values = [lhs, rhs]
            .compactMap { $0 }
            .filter { $0.isFinite && $0 > 0 }
        return values.max()
    }
}
