import Foundation

public struct ClassicAnchorDecodedTrackSet: Sendable {
    public let eventCandidates: [ClassicAnchorCandidate]
    public let stateCandidates: [ClassicAnchorCandidate]
    public let gapCandidates: [ClassicAnchorCandidate]
    public let reviewCandidates: [ClassicAnchorCandidate]
    public let diagnosticCandidates: [ClassicAnchorCandidate]
    public let profileCandidates: [ClassicAnchorCandidate]

    public init(candidates: [ClassicAnchorCandidate], selectedOnly: Bool = true) {
        let pool = selectedOnly ? candidates.filter(\.selectedForAuto) : candidates
        self.eventCandidates = pool.filter { $0.auditRecommendedTrack == .event }
        self.stateCandidates = pool.filter { $0.auditRecommendedTrack == .state }
        self.gapCandidates = pool.filter { $0.auditRecommendedTrack == .gap }
        self.reviewCandidates = pool.filter { $0.auditRecommendedTrack == .review }
        self.diagnosticCandidates = pool.filter { $0.auditRecommendedTrack == .diagnostic }
        self.profileCandidates = pool.filter { $0.auditRecommendedTrack == .profile }
    }

    public var selectedCount: Int {
        eventCandidates.count +
            stateCandidates.count +
            gapCandidates.count +
            reviewCandidates.count +
            diagnosticCandidates.count +
            profileCandidates.count
    }
}

public struct ClassicAnchorDatasetRerunProvenance: Hashable, Sendable {
    public let stagePath: [String]
    public let trainCount: Int
    public let initialDatasetSummary: StructuralDatasetSeedSummary
    public let finalDatasetSummary: StructuralDatasetSeedSummary
    /// True when at least one train received a usable (anchored) leave-one-out bridge
    /// dataset summary, i.e. bridge expansion actually consumed a dataset structural prior
    /// for some train. Bridge expansion consumes a per-train leave-one-out summary; the
    /// full self-inclusive summary is retained for audit/display only via
    /// `initialDatasetSummary`. Provenance only; not read by detection logic.
    public let bridgeExpansionUsedDatasetSummary: Bool
    public let datasetSummaryAppliedToResolutions: Bool
    public let rerunExecuted: Bool
    public let rerunTrainCount: Int
    public let source: String
    /// True when the dataset structural seed summary applied to per-train resolutions
    /// was computed over a set that included those trains themselves (self-inclusion).
    /// Provenance only; not read by detection logic.
    public let datasetSummaryIncludedTargetTrain: Bool
    /// True when the per-train dataset structural prior is applied leave-one-out: each
    /// train's applied prior is aggregated only from other trains' train-local evidence,
    /// never its own. Provenance only; not read by detection logic.
    public let datasetPriorAppliedLeaveOneOut: Bool
    /// True when bridge expansion applies the dataset structural prior leave-one-out:
    /// each train's bridge prior is aggregated only from other trains' train-local
    /// evidence, never its own (every per-train bridge summary is non-self-inclusive).
    /// Provenance only; not read by detection logic.
    public let bridgeExpansionPriorAppliedLeaveOneOut: Bool
    /// True when any per-train bridge expansion summary was self-inclusive, i.e. included
    /// the target train's own evidence. Expected false after the leave-one-out fix.
    /// Provenance only; not read by detection logic.
    public let bridgeExpansionSummaryIncludedTargetTrain: Bool

    public init(
        stagePath: [String] = [],
        trainCount: Int = 0,
        initialDatasetSummary: StructuralDatasetSeedSummary = .empty,
        finalDatasetSummary: StructuralDatasetSeedSummary = .empty,
        bridgeExpansionUsedDatasetSummary: Bool = false,
        datasetSummaryAppliedToResolutions: Bool = false,
        rerunExecuted: Bool = false,
        rerunTrainCount: Int = 0,
        source: String = "none",
        datasetSummaryIncludedTargetTrain: Bool = false,
        datasetPriorAppliedLeaveOneOut: Bool = false,
        bridgeExpansionPriorAppliedLeaveOneOut: Bool = false,
        bridgeExpansionSummaryIncludedTargetTrain: Bool = false
    ) {
        self.stagePath = stagePath
        self.trainCount = max(0, trainCount)
        self.initialDatasetSummary = initialDatasetSummary
        self.finalDatasetSummary = finalDatasetSummary
        self.bridgeExpansionUsedDatasetSummary = bridgeExpansionUsedDatasetSummary
        self.datasetSummaryAppliedToResolutions = datasetSummaryAppliedToResolutions
        self.rerunExecuted = rerunExecuted
        self.rerunTrainCount = max(0, rerunTrainCount)
        self.source = source
        self.datasetSummaryIncludedTargetTrain = datasetSummaryIncludedTargetTrain
        self.datasetPriorAppliedLeaveOneOut = datasetPriorAppliedLeaveOneOut
        self.bridgeExpansionPriorAppliedLeaveOneOut = bridgeExpansionPriorAppliedLeaveOneOut
        self.bridgeExpansionSummaryIncludedTargetTrain = bridgeExpansionSummaryIncludedTargetTrain
    }

    public static let empty = ClassicAnchorDatasetRerunProvenance()
}

/// Selected tonic-family counts by subtype for UI/summary surfaces.
public struct TonicSubtypeCounts: Hashable, Sendable {
    public let classic: Int
    public let irregular: Int
    public let highFrequency: Int

    public init(classic: Int, irregular: Int, highFrequency: Int) {
        self.classic = max(0, classic)
        self.irregular = max(0, irregular)
        self.highFrequency = max(0, highFrequency)
    }

    public var total: Int { classic + irregular + highFrequency }

    /// Concise, scanner-friendly status suffix, e.g. `Tonic: classic 4, irregular 9, HF 2`.
    /// Empty when there are no selected tonic-family candidates.
    public var statusSummary: String {
        guard total > 0 else { return "" }
        return "Tonic: classic \(classic), irregular \(irregular), HF \(highFrequency)"
    }
}

public struct ClassicAnchorDetectionRun: Sendable {
    public let runIdentity: DetectionRunIdentity
    public let bandSettings: TrainAdaptiveBandSettings
    public let qualitySettings: SpikeQualitySettings
    public let resolutions: [TrainAdaptiveBandResolution]
    public let results: [ClassicAnchorDetectionResult]
    public let datasetStructuralSeedSummary: StructuralDatasetSeedSummary
    public let datasetRerunProvenance: ClassicAnchorDatasetRerunProvenance
    public let performanceReport: ClassicAnchorDetectionPerformanceReport?
    /// Phase D2-wire: the dataset's distribution-first ISI models, computed once at the detector/band
    /// floor (`bandSettings.minValidISISec`) before band resolution. Purely diagnostic — no detector
    /// reads it yet. D3 will derive burst/tonic/pause `ModeISIInterval`s from it. Optional (nil for
    /// callers that construct a run without the pipeline), mirroring `performanceReport`.
    public let datasetISIDistribution: DatasetISIDistribution?

    public init(
        bandSettings: TrainAdaptiveBandSettings,
        qualitySettings: SpikeQualitySettings,
        resolutions: [TrainAdaptiveBandResolution],
        results: [ClassicAnchorDetectionResult],
        datasetStructuralSeedSummary: StructuralDatasetSeedSummary = .empty,
        datasetRerunProvenance: ClassicAnchorDatasetRerunProvenance = .empty,
        performanceReport: ClassicAnchorDetectionPerformanceReport? = nil,
        datasetISIDistribution: DatasetISIDistribution? = nil,
        runIdentity: DetectionRunIdentity = .legacyUnidentified
    ) {
        self.runIdentity = runIdentity
        self.bandSettings = bandSettings
        self.qualitySettings = qualitySettings
        self.resolutions = resolutions
        self.results = results
        self.datasetStructuralSeedSummary = datasetStructuralSeedSummary
        self.datasetRerunProvenance = datasetRerunProvenance
        self.performanceReport = performanceReport
        self.datasetISIDistribution = datasetISIDistribution
    }

    public var candidates: [ClassicAnchorCandidate] {
        results.flatMap(\.candidates)
    }

    public var hfsBurstArbitrationAuditRows: [HFSBurstArbitrationAuditRow] {
        results.flatMap(\.hfsBurstArbitrationAuditRows)
    }

    public var candidateCount: Int {
        candidates.count
    }

    public var lockedClassicCount: Int {
        candidates.filter { $0.anchorLockLevel == .lockedClassic }.count
    }

    public var strongCandidateCount: Int {
        candidates.filter { $0.anchorLockLevel == .strongCandidate }.count
    }

    public var burstCount: Int {
        candidates.filter { $0.finalLabel == .burst }.count
    }

    public var longBurstCount: Int {
        candidates.filter { $0.finalLabel == .longBurst }.count
    }

    public var highFrequencyBurstCount: Int {
        candidates.filter { $0.finalLabel == .highFrequencyBurst }.count
    }

    public var possibleBurstCount: Int {
        candidates.filter { $0.finalLabel == .possibleBurst }.count
    }

    public var pauseCount: Int {
        candidates.filter { $0.finalLabel == .pause }.count
    }

    public var tonicCount: Int {
        candidates.filter { $0.finalLabel == .tonic }.count
    }

    public var highFrequencyTonicCount: Int {
        candidates.filter { $0.finalLabel == .highFrequencyTonic }.count
    }

    public var highFrequencySpikingCount: Int {
        candidates.filter { $0.finalLabel == .highFrequencySpiking }.count
    }

    public var selectedAutoCount: Int {
        candidates.filter(\.selectedForAuto).count
    }

    /// Selected tonic-family counts by subtype, for UI/summary surfaces. `.tonic` candidates
    /// are bucketed by `stateTonicSubtype` (irregular vs classic; a `.tonic` without a subtype
    /// is treated as classic); `.highFrequencyTonic` is counted as `highFrequency`.
    public var selectedTonicSubtypeCounts: TonicSubtypeCounts {
        var classic = 0
        var irregular = 0
        var highFrequency = 0
        for candidate in candidates where candidate.selectedForAuto {
            switch candidate.finalLabel {
            case .tonic:
                if candidate.stateTonicSubtype == "irregular" {
                    irregular += 1
                } else {
                    classic += 1
                }
            case .highFrequencyTonic:
                highFrequency += 1
            default:
                break
            }
        }
        return TonicSubtypeCounts(classic: classic, irregular: irregular, highFrequency: highFrequency)
    }

    public var selectedTracks: ClassicAnchorDecodedTrackSet {
        ClassicAnchorDecodedTrackSet(candidates: candidates)
    }

    public var selectedEventCount: Int {
        selectedTracks.eventCandidates.count
    }

    public var selectedStateCount: Int {
        selectedTracks.stateCandidates.count
    }

    public var selectedGapCount: Int {
        selectedTracks.gapCandidates.count
    }

    public var selectedReviewCount: Int {
        selectedTracks.reviewCandidates.count
    }

    public var selectedDiagnosticCount: Int {
        selectedTracks.diagnosticCandidates.count
    }

    public func resolution(for trainID: String) -> TrainAdaptiveBandResolution? {
        resolutions.first { $0.trainID == trainID }
    }

    public func result(for trainID: String) -> ClassicAnchorDetectionResult? {
        results.first { $0.trainID == trainID }
    }
}

public enum ClassicAnchorDetectionPipeline {
    private final class PerformanceRecorder: @unchecked Sendable {
        private let lock = NSLock()
        private let runStartNanos = DispatchTime.now().uptimeNanoseconds
        private var wallStageNanos: [String: UInt64] = [:]
        private var wallStageOrder: [String] = []
        private var trainStageNanos: [String: [String: UInt64]] = [:]
        private var trainStageOrder: [String: [String]] = [:]
        private var trainNamesByID: [String: String] = [:]
        private var spikeCountsByTrainID: [String: Int] = [:]
        private var resultCountsByTrainID: [String: (candidates: Int, selectedAuto: Int, hfsAuditRows: Int)] = [:]
        private let trainOrder: [String]
        private let trainCount: Int
        private let spikeCount: Int

        init(dataset: SpikeDataset) {
            self.trainOrder = dataset.trains.map(\.id)
            self.trainCount = dataset.trains.count
            self.spikeCount = dataset.totalSpikeCount
            for train in dataset.trains {
                trainNamesByID[train.id] = train.name
                spikeCountsByTrainID[train.id] = train.spikeCount
            }
        }

        func measureWallStage<T>(_ name: String, _ work: () -> T) -> T {
            let start = DispatchTime.now().uptimeNanoseconds
            let value = work()
            recordWallStage(name, elapsedNanos: elapsedNanos(since: start))
            return value
        }

        func recordWallStage(_ name: String) {
            recordWallStage(name, elapsedNanos: 0)
        }

        func measureTrainStage<T>(_ name: String, train: SpikeTrain, _ work: () -> T) -> T {
            recordTrainMetadata(train)
            let start = DispatchTime.now().uptimeNanoseconds
            let value = work()
            recordTrainStage(name, trainID: train.id, elapsedNanos: elapsedNanos(since: start))
            return value
        }

        func recordTrainResult(_ result: ClassicAnchorDetectionResult) {
            lock.lock()
            resultCountsByTrainID[result.trainID] = (
                candidates: result.candidates.count,
                selectedAuto: result.candidates.filter(\.selectedForAuto).count,
                hfsAuditRows: result.hfsBurstArbitrationAuditRows.count
            )
            trainNamesByID[result.trainID] = result.trainName
            lock.unlock()
        }

        func makeReport(results: [ClassicAnchorDetectionResult]) -> ClassicAnchorDetectionPerformanceReport {
            for result in results {
                recordTrainResult(result)
            }

            lock.lock()
            let totalWallTimeMs = nanosToMilliseconds(elapsedNanos(since: runStartNanos))
            let stageTimings = wallStageOrder.compactMap { stage -> ClassicAnchorPerformanceStageTiming? in
                guard let nanos = wallStageNanos[stage] else {
                    return nil
                }
                return ClassicAnchorPerformanceStageTiming(
                    name: stage,
                    wallTimeMs: nanosToMilliseconds(nanos)
                )
            }
            let trainSummaries = trainOrder.map { trainID in
                let stageOrder = trainStageOrder[trainID] ?? []
                let timings = stageOrder.compactMap { stage -> ClassicAnchorPerformanceStageTiming? in
                    guard let nanos = trainStageNanos[trainID]?[stage] else {
                        return nil
                    }
                    return ClassicAnchorPerformanceStageTiming(
                        name: stage,
                        wallTimeMs: nanosToMilliseconds(nanos)
                    )
                }
                let resultCounts = resultCountsByTrainID[trainID] ?? (0, 0, 0)
                return ClassicAnchorTrainPerformanceSummary(
                    trainID: trainID,
                    trainName: trainNamesByID[trainID] ?? trainID,
                    spikeCount: spikeCountsByTrainID[trainID] ?? 0,
                    candidateCount: resultCounts.candidates,
                    selectedAutoCount: resultCounts.selectedAuto,
                    hfsBurstAuditRowCount: resultCounts.hfsAuditRows,
                    stageTimings: timings
                )
            }
            lock.unlock()

            return ClassicAnchorDetectionPerformanceReport(
                totalWallTimeMs: totalWallTimeMs,
                trainCount: trainCount,
                spikeCount: spikeCount,
                stageTimings: stageTimings,
                trainSummaries: trainSummaries
            )
        }

        private func recordTrainMetadata(_ train: SpikeTrain) {
            lock.lock()
            trainNamesByID[train.id] = train.name
            spikeCountsByTrainID[train.id] = train.spikeCount
            lock.unlock()
        }

        private func recordWallStage(_ name: String, elapsedNanos: UInt64) {
            lock.lock()
            if wallStageNanos[name] == nil {
                wallStageOrder.append(name)
            }
            wallStageNanos[name, default: 0] += elapsedNanos
            lock.unlock()
        }

        private func recordTrainStage(_ name: String, trainID: String, elapsedNanos: UInt64) {
            lock.lock()
            var order = trainStageOrder[trainID] ?? []
            if trainStageNanos[trainID]?[name] == nil {
                order.append(name)
            }
            trainStageOrder[trainID] = order
            var stages = trainStageNanos[trainID] ?? [:]
            stages[name, default: 0] += elapsedNanos
            trainStageNanos[trainID] = stages
            lock.unlock()
        }

        private func elapsedNanos(since start: UInt64) -> UInt64 {
            let now = DispatchTime.now().uptimeNanoseconds
            return now >= start ? now - start : 0
        }

        private func nanosToMilliseconds(_ nanos: UInt64) -> Double {
            Double(nanos) / 1_000_000
        }
    }

    public static func run(
        dataset: SpikeDataset,
        bandSettings: TrainAdaptiveBandSettings = TrainAdaptiveBandSettings(),
        qualitySettings: SpikeQualitySettings = SpikeQualitySettings(),
        refractoryAction: ClassicAnchorRefractoryAction = .warnOnly,
        stateTuning: StatePatternDetectorTuning = StatePatternDetectorTuning(),
        detectorParameters: PatternDetectionParameterSettings = .defaults,
        manualThresholdProfile: ManualThresholdProfile = .automatic,
        frameworkPolicy: HybridPatternDetectionPolicy = .legacyCompatible,
        // P3A: Adaptive-v2 burst canonicalization chokepoint. DEFAULT OFF — when false the pipeline is byte-identical
        // to current behavior (the gated pass early-returns). Do not enable without re-baselining the P0 pins.
        useAdaptiveV2Canonicalization: Bool = false,
        // P9: scope for manual HARD gates. `.allTrains` (default) = byte-identical global application. For `.currentTrain`
        // / `.selectedTrains`, hard gates apply only to in-scope trains; out-of-scope trains see their hard gates demoted
        // to automatic. Soft anchors and automatic mode stay global. If the scope resolves to no train, hard gates affect
        // no train (never a silent fallback to all).
        manualThresholdScope: ManualThresholdScope = .allTrains,
        // Build systems may inject a commit or release identifier. This identifier alone is not a
        // reproducibility guarantee; the default is explicit rather than guessed from mutable state.
        buildCommit: String = DetectionRunIdentity.unavailable
    ) -> ClassicAnchorDetectionRun {
        let settingsSnapshot = DetectionRunSettingsSnapshot.make(
            bandSettings: bandSettings,
            qualitySettings: qualitySettings,
            refractoryAction: refractoryAction,
            stateTuning: stateTuning,
            detectorParameters: detectorParameters,
            manualThresholdProfile: manualThresholdProfile,
            frameworkPolicy: frameworkPolicy,
            useAdaptiveV2Canonicalization: useAdaptiveV2Canonicalization,
            manualThresholdScope: manualThresholdScope
        )
        let runIdentity = DetectionRunIdentity.make(
            dataset: dataset,
            settings: settingsSnapshot,
            buildCommit: buildCommit
        )
        // Identity hashing is invocation bookkeeping, not detector runtime. Start performance
        // accounting only after the immutable run identity has been captured.
        let performanceRecorder = PerformanceRecorder(dataset: dataset)
        // Phase D2-wire: compute the dataset ISI distribution once, before per-train band resolution,
        // at the detector/band floor so it aligns with the valid-ISI set the resolver uses. Diagnostic
        // only in this slice — threaded into the returned run, consumed by no detector (D3 later).
        let datasetISIDistribution = DatasetISIDistributionService.compute(
            dataset: dataset,
            minimumValidISISec: bandSettings.minValidISISec
        )
        let initialPass = performanceRecorder.measureWallStage("initial_train_pass_parallel") {
            parallelMap(count: dataset.trains.count) { index in
                let train = dataset.trains[index]
                return performanceRecorder.measureTrainStage("initial_train_pass", train: train) {
                    let initialResolution = TrainAdaptiveBandResolver.resolve(
                        train: train,
                        settings: bandSettings
                    )
                    let initialDetectorSettings = ClassicAnchorSettings(
                        adaptiveResolution: initialResolution,
                        qualitySettings: qualitySettings,
                        refractoryAction: refractoryAction
                    )
                    .applyingHybridPolicy(frameworkPolicy)
                    .applyingDetectorParameters(detectorParameters)
                    let initialBurstResult = ClassicAnchorDetector.detect(
                        train: train,
                        settings: initialDetectorSettings
                    )
                    let initialBurstCandidates = tagPipelineStage(
                        initialBurstResult.candidates,
                        "train_initial_structural_burst_screen"
                    )
                    let initialStructuralBursts = ClassicAnchorCandidateArbitrator.arbitrate(
                        initialBurstCandidates
                    )
                    let resolution = StructuralAnchorBandRefiner.refine(
                        train: train,
                        resolution: initialResolution,
                        burstCandidates: initialStructuralBursts,
                        qualitySettings: qualitySettings,
                        classicBurstFlankPauseContrastMin: detectorParameters.classicBurstFlankPauseContrastMin
                    )
                    let baseDetectorSettings = ClassicAnchorSettings(
                        adaptiveResolution: resolution,
                        qualitySettings: qualitySettings,
                        refractoryAction: refractoryAction
                    )
                    .applyingHybridPolicy(frameworkPolicy)
                    .applyingDetectorParameters(detectorParameters)
                    var effectiveStateTuning = stateTuning
                    if baseDetectorSettings.longMaxSpikes > 0 {
                        effectiveStateTuning.highFrequencySpikingMinSpikes = max(
                            effectiveStateTuning.highFrequencySpikingMinSpikes,
                            baseDetectorSettings.longMaxSpikes + 1
                        )
                    }
                    let basePauseSettings = PauseDetectorSettings(
                        adaptiveResolution: resolution,
                        qualitySettings: qualitySettings,
                        pauseMinISISecOverride: detectorParameters.pauseMinISISecOverride,
                        classicBurstFlankPauseContrastMin: detectorParameters.classicBurstFlankPauseContrastMin
                    )
                    let baseStateSettings = StatePatternDetectorSettings(
                        adaptiveResolution: resolution,
                        qualitySettings: qualitySettings,
                        tuning: effectiveStateTuning
                    )
                    let resolvedDetectorSettings = applyingManualThresholds(
                        scopedManualProfile(manualThresholdProfile, scope: manualThresholdScope, trainID: train.id),
                        classic: baseDetectorSettings,
                        pause: basePauseSettings,
                        state: baseStateSettings
                    )
                    let detectorSettings = resolvedDetectorSettings.classic
                    let pauseSettings = resolvedDetectorSettings.pause
                    let stateSettings = resolvedDetectorSettings.state
                    let burstResult = ClassicAnchorDetector.detect(train: train, settings: detectorSettings)
                    let burstSeedRunCandidates = tagPipelineStage(
                        BurstSeedRunAssembler.detect(
                            train: train,
                            candidates: initialStructuralBursts,
                            resolution: resolution,
                            settings: detectorSettings
                        ),
                        "train_adaptive_burst_seed_run_assembly"
                    )
                    let pauseResult = PauseDetector.detect(train: train, settings: pauseSettings)
                    let stateResult = StatePatternDetector.detect(train: train, settings: stateSettings)
                    let baseCandidates = tagPipelineStage(
                        tagPipelineStage(burstResult.candidates, "train_adaptive_burst_core") +
                            burstSeedRunCandidates +
                            tagPipelineStage(pauseResult.candidates, "train_adaptive_pause_core") +
                            tagPipelineStage(stateResult.candidates, "train_adaptive_state_core"),
                        "train_adaptive_multitrack_core_input"
                    )
                    let coreSelectedCandidates = ClassicAnchorCandidateArbitrator.arbitrateBySemanticTrack(baseCandidates)
                    let flankPauseCandidates = tagPipelineStage(
                        ClassicBurstFlankPauseDetector.detect(
                            train: train,
                            candidates: coreSelectedCandidates,
                            settings: pauseSettings,
                            existingCandidates: baseCandidates
                        ),
                        "train_adaptive_burst_flank_pause_pool"
                    )
                    let phase1BResolution = MultiTrackPhase1BResolver.resolveWithAudit(
                        train: train,
                        candidates: baseCandidates + flankPauseCandidates,
                        pauseSettings: pauseSettings,
                        stateSettings: stateSettings,
                        stagePrefix: "train_adaptive"
                    )
                    let candidates = tagPipelineStage(
                        phase1BResolution.candidates,
                        "train_structural_seed_summary_input"
                    )
                    let structuralSeedSummary = StructuralSeedBandResolver.summarize(
                        train: train,
                        resolution: resolution,
                        candidates: candidates,
                        qualitySettings: qualitySettings
                    )
                    let finalResolution = StructuralSeedBandResolver.attachingSummary(
                        to: resolution,
                        summary: structuralSeedSummary
                    )
                    let profileCandidate = ClassicAnchorProfileAuditor.seedBandProfileCandidate(
                        train: train,
                        resolution: finalResolution
                    )
                    let eventCoreProfileCandidate = ClassicAnchorProfileAuditor.eventCoreProfileCandidate(
                        train: train,
                        resolution: finalResolution,
                        detectorSettings: detectorSettings,
                        pauseSettings: pauseSettings
                    )
                    let structuralSeedProfileCandidate = ClassicAnchorProfileAuditor.structuralSeedProfileCandidate(
                        train: train,
                        resolution: finalResolution
                    )
                    let result = ClassicAnchorDetectionResult(
                        trainID: train.id,
                        trainName: train.name,
                        candidates: [profileCandidate, eventCoreProfileCandidate, structuralSeedProfileCandidate] + candidates,
                        hfsBurstArbitrationAuditRows: phase1BResolution.hfsBurstArbitrationAuditRows
                    )
                    performanceRecorder.recordTrainResult(result)
                    return (finalResolution, result)
                }
            }
        }
        var resolutions = initialPass.map(\.0)
        var results = initialPass.map(\.1)

        // Pure, pre-bridge train-local resolutions captured before any dataset-level
        // influence. Used as the firewalled, self-excluding source for per-train
        // leave-one-out dataset priors below.
        let initialTrainLocalResolutions = resolutions

        let expansionSeedSummary = performanceRecorder.measureWallStage("dataset_seed_aggregation") {
            StructuralDatasetSeedAggregator.aggregate(
                resolutions: resolutions
            )
        }
        let trainsByID = Dictionary(uniqueKeysWithValues: dataset.trains.map { ($0.id, $0) })
        let resolutionsByID = Dictionary(uniqueKeysWithValues: resolutions.map { ($0.trainID, $0) })
        let bridgeInputResults = results
        let bridgeExpansionApplications = performanceRecorder.measureWallStage("dataset_bridge_expansion_parallel") {
            parallelMap(count: bridgeInputResults.count) {
                index -> (result: ClassicAnchorDetectionResult, bridgeSummary: StructuralDatasetSeedSummary) in
                let result = bridgeInputResults[index]
                guard let train = trainsByID[result.trainID],
                      let resolution = resolutionsByID[result.trainID] else {
                    return (result, .empty)
                }
                return performanceRecorder.measureTrainStage("dataset_bridge_expansion", train: train) {
                    // Leave-one-out: exclude the target train so its own structural evidence
                    // cannot influence the bridge prior applied to itself. Aggregated only from
                    // the firewalled initial train-local resolutions; an empty (no-anchor)
                    // summary degrades bridge expansion to purely train-local. The full
                    // self-inclusive `expansionSeedSummary` above is retained only for
                    // dataset-level audit/display.
                    let bridgeDatasetSummary = StructuralDatasetSeedAggregator.aggregate(
                        resolutions: initialTrainLocalResolutions,
                        excluding: result.trainID
                    )
                    let eventCandidates = result.candidates.filter { $0.finalLabel != .profile }
                    let baseBridgeSettings = ClassicAnchorSettings(
                        adaptiveResolution: resolution,
                        qualitySettings: qualitySettings,
                        refractoryAction: refractoryAction
                    )
                    .applyingHybridPolicy(frameworkPolicy)
                    .applyingDetectorParameters(detectorParameters)
                    let baseBridgePauseSettings = PauseDetectorSettings(
                        adaptiveResolution: resolution,
                        qualitySettings: qualitySettings,
                        pauseMinISISecOverride: detectorParameters.pauseMinISISecOverride,
                        classicBurstFlankPauseContrastMin: detectorParameters.classicBurstFlankPauseContrastMin
                    )
                    var bridgeStateTuning = stateTuning
                    if baseBridgeSettings.longMaxSpikes > 0 {
                        bridgeStateTuning.highFrequencySpikingMinSpikes = max(
                            bridgeStateTuning.highFrequencySpikingMinSpikes,
                            baseBridgeSettings.longMaxSpikes + 1
                        )
                    }
                    let baseBridgeStateSettings = StatePatternDetectorSettings(
                        adaptiveResolution: resolution,
                        qualitySettings: qualitySettings,
                        tuning: bridgeStateTuning
                    )
                    let resolvedBridgeSettings = applyingManualThresholds(
                        scopedManualProfile(manualThresholdProfile, scope: manualThresholdScope, trainID: result.trainID),
                        classic: baseBridgeSettings,
                        pause: baseBridgePauseSettings,
                        state: baseBridgeStateSettings
                    )
                    let bridgeSettings = resolvedBridgeSettings.classic
                    let bridgePauseSettings = resolvedBridgeSettings.pause
                    let bridgeStateSettings = resolvedBridgeSettings.state
                    let bridgeCandidates = tagPipelineStage(
                        StructuralBridgeExpansionResolver.detect(
                            train: train,
                            candidates: eventCandidates,
                            resolution: resolution,
                            datasetSummary: bridgeDatasetSummary,
                            detectorSettings: bridgeSettings,
                            qualitySettings: qualitySettings
                        ),
                        "dataset_seed_bridge_expansion"
                    )
                    guard !bridgeCandidates.isEmpty else {
                        performanceRecorder.recordTrainResult(result)
                        return (result, bridgeDatasetSummary)
                    }
                    let expandedInputCandidates = uniqueCandidatesByID(
                        eventCandidates + bridgeCandidates
                    )
                    let preliminaryExpandedEvents = ClassicAnchorCandidateArbitrator.arbitrateBySemanticTrack(
                        expandedInputCandidates
                    )
                    let expandedFlankPauseCandidates = tagPipelineStage(
                        ClassicBurstFlankPauseDetector.detect(
                            train: train,
                            candidates: preliminaryExpandedEvents,
                            settings: bridgePauseSettings,
                            existingCandidates: expandedInputCandidates
                        ),
                        "dataset_seed_bridge_flank_pause_pool"
                    )
                    let bridgePhase1BResolution = MultiTrackPhase1BResolver.resolveWithAudit(
                        train: train,
                        candidates: expandedInputCandidates + expandedFlankPauseCandidates,
                        pauseSettings: bridgePauseSettings,
                        stateSettings: bridgeStateSettings,
                        stagePrefix: "dataset_seed_bridge"
                    )
                    let expandedEvents = tagPipelineStage(
                        bridgePhase1BResolution.candidates,
                        "dataset_seed_bridge_arbitrated_events"
                    )
                    let expandedResult = ClassicAnchorDetectionResult(
                        trainID: result.trainID,
                        trainName: result.trainName,
                        candidates: expandedEvents,
                        hfsBurstArbitrationAuditRows: bridgePhase1BResolution.hfsBurstArbitrationAuditRows
                    )
                    performanceRecorder.recordTrainResult(expandedResult)
                    return (expandedResult, bridgeDatasetSummary)
                }
            }
        }
        results = bridgeExpansionApplications.map(\.result)
        // Provenance derived from the actual per-train summaries fed to bridge expansion:
        //  - used: at least one train received a usable (anchored) bridge dataset summary;
        //  - leave-one-out: every per-train bridge summary is non-self-inclusive;
        //  - target-inclusion: any per-train bridge summary included the target train.
        let bridgeExpansionUsedDatasetSummary = bridgeExpansionApplications.contains {
            $0.bridgeSummary.hasAnyAnchor
        }
        let bridgeExpansionPriorAppliedLeaveOneOut = bridgeExpansionApplications.allSatisfy {
            !$0.bridgeSummary.isSelfInclusive
        }
        let bridgeExpansionSummaryIncludedTargetTrain = bridgeExpansionApplications.contains {
            $0.bridgeSummary.isSelfInclusive
        }
        let resultsByTrainID = Dictionary(uniqueKeysWithValues: results.map { ($0.trainID, $0) })
        let bridgeInputResolutions = resolutions
        resolutions = performanceRecorder.measureWallStage("train_seed_summary_refresh_parallel") {
            parallelMap(count: bridgeInputResolutions.count) { index in
                let resolution = bridgeInputResolutions[index]
                guard let train = trainsByID[resolution.trainID],
                      let result = resultsByTrainID[resolution.trainID] else {
                    return resolution
                }
                return performanceRecorder.measureTrainStage("train_seed_summary_refresh", train: train) {
                    let eventCandidates = result.candidates.filter { $0.finalLabel != .profile }
                    let structuralSeedSummary = StructuralSeedBandResolver.summarize(
                        train: train,
                        resolution: resolution,
                        candidates: eventCandidates,
                        qualitySettings: qualitySettings
                    )
                    return StructuralSeedBandResolver.attachingSummary(
                        to: resolution,
                        summary: structuralSeedSummary
                    )
                }
            }
        }

        let finalDatasetStructuralSeedSummary = performanceRecorder.measureWallStage("final_dataset_seed_aggregation") {
            StructuralDatasetSeedAggregator.aggregate(
                resolutions: resolutions
            )
        }
        let preApplyResolutions = resolutions
        let leaveOneOutApplications = performanceRecorder.measureWallStage("apply_dataset_seed_summary") {
            preApplyResolutions.map { resolution -> (resolution: TrainAdaptiveBandResolution, applied: Bool) in
                // Leave-one-out: exclude the target train so its own structural evidence
                // cannot contribute to the prior applied back to itself. Aggregated only
                // from the firewalled initial train-local resolutions. When no other train
                // has usable evidence the summary has no anchors and is not applied.
                let leaveOneOutSummary = StructuralDatasetSeedAggregator.aggregate(
                    resolutions: initialTrainLocalResolutions,
                    excluding: resolution.trainID
                )
                return (
                    StructuralSeedBandResolver.applyingDatasetSummary(
                        to: resolution,
                        datasetSummary: leaveOneOutSummary
                    ),
                    leaveOneOutSummary.hasAnyAnchor
                )
            }
        }
        resolutions = leaveOneOutApplications.map(\.resolution)
        let datasetSummaryAppliedToAnyResolution = leaveOneOutApplications.contains { $0.applied }
        let finalResolutionsByID = Dictionary(uniqueKeysWithValues: resolutions.map { ($0.trainID, $0) })
        let finalInputResults = results
        results = performanceRecorder.measureWallStage("dataset_seed_aware_rerun_parallel") {
            parallelMap(count: finalInputResults.count) { index in
                let result = finalInputResults[index]
                guard let train = trainsByID[result.trainID],
                      let resolution = finalResolutionsByID[result.trainID] else {
                    return result
                }
                return performanceRecorder.measureTrainStage("dataset_seed_aware_rerun", train: train) {
                    let existingEventCandidates = result.candidates.filter { candidate in
                        candidate.finalLabel != .profile &&
                            (candidate.arbitrationTrack == .event || candidate.arbitrationTrack == .review)
                    }
                    let baseFinalDetectorSettings = ClassicAnchorSettings(
                        adaptiveResolution: resolution,
                        qualitySettings: qualitySettings,
                        refractoryAction: refractoryAction
                    )
                    .applyingHybridPolicy(frameworkPolicy)
                    .applyingDetectorParameters(detectorParameters)
                    let baseFinalPauseSettings = PauseDetectorSettings(
                        adaptiveResolution: resolution,
                        qualitySettings: qualitySettings,
                        pauseMinISISecOverride: detectorParameters.pauseMinISISecOverride,
                        classicBurstFlankPauseContrastMin: detectorParameters.classicBurstFlankPauseContrastMin
                    )
                    var finalStateTuning = stateTuning
                    if baseFinalDetectorSettings.longMaxSpikes > 0 {
                        finalStateTuning.highFrequencySpikingMinSpikes = max(
                            finalStateTuning.highFrequencySpikingMinSpikes,
                            baseFinalDetectorSettings.longMaxSpikes + 1
                        )
                    }
                    let baseFinalStateSettings = StatePatternDetectorSettings(
                        adaptiveResolution: resolution,
                        qualitySettings: qualitySettings,
                        tuning: finalStateTuning
                    )
                    let resolvedFinalSettings = applyingManualThresholds(
                        scopedManualProfile(manualThresholdProfile, scope: manualThresholdScope, trainID: result.trainID),
                        classic: baseFinalDetectorSettings,
                        pause: baseFinalPauseSettings,
                        state: baseFinalStateSettings
                    )
                    let finalDetectorSettings = resolvedFinalSettings.classic
                    let finalPauseSettings = resolvedFinalSettings.pause
                    let finalStateSettings = resolvedFinalSettings.state
                    let seedAwareBurstResult = ClassicAnchorDetector.detect(
                        train: train,
                        settings: finalDetectorSettings
                    )
                    let seedAwareBurstSeedRunCandidates = tagPipelineStage(
                        BurstSeedRunAssembler.detect(
                            train: train,
                            candidates: existingEventCandidates,
                            resolution: resolution,
                            settings: finalDetectorSettings
                        ),
                        "dataset_seed_aware_burst_seed_run_assembly"
                    )
                    let seedAwarePauseResult = PauseDetector.detect(
                        train: train,
                        settings: finalPauseSettings
                    )
                    let seedAwareStateResult = StatePatternDetector.detect(
                        train: train,
                        settings: finalStateSettings
                    )
                    let seedAwareInputCandidates = uniqueCandidatesByID(
                        existingEventCandidates +
                            tagPipelineStage(seedAwareBurstResult.candidates, "dataset_seed_aware_burst_core") +
                            seedAwareBurstSeedRunCandidates +
                            tagPipelineStage(seedAwarePauseResult.candidates, "dataset_seed_aware_pause_core") +
                            tagPipelineStage(seedAwareStateResult.candidates, "dataset_seed_aware_state_core")
                    )
                    let seedAwarePreliminaryCandidates = ClassicAnchorCandidateArbitrator.arbitrateBySemanticTrack(
                        seedAwareInputCandidates
                    )
                    let seedAwareFlankPauseCandidates = tagPipelineStage(
                        ClassicBurstFlankPauseDetector.detect(
                            train: train,
                            candidates: seedAwarePreliminaryCandidates,
                            settings: finalPauseSettings,
                            existingCandidates: seedAwareInputCandidates
                        ),
                        "dataset_seed_aware_burst_flank_pause_pool"
                    )
                    let finalPhase1BResolution = MultiTrackPhase1BResolver.resolveWithAudit(
                        train: train,
                        candidates: seedAwareInputCandidates + seedAwareFlankPauseCandidates,
                        pauseSettings: finalPauseSettings,
                        stateSettings: finalStateSettings,
                        stagePrefix: "dataset_seed_aware"
                    )
                    let seedAwareCandidates = taggingManualThresholdProvenance(
                        tagPipelineStage(
                            applyingAdaptiveV2BurstCanonicalization(
                                demotingTonicRateRegularBurstsAndRearbitrating(
                                    enforcingBurstHardGateAndRearbitrating(
                                        // BCB-HF: normalize adaptive local-HF burst packets here, at the seed-aware seam,
                                        // BEFORE Adaptive-V2 canonicalization and the final arbitration. These packets are
                                        // born under a collapsed band that starves BCB-1's seed-band reference at `detect`,
                                        // so a gross incompatible boundary ISI (several× the train-local seed-band upper)
                                        // could survive and win Adaptive-V2 possible-burst selection. The seam re-applies
                                        // the same scale-free two-evidence trim with the VALID seed-aware band reference.
                                        ClassicAnchorDetector.boundaryNormalizedAdaptiveHFBurstPackets(
                                            finalPhase1BResolution.candidates,
                                            train: train,
                                            settings: finalDetectorSettings
                                        ),
                                        settings: finalDetectorSettings
                                    ),
                                    settings: finalDetectorSettings,
                                    manualBurstGateActive: hasUserBurstGate(resolvedFinalSettings.resolved)
                                ),
                                enabled: useAdaptiveV2Canonicalization,
                                train: train,
                                classic: finalDetectorSettings,
                                state: finalStateSettings,
                                pause: finalPauseSettings,
                                manualBurstGateActive: hasUserBurstGate(resolvedFinalSettings.resolved),
                                minValidISISec: bandSettings.minValidISISec
                            ),
                            "dataset_seed_aware_final_arbitration"
                        ),
                        resolved: resolvedFinalSettings.resolved,
                        scope: manualThresholdScope,
                        trainID: train.id
                    )
                    let profileCandidate = ClassicAnchorProfileAuditor.seedBandProfileCandidate(
                        train: train,
                        resolution: resolution
                    )
                    let eventCoreProfileCandidate = ClassicAnchorProfileAuditor.eventCoreProfileCandidate(
                        train: train,
                        resolution: resolution,
                        detectorSettings: finalDetectorSettings,
                        pauseSettings: finalPauseSettings
                    )
                    let structuralSeedProfileCandidate = ClassicAnchorProfileAuditor.structuralSeedProfileCandidate(
                        train: train,
                        resolution: resolution
                    )
                    let finalResult = ClassicAnchorDetectionResult(
                        trainID: result.trainID,
                        trainName: result.trainName,
                        candidates: [profileCandidate, eventCoreProfileCandidate, structuralSeedProfileCandidate] + seedAwareCandidates,
                        hfsBurstArbitrationAuditRows: finalPhase1BResolution.hfsBurstArbitrationAuditRows
                    )
                    performanceRecorder.recordTrainResult(finalResult)
                    return finalResult
                }
            }
        }
        if let datasetProfileCandidate = ClassicAnchorProfileAuditor.datasetStructuralSeedProfileCandidate(
            dataset: dataset,
            summary: finalDatasetStructuralSeedSummary
        ),
           let firstResult = results.first {
            results[0] = ClassicAnchorDetectionResult(
                trainID: firstResult.trainID,
                trainName: firstResult.trainName,
                candidates: [datasetProfileCandidate] + firstResult.candidates,
                hfsBurstArbitrationAuditRows: firstResult.hfsBurstArbitrationAuditRows
            )
        }

        let datasetRerunProvenance = ClassicAnchorDatasetRerunProvenance(
            stagePath: [
                "dataset_seed_aggregation",
                "dataset_bridge_expansion",
                "train_seed_summary_refresh",
                "final_dataset_seed_aggregation",
                "apply_dataset_seed_summary",
                "dataset_seed_aware_rerun"
            ],
            trainCount: dataset.trains.count,
            initialDatasetSummary: expansionSeedSummary,
            finalDatasetSummary: finalDatasetStructuralSeedSummary,
            // Reflects the per-train leave-one-out summaries actually fed to bridge
            // expansion: true when at least one train received a usable anchored prior.
            bridgeExpansionUsedDatasetSummary: bridgeExpansionUsedDatasetSummary,
            datasetSummaryAppliedToResolutions: datasetSummaryAppliedToAnyResolution,
            rerunExecuted: !finalInputResults.isEmpty,
            rerunTrainCount: finalInputResults.count,
            source: "dataset_seed_aware_rerun",
            // Leave-one-out: the per-train applied prior never includes the target
            // train, so this is always false even though finalDatasetSummary above
            // remains the full self-inclusive summary for dataset-level audit/display.
            datasetSummaryIncludedTargetTrain: false,
            // The per-train dataset structural prior is applied leave-one-out.
            datasetPriorAppliedLeaveOneOut: true,
            // Bridge expansion likewise applies the dataset prior leave-one-out per train;
            // target-inclusion is false because every bridge summary excludes its target.
            bridgeExpansionPriorAppliedLeaveOneOut: bridgeExpansionPriorAppliedLeaveOneOut,
            bridgeExpansionSummaryIncludedTargetTrain: bridgeExpansionSummaryIncludedTargetTrain
        )

        return ClassicAnchorDetectionRun(
            bandSettings: bandSettings,
            qualitySettings: qualitySettings,
            resolutions: resolutions,
            results: results,
            datasetStructuralSeedSummary: finalDatasetStructuralSeedSummary,
            datasetRerunProvenance: datasetRerunProvenance,
            performanceReport: performanceRecorder.makeReport(results: results),
            datasetISIDistribution: datasetISIDistribution,
            runIdentity: runIdentity
        )
    }

    private final class ParallelMapBox<T: Sendable>: @unchecked Sendable {
        private var values: [T?]
        private let lock = NSLock()

        init(count: Int) {
            values = Array<T?>(repeating: nil, count: count)
        }

        func set(_ value: T, at index: Int) {
            lock.lock()
            values[index] = value
            lock.unlock()
        }

        func materialize(fillingMissingWith fallback: (Int) -> T) -> [T] {
            values.enumerated().map { index, value in
                value ?? fallback(index)
            }
        }
    }

    private static func parallelMap<T: Sendable>(
        count: Int,
        _ transform: @escaping @Sendable (Int) -> T
    ) -> [T] {
        guard count > 1 else {
            return (0..<count).map(transform)
        }

        let box = ParallelMapBox<T>(count: count)
        DispatchQueue.concurrentPerform(iterations: count) { index in
            box.set(transform(index), at: index)
        }
        return box.materialize { index in
            transform(index)
        }
    }

    private static func uniqueCandidatesByID(
        _ candidates: [ClassicAnchorCandidate]
    ) -> [ClassicAnchorCandidate] {
        var latestByID: [String: ClassicAnchorCandidate] = [:]
        var order: [String] = []
        order.reserveCapacity(candidates.count)

        for candidate in candidates {
            if latestByID[candidate.id] == nil {
                order.append(candidate.id)
            }
            latestByID[candidate.id] = candidate
        }

        return order.compactMap { latestByID[$0] }
    }

    private static func candidatesWithPauseFloorCompletion(
        train: SpikeTrain,
        candidates: [ClassicAnchorCandidate],
        settings: PauseDetectorSettings,
        stage: String
    ) -> [ClassicAnchorCandidate] {
        let preliminary = ClassicAnchorCandidateArbitrator.arbitrateBySemanticTrack(candidates)
        let completions = tagPipelineStage(
            PauseMonotonicCompletionDetector.detect(
                train: train,
                candidates: preliminary,
                settings: settings
            ),
            stage
        )
        guard !completions.isEmpty else {
            return candidates
        }
        return uniqueCandidatesByID(candidates + completions)
    }

    private static func tagPipelineStage(
        _ candidates: [ClassicAnchorCandidate],
        _ stage: String
    ) -> [ClassicAnchorCandidate] {
        candidates.map { tagPipelineStage($0, stage) }
    }

    private static func tagPipelineStage(
        _ candidate: ClassicAnchorCandidate,
        _ stage: String
    ) -> ClassicAnchorCandidate {
        let tag = "pipeline_stage=\(stage)"
        guard !candidate.decisionPath.contains(tag) else {
            return candidate
        }
        let trimmed = candidate.decisionPath.trimmingCharacters(in: .whitespacesAndNewlines)
        let decisionPath = trimmed.isEmpty ? tag : "\(candidate.decisionPath);\(tag)"
        return candidate.withDiagnosticOverride(
            decisionPath: decisionPath,
            selectedForAuto: candidate.selectedForAuto,
            selectionStatus: candidate.selectionStatus
        )
    }

    // MARK: - Manual threshold resolution

    /// The three detector settings after the per-train manual threshold profile is resolved and
    /// applied. `resolved` is nil for an all-automatic profile so the pure-adaptive path is
    /// byte-for-byte preserved and no provenance is tagged.
    private struct ResolvedDetectorSettings {
        var classic: ClassicAnchorSettings
        var pause: PauseDetectorSettings
        var state: StatePatternDetectorSettings
        var resolved: ResolvedThresholdProfile?
    }

    /// Resolve the manual threshold profile against the per-train effective adaptive settings and
    /// feed the resolved bounds/counts back into the three detector settings. Narrow-only/union
    /// semantics live entirely in `ManualThresholdResolver`; here we only thread the resolved values
    /// into the detectors and engage the existing burst hard-threshold route when a burst ISI bound
    /// is hard-gated. An all-automatic profile short-circuits to the untouched settings.
    /// P9: the manual threshold profile a given train should see, given the manual hard-threshold scope. A train inside
    /// the scope — and every train when scope is `.allTrains` — sees the profile unchanged (so `.allTrains` is
    /// byte-identical to the global path). A train outside the scope sees its hard gates demoted to automatic, while soft
    /// anchors and automatic fields stay (soft anchors remain global). A scope that resolves to no train therefore drops
    /// hard gates for every train — never a silent fallback to all.
    private static func scopedManualProfile(
        _ profile: ManualThresholdProfile,
        scope: ManualThresholdScope,
        trainID: String
    ) -> ManualThresholdProfile {
        scope.appliesTo(trainID: trainID) ? profile : profile.droppingHardGates()
    }

    private static func applyingManualThresholds(
        _ profile: ManualThresholdProfile,
        classic: ClassicAnchorSettings,
        pause: PauseDetectorSettings,
        state: StatePatternDetectorSettings
    ) -> ResolvedDetectorSettings {
        guard !profile.isAllAutomatic else {
            return ResolvedDetectorSettings(classic: classic, pause: pause, state: state, resolved: nil)
        }

        let input = AdaptiveThresholdInput(
            burstSeedLowerSec: classic.effectiveBurstBandLowerSec,
            burstSeedUpperSec: classic.effectiveBurstBandUpperSec,
            burstBridgeUpperSec: classic.effectiveBurstBridgeUpperSec,
            burstMinSpikes: classic.minSpikes,
            burstClassicMaxSpikes: classic.classicMaxSpikes,
            burstLongMinSpikes: classic.longMinSpikes,
            burstLongMaxSpikes: classic.longMaxSpikes,
            hfsMinSpikes: state.highFrequencySpikingMinSpikes,
            hfsMinDurationSec: state.highFrequencySpikingMinDurationSec,
            hfTonicFloorSec: state.highFrequencyTonicFloorSec,
            hfTonicUpperSec: state.highFrequencyTonicUpperSec,
            hfTonicMinSpikes: state.highFrequencyTonicMinSpikes,
            tonicLowerSec: state.tonicLowerSec,
            tonicUpperSec: state.tonicUpperSec,
            tonicMinSpikes: state.tonicMinSpikes,
            pauseLowerSec: pause.seedBaselineSec
        )
        var resolved = ManualThresholdResolver.resolve(profile: profile, adaptive: input)
        // Phase 1D: carry learned-provenance notes through to candidate tagging (additive metadata only).
        resolved.learnedProvenanceByKey = profile.learnedProvenanceByKey

        // ---- Burst (ClassicAnchorSettings) ----
        var newClassic = classic
        newClassic.burstBandLowerSec = resolved.burst.lowerSec
        newClassic.burstBandUpperSec = max(resolved.burst.lowerSec, resolved.burst.upperSec)
        if let bridge = resolved.burst.bridgeUpperSec {
            newClassic.burstBridgeUpperSec = max(newClassic.burstBandUpperSec, bridge)
        }
        if let value = resolved.burst.minSpikes {
            newClassic.minSpikes = max(3, value)
        }
        if let value = resolved.burst.classicMaxSpikes {
            newClassic.classicMaxSpikes = max(newClassic.minSpikes, value)
        }
        if let value = resolved.burst.longMinSpikes {
            newClassic.longMinSpikes = max(newClassic.classicMaxSpikes + 1, value)
        }
        if let value = resolved.burst.longMaxSpikes {
            newClassic.longMaxSpikes = max(newClassic.longMinSpikes, value)
        }
        // Reuse the existing direct seed+bridge burst hard-threshold route only when the user
        // explicitly hard-gated a burst ISI bound (a soft anchor only widens the adaptive band and
        // must never force a candidate, so it does not engage the forcing route).
        if hasHardGate(resolved, keys: ["burst.seed_lower_sec", "burst.seed_upper_sec", "burst.bridge_upper_sec"]) {
            newClassic.burstHardThresholdEnabled = true
            newClassic.burstHardThresholdSource = "manual_threshold_burst_hard_gate"
        }

        // ---- Pause (PauseDetectorSettings) ----
        var newPause = pause
        newPause.adaptiveLowerSec = resolved.pause.lowerSec
        newPause.adaptiveUpperSec = max(newPause.adaptiveUpperSec ?? resolved.pause.upperSec, resolved.pause.upperSec)

        // ---- Tonic / HF-tonic / HFS (StatePatternDetectorSettings) ----
        var newState = state
        newState.tonicLowerSec = resolved.tonic.lowerSec
        newState.tonicUpperSec = max(resolved.tonic.lowerSec, resolved.tonic.upperSec)
        if let value = resolved.tonic.minSpikes {
            newState.tonicMinSpikes = max(3, value)
        }
        // A tonic ISI hard gate must actually constrain tonic candidate generation; the tonic
        // detector's runtime search band is quantile-derived and ignores tonicLowerSec/tonicUpperSec.
        // Route the user's RAW hard bound into the dedicated manual band (narrow-only is enforced by
        // the max/min applied in tonicStructuralSearchBounds). The resolved band still drives the
        // settings fields and provenance; only the search-band narrowing uses the raw user value so
        // it stays on the same scale as the quantile search band.
        if profile.tonic.isiLower.mode == .hardGate,
           let value = profile.tonic.isiLower.valueSec, value.isFinite, value > 0 {
            newState.manualTonicHardLowerSec = value
        }
        if profile.tonic.isiUpper.mode == .hardGate,
           let value = profile.tonic.isiUpper.valueSec, value.isFinite, value > 0 {
            newState.manualTonicHardUpperSec = value
        }
        newState.highFrequencyTonicFloorSec = resolved.hfTonic.lowerSec
        newState.highFrequencyTonicUpperSec = max(resolved.hfTonic.lowerSec, resolved.hfTonic.upperSec)
        if let value = resolved.hfTonic.minSpikes {
            newState.highFrequencyTonicMinSpikes = max(3, value)
        }
        if let value = resolved.hfs.minSpikes {
            newState.highFrequencySpikingMinSpikes = max(3, value)
        }
        if let value = resolved.hfs.minDurationSec {
            newState.highFrequencySpikingMinDurationSec = max(0, value)
        }
        // Preserve the existing invariant that HFS min spikes sits above the long-burst ceiling
        // (the pipeline normally bumps it to longMaxSpikes + 1) UNLESS the user explicitly
        // hard-gated HFS min spikes, in which case their value is respected verbatim.
        if !hasHardGate(resolved, keys: ["hfs.min_spikes"]) {
            newState.highFrequencySpikingMinSpikes = max(
                newState.highFrequencySpikingMinSpikes,
                newClassic.longMaxSpikes + 1
            )
        }

        return ResolvedDetectorSettings(classic: newClassic, pause: newPause, state: newState, resolved: resolved)
    }

    private static func hasHardGate(_ resolved: ResolvedThresholdProfile, keys: Set<String>) -> Bool {
        resolved.provenance.contains { keys.contains($0.key) && $0.source == .userHardGate }
    }

    /// True when the user supplied ANY burst seed/bridge gate — hard gate OR soft anchor — for which the
    /// AUTOMATIC-ONLY Phase 11 tonic-rate regular-burst guard must stand down. A soft anchor does not set
    /// `burstHardThresholdEnabled` (it never forces a candidate) nor `burstBandSource`, so it is detected
    /// here via the resolved manual-threshold provenance instead. Nil (all-automatic) → false.
    private static func hasUserBurstGate(_ resolved: ResolvedThresholdProfile?) -> Bool {
        guard let resolved else { return false }
        let burstKeys: Set<String> = ["burst.seed_lower_sec", "burst.seed_upper_sec", "burst.bridge_upper_sec"]
        return resolved.provenance.contains {
            burstKeys.contains($0.key) && ($0.source == .userHardGate || $0.source == .userSoftAnchor)
        }
    }

    /// PARAM-1/PARAM-2 hard-gate enforcement. When the user's Burst hard gate is active
    /// (`settings.burstHardThresholdEnabled`), no auto-selected burst-family candidate may cover an ISI
    /// above the effective hard bridge ceiling (`effectiveBurstBridgeUpperSec` — the absolute legal
    /// maximum for any seed or bridge interval; `>= effectiveBurstBandUpperSec`). Several burst routes
    /// (structure-first, adaptive-local-HF, burst-local-completion, seed-run assembler, structural
    /// bridge expansion) derive their admission ceiling from train-relative / structural quantities and
    /// never consult the resolved band, and no earlier choke point re-checks it. This final pass marks
    /// every violator ineligible, then re-runs semantic-track arbitration so a smaller compliant burst
    /// can replace an over-wide winner. Demotion (not deletion) keeps the candidate visible for review /
    /// CSV with an explicit `decisionPath` note. It is a strict no-op unless a real burst hard gate is engaged
    /// (`burstHardThresholdEnabled` is set only for a hard-gated burst ISI bound, never for soft anchors
    /// or the automatic path), so all-automatic and soft-anchor output stay byte-for-byte unchanged.
    private static func enforcingBurstHardGateAndRearbitrating(
        _ candidates: [ClassicAnchorCandidate],
        settings: ClassicAnchorSettings
    ) -> [ClassicAnchorCandidate] {
        guard settings.burstHardThresholdEnabled else { return candidates }
        let demoted = enforcingBurstHardGate(candidates, settings: settings)
        let demotedIDs = Set(demoted.filter { isBurstHardGateDemotion($0) }.map(\.id))
        guard !demotedIDs.isEmpty else { return demoted }

        return ClassicAnchorCandidateArbitrator.arbitrateBySemanticTrack(demoted).map { candidate in
            guard demotedIDs.contains(candidate.id) else { return candidate }
            return candidate.withDiagnosticOverride(
                gateStatus: "reject_burst_hard_gate_band_exceeded",
                action: "reject",
                selectedForAuto: false,
                selectionStatus: "not_selected__burst_hard_gate_band_exceeded"
            )
        }
    }

    private static func enforcingBurstHardGate(
        _ candidates: [ClassicAnchorCandidate],
        settings: ClassicAnchorSettings
    ) -> [ClassicAnchorCandidate] {
        guard settings.burstHardThresholdEnabled else { return candidates }
        let bridgeUpper = settings.effectiveBurstBridgeUpperSec
        let tolerance = 1e-9
        return candidates.map { candidate in
            guard candidate.finalLabel.isBurstEventFamily,
                  candidate.isEligibleForAutoSelection,
                  let maxIntra = candidate.maxIntraISISec,
                  maxIntra > bridgeUpper + tolerance else {
                return candidate
            }
            let note = String(
                format: ";hard_gate_burst_band_demotion=covered_isi_%.4fs_exceeds_bridge_upper_%.4fs",
                maxIntra, bridgeUpper
            )
            return candidate.withDiagnosticOverride(
                gateStatus: "reject_burst_hard_gate_band_exceeded",
                decisionPath: candidate.decisionPath + note,
                action: "reject",
                selectedForAuto: false,
                selectionStatus: "not_selected__burst_hard_gate_band_exceeded"
            )
        }
    }

    private static func isBurstHardGateDemotion(_ candidate: ClassicAnchorCandidate) -> Bool {
        candidate.selectionStatus == "not_selected__burst_hard_gate_band_exceeded" &&
            candidate.decisionPath.contains("hard_gate_burst_band_demotion=")
    }

    // MARK: - ALGO-RETUNE Phase 11: tonic-rate regular burst guard (automatic mode only)
    //
    // A classic burst is a transient HIGH-FREQUENCY discharge. In a pause-dominated train whose only
    // fast structure is a modest acceleration (e.g. the pause_response_2_s packet: four perfectly regular
    // ~0.080 s ISIs in a 2.0 s silent baseline), the adaptive burst seed band and compactness ceiling both
    // stretch to that tonic-rate cluster, and the structure-first / classic-anchor routes admit it as a
    // Burst purely on edge contrast against the SILENCE. R resolves the classical (non-slow-tail) burst
    // bridge upper to <= 0.050 s (R/39_threshold_resolved_event_grammar.R) and treats ISIs up to ~0.060 s
    // (T_max) as tonic (R/38_event_grammar_core.R), so a cluster whose bulk interval (intra q90) sits above
    // that classical-burst core ceiling AND is near-perfectly regular is sustained tonic-rate firing, not a
    // burst. This pass demotes only such candidates and re-arbitrates, mirroring the burst hard-gate pass.
    //
    // It is gated to the fully AUTOMATIC burst path: a user burst hard/soft gate expresses explicit intent
    // about which ISI scale is a burst (e.g. a Hard seed max = 100 ms keeps a regular 90 ms triplet a Burst)
    // and is respected verbatim. It is a strict no-op for any train whose bursts are faster than the
    // classical core ceiling or irregular, so normal automatic detection stays byte-for-byte unchanged.

    /// Classical-burst core ISI ceiling (seconds). R caps the classical burst bridge upper at 0.050 s; an
    /// intra-burst bulk interval (q90) above this is tonic / HF-tonic scale, not a high-frequency burst.
    private static let autoTonicRateBurstCoreCeilingSec = 0.050
    /// A slow cluster that is also this regular is sustained tonic-rate firing, not a transient burst.
    /// Genuine classic bursts are irregular (accelerating–decelerating; cv typically >= 0.18).
    private static let autoTonicRateBurstRegularityCVMax = 0.15

    private static func isTonicRateRegularBurst(_ candidate: ClassicAnchorCandidate) -> Bool {
        guard let intraQ90 = candidate.intraQ90Sec, intraQ90.isFinite,
              intraQ90 > autoTonicRateBurstCoreCeilingSec + 1e-9,
              let cv = candidate.cv, cv.isFinite,
              cv <= autoTonicRateBurstRegularityCVMax else {
            return false
        }
        return true
    }

    // P3A: central Adaptive-v2 burst canonicalization chokepoint. DEFAULT OFF: when `enabled` is false this is a pure
    // no-op (returns the input unchanged → byte-identical pipeline output). When enabled (P3B), each currently-canonical
    // burst is run through the P1 evidence + P2 verdict + `BurstCanonicalizationGate.decide`; non-canonical ones are
    // demoted to `possibleBurst` and the pool is re-arbitrated so competing states can win. Reuses the same diagnostic-
    // override + re-arbitration machinery as the existing demotion passes.
    /// The EFFECTIVE per-family interval bands after adaptive + manual resolution, as a `ResolvedThresholdProfile`.
    /// Unlike `ResolvedDetectorSettings.resolved` (nil in automatic mode), this is always available — it reads the
    /// resolved detector/state/pause settings directly. Audit-only; used only by the Adaptive-v2 chokepoint.
    private static func effectiveResolvedProfile(
        classic: ClassicAnchorSettings, state: StatePatternDetectorSettings, pause: PauseDetectorSettings
    ) -> ResolvedThresholdProfile {
        ResolvedThresholdProfile(
            burst: ResolvedFamilyThresholds(
                lowerSec: classic.effectiveBurstBandLowerSec,
                upperSec: classic.effectiveBurstBandUpperSec,
                bridgeUpperSec: classic.effectiveBurstBridgeUpperSec,
                minSpikes: classic.minSpikes,
                classicMaxSpikes: classic.classicMaxSpikes,
                longMinSpikes: classic.longMinSpikes,
                longMaxSpikes: classic.longMaxSpikes),
            hfs: ResolvedFamilyThresholds(
                lowerSec: classic.effectiveBurstBandLowerSec, upperSec: classic.effectiveBurstBandUpperSec,
                minSpikes: state.highFrequencySpikingMinSpikes, minDurationSec: state.highFrequencySpikingMinDurationSec),
            hfTonic: ResolvedFamilyThresholds(
                lowerSec: state.highFrequencyTonicFloorSec, upperSec: state.highFrequencyTonicUpperSec,
                minSpikes: state.highFrequencyTonicMinSpikes),
            tonic: ResolvedFamilyThresholds(
                lowerSec: state.tonicLowerSec, upperSec: state.tonicUpperSec, minSpikes: state.tonicMinSpikes),
            pause: ResolvedFamilyThresholds(lowerSec: pause.seedBaselineSec, upperSec: .infinity),
            provenance: []
        )
    }

    private static func applyingAdaptiveV2BurstCanonicalization(
        _ candidates: [ClassicAnchorCandidate],
        enabled: Bool,
        train: SpikeTrain,
        classic: ClassicAnchorSettings,
        state: StatePatternDetectorSettings,
        pause: PauseDetectorSettings,
        manualBurstGateActive: Bool,
        minValidISISec: Double
    ) -> [ClassicAnchorCandidate] {
        guard enabled else { return candidates }   // P3A: OFF by default ⇒ byte-identical (zero extra work).
        // P6B-2: AUTOMATIC-ONLY. The Adaptive-v2 canonicalization (demotion + boundary trim) is an automatic heuristic and
        // must not demote or trim a burst the user explicitly forced or bounded — parity with
        // `demotingTonicRateRegularBurstsAndRearbitrating`. Any explicit user burst gate bypasses the whole gate so that
        // hard numeric gates stay eligibility bounds and user bands/anchors are honored.
        guard !classic.burstHardThresholdEnabled,
              classic.burstBandSource != .userPatternISILimit,
              !manualBurstGateActive else {
            return candidates
        }
        // EFFECTIVE per-family bands (adaptive or manual). `resolvedFinalSettings.resolved` is nil in automatic mode,
        // so build the profile from the resolved detector/state/pause settings (always available).
        let resolved = effectiveResolvedProfile(classic: classic, state: state, pause: pause)

        let burstReference = ResolvedFamilyDistributionSummary.fromTrain(
            train: train, family: .burst, lowerSec: resolved.burst.lowerSec,
            upperSec: resolved.burst.bridgeUpperSec ?? resolved.burst.upperSec, provenance: .adaptive,
            minValidISISec: minValidISISec)
        let tonicReference = ResolvedFamilyDistributionSummary.fromTrain(
            train: train, family: .tonic, lowerSec: resolved.tonic.lowerSec, upperSec: resolved.tonic.upperSec,
            provenance: .adaptive, minValidISISec: minValidISISec)
        let hfTonicReference = ResolvedFamilyDistributionSummary.fromTrain(
            train: train, family: .hfTonic, lowerSec: resolved.hfTonic.lowerSec, upperSec: resolved.hfTonic.upperSec,
            provenance: .adaptive, minValidISISec: minValidISISec)

        let burstEligibilityCeilingSec = resolved.burst.bridgeUpperSec ?? resolved.burst.upperSec
        // P6B-2: spans of the currently-SELECTED canonical bursts, for the promote-tight skip (when an already-selected
        // tighter canonical burst is contained in a contaminated wide span, promotion via re-arbitration covers the core
        // and the wide span is demoted as before — no trimmed candidate is synthesized).
        let selectedCanonicalBurstSpans: [(start: Int, end: Int, id: String)] = candidates
            .filter { $0.finalLabel.isCanonicalBurstFamily && $0.selectedForAuto }
            .map { (start: $0.startISIIndex, end: $0.endISIIndex, id: $0.id) }

        var changed = false
        let decided = candidates.map { candidate -> ClassicAnchorCandidate in
            guard candidate.finalLabel.isCanonicalBurstFamily, candidate.isEligibleForAutoSelection else { return candidate }
            let evidence = CandidateIntervalEvidence.build(
                candidate: candidate, train: train, profile: resolved,
                eventness: ISICandidateEventnessAuditor.makeAudit(for: candidate, train: train, minValidISISec: minValidISISec),
                minValidISISec: minValidISISec)
            let verdict = CanonicalizationVerdictBuilder.verdict(
                evidence: evidence, burstReference: burstReference, tonicReference: tonicReference,
                hfTonicReference: hfTonicReference,
                burstEligibilityCeilingSec: burstEligibilityCeilingSec)
            guard BurstCanonicalizationGate.decide(candidate: candidate, evidence: evidence, verdict: verdict) == .demoteToPossible else {
                // P11A: a strong-core rescue is what KEPT this canonical (strong in-band core overrides a moderate q95
                // inflation). Record it on the decision path so the Inspector / explanation / export can show why it
                // stayed a canonical burst. The label and selection are unchanged — only the diagnostic marker is added.
                if verdict.verdict == .strongCoreRescue {
                    changed = true
                    return candidate.withDiagnosticOverride(
                        gateStatus: "keep_adaptive_v2_strong_core_rescue",
                        decisionPath: candidate.decisionPath
                            + ";adaptive_v2_canonicalization=strong_core_rescue(core_\(evidence.burstCoreISICount ?? 0))",
                        action: "strong_core_rescue",
                        selectedForAuto: candidate.selectedForAuto,
                        selectionStatus: candidate.selectionStatus)
                }
                return candidate
            }
            // P6B-2: before demoting, try to RESCUE a boundary-contaminated burst — keep its tight core canonical by
            // trimming at most one slow edge ISI per side (unless a selected tighter subcandidate already covers it).
            if let rescued = boundaryRescuedBurst(
                candidate, train: train, resolved: resolved,
                burstEligibilityCeilingSec: burstEligibilityCeilingSec,
                edgeTolerance: CanonicalizationVerdictSettings().quantileCompatibilityRelativeTolerance,
                burstReference: burstReference, tonicReference: tonicReference, hfTonicReference: hfTonicReference,
                selectedCanonicalBurstSpans: selectedCanonicalBurstSpans, settings: classic, minValidISISec: minValidISISec) {
                changed = true
                return rescued
            }
            changed = true
            return candidate.withDiagnosticOverride(
                finalLabel: .possibleBurst,
                gateStatus: "demote_adaptive_v2_non_canonical_burst",
                decisionPath: candidate.decisionPath + ";adaptive_v2_canonicalization=demote_to_possible(verdict_\(verdict.verdict.rawValue))",
                action: "demote_to_possible",
                selectedForAuto: candidate.selectedForAuto,
                selectionStatus: candidate.selectionStatus
            )
        }
        guard changed else { return decided }
        return ClassicAnchorCandidateArbitrator.arbitrateBySemanticTrack(decided)
    }

    /// P6B-2 boundary rescue. When an Adaptive-v2 burst would be demoted, salvage its tight core if the only reason it
    /// fails the P6B-1 eligibility ceiling is a slow boundary ISI: trim at most one slow ISI per side (an ISI exceeding
    /// `ceiling × (1 + tolerance)`) and keep the candidate canonical IFF the trimmed sub-span re-passes the verdict
    /// (sufficient core+bridge support, q within ceiling, not bridge-without-core). Returns the trimmed canonical
    /// candidate on success, or nil to demote as usual. PREFERS PROMOTION: if a selected tighter canonical burst is
    /// already contained in the span, no trim is synthesized (the wide span is demoted; the selected subcandidate carries
    /// the canonical burst and the slow edge leaves it). Manual semantic overrides never reach here (they keep canonical
    /// at the gate), so trimming cannot override a user's explicit label.
    private static func boundaryRescuedBurst(
        _ candidate: ClassicAnchorCandidate,
        train: SpikeTrain,
        resolved: ResolvedThresholdProfile,
        burstEligibilityCeilingSec ceiling: Double,
        edgeTolerance: Double,
        burstReference: ResolvedFamilyDistributionSummary,
        tonicReference: ResolvedFamilyDistributionSummary,
        hfTonicReference: ResolvedFamilyDistributionSummary,
        selectedCanonicalBurstSpans: [(start: Int, end: Int, id: String)],
        settings: ClassicAnchorSettings,
        minValidISISec: Double
    ) -> ClassicAnchorCandidate? {
        guard ceiling.isFinite, ceiling > 0 else { return nil }
        let start = candidate.startISIIndex, end = candidate.endISIIndex
        guard start >= 0, end < train.isiSec.count, end >= start else { return nil }
        let edgeCeiling = ceiling * (1 + edgeTolerance)
        func isSlowEdge(_ index: Int) -> Bool {
            guard let value = train.isiSec[index], value.isFinite else { return false }
            return value > edgeCeiling
        }
        let slowLeft = isSlowEdge(start)
        let slowRight = isSlowEdge(end)
        // No slow boundary ISI ⇒ this is not edge contamination (e.g. a genuinely near-ceiling tiny span); demote.
        guard slowLeft || slowRight else { return nil }
        // Prefer promotion: a selected, strictly-tighter canonical burst inside the span already carries the core.
        if selectedCanonicalBurstSpans.contains(where: {
            $0.id != candidate.id && $0.start >= start && $0.end <= end && ($0.end - $0.start) < (end - start)
        }) {
            return nil
        }
        let note = String(
            format: ";adaptive_v2_canonicalization=boundary_trim(left:%@,right:%@,ceiling_%.4fs)",
            slowLeft ? "1" : "0", slowRight ? "1" : "0", ceiling)
        guard let trimmed = ClassicAnchorDetector.boundaryTrimmedBurstCandidate(
            from: candidate, train: train, dropLeft: slowLeft, dropRight: slowRight, settings: settings, note: note) else {
            return nil
        }
        let trimmedEvidence = CandidateIntervalEvidence.build(
            candidate: trimmed, train: train, profile: resolved,
            eventness: ISICandidateEventnessAuditor.makeAudit(for: trimmed, train: train, minValidISISec: minValidISISec),
            minValidISISec: minValidISISec)
        let trimmedVerdict = CanonicalizationVerdictBuilder.verdict(
            evidence: trimmedEvidence, burstReference: burstReference, tonicReference: tonicReference,
            hfTonicReference: hfTonicReference, burstEligibilityCeilingSec: ceiling)
        // Keep canonical only if the trimmed core genuinely re-passes the verdict (preserves a core, q within ceiling).
        guard trimmedVerdict.verdict == .canonicalCandidate,
              BurstCanonicalizationGate.decide(candidate: trimmed, evidence: trimmedEvidence, verdict: trimmedVerdict) == .keepCanonical else {
            return nil
        }
        // BURST-BOUNDARY-AUTH: the trimmed core re-passes, so this Adaptive-V2 demotion is a BOUNDARY-ONLY failure. Every
        // slow edge here is out-of-band (> the eligibility ceiling ≥ the seed band), so BCB's NON-SEED extension rule is
        // the governing evidence. Classify each via `nonSeedBoundaryExtensionClass` (BCB's non-seed 2.0×/3.0× rule, the
        // subset relevant to a slow edge): if EVERY slow edge is a BCB-vetted COMPATIBLE EXTENSION (not a contaminant BCB
        // would itself trim), keep the FULL candidate canonical rather than re-trimming against the tighter V2 ceiling.
        // A BCB `contaminant` boundary still trims as before; a non-boundary failure already returned nil above.
        let slowLeftClass = slowLeft ? ClassicAnchorDetector.nonSeedBoundaryExtensionClass(of: candidate, trailing: false, train: train, settings: settings) : .none
        let slowRightClass = slowRight ? ClassicAnchorDetector.nonSeedBoundaryExtensionClass(of: candidate, trailing: true, train: train, settings: settings) : .none
        let anySlowContaminant = slowLeftClass == .contaminant || slowRightClass == .contaminant
        let everySlowCompatible = (!slowLeft || slowLeftClass == .compatibleExtension)
            && (!slowRight || slowRightClass == .compatibleExtension)
        if everySlowCompatible, !anySlowContaminant {
            return candidate.withDiagnosticOverride(
                gateStatus: "keep_adaptive_v2_bcb_compatible_boundary",
                decisionPath: candidate.decisionPath
                    + ";adaptive_v2_canonicalization=bcb_compatible_boundary_kept(left:\(slowLeft ? "1" : "0"),right:\(slowRight ? "1" : "0"))",
                action: "bcb_compatible_boundary_kept",
                selectedForAuto: candidate.selectedForAuto,
                selectionStatus: candidate.selectionStatus)
        }
        return trimmed
    }

    private static func demotingTonicRateRegularBurstsAndRearbitrating(
        _ candidates: [ClassicAnchorCandidate],
        settings: ClassicAnchorSettings,
        manualBurstGateActive: Bool
    ) -> [ClassicAnchorCandidate] {
        // AUTOMATIC-ONLY guard. Any explicit user burst gate bypasses it: a hard forcing gate
        // (`burstHardThresholdEnabled`), a user-supplied burst band (`burstBandSource == .userPatternISILimit`),
        // OR a user hard/soft burst seed/bridge anchor (`manualBurstGateActive`, from resolved provenance).
        guard !settings.burstHardThresholdEnabled,
              settings.burstBandSource != .userPatternISILimit,
              !manualBurstGateActive else {
            return candidates
        }
        let demoted = demotingTonicRateRegularBursts(
            candidates, settings: settings, manualBurstGateActive: manualBurstGateActive
        )
        let demotedIDs = Set(demoted.filter { isTonicRateBurstDemotion($0) }.map(\.id))
        guard !demotedIDs.isEmpty else { return demoted }

        return ClassicAnchorCandidateArbitrator.arbitrateBySemanticTrack(demoted).map { candidate in
            guard demotedIDs.contains(candidate.id) else { return candidate }
            return candidate.withDiagnosticOverride(
                gateStatus: "reject_tonic_rate_regular_burst_auto",
                action: "reject",
                selectedForAuto: false,
                selectionStatus: "not_selected__tonic_rate_regular_burst_auto"
            )
        }
    }

    private static func demotingTonicRateRegularBursts(
        _ candidates: [ClassicAnchorCandidate],
        settings: ClassicAnchorSettings,
        manualBurstGateActive: Bool
    ) -> [ClassicAnchorCandidate] {
        guard !settings.burstHardThresholdEnabled,
              settings.burstBandSource != .userPatternISILimit,
              !manualBurstGateActive else {
            return candidates
        }
        return candidates.map { candidate in
            guard candidate.finalLabel.isBurstEventFamily,
                  candidate.isEligibleForAutoSelection,
                  isTonicRateRegularBurst(candidate),
                  let intraQ90 = candidate.intraQ90Sec,
                  let cv = candidate.cv else {
                return candidate
            }
            let note = String(
                format: ";auto_tonic_rate_regular_burst_demotion=intra_q90_%.4fs_above_classic_core_ceiling_%.4fs_cv_%.3f",
                intraQ90, autoTonicRateBurstCoreCeilingSec, cv
            )
            return candidate.withDiagnosticOverride(
                gateStatus: "reject_tonic_rate_regular_burst_auto",
                decisionPath: candidate.decisionPath + note,
                action: "reject",
                selectedForAuto: false,
                selectionStatus: "not_selected__tonic_rate_regular_burst_auto"
            )
        }
    }

    private static func isTonicRateBurstDemotion(_ candidate: ClassicAnchorCandidate) -> Bool {
        candidate.selectionStatus == "not_selected__tonic_rate_regular_burst_auto" &&
            candidate.decisionPath.contains("auto_tonic_rate_regular_burst_demotion=")
    }

    /// Append the family-relevant manual threshold provenance tokens to each candidate's
    /// decisionPath (which surfaces verbatim in the CSV `decision_path` and `resolved_thresholds`
    /// columns). No-op for an all-automatic profile, so default output is byte-for-byte preserved.
    private static func taggingManualThresholdProvenance(
        _ candidates: [ClassicAnchorCandidate],
        resolved: ResolvedThresholdProfile?,
        // P10: the manual hard-threshold scope + this train's id, so a candidate whose OWN family was hard-gated records
        // a `manual_threshold_scope=...` audit note. Out-of-scope trains have `resolved == nil` (their hard gates were
        // dropped to automatic), so they never reach here; soft-only / automatic families have no `.userHardGate`
        // provenance, so they get no scope note. Default `.allTrains` keeps existing callers byte-identical.
        scope: ManualThresholdScope = .allTrains,
        trainID: String = ""
    ) -> [ClassicAnchorCandidate] {
        guard let resolved else { return candidates }
        let keys = resolved.appliedManualThreshold ? resolved.manualProvenanceKeys() : []
        let learned = resolved.learnedProvenanceByKey
        guard !keys.isEmpty || !learned.isEmpty else {
            return candidates
        }
        let scopeNote = scope.hardGateProvenanceNote(trainID: trainID)
        return candidates.map { candidate in
            guard let prefix = manualProvenancePrefix(for: candidate.finalLabel) else {
                return candidate
            }
            let familyKeys = keys.filter { $0.contains("[\(prefix)") }
            // Phase 1D: per-family learned-from-annotations provenance notes (additive, family-scoped so a
            // candidate only receives notes for its own family — never unrelated learned notes).
            let familyLearned = learned.filter { $0.key.hasPrefix(prefix) }.map(\.value).sorted()
            // P10: the scope note is added ONLY when THIS candidate's family was HARD-gated on this train (a soft-only
            // family receives its soft provenance but no hard-gate scope note).
            let familyHardGated = resolved.provenance.contains { $0.key.hasPrefix(prefix) && $0.source == .userHardGate }
            let familyAdditions = familyKeys + familyLearned + (familyHardGated ? [scopeNote] : [])
            guard !familyAdditions.isEmpty else {
                return candidate
            }
            let addition = familyAdditions.joined(separator: ";")
            guard !candidate.decisionPath.contains(addition) else {
                return candidate
            }
            let trimmed = candidate.decisionPath.trimmingCharacters(in: .whitespacesAndNewlines)
            let decisionPath = trimmed.isEmpty ? addition : "\(candidate.decisionPath);\(addition)"
            return candidate.withDiagnosticOverride(
                decisionPath: decisionPath,
                selectedForAuto: candidate.selectedForAuto,
                selectionStatus: candidate.selectionStatus
            )
        }
    }

    private static func manualProvenancePrefix(for label: ClassicAnchorLabel) -> String? {
        switch label {
        case .burst, .highFrequencyBurst, .longBurst, .possibleBurst:
            return "burst."
        case .tonic:
            return "tonic."
        case .highFrequencyTonic:
            return "hf_tonic."
        case .highFrequencySpiking:
            return "hfs."
        case .pause:
            return "pause."
        case .profile, .reject:
            return nil
        }
    }
}
