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

public struct ClassicAnchorDetectionRun: Sendable {
    public let bandSettings: TrainAdaptiveBandSettings
    public let qualitySettings: SpikeQualitySettings
    public let resolutions: [TrainAdaptiveBandResolution]
    public let results: [ClassicAnchorDetectionResult]
    public let datasetStructuralSeedSummary: StructuralDatasetSeedSummary
    public let performanceReport: ClassicAnchorDetectionPerformanceReport?

    public init(
        bandSettings: TrainAdaptiveBandSettings,
        qualitySettings: SpikeQualitySettings,
        resolutions: [TrainAdaptiveBandResolution],
        results: [ClassicAnchorDetectionResult],
        datasetStructuralSeedSummary: StructuralDatasetSeedSummary = .empty,
        performanceReport: ClassicAnchorDetectionPerformanceReport? = nil
    ) {
        self.bandSettings = bandSettings
        self.qualitySettings = qualitySettings
        self.resolutions = resolutions
        self.results = results
        self.datasetStructuralSeedSummary = datasetStructuralSeedSummary
        self.performanceReport = performanceReport
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
        frameworkPolicy: HybridPatternDetectionPolicy = .legacyCompatible
    ) -> ClassicAnchorDetectionRun {
        let performanceRecorder = PerformanceRecorder(dataset: dataset)
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
                    let detectorSettings = ClassicAnchorSettings(
                        adaptiveResolution: resolution,
                        qualitySettings: qualitySettings,
                        refractoryAction: refractoryAction
                    )
                    .applyingHybridPolicy(frameworkPolicy)
                    .applyingDetectorParameters(detectorParameters)
                    var effectiveStateTuning = stateTuning
                    if detectorSettings.longMaxSpikes > 0 {
                        effectiveStateTuning.highFrequencySpikingMinSpikes = max(
                            effectiveStateTuning.highFrequencySpikingMinSpikes,
                            detectorSettings.longMaxSpikes + 1
                        )
                    }
                    let pauseSettings = PauseDetectorSettings(
                        adaptiveResolution: resolution,
                        qualitySettings: qualitySettings,
                        pauseMinISISecOverride: detectorParameters.pauseMinISISecOverride,
                        classicBurstFlankPauseContrastMin: detectorParameters.classicBurstFlankPauseContrastMin
                    )
                    let stateSettings = StatePatternDetectorSettings(
                        adaptiveResolution: resolution,
                        qualitySettings: qualitySettings,
                        tuning: effectiveStateTuning
                    )
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

        let expansionSeedSummary = performanceRecorder.measureWallStage("dataset_seed_aggregation") {
            StructuralDatasetSeedAggregator.aggregate(
                resolutions: resolutions
            )
        }
        let trainsByID = Dictionary(uniqueKeysWithValues: dataset.trains.map { ($0.id, $0) })
        let resolutionsByID = Dictionary(uniqueKeysWithValues: resolutions.map { ($0.trainID, $0) })
        let bridgeInputResults = results
        results = performanceRecorder.measureWallStage("dataset_bridge_expansion_parallel") {
            parallelMap(count: bridgeInputResults.count) { index in
                let result = bridgeInputResults[index]
                guard let train = trainsByID[result.trainID],
                      let resolution = resolutionsByID[result.trainID] else {
                    return result
                }
                return performanceRecorder.measureTrainStage("dataset_bridge_expansion", train: train) {
                    let eventCandidates = result.candidates.filter { $0.finalLabel != .profile }
                    let bridgeSettings = ClassicAnchorSettings(
                        adaptiveResolution: resolution,
                        qualitySettings: qualitySettings,
                        refractoryAction: refractoryAction
                    )
                    .applyingHybridPolicy(frameworkPolicy)
                    .applyingDetectorParameters(detectorParameters)
                    let bridgePauseSettings = PauseDetectorSettings(
                        adaptiveResolution: resolution,
                        qualitySettings: qualitySettings,
                        pauseMinISISecOverride: detectorParameters.pauseMinISISecOverride,
                        classicBurstFlankPauseContrastMin: detectorParameters.classicBurstFlankPauseContrastMin
                    )
                    var bridgeStateTuning = stateTuning
                    if bridgeSettings.longMaxSpikes > 0 {
                        bridgeStateTuning.highFrequencySpikingMinSpikes = max(
                            bridgeStateTuning.highFrequencySpikingMinSpikes,
                            bridgeSettings.longMaxSpikes + 1
                        )
                    }
                    let bridgeStateSettings = StatePatternDetectorSettings(
                        adaptiveResolution: resolution,
                        qualitySettings: qualitySettings,
                        tuning: bridgeStateTuning
                    )
                    let bridgeCandidates = tagPipelineStage(
                        StructuralBridgeExpansionResolver.detect(
                            train: train,
                            candidates: eventCandidates,
                            resolution: resolution,
                            datasetSummary: expansionSeedSummary,
                            detectorSettings: bridgeSettings,
                            qualitySettings: qualitySettings
                        ),
                        "dataset_seed_bridge_expansion"
                    )
                    guard !bridgeCandidates.isEmpty else {
                        performanceRecorder.recordTrainResult(result)
                        return result
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
                    return expandedResult
                }
            }
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
        resolutions = performanceRecorder.measureWallStage("apply_dataset_seed_summary") {
            resolutions.map { resolution in
                StructuralSeedBandResolver.applyingDatasetSummary(
                    to: resolution,
                    datasetSummary: finalDatasetStructuralSeedSummary
                )
            }
        }
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
                    let finalDetectorSettings = ClassicAnchorSettings(
                        adaptiveResolution: resolution,
                        qualitySettings: qualitySettings,
                        refractoryAction: refractoryAction
                    )
                    .applyingHybridPolicy(frameworkPolicy)
                    .applyingDetectorParameters(detectorParameters)
                    let finalPauseSettings = PauseDetectorSettings(
                        adaptiveResolution: resolution,
                        qualitySettings: qualitySettings,
                        pauseMinISISecOverride: detectorParameters.pauseMinISISecOverride,
                        classicBurstFlankPauseContrastMin: detectorParameters.classicBurstFlankPauseContrastMin
                    )
                    var finalStateTuning = stateTuning
                    if finalDetectorSettings.longMaxSpikes > 0 {
                        finalStateTuning.highFrequencySpikingMinSpikes = max(
                            finalStateTuning.highFrequencySpikingMinSpikes,
                            finalDetectorSettings.longMaxSpikes + 1
                        )
                    }
                    let finalStateSettings = StatePatternDetectorSettings(
                        adaptiveResolution: resolution,
                        qualitySettings: qualitySettings,
                        tuning: finalStateTuning
                    )
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
                    let seedAwareCandidates = tagPipelineStage(
                        finalPhase1BResolution.candidates,
                        "dataset_seed_aware_final_arbitration"
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

        return ClassicAnchorDetectionRun(
            bandSettings: bandSettings,
            qualitySettings: qualitySettings,
            resolutions: resolutions,
            results: results,
            datasetStructuralSeedSummary: finalDatasetStructuralSeedSummary,
            performanceReport: performanceRecorder.makeReport(results: results)
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
}
