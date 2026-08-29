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

/// The final, post-clamp threshold state used for one train's authoritative detector rerun.
///
/// Requested settings and resolver provenance alone are insufficient because detector settings can
/// apply additional safety clamps. Capturing the effective profile at the point of use lets result
/// exporters report what the detector actually consumed without reconstructing mutable logic.
public struct ClassicAnchorResolvedThresholdEvidence: Hashable, Sendable {
    public let trainID: String
    public let trainName: String
    public let stagePath: String
    public let effectiveProfile: ResolvedThresholdProfile
    public let resolutionProvenance: [ResolvedThreshold]
    public let learnedProvenanceByKey: [String: String]

    public init(
        trainID: String,
        trainName: String,
        stagePath: String,
        effectiveProfile: ResolvedThresholdProfile,
        resolutionProvenance: [ResolvedThreshold],
        learnedProvenanceByKey: [String: String]
    ) {
        self.trainID = trainID
        self.trainName = trainName
        self.stagePath = stagePath
        self.effectiveProfile = effectiveProfile
        self.resolutionProvenance = resolutionProvenance
        self.learnedProvenanceByKey = learnedProvenanceByKey
    }
}

public struct ClassicAnchorDetectionRun: Sendable {
    private struct AuthorityBandSnapshot: Equatable, Sendable {
        let pattern: String
        let seedLowerSec: Double
        let seedUpperSec: Double
        let bridgeUpperSec: Double
        let contrastS: Double?
        let primarySource: String

        init(_ band: AdaptiveBand) {
            self.pattern = band.pattern.rawValue
            self.seedLowerSec = band.seedLowerSec
            self.seedUpperSec = band.seedUpperSec
            self.bridgeUpperSec = band.bridgeUpperSec
            self.contrastS = band.contrastS
            self.primarySource = band.primarySource.rawValue
        }
    }

    private struct AuthorityThresholdRowSnapshot: Equatable, Sendable {
        let id: String
        let pattern: String
        let field: String
        let histogramSec: Double?
        let defaultSec: Double?
        let effectiveSec: Double?
        let source: String

        init(_ row: AdaptiveBandThresholdRow) {
            self.id = row.id
            self.pattern = row.pattern.rawValue
            self.field = row.field.rawValue
            self.histogramSec = row.histogramSec
            self.defaultSec = row.defaultSec
            self.effectiveSec = row.effectiveSec
            self.source = row.source.rawValue
        }
    }

    private struct AuthorityResolutionSnapshot: Equatable, Sendable {
        let trainID: String
        let trainName: String
        let minValidISISec: Double
        let histogramBinWidthSec: Double
        let validISICount: Int
        let bands: [AuthorityBandSnapshot]
        let thresholdRows: [AuthorityThresholdRowSnapshot]
        let seedBandProfile: TrainSeedBandProfile
        let structuralSeedSummary: StructuralSeedBandSummary

        init(_ resolution: TrainAdaptiveBandResolution) {
            self.trainID = resolution.trainID
            self.trainName = resolution.trainName
            self.minValidISISec = resolution.minValidISISec
            self.histogramBinWidthSec = resolution.histogramBinWidthSec
            self.validISICount = resolution.validISICount
            self.bands = resolution.bands.values
                .map(AuthorityBandSnapshot.init)
                .sorted { $0.pattern < $1.pattern }
            self.thresholdRows = resolution.thresholdRows
                .map(AuthorityThresholdRowSnapshot.init)
                .sorted {
                    if $0.pattern != $1.pattern {
                        return $0.pattern < $1.pattern
                    }
                    if $0.field != $1.field {
                        return $0.field < $1.field
                    }
                    return $0.id < $1.id
                }
            self.seedBandProfile = resolution.seedBandProfile
            self.structuralSeedSummary = resolution.structuralSeedSummary
        }
    }

    /// Module-private structural seal for detector-produced runs.
    ///
    /// Public callers may still construct a run for UI/tests, but only the detector pipeline (or a
    /// trusted in-module transformation of an already sealed run) can create this snapshot. The
    /// result-package exporter recomputes it from the immutable public fields before export, so a
    /// coordinated public rewrap of results, settings, or threshold evidence cannot masquerade as
    /// the authoritative detector output.
    private struct ResultPackageAuthoritySnapshot: Equatable, Sendable {
        let runIdentity: DetectionRunIdentity
        let invocationSettingsSnapshot: DetectionRunSettingsSnapshot?
        let datasetMetadataSnapshot: DetectionDatasetMetadataSnapshot?
        let resolvedThresholdEvidence: [ClassicAnchorResolvedThresholdEvidence]
        let bandSettings: TrainAdaptiveBandSettings
        let qualitySettings: SpikeQualitySettings
        let resolutions: [AuthorityResolutionSnapshot]
        let results: [ClassicAnchorDetectionResult]
        let datasetStructuralSeedSummary: StructuralDatasetSeedSummary
        let datasetRerunProvenance: ClassicAnchorDatasetRerunProvenance
        let datasetISIDistribution: DatasetISIDistribution?
    }

    public let runIdentity: DetectionRunIdentity
    /// The complete immutable invocation snapshot captured by the detector entry point.
    ///
    /// `runIdentity.settingsSnapshot` is useful for portable identity, but callers can construct a
    /// `ClassicAnchorDetectionRun` manually. Keeping the independently threaded entry-point
    /// snapshot lets scientific exporters prove that a run was produced from the settings it
    /// claims, rather than merely rewrapped with a matching identity.
    public let invocationSettingsSnapshot: DetectionRunSettingsSnapshot?
    /// Dataset display/source metadata frozen at the same detector entry point as the input digest.
    public let datasetMetadataSnapshot: DetectionDatasetMetadataSnapshot?
    /// One final effective threshold snapshot per train, captured at the authoritative rerun seam.
    public let resolvedThresholdEvidence: [ClassicAnchorResolvedThresholdEvidence]
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
    private let resultPackageAuthority: ResultPackageAuthoritySnapshot?

    public init(
        bandSettings: TrainAdaptiveBandSettings,
        qualitySettings: SpikeQualitySettings,
        resolutions: [TrainAdaptiveBandResolution],
        results: [ClassicAnchorDetectionResult],
        datasetStructuralSeedSummary: StructuralDatasetSeedSummary = .empty,
        datasetRerunProvenance: ClassicAnchorDatasetRerunProvenance = .empty,
        performanceReport: ClassicAnchorDetectionPerformanceReport? = nil,
        datasetISIDistribution: DatasetISIDistribution? = nil,
        invocationSettingsSnapshot: DetectionRunSettingsSnapshot? = nil,
        datasetMetadataSnapshot: DetectionDatasetMetadataSnapshot? = nil,
        resolvedThresholdEvidence: [ClassicAnchorResolvedThresholdEvidence] = [],
        runIdentity: DetectionRunIdentity = .legacyUnidentified
    ) {
        self.init(
            bandSettings: bandSettings,
            qualitySettings: qualitySettings,
            resolutions: resolutions,
            results: results,
            datasetStructuralSeedSummary: datasetStructuralSeedSummary,
            datasetRerunProvenance: datasetRerunProvenance,
            performanceReport: performanceReport,
            datasetISIDistribution: datasetISIDistribution,
            invocationSettingsSnapshot: invocationSettingsSnapshot,
            datasetMetadataSnapshot: datasetMetadataSnapshot,
            resolvedThresholdEvidence: resolvedThresholdEvidence,
            runIdentity: runIdentity,
            sealResultPackageAuthority: false
        )
    }

    private init(
        bandSettings: TrainAdaptiveBandSettings,
        qualitySettings: SpikeQualitySettings,
        resolutions: [TrainAdaptiveBandResolution],
        results: [ClassicAnchorDetectionResult],
        datasetStructuralSeedSummary: StructuralDatasetSeedSummary,
        datasetRerunProvenance: ClassicAnchorDatasetRerunProvenance,
        performanceReport: ClassicAnchorDetectionPerformanceReport?,
        datasetISIDistribution: DatasetISIDistribution?,
        invocationSettingsSnapshot: DetectionRunSettingsSnapshot?,
        datasetMetadataSnapshot: DetectionDatasetMetadataSnapshot?,
        resolvedThresholdEvidence: [ClassicAnchorResolvedThresholdEvidence],
        runIdentity: DetectionRunIdentity,
        sealResultPackageAuthority: Bool
    ) {
        let authority = sealResultPackageAuthority
            ? Self.makeResultPackageAuthoritySnapshot(
                bandSettings: bandSettings,
                qualitySettings: qualitySettings,
                resolutions: resolutions,
                results: results,
                datasetStructuralSeedSummary: datasetStructuralSeedSummary,
                datasetRerunProvenance: datasetRerunProvenance,
                datasetISIDistribution: datasetISIDistribution,
                invocationSettingsSnapshot: invocationSettingsSnapshot,
                datasetMetadataSnapshot: datasetMetadataSnapshot,
                resolvedThresholdEvidence: resolvedThresholdEvidence,
                runIdentity: runIdentity
            )
            : nil
        self.runIdentity = runIdentity
        self.invocationSettingsSnapshot = invocationSettingsSnapshot
        self.datasetMetadataSnapshot = datasetMetadataSnapshot
        self.resolvedThresholdEvidence = resolvedThresholdEvidence
        self.bandSettings = bandSettings
        self.qualitySettings = qualitySettings
        self.resolutions = resolutions
        self.results = results
        self.datasetStructuralSeedSummary = datasetStructuralSeedSummary
        self.datasetRerunProvenance = datasetRerunProvenance
        self.performanceReport = performanceReport
        self.datasetISIDistribution = datasetISIDistribution
        self.resultPackageAuthority = authority
    }

    static func authoritativeDetectorRun(
        bandSettings: TrainAdaptiveBandSettings,
        qualitySettings: SpikeQualitySettings,
        resolutions: [TrainAdaptiveBandResolution],
        results: [ClassicAnchorDetectionResult],
        datasetStructuralSeedSummary: StructuralDatasetSeedSummary = .empty,
        datasetRerunProvenance: ClassicAnchorDatasetRerunProvenance = .empty,
        performanceReport: ClassicAnchorDetectionPerformanceReport? = nil,
        datasetISIDistribution: DatasetISIDistribution? = nil,
        invocationSettingsSnapshot: DetectionRunSettingsSnapshot?,
        datasetMetadataSnapshot: DetectionDatasetMetadataSnapshot?,
        resolvedThresholdEvidence: [ClassicAnchorResolvedThresholdEvidence],
        runIdentity: DetectionRunIdentity
    ) -> ClassicAnchorDetectionRun {
        ClassicAnchorDetectionRun(
            bandSettings: bandSettings,
            qualitySettings: qualitySettings,
            resolutions: resolutions,
            results: results,
            datasetStructuralSeedSummary: datasetStructuralSeedSummary,
            datasetRerunProvenance: datasetRerunProvenance,
            performanceReport: performanceReport,
            datasetISIDistribution: datasetISIDistribution,
            invocationSettingsSnapshot: invocationSettingsSnapshot,
            datasetMetadataSnapshot: datasetMetadataSnapshot,
            resolvedThresholdEvidence: resolvedThresholdEvidence,
            runIdentity: runIdentity,
            sealResultPackageAuthority: true
        )
    }

    /// Re-seals a sanctioned in-module transformation of an authoritative detector run.
    ///
    /// This is intentionally internal. It exists for framework tagging and focused test
    /// projections that alter only result records after the detector has produced a sealed run.
    func replacingResultsFromTrustedModuleTransform(
        _ results: [ClassicAnchorDetectionResult]
    ) -> ClassicAnchorDetectionRun {
        precondition(
            hasValidResultPackageAuthority,
            "trusted result replacement requires an authoritative detector-produced run"
        )
        return Self.authoritativeDetectorRun(
            bandSettings: bandSettings,
            qualitySettings: qualitySettings,
            resolutions: resolutions,
            results: results,
            datasetStructuralSeedSummary: datasetStructuralSeedSummary,
            datasetRerunProvenance: datasetRerunProvenance,
            performanceReport: performanceReport,
            datasetISIDistribution: datasetISIDistribution,
            invocationSettingsSnapshot: invocationSettingsSnapshot,
            datasetMetadataSnapshot: datasetMetadataSnapshot,
            resolvedThresholdEvidence: resolvedThresholdEvidence,
            runIdentity: runIdentity
        )
    }

    var hasValidResultPackageAuthority: Bool {
        guard let resultPackageAuthority else {
            return false
        }
        return resultPackageAuthority == Self.makeResultPackageAuthoritySnapshot(
            bandSettings: bandSettings,
            qualitySettings: qualitySettings,
            resolutions: resolutions,
            results: results,
            datasetStructuralSeedSummary: datasetStructuralSeedSummary,
            datasetRerunProvenance: datasetRerunProvenance,
            datasetISIDistribution: datasetISIDistribution,
            invocationSettingsSnapshot: invocationSettingsSnapshot,
            datasetMetadataSnapshot: datasetMetadataSnapshot,
            resolvedThresholdEvidence: resolvedThresholdEvidence,
            runIdentity: runIdentity
        )
    }

    private static func makeResultPackageAuthoritySnapshot(
        bandSettings: TrainAdaptiveBandSettings,
        qualitySettings: SpikeQualitySettings,
        resolutions: [TrainAdaptiveBandResolution],
        results: [ClassicAnchorDetectionResult],
        datasetStructuralSeedSummary: StructuralDatasetSeedSummary,
        datasetRerunProvenance: ClassicAnchorDatasetRerunProvenance,
        datasetISIDistribution: DatasetISIDistribution?,
        invocationSettingsSnapshot: DetectionRunSettingsSnapshot?,
        datasetMetadataSnapshot: DetectionDatasetMetadataSnapshot?,
        resolvedThresholdEvidence: [ClassicAnchorResolvedThresholdEvidence],
        runIdentity: DetectionRunIdentity
    ) -> ResultPackageAuthoritySnapshot {
        ResultPackageAuthoritySnapshot(
            runIdentity: runIdentity,
            invocationSettingsSnapshot: invocationSettingsSnapshot,
            datasetMetadataSnapshot: datasetMetadataSnapshot,
            resolvedThresholdEvidence: resolvedThresholdEvidence,
            bandSettings: bandSettings,
            qualitySettings: qualitySettings,
            resolutions: resolutions.map(AuthorityResolutionSnapshot.init),
            results: results,
            datasetStructuralSeedSummary: datasetStructuralSeedSummary,
            datasetRerunProvenance: datasetRerunProvenance,
            datasetISIDistribution: datasetISIDistribution
        )
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
    private struct FinalRerunOutput: Sendable {
        let result: ClassicAnchorDetectionResult
        let thresholdEvidence: ClassicAnchorResolvedThresholdEvidence?
    }

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
        let datasetMetadataSnapshot = DetectionDatasetMetadataSnapshot.make(dataset: dataset)
        let runIdentity = DetectionRunIdentity.make(
            dataset: dataset,
            settings: settingsSnapshot,
            buildCommit: buildCommit
        )
        // Identity hashing is invocation bookkeeping, not detector runtime. Start performance
        // accounting only after the immutable run identity has been captured.
        let performanceRecorder = PerformanceRecorder(dataset: dataset)
        // Freeze one raw-data-only relative state-band profile for this dataset/sheet before any
        // Burst, Pause, Tonic, or HFS candidate exists. The fixed QC floor is the same effective
        // floor used by state detection. This snapshot is never recomputed from, or updated by,
        // detector output, so later reruns cannot create a feedback loop or pool separate sheets.
        let frozenDatasetStateBandProfile = FrozenDatasetStateBandProfile.compute(
            dataset: dataset,
            minimumValidISISec: max(
                bandSettings.minValidISISec,
                qualitySettings.artifactThresholdSec
            )
        )
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
                    var stateSettings = resolvedDetectorSettings.state
                    stateSettings.frozenDatasetStateBandProfile = frozenDatasetStateBandProfile
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
                    var bridgeStateSettings = resolvedBridgeSettings.state
                    bridgeStateSettings.frozenDatasetStateBandProfile = frozenDatasetStateBandProfile
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
        let finalRerunOutputs = performanceRecorder.measureWallStage("dataset_seed_aware_rerun_parallel") {
            parallelMap(count: finalInputResults.count) { index in
                let result = finalInputResults[index]
                guard let train = trainsByID[result.trainID],
                      let resolution = finalResolutionsByID[result.trainID] else {
                    return FinalRerunOutput(result: result, thresholdEvidence: nil)
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
                    var finalStateSettings = resolvedFinalSettings.state
                    finalStateSettings.frozenDatasetStateBandProfile = frozenDatasetStateBandProfile
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
                    let baseSeedAwareCandidates = taggingManualThresholdProvenance(
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
                    let seedAwareCandidates = reconcilingPositiveTinyISIHypotheses(
                        train: train,
                        candidates: baseSeedAwareCandidates,
                        resolution: resolution,
                        detectorSettings: finalDetectorSettings,
                        pauseSettings: finalPauseSettings,
                        stateSettings: finalStateSettings,
                        resolvedManualThresholds: resolvedFinalSettings.resolved,
                        manualThresholdScope: manualThresholdScope,
                        useAdaptiveV2Canonicalization: useAdaptiveV2Canonicalization,
                        minimumValidISISec: bandSettings.minValidISISec
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
                        hfsBurstArbitrationAuditRows:
                            PositiveTinyISIHypothesisResolver.reconcilingHFSBurstAuditRows(
                                finalPhase1BResolution.hfsBurstArbitrationAuditRows,
                                train: train,
                                finalCandidates: seedAwareCandidates,
                                artifactThresholdSec: finalDetectorSettings.minValidISISec,
                                settings: HFSBurstArbitrationAuditSettings().fillingMissingFallbacks(
                                    pauseLikeThresholdSec:
                                        finalStateSettings.highFrequencySpikingPauseBreakSec ??
                                        finalStateSettings.highFrequencySpikingToleratedGapSec,
                                    burstSeedUpperSec: finalStateSettings.burstSeedUpperSec,
                                    burstBridgeUpperSec:
                                        finalStateSettings.highFrequencySpikingEpochBridgeSec
                                )
                            )
                    )
                    performanceRecorder.recordTrainResult(finalResult)
                    let thresholdEvidence = ClassicAnchorResolvedThresholdEvidence(
                        trainID: train.id,
                        trainName: train.name,
                        stagePath: "dataset_seed_aware_rerun",
                        effectiveProfile: effectiveResolvedProfile(
                            classic: finalDetectorSettings,
                            state: finalStateSettings,
                            pause: finalPauseSettings
                        ),
                        resolutionProvenance: resolvedFinalSettings.resolved?.provenance ?? [],
                        learnedProvenanceByKey:
                            resolvedFinalSettings.resolved?.learnedProvenanceByKey ?? [:]
                    )
                    return FinalRerunOutput(
                        result: finalResult,
                        thresholdEvidence: thresholdEvidence
                    )
                }
            }
        }
        results = finalRerunOutputs.map(\.result)
        let resolvedThresholdEvidence = finalRerunOutputs.compactMap(\.thresholdEvidence)
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

        return ClassicAnchorDetectionRun.authoritativeDetectorRun(
            bandSettings: bandSettings,
            qualitySettings: qualitySettings,
            resolutions: resolutions,
            results: results,
            datasetStructuralSeedSummary: finalDatasetStructuralSeedSummary,
            datasetRerunProvenance: datasetRerunProvenance,
            performanceReport: performanceRecorder.makeReport(results: results),
            datasetISIDistribution: datasetISIDistribution,
            invocationSettingsSnapshot: settingsSnapshot,
            datasetMetadataSnapshot: datasetMetadataSnapshot,
            resolvedThresholdEvidence: resolvedThresholdEvidence,
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

    private struct MappedPositiveTinyISICandidate {
        let scenario: PositiveTinyISIJointHypothesisScenario
        let candidate: ClassicAnchorCandidate
        let originalSpan: ClosedRange<Int>

        var signature: String {
            [
                candidate.finalLabel.rawValue,
                String(originalSpan.lowerBound),
                String(originalSpan.upperBound),
                candidate.stateTonicSubtype ?? "",
                candidate.stateHighFrequencySubtype ?? "",
                candidate.pauseBoundaryRole?.rawValue ?? "",
                ClassicAnchorDetectionPipeline.positiveTinyMappedBurstCoreGeometry(
                    scenario: scenario,
                    candidate: candidate
                ),
            ].joined(separator: "|")
        }
    }

    /// Resolves an isolated positive sub-floor ISI without deleting an ISI value or guessing which
    /// boundary spike is wrong. Both exact virtual spike-removal hypotheses are classified under the
    /// already-frozen train/dataset thresholds. Only label-and-original-geometry consensus is allowed
    /// to become authoritative; disagreement, unsafe topology, and missing evidence stay audit-only.
    /// No hypothesis result is fed back into band estimation.
    private static func reconcilingPositiveTinyISIHypotheses(
        train: SpikeTrain,
        candidates: [ClassicAnchorCandidate],
        resolution: TrainAdaptiveBandResolution,
        detectorSettings: ClassicAnchorSettings,
        pauseSettings: PauseDetectorSettings,
        stateSettings: StatePatternDetectorSettings,
        resolvedManualThresholds: ResolvedThresholdProfile?,
        manualThresholdScope: ManualThresholdScope,
        useAdaptiveV2Canonicalization: Bool,
        minimumValidISISec: Double
    ) -> [ClassicAnchorCandidate] {
        let plans = PositiveTinyISIHypothesisResolver.plans(
            train: train,
            artifactThresholdSec: detectorSettings.minValidISISec
        )
        guard !plans.isEmpty else { return candidates }

        var pool = candidates
        let unsafePlans = plans.filter { $0.status != .eligibleForExactHypotheses }
        guard unsafePlans.isEmpty else {
            pool = suppressingCandidatesAffectedByPositiveTinyISIReview(
                pool,
                train: train,
                plans: plans,
                reason: "unsafe_positive_tiny_isi_topology"
            )
            pool.append(
                positiveTinyISIReviewCandidate(
                    train: train,
                    plans: plans,
                    evidence: [],
                    reason: unsafePlans.compactMap(\.reviewReason).sorted().joined(separator: ",")
                )
            )
            return ClassicAnchorCandidateArbitrator.arbitrateBySemanticTrack(
                uniqueCandidatesByID(pool)
            )
        }

        let scenarios = PositiveTinyISIHypothesisResolver.jointScenarios(
            train: train,
            plans: plans
        )
        let expectedScenarioCount = 1 << plans.count
        guard scenarios.count == expectedScenarioCount else {
            pool = suppressingCandidatesAffectedByPositiveTinyISIReview(
                pool,
                train: train,
                plans: plans,
                reason: "joint_hypothesis_construction_failed"
            )
            pool.append(
                positiveTinyISIReviewCandidate(
                    train: train,
                    plans: plans,
                    evidence: [],
                    reason: "joint_hypothesis_construction_failed"
                )
            )
            return ClassicAnchorCandidateArbitrator.arbitrateBySemanticTrack(
                uniqueCandidatesByID(pool)
            )
        }

        let evidenceByScenario = scenarios.map { scenario in
            let evidence: [MappedPositiveTinyISICandidate] = {
                let hypothesisCandidates = positiveTinyISIHypothesisCandidates(
                    train: scenario.virtualTrain,
                    resolution: resolution,
                    detectorSettings: detectorSettings,
                    pauseSettings: pauseSettings,
                    stateSettings: stateSettings,
                    resolvedManualThresholds: resolvedManualThresholds,
                    manualThresholdScope: manualThresholdScope,
                    useAdaptiveV2Canonicalization: useAdaptiveV2Canonicalization,
                    minimumValidISISec: minimumValidISISec
                )
                return hypothesisCandidates.compactMap { candidate in
                    guard candidate.isEligibleForAutoSelection,
                          scenario.mergedVirtualISIIndicesByOriginalISIIndex.values.contains(where: {
                            candidate.startISIIndex <= $0 && $0 <= candidate.endISIIndex
                          }),
                          let span = scenario.originalISISpan(
                            forVirtualStart: candidate.startISIIndex,
                            end: candidate.endISIIndex
                          ),
                          positiveTinyMappedBurstCoreGeometry(
                            scenario: scenario,
                            candidate: candidate
                          ) != "invalid" else {
                        return nil
                    }
                    return MappedPositiveTinyISICandidate(
                        scenario: scenario,
                        candidate: candidate,
                        originalSpan: span
                    )
                }
            }()
            return (scenario: scenario, evidence: evidence)
        }
        let allEvidence = evidenceByScenario.flatMap(\.evidence)
        let signatures = Set(allEvidence.map(\.signature))
        let agreements = signatures.compactMap { signature -> [MappedPositiveTinyISICandidate]? in
            let selected = evidenceByScenario.compactMap { entry in
                entry.evidence
                    .filter { $0.signature == signature }
                    .sorted {
                        if $0.candidate.score != $1.candidate.score {
                            return $0.candidate.score > $1.candidate.score
                        }
                        return $0.candidate.id < $1.candidate.id
                    }
                    .first
            }
            return selected.count == scenarios.count ? selected : nil
        }
        .sorted {
            let lhs = $0.reduce(0) { $0 + $1.candidate.score }
            let rhs = $1.reduce(0) { $0 + $1.candidate.score }
            if lhs != rhs { return lhs > rhs }
            return ($0.first?.signature ?? "") < ($1.first?.signature ?? "")
        }

        guard !agreements.isEmpty else {
            pool = suppressingCandidatesAffectedByPositiveTinyISIReview(
                pool,
                train: train,
                plans: plans,
                reason: "hypothesis_label_or_geometry_disagreement"
            )
            pool.append(
                positiveTinyISIReviewCandidate(
                    train: train,
                    plans: plans,
                    evidence: allEvidence,
                    reason: "hypothesis_label_or_geometry_disagreement"
                )
            )
            return ClassicAnchorCandidateArbitrator.arbitrateBySemanticTrack(
                uniqueCandidatesByID(pool)
            )
        }

        // Materialize every joint-hypothesis agreement, then let the ordinary semantic-track
        // arbitrator choose among overlapping alternatives. This preserves independent local
        // consensuses and State/Event overlays instead of forcing one global winning signature.
        let consensuses = agreements.compactMap { agreement -> ClassicAnchorCandidate? in
            guard let span = agreement.first?.originalSpan else { return nil }
            let relevantPlans = plans.filter { span.contains($0.originalISIIndex) }
            guard !relevantPlans.isEmpty else { return nil }
            return positiveTinyISIConsensusCandidate(
                train: train,
                plans: relevantPlans,
                evidence: agreement,
                stateSettings: stateSettings,
                minimumValidISISec: minimumValidISISec
            )
        }
        guard !consensuses.isEmpty else {
            pool = suppressingCandidatesAffectedByPositiveTinyISIReview(
                pool,
                train: train,
                plans: plans,
                reason: "hypothesis_consensus_materialization_failed"
            )
            pool.append(
                positiveTinyISIReviewCandidate(
                    train: train,
                    plans: plans,
                    evidence: allEvidence,
                    reason: "hypothesis_consensus_materialization_failed"
                )
            )
            return ClassicAnchorCandidateArbitrator.arbitrateBySemanticTrack(
                uniqueCandidatesByID(pool)
            )
        }

        // Supersede only the pre-hypothesis candidates. Consensus alternatives must remain
        // mutually independent so that the ordinary track arbitrator, not append order, chooses
        // the best-scoring alternative. In particular, a later lower-scoring consensus must not
        // demote an earlier higher-scoring consensus merely because their spans overlap.
        pool = pool.map { candidate in
            let supersedingConsensusIDs = consensuses.compactMap { consensus -> String? in
                let span = ClosedRange(uncheckedBounds: (
                    lower: min(consensus.startISIIndex, consensus.endISIIndex),
                    upper: max(consensus.startISIIndex, consensus.endISIIndex)
                ))
                guard candidate.isEligibleForAutoSelection,
                      candidate.arbitrationTrack == consensus.arbitrationTrack,
                      candidate.startISIIndex <= span.upperBound,
                      candidate.endISIIndex >= span.lowerBound else {
                    return nil
                }
                return consensus.id
            }
            guard !supersedingConsensusIDs.isEmpty else { return candidate }
            return candidate.withDiagnosticOverride(
                gateStatus: "superseded_by_positive_tiny_isi_consensus",
                decisionPath: appendDecisionTokens(
                    candidate.decisionPath,
                    supersedingConsensusIDs.sorted().map {
                        "superseded_by_positive_tiny_isi_consensus=\($0)"
                    }
                ),
                action: "audit_only",
                selectedForAuto: false,
                selectionStatus: "superseded_by_positive_tiny_isi_consensus"
            )
        }
        pool.append(contentsOf: consensuses)

        var reconciled = ClassicAnchorCandidateArbitrator.arbitrateBySemanticTrack(
            uniqueCandidatesByID(pool)
        )
        // A merely materialized alternative is not enough: every automatically resolved ambiguity
        // must be covered by a consensus that survived ordinary semantic-track arbitration.
        let selectedConsensuses = reconciled.filter {
            $0.selectedForAuto &&
                $0.isEligibleForAutoSelection &&
                $0.gateStatus == "positive_tiny_isi_hypothesis_consensus"
        }
        let coveredAmbiguityIndices = Set(selectedConsensuses.flatMap { consensus in
            plans.compactMap { plan in
                min(consensus.startISIIndex, consensus.endISIIndex) <= plan.originalISIIndex &&
                    plan.originalISIIndex <= max(consensus.startISIIndex, consensus.endISIIndex)
                    ? plan.originalISIIndex
                    : nil
            }
        })
        let unresolvedPlans = plans.filter { !coveredAmbiguityIndices.contains($0.originalISIIndex) }
        if !unresolvedPlans.isEmpty {
            let unresolvedSet = Set(unresolvedPlans.map(\.originalISIIndex))
            reconciled = suppressingCandidatesAffectedByPositiveTinyISIReview(
                reconciled,
                train: train,
                plans: unresolvedPlans,
                reason: "hypothesis_consensus_not_selected"
            )
            reconciled.append(
                positiveTinyISIReviewCandidate(
                    train: train,
                    plans: unresolvedPlans,
                    evidence: allEvidence.filter { evidence in
                        unresolvedSet.contains(where: { evidence.originalSpan.contains($0) })
                    },
                    reason: "hypothesis_label_or_geometry_disagreement"
                )
            )
            return ClassicAnchorCandidateArbitrator.arbitrateBySemanticTrack(
                uniqueCandidatesByID(reconciled)
            )
        }
        return reconciled
    }

    /// A review record must also remove automatic authority from every pre-hypothesis candidate
    /// whose support can change under the unresolved spike-removal choice. For ambiguity `i`,
    /// H_earlier can merge original ISIs `i-1...i` and H_later can merge `i...i+1`; therefore the
    /// conservative affected footprint is `i-1...i+1`, clipped to the train. Consensus candidates
    /// are excluded because they have already been evaluated under every safe joint hypothesis.
    private static func suppressingCandidatesAffectedByPositiveTinyISIReview(
        _ candidates: [ClassicAnchorCandidate],
        train: SpikeTrain,
        plans: [PositiveTinyISIHypothesisPlan],
        reason: String
    ) -> [ClassicAnchorCandidate] {
        let lastISIIndex = max(1, train.spikeCount - 1)
        let footprints = plans.map { plan in
            max(1, plan.originalISIIndex - 1)...min(lastISIIndex, plan.originalISIIndex + 1)
        }
        let reviewIndices = plans.map(\.originalISIIndex).sorted()
        return candidates.map { candidate in
            let lower = min(candidate.startISIIndex, candidate.endISIIndex)
            let upper = max(candidate.startISIIndex, candidate.endISIIndex)
            let isAffected = footprints.contains { footprint in
                max(lower, footprint.lowerBound) <= min(upper, footprint.upperBound)
            }
            guard candidate.trainID == train.id,
                  candidate.isEligibleForAutoSelection,
                  candidate.arbitrationTrack != nil,
                  candidate.gateStatus != "positive_tiny_isi_hypothesis_consensus",
                  isAffected else {
                return candidate
            }
            return candidate.withDiagnosticOverride(
                gateStatus: "positive_tiny_isi_hypothesis_review_required",
                decisionPath: appendDecisionTokens(
                    candidate.decisionPath,
                    [
                        "positive_tiny_isi_authority_blocked_by_review=true",
                        "positive_tiny_isi_review_indices=\(reviewIndices.map(String.init).joined(separator: ","))",
                        "positive_tiny_isi_review_reason=\(reason)",
                    ]
                ),
                action: "audit_only",
                selectedForAuto: false,
                selectionStatus: "not_selected__positive_tiny_isi_review_required"
            )
        }
    }

    /// Runs only the train-local final candidate layer with the already-frozen resolution/settings.
    /// This avoids a dataset rerun, re-estimation loop, or combinatorial search while keeping each
    /// H_earlier/H_later classification on the same scientific thresholds as the authoritative run.
    private static func positiveTinyISIHypothesisCandidates(
        train: SpikeTrain,
        resolution: TrainAdaptiveBandResolution,
        detectorSettings: ClassicAnchorSettings,
        pauseSettings: PauseDetectorSettings,
        stateSettings: StatePatternDetectorSettings,
        resolvedManualThresholds: ResolvedThresholdProfile?,
        manualThresholdScope: ManualThresholdScope,
        useAdaptiveV2Canonicalization: Bool,
        minimumValidISISec: Double
    ) -> [ClassicAnchorCandidate] {
        let burstResult = ClassicAnchorDetector.detect(train: train, settings: detectorSettings)
        let burstSeedCandidates = tagPipelineStage(
            BurstSeedRunAssembler.detect(
                train: train,
                candidates: burstResult.candidates,
                resolution: resolution,
                settings: detectorSettings
            ),
            "positive_tiny_isi_hypothesis_burst_seed_run"
        )
        let pauseResult = PauseDetector.detect(train: train, settings: pauseSettings)
        let stateResult = StatePatternDetector.detect(train: train, settings: stateSettings)
        let input = uniqueCandidatesByID(
            tagPipelineStage(burstResult.candidates, "positive_tiny_isi_hypothesis_burst") +
                burstSeedCandidates +
                tagPipelineStage(pauseResult.candidates, "positive_tiny_isi_hypothesis_pause") +
                tagPipelineStage(stateResult.candidates, "positive_tiny_isi_hypothesis_state")
        )
        let preliminary = ClassicAnchorCandidateArbitrator.arbitrateBySemanticTrack(input)
        let flankPauses = tagPipelineStage(
            ClassicBurstFlankPauseDetector.detect(
                train: train,
                candidates: preliminary,
                settings: pauseSettings,
                existingCandidates: input
            ),
            "positive_tiny_isi_hypothesis_flank_pause"
        )
        let phase1B = MultiTrackPhase1BResolver.resolveWithAudit(
            train: train,
            candidates: input + flankPauses,
            pauseSettings: pauseSettings,
            stateSettings: stateSettings,
            stagePrefix: "positive_tiny_isi_hypothesis"
        )
        let finalized = applyingAdaptiveV2BurstCanonicalization(
            demotingTonicRateRegularBurstsAndRearbitrating(
                enforcingBurstHardGateAndRearbitrating(
                    ClassicAnchorDetector.boundaryNormalizedAdaptiveHFBurstPackets(
                        phase1B.candidates,
                        train: train,
                        settings: detectorSettings
                    ),
                    settings: detectorSettings
                ),
                settings: detectorSettings,
                manualBurstGateActive: hasUserBurstGate(resolvedManualThresholds)
            ),
            enabled: useAdaptiveV2Canonicalization,
            train: train,
            classic: detectorSettings,
            state: stateSettings,
            pause: pauseSettings,
            manualBurstGateActive: hasUserBurstGate(resolvedManualThresholds),
            minValidISISec: minimumValidISISec
        )
        return taggingManualThresholdProvenance(
            tagPipelineStage(finalized, "positive_tiny_isi_hypothesis_final"),
            resolved: resolvedManualThresholds,
            scope: manualThresholdScope,
            trainID: train.id
        )
    }

    private static func positiveTinyISIConsensusCandidate(
        train: SpikeTrain,
        plans: [PositiveTinyISIHypothesisPlan],
        evidence: [MappedPositiveTinyISICandidate],
        stateSettings: StatePatternDetectorSettings,
        minimumValidISISec: Double
    ) -> ClassicAnchorCandidate? {
        guard !evidence.isEmpty else { return nil }
        let orderedEvidence = evidence.sorted { $0.scenario.key < $1.scenario.key }
        guard let representative = orderedEvidence.first,
              let maximumScore = orderedEvidence.map(\.candidate.score).max(),
              maximumScore.isFinite,
              let maximumPriority = orderedEvidence.map(\.candidate.priority).max() else {
            return nil
        }
        let source = representative.candidate
        let span = representative.originalSpan
        let ambiguityIndices = Set(plans.map(\.originalISIIndex))
        let nISI = span.upperBound - span.lowerBound + 1
        let burstCoreGeometryTokenValue = orderedEvidence
            .map {
                positiveTinyMappedBurstCoreGeometry(
                    scenario: $0.scenario,
                    candidate: $0.candidate
                )
            }
            .joined(separator: "|")
        var tokens = [
            "positive_tiny_isi_hypothesis=consensus",
            "positive_tiny_isi_indices=\(plans.map(\.originalISIIndex).sorted().map(String.init).joined(separator: ","))",
            "positive_tiny_isi_seconds=\(plans.sorted { $0.originalISIIndex < $1.originalISIIndex }.map { formatHypothesisNumber($0.valueSec) }.joined(separator: ","))",
            "hypothesis_scenario_count=\(orderedEvidence.count)",
            "hypothesis_assignments=\(orderedEvidence.map { $0.scenario.key }.joined(separator: ","))",
            "hypothesis_labels=\(orderedEvidence.map { $0.candidate.finalLabel.rawValue }.joined(separator: "|"))",
            "hypothesis_original_geometry=\(orderedEvidence.map { "\($0.originalSpan.lowerBound)-\($0.originalSpan.upperBound)" }.joined(separator: "|"))",
            "hypothesis_burst_core_geometry=\(burstCoreGeometryTokenValue)",
            metricRangeToken("hypothesis_cv_range", orderedEvidence.map { $0.candidate.cv }),
            metricRangeToken("hypothesis_cv2_range", orderedEvidence.map { $0.candidate.cv2 }),
            metricRangeToken("hypothesis_lv_range", orderedEvidence.map { $0.candidate.lv }),
            "hypothesis_thresholds_frozen=true",
            "hypothesis_feedback_to_bands=false",
        ]

        var stateDirectSpans: [ISISpan] = []
        var stateInterruptionSpans: [ISISpan] = []
        var stateMetrics: PositiveTinyISIDirectMetrics?
        if source.arbitrationTrack == .state {
            let supportSets = orderedEvidence.map { mappedStateDirectSupportIndices($0) }
            var directIndices = supportSets.dropFirst().reduce(supportSets.first ?? Set<Int>()) {
                $0.intersection($1)
            }
            directIndices.subtract(ambiguityIndices)
            directIndices = Set(directIndices.filter { index in
                guard span.contains(index),
                      train.isiSec.indices.contains(index),
                      let value = train.isiSec[index] else { return false }
                return value.isFinite && value >= minimumValidISISec
            })

            switch source.finalLabel {
            case .tonic, .highFrequencyTonic:
                // Each virtual hypothesis has already passed its own final state-support authority,
                // but their intersection can be smaller after mapping back to untouched raw geometry.
                // Reclassify that exact original support once and fail closed if it no longer meets
                // the owner-approved core/deviation or family minimum-count contract.
                let supportSettings = StateSupportClassifierSettings(
                    minimumValidISISec: minimumValidISISec
                )
                let originalSupport = StateSupportClassifier.analyze(
                    span.map { index in
                        StateSupportISIObservation(
                            sourceIndex: index,
                            valueSec: directIndices.contains(index) ? train.isiSec[index] : nil
                        )
                    },
                    settings: supportSettings
                )
                let excludedCount = nISI - directIndices.count
                guard originalSupport.isEligibleForAutomaticTonic(
                    settings: supportSettings,
                    recognizedInterruptionCount: excludedCount
                ), positiveTinyOriginalSupportMeetsFamilyMinimum(
                    label: source.finalLabel,
                    nSupport: originalSupport.nSupport,
                    settings: stateSettings
                ) else {
                    return nil
                }
                directIndices = Set(originalSupport.observations.compactMap { observation in
                    switch observation.classification {
                    case .core, .ordinaryDeviation: return observation.sourceIndex
                    case .invalid, .competingExcursion: return nil
                    }
                })
                tokens += [
                    "positive_tiny_original_state_support_revalidated=true",
                    "positive_tiny_original_state_n_support=\(originalSupport.nSupport)",
                    "positive_tiny_original_state_n_core=\(originalSupport.nCore)",
                    "positive_tiny_original_state_ordinary_deviations=\(originalSupport.ordinaryDeviationCount)",
                ]

            case .highFrequencySpiking:
                let frozenDirectUpper = orderedEvidence.compactMap { mapped -> Double? in
                    if let value = mapped.candidate.hfSpikingShortUpperSec,
                       value.isFinite, value > 0 {
                        return value
                    }
                    let fallback = mapped.candidate.anchorBandUpperSec
                    return fallback.isFinite && fallback > 0 ? fallback : nil
                }.min()
                if let frozenDirectUpper {
                    directIndices = Set(directIndices.filter { index in
                        guard let value = train.isiSec[index] else { return false }
                        return value <= frozenDirectUpper + max(1e-12, abs(frozenDirectUpper) * 1e-6)
                    })
                }
                let requiredSpikes = max(
                    3,
                    orderedEvidence.compactMap { $0.candidate.hfSpikingMinSpikesRequired }.max()
                        ?? stateSettings.highFrequencySpikingMinSpikes
                )
                let rawRequiredSupport = ceil(
                    Double(requiredSpikes - 1) * stateSettings.highFrequencySpikingShortFractionMin
                )
                let requiredSupport = !rawRequiredSupport.isFinite || rawRequiredSupport >= Double(Int.max)
                    ? Int.max
                    : max(1, Int(rawRequiredSupport))
                let duration = originalDuration(train: train, span: span)
                let durationPass = duration.map { value in
                    value >= stateSettings.highFrequencySpikingMinDurationSec -
                        max(1e-12, abs(stateSettings.highFrequencySpikingMinDurationSec) * 1e-6)
                } ?? false
                guard directIndices.count >= requiredSupport,
                      durationPass else {
                    return nil
                }
                tokens += [
                    "positive_tiny_original_hfs_support_revalidated=true",
                    "positive_tiny_original_hfs_direct_support=\(directIndices.count)",
                    "positive_tiny_original_hfs_direct_support_required=\(requiredSupport)",
                ]

            default:
                return nil
            }
            stateDirectSpans = contiguousISISpans(indices: directIndices, trainID: train.id)
            stateInterruptionSpans = contiguousISISpans(
                indices: Set(span).subtracting(directIndices),
                trainID: train.id
            )
            stateMetrics = positiveTinyISIDirectMetrics(
                train: train,
                spans: stateDirectSpans,
                minimumValidISISec: minimumValidISISec
            )
            tokens += [
                "state_metrics_scope=original_direct_support_after_positive_tiny_isi",
                "state_direct_support_intersection_across_hypotheses=true",
                "state_cv2_lv_cross_positive_tiny_isi=false",
            ]
        }

        let decisionPath = appendDecisionTokens(source.decisionPath, tokens)
        let commonDuration = originalDuration(train: train, span: span)
        let commonCV = commonMetric(orderedEvidence.map { $0.candidate.cv })
        let commonCV2 = commonMetric(orderedEvidence.map { $0.candidate.cv2 })
        let commonLV = commonMetric(orderedEvidence.map { $0.candidate.lv })
        // Every semantic field used by the cross-hypothesis agreement signature must also
        // participate in the transient consensus ID. Otherwise two valid agreements with the
        // same label/geometry but different Pause boundary authority could overwrite one another
        // in `uniqueCandidatesByID` before ordinary arbitration sees them.
        let subtypeIdentity = [
            source.stateTonicSubtype,
            source.stateHighFrequencySubtype,
            source.pauseBoundaryRole?.rawValue,
        ]
            .compactMap { $0 }
            .joined(separator: "-")
        let subtypeIDSuffix = subtypeIdentity.isEmpty ? "" : "-\(subtypeIdentity)"
        let elevatedScore = maximumScore + 1e-9
        let consensusScore = elevatedScore.isFinite ? elevatedScore : maximumScore
        var candidate = source.withGeometry(
            idOverride: "\(train.id)-positive-tiny-consensus-\(plans.map(\.originalISIIndex).sorted().map(String.init).joined(separator: "-"))-\(source.finalLabel.rawValue)-\(span.lowerBound)-\(span.upperBound)\(subtypeIDSuffix)",
            startISIIndex: span.lowerBound,
            endISIIndex: span.upperBound,
            startSpikeIndex: span.lowerBound,
            endSpikeIndex: span.upperBound + 1,
            nISI: nISI,
            nValidISI: source.arbitrationTrack == .state
                ? stateDirectSpans.reduce(0) { $0 + $1.rawISICount }
                : orderedEvidence.map { $0.candidate.nValidISI }.min() ?? 0,
            nSpikes: nISI + 1,
            durationSec: commonDuration,
            intraQ10Sec: commonMetric(orderedEvidence.map { $0.candidate.intraQ10Sec }),
            intraQ40Sec: commonMetric(orderedEvidence.map { $0.candidate.intraQ40Sec }),
            intraQ50Sec: commonMetric(orderedEvidence.map { $0.candidate.intraQ50Sec }),
            intraQ90Sec: commonMetric(orderedEvidence.map { $0.candidate.intraQ90Sec }),
            intraQ95Sec: commonMetric(orderedEvidence.map { $0.candidate.intraQ95Sec }),
            maxIntraISISec: commonMetric(orderedEvidence.map { $0.candidate.maxIntraISISec }),
            meanIntraISISec: commonMetric(orderedEvidence.map { $0.candidate.meanIntraISISec }),
            cv: source.arbitrationTrack == .state ? stateMetrics?.cv : commonCV,
            lv: source.arbitrationTrack == .state ? stateMetrics?.lv : commonLV,
            preGapSec: commonMetric(orderedEvidence.map { $0.candidate.preGapSec }),
            postGapSec: commonMetric(orderedEvidence.map { $0.candidate.postGapSec }),
            preRatioQ90: commonMetric(orderedEvidence.map { $0.candidate.preRatioQ90 }),
            postRatioQ90: commonMetric(orderedEvidence.map { $0.candidate.postRatioQ90 }),
            edgeContrastMinQ90: commonMetric(orderedEvidence.map { $0.candidate.edgeContrastMinQ90 }),
            edgeContrastGeomQ90: commonMetric(orderedEvidence.map { $0.candidate.edgeContrastGeomQ90 }),
            decisionPath: decisionPath
        )
        candidate.cv2 = source.arbitrationTrack == .state ? stateMetrics?.cv2 : commonCV2
        if candidate.arbitrationTrack == .state {
            candidate.stateDirectSupportSpans = stateDirectSpans
            candidate.stateInterruptionSpans = stateInterruptionSpans
            candidate.stateDirectSupportISICount = stateDirectSpans.reduce(0) { $0 + $1.rawISICount }
            candidate.stateDirectSupportAdjacentPairCount = stateDirectSpans.reduce(0) {
                $0 + max(0, $1.endISIIndex - $1.startISIIndex)
            }
        }
        if let mapped = positiveTinyMappedBurstCoreSpan(
            scenario: representative.scenario,
            candidate: source
        ) {
            candidate.burstSeedRunStartISI = mapped.lowerBound
            candidate.burstSeedRunEndISI = mapped.upperBound
        } else {
            candidate.burstSeedRunStartISI = nil
            candidate.burstSeedRunEndISI = nil
        }
        return candidate.withDiagnosticOverride(
            gateStatus: "positive_tiny_isi_hypothesis_consensus",
            action: "accept",
            score: consensusScore,
            priority: maximumPriority == .max ? .max : maximumPriority + 1,
            selectedForAuto: false,
            selectionStatus: "not_selected"
        )
    }

    static func positiveTinyMappedBurstCoreSpan(
        scenario: PositiveTinyISIJointHypothesisScenario,
        candidate: ClassicAnchorCandidate
    ) -> ClosedRange<Int>? {
        guard candidate.finalLabel.isBurstEventFamily,
              let start = candidate.burstSeedRunStartISI,
              let end = candidate.burstSeedRunEndISI else {
            return nil
        }
        return scenario.originalISISpan(
            forVirtualStart: min(start, end),
            end: max(start, end)
        )
    }

    private static func positiveTinyMappedBurstCoreGeometry(
        scenario: PositiveTinyISIJointHypothesisScenario,
        candidate: ClassicAnchorCandidate
    ) -> String {
        guard candidate.finalLabel.isBurstEventFamily else { return "not_applicable" }
        switch (candidate.burstSeedRunStartISI, candidate.burstSeedRunEndISI) {
        case (nil, nil):
            return "unspecified"
        case (.some, .some):
            guard let mapped = positiveTinyMappedBurstCoreSpan(
                scenario: scenario,
                candidate: candidate
            ) else { return "invalid" }
            return "\(mapped.lowerBound)-\(mapped.upperBound)"
        default:
            return "invalid"
        }
    }

    /// The shared support classifier is authoritative for ordinary Tonic and deliberately permits
    /// 3–4 ISIs when `d == 0`. Reapplying the detector's longer seed-window minimum here would erase
    /// that approved short-structural-support route after positive-tiny-ISI reconciliation. HF-Tonic
    /// keeps its independent minimum because the high-frequency subtype requires stronger separation
    /// from Burst/HFS than ordinary short Tonic.
    static func positiveTinyOriginalSupportMeetsFamilyMinimum(
        label: ClassicAnchorLabel,
        nSupport: Int,
        settings: StatePatternDetectorSettings
    ) -> Bool {
        switch label {
        case .tonic:
            return nSupport >= 3
        case .highFrequencyTonic:
            return nSupport + 1 >= settings.highFrequencyTonicMinSpikes
        default:
            return false
        }
    }

    private static func positiveTinyISIReviewCandidate(
        train: SpikeTrain,
        plans: [PositiveTinyISIHypothesisPlan],
        evidence: [MappedPositiveTinyISICandidate],
        reason: String
    ) -> ClassicAnchorCandidate {
        let orderedPlans = plans.sorted { $0.originalISIIndex < $1.originalISIIndex }
        let indices = orderedPlans.map(\.originalISIIndex)
        let lower = indices.min() ?? 1
        let upper = indices.max() ?? lower
        let labels = evidence.map { $0.candidate.finalLabel.rawValue }.sorted().joined(separator: "|")
        let geometries = evidence.map {
            "\($0.scenario.key):\($0.originalSpan.lowerBound)-\($0.originalSpan.upperBound)"
        }.sorted().joined(separator: "|")
        let tokens = [
            "positive_tiny_isi_hypothesis=review",
            "positive_tiny_isi_indices=\(indices.map(String.init).joined(separator: ","))",
            "positive_tiny_isi_seconds=\(orderedPlans.map { formatHypothesisNumber($0.valueSec) }.joined(separator: ","))",
            "positive_tiny_isi_review_reason=\(reason)",
            "hypothesis_labels=\(labels.isEmpty ? "none" : labels)",
            "hypothesis_original_geometry=\(geometries.isEmpty ? "none" : geometries)",
            "hypothesis_thresholds_frozen=true",
            "hypothesis_feedback_to_bands=false",
        ]
        return ClassicAnchorCandidate(
            id: "\(train.id)-positive-tiny-review-\(indices.map(String.init).joined(separator: "-"))",
            trainID: train.id,
            trainName: train.name,
            candidateLayer: "positive_tiny_isi_hypothesis_review",
            candidateClass: "positive_tiny_isi_ambiguity",
            finalLabel: .reject,
            gateStatus: "positive_tiny_isi_hypothesis_review_required",
            decisionPath: tokens.joined(separator: ";"),
            action: "audit_only",
            score: 0,
            priority: 0,
            selectedForAuto: false,
            selectionStatus: "not_selected",
            startISIIndex: lower,
            endISIIndex: upper,
            startSpikeIndex: lower,
            endSpikeIndex: upper + 1,
            nISI: upper - lower + 1,
            nValidISI: 0,
            nSpikes: upper - lower + 2,
            durationSec: originalDuration(train: train, span: lower...upper),
            intraQ10Sec: nil,
            intraQ40Sec: nil,
            intraQ50Sec: nil,
            intraQ90Sec: nil,
            intraQ95Sec: nil,
            maxIntraISISec: nil,
            meanIntraISISec: nil,
            cv: nil,
            lv: nil,
            preGapSec: finiteISI(train: train, index: lower - 1),
            postGapSec: finiteISI(train: train, index: upper + 1),
            preRatioQ90: nil,
            postRatioQ90: nil,
            edgeContrastMinQ90: nil,
            edgeContrastGeomQ90: nil,
            anchorFamily: "positive_tiny_isi_ambiguity",
            anchorLockLevel: .auditOnly,
            anchorBandLowerSec: 0,
            anchorBandUpperSec: max(0, orderedPlans.map(\.valueSec).max() ?? 0),
            anchorBandSource: .structure,
            anchorContrastMinRequired: 0,
            anchorContrastGeomRequired: 0,
            refractorySuspectCount: 0,
            refractorySuspectAction: nil
        )
    }

    private static func contiguousISISpans(indices: Set<Int>, trainID: String) -> [ISISpan] {
        let sorted = indices.sorted()
        guard var start = sorted.first else { return [] }
        var end = start
        var spans: [ISISpan] = []
        for index in sorted.dropFirst() {
            if index == end + 1 {
                end = index
            } else {
                spans.append(ISISpan(trainID: trainID, startISIIndex: start, endISIIndex: end))
                start = index
                end = index
            }
        }
        spans.append(ISISpan(trainID: trainID, startISIIndex: start, endISIIndex: end))
        return spans
    }

    private struct PositiveTinyISIDirectMetrics {
        let cv: Double?
        let cv2: Double?
        let lv: Double?
    }

    private static func mappedStateDirectSupportIndices(
        _ evidence: MappedPositiveTinyISICandidate
    ) -> Set<Int> {
        let candidate = evidence.candidate
        let virtualSpans = candidate.stateDirectSupportSpans.isEmpty
            ? [ISISpan(
                trainID: candidate.trainID,
                startISIIndex: candidate.startISIIndex,
                endISIIndex: candidate.endISIIndex
            )]
            : candidate.stateDirectSupportSpans
        return Set(virtualSpans.flatMap { span -> [Int] in
            guard let mapped = evidence.scenario.originalISISpan(
                forVirtualStart: span.startISIIndex,
                end: span.endISIIndex
            ) else { return [] }
            return Array(mapped)
        })
    }

    /// Computes the default state-support metrics only from untouched original ISIs. CV is allowed
    /// to pool direct values; CV2/LV are calculated within each contiguous direct-support span and
    /// therefore never create a synthetic adjacency across a positive-tiny ambiguity.
    private static func positiveTinyISIDirectMetrics(
        train: SpikeTrain,
        spans: [ISISpan],
        minimumValidISISec: Double
    ) -> PositiveTinyISIDirectMetrics? {
        let segments = spans.map { span in
            (span.startISIIndex...span.endISIIndex).compactMap { index -> Double? in
                guard train.isiSec.indices.contains(index),
                      let value = train.isiSec[index],
                      value.isFinite,
                      value >= minimumValidISISec else { return nil }
                return value
            }
        }
        let values = segments.flatMap { $0 }
        guard !values.isEmpty else { return nil }
        var cv2Terms: [Double] = []
        var lvTerms: [Double] = []
        for segment in segments {
            for (left, right) in zip(segment, segment.dropFirst()) {
                let denominator = left + right
                guard denominator > 0 else { continue }
                cv2Terms.append(2 * abs(right - left) / denominator)
                lvTerms.append(3 * pow(right - left, 2) / pow(denominator, 2))
            }
        }
        return PositiveTinyISIDirectMetrics(
            cv: STPDStatistics.coefficientOfVariation(values),
            cv2: cv2Terms.isEmpty ? nil : cv2Terms.reduce(0, +) / Double(cv2Terms.count),
            lv: lvTerms.isEmpty ? nil : lvTerms.reduce(0, +) / Double(lvTerms.count)
        )
    }

    private static func originalDuration(
        train: SpikeTrain,
        span: ClosedRange<Int>
    ) -> Double? {
        let earlierTimestampIndex = span.lowerBound - 1
        let laterTimestampIndex = span.upperBound
        guard earlierTimestampIndex >= 0,
              laterTimestampIndex < train.timestampsSec.count else { return nil }
        let duration = train.timestampsSec[laterTimestampIndex] -
            train.timestampsSec[earlierTimestampIndex]
        return duration.isFinite && duration >= 0 ? duration : nil
    }

    private static func finiteISI(train: SpikeTrain, index: Int) -> Double? {
        guard train.isiSec.indices.contains(index),
              let value = train.isiSec[index], value.isFinite else { return nil }
        return value
    }

    private static func commonMetric(_ values: [Double?]) -> Double? {
        guard !values.isEmpty, values.allSatisfy({ $0 == nil }) == false else { return nil }
        let finite = values.compactMap { value -> Double? in
            guard let value, value.isFinite else { return nil }
            return value
        }
        guard finite.count == values.count, let first = finite.first else { return nil }
        return finite.dropFirst().allSatisfy { value in
            let tolerance = max(1e-12, max(abs(first), abs(value)) * 1e-9)
            return abs(first - value) <= tolerance
        } ? first : nil
    }

    private static func metricRangeToken(_ key: String, _ metrics: [Double?]) -> String {
        let values = metrics.compactMap { value -> Double? in
            guard let value, value.isFinite else { return nil }
            return value
        }
        guard let lower = values.min(), let upper = values.max() else {
            return "\(key)=undefined"
        }
        return "\(key)=\(formatHypothesisNumber(lower))..\(formatHypothesisNumber(upper))"
    }

    private static func formatHypothesisNumber(_ value: Double) -> String {
        guard value.isFinite else { return "undefined" }
        return String(format: "%.17g", locale: Locale(identifier: "en_US_POSIX"), value)
    }

    private static func appendDecisionTokens(_ path: String, _ tokens: [String]) -> String {
        var existing = Set(
            path.split(separator: ";").map {
                String($0).trimmingCharacters(in: .whitespacesAndNewlines)
            }
        )
        var parts = path.split(separator: ";").map(String.init)
        for token in tokens where !token.isEmpty && existing.insert(token).inserted {
            parts.append(token)
        }
        return parts.joined(separator: ";")
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
        if profile.pause.isiLower.mode == .hardGate,
           let value = profile.pause.isiLower.valueSec, value.isFinite, value > 0 {
            newPause.manualHardLowerSec = value
        }

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
        let hfsMinSpikesWasLearned =
            profile.learnedProvenanceByKey["hfs.min_spikes"] != nil
        if !hasHardGate(resolved, keys: ["hfs.min_spikes"]) || hfsMinSpikesWasLearned {
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
