import AppKit
import Foundation
import Observation
import STPDCore
import UniformTypeIdentifiers

struct PinnedTonicSensitivity: Equatable {
    let preview: SegmentSensitivityPreview
    let candidateID: String
    let trainName: String
    let startISIIndex: Int
    let endISIIndex: Int
}

/// How the currently installed dataset entered the legacy active-document surface.
///
/// The current preparation-only importer intentionally has no "authoritative" case: it validates
/// a value but cannot activate it yet. Demo and legacy paths may support exploration, but neither
/// can mint a sealed scientific result.
enum ActiveDatasetScientificStanding: Equatable, Sendable {
    case noDataset
    case nonAuthoritativeDemo
    case legacyUnreviewedImport
    /// A Double-seconds display projection of an exact, persisted canonical import. It may drive
    /// plots, per-train QC, and manual exploration, but the exact microsecond canonical dataset
    /// remains the source of truth for manual decisions and result-package sealing.
    case canonicalConfirmedExploration(datasetDigest: String)

    var permitsAuthoritativeDetectorArtifactExport: Bool { false }
    var permitsSealedResultExport: Bool { permitsAuthoritativeDetectorArtifactExport }

    var detectorResultPrefix: String {
        switch self {
        case .noDataset:
            return ""
        case .nonAuthoritativeDemo:
            return "演示结果——非权威；封存导出已锁定。"
        case .legacyUnreviewedImport:
            return "尚未审核的旧式导入结果——非权威；封存导出已锁定。"
        case .canonicalConfirmedExploration:
            return "已确认规范数据的探索视图——自动检测结果仍为非权威；封存导出已锁定。"
        }
    }
}

enum ActiveDatasetScientificStandingError: Error, Equatable, LocalizedError, Sendable {
    case canonicalConfirmationRequired
    case authoritativeDetectorArtifactExportRequiresCanonicalConfirmation

    var errorDescription: String? {
        switch self {
        case .canonicalConfirmationRequired:
            return "封存结果导出需要经过规范确认的科学导入。演示数据和旧式 CSV 数据均不具备权威性。"
        case .authoritativeDetectorArtifactExportRequiresCanonicalConfirmation:
            return "检测器 CSV 导出需要经过规范确认的科学导入。演示和旧式 CSV 结果仅供探索，不能作为科学结果导出。"
        }
    }
}

private struct ClassicAnchorAnnotationCache: Sendable {
    let eventAnnotations: [ClassicAnchorEventAnnotation]
    let stateAnnotations: [ClassicAnchorEventAnnotation]
    let selectedTrackAnnotations: [ClassicAnchorEventAnnotation]
    let candidateAuditAnnotations: [ClassicAnchorEventAnnotation]

    static let empty = ClassicAnchorAnnotationCache(
        eventAnnotations: [],
        stateAnnotations: [],
        selectedTrackAnnotations: [],
        candidateAuditAnnotations: []
    )

    init(
        eventAnnotations: [ClassicAnchorEventAnnotation],
        stateAnnotations: [ClassicAnchorEventAnnotation],
        selectedTrackAnnotations: [ClassicAnchorEventAnnotation],
        candidateAuditAnnotations: [ClassicAnchorEventAnnotation]
    ) {
        self.eventAnnotations = eventAnnotations
        self.stateAnnotations = stateAnnotations
        self.selectedTrackAnnotations = selectedTrackAnnotations
        self.candidateAuditAnnotations = candidateAuditAnnotations
    }

    init(dataset: SpikeDataset, run: ClassicAnchorDetectionRun) {
        self.eventAnnotations = run.eventAnnotations(in: dataset, tracks: [.event, .gap, .state])
        self.stateAnnotations = run.eventAnnotations(in: dataset, tracks: [.state])
        self.selectedTrackAnnotations = run.eventAnnotations(
            in: dataset,
            tracks: [.event, .gap, .state, .review]
        )
        self.candidateAuditAnnotations = run.eventAnnotations(
            in: dataset,
            selectedOnly: false,
            tracks: Set(ClassicAnchorSemanticTrack.allCases),
            includeEvidenceOnly: true
        )
    }
}

/// A user-locked interval used only as a visual/manual-threshold reference. It does not influence
/// detection until the reviewer explicitly copies its value into a manual threshold and reruns.
struct ISITimelineReference: Equatable, Hashable {
    var trainID: String
    var trainName: String
    var intervalIndex: Int
    var isiSec: Double
}

enum ISIReferenceThresholdTarget: String, CaseIterable, Identifiable {
    case burstSeedMax, pauseMin, tonicMin, tonicMax, hfTonicMin, hfTonicMax
    var id: String { rawValue }
    var menuTitle: String {
        switch self {
        case .burstSeedMax: return "Burst seed 最大 ISI"
        case .pauseMin: return "Pause 最小 ISI"
        case .tonicMin: return "Tonic 最小 ISI"
        case .tonicMax: return "Tonic 最大 ISI"
        case .hfTonicMin: return "HF tonic 最小 ISI"
        case .hfTonicMax: return "HF tonic 最大 ISI"
        }
    }
}

struct ISIManualThresholdLine: Hashable {
    var isiSec: Double
    var label: String
    var isHardGate: Bool
}

struct ManualLearnedThresholdRollback: Hashable {
    let previousState: ManualThresholdFieldState
    let previousApplyResult: LearnedThresholdApplyResult?
    let previousAppliedProposal: ManualPatternLearningProposal?
    let previousAppliedProposalsByFamily: [String: ManualPatternLearningProposal]
    let appliedState: ManualThresholdFieldState
}

@MainActor
@Observable
final class RasterDocument {
    private static let defaultVisibleTrainCount = 10
    private static let defaultISIVisibleTrainCount = 4
    private static let defaultISIStateSpaceVisibleTrainCount = 1
    private static let defaultNeuralManifoldVisibleTrainCount = 8
    private static let fallbackRasterVisibleWindowSeconds = 5.0

    var dataset: SpikeDataset?
    private(set) var activeDatasetScientificStanding: ActiveDatasetScientificStanding = .noDataset
    let scientificImportCoordinator: ScientificImportCoordinator
    var isScientificImportSheetPresented = false
    var selectedTrainIDs: Set<String> = []
    var isiSelectedTrainIDs: Set<String> = []
    var isiStateSpaceSelectedTrainIDs: Set<String> = []
    var neuralManifoldSelectedTrainIDs: Set<String> = []
    var statusMessage = "尚未加载数据集。"
    var lastErrorMessage: String?
    var rawImportUnit: SpikeTimeUnit = .seconds
    var rawCSVHasHeader = true
    var duplicateTimestampPolicy: DuplicateTimestampPolicy = .errorKeep
    var qcDisplayUnit: QualityDisplayUnit = .milliseconds
    var artifactThresholdMs = 0.9 {
        didSet {
            guard artifactThresholdMs != oldValue else { return }
            // The learning snapshot classifies direct support against this exact boundary. A
            // changed boundary therefore invalidates both an in-flight build and any unapplied
            // preview; it never silently reuses or recomputes the old evidence.
            invalidateManualPatternLearningPreview()
        }
    }
    var refractorySuspectThresholdMs = 1.0
    var artifactThresholdUnit: QualityDisplayUnit = .milliseconds
    var refractorySuspectThresholdUnit: QualityDisplayUnit = .milliseconds
    var requestedVisibleTrainCount = defaultVisibleTrainCount
    var requestedISIVisibleTrainCount = defaultISIVisibleTrainCount
    var requestedISIStateSpaceVisibleTrainCount = defaultISIStateSpaceVisibleTrainCount
    var requestedNeuralManifoldVisibleTrainCount = defaultNeuralManifoldVisibleTrainCount
    var neuralManifoldBinMs = 50.0
    var neuralManifoldTimeOrigin: NeuralPopulationTimeOrigin = .raw
    var neuralManifoldTransform: NeuralPopulationTransform = .sqrtCount
    var neuralManifoldScaling: NeuralPopulationScaling = .zscore
    var neuralManifoldSmoothingSigmaBins = 1.0
    var neuralManifoldXAxis: NeuralManifoldPCAAxis = .nm1
    var neuralManifoldYAxis: NeuralManifoldPCAAxis = .nm2
    var neuralManifoldZAxis: NeuralManifoldPCAAxis = .nm3
    var neuralManifoldDisplayMode: NeuralManifoldDisplayMode = .twoD
    var neuralManifoldMethod: NeuralManifoldMethod = .pca
    var neuralManifoldIsomapNeighbors = 15
    var neuralManifoldMaxEmbeddedBins = 1200
    var neuralManifoldIsomapComponentMode: NeuralManifoldIsomapComponentMode = .largest
    var neuralManifoldDiffusionTime = 3
    var neuralManifoldIsomapState: NeuralManifoldIsomapState = .idle
    var neuralManifoldIsomapGeneration = 0
    var neuralManifoldPhateState: NeuralManifoldPhateState = .idle
    var neuralManifoldPhateGeneration = 0
    var neuralManifoldColorMode: NeuralManifoldColorMode = .time
    var neuralManifoldShowsAxes = true
    var neuralManifoldShowsTrajectoryLine = true
    var neuralManifoldLineStartSec = 0.0
    var neuralManifoldLineEndSec = 0.0
    var neuralManifoldUsesTimeGradient = true
    var rasterVisibleWindowSeconds = fallbackRasterVisibleWindowSeconds
    // Once the reviewer manually sets the visible-window length, the per-candidate
    // auto-fit must stop overriding it so the chosen zoom persists across reviews.
    var rasterVisibleWindowUserLocked = false
    var rasterVisibleWindowUnit: QualityDisplayUnit = .seconds
    var rasterSpikeTickHeightPx = 50
    var isiTimeMode: RasterTimeMode = .aligned
    var isiDisplayUnit: QualityDisplayUnit = .milliseconds
    var isiVisibleWindowSeconds = 5.0
    var isiVisibleWindowUnit: QualityDisplayUnit = .seconds
    var isiYAxisScale: ISIYAxisScale = .linear
    var isiLayoutMode: ISITimelineLayoutMode = .separateAxes
    var isiLockYAxis = false
    var isiTimelineReference: ISITimelineReference?
    var isiTimelineReferenceTolerance: Double = 0.10
    var isiTimelineShowsThresholdLines = true
    var isiStateSpaceTimeMode: RasterTimeMode = .aligned
    var isiStateSpaceLayoutMode: ISIStateSpaceLayoutMode = .singleTrain
    var isiStateSpaceDisplayUnit: QualityDisplayUnit = .milliseconds
    var isiStateSpaceAxisScale: ISIYAxisScale = .log
    var isiStateSpaceHalfWindowK = 3
    /// R-compatible state-space validity floor, in seconds. This is independent of the QC flagging
    /// threshold and is used only by state-space feature construction.
    var isiStateSpaceMinValidISISec = SpikeISIStateSpaceFeatureBuilder.defaultMinValidISISec
    var isiStateSpaceScaling: ISIStateSpaceScaling = .robust
    var isiStateSpaceColorMode: ISIStateSpaceColorMode = .qc
    var isiStateSpaceLabelSource: ISIStateSpaceLabelSource = .auditFinal
    var isiStateSpaceWinsorizeExtremeLogISI = true
    var isiStateSpaceBreakLongISI = true
    var isiStateSpaceBreakThresholdMs = 150.0
    var isiStateSpaceMethod: ISIStateSpaceMethod = .featureAxes
    var isiStateSpaceIsomapNeighbors = 15
    var isiStateSpaceIsomapComponentMode: ISIStateSpaceIsomapComponentMode = .largest
    var isiStateSpacePhasePortraitLag = 1
    var classicAnchorDetectionRun: ClassicAnchorDetectionRun?
    var isResultPackageExporting = false
    var isManualAnnotationImporting = false
    // MARK: Result package readback (Phase 2.2C-B4) — strictly read-only, isolated from the active
    // detector document. Loading a package never touches dataset / run / settings / reviews / manual
    // annotations. See RasterDocument+ResultPackageReadback.swift for the read flow.
    /// The immutable, verified result of the last successful `.stpdresult` read-back, if any.
    var loadedResultPackage: STPDResultPackageReadResult?
    /// A verified detector-independent, complete-manual-review result package. This is isolated from
    /// both the active detector document and `loadedResultPackage`; only one readback kind is shown.
    var loadedCanonicalManualResultPackage: CanonicalManualResultPackageReadResult?
    /// The selected package URL, retained only for display.
    var loadedResultPackageURL: URL?
    /// True while a read is in flight; also used to prevent duplicate read requests.
    var isResultPackageReading = false
    /// The last read failure message (`error.localizedDescription`), cleared on success.
    var resultPackageReadbackErrorMessage: String?
    /// Monotonically increasing token; a detached read whose token is stale (superseded by a newer
    /// request or an explicit clear) is ignored on completion.
    @ObservationIgnored var resultPackageReadRequestToken = 0
    /// Bumps on each SUCCESSFUL load so the UI can navigate to `.eventsOutput`, including on a reload of
    /// the same URL.
    var resultPackageLoadCompletionID = 0
    var focusedClassicAnchorCandidateID: String?
    var classicAnchorFocusRequestID = 0
    private(set) var pinnedISIDiagnostic: PinnedISIDiagnostic?
    private(set) var pinnedISIRequestID = 0
    private(set) var pinnedISIComparison: PinnedISIComparison?
    private var pinnedISIBeforeRerun: PinnedISIDiagnostic?
    /// Pattern-family filter for manual-review navigation. This changes only queue presentation,
    /// never detector candidates or labels.
    var activeReviewChannel: ClassicAnchorReviewChannel = .all
    var classicAnchorReviewStatuses: [String: ClassicAnchorReviewStatus] = [:]
    /// Provenance captured when this app instance authors a candidate review.
    /// Imported or legacy status-only rows have no entry and cannot silently
    /// inherit the identity of the person who later exports them.
    var classicAnchorReviewInputs: [String: STPDCandidateReviewInput] = [:]
    /// Reserved for a future sealed local-authoring workflow. Bare entries are never populated by
    /// CSV import and are rejected by result-package export until that workflow supplies provenance.
    var manualAnnotationsByTrain: [String: [ManualAnnotation]] = [:]
    /// Raster-integrated authoring is an explicit local editing mode. It is detector-independent and
    /// starts disabled so ordinary pan/hover behavior is unchanged until the reviewer opts in.
    var manualAnnotationModeEnabled = false
    var rasterManualAnnotationEditMode: RasterManualAnnotationEditMode = .apply
    var selectedManualLabel: ManualAnnotationLabel = .burst
    var selectedManualClearTrack: ManualAnnotationSemanticTrack = .event
    var selectedManualAnnotationID: UUID?
    var manualISIUndoStack: [ManualISIUndoSnapshot] = []
    /// Exact integer-microsecond source retained only for the canonical manual-review workbench.
    /// It is projected from the persisted confirmed import and never activates the legacy detector.
    var canonicalManualDataset: CanonicalScientificDataset?
    /// Identity-bound manual decisions. Any edit clears `confirmedCanonicalManualLabels`.
    var canonicalManualISILabelDraft: CanonicalManualISILabelDraft?
    /// True only while an identity-bound XLSX draft is decoded and verified off the main actor.
    /// It prevents concurrent replacements of the in-memory manual workbench draft.
    var isCanonicalManualISIDraftImporting = false
    /// A complete explicit human confirmation; still not a written result package by itself.
    var confirmedCanonicalManualLabels: ConfirmedCanonicalManualISILabels?
    /// Atomic identity-bound CSV batches. Their annotation payload is intentionally not mirrored
    /// into `manualAnnotationsByTrain`, so imported evidence cannot outlive its approval receipt.
    var approvedManualAnnotationImports:
        [ManualAnnotationCSVApprovedBatch] = []
    var detectorStatusMessage = "Detector has not run."
    var detectorLastRunDate: Date?
    /// Signature of the exact inputs that produced the displayed detector run. Parameter edits do
    /// not silently masquerade as applied results; the UI compares this with the live settings.
    private var lastDetectionInputsSignature: DetectionInputsSignature?
    /// Restored product default from the reviewed integration snapshot. This remains explicit in
    /// run identity and can be disabled for comparison with the classic canonicalization path.
    var useAdaptiveV2Canonicalization = true
    var detectorHistogramBinWidthMs = 5.0
    var detectorClassicBurstContrastMin = 3.0
    var detectorClassicBurstFlankPauseContrastMin = 5.0
    var detectorClassicBurstMinSpikes = 3
    var detectorClassicBurstMaxSpikes = 9
    var detectorPauseMinISIMs = 0.0
    var detectorTonicMinSpikes = 5
    var detectorTonicCVMax = 0.30
    var detectorTonicCV2Max = 0.30
    var detectorTonicLVMax = 0.35
    var detectorTonicBurstSeedFractionMax = 0.20
    var detectorHighFrequencyTonicMinSpikes = 6
    var detectorHighFrequencyTonicLowTailFractionMax = 0.05
    var detectorHighFrequencyTonicCVMax = 0.30
    var detectorHighFrequencyTonicCV2Max = 0.30
    var detectorHighFrequencyTonicLVMax = 0.35
    var detectorHighFrequencySpikingMinSpikes = 30
    var detectorHighFrequencySpikingShortFractionMin = 0.70
    var detectorHighFrequencySpikingAllowedLargeFraction = 0.25
    var detectorHighFrequencySpikingMaxConsecutiveLargeISI = 3
    // Requested manual thresholds. Zero values are unset; automatic modes are
    // behavior-neutral. The result package records both request and effect.
    var manualBurstMode: ThresholdMode = .automatic
    var manualBurstSeedMaxISIMs = 0.0
    var manualBurstBridgeMaxISIMs = 0.0
    var manualBurstMinSpikes = 0
    var manualHFSMode: ThresholdMode = .automatic
    var manualHFSMinSpikes = 0
    var manualHFSMinDurationMs = 0.0
    var manualHFTonicMode: ThresholdMode = .automatic
    var manualHFTonicMinISIMs = 0.0
    var manualHFTonicMaxISIMs = 0.0
    var manualHFTonicMinSpikes = 0
    var manualTonicMode: ThresholdMode = .automatic
    var manualTonicMinISIMs = 0.0
    var manualTonicMaxISIMs = 0.0
    var manualTonicMinSpikes = 0
    var manualPauseMode: ThresholdMode = .automatic
    var manualPauseMinISIMs = 0.0
    var manualThresholdScopeKind: ManualThresholdScopeKind = .allTrains
    /// Explicitly generated, identity-bound preview. It never changes detector fields by itself.
    var manualPatternLearningProposal: ManualPatternLearningProposal?
    var isManualPatternLearning = false
    var manualPatternLearningErrorMessage: String?
    @ObservationIgnored var manualPatternLearningGeneration = 0
    /// Explicit train-level validation never chooses a split silently. The user selects held-out
    /// trains; every remaining canonical train is calibration evidence for that one comparison.
    var manualLearningHeldOutTrainIDs: Set<String> = []
    var manualLearningHoldoutReport: ManualLearningHoldoutValidationReport?
    var manualLearningHoldoutErrorMessage: String?
    var isManualLearningHoldoutValidating = false
    @ObservationIgnored var manualLearningHoldoutGeneration = 0
    @ObservationIgnored var manualLearningHoldoutConfigurationSnapshot:
        ManualLearningHoldoutValidationConfiguration?
    /// The proposal whose compatible values were last explicitly applied. It is retained even when
    /// later annotation edits invalidate the preview, so a subsequent detector run keeps the exact
    /// provenance of the values currently installed in the parameter fields.
    var appliedManualPatternLearningProposal: ManualPatternLearningProposal?
    /// Per-family sources preserve provenance when later proposals update only a subset of fields.
    var appliedManualPatternLearningProposalsByFamily: [String: ManualPatternLearningProposal] = [:]
    var manualLearnedThresholdRollback: ManualLearnedThresholdRollback?
    var lastLearnedApplyResult: LearnedThresholdApplyResult?
    var isDetectorRunning = false
    private var detectorRunGeneration = 0
    private var classicAnchorAnnotationCache = ClassicAnchorAnnotationCache.empty

    private let sampleFileName = "Grechishnikova_STN_2017_subset"
    private let sampleFileExtension = "csv"
    private let reviewPersistence = ClassicAnchorReviewPersistence()
    let manualAnnotationPersistence = ManualAnnotationPersistence()
    private var loadedCSVURL: URL?
    private var loadedCSVUnit: SpikeTimeUnit = .seconds
    private var loadedCSVHasHeader = true

    init(
        scientificImportCoordinator: ScientificImportCoordinator = ScientificImportCoordinator()
    ) {
        self.scientificImportCoordinator = scientificImportCoordinator
    }

    var qualitySettings: SpikeQualitySettings {
        let artifactSec = max(0, artifactThresholdMs) / 1000
        let refractorySec = max(artifactThresholdMs, refractorySuspectThresholdMs) / 1000
        return SpikeQualitySettings(
            artifactThresholdSec: artifactSec,
            refractorySuspectThresholdSec: refractorySec,
            displayUnit: qcDisplayUnit
        )
    }

    var qualityReport: SpikeDatasetQualityReport? {
        guard let dataset else {
            return nil
        }

        return SpikeQualityAnalyzer.analyze(dataset: dataset, settings: qualitySettings)
    }

    var adaptiveDetectorBandSettings: TrainAdaptiveBandSettings {
        TrainAdaptiveBandSettings(
            minValidISISec: max(qualitySettings.artifactThresholdSec, 1e-9),
            histogramBinWidthSec: max(0.001, detectorHistogramBinWidthMs / 1000)
        )
    }

    var detectorParameterSettings: PatternDetectionParameterSettings {
        PatternDetectionParameterSettings(
            classicBurstContrastMin: detectorClassicBurstContrastMin,
            classicBurstFlankPauseContrastMin: detectorClassicBurstFlankPauseContrastMin,
            classicBurstMinSpikes: detectorClassicBurstMinSpikes,
            classicBurstMaxSpikes: detectorClassicBurstMaxSpikes,
            pauseMinISISecOverride: detectorPauseMinISIMs > 0 ? detectorPauseMinISIMs / 1000 : nil
        )
    }

    func lockISITimelineReference(
        trainID: String,
        trainName: String,
        intervalIndex: Int,
        isiSec: Double
    ) {
        guard isiSec.isFinite, isiSec > 0 else { return }
        isiTimelineReference = ISITimelineReference(
            trainID: trainID,
            trainName: trainName,
            intervalIndex: intervalIndex,
            isiSec: isiSec
        )
    }

    func clearISITimelineReference() {
        isiTimelineReference = nil
    }

    @discardableResult
    func copyISITimelineReferenceToManualThreshold(
        _ target: ISIReferenceThresholdTarget
    ) -> Bool {
        guard let reference = isiTimelineReference,
              reference.isiSec.isFinite,
              reference.isiSec > 0 else { return false }
        let milliseconds = reference.isiSec * 1_000
        switch target {
        case .burstSeedMax:
            manualBurstSeedMaxISIMs = milliseconds
            if manualBurstMode == .automatic { manualBurstMode = .softAnchor }
        case .pauseMin:
            manualPauseMinISIMs = milliseconds
            if manualPauseMode == .automatic { manualPauseMode = .softAnchor }
        case .tonicMin:
            manualTonicMinISIMs = milliseconds
            if manualTonicMode == .automatic { manualTonicMode = .softAnchor }
        case .tonicMax:
            manualTonicMaxISIMs = milliseconds
            if manualTonicMode == .automatic { manualTonicMode = .softAnchor }
        case .hfTonicMin:
            manualHFTonicMinISIMs = milliseconds
            if manualHFTonicMode == .automatic { manualHFTonicMode = .softAnchor }
        case .hfTonicMax:
            manualHFTonicMaxISIMs = milliseconds
            if manualHFTonicMode == .automatic { manualHFTonicMode = .softAnchor }
        }
        return true
    }

    var activeManualISIThresholdLines: [ISIManualThresholdLine] {
        var lines: [ISIManualThresholdLine] = []
        func add(_ milliseconds: Double, _ mode: ThresholdMode, _ label: String) {
            guard mode != .automatic,
                  milliseconds.isFinite,
                  milliseconds > 0 else { return }
            lines.append(
                ISIManualThresholdLine(
                    isiSec: milliseconds / 1_000,
                    label: label,
                    isHardGate: mode == .hardGate
                )
            )
        }
        add(manualBurstSeedMaxISIMs, manualBurstMode, "burst seed ≤")
        add(manualPauseMinISIMs, manualPauseMode, "pause ≥")
        add(manualTonicMinISIMs, manualTonicMode, "tonic min")
        add(manualTonicMaxISIMs, manualTonicMode, "tonic max")
        add(manualHFTonicMinISIMs, manualHFTonicMode, "HF tonic min")
        add(manualHFTonicMaxISIMs, manualHFTonicMode, "HF tonic max")
        return lines
    }

    var stateDetectorTuning: StatePatternDetectorTuning {
        StatePatternDetectorTuning(
            tonicMinSpikes: detectorTonicMinSpikes,
            tonicCVMax: detectorTonicCVMax,
            tonicCV2Max: detectorTonicCV2Max,
            tonicLVMax: detectorTonicLVMax,
            tonicBurstSeedFractionMax: detectorTonicBurstSeedFractionMax,
            highFrequencyTonicMinSpikes: detectorHighFrequencyTonicMinSpikes,
            highFrequencyTonicLowTailFractionMax: detectorHighFrequencyTonicLowTailFractionMax,
            highFrequencyTonicCVMax: detectorHighFrequencyTonicCVMax,
            highFrequencyTonicCV2Max: detectorHighFrequencyTonicCV2Max,
            highFrequencyTonicLVMax: detectorHighFrequencyTonicLVMax,
            highFrequencySpikingMinSpikes: detectorHighFrequencySpikingMinSpikes,
            highFrequencySpikingShortFractionMin: detectorHighFrequencySpikingShortFractionMin,
            highFrequencySpikingAllowedLargeFraction: detectorHighFrequencySpikingAllowedLargeFraction,
            highFrequencySpikingMaxConsecutiveLargeISI: detectorHighFrequencySpikingMaxConsecutiveLargeISI
        )
    }

    var manualThresholdScope: ManualThresholdScope {
        ManualThresholdScope.resolve(
            kind: manualThresholdScopeKind,
            focusedTrainID: focusedClassicAnchorCandidate?.trainID,
            selectedTrainIDs: selectedTrainIDs,
            allTrainIDs: dataset?.trains.map(\.id) ?? []
        )
    }

    /// Converts the UI's millisecond fields to the detector's seconds-internal threshold contract.
    /// Automatic fields are inert; spike-count limits are hard-gate only.
    var manualThresholdProfile: ManualThresholdProfile {
        func isi(_ milliseconds: Double, mode: ThresholdMode) -> ManualISIThreshold {
            mode != .automatic && milliseconds > 0
                ? ManualISIThreshold(mode: mode, valueSec: milliseconds / 1_000)
                : .automatic
        }
        func count(_ value: Int, mode: ThresholdMode) -> ManualSpikeCountThreshold {
            mode == .hardGate && value > 0
                ? ManualSpikeCountThreshold(mode: .hardGate, value: value)
                : .automatic
        }
        var profile = ManualThresholdProfile(
            burst: BurstManualThresholds(
                seedUpperISI: isi(manualBurstSeedMaxISIMs, mode: manualBurstMode),
                bridgeUpperISI: isi(manualBurstBridgeMaxISIMs, mode: manualBurstMode),
                minSpikes: count(manualBurstMinSpikes, mode: manualBurstMode)
            ),
            hfs: HFSManualThresholds(
                minSpikes: count(manualHFSMinSpikes, mode: manualHFSMode),
                minDurationSec: isi(manualHFSMinDurationMs, mode: manualHFSMode)
            ),
            hfTonic: HFTonicManualThresholds(
                minSpikes: count(manualHFTonicMinSpikes, mode: manualHFTonicMode),
                isiFloor: isi(manualHFTonicMinISIMs, mode: manualHFTonicMode),
                isiUpper: isi(manualHFTonicMaxISIMs, mode: manualHFTonicMode)
            ),
            tonic: TonicManualThresholds(
                minSpikes: count(manualTonicMinSpikes, mode: manualTonicMode),
                isiLower: isi(manualTonicMinISIMs, mode: manualTonicMode),
                isiUpper: isi(manualTonicMaxISIMs, mode: manualTonicMode)
            ),
            pause: PauseManualThresholds(
                isiLower: isi(manualPauseMinISIMs, mode: manualPauseMode)
            )
        )
        profile.learnedProvenanceByKey = activeLearnedThresholdProvenanceByKey
        return profile
    }

    var currentDetectionInputsSignature: DetectionInputsSignature? {
        guard let dataset else { return nil }
        return DetectionInputsSignature(
            datasetID: dataset.id.uuidString,
            bandSettings: adaptiveDetectorBandSettings,
            qualitySettings: qualitySettings,
            stateTuning: stateDetectorTuning,
            detectorParameters: detectorParameterSettings,
            manualThresholdProfile: manualThresholdProfile,
            useAdaptiveV2Canonicalization: useAdaptiveV2Canonicalization,
            manualThresholdScope: manualThresholdScope
        )
    }

    var detectionResultsAreStale: Bool {
        guard hasDetectorResults, let lastDetectionInputsSignature else { return false }
        return currentDetectionInputsSignature != lastDetectionInputsSignature
    }

    var appliedDuplicateTimestampPolicy: DuplicateTimestampPolicy? {
        guard let dataset, let firstPolicy = dataset.trains.first?.duplicateTimestampPolicy else {
            return nil
        }

        let allPolicies = Set(dataset.trains.map(\.duplicateTimestampPolicy))
        return allPolicies.count == 1 ? firstPolicy : nil
    }

    var hasPendingDuplicateTimestampPolicy: Bool {
        guard dataset != nil, let appliedDuplicateTimestampPolicy else {
            return false
        }

        return duplicateTimestampPolicy != appliedDuplicateTimestampPolicy
    }

    var hasDetectorResults: Bool {
        classicAnchorDetectionRun != nil
    }

    var canExportClassicAnchorEventsCSV: Bool {
        activeDatasetScientificStanding.permitsAuthoritativeDetectorArtifactExport
            && hasDetectorResults
    }

    var hasHFSBurstArbitrationAuditRows: Bool {
        guard let classicAnchorDetectionRun else {
            return false
        }
        return !classicAnchorDetectionRun.hfsBurstArbitrationAuditRows.isEmpty
    }

    var canExportHFSBurstArbitrationAuditCSV: Bool {
        activeDatasetScientificStanding.permitsAuthoritativeDetectorArtifactExport
            && hasHFSBurstArbitrationAuditRows
    }

    var classicAnchorEventAnnotations: [ClassicAnchorEventAnnotation] {
        classicAnchorAnnotationCache.eventAnnotations.filter { isManuallyRejected($0) == false }
    }

    /// Unfiltered automatic annotations used only for audit/export. A rejected candidate must remain
    /// visible in `auto_pattern`; rejection changes the reviewed/final projection, not detector history.
    var classicAnchorRawEventAnnotations: [ClassicAnchorEventAnnotation] {
        classicAnchorAnnotationCache.eventAnnotations
    }

    var taskEvents: [TaskEvent] { dataset?.taskEvents ?? [] }

    /// Public event layer after local manual locks/vetoes. The detector's raw annotations remain
    /// untouched; this projection is presentation/review evidence only.
    var classicAnchorPublicEventAnnotations: [ClassicAnchorEventAnnotation] {
        let base = classicAnchorEventAnnotations
        guard let dataset, !manualAnnotationsByTrain.isEmpty else { return base }
        var vetoed: [String: Set<Int>] = [:]
        var locked: [String: Set<Int>] = [:]
        var manualBurst: [String: Set<Int>] = [:]
        for train in dataset.trains {
            guard let projection = manualAnnotationProjection(forTrainID: train.id),
                  projection.hasManualEffect else { continue }
            if !projection.autoBurstBlockedByVetoISIs.isEmpty {
                vetoed[train.id] = projection.autoBurstBlockedByVetoISIs
            }
            if !projection.autoBlockedByManualLockISIs.isEmpty {
                locked[train.id] = projection.autoBlockedByManualLockISIs
            }
            let support = Set(projection.manualPositiveLabelByISI.compactMap { index, label in
                ManualAnnotationProjector.burstFamilyLabels.contains(label) ? index : nil
            })
            if !support.isEmpty { manualBurst[train.id] = support }
        }
        let trains = Dictionary(uniqueKeysWithValues: dataset.trains.map { ($0.id, $0) })
        return ManualAnnotationProjector.projectPublicEventAnnotations(
            base,
            vetoedBurstISIsByTrain: vetoed,
            lockSuppressedISIsByTrain: locked,
            validatedManualBurstSupportISIsByTrain: manualBurst,
            trainsByID: trains
        ).annotations
    }

    var classicAnchorStateAnnotations: [ClassicAnchorEventAnnotation] {
        classicAnchorAnnotationCache.stateAnnotations.filter { isManuallyRejected($0) == false }
    }

    var classicAnchorSelectedTrackAnnotations: [ClassicAnchorEventAnnotation] {
        classicAnchorAnnotationCache.selectedTrackAnnotations.filter { isManuallyRejected($0) == false }
    }

    var classicAnchorCandidateAuditAnnotations: [ClassicAnchorEventAnnotation] {
        classicAnchorAnnotationCache.candidateAuditAnnotations
    }

    var focusedClassicAnchorEventAnnotation: ClassicAnchorEventAnnotation? {
        guard let focusedClassicAnchorCandidateID else {
            return nil
        }
        return classicAnchorCandidateAuditAnnotations.first { $0.candidateID == focusedClassicAnchorCandidateID }
    }

    var focusedClassicAnchorCandidate: ClassicAnchorCandidate? {
        guard let focusedClassicAnchorCandidateID else {
            return nil
        }
        return classicAnchorDetectionRun?.candidates.first { $0.id == focusedClassicAnchorCandidateID }
    }

    private func isManuallyRejected(_ annotation: ClassicAnchorEventAnnotation) -> Bool {
        reviewStatus(for: annotation.candidateID) == .rejected
    }

    func loadBundledSampleIfNeeded() {
        guard dataset == nil else {
            return
        }
        loadBundledSample()
    }

    func loadBundledSample() {
        lastErrorMessage = nil

        guard let url = bundledSampleURL() else {
            lastErrorMessage = "Could not locate the bundled sample CSV."
            statusMessage = "Sample load failed."
            return
        }

        loadCSV(
            from: url,
            unit: .seconds,
            hasHeader: true,
            duplicatePolicy: duplicateTimestampPolicy,
            scientificStanding: .nonAuthoritativeDemo
        )
    }

    func openScientificImportWithPanel() {
        let panel = NSOpenPanel()
        panel.allowsMultipleSelection = false
        panel.canChooseDirectories = false
        var supportedContentTypes: [UTType] = [.commaSeparatedText]
        if let xlsx = UTType(filenameExtension: "xlsx") {
            supportedContentTypes.append(xlsx)
        }
        if let nex = UTType(filenameExtension: "nex", conformingTo: .data) {
            supportedContentTypes.append(nex)
        }
        panel.allowedContentTypes = supportedContentTypes
        panel.message = "请选择 CSV、XLSX 时间戳表格或 NeuroExplorer NEX 文件。CSV/XLSX 进入规范导入向导；NEX 直接进入浏览与手工标记。"

        guard panel.runModal() == .OK, let url = panel.url else {
            return
        }

        if url.pathExtension.lowercased() == "nex" {
            loadNEX(from: url, duplicatePolicy: duplicateTimestampPolicy)
        } else {
            isScientificImportSheetPresented = true
            Task { await scientificImportCoordinator.beginImport(from: url) }
        }
    }

    /// Compatibility entry point for older callers. All user-facing raw-data imports now enter the
    /// exact-snapshot two-stage review instead of installing a legacy CSV immediately.
    func openCSVWithPanel() {
        openScientificImportWithPanel()
    }

    func dismissScientificImportReview() {
        isScientificImportSheetPresented = false
    }

    /// Installs a plotting/exploration projection of the exact canonical dataset. The conversion to
    /// Double seconds is deliberately downstream of canonical identity and never feeds canonical
    /// review or sealed manual-result construction. Exact duplicate ticks remain repeated spikes.
    func installCanonicalDatasetForExploration(
        _ canonical: CanonicalScientificDataset,
        fingerprint: CanonicalScientificDatasetFingerprint
    ) {
        let segmentID = canonical.recordingSegment.semanticID.semanticID.canonicalText
        let sourceDescription = "规范导入 · SHA-256 \(fingerprint.datasetDigest.prefix(12))…"
        let trains = canonical.spikeTrains.map { train in
            SpikeTrain(
                name: train.semanticID.semanticID.canonicalText,
                timestampsSec: train.rawTimestamps.map {
                    Double($0.microseconds) / 1_000_000
                },
                duplicateTimestampPolicy: .errorKeep
            )
        }
        let taskEvents = canonical.eventScopeGroups.flatMap { group in
            let groupID = group.semanticID.semanticID.canonicalText
            return group.eventDefinitions.flatMap { definition in
                let definitionID = definition.semanticID.semanticID.canonicalText
                return definition.occurrences.enumerated().map { index, occurrence in
                    TaskEvent(
                        id: "\(groupID):\(definitionID):\(index + 1)",
                        name: definitionID,
                        timeSec: Double(occurrence.tick.microseconds) / 1_000_000,
                        column: definitionID,
                        eventIndex: index + 1,
                        trialID: segmentID,
                        source: sourceDescription
                    )
                }
            }
        }
        let displayDataset = SpikeDataset(
            name: segmentID,
            sourceDescription: sourceDescription,
            trains: trains,
            taskEvents: taskEvents
        )
        installDataset(
            displayDataset,
            sourceURL: nil,
            unit: .seconds,
            hasHeader: true,
            duplicatePolicy: .errorKeep,
            preserveSelection: false,
            scientificStanding: .canonicalConfirmedExploration(
                datasetDigest: fingerprint.datasetDigest
            )
        )
    }

    func applyDuplicateTimestampPolicy(_ policy: DuplicateTimestampPolicy) {
        duplicateTimestampPolicy = policy

        guard let dataset else {
            return
        }

        if let loadedCSVURL {
            do {
                let parsed = try parseCSV(
                    from: loadedCSVURL,
                    unit: loadedCSVUnit,
                    hasHeader: loadedCSVHasHeader,
                    duplicatePolicy: policy
                )
                installDataset(
                    parsed,
                    sourceURL: loadedCSVURL,
                    unit: loadedCSVUnit,
                    hasHeader: loadedCSVHasHeader,
                    duplicatePolicy: policy,
                    preserveSelection: true,
                    scientificStanding: activeDatasetScientificStanding
                )
            } catch {
                statusMessage = "Duplicate timestamp policy change failed."
                lastErrorMessage = error.localizedDescription
            }
            return
        }

        installDataset(
            dataset.applyingDuplicateTimestampPolicy(policy),
            sourceURL: nil,
            unit: loadedCSVUnit,
            hasHeader: loadedCSVHasHeader,
            duplicatePolicy: policy,
            preserveSelection: true,
            scientificStanding: activeDatasetScientificStanding
        )
    }

    func runAdaptiveClassicAnchorDetection() {
        guard canRunAdaptiveClassicAnchorDetection else {
            return
        }

        guard let dataset else {
            detectorStatusMessage = "尚未加载数据集。"
            lastErrorMessage = nil
            return
        }

        let generation = detectorRunGeneration &+ 1
        detectorRunGeneration = generation
        let datasetSnapshot = dataset
        let bandSettings = adaptiveDetectorBandSettings
        let qualitySettings = qualitySettings
        let stateTuning = stateDetectorTuning
        let detectorParameters = detectorParameterSettings
        let manualProfile = manualThresholdProfile
        let adaptiveV2 = useAdaptiveV2Canonicalization
        let manualScope = manualThresholdScope
        let signature = DetectionInputsSignature(
            datasetID: datasetSnapshot.id.uuidString,
            bandSettings: bandSettings,
            qualitySettings: qualitySettings,
            stateTuning: stateTuning,
            detectorParameters: detectorParameters,
            manualThresholdProfile: manualProfile,
            useAdaptiveV2Canonicalization: adaptiveV2,
            manualThresholdScope: manualScope
        )

        isDetectorRunning = true
        pinnedISIBeforeRerun = pinnedISIDiagnostic
        detectorStatusMessage = "Running structural candidate detection..."
        lastErrorMessage = nil

        Task.detached(priority: .userInitiated) {
            let run = HybridPatternDetectionFramework.run(
                dataset: datasetSnapshot,
                bandSettings: bandSettings,
                qualitySettings: qualitySettings,
                refractoryAction: .warnOnly,
                stateTuning: stateTuning,
                detectorParameters: detectorParameters,
                manualThresholdProfile: manualProfile,
                useAdaptiveV2Canonicalization: adaptiveV2,
                manualThresholdScope: manualScope,
                buildCommit: ResultPackageAppBuildIdentity.current
            )
            let annotationCache = ClassicAnchorAnnotationCache(dataset: datasetSnapshot, run: run)

            await MainActor.run {
                self.finishAdaptiveClassicAnchorDetection(
                    run,
                    annotationCache: annotationCache,
                    generation: generation,
                    signature: signature
                )
            }
        }
    }

    var canRunAdaptiveClassicAnchorDetection: Bool {
        dataset != nil
            && !isDetectorRunning
            && !isManualAnnotationImporting
            && !isResultPackageExporting
    }

    private func finishAdaptiveClassicAnchorDetection(
        _ run: ClassicAnchorDetectionRun,
        annotationCache: ClassicAnchorAnnotationCache,
        generation: Int,
        signature: DetectionInputsSignature
    ) {
        guard generation == detectorRunGeneration else {
            return
        }

        approvedManualAnnotationImports =
            ManualAnnotationCSVApprovedBatch.retainingAuthorityBound(
                approvedManualAnnotationImports,
                toRunID: run.runIdentity.runID,
                settingsDigest: run.runIdentity.settingsDigest
            )
        classicAnchorDetectionRun = run
        classicAnchorAnnotationCache = annotationCache
        lastDetectionInputsSignature = signature
        retainReviewStatuses(for: run)
        restorePersistedReviewStatuses(for: run)
        if let before = pinnedISIBeforeRerun {
            recomputePinnedISIAfterRerun(before: before)
        }
        pinnedISIBeforeRerun = nil
        if let focusedClassicAnchorCandidateID,
           !run.candidates.contains(where: { $0.id == focusedClassicAnchorCandidateID }) {
            self.focusedClassicAnchorCandidateID = nil
        }
        detectorLastRunDate = Date()
        let runtimeSuffix = run.performanceReport.map {
            " Runtime \(formatDetectorRuntime($0.totalWallTimeMs))."
        } ?? ""
        detectorStatusMessage = activeDatasetScientificStanding.detectorResultPrefix
            + "\(run.candidateCount) candidate(s), \(run.selectedEventCount) event, \(run.selectedGapCount) gap, and \(run.selectedStateCount) state selected.\(runtimeSuffix)"
        lastErrorMessage = nil
        isDetectorRunning = false
    }

    /// Invalidates any detached detector task before the dataset identity changes.
    ///
    /// A stale task may still finish its CPU work, but its generation can no longer
    /// publish into this document. Resetting the running flag here also prevents a
    /// discarded completion from leaving the detector UI permanently disabled.
    private func invalidateDetectorRunForDatasetMutation() {
        detectorRunGeneration &+= 1
        isDetectorRunning = false
    }

    private func formatDetectorRuntime(_ milliseconds: Double) -> String {
        guard milliseconds.isFinite, milliseconds >= 0 else {
            return "NA"
        }
        if milliseconds < 1_000 {
            return String(format: "%.0f ms", milliseconds)
        }
        return String(format: "%.2f s", milliseconds / 1_000)
    }

    func focusClassicAnchorCandidate(_ candidateID: String, adjustRasterReviewWindow: Bool = true) {
        focusedClassicAnchorCandidateID = candidateID
        if adjustRasterReviewWindow, !rasterVisibleWindowUserLocked {
            applyRasterReviewWindow(for: candidateID)
        }
        classicAnchorFocusRequestID &+= 1
    }

    func clearClassicAnchorFocus() {
        focusedClassicAnchorCandidateID = nil
        classicAnchorFocusRequestID &+= 1
    }

    func pinISIDiagnostic(_ diagnostic: PinnedISIDiagnostic) {
        pinnedISIDiagnostic = diagnostic
        pinnedISIComparison = nil
        pinnedISIRequestID &+= 1
    }

    func clearPinnedISIDiagnostic() {
        pinnedISIDiagnostic = nil
        pinnedISIComparison = nil
        pinnedISIBeforeRerun = nil
        pinnedISIRequestID &+= 1
    }

    private func recomputePinnedISIAfterRerun(before: PinnedISIDiagnostic) {
        guard let dataset,
              let train = dataset.trains.first(where: { $0.id == before.trainID }),
              train.isiSec.indices.contains(before.isiIndex),
              let isi = train.isiSec[before.isiIndex],
              isi.isFinite else {
            pinnedISIComparison = nil
            return
        }
        let covering = classicAnchorPublicEventAnnotations
            .filter { annotation in
                (classicAnchorReviewStatuses[annotation.candidateID] ?? .unreviewed) != .rejected
                    && annotation.trainID == train.id
                    && (annotation.coveredISIIndices(in: train)?.contains(before.isiIndex) ?? false)
            }
            .sorted {
                $0.priority != $1.priority ? $0.priority > $1.priority : $0.id < $1.id
            }
            .first
        let band = classicAnchorDetectionRun?.resolution(for: train.id)?.burstBand
        let manualPauseLowerSec = manualPauseMode != .automatic && manualPauseMinISIMs > 0
            ? manualPauseMinISIMs / 1_000
            : nil
        let diagnostic = PerISIDiagnosticBuilder.diagnose(
            isiSec: isi,
            coveringCandidateLabel: covering?.displayFamilyName,
            seedLowerSec: band?.seedLowerSec,
            seedUpperSec: band?.seedUpperSec,
            bridgeUpperSec: band?.bridgeUpperSec,
            manualPauseLowerSec: manualPauseLowerSec
        )
        let after = PinnedISIDiagnostic(
            trainID: before.trainID,
            trainName: train.name,
            isiIndex: before.isiIndex,
            leftTimestampSec: before.leftTimestampSec,
            rightTimestampSec: before.rightTimestampSec,
            isiSec: isi,
            reviewStatus: covering.map {
                (classicAnchorReviewStatuses[$0.candidateID] ?? .unreviewed).title
            },
            diagnostic: diagnostic
        )
        pinnedISIDiagnostic = after
        pinnedISIComparison = PinnedISIComparison(before: before, after: after)
    }

    func adaptiveV2Explanation(
        trainID: String,
        isiIndex: Int
    ) -> AdaptiveV2CanonicalizationExplanation {
        guard let candidates = classicAnchorDetectionRun?.result(for: trainID)?.candidates else {
            return .absent
        }
        let covering = candidates
            .filter {
                $0.startISIIndex <= isiIndex
                    && isiIndex <= $0.endISIIndex
                    && $0.decisionPath.contains("adaptive_v2_canonicalization=")
            }
            .sorted { lhs, rhs in
                if lhs.selectedForAuto != rhs.selectedForAuto {
                    return lhs.selectedForAuto && !rhs.selectedForAuto
                }
                if lhs.priority != rhs.priority { return lhs.priority > rhs.priority }
                return lhs.id < rhs.id
            }
            .first
        return covering.map {
            AdaptiveV2CanonicalizationExplanation.parse(decisionPath: $0.decisionPath)
        } ?? .absent
    }

    func reviewStatus(for candidateID: String) -> ClassicAnchorReviewStatus {
        classicAnchorReviewStatuses[candidateID] ?? .unreviewed
    }

    func setReviewStatus(_ status: ClassicAnchorReviewStatus, for candidateID: String) {
        if status == .unreviewed {
            classicAnchorReviewStatuses.removeValue(forKey: candidateID)
            classicAnchorReviewInputs.removeValue(forKey: candidateID)
        } else {
            guard let run = classicAnchorDetectionRun,
                  run.candidates.contains(where: { $0.id == candidateID }) else {
                statusMessage = "Candidate review was not saved."
                lastErrorMessage =
                    "Run detection again and select a candidate from the current run."
                return
            }
            classicAnchorReviewStatuses[candidateID] = status
            let packageStatus: STPDCandidateReviewStatus
            switch status {
            case .unreviewed:
                return
            case .accepted:
                packageStatus = .accepted
            case .rejected:
                packageStatus = .rejected
            case .needsReview:
                packageStatus = .needsReview
            }
            classicAnchorReviewInputs[candidateID] = STPDCandidateReviewInput(
                sourceCandidateID: candidateID,
                status: packageStatus,
                reviewer: ResultPackageAppReviewerIdentity.current,
                note: "reviewer_identity=local_macos_account;source=app_candidate_review",
                reviewedAt: Date(),
                reviewedRunID: run.runIdentity.runID
            )
        }
        let candidateSummary = classicAnchorDetectionRun?.candidates.first { $0.id == candidateID }.map {
            "\($0.finalLabel.rawValue) · \($0.trainName) · ISI \($0.startISIIndex)-\($0.endISIIndex)"
        } ?? "candidate"
        statusMessage = status == .unreviewed
            ? "Manual review cleared for \(candidateSummary)."
            : "Manual review saved: \(status.title) for \(candidateSummary)."
        lastErrorMessage = nil
        persistReviewStatuses()
    }

    func setReviewStatusAndAdvance(_ status: ClassicAnchorReviewStatus, for candidateID: String) {
        setReviewStatus(status, for: candidateID)

        guard status != .unreviewed else {
            return
        }

        if let nextCandidateID = nextUnreviewedReviewCandidateID(after: candidateID) {
            focusClassicAnchorCandidate(nextCandidateID)
            statusMessage += " Moved to the next unreviewed structure."
        } else {
            clearClassicAnchorFocus()
            statusMessage += " All selected review structures are complete."
        }
    }

    private func nextUnreviewedReviewCandidateID(after candidateID: String) -> String? {
        guard let classicAnchorDetectionRun else {
            return nil
        }

        let reviewableCandidateIDs = Set(classicAnchorCandidateAuditAnnotations.map(\.candidateID))
        let reviewQueue = classicAnchorDetectionRun.candidates.filter { candidate in
            candidate.selectedForAuto &&
                reviewableCandidateIDs.contains(candidate.id) &&
                (
                    candidate.auditRecommendedTrack == .event ||
                        candidate.auditRecommendedTrack == .gap ||
                        candidate.auditRecommendedTrack == .state ||
                        candidate.auditRecommendedTrack == .review
                )
        }
        .sorted(by: manualReviewQueueSort)

        guard !reviewQueue.isEmpty else {
            return nil
        }

        let currentIndex = reviewQueue.firstIndex { $0.id == candidateID } ?? -1
        let afterCurrent = reviewQueue.dropFirst(currentIndex + 1)
        if let next = afterCurrent.first(where: { reviewStatus(for: $0.id) == .unreviewed }) {
            return next.id
        }

        let beforeCurrent = reviewQueue.prefix(max(currentIndex, 0))
        return beforeCurrent.first(where: { reviewStatus(for: $0.id) == .unreviewed })?.id
    }

    private func manualReviewQueueSort(_ lhs: ClassicAnchorCandidate, _ rhs: ClassicAnchorCandidate) -> Bool {
        let lhsRank = manualReviewQueueRank(lhs)
        let rhsRank = manualReviewQueueRank(rhs)
        if lhsRank != rhsRank {
            return lhsRank < rhsRank
        }
        if lhs.anchorLockLevel != rhs.anchorLockLevel {
            return lhs.anchorLockLevel == .strongCandidate
        }
        if lhs.priority != rhs.priority {
            return lhs.priority > rhs.priority
        }
        if lhs.trainName != rhs.trainName {
            return lhs.trainName < rhs.trainName
        }
        return lhs.startISIIndex < rhs.startISIIndex
    }

    private func manualReviewQueueRank(_ candidate: ClassicAnchorCandidate) -> Int {
        let manualStatus = reviewStatus(for: candidate.id)
        if manualStatus == .needsReview {
            return 0
        }
        if manualStatus == .unreviewed, candidate.auditReviewRequired {
            return 1
        }
        if manualStatus == .unreviewed, candidate.auditRecommendedTrack == .review {
            return 2
        }
        if manualStatus == .unreviewed, candidate.anchorLockLevel == .strongCandidate {
            return 3
        }
        if manualStatus == .unreviewed {
            return 4
        }
        return 5
    }

    private func applyRasterReviewWindow(for candidateID: String) {
        guard let annotation = classicAnchorCandidateAuditAnnotations.first(where: { $0.candidateID == candidateID }) else {
            return
        }

        let alignedSpan = abs(annotation.alignedEndSec - annotation.alignedStartSec)
        let rawSpan = abs(annotation.rawEndSec - annotation.rawStartSec)
        let eventDuration = max(annotation.durationSec, alignedSpan, rawSpan, 0.001)
        let reviewWindow = rasterReviewWindowSeconds(forEventDuration: eventDuration)
        rasterVisibleWindowSeconds = reviewWindow
        rasterVisibleWindowUnit = reviewWindow < 1 ? .milliseconds : .seconds
    }

    private func rasterReviewWindowSeconds(forEventDuration eventDuration: Double) -> Double {
        let safeDuration = max(eventDuration, 0.001)
        let highResolutionContext = min(max(safeDuration * 8, 0.18), 2.5)
        let eventCoverageContext = safeDuration * 1.25
        return max(highResolutionContext, eventCoverageContext)
    }

    func resetRasterVisibleWindowToStandard() {
        if let dataset {
            let standardWindow = standardRasterVisibleWindow(for: dataset)
            rasterVisibleWindowSeconds = standardWindow.seconds
            rasterVisibleWindowUnit = standardWindow.unit
        } else {
            rasterVisibleWindowSeconds = Self.fallbackRasterVisibleWindowSeconds
            rasterVisibleWindowUnit = .seconds
        }
        // This is the dataset-derived default, not an explicit user choice. Keep it unlocked so
        // selecting a structural candidate can automatically load a review-sized timestamp window.
        rasterVisibleWindowUserLocked = false
    }

    private func standardRasterVisibleWindow(
        for dataset: SpikeDataset
    ) -> (seconds: Double, unit: QualityDisplayUnit) {
        let durations = dataset.trains
            .map(\.rawDurationSec)
            .filter { $0.isFinite && $0 > 0 }
        guard !durations.isEmpty else {
            return (Self.fallbackRasterVisibleWindowSeconds, .seconds)
        }

        let meanSeconds = durations.reduce(0, +) / Double(durations.count)
        guard meanSeconds.isFinite, meanSeconds > 0 else {
            return (Self.fallbackRasterVisibleWindowSeconds, .seconds)
        }

        if meanSeconds >= 1 {
            return (max(1, meanSeconds.rounded()), .seconds)
        }

        let roundedMilliseconds = max(1, (meanSeconds * 1_000).rounded())
        return (roundedMilliseconds / 1_000, .milliseconds)
    }

    func exportClassicAnchorEventsCSVWithPanel() {
        guard activeDatasetScientificStanding.permitsAuthoritativeDetectorArtifactExport else {
            statusMessage = "Detector CSV export blocked."
            lastErrorMessage = ActiveDatasetScientificStandingError
                .authoritativeDetectorArtifactExportRequiresCanonicalConfirmation
                .localizedDescription
            return
        }
        guard let dataset, let classicAnchorDetectionRun else {
            statusMessage = "No detection events to export."
            lastErrorMessage = nil
            return
        }

        let selectedTrackAnnotations = classicAnchorSelectedTrackAnnotations
        guard !selectedTrackAnnotations.isEmpty else {
            statusMessage = "No auto-selected detection tracks to export."
            lastErrorMessage = nil
            return
        }

        let panel = NSSavePanel()
        panel.allowedContentTypes = [.commaSeparatedText]
        panel.canCreateDirectories = true
        panel.isExtensionHidden = false
        panel.nameFieldStringValue = defaultClassicAnchorExportFileName(datasetName: dataset.name)
        panel.message = "Export structural candidate events with algorithm metrics, graph ranges, and manual review status."

        guard panel.runModal() == .OK, let url = panel.url else {
            return
        }

        do {
            let csv = ClassicAnchorEventCSVExporter.csv(
                dataset: dataset,
                run: classicAnchorDetectionRun,
                annotations: selectedTrackAnnotations,
                reviewStatuses: classicAnchorReviewStatuses.mapValues { $0.rawValue }
            )
            try csv.write(to: url, atomically: true, encoding: .utf8)
            statusMessage = "Exported \(selectedTrackAnnotations.count) selected detection track annotation(s) to \(url.lastPathComponent)."
            lastErrorMessage = nil
        } catch {
            statusMessage = "Event export failed."
            lastErrorMessage = error.localizedDescription
        }
    }

    func exportHFSBurstArbitrationAuditCSVWithPanel() {
        guard activeDatasetScientificStanding.permitsAuthoritativeDetectorArtifactExport else {
            statusMessage = "Detector audit CSV export blocked."
            lastErrorMessage = ActiveDatasetScientificStandingError
                .authoritativeDetectorArtifactExportRequiresCanonicalConfirmation
                .localizedDescription
            return
        }
        guard let dataset, let classicAnchorDetectionRun else {
            statusMessage = "Run detection before exporting HFS-burst audit rows."
            lastErrorMessage = nil
            return
        }

        let rows = classicAnchorDetectionRun.hfsBurstArbitrationAuditRows
        guard !rows.isEmpty else {
            statusMessage = "No HFS-burst arbitration audit rows to export."
            lastErrorMessage = nil
            return
        }

        let panel = NSSavePanel()
        panel.allowedContentTypes = [.commaSeparatedText]
        panel.canCreateDirectories = true
        panel.isExtensionHidden = false
        panel.nameFieldStringValue = defaultHFSBurstAuditExportFileName(datasetName: dataset.name)
        panel.message = "Export diagnostic-only HFS versus burst arbitration evidence."

        guard panel.runModal() == .OK, let url = panel.url else {
            return
        }

        do {
            let csv = HFSBurstArbitrationAudit.csvString(rows: rows)
            try csv.write(to: url, atomically: true, encoding: .utf8)
            statusMessage = "Exported \(rows.count) HFS-burst arbitration audit row(s) to \(url.lastPathComponent)."
            lastErrorMessage = nil
        } catch {
            statusMessage = "HFS-burst audit export failed."
            lastErrorMessage = error.localizedDescription
        }
    }

    func importClassicAnchorReviewStatusesWithPanel() {
        guard let classicAnchorDetectionRun else {
            statusMessage = "Run detection before importing review statuses."
            lastErrorMessage = nil
            return
        }

        let panel = NSOpenPanel()
        panel.allowsMultipleSelection = false
        panel.canChooseDirectories = false
        panel.allowedContentTypes = [.commaSeparatedText, .plainText]
        panel.message = "Choose an exported structural event CSV containing candidate_id and review_status columns."

        guard panel.runModal() == .OK, let url = panel.url else {
            return
        }

        do {
            let csv = try String(contentsOf: url, encoding: .utf8)
            let importedRaw = try ClassicAnchorReviewStatusCSVImporter.importStatuses(contents: csv)
            let activeIDs = Set(classicAnchorDetectionRun.candidates.map(\.id))
            var applied = 0
            var ignored = 0

            for (candidateID, rawStatus) in importedRaw {
                guard activeIDs.contains(candidateID) else {
                    ignored += 1
                    continue
                }
                guard let status = ClassicAnchorReviewStatus.parse(rawStatus) else {
                    ignored += 1
                    continue
                }

                if status == .unreviewed {
                    classicAnchorReviewStatuses.removeValue(forKey: candidateID)
                } else {
                    classicAnchorReviewStatuses[candidateID] = status
                }
                // Imported CSV carries status only, not trustworthy actor/time
                // provenance. It must never be attributed to the importer.
                classicAnchorReviewInputs.removeValue(forKey: candidateID)
                applied += 1
            }

            persistReviewStatuses()
            statusMessage = "Imported \(applied) review status(es) from \(url.lastPathComponent)."
            lastErrorMessage = ignored > 0 ? "Ignored \(ignored) row(s) that did not match current candidates or review statuses." : nil
        } catch {
            statusMessage = "Review status import failed."
            lastErrorMessage = error.localizedDescription
        }
    }

    func selectedTrainIDsIncludingFocusedCandidate(for scope: SpikeTrainSelectionScope) -> Set<String> {
        var selection = selectedTrainIDs(for: scope)
        if let focusedClassicAnchorEventAnnotation {
            selection.insert(focusedClassicAnchorEventAnnotation.trainID)
        }
        return selection
    }

    func selectedTrainIDs(for scope: SpikeTrainSelectionScope) -> Set<String> {
        switch scope {
        case .raster:
            return selectedTrainIDs
        case .isiTimeline:
            return isiSelectedTrainIDs
        case .isiStateSpace:
            return isiStateSpaceSelectedTrainIDs
        case .neuralManifold:
            return neuralManifoldSelectedTrainIDs
        }
    }

    func requestedVisibleTrainCount(for scope: SpikeTrainSelectionScope) -> Int {
        switch scope {
        case .raster:
            return requestedVisibleTrainCount
        case .isiTimeline:
            return requestedISIVisibleTrainCount
        case .isiStateSpace:
            return requestedISIStateSpaceVisibleTrainCount
        case .neuralManifold:
            return requestedNeuralManifoldVisibleTrainCount
        }
    }

    func selectVisibleTrainCount(_ count: Int, scope: SpikeTrainSelectionScope = .raster) {
        guard let dataset else {
            switch scope {
            case .raster:
                requestedVisibleTrainCount = max(1, count)
                selectedTrainIDs = []
            case .isiTimeline:
                requestedISIVisibleTrainCount = max(1, count)
                isiSelectedTrainIDs = []
            case .isiStateSpace:
                requestedISIStateSpaceVisibleTrainCount = max(1, count)
                isiStateSpaceSelectedTrainIDs = []
            case .neuralManifold:
                requestedNeuralManifoldVisibleTrainCount = max(1, count)
                neuralManifoldSelectedTrainIDs = []
            }
            return
        }

        let clampedCount = clampedVisibleTrainCount(count, total: dataset.trains.count)
        // A numeric display-count edit must not silently discard a reviewer’s explicit train
        // choice and replace it with the first N dataset columns. Keep current valid choices
        // first; only fill newly requested slots from the dataset's stable source order.
        let selectedIDs = preferredVisibleTrainIDs(
            currentSelection: selectedTrainIDs(for: scope),
            count: clampedCount,
            dataset: dataset
        )
        switch scope {
        case .raster:
            requestedVisibleTrainCount = clampedCount
            selectedTrainIDs = selectedIDs
        case .isiTimeline:
            requestedISIVisibleTrainCount = clampedCount
            isiSelectedTrainIDs = selectedIDs
        case .isiStateSpace:
            requestedISIStateSpaceVisibleTrainCount = clampedCount
            isiStateSpaceSelectedTrainIDs = selectedIDs
        case .neuralManifold:
            requestedNeuralManifoldVisibleTrainCount = clampedCount
            neuralManifoldSelectedTrainIDs = selectedIDs
        }
    }

    func updateSelectedTrainIDs(_ selection: Set<String>, scope: SpikeTrainSelectionScope = .raster) {
        switch scope {
        case .raster:
            selectedTrainIDs = selection
            if !selection.isEmpty {
                requestedVisibleTrainCount = selection.count
            }
        case .isiTimeline:
            isiSelectedTrainIDs = selection
            if !selection.isEmpty {
                requestedISIVisibleTrainCount = selection.count
            }
        case .isiStateSpace:
            isiStateSpaceSelectedTrainIDs = selection
            if !selection.isEmpty {
                requestedISIStateSpaceVisibleTrainCount = selection.count
            }
        case .neuralManifold:
            neuralManifoldSelectedTrainIDs = selection
            if !selection.isEmpty {
                requestedNeuralManifoldVisibleTrainCount = selection.count
            }
        }
    }

    private func preferredVisibleTrainIDs(
        currentSelection: Set<String>,
        count: Int,
        dataset: SpikeDataset
    ) -> Set<String> {
        let retained = dataset.trains.filter { currentSelection.contains($0.id) }
        let remainingCapacity = max(0, count - retained.count)
        let additions = dataset.trains
            .filter { !currentSelection.contains($0.id) }
            .prefix(remainingCapacity)
        return Set(retained.prefix(count).map(\.id) + additions.map(\.id))
    }

    func loadCSV(
        from url: URL,
        unit: SpikeTimeUnit,
        hasHeader: Bool,
        duplicatePolicy: DuplicateTimestampPolicy
    ) {
        loadCSV(
            from: url,
            unit: unit,
            hasHeader: hasHeader,
            duplicatePolicy: duplicatePolicy,
            scientificStanding: .legacyUnreviewedImport
        )
    }

    private func loadCSV(
        from url: URL,
        unit: SpikeTimeUnit,
        hasHeader: Bool,
        duplicatePolicy: DuplicateTimestampPolicy,
        scientificStanding: ActiveDatasetScientificStanding
    ) {
        do {
            let parsed = try parseCSV(
                from: url,
                unit: unit,
                hasHeader: hasHeader,
                duplicatePolicy: duplicatePolicy
            )
            installDataset(
                parsed,
                sourceURL: url,
                unit: unit,
                hasHeader: hasHeader,
                duplicatePolicy: duplicatePolicy,
                preserveSelection: false,
                scientificStanding: scientificStanding
            )
        } catch {
            statusMessage = dataset == nil
                ? "CSV load failed."
                : "CSV load failed. Existing dataset preserved."
            lastErrorMessage = error.localizedDescription
        }
    }

    private func loadNEX(
        from url: URL,
        duplicatePolicy: DuplicateTimestampPolicy
    ) {
        do {
            let source = try BoundedScientificSourceReader.readNEX(from: url)
            let imported = try NeuroExplorerNEXCodec.read(source.snapshot)
            let sourceDescription = "\(url.path) · SHA-256 \(source.sourceSHA256)"
            let parsed = NeuroExplorerNEXDatasetAdapter.dataset(
                from: imported,
                name: url.deletingPathExtension().lastPathComponent,
                sourceDescription: sourceDescription,
                duplicateTimestampPolicy: duplicatePolicy
            )
            installDataset(
                parsed,
                sourceURL: nil,
                unit: .seconds,
                hasHeader: false,
                duplicatePolicy: duplicatePolicy,
                preserveSelection: false,
                scientificStanding: .legacyUnreviewedImport
            )
            let ignoredSuffix = imported.ignoredVariableNames.isEmpty
                ? ""
                : "；已忽略 \(imported.ignoredVariableNames.count) 个 Continuous/Population Vector 变量"
            statusMessage = "已从 NEX 加载 \(parsed.trains.count) 条 spike train、\(parsed.totalSpikeCount) 个 spike、\(parsed.taskEvents.count) 个事件边界\(ignoredSuffix)。当前可浏览、手工标记并导出；尚未获得自动检测或封存结果权限。"
        } catch {
            statusMessage = dataset == nil
                ? "NEX 加载失败。"
                : "NEX 加载失败；现有数据集已保留。"
            lastErrorMessage = error.localizedDescription
        }
    }

    private func parseCSV(
        from url: URL,
        unit: SpikeTimeUnit,
        hasHeader: Bool,
        duplicatePolicy: DuplicateTimestampPolicy
    ) throws -> SpikeDataset {
        let contents = try String(contentsOf: url, encoding: .utf8)
        return try CSVSpikeMatrixParser.parse(
            contents: contents,
            datasetName: url.deletingPathExtension().lastPathComponent,
            sourceDescription: url.path,
            unit: unit,
            hasHeader: hasHeader,
            duplicatePolicy: duplicatePolicy
        )
    }

    private func installDataset(
        _ parsed: SpikeDataset,
        sourceURL: URL?,
        unit: SpikeTimeUnit,
        hasHeader: Bool,
        duplicatePolicy: DuplicateTimestampPolicy,
        preserveSelection: Bool,
        scientificStanding: ActiveDatasetScientificStanding
    ) {
        let previousSelection = selectedTrainIDs
        let previousISISelection = isiSelectedTrainIDs
        let previousISIStateSpaceSelection = isiStateSpaceSelectedTrainIDs
        let previousNeuralManifoldSelection = neuralManifoldSelectedTrainIDs
        let allTrainIDs = Set(parsed.trains.map(\.id))
        let retainedSelection = previousSelection.intersection(allTrainIDs)
        let retainedISISelection = previousISISelection.intersection(allTrainIDs)
        let retainedISIStateSpaceSelection = previousISIStateSpaceSelection.intersection(allTrainIDs)
        let retainedNeuralManifoldSelection = previousNeuralManifoldSelection.intersection(allTrainIDs)

        invalidateDetectorRunForDatasetMutation()
        if !preserveSelection {
            resetManualThresholdFields()
        }
        resetNeuralManifoldEmbeddingCaches()
        dataset = parsed
        activeDatasetScientificStanding = scientificStanding
        // Use the rounded mean train duration as a dataset-specific, legible initial zoom.
        // The reviewer can still change it manually.
        let standardWindow = standardRasterVisibleWindow(for: parsed)
        rasterVisibleWindowSeconds = standardWindow.seconds
        rasterVisibleWindowUnit = standardWindow.unit
        // Dataset-derived defaults remain eligible for automatic candidate-review zoom. Only a
        // direct user edit of the visible-window field locks the scale.
        rasterVisibleWindowUserLocked = false
        classicAnchorDetectionRun = nil
        classicAnchorAnnotationCache = .empty
        focusedClassicAnchorCandidateID = nil
        classicAnchorFocusRequestID &+= 1
        pinnedISIDiagnostic = nil
        pinnedISIComparison = nil
        pinnedISIBeforeRerun = nil
        classicAnchorReviewStatuses = [:]
        classicAnchorReviewInputs = [:]
        manualAnnotationsByTrain = [:]
        manualISIUndoStack = []
        rasterManualAnnotationEditMode = .apply
        canonicalManualDataset = nil
        canonicalManualISILabelDraft = nil
        confirmedCanonicalManualLabels = nil
        invalidateManualPatternLearningPreview()
        appliedManualPatternLearningProposal = nil
        appliedManualPatternLearningProposalsByFamily = [:]
        manualLearnedThresholdRollback = nil
        approvedManualAnnotationImports = []
        detectorLastRunDate = nil
        lastDetectionInputsSignature = nil
        detectorStatusMessage = "Detector has not run."
        selectedTrainIDs = preserveSelection && !retainedSelection.isEmpty ? retainedSelection : defaultVisibleTrainIDs(for: parsed)
        isiSelectedTrainIDs = preserveSelection && !retainedISISelection.isEmpty ? retainedISISelection : defaultISIVisibleTrainIDs(for: parsed)
        isiStateSpaceSelectedTrainIDs = preserveSelection && !retainedISIStateSpaceSelection.isEmpty ? retainedISIStateSpaceSelection : defaultISIStateSpaceVisibleTrainIDs(for: parsed)
        neuralManifoldSelectedTrainIDs = preserveSelection && !retainedNeuralManifoldSelection.isEmpty
            ? retainedNeuralManifoldSelection
            : defaultNeuralManifoldVisibleTrainIDs(for: parsed)
        if !selectedTrainIDs.isEmpty {
            requestedVisibleTrainCount = selectedTrainIDs.count
        }
        if !isiSelectedTrainIDs.isEmpty {
            requestedISIVisibleTrainCount = isiSelectedTrainIDs.count
        }
        if !isiStateSpaceSelectedTrainIDs.isEmpty {
            requestedISIStateSpaceVisibleTrainCount = isiStateSpaceSelectedTrainIDs.count
        }
        if !neuralManifoldSelectedTrainIDs.isEmpty {
            requestedNeuralManifoldVisibleTrainCount = neuralManifoldSelectedTrainIDs.count
        }
        loadedCSVURL = sourceURL
        loadedCSVUnit = unit
        loadedCSVHasHeader = hasHeader
        duplicateTimestampPolicy = duplicatePolicy

        // Manual drafts are keyed to the dataset rather than a detector run. Revalidate every
        // restored range against the newly installed train geometry so stale/cross-dataset marks
        // never become visible annotations.
        loadManualAnnotations()

        let report = SpikeQualityAnalyzer.analyze(dataset: parsed, settings: qualitySettings)
        let qualitySuffix = report.errorCount > 0 ? "，\(report.errorCount) 条 QC 错误" :
            report.warningCount > 0 ? "，\(report.warningCount) 条 QC 警告" : "，QC 已通过"
        let droppedSuffix = report.droppedDuplicateTimestampCount > 0 ?
            "，已折叠 \(report.droppedDuplicateTimestampCount) 个重复时间戳" : ""
        let standingPrefix = switch scientificStanding {
        case .noDataset:
            ""
        case .nonAuthoritativeDemo:
            "仅供演示——非权威。"
        case .legacyUnreviewedImport:
            "旧式导入——尚未经过科学确认。"
        case .canonicalConfirmedExploration:
            "已确认规范导入的探索视图。"
        }
        statusMessage = standingPrefix
            + "已加载 \(parsed.trains.count) 条序列、\(parsed.totalSpikeCount) 个 spike\(qualitySuffix)\(droppedSuffix)。"
        lastErrorMessage = nil
    }

    private func defaultVisibleTrainIDs(for dataset: SpikeDataset) -> Set<String> {
        Set(dataset.trains.map(\.id))
    }

    private func defaultISIVisibleTrainIDs(for dataset: SpikeDataset) -> Set<String> {
        let count = clampedVisibleTrainCount(Self.defaultISIVisibleTrainCount, total: dataset.trains.count)
        return Set(dataset.trains.prefix(count).map(\.id))
    }

    private func defaultISIStateSpaceVisibleTrainIDs(for dataset: SpikeDataset) -> Set<String> {
        let count = clampedVisibleTrainCount(Self.defaultISIStateSpaceVisibleTrainCount, total: dataset.trains.count)
        return Set(dataset.trains.prefix(count).map(\.id))
    }

    private func defaultNeuralManifoldVisibleTrainIDs(for dataset: SpikeDataset) -> Set<String> {
        let count = clampedVisibleTrainCount(
            Self.defaultNeuralManifoldVisibleTrainCount,
            total: dataset.trains.count
        )
        return Set(dataset.trains.prefix(count).map(\.id))
    }

    private func clampedVisibleTrainCount(_ count: Int, total: Int) -> Int {
        guard total > 0 else {
            return max(1, count)
        }
        return min(max(1, count), total)
    }

    private func retainReviewStatuses(for run: ClassicAnchorDetectionRun) {
        let activeIDs = Set(run.candidates.map(\.id))
        classicAnchorReviewStatuses = classicAnchorReviewStatuses.filter { activeIDs.contains($0.key) }
        classicAnchorReviewInputs = classicAnchorReviewInputs.filter {
            activeIDs.contains($0.key)
                && $0.value.reviewedRunID == run.runIdentity.runID
        }
    }

    private func restorePersistedReviewStatuses(for run: ClassicAnchorDetectionRun) {
        guard let dataset else {
            return
        }

        do {
            let key = reviewPersistenceKey(dataset: dataset, run: run)
            let activeIDs = Set(run.candidates.map(\.id))
            let restoredRaw = try reviewPersistence.loadStatuses(runKey: key)
            let restored = restoredRaw.reduce(into: [String: ClassicAnchorReviewStatus]()) { partial, item in
                guard activeIDs.contains(item.key),
                      let status = ClassicAnchorReviewStatus.parse(item.value),
                      status != .unreviewed else {
                    return
                }
                partial[item.key] = status
            }
            classicAnchorReviewStatuses = restored.merging(classicAnchorReviewStatuses) { _, current in current }
            persistReviewStatuses()
        } catch {
            lastErrorMessage = "Review status restore failed: \(error.localizedDescription)"
        }
    }

    private func persistReviewStatuses() {
        guard let dataset, let classicAnchorDetectionRun else {
            return
        }

        let activeIDs = Set(classicAnchorDetectionRun.candidates.map(\.id))
        let statuses = classicAnchorReviewStatuses
            .filter { activeIDs.contains($0.key) && $0.value != .unreviewed }
            .mapValues { $0.rawValue }

        do {
            let key = reviewPersistenceKey(dataset: dataset, run: classicAnchorDetectionRun)
            try reviewPersistence.saveStatuses(
                statuses,
                runKey: key,
                dataset: dataset,
                run: classicAnchorDetectionRun
            )
        } catch {
            lastErrorMessage = "Review status autosave failed: \(error.localizedDescription)"
        }
    }

    private func reviewPersistenceKey(dataset: SpikeDataset, run: ClassicAnchorDetectionRun) -> String {
        var parts: [String] = [
            dataset.name,
            dataset.sourceDescription,
            "\(dataset.trains.count)",
            "\(dataset.totalSpikeCount)",
            String(format: "%.12g", locale: Locale(identifier: "en_US_POSIX"), qualitySettings.artifactThresholdSec),
            String(format: "%.12g", locale: Locale(identifier: "en_US_POSIX"), qualitySettings.refractorySuspectThresholdSec),
            String(format: "%.12g", locale: Locale(identifier: "en_US_POSIX"), run.bandSettings.minValidISISec),
            String(format: "%.12g", locale: Locale(identifier: "en_US_POSIX"), run.bandSettings.histogramBinWidthSec)
        ]
        parts.append(contentsOf: dataset.trains.map { train in
            [
                train.id,
                "\(train.spikeCount)",
                String(format: "%.12g", locale: Locale(identifier: "en_US_POSIX"), train.firstTimestampSec ?? 0),
                String(format: "%.12g", locale: Locale(identifier: "en_US_POSIX"), train.lastTimestampSec ?? 0)
            ].joined(separator: ":")
        })
        parts.append(contentsOf: run.candidates.map { candidate in
            [
                candidate.id,
                "\(candidate.startSpikeIndex)-\(candidate.endSpikeIndex)",
                candidate.finalLabel.rawValue,
                candidate.anchorLockLevel.rawValue
            ].joined(separator: ":")
        })
        return stableHash(parts.joined(separator: "|"))
    }

    private func stableHash(_ text: String) -> String {
        var hash: UInt64 = 14_695_981_039_346_656_037
        for byte in text.utf8 {
            hash ^= UInt64(byte)
            hash &*= 1_099_511_628_211
        }
        return String(hash, radix: 16)
    }

    private func defaultClassicAnchorExportFileName(datasetName: String) -> String {
        "\(safeExportBaseName(datasetName: datasetName))_classic_anchor_events.csv"
    }

    private func defaultHFSBurstAuditExportFileName(datasetName: String) -> String {
        "\(safeExportBaseName(datasetName: datasetName))_hfs_burst_arbitration_audit.csv"
    }

    private func safeExportBaseName(datasetName: String) -> String {
        let safeName = datasetName
            .replacingOccurrences(of: "[^A-Za-z0-9._-]+", with: "_", options: .regularExpression)
            .trimmingCharacters(in: CharacterSet(charactersIn: "._-"))
        return safeName.isEmpty ? "spike_train_events" : safeName
    }

    private func bundledSampleURL() -> URL? {
        if let resourceURL = Bundle.main.url(forResource: sampleFileName, withExtension: sampleFileExtension) {
            return resourceURL
        }

        let fileName = "\(sampleFileName).\(sampleFileExtension)"
        let fileManager = FileManager.default
        var candidates: [URL] = []

        let currentDirectory = URL(fileURLWithPath: fileManager.currentDirectoryPath)
        candidates.append(currentDirectory.appendingPathComponent("../../inst/extdata/\(fileName)"))
        candidates.append(currentDirectory.appendingPathComponent("inst/extdata/\(fileName)"))

        let bundleURL = Bundle.main.bundleURL
        candidates.append(bundleURL.deletingLastPathComponent().deletingLastPathComponent().appendingPathComponent("inst/extdata/\(fileName)"))

        return candidates.first { fileManager.fileExists(atPath: $0.path) }
    }
}
