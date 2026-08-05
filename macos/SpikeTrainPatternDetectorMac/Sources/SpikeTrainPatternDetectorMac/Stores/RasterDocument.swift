import AppKit
import Foundation
import Observation
import STPDCore
import UniformTypeIdentifiers

/// How the currently installed dataset entered the legacy active-document surface.
///
/// The current preparation-only importer intentionally has no "authoritative" case: it validates
/// a value but cannot activate it yet. Demo and legacy paths may support exploration, but neither
/// can mint a sealed scientific result.
enum ActiveDatasetScientificStanding: Equatable, Sendable {
    case noDataset
    case nonAuthoritativeDemo
    case legacyUnreviewedImport

    var permitsAuthoritativeDetectorArtifactExport: Bool { false }
    var permitsSealedResultExport: Bool { permitsAuthoritativeDetectorArtifactExport }

    var detectorResultPrefix: String {
        switch self {
        case .noDataset:
            return ""
        case .nonAuthoritativeDemo:
            return "Demo result — non-authoritative; sealed export is locked. "
        case .legacyUnreviewedImport:
            return "Unreviewed legacy-import result — non-authoritative; sealed export is locked. "
        }
    }
}

enum ActiveDatasetScientificStandingError: Error, Equatable, LocalizedError, Sendable {
    case canonicalConfirmationRequired
    case authoritativeDetectorArtifactExportRequiresCanonicalConfirmation

    var errorDescription: String? {
        switch self {
        case .canonicalConfirmationRequired:
            return "Sealed result export requires a canonically confirmed scientific import. Demo and legacy CSV data are non-authoritative."
        case .authoritativeDetectorArtifactExportRequiresCanonicalConfirmation:
            return "Detector CSV export requires a canonically confirmed scientific import. Demo and legacy CSV results are exploratory and cannot be exported as scientific artifacts."
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

@MainActor
@Observable
final class RasterDocument {
    private static let defaultVisibleTrainCount = 10
    private static let defaultISIVisibleTrainCount = 4
    private static let defaultISIStateSpaceVisibleTrainCount = 1

    var dataset: SpikeDataset?
    private(set) var activeDatasetScientificStanding: ActiveDatasetScientificStanding = .noDataset
    let scientificImportCoordinator = ScientificImportCoordinator()
    var isScientificImportSheetPresented = false
    var selectedTrainIDs: Set<String> = []
    var isiSelectedTrainIDs: Set<String> = []
    var isiStateSpaceSelectedTrainIDs: Set<String> = []
    var statusMessage = "No dataset loaded."
    var lastErrorMessage: String?
    var rawImportUnit: SpikeTimeUnit = .seconds
    var rawCSVHasHeader = true
    var duplicateTimestampPolicy: DuplicateTimestampPolicy = .errorKeep
    var qcDisplayUnit: QualityDisplayUnit = .milliseconds
    var artifactThresholdMs = 0.9
    var refractorySuspectThresholdMs = 1.0
    var artifactThresholdUnit: QualityDisplayUnit = .milliseconds
    var refractorySuspectThresholdUnit: QualityDisplayUnit = .milliseconds
    var requestedVisibleTrainCount = defaultVisibleTrainCount
    var requestedISIVisibleTrainCount = defaultISIVisibleTrainCount
    var requestedISIStateSpaceVisibleTrainCount = defaultISIStateSpaceVisibleTrainCount
    var rasterVisibleWindowSeconds = 5.0
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
    var isiStateSpaceTimeMode: RasterTimeMode = .aligned
    var isiStateSpaceLayoutMode: ISIStateSpaceLayoutMode = .singleTrain
    var isiStateSpaceDisplayUnit: QualityDisplayUnit = .milliseconds
    var isiStateSpaceAxisScale: ISIYAxisScale = .log
    var isiStateSpaceHalfWindowK = 3
    var isiStateSpaceScaling: ISIStateSpaceScaling = .robust
    var isiStateSpaceLabelSource: ISIStateSpaceLabelSource = .auditFinal
    var isiStateSpaceWinsorizeExtremeLogISI = true
    var isiStateSpaceBreakLongISI = true
    var isiStateSpaceBreakThresholdMs = 150.0
    var classicAnchorDetectionRun: ClassicAnchorDetectionRun?
    var isResultPackageExporting = false
    var isManualAnnotationImporting = false
    // MARK: Result package readback (Phase 2.2C-B4) — strictly read-only, isolated from the active
    // detector document. Loading a package never touches dataset / run / settings / reviews / manual
    // annotations. See RasterDocument+ResultPackageReadback.swift for the read flow.
    /// The immutable, verified result of the last successful `.stpdresult` read-back, if any.
    var loadedResultPackage: STPDResultPackageReadResult?
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
    var classicAnchorReviewStatuses: [String: ClassicAnchorReviewStatus] = [:]
    /// Provenance captured when this app instance authors a candidate review.
    /// Imported or legacy status-only rows have no entry and cannot silently
    /// inherit the identity of the person who later exports them.
    var classicAnchorReviewInputs: [String: STPDCandidateReviewInput] = [:]
    /// Reserved for a future sealed local-authoring workflow. Bare entries are never populated by
    /// CSV import and are rejected by result-package export until that workflow supplies provenance.
    var manualAnnotationsByTrain: [String: [ManualAnnotation]] = [:]
    /// Atomic identity-bound CSV batches. Their annotation payload is intentionally not mirrored
    /// into `manualAnnotationsByTrain`, so imported evidence cannot outlive its approval receipt.
    var approvedManualAnnotationImports:
        [ManualAnnotationCSVApprovedBatch] = []
    var detectorStatusMessage = "Detector has not run."
    var detectorLastRunDate: Date?
    /// Part of detector authority and result-package provenance. The default
    /// matches `HybridPatternDetectionFramework.run` and preserves the clean app path.
    var useAdaptiveV2Canonicalization = false
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
    var isDetectorRunning = false
    private var detectorRunGeneration = 0
    private var classicAnchorAnnotationCache = ClassicAnchorAnnotationCache.empty

    private let sampleFileName = "Grechishnikova_STN_2017_subset"
    private let sampleFileExtension = "csv"
    private let reviewPersistence = ClassicAnchorReviewPersistence()
    private var loadedCSVURL: URL?
    private var loadedCSVUnit: SpikeTimeUnit = .seconds
    private var loadedCSVHasHeader = true

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
        panel.allowedContentTypes = supportedContentTypes
        panel.message = "Choose a CSV or XLSX timestamp table. Source facts and scientific meaning will be confirmed separately."

        guard panel.runModal() == .OK, let url = panel.url else {
            return
        }

        isScientificImportSheetPresented = true
        Task { await scientificImportCoordinator.beginImport(from: url) }
    }

    /// Compatibility entry point for older callers. All user-facing raw-data imports now enter the
    /// exact-snapshot two-stage review instead of installing a legacy CSV immediately.
    func openCSVWithPanel() {
        openScientificImportWithPanel()
    }

    func dismissScientificImportReview() {
        isScientificImportSheetPresented = false
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
            detectorStatusMessage = "No dataset loaded."
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

        isDetectorRunning = true
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
                buildCommit: ResultPackageAppBuildIdentity.current
            )
            let annotationCache = ClassicAnchorAnnotationCache(dataset: datasetSnapshot, run: run)

            await MainActor.run {
                self.finishAdaptiveClassicAnchorDetection(
                    run,
                    annotationCache: annotationCache,
                    generation: generation
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
        generation: Int
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
        retainReviewStatuses(for: run)
        restorePersistedReviewStatuses(for: run)
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

    private func standardRasterVisibleWindowSeconds(for dataset: SpikeDataset) -> Double {
        let candidate = dataset.maxAlignedDurationSec / 5.0
        guard candidate.isFinite, candidate > 0 else {
            return 5.0
        }
        return min(max(candidate, 0.01), 600.0)
    }

    func resetRasterVisibleWindowToStandard() {
        if let dataset {
            rasterVisibleWindowSeconds = standardRasterVisibleWindowSeconds(for: dataset)
        } else {
            rasterVisibleWindowSeconds = 5.0
        }
        rasterVisibleWindowUnit = rasterVisibleWindowSeconds < 1 ? .milliseconds : .seconds
        rasterVisibleWindowUserLocked = true
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
            }
            return
        }

        let clampedCount = clampedVisibleTrainCount(count, total: dataset.trains.count)
        let selectedIDs = Set(dataset.trains.prefix(clampedCount).map(\.id))
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
        }
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
        let allTrainIDs = Set(parsed.trains.map(\.id))
        let retainedSelection = previousSelection.intersection(allTrainIDs)
        let retainedISISelection = previousISISelection.intersection(allTrainIDs)
        let retainedISIStateSpaceSelection = previousISIStateSpaceSelection.intersection(allTrainIDs)

        invalidateDetectorRunForDatasetMutation()
        dataset = parsed
        activeDatasetScientificStanding = scientificStanding
        // Default standard visible window = 1/5 of the longest spike-train duration,
        // and keep it stable across reviews (Center re-centres at this window instead
        // of auto-zooming per candidate). The reviewer can still change it manually.
        rasterVisibleWindowSeconds = standardRasterVisibleWindowSeconds(for: parsed)
        rasterVisibleWindowUnit = rasterVisibleWindowSeconds < 1 ? .milliseconds : .seconds
        rasterVisibleWindowUserLocked = true
        classicAnchorDetectionRun = nil
        classicAnchorAnnotationCache = .empty
        focusedClassicAnchorCandidateID = nil
        classicAnchorFocusRequestID &+= 1
        classicAnchorReviewStatuses = [:]
        classicAnchorReviewInputs = [:]
        manualAnnotationsByTrain = [:]
        approvedManualAnnotationImports = []
        detectorLastRunDate = nil
        detectorStatusMessage = "Detector has not run."
        selectedTrainIDs = preserveSelection && !retainedSelection.isEmpty ? retainedSelection : defaultVisibleTrainIDs(for: parsed)
        isiSelectedTrainIDs = preserveSelection && !retainedISISelection.isEmpty ? retainedISISelection : defaultISIVisibleTrainIDs(for: parsed)
        isiStateSpaceSelectedTrainIDs = preserveSelection && !retainedISIStateSpaceSelection.isEmpty ? retainedISIStateSpaceSelection : defaultISIStateSpaceVisibleTrainIDs(for: parsed)
        if !selectedTrainIDs.isEmpty {
            requestedVisibleTrainCount = selectedTrainIDs.count
        }
        if !isiSelectedTrainIDs.isEmpty {
            requestedISIVisibleTrainCount = isiSelectedTrainIDs.count
        }
        if !isiStateSpaceSelectedTrainIDs.isEmpty {
            requestedISIStateSpaceVisibleTrainCount = isiStateSpaceSelectedTrainIDs.count
        }
        loadedCSVURL = sourceURL
        loadedCSVUnit = unit
        loadedCSVHasHeader = hasHeader
        duplicateTimestampPolicy = duplicatePolicy

        let report = SpikeQualityAnalyzer.analyze(dataset: parsed, settings: qualitySettings)
        let qualitySuffix = report.errorCount > 0 ? ", \(report.errorCount) QC error(s)" :
            report.warningCount > 0 ? ", \(report.warningCount) QC warning(s)" : ", QC passed"
        let droppedSuffix = report.droppedDuplicateTimestampCount > 0 ?
            ", \(report.droppedDuplicateTimestampCount) duplicate timestamp(s) collapsed" : ""
        let standingPrefix = switch scientificStanding {
        case .noDataset:
            ""
        case .nonAuthoritativeDemo:
            "Demo only — non-authoritative. "
        case .legacyUnreviewedImport:
            "Legacy import — not scientifically confirmed. "
        }
        statusMessage = standingPrefix
            + "\(parsed.trains.count) trains, \(parsed.totalSpikeCount) spikes loaded\(qualitySuffix)\(droppedSuffix)."
        lastErrorMessage = nil
    }

    private func defaultVisibleTrainIDs(for dataset: SpikeDataset) -> Set<String> {
        let count = clampedVisibleTrainCount(Self.defaultVisibleTrainCount, total: dataset.trains.count)
        return Set(dataset.trains.prefix(count).map(\.id))
    }

    private func defaultISIVisibleTrainIDs(for dataset: SpikeDataset) -> Set<String> {
        let count = clampedVisibleTrainCount(Self.defaultISIVisibleTrainCount, total: dataset.trains.count)
        return Set(dataset.trains.prefix(count).map(\.id))
    }

    private func defaultISIStateSpaceVisibleTrainIDs(for dataset: SpikeDataset) -> Set<String> {
        let count = clampedVisibleTrainCount(Self.defaultISIStateSpaceVisibleTrainCount, total: dataset.trains.count)
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
