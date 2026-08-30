import Foundation
import Observation
import STPDCore
import STPDTabularIO

enum ScientificImportCoordinatorPhase: String, Equatable, Sendable {
    case idle
    case readingSource
    case awaitingSourceDecisions
    case stagingSource
    case reviewingScientificMeaning
    case preflightingSourceFacts
    case validatingScientificMeaning
    case validatedPreparation
    case failed
}

enum ScientificImportReviewOutcome: Sendable {
    case formIssues([ScientificImportManifestFormIssue])
    case planIssues([ScientificImportPlanIssue])
    case normalizationIssues([ScientificImportNormalizationIssue], additionalCount: Int)
    case validation(PreparedScientificImportValidationReport)
}

enum ScientificImportTransportStaging: Sendable {
    case csv(CanonicalCSVStagingResult)
    case xlsx(CanonicalXLSXWorksheetStagingResult)

    var stagedImport: StagedScientificImport {
        switch self {
        case .csv(let result):
            result.stagedImport
        case .xlsx(let result):
            result.stagedImport
        }
    }
}

/// One validated preparation plus the complete transport evidence that produced it.
///
/// Transport provenance (source bytes, explicit header decision, and XLSX worksheet identity) is
/// deliberately retained beside — never folded into — the normalized scientific value. Future
/// confirmation code must consume this wrapper instead of detaching `PreparedScientificImport`
/// from the exact review transaction.
struct ValidatedScientificImportPreparation: Sendable {
    let source: BoundedScientificSource
    let transportStaging: ScientificImportTransportStaging
    let manifestDraft: ScientificImportManifestDraft
    let preparedImport: PreparedScientificImport
    let validationReport: PreparedScientificImportValidationReport
    /// The sealed validated import paired with its shadow projection. Confirmation consumes this so
    /// the validated import, shadow projection, source binding, and fingerprint cannot be detached
    /// or mismatched. It grants no permission to replace the active dataset, run detectors, or
    /// export results.
    let confirmable: ConfirmableScientificImport

    /// The validated canonical scientific value, projected from `confirmable` to avoid a second
    /// mutable source of truth.
    var shadowCanonicalImport: ShadowCanonicalScientificImport { confirmable.shadow }
}

private enum ScientificImportCoordinatorInternalError: Error, LocalizedError {
    case sourceBindingDigestMismatch
    case selectedWorksheetUnavailable

    var errorDescription: String? {
        switch self {
        case .sourceBindingDigestMismatch:
            "The staged table does not match the exact source snapshot."
        case .selectedWorksheetUnavailable:
            "The selected worksheet is no longer available in this workbook inspection."
        }
    }
}

/// Owns one two-stage scientific-import review without mutating the active `RasterDocument`.
///
/// Stage A reads one bounded byte snapshot, requires explicit worksheet/header decisions, and
/// binds an immutable staging result. Stage B starts with a wholly unresolved manifest form.
/// A clean review may retain a validated canonical value for shadow comparison, but no method in
/// this type installs a `SpikeDataset`, creates authoritative or persisted canonical identity,
/// grants analysis authority, or
/// launches a detector. Later identity and authority work must provide those separate boundaries.
@MainActor
@Observable
final class ScientificImportCoordinator {
    private(set) var phase: ScientificImportCoordinatorPhase = .idle
    private(set) var source: BoundedScientificSource?
    private(set) var workbookInspection: CanonicalXLSXWorkbookInspection?
    private(set) var headerDecision: CanonicalTabularHeaderDecision?
    private(set) var selectedWorksheetSheetID: UInt32?
    private(set) var transportStaging: ScientificImportTransportStaging?
    var manifestForm: ScientificImportManifestForm? {
        didSet {
            generation &+= 1
            clearPreparedReview()
            if transportStaging != nil, phase != .stagingSource {
                phase = .reviewingScientificMeaning
            }
        }
    }
    private(set) var manifestDraft: ScientificImportManifestDraft?
    private(set) var preparedImport: PreparedScientificImport?
    private(set) var validatedPreparation: ValidatedScientificImportPreparation?
    /// An immutable, in-memory, source-bound confirmation record, retained only after the explicit
    /// `confirmScientificImport()` action. It is not an authority receipt and installs nothing.
    private(set) var confirmedImport: ConfirmedScientificImportManifest?
    private(set) var reviewOutcome: ScientificImportReviewOutcome?
    private(set) var preflightReport: ScientificImportPreflightReport?
    private(set) var preflightFormIssues: [ScientificImportManifestFormIssue] = []
    private(set) var failureMessage: String?

    /// The sealed persisted-and-verified confirmation, when one has been written+read-back (Confirm &
    /// Save) or reconstructed by a full replay (Restore & Verify) this session. Its presence removes
    /// ONLY the persistence readiness blocker. Any form / source / header / worksheet / preflight /
    /// revalidation / cancel change clears it in memory; stored receipt history is never deleted.
    private(set) var persistedConfirmation: PersistedConfirmedScientificImportManifest?
    /// Decision-only summaries of saved records discovered under the current source's exact SHA.
    /// Populated by discovery only — never auto-restored.
    private(set) var savedManifestSummaries: [StoredManifestSummary] = []
    /// The explicitly selected saved record to restore. Never defaulted or silently auto-selected.
    private(set) var selectedSavedRecordDigest: String?
    /// A user-facing status/error message for the persistence actions.
    private(set) var persistenceMessage: String?
    /// The identifier of the in-flight save/restore operation, or `nil` when idle. A completion may
    /// mutate state only while it is still the active operation, so a stale save/restore can never
    /// clobber a newer one (it is not a shared busy flag).
    private(set) var activePersistenceOperationID: Int?

    var isPersisting: Bool { activePersistenceOperationID != nil }

    private let manifestStore: ScientificImportManifestFileStore

    @ObservationIgnored private var generation = 0
    @ObservationIgnored private var persistenceOperationSequence = 0
    /// Cancels the in-flight persistence store task, if any. `cancel()` invokes it so a cancellation
    /// actually signals the store (its cooperative lock wait and pre-publication checkpoints), rather
    /// than merely clearing UI state.
    @ObservationIgnored private var activePersistenceCanceller: (id: Int, cancel: @Sendable () -> Void)?

    /// Installs the canceller for a persistence operation, paired with its operation ID. If an OLDER
    /// operation's canceller is still installed, it is SIGNALLED (cancelled) before being replaced — a
    /// superseded store task is never left running (which, under the store-global lock, would otherwise
    /// block this newer operation indefinitely).
    private func setPersistenceCanceller(_ id: Int, _ cancel: @escaping @Sendable () -> Void) {
        activePersistenceCanceller?.cancel()
        activePersistenceCanceller = (id, cancel)
    }

    /// Clears the canceller ONLY if it still belongs to `id`. An older, overlapped operation's
    /// completion can therefore clear only its own canceller and never erase a newer operation's.
    private func clearPersistenceCanceller(_ id: Int) {
        if activePersistenceCanceller?.id == id { activePersistenceCanceller = nil }
    }

    init(manifestStore: ScientificImportManifestFileStore = .productionDefault()) {
        self.manifestStore = manifestStore
    }

    private func beginPersistenceOperation() -> Int {
        persistenceOperationSequence &+= 1
        let id = persistenceOperationSequence
        activePersistenceOperationID = id
        return id
    }

    private func isCurrentPersistenceOperation(_ id: Int) -> Bool {
        activePersistenceOperationID == id
    }

    private func finishPersistenceOperation(_ id: Int) {
        if activePersistenceOperationID == id { activePersistenceOperationID = nil }
    }

    /// Applies a persistence completion only if this operation is still the current one and its
    /// generation is unchanged, then retires the operation. A stale completion mutates nothing that
    /// belongs to a newer operation.
    private func completePersistence(_ id: Int, _ requestGeneration: Int, _ mutate: () -> Void) {
        defer { finishPersistenceOperation(id) }
        guard requestGeneration == generation, isCurrentPersistenceOperation(id) else { return }
        mutate()
    }

    var worksheets: [CanonicalXLSXWorksheetDescriptor] {
        workbookInspection?.worksheets ?? []
    }

    var selectedWorksheet: CanonicalXLSXWorksheetDescriptor? {
        guard let selectedWorksheetSheetID else { return nil }
        return worksheets.first { $0.sheetID == selectedWorksheetSheetID }
    }

    var stagedImport: StagedScientificImport? {
        transportStaging?.stagedImport
    }

    /// The canonical scientific value remains transaction-bound and shadow-only. Keeping this as
    /// a projection of `validatedPreparation` avoids a second mutable source of truth.
    var shadowCanonicalImport: ShadowCanonicalScientificImport? {
        validatedPreparation?.shadowCanonicalImport
    }

    var canBindSourceFacts: Bool {
        guard source != nil, headerDecision != nil else { return false }
        switch source?.format {
        case .csv:
            return true
        case .xlsx:
            return selectedWorksheet != nil
        case .none:
            return false
        }
    }

    var isBusy: Bool {
        phase == .readingSource || phase == .stagingSource
            || phase == .preflightingSourceFacts || phase == .validatingScientificMeaning
            || isPersisting
    }

    /// Whether a single clean validated preparation is available to confirm and save, and is not
    /// already persisted this session. This is not an authority token; saving grants no analysis,
    /// detector, or export permission.
    var canConfirmAndSave: Bool {
        phase == .validatedPreparation && validatedPreparation != nil && persistedConfirmation == nil
    }

    /// Whether the explicitly selected saved manifest can be restored right now. This is the exact
    /// precondition `restoreAndVerify()` enforces, exposed so the UI can DISABLE the Restore action
    /// rather than let a doomed click fall through to a message: a record is explicitly selected, the
    /// review is in the clean Stage-A state (no Stage-B artifacts), no persistence operation is in
    /// flight, and nothing has already been restored or saved this session. Once a wrapper is
    /// installed (`persistedConfirmation != nil`), Restore is replaced by the verified-standing status
    /// and re-restoring is neither offered nor permitted.
    var canRestoreSelectedManifest: Bool {
        selectedSavedRecordDigest != nil
            && phase == .awaitingSourceDecisions
            && !isBusy
            && persistedConfirmation == nil
            && transportStaging == nil && manifestForm == nil && manifestDraft == nil
            && preparedImport == nil && validatedPreparation == nil && confirmedImport == nil
    }

    /// A fully validated putative-single-unit preparation. This is deliberately not an authority
    /// token, activation permission, detector permission, or export permission.
    var hasValidatedPutativeSingleUnitPreparation: Bool {
        guard phase == .validatedPreparation,
              preparedImport?.data.activityMode == .putativeSingleUnit,
              case .validation(let report) = reviewOutcome,
              !report.hasBlockingIssues else {
            return false
        }
        return true
    }

    func beginImport(from url: URL) async {
        let requestGeneration = startNewRequest()
        phase = .readingSource

        do {
            let boundedSource = try await Task.detached(priority: .userInitiated) {
                try BoundedScientificSourceReader.read(from: url)
            }.value

            guard requestGeneration == generation else { return }

            let inspection: CanonicalXLSXWorkbookInspection?
            switch boundedSource.format {
            case .csv:
                inspection = nil
            case .xlsx:
                let snapshot = boundedSource.snapshot
                inspection = try await Task.detached(priority: .userInitiated) {
                    try CanonicalXLSXWorkbookInspector.inspect(
                        data: snapshot,
                        limits: .supportedDatasetEnvelope
                    )
                }.value
            }

            guard requestGeneration == generation else { return }
            source = boundedSource
            workbookInspection = inspection
            phase = .awaitingSourceDecisions
            // Discovery only: surface any saved manifests for this exact source. Never auto-restores.
            await refreshSavedManifests()
        } catch {
            guard requestGeneration == generation else { return }
            phase = .failed
            failureMessage = Self.userMessage(for: error, operation: "read the selected source")
        }
    }

    /// Stages the reselected source under the applied header/worksheet decisions. Pure: it reads and
    /// returns a staging result without mutating coordinator state or bumping the generation. The
    /// worksheet is passed explicitly so a transactional Restore can stage into local values without
    /// touching `selectedWorksheetSheetID`.
    private func stageTransport(
        source: BoundedScientificSource,
        headerDecision: CanonicalTabularHeaderDecision,
        worksheet: CanonicalXLSXWorksheetDescriptor?
    ) async throws -> ScientificImportTransportStaging {
        switch source.format {
        case .csv:
            let snapshot = source.snapshot
            return try await Task.detached(priority: .userInitiated) {
                .csv(
                    try CanonicalCSVStagingReader.readWithProvenance(
                        data: snapshot,
                        headerDecision: headerDecision,
                        limits: .supportedDatasetEnvelope
                    )
                )
            }.value
        case .xlsx:
            guard let inspection = workbookInspection, let worksheet else {
                throw ScientificImportCoordinatorInternalError.selectedWorksheetUnavailable
            }
            return try await Task.detached(priority: .userInitiated) {
                .xlsx(
                    try CanonicalXLSXWorksheetStagingReader.readWithProvenance(
                        inspection: inspection,
                        worksheet: worksheet,
                        headerDecision: headerDecision,
                        limits: .supportedDatasetEnvelope
                    )
                )
            }.value
        }
    }

    func cancel() {
        activePersistenceCanceller?.cancel()
        generation &+= 1
        clearTransaction()
        phase = .idle
    }

    func selectHeaderDecision(_ decision: CanonicalTabularHeaderDecision) {
        guard headerDecision != decision else { return }
        generation &+= 1
        headerDecision = decision
        invalidateBoundReview()
    }

    @discardableResult
    func selectWorksheet(sheetID: UInt32) -> Bool {
        guard worksheets.contains(where: { $0.sheetID == sheetID }) else {
            failureMessage = ScientificImportCoordinatorInternalError
                .selectedWorksheetUnavailable.localizedDescription
            return false
        }
        guard selectedWorksheetSheetID != sheetID else { return true }
        generation &+= 1
        selectedWorksheetSheetID = sheetID
        invalidateBoundReview()
        return true
    }

    func bindSourceFacts() async {
        guard let source, let headerDecision, canBindSourceFacts else {
            failureMessage = "Select the worksheet (for XLSX) and explicitly confirm the header rule first."
            return
        }

        generation &+= 1
        retireConfirmation()
        let requestGeneration = generation
        phase = .stagingSource
        failureMessage = nil

        do {
            let staging = try await stageTransport(
                source: source, headerDecision: headerDecision, worksheet: selectedWorksheet
            )

            guard requestGeneration == generation else { return }
            guard staging.stagedImport.sourceTransactionBinding?.sourceBytesSHA256
                    == source.sourceSHA256 else {
                throw ScientificImportCoordinatorInternalError.sourceBindingDigestMismatch
            }

            transportStaging = staging
            manifestForm = ScientificImportManifestForm(
                stagedImport: staging.stagedImport,
                suggestedRecordingSegmentIDText: source.suggestedRecordingSegmentIDText
            )
            phase = .reviewingScientificMeaning
        } catch {
            guard requestGeneration == generation else { return }
            transportStaging = nil
            manifestForm = nil
            phase = .awaitingSourceDecisions
            failureMessage = Self.userMessage(for: error, operation: "bind the selected table")
        }
    }

    /// Runs the complete pre-authority checking chain. A clean result constructs a deterministic,
    /// in-memory, shadow-only canonical dataset fingerprint (a content digest of the confirmed
    /// scientific dataset). That fingerprint is not an authority receipt: it does not confirm or
    /// activate a dataset, is not a persisted manifest identity, does not replace active data or
    /// launch a detector, and grants no detector, review, result-package, or export authority.
    func validateScientificReview() async {
        guard let source,
              let transportStaging,
              let stagedImport,
              let form = manifestForm else {
            failureMessage = "Bind source facts before validating scientific meaning."
            return
        }

        let draft: ScientificImportManifestDraft
        do {
            draft = try form.makeManifestDraft(boundTo: stagedImport)
        } catch let error as ScientificImportManifestFormBuildError {
            clearPreparedReview()
            reviewOutcome = .formIssues(error.issues)
            phase = .reviewingScientificMeaning
            return
        } catch {
            clearPreparedReview()
            failureMessage = Self.userMessage(for: error, operation: "build the manifest draft")
            phase = .reviewingScientificMeaning
            return
        }

        generation &+= 1
        let requestGeneration = generation
        manifestDraft = draft
        preparedImport = nil
        validatedPreparation = nil
        confirmedImport = nil
        // A new validation generation always invalidates any persisted wrapper, on success, rejection,
        // or failure paths alike (this clears it up front, before the async pipeline runs).
        persistedConfirmation = nil
        reviewOutcome = nil
        failureMessage = nil
        phase = .validatingScientificMeaning

        enum PipelineResult: Sendable {
            case planIssues([ScientificImportPlanIssue])
            case normalizationIssues([ScientificImportNormalizationIssue], additionalCount: Int)
            case prepared(
                PreparedScientificImport,
                PreparedScientificImportValidationReport,
                ConfirmableScientificImport
            )
            case validationRejected(
                PreparedScientificImport,
                PreparedScientificImportValidationReport
            )
            case projectionFailure(
                PreparedScientificImport,
                PreparedScientificImportValidationReport,
                CanonicalScientificImportProjectionError
            )
            case unexpectedProjectionFailure(
                PreparedScientificImport,
                PreparedScientificImportValidationReport
            )
        }

        let result = await Task.detached(priority: .userInitiated) {
            do {
                let plan = try ScientificImportPlanResolver.resolve(
                    stagedImport: stagedImport,
                    draft: draft
                )
                let prepared = try ScientificImportNormalizer.normalize(resolvedPlan: plan)
                switch PreparedScientificImportValidator.validateForCanonicalProjection(prepared) {
                case .accepted(let validated):
                    do {
                        let confirmable = try CanonicalScientificImportProjector
                            .projectConfirmable(validated)
                        return PipelineResult.prepared(
                            prepared,
                            validated.validationReport,
                            confirmable
                        )
                    } catch let error as CanonicalScientificImportProjectionError {
                        return PipelineResult.projectionFailure(
                            prepared,
                            validated.validationReport,
                            error
                        )
                    } catch {
                        return PipelineResult.unexpectedProjectionFailure(
                            prepared,
                            validated.validationReport
                        )
                    }
                case .rejected(let report):
                    return PipelineResult.validationRejected(prepared, report)
                }
            } catch let error as ScientificImportPlanResolutionError {
                return PipelineResult.planIssues(error.issues)
            } catch let error as ScientificImportNormalizationError {
                return PipelineResult.normalizationIssues(
                    error.issues,
                    additionalCount: error.additionalIssueCount
                )
            } catch {
                return PipelineResult.normalizationIssues([.resolvedPlanInvariant], additionalCount: 0)
            }
        }.value

        guard requestGeneration == generation else { return }
        switch result {
        case .planIssues(let issues):
            reviewOutcome = .planIssues(issues)
            phase = .reviewingScientificMeaning
        case .normalizationIssues(let issues, let additionalCount):
            reviewOutcome = .normalizationIssues(issues, additionalCount: additionalCount)
            phase = .reviewingScientificMeaning
        case .validationRejected(let prepared, let report):
            preparedImport = prepared
            reviewOutcome = .validation(report)
            validatedPreparation = nil
            phase = .reviewingScientificMeaning
        case .prepared(let prepared, let report, let confirmable):
            preparedImport = prepared
            reviewOutcome = .validation(report)
            validatedPreparation = ValidatedScientificImportPreparation(
                source: source,
                transportStaging: transportStaging,
                manifestDraft: draft,
                preparedImport: prepared,
                validationReport: report,
                confirmable: confirmable
            )
            phase = .validatedPreparation
        case .projectionFailure(let prepared, let report, let error):
            preparedImport = prepared
            validatedPreparation = nil
            reviewOutcome = .validation(report)
            phase = .failed
            failureMessage = Self.userMessage(
                for: error,
                operation: "construct the shadow canonical dataset"
            )
        case .unexpectedProjectionFailure(let prepared, let report):
            preparedImport = prepared
            validatedPreparation = nil
            reviewOutcome = .validation(report)
            phase = .failed
            failureMessage = "An internal error prevented construction of the shadow canonical dataset."
        }
    }

    /// Constructs only the bare in-memory confirmation for a clean validated preparation. Production
    /// `Confirm & Save` uses `confirmAndSaveScientificImport()` to construct the same base and then pass
    /// it through the durable store; this helper itself performs no persistence. A clean validation never
    /// auto-confirms. It reuses the fingerprint already produced during validation, creates no active
    /// dataset and no authority, installs no `SpikeDataset`, and does not mutate the active document,
    /// detector runs, reviews, caches, exports, or the filesystem.
    func confirmScientificImport() {
        guard phase == .validatedPreparation,
              let validated = validatedPreparation,
              !validated.validationReport.hasBlockingIssues else {
            return
        }
        confirmedImport = ScientificImportConfirmationBuilder.confirm(validated.confirmable)
    }

    // MARK: - Confirm & Save

    /// The sole production save path. Builds the base confirmation from the sealed confirmable,
    /// projects the decision-only receipt (traversing metadata only — no restage/normalize/validate/
    /// project/fingerprint pass), and hands it to the atomic store, which writes, fully synchronizes,
    /// atomically publishes with no-replace, and reads the final path back through the production
    /// bounded reader. Only an exact readback match mints the sealed persisted wrapper. A failure
    /// leaves the active dataset and all detector/export state unchanged and offers a retry.
    func confirmAndSaveScientificImport() async {
        guard phase == .validatedPreparation,
              let validated = validatedPreparation,
              !validated.validationReport.hasBlockingIssues,
              let headerDecision else {
            persistenceMessage = "Validate the scientific review before saving."
            return
        }
        let base = ScientificImportConfirmationBuilder.confirm(validated.confirmable)
        confirmedImport = base
        persistedConfirmation = nil

        // The sealed base is the ONLY authority for the persisted header rule: the store derives it
        // from the base and never accepts a caller-supplied rule or receipt. As a defensive stale-UI
        // check only, compare the current UI header decision against the base-derived rule; this can
        // detect an inconsistency but is never the source of the persisted rule. An empty/mixed base is
        // a fail-closed derivation defect the store reports, so it is not pre-empted here.
        if let baseRule = try? ConfirmedScientificImportManifestPersistence.deriveHeaderRule(from: base),
           baseRule != Self.persistedHeaderRule(headerDecision) {
            persistenceMessage =
                "The header decision no longer matches the validated source. Re-validate before saving."
            return
        }

        // Save must not invalidate the validated preparation, so it captures the generation WITHOUT
        // bumping it. Any concurrent form/source/header/worksheet/validation change bumps the
        // generation, and a newer persistence operation supersedes this one; either way the stale
        // completion mutates nothing.
        let requestGeneration = generation
        let operationID = beginPersistenceOperation()
        persistenceMessage = nil

        // The durable store is the SOLE minter of persistence standing: it writes, runs the full
        // durability barrier, reads its own derived final path back, and matches it against the base
        // confirmation before returning the sealed wrapper.
        // Wrap the store call in a cancellable task the coordinator can signal from `cancel()`.
        let store = manifestStore
        let saveTask = Task { try await store.save(baseConfirmation: base) }
        setPersistenceCanceller(operationID) { saveTask.cancel() }
        let result: StoredSaveResult
        do {
            result = try await saveTask.value
            clearPersistenceCanceller(operationID)
        } catch let error as ScientificImportManifestStoreError {
            clearPersistenceCanceller(operationID)
            completePersistence(operationID, requestGeneration) {
                self.persistenceMessage = Self.saveErrorMessage(for: error)
            }
            return
        } catch {
            clearPersistenceCanceller(operationID)
            completePersistence(operationID, requestGeneration) {
                self.persistenceMessage = "Could not save the confirmed manifest."
            }
            return
        }
        completePersistence(operationID, requestGeneration) {
            self.persistedConfirmation = result.manifest
            self.persistenceMessage = result.outcome == .idempotentExisting
                ? "This manifest was already saved (identical bytes). Analysis remains locked."
                : "Confirmed manifest saved and verified. Analysis remains locked."
        }
        await refreshSavedManifests()
    }

    /// Discovers saved records for the current source's exact SHA. Discovery only — it summarizes
    /// candidates and never restores or auto-selects anything.
    func refreshSavedManifests() async {
        guard let source else {
            savedManifestSummaries = []
            return
        }
        let sha = source.sourceSHA256
        let requestGeneration = generation
        do {
            let discovery = try await manifestStore.discover(sourceSHA256: sha)
            guard requestGeneration == generation else { return }
            savedManifestSummaries = discovery.summaries
            // Surface unreadable records distinctly; never silently report them as "no records".
            if discovery.unreadableRecordCount > 0 {
                persistenceMessage =
                    "\(discovery.unreadableRecordCount) saved record(s) for this source could not be read."
            }
        } catch let error as ScientificImportManifestStoreError {
            // A bounded-work, lock-timeout, or unreadable-store condition must reach the UI distinctly
            // even though discovery returns zero records — never collapsed to a bare "no records".
            guard requestGeneration == generation else { return }
            savedManifestSummaries = []
            persistenceMessage = Self.discoveryErrorMessage(for: error)
        } catch {
            guard requestGeneration == generation else { return }
            savedManifestSummaries = []
            persistenceMessage = "The saved-manifest store could not be read for this source."
        }
    }

    private static func discoveryErrorMessage(for error: ScientificImportManifestStoreError) -> String {
        switch error {
        case .discoveryBudgetExceeded:
            return "The saved-manifest store for this source is too large to scan safely; saved records are not listed."
        case .lockTimeout:
            return "The saved-manifest store was busy; saved records could not be listed. Try again."
        case .storeUnreadable, .read:
            return "The saved-manifest store could not be read for this source."
        case .unsafePathObject:
            return "A saved-manifest store path was not a safe regular file or directory; saved records are not listed."
        case .durabilityUnavailable:
            return "Durable storage is unavailable, so saved records cannot be listed."
        case .invalidSourceSHA:
            return "The source identifier was invalid; saved records could not be listed."
        default:
            return "The saved-manifest store could not be read for this source."
        }
    }

    /// Explicitly selects a discovered saved record to restore. Even a single candidate must be
    /// selected explicitly; nothing is ever defaulted or auto-selected.
    func selectSavedManifest(recordDigest: String?) {
        // A saved-record selection must not change underneath an in-flight save/restore.
        guard activePersistenceOperationID == nil else { return }
        selectedSavedRecordDigest = recordDigest
    }

    // MARK: - Restore & Verify

    /// Restores confirmation for the reselected source ONLY by replaying the complete import chain and
    /// matching the result exactly. Reads the selected saved receipt through the bounded reader,
    /// applies its saved worksheet and header decision, re-stages the source, reconstructs the draft,
    /// runs exactly one full chain (stage → resolve → normalize → validate → project → fingerprint →
    /// confirm), and mints the sealed persisted wrapper ONLY on complete equality. A decoded receipt
    /// alone never restores confirmation, readiness, or authority.
    func restoreAndVerify() async {
        guard let source else {
            persistenceMessage = "Reselect the source file before restoring."
            return
        }
        guard let recordDigest = selectedSavedRecordDigest else {
            persistenceMessage = "Select a saved manifest to restore first."
            return
        }
        // Transactional precondition: Restore is permitted only from a clean, Stage-B-absent state, so
        // no prior shadow/validated/confirmation can coexist with a restored one, and a failed restore
        // leaves the previous transaction byte-for-byte and field-for-field unchanged.
        guard phase == .awaitingSourceDecisions,
              transportStaging == nil, manifestForm == nil, manifestDraft == nil,
              preparedImport == nil, validatedPreparation == nil,
              confirmedImport == nil, persistedConfirmation == nil else {
            persistenceMessage = "Rebind or cancel the current review before restoring a saved manifest."
            return
        }

        generation &+= 1
        let requestGeneration = generation
        let operationID = beginPersistenceOperation()
        let sourceSHA = source.sourceSHA256
        persistenceMessage = nil

        // 1. Read the saved record as an UNTRUSTED bare receipt (from the store's own derived path) to
        //    drive the replay. It carries no proof and removes no readiness blocker; standing is minted
        //    only by the store's `restoreVerify` re-read in step 6.
        let receipt: ConfirmedScientificImportReceipt
        do {
            receipt = try await manifestStore.readForReplay(sourceSHA256: sourceSHA, recordDigest: recordDigest)
        } catch let error as ScientificImportManifestStoreError {
            completePersistence(operationID, requestGeneration) {
                self.persistenceMessage = Self.restoreStoreErrorMessage(for: error)
            }
            return
        } catch {
            completePersistence(operationID, requestGeneration) {
                self.persistenceMessage = "Could not read the saved manifest."
            }
            return
        }
        guard requestGeneration == generation, isCurrentPersistenceOperation(operationID) else {
            finishPersistenceOperation(operationID)
            return
        }

        // 2. Source-binding gate: the receipt must belong to this exact reselected source.
        guard receipt.sourceTransactionBinding.sourceBytesSHA256 == sourceSHA,
              Self.selection(receipt.sourceTransactionBinding.selection, matches: source.format) else {
            completePersistence(operationID, requestGeneration) {
                self.persistenceMessage = "This saved manifest was recorded for a different source."
            }
            return
        }

        // 3. Resolve the saved worksheet + header decision into LOCAL values (no coordinator mutation).
        let appliedHeaderRule = receipt.headerRule
        let appliedDecision = Self.tabularHeaderDecision(appliedHeaderRule)
        var localWorksheet: CanonicalXLSXWorksheetDescriptor?
        if case .excelWorksheet(_, let sheetID, _, _) = receipt.sourceTransactionBinding.selection {
            guard let worksheet = worksheets.first(where: { $0.sheetID == sheetID }) else {
                completePersistence(operationID, requestGeneration) {
                    self.persistenceMessage = "The saved worksheet is not present in this workbook."
                }
                return
            }
            localWorksheet = worksheet
        }

        // 4. Re-stage the reselected source into a LOCAL value.
        let staging: ScientificImportTransportStaging
        do {
            staging = try await stageTransport(
                source: source, headerDecision: appliedDecision, worksheet: localWorksheet
            )
        } catch {
            completePersistence(operationID, requestGeneration) {
                self.persistenceMessage = "Could not re-stage the source for verification."
            }
            return
        }
        guard requestGeneration == generation, isCurrentPersistenceOperation(operationID) else {
            finishPersistenceOperation(operationID)
            return
        }
        guard staging.stagedImport.sourceTransactionBinding?.sourceBytesSHA256 == sourceSHA else {
            completePersistence(operationID, requestGeneration) {
                self.persistenceMessage = "The re-staged table does not match the source snapshot."
            }
            return
        }

        // 5. Reconstruct the draft and run exactly one complete chain, off the main actor.
        let stagedImport = staging.stagedImport
        let outcome = await Task.detached(priority: .userInitiated) {
            Self.replayRestore(receipt: receipt, stagedImport: stagedImport)
        }.value
        guard requestGeneration == generation, isCurrentPersistenceOperation(operationID) else {
            finishPersistenceOperation(operationID)
            return
        }
        guard case .confirmable(let confirmable) = outcome else {
            completePersistence(operationID, requestGeneration) {
                self.persistenceMessage = outcome.failureMessage
            }
            return
        }

        // 6. Re-read the store's derived final record and match it against the replayed base to mint
        //    standing. On any failure nothing else was mutated, so the previous clean transaction is
        //    untouched.
        let base = ScientificImportConfirmationBuilder.confirm(confirmable)
        // Wrap the store call in a cancellable task the coordinator can signal from `cancel()`.
        let store = manifestStore
        let restoreTask = Task {
            try await store.restoreVerify(
                sourceSHA256: sourceSHA, recordDigest: recordDigest, baseConfirmation: base
            )
        }
        setPersistenceCanceller(operationID) { restoreTask.cancel() }
        let wrapper: PersistedConfirmedScientificImportManifest
        do {
            wrapper = try await restoreTask.value
            clearPersistenceCanceller(operationID)
        } catch let error as ScientificImportManifestStoreError {
            clearPersistenceCanceller(operationID)
            completePersistence(operationID, requestGeneration) {
                self.persistenceMessage = Self.restoreStoreErrorMessage(for: error)
            }
            return
        } catch {
            clearPersistenceCanceller(operationID)
            completePersistence(operationID, requestGeneration) {
                self.persistenceMessage = "Could not verify the saved manifest."
            }
            return
        }
        // Atomic commit: install the receipt's header + worksheet selections together with the restored
        // base confirmation and persisted wrapper, so the Stage-A UI and the coordinator show one
        // consistent restored transaction.
        completePersistence(operationID, requestGeneration) {
            if case .excelWorksheet(_, let sheetID, _, _) = receipt.sourceTransactionBinding.selection {
                self.selectedWorksheetSheetID = sheetID
            }
            self.headerDecision = appliedDecision
            self.confirmedImport = base
            self.persistedConfirmation = wrapper
            self.persistenceMessage =
                "Restored and verified from the saved manifest. Analysis remains locked."
        }
    }

    private enum RestoreChainOutcome: Sendable {
        case confirmable(ConfirmableScientificImport)
        case reconstructionFailed
        case planIssues
        case normalizationIssues
        case validationRejected
        case projectionFailed

        var failureMessage: String {
            switch self {
            case .confirmable:
                return ""
            case .reconstructionFailed:
                return "The saved decisions could not be rebuilt against the reselected source."
            case .planIssues:
                return "The saved decisions no longer resolve against the reselected source."
            case .normalizationIssues:
                return "The reselected source did not normalize to the saved shape."
            case .validationRejected:
                return "Independent validation rejected the reselected source."
            case .projectionFailed:
                return "The reselected source did not project to a canonical dataset."
            }
        }
    }

    nonisolated private static func replayRestore(
        receipt: ConfirmedScientificImportReceipt,
        stagedImport: StagedScientificImport
    ) -> RestoreChainOutcome {
        let draft: ScientificImportManifestDraft
        do {
            draft = try ConfirmedScientificImportManifestPersistence.reconstructDraft(
                from: receipt, boundTo: stagedImport
            )
        } catch {
            return .reconstructionFailed
        }
        do {
            let plan = try ScientificImportPlanResolver.resolve(stagedImport: stagedImport, draft: draft)
            let prepared = try ScientificImportNormalizer.normalize(resolvedPlan: plan)
            switch PreparedScientificImportValidator.validateForCanonicalProjection(prepared) {
            case .accepted(let validated):
                do {
                    return .confirmable(try CanonicalScientificImportProjector.projectConfirmable(validated))
                } catch {
                    return .projectionFailed
                }
            case .rejected:
                return .validationRejected
            }
        } catch is ScientificImportPlanResolutionError {
            return .planIssues
        } catch {
            return .normalizationIssues
        }
    }

    // MARK: - Persistence boundary mappers and messages

    private static func persistedHeaderRule(
        _ decision: CanonicalTabularHeaderDecision
    ) -> PersistedTabularHeaderRule {
        switch decision {
        case .firstRecordIsHeader: return .firstRecordIsHeader
        case .headerless: return .headerless
        }
    }

    private static func tabularHeaderDecision(
        _ rule: PersistedTabularHeaderRule
    ) -> CanonicalTabularHeaderDecision {
        switch rule {
        case .firstRecordIsHeader: return .firstRecordIsHeader
        case .headerless: return .headerless
        }
    }

    private static func selection(
        _ selection: StagedSourceTransactionSelection,
        matches format: ScientificSourceFormat
    ) -> Bool {
        switch (selection, format) {
        case (.commaSeparatedValues, .csv): return true
        case (.excelWorksheet, .xlsx): return true
        default: return false
        }
    }

    private static func saveErrorMessage(for error: ScientificImportManifestStoreError) -> String {
        switch error {
        case .durabilityUnavailable:
            return "Durable storage is unavailable, so the manifest was not saved."
        case .conflictingBytesAtTarget:
            return "A different saved manifest already exists at this record path; the existing receipt was not overwritten."
        case .candidateLimitExceeded(let maximum):
            return "This source already has the maximum of \(maximum) saved manifests. Nothing was deleted."
        case .oversizedOrNoncanonicalEncoding:
            return "The confirmation record could not be encoded within the supported bounds; nothing was saved."
        case .readbackMismatch:
            // These failures can occur after a link is published (during the durability barrier), so
            // promise only that no persistence standing was created — never that nothing was published.
            return "The written manifest did not read back exactly; no persistence standing was created. Try again."
        case .durabilitySyncFailed:
            return "The store could not be synchronized durably; no persistence standing was created. Try again."
        case .unsafePathObject:
            return "A store path was not a safe regular file or directory; no persistence standing was created."
        case .pathIdentityChanged:
            return "A store path changed identity during the operation; no persistence standing was created. Try again."
        case .temporaryCleanupFailed:
            // A post-link cleanup failure leaves the written record VISIBLE and retryable — it is not yet
            // guaranteed crash-durable (the directory barrier did not complete), and no standing was made.
            return "The store could not be left clean; no persistence standing was created. Any written record remains visible and can be safely retried."
        case .lockTimeout:
            return "The saved-manifest store was busy and the operation timed out. Try again."
        case .operationCancelled:
            // Neutral about any post-publication record: a cancellation after the link is published
            // leaves a visible record a later explicit retry can restore. Only the in-memory standing is
            // guaranteed not created here.
            return "The save was cancelled; no persistence standing was created. Any already-written record can be restored on a later retry."
        case .invalidSourceSHA, .invalidRecordDigest:
            return "The confirmation-record identifiers were invalid; nothing was saved."
        case .writeFailed, .publishFailed:
            return "The manifest could not be written to the store. Try again."
        case .storeUnreadable:
            return "The saved-manifest store could not be read."
        case .discoveryBudgetExceeded:
            return "The saved-manifest store for this source is too large to scan safely."
        case .recordNotFound:
            return "The saved manifest record was not found."
        case .headerRuleDerivation(let derivation):
            return Self.headerRuleDerivationCause(for: derivation)
                + " Nothing was saved and no persistence standing was created."
        case .read:
            return "A saved manifest file could not be read."
        case .verification(let verification):
            return Self.verificationErrorMessage(for: verification)
        }
    }

    private static func restoreStoreErrorMessage(for error: ScientificImportManifestStoreError) -> String {
        switch error {
        case .verification(let verification):
            return Self.verificationErrorMessage(for: verification)
        case .headerRuleDerivation(let derivation):
            return Self.headerRuleDerivationCause(for: derivation)
                + " Restore and verification stopped; no persistence standing was restored."
        case .read(.decode):
            return "The saved manifest is corrupt or unsupported and cannot be restored."
        case .read(.notRegularFile):
            return "The saved manifest path is not a regular file and was rejected."
        case .read(.fileTooLarge):
            return "The saved manifest exceeds the supported size and was rejected."
        case .read:
            return "The saved manifest file could not be read."
        case .recordNotFound:
            return "The selected saved manifest is no longer available for this source."
        case .storeUnreadable, .discoveryBudgetExceeded:
            return "The saved-manifest store could not be read."
        case .lockTimeout:
            return "The saved-manifest store was busy and the restore timed out. Try again."
        case .unsafePathObject:
            return "A store path was not a safe regular file; the saved manifest was rejected."
        case .pathIdentityChanged:
            return "A store path changed identity during verification; the restore stopped and no persistence standing was restored. Try again."
        case .temporaryCleanupFailed:
            return "The store could not be left clean during verification; the restore stopped and no persistence standing was restored. Try again."
        case .operationCancelled:
            return "The restore was cancelled; no persistence standing was restored."
        case .durabilityUnavailable:
            return "Durable storage is unavailable, so the manifest could not be restored."
        default:
            return "The saved manifest could not be read."
        }
    }

    /// The neutral cause of a header-rule derivation failure, with no operation-specific outcome. Save
    /// and restore callers append their own accurate suffix so a restore-time failure never claims the
    /// save-only "nothing was saved".
    private static func headerRuleDerivationCause(
        for error: PersistedHeaderRuleDerivationError
    ) -> String {
        switch error {
        case .noSourceColumns:
            return "The validated source has no columns, so its header rule could not be determined."
        case .mixedHeaderPresence:
            return "The validated source mixes header-present and header-absent columns, which is not a valid header state."
        }
    }

    private static func verificationErrorMessage(
        for error: PersistedConfirmationVerificationError
    ) -> String {
        switch error {
        case .sourceBindingMismatch:
            return "Source-binding mismatch: the saved manifest does not belong to this exact source."
        case .canonicalFingerprintMismatch:
            return "Replay divergence: the reselected source reproduces a different scientific dataset."
        case .confirmationRecordMismatch:
            return "Confirmation-record mismatch: the scientific dataset matches, but a confirmed decision differs."
        }
    }

    /// A deny-only, fail-closed analysis-readiness assessment, or `nil` when nothing is confirmed. It
    /// is informational and never grants authority. A verified persisted wrapper removes only the
    /// persistence blocker; a bare confirmation still reports it.
    var analysisReadiness: ScientificAnalysisReadinessAssessment? {
        if let persistedConfirmation {
            return ScientificAnalysisReadinessEvaluator.assess(persistedConfirmation)
        }
        return confirmedImport.map(ScientificAnalysisReadinessEvaluator.assess)
    }

    /// Discovers source facts for the user's current explicit column/group proposal. The Core
    /// preflight shares its lexical scanner with the normalizer; keys added to the form remain
    /// unresolved for type/role/unit/empty-policy decisions.
    func refreshPreflight() async {
        guard let stagedImport, let form = manifestForm else {
            failureMessage = "Bind source facts before running the source preflight."
            return
        }

        // Preflight needs valid source keys and their explicit roles, but does not need (and must
        // not invent) type/unit/empty-policy decisions. Ignore incomplete manual key rows for this
        // discovery pass; they remain visible and blocking in the final manifest review.
        var preflightForm = form
        preflightForm.attributes = form.attributes.compactMap { attribute in
            guard (try? EventAttributeKey(validating: attribute.keyText)) != nil else {
                return nil
            }
            var copy = attribute
            copy.unitChoice = nil
            copy.specifiedUnitText = ""
            return copy
        }

        let draft: ScientificImportManifestDraft
        do {
            draft = try preflightForm.makeManifestDraft(boundTo: stagedImport)
        } catch let error as ScientificImportManifestFormBuildError {
            preflightReport = nil
            preflightFormIssues = error.issues
            phase = .reviewingScientificMeaning
            return
        } catch {
            preflightReport = nil
            preflightFormIssues = []
            failureMessage = Self.userMessage(for: error, operation: "prepare source preflight")
            phase = .reviewingScientificMeaning
            return
        }

        generation &+= 1
        retireConfirmation()
        let requestGeneration = generation
        preflightReport = nil
        preflightFormIssues = []
        failureMessage = nil
        phase = .preflightingSourceFacts

        let report = await Task.detached(priority: .userInitiated) {
            ScientificImportPreflight.inspect(stagedImport: stagedImport, draft: draft)
        }.value

        guard requestGeneration == generation else { return }
        var mergedForm = form
        let existingKeys = Set(mergedForm.attributes.compactMap { attribute in
            try? EventAttributeKey(validating: attribute.keyText)
        })
        var knownKeys = existingKeys
        for discovered in report.discoveredEventAttributes where !knownKeys.contains(discovered.key) {
            mergedForm.addAttribute(keyText: discovered.key.canonicalText)
            knownKeys.insert(discovered.key)
        }
        if mergedForm.attributes.count != form.attributes.count {
            manifestForm = mergedForm
        }
        preflightReport = report
        preflightFormIssues = []
        phase = .reviewingScientificMeaning
    }

    private func startNewRequest() -> Int {
        generation &+= 1
        clearTransaction()
        return generation
    }

    private func clearTransaction() {
        source = nil
        workbookInspection = nil
        headerDecision = nil
        selectedWorksheetSheetID = nil
        transportStaging = nil
        manifestForm = nil
        clearPreparedReview()
        preflightReport = nil
        preflightFormIssues = []
        failureMessage = nil
        // Discovery and selection are source-specific; a new/cancelled source clears them. Stored
        // receipts on disk are never deleted.
        savedManifestSummaries = []
        selectedSavedRecordDigest = nil
        persistenceMessage = nil
        activePersistenceOperationID = nil
        activePersistenceCanceller = nil
    }

    private func invalidateBoundReview() {
        transportStaging = nil
        manifestForm = nil
        clearPreparedReview()
        preflightReport = nil
        preflightFormIssues = []
        failureMessage = nil
        if source != nil {
            phase = .awaitingSourceDecisions
        }
    }

    private static func userMessage(for error: Error, operation: String) -> String {
        if let localized = error as? LocalizedError,
           let description = localized.errorDescription,
           !description.isEmpty {
            return description
        }
        return "Could not \(operation): \(String(describing: error))"
    }

    private func clearPreparedReview() {
        manifestDraft = nil
        preparedImport = nil
        validatedPreparation = nil
        confirmedImport = nil
        // Invalidate the in-memory persisted wrapper; stored receipt history on disk is never deleted.
        persistedConfirmation = nil
        reviewOutcome = nil
        preflightReport = nil
        preflightFormIssues = []
    }

    /// Retires any confirmation the instant a new source-bound or validation-generation operation
    /// begins, before its asynchronous work runs. Generation-changing methods that route through
    /// `clearPreparedReview`, `clearTransaction`, or `invalidateBoundReview` already clear it; this
    /// is the direct path for `bindSourceFacts` and `refreshPreflight`, which do not.
    private func retireConfirmation() {
        confirmedImport = nil
        persistedConfirmation = nil
    }
}
