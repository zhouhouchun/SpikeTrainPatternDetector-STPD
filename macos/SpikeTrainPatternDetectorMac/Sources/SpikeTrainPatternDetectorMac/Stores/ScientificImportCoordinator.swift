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
/// No method in this type installs a `SpikeDataset`, grants analysis authority, or launches a
/// detector. A later canonical projection/authority slice must provide that separate boundary.
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
    private(set) var reviewOutcome: ScientificImportReviewOutcome?
    private(set) var preflightReport: ScientificImportPreflightReport?
    private(set) var preflightFormIssues: [ScientificImportManifestFormIssue] = []
    private(set) var failureMessage: String?

    @ObservationIgnored private var generation = 0

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
        } catch {
            guard requestGeneration == generation else { return }
            phase = .failed
            failureMessage = Self.userMessage(for: error, operation: "read the selected source")
        }
    }

    func cancel() {
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
        let requestGeneration = generation
        phase = .stagingSource
        failureMessage = nil

        do {
            let staging: ScientificImportTransportStaging
            switch source.format {
            case .csv:
                let snapshot = source.snapshot
                staging = try await Task.detached(priority: .userInitiated) {
                    .csv(
                        try CanonicalCSVStagingReader.readWithProvenance(
                            data: snapshot,
                            headerDecision: headerDecision,
                            limits: .supportedDatasetEnvelope
                        )
                    )
                }.value
            case .xlsx:
                guard let inspection = workbookInspection,
                      let worksheet = selectedWorksheet else {
                    throw ScientificImportCoordinatorInternalError.selectedWorksheetUnavailable
                }
                staging = try await Task.detached(priority: .userInitiated) {
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

            guard requestGeneration == generation else { return }
            guard staging.stagedImport.sourceTransactionBinding?.sourceBytesSHA256
                    == source.sourceSHA256 else {
                throw ScientificImportCoordinatorInternalError.sourceBindingDigestMismatch
            }

            transportStaging = staging
            manifestForm = ScientificImportManifestForm(stagedImport: staging.stagedImport)
            phase = .reviewingScientificMeaning
        } catch {
            guard requestGeneration == generation else { return }
            transportStaging = nil
            manifestForm = nil
            phase = .awaitingSourceDecisions
            failureMessage = Self.userMessage(for: error, operation: "bind the selected table")
        }
    }

    /// Runs the complete pre-authority checking chain. A clean result remains only a validated
    /// preparation: it does not confirm a manifest, project canonical identity, replace active
    /// data, or grant detector/export authority.
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
        reviewOutcome = nil
        failureMessage = nil
        phase = .validatingScientificMeaning

        enum PipelineResult: Sendable {
            case planIssues([ScientificImportPlanIssue])
            case normalizationIssues([ScientificImportNormalizationIssue], additionalCount: Int)
            case prepared(PreparedScientificImport, PreparedScientificImportValidationReport)
        }

        let result = await Task.detached(priority: .userInitiated) {
            do {
                let plan = try ScientificImportPlanResolver.resolve(
                    stagedImport: stagedImport,
                    draft: draft
                )
                let prepared = try ScientificImportNormalizer.normalize(resolvedPlan: plan)
                let report = PreparedScientificImportValidator.validate(prepared)
                return PipelineResult.prepared(prepared, report)
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
        case .prepared(let prepared, let report):
            preparedImport = prepared
            reviewOutcome = .validation(report)
            if report.hasBlockingIssues {
                validatedPreparation = nil
                phase = .reviewingScientificMeaning
            } else {
                validatedPreparation = ValidatedScientificImportPreparation(
                    source: source,
                    transportStaging: transportStaging,
                    manifestDraft: draft,
                    preparedImport: prepared,
                    validationReport: report
                )
                phase = .validatedPreparation
            }
        }
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
        reviewOutcome = nil
        preflightReport = nil
        preflightFormIssues = []
    }
}
