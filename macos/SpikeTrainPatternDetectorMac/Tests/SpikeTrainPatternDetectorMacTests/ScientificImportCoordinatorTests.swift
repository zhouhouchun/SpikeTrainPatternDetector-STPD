import Foundation
import STPDCore
import STPDTabularIO
@testable import SpikeTrainPatternDetectorMac
import Testing
import ZIPFoundation

@Suite("Scientific import coordinator", .serialized)
@MainActor
struct ScientificImportCoordinatorTests {
    @Test("CSV source facts require an explicit header decision before staging")
    func csvRequiresExplicitHeaderDecision() async throws {
        try await withTemporaryDirectory { directory in
            let url = directory.appendingPathComponent("source.csv")
            try Data("unit_A,event_stimulus\n1.000000,2.000000\n".utf8).write(to: url)
            let coordinator = ScientificImportCoordinator()

            await coordinator.beginImport(from: url)

            #expect(coordinator.phase == .awaitingSourceDecisions)
            #expect(coordinator.source?.format == .csv)
            #expect(coordinator.headerDecision == nil)
            #expect(!coordinator.canBindSourceFacts)
            #expect(coordinator.stagedImport == nil)
            #expect(coordinator.manifestForm == nil)

            coordinator.selectHeaderDecision(.firstRecordIsHeader)
            #expect(coordinator.canBindSourceFacts)
            await coordinator.bindSourceFacts()

            #expect(coordinator.phase == .reviewingScientificMeaning)
            #expect(coordinator.stagedImport?.columns.map(\.header) == ["unit_A", "event_stimulus"])
            #expect(coordinator.stagedImport?.dataRowCount == 1)
            #expect(coordinator.manifestForm?.sourceTimeUnit == nil)
            #expect(coordinator.manifestForm?.activityMode == nil)
            #expect(coordinator.manifestForm?.columns.allSatisfy { $0.role == nil } == true)
        }
    }

    @Test("Changing a bound source fact discards every Stage B choice")
    func changingHeaderInvalidatesReview() async throws {
        try await withTemporaryDirectory { directory in
            let url = directory.appendingPathComponent("source.csv")
            try Data("unit_A\n1.000000\n".utf8).write(to: url)
            let coordinator = ScientificImportCoordinator()
            await coordinator.beginImport(from: url)
            coordinator.selectHeaderDecision(.firstRecordIsHeader)
            await coordinator.bindSourceFacts()
            coordinator.manifestForm?.sourceTimeUnit = .seconds
            coordinator.manifestForm?.activityMode = .putativeSingleUnit

            coordinator.selectHeaderDecision(.headerless)

            #expect(coordinator.phase == .awaitingSourceDecisions)
            #expect(coordinator.headerDecision == .headerless)
            #expect(coordinator.transportStaging == nil)
            #expect(coordinator.manifestForm == nil)
        }
    }

    @Test("XLSX requires explicit worksheet selection even with one worksheet")
    func xlsxRequiresExplicitWorksheetSelection() async throws {
        try await withTemporaryDirectory { directory in
            let url = directory.appendingPathComponent("source.xlsx")
            try makeMinimalXLSX().write(to: url)
            let coordinator = ScientificImportCoordinator()

            await coordinator.beginImport(from: url)

            #expect(coordinator.phase == .awaitingSourceDecisions)
            #expect(coordinator.worksheets.count == 1)
            #expect(coordinator.selectedWorksheet == nil)
            coordinator.selectHeaderDecision(.firstRecordIsHeader)
            #expect(!coordinator.canBindSourceFacts)

            let worksheet = try #require(coordinator.worksheets.first)
            #expect(coordinator.selectWorksheet(sheetID: worksheet.sheetID))
            #expect(coordinator.canBindSourceFacts)
            await coordinator.bindSourceFacts()

            #expect(coordinator.phase == .reviewingScientificMeaning)
            #expect(coordinator.stagedImport?.columns.first?.header == "unit_A")
            #expect(coordinator.stagedImport?.dataRowCount == 1)

            configureOneSpikeTrain(in: coordinator, mode: .putativeSingleUnit)
            await coordinator.validateScientificReview()
            let validated = try #require(coordinator.validatedPreparation)
            guard case .xlsx(let xlsxStaging) = validated.transportStaging else {
                Issue.record("Expected XLSX transport provenance")
                return
            }
            #expect(xlsxStaging.provenance.headerDecision == .firstRecordIsHeader)
            #expect(xlsxStaging.provenance.sheetID == worksheet.sheetID)
            #expect(xlsxStaging.provenance.relationshipID == worksheet.relationshipID)
            #expect(xlsxStaging.provenance.normalizedPartPath == worksheet.normalizedPartPath)
            #expect(xlsxStaging.provenance.sourceSHA256 == validated.source.sourceSHA256)
        }
    }

    @Test("Staging uses the exact snapshot even if the source path later changes")
    func stagingUsesOwnedSnapshot() async throws {
        try await withTemporaryDirectory { directory in
            let url = directory.appendingPathComponent("source.csv")
            let reviewedBytes = Data("unit_A\n1.000000\n".utf8)
            try reviewedBytes.write(to: url)
            let coordinator = ScientificImportCoordinator()
            await coordinator.beginImport(from: url)
            let reviewedDigest = coordinator.source?.sourceSHA256

            try Data("unit_B\n9.000000\n10.000000\n".utf8).write(to: url)
            coordinator.selectHeaderDecision(.firstRecordIsHeader)
            await coordinator.bindSourceFacts()

            #expect(coordinator.stagedImport?.columns.first?.header == "unit_A")
            #expect(coordinator.stagedImport?.dataRowCount == 1)
            #expect(coordinator.stagedImport?.sourceTransactionBinding?.sourceBytesSHA256 == reviewedDigest)
        }
    }

    @Test("Cancel discards only the import transaction")
    func cancelClearsTransaction() async throws {
        try await withTemporaryDirectory { directory in
            let url = directory.appendingPathComponent("source.csv")
            try Data("unit_A\n1.000000\n".utf8).write(to: url)
            let coordinator = ScientificImportCoordinator()
            await coordinator.beginImport(from: url)

            coordinator.cancel()

            #expect(coordinator.phase == .idle)
            #expect(coordinator.source == nil)
            #expect(coordinator.workbookInspection == nil)
            #expect(coordinator.headerDecision == nil)
            #expect(coordinator.stagedImport == nil)
            #expect(coordinator.manifestForm == nil)
        }
    }

    @Test("A complete single-unit draft reaches validated preparation without activation")
    func completeDraftReachesValidatedPreparation() async throws {
        try await withTemporaryDirectory { directory in
            let url = directory.appendingPathComponent("source.csv")
            try Data("unit_A\n1.000000\n1.250000\n".utf8).write(to: url)
            let coordinator = ScientificImportCoordinator()
            await coordinator.beginImport(from: url)
            coordinator.selectHeaderDecision(.firstRecordIsHeader)
            await coordinator.bindSourceFacts()
            configureOneSpikeTrain(
                in: coordinator,
                mode: .putativeSingleUnit
            )

            await coordinator.validateScientificReview()

            #expect(coordinator.phase == .validatedPreparation)
            #expect(coordinator.preparedImport?.data.activityMode == .putativeSingleUnit)
            #expect(coordinator.hasValidatedPutativeSingleUnitPreparation)
            let validated = try #require(coordinator.validatedPreparation)
            let validatedStagedImport = try #require(coordinator.stagedImport)
            #expect(validated.source.sourceSHA256 == coordinator.source?.sourceSHA256)
            #expect(validated.manifestDraft.sourceBinding
                == ScientificImportDraftSourceBinding(stagedImport: validatedStagedImport))
            #expect(validated.preparedImport == coordinator.preparedImport)
            #expect(!validated.validationReport.hasBlockingIssues)
            #expect(coordinator.shadowCanonicalImport == validated.shadowCanonicalImport)
            #expect(validated.shadowCanonicalImport.sourceTransactionBinding
                == validatedStagedImport.sourceTransactionBinding)
            #expect(validated.shadowCanonicalImport.dataset.spikeTrains[0]
                .rawTimestamps.map(\.microseconds)
                == [1_000_000, 1_250_000])
            // The shadow carries a non-authoritative fingerprint bound to this canonical shape.
            #expect(validated.shadowCanonicalImport.fingerprint.schemaContractID
                == "canonical_microsecond_event_scope_dataset")
            #expect(validated.shadowCanonicalImport.fingerprint.datasetDigest.count == 64)
            guard case .csv(let csvStaging) = validated.transportStaging else {
                Issue.record("Expected CSV transport provenance")
                return
            }
            #expect(csvStaging.provenance.headerDecision == .firstRecordIsHeader)
            #expect(csvStaging.provenance.sourceSHA256 == validated.source.sourceSHA256)
            guard case .validation(let report) = coordinator.reviewOutcome else {
                Issue.record("Expected an independent validation report")
                return
            }
            #expect(!report.hasBlockingIssues)
        }
    }

    @Test("Single-unit virtual collapse never removes canonical raw duplicate spikes")
    func virtualCollapsePreservesShadowRawMultiplicity() async throws {
        try await withTemporaryDirectory { directory in
            let url = directory.appendingPathComponent("duplicates.csv")
            try Data("unit_A\n1.000000\n1.000000\n1.250000\n".utf8).write(to: url)
            let coordinator = ScientificImportCoordinator()
            await coordinator.beginImport(from: url)
            coordinator.selectHeaderDecision(.firstRecordIsHeader)
            await coordinator.bindSourceFacts()
            configureOneSpikeTrain(
                in: coordinator,
                mode: .putativeSingleUnit,
                duplicateDecision: .collapseExact
            )

            await coordinator.validateScientificReview()

            #expect(coordinator.phase == .validatedPreparation)
            let prepared = try #require(coordinator.preparedImport)
            let shadow = try #require(coordinator.shadowCanonicalImport)
            #expect(prepared.data.eventScopeGroups[0].spikeTrains[0]
                .timestamps.map(\.microseconds) == [1_000_000, 1_000_000, 1_250_000])
            #expect(shadow.dataset.spikeTrains[0]
                .rawTimestamps.map(\.microseconds) == [1_000_000, 1_000_000, 1_250_000])
            #expect(prepared.provenance.eventScopeGroups[0].spikeTrains[0]
                .duplicateDecision == .collapseExact)
        }
    }

    @Test("Editing the manifest invalidates the complete shadow transaction")
    func manifestEditClearsShadowCanonicalImport() async throws {
        try await withTemporaryDirectory { directory in
            let url = directory.appendingPathComponent("source.csv")
            try Data("unit_A\n1.000000\n1.250000\n".utf8).write(to: url)
            let coordinator = ScientificImportCoordinator()
            await coordinator.beginImport(from: url)
            coordinator.selectHeaderDecision(.firstRecordIsHeader)
            await coordinator.bindSourceFacts()
            configureOneSpikeTrain(in: coordinator, mode: .putativeSingleUnit)
            await coordinator.validateScientificReview()
            _ = try #require(coordinator.shadowCanonicalImport)

            var edited = try #require(coordinator.manifestForm)
            edited.activityMode = .unknownOrUncertain
            coordinator.manifestForm = edited

            #expect(coordinator.phase == .reviewingScientificMeaning)
            #expect(coordinator.preparedImport == nil)
            #expect(coordinator.validatedPreparation == nil)
            #expect(coordinator.shadowCanonicalImport == nil)
        }
    }

    @Test("Starting a replacement source transaction immediately retires the old shadow")
    func sourceReplacementClearsShadowCanonicalImport() async throws {
        try await withTemporaryDirectory { directory in
            let firstURL = directory.appendingPathComponent("first.csv")
            let replacementURL = directory.appendingPathComponent("replacement.csv")
            try Data("unit_A\n1.000000\n1.250000\n".utf8).write(to: firstURL)
            try Data("unit_B\n2.000000\n2.500000\n".utf8).write(to: replacementURL)
            let coordinator = ScientificImportCoordinator()
            await coordinator.beginImport(from: firstURL)
            coordinator.selectHeaderDecision(.firstRecordIsHeader)
            await coordinator.bindSourceFacts()
            configureOneSpikeTrain(in: coordinator, mode: .putativeSingleUnit)
            await coordinator.validateScientificReview()
            _ = try #require(coordinator.shadowCanonicalImport)
            let firstSourceDigest = try #require(coordinator.source?.sourceSHA256)

            await coordinator.beginImport(from: replacementURL)

            #expect(coordinator.phase == .awaitingSourceDecisions)
            #expect(coordinator.source?.sourceSHA256 != firstSourceDigest)
            #expect(coordinator.preparedImport == nil)
            #expect(coordinator.validatedPreparation == nil)
            #expect(coordinator.shadowCanonicalImport == nil)
            #expect(coordinator.reviewOutcome == nil)
        }
    }

    @Test("Core preflight discovers event keys without assigning scientific defaults")
    func preflightAddsUnresolvedEventKeys() async throws {
        try await withTemporaryDirectory { directory in
            let url = directory.appendingPathComponent("events.csv")
            try Data(
                "unit_A,event_stimulus\n0.000000,1.000000\n,@intensity=2.5\n,@intensity.unit=mW\n".utf8
            ).write(to: url)
            let coordinator = ScientificImportCoordinator()
            await coordinator.beginImport(from: url)
            coordinator.selectHeaderDecision(.firstRecordIsHeader)
            await coordinator.bindSourceFacts()

            var form = try #require(coordinator.manifestForm)
            form.sourceTimeUnit = .seconds
            form.columns[0].role = .spikeTrain
            form.columns[1].startsNewGroup = false
            form.columns[1].role = .eventDefinition
            coordinator.manifestForm = form

            await coordinator.refreshPreflight()

            let report = try #require(coordinator.preflightReport)
            #expect(report.discoveredEventAttributes.map(\.key.canonicalText) == ["intensity"])
            #expect(report.discoveredEventAttributes[0].inlineUnitSuggestions.map(\.rawUnit) == ["mW"])
            #expect(report.eventOriginCandidates.count == 1)
            let added = try #require(coordinator.manifestForm?.attributes.first)
            #expect(added.keyText == "intensity")
            #expect(added.scalarType == nil)
            #expect(added.role == nil)
            #expect(added.unitChoice == nil)
            #expect(added.emptyStringPolicy == nil)
        }
    }

    @Test("Unknown activity may validate for browsing but is never authority-eligible")
    func unknownModeRemainsBrowseOnly() async throws {
        try await withTemporaryDirectory { directory in
            let url = directory.appendingPathComponent("source.csv")
            try Data("unit_A\n1.000000\n1.250000\n".utf8).write(to: url)
            let coordinator = ScientificImportCoordinator()
            await coordinator.beginImport(from: url)
            coordinator.selectHeaderDecision(.firstRecordIsHeader)
            await coordinator.bindSourceFacts()
            configureOneSpikeTrain(
                in: coordinator,
                mode: .unknownOrUncertain
            )

            await coordinator.validateScientificReview()

            #expect(coordinator.phase == .validatedPreparation)
            #expect(coordinator.preparedImport?.data.activityMode == .unknownOrUncertain)
            #expect(!coordinator.hasValidatedPutativeSingleUnitPreparation)
        }
    }

    @Test("A timestamp that is not exactly representable in microseconds blocks preparation")
    func inexactTimestampBlocksPreparation() async throws {
        try await withTemporaryDirectory { directory in
            let url = directory.appendingPathComponent("source.csv")
            try Data("unit_A\n0.0000005\n".utf8).write(to: url)
            let coordinator = ScientificImportCoordinator()
            await coordinator.beginImport(from: url)
            coordinator.selectHeaderDecision(.firstRecordIsHeader)
            await coordinator.bindSourceFacts()
            configureOneSpikeTrain(
                in: coordinator,
                mode: .putativeSingleUnit
            )

            await coordinator.validateScientificReview()

            #expect(coordinator.phase == .reviewingScientificMeaning)
            #expect(coordinator.preparedImport == nil)
            #expect(!coordinator.hasValidatedPutativeSingleUnitPreparation)
            guard case .normalizationIssues(let issues, additionalCount: 0) = coordinator.reviewOutcome else {
                Issue.record("Expected an exact timestamp normalization blocker")
                return
            }
            #expect(issues.contains { issue in
                guard case .timestampParseFailed(_, _, let error) = issue else { return false }
                return error == .notExactlyRepresentableInMicroseconds
            })
        }
    }

    @Test("Staging and failed validation never mutate the active document")
    func reviewFailurePreservesActiveDocument() async throws {
        try await withTemporaryDirectory { directory in
            let activeURL = directory.appendingPathComponent("active.csv")
            try Data("active_unit\n1.0\n2.0\n".utf8).write(to: activeURL)
            let candidateURL = directory.appendingPathComponent("candidate.csv")
            try Data("candidate_unit\n0.0000005\n".utf8).write(to: candidateURL)

            let document = RasterDocument()
            document.loadCSV(
                from: activeURL,
                unit: .seconds,
                hasHeader: true,
                duplicatePolicy: .errorKeep
            )
            let activeDataset = try #require(document.dataset)
            document.classicAnchorReviewStatuses = ["preserved": .accepted]
            document.statusMessage = "Preserve this status"

            let coordinator = document.scientificImportCoordinator
            await coordinator.beginImport(from: candidateURL)
            coordinator.selectHeaderDecision(.firstRecordIsHeader)
            await coordinator.bindSourceFacts()
            configureOneSpikeTrain(in: coordinator, mode: .putativeSingleUnit)
            await coordinator.validateScientificReview()

            #expect(document.dataset == activeDataset)
            #expect(document.classicAnchorReviewStatuses == ["preserved": .accepted])
            #expect(document.statusMessage == "Preserve this status")
            #expect(coordinator.preparedImport == nil)
        }
    }

    @Test("Successful validated preparation still never mutates the active document")
    func successfulPreparationPreservesActiveDocument() async throws {
        try await withTemporaryDirectory { directory in
            let activeURL = directory.appendingPathComponent("active.csv")
            try Data(
                """
                fast_train,slower_train
                0.000,0.000
                0.100,0.300
                0.106,0.320
                0.112,0.340
                0.200,0.360
                ,0.860
                ,1.360
                ,1.860

                """.utf8
            ).write(to: activeURL)
            let candidateURL = directory.appendingPathComponent("candidate.csv")
            try Data("candidate_unit\n3.000000\n3.250000\n".utf8).write(to: candidateURL)

            let document = RasterDocument()
            document.loadCSV(
                from: activeURL,
                unit: .seconds,
                hasHeader: true,
                duplicatePolicy: .errorKeep
            )
            let activeDataset = try #require(document.dataset)
            let activeStanding = document.activeDatasetScientificStanding
            document.runAdaptiveClassicAnchorDetection()
            try await waitForDetector(in: document)
            let activeRun = try #require(document.classicAnchorDetectionRun)
            let activeRunID = activeRun.runIdentity.runID
            let activeCandidateIDs = activeRun.candidates.map(\.id)
            let activeEventAnnotationIDs = document.classicAnchorEventAnnotations.map(\.candidateID)
            let activeStateAnnotationIDs = document.classicAnchorStateAnnotations.map(\.candidateID)
            let activeDetectorDate = document.detectorLastRunDate
            let activeDetectorStatus = document.detectorStatusMessage
            document.classicAnchorReviewStatuses = ["preserved": .needsReview]
            document.statusMessage = "Active document remains installed"
            document.loadedResultPackageURL = directory.appendingPathComponent("preserved.stpdresult")
            document.resultPackageReadbackErrorMessage = "Preserved readback state"
            document.resultPackageLoadCompletionID = 42
            document.isResultPackageExporting = true
            let directoryEntriesBefore = try FileManager.default.contentsOfDirectory(
                atPath: directory.path
            ).sorted()

            let coordinator = document.scientificImportCoordinator
            await coordinator.beginImport(from: candidateURL)
            coordinator.selectHeaderDecision(.firstRecordIsHeader)
            await coordinator.bindSourceFacts()
            configureOneSpikeTrain(in: coordinator, mode: .putativeSingleUnit)
            await coordinator.validateScientificReview()

            #expect(coordinator.phase == .validatedPreparation)
            _ = try #require(coordinator.validatedPreparation)
            _ = try #require(coordinator.shadowCanonicalImport)
            #expect(document.dataset == activeDataset)
            #expect(document.activeDatasetScientificStanding == activeStanding)
            #expect(document.classicAnchorDetectionRun?.runIdentity.runID == activeRunID)
            #expect(document.classicAnchorDetectionRun?.candidates.map(\.id) == activeCandidateIDs)
            #expect(document.classicAnchorEventAnnotations.map(\.candidateID)
                == activeEventAnnotationIDs)
            #expect(document.classicAnchorStateAnnotations.map(\.candidateID)
                == activeStateAnnotationIDs)
            #expect(document.detectorLastRunDate == activeDetectorDate)
            #expect(document.detectorStatusMessage == activeDetectorStatus)
            #expect(document.classicAnchorReviewStatuses == ["preserved": .needsReview])
            #expect(document.statusMessage == "Active document remains installed")
            #expect(document.loadedResultPackageURL
                == directory.appendingPathComponent("preserved.stpdresult"))
            #expect(document.resultPackageReadbackErrorMessage == "Preserved readback state")
            #expect(document.resultPackageLoadCompletionID == 42)
            #expect(document.isResultPackageExporting)
            #expect(try FileManager.default.contentsOfDirectory(atPath: directory.path).sorted()
                == directoryEntriesBefore)
        }
    }

    private func waitForDetector(in document: RasterDocument) async throws {
        let clock = ContinuousClock()
        let deadline = clock.now.advanced(by: .seconds(10))
        while document.isDetectorRunning, clock.now < deadline {
            try await Task.sleep(for: .milliseconds(10))
        }
        #expect(!document.isDetectorRunning)
    }

    private func configureOneSpikeTrain(
        in coordinator: ScientificImportCoordinator,
        mode: ScientificDatasetActivityMode,
        duplicateDecision: ExactDuplicateDecision = .preserveMultiplicity
    ) {
        guard var form = coordinator.manifestForm else {
            Issue.record("Expected a manifest form")
            return
        }
        form.sourceTimeUnit = .seconds
        form.activityMode = mode
        form.columns[0].groupSemanticIDText = "group_1"
        form.columns[0].groupTimeBasis = .recordingElapsed
        form.columns[0].role = .spikeTrain
        form.columns[0].semanticIDText = "unit_A"
        form.columns[0].orderDecision = .preserveSourceOrder
        form.columns[0].duplicateDecision = duplicateDecision
        coordinator.manifestForm = form
    }

    private func withTemporaryDirectory<T>(
        _ body: (URL) async throws -> T
    ) async throws -> T {
        let directory = FileManager.default.temporaryDirectory
            .appendingPathComponent("ScientificImportCoordinatorTests")
            .appendingPathComponent(UUID().uuidString)
        try FileManager.default.createDirectory(
            at: directory,
            withIntermediateDirectories: true
        )
        defer { try? FileManager.default.removeItem(at: directory) }
        return try await body(directory)
    }

    private func makeMinimalXLSX() throws -> Data {
        let spreadsheetNamespace =
            "http://schemas.openxmlformats.org/spreadsheetml/2006/main"
        let documentRelationshipsNamespace =
            "http://schemas.openxmlformats.org/officeDocument/2006/relationships"
        let packageRelationshipsNamespace =
            "http://schemas.openxmlformats.org/package/2006/relationships"
        let contentTypesNamespace =
            "http://schemas.openxmlformats.org/package/2006/content-types"
        let entries: [(String, Data)] = [
            (
                "[Content_Types].xml",
                Data("""
                <Types xmlns="\(contentTypesNamespace)">
                  <Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>
                  <Override PartName="/xl/workbook.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"/>
                  <Override PartName="/xl/worksheets/sheet1.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/>
                </Types>
                """.utf8)
            ),
            (
                "_rels/.rels",
                Data("""
                <Relationships xmlns="\(packageRelationshipsNamespace)">
                  <Relationship Id="root" Type="\(documentRelationshipsNamespace)/officeDocument" Target="xl/workbook.xml"/>
                </Relationships>
                """.utf8)
            ),
            (
                "xl/workbook.xml",
                Data("""
                <workbook xmlns="\(spreadsheetNamespace)" xmlns:r="\(documentRelationshipsNamespace)">
                  <sheets><sheet name="Data" sheetId="1" r:id="sheet"/></sheets>
                </workbook>
                """.utf8)
            ),
            (
                "xl/_rels/workbook.xml.rels",
                Data("""
                <Relationships xmlns="\(packageRelationshipsNamespace)">
                  <Relationship Id="sheet" Type="\(documentRelationshipsNamespace)/worksheet" Target="worksheets/sheet1.xml"/>
                </Relationships>
                """.utf8)
            ),
            (
                "xl/worksheets/sheet1.xml",
                Data("""
                <worksheet xmlns="\(spreadsheetNamespace)">
                  <sheetData>
                    <row r="1"><c r="A1" t="inlineStr"><is><t>unit_A</t></is></c></row>
                    <row r="2"><c r="A2"><v>1</v></c></row>
                  </sheetData>
                </worksheet>
                """.utf8)
            ),
        ]
        let archive = try Archive(accessMode: .create)
        for (path, data) in entries {
            try archive.addEntry(
                with: path,
                type: .file,
                uncompressedSize: Int64(data.count),
                compressionMethod: .none,
                bufferSize: 64 * 1_024
            ) { position, size in
                let lower = Int(position)
                let upper = min(lower + size, data.count)
                return data.subdata(in: lower..<upper)
            }
        }
        return try #require(archive.data)
    }
}
