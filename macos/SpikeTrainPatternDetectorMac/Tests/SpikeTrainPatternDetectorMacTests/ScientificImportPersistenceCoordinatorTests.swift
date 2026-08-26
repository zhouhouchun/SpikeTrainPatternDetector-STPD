import Foundation
@testable import SpikeTrainPatternDetectorMac
@testable import STPDCore
import STPDTabularIO
import Testing
import ZIPFoundation

@Suite("Scientific import persistence coordinator", .serialized)
@MainActor
struct ScientificImportPersistenceCoordinatorTests {
    // MARK: - Helpers

    private func withStore<T>(_ body: (URL) async throws -> T) async throws -> T {
        let root = FileManager.default.temporaryDirectory
            .appendingPathComponent("PersistCoordinatorTests", isDirectory: true)
            .appendingPathComponent(UUID().uuidString, isDirectory: true)
        try FileManager.default.createDirectory(at: root, withIntermediateDirectories: true)
        defer { try? FileManager.default.removeItem(at: root) }
        return try await body(root)
    }

    private func writeCSV(
        in root: URL,
        name: String = "source.csv",
        content: String = "unit_A\n1.000000\n1.250000\n"
    ) throws -> URL {
        let url = root.appendingPathComponent(name)
        try Data(content.utf8).write(to: url)
        return url
    }

    private func store(_ root: URL) -> ScientificImportManifestFileStore {
        ScientificImportManifestFileStore(rootDirectory: root.appendingPathComponent("store", isDirectory: true))
    }

    /// Awaits a `DispatchSemaphore` from an async context without blocking a cooperative-pool thread.
    /// The hard deadline turns a missing hook/callback into a bounded test failure instead of a hung CI run.
    private func awaitSignal(_ semaphore: DispatchSemaphore) async -> Bool {
        await withCheckedContinuation { (continuation: CheckedContinuation<Bool, Never>) in
            DispatchQueue.global().async {
                continuation.resume(
                    returning: semaphore.wait(timeout: .now() + .seconds(5)) == .success
                )
            }
        }
    }

    private func prepareValidated(
        _ coordinator: ScientificImportCoordinator,
        url: URL,
        headerDecision: CanonicalTabularHeaderDecision = .firstRecordIsHeader
    ) async {
        await coordinator.beginImport(from: url)
        coordinator.selectHeaderDecision(headerDecision)
        await coordinator.bindSourceFacts()
        configureOneSpikeTrain(coordinator)
        await coordinator.validateScientificReview()
    }

    private func configureOneSpikeTrain(
        _ coordinator: ScientificImportCoordinator,
        segmentID: String = "segment_1",
        duplicate: ExactDuplicateDecision = .preserveMultiplicity
    ) {
        guard var form = coordinator.manifestForm else {
            Issue.record("Expected a manifest form")
            return
        }
        form.sourceTimeUnit = .seconds
        form.activityMode = .putativeSingleUnit
        form.recordingSegmentIDText = segmentID
        form.recordingRegime = .continuousUntrialed
        form.importedExcerptCoverage = .allSpikeTrainsFullImportedExcerpt
        form.observationBoundsConfirmedUnavailable = true
        form.columns[0].groupSemanticIDText = "group_1"
        form.columns[0].groupTimeBasis = .recordingElapsed
        form.columns[0].role = .spikeTrain
        form.columns[0].semanticIDText = "unit_A"
        form.columns[0].orderDecision = .preserveSourceOrder
        form.columns[0].duplicateDecision = duplicate
        coordinator.manifestForm = form
    }

    /// A minimal single-sheet XLSX (sheet "Data", header `unit_A`, values 1 and 2).
    private func makeMinimalXLSX() throws -> Data {
        let archive = try Archive(accessMode: .create)
        func add(_ path: String, _ contents: String) throws {
            let data = Data(contents.utf8)
            try archive.addEntry(with: path, type: .file, uncompressedSize: Int64(data.count)) { position, size in
                data.subdata(in: Int(position)..<Int(position) + size)
            }
        }
        try add("[Content_Types].xml", """
        <?xml version="1.0" encoding="UTF-8" standalone="yes"?>
        <Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">
        <Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>
        <Default Extension="xml" ContentType="application/xml"/>
        <Override PartName="/xl/workbook.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"/>
        <Override PartName="/xl/worksheets/sheet1.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/>
        </Types>
        """)
        try add("_rels/.rels", """
        <?xml version="1.0" encoding="UTF-8" standalone="yes"?>
        <Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">
        <Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="xl/workbook.xml"/>
        </Relationships>
        """)
        try add("xl/workbook.xml", """
        <?xml version="1.0" encoding="UTF-8" standalone="yes"?>
        <workbook xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">
        <sheets><sheet name="Data" sheetId="1" r:id="rId1"/></sheets>
        </workbook>
        """)
        try add("xl/_rels/workbook.xml.rels", """
        <?xml version="1.0" encoding="UTF-8" standalone="yes"?>
        <Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">
        <Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet" Target="worksheets/sheet1.xml"/>
        </Relationships>
        """)
        try add("xl/worksheets/sheet1.xml", """
        <?xml version="1.0" encoding="UTF-8" standalone="yes"?>
        <worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main">
        <sheetData>
        <row r="1"><c r="A1" t="inlineStr"><is><t>unit_A</t></is></c></row>
        <row r="2"><c r="A2"><v>1</v></c></row>
        <row r="3"><c r="A3"><v>2</v></c></row>
        </sheetData>
        </worksheet>
        """)
        return try #require(archive.data)
    }

    // MARK: - Tests

    @Test("Confirm & Save persists the manifest and removes only the persistence readiness blocker")
    func confirmAndSaveRemovesOnlyPersistenceBlocker() async throws {
        try await withStore { root in
            let coordinator = ScientificImportCoordinator(manifestStore: store(root))
            await prepareValidated(coordinator, url: try writeCSV(in: root))
            #expect(coordinator.phase == .validatedPreparation)

            await coordinator.confirmAndSaveScientificImport()

            let wrapper = try #require(coordinator.persistedConfirmation)
            #expect(wrapper.canonicalFingerprint == coordinator.confirmedImport?.canonicalFingerprint)
            let readiness = try #require(coordinator.analysisReadiness)
            #expect(readiness.isAnalysisBlocked)
            #expect(!readiness.blockers.contains(.confirmedManifestPersistenceUnavailable))
            #expect(readiness.blockers.contains(.observationBoundsUnavailable))
            #expect(readiness.blockers.contains(.runContractUnavailable))
        }
    }

    @Test("Validation alone never confirms or writes; Confirm & Save is the only save path")
    func validationAloneNeverWrites() async throws {
        try await withStore { root in
            let storeRoot = root.appendingPathComponent("store", isDirectory: true)
            let coordinator = ScientificImportCoordinator(manifestStore: ScientificImportManifestFileStore(rootDirectory: storeRoot))
            await prepareValidated(coordinator, url: try writeCSV(in: root))

            #expect(coordinator.confirmedImport == nil)
            #expect(coordinator.persistedConfirmation == nil)
            #expect(!FileManager.default.fileExists(atPath: storeRoot.appendingPathComponent("by-source").path))
        }
    }

    @Test("Saving the same confirmed manifest twice is idempotent")
    func idempotentSaveViaCoordinator() async throws {
        try await withStore { root in
            let coordinator = ScientificImportCoordinator(manifestStore: store(root))
            await prepareValidated(coordinator, url: try writeCSV(in: root))
            await coordinator.confirmAndSaveScientificImport()
            let firstDigest = try #require(coordinator.persistedConfirmation?.confirmationRecordDigest)

            await coordinator.confirmAndSaveScientificImport()
            #expect(coordinator.persistedConfirmation?.confirmationRecordDigest == firstDigest)
            #expect(coordinator.persistenceMessage?.contains("already saved") == true)
        }
    }

    @Test("A restarted coordinator discovers the saved record and restores only after explicit selection")
    func simulatedRestartDiscoversAndRestores() async throws {
        try await withStore { root in
            let storeRoot = root.appendingPathComponent("store", isDirectory: true)
            let url = try writeCSV(in: root)

            let session1 = ScientificImportCoordinator(manifestStore: ScientificImportManifestFileStore(rootDirectory: storeRoot))
            await prepareValidated(session1, url: url)
            await session1.confirmAndSaveScientificImport()
            let savedDigest = try #require(session1.persistedConfirmation?.confirmationRecordDigest)

            // Restart: a fresh coordinator + fresh store instance over the same root.
            let session2 = ScientificImportCoordinator(manifestStore: ScientificImportManifestFileStore(rootDirectory: storeRoot))
            await session2.beginImport(from: url)
            // Discovery only — never auto-restores.
            #expect(session2.persistedConfirmation == nil)
            #expect(session2.selectedSavedRecordDigest == nil)
            #expect(session2.savedManifestSummaries.contains { $0.recordDigest == savedDigest })

            session2.selectSavedManifest(recordDigest: savedDigest)
            await session2.restoreAndVerify()

            let restored = try #require(session2.persistedConfirmation)
            #expect(restored.confirmationRecordDigest == savedDigest)
            let readiness = try #require(session2.analysisReadiness)
            #expect(!readiness.blockers.contains(.confirmedManifestPersistenceUnavailable))
            #expect(readiness.blockers.contains(.observationBoundsUnavailable))
        }
    }

    @Test("A different source discovers none of another source's saved records")
    func differentSourceDiscoversNothing() async throws {
        try await withStore { root in
            let storeRoot = root.appendingPathComponent("store", isDirectory: true)
            let session1 = ScientificImportCoordinator(manifestStore: ScientificImportManifestFileStore(rootDirectory: storeRoot))
            await prepareValidated(session1, url: try writeCSV(in: root, name: "a.csv"))
            await session1.confirmAndSaveScientificImport()
            #expect(session1.persistedConfirmation != nil)

            // A different file (different bytes → different SHA).
            let urlB = try writeCSV(in: root, name: "b.csv", content: "unit_A\n2.000000\n3.000000\n")
            let session2 = ScientificImportCoordinator(manifestStore: ScientificImportManifestFileStore(rootDirectory: storeRoot))
            await session2.beginImport(from: urlB)
            #expect(session2.savedManifestSummaries.isEmpty)
        }
    }

    @Test("Editing the form invalidates the in-memory persisted wrapper but keeps stored history")
    func formEditInvalidatesWrapperKeepsHistory() async throws {
        try await withStore { root in
            let fileStore = store(root)
            let coordinator = ScientificImportCoordinator(manifestStore: fileStore)
            await prepareValidated(coordinator, url: try writeCSV(in: root))
            await coordinator.confirmAndSaveScientificImport()
            let sha = try #require(coordinator.confirmedImport?.sourceTransactionBinding.sourceBytesSHA256)
            #expect(coordinator.persistedConfirmation != nil)

            // Any recording-segment field edit reassigns the form, tripping invalidation.
            coordinator.manifestForm?.recordingRegime = .trialized
            #expect(coordinator.persistedConfirmation == nil)
            #expect(coordinator.confirmedImport == nil)

            // Stored receipt history on disk is untouched.
            let discovery = try await fileStore.discover(sourceSHA256: sha)
            #expect(discovery.summaries.count == 1)
        }
    }

    @Test("A successful save leaves an independent RasterDocument's dataset and detector state unchanged")
    func rasterDocumentUnchangedBySave() async throws {
        try await withStore { root in
            let document = RasterDocument()
            let datasetBefore = document.dataset
            let runningBefore = document.isDetectorRunning

            let coordinator = ScientificImportCoordinator(manifestStore: store(root))
            await prepareValidated(coordinator, url: try writeCSV(in: root))
            await coordinator.confirmAndSaveScientificImport()
            #expect(coordinator.persistedConfirmation != nil)

            #expect(document.dataset == datasetBefore)
            #expect(document.isDetectorRunning == runningBefore)
            // The document's own coordinator was never touched by the standalone save.
            #expect(document.scientificImportCoordinator.persistedConfirmation == nil)
        }
    }

    @Test("A saved compatible import opens canonical manual ISI analysis without running a detector")
    func savedImportOpensCanonicalManualAnalysis() async throws {
        try await withStore { root in
            let coordinator = ScientificImportCoordinator(manifestStore: store(root))
            let document = RasterDocument(scientificImportCoordinator: coordinator)
            await prepareValidated(coordinator, url: try writeCSV(in: root))
            await coordinator.confirmAndSaveScientificImport()

            #expect(coordinator.persistedConfirmation != nil)
            #expect(document.prepareCanonicalManualWorkbench())
            #expect(document.canonicalManualDataset?.spikeTrains.count == 1)
            #expect(document.canonicalManualDataset?.spikeTrains.first?.rawTimestamps.count == 2)
            #expect(document.dataset?.trains.count == 1)
            #expect(document.dataset?.trains.first?.name == "unit_A")
            #expect(document.dataset?.sourceDescription.contains("规范导入") == true)
            #expect(document.selectedTrainIDs.count == 1)
            #expect(document.canonicalManualUnreviewedISICount == 1)
            #expect(document.classicAnchorDetectionRun == nil)
        }
    }

    @Test("Restore is refused while a validated preparation exists; that preparation stays intact")
    func preExistingValidatedBlocksRestore() async throws {
        try await withStore { root in
            let storeRoot = root.appendingPathComponent("store", isDirectory: true)
            let url = try writeCSV(in: root)
            let session1 = ScientificImportCoordinator(manifestStore: ScientificImportManifestFileStore(rootDirectory: storeRoot))
            await prepareValidated(session1, url: url)
            await session1.confirmAndSaveScientificImport()
            let savedDigest = try #require(session1.persistedConfirmation?.confirmationRecordDigest)

            // Session 2 reaches a validated preparation (shadow A), then attempts to restore B.
            let session2 = ScientificImportCoordinator(manifestStore: ScientificImportManifestFileStore(rootDirectory: storeRoot))
            await prepareValidated(session2, url: url)
            #expect(session2.phase == .validatedPreparation)
            session2.selectSavedManifest(recordDigest: savedDigest)
            await session2.restoreAndVerify()

            // Refused; A is untouched and no persisted wrapper coexists with the shadow.
            #expect(session2.persistedConfirmation == nil)
            #expect(session2.validatedPreparation != nil)
            #expect(session2.phase == .validatedPreparation)
        }
    }

    @Test("A failed restore rolls back cleanly, leaving the clean transaction unchanged")
    func failedRestoreRollsBack() async throws {
        try await withStore { root in
            let storeRoot = root.appendingPathComponent("store", isDirectory: true)
            let url = try writeCSV(in: root)
            let session1 = ScientificImportCoordinator(manifestStore: ScientificImportManifestFileStore(rootDirectory: storeRoot))
            await prepareValidated(session1, url: url)
            await session1.confirmAndSaveScientificImport()
            let saved = try #require(session1.persistedConfirmation)

            let session2 = ScientificImportCoordinator(manifestStore: ScientificImportManifestFileStore(rootDirectory: storeRoot))
            await session2.beginImport(from: url)
            let sha = try #require(session2.source?.sourceSHA256)
            // Corrupt the stored file so the bounded read fails during restore.
            let fileURL = storeRoot
                .appendingPathComponent("by-source", isDirectory: true)
                .appendingPathComponent(sha, isDirectory: true)
                .appendingPathComponent("\(saved.confirmationRecordDigest).stpdimportmanifest")
            try Data([0x00, 0x01, 0x02]).write(to: fileURL)

            session2.selectSavedManifest(recordDigest: saved.confirmationRecordDigest)
            await session2.restoreAndVerify()

            // Nothing was partially applied.
            #expect(session2.persistedConfirmation == nil)
            #expect(session2.confirmedImport == nil)
            #expect(session2.transportStaging == nil)
            #expect(session2.phase == .awaitingSourceDecisions)
            #expect(session2.persistenceMessage != nil)
        }
    }

    @Test("A new validation generation invalidates the persisted wrapper")
    func revalidationInvalidatesPersistedWrapper() async throws {
        try await withStore { root in
            let coordinator = ScientificImportCoordinator(manifestStore: store(root))
            await prepareValidated(coordinator, url: try writeCSV(in: root))
            await coordinator.confirmAndSaveScientificImport()
            #expect(coordinator.persistedConfirmation != nil)

            await coordinator.validateScientificReview()
            #expect(coordinator.persistedConfirmation == nil)
        }
    }

    @Test("Multiple saved records for one source require explicit selection")
    func multipleRecordsRequireExplicitSelection() async throws {
        try await withStore { root in
            let storeRoot = root.appendingPathComponent("store", isDirectory: true)
            let url = try writeCSV(in: root)
            let session1 = ScientificImportCoordinator(manifestStore: ScientificImportManifestFileStore(rootDirectory: storeRoot))
            await prepareValidated(session1, url: url)
            await session1.confirmAndSaveScientificImport()
            // A second record for the same source: a different confirmed segment ID.
            session1.manifestForm?.recordingSegmentIDText = "segment_2"
            await session1.validateScientificReview()
            await session1.confirmAndSaveScientificImport()

            let session2 = ScientificImportCoordinator(manifestStore: ScientificImportManifestFileStore(rootDirectory: storeRoot))
            await session2.beginImport(from: url)
            #expect(session2.savedManifestSummaries.count == 2)
            #expect(session2.selectedSavedRecordDigest == nil)

            // Restore without an explicit selection is refused.
            await session2.restoreAndVerify()
            #expect(session2.persistedConfirmation == nil)

            // Explicit selection restores.
            let digest = try #require(session2.savedManifestSummaries.first?.recordDigest)
            session2.selectSavedManifest(recordDigest: digest)
            await session2.restoreAndVerify()
            #expect(session2.persistedConfirmation?.confirmationRecordDigest == digest)
        }
    }

    @Test("Overlapping restore operations converge without a stale completion clobbering state")
    func staleOverlappingRestoresConverge() async throws {
        try await withStore { root in
            let storeRoot = root.appendingPathComponent("store", isDirectory: true)
            let url = try writeCSV(in: root)
            let session1 = ScientificImportCoordinator(manifestStore: ScientificImportManifestFileStore(rootDirectory: storeRoot))
            await prepareValidated(session1, url: url)
            await session1.confirmAndSaveScientificImport()
            let digest = try #require(session1.persistedConfirmation?.confirmationRecordDigest)

            let session2 = ScientificImportCoordinator(manifestStore: ScientificImportManifestFileStore(rootDirectory: storeRoot))
            await session2.beginImport(from: url)
            session2.selectSavedManifest(recordDigest: digest)

            async let first: Void = session2.restoreAndVerify()
            async let second: Void = session2.restoreAndVerify()
            _ = await (first, second)

            // A single consistent result and no in-flight operation remains.
            #expect(session2.persistedConfirmation?.confirmationRecordDigest == digest)
            #expect(!session2.isPersisting)
        }
    }

    @Test("A real XLSX manifest saves, then restores after restart")
    func xlsxSaveRestartRestore() async throws {
        try await withStore { root in
            let storeRoot = root.appendingPathComponent("store", isDirectory: true)
            let url = root.appendingPathComponent("book.xlsx")
            try makeMinimalXLSX().write(to: url)

            let session1 = ScientificImportCoordinator(manifestStore: ScientificImportManifestFileStore(rootDirectory: storeRoot))
            await session1.beginImport(from: url)
            let sheetID = try #require(session1.worksheets.first?.sheetID)
            #expect(session1.selectWorksheet(sheetID: sheetID))
            session1.selectHeaderDecision(.firstRecordIsHeader)
            await session1.bindSourceFacts()
            configureOneSpikeTrain(session1)
            await session1.validateScientificReview()
            await session1.confirmAndSaveScientificImport()
            let digest = try #require(session1.persistedConfirmation?.confirmationRecordDigest)

            let session2 = ScientificImportCoordinator(manifestStore: ScientificImportManifestFileStore(rootDirectory: storeRoot))
            await session2.beginImport(from: url)
            #expect(session2.savedManifestSummaries.contains { $0.recordDigest == digest })
            session2.selectSavedManifest(recordDigest: digest)
            await session2.restoreAndVerify()
            #expect(session2.persistedConfirmation?.confirmationRecordDigest == digest)
            // Restore installs the saved worksheet binding for a consistent restored transaction, and
            // the replayed canonical fingerprint matches the saved record.
            #expect(session2.selectedWorksheetSheetID == sheetID)
            #expect(session2.persistedConfirmation?.canonicalFingerprint == session1.persistedConfirmation?.canonicalFingerprint)
        }
    }

    @Test("An exact-duplicate collapse-request decision restores exactly")
    func collapseExactDecisionRestores() async throws {
        try await withStore { root in
            let storeRoot = root.appendingPathComponent("store", isDirectory: true)
            let url = try writeCSV(in: root, content: "unit_A\n1.000000\n1.000000\n1.250000\n")

            let session1 = ScientificImportCoordinator(manifestStore: ScientificImportManifestFileStore(rootDirectory: storeRoot))
            await session1.beginImport(from: url)
            session1.selectHeaderDecision(.firstRecordIsHeader)
            await session1.bindSourceFacts()
            configureOneSpikeTrain(session1, duplicate: .collapseExact)
            await session1.validateScientificReview()
            await session1.confirmAndSaveScientificImport()
            let digest = try #require(session1.persistedConfirmation?.confirmationRecordDigest)

            let session2 = ScientificImportCoordinator(manifestStore: ScientificImportManifestFileStore(rootDirectory: storeRoot))
            await session2.beginImport(from: url)
            session2.selectSavedManifest(recordDigest: digest)
            await session2.restoreAndVerify()
            #expect(session2.persistedConfirmation?.confirmationRecordDigest == digest)
        }
    }

    @Test("Restore installs the receipt's header decision, giving one consistent restored transaction")
    func restoreInstallsHeaderDecision() async throws {
        try await withStore { root in
            let storeRoot = root.appendingPathComponent("store", isDirectory: true)
            let url = try writeCSV(in: root)
            let session1 = ScientificImportCoordinator(manifestStore: ScientificImportManifestFileStore(rootDirectory: storeRoot))
            await prepareValidated(session1, url: url) // saved with .firstRecordIsHeader
            await session1.confirmAndSaveScientificImport()
            let digest = try #require(session1.persistedConfirmation?.confirmationRecordDigest)

            let session2 = ScientificImportCoordinator(manifestStore: ScientificImportManifestFileStore(rootDirectory: storeRoot))
            await session2.beginImport(from: url)
            // No header decision set before restore.
            #expect(session2.headerDecision == nil)
            session2.selectSavedManifest(recordDigest: digest)
            await session2.restoreAndVerify()

            #expect(session2.persistedConfirmation != nil)
            // The receipt's header rule is installed together with the restored standing.
            #expect(session2.headerDecision == .firstRecordIsHeader)
        }
    }

    @Test("A selection change cannot redirect an in-flight restore")
    func selectionChangeCannotRedirectRestore() async throws {
        try await withStore { root in
            let storeRoot = root.appendingPathComponent("store", isDirectory: true)
            let url = try writeCSV(in: root)
            // Save two records for one source.
            let session1 = ScientificImportCoordinator(manifestStore: ScientificImportManifestFileStore(rootDirectory: storeRoot))
            await prepareValidated(session1, url: url)
            await session1.confirmAndSaveScientificImport()
            let digestA = try #require(session1.persistedConfirmation?.confirmationRecordDigest)
            session1.manifestForm?.recordingSegmentIDText = "segment_2"
            await session1.validateScientificReview()
            await session1.confirmAndSaveScientificImport()
            let digestB = try #require(session1.persistedConfirmation?.confirmationRecordDigest)
            #expect(digestA != digestB)

            let session2 = ScientificImportCoordinator(manifestStore: ScientificImportManifestFileStore(rootDirectory: storeRoot))
            await session2.beginImport(from: url)
            session2.selectSavedManifest(recordDigest: digestA)

            // Start the restore, let it capture digestA and mark itself active, then try to switch to B.
            let restore = Task { await session2.restoreAndVerify() }
            await Task.yield()
            session2.selectSavedManifest(recordDigest: digestB) // guarded no-op while persisting
            await restore.value

            #expect(session2.selectedSavedRecordDigest == digestA)
            #expect(session2.persistedConfirmation?.confirmationRecordDigest == digestA)
        }
    }

    @Test("Restore is enabled only in the clean state with a selection, and is disabled once installed")
    func restoreDisabledAfterInstall() async throws {
        try await withStore { root in
            let storeRoot = root.appendingPathComponent("store", isDirectory: true)
            let url = try writeCSV(in: root)
            let session1 = ScientificImportCoordinator(manifestStore: ScientificImportManifestFileStore(rootDirectory: storeRoot))
            await prepareValidated(session1, url: url)
            await session1.confirmAndSaveScientificImport()
            let digest = try #require(session1.persistedConfirmation?.confirmationRecordDigest)

            let session2 = ScientificImportCoordinator(manifestStore: ScientificImportManifestFileStore(rootDirectory: storeRoot))
            await session2.beginImport(from: url)
            // Clean Stage-A state but no explicit selection → cannot restore.
            #expect(!session2.canRestoreSelectedManifest)
            session2.selectSavedManifest(recordDigest: digest)
            #expect(session2.canRestoreSelectedManifest)

            await session2.restoreAndVerify()
            #expect(session2.persistedConfirmation != nil)
            // Once a wrapper is installed, Restore is replaced by the verified standing and disabled.
            #expect(!session2.canRestoreSelectedManifest)
            // A second restore attempt is a refused no-op that does not disturb the installed standing.
            await session2.restoreAndVerify()
            #expect(session2.persistedConfirmation?.confirmationRecordDigest == digest)
        }
    }

    @Test("A discovery budget overflow is surfaced distinctly, not reported as zero records")
    func discoveryBudgetSurfacedWithZeroRecords() async throws {
        try await withStore { root in
            let storeRoot = root.appendingPathComponent("store", isDirectory: true)
            let url = try writeCSV(in: root)
            let coordinator = ScientificImportCoordinator(manifestStore: ScientificImportManifestFileStore(rootDirectory: storeRoot))
            await coordinator.beginImport(from: url)
            let sha = try #require(coordinator.source?.sourceSHA256)
            // Overflow the per-source entry budget for this exact source with tiny files.
            let dir = storeRoot
                .appendingPathComponent("by-source", isDirectory: true)
                .appendingPathComponent(sha, isDirectory: true)
            try FileManager.default.createDirectory(at: dir, withIntermediateDirectories: true)
            for index in 0...ScientificImportManifestFileStore.maximumDiscoveryEntries {
                try Data([0x00]).write(to: dir.appendingPathComponent("f\(index).stpdimportmanifest"))
            }

            await coordinator.refreshSavedManifests()
            #expect(coordinator.savedManifestSummaries.isEmpty)
            #expect(coordinator.persistenceMessage?.contains("too large to scan safely") == true)
        }
    }

    @Test("A real headerless CSV survives a fresh-coordinator restart with receipt-driven re-staging")
    func headerlessCSVSurvivesRestartWithReceiptDrivenStaging() async throws {
        try await withStore { root in
            let storeRoot = root.appendingPathComponent("store", isDirectory: true)
            // A REAL headerless CSV: one column, data records only, NO header row.
            let url = try writeCSV(in: root, name: "headerless.csv", content: "1.000000\n1.250000\n")

            // === Session 1: import headerless, validate, Confirm & Save. ===
            let session1 = ScientificImportCoordinator(manifestStore: ScientificImportManifestFileStore(rootDirectory: storeRoot))
            await prepareValidated(session1, url: url, headerDecision: .headerless)
            #expect(session1.phase == .validatedPreparation)
            await session1.confirmAndSaveScientificImport()

            let savedWrapper = try #require(session1.persistedConfirmation)
            let savedBase = try #require(session1.confirmedImport)
            #expect(savedWrapper.receipt.headerRule == .headerless)
            #expect(try ConfirmedScientificImportManifestPersistence.deriveHeaderRule(from: savedBase) == .headerless)
            let savedDigest = savedWrapper.confirmationRecordDigest
            let savedFingerprint = savedWrapper.canonicalFingerprint

            // === Session 2: NEW coordinator and NEW store instance over the same store root. ===
            let session2 = ScientificImportCoordinator(manifestStore: ScientificImportManifestFileStore(rootDirectory: storeRoot))
            await session2.beginImport(from: url)
            // No header decision is chosen before restore; the restore installs it from the receipt via
            // the production mapping receipt.headerRule → CanonicalTabularHeaderDecision.headerless →
            // real headerless re-staging → full replay → base-derived verification.
            #expect(session2.headerDecision == nil)
            #expect(session2.persistedConfirmation == nil)
            #expect(session2.savedManifestSummaries.contains { $0.recordDigest == savedDigest })
            session2.selectSavedManifest(recordDigest: savedDigest)
            await session2.restoreAndVerify()

            // === After restore. ===
            #expect(session2.headerDecision == .headerless)
            let restoredWrapper = try #require(session2.persistedConfirmation)
            let restoredBase = try #require(session2.confirmedImport)
            #expect(restoredWrapper.receipt.headerRule == .headerless)
            #expect(try ConfirmedScientificImportManifestPersistence.deriveHeaderRule(from: restoredBase) == .headerless)
            #expect(restoredWrapper.confirmationRecordDigest == savedDigest)
            #expect(restoredWrapper.canonicalFingerprint == savedFingerprint)

            // The two data records survived as exact integer microsecond ticks (1.000000 s, 1.250000 s).
            // With .headerless, the first record is DATA (not consumed as a header), so both rows appear.
            let ticks = restoredBase.validatedImport.preparedImport.data.eventScopeGroups
                .flatMap { $0.spikeTrains.flatMap { $0.timestamps.map(\.microseconds) } }
            #expect(ticks == [1_000_000, 1_250_000])

            // Persistence readiness removes EXACTLY one blocker: assess the same restored base as a bare
            // confirmation and prove the persisted set is the bare set minus only the persistence blocker.
            let readiness = try #require(session2.analysisReadiness)
            let bareReadiness = ScientificAnalysisReadinessEvaluator.assess(restoredBase)
            #expect(readiness.isAnalysisBlocked)
            #expect(Set(readiness.blockers)
                == Set(bareReadiness.blockers).subtracting([.confirmedManifestPersistenceUnavailable]))
            // The unrelated blockers remain explicitly.
            #expect(readiness.blockers.contains(.observationBoundsUnavailable))
            #expect(readiness.blockers.contains(.runContractUnavailable))
            #expect(readiness.blockers.contains(.detectorConsumerClosureUnavailable))
            #expect(readiness.blockers.contains(.authoritativeExportClosureUnavailable))
        }
    }

    @Test("Coordinator cancel() signals the in-flight persistence task, not just the UI")
    func coordinatorCancelSignalsInFlightPersistence() async throws {
        try await withStore { root in
            let storeRoot = root.appendingPathComponent("store", isDirectory: true)
            let url = try writeCSV(in: root)
            let reachedLock = DispatchSemaphore(value: 0)
            let faultedStore = ScientificImportManifestFileStore(
                rootDirectory: storeRoot,
                faults: StoreFaultInjection(beforeLockAcquire: { reachedLock.signal() })
            )
            let coordinator = ScientificImportCoordinator(manifestStore: faultedStore)
            await coordinator.beginImport(from: url)
            let sha = try #require(coordinator.source?.sourceSHA256)

            // Hold the store-global anchor lock externally so the save blocks cooperatively at the lock.
            // The store locks its fixed trusted anchor (the directory that contains the store root),
            // which for this store root (root/store) is `root` itself.
            let anchorDir = storeRoot.deletingLastPathComponent()
            let heldFD = anchorDir.path.withCString { open($0, O_DIRECTORY | O_RDONLY | O_NOFOLLOW) }
            #expect(heldFD >= 0)
            #expect(flock(heldFD, LOCK_EX) == 0)
            defer { flock(heldFD, LOCK_UN); close(heldFD) }

            coordinator.selectHeaderDecision(.firstRecordIsHeader)
            await coordinator.bindSourceFacts()
            configureOneSpikeTrain(coordinator)
            await coordinator.validateScientificReview()
            #expect(coordinator.phase == .validatedPreparation)

            let saveTask = Task { await coordinator.confirmAndSaveScientificImport() }
            #expect(await awaitSignal(reachedLock), "save never reached the contended store lock")
            // Cancelling the coordinator signals the in-flight persistence task, which observes it at the
            // cooperative lock wait — not merely a UI reset.
            coordinator.cancel()
            await saveTask.value

            #expect(coordinator.persistedConfirmation == nil)
            #expect(!coordinator.isPersisting)
            // The save was cancelled before publication: no record exists for this source.
            let clean = ScientificImportManifestFileStore(rootDirectory: storeRoot)
            #expect(try await clean.discover(sourceSHA256: sha).summaries.isEmpty)
        }
    }

    @Test("A newer save supersedes and cancels the older; cancel() signals the newer; neither mints")
    func newerSaveSupersedesOlderAndCancelSignalsBoth() async throws {
        try await withStore { root in
            let storeRoot = root.appendingPathComponent("store", isDirectory: true)
            let url = try writeCSV(in: root)
            let atLock = DispatchSemaphore(value: 0)
            let store = ScientificImportManifestFileStore(
                rootDirectory: storeRoot,
                faults: StoreFaultInjection(
                    // Fires once as each save reaches the store lock — the cooperative pre-publication
                    // point. Holding both saves here (via an external anchor lock) keeps the store actor
                    // FREE, so the second save's confirmAndSave can run and supersede the first.
                    beforeLockAcquire: { atLock.signal() },
                    // A long deadline so a BROKEN canceller cannot pass merely by timing out after 5s:
                    // a still-running superseded/uncancelled save would instead outlast the hold, acquire
                    // the lock once released, and publish a record — which the assertions below catch.
                    lockTimeoutOverrideNanoseconds: 120_000_000_000
                )
            )
            let coordinator = ScientificImportCoordinator(manifestStore: store)
            await prepareValidated(coordinator, url: url, headerDecision: .firstRecordIsHeader)
            #expect(coordinator.phase == .validatedPreparation)
            let sha = try #require(coordinator.source?.sourceSHA256)

            // Hold the store-global anchor lock externally so BOTH saves park cooperatively at the lock
            // (the store actor stays free) and neither can publish while the lock is held.
            let anchorDir = storeRoot.deletingLastPathComponent()
            let heldFD = anchorDir.path.withCString { open($0, O_DIRECTORY | O_RDONLY | O_NOFOLLOW) }
            #expect(heldFD >= 0)
            #expect(flock(heldFD, LOCK_EX) == 0)
            var released = false
            func releaseLock() { if !released { released = true; flock(heldFD, LOCK_UN); close(heldFD) } }
            defer { releaseLock() }

            // Save A parks at the store lock.
            let taskADone = DispatchSemaphore(value: 0)
            let taskA = Task {
                await coordinator.confirmAndSaveScientificImport()
                taskADone.signal()
            }
            guard await awaitSignal(atLock) else {
                coordinator.cancel()
                releaseLock()
                await taskA.value
                Issue.record("save A never reached the contended store lock")
                return
            }

            // Save B: installing B's canceller SIGNALS (cancels) A before replacing its handle, so A
            // unparks with operationCancelled; B then parks at the store lock. Awaiting B reach the lock
            // proves B's canceller install (hence A's supersede) already ran.
            let taskB = Task { await coordinator.confirmAndSaveScientificImport() }
            guard await awaitSignal(atLock) else {
                coordinator.cancel()
                releaseLock()
                await taskA.value
                await taskB.value
                Issue.record("save B never reached the contended store lock")
                return
            }

            // Wait for A's cancelled completion BEFORE cancelling B. This makes the ID-scoped clear
            // assertion deterministic: if A could erase B's canceller, the following cancel would fail
            // to signal B and B would publish after the external lock is released.
            guard await awaitSignal(taskADone) else {
                coordinator.cancel()
                releaseLock()
                await taskA.value
                await taskB.value
                Issue.record("superseded save A did not finish promptly after cancellation")
                return
            }

            // cancel() must also signal the newer in-flight operation (B), which unparks with cancellation.
            coordinator.cancel()
            // Release the lock: with both supersede and cancel working, neither task ever acquires it. A
            // broken canceller would leave a task parked that now acquires the lock and publishes a record.
            releaseLock()
            await taskB.value

            // Neither operation installs persistence standing, and no final record was published.
            #expect(coordinator.persistedConfirmation == nil)
            #expect(!coordinator.isPersisting)
            let clean = ScientificImportManifestFileStore(rootDirectory: storeRoot)
            #expect(try await clean.discover(sourceSHA256: sha).summaries.isEmpty)
        }
    }
}
