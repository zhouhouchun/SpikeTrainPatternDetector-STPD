import Foundation
import Testing
@testable import STPDCore

// RESULT-PACKAGE-READBACK: strict on-disk `.stpdresult` reader/verifier.
//
// These tests write real packages with STPDResultPackageWriter, read them back with
// STPDResultPackageReader, and corrupt on-disk bytes/manifest to prove each verification layer fails
// closed with a typed error. Temporary directories are unique per test and removed in a guarded defer.

// MARK: - Fixtures

private let readerBuildCommit = "reader_test_build"

private func readerFixture(
    taskEvents: [TaskEvent] = []
) -> (dataset: SpikeDataset, run: ClassicAnchorDetectionRun) {
    let burstISIs =
        Array(repeating: 0.100, count: 3) +
        Array(repeating: 0.006, count: 7) +
        Array(repeating: 0.100, count: 3)
    let tonicISIs = [
        0.300, 0.310, 0.295, 0.305, 0.300,
        0.900,
        0.305, 0.300, 0.295, 0.310, 0.300,
    ]
    func timestamps(_ isis: [Double]) -> [Double] {
        isis.reduce(into: [0.0]) { values, isi in values.append((values.last ?? 0) + isi) }
    }
    let dataset = SpikeDataset(
        name: "reader-fixture",
        sourceDescription: "reader roundtrip",
        trains: [
            SpikeTrain(name: "burst_train", timestampsSec: timestamps(burstISIs)),
            SpikeTrain(name: "tonic_pause_train", timestampsSec: timestamps(tonicISIs)),
        ],
        taskEvents: taskEvents
    )
    let run = ClassicAnchorDetectionPipeline.run(
        dataset: dataset,
        bandSettings: TrainAdaptiveBandSettings(minValidISISec: 0.001, histogramBinWidthSec: 0.005),
        buildCommit: readerBuildCommit
    )
    return (dataset, run)
}

private func readerLocalPackageWithTaskEvents() throws -> STPDResultPackage {
    let event = TaskEvent(
        id: "evt_reader_1", name: "cue", timeSec: 0.5, column: "cue_column",
        eventIndex: 1, trialID: "trial_reader_1", source: "reader-fixture"
    )
    let fixture = readerFixture(taskEvents: [event])
    return try STPDResultPackageBuilder.build(
        STPDResultPackageInput.automatic(dataset: fixture.dataset, run: fixture.run)
    )
}

private func readerLocalPackage() throws -> STPDResultPackage {
    let fixture = readerFixture()
    let input = STPDResultPackageInput.automatic(dataset: fixture.dataset, run: fixture.run)
    return try STPDResultPackageBuilder.build(input)
}

private func readerTonicAnnotation(
    dataset: SpikeDataset,
    run: ClassicAnchorDetectionRun,
    id: UUID
) throws -> ManualAnnotation {
    let automatic = STPDResultPackageInput.automatic(dataset: dataset, run: run)
    let row = try #require(
        automatic.finalISILabelRows.first {
            $0.isiIndex > 0 && $0.autoPattern != ManualAnnotationLabel.tonic.rawValue
        }
    )
    let train = try #require(dataset.trains.first { $0.id == row.trainID })
    return ManualAnnotation(
        id: id,
        trainID: train.id,
        label: .tonic,
        startSec: train.timestampsSec[row.isiIndex - 1],
        endSec: train.timestampsSec[row.isiIndex],
        note: "reader imported manual annotation",
        annotator: "Reader Test Reviewer",
        annotatorIdentitySource: .userProvided,
        createdAt: Date(timeIntervalSince1970: 100),
        updatedAt: Date(timeIntervalSince1970: 200)
    )
}

private func readerImportedPackage() throws -> STPDResultPackage {
    let fixture = readerFixture()
    let annotation = try readerTonicAnnotation(
        dataset: fixture.dataset,
        run: fixture.run,
        id: UUID(uuidString: "beef0000-0000-0000-0000-000000000001")!
    )
    let csv = ManualAnnotationCSVExporter.csv(
        annotations: [annotation],
        identity: .forDataset(
            fixture.dataset,
            runID: fixture.run.runIdentity.runID,
            reviewState: .pendingConfirmation
        )
    )
    let imported = try ManualAnnotationCSVImporter.importIdentityBound(data: Data(csv.utf8))
    let gated = ManualAnnotationCSVImporter.gate(imported, activeDataset: fixture.dataset)
    #expect(gated.authority == .eligibleAfterExplicitConfirmation)
    let approval = try #require(
        ManualAnnotationCSVApproval(
            sourceFileDigest: gated.sourceFileDigest,
            activeDatasetDigest: gated.activeDatasetDigest,
            approvedRunID: fixture.run.runIdentity.runID,
            approvedSettingsDigest: fixture.run.runIdentity.settingsDigest,
            approver: "Dr. Import Approver",
            approvedAt: Date(timeIntervalSince1970: 2_500)
        )
    )
    let batch = try gated.authoritativeBatch(approval: approval)
    let input = try STPDResultPackageBuilder.preflightManualAnnotationImport(
        dataset: fixture.dataset,
        run: fixture.run,
        approvedBatch: batch
    )
    return try STPDResultPackageBuilder.build(input)
}

// MARK: - Write harness + on-disk corruption helpers

/// Writes `package` into a unique temp session directory, runs `body` with the package root, then removes
/// the session directory. The removal is guarded to a path that contains our unique marker.
private func withWrittenPackage<T>(
    _ package: STPDResultPackage,
    _ body: (URL) throws -> T
) throws -> T {
    let marker = "stpd-reader-tests-\(UUID().uuidString)"
    let session = FileManager.default.temporaryDirectory.appendingPathComponent(marker, isDirectory: true)
    try FileManager.default.createDirectory(at: session, withIntermediateDirectories: true)
    defer {
        if session.path.contains(marker), !session.path.isEmpty {
            try? FileManager.default.removeItem(at: session)
        }
    }
    let root = session.appendingPathComponent("package.stpdresult", isDirectory: true)
    try STPDResultPackageWriter.write(package, to: root)
    return try body(root)
}

private func manifestURL(_ root: URL) -> URL { root.appendingPathComponent("manifest.json") }
private func tableURL(_ root: URL, _ table: STPDResultTable) -> URL {
    root.appendingPathComponent(table.rawValue)
}

private func loadManifest(_ root: URL) throws -> STPDResultManifest {
    try JSONDecoder().decode(STPDResultManifest.self, from: Data(contentsOf: manifestURL(root)))
}

private func saveManifest(_ manifest: STPDResultManifest, _ root: URL) throws {
    try manifest.encodedData().write(to: manifestURL(root))
}

/// Returns a copy of `manifest` with the named table's metadata transformed.
private func replacingManifestTable(
    _ manifest: STPDResultManifest,
    fileName: String,
    _ transform: (STPDResultManifestTable) -> STPDResultManifestTable
) -> STPDResultManifest {
    let tables = manifest.tables.map { $0.fileName == fileName ? transform($0) : $0 }
    return STPDResultManifest(
        schemaVersion: manifest.schemaVersion,
        detectorVersion: manifest.detectorVersion,
        runID: manifest.runID,
        datasetDigest: manifest.datasetDigest,
        settingsDigest: manifest.settingsDigest,
        buildIdentifier: manifest.buildIdentifier,
        buildIdentifierKind: manifest.buildIdentifierKind,
        buildReproducibilityAttested: manifest.buildReproducibilityAttested,
        sourceMode: manifest.sourceMode,
        ownerName: manifest.ownerName,
        ownerEmail: manifest.ownerEmail,
        stringListEncoding: manifest.stringListEncoding,
        tables: tables
    )
}

/// Rewrites a table's CSV bytes on disk from headers+rows and resyncs the manifest's declared sha256 and
/// row count for that table, so the byte-integrity layer passes and deeper (semantic) validation runs.
private func rewriteTableAndResync(
    _ root: URL,
    _ table: STPDResultTable,
    headers: [String],
    rows: [[String]]
) throws {
    let bytes = STPDRFC4180.data(headers: headers, rows: rows)
    try bytes.write(to: tableURL(root, table))
    let manifest = try loadManifest(root)
    let updated = replacingManifestTable(manifest, fileName: table.rawValue) {
        STPDResultManifestTable(
            fileName: $0.fileName, grain: $0.grain, primaryKey: $0.primaryKey,
            columns: $0.columns, rowCount: rows.count, sha256: STPDStableIdentifier.digest(bytes)
        )
    }
    try saveManifest(updated, root)
}

/// Parses a table already on disk into (headers, rows) using the reader's strict parser.
private func loadTableRows(_ root: URL, _ table: STPDResultTable) throws -> (headers: [String], rows: [[String]]) {
    let text = try #require(String(data: Data(contentsOf: tableURL(root, table)), encoding: .utf8))
    let parsed = try STPDResultPackageReader.parseStrictCSV(text)
    let headers = try #require(parsed.first)
    return (headers, Array(parsed.dropFirst()))
}

private func columnIndex(_ headers: [String], _ name: String) throws -> Int {
    try #require(headers.firstIndex(of: name))
}

// MARK: - Positive tests

// 1. Writer -> disk -> reader round trip for a local-authority (automatic) package.
@Test func readerRoundTripsLocalAuthorityPackage() throws {
    let package = try readerLocalPackage()
    try withWrittenPackage(package) { root in
        let result = try STPDResultPackageReader.read(packageAt: root)
        #expect(result.verified)
        #expect(result.schemaVersion == STPDResultSchema.version)
        #expect(result.runID == package.manifest.runID)
        #expect(result.sourceMode == package.manifest.sourceMode)
        #expect(result.tables.count == STPDResultTable.allCases.count)
        // Each verified table's recomputed digest equals the manifest digest.
        let manifestByName = Dictionary(uniqueKeysWithValues: package.manifest.tables.map { ($0.fileName, $0) })
        for summary in result.tables {
            #expect(summary.sha256 == manifestByName[summary.fileName]?.sha256)
        }
    }
}

// 2. Writer -> disk -> reader round trip for an imported identity-bound manual package.
@Test func readerRoundTripsImportedManualPackage() throws {
    let package = try readerImportedPackage()
    try withWrittenPackage(package) { root in
        let result = try STPDResultPackageReader.read(packageAt: root)
        #expect(result.verified)
        #expect(result.sourceMode == package.manifest.sourceMode)
        // The imported-approval ledger is present and non-empty.
        let ledger = try #require(package.table(.manualAnnotationImportApprovals))
        #expect(ledger.rowCount >= 1)
    }
}

// 3. Quoted commas, escaped quotes, an embedded CRLF, and non-ASCII all round-trip through the writer's
// serializer and the strict reader parser. The writer detects the special characters by Unicode scalar,
// so a field containing an embedded "\r\n" is correctly quoted (not emitted unquoted and split into two
// records by the reader).
@Test func readerStrictCSVHandlesQuotingCRLFAndNonASCII() throws {
    let headers = ["a", "b", "c"]
    let rows = [
        ["plain", "has,comma", "has\"quote"],
        ["multi\r\nline", "naïve café — 日本語", "trailing space "],
        ["", "\"", ",,,"],
    ]
    let bytes = STPDRFC4180.data(headers: headers, rows: rows)
    let text = try #require(String(data: bytes, encoding: .utf8))
    #expect(try STPDResultPackageReader.parseStrictCSV(text) == [headers] + rows)
}

// 4. Re-reading the same package produces an equal, deterministic result.
@Test func readerIsDeterministic() throws {
    let package = try readerLocalPackage()
    try withWrittenPackage(package) { root in
        let a = try STPDResultPackageReader.read(packageAt: root)
        let b = try STPDResultPackageReader.read(packageAt: root)
        #expect(a == b)
    }
}

// 5. Reading leaves every package byte unchanged.
@Test func readerLeavesEveryByteUnchanged() throws {
    let package = try readerLocalPackage()
    try withWrittenPackage(package) { root in
        func snapshot() throws -> [String: Data] {
            var map: [String: Data] = [:]
            for entry in try FileManager.default.contentsOfDirectory(atPath: root.path) {
                map[entry] = try Data(contentsOf: root.appendingPathComponent(entry))
            }
            return map
        }
        let before = try snapshot()
        _ = try STPDResultPackageReader.read(packageAt: root)
        let after = try snapshot()
        #expect(before == after)
    }
}

// MARK: - Negative tests (each must fail closed with a typed error)

private func expectReaderError(
    _ root: URL,
    _ match: (STPDResultPackageReaderError) -> Bool,
    _ note: String
) {
    do {
        _ = try STPDResultPackageReader.read(packageAt: root)
        Issue.record("expected reader to fail closed: \(note)")
    } catch let error as STPDResultPackageReaderError {
        #expect(match(error), "unexpected reader error for \(note): \(error)")
    } catch {
        Issue.record("expected STPDResultPackageReaderError for \(note), got \(error)")
    }
}

// 6. Unknown schema version.
@Test func readerRejectsUnknownSchemaVersion() throws {
    let package = try readerLocalPackage()
    try withWrittenPackage(package) { root in
        var manifest = try loadManifest(root)
        manifest = STPDResultManifest(
            schemaVersion: "stpd_result_package_v999",
            detectorVersion: manifest.detectorVersion, runID: manifest.runID,
            datasetDigest: manifest.datasetDigest, settingsDigest: manifest.settingsDigest,
            buildIdentifier: manifest.buildIdentifier, buildIdentifierKind: manifest.buildIdentifierKind,
            buildReproducibilityAttested: manifest.buildReproducibilityAttested,
            sourceMode: manifest.sourceMode, ownerName: manifest.ownerName, ownerEmail: manifest.ownerEmail,
            stringListEncoding: manifest.stringListEncoding, tables: manifest.tables
        )
        try saveManifest(manifest, root)
        expectReaderError(root, { if case .unsupportedSchemaVersion = $0 { return true }; return false }, "unknown schema")
    }
}

// 7. Missing required table.
@Test func readerRejectsMissingRequiredTable() throws {
    let package = try readerLocalPackage()
    try withWrittenPackage(package) { root in
        try FileManager.default.removeItem(at: tableURL(root, .taskEvents))
        expectReaderError(root, {
            if case .tableSetMismatch(let missing, _) = $0 { return missing.contains(STPDResultTable.taskEvents.rawValue) }
            return false
        }, "missing table")
    }
}

// 8. Unexpected table.
@Test func readerRejectsUnexpectedTable() throws {
    let package = try readerLocalPackage()
    try withWrittenPackage(package) { root in
        try Data("x\r\n".utf8).write(to: root.appendingPathComponent("Extra.csv"))
        expectReaderError(root, {
            if case .unexpectedPackageEntry(let name) = $0 { return name == "Extra.csv" }
            if case .tableSetMismatch = $0 { return true }
            return false
        }, "unexpected table")
    }
}

// 9. Duplicate manifest table name.
@Test func readerRejectsDuplicateManifestTableName() throws {
    let package = try readerLocalPackage()
    try withWrittenPackage(package) { root in
        let manifest = try loadManifest(root)
        let first = try #require(manifest.tables.first)
        let dup = STPDResultManifest(
            schemaVersion: manifest.schemaVersion, detectorVersion: manifest.detectorVersion,
            runID: manifest.runID, datasetDigest: manifest.datasetDigest, settingsDigest: manifest.settingsDigest,
            buildIdentifier: manifest.buildIdentifier, buildIdentifierKind: manifest.buildIdentifierKind,
            buildReproducibilityAttested: manifest.buildReproducibilityAttested,
            sourceMode: manifest.sourceMode, ownerName: manifest.ownerName, ownerEmail: manifest.ownerEmail,
            stringListEncoding: manifest.stringListEncoding, tables: manifest.tables + [first]
        )
        try saveManifest(dup, root)
        expectReaderError(root, { if case .duplicateManifestTableName = $0 { return true }; return false }, "duplicate manifest table")
    }
}

// 10. Altered table byte after writing (manifest digest untouched).
@Test func readerRejectsAlteredTableByte() throws {
    let package = try readerLocalPackage()
    try withWrittenPackage(package) { root in
        var bytes = try Data(contentsOf: tableURL(root, .runMetadata))
        bytes.append(0x20) // append a space; manifest sha256 no longer matches
        try bytes.write(to: tableURL(root, .runMetadata))
        expectReaderError(root, {
            if case .digestMismatch(let t) = $0 { return t == STPDResultTable.runMetadata.rawValue }
            return false
        }, "altered table byte")
    }
}

// 11. Altered manifest digest (table untouched).
@Test func readerRejectsAlteredManifestDigest() throws {
    let package = try readerLocalPackage()
    try withWrittenPackage(package) { root in
        let manifest = try loadManifest(root)
        let updated = replacingManifestTable(manifest, fileName: STPDResultTable.runMetadata.rawValue) {
            STPDResultManifestTable(
                fileName: $0.fileName, grain: $0.grain, primaryKey: $0.primaryKey,
                columns: $0.columns, rowCount: $0.rowCount,
                sha256: String(repeating: "0", count: 64)
            )
        }
        try saveManifest(updated, root)
        expectReaderError(root, {
            if case .digestMismatch(let t) = $0 { return t == STPDResultTable.runMetadata.rawValue }
            return false
        }, "altered manifest digest")
    }
}

// 13. Declared row-count mismatch (manifest row_count wrong, table + digest intact).
@Test func readerRejectsDeclaredRowCountMismatch() throws {
    let package = try readerLocalPackage()
    try withWrittenPackage(package) { root in
        let manifest = try loadManifest(root)
        let updated = replacingManifestTable(manifest, fileName: STPDResultTable.isiLabelsFinal.rawValue) {
            STPDResultManifestTable(
                fileName: $0.fileName, grain: $0.grain, primaryKey: $0.primaryKey,
                columns: $0.columns, rowCount: $0.rowCount + 7, sha256: $0.sha256
            )
        }
        try saveManifest(updated, root)
        expectReaderError(root, {
            if case .rowCountMismatch(let t, _, _) = $0 { return t == STPDResultTable.isiLabelsFinal.rawValue }
            return false
        }, "declared row-count mismatch")
    }
}

// 14. Invalid UTF-8 (table bytes not decodable; manifest digest resynced so UTF-8 is the failing layer).
@Test func readerRejectsInvalidUTF8() throws {
    let package = try readerLocalPackage()
    try withWrittenPackage(package) { root in
        let invalid = Data([0x74, 0x72, 0x61, 0x69, 0x6E, 0xFF, 0xFE, 0x0D, 0x0A])
        try invalid.write(to: tableURL(root, .runMetadata))
        let manifest = try loadManifest(root)
        let updated = replacingManifestTable(manifest, fileName: STPDResultTable.runMetadata.rawValue) {
            STPDResultManifestTable(
                fileName: $0.fileName, grain: $0.grain, primaryKey: $0.primaryKey,
                columns: $0.columns, rowCount: $0.rowCount, sha256: STPDStableIdentifier.digest(invalid)
            )
        }
        try saveManifest(updated, root)
        expectReaderError(root, {
            if case .invalidUTF8(let t) = $0 { return t == STPDResultTable.runMetadata.rawValue }
            return false
        }, "invalid UTF-8")
    }
}

// 15. Mutated header / column order (manifest declares reordered columns; table + digest intact).
@Test func readerRejectsMutatedHeaderColumnOrder() throws {
    let package = try readerLocalPackage()
    try withWrittenPackage(package) { root in
        let manifest = try loadManifest(root)
        let updated = replacingManifestTable(manifest, fileName: STPDResultTable.runMetadata.rawValue) { t in
            var cols = t.columns
            if cols.count >= 2 { cols.swapAt(0, 1) }
            return STPDResultManifestTable(
                fileName: t.fileName, grain: t.grain, primaryKey: t.primaryKey,
                columns: cols, rowCount: t.rowCount, sha256: t.sha256
            )
        }
        try saveManifest(updated, root)
        expectReaderError(root, {
            if case .headerMismatch(let t) = $0 { return t == STPDResultTable.runMetadata.rawValue }
            return false
        }, "mutated header order")
    }
}

// 16-18. Path traversal / absolute / nested table names declared in the manifest.
@Test func readerRejectsIllegalManifestFileNames() throws {
    let illegalNames = ["../evil.csv", "/etc/passwd", "nested/Table.csv"]
    for illegal in illegalNames {
        let package = try readerLocalPackage()
        try withWrittenPackage(package) { root in
            let manifest = try loadManifest(root)
            let updated = replacingManifestTable(manifest, fileName: STPDResultTable.taskEvents.rawValue) {
                STPDResultManifestTable(
                    fileName: illegal, grain: $0.grain, primaryKey: $0.primaryKey,
                    columns: $0.columns, rowCount: $0.rowCount, sha256: $0.sha256
                )
            }
            try saveManifest(updated, root)
            expectReaderError(root, { if case .illegalEntryName = $0 { return true }; return false }, "illegal name \(illegal)")
        }
    }
}

// 19. Symlink escaping the package root (a table entry replaced by a symlink to an external file).
@Test func readerRejectsSymlinkTableEntry() throws {
    let package = try readerLocalPackage()
    try withWrittenPackage(package) { root in
        let target = FileManager.default.temporaryDirectory
            .appendingPathComponent("stpd-reader-symlink-target-\(UUID().uuidString).csv")
        try Data("run_id,settings_digest\r\n".utf8).write(to: target)
        defer { try? FileManager.default.removeItem(at: target) }
        let entry = tableURL(root, .runMetadata)
        try FileManager.default.removeItem(at: entry)
        try FileManager.default.createSymbolicLink(at: entry, withDestinationURL: target)
        expectReaderError(root, {
            if case .symlinkNotAllowed(let name) = $0 { return name == STPDResultTable.runMetadata.rawValue }
            return false
        }, "symlink table entry")
    }
}

// 20. authority_source tampering (imported -> local, digest resynced) fails complete validation.
@Test func readerRejectsAuthoritySourceTampering() throws {
    let package = try readerImportedPackage()
    try withWrittenPackage(package) { root in
        var (headers, rows) = try loadTableRows(root, .manualAnnotations)
        let idx = try columnIndex(headers, "authority_source")
        try #require(!rows.isEmpty)
        rows[0][idx] = "local" // an imported row relabeled local while retaining its import_approval_id
        try rewriteTableAndResync(root, .manualAnnotations, headers: headers, rows: rows)
        expectReaderError(root, { if case .packageValidationFailed = $0 { return true }; return false }, "authority_source tamper")
    }
}

// 21. import_approval_id tampering (cleared on an imported row) fails complete validation.
@Test func readerRejectsImportApprovalIDTampering() throws {
    let package = try readerImportedPackage()
    try withWrittenPackage(package) { root in
        var (headers, rows) = try loadTableRows(root, .manualAnnotations)
        let idx = try columnIndex(headers, "import_approval_id")
        try #require(!rows.isEmpty)
        rows[0][idx] = "" // break the reverse link from the annotation to its approval
        try rewriteTableAndResync(root, .manualAnnotations, headers: headers, rows: rows)
        expectReaderError(root, { if case .packageValidationFailed = $0 { return true }; return false }, "import_approval_id tamper")
    }
}

// 22. Approval-ledger / reverse-link tampering (mutate a ledger cell) fails complete validation.
@Test func readerRejectsApprovalLedgerTampering() throws {
    let package = try readerImportedPackage()
    try withWrittenPackage(package) { root in
        var (headers, rows) = try loadTableRows(root, .manualAnnotationImportApprovals)
        try #require(!rows.isEmpty)
        // Clear the ledger's approver assurance / approver so the ledger no longer reconstructs its receipt.
        let idx = try columnIndex(headers, "approver")
        rows[0][idx] = "Someone Else Entirely"
        try rewriteTableAndResync(root, .manualAnnotationImportApprovals, headers: headers, rows: rows)
        expectReaderError(root, { if case .packageValidationFailed = $0 { return true }; return false }, "approval-ledger tamper")
    }
}

// 23. Semantic-digest tampering on a manual row fails complete validation.
@Test func readerRejectsSemanticDigestTampering() throws {
    let package = try readerImportedPackage()
    try withWrittenPackage(package) { root in
        var (headers, rows) = try loadTableRows(root, .manualAnnotations)
        let idx = try columnIndex(headers, "note")
        try #require(!rows.isEmpty)
        // Changing a semantic-digest input column without recomputing the digest breaks the row digest.
        rows[0][idx] = "tampered note content"
        try rewriteTableAndResync(root, .manualAnnotations, headers: headers, rows: rows)
        expectReaderError(root, { if case .packageValidationFailed = $0 { return true }; return false }, "semantic-digest tamper")
    }
}

// 24. Foreign-key / bijection failure (add an orphan candidate-diagnostic reference) fails validation.
@Test func readerRejectsForeignKeyBijectionFailure() throws {
    let package = try readerImportedPackage()
    try withWrittenPackage(package) { root in
        var (headers, rows) = try loadTableRows(root, .manualAnnotations)
        let idx = try columnIndex(headers, "import_approval_id")
        try #require(!rows.isEmpty)
        rows[0][idx] = "manual_import_approval_deadbeef" // dangling reverse link with no ledger row
        try rewriteTableAndResync(root, .manualAnnotations, headers: headers, rows: rows)
        expectReaderError(root, { if case .packageValidationFailed = $0 { return true }; return false }, "foreign-key bijection")
    }
}

// 25. Repeated failures return the same typed error category and context.
@Test func readerRepeatedFailuresAreDeterministic() throws {
    let package = try readerLocalPackage()
    try withWrittenPackage(package) { root in
        var bytes = try Data(contentsOf: tableURL(root, .runMetadata))
        bytes.append(0x20)
        try bytes.write(to: tableURL(root, .runMetadata))
        func failure() -> STPDResultPackageReaderError? {
            do { _ = try STPDResultPackageReader.read(packageAt: root); return nil }
            catch let error as STPDResultPackageReaderError { return error }
            catch { return nil }
        }
        let first = failure()
        let second = failure()
        #expect(first == second)
        #expect(first == .digestMismatch(table: STPDResultTable.runMetadata.rawValue))
    }
}

// MARK: - Additional coverage (task-event round trip + direct error-branch tests)

/// Regression for the package-only task-event cross-check: a package built from a dataset that CONTAINS
/// task events must read back and verify. (Before the validator fix, package-only validation asserted
/// zero task events and rejected every such package.)
@Test func readerRoundTripsPackageWithTaskEvents() throws {
    let package = try readerLocalPackageWithTaskEvents()
    #expect((package.table(.taskEvents)?.rowCount ?? 0) >= 1) // fixture genuinely exercises the task-event path
    try withWrittenPackage(package) { root in
        let result = try STPDResultPackageReader.read(packageAt: root)
        #expect(result.verified)
    }
}

/// Writes arbitrary raw bytes for a table and resyncs only the manifest SHA-256 (not row count), so the
/// byte-integrity layer passes and a deeper layer (UTF-8 / CSV parse) is exercised.
private func writeRawAndResyncDigest(_ root: URL, _ table: STPDResultTable, bytes: Data) throws {
    try bytes.write(to: tableURL(root, table))
    let manifest = try loadManifest(root)
    let updated = replacingManifestTable(manifest, fileName: table.rawValue) {
        STPDResultManifestTable(
            fileName: $0.fileName, grain: $0.grain, primaryKey: $0.primaryKey,
            columns: $0.columns, rowCount: $0.rowCount, sha256: STPDStableIdentifier.digest(bytes)
        )
    }
    try saveManifest(updated, root)
}

// Malformed CSV (unterminated quote) with a resynced digest -> the strict parser fails closed.
@Test func readerRejectsMalformedCSV() throws {
    let package = try readerLocalPackage()
    try withWrittenPackage(package) { root in
        try writeRawAndResyncDigest(root, .runMetadata, bytes: Data("h1,h2\r\n\"unterminated\r\n".utf8))
        expectReaderError(root, {
            if case .malformedCSV(let t, _) = $0 { return t == STPDResultTable.runMetadata.rawValue }
            return false
        }, "malformed CSV")
    }
}

// A table entry replaced by a directory is not a regular file.
@Test func readerRejectsNonRegularFileTableEntry() throws {
    let package = try readerLocalPackage()
    try withWrittenPackage(package) { root in
        let entry = tableURL(root, .runMetadata)
        try FileManager.default.removeItem(at: entry)
        try FileManager.default.createDirectory(at: entry, withIntermediateDirectories: false)
        expectReaderError(root, {
            if case .entryNotARegularFile(let name) = $0 { return name == STPDResultTable.runMetadata.rawValue }
            return false
        }, "non-regular-file table entry")
    }
}

// A package root that is a regular file, not a directory.
@Test func readerRejectsNonDirectoryPackageRoot() throws {
    let marker = "stpd-reader-tests-\(UUID().uuidString)"
    let session = FileManager.default.temporaryDirectory.appendingPathComponent(marker, isDirectory: true)
    try FileManager.default.createDirectory(at: session, withIntermediateDirectories: true)
    defer { if session.path.contains(marker) { try? FileManager.default.removeItem(at: session) } }
    let notADir = session.appendingPathComponent("package.stpdresult")
    try Data("not a directory".utf8).write(to: notADir)
    expectReaderError(notADir, { if case .packageRootNotADirectory = $0 { return true }; return false }, "non-directory root")
}

// A corrupted manifest.json fails to decode.
@Test func readerRejectsUndecodableManifest() throws {
    let package = try readerLocalPackage()
    try withWrittenPackage(package) { root in
        try Data("{ this is not valid json".utf8).write(to: manifestURL(root))
        expectReaderError(root, { if case .manifestUndecodable = $0 { return true }; return false }, "undecodable manifest")
    }
}

// The package-only task-event guard: a run-metadata task_event_count that no longer matches the
// Task_events row count fails complete validation (the sole check tying the declared count to the table).
@Test func readerRejectsTaskEventCountMismatch() throws {
    let package = try readerLocalPackageWithTaskEvents()
    try withWrittenPackage(package) { root in
        var (headers, rows) = try loadTableRows(root, .runMetadata)
        let idx = try columnIndex(headers, "task_event_count")
        try #require(!rows.isEmpty)
        let current = Int(rows[0][idx]) ?? 0
        rows[0][idx] = String(current + 1) // declared count no longer matches the Task_events row count
        try rewriteTableAndResync(root, .runMetadata, headers: headers, rows: rows)
        expectReaderError(root, { if case .packageValidationFailed = $0 { return true }; return false }, "task-event count mismatch")
    }
}

// An unrecognized source_mode is a rejected manifest field.
@Test func readerRejectsInvalidSourceMode() throws {
    let package = try readerLocalPackage()
    try withWrittenPackage(package) { root in
        let manifest = try loadManifest(root)
        let bogus = STPDResultManifest(
            schemaVersion: manifest.schemaVersion, detectorVersion: manifest.detectorVersion,
            runID: manifest.runID, datasetDigest: manifest.datasetDigest, settingsDigest: manifest.settingsDigest,
            buildIdentifier: manifest.buildIdentifier, buildIdentifierKind: manifest.buildIdentifierKind,
            buildReproducibilityAttested: manifest.buildReproducibilityAttested,
            sourceMode: "not_a_real_mode", ownerName: manifest.ownerName, ownerEmail: manifest.ownerEmail,
            stringListEncoding: manifest.stringListEncoding, tables: manifest.tables
        )
        try saveManifest(bogus, root)
        expectReaderError(root, {
            if case .invalidManifestField(let field, _) = $0 { return field == "source_mode" }
            return false
        }, "invalid source_mode")
    }
}

// MARK: - Requirement 2: true writer -> on-disk package -> reader round trip with an embedded CRLF

/// A free-text cell (the consistency ledger's `details`) containing an embedded CRLF, a comma, an escaped
/// quote, and non-ASCII text is serialized by the writer, written to disk, and read back byte-for-byte.
/// Had the writer emitted the embedded CRLF unquoted, the reader's strict parser would split it into an
/// extra record and fail the structure/row-count layer.
@Test func readerRoundTripsPackageWithEmbeddedCRLFCell() throws {
    let package = try readerLocalPackage()
    try withWrittenPackage(package) { root in
        var (headers, rows) = try loadTableRows(root, .resultConsistencyCheck)
        let idx = try columnIndex(headers, "details")
        try #require(!rows.isEmpty)
        let special = "line1\r\nline2, with \"quote\" and café — 日本語"
        rows[0][idx] = special
        try rewriteTableAndResync(root, .resultConsistencyCheck, headers: headers, rows: rows)
        let result = try STPDResultPackageReader.read(packageAt: root)
        #expect(result.verified)
        // The exact bytes on disk round-trip back to the original cell.
        let (h2, r2) = try loadTableRows(root, .resultConsistencyCheck)
        #expect(r2[0][try columnIndex(h2, "details")] == special)
    }
}

// MARK: - Extra: canonical lowercase-hex digest requirement

/// The manifest's declared SHA-256 must itself be canonical lowercase 64-hex; a non-canonical form is
/// rejected rather than accepted by lowercasing it before comparison.
@Test func readerRejectsNonCanonicalDigestHex() throws {
    let package = try readerLocalPackage()
    try withWrittenPackage(package) { root in
        let manifest = try loadManifest(root)
        let updated = replacingManifestTable(manifest, fileName: STPDResultTable.runMetadata.rawValue) {
            STPDResultManifestTable(
                fileName: $0.fileName, grain: $0.grain, primaryKey: $0.primaryKey,
                columns: $0.columns, rowCount: $0.rowCount,
                sha256: String(repeating: "A", count: 64) // uppercase hex is non-canonical
            )
        }
        try saveManifest(updated, root)
        expectReaderError(root, {
            if case .nonCanonicalDigest(let t) = $0 { return t == STPDResultTable.runMetadata.rawValue }
            return false
        }, "non-canonical digest hex")
    }
}

// MARK: - Requirement 1: each manifest table definition must match the central schema contract

// Altered grain.
@Test func readerRejectsAlteredManifestTableGrain() throws {
    let package = try readerLocalPackage()
    try withWrittenPackage(package) { root in
        let manifest = try loadManifest(root)
        let updated = replacingManifestTable(manifest, fileName: STPDResultTable.runMetadata.rawValue) {
            STPDResultManifestTable(
                fileName: $0.fileName, grain: "a different grain", primaryKey: $0.primaryKey,
                columns: $0.columns, rowCount: $0.rowCount, sha256: $0.sha256
            )
        }
        try saveManifest(updated, root)
        expectReaderError(root, {
            if case .manifestContractMismatch(let t, let field) = $0 {
                return t == STPDResultTable.runMetadata.rawValue && field == "grain"
            }
            return false
        }, "altered manifest grain")
    }
}

// Altered primary key.
@Test func readerRejectsAlteredManifestTablePrimaryKey() throws {
    let package = try readerLocalPackage()
    try withWrittenPackage(package) { root in
        let manifest = try loadManifest(root)
        let updated = replacingManifestTable(manifest, fileName: STPDResultTable.runMetadata.rawValue) {
            STPDResultManifestTable(
                fileName: $0.fileName, grain: $0.grain, primaryKey: $0.primaryKey + ["extra_key"],
                columns: $0.columns, rowCount: $0.rowCount, sha256: $0.sha256
            )
        }
        try saveManifest(updated, root)
        expectReaderError(root, {
            if case .manifestContractMismatch(let t, let field) = $0 {
                return t == STPDResultTable.runMetadata.rawValue && field == "primary_key"
            }
            return false
        }, "altered manifest primary key")
    }
}

// Altered column TYPE (names/order — and thus the file header — still match).
@Test func readerRejectsAlteredManifestColumnType() throws {
    let package = try readerLocalPackage()
    try withWrittenPackage(package) { root in
        let manifest = try loadManifest(root)
        let updated = replacingManifestTable(manifest, fileName: STPDResultTable.runMetadata.rawValue) { t in
            var cols = t.columns
            let c0 = cols[0]
            let otherType: STPDResultColumnType = c0.type == .string ? .integer : .string
            cols[0] = STPDResultColumnDefinition(name: c0.name, type: otherType, nullable: c0.nullable)
            return STPDResultManifestTable(
                fileName: t.fileName, grain: t.grain, primaryKey: t.primaryKey,
                columns: cols, rowCount: t.rowCount, sha256: t.sha256
            )
        }
        try saveManifest(updated, root)
        expectReaderError(root, {
            if case .manifestContractMismatch(let t, let field) = $0 {
                return t == STPDResultTable.runMetadata.rawValue && field == "columns"
            }
            return false
        }, "altered manifest column type")
    }
}

// Altered column NULLABILITY.
@Test func readerRejectsAlteredManifestColumnNullability() throws {
    let package = try readerLocalPackage()
    try withWrittenPackage(package) { root in
        let manifest = try loadManifest(root)
        let updated = replacingManifestTable(manifest, fileName: STPDResultTable.runMetadata.rawValue) { t in
            var cols = t.columns
            let c0 = cols[0]
            cols[0] = STPDResultColumnDefinition(name: c0.name, type: c0.type, nullable: !c0.nullable)
            return STPDResultManifestTable(
                fileName: t.fileName, grain: t.grain, primaryKey: t.primaryKey,
                columns: cols, rowCount: t.rowCount, sha256: t.sha256
            )
        }
        try saveManifest(updated, root)
        expectReaderError(root, {
            if case .manifestContractMismatch(let t, let field) = $0 {
                return t == STPDResultTable.runMetadata.rawValue && field == "columns"
            }
            return false
        }, "altered manifest column nullability")
    }
}

// Altered column NAME is caught at the header layer (declared names no longer match the file header);
// this covers the name axis of the contract (column order is covered by test 15).
@Test func readerRejectsAlteredManifestColumnName() throws {
    let package = try readerLocalPackage()
    try withWrittenPackage(package) { root in
        let manifest = try loadManifest(root)
        let updated = replacingManifestTable(manifest, fileName: STPDResultTable.runMetadata.rawValue) { t in
            var cols = t.columns
            let c0 = cols[0]
            cols[0] = STPDResultColumnDefinition(name: c0.name + "_x", type: c0.type, nullable: c0.nullable)
            return STPDResultManifestTable(
                fileName: t.fileName, grain: t.grain, primaryKey: t.primaryKey,
                columns: cols, rowCount: t.rowCount, sha256: t.sha256
            )
        }
        try saveManifest(updated, root)
        expectReaderError(root, {
            if case .headerMismatch(let t) = $0 { return t == STPDResultTable.runMetadata.rawValue }
            return false
        }, "altered manifest column name")
    }
}

// MARK: - Requirement 3: manifest provenance fields bound to canonical authorities + Run_metadata

/// Returns a copy of `m` with only the named provenance field(s) overridden.
private func remakeManifest(
    _ m: STPDResultManifest,
    ownerName: String? = nil,
    ownerEmail: String? = nil,
    buildIdentifierKind: String? = nil,
    buildReproducibilityAttested: Bool? = nil,
    stringListEncoding: String? = nil
) -> STPDResultManifest {
    STPDResultManifest(
        schemaVersion: m.schemaVersion, detectorVersion: m.detectorVersion,
        runID: m.runID, datasetDigest: m.datasetDigest, settingsDigest: m.settingsDigest,
        buildIdentifier: m.buildIdentifier,
        buildIdentifierKind: buildIdentifierKind ?? m.buildIdentifierKind,
        buildReproducibilityAttested: buildReproducibilityAttested ?? m.buildReproducibilityAttested,
        sourceMode: m.sourceMode,
        ownerName: ownerName ?? m.ownerName, ownerEmail: ownerEmail ?? m.ownerEmail,
        stringListEncoding: stringListEncoding ?? m.stringListEncoding,
        tables: m.tables
    )
}

@Test func readerRejectsTamperedManifestOwnerName() throws {
    let package = try readerLocalPackage()
    try withWrittenPackage(package) { root in
        try saveManifest(remakeManifest(try loadManifest(root), ownerName: "Someone Else"), root)
        expectReaderError(root, {
            if case .invalidManifestField(let f, _) = $0 { return f == "owner_name" }
            return false
        }, "tampered manifest owner_name")
    }
}

@Test func readerRejectsTamperedManifestOwnerEmail() throws {
    let package = try readerLocalPackage()
    try withWrittenPackage(package) { root in
        try saveManifest(remakeManifest(try loadManifest(root), ownerEmail: "evil@example.com"), root)
        expectReaderError(root, {
            if case .invalidManifestField(let f, _) = $0 { return f == "owner_email" }
            return false
        }, "tampered manifest owner_email")
    }
}

@Test func readerRejectsTamperedManifestBuildIdentifierKind() throws {
    let package = try readerLocalPackage()
    try withWrittenPackage(package) { root in
        try saveManifest(remakeManifest(try loadManifest(root), buildIdentifierKind: "attested_forged"), root)
        expectReaderError(root, {
            if case .invalidManifestField(let f, _) = $0 { return f == "build_identifier_kind" }
            return false
        }, "tampered manifest build_identifier_kind")
    }
}

@Test func readerRejectsTamperedManifestBuildReproducibilityAttested() throws {
    let package = try readerLocalPackage()
    try withWrittenPackage(package) { root in
        try saveManifest(remakeManifest(try loadManifest(root), buildReproducibilityAttested: true), root)
        expectReaderError(root, {
            if case .invalidManifestField(let f, _) = $0 { return f == "build_reproducibility_attested" }
            return false
        }, "tampered manifest build_reproducibility_attested")
    }
}

@Test func readerRejectsTamperedManifestStringListEncoding() throws {
    let package = try readerLocalPackage()
    try withWrittenPackage(package) { root in
        try saveManifest(remakeManifest(try loadManifest(root), stringListEncoding: "csv_semicolons"), root)
        expectReaderError(root, {
            if case .invalidManifestField(let f, _) = $0 { return f == "string_list_encoding" }
            return false
        }, "tampered manifest string_list_encoding")
    }
}

// MARK: - Requirement 4: package-only consistency-ledger validator

// A consistency row whose run_id no longer matches the reconstructed identity fails closed (this row is
// excluded from the shared identity cross-check, so the narrow ledger validator is the sole guard).
@Test func readerRejectsConsistencyLedgerRunIDMismatch() throws {
    let package = try readerLocalPackage()
    try withWrittenPackage(package) { root in
        var (headers, rows) = try loadTableRows(root, .resultConsistencyCheck)
        let idx = try columnIndex(headers, "run_id")
        try #require(!rows.isEmpty)
        rows[0][idx] = rows[0][idx] + "_x"
        try rewriteTableAndResync(root, .resultConsistencyCheck, headers: headers, rows: rows)
        expectReaderError(root, {
            if case .consistencyLedgerInvalid = $0 { return true }
            return false
        }, "consistency ledger run_id mismatch")
    }
}

// A consistency row with status != "pass" fails closed.
@Test func readerRejectsConsistencyLedgerNonPassStatus() throws {
    let package = try readerLocalPackage()
    try withWrittenPackage(package) { root in
        var (headers, rows) = try loadTableRows(root, .resultConsistencyCheck)
        let idx = try columnIndex(headers, "status")
        try #require(!rows.isEmpty)
        rows[0][idx] = "fail"
        try rewriteTableAndResync(root, .resultConsistencyCheck, headers: headers, rows: rows)
        expectReaderError(root, {
            if case .consistencyLedgerInvalid = $0 { return true }
            return false
        }, "consistency ledger non-pass status")
    }
}

// A consistency row with severity != "info" fails closed.
@Test func readerRejectsConsistencyLedgerNonInfoSeverity() throws {
    let package = try readerLocalPackage()
    try withWrittenPackage(package) { root in
        var (headers, rows) = try loadTableRows(root, .resultConsistencyCheck)
        let idx = try columnIndex(headers, "severity")
        try #require(!rows.isEmpty)
        rows[0][idx] = "warning"
        try rewriteTableAndResync(root, .resultConsistencyCheck, headers: headers, rows: rows)
        expectReaderError(root, {
            if case .consistencyLedgerInvalid = $0 { return true }
            return false
        }, "consistency ledger non-info severity")
    }
}

// A header-only (zero-row) consistency ledger is rejected by the semantic ledger validator. Legitimate
// writer output always records at least one check (required_table_set), so an empty ledger is evidence no
// writer self-checks ran. BOTH the manifest row_count (0) and SHA-256 digest are resynced to the
// header-only bytes, so the rejection occurs in validateConsistencyLedger — not at the row-count or digest
// layer (this would be a producer-time self-consistent package, not just tampering).
@Test func readerRejectsEmptyConsistencyLedger() throws {
    let package = try readerLocalPackage()
    try withWrittenPackage(package) { root in
        let (headers, rows) = try loadTableRows(root, .resultConsistencyCheck)
        try #require(!rows.isEmpty) // the fixture genuinely starts with a non-empty ledger
        // Header-only rewrite: rewriteTableAndResync sets manifest row_count = 0 and the correct digest.
        try rewriteTableAndResync(root, .resultConsistencyCheck, headers: headers, rows: [])
        expectReaderError(root, {
            if case .consistencyLedgerInvalid(let reason) = $0 { return reason == "consistency ledger is empty" }
            return false
        }, "empty consistency ledger")
    }
}

// A duplicate check_id fails closed — caught either by the table's [run_id, check_id] primary-key
// uniqueness at reconstruction or by the ledger validator; both are fail-closed.
@Test func readerRejectsConsistencyLedgerDuplicateCheckID() throws {
    let package = try readerLocalPackage()
    try withWrittenPackage(package) { root in
        var (headers, rows) = try loadTableRows(root, .resultConsistencyCheck)
        let idx = try columnIndex(headers, "check_id")
        try #require(rows.count >= 2)
        rows[1][idx] = rows[0][idx] // collide check_id under the same run_id
        try rewriteTableAndResync(root, .resultConsistencyCheck, headers: headers, rows: rows)
        expectReaderError(root, {
            switch $0 {
            case .consistencyLedgerInvalid: return true
            case .tableStructureInvalid(let t, _): return t == STPDResultTable.resultConsistencyCheck.rawValue
            default: return false
            }
        }, "consistency ledger duplicate check_id")
    }
}
