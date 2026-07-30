import Darwin
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

private func readerLocalPackageWithSharedTrialTaskEvents() throws
    -> STPDResultPackage {
    let events = [
        TaskEvent(
            id: "evt_reader_trial_cue",
            name: "cue",
            timeSec: 0.5,
            column: "cue_column",
            eventIndex: 1,
            trialID: "trial_reader_shared",
            source: "reader-fixture"
        ),
        TaskEvent(
            id: "evt_reader_trial_reward",
            name: "reward",
            timeSec: 0.9,
            column: "reward_column",
            eventIndex: 2,
            trialID: "trial_reader_shared",
            source: "reader-fixture"
        ),
    ]
    let fixture = readerFixture(taskEvents: events)
    return try STPDResultPackageBuilder.build(
        .automatic(dataset: fixture.dataset, run: fixture.run)
    )
}

private func readerLocalPackage() throws -> STPDResultPackage {
    let fixture = readerFixture()
    let input = STPDResultPackageInput.automatic(dataset: fixture.dataset, run: fixture.run)
    return try STPDResultPackageBuilder.build(input)
}

private func readerLocalPackageWithDegenerateTrains() throws
    -> STPDResultPackage {
    let base = readerFixture().dataset
    let dataset = SpikeDataset(
        name: "reader-degenerate-population",
        sourceDescription: "reader package-only population binding",
        trains: base.trains + [
            SpikeTrain(name: "zero_spike_train", timestampsSec: []),
            SpikeTrain(name: "one_spike_train", timestampsSec: [0.5]),
        ]
    )
    let run = ClassicAnchorDetectionPipeline.run(
        dataset: dataset,
        bandSettings: TrainAdaptiveBandSettings(
            minValidISISec: 0.001,
            histogramBinWidthSec: 0.005
        ),
        buildCommit: readerBuildCommit
    )
    return try STPDResultPackageBuilder.build(
        .automatic(dataset: dataset, run: run)
    )
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

private let baselineB3LedgerRows: [String: String] = [
    "isi_complete_coverage":
        "every dataset ISI is present exactly once with sealed train, spike, time, and UID geometry",
    "task_event_projection":
        "Task_events exactly and deterministically represents every dataset task/stimulus event",
]

/// Rewrites the current v4 consistency ledger into the exact older B3 v4 shape. The B3 writer already
/// emitted the two dataset-backed rows below; B3.1 later added two package-only count rows without changing
/// the schema identifier. Removing both new rows therefore reproduces the complete historical shape.
private func rewriteConsistencyLedgerAsBaselineB3V4(
    _ root: URL
) throws {
    var (headers, rows) = try loadTableRows(
        root,
        .resultConsistencyCheck
    )
    let checkIDIndex = try columnIndex(headers, "check_id")
    let statusIndex = try columnIndex(headers, "status")
    let severityIndex = try columnIndex(headers, "severity")
    let detailsIndex = try columnIndex(headers, "details")

    for (checkID, expectedDetails) in baselineB3LedgerRows {
        let row = try #require(rows.first { $0[checkIDIndex] == checkID })
        #expect(row[statusIndex] == "pass")
        #expect(row[severityIndex] == "info")
        #expect(row[detailsIndex] == expectedDetails)
    }

    let currentOnlyIDs: Set<String> = [
        "isi_declared_row_count",
        "task_event_count",
    ]
    rows.removeAll { currentOnlyIDs.contains($0[checkIDIndex]) }
    try rewriteTableAndResync(
        root,
        .resultConsistencyCheck,
        headers: headers,
        rows: rows
    )
}

private func readerLimits(
    maximumManifestBytes: Int = STPDResultPackageReadLimits.standard.maximumManifestBytes,
    maximumTableBytes: Int = STPDResultPackageReadLimits.standard.maximumTableBytes,
    maximumTotalBytes: Int = STPDResultPackageReadLimits.standard.maximumTotalBytes,
    maximumRowsPerTable: Int = STPDResultPackageReadLimits.standard.maximumRowsPerTable,
    maximumColumnsPerTable: Int = STPDResultPackageReadLimits.standard.maximumColumnsPerTable,
    maximumFieldUnicodeScalars: Int =
        STPDResultPackageReadLimits.standard.maximumFieldUnicodeScalars,
    maximumCellsPerTable: Int =
        STPDResultPackageReadLimits.standard.maximumCellsPerTable,
    maximumTotalDecodedRows: Int =
        STPDResultPackageReadLimits.standard.maximumTotalDecodedRows,
    maximumTotalDecodedCells: Int =
        STPDResultPackageReadLimits.standard.maximumTotalDecodedCells,
    maximumTotalDecodedUTF8Bytes: Int =
        STPDResultPackageReadLimits.standard.maximumTotalDecodedUTF8Bytes
) -> STPDResultPackageReadLimits {
    STPDResultPackageReadLimits(
        maximumManifestBytes: maximumManifestBytes,
        maximumTableBytes: maximumTableBytes,
        maximumTotalBytes: maximumTotalBytes,
        maximumRowsPerTable: maximumRowsPerTable,
        maximumColumnsPerTable: maximumColumnsPerTable,
        maximumFieldUnicodeScalars: maximumFieldUnicodeScalars,
        maximumCellsPerTable: maximumCellsPerTable,
        maximumTotalDecodedRows: maximumTotalDecodedRows,
        maximumTotalDecodedCells: maximumTotalDecodedCells,
        maximumTotalDecodedUTF8Bytes: maximumTotalDecodedUTF8Bytes
    )!
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

@Test func readerLegacyFileManagerEntryPointRemainsSourceCompatible() throws {
    let package = try readerLocalPackage()
    try withWrittenPackage(package) { root in
        let result = try STPDResultPackageReader.read(
            packageAt: root,
            fileManager: .default
        )
        #expect(result.verified)
        #expect(result.runID == package.manifest.runID)
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

private func expectReaderError(
    _ root: URL,
    limits: STPDResultPackageReadLimits,
    _ match: (STPDResultPackageReaderError) -> Bool,
    _ note: String
) {
    do {
        _ = try STPDResultPackageReader.read(packageAt: root, limits: limits)
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
            if case .unexpectedPackageEntry(let name) = $0 {
                return name == "<unexpected-package-entry>"
            }
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

@Test func readerRejectsReorderedManifestTables() throws {
    let package = try readerLocalPackage()
    try withWrittenPackage(package) { root in
        let manifest = try loadManifest(root)
        var tables = manifest.tables
        try #require(tables.count >= 2)
        tables.swapAt(0, 1)
        let reordered = STPDResultManifest(
            schemaVersion: manifest.schemaVersion,
            detectorVersion: manifest.detectorVersion,
            runID: manifest.runID,
            datasetDigest: manifest.datasetDigest,
            settingsDigest: manifest.settingsDigest,
            buildIdentifier: manifest.buildIdentifier,
            buildIdentifierKind: manifest.buildIdentifierKind,
            buildReproducibilityAttested:
                manifest.buildReproducibilityAttested,
            sourceMode: manifest.sourceMode,
            ownerName: manifest.ownerName,
            ownerEmail: manifest.ownerEmail,
            stringListEncoding: manifest.stringListEncoding,
            tables: tables
        )
        try saveManifest(reordered, root)
        expectReaderError(
            root,
            {
                if case .manifestContractMismatch(let table, let field) = $0 {
                    return table == STPDResultSchema.manifestFileName
                        && field == "table_order"
                }
                return false
            },
            "reordered manifest tables"
        )
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
        let regenerated = Set(result.consistencyChecks.map(\.id))
        #expect(regenerated.contains("isi_declared_row_count"))
        #expect(regenerated.contains("task_event_count"))
        #expect(!regenerated.contains("isi_complete_coverage"))
        #expect(!regenerated.contains("task_event_projection"))

        let ledger = try #require(package.table(.resultConsistencyCheck))
        let checkIDIndex = try #require(ledger.headers.firstIndex(of: "check_id"))
        let ledgerIDs = Set(ledger.rows.map { $0[checkIDIndex] })
        #expect(ledgerIDs.contains("isi_complete_coverage"))
        #expect(ledgerIDs.contains("task_event_projection"))
    }
}

@Test func readerB31RoundTripsTaskEventWithBlankSource() throws {
    let event = TaskEvent(
        id: "evt_reader_blank_source",
        name: "cue",
        timeSec: 0.5,
        column: "cue_column",
        eventIndex: 1,
        trialID: "trial_reader_blank_source",
        source: ""
    )
    let fixture = readerFixture(taskEvents: [event])
    let package = try STPDResultPackageBuilder.build(
        .automatic(dataset: fixture.dataset, run: fixture.run)
    )
    let table = try #require(package.table(.taskEvents))
    let sourceIndex = try #require(table.headers.firstIndex(of: "source"))
    #expect(table.rowCount == 1)
    #expect(table.rows[0][sourceIndex].isEmpty)

    try withWrittenPackage(package) { root in
        let result = try STPDResultPackageReader.read(packageAt: root)
        #expect(result.verified)
        #expect(result.runID == package.manifest.runID)
    }
}

@Test func readerB31RoundTripsEmptyDatasetPackage() throws {
    let dataset = SpikeDataset(
        name: "reader-empty-dataset",
        sourceDescription: "reader empty package roundtrip",
        trains: []
    )
    let run = ClassicAnchorDetectionPipeline.run(
        dataset: dataset,
        bandSettings: TrainAdaptiveBandSettings(
            minValidISISec: 0.001,
            histogramBinWidthSec: 0.005
        ),
        buildCommit: readerBuildCommit
    )
    let package = try STPDResultPackageBuilder.build(
        .automatic(dataset: dataset, run: run)
    )
    #expect(package.table(.dataQualityQC)?.rows.isEmpty == true)
    #expect(package.table(.isiLabelsFinal)?.rows.isEmpty == true)

    try withWrittenPackage(package) { root in
        let result = try STPDResultPackageReader.read(packageAt: root)
        #expect(result.verified)
        #expect(result.runID == package.manifest.runID)
    }
}

@Test func readerRoundTripsMultipleTaskEventsWithinOneTrial() throws {
    let package = try readerLocalPackageWithSharedTrialTaskEvents()
    try withWrittenPackage(package) { root in
        let result = try STPDResultPackageReader.read(packageAt: root)
        #expect(result.verified)

        let table = try #require(package.table(.taskEvents))
        #expect(table.rowCount == 2)
        let trialIDIndex = try #require(
            table.headers.firstIndex(of: "trial_id")
        )
        #expect(Set(table.rows.map { $0[trialIDIndex] })
            == ["trial_reader_shared"])
    }
}

@Test func readerAcceptsBaselineB3V4ConsistencyLedgerVariant() throws {
    let package = try readerLocalPackageWithTaskEvents()
    try withWrittenPackage(package) { root in
        try rewriteConsistencyLedgerAsBaselineB3V4(root)

        let result = try STPDResultPackageReader.read(packageAt: root)
        #expect(result.verified)
        #expect(result.runID == package.manifest.runID)
        #expect(Set(result.consistencyChecks.map(\.id)).contains("isi_declared_row_count"))
        #expect(Set(result.consistencyChecks.map(\.id)).contains("task_event_count"))
    }
}

@Test func readerRejectsMixedBaselineB3AndCurrentV4LedgerShape() throws {
    let package = try readerLocalPackageWithTaskEvents()
    try withWrittenPackage(package) { root in
        var (headers, rows) = try loadTableRows(
            root,
            .resultConsistencyCheck
        )
        let checkIDIndex = try columnIndex(headers, "check_id")
        rows.removeAll { $0[checkIDIndex] == "task_event_count" }
        try rewriteTableAndResync(
            root,
            .resultConsistencyCheck,
            headers: headers,
            rows: rows
        )

        expectReaderError(root, {
            if case .consistencyLedgerInvalid(let reason) = $0 {
                return reason.contains(
                    "mixes baseline-B3 and current v4"
                )
            }
            return false
        }, "mixed baseline-B3/current-v4 consistency-ledger shape")
    }
}

@Test func readerRejectsAlteredBaselineB3LegacyLedgerDetail() throws {
    let package = try readerLocalPackageWithTaskEvents()
    try withWrittenPackage(package) { root in
        try rewriteConsistencyLedgerAsBaselineB3V4(root)

        var (headers, rows) = try loadTableRows(
            root,
            .resultConsistencyCheck
        )
        let checkIDIndex = try columnIndex(headers, "check_id")
        let detailsIndex = try columnIndex(headers, "details")
        let rowIndex = try #require(
            rows.firstIndex {
                $0[checkIDIndex] == "task_event_projection"
            }
        )
        rows[rowIndex][detailsIndex] += " altered"
        try rewriteTableAndResync(
            root,
            .resultConsistencyCheck,
            headers: headers,
            rows: rows
        )

        expectReaderError(root, {
            if case .consistencyLedgerInvalid(let reason) = $0 {
                return reason.contains(
                    "task_event_projection does not match"
                )
            }
            return false
        }, "altered baseline-B3 legacy consistency detail")
    }
}

@Test func readerRejectsUnknownCurrentV4ConsistencyCheck() throws {
    let package = try readerLocalPackageWithTaskEvents()
    try withWrittenPackage(package) { root in
        var (headers, rows) = try loadTableRows(
            root,
            .resultConsistencyCheck
        )
        let checkIDIndex = try columnIndex(headers, "check_id")
        let detailsIndex = try columnIndex(headers, "details")
        var unknown = try #require(rows.first)
        unknown[checkIDIndex] = "unknown_passing_check"
        unknown[detailsIndex] = "an unregistered passing row"
        rows.append(unknown)
        try rewriteTableAndResync(
            root,
            .resultConsistencyCheck,
            headers: headers,
            rows: rows
        )

        expectReaderError(root, {
            if case .consistencyLedgerInvalid(let reason) = $0 {
                return reason.contains("shape mismatch")
                    && reason.contains("unexpected=unknown_passing_check")
            }
            return false
        }, "unknown current-v4 consistency check")
    }
}

@Test func readerRejectsMissingCurrentV4HistoricalConsistencyCheck() throws {
    let package = try readerLocalPackageWithTaskEvents()
    try withWrittenPackage(package) { root in
        var (headers, rows) = try loadTableRows(
            root,
            .resultConsistencyCheck
        )
        let checkIDIndex = try columnIndex(headers, "check_id")
        rows.removeAll {
            $0[checkIDIndex] == "automatic_projection_authority"
        }
        try rewriteTableAndResync(
            root,
            .resultConsistencyCheck,
            headers: headers,
            rows: rows
        )

        expectReaderError(root, {
            if case .consistencyLedgerInvalid(let reason) = $0 {
                return reason.contains("shape mismatch")
                    && reason.contains(
                        "missing=automatic_projection_authority"
                    )
            }
            return false
        }, "missing current-v4 historical consistency check")
    }
}

@Test func readerRejectsUnknownBaselineB3V4ConsistencyCheck() throws {
    let package = try readerLocalPackageWithTaskEvents()
    try withWrittenPackage(package) { root in
        try rewriteConsistencyLedgerAsBaselineB3V4(root)

        var (headers, rows) = try loadTableRows(
            root,
            .resultConsistencyCheck
        )
        let checkIDIndex = try columnIndex(headers, "check_id")
        let detailsIndex = try columnIndex(headers, "details")
        var unknown = try #require(rows.first)
        unknown[checkIDIndex] = "unknown_baseline_passing_check"
        unknown[detailsIndex] = "an unregistered baseline passing row"
        rows.append(unknown)
        try rewriteTableAndResync(
            root,
            .resultConsistencyCheck,
            headers: headers,
            rows: rows
        )

        expectReaderError(root, {
            if case .consistencyLedgerInvalid(let reason) = $0 {
                return reason.contains("shape mismatch")
                    && reason.contains(
                        "unexpected=unknown_baseline_passing_check"
                    )
            }
            return false
        }, "unknown baseline-B3-v4 consistency check")
    }
}

@Test func readerRejectsMissingBaselineB3V4HistoricalConsistencyCheck() throws {
    let package = try readerLocalPackageWithTaskEvents()
    try withWrittenPackage(package) { root in
        try rewriteConsistencyLedgerAsBaselineB3V4(root)

        var (headers, rows) = try loadTableRows(
            root,
            .resultConsistencyCheck
        )
        let checkIDIndex = try columnIndex(headers, "check_id")
        rows.removeAll {
            $0[checkIDIndex] == "automatic_projection_authority"
        }
        try rewriteTableAndResync(
            root,
            .resultConsistencyCheck,
            headers: headers,
            rows: rows
        )

        expectReaderError(root, {
            if case .consistencyLedgerInvalid(let reason) = $0 {
                return reason.contains("shape mismatch")
                    && reason.contains(
                        "missing=automatic_projection_authority"
                    )
            }
            return false
        }, "missing baseline-B3-v4 historical consistency check")
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

@Test func readerRejectsFIFOManifestWithoutBlocking() throws {
    let package = try readerLocalPackage()
    try withWrittenPackage(package) { root in
        let entry = manifestURL(root)
        try FileManager.default.removeItem(at: entry)
        try #require(mkfifo(entry.path, mode_t(0o600)) == 0)
        expectReaderError(
            root,
            {
                if case .entryNotARegularFile(let name) = $0 {
                    return name == STPDResultSchema.manifestFileName
                }
                return false
            },
            "FIFO manifest"
        )
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

@Test func readerRejectsTaskEventAuthorityAndIdentityTampering() throws {
    let package = try readerLocalPackageWithTaskEvents()
    let mutations: [(column: String, value: String, reason: String)] = [
        (
            "event_name",
            "forged_cue",
            "deterministic scientific identity"
        ),
        (
            "source",
            "",
            "task_event_source_digest does not match the sealed run"
        ),
        (
            "run_id",
            "forged_run",
            "row identity differs from the declared detector run"
        ),
    ]

    for mutation in mutations {
        try withWrittenPackage(package) { root in
            var (headers, rows) = try loadTableRows(root, .taskEvents)
            let index = try columnIndex(headers, mutation.column)
            try #require(rows.count == 1)
            rows[0][index] = mutation.value
            try rewriteTableAndResync(
                root,
                .taskEvents,
                headers: headers,
                rows: rows
            )
            expectReaderError(
                root,
                {
                    guard case .packageValidationFailed(let reason) = $0 else {
                        return false
                    }
                    return reason.contains(mutation.reason)
                },
                "task-event \(mutation.column) authority tampering"
            )
        }
    }
}

@Test func readerRejectsDuplicateTaskEventSourceIdentity() throws {
    let package = try readerLocalPackageWithSharedTrialTaskEvents()
    try withWrittenPackage(package) { root in
        var (headers, rows) = try loadTableRows(root, .taskEvents)
        let sourceIDIndex = try columnIndex(headers, "source_event_id")
        try #require(rows.count == 2)
        rows[1][sourceIDIndex] = rows[0][sourceIDIndex]
        try rewriteTableAndResync(
            root,
            .taskEvents,
            headers: headers,
            rows: rows
        )
        expectReaderError(
            root,
            {
                guard case .packageValidationFailed(let reason) = $0 else {
                    return false
                }
                return reason.contains("task-event source ids must be unique")
            },
            "duplicate task-event source identity"
        )
    }
}

@Test func readerRejectsRunMetadataPopulationCountsThatContradictMaterializedTables()
    throws {
    let package = try readerLocalPackage()
    for column in ["train_count", "spike_count"] {
        try withWrittenPackage(package) { root in
            var (headers, rows) = try loadTableRows(root, .runMetadata)
            let index = try columnIndex(headers, column)
            try #require(!rows.isEmpty)
            let current = try #require(Int(rows[0][index]))
            rows[0][index] = String(current + 1)
            try rewriteTableAndResync(
                root,
                .runMetadata,
                headers: headers,
                rows: rows
            )
            expectReaderError(
                root,
                {
                    if case .packageValidationFailed = $0 {
                        return true
                    }
                    return false
                },
                "run metadata \(column) contradiction"
            )
        }
    }
}

@Test func readerRejectsCoordinatedQCPopulationTamperingWithoutSealedDataset()
    throws {
    let package = try readerLocalPackage()
    for mutateRawISICount in [false, true] {
        try withWrittenPackage(package) { root in
            var (qcHeaders, qcRows) = try loadTableRows(root, .dataQualityQC)
            let qcSpikeIndex = try columnIndex(qcHeaders, "spike_count")
            let qcRawISIIndex = try columnIndex(qcHeaders, "raw_isi_count")
            try #require(!qcRows.isEmpty)
            let originalSpikeCount = try #require(Int(qcRows[0][qcSpikeIndex]))
            let originalRawISICount = try #require(Int(qcRows[0][qcRawISIIndex]))
            qcRows[0][qcSpikeIndex] = String(originalSpikeCount + 1)
            if mutateRawISICount {
                qcRows[0][qcRawISIIndex] = String(originalRawISICount + 1)
            }
            try rewriteTableAndResync(
                root,
                .dataQualityQC,
                headers: qcHeaders,
                rows: qcRows
            )

            var (metadataHeaders, metadataRows) = try loadTableRows(
                root,
                .runMetadata
            )
            let metadataSpikeIndex = try columnIndex(
                metadataHeaders,
                "spike_count"
            )
            try #require(metadataRows.count == 1)
            let originalMetadataSpikeCount = try #require(
                Int(metadataRows[0][metadataSpikeIndex])
            )
            metadataRows[0][metadataSpikeIndex] = String(
                originalMetadataSpikeCount + 1
            )
            try rewriteTableAndResync(
                root,
                .runMetadata,
                headers: metadataHeaders,
                rows: metadataRows
            )

            expectReaderError(
                root,
                {
                    guard case .packageValidationFailed(let reason) = $0 else {
                        return false
                    }
                    return mutateRawISICount
                        ? reason.contains(
                            "raw_isi_count contradicts the materialized ISI row count"
                        )
                        : reason.contains(
                            "raw_isi_count contradicts spike_count"
                        )
                },
                mutateRawISICount
                    ? "coordinated QC spike/raw population inflation"
                    : "coordinated QC spike population inflation"
            )
        }
    }
}

@Test func readerRejectsResolvedQualityValueThatContradictsMaterializedQC()
    throws {
    let package = try readerLocalPackage()
    try withWrittenPackage(package) { root in
        var (headers, rows) = try loadTableRows(root, .resolvedParameters)
        let keyIndex = try columnIndex(headers, "parameter_key")
        let valueIndex = try columnIndex(headers, "effective_value")
        let rowIndex = try #require(
            rows.firstIndex {
                $0[keyIndex] == "quality.valid_isi_count"
            }
        )
        let current = try #require(Int(rows[rowIndex][valueIndex]))
        rows[rowIndex][valueIndex] = String(current + 1)
        try rewriteTableAndResync(
            root,
            .resolvedParameters,
            headers: headers,
            rows: rows
        )
        expectReaderError(
            root,
            {
                guard case .packageValidationFailed(let reason) = $0 else {
                    return false
                }
                return reason.contains(
                    "resolved quality.valid_isi_count differs from materialized QC"
                )
            },
            "resolved quality value contradicts materialized QC"
        )
    }
}

@Test func readerRejectsIncompleteOrMisattributedResolvedQualityPopulation()
    throws {
    let package = try readerLocalPackage()
    enum Mutation {
        case removeRequiredKey
        case wrongSource
        case wrongTrainName
    }
    let mutations: [(Mutation, String)] = [
        (.removeRequiredKey, "incomplete key set"),
        (.wrongSource, "invalid scope, key, or source"),
        (.wrongTrainName, "wrong train name"),
    ]

    for (mutation, expectedReason) in mutations {
        try withWrittenPackage(package) { root in
            var (headers, rows) = try loadTableRows(root, .resolvedParameters)
            let keyIndex = try columnIndex(headers, "parameter_key")
            let sourceIndex = try columnIndex(headers, "source")
            let nameIndex = try columnIndex(headers, "scope_name")
            let rowIndex = try #require(
                rows.firstIndex {
                    $0[keyIndex] == "quality.valid_isi_count"
                }
            )
            switch mutation {
            case .removeRequiredKey:
                rows.remove(at: rowIndex)
            case .wrongSource:
                rows[rowIndex][sourceIndex] = "forged_source"
            case .wrongTrainName:
                rows[rowIndex][nameIndex] = "forged_train_name"
            }
            try rewriteTableAndResync(
                root,
                .resolvedParameters,
                headers: headers,
                rows: rows
            )
            expectReaderError(
                root,
                {
                    guard case .packageValidationFailed(let reason) = $0 else {
                        return false
                    }
                    return reason.contains(expectedReason)
                },
                "resolved quality population \(expectedReason)"
            )
        }
    }
}

@Test func readerReportsResolvedQualityFailuresInSortedTrainOrder() throws {
    let package = try readerLocalPackage()
    try withWrittenPackage(package) { root in
        var (headers, rows) = try loadTableRows(root, .resolvedParameters)
        let keyIndex = try columnIndex(headers, "parameter_key")
        let scopeIDIndex = try columnIndex(headers, "scope_id")
        let valueIndex = try columnIndex(headers, "effective_value")
        var affectedTrainIDs: [String] = []
        for index in rows.indices
        where rows[index][keyIndex] == "quality.valid_isi_count" {
            affectedTrainIDs.append(rows[index][scopeIDIndex])
            let current = try #require(Int(rows[index][valueIndex]))
            rows[index][valueIndex] = String(current + 1)
        }
        let firstTrainID = try #require(affectedTrainIDs.sorted().first)
        #expect(Set(affectedTrainIDs).count >= 2)
        try rewriteTableAndResync(
            root,
            .resolvedParameters,
            headers: headers,
            rows: rows
        )
        expectReaderError(
            root,
            {
                guard case .packageValidationFailed(let reason) = $0 else {
                    return false
                }
                return reason.contains(
                    "resolved quality.valid_isi_count differs from materialized QC"
                ) && reason.contains("train \(firstTrainID)")
            },
            "deterministic sorted-train quality diagnostic"
        )
    }
}

@Test func readerRejectsISITrainGeometryAndNoncontiguousIndices() throws {
    let package = try readerLocalPackage()
    enum Mutation {
        case wrongTrainName
        case inconsistentSpikeGeometry
        case noncontiguousIndex
    }
    let mutations: [(Mutation, String)] = [
        (
            .wrongTrainName,
            "train name or spike geometry is invalid"
        ),
        (
            .inconsistentSpikeGeometry,
            "train name or spike geometry is invalid"
        ),
        (
            .noncontiguousIndex,
            "does not exactly cover contiguous indices"
        ),
    ]

    for (mutation, expectedReason) in mutations {
        try withWrittenPackage(package) { root in
            var (headers, rows) = try loadTableRows(root, .isiLabelsFinal)
            let trainIDIndex = try columnIndex(headers, "train_id")
            let trainNameIndex = try columnIndex(headers, "train_name")
            let isiIndex = try columnIndex(headers, "isi_index")
            let uidIndex = try columnIndex(headers, "isi_uid")
            let leftArrayIndex = try columnIndex(
                headers,
                "left_spike_array_index"
            )
            let rightArrayIndex = try columnIndex(
                headers,
                "right_spike_array_index"
            )
            let leftOrdinalIndex = try columnIndex(
                headers,
                "left_spike_ordinal"
            )
            let rightOrdinalIndex = try columnIndex(
                headers,
                "right_spike_ordinal"
            )
            try #require(!rows.isEmpty)

            switch mutation {
            case .wrongTrainName:
                rows[0][trainNameIndex] = "forged_train_name"
            case .inconsistentSpikeGeometry:
                rows[0][leftArrayIndex] = rows[0][rightArrayIndex]
            case .noncontiguousIndex:
                let trainID = rows[0][trainIDIndex]
                let rowIndex = try #require(
                    rows.indices
                        .filter { rows[$0][trainIDIndex] == trainID }
                        .max {
                            (Int(rows[$0][isiIndex]) ?? 0)
                                < (Int(rows[$1][isiIndex]) ?? 0)
                        }
                )
                let originalIndex = try #require(
                    Int(rows[rowIndex][isiIndex])
                )
                let changedIndex = originalIndex + 1
                rows[rowIndex][isiIndex] = String(changedIndex)
                rows[rowIndex][leftArrayIndex] = String(changedIndex - 1)
                rows[rowIndex][rightArrayIndex] = String(changedIndex)
                rows[rowIndex][leftOrdinalIndex] = String(changedIndex)
                rows[rowIndex][rightOrdinalIndex] = String(changedIndex + 1)
                rows[rowIndex][uidIndex] =
                    STPDStableIdentifier.make(
                        prefix: "isi",
                        domain: "stpd_isi_uid_v1",
                        components: [
                            package.manifest.datasetDigest,
                            trainID,
                            String(changedIndex),
                        ]
                    )
            }

            try rewriteTableAndResync(
                root,
                .isiLabelsFinal,
                headers: headers,
                rows: rows
            )
            expectReaderError(
                root,
                {
                    guard case .packageValidationFailed(let reason) = $0 else {
                        return false
                    }
                    return reason.contains(expectedReason)
                },
                "ISI geometry mutation \(expectedReason)"
            )
        }
    }
}

// MARK: - B3.1 adversarial package-only authority tests

@Test func readerB31RejectsMaximumISIIndexWithoutIntegerOverflow() throws {
    let package = try readerLocalPackage()
    try withWrittenPackage(package) { root in
        var (headers, rows) = try loadTableRows(root, .isiLabelsFinal)
        let trainIDIndex = try columnIndex(headers, "train_id")
        let isiIndex = try columnIndex(headers, "isi_index")
        let uidIndex = try columnIndex(headers, "isi_uid")
        let leftArrayIndex = try columnIndex(
            headers,
            "left_spike_array_index"
        )
        let rightArrayIndex = try columnIndex(
            headers,
            "right_spike_array_index"
        )
        let leftOrdinalIndex = try columnIndex(
            headers,
            "left_spike_ordinal"
        )
        let rightOrdinalIndex = try columnIndex(
            headers,
            "right_spike_ordinal"
        )
        try #require(!rows.isEmpty)

        let maximum = Int.max
        let rowIndex = rows.index(before: rows.endIndex)
        rows[rowIndex][isiIndex] = String(maximum)
        rows[rowIndex][leftArrayIndex] = String(maximum - 1)
        rows[rowIndex][rightArrayIndex] = String(maximum)
        rows[rowIndex][leftOrdinalIndex] = String(maximum)
        rows[rowIndex][rightOrdinalIndex] = String(maximum)
        rows[rowIndex][uidIndex] = STPDStableIdentifier.make(
            prefix: "isi",
            domain: "stpd_isi_uid_v1",
            components: [
                package.manifest.datasetDigest,
                rows[rowIndex][trainIDIndex],
                String(maximum),
            ]
        )

        try rewriteTableAndResync(
            root,
            .isiLabelsFinal,
            headers: headers,
            rows: rows
        )
        expectReaderError(
            root,
            {
                guard case .packageValidationFailed(let reason) = $0 else {
                    return false
                }
                return reason.contains(
                    "train name or spike geometry is invalid"
                )
            },
            "maximum ISI index must fail closed without trapping"
        )
    }
}

@Test func readerB31RejectsTaskEventSourceDigestTampering() throws {
    let package = try readerLocalPackageWithTaskEvents()
    try withWrittenPackage(package) { root in
        var (headers, rows) = try loadTableRows(root, .taskEvents)
        let sourceIndex = try columnIndex(headers, "source")
        try #require(rows.count == 1)
        rows[0][sourceIndex] = "forged-nonempty-source"
        try rewriteTableAndResync(
            root,
            .taskEvents,
            headers: headers,
            rows: rows
        )
        expectReaderError(
            root,
            {
                guard case .packageValidationFailed(let reason) = $0 else {
                    return false
                }
                return reason.contains(
                    "task_event_source_digest does not match the sealed run"
                )
            },
            "task-event source digest tampering"
        )
    }
}

@Test func readerB31RejectsResolvedQualityFixedEnvelopeTampering() throws {
    let package = try readerLocalPackage()
    try withWrittenPackage(package) { root in
        var (headers, rows) = try loadTableRows(root, .resolvedParameters)
        let keyIndex = try columnIndex(headers, "parameter_key")
        let requestedIndex = try columnIndex(headers, "requested_value")
        let rowIndex = try #require(
            rows.firstIndex {
                $0[keyIndex] == "quality.valid_isi_count"
            }
        )
        rows[rowIndex][requestedIndex] = "forged-requested-value"
        try rewriteTableAndResync(
            root,
            .resolvedParameters,
            headers: headers,
            rows: rows
        )
        expectReaderError(
            root,
            {
                guard case .packageValidationFailed(let reason) = $0 else {
                    return false
                }
                return reason.contains("invalid fixed row envelope")
            },
            "resolved quality fixed-envelope tampering"
        )
    }
}

@Test func readerB31RejectsPreviouslyUnboundResolvedQualityValues() throws {
    let package = try readerLocalPackage()
    let mutations: [
        (key: String, expectedReason: String, isReal: Bool)
    ] = [
        (
            "first_spike_sec",
            "contradicts reconstructed ISI evidence",
            true
        ),
        (
            "last_spike_sec",
            "contradicts reconstructed ISI evidence",
            true
        ),
        (
            "zero_or_negative_timestamp_step_count",
            "contradicts reconstructed ISI evidence",
            false
        ),
        (
            "input_duplicate_timestamp_step_count",
            "differs from the materialized ISI snapshot",
            false
        ),
        (
            "input_zero_or_negative_step_count",
            "differs from the materialized ISI snapshot",
            false
        ),
    ]

    for mutation in mutations {
        try withWrittenPackage(package) { root in
            var (headers, rows) = try loadTableRows(
                root,
                .resolvedParameters
            )
            let keyIndex = try columnIndex(headers, "parameter_key")
            let valueIndex = try columnIndex(headers, "effective_value")
            let rowIndex = try #require(
                rows.firstIndex {
                    $0[keyIndex] == "quality.\(mutation.key)"
                }
            )
            if mutation.isReal {
                let current = try #require(
                    Double(rows[rowIndex][valueIndex])
                )
                rows[rowIndex][valueIndex] =
                    STPDCanonicalValue.double(current + 0.125)
            } else {
                let current = try #require(
                    Int(rows[rowIndex][valueIndex])
                )
                rows[rowIndex][valueIndex] = String(current + 1)
            }
            try rewriteTableAndResync(
                root,
                .resolvedParameters,
                headers: headers,
                rows: rows
            )
            expectReaderError(
                root,
                {
                    guard case .packageValidationFailed(let reason) = $0 else {
                        return false
                    }
                    return reason.contains(
                        "resolved quality.\(mutation.key)"
                    ) && reason.contains(mutation.expectedReason)
                },
                "unbound resolved quality.\(mutation.key) tampering"
            )
        }
    }
}

@Test func readerB31RejectsQCThresholdsThatDisagreeWithISI() throws {
    let package = try readerLocalPackage()
    let columns = [
        "artifact_threshold_sec",
        "refractory_suspect_threshold_sec",
    ]
    for column in columns {
        try withWrittenPackage(package) { root in
            var (headers, rows) = try loadTableRows(root, .dataQualityQC)
            let thresholdIndex = try columnIndex(headers, column)
            try #require(!rows.isEmpty)
            for rowIndex in rows.indices {
                let current = try #require(
                    Double(rows[rowIndex][thresholdIndex])
                )
                rows[rowIndex][thresholdIndex] =
                    STPDCanonicalValue.double(current + 0.0005)
            }
            try rewriteTableAndResync(
                root,
                .dataQualityQC,
                headers: headers,
                rows: rows
            )
            expectReaderError(
                root,
                {
                    guard case .packageValidationFailed(let reason) = $0 else {
                        return false
                    }
                    return reason.contains(
                        "Data_quality_QC thresholds disagree with ISI_labels_final"
                    )
                },
                "QC \(column) disagrees with ISI threshold projection"
            )
        }
    }
}

@Test func readerB31RejectsISITemporalGeometryTampering() throws {
    let package = try readerLocalPackage()
    try withWrittenPackage(package) { root in
        var (headers, rows) = try loadTableRows(root, .isiLabelsFinal)
        let trainIDIndex = try columnIndex(headers, "train_id")
        let isiIndex = try columnIndex(headers, "isi_index")
        let timestampIndex = try columnIndex(headers, "timestamp_sec")
        let targetTrainID = try #require(rows.first?[trainIDIndex])
        let rowIndex = try #require(
            rows.firstIndex {
                $0[trainIDIndex] == targetTrainID
                    && $0[isiIndex] == "2"
            }
        )
        let current = try #require(Double(rows[rowIndex][timestampIndex]))
        rows[rowIndex][timestampIndex] =
            STPDCanonicalValue.double(current + 0.1)
        try rewriteTableAndResync(
            root,
            .isiLabelsFinal,
            headers: headers,
            rows: rows
        )
        expectReaderError(
            root,
            {
                guard case .packageValidationFailed(let reason) = $0 else {
                    return false
                }
                return reason.contains(
                    "temporal geometry is inconsistent"
                )
            },
            "ISI timestamp geometry tampering"
        )
    }
}

@Test
func readerB31RejectsCoordinatedQCResolvedAndSnapshotMetricTampering()
    throws {
    let package = try readerLocalPackage()
    try withWrittenPackage(package) { root in
        var (qcHeaders, qcRows) = try loadTableRows(root, .dataQualityQC)
        let qcTrainIDIndex = try columnIndex(qcHeaders, "train_id")
        let qcDuplicateIndex = try columnIndex(
            qcHeaders,
            "duplicate_timestamp_count"
        )
        try #require(!qcRows.isEmpty)
        let trainID = qcRows[0][qcTrainIDIndex]
        let forgedValue = String(
            try #require(Int(qcRows[0][qcDuplicateIndex])) + 1
        )
        qcRows[0][qcDuplicateIndex] = forgedValue
        try rewriteTableAndResync(
            root,
            .dataQualityQC,
            headers: qcHeaders,
            rows: qcRows
        )

        var (isiHeaders, isiRows) = try loadTableRows(
            root,
            .isiLabelsFinal
        )
        let isiTrainIDIndex = try columnIndex(isiHeaders, "train_id")
        let snapshotIndex = try columnIndex(
            isiHeaders,
            "train_qc_duplicate_timestamp_count"
        )
        var mutatedISIRowCount = 0
        for rowIndex in isiRows.indices
        where isiRows[rowIndex][isiTrainIDIndex] == trainID {
            isiRows[rowIndex][snapshotIndex] = forgedValue
            mutatedISIRowCount += 1
        }
        #expect(mutatedISIRowCount > 0)
        try rewriteTableAndResync(
            root,
            .isiLabelsFinal,
            headers: isiHeaders,
            rows: isiRows
        )

        var (resolvedHeaders, resolvedRows) = try loadTableRows(
            root,
            .resolvedParameters
        )
        let scopeIDIndex = try columnIndex(
            resolvedHeaders,
            "scope_id"
        )
        let keyIndex = try columnIndex(
            resolvedHeaders,
            "parameter_key"
        )
        let valueIndex = try columnIndex(
            resolvedHeaders,
            "effective_value"
        )
        let resolvedRowIndex = try #require(
            resolvedRows.firstIndex {
                $0[scopeIDIndex] == trainID
                    && $0[keyIndex] ==
                        "quality.duplicate_timestamp_count"
            }
        )
        resolvedRows[resolvedRowIndex][valueIndex] = forgedValue
        try rewriteTableAndResync(
            root,
            .resolvedParameters,
            headers: resolvedHeaders,
            rows: resolvedRows
        )

        expectReaderError(
            root,
            {
                guard case .packageValidationFailed(let reason) = $0 else {
                    return false
                }
                return reason.contains(
                    "Data_quality_QC duplicate_timestamp_count contradicts reconstructed ISI evidence"
                )
            },
            "coordinated QC, ISI snapshot, and resolved quality tampering"
        )
    }
}

@Test
func readerRejectsPhantomOneSpikeTrainEvenWhenQCAndMetadataAgree() throws {
    let package = try readerLocalPackageWithDegenerateTrains()
    try withWrittenPackage(package) { root in
        var (qcHeaders, qcRows) = try loadTableRows(root, .dataQualityQC)
        let trainIDIndex = try columnIndex(qcHeaders, "train_id")
        let trainNameIndex = try columnIndex(qcHeaders, "train_name")
        let oneSpikeRow = try #require(
            qcRows.first { $0[trainNameIndex] == "one_spike_train" }
        )
        var phantom = oneSpikeRow
        phantom[trainIDIndex] = "zz_phantom_one_spike_train"
        phantom[trainNameIndex] = "phantom_one_spike_train"
        qcRows.append(phantom)
        try rewriteTableAndResync(
            root,
            .dataQualityQC,
            headers: qcHeaders,
            rows: qcRows
        )

        var (metadataHeaders, metadataRows) = try loadTableRows(
            root,
            .runMetadata
        )
        let trainCountIndex = try columnIndex(
            metadataHeaders,
            "train_count"
        )
        let spikeCountIndex = try columnIndex(
            metadataHeaders,
            "spike_count"
        )
        try #require(metadataRows.count == 1)
        metadataRows[0][trainCountIndex] = String(
            try #require(Int(metadataRows[0][trainCountIndex])) + 1
        )
        metadataRows[0][spikeCountIndex] = String(
            try #require(Int(metadataRows[0][spikeCountIndex])) + 1
        )
        try rewriteTableAndResync(
            root,
            .runMetadata,
            headers: metadataHeaders,
            rows: metadataRows
        )

        expectReaderError(
            root,
            {
                guard case .packageValidationFailed(let reason) = $0 else {
                    return false
                }
                return reason.contains(
                    "resolved quality population"
                )
            },
            "phantom one-spike train synchronized across QC and metadata"
        )
    }
}

@Test
func readerRejectsDeletedOneSpikeTrainEvenWhenQCAndMetadataAgree() throws {
    let package = try readerLocalPackageWithDegenerateTrains()
    try withWrittenPackage(package) { root in
        var (qcHeaders, qcRows) = try loadTableRows(root, .dataQualityQC)
        let trainNameIndex = try columnIndex(qcHeaders, "train_name")
        let originalCount = qcRows.count
        qcRows.removeAll {
            $0[trainNameIndex] == "one_spike_train"
        }
        try #require(qcRows.count == originalCount - 1)
        try rewriteTableAndResync(
            root,
            .dataQualityQC,
            headers: qcHeaders,
            rows: qcRows
        )

        var (metadataHeaders, metadataRows) = try loadTableRows(
            root,
            .runMetadata
        )
        let trainCountIndex = try columnIndex(
            metadataHeaders,
            "train_count"
        )
        let spikeCountIndex = try columnIndex(
            metadataHeaders,
            "spike_count"
        )
        try #require(metadataRows.count == 1)
        metadataRows[0][trainCountIndex] = String(
            try #require(Int(metadataRows[0][trainCountIndex])) - 1
        )
        metadataRows[0][spikeCountIndex] = String(
            try #require(Int(metadataRows[0][spikeCountIndex])) - 1
        )
        try rewriteTableAndResync(
            root,
            .runMetadata,
            headers: metadataHeaders,
            rows: metadataRows
        )

        expectReaderError(
            root,
            {
                guard case .packageValidationFailed(let reason) = $0 else {
                    return false
                }
                return reason.contains(
                    "resolved quality population"
                )
            },
            "deleted one-spike train synchronized across QC and metadata"
        )
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

/// A coordinated rewrite can encode an arbitrary consistency detail as valid RFC 4180, including an
/// embedded CRLF, but semantic readback still rejects it because consistency details are canonical
/// contract text. `readerStrictCSVHandlesQuotingCRLFAndNonASCII` separately pins the lossless parser.
@Test func readerRejectsNoncanonicalConsistencyDetailEvenWhenRFC4180Escaped() throws {
    let package = try readerLocalPackage()
    try withWrittenPackage(package) { root in
        var (headers, rows) = try loadTableRows(root, .resultConsistencyCheck)
        let idx = try columnIndex(headers, "details")
        try #require(!rows.isEmpty)
        let special = "line1\r\nline2, with \"quote\" and café — 日本語"
        rows[0][idx] = special
        try rewriteTableAndResync(root, .resultConsistencyCheck, headers: headers, rows: rows)

        // Serialization itself remains lossless and valid RFC 4180.
        let (h2, r2) = try loadTableRows(root, .resultConsistencyCheck)
        #expect(r2[0][try columnIndex(h2, "details")] == special)
        expectReaderError(root, {
            if case .consistencyLedgerInvalid(let reason) = $0 {
                return reason.contains("does not match its canonical ledger row")
            }
            return false
        }, "noncanonical consistency detail encoded as valid RFC 4180")
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
        for row in rows.indices {
            rows[row][idx] = rows[row][idx] + "_x"
        }
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

// MARK: - B3.1: authoritative schema, canonical bytes, bounded reads, and FD pinning

@Test func readerReturnsImmutableNormalizedTablesAfterPackageRemoval() throws {
    let package = try readerLocalPackage()
    let result = try withWrittenPackage(package) { root in
        try STPDResultPackageReader.read(packageAt: root)
    }

    let runMetadata = try #require(result.table(.runMetadata))
    #expect(runMetadata.rowCount == 1)
    #expect(runMetadata.columns.map(\.name) == package.table(.runMetadata)?.headers)
    #expect(runMetadata.rows == package.table(.runMetadata)?.rows)
    #expect(runMetadata.value(row: 0, column: "run_id") == package.manifest.runID)
    #expect(runMetadata.value(row: 1, column: "run_id") == nil)
    #expect(runMetadata.value(row: 0, column: "not_a_column") == nil)
}

@Test func readerRejectsCoordinatedUnknownColumnInManifestAndCSV() throws {
    let package = try readerLocalPackage()
    try withWrittenPackage(package) { root in
        let table = STPDResultTable.runMetadata
        var (headers, rows) = try loadTableRows(root, table)
        headers.append("unknown_column")
        for index in rows.indices {
            rows[index].append("coordinated")
        }
        let bytes = STPDRFC4180.data(headers: headers, rows: rows)
        try bytes.write(to: tableURL(root, table))

        let manifest = try loadManifest(root)
        let updated = replacingManifestTable(manifest, fileName: table.rawValue) {
            STPDResultManifestTable(
                fileName: $0.fileName,
                grain: $0.grain,
                primaryKey: $0.primaryKey,
                columns: $0.columns + [
                    STPDResultColumnDefinition(
                        name: "unknown_column",
                        type: .string,
                        nullable: true
                    ),
                ],
                rowCount: rows.count,
                sha256: STPDStableIdentifier.digest(bytes)
            )
        }
        try saveManifest(updated, root)

        expectReaderError(root, {
            if case .manifestContractMismatch(let name, let field) = $0 {
                return name == table.rawValue && field == "ordered_columns"
            }
            return false
        }, "coordinated manifest and CSV unknown column")
    }
}

@Test func readerRejectsMissingRegeneratedConsistencyCheck() throws {
    let package = try readerLocalPackage()
    try withWrittenPackage(package) { root in
        let verified = try STPDResultPackageReader.read(packageAt: root)
        let missingID = try #require(verified.consistencyChecks.first?.id)
        var (headers, rows) = try loadTableRows(root, .resultConsistencyCheck)
        let checkIndex = try columnIndex(headers, "check_id")
        rows.removeAll { $0[checkIndex] == missingID }
        try rewriteTableAndResync(
            root,
            .resultConsistencyCheck,
            headers: headers,
            rows: rows
        )

        expectReaderError(root, {
            if case .consistencyLedgerInvalid(let reason) = $0 {
                return reason.contains("shape mismatch")
                    && reason.contains("missing=\(missingID)")
            }
            return false
        }, "missing regenerated package-only consistency check")
    }
}

@Test func readerRejectsNonCanonicalIntegerRealAndTimestampCells() throws {
    let local = try readerLocalPackage()
    try withWrittenPackage(local) { root in
        var (headers, rows) = try loadTableRows(root, .runMetadata)
        let index = try columnIndex(headers, "train_count")
        rows[0][index] = "0\(rows[0][index])"
        try rewriteTableAndResync(root, .runMetadata, headers: headers, rows: rows)
        expectReaderError(root, {
            if case .tableStructureInvalid(let table, _) = $0 {
                return table == STPDResultTable.runMetadata.rawValue
            }
            return false
        }, "non-canonical integer")
    }

    try withWrittenPackage(local) { root in
        var (headers, rows) = try loadTableRows(root, .dataQualityQC)
        let index = try columnIndex(headers, "artifact_fraction")
        rows[0][index] = "0.0"
        try rewriteTableAndResync(root, .dataQualityQC, headers: headers, rows: rows)
        expectReaderError(root, {
            if case .tableStructureInvalid(let table, _) = $0 {
                return table == STPDResultTable.dataQualityQC.rawValue
            }
            return false
        }, "non-canonical real")
    }

    let imported = try readerImportedPackage()
    try withWrittenPackage(imported) { root in
        var (headers, rows) = try loadTableRows(root, .manualAnnotations)
        let index = try columnIndex(headers, "created_at")
        let original = rows[0][index]
        try #require(original.hasSuffix("Z"))
        rows[0][index] = String(original.dropLast()) + ".000Z"
        try rewriteTableAndResync(
            root,
            .manualAnnotations,
            headers: headers,
            rows: rows
        )
        expectReaderError(root, {
            if case .tableStructureInvalid(let table, _) = $0 {
                return table == STPDResultTable.manualAnnotations.rawValue
            }
            return false
        }, "non-canonical timestamp")
    }
}

@Test func readerRejectsUnknownAndDuplicateManifestKeys() throws {
    let package = try readerLocalPackage()
    try withWrittenPackage(package) { root in
        let original = try String(
            contentsOf: manifestURL(root),
            encoding: .utf8
        )
        let unknown = "{\n  \"unknown_key\" : true," + String(original.dropFirst())
        try Data(unknown.utf8).write(to: manifestURL(root))
        expectReaderError(root, {
            if case .manifestNotCanonical = $0 { return true }
            return false
        }, "unknown manifest key")
    }

    try withWrittenPackage(package) { root in
        let original = try String(
            contentsOf: manifestURL(root),
            encoding: .utf8
        )
        let encodedRunID = String(
            decoding: try JSONEncoder().encode(package.manifest.runID),
            as: UTF8.self
        )
        let duplicate =
            "{\n  \"run_id\" : \(encodedRunID)," + String(original.dropFirst())
        try Data(duplicate.utf8).write(to: manifestURL(root))
        expectReaderError(root, {
            switch $0 {
            case .manifestNotCanonical, .manifestUndecodable:
                return true
            default:
                return false
            }
        }, "duplicate manifest key")
    }
}

@Test func readerRejectsSemanticallyEquivalentNonCanonicalCSVBytes() throws {
    let package = try readerLocalPackage()
    try withWrittenPackage(package) { root in
        let table = STPDResultTable.runMetadata
        let (headers, rows) = try loadTableRows(root, table)
        let canonical = String(
            decoding: try Data(contentsOf: tableURL(root, table)),
            as: UTF8.self
        )
        let prefix =
            headers.joined(separator: ",") + "\r\n" + rows[0][0] + ","
        try #require(canonical.hasPrefix(prefix))
        let replacement =
            headers.joined(separator: ",") + "\r\n\"\(rows[0][0])\","
        let nonCanonical =
            replacement + String(canonical.dropFirst(prefix.count))
        try writeRawAndResyncDigest(
            root,
            table,
            bytes: Data(nonCanonical.utf8)
        )

        expectReaderError(root, {
            if case .nonCanonicalCSV(let name) = $0 {
                return name == table.rawValue
            }
            return false
        }, "semantically equivalent quoted CSV field")
    }
}

@Test func readerEnforcesManifestTableTotalAndCSVComplexityLimits() throws {
    let package = try readerLocalPackage()

    try withWrittenPackage(package) { root in
        let manifestBytes = try Data(contentsOf: manifestURL(root)).count
        expectReaderError(
            root,
            limits: readerLimits(maximumManifestBytes: manifestBytes - 1),
            {
                if case .resourceLimitExceeded(let resource, let limit) = $0 {
                    return resource == "\(STPDResultSchema.manifestFileName) bytes" &&
                        limit == manifestBytes - 1
                }
                return false
            },
            "manifest byte limit"
        )
    }

    try withWrittenPackage(package) { root in
        expectReaderError(
            root,
            limits: readerLimits(maximumTableBytes: 1),
            {
                if case .resourceLimitExceeded(let resource, let limit) = $0 {
                    return resource.hasSuffix(".csv bytes") && limit == 1
                }
                return false
            },
            "table byte limit"
        )
    }

    try withWrittenPackage(package) { root in
        let manifestBytes = try Data(contentsOf: manifestURL(root)).count
        expectReaderError(
            root,
            limits: readerLimits(maximumTotalBytes: manifestBytes),
            {
                if case .resourceLimitExceeded(let resource, let limit) = $0 {
                    return resource == "total package bytes" &&
                        limit == manifestBytes
                }
                return false
            },
            "total package byte limit"
        )
    }

    for (limits, expectedResource, expectedLimit) in [
        (readerLimits(maximumRowsPerTable: 1), "rows per table", 1),
        (readerLimits(maximumColumnsPerTable: 1), "columns per table", 1),
        (readerLimits(maximumFieldUnicodeScalars: 1), "field Unicode scalars", 1),
        (readerLimits(maximumCellsPerTable: 1), "cells per table", 1),
    ] {
        try withWrittenPackage(package) { root in
            expectReaderError(
                root,
                limits: limits,
                {
                    if case .resourceLimitExceeded(let resource, let limit) = $0 {
                        return resource == expectedResource && limit == expectedLimit
                    }
                    return false
                },
                "\(expectedResource) limit"
            )
        }
    }
}

@Test func readerEnforcesAggregateDecodedBudgetsAcrossAllTables() throws {
    let package = try readerLocalPackage()
    let totalRows = package.tables.values.reduce(0) { partial, table in
        partial + 1 + table.rows.count
    }
    let totalCells = package.tables.values.reduce(0) { partial, table in
        partial + table.headers.count + table.rows.reduce(0) { $0 + $1.count }
    }
    let totalDecodedUTF8Bytes = package.tables.values.reduce(0) { partial, table in
        partial
            + table.headers.reduce(0) { $0 + $1.utf8.count }
            + table.rows.reduce(0) { rowTotal, row in
                rowTotal + row.reduce(0) { $0 + $1.utf8.count }
            }
    }

    // The exact package-wide boundary succeeds; one less in each dimension fails closed.
    try withWrittenPackage(package) { root in
        let result = try STPDResultPackageReader.read(
            packageAt: root,
            limits: readerLimits(
                maximumTotalDecodedRows: totalRows,
                maximumTotalDecodedCells: totalCells,
                maximumTotalDecodedUTF8Bytes: totalDecodedUTF8Bytes
            )
        )
        #expect(result.verified)
    }

    for (limits, expectedResource, expectedLimit) in [
        (
            readerLimits(maximumTotalDecodedRows: totalRows - 1),
            "total decoded rows",
            totalRows - 1
        ),
        (
            readerLimits(maximumTotalDecodedCells: totalCells - 1),
            "total decoded cells",
            totalCells - 1
        ),
        (
            readerLimits(maximumTotalDecodedUTF8Bytes: totalDecodedUTF8Bytes - 1),
            "total decoded UTF-8 bytes",
            totalDecodedUTF8Bytes - 1
        ),
    ] {
        try withWrittenPackage(package) { root in
            expectReaderError(
                root,
                limits: limits,
                {
                    if case .resourceLimitExceeded(let resource, let limit) = $0 {
                        return resource == expectedResource && limit == expectedLimit
                    }
                    return false
                },
                "\(expectedResource) aggregate limit"
            )
        }
    }
}

@Test func readerLimitsRejectNonpositiveValuesWithoutTrapping() {
    #expect(
        STPDResultPackageReadLimits(
            maximumManifestBytes: 0,
            maximumTableBytes: 1,
            maximumTotalBytes: 1,
            maximumRowsPerTable: 1,
            maximumColumnsPerTable: 1,
            maximumFieldUnicodeScalars: 1,
            maximumCellsPerTable: 1,
            maximumTotalDecodedRows: 1,
            maximumTotalDecodedCells: 1,
            maximumTotalDecodedUTF8Bytes: 1
        ) == nil
    )
    #expect(
        STPDResultPackageReadLimits(
            maximumManifestBytes: 1,
            maximumTableBytes: 1,
            maximumTotalBytes: 1,
            maximumRowsPerTable: 1,
            maximumColumnsPerTable: 1,
            maximumFieldUnicodeScalars: 1,
            maximumCellsPerTable: 1,
            maximumTotalDecodedRows: 1,
            maximumTotalDecodedCells: -1,
            maximumTotalDecodedUTF8Bytes: 1
        ) == nil
    )
    #expect(
        STPDResultPackageReadLimits(
            maximumManifestBytes: 1,
            maximumTableBytes: 1,
            maximumTotalBytes: 1,
            maximumRowsPerTable: 1,
            maximumColumnsPerTable: 1,
            maximumFieldUnicodeScalars: 1,
            maximumCellsPerTable: 1,
            maximumTotalDecodedRows: 1,
            maximumTotalDecodedCells: 1,
            maximumTotalDecodedUTF8Bytes: 0
        ) == nil
    )
}

@Test func readerRejectsUnexpectedDirectoryEntryWithoutEnumeratingTheRest() throws {
    let package = try readerLocalPackage()
    try withWrittenPackage(package) { root in
        let unexpected = root.appendingPathComponent("000-unexpected")
        try Data().write(to: unexpected)
        for index in 0..<2_000 {
            let noise = root.appendingPathComponent(
                "noise-\(String(format: "%04d", index))"
            )
            try Data().write(to: noise)
        }

        expectReaderError(root, {
            if case .unexpectedPackageEntry(let name) = $0 {
                return name == "<unexpected-package-entry>"
            }
            return false
        }, "unexpected directory entry is rejected incrementally")
    }
}

@Test func readerPinsOpenedRootDescriptorAcrossSymlinkRetarget() throws {
    let package = try readerLocalPackage()
    try withWrittenPackage(package) { root in
        let session = root.deletingLastPathComponent()
        let alias = session.appendingPathComponent("active.stpdresult")
        let replacement = session.appendingPathComponent(
            "replacement.stpdresult",
            isDirectory: true
        )
        try FileManager.default.createDirectory(
            at: replacement,
            withIntermediateDirectories: false
        )
        try FileManager.default.createSymbolicLink(
            at: alias,
            withDestinationURL: root
        )

        var retargeted = false
        let result = try STPDResultPackageReader.readForTesting(
            packageAt: alias
        ) { checkpoint in
            guard checkpoint == .rootOpened else { return }
            try FileManager.default.removeItem(at: alias)
            try FileManager.default.createSymbolicLink(
                at: alias,
                withDestinationURL: replacement
            )
            retargeted = true
        }

        #expect(retargeted)
        #expect(result.runID == package.manifest.runID)
        #expect(result.verified)
        #expect(
            try FileManager.default.destinationOfSymbolicLink(
                atPath: alias.path
            ) == replacement.path
        )
    }
}

@Test func readerRejectsEntryInsertedAfterInitialDirectoryValidation() throws {
    let package = try readerLocalPackage()
    try withWrittenPackage(package) { root in
        let lateEntry = root.appendingPathComponent("late-extra.csv")
        var inserted = false

        do {
            _ = try STPDResultPackageReader.readForTesting(
                packageAt: root
            ) { checkpoint in
                guard checkpoint == .beforeFinalDirectoryValidation else {
                    return
                }
                try Data("late mutation".utf8).write(to: lateEntry)
                inserted = true
            }
            Issue.record(
                "expected reader to reject an entry inserted during validation"
            )
        } catch let error as STPDResultPackageReaderError {
            if case .unexpectedPackageEntry(let name) = error {
                #expect(name == "<unexpected-package-entry>")
            } else {
                Issue.record(
                    "unexpected reader error for late directory mutation: \(error)"
                )
            }
        } catch {
            Issue.record(
                "expected STPDResultPackageReaderError for late directory mutation, got \(error)"
            )
        }

        #expect(inserted)
        #expect(FileManager.default.fileExists(atPath: lateEntry.path))
    }
}
