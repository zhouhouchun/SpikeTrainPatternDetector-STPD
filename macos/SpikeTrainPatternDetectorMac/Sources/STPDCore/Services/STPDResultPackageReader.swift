import Darwin
import Foundation

/// Explicit memory/complexity bounds for fail-closed result-package readback.
public struct STPDResultPackageReadLimits: Sendable, Hashable {
    public let maximumManifestBytes: Int
    public let maximumTableBytes: Int
    public let maximumTotalBytes: Int
    public let maximumRowsPerTable: Int
    public let maximumColumnsPerTable: Int
    public let maximumFieldUnicodeScalars: Int
    public let maximumCellsPerTable: Int
    public let maximumTotalDecodedRows: Int
    public let maximumTotalDecodedCells: Int
    public let maximumTotalDecodedUTF8Bytes: Int

    public static let standard = STPDResultPackageReadLimits(
        maximumManifestBytes: 16 * 1_024 * 1_024,
        maximumTableBytes: 64 * 1_024 * 1_024,
        maximumTotalBytes: 512 * 1_024 * 1_024,
        maximumRowsPerTable: 1_000_000,
        maximumColumnsPerTable: 512,
        maximumFieldUnicodeScalars: 1 * 1_024 * 1_024,
        maximumCellsPerTable: 5_000_000,
        maximumTotalDecodedRows: 1_000_000,
        maximumTotalDecodedCells: 5_000_000,
        maximumTotalDecodedUTF8Bytes: 256 * 1_024 * 1_024
    )!

    public init?(
        maximumManifestBytes: Int,
        maximumTableBytes: Int,
        maximumTotalBytes: Int,
        maximumRowsPerTable: Int,
        maximumColumnsPerTable: Int,
        maximumFieldUnicodeScalars: Int,
        maximumCellsPerTable: Int,
        maximumTotalDecodedRows: Int,
        maximumTotalDecodedCells: Int,
        maximumTotalDecodedUTF8Bytes: Int
    ) {
        guard maximumManifestBytes > 0,
              maximumTableBytes > 0,
              maximumTotalBytes > 0,
              maximumRowsPerTable > 0,
              maximumColumnsPerTable > 0,
              maximumFieldUnicodeScalars > 0,
              maximumCellsPerTable > 0,
              maximumTotalDecodedRows > 0,
              maximumTotalDecodedCells > 0,
              maximumTotalDecodedUTF8Bytes > 0 else {
            return nil
        }
        self.maximumManifestBytes = maximumManifestBytes
        self.maximumTableBytes = maximumTableBytes
        self.maximumTotalBytes = maximumTotalBytes
        self.maximumRowsPerTable = maximumRowsPerTable
        self.maximumColumnsPerTable = maximumColumnsPerTable
        self.maximumFieldUnicodeScalars = maximumFieldUnicodeScalars
        self.maximumCellsPerTable = maximumCellsPerTable
        self.maximumTotalDecodedRows = maximumTotalDecodedRows
        self.maximumTotalDecodedCells = maximumTotalDecodedCells
        self.maximumTotalDecodedUTF8Bytes = maximumTotalDecodedUTF8Bytes
    }
}

/// Reads and strictly verifies a previously-written on-disk `.stpdresult` package directory.
///
/// This is the read-back half of the result-package provenance loop. It NEVER modifies, normalizes,
/// repairs, regenerates, or reinterprets the package: every table file is opened read-only and its exact
/// bytes are hashed. The reader only ADDS the filesystem/manifest/byte-integrity layer; every structural,
/// canonical-value, referential-integrity, manual-authority, semantic-digest, and reverse-link invariant
/// is delegated to the SAME authorities the writer used — `STPDResultSchema` (the one table registry),
/// `STPDResultTableData` construction, and `STPDResultPackageValidator.validate` in its package-only mode.
///
/// Threat model: it detects accidental corruption, ordinary tampering, provenance loss, and API misuse.
/// It does not claim cryptographic protection against a determined forger who recomputes every dependent
/// digest inside the package (the manifest itself is not signed); such coordinated forgery is out of scope.
public enum STPDResultPackageReader {
    static let unexpectedEntryDiagnosticName = "<unexpected-package-entry>"

    enum Checkpoint: Hashable, Sendable {
        case rootOpened
        case beforeFinalDirectoryValidation
    }

    /// Reads the `.stpdresult` bundle at `url` and returns an immutable, fully-verified read result, or
    /// throws a typed `STPDResultPackageReaderError` identifying the failing file/table/field. The
    /// package on disk is never modified.
    @_disfavoredOverload
    public static func read(
        packageAt url: URL,
        limits: STPDResultPackageReadLimits = .standard
    ) throws -> STPDResultPackageReadResult {
        try read(
            packageAt: url,
            limits: limits,
            checkpoint: { _ in }
        )
    }

    /// Reads the detector-independent canonical complete-manual-review package. The filesystem,
    /// byte limits, symlink rejection, strict RFC 4180 parser, and canonical-byte checks are shared
    /// with detector package readback; no detector run is reconstructed or implied.
    public static func readCanonicalManualResult(
        packageAt url: URL,
        limits: STPDResultPackageReadLimits = .standard
    ) throws -> CanonicalManualResultPackageReadResult {
        let rootPath = url.path
        let dirFD = open(rootPath, O_RDONLY | O_DIRECTORY | O_CLOEXEC)
        guard dirFD >= 0 else {
            if errno == ENOTDIR {
                throw STPDResultPackageReaderError.packageRootNotADirectory(path: rootPath)
            }
            throw STPDResultPackageReaderError.packageRootUnreadable(path: rootPath)
        }
        defer { close(dirFD) }
        var totalBytes = 0
        let manifestBytes = try readRegularFile(
            dirFD: dirFD,
            name: CanonicalManualResultPackageBuilder.manifestFileName,
            maximumBytes: limits.maximumManifestBytes,
            maximumTotalBytes: limits.maximumTotalBytes,
            totalBytes: &totalBytes
        )
        let manifest: CanonicalManualResultPackageManifest
        do {
            manifest = try JSONDecoder().decode(
                CanonicalManualResultPackageManifest.self,
                from: manifestBytes
            )
        } catch {
            throw STPDResultPackageReaderError.manifestUndecodable(
                reason: shortDecodingReason(error)
            )
        }
        guard try manifest.encodedData() == manifestBytes else {
            throw STPDResultPackageReaderError.manifestNotCanonical
        }
        guard manifest.schemaContractID
                == CanonicalManualResultPackageBuilder.schemaContractID,
              manifest.schemaContractDigest
                == CanonicalManualResultPackageBuilder.schemaContractDigest,
              manifest.sourceMode
                == CanonicalManualResultPackageBuilder.sourceMode else {
            throw STPDResultPackageReaderError.unsupportedSchemaVersion(
                manifest.schemaContractID
            )
        }
        guard manifest.files.count == 1,
              let file = manifest.files.first,
              file.fileName
                == CanonicalManualResultPackageBuilder.isiLabelsFileName else {
            throw STPDResultPackageReaderError.tableSetMismatch(
                missing: [CanonicalManualResultPackageBuilder.isiLabelsFileName],
                unexpected: manifest.files.map(\.fileName)
            )
        }
        try requireLegalCanonicalFileName(file.fileName)
        let allowed = Set([
            CanonicalManualResultPackageBuilder.manifestFileName,
            CanonicalManualResultPackageBuilder.isiLabelsFileName,
        ])
        let entries = try directoryEntries(dirFD: dirFD, allowedEntries: allowed)
        guard entries == allowed else {
            throw STPDResultPackageReaderError.tableSetMismatch(
                missing: allowed.subtracting(entries).sorted(),
                unexpected: entries.subtracting(allowed).sorted()
            )
        }
        let bytes = try readRegularFile(
            dirFD: dirFD,
            name: file.fileName,
            maximumBytes: limits.maximumTableBytes,
            maximumTotalBytes: limits.maximumTotalBytes,
            totalBytes: &totalBytes
        )
        guard file.byteCount == bytes.count else {
            throw STPDResultPackageReaderError.manifestContractMismatch(
                table: file.fileName,
                field: "byte_count"
            )
        }
        guard isCanonicalSHA256Hex(file.sha256) else {
            throw STPDResultPackageReaderError.nonCanonicalDigest(table: file.fileName)
        }
        guard STPDStableIdentifier.digest(bytes) == file.sha256 else {
            throw STPDResultPackageReaderError.digestMismatch(table: file.fileName)
        }
        let parsed: ParsedCSV
        do {
            parsed = try parseStrictCSV(
                bytes,
                rowBudget: CSVResourceBudget(
                    maximum: includingHeader(limits.maximumRowsPerTable),
                    resource: "rows per table",
                    reportedLimit: limits.maximumRowsPerTable
                ),
                maximumColumns: limits.maximumColumnsPerTable,
                maximumFieldUnicodeScalars: limits.maximumFieldUnicodeScalars,
                cellBudget: CSVResourceBudget(
                    maximum: limits.maximumCellsPerTable,
                    resource: "cells per table",
                    reportedLimit: limits.maximumCellsPerTable
                ),
                decodedUTF8Budget: CSVResourceBudget(
                    maximum: limits.maximumTotalDecodedUTF8Bytes,
                    resource: "decoded UTF-8 bytes",
                    reportedLimit: limits.maximumTotalDecodedUTF8Bytes
                )
            )
        } catch let error as CSVParseFailure {
            if error.invalidUTF8 {
                throw STPDResultPackageReaderError.invalidUTF8(table: file.fileName)
            }
            throw STPDResultPackageReaderError.malformedCSV(
                table: file.fileName,
                reason: error.reason
            )
        }
        guard parsed.header
                == CanonicalManualResultPackageBuilder.isiLabelsHeaders else {
            throw STPDResultPackageReaderError.headerMismatch(table: file.fileName)
        }
        guard parsed.rows.count == file.rowCount else {
            throw STPDResultPackageReaderError.rowCountMismatch(
                table: file.fileName,
                declared: file.rowCount,
                actual: parsed.rows.count
            )
        }
        guard STPDRFC4180.data(
            headers: CanonicalManualResultPackageBuilder.isiLabelsHeaders,
            rows: parsed.rows
        ) == bytes else {
            throw STPDResultPackageReaderError.nonCanonicalCSV(table: file.fileName)
        }

        func invalid(_ reason: String) -> STPDResultPackageReaderError {
            .tableStructureInvalid(table: file.fileName, reason: reason)
        }
        func validLabel(
            _ rawValue: String,
            track: ManualAnnotationSemanticTrack
        ) -> Bool {
            guard !rawValue.isEmpty else { return true }
            guard let label = ManualAnnotationLabel(rawValue: rawValue) else {
                return false
            }
            return label.polarity == .positive && label.semanticTrack == track
        }
        var rows: [CanonicalManualResultPackageReadRow] = []
        rows.reserveCapacity(parsed.rows.count)
        var lastTrain = ""
        var lastIndex = 0
        var lastRight: Int64?
        var seen = Set<String>()
        for values in parsed.rows {
            guard values.count
                    == CanonicalManualResultPackageBuilder.isiLabelsHeaders.count else {
                throw invalid("row width does not match the manual ISI contract")
            }
            guard values[0] == manifest.canonicalSchemaContractID,
                  values[1] == manifest.canonicalSchemaContractDigest,
                  values[2] == manifest.canonicalDatasetDigest,
                  values[3] == manifest.confirmedImportRecordDigest,
                  values[4] == manifest.manualDecisionDigest,
                  values[13] == manifest.reviewer,
                  values[14] == manifest.confirmedAtUnixSeconds,
                  values[15] == "sealed_complete_manual_review" else {
                throw invalid("row identity or review evidence disagrees with manifest")
            }
            let trainID = values[5]
            guard !trainID.isEmpty,
                  let isiIndex = Int(values[6]), String(isiIndex) == values[6],
                  let left = Int64(values[7]), String(left) == values[7],
                  let right = Int64(values[8]), String(right) == values[8],
                  let interval = Int64(values[9]), String(interval) == values[9],
                  isiIndex > 0 else {
                throw invalid("row contains a non-canonical integer or blank train ID")
            }
            let (expectedInterval, overflow) = right.subtractingReportingOverflow(left)
            guard !overflow, interval == expectedInterval else {
                throw invalid("ISI interval disagrees with exact timestamp subtraction")
            }
            let state = values[10]
            let event = values[11]
            let other = values[12]
            guard validLabel(state, track: .state),
                  validLabel(event, track: .event),
                  validLabel(other, track: .other),
                  !(other.isEmpty == false && (!state.isEmpty || !event.isEmpty)),
                  !state.isEmpty || !event.isEmpty || !other.isEmpty else {
                throw invalid("row labels violate track or complete-review rules")
            }
            let key = "\(trainID)\u{1}\(isiIndex)"
            guard seen.insert(key).inserted else {
                throw invalid("duplicate train/ISI key")
            }
            if trainID == lastTrain {
                guard isiIndex == lastIndex + 1,
                      lastRight == left else {
                    throw invalid("train rows are not contiguous exact ISI geometry")
                }
            } else {
                guard lastTrain.isEmpty
                        || lastTrain.utf8.lexicographicallyPrecedes(trainID.utf8),
                      isiIndex == 1 else {
                    throw invalid("train order or first ISI index is not canonical")
                }
                lastTrain = trainID
            }
            lastIndex = isiIndex
            lastRight = right
            rows.append(CanonicalManualResultPackageReadRow(
                trainID: trainID,
                isiIndex: isiIndex,
                leftTimestampMicroseconds: left,
                rightTimestampMicroseconds: right,
                intervalMicroseconds: interval,
                statePattern: state,
                eventPattern: event,
                otherPattern: other
            ))
        }
        return CanonicalManualResultPackageReadResult(
            manifest: manifest,
            rows: rows
        )
    }

    /// Source-compatible entry point retained for callers compiled against the B3 reader API.
    ///
    /// Descriptor-relative I/O is intentionally authoritative; the supplied `FileManager` is not used
    /// to reopen or enumerate package entries.
    @available(*, deprecated, message: "Use read(packageAt:limits:) instead.")
    public static func read(
        packageAt url: URL,
        fileManager: FileManager
    ) throws -> STPDResultPackageReadResult {
        _ = fileManager
        return try read(packageAt: url, limits: .standard)
    }

    static func readForTesting(
        packageAt url: URL,
        limits: STPDResultPackageReadLimits = .standard,
        checkpoint: (Checkpoint) throws -> Void
    ) throws -> STPDResultPackageReadResult {
        try read(
            packageAt: url,
            limits: limits,
            checkpoint: checkpoint
        )
    }

    private static func read(
        packageAt url: URL,
        limits: STPDResultPackageReadLimits,
        checkpoint: (Checkpoint) throws -> Void
    ) throws -> STPDResultPackageReadResult {
        let rootPath = url.path

        // Open the root once and use that descriptor for both enumeration and entry reads. Following a
        // user-supplied symlink to the root is allowed, but later retargeting cannot split the authority.
        let dirFD = open(rootPath, O_RDONLY | O_DIRECTORY | O_CLOEXEC)
        guard dirFD >= 0 else {
            if errno == ENOTDIR {
                throw STPDResultPackageReaderError.packageRootNotADirectory(path: rootPath)
            }
            throw STPDResultPackageReaderError.packageRootUnreadable(path: rootPath)
        }
        defer { close(dirFD) }
        try checkpoint(.rootOpened)
        var totalBytes = 0

        // 2. Decode the manifest strictly (all declared fields required and correctly typed).
        let manifestBytes = try readRegularFile(
            dirFD: dirFD,
            name: STPDResultSchema.manifestFileName,
            maximumBytes: limits.maximumManifestBytes,
            maximumTotalBytes: limits.maximumTotalBytes,
            totalBytes: &totalBytes
        )
        let manifest: STPDResultManifest
        do {
            manifest = try JSONDecoder().decode(STPDResultManifest.self, from: manifestBytes)
        } catch {
            throw STPDResultPackageReaderError.manifestUndecodable(
                reason: shortDecodingReason(error)
            )
        }
        let canonicalManifestBytes: Data
        do {
            canonicalManifestBytes = try manifest.encodedData()
        } catch {
            throw STPDResultPackageReaderError.manifestUndecodable(
                reason: shortDecodingReason(error)
            )
        }
        guard canonicalManifestBytes == manifestBytes else {
            throw STPDResultPackageReaderError.manifestNotCanonical
        }

        // 3. Reject unsupported/unknown schema versions. This reader implements the current v4 contract;
        // the complete validator requires the full v4 table set, so earlier versions are not read here.
        guard manifest.schemaVersion == STPDResultSchema.version else {
            throw STPDResultPackageReaderError.unsupportedSchemaVersion(manifest.schemaVersion)
        }
        // 4. Resolve the required table set EXCLUSIVELY through the central schema registry.
        guard let requiredFileNames =
            STPDResultSchema.requiredTableFileNames(forSchemaVersion: manifest.schemaVersion) else {
            throw STPDResultPackageReaderError.unsupportedSchemaVersion(manifest.schemaVersion)
        }
        let tableByFileName = Dictionary(
            uniqueKeysWithValues: STPDResultTable.allCases.map { ($0.rawValue, $0) }
        )

        // 5-7. Validate the manifest's declared table set: legal bare filenames, no duplicates, and
        // exactly the required set (no missing, no unexpected).
        var manifestTableByName: [String: STPDResultManifestTable] = [:]
        for table in manifest.tables {
            try requireLegalCanonicalFileName(table.fileName)
            guard manifestTableByName[table.fileName] == nil else {
                throw STPDResultPackageReaderError.duplicateManifestTableName(table.fileName)
            }
            manifestTableByName[table.fileName] = table
        }
        let manifestNames = Set(manifestTableByName.keys)
        if manifestNames != requiredFileNames {
            throw STPDResultPackageReaderError.tableSetMismatch(
                missing: requiredFileNames.subtracting(manifestNames).sorted(),
                unexpected: manifestNames.subtracting(requiredFileNames).sorted()
            )
        }
        let expectedManifestTableOrder = STPDResultTable.allCases.map(\.rawValue)
        guard manifest.tables.map(\.fileName) == expectedManifestTableOrder else {
            throw STPDResultPackageReaderError.manifestContractMismatch(
                table: STPDResultSchema.manifestFileName,
                field: "table_order"
            )
        }

        // 6/8. The on-disk directory must contain EXACTLY the required tables plus the manifest — no
        // unexpected entries, no traversal/absolute/nested names.
        let allowedEntries = requiredFileNames.union([STPDResultSchema.manifestFileName])
        let entrySet = try directoryEntries(
            dirFD: dirFD,
            allowedEntries: allowedEntries
        )
        if entrySet != allowedEntries {
            throw STPDResultPackageReaderError.tableSetMismatch(
                missing: allowedEntries.subtracting(entrySet).sorted(),
                unexpected: entrySet.subtracting(allowedEntries).sorted()
            )
        }

        // 9-16. For each table, in a deterministic order: read exact bytes (regular file, no symlink),
        // verify the manifest SHA-256 and row count, require valid UTF-8, parse strict RFC 4180, verify
        // the header equals the manifest's declared column order, and reconstruct the in-memory table via
        // the SAME validating constructor the writer used (which enforces canonical cell values, column
        // order, and primary-key structure).
        var tables: [STPDResultTable: STPDResultTableData] = [:]
        var summaries: [STPDResultPackageReadResult.VerifiedTable] = []
        var totalDecodedRows = 0
        var totalDecodedCells = 0
        var totalDecodedUTF8Bytes = 0
        for fileName in requiredFileNames.sorted() {
            guard let table = tableByFileName[fileName],
                  let manifestTable = manifestTableByName[fileName] else {
                // Unreachable given the set-equality checks above, but fail closed rather than assume.
                throw STPDResultPackageReaderError.tableSetMismatch(
                    missing: [fileName], unexpected: []
                )
            }

            let bytes = try readRegularFile(
                dirFD: dirFD,
                name: fileName,
                maximumBytes: limits.maximumTableBytes,
                maximumTotalBytes: limits.maximumTotalBytes,
                totalBytes: &totalBytes
            )

            // The manifest's declared digest must itself be canonical lowercase 64-hex — a non-canonical
            // form (e.g. uppercase) is rejected, not accepted by lowercasing it before comparison.
            guard isCanonicalSHA256Hex(manifestTable.sha256) else {
                throw STPDResultPackageReaderError.nonCanonicalDigest(table: fileName)
            }
            let digest = STPDStableIdentifier.digest(bytes)
            guard digest == manifestTable.sha256 else {
                throw STPDResultPackageReaderError.digestMismatch(table: fileName)
            }

            let parsed: ParsedCSV
            do {
                let rowBudget = tighterBudget(
                    CSVResourceBudget(
                        maximum: includingHeader(limits.maximumRowsPerTable),
                        resource: "rows per table",
                        reportedLimit: limits.maximumRowsPerTable
                    ),
                    CSVResourceBudget(
                        maximum: limits.maximumTotalDecodedRows - totalDecodedRows,
                        resource: "total decoded rows",
                        reportedLimit: limits.maximumTotalDecodedRows
                    )
                )
                let cellBudget = tighterBudget(
                    CSVResourceBudget(
                        maximum: limits.maximumCellsPerTable,
                        resource: "cells per table",
                        reportedLimit: limits.maximumCellsPerTable
                    ),
                    CSVResourceBudget(
                        maximum: limits.maximumTotalDecodedCells - totalDecodedCells,
                        resource: "total decoded cells",
                        reportedLimit: limits.maximumTotalDecodedCells
                    )
                )
                let decodedUTF8Budget = CSVResourceBudget(
                    maximum:
                        limits.maximumTotalDecodedUTF8Bytes
                            - totalDecodedUTF8Bytes,
                    resource: "total decoded UTF-8 bytes",
                    reportedLimit: limits.maximumTotalDecodedUTF8Bytes
                )
                parsed = try parseStrictCSV(
                    bytes,
                    rowBudget: rowBudget,
                    maximumColumns: limits.maximumColumnsPerTable,
                    maximumFieldUnicodeScalars: limits.maximumFieldUnicodeScalars,
                    cellBudget: cellBudget,
                    decodedUTF8Budget: decodedUTF8Budget
                )
            } catch let error as CSVParseFailure {
                if error.invalidUTF8 {
                    throw STPDResultPackageReaderError.invalidUTF8(table: fileName)
                }
                if let resource = error.resource, let limit = error.limit {
                    throw STPDResultPackageReaderError.resourceLimitExceeded(
                        resource: resource,
                        limit: limit
                    )
                }
                throw STPDResultPackageReaderError.malformedCSV(
                    table: fileName, reason: error.reason
                )
            }
            guard let header = parsed.header else {
                throw STPDResultPackageReaderError.malformedCSV(
                    table: fileName, reason: "no header row"
                )
            }
            let dataRows = parsed.rows
            try addToAggregate(
                parsed.rowCountIncludingHeader,
                total: &totalDecodedRows,
                limit: limits.maximumTotalDecodedRows,
                resource: "total decoded rows"
            )
            try addToAggregate(
                parsed.cellCount,
                total: &totalDecodedCells,
                limit: limits.maximumTotalDecodedCells,
                resource: "total decoded cells"
            )
            try addToAggregate(
                parsed.decodedUTF8ByteCount,
                total: &totalDecodedUTF8Bytes,
                limit: limits.maximumTotalDecodedUTF8Bytes,
                resource: "total decoded UTF-8 bytes"
            )

            let contract = try tableContract(for: table)

            // Header and manifest definitions must independently match the authoritative registry.
            let declaredColumns = manifestTable.columns.map(\.name)
            guard header == declaredColumns else {
                throw STPDResultPackageReaderError.headerMismatch(table: fileName)
            }
            guard header == contract.orderedColumns else {
                throw STPDResultPackageReaderError.manifestContractMismatch(
                    table: fileName, field: "ordered_columns"
                )
            }
            let authoritativeDefinitions = STPDResultColumnCatalog.definitions(
                table: table,
                headers: contract.orderedColumns,
                nonNullable: Set(contract.requiredIdentityColumns + contract.primaryKey)
            )
            guard manifestTable.columns == authoritativeDefinitions else {
                throw STPDResultPackageReaderError.manifestContractMismatch(
                    table: fileName, field: "columns"
                )
            }
            // Declared row count must equal the parsed data-row count.
            guard dataRows.count == manifestTable.rowCount else {
                throw STPDResultPackageReaderError.rowCountMismatch(
                    table: fileName,
                    declared: manifestTable.rowCount,
                    actual: dataRows.count
                )
            }

            // Reconstruct via the writer's validating constructor: header/column-order exactness against
            // the schema-version column catalog, canonical cell values/units, and primary-key structure.
            let tableData: STPDResultTableData
            do {
                tableData = try STPDResultTableData(
                    contract: contract,
                    headers: header,
                    columnDefinitions: authoritativeDefinitions,
                    rows: dataRows
                )
            } catch {
                throw STPDResultPackageReaderError.tableStructureInvalid(
                    table: fileName, reason: shortErrorReason(error)
                )
            }

            // The manifest table definition must exactly match the single central schema authority: grain,
            // primary key, and the full ordered column set (name, type, nullable). `tableData`'s column
            // definitions were resolved from the schema column catalog, so this binds the manifest to the
            // schema authority without introducing a second registry.
            guard manifestTable.grain == contract.grain else {
                throw STPDResultPackageReaderError.manifestContractMismatch(table: fileName, field: "grain")
            }
            guard manifestTable.primaryKey == contract.primaryKey else {
                throw STPDResultPackageReaderError.manifestContractMismatch(table: fileName, field: "primary_key")
            }
            guard manifestTable.columns == tableData.columnDefinitions else {
                throw STPDResultPackageReaderError.manifestContractMismatch(table: fileName, field: "columns")
            }
            guard tableData.csvData == bytes else {
                throw STPDResultPackageReaderError.nonCanonicalCSV(table: fileName)
            }

            tables[table] = tableData
            summaries.append(
                .init(
                    fileName: fileName,
                    rowCount: dataRows.count,
                    sha256: digest,
                    columns: tableData.columnDefinitions,
                    rows: tableData.rows
                )
            )
        }

        // 17. Reconstruct the run identity from manifest authority fields plus materialized population
        // tables. Run_metadata is then independently checked against those populations by the shared
        // validator; it never self-authenticates its own counts.
        guard let sourceMode = STPDResultPackageSourceMode(rawValue: manifest.sourceMode) else {
            throw STPDResultPackageReaderError.invalidManifestField(
                field: "source_mode", value: manifest.sourceMode
            )
        }
        let identity = try reconstructIdentity(manifest: manifest, tables: tables)
        // Bind the top-level (unsigned) manifest provenance fields to their canonical authorities AND to
        // the byte-verified Run_metadata cells, so a lone manifest edit of an owner/build/encoding field
        // fails closed.
        try validateManifestProvenance(manifest: manifest, tables: tables)
        guard let isiTable = tables[.isiLabelsFinal] else {
            throw STPDResultPackageReaderError.tableSetMismatch(
                missing: [STPDResultTable.isiLabelsFinal.rawValue], unexpected: []
            )
        }

        // The `Result_consistency_check.csv` table records the writer's dataset-backed self-checks.
        // Package-only re-validation runs a dataset-free subset, so exclude the historical ledger from
        // validator input and independently require every regenerated package-only check to have an
        // exact matching ledger row below.
        let validationTables = tables.filter { $0.key != .resultConsistencyCheck }
        let checks: [STPDConsistencyCheck]
        do {
            checks = try STPDResultPackageValidator.validate(
                identity: identity,
                sourceMode: sourceMode,
                tables: validationTables,
                expectedISICount: isiTable.rowCount
            )
        } catch {
            throw STPDResultPackageReaderError.packageValidationFailed(
                reason: shortErrorReason(error)
            )
        }

        // Package-only validation cannot reproduce every dataset-backed check. It does regenerate the
        // checks available from package contents, and the ledger validator requires exact status, severity,
        // and detail agreement for each of those checks in addition to validating every historical row.
        try validateConsistencyLedger(
            tables[.resultConsistencyCheck],
            identity: identity,
            requiredChecks: checks
        )

        // Recheck the exact entry set through the same pinned directory descriptor immediately before
        // returning success. This closes the validation-window gap where an undeclared entry could be
        // inserted after the initial enumeration while all declared files were being verified.
        try checkpoint(.beforeFinalDirectoryValidation)
        let finalEntrySet = try directoryEntries(
            dirFD: dirFD,
            allowedEntries: allowedEntries
        )
        if finalEntrySet != allowedEntries {
            throw STPDResultPackageReaderError.tableSetMismatch(
                missing: allowedEntries.subtracting(finalEntrySet).sorted(),
                unexpected: finalEntrySet.subtracting(allowedEntries).sorted()
            )
        }

        return STPDResultPackageReadResult(
            schemaVersion: manifest.schemaVersion,
            detectorVersion: manifest.detectorVersion,
            runID: manifest.runID,
            datasetDigest: manifest.datasetDigest,
            settingsDigest: manifest.settingsDigest,
            sourceMode: manifest.sourceMode,
            ownerName: manifest.ownerName,
            ownerEmail: manifest.ownerEmail,
            tables: summaries.sorted { $0.fileName < $1.fileName },
            consistencyChecks: checks.map {
                .init(id: $0.id, status: $0.status, severity: $0.severity, details: $0.details)
            },
            verified: true
        )
    }

    // MARK: - Identity reconstruction

    private static func reconstructIdentity(
        manifest: STPDResultManifest,
        tables: [STPDResultTable: STPDResultTableData]
    ) throws -> DetectionRunIdentity {
        // Run_metadata must exist, but it is not the population authority: otherwise a coordinated edit
        // of its declared counts and digest would compare the table back to itself.
        guard let metadata = tables[.runMetadata], metadata.rowCount == 1 else {
            throw STPDResultPackageReaderError.invalidManifestField(
                field: STPDResultTable.runMetadata.rawValue, value: "expected exactly one metadata row"
            )
        }
        guard let quality = tables[.dataQualityQC] else {
            throw STPDResultPackageReaderError.invalidManifestField(
                field: STPDResultTable.dataQualityQC.rawValue,
                value: "missing materialized train population"
            )
        }
        var spikeCount = 0
        for row in 0..<quality.rowCount {
            guard let raw = quality.value(row: row, column: "spike_count"),
                  let value = Int(raw),
                  value >= 0 else {
                throw STPDResultPackageReaderError.invalidManifestField(
                    field: "spike_count",
                    value: quality.value(row: row, column: "spike_count") ?? "<missing>"
                )
            }
            let addition = spikeCount.addingReportingOverflow(value)
            guard !addition.overflow else {
                throw STPDResultPackageReaderError.invalidManifestField(
                    field: "spike_count",
                    value: "materialized population overflows Int"
                )
            }
            spikeCount = addition.partialValue
        }
        guard let taskEvents = tables[.taskEvents] else {
            throw STPDResultPackageReaderError.invalidManifestField(
                field: STPDResultTable.taskEvents.rawValue,
                value: "missing materialized task-event population"
            )
        }
        return DetectionRunIdentity(
            runID: manifest.runID,
            datasetDigest: manifest.datasetDigest,
            settingsDigest: manifest.settingsDigest,
            resultSchemaVersion: manifest.schemaVersion,
            detectorVersion: manifest.detectorVersion,
            buildCommit: manifest.buildIdentifier,
            trainCount: quality.rowCount,
            spikeCount: spikeCount,
            taskEventCount: taskEvents.rowCount,
            settingsSnapshot: nil
        )
    }

    /// The one schema contract for a table, sourced from the central `STPDResultSchema` registry (no
    /// second registry is introduced). Every table has a contract, so absence is a fail-closed invariant.
    private static func tableContract(for table: STPDResultTable) throws -> STPDResultTableContract {
        guard let contract = STPDResultSchema.tables.first(where: { $0.table == table }) else {
            throw STPDResultPackageReaderError.tableStructureInvalid(
                table: table.rawValue, reason: "no schema contract registered"
            )
        }
        return contract
    }

    // MARK: - Digest form, manifest provenance, and consistency ledger

    /// True iff `value` is exactly 64 lowercase hexadecimal characters (canonical SHA-256 hex form).
    private static func isCanonicalSHA256Hex(_ value: String) -> Bool {
        guard value.count == 64 else { return false }
        return value.unicodeScalars.allSatisfy { scalar in
            (scalar >= "0" && scalar <= "9") || (scalar >= "a" && scalar <= "f")
        }
    }

    private static func includingHeader(_ maximumDataRows: Int) -> Int {
        maximumDataRows == Int.max ? Int.max : maximumDataRows + 1
    }

    /// Binds the top-level (unsigned) manifest provenance fields to their canonical authorities AND to the
    /// byte-verified `Detector_run_metadata.csv` cells, so a lone manifest edit of one of these fields
    /// fails closed. `string_list_encoding` is a manifest-only field (no metadata column) bound to the
    /// canonical v4 encoding.
    private static func validateManifestProvenance(
        manifest: STPDResultManifest,
        tables: [STPDResultTable: STPDResultTableData]
    ) throws {
        guard let metadata = tables[.runMetadata], metadata.rowCount == 1 else {
            throw STPDResultPackageReaderError.invalidManifestField(
                field: STPDResultTable.runMetadata.rawValue, value: "expected exactly one metadata row"
            )
        }
        func cell(_ column: String) throws -> String {
            guard let value = metadata.value(row: 0, column: column) else {
                throw STPDResultPackageReaderError.invalidManifestField(field: column, value: "<missing>")
            }
            return value
        }
        func bind(_ field: String, _ manifestValue: String, canonical: String, metadataColumn: String) throws {
            guard manifestValue == canonical, try manifestValue == cell(metadataColumn) else {
                throw STPDResultPackageReaderError.invalidManifestField(field: field, value: manifestValue)
            }
        }
        try bind("owner_name", manifest.ownerName,
                 canonical: STPDResultPackageOwnership.ownerName, metadataColumn: "owner_name")
        try bind("owner_email", manifest.ownerEmail,
                 canonical: STPDResultPackageOwnership.ownerEmail, metadataColumn: "owner_email")
        try bind("build_identifier_kind", manifest.buildIdentifierKind,
                 canonical: STPDResultPackageOwnership.buildIdentifierKind, metadataColumn: "build_identifier_kind")
        // The manifest carries a Bool; the run-metadata cell carries its canonical string encoding.
        let canonicalAttested = STPDResultPackageOwnership.buildReproducibilityAttested
        guard manifest.buildReproducibilityAttested == canonicalAttested,
              try cell("build_reproducibility_attested") == STPDCanonicalValue.bool(canonicalAttested) else {
            throw STPDResultPackageReaderError.invalidManifestField(
                field: "build_reproducibility_attested",
                value: STPDCanonicalValue.bool(manifest.buildReproducibilityAttested)
            )
        }
        guard manifest.stringListEncoding == STPDResultPackageOwnership.stringListEncoding else {
            throw STPDResultPackageReaderError.invalidManifestField(
                field: "string_list_encoding", value: manifest.stringListEncoding
            )
        }
    }

    /// A package-only validator for `Result_consistency_check.csv`: every row must carry the reconstructed
    /// run identity, `check_id` values must be unique and non-empty, and the verdict must be exactly
    /// `pass`/`info`. It also requires the complete ledger to exactly match one recognized v4 shape and
    /// requires package-only regeneration to execute the complete package-only check subset.
    ///
    /// The v4 schema has two exact historical ledger shapes. The original B3 writer used the dataset-backed
    /// `isi_complete_coverage` and `task_event_projection` rows as the only counterparts of the package-only
    /// count checks. B3.1 added explicit `isi_declared_row_count` and `task_event_count` rows without changing
    /// the schema identifier. Readback therefore recognizes either complete shape, but rejects mixed,
    /// incomplete, extended, or textually altered ledgers.
    private static func validateConsistencyLedger(
        _ table: STPDResultTableData?,
        identity: DetectionRunIdentity,
        requiredChecks: [STPDConsistencyCheck]
    ) throws {
        guard let table else {
            throw STPDResultPackageReaderError.consistencyLedgerInvalid(reason: "missing consistency table")
        }
        // A header-only ledger is evidence that no writer self-checks ran: legitimate writer output always
        // records at least `required_table_set`. Reject it here so a zero-row loop cannot pass vacuously.
        guard table.rowCount > 0 else {
            throw STPDResultPackageReaderError.consistencyLedgerInvalid(reason: "consistency ledger is empty")
        }
        var seenCheckIDs = Set<String>()
        var ledgerByCheckID: [String: (status: String, severity: String, details: String)] = [:]
        for row in 0..<table.rowCount {
            func cell(_ column: String) throws -> String {
                guard let value = table.value(row: row, column: column) else {
                    throw STPDResultPackageReaderError.consistencyLedgerInvalid(reason: "missing \(column)")
                }
                return value
            }
            guard try cell("run_id") == identity.runID else {
                throw STPDResultPackageReaderError.consistencyLedgerInvalid(
                    reason: "row run_id does not match the run identity"
                )
            }
            guard try cell("settings_digest") == identity.settingsDigest else {
                throw STPDResultPackageReaderError.consistencyLedgerInvalid(
                    reason: "row settings_digest does not match the run identity"
                )
            }
            guard try cell("status") == "pass" else {
                throw STPDResultPackageReaderError.consistencyLedgerInvalid(reason: "row status is not \"pass\"")
            }
            guard try cell("severity") == "info" else {
                throw STPDResultPackageReaderError.consistencyLedgerInvalid(reason: "row severity is not \"info\"")
            }
            let checkID = try cell("check_id")
            guard !checkID.isEmpty else {
                throw STPDResultPackageReaderError.consistencyLedgerInvalid(reason: "empty check_id")
            }
            guard seenCheckIDs.insert(checkID).inserted else {
                throw STPDResultPackageReaderError.consistencyLedgerInvalid(reason: "duplicate check_id")
            }
            ledgerByCheckID[checkID] = (
                status: try cell("status"),
                severity: try cell("severity"),
                details: try cell("details")
            )
        }

        let regeneratedRows = requiredChecks.map {
            [$0.id, $0.status, $0.severity, $0.details]
        }
        let requiredPackageOnlyRows =
            STPDResultConsistencyLedgerContract.packageOnlyChecks.map {
                [$0.id, $0.status, $0.severity, $0.details]
            }
        guard regeneratedRows == requiredPackageOnlyRows else {
            throw STPDResultPackageReaderError.consistencyLedgerInvalid(
                reason: "package-only validator did not execute its exact required check contract"
            )
        }

        let currentShapeIDs =
            STPDResultConsistencyLedgerContract.currentOnlyCheckIDs
        let presentCurrentShapeIDs = currentShapeIDs.filter {
            ledgerByCheckID[$0] != nil
        }
        let shape: STPDResultConsistencyLedgerShape
        switch presentCurrentShapeIDs.count {
        case 0:
            shape = .baselineB3V4
        case currentShapeIDs.count:
            shape = .currentV4
        default:
            throw STPDResultPackageReaderError.consistencyLedgerInvalid(
                reason: "consistency ledger mixes baseline-B3 and current v4 package-only check shapes"
            )
        }

        let expectedChecks =
            STPDResultConsistencyLedgerContract.checks(for: shape)
        let expectedCheckIDs = Set(expectedChecks.map(\.id))
        let actualCheckIDs = Set(ledgerByCheckID.keys)
        guard actualCheckIDs == expectedCheckIDs else {
            let missing = expectedCheckIDs.subtracting(actualCheckIDs).sorted()
            let unexpected = actualCheckIDs.subtracting(expectedCheckIDs).sorted()
            throw STPDResultPackageReaderError.consistencyLedgerInvalid(
                reason:
                    "consistency ledger shape mismatch; missing=\(missing.joined(separator: "|")); " +
                    "unexpected=\(unexpected.joined(separator: "|"))"
            )
        }

        for expected in expectedChecks {
            guard let ledger = ledgerByCheckID[expected.id],
                  ledger.status == expected.status,
                  ledger.severity == expected.severity,
                  ledger.details == expected.details else {
                throw STPDResultPackageReaderError.consistencyLedgerInvalid(
                    reason: "consistency check \(expected.id) does not match its canonical ledger row"
                )
            }
        }
    }

    // MARK: - Filesystem safety

    /// Reads an entry's exact bytes relative to the package directory fd, rejecting symlinks and any
    /// non-regular file. Using `openat` with `O_NOFOLLOW` prevents symlink escape from the package root.
    private static func readRegularFile(
        dirFD: Int32,
        name: String,
        maximumBytes: Int,
        maximumTotalBytes: Int,
        totalBytes: inout Int
    ) throws -> Data {
        let fd = openat(
            dirFD,
            name,
            O_RDONLY | O_NONBLOCK | O_NOFOLLOW | O_CLOEXEC
        )
        if fd < 0 {
            let code = errno
            if code == ELOOP {
                throw STPDResultPackageReaderError.symlinkNotAllowed(name: name)
            }
            if code == ENOENT {
                throw STPDResultPackageReaderError.entryMissing(name: name)
            }
            throw STPDResultPackageReaderError.entryUnreadable(
                name: name, reason: String(cString: strerror(code))
            )
        }
        defer { close(fd) }
        var meta = stat()
        guard fstat(fd, &meta) == 0 else {
            throw STPDResultPackageReaderError.entryUnreadable(
                name: name, reason: String(cString: strerror(errno))
            )
        }
        guard (meta.st_mode & S_IFMT) == S_IFREG else {
            throw STPDResultPackageReaderError.entryNotARegularFile(name: name)
        }
        guard meta.st_size >= 0,
              meta.st_size <= off_t(maximumBytes) else {
            throw STPDResultPackageReaderError.resourceLimitExceeded(
                resource: "\(name) bytes", limit: maximumBytes
            )
        }
        guard totalBytes <= maximumTotalBytes - Int(meta.st_size) else {
            throw STPDResultPackageReaderError.resourceLimitExceeded(
                resource: "total package bytes", limit: maximumTotalBytes
            )
        }
        var data = Data()
        data.reserveCapacity(Int(meta.st_size))
        let bufferSize = 1 << 16
        var buffer = [UInt8](repeating: 0, count: bufferSize)
        while true {
            let count = buffer.withUnsafeMutableBytes { Darwin.read(fd, $0.baseAddress, bufferSize) }
            if count < 0 {
                if errno == EINTR {
                    continue
                }
                throw STPDResultPackageReaderError.entryUnreadable(
                    name: name, reason: String(cString: strerror(errno))
                )
            }
            if count == 0 { break }
            guard data.count <= maximumBytes - count,
                  totalBytes <= maximumTotalBytes - count else {
                throw STPDResultPackageReaderError.resourceLimitExceeded(
                    resource: "\(name) or total package bytes",
                    limit: min(maximumBytes, maximumTotalBytes)
                )
            }
            data.append(contentsOf: buffer[0..<count])
            totalBytes += count
        }
        return data
    }

    /// Enumerates the already-open package root. `fdopendir` owns and closes the duplicated descriptor,
    /// while the original descriptor remains authoritative for all `openat` reads.
    private static func directoryEntries(
        dirFD: Int32,
        allowedEntries: Set<String>
    ) throws -> Set<String> {
        let duplicate = fcntl(dirFD, F_DUPFD_CLOEXEC, 0)
        guard duplicate >= 0 else {
            throw STPDResultPackageReaderError.entryUnreadable(
                name: "<package-root>", reason: String(cString: strerror(errno))
            )
        }
        // Duplicated directory descriptors share the same open-file-description offset. Reset the
        // duplicate before every enumeration so the final TOCTOU recheck sees the complete pinned root.
        guard lseek(duplicate, 0, SEEK_SET) >= 0 else {
            let code = errno
            close(duplicate)
            throw STPDResultPackageReaderError.entryUnreadable(
                name: "<package-root>", reason: String(cString: strerror(code))
            )
        }
        guard let directory = fdopendir(duplicate) else {
            let code = errno
            close(duplicate)
            throw STPDResultPackageReaderError.entryUnreadable(
                name: "<package-root>", reason: String(cString: strerror(code))
            )
        }
        defer { closedir(directory) }
        var names: Set<String> = []
        errno = 0
        while let entry = readdir(directory) {
            var tuple = entry.pointee.d_name
            let name = withUnsafePointer(to: &tuple) { pointer in
                pointer.withMemoryRebound(to: CChar.self, capacity: Int(NAME_MAX) + 1) {
                    String(cString: $0)
                }
            }
            if name != ".", name != ".." {
                try requireLegalCanonicalFileName(name)
                guard allowedEntries.contains(name) else {
                    throw STPDResultPackageReaderError.unexpectedPackageEntry(
                        unexpectedEntryDiagnosticName
                    )
                }
                names.insert(name)
                guard names.count <= allowedEntries.count else {
                    throw STPDResultPackageReaderError.resourceLimitExceeded(
                        resource: "package directory entries",
                        limit: allowedEntries.count
                    )
                }
            }
            errno = 0
        }
        if errno != 0 {
            throw STPDResultPackageReaderError.entryUnreadable(
                name: "<package-root>", reason: String(cString: strerror(errno))
            )
        }
        return names
    }

    /// A package entry name must be a bare canonical filename: not empty, not "." / "..", no path
    /// separators (nested paths), and not absolute. This rejects traversal and absolute-path filenames.
    private static func requireLegalCanonicalFileName(_ name: String) throws {
        guard !name.isEmpty,
              name != ".",
              name != "..",
              !name.contains("/"),
              !name.hasPrefix("/"),
              !name.contains("\0") else {
            throw STPDResultPackageReaderError.illegalEntryName(name: name)
        }
    }

    // MARK: - Strict RFC 4180 parsing (the exact inverse of STPDRFC4180.data)

    private struct ParsedCSV {
        let header: [String]?
        let rows: [[String]]
        let rowCountIncludingHeader: Int
        let cellCount: Int
        let decodedUTF8ByteCount: Int

        var matrix: [[String]] {
            guard let header else { return rows }
            return [header] + rows
        }
    }

    private struct CSVResourceBudget {
        let maximum: Int
        let resource: String
        let reportedLimit: Int
    }

    private struct CSVParseFailure: Error {
        let reason: String
        let resource: String?
        let limit: Int?
        let invalidUTF8: Bool

        static func malformed(_ reason: String) -> CSVParseFailure {
            CSVParseFailure(
                reason: reason,
                resource: nil,
                limit: nil,
                invalidUTF8: false
            )
        }

        static var invalidUTF8Field: CSVParseFailure {
            CSVParseFailure(
                reason: "field is not valid UTF-8",
                resource: nil,
                limit: nil,
                invalidUTF8: true
            )
        }

        static func limit(resource: String, limit: Int) -> CSVParseFailure {
            CSVParseFailure(
                reason: "\(resource) exceeds the configured limit",
                resource: resource,
                limit: limit,
                invalidUTF8: false
            )
        }
    }

    /// Parses the repository's RFC 4180 dialect exactly as produced by `STPDRFC4180.data`:
    /// comma-separated fields, records terminated by CRLF (with a trailing CRLF), and quoted fields
    /// (`"..."`) that may contain commas, CR, and LF, using `""` for a literal quote. Non-ASCII UTF-8
    /// content inside fields is preserved verbatim. Any structural violation throws — nothing is repaired.
    static func parseStrictCSV(_ text: String) throws -> [[String]] {
        try parseStrictCSV(
            Data(text.utf8),
            rowBudget: CSVResourceBudget(
                maximum: includingHeader(
                    STPDResultPackageReadLimits.standard.maximumRowsPerTable
                ),
                resource: "rows per table",
                reportedLimit:
                    STPDResultPackageReadLimits.standard.maximumRowsPerTable
            ),
            maximumColumns: STPDResultPackageReadLimits.standard.maximumColumnsPerTable,
            maximumFieldUnicodeScalars:
                STPDResultPackageReadLimits.standard.maximumFieldUnicodeScalars,
            cellBudget: CSVResourceBudget(
                maximum: STPDResultPackageReadLimits.standard.maximumCellsPerTable,
                resource: "cells per table",
                reportedLimit:
                    STPDResultPackageReadLimits.standard.maximumCellsPerTable
            ),
            decodedUTF8Budget: CSVResourceBudget(
                maximum: Int.max,
                resource: "decoded UTF-8 bytes",
                reportedLimit: Int.max
            )
        ).matrix
    }

    private static func parseStrictCSV(
        _ bytes: Data,
        rowBudget: CSVResourceBudget,
        maximumColumns: Int,
        maximumFieldUnicodeScalars: Int,
        cellBudget: CSVResourceBudget,
        decodedUTF8Budget: CSVResourceBudget
    ) throws -> ParsedCSV {
        let maximumFieldUTF8Bytes =
            maximumFieldUnicodeScalars > Int.max / 4
                ? Int.max
                : maximumFieldUnicodeScalars * 4

        return try bytes.withUnsafeBytes { rawBytes in
            let input = rawBytes.bindMemory(to: UInt8.self)
            var header: [String]?
            var rows: [[String]] = []
            var currentRow: [String] = []
            var fieldBytes: [UInt8] = []
            var totalCells = 0
            var totalRows = 0
            var decodedUTF8ByteCount = 0
            var index = 0
            let comma: UInt8 = 0x2C
            let quote: UInt8 = 0x22
            let cr: UInt8 = 0x0D
            let lf: UInt8 = 0x0A

            func appendFieldByte(_ byte: UInt8) throws {
                guard fieldBytes.count < maximumFieldUTF8Bytes else {
                    throw CSVParseFailure.limit(
                        resource: "field Unicode scalars",
                        limit: maximumFieldUnicodeScalars
                    )
                }
                guard decodedUTF8ByteCount <= decodedUTF8Budget.maximum,
                      fieldBytes.count
                        < decodedUTF8Budget.maximum - decodedUTF8ByteCount else {
                    throw CSVParseFailure.limit(
                        resource: decodedUTF8Budget.resource,
                        limit: decodedUTF8Budget.reportedLimit
                    )
                }
                fieldBytes.append(byte)
            }

            func endField() throws {
                guard currentRow.count < maximumColumns else {
                    throw CSVParseFailure.limit(
                        resource: "columns per table",
                        limit: maximumColumns
                    )
                }
                guard totalCells < cellBudget.maximum else {
                    throw CSVParseFailure.limit(
                        resource: cellBudget.resource,
                        limit: cellBudget.reportedLimit
                    )
                }
                guard let field = String(bytes: fieldBytes, encoding: .utf8) else {
                    throw CSVParseFailure.invalidUTF8Field
                }
                guard field.unicodeScalars.count <= maximumFieldUnicodeScalars else {
                    throw CSVParseFailure.limit(
                        resource: "field Unicode scalars",
                        limit: maximumFieldUnicodeScalars
                    )
                }
                currentRow.append(field)
                totalCells += 1
                decodedUTF8ByteCount += fieldBytes.count
                fieldBytes.removeAll(keepingCapacity: true)
            }

            func endRecord() throws {
                guard totalRows < rowBudget.maximum else {
                    throw CSVParseFailure.limit(
                        resource: rowBudget.resource,
                        limit: rowBudget.reportedLimit
                    )
                }
                try endField()
                if header == nil {
                    header = currentRow
                } else {
                    rows.append(currentRow)
                }
                totalRows += 1
                currentRow = []
            }

            while index < input.count {
                let byte = input[index]
                if byte == quote {
                    guard fieldBytes.isEmpty else {
                        throw CSVParseFailure.malformed(
                            "unexpected quote inside an unquoted field"
                        )
                    }
                    index += 1
                    while true {
                        guard index < input.count else {
                            throw CSVParseFailure.malformed("unterminated quoted field")
                        }
                        let inner = input[index]
                        if inner == quote {
                            if index + 1 < input.count, input[index + 1] == quote {
                                try appendFieldByte(quote)
                                index += 2
                                continue
                            }
                            index += 1
                            break
                        }
                        try appendFieldByte(inner)
                        index += 1
                    }
                    guard index < input.count else {
                        throw CSVParseFailure.malformed(
                            "quoted field not terminated by CRLF"
                        )
                    }
                    let next = input[index]
                    if next == comma {
                        try endField()
                        index += 1
                    } else if next == cr {
                        guard index + 1 < input.count, input[index + 1] == lf else {
                            throw CSVParseFailure.malformed(
                                "record terminator must be CRLF"
                            )
                        }
                        try endRecord()
                        index += 2
                    } else {
                        throw CSVParseFailure.malformed(
                            "unexpected character after closing quote"
                        )
                    }
                } else if byte == comma {
                    try endField()
                    index += 1
                } else if byte == cr {
                    guard index + 1 < input.count, input[index + 1] == lf else {
                        throw CSVParseFailure.malformed(
                            "record terminator must be CRLF"
                        )
                    }
                    try endRecord()
                    index += 2
                } else if byte == lf {
                    throw CSVParseFailure.malformed(
                        "bare LF is not a legal record terminator"
                    )
                } else {
                    try appendFieldByte(byte)
                    index += 1
                }
            }
            guard fieldBytes.isEmpty, currentRow.isEmpty else {
                throw CSVParseFailure.malformed(
                    "file does not end with a CRLF-terminated record"
                )
            }
            return ParsedCSV(
                header: header,
                rows: rows,
                rowCountIncludingHeader: totalRows,
                cellCount: totalCells,
                decodedUTF8ByteCount: decodedUTF8ByteCount
            )
        }
    }

    private static func tighterBudget(
        _ first: CSVResourceBudget,
        _ second: CSVResourceBudget
    ) -> CSVResourceBudget {
        second.maximum <= first.maximum ? second : first
    }

    private static func addToAggregate(
        _ value: Int,
        total: inout Int,
        limit: Int,
        resource: String
    ) throws {
        guard value <= limit - total else {
            throw STPDResultPackageReaderError.resourceLimitExceeded(
                resource: resource,
                limit: limit
            )
        }
        total += value
    }

    // MARK: - Error context helpers (never leak arbitrary file contents)

    private static func shortDecodingReason(_ error: Error) -> String {
        if let decoding = error as? DecodingError {
            switch decoding {
            case .keyNotFound(let key, _): return "missing key '\(key.stringValue)'"
            case .typeMismatch(_, let context): return "type mismatch at \(pathString(context.codingPath))"
            case .valueNotFound(_, let context): return "missing value at \(pathString(context.codingPath))"
            case .dataCorrupted(let context):
                return context.codingPath.isEmpty ? "corrupted JSON" : "corrupted value at \(pathString(context.codingPath))"
            @unknown default: return "undecodable manifest"
            }
        }
        return "undecodable manifest"
    }

    private static func pathString(_ path: [CodingKey]) -> String {
        path.map(\.stringValue).joined(separator: ".")
    }

    private static func shortErrorReason(_ error: Error) -> String {
        if let packageError = error as? STPDResultPackageError {
            return packageError.errorDescription ?? "package invariant violated"
        }
        return "package invariant violated"
    }
}

/// An immutable, deterministic summary of a successfully verified on-disk result package.
public struct STPDResultPackageReadResult: Sendable, Hashable {
    public let schemaVersion: String
    public let detectorVersion: String
    public let runID: String
    public let datasetDigest: String
    public let settingsDigest: String
    public let sourceMode: String
    public let ownerName: String
    public let ownerEmail: String
    /// One immutable normalized snapshot per verified table, sorted by file name. `sha256` is the
    /// recomputed exact-byte digest (which equalled the manifest's declared digest).
    public let tables: [VerifiedTable]
    /// The consistency checks returned by the reused complete package validator.
    public let consistencyChecks: [ConsistencyCheck]
    /// Always `true` for a returned result: the reader fails closed (throws) on any verification failure.
    public let verified: Bool

    public struct VerifiedTable: Sendable, Hashable {
        public let fileName: String
        public let rowCount: Int
        public let sha256: String
        public let columns: [STPDResultColumnDefinition]
        public let rows: [[String]]

        public func value(row: Int, column: String) -> String? {
            guard rows.indices.contains(row),
                  let index = columns.firstIndex(where: { $0.name == column }),
                  rows[row].indices.contains(index) else {
                return nil
            }
            return rows[row][index]
        }
    }

    public struct ConsistencyCheck: Sendable, Hashable {
        public let id: String
        public let status: String
        public let severity: String
        public let details: String
    }

    public func table(_ table: STPDResultTable) -> VerifiedTable? {
        tables.first { $0.fileName == table.rawValue }
    }
}

/// Typed, deterministic reader errors. Each identifies the failing file/table/field with enough context
/// to diagnose the failure, without echoing arbitrary package cell contents.
public enum STPDResultPackageReaderError: Error, LocalizedError, Hashable {
    case packageRootUnreadable(path: String)
    case packageRootNotADirectory(path: String)
    case manifestUndecodable(reason: String)
    case manifestNotCanonical
    case unsupportedSchemaVersion(String)
    case tableSetMismatch(missing: [String], unexpected: [String])
    case duplicateManifestTableName(String)
    case unexpectedPackageEntry(String)
    case illegalEntryName(name: String)
    case symlinkNotAllowed(name: String)
    case entryMissing(name: String)
    case entryUnreadable(name: String, reason: String)
    case entryNotARegularFile(name: String)
    case resourceLimitExceeded(resource: String, limit: Int)
    case nonCanonicalDigest(table: String)
    case digestMismatch(table: String)
    case nonCanonicalCSV(table: String)
    case manifestContractMismatch(table: String, field: String)
    case consistencyLedgerInvalid(reason: String)
    case rowCountMismatch(table: String, declared: Int, actual: Int)
    case invalidUTF8(table: String)
    case malformedCSV(table: String, reason: String)
    case headerMismatch(table: String)
    case tableStructureInvalid(table: String, reason: String)
    case invalidManifestField(field: String, value: String)
    case packageValidationFailed(reason: String)

    public var errorDescription: String? {
        switch self {
        case .packageRootUnreadable(let path):
            return "Result package root cannot be read: \(path)."
        case .packageRootNotADirectory(let path):
            return "Result package root is not a directory: \(path)."
        case .manifestUndecodable(let reason):
            return "Result package manifest.json could not be decoded: \(reason)."
        case .manifestNotCanonical:
            return "Result package manifest.json is not the canonical writer encoding."
        case .unsupportedSchemaVersion(let version):
            return "Result package schema version is unsupported by this reader: \(version)."
        case .tableSetMismatch(let missing, let unexpected):
            return "Result package table set mismatch; missing=[\(missing.joined(separator: ", "))]; "
                + "unexpected=[\(unexpected.joined(separator: ", "))]."
        case .duplicateManifestTableName(let name):
            return "Result package manifest declares a duplicate table name: \(name)."
        case .unexpectedPackageEntry(let name):
            return "Result package directory contains an unexpected entry: \(name)."
        case .illegalEntryName(let name):
            return "Result package contains an illegal (absolute/traversal/nested) entry name: \(name)."
        case .symlinkNotAllowed(let name):
            return "Result package entry is a symlink, which is not allowed: \(name)."
        case .entryMissing(let name):
            return "Result package is missing a required entry: \(name)."
        case .entryUnreadable(let name, let reason):
            return "Result package entry \(name) is unreadable: \(reason)."
        case .entryNotARegularFile(let name):
            return "Result package entry is not a regular file: \(name)."
        case .resourceLimitExceeded(let resource, let limit):
            return "Result package exceeds the configured \(resource) limit of \(limit)."
        case .nonCanonicalDigest(let table):
            return "Result package manifest declares a non-canonical (non-lowercase-64-hex) SHA-256 for table \(table)."
        case .digestMismatch(let table):
            return "Result package table \(table) does not match its declared SHA-256 digest."
        case .nonCanonicalCSV(let table):
            return "Result package table \(table) is semantically valid but not encoded in canonical writer CSV bytes."
        case .manifestContractMismatch(let table, let field):
            return "Result package manifest table \(table) \(field) does not match the schema contract."
        case .consistencyLedgerInvalid(let reason):
            return "Result package consistency ledger is invalid: \(reason)."
        case .rowCountMismatch(let table, let declared, let actual):
            return "Result package table \(table) declared \(declared) rows but contains \(actual)."
        case .invalidUTF8(let table):
            return "Result package table \(table) is not valid UTF-8."
        case .malformedCSV(let table, let reason):
            return "Result package table \(table) is not well-formed CSV: \(reason)."
        case .headerMismatch(let table):
            return "Result package table \(table) header does not match the declared column order."
        case .tableStructureInvalid(let table, let reason):
            return "Result package table \(table) failed structural validation: \(reason)."
        case .invalidManifestField(let field, let value):
            return "Result package manifest field \(field) is invalid: \(value)."
        case .packageValidationFailed(let reason):
            return "Result package failed complete validation: \(reason)."
        }
    }
}
