import Darwin
import Foundation

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
    /// Reads the `.stpdresult` bundle at `url` and returns an immutable, fully-verified read result, or
    /// throws a typed `STPDResultPackageReaderError` identifying the failing file/table/field. The
    /// package on disk is never modified.
    public static func read(
        packageAt url: URL,
        fileManager: FileManager = .default
    ) throws -> STPDResultPackageReadResult {
        let rootPath = url.path

        // 1. The package root must be a real directory. Following a user-supplied symlink to the root is
        // acceptable; symlink escape is prevented per-entry below via openat(..., O_NOFOLLOW).
        var rootStat = stat()
        guard stat(rootPath, &rootStat) == 0 else {
            throw STPDResultPackageReaderError.packageRootUnreadable(path: rootPath)
        }
        guard (rootStat.st_mode & S_IFMT) == S_IFDIR else {
            throw STPDResultPackageReaderError.packageRootNotADirectory(path: rootPath)
        }
        let dirFD = open(rootPath, O_RDONLY | O_DIRECTORY)
        guard dirFD >= 0 else {
            throw STPDResultPackageReaderError.packageRootUnreadable(path: rootPath)
        }
        defer { close(dirFD) }

        // 2. Decode the manifest strictly (all declared fields required and correctly typed).
        let manifestBytes = try readRegularFile(dirFD: dirFD, name: STPDResultSchema.manifestFileName)
        let manifest: STPDResultManifest
        do {
            manifest = try JSONDecoder().decode(STPDResultManifest.self, from: manifestBytes)
        } catch {
            throw STPDResultPackageReaderError.manifestUndecodable(
                reason: shortDecodingReason(error)
            )
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

        // 6/8. The on-disk directory must contain EXACTLY the required tables plus the manifest — no
        // unexpected entries, no traversal/absolute/nested names.
        let entries = try fileManager.contentsOfDirectory(atPath: rootPath)
        let allowedEntries = requiredFileNames.union([STPDResultSchema.manifestFileName])
        for entry in entries {
            try requireLegalCanonicalFileName(entry)
            guard allowedEntries.contains(entry) else {
                throw STPDResultPackageReaderError.unexpectedPackageEntry(entry)
            }
        }
        let entrySet = Set(entries)
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
        for fileName in requiredFileNames.sorted() {
            guard let table = tableByFileName[fileName],
                  let manifestTable = manifestTableByName[fileName] else {
                // Unreachable given the set-equality checks above, but fail closed rather than assume.
                throw STPDResultPackageReaderError.tableSetMismatch(
                    missing: [fileName], unexpected: []
                )
            }

            let bytes = try readRegularFile(dirFD: dirFD, name: fileName)

            // The manifest's declared digest must itself be canonical lowercase 64-hex — a non-canonical
            // form (e.g. uppercase) is rejected, not accepted by lowercasing it before comparison.
            guard isCanonicalSHA256Hex(manifestTable.sha256) else {
                throw STPDResultPackageReaderError.nonCanonicalDigest(table: fileName)
            }
            let digest = STPDStableIdentifier.digest(bytes)
            guard digest == manifestTable.sha256 else {
                throw STPDResultPackageReaderError.digestMismatch(table: fileName)
            }

            guard let text = String(data: bytes, encoding: .utf8) else {
                throw STPDResultPackageReaderError.invalidUTF8(table: fileName)
            }

            let parsed: [[String]]
            do {
                parsed = try parseStrictCSV(text)
            } catch let error as CSVParseFailure {
                throw STPDResultPackageReaderError.malformedCSV(
                    table: fileName, reason: error.reason
                )
            }
            guard let header = parsed.first else {
                throw STPDResultPackageReaderError.malformedCSV(
                    table: fileName, reason: "no header row"
                )
            }
            let dataRows = Array(parsed.dropFirst())

            // Header must exactly match the manifest's declared column names and order.
            let declaredColumns = manifestTable.columns.map(\.name)
            guard header == declaredColumns else {
                throw STPDResultPackageReaderError.headerMismatch(table: fileName)
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
            let contract = try tableContract(for: table)
            let tableData: STPDResultTableData
            do {
                tableData = try STPDResultTableData(
                    contract: contract,
                    headers: header,
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

            tables[table] = tableData
            summaries.append(
                .init(fileName: fileName, rowCount: dataRows.count, sha256: digest)
            )
        }

        // 17. Reconstruct the run identity from the manifest (authority fields) plus the byte-integrity-
        // verified run-metadata counts, then invoke the COMPLETE package validator in package-only mode.
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

        // The `Result_consistency_check.csv` table records the self-checks the WRITER's dataset-backed
        // validation executed at build time. Package-only re-validation runs a different (dataset-free)
        // subset, so it cannot reproduce that build-time audit record and must not cross-check it against
        // its own reduced check list. The reader has already byte- and structure-verified that table
        // (digest, header, row count, canonical cells) above; here it is excluded from re-validation.
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

        // Package-only validation cannot reproduce the dataset-backed check list, but the consistency
        // ledger is not semantically ignored: this narrow validator requires every row to carry the
        // reconstructed run identity, unique check ids, and a "pass"/"info" verdict, failing closed on any
        // malformed or contradictory row. It does not fabricate or regenerate dataset-backed check details.
        try validateConsistencyLedger(tables[.resultConsistencyCheck], identity: identity)

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
        // Counts are not carried in the manifest; read them from the run-metadata table whose exact bytes
        // were already verified against the manifest SHA-256, so they cannot have been tampered undetected.
        guard let metadata = tables[.runMetadata], metadata.rowCount == 1 else {
            throw STPDResultPackageReaderError.invalidManifestField(
                field: STPDResultTable.runMetadata.rawValue, value: "expected exactly one metadata row"
            )
        }
        func intColumn(_ column: String) throws -> Int {
            guard let raw = metadata.value(row: 0, column: column), let value = Int(raw) else {
                throw STPDResultPackageReaderError.invalidManifestField(
                    field: column, value: metadata.value(row: 0, column: column) ?? "<missing>"
                )
            }
            return value
        }
        return DetectionRunIdentity(
            runID: manifest.runID,
            datasetDigest: manifest.datasetDigest,
            settingsDigest: manifest.settingsDigest,
            resultSchemaVersion: manifest.schemaVersion,
            detectorVersion: manifest.detectorVersion,
            buildCommit: manifest.buildIdentifier,
            trainCount: try intColumn("train_count"),
            spikeCount: try intColumn("spike_count"),
            taskEventCount: try intColumn("task_event_count"),
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

    /// A narrow package-only validator for `Result_consistency_check.csv`: every row must carry the
    /// reconstructed run identity, `check_id` values must be unique and non-empty, and the verdict must be
    /// exactly `pass`/`info`. It fails closed on any malformed or contradictory row and does NOT reproduce
    /// or fabricate the dataset-backed check details.
    private static func validateConsistencyLedger(
        _ table: STPDResultTableData?,
        identity: DetectionRunIdentity
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
        }
    }

    // MARK: - Filesystem safety

    /// Reads an entry's exact bytes relative to the package directory fd, rejecting symlinks and any
    /// non-regular file. Using `openat` with `O_NOFOLLOW` prevents symlink escape from the package root.
    private static func readRegularFile(dirFD: Int32, name: String) throws -> Data {
        let fd = openat(dirFD, name, O_RDONLY | O_NOFOLLOW)
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
        var data = Data()
        let bufferSize = 1 << 16
        var buffer = [UInt8](repeating: 0, count: bufferSize)
        while true {
            let count = buffer.withUnsafeMutableBytes { Darwin.read(fd, $0.baseAddress, bufferSize) }
            if count < 0 {
                throw STPDResultPackageReaderError.entryUnreadable(
                    name: name, reason: String(cString: strerror(errno))
                )
            }
            if count == 0 { break }
            data.append(contentsOf: buffer[0..<count])
        }
        return data
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

    private struct CSVParseFailure: Error { let reason: String }

    /// Parses the repository's RFC 4180 dialect exactly as produced by `STPDRFC4180.data`:
    /// comma-separated fields, records terminated by CRLF (with a trailing CRLF), and quoted fields
    /// (`"..."`) that may contain commas, CR, and LF, using `""` for a literal quote. Non-ASCII UTF-8
    /// content inside fields is preserved verbatim. Any structural violation throws — nothing is repaired.
    static func parseStrictCSV(_ text: String) throws -> [[String]] {
        let scalars = Array(text.unicodeScalars)
        var rows: [[String]] = []
        var currentRow: [String] = []
        var field = String.UnicodeScalarView()
        var index = 0
        let comma: UnicodeScalar = ","
        let quote: UnicodeScalar = "\""
        let cr: UnicodeScalar = "\r"
        let lf: UnicodeScalar = "\n"

        func endField() { currentRow.append(String(field)); field = String.UnicodeScalarView() }
        func endRecord() { endField(); rows.append(currentRow); currentRow = [] }

        while index < scalars.count {
            let scalar = scalars[index]
            if scalar == quote {
                // A quote may open a field only at the field's start.
                guard field.isEmpty else {
                    throw CSVParseFailure(reason: "unexpected quote inside an unquoted field")
                }
                index += 1
                // Consume the quoted body.
                while true {
                    guard index < scalars.count else {
                        throw CSVParseFailure(reason: "unterminated quoted field")
                    }
                    let inner = scalars[index]
                    if inner == quote {
                        if index + 1 < scalars.count, scalars[index + 1] == quote {
                            field.append(quote)
                            index += 2
                            continue
                        }
                        // Closing quote: must be followed by a comma or CRLF (or end of input).
                        index += 1
                        break
                    }
                    field.append(inner)
                    index += 1
                }
                // After a closing quote only a delimiter or record terminator is legal.
                if index >= scalars.count {
                    throw CSVParseFailure(reason: "quoted field not terminated by CRLF")
                }
                let next = scalars[index]
                if next == comma {
                    endField(); index += 1
                } else if next == cr {
                    guard index + 1 < scalars.count, scalars[index + 1] == lf else {
                        throw CSVParseFailure(reason: "record terminator must be CRLF")
                    }
                    endRecord(); index += 2
                } else {
                    throw CSVParseFailure(reason: "unexpected character after closing quote")
                }
            } else if scalar == comma {
                endField(); index += 1
            } else if scalar == cr {
                guard index + 1 < scalars.count, scalars[index + 1] == lf else {
                    throw CSVParseFailure(reason: "record terminator must be CRLF")
                }
                endRecord(); index += 2
            } else if scalar == lf {
                throw CSVParseFailure(reason: "bare LF is not a legal record terminator")
            } else {
                field.append(scalar)
                index += 1
            }
        }
        // A well-formed file ends exactly on a record boundary (trailing CRLF), leaving no dangling field.
        guard field.isEmpty, currentRow.isEmpty else {
            throw CSVParseFailure(reason: "file does not end with a CRLF-terminated record")
        }
        return rows
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
    /// One entry per verified table, sorted by file name. `sha256` is the recomputed exact-byte digest
    /// (which equalled the manifest's declared digest).
    public let tables: [VerifiedTable]
    /// The consistency checks returned by the reused complete package validator.
    public let consistencyChecks: [ConsistencyCheck]
    /// Always `true` for a returned result: the reader fails closed (throws) on any verification failure.
    public let verified: Bool

    public struct VerifiedTable: Sendable, Hashable {
        public let fileName: String
        public let rowCount: Int
        public let sha256: String
    }

    public struct ConsistencyCheck: Sendable, Hashable {
        public let id: String
        public let status: String
        public let severity: String
        public let details: String
    }
}

/// Typed, deterministic reader errors. Each identifies the failing file/table/field with enough context
/// to diagnose the failure, without echoing arbitrary package cell contents.
public enum STPDResultPackageReaderError: Error, LocalizedError, Hashable {
    case packageRootUnreadable(path: String)
    case packageRootNotADirectory(path: String)
    case manifestUndecodable(reason: String)
    case unsupportedSchemaVersion(String)
    case tableSetMismatch(missing: [String], unexpected: [String])
    case duplicateManifestTableName(String)
    case unexpectedPackageEntry(String)
    case illegalEntryName(name: String)
    case symlinkNotAllowed(name: String)
    case entryMissing(name: String)
    case entryUnreadable(name: String, reason: String)
    case entryNotARegularFile(name: String)
    case nonCanonicalDigest(table: String)
    case digestMismatch(table: String)
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
        case .nonCanonicalDigest(let table):
            return "Result package manifest declares a non-canonical (non-lowercase-64-hex) SHA-256 for table \(table)."
        case .digestMismatch(let table):
            return "Result package table \(table) does not match its declared SHA-256 digest."
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
