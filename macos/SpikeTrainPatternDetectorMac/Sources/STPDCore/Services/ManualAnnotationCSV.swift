import CryptoKit
import Foundation

/// Exports manual annotations to a CSV that is intentionally SEPARATE from the public auto-event CSV
/// (`ClassicAnchorEventCSVExporter`). Manual marks must never appear as auto events.
public enum ManualAnnotationCSVExporter {
    public static let headers: [String] = [
        "annotation_id",
        "train_id",
        "polarity",
        "label",
        "start_sec",
        "end_sec",
        "start_isi_index",
        "end_isi_index",
        "start_spike_index",
        "end_spike_index",
        "note",
        "annotator",
        "annotator_identity_source",
        "created_at",
        "updated_at",
        "created_at_unix_sec",
        "updated_at_unix_sec"
    ]

    public static func csv(annotations: [ManualAnnotation]) -> String {
        var lines: [String] = [headers.map(csvEscaped).joined(separator: ",")]
        for annotation in annotations {
            let row: [String] = [
                annotation.id.uuidString,
                annotation.trainID,
                annotation.polarity.rawValue,
                annotation.label.rawValue,
                number(annotation.startSec),
                number(annotation.endSec),
                integer(annotation.startISIIndex),
                integer(annotation.endISIIndex),
                integer(annotation.startSpikeIndex),
                integer(annotation.endSpikeIndex),
                annotation.note ?? "",
                annotation.annotator ?? "",
                annotation.annotatorIdentitySource?.rawValue ?? "",
                STPDCanonicalValue.date(annotation.createdAt),
                STPDCanonicalValue.date(annotation.updatedAt),
                STPDCanonicalValue.double(annotation.createdAt.timeIntervalSince1970),
                STPDCanonicalValue.double(annotation.updatedAt.timeIntervalSince1970)
            ]
            lines.append(row.map(csvEscaped).joined(separator: ","))
        }
        return lines.joined(separator: "\r\n") + "\r\n"
    }

    /// The standalone identity-bound CSV format version. A file that declares `schema_version` with any
    /// other value is treated as an unsupported schema (fail-closed), never as a legacy file.
    public static let identitySchemaVersion = "manual_annotation_csv_v2"

    /// The identity-bound header: the original 17 legacy columns followed by the four optional
    /// identity/governance columns, appended at the end so the legacy columns keep their positions.
    public static let identityBoundHeaders: [String] =
        headers + ["schema_version", "dataset_digest", "run_id", "review_state"]

    /// Exports manual annotations as an identity-bound CSV (21 columns). The identity envelope
    /// (`schema_version`, `dataset_digest`, `run_id`, `review_state`) is repeated verbatim on every row
    /// and validated as a file-level envelope on import. This binds the exchange file to the dataset it
    /// was authored against; it does not itself grant authority (see `ManualAnnotationCSVImporter`).
    public static func csv(
        annotations: [ManualAnnotation],
        identity: ManualAnnotationCSVExportIdentity
    ) -> String {
        let envelope = [
            identitySchemaVersion,
            identity.datasetDigest,
            identity.runID ?? "",
            identity.reviewState.rawValue,
        ]
        var lines: [String] = [identityBoundHeaders.map(csvEscaped).joined(separator: ",")]
        for annotation in annotations {
            let row: [String] = [
                annotation.id.uuidString,
                annotation.trainID,
                annotation.polarity.rawValue,
                annotation.label.rawValue,
                number(annotation.startSec),
                number(annotation.endSec),
                integer(annotation.startISIIndex),
                integer(annotation.endISIIndex),
                integer(annotation.startSpikeIndex),
                integer(annotation.endSpikeIndex),
                annotation.note ?? "",
                annotation.annotator ?? "",
                annotation.annotatorIdentitySource?.rawValue ?? "",
                STPDCanonicalValue.date(annotation.createdAt),
                STPDCanonicalValue.date(annotation.updatedAt),
                STPDCanonicalValue.double(annotation.createdAt.timeIntervalSince1970),
                STPDCanonicalValue.double(annotation.updatedAt.timeIntervalSince1970),
            ] + envelope
            lines.append(row.map(csvEscaped).joined(separator: ","))
        }
        return lines.joined(separator: "\r\n") + "\r\n"
    }

    private static func number(_ value: Double) -> String {
        guard value.isFinite else { return "" }
        return STPDCanonicalValue.double(value)
    }

    private static func integer(_ value: Int?) -> String {
        value.map(String.init) ?? ""
    }

    private static func csvEscaped(_ value: String) -> String {
        guard value.contains(where: { $0 == "," || $0 == "\"" || $0 == "\n" || $0 == "\r" }) else {
            return value
        }
        return "\"\(value.replacingOccurrences(of: "\"", with: "\"\""))\""
    }
}

private enum ManualAnnotationCSVTimestamp {
    typealias Parsed = STPDParsedTimestamp

    static func parseISO(_ value: String) -> Parsed? {
        STPDCanonicalTimestamp.parse(value)
    }
}

/// Imports manual annotations from CSV. Tolerates missing optional index columns (geometry is
/// recomputed from time when a dataset is available), and reports rows whose `label` is not a known
/// consumed label as `unsupported` rather than importing a silently-inert annotation.
public enum ManualAnnotationCSVImporter {
    public struct ImportResult: Hashable, Sendable {
        public var annotations: [ManualAnnotation]
        /// `label` values that were not recognized (kept out of the active set so an unknown veto can
        /// never silently behave as an active veto).
        public var unsupportedLabels: [String]
        /// Rows dropped because a required column/value was missing or malformed.
        public var skippedRowCount: Int

        public init(annotations: [ManualAnnotation], unsupportedLabels: [String], skippedRowCount: Int) {
            self.annotations = annotations
            self.unsupportedLabels = unsupportedLabels
            self.skippedRowCount = skippedRowCount
        }
    }

    public enum ImportError: Error, LocalizedError {
        case missingColumn(String)
        case malformedHeader
        case duplicateColumn(String)
        /// The legacy parse-only API was handed an identity-bound file (any of the four identity
        /// columns present). Callers must route identity-bound files through `importIdentityBound(_:)`
        /// so the identity envelope and authority gating are never silently ignored.
        case identityColumnsPresent
        /// Raw-byte ingestion (`importIdentityBound(data:)`) was handed bytes that are not valid UTF-8.
        /// Decoding fails closed — no lossy replacement, no import object is produced — so a corrupted or
        /// wrongly-encoded file can never be silently reinterpreted.
        case invalidUTF8

        public var errorDescription: String? {
            switch self {
            case .missingColumn(let column):
                return "Manual annotation CSV is missing the required \(column) column."
            case .malformedHeader:
                return "Manual annotation CSV has a malformed quoted header."
            case .duplicateColumn(let column):
                return "Manual annotation CSV contains the duplicate \(column) column."
            case .identityColumnsPresent:
                return "This CSV carries manual-annotation identity columns; use "
                    + "importIdentityBound(contents:) instead of the legacy importAnnotations(contents:)."
            case .invalidUTF8:
                return "Manual annotation CSV bytes are not valid UTF-8; the file cannot be decoded."
            }
        }
    }

    /// The four optional identity/governance columns that mark an identity-bound standalone CSV.
    static let identityColumnNames: Set<String> = ["schema_version", "dataset_digest", "run_id", "review_state"]

    static func headerHasIdentityColumns(_ header: [String]) -> Bool {
        header.contains { identityColumnNames.contains($0) }
    }

    /// Legacy, parse-only import for the 17-column standalone CSV. It fails closed if any identity
    /// column is present so an identity-bound file can never be read while ignoring its identity
    /// envelope. Identity-bound files must use `importIdentityBound(contents:)`.
    public static func importAnnotations(contents: String) throws -> ImportResult {
        let normalized = strippingLeadingBOM(contents)
        let rows = parseCSV(normalized)
        if let headerRow = rows.first, headerRow.isValid, headerHasIdentityColumns(headerRow.fields) {
            throw ImportError.identityColumnsPresent
        }
        return try parseAnnotationsCore(contents: contents)
    }

    /// Shared private parser used by both the legacy `importAnnotations` and the identity-bound path.
    /// It parses the annotation columns and ignores any unknown (including identity) columns; it does
    /// not itself gate identity, so `importIdentityBound` does not depend on the public legacy bypass.
    private static func parseAnnotationsCore(contents: String) throws -> ImportResult {
        let rows = parseCSV(strippingLeadingBOM(contents))
        guard let headerRow = rows.first else {
            return ImportResult(annotations: [], unsupportedLabels: [], skippedRowCount: 0)
        }
        guard headerRow.isValid else { throw ImportError.malformedHeader }
        let header = headerRow.fields
        var seenColumns = Set<String>()
        if let duplicate = header.first(where: {
            !seenColumns.insert($0).inserted
        }) {
            throw ImportError.duplicateColumn(duplicate)
        }

        func requireColumn(_ name: String) throws -> Int {
            guard let index = header.firstIndex(of: name) else { throw ImportError.missingColumn(name) }
            return index
        }
        let trainIDIndex = try requireColumn("train_id")
        let labelIndex = try requireColumn("label")
        let startIndex = try requireColumn("start_sec")
        let endIndex = try requireColumn("end_sec")

        // Optional columns.
        let idIndex = header.firstIndex(of: "annotation_id")
        let polarityIndex = header.firstIndex(of: "polarity")
        let startISIIndex = header.firstIndex(of: "start_isi_index")
        let endISIIndex = header.firstIndex(of: "end_isi_index")
        let startSpikeIndex = header.firstIndex(of: "start_spike_index")
        let endSpikeIndex = header.firstIndex(of: "end_spike_index")
        let noteIndex = header.firstIndex(of: "note")
        let annotatorIndex = header.firstIndex(of: "annotator")
        let annotatorIdentitySourceIndex = header.firstIndex(of: "annotator_identity_source")
        let createdIndex = header.firstIndex(of: "created_at")
        let updatedIndex = header.firstIndex(of: "updated_at")
        let createdUnixIndex = header.firstIndex(of: "created_at_unix_sec")
        let updatedUnixIndex = header.firstIndex(of: "updated_at_unix_sec")

        var annotations: [ManualAnnotation] = []
        var unsupported: [String] = []
        var skipped = 0

        func field(_ row: [String], _ index: Int?) -> String? {
            guard let index, index < row.count else { return nil }
            let value = row[index].trimmingCharacters(in: .whitespacesAndNewlines)
            return value.isEmpty ? nil : value
        }

        func verbatimField(_ row: [String], _ index: Int?) -> String? {
            guard let index, index < row.count else { return nil }
            let value = row[index]
            return value.isEmpty ? nil : value
        }

        func timestamp(
            row: [String],
            isoIndex: Int?,
            exactIndex: Int?,
            fallback: Date
        ) -> (valid: Bool, value: Date) {
            func declaredField(_ index: Int?) -> (
                declared: Bool,
                value: String?
            ) {
                guard let index else {
                    return (false, nil)
                }
                guard index < row.count else {
                    return (true, nil)
                }
                let value = row[index].trimmingCharacters(
                    in: .whitespacesAndNewlines
                )
                return (true, value.isEmpty ? nil : value)
            }

            let isoEvidence = declaredField(isoIndex)
            let exactEvidence = declaredField(exactIndex)
            guard isoEvidence.declared || exactEvidence.declared else {
                // Legacy schemas may omit both timestamp representations.
                return (true, fallback)
            }
            guard !isoEvidence.declared || isoEvidence.value != nil,
                  !exactEvidence.declared || exactEvidence.value != nil else {
                // Once a timestamp column is declared, a blank or truncated cell
                // is missing authority evidence and must not invent chronology.
                return (false, fallback)
            }

            let isoRaw = isoEvidence.value
            let exactRaw = exactEvidence.value
            let parsedISO: ManualAnnotationCSVTimestamp.Parsed?
            if let isoRaw {
                guard let parsed = ManualAnnotationCSVTimestamp.parseISO(isoRaw) else {
                    return (false, fallback)
                }
                parsedISO = parsed
            } else {
                parsedISO = nil
            }
            let parsedExact: Double?
            if let exactRaw {
                guard let seconds = Double(exactRaw), seconds.isFinite else {
                    return (false, fallback)
                }
                let exactDate = Date(timeIntervalSince1970: seconds)
                guard !STPDCanonicalTimestamp.string(exactDate).isEmpty else {
                    return (false, fallback)
                }
                if parsedISO == nil,
                   exactDate.timeIntervalSince1970.bitPattern
                    != seconds.bitPattern {
                    // Without independent ISO evidence, Date must preserve the
                    // exact numeric authority rather than silently shifting it.
                    return (false, fallback)
                }
                parsedExact = seconds
            } else {
                parsedExact = nil
            }
            if let parsedISO, let parsedExact {
                if parsedISO.hasFractionalSeconds {
                    let exactProjection = STPDCanonicalValue.date(
                        Date(timeIntervalSince1970: parsedExact)
                    )
                    guard exactProjection == isoRaw,
                          parsedISO.seconds.bitPattern
                            == parsedExact.bitPattern else {
                        return (false, fallback)
                    }
                } else {
                    guard abs(parsedISO.seconds - parsedExact)
                        <= parsedISO.precisionTolerance else {
                        return (false, fallback)
                    }
                }
            }
            guard let seconds = parsedExact ?? parsedISO?.seconds else {
                return (false, fallback)
            }
            return (true, Date(timeIntervalSince1970: seconds))
        }

        for (rowOffset, parsedRow) in rows.dropFirst().enumerated() {
            guard parsedRow.isValid else {
                skipped += 1
                continue
            }
            let row = parsedRow.fields
            guard trainIDIndex < row.count, labelIndex < row.count, startIndex < row.count, endIndex < row.count else {
                skipped += 1
                continue
            }
            let trainID = row[trainIDIndex].trimmingCharacters(in: .whitespacesAndNewlines)
            let labelString = row[labelIndex].trimmingCharacters(in: .whitespacesAndNewlines)
            guard !trainID.isEmpty,
                  let startSec = Double(row[startIndex].trimmingCharacters(in: .whitespacesAndNewlines)),
                  startSec.isFinite,
                  let endSec = Double(row[endIndex].trimmingCharacters(in: .whitespacesAndNewlines)),
                  endSec.isFinite else {
                // Non-finite (nan/inf) or unparseable bounds are dropped, never imported as inert rows.
                skipped += 1
                continue
            }
            guard let label = ManualAnnotationLabel(rawValue: labelString) else {
                unsupported.append(labelString)
                continue
            }
            if let polarityIndex {
                guard polarityIndex < row.count,
                      let polarity = ManualAnnotationPolarity(
                        rawValue: row[polarityIndex]
                            .trimmingCharacters(in: .whitespacesAndNewlines)
                      ),
                      polarity == label.polarity else {
                    skipped += 1
                    continue
                }
            }

            let explicitID: UUID?
            if let idIndex {
                guard idIndex < row.count else {
                    skipped += 1
                    continue
                }
                let rawID = row[idIndex]
                let trimmedID = rawID.trimmingCharacters(
                    in: .whitespacesAndNewlines
                )
                guard rawID == trimmedID,
                      !rawID.isEmpty,
                      let parsedID = UUID(uuidString: rawID),
                      rawID.lowercased()
                        == parsedID.uuidString.lowercased() else {
                    skipped += 1
                    continue
                }
                explicitID = parsedID
            } else {
                explicitID = nil
            }
            let createdResult = timestamp(
                row: row,
                isoIndex: createdIndex,
                exactIndex: createdUnixIndex,
                fallback: Date(timeIntervalSince1970: 0)
            )
            guard createdResult.valid else {
                skipped += 1
                continue
            }
            let updatedResult = timestamp(
                row: row,
                isoIndex: updatedIndex,
                exactIndex: updatedUnixIndex,
                fallback: createdResult.value
            )
            guard updatedResult.valid else {
                skipped += 1
                continue
            }
            let created = createdResult.value
            let updated = updatedResult.value
            guard updated >= created else {
                skipped += 1
                continue
            }
            let annotator = verbatimField(row, annotatorIndex)
            let annotatorIdentitySource: ManualAnnotationIdentitySource?
            if let annotatorIdentitySourceIndex {
                // A present new-format column is explicit provenance, even when its
                // cell is blank or truncated from the row. Never reinterpret that
                // absence of evidence as imported identity authority.
                let rawIdentitySource =
                    annotatorIdentitySourceIndex < row.count
                    ? row[annotatorIdentitySourceIndex]
                        .trimmingCharacters(in: .whitespacesAndNewlines)
                    : ""
                annotatorIdentitySource = rawIdentitySource.isEmpty
                    ? .unknown
                    : ManualAnnotationIdentitySource(
                        rawValue: rawIdentitySource
                    ) ?? .unknown
            } else {
                // Only a genuinely absent legacy column may inherit `.imported`.
                annotatorIdentitySource = annotator == nil ? nil : .imported
            }
            let id = explicitID ?? deterministicLegacyID(
                rowOrdinal: rowOffset + 1,
                trainID: trainID,
                label: label,
                startSec: startSec,
                endSec: endSec,
                note: verbatimField(row, noteIndex),
                annotator: annotator,
                annotatorIdentitySource: annotatorIdentitySource,
                createdAt: created,
                updatedAt: updated
            )

            annotations.append(
                ManualAnnotation(
                    id: id,
                    trainID: trainID,
                    label: label,
                    startSec: startSec,
                    endSec: endSec,
                    startISIIndex: field(row, startISIIndex).flatMap(Int.init),
                    endISIIndex: field(row, endISIIndex).flatMap(Int.init),
                    startSpikeIndex: field(row, startSpikeIndex).flatMap(Int.init),
                    endSpikeIndex: field(row, endSpikeIndex).flatMap(Int.init),
                    note: verbatimField(row, noteIndex),
                    annotator: annotator,
                    annotatorIdentitySource: annotatorIdentitySource,
                    createdAt: created,
                    updatedAt: updated
                )
            )
        }

        return ImportResult(annotations: annotations, unsupportedLabels: unsupported, skippedRowCount: skipped)
    }

    private static func deterministicLegacyID(
        rowOrdinal: Int,
        trainID: String,
        label: ManualAnnotationLabel,
        startSec: Double,
        endSec: Double,
        note: String?,
        annotator: String?,
        annotatorIdentitySource: ManualAnnotationIdentitySource?,
        createdAt: Date,
        updatedAt: Date
    ) -> UUID {
        var hasher = SHA256()

        func append(_ value: String) {
            var byteCount = UInt64(value.utf8.count).bigEndian
            withUnsafeBytes(of: &byteCount) {
                hasher.update(data: Data($0))
            }
            hasher.update(data: Data(value.utf8))
        }

        append("stpd.manual_annotation.legacy_csv.v1")
        [
            String(rowOrdinal),
            trainID,
            label.rawValue,
            STPDCanonicalValue.double(startSec),
            STPDCanonicalValue.double(endSec),
            note ?? "",
            annotator ?? "",
            annotatorIdentitySource?.rawValue ?? "",
            STPDCanonicalValue.double(createdAt.timeIntervalSince1970),
            STPDCanonicalValue.double(updatedAt.timeIntervalSince1970),
        ].forEach(append)

        var bytes = Array(hasher.finalize().prefix(16))
        // RFC 4122 variant plus a version-5-style marker communicates that this
        // UUID is name-derived migration identity, not a random authoring ID.
        bytes[6] = (bytes[6] & 0x0f) | 0x50
        bytes[8] = (bytes[8] & 0x3f) | 0x80
        return UUID(uuid: (
            bytes[0], bytes[1], bytes[2], bytes[3],
            bytes[4], bytes[5], bytes[6], bytes[7],
            bytes[8], bytes[9], bytes[10], bytes[11],
            bytes[12], bytes[13], bytes[14], bytes[15]
        ))
    }

    /// Removes a single leading UTF-8 BOM (U+FEFF) from the first header cell only. Record-ending
    /// normalization (LF / CRLF / lone-CR) is already handled by `parseCSV` and is never applied inside
    /// quoted fields.
    static func strippingLeadingBOM(_ contents: String) -> String {
        contents.hasPrefix("\u{FEFF}") ? String(contents.dropFirst()) : contents
    }

    private struct ParsedCSVRow {
        let fields: [String]
        let isValid: Bool
    }

    private enum CSVFieldState {
        case unquoted
        case quoted
        case afterClosingQuote
    }

    /// Parse RFC 4180-style fields while retaining row validity. A quote may open only at the
    /// beginning of a field, doubled quotes are the only quotes permitted inside a quoted field,
    /// and no character may follow a closing quote except a delimiter or record terminator.
    /// Invalid data rows are recoverable and skipped by the importer; a malformed header is fatal.
    private static func parseCSV(_ csv: String) -> [ParsedCSVRow] {
        let scalars = Array(csv.unicodeScalars)
        var rows: [ParsedCSVRow] = []
        var row: [String] = []
        var field = ""
        var state = CSVFieldState.unquoted
        var rowIsValid = true
        var index = 0

        func appendRow() {
            row.append(field)
            if !row.allSatisfy(\.isEmpty) || !rowIsValid {
                rows.append(
                    ParsedCSVRow(fields: row, isValid: rowIsValid)
                )
            }
            row = []
            field = ""
            state = .unquoted
            rowIsValid = true
        }

        while index < scalars.count {
            let scalar = scalars[index]
            switch state {
            case .unquoted:
                if scalar.value == 0x22 {
                    if field.isEmpty {
                        state = .quoted
                    } else {
                        rowIsValid = false
                        field.unicodeScalars.append(scalar)
                    }
                } else if scalar.value == 0x2C {
                    row.append(field)
                    field = ""
                } else if scalar.value == 0x0D || scalar.value == 0x0A {
                    appendRow()
                    if scalar.value == 0x0D,
                       index + 1 < scalars.count,
                       scalars[index + 1].value == 0x0A {
                        index += 1
                    }
                } else {
                    field.unicodeScalars.append(scalar)
                }

            case .quoted:
                if scalar.value == 0x22 {
                    if index + 1 < scalars.count,
                       scalars[index + 1].value == 0x22 {
                        field.unicodeScalars.append(scalar)
                        index += 1
                    } else {
                        state = .afterClosingQuote
                    }
                } else {
                    field.unicodeScalars.append(scalar)
                }

            case .afterClosingQuote:
                if scalar.value == 0x2C {
                    row.append(field)
                    field = ""
                    state = .unquoted
                } else if scalar.value == 0x0D || scalar.value == 0x0A {
                    appendRow()
                    if index + 1 < scalars.count,
                       scalar.value == 0x0D,
                       scalars[index + 1].value == 0x0A {
                        index += 1
                    }
                } else {
                    rowIsValid = false
                    field.unicodeScalars.append(scalar)
                    state = .unquoted
                }
            }
            index += 1
        }

        if state == .quoted {
            rowIsValid = false
        }
        if !field.isEmpty || !row.isEmpty || !rowIsValid {
            appendRow()
        }
        return rows
    }
}

// MARK: - Phase 2.2C-A: dataset-identity binding and fail-closed authority gating

/// The governance token recorded in the standalone CSV `review_state` column. The token can only
/// RESTRICT authority, never grant it. Frozen policy (see `gate`):
/// - `pending_confirmation`: may become eligible only after every other gate passes.
/// - `review_only`: permanently review-only for that import — approval cannot promote it.
/// - a missing `review_state`: defaults to `review_only`.
/// - an unknown or inconsistent `review_state`: `malformedIdentity`, review-only.
/// (A legacy unbound file is review-only regardless.)
public enum ManualAnnotationCSVReviewState: String, Hashable, Sendable, CaseIterable {
    /// Awaiting explicit approval on import; may become eligible once all other gates pass.
    case pendingConfirmation = "pending_confirmation"
    /// Permanently review-only for that import; explicit approval cannot promote it.
    case reviewOnly = "review_only"
}

/// Identity envelope supplied when exporting an identity-bound standalone CSV. `datasetDigest` must be
/// the canonical `DetectionDatasetSnapshot` digest of the dataset the annotations were authored against.
public struct ManualAnnotationCSVExportIdentity: Hashable, Sendable {
    public let datasetDigest: String
    public let runID: String?
    public let reviewState: ManualAnnotationCSVReviewState

    public init(
        datasetDigest: String,
        runID: String? = nil,
        reviewState: ManualAnnotationCSVReviewState = .pendingConfirmation
    ) {
        self.datasetDigest = datasetDigest
        self.runID = runID
        self.reviewState = reviewState
    }

    /// Reuses the canonical dataset-identity digest (`DetectionDatasetSnapshot`) — the same digest used
    /// by the run/result-package system. Identity is never derived from train_id, train name, ISI index,
    /// or row geometry.
    public static func forDataset(
        _ dataset: SpikeDataset,
        runID: String? = nil,
        reviewState: ManualAnnotationCSVReviewState = .pendingConfirmation
    ) -> ManualAnnotationCSVExportIdentity {
        ManualAnnotationCSVExportIdentity(
            datasetDigest: DetectionDatasetSnapshot.make(dataset: dataset).digest,
            runID: runID,
            reviewState: reviewState
        )
    }
}

/// The file-level identity envelope parsed from a standalone CSV (before matching against an active
/// dataset). Every nonempty value of each declared column must agree across rows.
public struct ManualAnnotationCSVIdentityEnvelope: Hashable, Sendable {
    public let schemaVersion: String?
    public let datasetDigest: String?
    public let runID: String?
    public let reviewState: String?
}

/// Dataset-INDEPENDENT classification of a parsed standalone CSV's identity envelope. `matching` vs
/// `mismatch` are only resolvable against an active dataset (see `ManualAnnotationCSVIdentity`).
public enum ManualAnnotationCSVFileIdentity: Hashable, Sendable {
    /// The four identity columns are genuinely absent from the header (classic legacy file).
    case legacyUnbound
    /// Identity columns are declared but blank/truncated, inconsistent across rows, missing the
    /// dataset_digest anchor, or carry an invalid review_state token. Fails closed.
    case malformedIdentity
    /// A `schema_version` is declared with an unrecognized value. Fails closed (never treated as legacy).
    case unsupportedSchema
    /// A well-formed, consistent identity envelope with a dataset_digest anchor.
    case bound(datasetDigest: String)
}

/// Final identity classification of a standalone CSV against an active dataset.
public enum ManualAnnotationCSVIdentity: Hashable, Sendable {
    case matchingDataset
    case datasetMismatch
    case legacyUnbound
    case malformedIdentity
    case unsupportedSchema
}

/// Authority classification. Parsing and identity matching are never authority: only
/// `eligibleAfterExplicitConfirmation` may be promoted, and only via an explicit approval operation.
public enum ManualAnnotationCSVAuthority: Hashable, Sendable {
    case reviewOnly
    case eligibleAfterExplicitConfirmation
}

/// The result of parsing a standalone CSV with its file-level identity envelope. This is not authority:
/// it must be gated against an active dataset (`ManualAnnotationCSVImporter.gate`).
///
/// Every stored property is `let`: the identity, the parsed annotations, the parse-loss metadata, the
/// governance envelope, and the source-file digest are one integrity-bound parse result, and the digest
/// stands for exactly these parsed contents. External mutation between parsing and gating must be
/// impossible — a caller must not be able to retarget the identity, inject/replace annotations under a
/// stale digest, or erase parse-loss blockers before handing this value to `gate`. STPDCore constructs
/// this value once (in `importIdentityBound`) and never mutates it, so `let` costs nothing internally.
public struct ManualAnnotationCSVImport: Hashable, Sendable {
    public let annotations: [ManualAnnotation]
    public let unsupportedLabels: [String]
    public let skippedRowCount: Int
    public let fileIdentity: ManualAnnotationCSVFileIdentity
    public let envelope: ManualAnnotationCSVIdentityEnvelope
    /// The approval-bound source-file digest (lowercase SHA-256 hex). Its byte basis depends on the entry
    /// point: via `importIdentityBound(data:)` it binds the EXACT supplied file bytes (raw-byte digest);
    /// via `importIdentityBound(contents:)` it is the SHA-256 of `Data(contents.utf8)` (the re-encoding of
    /// the already-decoded `String`, which need not equal the on-disk bytes). Either way it is carried
    /// through gating unchanged so a typed approval verifies it is approving the same source that was
    /// gated. Prefer the `data:` entry point when the raw file bytes are available.
    public let sourceFileDigest: String
}

/// A specific, typed reason authority is withheld. Recorded on every gated import so the reason is
/// auditable rather than encoded only in a free-form string.
public enum ManualAnnotationCSVAuthorityBlocker: Hashable, Sendable {
    case legacyUnbound
    case malformedIdentity
    case unsupportedSchema
    case datasetMismatch
    /// `review_state` is `review_only` (or missing → defaulted to `review_only`).
    case reviewOnlyState
    case geometryIncompatible
    case skippedRows(Int)
    case unsupportedLabels([String])
    case emptyImport
}

/// An auditable approval record required to promote an eligible import to authoritative annotations.
/// It records approval evidence (which file, which dataset, who, when); it does not cryptographically
/// prove a human click — the App confirmation action is a later Phase 2.2C-B concern.
public struct ManualAnnotationCSVApproval: Hashable, Sendable {
    public let sourceFileDigest: String
    public let activeDatasetDigest: String
    public let approver: String
    public let approvedAt: Date

    /// Fails to construct when the approver identity is empty/whitespace, either digest is empty, or the
    /// approval timestamp is non-finite (NaN/±infinity) — so an approval object can never exist without a
    /// nonempty approver, both digests, and a finite, audit-quality timestamp. The timestamp check is
    /// audit hardening only; it does not otherwise alter authority decisions.
    public init?(
        sourceFileDigest: String,
        activeDatasetDigest: String,
        approver: String,
        approvedAt: Date
    ) {
        let trimmedApprover = approver.trimmingCharacters(in: .whitespacesAndNewlines)
        guard !trimmedApprover.isEmpty, !sourceFileDigest.isEmpty, !activeDatasetDigest.isEmpty,
              approvedAt.timeIntervalSinceReferenceDate.isFinite else {
            return nil
        }
        self.sourceFileDigest = sourceFileDigest
        self.activeDatasetDigest = activeDatasetDigest
        self.approver = trimmedApprover
        self.approvedAt = approvedAt
    }
}

public enum ManualAnnotationCSVAuthorityError: Error, LocalizedError {
    case notEligibleForAuthority(ManualAnnotationCSVIdentity, [ManualAnnotationCSVAuthorityBlocker])
    case sourceFileDigestMismatch
    case datasetDigestMismatch

    public var errorDescription: String? {
        switch self {
        case .notEligibleForAuthority(let identity, let blockers):
            return "Manual annotation import is not eligible for authority (identity: \(identity); "
                + "blockers: \(blockers)); it remains review-only."
        case .sourceFileDigestMismatch:
            return "Approval source-file digest does not match the gated import."
        case .datasetDigestMismatch:
            return "Approval dataset digest does not match the gated import's active dataset."
        }
    }
}

/// A standalone CSV import gated against an active dataset. The ONLY way to obtain authoritative
/// annotations is `authoritativeAnnotations(approval:)` with a matching `ManualAnnotationCSVApproval`;
/// it succeeds solely when the import is `eligibleAfterExplicitConfirmation` (active-dataset digest
/// match AND geometry compatibility AND no parse loss AND a non-`review_only` state) and the approval's
/// source-file and dataset digests match. There is no no-argument promotion and no boolean bypass; a
/// mismatched, legacy, malformed, unsupported, geometry-incompatible, review-only, partially-parsed, or
/// empty import can never be promoted. Import never mutates detector labels or application state.
public struct ManualAnnotationCSVGatedImport: Hashable, Sendable {
    /// When `authority == .eligibleAfterExplicitConfirmation`, these are the geometry-resolved
    /// annotations (indices recomputed against the active dataset from authoritative time). Otherwise
    /// they are the raw parsed annotations and must be treated as review-only.
    public let annotations: [ManualAnnotation]
    public let identity: ManualAnnotationCSVIdentity
    public let authority: ManualAnnotationCSVAuthority
    /// The specific, typed reasons authority is withheld (empty iff `eligibleAfterExplicitConfirmation`).
    public let blockers: [ManualAnnotationCSVAuthorityBlocker]
    public let sourceFileDigest: String
    public let activeDatasetDigest: String
    public let envelope: ManualAnnotationCSVIdentityEnvelope
    public let unsupportedLabels: [String]
    public let skippedRowCount: Int

    /// Promote to authoritative annotations using an auditable approval record. Throws unless the import
    /// is eligible AND the approval's `sourceFileDigest` and `activeDatasetDigest` match this gated
    /// import. Returns exactly the geometry-resolved annotations. There is no no-argument promotion.
    public func authoritativeAnnotations(
        approval: ManualAnnotationCSVApproval
    ) throws -> [ManualAnnotation] {
        guard authority == .eligibleAfterExplicitConfirmation else {
            throw ManualAnnotationCSVAuthorityError.notEligibleForAuthority(identity, blockers)
        }
        guard approval.sourceFileDigest == sourceFileDigest else {
            throw ManualAnnotationCSVAuthorityError.sourceFileDigestMismatch
        }
        guard approval.activeDatasetDigest == activeDatasetDigest else {
            throw ManualAnnotationCSVAuthorityError.datasetDigestMismatch
        }
        return annotations
    }
}

public extension ManualAnnotationCSVImporter {
    /// Lowercase SHA-256 (hex) over the EXACT supplied bytes. This is the raw-byte, approval-bound
    /// source-file digest: it binds the precise bytes of the file the user selected (BOM included; a
    /// LF file and its CRLF twin hash differently). The bytes are never normalized before hashing.
    static func sourceFileDigest(_ data: Data) -> String {
        SHA256.hash(data: data).map { String(format: "%02x", $0) }.joined()
    }

    /// SHA-256 (hex) of the UTF-8 encoding of the exact `String` supplied to the importer
    /// (`Data(contents.utf8)`) — NOT of the original on-disk file bytes. Behavior is unchanged from
    /// earlier phases; it delegates to the raw-byte overload so the two share one hash implementation.
    /// When the App has the original file bytes it should prefer `importIdentityBound(data:)`, whose
    /// digest binds those exact bytes rather than the re-encoding of an already-decoded String.
    static func sourceFileDigest(_ contents: String) -> String {
        sourceFileDigest(Data(contents.utf8))
    }

    /// Parses a standalone CSV (decoded `String`) and classifies its file-level identity envelope. Its
    /// approval-bound digest is the SHA-256 of `Data(contents.utf8)` (see `sourceFileDigest(_:String)`),
    /// which does NOT necessarily equal the original file's on-disk bytes. Observable behavior is
    /// unchanged from earlier phases. Prefer `importIdentityBound(data:)` when the raw bytes are available.
    static func importIdentityBound(contents: String) throws -> ManualAnnotationCSVImport {
        try makeIdentityBoundImport(contents: contents, sourceFileDigest: sourceFileDigest(contents))
    }

    /// Raw-byte identity-bound import. The approval-bound source digest binds the EXACT supplied bytes:
    /// it is computed BEFORE decoding and carried unchanged through import → gate → approval matching.
    /// Bytes are decoded with STRICT UTF-8 (`String(data:encoding:)`), which fails closed on invalid
    /// UTF-8 (throwing `ImportError.invalidUTF8`) — never lossy replacement. Parsing of the decoded
    /// contents (leading-BOM stripping, LF/CRLF/lone-CR handling) is identical to the String path and
    /// does not affect the raw digest.
    static func importIdentityBound(data: Data) throws -> ManualAnnotationCSVImport {
        let digest = sourceFileDigest(data)
        guard let contents = String(data: data, encoding: .utf8) else {
            throw ImportError.invalidUTF8
        }
        return try makeIdentityBoundImport(contents: contents, sourceFileDigest: digest)
    }

    /// Shared identity-bound import: parses the decoded `contents` and classifies its identity envelope,
    /// carrying the ALREADY-COMPUTED `sourceFileDigest` through unchanged. Both the String and raw-byte
    /// entry points delegate here, so parsing and identity-envelope logic exist in exactly one place; the
    /// only difference between the two entry points is which bytes the digest binds.
    private static func makeIdentityBoundImport(
        contents: String,
        sourceFileDigest digest: String
    ) throws -> ManualAnnotationCSVImport {
        let base = try parseAnnotationsCore(contents: contents)
        let (fileIdentity, envelope) = try classifyIdentityEnvelope(contents: contents)
        return ManualAnnotationCSVImport(
            annotations: base.annotations,
            unsupportedLabels: base.unsupportedLabels,
            skippedRowCount: base.skippedRowCount,
            fileIdentity: fileIdentity,
            envelope: envelope,
            sourceFileDigest: digest
        )
    }

    /// Gates a parsed import against the active dataset. Authority is granted (as
    /// `eligibleAfterExplicitConfirmation`) only when ALL of these independent gates pass: the file is
    /// `bound` and its `dataset_digest` matches the canonical active-dataset digest; every annotation is
    /// geometry-compatible (train resolvable + within-train time bounds); `review_state` is
    /// `pending_confirmation` (missing/`review_only` blocks); no row was skipped and no label was
    /// unsupported (no partial-parse loss); and the import is non-empty. Otherwise the import is
    /// `reviewOnly` with typed `blockers`. Even then, promotion still requires a matching typed approval.
    static func gate(
        _ imported: ManualAnnotationCSVImport,
        activeDataset: SpikeDataset
    ) -> ManualAnnotationCSVGatedImport {
        let activeDigest = DetectionDatasetSnapshot.make(dataset: activeDataset).digest
        var blockers: [ManualAnnotationCSVAuthorityBlocker] = []
        var identity: ManualAnnotationCSVIdentity
        var resolvedAnnotations = imported.annotations

        switch imported.fileIdentity {
        case .legacyUnbound:
            identity = .legacyUnbound
            blockers.append(.legacyUnbound)
        case .malformedIdentity:
            identity = .malformedIdentity
            blockers.append(.malformedIdentity)
        case .unsupportedSchema:
            identity = .unsupportedSchema
            blockers.append(.unsupportedSchema)
        case .bound(let fileDigest):
            if fileDigest != activeDigest {
                identity = .datasetMismatch
                blockers.append(.datasetMismatch)
            } else {
                identity = .matchingDataset
                // review_state policy: `pending_confirmation` may proceed; `review_only` (or a missing
                // review_state, which defaults to `review_only`) is a permanent review-only blocker.
                // The CSV token can only RESTRICT authority, never grant it.
                let effectiveReviewState = imported.envelope.reviewState
                    .flatMap(ManualAnnotationCSVReviewState.init(rawValue:)) ?? .reviewOnly
                if effectiveReviewState == .reviewOnly {
                    blockers.append(.reviewOnlyState)
                }
                // Geometry gate: authoritative time (startSec/endSec) resolves indices against the active
                // dataset; a missing train or out-of-train time fails. Cached indices are recomputed, not
                // trusted, so a stale cached index never blocks a time-compatible annotation.
                let resolved = imported.annotations.map {
                    ManualAnnotationGeometryResolver.resolvingIndicesIfCompatible($0, in: activeDataset.trains)
                }
                if resolved.contains(where: { $0 == nil }) {
                    blockers.append(.geometryIncompatible)
                } else {
                    resolvedAnnotations = resolved.compactMap { $0 }
                }
            }
        }

        // Partial-parse loss and empty imports can never become authoritative, regardless of identity.
        if imported.skippedRowCount > 0 { blockers.append(.skippedRows(imported.skippedRowCount)) }
        if !imported.unsupportedLabels.isEmpty { blockers.append(.unsupportedLabels(imported.unsupportedLabels)) }
        if imported.annotations.isEmpty { blockers.append(.emptyImport) }

        let eligible = identity == .matchingDataset && blockers.isEmpty
        let authority: ManualAnnotationCSVAuthority = eligible ? .eligibleAfterExplicitConfirmation : .reviewOnly
        return ManualAnnotationCSVGatedImport(
            annotations: eligible ? resolvedAnnotations : imported.annotations,
            identity: identity,
            authority: authority,
            blockers: blockers,
            sourceFileDigest: imported.sourceFileDigest,
            activeDatasetDigest: activeDigest,
            envelope: imported.envelope,
            unsupportedLabels: imported.unsupportedLabels,
            skippedRowCount: imported.skippedRowCount
        )
    }

    /// Validates the four identity columns as a repeated file-level envelope and classifies the file.
    private static func classifyIdentityEnvelope(
        contents: String
    ) throws -> (ManualAnnotationCSVFileIdentity, ManualAnnotationCSVIdentityEnvelope) {
        let rows = parseCSV(strippingLeadingBOM(contents))
        let empty = ManualAnnotationCSVIdentityEnvelope(
            schemaVersion: nil, datasetDigest: nil, runID: nil, reviewState: nil
        )
        guard let headerRow = rows.first, headerRow.isValid else {
            return (.legacyUnbound, empty)
        }
        let header = headerRow.fields
        let schemaIdx = header.firstIndex(of: "schema_version")
        let digestIdx = header.firstIndex(of: "dataset_digest")
        let runIdx = header.firstIndex(of: "run_id")
        let reviewIdx = header.firstIndex(of: "review_state")

        // Legacy means the identity columns are genuinely ABSENT (not present-but-empty).
        if schemaIdx == nil, digestIdx == nil, runIdx == nil, reviewIdx == nil {
            return (.legacyUnbound, empty)
        }

        // Correction A: in an identity-bound file EVERY physical data row participates in identity
        // validation. Any syntactically invalid data row (e.g. one whose malformed quoting could hide a
        // conflicting dataset_digest) fails the whole file closed — it can never be a clean bound file.
        if rows.dropFirst().contains(where: { !$0.isValid }) {
            return (.malformedIdentity, empty)
        }

        let dataRows = rows.dropFirst()

        // For a declared column, resolve the single file-level value repeated across every physical data
        // row. The envelope must be CONSISTENT: a column is malformed if it carries more than one
        // distinct nonempty value, OR if it is blank on some rows but nonblank on others (mixed
        // blank/nonblank — one populated row must not silently speak for a blank row). A
        // `requiredWhenDeclared` column (the dataset_digest identity anchor and the schema_version format
        // marker) is additionally malformed when every row leaves it blank, preventing a declared-but-
        // empty identity from being silently downgraded to legacy. An optional column (run_id,
        // review_state) that is uniformly blank is a valid missing value (review_state then defaults to
        // review_only). run_id is never itself an authority gate — this rule only enforces file-level
        // envelope consistency.
        func column(
            _ index: Int?,
            requiredWhenDeclared: Bool
        ) -> (declared: Bool, value: String?, malformed: Bool) {
            guard let index else { return (false, nil, false) }
            var values = Set<String>()
            var sawBlank = false
            for row in dataRows {
                let cell = index < row.fields.count
                    ? row.fields[index].trimmingCharacters(in: .whitespacesAndNewlines)
                    : ""
                if cell.isEmpty { sawBlank = true } else { values.insert(cell) }
            }
            // A) More than one distinct nonempty value across rows — inconsistent envelope.
            if values.count > 1 { return (true, nil, true) }
            // B) Blank on some rows but nonblank on others — the envelope is not repeated consistently
            // across every physical data row. Applies to required AND optional columns alike.
            if sawBlank, !values.isEmpty { return (true, nil, true) }
            // E) Zero physical data rows: preserve the existing fail-closed empty-file behavior (no value;
            // a required column then fails the downstream dataset_digest binding guard).
            if dataRows.isEmpty { return (true, values.first, false) }
            // C) All rows blank: required columns are malformed; optional columns are a valid missing value.
            if requiredWhenDeclared, values.isEmpty { return (true, nil, true) }
            // D) Every row carries the same nonempty value (or an optional column is uniformly blank).
            return (true, values.first, false)
        }

        let schema = column(schemaIdx, requiredWhenDeclared: true)
        let digest = column(digestIdx, requiredWhenDeclared: true)
        let run = column(runIdx, requiredWhenDeclared: false)
        let review = column(reviewIdx, requiredWhenDeclared: false)
        let envelope = ManualAnnotationCSVIdentityEnvelope(
            schemaVersion: schema.value, datasetDigest: digest.value,
            runID: run.value, reviewState: review.value
        )

        // Unknown schema version fails closed (and is not silently interpreted as legacy).
        if schema.declared, !schema.malformed, let sv = schema.value,
           sv != ManualAnnotationCSVExporter.identitySchemaVersion {
            return (.unsupportedSchema, envelope)
        }
        // Any declared-but-inconsistent/blank identity column is malformed.
        if schema.malformed || digest.malformed || run.malformed || review.malformed {
            return (.malformedIdentity, envelope)
        }
        // A declared review_state must be a recognized governance token.
        if let rv = review.value, ManualAnnotationCSVReviewState(rawValue: rv) == nil {
            return (.malformedIdentity, envelope)
        }
        // Binding requires a consistent, nonempty dataset_digest anchor.
        guard digest.declared, let datasetDigest = digest.value else {
            return (.malformedIdentity, envelope)
        }
        return (.bound(datasetDigest: datasetDigest), envelope)
    }
}
