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

        public var errorDescription: String? {
            switch self {
            case .missingColumn(let column):
                return "Manual annotation CSV is missing the required \(column) column."
            case .malformedHeader:
                return "Manual annotation CSV has a malformed quoted header."
            case .duplicateColumn(let column):
                return "Manual annotation CSV contains the duplicate \(column) column."
            }
        }
    }

    public static func importAnnotations(contents: String) throws -> ImportResult {
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

/// The governance token recorded in the standalone CSV `review_state` column. It is informational
/// provenance only: a CSV-declared review_state can never grant authority, bypass dataset matching,
/// bypass geometry validation, or bypass explicit confirmation.
public enum ManualAnnotationCSVReviewState: String, Hashable, Sendable, CaseIterable {
    case pendingConfirmation = "pending_confirmation"
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
public struct ManualAnnotationCSVImport: Hashable, Sendable {
    public var annotations: [ManualAnnotation]
    public var unsupportedLabels: [String]
    public var skippedRowCount: Int
    public var fileIdentity: ManualAnnotationCSVFileIdentity
    public var envelope: ManualAnnotationCSVIdentityEnvelope
}

public enum ManualAnnotationCSVAuthorityError: Error, LocalizedError {
    case notEligibleForAuthority(ManualAnnotationCSVIdentity, ManualAnnotationCSVAuthority)

    public var errorDescription: String? {
        switch self {
        case .notEligibleForAuthority(let identity, _):
            return "Manual annotation import is not eligible for authority (identity: \(identity)); "
                + "it remains review-only."
        }
    }
}

/// A standalone CSV import gated against an active dataset. The ONLY way to obtain authoritative
/// annotations is `confirmAuthoritative()`, which succeeds solely when the import is
/// `eligibleAfterExplicitConfirmation` (active-dataset digest match AND geometry compatibility). A
/// mismatched, legacy, malformed, unsupported, or geometry-incompatible import can never be promoted,
/// and no boolean flag can bypass this. Import never mutates detector labels or application state.
public struct ManualAnnotationCSVGatedImport: Hashable, Sendable {
    /// When `authority == .eligibleAfterExplicitConfirmation`, these are the geometry-resolved
    /// annotations (indices filled against the active dataset). Otherwise they are the raw parsed
    /// annotations and must be treated as review-only.
    public let annotations: [ManualAnnotation]
    public let identity: ManualAnnotationCSVIdentity
    public let authority: ManualAnnotationCSVAuthority
    public let envelope: ManualAnnotationCSVIdentityEnvelope
    public let unsupportedLabels: [String]
    public let skippedRowCount: Int

    /// Explicit-confirmation promotion. This is the separate approval step; it returns authoritative
    /// annotations only for an already identity-matched, geometry-compatible import, and throws
    /// otherwise. There is deliberately no `confirmedByUser: Bool` parameter that could bypass a
    /// missing or mismatched identity.
    public func confirmAuthoritative() throws -> [ManualAnnotation] {
        guard authority == .eligibleAfterExplicitConfirmation else {
            throw ManualAnnotationCSVAuthorityError.notEligibleForAuthority(identity, authority)
        }
        return annotations
    }
}

public extension ManualAnnotationCSVImporter {
    /// Parses a standalone CSV and classifies its file-level identity envelope. Reuses
    /// `importAnnotations` for all annotation/label/geometry-field/skip parsing, then validates the four
    /// identity columns as a repeated file-level envelope. This is not authority.
    static func importIdentityBound(contents: String) throws -> ManualAnnotationCSVImport {
        let base = try importAnnotations(contents: contents)
        let (fileIdentity, envelope) = try classifyIdentityEnvelope(contents: contents)
        return ManualAnnotationCSVImport(
            annotations: base.annotations,
            unsupportedLabels: base.unsupportedLabels,
            skippedRowCount: base.skippedRowCount,
            fileIdentity: fileIdentity,
            envelope: envelope
        )
    }

    /// Gates a parsed import against the active dataset: resolves `matching`/`mismatch` via the canonical
    /// dataset digest, applies geometry compatibility, and assigns authority. Matching identity alone is
    /// never sufficient — a matching import is at most `eligibleAfterExplicitConfirmation`, and a matching
    /// but geometry-incompatible import stays `reviewOnly`.
    static func gate(
        _ imported: ManualAnnotationCSVImport,
        activeDataset: SpikeDataset
    ) -> ManualAnnotationCSVGatedImport {
        func result(
            _ annotations: [ManualAnnotation],
            _ identity: ManualAnnotationCSVIdentity,
            _ authority: ManualAnnotationCSVAuthority
        ) -> ManualAnnotationCSVGatedImport {
            ManualAnnotationCSVGatedImport(
                annotations: annotations,
                identity: identity,
                authority: authority,
                envelope: imported.envelope,
                unsupportedLabels: imported.unsupportedLabels,
                skippedRowCount: imported.skippedRowCount
            )
        }

        switch imported.fileIdentity {
        case .legacyUnbound:
            return result(imported.annotations, .legacyUnbound, .reviewOnly)
        case .malformedIdentity:
            return result(imported.annotations, .malformedIdentity, .reviewOnly)
        case .unsupportedSchema:
            return result(imported.annotations, .unsupportedSchema, .reviewOnly)
        case .bound(let fileDigest):
            let activeDigest = DetectionDatasetSnapshot.make(dataset: activeDataset).digest
            guard fileDigest == activeDigest else {
                // A mismatched digest is never authoritative.
                return result(imported.annotations, .datasetMismatch, .reviewOnly)
            }
            // Identity matches. Geometry is a SEPARATE gate: every annotation must resolve against the
            // active dataset (train resolvable + within-train bounds) or the import stays review-only.
            let resolved = imported.annotations.map {
                ManualAnnotationGeometryResolver.resolvingIndicesIfCompatible($0, in: activeDataset.trains)
            }
            guard resolved.allSatisfy({ $0 != nil }) else {
                return result(imported.annotations, .matchingDataset, .reviewOnly)
            }
            // Matching identity + geometry-compatible => eligible, but only after explicit confirmation.
            // The CSV `review_state` is intentionally not consulted here; it can never grant authority.
            return result(resolved.compactMap { $0 }, .matchingDataset, .eligibleAfterExplicitConfirmation)
        }
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

        let dataRows = rows.dropFirst().filter(\.isValid)

        // For a declared column, gather the single consistent nonempty value. Any declared column that
        // is inconsistent (>1 distinct nonempty value across data rows) is malformed. A column that is
        // `requiredWhenDeclared` (the dataset_digest identity anchor and the schema_version format
        // marker) is additionally malformed if any data row leaves it blank/truncated — this prevents a
        // declared-but-empty identity from being silently downgraded to legacy. The optional governance
        // columns (run_id, review_state) may be blank; only inconsistency makes them malformed.
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
            if values.count > 1 { return (true, nil, true) }
            if dataRows.isEmpty { return (true, values.first, false) }
            if requiredWhenDeclared, sawBlank || values.isEmpty { return (true, values.first, true) }
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
