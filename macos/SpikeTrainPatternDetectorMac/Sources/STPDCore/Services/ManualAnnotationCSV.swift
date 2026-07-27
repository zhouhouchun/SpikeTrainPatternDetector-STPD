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
        let rows = parseCSV(contents)
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
