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
        "created_at",
        "updated_at"
    ]

    public static func csv(annotations: [ManualAnnotation]) -> String {
        let formatter = ISO8601DateFormatter()
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
                formatter.string(from: annotation.createdAt),
                formatter.string(from: annotation.updatedAt)
            ]
            lines.append(row.map(csvEscaped).joined(separator: ","))
        }
        return lines.joined(separator: "\n") + "\n"
    }

    private static func number(_ value: Double) -> String {
        guard value.isFinite else { return "" }
        return String(format: "%.9g", value)
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

        public var errorDescription: String? {
            switch self {
            case .missingColumn(let column):
                return "Manual annotation CSV is missing the required \(column) column."
            }
        }
    }

    public static func importAnnotations(contents: String) throws -> ImportResult {
        let rows = parseCSV(contents)
        guard let header = rows.first else {
            return ImportResult(annotations: [], unsupportedLabels: [], skippedRowCount: 0)
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
        let startISIIndex = header.firstIndex(of: "start_isi_index")
        let endISIIndex = header.firstIndex(of: "end_isi_index")
        let startSpikeIndex = header.firstIndex(of: "start_spike_index")
        let endSpikeIndex = header.firstIndex(of: "end_spike_index")
        let noteIndex = header.firstIndex(of: "note")
        let createdIndex = header.firstIndex(of: "created_at")
        let updatedIndex = header.firstIndex(of: "updated_at")

        let formatter = ISO8601DateFormatter()
        var annotations: [ManualAnnotation] = []
        var unsupported: [String] = []
        var skipped = 0

        func field(_ row: [String], _ index: Int?) -> String? {
            guard let index, index < row.count else { return nil }
            let value = row[index].trimmingCharacters(in: .whitespacesAndNewlines)
            return value.isEmpty ? nil : value
        }

        for row in rows.dropFirst() {
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

            let id = field(row, idIndex).flatMap(UUID.init(uuidString:)) ?? UUID()
            let created = field(row, createdIndex).flatMap { formatter.date(from: $0) } ?? Date(timeIntervalSince1970: 0)
            let updated = field(row, updatedIndex).flatMap { formatter.date(from: $0) } ?? created

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
                    note: field(row, noteIndex),
                    createdAt: created,
                    updatedAt: updated
                )
            )
        }

        return ImportResult(annotations: annotations, unsupportedLabels: unsupported, skippedRowCount: skipped)
    }

    private static func parseCSV(_ csv: String) -> [[String]] {
        var rows: [[String]] = []
        var row: [String] = []
        var field = ""
        var isQuoted = false
        var index = csv.startIndex

        while index < csv.endIndex {
            let character = csv[index]
            if character == "\"" {
                let next = csv.index(after: index)
                if isQuoted, next < csv.endIndex, csv[next] == "\"" {
                    field.append("\"")
                    index = csv.index(after: next)
                    continue
                }
                isQuoted.toggle()
            } else if character == ",", !isQuoted {
                row.append(field)
                field = ""
            } else if character == "\n", !isQuoted {
                row.append(field)
                if !row.allSatisfy(\.isEmpty) {
                    rows.append(row)
                }
                row = []
                field = ""
            } else if character != "\r" {
                field.append(character)
            }
            index = csv.index(after: index)
        }

        if !field.isEmpty || !row.isEmpty {
            row.append(field)
            rows.append(row)
        }
        return rows
    }
}
