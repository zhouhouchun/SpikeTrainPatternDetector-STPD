import Foundation

/// Serializes `ReviewedISIExportRow`s to the reviewed-final per-ISI CSV (one row per spike timestamp,
/// carrying both the raw `auto_pattern` and the reviewed `final_pattern`). Read-only / additive — it is
/// independent of the existing candidate/audit exporter.
public enum ReviewedISIExportCSVExporter {
    public static let headers = [
        "train_id",
        "train_name",
        "spike_index",
        "timestamp_sec",
        "aligned_timestamp_sec",
        "isi_index",
        "isi_sec",
        "auto_pattern",
        "auto_subtype",
        "auto_candidate_id",
        "final_pattern",
        "final_subtype",
        "final_source",
        "manual_veto_suppressed",
        "review_note"
    ]

    public static func csv(rows: [ReviewedISIExportRow]) -> String {
        var lines: [[String]] = [headers]
        for row in rows {
            lines.append([
                row.trainID,
                row.trainName,
                String(row.spikeIndex),
                number(row.timestampSec),
                number(row.alignedTimestampSec),
                String(row.isiIndex),
                number(row.isiSec),
                row.autoPattern,
                row.autoSubtype,
                row.autoCandidateID,
                row.finalPattern,
                row.finalSubtype,
                row.finalSource,
                row.manualVetoSuppressed ? "true" : "false",
                row.reviewNote
            ])
        }
        return lines.map { $0.map(escaped).joined(separator: ",") }.joined(separator: "\n") + "\n"
    }

    private static func number(_ value: Double) -> String {
        guard value.isFinite else { return "" }
        return String(format: "%.12g", value)
    }

    private static func escaped(_ field: String) -> String {
        guard field.contains(",") || field.contains("\"") || field.contains("\n") else {
            return field
        }
        return "\"" + field.replacingOccurrences(of: "\"", with: "\"\"") + "\""
    }
}
