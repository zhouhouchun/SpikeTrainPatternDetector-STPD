import Foundation

/// Exports the manual-derived calibration summary to a CSV (preview/audit only). Separate from the
/// manual-annotation CSV and the public auto-event CSV. Every row carries provenance: `source`,
/// `applied_to_detector` (always `false` in this phase), and `method`.
public enum ManualAnnotationCalibrationCSVExporter {
    public static let headers: [String] = [
        "label",
        "is_family",
        "is_positive",
        "annotation_count",
        "train_count",
        "covered_isi_count",
        "q10_ms",
        "median_ms",
        "q90_ms",
        "q95_ms",
        "sample_cv",
        "usable_for_calibration",
        "source",
        "applied_to_detector",
        "method"
    ]

    public static func csv(summary: ManualAnnotationCalibrationSummary) -> String {
        var lines: [String] = [headers.map(csvEscaped).joined(separator: ",")]
        for row in summary.rows {
            let fields: [String] = [
                row.label,
                row.isFamily ? "true" : "false",
                row.isPositive ? "true" : "false",
                String(row.annotationCount),
                String(row.trainCount),
                String(row.coveredISICount),
                milliseconds(row.q10ISISeconds),
                milliseconds(row.medianISISeconds),
                milliseconds(row.q90ISISeconds),
                milliseconds(row.q95ISISeconds),
                number(row.sampleCV),
                row.isUsableForCalibration ? "true" : "false",
                row.source,
                row.appliedToDetector ? "true" : "false",
                row.method
            ]
            lines.append(fields.map(csvEscaped).joined(separator: ","))
        }
        return lines.joined(separator: "\n") + "\n"
    }

    private static func milliseconds(_ seconds: Double?) -> String {
        guard let seconds, seconds.isFinite else { return "" }
        return String(format: "%.6g", seconds * 1000)
    }

    private static func number(_ value: Double?) -> String {
        guard let value, value.isFinite else { return "" }
        return String(format: "%.6g", value)
    }

    private static func csvEscaped(_ value: String) -> String {
        guard value.contains(where: { $0 == "," || $0 == "\"" || $0 == "\n" || $0 == "\r" }) else {
            return value
        }
        return "\"\(value.replacingOccurrences(of: "\"", with: "\"\""))\""
    }
}
