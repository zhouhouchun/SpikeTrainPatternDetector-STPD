import Foundation

enum TimeFormatting {
    static func seconds(_ value: Double) -> String {
        guard value.isFinite else {
            return "NA"
        }

        if abs(value) < 1e-12 {
            return "0 s"
        }
        if value < 0.001 {
            return String(format: "%.0f us", value * 1_000_000)
        }
        if value < 1 {
            return String(format: "%.1f ms", value * 1_000)
        }
        if value < 10 {
            return String(format: "%.3f s", value)
        }
        if value < 100 {
            return String(format: "%.2f s", value)
        }
        return String(format: "%.1f s", value)
    }
}
