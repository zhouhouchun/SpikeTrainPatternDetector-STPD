import Foundation

/// A format-neutral, rectangular result table used by CSV and XLSX writers.
///
/// Every row represents one real ISI. This value carries no authority of its own; the caller is
/// responsible for supplying the authority/status columns appropriate to the source workflow.
public struct ManualISIExportTable: Hashable, Sendable {
    public let headers: [String]
    public let rows: [[String]]

    public init(headers: [String], rows: [[String]]) {
        precondition(!headers.isEmpty)
        precondition(rows.allSatisfy { $0.count == headers.count })
        self.headers = headers
        self.rows = rows
    }

    public func csv(lineEnding: String = "\n") -> String {
        ([headers] + rows)
            .map { $0.map(Self.escapeCSV).joined(separator: ",") }
            .joined(separator: lineEnding) + lineEnding
    }

    private static func escapeCSV(_ field: String) -> String {
        guard field.contains(",") || field.contains("\"")
                || field.contains("\n") || field.contains("\r") else {
            return field
        }
        return "\"" + field.replacingOccurrences(of: "\"", with: "\"\"") + "\""
    }
}

/// Exact-microsecond input for the NeuroExplorer manual-result writer.
public struct ManualISINEXRow: Hashable, Sendable {
    public let isiIndex: Int
    public let leftTimestampMicroseconds: Int64
    public let rightTimestampMicroseconds: Int64
    public let statePattern: String
    public let eventPattern: String
    public let otherPattern: String
    public let stateAnnotationID: String
    public let eventAnnotationID: String
    public let otherAnnotationID: String
    public let burstVeto: Bool
    public let reviewNote: String
    public let authorityStatus: String

    public init(
        isiIndex: Int,
        leftTimestampMicroseconds: Int64,
        rightTimestampMicroseconds: Int64,
        statePattern: String,
        eventPattern: String,
        otherPattern: String,
        stateAnnotationID: String = "",
        eventAnnotationID: String = "",
        otherAnnotationID: String = "",
        burstVeto: Bool = false,
        reviewNote: String = "",
        authorityStatus: String
    ) {
        self.isiIndex = isiIndex
        self.leftTimestampMicroseconds = leftTimestampMicroseconds
        self.rightTimestampMicroseconds = rightTimestampMicroseconds
        self.statePattern = statePattern
        self.eventPattern = eventPattern
        self.otherPattern = otherPattern
        self.stateAnnotationID = stateAnnotationID
        self.eventAnnotationID = eventAnnotationID
        self.otherAnnotationID = otherAnnotationID
        self.burstVeto = burstVeto
        self.reviewNote = reviewNote
        self.authorityStatus = authorityStatus
    }
}

public struct ManualISINEXTrain: Hashable, Sendable {
    public let trainID: String
    public let trainName: String
    public let spikeTimestampsMicroseconds: [Int64]
    public let rows: [ManualISINEXRow]

    public init(
        trainID: String,
        trainName: String,
        spikeTimestampsMicroseconds: [Int64],
        rows: [ManualISINEXRow]
    ) {
        self.trainID = trainID
        self.trainName = trainName
        self.spikeTimestampsMicroseconds = spikeTimestampsMicroseconds
        self.rows = rows
    }
}
