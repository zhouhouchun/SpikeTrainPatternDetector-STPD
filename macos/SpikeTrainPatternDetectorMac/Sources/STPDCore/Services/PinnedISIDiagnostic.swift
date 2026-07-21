import Foundation

/// A pinned snapshot of a single ISI's diagnostic — captured when the user clicks an interval on the raster so it
/// can be inspected/compared without relying on hover. It is a plain value snapshot (train + interval identity,
/// timestamps, ISI value, the covering candidate's review status) wrapping the read-only Phase 7
/// `PerISIDiagnostic`. It carries no detection logic and never changes detector output.
public struct PinnedISIDiagnostic: Hashable, Sendable, Identifiable {
    public let trainID: String
    public let trainName: String
    /// 1-based ISI index (matching the candidate ISI-index convention: the interval ending at this spike).
    public let isiIndex: Int
    public let leftTimestampSec: Double
    public let rightTimestampSec: Double
    public let isiSec: Double
    /// The manual review status title of the covering candidate ("Accepted"/…), or `nil` when the ISI is not
    /// covered by a public candidate.
    public let reviewStatus: String?
    /// The reused read-only per-ISI diagnostic (band relation + explanation + candidate coverage).
    public let diagnostic: PerISIDiagnostic

    public init(
        trainID: String,
        trainName: String,
        isiIndex: Int,
        leftTimestampSec: Double,
        rightTimestampSec: Double,
        isiSec: Double,
        reviewStatus: String?,
        diagnostic: PerISIDiagnostic
    ) {
        self.trainID = trainID
        self.trainName = trainName
        self.isiIndex = isiIndex
        self.leftTimestampSec = leftTimestampSec
        self.rightTimestampSec = rightTimestampSec
        self.isiSec = isiSec
        self.reviewStatus = reviewStatus
        self.diagnostic = diagnostic
    }

    /// Stable identity for the pinned interval.
    public var id: String { "\(trainID)#\(isiIndex)" }

    /// The covering candidate's (readable) label, or `nil` for an "Other" ISI.
    public var autoLabel: String? { diagnostic.candidateLabel }

    /// Whether a public candidate covers this ISI.
    public var belongsToCandidate: Bool { diagnostic.belongsToCandidate }
}
