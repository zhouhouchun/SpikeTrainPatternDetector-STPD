/// A format-neutral, explicit decision about whether the first logical tabular record is a header.
public enum CanonicalTabularHeaderDecision: Hashable, Sendable {
    case firstRecordIsHeader
    case headerless
}

/// The former CSV-specific name remains source-compatible while callers migrate to the shared
/// tabular contract. A type alias is intentional: it does not create a second overload surface.
@available(*, deprecated, renamed: "CanonicalTabularHeaderDecision")
public typealias CanonicalCSVHeaderDecision = CanonicalTabularHeaderDecision

/// Format-neutral workload limits shared by lossless CSV and XLSX staging boundaries.
///
/// Readers validate each caller-selected value as positive and no greater than
/// `supportedDatasetEnvelope` before reading any source content.
public struct CanonicalTabularWorkloadLimits: Hashable, Sendable {
    public let maximumColumnCount: Int
    public let maximumLogicalDataRowCount: Int
    public let maximumMaterializedDataCellCount: Int
    public let maximumDecodedCellUTF8ByteCount: Int
    public let maximumTotalMaterializedTextUTF8ByteCount: Int

    public init(
        maximumColumnCount: Int,
        maximumLogicalDataRowCount: Int,
        maximumMaterializedDataCellCount: Int,
        maximumDecodedCellUTF8ByteCount: Int,
        maximumTotalMaterializedTextUTF8ByteCount: Int
    ) {
        self.maximumColumnCount = maximumColumnCount
        self.maximumLogicalDataRowCount = maximumLogicalDataRowCount
        self.maximumMaterializedDataCellCount = maximumMaterializedDataCellCount
        self.maximumDecodedCellUTF8ByteCount = maximumDecodedCellUTF8ByteCount
        self.maximumTotalMaterializedTextUTF8ByteCount =
            maximumTotalMaterializedTextUTF8ByteCount
    }

    /// Canonical string (65,536) + event-attribute key (256) + `@` and `=` delimiters.
    public static let supportedMaximumDecodedCellUTF8ByteCount = 65_536 + 256 + 2

    /// The shared product envelope. Transport-specific readers may impose stricter source limits.
    public static let supportedDatasetEnvelope = CanonicalTabularWorkloadLimits(
        maximumColumnCount: 512,
        maximumLogicalDataRowCount: 1_000_000,
        maximumMaterializedDataCellCount: 1_000_000,
        maximumDecodedCellUTF8ByteCount: supportedMaximumDecodedCellUTF8ByteCount,
        maximumTotalMaterializedTextUTF8ByteCount: 64 * 1_024 * 1_024
    )
}
