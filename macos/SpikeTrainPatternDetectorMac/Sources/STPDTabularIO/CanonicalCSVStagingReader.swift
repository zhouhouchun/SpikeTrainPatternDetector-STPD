import CryptoKit
import Foundation
import STPDCore

public enum CanonicalCSVStagingLimitKind: String, Hashable, Sendable {
    case sourceBytes
    case columns
    case logicalDataRows
    case materializedDataCells
    case rawFieldUTF8Bytes
    case decodedFieldUTF8Bytes
    case totalMaterializedTextUTF8Bytes
}

/// Immutable engineering limits for lossless CSV staging.
public struct CanonicalCSVStagingLimits: Hashable, Sendable {
    public let maximumSourceByteCount: Int
    public let maximumColumnCount: Int
    public let maximumLogicalDataRowCount: Int
    public let maximumMaterializedDataCellCount: Int
    public let maximumRawFieldUTF8ByteCount: Int
    public let maximumDecodedFieldUTF8ByteCount: Int
    public let maximumTotalMaterializedTextUTF8ByteCount: Int

    public init(
        maximumSourceByteCount: Int,
        maximumColumnCount: Int,
        maximumLogicalDataRowCount: Int,
        maximumMaterializedDataCellCount: Int,
        maximumRawFieldUTF8ByteCount: Int,
        maximumDecodedFieldUTF8ByteCount: Int,
        maximumTotalMaterializedTextUTF8ByteCount: Int =
            CanonicalTabularWorkloadLimits.supportedDatasetEnvelope
                .maximumTotalMaterializedTextUTF8ByteCount
    ) {
        self.maximumSourceByteCount = maximumSourceByteCount
        self.maximumColumnCount = maximumColumnCount
        self.maximumLogicalDataRowCount = maximumLogicalDataRowCount
        self.maximumMaterializedDataCellCount = maximumMaterializedDataCellCount
        self.maximumRawFieldUTF8ByteCount = maximumRawFieldUTF8ByteCount
        self.maximumDecodedFieldUTF8ByteCount = maximumDecodedFieldUTF8ByteCount
        self.maximumTotalMaterializedTextUTF8ByteCount =
            maximumTotalMaterializedTextUTF8ByteCount
    }

    /// The format-neutral workload projection used by this CSV transport configuration.
    public var workloadLimits: CanonicalTabularWorkloadLimits {
        CanonicalTabularWorkloadLimits(
            maximumColumnCount: maximumColumnCount,
            maximumLogicalDataRowCount: maximumLogicalDataRowCount,
            maximumMaterializedDataCellCount: maximumMaterializedDataCellCount,
            maximumDecodedCellUTF8ByteCount: maximumDecodedFieldUTF8ByteCount,
            maximumTotalMaterializedTextUTF8ByteCount:
                maximumTotalMaterializedTextUTF8ByteCount
        )
    }

    /// Canonical string (65,536) + event-attribute key (256) + `@` and `=` delimiters.
    public static let supportedMaximumDecodedFieldUTF8ByteCount =
        CanonicalTabularWorkloadLimits.supportedMaximumDecodedCellUTF8ByteCount

    /// Worst-case RFC4180 lexeme: every decoded byte is a doubled quote, plus enclosing quotes.
    public static let supportedMaximumRawFieldUTF8ByteCount =
        2 * supportedMaximumDecodedFieldUTF8ByteCount + 2

    /// The supported product envelope: exactly 5 MiB with explicit shape and field caps.
    public static let supportedDatasetEnvelope = CanonicalCSVStagingLimits(
        maximumSourceByteCount: 5 * 1_024 * 1_024,
        maximumColumnCount:
            CanonicalTabularWorkloadLimits.supportedDatasetEnvelope.maximumColumnCount,
        maximumLogicalDataRowCount:
            CanonicalTabularWorkloadLimits.supportedDatasetEnvelope.maximumLogicalDataRowCount,
        maximumMaterializedDataCellCount:
            CanonicalTabularWorkloadLimits.supportedDatasetEnvelope
                .maximumMaterializedDataCellCount,
        maximumRawFieldUTF8ByteCount: supportedMaximumRawFieldUTF8ByteCount,
        maximumDecodedFieldUTF8ByteCount: supportedMaximumDecodedFieldUTF8ByteCount,
        maximumTotalMaterializedTextUTF8ByteCount:
            CanonicalTabularWorkloadLimits.supportedDatasetEnvelope
                .maximumTotalMaterializedTextUTF8ByteCount
    )
}

/// A byte-oriented location in the original source `Data`.
public struct CanonicalCSVSourceLocation: Hashable, Sendable {
    public let oneBasedByteOffset: Int
    public let oneBasedPhysicalLine: Int
    public let oneBasedByteColumn: Int

    public init(
        oneBasedByteOffset: Int,
        oneBasedPhysicalLine: Int,
        oneBasedByteColumn: Int
    ) {
        self.oneBasedByteOffset = oneBasedByteOffset
        self.oneBasedPhysicalLine = oneBasedPhysicalLine
        self.oneBasedByteColumn = oneBasedByteColumn
    }
}

/// A field position combines physical byte provenance with logical RFC4180 record structure.
public struct CanonicalCSVFieldLocation: Hashable, Sendable {
    public let source: CanonicalCSVSourceLocation
    public let oneBasedLogicalRecord: Int
    public let oneBasedField: Int

    public init(
        source: CanonicalCSVSourceLocation,
        oneBasedLogicalRecord: Int,
        oneBasedField: Int
    ) {
        self.source = source
        self.oneBasedLogicalRecord = oneBasedLogicalRecord
        self.oneBasedField = oneBasedField
    }
}

/// The exact source and decoded facts for one physically present RFC4180 field.
public struct CanonicalCSVFieldProvenance: Hashable, Sendable {
    /// The complete field lexeme, including enclosing and doubled quotes when present.
    /// `Data` preserves byte identity even for canonically equivalent Unicode spellings.
    public let rawUTF8Lexeme: Data
    public let decodedText: String
    public let wasQuoted: Bool
    public let location: CanonicalCSVFieldLocation

    internal init(
        rawUTF8Lexeme: Data,
        decodedText: String,
        wasQuoted: Bool,
        location: CanonicalCSVFieldLocation
    ) {
        self.rawUTF8Lexeme = rawUTF8Lexeme
        self.decodedText = decodedText
        self.wasQuoted = wasQuoted
        self.location = location
    }
}

/// Source fields grouped by their RFC4180 logical record.
public struct CanonicalCSVLogicalRecordProvenance: Hashable, Sendable {
    public let oneBasedLogicalRecord: Int
    public let fields: [CanonicalCSVFieldProvenance]

    internal init(
        oneBasedLogicalRecord: Int,
        fields: [CanonicalCSVFieldProvenance]
    ) {
        self.oneBasedLogicalRecord = oneBasedLogicalRecord
        self.fields = fields
    }
}

/// Lossless, source-only facts retained beside CSV staging output.
public struct CanonicalCSVStagingProvenance: Hashable, Sendable {
    public let sourceData: Data
    public let sourceSHA256: String
    public let sourceByteCount: Int
    public let appliedLimits: CanonicalCSVStagingLimits
    public let headerDecision: CanonicalTabularHeaderDecision
    public let logicalRecords: [CanonicalCSVLogicalRecordProvenance]

    internal init(
        sourceData: Data,
        sourceSHA256: String,
        sourceByteCount: Int,
        appliedLimits: CanonicalCSVStagingLimits,
        headerDecision: CanonicalTabularHeaderDecision,
        logicalRecords: [CanonicalCSVLogicalRecordProvenance]
    ) {
        self.sourceData = sourceData
        self.sourceSHA256 = sourceSHA256
        self.sourceByteCount = sourceByteCount
        self.appliedLimits = appliedLimits
        self.headerDecision = headerDecision
        self.logicalRecords = logicalRecords
    }

    /// Returns the physical header field for a staged column, or `nil` for headerless input.
    public func headerField(
        for column: StagedSourceColumnReference
    ) -> CanonicalCSVFieldProvenance? {
        guard headerDecision == .firstRecordIsHeader,
              let record = logicalRecords.first else { return nil }
        let fieldOffset = column.oneBasedIndex - 1
        guard record.fields.indices.contains(fieldOffset) else { return nil }
        return record.fields[fieldOffset]
    }

    /// Returns the physical field behind a staged data cell. A structural blank introduced while
    /// rectangularizing a ragged record has no physical field and therefore returns `nil`.
    public func dataField(
        at cell: StagedSourceCellReference
    ) -> CanonicalCSVFieldProvenance? {
        let headerRecordCount = headerDecision == .firstRecordIsHeader ? 1 : 0
        let zeroBasedDataRow = cell.oneBasedDataRowIndex - 1
        let (recordOffset, overflow) = zeroBasedDataRow.addingReportingOverflow(
            headerRecordCount
        )
        guard !overflow else { return nil }
        guard logicalRecords.indices.contains(recordOffset) else { return nil }
        let fieldOffset = cell.column.oneBasedIndex - 1
        let record = logicalRecords[recordOffset]
        guard record.fields.indices.contains(fieldOffset) else { return nil }
        return record.fields[fieldOffset]
    }
}

/// Atomic CSV staging output with source provenance kept outside scientific identity models.
public struct CanonicalCSVStagingResult: Hashable, Sendable {
    public let stagedImport: StagedScientificImport
    public let provenance: CanonicalCSVStagingProvenance

    internal init(
        stagedImport: StagedScientificImport,
        provenance: CanonicalCSVStagingProvenance
    ) {
        self.stagedImport = stagedImport
        self.provenance = provenance
    }
}

public enum CanonicalCSVSyntaxIssue: String, Hashable, Sendable {
    case quoteInUnquotedField
    case unexpectedContentAfterClosingQuote
    case unterminatedQuotedField
}

public enum CanonicalCSVStagingReaderError: Error, Hashable, Sendable {
    case invalidLimit(kind: CanonicalCSVStagingLimitKind, actual: Int)
    case limitExceedsSupportedEnvelope(
        kind: CanonicalCSVStagingLimitKind,
        maximumSupported: Int,
        actual: Int
    )
    case sourceByteLimitExceeded(maximum: Int, actual: Int)
    case emptySource
    case zeroColumnLogicalTable
    case invalidUTF8(location: CanonicalCSVSourceLocation)
    case syntax(issue: CanonicalCSVSyntaxIssue, location: CanonicalCSVFieldLocation)
    case rawFieldByteLimitExceeded(
        maximum: Int,
        actual: Int,
        location: CanonicalCSVFieldLocation
    )
    case decodedFieldByteLimitExceeded(
        maximum: Int,
        actual: Int,
        location: CanonicalCSVFieldLocation
    )
    case columnLimitExceeded(
        maximum: Int,
        actual: Int,
        location: CanonicalCSVFieldLocation
    )
    case logicalDataRowLimitExceeded(
        maximum: Int,
        actual: Int,
        location: CanonicalCSVSourceLocation
    )
    case materializedDataCellLimitExceeded(
        maximum: Int,
        actual: Int,
        location: CanonicalCSVSourceLocation
    )
    case materializedDataCellCountOverflow(
        maximum: Int,
        location: CanonicalCSVSourceLocation
    )
    case totalMaterializedTextUTF8ByteLimitExceeded(
        maximum: Int,
        actual: Int,
        location: CanonicalCSVFieldLocation
    )
    case totalMaterializedTextUTF8ByteCountOverflow(
        maximum: Int,
        location: CanonicalCSVFieldLocation
    )
    case dataRecordWiderThanHeader(
        headerColumnCount: Int,
        actual: Int,
        location: CanonicalCSVFieldLocation
    )
    case internalInvariant
}

/// A Data-only, inactive CSV-to-staging boundary. It performs no scientific interpretation.
public enum CanonicalCSVStagingReader {
    /// Reads one complete source atomically and returns the existing staging model.
    public static func read(
        data: Data,
        headerDecision: CanonicalTabularHeaderDecision,
        limits: CanonicalCSVStagingLimits
    ) throws -> StagedScientificImport {
        try readWithProvenance(
            data: data,
            headerDecision: headerDecision,
            limits: limits
        ).stagedImport
    }

    /// Reads one complete source atomically while retaining lossless source provenance.
    ///
    /// Raw-field byte limits include the complete CSV field lexeme, including enclosing quotes
    /// and doubled quote escapes. Decoded-field limits apply after CSV quote decoding.
    public static func readWithProvenance(
        data: Data,
        headerDecision: CanonicalTabularHeaderDecision,
        limits: CanonicalCSVStagingLimits
    ) throws -> CanonicalCSVStagingResult {
        try validate(limits: limits)
        guard data.count <= limits.maximumSourceByteCount else {
            throw CanonicalCSVStagingReaderError.sourceByteLimitExceeded(
                maximum: limits.maximumSourceByteCount,
                actual: data.count
            )
        }
        guard !data.isEmpty else {
            throw CanonicalCSVStagingReaderError.emptySource
        }

        let bytes = Array(data)
        // Own the exact bytes that were inspected instead of retaining potentially externally
        // backed `Data`. This snapshot is the immutable source of both provenance and its digest.
        let sourceData = Data(bytes)
        let contentStart = hasUTF8BOM(bytes) ? 3 : 0
        guard contentStart < bytes.count else {
            throw CanonicalCSVStagingReaderError.zeroColumnLogicalTable
        }
        if let invalidOffset = firstInvalidUTF8Offset(in: bytes, startingAt: contentStart) {
            throw CanonicalCSVStagingReaderError.invalidUTF8(
                location: sourceLocation(
                    originalOffset: invalidOffset,
                    bytes: bytes,
                    contentStart: contentStart
                )
            )
        }

        var headers: [String]?
        var columnCells: [[StagedCellValue]] = []
        var logicalDataRowCount = 0
        var logicalRecords: [CanonicalCSVLogicalRecordProvenance] = []
        var parser = CanonicalCSVByteParser(
            bytes: bytes,
            contentStart: contentStart,
            limits: limits
        )
        try parser.parse { record in
            logicalRecords.append(record.provenance)
            if record.oneBasedLogicalRecord == 1,
               headerDecision == .firstRecordIsHeader {
                headers = record.fields.map(\.text)
                columnCells = Array(repeating: [], count: record.fields.count)
                return
            }

            let proposedRowCount = logicalDataRowCount + 1
            guard proposedRowCount <= limits.maximumLogicalDataRowCount else {
                throw CanonicalCSVStagingReaderError.logicalDataRowLimitExceeded(
                    maximum: limits.maximumLogicalDataRowCount,
                    actual: proposedRowCount,
                    location: record.location.source
                )
            }

            let targetWidth: Int
            if let headers {
                guard record.fields.count <= headers.count else {
                    throw CanonicalCSVStagingReaderError.dataRecordWiderThanHeader(
                        headerColumnCount: headers.count,
                        actual: record.fields.count,
                        location: record.fields[headers.count].location
                    )
                }
                targetWidth = headers.count
            } else {
                targetWidth = max(columnCells.count, record.fields.count)
            }

            let (materializedCellCount, overflow) = proposedRowCount
                .multipliedReportingOverflow(by: targetWidth)
            guard !overflow else {
                throw CanonicalCSVStagingReaderError.materializedDataCellCountOverflow(
                    maximum: limits.maximumMaterializedDataCellCount,
                    location: record.location.source
                )
            }
            guard materializedCellCount <= limits.maximumMaterializedDataCellCount else {
                throw CanonicalCSVStagingReaderError.materializedDataCellLimitExceeded(
                    maximum: limits.maximumMaterializedDataCellCount,
                    actual: materializedCellCount,
                    location: record.location.source
                )
            }

            if targetWidth > columnCells.count {
                for _ in columnCells.count..<targetWidth {
                    columnCells.append(
                        Array(repeating: .blank, count: logicalDataRowCount)
                    )
                }
            }
            for columnOffset in 0..<targetWidth {
                if record.fields.indices.contains(columnOffset) {
                    columnCells[columnOffset].append(record.fields[columnOffset].stagedValue)
                } else {
                    columnCells[columnOffset].append(.blank)
                }
            }
            logicalDataRowCount = proposedRowCount
        }

        guard !columnCells.isEmpty else {
            throw CanonicalCSVStagingReaderError.zeroColumnLogicalTable
        }
        let stagedImport: StagedScientificImport
        do {
            let stagedColumns = try columnCells.indices.map { offset in
                StagedScientificColumn(
                    sourceColumn: try StagedSourceColumnReference(
                        oneBasedIndex: offset + 1
                    ),
                    header: headers?[offset],
                    cells: columnCells[offset]
                )
            }
            stagedImport = try StagedScientificImport(
                source: .commaSeparatedValues,
                columns: stagedColumns,
                suggestions: .none
            )
        } catch is StagedSourceReferenceError {
            throw CanonicalCSVStagingReaderError.internalInvariant
        } catch is StagedScientificImportStructureError {
            throw CanonicalCSVStagingReaderError.internalInvariant
        }
        let digest = SHA256.hash(data: sourceData).map { String(format: "%02x", $0) }.joined()
        return CanonicalCSVStagingResult(
            stagedImport: stagedImport,
            provenance: CanonicalCSVStagingProvenance(
                sourceData: sourceData,
                sourceSHA256: digest,
                sourceByteCount: sourceData.count,
                appliedLimits: limits,
                headerDecision: headerDecision,
                logicalRecords: logicalRecords
            )
        )
    }

    private static func validate(limits: CanonicalCSVStagingLimits) throws {
        let supported = CanonicalCSVStagingLimits.supportedDatasetEnvelope
        let values: [(CanonicalCSVStagingLimitKind, Int, Int)] = [
            (
                .sourceBytes,
                limits.maximumSourceByteCount,
                supported.maximumSourceByteCount
            ),
            (.columns, limits.maximumColumnCount, supported.maximumColumnCount),
            (
                .logicalDataRows,
                limits.maximumLogicalDataRowCount,
                supported.maximumLogicalDataRowCount
            ),
            (
                .materializedDataCells,
                limits.maximumMaterializedDataCellCount,
                supported.maximumMaterializedDataCellCount
            ),
            (
                .rawFieldUTF8Bytes,
                limits.maximumRawFieldUTF8ByteCount,
                supported.maximumRawFieldUTF8ByteCount
            ),
            (
                .decodedFieldUTF8Bytes,
                limits.maximumDecodedFieldUTF8ByteCount,
                supported.maximumDecodedFieldUTF8ByteCount
            ),
            (
                .totalMaterializedTextUTF8Bytes,
                limits.maximumTotalMaterializedTextUTF8ByteCount,
                supported.maximumTotalMaterializedTextUTF8ByteCount
            ),
        ]
        for (kind, actual, maximumSupported) in values {
            guard actual > 0 else {
                throw CanonicalCSVStagingReaderError.invalidLimit(kind: kind, actual: actual)
            }
            guard actual <= maximumSupported else {
                throw CanonicalCSVStagingReaderError.limitExceedsSupportedEnvelope(
                    kind: kind,
                    maximumSupported: maximumSupported,
                    actual: actual
                )
            }
        }
    }

    private static func hasUTF8BOM(_ bytes: [UInt8]) -> Bool {
        bytes.count >= 3 && bytes[0] == 0xEF && bytes[1] == 0xBB && bytes[2] == 0xBF
    }

    private static func firstInvalidUTF8Offset(
        in bytes: [UInt8],
        startingAt start: Int
    ) -> Int? {
        var index = start
        while index < bytes.count {
            let first = bytes[index]
            if first <= 0x7F {
                index += 1
                continue
            }

            let continuationCount: Int
            let secondRange: ClosedRange<UInt8>
            switch first {
            case 0xC2...0xDF:
                continuationCount = 1
                secondRange = 0x80...0xBF
            case 0xE0:
                continuationCount = 2
                secondRange = 0xA0...0xBF
            case 0xE1...0xEC, 0xEE...0xEF:
                continuationCount = 2
                secondRange = 0x80...0xBF
            case 0xED:
                continuationCount = 2
                secondRange = 0x80...0x9F
            case 0xF0:
                continuationCount = 3
                secondRange = 0x90...0xBF
            case 0xF1...0xF3:
                continuationCount = 3
                secondRange = 0x80...0xBF
            case 0xF4:
                continuationCount = 3
                secondRange = 0x80...0x8F
            default:
                return index
            }

            guard index + continuationCount < bytes.count else { return index }
            let secondIndex = index + 1
            guard secondRange.contains(bytes[secondIndex]) else { return secondIndex }
            if continuationCount >= 2 {
                let thirdIndex = index + 2
                guard (0x80...0xBF).contains(bytes[thirdIndex]) else { return thirdIndex }
            }
            if continuationCount == 3 {
                let fourthIndex = index + 3
                guard (0x80...0xBF).contains(bytes[fourthIndex]) else {
                    return fourthIndex
                }
            }
            index += continuationCount + 1
        }
        return nil
    }

    fileprivate static func sourceLocation(
        originalOffset: Int,
        bytes: [UInt8],
        contentStart: Int
    ) -> CanonicalCSVSourceLocation {
        var line = 1
        var column = 1
        var index = contentStart
        while index < originalOffset, index < bytes.count {
            if bytes[index] == 0x0D {
                line += 1
                column = 1
                if index + 1 < originalOffset,
                   index + 1 < bytes.count,
                   bytes[index + 1] == 0x0A {
                    index += 2
                    continue
                }
            } else if bytes[index] == 0x0A {
                line += 1
                column = 1
            } else {
                column += 1
            }
            index += 1
        }
        return CanonicalCSVSourceLocation(
            oneBasedByteOffset: originalOffset + 1,
            oneBasedPhysicalLine: line,
            oneBasedByteColumn: column
        )
    }
}

private struct CanonicalCSVParsedField {
    let rawUTF8Lexeme: Data
    let text: String
    let wasQuoted: Bool
    let location: CanonicalCSVFieldLocation

    var stagedValue: StagedCellValue {
        if text.isEmpty, !wasQuoted { return .blank }
        return .text(rawText: text)
    }

    var provenance: CanonicalCSVFieldProvenance {
        CanonicalCSVFieldProvenance(
            rawUTF8Lexeme: rawUTF8Lexeme,
            decodedText: text,
            wasQuoted: wasQuoted,
            location: location
        )
    }
}

private struct CanonicalCSVParsedRecord {
    let oneBasedLogicalRecord: Int
    let fields: [CanonicalCSVParsedField]

    var location: CanonicalCSVFieldLocation {
        fields[0].location
    }

    var provenance: CanonicalCSVLogicalRecordProvenance {
        CanonicalCSVLogicalRecordProvenance(
            oneBasedLogicalRecord: oneBasedLogicalRecord,
            fields: fields.map(\.provenance)
        )
    }
}

private enum CanonicalCSVParserState {
    case fieldStart
    case unquoted
    case quoted
    case afterClosingQuote
}

private struct CanonicalCSVByteParser {
    let bytes: [UInt8]
    let contentStart: Int
    let limits: CanonicalCSVStagingLimits
    /// Built once so successful field finalization never rescans the source prefix.
    let physicalLineStartOffsets: [Int]

    private var index: Int
    private var state = CanonicalCSVParserState.fieldStart
    private var recordFields: [CanonicalCSVParsedField] = []
    private var decodedFieldBytes: [UInt8] = []
    private var rawFieldByteCount = 0
    private var totalMaterializedTextUTF8ByteCount = 0
    private var fieldWasQuoted = false
    private var fieldStartOffset: Int
    private var recordHasStarted = false
    private var completedRecordCount = 0

    init(
        bytes: [UInt8],
        contentStart: Int,
        limits: CanonicalCSVStagingLimits
    ) {
        self.bytes = bytes
        self.contentStart = contentStart
        self.limits = limits
        self.physicalLineStartOffsets = Self.makePhysicalLineStartOffsets(
            bytes: bytes,
            contentStart: contentStart
        )
        self.index = contentStart
        self.fieldStartOffset = contentStart
    }

    mutating func parse(
        consume: (CanonicalCSVParsedRecord) throws -> Void
    ) throws {
        while index < bytes.count {
            let byte = bytes[index]
            switch state {
            case .fieldStart:
                if byte == 0x2C {
                    try finishField(endOffset: index)
                    recordHasStarted = true
                    index += 1
                    fieldStartOffset = index
                } else if byte == 0x22 {
                    fieldWasQuoted = true
                    recordHasStarted = true
                    try addRawBytes(1, at: index)
                    state = .quoted
                    index += 1
                } else if byte == 0x0D || byte == 0x0A {
                    try finishField(endOffset: index)
                    try finishRecord(consume: consume)
                    consumeRecordSeparator()
                } else {
                    recordHasStarted = true
                    try addRawBytes(1, at: index)
                    try addDecodedByte(byte, at: index)
                    state = .unquoted
                    index += 1
                }
            case .unquoted:
                if byte == 0x2C {
                    try finishField(endOffset: index)
                    state = .fieldStart
                    index += 1
                    fieldStartOffset = index
                } else if byte == 0x0D || byte == 0x0A {
                    try finishField(endOffset: index)
                    try finishRecord(consume: consume)
                    consumeRecordSeparator()
                } else if byte == 0x22 {
                    throw CanonicalCSVStagingReaderError.syntax(
                        issue: .quoteInUnquotedField,
                        location: fieldLocation(at: index)
                    )
                } else {
                    try addRawBytes(1, at: index)
                    try addDecodedByte(byte, at: index)
                    index += 1
                }
            case .quoted:
                if byte == 0x22 {
                    if index + 1 < bytes.count, bytes[index + 1] == 0x22 {
                        try addRawBytes(2, at: index)
                        try addDecodedByte(0x22, at: index)
                        index += 2
                    } else {
                        try addRawBytes(1, at: index)
                        state = .afterClosingQuote
                        index += 1
                    }
                } else {
                    try addRawBytes(1, at: index)
                    try addDecodedByte(byte, at: index)
                    index += 1
                }
            case .afterClosingQuote:
                if byte == 0x2C {
                    try finishField(endOffset: index)
                    state = .fieldStart
                    index += 1
                    fieldStartOffset = index
                } else if byte == 0x0D || byte == 0x0A {
                    try finishField(endOffset: index)
                    try finishRecord(consume: consume)
                    consumeRecordSeparator()
                } else {
                    throw CanonicalCSVStagingReaderError.syntax(
                        issue: .unexpectedContentAfterClosingQuote,
                        location: fieldLocation(at: index)
                    )
                }
            }
        }

        switch state {
        case .quoted:
            throw CanonicalCSVStagingReaderError.syntax(
                issue: .unterminatedQuotedField,
                location: fieldLocation(at: fieldStartOffset)
            )
        case .fieldStart:
            if recordHasStarted {
                try finishField(endOffset: index)
                try finishRecord(consume: consume)
            }
        case .unquoted, .afterClosingQuote:
            try finishField(endOffset: index)
            try finishRecord(consume: consume)
        }
    }

    private mutating func addRawBytes(_ count: Int, at offset: Int) throws {
        let proposed = rawFieldByteCount + count
        guard proposed <= limits.maximumRawFieldUTF8ByteCount else {
            throw CanonicalCSVStagingReaderError.rawFieldByteLimitExceeded(
                maximum: limits.maximumRawFieldUTF8ByteCount,
                actual: proposed,
                location: fieldLocation(at: offset)
            )
        }
        rawFieldByteCount = proposed
    }

    private mutating func addDecodedByte(_ byte: UInt8, at offset: Int) throws {
        let proposed = decodedFieldBytes.count + 1
        guard proposed <= limits.maximumDecodedFieldUTF8ByteCount else {
            throw CanonicalCSVStagingReaderError.decodedFieldByteLimitExceeded(
                maximum: limits.maximumDecodedFieldUTF8ByteCount,
                actual: proposed,
                location: fieldLocation(at: offset)
            )
        }
        decodedFieldBytes.append(byte)
    }

    private mutating func finishField(endOffset: Int) throws {
        let location = fieldLocation(at: fieldStartOffset)
        let (proposedTotalTextByteCount, totalTextOverflow) =
            totalMaterializedTextUTF8ByteCount.addingReportingOverflow(
                decodedFieldBytes.count
            )
        guard !totalTextOverflow else {
            throw CanonicalCSVStagingReaderError
                .totalMaterializedTextUTF8ByteCountOverflow(
                    maximum: limits.maximumTotalMaterializedTextUTF8ByteCount,
                    location: location
                )
        }
        guard proposedTotalTextByteCount <= limits.maximumTotalMaterializedTextUTF8ByteCount else {
            throw CanonicalCSVStagingReaderError.totalMaterializedTextUTF8ByteLimitExceeded(
                maximum: limits.maximumTotalMaterializedTextUTF8ByteCount,
                actual: proposedTotalTextByteCount,
                location: location
            )
        }
        totalMaterializedTextUTF8ByteCount = proposedTotalTextByteCount
        recordFields.append(
            CanonicalCSVParsedField(
                rawUTF8Lexeme: Data(bytes[fieldStartOffset..<endOffset]),
                text: String(decoding: decodedFieldBytes, as: UTF8.self),
                wasQuoted: fieldWasQuoted,
                location: location
            )
        )
        guard recordFields.count <= limits.maximumColumnCount else {
            throw CanonicalCSVStagingReaderError.columnLimitExceeded(
                maximum: limits.maximumColumnCount,
                actual: recordFields.count,
                location: location
            )
        }
        decodedFieldBytes.removeAll(keepingCapacity: true)
        rawFieldByteCount = 0
        fieldWasQuoted = false
        fieldStartOffset = endOffset + 1
    }

    private mutating func finishRecord(
        consume: (CanonicalCSVParsedRecord) throws -> Void
    ) throws {
        completedRecordCount += 1
        try consume(
            CanonicalCSVParsedRecord(
                oneBasedLogicalRecord: completedRecordCount,
                fields: recordFields
            )
        )
        recordFields.removeAll(keepingCapacity: true)
        recordHasStarted = false
        state = .fieldStart
    }

    private mutating func consumeRecordSeparator() {
        if bytes[index] == 0x0D,
           index + 1 < bytes.count,
           bytes[index + 1] == 0x0A {
            index += 2
        } else {
            index += 1
        }
        fieldStartOffset = index
    }

    private func fieldLocation(at offset: Int) -> CanonicalCSVFieldLocation {
        var lowerBound = 0
        var upperBound = physicalLineStartOffsets.count
        while lowerBound < upperBound {
            let middle = lowerBound + (upperBound - lowerBound) / 2
            if physicalLineStartOffsets[middle] <= offset {
                lowerBound = middle + 1
            } else {
                upperBound = middle
            }
        }
        let lineOffset = max(0, lowerBound - 1)
        let lineStart = physicalLineStartOffsets[lineOffset]
        return CanonicalCSVFieldLocation(
            source: CanonicalCSVSourceLocation(
                oneBasedByteOffset: offset + 1,
                oneBasedPhysicalLine: lineOffset + 1,
                oneBasedByteColumn: offset - lineStart + 1
            ),
            oneBasedLogicalRecord: completedRecordCount + 1,
            oneBasedField: recordFields.count + 1
        )
    }

    private static func makePhysicalLineStartOffsets(
        bytes: [UInt8],
        contentStart: Int
    ) -> [Int] {
        var starts = [contentStart]
        var offset = contentStart
        while offset < bytes.count {
            if bytes[offset] == 0x0D {
                if offset + 1 < bytes.count, bytes[offset + 1] == 0x0A {
                    offset += 2
                } else {
                    offset += 1
                }
                starts.append(offset)
            } else if bytes[offset] == 0x0A {
                offset += 1
                starts.append(offset)
            } else {
                offset += 1
            }
        }
        return starts
    }
}
