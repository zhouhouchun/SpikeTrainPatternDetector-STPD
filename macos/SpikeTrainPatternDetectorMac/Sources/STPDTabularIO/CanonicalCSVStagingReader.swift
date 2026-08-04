import Foundation
import STPDCore

/// An explicit decision about whether the first logical CSV record is a header.
public enum CanonicalCSVHeaderDecision: Hashable, Sendable {
    case firstRecordIsHeader
    case headerless
}

public enum CanonicalCSVStagingLimitKind: String, Hashable, Sendable {
    case sourceBytes
    case columns
    case logicalDataRows
    case materializedDataCells
    case rawFieldUTF8Bytes
    case decodedFieldUTF8Bytes
}

/// Immutable engineering limits for lossless CSV staging.
public struct CanonicalCSVStagingLimits: Hashable, Sendable {
    public let maximumSourceByteCount: Int
    public let maximumColumnCount: Int
    public let maximumLogicalDataRowCount: Int
    public let maximumMaterializedDataCellCount: Int
    public let maximumRawFieldUTF8ByteCount: Int
    public let maximumDecodedFieldUTF8ByteCount: Int

    public init(
        maximumSourceByteCount: Int,
        maximumColumnCount: Int,
        maximumLogicalDataRowCount: Int,
        maximumMaterializedDataCellCount: Int,
        maximumRawFieldUTF8ByteCount: Int,
        maximumDecodedFieldUTF8ByteCount: Int
    ) {
        self.maximumSourceByteCount = maximumSourceByteCount
        self.maximumColumnCount = maximumColumnCount
        self.maximumLogicalDataRowCount = maximumLogicalDataRowCount
        self.maximumMaterializedDataCellCount = maximumMaterializedDataCellCount
        self.maximumRawFieldUTF8ByteCount = maximumRawFieldUTF8ByteCount
        self.maximumDecodedFieldUTF8ByteCount = maximumDecodedFieldUTF8ByteCount
    }

    /// Canonical string (65,536) + event-attribute key (256) + `@` and `=` delimiters.
    public static let supportedMaximumDecodedFieldUTF8ByteCount = 65_536 + 256 + 2

    /// Worst-case RFC4180 lexeme: every decoded byte is a doubled quote, plus enclosing quotes.
    public static let supportedMaximumRawFieldUTF8ByteCount =
        2 * supportedMaximumDecodedFieldUTF8ByteCount + 2

    /// The supported product envelope: exactly 5 MiB with explicit shape and field caps.
    public static let supportedDatasetEnvelope = CanonicalCSVStagingLimits(
        maximumSourceByteCount: 5 * 1_024 * 1_024,
        maximumColumnCount: 512,
        maximumLogicalDataRowCount: 1_000_000,
        maximumMaterializedDataCellCount: 1_000_000,
        maximumRawFieldUTF8ByteCount: supportedMaximumRawFieldUTF8ByteCount,
        maximumDecodedFieldUTF8ByteCount: supportedMaximumDecodedFieldUTF8ByteCount
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
    case dataRecordWiderThanHeader(
        headerColumnCount: Int,
        actual: Int,
        location: CanonicalCSVFieldLocation
    )
    case internalInvariant
}

/// A Data-only, inactive CSV-to-staging boundary. It performs no scientific interpretation.
public enum CanonicalCSVStagingReader {
    /// Reads one complete source atomically. Both interpretation decisions are explicit.
    ///
    /// Raw-field byte limits include the complete CSV field lexeme, including enclosing quotes
    /// and doubled quote escapes. Decoded-field limits apply after CSV quote decoding.
    public static func read(
        data: Data,
        headerDecision: CanonicalCSVHeaderDecision,
        limits: CanonicalCSVStagingLimits
    ) throws -> StagedScientificImport {
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
        var parser = CanonicalCSVByteParser(
            bytes: bytes,
            contentStart: contentStart,
            limits: limits
        )
        try parser.parse { record in
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
            return try StagedScientificImport(
                source: .commaSeparatedValues,
                columns: stagedColumns,
                suggestions: .none
            )
        } catch is StagedSourceReferenceError {
            throw CanonicalCSVStagingReaderError.internalInvariant
        } catch is StagedScientificImportStructureError {
            throw CanonicalCSVStagingReaderError.internalInvariant
        }
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
    let text: String
    let wasQuoted: Bool
    let location: CanonicalCSVFieldLocation

    var stagedValue: StagedCellValue {
        if text.isEmpty, !wasQuoted { return .blank }
        return .text(rawText: text)
    }
}

private struct CanonicalCSVParsedRecord {
    let oneBasedLogicalRecord: Int
    let fields: [CanonicalCSVParsedField]

    var location: CanonicalCSVFieldLocation {
        fields[0].location
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
        recordFields.append(
            CanonicalCSVParsedField(
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
