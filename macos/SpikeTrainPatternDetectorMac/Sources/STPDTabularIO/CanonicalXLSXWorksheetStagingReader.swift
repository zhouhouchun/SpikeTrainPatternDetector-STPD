import Foundation
import STPDCore

public enum CanonicalXLSXWorksheetStagingLimitKind: String, Hashable, Sendable {
    case columns
    case logicalDataRows
    case materializedDataCells
    case physicalRows
    case physicalCells
    case columnDefinitions
    case mergeRanges
    case decodedCellUTF8Bytes
    case totalMaterializedTextUTF8Bytes
}

/// Transport-specific limits for materializing one selected worksheet.
///
/// XLSX is sparse on disk, so its supported rectangular table can be larger than the CSV reader's
/// independently frozen envelope. No value here broadens CSV parsing.
public struct CanonicalXLSXWorksheetStagingLimits: Hashable, Sendable {
    public let maximumColumnCount: Int
    public let maximumLogicalDataRowCount: Int
    public let maximumMaterializedDataCellCount: Int
    public let maximumPhysicalRowCount: Int
    public let maximumPhysicalCellCount: Int
    public let maximumColumnDefinitionCount: Int
    public let maximumMergeRangeCount: Int
    public let maximumDecodedCellUTF8ByteCount: Int
    public let maximumTotalMaterializedTextUTF8ByteCount: Int

    public init(
        maximumColumnCount: Int,
        maximumLogicalDataRowCount: Int,
        maximumMaterializedDataCellCount: Int,
        maximumPhysicalRowCount: Int,
        maximumPhysicalCellCount: Int,
        maximumColumnDefinitionCount: Int,
        maximumMergeRangeCount: Int,
        maximumDecodedCellUTF8ByteCount: Int,
        maximumTotalMaterializedTextUTF8ByteCount: Int
    ) {
        self.maximumColumnCount = maximumColumnCount
        self.maximumLogicalDataRowCount = maximumLogicalDataRowCount
        self.maximumMaterializedDataCellCount = maximumMaterializedDataCellCount
        self.maximumPhysicalRowCount = maximumPhysicalRowCount
        self.maximumPhysicalCellCount = maximumPhysicalCellCount
        self.maximumColumnDefinitionCount = maximumColumnDefinitionCount
        self.maximumMergeRangeCount = maximumMergeRangeCount
        self.maximumDecodedCellUTF8ByteCount = maximumDecodedCellUTF8ByteCount
        self.maximumTotalMaterializedTextUTF8ByteCount =
            maximumTotalMaterializedTextUTF8ByteCount
    }

    public var workloadLimits: CanonicalTabularWorkloadLimits {
        CanonicalTabularWorkloadLimits(
            maximumColumnCount: maximumColumnCount,
            maximumLogicalDataRowCount: maximumLogicalDataRowCount,
            maximumMaterializedDataCellCount: maximumMaterializedDataCellCount,
            maximumDecodedCellUTF8ByteCount: maximumDecodedCellUTF8ByteCount,
            maximumTotalMaterializedTextUTF8ByteCount:
                maximumTotalMaterializedTextUTF8ByteCount
        )
    }

    /// Sized for the owner's two representative workbooks while bounding dense staging memory.
    public static let supportedDatasetEnvelope = CanonicalXLSXWorksheetStagingLimits(
        maximumColumnCount: 1_024,
        maximumLogicalDataRowCount: 1_000_000,
        maximumMaterializedDataCellCount: 5_000_000,
        maximumPhysicalRowCount: 1_000_000,
        maximumPhysicalCellCount: 1_000_000,
        maximumColumnDefinitionCount: 16_384,
        maximumMergeRangeCount: 100_000,
        maximumDecodedCellUTF8ByteCount:
            CanonicalTabularWorkloadLimits.supportedMaximumDecodedCellUTF8ByteCount,
        maximumTotalMaterializedTextUTF8ByteCount: 64 * 1_024 * 1_024
    )
}

public struct CanonicalXLSXCellAddress: Hashable, Comparable, Sendable {
    public let oneBasedColumn: Int
    public let oneBasedRow: Int
    public let a1Reference: String

    fileprivate init(oneBasedColumn: Int, oneBasedRow: Int, a1Reference: String) {
        self.oneBasedColumn = oneBasedColumn
        self.oneBasedRow = oneBasedRow
        self.a1Reference = a1Reference
    }

    public static func < (lhs: Self, rhs: Self) -> Bool {
        if lhs.oneBasedRow != rhs.oneBasedRow {
            return lhs.oneBasedRow < rhs.oneBasedRow
        }
        return lhs.oneBasedColumn < rhs.oneBasedColumn
    }
}

public struct CanonicalXLSXCellRange: Hashable, Sendable {
    public let topLeft: CanonicalXLSXCellAddress
    public let bottomRight: CanonicalXLSXCellAddress
    public let a1Reference: String

    fileprivate init(
        topLeft: CanonicalXLSXCellAddress,
        bottomRight: CanonicalXLSXCellAddress,
        a1Reference: String
    ) {
        self.topLeft = topLeft
        self.bottomRight = bottomRight
        self.a1Reference = a1Reference
    }

    fileprivate func contains(_ address: CanonicalXLSXCellAddress) -> Bool {
        (topLeft.oneBasedColumn...bottomRight.oneBasedColumn)
            .contains(address.oneBasedColumn)
            && (topLeft.oneBasedRow...bottomRight.oneBasedRow)
                .contains(address.oneBasedRow)
    }

    fileprivate func intersects(_ other: CanonicalXLSXCellRange) -> Bool {
        topLeft.oneBasedColumn <= other.bottomRight.oneBasedColumn
            && bottomRight.oneBasedColumn >= other.topLeft.oneBasedColumn
            && topLeft.oneBasedRow <= other.bottomRight.oneBasedRow
            && bottomRight.oneBasedRow >= other.topLeft.oneBasedRow
    }
}

public enum CanonicalXLSXPhysicalCellKind: Hashable, Sendable {
    case blank
    case number(rawLexeme: String)
    case directString(rawLexeme: String, decodedText: String)
    case sharedString(index: UInt32, decodedText: String)
    case inlineString(decodedText: String)

    fileprivate var stagedValue: StagedCellValue {
        switch self {
        case .blank:
            return .blank
        case .number(let rawLexeme):
            return .spreadsheetNumber(rawLexeme: rawLexeme)
        case .directString(_, let decodedText),
             .sharedString(_, let decodedText),
             .inlineString(let decodedText):
            return .text(rawText: decodedText)
        }
    }

    fileprivate var materializedTextUTF8ByteCount: Int {
        switch self {
        case .blank:
            return 0
        case .number(let rawLexeme):
            return rawLexeme.utf8.count
        case .directString(_, let decodedText),
             .sharedString(_, let decodedText),
             .inlineString(let decodedText):
            return decodedText.utf8.count
        }
    }
}

public enum CanonicalXLSXEffectiveStyleSource: String, Hashable, Sendable {
    case workbookDefault
    case columnDefault
    case rowDefault
    case cell
}

public enum CanonicalXLSXProvenanceNumberFormatDisposition: String, Hashable, Sendable {
    case provenNonDate
    case dateOrTime
    case unsupported
}

public struct CanonicalXLSXPhysicalCellProvenance: Hashable, Sendable {
    public let address: CanonicalXLSXCellAddress
    public let rawCellTypeAttribute: String?
    public let explicitStyleIndex: UInt32?
    public let effectiveStyleIndex: UInt32?
    public let effectiveStyleSource: CanonicalXLSXEffectiveStyleSource
    public let numberFormatID: UInt16
    public let numberFormatDisposition: CanonicalXLSXProvenanceNumberFormatDisposition
    public let kind: CanonicalXLSXPhysicalCellKind
}

public struct CanonicalXLSXRowProvenance: Hashable, Sendable {
    public let oneBasedRow: Int
    public let hidden: Bool
    public let declaredStyleIndex: UInt32?
    public let customFormat: Bool
}

public struct CanonicalXLSXColumnDefinitionProvenance: Hashable, Sendable {
    public let minimumOneBasedColumn: Int
    public let maximumOneBasedColumn: Int
    public let hidden: Bool
    public let styleIndex: UInt32?
}

public struct CanonicalXLSXWorksheetStagingProvenance: Hashable, Sendable {
    public let sourceData: Data
    public let sourceSHA256: String
    public let sourceByteCount: Int
    public let inspectionLimits: CanonicalXLSXInspectionLimits
    public let stagingLimits: CanonicalXLSXWorksheetStagingLimits
    public let worksheetName: String
    public let sheetID: UInt32
    public let relationshipID: String
    public let normalizedPartPath: String
    public let visibility: CanonicalXLSXWorksheetVisibility
    public let sharedStringsPartPath: String?
    public let stylesPartPath: String?
    public let headerDecision: CanonicalTabularHeaderDecision
    public let fixedOriginRectangle: CanonicalXLSXCellRange
    public let declaredDimension: CanonicalXLSXCellRange?
    public let rows: [CanonicalXLSXRowProvenance]
    public let columnDefinitions: [CanonicalXLSXColumnDefinitionProvenance]
    public let physicalCells: [CanonicalXLSXPhysicalCellProvenance]
    public let mergeRanges: [CanonicalXLSXCellRange]
    public let totalMaterializedTextUTF8ByteCount: Int
}

public struct CanonicalXLSXWorksheetStagingResult: Hashable, Sendable {
    public let stagedImport: StagedScientificImport
    public let provenance: CanonicalXLSXWorksheetStagingProvenance
}

public enum CanonicalXLSXWorksheetIssue: String, Hashable, Sendable {
    case unexpectedRoot
    case invalidElementStructure
    case duplicateSection
    case invalidSectionOrder
    case unsupportedSection
    case missingSheetData
    case missingRequiredAttribute
    case invalidBoolean
    case invalidUnsignedInteger
    case invalidCellReference
    case invalidRangeReference
    case nonascendingRows
    case nonascendingCells
    case cellRowMismatch
    case dimensionDoesNotContainCell
    case overlappingColumnDefinitions
    case unsupportedCellType
    case formulaUnsupported
    case invalidCellValueStructure
    case invalidNumericLexeme
    case sharedStringsRequired
    case sharedStringIndexOutOfBounds
    case invalidString
    case styleIndexRequiresStylesPart
    case styleIndexOutOfBounds
    case dateOrTimeNumberFormat
    case unprovenNumberFormat
    case numericHeaderUnsupported
    case duplicateMergeRange
    case mergeMustSpanMultipleCells
    case mergeIntersectsTable
    case declaredCountMismatch
    case unexpectedText
    case invalidCDATA
    case zeroColumnLogicalTable
    case internalInvariant
}

public enum CanonicalXLSXWorksheetSupportPartKind: String, Hashable, Sendable {
    case styles
    case sharedStrings
}

public enum CanonicalXLSXWorksheetStagingReaderError: Error, Hashable, Sendable {
    case invalidLimit(kind: CanonicalXLSXWorksheetStagingLimitKind, actual: Int)
    case limitExceedsSupportedEnvelope(
        kind: CanonicalXLSXWorksheetStagingLimitKind,
        maximumSupported: Int,
        actual: Int
    )
    case worksheetSelectionNotInInspection
    case sourcePackageFailure(CanonicalXLSXWorkbookInspectionError)
    case invalidSupportPart(part: String, kind: CanonicalXLSXWorksheetSupportPartKind)
    case invalidWorksheet(
        part: String,
        issue: CanonicalXLSXWorksheetIssue,
        reference: String?,
        value: String?
    )
    case columnLimitExceeded(maximum: Int, actual: Int, reference: String)
    case logicalDataRowLimitExceeded(maximum: Int, actual: Int)
    case materializedDataCellCountOverflow(maximum: Int)
    case materializedDataCellLimitExceeded(maximum: Int, actual: Int)
    case physicalRowLimitExceeded(maximum: Int, actual: Int, reference: String)
    case physicalCellLimitExceeded(maximum: Int, actual: Int, reference: String)
    case columnDefinitionLimitExceeded(maximum: Int, actual: Int)
    case mergeRangeLimitExceeded(maximum: Int, actual: Int)
    case decodedCellByteLimitExceeded(reference: String, maximum: Int, actual: Int)
    case totalMaterializedTextByteLimitExceeded(
        reference: String,
        maximum: Int,
        actual: Int
    )
}

public enum CanonicalXLSXWorksheetStagingReader {
    public static func read(
        inspection: CanonicalXLSXWorkbookInspection,
        worksheet: CanonicalXLSXWorksheetDescriptor,
        headerDecision: CanonicalTabularHeaderDecision,
        limits: CanonicalXLSXWorksheetStagingLimits
    ) throws -> StagedScientificImport {
        try readWithProvenance(
            inspection: inspection,
            worksheet: worksheet,
            headerDecision: headerDecision,
            limits: limits
        ).stagedImport
    }

    public static func readWithProvenance(
        inspection: CanonicalXLSXWorkbookInspection,
        worksheet: CanonicalXLSXWorksheetDescriptor,
        headerDecision: CanonicalTabularHeaderDecision,
        limits: CanonicalXLSXWorksheetStagingLimits
    ) throws -> CanonicalXLSXWorksheetStagingResult {
        try validate(limits)
        guard worksheet.inspectionBinding == inspection.inspectionBinding,
              inspection.worksheets.contains(worksheet) else {
            throw CanonicalXLSXWorksheetStagingReaderError
                .worksheetSelectionNotInInspection
        }

        let parsed = try parseSelectedWorksheet(
            inspection: inspection,
            worksheet: worksheet,
            limits: limits
        )
        let dataRowCount = headerDecision == .firstRecordIsHeader
            ? parsed.maximumRow - 1
            : parsed.maximumRow
        guard dataRowCount <= limits.maximumLogicalDataRowCount else {
            throw CanonicalXLSXWorksheetStagingReaderError.logicalDataRowLimitExceeded(
                maximum: limits.maximumLogicalDataRowCount,
                actual: dataRowCount
            )
        }
        let (materializedCellCount, overflow) = dataRowCount.multipliedReportingOverflow(
            by: parsed.maximumColumn
        )
        guard !overflow else {
            throw CanonicalXLSXWorksheetStagingReaderError
                .materializedDataCellCountOverflow(
                    maximum: limits.maximumMaterializedDataCellCount
                )
        }
        guard materializedCellCount <= limits.maximumMaterializedDataCellCount else {
            throw CanonicalXLSXWorksheetStagingReaderError
                .materializedDataCellLimitExceeded(
                    maximum: limits.maximumMaterializedDataCellCount,
                    actual: materializedCellCount
                )
        }

        var headers: [String]?
        if headerDecision == .firstRecordIsHeader {
            var materializedHeaders = Array(repeating: "", count: parsed.maximumColumn)
            for cell in parsed.physicalCells where cell.address.oneBasedRow == 1 {
                switch cell.kind.stagedValue {
                case .blank:
                    break
                case .text(let rawText):
                    materializedHeaders[cell.address.oneBasedColumn - 1] = rawText
                case .spreadsheetNumber:
                    throw worksheetError(
                        worksheet.normalizedPartPath,
                        .numericHeaderUnsupported,
                        reference: cell.address.a1Reference
                    )
                }
            }
            headers = materializedHeaders
        }

        // Allocate the dense result exactly once. The sparse provenance remains row-major; no
        // second rectangular row-major table or append-growth transposition is constructed.
        var columnCells = (0..<parsed.maximumColumn).map { _ in
            Array(repeating: StagedCellValue.blank, count: dataRowCount)
        }
        let firstDataRow = headerDecision == .firstRecordIsHeader ? 2 : 1
        for cell in parsed.physicalCells where cell.address.oneBasedRow >= firstDataRow {
            let rowOffset = cell.address.oneBasedRow - firstDataRow
            columnCells[cell.address.oneBasedColumn - 1][rowOffset] = cell.kind.stagedValue
        }

        let binding: StagedSourceTransactionBinding
        let stagedImport: StagedScientificImport
        do {
            binding = try StagedSourceTransactionBinding(
                sourceBytesSHA256: inspection.sourceSHA256,
                selection: .excelWorksheet(
                    worksheetName: worksheet.name,
                    sheetID: worksheet.sheetID,
                    relationshipID: worksheet.relationshipID,
                    normalizedPartPath: worksheet.normalizedPartPath
                )
            )
            let columns = try columnCells.indices.map { offset in
                StagedScientificColumn(
                    sourceColumn: try StagedSourceColumnReference(oneBasedIndex: offset + 1),
                    header: headers?[offset],
                    cells: columnCells[offset]
                )
            }
            stagedImport = try StagedScientificImport(
                source: .excelWorkbook(worksheetName: worksheet.name),
                columns: columns,
                suggestions: .none,
                sourceTransactionBinding: binding
            )
        } catch {
            throw worksheetError(
                worksheet.normalizedPartPath,
                .internalInvariant
            )
        }

        return CanonicalXLSXWorksheetStagingResult(
            stagedImport: stagedImport,
            provenance: CanonicalXLSXWorksheetStagingProvenance(
                sourceData: inspection.sourceData,
                sourceSHA256: inspection.sourceSHA256,
                sourceByteCount: inspection.sourceByteCount,
                inspectionLimits: inspection.limits,
                stagingLimits: limits,
                worksheetName: worksheet.name,
                sheetID: worksheet.sheetID,
                relationshipID: worksheet.relationshipID,
                normalizedPartPath: worksheet.normalizedPartPath,
                visibility: worksheet.visibility,
                sharedStringsPartPath: inspection.partBindings.sharedStringsPartPath,
                stylesPartPath: inspection.partBindings.stylesPartPath,
                headerDecision: headerDecision,
                fixedOriginRectangle: parsed.fixedOriginRectangle,
                declaredDimension: parsed.declaredDimension,
                rows: parsed.rows,
                columnDefinitions: parsed.columnDefinitions,
                physicalCells: parsed.physicalCells,
                mergeRanges: parsed.mergeRanges,
                totalMaterializedTextUTF8ByteCount:
                    parsed.totalMaterializedTextUTF8ByteCount
            )
        )
    }

    private static func validate(_ limits: CanonicalXLSXWorksheetStagingLimits) throws {
        let supported = CanonicalXLSXWorksheetStagingLimits.supportedDatasetEnvelope
        let values: [(CanonicalXLSXWorksheetStagingLimitKind, Int, Int)] = [
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
            (.physicalRows, limits.maximumPhysicalRowCount, supported.maximumPhysicalRowCount),
            (
                .physicalCells,
                limits.maximumPhysicalCellCount,
                supported.maximumPhysicalCellCount
            ),
            (
                .columnDefinitions,
                limits.maximumColumnDefinitionCount,
                supported.maximumColumnDefinitionCount
            ),
            (.mergeRanges, limits.maximumMergeRangeCount, supported.maximumMergeRangeCount),
            (
                .decodedCellUTF8Bytes,
                limits.maximumDecodedCellUTF8ByteCount,
                supported.maximumDecodedCellUTF8ByteCount
            ),
            (
                .totalMaterializedTextUTF8Bytes,
                limits.maximumTotalMaterializedTextUTF8ByteCount,
                supported.maximumTotalMaterializedTextUTF8ByteCount
            ),
        ]
        for (kind, actual, maximumSupported) in values {
            guard actual > 0 else {
                throw CanonicalXLSXWorksheetStagingReaderError.invalidLimit(
                    kind: kind,
                    actual: actual
                )
            }
            guard actual <= maximumSupported else {
                throw CanonicalXLSXWorksheetStagingReaderError.limitExceedsSupportedEnvelope(
                    kind: kind,
                    maximumSupported: maximumSupported,
                    actual: actual
                )
            }
        }
    }

    private static func parseSelectedWorksheet(
        inspection: CanonicalXLSXWorkbookInspection,
        worksheet: CanonicalXLSXWorksheetDescriptor,
        limits: CanonicalXLSXWorksheetStagingLimits
    ) throws -> ParsedCanonicalXLSXWorksheet {
        var paths: Set<String> = [worksheet.normalizedPartPath]
        if let path = inspection.partBindings.stylesPartPath { paths.insert(path) }
        if let path = inspection.partBindings.sharedStringsPartPath { paths.insert(path) }
        var parts: [String: Data]
        do {
            parts = try inspection.extractRequiredParts(paths)
        } catch let error as CanonicalXLSXWorkbookInspectionError {
            throw CanonicalXLSXWorksheetStagingReaderError.sourcePackageFailure(error)
        } catch {
            throw worksheetError(worksheet.normalizedPartPath, .internalInvariant)
        }

        let styles: CanonicalXLSXStylesAvailability
        if let path = inspection.partBindings.stylesPartPath {
            guard let data = parts.removeValue(forKey: path) else {
                throw worksheetError(worksheet.normalizedPartPath, .internalInvariant)
            }
            do {
                styles = .present(try CanonicalXLSXStyleCatalogParser.parse(
                    data: data,
                    part: path,
                    limits: inspection.limits
                ))
            } catch let error as CanonicalXLSXWorkbookInspectionError {
                throw CanonicalXLSXWorksheetStagingReaderError.sourcePackageFailure(error)
            } catch is CanonicalXLSXStyleParsingError {
                throw CanonicalXLSXWorksheetStagingReaderError.invalidSupportPart(
                    part: path,
                    kind: .styles
                )
            } catch {
                throw worksheetError(worksheet.normalizedPartPath, .internalInvariant)
            }
        } else {
            styles = .absent
        }

        let sharedStrings: CanonicalXLSXSharedStringTable?
        if let path = inspection.partBindings.sharedStringsPartPath {
            guard let data = parts.removeValue(forKey: path) else {
                throw worksheetError(worksheet.normalizedPartPath, .internalInvariant)
            }
            do {
                sharedStrings = try CanonicalXLSXSharedStringTableParser.parse(
                    data: data,
                    part: path,
                    xmlLimits: inspection.limits
                )
            } catch let error as CanonicalXLSXWorkbookInspectionError {
                throw CanonicalXLSXWorksheetStagingReaderError.sourcePackageFailure(error)
            } catch is CanonicalXLSXSharedStringParsingError {
                throw CanonicalXLSXWorksheetStagingReaderError.invalidSupportPart(
                    part: path,
                    kind: .sharedStrings
                )
            } catch {
                throw worksheetError(worksheet.normalizedPartPath, .internalInvariant)
            }
        } else {
            sharedStrings = nil
        }

        guard let worksheetData = parts.removeValue(forKey: worksheet.normalizedPartPath),
              parts.isEmpty else {
            throw worksheetError(worksheet.normalizedPartPath, .internalInvariant)
        }
        let delegate = WorksheetXMLDelegate(
            part: worksheet.normalizedPartPath,
            xmlLimits: inspection.limits,
            stagingLimits: limits,
            styles: styles,
            sharedStrings: sharedStrings
        )
        do {
            try parseXML(data: worksheetData, delegate: delegate)
            return try delegate.makeWorksheet()
        } catch let error as CanonicalXLSXWorksheetStagingReaderError {
            throw error
        } catch let error as CanonicalXLSXWorkbookInspectionError {
            throw CanonicalXLSXWorksheetStagingReaderError.sourcePackageFailure(error)
        } catch {
            throw worksheetError(worksheet.normalizedPartPath, .internalInvariant)
        }
    }
}

private let worksheetSpreadsheetNamespace =
    "http://schemas.openxmlformats.org/spreadsheetml/2006/main"
private let worksheetMarkupCompatibilityNamespace =
    "http://schemas.openxmlformats.org/markup-compatibility/2006"

private let worksheetTopLevelOrder: [String: Int] = [
    "sheetPr": 0,
    "dimension": 1,
    "sheetViews": 2,
    "sheetFormatPr": 3,
    "cols": 4,
    "sheetData": 5,
    "sheetCalcPr": 6,
    "sheetProtection": 7,
    "protectedRanges": 8,
    "scenarios": 9,
    "autoFilter": 10,
    "sortState": 11,
    "dataConsolidate": 12,
    "customSheetViews": 13,
    "mergeCells": 14,
    "phoneticPr": 15,
    "conditionalFormatting": 16,
    "dataValidations": 17,
    "hyperlinks": 18,
    "printOptions": 19,
    "pageMargins": 20,
    "pageSetup": 21,
    "headerFooter": 22,
    "rowBreaks": 23,
    "colBreaks": 24,
    "customProperties": 25,
    "cellWatches": 26,
    "ignoredErrors": 27,
    "smartTags": 28,
    "drawing": 29,
    "legacyDrawing": 30,
    "legacyDrawingHF": 31,
    "picture": 32,
    "oleObjects": 33,
    "controls": 34,
    "webPublishItems": 35,
    "tableParts": 36,
    "extLst": 37,
]

private let ignoredWorksheetTopLevelElements: Set<String> = [
    "sheetPr", "sheetViews", "sheetFormatPr", "sheetCalcPr", "sheetProtection",
    "protectedRanges", "autoFilter", "sortState", "phoneticPr", "conditionalFormatting",
    "dataValidations", "hyperlinks", "printOptions", "pageMargins", "pageSetup",
    "headerFooter", "rowBreaks", "colBreaks", "customProperties", "cellWatches",
    "ignoredErrors", "drawing", "legacyDrawing", "legacyDrawingHF", "picture",
    "webPublishItems", "tableParts",
]

private let rejectedWorksheetTopLevelElements: Set<String> = [
    "scenarios", "dataConsolidate", "customSheetViews", "smartTags", "oleObjects",
    "controls", "extLst",
]

private struct ParsedCanonicalXLSXWorksheet {
    let maximumColumn: Int
    let maximumRow: Int
    let fixedOriginRectangle: CanonicalXLSXCellRange
    let declaredDimension: CanonicalXLSXCellRange?
    let rows: [CanonicalXLSXRowProvenance]
    let columnDefinitions: [CanonicalXLSXColumnDefinitionProvenance]
    let physicalCells: [CanonicalXLSXPhysicalCellProvenance]
    let mergeRanges: [CanonicalXLSXCellRange]
    let totalMaterializedTextUTF8ByteCount: Int
}

private struct ActiveWorksheetRow {
    let oneBasedRow: Int
    let hidden: Bool
    let declaredStyleIndex: UInt32?
    let customFormat: Bool
    var lastCellColumn: Int?
}

private final class ActiveWorksheetCell {
    let address: CanonicalXLSXCellAddress
    let rawType: String?
    let explicitStyleIndex: UInt32?
    let effectiveStyleIndex: UInt32?
    let effectiveStyleSource: CanonicalXLSXEffectiveStyleSource
    let effectiveFormat: CanonicalXLSXCellFormat
    var sawValue = false
    var collectingValue = false
    var valueText = ""
    var sawInlineString = false
    var inlineString: String?
    var stringAssembler: CanonicalXLSXStringItemAssembler?

    init(
        address: CanonicalXLSXCellAddress,
        rawType: String?,
        explicitStyleIndex: UInt32?,
        effectiveStyleIndex: UInt32?,
        effectiveStyleSource: CanonicalXLSXEffectiveStyleSource,
        effectiveFormat: CanonicalXLSXCellFormat
    ) {
        self.address = address
        self.rawType = rawType
        self.explicitStyleIndex = explicitStyleIndex
        self.effectiveStyleIndex = effectiveStyleIndex
        self.effectiveStyleSource = effectiveStyleSource
        self.effectiveFormat = effectiveFormat
    }
}

private final class WorksheetXMLDelegate: BoundedXMLDelegate {
    private let stagingLimits: CanonicalXLSXWorksheetStagingLimits
    private let styles: CanonicalXLSXStylesAvailability
    private let sharedStrings: CanonicalXLSXSharedStringTable?
    private let maximumRawStringTextUTF8ByteCount: Int
    private var sawRoot = false
    private var ignorablePrefixes = Set<String>()
    private var seenTopLevelElements = Set<String>()
    private var lastTopLevelOrder = -1
    private var ignoredTopLevelDepth: Int?
    private var sawSheetData = false
    private var insideColumns = false
    private var insideSheetData = false
    private var insideMergeCells = false
    private var declaredDimension: CanonicalXLSXCellRange?
    private var rows: [CanonicalXLSXRowProvenance] = []
    private var lastRowIndex: Int?
    private var activeRow: ActiveWorksheetRow?
    private var columnDefinitions: [CanonicalXLSXColumnDefinitionProvenance] = []
    private var physicalCells: [CanonicalXLSXPhysicalCellProvenance] = []
    private var activeCell: ActiveWorksheetCell?
    private var maximumColumn = 0
    private var maximumRow = 0
    private var mergeRanges: [CanonicalXLSXCellRange] = []
    private var mergeRangeSet = Set<CanonicalXLSXCellRange>()
    private var declaredMergeCount: Int?
    private var totalMaterializedTextUTF8ByteCount = 0

    override var maximumElementCharacterDataUTF8ByteCount: Int {
        if let activeCell {
            if activeCell.stringAssembler?.isInsideTextElement == true {
                return maximumRawStringTextUTF8ByteCount
            }
            if activeCell.collectingValue, activeCell.rawType == "str" {
                return maximumRawStringTextUTF8ByteCount
            }
        }
        return super.maximumElementCharacterDataUTF8ByteCount
    }

    init(
        part: String,
        xmlLimits: CanonicalXLSXInspectionLimits,
        stagingLimits: CanonicalXLSXWorksheetStagingLimits,
        styles: CanonicalXLSXStylesAvailability,
        sharedStrings: CanonicalXLSXSharedStringTable?
    ) {
        self.stagingLimits = stagingLimits
        self.styles = styles
        self.sharedStrings = sharedStrings
        self.maximumRawStringTextUTF8ByteCount =
            7 * stagingLimits.maximumDecodedCellUTF8ByteCount
        super.init(part: part, limits: xmlLimits)
    }

    override func handleStart(
        elementName: String,
        namespaceURI: String?,
        qualifiedName: String?,
        attributes: [String: String]
    ) throws {
        if let ignoredTopLevelDepth {
            guard depth > ignoredTopLevelDepth,
                  namespaceURI == worksheetSpreadsheetNamespace else {
                throw invalid(.invalidElementStructure, value: qualifiedName ?? elementName)
            }
            return
        }

        if let activeCell, activeCell.stringAssembler != nil, depth >= 6 {
            do {
                try activeCell.stringAssembler?.handleStart(
                    elementName: elementName,
                    namespaceURI: namespaceURI,
                    qualifiedName: qualifiedName,
                    attributes: attributes
                )
            } catch {
                throw mappedStringError(error, reference: activeCell.address.a1Reference)
            }
            return
        }

        switch depth {
        case 1:
            guard elementName == "worksheet",
                  namespaceURI == worksheetSpreadsheetNamespace else {
                throw invalid(.unexpectedRoot, value: namespaceURI)
            }
            try parseRootAttributes(attributes)
            sawRoot = true

        case 2:
            try startTopLevel(
                elementName: elementName,
                namespaceURI: namespaceURI,
                qualifiedName: qualifiedName,
                attributes: attributes
            )

        case 3:
            if insideColumns {
                try parseColumn(elementName, namespaceURI: namespaceURI, attributes: attributes)
            } else if insideSheetData {
                try startRow(elementName, namespaceURI: namespaceURI, attributes: attributes)
            } else if insideMergeCells {
                try parseMerge(elementName, namespaceURI: namespaceURI, attributes: attributes)
            } else {
                throw invalid(.invalidElementStructure, value: qualifiedName ?? elementName)
            }

        case 4:
            guard insideSheetData, activeRow != nil else {
                throw invalid(.invalidElementStructure, value: qualifiedName ?? elementName)
            }
            try startCell(elementName, namespaceURI: namespaceURI, attributes: attributes)

        case 5:
            try startCellChild(
                elementName,
                namespaceURI: namespaceURI,
                qualifiedName: qualifiedName,
                attributes: attributes
            )

        default:
            throw invalid(.invalidElementStructure, value: qualifiedName ?? elementName)
        }
    }

    override func handleEnd(
        elementName: String,
        namespaceURI: String?,
        qualifiedName: String?
    ) throws {
        if let ignoredTopLevelDepth {
            if depth == ignoredTopLevelDepth {
                self.ignoredTopLevelDepth = nil
            }
            return
        }

        if let activeCell, activeCell.stringAssembler != nil, depth >= 6 {
            do {
                try activeCell.stringAssembler?.handleEnd(
                    elementName: elementName,
                    namespaceURI: namespaceURI,
                    qualifiedName: qualifiedName
                )
            } catch {
                throw mappedStringError(error, reference: activeCell.address.a1Reference)
            }
            return
        }

        switch depth {
        case 5:
            guard let activeCell else {
                throw invalid(.invalidElementStructure, value: elementName)
            }
            if elementName == "v", namespaceURI == worksheetSpreadsheetNamespace {
                guard activeCell.collectingValue else {
                    throw invalid(
                        .invalidCellValueStructure,
                        reference: activeCell.address.a1Reference
                    )
                }
                activeCell.collectingValue = false
            } else if elementName == "is", namespaceURI == worksheetSpreadsheetNamespace {
                guard activeCell.sawInlineString,
                      let assembler = activeCell.stringAssembler else {
                    throw invalid(
                        .invalidCellValueStructure,
                        reference: activeCell.address.a1Reference
                    )
                }
                do {
                    activeCell.inlineString = try assembler.finish()
                } catch {
                    throw mappedStringError(error, reference: activeCell.address.a1Reference)
                }
                activeCell.stringAssembler = nil
            } else {
                throw invalid(.invalidElementStructure, value: qualifiedName ?? elementName)
            }

        case 4:
            guard elementName == "c", namespaceURI == worksheetSpreadsheetNamespace else {
                throw invalid(.invalidElementStructure, value: qualifiedName ?? elementName)
            }
            try finishCell()

        case 3:
            if insideSheetData {
                guard elementName == "row", namespaceURI == worksheetSpreadsheetNamespace,
                      let row = activeRow else {
                    throw invalid(.invalidElementStructure, value: qualifiedName ?? elementName)
                }
                rows.append(
                    CanonicalXLSXRowProvenance(
                        oneBasedRow: row.oneBasedRow,
                        hidden: row.hidden,
                        declaredStyleIndex: row.declaredStyleIndex,
                        customFormat: row.customFormat
                    )
                )
                activeRow = nil
            }

        case 2:
            if elementName == "cols", namespaceURI == worksheetSpreadsheetNamespace {
                insideColumns = false
            } else if elementName == "sheetData", namespaceURI == worksheetSpreadsheetNamespace {
                insideSheetData = false
            } else if elementName == "mergeCells", namespaceURI == worksheetSpreadsheetNamespace {
                insideMergeCells = false
            }

        default:
            break
        }
    }

    override func handleCharacters(_ string: String) throws {
        if ignoredTopLevelDepth != nil { return }
        if let activeCell {
            if let assembler = activeCell.stringAssembler {
                do {
                    try assembler.handleCharacters(string)
                } catch {
                    throw mappedStringError(error, reference: activeCell.address.a1Reference)
                }
                return
            }
            if activeCell.collectingValue {
                activeCell.valueText.append(string)
                return
            }
        }
        guard string.unicodeScalars.allSatisfy(isWorksheetXMLWhitespace) else {
            throw invalid(.unexpectedText)
        }
    }

    override func handleCDATA(_ data: Data) throws {
        if ignoredTopLevelDepth != nil { return }
        guard let activeCell else { throw invalid(.invalidCDATA) }
        if let assembler = activeCell.stringAssembler {
            do {
                try assembler.handleCDATA(data)
            } catch {
                throw mappedStringError(error, reference: activeCell.address.a1Reference)
            }
            return
        }
        guard activeCell.collectingValue,
              let string = String(data: data, encoding: .utf8) else {
            throw invalid(.invalidCDATA, reference: activeCell.address.a1Reference)
        }
        activeCell.valueText.append(string)
    }

    override func validateDocumentComplete() throws {
        guard sawRoot else { throw invalid(.unexpectedRoot) }
        guard ignoredTopLevelDepth == nil, !insideColumns, !insideSheetData,
              !insideMergeCells, activeRow == nil, activeCell == nil else {
            throw invalid(.invalidElementStructure)
        }
    }

    func makeWorksheet() throws -> ParsedCanonicalXLSXWorksheet {
        guard sawSheetData else { throw invalid(.missingSheetData) }
        guard maximumColumn > 0, maximumRow > 0 else {
            throw invalid(.zeroColumnLogicalTable)
        }
        if let declaredMergeCount, declaredMergeCount != mergeRanges.count {
            throw invalid(
                .declaredCountMismatch,
                value: "mergeCells:\(declaredMergeCount):\(mergeRanges.count)"
            )
        }
        if let declaredDimension,
           let outside = physicalCells.first(where: {
               !declaredDimension.contains($0.address)
           }) {
            throw invalid(
                .dimensionDoesNotContainCell,
                reference: outside.address.a1Reference,
                value: declaredDimension.a1Reference
            )
        }
        let origin = try parseCellAddress("A1")
        let bottomRight = CanonicalXLSXCellAddress(
            oneBasedColumn: maximumColumn,
            oneBasedRow: maximumRow,
            a1Reference: makeA1Reference(column: maximumColumn, row: maximumRow)
        )
        let rectangle = CanonicalXLSXCellRange(
            topLeft: origin,
            bottomRight: bottomRight,
            a1Reference: "A1:\(bottomRight.a1Reference)"
        )
        if let intersecting = mergeRanges.first(where: { $0.intersects(rectangle) }) {
            throw invalid(
                .mergeIntersectsTable,
                reference: intersecting.a1Reference,
                value: rectangle.a1Reference
            )
        }
        return ParsedCanonicalXLSXWorksheet(
            maximumColumn: maximumColumn,
            maximumRow: maximumRow,
            fixedOriginRectangle: rectangle,
            declaredDimension: declaredDimension,
            rows: rows,
            columnDefinitions: columnDefinitions,
            physicalCells: physicalCells,
            mergeRanges: mergeRanges,
            totalMaterializedTextUTF8ByteCount: totalMaterializedTextUTF8ByteCount
        )
    }

    private func parseRootAttributes(_ attributes: [String: String]) throws {
        var ignorableKey: String?
        for key in attributes.keys.sorted() {
            guard let prefix = attributePrefix(key),
                  attributeLocalName(key) == "Ignorable",
                  namespaceURI(forPrefix: prefix) == worksheetMarkupCompatibilityNamespace else {
                continue
            }
            guard ignorableKey == nil else {
                throw invalid(.invalidElementStructure, value: key)
            }
            ignorableKey = key
            let tokens = attributes[key, default: ""].split(whereSeparator: {
                $0 == " " || $0 == "\t" || $0 == "\n" || $0 == "\r"
            })
            guard !tokens.isEmpty else {
                throw invalid(.invalidElementStructure, value: attributes[key])
            }
            for token in tokens {
                let prefix = String(token)
                guard namespaceURI(forPrefix: prefix) != nil else {
                    throw invalid(.invalidElementStructure, value: prefix)
                }
                ignorablePrefixes.insert(prefix)
            }
        }
        for key in attributes.keys.sorted() {
            if key == ignorableKey { continue }
            guard let prefix = attributePrefix(key), ignorablePrefixes.contains(prefix),
                  namespaceURI(forPrefix: prefix) != nil else {
                throw invalid(.invalidElementStructure, value: key)
            }
        }
    }

    private func startTopLevel(
        elementName: String,
        namespaceURI: String?,
        qualifiedName: String?,
        attributes: [String: String]
    ) throws {
        guard sawRoot, namespaceURI == worksheetSpreadsheetNamespace,
              let order = worksheetTopLevelOrder[elementName] else {
            throw invalid(.invalidElementStructure, value: qualifiedName ?? elementName)
        }
        if rejectedWorksheetTopLevelElements.contains(elementName) {
            throw invalid(.unsupportedSection, value: elementName)
        }
        let permitsRepeat = elementName == "conditionalFormatting"
        if !permitsRepeat, !seenTopLevelElements.insert(elementName).inserted {
            throw invalid(.duplicateSection, value: elementName)
        }
        guard order > lastTopLevelOrder || (permitsRepeat && order == lastTopLevelOrder) else {
            throw invalid(.invalidSectionOrder, value: elementName)
        }
        lastTopLevelOrder = order

        switch elementName {
        case "dimension":
            guard attributes.count == 1, let reference = attributes["ref"] else {
                throw invalid(.missingRequiredAttribute, value: "dimension.ref")
            }
            declaredDimension = try parseRange(reference, permitsSingleCell: true)

        case "cols":
            guard attributes.isEmpty else {
                throw invalid(.invalidElementStructure, value: attributes.keys.sorted().first)
            }
            insideColumns = true

        case "sheetData":
            guard attributes.isEmpty else {
                throw invalid(.invalidElementStructure, value: attributes.keys.sorted().first)
            }
            sawSheetData = true
            insideSheetData = true

        case "mergeCells":
            if let unexpected = attributes.keys.sorted().first(where: { $0 != "count" }) {
                throw invalid(.invalidElementStructure, value: unexpected)
            }
            if let rawCount = attributes["count"] {
                declaredMergeCount = try parseNonnegativeInt(rawCount)
                guard declaredMergeCount! <= stagingLimits.maximumMergeRangeCount else {
                    throw CanonicalXLSXWorksheetStagingReaderError.mergeRangeLimitExceeded(
                        maximum: stagingLimits.maximumMergeRangeCount,
                        actual: declaredMergeCount!
                    )
                }
            }
            insideMergeCells = true

        default:
            guard ignoredWorksheetTopLevelElements.contains(elementName) else {
                throw invalid(.unsupportedSection, value: elementName)
            }
            ignoredTopLevelDepth = depth
        }
    }

    private func parseColumn(
        _ elementName: String,
        namespaceURI: String?,
        attributes: [String: String]
    ) throws {
        guard elementName == "col", namespaceURI == worksheetSpreadsheetNamespace else {
            throw invalid(.invalidElementStructure, value: elementName)
        }
        let allowed: Set<String> = [
            "min", "max", "width", "style", "hidden", "bestFit", "customWidth",
            "phonetic", "outlineLevel", "collapsed",
        ]
        if let unexpected = attributes.keys.sorted().first(where: { !allowed.contains($0) }) {
            throw invalid(.invalidElementStructure, value: unexpected)
        }
        guard let rawMinimum = attributes["min"], let rawMaximum = attributes["max"] else {
            throw invalid(.missingRequiredAttribute, value: "col.min/max")
        }
        let minimum = try parsePositiveInt(rawMinimum, maximum: 16_384)
        let maximum = try parsePositiveInt(rawMaximum, maximum: 16_384)
        guard minimum <= maximum else {
            throw invalid(.invalidUnsignedInteger, value: "\(rawMinimum):\(rawMaximum)")
        }
        if let previous = columnDefinitions.last,
           minimum <= previous.maximumOneBasedColumn {
            throw invalid(
                .overlappingColumnDefinitions,
                reference: "\(minimum):\(maximum)",
                value: "\(previous.minimumOneBasedColumn):\(previous.maximumOneBasedColumn)"
            )
        }
        let hidden = try attributes["hidden"].map(parseBoolean) ?? false
        let styleIndex = try attributes["style"].map(parseUInt32)
        if let styleIndex {
            _ = try resolveStyle(
                styleIndex,
                reference: "columns:\(minimum):\(maximum)"
            )
        }
        let proposedCount = columnDefinitions.count + 1
        guard proposedCount <= stagingLimits.maximumColumnDefinitionCount else {
            throw CanonicalXLSXWorksheetStagingReaderError.columnDefinitionLimitExceeded(
                maximum: stagingLimits.maximumColumnDefinitionCount,
                actual: proposedCount
            )
        }
        columnDefinitions.append(
            CanonicalXLSXColumnDefinitionProvenance(
                minimumOneBasedColumn: minimum,
                maximumOneBasedColumn: maximum,
                hidden: hidden,
                styleIndex: styleIndex
            )
        )
    }

    private func startRow(
        _ elementName: String,
        namespaceURI: String?,
        attributes: [String: String]
    ) throws {
        guard elementName == "row", namespaceURI == worksheetSpreadsheetNamespace,
              activeRow == nil else {
            throw invalid(.invalidElementStructure, value: elementName)
        }
        let allowed: Set<String> = [
            "r", "spans", "s", "customFormat", "ht", "hidden", "customHeight",
            "outlineLevel", "collapsed", "thickTop", "thickBot", "ph",
        ]
        if let unexpected = attributes.keys.sorted().first(where: {
            !allowed.contains($0) && !isIgnorableAttribute($0)
        }) {
            throw invalid(.invalidElementStructure, value: unexpected)
        }
        guard let rawRow = attributes["r"] else {
            throw invalid(.missingRequiredAttribute, value: "row.r")
        }
        let row = try parsePositiveInt(rawRow, maximum: 1_048_576)
        if let lastRowIndex, row <= lastRowIndex {
            throw invalid(.nonascendingRows, reference: rawRow, value: String(lastRowIndex))
        }
        let proposedCount = rows.count + 1
        guard proposedCount <= stagingLimits.maximumPhysicalRowCount else {
            throw CanonicalXLSXWorksheetStagingReaderError.physicalRowLimitExceeded(
                maximum: stagingLimits.maximumPhysicalRowCount,
                actual: proposedCount,
                reference: rawRow
            )
        }
        let hidden = try attributes["hidden"].map(parseBoolean) ?? false
        let styleIndex = try attributes["s"].map(parseUInt32)
        let customFormat = try attributes["customFormat"].map(parseBoolean) ?? false
        if customFormat, styleIndex == nil {
            throw invalid(.missingRequiredAttribute, reference: "row:\(row)", value: "row.s")
        }
        if let styleIndex {
            _ = try resolveStyle(styleIndex, reference: "row:\(row)")
        }
        activeRow = ActiveWorksheetRow(
            oneBasedRow: row,
            hidden: hidden,
            declaredStyleIndex: styleIndex,
            customFormat: customFormat,
            lastCellColumn: nil
        )
        lastRowIndex = row
    }

    private func startCell(
        _ elementName: String,
        namespaceURI: String?,
        attributes: [String: String]
    ) throws {
        guard elementName == "c", namespaceURI == worksheetSpreadsheetNamespace,
              activeCell == nil, var row = activeRow else {
            throw invalid(.invalidElementStructure, value: elementName)
        }
        let allowed: Set<String> = ["r", "s", "t", "ph"]
        if let unexpected = attributes.keys.sorted().first(where: {
            !allowed.contains($0) && !isIgnorableAttribute($0)
        }) {
            throw invalid(.invalidElementStructure, value: unexpected)
        }
        guard let rawReference = attributes["r"] else {
            throw invalid(.missingRequiredAttribute, value: "c.r")
        }
        let address = try parseCellAddress(rawReference)
        guard address.oneBasedRow == row.oneBasedRow else {
            throw invalid(
                .cellRowMismatch,
                reference: rawReference,
                value: String(row.oneBasedRow)
            )
        }
        if let lastColumn = row.lastCellColumn, address.oneBasedColumn <= lastColumn {
            throw invalid(
                .nonascendingCells,
                reference: rawReference,
                value: String(lastColumn)
            )
        }
        guard address.oneBasedColumn <= stagingLimits.maximumColumnCount else {
            throw CanonicalXLSXWorksheetStagingReaderError.columnLimitExceeded(
                maximum: stagingLimits.maximumColumnCount,
                actual: address.oneBasedColumn,
                reference: rawReference
            )
        }
        let proposedPhysicalCount = physicalCells.count + 1
        guard proposedPhysicalCount <= stagingLimits.maximumPhysicalCellCount else {
            throw CanonicalXLSXWorksheetStagingReaderError.physicalCellLimitExceeded(
                maximum: stagingLimits.maximumPhysicalCellCount,
                actual: proposedPhysicalCount,
                reference: rawReference
            )
        }
        if let rawType = attributes["t"],
           !["n", "s", "inlineStr", "str"].contains(rawType) {
            throw invalid(.unsupportedCellType, reference: rawReference, value: rawType)
        }
        if let rawPhonetic = attributes["ph"] { _ = try parseBoolean(rawPhonetic) }
        let explicitStyle = try attributes["s"].map(parseUInt32)
        let (effectiveStyle, styleSource) = effectiveStyle(
            explicitCellStyle: explicitStyle,
            row: row,
            column: address.oneBasedColumn
        )
        let format = try resolveStyle(effectiveStyle, reference: rawReference)

        row.lastCellColumn = address.oneBasedColumn
        activeRow = row
        maximumColumn = max(maximumColumn, address.oneBasedColumn)
        maximumRow = max(maximumRow, address.oneBasedRow)
        activeCell = ActiveWorksheetCell(
            address: address,
            rawType: attributes["t"],
            explicitStyleIndex: explicitStyle,
            effectiveStyleIndex: effectiveStyle,
            effectiveStyleSource: styleSource,
            effectiveFormat: format
        )
    }

    private func startCellChild(
        _ elementName: String,
        namespaceURI: String?,
        qualifiedName: String?,
        attributes: [String: String]
    ) throws {
        guard let activeCell, namespaceURI == worksheetSpreadsheetNamespace else {
            throw invalid(.invalidElementStructure, value: qualifiedName ?? elementName)
        }
        switch elementName {
        case "f":
            throw invalid(.formulaUnsupported, reference: activeCell.address.a1Reference)
        case "v":
            guard attributes.isEmpty, !activeCell.sawValue, !activeCell.sawInlineString else {
                throw invalid(
                    .invalidCellValueStructure,
                    reference: activeCell.address.a1Reference
                )
            }
            activeCell.sawValue = true
            activeCell.collectingValue = true
            activeCell.valueText.removeAll(keepingCapacity: true)
        case "is":
            guard attributes.isEmpty, !activeCell.sawValue, !activeCell.sawInlineString,
                  activeCell.rawType == "inlineStr" else {
                throw invalid(
                    .invalidCellValueStructure,
                    reference: activeCell.address.a1Reference
                )
            }
            activeCell.sawInlineString = true
            activeCell.stringAssembler = CanonicalXLSXStringItemAssembler(
                part: part,
                context: .inlineString(cellReference: activeCell.address.a1Reference),
                maximumRawTextNodeUTF8ByteCount: maximumRawStringTextUTF8ByteCount,
                maximumDecodedStringUTF8ByteCount:
                    stagingLimits.maximumDecodedCellUTF8ByteCount
            )
        default:
            throw invalid(.invalidCellValueStructure, reference: activeCell.address.a1Reference)
        }
    }

    private func finishCell() throws {
        guard let activeCell else { throw invalid(.invalidElementStructure, value: "c") }
        let kind: CanonicalXLSXPhysicalCellKind
        switch activeCell.rawType {
        case nil, "n":
            guard !activeCell.sawInlineString else {
                throw invalid(
                    .invalidCellValueStructure,
                    reference: activeCell.address.a1Reference
                )
            }
            if activeCell.sawValue {
                guard !activeCell.valueText.isEmpty else {
                    throw invalid(
                        .invalidNumericLexeme,
                        reference: activeCell.address.a1Reference
                    )
                }
                do {
                    try SpreadsheetNumericTimestampCodec.validateStoredNumberLexeme(
                        activeCell.valueText
                    )
                } catch {
                    throw invalid(
                        .invalidNumericLexeme,
                        reference: activeCell.address.a1Reference
                    )
                }
                do {
                    _ = try styles.requireTimestampSafeCellFormat(
                        explicitStyleIndex: activeCell.effectiveStyleIndex
                    )
                } catch let error as CanonicalXLSXStyleResolutionError {
                    throw mappedStyleError(
                        error,
                        reference: activeCell.address.a1Reference
                    )
                }
                kind = .number(rawLexeme: activeCell.valueText)
            } else {
                kind = .blank
            }

        case "str":
            guard activeCell.sawValue, !activeCell.sawInlineString else {
                throw invalid(
                    .invalidCellValueStructure,
                    reference: activeCell.address.a1Reference
                )
            }
            let decoded: String
            do {
                decoded = try CanonicalXLSXStringDecoder.decodeTextNode(
                    activeCell.valueText,
                    maximumRawUTF8ByteCount: maximumRawStringTextUTF8ByteCount,
                    maximumDecodedUTF8ByteCount:
                        stagingLimits.maximumDecodedCellUTF8ByteCount
                )
            } catch {
                throw mappedStringError(error, reference: activeCell.address.a1Reference)
            }
            kind = .directString(rawLexeme: activeCell.valueText, decodedText: decoded)

        case "s":
            guard activeCell.sawValue, !activeCell.sawInlineString else {
                throw invalid(
                    .invalidCellValueStructure,
                    reference: activeCell.address.a1Reference
                )
            }
            let index = try parseUInt32(activeCell.valueText)
            guard let sharedStrings else {
                throw invalid(
                    .sharedStringsRequired,
                    reference: activeCell.address.a1Reference
                )
            }
            let value: String
            do {
                value = try sharedStrings.value(at: index)
            } catch {
                throw invalid(
                    .sharedStringIndexOutOfBounds,
                    reference: activeCell.address.a1Reference,
                    value: "\(index):\(sharedStrings.values.count)"
                )
            }
            guard value.utf8.count <= stagingLimits.maximumDecodedCellUTF8ByteCount else {
                throw CanonicalXLSXWorksheetStagingReaderError.decodedCellByteLimitExceeded(
                    reference: activeCell.address.a1Reference,
                    maximum: stagingLimits.maximumDecodedCellUTF8ByteCount,
                    actual: value.utf8.count
                )
            }
            kind = .sharedString(index: index, decodedText: value)

        case "inlineStr":
            guard activeCell.sawInlineString, !activeCell.sawValue,
                  let value = activeCell.inlineString else {
                throw invalid(
                    .invalidCellValueStructure,
                    reference: activeCell.address.a1Reference
                )
            }
            kind = .inlineString(decodedText: value)

        default:
            throw invalid(
                .unsupportedCellType,
                reference: activeCell.address.a1Reference,
                value: activeCell.rawType
            )
        }

        let byteCount = kind.materializedTextUTF8ByteCount
        if byteCount > stagingLimits.maximumDecodedCellUTF8ByteCount {
            throw CanonicalXLSXWorksheetStagingReaderError.decodedCellByteLimitExceeded(
                reference: activeCell.address.a1Reference,
                maximum: stagingLimits.maximumDecodedCellUTF8ByteCount,
                actual: byteCount
            )
        }
        let (proposedTotal, overflow) = totalMaterializedTextUTF8ByteCount
            .addingReportingOverflow(byteCount)
        guard !overflow,
              proposedTotal <= stagingLimits.maximumTotalMaterializedTextUTF8ByteCount else {
            throw CanonicalXLSXWorksheetStagingReaderError
                .totalMaterializedTextByteLimitExceeded(
                    reference: activeCell.address.a1Reference,
                    maximum: stagingLimits.maximumTotalMaterializedTextUTF8ByteCount,
                    actual: overflow ? Int.max : proposedTotal
                )
        }
        totalMaterializedTextUTF8ByteCount = proposedTotal
        physicalCells.append(
            CanonicalXLSXPhysicalCellProvenance(
                address: activeCell.address,
                rawCellTypeAttribute: activeCell.rawType,
                explicitStyleIndex: activeCell.explicitStyleIndex,
                effectiveStyleIndex: activeCell.effectiveStyleIndex,
                effectiveStyleSource: activeCell.effectiveStyleSource,
                numberFormatID: activeCell.effectiveFormat.numberFormatID,
                numberFormatDisposition: provenanceDisposition(
                    activeCell.effectiveFormat.disposition
                ),
                kind: kind
            )
        )
        self.activeCell = nil
    }

    private func parseMerge(
        _ elementName: String,
        namespaceURI: String?,
        attributes: [String: String]
    ) throws {
        guard elementName == "mergeCell", namespaceURI == worksheetSpreadsheetNamespace,
              attributes.count == 1, let rawReference = attributes["ref"] else {
            throw invalid(.invalidElementStructure, value: elementName)
        }
        let range = try parseRange(rawReference, permitsSingleCell: false)
        guard mergeRangeSet.insert(range).inserted else {
            throw invalid(.duplicateMergeRange, reference: rawReference)
        }
        let proposedCount = mergeRanges.count + 1
        guard proposedCount <= stagingLimits.maximumMergeRangeCount else {
            throw CanonicalXLSXWorksheetStagingReaderError.mergeRangeLimitExceeded(
                maximum: stagingLimits.maximumMergeRangeCount,
                actual: proposedCount
            )
        }
        mergeRanges.append(range)
    }

    private func effectiveStyle(
        explicitCellStyle: UInt32?,
        row: ActiveWorksheetRow,
        column: Int
    ) -> (UInt32?, CanonicalXLSXEffectiveStyleSource) {
        if let explicitCellStyle { return (explicitCellStyle, .cell) }
        if row.customFormat, let rowStyle = row.declaredStyleIndex {
            return (rowStyle, .rowDefault)
        }
        if let columnStyle = columnDefinition(for: column)?.styleIndex {
            return (columnStyle, .columnDefault)
        }
        switch styles {
        case .absent: return (nil, .workbookDefault)
        case .present: return (0, .workbookDefault)
        }
    }

    private func columnDefinition(
        for column: Int
    ) -> CanonicalXLSXColumnDefinitionProvenance? {
        var lower = 0
        var upper = columnDefinitions.count
        while lower < upper {
            let middle = lower + (upper - lower) / 2
            let candidate = columnDefinitions[middle]
            if column < candidate.minimumOneBasedColumn {
                upper = middle
            } else if column > candidate.maximumOneBasedColumn {
                lower = middle + 1
            } else {
                return candidate
            }
        }
        return nil
    }

    private func resolveStyle(
        _ styleIndex: UInt32?,
        reference: String
    ) throws -> CanonicalXLSXCellFormat {
        switch styles {
        case .absent:
            guard styleIndex == nil else {
                throw invalid(
                    .styleIndexRequiresStylesPart,
                    reference: reference,
                    value: styleIndex.map(String.init)
                )
            }
            return CanonicalXLSXCellFormat(
                numberFormatID: 0,
                disposition: .provenNonDate
            )
        case .present(let catalog):
            let index = styleIndex ?? 0
            guard catalog.cellFormats.indices.contains(Int(index)) else {
                throw invalid(
                    .styleIndexOutOfBounds,
                    reference: reference,
                    value: "\(index):\(catalog.cellFormats.count)"
                )
            }
            return catalog.cellFormats[Int(index)]
        }
    }

    private func mappedStyleError(
        _ error: CanonicalXLSXStyleResolutionError,
        reference: String
    ) -> CanonicalXLSXWorksheetStagingReaderError {
        switch error {
        case .styleIndexRequiresStylesPart(let index):
            return invalid(
                .styleIndexRequiresStylesPart,
                reference: reference,
                value: String(index)
            )
        case .cellStyleIndexOutOfBounds(let index, let count):
            return invalid(
                .styleIndexOutOfBounds,
                reference: reference,
                value: "\(index):\(count)"
            )
        case .dateOrTimeNumberFormat(let styleIndex, let numberFormatID):
            return invalid(
                .dateOrTimeNumberFormat,
                reference: reference,
                value: "\(styleIndex):\(numberFormatID)"
            )
        case .unprovenNumberFormat(let styleIndex, let numberFormatID):
            return invalid(
                .unprovenNumberFormat,
                reference: reference,
                value: "\(styleIndex):\(numberFormatID)"
            )
        }
    }

    private func mappedStringError(
        _ error: Error,
        reference: String
    ) -> CanonicalXLSXWorksheetStagingReaderError {
        if let decoding = error as? CanonicalXLSXStringDecodingError {
            switch decoding {
            case .decodedTextLimitExceeded(let maximum, let actual):
                return .decodedCellByteLimitExceeded(
                    reference: reference,
                    maximum: maximum,
                    actual: actual
                )
            case .decodedTextByteCountOverflow(let maximum):
                return .decodedCellByteLimitExceeded(
                    reference: reference,
                    maximum: maximum,
                    actual: Int.max
                )
            default:
                return invalid(.invalidString, reference: reference)
            }
        }
        if let assembly = error as? CanonicalXLSXSharedStringParsingError {
            switch assembly {
            case .invalidStringText(_, _, _, let decoding):
                return mappedStringError(decoding, reference: reference)
            case .invalidStringItem(_, _, .decodedItemLimitExceeded, let value):
                let components = value?.split(separator: ":", omittingEmptySubsequences: false)
                let actual = components?.last.flatMap { Int($0) } ?? Int.max
                return .decodedCellByteLimitExceeded(
                    reference: reference,
                    maximum: stagingLimits.maximumDecodedCellUTF8ByteCount,
                    actual: actual
                )
            default:
                return invalid(.invalidString, reference: reference)
            }
        }
        return invalid(.invalidString, reference: reference)
    }

    private func parseRange(
        _ raw: String,
        permitsSingleCell: Bool
    ) throws -> CanonicalXLSXCellRange {
        let components = raw.split(separator: ":", omittingEmptySubsequences: false)
        if components.count == 1, permitsSingleCell {
            let address: CanonicalXLSXCellAddress
            do {
                address = try parseCanonicalCellAddress(String(components[0]))
            } catch {
                throw invalid(.invalidRangeReference, reference: raw)
            }
            return CanonicalXLSXCellRange(
                topLeft: address,
                bottomRight: address,
                a1Reference: raw
            )
        }
        guard components.count == 2 else {
            throw invalid(.invalidRangeReference, reference: raw)
        }
        let topLeft: CanonicalXLSXCellAddress
        let bottomRight: CanonicalXLSXCellAddress
        do {
            topLeft = try parseCanonicalCellAddress(String(components[0]))
            bottomRight = try parseCanonicalCellAddress(String(components[1]))
        } catch {
            throw invalid(.invalidRangeReference, reference: raw)
        }
        guard topLeft.oneBasedColumn <= bottomRight.oneBasedColumn,
              topLeft.oneBasedRow <= bottomRight.oneBasedRow else {
            throw invalid(.invalidRangeReference, reference: raw)
        }
        if !permitsSingleCell,
           topLeft.oneBasedColumn == bottomRight.oneBasedColumn,
           topLeft.oneBasedRow == bottomRight.oneBasedRow {
            throw invalid(.mergeMustSpanMultipleCells, reference: raw)
        }
        return CanonicalXLSXCellRange(
            topLeft: topLeft,
            bottomRight: bottomRight,
            a1Reference: raw
        )
    }

    private func parseCellAddress(_ raw: String) throws -> CanonicalXLSXCellAddress {
        do {
            return try parseCanonicalCellAddress(raw)
        } catch {
            throw invalid(.invalidCellReference, reference: raw)
        }
    }

    private func parseNonnegativeInt(_ raw: String) throws -> Int {
        guard !raw.isEmpty, raw.utf8.allSatisfy({ (48...57).contains($0) }),
              let value = Int(raw) else {
            throw invalid(.invalidUnsignedInteger, value: raw)
        }
        return value
    }

    private func parsePositiveInt(_ raw: String, maximum: Int) throws -> Int {
        let value = try parseNonnegativeInt(raw)
        guard value > 0, value <= maximum, String(value) == raw else {
            throw invalid(.invalidUnsignedInteger, value: raw)
        }
        return value
    }

    private func parseUInt32(_ raw: String) throws -> UInt32 {
        guard !raw.isEmpty, raw.utf8.allSatisfy({ (48...57).contains($0) }),
              let value = UInt32(raw) else {
            throw invalid(.invalidUnsignedInteger, value: raw)
        }
        return value
    }

    private func parseBoolean(_ raw: String) throws -> Bool {
        switch raw {
        case "1", "true": return true
        case "0", "false": return false
        default: throw invalid(.invalidBoolean, value: raw)
        }
    }

    private func isIgnorableAttribute(_ qualifiedName: String) -> Bool {
        guard let prefix = attributePrefix(qualifiedName), ignorablePrefixes.contains(prefix) else {
            return false
        }
        return namespaceURI(forPrefix: prefix) != nil
    }

    private func invalid(
        _ issue: CanonicalXLSXWorksheetIssue,
        reference: String? = nil,
        value: String? = nil
    ) -> CanonicalXLSXWorksheetStagingReaderError {
        worksheetError(part, issue, reference: reference, value: value)
    }
}

private enum CanonicalXLSXCellAddressParsingError: Error {
    case invalid
}

private func parseCanonicalCellAddress(
    _ raw: String
) throws -> CanonicalXLSXCellAddress {
    let bytes = Array(raw.utf8)
    guard !bytes.isEmpty else { throw CanonicalXLSXCellAddressParsingError.invalid }
    var index = 0
    var column = 0
    while index < bytes.count, (65...90).contains(bytes[index]) {
        let (times, multiplyOverflow) = column.multipliedReportingOverflow(by: 26)
        let (next, addOverflow) = times.addingReportingOverflow(Int(bytes[index] - 64))
        guard !multiplyOverflow, !addOverflow, next <= 16_384 else {
            throw CanonicalXLSXCellAddressParsingError.invalid
        }
        column = next
        index += 1
    }
    guard column > 0, index < bytes.count, bytes[index] != 48 else {
        throw CanonicalXLSXCellAddressParsingError.invalid
    }
    var row = 0
    while index < bytes.count, (48...57).contains(bytes[index]) {
        let (times, multiplyOverflow) = row.multipliedReportingOverflow(by: 10)
        let (next, addOverflow) = times.addingReportingOverflow(Int(bytes[index] - 48))
        guard !multiplyOverflow, !addOverflow, next <= 1_048_576 else {
            throw CanonicalXLSXCellAddressParsingError.invalid
        }
        row = next
        index += 1
    }
    guard row > 0, index == bytes.count,
          makeA1Reference(column: column, row: row) == raw else {
        throw CanonicalXLSXCellAddressParsingError.invalid
    }
    return CanonicalXLSXCellAddress(
        oneBasedColumn: column,
        oneBasedRow: row,
        a1Reference: raw
    )
}

private func makeA1Reference(column: Int, row: Int) -> String {
    var value = column
    var letters: [UInt8] = []
    while value > 0 {
        value -= 1
        letters.append(UInt8(value % 26) + 65)
        value /= 26
    }
    return String(decoding: letters.reversed(), as: UTF8.self) + String(row)
}

private func provenanceDisposition(
    _ disposition: CanonicalXLSXNumberFormatDisposition
) -> CanonicalXLSXProvenanceNumberFormatDisposition {
    switch disposition {
    case .provenNonDate: return .provenNonDate
    case .dateOrTime: return .dateOrTime
    case .unsupported: return .unsupported
    }
}

private func attributePrefix(_ qualifiedName: String) -> String? {
    guard let separator = qualifiedName.firstIndex(of: ":") else { return nil }
    return String(qualifiedName[..<separator])
}

private func attributeLocalName(_ qualifiedName: String) -> Substring {
    guard let separator = qualifiedName.firstIndex(of: ":") else {
        return qualifiedName[...]
    }
    return qualifiedName[qualifiedName.index(after: separator)...]
}

private func isWorksheetXMLWhitespace(_ scalar: Unicode.Scalar) -> Bool {
    scalar.value == 0x09 || scalar.value == 0x0A
        || scalar.value == 0x0D || scalar.value == 0x20
}

private func worksheetError(
    _ part: String,
    _ issue: CanonicalXLSXWorksheetIssue,
    reference: String? = nil,
    value: String? = nil
) -> CanonicalXLSXWorksheetStagingReaderError {
    .invalidWorksheet(
        part: part,
        issue: issue,
        reference: reference,
        value: value
    )
}
