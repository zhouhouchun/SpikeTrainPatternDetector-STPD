import Foundation

public enum StagedSourceReferenceError: Error, Equatable, Sendable {
    case columnIndexMustBePositive
    case dataRowIndexMustBePositive
}

/// A one-based physical column reference used only while reviewing an imported source.
///
/// Source positions are provenance. They must never be substituted for a scientific semantic ID.
public struct StagedSourceColumnReference: Hashable, Comparable, Sendable {
    public let oneBasedIndex: Int

    public init(oneBasedIndex: Int) throws {
        guard oneBasedIndex > 0 else {
            throw StagedSourceReferenceError.columnIndexMustBePositive
        }
        self.oneBasedIndex = oneBasedIndex
    }

    public static func < (
        lhs: StagedSourceColumnReference,
        rhs: StagedSourceColumnReference
    ) -> Bool {
        lhs.oneBasedIndex < rhs.oneBasedIndex
    }
}

/// A logical data-row position within the selected table. The header, when present, is not a data
/// row. Source readers retain any additional physical-line or workbook provenance separately.
public struct StagedSourceCellReference: Hashable, Sendable {
    public let column: StagedSourceColumnReference
    public let oneBasedDataRowIndex: Int

    public init(
        column: StagedSourceColumnReference,
        oneBasedDataRowIndex: Int
    ) throws {
        guard oneBasedDataRowIndex > 0 else {
            throw StagedSourceReferenceError.dataRowIndexMustBePositive
        }
        self.column = column
        self.oneBasedDataRowIndex = oneBasedDataRowIndex
    }
}

/// A source occurrence selection used only while a manifest is being prepared. Its source position
/// is not the occurrence's eventual scientific identity.
public struct StagedEventOccurrenceReference: Hashable, Sendable {
    public let timestampCell: StagedSourceCellReference

    public init(timestampCell: StagedSourceCellReference) {
        self.timestampCell = timestampCell
    }
}

public enum StagedTabularSource: Hashable, Sendable {
    case commaSeparatedValues
    case excelWorkbook(worksheetName: String)
}

/// A source cell before any scientific role, timestamp unit, or attribute type is confirmed.
///
/// `.blank` is deliberately distinct from a text cell containing an empty string. Workbook numbers
/// retain their raw OOXML lexeme; no binary floating-point value belongs in staging.
public enum StagedCellValue: Hashable, Sendable {
    case blank
    case text(rawText: String)
    case spreadsheetNumber(rawLexeme: String)
}

public struct StagedScientificColumn: Hashable, Sendable {
    public let sourceColumn: StagedSourceColumnReference
    /// `nil` means the source was imported without a header row. An empty string is a present,
    /// empty header and remains distinguishable from a headerless source.
    public let header: String?
    /// Cells are in logical source-row order and preserve blanks, duplicate values, and raw text.
    public let cells: [StagedCellValue]

    public init(
        sourceColumn: StagedSourceColumnReference,
        header: String?,
        cells: [StagedCellValue]
    ) {
        self.sourceColumn = sourceColumn
        self.header = header
        self.cells = cells
    }
}

public enum StagedColumnKindSuggestion: String, Hashable, Sendable {
    case spikeTrain
    case eventDefinition
}

/// A parser or UI may suggest an interpretation, but none of these strings or roles is authoritative.
public struct StagedColumnSuggestion: Hashable, Sendable {
    public let sourceColumn: StagedSourceColumnReference
    public let kind: StagedColumnKindSuggestion
    public let suggestedSemanticIDText: String?
    public let suggestedEventTypeIDText: String?

    public init(
        sourceColumn: StagedSourceColumnReference,
        kind: StagedColumnKindSuggestion,
        suggestedSemanticIDText: String?,
        suggestedEventTypeIDText: String?
    ) {
        self.sourceColumn = sourceColumn
        self.kind = kind
        self.suggestedSemanticIDText = suggestedSemanticIDText
        self.suggestedEventTypeIDText = suggestedEventTypeIDText
    }
}

/// A non-authoritative proposed contiguous grouping. Confirmation is represented only in the
/// manifest draft, never by this suggestion.
public struct StagedEventScopeGroupSuggestion: Hashable, Sendable {
    public let sourceColumns: [StagedSourceColumnReference]
    public let suggestedSemanticIDText: String?

    public init(
        sourceColumns: [StagedSourceColumnReference],
        suggestedSemanticIDText: String?
    ) {
        self.sourceColumns = sourceColumns
        self.suggestedSemanticIDText = suggestedSemanticIDText
    }
}

public struct StagedScientificImportSuggestions: Hashable, Sendable {
    public let columns: [StagedColumnSuggestion]
    public let eventScopeGroups: [StagedEventScopeGroupSuggestion]

    public init(
        columns: [StagedColumnSuggestion],
        eventScopeGroups: [StagedEventScopeGroupSuggestion]
    ) {
        self.columns = columns
        self.eventScopeGroups = eventScopeGroups
    }

    public static let none = StagedScientificImportSuggestions(
        columns: [],
        eventScopeGroups: []
    )
}

public enum StagedScientificImportStructureError: Error, Equatable, Sendable {
    case noColumns
    case nonSequentialColumnReference(expected: Int, actual: Int)
    case inconsistentDataRowCount(column: Int, expected: Int, actual: Int)
    case mixedHeaderPresence
    case suggestionReferencesMissingColumn(column: Int)
    case emptyEventScopeGroupSuggestion(group: Int)
    case noncontiguousEventScopeGroupSuggestion(group: Int, expected: Int, actual: Int)
    case duplicateEventScopeGroupSuggestionColumn(column: Int)
    case eventScopeGroupSuggestionsOutOfSourceOrder(previous: Int, actual: Int)
}

/// A bounded reader's format-neutral, lossless table output.
///
/// This type contains source facts and suggestions only. It deliberately has no activity mode,
/// timestamp unit, canonical tick, typed event attribute, scientific digest, or confirmation API.
public struct StagedScientificImport: Hashable, Sendable {
    public let source: StagedTabularSource
    public let columns: [StagedScientificColumn]
    public let dataRowCount: Int
    public let suggestions: StagedScientificImportSuggestions

    public init(
        source: StagedTabularSource,
        columns: [StagedScientificColumn],
        suggestions: StagedScientificImportSuggestions
    ) throws {
        guard let firstColumn = columns.first else {
            throw StagedScientificImportStructureError.noColumns
        }

        let expectedRowCount = firstColumn.cells.count
        let sourceHasHeaders = firstColumn.header != nil
        for (offset, column) in columns.enumerated() {
            let expectedColumnIndex = offset + 1
            guard column.sourceColumn.oneBasedIndex == expectedColumnIndex else {
                throw StagedScientificImportStructureError.nonSequentialColumnReference(
                    expected: expectedColumnIndex,
                    actual: column.sourceColumn.oneBasedIndex
                )
            }
            guard column.cells.count == expectedRowCount else {
                throw StagedScientificImportStructureError.inconsistentDataRowCount(
                    column: column.sourceColumn.oneBasedIndex,
                    expected: expectedRowCount,
                    actual: column.cells.count
                )
            }
            guard (column.header != nil) == sourceHasHeaders else {
                throw StagedScientificImportStructureError.mixedHeaderPresence
            }
        }

        let availableColumnRange = 1...columns.count
        for suggestion in suggestions.columns {
            let index = suggestion.sourceColumn.oneBasedIndex
            guard availableColumnRange.contains(index) else {
                throw StagedScientificImportStructureError
                    .suggestionReferencesMissingColumn(column: index)
            }
        }

        var usedGroupColumns = Set<Int>()
        var previousGroupLastColumn: Int?
        for (groupOffset, group) in suggestions.eventScopeGroups.enumerated() {
            let groupNumber = groupOffset + 1
            guard let firstReference = group.sourceColumns.first else {
                throw StagedScientificImportStructureError
                    .emptyEventScopeGroupSuggestion(group: groupNumber)
            }
            let firstIndex = firstReference.oneBasedIndex
            for (columnOffset, reference) in group.sourceColumns.enumerated() {
                let actual = reference.oneBasedIndex
                guard availableColumnRange.contains(actual) else {
                    throw StagedScientificImportStructureError
                        .suggestionReferencesMissingColumn(column: actual)
                }
                guard usedGroupColumns.insert(actual).inserted else {
                    throw StagedScientificImportStructureError
                        .duplicateEventScopeGroupSuggestionColumn(column: actual)
                }
                let expected = firstIndex + columnOffset
                guard actual == expected else {
                    throw StagedScientificImportStructureError
                        .noncontiguousEventScopeGroupSuggestion(
                            group: groupNumber,
                            expected: expected,
                            actual: actual
                        )
                }
            }
            if let previousGroupLastColumn,
               firstIndex <= previousGroupLastColumn {
                throw StagedScientificImportStructureError
                    .eventScopeGroupSuggestionsOutOfSourceOrder(
                        previous: previousGroupLastColumn,
                        actual: firstIndex
                    )
            }
            previousGroupLastColumn = group.sourceColumns.last?.oneBasedIndex
        }

        self.source = source
        self.columns = columns
        self.dataRowCount = expectedRowCount
        self.suggestions = suggestions
    }

    public func cell(at reference: StagedSourceCellReference) -> StagedCellValue? {
        let columnOffset = reference.column.oneBasedIndex - 1
        let rowOffset = reference.oneBasedDataRowIndex - 1
        guard columns.indices.contains(columnOffset),
              columns[columnOffset].cells.indices.contains(rowOffset) else {
            return nil
        }
        return columns[columnOffset].cells[rowOffset]
    }
}
