/// Shared lexical scanning for scientific-import source columns.
///
/// This is deliberately internal. `ScientificImportNormalizer` and the public read-only preflight
/// service both consume these facts so the UI never grows a second timestamp/event-metadata grammar.
enum ScientificImportSourceScanIssue: Equatable {
    case spreadsheetNumberOutsideWorkbook(cell: StagedSourceCellReference)
    case spreadsheetNumberTimestampDecodeFailed(
        cell: StagedSourceCellReference,
        error: SpreadsheetNumericTimestampDecodeError
    )
    case timestampParseFailed(
        cell: StagedSourceCellReference,
        error: ExactTimestampParseError
    )
    case eventMetadataInSpikeTrain(cell: StagedSourceCellReference)
    case eventMetadataWithoutOccurrence(cell: StagedSourceCellReference)
    case eventMetadataMissingEquals(cell: StagedSourceCellReference)
    case invalidEventAttributeKey(
        cell: StagedSourceCellReference,
        error: EventAttributeKeyError
    )
    case internalInvariant
}

enum ScientificImportScannedMetadata: Equatable {
    case attribute(
        key: EventAttributeKey,
        rawValue: String,
        sourceCell: StagedSourceCellReference
    )
    case unitSuggestion(
        key: EventAttributeKey,
        rawValue: String,
        sourceCell: StagedSourceCellReference
    )

    var key: EventAttributeKey {
        switch self {
        case .attribute(let key, _, _), .unitSuggestion(let key, _, _):
            return key
        }
    }

    var sourceCell: StagedSourceCellReference {
        switch self {
        case .attribute(_, _, let sourceCell), .unitSuggestion(_, _, let sourceCell):
            return sourceCell
        }
    }
}

struct ScientificImportScannedTimestamp: Equatable {
    let tick: MicrosecondTick
    let sourceCell: StagedSourceCellReference
}

struct ScientificImportScannedEventOccurrence: Equatable {
    let sourceTick: MicrosecondTick
    let timestampCell: StagedSourceCellReference
    var metadata: [ScientificImportScannedMetadata]
}

struct ScientificImportSpikeColumnScan: Equatable {
    let timestamps: [ScientificImportScannedTimestamp]
}

struct ScientificImportEventColumnScan: Equatable {
    let occurrences: [ScientificImportScannedEventOccurrence]
}

enum ScientificImportSourceScanner {
    static func scanSpikeColumn(
        _ column: StagedScientificColumn,
        source: StagedTabularSource,
        sourceTimeUnit: SpikeTimeUnit,
        onIssue: (ScientificImportSourceScanIssue) -> Void
    ) -> ScientificImportSpikeColumnScan {
        var timestamps: [ScientificImportScannedTimestamp] = []
        timestamps.reserveCapacity(column.cells.count)

        for (rowOffset, value) in column.cells.enumerated() {
            guard let cell = sourceCell(
                column: column.sourceColumn,
                oneBasedRow: rowOffset + 1
            ) else {
                onIssue(.internalInvariant)
                continue
            }

            switch value {
            case .blank:
                continue
            case .spreadsheetNumber(let rawLexeme):
                guard case .excelWorkbook = source else {
                    onIssue(.spreadsheetNumberOutsideWorkbook(cell: cell))
                    continue
                }
                if let tick = decodeSpreadsheetTimestamp(
                    rawLexeme,
                    sourceTimeUnit: sourceTimeUnit,
                    cell: cell,
                    onIssue: onIssue
                ) {
                    timestamps.append(
                        ScientificImportScannedTimestamp(tick: tick, sourceCell: cell)
                    )
                }
            case .text(let rawText):
                if startsWithMetadataMarker(rawText) {
                    onIssue(.eventMetadataInSpikeTrain(cell: cell))
                    continue
                }
                if let tick = decodeTextTimestamp(
                    rawText,
                    sourceTimeUnit: sourceTimeUnit,
                    cell: cell,
                    onIssue: onIssue
                ) {
                    timestamps.append(
                        ScientificImportScannedTimestamp(tick: tick, sourceCell: cell)
                    )
                }
            }
        }

        return ScientificImportSpikeColumnScan(timestamps: timestamps)
    }

    static func scanEventColumn(
        _ column: StagedScientificColumn,
        source: StagedTabularSource,
        sourceTimeUnit: SpikeTimeUnit,
        onIssue: (ScientificImportSourceScanIssue) -> Void
    ) -> ScientificImportEventColumnScan {
        var occurrences: [ScientificImportScannedEventOccurrence] = []
        var openOccurrenceIndex: Int?
        occurrences.reserveCapacity(column.cells.count)

        for (rowOffset, value) in column.cells.enumerated() {
            guard let cell = sourceCell(
                column: column.sourceColumn,
                oneBasedRow: rowOffset + 1
            ) else {
                onIssue(.internalInvariant)
                continue
            }

            switch value {
            case .blank:
                openOccurrenceIndex = nil
            case .spreadsheetNumber(let rawLexeme):
                guard case .excelWorkbook = source else {
                    onIssue(.spreadsheetNumberOutsideWorkbook(cell: cell))
                    openOccurrenceIndex = nil
                    continue
                }
                if let tick = decodeSpreadsheetTimestamp(
                    rawLexeme,
                    sourceTimeUnit: sourceTimeUnit,
                    cell: cell,
                    onIssue: onIssue
                ) {
                    occurrences.append(
                        ScientificImportScannedEventOccurrence(
                            sourceTick: tick,
                            timestampCell: cell,
                            metadata: []
                        )
                    )
                    openOccurrenceIndex = occurrences.count - 1
                } else {
                    openOccurrenceIndex = nil
                }
            case .text(let rawText):
                if startsWithMetadataMarker(rawText) {
                    guard let openOccurrenceIndex else {
                        onIssue(.eventMetadataWithoutOccurrence(cell: cell))
                        continue
                    }
                    if let metadata = parseMetadata(
                        rawText,
                        cell: cell,
                        onIssue: onIssue
                    ) {
                        occurrences[openOccurrenceIndex].metadata.append(metadata)
                    }
                } else if let tick = decodeTextTimestamp(
                    rawText,
                    sourceTimeUnit: sourceTimeUnit,
                    cell: cell,
                    onIssue: onIssue
                ) {
                    occurrences.append(
                        ScientificImportScannedEventOccurrence(
                            sourceTick: tick,
                            timestampCell: cell,
                            metadata: []
                        )
                    )
                    openOccurrenceIndex = occurrences.count - 1
                } else {
                    openOccurrenceIndex = nil
                }
            }
        }

        return ScientificImportEventColumnScan(occurrences: occurrences)
    }

    private static func decodeTextTimestamp(
        _ rawText: String,
        sourceTimeUnit: SpikeTimeUnit,
        cell: StagedSourceCellReference,
        onIssue: (ScientificImportSourceScanIssue) -> Void
    ) -> MicrosecondTick? {
        do {
            return try ExactTimestampTextCodec.decode(rawText, sourceUnit: sourceTimeUnit)
        } catch let error as ExactTimestampParseError {
            onIssue(.timestampParseFailed(cell: cell, error: error))
        } catch {
            onIssue(.internalInvariant)
        }
        return nil
    }

    private static func decodeSpreadsheetTimestamp(
        _ rawLexeme: String,
        sourceTimeUnit: SpikeTimeUnit,
        cell: StagedSourceCellReference,
        onIssue: (ScientificImportSourceScanIssue) -> Void
    ) -> MicrosecondTick? {
        do {
            return try SpreadsheetNumericTimestampCodec.decode(
                rawLexeme: rawLexeme,
                sourceUnit: sourceTimeUnit
            )
        } catch let error as SpreadsheetNumericTimestampDecodeError {
            onIssue(
                .spreadsheetNumberTimestampDecodeFailed(cell: cell, error: error)
            )
        } catch {
            onIssue(.internalInvariant)
        }
        return nil
    }

    private static func parseMetadata(
        _ rawText: String,
        cell: StagedSourceCellReference,
        onIssue: (ScientificImportSourceScanIssue) -> Void
    ) -> ScientificImportScannedMetadata? {
        let payload = rawText.dropFirst()
        guard let equalsIndex = payload.firstIndex(of: "=") else {
            onIssue(.eventMetadataMissingEquals(cell: cell))
            return nil
        }

        let rawKey = String(payload[..<equalsIndex])
        let rawValue = String(payload[payload.index(after: equalsIndex)...])
        let unitSuffix = ".unit"
        let isUnitSuggestion = rawKey.hasSuffix(unitSuffix)
        let keyText = isUnitSuggestion
            ? String(rawKey.dropLast(unitSuffix.count))
            : rawKey

        do {
            let key = try EventAttributeKey(validating: keyText)
            if isUnitSuggestion {
                return .unitSuggestion(key: key, rawValue: rawValue, sourceCell: cell)
            }
            return .attribute(key: key, rawValue: rawValue, sourceCell: cell)
        } catch let error as EventAttributeKeyError {
            onIssue(.invalidEventAttributeKey(cell: cell, error: error))
        } catch {
            onIssue(.internalInvariant)
        }
        return nil
    }

    private static func sourceCell(
        column: StagedSourceColumnReference,
        oneBasedRow: Int
    ) -> StagedSourceCellReference? {
        try? StagedSourceCellReference(
            column: column,
            oneBasedDataRowIndex: oneBasedRow
        )
    }

    private static func startsWithMetadataMarker(_ text: String) -> Bool {
        text.utf8.first == 0x40
    }
}
