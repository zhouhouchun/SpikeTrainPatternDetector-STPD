/// Read-only discovery for the current scientific-import draft.
///
/// This service does not resolve a plan, normalize values, choose defaults, construct a canonical
/// dataset, or activate analysis. It only exposes exact source facts needed by a two-stage UI.
public enum ScientificImportPreflight {
    public static let maximumReportedIssueCount = 128

    public static func inspect(
        stagedImport: StagedScientificImport,
        draft: ScientificImportManifestDraft
    ) -> ScientificImportPreflightReport {
        guard draft.sourceBinding == ScientificImportDraftSourceBinding(
            stagedImport: stagedImport
        ) else {
            return emptyReport(issue: .sourceBindingMismatch)
        }
        guard let sourceTimeUnit = draft.sourceTimeUnit else {
            return emptyReport(issue: .missingSourceTimeUnit)
        }
        guard let groups = draft.eventScopeGroups, !groups.isEmpty else {
            return emptyReport(issue: .missingEventScopeGroups)
        }

        let scientificAttributeKeys = explicitlyScientificAttributeKeys(
            draft.eventAttributeDefinitions
        )
        var columnFacts: [ScientificImportPreflightColumnFacts] = []
        var originCandidates: [ScientificImportEventOriginCandidate] = []
        var issues = PreflightIssueAccumulator()
        var attributes: [EventAttributeKey: AttributeAccumulator] = [:]
        var firstGroupBySourceColumn: [StagedSourceColumnReference: Int] = [:]

        for (groupOffset, group) in groups.enumerated() {
            let groupIndex = groupOffset + 1

            for spike in group.spikeTrains {
                guard register(
                    spike.sourceColumn,
                    groupIndex: groupIndex,
                    firstGroupBySourceColumn: &firstGroupBySourceColumn,
                    issues: &issues
                ) else {
                    continue
                }
                guard let column = sourceColumn(
                    spike.sourceColumn,
                    from: stagedImport
                ) else {
                    issues.append(
                        .sourceColumnOutsideStaging(
                            groupIndex: groupIndex,
                            column: spike.sourceColumn
                        )
                    )
                    continue
                }

                let scan = ScientificImportSourceScanner.scanSpikeColumn(
                    column,
                    source: stagedImport.source,
                    sourceTimeUnit: sourceTimeUnit,
                    onIssue: { issue in
                        append(issue, groupIndex: groupIndex, to: &issues)
                    }
                )
                columnFacts.append(
                    makeColumnFacts(
                        groupIndex: groupIndex,
                        sourceColumn: spike.sourceColumn,
                        role: .spikeTrain,
                        timestamps: scan.timestamps
                    )
                )
            }

            for event in group.eventDefinitions {
                guard register(
                    event.sourceColumn,
                    groupIndex: groupIndex,
                    firstGroupBySourceColumn: &firstGroupBySourceColumn,
                    issues: &issues
                ) else {
                    continue
                }
                guard let column = sourceColumn(
                    event.sourceColumn,
                    from: stagedImport
                ) else {
                    issues.append(
                        .sourceColumnOutsideStaging(
                            groupIndex: groupIndex,
                            column: event.sourceColumn
                        )
                    )
                    continue
                }

                let scan = ScientificImportSourceScanner.scanEventColumn(
                    column,
                    source: stagedImport.source,
                    sourceTimeUnit: sourceTimeUnit,
                    onIssue: { issue in
                        append(issue, groupIndex: groupIndex, to: &issues)
                    }
                )
                let timestamps = scan.occurrences.map {
                    ScientificImportScannedTimestamp(
                        tick: $0.sourceTick,
                        sourceCell: $0.timestampCell
                    )
                }
                columnFacts.append(
                    makeColumnFacts(
                        groupIndex: groupIndex,
                        sourceColumn: event.sourceColumn,
                        role: .eventDefinition,
                        timestamps: timestamps
                    )
                )

                for occurrence in scan.occurrences {
                    let indexed = indexMetadata(
                        occurrence.metadata,
                        groupIndex: groupIndex,
                        eventTimestampCell: occurrence.timestampCell,
                        attributes: &attributes,
                        issues: &issues
                    )
                    let scientificAttributes = indexed.firstAttributeByKey
                        .compactMap { key, line
                            -> ScientificImportPreflightOriginScientificAttribute? in
                            guard scientificAttributeKeys.contains(key) else { return nil }
                            return ScientificImportPreflightOriginScientificAttribute(
                                key: key,
                                rawValue: line.rawValue,
                                sourceCell: line.sourceCell
                            )
                        }
                        .sorted { lhs, rhs in
                            keyIsOrderedBefore(lhs.key, rhs.key)
                        }
                    originCandidates.append(
                        ScientificImportEventOriginCandidate(
                            groupIndex: groupIndex,
                            sourceColumn: event.sourceColumn,
                            eventDefinitionID: event.semanticID,
                            timestampCell: occurrence.timestampCell,
                            sourceTick: occurrence.sourceTick,
                            scientificAttributes: scientificAttributes
                        )
                    )
                }
            }
        }

        columnFacts.sort(by: columnFactsAreOrderedBefore)
        originCandidates.sort(by: originCandidatesAreOrderedBefore)
        let discoveredAttributes = attributes.keys
            .sorted(by: keyIsOrderedBefore)
            .compactMap { key -> ScientificImportDiscoveredEventAttribute? in
                guard let accumulated = attributes[key] else { return nil }
                return ScientificImportDiscoveredEventAttribute(
                    key: key,
                    valueSightings: accumulated.valueSightings,
                    inlineUnitSuggestions: accumulated.inlineUnitSuggestions
                )
            }

        return ScientificImportPreflightReport(
            columnFacts: columnFacts,
            discoveredEventAttributes: discoveredAttributes,
            eventOriginCandidates: originCandidates,
            issues: issues.retained,
            additionalIssueCount: issues.additionalCount
        )
    }

    private static func emptyReport(
        issue: ScientificImportPreflightIssue
    ) -> ScientificImportPreflightReport {
        ScientificImportPreflightReport(
            columnFacts: [],
            discoveredEventAttributes: [],
            eventOriginCandidates: [],
            issues: [issue],
            additionalIssueCount: 0
        )
    }

    private static func register(
        _ sourceColumn: StagedSourceColumnReference,
        groupIndex: Int,
        firstGroupBySourceColumn: inout [StagedSourceColumnReference: Int],
        issues: inout PreflightIssueAccumulator
    ) -> Bool {
        if let firstGroupIndex = firstGroupBySourceColumn[sourceColumn] {
            issues.append(
                .sourceColumnAssignedMultipleTimes(
                    column: sourceColumn,
                    firstGroupIndex: firstGroupIndex,
                    duplicateGroupIndex: groupIndex
                )
            )
            return false
        }
        firstGroupBySourceColumn[sourceColumn] = groupIndex
        return true
    }

    private static func sourceColumn(
        _ reference: StagedSourceColumnReference,
        from stagedImport: StagedScientificImport
    ) -> StagedScientificColumn? {
        let offset = reference.oneBasedIndex - 1
        guard stagedImport.columns.indices.contains(offset) else { return nil }
        let column = stagedImport.columns[offset]
        guard column.sourceColumn == reference else { return nil }
        return column
    }

    private static func append(
        _ issue: ScientificImportSourceScanIssue,
        groupIndex: Int,
        to issues: inout PreflightIssueAccumulator
    ) {
        switch issue {
        case .spreadsheetNumberOutsideWorkbook(let cell):
            issues.append(
                .spreadsheetNumberOutsideWorkbook(groupIndex: groupIndex, cell: cell)
            )
        case .spreadsheetNumberTimestampDecodeFailed(let cell, let error):
            issues.append(
                .spreadsheetNumberTimestampDecodeFailed(
                    groupIndex: groupIndex,
                    cell: cell,
                    error: error
                )
            )
        case .timestampParseFailed(let cell, let error):
            issues.append(
                .timestampParseFailed(
                    groupIndex: groupIndex,
                    cell: cell,
                    error: error
                )
            )
        case .eventMetadataInSpikeTrain(let cell):
            issues.append(.eventMetadataInSpikeTrain(groupIndex: groupIndex, cell: cell))
        case .eventMetadataWithoutOccurrence(let cell):
            issues.append(
                .eventMetadataWithoutOccurrence(groupIndex: groupIndex, cell: cell)
            )
        case .eventMetadataMissingEquals(let cell):
            issues.append(.eventMetadataMissingEquals(groupIndex: groupIndex, cell: cell))
        case .invalidEventAttributeKey(let cell, let error):
            issues.append(
                .invalidEventAttributeKey(
                    groupIndex: groupIndex,
                    cell: cell,
                    error: error
                )
            )
        case .internalInvariant:
            issues.append(.internalInvariant)
        }
    }

    private static func indexMetadata(
        _ metadata: [ScientificImportScannedMetadata],
        groupIndex: Int,
        eventTimestampCell: StagedSourceCellReference,
        attributes: inout [EventAttributeKey: AttributeAccumulator],
        issues: inout PreflightIssueAccumulator
    ) -> IndexedMetadata {
        var firstAttributeByKey: [EventAttributeKey: MetadataLine] = [:]
        var firstUnitByKey: [EventAttributeKey: MetadataLine] = [:]

        for item in metadata {
            switch item {
            case .attribute(let key, let rawValue, let sourceCell):
                attributes[key, default: AttributeAccumulator()].valueSightings.append(
                    ScientificImportEventAttributeValueSighting(
                        groupIndex: groupIndex,
                        eventTimestampCell: eventTimestampCell,
                        valueCell: sourceCell,
                        rawValue: rawValue
                    )
                )
                if let first = firstAttributeByKey[key] {
                    issues.append(
                        .duplicateEventAttributeKey(
                            groupIndex: groupIndex,
                            eventTimestampCell: eventTimestampCell,
                            key: key,
                            firstCell: first.sourceCell,
                            duplicateCell: sourceCell
                        )
                    )
                } else {
                    firstAttributeByKey[key] = MetadataLine(
                        rawValue: rawValue,
                        sourceCell: sourceCell
                    )
                }
            case .unitSuggestion(let key, let rawValue, let sourceCell):
                attributes[key, default: AttributeAccumulator()].inlineUnitSuggestions.append(
                    ScientificImportInlineUnitSuggestionFact(
                        groupIndex: groupIndex,
                        eventTimestampCell: eventTimestampCell,
                        unitCell: sourceCell,
                        rawUnit: rawValue
                    )
                )
                if let first = firstUnitByKey[key] {
                    issues.append(
                        .duplicateInlineUnitSuggestion(
                            groupIndex: groupIndex,
                            eventTimestampCell: eventTimestampCell,
                            key: key,
                            firstCell: first.sourceCell,
                            duplicateCell: sourceCell
                        )
                    )
                } else {
                    firstUnitByKey[key] = MetadataLine(
                        rawValue: rawValue,
                        sourceCell: sourceCell
                    )
                }
            }
        }

        for key in firstUnitByKey.keys.sorted(by: keyIsOrderedBefore) {
            guard firstAttributeByKey[key] == nil,
                  let unit = firstUnitByKey[key] else {
                continue
            }
            issues.append(
                .inlineUnitWithoutAttribute(
                    groupIndex: groupIndex,
                    eventTimestampCell: eventTimestampCell,
                    key: key,
                    unitCell: unit.sourceCell
                )
            )
        }

        return IndexedMetadata(firstAttributeByKey: firstAttributeByKey)
    }

    private static func makeColumnFacts(
        groupIndex: Int,
        sourceColumn: StagedSourceColumnReference,
        role: ScientificImportPreflightColumnRole,
        timestamps: [ScientificImportScannedTimestamp]
    ) -> ScientificImportPreflightColumnFacts {
        var negativeCount = 0
        var firstNegativeCell: StagedSourceCellReference?
        var descentCount = 0
        var firstDescent: ScientificImportPreflightSourceOrderDescent?
        var firstCellByTick: [MicrosecondTick: StagedSourceCellReference] = [:]
        var duplicateCount = 0
        var firstDuplicate: ScientificImportPreflightExactDuplicate?

        for (index, timestamp) in timestamps.enumerated() {
            if timestamp.tick.microseconds < 0 {
                negativeCount += 1
                if firstNegativeCell == nil {
                    firstNegativeCell = timestamp.sourceCell
                }
            }
            if index > 0, timestamp.tick < timestamps[index - 1].tick {
                descentCount += 1
                if firstDescent == nil {
                    firstDescent = ScientificImportPreflightSourceOrderDescent(
                        previousCell: timestamps[index - 1].sourceCell,
                        currentCell: timestamp.sourceCell
                    )
                }
            }
            if let firstCell = firstCellByTick[timestamp.tick] {
                duplicateCount += 1
                if firstDuplicate == nil {
                    firstDuplicate = ScientificImportPreflightExactDuplicate(
                        tick: timestamp.tick,
                        firstCell: firstCell,
                        duplicateCell: timestamp.sourceCell
                    )
                }
            } else {
                firstCellByTick[timestamp.tick] = timestamp.sourceCell
            }
        }

        return ScientificImportPreflightColumnFacts(
            groupIndex: groupIndex,
            sourceColumn: sourceColumn,
            role: role,
            validTimestampCount: timestamps.count,
            negativeTimestampCount: negativeCount,
            firstNegativeTimestampCell: firstNegativeCell,
            sourceOrderDescentCount: descentCount,
            firstSourceOrderDescent: firstDescent,
            exactDuplicateTimestampCount: duplicateCount,
            firstExactDuplicateTimestamp: firstDuplicate
        )
    }

    private static func explicitlyScientificAttributeKeys(
        _ definitions: [EventAttributeDefinitionDraft]
    ) -> Set<EventAttributeKey> {
        var rolesByKey: [EventAttributeKey: [EventAttributeRole?]] = [:]
        for definition in definitions {
            rolesByKey[definition.key, default: []].append(definition.role)
        }
        return Set(rolesByKey.compactMap { key, roles in
            roles.count == 1 && roles[0] == .scientific ? key : nil
        })
    }

    private static func columnFactsAreOrderedBefore(
        _ lhs: ScientificImportPreflightColumnFacts,
        _ rhs: ScientificImportPreflightColumnFacts
    ) -> Bool {
        if lhs.groupIndex != rhs.groupIndex { return lhs.groupIndex < rhs.groupIndex }
        return lhs.sourceColumn < rhs.sourceColumn
    }

    private static func originCandidatesAreOrderedBefore(
        _ lhs: ScientificImportEventOriginCandidate,
        _ rhs: ScientificImportEventOriginCandidate
    ) -> Bool {
        if lhs.groupIndex != rhs.groupIndex { return lhs.groupIndex < rhs.groupIndex }
        if lhs.sourceColumn != rhs.sourceColumn { return lhs.sourceColumn < rhs.sourceColumn }
        return lhs.timestampCell.oneBasedDataRowIndex
            < rhs.timestampCell.oneBasedDataRowIndex
    }

    private static func keyIsOrderedBefore(
        _ lhs: EventAttributeKey,
        _ rhs: EventAttributeKey
    ) -> Bool {
        lhs.canonicalText.utf8.lexicographicallyPrecedes(rhs.canonicalText.utf8)
    }
}

private struct AttributeAccumulator {
    var valueSightings: [ScientificImportEventAttributeValueSighting] = []
    var inlineUnitSuggestions: [ScientificImportInlineUnitSuggestionFact] = []
}

private struct MetadataLine {
    let rawValue: String
    let sourceCell: StagedSourceCellReference
}

private struct IndexedMetadata {
    let firstAttributeByKey: [EventAttributeKey: MetadataLine]
}

private struct PreflightIssueAccumulator {
    private(set) var retained: [ScientificImportPreflightIssue] = []
    private(set) var totalCount = 0

    var additionalCount: Int {
        totalCount - retained.count
    }

    mutating func append(_ issue: ScientificImportPreflightIssue) {
        totalCount += 1
        if retained.count < ScientificImportPreflight.maximumReportedIssueCount {
            retained.append(issue)
        }
    }
}
