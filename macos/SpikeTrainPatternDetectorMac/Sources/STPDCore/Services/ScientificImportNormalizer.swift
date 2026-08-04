public enum ScientificImportAttributeValueIssue: Equatable, Sendable {
    case explicitEmptyStringForbidden
    case stringValue(CanonicalStringValueError)
    case exactNumber(ExactEventAttributeParseError)
    case booleanEmpty
    case booleanLexemeTooLong(maximumUTF8Bytes: Int)
    case invalidBoolean
}

public enum ScientificImportNormalizationIssue: Equatable, Sendable {
    case spreadsheetNumberTimestampRequiresPrecisionProof(
        group: Int,
        cell: StagedSourceCellReference
    )
    case timestampParseFailed(
        group: Int,
        cell: StagedSourceCellReference,
        error: ExactTimestampParseError
    )
    case eventMetadataInSpikeTrain(group: Int, cell: StagedSourceCellReference)
    case eventMetadataWithoutOccurrence(group: Int, cell: StagedSourceCellReference)
    case eventMetadataMissingEquals(group: Int, cell: StagedSourceCellReference)
    case invalidEventAttributeKey(
        group: Int,
        cell: StagedSourceCellReference,
        error: EventAttributeKeyError
    )
    case unknownEventAttributeKey(
        group: Int,
        cell: StagedSourceCellReference,
        key: EventAttributeKey
    )
    case duplicateEventAttributeKey(
        group: Int,
        eventTimestampCell: StagedSourceCellReference,
        key: EventAttributeKey,
        firstCell: StagedSourceCellReference,
        duplicateCell: StagedSourceCellReference
    )
    case eventAttributeValueInvalid(
        group: Int,
        cell: StagedSourceCellReference,
        key: EventAttributeKey,
        expectedType: EventAttributeScalarType,
        issue: ScientificImportAttributeValueIssue
    )
    case duplicateInlineUnitSuggestion(
        group: Int,
        eventTimestampCell: StagedSourceCellReference,
        key: EventAttributeKey,
        firstCell: StagedSourceCellReference,
        duplicateCell: StagedSourceCellReference
    )
    case inlineUnitWithoutAttribute(
        group: Int,
        eventTimestampCell: StagedSourceCellReference,
        key: EventAttributeKey,
        unitCell: StagedSourceCellReference
    )
    case inlineUnitSuggestionInvalid(
        group: Int,
        cell: StagedSourceCellReference,
        key: EventAttributeKey,
        error: OpaqueUnitSymbolError
    )
    case inlineUnitConflictsWithDefinition(
        group: Int,
        cell: StagedSourceCellReference,
        key: EventAttributeKey,
        suggested: OpaqueUnitSymbol,
        confirmed: ConfirmedEventAttributeUnit
    )
    case inlineUnitSuggestionsConflict(
        key: EventAttributeKey,
        firstCell: StagedSourceCellReference,
        firstUnit: OpaqueUnitSymbol,
        conflictingCell: StagedSourceCellReference,
        conflictingUnit: OpaqueUnitSymbol
    )
    case selectedOriginIsNotEventTimestamp(group: Int, cell: StagedSourceCellReference)
    case selectedOriginAmbiguous(
        group: Int,
        eventDefinitionID: ScientificEventDefinitionID,
        cell: StagedSourceCellReference,
        multiplicity: Int
    )
    case recordingElapsedTimestampIsNegative(group: Int, cell: StagedSourceCellReference)
    case eventRelativeRebaseOverflow(
        group: Int,
        valueCell: StagedSourceCellReference,
        originCell: StagedSourceCellReference
    )
    case sourceOrderNotAscending(
        group: Int,
        column: StagedSourceColumnReference,
        descentCount: Int,
        firstPreviousCell: StagedSourceCellReference,
        firstCurrentCell: StagedSourceCellReference
    )
    case duplicateCollapseNotAllowed(
        activityMode: ScientificDatasetActivityMode,
        group: Int,
        column: StagedSourceColumnReference
    )
    case resolvedPlanInvariant
}

public struct ScientificImportNormalizationError: Error, Equatable, Sendable {
    public let issues: [ScientificImportNormalizationIssue]
    /// Number of additional deterministic issues not retained after the diagnostic cap.
    public let additionalIssueCount: Int

    internal init(
        issues: [ScientificImportNormalizationIssue],
        additionalIssueCount: Int
    ) {
        self.issues = issues
        self.additionalIssueCount = additionalIssueCount
    }
}

/// Pure preparation of exact timestamp and event-attribute values. The service has no persistence,
/// application-state, analysis-permission, or downstream projection behavior.
public enum ScientificImportNormalizer {
    public static let maximumReportedIssueCount = 128

    public static func normalize(
        resolvedPlan: ResolvedScientificImportPlan
    ) throws -> PreparedScientificImport {
        var lexicalIssues = IssueAccumulator()
        let rawGroups = parseRawGroups(plan: resolvedPlan, issues: &lexicalIssues)
        try lexicalIssues.throwIfPresent()

        var attributeIssues = IssueAccumulator()
        let typedGroups = typeEventAttributes(
            rawGroups: rawGroups,
            definitions: resolvedPlan.eventAttributeDefinitions,
            issues: &attributeIssues
        )
        try attributeIssues.throwIfPresent()

        var originIssues = IssueAccumulator()
        let groupOrigins = resolveGroupOrigins(
            typedGroups: typedGroups,
            issues: &originIssues
        )
        try originIssues.throwIfPresent()

        var coordinateIssues = IssueAccumulator()
        let rebasedGroups = applyTimeBases(
            typedGroups: typedGroups,
            origins: groupOrigins,
            issues: &coordinateIssues
        )
        try coordinateIssues.throwIfPresent()

        var orderingIssues = IssueAccumulator()
        let prepared = prepareFinalValues(
            rebasedGroups: rebasedGroups,
            plan: resolvedPlan,
            issues: &orderingIssues
        )
        try orderingIssues.throwIfPresent()

        guard let prepared else {
            throw ScientificImportNormalizationError(
                issues: [.resolvedPlanInvariant],
                additionalIssueCount: 0
            )
        }
        return prepared
    }

    private static func parseRawGroups(
        plan: ResolvedScientificImportPlan,
        issues: inout IssueAccumulator
    ) -> [RawGroup] {
        var groups: [RawGroup] = []
        groups.reserveCapacity(plan.eventScopeGroups.count)

        for (groupOffset, groupPlan) in plan.eventScopeGroups.enumerated() {
            let groupNumber = groupOffset + 1
            var spikeColumns: [RawSpikeColumn] = []
            var eventColumns: [RawEventColumn] = []
            spikeColumns.reserveCapacity(groupPlan.spikeTrains.count)
            eventColumns.reserveCapacity(groupPlan.eventDefinitions.count)

            for spikePlan in groupPlan.spikeTrains {
                guard let column = sourceColumn(
                    spikePlan.sourceColumn,
                    from: plan.source
                ) else {
                    issues.append(.resolvedPlanInvariant)
                    continue
                }
                var timestamps: [RawTimestamp] = []
                timestamps.reserveCapacity(column.cells.count)
                for (rowOffset, cellValue) in column.cells.enumerated() {
                    guard let cell = sourceCell(
                        column: spikePlan.sourceColumn,
                        oneBasedRow: rowOffset + 1
                    ) else {
                        issues.append(.resolvedPlanInvariant)
                        continue
                    }
                    switch cellValue {
                    case .blank:
                        continue
                    case .spreadsheetNumber:
                        issues.append(
                            .spreadsheetNumberTimestampRequiresPrecisionProof(
                                group: groupNumber,
                                cell: cell
                            )
                        )
                    case .text(let rawText):
                        if startsWithMetadataMarker(rawText) {
                            issues.append(
                                .eventMetadataInSpikeTrain(group: groupNumber, cell: cell)
                            )
                            continue
                        }
                        if let tick = parseTimestamp(
                            rawText,
                            unit: plan.sourceTimeUnit,
                            group: groupNumber,
                            cell: cell,
                            issues: &issues
                        ) {
                            timestamps.append(RawTimestamp(tick: tick, sourceCell: cell))
                        }
                    }
                }
                spikeColumns.append(RawSpikeColumn(plan: spikePlan, timestamps: timestamps))
            }

            for eventPlan in groupPlan.eventDefinitions {
                guard let column = sourceColumn(
                    eventPlan.sourceColumn,
                    from: plan.source
                ) else {
                    issues.append(.resolvedPlanInvariant)
                    continue
                }
                var occurrences: [RawEventOccurrence] = []
                var openOccurrenceIndex: Int?
                occurrences.reserveCapacity(column.cells.count)

                for (rowOffset, cellValue) in column.cells.enumerated() {
                    guard let cell = sourceCell(
                        column: eventPlan.sourceColumn,
                        oneBasedRow: rowOffset + 1
                    ) else {
                        issues.append(.resolvedPlanInvariant)
                        continue
                    }
                    switch cellValue {
                    case .blank:
                        openOccurrenceIndex = nil
                    case .spreadsheetNumber:
                        issues.append(
                            .spreadsheetNumberTimestampRequiresPrecisionProof(
                                group: groupNumber,
                                cell: cell
                            )
                        )
                        openOccurrenceIndex = nil
                    case .text(let rawText):
                        if startsWithMetadataMarker(rawText) {
                            guard let openOccurrenceIndex else {
                                issues.append(
                                    .eventMetadataWithoutOccurrence(
                                        group: groupNumber,
                                        cell: cell
                                    )
                                )
                                continue
                            }
                            if let metadata = parseMetadata(
                                rawText,
                                group: groupNumber,
                                cell: cell,
                                issues: &issues
                            ) {
                                occurrences[openOccurrenceIndex].metadata.append(metadata)
                            }
                        } else if let tick = parseTimestamp(
                            rawText,
                            unit: plan.sourceTimeUnit,
                            group: groupNumber,
                            cell: cell,
                            issues: &issues
                        ) {
                            occurrences.append(
                                RawEventOccurrence(
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
                eventColumns.append(RawEventColumn(plan: eventPlan, occurrences: occurrences))
            }

            groups.append(
                RawGroup(
                    groupNumber: groupNumber,
                    plan: groupPlan,
                    spikeColumns: spikeColumns,
                    eventColumns: eventColumns
                )
            )
        }
        return groups
    }

    private static func parseTimestamp(
        _ rawText: String,
        unit: SpikeTimeUnit,
        group: Int,
        cell: StagedSourceCellReference,
        issues: inout IssueAccumulator
    ) -> MicrosecondTick? {
        do {
            return try ExactTimestampTextCodec.decode(rawText, sourceUnit: unit)
        } catch let error as ExactTimestampParseError {
            issues.append(.timestampParseFailed(group: group, cell: cell, error: error))
        } catch {
            issues.append(.resolvedPlanInvariant)
        }
        return nil
    }

    private static func parseMetadata(
        _ rawText: String,
        group: Int,
        cell: StagedSourceCellReference,
        issues: inout IssueAccumulator
    ) -> RawMetadata? {
        let payload = rawText.dropFirst()
        guard let equalsIndex = payload.firstIndex(of: "=") else {
            issues.append(.eventMetadataMissingEquals(group: group, cell: cell))
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
            issues.append(
                .invalidEventAttributeKey(group: group, cell: cell, error: error)
            )
        } catch {
            issues.append(.resolvedPlanInvariant)
        }
        return nil
    }

    private static func typeEventAttributes(
        rawGroups: [RawGroup],
        definitions: [ResolvedEventAttributeDefinitionPlan],
        issues: inout IssueAccumulator
    ) -> [TypedGroup] {
        let definitionsByKey = Dictionary(uniqueKeysWithValues: definitions.map { ($0.key, $0) })
        var firstUnitByKey: [EventAttributeKey: UnitSighting] = [:]
        var groups: [TypedGroup] = []
        groups.reserveCapacity(rawGroups.count)

        for rawGroup in rawGroups {
            var eventColumns: [TypedEventColumn] = []
            eventColumns.reserveCapacity(rawGroup.eventColumns.count)

            for rawColumn in rawGroup.eventColumns {
                var occurrences: [TypedEventOccurrence] = []
                occurrences.reserveCapacity(rawColumn.occurrences.count)

                for rawOccurrence in rawColumn.occurrences {
                    var firstAttributeLineByKey: [EventAttributeKey: RawValueLine] = [:]
                    var firstUnitLineByKey: [EventAttributeKey: RawValueLine] = [:]

                    for metadata in rawOccurrence.metadata {
                        switch metadata {
                        case .attribute(let key, let rawValue, let sourceCell):
                            guard definitionsByKey[key] != nil else {
                                issues.append(
                                    .unknownEventAttributeKey(
                                        group: rawGroup.groupNumber,
                                        cell: sourceCell,
                                        key: key
                                    )
                                )
                                continue
                            }
                            if let first = firstAttributeLineByKey[key] {
                                issues.append(
                                    .duplicateEventAttributeKey(
                                        group: rawGroup.groupNumber,
                                        eventTimestampCell: rawOccurrence.timestampCell,
                                        key: key,
                                        firstCell: first.sourceCell,
                                        duplicateCell: sourceCell
                                    )
                                )
                            } else {
                                firstAttributeLineByKey[key] = RawValueLine(
                                    rawValue: rawValue,
                                    sourceCell: sourceCell
                                )
                            }
                        case .unitSuggestion(let key, let rawValue, let sourceCell):
                            guard definitionsByKey[key] != nil else {
                                issues.append(
                                    .unknownEventAttributeKey(
                                        group: rawGroup.groupNumber,
                                        cell: sourceCell,
                                        key: key
                                    )
                                )
                                continue
                            }
                            if let first = firstUnitLineByKey[key] {
                                issues.append(
                                    .duplicateInlineUnitSuggestion(
                                        group: rawGroup.groupNumber,
                                        eventTimestampCell: rawOccurrence.timestampCell,
                                        key: key,
                                        firstCell: first.sourceCell,
                                        duplicateCell: sourceCell
                                    )
                                )
                            } else {
                                firstUnitLineByKey[key] = RawValueLine(
                                    rawValue: rawValue,
                                    sourceCell: sourceCell
                                )
                            }
                        }
                    }

                    var parsedUnits: [EventAttributeKey: ParsedUnitSuggestion] = [:]
                    let orderedUnitKeys = firstUnitLineByKey.keys.sorted(by: keyIsOrderedBefore)
                    for key in orderedUnitKeys {
                        guard let line = firstUnitLineByKey[key],
                              let definition = definitionsByKey[key] else {
                            issues.append(.resolvedPlanInvariant)
                            continue
                        }
                        guard firstAttributeLineByKey[key] != nil else {
                            issues.append(
                                .inlineUnitWithoutAttribute(
                                    group: rawGroup.groupNumber,
                                    eventTimestampCell: rawOccurrence.timestampCell,
                                    key: key,
                                    unitCell: line.sourceCell
                                )
                            )
                            continue
                        }
                        let symbol: OpaqueUnitSymbol
                        do {
                            symbol = try OpaqueUnitSymbol(validating: line.rawValue)
                        } catch let error as OpaqueUnitSymbolError {
                            issues.append(
                                .inlineUnitSuggestionInvalid(
                                    group: rawGroup.groupNumber,
                                    cell: line.sourceCell,
                                    key: key,
                                    error: error
                                )
                            )
                            continue
                        } catch {
                            issues.append(.resolvedPlanInvariant)
                            continue
                        }

                        if case .specified(let confirmedSymbol) = definition.unit,
                           confirmedSymbol == symbol {
                            parsedUnits[key] = ParsedUnitSuggestion(
                                symbol: symbol,
                                sourceCell: line.sourceCell
                            )
                        } else {
                            issues.append(
                                .inlineUnitConflictsWithDefinition(
                                    group: rawGroup.groupNumber,
                                    cell: line.sourceCell,
                                    key: key,
                                    suggested: symbol,
                                    confirmed: definition.unit
                                )
                            )
                        }

                        if let first = firstUnitByKey[key] {
                            if first.symbol != symbol {
                                issues.append(
                                    .inlineUnitSuggestionsConflict(
                                        key: key,
                                        firstCell: first.sourceCell,
                                        firstUnit: first.symbol,
                                        conflictingCell: line.sourceCell,
                                        conflictingUnit: symbol
                                    )
                                )
                            }
                        } else {
                            firstUnitByKey[key] = UnitSighting(
                                symbol: symbol,
                                sourceCell: line.sourceCell
                            )
                        }
                    }

                    var attributes: [TypedAttribute] = []
                    let orderedAttributeKeys = firstAttributeLineByKey.keys.sorted(
                        by: keyIsOrderedBefore
                    )
                    attributes.reserveCapacity(orderedAttributeKeys.count)
                    for key in orderedAttributeKeys {
                        guard let line = firstAttributeLineByKey[key],
                              let definition = definitionsByKey[key] else {
                            issues.append(.resolvedPlanInvariant)
                            continue
                        }
                        guard let value = parseAttributeValue(
                            line.rawValue,
                            definition: definition,
                            group: rawGroup.groupNumber,
                            cell: line.sourceCell,
                            issues: &issues
                        ) else {
                            continue
                        }
                        let unitProvenance: PreparedInlineUnitSuggestionProvenance
                        if let parsedUnit = parsedUnits[key] {
                            unitProvenance = .present(
                                symbol: parsedUnit.symbol,
                                sourceCell: parsedUnit.sourceCell
                            )
                        } else {
                            unitProvenance = .absent
                        }
                        attributes.append(
                            TypedAttribute(
                                definition: definition,
                                value: PreparedEventAttribute(key: key, value: value),
                                provenance: PreparedEventAttributeProvenance(
                                    key: key,
                                    valueCell: line.sourceCell,
                                    inlineUnitSuggestion: unitProvenance
                                )
                            )
                        )
                    }
                    occurrences.append(
                        TypedEventOccurrence(
                            sourceTick: rawOccurrence.sourceTick,
                            timestampCell: rawOccurrence.timestampCell,
                            attributes: attributes
                        )
                    )
                }
                eventColumns.append(
                    TypedEventColumn(plan: rawColumn.plan, occurrences: occurrences)
                )
            }

            groups.append(
                TypedGroup(
                    groupNumber: rawGroup.groupNumber,
                    plan: rawGroup.plan,
                    spikeColumns: rawGroup.spikeColumns,
                    eventColumns: eventColumns
                )
            )
        }
        return groups
    }

    private static func parseAttributeValue(
        _ rawValue: String,
        definition: ResolvedEventAttributeDefinitionPlan,
        group: Int,
        cell: StagedSourceCellReference,
        issues: inout IssueAccumulator
    ) -> EventAttributeValue? {
        do {
            switch definition.scalarType {
            case .string:
                if rawValue.isEmpty, definition.emptyStringPolicy == .forbid {
                    issues.append(
                        .eventAttributeValueInvalid(
                            group: group,
                            cell: cell,
                            key: definition.key,
                            expectedType: .string,
                            issue: .explicitEmptyStringForbidden
                        )
                    )
                    return nil
                }
                return .string(try CanonicalStringValue(validating: rawValue))
            case .integer:
                return .integer(try ExactIntegerValue.parse(rawValue))
            case .exactDecimal:
                return .exactDecimal(try ExactDecimalValue.parse(rawValue))
            case .boolean:
                return .boolean(try parseBoolean(rawValue))
            }
        } catch let error as CanonicalStringValueError {
            issues.append(
                .eventAttributeValueInvalid(
                    group: group,
                    cell: cell,
                    key: definition.key,
                    expectedType: definition.scalarType,
                    issue: .stringValue(error)
                )
            )
        } catch let error as ExactEventAttributeParseError {
            issues.append(
                .eventAttributeValueInvalid(
                    group: group,
                    cell: cell,
                    key: definition.key,
                    expectedType: definition.scalarType,
                    issue: .exactNumber(error)
                )
            )
        } catch let error as BooleanLexemeError {
            let issue: ScientificImportAttributeValueIssue
            switch error {
            case .empty:
                issue = .booleanEmpty
            case .tooLong(let maximumUTF8Bytes):
                issue = .booleanLexemeTooLong(maximumUTF8Bytes: maximumUTF8Bytes)
            case .invalid:
                issue = .invalidBoolean
            }
            issues.append(
                .eventAttributeValueInvalid(
                    group: group,
                    cell: cell,
                    key: definition.key,
                    expectedType: definition.scalarType,
                    issue: issue
                )
            )
        } catch {
            issues.append(.resolvedPlanInvariant)
        }
        return nil
    }

    private static func resolveGroupOrigins(
        typedGroups: [TypedGroup],
        issues: inout IssueAccumulator
    ) -> [ResolvedGroupTimeBasis] {
        var results: [ResolvedGroupTimeBasis] = []
        results.reserveCapacity(typedGroups.count)

        for group in typedGroups {
            switch group.plan.timeBasis {
            case .recordingElapsed:
                results.append(.recordingElapsed)
            case .eventRelative(let stagedOrigin):
                let originCell = stagedOrigin.timestampCell
                var selectedDefinitionID: ScientificEventDefinitionID?
                var selectedOccurrence: TypedEventOccurrence?
                var selectedColumn: TypedEventColumn?

                for eventColumn in group.eventColumns {
                    if let occurrence = eventColumn.occurrences.first(where: {
                        $0.timestampCell == originCell
                    }) {
                        selectedDefinitionID = eventColumn.plan.semanticID
                        selectedOccurrence = occurrence
                        selectedColumn = eventColumn
                        break
                    }
                }

                guard let definitionID = selectedDefinitionID,
                      let occurrence = selectedOccurrence,
                      let eventColumn = selectedColumn else {
                    issues.append(
                        .selectedOriginIsNotEventTimestamp(
                            group: group.groupNumber,
                            cell: originCell
                        )
                    )
                    results.append(.unresolved)
                    continue
                }

                let scientificAttributes = occurrence.attributes
                    .filter { $0.definition.role == .scientific }
                    .map(\.value)
                let multiplicity = eventColumn.occurrences.reduce(into: 0) { count, candidate in
                    let candidateScientificAttributes = candidate.attributes
                        .filter { $0.definition.role == .scientific }
                        .map(\.value)
                    if candidate.sourceTick == occurrence.sourceTick,
                       candidateScientificAttributes == scientificAttributes {
                        count += 1
                    }
                }
                if multiplicity > 1 {
                    issues.append(
                        .selectedOriginAmbiguous(
                            group: group.groupNumber,
                            eventDefinitionID: definitionID,
                            cell: originCell,
                            multiplicity: multiplicity
                        )
                    )
                    results.append(.unresolved)
                    continue
                }

                results.append(
                    .eventRelative(
                        ResolvedOrigin(
                            eventDefinitionID: definitionID,
                            sourceTick: occurrence.sourceTick,
                            sourceCell: originCell,
                            scientificAttributes: scientificAttributes
                        )
                    )
                )
            }
        }
        return results
    }

    private static func applyTimeBases(
        typedGroups: [TypedGroup],
        origins: [ResolvedGroupTimeBasis],
        issues: inout IssueAccumulator
    ) -> [RebasedGroup] {
        guard typedGroups.count == origins.count else {
            issues.append(.resolvedPlanInvariant)
            return []
        }

        var groups: [RebasedGroup] = []
        groups.reserveCapacity(typedGroups.count)
        for index in typedGroups.indices {
            let group = typedGroups[index]
            let origin = origins[index]
            var spikeColumns: [RebasedSpikeColumn] = []
            var eventColumns: [RebasedEventColumn] = []

            for spikeColumn in group.spikeColumns {
                var timestamps: [RebasedTimestamp] = []
                timestamps.reserveCapacity(spikeColumn.timestamps.count)
                for timestamp in spikeColumn.timestamps {
                    if let tick = applyTimeBasis(
                        sourceTick: timestamp.tick,
                        valueCell: timestamp.sourceCell,
                        group: group.groupNumber,
                        basis: origin,
                        issues: &issues
                    ) {
                        timestamps.append(
                            RebasedTimestamp(tick: tick, sourceCell: timestamp.sourceCell)
                        )
                    }
                }
                spikeColumns.append(
                    RebasedSpikeColumn(plan: spikeColumn.plan, timestamps: timestamps)
                )
            }

            for eventColumn in group.eventColumns {
                var occurrences: [RebasedEventOccurrence] = []
                occurrences.reserveCapacity(eventColumn.occurrences.count)
                for occurrence in eventColumn.occurrences {
                    if let tick = applyTimeBasis(
                        sourceTick: occurrence.sourceTick,
                        valueCell: occurrence.timestampCell,
                        group: group.groupNumber,
                        basis: origin,
                        issues: &issues
                    ) {
                        occurrences.append(
                            RebasedEventOccurrence(
                                tick: tick,
                                timestampCell: occurrence.timestampCell,
                                attributes: occurrence.attributes
                            )
                        )
                    }
                }
                eventColumns.append(
                    RebasedEventColumn(plan: eventColumn.plan, occurrences: occurrences)
                )
            }

            groups.append(
                RebasedGroup(
                    groupNumber: group.groupNumber,
                    plan: group.plan,
                    timeBasis: origin,
                    spikeColumns: spikeColumns,
                    eventColumns: eventColumns
                )
            )
        }
        return groups
    }

    private static func applyTimeBasis(
        sourceTick: MicrosecondTick,
        valueCell: StagedSourceCellReference,
        group: Int,
        basis: ResolvedGroupTimeBasis,
        issues: inout IssueAccumulator
    ) -> MicrosecondTick? {
        switch basis {
        case .recordingElapsed:
            if sourceTick.microseconds < 0 {
                issues.append(
                    .recordingElapsedTimestampIsNegative(group: group, cell: valueCell)
                )
            }
            return sourceTick
        case .eventRelative(let origin):
            do {
                return try sourceTick.rebased(relativeTo: origin.sourceTick)
            } catch is MicrosecondArithmeticError {
                issues.append(
                    .eventRelativeRebaseOverflow(
                        group: group,
                        valueCell: valueCell,
                        originCell: origin.sourceCell
                    )
                )
            } catch {
                issues.append(.resolvedPlanInvariant)
            }
            return nil
        case .unresolved:
            issues.append(.resolvedPlanInvariant)
            return nil
        }
    }

    private static func prepareFinalValues(
        rebasedGroups: [RebasedGroup],
        plan: ResolvedScientificImportPlan,
        issues: inout IssueAccumulator
    ) -> PreparedScientificImport? {
        var groupPairs: [PreparedGroupPair] = []
        groupPairs.reserveCapacity(rebasedGroups.count)

        for group in rebasedGroups {
            var spikePairs: [PreparedSpikePair] = []
            var eventPairs: [PreparedEventPair] = []

            for spikeColumn in group.spikeColumns {
                let descent = descentSummary(spikeColumn.timestamps)
                if spikeColumn.plan.orderDecision == .preserveSourceOrder,
                   let descent {
                    issues.append(
                        .sourceOrderNotAscending(
                            group: group.groupNumber,
                            column: spikeColumn.plan.sourceColumn,
                            descentCount: descent.count,
                            firstPreviousCell: descent.firstPreviousCell,
                            firstCurrentCell: descent.firstCurrentCell
                        )
                    )
                }
                let descentCount = descent?.count ?? 0
                let ordered: [RebasedTimestamp]
                switch spikeColumn.plan.orderDecision {
                case .preserveSourceOrder:
                    ordered = spikeColumn.timestamps
                case .stableAscendingSort:
                    ordered = spikeColumn.timestamps.sorted(by: timestampIsOrderedBefore)
                }

                let timestamps: [MicrosecondTick]
                let timestampSources: [[StagedSourceCellReference]]
                switch spikeColumn.plan.duplicateDecision {
                case .preserveMultiplicity:
                    timestamps = ordered.map(\.tick)
                    timestampSources = ordered.map { [$0.sourceCell] }
                case .collapseExact:
                    if plan.activityMode != .putativeSingleUnit {
                        issues.append(
                            .duplicateCollapseNotAllowed(
                                activityMode: plan.activityMode,
                                group: group.groupNumber,
                                column: spikeColumn.plan.sourceColumn
                            )
                        )
                    }
                    let collapsed = collapseExactTimestamps(ordered)
                    timestamps = collapsed.map(\.tick)
                    timestampSources = collapsed.map(\.sourceCells)
                }

                spikePairs.append(
                    PreparedSpikePair(
                        data: PreparedSpikeTrain(
                            semanticID: spikeColumn.plan.semanticID,
                            timestamps: timestamps
                        ),
                        provenance: PreparedSpikeTrainProvenance(
                            semanticID: spikeColumn.plan.semanticID,
                            sourceColumn: spikeColumn.plan.sourceColumn,
                            orderDecision: spikeColumn.plan.orderDecision,
                            duplicateDecision: spikeColumn.plan.duplicateDecision,
                            sourceOrderDescentCount: descentCount,
                            timestampSources: timestampSources
                        )
                    )
                )
            }

            for eventColumn in group.eventColumns {
                let descent = eventDescentSummary(eventColumn.occurrences)
                if eventColumn.plan.orderDecision == .preserveSourceOrder,
                   let descent {
                    issues.append(
                        .sourceOrderNotAscending(
                            group: group.groupNumber,
                            column: eventColumn.plan.sourceColumn,
                            descentCount: descent.count,
                            firstPreviousCell: descent.firstPreviousCell,
                            firstCurrentCell: descent.firstCurrentCell
                        )
                    )
                }
                let descentCount = descent?.count ?? 0
                let ordered: [RebasedEventOccurrence]
                switch eventColumn.plan.orderDecision {
                case .preserveSourceOrder:
                    ordered = eventColumn.occurrences
                case .stableAscendingSort:
                    ordered = eventColumn.occurrences.sorted(by: eventIsOrderedBefore)
                }

                let dataOccurrences = ordered.map { occurrence in
                    PreparedEventOccurrence(
                        tick: occurrence.tick,
                        scientificAttributes: occurrence.attributes
                            .filter { $0.definition.role == .scientific }
                            .map(\.value),
                        presentationAttributes: occurrence.attributes
                            .filter { $0.definition.role == .presentation }
                            .map(\.value)
                    )
                }
                let provenanceOccurrences = ordered.map { occurrence in
                    PreparedEventOccurrenceProvenance(
                        timestampCell: occurrence.timestampCell,
                        attributes: occurrence.attributes.map(\.provenance)
                    )
                }
                eventPairs.append(
                    PreparedEventPair(
                        data: PreparedEventDefinition(
                            semanticID: eventColumn.plan.semanticID,
                            eventTypeID: eventColumn.plan.eventTypeID,
                            occurrences: dataOccurrences
                        ),
                        provenance: PreparedEventDefinitionProvenance(
                            semanticID: eventColumn.plan.semanticID,
                            sourceColumn: eventColumn.plan.sourceColumn,
                            orderDecision: eventColumn.plan.orderDecision,
                            sourceOrderDescentCount: descentCount,
                            occurrences: provenanceOccurrences
                        )
                    )
                )
            }

            spikePairs.sort {
                utf8TextIsOrderedBefore(
                    semanticIDText($0.data.semanticID),
                    semanticIDText($1.data.semanticID)
                )
            }
            eventPairs.sort {
                utf8TextIsOrderedBefore(
                    semanticIDText($0.data.semanticID),
                    semanticIDText($1.data.semanticID)
                )
            }

            let dataTimeBasis: PreparedEventScopeTimeBasis
            let provenanceTimeBasis: PreparedEventScopeTimeBasisProvenance
            switch group.timeBasis {
            case .recordingElapsed:
                dataTimeBasis = .recordingElapsed
                provenanceTimeBasis = .recordingElapsed
            case .eventRelative(let origin):
                dataTimeBasis = .eventRelative(
                    origin: PreparedEventOrigin(
                        eventDefinitionID: origin.eventDefinitionID,
                        scientificAttributes: origin.scientificAttributes
                    )
                )
                provenanceTimeBasis = .eventRelative(
                    originCell: origin.sourceCell,
                    sourceOriginTick: origin.sourceTick
                )
            case .unresolved:
                issues.append(.resolvedPlanInvariant)
                continue
            }

            groupPairs.append(
                PreparedGroupPair(
                    data: PreparedEventScopeGroup(
                        semanticID: group.plan.semanticID,
                        timeBasis: dataTimeBasis,
                        spikeTrains: spikePairs.map(\.data),
                        eventDefinitions: eventPairs.map(\.data)
                    ),
                    provenance: PreparedEventScopeGroupProvenance(
                        semanticID: group.plan.semanticID,
                        timeBasis: provenanceTimeBasis,
                        spikeTrains: spikePairs.map(\.provenance),
                        eventDefinitions: eventPairs.map(\.provenance)
                    )
                )
            )
        }

        groupPairs.sort {
            utf8TextIsOrderedBefore(
                semanticIDText($0.data.semanticID),
                semanticIDText($1.data.semanticID)
            )
        }

        let orderedDefinitions = plan.eventAttributeDefinitions.sorted {
            utf8TextIsOrderedBefore($0.key.canonicalText, $1.key.canonicalText)
        }
        return PreparedScientificImport(
            data: PreparedScientificImportData(
                activityMode: plan.activityMode,
                eventScopeGroups: groupPairs.map(\.data),
                scientificAttributeDefinitions: orderedDefinitions.filter {
                    $0.role == .scientific
                },
                presentationAttributeDefinitions: orderedDefinitions.filter {
                    $0.role == .presentation
                }
            ),
            provenance: ScientificImportNormalizationProvenance(
                resolvedPlan: plan,
                eventScopeGroups: groupPairs.map(\.provenance)
            )
        )
    }

    private static func parseBoolean(_ source: String) throws -> Bool {
        let maximumUTF8Bytes = ExactIntegerValue.maximumLexemeUTF8ByteCount
        guard source.utf8.prefix(maximumUTF8Bytes + 1).count <= maximumUTF8Bytes else {
            throw BooleanLexemeError.tooLong(maximumUTF8Bytes: maximumUTF8Bytes)
        }
        let bytes = Array(source.utf8)
        var start = 0
        var end = bytes.count
        while start < end, bytes[start] == 0x20 || bytes[start] == 0x09 { start += 1 }
        while end > start, bytes[end - 1] == 0x20 || bytes[end - 1] == 0x09 { end -= 1 }
        guard start < end else { throw BooleanLexemeError.empty }
        let value = bytes[start..<end]
        if value.elementsEqual([0x74, 0x72, 0x75, 0x65]) { return true }
        if value.elementsEqual([0x66, 0x61, 0x6C, 0x73, 0x65]) { return false }
        throw BooleanLexemeError.invalid
    }

    private static func descentSummary(
        _ timestamps: [RebasedTimestamp]
    ) -> DescentSummary? {
        guard timestamps.count > 1 else { return nil }
        var count = 0
        var firstPreviousCell: StagedSourceCellReference?
        var firstCurrentCell: StagedSourceCellReference?
        for index in 1..<timestamps.count where timestamps[index].tick < timestamps[index - 1].tick {
            count += 1
            if firstPreviousCell == nil {
                firstPreviousCell = timestamps[index - 1].sourceCell
                firstCurrentCell = timestamps[index].sourceCell
            }
        }
        guard count > 0, let firstPreviousCell, let firstCurrentCell else { return nil }
        return DescentSummary(
            count: count,
            firstPreviousCell: firstPreviousCell,
            firstCurrentCell: firstCurrentCell
        )
    }

    private static func eventDescentSummary(
        _ occurrences: [RebasedEventOccurrence]
    ) -> DescentSummary? {
        guard occurrences.count > 1 else { return nil }
        var count = 0
        var firstPreviousCell: StagedSourceCellReference?
        var firstCurrentCell: StagedSourceCellReference?
        for index in 1..<occurrences.count
            where occurrences[index].tick < occurrences[index - 1].tick {
            count += 1
            if firstPreviousCell == nil {
                firstPreviousCell = occurrences[index - 1].timestampCell
                firstCurrentCell = occurrences[index].timestampCell
            }
        }
        guard count > 0, let firstPreviousCell, let firstCurrentCell else { return nil }
        return DescentSummary(
            count: count,
            firstPreviousCell: firstPreviousCell,
            firstCurrentCell: firstCurrentCell
        )
    }

    private static func collapseExactTimestamps(
        _ timestamps: [RebasedTimestamp]
    ) -> [CollapsedTimestamp] {
        var collapsed: [CollapsedTimestamp] = []
        collapsed.reserveCapacity(timestamps.count)
        for timestamp in timestamps {
            if let lastIndex = collapsed.indices.last,
               collapsed[lastIndex].tick == timestamp.tick {
                collapsed[lastIndex].sourceCells.append(timestamp.sourceCell)
            } else {
                collapsed.append(
                    CollapsedTimestamp(
                        tick: timestamp.tick,
                        sourceCells: [timestamp.sourceCell]
                    )
                )
            }
        }
        return collapsed
    }

    private static func timestampIsOrderedBefore(
        _ lhs: RebasedTimestamp,
        _ rhs: RebasedTimestamp
    ) -> Bool {
        if lhs.tick != rhs.tick { return lhs.tick < rhs.tick }
        return lhs.sourceCell.oneBasedDataRowIndex < rhs.sourceCell.oneBasedDataRowIndex
    }

    private static func eventIsOrderedBefore(
        _ lhs: RebasedEventOccurrence,
        _ rhs: RebasedEventOccurrence
    ) -> Bool {
        if lhs.tick != rhs.tick { return lhs.tick < rhs.tick }
        return lhs.timestampCell.oneBasedDataRowIndex < rhs.timestampCell.oneBasedDataRowIndex
    }

    private static func sourceColumn(
        _ reference: StagedSourceColumnReference,
        from source: ResolvedScientificImportSource
    ) -> StagedScientificColumn? {
        let offset = reference.oneBasedIndex - 1
        guard source.columns.indices.contains(offset) else { return nil }
        let column = source.columns[offset]
        guard column.sourceColumn == reference else { return nil }
        return column
    }

    private static func sourceCell(
        column: StagedSourceColumnReference,
        oneBasedRow: Int
    ) -> StagedSourceCellReference? {
        do {
            return try StagedSourceCellReference(
                column: column,
                oneBasedDataRowIndex: oneBasedRow
            )
        } catch {
            return nil
        }
    }

    private static func startsWithMetadataMarker(_ text: String) -> Bool {
        text.utf8.first == 0x40
    }

    private static func keyIsOrderedBefore(
        _ lhs: EventAttributeKey,
        _ rhs: EventAttributeKey
    ) -> Bool {
        utf8TextIsOrderedBefore(lhs.canonicalText, rhs.canonicalText)
    }

    private static func utf8TextIsOrderedBefore(_ lhs: String, _ rhs: String) -> Bool {
        lhs.utf8.lexicographicallyPrecedes(rhs.utf8)
    }

    private static func semanticIDText(_ value: ScientificEventScopeGroupID) -> String {
        value.semanticID.canonicalText
    }

    private static func semanticIDText(_ value: ScientificSpikeTrainID) -> String {
        value.semanticID.canonicalText
    }

    private static func semanticIDText(_ value: ScientificEventDefinitionID) -> String {
        value.semanticID.canonicalText
    }
}

private struct IssueAccumulator {
    private(set) var retained: [ScientificImportNormalizationIssue] = []
    private(set) var totalCount = 0

    mutating func append(_ issue: ScientificImportNormalizationIssue) {
        totalCount += 1
        if retained.count < ScientificImportNormalizer.maximumReportedIssueCount {
            retained.append(issue)
        }
    }

    func throwIfPresent() throws {
        guard totalCount > 0 else { return }
        throw ScientificImportNormalizationError(
            issues: retained,
            additionalIssueCount: totalCount - retained.count
        )
    }
}

private enum RawMetadata {
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
}

private struct RawTimestamp {
    let tick: MicrosecondTick
    let sourceCell: StagedSourceCellReference
}

private struct RawSpikeColumn {
    let plan: ResolvedSpikeTrainColumnPlan
    let timestamps: [RawTimestamp]
}

private struct RawEventOccurrence {
    let sourceTick: MicrosecondTick
    let timestampCell: StagedSourceCellReference
    var metadata: [RawMetadata]
}

private struct RawEventColumn {
    let plan: ResolvedEventDefinitionColumnPlan
    let occurrences: [RawEventOccurrence]
}

private struct RawGroup {
    let groupNumber: Int
    let plan: ResolvedEventScopeGroupPlan
    let spikeColumns: [RawSpikeColumn]
    let eventColumns: [RawEventColumn]
}

private struct RawValueLine {
    let rawValue: String
    let sourceCell: StagedSourceCellReference
}

private struct ParsedUnitSuggestion {
    let symbol: OpaqueUnitSymbol
    let sourceCell: StagedSourceCellReference
}

private struct UnitSighting {
    let symbol: OpaqueUnitSymbol
    let sourceCell: StagedSourceCellReference
}

private struct TypedAttribute {
    let definition: ResolvedEventAttributeDefinitionPlan
    let value: PreparedEventAttribute
    let provenance: PreparedEventAttributeProvenance
}

private struct TypedEventOccurrence {
    let sourceTick: MicrosecondTick
    let timestampCell: StagedSourceCellReference
    let attributes: [TypedAttribute]
}

private struct TypedEventColumn {
    let plan: ResolvedEventDefinitionColumnPlan
    let occurrences: [TypedEventOccurrence]
}

private struct TypedGroup {
    let groupNumber: Int
    let plan: ResolvedEventScopeGroupPlan
    let spikeColumns: [RawSpikeColumn]
    let eventColumns: [TypedEventColumn]
}

private struct ResolvedOrigin {
    let eventDefinitionID: ScientificEventDefinitionID
    let sourceTick: MicrosecondTick
    let sourceCell: StagedSourceCellReference
    let scientificAttributes: [PreparedEventAttribute]
}

private enum ResolvedGroupTimeBasis {
    case recordingElapsed
    case eventRelative(ResolvedOrigin)
    case unresolved
}

private struct RebasedTimestamp {
    let tick: MicrosecondTick
    let sourceCell: StagedSourceCellReference
}

private struct RebasedSpikeColumn {
    let plan: ResolvedSpikeTrainColumnPlan
    let timestamps: [RebasedTimestamp]
}

private struct RebasedEventOccurrence {
    let tick: MicrosecondTick
    let timestampCell: StagedSourceCellReference
    let attributes: [TypedAttribute]
}

private struct RebasedEventColumn {
    let plan: ResolvedEventDefinitionColumnPlan
    let occurrences: [RebasedEventOccurrence]
}

private struct RebasedGroup {
    let groupNumber: Int
    let plan: ResolvedEventScopeGroupPlan
    let timeBasis: ResolvedGroupTimeBasis
    let spikeColumns: [RebasedSpikeColumn]
    let eventColumns: [RebasedEventColumn]
}

private struct DescentSummary {
    let count: Int
    let firstPreviousCell: StagedSourceCellReference
    let firstCurrentCell: StagedSourceCellReference
}

private struct CollapsedTimestamp {
    let tick: MicrosecondTick
    var sourceCells: [StagedSourceCellReference]
}

private struct PreparedSpikePair {
    let data: PreparedSpikeTrain
    let provenance: PreparedSpikeTrainProvenance
}

private struct PreparedEventPair {
    let data: PreparedEventDefinition
    let provenance: PreparedEventDefinitionProvenance
}

private struct PreparedGroupPair {
    let data: PreparedEventScopeGroup
    let provenance: PreparedEventScopeGroupProvenance
}

private enum BooleanLexemeError: Error {
    case empty
    case tooLong(maximumUTF8Bytes: Int)
    case invalid
}
