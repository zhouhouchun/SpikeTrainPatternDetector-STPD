/// Independent defensive validation for a prepared scientific import.
///
/// This checkpoint validates the embedded source/plan structure; independently replays lexical
/// and typed source grammar, group-local origin resolution, exact time-coordinate conversion,
/// stable ordering, and spike multiplicity policy; and compares private expected values against
/// the prepared data and derived provenance. It deliberately does not call the normalizer or reuse
/// its transformation helpers. A clean report proves self-consistency only; it grants no authority.
public enum PreparedScientificImportValidator {
    public static let maximumReportedIssueCount = 128
    public static let maximumReportedWarningCount = 128

    public static func validate(
        _ prepared: PreparedScientificImport
    ) -> PreparedScientificImportValidationReport {
        let plan = prepared.provenance.resolvedPlan
        var structuralIssues = PreparedValidationIssueAccumulator()

        validateStructure(
            plan: plan,
            issues: &structuralIssues
        )
        if structuralIssues.totalCount > 0 {
            return makeReport(issues: structuralIssues)
        }

        var lexicalIssues = PreparedValidationIssueAccumulator()
        let rawGroups = replayRawSource(plan: plan, issues: &lexicalIssues)
        if lexicalIssues.totalCount > 0 {
            return makeReport(issues: lexicalIssues)
        }

        var typedIssues = PreparedValidationIssueAccumulator()
        let typedGroups = replayTypedAttributes(
            rawGroups: rawGroups,
            definitions: plan.eventAttributeDefinitions,
            issues: &typedIssues
        )
        if typedIssues.totalCount > 0 {
            return makeReport(issues: typedIssues)
        }

        var originIssues = PreparedValidationIssueAccumulator()
        let timeBases = resolveReplayTimeBases(
            typedGroups: typedGroups,
            issues: &originIssues
        )
        if originIssues.totalCount > 0 {
            return makeReport(issues: originIssues)
        }

        var coordinateIssues = PreparedValidationIssueAccumulator()
        let coordinateGroups = applyReplayTimeBases(
            typedGroups: typedGroups,
            timeBases: timeBases,
            issues: &coordinateIssues
        )
        if coordinateIssues.totalCount > 0 {
            return makeReport(issues: coordinateIssues)
        }

        var orderingIssues = PreparedValidationIssueAccumulator()
        let expected = prepareExpectedReplay(
            coordinateGroups: coordinateGroups,
            plan: plan,
            issues: &orderingIssues
        )
        if orderingIssues.totalCount > 0 {
            return makeReport(issues: orderingIssues)
        }

        var comparisonIssues = PreparedValidationIssueAccumulator()
        compareExpectedReplay(
            expected,
            actual: prepared,
            issues: &comparisonIssues
        )
        if comparisonIssues.totalCount > 0 {
            return makeReport(issues: comparisonIssues)
        }

        var warnings = PreparedValidationWarningAccumulator()
        emitEmptyEventDefinitionWarnings(
            expected: expected,
            warnings: &warnings
        )
        return makeReport(issues: comparisonIssues, warnings: warnings)
    }

    private static func makeReport(
        issues: PreparedValidationIssueAccumulator,
        warnings: PreparedValidationWarningAccumulator = PreparedValidationWarningAccumulator()
    ) -> PreparedScientificImportValidationReport {
        return PreparedScientificImportValidationReport(
            blockingIssues: issues.retained,
            additionalBlockingIssueCount: issues.additionalCount,
            warnings: warnings.retained,
            additionalWarningCount: warnings.additionalCount
        )
    }

    private static func validateStructure(
        plan: ResolvedScientificImportPlan,
        issues: inout PreparedValidationIssueAccumulator
    ) {
        validateSource(plan.source, issues: &issues)

        let groups = plan.eventScopeGroups
        if groups.isEmpty {
            issues.append(
                PreparedScientificImportValidationIssue(
                    kind: .eventScopeGroupsAreEmpty,
                    location: .dataset
                )
            )
        }

        var firstGroupIndexByID: [ScientificEventScopeGroupID: Int] = [:]
        var partition: [PreparedPlanPartitionEntry] = []
        partition.reserveCapacity(
            groups.reduce(into: 0) { count, group in
                count += group.spikeTrains.count + group.eventDefinitions.count
            }
        )

        for (groupOffset, group) in groups.enumerated() {
            let groupIndex = groupOffset + 1
            let groupLocation = PreparedScientificImportValidationLocation(
                groupIndex: groupIndex,
                groupID: group.semanticID
            )

            if let firstGroupIndex = firstGroupIndexByID[group.semanticID] {
                issues.append(
                    PreparedScientificImportValidationIssue(
                        kind: .duplicateGroupSemanticID(firstGroupIndex: firstGroupIndex),
                        location: groupLocation
                    )
                )
            } else {
                firstGroupIndexByID[group.semanticID] = groupIndex
            }

            if group.spikeTrains.isEmpty {
                issues.append(
                    PreparedScientificImportValidationIssue(
                        kind: .groupHasNoSpikeTrains,
                        location: groupLocation
                    )
                )
            }

            var firstSpikeColumnByID: [ScientificSpikeTrainID: StagedSourceColumnReference] = [:]
            for spike in group.spikeTrains {
                let location = PreparedScientificImportValidationLocation(
                    groupIndex: groupIndex,
                    groupID: group.semanticID,
                    spikeTrainID: spike.semanticID,
                    sourceColumn: spike.sourceColumn
                )
                if let firstColumn = firstSpikeColumnByID[spike.semanticID] {
                    issues.append(
                        PreparedScientificImportValidationIssue(
                            kind: .duplicateSpikeTrainSemanticID(firstColumn: firstColumn),
                            location: location
                        )
                    )
                } else {
                    firstSpikeColumnByID[spike.semanticID] = spike.sourceColumn
                }

                if plan.activityMode != .putativeSingleUnit,
                   spike.duplicateDecision == .collapseExact {
                    issues.append(
                        PreparedScientificImportValidationIssue(
                            kind: .duplicateCollapseNotAllowed(
                                activityMode: plan.activityMode
                            ),
                            location: location
                        )
                    )
                }

                partition.append(
                    PreparedPlanPartitionEntry(
                        sourceColumn: spike.sourceColumn,
                        location: location
                    )
                )
            }

            var firstEventColumnByID:
                [ScientificEventDefinitionID: StagedSourceColumnReference] = [:]
            for event in group.eventDefinitions {
                let location = PreparedScientificImportValidationLocation(
                    groupIndex: groupIndex,
                    groupID: group.semanticID,
                    eventDefinitionID: event.semanticID,
                    sourceColumn: event.sourceColumn
                )
                if let firstColumn = firstEventColumnByID[event.semanticID] {
                    issues.append(
                        PreparedScientificImportValidationIssue(
                            kind: .duplicateEventDefinitionSemanticID(firstColumn: firstColumn),
                            location: location
                        )
                    )
                } else {
                    firstEventColumnByID[event.semanticID] = event.sourceColumn
                }

                partition.append(
                    PreparedPlanPartitionEntry(
                        sourceColumn: event.sourceColumn,
                        location: location
                    )
                )
            }

            if case .eventRelative(let origin) = group.timeBasis {
                let location = PreparedScientificImportValidationLocation(
                    groupIndex: groupIndex,
                    groupID: group.semanticID,
                    sourceColumn: origin.timestampCell.column,
                    sourceCell: origin.timestampCell
                )
                if !sourceContains(origin.timestampCell, in: plan.source) {
                    issues.append(
                        PreparedScientificImportValidationIssue(
                            kind: .eventRelativeOriginOutsideSource,
                            location: location
                        )
                    )
                } else if !group.eventDefinitions.contains(where: {
                    $0.sourceColumn == origin.timestampCell.column
                }) {
                    issues.append(
                        PreparedScientificImportValidationIssue(
                            kind: .eventRelativeOriginNotInGroupEventDefinition,
                            location: location
                        )
                    )
                }
            }
        }

        validatePartition(
            partition,
            sourceColumnCount: plan.source.columns.count,
            issues: &issues
        )
        validateHeaderlessContract(plan: plan, issues: &issues)
        validateAttributeDefinitions(plan.eventAttributeDefinitions, issues: &issues)
    }

    private static func validateSource(
        _ source: ResolvedScientificImportSource,
        issues: inout PreparedValidationIssueAccumulator
    ) {
        if source.columns.isEmpty {
            issues.append(
                PreparedScientificImportValidationIssue(
                    kind: .sourceHasNoColumns,
                    location: .dataset
                )
            )
        }
        if source.dataRowCount < 0 {
            issues.append(
                PreparedScientificImportValidationIssue(
                    kind: .sourceDataRowCountIsNegative(actual: source.dataRowCount),
                    location: .dataset
                )
            )
        }

        let expectedHeaderPresence = source.columns.first.map { $0.header != nil }
        for (offset, column) in source.columns.enumerated() {
            let expectedColumnIndex = offset + 1
            let location = PreparedScientificImportValidationLocation(
                sourceColumn: column.sourceColumn
            )
            if column.sourceColumn.oneBasedIndex != expectedColumnIndex {
                issues.append(
                    PreparedScientificImportValidationIssue(
                        kind: .sourceColumnReferenceMismatch(
                            expected: expectedColumnIndex,
                            actual: column.sourceColumn.oneBasedIndex
                        ),
                        location: location
                    )
                )
            }
            if column.cells.count != source.dataRowCount {
                issues.append(
                    PreparedScientificImportValidationIssue(
                        kind: .sourceColumnRowCountMismatch(
                            expected: source.dataRowCount,
                            actual: column.cells.count
                        ),
                        location: location
                    )
                )
            }
            if let expectedHeaderPresence,
               (column.header != nil) != expectedHeaderPresence {
                issues.append(
                    PreparedScientificImportValidationIssue(
                        kind: .sourceHeaderPresenceMismatch,
                        location: location
                    )
                )
            }
        }
    }

    private static func validatePartition(
        _ partition: [PreparedPlanPartitionEntry],
        sourceColumnCount: Int,
        issues: inout PreparedValidationIssueAccumulator
    ) {
        if partition.count != sourceColumnCount {
            issues.append(
                PreparedScientificImportValidationIssue(
                    kind: .sourcePartitionCountMismatch(
                        expected: sourceColumnCount,
                        actual: partition.count
                    ),
                    location: .dataset
                )
            )
        }

        let comparableCount = Swift.min(partition.count, sourceColumnCount)
        guard comparableCount > 0 else { return }
        for offset in 0..<comparableCount {
            let expected = offset + 1
            let actual = partition[offset].sourceColumn.oneBasedIndex
            if actual != expected {
                issues.append(
                    PreparedScientificImportValidationIssue(
                        kind: .sourcePartitionOrderMismatch(
                            position: expected,
                            expected: expected,
                            actual: actual
                        ),
                        location: partition[offset].location
                    )
                )
            }
        }
    }

    private static func validateHeaderlessContract(
        plan: ResolvedScientificImportPlan,
        issues: inout PreparedValidationIssueAccumulator
    ) {
        guard !plan.source.columns.isEmpty,
              plan.source.columns.allSatisfy({ $0.header == nil }),
              !plan.eventScopeGroups.isEmpty else {
            return
        }

        if plan.eventScopeGroups.count != 1 {
            issues.append(
                PreparedScientificImportValidationIssue(
                    kind: .headerlessSourceRequiresSingleGroup(
                        actual: plan.eventScopeGroups.count
                    ),
                    location: .dataset
                )
            )
        }

        for (groupOffset, group) in plan.eventScopeGroups.enumerated() {
            let groupIndex = groupOffset + 1
            for event in group.eventDefinitions {
                issues.append(
                    PreparedScientificImportValidationIssue(
                        kind: .headerlessSourceContainsEventDefinition,
                        location: PreparedScientificImportValidationLocation(
                            groupIndex: groupIndex,
                            groupID: group.semanticID,
                            eventDefinitionID: event.semanticID,
                            sourceColumn: event.sourceColumn
                        )
                    )
                )
            }
            if case .eventRelative = group.timeBasis {
                issues.append(
                    PreparedScientificImportValidationIssue(
                        kind: .headerlessSourceRequiresRecordingElapsed,
                        location: PreparedScientificImportValidationLocation(
                            groupIndex: groupIndex,
                            groupID: group.semanticID
                        )
                    )
                )
            }
        }
    }

    private static func validateAttributeDefinitions(
        _ definitions: [ResolvedEventAttributeDefinitionPlan],
        issues: inout PreparedValidationIssueAccumulator
    ) {
        var firstDefinitionIndexByKey: [EventAttributeKey: Int] = [:]
        for (definitionOffset, definition) in definitions.enumerated() {
            let definitionIndex = definitionOffset + 1
            let location = PreparedScientificImportValidationLocation(
                attributeKey: definition.key
            )
            if let firstDefinitionIndex = firstDefinitionIndexByKey[definition.key] {
                issues.append(
                    PreparedScientificImportValidationIssue(
                        kind: .duplicateEventAttributeDefinitionKey(
                            firstDefinitionIndex: firstDefinitionIndex
                        ),
                        location: location
                    )
                )
            } else {
                firstDefinitionIndexByKey[definition.key] = definitionIndex
            }

            if definition.scalarType != .string,
               definition.emptyStringPolicy == .allowExplicitEmptyString {
                issues.append(
                    PreparedScientificImportValidationIssue(
                        kind: .explicitEmptyStringAllowedForNonString(
                            scalarType: definition.scalarType
                        ),
                        location: location
                    )
                )
            }
        }
    }

    private static func replayRawSource(
        plan: ResolvedScientificImportPlan,
        issues: inout PreparedValidationIssueAccumulator
    ) -> [PreparedValidationRawGroup] {
        var groups: [PreparedValidationRawGroup] = []
        groups.reserveCapacity(plan.eventScopeGroups.count)

        for (groupOffset, groupPlan) in plan.eventScopeGroups.enumerated() {
            let groupIndex = groupOffset + 1
            var spikeColumns: [PreparedValidationRawSpikeColumn] = []
            var eventColumns: [PreparedValidationRawEventColumn] = []
            spikeColumns.reserveCapacity(groupPlan.spikeTrains.count)
            eventColumns.reserveCapacity(groupPlan.eventDefinitions.count)

            for spikePlan in groupPlan.spikeTrains {
                guard let sourceColumn = replaySourceColumn(
                    spikePlan.sourceColumn,
                    from: plan.source
                ) else {
                    issues.append(
                        PreparedScientificImportValidationIssue(
                            kind: .independentReplayInvariant,
                            location: PreparedScientificImportValidationLocation(
                                groupIndex: groupIndex,
                                groupID: groupPlan.semanticID,
                                spikeTrainID: spikePlan.semanticID,
                                sourceColumn: spikePlan.sourceColumn
                            )
                        )
                    )
                    continue
                }

                var timestamps: [PreparedValidationRawTimestamp] = []
                timestamps.reserveCapacity(sourceColumn.cells.count)
                for (rowOffset, value) in sourceColumn.cells.enumerated() {
                    guard let cell = replaySourceCell(
                        column: spikePlan.sourceColumn,
                        row: rowOffset + 1
                    ) else {
                        issues.append(
                            PreparedScientificImportValidationIssue(
                                kind: .independentReplayInvariant,
                                location: PreparedScientificImportValidationLocation(
                                    groupIndex: groupIndex,
                                    groupID: groupPlan.semanticID,
                                    spikeTrainID: spikePlan.semanticID,
                                    sourceColumn: spikePlan.sourceColumn
                                )
                            )
                        )
                        continue
                    }
                    let location = PreparedScientificImportValidationLocation(
                        groupIndex: groupIndex,
                        groupID: groupPlan.semanticID,
                        spikeTrainID: spikePlan.semanticID,
                        sourceColumn: spikePlan.sourceColumn,
                        sourceCell: cell
                    )
                    switch value {
                    case .blank:
                        continue
                    case .spreadsheetNumber(let rawLexeme):
                        guard case .excelWorkbook = plan.source.source else {
                            issues.append(
                                PreparedScientificImportValidationIssue(
                                    kind: .spreadsheetNumberOutsideWorkbook,
                                    location: location
                                )
                            )
                            continue
                        }
                        if let tick = replaySpreadsheetTimestamp(
                            rawLexeme,
                            unit: plan.sourceTimeUnit,
                            location: location,
                            issues: &issues
                        ) {
                            timestamps.append(
                                PreparedValidationRawTimestamp(
                                    tick: tick,
                                    sourceCell: cell
                                )
                            )
                        }
                    case .text(let rawText):
                        if replayStartsWithMetadataMarker(rawText) {
                            issues.append(
                                PreparedScientificImportValidationIssue(
                                    kind: .eventMetadataInSpikeTrain,
                                    location: location
                                )
                            )
                        } else if let tick = replayTimestamp(
                            rawText,
                            unit: plan.sourceTimeUnit,
                            location: location,
                            issues: &issues
                        ) {
                            timestamps.append(
                                PreparedValidationRawTimestamp(
                                    tick: tick,
                                    sourceCell: cell
                                )
                            )
                        }
                    }
                }
                spikeColumns.append(
                    PreparedValidationRawSpikeColumn(
                        plan: spikePlan,
                        timestamps: timestamps
                    )
                )
            }

            for eventPlan in groupPlan.eventDefinitions {
                guard let sourceColumn = replaySourceColumn(
                    eventPlan.sourceColumn,
                    from: plan.source
                ) else {
                    issues.append(
                        PreparedScientificImportValidationIssue(
                            kind: .independentReplayInvariant,
                            location: PreparedScientificImportValidationLocation(
                                groupIndex: groupIndex,
                                groupID: groupPlan.semanticID,
                                eventDefinitionID: eventPlan.semanticID,
                                sourceColumn: eventPlan.sourceColumn
                            )
                        )
                    )
                    continue
                }

                var occurrences: [PreparedValidationRawEventOccurrence] = []
                var openOccurrenceIndex: Int?
                occurrences.reserveCapacity(sourceColumn.cells.count)

                for (rowOffset, value) in sourceColumn.cells.enumerated() {
                    guard let cell = replaySourceCell(
                        column: eventPlan.sourceColumn,
                        row: rowOffset + 1
                    ) else {
                        issues.append(
                            PreparedScientificImportValidationIssue(
                                kind: .independentReplayInvariant,
                                location: PreparedScientificImportValidationLocation(
                                    groupIndex: groupIndex,
                                    groupID: groupPlan.semanticID,
                                    eventDefinitionID: eventPlan.semanticID,
                                    sourceColumn: eventPlan.sourceColumn
                                )
                            )
                        )
                        openOccurrenceIndex = nil
                        continue
                    }
                    let location = PreparedScientificImportValidationLocation(
                        groupIndex: groupIndex,
                        groupID: groupPlan.semanticID,
                        eventDefinitionID: eventPlan.semanticID,
                        sourceColumn: eventPlan.sourceColumn,
                        sourceCell: cell
                    )
                    switch value {
                    case .blank:
                        openOccurrenceIndex = nil
                    case .spreadsheetNumber(let rawLexeme):
                        guard case .excelWorkbook = plan.source.source else {
                            issues.append(
                                PreparedScientificImportValidationIssue(
                                    kind: .spreadsheetNumberOutsideWorkbook,
                                    location: location
                                )
                            )
                            openOccurrenceIndex = nil
                            continue
                        }
                        if let tick = replaySpreadsheetTimestamp(
                            rawLexeme,
                            unit: plan.sourceTimeUnit,
                            location: location,
                            issues: &issues
                        ) {
                            occurrences.append(
                                PreparedValidationRawEventOccurrence(
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
                        if replayStartsWithMetadataMarker(rawText) {
                            guard let openOccurrenceIndex else {
                                issues.append(
                                    PreparedScientificImportValidationIssue(
                                        kind: .eventMetadataWithoutOccurrence,
                                        location: location
                                    )
                                )
                                continue
                            }
                            if let metadata = replayMetadata(
                                rawText,
                                sourceCell: cell,
                                location: location,
                                issues: &issues
                            ) {
                                occurrences[openOccurrenceIndex].metadata.append(metadata)
                            }
                        } else if let tick = replayTimestamp(
                            rawText,
                            unit: plan.sourceTimeUnit,
                            location: location,
                            issues: &issues
                        ) {
                            occurrences.append(
                                PreparedValidationRawEventOccurrence(
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
                eventColumns.append(
                    PreparedValidationRawEventColumn(
                        plan: eventPlan,
                        occurrences: occurrences
                    )
                )
            }

            groups.append(
                PreparedValidationRawGroup(
                    groupIndex: groupIndex,
                    plan: groupPlan,
                    spikeColumns: spikeColumns,
                    eventColumns: eventColumns
                )
            )
        }
        return groups
    }

    private static func replayTimestamp(
        _ rawText: String,
        unit: SpikeTimeUnit,
        location: PreparedScientificImportValidationLocation,
        issues: inout PreparedValidationIssueAccumulator
    ) -> MicrosecondTick? {
        do {
            return try ExactTimestampTextCodec.decode(rawText, sourceUnit: unit)
        } catch let error as ExactTimestampParseError {
            issues.append(
                PreparedScientificImportValidationIssue(
                    kind: .timestampParseFailed(issue: replayTimestampIssue(error)),
                    location: location
                )
            )
        } catch {
            issues.append(
                PreparedScientificImportValidationIssue(
                    kind: .independentReplayInvariant,
                    location: location
                )
            )
        }
        return nil
    }

    private static func replaySpreadsheetTimestamp(
        _ rawLexeme: String,
        unit: SpikeTimeUnit,
        location: PreparedScientificImportValidationLocation,
        issues: inout PreparedValidationIssueAccumulator
    ) -> MicrosecondTick? {
        do {
            return try SpreadsheetNumericTimestampCodec.decode(
                rawLexeme: rawLexeme,
                sourceUnit: unit
            )
        } catch let error as SpreadsheetNumericTimestampDecodeError {
            issues.append(
                PreparedScientificImportValidationIssue(
                    kind: .spreadsheetNumberTimestampDecodeFailed(issue: error),
                    location: location
                )
            )
        } catch {
            issues.append(
                PreparedScientificImportValidationIssue(
                    kind: .independentReplayInvariant,
                    location: location
                )
            )
        }
        return nil
    }

    private static func replayMetadata(
        _ rawText: String,
        sourceCell: StagedSourceCellReference,
        location: PreparedScientificImportValidationLocation,
        issues: inout PreparedValidationIssueAccumulator
    ) -> PreparedValidationRawMetadata? {
        let payload = rawText.dropFirst()
        guard let equalsIndex = payload.firstIndex(of: "=") else {
            issues.append(
                PreparedScientificImportValidationIssue(
                    kind: .eventMetadataMissingEquals,
                    location: location
                )
            )
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
                return .unitSuggestion(
                    key: key,
                    rawValue: rawValue,
                    sourceCell: sourceCell
                )
            }
            return .attribute(
                key: key,
                rawValue: rawValue,
                sourceCell: sourceCell
            )
        } catch let error as EventAttributeKeyError {
            issues.append(
                PreparedScientificImportValidationIssue(
                    kind: .invalidEventAttributeKey(
                        issue: replayEventAttributeKeyIssue(error)
                    ),
                    location: location
                )
            )
        } catch {
            issues.append(
                PreparedScientificImportValidationIssue(
                    kind: .independentReplayInvariant,
                    location: location
                )
            )
        }
        return nil
    }

    private static func replayTypedAttributes(
        rawGroups: [PreparedValidationRawGroup],
        definitions: [ResolvedEventAttributeDefinitionPlan],
        issues: inout PreparedValidationIssueAccumulator
    ) -> [PreparedValidationTypedGroup] {
        var definitionsByKey: [EventAttributeKey: ResolvedEventAttributeDefinitionPlan] = [:]
        for definition in definitions {
            definitionsByKey[definition.key] = definition
        }

        var firstUnitByKey: [EventAttributeKey: PreparedValidationUnitSighting] = [:]
        var groups: [PreparedValidationTypedGroup] = []
        groups.reserveCapacity(rawGroups.count)

        for rawGroup in rawGroups {
            var eventColumns: [PreparedValidationTypedEventColumn] = []
            eventColumns.reserveCapacity(rawGroup.eventColumns.count)

            for rawColumn in rawGroup.eventColumns {
                var occurrences: [PreparedValidationTypedEventOccurrence] = []
                occurrences.reserveCapacity(rawColumn.occurrences.count)

                for rawOccurrence in rawColumn.occurrences {
                    var firstAttributeByKey: [EventAttributeKey: PreparedValidationRawValueLine] = [:]
                    var firstUnitByOccurrenceKey:
                        [EventAttributeKey: PreparedValidationRawValueLine] = [:]

                    for metadata in rawOccurrence.metadata {
                        switch metadata {
                        case .attribute(let key, let rawValue, let sourceCell):
                            let location = PreparedScientificImportValidationLocation(
                                groupIndex: rawGroup.groupIndex,
                                groupID: rawGroup.plan.semanticID,
                                eventDefinitionID: rawColumn.plan.semanticID,
                                attributeKey: key,
                                sourceColumn: rawColumn.plan.sourceColumn,
                                sourceCell: sourceCell
                            )
                            guard definitionsByKey[key] != nil else {
                                issues.append(
                                    PreparedScientificImportValidationIssue(
                                        kind: .unknownEventAttributeKey,
                                        location: location
                                    )
                                )
                                continue
                            }
                            if let first = firstAttributeByKey[key] {
                                issues.append(
                                    PreparedScientificImportValidationIssue(
                                        kind: .duplicateEventAttributeInOccurrence(
                                            eventTimestampCell: rawOccurrence.timestampCell,
                                            firstCell: first.sourceCell
                                        ),
                                        location: location
                                    )
                                )
                            } else {
                                firstAttributeByKey[key] = PreparedValidationRawValueLine(
                                    rawValue: rawValue,
                                    sourceCell: sourceCell
                                )
                            }
                        case .unitSuggestion(let key, let rawValue, let sourceCell):
                            let location = PreparedScientificImportValidationLocation(
                                groupIndex: rawGroup.groupIndex,
                                groupID: rawGroup.plan.semanticID,
                                eventDefinitionID: rawColumn.plan.semanticID,
                                attributeKey: key,
                                sourceColumn: rawColumn.plan.sourceColumn,
                                sourceCell: sourceCell
                            )
                            guard definitionsByKey[key] != nil else {
                                issues.append(
                                    PreparedScientificImportValidationIssue(
                                        kind: .unknownEventAttributeKey,
                                        location: location
                                    )
                                )
                                continue
                            }
                            if let first = firstUnitByOccurrenceKey[key] {
                                issues.append(
                                    PreparedScientificImportValidationIssue(
                                        kind: .duplicateInlineUnitSuggestion(
                                            eventTimestampCell: rawOccurrence.timestampCell,
                                            firstCell: first.sourceCell
                                        ),
                                        location: location
                                    )
                                )
                            } else {
                                firstUnitByOccurrenceKey[key] = PreparedValidationRawValueLine(
                                    rawValue: rawValue,
                                    sourceCell: sourceCell
                                )
                            }
                        }
                    }

                    var acceptedUnits: [EventAttributeKey: PreparedValidationParsedUnit] = [:]
                    let orderedUnitKeys = firstUnitByOccurrenceKey.keys.sorted(
                        by: replayKeyIsOrderedBefore
                    )
                    for key in orderedUnitKeys {
                        guard let line = firstUnitByOccurrenceKey[key],
                              let definition = definitionsByKey[key] else {
                            issues.append(
                                PreparedScientificImportValidationIssue(
                                    kind: .independentReplayInvariant,
                                    location: PreparedScientificImportValidationLocation(
                                        groupIndex: rawGroup.groupIndex,
                                        groupID: rawGroup.plan.semanticID,
                                        eventDefinitionID: rawColumn.plan.semanticID,
                                        attributeKey: key,
                                        sourceColumn: rawColumn.plan.sourceColumn
                                    )
                                )
                            )
                            continue
                        }
                        let location = PreparedScientificImportValidationLocation(
                            groupIndex: rawGroup.groupIndex,
                            groupID: rawGroup.plan.semanticID,
                            eventDefinitionID: rawColumn.plan.semanticID,
                            attributeKey: key,
                            sourceColumn: rawColumn.plan.sourceColumn,
                            sourceCell: line.sourceCell
                        )
                        guard firstAttributeByKey[key] != nil else {
                            issues.append(
                                PreparedScientificImportValidationIssue(
                                    kind: .inlineUnitWithoutAttribute(
                                        eventTimestampCell: rawOccurrence.timestampCell
                                    ),
                                    location: location
                                )
                            )
                            continue
                        }

                        let symbol: OpaqueUnitSymbol
                        do {
                            symbol = try OpaqueUnitSymbol(validating: line.rawValue)
                        } catch let error as OpaqueUnitSymbolError {
                            issues.append(
                                PreparedScientificImportValidationIssue(
                                    kind: .inlineUnitSuggestionInvalid(
                                        issue: replayUnitSymbolIssue(error)
                                    ),
                                    location: location
                                )
                            )
                            continue
                        } catch {
                            issues.append(
                                PreparedScientificImportValidationIssue(
                                    kind: .independentReplayInvariant,
                                    location: location
                                )
                            )
                            continue
                        }

                        if case .specified(let confirmedSymbol) = definition.unit,
                           confirmedSymbol == symbol {
                            acceptedUnits[key] = PreparedValidationParsedUnit(
                                symbol: symbol,
                                sourceCell: line.sourceCell
                            )
                        } else {
                            issues.append(
                                PreparedScientificImportValidationIssue(
                                    kind: .inlineUnitConflictsWithDefinition(
                                        suggested: symbol,
                                        confirmed: definition.unit
                                    ),
                                    location: location
                                )
                            )
                        }

                        if let first = firstUnitByKey[key] {
                            if first.symbol != symbol {
                                issues.append(
                                    PreparedScientificImportValidationIssue(
                                        kind: .inlineUnitSuggestionsConflict(
                                            firstCell: first.sourceCell,
                                            firstUnit: first.symbol,
                                            conflictingUnit: symbol
                                        ),
                                        location: location
                                    )
                                )
                            }
                        } else {
                            firstUnitByKey[key] = PreparedValidationUnitSighting(
                                symbol: symbol,
                                sourceCell: line.sourceCell
                            )
                        }
                    }

                    let orderedAttributeKeys = firstAttributeByKey.keys.sorted(
                        by: replayKeyIsOrderedBefore
                    )
                    var attributes: [PreparedValidationTypedAttribute] = []
                    attributes.reserveCapacity(orderedAttributeKeys.count)
                    for key in orderedAttributeKeys {
                        guard let line = firstAttributeByKey[key],
                              let definition = definitionsByKey[key] else {
                            issues.append(
                                PreparedScientificImportValidationIssue(
                                    kind: .independentReplayInvariant,
                                    location: PreparedScientificImportValidationLocation(
                                        groupIndex: rawGroup.groupIndex,
                                        groupID: rawGroup.plan.semanticID,
                                        eventDefinitionID: rawColumn.plan.semanticID,
                                        attributeKey: key,
                                        sourceColumn: rawColumn.plan.sourceColumn
                                    )
                                )
                            )
                            continue
                        }
                        let location = PreparedScientificImportValidationLocation(
                            groupIndex: rawGroup.groupIndex,
                            groupID: rawGroup.plan.semanticID,
                            eventDefinitionID: rawColumn.plan.semanticID,
                            attributeKey: key,
                            sourceColumn: rawColumn.plan.sourceColumn,
                            sourceCell: line.sourceCell
                        )
                        guard let value = replayAttributeValue(
                            line.rawValue,
                            definition: definition,
                            location: location,
                            issues: &issues
                        ) else {
                            continue
                        }
                        let unit: PreparedValidationExpectedInlineUnit
                        if let acceptedUnit = acceptedUnits[key] {
                            unit = .present(
                                symbol: acceptedUnit.symbol,
                                sourceCell: acceptedUnit.sourceCell
                            )
                        } else {
                            unit = .absent
                        }
                        attributes.append(
                            PreparedValidationTypedAttribute(
                                definition: definition,
                                value: value,
                                valueCell: line.sourceCell,
                                inlineUnit: unit
                            )
                        )
                    }

                    occurrences.append(
                        PreparedValidationTypedEventOccurrence(
                            sourceTick: rawOccurrence.sourceTick,
                            timestampCell: rawOccurrence.timestampCell,
                            attributes: attributes
                        )
                    )
                }

                eventColumns.append(
                    PreparedValidationTypedEventColumn(
                        plan: rawColumn.plan,
                        occurrences: occurrences
                    )
                )
            }

            groups.append(
                PreparedValidationTypedGroup(
                    groupIndex: rawGroup.groupIndex,
                    plan: rawGroup.plan,
                    spikeColumns: rawGroup.spikeColumns,
                    eventColumns: eventColumns
                )
            )
        }
        return groups
    }

    private static func resolveReplayTimeBases(
        typedGroups: [PreparedValidationTypedGroup],
        issues: inout PreparedValidationIssueAccumulator
    ) -> [PreparedValidationResolvedTimeBasis] {
        var results: [PreparedValidationResolvedTimeBasis] = []
        results.reserveCapacity(typedGroups.count)

        for group in typedGroups {
            switch group.plan.timeBasis {
            case .recordingElapsed:
                results.append(.recordingElapsed)
            case .eventRelative(let stagedOrigin):
                let originCell = stagedOrigin.timestampCell
                var selectedDefinitionID: ScientificEventDefinitionID?
                var selectedOccurrence: PreparedValidationTypedEventOccurrence?
                var selectedColumn: PreparedValidationTypedEventColumn?

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

                let plannedDefinition = group.plan.eventDefinitions.first(where: {
                    $0.sourceColumn == originCell.column
                })
                let originLocation = PreparedScientificImportValidationLocation(
                    groupIndex: group.groupIndex,
                    groupID: group.plan.semanticID,
                    eventDefinitionID: plannedDefinition?.semanticID,
                    sourceColumn: originCell.column,
                    sourceCell: originCell
                )
                guard let definitionID = selectedDefinitionID,
                      let occurrence = selectedOccurrence,
                      let eventColumn = selectedColumn else {
                    issues.append(
                        PreparedScientificImportValidationIssue(
                            kind: .selectedOriginIsNotEventTimestamp,
                            location: originLocation
                        )
                    )
                    results.append(.unresolved)
                    continue
                }

                let scientificAttributes = replayScientificAttributes(
                    occurrence.attributes
                )
                let multiplicity = eventColumn.occurrences.reduce(into: 0) {
                    count,
                    candidate in
                    if candidate.sourceTick == occurrence.sourceTick,
                       replayScientificAttributes(candidate.attributes) == scientificAttributes {
                        count += 1
                    }
                }
                if multiplicity > 1 {
                    issues.append(
                        PreparedScientificImportValidationIssue(
                            kind: .selectedOriginAmbiguous(
                                eventDefinitionID: definitionID,
                                multiplicity: multiplicity
                            ),
                            location: PreparedScientificImportValidationLocation(
                                groupIndex: group.groupIndex,
                                groupID: group.plan.semanticID,
                                eventDefinitionID: definitionID,
                                sourceColumn: originCell.column,
                                sourceCell: originCell
                            )
                        )
                    )
                    results.append(.unresolved)
                    continue
                }

                results.append(
                    .eventRelative(
                        PreparedValidationResolvedOrigin(
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

    private static func applyReplayTimeBases(
        typedGroups: [PreparedValidationTypedGroup],
        timeBases: [PreparedValidationResolvedTimeBasis],
        issues: inout PreparedValidationIssueAccumulator
    ) -> [PreparedValidationCoordinateGroup] {
        guard typedGroups.count == timeBases.count else {
            issues.append(
                PreparedScientificImportValidationIssue(
                    kind: .independentReplayInvariant,
                    location: .dataset
                )
            )
            return []
        }

        var groups: [PreparedValidationCoordinateGroup] = []
        groups.reserveCapacity(typedGroups.count)

        for index in typedGroups.indices {
            let group = typedGroups[index]
            let timeBasis = timeBases[index]
            var spikeColumns: [PreparedValidationCoordinateSpikeColumn] = []
            var eventColumns: [PreparedValidationCoordinateEventColumn] = []
            spikeColumns.reserveCapacity(group.spikeColumns.count)
            eventColumns.reserveCapacity(group.eventColumns.count)

            for spikeColumn in group.spikeColumns {
                var timestamps: [PreparedValidationCoordinateTimestamp] = []
                timestamps.reserveCapacity(spikeColumn.timestamps.count)
                for timestamp in spikeColumn.timestamps {
                    let location = PreparedScientificImportValidationLocation(
                        groupIndex: group.groupIndex,
                        groupID: group.plan.semanticID,
                        spikeTrainID: spikeColumn.plan.semanticID,
                        sourceColumn: spikeColumn.plan.sourceColumn,
                        sourceCell: timestamp.sourceCell
                    )
                    if let tick = applyReplayTimeBasis(
                        sourceTick: timestamp.tick,
                        timeBasis: timeBasis,
                        location: location,
                        issues: &issues
                    ) {
                        timestamps.append(
                            PreparedValidationCoordinateTimestamp(
                                tick: tick,
                                sourceCell: timestamp.sourceCell
                            )
                        )
                    }
                }
                spikeColumns.append(
                    PreparedValidationCoordinateSpikeColumn(
                        plan: spikeColumn.plan,
                        timestamps: timestamps
                    )
                )
            }

            for eventColumn in group.eventColumns {
                var occurrences: [PreparedValidationCoordinateEventOccurrence] = []
                occurrences.reserveCapacity(eventColumn.occurrences.count)
                for occurrence in eventColumn.occurrences {
                    let location = PreparedScientificImportValidationLocation(
                        groupIndex: group.groupIndex,
                        groupID: group.plan.semanticID,
                        eventDefinitionID: eventColumn.plan.semanticID,
                        sourceColumn: eventColumn.plan.sourceColumn,
                        sourceCell: occurrence.timestampCell
                    )
                    if let tick = applyReplayTimeBasis(
                        sourceTick: occurrence.sourceTick,
                        timeBasis: timeBasis,
                        location: location,
                        issues: &issues
                    ) {
                        occurrences.append(
                            PreparedValidationCoordinateEventOccurrence(
                                tick: tick,
                                timestampCell: occurrence.timestampCell,
                                attributes: occurrence.attributes
                            )
                        )
                    }
                }
                eventColumns.append(
                    PreparedValidationCoordinateEventColumn(
                        plan: eventColumn.plan,
                        occurrences: occurrences
                    )
                )
            }

            groups.append(
                PreparedValidationCoordinateGroup(
                    groupIndex: group.groupIndex,
                    plan: group.plan,
                    timeBasis: timeBasis,
                    spikeColumns: spikeColumns,
                    eventColumns: eventColumns
                )
            )
        }
        return groups
    }

    private static func applyReplayTimeBasis(
        sourceTick: MicrosecondTick,
        timeBasis: PreparedValidationResolvedTimeBasis,
        location: PreparedScientificImportValidationLocation,
        issues: inout PreparedValidationIssueAccumulator
    ) -> MicrosecondTick? {
        switch timeBasis {
        case .recordingElapsed:
            if sourceTick.microseconds < 0 {
                issues.append(
                    PreparedScientificImportValidationIssue(
                        kind: .recordingElapsedTimestampIsNegative,
                        location: location
                    )
                )
            }
            return sourceTick
        case .eventRelative(let origin):
            let (rebased, overflow) = sourceTick.microseconds.subtractingReportingOverflow(
                origin.sourceTick.microseconds
            )
            guard !overflow else {
                issues.append(
                    PreparedScientificImportValidationIssue(
                        kind: .eventRelativeRebaseOverflow(
                            originCell: origin.sourceCell
                        ),
                        location: location
                    )
                )
                return nil
            }
            return MicrosecondTick(microseconds: rebased)
        case .unresolved:
            issues.append(
                PreparedScientificImportValidationIssue(
                    kind: .independentReplayInvariant,
                    location: location
                )
            )
            return nil
        }
    }

    private static func prepareExpectedReplay(
        coordinateGroups: [PreparedValidationCoordinateGroup],
        plan: ResolvedScientificImportPlan,
        issues: inout PreparedValidationIssueAccumulator
    ) -> PreparedValidationExpectedImport {
        var groupPairs: [PreparedValidationExpectedGroupPair] = []
        groupPairs.reserveCapacity(coordinateGroups.count)

        for group in coordinateGroups {
            var spikePairs: [PreparedValidationExpectedSpikePair] = []
            var eventPairs: [PreparedValidationExpectedEventPair] = []

            for spikeColumn in group.spikeColumns {
                let descent = replayTimestampDescent(spikeColumn.timestamps)
                if spikeColumn.plan.orderDecision == .preserveSourceOrder,
                   let descent {
                    issues.append(
                        PreparedScientificImportValidationIssue(
                            kind: .sourceOrderNotAscending(
                                descentCount: descent.count,
                                firstPreviousCell: descent.firstPreviousCell,
                                firstCurrentCell: descent.firstCurrentCell
                            ),
                            location: PreparedScientificImportValidationLocation(
                                groupIndex: group.groupIndex,
                                groupID: group.plan.semanticID,
                                spikeTrainID: spikeColumn.plan.semanticID,
                                sourceColumn: spikeColumn.plan.sourceColumn
                            )
                        )
                    )
                }
                let descentCount = descent?.count ?? 0
                let ordered: [PreparedValidationCoordinateTimestamp]
                switch spikeColumn.plan.orderDecision {
                case .preserveSourceOrder:
                    ordered = spikeColumn.timestamps
                case .stableAscendingSort:
                    ordered = spikeColumn.timestamps.sorted(
                        by: replayTimestampIsOrderedBefore
                    )
                }

                let finalTimestamps: [MicrosecondTick]
                let timestampSources: [[StagedSourceCellReference]]
                switch spikeColumn.plan.duplicateDecision {
                case .preserveMultiplicity:
                    finalTimestamps = ordered.map(\.tick)
                    timestampSources = ordered.map { [$0.sourceCell] }
                case .collapseExact:
                    if plan.activityMode != .putativeSingleUnit {
                        issues.append(
                            PreparedScientificImportValidationIssue(
                                kind: .independentReplayInvariant,
                                location: PreparedScientificImportValidationLocation(
                                    groupIndex: group.groupIndex,
                                    groupID: group.plan.semanticID,
                                    spikeTrainID: spikeColumn.plan.semanticID,
                                    sourceColumn: spikeColumn.plan.sourceColumn
                                )
                            )
                        )
                    }
                    let collapsed = replayCollapseExactTimestamps(ordered)
                    finalTimestamps = collapsed.map(\.tick)
                    timestampSources = collapsed.map(\.sourceCells)
                }

                spikePairs.append(
                    PreparedValidationExpectedSpikePair(
                        data: PreparedValidationExpectedSpikeTrain(
                            semanticID: spikeColumn.plan.semanticID,
                            timestamps: finalTimestamps
                        ),
                        provenance: PreparedValidationExpectedSpikeProvenance(
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
                let descent = replayEventDescent(eventColumn.occurrences)
                if eventColumn.plan.orderDecision == .preserveSourceOrder,
                   let descent {
                    issues.append(
                        PreparedScientificImportValidationIssue(
                            kind: .sourceOrderNotAscending(
                                descentCount: descent.count,
                                firstPreviousCell: descent.firstPreviousCell,
                                firstCurrentCell: descent.firstCurrentCell
                            ),
                            location: PreparedScientificImportValidationLocation(
                                groupIndex: group.groupIndex,
                                groupID: group.plan.semanticID,
                                eventDefinitionID: eventColumn.plan.semanticID,
                                sourceColumn: eventColumn.plan.sourceColumn
                            )
                        )
                    )
                }
                let descentCount = descent?.count ?? 0
                let ordered: [PreparedValidationCoordinateEventOccurrence]
                switch eventColumn.plan.orderDecision {
                case .preserveSourceOrder:
                    ordered = eventColumn.occurrences
                case .stableAscendingSort:
                    ordered = eventColumn.occurrences.sorted(
                        by: replayEventIsOrderedBefore
                    )
                }

                let dataOccurrences = ordered.map { occurrence in
                    PreparedValidationExpectedEventOccurrence(
                        tick: occurrence.tick,
                        scientificAttributes: replayExpectedAttributes(
                            occurrence.attributes,
                            role: .scientific
                        ),
                        presentationAttributes: replayExpectedAttributes(
                            occurrence.attributes,
                            role: .presentation
                        )
                    )
                }
                let provenanceOccurrences = ordered.map { occurrence in
                    PreparedValidationExpectedEventOccurrenceProvenance(
                        timestampCell: occurrence.timestampCell,
                        attributes: occurrence.attributes.map {
                            PreparedValidationExpectedAttributeProvenance(
                                key: $0.definition.key,
                                valueCell: $0.valueCell,
                                inlineUnit: $0.inlineUnit
                            )
                        }
                    )
                }
                eventPairs.append(
                    PreparedValidationExpectedEventPair(
                        data: PreparedValidationExpectedEventDefinition(
                            semanticID: eventColumn.plan.semanticID,
                            eventTypeID: eventColumn.plan.eventTypeID,
                            occurrences: dataOccurrences
                        ),
                        provenance: PreparedValidationExpectedEventProvenance(
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
                replayTextIsOrderedBefore(
                    $0.data.semanticID.semanticID.canonicalText,
                    $1.data.semanticID.semanticID.canonicalText
                )
            }
            eventPairs.sort {
                replayTextIsOrderedBefore(
                    $0.data.semanticID.semanticID.canonicalText,
                    $1.data.semanticID.semanticID.canonicalText
                )
            }

            let dataTimeBasis: PreparedValidationExpectedDataTimeBasis
            let provenanceTimeBasis: PreparedValidationExpectedProvenanceTimeBasis
            switch group.timeBasis {
            case .recordingElapsed:
                dataTimeBasis = .recordingElapsed
                provenanceTimeBasis = .recordingElapsed
            case .eventRelative(let origin):
                dataTimeBasis = .eventRelative(
                    origin: PreparedValidationExpectedEventOrigin(
                        eventDefinitionID: origin.eventDefinitionID,
                        tick: .zero,
                        scientificAttributes: origin.scientificAttributes
                    )
                )
                provenanceTimeBasis = .eventRelative(
                    originCell: origin.sourceCell,
                    sourceOriginTick: origin.sourceTick
                )
            case .unresolved:
                issues.append(
                    PreparedScientificImportValidationIssue(
                        kind: .independentReplayInvariant,
                        location: PreparedScientificImportValidationLocation(
                            groupIndex: group.groupIndex,
                            groupID: group.plan.semanticID
                        )
                    )
                )
                dataTimeBasis = .recordingElapsed
                provenanceTimeBasis = .recordingElapsed
            }

            groupPairs.append(
                PreparedValidationExpectedGroupPair(
                    data: PreparedValidationExpectedGroupData(
                        originalPlanIndex: group.groupIndex,
                        semanticID: group.plan.semanticID,
                        timeBasis: dataTimeBasis,
                        spikeTrains: spikePairs.map(\.data),
                        eventDefinitions: eventPairs.map(\.data)
                    ),
                    provenance: PreparedValidationExpectedGroupProvenance(
                        originalPlanIndex: group.groupIndex,
                        semanticID: group.plan.semanticID,
                        timeBasis: provenanceTimeBasis,
                        spikeTrains: spikePairs.map(\.provenance),
                        eventDefinitions: eventPairs.map(\.provenance)
                    )
                )
            )
        }

        groupPairs.sort {
            replayTextIsOrderedBefore(
                $0.data.semanticID.semanticID.canonicalText,
                $1.data.semanticID.semanticID.canonicalText
            )
        }
        let orderedDefinitions = plan.eventAttributeDefinitions.sorted {
            replayKeyIsOrderedBefore($0.key, $1.key)
        }
        return PreparedValidationExpectedImport(
            data: PreparedValidationExpectedImportData(
                activityMode: plan.activityMode,
                groups: groupPairs.map(\.data),
                scientificAttributeDefinitions: orderedDefinitions.filter {
                    $0.role == .scientific
                },
                presentationAttributeDefinitions: orderedDefinitions.filter {
                    $0.role == .presentation
                }
            ),
            provenance: PreparedValidationExpectedImportProvenance(
                groups: groupPairs.map(\.provenance)
            )
        )
    }

    private static func compareExpectedReplay(
        _ expected: PreparedValidationExpectedImport,
        actual: PreparedScientificImport,
        issues: inout PreparedValidationIssueAccumulator
    ) {
        if actual.data.activityMode != expected.data.activityMode {
            appendComparisonMismatch(
                .dataActivityMode,
                location: .dataset,
                issues: &issues
            )
        }
        compareAttributeDefinitions(
            expected: expected.data.scientificAttributeDefinitions,
            actual: actual.data.scientificAttributeDefinitions,
            countField: .dataScientificAttributeDefinitionCount,
            keyField: { .dataScientificAttributeDefinitionKey(position: $0) },
            scalarTypeField: {
                .dataScientificAttributeDefinitionScalarType(position: $0)
            },
            roleField: { .dataScientificAttributeDefinitionRole(position: $0) },
            unitField: { .dataScientificAttributeDefinitionUnit(position: $0) },
            emptyStringField: {
                .dataScientificAttributeDefinitionEmptyStringPolicy(position: $0)
            },
            issues: &issues
        )
        compareAttributeDefinitions(
            expected: expected.data.presentationAttributeDefinitions,
            actual: actual.data.presentationAttributeDefinitions,
            countField: .dataPresentationAttributeDefinitionCount,
            keyField: { .dataPresentationAttributeDefinitionKey(position: $0) },
            scalarTypeField: {
                .dataPresentationAttributeDefinitionScalarType(position: $0)
            },
            roleField: { .dataPresentationAttributeDefinitionRole(position: $0) },
            unitField: { .dataPresentationAttributeDefinitionUnit(position: $0) },
            emptyStringField: {
                .dataPresentationAttributeDefinitionEmptyStringPolicy(position: $0)
            },
            issues: &issues
        )
        compareDataGroupTopology(
            expected: expected.data.groups,
            expectedProvenance: expected.provenance.groups,
            actual: actual.data.eventScopeGroups,
            issues: &issues
        )
        compareProvenanceGroupTopology(
            expected: expected.provenance.groups,
            actual: actual.provenance.eventScopeGroups,
            issues: &issues
        )
    }

    private static func compareAttributeDefinitions(
        expected: [ResolvedEventAttributeDefinitionPlan],
        actual: [ResolvedEventAttributeDefinitionPlan],
        countField: PreparedScientificImportComparisonField,
        keyField: (Int) -> PreparedScientificImportComparisonField,
        scalarTypeField: (Int) -> PreparedScientificImportComparisonField,
        roleField: (Int) -> PreparedScientificImportComparisonField,
        unitField: (Int) -> PreparedScientificImportComparisonField,
        emptyStringField: (Int) -> PreparedScientificImportComparisonField,
        issues: inout PreparedValidationIssueAccumulator
    ) {
        guard expected.count == actual.count else {
            appendComparisonCountMismatch(
                countField,
                expected: expected.count,
                actual: actual.count,
                location: .dataset,
                issues: &issues
            )
            return
        }

        for offset in expected.indices {
            let oneBasedPosition = offset + 1
            let expectedDefinition = expected[offset]
            let actualDefinition = actual[offset]
            let location = PreparedScientificImportValidationLocation(
                attributeKey: expectedDefinition.key
            )
            guard expectedDefinition.key == actualDefinition.key else {
                appendComparisonMismatch(
                    keyField(oneBasedPosition),
                    location: location,
                    issues: &issues
                )
                continue
            }
            if expectedDefinition.scalarType != actualDefinition.scalarType {
                appendComparisonMismatch(
                    scalarTypeField(oneBasedPosition),
                    location: location,
                    issues: &issues
                )
            }
            if expectedDefinition.role != actualDefinition.role {
                appendComparisonMismatch(
                    roleField(oneBasedPosition),
                    location: location,
                    issues: &issues
                )
            }
            if expectedDefinition.unit != actualDefinition.unit {
                appendComparisonMismatch(
                    unitField(oneBasedPosition),
                    location: location,
                    issues: &issues
                )
            }
            if expectedDefinition.emptyStringPolicy != actualDefinition.emptyStringPolicy {
                appendComparisonMismatch(
                    emptyStringField(oneBasedPosition),
                    location: location,
                    issues: &issues
                )
            }
        }
    }

    private static func compareDataGroupTopology(
        expected: [PreparedValidationExpectedGroupData],
        expectedProvenance: [PreparedValidationExpectedGroupProvenance],
        actual: [PreparedEventScopeGroup],
        issues: inout PreparedValidationIssueAccumulator
    ) {
        guard expected.count == expectedProvenance.count else {
            issues.append(
                PreparedScientificImportValidationIssue(
                    kind: .independentReplayInvariant,
                    location: .dataset
                )
            )
            return
        }
        guard expected.count == actual.count else {
            appendComparisonCountMismatch(
                .dataGroupCount,
                expected: expected.count,
                actual: actual.count,
                location: .dataset,
                issues: &issues
            )
            return
        }

        for offset in expected.indices {
            let expectedGroup = expected[offset]
            let location = comparisonGroupLocation(expectedGroup)
            guard expectedGroup.semanticID == actual[offset].semanticID else {
                appendComparisonMismatch(
                    .dataGroupSemanticID(position: offset + 1),
                    location: location,
                    issues: &issues
                )
                continue
            }
            compareDataGroup(
                expected: expectedGroup,
                expectedProvenance: expectedProvenance[offset],
                actual: actual[offset],
                issues: &issues
            )
        }
    }

    private static func compareProvenanceGroupTopology(
        expected: [PreparedValidationExpectedGroupProvenance],
        actual: [PreparedEventScopeGroupProvenance],
        issues: inout PreparedValidationIssueAccumulator
    ) {
        guard expected.count == actual.count else {
            appendComparisonCountMismatch(
                .provenanceGroupCount,
                expected: expected.count,
                actual: actual.count,
                location: .dataset,
                issues: &issues
            )
            return
        }

        for offset in expected.indices {
            let expectedGroup = expected[offset]
            let location = comparisonGroupLocation(expectedGroup)
            guard expectedGroup.semanticID == actual[offset].semanticID else {
                appendComparisonMismatch(
                    .provenanceGroupSemanticID(position: offset + 1),
                    location: location,
                    issues: &issues
                )
                continue
            }
            compareProvenanceGroup(
                expected: expectedGroup,
                actual: actual[offset],
                issues: &issues
            )
        }
    }

    private static func compareDataGroup(
        expected: PreparedValidationExpectedGroupData,
        expectedProvenance: PreparedValidationExpectedGroupProvenance,
        actual: PreparedEventScopeGroup,
        issues: inout PreparedValidationIssueAccumulator
    ) {
        guard expected.semanticID == expectedProvenance.semanticID,
              expected.originalPlanIndex == expectedProvenance.originalPlanIndex else {
            issues.append(
                PreparedScientificImportValidationIssue(
                    kind: .independentReplayInvariant,
                    location: comparisonGroupLocation(expected)
                )
            )
            return
        }
        _ = compareDataTimeBasis(
            expected: expected.timeBasis,
            expectedProvenance: expectedProvenance,
            group: expected,
            actual: actual.timeBasis,
            issues: &issues
        )
        compareDataSpikeTrains(
            expected: expected.spikeTrains,
            expectedProvenance: expectedProvenance.spikeTrains,
            group: expected,
            actual: actual.spikeTrains,
            issues: &issues
        )
        compareDataEventDefinitions(
            expected: expected.eventDefinitions,
            expectedProvenance: expectedProvenance.eventDefinitions,
            group: expected,
            actual: actual.eventDefinitions,
            issues: &issues
        )
    }

    private static func compareDataTimeBasis(
        expected: PreparedValidationExpectedDataTimeBasis,
        expectedProvenance: PreparedValidationExpectedGroupProvenance,
        group: PreparedValidationExpectedGroupData,
        actual: PreparedEventScopeTimeBasis,
        issues: inout PreparedValidationIssueAccumulator
    ) -> Bool {
        switch (expected, actual) {
        case (.recordingElapsed, .recordingElapsed):
            return true
        case (.eventRelative(let expectedOrigin), .eventRelative(let actualOrigin)):
            let originCell: StagedSourceCellReference?
            switch expectedProvenance.timeBasis {
            case .eventRelative(let cell, _):
                originCell = cell
            case .recordingElapsed:
                originCell = nil
            }
            let baseLocation = PreparedScientificImportValidationLocation(
                groupIndex: group.originalPlanIndex,
                groupID: group.semanticID,
                eventDefinitionID: expectedOrigin.eventDefinitionID,
                sourceColumn: originCell?.column,
                sourceCell: originCell
            )
            guard expectedOrigin.eventDefinitionID == actualOrigin.eventDefinitionID else {
                appendComparisonMismatch(
                    .dataGroupEventOriginDefinitionID,
                    location: baseLocation,
                    issues: &issues
                )
                return false
            }
            guard expectedOrigin.tick == actualOrigin.tick else {
                appendComparisonMismatch(
                    .dataGroupEventOriginTick,
                    location: baseLocation,
                    issues: &issues
                )
                return false
            }
            let issueCountBeforeAttributes = issues.totalCount
            let originAttributeTraces = originCell.flatMap { selectedCell in
                expectedProvenance.eventDefinitions
                    .first(where: { $0.semanticID == expectedOrigin.eventDefinitionID })?
                    .occurrences
                    .first(where: { $0.timestampCell == selectedCell })?
                    .attributes
            }
            compareDataAttributes(
                expected: expectedOrigin.scientificAttributes,
                actual: actualOrigin.scientificAttributes,
                countField: .dataGroupEventOriginScientificAttributeCount,
                keyField: {
                    .dataGroupEventOriginScientificAttributeKey(position: $0)
                },
                valueField: {
                    .dataGroupEventOriginScientificAttributeValue(position: $0)
                },
                baseLocation: baseLocation,
                expectedTraces: originAttributeTraces,
                issues: &issues
            )
            return issues.totalCount == issueCountBeforeAttributes
        default:
            appendComparisonMismatch(
                .dataGroupTimeBasis,
                location: comparisonGroupLocation(group),
                issues: &issues
            )
            return false
        }
    }

    private static func compareDataSpikeTrains(
        expected: [PreparedValidationExpectedSpikeTrain],
        expectedProvenance: [PreparedValidationExpectedSpikeProvenance],
        group: PreparedValidationExpectedGroupData,
        actual: [PreparedSpikeTrain],
        issues: inout PreparedValidationIssueAccumulator
    ) {
        guard expected.count == expectedProvenance.count else {
            issues.append(
                PreparedScientificImportValidationIssue(
                    kind: .independentReplayInvariant,
                    location: comparisonGroupLocation(group)
                )
            )
            return
        }
        guard expected.count == actual.count else {
            appendComparisonCountMismatch(
                .dataSpikeTrainCount,
                expected: expected.count,
                actual: actual.count,
                location: comparisonGroupLocation(group),
                issues: &issues
            )
            return
        }

        for offset in expected.indices {
            let expectedTrain = expected[offset]
            let expectedTrace = expectedProvenance[offset]
            let location = PreparedScientificImportValidationLocation(
                groupIndex: group.originalPlanIndex,
                groupID: group.semanticID,
                spikeTrainID: expectedTrain.semanticID,
                sourceColumn: expectedTrace.sourceColumn
            )
            guard expectedTrain.semanticID == expectedTrace.semanticID else {
                issues.append(
                    PreparedScientificImportValidationIssue(
                        kind: .independentReplayInvariant,
                        location: location
                    )
                )
                continue
            }
            guard expectedTrain.semanticID == actual[offset].semanticID else {
                appendComparisonMismatch(
                    .dataSpikeTrainSemanticID(position: offset + 1),
                    location: location,
                    issues: &issues
                )
                continue
            }
            guard expectedTrain.timestamps.count == actual[offset].timestamps.count else {
                appendComparisonCountMismatch(
                    .dataSpikeTimestampCount,
                    expected: expectedTrain.timestamps.count,
                    actual: actual[offset].timestamps.count,
                    location: location,
                    issues: &issues
                )
                continue
            }
            for timestampOffset in expectedTrain.timestamps.indices
                where expectedTrain.timestamps[timestampOffset]
                    != actual[offset].timestamps[timestampOffset] {
                appendComparisonMismatch(
                    .dataSpikeTimestamp(position: timestampOffset + 1),
                    location: comparisonLocation(
                        from: location,
                        sourceCell: expectedTrace.timestampSources.indices.contains(timestampOffset)
                            ? expectedTrace.timestampSources[timestampOffset].first
                            : nil
                    ),
                    issues: &issues
                )
            }
        }
    }

    private static func compareDataEventDefinitions(
        expected: [PreparedValidationExpectedEventDefinition],
        expectedProvenance: [PreparedValidationExpectedEventProvenance],
        group: PreparedValidationExpectedGroupData,
        actual: [PreparedEventDefinition],
        issues: inout PreparedValidationIssueAccumulator
    ) {
        guard expected.count == expectedProvenance.count else {
            issues.append(
                PreparedScientificImportValidationIssue(
                    kind: .independentReplayInvariant,
                    location: comparisonGroupLocation(group)
                )
            )
            return
        }
        guard expected.count == actual.count else {
            appendComparisonCountMismatch(
                .dataEventDefinitionCount,
                expected: expected.count,
                actual: actual.count,
                location: comparisonGroupLocation(group),
                issues: &issues
            )
            return
        }

        for offset in expected.indices {
            let expectedDefinition = expected[offset]
            let expectedTrace = expectedProvenance[offset]
            let location = PreparedScientificImportValidationLocation(
                groupIndex: group.originalPlanIndex,
                groupID: group.semanticID,
                eventDefinitionID: expectedDefinition.semanticID,
                sourceColumn: expectedTrace.sourceColumn
            )
            guard expectedDefinition.semanticID == expectedTrace.semanticID else {
                issues.append(
                    PreparedScientificImportValidationIssue(
                        kind: .independentReplayInvariant,
                        location: location
                    )
                )
                continue
            }
            let actualDefinition = actual[offset]
            guard expectedDefinition.semanticID == actualDefinition.semanticID else {
                appendComparisonMismatch(
                    .dataEventDefinitionSemanticID(position: offset + 1),
                    location: location,
                    issues: &issues
                )
                continue
            }
            if expectedDefinition.eventTypeID != actualDefinition.eventTypeID {
                appendComparisonMismatch(
                    .dataEventTypeID,
                    location: location,
                    issues: &issues
                )
            }
            compareDataEventOccurrences(
                expected: expectedDefinition.occurrences,
                expectedProvenance: expectedTrace.occurrences,
                baseLocation: location,
                actual: actualDefinition.occurrences,
                issues: &issues
            )
        }
    }

    private static func compareDataEventOccurrences(
        expected: [PreparedValidationExpectedEventOccurrence],
        expectedProvenance: [PreparedValidationExpectedEventOccurrenceProvenance],
        baseLocation: PreparedScientificImportValidationLocation,
        actual: [PreparedEventOccurrence],
        issues: inout PreparedValidationIssueAccumulator
    ) {
        guard expected.count == expectedProvenance.count else {
            issues.append(
                PreparedScientificImportValidationIssue(
                    kind: .independentReplayInvariant,
                    location: baseLocation
                )
            )
            return
        }
        guard expected.count == actual.count else {
            appendComparisonCountMismatch(
                .dataEventOccurrenceCount,
                expected: expected.count,
                actual: actual.count,
                location: baseLocation,
                issues: &issues
            )
            return
        }

        for offset in expected.indices {
            let occurrenceLocation = comparisonLocation(
                from: baseLocation,
                sourceCell: expectedProvenance[offset].timestampCell
            )
            guard expected[offset].tick == actual[offset].tick else {
                appendComparisonMismatch(
                    .dataEventOccurrenceTick(position: offset + 1),
                    location: occurrenceLocation,
                    issues: &issues
                )
                continue
            }
            compareDataAttributes(
                expected: expected[offset].scientificAttributes,
                actual: actual[offset].scientificAttributes,
                countField: .dataEventOccurrenceScientificAttributeCount(
                    occurrence: offset + 1
                ),
                keyField: {
                    .dataEventOccurrenceScientificAttributeKey(
                        occurrence: offset + 1,
                        position: $0
                    )
                },
                valueField: {
                    .dataEventOccurrenceScientificAttributeValue(
                        occurrence: offset + 1,
                        position: $0
                    )
                },
                baseLocation: occurrenceLocation,
                expectedTraces: expectedProvenance[offset].attributes,
                issues: &issues
            )
            compareDataAttributes(
                expected: expected[offset].presentationAttributes,
                actual: actual[offset].presentationAttributes,
                countField: .dataEventOccurrencePresentationAttributeCount(
                    occurrence: offset + 1
                ),
                keyField: {
                    .dataEventOccurrencePresentationAttributeKey(
                        occurrence: offset + 1,
                        position: $0
                    )
                },
                valueField: {
                    .dataEventOccurrencePresentationAttributeValue(
                        occurrence: offset + 1,
                        position: $0
                    )
                },
                baseLocation: occurrenceLocation,
                expectedTraces: expectedProvenance[offset].attributes,
                issues: &issues
            )
        }
    }

    private static func compareDataAttributes(
        expected: [PreparedValidationExpectedEventAttribute],
        actual: [PreparedEventAttribute],
        countField: PreparedScientificImportComparisonField,
        keyField: (Int) -> PreparedScientificImportComparisonField,
        valueField: (Int) -> PreparedScientificImportComparisonField,
        baseLocation: PreparedScientificImportValidationLocation,
        expectedTraces: [PreparedValidationExpectedAttributeProvenance]?,
        issues: inout PreparedValidationIssueAccumulator
    ) {
        guard expected.count == actual.count else {
            appendComparisonCountMismatch(
                countField,
                expected: expected.count,
                actual: actual.count,
                location: baseLocation,
                issues: &issues
            )
            return
        }
        for offset in expected.indices {
            let expectedAttribute = expected[offset]
            let sourceCell = expectedTraces?.first(where: {
                $0.key == expectedAttribute.key
            })?.valueCell
            let location = comparisonLocation(
                from: baseLocation,
                attributeKey: expectedAttribute.key,
                sourceCell: sourceCell
            )
            guard expectedAttribute.key == actual[offset].key else {
                appendComparisonMismatch(
                    keyField(offset + 1),
                    location: location,
                    issues: &issues
                )
                continue
            }
            if expectedAttribute.value != actual[offset].value {
                appendComparisonMismatch(
                    valueField(offset + 1),
                    location: location,
                    issues: &issues
                )
            }
        }
    }

    private static func compareProvenanceGroup(
        expected: PreparedValidationExpectedGroupProvenance,
        actual: PreparedEventScopeGroupProvenance,
        issues: inout PreparedValidationIssueAccumulator
    ) {
        _ = compareProvenanceTimeBasis(
            expected: expected.timeBasis,
            group: expected,
            actual: actual.timeBasis,
            issues: &issues
        )
        compareProvenanceSpikeTrains(
            expected: expected.spikeTrains,
            group: expected,
            actual: actual.spikeTrains,
            issues: &issues
        )
        compareProvenanceEventDefinitions(
            expected: expected.eventDefinitions,
            group: expected,
            actual: actual.eventDefinitions,
            issues: &issues
        )
    }

    private static func compareProvenanceTimeBasis(
        expected: PreparedValidationExpectedProvenanceTimeBasis,
        group: PreparedValidationExpectedGroupProvenance,
        actual: PreparedEventScopeTimeBasisProvenance,
        issues: inout PreparedValidationIssueAccumulator
    ) -> Bool {
        switch (expected, actual) {
        case (.recordingElapsed, .recordingElapsed):
            return true
        case (
            .eventRelative(let expectedCell, let expectedTick),
            .eventRelative(let actualCell, let actualTick)
        ):
            let location = comparisonLocation(
                from: comparisonGroupLocation(group),
                sourceCell: expectedCell
            )
            guard expectedCell == actualCell else {
                appendComparisonMismatch(
                    .provenanceGroupEventOriginCell,
                    location: location,
                    issues: &issues
                )
                return false
            }
            guard expectedTick == actualTick else {
                appendComparisonMismatch(
                    .provenanceGroupSourceOriginTick,
                    location: location,
                    issues: &issues
                )
                return false
            }
            return true
        default:
            appendComparisonMismatch(
                .provenanceGroupTimeBasis,
                location: comparisonGroupLocation(group),
                issues: &issues
            )
            return false
        }
    }

    private static func compareProvenanceSpikeTrains(
        expected: [PreparedValidationExpectedSpikeProvenance],
        group: PreparedValidationExpectedGroupProvenance,
        actual: [PreparedSpikeTrainProvenance],
        issues: inout PreparedValidationIssueAccumulator
    ) {
        guard expected.count == actual.count else {
            appendComparisonCountMismatch(
                .provenanceSpikeTrainCount,
                expected: expected.count,
                actual: actual.count,
                location: comparisonGroupLocation(group),
                issues: &issues
            )
            return
        }

        for offset in expected.indices {
            let expectedTrain = expected[offset]
            let actualTrain = actual[offset]
            let location = PreparedScientificImportValidationLocation(
                groupIndex: group.originalPlanIndex,
                groupID: group.semanticID,
                spikeTrainID: expectedTrain.semanticID,
                sourceColumn: expectedTrain.sourceColumn
            )
            guard expectedTrain.semanticID == actualTrain.semanticID else {
                appendComparisonMismatch(
                    .provenanceSpikeTrainSemanticID(position: offset + 1),
                    location: location,
                    issues: &issues
                )
                continue
            }
            if expectedTrain.sourceColumn != actualTrain.sourceColumn {
                appendComparisonMismatch(
                    .provenanceSpikeSourceColumn,
                    location: location,
                    issues: &issues
                )
            }
            if expectedTrain.orderDecision != actualTrain.orderDecision {
                appendComparisonMismatch(
                    .provenanceSpikeOrderDecision,
                    location: location,
                    issues: &issues
                )
            }
            if expectedTrain.duplicateDecision != actualTrain.duplicateDecision {
                appendComparisonMismatch(
                    .provenanceSpikeDuplicateDecision,
                    location: location,
                    issues: &issues
                )
            }

            if expectedTrain.sourceOrderDescentCount
                != actualTrain.sourceOrderDescentCount {
                appendComparisonMismatch(
                    .provenanceSpikeSourceOrderDescentCount,
                    location: location,
                    issues: &issues
                )
            }
            guard expectedTrain.timestampSources.count
                == actualTrain.timestampSources.count else {
                appendComparisonCountMismatch(
                    .provenanceSpikeTimestampSourceCount,
                    expected: expectedTrain.timestampSources.count,
                    actual: actualTrain.timestampSources.count,
                    location: location,
                    issues: &issues
                )
                continue
            }
            for outputOffset in expectedTrain.timestampSources.indices {
                let expectedCells = expectedTrain.timestampSources[outputOffset]
                let actualCells = actualTrain.timestampSources[outputOffset]
                let outputLocation = comparisonLocation(
                    from: location,
                    sourceCell: expectedCells.first
                )
                guard expectedCells.count == actualCells.count else {
                    appendComparisonCountMismatch(
                        .provenanceSpikeTimestampSourceMultiplicity(
                            outputPosition: outputOffset + 1
                        ),
                        expected: expectedCells.count,
                        actual: actualCells.count,
                        location: outputLocation,
                        issues: &issues
                    )
                    continue
                }
                for sourceOffset in expectedCells.indices
                    where expectedCells[sourceOffset] != actualCells[sourceOffset] {
                    appendComparisonMismatch(
                        .provenanceSpikeTimestampSourceCell(
                            outputPosition: outputOffset + 1,
                            sourcePosition: sourceOffset + 1
                        ),
                        location: comparisonLocation(
                            from: location,
                            sourceCell: expectedCells[sourceOffset]
                        ),
                        issues: &issues
                    )
                }
            }
        }
    }

    private static func compareProvenanceEventDefinitions(
        expected: [PreparedValidationExpectedEventProvenance],
        group: PreparedValidationExpectedGroupProvenance,
        actual: [PreparedEventDefinitionProvenance],
        issues: inout PreparedValidationIssueAccumulator
    ) {
        guard expected.count == actual.count else {
            appendComparisonCountMismatch(
                .provenanceEventDefinitionCount,
                expected: expected.count,
                actual: actual.count,
                location: comparisonGroupLocation(group),
                issues: &issues
            )
            return
        }

        for offset in expected.indices {
            let expectedDefinition = expected[offset]
            let actualDefinition = actual[offset]
            let location = PreparedScientificImportValidationLocation(
                groupIndex: group.originalPlanIndex,
                groupID: group.semanticID,
                eventDefinitionID: expectedDefinition.semanticID,
                sourceColumn: expectedDefinition.sourceColumn
            )
            guard expectedDefinition.semanticID == actualDefinition.semanticID else {
                appendComparisonMismatch(
                    .provenanceEventDefinitionSemanticID(position: offset + 1),
                    location: location,
                    issues: &issues
                )
                continue
            }
            if expectedDefinition.sourceColumn != actualDefinition.sourceColumn {
                appendComparisonMismatch(
                    .provenanceEventSourceColumn,
                    location: location,
                    issues: &issues
                )
            }
            if expectedDefinition.orderDecision != actualDefinition.orderDecision {
                appendComparisonMismatch(
                    .provenanceEventOrderDecision,
                    location: location,
                    issues: &issues
                )
            }
            if expectedDefinition.sourceOrderDescentCount
                != actualDefinition.sourceOrderDescentCount {
                appendComparisonMismatch(
                    .provenanceEventSourceOrderDescentCount,
                    location: location,
                    issues: &issues
                )
            }
            compareProvenanceEventOccurrences(
                expected: expectedDefinition.occurrences,
                baseLocation: location,
                actual: actualDefinition.occurrences,
                issues: &issues
            )
        }
    }

    private static func compareProvenanceEventOccurrences(
        expected: [PreparedValidationExpectedEventOccurrenceProvenance],
        baseLocation: PreparedScientificImportValidationLocation,
        actual: [PreparedEventOccurrenceProvenance],
        issues: inout PreparedValidationIssueAccumulator
    ) {
        guard expected.count == actual.count else {
            appendComparisonCountMismatch(
                .provenanceEventOccurrenceCount,
                expected: expected.count,
                actual: actual.count,
                location: baseLocation,
                issues: &issues
            )
            return
        }

        for offset in expected.indices {
            let occurrenceLocation = comparisonLocation(
                from: baseLocation,
                sourceCell: expected[offset].timestampCell
            )
            guard expected[offset].timestampCell == actual[offset].timestampCell else {
                appendComparisonMismatch(
                    .provenanceEventOccurrenceTimestampCell(position: offset + 1),
                    location: occurrenceLocation,
                    issues: &issues
                )
                continue
            }
            compareProvenanceAttributes(
                expected: expected[offset].attributes,
                occurrence: offset + 1,
                baseLocation: occurrenceLocation,
                actual: actual[offset].attributes,
                issues: &issues
            )
        }
    }

    private static func compareProvenanceAttributes(
        expected: [PreparedValidationExpectedAttributeProvenance],
        occurrence: Int,
        baseLocation: PreparedScientificImportValidationLocation,
        actual: [PreparedEventAttributeProvenance],
        issues: inout PreparedValidationIssueAccumulator
    ) {
        guard expected.count == actual.count else {
            appendComparisonCountMismatch(
                .provenanceEventAttributeCount(occurrence: occurrence),
                expected: expected.count,
                actual: actual.count,
                location: baseLocation,
                issues: &issues
            )
            return
        }

        for offset in expected.indices {
            let expectedAttribute = expected[offset]
            let actualAttribute = actual[offset]
            let location = comparisonLocation(
                from: baseLocation,
                attributeKey: expectedAttribute.key,
                sourceCell: expectedAttribute.valueCell
            )
            guard expectedAttribute.key == actualAttribute.key else {
                appendComparisonMismatch(
                    .provenanceEventAttributeKey(
                        occurrence: occurrence,
                        position: offset + 1
                    ),
                    location: location,
                    issues: &issues
                )
                continue
            }
            if expectedAttribute.valueCell != actualAttribute.valueCell {
                appendComparisonMismatch(
                    .provenanceEventAttributeValueCell(
                        occurrence: occurrence,
                        position: offset + 1
                    ),
                    location: location,
                    issues: &issues
                )
            }
            compareInlineUnitTrace(
                expected: expectedAttribute.inlineUnit,
                occurrence: occurrence,
                position: offset + 1,
                baseLocation: location,
                actual: actualAttribute.inlineUnitSuggestion,
                issues: &issues
            )
        }
    }

    private static func compareInlineUnitTrace(
        expected: PreparedValidationExpectedInlineUnit,
        occurrence: Int,
        position: Int,
        baseLocation: PreparedScientificImportValidationLocation,
        actual: PreparedInlineUnitSuggestionProvenance,
        issues: inout PreparedValidationIssueAccumulator
    ) {
        switch (expected, actual) {
        case (.absent, .absent):
            return
        case (
            .present(let expectedSymbol, let expectedCell),
            .present(let actualSymbol, let actualCell)
        ):
            let location = comparisonLocation(from: baseLocation, sourceCell: expectedCell)
            if expectedSymbol != actualSymbol {
                appendComparisonMismatch(
                    .provenanceEventAttributeInlineUnitSymbol(
                        occurrence: occurrence,
                        position: position
                    ),
                    location: location,
                    issues: &issues
                )
            }
            if expectedCell != actualCell {
                appendComparisonMismatch(
                    .provenanceEventAttributeInlineUnitSourceCell(
                        occurrence: occurrence,
                        position: position
                    ),
                    location: location,
                    issues: &issues
                )
            }
        default:
            appendComparisonMismatch(
                .provenanceEventAttributeInlineUnitCase(
                    occurrence: occurrence,
                    position: position
                ),
                location: baseLocation,
                issues: &issues
            )
        }
    }

    private static func appendComparisonCountMismatch(
        _ field: PreparedScientificImportComparisonField,
        expected: Int,
        actual: Int,
        location: PreparedScientificImportValidationLocation,
        issues: inout PreparedValidationIssueAccumulator
    ) {
        issues.append(
            PreparedScientificImportValidationIssue(
                kind: .preparedComparisonCountMismatch(
                    field: field,
                    expected: expected,
                    actual: actual
                ),
                location: location
            )
        )
    }

    private static func appendComparisonMismatch(
        _ field: PreparedScientificImportComparisonField,
        location: PreparedScientificImportValidationLocation,
        issues: inout PreparedValidationIssueAccumulator
    ) {
        issues.append(
            PreparedScientificImportValidationIssue(
                kind: .preparedComparisonValueMismatch(field: field),
                location: location
            )
        )
    }

    private static func comparisonGroupLocation(
        _ group: PreparedValidationExpectedGroupData
    ) -> PreparedScientificImportValidationLocation {
        PreparedScientificImportValidationLocation(
            groupIndex: group.originalPlanIndex,
            groupID: group.semanticID
        )
    }

    private static func comparisonGroupLocation(
        _ group: PreparedValidationExpectedGroupProvenance
    ) -> PreparedScientificImportValidationLocation {
        PreparedScientificImportValidationLocation(
            groupIndex: group.originalPlanIndex,
            groupID: group.semanticID
        )
    }

    private static func comparisonLocation(
        from base: PreparedScientificImportValidationLocation,
        attributeKey: EventAttributeKey? = nil,
        sourceCell: StagedSourceCellReference? = nil
    ) -> PreparedScientificImportValidationLocation {
        let resolvedCell = sourceCell ?? base.sourceCell
        return PreparedScientificImportValidationLocation(
            groupIndex: base.groupIndex,
            groupID: base.groupID,
            spikeTrainID: base.spikeTrainID,
            eventDefinitionID: base.eventDefinitionID,
            attributeKey: attributeKey ?? base.attributeKey,
            sourceColumn: resolvedCell?.column ?? base.sourceColumn,
            sourceCell: resolvedCell
        )
    }

    private static func emitEmptyEventDefinitionWarnings(
        expected: PreparedValidationExpectedImport,
        warnings: inout PreparedValidationWarningAccumulator
    ) {
        guard expected.data.groups.count == expected.provenance.groups.count else { return }
        for groupOffset in expected.data.groups.indices {
            let dataGroup = expected.data.groups[groupOffset]
            let provenanceGroup = expected.provenance.groups[groupOffset]
            guard dataGroup.semanticID == provenanceGroup.semanticID,
                  dataGroup.eventDefinitions.count
                    == provenanceGroup.eventDefinitions.count else {
                continue
            }
            for definitionOffset in dataGroup.eventDefinitions.indices {
                let definition = dataGroup.eventDefinitions[definitionOffset]
                let trace = provenanceGroup.eventDefinitions[definitionOffset]
                guard definition.semanticID == trace.semanticID,
                      definition.occurrences.isEmpty else {
                    continue
                }
                warnings.append(
                    PreparedScientificImportValidationWarning(
                        kind: .emptyEventDefinition,
                        location: PreparedScientificImportValidationLocation(
                            groupIndex: dataGroup.originalPlanIndex,
                            groupID: dataGroup.semanticID,
                            eventDefinitionID: definition.semanticID,
                            sourceColumn: trace.sourceColumn
                        )
                    )
                )
            }
        }
    }

    private static func replayScientificAttributes(
        _ attributes: [PreparedValidationTypedAttribute]
    ) -> [PreparedValidationExpectedEventAttribute] {
        replayExpectedAttributes(attributes, role: .scientific)
    }

    private static func replayExpectedAttributes(
        _ attributes: [PreparedValidationTypedAttribute],
        role: EventAttributeRole
    ) -> [PreparedValidationExpectedEventAttribute] {
        attributes.compactMap { attribute in
            guard attribute.definition.role == role else { return nil }
            return PreparedValidationExpectedEventAttribute(
                key: attribute.definition.key,
                value: attribute.value
            )
        }
    }

    private static func replayTimestampDescent(
        _ timestamps: [PreparedValidationCoordinateTimestamp]
    ) -> PreparedValidationDescent? {
        guard timestamps.count > 1 else { return nil }
        var count = 0
        var firstPreviousCell: StagedSourceCellReference?
        var firstCurrentCell: StagedSourceCellReference?
        for index in 1..<timestamps.count
            where timestamps[index].tick < timestamps[index - 1].tick {
            count += 1
            if firstPreviousCell == nil {
                firstPreviousCell = timestamps[index - 1].sourceCell
                firstCurrentCell = timestamps[index].sourceCell
            }
        }
        guard count > 0, let firstPreviousCell, let firstCurrentCell else { return nil }
        return PreparedValidationDescent(
            count: count,
            firstPreviousCell: firstPreviousCell,
            firstCurrentCell: firstCurrentCell
        )
    }

    private static func replayEventDescent(
        _ occurrences: [PreparedValidationCoordinateEventOccurrence]
    ) -> PreparedValidationDescent? {
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
        return PreparedValidationDescent(
            count: count,
            firstPreviousCell: firstPreviousCell,
            firstCurrentCell: firstCurrentCell
        )
    }

    private static func replayTimestampIsOrderedBefore(
        _ lhs: PreparedValidationCoordinateTimestamp,
        _ rhs: PreparedValidationCoordinateTimestamp
    ) -> Bool {
        if lhs.tick != rhs.tick { return lhs.tick < rhs.tick }
        return lhs.sourceCell.oneBasedDataRowIndex < rhs.sourceCell.oneBasedDataRowIndex
    }

    private static func replayEventIsOrderedBefore(
        _ lhs: PreparedValidationCoordinateEventOccurrence,
        _ rhs: PreparedValidationCoordinateEventOccurrence
    ) -> Bool {
        if lhs.tick != rhs.tick { return lhs.tick < rhs.tick }
        return lhs.timestampCell.oneBasedDataRowIndex < rhs.timestampCell.oneBasedDataRowIndex
    }

    private static func replayCollapseExactTimestamps(
        _ timestamps: [PreparedValidationCoordinateTimestamp]
    ) -> [PreparedValidationCollapsedTimestamp] {
        var collapsed: [PreparedValidationCollapsedTimestamp] = []
        collapsed.reserveCapacity(timestamps.count)
        for timestamp in timestamps {
            if let lastIndex = collapsed.indices.last,
               collapsed[lastIndex].tick == timestamp.tick {
                collapsed[lastIndex].sourceCells.append(timestamp.sourceCell)
            } else {
                collapsed.append(
                    PreparedValidationCollapsedTimestamp(
                        tick: timestamp.tick,
                        sourceCells: [timestamp.sourceCell]
                    )
                )
            }
        }
        return collapsed
    }

    private static func replayTextIsOrderedBefore(_ lhs: String, _ rhs: String) -> Bool {
        lhs.utf8.lexicographicallyPrecedes(rhs.utf8)
    }

    private static func replayAttributeValue(
        _ rawValue: String,
        definition: ResolvedEventAttributeDefinitionPlan,
        location: PreparedScientificImportValidationLocation,
        issues: inout PreparedValidationIssueAccumulator
    ) -> EventAttributeValue? {
        do {
            switch definition.scalarType {
            case .string:
                if rawValue.isEmpty, definition.emptyStringPolicy == .forbid {
                    issues.append(
                        PreparedScientificImportValidationIssue(
                            kind: .eventAttributeValueInvalid(
                                expectedType: .string,
                                issue: .explicitEmptyStringForbidden
                            ),
                            location: location
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
                return .boolean(try replayBoolean(rawValue))
            }
        } catch let error as CanonicalStringValueError {
            let issue: PreparedScientificImportAttributeValueIssue
            switch error {
            case .sourceTooLong(let maximumUTF8Bytes):
                issue = .stringSourceTooLong(maximumUTF8Bytes: maximumUTF8Bytes)
            case .canonicalValueTooLong(let maximumUTF8Bytes):
                issue = .stringCanonicalValueTooLong(maximumUTF8Bytes: maximumUTF8Bytes)
            }
            issues.append(
                PreparedScientificImportValidationIssue(
                    kind: .eventAttributeValueInvalid(
                        expectedType: definition.scalarType,
                        issue: issue
                    ),
                    location: location
                )
            )
        } catch let error as ExactEventAttributeParseError {
            issues.append(
                PreparedScientificImportValidationIssue(
                    kind: .eventAttributeValueInvalid(
                        expectedType: definition.scalarType,
                        issue: .exactNumber(replayExactAttributeIssue(error))
                    ),
                    location: location
                )
            )
        } catch let error as PreparedValidationBooleanError {
            let issue: PreparedScientificImportAttributeValueIssue
            switch error {
            case .empty:
                issue = .booleanEmpty
            case .tooLong(let maximumUTF8Bytes):
                issue = .booleanLexemeTooLong(maximumUTF8Bytes: maximumUTF8Bytes)
            case .invalid:
                issue = .invalidBoolean
            }
            issues.append(
                PreparedScientificImportValidationIssue(
                    kind: .eventAttributeValueInvalid(
                        expectedType: definition.scalarType,
                        issue: issue
                    ),
                    location: location
                )
            )
        } catch {
            issues.append(
                PreparedScientificImportValidationIssue(
                    kind: .independentReplayInvariant,
                    location: location
                )
            )
        }
        return nil
    }

    private static func replayBoolean(_ source: String) throws -> Bool {
        let maximumUTF8Bytes = ExactIntegerValue.maximumLexemeUTF8ByteCount
        guard source.utf8.prefix(maximumUTF8Bytes + 1).count <= maximumUTF8Bytes else {
            throw PreparedValidationBooleanError.tooLong(
                maximumUTF8Bytes: maximumUTF8Bytes
            )
        }
        let bytes = Array(source.utf8)
        var start = 0
        var end = bytes.count
        while start < end, bytes[start] == 0x20 || bytes[start] == 0x09 { start += 1 }
        while end > start, bytes[end - 1] == 0x20 || bytes[end - 1] == 0x09 { end -= 1 }
        guard start < end else { throw PreparedValidationBooleanError.empty }
        let value = bytes[start..<end]
        if value.elementsEqual([0x74, 0x72, 0x75, 0x65]) { return true }
        if value.elementsEqual([0x66, 0x61, 0x6C, 0x73, 0x65]) { return false }
        throw PreparedValidationBooleanError.invalid
    }

    private static func replayTimestampIssue(
        _ error: ExactTimestampParseError
    ) -> PreparedScientificImportTimestampParseIssue {
        switch error {
        case .empty:
            return .empty
        case .lexemeTooLong(let maximumUTF8Bytes):
            return .lexemeTooLong(maximumUTF8Bytes: maximumUTF8Bytes)
        case .invalidSyntax(let utf8Offset, let issue):
            let mapped: PreparedScientificImportTimestampSyntaxIssue
            switch issue {
            case .missingMantissaDigits:
                mapped = .missingMantissaDigits
            case .missingExponentDigits:
                mapped = .missingExponentDigits
            case .unexpectedCharacter:
                mapped = .unexpectedCharacter
            }
            return .invalidSyntax(utf8Offset: utf8Offset, issue: mapped)
        case .notExactlyRepresentableInMicroseconds:
            return .notExactlyRepresentableInMicroseconds
        case .outsideSignedMicrosecondRange:
            return .outsideSignedMicrosecondRange
        }
    }

    private static func replayEventAttributeKeyIssue(
        _ error: EventAttributeKeyError
    ) -> PreparedScientificImportEventAttributeKeyIssue {
        switch error {
        case .sourceTooLong(let maximumUTF8Bytes):
            return .sourceTooLong(maximumUTF8Bytes: maximumUTF8Bytes)
        case .canonicalValueTooLong(let maximumUTF8Bytes):
            return .canonicalValueTooLong(maximumUTF8Bytes: maximumUTF8Bytes)
        case .empty:
            return .empty
        case .blank:
            return .blank
        case .containsControlCharacter:
            return .containsControlCharacter
        case .containsMetadataSeparator:
            return .containsMetadataSeparator
        case .reservedUnitSuffix:
            return .reservedUnitSuffix
        }
    }

    private static func replayExactAttributeIssue(
        _ error: ExactEventAttributeParseError
    ) -> PreparedScientificImportExactAttributeParseIssue {
        switch error {
        case .empty:
            return .empty
        case .lexemeTooLong(let maximumUTF8Bytes):
            return .lexemeTooLong(maximumUTF8Bytes: maximumUTF8Bytes)
        case .invalidSyntax(let utf8Offset, let issue):
            let mapped: PreparedScientificImportExactAttributeSyntaxIssue
            switch issue {
            case .missingMantissaDigits:
                mapped = .missingMantissaDigits
            case .missingExponentDigits:
                mapped = .missingExponentDigits
            case .unexpectedCharacter:
                mapped = .unexpectedCharacter
            }
            return .invalidSyntax(utf8Offset: utf8Offset, issue: mapped)
        case .exponentOutOfRange:
            return .exponentOutOfRange
        case .normalizedExponentOutOfRange:
            return .normalizedExponentOutOfRange
        }
    }

    private static func replayUnitSymbolIssue(
        _ error: OpaqueUnitSymbolError
    ) -> PreparedScientificImportUnitSymbolIssue {
        switch error {
        case .sourceTooLong(let maximumUTF8Bytes):
            return .sourceTooLong(maximumUTF8Bytes: maximumUTF8Bytes)
        case .canonicalValueTooLong(let maximumUTF8Bytes):
            return .canonicalValueTooLong(maximumUTF8Bytes: maximumUTF8Bytes)
        case .empty:
            return .empty
        case .blank:
            return .blank
        case .containsControlCharacter:
            return .containsControlCharacter
        }
    }

    private static func replaySourceColumn(
        _ reference: StagedSourceColumnReference,
        from source: ResolvedScientificImportSource
    ) -> StagedScientificColumn? {
        let offset = reference.oneBasedIndex - 1
        guard source.columns.indices.contains(offset) else { return nil }
        let column = source.columns[offset]
        guard column.sourceColumn == reference else { return nil }
        return column
    }

    private static func replaySourceCell(
        column: StagedSourceColumnReference,
        row: Int
    ) -> StagedSourceCellReference? {
        try? StagedSourceCellReference(
            column: column,
            oneBasedDataRowIndex: row
        )
    }

    private static func replayStartsWithMetadataMarker(_ text: String) -> Bool {
        text.utf8.first == 0x40
    }

    private static func replayKeyIsOrderedBefore(
        _ lhs: EventAttributeKey,
        _ rhs: EventAttributeKey
    ) -> Bool {
        lhs.canonicalText.utf8.lexicographicallyPrecedes(rhs.canonicalText.utf8)
    }

    private static func sourceContains(
        _ cell: StagedSourceCellReference,
        in source: ResolvedScientificImportSource
    ) -> Bool {
        let columnOffset = cell.column.oneBasedIndex - 1
        let rowOffset = cell.oneBasedDataRowIndex - 1
        guard source.columns.indices.contains(columnOffset),
              source.dataRowCount > rowOffset,
              source.columns[columnOffset].sourceColumn == cell.column,
              source.columns[columnOffset].cells.indices.contains(rowOffset) else {
            return false
        }
        return true
    }
}

private enum PreparedValidationRawMetadata {
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

private struct PreparedValidationRawTimestamp {
    let tick: MicrosecondTick
    let sourceCell: StagedSourceCellReference
}

private struct PreparedValidationRawSpikeColumn {
    let plan: ResolvedSpikeTrainColumnPlan
    let timestamps: [PreparedValidationRawTimestamp]
}

private struct PreparedValidationRawEventOccurrence {
    let sourceTick: MicrosecondTick
    let timestampCell: StagedSourceCellReference
    var metadata: [PreparedValidationRawMetadata]
}

private struct PreparedValidationRawEventColumn {
    let plan: ResolvedEventDefinitionColumnPlan
    let occurrences: [PreparedValidationRawEventOccurrence]
}

private struct PreparedValidationRawGroup {
    let groupIndex: Int
    let plan: ResolvedEventScopeGroupPlan
    let spikeColumns: [PreparedValidationRawSpikeColumn]
    let eventColumns: [PreparedValidationRawEventColumn]
}

private struct PreparedValidationRawValueLine {
    let rawValue: String
    let sourceCell: StagedSourceCellReference
}

private struct PreparedValidationParsedUnit {
    let symbol: OpaqueUnitSymbol
    let sourceCell: StagedSourceCellReference
}

private struct PreparedValidationUnitSighting {
    let symbol: OpaqueUnitSymbol
    let sourceCell: StagedSourceCellReference
}

private enum PreparedValidationExpectedInlineUnit: Hashable {
    case absent
    case present(symbol: OpaqueUnitSymbol, sourceCell: StagedSourceCellReference)
}

private struct PreparedValidationTypedAttribute {
    let definition: ResolvedEventAttributeDefinitionPlan
    let value: EventAttributeValue
    let valueCell: StagedSourceCellReference
    let inlineUnit: PreparedValidationExpectedInlineUnit
}

private struct PreparedValidationTypedEventOccurrence {
    let sourceTick: MicrosecondTick
    let timestampCell: StagedSourceCellReference
    /// Deterministic UTF-8 key order, independent of metadata-row order.
    let attributes: [PreparedValidationTypedAttribute]
}

private struct PreparedValidationTypedEventColumn {
    let plan: ResolvedEventDefinitionColumnPlan
    let occurrences: [PreparedValidationTypedEventOccurrence]
}

private struct PreparedValidationTypedGroup {
    let groupIndex: Int
    let plan: ResolvedEventScopeGroupPlan
    let spikeColumns: [PreparedValidationRawSpikeColumn]
    let eventColumns: [PreparedValidationTypedEventColumn]
}

private struct PreparedValidationResolvedOrigin {
    let eventDefinitionID: ScientificEventDefinitionID
    let sourceTick: MicrosecondTick
    let sourceCell: StagedSourceCellReference
    let scientificAttributes: [PreparedValidationExpectedEventAttribute]
}

private enum PreparedValidationResolvedTimeBasis {
    case recordingElapsed
    case eventRelative(PreparedValidationResolvedOrigin)
    case unresolved
}

private struct PreparedValidationCoordinateTimestamp {
    let tick: MicrosecondTick
    let sourceCell: StagedSourceCellReference
}

private struct PreparedValidationCoordinateSpikeColumn {
    let plan: ResolvedSpikeTrainColumnPlan
    let timestamps: [PreparedValidationCoordinateTimestamp]
}

private struct PreparedValidationCoordinateEventOccurrence {
    let tick: MicrosecondTick
    let timestampCell: StagedSourceCellReference
    let attributes: [PreparedValidationTypedAttribute]
}

private struct PreparedValidationCoordinateEventColumn {
    let plan: ResolvedEventDefinitionColumnPlan
    let occurrences: [PreparedValidationCoordinateEventOccurrence]
}

private struct PreparedValidationCoordinateGroup {
    let groupIndex: Int
    let plan: ResolvedEventScopeGroupPlan
    let timeBasis: PreparedValidationResolvedTimeBasis
    let spikeColumns: [PreparedValidationCoordinateSpikeColumn]
    let eventColumns: [PreparedValidationCoordinateEventColumn]
}

private struct PreparedValidationDescent {
    let count: Int
    let firstPreviousCell: StagedSourceCellReference
    let firstCurrentCell: StagedSourceCellReference
}

private struct PreparedValidationCollapsedTimestamp {
    let tick: MicrosecondTick
    var sourceCells: [StagedSourceCellReference]
}

private struct PreparedValidationExpectedEventAttribute: Hashable {
    let key: EventAttributeKey
    let value: EventAttributeValue
}

private enum PreparedValidationExpectedDataTimeBasis {
    case recordingElapsed
    case eventRelative(origin: PreparedValidationExpectedEventOrigin)
}

private struct PreparedValidationExpectedEventOrigin {
    let eventDefinitionID: ScientificEventDefinitionID
    let tick: MicrosecondTick
    let scientificAttributes: [PreparedValidationExpectedEventAttribute]
}

private struct PreparedValidationExpectedSpikeTrain {
    let semanticID: ScientificSpikeTrainID
    let timestamps: [MicrosecondTick]
}

private struct PreparedValidationExpectedEventOccurrence {
    let tick: MicrosecondTick
    let scientificAttributes: [PreparedValidationExpectedEventAttribute]
    let presentationAttributes: [PreparedValidationExpectedEventAttribute]
}

private struct PreparedValidationExpectedEventDefinition {
    let semanticID: ScientificEventDefinitionID
    let eventTypeID: ScientificEventTypeID
    let occurrences: [PreparedValidationExpectedEventOccurrence]
}

private struct PreparedValidationExpectedGroupData {
    let originalPlanIndex: Int
    let semanticID: ScientificEventScopeGroupID
    let timeBasis: PreparedValidationExpectedDataTimeBasis
    let spikeTrains: [PreparedValidationExpectedSpikeTrain]
    let eventDefinitions: [PreparedValidationExpectedEventDefinition]
}

private struct PreparedValidationExpectedImportData {
    let activityMode: ScientificDatasetActivityMode
    let groups: [PreparedValidationExpectedGroupData]
    let scientificAttributeDefinitions: [ResolvedEventAttributeDefinitionPlan]
    let presentationAttributeDefinitions: [ResolvedEventAttributeDefinitionPlan]
}

private enum PreparedValidationExpectedProvenanceTimeBasis {
    case recordingElapsed
    case eventRelative(
        originCell: StagedSourceCellReference,
        sourceOriginTick: MicrosecondTick
    )
}

private struct PreparedValidationExpectedSpikeProvenance {
    let semanticID: ScientificSpikeTrainID
    let sourceColumn: StagedSourceColumnReference
    let orderDecision: TimestampOrderDecision
    let duplicateDecision: ExactDuplicateDecision
    let sourceOrderDescentCount: Int
    let timestampSources: [[StagedSourceCellReference]]
}

private struct PreparedValidationExpectedAttributeProvenance {
    let key: EventAttributeKey
    let valueCell: StagedSourceCellReference
    let inlineUnit: PreparedValidationExpectedInlineUnit
}

private struct PreparedValidationExpectedEventOccurrenceProvenance {
    let timestampCell: StagedSourceCellReference
    let attributes: [PreparedValidationExpectedAttributeProvenance]
}

private struct PreparedValidationExpectedEventProvenance {
    let semanticID: ScientificEventDefinitionID
    let sourceColumn: StagedSourceColumnReference
    let orderDecision: TimestampOrderDecision
    let sourceOrderDescentCount: Int
    let occurrences: [PreparedValidationExpectedEventOccurrenceProvenance]
}

private struct PreparedValidationExpectedGroupProvenance {
    let originalPlanIndex: Int
    let semanticID: ScientificEventScopeGroupID
    let timeBasis: PreparedValidationExpectedProvenanceTimeBasis
    let spikeTrains: [PreparedValidationExpectedSpikeProvenance]
    let eventDefinitions: [PreparedValidationExpectedEventProvenance]
}

private struct PreparedValidationExpectedImportProvenance {
    let groups: [PreparedValidationExpectedGroupProvenance]
}

private struct PreparedValidationExpectedImport {
    let data: PreparedValidationExpectedImportData
    let provenance: PreparedValidationExpectedImportProvenance
}

private struct PreparedValidationExpectedSpikePair {
    let data: PreparedValidationExpectedSpikeTrain
    let provenance: PreparedValidationExpectedSpikeProvenance
}

private struct PreparedValidationExpectedEventPair {
    let data: PreparedValidationExpectedEventDefinition
    let provenance: PreparedValidationExpectedEventProvenance
}

private struct PreparedValidationExpectedGroupPair {
    let data: PreparedValidationExpectedGroupData
    let provenance: PreparedValidationExpectedGroupProvenance
}

private enum PreparedValidationBooleanError: Error {
    case empty
    case tooLong(maximumUTF8Bytes: Int)
    case invalid
}

private struct PreparedPlanPartitionEntry {
    let sourceColumn: StagedSourceColumnReference
    let location: PreparedScientificImportValidationLocation
}

private struct PreparedValidationIssueAccumulator {
    private(set) var retained: [PreparedScientificImportValidationIssue] = []
    private(set) var totalCount = 0

    var additionalCount: Int {
        totalCount - retained.count
    }

    mutating func append(_ issue: PreparedScientificImportValidationIssue) {
        totalCount += 1
        if retained.count < PreparedScientificImportValidator.maximumReportedIssueCount {
            retained.append(issue)
        }
    }
}

private struct PreparedValidationWarningAccumulator {
    private(set) var retained: [PreparedScientificImportValidationWarning] = []
    private(set) var totalCount = 0

    var additionalCount: Int {
        totalCount - retained.count
    }

    mutating func append(_ warning: PreparedScientificImportValidationWarning) {
        totalCount += 1
        if retained.count < PreparedScientificImportValidator.maximumReportedWarningCount {
            retained.append(warning)
        }
    }
}
