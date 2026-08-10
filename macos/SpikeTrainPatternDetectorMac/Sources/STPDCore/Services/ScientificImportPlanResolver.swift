public enum ScientificImportPlanIssue: Hashable, Sendable {
    case sourceBindingMismatch
    case missingSourceTimeUnit
    case missingActivityMode
    case missingEventScopeGroups
    case emptyEventScopeGroups

    case missingGroupSemanticID(group: Int)
    case duplicateGroupSemanticID(
        semanticID: ScientificEventScopeGroupID,
        firstGroup: Int,
        duplicateGroup: Int
    )
    case groupHasNoSpikeTrains(group: Int)
    case missingGroupTimeBasis(group: Int)
    case missingEventRelativeOrigin(group: Int)
    case eventRelativeOriginOutsideStaging(group: Int, cell: StagedSourceCellReference)
    case eventRelativeOriginNotInSameGroupEventDefinition(group: Int, column: Int)

    case sourceColumnOutsideStaging(group: Int, column: Int)
    case missingSpikeTrainSemanticID(group: Int, column: Int)
    case missingSpikeTrainOrderDecision(group: Int, column: Int)
    case missingSpikeTrainDuplicateDecision(group: Int, column: Int)
    case duplicateSpikeTrainSemanticID(
        group: Int,
        semanticID: ScientificSpikeTrainID,
        firstColumn: Int,
        duplicateColumn: Int
    )
    /// A `ScientificSpikeTrainID` is unique across the whole dataset, not merely within one group.
    /// The same identity appeared in two different EventScopeGroups.
    case duplicateSpikeTrainSemanticIDAcrossGroups(
        semanticID: ScientificSpikeTrainID,
        firstGroup: Int,
        firstColumn: Int,
        duplicateGroup: Int,
        duplicateColumn: Int
    )
    case duplicateCollapseNotAllowed(
        activityMode: ScientificDatasetActivityMode,
        group: Int,
        column: Int
    )
    case missingEventDefinitionSemanticID(group: Int, column: Int)
    case missingEventTypeID(group: Int, column: Int)
    case missingEventOrderDecision(group: Int, column: Int)
    case duplicateEventDefinitionSemanticID(
        group: Int,
        semanticID: ScientificEventDefinitionID,
        firstColumn: Int,
        duplicateColumn: Int
    )

    case headerlessSourceRequiresSingleGroup(actualGroupCount: Int)
    case headerlessSourceContainsEventDefinition(group: Int, column: Int)
    case headerlessSourceRequiresRecordingElapsed(group: Int)

    case flattenedSourcePartitionCountMismatch(expected: Int, actual: Int)
    case flattenedSourcePartitionOrderMismatch(position: Int, expected: Int, actual: Int)
    case sourceColumnNotAssigned(column: Int)
    case sourceColumnAssignedMultipleTimes(column: Int, count: Int)

    case duplicateEventAttributeKey(
        key: EventAttributeKey,
        firstDefinition: Int,
        duplicateDefinition: Int
    )
    case missingEventAttributeScalarType(definition: Int, key: EventAttributeKey)
    case missingEventAttributeRole(definition: Int, key: EventAttributeKey)
    case missingEventAttributeUnit(definition: Int, key: EventAttributeKey)
    case missingEventAttributeEmptyStringPolicy(definition: Int, key: EventAttributeKey)
    case explicitEmptyStringAllowedForNonString(
        definition: Int,
        key: EventAttributeKey,
        scalarType: EventAttributeScalarType
    )

    /// Indicates an implementation inconsistency, never a user decision or source-data finding.
    case internalResolutionInvariant
}

public struct ScientificImportPlanResolutionError: Error, Equatable, Sendable {
    public let issues: [ScientificImportPlanIssue]

    internal init(issues: [ScientificImportPlanIssue]) {
        self.issues = issues
    }
}

/// Resolves only structural bindings and explicit choices. It does not inspect timestamp or event-
/// attribute grammar and cannot produce a canonical, confirmed, or identity-bearing dataset.
public enum ScientificImportPlanResolver {
    public static func resolve(
        stagedImport: StagedScientificImport,
        draft: ScientificImportManifestDraft
    ) throws -> ResolvedScientificImportPlan {
        guard draft.sourceBinding
            == ScientificImportDraftSourceBinding(stagedImport: stagedImport) else {
            throw ScientificImportPlanResolutionError(issues: [.sourceBindingMismatch])
        }

        var issues: [ScientificImportPlanIssue] = []

        let sourceTimeUnit = draft.sourceTimeUnit
        if sourceTimeUnit == nil {
            issues.append(.missingSourceTimeUnit)
        }

        let activityMode = draft.activityMode
        if activityMode == nil {
            issues.append(.missingActivityMode)
        }

        let groupDrafts = draft.eventScopeGroups
        if groupDrafts == nil {
            issues.append(.missingEventScopeGroups)
        } else if groupDrafts?.isEmpty == true {
            issues.append(.emptyEventScopeGroups)
        }

        var resolvedGroups: [ResolvedEventScopeGroupPlan] = []
        var flattenedSourceColumns: [Int] = []
        var firstGroupByID: [ScientificEventScopeGroupID: Int] = [:]
        // Spike-train identity is dataset-global, so first occurrences are tracked across all
        // groups rather than reset per group.
        var firstSpikeTrainByID: [ScientificSpikeTrainID: (group: Int, column: Int)] = [:]

        if let groupDrafts, !groupDrafts.isEmpty {
            resolvedGroups.reserveCapacity(groupDrafts.count)
            for (groupOffset, groupDraft) in groupDrafts.enumerated() {
                let groupNumber = groupOffset + 1

                if let semanticID = groupDraft.semanticID {
                    if let firstGroup = firstGroupByID[semanticID] {
                        issues.append(
                            .duplicateGroupSemanticID(
                                semanticID: semanticID,
                                firstGroup: firstGroup,
                                duplicateGroup: groupNumber
                            )
                        )
                    } else {
                        firstGroupByID[semanticID] = groupNumber
                    }
                } else {
                    issues.append(.missingGroupSemanticID(group: groupNumber))
                }

                if groupDraft.spikeTrains.isEmpty {
                    issues.append(.groupHasNoSpikeTrains(group: groupNumber))
                }

                let resolvedTimeBasis: ResolvedEventScopeTimeBasisPlan?
                switch groupDraft.timeBasis {
                case .none:
                    issues.append(.missingGroupTimeBasis(group: groupNumber))
                    resolvedTimeBasis = nil
                case .recordingElapsed:
                    resolvedTimeBasis = .recordingElapsed
                case .eventRelative(let possibleOrigin):
                    guard let origin = possibleOrigin else {
                        issues.append(.missingEventRelativeOrigin(group: groupNumber))
                        resolvedTimeBasis = nil
                        break
                    }
                    if stagedImport.cell(at: origin.timestampCell) == nil {
                        issues.append(
                            .eventRelativeOriginOutsideStaging(
                                group: groupNumber,
                                cell: origin.timestampCell
                            )
                        )
                    } else if !groupDraft.eventDefinitions.contains(where: {
                        $0.sourceColumn == origin.timestampCell.column
                    }) {
                        issues.append(
                            .eventRelativeOriginNotInSameGroupEventDefinition(
                                group: groupNumber,
                                column: origin.timestampCell.column.oneBasedIndex
                            )
                        )
                    }
                    resolvedTimeBasis = .eventRelative(origin: origin)
                }

                var resolvedSpikeTrains: [ResolvedSpikeTrainColumnPlan] = []
                resolvedSpikeTrains.reserveCapacity(groupDraft.spikeTrains.count)
                for spikeDraft in groupDraft.spikeTrains {
                    let column = spikeDraft.sourceColumn.oneBasedIndex
                    flattenedSourceColumns.append(column)
                    appendOutOfRangeIssueIfNeeded(
                        column: column,
                        group: groupNumber,
                        stagedColumnCount: stagedImport.columns.count,
                        issues: &issues
                    )

                    if let semanticID = spikeDraft.semanticID {
                        if let first = firstSpikeTrainByID[semanticID] {
                            if first.group == groupNumber {
                                issues.append(
                                    .duplicateSpikeTrainSemanticID(
                                        group: groupNumber,
                                        semanticID: semanticID,
                                        firstColumn: first.column,
                                        duplicateColumn: column
                                    )
                                )
                            } else {
                                issues.append(
                                    .duplicateSpikeTrainSemanticIDAcrossGroups(
                                        semanticID: semanticID,
                                        firstGroup: first.group,
                                        firstColumn: first.column,
                                        duplicateGroup: groupNumber,
                                        duplicateColumn: column
                                    )
                                )
                            }
                        } else {
                            firstSpikeTrainByID[semanticID] = (group: groupNumber, column: column)
                        }
                    } else {
                        issues.append(
                            .missingSpikeTrainSemanticID(group: groupNumber, column: column)
                        )
                    }

                    if spikeDraft.orderDecision == nil {
                        issues.append(
                            .missingSpikeTrainOrderDecision(group: groupNumber, column: column)
                        )
                    }
                    if spikeDraft.duplicateDecision == nil {
                        issues.append(
                            .missingSpikeTrainDuplicateDecision(group: groupNumber, column: column)
                        )
                    }
                    if let activityMode,
                       activityMode != .putativeSingleUnit,
                       spikeDraft.duplicateDecision == .collapseExact {
                        issues.append(
                            .duplicateCollapseNotAllowed(
                                activityMode: activityMode,
                                group: groupNumber,
                                column: column
                            )
                        )
                    }

                    if let semanticID = spikeDraft.semanticID,
                       let orderDecision = spikeDraft.orderDecision,
                       let duplicateDecision = spikeDraft.duplicateDecision {
                        resolvedSpikeTrains.append(
                            ResolvedSpikeTrainColumnPlan(
                                sourceColumn: spikeDraft.sourceColumn,
                                semanticID: semanticID,
                                orderDecision: orderDecision,
                                duplicateDecision: duplicateDecision
                            )
                        )
                    }
                }

                var resolvedEventDefinitions: [ResolvedEventDefinitionColumnPlan] = []
                var firstEventColumnByID: [ScientificEventDefinitionID: Int] = [:]
                resolvedEventDefinitions.reserveCapacity(groupDraft.eventDefinitions.count)
                for eventDraft in groupDraft.eventDefinitions {
                    let column = eventDraft.sourceColumn.oneBasedIndex
                    flattenedSourceColumns.append(column)
                    appendOutOfRangeIssueIfNeeded(
                        column: column,
                        group: groupNumber,
                        stagedColumnCount: stagedImport.columns.count,
                        issues: &issues
                    )

                    if let semanticID = eventDraft.semanticID {
                        if let firstColumn = firstEventColumnByID[semanticID] {
                            issues.append(
                                .duplicateEventDefinitionSemanticID(
                                    group: groupNumber,
                                    semanticID: semanticID,
                                    firstColumn: firstColumn,
                                    duplicateColumn: column
                                )
                            )
                        } else {
                            firstEventColumnByID[semanticID] = column
                        }
                    } else {
                        issues.append(
                            .missingEventDefinitionSemanticID(group: groupNumber, column: column)
                        )
                    }
                    if eventDraft.eventTypeID == nil {
                        issues.append(.missingEventTypeID(group: groupNumber, column: column))
                    }
                    if eventDraft.orderDecision == nil {
                        issues.append(.missingEventOrderDecision(group: groupNumber, column: column))
                    }

                    if let semanticID = eventDraft.semanticID,
                       let eventTypeID = eventDraft.eventTypeID,
                       let orderDecision = eventDraft.orderDecision {
                        resolvedEventDefinitions.append(
                            ResolvedEventDefinitionColumnPlan(
                                sourceColumn: eventDraft.sourceColumn,
                                semanticID: semanticID,
                                eventTypeID: eventTypeID,
                                orderDecision: orderDecision
                            )
                        )
                    }
                }

                if let semanticID = groupDraft.semanticID,
                   let resolvedTimeBasis,
                   !groupDraft.spikeTrains.isEmpty,
                   resolvedSpikeTrains.count == groupDraft.spikeTrains.count,
                   resolvedEventDefinitions.count == groupDraft.eventDefinitions.count {
                    resolvedGroups.append(
                        ResolvedEventScopeGroupPlan(
                            semanticID: semanticID,
                            spikeTrains: resolvedSpikeTrains,
                            eventDefinitions: resolvedEventDefinitions,
                            timeBasis: resolvedTimeBasis
                        )
                    )
                }
            }

            appendHeaderlessSourceIssues(
                stagedImport: stagedImport,
                groupDrafts: groupDrafts,
                issues: &issues
            )
            appendPartitionIssues(
                flattenedSourceColumns: flattenedSourceColumns,
                stagedColumnCount: stagedImport.columns.count,
                issues: &issues
            )
        }

        var resolvedAttributeDefinitions: [ResolvedEventAttributeDefinitionPlan] = []
        var firstAttributeDefinitionByKey: [EventAttributeKey: Int] = [:]
        resolvedAttributeDefinitions.reserveCapacity(draft.eventAttributeDefinitions.count)
        for (definitionOffset, definitionDraft) in draft.eventAttributeDefinitions.enumerated() {
            let definitionNumber = definitionOffset + 1
            let key = definitionDraft.key

            if let firstDefinition = firstAttributeDefinitionByKey[key] {
                issues.append(
                    .duplicateEventAttributeKey(
                        key: key,
                        firstDefinition: firstDefinition,
                        duplicateDefinition: definitionNumber
                    )
                )
            } else {
                firstAttributeDefinitionByKey[key] = definitionNumber
            }
            if definitionDraft.scalarType == nil {
                issues.append(
                    .missingEventAttributeScalarType(definition: definitionNumber, key: key)
                )
            }
            if definitionDraft.role == nil {
                issues.append(.missingEventAttributeRole(definition: definitionNumber, key: key))
            }
            if definitionDraft.unit == nil {
                issues.append(.missingEventAttributeUnit(definition: definitionNumber, key: key))
            }
            if definitionDraft.emptyStringPolicy == nil {
                issues.append(
                    .missingEventAttributeEmptyStringPolicy(
                        definition: definitionNumber,
                        key: key
                    )
                )
            }
            if let scalarType = definitionDraft.scalarType,
               scalarType != .string,
               definitionDraft.emptyStringPolicy == .allowExplicitEmptyString {
                issues.append(
                    .explicitEmptyStringAllowedForNonString(
                        definition: definitionNumber,
                        key: key,
                        scalarType: scalarType
                    )
                )
            }

            if let scalarType = definitionDraft.scalarType,
               let role = definitionDraft.role,
               let unit = definitionDraft.unit,
               let emptyStringPolicy = definitionDraft.emptyStringPolicy {
                resolvedAttributeDefinitions.append(
                    ResolvedEventAttributeDefinitionPlan(
                        key: key,
                        scalarType: scalarType,
                        role: role,
                        unit: unit,
                        emptyStringPolicy: emptyStringPolicy
                    )
                )
            }
        }

        if !issues.isEmpty {
            throw ScientificImportPlanResolutionError(issues: issues)
        }

        guard let sourceTimeUnit,
              let activityMode,
              let groupDrafts,
              resolvedGroups.count == groupDrafts.count,
              resolvedAttributeDefinitions.count == draft.eventAttributeDefinitions.count else {
            throw ScientificImportPlanResolutionError(issues: [.internalResolutionInvariant])
        }

        return ResolvedScientificImportPlan(
            source: ResolvedScientificImportSource(stagedImport: stagedImport),
            sourceTimeUnit: sourceTimeUnit,
            activityMode: activityMode,
            eventScopeGroups: resolvedGroups,
            eventAttributeDefinitions: resolvedAttributeDefinitions
        )
    }

    private static func appendOutOfRangeIssueIfNeeded(
        column: Int,
        group: Int,
        stagedColumnCount: Int,
        issues: inout [ScientificImportPlanIssue]
    ) {
        guard !(1...stagedColumnCount).contains(column) else { return }
        issues.append(.sourceColumnOutsideStaging(group: group, column: column))
    }

    private static func appendHeaderlessSourceIssues(
        stagedImport: StagedScientificImport,
        groupDrafts: [EventScopeGroupManifestDraft],
        issues: inout [ScientificImportPlanIssue]
    ) {
        guard stagedImport.columns.first?.header == nil, !groupDrafts.isEmpty else { return }

        if groupDrafts.count != 1 {
            issues.append(
                .headerlessSourceRequiresSingleGroup(actualGroupCount: groupDrafts.count)
            )
        }
        for (groupOffset, group) in groupDrafts.enumerated() {
            let groupNumber = groupOffset + 1
            for eventDefinition in group.eventDefinitions {
                issues.append(
                    .headerlessSourceContainsEventDefinition(
                        group: groupNumber,
                        column: eventDefinition.sourceColumn.oneBasedIndex
                    )
                )
            }
            if case .eventRelative = group.timeBasis {
                issues.append(.headerlessSourceRequiresRecordingElapsed(group: groupNumber))
            }
        }
    }

    private static func appendPartitionIssues(
        flattenedSourceColumns: [Int],
        stagedColumnCount: Int,
        issues: inout [ScientificImportPlanIssue]
    ) {
        if flattenedSourceColumns.count != stagedColumnCount {
            issues.append(
                .flattenedSourcePartitionCountMismatch(
                    expected: stagedColumnCount,
                    actual: flattenedSourceColumns.count
                )
            )
        }

        let comparableCount = Swift.min(flattenedSourceColumns.count, stagedColumnCount)
        if comparableCount > 0 {
            for offset in 0..<comparableCount {
                let expected = offset + 1
                let actual = flattenedSourceColumns[offset]
                if actual != expected {
                    issues.append(
                        .flattenedSourcePartitionOrderMismatch(
                            position: expected,
                            expected: expected,
                            actual: actual
                        )
                    )
                }
            }
        }

        var assignmentCounts = Array(repeating: 0, count: stagedColumnCount)
        for column in flattenedSourceColumns where (1...stagedColumnCount).contains(column) {
            assignmentCounts[column - 1] += 1
        }
        for (offset, count) in assignmentCounts.enumerated() {
            let column = offset + 1
            if count == 0 {
                issues.append(.sourceColumnNotAssigned(column: column))
            } else if count > 1 {
                issues.append(.sourceColumnAssignedMultipleTimes(column: column, count: count))
            }
        }
    }
}
