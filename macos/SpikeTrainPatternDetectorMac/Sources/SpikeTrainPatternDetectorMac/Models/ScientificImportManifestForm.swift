import Foundation
import STPDCore

enum ScientificImportColumnRole: String, CaseIterable, Hashable, Sendable {
    case spikeTrain = "spike_train"
    case eventDefinition = "event_definition"
}

enum ScientificImportTimeBasisChoice: String, CaseIterable, Hashable, Sendable {
    case recordingElapsed = "recording_elapsed"
    case eventRelative = "event_relative"
}

enum ScientificImportAttributeUnitChoice: String, CaseIterable, Hashable, Sendable {
    case notApplicable = "not_applicable"
    case dimensionless
    case specified
}

struct ScientificImportColumnDecisionForm: Identifiable, Sendable {
    /// Ephemeral row identity for SwiftUI only. It never enters a manifest or scientific identity.
    let id: UUID
    let sourceColumn: StagedSourceColumnReference
    let header: String?

    /// The first source column necessarily starts the first group. Every later boundary remains
    /// unresolved until the user explicitly chooses whether a new contiguous group starts there.
    var startsNewGroup: Bool?
    var groupSemanticIDText = ""
    var groupTimeBasis: ScientificImportTimeBasisChoice?
    var eventOriginColumnText = ""
    var eventOriginDataRowText = ""

    var role: ScientificImportColumnRole?
    var semanticIDText = ""
    var eventTypeIDText = ""
    var orderDecision: TimestampOrderDecision?
    var duplicateDecision: ExactDuplicateDecision?

    init(column: StagedScientificColumn, isFirst: Bool) {
        id = UUID()
        sourceColumn = column.sourceColumn
        header = column.header
        startsNewGroup = isFirst ? true : nil
    }
}

struct ScientificImportAttributeDecisionForm: Identifiable, Sendable {
    /// Ephemeral row identity for SwiftUI only. It never enters a manifest or scientific identity.
    let id: UUID
    var keyText: String
    var scalarType: EventAttributeScalarType?
    var role: EventAttributeRole?
    var unitChoice: ScientificImportAttributeUnitChoice?
    var specifiedUnitText = ""
    var emptyStringPolicy: EventEmptyStringPolicy?

    init(keyText: String = "") {
        id = UUID()
        self.keyText = keyText
    }
}

enum ScientificImportManifestFormIssue: Error, Equatable, Sendable {
    case sourceBindingChanged
    case sourceColumnsChanged
    case invalidRecordingSegmentID(error: ScientificSemanticIDError)
    case missingGroupBoundary(column: Int)
    case missingColumnRole(column: Int)
    case groupStartsWithEvent(column: Int)
    case spikeAfterEventWithinGroup(column: Int)
    case invalidGroupSemanticID(column: Int, error: ScientificSemanticIDError)
    case invalidColumnSemanticID(column: Int, error: ScientificSemanticIDError)
    case invalidEventTypeID(column: Int, error: ScientificSemanticIDError)
    case invalidEventOriginColumn(column: Int)
    case invalidEventOriginDataRow(column: Int)
    case missingAttributeKey(definition: Int)
    case invalidAttributeKey(definition: Int, error: EventAttributeKeyError)
    case missingSpecifiedUnit(definition: Int)
    case invalidSpecifiedUnit(definition: Int, error: OpaqueUnitSymbolError)
}

struct ScientificImportManifestFormBuildError: Error, Equatable, Sendable {
    let issues: [ScientificImportManifestFormIssue]
}

/// UI-facing, deliberately unresolved scientific choices for one exact staged source.
///
/// This form does not infer roles, semantic IDs, timestamp units, ordering, duplicate handling,
/// event origins, or attribute semantics. Building it produces only a manifest *draft*; the Core
/// resolver, normalizer, and independent validator remain the authoritative checking pipeline.
struct ScientificImportManifestForm: Sendable {
    let sourceBinding: ScientificImportDraftSourceBinding
    var sourceTimeUnit: SpikeTimeUnit?
    var activityMode: ScientificDatasetActivityMode?
    /// The single dataset-global RecordingSegment decisions. All start unresolved (blank text /
    /// nil / unconfirmed); none is silently defaulted.
    var recordingSegmentIDText: String = ""
    var recordingRegime: ScientificRecordingRegime?
    var importedExcerptCoverage: ImportedExcerptCoverage?
    /// The user must explicitly confirm that exact acquisition bounds are unknown/unavailable; until
    /// then bounds remain unresolved. `false` is the unconfirmed state, never a hidden default.
    var observationBoundsConfirmedUnavailable: Bool = false
    var columns: [ScientificImportColumnDecisionForm]
    var attributes: [ScientificImportAttributeDecisionForm] = []

    init(stagedImport: StagedScientificImport) {
        sourceBinding = ScientificImportDraftSourceBinding(stagedImport: stagedImport)
        sourceTimeUnit = nil
        activityMode = nil
        columns = stagedImport.columns.enumerated().map { offset, column in
            ScientificImportColumnDecisionForm(column: column, isFirst: offset == 0)
        }
    }

    /// The confirmed bounds availability, or `nil` until the user explicitly confirms.
    var confirmedObservationBounds: ObservationBoundsAvailability? {
        observationBoundsConfirmedUnavailable ? .unknownOrUnavailable : nil
    }

    mutating func addAttribute(keyText: String = "") {
        attributes.append(ScientificImportAttributeDecisionForm(keyText: keyText))
    }

    /// Activity mode changes the scientific meaning of coincident timestamps. Existing duplicate
    /// decisions therefore become stale and must be confirmed again; they are never carried across
    /// modes or silently rewritten to another policy.
    mutating func selectActivityMode(_ mode: ScientificDatasetActivityMode?) {
        guard activityMode != mode else { return }
        activityMode = mode
        for index in columns.indices {
            columns[index].duplicateDecision = nil
        }
    }

    mutating func removeAttributes(ids: Set<UUID>) {
        attributes.removeAll { ids.contains($0.id) }
    }

    mutating func applyAttributeRole(_ role: EventAttributeRole, to ids: Set<UUID>) {
        for index in attributes.indices where ids.contains(attributes[index].id) {
            attributes[index].role = role
        }
    }

    @discardableResult
    mutating func updateColumn(
        id: UUID,
        _ mutation: (inout ScientificImportColumnDecisionForm) -> Void
    ) -> Bool {
        guard let index = columns.firstIndex(where: { $0.id == id }) else { return false }
        mutation(&columns[index])
        return true
    }

    @discardableResult
    mutating func updateAttribute(
        id: UUID,
        _ mutation: (inout ScientificImportAttributeDecisionForm) -> Void
    ) -> Bool {
        guard let index = attributes.firstIndex(where: { $0.id == id }) else { return false }
        mutation(&attributes[index])
        return true
    }

    func makeManifestDraft(
        boundTo stagedImport: StagedScientificImport
    ) throws -> ScientificImportManifestDraft {
        var issues: [ScientificImportManifestFormIssue] = []
        guard sourceBinding == ScientificImportDraftSourceBinding(stagedImport: stagedImport) else {
            throw ScientificImportManifestFormBuildError(issues: [.sourceBindingChanged])
        }
        guard columns.count == stagedImport.columns.count,
              zip(columns, stagedImport.columns).allSatisfy({ form, staged in
                  form.sourceColumn == staged.sourceColumn && form.header == staged.header
              }) else {
            throw ScientificImportManifestFormBuildError(issues: [.sourceColumnsChanged])
        }

        var groupDrafts: [EventScopeGroupManifestDraft] = []
        var currentGroup: WorkingGroup?

        func semanticID(
            from text: String,
            column: Int,
            issue: (Int, ScientificSemanticIDError) -> ScientificImportManifestFormIssue
        ) -> ScientificSemanticID? {
            guard !text.isEmpty else { return nil }
            do {
                return try ScientificSemanticID(validating: text)
            } catch let error as ScientificSemanticIDError {
                issues.append(issue(column, error))
                return nil
            } catch {
                return nil
            }
        }

        func finishCurrentGroup() {
            guard let group = currentGroup else { return }
            groupDrafts.append(
                EventScopeGroupManifestDraft(
                    semanticID: group.semanticID.map(ScientificEventScopeGroupID.init),
                    spikeTrains: group.spikeTrains,
                    eventDefinitions: group.eventDefinitions,
                    timeBasis: group.timeBasis
                )
            )
        }

        for (offset, column) in columns.enumerated() {
            let sourceIndex = column.sourceColumn.oneBasedIndex
            let startsNewGroup: Bool
            if offset == 0 {
                startsNewGroup = true
            } else if let decision = column.startsNewGroup {
                startsNewGroup = decision
            } else {
                issues.append(.missingGroupBoundary(column: sourceIndex))
                continue
            }

            if startsNewGroup {
                finishCurrentGroup()
                let groupID = semanticID(
                    from: column.groupSemanticIDText,
                    column: sourceIndex,
                    issue: { .invalidGroupSemanticID(column: $0, error: $1) }
                )
                let timeBasis = makeTimeBasis(
                    from: column,
                    issues: &issues
                )
                currentGroup = WorkingGroup(
                    semanticID: groupID,
                    timeBasis: timeBasis,
                    spikeTrains: [],
                    eventDefinitions: [],
                    hasSeenEventDefinition: false
                )
            }

            guard var group = currentGroup else {
                continue
            }
            guard let role = column.role else {
                issues.append(.missingColumnRole(column: sourceIndex))
                currentGroup = group
                continue
            }

            let columnID = semanticID(
                from: column.semanticIDText,
                column: sourceIndex,
                issue: { .invalidColumnSemanticID(column: $0, error: $1) }
            )
            switch role {
            case .spikeTrain:
                if group.hasSeenEventDefinition {
                    issues.append(.spikeAfterEventWithinGroup(column: sourceIndex))
                }
                group.spikeTrains.append(
                    SpikeTrainColumnManifestDraft(
                        sourceColumn: column.sourceColumn,
                        semanticID: columnID.map { ScientificSpikeTrainID($0) },
                        orderDecision: column.orderDecision,
                        duplicateDecision: column.duplicateDecision
                    )
                )
            case .eventDefinition:
                if group.spikeTrains.isEmpty {
                    issues.append(.groupStartsWithEvent(column: sourceIndex))
                }
                group.hasSeenEventDefinition = true
                let eventTypeID = semanticID(
                    from: column.eventTypeIDText,
                    column: sourceIndex,
                    issue: { .invalidEventTypeID(column: $0, error: $1) }
                )
                group.eventDefinitions.append(
                    EventDefinitionColumnManifestDraft(
                        sourceColumn: column.sourceColumn,
                        semanticID: columnID.map { ScientificEventDefinitionID($0) },
                        eventTypeID: eventTypeID.map { ScientificEventTypeID($0) },
                        orderDecision: column.orderDecision
                    )
                )
            }
            currentGroup = group
        }
        finishCurrentGroup()

        let attributeDrafts = makeAttributeDrafts(issues: &issues)

        // The exact entered text is validated with no whitespace trimming: an empty field stays
        // unresolved (the resolver later reports it missing), while any nonempty text is validated
        // verbatim so a nonblank invalid value produces a precise `.invalidRecordingSegmentID`
        // diagnostic instead of being silently dropped or rewritten.
        let recordingSegmentID: ScientificRecordingSegmentID?
        if recordingSegmentIDText.isEmpty {
            recordingSegmentID = nil
        } else {
            do {
                recordingSegmentID = ScientificRecordingSegmentID(
                    try ScientificSemanticID(validating: recordingSegmentIDText)
                )
            } catch let error as ScientificSemanticIDError {
                issues.append(.invalidRecordingSegmentID(error: error))
                recordingSegmentID = nil
            } catch {
                recordingSegmentID = nil
            }
        }

        guard issues.isEmpty else {
            throw ScientificImportManifestFormBuildError(issues: issues)
        }

        return ScientificImportManifestDraft(
            boundTo: stagedImport,
            sourceTimeUnit: sourceTimeUnit,
            activityMode: activityMode,
            recordingSegmentID: recordingSegmentID,
            recordingRegime: recordingRegime,
            importedExcerptCoverage: importedExcerptCoverage,
            observationBoundsAvailability: confirmedObservationBounds,
            eventScopeGroups: groupDrafts,
            eventAttributeDefinitions: attributeDrafts
        )
    }

    private func makeTimeBasis(
        from column: ScientificImportColumnDecisionForm,
        issues: inout [ScientificImportManifestFormIssue]
    ) -> EventScopeTimeBasisDraft? {
        switch column.groupTimeBasis {
        case .none:
            return nil
        case .recordingElapsed:
            return .recordingElapsed
        case .eventRelative:
            guard !column.eventOriginColumnText.isEmpty else {
                return .eventRelative(origin: nil)
            }
            guard let originColumn = Int(column.eventOriginColumnText), originColumn > 0 else {
                issues.append(.invalidEventOriginColumn(column: column.sourceColumn.oneBasedIndex))
                return .eventRelative(origin: nil)
            }
            guard !column.eventOriginDataRowText.isEmpty else {
                return .eventRelative(origin: nil)
            }
            guard let originRow = Int(column.eventOriginDataRowText), originRow > 0 else {
                issues.append(.invalidEventOriginDataRow(column: column.sourceColumn.oneBasedIndex))
                return .eventRelative(origin: nil)
            }
            guard let sourceColumn = try? StagedSourceColumnReference(oneBasedIndex: originColumn),
                  let sourceCell = try? StagedSourceCellReference(
                      column: sourceColumn,
                      oneBasedDataRowIndex: originRow
                  ) else {
                return .eventRelative(origin: nil)
            }
            return .eventRelative(
                origin: StagedEventOccurrenceReference(timestampCell: sourceCell)
            )
        }
    }

    private func makeAttributeDrafts(
        issues: inout [ScientificImportManifestFormIssue]
    ) -> [EventAttributeDefinitionDraft] {
        var result: [EventAttributeDefinitionDraft] = []
        result.reserveCapacity(attributes.count)

        for (offset, form) in attributes.enumerated() {
            let definition = offset + 1
            guard !form.keyText.isEmpty else {
                issues.append(.missingAttributeKey(definition: definition))
                continue
            }
            let key: EventAttributeKey
            do {
                key = try EventAttributeKey(validating: form.keyText)
            } catch let error as EventAttributeKeyError {
                issues.append(.invalidAttributeKey(definition: definition, error: error))
                continue
            } catch {
                continue
            }

            let unit: ConfirmedEventAttributeUnit?
            switch form.unitChoice {
            case .none:
                unit = nil
            case .notApplicable:
                unit = .notApplicable
            case .dimensionless:
                unit = .dimensionless
            case .specified:
                guard !form.specifiedUnitText.isEmpty else {
                    issues.append(.missingSpecifiedUnit(definition: definition))
                    continue
                }
                do {
                    unit = .specified(try OpaqueUnitSymbol(validating: form.specifiedUnitText))
                } catch let error as OpaqueUnitSymbolError {
                    issues.append(.invalidSpecifiedUnit(definition: definition, error: error))
                    continue
                } catch {
                    continue
                }
            }

            result.append(
                EventAttributeDefinitionDraft(
                    key: key,
                    scalarType: form.scalarType,
                    role: form.role,
                    unit: unit,
                    emptyStringPolicy: form.emptyStringPolicy
                )
            )
        }
        return result
    }
}

private struct WorkingGroup {
    let semanticID: ScientificSemanticID?
    let timeBasis: EventScopeTimeBasisDraft?
    var spikeTrains: [SpikeTrainColumnManifestDraft]
    var eventDefinitions: [EventDefinitionColumnManifestDraft]
    var hasSeenEventDefinition: Bool
}
