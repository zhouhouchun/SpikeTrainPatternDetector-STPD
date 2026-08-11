/// Immutable source facts bound to a resolved import plan.
///
/// Suggestions are deliberately absent: changing a non-authoritative suggestion cannot change a
/// resolved plan, while raw headers, cells, blanks, multiplicity, and source order remain present.
public struct ResolvedScientificImportSource: Hashable, Sendable {
    public let source: StagedTabularSource
    public let sourceTransactionBinding: StagedSourceTransactionBinding?
    public let columns: [StagedScientificColumn]
    public let dataRowCount: Int

    internal init(stagedImport: StagedScientificImport) {
        self.source = stagedImport.source
        self.sourceTransactionBinding = stagedImport.sourceTransactionBinding
        self.columns = stagedImport.columns
        self.dataRowCount = stagedImport.dataRowCount
    }
}

/// A complete group-local time-basis choice. Content validation of the referenced event cell is
/// intentionally outside the plan resolver.
public enum ResolvedEventScopeTimeBasisPlan: Hashable, Sendable {
    case recordingElapsed
    case eventRelative(origin: StagedEventOccurrenceReference)
}

public struct ResolvedSpikeTrainColumnPlan: Hashable, Sendable {
    public let sourceColumn: StagedSourceColumnReference
    public let semanticID: ScientificSpikeTrainID
    public let orderDecision: TimestampOrderDecision
    public let duplicateDecision: ExactDuplicateDecision

    internal init(
        sourceColumn: StagedSourceColumnReference,
        semanticID: ScientificSpikeTrainID,
        orderDecision: TimestampOrderDecision,
        duplicateDecision: ExactDuplicateDecision
    ) {
        self.sourceColumn = sourceColumn
        self.semanticID = semanticID
        self.orderDecision = orderDecision
        self.duplicateDecision = duplicateDecision
    }
}

public struct ResolvedEventDefinitionColumnPlan: Hashable, Sendable {
    public let sourceColumn: StagedSourceColumnReference
    public let semanticID: ScientificEventDefinitionID
    public let eventTypeID: ScientificEventTypeID
    public let orderDecision: TimestampOrderDecision

    internal init(
        sourceColumn: StagedSourceColumnReference,
        semanticID: ScientificEventDefinitionID,
        eventTypeID: ScientificEventTypeID,
        orderDecision: TimestampOrderDecision
    ) {
        self.sourceColumn = sourceColumn
        self.semanticID = semanticID
        self.eventTypeID = eventTypeID
        self.orderDecision = orderDecision
    }
}

public struct ResolvedEventScopeGroupPlan: Hashable, Sendable {
    public let semanticID: ScientificEventScopeGroupID
    public let spikeTrains: [ResolvedSpikeTrainColumnPlan]
    public let eventDefinitions: [ResolvedEventDefinitionColumnPlan]
    public let timeBasis: ResolvedEventScopeTimeBasisPlan

    internal init(
        semanticID: ScientificEventScopeGroupID,
        spikeTrains: [ResolvedSpikeTrainColumnPlan],
        eventDefinitions: [ResolvedEventDefinitionColumnPlan],
        timeBasis: ResolvedEventScopeTimeBasisPlan
    ) {
        self.semanticID = semanticID
        self.spikeTrains = spikeTrains
        self.eventDefinitions = eventDefinitions
        self.timeBasis = timeBasis
    }
}

public struct ResolvedEventAttributeDefinitionPlan: Hashable, Sendable {
    public let key: EventAttributeKey
    public let scalarType: EventAttributeScalarType
    public let role: EventAttributeRole
    public let unit: ConfirmedEventAttributeUnit
    public let emptyStringPolicy: EventEmptyStringPolicy

    internal init(
        key: EventAttributeKey,
        scalarType: EventAttributeScalarType,
        role: EventAttributeRole,
        unit: ConfirmedEventAttributeUnit,
        emptyStringPolicy: EventEmptyStringPolicy
    ) {
        self.key = key
        self.scalarType = scalarType
        self.role = role
        self.unit = unit
        self.emptyStringPolicy = emptyStringPolicy
    }
}

/// A complete, source-bound import plan. "Resolved" here means that structural references and
/// explicit choices are complete; it does not claim that timestamp or attribute grammar is valid.
public struct ResolvedScientificImportPlan: Hashable, Sendable {
    public let source: ResolvedScientificImportSource
    /// The single dataset-global RecordingSegment that structurally owns the whole plan.
    public let recordingSegment: ConfirmedRecordingSegment
    public let sourceTimeUnit: SpikeTimeUnit
    public let activityMode: ScientificDatasetActivityMode
    public let eventScopeGroups: [ResolvedEventScopeGroupPlan]
    public let eventAttributeDefinitions: [ResolvedEventAttributeDefinitionPlan]

    internal init(
        source: ResolvedScientificImportSource,
        recordingSegment: ConfirmedRecordingSegment,
        sourceTimeUnit: SpikeTimeUnit,
        activityMode: ScientificDatasetActivityMode,
        eventScopeGroups: [ResolvedEventScopeGroupPlan],
        eventAttributeDefinitions: [ResolvedEventAttributeDefinitionPlan]
    ) {
        self.source = source
        self.recordingSegment = recordingSegment
        self.sourceTimeUnit = sourceTimeUnit
        self.activityMode = activityMode
        self.eventScopeGroups = eventScopeGroups
        self.eventAttributeDefinitions = eventAttributeDefinitions
    }
}
