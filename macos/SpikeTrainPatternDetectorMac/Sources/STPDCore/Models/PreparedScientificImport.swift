/// A successfully parsed and transformed scientific import that is still pre-analysis.
///
/// This value is not a canonical dataset, a confirmation, an authority grant, or a scientific
/// identity projection. In particular, `data` still contains presentation attributes, while
/// `provenance` retains source-only facts and user-confirmed transformation choices.
public struct PreparedScientificImport: Hashable, Sendable {
    public let data: PreparedScientificImportData
    public let provenance: ScientificImportNormalizationProvenance

    internal init(
        data: PreparedScientificImportData,
        provenance: ScientificImportNormalizationProvenance
    ) {
        self.data = data
        self.provenance = provenance
    }
}

/// Parsed values separated from their source locations. This is deliberately not an identity
/// projection because it includes presentation-only attributes.
public struct PreparedScientificImportData: Hashable, Sendable {
    public let activityMode: ScientificDatasetActivityMode
    public let eventScopeGroups: [PreparedEventScopeGroup]
    public let scientificAttributeDefinitions: [ResolvedEventAttributeDefinitionPlan]
    public let presentationAttributeDefinitions: [ResolvedEventAttributeDefinitionPlan]

    internal init(
        activityMode: ScientificDatasetActivityMode,
        eventScopeGroups: [PreparedEventScopeGroup],
        scientificAttributeDefinitions: [ResolvedEventAttributeDefinitionPlan],
        presentationAttributeDefinitions: [ResolvedEventAttributeDefinitionPlan]
    ) {
        self.activityMode = activityMode
        self.eventScopeGroups = eventScopeGroups
        self.scientificAttributeDefinitions = scientificAttributeDefinitions
        self.presentationAttributeDefinitions = presentationAttributeDefinitions
    }
}

public struct PreparedEventScopeGroup: Hashable, Sendable {
    public let semanticID: ScientificEventScopeGroupID
    public let timeBasis: PreparedEventScopeTimeBasis
    public let spikeTrains: [PreparedSpikeTrain]
    public let eventDefinitions: [PreparedEventDefinition]

    internal init(
        semanticID: ScientificEventScopeGroupID,
        timeBasis: PreparedEventScopeTimeBasis,
        spikeTrains: [PreparedSpikeTrain],
        eventDefinitions: [PreparedEventDefinition]
    ) {
        self.semanticID = semanticID
        self.timeBasis = timeBasis
        self.spikeTrains = spikeTrains
        self.eventDefinitions = eventDefinitions
    }
}

public enum PreparedEventScopeTimeBasis: Hashable, Sendable {
    case recordingElapsed
    case eventRelative(origin: PreparedEventOrigin)
}

/// A group-local, semantically unique selected origin. Its tick is always zero. Source location
/// and the pre-rebase source tick remain in `ScientificImportNormalizationProvenance` only.
public struct PreparedEventOrigin: Hashable, Sendable {
    public let eventDefinitionID: ScientificEventDefinitionID
    public let tick: MicrosecondTick
    public let scientificAttributes: [PreparedEventAttribute]

    internal init(
        eventDefinitionID: ScientificEventDefinitionID,
        scientificAttributes: [PreparedEventAttribute]
    ) {
        self.eventDefinitionID = eventDefinitionID
        self.tick = .zero
        self.scientificAttributes = scientificAttributes
    }
}

public struct PreparedSpikeTrain: Hashable, Sendable {
    public let semanticID: ScientificSpikeTrainID
    public let timestamps: [MicrosecondTick]

    internal init(
        semanticID: ScientificSpikeTrainID,
        timestamps: [MicrosecondTick]
    ) {
        self.semanticID = semanticID
        self.timestamps = timestamps
    }
}

public struct PreparedEventDefinition: Hashable, Sendable {
    public let semanticID: ScientificEventDefinitionID
    public let eventTypeID: ScientificEventTypeID
    public let occurrences: [PreparedEventOccurrence]

    internal init(
        semanticID: ScientificEventDefinitionID,
        eventTypeID: ScientificEventTypeID,
        occurrences: [PreparedEventOccurrence]
    ) {
        self.semanticID = semanticID
        self.eventTypeID = eventTypeID
        self.occurrences = occurrences
    }
}

public struct PreparedEventOccurrence: Hashable, Sendable {
    public let tick: MicrosecondTick
    public let scientificAttributes: [PreparedEventAttribute]
    public let presentationAttributes: [PreparedEventAttribute]

    internal init(
        tick: MicrosecondTick,
        scientificAttributes: [PreparedEventAttribute],
        presentationAttributes: [PreparedEventAttribute]
    ) {
        self.tick = tick
        self.scientificAttributes = scientificAttributes
        self.presentationAttributes = presentationAttributes
    }
}

public struct PreparedEventAttribute: Hashable, Sendable {
    public let key: EventAttributeKey
    public let value: EventAttributeValue

    internal init(key: EventAttributeKey, value: EventAttributeValue) {
        self.key = key
        self.value = value
    }
}

/// All source-only facts and the complete source-bound plan used to produce `data`.
public struct ScientificImportNormalizationProvenance: Hashable, Sendable {
    public let resolvedPlan: ResolvedScientificImportPlan
    public let eventScopeGroups: [PreparedEventScopeGroupProvenance]

    internal init(
        resolvedPlan: ResolvedScientificImportPlan,
        eventScopeGroups: [PreparedEventScopeGroupProvenance]
    ) {
        self.resolvedPlan = resolvedPlan
        self.eventScopeGroups = eventScopeGroups
    }
}

public struct PreparedEventScopeGroupProvenance: Hashable, Sendable {
    public let semanticID: ScientificEventScopeGroupID
    public let timeBasis: PreparedEventScopeTimeBasisProvenance
    public let spikeTrains: [PreparedSpikeTrainProvenance]
    public let eventDefinitions: [PreparedEventDefinitionProvenance]

    internal init(
        semanticID: ScientificEventScopeGroupID,
        timeBasis: PreparedEventScopeTimeBasisProvenance,
        spikeTrains: [PreparedSpikeTrainProvenance],
        eventDefinitions: [PreparedEventDefinitionProvenance]
    ) {
        self.semanticID = semanticID
        self.timeBasis = timeBasis
        self.spikeTrains = spikeTrains
        self.eventDefinitions = eventDefinitions
    }
}

/// The inspected source coordinate behind the prepared time basis. These values are audit facts,
/// not part of the prepared event-origin tuple.
public enum PreparedEventScopeTimeBasisProvenance: Hashable, Sendable {
    case recordingElapsed
    case eventRelative(
        originCell: StagedSourceCellReference,
        sourceOriginTick: MicrosecondTick
    )
}

public struct PreparedSpikeTrainProvenance: Hashable, Sendable {
    public let semanticID: ScientificSpikeTrainID
    public let sourceColumn: StagedSourceColumnReference
    public let orderDecision: TimestampOrderDecision
    public let duplicateDecision: ExactDuplicateDecision
    public let sourceOrderDescentCount: Int
    /// One nonempty source-cell list per output timestamp. Multiple cells mean an explicit exact
    /// duplicate collapse; cells remain in source-row order.
    public let timestampSources: [[StagedSourceCellReference]]

    internal init(
        semanticID: ScientificSpikeTrainID,
        sourceColumn: StagedSourceColumnReference,
        orderDecision: TimestampOrderDecision,
        duplicateDecision: ExactDuplicateDecision,
        sourceOrderDescentCount: Int,
        timestampSources: [[StagedSourceCellReference]]
    ) {
        self.semanticID = semanticID
        self.sourceColumn = sourceColumn
        self.orderDecision = orderDecision
        self.duplicateDecision = duplicateDecision
        self.sourceOrderDescentCount = sourceOrderDescentCount
        self.timestampSources = timestampSources
    }
}

public struct PreparedEventDefinitionProvenance: Hashable, Sendable {
    public let semanticID: ScientificEventDefinitionID
    public let sourceColumn: StagedSourceColumnReference
    public let orderDecision: TimestampOrderDecision
    public let sourceOrderDescentCount: Int
    /// Entries use the final occurrence order and remain one-to-one with prepared occurrences.
    public let occurrences: [PreparedEventOccurrenceProvenance]

    internal init(
        semanticID: ScientificEventDefinitionID,
        sourceColumn: StagedSourceColumnReference,
        orderDecision: TimestampOrderDecision,
        sourceOrderDescentCount: Int,
        occurrences: [PreparedEventOccurrenceProvenance]
    ) {
        self.semanticID = semanticID
        self.sourceColumn = sourceColumn
        self.orderDecision = orderDecision
        self.sourceOrderDescentCount = sourceOrderDescentCount
        self.occurrences = occurrences
    }
}

public struct PreparedEventOccurrenceProvenance: Hashable, Sendable {
    public let timestampCell: StagedSourceCellReference
    /// Attribute traces use deterministic UTF-8 key order, independent of metadata-row order.
    public let attributes: [PreparedEventAttributeProvenance]

    internal init(
        timestampCell: StagedSourceCellReference,
        attributes: [PreparedEventAttributeProvenance]
    ) {
        self.timestampCell = timestampCell
        self.attributes = attributes
    }
}

public struct PreparedEventAttributeProvenance: Hashable, Sendable {
    public let key: EventAttributeKey
    public let valueCell: StagedSourceCellReference
    public let inlineUnitSuggestion: PreparedInlineUnitSuggestionProvenance

    internal init(
        key: EventAttributeKey,
        valueCell: StagedSourceCellReference,
        inlineUnitSuggestion: PreparedInlineUnitSuggestionProvenance
    ) {
        self.key = key
        self.valueCell = valueCell
        self.inlineUnitSuggestion = inlineUnitSuggestion
    }
}

public enum PreparedInlineUnitSuggestionProvenance: Hashable, Sendable {
    case absent
    case present(symbol: OpaqueUnitSymbol, sourceCell: StagedSourceCellReference)
}
