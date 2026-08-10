/// A format-neutral, pre-analysis scientific dataset projected from an independently validated
/// import. This model deliberately contains no source locations, presentation attributes, detector
/// settings, result authority, or inferred RecordingSegment/Trial structure.
public struct CanonicalScientificDataset: Hashable, Sendable {
    public let activityMode: ScientificDatasetActivityMode
    public let eventScopeGroups: [CanonicalEventScopeGroup]
    public let scientificAttributeDefinitions: [CanonicalEventAttributeDefinition]

    internal init(
        activityMode: ScientificDatasetActivityMode,
        eventScopeGroups: [CanonicalEventScopeGroup],
        scientificAttributeDefinitions: [CanonicalEventAttributeDefinition]
    ) {
        self.activityMode = activityMode
        self.eventScopeGroups = eventScopeGroups
        self.scientificAttributeDefinitions = scientificAttributeDefinitions
    }
}

public struct CanonicalEventScopeGroup: Hashable, Sendable {
    public let semanticID: ScientificEventScopeGroupID
    public let timeBasis: CanonicalEventScopeTimeBasis
    public let spikeTrains: [CanonicalSpikeTrain]
    public let eventDefinitions: [CanonicalEventDefinition]

    internal init(
        semanticID: ScientificEventScopeGroupID,
        timeBasis: CanonicalEventScopeTimeBasis,
        spikeTrains: [CanonicalSpikeTrain],
        eventDefinitions: [CanonicalEventDefinition]
    ) {
        self.semanticID = semanticID
        self.timeBasis = timeBasis
        self.spikeTrains = spikeTrains
        self.eventDefinitions = eventDefinitions
    }
}

public enum CanonicalEventScopeTimeBasis: Hashable, Sendable {
    case recordingElapsed
    case eventRelative(origin: CanonicalEventOrigin)
}

/// The scientific identity of a confirmed event-relative origin. Its projected tick remains the
/// canonical zero established by normalization; source coordinates stay outside this model.
public struct CanonicalEventOrigin: Hashable, Sendable {
    public let eventDefinitionID: ScientificEventDefinitionID
    public let tick: MicrosecondTick
    public let scientificAttributes: [CanonicalEventAttributeValue]

    internal init(
        eventDefinitionID: ScientificEventDefinitionID,
        tick: MicrosecondTick,
        scientificAttributes: [CanonicalEventAttributeValue]
    ) {
        self.eventDefinitionID = eventDefinitionID
        self.tick = tick
        self.scientificAttributes = scientificAttributes
    }
}

public struct CanonicalSpikeTrain: Hashable, Sendable {
    public let semanticID: ScientificSpikeTrainID
    /// Ordered canonical-grid timestamps. Repeated ticks are repeated spikes and remain present.
    public let rawTimestamps: [MicrosecondTick]

    internal init(
        semanticID: ScientificSpikeTrainID,
        rawTimestamps: [MicrosecondTick]
    ) {
        self.semanticID = semanticID
        self.rawTimestamps = rawTimestamps
    }
}

public struct CanonicalEventDefinition: Hashable, Sendable {
    public let semanticID: ScientificEventDefinitionID
    public let eventTypeID: ScientificEventTypeID
    public let occurrences: [CanonicalEventOccurrence]

    internal init(
        semanticID: ScientificEventDefinitionID,
        eventTypeID: ScientificEventTypeID,
        occurrences: [CanonicalEventOccurrence]
    ) {
        self.semanticID = semanticID
        self.eventTypeID = eventTypeID
        self.occurrences = occurrences
    }
}

public struct CanonicalEventOccurrence: Hashable, Sendable {
    public let tick: MicrosecondTick
    public let scientificAttributes: [CanonicalEventAttributeValue]

    internal init(
        tick: MicrosecondTick,
        scientificAttributes: [CanonicalEventAttributeValue]
    ) {
        self.tick = tick
        self.scientificAttributes = scientificAttributes
    }
}

/// A confirmed scientific event-attribute contract. The scientific role is encoded by this type;
/// presentation-only definitions never enter a `CanonicalScientificDataset`.
public struct CanonicalEventAttributeDefinition: Hashable, Sendable {
    public let key: EventAttributeKey
    public let scalarType: EventAttributeScalarType
    public let unit: ConfirmedEventAttributeUnit
    public let emptyStringPolicy: EventEmptyStringPolicy

    internal init(
        key: EventAttributeKey,
        scalarType: EventAttributeScalarType,
        unit: ConfirmedEventAttributeUnit,
        emptyStringPolicy: EventEmptyStringPolicy
    ) {
        self.key = key
        self.scalarType = scalarType
        self.unit = unit
        self.emptyStringPolicy = emptyStringPolicy
    }
}

/// One typed scientific value attached to an event occurrence or event-relative origin.
public struct CanonicalEventAttributeValue: Hashable, Sendable {
    public let key: EventAttributeKey
    public let value: EventAttributeValue

    internal init(key: EventAttributeKey, value: EventAttributeValue) {
        self.key = key
        self.value = value
    }
}
