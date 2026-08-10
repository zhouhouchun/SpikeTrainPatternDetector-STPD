/// A format-neutral, pre-analysis scientific dataset projected from an independently validated
/// import. This model deliberately contains no source locations, presentation attributes, detector
/// settings, result authority, or inferred RecordingSegment/Trial structure.
///
/// Spike-train identity is dataset-global: every `CanonicalSpikeTrain` is defined once in the
/// ordered `spikeTrains` registry, and each `CanonicalEventScopeGroup` refers to its members by
/// `ScientificSpikeTrainID`. `EventScopeGroup` is a confirmed Unit/Event applicability relationship,
/// not a Unit namespace, so a spike-train identity is unique across the whole dataset rather than
/// only within one group.
public struct CanonicalScientificDataset: Hashable, Sendable {
    public let activityMode: ScientificDatasetActivityMode
    /// The dataset-global spike-train registry, ordered by canonical semantic ID. Each entry is a
    /// distinct global spike-train entity; `ScientificSpikeTrainID` is unique across this array.
    public let spikeTrains: [CanonicalSpikeTrain]
    public let eventScopeGroups: [CanonicalEventScopeGroup]
    public let scientificAttributeDefinitions: [CanonicalEventAttributeDefinition]

    internal init(
        activityMode: ScientificDatasetActivityMode,
        spikeTrains: [CanonicalSpikeTrain],
        eventScopeGroups: [CanonicalEventScopeGroup],
        scientificAttributeDefinitions: [CanonicalEventAttributeDefinition]
    ) {
        self.activityMode = activityMode
        self.spikeTrains = spikeTrains
        self.eventScopeGroups = eventScopeGroups
        self.scientificAttributeDefinitions = scientificAttributeDefinitions
    }
}

public struct CanonicalEventScopeGroup: Hashable, Sendable {
    public let semanticID: ScientificEventScopeGroupID
    public let timeBasis: CanonicalEventScopeTimeBasis
    /// Sorted references into the dataset-global `CanonicalScientificDataset.spikeTrains` registry.
    /// A group never redefines a spike train; it only names the global identities that apply to it.
    public let spikeTrainReferences: [ScientificSpikeTrainID]
    public let eventDefinitions: [CanonicalEventDefinition]

    internal init(
        semanticID: ScientificEventScopeGroupID,
        timeBasis: CanonicalEventScopeTimeBasis,
        spikeTrainReferences: [ScientificSpikeTrainID],
        eventDefinitions: [CanonicalEventDefinition]
    ) {
        self.semanticID = semanticID
        self.timeBasis = timeBasis
        self.spikeTrainReferences = spikeTrainReferences
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
    /// The dataset-global identity of this spike train. The name `ScientificSpikeTrainID` is kept
    /// deliberately: intentional multi-unit data may represent an aggregate spike train rather than
    /// a biological single neuron. The identity is unique across the whole canonical dataset.
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
