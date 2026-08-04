public struct ScientificEventScopeGroupID: Hashable, Sendable {
    public let semanticID: ScientificSemanticID

    public init(_ semanticID: ScientificSemanticID) {
        self.semanticID = semanticID
    }
}

public struct ScientificSpikeTrainID: Hashable, Sendable {
    public let semanticID: ScientificSemanticID

    public init(_ semanticID: ScientificSemanticID) {
        self.semanticID = semanticID
    }
}

public struct ScientificEventDefinitionID: Hashable, Sendable {
    public let semanticID: ScientificSemanticID

    public init(_ semanticID: ScientificSemanticID) {
        self.semanticID = semanticID
    }
}

public struct ScientificEventTypeID: Hashable, Sendable {
    public let semanticID: ScientificSemanticID

    public init(_ semanticID: ScientificSemanticID) {
        self.semanticID = semanticID
    }
}

/// One explicitly confirmed interpretation applies to the entire import batch.
public enum ScientificDatasetActivityMode: String, CaseIterable, Hashable, Sendable {
    case putativeSingleUnit = "putative_single_unit"
    case intentionalMultiUnit = "intentional_multi_unit"
    case unknownOrUncertain = "unknown_or_uncertain"
}

public enum TimestampOrderDecision: String, CaseIterable, Hashable, Sendable {
    case preserveSourceOrder = "preserve_source_order"
    case stableAscendingSort = "stable_ascending_sort"
}

public enum ExactDuplicateDecision: String, CaseIterable, Hashable, Sendable {
    case preserveMultiplicity = "preserve_multiplicity"
    case collapseExact = "collapse_exact"
}

/// The associated origin remains optional while the UI is collecting an event-relative decision.
/// A later validator must reject `.eventRelative(origin: nil)`.
public enum EventScopeTimeBasisDraft: Hashable, Sendable {
    case recordingElapsed
    case eventRelative(origin: StagedEventOccurrenceReference?)
}

public enum EventAttributeRole: String, CaseIterable, Hashable, Sendable {
    case scientific
    case presentation
}

public enum EventEmptyStringPolicy: String, CaseIterable, Hashable, Sendable {
    case forbid
    case allowExplicitEmptyString = "allow_explicit_empty_string"
}

public struct SpikeTrainColumnManifestDraft: Hashable, Sendable {
    public let sourceColumn: StagedSourceColumnReference
    public var semanticID: ScientificSpikeTrainID?
    public var orderDecision: TimestampOrderDecision?
    public var duplicateDecision: ExactDuplicateDecision?

    public init(
        sourceColumn: StagedSourceColumnReference,
        semanticID: ScientificSpikeTrainID? = nil,
        orderDecision: TimestampOrderDecision? = nil,
        duplicateDecision: ExactDuplicateDecision? = nil
    ) {
        self.sourceColumn = sourceColumn
        self.semanticID = semanticID
        self.orderDecision = orderDecision
        self.duplicateDecision = duplicateDecision
    }
}

public struct EventDefinitionColumnManifestDraft: Hashable, Sendable {
    public let sourceColumn: StagedSourceColumnReference
    public var semanticID: ScientificEventDefinitionID?
    public var eventTypeID: ScientificEventTypeID?
    public var orderDecision: TimestampOrderDecision?

    public init(
        sourceColumn: StagedSourceColumnReference,
        semanticID: ScientificEventDefinitionID? = nil,
        eventTypeID: ScientificEventTypeID? = nil,
        orderDecision: TimestampOrderDecision? = nil
    ) {
        self.sourceColumn = sourceColumn
        self.semanticID = semanticID
        self.eventTypeID = eventTypeID
        self.orderDecision = orderDecision
    }
}

/// The two arrays encode the approved `S+ E*` grammar directly: one or more spike-train columns
/// followed by zero or more event-definition columns. Completeness and source contiguity are checked
/// by the later independent validator.
public struct EventScopeGroupManifestDraft: Hashable, Sendable {
    public var semanticID: ScientificEventScopeGroupID?
    public var spikeTrains: [SpikeTrainColumnManifestDraft]
    public var eventDefinitions: [EventDefinitionColumnManifestDraft]
    public var timeBasis: EventScopeTimeBasisDraft?

    public init(
        semanticID: ScientificEventScopeGroupID? = nil,
        spikeTrains: [SpikeTrainColumnManifestDraft],
        eventDefinitions: [EventDefinitionColumnManifestDraft],
        timeBasis: EventScopeTimeBasisDraft? = nil
    ) {
        self.semanticID = semanticID
        self.spikeTrains = spikeTrains
        self.eventDefinitions = eventDefinitions
        self.timeBasis = timeBasis
    }
}

/// Dataset-wide attribute decisions. Every Optional is an unresolved user decision, never a
/// fallback inferred from an observed value or inline unit suggestion.
public struct EventAttributeDefinitionDraft: Hashable, Sendable {
    public let key: EventAttributeKey
    public var scalarType: EventAttributeScalarType?
    public var role: EventAttributeRole?
    public var unit: ConfirmedEventAttributeUnit?
    public var emptyStringPolicy: EventEmptyStringPolicy?

    public init(
        key: EventAttributeKey,
        scalarType: EventAttributeScalarType? = nil,
        role: EventAttributeRole? = nil,
        unit: ConfirmedEventAttributeUnit? = nil,
        emptyStringPolicy: EventEmptyStringPolicy? = nil
    ) {
        self.key = key
        self.scalarType = scalarType
        self.role = role
        self.unit = unit
        self.emptyStringPolicy = emptyStringPolicy
    }
}

/// Exact source facts that bind a manifest draft to the table the user actually reviewed.
///
/// This is a transaction-safety value, not a scientific identity. It deliberately preserves the
/// selected source, ordered headers, raw cells, blanks, multiplicity, and row count while excluding
/// all non-authoritative staging suggestions. Its only public construction path is a staged import.
public struct ScientificImportDraftSourceBinding: Hashable, Sendable {
    public let source: StagedTabularSource
    public let sourceTransactionBinding: StagedSourceTransactionBinding?
    public let columns: [StagedScientificColumn]
    public let dataRowCount: Int

    public init(stagedImport: StagedScientificImport) {
        self.source = stagedImport.source
        self.sourceTransactionBinding = stagedImport.sourceTransactionBinding
        self.columns = stagedImport.columns
        self.dataRowCount = stagedImport.dataRowCount
    }
}

/// User decisions under construction. This type deliberately provides no default scientific
/// choice, readiness shortcut, confirmation conversion, digest, persistence, or activation API.
public struct ScientificImportManifestDraft: Hashable, Sendable {
    /// Immutable review-transaction binding. A different file, worksheet, header, column order, or
    /// raw cell value requires a new draft; suggestion-only changes do not.
    public let sourceBinding: ScientificImportDraftSourceBinding
    public var sourceTimeUnit: SpikeTimeUnit?
    public var activityMode: ScientificDatasetActivityMode?
    /// `nil` means grouping has not been decided. An empty array is a distinct, invalid proposal
    /// that the later validator can diagnose without silently replacing it.
    public var eventScopeGroups: [EventScopeGroupManifestDraft]?
    public var eventAttributeDefinitions: [EventAttributeDefinitionDraft]

    public init(
        boundTo stagedImport: StagedScientificImport,
        sourceTimeUnit: SpikeTimeUnit? = nil,
        activityMode: ScientificDatasetActivityMode? = nil,
        eventScopeGroups: [EventScopeGroupManifestDraft]? = nil,
        eventAttributeDefinitions: [EventAttributeDefinitionDraft] = []
    ) {
        self.sourceBinding = ScientificImportDraftSourceBinding(stagedImport: stagedImport)
        self.sourceTimeUnit = sourceTimeUnit
        self.activityMode = activityMode
        self.eventScopeGroups = eventScopeGroups
        self.eventAttributeDefinitions = eventAttributeDefinitions
    }
}
