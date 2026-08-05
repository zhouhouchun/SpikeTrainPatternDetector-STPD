/// The explicitly proposed role of one source column during read-only import preflight.
public enum ScientificImportPreflightColumnRole: String, Hashable, Sendable {
    case spikeTrain = "spike_train"
    case eventDefinition = "event_definition"
}

public struct ScientificImportPreflightSourceOrderDescent: Hashable, Sendable {
    public let previousCell: StagedSourceCellReference
    public let currentCell: StagedSourceCellReference

    internal init(
        previousCell: StagedSourceCellReference,
        currentCell: StagedSourceCellReference
    ) {
        self.previousCell = previousCell
        self.currentCell = currentCell
    }
}

public struct ScientificImportPreflightExactDuplicate: Hashable, Sendable {
    public let tick: MicrosecondTick
    public let firstCell: StagedSourceCellReference
    public let duplicateCell: StagedSourceCellReference

    internal init(
        tick: MicrosecondTick,
        firstCell: StagedSourceCellReference,
        duplicateCell: StagedSourceCellReference
    ) {
        self.tick = tick
        self.firstCell = firstCell
        self.duplicateCell = duplicateCell
    }
}

/// Exact, source-located timestamp facts for one explicitly classified column.
///
/// `exactDuplicateTimestampCount` is the number of observations beyond the first observation at
/// each tick. For event columns it is descriptive only; preflight never collapses occurrences.
public struct ScientificImportPreflightColumnFacts: Hashable, Sendable {
    public let groupIndex: Int
    public let sourceColumn: StagedSourceColumnReference
    public let role: ScientificImportPreflightColumnRole
    public let validTimestampCount: Int
    public let negativeTimestampCount: Int
    public let firstNegativeTimestampCell: StagedSourceCellReference?
    public let sourceOrderDescentCount: Int
    public let firstSourceOrderDescent: ScientificImportPreflightSourceOrderDescent?
    public let exactDuplicateTimestampCount: Int
    public let firstExactDuplicateTimestamp: ScientificImportPreflightExactDuplicate?

    internal init(
        groupIndex: Int,
        sourceColumn: StagedSourceColumnReference,
        role: ScientificImportPreflightColumnRole,
        validTimestampCount: Int,
        negativeTimestampCount: Int,
        firstNegativeTimestampCell: StagedSourceCellReference?,
        sourceOrderDescentCount: Int,
        firstSourceOrderDescent: ScientificImportPreflightSourceOrderDescent?,
        exactDuplicateTimestampCount: Int,
        firstExactDuplicateTimestamp: ScientificImportPreflightExactDuplicate?
    ) {
        self.groupIndex = groupIndex
        self.sourceColumn = sourceColumn
        self.role = role
        self.validTimestampCount = validTimestampCount
        self.negativeTimestampCount = negativeTimestampCount
        self.firstNegativeTimestampCell = firstNegativeTimestampCell
        self.sourceOrderDescentCount = sourceOrderDescentCount
        self.firstSourceOrderDescent = firstSourceOrderDescent
        self.exactDuplicateTimestampCount = exactDuplicateTimestampCount
        self.firstExactDuplicateTimestamp = firstExactDuplicateTimestamp
    }
}

public struct ScientificImportEventAttributeValueSighting: Hashable, Sendable {
    public let groupIndex: Int
    public let eventTimestampCell: StagedSourceCellReference
    public let valueCell: StagedSourceCellReference
    public let rawValue: String

    internal init(
        groupIndex: Int,
        eventTimestampCell: StagedSourceCellReference,
        valueCell: StagedSourceCellReference,
        rawValue: String
    ) {
        self.groupIndex = groupIndex
        self.eventTimestampCell = eventTimestampCell
        self.valueCell = valueCell
        self.rawValue = rawValue
    }
}

/// An inline `@key.unit=value` source declaration. It is a suggestion/provenance fact only.
public struct ScientificImportInlineUnitSuggestionFact: Hashable, Sendable {
    public let groupIndex: Int
    public let eventTimestampCell: StagedSourceCellReference
    public let unitCell: StagedSourceCellReference
    public let rawUnit: String

    internal init(
        groupIndex: Int,
        eventTimestampCell: StagedSourceCellReference,
        unitCell: StagedSourceCellReference,
        rawUnit: String
    ) {
        self.groupIndex = groupIndex
        self.eventTimestampCell = eventTimestampCell
        self.unitCell = unitCell
        self.rawUnit = rawUnit
    }
}

/// One canonical attribute key discovered from event metadata.
///
/// `.unit` is never represented as a separate key. A key observed only through an inline-unit
/// declaration remains present with an empty `valueSightings` array so the UI can show the orphan.
public struct ScientificImportDiscoveredEventAttribute: Hashable, Sendable {
    public let key: EventAttributeKey
    public let valueSightings: [ScientificImportEventAttributeValueSighting]
    public let inlineUnitSuggestions: [ScientificImportInlineUnitSuggestionFact]

    internal init(
        key: EventAttributeKey,
        valueSightings: [ScientificImportEventAttributeValueSighting],
        inlineUnitSuggestions: [ScientificImportInlineUnitSuggestionFact]
    ) {
        self.key = key
        self.valueSightings = valueSightings
        self.inlineUnitSuggestions = inlineUnitSuggestions
    }
}

/// Raw provenance for an attribute whose draft role is explicitly `scientific`.
///
/// The raw value is not typed or canonicalized here. Presentation and unresolved roles are omitted.
public struct ScientificImportPreflightOriginScientificAttribute: Hashable, Sendable {
    public let key: EventAttributeKey
    public let rawValue: String
    public let sourceCell: StagedSourceCellReference

    internal init(
        key: EventAttributeKey,
        rawValue: String,
        sourceCell: StagedSourceCellReference
    ) {
        self.key = key
        self.rawValue = rawValue
        self.sourceCell = sourceCell
    }
}

/// A successfully decoded event timestamp that can be shown as an origin-selection candidate.
///
/// This is not proof that the candidate is semantically unique. Final typing and the normalizer's
/// scientific-attribute-only ambiguity check remain authoritative.
public struct ScientificImportEventOriginCandidate: Hashable, Sendable {
    public let groupIndex: Int
    public let sourceColumn: StagedSourceColumnReference
    public let eventDefinitionID: ScientificEventDefinitionID?
    public let timestampCell: StagedSourceCellReference
    public let sourceTick: MicrosecondTick
    public let scientificAttributes: [ScientificImportPreflightOriginScientificAttribute]

    internal init(
        groupIndex: Int,
        sourceColumn: StagedSourceColumnReference,
        eventDefinitionID: ScientificEventDefinitionID?,
        timestampCell: StagedSourceCellReference,
        sourceTick: MicrosecondTick,
        scientificAttributes: [ScientificImportPreflightOriginScientificAttribute]
    ) {
        self.groupIndex = groupIndex
        self.sourceColumn = sourceColumn
        self.eventDefinitionID = eventDefinitionID
        self.timestampCell = timestampCell
        self.sourceTick = sourceTick
        self.scientificAttributes = scientificAttributes
    }
}

/// Deterministic preflight diagnostics. These findings never grant or deny authority by themselves;
/// the resolver, normalizer, independent prepared validator, and later confirmation gate remain
/// mandatory.
public enum ScientificImportPreflightIssue: Equatable, Sendable {
    case sourceBindingMismatch
    case missingSourceTimeUnit
    case missingEventScopeGroups
    case sourceColumnOutsideStaging(groupIndex: Int, column: StagedSourceColumnReference)
    case sourceColumnAssignedMultipleTimes(
        column: StagedSourceColumnReference,
        firstGroupIndex: Int,
        duplicateGroupIndex: Int
    )

    case spreadsheetNumberOutsideWorkbook(
        groupIndex: Int,
        cell: StagedSourceCellReference
    )
    case spreadsheetNumberTimestampDecodeFailed(
        groupIndex: Int,
        cell: StagedSourceCellReference,
        error: SpreadsheetNumericTimestampDecodeError
    )
    case timestampParseFailed(
        groupIndex: Int,
        cell: StagedSourceCellReference,
        error: ExactTimestampParseError
    )
    case eventMetadataInSpikeTrain(
        groupIndex: Int,
        cell: StagedSourceCellReference
    )
    case eventMetadataWithoutOccurrence(
        groupIndex: Int,
        cell: StagedSourceCellReference
    )
    case eventMetadataMissingEquals(
        groupIndex: Int,
        cell: StagedSourceCellReference
    )
    case invalidEventAttributeKey(
        groupIndex: Int,
        cell: StagedSourceCellReference,
        error: EventAttributeKeyError
    )
    case duplicateEventAttributeKey(
        groupIndex: Int,
        eventTimestampCell: StagedSourceCellReference,
        key: EventAttributeKey,
        firstCell: StagedSourceCellReference,
        duplicateCell: StagedSourceCellReference
    )
    case duplicateInlineUnitSuggestion(
        groupIndex: Int,
        eventTimestampCell: StagedSourceCellReference,
        key: EventAttributeKey,
        firstCell: StagedSourceCellReference,
        duplicateCell: StagedSourceCellReference
    )
    case inlineUnitWithoutAttribute(
        groupIndex: Int,
        eventTimestampCell: StagedSourceCellReference,
        key: EventAttributeKey,
        unitCell: StagedSourceCellReference
    )
    case internalInvariant
}

/// Read-only source facts for the current explicit draft proposal.
public struct ScientificImportPreflightReport: Equatable, Sendable {
    public let columnFacts: [ScientificImportPreflightColumnFacts]
    public let discoveredEventAttributes: [ScientificImportDiscoveredEventAttribute]
    public let eventOriginCandidates: [ScientificImportEventOriginCandidate]
    public let issues: [ScientificImportPreflightIssue]
    /// Number of deterministic findings omitted after the public diagnostic cap.
    public let additionalIssueCount: Int

    public var hasIssues: Bool {
        !issues.isEmpty || additionalIssueCount > 0
    }

    internal init(
        columnFacts: [ScientificImportPreflightColumnFacts],
        discoveredEventAttributes: [ScientificImportDiscoveredEventAttribute],
        eventOriginCandidates: [ScientificImportEventOriginCandidate],
        issues: [ScientificImportPreflightIssue],
        additionalIssueCount: Int
    ) {
        self.columnFacts = columnFacts
        self.discoveredEventAttributes = discoveredEventAttributes
        self.eventOriginCandidates = eventOriginCandidates
        self.issues = issues
        self.additionalIssueCount = additionalIssueCount
    }
}
