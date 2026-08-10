/// A source and semantic location attached to a prepared-import consistency finding.
///
/// Every field is optional because a malformed embedded plan may fail before a complete semantic
/// location exists. Source positions remain provenance and are never substituted for semantic IDs.
public struct PreparedScientificImportValidationLocation: Hashable, Sendable {
    /// One-based position in the resolved plan's ordered group list.
    public let groupIndex: Int?
    public let groupID: ScientificEventScopeGroupID?
    public let spikeTrainID: ScientificSpikeTrainID?
    public let eventDefinitionID: ScientificEventDefinitionID?
    public let attributeKey: EventAttributeKey?
    public let sourceColumn: StagedSourceColumnReference?
    public let sourceCell: StagedSourceCellReference?

    internal init(
        groupIndex: Int? = nil,
        groupID: ScientificEventScopeGroupID? = nil,
        spikeTrainID: ScientificSpikeTrainID? = nil,
        eventDefinitionID: ScientificEventDefinitionID? = nil,
        attributeKey: EventAttributeKey? = nil,
        sourceColumn: StagedSourceColumnReference? = nil,
        sourceCell: StagedSourceCellReference? = nil
    ) {
        self.groupIndex = groupIndex
        self.groupID = groupID
        self.spikeTrainID = spikeTrainID
        self.eventDefinitionID = eventDefinitionID
        self.attributeKey = attributeKey
        self.sourceColumn = sourceColumn
        self.sourceCell = sourceCell
    }

    internal static let dataset = PreparedScientificImportValidationLocation()
}

/// Hashable validator vocabulary corresponding to the exact timestamp codec's lexical failures.
public enum PreparedScientificImportTimestampSyntaxIssue: String, Hashable, Sendable {
    case missingMantissaDigits
    case missingExponentDigits
    case unexpectedCharacter
}

public enum PreparedScientificImportTimestampParseIssue: Hashable, Sendable {
    case empty
    case lexemeTooLong(maximumUTF8Bytes: Int)
    case invalidSyntax(
        utf8Offset: Int,
        issue: PreparedScientificImportTimestampSyntaxIssue
    )
    case notExactlyRepresentableInMicroseconds
    case outsideSignedMicrosecondRange
}

public enum PreparedScientificImportEventAttributeKeyIssue: Hashable, Sendable {
    case sourceTooLong(maximumUTF8Bytes: Int)
    case canonicalValueTooLong(maximumUTF8Bytes: Int)
    case empty
    case blank
    case containsControlCharacter
    case containsMetadataSeparator
    case reservedUnitSuffix
}

public enum PreparedScientificImportExactAttributeSyntaxIssue: String, Hashable, Sendable {
    case missingMantissaDigits
    case missingExponentDigits
    case unexpectedCharacter
}

public enum PreparedScientificImportExactAttributeParseIssue: Hashable, Sendable {
    case empty
    case lexemeTooLong(maximumUTF8Bytes: Int)
    case invalidSyntax(
        utf8Offset: Int,
        issue: PreparedScientificImportExactAttributeSyntaxIssue
    )
    case exponentOutOfRange
    case normalizedExponentOutOfRange
}

public enum PreparedScientificImportAttributeValueIssue: Hashable, Sendable {
    case explicitEmptyStringForbidden
    case stringSourceTooLong(maximumUTF8Bytes: Int)
    case stringCanonicalValueTooLong(maximumUTF8Bytes: Int)
    case exactNumber(PreparedScientificImportExactAttributeParseIssue)
    case booleanEmpty
    case booleanLexemeTooLong(maximumUTF8Bytes: Int)
    case invalidBoolean
}

public enum PreparedScientificImportUnitSymbolIssue: Hashable, Sendable {
    case sourceTooLong(maximumUTF8Bytes: Int)
    case canonicalValueTooLong(maximumUTF8Bytes: Int)
    case empty
    case blank
    case containsControlCharacter
}

/// A bounded path vocabulary for exact prepared-value comparison findings.
///
/// Collection positions are one-based, matching all source and plan positions in validation
/// diagnostics. They are diagnostic coordinates, not Swift array indices.
public enum PreparedScientificImportComparisonField: Hashable, Sendable {
    case dataActivityMode
    case dataScientificAttributeDefinitionCount
    case dataScientificAttributeDefinitionKey(position: Int)
    case dataScientificAttributeDefinitionScalarType(position: Int)
    case dataScientificAttributeDefinitionRole(position: Int)
    case dataScientificAttributeDefinitionUnit(position: Int)
    case dataScientificAttributeDefinitionEmptyStringPolicy(position: Int)
    case dataPresentationAttributeDefinitionCount
    case dataPresentationAttributeDefinitionKey(position: Int)
    case dataPresentationAttributeDefinitionScalarType(position: Int)
    case dataPresentationAttributeDefinitionRole(position: Int)
    case dataPresentationAttributeDefinitionUnit(position: Int)
    case dataPresentationAttributeDefinitionEmptyStringPolicy(position: Int)

    case dataGroupCount
    case dataGroupSemanticID(position: Int)
    case dataGroupTimeBasis
    case dataGroupEventOriginDefinitionID
    case dataGroupEventOriginTick
    case dataGroupEventOriginScientificAttributeCount
    case dataGroupEventOriginScientificAttributeKey(position: Int)
    case dataGroupEventOriginScientificAttributeValue(position: Int)
    case dataSpikeTrainCount
    case dataSpikeTrainSemanticID(position: Int)
    case dataSpikeTimestampCount
    case dataSpikeTimestamp(position: Int)
    case dataEventDefinitionCount
    case dataEventDefinitionSemanticID(position: Int)
    case dataEventTypeID
    case dataEventOccurrenceCount
    case dataEventOccurrenceTick(position: Int)
    case dataEventOccurrenceScientificAttributeCount(occurrence: Int)
    case dataEventOccurrenceScientificAttributeKey(occurrence: Int, position: Int)
    case dataEventOccurrenceScientificAttributeValue(occurrence: Int, position: Int)
    case dataEventOccurrencePresentationAttributeCount(occurrence: Int)
    case dataEventOccurrencePresentationAttributeKey(occurrence: Int, position: Int)
    case dataEventOccurrencePresentationAttributeValue(occurrence: Int, position: Int)

    case provenanceGroupCount
    case provenanceGroupSemanticID(position: Int)
    case provenanceGroupTimeBasis
    case provenanceGroupEventOriginCell
    case provenanceGroupSourceOriginTick
    case provenanceSpikeTrainCount
    case provenanceSpikeTrainSemanticID(position: Int)
    case provenanceSpikeSourceColumn
    case provenanceSpikeOrderDecision
    case provenanceSpikeDuplicateDecision
    case provenanceSpikeSourceOrderDescentCount
    case provenanceSpikeTimestampSourceCount
    case provenanceSpikeTimestampSourceMultiplicity(outputPosition: Int)
    case provenanceSpikeTimestampSourceCell(outputPosition: Int, sourcePosition: Int)
    case provenanceEventDefinitionCount
    case provenanceEventDefinitionSemanticID(position: Int)
    case provenanceEventSourceColumn
    case provenanceEventOrderDecision
    case provenanceEventSourceOrderDescentCount
    case provenanceEventOccurrenceCount
    case provenanceEventOccurrenceTimestampCell(position: Int)
    case provenanceEventAttributeCount(occurrence: Int)
    case provenanceEventAttributeKey(occurrence: Int, position: Int)
    case provenanceEventAttributeValueCell(occurrence: Int, position: Int)
    case provenanceEventAttributeInlineUnitCase(occurrence: Int, position: Int)
    case provenanceEventAttributeInlineUnitSymbol(occurrence: Int, position: Int)
    case provenanceEventAttributeInlineUnitSourceCell(occurrence: Int, position: Int)
}

/// A blocking inconsistency found while independently checking a prepared import.
public enum PreparedScientificImportValidationIssueKind: Hashable, Sendable {
    case sourceHasNoColumns
    case sourceDataRowCountIsNegative(actual: Int)
    case sourceColumnReferenceMismatch(expected: Int, actual: Int)
    case sourceColumnRowCountMismatch(expected: Int, actual: Int)
    case sourceHeaderPresenceMismatch

    case eventScopeGroupsAreEmpty
    /// `firstGroupIndex` is one-based.
    case duplicateGroupSemanticID(firstGroupIndex: Int)
    case groupHasNoSpikeTrains
    /// A `ScientificSpikeTrainID` recurs in the resolved plan. Identity is dataset-global, so this
    /// fires within or across groups; the issue location carries the duplicate group/column and
    /// `firstGroupIndex`/`firstColumn` carry the first occurrence. `firstGroupIndex` is one-based.
    case duplicateSpikeTrainSemanticID(
        firstGroupIndex: Int,
        firstColumn: StagedSourceColumnReference
    )
    /// A `ScientificSpikeTrainID` is repeated in the prepared data — within one EventScopeGroup or
    /// across groups. This is the independent `prepared.data` defense-in-depth pass, which rejects
    /// any repeated global ID. `firstGroupIndex` is one-based.
    case duplicateGlobalSpikeTrainSemanticID(firstGroupIndex: Int)
    case duplicateEventDefinitionSemanticID(firstColumn: StagedSourceColumnReference)
    case duplicateCollapseNotAllowed(activityMode: ScientificDatasetActivityMode)

    case sourcePartitionCountMismatch(expected: Int, actual: Int)
    case sourcePartitionOrderMismatch(position: Int, expected: Int, actual: Int)
    case eventRelativeOriginOutsideSource
    case eventRelativeOriginNotInGroupEventDefinition

    case headerlessSourceRequiresSingleGroup(actual: Int)
    case headerlessSourceContainsEventDefinition
    case headerlessSourceRequiresRecordingElapsed

    /// `firstDefinitionIndex` is one-based.
    case duplicateEventAttributeDefinitionKey(firstDefinitionIndex: Int)
    case explicitEmptyStringAllowedForNonString(scalarType: EventAttributeScalarType)

    case spreadsheetNumberOutsideWorkbook
    case spreadsheetNumberTimestampDecodeFailed(
        issue: SpreadsheetNumericTimestampDecodeError
    )
    case timestampParseFailed(issue: PreparedScientificImportTimestampParseIssue)
    case eventMetadataInSpikeTrain
    case eventMetadataWithoutOccurrence
    case eventMetadataMissingEquals
    case invalidEventAttributeKey(issue: PreparedScientificImportEventAttributeKeyIssue)
    case unknownEventAttributeKey
    case duplicateEventAttributeInOccurrence(
        eventTimestampCell: StagedSourceCellReference,
        firstCell: StagedSourceCellReference
    )
    case eventAttributeValueInvalid(
        expectedType: EventAttributeScalarType,
        issue: PreparedScientificImportAttributeValueIssue
    )
    case duplicateInlineUnitSuggestion(
        eventTimestampCell: StagedSourceCellReference,
        firstCell: StagedSourceCellReference
    )
    case inlineUnitWithoutAttribute(eventTimestampCell: StagedSourceCellReference)
    case inlineUnitSuggestionInvalid(issue: PreparedScientificImportUnitSymbolIssue)
    case inlineUnitConflictsWithDefinition(
        suggested: OpaqueUnitSymbol,
        confirmed: ConfirmedEventAttributeUnit
    )
    case inlineUnitSuggestionsConflict(
        firstCell: StagedSourceCellReference,
        firstUnit: OpaqueUnitSymbol,
        conflictingUnit: OpaqueUnitSymbol
    )

    case selectedOriginIsNotEventTimestamp
    case selectedOriginAmbiguous(
        eventDefinitionID: ScientificEventDefinitionID,
        multiplicity: Int
    )
    case recordingElapsedTimestampIsNegative
    case eventRelativeRebaseOverflow(originCell: StagedSourceCellReference)
    case sourceOrderNotAscending(
        descentCount: Int,
        firstPreviousCell: StagedSourceCellReference,
        firstCurrentCell: StagedSourceCellReference
    )

    case preparedComparisonCountMismatch(
        field: PreparedScientificImportComparisonField,
        expected: Int,
        actual: Int
    )
    case preparedComparisonValueMismatch(field: PreparedScientificImportComparisonField)

    /// A defensive failure of the independent replay implementation, not a source-data decision.
    case independentReplayInvariant
}

public struct PreparedScientificImportValidationIssue: Hashable, Sendable {
    public let kind: PreparedScientificImportValidationIssueKind
    public let location: PreparedScientificImportValidationLocation

    internal init(
        kind: PreparedScientificImportValidationIssueKind,
        location: PreparedScientificImportValidationLocation
    ) {
        self.kind = kind
        self.location = location
    }
}

/// The warning vocabulary is intentionally separate from blocking issues. At present the only
/// approved warning is an empty confirmed event definition, emitted after complete validation.
public enum PreparedScientificImportValidationWarningKind: Hashable, Sendable {
    case emptyEventDefinition
}

public struct PreparedScientificImportValidationWarning: Hashable, Sendable {
    public let kind: PreparedScientificImportValidationWarningKind
    public let location: PreparedScientificImportValidationLocation

    internal init(
        kind: PreparedScientificImportValidationWarningKind,
        location: PreparedScientificImportValidationLocation
    ) {
        self.kind = kind
        self.location = location
    }
}

/// A bounded self-consistency report for one `PreparedScientificImport`.
///
/// A report without blocking issues means only that the prepared value is exactly
/// reproducible from its embedded resolved plan and raw staged cells. It is not proof that the
/// embedded plan equals an earlier user-confirmed transaction, and it grants no scientific or
/// analysis authority.
public struct PreparedScientificImportValidationReport: Hashable, Sendable {
    public let blockingIssues: [PreparedScientificImportValidationIssue]
    public let additionalBlockingIssueCount: Int
    public let warnings: [PreparedScientificImportValidationWarning]
    public let additionalWarningCount: Int

    public var hasBlockingIssues: Bool {
        !blockingIssues.isEmpty || additionalBlockingIssueCount > 0
    }

    internal init(
        blockingIssues: [PreparedScientificImportValidationIssue],
        additionalBlockingIssueCount: Int,
        warnings: [PreparedScientificImportValidationWarning],
        additionalWarningCount: Int
    ) {
        self.blockingIssues = blockingIssues
        self.additionalBlockingIssueCount = additionalBlockingIssueCount
        self.warnings = warnings
        self.additionalWarningCount = additionalWarningCount
    }
}
