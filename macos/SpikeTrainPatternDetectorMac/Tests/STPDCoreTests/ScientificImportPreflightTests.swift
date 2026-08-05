import STPDCore
import Testing

@Test
func scientificImportPreflightNeverSuppliesMissingScientificDecisions() throws {
    let staged = try preflightStaged(
        spikeCells: [.text(rawText: "0")],
        eventCells: [.blank]
    )
    var draft = ScientificImportManifestDraft(boundTo: staged)

    var report = ScientificImportPreflight.inspect(stagedImport: staged, draft: draft)
    #expect(report.issues == [.missingSourceTimeUnit])
    #expect(report.columnFacts.isEmpty)
    #expect(report.discoveredEventAttributes.isEmpty)
    #expect(report.eventOriginCandidates.isEmpty)

    draft.sourceTimeUnit = .seconds
    report = ScientificImportPreflight.inspect(stagedImport: staged, draft: draft)
    #expect(report.issues == [.missingEventScopeGroups])

    let changedSource = try preflightStaged(
        spikeCells: [.text(rawText: "1")],
        eventCells: [.blank]
    )
    report = ScientificImportPreflight.inspect(
        stagedImport: changedSource,
        draft: draft
    )
    #expect(report.issues == [.sourceBindingMismatch])
}

@Test
func scientificImportPreflightDiscoversMetadataAndExactColumnFacts() throws {
    let blankTail = Array(repeating: StagedCellValue.blank, count: 8)
    let staged = try preflightStaged(
        spikeCells: [
            .text(rawText: "-1.000000"),
            .text(rawText: "2.000000"),
            .text(rawText: "1.000000"),
            .text(rawText: "1.000000"),
        ] + blankTail,
        eventCells: [
            .text(rawText: "1.000000"),
            .text(rawText: "@intensity=2.5"),
            .text(rawText: "@intensity.unit=mW"),
            .text(rawText: "@label=shown"),
            .text(rawText: "@intensity=3.0"),
            .text(rawText: "@orphan.unit=mV"),
            .text(rawText: "@intensity.unit=mW"),
            .blank,
            .text(rawText: "@outside=value"),
            .text(rawText: "2.000000"),
            .text(rawText: "@broken"),
            .text(rawText: "@.unit=mW"),
        ]
    )
    let intensity = try EventAttributeKey(validating: "intensity")
    let label = try EventAttributeKey(validating: "label")
    let orphan = try EventAttributeKey(validating: "orphan")
    let draft = try preflightDraft(
        staged: staged,
        attributeDefinitions: [
            EventAttributeDefinitionDraft(key: intensity, role: .scientific),
            EventAttributeDefinitionDraft(key: label, role: .presentation),
        ]
    )

    let report = ScientificImportPreflight.inspect(stagedImport: staged, draft: draft)
    let spikeRow1 = try preflightCell(column: 1, row: 1)
    let spikeRow2 = try preflightCell(column: 1, row: 2)
    let spikeRow3 = try preflightCell(column: 1, row: 3)
    let spikeRow4 = try preflightCell(column: 1, row: 4)
    let eventRow1 = try preflightCell(column: 2, row: 1)
    let eventRow2 = try preflightCell(column: 2, row: 2)
    let eventRow3 = try preflightCell(column: 2, row: 3)
    let eventRow5 = try preflightCell(column: 2, row: 5)
    let eventRow6 = try preflightCell(column: 2, row: 6)
    let eventRow7 = try preflightCell(column: 2, row: 7)
    let eventRow9 = try preflightCell(column: 2, row: 9)
    let eventRow10 = try preflightCell(column: 2, row: 10)
    let eventRow11 = try preflightCell(column: 2, row: 11)
    let eventRow12 = try preflightCell(column: 2, row: 12)

    let spikeFacts = try #require(report.columnFacts.first(where: {
        $0.role == .spikeTrain
    }))
    #expect(spikeFacts.validTimestampCount == 4)
    #expect(spikeFacts.negativeTimestampCount == 1)
    #expect(spikeFacts.firstNegativeTimestampCell == spikeRow1)
    #expect(spikeFacts.sourceOrderDescentCount == 1)
    #expect(spikeFacts.firstSourceOrderDescent?.previousCell == spikeRow2)
    #expect(spikeFacts.firstSourceOrderDescent?.currentCell == spikeRow3)
    #expect(spikeFacts.exactDuplicateTimestampCount == 1)
    #expect(spikeFacts.firstExactDuplicateTimestamp?.tick == MicrosecondTick(
        microseconds: 1_000_000
    ))
    #expect(spikeFacts.firstExactDuplicateTimestamp?.firstCell == spikeRow3)
    #expect(spikeFacts.firstExactDuplicateTimestamp?.duplicateCell == spikeRow4)

    #expect(report.discoveredEventAttributes.map(\.key) == [intensity, label, orphan])
    let intensityFacts = try #require(report.discoveredEventAttributes.first(where: {
        $0.key == intensity
    }))
    #expect(intensityFacts.valueSightings.map(\.rawValue) == ["2.5", "3.0"])
    #expect(intensityFacts.inlineUnitSuggestions.map(\.rawUnit) == ["mW", "mW"])
    let orphanFacts = try #require(report.discoveredEventAttributes.first(where: {
        $0.key == orphan
    }))
    #expect(orphanFacts.valueSightings.isEmpty)
    #expect(orphanFacts.inlineUnitSuggestions.map(\.rawUnit) == ["mV"])

    #expect(report.eventOriginCandidates.count == 2)
    #expect(report.eventOriginCandidates[0].timestampCell == eventRow1)
    #expect(report.eventOriginCandidates[0].scientificAttributes.map(\.key) == [intensity])
    #expect(report.eventOriginCandidates[0].scientificAttributes.map(\.rawValue) == ["2.5"])
    #expect(!report.eventOriginCandidates[0].scientificAttributes.contains(where: {
        $0.key == label
    }))
    #expect(report.eventOriginCandidates[1].timestampCell == eventRow10)

    #expect(report.issues.contains(.eventMetadataWithoutOccurrence(
        groupIndex: 1,
        cell: eventRow9
    )))
    #expect(report.issues.contains(.eventMetadataMissingEquals(
        groupIndex: 1,
        cell: eventRow11
    )))
    #expect(report.issues.contains(.invalidEventAttributeKey(
        groupIndex: 1,
        cell: eventRow12,
        error: .empty
    )))
    #expect(report.issues.contains(.duplicateEventAttributeKey(
        groupIndex: 1,
        eventTimestampCell: eventRow1,
        key: intensity,
        firstCell: eventRow2,
        duplicateCell: eventRow5
    )))
    #expect(report.issues.contains(.duplicateInlineUnitSuggestion(
        groupIndex: 1,
        eventTimestampCell: eventRow1,
        key: intensity,
        firstCell: eventRow3,
        duplicateCell: eventRow7
    )))
    #expect(report.issues.contains(.inlineUnitWithoutAttribute(
        groupIndex: 1,
        eventTimestampCell: eventRow1,
        key: orphan,
        unitCell: eventRow6
    )))
}

@Test
func scientificImportPreflightAndNormalizerShareLexicalEventGrammar() throws {
    let staged = try preflightStaged(
        spikeCells: Array(repeating: .blank, count: 4),
        eventCells: [
            .text(rawText: "@outside=value"),
            .text(rawText: "1.000000"),
            .text(rawText: "@broken"),
            .text(rawText: "@.unit=mW"),
        ]
    )
    let draft = try preflightCompleteDraft(staged: staged)
    let report = ScientificImportPreflight.inspect(stagedImport: staged, draft: draft)
    let plan = try ScientificImportPlanResolver.resolve(stagedImport: staged, draft: draft)

    let normalizationIssues: [ScientificImportNormalizationIssue]
    do {
        _ = try ScientificImportNormalizer.normalize(resolvedPlan: plan)
        Issue.record("Expected normalization to reject the shared lexical failures")
        normalizationIssues = []
    } catch let error as ScientificImportNormalizationError {
        normalizationIssues = error.issues
    }

    let outsideCell = try preflightCell(column: 2, row: 1)
    let brokenCell = try preflightCell(column: 2, row: 3)
    let invalidKeyCell = try preflightCell(column: 2, row: 4)
    #expect(Array(report.issues.prefix(3)) == [
        .eventMetadataWithoutOccurrence(groupIndex: 1, cell: outsideCell),
        .eventMetadataMissingEquals(groupIndex: 1, cell: brokenCell),
        .invalidEventAttributeKey(groupIndex: 1, cell: invalidKeyCell, error: .empty),
    ])
    #expect(normalizationIssues == [
        .eventMetadataWithoutOccurrence(group: 1, cell: outsideCell),
        .eventMetadataMissingEquals(group: 1, cell: brokenCell),
        .invalidEventAttributeKey(group: 1, cell: invalidKeyCell, error: .empty),
    ])
}

private func preflightStaged(
    spikeCells: [StagedCellValue],
    eventCells: [StagedCellValue]
) throws -> StagedScientificImport {
    #expect(spikeCells.count == eventCells.count)
    return try StagedScientificImport(
        source: .commaSeparatedValues,
        columns: [
            StagedScientificColumn(
                sourceColumn: try StagedSourceColumnReference(oneBasedIndex: 1),
                header: "unit_A",
                cells: spikeCells
            ),
            StagedScientificColumn(
                sourceColumn: try StagedSourceColumnReference(oneBasedIndex: 2),
                header: "event_stimulus",
                cells: eventCells
            ),
        ],
        suggestions: .none
    )
}

private func preflightDraft(
    staged: StagedScientificImport,
    attributeDefinitions: [EventAttributeDefinitionDraft] = []
) throws -> ScientificImportManifestDraft {
    let spikeColumn = try StagedSourceColumnReference(oneBasedIndex: 1)
    let eventColumn = try StagedSourceColumnReference(oneBasedIndex: 2)
    return ScientificImportManifestDraft(
        boundTo: staged,
        sourceTimeUnit: .seconds,
        eventScopeGroups: [
            EventScopeGroupManifestDraft(
                spikeTrains: [SpikeTrainColumnManifestDraft(sourceColumn: spikeColumn)],
                eventDefinitions: [
                    EventDefinitionColumnManifestDraft(sourceColumn: eventColumn),
                ]
            ),
        ],
        eventAttributeDefinitions: attributeDefinitions
    )
}

private func preflightCompleteDraft(
    staged: StagedScientificImport
) throws -> ScientificImportManifestDraft {
    let spikeColumn = try StagedSourceColumnReference(oneBasedIndex: 1)
    let eventColumn = try StagedSourceColumnReference(oneBasedIndex: 2)
    let groupID = ScientificEventScopeGroupID(
        try ScientificSemanticID(validating: "group")
    )
    let spikeID = ScientificSpikeTrainID(
        try ScientificSemanticID(validating: "unit_A")
    )
    let eventID = ScientificEventDefinitionID(
        try ScientificSemanticID(validating: "event_stimulus")
    )
    let eventTypeID = ScientificEventTypeID(
        try ScientificSemanticID(validating: "stimulus")
    )
    return ScientificImportManifestDraft(
        boundTo: staged,
        sourceTimeUnit: .seconds,
        activityMode: .putativeSingleUnit,
        eventScopeGroups: [
            EventScopeGroupManifestDraft(
                semanticID: groupID,
                spikeTrains: [
                    SpikeTrainColumnManifestDraft(
                        sourceColumn: spikeColumn,
                        semanticID: spikeID,
                        orderDecision: .preserveSourceOrder,
                        duplicateDecision: .preserveMultiplicity
                    ),
                ],
                eventDefinitions: [
                    EventDefinitionColumnManifestDraft(
                        sourceColumn: eventColumn,
                        semanticID: eventID,
                        eventTypeID: eventTypeID,
                        orderDecision: .preserveSourceOrder
                    ),
                ],
                timeBasis: .recordingElapsed
            ),
        ]
    )
}

private func preflightCell(
    column: Int,
    row: Int
) throws -> StagedSourceCellReference {
    try StagedSourceCellReference(
        column: StagedSourceColumnReference(oneBasedIndex: column),
        oneBasedDataRowIndex: row
    )
}
