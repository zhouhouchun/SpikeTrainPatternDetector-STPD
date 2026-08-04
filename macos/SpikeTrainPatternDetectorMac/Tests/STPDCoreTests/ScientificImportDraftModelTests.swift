import STPDCore
import Testing

@Test
func scientificImportDraftStartsWithoutScientificDefaults() {
    let draft = ScientificImportManifestDraft()

    #expect(draft.sourceTimeUnit == nil)
    #expect(draft.activityMode == nil)
    #expect(draft.eventScopeGroups == nil)
    #expect(draft.eventAttributeDefinitions.isEmpty)
    #expect(ScientificDatasetActivityMode.unknownOrUncertain != draft.activityMode)
}

@Test
func scientificImportStagingPreservesRawCellDistinctionsAndDuplicateHeaders() throws {
    let first = try StagedSourceColumnReference(oneBasedIndex: 1)
    let second = try StagedSourceColumnReference(oneBasedIndex: 2)
    let columns = [
        StagedScientificColumn(
            sourceColumn: first,
            header: "unit",
            cells: [.blank, .text(rawText: ""), .text(rawText: "1.000000")]
        ),
        StagedScientificColumn(
            sourceColumn: second,
            header: "unit",
            cells: [
                .spreadsheetNumber(rawLexeme: "1.0"),
                .text(rawText: "@intensity.unit=mW"),
                .text(rawText: "@intensity.unit=uW"),
            ]
        ),
    ]
    let staged = try StagedScientificImport(
        source: .excelWorkbook(worksheetName: "Recording"),
        columns: columns,
        suggestions: .none
    )

    #expect(staged.columns.map(\.header) == ["unit", "unit"])
    #expect(staged.dataRowCount == 3)
    #expect(
        staged.cell(
            at: try StagedSourceCellReference(
                column: first,
                oneBasedDataRowIndex: 1
            )
        ) == .blank
    )
    #expect(
        staged.cell(
            at: try StagedSourceCellReference(
                column: first,
                oneBasedDataRowIndex: 2
            )
        ) == .text(rawText: "")
    )
    #expect(staged.columns[1].cells[1] != staged.columns[1].cells[2])
}

@Test
func scientificImportStagingRejectsOnlyStructuralContradictions() throws {
    #expect(stagedImportError(columns: []) == .noColumns)

    let first = try StagedSourceColumnReference(oneBasedIndex: 1)
    let third = try StagedSourceColumnReference(oneBasedIndex: 3)
    #expect(
        stagedImportError(columns: [
            StagedScientificColumn(sourceColumn: first, header: "a", cells: [.blank]),
            StagedScientificColumn(sourceColumn: third, header: "b", cells: [.blank]),
        ]) == .nonSequentialColumnReference(expected: 2, actual: 3)
    )

    let second = try StagedSourceColumnReference(oneBasedIndex: 2)
    #expect(
        stagedImportError(columns: [
            StagedScientificColumn(sourceColumn: first, header: "a", cells: [.blank]),
            StagedScientificColumn(sourceColumn: second, header: "b", cells: []),
        ]) == .inconsistentDataRowCount(column: 2, expected: 1, actual: 0)
    )
    #expect(
        stagedImportError(columns: [
            StagedScientificColumn(sourceColumn: first, header: nil, cells: [.blank]),
            StagedScientificColumn(sourceColumn: second, header: "b", cells: [.blank]),
        ]) == .mixedHeaderPresence
    )
}

@Test
func scientificImportStagingValidatesSuggestionReferencesWithoutMakingThemAuthoritative() throws {
    let first = try StagedSourceColumnReference(oneBasedIndex: 1)
    let second = try StagedSourceColumnReference(oneBasedIndex: 2)
    let third = try StagedSourceColumnReference(oneBasedIndex: 3)
    let columns = [first, second, third].map {
        StagedScientificColumn(sourceColumn: $0, header: "column", cells: [.blank])
    }

    #expect(
        stagedImportError(
            columns: columns,
            suggestions: StagedScientificImportSuggestions(
                columns: [
                    StagedColumnSuggestion(
                        sourceColumn: try StagedSourceColumnReference(oneBasedIndex: 4),
                        kind: .spikeTrain,
                        suggestedSemanticIDText: nil,
                        suggestedEventTypeIDText: nil
                    ),
                ],
                eventScopeGroups: []
            )
        ) == .suggestionReferencesMissingColumn(column: 4)
    )
    #expect(
        stagedImportError(
            columns: columns,
            suggestions: StagedScientificImportSuggestions(
                columns: [],
                eventScopeGroups: [
                    StagedEventScopeGroupSuggestion(
                        sourceColumns: [],
                        suggestedSemanticIDText: nil
                    ),
                ]
            )
        ) == .emptyEventScopeGroupSuggestion(group: 1)
    )
    #expect(
        stagedImportError(
            columns: columns,
            suggestions: StagedScientificImportSuggestions(
                columns: [],
                eventScopeGroups: [
                    StagedEventScopeGroupSuggestion(
                        sourceColumns: [first, third],
                        suggestedSemanticIDText: nil
                    ),
                ]
            )
        ) == .noncontiguousEventScopeGroupSuggestion(group: 1, expected: 2, actual: 3)
    )
    #expect(
        stagedImportError(
            columns: columns,
            suggestions: StagedScientificImportSuggestions(
                columns: [],
                eventScopeGroups: [
                    StagedEventScopeGroupSuggestion(
                        sourceColumns: [first, second],
                        suggestedSemanticIDText: nil
                    ),
                    StagedEventScopeGroupSuggestion(
                        sourceColumns: [second, third],
                        suggestedSemanticIDText: nil
                    ),
                ]
            )
        ) == .duplicateEventScopeGroupSuggestionColumn(column: 2)
    )
    #expect(
        stagedImportError(
            columns: columns,
            suggestions: StagedScientificImportSuggestions(
                columns: [],
                eventScopeGroups: [
                    StagedEventScopeGroupSuggestion(
                        sourceColumns: [third],
                        suggestedSemanticIDText: nil
                    ),
                    StagedEventScopeGroupSuggestion(
                        sourceColumns: [first],
                        suggestedSemanticIDText: nil
                    ),
                ]
            )
        ) == .eventScopeGroupSuggestionsOutOfSourceOrder(previous: 3, actual: 1)
    )
}

@Test
func scientificImportSuggestionsDoNotPopulateManifestDecisions() throws {
    let column = try StagedSourceColumnReference(oneBasedIndex: 1)
    let suggestions = StagedScientificImportSuggestions(
        columns: [
            StagedColumnSuggestion(
                sourceColumn: column,
                kind: .eventDefinition,
                suggestedSemanticIDText: "stimulus",
                suggestedEventTypeIDText: "light"
            ),
        ],
        eventScopeGroups: [
            StagedEventScopeGroupSuggestion(
                sourceColumns: [column],
                suggestedSemanticIDText: "group_alpha"
            ),
        ]
    )
    let staged = try StagedScientificImport(
        source: .commaSeparatedValues,
        columns: [
            StagedScientificColumn(
                sourceColumn: column,
                header: "event_stimulus",
                cells: [.text(rawText: "@intensity.unit=mW")]
            ),
        ],
        suggestions: suggestions
    )
    let draft = ScientificImportManifestDraft()

    #expect(staged.suggestions.columns[0].suggestedSemanticIDText == "stimulus")
    #expect(staged.suggestions.eventScopeGroups.count == 1)
    #expect(draft.eventScopeGroups == nil)
    #expect(draft.eventAttributeDefinitions.isEmpty)
}

@Test
func scientificImportManifestRepresentsTwoIndependentSpikeEventGroups() throws {
    let references = try (1...7).map(StagedSourceColumnReference.init(oneBasedIndex:))
    let groupAlpha = EventScopeGroupManifestDraft(
        semanticID: try groupID("group_alpha"),
        spikeTrains: [
            SpikeTrainColumnManifestDraft(
                sourceColumn: references[0],
                semanticID: try trainID("unit_A")
            ),
            SpikeTrainColumnManifestDraft(
                sourceColumn: references[1],
                semanticID: try trainID("unit_B")
            ),
        ],
        eventDefinitions: [
            EventDefinitionColumnManifestDraft(
                sourceColumn: references[2],
                semanticID: try eventDefinitionID("event_stimulus"),
                eventTypeID: try eventTypeID("stimulus")
            ),
            EventDefinitionColumnManifestDraft(
                sourceColumn: references[3],
                semanticID: try eventDefinitionID("event_reward"),
                eventTypeID: try eventTypeID("reward")
            ),
        ],
        timeBasis: .recordingElapsed
    )
    let groupBeta = EventScopeGroupManifestDraft(
        semanticID: try groupID("group_beta"),
        spikeTrains: [
            SpikeTrainColumnManifestDraft(
                sourceColumn: references[4],
                semanticID: try trainID("unit_C")
            ),
            SpikeTrainColumnManifestDraft(
                sourceColumn: references[5],
                semanticID: try trainID("unit_D")
            ),
        ],
        eventDefinitions: [
            EventDefinitionColumnManifestDraft(
                sourceColumn: references[6],
                semanticID: try eventDefinitionID("event_light"),
                eventTypeID: try eventTypeID("light")
            ),
        ],
        timeBasis: .recordingElapsed
    )
    let draft = ScientificImportManifestDraft(
        sourceTimeUnit: .seconds,
        activityMode: .putativeSingleUnit,
        eventScopeGroups: [groupAlpha, groupBeta]
    )

    #expect(draft.activityMode == .putativeSingleUnit)
    #expect(draft.eventScopeGroups?.count == 2)
    #expect(
        draft.eventScopeGroups?[0].spikeTrains.map(\.sourceColumn)
            == Array(references[0...1])
    )
    #expect(
        draft.eventScopeGroups?[0].eventDefinitions.map(\.sourceColumn)
            == Array(references[2...3])
    )
    #expect(
        draft.eventScopeGroups?[1].spikeTrains.map(\.sourceColumn)
            == Array(references[4...5])
    )
    #expect(draft.eventScopeGroups?[1].eventDefinitions.map(\.sourceColumn) == [references[6]])
}

@Test
func scientificImportEventRelativeBasisCarriesOnlyAStagedOriginReference() throws {
    let eventColumn = try StagedSourceColumnReference(oneBasedIndex: 3)
    let timestampCell = try StagedSourceCellReference(
        column: eventColumn,
        oneBasedDataRowIndex: 8
    )
    let origin = StagedEventOccurrenceReference(timestampCell: timestampCell)
    let selected = EventScopeTimeBasisDraft.eventRelative(origin: origin)
    let incomplete = EventScopeTimeBasisDraft.eventRelative(origin: nil)

    guard case .eventRelative(let selectedOrigin) = selected else {
        Issue.record("Expected event-relative time basis")
        return
    }
    #expect(selectedOrigin == origin)
    #expect(selectedOrigin?.timestampCell == timestampCell)
    #expect(incomplete != .recordingElapsed)
}

@Test
func scientificImportAttributeDecisionsRemainIndependentAndExplicit() throws {
    let intensity = try EventAttributeKey(validating: "intensity")
    let condition = try EventAttributeKey(validating: "condition")
    let unresolved = EventAttributeDefinitionDraft(key: intensity)
    let explicit = EventAttributeDefinitionDraft(
        key: condition,
        scalarType: .string,
        role: .scientific,
        unit: .notApplicable,
        emptyStringPolicy: .allowExplicitEmptyString
    )

    #expect(unresolved.scalarType == nil)
    #expect(unresolved.role == nil)
    #expect(unresolved.unit == nil)
    #expect(unresolved.emptyStringPolicy == nil)
    #expect(explicit.scalarType == .string)
    #expect(explicit.role == .scientific)
    #expect(explicit.unit == .notApplicable)
    #expect(explicit.emptyStringPolicy == .allowExplicitEmptyString)

    let roles: Set<EventAttributeRole> = [.scientific, .presentation]
    let units: Set<ConfirmedEventAttributeUnit> = [
        .notApplicable,
        .dimensionless,
        .specified(try OpaqueUnitSymbol(validating: "mW")),
    ]
    let emptyPolicies: Set<EventEmptyStringPolicy> = [.forbid, .allowExplicitEmptyString]
    #expect(roles.count == 2)
    #expect(units.count == 3)
    #expect(emptyPolicies.count == 2)
}

@Test
func scientificImportModeChangesDoNotRewriteColumnDecisions() throws {
    let sourceColumn = try StagedSourceColumnReference(oneBasedIndex: 1)
    let column = SpikeTrainColumnManifestDraft(
        sourceColumn: sourceColumn,
        semanticID: try trainID("unit_A"),
        orderDecision: .stableAscendingSort,
        duplicateDecision: .collapseExact
    )
    let group = EventScopeGroupManifestDraft(
        spikeTrains: [column],
        eventDefinitions: [],
        timeBasis: .recordingElapsed
    )
    var draft = ScientificImportManifestDraft(
        sourceTimeUnit: .milliseconds,
        activityMode: .putativeSingleUnit,
        eventScopeGroups: [group]
    )

    draft.activityMode = .intentionalMultiUnit

    #expect(draft.eventScopeGroups?[0].spikeTrains[0].orderDecision == .stableAscendingSort)
    #expect(draft.eventScopeGroups?[0].spikeTrains[0].duplicateDecision == .collapseExact)
}

private func stagedImportError(
    columns: [StagedScientificColumn],
    suggestions: StagedScientificImportSuggestions = .none
) -> StagedScientificImportStructureError? {
    do {
        _ = try StagedScientificImport(
            source: .commaSeparatedValues,
            columns: columns,
            suggestions: suggestions
        )
        Issue.record("Expected staged-import construction to fail")
        return nil
    } catch let error as StagedScientificImportStructureError {
        return error
    } catch {
        Issue.record("Unexpected staged-import error type: \(error)")
        return nil
    }
}

private func groupID(_ source: String) throws -> ScientificEventScopeGroupID {
    ScientificEventScopeGroupID(try ScientificSemanticID(validating: source))
}

private func trainID(_ source: String) throws -> ScientificSpikeTrainID {
    ScientificSpikeTrainID(try ScientificSemanticID(validating: source))
}

private func eventDefinitionID(_ source: String) throws -> ScientificEventDefinitionID {
    ScientificEventDefinitionID(try ScientificSemanticID(validating: source))
}

private func eventTypeID(_ source: String) throws -> ScientificEventTypeID {
    ScientificEventTypeID(try ScientificSemanticID(validating: source))
}
