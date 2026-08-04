import STPDCore
import Testing

@Test
func scientificImportPlanResolverBuildsBoundSingleTrainPlan() throws {
    let staged = try makeStagedImport(columnCount: 1)
    let draft = try makeSingleSpikeDraft(boundTo: staged)

    let plan = try ScientificImportPlanResolver.resolve(stagedImport: staged, draft: draft)

    #expect(plan.source.source == staged.source)
    #expect(plan.source.columns == staged.columns)
    #expect(plan.source.dataRowCount == staged.dataRowCount)
    #expect(plan.sourceTimeUnit == .seconds)
    #expect(plan.activityMode == .putativeSingleUnit)
    #expect(plan.eventScopeGroups.count == 1)
    #expect(plan.eventScopeGroups[0].spikeTrains[0].sourceColumn.oneBasedIndex == 1)
    #expect(plan.eventAttributeDefinitions.isEmpty)
}

@Test
func scientificImportPlanResolverBindsRawSourceButExcludesSuggestions() throws {
    let column = try StagedSourceColumnReference(oneBasedIndex: 1)
    let columns = [
        StagedScientificColumn(
            sourceColumn: column,
            header: "unit",
            cells: [.blank, .text(rawText: ""), .text(rawText: "1.000000")]
        ),
    ]
    let firstStaging = try StagedScientificImport(
        source: .commaSeparatedValues,
        columns: columns,
        suggestions: .none
    )
    let secondStaging = try StagedScientificImport(
        source: .commaSeparatedValues,
        columns: columns,
        suggestions: StagedScientificImportSuggestions(
            columns: [
                StagedColumnSuggestion(
                    sourceColumn: column,
                    kind: .eventDefinition,
                    suggestedSemanticIDText: "different",
                    suggestedEventTypeIDText: "suggestion"
                ),
            ],
            eventScopeGroups: [
                StagedEventScopeGroupSuggestion(
                    sourceColumns: [column],
                    suggestedSemanticIDText: "suggested_group"
                ),
            ]
        )
    )
    let draft = try makeSingleSpikeDraft(boundTo: firstStaging)

    let firstPlan = try ScientificImportPlanResolver.resolve(
        stagedImport: firstStaging,
        draft: draft
    )
    let secondPlan = try ScientificImportPlanResolver.resolve(
        stagedImport: secondStaging,
        draft: draft
    )

    #expect(firstPlan == secondPlan)
    #expect(firstPlan.source.columns[0].cells == columns[0].cells)
}

@Test
func scientificImportPlanResolverRejectsDraftBoundToDifferentSourceFacts() throws {
    let base = try makeCustomStagedImport(
        source: .excelWorkbook(worksheetName: "Recording"),
        headers: ["unit_A", "unit_B"],
        rawCellTexts: ["1.000000", "2.000000"]
    )
    let references = try sourceColumns(2)
    let group = EventScopeGroupManifestDraft(
        semanticID: try resolvedGroupID("group"),
        spikeTrains: [
            try completeSpike(references[0], idText: "unit_A"),
            try completeSpike(references[1], idText: "unit_B"),
        ],
        eventDefinitions: [],
        timeBasis: .recordingElapsed
    )
    let draft = completeRoot(stagedImport: base, groups: [group])
    let basePlan = try ScientificImportPlanResolver.resolve(stagedImport: base, draft: draft)

    let mismatches = [
        try makeCustomStagedImport(
            source: .excelWorkbook(worksheetName: "Recording"),
            headers: ["renamed_A", "unit_B"],
            rawCellTexts: ["1.000000", "2.000000"]
        ),
        try makeCustomStagedImport(
            source: .excelWorkbook(worksheetName: "Recording"),
            headers: ["unit_A", "unit_B"],
            rawCellTexts: ["1.000001", "2.000000"]
        ),
        try makeCustomStagedImport(
            source: .excelWorkbook(worksheetName: "OtherSheet"),
            headers: ["unit_A", "unit_B"],
            rawCellTexts: ["1.000000", "2.000000"]
        ),
        try makeCustomStagedImport(
            source: .excelWorkbook(worksheetName: "Recording"),
            headers: ["unit_B", "unit_A"],
            rawCellTexts: ["2.000000", "1.000000"]
        ),
    ]
    for mismatched in mismatches {
        #expect(resolutionIssues(stagedImport: mismatched, draft: draft) == [
            .sourceBindingMismatch,
        ])
    }

    let suggestionOnlyChange = try makeCustomStagedImport(
        source: .excelWorkbook(worksheetName: "Recording"),
        headers: ["unit_A", "unit_B"],
        rawCellTexts: ["1.000000", "2.000000"],
        suggestions: StagedScientificImportSuggestions(
            columns: [
                StagedColumnSuggestion(
                    sourceColumn: references[0],
                    kind: .eventDefinition,
                    suggestedSemanticIDText: "wrong_but_non_authoritative",
                    suggestedEventTypeIDText: "suggestion"
                ),
            ],
            eventScopeGroups: []
        )
    )
    let suggestionPlan = try ScientificImportPlanResolver.resolve(
        stagedImport: suggestionOnlyChange,
        draft: draft
    )
    #expect(suggestionPlan == basePlan)
}

@Test
func scientificImportPlanResolverBindsExactSourceSnapshotAndWorksheetSelection() throws {
    func workbookBinding(
        digestCharacter: Character,
        sheetID: UInt32 = 7
    ) throws -> StagedSourceTransactionBinding {
        try StagedSourceTransactionBinding(
            sourceBytesSHA256: String(repeating: digestCharacter, count: 64),
            selection: .excelWorksheet(
                worksheetName: "Recording",
                sheetID: sheetID,
                relationshipID: "recordingSheet",
                normalizedPartPath: "pkg/sheets/recording.xml"
            )
        )
    }

    let base = try makeCustomStagedImport(
        source: .excelWorkbook(worksheetName: "Recording"),
        headers: ["unit"],
        rawCellTexts: ["0.001"],
        sourceTransactionBinding: workbookBinding(digestCharacter: "a")
    )
    let draft = try makeSingleSpikeDraft(boundTo: base)
    let plan = try ScientificImportPlanResolver.resolve(stagedImport: base, draft: draft)
    #expect(plan.source.sourceTransactionBinding == base.sourceTransactionBinding)

    let sameTableDifferentBytes = try makeCustomStagedImport(
        source: .excelWorkbook(worksheetName: "Recording"),
        headers: ["unit"],
        rawCellTexts: ["0.001"],
        sourceTransactionBinding: workbookBinding(digestCharacter: "b")
    )
    let sameBytesDifferentSheetSelection = try makeCustomStagedImport(
        source: .excelWorkbook(worksheetName: "Recording"),
        headers: ["unit"],
        rawCellTexts: ["0.001"],
        sourceTransactionBinding: workbookBinding(digestCharacter: "a", sheetID: 8)
    )
    let sameTableWithoutBinding = try makeCustomStagedImport(
        source: .excelWorkbook(worksheetName: "Recording"),
        headers: ["unit"],
        rawCellTexts: ["0.001"]
    )
    for mismatched in [
        sameTableDifferentBytes,
        sameBytesDifferentSheetSelection,
        sameTableWithoutBinding,
    ] {
        #expect(resolutionIssues(stagedImport: mismatched, draft: draft) == [
            .sourceBindingMismatch,
        ])
    }
}

@Test
func scientificImportPlanResolverAcceptsTwoOrderedGroupsAndLocalIDReuse() throws {
    let staged = try makeStagedImport(columnCount: 4)
    let references = try sourceColumns(4)
    let sharedTrainID = try resolvedTrainID("local_unit")
    let sharedDefinitionID = try resolvedEventDefinitionID("local_event")
    let sharedEventTypeID = try resolvedEventTypeID("stimulus")
    let groups = [
        EventScopeGroupManifestDraft(
            semanticID: try resolvedGroupID("group_alpha"),
            spikeTrains: [completeSpike(references[0], semanticID: sharedTrainID)],
            eventDefinitions: [
                completeEvent(
                    references[1],
                    semanticID: sharedDefinitionID,
                    eventTypeID: sharedEventTypeID
                ),
            ],
            timeBasis: .recordingElapsed
        ),
        EventScopeGroupManifestDraft(
            semanticID: try resolvedGroupID("group_beta"),
            spikeTrains: [completeSpike(references[2], semanticID: sharedTrainID)],
            eventDefinitions: [
                completeEvent(
                    references[3],
                    semanticID: sharedDefinitionID,
                    eventTypeID: sharedEventTypeID
                ),
            ],
            timeBasis: .recordingElapsed
        ),
    ]
    let draft = ScientificImportManifestDraft(
        boundTo: staged,
        sourceTimeUnit: .milliseconds,
        activityMode: .putativeSingleUnit,
        eventScopeGroups: groups
    )

    let plan = try ScientificImportPlanResolver.resolve(stagedImport: staged, draft: draft)

    #expect(plan.eventScopeGroups.count == 2)
    #expect(plan.eventScopeGroups.flatMap { group in
        group.spikeTrains.map(\.sourceColumn) + group.eventDefinitions.map(\.sourceColumn)
    } == references)
    #expect(plan.eventScopeGroups[0].spikeTrains[0].semanticID == sharedTrainID)
    #expect(plan.eventScopeGroups[1].spikeTrains[0].semanticID == sharedTrainID)
    #expect(plan.eventScopeGroups[0].eventDefinitions[0].eventTypeID == sharedEventTypeID)
    #expect(plan.eventScopeGroups[1].eventDefinitions[0].eventTypeID == sharedEventTypeID)
}

@Test
func scientificImportPlanResolverCollectsMissingDecisionsInStableOrder() throws {
    let staged = try makeStagedImport(columnCount: 1)
    let column = try StagedSourceColumnReference(oneBasedIndex: 1)
    let key = try EventAttributeKey(validating: "condition")
    let draft = ScientificImportManifestDraft(
        boundTo: staged,
        eventScopeGroups: [
            EventScopeGroupManifestDraft(
                spikeTrains: [SpikeTrainColumnManifestDraft(sourceColumn: column)],
                eventDefinitions: [],
                timeBasis: .eventRelative(origin: nil)
            ),
        ],
        eventAttributeDefinitions: [EventAttributeDefinitionDraft(key: key)]
    )
    let expected: [ScientificImportPlanIssue] = [
        .missingSourceTimeUnit,
        .missingActivityMode,
        .missingGroupSemanticID(group: 1),
        .missingEventRelativeOrigin(group: 1),
        .missingSpikeTrainSemanticID(group: 1, column: 1),
        .missingSpikeTrainOrderDecision(group: 1, column: 1),
        .missingSpikeTrainDuplicateDecision(group: 1, column: 1),
        .missingEventAttributeScalarType(definition: 1, key: key),
        .missingEventAttributeRole(definition: 1, key: key),
        .missingEventAttributeUnit(definition: 1, key: key),
        .missingEventAttributeEmptyStringPolicy(definition: 1, key: key),
    ]

    let first = resolutionIssues(stagedImport: staged, draft: draft)
    let second = resolutionIssues(stagedImport: staged, draft: draft)
    #expect(first == expected)
    #expect(second == expected)
}

@Test
func scientificImportPlanResolverSeparatesMissingAndEmptyGroupDecisions() throws {
    let staged = try makeStagedImport(columnCount: 1)
    let missing = ScientificImportManifestDraft(
        boundTo: staged,
        sourceTimeUnit: .seconds,
        activityMode: .putativeSingleUnit,
        eventScopeGroups: nil
    )
    let empty = ScientificImportManifestDraft(
        boundTo: staged,
        sourceTimeUnit: .seconds,
        activityMode: .putativeSingleUnit,
        eventScopeGroups: []
    )

    #expect(resolutionIssues(stagedImport: staged, draft: missing) == [
        .missingEventScopeGroups,
    ])
    #expect(resolutionIssues(stagedImport: staged, draft: empty) == [
        .emptyEventScopeGroups,
    ])
}

@Test
func scientificImportPlanResolverRejectsMissingGroupTimeBasisWithoutDefaulting() throws {
    let staged = try makeStagedImport(columnCount: 1)
    var group = try makeSingleSpikeGroup()
    group.timeBasis = nil

    #expect(resolutionIssues(
        stagedImport: staged,
        draft: completeRoot(stagedImport: staged, groups: [group])
    ) == [
        .missingGroupTimeBasis(group: 1),
    ])
}

@Test
func scientificImportPlanResolverRequiresEveryEventColumnDecision() throws {
    let staged = try makeStagedImport(columnCount: 2)
    let references = try sourceColumns(2)
    let group = EventScopeGroupManifestDraft(
        semanticID: try resolvedGroupID("group"),
        spikeTrains: [try completeSpike(references[0], idText: "unit")],
        eventDefinitions: [EventDefinitionColumnManifestDraft(sourceColumn: references[1])],
        timeBasis: .recordingElapsed
    )
    let draft = completeRoot(stagedImport: staged, groups: [group])

    #expect(resolutionIssues(stagedImport: staged, draft: draft) == [
        .missingEventDefinitionSemanticID(group: 1, column: 2),
        .missingEventTypeID(group: 1, column: 2),
        .missingEventOrderDecision(group: 1, column: 2),
    ])
}

@Test
func scientificImportPlanResolverReportsExactFlattenedPartitionFailures() throws {
    let staged = try makeStagedImport(columnCount: 3)
    let second = try StagedSourceColumnReference(oneBasedIndex: 2)
    let fourth = try StagedSourceColumnReference(oneBasedIndex: 4)
    let group = EventScopeGroupManifestDraft(
        semanticID: try resolvedGroupID("group"),
        spikeTrains: [
            try completeSpike(second, idText: "a"),
            try completeSpike(second, idText: "b"),
            try completeSpike(fourth, idText: "c"),
        ],
        eventDefinitions: [],
        timeBasis: .recordingElapsed
    )

    #expect(resolutionIssues(
        stagedImport: staged,
        draft: completeRoot(stagedImport: staged, groups: [group])
    ) == [
        .sourceColumnOutsideStaging(group: 1, column: 4),
        .flattenedSourcePartitionOrderMismatch(position: 1, expected: 1, actual: 2),
        .flattenedSourcePartitionOrderMismatch(position: 3, expected: 3, actual: 4),
        .sourceColumnNotAssigned(column: 1),
        .sourceColumnAssignedMultipleTimes(column: 2, count: 2),
        .sourceColumnNotAssigned(column: 3),
    ])
}

@Test
func scientificImportPlanResolverEnforcesHeaderlessStructureAndTimeBasis() throws {
    let staged = try makeStagedImport(columnCount: 2, header: nil)
    let references = try sourceColumns(2)
    let origin = try eventOrigin(column: references[1], row: 1)
    let group = EventScopeGroupManifestDraft(
        semanticID: try resolvedGroupID("group"),
        spikeTrains: [try completeSpike(references[0], idText: "unit")],
        eventDefinitions: [try completeEvent(references[1], idText: "event", typeText: "type")],
        timeBasis: .eventRelative(origin: origin)
    )

    #expect(resolutionIssues(
        stagedImport: staged,
        draft: completeRoot(stagedImport: staged, groups: [group])
    ) == [
        .headerlessSourceContainsEventDefinition(group: 1, column: 2),
        .headerlessSourceRequiresRecordingElapsed(group: 1),
    ])

    let twoGroups = [
        EventScopeGroupManifestDraft(
            semanticID: try resolvedGroupID("first"),
            spikeTrains: [try completeSpike(references[0], idText: "local")],
            eventDefinitions: [],
            timeBasis: .recordingElapsed
        ),
        EventScopeGroupManifestDraft(
            semanticID: try resolvedGroupID("second"),
            spikeTrains: [try completeSpike(references[1], idText: "local")],
            eventDefinitions: [],
            timeBasis: .recordingElapsed
        ),
    ]
    #expect(
        resolutionIssues(
            stagedImport: staged,
            draft: completeRoot(stagedImport: staged, groups: twoGroups)
        )
            == [.headerlessSourceRequiresSingleGroup(actualGroupCount: 2)]
    )
}

@Test
func scientificImportPlanResolverAcceptsHeaderlessSingleAllSpikeGroup() throws {
    let staged = try makeStagedImport(columnCount: 2, header: nil)
    let references = try sourceColumns(2)
    let group = EventScopeGroupManifestDraft(
        semanticID: try resolvedGroupID("group"),
        spikeTrains: [
            try completeSpike(references[0], idText: "unit_A"),
            try completeSpike(references[1], idText: "unit_B"),
        ],
        eventDefinitions: [],
        timeBasis: .recordingElapsed
    )

    let plan = try ScientificImportPlanResolver.resolve(
        stagedImport: staged,
        draft: completeRoot(stagedImport: staged, groups: [group])
    )
    #expect(plan.eventScopeGroups.count == 1)
    #expect(plan.eventScopeGroups[0].eventDefinitions.isEmpty)
    #expect(plan.eventScopeGroups[0].spikeTrains.map(\.sourceColumn) == references)
}

@Test
func scientificImportPlanResolverChecksEventRelativeOriginOnlyStructurally() throws {
    let staged = try makeStagedImport(columnCount: 2)
    let references = try sourceColumns(2)
    let validGroup = EventScopeGroupManifestDraft(
        semanticID: try resolvedGroupID("group"),
        spikeTrains: [try completeSpike(references[0], idText: "unit")],
        eventDefinitions: [try completeEvent(references[1], idText: "event", typeText: "type")],
        timeBasis: .eventRelative(origin: try eventOrigin(column: references[1], row: 1))
    )
    let validPlan = try ScientificImportPlanResolver.resolve(
        stagedImport: staged,
        draft: completeRoot(stagedImport: staged, groups: [validGroup])
    )
    guard case .eventRelative(let resolvedOrigin) = validPlan.eventScopeGroups[0].timeBasis else {
        Issue.record("Expected an event-relative plan")
        return
    }
    let expectedOrigin = try eventOrigin(column: references[1], row: 1)
    #expect(resolvedOrigin == expectedOrigin)

    var outside = validGroup
    let outsideOrigin = try eventOrigin(column: references[1], row: 2)
    outside.timeBasis = .eventRelative(origin: outsideOrigin)
    #expect(resolutionIssues(
        stagedImport: staged,
        draft: completeRoot(stagedImport: staged, groups: [outside])
    ) == [
        .eventRelativeOriginOutsideStaging(group: 1, cell: outsideOrigin.timestampCell),
    ])

    var wrongColumn = validGroup
    wrongColumn.timeBasis = .eventRelative(
        origin: try eventOrigin(column: references[0], row: 1)
    )
    #expect(resolutionIssues(
        stagedImport: staged,
        draft: completeRoot(stagedImport: staged, groups: [wrongColumn])
    ) == [
        .eventRelativeOriginNotInSameGroupEventDefinition(group: 1, column: 1),
    ])
}

@Test
func scientificImportPlanResolverRejectsAnOriginFromAnotherGroup() throws {
    let staged = try makeStagedImport(columnCount: 4)
    let references = try sourceColumns(4)
    let groups = [
        EventScopeGroupManifestDraft(
            semanticID: try resolvedGroupID("first"),
            spikeTrains: [try completeSpike(references[0], idText: "unit")],
            eventDefinitions: [
                try completeEvent(references[1], idText: "event", typeText: "stimulus"),
            ],
            timeBasis: .eventRelative(
                origin: try eventOrigin(column: references[3], row: 1)
            )
        ),
        EventScopeGroupManifestDraft(
            semanticID: try resolvedGroupID("second"),
            spikeTrains: [try completeSpike(references[2], idText: "unit")],
            eventDefinitions: [
                try completeEvent(references[3], idText: "event", typeText: "stimulus"),
            ],
            timeBasis: .recordingElapsed
        ),
    ]

    #expect(
        resolutionIssues(
            stagedImport: staged,
            draft: completeRoot(stagedImport: staged, groups: groups)
        ) == [
            .eventRelativeOriginNotInSameGroupEventDefinition(group: 1, column: 4),
        ]
    )
}

@Test
func scientificImportPlanResolverUsesGlobalGroupAndGroupLocalColumnIDUniqueness() throws {
    let staged = try makeStagedImport(columnCount: 6)
    let references = try sourceColumns(6)
    let repeatedGroupID = try resolvedGroupID("group")
    let repeatedTrainID = try resolvedTrainID("unit")
    let repeatedDefinitionID = try resolvedEventDefinitionID("event")
    let repeatedEventTypeID = try resolvedEventTypeID("type")
    let groups = [
        EventScopeGroupManifestDraft(
            semanticID: repeatedGroupID,
            spikeTrains: [
                completeSpike(references[0], semanticID: repeatedTrainID),
                completeSpike(references[1], semanticID: repeatedTrainID),
            ],
            eventDefinitions: [
                completeEvent(
                    references[2],
                    semanticID: repeatedDefinitionID,
                    eventTypeID: repeatedEventTypeID
                ),
                completeEvent(
                    references[3],
                    semanticID: repeatedDefinitionID,
                    eventTypeID: repeatedEventTypeID
                ),
            ],
            timeBasis: .recordingElapsed
        ),
        EventScopeGroupManifestDraft(
            semanticID: repeatedGroupID,
            spikeTrains: [completeSpike(references[4], semanticID: repeatedTrainID)],
            eventDefinitions: [
                completeEvent(
                    references[5],
                    semanticID: repeatedDefinitionID,
                    eventTypeID: repeatedEventTypeID
                ),
            ],
            timeBasis: .recordingElapsed
        ),
    ]

    #expect(resolutionIssues(
        stagedImport: staged,
        draft: completeRoot(stagedImport: staged, groups: groups)
    ) == [
        .duplicateSpikeTrainSemanticID(
            group: 1,
            semanticID: repeatedTrainID,
            firstColumn: 1,
            duplicateColumn: 2
        ),
        .duplicateEventDefinitionSemanticID(
            group: 1,
            semanticID: repeatedDefinitionID,
            firstColumn: 3,
            duplicateColumn: 4
        ),
        .duplicateGroupSemanticID(
            semanticID: repeatedGroupID,
            firstGroup: 1,
            duplicateGroup: 2
        ),
    ])
}

@Test
func scientificImportPlanResolverRejectsCollapseForMultiUnitAndUnknown() throws {
    let staged = try makeStagedImport(columnCount: 1)
    let reference = try StagedSourceColumnReference(oneBasedIndex: 1)
    let group = EventScopeGroupManifestDraft(
        semanticID: try resolvedGroupID("group"),
        spikeTrains: [
            SpikeTrainColumnManifestDraft(
                sourceColumn: reference,
                semanticID: try resolvedTrainID("unit"),
                orderDecision: .preserveSourceOrder,
                duplicateDecision: .collapseExact
            ),
        ],
        eventDefinitions: [],
        timeBasis: .recordingElapsed
    )

    for mode in [
        ScientificDatasetActivityMode.intentionalMultiUnit,
        .unknownOrUncertain,
    ] {
        #expect(resolutionIssues(
            stagedImport: staged,
            draft: completeRoot(stagedImport: staged, mode: mode, groups: [group])
        ) == [
            .duplicateCollapseNotAllowed(activityMode: mode, group: 1, column: 1),
        ])
    }

    _ = try ScientificImportPlanResolver.resolve(
        stagedImport: staged,
        draft: completeRoot(
            stagedImport: staged,
            mode: .putativeSingleUnit,
            groups: [group]
        )
    )
}

@Test
func scientificImportPlanResolverAcceptsPreservedMultiplicityForEveryActivityMode() throws {
    let staged = try makeStagedImport(columnCount: 1)
    let group = try makeSingleSpikeGroup()

    for mode in ScientificDatasetActivityMode.allCases {
        let plan = try ScientificImportPlanResolver.resolve(
            stagedImport: staged,
            draft: completeRoot(stagedImport: staged, mode: mode, groups: [group])
        )
        #expect(plan.activityMode == mode)
        #expect(plan.eventScopeGroups[0].spikeTrains[0].duplicateDecision
            == .preserveMultiplicity)
    }
}

@Test
func scientificImportPlanResolverReportsEmptySpikeGroupAndIncompletePartition() throws {
    let staged = try makeStagedImport(columnCount: 3)
    let references = try sourceColumns(3)
    let group = EventScopeGroupManifestDraft(
        semanticID: try resolvedGroupID("group"),
        spikeTrains: [],
        eventDefinitions: [
            try completeEvent(references[0], idText: "event_A", typeText: "stimulus"),
            try completeEvent(references[1], idText: "event_B", typeText: "stimulus"),
        ],
        timeBasis: .recordingElapsed
    )

    #expect(resolutionIssues(
        stagedImport: staged,
        draft: completeRoot(stagedImport: staged, groups: [group])
    ) == [
        .groupHasNoSpikeTrains(group: 1),
        .flattenedSourcePartitionCountMismatch(expected: 3, actual: 2),
        .sourceColumnNotAssigned(column: 3),
    ])
}

@Test
func scientificImportPlanResolverRequiresAndPreservesAllAttributeDecisions() throws {
    let staged = try makeStagedImport(columnCount: 1)
    let rootGroup = try makeSingleSpikeGroup()
    let stringKey = try EventAttributeKey(validating: "label")
    let integerKey = try EventAttributeKey(validating: "count")
    let decimalKey = try EventAttributeKey(validating: "intensity")
    let booleanKey = try EventAttributeKey(validating: "success")
    let powerUnit = try OpaqueUnitSymbol(validating: "mW")
    let definitions = [
        EventAttributeDefinitionDraft(
            key: stringKey,
            scalarType: .string,
            role: .scientific,
            unit: .notApplicable,
            emptyStringPolicy: .allowExplicitEmptyString
        ),
        EventAttributeDefinitionDraft(
            key: integerKey,
            scalarType: .integer,
            role: .presentation,
            unit: .dimensionless,
            emptyStringPolicy: .forbid
        ),
        EventAttributeDefinitionDraft(
            key: decimalKey,
            scalarType: .exactDecimal,
            role: .scientific,
            unit: .specified(powerUnit),
            emptyStringPolicy: .forbid
        ),
        EventAttributeDefinitionDraft(
            key: booleanKey,
            scalarType: .boolean,
            role: .presentation,
            unit: .notApplicable,
            emptyStringPolicy: .forbid
        ),
    ]
    let draft = ScientificImportManifestDraft(
        boundTo: staged,
        sourceTimeUnit: .seconds,
        activityMode: .putativeSingleUnit,
        eventScopeGroups: [rootGroup],
        eventAttributeDefinitions: definitions
    )

    let plan = try ScientificImportPlanResolver.resolve(stagedImport: staged, draft: draft)
    #expect(plan.eventAttributeDefinitions.map(\.key) == [
        stringKey,
        integerKey,
        decimalKey,
        booleanKey,
    ])
    #expect(plan.eventAttributeDefinitions.map(\.scalarType)
        == [.string, .integer, .exactDecimal, .boolean])
    #expect(plan.eventAttributeDefinitions.map(\.role)
        == [.scientific, .presentation, .scientific, .presentation])
    #expect(plan.eventAttributeDefinitions.map(\.unit) == [
        .notApplicable,
        .dimensionless,
        .specified(powerUnit),
        .notApplicable,
    ])
    #expect(plan.eventAttributeDefinitions.map(\.emptyStringPolicy) == [
        .allowExplicitEmptyString,
        .forbid,
        .forbid,
        .forbid,
    ])
}

@Test
func scientificImportPlanResolverRejectsDuplicateAttributeKeysAndNonStringEmptyPolicy() throws {
    let staged = try makeStagedImport(columnCount: 1)
    let key = try EventAttributeKey(validating: "count")
    let definitions = [
        EventAttributeDefinitionDraft(
            key: key,
            scalarType: .integer,
            role: .scientific,
            unit: .dimensionless,
            emptyStringPolicy: .allowExplicitEmptyString
        ),
        EventAttributeDefinitionDraft(
            key: key,
            scalarType: .exactDecimal,
            role: .presentation,
            unit: .notApplicable,
            emptyStringPolicy: .allowExplicitEmptyString
        ),
    ]
    let draft = ScientificImportManifestDraft(
        boundTo: staged,
        sourceTimeUnit: .seconds,
        activityMode: .putativeSingleUnit,
        eventScopeGroups: [try makeSingleSpikeGroup()],
        eventAttributeDefinitions: definitions
    )

    #expect(resolutionIssues(stagedImport: staged, draft: draft) == [
        .explicitEmptyStringAllowedForNonString(definition: 1, key: key, scalarType: .integer),
        .duplicateEventAttributeKey(key: key, firstDefinition: 1, duplicateDefinition: 2),
        .explicitEmptyStringAllowedForNonString(
            definition: 2,
            key: key,
            scalarType: .exactDecimal
        ),
    ])
}

private func makeStagedImport(
    columnCount: Int,
    header: String? = "column"
) throws -> StagedScientificImport {
    let columns = try sourceColumns(columnCount).map { reference in
        StagedScientificColumn(
            sourceColumn: reference,
            header: header,
            cells: [.text(rawText: "source text")]
        )
    }
    return try StagedScientificImport(
        source: .commaSeparatedValues,
        columns: columns,
        suggestions: .none
    )
}

private func makeCustomStagedImport(
    source: StagedTabularSource,
    headers: [String?],
    rawCellTexts: [String],
    suggestions: StagedScientificImportSuggestions = .none,
    sourceTransactionBinding: StagedSourceTransactionBinding? = nil
) throws -> StagedScientificImport {
    precondition(headers.count == rawCellTexts.count)
    let columns = try headers.indices.map { offset in
        StagedScientificColumn(
            sourceColumn: try StagedSourceColumnReference(oneBasedIndex: offset + 1),
            header: headers[offset],
            cells: [.text(rawText: rawCellTexts[offset])]
        )
    }
    if let sourceTransactionBinding {
        return try StagedScientificImport(
            source: source,
            columns: columns,
            suggestions: suggestions,
            sourceTransactionBinding: sourceTransactionBinding
        )
    }
    return try StagedScientificImport(
        source: source,
        columns: columns,
        suggestions: suggestions
    )
}

private func sourceColumns(_ count: Int) throws -> [StagedSourceColumnReference] {
    try (1...count).map(StagedSourceColumnReference.init(oneBasedIndex:))
}

private func completeRoot(
    stagedImport: StagedScientificImport,
    mode: ScientificDatasetActivityMode = .putativeSingleUnit,
    groups: [EventScopeGroupManifestDraft]
) -> ScientificImportManifestDraft {
    ScientificImportManifestDraft(
        boundTo: stagedImport,
        sourceTimeUnit: .seconds,
        activityMode: mode,
        eventScopeGroups: groups
    )
}

private func makeSingleSpikeDraft(
    boundTo stagedImport: StagedScientificImport
) throws -> ScientificImportManifestDraft {
    completeRoot(stagedImport: stagedImport, groups: [try makeSingleSpikeGroup()])
}

private func makeSingleSpikeGroup() throws -> EventScopeGroupManifestDraft {
    let column = try StagedSourceColumnReference(oneBasedIndex: 1)
    return EventScopeGroupManifestDraft(
        semanticID: try resolvedGroupID("group"),
        spikeTrains: [try completeSpike(column, idText: "unit")],
        eventDefinitions: [],
        timeBasis: .recordingElapsed
    )
}

private func completeSpike(
    _ sourceColumn: StagedSourceColumnReference,
    idText: String
) throws -> SpikeTrainColumnManifestDraft {
    SpikeTrainColumnManifestDraft(
        sourceColumn: sourceColumn,
        semanticID: try resolvedTrainID(idText),
        orderDecision: .preserveSourceOrder,
        duplicateDecision: .preserveMultiplicity
    )
}

private func completeSpike(
    _ sourceColumn: StagedSourceColumnReference,
    semanticID: ScientificSpikeTrainID
) -> SpikeTrainColumnManifestDraft {
    SpikeTrainColumnManifestDraft(
        sourceColumn: sourceColumn,
        semanticID: semanticID,
        orderDecision: .preserveSourceOrder,
        duplicateDecision: .preserveMultiplicity
    )
}

private func completeEvent(
    _ sourceColumn: StagedSourceColumnReference,
    idText: String,
    typeText: String
) throws -> EventDefinitionColumnManifestDraft {
    EventDefinitionColumnManifestDraft(
        sourceColumn: sourceColumn,
        semanticID: try resolvedEventDefinitionID(idText),
        eventTypeID: try resolvedEventTypeID(typeText),
        orderDecision: .preserveSourceOrder
    )
}

private func completeEvent(
    _ sourceColumn: StagedSourceColumnReference,
    semanticID: ScientificEventDefinitionID,
    eventTypeID: ScientificEventTypeID
) -> EventDefinitionColumnManifestDraft {
    EventDefinitionColumnManifestDraft(
        sourceColumn: sourceColumn,
        semanticID: semanticID,
        eventTypeID: eventTypeID,
        orderDecision: .preserveSourceOrder
    )
}

private func eventOrigin(
    column: StagedSourceColumnReference,
    row: Int
) throws -> StagedEventOccurrenceReference {
    StagedEventOccurrenceReference(
        timestampCell: try StagedSourceCellReference(
            column: column,
            oneBasedDataRowIndex: row
        )
    )
}

private func resolvedGroupID(_ text: String) throws -> ScientificEventScopeGroupID {
    ScientificEventScopeGroupID(try ScientificSemanticID(validating: text))
}

private func resolvedTrainID(_ text: String) throws -> ScientificSpikeTrainID {
    ScientificSpikeTrainID(try ScientificSemanticID(validating: text))
}

private func resolvedEventDefinitionID(_ text: String) throws -> ScientificEventDefinitionID {
    ScientificEventDefinitionID(try ScientificSemanticID(validating: text))
}

private func resolvedEventTypeID(_ text: String) throws -> ScientificEventTypeID {
    ScientificEventTypeID(try ScientificSemanticID(validating: text))
}

private func resolutionIssues(
    stagedImport: StagedScientificImport,
    draft: ScientificImportManifestDraft
) -> [ScientificImportPlanIssue] {
    do {
        _ = try ScientificImportPlanResolver.resolve(stagedImport: stagedImport, draft: draft)
        Issue.record("Expected scientific import plan resolution to fail")
        return []
    } catch let error as ScientificImportPlanResolutionError {
        return error.issues
    } catch {
        Issue.record("Unexpected scientific import plan resolution error: \(error)")
        return []
    }
}
