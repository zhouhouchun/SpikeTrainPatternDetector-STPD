@testable import STPDCore
import Foundation
import Testing

@Test
func preparedScientificImportValidatorAcceptsNormalizerProducedValue() throws {
    let staged = try validatorStaged(
        headers: ["unit"],
        columns: [[.text(rawText: "0"), .text(rawText: "0.001")]]
    )
    let column = try validatorColumn(1)
    let group = ResolvedEventScopeGroupPlan(
        semanticID: try validatorGroupID("group"),
        spikeTrains: [
            ResolvedSpikeTrainColumnPlan(
                sourceColumn: column,
                semanticID: try validatorSpikeID("unit"),
                orderDecision: .preserveSourceOrder,
                duplicateDecision: .preserveMultiplicity
            ),
        ],
        eventDefinitions: [],
        timeBasis: .recordingElapsed
    )
    let plan = validatorPlan(staged: staged, groups: [group])
    let prepared = try ScientificImportNormalizer.normalize(resolvedPlan: plan)

    let report = PreparedScientificImportValidator.validate(prepared)

    #expect(report.blockingIssues.isEmpty)
    #expect(report.additionalBlockingIssueCount == 0)
    #expect(report.warnings.isEmpty)
    #expect(report.additionalWarningCount == 0)
    #expect(!report.hasBlockingIssues)
}

@Test
func preparedScientificImportValidatorAcceptsAllApprovedModeAndMultiplicityPairs() throws {
    let staged = try validatorStaged(
        headers: ["unit"],
        columns: [[.text(rawText: "1"), .text(rawText: "1")]]
    )
    let column = try validatorColumn(1)
    let cases: [(ScientificDatasetActivityMode, ExactDuplicateDecision)] = [
        (.putativeSingleUnit, .preserveMultiplicity),
        (.putativeSingleUnit, .collapseExact),
        (.intentionalMultiUnit, .preserveMultiplicity),
        (.unknownOrUncertain, .preserveMultiplicity),
    ]

    for (mode, duplicateDecision) in cases {
        let group = ResolvedEventScopeGroupPlan(
            semanticID: try validatorGroupID("group"),
            spikeTrains: [
                ResolvedSpikeTrainColumnPlan(
                    sourceColumn: column,
                    semanticID: try validatorSpikeID("unit"),
                    orderDecision: .preserveSourceOrder,
                    duplicateDecision: duplicateDecision
                ),
            ],
            eventDefinitions: [],
            timeBasis: .recordingElapsed
        )
        let plan = validatorPlan(staged: staged, mode: mode, groups: [group])
        let prepared = try ScientificImportNormalizer.normalize(resolvedPlan: plan)
        let report = PreparedScientificImportValidator.validate(prepared)
        #expect(report.blockingIssues.isEmpty)
        #expect(report.warnings.isEmpty)
    }
}

@Test
func preparedScientificImportValidatorTreatsEquivalentCSVAndXLSXTextAsSameData() throws {
    let headers = ["unit"]
    let columns = [[StagedCellValue.text(rawText: "0"), .text(rawText: "0.001")]]
    let csv = try validatorStaged(
        source: .commaSeparatedValues,
        headers: headers,
        columns: columns
    )
    let xlsx = try validatorStaged(
        source: .excelWorkbook(worksheetName: "Sheet1"),
        headers: headers,
        columns: columns
    )
    let csvPlan = try validatorSingleSpikePlan(staged: csv, attributes: [])
    let xlsxPlan = try validatorSingleSpikePlan(staged: xlsx, attributes: [])
    let csvPrepared = try ScientificImportNormalizer.normalize(resolvedPlan: csvPlan)
    let xlsxPrepared = try ScientificImportNormalizer.normalize(resolvedPlan: xlsxPlan)

    #expect(csvPrepared.data == xlsxPrepared.data)
    #expect(csvPrepared.provenance != xlsxPrepared.provenance)
    #expect(PreparedScientificImportValidator.validate(csvPrepared).blockingIssues.isEmpty)
    #expect(PreparedScientificImportValidator.validate(xlsxPrepared).blockingIssues.isEmpty)
}

@Test
func preparedScientificImportValidatorAcceptsMultiGroupEventRelativeImport() throws {
    let staged = try validatorStaged(
        headers: ["unit_Z", "event_Z", "unit_A", "event_A"],
        columns: [
            [.text(rawText: "0"), .text(rawText: "2")],
            [.text(rawText: "1"), .blank],
            [.text(rawText: "5"), .text(rawText: "6")],
            [.text(rawText: "5"), .blank],
        ]
    )
    let columns = try (1...4).map(validatorColumn)
    let groupZ = ResolvedEventScopeGroupPlan(
        semanticID: try validatorGroupID("group_Z"),
        spikeTrains: [
            ResolvedSpikeTrainColumnPlan(
                sourceColumn: columns[0],
                semanticID: try validatorSpikeID("unit_Z"),
                orderDecision: .preserveSourceOrder,
                duplicateDecision: .preserveMultiplicity
            ),
        ],
        eventDefinitions: [
            ResolvedEventDefinitionColumnPlan(
                sourceColumn: columns[1],
                semanticID: try validatorEventID("event_Z"),
                eventTypeID: try validatorEventTypeID("stimulus_Z"),
                orderDecision: .preserveSourceOrder
            ),
        ],
        timeBasis: .eventRelative(
            origin: StagedEventOccurrenceReference(
                timestampCell: try validatorCell(column: 2, row: 1)
            )
        )
    )
    let groupA = ResolvedEventScopeGroupPlan(
        semanticID: try validatorGroupID("group_A"),
        spikeTrains: [
            ResolvedSpikeTrainColumnPlan(
                sourceColumn: columns[2],
                semanticID: try validatorSpikeID("unit_A"),
                orderDecision: .preserveSourceOrder,
                duplicateDecision: .preserveMultiplicity
            ),
        ],
        eventDefinitions: [
            ResolvedEventDefinitionColumnPlan(
                sourceColumn: columns[3],
                semanticID: try validatorEventID("event_A"),
                eventTypeID: try validatorEventTypeID("stimulus_A"),
                orderDecision: .preserveSourceOrder
            ),
        ],
        timeBasis: .eventRelative(
            origin: StagedEventOccurrenceReference(
                timestampCell: try validatorCell(column: 4, row: 1)
            )
        )
    )
    let plan = validatorPlan(staged: staged, groups: [groupZ, groupA])
    let prepared = try ScientificImportNormalizer.normalize(resolvedPlan: plan)

    let report = PreparedScientificImportValidator.validate(prepared)

    #expect(report.blockingIssues.isEmpty)
    #expect(report.warnings.isEmpty)
    #expect(prepared.data.eventScopeGroups.map { $0.semanticID.semanticID.canonicalText }
        == ["group_A", "group_Z"])
}

@Test
func preparedScientificImportValidatorRejectsEmptyGroupListBeforeReplay() throws {
    let staged = try validatorStaged(
        headers: ["unit"],
        columns: [[.text(rawText: "0")]]
    )
    let plan = validatorPlan(staged: staged, groups: [])

    let report = PreparedScientificImportValidator.validate(
        validatorForgedPrepared(plan: plan)
    )

    #expect(report.blockingIssues.map(\.kind) == [
        .eventScopeGroupsAreEmpty,
        .sourcePartitionCountMismatch(expected: 1, actual: 0),
    ])
}

@Test
func preparedScientificImportValidatorChecksGroupLocalIDsAndExactOrderedPartition() throws {
    let staged = try validatorStaged(
        headers: ["unit_A", "unit_B", "event_A", "event_B"],
        columns: Array(repeating: [.text(rawText: "0")], count: 4)
    )
    let columns = try (1...4).map(validatorColumn)
    let groupID = try validatorGroupID("same_group")
    let spikeID = try validatorSpikeID("same_unit")
    let eventID = try validatorEventID("same_event")
    let groupOne = ResolvedEventScopeGroupPlan(
        semanticID: groupID,
        spikeTrains: [
            ResolvedSpikeTrainColumnPlan(
                sourceColumn: columns[1],
                semanticID: spikeID,
                orderDecision: .preserveSourceOrder,
                duplicateDecision: .preserveMultiplicity
            ),
            ResolvedSpikeTrainColumnPlan(
                sourceColumn: columns[0],
                semanticID: spikeID,
                orderDecision: .preserveSourceOrder,
                duplicateDecision: .preserveMultiplicity
            ),
        ],
        eventDefinitions: [
            ResolvedEventDefinitionColumnPlan(
                sourceColumn: columns[3],
                semanticID: eventID,
                eventTypeID: try validatorEventTypeID("stimulus"),
                orderDecision: .preserveSourceOrder
            ),
            ResolvedEventDefinitionColumnPlan(
                sourceColumn: columns[2],
                semanticID: eventID,
                eventTypeID: try validatorEventTypeID("reward"),
                orderDecision: .preserveSourceOrder
            ),
        ],
        timeBasis: .recordingElapsed
    )
    let groupTwo = ResolvedEventScopeGroupPlan(
        semanticID: groupID,
        spikeTrains: [],
        eventDefinitions: [],
        timeBasis: .recordingElapsed
    )
    let plan = validatorPlan(staged: staged, groups: [groupOne, groupTwo])

    let report = PreparedScientificImportValidator.validate(
        validatorForgedPrepared(plan: plan)
    )

    #expect(report.blockingIssues.map(\.kind) == [
        .duplicateSpikeTrainSemanticID(firstColumn: columns[1]),
        .duplicateEventDefinitionSemanticID(firstColumn: columns[3]),
        .duplicateGroupSemanticID(firstGroupIndex: 1),
        .groupHasNoSpikeTrains,
        .sourcePartitionOrderMismatch(position: 1, expected: 1, actual: 2),
        .sourcePartitionOrderMismatch(position: 2, expected: 2, actual: 1),
        .sourcePartitionOrderMismatch(position: 3, expected: 3, actual: 4),
        .sourcePartitionOrderMismatch(position: 4, expected: 4, actual: 3),
    ])
    #expect(report.blockingIssues[0].location.groupIndex == 1)
    #expect(report.blockingIssues[2].location.groupIndex == 2)
    #expect(report.blockingIssues[2].location.groupID == groupID)
}

@Test
func preparedScientificImportValidatorAllowsCollapseOnlyForPutativeSingleUnits() throws {
    let staged = try validatorStaged(
        headers: ["unit"],
        columns: [[.text(rawText: "0"), .text(rawText: "0")]]
    )
    let column = try validatorColumn(1)
    let group = ResolvedEventScopeGroupPlan(
        semanticID: try validatorGroupID("group"),
        spikeTrains: [
            ResolvedSpikeTrainColumnPlan(
                sourceColumn: column,
                semanticID: try validatorSpikeID("unit"),
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
        let plan = validatorPlan(staged: staged, mode: mode, groups: [group])
        let report = PreparedScientificImportValidator.validate(
            validatorForgedPrepared(plan: plan)
        )
        #expect(report.blockingIssues.map(\.kind) == [
            .duplicateCollapseNotAllowed(activityMode: mode),
        ])
    }

    let singleUnitPlan = validatorPlan(
        staged: staged,
        mode: .putativeSingleUnit,
        groups: [group]
    )
    let singleUnitReport = PreparedScientificImportValidator.validate(
        try ScientificImportNormalizer.normalize(resolvedPlan: singleUnitPlan)
    )
    #expect(singleUnitReport.blockingIssues.isEmpty)
}

@Test
func preparedScientificImportValidatorChecksEventRelativeOriginBoundsAndMembership() throws {
    let staged = try validatorStaged(
        headers: ["unit", "event_A", "event_B"],
        columns: Array(repeating: [.text(rawText: "0")], count: 3)
    )
    let columns = try (1...3).map(validatorColumn)
    let baseSpike = ResolvedSpikeTrainColumnPlan(
        sourceColumn: columns[0],
        semanticID: try validatorSpikeID("unit"),
        orderDecision: .preserveSourceOrder,
        duplicateDecision: .preserveMultiplicity
    )
    let events = [
        ResolvedEventDefinitionColumnPlan(
            sourceColumn: columns[1],
            semanticID: try validatorEventID("event_A"),
            eventTypeID: try validatorEventTypeID("stimulus"),
            orderDecision: .preserveSourceOrder
        ),
        ResolvedEventDefinitionColumnPlan(
            sourceColumn: columns[2],
            semanticID: try validatorEventID("event_B"),
            eventTypeID: try validatorEventTypeID("reward"),
            orderDecision: .preserveSourceOrder
        ),
    ]

    let outsideCell = try validatorCell(column: 2, row: 2)
    let outsideGroup = ResolvedEventScopeGroupPlan(
        semanticID: try validatorGroupID("group"),
        spikeTrains: [baseSpike],
        eventDefinitions: events,
        timeBasis: .eventRelative(
            origin: StagedEventOccurrenceReference(timestampCell: outsideCell)
        )
    )
    let outsideReport = PreparedScientificImportValidator.validate(
        validatorForgedPrepared(plan: validatorPlan(staged: staged, groups: [outsideGroup]))
    )
    #expect(outsideReport.blockingIssues.map(\.kind) == [
        .eventRelativeOriginOutsideSource,
    ])
    #expect(outsideReport.blockingIssues[0].location.sourceCell == outsideCell)

    let spikeCell = try validatorCell(column: 1, row: 1)
    let wrongColumnGroup = ResolvedEventScopeGroupPlan(
        semanticID: try validatorGroupID("group"),
        spikeTrains: [baseSpike],
        eventDefinitions: events,
        timeBasis: .eventRelative(
            origin: StagedEventOccurrenceReference(timestampCell: spikeCell)
        )
    )
    let wrongColumnReport = PreparedScientificImportValidator.validate(
        validatorForgedPrepared(plan: validatorPlan(staged: staged, groups: [wrongColumnGroup]))
    )
    #expect(wrongColumnReport.blockingIssues.map(\.kind) == [
        .eventRelativeOriginNotInGroupEventDefinition,
    ])
}

@Test
func preparedScientificImportValidatorEnforcesHeaderlessSingleAllSpikeElapsedShape() throws {
    let staged = try validatorStaged(
        headers: [nil, nil, nil, nil],
        columns: Array(repeating: [.text(rawText: "0")], count: 4)
    )
    let columns = try (1...4).map(validatorColumn)
    let eventA = ResolvedEventDefinitionColumnPlan(
        sourceColumn: columns[1],
        semanticID: try validatorEventID("event_A"),
        eventTypeID: try validatorEventTypeID("stimulus"),
        orderDecision: .preserveSourceOrder
    )
    let eventB = ResolvedEventDefinitionColumnPlan(
        sourceColumn: columns[3],
        semanticID: try validatorEventID("event_B"),
        eventTypeID: try validatorEventTypeID("reward"),
        orderDecision: .preserveSourceOrder
    )
    let groupA = ResolvedEventScopeGroupPlan(
        semanticID: try validatorGroupID("group_A"),
        spikeTrains: [
            ResolvedSpikeTrainColumnPlan(
                sourceColumn: columns[0],
                semanticID: try validatorSpikeID("unit_A"),
                orderDecision: .preserveSourceOrder,
                duplicateDecision: .preserveMultiplicity
            ),
        ],
        eventDefinitions: [eventA],
        timeBasis: .eventRelative(
            origin: StagedEventOccurrenceReference(
                timestampCell: try validatorCell(column: 2, row: 1)
            )
        )
    )
    let groupB = ResolvedEventScopeGroupPlan(
        semanticID: try validatorGroupID("group_B"),
        spikeTrains: [
            ResolvedSpikeTrainColumnPlan(
                sourceColumn: columns[2],
                semanticID: try validatorSpikeID("unit_B"),
                orderDecision: .preserveSourceOrder,
                duplicateDecision: .preserveMultiplicity
            ),
        ],
        eventDefinitions: [eventB],
        timeBasis: .recordingElapsed
    )
    let plan = validatorPlan(staged: staged, groups: [groupA, groupB])

    let report = PreparedScientificImportValidator.validate(
        validatorForgedPrepared(plan: plan)
    )

    #expect(report.blockingIssues.map(\.kind) == [
        .headerlessSourceRequiresSingleGroup(actual: 2),
        .headerlessSourceContainsEventDefinition,
        .headerlessSourceRequiresRecordingElapsed,
        .headerlessSourceContainsEventDefinition,
    ])
    #expect(report.blockingIssues.map(\.location.groupIndex) == [nil, 1, 1, 2])
}

@Test
func preparedScientificImportValidatorChecksAttributeUniquenessAndEmptyPolicy() throws {
    let staged = try validatorStaged(
        headers: ["unit"],
        columns: [[.text(rawText: "0")]]
    )
    let keyX = try EventAttributeKey(validating: "x")
    let keyY = try EventAttributeKey(validating: "y")
    let definitions = [
        ResolvedEventAttributeDefinitionPlan(
            key: keyX,
            scalarType: .integer,
            role: .scientific,
            unit: .dimensionless,
            emptyStringPolicy: .allowExplicitEmptyString
        ),
        ResolvedEventAttributeDefinitionPlan(
            key: keyX,
            scalarType: .string,
            role: .presentation,
            unit: .notApplicable,
            emptyStringPolicy: .forbid
        ),
        ResolvedEventAttributeDefinitionPlan(
            key: keyY,
            scalarType: .boolean,
            role: .scientific,
            unit: .notApplicable,
            emptyStringPolicy: .allowExplicitEmptyString
        ),
    ]
    let plan = try validatorSingleSpikePlan(staged: staged, attributes: definitions)

    let report = PreparedScientificImportValidator.validate(
        validatorForgedPrepared(plan: plan)
    )

    #expect(report.blockingIssues.map(\.kind) == [
        .explicitEmptyStringAllowedForNonString(scalarType: .integer),
        .duplicateEventAttributeDefinitionKey(firstDefinitionIndex: 1),
        .explicitEmptyStringAllowedForNonString(scalarType: .boolean),
    ])
    #expect(report.blockingIssues.map(\.location.attributeKey) == [keyX, keyX, keyY])
}

@Test
func preparedScientificImportValidatorCapsIssuesDeterministically() throws {
    let staged = try validatorStaged(
        headers: ["unit"],
        columns: [[.text(rawText: "0")]]
    )
    let repeatedKey = try EventAttributeKey(validating: "condition")
    let duplicateIssueCount = PreparedScientificImportValidator.maximumReportedIssueCount + 2
    let definitions = Array(
        repeating: ResolvedEventAttributeDefinitionPlan(
            key: repeatedKey,
            scalarType: .string,
            role: .scientific,
            unit: .notApplicable,
            emptyStringPolicy: .forbid
        ),
        count: duplicateIssueCount + 1
    )
    let plan = try validatorSingleSpikePlan(staged: staged, attributes: definitions)
    let prepared = validatorForgedPrepared(plan: plan)

    let first = PreparedScientificImportValidator.validate(prepared)
    let second = PreparedScientificImportValidator.validate(prepared)

    #expect(first == second)
    #expect(first.blockingIssues.count
        == PreparedScientificImportValidator.maximumReportedIssueCount)
    #expect(first.additionalBlockingIssueCount == 2)
    #expect(first.blockingIssues.first?.kind
        == .duplicateEventAttributeDefinitionKey(firstDefinitionIndex: 1))
    #expect(first.blockingIssues.last?.kind
        == .duplicateEventAttributeDefinitionKey(firstDefinitionIndex: 1))
    #expect(first.warnings.isEmpty)
    #expect(first.additionalWarningCount == 0)
}

@Test
func preparedScientificImportValidatorTreatsOnlyStructuralBlankAsMissing() throws {
    let staged = try validatorStaged(
        headers: ["unit"],
        columns: [[.blank, .text(rawText: ""), .text(rawText: " \t")]]
    )
    let plan = try validatorSingleSpikePlan(staged: staged, attributes: [])

    let report = PreparedScientificImportValidator.validate(
        validatorForgedPrepared(plan: plan)
    )

    #expect(report.blockingIssues.map(\.kind) == [
        .timestampParseFailed(issue: .empty),
        .timestampParseFailed(issue: .empty),
    ])
    #expect(report.blockingIssues.map { $0.location.sourceCell?.oneBasedDataRowIndex }
        == [2, 3])
}

@Test
func preparedScientificImportValidatorBlocksNumericOffGridAndOutOfRangeTimestamps() throws {
    let staged = try validatorStaged(
        headers: ["unit", "event"],
        columns: [
            [
                .spreadsheetNumber(rawLexeme: "0.000001"),
                .text(rawText: "0.0000005"),
                .text(rawText: "9223372036854.775808"),
            ],
            [.spreadsheetNumber(rawLexeme: "1"), .blank, .blank],
        ]
    )
    let plan = try validatorSpikeEventPlan(staged: staged, attributes: [])

    let report = PreparedScientificImportValidator.validate(
        validatorForgedPrepared(plan: plan)
    )

    #expect(report.blockingIssues.map(\.kind) == [
        .spreadsheetNumberTimestampRequiresPrecisionProof,
        .timestampParseFailed(issue: .notExactlyRepresentableInMicroseconds),
        .timestampParseFailed(issue: .outsideSignedMicrosecondRange),
        .spreadsheetNumberTimestampRequiresPrecisionProof,
    ])
}

@Test
func preparedScientificImportValidatorReplaysEventColumnStateMachine() throws {
    let staged = try validatorStaged(
        headers: ["unit", "event"],
        columns: [
            [.text(rawText: "@x=1")] + Array(repeating: .blank, count: 8),
            [
                .text(rawText: "@orphan=1"),
                .text(rawText: "0"),
                .text(rawText: "@missing_equals"),
                .text(rawText: "@=bad"),
                .blank,
                .text(rawText: "@orphan=2"),
                .text(rawText: "bad"),
                .text(rawText: "@orphan=3"),
                .text(rawText: "0.1"),
            ],
        ]
    )
    let plan = try validatorSpikeEventPlan(staged: staged, attributes: [])

    let report = PreparedScientificImportValidator.validate(
        validatorForgedPrepared(plan: plan)
    )

    #expect(report.blockingIssues.map(\.kind) == [
        .eventMetadataInSpikeTrain,
        .eventMetadataWithoutOccurrence,
        .eventMetadataMissingEquals,
        .invalidEventAttributeKey(issue: .empty),
        .eventMetadataWithoutOccurrence,
        .timestampParseFailed(
            issue: .invalidSyntax(utf8Offset: 0, issue: .unexpectedCharacter)
        ),
        .eventMetadataWithoutOccurrence,
    ])
    #expect(report.blockingIssues.map { $0.location.sourceCell?.oneBasedDataRowIndex }
        == [1, 1, 3, 4, 6, 7, 8])
}

@Test
func preparedScientificImportValidatorAcceptsAllExactAttributeTypesAndMetadataGrammar() throws {
    let decomposedCafe = "cafe\u{301}"
    let staged = try validatorStaged(
        headers: ["unit", "event"],
        columns: [
            Array(repeating: .blank, count: 8),
            [
                .text(rawText: "0"),
                .text(rawText: "@label="),
                .text(rawText: "@count=+00042"),
                .text(rawText: "@intensity=001.2300e+2"),
                .text(rawText: "@intensity.unit=mW"),
                .text(rawText: "@enabled=\ttrue "),
                .text(rawText: "@@condition=drug=high"),
                .text(rawText: "@\(decomposedCafe)=stim"),
            ],
        ]
    )
    let definitions = [
        try validatorAttribute(
            "label",
            type: .string,
            role: .presentation,
            unit: .notApplicable,
            empty: .allowExplicitEmptyString
        ),
        try validatorAttribute(
            "count",
            type: .integer,
            unit: .dimensionless
        ),
        try validatorAttribute(
            "intensity",
            type: .exactDecimal,
            unit: .specified(OpaqueUnitSymbol(validating: "mW"))
        ),
        try validatorAttribute(
            "enabled",
            type: .boolean,
            unit: .notApplicable
        ),
        try validatorAttribute(
            "@condition",
            type: .string,
            unit: .notApplicable
        ),
        try validatorAttribute(
            "caf\u{e9}",
            type: .string,
            role: .presentation,
            unit: .notApplicable
        ),
    ]
    let plan = try validatorSpikeEventPlan(staged: staged, attributes: definitions)
    let prepared = try ScientificImportNormalizer.normalize(resolvedPlan: plan)

    let report = PreparedScientificImportValidator.validate(prepared)

    #expect(report.blockingIssues.isEmpty)
}

@Test
func preparedScientificImportValidatorRejectsInvalidBooleanAndExplicitEmptyString() throws {
    let tooLongBoolean = String(repeating: "t", count: 129)
    let staged = try validatorStaged(
        headers: ["unit", "event"],
        columns: [
            Array(repeating: .blank, count: 7),
            [
                .text(rawText: "0"),
                .text(rawText: "@flag=TRUE"),
                .text(rawText: "@label="),
                .text(rawText: "1"),
                .text(rawText: "@flag= \t"),
                .text(rawText: "2"),
                .text(rawText: "@flag=\(tooLongBoolean)"),
            ],
        ]
    )
    let definitions = [
        try validatorAttribute("flag", type: .boolean, unit: .notApplicable),
        try validatorAttribute("label", type: .string, unit: .notApplicable),
    ]
    let plan = try validatorSpikeEventPlan(staged: staged, attributes: definitions)

    let report = PreparedScientificImportValidator.validate(
        validatorForgedPrepared(plan: plan)
    )

    #expect(report.blockingIssues.map(\.kind) == [
        .eventAttributeValueInvalid(expectedType: .boolean, issue: .invalidBoolean),
        .eventAttributeValueInvalid(
            expectedType: .string,
            issue: .explicitEmptyStringForbidden
        ),
        .eventAttributeValueInvalid(expectedType: .boolean, issue: .booleanEmpty),
        .eventAttributeValueInvalid(
            expectedType: .boolean,
            issue: .booleanLexemeTooLong(maximumUTF8Bytes: 128)
        ),
    ])
}

@Test
func preparedScientificImportValidatorChecksInlineUnitsGloballyAndDeterministically() throws {
    let staged = try validatorStaged(
        headers: ["unit", "event"],
        columns: [
            Array(repeating: .blank, count: 15),
            [
                .text(rawText: "0"),
                .text(rawText: "@intensity=1"),
                .text(rawText: "@intensity=2"),
                .text(rawText: "@intensity.unit=mW"),
                .text(rawText: "@intensity.unit=mW"),
                .text(rawText: "@Intensity=1"),
                .text(rawText: "@Intensity.unit=mW"),
                .text(rawText: "@bad=3"),
                .text(rawText: "@bad.unit="),
                .text(rawText: "@gain=2"),
                .text(rawText: "@gain.unit=mW"),
                .text(rawText: "@orphan.unit=V"),
                .text(rawText: "1"),
                .text(rawText: "@intensity=4"),
                .text(rawText: "@intensity.unit=W"),
            ],
        ]
    )
    let milliWatt = try OpaqueUnitSymbol(validating: "mW")
    let volt = try OpaqueUnitSymbol(validating: "V")
    let watt = try OpaqueUnitSymbol(validating: "W")
    let definitions = [
        try validatorAttribute(
            "intensity",
            type: .exactDecimal,
            unit: .specified(milliWatt)
        ),
        try validatorAttribute(
            "bad",
            type: .exactDecimal,
            unit: .specified(milliWatt)
        ),
        try validatorAttribute("gain", type: .exactDecimal, unit: .dimensionless),
        try validatorAttribute(
            "orphan",
            type: .exactDecimal,
            unit: .specified(volt)
        ),
    ]
    let plan = try validatorSpikeEventPlan(staged: staged, attributes: definitions)

    let report = PreparedScientificImportValidator.validate(
        validatorForgedPrepared(plan: plan)
    )

    #expect(report.blockingIssues.map(\.kind) == [
        .duplicateEventAttributeInOccurrence(
            eventTimestampCell: try validatorCell(column: 2, row: 1),
            firstCell: try validatorCell(column: 2, row: 2)
        ),
        .duplicateInlineUnitSuggestion(
            eventTimestampCell: try validatorCell(column: 2, row: 1),
            firstCell: try validatorCell(column: 2, row: 4)
        ),
        .unknownEventAttributeKey,
        .unknownEventAttributeKey,
        .inlineUnitSuggestionInvalid(issue: .empty),
        .inlineUnitConflictsWithDefinition(
            suggested: milliWatt,
            confirmed: .dimensionless
        ),
        .inlineUnitWithoutAttribute(
            eventTimestampCell: try validatorCell(column: 2, row: 1)
        ),
        .inlineUnitConflictsWithDefinition(
            suggested: watt,
            confirmed: .specified(milliWatt)
        ),
        .inlineUnitSuggestionsConflict(
            firstCell: try validatorCell(column: 2, row: 4),
            firstUnit: milliWatt,
            conflictingUnit: watt
        ),
    ])
    #expect(report.blockingIssues.map { $0.location.sourceCell?.oneBasedDataRowIndex }
        == [3, 5, 6, 7, 9, 11, 12, 15, 15])
}

@Test
func preparedScientificImportValidatorEnforcesPhaseBarriers() throws {
    let structurallyInvalidStaged = try validatorStaged(
        headers: ["unit"],
        columns: [[.text(rawText: "")]]
    )
    let repeatedKey = try EventAttributeKey(validating: "same")
    let repeatedDefinition = ResolvedEventAttributeDefinitionPlan(
        key: repeatedKey,
        scalarType: .string,
        role: .scientific,
        unit: .notApplicable,
        emptyStringPolicy: .forbid
    )
    let structuralPlan = try validatorSingleSpikePlan(
        staged: structurallyInvalidStaged,
        attributes: [repeatedDefinition, repeatedDefinition]
    )
    let structuralReport = PreparedScientificImportValidator.validate(
        validatorForgedPrepared(plan: structuralPlan)
    )
    #expect(structuralReport.blockingIssues.map(\.kind) == [
        .duplicateEventAttributeDefinitionKey(firstDefinitionIndex: 1),
    ])

    let lexicallyInvalidStaged = try validatorStaged(
        headers: ["unit", "event"],
        columns: [
            [.text(rawText: ""), .blank],
            [.text(rawText: "0"), .text(rawText: "@unknown=1")],
        ]
    )
    let lexicalPlan = try validatorSpikeEventPlan(
        staged: lexicallyInvalidStaged,
        attributes: []
    )
    let lexicalReport = PreparedScientificImportValidator.validate(
        validatorForgedPrepared(plan: lexicalPlan)
    )
    #expect(lexicalReport.blockingIssues.map(\.kind) == [
        .timestampParseFailed(issue: .empty),
    ])
}

@Test
func preparedScientificImportValidatorCapsTypedIssuesInUTF8KeyOrder() throws {
    let issueCount = PreparedScientificImportValidator.maximumReportedIssueCount + 2
    let keys = try (0..<issueCount).map {
        try EventAttributeKey(validating: String(format: "k%03d", $0))
    }
    let definitions = keys.reversed().map {
        ResolvedEventAttributeDefinitionPlan(
            key: $0,
            scalarType: .integer,
            role: .scientific,
            unit: .dimensionless,
            emptyStringPolicy: .forbid
        )
    }
    let metadata = keys.reversed().map {
        StagedCellValue.text(rawText: "@\($0.canonicalText)=")
    }
    let eventCells = [StagedCellValue.text(rawText: "0")] + metadata
    let staged = try validatorStaged(
        headers: ["unit", "event"],
        columns: [Array(repeating: .blank, count: eventCells.count), eventCells]
    )
    let plan = try validatorSpikeEventPlan(staged: staged, attributes: definitions)
    let prepared = validatorForgedPrepared(plan: plan)

    let first = PreparedScientificImportValidator.validate(prepared)
    let second = PreparedScientificImportValidator.validate(prepared)

    #expect(first == second)
    #expect(first.blockingIssues.count
        == PreparedScientificImportValidator.maximumReportedIssueCount)
    #expect(first.additionalBlockingIssueCount == 2)
    #expect(first.blockingIssues.first?.location.attributeKey == keys[0])
    #expect(first.blockingIssues.last?.location.attributeKey
        == keys[PreparedScientificImportValidator.maximumReportedIssueCount - 1])
    #expect(first.blockingIssues.allSatisfy {
        $0.kind == .eventAttributeValueInvalid(
            expectedType: .integer,
            issue: .exactNumber(.empty)
        )
    })
}

@Test
func preparedScientificImportValidatorReplaysSignedWholeGroupEventRebase() throws {
    let staged = try validatorStaged(
        headers: ["unit", "event"],
        columns: [
            [
                .text(rawText: "-1"),
                .text(rawText: "0"),
                .text(rawText: "2"),
            ],
            [
                .text(rawText: "1"),
                .text(rawText: "@condition=x"),
                .text(rawText: "-1"),
            ],
        ]
    )
    let condition = try validatorAttribute(
        "condition",
        type: .string,
        unit: .notApplicable
    )
    let originCell = try validatorCell(column: 2, row: 1)
    let plan = try validatorConfiguredSpikeEventPlan(
        staged: staged,
        eventOrder: .stableAscendingSort,
        timeBasis: .eventRelative(
            origin: StagedEventOccurrenceReference(timestampCell: originCell)
        ),
        attributes: [condition]
    )
    let prepared = try ScientificImportNormalizer.normalize(resolvedPlan: plan)

    let report = PreparedScientificImportValidator.validate(prepared)

    #expect(report.blockingIssues.isEmpty)
}

@Test
func preparedScientificImportValidatorRejectsSelectedMetadataCellAsOrigin() throws {
    let staged = try validatorStaged(
        headers: ["unit", "event"],
        columns: [
            [.blank, .blank],
            [.text(rawText: "0"), .text(rawText: "@label=x")],
        ]
    )
    let label = try validatorAttribute("label", type: .string, unit: .notApplicable)
    let selectedCell = try validatorCell(column: 2, row: 2)
    let eventID = try validatorEventID("event")
    let plan = try validatorConfiguredSpikeEventPlan(
        staged: staged,
        timeBasis: .eventRelative(
            origin: StagedEventOccurrenceReference(timestampCell: selectedCell)
        ),
        attributes: [label]
    )

    let report = PreparedScientificImportValidator.validate(
        validatorForgedPrepared(plan: plan)
    )

    #expect(report.blockingIssues.map(\.kind) == [.selectedOriginIsNotEventTimestamp])
    #expect(report.blockingIssues[0].location.sourceCell == selectedCell)
    #expect(report.blockingIssues[0].location.eventDefinitionID == eventID)
}

@Test
func preparedScientificImportValidatorUsesOnlyScientificAttributesForOriginIdentity() throws {
    let staged = try validatorStaged(
        headers: ["unit", "event"],
        columns: [
            [.blank, .blank, .blank, .blank, .blank],
            [
                .text(rawText: "1"),
                .text(rawText: "@condition=A"),
                .blank,
                .text(rawText: "1"),
                .text(rawText: "@condition=B"),
            ],
        ]
    )
    let originCell = try validatorCell(column: 2, row: 1)
    let eventID = try validatorEventID("event")
    let timeBasis = ResolvedEventScopeTimeBasisPlan.eventRelative(
        origin: StagedEventOccurrenceReference(timestampCell: originCell)
    )
    let scientific = try validatorAttribute(
        "condition",
        type: .string,
        role: .scientific,
        unit: .notApplicable
    )
    let scientificPlan = try validatorConfiguredSpikeEventPlan(
        staged: staged,
        timeBasis: timeBasis,
        attributes: [scientific]
    )
    let scientificReport = PreparedScientificImportValidator.validate(
        try ScientificImportNormalizer.normalize(resolvedPlan: scientificPlan)
    )
    #expect(scientificReport.blockingIssues.isEmpty)

    let presentation = try validatorAttribute(
        "condition",
        type: .string,
        role: .presentation,
        unit: .notApplicable
    )
    let presentationPlan = try validatorConfiguredSpikeEventPlan(
        staged: staged,
        timeBasis: timeBasis,
        attributes: [presentation]
    )
    let presentationReport = PreparedScientificImportValidator.validate(
        validatorForgedPrepared(plan: presentationPlan)
    )
    #expect(presentationReport.blockingIssues.map(\.kind) == [
        .selectedOriginAmbiguous(
            eventDefinitionID: eventID,
            multiplicity: 2
        ),
    ])
    #expect(presentationReport.blockingIssues[0].location.sourceCell == originCell)
}

@Test
func preparedScientificImportValidatorChecksRebaseOverflowWithValueAndOriginCells() throws {
    let staged = try validatorStaged(
        headers: ["unit", "event"],
        columns: [
            [
                .text(rawText: "9223372036854.775807"),
                .text(rawText: "-9223372036854.775808"),
            ],
            [
                .text(rawText: "-9223372036854.775808"),
                .blank,
            ],
        ]
    )
    let originCell = try validatorCell(column: 2, row: 1)
    let overflowValueCell = try validatorCell(column: 1, row: 1)
    let plan = try validatorConfiguredSpikeEventPlan(
        staged: staged,
        timeBasis: .eventRelative(
            origin: StagedEventOccurrenceReference(timestampCell: originCell)
        ),
        attributes: []
    )

    let report = PreparedScientificImportValidator.validate(
        validatorForgedPrepared(plan: plan)
    )

    #expect(report.blockingIssues.map(\.kind) == [
        .eventRelativeRebaseOverflow(originCell: originCell),
    ])
    #expect(report.blockingIssues[0].location.sourceCell == overflowValueCell)
    #expect(!report.blockingIssues.contains(where: {
        if case .sourceOrderNotAscending = $0.kind { return true }
        return false
    }))
}

@Test
func preparedScientificImportValidatorRejectsNegativeRecordingElapsedTimestamps() throws {
    let staged = try validatorStaged(
        headers: ["unit", "event"],
        columns: [
            [.text(rawText: "-0.000001")],
            [.text(rawText: "-0.000002")],
        ]
    )
    let plan = try validatorSpikeEventPlan(staged: staged, attributes: [])
    let spikeCell = try validatorCell(column: 1, row: 1)
    let eventCell = try validatorCell(column: 2, row: 1)

    let report = PreparedScientificImportValidator.validate(
        validatorForgedPrepared(plan: plan)
    )

    #expect(report.blockingIssues.map(\.kind) == [
        .recordingElapsedTimestampIsNegative,
        .recordingElapsedTimestampIsNegative,
    ])
    #expect(report.blockingIssues.map { $0.location.sourceCell } == [spikeCell, eventCell])
}

@Test
func preparedScientificImportValidatorCountsAdjacentSourceOrderDescents() throws {
    let staged = try validatorStaged(
        headers: ["unit"],
        columns: [[
            .text(rawText: "3"),
            .text(rawText: "2"),
            .text(rawText: "1"),
        ]]
    )
    let plan = try validatorSingleSpikePlan(staged: staged, attributes: [])
    let spikeID = try validatorSpikeID("unit")
    let firstPreviousCell = try validatorCell(column: 1, row: 1)
    let firstCurrentCell = try validatorCell(column: 1, row: 2)

    let report = PreparedScientificImportValidator.validate(
        validatorForgedPrepared(plan: plan)
    )

    #expect(report.blockingIssues.map(\.kind) == [
        .sourceOrderNotAscending(
            descentCount: 2,
            firstPreviousCell: firstPreviousCell,
            firstCurrentCell: firstCurrentCell
        ),
    ])
    #expect(report.blockingIssues[0].location.spikeTrainID == spikeID)
}

@Test
func preparedScientificImportValidatorCountsEventSourceOrderDescents() throws {
    let staged = try validatorStaged(
        headers: ["unit", "event"],
        columns: [
            [.blank, .blank, .blank],
            [
                .text(rawText: "3"),
                .text(rawText: "2"),
                .text(rawText: "1"),
            ],
        ]
    )
    let plan = try validatorSpikeEventPlan(staged: staged, attributes: [])
    let eventID = try validatorEventID("event")
    let firstPreviousCell = try validatorCell(column: 2, row: 1)
    let firstCurrentCell = try validatorCell(column: 2, row: 2)

    let report = PreparedScientificImportValidator.validate(
        validatorForgedPrepared(plan: plan)
    )

    #expect(report.blockingIssues.map(\.kind) == [
        .sourceOrderNotAscending(
            descentCount: 2,
            firstPreviousCell: firstPreviousCell,
            firstCurrentCell: firstCurrentCell
        ),
    ])
    #expect(report.blockingIssues[0].location.eventDefinitionID == eventID)
    #expect(report.blockingIssues[0].location.spikeTrainID == nil)
}

@Test
func preparedScientificImportValidatorReplaysStableTiesAndBothDuplicatePolicies() throws {
    let staged = try validatorStaged(
        headers: ["unit"],
        columns: [[
            .text(rawText: "2"),
            .text(rawText: "1"),
            .text(rawText: "1"),
        ]]
    )
    let column = try validatorColumn(1)

    for duplicateDecision in [
        ExactDuplicateDecision.preserveMultiplicity,
        .collapseExact,
    ] {
        let group = ResolvedEventScopeGroupPlan(
            semanticID: try validatorGroupID("group"),
            spikeTrains: [
                ResolvedSpikeTrainColumnPlan(
                    sourceColumn: column,
                    semanticID: try validatorSpikeID("unit"),
                    orderDecision: .stableAscendingSort,
                    duplicateDecision: duplicateDecision
                ),
            ],
            eventDefinitions: [],
            timeBasis: .recordingElapsed
        )
        let plan = validatorPlan(staged: staged, groups: [group])
        let prepared = try ScientificImportNormalizer.normalize(resolvedPlan: plan)
        let report = PreparedScientificImportValidator.validate(
            prepared
        )
        #expect(report.blockingIssues.isEmpty)
    }
}

@Test
func preparedScientificImportValidatorStableSortsEqualEventsWithoutCollapsingMetadata() throws {
    let staged = try validatorStaged(
        headers: ["unit", "event"],
        columns: [
            Array(repeating: .blank, count: 6),
            [
                .text(rawText: "2"),
                .text(rawText: "@label=late"),
                .text(rawText: "1"),
                .text(rawText: "@label=first-tie"),
                .text(rawText: "1"),
                .text(rawText: "@label=second-tie"),
            ],
        ]
    )
    let label = try validatorAttribute("label", type: .string, unit: .notApplicable)
    let plan = try validatorConfiguredSpikeEventPlan(
        staged: staged,
        eventOrder: .stableAscendingSort,
        attributes: [label]
    )
    let prepared = try ScientificImportNormalizer.normalize(resolvedPlan: plan)

    let report = PreparedScientificImportValidator.validate(prepared)

    #expect(report.blockingIssues.isEmpty)
}

@Test
func preparedScientificImportValidatorStopsAtOriginPhaseBeforeCoordinateAndOrderChecks() throws {
    let staged = try validatorStaged(
        headers: ["unit", "event"],
        columns: [
            [.text(rawText: "3"), .text(rawText: "-1")],
            [.text(rawText: "0"), .text(rawText: "@label=x")],
        ]
    )
    let label = try validatorAttribute("label", type: .string, unit: .notApplicable)
    let selectedMetadataCell = try validatorCell(column: 2, row: 2)
    let plan = try validatorConfiguredSpikeEventPlan(
        staged: staged,
        timeBasis: .eventRelative(
            origin: StagedEventOccurrenceReference(timestampCell: selectedMetadataCell)
        ),
        attributes: [label]
    )

    let report = PreparedScientificImportValidator.validate(
        validatorForgedPrepared(plan: plan)
    )

    #expect(report.blockingIssues.map(\.kind) == [.selectedOriginIsNotEventTimestamp])
}

@Test
func preparedScientificImportValidatorFindsDatasetFieldsAndGatesGroupIdentity() throws {
    let prepared = try validatorRichPrepared()
    let originalData = prepared.data
    let originalGroup = originalData.eventScopeGroups[0]
    let originalTrain = originalGroup.spikeTrains[0]
    let tamperedTrain = PreparedSpikeTrain(
        semanticID: originalTrain.semanticID,
        timestamps: [MicrosecondTick(microseconds: 123)]
    )
    let tamperedGroup = PreparedEventScopeGroup(
        semanticID: try validatorGroupID("tampered_group"),
        timeBasis: originalGroup.timeBasis,
        spikeTrains: [tamperedTrain],
        eventDefinitions: originalGroup.eventDefinitions
    )
    let originalScientific = originalData.scientificAttributeDefinitions
    let firstDefinition = originalScientific[0]
    let tamperedFirstDefinition = ResolvedEventAttributeDefinitionPlan(
        key: firstDefinition.key,
        scalarType: .boolean,
        role: firstDefinition.role,
        unit: firstDefinition.unit,
        emptyStringPolicy: firstDefinition.emptyStringPolicy
    )
    let tamperedData = PreparedScientificImportData(
        activityMode: .intentionalMultiUnit,
        eventScopeGroups: [tamperedGroup],
        scientificAttributeDefinitions: [tamperedFirstDefinition]
            + originalScientific.dropFirst(),
        presentationAttributeDefinitions: []
    )

    let report = PreparedScientificImportValidator.validate(
        validatorReplacing(prepared, data: tamperedData)
    )

    #expect(validatorComparisonFields(report) == [
        .dataActivityMode,
        .dataScientificAttributeDefinitionScalarType(position: 1),
        .dataPresentationAttributeDefinitionCount,
        .dataGroupSemanticID(position: 1),
    ])
    #expect(!validatorComparisonFields(report).contains(.dataSpikeTimestamp(position: 1)))
    #expect(report.warnings.isEmpty)
}

@Test
func preparedScientificImportValidatorGatesDataAndProvenanceGroupCountsIndependently() throws {
    let prepared = try validatorRichPrepared()
    let emptyData = PreparedScientificImportData(
        activityMode: prepared.data.activityMode,
        eventScopeGroups: [],
        scientificAttributeDefinitions: prepared.data.scientificAttributeDefinitions,
        presentationAttributeDefinitions: prepared.data.presentationAttributeDefinitions
    )
    let emptyProvenance = ScientificImportNormalizationProvenance(
        resolvedPlan: prepared.provenance.resolvedPlan,
        eventScopeGroups: []
    )

    let report = PreparedScientificImportValidator.validate(
        validatorReplacing(
            prepared,
            data: emptyData,
            provenance: emptyProvenance
        )
    )

    #expect(validatorComparisonFields(report) == [
        .dataGroupCount,
        .provenanceGroupCount,
    ])
    #expect(report.blockingIssues.allSatisfy {
        if case .preparedComparisonCountMismatch(_, expected: 1, actual: 0) = $0.kind {
            return true
        }
        return false
    })
}

@Test
func preparedScientificImportValidatorComparesDataSiblingsAndGatesOccurrenceTick() throws {
    let prepared = try validatorRichPrepared()
    let group = prepared.data.eventScopeGroups[0]
    let train = group.spikeTrains[0]
    var tamperedTimestamps = train.timestamps
    tamperedTimestamps[0] = MicrosecondTick(
        microseconds: tamperedTimestamps[0].microseconds + 10
    )
    let tamperedTrain = PreparedSpikeTrain(
        semanticID: train.semanticID,
        timestamps: tamperedTimestamps
    )

    let definition = group.eventDefinitions[0]
    let first = definition.occurrences[0]
    let second = definition.occurrences[1]
    let tamperedValue = EventAttributeValue.string(
        try CanonicalStringValue(validating: "tampered")
    )
    var firstScientific = first.scientificAttributes
    firstScientific[0] = PreparedEventAttribute(
        key: firstScientific[0].key,
        value: tamperedValue
    )
    let tamperedFirst = PreparedEventOccurrence(
        tick: MicrosecondTick(microseconds: first.tick.microseconds + 1),
        scientificAttributes: firstScientific,
        presentationAttributes: first.presentationAttributes
    )
    var secondScientific = second.scientificAttributes
    secondScientific[0] = PreparedEventAttribute(
        key: secondScientific[0].key,
        value: tamperedValue
    )
    let tamperedSecond = PreparedEventOccurrence(
        tick: second.tick,
        scientificAttributes: secondScientific,
        presentationAttributes: second.presentationAttributes
    )
    let tamperedDefinition = PreparedEventDefinition(
        semanticID: definition.semanticID,
        eventTypeID: try validatorEventTypeID("tampered_type"),
        occurrences: [tamperedFirst, tamperedSecond]
    )
    let tamperedGroup = PreparedEventScopeGroup(
        semanticID: group.semanticID,
        timeBasis: .recordingElapsed,
        spikeTrains: [tamperedTrain],
        eventDefinitions: [tamperedDefinition]
    )
    let tamperedData = PreparedScientificImportData(
        activityMode: prepared.data.activityMode,
        eventScopeGroups: [tamperedGroup],
        scientificAttributeDefinitions: prepared.data.scientificAttributeDefinitions,
        presentationAttributeDefinitions: prepared.data.presentationAttributeDefinitions
    )

    let report = PreparedScientificImportValidator.validate(
        validatorReplacing(prepared, data: tamperedData)
    )

    #expect(validatorComparisonFields(report) == [
        .dataGroupTimeBasis,
        .dataSpikeTimestamp(position: 1),
        .dataEventTypeID,
        .dataEventOccurrenceTick(position: 1),
        .dataEventOccurrenceScientificAttributeValue(occurrence: 2, position: 1),
    ])
    #expect(!validatorComparisonFields(report).contains(
        .dataEventOccurrenceScientificAttributeValue(occurrence: 1, position: 1)
    ))
}

@Test
func preparedScientificImportValidatorGatesAttributeValueAfterKeyMismatch() throws {
    let prepared = try validatorRichPrepared()
    let group = prepared.data.eventScopeGroups[0]
    let definition = group.eventDefinitions[0]
    var occurrences = definition.occurrences
    let first = occurrences[0]
    var scientific = first.scientificAttributes
    scientific[0] = PreparedEventAttribute(
        key: try EventAttributeKey(validating: "tampered_key"),
        value: .string(try CanonicalStringValue(validating: "tampered_value"))
    )
    occurrences[0] = PreparedEventOccurrence(
        tick: first.tick,
        scientificAttributes: scientific,
        presentationAttributes: first.presentationAttributes
    )
    let tamperedDefinition = PreparedEventDefinition(
        semanticID: definition.semanticID,
        eventTypeID: definition.eventTypeID,
        occurrences: occurrences
    )
    let tamperedGroup = PreparedEventScopeGroup(
        semanticID: group.semanticID,
        timeBasis: group.timeBasis,
        spikeTrains: group.spikeTrains,
        eventDefinitions: [tamperedDefinition]
    )
    let tamperedData = PreparedScientificImportData(
        activityMode: prepared.data.activityMode,
        eventScopeGroups: [tamperedGroup],
        scientificAttributeDefinitions: prepared.data.scientificAttributeDefinitions,
        presentationAttributeDefinitions: prepared.data.presentationAttributeDefinitions
    )

    let report = PreparedScientificImportValidator.validate(
        validatorReplacing(prepared, data: tamperedData)
    )

    #expect(validatorComparisonFields(report) == [
        .dataEventOccurrenceScientificAttributeKey(occurrence: 1, position: 1),
    ])
}

@Test
func preparedScientificImportValidatorDetectsDataAndProvenanceOriginTampering() throws {
    let prepared = try validatorRichPrepared()
    let dataGroup = prepared.data.eventScopeGroups[0]
    guard case .eventRelative(let originalOrigin) = dataGroup.timeBasis else {
        Issue.record("Expected event-relative rich fixture")
        return
    }
    let tamperedOrigin = PreparedEventOrigin(
        eventDefinitionID: try validatorEventID("tampered_origin"),
        scientificAttributes: originalOrigin.scientificAttributes
    )
    let tamperedDataGroup = PreparedEventScopeGroup(
        semanticID: dataGroup.semanticID,
        timeBasis: .eventRelative(origin: tamperedOrigin),
        spikeTrains: dataGroup.spikeTrains,
        eventDefinitions: dataGroup.eventDefinitions
    )
    let tamperedData = PreparedScientificImportData(
        activityMode: prepared.data.activityMode,
        eventScopeGroups: [tamperedDataGroup],
        scientificAttributeDefinitions: prepared.data.scientificAttributeDefinitions,
        presentationAttributeDefinitions: prepared.data.presentationAttributeDefinitions
    )

    let provenanceGroup = prepared.provenance.eventScopeGroups[0]
    guard case .eventRelative(let originCell, let sourceOriginTick) =
        provenanceGroup.timeBasis else {
        Issue.record("Expected event-relative provenance")
        return
    }
    let tamperedProvenanceGroup = PreparedEventScopeGroupProvenance(
        semanticID: provenanceGroup.semanticID,
        timeBasis: .eventRelative(
            originCell: originCell,
            sourceOriginTick: MicrosecondTick(
                microseconds: sourceOriginTick.microseconds + 1
            )
        ),
        spikeTrains: provenanceGroup.spikeTrains,
        eventDefinitions: provenanceGroup.eventDefinitions
    )
    let tamperedProvenance = ScientificImportNormalizationProvenance(
        resolvedPlan: prepared.provenance.resolvedPlan,
        eventScopeGroups: [tamperedProvenanceGroup]
    )

    let report = PreparedScientificImportValidator.validate(
        validatorReplacing(
            prepared,
            data: tamperedData,
            provenance: tamperedProvenance
        )
    )

    #expect(validatorComparisonFields(report) == [
        .dataGroupEventOriginDefinitionID,
        .provenanceGroupSourceOriginTick,
    ])
}

@Test
func preparedScientificImportValidatorReportsAllTandemProvenanceSiblingTampering() throws {
    let prepared = try validatorRichPrepared()
    let group = prepared.provenance.eventScopeGroups[0]
    guard case .eventRelative(let originCell, let sourceOriginTick) = group.timeBasis else {
        Issue.record("Expected event-relative provenance")
        return
    }

    let spike = group.spikeTrains[0]
    var tamperedSources = spike.timestampSources
    tamperedSources[0][0] = try validatorCell(column: 1, row: 1)
    let tamperedSpike = PreparedSpikeTrainProvenance(
        semanticID: spike.semanticID,
        sourceColumn: try validatorColumn(2),
        orderDecision: .preserveSourceOrder,
        duplicateDecision: .preserveMultiplicity,
        sourceOrderDescentCount: 99,
        timestampSources: tamperedSources
    )

    let event = group.eventDefinitions[0]
    var occurrences = event.occurrences
    var firstAttributes = occurrences[0].attributes
    let intensity = firstAttributes[1]
    firstAttributes[1] = PreparedEventAttributeProvenance(
        key: intensity.key,
        valueCell: try validatorCell(column: 2, row: 7),
        inlineUnitSuggestion: .present(
            symbol: try OpaqueUnitSymbol(validating: "W"),
            sourceCell: try validatorCell(column: 2, row: 8)
        )
    )
    occurrences[0] = PreparedEventOccurrenceProvenance(
        timestampCell: occurrences[0].timestampCell,
        attributes: firstAttributes
    )
    let tamperedEvent = PreparedEventDefinitionProvenance(
        semanticID: event.semanticID,
        sourceColumn: try validatorColumn(1),
        orderDecision: .preserveSourceOrder,
        sourceOrderDescentCount: 99,
        occurrences: occurrences
    )
    let tamperedGroup = PreparedEventScopeGroupProvenance(
        semanticID: group.semanticID,
        timeBasis: .eventRelative(
            originCell: originCell,
            sourceOriginTick: MicrosecondTick(
                microseconds: sourceOriginTick.microseconds + 1
            )
        ),
        spikeTrains: [tamperedSpike],
        eventDefinitions: [tamperedEvent]
    )
    let tamperedProvenance = ScientificImportNormalizationProvenance(
        resolvedPlan: prepared.provenance.resolvedPlan,
        eventScopeGroups: [tamperedGroup]
    )

    let report = PreparedScientificImportValidator.validate(
        validatorReplacing(prepared, provenance: tamperedProvenance)
    )

    #expect(validatorComparisonFields(report) == [
        .provenanceGroupSourceOriginTick,
        .provenanceSpikeSourceColumn,
        .provenanceSpikeOrderDecision,
        .provenanceSpikeDuplicateDecision,
        .provenanceSpikeSourceOrderDescentCount,
        .provenanceSpikeTimestampSourceCell(outputPosition: 1, sourcePosition: 1),
        .provenanceEventSourceColumn,
        .provenanceEventOrderDecision,
        .provenanceEventSourceOrderDescentCount,
        .provenanceEventAttributeValueCell(occurrence: 1, position: 2),
        .provenanceEventAttributeInlineUnitSymbol(occurrence: 1, position: 2),
        .provenanceEventAttributeInlineUnitSourceCell(occurrence: 1, position: 2),
    ])
}

@Test
func preparedScientificImportValidatorCapsEmptyDefinitionWarningsAndIsPure() throws {
    let warningCount = PreparedScientificImportValidator.maximumReportedWarningCount + 2
    let headers = ["unit"] + (0..<warningCount).map {
        String(format: "event_%03d", $0)
    }
    let columns = Array(
        repeating: [StagedCellValue](),
        count: warningCount + 1
    )
    let staged = try validatorStaged(headers: headers, columns: columns)
    let sourceColumns = try (1...(warningCount + 1)).map(validatorColumn)
    let spike = ResolvedSpikeTrainColumnPlan(
        sourceColumn: sourceColumns[0],
        semanticID: try validatorSpikeID("unit"),
        orderDecision: .preserveSourceOrder,
        duplicateDecision: .preserveMultiplicity
    )
    let events = try (0..<warningCount).map { offset in
        ResolvedEventDefinitionColumnPlan(
            sourceColumn: sourceColumns[offset + 1],
            semanticID: try validatorEventID(String(format: "event_%03d", offset)),
            eventTypeID: try validatorEventTypeID("stimulus"),
            orderDecision: .preserveSourceOrder
        )
    }
    let group = ResolvedEventScopeGroupPlan(
        semanticID: try validatorGroupID("group"),
        spikeTrains: [spike],
        eventDefinitions: events,
        timeBasis: .recordingElapsed
    )
    let plan = validatorPlan(staged: staged, groups: [group])
    let prepared = try ScientificImportNormalizer.normalize(resolvedPlan: plan)
    let before = prepared
    let firstEventID = try validatorEventID("event_000")
    let lastRetainedEventID = try validatorEventID("event_127")
    let firstEventColumn = try validatorColumn(2)
    let lastRetainedEventColumn = try validatorColumn(129)

    let first = PreparedScientificImportValidator.validate(prepared)
    let second = PreparedScientificImportValidator.validate(prepared)

    #expect(prepared == before)
    #expect(first == second)
    #expect(first.blockingIssues.isEmpty)
    #expect(first.warnings.count
        == PreparedScientificImportValidator.maximumReportedWarningCount)
    #expect(first.additionalWarningCount == 2)
    #expect(first.warnings.allSatisfy { $0.kind == .emptyEventDefinition })
    #expect(first.warnings.first?.location.groupIndex == 1)
    #expect(first.warnings.first?.location.eventDefinitionID == firstEventID)
    #expect(first.warnings.first?.location.sourceColumn == firstEventColumn)
    #expect(first.warnings.last?.location.eventDefinitionID == lastRetainedEventID)
    #expect(first.warnings.last?.location.sourceColumn == lastRetainedEventColumn)

    let tamperedData = PreparedScientificImportData(
        activityMode: .intentionalMultiUnit,
        eventScopeGroups: prepared.data.eventScopeGroups,
        scientificAttributeDefinitions: prepared.data.scientificAttributeDefinitions,
        presentationAttributeDefinitions: prepared.data.presentationAttributeDefinitions
    )
    let blocked = PreparedScientificImportValidator.validate(
        validatorReplacing(prepared, data: tamperedData)
    )
    #expect(validatorComparisonFields(blocked) == [.dataActivityMode])
    #expect(blocked.warnings.isEmpty)
    #expect(blocked.additionalWarningCount == 0)
}

@Test
func preparedScientificImportValidatorCapsComparisonIssuesDeterministically() throws {
    let mismatchCount = PreparedScientificImportValidator.maximumReportedIssueCount + 2
    let headers = (0..<mismatchCount).map { String(format: "unit_%03d", $0) }
    let staged = try validatorStaged(
        headers: headers,
        columns: Array(
            repeating: [StagedCellValue.text(rawText: "1")],
            count: mismatchCount
        )
    )
    let spikes = try (0..<mismatchCount).map { offset in
        ResolvedSpikeTrainColumnPlan(
            sourceColumn: try validatorColumn(offset + 1),
            semanticID: try validatorSpikeID(String(format: "unit_%03d", offset)),
            orderDecision: .preserveSourceOrder,
            duplicateDecision: .preserveMultiplicity
        )
    }
    let group = ResolvedEventScopeGroupPlan(
        semanticID: try validatorGroupID("group"),
        spikeTrains: spikes,
        eventDefinitions: [],
        timeBasis: .recordingElapsed
    )
    let plan = validatorPlan(staged: staged, groups: [group])
    let prepared = try ScientificImportNormalizer.normalize(resolvedPlan: plan)
    let originalGroup = prepared.data.eventScopeGroups[0]
    let tamperedTrains = originalGroup.spikeTrains.map { train in
        PreparedSpikeTrain(
            semanticID: train.semanticID,
            timestamps: train.timestamps.map {
                MicrosecondTick(microseconds: $0.microseconds + 1)
            }
        )
    }
    let tamperedGroup = PreparedEventScopeGroup(
        semanticID: originalGroup.semanticID,
        timeBasis: originalGroup.timeBasis,
        spikeTrains: tamperedTrains,
        eventDefinitions: originalGroup.eventDefinitions
    )
    let tamperedData = PreparedScientificImportData(
        activityMode: prepared.data.activityMode,
        eventScopeGroups: [tamperedGroup],
        scientificAttributeDefinitions: prepared.data.scientificAttributeDefinitions,
        presentationAttributeDefinitions: prepared.data.presentationAttributeDefinitions
    )
    let tampered = validatorReplacing(prepared, data: tamperedData)
    let firstSpikeID = try validatorSpikeID("unit_000")
    let lastRetainedSpikeID = try validatorSpikeID("unit_127")

    let first = PreparedScientificImportValidator.validate(tampered)
    let second = PreparedScientificImportValidator.validate(tampered)

    #expect(first == second)
    #expect(first.blockingIssues.count
        == PreparedScientificImportValidator.maximumReportedIssueCount)
    #expect(first.additionalBlockingIssueCount == 2)
    #expect(first.blockingIssues.allSatisfy {
        $0.kind == .preparedComparisonValueMismatch(
            field: .dataSpikeTimestamp(position: 1)
        )
    })
    #expect(first.blockingIssues.first?.location.spikeTrainID == firstSpikeID)
    #expect(first.blockingIssues.last?.location.spikeTrainID == lastRetainedSpikeID)
    #expect(first.warnings.isEmpty)
}

private enum PreparedValidatorFixtureError: Error {
    case mismatchedColumns
    case expectedTwoColumns
}

private func validatorStaged(
    source: StagedTabularSource = .commaSeparatedValues,
    headers: [String?],
    columns: [[StagedCellValue]]
) throws -> StagedScientificImport {
    guard headers.count == columns.count else {
        throw PreparedValidatorFixtureError.mismatchedColumns
    }
    let stagedColumns = try headers.indices.map { offset in
        StagedScientificColumn(
            sourceColumn: try validatorColumn(offset + 1),
            header: headers[offset],
            cells: columns[offset]
        )
    }
    return try StagedScientificImport(
        source: source,
        columns: stagedColumns,
        suggestions: .none
    )
}

private func validatorPlan(
    staged: StagedScientificImport,
    unit: SpikeTimeUnit = .seconds,
    mode: ScientificDatasetActivityMode = .putativeSingleUnit,
    groups: [ResolvedEventScopeGroupPlan],
    attributes: [ResolvedEventAttributeDefinitionPlan] = []
) -> ResolvedScientificImportPlan {
    ResolvedScientificImportPlan(
        source: ResolvedScientificImportSource(stagedImport: staged),
        sourceTimeUnit: unit,
        activityMode: mode,
        eventScopeGroups: groups,
        eventAttributeDefinitions: attributes
    )
}

private func validatorSingleSpikePlan(
    staged: StagedScientificImport,
    attributes: [ResolvedEventAttributeDefinitionPlan]
) throws -> ResolvedScientificImportPlan {
    let column = try validatorColumn(1)
    let group = ResolvedEventScopeGroupPlan(
        semanticID: try validatorGroupID("group"),
        spikeTrains: [
            ResolvedSpikeTrainColumnPlan(
                sourceColumn: column,
                semanticID: try validatorSpikeID("unit"),
                orderDecision: .preserveSourceOrder,
                duplicateDecision: .preserveMultiplicity
            ),
        ],
        eventDefinitions: [],
        timeBasis: .recordingElapsed
    )
    return validatorPlan(staged: staged, groups: [group], attributes: attributes)
}

private func validatorSpikeEventPlan(
    staged: StagedScientificImport,
    unit: SpikeTimeUnit = .seconds,
    attributes: [ResolvedEventAttributeDefinitionPlan]
) throws -> ResolvedScientificImportPlan {
    try validatorConfiguredSpikeEventPlan(
        staged: staged,
        unit: unit,
        attributes: attributes
    )
}

private func validatorConfiguredSpikeEventPlan(
    staged: StagedScientificImport,
    unit: SpikeTimeUnit = .seconds,
    mode: ScientificDatasetActivityMode = .putativeSingleUnit,
    spikeOrder: TimestampOrderDecision = .preserveSourceOrder,
    duplicateDecision: ExactDuplicateDecision = .preserveMultiplicity,
    eventOrder: TimestampOrderDecision = .preserveSourceOrder,
    timeBasis: ResolvedEventScopeTimeBasisPlan = .recordingElapsed,
    attributes: [ResolvedEventAttributeDefinitionPlan]
) throws -> ResolvedScientificImportPlan {
    guard staged.columns.count == 2 else {
        throw PreparedValidatorFixtureError.expectedTwoColumns
    }
    let spikeColumn = try validatorColumn(1)
    let eventColumn = try validatorColumn(2)
    let group = ResolvedEventScopeGroupPlan(
        semanticID: try validatorGroupID("group"),
        spikeTrains: [
            ResolvedSpikeTrainColumnPlan(
                sourceColumn: spikeColumn,
                semanticID: try validatorSpikeID("unit"),
                orderDecision: spikeOrder,
                duplicateDecision: duplicateDecision
            ),
        ],
        eventDefinitions: [
            ResolvedEventDefinitionColumnPlan(
                sourceColumn: eventColumn,
                semanticID: try validatorEventID("event"),
                eventTypeID: try validatorEventTypeID("stimulus"),
                orderDecision: eventOrder
            ),
        ],
        timeBasis: timeBasis
    )
    return validatorPlan(
        staged: staged,
        unit: unit,
        mode: mode,
        groups: [group],
        attributes: attributes
    )
}

private func validatorAttribute(
    _ key: String,
    type: EventAttributeScalarType,
    role: EventAttributeRole = .scientific,
    unit: ConfirmedEventAttributeUnit,
    empty: EventEmptyStringPolicy = .forbid
) throws -> ResolvedEventAttributeDefinitionPlan {
    ResolvedEventAttributeDefinitionPlan(
        key: try EventAttributeKey(validating: key),
        scalarType: type,
        role: role,
        unit: unit,
        emptyStringPolicy: empty
    )
}

private func validatorForgedPrepared(
    plan: ResolvedScientificImportPlan
) -> PreparedScientificImport {
    PreparedScientificImport(
        data: PreparedScientificImportData(
            activityMode: plan.activityMode,
            eventScopeGroups: [],
            scientificAttributeDefinitions: [],
            presentationAttributeDefinitions: []
        ),
        provenance: ScientificImportNormalizationProvenance(
            resolvedPlan: plan,
            eventScopeGroups: []
        )
    )
}

private func validatorRichPrepared() throws -> PreparedScientificImport {
    let staged = try validatorStaged(
        headers: ["unit", "event"],
        columns: [
            [
                .text(rawText: "2"),
                .text(rawText: "1"),
                .text(rawText: "1"),
                .blank,
                .blank,
                .blank,
                .blank,
                .blank,
                .blank,
                .blank,
            ],
            [
                .text(rawText: "2"),
                .text(rawText: "@condition=B"),
                .text(rawText: "@label=late"),
                .text(rawText: "@intensity=2"),
                .text(rawText: "@intensity.unit=mW"),
                .text(rawText: "1"),
                .text(rawText: "@condition=A"),
                .text(rawText: "@label=early"),
                .text(rawText: "@intensity=1"),
                .text(rawText: "@intensity.unit=mW"),
            ],
        ]
    )
    let definitions = [
        try validatorAttribute(
            "label",
            type: .string,
            role: .presentation,
            unit: .notApplicable
        ),
        try validatorAttribute(
            "intensity",
            type: .exactDecimal,
            role: .scientific,
            unit: .specified(OpaqueUnitSymbol(validating: "mW"))
        ),
        try validatorAttribute(
            "condition",
            type: .string,
            role: .scientific,
            unit: .notApplicable
        ),
    ]
    let plan = try validatorConfiguredSpikeEventPlan(
        staged: staged,
        spikeOrder: .stableAscendingSort,
        duplicateDecision: .collapseExact,
        eventOrder: .stableAscendingSort,
        timeBasis: .eventRelative(
            origin: StagedEventOccurrenceReference(
                timestampCell: try validatorCell(column: 2, row: 6)
            )
        ),
        attributes: definitions
    )
    return try ScientificImportNormalizer.normalize(resolvedPlan: plan)
}

private func validatorReplacing(
    _ prepared: PreparedScientificImport,
    data: PreparedScientificImportData? = nil,
    provenance: ScientificImportNormalizationProvenance? = nil
) -> PreparedScientificImport {
    PreparedScientificImport(
        data: data ?? prepared.data,
        provenance: provenance ?? prepared.provenance
    )
}

private func validatorComparisonFields(
    _ report: PreparedScientificImportValidationReport
) -> [PreparedScientificImportComparisonField] {
    report.blockingIssues.compactMap { issue in
        switch issue.kind {
        case .preparedComparisonValueMismatch(let field):
            return field
        case .preparedComparisonCountMismatch(let field, _, _):
            return field
        default:
            return nil
        }
    }
}

private func validatorColumn(_ index: Int) throws -> StagedSourceColumnReference {
    try StagedSourceColumnReference(oneBasedIndex: index)
}

private func validatorCell(column: Int, row: Int) throws -> StagedSourceCellReference {
    try StagedSourceCellReference(
        column: validatorColumn(column),
        oneBasedDataRowIndex: row
    )
}

private func validatorGroupID(_ text: String) throws -> ScientificEventScopeGroupID {
    ScientificEventScopeGroupID(try ScientificSemanticID(validating: text))
}

private func validatorSpikeID(_ text: String) throws -> ScientificSpikeTrainID {
    ScientificSpikeTrainID(try ScientificSemanticID(validating: text))
}

private func validatorEventID(_ text: String) throws -> ScientificEventDefinitionID {
    ScientificEventDefinitionID(try ScientificSemanticID(validating: text))
}

private func validatorEventTypeID(_ text: String) throws -> ScientificEventTypeID {
    ScientificEventTypeID(try ScientificSemanticID(validating: text))
}
