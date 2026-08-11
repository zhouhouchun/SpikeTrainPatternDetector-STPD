@testable import STPDCore
import Testing

@Test
func scientificImportNormalizerPreparesValidMultiGroupDataDeterministically() throws {
    let staged = try normalizerStaged(
        source: .commaSeparatedValues,
        headers: ["unit_A", "event_A", "unit_B", "event_B"],
        columns: [
            [.text(rawText: "0"), .text(rawText: "1"), .blank],
            [.text(rawText: "0.5"), .blank, .blank],
            [.text(rawText: "2"), .text(rawText: "3"), .blank],
            [.blank, .text(rawText: "2.5"), .blank],
        ]
    )
    let references = try normalizerColumns(4)
    let groups = [
        EventScopeGroupManifestDraft(
            semanticID: try normalizerGroupID("z_group"),
            spikeTrains: [try normalizerSpike(references[0], id: "unit_A")],
            eventDefinitions: [try normalizerEvent(references[1], id: "event_A")],
            timeBasis: .recordingElapsed
        ),
        EventScopeGroupManifestDraft(
            semanticID: try normalizerGroupID("a_group"),
            spikeTrains: [try normalizerSpike(references[2], id: "unit_B")],
            eventDefinitions: [try normalizerEvent(references[3], id: "event_B")],
            timeBasis: .recordingElapsed
        ),
    ]
    let plan = try normalizerPlan(staged: staged, groups: groups)

    let prepared = try ScientificImportNormalizer.normalize(resolvedPlan: plan)

    #expect(prepared.data.eventScopeGroups.map {
        $0.semanticID.semanticID.canonicalText
    } == ["a_group", "z_group"])
    #expect(prepared.data.eventScopeGroups[0].spikeTrains[0].timestamps.map(\.microseconds)
        == [2_000_000, 3_000_000])
    #expect(prepared.data.eventScopeGroups[1].eventDefinitions[0].occurrences
        .map(\.tick.microseconds) == [500_000])
    #expect(prepared.provenance.resolvedPlan == plan)
    #expect(prepared.provenance.eventScopeGroups.map {
        $0.semanticID.semanticID.canonicalText
    } == ["a_group", "z_group"])
}

@Test
func scientificImportNormalizerSeparatesEquivalentTextDataFromSourceProvenance() throws {
    let textCells: [[StagedCellValue]] = [[
        .text(rawText: "0.000001"),
        .text(rawText: "1e-6"),
    ]]
    let numericCells: [[StagedCellValue]] = [[
        .spreadsheetNumber(rawLexeme: "0.000001"),
        .spreadsheetNumber(rawLexeme: "1e-6"),
    ]]
    let csv = try normalizerStaged(
        source: .commaSeparatedValues,
        headers: ["unit"],
        columns: textCells
    )
    let workbook = try normalizerStaged(
        source: .excelWorkbook(worksheetName: "Recording"),
        headers: ["unit"],
        columns: numericCells
    )
    let csvPrepared = try ScientificImportNormalizer.normalize(
        resolvedPlan: normalizerSingleSpikePlan(staged: csv)
    )
    let workbookPrepared = try ScientificImportNormalizer.normalize(
        resolvedPlan: normalizerSingleSpikePlan(staged: workbook)
    )

    #expect(csvPrepared.data == workbookPrepared.data)
    #expect(csvPrepared.provenance != workbookPrepared.provenance)
    #expect(csvPrepared.data.eventScopeGroups[0].spikeTrains[0].timestamps
        == [MicrosecondTick(microseconds: 1), MicrosecondTick(microseconds: 1)])
}

@Test
func scientificImportNormalizerExcludesExactSourceSnapshotFromPreparedData() throws {
    func csvBinding(_ character: Character) throws -> StagedSourceTransactionBinding {
        try StagedSourceTransactionBinding(
            sourceBytesSHA256: String(repeating: character, count: 64),
            selection: .commaSeparatedValues
        )
    }
    let cells: [[StagedCellValue]] = [[
        .text(rawText: "0"),
        .text(rawText: "0.001"),
    ]]
    let first = try normalizerStaged(
        source: .commaSeparatedValues,
        headers: ["unit"],
        columns: cells,
        sourceTransactionBinding: csvBinding("a")
    )
    let second = try normalizerStaged(
        source: .commaSeparatedValues,
        headers: ["unit"],
        columns: cells,
        sourceTransactionBinding: csvBinding("b")
    )
    let firstPrepared = try ScientificImportNormalizer.normalize(
        resolvedPlan: normalizerSingleSpikePlan(staged: first)
    )
    let secondPrepared = try ScientificImportNormalizer.normalize(
        resolvedPlan: normalizerSingleSpikePlan(staged: second)
    )

    #expect(firstPrepared.data == secondPrepared.data)
    #expect(firstPrepared.provenance != secondPrepared.provenance)
    #expect(firstPrepared != secondPrepared)
    #expect(
        firstPrepared.provenance.resolvedPlan.source.sourceTransactionBinding
            == first.sourceTransactionBinding
    )
    #expect(
        secondPrepared.provenance.resolvedPlan.source.sourceTransactionBinding
            == second.sourceTransactionBinding
    )
}

@Test
func scientificImportNormalizerTreatsCSVAndXMLLineEndingsAsOneScientificString() throws {
    func prepared(
        source: StagedTabularSource,
        noteLineBreak: String
    ) throws -> PreparedScientificImport {
        let staged = try normalizerStaged(
            source: source,
            headers: ["unit", "event"],
            columns: [
                [.blank, .blank],
                [
                    .text(rawText: "0"),
                    .text(rawText: "@note=line 1\(noteLineBreak)line 2"),
                ],
            ]
        )
        let columns = try normalizerColumns(2)
        let group = EventScopeGroupManifestDraft(
            semanticID: try normalizerGroupID("group"),
            spikeTrains: [try normalizerSpike(columns[0], id: "unit")],
            eventDefinitions: [try normalizerEvent(columns[1], id: "event")],
            timeBasis: .recordingElapsed
        )
        let note = try normalizerAttribute(
            "note",
            type: .string,
            role: .scientific,
            unit: .notApplicable,
            empty: .forbid
        )
        return try ScientificImportNormalizer.normalize(
            resolvedPlan: normalizerPlan(
                staged: staged,
                groups: [group],
                attributes: [note]
            )
        )
    }

    let csv = try prepared(
        source: .commaSeparatedValues,
        noteLineBreak: "\r\n"
    )
    let workbook = try prepared(
        source: .excelWorkbook(worksheetName: "Recording"),
        noteLineBreak: "\n"
    )

    #expect(csv.data == workbook.data)
    #expect(csv.provenance != workbook.provenance)
    let occurrence = try #require(
        csv.data.eventScopeGroups.first?.eventDefinitions.first?.occurrences.first
    )
    guard case .string(let note) = occurrence.scientificAttributes.first?.value else {
        Issue.record("Expected a scientific string attribute")
        return
    }
    #expect(note.canonicalText == "line 1\nline 2")
}

@Test
func scientificImportNormalizerTreatsOnlyStructuralBlankAsMissing() throws {
    let staged = try normalizerStaged(
        source: .commaSeparatedValues,
        headers: ["unit"],
        columns: [[.blank, .text(rawText: ""), .text(rawText: " \t")]]
    )
    let plan = try normalizerSingleSpikePlan(staged: staged)

    let error = try #require(normalizerError(plan))
    #expect(error.issues == [
        .timestampParseFailed(
            group: 1,
            cell: try normalizerCell(column: 1, row: 2),
            error: .empty
        ),
        .timestampParseFailed(
            group: 1,
            cell: try normalizerCell(column: 1, row: 3),
            error: .empty
        ),
    ])
}

@Test
func scientificImportNormalizerAcceptsSpreadsheetNumericSpikeAndEventWithMetadata() throws {
    let staged = try normalizerStaged(
        source: .excelWorkbook(worksheetName: "Recording"),
        headers: ["unit", "event"],
        columns: [
            [
                .spreadsheetNumber(rawLexeme: "0.000001"),
                .spreadsheetNumber(rawLexeme: "1e-6"),
                .blank,
            ],
            [
                .spreadsheetNumber(rawLexeme: "0.5"),
                .text(rawText: "@condition=A"),
                .blank,
            ],
        ]
    )
    let references = try normalizerColumns(2)
    let group = EventScopeGroupManifestDraft(
        semanticID: try normalizerGroupID("group"),
        spikeTrains: [try normalizerSpike(references[0], id: "unit")],
        eventDefinitions: [try normalizerEvent(references[1], id: "event")],
        timeBasis: .recordingElapsed
    )
    let condition = try normalizerAttribute(
        "condition",
        type: .string,
        role: .scientific,
        unit: .notApplicable,
        empty: .forbid
    )
    let plan = try normalizerPlan(
        staged: staged,
        groups: [group],
        attributes: [condition]
    )

    let prepared = try ScientificImportNormalizer.normalize(resolvedPlan: plan)
    let preparedGroup = prepared.data.eventScopeGroups[0]
    #expect(preparedGroup.spikeTrains[0].timestamps.map(\.microseconds) == [1, 1])
    #expect(preparedGroup.eventDefinitions[0].occurrences.map(\.tick.microseconds)
        == [500_000])
    #expect(preparedGroup.eventDefinitions[0].occurrences[0]
        .scientificAttributes.map(\.key.canonicalText) == ["condition"])
    #expect(prepared.provenance.resolvedPlan.source.columns[0].cells[0]
        == .spreadsheetNumber(rawLexeme: "0.000001"))
    #expect(prepared.provenance.resolvedPlan.source.columns[0].cells[1]
        == .spreadsheetNumber(rawLexeme: "1e-6"))
    #expect(prepared.provenance.resolvedPlan.source.columns[0].cells[0]
        != prepared.provenance.resolvedPlan.source.columns[0].cells[1])
}

@Test
func scientificImportNormalizerRejectsSpreadsheetNumberCellInCSVSource() throws {
    let staged = try normalizerStaged(
        source: .commaSeparatedValues,
        headers: ["unit", "event"],
        columns: [
            [.spreadsheetNumber(rawLexeme: "0.000001"), .blank],
            [
                .spreadsheetNumber(rawLexeme: "1"),
                .text(rawText: "@condition=orphan"),
            ],
        ]
    )
    let references = try normalizerColumns(2)
    let group = EventScopeGroupManifestDraft(
        semanticID: try normalizerGroupID("group"),
        spikeTrains: [try normalizerSpike(references[0], id: "unit")],
        eventDefinitions: [try normalizerEvent(references[1], id: "event")],
        timeBasis: .recordingElapsed
    )
    let plan = try normalizerPlan(staged: staged, groups: [group])

    let error = try #require(normalizerError(plan))
    #expect(error.issues == [
        .spreadsheetNumberOutsideWorkbook(
            group: 1,
            cell: try normalizerCell(column: 1, row: 1)
        ),
        .spreadsheetNumberOutsideWorkbook(
            group: 1,
            cell: try normalizerCell(column: 2, row: 1)
        ),
        .eventMetadataWithoutOccurrence(
            group: 1,
            cell: try normalizerCell(column: 2, row: 2)
        ),
    ])
}

@Test
func scientificImportNormalizerLocatesNumericFailuresAndClosesEventOccurrence() throws {
    let staged = try normalizerStaged(
        source: .excelWorkbook(worksheetName: "Recording"),
        headers: ["unit", "event"],
        columns: [
            [
                .spreadsheetNumber(rawLexeme: "bad"),
                .spreadsheetNumber(rawLexeme: "0.0000005"),
                .blank,
                .blank,
            ],
            [
                .spreadsheetNumber(rawLexeme: "1"),
                .text(rawText: "@condition=A"),
                .spreadsheetNumber(rawLexeme: "0.0000005"),
                .text(rawText: "@condition=B"),
            ],
        ]
    )
    let references = try normalizerColumns(2)
    let group = EventScopeGroupManifestDraft(
        semanticID: try normalizerGroupID("group"),
        spikeTrains: [try normalizerSpike(references[0], id: "unit")],
        eventDefinitions: [try normalizerEvent(references[1], id: "event")],
        timeBasis: .recordingElapsed
    )
    let condition = try normalizerAttribute(
        "condition", type: .string, role: .scientific,
        unit: .notApplicable, empty: .forbid
    )
    let plan = try normalizerPlan(
        staged: staged,
        groups: [group],
        attributes: [condition]
    )

    let error = try #require(normalizerError(plan))
    #expect(error.issues == [
        .spreadsheetNumberTimestampDecodeFailed(
            group: 1,
            cell: try normalizerCell(column: 1, row: 1),
            error: .invalidSyntax(utf8Offset: 0, issue: .unexpectedCharacter)
        ),
        .spreadsheetNumberTimestampDecodeFailed(
            group: 1,
            cell: try normalizerCell(column: 1, row: 2),
            error: .noWholeMicrosecondRoundTrip
        ),
        .spreadsheetNumberTimestampDecodeFailed(
            group: 1,
            cell: try normalizerCell(column: 2, row: 3),
            error: .noWholeMicrosecondRoundTrip
        ),
        .eventMetadataWithoutOccurrence(
            group: 1,
            cell: try normalizerCell(column: 2, row: 4)
        ),
    ])
}

@Test
func scientificImportNormalizerUsesSpreadsheetNumericEventOriginExactly() throws {
    let staged = try normalizerStaged(
        source: .excelWorkbook(worksheetName: "Recording"),
        headers: ["unit", "event"],
        columns: [
            [
                .spreadsheetNumber(rawLexeme: "-1"),
                .spreadsheetNumber(rawLexeme: "0"),
                .spreadsheetNumber(rawLexeme: "2"),
            ],
            [
                .spreadsheetNumber(rawLexeme: "1"),
                .text(rawText: "@condition=x"),
                .spreadsheetNumber(rawLexeme: "-1"),
            ],
        ]
    )
    let references = try normalizerColumns(2)
    let originCell = try normalizerCell(column: 2, row: 1)
    let group = EventScopeGroupManifestDraft(
        semanticID: try normalizerGroupID("group"),
        spikeTrains: [try normalizerSpike(references[0], id: "unit")],
        eventDefinitions: [
            try normalizerEvent(references[1], id: "event", order: .stableAscendingSort),
        ],
        timeBasis: .eventRelative(
            origin: StagedEventOccurrenceReference(timestampCell: originCell)
        )
    )
    let condition = try normalizerAttribute(
        "condition", type: .string, role: .scientific,
        unit: .notApplicable, empty: .forbid
    )
    let plan = try normalizerPlan(
        staged: staged,
        groups: [group],
        attributes: [condition]
    )

    let prepared = try ScientificImportNormalizer.normalize(resolvedPlan: plan)
    let preparedGroup = prepared.data.eventScopeGroups[0]
    #expect(preparedGroup.spikeTrains[0].timestamps.map(\.microseconds)
        == [-2_000_000, -1_000_000, 1_000_000])
    #expect(preparedGroup.eventDefinitions[0].occurrences.map(\.tick.microseconds)
        == [-2_000_000, 0])
    guard case .eventRelative(let origin) = preparedGroup.timeBasis else {
        Issue.record("Expected event-relative prepared time basis")
        return
    }
    #expect(origin.eventDefinitionID.semanticID.canonicalText == "event")
    #expect(origin.tick == .zero)
    #expect(origin.scientificAttributes.map(\.key.canonicalText) == ["condition"])
    #expect(origin.scientificAttributes.map(\.value) == [
        .string(try CanonicalStringValue(validating: "x")),
    ])
    #expect(prepared.provenance.eventScopeGroups[0].timeBasis == .eventRelative(
        originCell: originCell,
        sourceOriginTick: MicrosecondTick(microseconds: 1_000_000)
    ))
}

@Test
func scientificImportNormalizerReportsOffGridAndRangeErrorsInSourceOrder() throws {
    let staged = try normalizerStaged(
        source: .commaSeparatedValues,
        headers: ["unit"],
        columns: [[
            .text(rawText: "0.0000005"),
            .text(rawText: "9223372036854.775808"),
        ]]
    )
    let plan = try normalizerSingleSpikePlan(staged: staged)

    let error = try #require(normalizerError(plan))
    #expect(error.issues == [
        .timestampParseFailed(
            group: 1,
            cell: try normalizerCell(column: 1, row: 1),
            error: .notExactlyRepresentableInMicroseconds
        ),
        .timestampParseFailed(
            group: 1,
            cell: try normalizerCell(column: 1, row: 2),
            error: .outsideSignedMicrosecondRange
        ),
    ])
}

@Test
func scientificImportNormalizerRejectsNegativeRecordingElapsedTimestamp() throws {
    let staged = try normalizerStaged(
        source: .commaSeparatedValues,
        headers: ["unit"],
        columns: [[.text(rawText: "1"), .text(rawText: "-0.000001")]]
    )
    let plan = try normalizerSingleSpikePlan(staged: staged)

    let error = try #require(normalizerError(plan))
    #expect(error.issues == [
        .recordingElapsedTimestampIsNegative(
            group: 1,
            cell: try normalizerCell(column: 1, row: 2)
        ),
    ])
}

@Test
func scientificImportNormalizerRebasesWholeEventRelativeGroupExactly() throws {
    let staged = try normalizerStaged(
        source: .commaSeparatedValues,
        headers: ["unit", "event"],
        columns: [
            [.text(rawText: "-1"), .text(rawText: "0"), .text(rawText: "2")],
            [.text(rawText: "1"), .text(rawText: "@condition=x"), .text(rawText: "-1")],
        ]
    )
    let references = try normalizerColumns(2)
    let originCell = try normalizerCell(column: 2, row: 1)
    let group = EventScopeGroupManifestDraft(
        semanticID: try normalizerGroupID("group"),
        spikeTrains: [try normalizerSpike(references[0], id: "unit")],
        eventDefinitions: [
            try normalizerEvent(
                references[1],
                id: "event",
                order: .stableAscendingSort
            ),
        ],
        timeBasis: .eventRelative(
            origin: StagedEventOccurrenceReference(timestampCell: originCell)
        )
    )
    let condition = try normalizerAttribute(
        "condition",
        type: .string,
        role: .scientific,
        unit: .notApplicable,
        empty: .forbid
    )
    let plan = try normalizerPlan(staged: staged, groups: [group], attributes: [condition])

    let prepared = try ScientificImportNormalizer.normalize(resolvedPlan: plan)
    let preparedGroup = prepared.data.eventScopeGroups[0]

    #expect(preparedGroup.spikeTrains[0].timestamps.map(\.microseconds)
        == [-2_000_000, -1_000_000, 1_000_000])
    #expect(preparedGroup.eventDefinitions[0].occurrences.map(\.tick.microseconds)
        == [-2_000_000, 0])
    guard case .eventRelative(let origin) = preparedGroup.timeBasis else {
        Issue.record("Expected event-relative prepared time basis")
        return
    }
    #expect(origin.tick == .zero)
    #expect(origin.eventDefinitionID.semanticID.canonicalText == "event")
    #expect(origin.scientificAttributes.map(\.key.canonicalText) == ["condition"])
    guard case .eventRelative(let provenanceOriginCell, let sourceOriginTick) =
        prepared.provenance.eventScopeGroups[0].timeBasis else {
        Issue.record("Expected event-relative provenance")
        return
    }
    #expect(provenanceOriginCell == originCell)
    #expect(sourceOriginTick.microseconds == 1_000_000)
}

@Test
func scientificImportNormalizerReportsCheckedRebaseOverflowWithBothCells() throws {
    let staged = try normalizerStaged(
        source: .commaSeparatedValues,
        headers: ["unit", "event"],
        columns: [
            [.text(rawText: "9223372036854.775807")],
            [.text(rawText: "-9223372036854.775808")],
        ]
    )
    let references = try normalizerColumns(2)
    let originCell = try normalizerCell(column: 2, row: 1)
    let group = EventScopeGroupManifestDraft(
        semanticID: try normalizerGroupID("group"),
        spikeTrains: [try normalizerSpike(references[0], id: "unit")],
        eventDefinitions: [try normalizerEvent(references[1], id: "event")],
        timeBasis: .eventRelative(
            origin: StagedEventOccurrenceReference(timestampCell: originCell)
        )
    )
    let plan = try normalizerPlan(staged: staged, groups: [group])

    let error = try #require(normalizerError(plan))
    #expect(error.issues == [
        .eventRelativeRebaseOverflow(
            group: 1,
            valueCell: try normalizerCell(column: 1, row: 1),
            originCell: originCell
        ),
    ])
}

@Test
func scientificImportNormalizerUsesExactEventColumnStateMachine() throws {
    let staged = try normalizerStaged(
        source: .commaSeparatedValues,
        headers: ["unit", "event"],
        columns: [
            [
                .text(rawText: "2"),
                .text(rawText: "1"),
                .blank,
                .blank,
                .blank,
                .blank,
                .blank,
                .blank,
            ],
            [
                .text(rawText: "1"),
                .text(rawText: "@label=alpha=beta"),
                .text(rawText: "@note=two"),
                .blank,
                .text(rawText: "@label=orphan"),
                .text(rawText: " @label=not-metadata"),
                .text(rawText: "2"),
                .text(rawText: "@missing"),
            ],
        ]
    )
    let references = try normalizerColumns(2)
    let group = EventScopeGroupManifestDraft(
        semanticID: try normalizerGroupID("group"),
        spikeTrains: [try normalizerSpike(references[0], id: "unit")],
        eventDefinitions: [try normalizerEvent(references[1], id: "event")],
        timeBasis: .recordingElapsed
    )
    let attributes = [
        try normalizerAttribute(
            "label",
            type: .string,
            role: .scientific,
            unit: .notApplicable,
            empty: .forbid
        ),
        try normalizerAttribute(
            "note",
            type: .string,
            role: .presentation,
            unit: .notApplicable,
            empty: .forbid
        ),
    ]
    let plan = try normalizerPlan(staged: staged, groups: [group], attributes: attributes)

    let error = try #require(normalizerError(plan))
    #expect(error.issues == [
        .eventMetadataWithoutOccurrence(
            group: 1,
            cell: try normalizerCell(column: 2, row: 5)
        ),
        .timestampParseFailed(
            group: 1,
            cell: try normalizerCell(column: 2, row: 6),
            error: .invalidSyntax(utf8Offset: 1, issue: .unexpectedCharacter)
        ),
        .eventMetadataMissingEquals(
            group: 1,
            cell: try normalizerCell(column: 2, row: 8)
        ),
    ])
}

@Test
func scientificImportNormalizerParsesAllAttributeScalarsAndSeparatesRoles() throws {
    let staged = try normalizerStaged(
        source: .commaSeparatedValues,
        headers: ["unit", "event"],
        columns: [
            Array(repeating: .blank, count: 7),
            [
                .text(rawText: "1"),
                .text(rawText: "@label=e\u{301}=tail"),
                .text(rawText: "@count= +001 \t"),
                .text(rawText: "@intensity=001.2300e2"),
                .text(rawText: "@enabled=\ttrue "),
                .text(rawText: "@empty="),
                .text(rawText: "@calibration.Unit=literal"),
            ],
        ]
    )
    let references = try normalizerColumns(2)
    let group = EventScopeGroupManifestDraft(
        semanticID: try normalizerGroupID("group"),
        spikeTrains: [try normalizerSpike(references[0], id: "unit")],
        eventDefinitions: [try normalizerEvent(references[1], id: "event")],
        timeBasis: .recordingElapsed
    )
    let attributes = [
        try normalizerAttribute(
            "label", type: .string, role: .scientific,
            unit: .notApplicable, empty: .forbid
        ),
        try normalizerAttribute(
            "count", type: .integer, role: .presentation,
            unit: .dimensionless, empty: .forbid
        ),
        try normalizerAttribute(
            "intensity", type: .exactDecimal, role: .scientific,
            unit: .dimensionless, empty: .forbid
        ),
        try normalizerAttribute(
            "enabled", type: .boolean, role: .presentation,
            unit: .dimensionless, empty: .forbid
        ),
        try normalizerAttribute(
            "empty", type: .string, role: .scientific,
            unit: .notApplicable, empty: .allowExplicitEmptyString
        ),
        try normalizerAttribute(
            "calibration.Unit", type: .string, role: .presentation,
            unit: .notApplicable, empty: .forbid
        ),
    ]
    let plan = try normalizerPlan(staged: staged, groups: [group], attributes: attributes)

    let prepared = try ScientificImportNormalizer.normalize(resolvedPlan: plan)
    let occurrence = prepared.data.eventScopeGroups[0].eventDefinitions[0].occurrences[0]

    #expect(prepared.data.scientificAttributeDefinitions.map(\.key.canonicalText)
        == ["empty", "intensity", "label"])
    #expect(prepared.data.presentationAttributeDefinitions.map(\.key.canonicalText)
        == ["calibration.Unit", "count", "enabled"])
    #expect(occurrence.scientificAttributes.map(\.key.canonicalText)
        == ["empty", "intensity", "label"])
    #expect(occurrence.presentationAttributes.map(\.key.canonicalText)
        == ["calibration.Unit", "count", "enabled"])

    guard case .string(let empty) = occurrence.scientificAttributes[0].value,
          case .exactDecimal(let intensity) = occurrence.scientificAttributes[1].value,
          case .string(let label) = occurrence.scientificAttributes[2].value,
          case .string(let calibration) = occurrence.presentationAttributes[0].value,
          case .integer(let count) = occurrence.presentationAttributes[1].value,
          case .boolean(let enabled) = occurrence.presentationAttributes[2].value else {
        Issue.record("Expected all four typed event-attribute representations")
        return
    }
    #expect(empty.canonicalText.isEmpty)
    #expect(intensity.canonicalText == "123")
    #expect(label.canonicalText == "é=tail")
    #expect(calibration.canonicalText == "literal")
    #expect(count.canonicalText == "1")
    #expect(enabled)

    let trace = prepared.provenance.eventScopeGroups[0]
        .eventDefinitions[0].occurrences[0].attributes
    #expect(trace.map(\.key.canonicalText)
        == ["calibration.Unit", "count", "empty", "enabled", "intensity", "label"])
    #expect(trace.map(\.valueCell.oneBasedDataRowIndex) == [7, 3, 6, 5, 4, 2])
    #expect(trace.allSatisfy { $0.inlineUnitSuggestion == .absent })
}

@Test
func scientificImportNormalizerReportsAttributeIssuesInDeterministicPhaseOrder() throws {
    let overlongBoolean = String(
        repeating: "x",
        count: ExactIntegerValue.maximumLexemeUTF8ByteCount + 1
    )
    let staged = try normalizerStaged(
        source: .commaSeparatedValues,
        headers: ["unit", "event"],
        columns: [
            [
                .text(rawText: "2"),
                .text(rawText: "1"),
                .blank,
                .blank,
                .blank,
                .blank,
                .blank,
                .blank,
                .blank,
                .blank,
            ],
            [
                .text(rawText: "1"),
                .text(rawText: "@unknown=x"),
                .text(rawText: "@z=first"),
                .text(rawText: "@z=second"),
                .text(rawText: "@a=True"),
                .text(rawText: "@b="),
                .text(rawText: "@c="),
                .text(rawText: "@d="),
                .text(rawText: "@e="),
                .text(rawText: "@f=\(overlongBoolean)"),
            ],
        ]
    )
    let references = try normalizerColumns(2)
    let group = EventScopeGroupManifestDraft(
        semanticID: try normalizerGroupID("group"),
        spikeTrains: [try normalizerSpike(references[0], id: "unit")],
        eventDefinitions: [try normalizerEvent(references[1], id: "event")],
        timeBasis: .recordingElapsed
    )
    let attributes = [
        try normalizerAttribute(
            "z", type: .string, role: .scientific,
            unit: .notApplicable, empty: .forbid
        ),
        try normalizerAttribute(
            "a", type: .boolean, role: .scientific,
            unit: .dimensionless, empty: .forbid
        ),
        try normalizerAttribute(
            "b", type: .integer, role: .scientific,
            unit: .dimensionless, empty: .forbid
        ),
        try normalizerAttribute(
            "c", type: .exactDecimal, role: .scientific,
            unit: .dimensionless, empty: .forbid
        ),
        try normalizerAttribute(
            "d", type: .string, role: .scientific,
            unit: .notApplicable, empty: .forbid
        ),
        try normalizerAttribute(
            "e", type: .boolean, role: .scientific,
            unit: .dimensionless, empty: .forbid
        ),
        try normalizerAttribute(
            "f", type: .boolean, role: .scientific,
            unit: .dimensionless, empty: .forbid
        ),
    ]
    let plan = try normalizerPlan(staged: staged, groups: [group], attributes: attributes)
    let unknown = try EventAttributeKey(validating: "unknown")
    let z = try EventAttributeKey(validating: "z")
    let a = try EventAttributeKey(validating: "a")
    let b = try EventAttributeKey(validating: "b")
    let c = try EventAttributeKey(validating: "c")
    let d = try EventAttributeKey(validating: "d")
    let e = try EventAttributeKey(validating: "e")
    let f = try EventAttributeKey(validating: "f")

    let error = try #require(normalizerError(plan))
    #expect(error.issues == [
        .unknownEventAttributeKey(
            group: 1,
            cell: try normalizerCell(column: 2, row: 2),
            key: unknown
        ),
        .duplicateEventAttributeKey(
            group: 1,
            eventTimestampCell: try normalizerCell(column: 2, row: 1),
            key: z,
            firstCell: try normalizerCell(column: 2, row: 3),
            duplicateCell: try normalizerCell(column: 2, row: 4)
        ),
        .eventAttributeValueInvalid(
            group: 1,
            cell: try normalizerCell(column: 2, row: 5),
            key: a,
            expectedType: .boolean,
            issue: .invalidBoolean
        ),
        .eventAttributeValueInvalid(
            group: 1,
            cell: try normalizerCell(column: 2, row: 6),
            key: b,
            expectedType: .integer,
            issue: .exactNumber(.empty)
        ),
        .eventAttributeValueInvalid(
            group: 1,
            cell: try normalizerCell(column: 2, row: 7),
            key: c,
            expectedType: .exactDecimal,
            issue: .exactNumber(.empty)
        ),
        .eventAttributeValueInvalid(
            group: 1,
            cell: try normalizerCell(column: 2, row: 8),
            key: d,
            expectedType: .string,
            issue: .explicitEmptyStringForbidden
        ),
        .eventAttributeValueInvalid(
            group: 1,
            cell: try normalizerCell(column: 2, row: 9),
            key: e,
            expectedType: .boolean,
            issue: .booleanEmpty
        ),
        .eventAttributeValueInvalid(
            group: 1,
            cell: try normalizerCell(column: 2, row: 10),
            key: f,
            expectedType: .boolean,
            issue: .booleanLexemeTooLong(
                maximumUTF8Bytes: ExactIntegerValue.maximumLexemeUTF8ByteCount
            )
        ),
    ])
}

@Test
func scientificImportNormalizerAcceptsOnlyMatchingNFCInlineUnitAndTracesIt() throws {
    let staged = try normalizerStaged(
        source: .commaSeparatedValues,
        headers: ["unit", "event"],
        columns: [
            [.blank, .blank, .blank],
            [
                .text(rawText: "1"),
                .text(rawText: "@intensity=1.25"),
                .text(rawText: "@intensity.unit=e\u{301}V"),
            ],
        ]
    )
    let references = try normalizerColumns(2)
    let group = EventScopeGroupManifestDraft(
        semanticID: try normalizerGroupID("group"),
        spikeTrains: [try normalizerSpike(references[0], id: "unit")],
        eventDefinitions: [try normalizerEvent(references[1], id: "event")],
        timeBasis: .recordingElapsed
    )
    let confirmedUnit = try OpaqueUnitSymbol(validating: "éV")
    let definition = try normalizerAttribute(
        "intensity",
        type: .exactDecimal,
        role: .scientific,
        unit: .specified(confirmedUnit),
        empty: .forbid
    )
    let plan = try normalizerPlan(
        staged: staged,
        groups: [group],
        attributes: [definition]
    )

    let prepared = try ScientificImportNormalizer.normalize(resolvedPlan: plan)
    let trace = prepared.provenance.eventScopeGroups[0]
        .eventDefinitions[0].occurrences[0].attributes[0]
    #expect(trace.inlineUnitSuggestion == .present(
        symbol: confirmedUnit,
        sourceCell: try normalizerCell(column: 2, row: 3)
    ))
}

@Test
func scientificImportNormalizerLocatesDuplicateOrphanAndInvalidInlineUnits() throws {
    let mW = try OpaqueUnitSymbol(validating: "mW")
    let staged = try normalizerStaged(
        source: .commaSeparatedValues,
        headers: ["unit", "event"],
        columns: [
            Array(repeating: .blank, count: 11),
            [
                .text(rawText: "1"),
                .text(rawText: "@intensity=1"),
                .text(rawText: "@intensity.unit=mW"),
                .text(rawText: "@intensity.unit=mW"),
                .blank,
                .text(rawText: "2"),
                .text(rawText: "@intensity.unit=mW"),
                .blank,
                .text(rawText: "3"),
                .text(rawText: "@intensity=3"),
                .text(rawText: "@intensity.unit="),
            ],
        ]
    )
    let references = try normalizerColumns(2)
    let group = EventScopeGroupManifestDraft(
        semanticID: try normalizerGroupID("group"),
        spikeTrains: [try normalizerSpike(references[0], id: "unit")],
        eventDefinitions: [try normalizerEvent(references[1], id: "event")],
        timeBasis: .recordingElapsed
    )
    let definition = try normalizerAttribute(
        "intensity", type: .exactDecimal, role: .scientific,
        unit: .specified(mW), empty: .forbid
    )
    let plan = try normalizerPlan(
        staged: staged,
        groups: [group],
        attributes: [definition]
    )
    let key = try EventAttributeKey(validating: "intensity")

    let error = try #require(normalizerError(plan))
    #expect(error.issues == [
        .duplicateInlineUnitSuggestion(
            group: 1,
            eventTimestampCell: try normalizerCell(column: 2, row: 1),
            key: key,
            firstCell: try normalizerCell(column: 2, row: 3),
            duplicateCell: try normalizerCell(column: 2, row: 4)
        ),
        .inlineUnitWithoutAttribute(
            group: 1,
            eventTimestampCell: try normalizerCell(column: 2, row: 6),
            key: key,
            unitCell: try normalizerCell(column: 2, row: 7)
        ),
        .inlineUnitSuggestionInvalid(
            group: 1,
            cell: try normalizerCell(column: 2, row: 11),
            key: key,
            error: .empty
        ),
    ])
}

@Test
func scientificImportNormalizerRejectsDefinitionAndGlobalInlineUnitConflicts() throws {
    let mW = try OpaqueUnitSymbol(validating: "mW")
    let uW = try OpaqueUnitSymbol(validating: "uW")
    let staged = try normalizerStaged(
        source: .commaSeparatedValues,
        headers: ["unit", "event"],
        columns: [
            Array(repeating: .blank, count: 7),
            [
                .text(rawText: "1"),
                .text(rawText: "@intensity=1"),
                .text(rawText: "@intensity.unit=mW"),
                .blank,
                .text(rawText: "2"),
                .text(rawText: "@intensity=2"),
                .text(rawText: "@intensity.unit=uW"),
            ],
        ]
    )
    let references = try normalizerColumns(2)
    let group = EventScopeGroupManifestDraft(
        semanticID: try normalizerGroupID("group"),
        spikeTrains: [try normalizerSpike(references[0], id: "unit")],
        eventDefinitions: [try normalizerEvent(references[1], id: "event")],
        timeBasis: .recordingElapsed
    )
    let definition = try normalizerAttribute(
        "intensity", type: .exactDecimal, role: .scientific,
        unit: .specified(mW), empty: .forbid
    )
    let plan = try normalizerPlan(
        staged: staged,
        groups: [group],
        attributes: [definition]
    )
    let key = try EventAttributeKey(validating: "intensity")

    let error = try #require(normalizerError(plan))
    #expect(error.issues == [
        .inlineUnitConflictsWithDefinition(
            group: 1,
            cell: try normalizerCell(column: 2, row: 7),
            key: key,
            suggested: uW,
            confirmed: .specified(mW)
        ),
        .inlineUnitSuggestionsConflict(
            key: key,
            firstCell: try normalizerCell(column: 2, row: 3),
            firstUnit: mW,
            conflictingCell: try normalizerCell(column: 2, row: 7),
            conflictingUnit: uW
        ),
    ])
}

@Test
func scientificImportNormalizerRejectsInlineUnitsForNonSpecifiedDefinitions() throws {
    let one = try OpaqueUnitSymbol(validating: "1")
    let staged = try normalizerStaged(
        source: .commaSeparatedValues,
        headers: ["unit", "event"],
        columns: [
            Array(repeating: .blank, count: 5),
            [
                .text(rawText: "1"),
                .text(rawText: "@category=A"),
                .text(rawText: "@category.unit=1"),
                .text(rawText: "@ratio=0.5"),
                .text(rawText: "@ratio.unit=1"),
            ],
        ]
    )
    let references = try normalizerColumns(2)
    let group = EventScopeGroupManifestDraft(
        semanticID: try normalizerGroupID("group"),
        spikeTrains: [try normalizerSpike(references[0], id: "unit")],
        eventDefinitions: [try normalizerEvent(references[1], id: "event")],
        timeBasis: .recordingElapsed
    )
    let definitions = [
        try normalizerAttribute(
            "category", type: .string, role: .scientific,
            unit: .notApplicable, empty: .forbid
        ),
        try normalizerAttribute(
            "ratio", type: .exactDecimal, role: .scientific,
            unit: .dimensionless, empty: .forbid
        ),
    ]
    let plan = try normalizerPlan(
        staged: staged,
        groups: [group],
        attributes: definitions
    )

    let error = try #require(normalizerError(plan))
    #expect(error.issues == [
        .inlineUnitConflictsWithDefinition(
            group: 1,
            cell: try normalizerCell(column: 2, row: 3),
            key: try EventAttributeKey(validating: "category"),
            suggested: one,
            confirmed: .notApplicable
        ),
        .inlineUnitConflictsWithDefinition(
            group: 1,
            cell: try normalizerCell(column: 2, row: 5),
            key: try EventAttributeKey(validating: "ratio"),
            suggested: one,
            confirmed: .dimensionless
        ),
    ])
}

@Test
func scientificImportNormalizerUsesOnlyScientificAttributesToDisambiguateOrigin() throws {
    let staged = try normalizerStaged(
        source: .commaSeparatedValues,
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
    let references = try normalizerColumns(2)
    let originCell = try normalizerCell(column: 2, row: 1)
    let group = EventScopeGroupManifestDraft(
        semanticID: try normalizerGroupID("group"),
        spikeTrains: [try normalizerSpike(references[0], id: "unit")],
        eventDefinitions: [try normalizerEvent(references[1], id: "event")],
        timeBasis: .eventRelative(
            origin: StagedEventOccurrenceReference(timestampCell: originCell)
        )
    )
    let scientificDefinition = try normalizerAttribute(
        "condition", type: .string, role: .scientific,
        unit: .notApplicable, empty: .forbid
    )
    let scientificPlan = try normalizerPlan(
        staged: staged,
        groups: [group],
        attributes: [scientificDefinition]
    )

    let prepared = try ScientificImportNormalizer.normalize(resolvedPlan: scientificPlan)
    let occurrences = prepared.data.eventScopeGroups[0].eventDefinitions[0].occurrences
    #expect(occurrences.count == 2)
    #expect(occurrences.map(\.tick) == [.zero, .zero])
    guard case .eventRelative(let origin) = prepared.data.eventScopeGroups[0].timeBasis,
          case .string(let originCondition) = origin.scientificAttributes[0].value else {
        Issue.record("Expected a scientific-attribute-disambiguated origin")
        return
    }
    #expect(originCondition.canonicalText == "A")

    let presentationDefinition = try normalizerAttribute(
        "condition", type: .string, role: .presentation,
        unit: .notApplicable, empty: .forbid
    )
    let presentationPlan = try normalizerPlan(
        staged: staged,
        groups: [group],
        attributes: [presentationDefinition]
    )
    let presentationError = try #require(normalizerError(presentationPlan))
    #expect(presentationError.issues == [
        .selectedOriginAmbiguous(
            group: 1,
            eventDefinitionID: ScientificEventDefinitionID(
                try ScientificSemanticID(validating: "event")
            ),
            cell: originCell,
            multiplicity: 2
        ),
    ])
}

@Test
func scientificImportNormalizerCountsAdjacentDescentsAndStableSortsByTickThenRow() throws {
    let staged = try normalizerStaged(
        source: .commaSeparatedValues,
        headers: ["unit"],
        columns: [[
            .text(rawText: "3"),
            .text(rawText: "2"),
            .text(rawText: "1"),
        ]]
    )
    let preservePlan = try normalizerSingleSpikePlan(staged: staged)
    let preserveError = try #require(normalizerError(preservePlan))
    #expect(preserveError.issues == [
        .sourceOrderNotAscending(
            group: 1,
            column: try StagedSourceColumnReference(oneBasedIndex: 1),
            descentCount: 2,
            firstPreviousCell: try normalizerCell(column: 1, row: 1),
            firstCurrentCell: try normalizerCell(column: 1, row: 2)
        ),
    ])

    let sortedPlan = try normalizerSingleSpikePlan(
        staged: staged,
        order: .stableAscendingSort
    )
    let sorted = try ScientificImportNormalizer.normalize(resolvedPlan: sortedPlan)
    let train = sorted.data.eventScopeGroups[0].spikeTrains[0]
    let trace = sorted.provenance.eventScopeGroups[0].spikeTrains[0]
    #expect(train.timestamps.map(\.microseconds) == [1_000_000, 2_000_000, 3_000_000])
    #expect(trace.sourceOrderDescentCount == 2)
    #expect(trace.timestampSources.map { $0[0].oneBasedDataRowIndex } == [3, 2, 1])

    let tiedStaged = try normalizerStaged(
        source: .commaSeparatedValues,
        headers: ["unit"],
        columns: [[
            .text(rawText: "2"),
            .text(rawText: "1"),
            .text(rawText: "1"),
        ]]
    )
    let tiedPlan = try normalizerSingleSpikePlan(
        staged: tiedStaged,
        order: .stableAscendingSort
    )
    let tied = try ScientificImportNormalizer.normalize(resolvedPlan: tiedPlan)
    let tiedTrace = tied.provenance.eventScopeGroups[0].spikeTrains[0]
    #expect(tiedTrace.sourceOrderDescentCount == 1)
    #expect(tiedTrace.timestampSources.map { $0[0].oneBasedDataRowIndex } == [2, 3, 1])
}

@Test
func scientificImportNormalizerStableSortsEventsWithoutCollapsingEqualTicks() throws {
    let staged = try normalizerStaged(
        source: .commaSeparatedValues,
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
    let references = try normalizerColumns(2)
    let group = EventScopeGroupManifestDraft(
        semanticID: try normalizerGroupID("group"),
        spikeTrains: [try normalizerSpike(references[0], id: "unit")],
        eventDefinitions: [
            try normalizerEvent(
                references[1],
                id: "event",
                order: .stableAscendingSort
            ),
        ],
        timeBasis: .recordingElapsed
    )
    let label = try normalizerAttribute(
        "label", type: .string, role: .scientific,
        unit: .notApplicable, empty: .forbid
    )
    let plan = try normalizerPlan(
        staged: staged,
        groups: [group],
        attributes: [label]
    )

    let prepared = try ScientificImportNormalizer.normalize(resolvedPlan: plan)
    let occurrences = prepared.data.eventScopeGroups[0].eventDefinitions[0].occurrences
    let trace = prepared.provenance.eventScopeGroups[0].eventDefinitions[0]
    #expect(occurrences.map(\.tick.microseconds) == [1_000_000, 1_000_000, 2_000_000])
    #expect(trace.sourceOrderDescentCount == 1)
    #expect(trace.occurrences.map(\.timestampCell.oneBasedDataRowIndex) == [3, 5, 1])

    var labels: [String] = []
    for occurrence in occurrences {
        guard case .string(let value) = occurrence.scientificAttributes[0].value else {
            Issue.record("Expected event label string")
            return
        }
        labels.append(value.canonicalText)
    }
    #expect(labels == ["first-tie", "second-tie", "late"])
}

@Test
func scientificImportNormalizerRetainsCanonicalRawMultiplicityAndRecordsAnalysisPolicy() throws {
    let staged = try normalizerStaged(
        source: .commaSeparatedValues,
        headers: ["unit"],
        columns: [[
            .text(rawText: "1"),
            .text(rawText: "1"),
            .text(rawText: "1.000001"),
        ]]
    )
    let collapsePlan = try normalizerSingleSpikePlan(
        staged: staged,
        order: .stableAscendingSort,
        duplicates: .collapseExact
    )
    let collapseRequested = try ScientificImportNormalizer.normalize(resolvedPlan: collapsePlan)
    let retainedTrain = collapseRequested.data.eventScopeGroups[0].spikeTrains[0]
    let retainedTrace = collapseRequested.provenance.eventScopeGroups[0].spikeTrains[0]
    #expect(retainedTrain.timestamps.map(\.microseconds)
        == [1_000_000, 1_000_000, 1_000_001])
    #expect(retainedTrace.timestampSources == [
        [try normalizerCell(column: 1, row: 1)],
        [try normalizerCell(column: 1, row: 2)],
        [try normalizerCell(column: 1, row: 3)],
    ])
    #expect(retainedTrace.duplicateDecision == .collapseExact)

    for mode in [
        ScientificDatasetActivityMode.intentionalMultiUnit,
        .unknownOrUncertain,
    ] {
        let preservePlan = try normalizerSingleSpikePlan(
            staged: staged,
            mode: mode,
            order: .stableAscendingSort,
            duplicates: .preserveMultiplicity
        )
        let preserved = try ScientificImportNormalizer.normalize(resolvedPlan: preservePlan)
        #expect(preserved.data.eventScopeGroups[0].spikeTrains[0].timestamps.count == 3)
        #expect(preserved.data.activityMode == mode)
    }
}

@Test
func scientificImportNormalizerDefensivelyRejectsForgedNonSingleUnitCollapsePlan() throws {
    let staged = try normalizerStaged(
        source: .commaSeparatedValues,
        headers: ["unit"],
        columns: [[.text(rawText: "1"), .text(rawText: "1")]]
    )
    let singleUnitPlan = try normalizerSingleSpikePlan(
        staged: staged,
        duplicates: .collapseExact
    )
    let forgedPlan = ResolvedScientificImportPlan(
        source: singleUnitPlan.source,
        recordingSegment: singleUnitPlan.recordingSegment,
        sourceTimeUnit: singleUnitPlan.sourceTimeUnit,
        activityMode: .intentionalMultiUnit,
        eventScopeGroups: singleUnitPlan.eventScopeGroups,
        eventAttributeDefinitions: singleUnitPlan.eventAttributeDefinitions
    )

    let error = try #require(normalizerError(forgedPlan))
    #expect(error.issues == [
        .duplicateCollapseNotAllowed(
            activityMode: .intentionalMultiUnit,
            group: 1,
            column: try StagedSourceColumnReference(oneBasedIndex: 1)
        ),
    ])
}

@Test
func scientificImportNormalizerCapsDiagnosticsDeterministically() throws {
    let count = ScientificImportNormalizer.maximumReportedIssueCount + 3
    let staged = try normalizerStaged(
        source: .commaSeparatedValues,
        headers: ["unit"],
        columns: [Array(repeating: .text(rawText: ""), count: count)]
    )
    let plan = try normalizerSingleSpikePlan(staged: staged)

    let first = try #require(normalizerError(plan))
    let second = try #require(normalizerError(plan))
    #expect(first == second)
    #expect(first.issues.count == ScientificImportNormalizer.maximumReportedIssueCount)
    #expect(first.additionalIssueCount == 3)
    #expect(first.issues.first == .timestampParseFailed(
        group: 1,
        cell: try normalizerCell(column: 1, row: 1),
        error: .empty
    ))
    #expect(first.issues.last == .timestampParseFailed(
        group: 1,
        cell: try normalizerCell(
            column: 1,
            row: ScientificImportNormalizer.maximumReportedIssueCount
        ),
        error: .empty
    ))
}

@Test
func scientificImportNormalizerCapsNumericDiagnosticsInSourceOrder() throws {
    let count = ScientificImportNormalizer.maximumReportedIssueCount + 3
    let staged = try normalizerStaged(
        source: .excelWorkbook(worksheetName: "Recording"),
        headers: ["unit"],
        columns: [Array(
            repeating: .spreadsheetNumber(rawLexeme: "0.0000005"),
            count: count
        )]
    )
    let plan = try normalizerSingleSpikePlan(staged: staged)

    let error = try #require(normalizerError(plan))
    #expect(error.issues.count == ScientificImportNormalizer.maximumReportedIssueCount)
    #expect(error.additionalIssueCount == 3)
    #expect(error.issues.first == .spreadsheetNumberTimestampDecodeFailed(
        group: 1,
        cell: try normalizerCell(column: 1, row: 1),
        error: .noWholeMicrosecondRoundTrip
    ))
    #expect(error.issues.last == .spreadsheetNumberTimestampDecodeFailed(
        group: 1,
        cell: try normalizerCell(
            column: 1,
            row: ScientificImportNormalizer.maximumReportedIssueCount
        ),
        error: .noWholeMicrosecondRoundTrip
    ))
}

private enum NormalizerFixtureError: Error {
    case mismatchedColumnMetadata
}

private func normalizerStaged(
    source: StagedTabularSource,
    headers: [String],
    columns: [[StagedCellValue]],
    sourceTransactionBinding: StagedSourceTransactionBinding? = nil
) throws -> StagedScientificImport {
    guard headers.count == columns.count else {
        throw NormalizerFixtureError.mismatchedColumnMetadata
    }
    var stagedColumns: [StagedScientificColumn] = []
    stagedColumns.reserveCapacity(columns.count)
    for offset in columns.indices {
        let reference = try StagedSourceColumnReference(oneBasedIndex: offset + 1)
        stagedColumns.append(
            StagedScientificColumn(
                sourceColumn: reference,
                header: headers[offset],
                cells: columns[offset]
            )
        )
    }
    if let sourceTransactionBinding {
        return try StagedScientificImport(
            source: source,
            columns: stagedColumns,
            suggestions: .none,
            sourceTransactionBinding: sourceTransactionBinding
        )
    }
    return try StagedScientificImport(
        source: source,
        columns: stagedColumns,
        suggestions: .none
    )
}

private func normalizerPlan(
    staged: StagedScientificImport,
    unit: SpikeTimeUnit = .seconds,
    mode: ScientificDatasetActivityMode = .putativeSingleUnit,
    groups: [EventScopeGroupManifestDraft],
    attributes: [EventAttributeDefinitionDraft] = []
) throws -> ResolvedScientificImportPlan {
    let draft = ScientificImportManifestDraft(
        boundTo: staged,
        sourceTimeUnit: unit,
        activityMode: mode,
        recordingSegmentID: testSegmentID(),
        recordingRegime: .continuousUntrialed,
        importedExcerptCoverage: .allSpikeTrainsFullImportedExcerpt,
        observationBoundsAvailability: .unknownOrUnavailable,
        eventScopeGroups: groups,
        eventAttributeDefinitions: attributes
    )
    return try ScientificImportPlanResolver.resolve(stagedImport: staged, draft: draft)
}

private func normalizerSingleSpikePlan(
    staged: StagedScientificImport,
    unit: SpikeTimeUnit = .seconds,
    mode: ScientificDatasetActivityMode = .putativeSingleUnit,
    order: TimestampOrderDecision = .preserveSourceOrder,
    duplicates: ExactDuplicateDecision = .preserveMultiplicity
) throws -> ResolvedScientificImportPlan {
    let column = try StagedSourceColumnReference(oneBasedIndex: 1)
    let group = EventScopeGroupManifestDraft(
        semanticID: try normalizerGroupID("group"),
        spikeTrains: [
            try normalizerSpike(
                column,
                id: "unit",
                order: order,
                duplicates: duplicates
            ),
        ],
        eventDefinitions: [],
        timeBasis: .recordingElapsed
    )
    return try normalizerPlan(staged: staged, unit: unit, mode: mode, groups: [group])
}

private func normalizerColumns(_ count: Int) throws -> [StagedSourceColumnReference] {
    var references: [StagedSourceColumnReference] = []
    references.reserveCapacity(count)
    for index in 1...count {
        references.append(try StagedSourceColumnReference(oneBasedIndex: index))
    }
    return references
}

private func normalizerCell(
    column: Int,
    row: Int
) throws -> StagedSourceCellReference {
    try StagedSourceCellReference(
        column: StagedSourceColumnReference(oneBasedIndex: column),
        oneBasedDataRowIndex: row
    )
}

private func normalizerSpike(
    _ column: StagedSourceColumnReference,
    id: String,
    order: TimestampOrderDecision = .preserveSourceOrder,
    duplicates: ExactDuplicateDecision = .preserveMultiplicity
) throws -> SpikeTrainColumnManifestDraft {
    SpikeTrainColumnManifestDraft(
        sourceColumn: column,
        semanticID: ScientificSpikeTrainID(try ScientificSemanticID(validating: id)),
        orderDecision: order,
        duplicateDecision: duplicates
    )
}

private func normalizerEvent(
    _ column: StagedSourceColumnReference,
    id: String,
    type: String = "stimulus",
    order: TimestampOrderDecision = .preserveSourceOrder
) throws -> EventDefinitionColumnManifestDraft {
    EventDefinitionColumnManifestDraft(
        sourceColumn: column,
        semanticID: ScientificEventDefinitionID(try ScientificSemanticID(validating: id)),
        eventTypeID: ScientificEventTypeID(try ScientificSemanticID(validating: type)),
        orderDecision: order
    )
}

private func normalizerGroupID(_ text: String) throws -> ScientificEventScopeGroupID {
    ScientificEventScopeGroupID(try ScientificSemanticID(validating: text))
}

private func normalizerAttribute(
    _ key: String,
    type: EventAttributeScalarType,
    role: EventAttributeRole,
    unit: ConfirmedEventAttributeUnit,
    empty: EventEmptyStringPolicy
) throws -> EventAttributeDefinitionDraft {
    EventAttributeDefinitionDraft(
        key: try EventAttributeKey(validating: key),
        scalarType: type,
        role: role,
        unit: unit,
        emptyStringPolicy: empty
    )
}

private func normalizerError(
    _ plan: ResolvedScientificImportPlan
) -> ScientificImportNormalizationError? {
    do {
        _ = try ScientificImportNormalizer.normalize(resolvedPlan: plan)
        Issue.record("Expected scientific import normalization to fail")
        return nil
    } catch let error as ScientificImportNormalizationError {
        return error
    } catch {
        Issue.record("Unexpected scientific import normalization error: \(error)")
        return nil
    }
}
