@testable import STPDCore
import Testing

@Test
func canonicalProjectorRequiresExactSourceTransactionBinding() throws {
    let staged = try projectorStaged(
        headers: ["unit"],
        columns: [[.text(rawText: "1")]],
        includeSourceBinding: false
    )
    let column = try projectorColumn(1)
    let plan = try projectorPlan(
        staged: staged,
        groups: [
            EventScopeGroupManifestDraft(
                semanticID: try projectorGroupID("group"),
                spikeTrains: [try projectorSpike(column, id: "unit")],
                eventDefinitions: [],
                timeBasis: .recordingElapsed
            ),
        ]
    )
    let prepared = try ScientificImportNormalizer.normalize(resolvedPlan: plan)
    let validated = try projectorValidated(prepared)

    #expect(throws: CanonicalScientificImportProjectionError.missingSourceTransactionBinding) {
        _ = try CanonicalScientificImportProjector.project(validated)
    }
}

@Test
func canonicalProjectorPreservesRawDuplicatesAndStripsPresentation() throws {
    let staged = try projectorStaged(
        headers: ["unit", "event"],
        columns: [
            [.text(rawText: "1"), .text(rawText: "1"), .text(rawText: "2")],
            [
                .text(rawText: "0.5"),
                .text(rawText: "@z_scientific=7"),
                .text(rawText: "@a_presentation=caption"),
            ],
        ]
    )
    let spikeColumn = try projectorColumn(1)
    let eventColumn = try projectorColumn(2)
    let plan = try projectorPlan(
        staged: staged,
        groups: [
            EventScopeGroupManifestDraft(
                semanticID: try projectorGroupID("group"),
                spikeTrains: [
                    try projectorSpike(
                        spikeColumn,
                        id: "unit",
                        duplicates: .collapseExact
                    ),
                ],
                eventDefinitions: [try projectorEvent(eventColumn, id: "event")],
                timeBasis: .recordingElapsed
            ),
        ],
        attributes: [
            try projectorAttribute(
                "a_presentation",
                type: .string,
                role: .presentation,
                unit: .notApplicable
            ),
            try projectorAttribute(
                "z_scientific",
                type: .integer,
                role: .scientific,
                unit: .dimensionless
            ),
        ]
    )
    let prepared = try ScientificImportNormalizer.normalize(resolvedPlan: plan)
    let validated = try projectorValidated(prepared)

    let shadow = try CanonicalScientificImportProjector.project(validated)
    let train = shadow.dataset.spikeTrains[0]
    #expect(train.rawTimestamps.map(\.microseconds) == [1_000_000, 1_000_000, 2_000_000])
    #expect(shadow.dataset.eventScopeGroups[0].spikeTrainReferences
        .map(\.semanticID.canonicalText) == ["unit"])
    #expect(shadow.dataset.scientificAttributeDefinitions.map(\.key.canonicalText)
        == ["z_scientific"])

    let occurrence = shadow.dataset.eventScopeGroups[0].eventDefinitions[0].occurrences[0]
    #expect(occurrence.scientificAttributes.map(\.key.canonicalText) == ["z_scientific"])
    guard case .integer(let integer) = occurrence.scientificAttributes[0].value else {
        Issue.record("Expected the scientific integer attribute")
        return
    }
    #expect(integer.canonicalText == "7")
    #expect(shadow.sourceTransactionBinding == staged.sourceTransactionBinding)
}

@Test
func canonicalProjectorOrdersCollectionsByConfirmedSemanticID() throws {
    let staged = try projectorStaged(
        headers: ["z_train", "a_train", "z_event", "a_event", "m_train"],
        columns: [
            [.text(rawText: "1")],
            [.text(rawText: "2")],
            [.text(rawText: "3")],
            [.text(rawText: "4")],
            [.text(rawText: "5")],
        ]
    )
    let columns = try (1...5).map(projectorColumn)
    let plan = try projectorPlan(
        staged: staged,
        groups: [
            EventScopeGroupManifestDraft(
                semanticID: try projectorGroupID("z_group"),
                spikeTrains: [
                    try projectorSpike(columns[0], id: "z_train"),
                    try projectorSpike(columns[1], id: "a_train"),
                ],
                eventDefinitions: [
                    try projectorEvent(columns[2], id: "z_event"),
                    try projectorEvent(columns[3], id: "a_event"),
                ],
                timeBasis: .recordingElapsed
            ),
            EventScopeGroupManifestDraft(
                semanticID: try projectorGroupID("a_group"),
                spikeTrains: [try projectorSpike(columns[4], id: "m_train")],
                eventDefinitions: [],
                timeBasis: .recordingElapsed
            ),
        ]
    )
    let prepared = try ScientificImportNormalizer.normalize(resolvedPlan: plan)
    let validated = try projectorValidated(prepared)

    let dataset = try CanonicalScientificImportProjector.project(validated).dataset
    // The spike-train registry is dataset-global and globally ordered across all groups.
    #expect(dataset.spikeTrains.map {
        $0.semanticID.semanticID.canonicalText
    } == ["a_train", "m_train", "z_train"])
    #expect(dataset.eventScopeGroups.map {
        $0.semanticID.semanticID.canonicalText
    } == ["a_group", "z_group"])

    let aGroup = dataset.eventScopeGroups[0]
    #expect(aGroup.spikeTrainReferences.map(\.semanticID.canonicalText) == ["m_train"])

    let zGroup = dataset.eventScopeGroups[1]
    #expect(zGroup.spikeTrainReferences.map(\.semanticID.canonicalText) == ["a_train", "z_train"])
    #expect(zGroup.eventDefinitions.map {
        $0.semanticID.semanticID.canonicalText
    } == ["a_event", "z_event"])
}

@Test
func canonicalProjectorExcludesVirtualDuplicatePolicyFromCanonicalRaw() throws {
    let staged = try projectorStaged(
        headers: ["unit"],
        columns: [[
            .text(rawText: "1"),
            .text(rawText: "1"),
            .text(rawText: "2"),
        ]]
    )
    let column = try projectorColumn(1)

    func project(_ duplicates: ExactDuplicateDecision) throws
        -> ShadowCanonicalScientificImport {
        let plan = try projectorPlan(
            staged: staged,
            groups: [
                EventScopeGroupManifestDraft(
                    semanticID: try projectorGroupID("group"),
                    spikeTrains: [
                        try projectorSpike(column, id: "unit", duplicates: duplicates),
                    ],
                    eventDefinitions: [],
                    timeBasis: .recordingElapsed
                ),
            ]
        )
        return try CanonicalScientificImportProjector.project(
            projectorValidated(
                ScientificImportNormalizer.normalize(resolvedPlan: plan)
            )
        )
    }

    let preserved = try project(.preserveMultiplicity)
    let virtualCollapseRequested = try project(.collapseExact)

    #expect(preserved.dataset == virtualCollapseRequested.dataset)
    #expect(preserved == virtualCollapseRequested)
    #expect(preserved.fingerprint == virtualCollapseRequested.fingerprint)
    #expect(preserved.dataset.spikeTrains[0]
        .rawTimestamps.map(\.microseconds) == [1_000_000, 1_000_000, 2_000_000])
}

@Test
func canonicalProjectorMakesCSVSecondsAndXLSXMillisecondsScientificallyEquivalent() throws {
    let csv = try projectorStaged(
        source: .commaSeparatedValues,
        headers: ["unit"],
        columns: [[.text(rawText: "1.000000"), .text(rawText: "1.250000")]],
        bindingDigestCharacter: "a"
    )
    let workbook = try projectorStaged(
        source: .excelWorkbook(worksheetName: "Recording"),
        headers: ["unit"],
        columns: [[
            .spreadsheetNumber(rawLexeme: "1000.000"),
            .spreadsheetNumber(rawLexeme: "1250.000"),
        ]],
        bindingDigestCharacter: "b"
    )
    let column = try projectorColumn(1)

    func project(
        _ staged: StagedScientificImport,
        unit: SpikeTimeUnit
    ) throws -> ShadowCanonicalScientificImport {
        let plan = try projectorPlan(
            staged: staged,
            groups: [
                EventScopeGroupManifestDraft(
                    semanticID: try projectorGroupID("group"),
                    spikeTrains: [try projectorSpike(column, id: "unit")],
                    eventDefinitions: [],
                    timeBasis: .recordingElapsed
                ),
            ],
            sourceTimeUnit: unit
        )
        return try CanonicalScientificImportProjector.project(
            projectorValidated(
                ScientificImportNormalizer.normalize(resolvedPlan: plan)
            )
        )
    }

    let csvShadow = try project(csv, unit: .seconds)
    let workbookShadow = try project(workbook, unit: .milliseconds)

    #expect(csvShadow.dataset == workbookShadow.dataset)
    // Equivalent CSV-seconds and XLSX-milliseconds imports share one canonical fingerprint even
    // though their source transaction bindings differ.
    #expect(csvShadow.fingerprint == workbookShadow.fingerprint)
    #expect(csvShadow.sourceTransactionBinding != workbookShadow.sourceTransactionBinding)
    #expect(csvShadow != workbookShadow)
}

@Test
func canonicalProjectorExcludesPresentationChangesButIncludesActivityMode() throws {
    func project(
        presentationText: String,
        mode: ScientificDatasetActivityMode,
        digestCharacter: Character
    ) throws -> CanonicalScientificDataset {
        let staged = try projectorStaged(
            headers: ["unit", "event"],
            columns: [
                [.text(rawText: "1"), .blank, .blank],
                [
                    .text(rawText: "0.5"),
                    .text(rawText: "@scientific=7"),
                    .text(rawText: "@presentation=\(presentationText)"),
                ],
            ],
            bindingDigestCharacter: digestCharacter
        )
        let plan = try projectorPlan(
            staged: staged,
            groups: [
                EventScopeGroupManifestDraft(
                    semanticID: try projectorGroupID("group"),
                    spikeTrains: [try projectorSpike(projectorColumn(1), id: "unit")],
                    eventDefinitions: [try projectorEvent(projectorColumn(2), id: "event")],
                    timeBasis: .recordingElapsed
                ),
            ],
            attributes: [
                try projectorAttribute(
                    "presentation",
                    type: .string,
                    role: .presentation,
                    unit: .notApplicable
                ),
                try projectorAttribute(
                    "scientific",
                    type: .integer,
                    role: .scientific,
                    unit: .dimensionless
                ),
            ],
            activityMode: mode
        )
        let prepared = try ScientificImportNormalizer.normalize(resolvedPlan: plan)
        return try CanonicalScientificImportProjector.project(
            projectorValidated(prepared)
        ).dataset
    }

    let first = try project(
        presentationText: "first caption",
        mode: .putativeSingleUnit,
        digestCharacter: "a"
    )
    let presentationChanged = try project(
        presentationText: "different caption",
        mode: .putativeSingleUnit,
        digestCharacter: "b"
    )
    let activityModeChanged = try project(
        presentationText: "first caption",
        mode: .intentionalMultiUnit,
        digestCharacter: "c"
    )

    #expect(first == presentationChanged)
    #expect(first != activityModeChanged)
}

@Test
func canonicalProjectorPreservesExplicitEventRelativeNegativeTimeBasis() throws {
    let staged = try projectorStaged(
        headers: ["unit", "event"],
        columns: [
            [.text(rawText: "-1"), .text(rawText: "1")],
            [.text(rawText: "0"), .blank],
        ]
    )
    let originReference = StagedEventOccurrenceReference(
        timestampCell: try projectorCell(column: 2, row: 1)
    )
    let plan = try projectorPlan(
        staged: staged,
        groups: [
            EventScopeGroupManifestDraft(
                semanticID: try projectorGroupID("group"),
                spikeTrains: [try projectorSpike(projectorColumn(1), id: "unit")],
                eventDefinitions: [try projectorEvent(projectorColumn(2), id: "event")],
                timeBasis: .eventRelative(origin: originReference)
            ),
        ]
    )
    let prepared = try ScientificImportNormalizer.normalize(resolvedPlan: plan)
    let dataset = try CanonicalScientificImportProjector.project(
        projectorValidated(prepared)
    ).dataset
    let group = dataset.eventScopeGroups[0]

    #expect(dataset.spikeTrains[0].rawTimestamps.map(\.microseconds)
        == [-1_000_000, 1_000_000])
    #expect(group.spikeTrainReferences.map(\.semanticID.canonicalText) == ["unit"])
    guard case .eventRelative(let origin) = group.timeBasis else {
        Issue.record("Expected the explicitly confirmed event-relative time basis")
        return
    }
    #expect(origin.eventDefinitionID.semanticID.canonicalText == "event")
    #expect(origin.tick == .zero)
}

@Test
func canonicalProjectorRemovesSourceRowOrderFromSameTickScientificEvents() throws {
    let csv = try projectorStaged(
        source: .commaSeparatedValues,
        headers: ["unit", "event"],
        columns: [
            [.text(rawText: "0"), .blank, .blank, .blank],
            [
                .text(rawText: "1"),
                .text(rawText: "@condition=B"),
                .text(rawText: "1"),
                .text(rawText: "@condition=A"),
            ],
        ],
        bindingDigestCharacter: "a"
    )
    let workbook = try projectorStaged(
        source: .excelWorkbook(worksheetName: "Recording"),
        headers: ["unit", "event"],
        columns: [
            [.spreadsheetNumber(rawLexeme: "0"), .blank, .blank, .blank],
            [
                .spreadsheetNumber(rawLexeme: "1"),
                .text(rawText: "@condition=A"),
                .spreadsheetNumber(rawLexeme: "1"),
                .text(rawText: "@condition=B"),
            ],
        ],
        bindingDigestCharacter: "b"
    )

    func project(_ staged: StagedScientificImport) throws -> CanonicalScientificDataset {
        let plan = try projectorPlan(
            staged: staged,
            groups: [
                EventScopeGroupManifestDraft(
                    semanticID: try projectorGroupID("group"),
                    spikeTrains: [try projectorSpike(projectorColumn(1), id: "unit")],
                    eventDefinitions: [try projectorEvent(projectorColumn(2), id: "event")],
                    timeBasis: .recordingElapsed
                ),
            ],
            attributes: [
                try projectorAttribute(
                    "condition",
                    type: .string,
                    role: .scientific,
                    unit: .notApplicable
                ),
            ]
        )
        let prepared = try ScientificImportNormalizer.normalize(resolvedPlan: plan)
        return try CanonicalScientificImportProjector.project(
            projectorValidated(prepared)
        ).dataset
    }

    let csvDataset = try project(csv)
    let workbookDataset = try project(workbook)
    #expect(csvDataset == workbookDataset)

    let occurrences = csvDataset.eventScopeGroups[0].eventDefinitions[0].occurrences
    let orderedConditions = occurrences.compactMap { occurrence -> String? in
        guard let attribute = occurrence.scientificAttributes.first,
              case .string(let value) = attribute.value else {
            return nil
        }
        return value.canonicalText
    }
    #expect(orderedConditions == ["A", "B"])
}

private enum CanonicalProjectorFixtureError: Error {
    case validationRejected
}

private func projectorValidated(
    _ prepared: PreparedScientificImport
) throws -> CanonicalProjectionValidatedImport {
    switch PreparedScientificImportValidator.validateForCanonicalProjection(prepared) {
    case .accepted(let validated):
        return validated
    case .rejected(let report):
        Issue.record("Expected canonical-projection validation to pass: \(report)")
        throw CanonicalProjectorFixtureError.validationRejected
    }
}

private func projectorStaged(
    source: StagedTabularSource = .commaSeparatedValues,
    headers: [String],
    columns: [[StagedCellValue]],
    includeSourceBinding: Bool = true,
    bindingDigestCharacter: Character = "a"
) throws -> StagedScientificImport {
    let stagedColumns = try columns.indices.map { offset in
        StagedScientificColumn(
            sourceColumn: try projectorColumn(offset + 1),
            header: headers[offset],
            cells: columns[offset]
        )
    }
    guard includeSourceBinding else {
        return try StagedScientificImport(
            source: source,
            columns: stagedColumns,
            suggestions: .none
        )
    }
    let selection: StagedSourceTransactionSelection
    switch source {
    case .commaSeparatedValues:
        selection = .commaSeparatedValues
    case .excelWorkbook(let worksheetName):
        selection = .excelWorksheet(
            worksheetName: worksheetName,
            sheetID: 1,
            relationshipID: "rId1",
            normalizedPartPath: "xl/worksheets/sheet1.xml"
        )
    }
    return try StagedScientificImport(
        source: source,
        columns: stagedColumns,
        suggestions: .none,
        sourceTransactionBinding: StagedSourceTransactionBinding(
            sourceBytesSHA256: String(repeating: bindingDigestCharacter, count: 64),
            selection: selection
        )
    )
}

private func projectorPlan(
    staged: StagedScientificImport,
    groups: [EventScopeGroupManifestDraft],
    attributes: [EventAttributeDefinitionDraft] = [],
    sourceTimeUnit: SpikeTimeUnit = .seconds,
    activityMode: ScientificDatasetActivityMode = .putativeSingleUnit
) throws -> ResolvedScientificImportPlan {
    let draft = ScientificImportManifestDraft(
        boundTo: staged,
        sourceTimeUnit: sourceTimeUnit,
        activityMode: activityMode,
        eventScopeGroups: groups,
        eventAttributeDefinitions: attributes
    )
    return try ScientificImportPlanResolver.resolve(stagedImport: staged, draft: draft)
}

private func projectorColumn(_ index: Int) throws -> StagedSourceColumnReference {
    try StagedSourceColumnReference(oneBasedIndex: index)
}

private func projectorCell(column: Int, row: Int) throws -> StagedSourceCellReference {
    try StagedSourceCellReference(
        column: projectorColumn(column),
        oneBasedDataRowIndex: row
    )
}

private func projectorGroupID(_ text: String) throws -> ScientificEventScopeGroupID {
    ScientificEventScopeGroupID(try ScientificSemanticID(validating: text))
}

private func projectorSpike(
    _ column: StagedSourceColumnReference,
    id: String,
    duplicates: ExactDuplicateDecision = .preserveMultiplicity
) throws -> SpikeTrainColumnManifestDraft {
    SpikeTrainColumnManifestDraft(
        sourceColumn: column,
        semanticID: ScientificSpikeTrainID(try ScientificSemanticID(validating: id)),
        orderDecision: .preserveSourceOrder,
        duplicateDecision: duplicates
    )
}

private func projectorEvent(
    _ column: StagedSourceColumnReference,
    id: String
) throws -> EventDefinitionColumnManifestDraft {
    EventDefinitionColumnManifestDraft(
        sourceColumn: column,
        semanticID: ScientificEventDefinitionID(try ScientificSemanticID(validating: id)),
        eventTypeID: ScientificEventTypeID(try ScientificSemanticID(validating: "stimulus")),
        orderDecision: .preserveSourceOrder
    )
}

private func projectorAttribute(
    _ key: String,
    type: EventAttributeScalarType,
    role: EventAttributeRole,
    unit: ConfirmedEventAttributeUnit
) throws -> EventAttributeDefinitionDraft {
    EventAttributeDefinitionDraft(
        key: try EventAttributeKey(validating: key),
        scalarType: type,
        role: role,
        unit: unit,
        emptyStringPolicy: .forbid
    )
}
