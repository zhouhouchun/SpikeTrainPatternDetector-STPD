@testable import STPDCore
import Testing

// MARK: - Confirmation record

@Test
func scientificImportConfirmationCannotBeBuiltFromRejectedPreparation() throws {
    // A prepared value the independent validator rejects yields no validated token, and therefore
    // no `ConfirmableScientificImport` and no confirmation record can be produced.
    let staged = try confStaged(headers: ["unit_one", "unit_two"], columns: [[.text(rawText: "0")], [.text(rawText: "0")]])
    let cleanPlan = try confPlan(
        staged: staged,
        groups: [
            confGroup("group_a", [try confSpike(confColumn(1), id: "unit_a")]),
            confGroup("group_b", [try confSpike(confColumn(2), id: "unit_b")]),
        ]
    )
    let sharedID = ScientificSpikeTrainID(try ScientificSemanticID(validating: "unit"))
    let tampered = PreparedScientificImport(
        data: PreparedScientificImportData(
            recordingSegment: standardConfirmedRecordingSegment(),
            activityMode: .putativeSingleUnit,
            eventScopeGroups: [
                PreparedEventScopeGroup(
                    semanticID: try confGroupID("group_a"),
                    timeBasis: .recordingElapsed,
                    spikeTrains: [PreparedSpikeTrain(semanticID: sharedID, timestamps: [.zero])],
                    eventDefinitions: []
                ),
                PreparedEventScopeGroup(
                    semanticID: try confGroupID("group_b"),
                    timeBasis: .recordingElapsed,
                    spikeTrains: [PreparedSpikeTrain(semanticID: sharedID, timestamps: [.zero])],
                    eventDefinitions: []
                ),
            ],
            scientificAttributeDefinitions: [],
            presentationAttributeDefinitions: []
        ),
        provenance: ScientificImportNormalizationProvenance(resolvedPlan: cleanPlan, eventScopeGroups: [])
    )

    switch PreparedScientificImportValidator.validateForCanonicalProjection(tampered) {
    case .accepted:
        Issue.record("A tampered prepared import must not be accepted for canonical projection.")
    case .rejected(let report):
        #expect(report.hasBlockingIssues)
    }
}

@Test
func scientificImportConfirmationIsDeterministicForSameValidatedTransaction() throws {
    let first = try confManifest()
    let second = try confManifest()
    // Compare the explicit relevant fields; the manifest is intentionally not Equatable.
    #expect(first.canonicalFingerprint == second.canonicalFingerprint)
    #expect(first.sourceTransactionBinding == second.sourceTransactionBinding)
    #expect(first.temporalScope == second.temporalScope)
    #expect(first.persistence == second.persistence)
    #expect(first.activityMode == second.activityMode)
    #expect(first.temporalScope == .singleRecordingSegmentBoundsUnavailable)
    #expect(first.persistence == .unavailableInMemoryOnly)
    #expect(first.activityMode == .putativeSingleUnit)
}

@Test
func scientificImportConfirmationKeepsEquivalentCSVAndXLSXFingerprintWithDistinctSourceBinding() throws {
    let csv = try confManifest(
        source: .commaSeparatedValues,
        columns: [[.text(rawText: "1.000000"), .text(rawText: "1.250000")]],
        sourceTimeUnit: .seconds,
        bindingCharacter: "a"
    )
    let workbook = try confManifest(
        source: .excelWorkbook(worksheetName: "Recording"),
        columns: [[.spreadsheetNumber(rawLexeme: "1000.000"), .spreadsheetNumber(rawLexeme: "1250.000")]],
        sourceTimeUnit: .milliseconds,
        bindingCharacter: "b"
    )
    // Same canonical scientific identity, distinct source-bound confirmation context.
    #expect(csv.canonicalFingerprint == workbook.canonicalFingerprint)
    #expect(csv.sourceTransactionBinding != workbook.sourceTransactionBinding)
}

@Test
func scientificImportConfirmationExcludesPresentationAndSourceFromScientificIdentity() throws {
    // Presentation-only change plus a different source binding: scientific identity is unchanged.
    let first = try confManifest(
        columns: [[.text(rawText: "1"), .blank, .blank]],
        eventColumn: [.text(rawText: "0.5"), .text(rawText: "@scientific=7"), .text(rawText: "@presentation=first")],
        bindingCharacter: "a"
    )
    let second = try confManifest(
        columns: [[.text(rawText: "1"), .blank, .blank]],
        eventColumn: [.text(rawText: "0.5"), .text(rawText: "@scientific=7"), .text(rawText: "@presentation=different")],
        bindingCharacter: "b"
    )
    #expect(first.canonicalFingerprint == second.canonicalFingerprint)
    #expect(first.sourceTransactionBinding != second.sourceTransactionBinding)
}

@Test
func scientificImportConfirmationScientificChangeAltersFingerprintAndInvalidatesPrior() throws {
    let base = try confManifest(columns: [[.text(rawText: "1"), .text(rawText: "2")]])
    let changed = try confManifest(columns: [[.text(rawText: "1"), .text(rawText: "3")]])
    // A scientific-data change alters the sole scientific identity (the fingerprint).
    #expect(base.canonicalFingerprint != changed.canonicalFingerprint)
}

@Test
func scientificImportConfirmationRetainsSortAndDuplicatePolicyWhileRawMultiplicityUnchanged() throws {
    let preserved = try confManifest(
        columns: [[.text(rawText: "1"), .text(rawText: "1"), .text(rawText: "2")]],
        duplicateDecision: .preserveMultiplicity
    )
    let collapseRequested = try confManifest(
        columns: [[.text(rawText: "1"), .text(rawText: "1"), .text(rawText: "2")]],
        duplicateDecision: .collapseExact
    )
    // The requested duplicate policy is retained as a confirmed decision/provenance, but the
    // canonical raw multiplicity — and therefore the scientific fingerprint — is unchanged.
    #expect(preserved.canonicalFingerprint == collapseRequested.canonicalFingerprint)
    let preservedTrain = preserved.validatedImport.preparedImport.data.eventScopeGroups[0].spikeTrains[0]
    let collapseTrain = collapseRequested.validatedImport.preparedImport.data.eventScopeGroups[0].spikeTrains[0]
    #expect(preservedTrain.timestamps.map(\.microseconds) == [1_000_000, 1_000_000, 2_000_000])
    #expect(collapseTrain.timestamps.map(\.microseconds) == [1_000_000, 1_000_000, 2_000_000])
    #expect(collapseRequested.validatedImport.preparedImport.provenance.eventScopeGroups[0].spikeTrains[0]
        .duplicateDecision == .collapseExact)
}

@Test
func scientificImportConfirmationReportsSingleRecordingSegmentAndInMemoryPersistence() throws {
    let manifest = try confManifest()
    #expect(manifest.temporalScope == .singleRecordingSegmentBoundsUnavailable)
    #expect(manifest.persistence == .unavailableInMemoryOnly)
}

@Test
func scientificImportConfirmationReusesExistingFingerprintValue() throws {
    // This proves only that the confirmation carries the exact fingerprint value already produced
    // by the projector — not that no hashing occurred. The "no second hash/validate/project pass"
    // property is established by source/dependency review of the builder, not by digest equality.
    let confirmable = try confConfirmable()
    let manifest = ScientificImportConfirmationBuilder.confirm(confirmable)
    #expect(manifest.canonicalFingerprint == confirmable.shadow.fingerprint)
    #expect(manifest.sourceTransactionBinding == confirmable.shadow.sourceTransactionBinding)
}

// MARK: - Analysis readiness (deny-only, fail-closed)

@Test
func scientificAnalysisReadinessReportsObservationBoundsUnavailable() throws {
    let assessment = ScientificAnalysisReadinessEvaluator.assess(try confManifest())
    #expect(assessment.blockers.contains(.observationBoundsUnavailable))
}

@Test
func scientificAnalysisReadinessPutativeSingleUnitContinuousRemainsShadowOnly() throws {
    // Putative single-unit + continuous_untrialed + all-coverage + in-memory: the minimal blocker
    // set. continuous_untrialed adds no Trial blocker; all-coverage adds no coverage blocker.
    let assessment = ScientificAnalysisReadinessEvaluator.assess(
        try confManifest(activityMode: .putativeSingleUnit)
    )
    #expect(assessment.isAnalysisBlocked)
    #expect(assessment.blockers == [
        .confirmedManifestPersistenceUnavailable,
        .observationBoundsUnavailable,
        .runContractUnavailable,
        .detectorConsumerClosureUnavailable,
        .authoritativeExportClosureUnavailable,
    ])
}

@Test
func scientificAnalysisReadinessMultiUnitAndUnknownReportBiologicalDetectionNotEvaluated() throws {
    for mode in [ScientificDatasetActivityMode.intentionalMultiUnit, .unknownOrUncertain] {
        let assessment = ScientificAnalysisReadinessEvaluator.assess(try confManifest(activityMode: mode))
        #expect(assessment.isAnalysisBlocked)
        #expect(assessment.blockers.contains(
            .authoritativeBiologicalPatternDetectionNotDefinedForActivityMode(mode)
        ))
    }
}

// MARK: - Fixtures (full validate-once path)

private enum ConfirmationFixtureError: Error { case rejected }

private func confManifest(
    source: StagedTabularSource = .commaSeparatedValues,
    columns: [[StagedCellValue]] = [[.text(rawText: "1"), .text(rawText: "2")]],
    eventColumn: [StagedCellValue]? = nil,
    duplicateDecision: ExactDuplicateDecision = .preserveMultiplicity,
    sourceTimeUnit: SpikeTimeUnit = .seconds,
    activityMode: ScientificDatasetActivityMode = .putativeSingleUnit,
    bindingCharacter: Character = "a"
) throws -> ConfirmedScientificImportManifest {
    ScientificImportConfirmationBuilder.confirm(
        try confConfirmable(
            source: source,
            columns: columns,
            eventColumn: eventColumn,
            duplicateDecision: duplicateDecision,
            sourceTimeUnit: sourceTimeUnit,
            activityMode: activityMode,
            bindingCharacter: bindingCharacter
        )
    )
}

private func confConfirmable(
    source: StagedTabularSource = .commaSeparatedValues,
    columns: [[StagedCellValue]] = [[.text(rawText: "1"), .text(rawText: "2")]],
    eventColumn: [StagedCellValue]? = nil,
    duplicateDecision: ExactDuplicateDecision = .preserveMultiplicity,
    sourceTimeUnit: SpikeTimeUnit = .seconds,
    activityMode: ScientificDatasetActivityMode = .putativeSingleUnit,
    bindingCharacter: Character = "a"
) throws -> ConfirmableScientificImport {
    var headers = ["unit"]
    var allColumns = columns
    var groups = [confGroup("group", [try confSpike(confColumn(1), id: "unit", duplicates: duplicateDecision)])]
    var attributes: [EventAttributeDefinitionDraft] = []
    if let eventColumn {
        headers.append("event")
        allColumns.append(eventColumn)
        groups = [
            EventScopeGroupManifestDraft(
                semanticID: try confGroupID("group"),
                spikeTrains: [try confSpike(confColumn(1), id: "unit", duplicates: duplicateDecision)],
                eventDefinitions: [try confEvent(confColumn(2), id: "event")],
                timeBasis: .recordingElapsed
            ),
        ]
        attributes = [
            try confAttribute("presentation", role: .presentation),
            try confAttribute("scientific", role: .scientific, type: .integer, unit: .dimensionless),
        ]
    }
    let staged = try confStaged(source: source, headers: headers, columns: allColumns, bindingCharacter: bindingCharacter)
    let plan = try confPlan(
        staged: staged,
        groups: groups,
        attributes: attributes,
        sourceTimeUnit: sourceTimeUnit,
        activityMode: activityMode
    )
    let prepared = try ScientificImportNormalizer.normalize(resolvedPlan: plan)
    switch PreparedScientificImportValidator.validateForCanonicalProjection(prepared) {
    case .accepted(let validated):
        return try CanonicalScientificImportProjector.projectConfirmable(validated)
    case .rejected(let report):
        Issue.record("Expected acceptance, got: \(report)")
        throw ConfirmationFixtureError.rejected
    }
}

private func confStaged(
    source: StagedTabularSource = .commaSeparatedValues,
    headers: [String],
    columns: [[StagedCellValue]],
    bindingCharacter: Character = "a"
) throws -> StagedScientificImport {
    let stagedColumns = try columns.indices.map { offset in
        StagedScientificColumn(sourceColumn: try confColumn(offset + 1), header: headers[offset], cells: columns[offset])
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
            sourceBytesSHA256: String(repeating: bindingCharacter, count: 64),
            selection: selection
        )
    )
}

private func confPlan(
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
        recordingSegmentID: testSegmentID(),
        recordingRegime: .continuousUntrialed,
        importedExcerptCoverage: .allSpikeTrainsFullImportedExcerpt,
        observationBoundsAvailability: .unknownOrUnavailable,
        eventScopeGroups: groups,
        eventAttributeDefinitions: attributes
    )
    return try ScientificImportPlanResolver.resolve(stagedImport: staged, draft: draft)
}

private func confColumn(_ index: Int) throws -> StagedSourceColumnReference {
    try StagedSourceColumnReference(oneBasedIndex: index)
}
private func confGroupID(_ text: String) throws -> ScientificEventScopeGroupID {
    ScientificEventScopeGroupID(try ScientificSemanticID(validating: text))
}
private func confGroup(_ id: String, _ spikes: [SpikeTrainColumnManifestDraft]) -> EventScopeGroupManifestDraft {
    EventScopeGroupManifestDraft(
        semanticID: try! confGroupID(id),
        spikeTrains: spikes,
        eventDefinitions: [],
        timeBasis: .recordingElapsed
    )
}
private func confSpike(
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
private func confEvent(_ column: StagedSourceColumnReference, id: String) throws -> EventDefinitionColumnManifestDraft {
    EventDefinitionColumnManifestDraft(
        sourceColumn: column,
        semanticID: ScientificEventDefinitionID(try ScientificSemanticID(validating: id)),
        eventTypeID: ScientificEventTypeID(try ScientificSemanticID(validating: "stimulus")),
        orderDecision: .preserveSourceOrder
    )
}
private func confAttribute(
    _ key: String,
    role: EventAttributeRole,
    type: EventAttributeScalarType = .string,
    unit: ConfirmedEventAttributeUnit = .notApplicable
) throws -> EventAttributeDefinitionDraft {
    EventAttributeDefinitionDraft(
        key: try EventAttributeKey(validating: key),
        scalarType: type,
        role: role,
        unit: unit,
        emptyStringPolicy: .forbid
    )
}
