@testable import STPDCore

/// Shared fixtures for the persisted-confirmed-manifest slice: end-to-end confirmed manifests built
/// through the real import chain, and directly-constructed receipts (with correct digests) for
/// exhaustive codec-branch and failure-injection tests.

enum PersistenceFixtureError: Error { case rejected }

// MARK: - Confirmed manifests via the real chain

func stagedForPersistence(
    source: StagedTabularSource = .commaSeparatedValues,
    headers: [String],
    columns: [[StagedCellValue]],
    bindingCharacter: Character = "a",
    headerPresent: Bool = true
) throws -> StagedScientificImport {
    // `headerPresent == false` models a headerless staging where NO column carries header text
    // (`header == nil`), driving a real headerless base through the chain.
    let cols = try headers.enumerated().map { index, header in
        StagedScientificColumn(
            sourceColumn: try StagedSourceColumnReference(oneBasedIndex: index + 1),
            header: headerPresent ? header : nil,
            cells: columns[index]
        )
    }
    let selection: StagedSourceTransactionSelection
    switch source {
    case .commaSeparatedValues:
        selection = .commaSeparatedValues
    case .excelWorkbook(let name):
        selection = .excelWorksheet(
            worksheetName: name, sheetID: 1,
            relationshipID: "rId1", normalizedPartPath: "xl/worksheets/sheet1.xml"
        )
    }
    return try StagedScientificImport(
        source: source,
        columns: cols,
        suggestions: .none,
        sourceTransactionBinding: try StagedSourceTransactionBinding(
            sourceBytesSHA256: String(repeating: bindingCharacter, count: 64),
            selection: selection
        )
    )
}

func confirmableForPersistence(
    staged: StagedScientificImport,
    groups: [EventScopeGroupManifestDraft],
    attributes: [EventAttributeDefinitionDraft] = [],
    sourceTimeUnit: SpikeTimeUnit = .seconds,
    activityMode: ScientificDatasetActivityMode = .putativeSingleUnit
) throws -> ConfirmableScientificImport {
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
    let plan = try ScientificImportPlanResolver.resolve(stagedImport: staged, draft: draft)
    let prepared = try ScientificImportNormalizer.normalize(resolvedPlan: plan)
    guard case .accepted(let validated) =
        PreparedScientificImportValidator.validateForCanonicalProjection(prepared) else {
        throw PersistenceFixtureError.rejected
    }
    return try CanonicalScientificImportProjector.projectConfirmable(validated)
}

/// A one-group, one-spike-train confirmed manifest with no events and no attributes. With
/// `headerPresent: false` it is a real HEADERLESS base (its single column carries no header text), so
/// its derived rule is `.headerless`; the topology (single group, no events, recording-elapsed)
/// remains a valid headerless graph.
func simpleConfirmedManifest(
    bindingCharacter: Character = "a",
    activityMode: ScientificDatasetActivityMode = .putativeSingleUnit,
    duplicate: ExactDuplicateDecision = .preserveMultiplicity,
    headerPresent: Bool = true
) throws -> ConfirmedScientificImportManifest {
    let staged = try stagedForPersistence(
        headers: ["unit"],
        columns: [[.text(rawText: "1"), .text(rawText: "2")]],
        bindingCharacter: bindingCharacter,
        headerPresent: headerPresent
    )
    let group = EventScopeGroupManifestDraft(
        semanticID: psGroupID("group_1"),
        spikeTrains: [spikeDraft(1, "unit_a", duplicate: duplicate)],
        eventDefinitions: [],
        timeBasis: .recordingElapsed
    )
    return ScientificImportConfirmationBuilder.confirm(
        try confirmableForPersistence(staged: staged, groups: [group], activityMode: activityMode)
    )
}

/// A one-group manifest with two spike trains, one event definition, and both a scientific and a
/// Presentation attribute — exercising events and Presentation-role definitions in the receipt.
func richConfirmedManifest(
    bindingCharacter: Character = "a",
    source: StagedTabularSource = .commaSeparatedValues
) throws -> ConfirmedScientificImportManifest {
    let staged = try stagedForPersistence(
        source: source,
        headers: ["unit_a", "unit_b", "event_x"],
        columns: [
            [.text(rawText: "1"), .text(rawText: "2")],
            [.text(rawText: "3"), .text(rawText: "4")],
            [.text(rawText: "5"), .blank],
        ],
        bindingCharacter: bindingCharacter
    )
    let group = EventScopeGroupManifestDraft(
        semanticID: psGroupID("group_1"),
        spikeTrains: [spikeDraft(1, "unit_a"), spikeDraft(2, "unit_b")],
        eventDefinitions: [eventDraft(3, "stim", type: "stimulus")],
        timeBasis: .recordingElapsed
    )
    let attributes = [
        attributeDraft("amplitude", role: .scientific, type: .integer, unit: .dimensionless),
        attributeDraft("color", role: .presentation, type: .string, unit: .notApplicable),
    ]
    return ScientificImportConfirmationBuilder.confirm(
        try confirmableForPersistence(staged: staged, groups: [group], attributes: attributes)
    )
}

/// The immutable source facts of the two-group event-ID-reuse scenario, built FRESH on every call so a
/// restart replay can construct an independent staged import rather than reuse the original value. Both
/// groups legitimately reuse the event-definition ID `stimulus` (group-local); spike-train IDs stay
/// dataset-unique; the source columns form a contiguous 1..4 partition in group order (group_a = [1,2],
/// group_b = [3,4]).
func twoGroupEventReuseStagedImport(
    bindingCharacter: Character = "a"
) throws -> StagedScientificImport {
    try stagedForPersistence(
        headers: ["unit_a", "stim_a", "unit_b", "stim_b"],
        columns: [
            [.text(rawText: "1"), .text(rawText: "2")],
            [.text(rawText: "3"), .blank],
            [.text(rawText: "5"), .text(rawText: "6")],
            [.text(rawText: "7"), .blank],
        ],
        bindingCharacter: bindingCharacter
    )
}

/// The two group drafts for the reuse scenario. Used ONLY to construct the original base; a restart
/// replay reconstructs its draft from the decoded on-disk receipt instead of reusing these.
func twoGroupEventReuseGroups() -> [EventScopeGroupManifestDraft] {
    [
        EventScopeGroupManifestDraft(
            semanticID: psGroupID("group_a"),
            spikeTrains: [spikeDraft(1, "unit_a")],
            eventDefinitions: [eventDraft(2, "stimulus", type: "stim")],
            timeBasis: .recordingElapsed
        ),
        EventScopeGroupManifestDraft(
            semanticID: psGroupID("group_b"),
            spikeTrains: [spikeDraft(3, "unit_b")],
            eventDefinitions: [eventDraft(4, "stimulus", type: "stim")],
            timeBasis: .recordingElapsed
        ),
    ]
}

/// A two-group manifest whose groups legitimately REUSE the event-definition ID `stimulus`
/// (group-local), built through the real chain from a fresh staged import.
func twoGroupEventReuseConfirmedManifest(
    bindingCharacter: Character = "a"
) throws -> ConfirmedScientificImportManifest {
    ScientificImportConfirmationBuilder.confirm(
        try confirmableForPersistence(
            staged: try twoGroupEventReuseStagedImport(bindingCharacter: bindingCharacter),
            groups: twoGroupEventReuseGroups()
        )
    )
}

// MARK: - Draft builders

func psGroupID(_ text: String) -> ScientificEventScopeGroupID {
    ScientificEventScopeGroupID(try! ScientificSemanticID(validating: text))
}

func spikeDraft(
    _ column: Int,
    _ id: String,
    order: TimestampOrderDecision = .preserveSourceOrder,
    duplicate: ExactDuplicateDecision = .preserveMultiplicity
) -> SpikeTrainColumnManifestDraft {
    SpikeTrainColumnManifestDraft(
        sourceColumn: try! StagedSourceColumnReference(oneBasedIndex: column),
        semanticID: ScientificSpikeTrainID(try! ScientificSemanticID(validating: id)),
        orderDecision: order,
        duplicateDecision: duplicate
    )
}

func eventDraft(
    _ column: Int,
    _ id: String,
    type: String,
    order: TimestampOrderDecision = .preserveSourceOrder
) -> EventDefinitionColumnManifestDraft {
    EventDefinitionColumnManifestDraft(
        sourceColumn: try! StagedSourceColumnReference(oneBasedIndex: column),
        semanticID: ScientificEventDefinitionID(try! ScientificSemanticID(validating: id)),
        eventTypeID: ScientificEventTypeID(try! ScientificSemanticID(validating: type)),
        orderDecision: order
    )
}

func attributeDraft(
    _ key: String,
    role: EventAttributeRole,
    type: EventAttributeScalarType = .string,
    unit: ConfirmedEventAttributeUnit = .notApplicable,
    emptyStringPolicy: EventEmptyStringPolicy = .forbid
) -> EventAttributeDefinitionDraft {
    EventAttributeDefinitionDraft(
        key: try! EventAttributeKey(validating: key),
        scalarType: type,
        role: role,
        unit: unit,
        emptyStringPolicy: emptyStringPolicy
    )
}

// MARK: - Directly-constructed receipts (correct digest) for codec tests

func csvBinding(_ character: Character = "a") -> StagedSourceTransactionBinding {
    try! StagedSourceTransactionBinding(
        sourceBytesSHA256: String(repeating: character, count: 64),
        selection: .commaSeparatedValues
    )
}

func worksheetBinding(_ character: Character = "b") -> StagedSourceTransactionBinding {
    try! StagedSourceTransactionBinding(
        sourceBytesSHA256: String(repeating: character, count: 64),
        selection: .excelWorksheet(
            worksheetName: "Data", sheetID: 3,
            relationshipID: "rId2", normalizedPartPath: "xl/worksheets/sheet2.xml"
        )
    )
}

func receiptSpike(
    _ column: Int,
    _ id: String,
    order: TimestampOrderDecision = .preserveSourceOrder,
    duplicate: ExactDuplicateDecision = .preserveMultiplicity
) -> ReceiptSpikeTrainColumn {
    ReceiptSpikeTrainColumn(
        sourceColumnOneBasedIndex: column,
        semanticID: ScientificSpikeTrainID(try! ScientificSemanticID(validating: id)),
        orderDecision: order,
        duplicateDecision: duplicate
    )
}

func receiptEvent(
    _ column: Int,
    _ id: String,
    type: String,
    order: TimestampOrderDecision = .preserveSourceOrder
) -> ReceiptEventDefinitionColumn {
    ReceiptEventDefinitionColumn(
        sourceColumnOneBasedIndex: column,
        semanticID: ScientificEventDefinitionID(try! ScientificSemanticID(validating: id)),
        eventTypeID: ScientificEventTypeID(try! ScientificSemanticID(validating: type)),
        orderDecision: order
    )
}

func receiptGroupValue(
    id: String = "group_1",
    timeBasis: ReceiptEventScopeTimeBasis = .recordingElapsed,
    spikes: [ReceiptSpikeTrainColumn] = [receiptSpike(1, "spike_one")],
    events: [ReceiptEventDefinitionColumn] = []
) -> ReceiptEventScopeGroup {
    ReceiptEventScopeGroup(
        semanticID: psGroupID(id), timeBasis: timeBasis,
        spikeTrains: spikes, eventDefinitions: events
    )
}

func receiptAttribute(
    _ key: String,
    role: EventAttributeRole = .scientific,
    type: EventAttributeScalarType = .string,
    unit: ConfirmedEventAttributeUnit = .notApplicable,
    emptyStringPolicy: EventEmptyStringPolicy = .forbid
) -> ReceiptAttributeDefinition {
    ReceiptAttributeDefinition(
        key: try! EventAttributeKey(validating: key),
        scalarType: type, role: role, unit: unit, emptyStringPolicy: emptyStringPolicy
    )
}

/// Builds a receipt with a correct `confirmationRecordDigest` (computed by the codec), so encode →
/// decode round-trips. Fields default to a minimal valid CSV single-train receipt.
func makeReceipt(
    canonicalSchemaContractID: String = "canonical_microsecond_single_recording_segment_event_scope_dataset",
    canonicalSchemaContractDigest: String = String(repeating: "d", count: 64),
    canonicalDatasetDigest: String = String(repeating: "e", count: 64),
    binding: StagedSourceTransactionBinding? = nil,
    headerRule: PersistedTabularHeaderRule = .firstRecordIsHeader,
    sourceTimeUnit: SpikeTimeUnit = .seconds,
    activityMode: ScientificDatasetActivityMode = .putativeSingleUnit,
    recordingSegment: ConfirmedRecordingSegment = standardConfirmedRecordingSegment(),
    groups: [ReceiptEventScopeGroup] = [receiptGroupValue()],
    attributes: [ReceiptAttributeDefinition] = []
) -> ConfirmedScientificImportReceipt {
    let resolvedBinding = binding ?? csvBinding()
    func build(_ digest: String) -> ConfirmedScientificImportReceipt {
        ConfirmedScientificImportReceipt(
            canonicalSchemaContractID: canonicalSchemaContractID,
            canonicalSchemaContractDigest: canonicalSchemaContractDigest,
            canonicalDatasetDigest: canonicalDatasetDigest,
            sourceTransactionBinding: resolvedBinding,
            headerRule: headerRule,
            sourceTimeUnit: sourceTimeUnit,
            activityMode: activityMode,
            recordingSegment: recordingSegment,
            eventScopeGroups: groups,
            attributeDefinitions: attributes,
            confirmationRecordDigest: digest
        )
    }
    let digest = ConfirmedScientificImportManifestPersistence.recordDigest(of: build(""))
    return build(digest)
}
