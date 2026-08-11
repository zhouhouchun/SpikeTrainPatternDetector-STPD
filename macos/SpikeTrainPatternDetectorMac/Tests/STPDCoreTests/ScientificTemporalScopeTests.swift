@testable import STPDCore
import Testing

// MARK: - Resolver fails closed on any unresolved / missing segment field

@Test
func scientificTemporalScopeResolverFailsClosedOnEachUnresolvedSegmentField() throws {
    let staged = try temporalStaged()
    // Everything resolved except the segment: exactly the four missing-segment issues appear.
    let draft = temporalDraft(staged: staged, applySegment: false)
    #expect(temporalResolutionIssues(staged: staged, draft: draft) == [
        .missingRecordingSegmentID,
        .missingRecordingRegime,
        .missingImportedExcerptCoverage,
        .missingObservationBoundsAvailability,
    ])

    // Each field individually unresolved fails closed.
    var missingID = temporalDraft(staged: staged, applySegment: true)
    missingID.recordingSegmentID = nil
    #expect(temporalResolutionIssues(staged: staged, draft: missingID) == [.missingRecordingSegmentID])

    var missingRegime = temporalDraft(staged: staged, applySegment: true)
    missingRegime.recordingRegime = nil
    #expect(temporalResolutionIssues(staged: staged, draft: missingRegime) == [.missingRecordingRegime])

    var missingCoverage = temporalDraft(staged: staged, applySegment: true)
    missingCoverage.importedExcerptCoverage = nil
    #expect(temporalResolutionIssues(staged: staged, draft: missingCoverage) == [.missingImportedExcerptCoverage])

    var missingBounds = temporalDraft(staged: staged, applySegment: true)
    missingBounds.observationBoundsAvailability = nil
    #expect(temporalResolutionIssues(staged: staged, draft: missingBounds) == [.missingObservationBoundsAvailability])
}

// MARK: - All four fields survive the full chain to canonical + confirmation

@Test
func scientificTemporalScopeSegmentSurvivesDraftToCanonicalToConfirmation() throws {
    let segment = standardConfirmedRecordingSegment(
        id: "recording_7", regime: .trialized, coverage: .notAllSpikeTrainsFullImportedExcerpt
    )
    let confirmable = try temporalConfirmable(segment: segment)
    #expect(confirmable.shadow.dataset.recordingSegment == segment)
    let manifest = ScientificImportConfirmationBuilder.confirm(confirmable)
    #expect(manifest.recordingSegment == segment)
    // The confirmation never copies segment fields into a second truth; it reads the sealed import.
    #expect(manifest.validatedImport.preparedImport.data.recordingSegment == segment)
}

// MARK: - Independent validator rejects prepared-data tampering of each segment field

@Test
func scientificTemporalScopeValidatorRejectsSegmentTampering() throws {
    let base = standardConfirmedRecordingSegment()
    // Observation-bounds tampering is intentionally absent: `ObservationBoundsAvailability` has a
    // single valid case (`unknown_or_unavailable`), so there is no distinct valid value to forge into
    // the prepared data. The field is still bound by the codec and compared independently (see the
    // fingerprint header and `.dataObservationBoundsAvailability`); this list covers every segment
    // field that currently admits a divergent value.
    let tampers: [(ConfirmedRecordingSegment, PreparedScientificImportComparisonField)] = [
        (standardConfirmedRecordingSegment(id: "other"), .dataRecordingSegmentID),
        (standardConfirmedRecordingSegment(regime: .trialized), .dataRecordingRegime),
        (standardConfirmedRecordingSegment(coverage: .unknownOrUncertain), .dataImportedExcerptCoverage),
    ]
    for (tamperedSegment, field) in tampers {
        // Clean plan carries `base`; the prepared data is forged with a divergent segment.
        let forged = try temporalForgedPrepared(planSegment: base, dataSegment: tamperedSegment)
        let report = PreparedScientificImportValidator.validate(forged)
        #expect(report.blockingIssues.map(\.kind).contains(.preparedComparisonValueMismatch(field: field)))
    }
}

// MARK: - Fingerprint sensitivity: segment ID / regime / coverage change the digest

@Test
func scientificTemporalScopeSegmentFieldsAreFingerprintIdentity() throws {
    let baseline = try temporalDigest(standardConfirmedRecordingSegment())
    #expect(try temporalDigest(standardConfirmedRecordingSegment(id: "segment_z")) != baseline)
    #expect(try temporalDigest(standardConfirmedRecordingSegment(regime: .trialized)) != baseline)
    #expect(try temporalDigest(standardConfirmedRecordingSegment(regime: .unknownOrUncertain)) != baseline)
    #expect(try temporalDigest(standardConfirmedRecordingSegment(coverage: .notAllSpikeTrainsFullImportedExcerpt)) != baseline)
    #expect(try temporalDigest(standardConfirmedRecordingSegment(coverage: .unknownOrUncertain)) != baseline)
}

// MARK: - Deny-only readiness matrix (all 3 x 3 regime x coverage + activity modes)

@Test
func scientificTemporalScopeReadinessMatrixIsDenyOnly() throws {
    let regimes: [(ScientificRecordingRegime, [ScientificAnalysisReadinessBlocker])] = [
        (.continuousUntrialed, []),
        (.trialized, [.trialEntitiesNotRepresented]),
        (.unknownOrUncertain, [.recordingRegimeUnknownOrUncertain, .trialContractUnavailable]),
    ]
    let coverages: [(ImportedExcerptCoverage, [ScientificAnalysisReadinessBlocker])] = [
        (.allSpikeTrainsFullImportedExcerpt, []),
        (.notAllSpikeTrainsFullImportedExcerpt, [.partialImportedExcerptCoverage]),
        (.unknownOrUncertain, [.importedExcerptCoverageUnknownOrUncertain]),
    ]
    for (regime, regimeBlockers) in regimes {
        for (coverage, coverageBlockers) in coverages {
            let manifest = try temporalManifest(segment: standardConfirmedRecordingSegment(
                regime: regime, coverage: coverage
            ))
            let assessment = ScientificAnalysisReadinessEvaluator.assess(manifest)
            // Deny-only: always blocked, persistence is always the first blocker.
            #expect(assessment.isAnalysisBlocked)
            #expect(assessment.firstBlocker == .confirmedManifestPersistenceUnavailable)
            // Exact blocker set for a putative single-unit manifest: the invariant contract blockers
            // plus exactly the regime/coverage matrix blockers — and nothing else. The exact `==`
            // makes any stray or missing blocker (e.g. a Trial blocker under continuous_untrialed) fail.
            let invariant: Set<ScientificAnalysisReadinessBlocker> = [
                .confirmedManifestPersistenceUnavailable, .observationBoundsUnavailable,
                .runContractUnavailable, .detectorConsumerClosureUnavailable,
                .authoritativeExportClosureUnavailable,
            ]
            let expected = invariant.union(regimeBlockers).union(coverageBlockers)
            #expect(Set(assessment.blockers) == expected)
        }
    }
}

@Test
func scientificTemporalScopeReadinessActivityModeMatrix() throws {
    // Putative single-unit adds no biological-detection blocker; MUA/unknown add the not_evaluated one.
    let su = try temporalManifest(segment: standardConfirmedRecordingSegment(), activityMode: .putativeSingleUnit)
    #expect(!ScientificAnalysisReadinessEvaluator.assess(su).blockers.contains(
        .authoritativeBiologicalPatternDetectionNotDefinedForActivityMode(.putativeSingleUnit)))
    for mode in [ScientificDatasetActivityMode.intentionalMultiUnit, .unknownOrUncertain] {
        let manifest = try temporalManifest(segment: standardConfirmedRecordingSegment(), activityMode: mode)
        #expect(ScientificAnalysisReadinessEvaluator.assess(manifest).blockers.contains(
            .authoritativeBiologicalPatternDetectionNotDefinedForActivityMode(mode)))
    }
}

// MARK: - Helpers (full chain with a custom segment)

private enum TemporalFixtureError: Error { case rejected }

private func temporalStaged() throws -> StagedScientificImport {
    try StagedScientificImport(
        source: .commaSeparatedValues,
        columns: [
            StagedScientificColumn(
                sourceColumn: try StagedSourceColumnReference(oneBasedIndex: 1),
                header: "unit",
                cells: [.text(rawText: "1"), .text(rawText: "2")]
            ),
        ],
        suggestions: .none,
        sourceTransactionBinding: StagedSourceTransactionBinding(
            sourceBytesSHA256: String(repeating: "a", count: 64),
            selection: .commaSeparatedValues
        )
    )
}

private func temporalDraft(
    staged: StagedScientificImport,
    applySegment: Bool,
    segment: ConfirmedRecordingSegment = standardConfirmedRecordingSegment()
) -> ScientificImportManifestDraft {
    let column = try! StagedSourceColumnReference(oneBasedIndex: 1)
    var draft = ScientificImportManifestDraft(
        boundTo: staged,
        sourceTimeUnit: .seconds,
        activityMode: .putativeSingleUnit,
        eventScopeGroups: [
            EventScopeGroupManifestDraft(
                semanticID: ScientificEventScopeGroupID(try! ScientificSemanticID(validating: "group")),
                spikeTrains: [SpikeTrainColumnManifestDraft(
                    sourceColumn: column,
                    semanticID: ScientificSpikeTrainID(try! ScientificSemanticID(validating: "unit")),
                    orderDecision: .preserveSourceOrder,
                    duplicateDecision: .preserveMultiplicity
                )],
                eventDefinitions: [],
                timeBasis: .recordingElapsed
            ),
        ]
    )
    if applySegment {
        draft.recordingSegmentID = segment.semanticID
        draft.recordingRegime = segment.regime
        draft.importedExcerptCoverage = segment.importedExcerptCoverage
        draft.observationBoundsAvailability = segment.observationBounds
    }
    return draft
}

private func temporalResolutionIssues(
    staged: StagedScientificImport,
    draft: ScientificImportManifestDraft
) -> [ScientificImportPlanIssue] {
    do {
        _ = try ScientificImportPlanResolver.resolve(stagedImport: staged, draft: draft)
        return []
    } catch let error as ScientificImportPlanResolutionError {
        return error.issues
    } catch {
        return []
    }
}

private func temporalConfirmable(segment: ConfirmedRecordingSegment) throws -> ConfirmableScientificImport {
    let staged = try temporalStaged()
    let draft = temporalDraft(staged: staged, applySegment: true, segment: segment)
    let plan = try ScientificImportPlanResolver.resolve(stagedImport: staged, draft: draft)
    let prepared = try ScientificImportNormalizer.normalize(resolvedPlan: plan)
    switch PreparedScientificImportValidator.validateForCanonicalProjection(prepared) {
    case .accepted(let validated):
        return try CanonicalScientificImportProjector.projectConfirmable(validated)
    case .rejected:
        throw TemporalFixtureError.rejected
    }
}

private func temporalManifest(
    segment: ConfirmedRecordingSegment,
    activityMode: ScientificDatasetActivityMode = .putativeSingleUnit
) throws -> ConfirmedScientificImportManifest {
    // For activity-mode variation, rebuild the draft with the requested mode.
    let staged = try temporalStaged()
    var draft = temporalDraft(staged: staged, applySegment: true, segment: segment)
    draft.activityMode = activityMode
    let plan = try ScientificImportPlanResolver.resolve(stagedImport: staged, draft: draft)
    let prepared = try ScientificImportNormalizer.normalize(resolvedPlan: plan)
    guard case .accepted(let validated) =
        PreparedScientificImportValidator.validateForCanonicalProjection(prepared) else {
        throw TemporalFixtureError.rejected
    }
    return ScientificImportConfirmationBuilder.confirm(
        try CanonicalScientificImportProjector.projectConfirmable(validated)
    )
}

private func temporalDigest(_ segment: ConfirmedRecordingSegment) throws -> String {
    let train = CanonicalSpikeTrain(
        semanticID: ScientificSpikeTrainID(try ScientificSemanticID(validating: "unit")),
        rawTimestamps: [MicrosecondTick(microseconds: 1)]
    )
    let group = CanonicalEventScopeGroup(
        semanticID: ScientificEventScopeGroupID(try ScientificSemanticID(validating: "group")),
        timeBasis: .recordingElapsed,
        spikeTrainReferences: [train.semanticID],
        eventDefinitions: []
    )
    let dataset = CanonicalScientificDataset(
        recordingSegment: segment,
        activityMode: .putativeSingleUnit,
        spikeTrains: [train],
        eventScopeGroups: [group],
        scientificAttributeDefinitions: []
    )
    return try CanonicalScientificDatasetFingerprinter.fingerprint(dataset).datasetDigest
}

private func temporalForgedPrepared(
    planSegment: ConfirmedRecordingSegment,
    dataSegment: ConfirmedRecordingSegment
) throws -> PreparedScientificImport {
    // Build a clean plan carrying `planSegment`, then forge prepared data with `dataSegment`.
    let staged = try temporalStaged()
    let draft = temporalDraft(staged: staged, applySegment: true, segment: planSegment)
    let plan = try ScientificImportPlanResolver.resolve(stagedImport: staged, draft: draft)
    let clean = try ScientificImportNormalizer.normalize(resolvedPlan: plan)
    let tamperedData = PreparedScientificImportData(
        recordingSegment: dataSegment,
        activityMode: clean.data.activityMode,
        eventScopeGroups: clean.data.eventScopeGroups,
        scientificAttributeDefinitions: clean.data.scientificAttributeDefinitions,
        presentationAttributeDefinitions: clean.data.presentationAttributeDefinitions
    )
    return PreparedScientificImport(data: tamperedData, provenance: clean.provenance)
}
