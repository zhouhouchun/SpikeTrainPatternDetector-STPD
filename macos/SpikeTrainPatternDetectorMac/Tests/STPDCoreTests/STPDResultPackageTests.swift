import Foundation
import Testing
@testable import STPDCore

private let resultPackageBuildCommit = "0123456789abcdef0123456789abcdef01234567"

private func resultPackageFixture() -> (
    dataset: SpikeDataset,
    run: ClassicAnchorDetectionRun
) {
    let burstISIs =
        Array(repeating: 0.100, count: 3) +
        Array(repeating: 0.006, count: 7) +
        Array(repeating: 0.100, count: 3)
    let tonicISIs = [
        0.300, 0.310, 0.295, 0.305, 0.300,
        0.900,
        0.305, 0.300, 0.295, 0.310, 0.300,
    ]

    func timestamps(_ isis: [Double]) -> [Double] {
        isis.reduce(into: [0.0]) { values, isi in
            values.append((values.last ?? 0) + isi)
        }
    }

    let dataset = SpikeDataset(
        name: "result-package-fixture",
        sourceDescription: "synthetic, \"quoted\"\nsource",
        trains: [
            SpikeTrain(name: "burst_train", timestampsSec: timestamps(burstISIs)),
            SpikeTrain(name: "tonic_pause_train", timestampsSec: timestamps(tonicISIs)),
        ]
    )
    let run = ClassicAnchorDetectionPipeline.run(
        dataset: dataset,
        bandSettings: TrainAdaptiveBandSettings(
            minValidISISec: 0.001,
            histogramBinWidthSec: 0.005
        ),
        buildCommit: resultPackageBuildCommit
    )
    return (dataset, run)
}

private func resultPackageColumn(
    _ table: STPDResultTableData,
    _ name: String
) throws -> [String] {
    let index = try #require(table.headers.firstIndex(of: name))
    return table.rows.map { $0[index] }
}

private func resultPackageRowFingerprint(
    _ table: STPDResultTableData,
    columns: [String]
) throws -> Set<String> {
    let indices = try columns.map { name in
        try #require(table.headers.firstIndex(of: name))
    }
    return Set(table.rows.map { row in
        indices.map { row[$0] }.joined(separator: "\u{1}")
    })
}

private func resultPackageReviewedRow(
    from row: ReviewedISIExportRow,
    note: String,
    finalPattern: String = ManualAnnotationLabel.tonic.rawValue,
    finalSubtype: String = "",
    finalSource: String = ReviewedISIExportBuilder.sourceManualPositive
) -> ReviewedISIExportRow {
    ReviewedISIExportRow(
        trainID: row.trainID,
        trainName: row.trainName,
        spikeIndex: row.spikeIndex,
        timestampSec: row.timestampSec,
        alignedTimestampSec: row.alignedTimestampSec,
        isiIndex: row.isiIndex,
        isiSec: row.isiSec,
        autoPattern: row.autoPattern,
        autoSubtype: row.autoSubtype,
        autoCandidateID: row.autoCandidateID,
        finalPattern: finalPattern,
        finalSubtype: finalSubtype,
        finalSource: finalSource,
        manualVetoSuppressed: false,
        reviewNote: note
    )
}

private func resultPackageISIRow(
    from row: ReviewedISIExportRow,
    spikeIndex: Int? = nil,
    isiIndex: Int? = nil,
    isiSec: Double? = nil,
    finalPattern: String? = nil
) -> ReviewedISIExportRow {
    ReviewedISIExportRow(
        trainID: row.trainID,
        trainName: row.trainName,
        spikeIndex: spikeIndex ?? row.spikeIndex,
        timestampSec: row.timestampSec,
        alignedTimestampSec: row.alignedTimestampSec,
        isiIndex: isiIndex ?? row.isiIndex,
        isiSec: isiSec ?? row.isiSec,
        autoPattern: row.autoPattern,
        autoSubtype: row.autoSubtype,
        autoCandidateID: row.autoCandidateID,
        finalPattern: finalPattern ?? row.finalPattern,
        finalSubtype: row.finalSubtype,
        finalSource: row.finalSource,
        manualVetoSuppressed: row.manualVetoSuppressed,
        reviewNote: row.reviewNote
    )
}

private func resultPackageHFSCandidate(
    train: SpikeTrain,
    id: String,
    candidateLayer: String = "result_package_hfs_fixture",
    score: Double = 0.12345678901234566,
    cv2: Double? = nil,
    nISI: Int = 3,
    nValidISI: Int? = nil,
    nSpikes: Int = 4,
    durationSec: Double? = nil,
    intraQ10Sec: Double? = nil,
    intraQ90Sec: Double? = nil,
    candidateClass: String = ClassicAnchorLabel.highFrequencySpiking.rawValue,
    finalLabel: ClassicAnchorLabel = .highFrequencySpiking,
    gateStatus: String = "pass",
    decisionPath: String = "result_package_hfs_fixture",
    action: String = "audit_only",
    priority: Int = 100,
    selectedForAuto: Bool = false,
    selectionStatus: String = "not_selected",
    startISIIndex: Int = 1,
    endISIIndex: Int = 3,
    anchorFamily: String = "high_frequency_spiking"
) -> ClassicAnchorCandidate {
    let observedISIs = (startISIIndex...endISIIndex).map {
        train.timestampsSec[$0] - train.timestampsSec[$0 - 1]
    }
    let sample = SortedFiniteSample(observedISIs)
    let observedDuration =
        train.timestampsSec[endISIIndex] -
        train.timestampsSec[startISIIndex - 1]
    var candidate = ClassicAnchorCandidate(
        id: id,
        trainID: train.id,
        trainName: train.name,
        candidateLayer: candidateLayer,
        candidateClass: candidateClass,
        finalLabel: finalLabel,
        gateStatus: gateStatus,
        decisionPath: decisionPath,
        action: action,
        score: score,
        priority: priority,
        selectedForAuto: selectedForAuto,
        selectionStatus: selectionStatus,
        startISIIndex: startISIIndex,
        endISIIndex: endISIIndex,
        startSpikeIndex: startISIIndex,
        endSpikeIndex: endISIIndex + 1,
        nISI: nISI,
        nValidISI: nValidISI ?? nISI,
        nSpikes: nSpikes,
        durationSec: durationSec ?? observedDuration,
        intraQ10Sec: intraQ10Sec ?? sample.quantile(0.10),
        intraQ40Sec: sample.quantile(0.40),
        intraQ50Sec: sample.quantile(0.50),
        intraQ90Sec: intraQ90Sec ?? sample.quantile(0.90),
        intraQ95Sec: sample.quantile(0.95),
        maxIntraISISec: observedISIs.max(),
        meanIntraISISec: STPDStatistics.mean(observedISIs),
        cv: STPDStatistics.coefficientOfVariation(observedISIs),
        lv: STPDStatistics.localVariation(observedISIs),
        preGapSec: nil,
        postGapSec: nil,
        preRatioQ90: nil,
        postRatioQ90: nil,
        edgeContrastMinQ90: nil,
        edgeContrastGeomQ90: nil,
        anchorFamily: anchorFamily,
        anchorLockLevel: .strongCandidate,
        anchorBandLowerSec: 0.001,
        anchorBandUpperSec: 0.1,
        anchorBandSource: .structure,
        anchorContrastMinRequired: 1,
        anchorContrastGeomRequired: 1,
        refractorySuspectCount: 0,
        refractorySuspectAction: nil
    )
    candidate.cv2 = cv2
    if finalLabel == .highFrequencySpiking {
        candidate.hfSpikingAcceptanceRoute = "fixture"
    }
    return candidate
}

private func resultPackageHFSRow(
    train: SpikeTrain,
    candidateID: String,
    rootID: String,
    rowID: String,
    rawScore: Double = 0.12345678901234566,
    packetCoverage: Double = 0,
    seedBandUpperSec: Double = 0.1,
    bridgeBandUpperSec: Double = 0.2,
    strongestBurstCandidateID: String? = nil,
    strongestBurstSubtype: String? = nil,
    strongestBurstRawScore: Double? = nil,
    strongestBurstPriority: Int? = nil,
    strongestLongBurstCandidateID: String? = nil,
    longBurstRawScore: Double? = nil,
    longBurstPriority: Int? = nil,
    hfsStartISIIndex: Int = 1,
    hfsEndISIIndex: Int = 3,
    conflictStartISIIndex: Int? = nil,
    conflictEndISIIndex: Int? = nil,
    decisionReason: String =
        "decision=unresolved_review;selected_event_subtypes=none"
) -> HFSBurstArbitrationAuditRow {
    HFSBurstArbitrationAuditRow(
        id: rowID,
        pipelineStage: "result_package_test",
        trainID: train.id,
        trainName: train.name,
        hfsCandidateID: candidateID,
        hfsRootCandidateID: rootID,
        hfsStartISIIndex: hfsStartISIIndex,
        hfsEndISIIndex: hfsEndISIIndex,
        conflictStartISIIndex: conflictStartISIIndex ?? hfsStartISIIndex,
        conflictEndISIIndex: conflictEndISIIndex ?? hfsEndISIIndex,
        scoreScaleNote: HFSBurstArbitrationAudit.scoreScaleNote,
        hfsRawScore: rawScore,
        hfsPriority: 100,
        hfsSelectedForAuto: false,
        hfsSelectionStatus: "not_selected",
        hfsAcceptanceRoute: "fixture",
        strongestBurstCandidateID: strongestBurstCandidateID,
        strongestBurstSubtype: strongestBurstSubtype,
        strongestBurstRawScore: strongestBurstRawScore,
        strongestBurstPriority: strongestBurstPriority,
        strongestLongBurstCandidateID: strongestLongBurstCandidateID,
        longBurstRawScore: longBurstRawScore,
        longBurstPriority: longBurstPriority,
        packetEvidenceEventCount: 0,
        packetCount: 0,
        packetCoverage: packetCoverage,
        allSelectedBurstEventCount: 0,
        selectedBurstIICount: 0,
        selectedLongBurstCount: 0,
        suppressedBurstProposalCount: 0,
        pauseLikeThresholdSec: nil,
        pauseLikeBreakCount: 0,
        pauseLikeBreakGroupCount: 0,
        pauseLikeBreakFraction: nil,
        seedBandLowerSec: 0.001,
        seedBandUpperSec: seedBandUpperSec,
        bridgeBandUpperSec: bridgeBandUpperSec,
        seedFraction: 1,
        bridgeFraction: 0,
        hfsShortFraction: 1,
        hfsBridgeFraction: 0,
        hfsLargeFraction: 0,
        hfsCV: 0,
        hfsLV: 0,
        burstPacketLike: false,
        burstDominated: false,
        finalDecision: .unresolvedReview,
        finalSelectedEventSubtypes: [],
        requiresReview: true,
        decisionReason: decisionReason
    )
}

private func resultPackageRun(
    from run: ClassicAnchorDetectionRun,
    appending candidate: ClassicAnchorCandidate,
    auditRow: HFSBurstArbitrationAuditRow
) -> ClassicAnchorDetectionRun {
    resultPackageRun(
        from: run,
        appending: [candidate],
        auditRows: [auditRow]
    )
}

private func resultPackageRun(
    from run: ClassicAnchorDetectionRun,
    appending candidates: [ClassicAnchorCandidate],
    auditRows: [HFSBurstArbitrationAuditRow]
) -> ClassicAnchorDetectionRun {
    let candidatesByTrain = Dictionary(grouping: candidates, by: \.trainID)
    let auditRowsByTrain = Dictionary(grouping: auditRows, by: \.trainID)
    let results = run.results.map { result in
        let appendedCandidates = candidatesByTrain[result.trainID] ?? []
        let appendedAuditRows = auditRowsByTrain[result.trainID] ?? []
        guard !appendedCandidates.isEmpty || !appendedAuditRows.isEmpty else {
            return result
        }
        return ClassicAnchorDetectionResult(
            trainID: result.trainID,
            trainName: result.trainName,
            candidates: result.candidates + appendedCandidates,
            hfsBurstArbitrationAuditRows:
                result.hfsBurstArbitrationAuditRows + appendedAuditRows
        )
    }
    return run.replacingResultsFromTrustedModuleTransform(results)
}

private func resultPackageRun(
    from run: ClassicAnchorDetectionRun,
    replacingCandidatesByTrainID: [String: [ClassicAnchorCandidate]],
    replacingAuditRowsByTrainID: [String: [HFSBurstArbitrationAuditRow]] = [:]
) -> ClassicAnchorDetectionRun {
    let results = run.results.map { result in
        ClassicAnchorDetectionResult(
            trainID: result.trainID,
            trainName: result.trainName,
            candidates:
                replacingCandidatesByTrainID[result.trainID] ?? result.candidates,
            hfsBurstArbitrationAuditRows:
                replacingAuditRowsByTrainID[result.trainID]
                ?? result.hfsBurstArbitrationAuditRows
        )
    }
    return run.replacingResultsFromTrustedModuleTransform(results)
}

private func resultPackageRunRebindingEvidence(
    from run: ClassicAnchorDetectionRun,
    invocationSettingsSnapshot: DetectionRunSettingsSnapshot?,
    datasetMetadataSnapshot: DetectionDatasetMetadataSnapshot?,
    resolvedThresholdEvidence: [ClassicAnchorResolvedThresholdEvidence]
) -> ClassicAnchorDetectionRun {
    ClassicAnchorDetectionRun(
        bandSettings: run.bandSettings,
        qualitySettings: run.qualitySettings,
        resolutions: run.resolutions,
        results: run.results,
        datasetStructuralSeedSummary: run.datasetStructuralSeedSummary,
        datasetRerunProvenance: run.datasetRerunProvenance,
        performanceReport: run.performanceReport,
        datasetISIDistribution: run.datasetISIDistribution,
        invocationSettingsSnapshot: invocationSettingsSnapshot,
        datasetMetadataSnapshot: datasetMetadataSnapshot,
        resolvedThresholdEvidence: resolvedThresholdEvidence,
        runIdentity: run.runIdentity
    )
}

private func resultPackageReviewedInput(
    dataset: SpikeDataset,
    run: ClassicAnchorDetectionRun,
    manualAnnotations: [ManualAnnotation] = [],
    candidateReviews: [STPDCandidateReviewInput] = [],
    candidateDiagnostics: [STPDCandidateDiagnosticInput] = []
) throws -> STPDResultPackageInput {
    let automatic = STPDResultPackageInput.automatic(dataset: dataset, run: run)
    let resolvedAnnotations = try manualAnnotations.map { annotation in
        try #require(
            ManualAnnotationGeometryResolver.resolvingIndicesIfCompatible(
                annotation,
                in: dataset.trains
            )
        )
    }
    let annotationsByTrain = Dictionary(grouping: resolvedAnnotations, by: \.trainID)
    let automaticIntervalRows = automatic.finalISILabelRows.filter { $0.isiIndex > 0 }
    let automaticRowsByTrain = Dictionary(grouping: automaticIntervalRows, by: \.trainID)
    var projectionsByTrain: [String: ManualAnnotationProjection] = [:]

    for train in dataset.trains {
        let autoLabels = Dictionary(
            uniqueKeysWithValues: (automaticRowsByTrain[train.id] ?? [])
                .filter { !$0.autoPattern.isEmpty }
                .map { ($0.isiIndex, $0.autoPattern) }
        )
        projectionsByTrain[train.id] = ManualAnnotationProjector.project(
            train: train,
            autoLabelsByISI: autoLabels,
            annotations: annotationsByTrain[train.id] ?? [],
            honorManualLock: true,
            manualNegativeLabelsEnabled: true,
            minValidISISeconds: run.qualitySettings.artifactThresholdSec
        )
    }

    let rejectedCandidateIDs = Set(candidateReviews.compactMap {
        $0.status == .rejected ? $0.sourceCandidateID : nil
    })
    let finalRows = ReviewedISIExportBuilder.build(
        dataset: dataset,
        autoAnnotations: automatic.finalEvents,
        projectionsByTrain: projectionsByTrain,
        reviewRejectedCandidateIDs: rejectedCandidateIDs
    )
    let rejectedISIsByTrain = Dictionary(
        grouping: automaticIntervalRows.filter {
            rejectedCandidateIDs.contains($0.autoCandidateID)
        },
        by: \.trainID
    )
    .mapValues { Set($0.map(\.isiIndex)) }
    var lockSuppressedByTrain: [String: Set<Int>] = [:]
    var vetoedBurstISIsByTrain: [String: Set<Int>] = [:]
    var manualBurstISIsByTrain: [String: Set<Int>] = [:]
    for train in dataset.trains {
        let projection = projectionsByTrain[train.id]
        lockSuppressedByTrain[train.id] =
            (projection?.autoBlockedByManualLockISIs ?? [])
            .union(rejectedISIsByTrain[train.id] ?? [])
        vetoedBurstISIsByTrain[train.id] =
            projection?.autoBurstBlockedByVetoISIs ?? []
        manualBurstISIsByTrain[train.id] = Set(
            (projection?.manualPositiveLabelByISI ?? [:]).compactMap {
                ManualAnnotationProjector.burstFamilyLabels.contains($0.value)
                    ? $0.key
                    : nil
            }
        )
    }
    let finalEvents = ManualAnnotationProjector.projectPublicEventAnnotations(
        automatic.finalEvents,
        vetoedBurstISIsByTrain: vetoedBurstISIsByTrain,
        lockSuppressedISIsByTrain: lockSuppressedByTrain,
        validatedManualBurstSupportISIsByTrain: manualBurstISIsByTrain,
        trainsByID: Dictionary(uniqueKeysWithValues: dataset.trains.map { ($0.id, $0) })
    ).annotations

    var reviewLinks: [STPDResultReviewLink] = []
    for annotation in resolvedAnnotations {
        guard let lower = annotation.startISIIndex,
              let upper = annotation.endISIIndex else {
            continue
        }
        let expectedRows = finalRows.filter { row in
            guard row.trainID == annotation.trainID,
                  min(lower, upper) ... max(lower, upper) ~= row.isiIndex else {
                return false
            }
            switch annotation.polarity {
            case .positive:
                return row.finalSource == ReviewedISIExportBuilder.sourceManualPositive
                    && row.finalPattern == annotation.label.finalPatternString
            case .negative:
                return row.finalSource == ReviewedISIExportBuilder.sourceManualVetoRemoved
            }
        }
        reviewLinks.append(contentsOf: expectedRows.map {
            STPDResultReviewLink(
                trainID: $0.trainID,
                isiIndex: $0.isiIndex,
                evidence: .manualAnnotation(annotation.id)
            )
        })
    }
    for review in candidateReviews where review.status.grantsReviewAuthority {
        reviewLinks.append(contentsOf: automaticIntervalRows
            .filter { $0.autoCandidateID == review.sourceCandidateID }
            .map {
                STPDResultReviewLink(
                    trainID: $0.trainID,
                    isiIndex: $0.isiIndex,
                    evidence: .candidateReview(
                        sourceCandidateID: review.sourceCandidateID
                    )
                )
            })
    }

    return STPDResultPackageInput(
        dataset: dataset,
        run: run,
        sourceMode: .reviewed,
        finalEvents: finalEvents,
        finalISILabelRows: finalRows,
        manualAnnotations: manualAnnotations,
        candidateReviews: candidateReviews,
        reviewLinks: reviewLinks,
        candidateDiagnostics: candidateDiagnostics
    )
}

private func resultPackageManualTonicAnnotation(
    dataset: SpikeDataset,
    run: ClassicAnchorDetectionRun,
    id: UUID,
    note: String = "",
    ordinal: Int = 0
) throws -> ManualAnnotation {
    let automatic = STPDResultPackageInput.automatic(dataset: dataset, run: run)
    let candidates = automatic.finalISILabelRows.filter {
        $0.isiIndex > 0 && $0.autoPattern != ManualAnnotationLabel.tonic.rawValue
    }
    let row = try #require(candidates.dropFirst(ordinal).first)
    let train = try #require(dataset.trains.first { $0.id == row.trainID })
    return ManualAnnotation(
        id: id,
        trainID: train.id,
        label: .tonic,
        startSec: train.timestampsSec[row.isiIndex - 1],
        endSec: train.timestampsSec[row.isiIndex],
        note: note,
        createdAt: Date(timeIntervalSince1970: 100),
        updatedAt: Date(timeIntervalSince1970: 200)
    )
}

@Test
func resultPackageBuildsAllThirteenNormalizedTables() throws {
    let fixture = resultPackageFixture()
    let package = try STPDResultPackageBuilder.build(
        .automatic(dataset: fixture.dataset, run: fixture.run)
    )

    #expect(Set(package.tables.keys) == Set(STPDResultTable.allCases))
    #expect(package.manifest.tables.count == STPDResultTable.allCases.count)
    #expect(package.manifest.tables.map(\.fileName) == STPDResultTable.allCases.map(\.rawValue))
    #expect(package.manifest.schemaVersion == STPDResultSchema.version)
    #expect(package.manifest.detectorVersion == STPDResultSchema.detectorVersion)
    #expect(package.manifest.ownerName == "Zhou Houchun")
    #expect(package.manifest.ownerEmail == "zhouhouchun@outlook.com")
    #expect(package.manifest.sourceMode == STPDResultPackageSourceMode.automatic.rawValue)
    #expect(package.manifest.buildIdentifier == resultPackageBuildCommit)
    #expect(package.manifest.buildIdentifierKind == "caller_supplied_unattested")
    #expect(package.manifest.buildReproducibilityAttested == false)

    let expectedISICount = fixture.dataset.trains.reduce(0) {
        $0 + max(0, $1.spikeCount - 1)
    }
    let isiTable = try #require(package.table(.isiLabelsFinal))
    #expect(isiTable.rowCount == expectedISICount)

    let candidateCount = fixture.run.candidates.count
    #expect(package.table(.candidateLedger)?.rowCount == candidateCount)
    #expect(package.table(.candidateFeatures)?.rowCount == candidateCount)
    #expect(package.table(.finalDecisions)?.rowCount == candidateCount)

    let metadata = try #require(package.table(.runMetadata))
    #expect(metadata.rowCount == 1)
    #expect(metadata.value(row: 0, column: "source_mode") == "automatic")
    #expect(metadata.value(row: 0, column: "final_isi_count") == String(expectedISICount))
    #expect(metadata.value(row: 0, column: "build_identifier") == resultPackageBuildCommit)
    #expect(metadata.value(row: 0, column: "build_identifier_kind")
        == "caller_supplied_unattested")
    #expect(metadata.value(row: 0, column: "build_reproducibility_attested") == "false")
    #expect(metadata.value(row: 0, column: "dataset_name") == fixture.dataset.name)
    #expect(metadata.value(row: 0, column: "dataset_source")
        == fixture.dataset.sourceDescription)

    let checks = try #require(package.table(.resultConsistencyCheck))
    #expect(checks.rowCount >= 6)
    #expect(try resultPackageColumn(checks, "status").allSatisfy { $0 == "pass" })
    #expect(try resultPackageColumn(checks, "severity").allSatisfy { $0 == "info" })
    #expect(Set(try resultPackageColumn(checks, "check_id")).isSuperset(of: [
        "required_table_set",
        "run_identity",
        "primary_keys",
        "source_mode",
        "candidate_one_to_one",
        "candidate_foreign_keys",
        "isi_complete_coverage",
    ]))
    let requiredTableRowIndex = try #require(
        resultPackageColumn(checks, "check_id").firstIndex(of: "required_table_set")
    )
    #expect(checks.value(row: requiredTableRowIndex, column: "details")
        == "all required tables for this validation stage are present")
}

@Test
func resultPackageBindsRequestedAndEffectiveParametersToDetectorEntryPoint() throws {
    let fixture = resultPackageFixture()
    let package = try STPDResultPackageBuilder.build(
        .automatic(dataset: fixture.dataset, run: fixture.run)
    )
    let snapshot = try #require(fixture.run.runIdentity.settingsSnapshot)
    let parameters = try #require(package.table(.parametersReport))
    let requestedFingerprint = try resultPackageRowFingerprint(
        parameters,
        columns: ["parameter_key", "requested_value"]
    )
    #expect(requestedFingerprint == Set(snapshot.entries.map {
        [$0.key, $0.value].joined(separator: "\u{1}")
    }))

    let resolved = try #require(package.table(.resolvedParameters))
    let scopeTypeIndex = try #require(resolved.headers.firstIndex(of: "scope_type"))
    let scopeIDIndex = try #require(resolved.headers.firstIndex(of: "scope_id"))
    let keyIndex = try #require(resolved.headers.firstIndex(of: "parameter_key"))
    let requestedIndex = try #require(resolved.headers.firstIndex(of: "requested_value"))
    let effectiveIndex = try #require(resolved.headers.firstIndex(of: "effective_value"))
    let sourceIndex = try #require(resolved.headers.firstIndex(of: "source"))
    let modeIndex = try #require(resolved.headers.firstIndex(of: "resolution_mode"))
    let noteIndex = try #require(resolved.headers.firstIndex(of: "resolution_note"))

    let requestedRows = resolved.rows.filter { $0[scopeTypeIndex] == "run" }
    #expect(Set(requestedRows.map {
        [$0[keyIndex], $0[requestedIndex]].joined(separator: "\u{1}")
    }) == requestedFingerprint)

    #expect(fixture.run.resolvedThresholdEvidence.count == fixture.dataset.trains.count)
    for evidence in fixture.run.resolvedThresholdEvidence {
        let stageRows = resolved.rows.filter {
            $0[scopeIDIndex] == evidence.trainID
                && $0[keyIndex] == "resolution.stage_path"
        }
        #expect(stageRows.count == 1)
        #expect(stageRows.first?[effectiveIndex] == evidence.stagePath)
        #expect(stageRows.first?[sourceIndex] == "detector_stage")

        let profile = evidence.effectiveProfile
        let expectedEffectiveValues: [String: String] = [
            "burst.seed_lower_sec": STPDCanonicalValue.double(profile.burst.lowerSec),
            "burst.seed_upper_sec": STPDCanonicalValue.double(profile.burst.upperSec),
            "burst.bridge_upper_sec":
                STPDCanonicalValue.double(profile.burst.bridgeUpperSec),
            "burst.min_spikes": STPDCanonicalValue.int(profile.burst.minSpikes),
            "burst.classic_max_spikes":
                STPDCanonicalValue.int(profile.burst.classicMaxSpikes),
            "burst.long_min_spikes":
                STPDCanonicalValue.int(profile.burst.longMinSpikes),
            "burst.long_max_spikes":
                STPDCanonicalValue.int(profile.burst.longMaxSpikes),
            "hfs.seed_lower_sec": STPDCanonicalValue.double(profile.hfs.lowerSec),
            "hfs.seed_upper_sec": STPDCanonicalValue.double(profile.hfs.upperSec),
            "hfs.min_spikes": STPDCanonicalValue.int(profile.hfs.minSpikes),
            "hfs.min_duration_sec":
                STPDCanonicalValue.double(profile.hfs.minDurationSec),
            "hf_tonic.isi_floor_sec":
                STPDCanonicalValue.double(profile.hfTonic.lowerSec),
            "hf_tonic.isi_upper_sec":
                STPDCanonicalValue.double(profile.hfTonic.upperSec),
            "hf_tonic.min_spikes":
                STPDCanonicalValue.int(profile.hfTonic.minSpikes),
            "tonic.isi_lower_sec": STPDCanonicalValue.double(profile.tonic.lowerSec),
            "tonic.isi_upper_sec": STPDCanonicalValue.double(profile.tonic.upperSec),
            "tonic.min_spikes": STPDCanonicalValue.int(profile.tonic.minSpikes),
            "pause.isi_lower_sec": STPDCanonicalValue.double(profile.pause.lowerSec),
        ]
        let provenanceByKey = Dictionary(
            uniqueKeysWithValues: evidence.resolutionProvenance.map { ($0.key, $0) }
        )
        for (key, expectedValue) in expectedEffectiveValues {
            let rows = resolved.rows.filter {
                $0[scopeIDIndex] == evidence.trainID && $0[keyIndex] == key
            }
            let row = try #require(rows.first)
            #expect(rows.count == 1)
            #expect(row[effectiveIndex] == expectedValue)
            #expect(row[sourceIndex]
                == (provenanceByKey[key]?.source.rawValue ?? "post_clamp_effective"))
            #expect(row[modeIndex]
                == (provenanceByKey[key]?.mode.rawValue
                    ?? ThresholdMode.automatic.rawValue))
            #expect(row[noteIndex].contains(
                "post_clamp_effective_captured_at_authoritative_rerun"
            ))
        }
    }
}

@Test
func resultPackageRejectsDetectorEntryPointEvidenceDrift() throws {
    let fixture = resultPackageFixture()
    let wrongMetadataRun = resultPackageRunRebindingEvidence(
        from: fixture.run,
        invocationSettingsSnapshot: fixture.run.invocationSettingsSnapshot,
        datasetMetadataSnapshot: DetectionDatasetMetadataSnapshot(
            name: "silently-renamed-dataset",
            sourceDescription: fixture.dataset.sourceDescription
        ),
        resolvedThresholdEvidence: fixture.run.resolvedThresholdEvidence
    )
    #expect(throws: STPDResultPackageError.self) {
        _ = try STPDResultPackageBuilder.build(
            .automatic(dataset: fixture.dataset, run: wrongMetadataRun)
        )
    }

    let missingInvocationRun = resultPackageRunRebindingEvidence(
        from: fixture.run,
        invocationSettingsSnapshot: nil,
        datasetMetadataSnapshot: fixture.run.datasetMetadataSnapshot,
        resolvedThresholdEvidence: fixture.run.resolvedThresholdEvidence
    )
    #expect(throws: STPDResultPackageError.self) {
        _ = try STPDResultPackageBuilder.build(
            .automatic(dataset: fixture.dataset, run: missingInvocationRun)
        )
    }

    let missingThresholdEvidenceRun = resultPackageRunRebindingEvidence(
        from: fixture.run,
        invocationSettingsSnapshot: fixture.run.invocationSettingsSnapshot,
        datasetMetadataSnapshot: fixture.run.datasetMetadataSnapshot,
        resolvedThresholdEvidence: Array(fixture.run.resolvedThresholdEvidence.dropLast())
    )
    #expect(throws: STPDResultPackageError.self) {
        _ = try STPDResultPackageBuilder.build(
            .automatic(dataset: fixture.dataset, run: missingThresholdEvidenceRun)
        )
    }

    let contradictoryBandRun = ClassicAnchorDetectionRun(
        bandSettings: TrainAdaptiveBandSettings(
            minValidISISec: fixture.run.bandSettings.minValidISISec * 2,
            histogramBinWidthSec: fixture.run.bandSettings.histogramBinWidthSec
        ),
        qualitySettings: fixture.run.qualitySettings,
        resolutions: fixture.run.resolutions,
        results: fixture.run.results,
        datasetStructuralSeedSummary: fixture.run.datasetStructuralSeedSummary,
        datasetRerunProvenance: fixture.run.datasetRerunProvenance,
        performanceReport: fixture.run.performanceReport,
        datasetISIDistribution: fixture.run.datasetISIDistribution,
        invocationSettingsSnapshot: fixture.run.invocationSettingsSnapshot,
        datasetMetadataSnapshot: fixture.run.datasetMetadataSnapshot,
        resolvedThresholdEvidence: fixture.run.resolvedThresholdEvidence,
        runIdentity: fixture.run.runIdentity
    )
    #expect(throws: STPDResultPackageError.self) {
        _ = try STPDResultPackageBuilder.build(
            .automatic(dataset: fixture.dataset, run: contradictoryBandRun)
        )
    }
}

@Test
func resultPackageAuthorityRejectsPublicRewrapAndCoordinatedThresholdReplacement() throws {
    let fixture = resultPackageFixture()
    #expect(fixture.run.hasValidResultPackageAuthority)

    let exactPublicRewrap = resultPackageRunRebindingEvidence(
        from: fixture.run,
        invocationSettingsSnapshot: fixture.run.invocationSettingsSnapshot,
        datasetMetadataSnapshot: fixture.run.datasetMetadataSnapshot,
        resolvedThresholdEvidence: fixture.run.resolvedThresholdEvidence
    )
    #expect(!exactPublicRewrap.hasValidResultPackageAuthority)
    #expect(throws: STPDResultPackageError.self) {
        _ = try STPDResultPackageBuilder.build(
            .automatic(dataset: fixture.dataset, run: exactPublicRewrap)
        )
    }

    let firstEvidence = try #require(fixture.run.resolvedThresholdEvidence.first)
    let originalProfile = firstEvidence.effectiveProfile
    let originalBurst = originalProfile.burst
    let replacementBurst = ResolvedFamilyThresholds(
        lowerSec: originalBurst.lowerSec,
        upperSec: originalBurst.upperSec * 1.01,
        bridgeUpperSec: originalBurst.bridgeUpperSec,
        minSpikes: originalBurst.minSpikes,
        classicMaxSpikes: originalBurst.classicMaxSpikes,
        longMinSpikes: originalBurst.longMinSpikes,
        longMaxSpikes: originalBurst.longMaxSpikes,
        minDurationSec: originalBurst.minDurationSec
    )
    var replacementProfile = ResolvedThresholdProfile(
        burst: replacementBurst,
        hfs: originalProfile.hfs,
        hfTonic: originalProfile.hfTonic,
        tonic: originalProfile.tonic,
        pause: originalProfile.pause,
        provenance: originalProfile.provenance
    )
    replacementProfile.learnedProvenanceByKey =
        originalProfile.learnedProvenanceByKey
    let replacementEvidence = ClassicAnchorResolvedThresholdEvidence(
        trainID: firstEvidence.trainID,
        trainName: firstEvidence.trainName,
        stagePath: firstEvidence.stagePath,
        effectiveProfile: replacementProfile,
        resolutionProvenance: firstEvidence.resolutionProvenance,
        learnedProvenanceByKey: firstEvidence.learnedProvenanceByKey
    )
    let coordinatedEvidence = [replacementEvidence]
        + fixture.run.resolvedThresholdEvidence.dropFirst()
    let coordinatedPublicRewrap = resultPackageRunRebindingEvidence(
        from: fixture.run,
        invocationSettingsSnapshot: fixture.run.invocationSettingsSnapshot,
        datasetMetadataSnapshot: fixture.run.datasetMetadataSnapshot,
        resolvedThresholdEvidence: coordinatedEvidence
    )
    #expect(!coordinatedPublicRewrap.hasValidResultPackageAuthority)
    #expect(throws: STPDResultPackageError.self) {
        _ = try STPDResultPackageBuilder.build(
            .automatic(dataset: fixture.dataset, run: coordinatedPublicRewrap)
        )
    }
}

@Test
func resultPackageHybridFrameworkPreservesDetectorAuthoritySeal() throws {
    let fixture = resultPackageFixture()
    let hybridRun = HybridPatternDetectionFramework.run(
        dataset: fixture.dataset,
        bandSettings: fixture.run.bandSettings,
        qualitySettings: fixture.run.qualitySettings,
        buildCommit: resultPackageBuildCommit
    )

    #expect(hybridRun.hasValidResultPackageAuthority)
    let package = try STPDResultPackageBuilder.build(
        .automatic(dataset: fixture.dataset, run: hybridRun)
    )
    #expect(package.identity.runID == hybridRun.runIdentity.runID)
    #expect(package.table(.runMetadata)?.rowCount == 1)
}

@Test
func resultPackageExportsQCEventEvidenceSubtypesAndExactConsistencyChecks() throws {
    let fixture = resultPackageFixture()
    let package = try STPDResultPackageBuilder.build(
        .automatic(dataset: fixture.dataset, run: fixture.run)
    )

    let checks = try #require(package.table(.resultConsistencyCheck))
    #expect(Set(try resultPackageColumn(checks, "check_id")) == [
        "candidate_foreign_keys",
        "candidate_one_to_one",
        "event_source_evidence",
        "isi_complete_coverage",
        "isi_qc_provenance",
        "primary_keys",
        "required_table_set",
        "review_authority",
        "run_identity",
        "source_mode",
    ])
    #expect(try resultPackageColumn(checks, "status").allSatisfy { $0 == "pass" })
    #expect(try resultPackageColumn(checks, "severity").allSatisfy { $0 == "info" })

    let isiTable = try #require(package.table(.isiLabelsFinal))
    let isiValues = try resultPackageColumn(isiTable, "isi_sec")
    let artifactThresholds = try resultPackageColumn(
        isiTable,
        "qc_artifact_threshold_sec"
    )
    let refractoryThresholds = try resultPackageColumn(
        isiTable,
        "qc_refractory_threshold_sec"
    )
    let qcClasses = try resultPackageColumn(isiTable, "isi_qc_class")
    let artifactStatuses = try resultPackageColumn(isiTable, "artifact_floor_status")
    let refractoryFlags = try resultPackageColumn(isiTable, "qc_refractory_suspect")
    #expect(Set(artifactThresholds) == [
        STPDCanonicalValue.double(fixture.run.qualitySettings.artifactThresholdSec),
    ])
    #expect(Set(refractoryThresholds) == [
        STPDCanonicalValue.double(
            fixture.run.qualitySettings.refractorySuspectThresholdSec
        ),
    ])
    for rowIndex in isiValues.indices {
        let isi = try #require(Double(isiValues[rowIndex]))
        let artifactThreshold = try #require(Double(artifactThresholds[rowIndex]))
        let refractoryThreshold = try #require(Double(refractoryThresholds[rowIndex]))
        let expectedClass: String
        if isi < artifactThreshold {
            expectedClass = "artifact_below_floor"
        } else if refractoryThreshold > artifactThreshold && isi < refractoryThreshold {
            expectedClass = "refractory_suspect"
        } else {
            expectedClass = "valid"
        }
        #expect(qcClasses[rowIndex] == expectedClass)
        #expect(artifactStatuses[rowIndex]
            == (isi < artifactThreshold
                ? "below_artifact_floor"
                : "at_or_above_artifact_floor"))
        #expect(refractoryFlags[rowIndex]
            == (expectedClass == "refractory_suspect" ? "true" : "false"))
    }
    #expect(try resultPackageColumn(isiTable, "train_qc_valid_isi_count")
        .allSatisfy { Int($0) != nil })
    #expect(try resultPackageColumn(isiTable, "train_qc_warning_level")
        .allSatisfy { !$0.isEmpty })

    let events = try #require(package.table(.eventsFinal))
    let eventUIDs = Set(try resultPackageColumn(events, "event_uid"))
    let eventLabels = try resultPackageColumn(events, "final_label")
    let tonicSubtypes = try resultPackageColumn(events, "state_tonic_subtype")
    for rowIndex in eventLabels.indices
    where eventLabels[rowIndex] == ClassicAnchorLabel.tonic.rawValue
        || eventLabels[rowIndex] == ClassicAnchorLabel.highFrequencyTonic.rawValue {
        #expect(!tonicSubtypes[rowIndex].isEmpty)
    }

    let ledgerUIDs = Set(try resultPackageColumn(
        #require(package.table(.candidateLedger)),
        "candidate_uid"
    ))
    let diagnostics = try #require(package.table(.candidateDiagnosticAudit))
    let evidenceKinds = try resultPackageColumn(diagnostics, "evidence_kind")
    let diagnosticEventUIDs = try resultPackageColumn(diagnostics, "event_uid")
    let diagnosticCandidateUIDs = try resultPackageColumn(diagnostics, "candidate_uid")
    for rowIndex in evidenceKinds.indices where evidenceKinds[rowIndex] == "event_source" {
        #expect(eventUIDs.contains(diagnosticEventUIDs[rowIndex]))
        #expect(ledgerUIDs.contains(diagnosticCandidateUIDs[rowIndex]))
    }
}

@Test
func resultPackageISIEndpointsDistinguishArrayIndicesFromScientificOrdinals() throws {
    let fixture = resultPackageFixture()
    let package = try STPDResultPackageBuilder.build(
        .automatic(dataset: fixture.dataset, run: fixture.run)
    )
    let table = try #require(package.table(.isiLabelsFinal))

    #expect(!table.headers.contains("spike_array_index"))
    #expect(!table.headers.contains("spike_ordinal"))

    let trainIndex = try #require(table.headers.firstIndex(of: "train_id"))
    let isiIndex = try #require(table.headers.firstIndex(of: "isi_index"))
    let leftArrayIndex = try #require(
        table.headers.firstIndex(of: "left_spike_array_index")
    )
    let rightArrayIndex = try #require(
        table.headers.firstIndex(of: "right_spike_array_index")
    )
    let leftOrdinalIndex = try #require(
        table.headers.firstIndex(of: "left_spike_ordinal")
    )
    let rightOrdinalIndex = try #require(
        table.headers.firstIndex(of: "right_spike_ordinal")
    )
    let firstISI = try #require(table.rows.first {
        $0[trainIndex] == fixture.dataset.trains[0].id
            && $0[isiIndex] == "1"
    })

    #expect(firstISI[leftArrayIndex] == "0")
    #expect(firstISI[rightArrayIndex] == "1")
    #expect(firstISI[leftOrdinalIndex] == "1")
    #expect(firstISI[rightOrdinalIndex] == "2")
}

@Test
func resultPackageIsByteDeterministicForTheSameRun() throws {
    let fixture = resultPackageFixture()
    let input = STPDResultPackageInput.automatic(
        dataset: fixture.dataset,
        run: fixture.run
    )
    let first = try STPDResultPackageBuilder.build(input)
    let second = try STPDResultPackageBuilder.build(input)

    for table in STPDResultTable.allCases {
        #expect(first.table(table)?.csvData == second.table(table)?.csvData)
    }
    #expect(try first.manifest.encodedData() == second.manifest.encodedData())

    for entry in first.manifest.tables {
        let table = try #require(
            STPDResultTable.allCases.first { $0.rawValue == entry.fileName }
        )
        let data = try #require(first.table(table)?.csvData)
        #expect(entry.sha256 == STPDStableIdentifier.digest(data))
    }
}

@Test
func resultPackageStableUIDsSurviveAnEquivalentDetectorRerun() throws {
    let firstFixture = resultPackageFixture()
    let secondFixture = resultPackageFixture()
    let first = try STPDResultPackageBuilder.build(
        .automatic(dataset: firstFixture.dataset, run: firstFixture.run)
    )
    let second = try STPDResultPackageBuilder.build(
        .automatic(dataset: secondFixture.dataset, run: secondFixture.run)
    )

    #expect(first.identity.runID != second.identity.runID)
    #expect(first.identity.datasetDigest == second.identity.datasetDigest)
    #expect(first.identity.settingsDigest == second.identity.settingsDigest)

    let ledgerColumns = [
        "candidate_uid", "train_id", "candidate_layer", "candidate_class",
        "start_isi_index", "end_isi_index",
        "start_spike_ordinal", "end_spike_ordinal",
    ]
    let decisionColumns = [
        "candidate_uid", "final_label", "gate_status", "action",
        "selection_status", "semantic_track", "event_track_class",
    ]
    let eventColumns = [
        "event_uid", "train_id", "final_label", "semantic_track",
        "start_isi_index", "end_isi_index",
        "start_spike_ordinal", "end_spike_ordinal",
    ]
    #expect(
        try resultPackageRowFingerprint(
            #require(first.table(.candidateLedger)),
            columns: ledgerColumns
        ) ==
        resultPackageRowFingerprint(
            #require(second.table(.candidateLedger)),
            columns: ledgerColumns
        )
    )
    #expect(
        try resultPackageRowFingerprint(
            #require(first.table(.finalDecisions)),
            columns: decisionColumns
        ) ==
        resultPackageRowFingerprint(
            #require(second.table(.finalDecisions)),
            columns: decisionColumns
        )
    )
    #expect(
        try resultPackageRowFingerprint(
            #require(first.table(.eventsFinal)),
            columns: eventColumns
        ) ==
        resultPackageRowFingerprint(
            #require(second.table(.eventsFinal)),
            columns: eventColumns
        )
    )
}

@Test
func resultPackagePreservesAdjacentSameLabelSourceEventsOneForOne() throws {
    let fixture = resultPackageFixture()
    let train = fixture.dataset.trains[0]
    let first = resultPackageHFSCandidate(
        train: train,
        id: "adjacent-burst-a",
        candidateLayer: "adjacent_event_fixture",
        nISI: 2,
        nSpikes: 3,
        candidateClass: ClassicAnchorLabel.burst.rawValue,
        finalLabel: .burst,
        decisionPath: "adjacent_event_fixture_a",
        action: "accept_burst",
        priority: 200,
        selectedForAuto: true,
        selectionStatus: "selected",
        startISIIndex: 1,
        endISIIndex: 2,
        anchorFamily: "burst"
    )
    let second = resultPackageHFSCandidate(
        train: train,
        id: "adjacent-burst-b",
        candidateLayer: "adjacent_event_fixture",
        nISI: 2,
        nSpikes: 3,
        candidateClass: ClassicAnchorLabel.burst.rawValue,
        finalLabel: .burst,
        decisionPath: "adjacent_event_fixture_b",
        action: "accept_burst",
        priority: 200,
        selectedForAuto: true,
        selectionStatus: "selected",
        startISIIndex: 3,
        endISIIndex: 4,
        anchorFamily: "burst"
    )
    let run = resultPackageRun(
        from: fixture.run,
        replacingCandidatesByTrainID: [train.id: [second, first]],
        replacingAuditRowsByTrainID: [train.id: []]
    )
    let package = try STPDResultPackageBuilder.build(
        .automatic(dataset: fixture.dataset, run: run)
    )
    let events = try #require(package.table(.eventsFinal))
    let trainIDIndex = try #require(events.headers.firstIndex(of: "train_id"))
    let labelIndex = try #require(events.headers.firstIndex(of: "final_label"))
    let startIndex = try #require(events.headers.firstIndex(of: "start_isi_index"))
    let endIndex = try #require(events.headers.firstIndex(of: "end_isi_index"))
    let sourceEventIndex = try #require(events.headers.firstIndex(of: "source_event_ids"))
    let sourceCandidateIndex = try #require(
        events.headers.firstIndex(of: "source_candidate_uids")
    )
    let targetRows = events.rows
        .filter {
            $0[trainIDIndex] == train.id &&
                $0[labelIndex] == ClassicAnchorLabel.burst.rawValue
        }
        .sorted { Int($0[startIndex])! < Int($1[startIndex])! }

    #expect(targetRows.count == 2)
    #expect(targetRows.map { $0[startIndex] } == ["1", "3"])
    #expect(targetRows.map { $0[endIndex] } == ["2", "4"])
    #expect(targetRows.allSatisfy {
        STPDCanonicalValue.parseStringList($0[sourceEventIndex])?.count == 1
    })
    #expect(targetRows.allSatisfy {
        STPDCanonicalValue.parseStringList($0[sourceCandidateIndex])?.count == 1
    })
}

@Test
func resultPackagePartialManualRelabelSplitsAutomaticSourceEvent() throws {
    let fixture = resultPackageFixture()
    let train = fixture.dataset.trains[0]
    let candidate = resultPackageHFSCandidate(
        train: train,
        id: "partial-manual-source-burst",
        candidateLayer: "partial_manual_source_fixture",
        score: 0.8,
        nISI: 5,
        nSpikes: 6,
        candidateClass: ClassicAnchorLabel.burst.rawValue,
        finalLabel: .burst,
        decisionPath: "partial_manual_source_fixture",
        action: "accept_burst",
        priority: 240,
        selectedForAuto: true,
        selectionStatus: "selected",
        startISIIndex: 1,
        endISIIndex: 5,
        anchorFamily: "burst"
    )
    let run = resultPackageRun(
        from: fixture.run,
        replacingCandidatesByTrainID: [train.id: [candidate]],
        replacingAuditRowsByTrainID: [train.id: []]
    )
    let annotationID = UUID(uuidString: "11111111-2222-3333-4444-555555555555")!
    let annotation = ManualAnnotation(
        id: annotationID,
        trainID: train.id,
        label: .tonic,
        startSec: train.timestampsSec[2],
        endSec: train.timestampsSec[3],
        note: "relabel only ISI 3",
        createdAt: Date(timeIntervalSince1970: 100),
        updatedAt: Date(timeIntervalSince1970: 200)
    )
    let input = try resultPackageReviewedInput(
        dataset: fixture.dataset,
        run: run,
        manualAnnotations: [annotation]
    )
    let package = try STPDResultPackageBuilder.build(input)
    let events = try #require(package.table(STPDResultTable.eventsFinal))
    let trainIndex = try #require(events.headers.firstIndex(of: "train_id"))
    let labelIndex = try #require(events.headers.firstIndex(of: "final_label"))
    let startIndex = try #require(events.headers.firstIndex(of: "start_isi_index"))
    let endIndex = try #require(events.headers.firstIndex(of: "end_isi_index"))
    let sourceIndex = try #require(events.headers.firstIndex(of: "source_event_ids"))
    let authorityIndex = try #require(events.headers.firstIndex(of: "authority_origin"))
    let targetRows: [[String]] = events.rows.filter { row in
        row[trainIndex] == train.id
    }

    var burstRows: [[String]] = targetRows.filter { row in
        row[labelIndex] == ClassicAnchorLabel.burst.rawValue
    }
    burstRows.sort { lhs, rhs in
        Int(lhs[startIndex])! < Int(rhs[startIndex])!
    }
    #expect(burstRows.map { [$0[startIndex], $0[endIndex]] } == [
        ["1", "2"],
        ["4", "5"],
    ])
    #expect(burstRows.allSatisfy {
        STPDCanonicalValue.parseStringList($0[sourceIndex])?.count == 1
    })
    let tonicRows: [[String]] = targetRows.filter { row in
        row[labelIndex] == ManualAnnotationLabel.tonic.rawValue &&
            row[startIndex] == "3" &&
            row[endIndex] == "3"
    }
    #expect(tonicRows.count == 1)
    #expect(tonicRows[0][authorityIndex] == "manual_only")
}

@Test
func resultPackageSameLabelManualPositiveDoesNotInheritAutomaticTonicEvidence() throws {
    let fixture = resultPackageFixture()
    let train = fixture.dataset.trains[0]
    var candidate = resultPackageHFSCandidate(
        train: train,
        id: "manual-inside-irregular-tonic",
        candidateLayer: "manual_tonic_isolation_fixture",
        nISI: 5,
        nSpikes: 6,
        candidateClass: ClassicAnchorLabel.tonic.rawValue,
        finalLabel: .tonic,
        action: "accept_tonic",
        priority: 240,
        selectedForAuto: true,
        selectionStatus: "selected",
        startISIIndex: 1,
        endISIIndex: 5,
        anchorFamily: "tonic"
    )
    candidate.stateTonicSubtype = "irregular"
    let run = resultPackageRun(
        from: fixture.run,
        replacingCandidatesByTrainID: [train.id: [candidate]],
        replacingAuditRowsByTrainID: [train.id: []]
    )
    let annotation = ManualAnnotation(
        id: UUID(uuidString: "12121212-3434-5656-7878-909090909090")!,
        trainID: train.id,
        label: .tonic,
        startSec: train.timestampsSec[2],
        endSec: train.timestampsSec[3],
        note: "manual tonic inside automatic irregular tonic",
        createdAt: Date(timeIntervalSince1970: 100),
        updatedAt: Date(timeIntervalSince1970: 200)
    )
    let package = try STPDResultPackageBuilder.build(
        resultPackageReviewedInput(
            dataset: fixture.dataset,
            run: run,
            manualAnnotations: [annotation]
        )
    )
    let events = try #require(package.table(.eventsFinal))
    let trainIndex = try #require(events.headers.firstIndex(of: "train_id"))
    let labelIndex = try #require(events.headers.firstIndex(of: "final_label"))
    let startIndex = try #require(events.headers.firstIndex(of: "start_isi_index"))
    let endIndex = try #require(events.headers.firstIndex(of: "end_isi_index"))
    let tonicSubtypeIndex = try #require(
        events.headers.firstIndex(of: "state_tonic_subtype")
    )
    let hfsSubtypeIndex = try #require(
        events.headers.firstIndex(of: "state_high_frequency_subtypes")
    )
    let sourceCandidateIndex = try #require(
        events.headers.firstIndex(of: "source_candidate_uids")
    )
    let supportIndex = try #require(
        events.headers.firstIndex(of: "automatic_support_isi_indices")
    )
    let auditSubtypeIndex = try #require(
        events.headers.firstIndex(of: "audit_recommended_subtype")
    )
    let auditStatusIndex = try #require(
        events.headers.firstIndex(of: "audit_review_status")
    )
    let decisionPathIndex = try #require(
        events.headers.firstIndex(of: "decision_path")
    )
    let authorityIndex = try #require(
        events.headers.firstIndex(of: "authority_origin")
    )
    let targetRows = events.rows
        .filter {
            $0[trainIndex] == train.id
                && $0[labelIndex] == ClassicAnchorLabel.tonic.rawValue
                && (Int($0[startIndex]) ?? 0) <= 5
        }
        .sorted { Int($0[startIndex])! < Int($1[startIndex])! }

    try #require(targetRows.count == 3)
    #expect(targetRows.map { [$0[startIndex], $0[endIndex]] } == [
        ["1", "2"],
        ["3", "3"],
        ["4", "5"],
    ])
    let automaticRows = [targetRows[0], targetRows[2]]
    #expect(automaticRows.allSatisfy {
        $0[tonicSubtypeIndex] == "irregular"
            && $0[auditSubtypeIndex] == "classic_tonic"
            && $0[authorityIndex] == "automatic"
            && STPDCanonicalValue.parseStringList($0[sourceCandidateIndex])?.count == 1
            && !(STPDCanonicalValue.parseStringList($0[supportIndex]) ?? []).isEmpty
    })

    let manualRow = targetRows[1]
    #expect(manualRow[tonicSubtypeIndex].isEmpty)
    #expect(STPDCanonicalValue.parseStringList(manualRow[hfsSubtypeIndex]) == [])
    #expect(STPDCanonicalValue.parseStringList(manualRow[sourceCandidateIndex]) == [])
    #expect(STPDCanonicalValue.parseStringList(manualRow[supportIndex]) == [])
    #expect(manualRow[auditSubtypeIndex] == "manual_positive")
    #expect(manualRow[auditStatusIndex] == "manual")
    #expect(manualRow[decisionPathIndex] == "manual_positive_public_projection")
    #expect(manualRow[authorityIndex] == "manual_only")
}

@Test
func resultPackageManualOnlyAdjacentEventsSplitWhenEvidenceDiffers() throws {
    let fixture = resultPackageFixture()
    let train = fixture.dataset.trains[0]
    let run = resultPackageRun(
        from: fixture.run,
        replacingCandidatesByTrainID: [train.id: []],
        replacingAuditRowsByTrainID: [train.id: []]
    )
    let firstID = UUID(uuidString: "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeee1")!
    let secondID = UUID(uuidString: "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeee2")!
    let annotations = [
        ManualAnnotation(
            id: firstID,
            trainID: train.id,
            label: .tonic,
            startSec: train.timestampsSec[0],
            endSec: train.timestampsSec[1],
            note: "first manual island",
            createdAt: Date(timeIntervalSince1970: 100),
            updatedAt: Date(timeIntervalSince1970: 200)
        ),
        ManualAnnotation(
            id: secondID,
            trainID: train.id,
            label: .tonic,
            startSec: train.timestampsSec[1],
            endSec: train.timestampsSec[2],
            note: "second manual island",
            createdAt: Date(timeIntervalSince1970: 300),
            updatedAt: Date(timeIntervalSince1970: 400)
        ),
    ]
    let input = try resultPackageReviewedInput(
        dataset: fixture.dataset,
        run: run,
        manualAnnotations: annotations
    )
    let package = try STPDResultPackageBuilder.build(input)
    let events = try #require(package.table(STPDResultTable.eventsFinal))
    let trainIndex = try #require(events.headers.firstIndex(of: "train_id"))
    let labelIndex = try #require(events.headers.firstIndex(of: "final_label"))
    let startIndex = try #require(events.headers.firstIndex(of: "start_isi_index"))
    let endIndex = try #require(events.headers.firstIndex(of: "end_isi_index"))
    let evidenceIndex = try #require(events.headers.firstIndex(of: "review_evidence_uids"))
    var targetRows: [[String]] = events.rows.filter { row in
        row[trainIndex] == train.id &&
            row[labelIndex] == ManualAnnotationLabel.tonic.rawValue
    }
    targetRows.sort { lhs, rhs in
        Int(lhs[startIndex])! < Int(rhs[startIndex])!
    }

    #expect(targetRows.map { [$0[startIndex], $0[endIndex]] } == [
        ["1", "1"],
        ["2", "2"],
    ])
    #expect(targetRows.allSatisfy {
        STPDCanonicalValue.parseStringList($0[evidenceIndex])?.count == 1
    })
    #expect(targetRows[0][evidenceIndex] != targetRows[1][evidenceIndex])
}

@Test
func resultPackageEventUIDExcludesSourceIDsScoresPrioritiesAndDiagnostics() throws {
    let fixture = resultPackageFixture()
    let train = fixture.dataset.trains[0]

    func package(
        candidateID: String,
        score: Double,
        priority: Int,
        decisionPath: String
    ) throws -> STPDResultPackage {
        let candidate = resultPackageHFSCandidate(
            train: train,
            id: candidateID,
            candidateLayer: "event_uid_metadata_fixture",
            score: score,
            nISI: 4,
            nSpikes: 5,
            candidateClass: ClassicAnchorLabel.burst.rawValue,
            finalLabel: .burst,
            decisionPath: decisionPath,
            action: "accept_burst",
            priority: priority,
            selectedForAuto: true,
            selectionStatus: "selected",
            startISIIndex: 1,
            endISIIndex: 4,
            anchorFamily: "burst"
        )
        let run = resultPackageRun(
            from: fixture.run,
            replacingCandidatesByTrainID: [train.id: [candidate]],
            replacingAuditRowsByTrainID: [train.id: []]
        )
        return try STPDResultPackageBuilder.build(
            .automatic(dataset: fixture.dataset, run: run)
        )
    }

    let first = try package(
        candidateID: "transient-event-source-a",
        score: 0.2,
        priority: 100,
        decisionPath: "diagnostic_path_a"
    )
    let second = try package(
        candidateID: "renamed-transient-event-source-b",
        score: 0.9,
        priority: 900,
        decisionPath: "different_diagnostic_path_b"
    )

    func targetUID(_ package: STPDResultPackage) throws -> String {
        let table = try #require(package.table(.eventsFinal))
        let trainIndex = try #require(table.headers.firstIndex(of: "train_id"))
        let labelIndex = try #require(table.headers.firstIndex(of: "final_label"))
        let startIndex = try #require(table.headers.firstIndex(of: "start_isi_index"))
        let endIndex = try #require(table.headers.firstIndex(of: "end_isi_index"))
        let uidIndex = try #require(table.headers.firstIndex(of: "event_uid"))
        let row = try #require(table.rows.first {
            $0[trainIndex] == train.id &&
                $0[labelIndex] == ClassicAnchorLabel.burst.rawValue &&
                $0[startIndex] == "1" &&
                $0[endIndex] == "4"
        })
        return row[uidIndex]
    }

    #expect(try targetUID(first) == targetUID(second))
}

@Test
func resultPackageIncludesQCForATrainWithZeroISIs() throws {
    let zeroISITrain = SpikeTrain(name: "zero_isi_train", timestampsSec: [])
    let normalTrain = SpikeTrain(
        name: "normal_train",
        timestampsSec: [0, 0.1, 0.2, 0.3]
    )
    let dataset = SpikeDataset(
        name: "zero-isi-qc-fixture",
        sourceDescription: "zero ISI QC fixture",
        trains: [zeroISITrain, normalTrain]
    )
    let run = ClassicAnchorDetectionPipeline.run(
        dataset: dataset,
        bandSettings: TrainAdaptiveBandSettings(
            minValidISISec: 0.001,
            histogramBinWidthSec: 0.005
        ),
        buildCommit: resultPackageBuildCommit
    )
    let package = try STPDResultPackageBuilder.build(
        .automatic(dataset: dataset, run: run)
    )
    let parameters = try #require(package.table(.resolvedParameters))
    let scopeIDIndex = try #require(parameters.headers.firstIndex(of: "scope_id"))
    let keyIndex = try #require(parameters.headers.firstIndex(of: "parameter_key"))
    let valueIndex = try #require(parameters.headers.firstIndex(of: "effective_value"))
    let zeroQC: [String: String] = Dictionary(
        uniqueKeysWithValues: parameters.rows.compactMap { row in
            guard row[scopeIDIndex] == zeroISITrain.id,
                  row[keyIndex].hasPrefix("quality.") else {
                return nil
            }
            return (key: row[keyIndex], value: row[valueIndex])
        }
    )

    #expect(zeroQC["quality.spike_count"] == "0")
    #expect(zeroQC["quality.valid_isi_count"] == "0")
    #expect(zeroQC["quality.artifact_isi_count"] == "0")
    let finalISIs = try #require(package.table(.isiLabelsFinal))
    let trainIDs = try resultPackageColumn(finalISIs, "train_id")
    #expect(!trainIDs.contains(zeroISITrain.id))
    #expect(finalISIs.rowCount == normalTrain.spikeCount - 1)
}

@Test
func resultPackageReviewedModeKeepsManualAndReviewEvidenceExplicit() throws {
    let fixture = resultPackageFixture()
    let automatic = STPDResultPackageInput.automatic(
        dataset: fixture.dataset,
        run: fixture.run
    )
    let publicCandidateID = try #require(
        automatic.finalISILabelRows.first {
            $0.isiIndex > 0 && !$0.autoCandidateID.isEmpty
        }?.autoCandidateID
    )
    let sourceCandidate = try #require(
        fixture.run.candidates.first { $0.id == publicCandidateID }
    )
    let annotation = try resultPackageManualTonicAnnotation(
        dataset: fixture.dataset,
        run: fixture.run,
        id: UUID(uuidString: "11111111-2222-3333-4444-555555555555")!,
        note: "annotation, \"quoted\"\nline"
    )
    let input = try resultPackageReviewedInput(
        dataset: fixture.dataset,
        run: fixture.run,
        manualAnnotations: [annotation],
        candidateReviews: [
            STPDCandidateReviewInput(
                sourceCandidateID: sourceCandidate.id,
                status: .modified,
                reviewer: "Reviewer, A",
                note: "candidate \"modified\"\nafter review",
                reviewedAt: Date(timeIntervalSince1970: 300)
            )
        ],
        candidateDiagnostics: [
            STPDCandidateDiagnosticInput(
                sourceCandidateID: sourceCandidate.id,
                stageName: "human_review",
                stageOrdinal: 1,
                status: "accepted",
                details: "checked, independently"
            )
        ]
    )
    let package = try STPDResultPackageBuilder.build(input)

    let metadata = try #require(package.table(.runMetadata))
    #expect(metadata.value(row: 0, column: "source_mode") == "reviewed")
    #expect(package.manifest.sourceMode == "reviewed")
    #expect(package.table(.manualAnnotations)?.rowCount == 1)
    #expect(package.table(.reviewStatus)?.rowCount == 1)
    let diagnostics = try #require(package.table(.candidateDiagnosticAudit))
    let evidenceKinds = try resultPackageColumn(diagnostics, "evidence_kind")
    #expect(evidenceKinds.filter { $0 == "candidate_terminal" }.count
        == fixture.run.candidates.count)
    #expect(evidenceKinds.filter { $0 == "candidate_diagnostic" }.count == 1)
    #expect(Set(evidenceKinds).isSubset(
        of: ["candidate_terminal", "candidate_diagnostic", "event_source"]
    ))
    for row in diagnostics.rows.indices where
        diagnostics.value(row: row, column: "evidence_kind") == "event_source" {
        #expect(diagnostics.value(row: row, column: "candidate_uid")?.isEmpty == false)
        #expect(diagnostics.value(row: row, column: "event_uid")?.isEmpty == false)
        #expect(diagnostics.value(row: row, column: "automatic_source_id")?.isEmpty == false)
    }

    let review = try #require(package.table(.reviewStatus))
    let reviewCandidateUID = try #require(review.value(row: 0, column: "candidate_uid"))
    let ledgerUIDs = Set(try resultPackageColumn(
        #require(package.table(.candidateLedger)),
        "candidate_uid"
    ))
    #expect(ledgerUIDs.contains(reviewCandidateUID))

    let isiTable = try #require(package.table(.isiLabelsFinal))
    let evidenceColumns = try resultPackageColumn(isiTable, "review_evidence_uids")
    #expect(evidenceColumns.contains { $0.contains("manual_") })
    #expect(evidenceColumns.contains { $0.contains("review_") })
    #expect(try resultPackageColumn(isiTable, "review_evidence_present")
        .contains("true"))
    #expect(try resultPackageColumn(isiTable, "review_changed_projection")
        .contains("true"))
}

@Test
func resultPackageCandidateReviewAuthorityIsCausalAndFailClosed() throws {
    let fixture = resultPackageFixture()
    let automatic = STPDResultPackageInput.automatic(
        dataset: fixture.dataset,
        run: fixture.run
    )
    let sourceCandidateID = try #require(
        automatic.finalISILabelRows.first {
            $0.isiIndex > 0 && !$0.autoCandidateID.isEmpty
        }?.autoCandidateID
    )

    let accepted = try resultPackageReviewedInput(
        dataset: fixture.dataset,
        run: fixture.run,
        candidateReviews: [
            STPDCandidateReviewInput(
                sourceCandidateID: sourceCandidateID,
                status: .accepted,
                reviewer: "Independent reviewer",
                reviewedAt: Date(timeIntervalSince1970: 400)
            ),
        ]
    )
    let acceptedPackage = try STPDResultPackageBuilder.build(accepted)
    let reviewTable = try #require(acceptedPackage.table(.reviewStatus))
    #expect(reviewTable.rowCount == 1)
    #expect(reviewTable.value(row: 0, column: "status") == "accepted")
    let linkedISIUIDs = try #require(
        reviewTable.value(row: 0, column: "linked_isi_uids")
    )
    let parsedLinkedISIUIDs = try #require(
        STPDCanonicalValue.parseStringList(linkedISIUIDs)
    )
    #expect(!parsedLinkedISIUIDs.isEmpty)
    #expect(parsedLinkedISIUIDs.allSatisfy {
        $0.hasPrefix("isi_")
    })
    #expect(reviewTable.value(row: 0, column: "link_scope")
        == "public_projection")

    let needsReview = try resultPackageReviewedInput(
        dataset: fixture.dataset,
        run: fixture.run,
        candidateReviews: [
            STPDCandidateReviewInput(
                sourceCandidateID: sourceCandidateID,
                status: .needsReview,
                reviewer: "Independent reviewer"
            ),
        ]
    )
    #expect(throws: STPDResultPackageError.self) {
        _ = try STPDResultPackageBuilder.build(needsReview)
    }

    let modifiedWithoutReplacement = try resultPackageReviewedInput(
        dataset: fixture.dataset,
        run: fixture.run,
        candidateReviews: [
            STPDCandidateReviewInput(
                sourceCandidateID: sourceCandidateID,
                status: .modified,
                reviewer: "Independent reviewer"
            ),
        ]
    )
    #expect(throws: STPDResultPackageError.self) {
        _ = try STPDResultPackageBuilder.build(modifiedWithoutReplacement)
    }

    let acceptedWithoutLinks = STPDResultPackageInput(
        dataset: accepted.dataset,
        run: accepted.run,
        sourceMode: accepted.sourceMode,
        finalEvents: accepted.finalEvents,
        finalISILabelRows: accepted.finalISILabelRows,
        manualAnnotations: accepted.manualAnnotations,
        candidateReviews: accepted.candidateReviews,
        reviewLinks: [],
        candidateDiagnostics: accepted.candidateDiagnostics
    )
    #expect(throws: STPDResultPackageError.self) {
        _ = try STPDResultPackageBuilder.build(acceptedWithoutLinks)
    }
}

@Test
func resultPackageRejectsFinalISIProjectionThatContradictsFinalEvents() throws {
    let fixture = resultPackageFixture()
    let annotation = try resultPackageManualTonicAnnotation(
        dataset: fixture.dataset,
        run: fixture.run,
        id: UUID(uuidString: "12345678-1111-2222-3333-444444444444")!
    )
    let valid = try resultPackageReviewedInput(
        dataset: fixture.dataset,
        run: fixture.run,
        manualAnnotations: [annotation]
    )
    var reviewedRows = valid.finalISILabelRows
    let reviewedIndex = try #require(reviewedRows.firstIndex {
        $0.isiIndex > 0
            && $0.finalSource != ReviewedISIExportBuilder.sourceManualPositive
    })
    reviewedRows[reviewedIndex] = resultPackageReviewedRow(
        from: reviewedRows[reviewedIndex],
        note: "contradictory final projection"
    )
    let input = STPDResultPackageInput(
        dataset: fixture.dataset,
        run: fixture.run,
        sourceMode: .reviewed,
        finalEvents: valid.finalEvents,
        finalISILabelRows: reviewedRows,
        manualAnnotations: valid.manualAnnotations,
        reviewLinks: valid.reviewLinks
    )

    #expect(throws: STPDResultPackageError.self) {
        _ = try STPDResultPackageBuilder.build(input)
    }
}

@Test
func resultPackageSourceModesFailClosed() throws {
    let fixture = resultPackageFixture()
    let automatic = STPDResultPackageInput.automatic(
        dataset: fixture.dataset,
        run: fixture.run
    )
    let sourceCandidate = try #require(
        fixture.run.candidates.first { $0.trainID != "__dataset__" }
    )
    let annotation = ManualAnnotation(
        trainID: fixture.dataset.trains[0].id,
        label: .burst,
        startSec: 0.1,
        endSec: 0.2
    )

    #expect(throws: STPDResultPackageError.self) {
        _ = try STPDResultPackageBuilder.build(STPDResultPackageInput(
            dataset: fixture.dataset,
            run: fixture.run,
            sourceMode: .automatic,
            finalEvents: automatic.finalEvents,
            finalISILabelRows: automatic.finalISILabelRows,
            manualAnnotations: [annotation]
        ))
    }
    #expect(throws: STPDResultPackageError.self) {
        _ = try STPDResultPackageBuilder.build(STPDResultPackageInput(
            dataset: fixture.dataset,
            run: fixture.run,
            sourceMode: .automatic,
            finalEvents: automatic.finalEvents,
            finalISILabelRows: automatic.finalISILabelRows,
            candidateReviews: [
                STPDCandidateReviewInput(
                    sourceCandidateID: sourceCandidate.id,
                    status: .accepted
                )
            ]
        ))
    }
    #expect(throws: STPDResultPackageError.self) {
        _ = try STPDResultPackageBuilder.build(STPDResultPackageInput(
            dataset: fixture.dataset,
            run: fixture.run,
            sourceMode: .reviewed,
            finalEvents: automatic.finalEvents,
            finalISILabelRows: automatic.finalISILabelRows,
            candidateDiagnostics: [
                STPDCandidateDiagnosticInput(
                    sourceCandidateID: sourceCandidate.id,
                    stageName: "diagnostic_only",
                    stageOrdinal: 1,
                    status: "observed",
                    details: "not review authority"
                )
            ]
        ))
    }
}

@Test
func resultPackageRejectsTamperedISIGeometryAndAutomaticFields() throws {
    let fixture = resultPackageFixture()
    let automatic = STPDResultPackageInput.automatic(
        dataset: fixture.dataset,
        run: fixture.run
    )
    let rowIndex = try #require(automatic.finalISILabelRows.firstIndex { $0.isiIndex > 0 })
    let original = automatic.finalISILabelRows[rowIndex]

    var geometryRows = automatic.finalISILabelRows
    geometryRows[rowIndex] = ReviewedISIExportRow(
        trainID: original.trainID,
        trainName: original.trainName,
        spikeIndex: original.spikeIndex,
        timestampSec: original.timestampSec,
        alignedTimestampSec: original.alignedTimestampSec,
        isiIndex: original.isiIndex,
        isiSec: original.isiSec + 0.001,
        autoPattern: original.autoPattern,
        autoSubtype: original.autoSubtype,
        autoCandidateID: original.autoCandidateID,
        finalPattern: original.finalPattern,
        finalSubtype: original.finalSubtype,
        finalSource: original.finalSource,
        manualVetoSuppressed: original.manualVetoSuppressed,
        reviewNote: original.reviewNote
    )
    #expect(throws: STPDResultPackageError.self) {
        _ = try STPDResultPackageBuilder.build(STPDResultPackageInput(
            dataset: fixture.dataset,
            run: fixture.run,
            sourceMode: .automatic,
            finalEvents: automatic.finalEvents,
            finalISILabelRows: geometryRows
        ))
    }

    var automaticRows = automatic.finalISILabelRows
    automaticRows[rowIndex] = ReviewedISIExportRow(
        trainID: original.trainID,
        trainName: original.trainName,
        spikeIndex: original.spikeIndex,
        timestampSec: original.timestampSec,
        alignedTimestampSec: original.alignedTimestampSec,
        isiIndex: original.isiIndex,
        isiSec: original.isiSec,
        autoPattern: "tampered",
        autoSubtype: original.autoSubtype,
        autoCandidateID: original.autoCandidateID,
        finalPattern: original.finalPattern,
        finalSubtype: original.finalSubtype,
        finalSource: original.finalSource,
        manualVetoSuppressed: original.manualVetoSuppressed,
        reviewNote: original.reviewNote
    )
    #expect(throws: STPDResultPackageError.self) {
        _ = try STPDResultPackageBuilder.build(STPDResultPackageInput(
            dataset: fixture.dataset,
            run: fixture.run,
            sourceMode: .automatic,
            finalEvents: automatic.finalEvents,
            finalISILabelRows: automaticRows
        ))
    }
}

@Test
func resultPackageRejectsNegativeMalformedAndDuplicateStructuralISIRows() throws {
    let fixture = resultPackageFixture()
    let automatic = STPDResultPackageInput.automatic(
        dataset: fixture.dataset,
        run: fixture.run
    )
    let intervalIndex = try #require(
        automatic.finalISILabelRows.firstIndex { $0.isiIndex > 0 }
    )
    let placeholderIndex = try #require(
        automatic.finalISILabelRows.firstIndex { $0.isiIndex == 0 }
    )

    var negativeRows = automatic.finalISILabelRows
    negativeRows[intervalIndex] = resultPackageISIRow(
        from: negativeRows[intervalIndex],
        spikeIndex: -1,
        isiIndex: -1
    )
    #expect(throws: STPDResultPackageError.self) {
        _ = try STPDResultPackageBuilder.build(STPDResultPackageInput(
            dataset: fixture.dataset,
            run: fixture.run,
            sourceMode: .automatic,
            finalEvents: automatic.finalEvents,
            finalISILabelRows: negativeRows
        ))
    }

    var malformedPlaceholderRows = automatic.finalISILabelRows
    malformedPlaceholderRows[placeholderIndex] = resultPackageISIRow(
        from: malformedPlaceholderRows[placeholderIndex],
        finalPattern: ManualAnnotationLabel.tonic.rawValue
    )
    #expect(throws: STPDResultPackageError.self) {
        _ = try STPDResultPackageBuilder.build(STPDResultPackageInput(
            dataset: fixture.dataset,
            run: fixture.run,
            sourceMode: .automatic,
            finalEvents: automatic.finalEvents,
            finalISILabelRows: malformedPlaceholderRows
        ))
    }

    let duplicatePlaceholderRows =
        automatic.finalISILabelRows + [automatic.finalISILabelRows[placeholderIndex]]
    #expect(throws: STPDResultPackageError.self) {
        _ = try STPDResultPackageBuilder.build(STPDResultPackageInput(
            dataset: fixture.dataset,
            run: fixture.run,
            sourceMode: .automatic,
            finalEvents: automatic.finalEvents,
            finalISILabelRows: duplicatePlaceholderRows
        ))
    }
}

@Test
func resultPackageManualGeometryIsRecomputedAndStableIDUsesPersistentUUID() throws {
    let fixture = resultPackageFixture()
    let annotationID = UUID(uuidString: "aaaaaaaa-1111-2222-3333-bbbbbbbbbbbb")!

    func package(start: Double, end: Double) throws -> STPDResultPackage {
        let annotation = ManualAnnotation(
            id: annotationID,
            trainID: fixture.dataset.trains[0].id,
            label: .tonic,
            startSec: start,
            endSec: end,
            startISIIndex: 999,
            endISIIndex: 999,
            startSpikeIndex: 999,
            endSpikeIndex: 999
        )
        let input = try resultPackageReviewedInput(
            dataset: fixture.dataset,
            run: fixture.run,
            manualAnnotations: [annotation]
        )
        return try STPDResultPackageBuilder.build(input)
    }

    let first = try package(start: 0.1, end: 0.2)
    let second = try package(start: 0.2, end: 0.3)
    let firstTable = try #require(first.table(.manualAnnotations))
    let secondTable = try #require(second.table(.manualAnnotations))
    #expect(firstTable.value(row: 0, column: "start_isi_index") != "999")
    #expect(firstTable.value(row: 0, column: "end_isi_index") != "999")
    #expect(firstTable.value(row: 0, column: "annotation_id")
        == secondTable.value(row: 0, column: "annotation_id"))
}

@Test
func resultPackageRejectsIncompatibleAndNonFiniteManualAnnotations() {
    let fixture = resultPackageFixture()
    let automatic = STPDResultPackageInput.automatic(
        dataset: fixture.dataset,
        run: fixture.run
    )
    for annotation in [
        ManualAnnotation(
            trainID: "missing-train",
            label: .tonic,
            startSec: 0.1,
            endSec: 0.2
        ),
        ManualAnnotation(
            trainID: fixture.dataset.trains[0].id,
            label: .tonic,
            startSec: .nan,
            endSec: 0.2
        ),
    ] {
        #expect(throws: STPDResultPackageError.self) {
            _ = try STPDResultPackageBuilder.build(STPDResultPackageInput(
                dataset: fixture.dataset,
                run: fixture.run,
                sourceMode: .reviewed,
                finalEvents: automatic.finalEvents,
                finalISILabelRows: automatic.finalISILabelRows,
                manualAnnotations: [annotation]
            ))
        }
    }
}

@Test
func resultPackageCanonicalDoubleNormalizesSignedZero() {
    #expect(STPDCanonicalValue.double(0.0) == "0")
    #expect(STPDCanonicalValue.double(-0.0) == "0")
}

@Test
func resultPackageStringListsDistinguishEmptyFromMissingAndRoundTripDelimiters() {
    let values = ["a|b", "quoted \"value\"", "line\nbreak", ""]
    let encoded = STPDCanonicalValue.stringList(values)

    #expect(STPDCanonicalValue.stringList([]) == "[]")
    #expect(STPDCanonicalValue.parseStringList("[]") == [])
    #expect(STPDCanonicalValue.parseStringList("") == nil)
    #expect(STPDCanonicalValue.parseStringList(encoded) == values)
}

@Test
func resultPackageHFSAuditUsesCanonicalValuesAndStableSplitLineage() throws {
    let fixture = resultPackageFixture()
    let train = fixture.dataset.trains[0]
    let firstCandidateID = "transient-root-a::state-split::1-3"
    let firstRun = resultPackageRun(
        from: fixture.run,
        appending: resultPackageHFSCandidate(train: train, id: firstCandidateID),
        auditRow: resultPackageHFSRow(
            train: train,
            candidateID: firstCandidateID,
            rootID: "transient-root-a",
            rowID: "transient-audit-a"
        )
    )
    let secondCandidateID = "transient-root-b::state-split::1-3"
    let secondRun = resultPackageRun(
        from: fixture.run,
        appending: resultPackageHFSCandidate(train: train, id: secondCandidateID),
        auditRow: resultPackageHFSRow(
            train: train,
            candidateID: secondCandidateID,
            rootID: "transient-root-b",
            rowID: "transient-audit-b"
        )
    )

    let first = try STPDResultPackageBuilder.build(
        .automatic(dataset: fixture.dataset, run: firstRun)
    )
    let second = try STPDResultPackageBuilder.build(
        .automatic(dataset: fixture.dataset, run: secondRun)
    )
    let firstTable = try #require(first.table(.hfsBurstArbitrationAudit))
    let secondTable = try #require(second.table(.hfsBurstArbitrationAudit))

    #expect(firstTable.rowCount == 1)
    #expect(try resultPackageColumn(firstTable, "audit_hfs_raw_score") == [
        STPDCanonicalValue.double(0.12345678901234566),
    ])
    #expect(try resultPackageColumn(firstTable, "audit_hfs_selected_for_auto") == ["false"])
    #expect(try resultPackageColumn(firstTable, "audit_burst_packet_like") == ["false"])
    #expect(try resultPackageColumn(firstTable, "audit_requires_review") == ["true"])
    #expect(try resultPackageColumn(firstTable, "hfs_candidate_uid").first?.isEmpty == false)
    #expect(try resultPackageColumn(firstTable, "hfs_root_lineage_uid").first?.isEmpty == false)
    #expect(try resultPackageColumn(firstTable, "hfs_root_reference_kind") == ["split_lineage"])
    #expect(try resultPackageColumn(firstTable, "unresolved_candidate_ids") == [
        STPDCanonicalValue.stringList([]),
    ])
    #expect(
        try resultPackageColumn(firstTable, "hfs_root_lineage_uid") ==
        resultPackageColumn(secondTable, "hfs_root_lineage_uid")
    )
    #expect(
        try resultPackageColumn(firstTable, "audit_row_id") ==
        resultPackageColumn(secondTable, "audit_row_id")
    )
}

@Test
func resultPackageHFSAuditUIDIgnoresTransientIDsInsideDecisionReason() throws {
    let fixture = resultPackageFixture()
    let train = fixture.dataset.trains[0]

    func package(
        rootID: String,
        candidateID: String,
        rowID: String,
        suppressedIDs: String
    ) throws -> STPDResultPackage {
        let run = resultPackageRun(
            from: fixture.run,
            appending: resultPackageHFSCandidate(
                train: train,
                id: candidateID
            ),
            auditRow: resultPackageHFSRow(
                train: train,
                candidateID: candidateID,
                rootID: rootID,
                rowID: rowID,
                decisionReason: [
                    "decision=unresolved_review",
                    "hfs_candidate=\(candidateID)",
                    "selected_event_subtypes=none",
                    "suppressed_event_ids=\(suppressedIDs)",
                ].joined(separator: ";")
            )
        )
        return try STPDResultPackageBuilder.build(
            .automatic(dataset: fixture.dataset, run: run)
        )
    }

    let first = try package(
        rootID: "transient-reason-root-a",
        candidateID: "transient-reason-root-a::state-split::1-3",
        rowID: "transient-reason-audit-a",
        suppressedIDs: "proposal-a|proposal-b"
    )
    let second = try package(
        rootID: "transient-reason-root-b",
        candidateID: "transient-reason-root-b::state-split::1-3",
        rowID: "transient-reason-audit-b",
        suppressedIDs: "renamed-proposal-x|renamed-proposal-y"
    )

    #expect(
        try resultPackageColumn(
            #require(first.table(.hfsBurstArbitrationAudit)),
            "audit_row_id"
        ) ==
        resultPackageColumn(
            #require(second.table(.hfsBurstArbitrationAudit)),
            "audit_row_id"
        )
    )
}

@Test
func resultPackageHFSRootLineageIsStableAcrossEquivalentSplitSegmentation() throws {
    let fixture = resultPackageFixture()
    let train = fixture.dataset.trains[0]
    let singleRoot = "single-transient-root"
    let singleID = "\(singleRoot)::state-split::1-4"
    let singleCandidate = resultPackageHFSCandidate(
        train: train,
        id: singleID,
        nISI: 4,
        nSpikes: 5,
        startISIIndex: 1,
        endISIIndex: 4
    )
    let singleAuditRow = resultPackageHFSRow(
        train: train,
        candidateID: singleID,
        rootID: singleRoot,
        rowID: "single-split-audit",
        hfsStartISIIndex: 1,
        hfsEndISIIndex: 4
    )
    let singleRun = resultPackageRun(
        from: fixture.run,
        replacingCandidatesByTrainID: [train.id: [singleCandidate]],
        replacingAuditRowsByTrainID: [train.id: [singleAuditRow]]
    )

    let segmentedRoot = "segmented-transient-root"
    let firstID = "\(segmentedRoot)::state-split::1-2"
    let secondID = "\(segmentedRoot)::state-split::3-4"
    let firstCandidate = resultPackageHFSCandidate(
        train: train,
        id: firstID,
        nISI: 2,
        nSpikes: 3,
        startISIIndex: 1,
        endISIIndex: 2
    )
    let secondCandidate = resultPackageHFSCandidate(
        train: train,
        id: secondID,
        nISI: 2,
        nSpikes: 3,
        startISIIndex: 3,
        endISIIndex: 4
    )
    let segmentedAuditRows = [
        resultPackageHFSRow(
            train: train,
            candidateID: firstID,
            rootID: segmentedRoot,
            rowID: "segmented-split-audit-a",
            hfsStartISIIndex: 1,
            hfsEndISIIndex: 2
        ),
        resultPackageHFSRow(
            train: train,
            candidateID: secondID,
            rootID: segmentedRoot,
            rowID: "segmented-split-audit-b",
            hfsStartISIIndex: 3,
            hfsEndISIIndex: 4
        ),
    ]
    let segmentedRun = resultPackageRun(
        from: fixture.run,
        replacingCandidatesByTrainID: [
            train.id: [firstCandidate, secondCandidate],
        ],
        replacingAuditRowsByTrainID: [train.id: segmentedAuditRows]
    )

    let single = try STPDResultPackageBuilder.build(
        .automatic(dataset: fixture.dataset, run: singleRun)
    )
    let segmented = try STPDResultPackageBuilder.build(
        .automatic(dataset: fixture.dataset, run: segmentedRun)
    )
    let singleLineage = Set(try resultPackageColumn(
        #require(single.table(.hfsBurstArbitrationAudit)),
        "hfs_root_lineage_uid"
    ))
    let segmentedLineage = Set(try resultPackageColumn(
        #require(segmented.table(.hfsBurstArbitrationAudit)),
        "hfs_root_lineage_uid"
    ))

    #expect(singleLineage.count == 1)
    #expect(segmentedLineage.count == 1)
    #expect(singleLineage == segmentedLineage)
}

@Test
func resultPackageHFSRejectsSplitSuffixThatContradictsCandidateGeometry() {
    let fixture = resultPackageFixture()
    let train = fixture.dataset.trains[0]
    let rootID = "malformed-split-root"
    let candidateID = "\(rootID)::state-split::2-4"
    let run = resultPackageRun(
        from: fixture.run,
        appending: resultPackageHFSCandidate(
            train: train,
            id: candidateID,
            startISIIndex: 1,
            endISIIndex: 3
        ),
        auditRow: resultPackageHFSRow(
            train: train,
            candidateID: candidateID,
            rootID: rootID,
            rowID: "malformed-split-audit"
        )
    )

    #expect(throws: STPDResultPackageError.self) {
        _ = try STPDResultPackageBuilder.build(
            .automatic(dataset: fixture.dataset, run: run)
        )
    }
}

@Test
func resultPackageReviewedManualBurstFeedsHFSArbitrationContext() throws {
    let fixture = resultPackageFixture()
    let train = fixture.dataset.trains[0]
    let hfsID = "manual-review-hfs::state-split::1-3"
    let hfs = resultPackageHFSCandidate(train: train, id: hfsID)
    let audit = resultPackageHFSRow(
        train: train,
        candidateID: hfsID,
        rootID: "manual-review-hfs",
        rowID: "manual-review-hfs-audit"
    )
    let run = resultPackageRun(
        from: fixture.run,
        replacingCandidatesByTrainID: [train.id: [hfs]],
        replacingAuditRowsByTrainID: [train.id: [audit]]
    )
    let annotation = ManualAnnotation(
        id: UUID(uuidString: "10101010-2020-3030-4040-505050505050")!,
        trainID: train.id,
        label: .burst,
        startSec: train.timestampsSec[0],
        endSec: train.timestampsSec[2],
        note: "manual burst inside HFS review span",
        createdAt: Date(timeIntervalSince1970: 100),
        updatedAt: Date(timeIntervalSince1970: 200)
    )
    let input = try resultPackageReviewedInput(
        dataset: fixture.dataset,
        run: run,
        manualAnnotations: [annotation]
    )
    let package = try STPDResultPackageBuilder.build(input)
    let hfsTable = try #require(package.table(.hfsBurstArbitrationAudit))

    #expect(hfsTable.rowCount == 1)
    #expect(hfsTable.value(row: 0, column: "package_final_event_count") == "1")
    #expect(
        STPDCanonicalValue.parseStringList(
            try #require(
                hfsTable.value(row: 0, column: "package_final_event_subtypes")
            )
        ) == ["burst_i"]
    )
    #expect(
        hfsTable.value(
            row: 0,
            column: "package_final_projection_differs_from_audit"
        ) == "true"
    )
}

@Test
func resultPackageHFSAuditRejectsUnknownCandidateAndNonFiniteEvidence() {
    let fixture = resultPackageFixture()
    let train = fixture.dataset.trains[0]
    let candidateID = "known-root::state-split::1-3"
    let candidate = resultPackageHFSCandidate(train: train, id: candidateID)

    let unknownCandidateRun = resultPackageRun(
        from: fixture.run,
        appending: candidate,
        auditRow: resultPackageHFSRow(
            train: train,
            candidateID: "missing-root::state-split::1-3",
            rootID: "missing-root",
            rowID: "unknown-candidate"
        )
    )
    #expect(throws: STPDResultPackageError.self) {
        _ = try STPDResultPackageBuilder.build(
            .automatic(dataset: fixture.dataset, run: unknownCandidateRun)
        )
    }

    let nonFiniteRun = resultPackageRun(
        from: fixture.run,
        appending: candidate,
        auditRow: resultPackageHFSRow(
            train: train,
            candidateID: candidateID,
            rootID: "known-root",
            rowID: "non-finite",
            rawScore: .nan
        )
    )
    #expect(throws: STPDResultPackageError.self) {
        _ = try STPDResultPackageBuilder.build(
            .automatic(dataset: fixture.dataset, run: nonFiniteRun)
        )
    }
}

@Test
func resultPackageHFSAuditRejectsOutOfRangeFractionAndInvertedBridgeBand() {
    let fixture = resultPackageFixture()
    let train = fixture.dataset.trains[0]
    let candidateID = "known-root::state-split::1-3"
    let candidate = resultPackageHFSCandidate(train: train, id: candidateID)

    let invalidFractionRun = resultPackageRun(
        from: fixture.run,
        appending: candidate,
        auditRow: resultPackageHFSRow(
            train: train,
            candidateID: candidateID,
            rootID: "known-root",
            rowID: "invalid-fraction",
            packetCoverage: 1.01
        )
    )
    #expect(throws: STPDResultPackageError.self) {
        _ = try STPDResultPackageBuilder.build(
            .automatic(dataset: fixture.dataset, run: invalidFractionRun)
        )
    }

    let invertedBridgeBandRun = resultPackageRun(
        from: fixture.run,
        appending: candidate,
        auditRow: resultPackageHFSRow(
            train: train,
            candidateID: candidateID,
            rootID: "known-root",
            rowID: "inverted-bridge-band",
            seedBandUpperSec: 0.1,
            bridgeBandUpperSec: 0.09
        )
    )
    #expect(throws: STPDResultPackageError.self) {
        _ = try STPDResultPackageBuilder.build(
            .automatic(dataset: fixture.dataset, run: invertedBridgeBandRun)
        )
    }
}

@Test
func resultPackageRejectsScientificallyDuplicateCandidatesDespiteDifferentTransientIDs() {
    let fixture = resultPackageFixture()
    let train = fixture.dataset.trains[0]
    let firstID = "transient-root-a::state-split::1-3"
    let secondID = "transient-root-b::state-split::1-3"
    let run = resultPackageRun(
        from: fixture.run,
        appending: [
            resultPackageHFSCandidate(train: train, id: firstID),
            resultPackageHFSCandidate(train: train, id: secondID),
        ],
        auditRows: [
            resultPackageHFSRow(
                train: train,
                candidateID: firstID,
                rootID: "transient-root-a",
                rowID: "duplicate-science-a"
            ),
            resultPackageHFSRow(
                train: train,
                candidateID: secondID,
                rootID: "transient-root-b",
                rowID: "duplicate-science-b"
            ),
        ]
    )

    #expect(throws: STPDResultPackageError.self) {
        _ = try STPDResultPackageBuilder.build(
            .automatic(dataset: fixture.dataset, run: run)
        )
    }
}

@Test
func resultPackageRejectsNonFiniteCandidateEvidence() {
    let fixture = resultPackageFixture()
    let train = fixture.dataset.trains[0]
    let candidateID = "non-finite-root::state-split::1-3"
    let run = resultPackageRun(
        from: fixture.run,
        appending: resultPackageHFSCandidate(
            train: train,
            id: candidateID,
            cv2: .nan
        ),
        auditRow: resultPackageHFSRow(
            train: train,
            candidateID: candidateID,
            rootID: "non-finite-root",
            rowID: "non-finite-candidate"
        )
    )

    #expect(throws: STPDResultPackageError.self) {
        _ = try STPDResultPackageBuilder.build(
            .automatic(dataset: fixture.dataset, run: run)
        )
    }
}

@Test
func resultPackageRejectsCandidateCountQuantileFractionAndDurationContradictions() {
    let fixture = resultPackageFixture()
    let train = fixture.dataset.trains[0]

    func expectRejected(
        _ candidate: ClassicAnchorCandidate,
        rootID: String
    ) {
        let run = resultPackageRun(
            from: fixture.run,
            appending: candidate,
            auditRow: resultPackageHFSRow(
                train: train,
                candidateID: candidate.id,
                rootID: rootID,
                rowID: "\(rootID)-audit"
            )
        )
        #expect(throws: STPDResultPackageError.self) {
            _ = try STPDResultPackageBuilder.build(
                .automatic(dataset: fixture.dataset, run: run)
            )
        }
    }

    expectRejected(
        resultPackageHFSCandidate(
            train: train,
            id: "count-mismatch::state-split::1-3",
            nISI: 2
        ),
        rootID: "count-mismatch"
    )
    expectRejected(
        resultPackageHFSCandidate(
            train: train,
            id: "quantile-mismatch::state-split::1-3",
            intraQ10Sec: 0.2
        ),
        rootID: "quantile-mismatch"
    )
    expectRejected(
        resultPackageHFSCandidate(
            train: train,
            id: "duration-mismatch::state-split::1-3",
            durationSec: 0.31
        ),
        rootID: "duration-mismatch"
    )

    var invalidFraction = resultPackageHFSCandidate(
        train: train,
        id: "fraction-mismatch::state-split::1-3"
    )
    invalidFraction.stateBurstSeedFraction = 1.01
    expectRejected(invalidFraction, rootID: "fraction-mismatch")

    var negativeEvidence = resultPackageHFSCandidate(
        train: train,
        id: "negative-evidence::state-split::1-3"
    )
    negativeEvidence.hfSpikingQ80Sec = -0.1
    expectRejected(negativeEvidence, rootID: "negative-evidence")
}

@Test
func resultPackageRejectsDuplicateHFSSnapshotDespiteDifferentTransientRowIDs() {
    let fixture = resultPackageFixture()
    let train = fixture.dataset.trains[0]
    let candidateID = "duplicate-audit-root::state-split::1-3"
    let run = resultPackageRun(
        from: fixture.run,
        appending: [resultPackageHFSCandidate(train: train, id: candidateID)],
        auditRows: [
            resultPackageHFSRow(
                train: train,
                candidateID: candidateID,
                rootID: "duplicate-audit-root",
                rowID: "duplicate-audit-a"
            ),
            resultPackageHFSRow(
                train: train,
                candidateID: candidateID,
                rootID: "duplicate-audit-root",
                rowID: "duplicate-audit-b"
            ),
        ]
    )

    #expect(throws: STPDResultPackageError.self) {
        _ = try STPDResultPackageBuilder.build(
            .automatic(dataset: fixture.dataset, run: run)
        )
    }
}

@Test
func resultPackageRejectsHFSRowsThatContradictCandidateSnapshot() {
    let fixture = resultPackageFixture()
    let train = fixture.dataset.trains[0]
    let scoreCandidateID = "contradictory-score-root::state-split::1-3"
    let scoreRun = resultPackageRun(
        from: fixture.run,
        appending: resultPackageHFSCandidate(train: train, id: scoreCandidateID),
        auditRow: resultPackageHFSRow(
            train: train,
            candidateID: scoreCandidateID,
            rootID: "contradictory-score-root",
            rowID: "contradictory-score",
            rawScore: 0.5
        )
    )
    #expect(throws: STPDResultPackageError.self) {
        _ = try STPDResultPackageBuilder.build(
            .automatic(dataset: fixture.dataset, run: scoreRun)
        )
    }

    let routeCandidateID = "contradictory-route-root::state-split::1-3"
    var routeCandidate = resultPackageHFSCandidate(
        train: train,
        id: routeCandidateID
    )
    routeCandidate.hfSpikingAcceptanceRoute = "candidate_route"
    let routeRun = resultPackageRun(
        from: fixture.run,
        appending: routeCandidate,
        auditRow: resultPackageHFSRow(
            train: train,
            candidateID: routeCandidateID,
            rootID: "contradictory-route-root",
            rowID: "contradictory-route"
        )
    )
    #expect(throws: STPDResultPackageError.self) {
        _ = try STPDResultPackageBuilder.build(
            .automatic(dataset: fixture.dataset, run: routeRun)
        )
    }
}

@Test
func resultPackageRejectsOutOfRangeStateScores() {
    let fixture = resultPackageFixture()
    let train = fixture.dataset.trains[0]

    func expectRejected(
        _ candidate: ClassicAnchorCandidate,
        rootID: String
    ) {
        let run = resultPackageRun(
            from: fixture.run,
            appending: candidate,
            auditRow: resultPackageHFSRow(
                train: train,
                candidateID: candidate.id,
                rootID: rootID,
                rowID: "\(rootID)-audit"
            )
        )
        #expect(throws: STPDResultPackageError.self) {
            _ = try STPDResultPackageBuilder.build(
                .automatic(dataset: fixture.dataset, run: run)
            )
        }
    }

    var invalidRegularity = resultPackageHFSCandidate(
        train: train,
        id: "invalid-regularity::state-split::1-3"
    )
    invalidRegularity.stateRegularityScore = 1.01
    expectRejected(invalidRegularity, rootID: "invalid-regularity")

    var invalidStability = resultPackageHFSCandidate(
        train: train,
        id: "invalid-stability::state-split::1-3"
    )
    invalidStability.stateLocalStabilityScore = -0.01
    expectRejected(invalidStability, rootID: "invalid-stability")
}

@Test
func resultPackageNormalizesAndValidatesHFSSuppressorRelationships() throws {
    let fixture = resultPackageFixture()
    let train = fixture.dataset.trains[0]
    let otherTrain = fixture.dataset.trains[1]
    let targetID = "suppressed-burst::1-3"
    let suppressorID = "hfs-suppressor::1-3"

    func target(
        suppressorID: String?,
        authority: Bool? = true
    ) -> ClassicAnchorCandidate {
        var candidate = resultPackageHFSCandidate(
            train: train,
            id: targetID,
            score: 0.2,
            candidateClass: ClassicAnchorLabel.burst.rawValue,
            finalLabel: .reject,
            priority: 50
        )
        candidate.suppressedByHFSpikingState = authority
        candidate.suppressedOriginalLabel = ClassicAnchorLabel.burst.rawValue
        candidate.hfSpikingSuppressorID = suppressorID
        return candidate
    }

    let suppressor = resultPackageHFSCandidate(
        train: train,
        id: suppressorID,
        score: 0.3,
        priority: 200
    )
    let validRun = resultPackageRun(
        from: fixture.run,
        appending: [target(suppressorID: suppressorID), suppressor],
        auditRows: []
    )
    let package = try STPDResultPackageBuilder.build(
        .automatic(dataset: fixture.dataset, run: validRun)
    )
    let features = try #require(package.table(.candidateFeatures))
    let sourceIDs = try resultPackageColumn(features, "source_candidate_id")
    let candidateUIDs = try resultPackageColumn(features, "candidate_uid")
    let suppressorUIDs = try resultPackageColumn(features, "hf_suppressor_candidate_uid")
    let targetIndex = try #require(sourceIDs.firstIndex(of: targetID))
    let suppressorIndex = try #require(sourceIDs.firstIndex(of: suppressorID))
    #expect(suppressorUIDs[targetIndex] == candidateUIDs[suppressorIndex])
    #expect(!suppressorUIDs[targetIndex].isEmpty)
    #expect(suppressorUIDs[targetIndex] != suppressorID)

    func expectRejected(_ candidates: [ClassicAnchorCandidate]) {
        let run = resultPackageRun(
            from: fixture.run,
            appending: candidates,
            auditRows: []
        )
        #expect(throws: STPDResultPackageError.self) {
            _ = try STPDResultPackageBuilder.build(
                .automatic(dataset: fixture.dataset, run: run)
            )
        }
    }

    expectRejected([
        target(suppressorID: suppressorID, authority: false),
        suppressor,
    ])
    expectRejected([target(suppressorID: "unknown-hfs-suppressor")])

    let tonicSuppressor = resultPackageHFSCandidate(
        train: train,
        id: "tonic-suppressor::1-3",
        candidateClass: ClassicAnchorLabel.tonic.rawValue,
        finalLabel: .tonic
    )
    expectRejected([
        target(suppressorID: tonicSuppressor.id),
        tonicSuppressor,
    ])

    let nonOverlappingSuppressor = resultPackageHFSCandidate(
        train: train,
        id: "non-overlap-hfs-suppressor::4-6",
        startISIIndex: 4,
        endISIIndex: 6
    )
    expectRejected([
        target(suppressorID: nonOverlappingSuppressor.id),
        nonOverlappingSuppressor,
    ])

    let crossTrainSuppressor = resultPackageHFSCandidate(
        train: otherTrain,
        id: "cross-train-hfs-suppressor::1-3"
    )
    expectRejected([
        target(suppressorID: crossTrainSuppressor.id),
        crossTrainSuppressor,
    ])
}

@Test
func resultPackageStableCandidateUIDIgnoresTransientIDsButTracksSuppressorIdentity() throws {
    let fixture = resultPackageFixture()
    let train = fixture.dataset.trains[0]

    func target(id: String, suppressorID: String) -> ClassicAnchorCandidate {
        var candidate = resultPackageHFSCandidate(
            train: train,
            id: id,
            score: 0.2,
            candidateClass: ClassicAnchorLabel.burst.rawValue,
            finalLabel: .reject,
            priority: 50
        )
        candidate.suppressedByHFSpikingState = true
        candidate.suppressedOriginalLabel = ClassicAnchorLabel.burst.rawValue
        candidate.hfSpikingSuppressorID = suppressorID
        return candidate
    }

    func package(
        targetID: String,
        suppressorID: String,
        suppressorScore: Double,
        reversed: Bool
    ) throws -> STPDResultPackage {
        let suppressor = resultPackageHFSCandidate(
            train: train,
            id: suppressorID,
            score: suppressorScore,
            priority: 200
        )
        let target = target(id: targetID, suppressorID: suppressorID)
        let candidates = reversed ? [suppressor, target] : [target, suppressor]
        let run = resultPackageRun(
            from: fixture.run,
            replacingCandidatesByTrainID: [train.id: candidates],
            replacingAuditRowsByTrainID: [train.id: []]
        )
        return try STPDResultPackageBuilder.build(
            .automatic(dataset: fixture.dataset, run: run)
        )
    }

    func uids(
        _ package: STPDResultPackage,
        targetID: String,
        suppressorID: String
    ) throws -> (target: String, suppressor: String) {
        let features = try #require(package.table(.candidateFeatures))
        let sourceIDs = try resultPackageColumn(features, "source_candidate_id")
        let candidateUIDs = try resultPackageColumn(features, "candidate_uid")
        let targetIndex = try #require(sourceIDs.firstIndex(of: targetID))
        let suppressorIndex = try #require(sourceIDs.firstIndex(of: suppressorID))
        return (candidateUIDs[targetIndex], candidateUIDs[suppressorIndex])
    }

    let first = try package(
        targetID: "transient-target-a",
        suppressorID: "transient-suppressor-a",
        suppressorScore: 0.3,
        reversed: false
    )
    let renamedAndReordered = try package(
        targetID: "transient-target-b",
        suppressorID: "transient-suppressor-b",
        suppressorScore: 0.3,
        reversed: true
    )
    let changedSuppressor = try package(
        targetID: "transient-target-a",
        suppressorID: "transient-suppressor-a",
        suppressorScore: 0.31,
        reversed: false
    )
    let firstUIDs = try uids(
        first,
        targetID: "transient-target-a",
        suppressorID: "transient-suppressor-a"
    )
    let renamedUIDs = try uids(
        renamedAndReordered,
        targetID: "transient-target-b",
        suppressorID: "transient-suppressor-b"
    )
    let changedUIDs = try uids(
        changedSuppressor,
        targetID: "transient-target-a",
        suppressorID: "transient-suppressor-a"
    )

    #expect(firstUIDs.target == renamedUIDs.target)
    #expect(firstUIDs.suppressor == renamedUIDs.suppressor)
    #expect(firstUIDs.suppressor != changedUIDs.suppressor)
    #expect(firstUIDs.target != changedUIDs.target)
}

@Test
func resultPackageRejectsIncompleteOrContradictoryHFSSnapshots() {
    let fixture = resultPackageFixture()
    let train = fixture.dataset.trains[0]
    let hfsID = "snapshot-hfs::state-split::1-3"
    let hfsCandidate = resultPackageHFSCandidate(train: train, id: hfsID)

    func expectRejected(
        candidates: [ClassicAnchorCandidate],
        row: HFSBurstArbitrationAuditRow
    ) {
        let run = resultPackageRun(
            from: fixture.run,
            appending: candidates,
            auditRows: [row]
        )
        #expect(throws: STPDResultPackageError.self) {
            _ = try STPDResultPackageBuilder.build(
                .automatic(dataset: fixture.dataset, run: run)
            )
        }
    }

    expectRejected(
        candidates: [hfsCandidate],
        row: resultPackageHFSRow(
            train: train,
            candidateID: hfsID,
            rootID: "snapshot-hfs",
            rowID: "partial-strongest-burst",
            strongestBurstCandidateID: "missing-burst"
        )
    )
    expectRejected(
        candidates: [hfsCandidate],
        row: resultPackageHFSRow(
            train: train,
            candidateID: hfsID,
            rootID: "snapshot-hfs",
            rowID: "partial-long-burst",
            strongestLongBurstCandidateID: "missing-long-burst"
        )
    )

    let burstID = "snapshot-burst::1-3"
    let burstCandidate = resultPackageHFSCandidate(
        train: train,
        id: burstID,
        score: 0.4,
        candidateClass: ClassicAnchorLabel.burst.rawValue,
        finalLabel: .burst,
        priority: 77
    )
    expectRejected(
        candidates: [hfsCandidate, burstCandidate],
        row: resultPackageHFSRow(
            train: train,
            candidateID: hfsID,
            rootID: "snapshot-hfs",
            rowID: "contradictory-strongest-burst",
            strongestBurstCandidateID: burstID,
            strongestBurstSubtype: "burst_i",
            strongestBurstRawScore: 0.41,
            strongestBurstPriority: 77
        )
    )

    let nonHFSRoot = resultPackageHFSCandidate(
        train: train,
        id: "snapshot-non-hfs-root",
        candidateClass: "review",
        finalLabel: .reject,
        anchorFamily: "review"
    )
    expectRejected(
        candidates: [hfsCandidate, nonHFSRoot],
        row: resultPackageHFSRow(
            train: train,
            candidateID: hfsID,
            rootID: nonHFSRoot.id,
            rowID: "non-hfs-root-reference"
        )
    )

    let nonOverlappingBurstID = "snapshot-non-overlap-burst::4-6"
    let nonOverlappingBurst = resultPackageHFSCandidate(
        train: train,
        id: nonOverlappingBurstID,
        score: 0.4,
        candidateClass: ClassicAnchorLabel.burst.rawValue,
        finalLabel: .burst,
        priority: 77,
        startISIIndex: 4,
        endISIIndex: 6,
        anchorFamily: "burst"
    )
    expectRejected(
        candidates: [hfsCandidate, nonOverlappingBurst],
        row: resultPackageHFSRow(
            train: train,
            candidateID: hfsID,
            rootID: "snapshot-hfs",
            rowID: "non-overlap-strongest-reference",
            strongestBurstCandidateID: nonOverlappingBurstID,
            strongestBurstSubtype: "burst_i",
            strongestBurstRawScore: 0.4,
            strongestBurstPriority: 77
        )
    )
}

@Test
func resultPackageWriterUsesRFC4180AndPublishesACompleteDirectory() throws {
    let fixture = resultPackageFixture()
    let automatic = STPDResultPackageInput.automatic(
        dataset: fixture.dataset,
        run: fixture.run
    )
    let publicCandidateID = try #require(
        automatic.finalISILabelRows.first {
            $0.isiIndex > 0 && !$0.autoCandidateID.isEmpty
        }?.autoCandidateID
    )
    let sourceCandidate = try #require(
        fixture.run.candidates.first { $0.id == publicCandidateID }
    )
    let annotation = try resultPackageManualTonicAnnotation(
        dataset: fixture.dataset,
        run: fixture.run,
        id: UUID(uuidString: "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee")!,
        note: "note, with \"quote\"\nand line"
    )
    let input = try resultPackageReviewedInput(
        dataset: fixture.dataset,
        run: fixture.run,
        manualAnnotations: [annotation],
        candidateReviews: [
            STPDCandidateReviewInput(
                sourceCandidateID: sourceCandidate.id,
                status: .modified,
                reviewer: "Reviewer B",
                note: "review, \"quoted\"\nline"
            )
        ]
    )
    let package = try STPDResultPackageBuilder.build(input)
    let root = FileManager.default.temporaryDirectory.appendingPathComponent(
        "stpd-result-package-tests-\(UUID().uuidString.lowercased())",
        isDirectory: true
    )
    let destination = root.appendingPathComponent("result", isDirectory: true)
    defer { try? FileManager.default.removeItem(at: root) }

    try STPDResultPackageWriter.write(package, to: destination)

    let files = try FileManager.default.contentsOfDirectory(
        at: destination,
        includingPropertiesForKeys: nil
    )
    #expect(Set(files.map(\.lastPathComponent)) ==
        Set(STPDResultTable.allCases.map(\.rawValue) + [STPDResultSchema.manifestFileName]))

    let manualCSV = try String(
        contentsOf: destination.appendingPathComponent(
            STPDResultTable.manualAnnotations.rawValue
        ),
        encoding: .utf8
    )
    #expect(manualCSV.contains("\"note, with \"\"quote\"\"\nand line\""))
    #expect(manualCSV.contains("\r\n"))

    let manifestData = try Data(
        contentsOf: destination.appendingPathComponent(STPDResultSchema.manifestFileName)
    )
    let decoded = try JSONDecoder().decode(STPDResultManifest.self, from: manifestData)
    #expect(decoded == package.manifest)
    for tableEntry in decoded.tables {
        let data = try Data(
            contentsOf: destination.appendingPathComponent(tableEntry.fileName)
        )
        #expect(tableEntry.sha256 == STPDStableIdentifier.digest(data))
    }

    #expect(throws: STPDResultPackageError.self) {
        try STPDResultPackageWriter.write(package, to: destination)
    }
    let siblings = try FileManager.default.contentsOfDirectory(
        at: root,
        includingPropertiesForKeys: nil
    )
    #expect(siblings.map(\.lastPathComponent) == ["result"])
}

@Test
func resultPackageRFC4180EscapesCommasQuotesAndNewlines() {
    let data = STPDRFC4180.data(
        headers: ["first", "second"],
        rows: [["plain", "comma, quote \"x\"\nand line"]]
    )
    #expect(
        String(decoding: data, as: UTF8.self) ==
        "first,second\r\nplain,\"comma, quote \"\"x\"\"\nand line\"\r\n"
    )
}

@Test
func resultPackageRejectsIncompleteISICoverage() {
    let fixture = resultPackageFixture()
    let automatic = STPDResultPackageInput.automatic(
        dataset: fixture.dataset,
        run: fixture.run
    )
    let input = STPDResultPackageInput(
        dataset: fixture.dataset,
        run: fixture.run,
        sourceMode: .automatic,
        finalEvents: automatic.finalEvents,
        finalISILabelRows: Array(automatic.finalISILabelRows.dropLast())
    )

    #expect(throws: STPDResultPackageError.self) {
        _ = try STPDResultPackageBuilder.build(input)
    }
}

@Test
func resultPackageRejectsLegacyUnidentifiedRun() {
    let dataset = SpikeDataset(
        name: "legacy",
        sourceDescription: "unit-test",
        trains: [SpikeTrain(name: "train", timestampsSec: [0, 0.1, 0.2])]
    )
    let run = ClassicAnchorDetectionRun(
        bandSettings: .init(),
        qualitySettings: .init(),
        resolutions: [],
        results: []
    )

    #expect(throws: STPDResultPackageError.self) {
        _ = try STPDResultPackageBuilder.build(
            .automatic(dataset: dataset, run: run)
        )
    }
}

@Test
func resultPackageTableDataRejectsMalformedSchemasAndTypedCells() {
    let contract = STPDResultTableContract(
        table: .runMetadata,
        grain: "test row",
        primaryKey: ["run_id"],
        requiredIdentityColumns: ["run_id"]
    )
    let headers = ["run_id", "integer_value", "real_value", "boolean_value", "timestamp_value"]
    let definitions: [STPDResultColumnDefinition] = [
        .init(name: "run_id", type: .string, nullable: false),
        .init(name: "integer_value", type: .integer, nullable: false),
        .init(name: "real_value", type: .real, nullable: false),
        .init(name: "boolean_value", type: .boolean, nullable: false),
        .init(name: "timestamp_value", type: .timestamp, nullable: false),
    ]
    let validRow = ["run", "1", "0.5", "true", "2026-07-24T00:00:00Z"]

    #expect(throws: STPDResultPackageError.self) {
        _ = try STPDResultTableData(
            contract: contract,
            headers: ["run_id", "run_id"],
            rows: [["run", "duplicate"]]
        )
    }
    #expect(throws: STPDResultPackageError.self) {
        _ = try STPDResultTableData(
            contract: contract,
            headers: headers,
            columnDefinitions: Array(definitions.dropLast()),
            rows: [validRow]
        )
    }
    for (column, invalidValue) in [
        (0, ""),
        (1, "1.5"),
        (2, "nan"),
        (3, "yes"),
        (4, "not-a-timestamp"),
    ] {
        var row = validRow
        row[column] = invalidValue
        #expect(throws: STPDResultPackageError.self) {
            _ = try STPDResultTableData(
                contract: contract,
                headers: headers,
                columnDefinitions: definitions,
                rows: [row]
            )
        }
    }
}

@Test
func resultPackageSchemaIsCompleteTypedAndStable() throws {
    let fixture = resultPackageFixture()
    let package = try STPDResultPackageBuilder.build(
        .automatic(dataset: fixture.dataset, run: fixture.run)
    )
    let expectedSchemaDigests: [STPDResultTable: String] = [
        .runMetadata: "9df036426a42a691890c11068b986549c164da32d644daf919d551387b8ef554",
        .parametersReport: "851e0fac9fac8269caadee89ef237fba838267c12caf420bd37065957efaf441",
        .resolvedParameters: "3e788241ca08dc2366abc20be401cb49bfef4a818a2bd8770d3d813bec7f1698",
        .candidateLedger: "b801819d0da346b98b1eb635f9e09a548114c25638bc280d5b9a630be7e3cf71",
        .candidateFeatures: "e01ed6778f97fcf1527487fdf98f9c51b821215e270bf2303b9dd59b2c7353a9",
        .finalDecisions: "da9f2568449651a7452064cf98a0044ea0c0f1123bd0327070f011a420a29c9d",
        .eventsFinal: "5c1542982f9fb98d1a1a1d048a1ff1a4e3d343cfe79fd1bb920e4330f086cee9",
        .isiLabelsFinal: "5b955914c2fe680ec83f7f0b6c3b22c02a3965deb40a4639f74564f423e45a59",
        .candidateDiagnosticAudit: "648dbb1f71d191c0d9bb9974cd16ca5157f7045e30d0badef062ca91055ac6d9",
        .resultConsistencyCheck: "9ac0cbfa6f1ef344c4cf1dbc5e3b6724e331efecc3a705f51aed11af25f314ef",
        .manualAnnotations: "c09794a9e5d3dcb699b9a9b882b56a9bb60d75b32752f01eb42069d834dfded0",
        .reviewStatus: "2d508fd12c9f408dd190ce9453194959e303dc212285ce8f84d69d982e9baa71",
        .hfsBurstArbitrationAudit:
            "2c42cd159f246affaf7d5e61132959a1c1102f7992c5fbbddd07d624f6dc0c6c",
    ]

    #expect(Set(expectedSchemaDigests.keys) == Set(STPDResultTable.allCases))
    for table in STPDResultTable.allCases {
        let data = try #require(package.table(table))
        let schema = data.columnDefinitions.map {
            "\($0.name):\($0.type.rawValue):\($0.nullable ? "1" : "0")"
        }.joined(separator: "|")
        #expect(STPDStableIdentifier.digest(Data(schema.utf8))
            == expectedSchemaDigests[table])
        #expect(data.headers == data.columnDefinitions.map(\.name))
        let manifestEntry = try #require(
            package.manifest.tables.first { $0.fileName == table.rawValue }
        )
        #expect(manifestEntry.columns == data.columnDefinitions)
        for primaryKey in data.contract.primaryKey {
            let definition = try #require(
                data.columnDefinitions.first { $0.name == primaryKey }
            )
            #expect(definition.nullable == false)
        }
    }

    func definition(
        _ table: STPDResultTable,
        _ name: String
    ) throws -> STPDResultColumnDefinition {
        try #require(
            package.table(table)?.columnDefinitions.first { $0.name == name }
        )
    }

    let requiredColumns: [(STPDResultTable, String)] = [
        (.runMetadata, "run_id"),
        (.runMetadata, "settings_digest"),
        (.runMetadata, "dataset_digest"),
        (.runMetadata, "source_mode"),
        (.candidateLedger, "candidate_uid"),
        (.candidateLedger, "train_id"),
        (.candidateLedger, "start_isi_index"),
        (.candidateLedger, "start_spike_ordinal"),
        (.finalDecisions, "candidate_uid"),
        (.finalDecisions, "final_label"),
        (.finalDecisions, "selection_status"),
        (.eventsFinal, "event_uid"),
        (.eventsFinal, "final_label"),
        (.eventsFinal, "start_isi_index"),
        (.eventsFinal, "end_isi_index"),
        (.eventsFinal, "source_event_ids"),
        (.eventsFinal, "source_candidate_uids"),
        (.eventsFinal, "review_evidence_uids"),
        (.isiLabelsFinal, "isi_uid"),
        (.isiLabelsFinal, "train_id"),
        (.isiLabelsFinal, "isi_index"),
        (.isiLabelsFinal, "left_spike_array_index"),
        (.isiLabelsFinal, "right_spike_array_index"),
        (.isiLabelsFinal, "left_spike_ordinal"),
        (.isiLabelsFinal, "right_spike_ordinal"),
        (.manualAnnotations, "start_isi_index"),
        (.hfsBurstArbitrationAudit, "hfs_candidate_uid"),
        (.hfsBurstArbitrationAudit, "audit_hfs_raw_score"),
        (.hfsBurstArbitrationAudit, "audit_final_decision"),
    ]
    for (table, name) in requiredColumns {
        #expect(try definition(table, name).nullable == false)
    }

    let optionalColumns: [(STPDResultTable, String)] = [
        (.runMetadata, "dataset_source"),
        (.finalDecisions, "failure_reason"),
        (.finalDecisions, "state_tonic_subtype"),
        (.eventsFinal, "state_tonic_subtype"),
        (.manualAnnotations, "note"),
        (.hfsBurstArbitrationAudit, "strongest_burst_candidate_uid"),
        (.hfsBurstArbitrationAudit, "strongest_long_burst_candidate_uid"),
        (.hfsBurstArbitrationAudit, "audit_strongest_burst_candidate_id"),
        (.hfsBurstArbitrationAudit, "audit_long_burst_raw_score"),
    ]
    for (table, name) in optionalColumns {
        #expect(try definition(table, name).nullable == true)
    }

    let typedColumns: [(STPDResultTable, String, STPDResultColumnType)] = [
        (.candidateFeatures, "hf_burst_packet_like", .boolean),
        (.candidateFeatures, "hf_burst_packet_neighbor", .boolean),
        (.candidateFeatures, "suppressed_by_hf_state", .boolean),
        (.candidateFeatures, "state_continuity_authority_frozen", .boolean),
        (.candidateFeatures, "state_continuity_merge_terminal", .boolean),
        (.candidateFeatures, "hf_min_spikes_required", .integer),
        (.candidateFeatures, "score", .real),
        (.candidateFeatures, "profile_burst_contrast_s", .real),
        (.isiLabelsFinal, "left_spike_array_index", .integer),
        (.isiLabelsFinal, "right_spike_array_index", .integer),
        (.isiLabelsFinal, "left_spike_ordinal", .integer),
        (.isiLabelsFinal, "right_spike_ordinal", .integer),
        (.isiLabelsFinal, "train_qc_firing_rate_hz", .real),
        (.eventsFinal, "source_event_ids", .stringList),
        (.eventsFinal, "source_candidate_uids", .stringList),
        (.eventsFinal, "state_high_frequency_subtypes", .stringList),
        (.hfsBurstArbitrationAudit, "package_final_event_subtypes", .stringList),
        (.hfsBurstArbitrationAudit, "unresolved_candidate_ids", .stringList),
    ]
    for (table, name, type) in typedColumns {
        #expect(try definition(table, name).type == type)
    }

    let ledger = try #require(package.table(.candidateLedger))
    let requiredIndex = try #require(ledger.headers.firstIndex(of: "candidate_uid"))
    var invalidRequiredRows = ledger.rows
    invalidRequiredRows[0][requiredIndex] = ""
    #expect(throws: STPDResultPackageError.self) {
        _ = try STPDResultTableData(
            contract: ledger.contract,
            headers: ledger.headers,
            columnDefinitions: ledger.columnDefinitions,
            rows: invalidRequiredRows
        )
    }

    let metadata = try #require(package.table(.runMetadata))
    let optionalIndex = try #require(metadata.headers.firstIndex(of: "dataset_source"))
    var validOptionalRows = metadata.rows
    validOptionalRows[0][optionalIndex] = ""
    _ = try STPDResultTableData(
        contract: metadata.contract,
        headers: metadata.headers,
        columnDefinitions: metadata.columnDefinitions,
        rows: validOptionalRows
    )

    let featureTypes = Dictionary(
        uniqueKeysWithValues: try #require(package.table(.candidateFeatures))
            .columnDefinitions.map { ($0.name, $0.type) }
    )
    for name in [
        "burst_boundary_floor_hard", "burst_strict_boundary_pass",
        "burst_possible_boundary_pass", "burst_bridge_count_pass",
        "burst_bridge_fraction_pass", "burst_q90_bridge_pass",
        "hard_threshold", "hf_burst_dominated", "anchor_band_ordered",
    ] {
        #expect(featureTypes[name] == .boolean)
    }
    for name in [
        "profile_max_seed_run_length", "hf_max_consecutive_large_isi",
        "state_core_burst_run_length", "burst_seed_run_start_isi",
        "burst_seed_run_end_isi", "hard_burst_core_isi_count",
    ] {
        #expect(featureTypes[name] == .integer)
    }
    for name in [
        "profile_seed_low_percentile", "profile_seed_high_percentile",
        "hf_embedded_burst_coverage", "state_train_percentile_median",
        "state_local_percentile_median", "state_local_robust_z_median",
        "anchor_contrast_min_required", "anchor_contrast_geom_required",
        "event_local_percentile_median", "event_local_robust_z_median",
    ] {
        #expect(featureTypes[name] == .real)
    }

    let hfsTypes = Dictionary(
        uniqueKeysWithValues: try #require(package.table(.hfsBurstArbitrationAudit))
            .columnDefinitions.map { ($0.name, $0.type) }
    )
    for name in [
        "audit_hfs_start_isi", "audit_hfs_end_isi",
        "audit_conflict_start_isi", "audit_conflict_end_isi",
    ] {
        #expect(hfsTypes[name] == .integer)
    }
    #expect(hfsTypes["audit_packet_coverage"] == .real)
    #expect(
        try #require(package.table(.finalDecisions))
            .columnDefinitions.first { $0.name == "audit_review_required" }?.type
            == .boolean
    )
}
