import Darwin
import Foundation
import Testing
@testable import STPDCore

private let resultPackageBuildCommit = "0123456789abcdef0123456789abcdef01234567"
private let resultPackageCurrentReaderEntryPoint:
    @Sendable (URL, STPDResultPackageReadLimits) throws
        -> STPDResultPackageReadResult =
    STPDResultPackageReader.read
private let resultPackageLegacyReaderEntryPoint =
    STPDResultPackageReader.read
private let resultPackageCurrentWriterEntryPoint:
    @Sendable (
        STPDResultPackage,
        URL,
        STPDResultPackageParentDirectoryPolicy
    ) throws
        -> Void =
    STPDResultPackageWriter.write
private let resultPackageLegacyWriterEntryPoint =
    STPDResultPackageWriter.write

private struct ResultPackageWriterInjectedFailure: Error {}

private struct ResultPackageACLTestFailure: Error {
    let operation: String
    let code: Int32
}

private enum ResultPackageTestACLKind {
    case allowWrite(inheritable: Bool)
    case denyDelete
}

private func installResultPackageTestExtendedACL(
    at directory: URL,
    kind: ResultPackageTestACLKind = .allowWrite(inheritable: false)
) throws {
    let descriptor = open(
        directory.path,
        O_RDONLY | O_DIRECTORY | O_CLOEXEC
    )
    guard descriptor >= 0 else {
        throw ResultPackageACLTestFailure(
            operation: "open",
            code: errno
        )
    }
    defer {
        _ = close(descriptor)
    }

    let rule: String
    switch kind {
    case .allowWrite(let inheritable):
        rule =
            "group:ABCDEFAB-CDEF-ABCD-EFAB-CDEF0000000C:"
            + "everyone:12:allow:write,delete_child"
            + (inheritable ? ":file_inherit,directory_inherit" : "")
    case .denyDelete:
        rule =
            "group:ABCDEFAB-CDEF-ABCD-EFAB-CDEF0000000C:"
            + "everyone:12:deny:delete"
    }
    let text = "!#acl 1\n\(rule)\n"
    let acl = text.withCString { acl_from_text($0) }
    guard let acl else {
        throw ResultPackageACLTestFailure(
            operation: "acl_from_text",
            code: errno
        )
    }
    defer {
        _ = acl_free(UnsafeMutableRawPointer(acl))
    }
    guard acl_set_fd_np(
        descriptor,
        acl,
        ACL_TYPE_EXTENDED
    ) == 0 else {
        throw ResultPackageACLTestFailure(
            operation: "acl_set_fd_np",
            code: errno
        )
    }
}

private func resultPackageEntryHasExtendedACL(at url: URL) throws -> Bool {
    let descriptor = open(
        url.path,
        O_RDONLY | O_NOFOLLOW | O_CLOEXEC
    )
    guard descriptor >= 0 else {
        throw ResultPackageACLTestFailure(
            operation: "open for ACL inspection",
            code: errno
        )
    }
    defer {
        _ = close(descriptor)
    }
    errno = 0
    guard let acl = acl_get_fd_np(descriptor, ACL_TYPE_EXTENDED) else {
        if errno == ENOENT {
            return false
        }
        throw ResultPackageACLTestFailure(
            operation: "acl_get_fd_np",
            code: errno
        )
    }
    defer {
        _ = acl_free(UnsafeMutableRawPointer(acl))
    }
    var entry: acl_entry_t?
    let result = acl_get_entry(
        acl,
        Int32(ACL_FIRST_ENTRY.rawValue),
        &entry
    )
    guard result == 0 || result == 1 else {
        throw ResultPackageACLTestFailure(
            operation: "acl_get_entry",
            code: errno
        )
    }
    return result == 0 && entry != nil
}

private func resultPackageFixture(
    taskEvents: [TaskEvent] = []
) -> (
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
        ],
        taskEvents: taskEvents
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

private func resultPackageReplacingCell(
    _ table: STPDResultTableData,
    row rowIndex: Int = 0,
    column: String,
    with value: String
) throws -> STPDResultTableData {
    let columnIndex = try #require(table.headers.firstIndex(of: column))
    var rows = table.rows
    try #require(rows.indices.contains(rowIndex))
    rows[rowIndex][columnIndex] = value
    return try STPDResultTableData(
        contract: table.contract,
        headers: table.headers,
        columnDefinitions: table.columnDefinitions,
        rows: rows
    )
}

private func resultPackageReplacingCells(
    _ table: STPDResultTableData,
    row rowIndex: Int = 0,
    updates: [String: String]
) throws -> STPDResultTableData {
    var rows = table.rows
    try #require(rows.indices.contains(rowIndex))
    for (column, value) in updates {
        let columnIndex = try #require(table.headers.firstIndex(of: column))
        rows[rowIndex][columnIndex] = value
    }
    return try STPDResultTableData(
        contract: table.contract,
        headers: table.headers,
        columnDefinitions: table.columnDefinitions,
        rows: rows
    )
}

private func resultPackageReplacingCells(
    _ table: STPDResultTableData,
    whereColumn column: String,
    equals expectedValue: String,
    updates: [String: String]
) throws -> STPDResultTableData {
    let predicateIndex = try #require(table.headers.firstIndex(of: column))
    var rows = table.rows
    var mutationCount = 0
    for rowIndex in rows.indices
    where rows[rowIndex][predicateIndex] == expectedValue {
        for (updateColumn, value) in updates {
            let updateIndex = try #require(
                table.headers.firstIndex(of: updateColumn)
            )
            rows[rowIndex][updateIndex] = value
        }
        mutationCount += 1
    }
    #expect(mutationCount > 0)
    return try STPDResultTableData(
        contract: table.contract,
        headers: table.headers,
        columnDefinitions: table.columnDefinitions,
        rows: rows
    )
}

private func resultPackageEventUID(
    identity: DetectionRunIdentity,
    table: STPDResultTableData,
    row rowIndex: Int = 0
) throws -> String {
    try #require(table.rows.indices.contains(rowIndex))
    let values = Dictionary(
        uniqueKeysWithValues: zip(table.headers, table.rows[rowIndex])
    )
    let scientificColumns = [
        "train_id",
        "final_label",
        "final_subtype",
        "state_tonic_subtype",
        "state_high_frequency_subtypes",
        "start_isi_index",
        "end_isi_index",
        "start_spike_ordinal",
        "end_spike_ordinal",
        "raw_start_sec",
        "raw_end_sec",
        "aligned_start_sec",
        "aligned_end_sec",
    ]
    return STPDStableIdentifier.make(
        prefix: "event",
        domain: "stpd_normalized_public_event_uid_v5",
        components: [identity.datasetDigest] +
            scientificColumns.map { values[$0] ?? "" }
    )
}

private func resultPackageRefreshingManualSemanticDigest(
    _ table: STPDResultTableData,
    row rowIndex: Int = 0
) throws -> STPDResultTableData {
    let semanticColumns = [
        "source_annotation_uuid",
        "authority_source",
        "import_approval_id",
        "train_id",
        "label",
        "polarity",
        "start_sec",
        "end_sec",
        "start_isi_index",
        "end_isi_index",
        "start_spike_array_index",
        "end_spike_array_index",
        "start_spike_ordinal",
        "end_spike_ordinal",
        "linked_isi_uids",
        "link_scope",
        "note",
        "annotator",
        "annotator_identity_source",
        "created_at",
        "updated_at",
        "created_at_unix_sec",
        "updated_at_unix_sec",
    ]
    let digestIndex = try #require(
        table.headers.firstIndex(of: "annotation_semantic_digest")
    )
    var rows = table.rows
    try #require(rows.indices.contains(rowIndex))
    let values = Dictionary(
        uniqueKeysWithValues: zip(table.headers, rows[rowIndex])
    )
    rows[rowIndex][digestIndex] = STPDStableIdentifier.make(
        prefix: "manual_semantics",
        domain: "stpd_manual_annotation_semantics_v2",
        components: semanticColumns.map { values[$0] ?? "" }
    )
    return try STPDResultTableData(
        contract: table.contract,
        headers: table.headers,
        columnDefinitions: table.columnDefinitions,
        rows: rows
    )
}

private func resultPackageValidate(
    _ package: STPDResultPackage,
    tables: [STPDResultTable: STPDResultTableData],
    dataset: SpikeDataset,
    run: ClassicAnchorDetectionRun,
    expectedCandidateReviews: [STPDCandidateReviewInput]? = nil,
    expectedManualAnnotations: [ManualAnnotation]? = nil,
    expectedManualAnnotationImportApprovals:
        [ManualAnnotationCSVApprovalReceipt]? = nil,
    expectedCandidateDiagnostics: [STPDCandidateDiagnosticInput]? = nil
) throws -> [STPDConsistencyCheck] {
    try STPDResultPackageValidator.validate(
        identity: package.identity,
        sourceMode: package.sourceMode,
        tables: tables,
        expectedISICount: dataset.trains.reduce(0) {
            $0 + max(0, $1.spikeCount - 1)
        },
        expectedTaskEvents: dataset.taskEvents,
        expectedDatasetMetadata: run.datasetMetadataSnapshot,
        expectedTrainIDs: Set(dataset.trains.map(\.id)),
        expectedDataset: dataset,
        expectedQualitySettings: run.qualitySettings,
        expectedRun: run,
        expectedCandidateReviews: expectedCandidateReviews,
        expectedManualAnnotations: expectedManualAnnotations,
        expectedManualAnnotationImportApprovals:
            expectedManualAnnotationImportApprovals,
        expectedCandidateDiagnostics: expectedCandidateDiagnostics
    )
}

private func resultPackageReplacingEvidenceUID(
    _ table: STPDResultTableData,
    oldUID: String,
    newUID: String
) throws -> STPDResultTableData {
    let columnIndex = try #require(
        table.headers.firstIndex(of: "review_evidence_uids")
    )
    var rows = table.rows
    for rowIndex in rows.indices {
        let values = try #require(
            STPDCanonicalValue.parseStringList(rows[rowIndex][columnIndex])
        )
        rows[rowIndex][columnIndex] = STPDCanonicalValue.stringList(
            values.map { $0 == oldUID ? newUID : $0 }
        )
    }
    return try STPDResultTableData(
        contract: table.contract,
        headers: table.headers,
        columnDefinitions: table.columnDefinitions,
        rows: rows
    )
}

private func resultPackageRemovingEvidenceUID(
    _ table: STPDResultTableData,
    row rowIndex: Int,
    evidenceUID: String
) throws -> STPDResultTableData {
    let evidenceIndex = try #require(
        table.headers.firstIndex(of: "review_evidence_uids")
    )
    let evidencePresentIndex = try #require(
        table.headers.firstIndex(of: "review_evidence_present")
    )
    var rows = table.rows
    try #require(rows.indices.contains(rowIndex))
    let evidence = try #require(
        STPDCanonicalValue.parseStringList(rows[rowIndex][evidenceIndex])
    )
    try #require(evidence.contains(evidenceUID))
    let retained = evidence.filter { $0 != evidenceUID }.sorted()
    rows[rowIndex][evidenceIndex] = STPDCanonicalValue.stringList(retained)
    rows[rowIndex][evidencePresentIndex] = retained.isEmpty ? "false" : "true"
    return try STPDResultTableData(
        contract: table.contract,
        headers: table.headers,
        columnDefinitions: table.columnDefinitions,
        rows: rows
    )
}

private func resultPackageRemovingRows(
    _ table: STPDResultTableData,
    whereColumn column: String,
    equals expectedValue: String
) throws -> STPDResultTableData {
    let predicateIndex = try #require(table.headers.firstIndex(of: column))
    let rows = table.rows.filter { $0[predicateIndex] != expectedValue }
    #expect(rows.count < table.rows.count)
    return try STPDResultTableData(
        contract: table.contract,
        headers: table.headers,
        columnDefinitions: table.columnDefinitions,
        rows: rows
    )
}

private func resultPackageRemovingEvidenceUIDFromAllRows(
    _ table: STPDResultTableData,
    evidenceUID: String
) throws -> STPDResultTableData {
    let evidenceIndex = try #require(
        table.headers.firstIndex(of: "review_evidence_uids")
    )
    let evidencePresentIndex = try #require(
        table.headers.firstIndex(of: "review_evidence_present")
    )
    var rows = table.rows
    for rowIndex in rows.indices {
        let evidence = try #require(
            STPDCanonicalValue.parseStringList(rows[rowIndex][evidenceIndex])
        )
        let retained = evidence.filter { $0 != evidenceUID }.sorted()
        rows[rowIndex][evidenceIndex] =
            STPDCanonicalValue.stringList(retained)
        rows[rowIndex][evidencePresentIndex] =
            retained.isEmpty ? "false" : "true"
    }
    return try STPDResultTableData(
        contract: table.contract,
        headers: table.headers,
        columnDefinitions: table.columnDefinitions,
        rows: rows
    )
}

private func resultPackageSynchronizingEventEvidence(
    _ eventTable: STPDResultTableData,
    with isiTable: STPDResultTableData
) throws -> STPDResultTableData {
    let isiTrainIndex = try #require(
        isiTable.headers.firstIndex(of: "train_id")
    )
    let isiIndex = try #require(
        isiTable.headers.firstIndex(of: "isi_index")
    )
    let isiEvidenceIndex = try #require(
        isiTable.headers.firstIndex(of: "review_evidence_uids")
    )
    let eventTrainIndex = try #require(
        eventTable.headers.firstIndex(of: "train_id")
    )
    let eventStartIndex = try #require(
        eventTable.headers.firstIndex(of: "start_isi_index")
    )
    let eventEndIndex = try #require(
        eventTable.headers.firstIndex(of: "end_isi_index")
    )
    let eventEvidenceIndex = try #require(
        eventTable.headers.firstIndex(of: "review_evidence_uids")
    )
    let eventEvidencePresentIndex = try #require(
        eventTable.headers.firstIndex(of: "review_evidence_present")
    )
    var eventRows = eventTable.rows
    for eventRowIndex in eventRows.indices {
        let trainID = eventRows[eventRowIndex][eventTrainIndex]
        let start = try #require(
            Int(eventRows[eventRowIndex][eventStartIndex])
        )
        let end = try #require(
            Int(eventRows[eventRowIndex][eventEndIndex])
        )
        var evidence: Set<String> = []
        for isiRow in isiTable.rows
        where isiRow[isiTrainIndex] == trainID {
            guard let index = Int(isiRow[isiIndex]),
                  start...end ~= index else {
                continue
            }
            evidence.formUnion(try #require(
                STPDCanonicalValue.parseStringList(
                    isiRow[isiEvidenceIndex]
                )
            ))
        }
        let ordered = evidence.sorted()
        eventRows[eventRowIndex][eventEvidenceIndex] =
            STPDCanonicalValue.stringList(ordered)
        eventRows[eventRowIndex][eventEvidencePresentIndex] =
            ordered.isEmpty ? "false" : "true"
    }
    return try STPDResultTableData(
        contract: eventTable.contract,
        headers: eventTable.headers,
        columnDefinitions: eventTable.columnDefinitions,
        rows: eventRows
    )
}

private func resultPackageAcceptedReviewPackage(
    reviewedAt: Date = Date(timeIntervalSince1970: 300)
) throws -> STPDResultPackage {
    let fixture = resultPackageFixture()
    let automatic = STPDResultPackageInput.automatic(
        dataset: fixture.dataset,
        run: fixture.run
    )
    let candidateID = try #require(
        automatic.finalISILabelRows.first {
            $0.isiIndex > 0 && !$0.autoCandidateID.isEmpty
        }?.autoCandidateID
    )
    let input = try resultPackageReviewedInput(
        dataset: fixture.dataset,
        run: fixture.run,
        candidateReviews: [
            STPDCandidateReviewInput(
                sourceCandidateID: candidateID,
                status: .accepted,
                reviewer: "Independent reviewer",
                reviewedAt: reviewedAt
            ),
        ]
    )
    return try STPDResultPackageBuilder.build(input)
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
    let runBoundReviews = candidateReviews.map { review in
        guard review.reviewedRunID.isEmpty else { return review }
        return STPDCandidateReviewInput(
            sourceCandidateID: review.sourceCandidateID,
            status: review.status,
            reviewer: review.reviewer,
            note: review.note,
            reviewedAt: review.reviewedAt,
            reviewedRunID: run.runIdentity.runID
        )
    }
    let attributedAnnotations = manualAnnotations.map { annotation in
        var attributed = annotation
        if attributed.annotator?.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty != false {
            attributed.annotator = "Result Package Test Reviewer"
        }
        if attributed.annotatorIdentitySource == nil {
            attributed.annotatorIdentitySource = .userProvided
        }
        return attributed
    }
    return try STPDResultPackageInput.reviewed(
        dataset: dataset,
        run: run,
        manualAnnotations: attributedAnnotations,
        candidateReviews: runBoundReviews,
        candidateDiagnostics: candidateDiagnostics
    )
}

private func resultPackageCurrentSettingsSnapshot(
    run: ClassicAnchorDetectionRun,
    bandSettings: TrainAdaptiveBandSettings? = nil,
    qualitySettings: SpikeQualitySettings? = nil,
    manualThresholdProfile: ManualThresholdProfile = .automatic
) -> DetectionRunSettingsSnapshot {
    DetectionRunSettingsSnapshot.make(
        bandSettings: bandSettings ?? run.bandSettings,
        qualitySettings: qualitySettings ?? run.qualitySettings,
        refractoryAction: .warnOnly,
        stateTuning: StatePatternDetectorTuning(),
        detectorParameters: .defaults,
        manualThresholdProfile: manualThresholdProfile,
        frameworkPolicy: .legacyCompatible,
        useAdaptiveV2Canonicalization: false,
        manualThresholdScope: .allTrains
    )
}

private func resultPackageManualTonicAnnotation(
    dataset: SpikeDataset,
    run: ClassicAnchorDetectionRun,
    id: UUID,
    note: String = "",
    ordinal: Int = 0,
    createdAt: Date = Date(timeIntervalSince1970: 100),
    updatedAt: Date = Date(timeIntervalSince1970: 200)
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
        annotator: "Result Package Test Reviewer",
        annotatorIdentitySource: .userProvided,
        createdAt: createdAt,
        updatedAt: updatedAt
    )
}

private struct ResultPackageApprovedManualImportFixture {
    let sourceData: Data
    let approvedBatch: ManualAnnotationCSVApprovedBatch
    let input: STPDResultPackageInput
    let package: STPDResultPackage
}

private func resultPackageApprovedManualImport(
    dataset: SpikeDataset,
    run: ClassicAnchorDetectionRun,
    annotation: ManualAnnotation,
    approver: String = "Dr. Import Approver",
    approvedAt: Date = Date(timeIntervalSince1970: 2_500)
) throws -> ResultPackageApprovedManualImportFixture {
    try resultPackageApprovedManualImport(
        dataset: dataset,
        run: run,
        annotations: [annotation],
        approver: approver,
        approvedAt: approvedAt
    )
}

private func resultPackageApprovedManualImport(
    dataset: SpikeDataset,
    run: ClassicAnchorDetectionRun,
    annotations: [ManualAnnotation],
    approver: String = "Dr. Import Approver",
    approvedAt: Date = Date(timeIntervalSince1970: 2_500)
) throws -> ResultPackageApprovedManualImportFixture {
    let csv = ManualAnnotationCSVExporter.csv(
        annotations: annotations,
        identity: .forDataset(
            dataset,
            runID: run.runIdentity.runID,
            reviewState: .pendingConfirmation
        )
    )
    let data = Data(csv.utf8)
    let imported = try ManualAnnotationCSVImporter.importIdentityBound(
        data: data
    )
    let gated = ManualAnnotationCSVImporter.gate(
        imported,
        activeDataset: dataset
    )
    #expect(gated.authority == .eligibleAfterExplicitConfirmation)
    let approval = try #require(
        ManualAnnotationCSVApproval(
            sourceFileDigest: gated.sourceFileDigest,
            activeDatasetDigest: gated.activeDatasetDigest,
            approvedRunID: run.runIdentity.runID,
            approvedSettingsDigest:
                run.runIdentity.settingsDigest,
            approver: approver,
            approvedAt: approvedAt
        )
    )
    let batch = try gated.authoritativeBatch(approval: approval)
    let input = try STPDResultPackageBuilder
        .preflightManualAnnotationImport(
            dataset: dataset,
            run: run,
            approvedBatch: batch
        )
    let package = try STPDResultPackageBuilder.build(input)
    return ResultPackageApprovedManualImportFixture(
        sourceData: data,
        approvedBatch: batch,
        input: input,
        package: package
    )
}

private func resultPackageMultiISITonicAnnotation(
    dataset: SpikeDataset,
    run: ClassicAnchorDetectionRun,
    id: UUID
) throws -> ManualAnnotation {
    let automatic = STPDResultPackageInput.automatic(
        dataset: dataset,
        run: run
    )
    let groupedRows = Dictionary(
        grouping: automatic.finalISILabelRows.filter {
            $0.isiIndex > 0
                && !$0.autoCandidateID.isEmpty
                && $0.autoPattern != ManualAnnotationLabel.tonic.rawValue
        },
        by: \.autoCandidateID
    )
    let rows = try #require(
        groupedRows.values
            .map { $0.sorted { $0.isiIndex < $1.isiIndex } }
            .first { rows in
                guard rows.count >= 3,
                      Set(rows.map(\.trainID)).count == 1 else {
                    return false
                }
                return zip(rows, rows.dropFirst()).allSatisfy {
                    $0.1.isiIndex == $0.0.isiIndex + 1
                }
            }
    )
    let first = try #require(rows.first)
    let last = try #require(rows.last)
    let train = try #require(
        dataset.trains.first { $0.id == first.trainID }
    )
    return ManualAnnotation(
        id: id,
        trainID: train.id,
        label: .tonic,
        startSec: train.timestampsSec[first.isiIndex - 1],
        endSec: train.timestampsSec[last.isiIndex],
        note: "multi-ISI causal evidence fixture",
        annotator: "Result Package Test Reviewer",
        annotatorIdentitySource: .userProvided,
        createdAt: Date(timeIntervalSince1970: 100),
        updatedAt: Date(timeIntervalSince1970: 200)
    )
}

@Test
func resultPackageBuildsAllNineteenNormalizedTables() throws {
    let fixture = resultPackageFixture()
    let package = try STPDResultPackageBuilder.build(
        .automatic(dataset: fixture.dataset, run: fixture.run)
    )

    #expect(STPDResultSchema.version == "stpd_result_package_v4")
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

    let diagnosticCandidateCount = fixture.run.candidates.count
    let publicCandidateCount = try #require(
        package.table(.candidateLedger)?.rowCount
    )
    #expect(package.table(.candidateFeatures)?.rowCount == publicCandidateCount)
    #expect(package.table(.finalDecisions)?.rowCount == publicCandidateCount)
    let diagnosticLedger = try #require(
        package.table(.candidateLedgerDiagnostic)
    )
    let diagnosticFeatures = try #require(
        package.table(.candidateFeaturesDiagnostic)
    )
    let diagnosticDecisions = try #require(
        package.table(.finalDecisionsDiagnostic)
    )
    #expect(diagnosticLedger.rowCount == diagnosticCandidateCount)
    #expect(diagnosticFeatures.rowCount == diagnosticCandidateCount)
    #expect(diagnosticDecisions.rowCount == diagnosticCandidateCount)
    let diagnosticUIDs = Set(
        try resultPackageColumn(diagnosticLedger, "candidate_uid")
    )
    #expect(Set(try resultPackageColumn(
        diagnosticFeatures,
        "candidate_uid"
    )) == diagnosticUIDs)
    #expect(Set(try resultPackageColumn(
        diagnosticDecisions,
        "candidate_uid"
    )) == diagnosticUIDs)
    let publicUIDs = Set(try resultPackageColumn(
        #require(package.table(.candidateLedger)),
        "candidate_uid"
    ))
    #expect(publicUIDs.isSubset(of: diagnosticUIDs))
    let diagnosticRegistry = try #require(
        package.table(.candidateDiagnosticAudit)
    )
    let stageNameIndex = try #require(
        diagnosticRegistry.headers.firstIndex(of: "stage_name")
    )
    let terminalRows = diagnosticRegistry.rows.filter {
        $0[stageNameIndex] == "terminal_decision"
    }
    #expect(terminalRows.count == diagnosticCandidateCount)
    let terminalCandidateUIDIndex = try #require(
        diagnosticRegistry.headers.firstIndex(of: "candidate_uid")
    )
    #expect(Set(terminalRows.map {
        $0[terminalCandidateUIDIndex]
    }) == diagnosticUIDs)
    #expect(publicCandidateCount <= diagnosticCandidateCount)

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
        "candidate_diagnostic_one_to_one",
        "candidate_foreign_keys",
        "isi_declared_row_count",
        "isi_complete_coverage",
        "final_event_isi_consistency",
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
            sourceDescription: fixture.dataset.sourceDescription,
            taskEventSourceDigest: try #require(
                fixture.run.datasetMetadataSnapshot
            ).taskEventSourceDigest
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
func resultPackageExportPreflightAcceptsExactInvocationSnapshot() throws {
    let fixture = resultPackageFixture()
    let invocation = try #require(fixture.run.invocationSettingsSnapshot)

    try STPDResultPackageBuilder.validateExportPreflight(
        dataset: fixture.dataset,
        run: fixture.run,
        currentSettingsSnapshot: invocation
    )
}

@Test
func resultPackageExportPreflightRejectsChangedDatasetIdentity() throws {
    let fixture = resultPackageFixture()
    let invocation = try #require(fixture.run.invocationSettingsSnapshot)
    let first = try #require(fixture.dataset.trains.first)
    var changedTimestamps = first.timestampsSec
    changedTimestamps[changedTimestamps.index(before: changedTimestamps.endIndex)] += 0.001
    let changedFirst = SpikeTrain(
        name: first.name,
        timestampsSec: changedTimestamps,
        duplicateTimestampPolicy: first.duplicateTimestampPolicy
    )
    let changedDataset = SpikeDataset(
        name: fixture.dataset.name,
        sourceDescription: fixture.dataset.sourceDescription,
        trains: [changedFirst] + fixture.dataset.trains.dropFirst(),
        taskEvents: fixture.dataset.taskEvents
    )

    #expect(throws: STPDResultPackageError.self) {
        try STPDResultPackageBuilder.validateExportPreflight(
            dataset: changedDataset,
            run: fixture.run,
            currentSettingsSnapshot: invocation
        )
    }
}

@Test
func resultPackageExportPreflightRejectsDetectionSemanticChange() {
    let fixture = resultPackageFixture()
    let changedBand = TrainAdaptiveBandSettings(
        minValidISISec: fixture.run.bandSettings.minValidISISec + 0.0001,
        histogramBinWidthSec: fixture.run.bandSettings.histogramBinWidthSec,
        datasetISIBoundaryFloorSec:
            fixture.run.bandSettings.datasetISIBoundaryFloorSec
    )

    #expect(throws: STPDResultPackageError.self) {
        try STPDResultPackageBuilder.validateExportPreflight(
            dataset: fixture.dataset,
            run: fixture.run,
            currentSettingsSnapshot: resultPackageCurrentSettingsSnapshot(
                run: fixture.run,
                bandSettings: changedBand
            )
        )
    }
}

@Test
func resultPackageExportPreflightIgnoresDisplayAndLearnedProvenanceOnlyChanges() throws {
    let fixture = resultPackageFixture()
    let displayUnit: QualityDisplayUnit =
        fixture.run.qualitySettings.displayUnit == .seconds
            ? .milliseconds
            : .seconds
    let changedQuality = SpikeQualitySettings(
        artifactThresholdSec: fixture.run.qualitySettings.artifactThresholdSec,
        refractorySuspectThresholdSec:
            fixture.run.qualitySettings.refractorySuspectThresholdSec,
        displayUnit: displayUnit
    )
    var profile = ManualThresholdProfile.automatic
    profile.learnedProvenanceByKey = [
        "tonic.isi_upper": "display-only provenance note",
    ]

    try STPDResultPackageBuilder.validateExportPreflight(
        dataset: fixture.dataset,
        run: fixture.run,
        currentSettingsSnapshot: resultPackageCurrentSettingsSnapshot(
            run: fixture.run,
            qualitySettings: changedQuality,
            manualThresholdProfile: profile
        )
    )
}

@Test
func resultPackageExportsQCEventEvidenceSubtypesAndExactConsistencyChecks() throws {
    let fixture = resultPackageFixture()
    let package = try STPDResultPackageBuilder.build(
        .automatic(dataset: fixture.dataset, run: fixture.run)
    )

    let checks = try #require(package.table(.resultConsistencyCheck))
    #expect(Set(try resultPackageColumn(checks, "check_id")) == [
        "candidate_counts",
        "candidate_diagnostic_one_to_one",
        "candidate_foreign_keys",
        "candidate_one_to_one",
        "candidate_public_diagnostic_partition",
        "event_source_evidence",
        "event_source_authority",
        "final_event_isi_consistency",
        "isi_declared_row_count",
        "isi_complete_coverage",
        "isi_qc_provenance",
        "automatic_projection_authority",
        "parameter_authority",
        "candidate_population_authority",
        "primary_keys",
        "required_table_set",
        "review_authority",
        "run_metadata",
        "run_identity",
        "source_mode",
        "task_event_count",
        "task_event_projection",
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
func resultPackageIsByteDeterministicAcrossRepeatedLegacyManualCSVImports()
    throws {
    let fixture = resultPackageFixture()
    let template = try resultPackageManualTonicAnnotation(
        dataset: fixture.dataset,
        run: fixture.run,
        id: UUID(uuidString: "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee")!
    )
    let csv = """
    train_id,label,start_sec,end_sec,annotator,annotator_identity_source,created_at_unix_sec,updated_at_unix_sec
    \(template.trainID),tonic,\(STPDCanonicalValue.double(template.startSec)),\(STPDCanonicalValue.double(template.endSec)),Result Package Test Reviewer,user_provided,100,200
    """
    let firstImport = try ManualAnnotationCSVImporter.importAnnotations(
        contents: csv
    )
    let secondImport = try ManualAnnotationCSVImporter.importAnnotations(
        contents: csv
    )
    let firstAnnotation = try #require(firstImport.annotations.first)
    let secondAnnotation = try #require(secondImport.annotations.first)
    #expect(firstImport.skippedRowCount == 0)
    #expect(secondImport.skippedRowCount == 0)
    #expect(firstAnnotation.id == secondAnnotation.id)

    let first = try STPDResultPackageBuilder.build(
        try resultPackageReviewedInput(
            dataset: fixture.dataset,
            run: fixture.run,
            manualAnnotations: firstImport.annotations
        )
    )
    let second = try STPDResultPackageBuilder.build(
        try resultPackageReviewedInput(
            dataset: fixture.dataset,
            run: fixture.run,
            manualAnnotations: secondImport.annotations
        )
    )

    for table in STPDResultTable.allCases {
        #expect(first.table(table)?.csvData == second.table(table)?.csvData)
    }
    #expect(try first.manifest.encodedData() == second.manifest.encodedData())
}

@Test
func resultPackageIsByteDeterministicAcrossReviewAndDiagnosticPermutations() throws {
    let fixture = resultPackageFixture()
    let automatic = STPDResultPackageInput.automatic(
        dataset: fixture.dataset,
        run: fixture.run
    )
    let publicCandidateIDs = Set(automatic.finalISILabelRows.compactMap {
        $0.isiIndex > 0 && !$0.autoCandidateID.isEmpty
            ? $0.autoCandidateID
            : nil
    })
    let publicCandidateID = try #require(publicCandidateIDs.sorted().first)
    let nonPublicCandidateID = try #require(
        fixture.run.candidates
            .filter {
                $0.trainID != "__dataset__"
                    && !publicCandidateIDs.contains($0.id)
            }
            .map(\.id)
            .sorted()
            .first
    )
    let reviews = [
        STPDCandidateReviewInput(
            sourceCandidateID: publicCandidateID,
            status: .needsReview,
            reviewer: "Reviewer A",
            reviewedAt: Date(timeIntervalSince1970: 100)
        ),
        STPDCandidateReviewInput(
            sourceCandidateID: nonPublicCandidateID,
            status: .accepted,
            reviewer: "Reviewer B",
            reviewedAt: Date(timeIntervalSince1970: 200),
            reviewedRunID: fixture.run.runIdentity.runID
        ),
    ]
    let diagnostics = [
        STPDCandidateDiagnosticInput(
            sourceCandidateID: publicCandidateID,
            stageName: "later_stage",
            stageOrdinal: 2,
            status: "observed",
            details: "second"
        ),
        STPDCandidateDiagnosticInput(
            sourceCandidateID: nonPublicCandidateID,
            stageName: "earlier_stage",
            stageOrdinal: 1,
            status: "observed",
            details: "first"
        ),
    ]
    let firstInput = try STPDResultPackageInput.snapshot(
        dataset: fixture.dataset,
        run: fixture.run,
        candidateReviews: reviews,
        candidateDiagnostics: diagnostics
    )
    let secondInput = try STPDResultPackageInput.snapshot(
        dataset: fixture.dataset,
        run: fixture.run,
        candidateReviews: Array(reviews.reversed()),
        candidateDiagnostics: Array(diagnostics.reversed())
    )
    let first = try STPDResultPackageBuilder.build(firstInput)
    let second = try STPDResultPackageBuilder.build(secondInput)

    #expect(firstInput.candidateDiagnostics == secondInput.candidateDiagnostics)
    for table in STPDResultTable.allCases {
        #expect(first.table(table)?.csvData == second.table(table)?.csvData)
    }
    #expect(try first.manifest.encodedData() == second.manifest.encodedData())
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
    #expect(STPDCanonicalValue.parseStringList(tonicRows[0][sourceIndex]) == [])
    #expect(tonicRows[0][authorityIndex] == "manual_only")
}

@Test
func resultPackageMixedBurstFamilyProjectionPartitionsByFinalISIAuthority() throws {
    let fixture = resultPackageFixture()
    let train = fixture.dataset.trains[0]
    let burst = resultPackageHFSCandidate(
        train: train,
        id: "mixed-family-burst",
        candidateLayer: "mixed_family_fixture",
        nISI: 2,
        nSpikes: 3,
        candidateClass: ClassicAnchorLabel.burst.rawValue,
        finalLabel: .burst,
        decisionPath: "mixed_family_burst",
        action: "accept_burst",
        priority: 260,
        selectedForAuto: true,
        selectionStatus: "selected",
        startISIIndex: 1,
        endISIIndex: 2,
        anchorFamily: "burst"
    )
    let possibleBurst = resultPackageHFSCandidate(
        train: train,
        id: "mixed-family-possible-burst",
        candidateLayer: "mixed_family_fixture",
        nISI: 2,
        nSpikes: 3,
        candidateClass: ClassicAnchorLabel.possibleBurst.rawValue,
        finalLabel: .possibleBurst,
        decisionPath: "mixed_family_possible_burst",
        action: "route_possible_burst",
        priority: 250,
        selectedForAuto: true,
        selectionStatus: "selected",
        startISIIndex: 4,
        endISIIndex: 5,
        anchorFamily: "burst"
    )
    let burstEvent = ClassicAnchorEventAnnotation(
        candidate: burst,
        train: train
    )
    let possibleBurstEvent = ClassicAnchorEventAnnotation(
        candidate: possibleBurst,
        train: train
    )
    let aggregateEvent = try #require(
        possibleBurstEvent.projectedBurstComponent(
            to: 1...5,
            in: train,
            automaticEventSources:
                burstEvent.automaticEventSources
                + possibleBurstEvent.automaticEventSources,
            includesManualSupport: true
        )
    )

    let baselineRows = Dictionary(
        uniqueKeysWithValues: STPDResultPackageInput
            .automatic(dataset: fixture.dataset, run: fixture.run)
            .finalISILabelRows
            .filter { $0.trainID == train.id && (1...5).contains($0.isiIndex) }
            .map { ($0.isiIndex, $0) }
    )
    func authoritativeRow(
        _ index: Int,
        autoPattern: String,
        autoCandidateID: String,
        finalPattern: String,
        finalSource: String
    ) throws -> ReviewedISIExportRow {
        let row = try #require(baselineRows[index])
        return ReviewedISIExportRow(
            trainID: row.trainID,
            trainName: row.trainName,
            spikeIndex: row.spikeIndex,
            timestampSec: row.timestampSec,
            alignedTimestampSec: row.alignedTimestampSec,
            isiIndex: row.isiIndex,
            isiSec: row.isiSec,
            autoPattern: autoPattern,
            autoSubtype: "",
            autoCandidateID: autoCandidateID,
            finalPattern: finalPattern,
            finalSubtype: "",
            finalSource: finalSource,
            manualVetoSuppressed: false,
            reviewNote: ""
        )
    }
    let finalRows = try [
        authoritativeRow(
            1,
            autoPattern: ClassicAnchorLabel.burst.rawValue,
            autoCandidateID: burst.id,
            finalPattern: ClassicAnchorLabel.burst.rawValue,
            finalSource: ReviewedISIExportBuilder.sourceAutoProjected
        ),
        authoritativeRow(
            2,
            autoPattern: ClassicAnchorLabel.burst.rawValue,
            autoCandidateID: burst.id,
            finalPattern: ClassicAnchorLabel.burst.rawValue,
            finalSource: ReviewedISIExportBuilder.sourceAutoProjected
        ),
        authoritativeRow(
            3,
            autoPattern: "",
            autoCandidateID: "",
            finalPattern: ClassicAnchorLabel.burst.rawValue,
            finalSource: ReviewedISIExportBuilder.sourceManualPositive
        ),
        authoritativeRow(
            4,
            autoPattern: ClassicAnchorLabel.possibleBurst.rawValue,
            autoCandidateID: possibleBurst.id,
            finalPattern: ClassicAnchorLabel.possibleBurst.rawValue,
            finalSource: ReviewedISIExportBuilder.sourceAutoProjected
        ),
        authoritativeRow(
            5,
            autoPattern: ClassicAnchorLabel.possibleBurst.rawValue,
            autoCandidateID: possibleBurst.id,
            finalPattern: ClassicAnchorLabel.possibleBurst.rawValue,
            finalSource: ReviewedISIExportBuilder.sourceAutoProjected
        ),
    ]
    let records: [STPDEventRecord] = try STPDResultPackageBuilder.normalizedEventRecords(
        automaticEvents: [aggregateEvent],
        finalISIRows: finalRows,
        datasetDigest: "mixed-family-authority-test-digest",
        candidateUIDBySourceID: [
            burst.id: "cand_burst",
            possibleBurst.id: "cand_possible_burst",
        ],
        candidateBySourceID: [
            burst.id: burst,
            possibleBurst.id: possibleBurst,
        ],
        dataset: fixture.dataset,
        evidenceUIDsByISIKey: [:],
        changedISIKeys: [
            STPDResultPackageBuilder.authoritativeISIEvidenceKey(
                trainID: train.id,
                isiIndex: 3
            )
        ]
    )
    .sorted { $0.event.startISIIndex < $1.event.startISIIndex }

    #expect(records.count == 2)
    #expect(records.map { $0.event.finalLabel } == [
        ClassicAnchorLabel.burst.rawValue,
        ClassicAnchorLabel.possibleBurst.rawValue,
    ])
    #expect(records.map { [$0.event.startISIIndex, $0.event.endISIIndex] } == [
        [1, 3],
        [4, 5],
    ])
    #expect(records.allSatisfy {
        !$0.event.sourceEventIDs.isEmpty && !$0.sourceCandidateUIDs.isEmpty
    })
    #expect(records[0].sourceCandidateUIDs == ["cand_burst"])
    #expect(records[1].sourceCandidateUIDs == ["cand_possible_burst"])
    #expect(records[0].event.authorityOrigin == "manual_augmented")
    #expect(records[1].event.authorityOrigin == "automatic")
}

@Test
func resultPackageSingleSpikeManualAnnotationUsesNullableISIIndices() throws {
    let train = SpikeTrain(name: "single_spike_train", timestampsSec: [1.25])
    let dataset = SpikeDataset(
        name: "single-spike-result-package",
        sourceDescription: "single spike manual annotation fixture",
        trains: [train]
    )
    let run = ClassicAnchorDetectionPipeline.run(
        dataset: dataset,
        bandSettings: TrainAdaptiveBandSettings(minValidISISec: 0.001),
        buildCommit: resultPackageBuildCommit
    )
    let annotation = ManualAnnotation(
        id: UUID(uuidString: "24242424-4646-6868-8080-020202020202")!,
        trainID: train.id,
        label: .tonic,
        startSec: 1.25,
        endSec: 1.25,
        note: "valid spike-only slow-label annotation",
        createdAt: Date(timeIntervalSince1970: 100),
        updatedAt: Date(timeIntervalSince1970: 200)
    )
    let package = try STPDResultPackageBuilder.build(
        resultPackageReviewedInput(
            dataset: dataset,
            run: run,
            manualAnnotations: [annotation]
        )
    )
    let manual = try #require(package.table(.manualAnnotations))
    try #require(manual.rows.count == 1)
    let startISIIndex = try #require(
        manual.headers.firstIndex(of: "start_isi_index")
    )
    let endISIIndex = try #require(
        manual.headers.firstIndex(of: "end_isi_index")
    )
    let startSpikeIndex = try #require(
        manual.headers.firstIndex(of: "start_spike_array_index")
    )
    let endSpikeIndex = try #require(
        manual.headers.firstIndex(of: "end_spike_array_index")
    )
    #expect(manual.rows[0][startISIIndex].isEmpty)
    #expect(manual.rows[0][endISIIndex].isEmpty)
    #expect(manual.rows[0][startSpikeIndex] == "0")
    #expect(manual.rows[0][endSpikeIndex] == "0")

    let definitions = Dictionary(
        uniqueKeysWithValues: manual.columnDefinitions.map { ($0.name, $0) }
    )
    #expect(definitions["start_isi_index"]?.nullable == true)
    #expect(definitions["end_isi_index"]?.nullable == true)
}

@Test
func resultPackageSameLabelManualPositiveKeepsEventContinuousAndRowEvidenceLocal() throws {
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
    let evidenceIndex = try #require(
        events.headers.firstIndex(of: "review_evidence_uids")
    )
    let targetRows = events.rows
        .filter {
            $0[trainIndex] == train.id
                && $0[labelIndex] == ClassicAnchorLabel.tonic.rawValue
                && (Int($0[startIndex]) ?? 0) <= 5
        }
        .sorted { Int($0[startIndex])! < Int($1[startIndex])! }

    try #require(targetRows.count == 1)
    let event = targetRows[0]
    #expect(event[startIndex] == "1")
    #expect(event[endIndex] == "5")
    #expect(event[tonicSubtypeIndex] == "irregular")
    #expect(STPDCanonicalValue.parseStringList(event[hfsSubtypeIndex]) == [])
    #expect(event[auditSubtypeIndex] == "classic_tonic")
    #expect(event[auditStatusIndex] != "manual")
    #expect(event[decisionPathIndex] != "manual_positive_public_projection")
    #expect(event[authorityIndex] == "manual_augmented")
    #expect(STPDCanonicalValue.parseStringList(event[sourceCandidateIndex])?.count == 1)
    #expect(!(STPDCanonicalValue.parseStringList(event[supportIndex]) ?? []).isEmpty)
    #expect(STPDCanonicalValue.parseStringList(event[evidenceIndex])?.count == 1)

    let isiTable = try #require(package.table(.isiLabelsFinal))
    let isiTrainIndex = try #require(isiTable.headers.firstIndex(of: "train_id"))
    let isiIndex = try #require(isiTable.headers.firstIndex(of: "isi_index"))
    let finalSourceIndex = try #require(
        isiTable.headers.firstIndex(of: "final_source")
    )
    let finalSubtypeIndex = try #require(
        isiTable.headers.firstIndex(of: "final_subtype")
    )
    let isiEvidenceIndex = try #require(
        isiTable.headers.firstIndex(of: "review_evidence_uids")
    )
    let isiRows = isiTable.rows.filter {
        $0[isiTrainIndex] == train.id
            && (1...5).contains(Int($0[isiIndex]) ?? 0)
    }
    try #require(isiRows.count == 5)
    let manualISI = try #require(isiRows.first { $0[isiIndex] == "3" })
    #expect(manualISI[finalSourceIndex]
        == ReviewedISIExportBuilder.sourceManualPositive)
    #expect(manualISI[finalSubtypeIndex].isEmpty)
    #expect(STPDCanonicalValue.parseStringList(
        manualISI[isiEvidenceIndex]
    )?.count == 1)
    #expect(isiRows.filter { $0[isiIndex] != "3" }.allSatisfy {
        $0[finalSourceIndex] == ReviewedISIExportBuilder.sourceAutoProjected
            && (STPDCanonicalValue.parseStringList($0[isiEvidenceIndex]) ?? []).isEmpty
    })
}

@Test
func resultPackageManualOnlyAdjacentEventsMergeWithoutLosingISIEvidence() throws {
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

    try #require(targetRows.count == 1)
    let event = targetRows[0]
    #expect(event[startIndex] == "1")
    #expect(event[endIndex] == "2")
    #expect(STPDCanonicalValue.parseStringList(event[evidenceIndex])?.count == 2)

    let isiTable = try #require(package.table(.isiLabelsFinal))
    let isiTrainIndex = try #require(isiTable.headers.firstIndex(of: "train_id"))
    let isiIndex = try #require(isiTable.headers.firstIndex(of: "isi_index"))
    let finalSourceIndex = try #require(
        isiTable.headers.firstIndex(of: "final_source")
    )
    let isiEvidenceIndex = try #require(
        isiTable.headers.firstIndex(of: "review_evidence_uids")
    )
    let isiRows = isiTable.rows
        .filter {
            $0[isiTrainIndex] == train.id
                && ["1", "2"].contains($0[isiIndex])
        }
        .sorted { Int($0[isiIndex])! < Int($1[isiIndex])! }
    try #require(isiRows.count == 2)
    #expect(isiRows.allSatisfy {
        $0[finalSourceIndex] == ReviewedISIExportBuilder.sourceManualPositive
            && STPDCanonicalValue.parseStringList($0[isiEvidenceIndex])?.count == 1
    })
    #expect(isiRows[0][isiEvidenceIndex] != isiRows[1][isiEvidenceIndex])
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
func resultPackageEventUIDIncludesAuthoritativeFinalSubtype() throws {
    let fixture = resultPackageFixture()
    let train = fixture.dataset.trains[0]

    func package(
        candidateLayer: String,
        decisionPath: String
    ) throws -> STPDResultPackage {
        let candidate = resultPackageHFSCandidate(
            train: train,
            id: "transient-burst-\(candidateLayer)",
            candidateLayer: candidateLayer,
            nISI: 4,
            nSpikes: 5,
            candidateClass: ClassicAnchorLabel.burst.rawValue,
            finalLabel: .burst,
            decisionPath: decisionPath,
            action: "accept_burst",
            priority: 1_000,
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

    func target(
        _ package: STPDResultPackage
    ) throws -> (uid: String, subtype: String) {
        let table = try #require(package.table(.eventsFinal))
        let trainIndex = try #require(table.headers.firstIndex(of: "train_id"))
        let labelIndex = try #require(table.headers.firstIndex(of: "final_label"))
        let startIndex = try #require(table.headers.firstIndex(of: "start_isi_index"))
        let endIndex = try #require(table.headers.firstIndex(of: "end_isi_index"))
        let subtypeIndex = try #require(table.headers.firstIndex(of: "final_subtype"))
        let uidIndex = try #require(table.headers.firstIndex(of: "event_uid"))
        let row = try #require(table.rows.first {
            $0[trainIndex] == train.id &&
                $0[labelIndex] == ClassicAnchorLabel.burst.rawValue &&
                $0[startIndex] == "1" &&
                $0[endIndex] == "4"
        })
        return (row[uidIndex], row[subtypeIndex])
    }

    let classicA = try target(package(
        candidateLayer: "event_uid_subtype_fixture",
        decisionPath: "transient_diagnostic_a"
    ))
    let classicB = try target(package(
        candidateLayer: "event_uid_subtype_fixture",
        decisionPath: "different_transient_diagnostic_b"
    ))
    let episodeMerge = try target(package(
        candidateLayer: "event_grammar_burst_episode",
        decisionPath: "transient_diagnostic_a"
    ))

    #expect(classicA.subtype == "classic_burst")
    #expect(episodeMerge.subtype == "classic_burst_episode_merge")
    #expect(classicA.uid == classicB.uid)
    #expect(classicA.uid != episodeMerge.uid)
}

@Test
func resultPackageTaskEventsAreCompleteDeterministicAndTamperEvident() throws {
    let taskEvents = [
        TaskEvent(
            id: "stimulus:2",
            name: "Stimulus",
            timeSec: 1.25,
            column: "event_stimulus",
            eventIndex: 2,
            trialID: "trial:2",
            source: "fixture.csv"
        ),
        TaskEvent(
            id: "stimulus:1",
            name: "Stimulus",
            timeSec: 0.75,
            column: "event_stimulus",
            eventIndex: 1,
            trialID: "trial:1",
            source: "fixture.csv"
        ),
    ]
    let fixture = resultPackageFixture(taskEvents: taskEvents)
    let input = STPDResultPackageInput.automatic(
        dataset: fixture.dataset,
        run: fixture.run
    )
    let first = try STPDResultPackageBuilder.build(input)
    let second = try STPDResultPackageBuilder.build(input)
    let reversedFixture = resultPackageFixture(
        taskEvents: Array(taskEvents.reversed())
    )
    let reversed = try STPDResultPackageBuilder.build(
        .automatic(
            dataset: reversedFixture.dataset,
            run: reversedFixture.run
        )
    )
    let table = try #require(first.table(.taskEvents))
    let repeated = try #require(second.table(.taskEvents))
    let reversedTable = try #require(reversed.table(.taskEvents))
    let expectedISICount = fixture.dataset.trains.reduce(0) {
        $0 + max(0, $1.spikeCount - 1)
    }

    #expect(table.rowCount == 2)
    #expect(table.rows == repeated.rows)
    let runIDColumn = try #require(table.headers.firstIndex(of: "run_id"))
    #expect(table.headers == reversedTable.headers)
    #expect(table.rows.map { row in
        row.enumerated().filter { $0.offset != runIDColumn }.map(\.element)
    } == reversedTable.rows.map { row in
        row.enumerated().filter { $0.offset != runIDColumn }.map(\.element)
    })
    #expect(Set(try resultPackageColumn(table, "source_event_id"))
        == ["stimulus:1", "stimulus:2"])
    let taskEventUIDs = try resultPackageColumn(table, "task_event_uid")
    #expect(taskEventUIDs.allSatisfy { !$0.isEmpty })
    #expect(Set(taskEventUIDs).count == taskEvents.count)

    let mutations: [(column: String, value: String)] = [
        ("source_event_id", "forged-event"),
        ("event_time_sec", "0.751"),
        (
            "event_time_sec",
            STPDCanonicalValue.double((0.75 as Double).nextDown)
        ),
        (
            "event_time_sec",
            STPDCanonicalValue.double((0.75 as Double).nextUp)
        ),
    ]
    for mutation in mutations {
        var tables = first.tables
        tables[.taskEvents] = try resultPackageReplacingCell(
            table,
            column: mutation.column,
            with: mutation.value
        )
        #expect(throws: STPDResultPackageError.self) {
            _ = try STPDResultPackageValidator.validate(
                identity: first.identity,
                sourceMode: first.sourceMode,
                tables: tables,
                expectedISICount: expectedISICount,
                expectedTaskEvents: taskEvents
            )
        }
    }
}

@Test
func resultPackageTaskEventUIDExcludesSourceButTracksScientificFields() throws {
    let originalEvent = TaskEvent(
        id: "stimulus:1",
        name: "Stimulus",
        timeSec: 0.75,
        column: "event_stimulus",
        eventIndex: 1,
        trialID: "trial:1",
        source: "first/location.csv"
    )
    let relocatedEvent = TaskEvent(
        id: originalEvent.id,
        name: originalEvent.name,
        timeSec: originalEvent.timeSec,
        column: originalEvent.column,
        eventIndex: originalEvent.eventIndex,
        trialID: originalEvent.trialID,
        source: "second/location.csv"
    )
    let changedTimeEvent = TaskEvent(
        id: originalEvent.id,
        name: originalEvent.name,
        timeSec: 0.751,
        column: originalEvent.column,
        eventIndex: originalEvent.eventIndex,
        trialID: originalEvent.trialID,
        source: originalEvent.source
    )

    func package(_ event: TaskEvent) throws -> STPDResultPackage {
        let fixture = resultPackageFixture(taskEvents: [event])
        return try STPDResultPackageBuilder.build(
            .automatic(dataset: fixture.dataset, run: fixture.run)
        )
    }

    let original = try package(originalEvent)
    let relocated = try package(relocatedEvent)
    let changedTime = try package(changedTimeEvent)
    let originalTable = try #require(original.table(.taskEvents))
    let relocatedTable = try #require(relocated.table(.taskEvents))
    let changedTable = try #require(changedTime.table(.taskEvents))

    #expect(original.identity.datasetDigest
        == relocated.identity.datasetDigest)
    #expect(originalTable.value(row: 0, column: "task_event_uid")
        == relocatedTable.value(row: 0, column: "task_event_uid"))
    #expect(originalTable.value(row: 0, column: "source")
        != relocatedTable.value(row: 0, column: "source"))
    #expect(original.identity.datasetDigest
        != changedTime.identity.datasetDigest)
    #expect(originalTable.value(row: 0, column: "task_event_uid")
        != changedTable.value(row: 0, column: "task_event_uid"))

    let originalFixture = resultPackageFixture(taskEvents: [originalEvent])
    let relocatedDataset = SpikeDataset(
        name: originalFixture.dataset.name,
        sourceDescription: originalFixture.dataset.sourceDescription,
        trains: originalFixture.dataset.trains,
        taskEvents: [relocatedEvent]
    )
    #expect(throws: STPDResultPackageError.self) {
        _ = try STPDResultPackageBuilder.build(
            .automatic(
                dataset: relocatedDataset,
                run: originalFixture.run
            )
        )
    }
}

@Test
func resultPackageRejectsDuplicateTaskSourceIdentifiersAndAllowsSharedTrial() throws {
    let first = TaskEvent(
        id: "stimulus:1",
        name: "Stimulus",
        timeSec: 0.75,
        column: "event_stimulus",
        eventIndex: 1,
        trialID: "trial:1",
        source: "fixture.csv"
    )
    let duplicateSourceID = TaskEvent(
        id: first.id,
        name: "Stimulus",
        timeSec: 1.25,
        column: "event_stimulus",
        eventIndex: 2,
        trialID: "trial:2",
        source: "fixture.csv"
    )
    let sharedTrialEvent = TaskEvent(
        id: "reward:1",
        name: "Reward",
        timeSec: 1.25,
        column: "event_reward",
        eventIndex: 2,
        trialID: first.trialID,
        source: "fixture.csv"
    )

    let duplicateSourceFixture = resultPackageFixture(
        taskEvents: [first, duplicateSourceID]
    )
    #expect(throws: STPDResultPackageError.self) {
        _ = try STPDResultPackageBuilder.build(
            .automatic(
                dataset: duplicateSourceFixture.dataset,
                run: duplicateSourceFixture.run
            )
        )
    }

    let sharedTrialFixture = resultPackageFixture(
        taskEvents: [first, sharedTrialEvent]
    )
    let package = try STPDResultPackageBuilder.build(
        .automatic(
            dataset: sharedTrialFixture.dataset,
            run: sharedTrialFixture.run
        )
    )
    let taskEvents = try #require(package.table(.taskEvents))
    #expect(taskEvents.rowCount == 2)
    #expect(try resultPackageColumn(taskEvents, "trial_id")
        == [first.trialID, first.trialID])
    #expect(Set(try resultPackageColumn(taskEvents, "source_event_id"))
        == [first.id, sharedTrialEvent.id])
    #expect(Set(try resultPackageColumn(taskEvents, "task_event_uid")).count == 2)
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
    #expect(review.value(row: 0, column: "reviewed_run_id")
        == fixture.run.runIdentity.runID)

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
    let sourceCandidateISIRowCount = automatic.finalISILabelRows.filter {
        $0.isiIndex > 0 && $0.autoCandidateID == sourceCandidateID
    }.count
    #expect(sourceCandidateISIRowCount > 0)

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
    #expect(parsedLinkedISIUIDs.count == sourceCandidateISIRowCount)
    #expect(Set(parsedLinkedISIUIDs).count == sourceCandidateISIRowCount)
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
    #expect(needsReview.sourceMode == .automatic)
    #expect(needsReview.candidateReviews.count == 1)
    #expect(needsReview.reviewLinks.isEmpty)
    #expect(needsReview.candidateDiagnostics.count == 1)
    #expect(needsReview.candidateDiagnostics[0].status
        == STPDCandidateReviewStatus.needsReview.rawValue)
    #expect(needsReview.candidateDiagnostics[0].details
        .contains("reason=non_authoritative_review_status"))
    let needsReviewPackage = try STPDResultPackageBuilder.build(needsReview)
    #expect(needsReviewPackage.table(.reviewStatus)?.rowCount == 1)
    #expect(needsReviewPackage.table(.reviewStatus)?
        .value(row: 0, column: "link_scope") == "non_authoritative")

    let modifiedWithoutReplacement = try resultPackageReviewedInput(
        dataset: fixture.dataset,
        run: fixture.run,
        candidateReviews: [
            STPDCandidateReviewInput(
                sourceCandidateID: sourceCandidateID,
                status: .modified,
                reviewer: "Independent reviewer",
                reviewedAt: Date(timeIntervalSince1970: 300)
            ),
        ]
    )
    #expect(modifiedWithoutReplacement.sourceMode == .automatic)
    #expect(modifiedWithoutReplacement.candidateReviews.count == 1)
    #expect(modifiedWithoutReplacement.reviewLinks.isEmpty)
    #expect(modifiedWithoutReplacement.candidateDiagnostics.count == 1)
    #expect(modifiedWithoutReplacement.candidateDiagnostics[0].status
        == STPDCandidateReviewStatus.modified.rawValue)
    #expect(modifiedWithoutReplacement.candidateDiagnostics[0].details
        .contains("reason=no_causal_public_projection"))
    let modifiedPackage = try STPDResultPackageBuilder.build(
        modifiedWithoutReplacement
    )
    #expect(modifiedPackage.table(.reviewStatus)?.rowCount == 1)
    #expect(modifiedPackage.table(.reviewStatus)?
        .value(row: 0, column: "link_scope") == "non_authoritative")

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
func resultPackageSnapshotKeepsPassiveReviewStateDiagnosticOnly() throws {
    let fixture = resultPackageFixture()
    let automatic = STPDResultPackageInput.automatic(
        dataset: fixture.dataset,
        run: fixture.run
    )
    let publicCandidateIDs = Set(automatic.finalISILabelRows.compactMap {
        $0.isiIndex > 0 && !$0.autoCandidateID.isEmpty
            ? $0.autoCandidateID
            : nil
    })
    let publicCandidateID = try #require(publicCandidateIDs.first)
    let nonPublicCandidateID = try #require(
        fixture.run.candidates.first {
            !publicCandidateIDs.contains($0.id)
                && $0.trainID != "__dataset__"
        }?.id
    )
    let input = try STPDResultPackageInput.snapshot(
        dataset: fixture.dataset,
        run: fixture.run,
        candidateReviews: [
            STPDCandidateReviewInput(
                sourceCandidateID: publicCandidateID,
                status: .needsReview,
                reviewer: "Reviewer A"
            ),
            STPDCandidateReviewInput(
                sourceCandidateID: nonPublicCandidateID,
                status: .accepted,
                reviewer: "Reviewer B",
                reviewedAt: Date(timeIntervalSince1970: 200),
                reviewedRunID: fixture.run.runIdentity.runID
            ),
        ]
    )

    #expect(input.sourceMode == .automatic)
    #expect(input.candidateReviews.count == 2)
    #expect(input.reviewLinks.isEmpty)
    #expect(input.candidateDiagnostics.map(\.sourceCandidateID)
        == [publicCandidateID, nonPublicCandidateID].sorted())
    let diagnosticsByCandidate = Dictionary(uniqueKeysWithValues:
        input.candidateDiagnostics.map { ($0.sourceCandidateID, $0) })
    let publicDiagnostic = try #require(diagnosticsByCandidate[publicCandidateID])
    let nonPublicDiagnostic = try #require(
        diagnosticsByCandidate[nonPublicCandidateID]
    )
    #expect(publicDiagnostic.status
        == STPDCandidateReviewStatus.needsReview.rawValue)
    #expect(nonPublicDiagnostic.status
        == STPDCandidateReviewStatus.accepted.rawValue)
    #expect(publicDiagnostic.details
        .contains("reason=non_authoritative_review_status"))
    #expect(nonPublicDiagnostic.details
        .contains("reason=no_causal_public_projection"))

    let package = try STPDResultPackageBuilder.build(input)
    #expect(package.manifest.sourceMode == "automatic")
    let reviewStatus = try #require(package.table(.reviewStatus))
    #expect(reviewStatus.rowCount == 2)
    #expect(try resultPackageColumn(reviewStatus, "link_scope")
        .allSatisfy { $0 == "non_authoritative" })
    let diagnostics = try #require(package.table(.candidateDiagnosticAudit))
    #expect(try resultPackageColumn(diagnostics, "evidence_kind")
        .filter { $0 == "candidate_diagnostic" }.count == 2)

    for tableKind in [
        STPDResultTable.candidateLedger,
        .candidateFeatures,
        .finalDecisions,
    ] {
        let table = try #require(package.table(tableKind))
        let sourceCandidateIDs = try resultPackageColumn(
            table,
            "source_candidate_id"
        )
        #expect(!sourceCandidateIDs.contains(nonPublicCandidateID))
    }

    var diagnosticCandidateUIDs = Set<String>()
    for tableKind in [
        STPDResultTable.candidateLedgerDiagnostic,
        .candidateFeaturesDiagnostic,
        .finalDecisionsDiagnostic,
    ] {
        let table = try #require(package.table(tableKind))
        let sourceIndex = try #require(
            table.headers.firstIndex(of: "source_candidate_id")
        )
        let uidIndex = try #require(
            table.headers.firstIndex(of: "candidate_uid")
        )
        let rows = table.rows.filter { $0[sourceIndex] == nonPublicCandidateID }
        let row = try #require(rows.first)
        #expect(rows.count == 1)
        diagnosticCandidateUIDs.insert(row[uidIndex])
    }
    let diagnosticCandidateUID = try #require(diagnosticCandidateUIDs.first)
    #expect(diagnosticCandidateUIDs.count == 1)

    let auditSourceIndex = try #require(
        diagnostics.headers.firstIndex(of: "source_candidate_id")
    )
    let auditUIDIndex = try #require(
        diagnostics.headers.firstIndex(of: "candidate_uid")
    )
    let auditKindIndex = try #require(
        diagnostics.headers.firstIndex(of: "evidence_kind")
    )
    let terminalRows = diagnostics.rows.filter {
        $0[auditSourceIndex] == nonPublicCandidateID
            && $0[auditKindIndex] == "candidate_terminal"
    }
    #expect(terminalRows.count == 1)
    #expect(terminalRows.first?[auditUIDIndex] == diagnosticCandidateUID)

    let diagnosticFeatures = try #require(
        package.table(.candidateFeaturesDiagnostic)
    )
    let featureSourceIndex = try #require(
        diagnosticFeatures.headers.firstIndex(of: "source_candidate_id")
    )
    let featureRowIndex = try #require(
        diagnosticFeatures.rows.firstIndex {
            $0[featureSourceIndex] == nonPublicCandidateID
        }
    )
    var tamperedFeatureRows = diagnosticFeatures.rows
    tamperedFeatureRows.remove(at: featureRowIndex)
    var tamperedTables = package.tables
    tamperedTables[.candidateFeaturesDiagnostic] = try STPDResultTableData(
        contract: diagnosticFeatures.contract,
        headers: diagnosticFeatures.headers,
        columnDefinitions: diagnosticFeatures.columnDefinitions,
        rows: tamperedFeatureRows
    )
    let expectedISICount = fixture.dataset.trains.reduce(0) {
        $0 + max(0, $1.spikeCount - 1)
    }
    #expect(throws: STPDResultPackageError.self) {
        _ = try STPDResultPackageValidator.validate(
            identity: package.identity,
            sourceMode: package.sourceMode,
            tables: tamperedTables,
            expectedISICount: expectedISICount
        )
    }
}

@Test
func resultPackageSnapshotPromotesOnlyPublicAuthorityToReviewedProjection() throws {
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
    let input = try STPDResultPackageInput.snapshot(
        dataset: fixture.dataset,
        run: fixture.run,
        candidateReviews: [
            STPDCandidateReviewInput(
                sourceCandidateID: publicCandidateID,
                status: .accepted,
                reviewer: STPDResultPackageOwnership.ownerName,
                reviewedAt: Date(timeIntervalSince1970: 300),
                reviewedRunID: fixture.run.runIdentity.runID
            ),
        ]
    )

    #expect(input.sourceMode == .reviewed)
    #expect(input.candidateReviews.map(\.sourceCandidateID) == [publicCandidateID])
    #expect(!input.reviewLinks.isEmpty)
    #expect(input.finalEvents == automatic.finalEvents)
    #expect(input.finalISILabelRows == automatic.finalISILabelRows)

    let package = try STPDResultPackageBuilder.build(input)
    #expect(package.manifest.sourceMode == "reviewed")
    #expect(package.table(.reviewStatus)?.rowCount == 1)
}

@Test
func resultPackageSnapshotRejectedCandidateMatchesCausalReviewedProjection() throws {
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
    let review = STPDCandidateReviewInput(
        sourceCandidateID: publicCandidateID,
        status: .rejected,
        reviewer: STPDResultPackageOwnership.ownerName,
        reviewedAt: Date(timeIntervalSince1970: 300),
        reviewedRunID: fixture.run.runIdentity.runID
    )
    let snapshot = try STPDResultPackageInput.snapshot(
        dataset: fixture.dataset,
        run: fixture.run,
        candidateReviews: [review]
    )
    let expected = try STPDResultPackageInput.reviewed(
        dataset: fixture.dataset,
        run: fixture.run,
        candidateReviews: [review]
    )

    #expect(snapshot.sourceMode == .reviewed)
    #expect(snapshot.finalEvents == expected.finalEvents)
    #expect(snapshot.finalISILabelRows == expected.finalISILabelRows)
    #expect(snapshot.reviewLinks == expected.reviewLinks)
    #expect(snapshot.finalISILabelRows.contains {
        $0.autoCandidateID == publicCandidateID
            && $0.finalSource == ReviewedISIExportBuilder.sourceManualReviewRejected
            && $0.finalPattern.isEmpty
    })

    let package = try STPDResultPackageBuilder.build(snapshot)
    #expect(package.manifest.sourceMode == "reviewed")
    let reviewTable = try #require(package.table(.reviewStatus))
    #expect(reviewTable.rowCount == 1)
    let linkedISIUIDsCell = try #require(
        reviewTable.value(row: 0, column: "linked_isi_uids")
    )
    let linkedISIUIDs = try #require(
        STPDCanonicalValue.parseStringList(linkedISIUIDsCell)
    )
    let automaticByKey = Dictionary(
        uniqueKeysWithValues: automatic.finalISILabelRows
            .filter { $0.isiIndex > 0 }
            .map { ("\($0.trainID):\($0.isiIndex)", $0) }
    )
    let changedKeys = Set(snapshot.finalISILabelRows.compactMap { row -> String? in
        guard row.isiIndex > 0,
              let baseline = automaticByKey["\(row.trainID):\(row.isiIndex)"],
              STPDResultPackageBuilder.isiProjectionComponents(row)
                != STPDResultPackageBuilder.isiProjectionComponents(baseline) else {
            return nil
        }
        return "\(row.trainID):\(row.isiIndex)"
    })
    let finalISITable = try #require(package.table(.isiLabelsFinal))
    let expectedLinkedUIDs = Set(finalISITable.rows.indices.compactMap {
        rowIndex -> String? in
        guard let trainID = finalISITable.value(
                  row: rowIndex,
                  column: "train_id"
              ),
              let isiIndex = finalISITable.value(
                  row: rowIndex,
                  column: "isi_index"
              ),
              changedKeys.contains("\(trainID):\(isiIndex)") else {
            return nil
        }
        return finalISITable.value(row: rowIndex, column: "isi_uid")
    })
    #expect(!expectedLinkedUIDs.isEmpty)
    #expect(Set(linkedISIUIDs) == expectedLinkedUIDs)
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
func resultPackageSourceModesRetainPassiveRecordsAndFailClosed() throws {
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

    let passiveAnnotationPackage = try STPDResultPackageBuilder.build(
        STPDResultPackageInput(
            dataset: fixture.dataset,
            run: fixture.run,
            sourceMode: .automatic,
            finalEvents: automatic.finalEvents,
            finalISILabelRows: automatic.finalISILabelRows,
            manualAnnotations: [annotation]
        )
    )
    #expect(passiveAnnotationPackage.table(.manualAnnotations)?.rowCount == 1)
    #expect(passiveAnnotationPackage.table(.manualAnnotations)?
        .value(row: 0, column: "link_scope") == "inactive_or_superseded")

    let passiveReviewPackage = try STPDResultPackageBuilder.build(
        STPDResultPackageInput(
            dataset: fixture.dataset,
            run: fixture.run,
            sourceMode: .automatic,
            finalEvents: automatic.finalEvents,
            finalISILabelRows: automatic.finalISILabelRows,
            candidateReviews: [
                STPDCandidateReviewInput(
                    sourceCandidateID: sourceCandidate.id,
                    status: .accepted,
                    reviewer: "Independent reviewer",
                    reviewedAt: Date(timeIntervalSince1970: 300),
                    reviewedRunID: fixture.run.runIdentity.runID
                )
            ]
        )
    )
    #expect(passiveReviewPackage.table(.reviewStatus)?.rowCount == 1)
    #expect(passiveReviewPackage.table(.reviewStatus)?
        .value(row: 0, column: "link_scope") == "non_authoritative")
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
func resultPackageOverlappingPositiveAnnotationsLinkOnlyTheEffectiveOwner() throws {
    let fixture = resultPackageFixture()
    let olderID = UUID(uuidString: "10000000-0000-0000-0000-000000000001")!
    let newerID = UUID(uuidString: "20000000-0000-0000-0000-000000000002")!
    let geometry = try resultPackageManualTonicAnnotation(
        dataset: fixture.dataset,
        run: fixture.run,
        id: olderID
    )
    let older = ManualAnnotation(
        id: olderID,
        trainID: geometry.trainID,
        label: .tonic,
        startSec: geometry.startSec,
        endSec: geometry.endSec,
        createdAt: Date(timeIntervalSince1970: 10),
        updatedAt: Date(timeIntervalSince1970: 20)
    )
    let newer = ManualAnnotation(
        id: newerID,
        trainID: geometry.trainID,
        label: .tonic,
        startSec: geometry.startSec,
        endSec: geometry.endSec,
        createdAt: Date(timeIntervalSince1970: 30),
        updatedAt: Date(timeIntervalSince1970: 40)
    )
    let input = try resultPackageReviewedInput(
        dataset: fixture.dataset,
        run: fixture.run,
        manualAnnotations: [newer, older]
    )
    let package = try STPDResultPackageBuilder.build(input)
    let table = try #require(package.table(.manualAnnotations))
    let uuidIndex = try #require(
        table.headers.firstIndex(of: "source_annotation_uuid")
    )
    let linksIndex = try #require(table.headers.firstIndex(of: "linked_isi_uids"))
    let scopeIndex = try #require(table.headers.firstIndex(of: "link_scope"))
    let rowsByUUID = Dictionary(uniqueKeysWithValues:
        table.rows.map { ($0[uuidIndex], $0) })
    let olderRow = try #require(rowsByUUID[olderID.uuidString.lowercased()])
    let newerRow = try #require(rowsByUUID[newerID.uuidString.lowercased()])

    #expect(STPDCanonicalValue.parseStringList(olderRow[linksIndex]) == [])
    #expect(olderRow[scopeIndex] == "inactive_or_superseded")
    #expect(STPDCanonicalValue.parseStringList(newerRow[linksIndex])?.count == 1)
    #expect(newerRow[scopeIndex] == "public_projection")
}

@Test
func resultPackageOverlappingNegativeVetoesLinkOnlyTheEffectiveOwner() throws {
    let fixture = resultPackageFixture()
    let automatic = STPDResultPackageInput.automatic(
        dataset: fixture.dataset,
        run: fixture.run
    )
    let row = try #require(automatic.finalISILabelRows.first {
        $0.isiIndex > 0
            && ManualAnnotationProjector.burstFamilyLabels.contains($0.autoPattern)
    })
    let train = try #require(
        fixture.dataset.trains.first { $0.id == row.trainID }
    )
    let olderID = UUID(uuidString: "30000000-0000-0000-0000-000000000003")!
    let newerID = UUID(uuidString: "40000000-0000-0000-0000-000000000004")!
    let older = ManualAnnotation(
        id: olderID,
        trainID: train.id,
        label: .notBurst,
        startSec: train.timestampsSec[row.isiIndex - 1],
        endSec: train.timestampsSec[row.isiIndex],
        createdAt: Date(timeIntervalSince1970: 10),
        updatedAt: Date(timeIntervalSince1970: 20)
    )
    let newer = ManualAnnotation(
        id: newerID,
        trainID: train.id,
        label: .notBurst,
        startSec: train.timestampsSec[row.isiIndex - 1],
        endSec: train.timestampsSec[row.isiIndex],
        createdAt: Date(timeIntervalSince1970: 30),
        updatedAt: Date(timeIntervalSince1970: 40)
    )
    let input = try resultPackageReviewedInput(
        dataset: fixture.dataset,
        run: fixture.run,
        manualAnnotations: [newer, older]
    )
    let package = try STPDResultPackageBuilder.build(input)
    let table = try #require(package.table(.manualAnnotations))
    let uuidIndex = try #require(
        table.headers.firstIndex(of: "source_annotation_uuid")
    )
    let linksIndex = try #require(table.headers.firstIndex(of: "linked_isi_uids"))
    let scopeIndex = try #require(table.headers.firstIndex(of: "link_scope"))
    let rowsByUUID = Dictionary(uniqueKeysWithValues:
        table.rows.map { ($0[uuidIndex], $0) })
    let olderRow = try #require(rowsByUUID[olderID.uuidString.lowercased()])
    let newerRow = try #require(rowsByUUID[newerID.uuidString.lowercased()])

    #expect(STPDCanonicalValue.parseStringList(olderRow[linksIndex]) == [])
    #expect(olderRow[scopeIndex] == "inactive_or_superseded")
    #expect(STPDCanonicalValue.parseStringList(newerRow[linksIndex])?.count == 1)
    #expect(newerRow[scopeIndex] == "public_projection")
}

@Test
func resultPackageSnapshotRetainsInactiveManualAnnotationWithoutAuthority() throws {
    let fixture = resultPackageFixture()
    let automatic = STPDResultPackageInput.automatic(
        dataset: fixture.dataset,
        run: fixture.run
    )
    let automaticRowsByTrainAndISI = Dictionary(
        uniqueKeysWithValues: automatic.finalISILabelRows
            .filter { $0.isiIndex > 0 }
            .map { ("\($0.trainID):\($0.isiIndex)", $0) }
    )
    let row = try #require(automatic.finalISILabelRows.first {
        guard $0.isiIndex > 0,
              !ManualAnnotationProjector.burstFamilyLabels.contains($0.autoPattern) else {
            return false
        }
        let previous = automaticRowsByTrainAndISI["\($0.trainID):\($0.isiIndex - 1)"]
        let next = automaticRowsByTrainAndISI["\($0.trainID):\($0.isiIndex + 1)"]
        return [previous, next].allSatisfy {
            guard let neighbor = $0 else { return true }
            return !ManualAnnotationProjector.burstFamilyLabels.contains(neighbor.autoPattern)
        }
    })
    let train = try #require(
        fixture.dataset.trains.first { $0.id == row.trainID }
    )
    let annotation = ManualAnnotation(
        id: UUID(uuidString: "70000000-0000-0000-0000-000000000007")!,
        trainID: train.id,
        label: .burst,
        startSec: train.timestampsSec[row.isiIndex - 1],
        endSec: train.timestampsSec[row.isiIndex],
        createdAt: Date(timeIntervalSince1970: 10),
        updatedAt: Date(timeIntervalSince1970: 20)
    )

    let input = try STPDResultPackageInput.snapshot(
        dataset: fixture.dataset,
        run: fixture.run,
        manualAnnotations: [annotation]
    )
    #expect(input.sourceMode == .automatic)
    #expect(input.manualAnnotations.count == 1)
    #expect(input.reviewLinks.isEmpty)

    let package = try STPDResultPackageBuilder.build(input)
    let table = try #require(package.table(.manualAnnotations))
    #expect(table.rowCount == 1)
    #expect(table.value(row: 0, column: "link_scope")
        == "inactive_or_superseded")
    #expect(STPDCanonicalValue.parseStringList(
        try #require(table.value(row: 0, column: "linked_isi_uids"))
    ) == [])
}

@Test
func resultPackageRejectsDuplicateCandidateReviewsBeforeAuthorityPartition() throws {
    let fixture = resultPackageFixture()
    let candidateID = try #require(
        fixture.run.candidates.first { $0.trainID != "__dataset__" }?.id
    )

    #expect(throws: STPDResultPackageError.self) {
        _ = try STPDResultPackageInput.snapshot(
            dataset: fixture.dataset,
            run: fixture.run,
            candidateReviews: [
                STPDCandidateReviewInput(
                    sourceCandidateID: candidateID,
                    status: .needsReview,
                    reviewer: "Reviewer A"
                ),
                STPDCandidateReviewInput(
                    sourceCandidateID: candidateID,
                    status: .accepted,
                    reviewer: "Reviewer B"
                ),
            ]
        )
    }
}

@Test
func resultPackageRejectsOverlappingManualEditsWithEqualTimestamps() throws {
    let fixture = resultPackageFixture()
    let geometry = try resultPackageManualTonicAnnotation(
        dataset: fixture.dataset,
        run: fixture.run,
        id: UUID(uuidString: "80000000-0000-0000-0000-000000000008")!
    )
    let createdAt = Date(timeIntervalSince1970: 10)
    let updatedAt = Date(timeIntervalSince1970: 20)
    let annotations = [
        ManualAnnotation(
            id: UUID(uuidString: "80000000-0000-0000-0000-000000000008")!,
            trainID: geometry.trainID,
            label: .tonic,
            startSec: geometry.startSec,
            endSec: geometry.endSec,
            createdAt: createdAt,
            updatedAt: updatedAt
        ),
        ManualAnnotation(
            id: UUID(uuidString: "90000000-0000-0000-0000-000000000009")!,
            trainID: geometry.trainID,
            label: .pause,
            startSec: geometry.startSec,
            endSec: geometry.endSec,
            createdAt: createdAt,
            updatedAt: updatedAt
        ),
    ]

    #expect(throws: STPDResultPackageError.self) {
        _ = try STPDResultPackageInput.snapshot(
            dataset: fixture.dataset,
            run: fixture.run,
            manualAnnotations: annotations
        )
    }
}

@Test
func resultPackageManualEvidenceOwnsStructurallyInducedBurstRemoval() throws {
    let fixture = resultPackageFixture()
    let automatic = STPDResultPackageInput.automatic(
        dataset: fixture.dataset,
        run: fixture.run
    )
    let burstRowsByTrain = Dictionary(
        grouping: automatic.finalISILabelRows.filter {
            $0.isiIndex > 0
                && ManualAnnotationProjector.burstFamilyLabels
                    .contains($0.autoPattern)
        },
        by: \.trainID
    )
    var selected: (trainID: String, firstISI: Int, vetoISI: Int)?
    for trainID in burstRowsByTrain.keys.sorted() {
        let indices = Set((burstRowsByTrain[trainID] ?? []).map(\.isiIndex))
        if let first = indices.sorted().first(where: {
            !indices.contains($0 - 1)
                && indices.contains($0 + 1)
                && indices.contains($0 + 2)
        }) {
            selected = (trainID, first, first + 1)
            break
        }
    }
    let target = try #require(selected)
    let train = try #require(
        fixture.dataset.trains.first { $0.id == target.trainID }
    )
    let annotation = ManualAnnotation(
        id: UUID(uuidString: "a0000000-0000-0000-0000-00000000000a")!,
        trainID: train.id,
        label: .notBurst,
        startSec: train.timestampsSec[target.vetoISI - 1],
        endSec: train.timestampsSec[target.vetoISI],
        annotator: "Result Package Test Reviewer",
        annotatorIdentitySource: .userProvided,
        createdAt: Date(timeIntervalSince1970: 10),
        updatedAt: Date(timeIntervalSince1970: 20)
    )

    let input = try STPDResultPackageInput.snapshot(
        dataset: fixture.dataset,
        run: fixture.run,
        manualAnnotations: [annotation]
    )
    let package = try STPDResultPackageBuilder.build(input)
    let isiTable = try #require(package.table(.isiLabelsFinal))
    let manualTable = try #require(package.table(.manualAnnotations))
    let linkedCell = try #require(
        manualTable.value(row: 0, column: "linked_isi_uids")
    )
    let linkedUIDs = Set(try #require(
        STPDCanonicalValue.parseStringList(linkedCell)
    ))
    let expectedUIDs = Set(isiTable.rows.indices.compactMap { rowIndex -> String? in
        guard isiTable.value(row: rowIndex, column: "train_id") == target.trainID,
              let indexText = isiTable.value(
                  row: rowIndex,
                  column: "isi_index"
              ),
              let isiIndex = Int(indexText),
              [target.firstISI, target.vetoISI].contains(isiIndex) else {
            return nil
        }
        return isiTable.value(row: rowIndex, column: "isi_uid")
    })

    #expect(expectedUIDs.count == 2)
    #expect(expectedUIDs.isSubset(of: linkedUIDs))
}

@Test
func resultPackageAcceptedReviewDoesNotClaimManuallyOverriddenISI() throws {
    let fixture = resultPackageFixture()
    let automatic = STPDResultPackageInput.automatic(
        dataset: fixture.dataset,
        run: fixture.run
    )
    let groupedRows = Dictionary(grouping: automatic.finalISILabelRows.filter {
        $0.isiIndex > 0 && !$0.autoCandidateID.isEmpty
    }, by: \.autoCandidateID)
    let candidateID = try #require(groupedRows.keys.sorted().first { candidateID in
        guard let rows = groupedRows[candidateID] else { return false }
        return rows.count >= 2
            && rows.contains {
                $0.autoPattern != ManualAnnotationLabel.tonic.rawValue
            }
    })
    let candidateRows = try #require(groupedRows[candidateID])
    let overriddenRow = try #require(candidateRows.first {
        $0.autoPattern != ManualAnnotationLabel.tonic.rawValue
    })
    let train = try #require(
        fixture.dataset.trains.first { $0.id == overriddenRow.trainID }
    )
    let annotation = ManualAnnotation(
        id: UUID(uuidString: "50000000-0000-0000-0000-000000000005")!,
        trainID: train.id,
        label: .tonic,
        startSec: train.timestampsSec[overriddenRow.isiIndex - 1],
        endSec: train.timestampsSec[overriddenRow.isiIndex],
        createdAt: Date(timeIntervalSince1970: 10),
        updatedAt: Date(timeIntervalSince1970: 20)
    )
    let input = try resultPackageReviewedInput(
        dataset: fixture.dataset,
        run: fixture.run,
        manualAnnotations: [annotation],
        candidateReviews: [
            STPDCandidateReviewInput(
                sourceCandidateID: candidateID,
                status: .accepted,
                reviewer: "Independent reviewer",
                reviewedAt: Date(timeIntervalSince1970: 300)
            ),
        ]
    )
    let package = try STPDResultPackageBuilder.build(input)
    let manualTable = try #require(package.table(.manualAnnotations))
    let reviewTable = try #require(package.table(.reviewStatus))
    let manualLinksCell = try #require(
        manualTable.value(row: 0, column: "linked_isi_uids")
    )
    let reviewLinksCell = try #require(
        reviewTable.value(row: 0, column: "linked_isi_uids")
    )
    let manualLinks = try #require(
        STPDCanonicalValue.parseStringList(manualLinksCell)
    )
    let reviewLinks = try #require(
        STPDCanonicalValue.parseStringList(reviewLinksCell)
    )

    #expect(manualLinks.count == 1)
    #expect(reviewLinks.isEmpty)
    #expect(reviewTable.value(row: 0, column: "link_scope")
        == "non_authoritative")
    #expect(input.candidateDiagnostics.contains {
        $0.sourceCandidateID == candidateID
            && $0.details.contains("reason=no_causal_public_projection")
    })
}

@Test
func resultPackageFullyOverriddenAcceptedReviewBecomesDiagnostic() throws {
    let fixture = resultPackageFixture()
    let automatic = STPDResultPackageInput.automatic(
        dataset: fixture.dataset,
        run: fixture.run
    )
    let groupedRows = Dictionary(grouping: automatic.finalISILabelRows.filter {
        $0.isiIndex > 0 && !$0.autoCandidateID.isEmpty
    }, by: \.autoCandidateID)
    let candidateID = try #require(groupedRows.keys.sorted().first { candidateID in
        guard let rows = groupedRows[candidateID], rows.count >= 2,
              Set(rows.map(\.trainID)).count == 1,
              rows.allSatisfy({
                  $0.autoPattern != ManualAnnotationLabel.tonic.rawValue
              }) else {
            return false
        }
        let indices = rows.map(\.isiIndex).sorted()
        return zip(indices, indices.dropFirst()).allSatisfy {
            $0.1 == $0.0 + 1
        }
    })
    let candidateRows = try #require(groupedRows[candidateID]?.sorted {
        $0.isiIndex < $1.isiIndex
    })
    let firstRow = try #require(candidateRows.first)
    let lastRow = try #require(candidateRows.last)
    let train = try #require(
        fixture.dataset.trains.first { $0.id == firstRow.trainID }
    )
    let annotation = ManualAnnotation(
        id: UUID(uuidString: "60000000-0000-0000-0000-000000000006")!,
        trainID: train.id,
        label: .tonic,
        startSec: train.timestampsSec[firstRow.isiIndex - 1],
        endSec: train.timestampsSec[lastRow.isiIndex],
        createdAt: Date(timeIntervalSince1970: 10),
        updatedAt: Date(timeIntervalSince1970: 20)
    )
    let input = try resultPackageReviewedInput(
        dataset: fixture.dataset,
        run: fixture.run,
        manualAnnotations: [annotation],
        candidateReviews: [
            STPDCandidateReviewInput(
                sourceCandidateID: candidateID,
                status: .accepted,
                reviewer: "Independent reviewer",
                reviewedAt: Date(timeIntervalSince1970: 30)
            ),
        ]
    )

    #expect(input.sourceMode == .manual)
    #expect(input.candidateReviews.count == 1)
    #expect(input.candidateDiagnostics.contains {
        $0.sourceCandidateID == candidateID
            && $0.status == STPDCandidateReviewStatus.accepted.rawValue
            && $0.details.contains("reason=no_causal_public_projection")
    })
    let package = try STPDResultPackageBuilder.build(input)
    #expect(package.manifest.sourceMode == "manual")
    #expect(package.table(.reviewStatus)?.rowCount == 1)
    #expect(package.table(.reviewStatus)?
        .value(row: 0, column: "link_scope") == "non_authoritative")
    #expect(package.table(.manualAnnotations)?.rowCount == 1)
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
        ManualAnnotation(
            trainID: fixture.dataset.trains[0].id,
            label: .tonic,
            startSec: 0.1,
            endSec: 0.2,
            createdAt: Date(timeIntervalSince1970: .nan)
        ),
        ManualAnnotation(
            trainID: fixture.dataset.trains[0].id,
            label: .tonic,
            startSec: 0.1,
            endSec: 0.2,
            updatedAt: Date(timeIntervalSince1970: .infinity)
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
    let diagnostics = try #require(package.table(.candidateDiagnosticAudit))
    let diagnosticSourceIDs = try resultPackageColumn(
        diagnostics,
        "source_candidate_id"
    )
    let diagnosticCandidateUIDs = try resultPackageColumn(
        diagnostics,
        "candidate_uid"
    )
    let diagnosticStages = try resultPackageColumn(diagnostics, "stage_name")
    let terminalIndices = diagnosticStages.indices.filter {
        diagnosticStages[$0] == "terminal_decision"
    }
    let targetIndex = try #require(terminalIndices.first {
        diagnosticSourceIDs[$0] == targetID
    })
    let suppressorIndex = try #require(terminalIndices.first {
        diagnosticSourceIDs[$0] == suppressorID
    })
    #expect(!diagnosticCandidateUIDs[targetIndex].isEmpty)
    #expect(!diagnosticCandidateUIDs[suppressorIndex].isEmpty)
    #expect(diagnosticCandidateUIDs[targetIndex]
        != diagnosticCandidateUIDs[suppressorIndex])

    let features = try #require(package.table(.candidateFeatures))
    let publicSourceIDs = try resultPackageColumn(features, "source_candidate_id")
    #expect(!publicSourceIDs.contains(targetID))

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
        let diagnostics = try #require(
            package.table(.candidateDiagnosticAudit)
        )
        let sourceIDs = try resultPackageColumn(
            diagnostics,
            "source_candidate_id"
        )
        let candidateUIDs = try resultPackageColumn(
            diagnostics,
            "candidate_uid"
        )
        let stages = try resultPackageColumn(diagnostics, "stage_name")
        let targetIndex = try #require(sourceIDs.indices.first {
            sourceIDs[$0] == targetID && stages[$0] == "terminal_decision"
        })
        let suppressorIndex = try #require(sourceIDs.indices.first {
            sourceIDs[$0] == suppressorID && stages[$0] == "terminal_decision"
        })
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
                note: "review, \"quoted\"\nline",
                reviewedAt: Date(timeIntervalSince1970: 300)
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
    try FileManager.default.createDirectory(
        at: root,
        withIntermediateDirectories: false
    )

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
func resultPackageWriterLegacyFileManagerEntryPointRemainsSourceCompatible()
    throws {
    let fixture = resultPackageFixture()
    let package = try STPDResultPackageBuilder.build(
        .automatic(dataset: fixture.dataset, run: fixture.run)
    )
    let root = FileManager.default.temporaryDirectory.appendingPathComponent(
        "stpd-result-writer-legacy-\(UUID().uuidString.lowercased())",
        isDirectory: true
    )
    let destination = root.appendingPathComponent("result", isDirectory: true)
    defer { try? FileManager.default.removeItem(at: root) }
    try FileManager.default.createDirectory(
        at: root,
        withIntermediateDirectories: false
    )

    try STPDResultPackageWriter.write(
        package,
        to: destination,
        fileManager: .default
    )
    #expect(
        try STPDResultPackageReader.read(packageAt: destination).verified
    )
}

@Test
func resultPackageCurrentAndLegacyEntryPointsRemainUnambiguousFunctionValues()
    throws {
    let fixture = resultPackageFixture()
    let package = try STPDResultPackageBuilder.build(
        .automatic(dataset: fixture.dataset, run: fixture.run)
    )
    let root = FileManager.default.temporaryDirectory.appendingPathComponent(
        "stpd-result-current-function-value-\(UUID().uuidString.lowercased())",
        isDirectory: true
    )
    let destination = root.appendingPathComponent(
        "result",
        isDirectory: true
    )
    defer { try? FileManager.default.removeItem(at: root) }
    try FileManager.default.createDirectory(
        at: root,
        withIntermediateDirectories: false
    )

    try resultPackageCurrentWriterEntryPoint(
        package,
        destination,
        .privateCurrentUserOnly
    )
    let result = try resultPackageCurrentReaderEntryPoint(
        destination,
        .standard
    )
    #expect(result.verified)

    let legacyDestination = root.appendingPathComponent(
        "legacy-result",
        isDirectory: true
    )
    try resultPackageLegacyWriterEntryPoint(
        package,
        legacyDestination,
        FileManager.default
    )
    let legacyResult = try resultPackageLegacyReaderEntryPoint(
        legacyDestination,
        FileManager.default
    )
    #expect(legacyResult.verified)
}

@Test
func resultPackageWriterPublicationFlagsRemainAvailableBeforeMacOS26() {
    let baseline = UInt32(RENAME_EXCL | RENAME_NOFOLLOW_ANY)
    let resolveBeneath = UInt32(0x20)

    #expect(
        STPDResultPackageWriter.publicationRenameFlagsForTesting(
            resolveBeneathAvailable: false
        ) == baseline
    )
    #expect(
        STPDResultPackageWriter.publicationRenameFlagsForTesting(
            resolveBeneathAvailable: true
        ) == baseline | resolveBeneath
    )
}

@Test
func resultPackageWriterRequiresExplicitPolicyForSharedParents() throws {
    let fixture = resultPackageFixture()
    let package = try STPDResultPackageBuilder.build(
        .automatic(dataset: fixture.dataset, run: fixture.run)
    )
    let modes: [mode_t] = [
        mode_t(0o770),
        mode_t(S_ISVTX) | mode_t(0o777),
    ]

    for mode in modes {
        let root = FileManager.default.temporaryDirectory.appendingPathComponent(
            "stpd-result-shared-parent-\(UUID().uuidString.lowercased())",
            isDirectory: true
        )
        let destination = root.appendingPathComponent("result", isDirectory: true)
        defer {
            _ = chmod(root.path, mode_t(0o700))
            try? FileManager.default.removeItem(at: root)
        }
        try FileManager.default.createDirectory(
            at: root,
            withIntermediateDirectories: false
        )
        try #require(chmod(root.path, mode) == 0)

        #expect(throws: STPDResultPackageError.self) {
            try STPDResultPackageWriter.write(package, to: destination)
        }
        #expect(!FileManager.default.fileExists(atPath: destination.path))

        try STPDResultPackageWriter.write(
            package,
            to: destination,
            parentDirectoryPolicy: .allowShared
        )
        var metadata = stat()
        try #require(lstat(destination.path, &metadata) == 0)
        #expect(metadata.st_mode & mode_t(0o777) == mode_t(0o700))
        let readback = try STPDResultPackageReader.read(packageAt: destination)
        #expect(readback.verified)
        #expect(readback.runID == package.manifest.runID)
    }
}

@Test
func resultPackageWriterRequiresExplicitSharedPolicyForExtendedACLParent()
    throws {
    let fixture = resultPackageFixture()
    let package = try STPDResultPackageBuilder.build(
        .automatic(dataset: fixture.dataset, run: fixture.run)
    )
    let root = FileManager.default.temporaryDirectory.appendingPathComponent(
        "stpd-result-acl-parent-\(UUID().uuidString.lowercased())",
        isDirectory: true
    )
    let destination = root.appendingPathComponent(
        "result",
        isDirectory: true
    )
    defer {
        try? FileManager.default.removeItem(at: root)
    }
    try FileManager.default.createDirectory(
        at: root,
        withIntermediateDirectories: false
    )
    try #require(chmod(root.path, mode_t(0o700)) == 0)
    try installResultPackageTestExtendedACL(at: root)

    do {
        try STPDResultPackageWriter.write(
            package,
            to: destination
        )
        Issue.record(
            "Expected privateCurrentUserOnly to reject a parent with an extended ACL"
        )
    } catch let error as STPDResultPackageError {
        guard case .invalidInput(let reason) = error else {
            Issue.record("Expected invalidInput, got \(error)")
            return
        }
        #expect(reason.contains("extended ACL"))
    }
    #expect(!FileManager.default.fileExists(atPath: destination.path))

    try STPDResultPackageWriter.write(
        package,
        to: destination,
        parentDirectoryPolicy: .allowShared
    )
    var metadata = stat()
    try #require(lstat(destination.path, &metadata) == 0)
    #expect(metadata.st_mode & mode_t(0o777) == mode_t(0o700))
    let readback = try STPDResultPackageReader.read(
        packageAt: destination
    )
    #expect(readback.verified)
    #expect(readback.runID == package.manifest.runID)
}

@Test
func resultPackageWriterPrivatePolicyPermitsDenyOnlyParentACL() throws {
    let fixture = resultPackageFixture()
    let package = try STPDResultPackageBuilder.build(
        .automatic(dataset: fixture.dataset, run: fixture.run)
    )
    let root = FileManager.default.temporaryDirectory.appendingPathComponent(
        "stpd-result-deny-acl-parent-\(UUID().uuidString.lowercased())",
        isDirectory: true
    )
    let destination = root.appendingPathComponent("result", isDirectory: true)
    defer { try? FileManager.default.removeItem(at: root) }
    try FileManager.default.createDirectory(
        at: root,
        withIntermediateDirectories: false
    )
    try #require(chmod(root.path, mode_t(0o700)) == 0)
    try installResultPackageTestExtendedACL(
        at: root,
        kind: .denyDelete
    )

    try STPDResultPackageWriter.write(package, to: destination)
    #expect(
        try STPDResultPackageReader.read(packageAt: destination).verified
    )
}

@Test
func resultPackageWriterSharedPolicyStripsInheritedAllowACLFromPackageEntries()
    throws {
    let fixture = resultPackageFixture()
    let package = try STPDResultPackageBuilder.build(
        .automatic(dataset: fixture.dataset, run: fixture.run)
    )
    let root = FileManager.default.temporaryDirectory.appendingPathComponent(
        "stpd-result-inherited-acl-parent-\(UUID().uuidString.lowercased())",
        isDirectory: true
    )
    let destination = root.appendingPathComponent("result", isDirectory: true)
    defer { try? FileManager.default.removeItem(at: root) }
    try FileManager.default.createDirectory(
        at: root,
        withIntermediateDirectories: false
    )
    try #require(chmod(root.path, mode_t(0o700)) == 0)
    try installResultPackageTestExtendedACL(
        at: root,
        kind: .allowWrite(inheritable: true)
    )

    try STPDResultPackageWriter.write(
        package,
        to: destination,
        parentDirectoryPolicy: .allowShared
    )
    #expect(try !resultPackageEntryHasExtendedACL(at: destination))
    #expect(
        try !resultPackageEntryHasExtendedACL(
            at: destination.appendingPathComponent(
                STPDResultTable.runMetadata.rawValue
            )
        )
    )
    #expect(
        try STPDResultPackageReader.read(packageAt: destination).verified
    )
}

@Test
func resultPackageAllNineteenTablesUseOneOrderedColumnAuthority() throws {
    let fixture = resultPackageFixture()
    let package = try STPDResultPackageBuilder.build(
        .automatic(dataset: fixture.dataset, run: fixture.run)
    )
    let contractByTable = Dictionary(
        uniqueKeysWithValues: STPDResultSchema.tables.map { ($0.table, $0) }
    )
    let manifestByName = Dictionary(
        uniqueKeysWithValues: package.manifest.tables.map {
            ($0.fileName, $0)
        }
    )

    #expect(STPDResultTable.allCases.count == 19)
    #expect(contractByTable.count == STPDResultTable.allCases.count)
    for table in STPDResultTable.allCases {
        let contract = try #require(contractByTable[table])
        let data = try #require(package.table(table))
        let manifest = try #require(manifestByName[table.rawValue])
        #expect(contract.orderedColumns == data.headers)
        #expect(data.columnDefinitions.map(\.name) == contract.orderedColumns)
        #expect(manifest.columns == data.columnDefinitions)
    }
}

@Test
func resultPackageWriterDurabilityCheckpointsFollowPublicationOrder() throws {
    let fixture = resultPackageFixture()
    let package = try STPDResultPackageBuilder.build(
        .automatic(dataset: fixture.dataset, run: fixture.run)
    )
    let root = FileManager.default.temporaryDirectory.appendingPathComponent(
        "stpd-result-writer-order-\(UUID().uuidString.lowercased())",
        isDirectory: true
    )
    let destination = root.appendingPathComponent("result", isDirectory: true)
    defer { try? FileManager.default.removeItem(at: root) }
    try FileManager.default.createDirectory(
        at: root,
        withIntermediateDirectories: false
    )
    var checkpoints: [STPDResultPackageWriter.Checkpoint] = []

    try STPDResultPackageWriter.writeForTesting(
        package,
        to: destination
    ) { checkpoints.append($0) }

    #expect(checkpoints == [
        .stagedContentsDurable,
        .stagingEntryDurable,
        .willPublish,
        .published,
        .publicationDurable,
    ])
    #expect(FileManager.default.fileExists(atPath: destination.path))
}

@Test
func resultPackageWriterPrepublicationFailureRemovesOnlyItsStagingTree()
    throws {
    let fixture = resultPackageFixture()
    let package = try STPDResultPackageBuilder.build(
        .automatic(dataset: fixture.dataset, run: fixture.run)
    )
    let root = FileManager.default.temporaryDirectory.appendingPathComponent(
        "stpd-result-writer-prepublish-\(UUID().uuidString.lowercased())",
        isDirectory: true
    )
    let destination = root.appendingPathComponent("result", isDirectory: true)
    defer { try? FileManager.default.removeItem(at: root) }
    try FileManager.default.createDirectory(
        at: root,
        withIntermediateDirectories: false
    )

    #expect(throws: ResultPackageWriterInjectedFailure.self) {
        try STPDResultPackageWriter.writeForTesting(
            package,
            to: destination
        ) { checkpoint in
            if checkpoint == .stagingEntryDurable {
                throw ResultPackageWriterInjectedFailure()
            }
        }
    }

    #expect(!FileManager.default.fileExists(atPath: destination.path))
    let siblings = try FileManager.default.contentsOfDirectory(
        at: root,
        includingPropertiesForKeys: nil
    )
    #expect(siblings.isEmpty)
}

@Test
func resultPackageWriterNoReplacePreservesConcurrentDestinationDirectory()
    throws {
    let fixture = resultPackageFixture()
    let package = try STPDResultPackageBuilder.build(
        .automatic(dataset: fixture.dataset, run: fixture.run)
    )
    let root = FileManager.default.temporaryDirectory.appendingPathComponent(
        "stpd-result-writer-race-\(UUID().uuidString.lowercased())",
        isDirectory: true
    )
    let destination = root.appendingPathComponent("result", isDirectory: true)
    let marker = destination.appendingPathComponent("winner.txt")
    defer { try? FileManager.default.removeItem(at: root) }
    try FileManager.default.createDirectory(
        at: root,
        withIntermediateDirectories: false
    )

    #expect(throws: STPDResultPackageError.self) {
        try STPDResultPackageWriter.writeForTesting(
            package,
            to: destination
        ) { checkpoint in
            guard checkpoint == .willPublish else { return }
            try FileManager.default.createDirectory(
                at: destination,
                withIntermediateDirectories: false
            )
            try Data("concurrent winner".utf8).write(to: marker)
        }
    }

    #expect(try Data(contentsOf: marker) == Data("concurrent winner".utf8))
    let siblings = try FileManager.default.contentsOfDirectory(
        at: root,
        includingPropertiesForKeys: nil
    )
    #expect(siblings.map(\.lastPathComponent) == ["result"])
}

@Test
func resultPackageWriterNoReplacePreservesConcurrentDestinationSymlinks()
    throws {
    let fixture = resultPackageFixture()
    let package = try STPDResultPackageBuilder.build(
        .automatic(dataset: fixture.dataset, run: fixture.run)
    )

    for dangling in [false, true] {
        let root = FileManager.default.temporaryDirectory.appendingPathComponent(
            "stpd-result-writer-symlink-\(UUID().uuidString.lowercased())",
            isDirectory: true
        )
        let destination = root.appendingPathComponent("result")
        let target = root.appendingPathComponent(
            dangling ? "missing-target" : "existing-target",
            isDirectory: true
        )
        defer { try? FileManager.default.removeItem(at: root) }
        try FileManager.default.createDirectory(
            at: root,
            withIntermediateDirectories: true
        )
        if !dangling {
            try FileManager.default.createDirectory(
                at: target,
                withIntermediateDirectories: false
            )
        }

        #expect(throws: STPDResultPackageError.self) {
            try STPDResultPackageWriter.writeForTesting(
                package,
                to: destination
            ) { checkpoint in
                guard checkpoint == .willPublish else { return }
                try FileManager.default.createSymbolicLink(
                    at: destination,
                    withDestinationURL: target
                )
            }
        }

        let linkedPath = try FileManager.default.destinationOfSymbolicLink(
            atPath: destination.path
        )
        #expect(linkedPath == target.path)
        let siblings = try FileManager.default.contentsOfDirectory(
            at: root,
            includingPropertiesForKeys: nil
        )
        let expectedSiblings = dangling
            ? Set(["result"])
            : Set(["result", target.lastPathComponent])
        #expect(Set(siblings.map(\.lastPathComponent)) == expectedSiblings)
    }
}

@Test
func resultPackageWriterPinsOpenedParentAcrossPathReplacement() throws {
    let fixture = resultPackageFixture()
    let package = try STPDResultPackageBuilder.build(
        .automatic(dataset: fixture.dataset, run: fixture.run)
    )
    let container = FileManager.default.temporaryDirectory
        .appendingPathComponent(
            "stpd-result-writer-parent-race-\(UUID().uuidString.lowercased())",
            isDirectory: true
        )
    let parent = container.appendingPathComponent("parent", isDirectory: true)
    let movedParent = container.appendingPathComponent(
        "opened-parent",
        isDirectory: true
    )
    let destination = parent.appendingPathComponent(
        "result",
        isDirectory: true
    )
    defer { try? FileManager.default.removeItem(at: container) }
    try FileManager.default.createDirectory(
        at: parent,
        withIntermediateDirectories: true
    )

    var replaced = false
    try STPDResultPackageWriter.writeForTesting(
        package,
        to: destination
    ) { checkpoint in
        guard checkpoint == .willPublish else { return }
        try FileManager.default.moveItem(at: parent, to: movedParent)
        try FileManager.default.createDirectory(
            at: parent,
            withIntermediateDirectories: false
        )
        replaced = true
    }

    let pinnedDestination = movedParent.appendingPathComponent(
        "result",
        isDirectory: true
    )
    #expect(replaced)
    #expect(FileManager.default.fileExists(atPath: pinnedDestination.path))
    #expect(!FileManager.default.fileExists(atPath: destination.path))
    let readback = try STPDResultPackageReader.read(
        packageAt: pinnedDestination
    )
    #expect(readback.runID == package.manifest.runID)
    #expect(readback.verified)
}

@Test
func resultPackageWriterPostpublicationFailureLeavesCompleteDestination()
    throws {
    let fixture = resultPackageFixture()
    let package = try STPDResultPackageBuilder.build(
        .automatic(dataset: fixture.dataset, run: fixture.run)
    )
    let root = FileManager.default.temporaryDirectory.appendingPathComponent(
        "stpd-result-writer-postpublish-\(UUID().uuidString.lowercased())",
        isDirectory: true
    )
    let destination = root.appendingPathComponent("result", isDirectory: true)
    defer { try? FileManager.default.removeItem(at: root) }
    try FileManager.default.createDirectory(
        at: root,
        withIntermediateDirectories: false
    )

    #expect(throws: ResultPackageWriterInjectedFailure.self) {
        try STPDResultPackageWriter.writeForTesting(
            package,
            to: destination
        ) { checkpoint in
            if checkpoint == .published {
                throw ResultPackageWriterInjectedFailure()
            }
        }
    }

    let files = try FileManager.default.contentsOfDirectory(
        at: destination,
        includingPropertiesForKeys: nil
    )
    #expect(Set(files.map(\.lastPathComponent)) ==
        Set(STPDResultTable.allCases.map(\.rawValue) + [
            STPDResultSchema.manifestFileName,
        ]))
    let siblings = try FileManager.default.contentsOfDirectory(
        at: root,
        includingPropertiesForKeys: nil
    )
    #expect(siblings.map(\.lastPathComponent) == ["result"])
}

@Test
func resultPackageWriterRejectsReplacedStagingPayloadBeforePublication()
    throws {
    let fixture = resultPackageFixture()
    let package = try STPDResultPackageBuilder.build(
        .automatic(dataset: fixture.dataset, run: fixture.run)
    )
    let root = FileManager.default.temporaryDirectory.appendingPathComponent(
        "stpd-result-writer-staging-race-\(UUID().uuidString.lowercased())",
        isDirectory: true
    )
    let destination = root.appendingPathComponent("result", isDirectory: true)
    defer { try? FileManager.default.removeItem(at: root) }
    try FileManager.default.createDirectory(
        at: root,
        withIntermediateDirectories: false
    )

    var replacementMarker: URL?
    #expect(throws: STPDResultPackageError.self) {
        try STPDResultPackageWriter.writeForTesting(
            package,
            to: destination
        ) { checkpoint in
            guard checkpoint == .willPublish else { return }
            let siblings = try FileManager.default.contentsOfDirectory(
                at: root,
                includingPropertiesForKeys: nil
            )
            let workspace = try #require(
                siblings.first {
                    $0.lastPathComponent.hasPrefix(".result.tmp.")
                }
            )
            let payload = workspace.appendingPathComponent(
                "payload",
                isDirectory: true
            )
            let displaced = workspace.appendingPathComponent(
                "displaced-payload",
                isDirectory: true
            )
            try FileManager.default.moveItem(at: payload, to: displaced)
            try FileManager.default.createDirectory(
                at: payload,
                withIntermediateDirectories: false
            )
            let marker = payload.appendingPathComponent("replacement.txt")
            try Data("must never publish".utf8).write(to: marker)
            replacementMarker = marker
        }
    }

    #expect(!FileManager.default.fileExists(atPath: destination.path))
    let marker = try #require(replacementMarker)
    #expect(try Data(contentsOf: marker) == Data("must never publish".utf8))
}

@Test
func resultPackageWriterRejectsMissingParentWithoutCreatingAnything() throws {
    let fixture = resultPackageFixture()
    let package = try STPDResultPackageBuilder.build(
        .automatic(dataset: fixture.dataset, run: fixture.run)
    )
    let missingParent = FileManager.default.temporaryDirectory
        .appendingPathComponent(
            "stpd-result-writer-missing-parent-\(UUID().uuidString.lowercased())",
            isDirectory: true
        )
    let destination = missingParent.appendingPathComponent(
        "result",
        isDirectory: true
    )
    defer { try? FileManager.default.removeItem(at: missingParent) }

    #expect(throws: STPDResultPackageError.self) {
        try STPDResultPackageWriter.write(package, to: destination)
    }
    #expect(!FileManager.default.fileExists(atPath: missingParent.path))
    #expect(!FileManager.default.fileExists(atPath: destination.path))
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
func resultPackageTableDataRejectsMalformedSchemasAndTypedCells() throws {
    let fixture = resultPackageFixture()
    let automaticPackage = try STPDResultPackageBuilder.build(
        .automatic(dataset: fixture.dataset, run: fixture.run)
    )
    let annotation = try resultPackageManualTonicAnnotation(
        dataset: fixture.dataset,
        run: fixture.run,
        id: UUID(
            uuidString: "a1100000-0000-0000-0000-000000000001"
        )!
    )
    let reviewedPackage = try STPDResultPackageBuilder.build(
        resultPackageReviewedInput(
            dataset: fixture.dataset,
            run: fixture.run,
            manualAnnotations: [annotation]
        )
    )
    let metadata = try #require(automaticPackage.table(.runMetadata))

    // Schema failures remain independently covered by real table contracts.
    #expect(throws: STPDResultPackageError.self) {
        _ = try STPDResultTableData(
            contract: metadata.contract,
            headers: ["run_id", "run_id"],
            rows: [["run", "duplicate"]]
        )
    }
    #expect(throws: STPDResultPackageError.self) {
        _ = try STPDResultTableData(
            contract: metadata.contract,
            headers: metadata.headers,
            columnDefinitions: Array(metadata.columnDefinitions.dropLast()),
            rows: metadata.rows
        )
    }

    func expectTypedCellRejection(
        _ table: STPDResultTableData,
        column: String,
        invalidValue: String
    ) throws {
        let columnIndex = try #require(
            table.headers.firstIndex(of: column)
        )
        var rows = table.rows
        try #require(!rows.isEmpty)
        rows[0][columnIndex] = invalidValue
        #expect(throws: STPDResultPackageError.self) {
            _ = try STPDResultTableData(
                contract: table.contract,
                headers: table.headers,
                columnDefinitions: table.columnDefinitions,
                rows: rows
            )
        }
    }

    try expectTypedCellRejection(
        metadata,
        column: "run_id",
        invalidValue: ""
    )
    try expectTypedCellRejection(
        metadata,
        column: "train_count",
        invalidValue: "1.5"
    )
    try expectTypedCellRejection(
        try #require(automaticPackage.table(.dataQualityQC)),
        column: "artifact_fraction",
        invalidValue: "nan"
    )
    try expectTypedCellRejection(
        metadata,
        column: "build_reproducibility_attested",
        invalidValue: "yes"
    )
    let manualTable = try #require(
        reviewedPackage.table(.manualAnnotations)
    )
    try expectTypedCellRejection(
        manualTable,
        column: "created_at",
        invalidValue: "not-a-timestamp"
    )
    for invalidValue in [
        "[\"a\", \"b\"]",
        "[\"a\",\"a\"]",
        "[\"\"]",
        "not-json",
    ] {
        try expectTypedCellRejection(
            manualTable,
            column: "linked_isi_uids",
            invalidValue: invalidValue
        )
    }
}

@Test
func resultPackageSchemaIsCompleteTypedAndStable() throws {
    let fixture = resultPackageFixture()
    let package = try STPDResultPackageBuilder.build(
        .automatic(dataset: fixture.dataset, run: fixture.run)
    )
    let expectedSchemaDigests: [STPDResultTable: String] = [
        .runMetadata: "0a900b992c89df8acf01682bdd83a144727593d87d7d71f4312883c8d3a06d26",
        .parametersReport: "851e0fac9fac8269caadee89ef237fba838267c12caf420bd37065957efaf441",
        .resolvedParameters: "3e788241ca08dc2366abc20be401cb49bfef4a818a2bd8770d3d813bec7f1698",
        .candidateLedger: "52d4be4eac5ba51d2d742206c5be99fd777c400d18fa7b6fa38bd170fc9df16f",
        .candidateFeatures: "e3ba638848f9b45abbe0372641f4d0e923ad58b8be26db51e27c069aca32c15e",
        .finalDecisions: "da9f2568449651a7452064cf98a0044ea0c0f1123bd0327070f011a420a29c9d",
        .candidateLedgerDiagnostic:
            "52d4be4eac5ba51d2d742206c5be99fd777c400d18fa7b6fa38bd170fc9df16f",
        .candidateFeaturesDiagnostic:
            "e3ba638848f9b45abbe0372641f4d0e923ad58b8be26db51e27c069aca32c15e",
        .finalDecisionsDiagnostic:
            "da9f2568449651a7452064cf98a0044ea0c0f1123bd0327070f011a420a29c9d",
        .eventsFinal: "ca9af10740963a2b1a2676f7f4d0efb0d06833d3718ce0481d1b1dfd17bd98e5",
        .isiLabelsFinal: "5b955914c2fe680ec83f7f0b6c3b22c02a3965deb40a4639f74564f423e45a59",
        .candidateDiagnosticAudit: "1d6506c9da3f757dcf414f3ee3feda6ab809ea752f3ac999586fab50a2efdc51",
        .resultConsistencyCheck: "9ac0cbfa6f1ef344c4cf1dbc5e3b6724e331efecc3a705f51aed11af25f314ef",
        .manualAnnotations: "41e9a180a092a7df951c1957cf4c79618a455b189b9dfd78089e6a05cebcf294",
        .manualAnnotationImportApprovals:
            "7c58bafcf708a2bd94d80013b4c4701321f16e031f1491db1f1660919443cb04",
        .reviewStatus: "7f911aedcf91a66883da01640b60c02bd97d7868e45ab750a6221ae1509a82c5",
        .hfsBurstArbitrationAudit:
            "2c42cd159f246affaf7d5e61132959a1c1102f7992c5fbbddd07d624f6dc0c6c",
        .taskEvents: "076b9aec172472b9a0a393d1c89567e48f0a4f83b0a96c95af6738cd9826f8ac",
        .dataQualityQC:
            "28696b84418f950933457ec9bc48e98fe9532eca708d25fac8aaf989dc40af86",
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
        (.hfsBurstArbitrationAudit, "hfs_candidate_uid"),
        (.hfsBurstArbitrationAudit, "audit_hfs_raw_score"),
        (.hfsBurstArbitrationAudit, "audit_final_decision"),
    ]
    for (table, name) in requiredColumns {
        #expect(try definition(table, name).nullable == false)
    }

    let optionalColumns: [(STPDResultTable, String)] = [
        (.runMetadata, "dataset_source"),
        (.candidateLedger, "pause_boundary_role"),
        (.candidateLedgerDiagnostic, "pause_boundary_role"),
        (.candidateFeatures, "state_direct_support_spans"),
        (.candidateFeatures, "state_interruption_spans"),
        (.candidateFeatures, "state_direct_support_isi_count"),
        (.candidateFeatures, "state_direct_support_adjacent_pair_count"),
        (.candidateDiagnosticAudit, "source_pause_boundary_role"),
        (.finalDecisions, "failure_reason"),
        (.finalDecisions, "state_tonic_subtype"),
        (.eventsFinal, "state_tonic_subtype"),
        (.manualAnnotations, "start_isi_index"),
        (.manualAnnotations, "end_isi_index"),
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
        (.candidateFeatures, "state_direct_support_spans", .stringList),
        (.candidateFeatures, "state_interruption_spans", .stringList),
        (.candidateFeatures, "state_direct_support_isi_count", .integer),
        (.candidateFeatures, "state_direct_support_adjacent_pair_count", .integer),
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

@Test
func resultPackageManualOnlyProjectionUsesManualSourceMode() throws {
    let fixture = resultPackageFixture()
    let annotation = try resultPackageManualTonicAnnotation(
        dataset: fixture.dataset,
        run: fixture.run,
        id: UUID(uuidString: "d1000000-0000-0000-0000-000000000001")!
    )
    let input = try STPDResultPackageInput.snapshot(
        dataset: fixture.dataset,
        run: fixture.run,
        manualAnnotations: [annotation]
    )

    #expect(input.sourceMode == .manual)
    #expect(input.candidateReviews.isEmpty)
    #expect(input.reviewLinks.contains {
        if case .manualAnnotation(annotation.id) = $0.evidence {
            return true
        }
        return false
    })

    let package = try STPDResultPackageBuilder.build(input)
    #expect(package.sourceMode == .manual)
    #expect(package.manifest.sourceMode == STPDResultPackageSourceMode.manual.rawValue)
    #expect(package.table(.reviewStatus)?.rowCount == 0)
    let manualTable = try #require(package.table(.manualAnnotations))
    #expect(manualTable.rowCount == 1)
    #expect(manualTable.value(row: 0, column: "link_scope")
        == "public_projection")
    #expect(STPDCanonicalValue.parseStringList(
        try #require(manualTable.value(row: 0, column: "linked_isi_uids"))
    )?.isEmpty == false)
}

@Test
func resultPackageManualHighFrequencyBurstUsesEventSemanticTrack() throws {
    let fixture = resultPackageFixture()
    let train = fixture.dataset.trains[0]
    let annotation = ManualAnnotation(
        id: UUID(uuidString: "d1100000-0000-0000-0000-000000000001")!,
        trainID: train.id,
        label: .highFrequencyBurst,
        startSec: train.timestampsSec[1],
        endSec: train.timestampsSec[3],
        annotator: "Result Package Test Reviewer",
        annotatorIdentitySource: .userProvided,
        createdAt: Date(timeIntervalSince1970: 10),
        updatedAt: Date(timeIntervalSince1970: 20)
    )
    let input = try STPDResultPackageInput.snapshot(
        dataset: fixture.dataset,
        run: fixture.run,
        manualAnnotations: [annotation]
    )
    let package = try STPDResultPackageBuilder.build(input)
    let events = try #require(package.table(.eventsFinal))
    let matchingRow = try #require(events.rows.first { row in
        guard let labelIndex = events.headers.firstIndex(of: "final_label") else { return false }
        return row[labelIndex] == ManualAnnotationLabel.highFrequencyBurst.rawValue
    })
    let semanticTrackIndex = try #require(events.headers.firstIndex(of: "semantic_track"))

    #expect(matchingRow[semanticTrackIndex] == ClassicAnchorSemanticTrack.event.rawValue)
}

@Test
func resultPackagePositiveAnnotationOwnsOverlapWithInactiveNegativeVeto() throws {
    let fixture = resultPackageFixture()
    let automatic = STPDResultPackageInput.automatic(
        dataset: fixture.dataset,
        run: fixture.run
    )
    let row = try #require(automatic.finalISILabelRows.first {
        $0.isiIndex > 0
            && ManualAnnotationProjector.burstFamilyLabels.contains($0.autoPattern)
    })
    let train = try #require(
        fixture.dataset.trains.first { $0.id == row.trainID }
    )
    let positiveID = UUID(uuidString: "d2000000-0000-0000-0000-000000000002")!
    let vetoID = UUID(uuidString: "d3000000-0000-0000-0000-000000000003")!
    let positive = ManualAnnotation(
        id: positiveID,
        trainID: train.id,
        label: .tonic,
        startSec: train.timestampsSec[row.isiIndex - 1],
        endSec: train.timestampsSec[row.isiIndex],
        annotator: "Result Package Test Reviewer",
        annotatorIdentitySource: .userProvided,
        createdAt: Date(timeIntervalSince1970: 10),
        updatedAt: Date(timeIntervalSince1970: 20)
    )
    let veto = ManualAnnotation(
        id: vetoID,
        trainID: train.id,
        label: .notBurst,
        startSec: train.timestampsSec[row.isiIndex - 1],
        endSec: train.timestampsSec[row.isiIndex],
        createdAt: Date(timeIntervalSince1970: 30),
        updatedAt: Date(timeIntervalSince1970: 40)
    )
    let input = try STPDResultPackageInput.snapshot(
        dataset: fixture.dataset,
        run: fixture.run,
        manualAnnotations: [veto, positive]
    )
    let package = try STPDResultPackageBuilder.build(input)
    let manualTable = try #require(package.table(.manualAnnotations))
    let sourceUUIDIndex = try #require(
        manualTable.headers.firstIndex(of: "source_annotation_uuid")
    )
    let annotationUIDIndex = try #require(
        manualTable.headers.firstIndex(of: "annotation_id")
    )
    let linksIndex = try #require(
        manualTable.headers.firstIndex(of: "linked_isi_uids")
    )
    let scopeIndex = try #require(
        manualTable.headers.firstIndex(of: "link_scope")
    )
    let rowsByUUID = Dictionary(uniqueKeysWithValues:
        manualTable.rows.map { ($0[sourceUUIDIndex], $0) })
    let positiveRow = try #require(
        rowsByUUID[positiveID.uuidString.lowercased()]
    )
    let vetoRow = try #require(rowsByUUID[vetoID.uuidString.lowercased()])
    let positiveUID = positiveRow[annotationUIDIndex]
    let vetoUID = vetoRow[annotationUIDIndex]

    #expect(STPDCanonicalValue.parseStringList(
        positiveRow[linksIndex]
    )?.isEmpty == false)
    #expect(positiveRow[scopeIndex] == "public_projection")
    #expect(STPDCanonicalValue.parseStringList(vetoRow[linksIndex]) == [])
    #expect(vetoRow[scopeIndex] == "inactive_or_superseded")

    let isiTable = try #require(package.table(.isiLabelsFinal))
    let evidence = try resultPackageColumn(isiTable, "review_evidence_uids")
        .compactMap(STPDCanonicalValue.parseStringList)
        .flatMap { $0 }
    #expect(evidence.contains(positiveUID))
    #expect(!evidence.contains(vetoUID))
}

@Test
func resultPackageRejectedNonPublicCandidateRemainsDiagnosticOnly() throws {
    let fixture = resultPackageFixture()
    let automatic = STPDResultPackageInput.automatic(
        dataset: fixture.dataset,
        run: fixture.run
    )
    let publicCandidateIDs = Set(automatic.finalISILabelRows.compactMap {
        $0.isiIndex > 0 && !$0.autoCandidateID.isEmpty
            ? $0.autoCandidateID
            : nil
    })
    let nonPublicCandidateID = try #require(
        fixture.run.candidates.first {
            !publicCandidateIDs.contains($0.id)
                && $0.trainID != "__dataset__"
        }?.id
    )
    let input = try STPDResultPackageInput.snapshot(
        dataset: fixture.dataset,
        run: fixture.run,
        candidateReviews: [
            STPDCandidateReviewInput(
                sourceCandidateID: nonPublicCandidateID,
                status: .rejected,
                reviewer: "Independent reviewer",
                reviewedAt: Date(timeIntervalSince1970: 500),
                reviewedRunID: fixture.run.runIdentity.runID
            ),
        ]
    )

    #expect(input.sourceMode == .automatic)
    #expect(input.reviewLinks.isEmpty)
    #expect(input.finalEvents == automatic.finalEvents)
    #expect(input.finalISILabelRows == automatic.finalISILabelRows)
    #expect(input.candidateDiagnostics.contains {
        $0.sourceCandidateID == nonPublicCandidateID
            && $0.status == STPDCandidateReviewStatus.rejected.rawValue
            && $0.details.contains("reason=no_causal_public_projection")
    })

    let package = try STPDResultPackageBuilder.build(input)
    #expect(package.sourceMode == .automatic)
    let reviewTable = try #require(package.table(.reviewStatus))
    #expect(reviewTable.rowCount == 1)
    #expect(reviewTable.value(row: 0, column: "link_scope")
        == "non_authoritative")
}

@Test
func resultPackageAuthorityBearingReviewRejectsStaleRunBinding() throws {
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
    let reviewedAt = Date(timeIntervalSince1970: 300)

    #expect(throws: STPDResultPackageError.self) {
        _ = try STPDResultPackageInput.snapshot(
            dataset: fixture.dataset,
            run: fixture.run,
            candidateReviews: [
                STPDCandidateReviewInput(
                    sourceCandidateID: publicCandidateID,
                    status: .accepted,
                    reviewer: "Independent reviewer",
                    reviewedAt: reviewedAt,
                    reviewedRunID: "stale_detector_run"
                ),
            ]
        )
    }

    let current = try STPDResultPackageInput.snapshot(
        dataset: fixture.dataset,
        run: fixture.run,
        candidateReviews: [
            STPDCandidateReviewInput(
                sourceCandidateID: publicCandidateID,
                status: .accepted,
                reviewer: "Independent reviewer",
                reviewedAt: reviewedAt,
                reviewedRunID: fixture.run.runIdentity.runID
            ),
        ]
    )
    #expect(current.sourceMode == .reviewed)
    #expect(!current.reviewLinks.isEmpty)
    #expect(try STPDResultPackageBuilder.build(current).sourceMode == .reviewed)
}

@Test
func resultPackageCandidateReviewRejectsNonFiniteTimestampAtSnapshot() throws {
    let fixture = resultPackageFixture()
    let candidateID = try #require(
        fixture.run.candidates.first { $0.trainID != "__dataset__" }?.id
    )

    #expect(throws: STPDResultPackageError.self) {
        _ = try STPDResultPackageInput.snapshot(
            dataset: fixture.dataset,
            run: fixture.run,
            candidateReviews: [
                STPDCandidateReviewInput(
                    sourceCandidateID: candidateID,
                    status: .needsReview,
                    reviewer: "Independent reviewer",
                    reviewedAt: Date(timeIntervalSince1970: .nan),
                    reviewedRunID: fixture.run.runIdentity.runID
                ),
            ]
        )
    }
}

@Test
func resultPackageAuthorityBearingReviewRequiresReviewTimestamp() throws {
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
    #expect(throws: STPDResultPackageError.self) {
        _ = try STPDResultPackageInput.snapshot(
            dataset: fixture.dataset,
            run: fixture.run,
            candidateReviews: [
                STPDCandidateReviewInput(
                    sourceCandidateID: publicCandidateID,
                    status: .accepted,
                    reviewer: "Independent reviewer",
                    reviewedRunID: fixture.run.runIdentity.runID
                ),
            ]
        )
    }
}

@Test
func resultPackageAuthorityBearingManualRequiresNamedAnnotator() throws {
    let fixture = resultPackageFixture()
    var annotation = try resultPackageManualTonicAnnotation(
        dataset: fixture.dataset,
        run: fixture.run,
        id: UUID(uuidString: "d8000000-0000-0000-0000-000000000008")!
    )
    annotation.annotator = nil
    annotation.annotatorIdentitySource = nil
    let input = try STPDResultPackageInput.snapshot(
        dataset: fixture.dataset,
        run: fixture.run,
        manualAnnotations: [annotation]
    )

    #expect(throws: STPDResultPackageError.self) {
        _ = try STPDResultPackageBuilder.build(input)
    }
}

@Test
func resultPackageAuthorityBearingManualRejectsUnknownIdentitySource() throws {
    let fixture = resultPackageFixture()
    var annotation = try resultPackageManualTonicAnnotation(
        dataset: fixture.dataset,
        run: fixture.run,
        id: UUID(uuidString: "d9000000-0000-0000-0000-000000000009")!
    )
    annotation.annotatorIdentitySource = .unknown
    let input = try STPDResultPackageInput.snapshot(
        dataset: fixture.dataset,
        run: fixture.run,
        manualAnnotations: [annotation]
    )

    #expect(throws: STPDResultPackageError.self) {
        _ = try STPDResultPackageBuilder.build(input)
    }
}

@Test
func resultPackageRejectsOrderIndependentEqualLatestManualConflicts() throws {
    let fixture = resultPackageFixture()
    let sharedID = UUID(
        uuidString: "da000000-0000-0000-0000-00000000000a"
    )!
    let first = try resultPackageManualTonicAnnotation(
        dataset: fixture.dataset,
        run: fixture.run,
        id: sharedID
    )
    var conflicting = first
    conflicting.annotator = "Different reviewer"
    conflicting.annotatorIdentitySource = .imported

    for annotations in [[first, conflicting], [conflicting, first]] {
        #expect(throws: STPDResultPackageError.self) {
            _ = try STPDResultPackageInput.snapshot(
                dataset: fixture.dataset,
                run: fixture.run,
                manualAnnotations: annotations
            )
        }
    }
}

@Test
func resultPackageCollapsesExactDuplicateLatestManualRecords() throws {
    let fixture = resultPackageFixture()
    let annotation = try resultPackageManualTonicAnnotation(
        dataset: fixture.dataset,
        run: fixture.run,
        id: UUID(
            uuidString: "db000000-0000-0000-0000-00000000000b"
        )!
    )
    let input = try STPDResultPackageInput.snapshot(
        dataset: fixture.dataset,
        run: fixture.run,
        manualAnnotations: [annotation, annotation]
    )
    let package = try STPDResultPackageBuilder.build(input)
    let manualTable = try #require(package.table(.manualAnnotations))

    #expect(manualTable.rowCount == 1)
}

@Test
func resultPackageDiagnosticOrdinalOverflowFailsClosedWithoutTrap() throws {
    let fixture = resultPackageFixture()
    let candidateID = try #require(
        fixture.run.candidates.first { $0.trainID != "__dataset__" }?.id
    )

    #expect(throws: STPDResultPackageError.self) {
        _ = try STPDResultPackageInput.snapshot(
            dataset: fixture.dataset,
            run: fixture.run,
            candidateReviews: [
                STPDCandidateReviewInput(
                    sourceCandidateID: candidateID,
                    status: .needsReview,
                    reviewer: "Independent reviewer"
                ),
            ],
            candidateDiagnostics: [
                STPDCandidateDiagnosticInput(
                    sourceCandidateID: candidateID,
                    stageName: "overflow_fixture",
                    stageOrdinal: Int.max,
                    status: "observed",
                    details: "forces generated diagnostic ordinal overflow"
                ),
            ]
        )
    }
}

@Test
func resultPackageMaximumSuppliedDiagnosticOrdinalNeedsNoExtensionWithoutReviews() throws {
    let fixture = resultPackageFixture()
    let candidateID = try #require(
        fixture.run.candidates.first { $0.trainID != "__dataset__" }?.id
    )

    let input = try STPDResultPackageInput.snapshot(
        dataset: fixture.dataset,
        run: fixture.run,
        candidateDiagnostics: [
            STPDCandidateDiagnosticInput(
                sourceCandidateID: candidateID,
                stageName: "maximum_supplied_ordinal",
                stageOrdinal: Int.max,
                status: "observed",
                details: "no generated review diagnostic is required"
            ),
        ]
    )

    #expect(input.candidateDiagnostics.count == 1)
    #expect(input.candidateDiagnostics.first?.stageOrdinal == Int.max)
}

@Test
func resultPackageValidatorRejectsEventISIProjectionTampering() throws {
    let fixture = resultPackageFixture()
    let package = try STPDResultPackageBuilder.build(
        .automatic(dataset: fixture.dataset, run: fixture.run)
    )
    let events = try #require(package.table(.eventsFinal))
    let expectedISICount = fixture.dataset.trains.reduce(0) {
        $0 + max(0, $1.spikeCount - 1)
    }
    let rawStart = try #require(events.value(row: 0, column: "raw_start_sec"))
    let endISI = try #require(events.value(row: 0, column: "end_isi_index"))

    let mutations: [(column: String, value: String)] = [
        ("final_label", "tampered_label"),
        (
            "raw_start_sec",
            STPDCanonicalValue.double(
                (Double(rawStart) ?? 0) + 0.125
            )
        ),
        ("end_isi_index", String((Int(endISI) ?? 1) + 1)),
        ("final_subtype", "tampered_subtype"),
    ]
    for mutation in mutations {
        var tables = package.tables
        tables[.eventsFinal] = try resultPackageReplacingCell(
            events,
            column: mutation.column,
            with: mutation.value
        )
        #expect(throws: STPDResultPackageError.self) {
            _ = try STPDResultPackageValidator.validate(
                identity: package.identity,
                sourceMode: package.sourceMode,
                tables: tables,
                expectedISICount: expectedISICount
            )
        }
    }
}

@Test
func resultPackageValidatorRejectsEventEndSpikeOrdinalOverflowWithoutTrap()
    throws {
    let fixture = resultPackageFixture()
    let package = try STPDResultPackageBuilder.build(
        .automatic(dataset: fixture.dataset, run: fixture.run)
    )
    let events = try #require(package.table(.eventsFinal))
    let diagnostics = try #require(
        package.table(.candidateDiagnosticAudit)
    )
    let oldEventUID = try #require(
        events.value(row: 0, column: "event_uid")
    )
    let maximum = String(Int.max)
    var changedEvents = try resultPackageReplacingCells(
        events,
        updates: [
            "start_isi_index": maximum,
            "end_isi_index": maximum,
            "start_spike_ordinal": maximum,
            "end_spike_ordinal": maximum,
        ]
    )
    let changedEventUID = try resultPackageEventUID(
        identity: package.identity,
        table: changedEvents
    )
    changedEvents = try resultPackageReplacingCell(
        changedEvents,
        column: "event_uid",
        with: changedEventUID
    )

    var tables = package.tables
    tables[.eventsFinal] = changedEvents
    tables[.candidateDiagnosticAudit] = try resultPackageReplacingCells(
        diagnostics,
        whereColumn: "event_uid",
        equals: oldEventUID,
        updates: ["event_uid": changedEventUID]
    )
    let expectedISICount = fixture.dataset.trains.reduce(0) {
        $0 + max(0, $1.spikeCount - 1)
    }

    do {
        _ = try STPDResultPackageValidator.validate(
            identity: package.identity,
            sourceMode: package.sourceMode,
            tables: tables,
            expectedISICount: expectedISICount
        )
        Issue.record("Expected an unrepresentable event end ordinal to fail closed")
    } catch STPDResultPackageError.invalidTable(let table, let reason) {
        #expect(table == STPDResultTable.eventsFinal.rawValue)
        #expect(
            reason.contains(
                "end ISI index cannot be represented as a spike ordinal"
            )
        )
    } catch {
        Issue.record("Expected invalidTable, got \(error)")
    }
}

@Test
func resultPackageValidatorRejectsContradictoryManualExactTimestamps() throws {
    let fixture = resultPackageFixture()
    let annotation = try resultPackageManualTonicAnnotation(
        dataset: fixture.dataset,
        run: fixture.run,
        id: UUID(uuidString: "da000000-0000-0000-0000-00000000000a")!
    )
    let input = try resultPackageReviewedInput(
        dataset: fixture.dataset,
        run: fixture.run,
        manualAnnotations: [annotation]
    )
    let package = try STPDResultPackageBuilder.build(input)
    let manual = try #require(package.table(.manualAnnotations))
    let expectedISICount = fixture.dataset.trains.reduce(0) {
        $0 + max(0, $1.spikeCount - 1)
    }

    for mutation in [
        (column: "created_at_unix_sec", value: "101"),
        (column: "updated_at_unix_sec", value: "99"),
    ] {
        var tables = package.tables
        tables[.manualAnnotations] = try resultPackageReplacingCell(
            manual,
            column: mutation.column,
            with: mutation.value
        )
        #expect(throws: STPDResultPackageError.self) {
            _ = try STPDResultPackageValidator.validate(
                identity: package.identity,
                sourceMode: package.sourceMode,
                tables: tables,
                expectedISICount: expectedISICount
            )
        }
    }
}

@Test
func canonicalTimestampRejectsInt64UpperDoubleBoundaryWithoutTrapping() {
    let roundedUpperBoundary = Double(Int64.max)

    #expect(
        STPDCanonicalTimestamp.string(
            Date(timeIntervalSince1970: roundedUpperBoundary)
        ).isEmpty
    )
    #expect(
        STPDCanonicalTimestamp.string(
            Date(timeIntervalSince1970: roundedUpperBoundary.nextUp)
        ).isEmpty
    )
}

@Test
func canonicalTimestampRejectsNonRoundTrippableExtendedYear() throws {
    let nonRoundTrippableSeconds = -1_000_000_000_000.0
    #expect(
        STPDCanonicalTimestamp.string(
            Date(timeIntervalSince1970: nonRoundTrippableSeconds)
        ).isEmpty
    )

    let supportedDate = Date(timeIntervalSince1970: -0.1)
    let supportedSeconds = supportedDate.timeIntervalSince1970
    let supported = STPDCanonicalTimestamp.string(
        supportedDate
    )
    let parsed = try #require(STPDCanonicalTimestamp.parse(supported))
    #expect(parsed.seconds.bitPattern == supportedSeconds.bitPattern)
}

@Test
func resultPackageValidatorRejectsManualTimestampULPContradictions()
    throws {
    let fixture = resultPackageFixture()
    let annotation = try resultPackageManualTonicAnnotation(
        dataset: fixture.dataset,
        run: fixture.run,
        id: UUID(uuidString: "db000000-0000-0000-0000-00000000000b")!
    )
    let input = try resultPackageReviewedInput(
        dataset: fixture.dataset,
        run: fixture.run,
        manualAnnotations: [annotation]
    )
    let package = try STPDResultPackageBuilder.build(input)
    let manual = try #require(package.table(.manualAnnotations))
    let expectedISICount = fixture.dataset.trains.reduce(0) {
        $0 + max(0, $1.spikeCount - 1)
    }
    let timestampPairs = [
        (iso: "created_at", exact: "created_at_unix_sec"),
        (iso: "updated_at", exact: "updated_at_unix_sec"),
    ]

    for pair in timestampPairs {
        let exact = try #require(
            manual.value(row: 0, column: pair.exact).flatMap(Double.init)
        )
        for contradictorySeconds in [exact.nextDown, exact.nextUp] {
            var changedManual = try resultPackageReplacingCell(
                manual,
                column: pair.iso,
                with: STPDCanonicalValue.date(
                    Date(timeIntervalSince1970: contradictorySeconds)
                )
            )
            changedManual = try resultPackageRefreshingManualSemanticDigest(
                changedManual
            )
            var tables = package.tables
            tables[.manualAnnotations] = changedManual

            #expect(throws: STPDResultPackageError.self) {
                _ = try STPDResultPackageValidator.validate(
                    identity: package.identity,
                    sourceMode: package.sourceMode,
                    tables: tables,
                    expectedISICount: expectedISICount
                )
            }
        }
    }
}

@Test
func resultPackageManualNegativeFractionalTimestampsRoundTripAndRejectULPChanges()
    throws {
    let fixture = resultPackageFixture()
    let createdAt = Date(timeIntervalSince1970: -123.987_654_321)
    let updatedAt = Date(timeIntervalSince1970: -0.1)
    let annotation = try resultPackageManualTonicAnnotation(
        dataset: fixture.dataset,
        run: fixture.run,
        id: UUID(uuidString: "dc000000-0000-0000-0000-00000000000c")!,
        createdAt: createdAt,
        updatedAt: updatedAt
    )
    let input = try resultPackageReviewedInput(
        dataset: fixture.dataset,
        run: fixture.run,
        manualAnnotations: [annotation]
    )
    let package = try STPDResultPackageBuilder.build(input)
    let manual = try #require(package.table(.manualAnnotations))
    let expectedISICount = fixture.dataset.trains.reduce(0) {
        $0 + max(0, $1.spikeCount - 1)
    }
    var preConsistencyTables = package.tables
    preConsistencyTables.removeValue(forKey: .resultConsistencyCheck)

    _ = try STPDResultPackageValidator.validate(
        identity: package.identity,
        sourceMode: package.sourceMode,
        tables: preConsistencyTables,
        expectedISICount: expectedISICount
    )
    #expect(manual.value(row: 0, column: "created_at_unix_sec")
        .flatMap(Double.init)?.bitPattern
        == createdAt.timeIntervalSince1970.bitPattern)
    #expect(manual.value(row: 0, column: "updated_at_unix_sec")
        .flatMap(Double.init)?.bitPattern
        == updatedAt.timeIntervalSince1970.bitPattern)

    for column in ["created_at", "updated_at"] {
        let exact = column == "created_at"
            ? createdAt.timeIntervalSince1970
            : updatedAt.timeIntervalSince1970
        for contradictorySeconds in [exact.nextDown, exact.nextUp] {
            var changedManual = try resultPackageReplacingCell(
                manual,
                column: column,
                with: STPDCanonicalValue.date(
                    Date(timeIntervalSince1970: contradictorySeconds)
                )
            )
            changedManual = try resultPackageRefreshingManualSemanticDigest(
                changedManual
            )
            var tables = package.tables
            tables[.manualAnnotations] = changedManual

            #expect(throws: STPDResultPackageError.self) {
                _ = try STPDResultPackageValidator.validate(
                    identity: package.identity,
                    sourceMode: package.sourceMode,
                    tables: tables,
                    expectedISICount: expectedISICount
                )
            }
        }
    }
}

@Test
func resultPackageValidatorRejectsReviewAuthorityTampering() throws {
    let fixture = resultPackageFixture()
    let automatic = STPDResultPackageInput.automatic(
        dataset: fixture.dataset,
        run: fixture.run
    )
    let candidateID = try #require(
        automatic.finalISILabelRows.first {
            $0.isiIndex > 0 && !$0.autoCandidateID.isEmpty
        }?.autoCandidateID
    )
    let input = try resultPackageReviewedInput(
        dataset: fixture.dataset,
        run: fixture.run,
        candidateReviews: [
            STPDCandidateReviewInput(
                sourceCandidateID: candidateID,
                status: .accepted,
                reviewer: "Independent reviewer",
                reviewedAt: Date(timeIntervalSince1970: 300)
            ),
        ]
    )
    let package = try STPDResultPackageBuilder.build(input)
    let reviews = try #require(package.table(.reviewStatus))
    let expectedISICount = fixture.dataset.trains.reduce(0) {
        $0 + max(0, $1.spikeCount - 1)
    }
    let mutations: [(column: String, value: String)] = [
        ("reviewed_run_id", "stale_detector_run"),
        ("reviewer", "  "),
        ("reviewed_at", ""),
        ("reviewed_at_unix_sec", "301"),
        ("status", STPDCandidateReviewStatus.needsReview.rawValue),
        ("link_scope", "non_authoritative"),
    ]
    for mutation in mutations {
        var tables = package.tables
        tables[.reviewStatus] = try resultPackageReplacingCell(
            reviews,
            column: mutation.column,
            with: mutation.value
        )
        #expect(throws: STPDResultPackageError.self) {
            _ = try STPDResultPackageValidator.validate(
                identity: package.identity,
                sourceMode: package.sourceMode,
                tables: tables,
                expectedISICount: expectedISICount
            )
        }
    }
}

@Test
func resultPackageValidatorRejectsReviewTimestampULPContradictions()
    throws {
    let package = try resultPackageAcceptedReviewPackage()
    let reviews = try #require(package.table(.reviewStatus))
    let expectedISICount = try #require(
        package.table(.isiLabelsFinal)?.rows.count
    )
    let exact = try #require(
        reviews.value(row: 0, column: "reviewed_at_unix_sec")
            .flatMap(Double.init)
    )

    for contradictorySeconds in [exact.nextDown, exact.nextUp] {
        var tables = package.tables
        tables[.reviewStatus] = try resultPackageReplacingCell(
            reviews,
            column: "reviewed_at",
            with: STPDCanonicalValue.date(
                Date(timeIntervalSince1970: contradictorySeconds)
            )
        )

        #expect(throws: STPDResultPackageError.self) {
            _ = try STPDResultPackageValidator.validate(
                identity: package.identity,
                sourceMode: package.sourceMode,
                tables: tables,
                expectedISICount: expectedISICount
            )
        }
    }
}

@Test
func resultPackageReviewNegativeFractionalTimestampRoundTripsAndRejectsULPChanges()
    throws {
    let reviewedAt = Date(timeIntervalSince1970: -0.1)
    let package = try resultPackageAcceptedReviewPackage(
        reviewedAt: reviewedAt
    )
    let reviews = try #require(package.table(.reviewStatus))
    let expectedISICount = try #require(
        package.table(.isiLabelsFinal)?.rows.count
    )
    let exact = try #require(
        reviews.value(row: 0, column: "reviewed_at_unix_sec")
            .flatMap(Double.init)
    )
    var preConsistencyTables = package.tables
    preConsistencyTables.removeValue(forKey: .resultConsistencyCheck)

    _ = try STPDResultPackageValidator.validate(
        identity: package.identity,
        sourceMode: package.sourceMode,
        tables: preConsistencyTables,
        expectedISICount: expectedISICount
    )
    #expect(exact.bitPattern == reviewedAt.timeIntervalSince1970.bitPattern)

    for contradictorySeconds in [exact.nextDown, exact.nextUp] {
        var tables = package.tables
        tables[.reviewStatus] = try resultPackageReplacingCell(
            reviews,
            column: "reviewed_at",
            with: STPDCanonicalValue.date(
                Date(timeIntervalSince1970: contradictorySeconds)
            )
        )

        #expect(throws: STPDResultPackageError.self) {
            _ = try STPDResultPackageValidator.validate(
                identity: package.identity,
                sourceMode: package.sourceMode,
                tables: tables,
                expectedISICount: expectedISICount
            )
        }
    }
}

@Test
func resultPackageValidatorRejectsReviewSemanticMutationWithoutUIDUpdate()
    throws {
    let package = try resultPackageAcceptedReviewPackage()
    let reviews = try #require(package.table(.reviewStatus))
    let expectedISICount = try #require(
        package.table(.isiLabelsFinal)?.rows.count
    )

    for status in [
        STPDCandidateReviewStatus.rejected,
        STPDCandidateReviewStatus.modified,
    ] {
        var tables = package.tables
        tables[.reviewStatus] = try resultPackageReplacingCell(
            reviews,
            column: "status",
            with: status.rawValue
        )
        #expect(throws: STPDResultPackageError.self) {
            _ = try STPDResultPackageValidator.validate(
                identity: package.identity,
                sourceMode: package.sourceMode,
                tables: tables,
                expectedISICount: expectedISICount
            )
        }
    }
}

@Test
func resultPackageValidatorRejectsPairedReviewActorAndTimeMutation()
    throws {
    let package = try resultPackageAcceptedReviewPackage()
    let reviews = try #require(package.table(.reviewStatus))
    let expectedISICount = try #require(
        package.table(.isiLabelsFinal)?.rows.count
    )
    let changedDate = Date(timeIntervalSince1970: 301.25)
    var changedReviews = try resultPackageReplacingCell(
        reviews,
        column: "reviewer",
        with: "Different reviewer"
    )
    changedReviews = try resultPackageReplacingCell(
        changedReviews,
        column: "reviewed_at",
        with: STPDCanonicalValue.date(changedDate)
    )
    changedReviews = try resultPackageReplacingCell(
        changedReviews,
        column: "reviewed_at_unix_sec",
        with: STPDCanonicalValue.double(changedDate.timeIntervalSince1970)
    )

    var tables = package.tables
    tables[.reviewStatus] = changedReviews
    #expect(throws: STPDResultPackageError.self) {
        _ = try STPDResultPackageValidator.validate(
            identity: package.identity,
            sourceMode: package.sourceMode,
            tables: tables,
            expectedISICount: expectedISICount
        )
    }
}

@Test
func resultPackageValidatorRejectsCoherentReviewIdentityRewriteWithoutCausality()
    throws {
    let package = try resultPackageAcceptedReviewPackage()
    let reviews = try #require(package.table(.reviewStatus))
    let finalISIs = try #require(package.table(.isiLabelsFinal))
    let finalEvents = try #require(package.table(.eventsFinal))
    let expectedISICount = finalISIs.rows.count
    let oldReviewUID = try #require(
        reviews.value(row: 0, column: "review_uid")
    )
    let candidateUID = try #require(
        reviews.value(row: 0, column: "candidate_uid")
    )
    let reviewer = try #require(
        reviews.value(row: 0, column: "reviewer")
    )
    let note = try #require(
        reviews.value(row: 0, column: "note")
    )
    let reviewedAtExact = try #require(
        reviews.value(row: 0, column: "reviewed_at_unix_sec")
    )
    let reviewedRunID = try #require(
        reviews.value(row: 0, column: "reviewed_run_id")
    )

    for status in [
        STPDCandidateReviewStatus.rejected,
        STPDCandidateReviewStatus.modified,
    ] {
        let newReviewUID = STPDCandidateReviewIdentity.make(
            candidateUID: candidateUID,
            status: status,
            reviewer: reviewer,
            note: note,
            reviewedAtUnixSec: reviewedAtExact,
            reviewedRunID: reviewedRunID
        )
        var changedReviews = try resultPackageReplacingCell(
            reviews,
            column: "status",
            with: status.rawValue
        )
        changedReviews = try resultPackageReplacingCell(
            changedReviews,
            column: "review_uid",
            with: newReviewUID
        )

        var tables = package.tables
        tables[.reviewStatus] = changedReviews
        tables[.isiLabelsFinal] = try resultPackageReplacingEvidenceUID(
            finalISIs,
            oldUID: oldReviewUID,
            newUID: newReviewUID
        )
        tables[.eventsFinal] = try resultPackageReplacingEvidenceUID(
            finalEvents,
            oldUID: oldReviewUID,
            newUID: newReviewUID
        )

        #expect(throws: STPDResultPackageError.self) {
            _ = try STPDResultPackageValidator.validate(
                identity: package.identity,
                sourceMode: package.sourceMode,
                tables: tables,
                expectedISICount: expectedISICount
            )
        }
    }
}

@Test
func resultPackageValidatorRejectsPublicDiagnosticCandidateSemanticMismatch()
    throws {
    let fixture = resultPackageFixture()
    let package = try STPDResultPackageBuilder.build(
        .automatic(dataset: fixture.dataset, run: fixture.run)
    )
    let expectedISICount = fixture.dataset.trains.reduce(0) {
        $0 + max(0, $1.spikeCount - 1)
    }
    let mutations: [
        (
            publicTable: STPDResultTable,
            diagnosticTable: STPDResultTable,
            column: String,
            value: String
        )
    ] = [
        (
            .candidateLedger,
            .candidateLedgerDiagnostic,
            "action",
            "tampered_action"
        ),
        (
            .candidateFeatures,
            .candidateFeaturesDiagnostic,
            "score",
            "123.5"
        ),
        (
            .finalDecisions,
            .finalDecisionsDiagnostic,
            "priority",
            "123456"
        ),
    ]

    for mutation in mutations {
        let publicTable = try #require(
            package.table(mutation.publicTable)
        )
        let diagnosticTable = try #require(
            package.table(mutation.diagnosticTable)
        )
        let candidateUID = try #require(
            publicTable.value(row: 0, column: "candidate_uid")
        )
        #expect(diagnosticTable.rows.contains { row in
            guard let uidIndex = diagnosticTable.headers.firstIndex(
                of: "candidate_uid"
            ) else {
                return false
            }
            return row[uidIndex] == candidateUID
        })

        var tables = package.tables
        tables[mutation.publicTable] = try resultPackageReplacingCell(
            publicTable,
            column: mutation.column,
            with: mutation.value
        )
        #expect(throws: STPDResultPackageError.self) {
            _ = try STPDResultPackageValidator.validate(
                identity: package.identity,
                sourceMode: package.sourceMode,
                tables: tables,
                expectedISICount: expectedISICount
            )
        }
    }
}

@Test
func resultPackageValidatorRejectsUnknownHFSuppressorCandidateUID() throws {
    let fixture = resultPackageFixture()
    let package = try STPDResultPackageBuilder.build(
        .automatic(dataset: fixture.dataset, run: fixture.run)
    )
    let publicFeatures = try #require(package.table(.candidateFeatures))
    let diagnosticFeatures = try #require(
        package.table(.candidateFeaturesDiagnostic)
    )
    let candidateUID = try #require(
        publicFeatures.value(row: 0, column: "candidate_uid")
    )
    let diagnosticUIDIndex = try #require(
        diagnosticFeatures.headers.firstIndex(of: "candidate_uid")
    )
    let diagnosticRowIndex = try #require(
        diagnosticFeatures.rows.firstIndex {
            $0[diagnosticUIDIndex] == candidateUID
        }
    )
    let expectedISICount = fixture.dataset.trains.reduce(0) {
        $0 + max(0, $1.spikeCount - 1)
    }

    var tables = package.tables
    tables[.candidateFeatures] = try resultPackageReplacingCell(
        publicFeatures,
        column: "hf_suppressor_candidate_uid",
        with: "candidate_missing"
    )
    tables[.candidateFeaturesDiagnostic] = try resultPackageReplacingCell(
        diagnosticFeatures,
        row: diagnosticRowIndex,
        column: "hf_suppressor_candidate_uid",
        with: "candidate_missing"
    )
    #expect(throws: STPDResultPackageError.self) {
        _ = try STPDResultPackageValidator.validate(
            identity: package.identity,
            sourceMode: package.sourceMode,
            tables: tables,
            expectedISICount: expectedISICount
        )
    }
}

@Test
func resultPackageValidatorRejectsUnlinkedAuthorityReviewMetadataTampering()
    throws {
    let fixture = resultPackageFixture()
    let automatic = STPDResultPackageInput.automatic(
        dataset: fixture.dataset,
        run: fixture.run
    )
    let publicCandidateIDs = Set(automatic.finalISILabelRows.compactMap {
        $0.isiIndex > 0 && !$0.autoCandidateID.isEmpty
            ? $0.autoCandidateID
            : nil
    })
    let nonPublicCandidateID = try #require(
        fixture.run.candidates.first {
            !publicCandidateIDs.contains($0.id)
                && $0.trainID != "__dataset__"
        }?.id
    )
    let input = try STPDResultPackageInput.snapshot(
        dataset: fixture.dataset,
        run: fixture.run,
        candidateReviews: [
            STPDCandidateReviewInput(
                sourceCandidateID: nonPublicCandidateID,
                status: .rejected,
                reviewer: "Independent reviewer",
                reviewedAt: Date(timeIntervalSince1970: 500),
                reviewedRunID: fixture.run.runIdentity.runID
            ),
        ]
    )
    let package = try STPDResultPackageBuilder.build(input)
    let reviews = try #require(package.table(.reviewStatus))
    #expect(reviews.value(row: 0, column: "link_scope")
        == "non_authoritative")
    let expectedISICount = fixture.dataset.trains.reduce(0) {
        $0 + max(0, $1.spikeCount - 1)
    }
    let mutations: [(column: String, value: String)] = [
        ("reviewed_run_id", "stale_detector_run"),
        ("reviewer", "  "),
        ("reviewed_at", ""),
        ("reviewed_at_unix_sec", "501"),
    ]

    for mutation in mutations {
        var tables = package.tables
        tables[.reviewStatus] = try resultPackageReplacingCell(
            reviews,
            column: mutation.column,
            with: mutation.value
        )
        #expect(throws: STPDResultPackageError.self) {
            _ = try STPDResultPackageValidator.validate(
                identity: package.identity,
                sourceMode: package.sourceMode,
                tables: tables,
                expectedISICount: expectedISICount
            )
        }
    }
}

@Test
func resultPackageReviewUIDPreservesSubmillisecondIdentityAndRejectsEpochTampering()
    throws {
    let fixture = resultPackageFixture()
    let automatic = STPDResultPackageInput.automatic(
        dataset: fixture.dataset,
        run: fixture.run
    )
    let candidateID = try #require(
        automatic.finalISILabelRows.first {
            $0.isiIndex > 0 && !$0.autoCandidateID.isEmpty
        }?.autoCandidateID
    )

    func package(reviewedAt: Double) throws -> STPDResultPackage {
        let input = try resultPackageReviewedInput(
            dataset: fixture.dataset,
            run: fixture.run,
            candidateReviews: [
                STPDCandidateReviewInput(
                    sourceCandidateID: candidateID,
                    status: .accepted,
                    reviewer: "Independent reviewer",
                    reviewedAt: Date(timeIntervalSince1970: reviewedAt)
                ),
            ]
        )
        return try STPDResultPackageBuilder.build(input)
    }

    let first = try package(reviewedAt: 1_800_000_000.123_456)
    let second = try package(reviewedAt: 1_800_000_000.123_956)
    let firstReviews = try #require(first.table(.reviewStatus))
    let secondReviews = try #require(second.table(.reviewStatus))
    let firstUID = try #require(
        firstReviews.value(row: 0, column: "review_uid")
    )
    let secondUID = try #require(
        secondReviews.value(row: 0, column: "review_uid")
    )
    #expect(firstUID != secondUID)
    #expect(firstReviews.value(row: 0, column: "reviewed_at")
        != secondReviews.value(row: 0, column: "reviewed_at"))
    #expect(firstReviews.value(row: 0, column: "reviewed_at_unix_sec")
        != secondReviews.value(row: 0, column: "reviewed_at_unix_sec"))

    var tamperedTables = first.tables
    tamperedTables[.reviewStatus] = try resultPackageReplacingCell(
        firstReviews,
        column: "reviewed_at_unix_sec",
        with: "1800000000.124456"
    )
    let expectedISICount = fixture.dataset.trains.reduce(0) {
        $0 + max(0, $1.spikeCount - 1)
    }
    #expect(throws: STPDResultPackageError.self) {
        _ = try STPDResultPackageValidator.validate(
            identity: first.identity,
            sourceMode: first.sourceMode,
            tables: tamperedTables,
            expectedISICount: expectedISICount
        )
    }
}

@Test
func resultPackageValidatorRejectsEventReviewEvidenceTampering() throws {
    let fixture = resultPackageFixture()
    let annotation = try resultPackageManualTonicAnnotation(
        dataset: fixture.dataset,
        run: fixture.run,
        id: UUID(uuidString: "d5000000-0000-0000-0000-000000000005")!
    )
    let input = try resultPackageReviewedInput(
        dataset: fixture.dataset,
        run: fixture.run,
        manualAnnotations: [annotation]
    )
    let package = try STPDResultPackageBuilder.build(input)
    let events = try #require(package.table(.eventsFinal))
    let evidenceColumn = try #require(
        events.headers.firstIndex(of: "review_evidence_uids")
    )
    let eventRow = try #require(events.rows.indices.first {
        !events.rows[$0][evidenceColumn].isEmpty
    })
    var tables = package.tables
    tables[.eventsFinal] = try resultPackageReplacingCell(
        events,
        row: eventRow,
        column: "review_evidence_uids",
        with: STPDCanonicalValue.stringList(["review_forged"])
    )
    let expectedISICount = fixture.dataset.trains.reduce(0) {
        $0 + max(0, $1.spikeCount - 1)
    }

    #expect(throws: STPDResultPackageError.self) {
        _ = try STPDResultPackageValidator.validate(
            identity: package.identity,
            sourceMode: package.sourceMode,
            tables: tables,
            expectedISICount: expectedISICount
        )
    }
}

@Test
func resultPackageValidatorRejectsOneSidedEvidenceLinkTampering() throws {
    let fixture = resultPackageFixture()
    let annotation = try resultPackageManualTonicAnnotation(
        dataset: fixture.dataset,
        run: fixture.run,
        id: UUID(uuidString: "d4000000-0000-0000-0000-000000000004")!
    )
    let input = try STPDResultPackageInput.snapshot(
        dataset: fixture.dataset,
        run: fixture.run,
        manualAnnotations: [annotation]
    )
    let package = try STPDResultPackageBuilder.build(input)
    let manualTable = try #require(package.table(.manualAnnotations))
    let isiTable = try #require(package.table(.isiLabelsFinal))
    let linksIndex = try #require(
        manualTable.headers.firstIndex(of: "linked_isi_uids")
    )
    let linkedUIDs = try #require(STPDCanonicalValue.parseStringList(
        manualTable.rows[0][linksIndex]
    ))
    let allISIUIDs = try resultPackageColumn(isiTable, "isi_uid")
    let extraUID = try #require(allISIUIDs.first {
        !linkedUIDs.contains($0)
    })
    var rows = manualTable.rows
    rows[0][linksIndex] = STPDCanonicalValue.stringList(
        (linkedUIDs + [extraUID]).sorted()
    )
    let tamperedManualTable = try STPDResultTableData(
        contract: manualTable.contract,
        headers: manualTable.headers,
        columnDefinitions: manualTable.columnDefinitions,
        rows: rows
    )
    var tamperedTables = package.tables
    tamperedTables[.manualAnnotations] = tamperedManualTable
    let expectedISICount = fixture.dataset.trains.reduce(0) {
        $0 + max(0, $1.spikeCount - 1)
    }

    #expect(throws: STPDResultPackageError.self) {
        _ = try STPDResultPackageValidator.validate(
            identity: package.identity,
            sourceMode: package.sourceMode,
            tables: tamperedTables,
            expectedISICount: expectedISICount
        )
    }
}

@Test
func resultPackageValidatorRejectsCoherentCrossCandidateReviewRewrite()
    throws {
    let fixture = resultPackageFixture()
    let automatic = STPDResultPackageInput.automatic(
        dataset: fixture.dataset,
        run: fixture.run
    )
    let reviewedSourceID = try #require(
        automatic.finalISILabelRows.first {
            $0.isiIndex > 0 && !$0.autoCandidateID.isEmpty
        }?.autoCandidateID
    )
    let input = try resultPackageReviewedInput(
        dataset: fixture.dataset,
        run: fixture.run,
        candidateReviews: [
            STPDCandidateReviewInput(
                sourceCandidateID: reviewedSourceID,
                status: .accepted,
                reviewer: "Independent reviewer",
                note: "candidate-bound review",
                reviewedAt: Date(timeIntervalSince1970: 600)
            ),
        ]
    )
    let package = try STPDResultPackageBuilder.build(input)
    let reviews = try #require(package.table(.reviewStatus))
    let candidates = try #require(package.table(.candidateLedger))
    let finalISIs = try #require(package.table(.isiLabelsFinal))
    let finalEvents = try #require(package.table(.eventsFinal))
    let oldReviewUID = try #require(
        reviews.value(row: 0, column: "review_uid")
    )
    let oldCandidateUID = try #require(
        reviews.value(row: 0, column: "candidate_uid")
    )
    let candidateUIDIndex = try #require(
        candidates.headers.firstIndex(of: "candidate_uid")
    )
    let sourceIDIndex = try #require(
        candidates.headers.firstIndex(of: "source_candidate_id")
    )
    let trainIDIndex = try #require(
        candidates.headers.firstIndex(of: "train_id")
    )
    let oldCandidateRow = try #require(candidates.rows.first {
        $0[candidateUIDIndex] == oldCandidateUID
    })
    let replacementRow = try #require(candidates.rows.first {
        $0[candidateUIDIndex] != oldCandidateUID &&
            $0[trainIDIndex] != oldCandidateRow[trainIDIndex]
    })
    let replacementCandidateUID = replacementRow[candidateUIDIndex]
    let replacementSourceID = replacementRow[sourceIDIndex]
    let reviewer = try #require(
        reviews.value(row: 0, column: "reviewer")
    )
    let note = try #require(reviews.value(row: 0, column: "note"))
    let reviewedAtExact = try #require(
        reviews.value(row: 0, column: "reviewed_at_unix_sec")
    )
    let reviewedRunID = try #require(
        reviews.value(row: 0, column: "reviewed_run_id")
    )
    let newReviewUID = STPDCandidateReviewIdentity.make(
        candidateUID: replacementCandidateUID,
        status: .accepted,
        reviewer: reviewer,
        note: note,
        reviewedAtUnixSec: reviewedAtExact,
        reviewedRunID: reviewedRunID
    )

    var tables = package.tables
    tables[.reviewStatus] = try resultPackageReplacingCells(
        reviews,
        updates: [
            "candidate_uid": replacementCandidateUID,
            "source_candidate_id": replacementSourceID,
            "review_uid": newReviewUID,
        ]
    )
    tables[.isiLabelsFinal] = try resultPackageReplacingEvidenceUID(
        finalISIs,
        oldUID: oldReviewUID,
        newUID: newReviewUID
    )
    tables[.eventsFinal] = try resultPackageReplacingEvidenceUID(
        finalEvents,
        oldUID: oldReviewUID,
        newUID: newReviewUID
    )

    #expect(throws: STPDResultPackageError.self) {
        _ = try resultPackageValidate(
            package,
            tables: tables,
            dataset: fixture.dataset,
            run: fixture.run
        )
    }
}

@Test
func resultPackageValidatorRejectsCanonicalButUnsortedReviewLinkList()
    throws {
    let fixture = resultPackageFixture()
    let automatic = STPDResultPackageInput.automatic(
        dataset: fixture.dataset,
        run: fixture.run
    )
    let candidateGroups = Dictionary(grouping:
        automatic.finalISILabelRows.filter {
            $0.isiIndex > 0 && !$0.autoCandidateID.isEmpty
        },
        by: \.autoCandidateID
    )
    let candidateID = try #require(
        candidateGroups.first { $0.value.count >= 2 }?.key
    )
    let input = try resultPackageReviewedInput(
        dataset: fixture.dataset,
        run: fixture.run,
        candidateReviews: [
            STPDCandidateReviewInput(
                sourceCandidateID: candidateID,
                status: .accepted,
                reviewer: "Independent reviewer",
                reviewedAt: Date(timeIntervalSince1970: 610)
            ),
        ]
    )
    let package = try STPDResultPackageBuilder.build(input)
    let reviews = try #require(package.table(.reviewStatus))
    let linkedValue = try #require(
        reviews.value(row: 0, column: "linked_isi_uids")
    )
    let linkedValues = try #require(
        STPDCanonicalValue.parseStringList(linkedValue)
    )
    try #require(linkedValues.count >= 2)
    let reversed = Array(linkedValues.reversed())
    #expect(reversed != linkedValues)

    #expect(throws: STPDResultPackageError.self) {
        _ = try resultPackageReplacingCell(
            reviews,
            column: "linked_isi_uids",
            with: STPDCanonicalValue.stringList(reversed)
        )
    }
}

@Test
func resultPackageValidatorRejectsCoherentAcceptedReviewLinkSubset()
    throws {
    let fixture = resultPackageFixture()
    let automatic = STPDResultPackageInput.automatic(
        dataset: fixture.dataset,
        run: fixture.run
    )
    let candidateGroups = Dictionary(
        grouping: automatic.finalISILabelRows.filter {
            $0.isiIndex > 0 && !$0.autoCandidateID.isEmpty
        },
        by: \.autoCandidateID
    )
    let candidateID = try #require(
        candidateGroups.keys.sorted().first {
            (candidateGroups[$0] ?? []).count >= 2
        }
    )
    let input = try resultPackageReviewedInput(
        dataset: fixture.dataset,
        run: fixture.run,
        candidateReviews: [
            STPDCandidateReviewInput(
                sourceCandidateID: candidateID,
                status: .accepted,
                reviewer: "Independent reviewer",
                reviewedAt: Date(timeIntervalSince1970: 620)
            ),
        ]
    )
    let package = try STPDResultPackageBuilder.build(input)
    let reviews = try #require(package.table(.reviewStatus))
    let finalISIs = try #require(package.table(.isiLabelsFinal))
    let reviewUID = try #require(
        reviews.value(row: 0, column: "review_uid")
    )
    let linkedValue = try #require(
        reviews.value(row: 0, column: "linked_isi_uids")
    )
    let linkedUIDs = try #require(
        STPDCanonicalValue.parseStringList(linkedValue)
    )
    try #require(linkedUIDs.count >= 2)
    let removedUID = try #require(linkedUIDs.last)
    let retainedUIDs = linkedUIDs.filter { $0 != removedUID }
    let isiUIDIndex = try #require(
        finalISIs.headers.firstIndex(of: "isi_uid")
    )
    let removedRowIndex = try #require(
        finalISIs.rows.indices.first {
            finalISIs.rows[$0][isiUIDIndex] == removedUID
        }
    )
    let changedReviews = try resultPackageReplacingCell(
        reviews,
        column: "linked_isi_uids",
        with: STPDCanonicalValue.stringList(retainedUIDs)
    )
    let changedISIs = try resultPackageRemovingEvidenceUID(
        finalISIs,
        row: removedRowIndex,
        evidenceUID: reviewUID
    )
    let changedEvents = try resultPackageSynchronizingEventEvidence(
        try #require(package.table(.eventsFinal)),
        with: changedISIs
    )
    var tables = package.tables
    tables[.reviewStatus] = changedReviews
    tables[.isiLabelsFinal] = changedISIs
    tables[.eventsFinal] = changedEvents

    #expect(throws: STPDResultPackageError.self) {
        _ = try resultPackageValidate(
            package,
            tables: tables,
            dataset: fixture.dataset,
            run: fixture.run
        )
    }
}

@Test
func resultPackageValidatorRejectsCoherentManualEvidenceLinkSubset()
    throws {
    let fixture = resultPackageFixture()
    let annotation = try resultPackageMultiISITonicAnnotation(
        dataset: fixture.dataset,
        run: fixture.run,
        id: UUID(uuidString: "de000000-0000-0000-0000-00000000000e")!
    )
    let input = try resultPackageReviewedInput(
        dataset: fixture.dataset,
        run: fixture.run,
        manualAnnotations: [annotation]
    )
    let package = try STPDResultPackageBuilder.build(input)
    let manual = try #require(package.table(.manualAnnotations))
    let finalISIs = try #require(package.table(.isiLabelsFinal))
    let annotationUID = try #require(
        manual.value(row: 0, column: "annotation_id")
    )
    let linkedValue = try #require(
        manual.value(row: 0, column: "linked_isi_uids")
    )
    let linkedUIDs = try #require(
        STPDCanonicalValue.parseStringList(linkedValue)
    )
    try #require(linkedUIDs.count >= 2)
    let removedUID = try #require(linkedUIDs.last)
    let retainedUIDs = linkedUIDs.filter { $0 != removedUID }
    let isiUIDIndex = try #require(
        finalISIs.headers.firstIndex(of: "isi_uid")
    )
    let removedRowIndex = try #require(
        finalISIs.rows.indices.first {
            finalISIs.rows[$0][isiUIDIndex] == removedUID
        }
    )
    var changedManual = try resultPackageReplacingCell(
        manual,
        column: "linked_isi_uids",
        with: STPDCanonicalValue.stringList(retainedUIDs)
    )
    changedManual = try resultPackageRefreshingManualSemanticDigest(
        changedManual
    )
    let changedISIs = try resultPackageRemovingEvidenceUID(
        finalISIs,
        row: removedRowIndex,
        evidenceUID: annotationUID
    )
    let changedEvents = try resultPackageSynchronizingEventEvidence(
        try #require(package.table(.eventsFinal)),
        with: changedISIs
    )
    var tables = package.tables
    tables[.manualAnnotations] = changedManual
    tables[.isiLabelsFinal] = changedISIs
    tables[.eventsFinal] = changedEvents

    #expect(throws: STPDResultPackageError.self) {
        _ = try resultPackageValidate(
            package,
            tables: tables,
            dataset: fixture.dataset,
            run: fixture.run
        )
    }
}

@Test
func resultPackageValidatorRejectsCoherentManualCachedGeometryRewrite()
    throws {
    let fixture = resultPackageFixture()
    let annotation = try resultPackageMultiISITonicAnnotation(
        dataset: fixture.dataset,
        run: fixture.run,
        id: UUID(uuidString: "df000000-0000-0000-0000-00000000000f")!
    )
    let input = try resultPackageReviewedInput(
        dataset: fixture.dataset,
        run: fixture.run,
        manualAnnotations: [annotation]
    )
    let package = try STPDResultPackageBuilder.build(input)
    let manual = try #require(package.table(.manualAnnotations))
    let startArrayValue = try #require(
        manual.value(row: 0, column: "start_spike_array_index")
    )
    let endArrayValue = try #require(
        manual.value(row: 0, column: "end_spike_array_index")
    )
    let startArray = try #require(Int(startArrayValue))
    let endArray = try #require(Int(endArrayValue))
    try #require(startArray + 1 <= endArray)
    var changedManual = try resultPackageReplacingCells(
        manual,
        updates: [
            "start_spike_array_index": String(startArray + 1),
            "start_spike_ordinal": String(startArray + 2),
        ]
    )
    changedManual = try resultPackageRefreshingManualSemanticDigest(
        changedManual
    )
    var tables = package.tables
    tables[.manualAnnotations] = changedManual

    #expect(throws: STPDResultPackageError.self) {
        _ = try resultPackageValidate(
            package,
            tables: tables,
            dataset: fixture.dataset,
            run: fixture.run
        )
    }
}

@Test
func resultPackageValidatorRejectsManualSpikeIndexOverflowWithoutTrap()
    throws {
    let fixture = resultPackageFixture()
    let annotation = try resultPackageMultiISITonicAnnotation(
        dataset: fixture.dataset,
        run: fixture.run,
        id: UUID(uuidString: "df100000-0000-0000-0000-000000000010")!
    )
    let input = try resultPackageReviewedInput(
        dataset: fixture.dataset,
        run: fixture.run,
        manualAnnotations: [annotation]
    )
    let package = try STPDResultPackageBuilder.build(input)
    let manual = try #require(package.table(.manualAnnotations))
    let overflow = String(Int.max)
    let mutations = [
        [
            "start_spike_array_index": overflow,
            "end_spike_array_index": overflow,
            "start_spike_ordinal": overflow,
            "end_spike_ordinal": overflow,
        ],
        [
            "end_spike_array_index": overflow,
            "end_spike_ordinal": overflow,
        ],
    ]

    for updates in mutations {
        var changedManual = try resultPackageReplacingCells(
            manual,
            updates: updates
        )
        changedManual = try resultPackageRefreshingManualSemanticDigest(
            changedManual
        )
        var tables = package.tables
        tables[.manualAnnotations] = changedManual

        #expect(throws: STPDResultPackageError.self) {
            _ = try resultPackageValidate(
                package,
                tables: tables,
                dataset: fixture.dataset,
                run: fixture.run
            )
        }
    }
}

@Test
func resultPackageManualStableIdentityIsSeparateFromSemanticRevisionDigest()
    throws {
    let fixture = resultPackageFixture()
    let sourceID = UUID(
        uuidString: "dc000000-0000-0000-0000-00000000000c"
    )!
    let first = try resultPackageManualTonicAnnotation(
        dataset: fixture.dataset,
        run: fixture.run,
        id: sourceID,
        note: "first interpretation"
    )
    var second = first
    second.note = "revised interpretation"
    second.updatedAt = Date(timeIntervalSince1970: 250)
    let firstPackage = try STPDResultPackageBuilder.build(
        resultPackageReviewedInput(
            dataset: fixture.dataset,
            run: fixture.run,
            manualAnnotations: [first]
        )
    )
    let secondPackage = try STPDResultPackageBuilder.build(
        resultPackageReviewedInput(
            dataset: fixture.dataset,
            run: fixture.run,
            manualAnnotations: [second]
        )
    )
    let firstManual = try #require(
        firstPackage.table(.manualAnnotations)
    )
    let secondManual = try #require(
        secondPackage.table(.manualAnnotations)
    )

    #expect(
        firstManual.value(row: 0, column: "annotation_id") ==
            secondManual.value(row: 0, column: "annotation_id")
    )
    #expect(
        firstManual.value(row: 0, column: "source_annotation_uuid") ==
            secondManual.value(row: 0, column: "source_annotation_uuid")
    )
    #expect(
        firstManual.value(row: 0, column: "annotation_semantic_digest") !=
            secondManual.value(row: 0, column: "annotation_semantic_digest")
    )
}

@Test
func resultPackageValidatorRejectsCoherentCrossTrainManualEvidenceRewrite()
    throws {
    let fixture = resultPackageFixture()
    let annotation = try resultPackageManualTonicAnnotation(
        dataset: fixture.dataset,
        run: fixture.run,
        id: UUID(uuidString: "dd000000-0000-0000-0000-00000000000d")!
    )
    let input = try resultPackageReviewedInput(
        dataset: fixture.dataset,
        run: fixture.run,
        manualAnnotations: [annotation]
    )
    let package = try STPDResultPackageBuilder.build(input)
    let manual = try #require(package.table(.manualAnnotations))
    let finalISIs = try #require(package.table(.isiLabelsFinal))
    let annotationUID = try #require(
        manual.value(row: 0, column: "annotation_id")
    )
    let annotationTrainID = try #require(
        manual.value(row: 0, column: "train_id")
    )
    let linkedValue = try #require(
        manual.value(row: 0, column: "linked_isi_uids")
    )
    let linkedUIDs = try #require(
        STPDCanonicalValue.parseStringList(linkedValue)
    )
    let isiUIDIndex = try #require(
        finalISIs.headers.firstIndex(of: "isi_uid")
    )
    let isiTrainIndex = try #require(
        finalISIs.headers.firstIndex(of: "train_id")
    )
    let extraRowIndex = try #require(finalISIs.rows.indices.first {
        let row = finalISIs.rows[$0]
        return row[isiTrainIndex] != annotationTrainID &&
            !linkedUIDs.contains(row[isiUIDIndex])
    })
    let extraUID = finalISIs.rows[extraRowIndex][isiUIDIndex]
    var changedManual = try resultPackageReplacingCell(
        manual,
        column: "linked_isi_uids",
        with: STPDCanonicalValue.stringList(
            (linkedUIDs + [extraUID]).sorted()
        )
    )
    changedManual = try resultPackageRefreshingManualSemanticDigest(
        changedManual
    )
    let existingEvidenceValue = try #require(
        finalISIs.value(
            row: extraRowIndex,
            column: "review_evidence_uids"
        )
    )
    let existingEvidence = try #require(
        STPDCanonicalValue.parseStringList(existingEvidenceValue)
    )
    let changedISIs = try resultPackageReplacingCells(
        finalISIs,
        row: extraRowIndex,
        updates: [
            "review_evidence_uids": STPDCanonicalValue.stringList(
                (existingEvidence + [annotationUID]).sorted()
            ),
            "review_evidence_present": "true",
        ]
    )

    var tables = package.tables
    tables[.manualAnnotations] = changedManual
    tables[.isiLabelsFinal] = changedISIs
    #expect(throws: STPDResultPackageError.self) {
        _ = try resultPackageValidate(
            package,
            tables: tables,
            dataset: fixture.dataset,
            run: fixture.run
        )
    }
}

@Test
func resultPackageValidatorRejectsSealedRunMetadataTampering() throws {
    let fixture = resultPackageFixture()
    let package = try STPDResultPackageBuilder.build(
        .automatic(dataset: fixture.dataset, run: fixture.run)
    )
    let metadata = try #require(package.table(.runMetadata))
    let candidateCount = Int(
        try #require(metadata.value(row: 0, column: "candidate_count"))
    ) ?? 0
    let mutations: [(String, String)] = [
        ("owner_name", "Unrelated Owner"),
        ("owner_email", "other@example.invalid"),
        ("build_identifier", "ffffffffffffffffffffffffffffffffffffffff"),
        ("build_identifier_kind", "locally_attested"),
        ("build_reproducibility_attested", "true"),
        ("source_mode", STPDResultPackageSourceMode.reviewed.rawValue),
        ("dataset_name", "different-dataset"),
        ("dataset_source", "different-source"),
        ("task_event_source_digest", "digest_tampered"),
        ("candidate_count", String(candidateCount + 1)),
    ]

    for (column, value) in mutations {
        var tables = package.tables
        tables[.runMetadata] = try resultPackageReplacingCell(
            metadata,
            column: column,
            with: value
        )
        #expect(throws: STPDResultPackageError.self) {
            _ = try resultPackageValidate(
                package,
                tables: tables,
                dataset: fixture.dataset,
                run: fixture.run
            )
        }
    }
}

@Test
func resultPackageValidatorRejectsCoherentlyRepeatedFalseQCSnapshot()
    throws {
    let fixture = resultPackageFixture()
    let package = try STPDResultPackageBuilder.build(
        .automatic(dataset: fixture.dataset, run: fixture.run)
    )
    let finalISIs = try #require(package.table(.isiLabelsFinal))
    let trainID = try #require(
        finalISIs.value(row: 0, column: "train_id")
    )
    let validCount = Int(try #require(
        finalISIs.value(row: 0, column: "train_qc_valid_isi_count")
    )) ?? 0
    let mutations: [[String: String]] = [
        ["train_qc_valid_isi_count": String(validCount + 1)],
        ["train_qc_artifact_fraction": "0.5"],
        ["train_qc_refractory_suspect_fraction": "0.5"],
    ]

    for updates in mutations {
        var tables = package.tables
        tables[.isiLabelsFinal] = try resultPackageReplacingCells(
            finalISIs,
            whereColumn: "train_id",
            equals: trainID,
            updates: updates
        )
        #expect(throws: STPDResultPackageError.self) {
            _ = try resultPackageValidate(
                package,
                tables: tables,
                dataset: fixture.dataset,
                run: fixture.run
            )
        }
    }
}

@Test
func resultPackageValidatorRejectsProfileSentinelReinterpretedAsEvent()
    throws {
    let fixture = resultPackageFixture()
    let package = try STPDResultPackageBuilder.build(
        .automatic(dataset: fixture.dataset, run: fixture.run)
    )
    let diagnostic = try #require(
        package.table(.candidateLedgerDiagnostic)
    )
    let labelIndex = try #require(
        diagnostic.headers.firstIndex(of: "final_label")
    )
    let profileRowIndex = try #require(diagnostic.rows.indices.first {
        diagnostic.rows[$0][labelIndex] ==
            ClassicAnchorLabel.profile.rawValue
    })
    var tables = package.tables
    tables[.candidateLedgerDiagnostic] = try resultPackageReplacingCell(
        diagnostic,
        row: profileRowIndex,
        column: "candidate_class",
        with: "burst"
    )

    #expect(throws: STPDResultPackageError.self) {
        _ = try resultPackageValidate(
            package,
            tables: tables,
            dataset: fixture.dataset,
            run: fixture.run
        )
    }
}

@Test
func resultPackageValidatorRejectsWholeReviewRowDeletionAfterEvidenceCleanup()
    throws {
    let fixture = resultPackageFixture()
    let automatic = STPDResultPackageInput.automatic(
        dataset: fixture.dataset,
        run: fixture.run
    )
    let candidateIDsByTrain = Dictionary(
        grouping: automatic.finalISILabelRows.filter {
            $0.isiIndex > 0 && !$0.autoCandidateID.isEmpty
        },
        by: \.trainID
    ).mapValues { rows in
        Array(Set(rows.map(\.autoCandidateID))).sorted()
    }
    let trainIDs = candidateIDsByTrain.keys.sorted()
    let primaryCandidateID = try #require(
        trainIDs.first.flatMap { candidateIDsByTrain[$0]?.first }
    )
    let secondaryCandidateID = try #require(
        trainIDs.dropFirst().first.flatMap {
            candidateIDsByTrain[$0]?.first
        }
    )

    for (ordinal, status) in [
        STPDCandidateReviewStatus.accepted,
        .rejected,
        .modified,
    ].enumerated() {
        let primaryReview = STPDCandidateReviewInput(
            sourceCandidateID: primaryCandidateID,
            status: status,
            reviewer: "Primary independent reviewer",
            note: "primary \(status.rawValue)",
            reviewedAt: Date(timeIntervalSince1970: 700 + Double(ordinal)),
            reviewedRunID: fixture.run.runIdentity.runID
        )
        let secondaryReview = STPDCandidateReviewInput(
            sourceCandidateID: secondaryCandidateID,
            status: .accepted,
            reviewer: "Secondary independent reviewer",
            note: "authority remains present",
            reviewedAt: Date(timeIntervalSince1970: 800 + Double(ordinal)),
            reviewedRunID: fixture.run.runIdentity.runID
        )
        let expectedReviews = [primaryReview, secondaryReview]
        let input = try STPDResultPackageInput.snapshot(
            dataset: fixture.dataset,
            run: fixture.run,
            candidateReviews: expectedReviews
        )
        let package = try STPDResultPackageBuilder.build(input)
        let reviews = try #require(package.table(.reviewStatus))
        let sourceIndex = try #require(
            reviews.headers.firstIndex(of: "source_candidate_id")
        )
        let reviewUIDIndex = try #require(
            reviews.headers.firstIndex(of: "review_uid")
        )
        let scopeIndex = try #require(
            reviews.headers.firstIndex(of: "link_scope")
        )
        let primaryRow = try #require(reviews.rows.first {
            $0[sourceIndex] == primaryCandidateID
        })
        let primaryReviewUID = primaryRow[reviewUIDIndex]
        let secondaryRow = try #require(reviews.rows.first {
            $0[sourceIndex] == secondaryCandidateID
        })
        #expect(secondaryRow[scopeIndex] == "public_projection")

        let finalISIs = try #require(package.table(.isiLabelsFinal))
        let finalEvents = try #require(package.table(.eventsFinal))
        var tables = package.tables
        tables[.reviewStatus] = try resultPackageRemovingRows(
            reviews,
            whereColumn: "source_candidate_id",
            equals: primaryCandidateID
        )
        tables[.isiLabelsFinal] =
            try resultPackageRemovingEvidenceUIDFromAllRows(
                finalISIs,
                evidenceUID: primaryReviewUID
            )
        tables[.eventsFinal] =
            try resultPackageRemovingEvidenceUIDFromAllRows(
                finalEvents,
                evidenceUID: primaryReviewUID
            )

        #expect(throws: STPDResultPackageError.self) {
            _ = try resultPackageValidate(
                package,
                tables: tables,
                dataset: fixture.dataset,
                run: fixture.run,
                expectedCandidateReviews: expectedReviews
            )
        }
    }
}

@Test
func resultPackageValidatorRejectsCoordinatedAutomaticProjectionRewrite()
    throws {
    let fixture = resultPackageFixture()
    let package = try STPDResultPackageBuilder.build(
        .automatic(dataset: fixture.dataset, run: fixture.run)
    )
    let finalISIs = try #require(package.table(.isiLabelsFinal))
    let candidates = try #require(package.table(.candidateLedger))

    let isiTrainIndex = try #require(
        finalISIs.headers.firstIndex(of: "train_id")
    )
    let isiPatternIndex = try #require(
        finalISIs.headers.firstIndex(of: "auto_pattern")
    )
    let isiSourceIndex = try #require(
        finalISIs.headers.firstIndex(of: "auto_source_candidate_id")
    )
    let candidateUIDIndex = try #require(
        candidates.headers.firstIndex(of: "candidate_uid")
    )
    let candidateSourceIndex = try #require(
        candidates.headers.firstIndex(of: "source_candidate_id")
    )
    let candidateTrainIndex = try #require(
        candidates.headers.firstIndex(of: "train_id")
    )
    let candidateLabelIndex = try #require(
        candidates.headers.firstIndex(of: "final_label")
    )

    var attack: (
        isiRow: Int,
        replacementSourceID: String,
        replacementUID: String,
        replacementPattern: String,
        replacementSubtype: String
    )?
    for rowIndex in finalISIs.rows.indices {
        let row = finalISIs.rows[rowIndex]
        guard !row[isiSourceIndex].isEmpty else {
            continue
        }
        for candidateRow in candidates.rows {
            guard candidateRow[candidateTrainIndex] == row[isiTrainIndex],
                  candidateRow[candidateSourceIndex] != row[isiSourceIndex],
                  candidateRow[candidateLabelIndex] != row[isiPatternIndex] else {
                continue
            }
            let replacementPattern = candidateRow[candidateLabelIndex]
            let replacementSubtype =
                replacementPattern == ManualAnnotationLabel.tonic.rawValue
                ? "classic"
                : ""
            attack = (
                isiRow: rowIndex,
                replacementSourceID: candidateRow[candidateSourceIndex],
                replacementUID: candidateRow[candidateUIDIndex],
                replacementPattern: replacementPattern,
                replacementSubtype: replacementSubtype
            )
            break
        }
        if attack != nil { break }
    }
    let rewrite = try #require(attack)

    var tables = package.tables
    tables[.isiLabelsFinal] = try resultPackageReplacingCells(
        finalISIs,
        row: rewrite.isiRow,
        updates: [
            "auto_pattern": rewrite.replacementPattern,
            "auto_subtype": rewrite.replacementSubtype,
            "auto_source_candidate_id": rewrite.replacementSourceID,
            "auto_candidate_uid": rewrite.replacementUID,
        ]
    )
    #expect(throws: STPDResultPackageError.self) {
        _ = try resultPackageValidate(
            package,
            tables: tables,
            dataset: fixture.dataset,
            run: fixture.run
        )
    }
}

@Test
func resultPackageValidatorRejectsEventSourceAndDiagnosticAuthorityRewrites()
    throws {
    let fixture = resultPackageFixture()
    let package = try STPDResultPackageBuilder.build(
        .automatic(dataset: fixture.dataset, run: fixture.run)
    )
    let events = try #require(package.table(.eventsFinal))
    let diagnostics = try #require(
        package.table(.candidateDiagnosticAudit)
    )
    let evidenceKindIndex = try #require(
        diagnostics.headers.firstIndex(of: "evidence_kind")
    )
    let eventSourceRow = try #require(diagnostics.rows.indices.first {
        diagnostics.rows[$0][evidenceKindIndex] == "event_source"
    })

    do {
        var tables = package.tables
        tables[.eventsFinal] = try resultPackageReplacingCell(
            events,
            column: "source_event_ids",
            with: STPDCanonicalValue.stringList([])
        )
        #expect(throws: STPDResultPackageError.self) {
            _ = try resultPackageValidate(
                package,
                tables: tables,
                dataset: fixture.dataset,
                run: fixture.run
            )
        }
    }

    do {
        let eventUID = try #require(
            diagnostics.value(row: eventSourceRow, column: "event_uid")
        )
        var tables = package.tables
        tables[.candidateDiagnosticAudit] =
            try resultPackageRemovingRows(
                diagnostics,
                whereColumn: "event_uid",
                equals: eventUID
            )
        #expect(throws: STPDResultPackageError.self) {
            _ = try resultPackageValidate(
                package,
                tables: tables,
                dataset: fixture.dataset,
                run: fixture.run
            )
        }
    }

    do {
        var tables = package.tables
        tables[.candidateDiagnosticAudit] =
            try resultPackageReplacingCell(
                diagnostics,
                row: eventSourceRow,
                column: "stage_id",
                with: "stage_forged"
            )
        #expect(throws: STPDResultPackageError.self) {
            _ = try resultPackageValidate(
                package,
                tables: tables,
                dataset: fixture.dataset,
                run: fixture.run
            )
        }
    }
}

@Test
func resultPackageValidatorRejectsInactiveManualAnnotationDeletionFromSealedInput()
    throws {
    let fixture = resultPackageFixture()
    let olderID = UUID(
        uuidString: "50000000-0000-0000-0000-000000000005"
    )!
    let newerID = UUID(
        uuidString: "60000000-0000-0000-0000-000000000006"
    )!
    let geometry = try resultPackageManualTonicAnnotation(
        dataset: fixture.dataset,
        run: fixture.run,
        id: olderID
    )
    let older = ManualAnnotation(
        id: olderID,
        trainID: geometry.trainID,
        label: .tonic,
        startSec: geometry.startSec,
        endSec: geometry.endSec,
        createdAt: Date(timeIntervalSince1970: 10),
        updatedAt: Date(timeIntervalSince1970: 20)
    )
    let newer = ManualAnnotation(
        id: newerID,
        trainID: geometry.trainID,
        label: .tonic,
        startSec: geometry.startSec,
        endSec: geometry.endSec,
        createdAt: Date(timeIntervalSince1970: 30),
        updatedAt: Date(timeIntervalSince1970: 40)
    )
    let input = try resultPackageReviewedInput(
        dataset: fixture.dataset,
        run: fixture.run,
        manualAnnotations: [newer, older]
    )
    let package = try STPDResultPackageBuilder.build(input)
    let manual = try #require(package.table(.manualAnnotations))
    let scopeIndex = try #require(
        manual.headers.firstIndex(of: "link_scope")
    )
    let uuidIndex = try #require(
        manual.headers.firstIndex(of: "source_annotation_uuid")
    )
    let inactiveRow = try #require(manual.rows.first {
        $0[scopeIndex] == "inactive_or_superseded"
    })
    #expect(inactiveRow[uuidIndex] == olderID.uuidString.lowercased())

    var tables = package.tables
    tables[.manualAnnotations] = try resultPackageRemovingRows(
        manual,
        whereColumn: "source_annotation_uuid",
        equals: olderID.uuidString.lowercased()
    )

    #expect(throws: STPDResultPackageError.self) {
        _ = try resultPackageValidate(
            package,
            tables: tables,
            dataset: fixture.dataset,
            run: fixture.run,
            expectedManualAnnotations: input.manualAnnotations
        )
    }
}

@Test
func resultPackageValidatorRejectsCoherentManualSnapshotRewrite()
    throws {
    let fixture = resultPackageFixture()
    let annotation = try resultPackageManualTonicAnnotation(
        dataset: fixture.dataset,
        run: fixture.run,
        id: UUID(
            uuidString: "70000000-0000-0000-0000-000000000007"
        )!,
        note: "sealed original note"
    )
    let input = try resultPackageReviewedInput(
        dataset: fixture.dataset,
        run: fixture.run,
        manualAnnotations: [annotation]
    )
    let package = try STPDResultPackageBuilder.build(input)
    let manual = try #require(package.table(.manualAnnotations))
    var rewritten = try resultPackageReplacingCell(
        manual,
        column: "note",
        with: "coherently rewritten note"
    )
    rewritten = try resultPackageRefreshingManualSemanticDigest(rewritten)

    var tables = package.tables
    tables[.manualAnnotations] = rewritten
    #expect(throws: STPDResultPackageError.self) {
        _ = try resultPackageValidate(
            package,
            tables: tables,
            dataset: fixture.dataset,
            run: fixture.run,
            expectedManualAnnotations: input.manualAnnotations
        )
    }
}

@Test
func resultPackageValidatorRejectsDiagnosticCandidatePopulationDeletion()
    throws {
    let fixture = resultPackageFixture()
    let package = try STPDResultPackageBuilder.build(
        .automatic(dataset: fixture.dataset, run: fixture.run)
    )
    let diagnosticLedger = try #require(
        package.table(.candidateLedgerDiagnostic)
    )
    let finalLabelIndex = try #require(
        diagnosticLedger.headers.firstIndex(of: "final_label")
    )
    let sourceIDIndex = try #require(
        diagnosticLedger.headers.firstIndex(of: "source_candidate_id")
    )
    let profileRow = try #require(diagnosticLedger.rows.first {
        $0[finalLabelIndex] == ClassicAnchorLabel.profile.rawValue
    })
    let sourceCandidateID = profileRow[sourceIDIndex]

    var tables = package.tables
    for tableKind in [
        STPDResultTable.candidateLedgerDiagnostic,
        .candidateFeaturesDiagnostic,
        .finalDecisionsDiagnostic,
    ] {
        let table = try #require(package.table(tableKind))
        tables[tableKind] = try resultPackageRemovingRows(
            table,
            whereColumn: "source_candidate_id",
            equals: sourceCandidateID
        )
    }

    #expect(throws: STPDResultPackageError.self) {
        _ = try resultPackageValidate(
            package,
            tables: tables,
            dataset: fixture.dataset,
            run: fixture.run
        )
    }
}

@Test
func resultPackageValidatorRejectsSuppliedDiagnosticDeletion()
    throws {
    let fixture = resultPackageFixture()
    let sourceCandidateID = try #require(fixture.run.candidates.first?.id)
    let supplied = [
        STPDCandidateDiagnosticInput(
            sourceCandidateID: sourceCandidateID,
            stageName: "sealed_diagnostic",
            stageOrdinal: 1,
            status: "observed",
            details: "sealed diagnostic details"
        ),
    ]
    let input = try STPDResultPackageInput.snapshot(
        dataset: fixture.dataset,
        run: fixture.run,
        candidateDiagnostics: supplied
    )
    let package = try STPDResultPackageBuilder.build(input)
    let diagnostics = try #require(
        package.table(.candidateDiagnosticAudit)
    )
    var tables = package.tables
    tables[.candidateDiagnosticAudit] = try resultPackageRemovingRows(
        diagnostics,
        whereColumn: "evidence_kind",
        equals: "candidate_diagnostic"
    )

    #expect(throws: STPDResultPackageError.self) {
        _ = try resultPackageValidate(
            package,
            tables: tables,
            dataset: fixture.dataset,
            run: fixture.run,
            expectedCandidateDiagnostics: supplied
        )
    }
}

@Test
func resultPackageValidatorRejectsHFSArbitrationDeletionAndMutation()
    throws {
    let fixture = resultPackageFixture()
    let train = try #require(fixture.dataset.trains.first)
    let candidateID = "sealed-hfs::state-split::1-3"
    let run = resultPackageRun(
        from: fixture.run,
        appending: resultPackageHFSCandidate(
            train: train,
            id: candidateID
        ),
        auditRow: resultPackageHFSRow(
            train: train,
            candidateID: candidateID,
            rootID: "sealed-hfs",
            rowID: "sealed-hfs-audit"
        )
    )
    let package = try STPDResultPackageBuilder.build(
        .automatic(dataset: fixture.dataset, run: run)
    )
    let hfs = try #require(package.table(.hfsBurstArbitrationAudit))

    do {
        var tables = package.tables
        tables[.hfsBurstArbitrationAudit] = try resultPackageRemovingRows(
            hfs,
            whereColumn: "audit_hfs_candidate_id",
            equals: candidateID
        )
        #expect(throws: STPDResultPackageError.self) {
            _ = try resultPackageValidate(
                package,
                tables: tables,
                dataset: fixture.dataset,
                run: run
            )
        }
    }

    do {
        var tables = package.tables
        tables[.hfsBurstArbitrationAudit] =
            try resultPackageReplacingCell(
                hfs,
                column: "audit_decision_reason",
                with: "forged_hfs_decision_reason"
            )
        #expect(throws: STPDResultPackageError.self) {
            _ = try resultPackageValidate(
                package,
                tables: tables,
                dataset: fixture.dataset,
                run: run
            )
        }
    }
}

@Test
func resultPackageValidatorRejectsEventDetectorProvenanceRewrites()
    throws {
    let fixture = resultPackageFixture()
    let package = try STPDResultPackageBuilder.build(
        .automatic(dataset: fixture.dataset, run: fixture.run)
    )
    let events = try #require(package.table(.eventsFinal))
    let priority = Int(events.value(row: 0, column: "priority") ?? "") ?? 0
    let mutations: [(column: String, value: String)] = [
        ("semantic_track", "forged_semantic_track"),
        ("event_track_class", "forged_event_track_class"),
        ("lock_level", "forged_lock_level"),
        ("score", "0.987654321"),
        ("priority", String(priority + 1)),
        ("audit_review_status", "forged_review_status"),
        ("decision_path", "forged_event_decision_path"),
    ]

    for mutation in mutations {
        var tables = package.tables
        tables[.eventsFinal] = try resultPackageReplacingCell(
            events,
            column: mutation.column,
            with: mutation.value
        )
        #expect(throws: STPDResultPackageError.self) {
            _ = try resultPackageValidate(
                package,
                tables: tables,
                dataset: fixture.dataset,
                run: fixture.run
            )
        }
    }
}

// MARK: - Phase 2.2B — Data_quality_QC (schema v3) permanent contract tests

private let dataQualityQCExpectedHeaders = [
    "run_id", "settings_digest", "dataset_digest", "train_id", "train_name",
    "spike_count", "raw_isi_count", "valid_isi_count", "artifact_isi_count", "artifact_fraction",
    "refractory_suspect_isi_count", "refractory_suspect_fraction", "zero_or_negative_isi_count",
    "duplicate_timestamp_count", "dropped_duplicate_timestamp_count", "input_was_unsorted",
    "input_nonmonotonic_step_count", "duplicate_timestamp_policy", "artifact_threshold_sec",
    "refractory_suspect_threshold_sec", "firing_rate_hz", "duration_sec", "raw_min_isi_sec",
    "min_valid_isi_sec", "artifact_min_isi_sec", "median_isi_sec", "max_isi_sec",
    "warning_level", "warning_message", "percentile_status",
]

private func dataQualityQCTimestamps(_ isis: [Double]) -> [Double] {
    isis.reduce(into: [0.0]) { values, isi in values.append((values.last ?? 0) + isi) }
}

private func dataQualityQCSettingsSnapshot() -> DetectionRunSettingsSnapshot {
    DetectionRunSettingsSnapshot.make(
        bandSettings: TrainAdaptiveBandSettings(minValidISISec: 0.001, histogramBinWidthSec: 0.005),
        qualitySettings: SpikeQualitySettings(),
        refractoryAction: .warnOnly,
        stateTuning: StatePatternDetectorTuning(),
        detectorParameters: .defaults,
        manualThresholdProfile: .automatic,
        frameworkPolicy: .legacyCompatible,
        useAdaptiveV2Canonicalization: false,
        manualThresholdScope: .allTrains
    )
}

private func dataQualityQCColumn(_ table: STPDResultTableData, _ name: String, row: Int) throws -> String {
    let index = try #require(table.headers.firstIndex(of: name))
    try #require(table.rows.indices.contains(row))
    return table.rows[row][index]
}

private func dataQualityQCRowByTrain(_ table: STPDResultTableData) throws -> [String: [String]] {
    let trainIndex = try #require(table.headers.firstIndex(of: "train_id"))
    var map: [String: [String]] = [:]
    for row in table.rows { map[row[trainIndex]] = row }
    return map
}

@Test func dataQualityQCHeaderIsExactlyThirtyOrderedColumns() throws {
    let fixture = resultPackageFixture()
    let package = try STPDResultPackageBuilder.build(.automatic(dataset: fixture.dataset, run: fixture.run))
    let table = try #require(package.tables[.dataQualityQC])
    #expect(table.headers == dataQualityQCExpectedHeaders)
    #expect(table.headers.count == 30)
    #expect(table.columnDefinitions.map(\.name) == dataQualityQCExpectedHeaders)
}

@Test func dataQualityQCHasExactlyOneRowPerTrainInAscendingTrainIDOrder() throws {
    let fixture = resultPackageFixture()
    let package = try STPDResultPackageBuilder.build(.automatic(dataset: fixture.dataset, run: fixture.run))
    let table = try #require(package.tables[.dataQualityQC])
    #expect(table.rows.count == fixture.dataset.trains.count)
    let trainIDs = try (0 ..< table.rows.count).map { try dataQualityQCColumn(table, "train_id", row: $0) }
    #expect(Set(trainIDs) == Set(fixture.dataset.trains.map(\.id)))
    #expect(trainIDs == trainIDs.sorted())
}

@Test func dataQualityQCProjectsEveryFieldFromAuthoritativeQC() throws {
    let fixture = resultPackageFixture()
    let package = try STPDResultPackageBuilder.build(.automatic(dataset: fixture.dataset, run: fixture.run))
    let table = try #require(package.tables[.dataQualityQC])
    let rowsByTrain = try dataQualityQCRowByTrain(table)
    let headerIndex = Dictionary(uniqueKeysWithValues: table.headers.enumerated().map { (offset, name) in (name, offset) })
    for train in fixture.dataset.trains {
        let quality = SpikeQualityAnalyzer.quality(for: train, settings: fixture.run.qualitySettings)
        let row = try #require(rowsByTrain[train.id])
        func cell(_ name: String) throws -> String { row[try #require(headerIndex[name])] }
        #expect(try cell("train_name") == quality.trainName)
        #expect(try cell("spike_count") == String(quality.spikeCount))
        #expect(try cell("raw_isi_count") == String(max(quality.spikeCount - 1, 0)))
        #expect(try cell("valid_isi_count") == String(quality.validISICount))
        #expect(try cell("artifact_isi_count") == String(quality.artifactISICount))
        #expect(try cell("artifact_fraction") == STPDCanonicalValue.double(quality.artifactFraction))
        #expect(try cell("refractory_suspect_isi_count") == String(quality.refractorySuspectISICount))
        #expect(try cell("refractory_suspect_fraction") == STPDCanonicalValue.double(quality.refractorySuspectFraction))
        #expect(try cell("firing_rate_hz") == STPDCanonicalValue.double(quality.firingRateHz))
        #expect(try cell("duration_sec") == STPDCanonicalValue.double(quality.durationSec))
        #expect(try cell("median_isi_sec") == STPDCanonicalValue.double(quality.medianISISec))
        #expect(try cell("min_valid_isi_sec") == STPDCanonicalValue.double(quality.minValidISISec))
        #expect(try cell("max_isi_sec") == STPDCanonicalValue.double(quality.maxISISec))
        #expect(try cell("input_was_unsorted") == STPDCanonicalValue.bool(quality.inputWasUnsorted))
        #expect(try cell("duplicate_timestamp_policy") == quality.duplicateTimestampPolicy.rawValue)
        #expect(try cell("warning_level") == quality.warningLevel.rawValue)
        #expect(try cell("warning_message") == quality.warningMessage)
        #expect(try cell("percentile_status") == quality.percentileStatus)
    }
}

@Test func dataQualityQCThresholdsComeFromRunSettings() throws {
    let fixture = resultPackageFixture()
    let package = try STPDResultPackageBuilder.build(.automatic(dataset: fixture.dataset, run: fixture.run))
    let table = try #require(package.tables[.dataQualityQC])
    for row in 0 ..< table.rows.count {
        #expect(try dataQualityQCColumn(table, "artifact_threshold_sec", row: row)
            == STPDCanonicalValue.double(fixture.run.qualitySettings.artifactThresholdSec))
        #expect(try dataQualityQCColumn(table, "refractory_suspect_threshold_sec", row: row)
            == STPDCanonicalValue.double(fixture.run.qualitySettings.refractorySuspectThresholdSec))
    }
}

private func resultPackageQCBoundaryFixture(
    isiSec: Double,
    artifactThresholdSec: Double,
    refractoryThresholdSec: Double
) -> (
    dataset: SpikeDataset,
    run: ClassicAnchorDetectionRun,
    qualitySettings: SpikeQualitySettings
) {
    let qualitySettings = SpikeQualitySettings(
        artifactThresholdSec: artifactThresholdSec,
        refractorySuspectThresholdSec: refractoryThresholdSec
    )
    let dataset = SpikeDataset(
        name: "result-package-qc-boundary",
        sourceDescription: "result package QC boundary fixture",
        trains: [
            SpikeTrain(
                name: "qc_boundary_train",
                timestampsSec: [0, isiSec]
            ),
        ]
    )
    let run = ClassicAnchorDetectionPipeline.run(
        dataset: dataset,
        bandSettings: TrainAdaptiveBandSettings(
            minValidISISec: artifactThresholdSec,
            histogramBinWidthSec: max(artifactThresholdSec, 0.001)
        ),
        qualitySettings: qualitySettings,
        buildCommit: resultPackageBuildCommit
    )
    return (dataset, run, qualitySettings)
}

@Test func resultPackageRejectsUnresolvedArtifactToleranceBand() throws {
    let artifactThreshold = 0.001
    let artifactTolerance = max(
        1e-12,
        abs(artifactThreshold) * 1e-6
    )
    let fixture = resultPackageQCBoundaryFixture(
        isiSec: artifactThreshold - (artifactTolerance / 2),
        artifactThresholdSec: artifactThreshold,
        refractoryThresholdSec: artifactThreshold
    )
    let quality = SpikeQualityAnalyzer.quality(
        for: fixture.dataset.trains[0],
        settings: fixture.qualitySettings
    )

    // The analyzer's tolerance-aware artifact predicate and exact valid-floor predicate leave this
    // narrow interval in neither primary population. Export must fail closed rather than publish
    // raw_isi_count=1 with artifact_isi_count=0 and valid_isi_count=0.
    #expect(quality.artifactISICount == 0)
    #expect(quality.validISICount == 0)
    #expect(throws: STPDResultPackageError.self) {
        _ = try STPDResultPackageBuilder.dataQualityQCTable(
            identity: fixture.run.runIdentity,
            dataset: fixture.dataset,
            qualitySettings: fixture.qualitySettings
        )
    }
    #expect(throws: STPDResultPackageError.self) {
        _ = try STPDResultPackageBuilder.build(
            .automatic(
                dataset: fixture.dataset,
                run: fixture.run
            )
        )
    }
}

@Test func resultPackageQCClassificationPinsArtifactAndRefractoryBoundaries()
    throws
{
    struct BoundaryCase {
        let name: String
        let isiSec: Double
        let artifactThresholdSec: Double
        let refractoryThresholdSec: Double
        let artifactCount: Int
        let validCount: Int
        let refractoryCount: Int
        let isiQCClass: String
    }

    let artifactThreshold = 0.001
    let artifactTolerance = max(
        1e-12,
        abs(artifactThreshold) * 1e-6
    )
    let refractoryThreshold = 0.002
    let refractoryTolerance = max(
        1e-12,
        abs(refractoryThreshold) * 1e-6
    )
    let cases = [
        BoundaryCase(
            name: "artifact below tolerance",
            isiSec: artifactThreshold - (2 * artifactTolerance),
            artifactThresholdSec: artifactThreshold,
            refractoryThresholdSec: artifactThreshold,
            artifactCount: 1,
            validCount: 0,
            refractoryCount: 0,
            isiQCClass: "artifact_below_floor"
        ),
        BoundaryCase(
            name: "artifact threshold is valid",
            isiSec: artifactThreshold,
            artifactThresholdSec: artifactThreshold,
            refractoryThresholdSec: artifactThreshold,
            artifactCount: 0,
            validCount: 1,
            refractoryCount: 0,
            isiQCClass: "valid"
        ),
        BoundaryCase(
            name: "below refractory cutoff",
            isiSec: refractoryThreshold - (2 * refractoryTolerance),
            artifactThresholdSec: artifactThreshold,
            refractoryThresholdSec: refractoryThreshold,
            artifactCount: 0,
            validCount: 1,
            refractoryCount: 1,
            isiQCClass: "refractory_suspect"
        ),
        BoundaryCase(
            name: "inside refractory tolerance",
            isiSec: refractoryThreshold - (refractoryTolerance / 2),
            artifactThresholdSec: artifactThreshold,
            refractoryThresholdSec: refractoryThreshold,
            artifactCount: 0,
            validCount: 1,
            refractoryCount: 0,
            isiQCClass: "valid"
        ),
        BoundaryCase(
            name: "refractory threshold is valid",
            isiSec: refractoryThreshold,
            artifactThresholdSec: artifactThreshold,
            refractoryThresholdSec: refractoryThreshold,
            artifactCount: 0,
            validCount: 1,
            refractoryCount: 0,
            isiQCClass: "valid"
        ),
    ]

    for boundaryCase in cases {
        let fixture = resultPackageQCBoundaryFixture(
            isiSec: boundaryCase.isiSec,
            artifactThresholdSec: boundaryCase.artifactThresholdSec,
            refractoryThresholdSec: boundaryCase.refractoryThresholdSec
        )
        let package = try STPDResultPackageBuilder.build(
            .automatic(
                dataset: fixture.dataset,
                run: fixture.run
            )
        )
        let qcTable = try #require(package.table(.dataQualityQC))
        let isiTable = try #require(package.table(.isiLabelsFinal))
        #expect(
            qcTable.rows.count == 1,
            Comment(rawValue: boundaryCase.name)
        )
        #expect(
            isiTable.rows.count == 1,
            Comment(rawValue: boundaryCase.name)
        )
        let artifactCount = try #require(
            Int(dataQualityQCColumn(qcTable, "artifact_isi_count", row: 0))
        )
        let validCount = try #require(
            Int(dataQualityQCColumn(qcTable, "valid_isi_count", row: 0))
        )
        let refractoryCount = try #require(
            Int(
                dataQualityQCColumn(
                    qcTable,
                    "refractory_suspect_isi_count",
                    row: 0
                )
            )
        )
        let rawCount = try #require(
            Int(dataQualityQCColumn(qcTable, "raw_isi_count", row: 0))
        )
        #expect(
            artifactCount == boundaryCase.artifactCount,
            Comment(rawValue: boundaryCase.name)
        )
        #expect(
            validCount == boundaryCase.validCount,
            Comment(rawValue: boundaryCase.name)
        )
        #expect(
            refractoryCount == boundaryCase.refractoryCount,
            Comment(rawValue: boundaryCase.name)
        )
        #expect(
            artifactCount + validCount == rawCount,
            Comment(rawValue: boundaryCase.name)
        )
        #expect(
            try #require(
                resultPackageColumn(
                    isiTable,
                    "isi_qc_class"
                ).first
            ) == boundaryCase.isiQCClass,
            Comment(rawValue: boundaryCase.name)
        )
    }
}

@Test func dataQualityQCHandlesZeroSpikeAndOneSpikeTrains() throws {
    // Built at table level: the full pipeline is not required to accept degenerate/empty datasets.
    let dataset = SpikeDataset(
        name: "degenerate",
        sourceDescription: "degenerate",
        trains: [
            SpikeTrain(name: "zero_spike", timestampsSec: []),
            SpikeTrain(name: "one_spike", timestampsSec: [0.5]),
            SpikeTrain(name: "normal", timestampsSec: dataQualityQCTimestamps([0.3, 0.31, 0.29, 0.30])),
        ]
    )
    let identity = DetectionRunIdentity.make(dataset: dataset, settings: dataQualityQCSettingsSnapshot())
    let table = try STPDResultPackageBuilder.dataQualityQCTable(
        identity: identity, dataset: dataset, qualitySettings: SpikeQualitySettings()
    )
    #expect(table.rows.count == 3)
    let rowsByTrain = try dataQualityQCRowByTrain(table)
    let headerIndex = Dictionary(uniqueKeysWithValues: table.headers.enumerated().map { (offset, name) in (name, offset) })
    for name in ["zero_spike", "one_spike"] {
        let row = try #require(rowsByTrain[name])
        func cell(_ column: String) throws -> String { row[try #require(headerIndex[column])] }
        #expect(try cell("raw_isi_count") == "0")
        #expect(try cell("valid_isi_count") == "0")
        #expect(try cell("artifact_isi_count") == "0")
        // Undefined statistics serialize as the canonical empty field.
        #expect(try cell("median_isi_sec").isEmpty)
        #expect(try cell("max_isi_sec").isEmpty)
    }
    #expect(try #require(rowsByTrain["zero_spike"])[try #require(headerIndex["spike_count"])] == "0")
    #expect(try #require(rowsByTrain["one_spike"])[try #require(headerIndex["spike_count"])] == "1")
}

@Test func dataQualityQCEmptyDatasetProducesHeaderOnlyTable() throws {
    let dataset = SpikeDataset(name: "empty", sourceDescription: "empty", trains: [])
    let identity = DetectionRunIdentity.make(dataset: dataset, settings: dataQualityQCSettingsSnapshot())
    let table = try STPDResultPackageBuilder.dataQualityQCTable(
        identity: identity, dataset: dataset, qualitySettings: SpikeQualitySettings()
    )
    #expect(table.rows.isEmpty)
    #expect(table.headers == dataQualityQCExpectedHeaders)
}

@Test func dataQualityQCKeysByStableTrainIDNotDisplayName() throws {
    // Two trains share the SAME display name; SpikeDataset disambiguates their stable IDs.
    let dataset = SpikeDataset(
        name: "dup-name",
        sourceDescription: "dup",
        trains: [
            SpikeTrain(name: "shared", timestampsSec: dataQualityQCTimestamps([0.30, 0.31, 0.29, 0.30])),
            SpikeTrain(name: "shared", timestampsSec: dataQualityQCTimestamps([0.20, 0.21, 0.19, 0.20])),
        ]
    )
    let ids = dataset.trains.map(\.id)
    #expect(Set(ids).count == 2)            // distinct stable IDs
    #expect(dataset.trains.allSatisfy { $0.name == "shared" })  // identical display names
    let identity = DetectionRunIdentity.make(dataset: dataset, settings: dataQualityQCSettingsSnapshot())
    let table = try STPDResultPackageBuilder.dataQualityQCTable(
        identity: identity, dataset: dataset, qualitySettings: SpikeQualitySettings()
    )
    let rowsByTrain = try dataQualityQCRowByTrain(table)
    #expect(Set(rowsByTrain.keys) == Set(ids))
    let nameIndex = try #require(table.headers.firstIndex(of: "train_name"))
    #expect(table.rows.allSatisfy { $0[nameIndex] == "shared" })
}

@Test func dataQualityQCMatchesISILabelsTrainQCForTrainsWithISIRows() throws {
    let fixture = resultPackageFixture()
    let package = try STPDResultPackageBuilder.build(.automatic(dataset: fixture.dataset, run: fixture.run))
    let qc = try #require(package.tables[.dataQualityQC])
    let isi = try #require(package.tables[.isiLabelsFinal])
    let qcByTrain = try dataQualityQCRowByTrain(qc)
    let isiTrainIndex = try #require(isi.headers.firstIndex(of: "train_id"))
    let shared = [
        "warning_level", "warning_message", "duration_sec", "firing_rate_hz", "median_isi_sec",
        "artifact_isi_count", "artifact_fraction", "valid_isi_count", "input_was_unsorted",
        "percentile_status",
    ]
    for isiRow in isi.rows {
        let trainID = isiRow[isiTrainIndex]
        let qcRow = try #require(qcByTrain[trainID])
        for field in shared {
            let qcIndex = try #require(qc.headers.firstIndex(of: field))
            let isiIndex = try #require(isi.headers.firstIndex(of: "train_qc_\(field)"))
            #expect(qcRow[qcIndex] == isiRow[isiIndex])
        }
    }
}

@Test func dataQualityQCIgnoresSelectedTrainScope() throws {
    // The detector runs over the full dataset; manualThresholdScope only affects manual hard gates,
    // never which trains appear in QC. Both scopes must yield one QC row per dataset train.
    let dataset = resultPackageFixture().dataset
    func run(scope: ManualThresholdScope) -> ClassicAnchorDetectionRun {
        ClassicAnchorDetectionPipeline.run(
            dataset: dataset,
            bandSettings: TrainAdaptiveBandSettings(minValidISISec: 0.001, histogramBinWidthSec: 0.005),
            manualThresholdScope: scope,
            buildCommit: "phase2_2b_scope"
        )
    }
    let all = try STPDResultPackageBuilder.build(.automatic(dataset: dataset, run: run(scope: .allTrains)))
    let currentScope = ManualThresholdScope(kind: .currentTrain, trainIDs: [dataset.trains[0].id])
    let scoped = try STPDResultPackageBuilder.build(.automatic(dataset: dataset, run: run(scope: currentScope)))
    let allTrains = try dataQualityQCRowByTrain(try #require(all.tables[.dataQualityQC])).keys
    let scopedTrains = try dataQualityQCRowByTrain(try #require(scoped.tables[.dataQualityQC])).keys
    #expect(Set(allTrains) == Set(dataset.trains.map(\.id)))
    #expect(Set(scopedTrains) == Set(dataset.trains.map(\.id)))
}

/// Rebuilds a Data_quality_QC table from a real package with one cell overwritten in row 0.
private func dataQualityQCTampered(
    _ package: STPDResultPackage, column: String, value: String, appendDuplicateRow0: Bool = false
) throws -> STPDResultTableData {
    let table = try #require(package.tables[.dataQualityQC])
    var rows = table.rows
    if appendDuplicateRow0 {
        rows.append(rows[0])
    } else {
        let index = try #require(table.headers.firstIndex(of: column))
        rows[0][index] = value
    }
    return try STPDResultTableData(
        contract: table.contract, headers: table.headers,
        columnDefinitions: table.columnDefinitions, rows: rows
    )
}

@Test func dataQualityQCValidatorRejectsDuplicatePrimaryKey() throws {
    let fixture = resultPackageFixture()
    let package = try STPDResultPackageBuilder.build(.automatic(dataset: fixture.dataset, run: fixture.run))
    let isi = try #require(package.tables[.isiLabelsFinal])
    let duplicated = try dataQualityQCTampered(package, column: "", value: "", appendDuplicateRow0: true)
    #expect(throws: STPDResultPackageError.self) {
        try STPDResultPackageValidator.validateDataQualityQC(
            identity: package.identity, table: duplicated, isiTable: isi,
            expectedTrainIDs: nil, expectedDataset: nil, expectedRun: nil
        )
    }
}

@Test func dataQualityQCAuthorityReDerivationRejectsTamperedCellWithSealedInputs() throws {
    // Tamper a column that passes the structural per-row checks and is NOT one of the ISI-shared
    // fields (so the cross-table check does not fire): the authoritative byte re-derivation, which
    // only runs when the sealed dataset+run are supplied, must reject it.
    let fixture = resultPackageFixture()
    let package = try STPDResultPackageBuilder.build(.automatic(dataset: fixture.dataset, run: fixture.run))
    let isi = try #require(package.tables[.isiLabelsFinal])
    let tampered = try dataQualityQCTampered(package, column: "spike_count", value: "999")
    #expect(throws: STPDResultPackageError.self) {
        try STPDResultPackageValidator.validateDataQualityQC(
            identity: package.identity, table: tampered, isiTable: isi,
            expectedTrainIDs: Set(fixture.dataset.trains.map(\.id)),
            expectedDataset: fixture.dataset, expectedRun: fixture.run
        )
    }
}

@Test func dataQualityQCValidatorRejectsUnknownTrain() throws {
    let fixture = resultPackageFixture()
    let package = try STPDResultPackageBuilder.build(.automatic(dataset: fixture.dataset, run: fixture.run))
    let isi = try #require(package.tables[.isiLabelsFinal])
    let table = try #require(package.tables[.dataQualityQC])
    #expect(throws: STPDResultPackageError.self) {
        try STPDResultPackageValidator.validateDataQualityQC(
            identity: package.identity, table: table, isiTable: isi,
            expectedTrainIDs: ["a-train-that-is-not-in-the-dataset"],
            expectedDataset: nil, expectedRun: nil
        )
    }
}

@Test func dataQualityQCValidatorRejectsNegativeCountsAndOutOfRangeFractions() throws {
    let fixture = resultPackageFixture()
    let package = try STPDResultPackageBuilder.build(.automatic(dataset: fixture.dataset, run: fixture.run))
    let isi = try #require(package.tables[.isiLabelsFinal])
    let ids = Set(fixture.dataset.trains.map(\.id))
    // Canonical table construction rejects invalid primitive ranges before the
    // cross-column validator runs.
    #expect(throws: STPDResultPackageError.self) {
        _ = try dataQualityQCTampered(package, column: "valid_isi_count", value: "-1")
    }
    #expect(throws: STPDResultPackageError.self) {
        _ = try dataQualityQCTampered(package, column: "artifact_fraction", value: "2.0")
    }
    // valid + artifact exceeding raw.
    let tooMany = try dataQualityQCTampered(package, column: "artifact_isi_count", value: "9999")
    #expect(throws: STPDResultPackageError.self) {
        try STPDResultPackageValidator.validateDataQualityQC(
            identity: package.identity, table: tooMany, isiTable: isi,
            expectedTrainIDs: ids, expectedDataset: nil, expectedRun: nil
        )
    }
}

@Test func dataQualityQCValidatorRejectsIntegerOverflowWithoutTrapping() throws {
    let fixture = resultPackageFixture()
    let package = try STPDResultPackageBuilder.build(
        .automatic(dataset: fixture.dataset, run: fixture.run)
    )
    let original = try #require(package.tables[.dataQualityQC])
    let isi = try #require(package.tables[.isiLabelsFinal])

    func table(mutating changes: [String: String]) throws
        -> STPDResultTableData {
        var rows = original.rows
        for (column, value) in changes {
            let index = try #require(original.headers.firstIndex(of: column))
            rows[0][index] = value
        }
        return try STPDResultTableData(
            contract: original.contract,
            headers: original.headers,
            columnDefinitions: original.columnDefinitions,
            rows: rows
        )
    }

    func overflowRow(
        basedOn row: [String],
        trainID: String,
        trainName: String,
        spikeCount: Int,
        rawISICount: Int
    ) throws -> [String] {
        var row = row
        for (column, value) in [
            ("train_id", trainID),
            ("train_name", trainName),
            ("spike_count", String(spikeCount)),
            ("raw_isi_count", String(rawISICount)),
            ("valid_isi_count", "0"),
            ("artifact_isi_count", "0"),
            ("artifact_fraction", "0"),
            ("refractory_suspect_isi_count", "0"),
            ("refractory_suspect_fraction", "0"),
        ] {
            let index = try #require(original.headers.firstIndex(of: column))
            row[index] = value
        }
        return row
    }

    let overflowingPopulation = try STPDResultTableData(
        contract: original.contract,
        headers: original.headers,
        columnDefinitions: original.columnDefinitions,
        rows: [
            try overflowRow(
                basedOn: try #require(original.rows.first),
                trainID: "a-overflow-base",
                trainName: "overflow_base",
                spikeCount: Int.max,
                rawISICount: Int.max - 1
            ),
            try overflowRow(
                basedOn: try #require(original.rows.dropFirst().first),
                trainID: "b-overflow-addend",
                trainName: "overflow_addend",
                spikeCount: 2,
                rawISICount: 1
            ),
        ]
    )
    do {
        try STPDResultPackageValidator.validateDataQualityQC(
            identity: package.identity,
            table: overflowingPopulation,
            isiTable: isi,
            expectedTrainIDs: nil,
            expectedDataset: nil,
            expectedRun: nil
        )
        Issue.record("expected materialized spike-population overflow")
    } catch STPDResultPackageError.invalidTable(let table, let reason) {
        #expect(table == STPDResultTable.dataQualityQC.rawValue)
        #expect(reason == "materialized spike population overflows Int")
    } catch {
        Issue.record("unexpected overflow error: \(error)")
    }

    let overflowingISIArithmetic = try table(
        mutating: [
            "spike_count": String(Int.max),
            "raw_isi_count": String(Int.max - 1),
            "valid_isi_count": String(Int.max - 1),
            "artifact_isi_count": String(Int.max),
        ]
    )
    do {
        try STPDResultPackageValidator.validateDataQualityQC(
            identity: package.identity,
            table: overflowingISIArithmetic,
            isiTable: isi,
            expectedTrainIDs: nil,
            expectedDataset: nil,
            expectedRun: nil
        )
        Issue.record("expected overflow-safe valid/artifact ISI rejection")
    } catch STPDResultPackageError.invalidTable(let table, let reason) {
        #expect(table == STPDResultTable.dataQualityQC.rawValue)
        #expect(reason == "valid+artifact ISI exceeds raw ISI slots")
    } catch {
        Issue.record("unexpected ISI arithmetic error: \(error)")
    }
}

@Test func schemaTableSetGateRecognizesExactV2V3AndV4Layouts() throws {
    let v4All = Set(STPDResultTable.allCases.map(\.rawValue))
    let v3Tables = Set(
        STPDResultTable.allCases
            .filter { $0 != .manualAnnotationImportApprovals }
            .map(\.rawValue)
    )
    let v2Tables = Set(
        STPDResultTable.allCases
            .filter {
                $0 != .dataQualityQC &&
                    $0 != .manualAnnotationImportApprovals
            }
            .map(\.rawValue)
    )

    // Historical table-set recognition is exact; this is not an on-disk legacy package reader.
    #expect(throws: Never.self) {
        try STPDResultPackageValidator.validateSchemaTableSet(
            schemaVersion: "stpd_result_package_v2", presentFileNames: v2Tables)
    }
    #expect(throws: STPDResultPackageError.self) {
        try STPDResultPackageValidator.validateSchemaTableSet(
            schemaVersion: "stpd_result_package_v2", presentFileNames: v3Tables)
    }
    #expect(throws: Never.self) {
        try STPDResultPackageValidator.validateSchemaTableSet(
            schemaVersion: "stpd_result_package_v3", presentFileNames: v3Tables)
    }
    #expect(throws: STPDResultPackageError.self) {
        try STPDResultPackageValidator.validateSchemaTableSet(
            schemaVersion: "stpd_result_package_v3", presentFileNames: v2Tables)
    }
    #expect(throws: STPDResultPackageError.self) {
        try STPDResultPackageValidator.validateSchemaTableSet(
            schemaVersion: "stpd_result_package_v3", presentFileNames: v4All)
    }
    #expect(throws: Never.self) {
        try STPDResultPackageValidator.validateSchemaTableSet(
            schemaVersion: "stpd_result_package_v4", presentFileNames: v4All)
    }
    #expect(throws: STPDResultPackageError.self) {
        try STPDResultPackageValidator.validateSchemaTableSet(
            schemaVersion: "stpd_result_package_v4", presentFileNames: v3Tables)
    }
    #expect(throws: STPDResultPackageError.self) {
        try STPDResultPackageValidator.validateSchemaTableSet(
            schemaVersion: "stpd_result_package_v99", presentFileNames: v4All)
    }
}

@Test func v4PreservesUnaffectedHistoricalTablesAndAddsManualAuthorityColumns() throws {
    // v4 intentionally extends Manual_annotations with authority_source + import_approval_id and
    // adds the approval ledger. The explicit digests below freeze the complete current projection.
    // Candidate-linked tables intentionally changed with the approved Pause-boundary and direct-state-
    // support contracts: one canonical Pause now splits the fixture's Tonic state into two independently
    // supported candidates, while brief Burst-flank interruptions remain noncanonical gap evidence. State metrics
    // now exclude only each frozen Burst core (not its wider envelope) and record that authority explicitly.
    // Those geometry, metric, selection, audit, and UID changes necessarily propagate to final event/ISI tables.
    // Parameters, QC, manual authority, task events, and HFS audit tables remain byte-identical.
    let historicalHashes: [STPDResultTable: String] = [
        .parametersReport: "1fdef9b34fdfde5ca551aedb9a6216b94a76b3ff9e5a4202a59675d195cfa53e",
        .resolvedParameters: "41a6793b2d12200b6dda7bab78650b9d49ae0b2155a78097774f4ddb648c3fe9",
        .candidateLedger: "376f68c2c829bc20e5040c6f719c9e86737544d74afb6a46364763fe0d2761fb",
        .candidateFeatures: "f3d6e4e5cc96e2b5aabba385a332d621dbf87ee15305e83a0bceefb3e0c8198d",
        .finalDecisions: "602c999dc44584e6d933c8936c88678107c790952eb17c41b0409263f5dc926e",
        .candidateLedgerDiagnostic: "ea9c6c829b7e525a62d79746a1423b48a15c42d787c68aead7972c1f6b864c98",
        .candidateFeaturesDiagnostic: "93abda20c59cb5d5b447cd3330c38f24fa94ecfe01d36d4c460f202c0e313160",
        .finalDecisionsDiagnostic: "146f1267b6ee3ef276f3dc6e85658a5ed1b94fa0fd9f6b7179d1eb29e1df3dde",
        .eventsFinal: "ecc8a5eabba67a957871dc98f1fb7c60940f0eb251acaf15f194b13b4634aad9",
        .isiLabelsFinal: "94f6a87817741c46de9e54def97dfa2a4c626ea5515db31d0a6c250eaa85f2e9",
        .candidateDiagnosticAudit: "e992e16361acc2b74e58b32f28141423238fe63d5b60448106dcca8df1bbeb92",
        .resultConsistencyCheck: "8cc44940f534e9180783d91464ef303b0a98b704f91114d44b954824621a3dba",
        .manualAnnotations: "7f9599665876bcfbb2b0b65ca0e9a6192b8d2631f087375305cee89edb104420",
        .reviewStatus: "c2b068df2ad0734f1363868ba3c74450f97cd491069d2eb291b05cb0353f37fa",
        .hfsBurstArbitrationAudit: "a3dbbf392b3b40e04f5fcb029716a49ec17d9eab05da2fcecf60e44d22775953",
        .taskEvents: "c33a69617a3ec5bd1442b43f43c43de1d477386576a6a15d0823cfbc8bf3dbf1",
    ]
    let fixture = resultPackageFixture()
    let package = try STPDResultPackageBuilder.build(.automatic(dataset: fixture.dataset, run: fixture.run))
    let rid = package.identity.runID
    func normalizedHash(_ table: STPDResultTableData) -> String {
        let text = String(decoding: table.csvData, as: UTF8.self).replacingOccurrences(of: rid, with: "RID")
        return STPDStableIdentifier.digest(Data(text.utf8))
    }
    // These deterministic hashes freeze the complete current package projection. Dataset-relative
    // state-band evidence changes resolved settings and candidate selection; Pause boundary role and
    // discontiguous state-support geometry are identity-bearing, so candidate UIDs and their event/ISI
    // foreign-key projections necessarily change with them.
    for (table, expected) in historicalHashes {
        let data = try #require(package.tables[table])
        #expect(normalizedHash(data) == expected, "table \(table.rawValue) is no longer byte-stable")
    }
    let approvalLedger = try #require(package.tables[.manualAnnotationImportApprovals])
    #expect(approvalLedger.rowCount == 0)

    // Detector_run_metadata carries result_schema_version. The current v4 value is confined to that
    // one cell; no other metadata cell silently carries a schema-version token.
    let runMetadata = try #require(package.tables[.runMetadata])
    #expect(runMetadata.rows.count == 1)
    let versionIndex = try #require(runMetadata.headers.firstIndex(of: "result_schema_version"))
    #expect(runMetadata.rows[0][versionIndex] == "stpd_result_package_v4")
    for (index, cell) in runMetadata.rows[0].enumerated() where index != versionIndex {
        #expect(!cell.contains("stpd_result_package_v"),
                "column \(runMetadata.headers[index]) unexpectedly carries a schema-version token")
    }
    #expect(package.identity.resultSchemaVersion == "stpd_result_package_v4")
}

@Test func dataQualityQCContainsNoTimestampUUIDOrUnstableContent() throws {
    let fixture = resultPackageFixture()
    let first = try STPDResultPackageBuilder.build(.automatic(dataset: fixture.dataset, run: fixture.run))
    let second = try STPDResultPackageBuilder.build(.automatic(dataset: fixture.dataset, run: fixture.run))
    let firstTable = try #require(first.tables[.dataQualityQC])
    let secondTable = try #require(second.tables[.dataQualityQC])
    func normalized(_ t: STPDResultTableData, _ rid: String) -> String {
        String(decoding: t.csvData, as: UTF8.self).replacingOccurrences(of: rid, with: "RID")
    }
    // Byte-identical (run_id normalized) => no wall-clock, no random UUID, no unordered-set output.
    #expect(normalized(firstTable, first.identity.runID) == normalized(secondTable, second.identity.runID))
    // No column carries an ISO timestamp type.
    #expect(firstTable.columnDefinitions.allSatisfy { $0.type != .timestamp })
}

@Test func dataQualityQCIsDeterministicAcrossSeparatelyConstructedDatasets() throws {
    // Two SEPARATE constructions -> different random SpikeDataset.id, identical QC bytes (run_id norm).
    func build() throws -> (STPDResultPackage, String) {
        let fixture = resultPackageFixture()
        let package = try STPDResultPackageBuilder.build(.automatic(dataset: fixture.dataset, run: fixture.run))
        return (package, package.identity.runID)
    }
    let (a, ridA) = try build()
    let (b, ridB) = try build()
    #expect(a.identity.datasetDigest == b.identity.datasetDigest)
    let qcA = String(decoding: try #require(a.tables[.dataQualityQC]).csvData, as: UTF8.self)
        .replacingOccurrences(of: ridA, with: "RID")
    let qcB = String(decoding: try #require(b.tables[.dataQualityQC]).csvData, as: UTF8.self)
        .replacingOccurrences(of: ridB, with: "RID")
    #expect(qcA == qcB)
}

// MARK: - Phase 2.2C-B0: spike-only manual authority must come from resolved geometry, not caller cache

// A dataset whose second train has a SINGLE spike, plus its detection run. A window over that lone spike
// resolves to genuine spike-only geometry (no ISI interval; a singleton spike) — the only geometry that
// grants `.manual` spike-only source authority while owning no ISI (so it clears causal linking and
// reaches the source-mode gate). The multi-ISI first train keeps the run/quality gates well-formed.
private func resultPackageSingleSpikeFixture() -> (
    dataset: SpikeDataset,
    run: ClassicAnchorDetectionRun,
    spikeTrain: SpikeTrain
) {
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
    let normalTrain = SpikeTrain(
        name: "tonic_pause_train",
        timestampsSec: timestamps(tonicISIs)
    )
    let singleSpikeTrain = SpikeTrain(
        name: "single_spike_train",
        timestampsSec: [0.5]
    )
    let dataset = SpikeDataset(
        name: "b0-single-spike-fixture",
        sourceDescription: "phase 2.2c-b0 spike-only authority fixture",
        trains: [normalTrain, singleSpikeTrain]
    )
    let run = ClassicAnchorDetectionPipeline.run(
        dataset: dataset,
        bandSettings: TrainAdaptiveBandSettings(
            minValidISISec: 0.001,
            histogramBinWidthSec: 0.005
        ),
        buildCommit: resultPackageBuildCommit
    )
    return (dataset, run, singleSpikeTrain)
}

private func resultPackageSpikeOnlyAnnotation(
    trainID: String,
    id: UUID
) -> ManualAnnotation {
    ManualAnnotation(
        id: id,
        trainID: trainID,
        label: .tonic,
        startSec: 0.4,
        endSec: 0.6, // brackets the lone spike at 0.5; no ISI interval exists on a single-spike train
        note: "spike-only manual authority fixture",
        annotator: "Result Package Test Reviewer",
        annotatorIdentitySource: .userProvided,
        createdAt: Date(timeIntervalSince1970: 100),
        updatedAt: Date(timeIntervalSince1970: 200)
    )
}

/// Characterization (FAILS against ac09aad, PASSES after the fix): `.manual` spike-only source authority
/// must be decided from the RESOLVED geometry, never from the caller-supplied cached indices.
///
/// The annotation's authoritative time resolves to genuine spike-only geometry (single-spike train), but
/// its cached indices are FORGED to look like an ISI interval (non-spike-only). Against ac09aad the gate
/// reads the raw forged cache, concludes "not spike-only", finds no manual authority, and wrongly REJECTS
/// the build with the source-mode error. After the fix the gate reads the resolved spike-only geometry
/// and correctly admits the manual authority, so that specific rejection no longer occurs.
@Test func callerSuppliedCacheCannotOverrideResolvedSpikeOnlyManualAuthority() throws {
    let fixture = resultPackageSingleSpikeFixture()
    let automatic = STPDResultPackageInput.automatic(
        dataset: fixture.dataset,
        run: fixture.run
    )
    var forged = resultPackageSpikeOnlyAnnotation(
        trainID: fixture.spikeTrain.id,
        id: UUID(uuidString: "51500000-0000-0000-0000-000000000001")!
    )
    // Sanity: the authoritative time genuinely resolves to spike-only geometry (no ISI interval).
    let resolved = try #require(
        ManualAnnotationGeometryResolver.resolvingIndicesIfCompatible(
            forged, in: fixture.dataset.trains
        )
    )
    #expect(resolved.startISIIndex == nil && resolved.endISIIndex == nil)
    #expect(resolved.startSpikeIndex != nil && resolved.startSpikeIndex == resolved.endSpikeIndex)
    // Forge the caller-supplied cache to a NON-spike-only shape (a declared ISI interval). Only the
    // pre-fix gate, which trusts the raw cache, would treat this as "not spike-only".
    forged.startISIIndex = 1
    forged.endISIIndex = 1
    forged.startSpikeIndex = nil
    forged.endSpikeIndex = nil

    let input = STPDResultPackageInput(
        dataset: fixture.dataset,
        run: fixture.run,
        sourceMode: .manual,
        finalEvents: automatic.finalEvents,
        finalISILabelRows: automatic.finalISILabelRows,
        manualAnnotations: [forged],
        candidateReviews: [],
        reviewLinks: [],
        candidateDiagnostics: []
    )

    // No permissive catch: `build` must succeed end-to-end and any thrown error fails the test naturally.
    // Against ac09aad the pre-fix gate reads the forged non-spike-only cache, denies spike-only authority,
    // and `build` throws the source-mode error -> the test FAILS. After the fix the gate reads the resolved
    // spike-only geometry, admits authority, and `build` returns an authoritative manual package.
    let package = try STPDResultPackageBuilder.build(input)
    #expect(package.sourceMode == .manual)
    #expect(package.manifest.sourceMode == STPDResultPackageSourceMode.manual.rawValue)
}

/// Positive control (PASSES before and after the fix): the SAME spike-only annotation with an HONEST
/// cache still obtains `.manual` authority, proving the fix does not over-reject legitimate spike-only
/// manual authority. Together with the characterization above, this shows authority tracks the resolved
/// geometry rather than the caller cache in both directions.
@Test func honestResolvedSpikeOnlyAnnotationRetainsManualAuthority() throws {
    let fixture = resultPackageSingleSpikeFixture()
    let automatic = STPDResultPackageInput.automatic(
        dataset: fixture.dataset,
        run: fixture.run
    )
    let honest = try #require(
        ManualAnnotationGeometryResolver.resolvingIndicesIfCompatible(
            resultPackageSpikeOnlyAnnotation(
                trainID: fixture.spikeTrain.id,
                id: UUID(uuidString: "51500000-0000-0000-0000-000000000002")!
            ),
            in: fixture.dataset.trains
        )
    )
    #expect(honest.startISIIndex == nil && honest.endISIIndex == nil)
    #expect(honest.startSpikeIndex != nil && honest.startSpikeIndex == honest.endSpikeIndex)

    let input = STPDResultPackageInput(
        dataset: fixture.dataset,
        run: fixture.run,
        sourceMode: .manual,
        finalEvents: automatic.finalEvents,
        finalISILabelRows: automatic.finalISILabelRows,
        manualAnnotations: [honest],
        candidateReviews: [],
        reviewLinks: [],
        candidateDiagnostics: []
    )

    // Build must succeed and yield an authoritative manual package, before and after the fix; any thrown
    // error fails the test naturally (no unrelated error can make this positive test pass).
    let package = try STPDResultPackageBuilder.build(input)
    #expect(package.sourceMode == .manual)
    #expect(package.manifest.sourceMode == STPDResultPackageSourceMode.manual.rawValue)
}

/// External-contract regression: a forged spike-only cache can NEVER grant authoritative manual
/// provenance. The annotation's authoritative time resolves to ISI-OWNING geometry (it changes real ISI
/// labels), but its cached indices are forged to look spike-only. With no valid causal review links the
/// build must FAIL CLOSED. This holds identically before and after the fix: the pre-existing causal-
/// linking gate (which runs on RESOLVED geometry, before `validateSourceMode`) rejects the unlinked
/// ISI-owning annotation, so the forged spike-only cache never reaches — let alone passes — the source-
/// mode gate. This test protects that contract regardless of the source-mode-gate change.
@Test func forgedSpikeOnlyCacheCannotGrantManualAuthorityEndToEnd() throws {
    let fixture = resultPackageFixture()
    let automatic = STPDResultPackageInput.automatic(
        dataset: fixture.dataset,
        run: fixture.run
    )
    // Authoritative time resolves to a multi-ISI interval that overrides non-tonic auto labels — i.e. it
    // causally OWNS those ISIs and would require exact review links to be authoritative.
    var forged = try resultPackageMultiISITonicAnnotation(
        dataset: fixture.dataset,
        run: fixture.run,
        id: UUID(uuidString: "51500000-0000-0000-0000-000000000003")!
    )
    let resolved = try #require(
        ManualAnnotationGeometryResolver.resolvingIndicesIfCompatible(
            forged, in: fixture.dataset.trains
        )
    )
    #expect(resolved.startISIIndex != nil && resolved.endISIIndex != nil) // resolved geometry owns an ISI interval
    // Forge the caller-supplied cache to look spike-only (no ISI interval + a singleton spike).
    forged.startISIIndex = nil
    forged.endISIIndex = nil
    forged.startSpikeIndex = 0
    forged.endSpikeIndex = 0

    let input = STPDResultPackageInput(
        dataset: fixture.dataset,
        run: fixture.run,
        sourceMode: .manual,
        finalEvents: automatic.finalEvents,
        finalISILabelRows: automatic.finalISILabelRows,
        manualAnnotations: [forged],
        candidateReviews: [],
        reviewLinks: [], // no valid causal review links
        candidateDiagnostics: []
    )

    do {
        _ = try STPDResultPackageBuilder.build(input)
        Issue.record(
            "a forged spike-only cache over ISI-owning geometry with no causal links must fail closed, not produce an authoritative manual package"
        )
    } catch STPDResultPackageError.invalidInput(let message) {
        // Fail-closed at the causal-linking gate (before validateSourceMode): the resolved ISI-owning
        // annotation must link exactly to the ISIs it changes.
        #expect(
            message.contains("must link exactly to the ISIs it changes"),
            "expected the fail-closed causal-linking rejection, got: \(message)"
        )
    } catch {
        Issue.record(
            "unexpected error type (expected STPDResultPackageError.invalidInput): \(error)"
        )
    }
}

// MARK: - Phase 2.2C-B2: approved manual-import package authority

@Test
func approvedManualBatchConstructorRejectsCoverageAndSemanticDrift() throws {
    let fixture = resultPackageFixture()
    let first = try resultPackageManualTonicAnnotation(
        dataset: fixture.dataset,
        run: fixture.run,
        id: UUID(
            uuidString: "b2000000-0000-0000-0000-000000000010"
        )!,
        ordinal: 0
    )
    let second = try resultPackageManualTonicAnnotation(
        dataset: fixture.dataset,
        run: fixture.run,
        id: UUID(
            uuidString: "b2000000-0000-0000-0000-000000000011"
        )!,
        ordinal: 1
    )
    let fullReceipt = try #require(
        ManualAnnotationCSVApprovalReceipt(
            sourceFileDigest: String(repeating: "a", count: 64),
            activeDatasetDigest:
                fixture.run.runIdentity.datasetDigest,
            approvedRunID: fixture.run.runIdentity.runID,
            approvedSettingsDigest:
                fixture.run.runIdentity.settingsDigest,
            approver: "Dr. Import Approver",
            approvedAt: Date(timeIntervalSince1970: 2_000),
            sourceSchemaVersion:
                ManualAnnotationCSVExporter.identitySchemaVersion,
            sourceRunID: fixture.run.runIdentity.runID,
            sourceReviewState:
                ManualAnnotationCSVReviewState
                    .pendingConfirmation.rawValue,
            annotations: [first, second]
        )
    )
    let subsetReceipt = try #require(
        ManualAnnotationCSVApprovalReceipt(
            sourceFileDigest: String(repeating: "b", count: 64),
            activeDatasetDigest:
                fixture.run.runIdentity.datasetDigest,
            approvedRunID: fixture.run.runIdentity.runID,
            approvedSettingsDigest:
                fixture.run.runIdentity.settingsDigest,
            approver: "Dr. Import Approver",
            approvedAt: Date(timeIntervalSince1970: 2_001),
            sourceSchemaVersion:
                ManualAnnotationCSVExporter.identitySchemaVersion,
            sourceRunID: fixture.run.runIdentity.runID,
            sourceReviewState:
                ManualAnnotationCSVReviewState
                    .pendingConfirmation.rawValue,
            annotations: [first]
        )
    )

    #expect(ManualAnnotationCSVApprovedBatch(
        annotations: [first, second],
        approvalReceipt: subsetReceipt
    ) == nil)
    #expect(ManualAnnotationCSVApprovedBatch(
        annotations: [first],
        approvalReceipt: fullReceipt
    ) == nil)

    var mutated = first
    mutated.note = "changed after approval"
    #expect(ManualAnnotationCSVApprovedBatch(
        annotations: [mutated],
        approvalReceipt: subsetReceipt
    ) == nil)
}

@Test
func approvedManualImportPreflightBuildsExactV4AuthorityLedger() throws {
    let fixture = resultPackageFixture()
    let sourceAnnotation = try resultPackageManualTonicAnnotation(
        dataset: fixture.dataset,
        run: fixture.run,
        id: UUID(
            uuidString: "b2000000-0000-0000-0000-000000000001"
        )!
    )
    let approved = try resultPackageApprovedManualImport(
        dataset: fixture.dataset,
        run: fixture.run,
        annotation: sourceAnnotation
    )
    let manualTable = try #require(
        approved.package.table(.manualAnnotations)
    )
    let approvalTable = try #require(
        approved.package.table(.manualAnnotationImportApprovals)
    )
    let receipt = approved.approvedBatch.approvalReceipt
    let manualUIDs = try resultPackageColumn(
        manualTable,
        "annotation_id"
    )
    let sourceSemanticDigests = receipt.annotationBindings.map(
        \.sourceSemanticDigest
    )

    #expect(approved.package.identity.resultSchemaVersion ==
        "stpd_result_package_v4")
    #expect(approved.input.sourceMode == .manual)
    #expect(approved.input.manualAnnotationImportApprovals == [receipt])
    #expect(manualTable.rowCount == 1)
    #expect(approvalTable.rowCount == 1)
    #expect(try resultPackageColumn(manualTable, "annotator") ==
        ["Result Package Test Reviewer"])
    #expect(try resultPackageColumn(
        manualTable,
        "annotator_identity_source"
    ) == [ManualAnnotationIdentitySource.userProvided.rawValue])
    #expect(try resultPackageColumn(
        manualTable,
        "authority_source"
    ) == ["approved_import"])
    #expect(try resultPackageColumn(approvalTable, "approver") ==
        ["Dr. Import Approver"])
    #expect(try resultPackageColumn(
        approvalTable,
        "approver_identity_assurance"
    ) == [
        ManualAnnotationCSVApproverIdentityAssurance
            .userSuppliedUnauthenticatedAuditAttribution.rawValue,
    ])
    #expect(try resultPackageColumn(
        approvalTable,
        "source_file_sha256"
    ) == [ManualAnnotationCSVImporter.sourceFileDigest(
        approved.sourceData
    )])
    #expect(try resultPackageColumn(
        approvalTable,
        "approved_dataset_digest"
    ) == [approved.package.identity.datasetDigest])
    #expect(try resultPackageColumn(
        approvalTable,
        "approved_run_id"
    ) == [fixture.run.runIdentity.runID])
    #expect(try resultPackageColumn(
        approvalTable,
        "approved_settings_digest"
    ) == [fixture.run.runIdentity.settingsDigest])
    #expect(try resultPackageColumn(approvalTable, "source_run_id") ==
        [fixture.run.runIdentity.runID])
    #expect(try resultPackageColumn(
        approvalTable,
        "source_schema_version"
    ) == [ManualAnnotationCSVExporter.identitySchemaVersion])
    #expect(try resultPackageColumn(
        approvalTable,
        "source_review_state"
    ) == [ManualAnnotationCSVReviewState.pendingConfirmation.rawValue])
    #expect(try resultPackageColumn(
        approvalTable,
        "annotation_ids"
    ) == [STPDCanonicalValue.stringList(manualUIDs)])
    #expect(try resultPackageColumn(
        approvalTable,
        "annotation_source_semantic_digests"
    ) == [STPDCanonicalValue.stringList(sourceSemanticDigests)])
    #expect(try resultPackageColumn(approvalTable, "annotation_count") ==
        ["1"])
    let approvalID = try #require(
        resultPackageColumn(approvalTable, "approval_id").first
    )
    #expect(approvalID.hasPrefix("manual_import_approval_"))
    #expect(try resultPackageColumn(
        manualTable,
        "import_approval_id"
    ) == [approvalID])
}

@Test
func localManualAnnotationCarriesNoImportApprovalReverseLink() throws {
    let fixture = resultPackageFixture()
    let local = try resultPackageManualTonicAnnotation(
        dataset: fixture.dataset,
        run: fixture.run,
        id: UUID(
            uuidString: "b2000000-0000-0000-0000-000000000012"
        )!
    )
    let input = try STPDResultPackageInput.snapshot(
        dataset: fixture.dataset,
        run: fixture.run,
        manualAnnotations: [local]
    )
    let package = try STPDResultPackageBuilder.build(input)
    let manualTable = try #require(
        package.table(.manualAnnotations)
    )
    let approvalTable = try #require(
        package.table(.manualAnnotationImportApprovals)
    )

    #expect(try resultPackageColumn(
        manualTable,
        "authority_source"
    ) == ["local"])
    #expect(try resultPackageColumn(
        manualTable,
        "import_approval_id"
    ) == [""])
    #expect(approvalTable.rowCount == 0)
}

@Test
func importedManualRowWithoutApprovalLedgerFailsPackageOnlyValidation()
    throws
{
    let fixture = resultPackageFixture()
    let annotation = try resultPackageManualTonicAnnotation(
        dataset: fixture.dataset,
        run: fixture.run,
        id: UUID(
            uuidString: "b2000000-0000-0000-0000-000000000013"
        )!
    )
    let approved = try resultPackageApprovedManualImport(
        dataset: fixture.dataset,
        run: fixture.run,
        annotation: annotation
    )
    let approvalTable = try #require(
        approved.package.table(.manualAnnotationImportApprovals)
    )
    let emptyApprovalTable = try STPDResultTableData(
        contract: approvalTable.contract,
        headers: approvalTable.headers,
        columnDefinitions: approvalTable.columnDefinitions,
        rows: []
    )
    var tamperedTables = approved.package.tables
    tamperedTables[.manualAnnotationImportApprovals] =
        emptyApprovalTable

    #expect(throws: STPDResultPackageError.self) {
        _ = try resultPackageValidate(
            approved.package,
            tables: tamperedTables,
            dataset: fixture.dataset,
            run: fixture.run
        )
    }
}

@Test
func importedManualReverseLinkTamperingFailsPackageOnlyValidation()
    throws
{
    let fixture = resultPackageFixture()
    let annotation = try resultPackageManualTonicAnnotation(
        dataset: fixture.dataset,
        run: fixture.run,
        id: UUID(
            uuidString: "b2000000-0000-0000-0000-000000000014"
        )!
    )
    let approved = try resultPackageApprovedManualImport(
        dataset: fixture.dataset,
        run: fixture.run,
        annotation: annotation
    )
    let manualTable = try #require(
        approved.package.table(.manualAnnotations)
    )
    var tamperedTables = approved.package.tables
    tamperedTables[.manualAnnotations] =
        try resultPackageReplacingCell(
            manualTable,
            column: "import_approval_id",
            with: "manual_import_approval_" +
                String(repeating: "0", count: 64)
        )

    #expect(throws: STPDResultPackageError.self) {
        _ = try resultPackageValidate(
            approved.package,
            tables: tamperedTables,
            dataset: fixture.dataset,
            run: fixture.run
        )
    }
}

@Test
func importedManualApprovalRejectsCrossPairedSemanticDigests()
    throws
{
    let fixture = resultPackageFixture()
    let first = try resultPackageManualTonicAnnotation(
        dataset: fixture.dataset,
        run: fixture.run,
        id: UUID(
            uuidString: "b2000000-0000-0000-0000-000000000015"
        )!,
        ordinal: 0
    )
    let second = try resultPackageManualTonicAnnotation(
        dataset: fixture.dataset,
        run: fixture.run,
        id: UUID(
            uuidString: "b2000000-0000-0000-0000-000000000016"
        )!,
        ordinal: 1
    )
    let approved = try resultPackageApprovedManualImport(
        dataset: fixture.dataset,
        run: fixture.run,
        annotations: [second, first]
    )
    let approvalTable = try #require(
        approved.package.table(.manualAnnotationImportApprovals)
    )
    let row = try #require(approvalTable.rows.first)
    let values = Dictionary(
        uniqueKeysWithValues: zip(approvalTable.headers, row)
    )
    let annotationIDsValue = try #require(values["annotation_ids"])
    let semanticDigestsValue = try #require(
        values["annotation_source_semantic_digests"]
    )
    let manualUIDs = try #require(
        STPDCanonicalValue.parseStringList(annotationIDsValue)
    )
    let semanticDigests = try #require(
        STPDCanonicalValue.parseStringList(semanticDigestsValue)
    )
    #expect(manualUIDs == manualUIDs.sorted())
    #expect(semanticDigests.count == 2)
    let swappedDigests = Array(semanticDigests.reversed())
    #expect(swappedDigests != semanticDigests)
    let sourceFileDigest = try #require(
        values["source_file_sha256"]
    )
    let activeDatasetDigest = try #require(
        values["approved_dataset_digest"]
    )
    let approvedRunID = try #require(values["approved_run_id"])
    let approvedSettingsDigest = try #require(
        values["approved_settings_digest"]
    )
    let approver = try #require(values["approver"])
    let approvedAtUnixSec = try #require(
        values["approved_at_unix_sec"]
    )
    let sourceSchemaVersion = try #require(
        values["source_schema_version"]
    )
    let sourceRunID = try #require(values["source_run_id"])
    let sourceReviewState = try #require(
        values["source_review_state"]
    )
    let forgedApprovalID = STPDStableIdentifier.make(
        prefix: "manual_import_approval",
        domain: "stpd_manual_import_approval_uid_v2",
        components: [
            sourceFileDigest,
            activeDatasetDigest,
            approvedRunID,
            approvedSettingsDigest,
            approver,
            approvedAtUnixSec,
            sourceSchemaVersion,
            sourceRunID,
            sourceReviewState,
            STPDCanonicalValue.stringList(manualUIDs),
            STPDCanonicalValue.stringList(swappedDigests),
        ]
    )
    let forgedApprovalTable = try resultPackageReplacingCells(
        approvalTable,
        updates: [
            "approval_id": forgedApprovalID,
            "annotation_source_semantic_digests":
                STPDCanonicalValue.stringList(swappedDigests),
        ]
    )
    #expect(forgedApprovalTable.rowCount == 1)
    var forgedManualTable = try #require(
        approved.package.table(.manualAnnotations)
    )
    for rowIndex in forgedManualTable.rows.indices {
        forgedManualTable = try resultPackageReplacingCell(
            forgedManualTable,
            row: rowIndex,
            column: "import_approval_id",
            with: forgedApprovalID
        )
    }
    var tamperedTables = approved.package.tables
    tamperedTables[.manualAnnotationImportApprovals] =
        forgedApprovalTable
    tamperedTables[.manualAnnotations] = forgedManualTable

    #expect(throws: STPDResultPackageError.self) {
        _ = try resultPackageValidate(
            approved.package,
            tables: tamperedTables,
            dataset: fixture.dataset,
            run: fixture.run
        )
    }
}

@Test
func approvedManualImportRejectsSemanticMutationUnderSameUUID() throws {
    let fixture = resultPackageFixture()
    let sourceAnnotation = try resultPackageManualTonicAnnotation(
        dataset: fixture.dataset,
        run: fixture.run,
        id: UUID(
            uuidString: "b2000000-0000-0000-0000-000000000002"
        )!
    )
    let approved = try resultPackageApprovedManualImport(
        dataset: fixture.dataset,
        run: fixture.run,
        annotation: sourceAnnotation
    )
    var mutated = try #require(
        approved.approvedBatch.annotations.first
    )
    mutated.note = "content changed after approval"

    #expect(ManualAnnotationCSVApprovedBatch(
        annotations: [mutated],
        approvalReceipt: approved.approvedBatch.approvalReceipt
    ) == nil)
    #expect(throws: STPDResultPackageError.self) {
        let valid = approved.input
        let input = STPDResultPackageInput(
            dataset: valid.dataset,
            run: valid.run,
            sourceMode: valid.sourceMode,
            finalEvents: valid.finalEvents,
            finalISILabelRows: valid.finalISILabelRows,
            manualAnnotations: [mutated],
            manualAnnotationImportApprovals: [
                approved.approvedBatch.approvalReceipt,
            ],
            candidateReviews: valid.candidateReviews,
            reviewLinks: valid.reviewLinks,
            candidateDiagnostics: valid.candidateDiagnostics
        )
        _ = try STPDResultPackageBuilder.build(input)
    }
}

@Test
func approvedManualImportRejectsDuplicateReceiptClaims() throws {
    let fixture = resultPackageFixture()
    let sourceAnnotation = try resultPackageManualTonicAnnotation(
        dataset: fixture.dataset,
        run: fixture.run,
        id: UUID(
            uuidString: "b2000000-0000-0000-0000-000000000003"
        )!
    )
    let approved = try resultPackageApprovedManualImport(
        dataset: fixture.dataset,
        run: fixture.run,
        annotation: sourceAnnotation
    )
    let receipt = approved.approvedBatch.approvalReceipt

    #expect(throws: STPDResultPackageError.self) {
        let valid = approved.input
        let input = STPDResultPackageInput(
            dataset: valid.dataset,
            run: valid.run,
            sourceMode: valid.sourceMode,
            finalEvents: valid.finalEvents,
            finalISILabelRows: valid.finalISILabelRows,
            manualAnnotations: approved.approvedBatch.annotations,
            manualAnnotationImportApprovals: [receipt, receipt],
            candidateReviews: valid.candidateReviews,
            reviewLinks: valid.reviewLinks,
            candidateDiagnostics: valid.candidateDiagnostics
        )
        _ = try STPDResultPackageBuilder.build(input)
    }
}

@Test
func approvedManualImportRejectsDifferentTargetRunOrSettings() throws {
    let fixture = resultPackageFixture()
    let sourceAnnotation = try resultPackageManualTonicAnnotation(
        dataset: fixture.dataset,
        run: fixture.run,
        id: UUID(
            uuidString: "b2000000-0000-0000-0000-000000000005"
        )!
    )
    let approved = try resultPackageApprovedManualImport(
        dataset: fixture.dataset,
        run: fixture.run,
        annotation: sourceAnnotation
    )
    let validReceipt = approved.approvedBatch.approvalReceipt

    func batch(
        approvedRunID: String,
        approvedSettingsDigest: String
    ) throws -> ManualAnnotationCSVApprovedBatch {
        let receipt = try #require(
            ManualAnnotationCSVApprovalReceipt(
                sourceFileDigest: validReceipt.sourceFileDigest,
                activeDatasetDigest:
                    validReceipt.activeDatasetDigest,
                approvedRunID: approvedRunID,
                approvedSettingsDigest: approvedSettingsDigest,
                approver: validReceipt.approver,
                approvedAt: validReceipt.approvedAt,
                sourceSchemaVersion:
                    validReceipt.sourceSchemaVersion,
                sourceRunID: validReceipt.sourceRunID,
                sourceReviewState:
                    validReceipt.sourceReviewState,
                annotations: approved.approvedBatch.annotations
            )
        )
        return try #require(
            ManualAnnotationCSVApprovedBatch(
                annotations: approved.approvedBatch.annotations,
                approvalReceipt: receipt
            )
        )
    }

    let wrongRunBatch = try batch(
        approvedRunID: "different-detector-run",
        approvedSettingsDigest:
            fixture.run.runIdentity.settingsDigest
    )
    let wrongSettingsBatch = try batch(
        approvedRunID: fixture.run.runIdentity.runID,
        approvedSettingsDigest: String(repeating: "d", count: 64)
    )

    for wrongBatch in [wrongRunBatch, wrongSettingsBatch] {
        let input = try STPDResultPackageInput.snapshot(
            dataset: fixture.dataset,
            run: fixture.run,
            approvedManualAnnotationImports: [wrongBatch]
        )
        do {
            _ = try STPDResultPackageBuilder.build(input)
            Issue.record(
                "an import approval targeting another run or settings snapshot must fail closed"
            )
        } catch STPDResultPackageError.invalidInput(let message) {
            #expect(
                message.contains(
                    "different detector run or settings snapshot"
                )
            )
        } catch {
            Issue.record(
                "expected invalidInput target-binding rejection, got \(error)"
            )
        }
    }
}

@Test
func approvedManualImportLedgerTamperingFailsValidation() throws {
    let fixture = resultPackageFixture()
    let sourceAnnotation = try resultPackageManualTonicAnnotation(
        dataset: fixture.dataset,
        run: fixture.run,
        id: UUID(
            uuidString: "b2000000-0000-0000-0000-000000000004"
        )!
    )
    let approved = try resultPackageApprovedManualImport(
        dataset: fixture.dataset,
        run: fixture.run,
        annotation: sourceAnnotation
    )
    let approvalTable = try #require(
        approved.package.table(.manualAnnotationImportApprovals)
    )
    let row = try #require(approvalTable.rows.first)
    let values = Dictionary(
        uniqueKeysWithValues: zip(approvalTable.headers, row)
    )
    let forgedApprover = "Different Approver"
    let annotationIDsValue = try #require(values["annotation_ids"])
    let semanticDigestsValue = try #require(
        values["annotation_source_semantic_digests"]
    )
    let manualUIDs = try #require(
        STPDCanonicalValue.parseStringList(annotationIDsValue)
    )
    let semanticDigests = try #require(
        STPDCanonicalValue.parseStringList(semanticDigestsValue)
    )
    let sourceFileDigest = try #require(
        values["source_file_sha256"]
    )
    let activeDatasetDigest = try #require(
        values["approved_dataset_digest"]
    )
    let approvedRunID = try #require(
        values["approved_run_id"]
    )
    let approvedSettingsDigest = try #require(
        values["approved_settings_digest"]
    )
    let approvedAtUnixSec = try #require(
        values["approved_at_unix_sec"]
    )
    let sourceSchemaVersion = try #require(
        values["source_schema_version"]
    )
    let sourceRunID = try #require(values["source_run_id"])
    let sourceReviewState = try #require(
        values["source_review_state"]
    )
    let forgedApprovalID = STPDStableIdentifier.make(
        prefix: "manual_import_approval",
        domain: "stpd_manual_import_approval_uid_v2",
        components: [
            sourceFileDigest,
            activeDatasetDigest,
            approvedRunID,
            approvedSettingsDigest,
            forgedApprover,
            approvedAtUnixSec,
            sourceSchemaVersion,
            sourceRunID,
            sourceReviewState,
            STPDCanonicalValue.stringList(manualUIDs),
            STPDCanonicalValue.stringList(semanticDigests),
        ]
    )
    var tamperedTables = approved.package.tables
    let forgedApproverTable = try resultPackageReplacingCell(
        approvalTable,
        column: "approver",
        with: forgedApprover
    )
    tamperedTables[.manualAnnotationImportApprovals] =
        try resultPackageReplacingCell(
            forgedApproverTable,
            column: "approval_id",
            with: forgedApprovalID
        )

    #expect(throws: STPDResultPackageError.self) {
        _ = try resultPackageValidate(
            approved.package,
            tables: tamperedTables,
            dataset: fixture.dataset,
            run: fixture.run,
            expectedManualAnnotations:
                approved.approvedBatch.annotations,
            expectedManualAnnotationImportApprovals: [
                approved.approvedBatch.approvalReceipt,
            ]
        )
    }

    var assuranceTamperedTables = approved.package.tables
    assuranceTamperedTables[.manualAnnotationImportApprovals] =
        try resultPackageReplacingCell(
            approvalTable,
            column: "approver_identity_assurance",
            with: "authenticated_identity"
        )
    #expect(throws: STPDResultPackageError.self) {
        _ = try resultPackageValidate(
            approved.package,
            tables: assuranceTamperedTables,
            dataset: fixture.dataset,
            run: fixture.run,
            expectedManualAnnotations:
                approved.approvedBatch.annotations,
            expectedManualAnnotationImportApprovals: [
                approved.approvedBatch.approvalReceipt,
            ]
        )
    }
}

@Test
func approvedManualImportSemanticDigestTamperingFailsWithoutExpectedApprovalObjects()
    throws
{
    let fixture = resultPackageFixture()
    let sourceAnnotation = try resultPackageManualTonicAnnotation(
        dataset: fixture.dataset,
        run: fixture.run,
        id: UUID(
            uuidString: "b2000000-0000-0000-0000-000000000006"
        )!
    )
    let approved = try resultPackageApprovedManualImport(
        dataset: fixture.dataset,
        run: fixture.run,
        annotation: sourceAnnotation
    )
    let approvalTable = try #require(
        approved.package.table(.manualAnnotationImportApprovals)
    )
    let row = try #require(approvalTable.rows.first)
    let values = Dictionary(
        uniqueKeysWithValues: zip(approvalTable.headers, row)
    )
    let annotationIDsValue = try #require(values["annotation_ids"])
    let annotationIDs = try #require(
        STPDCanonicalValue.parseStringList(annotationIDsValue)
    )
    let forgedSemanticDigests = [
        "manual_import_semantics_" + String(repeating: "0", count: 64),
    ]
    let originalSemanticDigestValue = try #require(
        values["annotation_source_semantic_digests"]
    )
    #expect(
        forgedSemanticDigests != STPDCanonicalValue.parseStringList(
            originalSemanticDigestValue
        )
    )
    let forgedApprovalID = STPDStableIdentifier.make(
        prefix: "manual_import_approval",
        domain: "stpd_manual_import_approval_uid_v2",
        components: [
            try #require(values["source_file_sha256"]),
            try #require(values["approved_dataset_digest"]),
            try #require(values["approved_run_id"]),
            try #require(values["approved_settings_digest"]),
            try #require(values["approver"]),
            try #require(values["approved_at_unix_sec"]),
            try #require(values["source_schema_version"]),
            try #require(values["source_run_id"]),
            try #require(values["source_review_state"]),
            STPDCanonicalValue.stringList(annotationIDs),
            STPDCanonicalValue.stringList(forgedSemanticDigests),
        ]
    )
    var tamperedTable = try resultPackageReplacingCell(
        approvalTable,
        column: "annotation_source_semantic_digests",
        with: STPDCanonicalValue.stringList(forgedSemanticDigests)
    )
    tamperedTable = try resultPackageReplacingCell(
        tamperedTable,
        column: "approval_id",
        with: forgedApprovalID
    )
    var tamperedTables = approved.package.tables
    tamperedTables[.manualAnnotationImportApprovals] = tamperedTable

    #expect(throws: STPDResultPackageError.self) {
        _ = try resultPackageValidate(
            approved.package,
            tables: tamperedTables,
            dataset: fixture.dataset,
            run: fixture.run
        )
    }
}

@Test
func approvedImportAuthorityFieldsAreBoundIntoManualSemanticDigest()
    throws
{
    let fixture = resultPackageFixture()
    let annotation = try resultPackageManualTonicAnnotation(
        dataset: fixture.dataset,
        run: fixture.run,
        id: UUID(
            uuidString: "b2000000-0000-0000-0000-000000000017"
        )!
    )
    let approved = try resultPackageApprovedManualImport(
        dataset: fixture.dataset,
        run: fixture.run,
        annotation: annotation
    )
    let manualTable = try #require(
        approved.package.table(.manualAnnotations)
    )
    let approvalTable = try #require(
        approved.package.table(.manualAnnotationImportApprovals)
    )
    let originalDigest = try #require(
        resultPackageColumn(
            manualTable,
            "annotation_semantic_digest"
        ).first
    )
    let authorityRewritten = try resultPackageReplacingCells(
        manualTable,
        updates: [
            "authority_source": "local",
            "import_approval_id": "",
        ]
    )
    let rewrittenDigest = try #require(
        resultPackageColumn(
            try resultPackageRefreshingManualSemanticDigest(
                authorityRewritten
            ),
            "annotation_semantic_digest"
        ).first
    )
    #expect(rewrittenDigest != originalDigest)

    let emptyApprovalTable = try STPDResultTableData(
        contract: approvalTable.contract,
        headers: approvalTable.headers,
        columnDefinitions: approvalTable.columnDefinitions,
        rows: []
    )
    var tamperedTables = approved.package.tables
    tamperedTables[.manualAnnotations] = authorityRewritten
    tamperedTables[.manualAnnotationImportApprovals] =
        emptyApprovalTable

    #expect(throws: STPDResultPackageError.self) {
        _ = try resultPackageValidate(
            approved.package,
            tables: tamperedTables,
            dataset: fixture.dataset,
            run: fixture.run
        )
    }
}

@Test
func packageOnlyValidationReconstructsApprovedImportAuthority()
    throws
{
    let fixture = resultPackageFixture()
    let annotation = try resultPackageManualTonicAnnotation(
        dataset: fixture.dataset,
        run: fixture.run,
        id: UUID(
            uuidString: "b2000000-0000-0000-0000-000000000018"
        )!
    )
    let approved = try resultPackageApprovedManualImport(
        dataset: fixture.dataset,
        run: fixture.run,
        annotation: annotation
    )

    let checks = try resultPackageValidate(
        approved.package,
        tables: approved.package.tables,
        dataset: fixture.dataset,
        run: fixture.run
    )

    #expect(checks.allSatisfy { $0.status == "pass" })
}

@Test
func sealedAuthorityContainersHaveOpaqueStandardMirrors() throws {
    let fixture = resultPackageFixture()
    let annotation = try resultPackageManualTonicAnnotation(
        dataset: fixture.dataset,
        run: fixture.run,
        id: UUID(
            uuidString: "b2000000-0000-0000-0000-000000000019"
        )!
    )
    let approved = try resultPackageApprovedManualImport(
        dataset: fixture.dataset,
        run: fixture.run,
        annotation: annotation
    )

    let batchChildren = Array(
        Mirror(reflecting: approved.approvedBatch).children
    )
    #expect(Set(batchChildren.compactMap(\.label)) == Set([
        "annotationCount",
        "hasExactReceiptCoverage",
    ]))
    #expect(!batchChildren.contains {
        $0.value is [ManualAnnotation]
            || $0.value is ManualAnnotationCSVApprovalReceipt
    })

    let inputChildren = Array(
        Mirror(reflecting: approved.input).children
    )
    #expect(Set(inputChildren.compactMap(\.label)) == Set([
        "sourceMode",
        "finalEventCount",
        "finalISILabelRowCount",
        "manualAnnotationCount",
        "manualAnnotationImportApprovalCount",
        "candidateReviewCount",
        "reviewLinkCount",
        "candidateDiagnosticCount",
    ]))
    #expect(!inputChildren.contains {
        $0.value is [ManualAnnotation]
            || $0.value is [ManualAnnotationCSVApprovalReceipt]
            || $0.value is ManualAnnotationCSVApprovalReceipt
    })
}
