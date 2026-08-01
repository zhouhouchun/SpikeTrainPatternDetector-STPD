import Foundation
import Testing
@testable import STPDCore

private func semanticSourceTable(
    _ table: STPDResultTable,
    columns: [String],
    rows: [[String: String]]
) -> STPDResultSemanticSourceTable {
    STPDResultSemanticSourceTable(
        fileName: table.rawValue,
        columns: columns.map {
            STPDResultColumnDefinition(
                name: $0,
                type: .string,
                nullable: true
            )
        },
        rows: rows.map { row in
            columns.map { row[$0] ?? "" }
        }
    )
}

private let semanticEventColumns = [
    "event_uid", "train_id", "train_name", "final_label", "final_subtype",
    "state_tonic_subtype", "state_high_frequency_subtypes", "start_isi_index",
    "end_isi_index", "raw_start_sec", "raw_end_sec", "aligned_start_sec",
    "aligned_end_sec", "duration_sec", "score", "source_candidate_uids",
    "audit_review_status", "audit_recommended_subtype", "authority_origin",
    "decision_path",
]

private let semanticFeatureColumns = [
    "candidate_uid", "pre_gap_sec", "post_gap_sec", "intra_q10_sec",
    "intra_q50_sec", "intra_q90_sec", "max_intra_isi_sec",
    "mean_intra_isi_sec", "cv", "cv2", "lv", "pre_ratio_q90",
    "post_ratio_q90", "edge_contrast_min_q90", "edge_contrast_geom_q90",
    "score", "anchor_band_source", "anchor_contrast_geom_required",
]

private let semanticDecisionColumns = [
    "candidate_uid", "final_label", "gate_status", "action",
    "selected_for_auto", "selection_status", "audit_review_status",
    "audit_confidence_tier", "audit_uncertainty_reason", "failure_reason",
    "decision_path",
]

private func semanticFixtureSources() -> [STPDResultSemanticSourceTable] {
    let events = semanticSourceTable(
        .eventsFinal,
        columns: semanticEventColumns,
        rows: [
            [
                "event_uid": "e-burst",
                "train_id": "train-b",
                "train_name": "Burst train",
                "final_label": "Burst",
                "final_subtype": "compact",
                "start_isi_index": "4",
                "end_isi_index": "7",
                "raw_start_sec": "1.2",
                "raw_end_sec": "1.24",
                "aligned_start_sec": "1.0",
                "aligned_end_sec": "1.04",
                "duration_sec": "0.04",
                "score": "0.93",
                "source_candidate_uids": "[\"c-burst\"]",
                "audit_review_status": "accepted",
                "authority_origin": "automatic",
                "decision_path": "burst_selected",
            ],
            [
                "event_uid": "e-possible",
                "train_id": "train-b",
                "train_name": "Burst train",
                "final_label": "possible_burst",
                "start_isi_index": "20",
                "end_isi_index": "22",
                "raw_start_sec": "5.2",
                "raw_end_sec": "5.29",
                "aligned_start_sec": "5.0",
                "aligned_end_sec": "5.09",
                "duration_sec": "0.09",
                "score": "0.51",
                "source_candidate_uids": "[\"c-possible\"]",
                "audit_review_status": "accepted",
                "authority_origin": "automatic",
                "decision_path": "possible_burst_review",
            ],
            [
                "event_uid": "e-tonic",
                "train_id": "train-a",
                "train_name": "Tonic train",
                "final_label": "tonic",
                "final_subtype": "classic",
                "start_isi_index": "1",
                "end_isi_index": "8",
                "raw_start_sec": "0.2",
                "raw_end_sec": "3.4",
                "aligned_start_sec": "0.0",
                "aligned_end_sec": "3.2",
                "duration_sec": "3.2",
                "score": "0.72",
                "source_candidate_uids": "[\"c-tonic\"]",
                "audit_review_status": "needs_review",
                "authority_origin": "automatic",
                "decision_path": "tonic_review",
            ],
            [
                "event_uid": "e-pause",
                "train_id": "train-a",
                "train_name": "Tonic train",
                "final_label": "pause",
                "start_isi_index": "9",
                "end_isi_index": "9",
                "raw_start_sec": "3.4",
                "raw_end_sec": "4.3",
                "aligned_start_sec": "3.2",
                "aligned_end_sec": "4.1",
                "duration_sec": "0.9",
                "score": "1.0",
                "source_candidate_uids": "[]",
                "audit_review_status": "manual",
                "authority_origin": "manual_annotation",
                "decision_path": "manual_pause",
            ],
        ]
    )
    let features = semanticSourceTable(
        .candidateFeatures,
        columns: semanticFeatureColumns,
        rows: [
            [
                "candidate_uid": "c-burst",
                "pre_gap_sec": "0.40",
                "post_gap_sec": "0.35",
                "intra_q10_sec": "0.006",
                "intra_q50_sec": "0.008",
                "intra_q90_sec": "0.012",
                "max_intra_isi_sec": "0.014",
                "mean_intra_isi_sec": "0.009",
                "cv": "0.20",
                "cv2": "0.18",
                "lv": "0.12",
                "pre_ratio_q90": "33.3",
                "post_ratio_q90": "29.2",
                "edge_contrast_min_q90": "29.2",
                "edge_contrast_geom_q90": "31.2",
                "anchor_band_source": "train_local_seed_band",
                "anchor_contrast_geom_required": "3.0",
                "score": "0.93",
            ],
            [
                "candidate_uid": "c-possible",
                "intra_q50_sec": "0.030",
                "cv": "0.31",
                "cv2": "0.28",
                "lv": "0.22",
                "score": "0.51",
            ],
            [
                "candidate_uid": "c-tonic",
                "intra_q10_sec": "0.38",
                "intra_q50_sec": "0.42",
                "intra_q90_sec": "0.47",
                "max_intra_isi_sec": "0.49",
                "mean_intra_isi_sec": "0.43",
                "cv": "0.09",
                "cv2": "0.08",
                "lv": "0.06",
                "score": "0.72",
            ],
        ]
    )
    let decisions = semanticSourceTable(
        .finalDecisions,
        columns: semanticDecisionColumns,
        rows: [
            [
                "candidate_uid": "c-burst",
                "final_label": "Burst",
                "gate_status": "pass",
                "action": "select",
                "selected_for_auto": "true",
                "selection_status": "selected",
                "audit_review_status": "accepted",
                "audit_confidence_tier": "high",
                "decision_path": "burst_selected",
            ],
            [
                "candidate_uid": "c-possible",
                "final_label": "possible_burst",
                "gate_status": "review",
                "action": "review",
                "selected_for_auto": "false",
                "selection_status": "review",
                "audit_review_status": "needs_review",
                "audit_confidence_tier": "low",
                "audit_uncertainty_reason": "weak flank contrast",
                "decision_path": "possible_burst_review",
            ],
            [
                "candidate_uid": "c-tonic",
                "final_label": "tonic",
                "gate_status": "review",
                "action": "review",
                "selected_for_auto": "true",
                "selection_status": "selected",
                "audit_review_status": "needs_review",
                "audit_confidence_tier": "medium",
                "audit_uncertainty_reason": "short epoch",
                "decision_path": "tonic_review",
            ],
        ]
    )
    let ledger = semanticSourceTable(
        .candidateLedger,
        columns: [
            "candidate_uid", "train_id", "train_name", "candidate_layer",
            "candidate_class", "start_isi_index", "end_isi_index", "n_isi",
            "n_spikes", "final_label", "gate_status", "action",
            "selected_for_auto", "selection_status",
        ],
        rows: [
            [
                "candidate_uid": "c-burst",
                "train_id": "train-b",
                "train_name": "Burst train",
                "candidate_layer": "burst",
                "candidate_class": "compact",
                "start_isi_index": "4",
                "end_isi_index": "7",
                "n_isi": "4",
                "n_spikes": "5",
            ],
            [
                "candidate_uid": "c-tonic",
                "train_id": "train-a",
                "train_name": "Tonic train",
                "candidate_layer": "state",
                "candidate_class": "tonic",
                "start_isi_index": "1",
                "end_isi_index": "8",
                "n_isi": "8",
                "n_spikes": "9",
            ],
            [
                "candidate_uid": "c-possible",
                "train_id": "train-b",
                "train_name": "Burst train",
                "candidate_layer": "burst",
                "candidate_class": "possible",
                "start_isi_index": "20",
                "end_isi_index": "22",
                "n_isi": "3",
                "n_spikes": "4",
            ],
        ]
    )
    let isiLabels = semanticSourceTable(
        .isiLabelsFinal,
        columns: [
            "isi_uid", "train_id", "train_name", "isi_index", "timestamp_sec",
            "aligned_timestamp_sec", "isi_sec", "auto_pattern", "auto_subtype",
            "final_pattern", "final_subtype", "final_source", "isi_qc_class",
            "artifact_floor_status", "qc_refractory_suspect", "review_note",
            "review_changed_projection",
        ],
        rows: [
            [
                "isi_uid": "isi-b-5",
                "train_id": "train-b",
                "train_name": "Burst train",
                "isi_index": "5",
                "timestamp_sec": "1.012",
                "aligned_timestamp_sec": "0.812",
                "isi_sec": "0.006",
                "auto_pattern": "Burst",
                "final_pattern": "Burst",
                "final_source": "automatic",
                "isi_qc_class": "valid",
                "artifact_floor_status": "pass",
                "qc_refractory_suspect": "false",
                "review_changed_projection": "false",
            ],
            [
                "isi_uid": "isi-a-9",
                "train_id": "train-a",
                "train_name": "Tonic train",
                "isi_index": "9",
                "timestamp_sec": "4.1",
                "aligned_timestamp_sec": "3.9",
                "isi_sec": "0.9",
                "auto_pattern": "tonic",
                "final_pattern": "pause",
                "final_source": "manual",
                "isi_qc_class": "valid",
                "artifact_floor_status": "pass",
                "qc_refractory_suspect": "false",
                "review_note": "confirmed pause",
                "review_changed_projection": "true",
            ],
        ]
    )
    let metadata = semanticSourceTable(
        .runMetadata,
        columns: ["dataset_name", "train_count", "spike_count"],
        rows: [[
            "dataset_name": "semantic-fixture",
            "train_count": "2",
            "spike_count": "42",
        ]]
    )

    return [events, features, decisions, ledger, isiLabels, metadata]
}

private func semanticValue(
    _ table: STPDResultSemanticTable,
    rowID: String,
    columnID: String
) throws -> String {
    let row = try #require(table.rows.first { $0.id == rowID })
    let index = try #require(table.columns.firstIndex { $0.id == columnID })
    return row.values[index]
}

@Test
func semanticPresentationPartitionsReviewAndScientificTables() throws {
    let presentation = try STPDResultSemanticPresentation.make(
        sourceTables: semanticFixtureSources(),
        consistencyChecks: [
            .init(id: "shape", status: "pass", severity: "info", details: "ok"),
            .init(id: "review", status: "attention", severity: "warning", details: "inspect"),
        ]
    )

    let authoritative = try #require(
        presentation.table(id: STPDResultSemanticPresentation.authoritativeEventsTableID)
    )
    let high = try #require(
        presentation.table(id: STPDResultSemanticPresentation.highConfidenceTableID)
    )
    let review = try #require(
        presentation.table(id: STPDResultSemanticPresentation.reviewCandidatesTableID)
    )
    #expect(authoritative.rows.map(\.id) == ["e-pause", "e-burst"])
    #expect(high.rows.map(\.id) == ["e-burst"])
    #expect(review.rows.map(\.id) == ["e-tonic", "e-possible"])
    #expect(!high.rows.contains { $0.id == "e-possible" })

    #expect(presentation.table(id: "science.burst")?.rows.map(\.id) == ["e-burst"])
    #expect(presentation.table(id: "science.tonic")?.rows.map(\.id) == ["e-tonic"])
    #expect(presentation.table(id: "science.pause")?.rows.map(\.id) == ["e-pause"])
    #expect(
        presentation.table(id: "science.possible_burst")?.rows.map(\.id)
            == ["e-possible"]
    )

    #expect(presentation.overview.datasetName == "semantic-fixture")
    #expect(presentation.overview.trainCount == 2)
    #expect(presentation.overview.spikeCount == 42)
    #expect(presentation.overview.allEventCount == 4)
    #expect(presentation.overview.authoritativeEventCount == 2)
    #expect(presentation.overview.highConfidenceEventCount == 1)
    #expect(presentation.overview.reviewCandidateCount == 2)
    #expect(presentation.overview.consistencyPassCount == 1)
    #expect(presentation.overview.consistencyAttentionCount == 1)
}

@Test
func semanticPresentationIsDeterministicAcrossSourceAndRowOrder() throws {
    let sources = semanticFixtureSources()
    let reversed = sources.reversed().map {
        STPDResultSemanticSourceTable(
            fileName: $0.fileName,
            columns: $0.columns,
            rows: $0.rows.reversed()
        )
    }
    let first = try STPDResultSemanticPresentation.make(sourceTables: sources)
    let second = try STPDResultSemanticPresentation.make(sourceTables: reversed)

    #expect(first.overview == second.overview)
    #expect(first.reviewTables == second.reviewTables)
    #expect(first.scientificTables == second.scientificTables)
    #expect(first.isiTables == second.isiTables)
    #expect(first.diagnosticTables == second.diagnosticTables)
    #expect(first.scientificNotes == second.scientificNotes)
}

@Test
func semanticPresentationJoinsOnlyOneUnambiguousCandidate() throws {
    var sources = semanticFixtureSources()
    let ambiguousEvent = semanticSourceTable(
        .eventsFinal,
        columns: semanticEventColumns,
        rows: [[
            "event_uid": "e-composite",
            "train_id": "train-b",
            "train_name": "Burst train",
            "final_label": "Burst",
            "start_isi_index": "4",
            "end_isi_index": "22",
            "source_candidate_uids": "[\"c-burst\",\"c-possible\"]",
            "audit_review_status": "needs_review",
            "authority_origin": "automatic",
            "decision_path": "merged_sources",
        ]]
    )
    sources.removeAll { $0.fileName == STPDResultTable.eventsFinal.rawValue }
    sources.append(ambiguousEvent)

    let presentation = try STPDResultSemanticPresentation.make(sourceTables: sources)
    let events = try #require(
        presentation.table(id: STPDResultSemanticPresentation.allEventsTableID)
    )
    #expect(
        try semanticValue(
            events,
            rowID: "e-composite",
            columnID: "metric_source"
        ) == "multiple source candidates"
    )
    #expect(
        try semanticValue(events, rowID: "e-composite", columnID: "median").isEmpty
    )
    #expect(
        try semanticValue(events, rowID: "e-composite", columnID: "cv2").isEmpty
    )
    #expect(
        presentation.scientificNotes.contains {
            $0.contains("multiple source candidates")
        }
    )
}

@Test
func semanticPresentationPreservesCandidateAndISILabelMeaning() throws {
    let presentation = try STPDResultSemanticPresentation.make(
        sourceTables: semanticFixtureSources()
    )
    let candidates = try #require(
        presentation.table(id: STPDResultSemanticPresentation.structureCandidatesTableID)
    )
    let isi = try #require(
        presentation.table(id: STPDResultSemanticPresentation.isiLabelsTableID)
    )

    #expect(candidates.rows.map(\.id) == ["c-tonic", "c-burst", "c-possible"])
    #expect(
        try semanticValue(candidates, rowID: "c-burst", columnID: "median") == "0.008"
    )
    #expect(
        try semanticValue(candidates, rowID: "c-possible", columnID: "final_label")
            == "possible_burst"
    )
    #expect(
        try semanticValue(candidates, rowID: "c-burst", columnID: "geom_contrast")
            == "31.2"
    )
    #expect(
        try semanticValue(candidates, rowID: "c-burst", columnID: "required_contrast")
            == "3.0"
    )
    #expect(
        try semanticValue(candidates, rowID: "c-burst", columnID: "boundary_source")
            == "train_local_seed_band"
    )
    #expect(isi.rows.map(\.id) == ["isi-a-9", "isi-b-5"])
    #expect(
        try semanticValue(isi, rowID: "isi-a-9", columnID: "auto_pattern") == "tonic"
    )
    #expect(
        try semanticValue(isi, rowID: "isi-a-9", columnID: "final_pattern") == "pause"
    )
    #expect(
        try semanticValue(isi, rowID: "isi-a-9", columnID: "changed") == "true"
    )
}

@Test
func semanticPresentationPrioritizesScientificColumnsBeforeTraceIdentifiers() throws {
    let presentation = try STPDResultSemanticPresentation.make(
        sourceTables: semanticFixtureSources()
    )
    let events = try #require(
        presentation.table(id: STPDResultSemanticPresentation.allEventsTableID)
    )
    let candidates = try #require(
        presentation.table(id: STPDResultSemanticPresentation.structureCandidatesTableID)
    )
    let isi = try #require(
        presentation.table(id: STPDResultSemanticPresentation.isiLabelsTableID)
    )

    #expect(
        Array(events.columns.prefix(10).map(\.id))
            == [
                "train", "pattern", "subtype", "review", "aligned_start",
                "aligned_end", "raw_start", "raw_end", "duration", "n_spikes",
            ]
    )
    #expect(events.columns.last?.id == "event_id")
    #expect(
        Array(candidates.columns.prefix(10).map(\.id))
            == [
                "train", "class", "final_label", "gate", "action", "selected",
                "review", "isi_range", "n_isi", "n_spikes",
            ]
    )
    #expect(candidates.columns.last?.id == "candidate_id")
    #expect(
        Array(isi.columns.prefix(10).map(\.id))
            == [
                "train", "isi_index", "aligned_time", "raw_time", "isi",
                "final_pattern", "final_subtype", "final_source", "auto_pattern",
                "auto_subtype",
            ]
    )
    #expect(isi.columns.last?.id == "isi_id")

    #expect(try semanticValue(events, rowID: "e-burst", columnID: "train") == "Burst train")
    #expect(try semanticValue(events, rowID: "e-burst", columnID: "aligned_start") == "1.0")
    #expect(try semanticValue(events, rowID: "e-burst", columnID: "raw_start") == "1.2")
    #expect(try semanticValue(events, rowID: "e-burst", columnID: "event_id") == "e-burst")
    #expect(try semanticValue(candidates, rowID: "c-burst", columnID: "class") == "compact")
    #expect(
        try semanticValue(candidates, rowID: "c-burst", columnID: "final_label")
            == "Burst"
    )
    #expect(
        try semanticValue(candidates, rowID: "c-burst", columnID: "candidate_id")
            == "c-burst"
    )
    #expect(try semanticValue(isi, rowID: "isi-b-5", columnID: "aligned_time") == "0.812")
    #expect(try semanticValue(isi, rowID: "isi-b-5", columnID: "raw_time") == "1.012")
    #expect(try semanticValue(isi, rowID: "isi-a-9", columnID: "final_pattern") == "pause")
    #expect(try semanticValue(isi, rowID: "isi-a-9", columnID: "isi_id") == "isi-a-9")
}

@Test
func semanticPresentationExplainsUnavailableRReviewMetrics() throws {
    let presentation = try STPDResultSemanticPresentation.make(
        sourceTables: semanticFixtureSources()
    )

    #expect(presentation.scientificNotes.contains { $0.contains("MM is unavailable") })
    #expect(
        presentation.scientificNotes.contains {
            $0.contains("Pre-LV and After-LV are not present")
        }
    )
    #expect(
        presentation.allSemanticTables.allSatisfy { table in
            table.columns.allSatisfy {
                !["mm", "pre_lv", "after_lv"].contains($0.id.lowercased())
            }
        }
    )
}

@Test
func semanticPresentationFailsClosedOnMalformedInputs() throws {
    #expect(throws: STPDResultSemanticPresentationError.missingRequiredTable(
        STPDResultTable.eventsFinal.rawValue
    )) {
        try STPDResultSemanticPresentation.make(sourceTables: [])
    }

    let events = try #require(
        semanticFixtureSources().first {
            $0.fileName == STPDResultTable.eventsFinal.rawValue
        }
    )
    #expect(throws: STPDResultSemanticPresentationError.duplicateTable(events.fileName)) {
        try STPDResultSemanticPresentation.make(sourceTables: [events, events])
    }

    let badWidth = STPDResultSemanticSourceTable(
        fileName: events.fileName,
        columns: events.columns,
        rows: [Array(events.rows[0].dropLast())]
    )
    #expect(throws: STPDResultSemanticPresentationError.invalidRowWidth(
        table: events.fileName,
        row: 0,
        expected: events.columns.count,
        actual: events.columns.count - 1
    )) {
        try STPDResultSemanticPresentation.make(sourceTables: [badWidth])
    }

    var invalidListRow = Dictionary(
        uniqueKeysWithValues: zip(semanticEventColumns, events.rows[0])
    )
    invalidListRow["source_candidate_uids"] = "c-burst"
    let invalidList = semanticSourceTable(
        .eventsFinal,
        columns: semanticEventColumns,
        rows: [invalidListRow]
    )
    #expect(throws: STPDResultSemanticPresentationError.invalidStringList(
        table: STPDResultTable.eventsFinal.rawValue,
        column: "source_candidate_uids"
    )) {
        try STPDResultSemanticPresentation.make(sourceTables: [invalidList])
    }
}

@Test
func semanticPresentationFailsClosedOnPartialEventJoinTables() throws {
    let sources = semanticFixtureSources().filter {
        $0.fileName != STPDResultTable.finalDecisions.rawValue
    }

    #expect(throws: STPDResultSemanticPresentationError.incompleteTableSet([
        STPDResultTable.candidateFeatures.rawValue,
        STPDResultTable.finalDecisions.rawValue,
    ])) {
        try STPDResultSemanticPresentation.make(sourceTables: sources)
    }
}

@Test
func semanticPresentationFailsClosedWhenEventReferencesMissingCandidateRow() throws {
    var sources = semanticFixtureSources()
    let index = try #require(sources.firstIndex {
        $0.fileName == STPDResultTable.candidateFeatures.rawValue
    })
    let source = sources[index]
    let uidIndex = try #require(source.columns.firstIndex { $0.name == "candidate_uid" })
    sources[index] = STPDResultSemanticSourceTable(
        fileName: source.fileName,
        columns: source.columns,
        rows: source.rows.filter { $0[uidIndex] != "c-burst" }
    )

    #expect(throws: STPDResultSemanticPresentationError.missingJoinedRow(
        table: STPDResultTable.candidateFeatures.rawValue,
        column: "candidate_uid",
        value: "c-burst"
    )) {
        try STPDResultSemanticPresentation.make(sourceTables: sources)
    }
}

@Test
func semanticPresentationFailsClosedOnOrphanCandidateFeatureRow() throws {
    var sources = semanticFixtureSources()
    let index = try #require(sources.firstIndex {
        $0.fileName == STPDResultTable.candidateFeatures.rawValue
    })
    let source = sources[index]
    var orphan = Dictionary(uniqueKeysWithValues: zip(
        source.columns.map(\.name),
        source.rows[0]
    ))
    orphan["candidate_uid"] = "c-orphan"
    sources[index] = STPDResultSemanticSourceTable(
        fileName: source.fileName,
        columns: source.columns,
        rows: source.rows + [source.columns.map { orphan[$0.name] ?? "" }]
    )

    #expect(throws: STPDResultSemanticPresentationError.orphanJoinedRow(
        table: STPDResultTable.candidateFeatures.rawValue,
        column: "candidate_uid",
        value: "c-orphan"
    )) {
        try STPDResultSemanticPresentation.make(sourceTables: sources)
    }
}

@Test
func semanticPresentationFailsClosedOnMissingCandidateMetricColumn() throws {
    var sources = semanticFixtureSources()
    let index = try #require(sources.firstIndex {
        $0.fileName == STPDResultTable.candidateFeatures.rawValue
    })
    let source = sources[index]
    let removedIndex = try #require(
        source.columns.firstIndex { $0.name == "edge_contrast_geom_q90" }
    )
    sources[index] = STPDResultSemanticSourceTable(
        fileName: source.fileName,
        columns: source.columns.enumerated().compactMap {
            $0.offset == removedIndex ? nil : $0.element
        },
        rows: source.rows.map { row in
            row.enumerated().compactMap {
                $0.offset == removedIndex ? nil : $0.element
            }
        }
    )

    #expect(throws: STPDResultSemanticPresentationError.missingRequiredColumn(
        table: STPDResultTable.candidateFeatures.rawValue,
        column: "edge_contrast_geom_q90"
    )) {
        try STPDResultSemanticPresentation.make(sourceTables: sources)
    }
}

@Test
func semanticPresentationLabelsDiagnosticRowsWithoutClaimingNearMissStatus() throws {
    var sources = semanticFixtureSources()
    let mappings: [(STPDResultTable, STPDResultTable)] = [
        (.candidateLedger, .candidateLedgerDiagnostic),
        (.candidateFeatures, .candidateFeaturesDiagnostic),
        (.finalDecisions, .finalDecisionsDiagnostic),
    ]
    for (sourceName, diagnosticName) in mappings {
        let source = try #require(sources.first { $0.fileName == sourceName.rawValue })
        sources.append(STPDResultSemanticSourceTable(
            fileName: diagnosticName.rawValue,
            columns: source.columns,
            rows: source.rows
        ))
    }

    let presentation = try STPDResultSemanticPresentation.make(sourceTables: sources)
    let diagnostic = try #require(
        presentation.table(id: STPDResultSemanticPresentation.diagnosticCandidatesTableID)
    )
    #expect(diagnostic.title == "Diagnostic candidates")
    #expect(diagnostic.subtitle.contains("not classified as near misses"))
    #expect(!presentation.diagnosticTables.contains {
        $0.title.lowercased().contains("near-miss")
            || $0.title.lowercased().contains("near miss")
    })
}
