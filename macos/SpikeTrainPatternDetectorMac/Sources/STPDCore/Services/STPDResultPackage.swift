import Foundation

public enum STPDResultPackageOwnership {
    public static let ownerName = "Zhou Houchun"
    public static let ownerEmail = "zhouhouchun@outlook.com"
}

public enum STPDResultPackageSourceMode: String, Hashable, Sendable {
    /// Final events and ISI labels were derived from the automatic detector projection only.
    case automatic
    /// The caller supplied the reviewed/manual projection that is authoritative for public output.
    case reviewed
}

public enum STPDCandidateReviewStatus: String, Hashable, Sendable, CaseIterable {
    /// The reviewer confirmed the automatic candidate without changing its public projection.
    case accepted
    /// The reviewer removed the automatic candidate from the public projection.
    case rejected
    /// The reviewer changed the public projection, with a linked manual annotation carrying the
    /// authoritative replacement geometry and label.
    case modified
    /// The candidate still needs review and therefore cannot authorize a reviewed result.
    case needsReview = "needs_review"

    var grantsReviewAuthority: Bool {
        self != .needsReview
    }
}

public struct STPDCandidateReviewInput: Hashable, Sendable {
    public let sourceCandidateID: String
    public let status: STPDCandidateReviewStatus
    public let reviewer: String
    public let note: String
    public let reviewedAt: Date?

    public init(
        sourceCandidateID: String,
        status: STPDCandidateReviewStatus,
        reviewer: String = "",
        note: String = "",
        reviewedAt: Date? = nil
    ) {
        self.sourceCandidateID = sourceCandidateID
        self.status = status
        self.reviewer = reviewer
        self.note = note
        self.reviewedAt = reviewedAt
    }
}

public enum STPDResultReviewEvidence: Hashable, Sendable {
    case manualAnnotation(UUID)
    case candidateReview(sourceCandidateID: String)
}

/// A causal edge from one authoritative review action to one public ISI result.
///
/// Reviewed packages are fail-closed: merely carrying a manual annotation or review record is not
/// sufficient. Every authority-bearing record must be linked to the ISIs it actually governs.
public struct STPDResultReviewLink: Hashable, Sendable {
    public let trainID: String
    public let isiIndex: Int
    public let evidence: STPDResultReviewEvidence

    public init(
        trainID: String,
        isiIndex: Int,
        evidence: STPDResultReviewEvidence
    ) {
        self.trainID = trainID
        self.isiIndex = isiIndex
        self.evidence = evidence
    }
}

public struct STPDCandidateDiagnosticInput: Hashable, Sendable {
    public let sourceCandidateID: String
    public let stageName: String
    public let stageOrdinal: Int
    public let status: String
    public let details: String

    public init(
        sourceCandidateID: String,
        stageName: String,
        stageOrdinal: Int,
        status: String,
        details: String
    ) {
        self.sourceCandidateID = sourceCandidateID
        self.stageName = stageName
        self.stageOrdinal = max(0, stageOrdinal)
        self.status = status
        self.details = details
    }
}

/// All inputs needed to materialize a normalized result package.
///
/// The detector run is immutable evidence. `finalEvents` and `finalISILabelRows` are explicit so a
/// reviewed result cannot be silently reconstructed from automatic candidates and mislabeled as
/// human-reviewed. Use `automatic(dataset:run:)` only when automatic projection is intended.
public struct STPDResultPackageInput: Sendable {
    public let dataset: SpikeDataset
    public let run: ClassicAnchorDetectionRun
    public let sourceMode: STPDResultPackageSourceMode
    public let finalEvents: [ClassicAnchorEventAnnotation]
    public let finalISILabelRows: [ReviewedISIExportRow]
    public let manualAnnotations: [ManualAnnotation]
    public let candidateReviews: [STPDCandidateReviewInput]
    public let reviewLinks: [STPDResultReviewLink]
    public let candidateDiagnostics: [STPDCandidateDiagnosticInput]

    public init(
        dataset: SpikeDataset,
        run: ClassicAnchorDetectionRun,
        sourceMode: STPDResultPackageSourceMode,
        finalEvents: [ClassicAnchorEventAnnotation],
        finalISILabelRows: [ReviewedISIExportRow],
        manualAnnotations: [ManualAnnotation] = [],
        candidateReviews: [STPDCandidateReviewInput] = [],
        reviewLinks: [STPDResultReviewLink] = [],
        candidateDiagnostics: [STPDCandidateDiagnosticInput] = []
    ) {
        self.dataset = dataset
        self.run = run
        self.sourceMode = sourceMode
        self.finalEvents = finalEvents
        self.finalISILabelRows = finalISILabelRows
        self.manualAnnotations = manualAnnotations
        self.candidateReviews = candidateReviews
        self.reviewLinks = reviewLinks
        self.candidateDiagnostics = candidateDiagnostics
    }

    public static func automatic(
        dataset: SpikeDataset,
        run: ClassicAnchorDetectionRun
    ) -> STPDResultPackageInput {
        let events = run.eventAnnotations(
            in: dataset,
            tracks: [.event, .gap, .state]
        )
        let isiRows = ReviewedISIExportBuilder.build(
            dataset: dataset,
            autoAnnotations: events,
            projectionsByTrain: [:]
        )
        return STPDResultPackageInput(
            dataset: dataset,
            run: run,
            sourceMode: .automatic,
            finalEvents: events,
            finalISILabelRows: isiRows
        )
    }
}

public enum STPDResultColumnType: String, Codable, Hashable, Sendable {
    case string
    case integer
    case real
    case boolean
    case timestamp
    case stringList = "string_list"
}

public struct STPDResultColumnDefinition: Codable, Hashable, Sendable {
    public let name: String
    public let type: STPDResultColumnType
    public let nullable: Bool

    public init(name: String, type: STPDResultColumnType, nullable: Bool) {
        self.name = name
        self.type = type
        self.nullable = nullable
    }
}

private enum STPDResultTimestamp {
    static func parse(_ value: String) -> Date? {
        let fractional = ISO8601DateFormatter()
        fractional.formatOptions = [.withInternetDateTime, .withFractionalSeconds]
        if let date = fractional.date(from: value) {
            return date
        }

        let wholeSeconds = ISO8601DateFormatter()
        wholeSeconds.formatOptions = [.withInternetDateTime]
        return wholeSeconds.date(from: value)
    }
}

public struct STPDResultTableData: Sendable {
    public let contract: STPDResultTableContract
    public let headers: [String]
    public let columnDefinitions: [STPDResultColumnDefinition]
    public let rows: [[String]]

    public init(
        contract: STPDResultTableContract,
        headers: [String],
        columnDefinitions: [STPDResultColumnDefinition]? = nil,
        rows: [[String]]
    ) throws {
        guard Set(headers).count == headers.count else {
            throw STPDResultPackageError.invalidTable(
                table: contract.table.rawValue,
                reason: "duplicate header"
            )
        }
        for column in contract.requiredIdentityColumns + contract.primaryKey where !headers.contains(column) {
            throw STPDResultPackageError.invalidTable(
                table: contract.table.rawValue,
                reason: "missing required column \(column)"
            )
        }
        guard rows.allSatisfy({ $0.count == headers.count }) else {
            throw STPDResultPackageError.invalidTable(
                table: contract.table.rawValue,
                reason: "row width does not match header width"
            )
        }
        let resolvedDefinitions = columnDefinitions
            ?? STPDResultColumnCatalog.definitions(
                table: contract.table,
                headers: headers,
                nonNullable: Set(contract.requiredIdentityColumns + contract.primaryKey)
            )
        guard resolvedDefinitions.map(\.name) == headers else {
            throw STPDResultPackageError.invalidTable(
                table: contract.table.rawValue,
                reason: "column definitions do not exactly match ordered headers"
            )
        }
        guard Set(resolvedDefinitions.map(\.name)).count == resolvedDefinitions.count else {
            throw STPDResultPackageError.invalidTable(
                table: contract.table.rawValue,
                reason: "duplicate column definition"
            )
        }
        let definitionByName = Dictionary(
            uniqueKeysWithValues: resolvedDefinitions.map { ($0.name, $0) }
        )
        for primaryKeyColumn in contract.primaryKey
        where definitionByName[primaryKeyColumn]?.nullable != false {
            throw STPDResultPackageError.invalidTable(
                table: contract.table.rawValue,
                reason: "primary-key column \(primaryKeyColumn) must be non-nullable"
            )
        }
        for (rowIndex, row) in rows.enumerated() {
            for (columnIndex, value) in row.enumerated() {
                try Self.validateCell(
                    value,
                    definition: resolvedDefinitions[columnIndex],
                    table: contract.table,
                    rowIndex: rowIndex
                )
            }
        }

        let primaryKeyIndices = contract.primaryKey.compactMap { headers.firstIndex(of: $0) }
        self.contract = contract
        self.headers = headers
        self.columnDefinitions = resolvedDefinitions
        self.rows = rows.sorted { lhs, rhs in
            for index in primaryKeyIndices where lhs[index] != rhs[index] {
                return Self.cellPrecedes(
                    lhs[index],
                    rhs[index],
                    type: resolvedDefinitions[index].type
                )
            }
            return lhs.lexicographicallyPrecedes(rhs)
        }
    }

    public var rowCount: Int {
        rows.count
    }

    public var csvData: Data {
        STPDRFC4180.data(headers: headers, rows: rows)
    }

    public func value(row: Int, column: String) -> String? {
        guard rows.indices.contains(row),
              let index = headers.firstIndex(of: column) else {
            return nil
        }
        return rows[row][index]
    }

    private static func validateCell(
        _ value: String,
        definition: STPDResultColumnDefinition,
        table: STPDResultTable,
        rowIndex: Int
    ) throws {
        if value.isEmpty {
            guard definition.nullable else {
                throw STPDResultPackageError.invalidTable(
                    table: table.rawValue,
                    reason: "row \(rowIndex) has an empty non-nullable \(definition.name)"
                )
            }
            return
        }

        let isValid: Bool
        switch definition.type {
        case .string:
            isValid = true
        case .stringList:
            isValid = STPDCanonicalValue.parseStringList(value) != nil
        case .integer:
            isValid = Int(value) != nil
        case .real:
            isValid = Double(value)?.isFinite == true
        case .boolean:
            isValid = value == "true" || value == "false"
        case .timestamp:
            isValid = STPDResultTimestamp.parse(value) != nil
        }
        guard isValid else {
            throw STPDResultPackageError.invalidTable(
                table: table.rawValue,
                reason:
                    "row \(rowIndex) column \(definition.name) is not a valid " +
                    definition.type.rawValue
            )
        }
    }

    private static func cellPrecedes(
        _ lhs: String,
        _ rhs: String,
        type: STPDResultColumnType
    ) -> Bool {
        if lhs.isEmpty || rhs.isEmpty {
            return lhs.isEmpty && !rhs.isEmpty
        }
        switch type {
        case .integer:
            return (Int(lhs) ?? 0) < (Int(rhs) ?? 0)
        case .real:
            return (Double(lhs) ?? 0) < (Double(rhs) ?? 0)
        case .boolean:
            return lhs == "false" && rhs == "true"
        case .timestamp:
            guard let left = STPDResultTimestamp.parse(lhs),
                  let right = STPDResultTimestamp.parse(rhs) else {
                return lhs < rhs
            }
            return left < right
        case .string, .stringList:
            return lhs < rhs
        }
    }
}

private enum STPDResultColumnCatalog {
    private static let booleanColumns: Set<String> = [
        "anchor_is_seed",
        "anchor_band_ordered",
        "audit_hfs_selected_for_auto",
        "audit_burst_packet_like",
        "audit_burst_dominated",
        "audit_requires_review",
        "audit_review_required",
        "burst_boundary_applied",
        "burst_boundary_floor_hard",
        "burst_bridge_count_pass",
        "burst_bridge_fraction_pass",
        "burst_possible_boundary_pass",
        "burst_q90_bridge_pass",
        "burst_strict_boundary_pass",
        "build_reproducibility_attested",
        "candidate_selected_for_auto",
        "dataset_summary_included_target_train",
        "dataset_summary_self_inclusive",
        "hf_burst_packet_like",
        "hf_burst_packet_neighbor",
        "hard_threshold",
        "hf_burst_dominated",
        "is_audit_only",
        "is_selected",
        "manual_veto_suppressed",
        "may_propagate_to_dataset",
        "may_select_final_label",
        "package_final_projection_differs_from_audit",
        "profile_boundary_floor_hard",
        "qc_refractory_suspect",
        "requires_review",
        "review_changed_projection",
        "review_evidence_present",
        "selected_for_auto",
        "source_selected_for_auto",
        "state_continuity_authority_frozen",
        "state_continuity_merge_terminal",
        "suppressed_by_hf_state",
        "train_qc_input_was_unsorted",
    ]

    private static let integerColumns: Set<String> = [
        "audit_all_selected_burst_event_count",
        "audit_conflict_end_isi",
        "audit_conflict_start_isi",
        "audit_hfs_end_isi",
        "audit_hfs_end_isi_index",
        "audit_hfs_priority",
        "audit_hfs_start_isi",
        "audit_hfs_start_isi_index",
        "audit_long_burst_priority",
        "audit_packet_count",
        "audit_packet_evidence_event_count",
        "audit_pause_like_break_count",
        "audit_pause_like_break_group_count",
        "audit_selected_burst_ii_count",
        "audit_selected_long_burst_count",
        "audit_strongest_burst_priority",
        "audit_suppressed_burst_proposal_count",
        "candidate_end_isi_index",
        "candidate_end_spike_index",
        "candidate_priority",
        "candidate_start_isi_index",
        "candidate_start_spike_index",
        "burst_seed_run_end_isi",
        "burst_seed_run_start_isi",
        "end_isi_index",
        "end_spike_index",
        "end_spike_array_index",
        "end_spike_ordinal",
        "event_end_isi_index",
        "event_end_spike_index",
        "event_priority",
        "event_start_isi_index",
        "event_start_spike_index",
        "final_event_count",
        "final_isi_count",
        "hard_burst_core_isi_count",
        "hf_max_consecutive_large_isi",
        "hf_min_spikes_required",
        "isi_index",
        "n_isi",
        "n_spikes",
        "n_valid_isi",
        "priority",
        "profile_max_seed_run_length",
        "refractory_suspect_count",
        "row_count",
        "spike_count",
        "spike_index",
        "spike_array_index",
        "spike_ordinal",
        "left_spike_array_index",
        "left_spike_ordinal",
        "right_spike_array_index",
        "right_spike_ordinal",
        "stage_ordinal",
        "state_core_burst_run_length",
        "start_isi_index",
        "start_spike_index",
        "start_spike_array_index",
        "start_spike_ordinal",
        "task_event_count",
        "train_qc_artifact_isi_count",
        "train_qc_dropped_duplicate_timestamp_count",
        "train_qc_duplicate_timestamp_count",
        "train_qc_input_duplicate_timestamp_step_count",
        "train_qc_input_nonmonotonic_step_count",
        "train_qc_input_zero_or_negative_step_count",
        "train_qc_refractory_suspect_isi_count",
        "train_qc_valid_isi_count",
        "train_qc_zero_or_negative_isi_count",
        "train_qc_zero_or_negative_timestamp_step_count",
        "train_count",
    ]

    private static let realColumns: Set<String> = [
        "anchor_contrast_geom_required",
        "anchor_contrast_min_required",
        "audit_packet_coverage",
        "event_local_percentile_median",
        "event_local_robust_z_median",
        "hf_embedded_burst_coverage",
        "profile_burst_contrast_s",
        "profile_possible_contrast_s",
        "profile_seed_high_percentile",
        "profile_seed_low_percentile",
        "cv",
        "cv2",
        "lv",
        "score",
        "state_local_percentile_median",
        "state_local_robust_z_median",
        "state_train_percentile_median",
        "burst_contrast_required",
        "burst_possible_contrast_required",
        "train_qc_firing_rate_hz",
    ]

    private static let timestampColumns: Set<String> = [
        "created_at",
        "reviewed_at",
        "updated_at",
    ]

    private static let stringListColumns: Set<String> = [
        "audit_final_selected_event_subtypes",
        "automatic_support_isi_indices",
        "event_source_candidate_uids",
        "linked_isi_uids",
        "package_final_event_subtypes",
        "review_evidence_uids",
        "source_event_ids",
        "source_candidate_uids",
        "source_support_isi_indices",
        "state_high_frequency_subtypes",
        "unresolved_candidate_ids",
        "unresolved_source_candidate_ids",
    ]

    static func definitions(
        table: STPDResultTable,
        headers: [String],
        nonNullable: Set<String>
    ) -> [STPDResultColumnDefinition] {
        let nullable = nullableColumns(for: table, headers: headers)
        return headers.map { name in
            STPDResultColumnDefinition(
                name: name,
                type: type(for: name),
                nullable: nullable.contains(name) && !nonNullable.contains(name)
            )
        }
    }

    /// Scientific nullability is table-specific. Columns are required by default; this whitelist
    /// records only values that can be absent under a valid detector or review state.
    private static func nullableColumns(
        for table: STPDResultTable,
        headers: [String]
    ) -> Set<String> {
        switch table {
        case .runMetadata:
            return ["dataset_source"]
        case .parametersReport, .candidateLedger, .resultConsistencyCheck:
            return []
        case .resolvedParameters:
            return [
                "requested_value", "adaptive_value", "effective_value",
                "resolution_note", "histogram_value", "default_value",
            ]
        case .candidateFeatures:
            let mandatory: Set<String> = [
                "run_id", "settings_digest", "candidate_uid", "source_candidate_id",
                "score",
                "anchor_band_lower_sec", "anchor_band_upper_sec", "anchor_band_source",
                "anchor_band_semantics", "anchor_band_ordered",
                "anchor_contrast_min_required", "anchor_contrast_geom_required",
                "refractory_suspect_count",
                "state_continuity_authority_frozen", "state_continuity_merge_terminal",
            ]
            return Set(headers).subtracting(mandatory)
        case .finalDecisions:
            return [
                "audit_uncertainty_reason", "failure_reason",
                "audit_long_burst_definition_status",
                "state_tonic_subtype", "state_high_frequency_subtype",
            ]
        case .eventsFinal:
            return [
                "state_tonic_subtype",
                "score", "priority",
            ]
        case .isiLabelsFinal:
            return [
                "auto_pattern", "auto_subtype",
                "auto_source_candidate_id", "auto_candidate_uid",
                "final_pattern", "final_subtype",
                "review_note",
                "train_qc_warning_message",
                "train_qc_duration_sec", "train_qc_firing_rate_hz",
                "train_qc_raw_min_isi_sec", "train_qc_min_valid_isi_sec",
                "train_qc_artifact_min_isi_sec", "train_qc_median_isi_sec",
                "train_qc_max_isi_sec", "train_qc_artifact_fraction",
                "train_qc_refractory_suspect_fraction",
            ]
        case .candidateDiagnosticAudit:
            return [
                "event_uid", "automatic_source_id",
                "source_support_isi_indices", "source_semantic_track",
                "source_event_track_class", "source_label", "source_lock_level",
                "source_state_tonic_subtype", "source_score", "source_priority",
                "source_decision_path",
            ]
        case .manualAnnotations:
            return ["note"]
        case .reviewStatus:
            return ["reviewer", "note", "reviewed_at"]
        case .hfsBurstArbitrationAudit:
            return [
                "strongest_burst_candidate_uid", "strongest_long_burst_candidate_uid",
                "audit_hfs_acceptance_route",
                "audit_strongest_burst_candidate_id", "audit_strongest_burst_subtype",
                "audit_strongest_burst_raw_score", "audit_strongest_burst_priority",
                "audit_strongest_long_burst_candidate_id", "audit_long_burst_raw_score",
                "audit_long_burst_priority",
                "audit_pause_like_threshold_sec", "audit_pause_like_break_fraction",
                "audit_seed_band_lower_sec", "audit_seed_band_upper_sec",
                "audit_bridge_band_upper_sec", "audit_seed_fraction",
                "audit_bridge_fraction", "audit_hfs_short_fraction",
                "audit_hfs_bridge_fraction", "audit_hfs_large_fraction",
                "audit_hfs_cv", "audit_hfs_lv",
            ]
        }
    }

    private static func type(for name: String) -> STPDResultColumnType {
        if timestampColumns.contains(name) {
            return .timestamp
        }
        if stringListColumns.contains(name)
            || name.hasSuffix("_uids")
            || name.hasSuffix("_indices") {
            return .stringList
        }
        if booleanColumns.contains(name)
            || name.hasPrefix("is_")
            || name.hasPrefix("has_") {
            return .boolean
        }
        if integerColumns.contains(name)
            || name.hasSuffix("_count")
            || name.hasSuffix("_ordinal")
            || name.hasSuffix("_priority")
            || name.hasSuffix("_index") {
            return .integer
        }
        if realColumns.contains(name)
            || name.hasSuffix("_sec")
            || name.hasSuffix("_ms")
            || name.hasSuffix("_ratio")
            || name.hasSuffix("_fraction")
            || name.hasSuffix("_score")
            || name.hasSuffix("_cv")
            || name.hasSuffix("_cv2")
            || name.hasSuffix("_lv")
            || name.hasSuffix("_q10")
            || name.hasSuffix("_q25")
            || name.hasSuffix("_q40")
            || name.hasSuffix("_q50")
            || name.hasSuffix("_q75")
            || name.hasSuffix("_q80")
            || name.hasSuffix("_q90")
            || name.hasSuffix("_q95") {
            return .real
        }
        return .string
    }
}

public struct STPDResultPackage: Sendable {
    public let identity: DetectionRunIdentity
    public let sourceMode: STPDResultPackageSourceMode
    public let tables: [STPDResultTable: STPDResultTableData]
    public let manifest: STPDResultManifest

    public func table(_ table: STPDResultTable) -> STPDResultTableData? {
        tables[table]
    }
}

public enum STPDResultPackageError: Error, LocalizedError, Sendable {
    case invalidRunIdentity(String)
    case invalidInput(String)
    case invalidTable(table: String, reason: String)
    case duplicatePrimaryKey(table: String, key: String)
    case foreignKeyViolation(table: String, column: String, value: String)
    case missingTable(String)
    case destinationAlreadyExists(String)

    public var errorDescription: String? {
        switch self {
        case .invalidRunIdentity(let reason):
            return "Invalid detector run identity: \(reason)"
        case .invalidInput(let reason):
            return "Invalid result-package input: \(reason)"
        case .invalidTable(let table, let reason):
            return "Invalid result table \(table): \(reason)"
        case .duplicatePrimaryKey(let table, let key):
            return "Duplicate primary key in \(table): \(key)"
        case .foreignKeyViolation(let table, let column, let value):
            return "Foreign-key violation in \(table).\(column): \(value)"
        case .missingTable(let name):
            return "Missing required result table: \(name)"
        case .destinationAlreadyExists(let path):
            return "Result-package destination already exists: \(path)"
        }
    }
}

public enum STPDResultPackageBuilder {
    public static func build(_ input: STPDResultPackageInput) throws -> STPDResultPackage {
        let identity = input.run.runIdentity
        try validateDeclaredIdentity(
            identity,
            dataset: input.dataset,
            run: input.run
        )
        let automaticEvents = input.run.eventAnnotations(
            in: input.dataset,
            tracks: [.event, .gap, .state]
        )
        let automaticRows = ReviewedISIExportBuilder.build(
            dataset: input.dataset,
            autoAnnotations: automaticEvents,
            projectionsByTrain: [:]
        )
        let candidates = try candidateRecords(
            candidates: input.run.candidates,
            datasetDigest: identity.datasetDigest,
            dataset: input.dataset
        )
        let candidateUIDBySourceID = Dictionary(
            uniqueKeysWithValues: candidates.map { ($0.candidate.id, $0.uid) }
        )
        let candidateBySourceID = Dictionary(
            uniqueKeysWithValues: candidates.map { ($0.candidate.id, $0.candidate) }
        )
        let resolvedManualAnnotations = try resolvedManualAnnotations(
            input.manualAnnotations,
            dataset: input.dataset
        )
        let reviewAuthority = try resolvedReviewAuthority(
            input,
            automaticEvents: automaticEvents,
            automaticRows: automaticRows,
            resolvedManualAnnotations: resolvedManualAnnotations,
            candidateUIDBySourceID: candidateUIDBySourceID,
            candidateBySourceID: candidateBySourceID
        )
        try validateSourceMode(input, reviewAuthority: reviewAuthority)
        let isiRows = try normalizedISIRows(
            input.finalISILabelRows,
            dataset: input.dataset,
            authoritativeAutomaticRows: automaticRows
        )
        let isiUIDByKey = Dictionary(
            uniqueKeysWithValues: isiRows.map { row in
                (
                    isiRowKey(row),
                    stableISIUID(
                        datasetDigest: identity.datasetDigest,
                        trainID: row.trainID,
                        isiIndex: row.isiIndex
                    )
                )
            }
        )
        guard isiProjectionFingerprint(isiRows)
                == isiProjectionFingerprint(reviewAuthority.authoritativeRows) else {
            throw STPDResultPackageError.invalidInput(
                "final ISI rows do not equal the causal automatic/manual/review projection"
            )
        }
        guard projectionFingerprint(input.finalEvents)
                == projectionFingerprint(reviewAuthority.projectedEvents) else {
            throw STPDResultPackageError.invalidInput(
                "final events do not equal the causal automatic/manual/review projection"
            )
        }
        let events = try eventRecords(
            automaticEvents: input.finalEvents,
            finalISIRows: isiRows,
            datasetDigest: identity.datasetDigest,
            candidateUIDBySourceID: candidateUIDBySourceID,
            candidateBySourceID: candidateBySourceID,
            dataset: input.dataset,
            evidenceUIDsByISIKey: reviewAuthority.evidenceUIDsByISIKey,
            changedISIKeys: reviewAuthority.changedISIKeys
        )
        try validateFinalProjection(
            events: input.finalEvents,
            normalizedEvents: events,
            isiRows: isiRows,
            dataset: input.dataset
        )

        var tables: [STPDResultTable: STPDResultTableData] = [:]
        tables[.runMetadata] = try runMetadataTable(
            input: input,
            candidateCount: candidates.count,
            finalEventCount: events.count,
            finalISICount: isiRows.count
        )
        tables[.parametersReport] = try parametersTable(identity: identity)
        tables[.resolvedParameters] = try resolvedParametersTable(
            identity: identity,
            run: input.run,
            dataset: input.dataset
        )
        tables[.candidateLedger] = try candidateLedgerTable(
            identity: identity,
            records: candidates
        )
        tables[.candidateFeatures] = try candidateFeaturesTable(
            identity: identity,
            records: candidates,
            candidateUIDBySourceID: candidateUIDBySourceID
        )
        tables[.finalDecisions] = try finalDecisionsTable(
            identity: identity,
            records: candidates
        )
        tables[.eventsFinal] = try eventsTable(
            identity: identity,
            records: events,
            evidenceUIDsByISIKey: reviewAuthority.evidenceUIDsByISIKey,
            changedISIKeys: reviewAuthority.changedISIKeys
        )
        tables[.isiLabelsFinal] = try isiLabelsTable(
            identity: identity,
            rows: isiRows,
            candidateUIDBySourceID: candidateUIDBySourceID,
            evidenceUIDsByISIKey: reviewAuthority.evidenceUIDsByISIKey,
            changedISIKeys: reviewAuthority.changedISIKeys,
            qualitySettings: input.run.qualitySettings,
            dataset: input.dataset
        )
        tables[.candidateDiagnosticAudit] = try diagnosticsTable(
            identity: identity,
            candidates: candidates,
            events: events,
            supplied: input.candidateDiagnostics,
            candidateUIDBySourceID: candidateUIDBySourceID
        )
        tables[.manualAnnotations] = try manualAnnotationsTable(
            identity: identity,
            annotations: resolvedManualAnnotations,
            manualUIDByUUID: reviewAuthority.manualUIDByUUID,
            linkedISIKeysByEvidenceUID: reviewAuthority.linkedISIKeysByEvidenceUID,
            isiUIDByKey: isiUIDByKey
        )
        tables[.reviewStatus] = try reviewStatusTable(
            identity: identity,
            reviews: input.candidateReviews,
            candidateUIDBySourceID: candidateUIDBySourceID,
            reviewUIDBySourceCandidateID: reviewAuthority.reviewUIDBySourceCandidateID,
            linkedISIKeysByEvidenceUID: reviewAuthority.linkedISIKeysByEvidenceUID,
            isiUIDByKey: isiUIDByKey
        )
        tables[.hfsBurstArbitrationAudit] = try hfsAuditTable(
            identity: identity,
            rows: input.run.hfsBurstArbitrationAuditRows,
            candidateUIDBySourceID: candidateUIDBySourceID,
            candidateBySourceID: candidateBySourceID,
            dataset: input.dataset,
            finalEvents: events,
            sourceMode: input.sourceMode
        )

        let checks = try STPDResultPackageValidator.validate(
            identity: identity,
            sourceMode: input.sourceMode,
            tables: tables,
            expectedISICount: input.dataset.trains.reduce(0) { $0 + max(0, $1.spikeCount - 1) }
        )
        tables[.resultConsistencyCheck] = try consistencyTable(
            identity: identity,
            checks: checks
        )

        let completeChecks = try STPDResultPackageValidator.validate(
            identity: identity,
            sourceMode: input.sourceMode,
            tables: tables,
            expectedISICount: input.dataset.trains.reduce(0) { $0 + max(0, $1.spikeCount - 1) }
        )
        tables[.resultConsistencyCheck] = try consistencyTable(
            identity: identity,
            checks: completeChecks
        )
        _ = try STPDResultPackageValidator.validate(
            identity: identity,
            sourceMode: input.sourceMode,
            tables: tables,
            expectedISICount: input.dataset.trains.reduce(0) { $0 + max(0, $1.spikeCount - 1) }
        )

        let manifestTables = try STPDResultTable.allCases.map { table in
            guard let data = tables[table] else {
                throw STPDResultPackageError.missingTable(table.rawValue)
            }
            return STPDResultManifestTable(
                fileName: table.rawValue,
                grain: data.contract.grain,
                primaryKey: data.contract.primaryKey,
                columns: data.columnDefinitions,
                rowCount: data.rowCount,
                sha256: STPDStableIdentifier.digest(data.csvData)
            )
        }
        let manifest = STPDResultManifest(
            schemaVersion: identity.resultSchemaVersion,
            detectorVersion: identity.detectorVersion,
            runID: identity.runID,
            datasetDigest: identity.datasetDigest,
            settingsDigest: identity.settingsDigest,
            buildIdentifier: identity.buildCommit,
            buildIdentifierKind: "caller_supplied_unattested",
            buildReproducibilityAttested: false,
            sourceMode: input.sourceMode.rawValue,
            ownerName: STPDResultPackageOwnership.ownerName,
            ownerEmail: STPDResultPackageOwnership.ownerEmail,
            stringListEncoding: "json_array_utf8",
            tables: manifestTables
        )
        return STPDResultPackage(
            identity: identity,
            sourceMode: input.sourceMode,
            tables: tables,
            manifest: manifest
        )
    }
}

private struct STPDCandidateRecord {
    let candidate: ClassicAnchorCandidate
    let uid: String
}

private struct STPDNormalizedEvent {
    let sourceEventIDs: [String]
    let trainID: String
    let trainName: String
    let finalLabel: String
    let stateTonicSubtype: String
    let stateHighFrequencySubtypes: [String]
    let semanticTrack: String
    let eventTrackClass: String
    let lockLevel: String
    let startISIIndex: Int
    let endISIIndex: Int
    /// One-based scientific spike ordinals. These are not zero-based Swift array indices.
    let startSpikeOrdinal: Int
    let endSpikeOrdinal: Int
    let rawStartSec: Double
    let rawEndSec: Double
    let alignedStartSec: Double
    let alignedEndSec: Double
    let score: Double?
    let priority: Int?
    let automaticSupportISIIndices: [Int]
    let auditRecommendedSubtype: String
    let auditReviewStatus: String
    let decisionPath: String
    let authorityOrigin: String
    let automaticEventSources: [ClassicAnchorAutomaticEventSource]
}

private struct STPDEventRecord {
    let event: STPDNormalizedEvent
    let uid: String
    let sourceCandidateUIDs: [String]
    let unresolvedSourceCandidateIDs: [String]
}

private struct STPDFinalEventSegment {
    let trainID: String
    let trainName: String
    let finalPattern: String
    let finalSubtype: String
    let startISIIndex: Int
    let endISIIndex: Int
    let evidenceUIDs: [String]
    let rows: [ReviewedISIExportRow]
}

private struct STPDConsistencyCheck {
    let id: String
    let status: String
    let severity: String
    let details: String
}

private struct STPDResolvedReviewAuthority {
    let projectedEvents: [ClassicAnchorEventAnnotation]
    let authoritativeRows: [ReviewedISIExportRow]
    let manualUIDByUUID: [UUID: String]
    let reviewUIDBySourceCandidateID: [String: String]
    let evidenceUIDsByISIKey: [String: [String]]
    let linkedISIKeysByEvidenceUID: [String: [String]]
    let changedISIKeys: Set<String>
    let manualPositiveISIKeys: Set<String>
}

private extension STPDResultPackageBuilder {
    static func eventProjectionMatches(
        row: ReviewedISIExportRow,
        finalLabel: String,
        auditRecommendedSubtype: String,
        allowsManualPositive: Bool
    ) -> Bool {
        guard row.finalPattern == finalLabel else {
            return false
        }
        // Manual-positive rows have no detector subtype. They form their own manual-authority
        // segment and must never inherit an automatic event's detector-only subtype/audit fields.
        if row.finalSource == ReviewedISIExportBuilder.sourceManualPositive {
            return allowsManualPositive && row.finalSubtype.isEmpty
        }
        return row.finalSubtype == auditRecommendedSubtype
    }

    static func validateDeclaredIdentity(
        _ identity: DetectionRunIdentity,
        dataset: SpikeDataset,
        run: ClassicAnchorDetectionRun
    ) throws {
        guard run.hasValidResultPackageAuthority else {
            throw STPDResultPackageError.invalidRunIdentity(
                "result package requires an internally sealed detector-produced run"
            )
        }
        guard identity.hasCompleteDeclaredRunIdentity else {
            throw STPDResultPackageError.invalidRunIdentity(
                "authoritative run UUID, input digests, settings snapshot, and build identifier are required"
            )
        }
        let actual = DetectionDatasetSnapshot.make(dataset: dataset)
        guard actual.digest == identity.datasetDigest else {
            throw STPDResultPackageError.invalidRunIdentity("dataset digest does not match package dataset")
        }
        guard actual.trainCount == identity.trainCount,
              actual.spikeCount == identity.spikeCount,
              actual.taskEventCount == identity.taskEventCount else {
            throw STPDResultPackageError.invalidRunIdentity("declared dataset counts do not match package dataset")
        }
        guard let metadataSnapshot = run.datasetMetadataSnapshot,
              metadataSnapshot == DetectionDatasetMetadataSnapshot.make(dataset: dataset) else {
            throw STPDResultPackageError.invalidRunIdentity(
                "dataset name/source metadata is not the detector entry-point snapshot"
            )
        }
        guard let snapshot = identity.settingsSnapshot else {
            throw STPDResultPackageError.invalidRunIdentity("settings snapshot is unavailable")
        }
        guard let invocationSnapshot = run.invocationSettingsSnapshot else {
            throw STPDResultPackageError.invalidRunIdentity(
                "detector entry-point settings evidence is unavailable"
            )
        }
        guard invocationSnapshot == snapshot,
              invocationSnapshot.digest == identity.settingsDigest else {
            throw STPDResultPackageError.invalidRunIdentity(
                "declared settings snapshot is not the detector entry-point invocation snapshot"
            )
        }
        let declared = Dictionary(uniqueKeysWithValues: snapshot.entries.map { ($0.key, $0.value) })
        let requiredSettings = [
            "band.min_valid_isi_sec":
                STPDCanonicalValue.double(run.bandSettings.minValidISISec),
            "band.histogram_bin_width_sec":
                STPDCanonicalValue.double(run.bandSettings.histogramBinWidthSec),
            "band.dataset_isi_boundary_floor_sec":
                STPDCanonicalValue.double(run.bandSettings.datasetISIBoundaryFloorSec),
            "quality.artifact_threshold_sec":
                STPDCanonicalValue.double(run.qualitySettings.artifactThresholdSec),
            "quality.refractory_suspect_threshold_sec":
                STPDCanonicalValue.double(run.qualitySettings.refractorySuspectThresholdSec),
            "quality.display_unit": run.qualitySettings.displayUnit.rawValue,
        ]
        let mismatchedSettings = requiredSettings.compactMap { key, expected -> String? in
            guard declared[key] == expected else {
                return "\(key):declared=\(declared[key] ?? "missing"),actual=\(expected)"
            }
            return nil
        }
        guard mismatchedSettings.isEmpty else {
            throw STPDResultPackageError.invalidRunIdentity(
                "settings snapshot contradicts the detector run: " +
                    mismatchedSettings.sorted().joined(separator: "|")
            )
        }

        let expectedTrainIDs = Set(dataset.trains.map(\.id))
        let trainsByID = Dictionary(uniqueKeysWithValues: dataset.trains.map { ($0.id, $0) })

        let resultIDs = run.results.map(\.trainID)
        let duplicateResultIDs = Dictionary(grouping: resultIDs, by: { $0 })
            .filter { $0.value.count > 1 }
            .keys
            .sorted()
        guard duplicateResultIDs.isEmpty,
              run.results.count == dataset.trains.count,
              Set(resultIDs) == expectedTrainIDs else {
            let missing = expectedTrainIDs.subtracting(resultIDs).sorted()
            let extra = Set(resultIDs).subtracting(expectedTrainIDs).sorted()
            throw STPDResultPackageError.invalidInput(
                "detector results must cover every train exactly once; " +
                    "duplicates=\(duplicateResultIDs.joined(separator: "|"));" +
                    "missing=\(missing.joined(separator: "|"));" +
                    "extra=\(extra.joined(separator: "|"))"
            )
        }
        let datasetProfileCandidates = run.results
            .flatMap(\.candidates)
            .filter { $0.trainID == "__dataset__" }
        guard datasetProfileCandidates.count <= 1,
              datasetProfileCandidates.allSatisfy(isValidDatasetProfileCandidate) else {
            throw STPDResultPackageError.invalidInput(
                "dataset-scoped detector evidence must be at most one well-formed structural seed profile"
            )
        }
        for result in run.results {
            guard let train = trainsByID[result.trainID],
                  train.name == result.trainName else {
                throw STPDResultPackageError.invalidInput(
                    "detector result \(result.trainID) has an unknown or mismatched train identity"
                )
            }
            let mismatchedCandidateIDs = result.candidates.compactMap { candidate -> String? in
                (candidate.trainID == result.trainID &&
                    candidate.trainName == result.trainName) ||
                    isValidDatasetProfileCandidate(candidate)
                    ? nil
                    : candidate.id
            }
            guard mismatchedCandidateIDs.isEmpty else {
                throw STPDResultPackageError.invalidInput(
                    "detector result \(result.trainID) has cross-train or mismatched candidates: " +
                        mismatchedCandidateIDs.sorted().joined(separator: "|")
                )
            }
            let mismatchedAuditIDs =
                result.hfsBurstArbitrationAuditRows.compactMap { row -> String? in
                    row.trainID == result.trainID &&
                        row.trainName == result.trainName
                        ? nil
                        : row.id
                }
            guard mismatchedAuditIDs.isEmpty else {
                throw STPDResultPackageError.invalidInput(
                    "detector result \(result.trainID) has cross-train or mismatched HFS audit rows: " +
                        mismatchedAuditIDs.sorted().joined(separator: "|")
                )
            }
        }

        let evidenceIDs = run.resolvedThresholdEvidence.map(\.trainID)
        let duplicateEvidenceIDs = Dictionary(grouping: evidenceIDs, by: { $0 })
            .filter { $0.value.count > 1 }
            .keys
            .sorted()
        guard duplicateEvidenceIDs.isEmpty,
              run.resolvedThresholdEvidence.count == dataset.trains.count,
              Set(evidenceIDs) == expectedTrainIDs else {
            let missing = expectedTrainIDs.subtracting(evidenceIDs).sorted()
            let extra = Set(evidenceIDs).subtracting(expectedTrainIDs).sorted()
            throw STPDResultPackageError.invalidInput(
                "final resolved-threshold evidence must cover every train exactly once; " +
                    "duplicates=\(duplicateEvidenceIDs.joined(separator: "|"));" +
                    "missing=\(missing.joined(separator: "|"));" +
                    "extra=\(extra.joined(separator: "|"))"
            )
        }
        for evidence in run.resolvedThresholdEvidence {
            guard let train = trainsByID[evidence.trainID],
                  train.name == evidence.trainName,
                  !evidence.stagePath.isEmpty else {
                throw STPDResultPackageError.invalidInput(
                    "resolved-threshold evidence \(evidence.trainID) has unknown or incomplete provenance"
                )
            }
            try validateResolvedThresholdEvidence(evidence)
        }

        let resolutionIDs = run.resolutions.map(\.trainID)
        let duplicateResolutionIDs = Dictionary(grouping: resolutionIDs, by: { $0 })
            .filter { $0.value.count > 1 }
            .keys
            .sorted()
        guard duplicateResolutionIDs.isEmpty,
              run.resolutions.count == dataset.trains.count,
              Set(resolutionIDs) == expectedTrainIDs else {
            let missing = expectedTrainIDs.subtracting(resolutionIDs).sorted()
            let extra = Set(resolutionIDs).subtracting(expectedTrainIDs).sorted()
            throw STPDResultPackageError.invalidInput(
                "adaptive-band resolutions must cover every train exactly once; " +
                    "duplicates=\(duplicateResolutionIDs.joined(separator: "|"));" +
                    "missing=\(missing.joined(separator: "|"));" +
                    "extra=\(extra.joined(separator: "|"))"
            )
        }
        for resolution in run.resolutions {
            guard let train = trainsByID[resolution.trainID],
                  train.name == resolution.trainName else {
                throw STPDResultPackageError.invalidInput(
                    "adaptive-band resolution \(resolution.trainID) has an unknown or mismatched train"
                )
            }
            let expectedValidCount = TrainAdaptiveBandResolver.validISIs(
                train: train,
                minValidISISec: run.bandSettings.minValidISISec
            ).count
            guard canonicalEqual(
                resolution.minValidISISec,
                run.bandSettings.minValidISISec
            ),
            canonicalEqual(
                resolution.histogramBinWidthSec,
                run.bandSettings.histogramBinWidthSec
            ),
            resolution.validISICount == expectedValidCount,
            resolution.seedBandProfile.nValidISI == expectedValidCount,
            canonicalEqual(
                resolution.seedBandProfile.datasetISIBoundaryFloorSec,
                run.bandSettings.datasetISIBoundaryFloorSec
            ) else {
                throw STPDResultPackageError.invalidInput(
                    "adaptive-band resolution \(resolution.trainID) contradicts its run settings or train data"
                )
            }
            try validateAdaptiveBandResolution(resolution)
        }
    }

    static func isValidDatasetProfileCandidate(
        _ candidate: ClassicAnchorCandidate
    ) -> Bool {
        candidate.trainID == "__dataset__" &&
            candidate.trainName == "Dataset structural seed profile" &&
            candidate.candidateLayer == "structural_dataset_seed_profile" &&
            candidate.candidateClass == "dataset_profile" &&
            candidate.finalLabel == .profile &&
            candidate.gateStatus == "profile" &&
            candidate.action == "audit_only" &&
            candidate.selectedForAuto == false &&
            candidate.selectionStatus == "not_selected" &&
            candidate.startISIIndex == 0 &&
            candidate.endISIIndex == 0 &&
            candidate.startSpikeIndex == 0 &&
            candidate.endSpikeIndex == 0 &&
            candidate.nISI == 0 &&
            candidate.anchorFamily == "structural_dataset_seed_profile" &&
            candidate.anchorLockLevel == .auditOnly
    }

    static func validateResolvedThresholdEvidence(
        _ evidence: ClassicAnchorResolvedThresholdEvidence
    ) throws {
        let profile = evidence.effectiveProfile
        let finiteNonnegative: (Double) -> Bool = { $0.isFinite && $0 >= 0 }
        let families = [
            ("burst", profile.burst, false),
            ("hfs", profile.hfs, false),
            ("hf_tonic", profile.hfTonic, false),
            ("tonic", profile.tonic, false),
            ("pause", profile.pause, true),
        ]
        for (name, family, allowsInfiniteUpper) in families {
            guard finiteNonnegative(family.lowerSec),
                  finiteNonnegative(family.upperSec) ||
                    (allowsInfiniteUpper && family.upperSec == .infinity),
                  family.lowerSec <= family.upperSec,
                  family.bridgeUpperSec == nil ||
                    (finiteNonnegative(family.bridgeUpperSec ?? -.infinity) &&
                        (family.bridgeUpperSec ?? -.infinity) >= family.upperSec),
                  family.minDurationSec == nil ||
                    finiteNonnegative(family.minDurationSec ?? -.infinity),
                  [
                      family.minSpikes,
                      family.classicMaxSpikes,
                      family.longMinSpikes,
                      family.longMaxSpikes,
                  ].compactMap({ $0 }).allSatisfy({ $0 >= 0 }) else {
                throw STPDResultPackageError.invalidInput(
                    "resolved-threshold evidence \(evidence.trainID) has invalid \(name) values"
                )
            }
        }
        let duplicateKeys = Dictionary(
            grouping: evidence.resolutionProvenance,
            by: \.key
        )
        .filter { $0.value.count > 1 }
        .keys
        .sorted()
        guard duplicateKeys.isEmpty,
              evidence.resolutionProvenance.allSatisfy({
                  [$0.adaptiveValue, $0.userValue, $0.effectiveValue]
                    .compactMap { $0 }
                    .allSatisfy(finiteNonnegative)
              }) else {
            throw STPDResultPackageError.invalidInput(
                "resolved-threshold evidence \(evidence.trainID) has duplicate or invalid provenance"
            )
        }
    }

    static func validateAdaptiveBandResolution(
        _ resolution: TrainAdaptiveBandResolution
    ) throws {
        let expectedPatterns = Set(AdaptiveBandPattern.allCases)
        guard Set(resolution.bands.keys) == expectedPatterns else {
            throw STPDResultPackageError.invalidInput(
                "adaptive-band resolution \(resolution.trainID) must contain one band per pattern"
            )
        }
        for band in resolution.bands.values {
            let bounds = [
                band.seedLowerSec,
                band.seedUpperSec,
                band.bridgeUpperSec,
            ]
            guard bounds.allSatisfy({ $0.isFinite && $0 >= 0 }),
                  band.contrastS == nil ||
                    (band.contrastS?.isFinite == true && (band.contrastS ?? 0) >= 0),
                  band.seedLowerSec <= band.seedUpperSec,
                  band.seedUpperSec <= band.bridgeUpperSec else {
                throw STPDResultPackageError.invalidInput(
                    "adaptive-band resolution \(resolution.trainID) has invalid \(band.pattern.rawValue) bounds"
                )
            }
        }

        let expectedKeys = Set(
            AdaptiveBandPattern.allCases.flatMap { pattern in
                AdaptiveBandField.allCases.map { field in
                    "\(pattern.rawValue)::\(field.rawValue)"
                }
            }
        )
        let keyedRows = Dictionary(
            grouping: resolution.thresholdRows,
            by: { "\($0.pattern.rawValue)::\($0.field.rawValue)" }
        )
        guard Set(keyedRows.keys) == expectedKeys,
              keyedRows.values.allSatisfy({ $0.count == 1 }) else {
            throw STPDResultPackageError.invalidInput(
                "adaptive-band resolution \(resolution.trainID) must contain one threshold row per pattern and field"
            )
        }
        for threshold in resolution.thresholdRows {
            guard threshold.effectiveSec == nil ||
                    (threshold.effectiveSec?.isFinite == true &&
                        (threshold.effectiveSec ?? 0) >= 0),
                  threshold.histogramSec == nil ||
                    (threshold.histogramSec?.isFinite == true &&
                        (threshold.histogramSec ?? 0) >= 0),
                  threshold.defaultSec == nil ||
                    (threshold.defaultSec?.isFinite == true &&
                        (threshold.defaultSec ?? 0) >= 0),
                  let band = resolution.bands[threshold.pattern] else {
                throw STPDResultPackageError.invalidInput(
                    "adaptive-band resolution \(resolution.trainID) has invalid threshold evidence"
                )
            }
            let expected: Double?
            switch threshold.field {
            case .seedLowerSec:
                expected = band.seedLowerSec
            case .seedUpperSec:
                expected = band.seedUpperSec
            case .bridgeUpperSec:
                expected = band.bridgeUpperSec
            case .contrastS:
                expected = band.contrastS
            }
            guard canonicalEqual(threshold.effectiveSec, expected) else {
                throw STPDResultPackageError.invalidInput(
                    "adaptive-band resolution \(resolution.trainID) threshold rows contradict final bands"
                )
            }
            switch threshold.source {
            case .histogram:
                guard threshold.histogramSec != nil else {
                    throw STPDResultPackageError.invalidInput(
                        "histogram threshold source lacks histogram evidence"
                    )
                }
            case .default:
                guard threshold.defaultSec != nil else {
                    throw STPDResultPackageError.invalidInput(
                        "default threshold source lacks default evidence"
                    )
                }
            case .structure, .none:
                break
            }
        }
        for band in resolution.bands.values {
            let seedUpper = keyedRows[
                "\(band.pattern.rawValue)::\(AdaptiveBandField.seedUpperSec.rawValue)"
            ]?.first
            guard band.primarySource == seedUpper?.source else {
                throw STPDResultPackageError.invalidInput(
                    "adaptive-band resolution \(resolution.trainID) primary source contradicts seed upper provenance"
                )
            }
        }
    }

    static func candidateRecords(
        candidates: [ClassicAnchorCandidate],
        datasetDigest: String,
        dataset: SpikeDataset
    ) throws -> [STPDCandidateRecord] {
        let duplicateSourceIDs = Dictionary(grouping: candidates, by: \.id)
            .filter { $0.value.count > 1 }
            .keys
            .sorted()
        guard duplicateSourceIDs.isEmpty else {
            throw STPDResultPackageError.invalidInput(
                "candidate source IDs are not unique: \(duplicateSourceIDs.joined(separator: "|"))"
            )
        }
        let trainsByID = Dictionary(uniqueKeysWithValues: dataset.trains.map { ($0.id, $0) })
        let candidatesByID = Dictionary(uniqueKeysWithValues: candidates.map { ($0.id, $0) })
        for candidate in candidates {
            try validateCandidate(candidate, trainsByID: trainsByID)
            try validateHFSuppressorReference(
                candidate,
                candidatesByID: candidatesByID
            )
        }

        let intrinsicUIDBySourceID = Dictionary(
            uniqueKeysWithValues: candidates.map { candidate in
                (
                    candidate.id,
                    STPDStableIdentifier.make(
                        prefix: "cand_intrinsic",
                        domain: "stpd_candidate_intrinsic_uid_v1",
                        components: [datasetDigest] +
                            candidateIntrinsicIdentityComponents(candidate)
                    )
                )
            }
        )
        let sorted = candidates.sorted {
            let lhs = candidateIdentityComponents(
                $0,
                intrinsicUIDBySourceID: intrinsicUIDBySourceID
            )
            let rhs = candidateIdentityComponents(
                $1,
                intrinsicUIDBySourceID: intrinsicUIDBySourceID
            )
            if lhs != rhs {
                return lhs.lexicographicallyPrecedes(rhs)
            }
            return $0.id < $1.id
        }
        let duplicateScientificIdentities = Dictionary(
            grouping: sorted,
            by: {
                compositeKey(
                    candidateIdentityComponents(
                        $0,
                        intrinsicUIDBySourceID: intrinsicUIDBySourceID
                    )
                )
            }
        )
        .filter { $0.value.count > 1 }
        .values
        .map { $0.map(\.id).sorted().joined(separator: "|") }
        .sorted()
        guard duplicateScientificIdentities.isEmpty else {
            throw STPDResultPackageError.invalidInput(
                "scientifically duplicate candidates are ambiguous: " +
                    duplicateScientificIdentities.joined(separator: ",")
            )
        }
        return sorted.map { candidate in
            let base = candidateIdentityComponents(
                candidate,
                intrinsicUIDBySourceID: intrinsicUIDBySourceID
            )
            let uid = STPDStableIdentifier.make(
                prefix: "cand",
                domain: "stpd_candidate_uid_v2",
                components: [datasetDigest] + base
            )
            return STPDCandidateRecord(candidate: candidate, uid: uid)
        }
    }

    static func eventRecords(
        automaticEvents: [ClassicAnchorEventAnnotation],
        finalISIRows: [ReviewedISIExportRow],
        datasetDigest: String,
        candidateUIDBySourceID: [String: String],
        candidateBySourceID: [String: ClassicAnchorCandidate],
        dataset: SpikeDataset,
        evidenceUIDsByISIKey: [String: [String]],
        changedISIKeys: Set<String>
    ) throws -> [STPDEventRecord] {
        let trainsByID = Dictionary(uniqueKeysWithValues: dataset.trains.map { ($0.id, $0) })
        let duplicateEventIDs = Dictionary(grouping: automaticEvents, by: \.id)
            .filter { $0.value.count > 1 }
            .keys
            .sorted()
        guard duplicateEventIDs.isEmpty else {
            throw STPDResultPackageError.invalidInput(
                "final event source IDs are not unique: \(duplicateEventIDs.joined(separator: "|"))"
            )
        }
        let finalRowsByKey = Dictionary(
            uniqueKeysWithValues: finalISIRows.map { (isiRowKey($0), $0) }
        )
        for event in automaticEvents {
            guard let train = trainsByID[event.trainID] else {
                throw STPDResultPackageError.invalidInput(
                    "event \(event.id) references unknown train \(event.trainID)"
                )
            }
            try validateEvent(
                event,
                train: train,
                candidateUIDBySourceID: candidateUIDBySourceID,
                candidateBySourceID: candidateBySourceID
            )
        }

        func scientificIdentity(event: STPDNormalizedEvent) -> [String] {
            return [
                event.trainID,
                event.finalLabel,
                event.stateTonicSubtype,
                STPDCanonicalValue.stringList(event.stateHighFrequencySubtypes),
                String(event.startISIIndex),
                String(event.endISIIndex),
                String(event.startSpikeOrdinal),
                String(event.endSpikeOrdinal),
                STPDCanonicalValue.double(event.rawStartSec),
                STPDCanonicalValue.double(event.rawEndSec),
                STPDCanonicalValue.double(event.alignedStartSec),
                STPDCanonicalValue.double(event.alignedEndSec),
            ]
        }

        var occupiedISIKeys = Set<String>()
        var records: [STPDEventRecord] = []
        for sourceEvent in automaticEvents {
            guard let train = trainsByID[sourceEvent.trainID] else {
                throw STPDResultPackageError.invalidInput(
                    "event \(sourceEvent.id) references unknown train \(sourceEvent.trainID)"
                )
            }
            let sourceRows = try (sourceEvent.startISISecIndex...sourceEvent.endISISecIndex)
                .map { index -> ReviewedISIExportRow in
                    let key = isiEvidenceKey(trainID: sourceEvent.trainID, isiIndex: index)
                    guard let row = finalRowsByKey[key] else {
                        throw STPDResultPackageError.invalidInput(
                            "event \(sourceEvent.id) has no authoritative final ISI row at \(index)"
                        )
                    }
                    return row
                }
            let allSourceIDs = Array(
                Set(
                    sourceEvent.sourceCandidateIDs
                        + [sourceEvent.candidateID]
                        + sourceEvent.automaticEventSources.map(\.candidateID)
                )
            )
            .filter { !$0.isEmpty }
            .sorted()
            for sourceID in allSourceIDs {
                guard candidateUIDBySourceID[sourceID] != nil else {
                    throw STPDResultPackageError.invalidInput(
                        "event \(sourceEvent.id) references unknown candidate \(sourceID)"
                    )
                }
            }
            let allAutomaticSources = sourceEvent.automaticEventSources.sorted {
                let lhsUID = candidateUIDBySourceID[$0.candidateID] ?? ""
                let rhsUID = candidateUIDBySourceID[$1.candidateID] ?? ""
                if lhsUID != rhsUID { return lhsUID < rhsUID }
                if $0.supportISIIndices != $1.supportISIIndices {
                    return $0.supportISIIndices.lexicographicallyPrecedes(
                        $1.supportISIIndices
                    )
                }
                if $0.semanticTrack != $1.semanticTrack {
                    return $0.semanticTrack.rawValue < $1.semanticTrack.rawValue
                }
                return $0.annotationID < $1.annotationID
            }

            var matchingSegments: [[ReviewedISIExportRow]] = []
            var currentSegment: [ReviewedISIExportRow] = []
            for row in sourceRows {
                if eventProjectionMatches(
                    row: row,
                    finalLabel: sourceEvent.label.rawValue,
                    auditRecommendedSubtype: sourceEvent.auditRecommendedSubtype,
                    allowsManualPositive: false
                ) {
                    currentSegment.append(row)
                } else if !currentSegment.isEmpty {
                    matchingSegments.append(currentSegment)
                    currentSegment = []
                }
            }
            if !currentSegment.isEmpty {
                matchingSegments.append(currentSegment)
            }

            for eventRows in matchingSegments {
                guard let firstRow = eventRows.first, let lastRow = eventRows.last else {
                    continue
                }
                for row in eventRows {
                    let key = isiRowKey(row)
                    guard occupiedISIKeys.insert(key).inserted else {
                        throw STPDResultPackageError.invalidInput(
                            "authoritative final events overlap at \(row.trainID):\(row.isiIndex)"
                        )
                    }
                }

                let segmentLower = firstRow.isiIndex
                let segmentUpper = lastRow.isiIndex
                let segmentISIs = Set(segmentLower...segmentUpper)
                let automaticSources = allAutomaticSources.compactMap {
                    $0.retainingSupport(segmentISIs)
                }
                .sorted {
                    let lhsUID = candidateUIDBySourceID[$0.candidateID] ?? ""
                    let rhsUID = candidateUIDBySourceID[$1.candidateID] ?? ""
                    if lhsUID != rhsUID { return lhsUID < rhsUID }
                    if $0.supportISIIndices != $1.supportISIIndices {
                        return $0.supportISIIndices.lexicographicallyPrecedes(
                            $1.supportISIIndices
                        )
                    }
                    if $0.semanticTrack != $1.semanticTrack {
                        return $0.semanticTrack.rawValue < $1.semanticTrack.rawValue
                    }
                    return $0.annotationID < $1.annotationID
                }
                let sourceIDs = allSourceIDs.filter { sourceID in
                    if automaticSources.contains(where: { $0.candidateID == sourceID }) {
                        return true
                    }
                    guard let candidate = candidateBySourceID[sourceID] else {
                        return false
                    }
                    let lower = min(candidate.startISIIndex, candidate.endISIIndex)
                    let upper = max(candidate.startISIIndex, candidate.endISIIndex)
                    return lower <= segmentUpper && upper >= segmentLower
                }
                let sourceUIDs = sourceIDs.compactMap {
                    candidateUIDBySourceID[$0]
                }.sorted()
                let changed = eventRows.contains {
                    changedISIKeys.contains(isiRowKey($0))
                }
                let authorityOrigin =
                    changed || eventRows.contains {
                        $0.finalSource == ReviewedISIExportBuilder.sourceManualPositive
                    }
                    ? "manual_augmented"
                    : "automatic"
                let hfsSubtypes = Set(
                    sourceIDs.compactMap {
                        candidateBySourceID[$0]?.stateHighFrequencySubtype
                    }
                    .filter { !$0.isEmpty }
                ).sorted()
                let aligned = train.alignedTimestampsSec.count == train.spikeCount
                    ? train.alignedTimestampsSec
                    : train.timestampsSec
                let lowerTimestampIndex = segmentLower - 1
                let upperTimestampIndex = segmentUpper
                let normalized = STPDNormalizedEvent(
                    sourceEventIDs: [sourceEvent.id],
                    trainID: sourceEvent.trainID,
                    trainName: sourceEvent.trainName,
                    finalLabel: sourceEvent.label.rawValue,
                    stateTonicSubtype: sourceEvent.stateTonicSubtype ?? "",
                    stateHighFrequencySubtypes: hfsSubtypes,
                    semanticTrack: sourceEvent.semanticTrack.rawValue,
                    eventTrackClass: sourceEvent.eventTrackClass,
                    lockLevel: sourceEvent.lockLevel.rawValue,
                    startISIIndex: segmentLower,
                    endISIIndex: segmentUpper,
                    startSpikeOrdinal: segmentLower,
                    endSpikeOrdinal: segmentUpper + 1,
                    rawStartSec: train.timestampsSec[lowerTimestampIndex],
                    rawEndSec: train.timestampsSec[upperTimestampIndex],
                    alignedStartSec: aligned[lowerTimestampIndex],
                    alignedEndSec: aligned[upperTimestampIndex],
                    score: sourceEvent.score,
                    priority: sourceEvent.priority,
                    automaticSupportISIIndices:
                        sourceEvent.automaticSupportISIIndices
                        .filter(segmentISIs.contains)
                        .sorted(),
                    auditRecommendedSubtype: sourceEvent.auditRecommendedSubtype,
                    auditReviewStatus: sourceEvent.auditReviewStatus,
                    decisionPath: sourceEvent.decisionPath,
                    authorityOrigin: authorityOrigin,
                    automaticEventSources: automaticSources
                )
                let uid = STPDStableIdentifier.make(
                    prefix: "event",
                    domain: "stpd_normalized_public_event_uid_v4",
                    components: [datasetDigest] + scientificIdentity(event: normalized)
                )
                records.append(
                    STPDEventRecord(
                        event: normalized,
                        uid: uid,
                        sourceCandidateUIDs: sourceUIDs,
                        unresolvedSourceCandidateIDs: []
                    )
                )
            }
        }

        let uncoveredRows = finalISIRows.filter {
            $0.isiIndex > 0
                && !$0.finalPattern.isEmpty
                && !occupiedISIKeys.contains(isiRowKey($0))
        }
        for segment in finalEventSegments(
            uncoveredRows,
            evidenceUIDsByISIKey: evidenceUIDsByISIKey
        ) {
            guard let train = trainsByID[segment.trainID] else {
                throw STPDResultPackageError.invalidInput(
                    "manual final event segment references unknown train \(segment.trainID)"
                )
            }
            let aligned = train.alignedTimestampsSec.count == train.spikeCount
                ? train.alignedTimestampsSec
                : train.timestampsSec
            let lowerTimestampIndex = segment.startISIIndex - 1
            let upperTimestampIndex = segment.endISIIndex
            let normalized = STPDNormalizedEvent(
                sourceEventIDs: [],
                trainID: segment.trainID,
                trainName: segment.trainName,
                finalLabel: segment.finalPattern,
                stateTonicSubtype: segment.finalSubtype,
                stateHighFrequencySubtypes: [],
                semanticTrack: manualSemanticTrack(for: segment.finalPattern),
                eventTrackClass: segment.finalPattern,
                lockLevel: "manual_authority",
                startISIIndex: segment.startISIIndex,
                endISIIndex: segment.endISIIndex,
                startSpikeOrdinal: segment.startISIIndex,
                endSpikeOrdinal: segment.endISIIndex + 1,
                rawStartSec: train.timestampsSec[lowerTimestampIndex],
                rawEndSec: train.timestampsSec[upperTimestampIndex],
                alignedStartSec: aligned[lowerTimestampIndex],
                alignedEndSec: aligned[upperTimestampIndex],
                score: nil,
                priority: nil,
                automaticSupportISIIndices: [],
                auditRecommendedSubtype: "manual_positive",
                auditReviewStatus: "manual",
                decisionPath: "manual_positive_public_projection",
                authorityOrigin: "manual_only",
                automaticEventSources: []
            )
            let uid = STPDStableIdentifier.make(
                prefix: "event",
                domain: "stpd_normalized_public_event_uid_v4",
                components: [datasetDigest] + scientificIdentity(event: normalized)
            )
            records.append(
                STPDEventRecord(
                    event: normalized,
                    uid: uid,
                    sourceCandidateUIDs: [],
                    unresolvedSourceCandidateIDs: []
                )
            )
        }
        let duplicateUIDs = Dictionary(grouping: records, by: \.uid)
            .filter { $0.value.count > 1 }
            .keys
        guard duplicateUIDs.isEmpty else {
            throw STPDResultPackageError.invalidInput(
                "scientifically duplicate normalized final events are ambiguous"
            )
        }
        return records.sorted {
            if $0.event.trainID != $1.event.trainID {
                return $0.event.trainID < $1.event.trainID
            }
            if $0.event.startISIIndex != $1.event.startISIIndex {
                return $0.event.startISIIndex < $1.event.startISIIndex
            }
            if $0.event.endISIIndex != $1.event.endISIIndex {
                return $0.event.endISIIndex < $1.event.endISIIndex
            }
            return $0.uid < $1.uid
        }
    }

    static func finalEventSegments(
        _ rows: [ReviewedISIExportRow],
        evidenceUIDsByISIKey: [String: [String]]
    ) -> [STPDFinalEventSegment] {
        func evidenceUIDs(for row: ReviewedISIExportRow) -> [String] {
            Array(Set(evidenceUIDsByISIKey[isiRowKey(row)] ?? [])).sorted()
        }

        let positive = rows
            .filter { $0.isiIndex > 0 && !$0.finalPattern.isEmpty }
            .sorted {
                if $0.trainID != $1.trainID { return $0.trainID < $1.trainID }
                return $0.isiIndex < $1.isiIndex
            }
        var segments: [STPDFinalEventSegment] = []
        var current: [ReviewedISIExportRow] = []

        func appendCurrent() {
            guard let first = current.first, let last = current.last else { return }
            segments.append(
                STPDFinalEventSegment(
                    trainID: first.trainID,
                    trainName: first.trainName,
                    finalPattern: first.finalPattern,
                    finalSubtype: first.finalSubtype,
                    startISIIndex: first.isiIndex,
                    endISIIndex: last.isiIndex,
                    evidenceUIDs: evidenceUIDs(for: first),
                    rows: current
                )
            )
        }

        for row in positive {
            if let previous = current.last,
               previous.trainID == row.trainID,
               previous.finalPattern == row.finalPattern,
               previous.finalSubtype == row.finalSubtype,
               evidenceUIDs(for: previous) == evidenceUIDs(for: row),
               row.isiIndex == previous.isiIndex + 1 {
                current.append(row)
            } else {
                appendCurrent()
                current = [row]
            }
        }
        appendCurrent()
        return segments
    }

    static func manualSemanticTrack(for finalPattern: String) -> String {
        switch finalPattern {
        case ManualAnnotationLabel.burst.rawValue,
             ManualAnnotationLabel.longBurst.rawValue:
            return ClassicAnchorSemanticTrack.event.rawValue
        case ManualAnnotationLabel.pause.rawValue:
            return ClassicAnchorSemanticTrack.gap.rawValue
        default:
            return ClassicAnchorSemanticTrack.state.rawValue
        }
    }

    static func normalizedISIRows(
        _ rows: [ReviewedISIExportRow],
        dataset: SpikeDataset,
        authoritativeAutomaticRows: [ReviewedISIExportRow]
    ) throws -> [ReviewedISIExportRow] {
        guard rows.allSatisfy({ $0.isiIndex >= 0 }) else {
            throw STPDResultPackageError.invalidInput(
                "final ISI table contains a negative ISI index"
            )
        }
        let trainsByID = Dictionary(uniqueKeysWithValues: dataset.trains.map { ($0.id, $0) })
        var seenPlaceholderTrains = Set<String>()
        for row in rows where row.isiIndex == 0 {
            guard let train = trainsByID[row.trainID],
                  train.spikeCount > 0,
                  !train.timestampsSec.isEmpty else {
                throw STPDResultPackageError.invalidInput(
                    "structural ISI placeholder references an unknown or empty train \(row.trainID)"
                )
            }
            let aligned = train.alignedTimestampsSec.count == train.spikeCount
                ? train.alignedTimestampsSec
                : train.timestampsSec
            guard row.trainName == train.name,
                  row.spikeIndex == 0,
                  row.timestampSec.isFinite,
                  row.alignedTimestampSec.isFinite,
                  row.isiSec.isFinite,
                  canonicalEqual(row.timestampSec, train.timestampsSec[0]),
                  canonicalEqual(row.alignedTimestampSec, aligned[0]),
                  canonicalEqual(row.isiSec, 0),
                  row.autoPattern.isEmpty,
                  row.autoSubtype.isEmpty,
                  row.autoCandidateID.isEmpty,
                  row.finalPattern.isEmpty,
                  row.finalSubtype.isEmpty,
                  row.finalSource == ReviewedISIExportBuilder.sourceNone,
                  row.manualVetoSuppressed == false,
                  row.reviewNote.isEmpty else {
                throw STPDResultPackageError.invalidInput(
                    "ISI index 0 must be the exact structural first-spike placeholder for train \(row.trainID)"
                )
            }
            guard seenPlaceholderTrains.insert(row.trainID).inserted else {
                throw STPDResultPackageError.invalidInput(
                    "duplicate structural ISI placeholder for train \(row.trainID)"
                )
            }
        }
        let intervalRows = rows.filter { $0.isiIndex > 0 }
        let automaticByKey = Dictionary(
            uniqueKeysWithValues: authoritativeAutomaticRows
                .filter { $0.isiIndex > 0 }
                .map { (isiRowKey($0), $0) }
        )
        var seen = Set<String>()
        for row in intervalRows {
            guard let train = trainsByID[row.trainID] else {
                throw STPDResultPackageError.invalidInput(
                    "ISI row references unknown train \(row.trainID)"
                )
            }
            guard row.isiIndex >= 1, row.isiIndex < train.spikeCount else {
                throw STPDResultPackageError.invalidInput(
                    "ISI row index \(row.isiIndex) is outside train \(row.trainID)"
                )
            }
            guard row.trainName == train.name else {
                throw STPDResultPackageError.invalidInput(
                    "ISI row train name does not match dataset train \(row.trainID)"
                )
            }
            guard row.spikeIndex == row.isiIndex else {
                throw STPDResultPackageError.invalidInput(
                    "ISI row \(row.trainID):\(row.isiIndex) has inconsistent spike index"
                )
            }
            guard row.timestampSec.isFinite,
                  row.alignedTimestampSec.isFinite,
                  row.isiSec.isFinite else {
                throw STPDResultPackageError.invalidInput(
                    "ISI row \(row.trainID):\(row.isiIndex) has non-finite geometry"
                )
            }
            let aligned = train.alignedTimestampsSec.count == train.spikeCount
                ? train.alignedTimestampsSec
                : train.timestampsSec
            let expectedTimestamp = train.timestampsSec[row.isiIndex]
            let expectedAligned = aligned[row.isiIndex]
            let expectedISI = expectedTimestamp - train.timestampsSec[row.isiIndex - 1]
            guard canonicalEqual(row.timestampSec, expectedTimestamp),
                  canonicalEqual(row.alignedTimestampSec, expectedAligned),
                  canonicalEqual(row.isiSec, expectedISI) else {
                throw STPDResultPackageError.invalidInput(
                    "ISI row \(row.trainID):\(row.isiIndex) geometry differs from dataset"
                )
            }
            let key = isiRowKey(row)
            guard seen.insert(key).inserted else {
                throw STPDResultPackageError.invalidInput("duplicate final ISI row \(key)")
            }
            guard let automatic = automaticByKey[key],
                  row.autoPattern == automatic.autoPattern,
                  row.autoSubtype == automatic.autoSubtype,
                  row.autoCandidateID == automatic.autoCandidateID else {
                throw STPDResultPackageError.invalidInput(
                    "ISI row \(row.trainID):\(row.isiIndex) automatic fields differ from detector projection"
                )
            }
        }
        let expected = dataset.trains.reduce(0) { $0 + max(0, $1.spikeCount - 1) }
        guard intervalRows.count == expected else {
            throw STPDResultPackageError.invalidInput(
                "final ISI table must contain exactly one row per interval; expected \(expected), got \(intervalRows.count)"
            )
        }
        return intervalRows
    }

    static func validateSourceMode(
        _ input: STPDResultPackageInput,
        reviewAuthority: STPDResolvedReviewAuthority
    ) throws {
        switch input.sourceMode {
        case .automatic:
            guard input.manualAnnotations.isEmpty,
                  input.candidateReviews.isEmpty,
                  input.reviewLinks.isEmpty,
                  reviewAuthority.evidenceUIDsByISIKey.isEmpty else {
                throw STPDResultPackageError.invalidInput(
                    "automatic source mode cannot contain manual or review authority"
                )
            }
        case .reviewed:
            guard !input.reviewLinks.isEmpty,
                  !reviewAuthority.evidenceUIDsByISIKey.isEmpty else {
                throw STPDResultPackageError.invalidInput(
                    "reviewed source mode requires causal review links to public ISI results"
                )
            }
        }
    }

    static func resolvedReviewAuthority(
        _ input: STPDResultPackageInput,
        automaticEvents: [ClassicAnchorEventAnnotation],
        automaticRows: [ReviewedISIExportRow],
        resolvedManualAnnotations: [ManualAnnotation],
        candidateUIDBySourceID: [String: String],
        candidateBySourceID: [String: ClassicAnchorCandidate]
    ) throws -> STPDResolvedReviewAuthority {
        for diagnostic in input.candidateDiagnostics
        where candidateUIDBySourceID[diagnostic.sourceCandidateID] == nil {
            throw STPDResultPackageError.invalidInput(
                "candidate diagnostic references unknown candidate \(diagnostic.sourceCandidateID)"
            )
        }
        if input.sourceMode == .automatic {
            return STPDResolvedReviewAuthority(
                projectedEvents: automaticEvents,
                authoritativeRows: automaticRows.filter { $0.isiIndex > 0 },
                manualUIDByUUID: [:],
                reviewUIDBySourceCandidateID: [:],
                evidenceUIDsByISIKey: [:],
                linkedISIKeysByEvidenceUID: [:],
                changedISIKeys: [],
                manualPositiveISIKeys: []
            )
        }

        let annotationByID = Dictionary(
            uniqueKeysWithValues: resolvedManualAnnotations.map { ($0.id, $0) }
        )
        let reviewSourceIDs = input.candidateReviews.map(\.sourceCandidateID)
        guard Set(reviewSourceIDs).count == reviewSourceIDs.count else {
            throw STPDResultPackageError.invalidInput(
                "candidate review source IDs are not unique"
            )
        }
        let reviewBySourceID = Dictionary(
            uniqueKeysWithValues: input.candidateReviews.map {
                ($0.sourceCandidateID, $0)
            }
        )
        for review in input.candidateReviews {
            guard candidateUIDBySourceID[review.sourceCandidateID] != nil else {
                throw STPDResultPackageError.invalidInput(
                    "candidate review references unknown candidate \(review.sourceCandidateID)"
                )
            }
            if review.status.grantsReviewAuthority,
               review.reviewer.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty {
                throw STPDResultPackageError.invalidInput(
                    "authority-bearing candidate review requires a reviewer"
                )
            }
        }

        let manualUIDByUUID = Dictionary(
            uniqueKeysWithValues: resolvedManualAnnotations.map { annotation in
                (
                    annotation.id,
                    STPDStableIdentifier.make(
                        prefix: "manual",
                        domain: "stpd_manual_annotation_uid_v1",
                        components: [
                            input.run.runIdentity.datasetDigest,
                            annotation.id.uuidString.lowercased(),
                        ]
                    )
                )
            }
        )
        let reviewUIDBySourceCandidateID = try Dictionary(
            uniqueKeysWithValues: input.candidateReviews.map { review in
                guard let candidateUID = candidateUIDBySourceID[review.sourceCandidateID] else {
                    throw STPDResultPackageError.invalidInput(
                        "candidate review references unknown candidate \(review.sourceCandidateID)"
                    )
                }
                return (
                    review.sourceCandidateID,
                    STPDStableIdentifier.make(
                        prefix: "review",
                        domain: "stpd_candidate_review_uid_v1",
                        components: [
                            candidateUID,
                            review.status.rawValue,
                            review.reviewer,
                            review.note,
                            STPDCanonicalValue.date(review.reviewedAt),
                        ]
                    )
                )
            }
        )

        let trainsByID = Dictionary(
            uniqueKeysWithValues: input.dataset.trains.map { ($0.id, $0) }
        )
        let automaticIntervalRows = automaticRows.filter { $0.isiIndex > 0 }
        let automaticByKey = Dictionary(
            uniqueKeysWithValues: automaticIntervalRows.map { (isiRowKey($0), $0) }
        )
        let annotationsByTrain = Dictionary(
            grouping: resolvedManualAnnotations,
            by: \.trainID
        )
        let automaticRowsByTrain = Dictionary(
            grouping: automaticIntervalRows,
            by: \.trainID
        )
        var projectionsByTrain: [String: ManualAnnotationProjection] = [:]
        for train in input.dataset.trains {
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
                minValidISISeconds: input.run.qualitySettings.artifactThresholdSec
            )
        }

        let rejectedCandidateIDs = Set(
            input.candidateReviews.compactMap {
                $0.status == .rejected ? $0.sourceCandidateID : nil
            }
        )
        let authoritativeRows = ReviewedISIExportBuilder.build(
            dataset: input.dataset,
            autoAnnotations: automaticEvents,
            projectionsByTrain: projectionsByTrain,
            reviewRejectedCandidateIDs: rejectedCandidateIDs
        )
        .filter { $0.isiIndex > 0 }
        let authoritativeByKey = Dictionary(
            uniqueKeysWithValues: authoritativeRows.map { (isiRowKey($0), $0) }
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
        var validatedManualBurstISIsByTrain: [String: Set<Int>] = [:]
        for train in input.dataset.trains {
            let projection = projectionsByTrain[train.id]
            lockSuppressedByTrain[train.id] =
                (projection?.autoBlockedByManualLockISIs ?? [])
                .union(rejectedISIsByTrain[train.id] ?? [])
            vetoedBurstISIsByTrain[train.id] =
                projection?.autoBurstBlockedByVetoISIs ?? []
            validatedManualBurstISIsByTrain[train.id] = Set(
                (projection?.manualPositiveLabelByISI ?? [:]).compactMap {
                    ManualAnnotationProjector.burstFamilyLabels.contains($0.value)
                        ? $0.key
                        : nil
                }
            )
        }
        let projectedEvents = ManualAnnotationProjector.projectPublicEventAnnotations(
            automaticEvents,
            vetoedBurstISIsByTrain: vetoedBurstISIsByTrain,
            lockSuppressedISIsByTrain: lockSuppressedByTrain,
            validatedManualBurstSupportISIsByTrain: validatedManualBurstISIsByTrain,
            trainsByID: trainsByID
        ).annotations

        var evidenceUIDsByISIKey: [String: Set<String>] = [:]
        var linkedISIKeysByEvidenceUID: [String: Set<String>] = [:]
        var seenLinks = Set<STPDResultReviewLink>()
        for link in input.reviewLinks {
            guard seenLinks.insert(link).inserted else {
                throw STPDResultPackageError.invalidInput(
                    "duplicate review link \(link.trainID):\(link.isiIndex)"
                )
            }
            let key = "\(link.trainID)\u{1}\(link.isiIndex)"
            guard let automaticRow = automaticByKey[key],
                  authoritativeByKey[key] != nil else {
                throw STPDResultPackageError.invalidInput(
                    "review link targets an unknown dataset ISI \(link.trainID):\(link.isiIndex)"
                )
            }
            let evidenceUID: String
            switch link.evidence {
            case .manualAnnotation(let annotationID):
                guard let annotation = annotationByID[annotationID],
                      annotation.trainID == link.trainID,
                      let lower = annotation.startISIIndex,
                      let upper = annotation.endISIIndex,
                      min(lower, upper) ... max(lower, upper) ~= link.isiIndex,
                      let uid = manualUIDByUUID[annotationID] else {
                    throw STPDResultPackageError.invalidInput(
                        "manual review link does not match annotation geometry"
                    )
                }
                evidenceUID = uid
            case .candidateReview(let sourceCandidateID):
                guard let review = reviewBySourceID[sourceCandidateID],
                      review.status.grantsReviewAuthority,
                      let candidate = candidateBySourceID[sourceCandidateID],
                      candidate.trainID == link.trainID,
                      min(candidate.startISIIndex, candidate.endISIIndex)
                        ... max(candidate.startISIIndex, candidate.endISIIndex)
                        ~= link.isiIndex,
                      automaticRow.autoCandidateID == sourceCandidateID,
                      let uid = reviewUIDBySourceCandidateID[sourceCandidateID] else {
                    throw STPDResultPackageError.invalidInput(
                        "candidate review link does not match projected candidate coverage"
                    )
                }
                evidenceUID = uid
            }
            evidenceUIDsByISIKey[key, default: []].insert(evidenceUID)
            linkedISIKeysByEvidenceUID[evidenceUID, default: []].insert(key)
        }

        let manualPositiveISIKeys = Set(authoritativeRows.compactMap { row in
            row.finalSource == ReviewedISIExportBuilder.sourceManualPositive
                ? isiRowKey(row)
                : nil
        })
        for annotation in resolvedManualAnnotations {
            guard let uid = manualUIDByUUID[annotation.id] else { continue }
            let linked = linkedISIKeysByEvidenceUID[uid] ?? []
            let expected = Set(authoritativeRows.compactMap { row -> String? in
                guard row.trainID == annotation.trainID,
                      let lower = annotation.startISIIndex,
                      let upper = annotation.endISIIndex,
                      min(lower, upper) ... max(lower, upper) ~= row.isiIndex else {
                    return nil
                }
                switch annotation.polarity {
                case .positive:
                    return row.finalSource == ReviewedISIExportBuilder.sourceManualPositive
                        && row.finalPattern == annotation.label.finalPatternString
                        ? isiRowKey(row)
                        : nil
                case .negative:
                    return row.finalSource == ReviewedISIExportBuilder.sourceManualVetoRemoved
                        ? isiRowKey(row)
                        : nil
                }
            })
            guard !expected.isEmpty, linked == expected else {
                throw STPDResultPackageError.invalidInput(
                    "manual annotation \(annotation.id.uuidString) must link exactly to the ISIs it changes"
                )
            }
        }

        for review in input.candidateReviews {
            guard let uid = reviewUIDBySourceCandidateID[review.sourceCandidateID] else {
                continue
            }
            let linked = linkedISIKeysByEvidenceUID[uid] ?? []
            if review.status == .needsReview {
                guard linked.isEmpty else {
                    throw STPDResultPackageError.invalidInput(
                        "needs_review cannot authorize a public result"
                    )
                }
                continue
            }
            let expected = Set(automaticIntervalRows.compactMap {
                $0.autoCandidateID == review.sourceCandidateID ? isiRowKey($0) : nil
            })
            guard !expected.isEmpty, linked == expected else {
                throw STPDResultPackageError.invalidInput(
                    "candidate review \(review.sourceCandidateID) must link exactly to its public ISI coverage"
                )
            }
            let changed = expected.contains { key in
                guard let automatic = automaticByKey[key],
                      let reviewed = authoritativeByKey[key] else {
                    return false
                }
                return isiProjectionComponents(automatic)
                    != isiProjectionComponents(reviewed)
            }
            switch review.status {
            case .accepted:
                guard !changed else {
                    throw STPDResultPackageError.invalidInput(
                        "accepted review cannot change the public projection"
                    )
                }
            case .rejected:
                guard changed else {
                    throw STPDResultPackageError.invalidInput(
                        "rejected review must remove or replace projected candidate coverage"
                    )
                }
            case .modified:
                let hasLinkedManualEvidence = expected.contains { key in
                    (evidenceUIDsByISIKey[key] ?? []).contains {
                        $0.hasPrefix("manual_")
                    }
                }
                guard changed, hasLinkedManualEvidence else {
                    throw STPDResultPackageError.invalidInput(
                        "modified review requires a linked manual replacement that changes output"
                    )
                }
            case .needsReview:
                break
            }
        }

        let normalizedEvidence = evidenceUIDsByISIKey.mapValues { $0.sorted() }
        let normalizedLinks = linkedISIKeysByEvidenceUID.mapValues { $0.sorted() }
        let changedISIKeys = Set(authoritativeRows.compactMap { reviewed -> String? in
            let key = isiRowKey(reviewed)
            guard let automatic = automaticByKey[key],
                  isiProjectionComponents(automatic)
                    != isiProjectionComponents(reviewed) else {
                return nil
            }
            return key
        })
        return STPDResolvedReviewAuthority(
            projectedEvents: projectedEvents,
            authoritativeRows: authoritativeRows,
            manualUIDByUUID: manualUIDByUUID,
            reviewUIDBySourceCandidateID: reviewUIDBySourceCandidateID,
            evidenceUIDsByISIKey: normalizedEvidence,
            linkedISIKeysByEvidenceUID: normalizedLinks,
            changedISIKeys: changedISIKeys,
            manualPositiveISIKeys: manualPositiveISIKeys
        )
    }

    static func resolvedManualAnnotations(
        _ annotations: [ManualAnnotation],
        dataset: SpikeDataset
    ) throws -> [ManualAnnotation] {
        let ids = annotations.map(\.id)
        guard Set(ids).count == ids.count else {
            throw STPDResultPackageError.invalidInput("manual annotation UUIDs are not unique")
        }
        return try annotations.map { annotation in
            guard annotation.startSec.isFinite, annotation.endSec.isFinite else {
                throw STPDResultPackageError.invalidInput(
                    "manual annotation \(annotation.id.uuidString) has non-finite time geometry"
                )
            }
            guard let resolved = ManualAnnotationGeometryResolver
                .resolvingIndicesIfCompatible(annotation, in: dataset.trains) else {
                throw STPDResultPackageError.invalidInput(
                    "manual annotation \(annotation.id.uuidString) is incompatible with the dataset"
                )
            }
            return resolved
        }
    }

    static func validateFinalProjection(
        events: [ClassicAnchorEventAnnotation],
        normalizedEvents: [STPDEventRecord],
        isiRows: [ReviewedISIExportRow],
        dataset: SpikeDataset
    ) throws {
        _ = ReviewedISIExportBuilder.build(
            dataset: dataset,
            autoAnnotations: events,
            projectionsByTrain: [:]
        )
        for row in isiRows where row.isiIndex > 0 {
            let covering = normalizedEvents.filter { record in
                let event = record.event
                return event.trainID == row.trainID
                    && event.startISIIndex <= row.isiIndex
                    && row.isiIndex <= event.endISIIndex
            }
            if row.finalPattern.isEmpty {
                guard covering.isEmpty else {
                    throw STPDResultPackageError.invalidInput(
                        "unlabeled final ISI row \(row.trainID):\(row.isiIndex) " +
                            "is covered by a normalized final event"
                    )
                }
                continue
            }
            guard covering.count == 1,
                  eventProjectionMatches(
                      row: row,
                      finalLabel: covering[0].event.finalLabel,
                      auditRecommendedSubtype:
                          covering[0].event.auditRecommendedSubtype,
                      allowsManualPositive: true
                  ) else {
                throw STPDResultPackageError.invalidInput(
                    "final ISI row \(row.trainID):\(row.isiIndex) is not represented " +
                        "exactly once by the normalized final-event projection"
                )
            }
        }
    }

    static func validateCandidate(
        _ candidate: ClassicAnchorCandidate,
        trainsByID: [String: SpikeTrain]
    ) throws {
        try validateDirectFiniteDoubles(
            candidate,
            entity: "candidate \(candidate.id)"
        )
        let isProfile = candidate.finalLabel == .profile
        var invalidFields: [String] = []
        if candidate.anchorBandLowerSec < 0 {
            invalidFields.append("anchorBandLowerSec=\(candidate.anchorBandLowerSec)")
        }
        if candidate.anchorBandUpperSec < 0 {
            invalidFields.append("anchorBandUpperSec=\(candidate.anchorBandUpperSec)")
        }
        if !isProfile && candidate.anchorBandUpperSec < candidate.anchorBandLowerSec {
            invalidFields.append(
                "anchorBandUpperSec=\(candidate.anchorBandUpperSec)<anchorBandLowerSec=\(candidate.anchorBandLowerSec)"
            )
        }
        if candidate.anchorContrastMinRequired < 0 {
            invalidFields.append("anchorContrastMinRequired")
        }
        if candidate.anchorContrastGeomRequired < 0 {
            invalidFields.append("anchorContrastGeomRequired")
        }
        if candidate.priority < 0 {
            invalidFields.append("priority")
        }
        if candidate.nISI < 0 {
            invalidFields.append("nISI")
        }
        if candidate.nValidISI < 0 || (!isProfile && candidate.nValidISI > candidate.nISI) {
            invalidFields.append("nValidISI")
        }
        if candidate.nSpikes < 0 {
            invalidFields.append("nSpikes")
        }
        if candidate.refractorySuspectCount < 0 {
            invalidFields.append("refractorySuspectCount")
        }
        let nonnegativeDoubles: [(String, Double?)] = [
            ("durationSec", candidate.durationSec),
            ("intraQ10Sec", candidate.intraQ10Sec),
            ("intraQ40Sec", candidate.intraQ40Sec),
            ("intraQ50Sec", candidate.intraQ50Sec),
            ("intraQ90Sec", candidate.intraQ90Sec),
            ("intraQ95Sec", candidate.intraQ95Sec),
            ("maxIntraISISec", candidate.maxIntraISISec),
            ("meanIntraISISec", candidate.meanIntraISISec),
            ("cv", candidate.cv),
            ("cv2", candidate.cv2),
            ("lv", candidate.lv),
            ("preGapSec", candidate.preGapSec),
            ("postGapSec", candidate.postGapSec),
            ("preRatioQ90", candidate.preRatioQ90),
            ("postRatioQ90", candidate.postRatioQ90),
            ("edgeContrastMinQ90", candidate.edgeContrastMinQ90),
            ("edgeContrastGeomQ90", candidate.edgeContrastGeomQ90),
            ("profileMedianISISec", candidate.profileMedianISISec),
            ("profileQ10ISISec", candidate.profileQ10ISISec),
            ("profileQ25ISISec", candidate.profileQ25ISISec),
            ("profileQ90ISISec", candidate.profileQ90ISISec),
            ("profileBridgeUpperSec", candidate.profileBridgeUpperSec),
            ("profileBoundaryFloorSec", candidate.profileBoundaryFloorSec),
            ("profileBurstContrastS", candidate.profileBurstContrastS),
            ("profilePossibleContrastS", candidate.profilePossibleContrastS),
            ("hfSpikingQ80Sec", candidate.hfSpikingQ80Sec),
            ("hfSpikingQ80MaxSec", candidate.hfSpikingQ80MaxSec),
            ("hfSpikingQ90MaxSec", candidate.hfSpikingQ90MaxSec),
            ("hfSpikingShortUpperSec", candidate.hfSpikingShortUpperSec),
            ("hfSpikingEpochBridgeSec", candidate.hfSpikingEpochBridgeSec),
            ("hfSpikingToleratedGapSec", candidate.hfSpikingToleratedGapSec),
            ("hfSpikingPatternMaxISISec", candidate.hfSpikingPatternMaxISISec),
            ("hfSpikingPauseBreakSec", candidate.hfSpikingPauseBreakSec),
            ("stateTrainPercentileMedian", candidate.stateTrainPercentileMedian),
            ("stateLocalPercentileMedian", candidate.stateLocalPercentileMedian),
            ("stateLocalPercentileQ90", candidate.stateLocalPercentileQ90),
            ("stateLocalRobustZAbsQ80", candidate.stateLocalRobustZAbsQ80),
            ("burstSeedBandLowerSec", candidate.burstSeedBandLowerSec),
            ("burstSeedBandUpperSec", candidate.burstSeedBandUpperSec),
            ("burstBridgeBandUpperSec", candidate.burstBridgeBandUpperSec),
            ("burstContrastRequired", candidate.burstContrastRequired),
            ("burstPossibleContrastRequired", candidate.burstPossibleContrastRequired),
            ("burstRequiredGapSec", candidate.burstRequiredGapSec),
            ("burstPossibleRequiredGapSec", candidate.burstPossibleRequiredGapSec),
            ("burstBoundaryFloorSec", candidate.burstBoundaryFloorSec),
            ("hardBurstSeedUpperSec", candidate.hardBurstSeedUpperSec),
            ("hardBurstBridgeUpperSec", candidate.hardBurstBridgeUpperSec),
            ("localBackgroundQ75Sec", candidate.localBackgroundQ75Sec),
            ("localCompressionQ90Ratio", candidate.localCompressionQ90Ratio),
            ("eventLocalMedianSec", candidate.eventLocalMedianSec),
            ("eventLocalPercentileMedian", candidate.eventLocalPercentileMedian),
            ("eventLocalPercentileQ90", candidate.eventLocalPercentileQ90),
            ("eventLocalRobustZAbsQ80", candidate.eventLocalRobustZAbsQ80),
        ]
        invalidFields.append(contentsOf: nonnegativeDoubles.compactMap { name, value in
            guard let value, value < 0 else {
                return nil
            }
            return name
        })
        let fractions: [(String, Double?)] = [
            ("profileSeedBandFraction", candidate.profileSeedBandFraction),
            ("profilePauseFraction", candidate.profilePauseFraction),
            ("hfSpikingShortFraction", candidate.hfSpikingShortFraction),
            ("hfSpikingQ90ShortFraction", candidate.hfSpikingQ90ShortFraction),
            ("hfSpikingBridgeFraction", candidate.hfSpikingBridgeFraction),
            ("hfSpikingLargeFraction", candidate.hfSpikingLargeFraction),
            ("hfSpikingToleratedFraction", candidate.hfSpikingToleratedFraction),
            ("hfSpikingEmbeddedBurstCoverage", candidate.hfSpikingEmbeddedBurstCoverage),
            ("stateRegularityScore", candidate.stateRegularityScore),
            ("stateBurstSeedFraction", candidate.stateBurstSeedFraction),
            ("stateLowTailFraction", candidate.stateLowTailFraction),
            ("stateLocalStabilityScore", candidate.stateLocalStabilityScore),
        ]
        invalidFields.append(contentsOf: fractions.compactMap { name, value in
            guard let value, value < 0 || value > 1 else {
                return nil
            }
            return name
        })
        let profilePercentiles: [(String, Double?)] = [
            ("profileSeedLowPercentileInTrain", candidate.profileSeedLowPercentileInTrain),
            ("profileSeedHighPercentileInTrain", candidate.profileSeedHighPercentileInTrain),
        ]
        invalidFields.append(contentsOf: profilePercentiles.compactMap { name, value in
            guard let value, value < 0 || value > 100 else {
                return nil
            }
            return name
        })
        if let low = candidate.profileSeedLowPercentileInTrain,
           let high = candidate.profileSeedHighPercentileInTrain,
           low > high {
            invalidFields.append("profileSeedPercentileOrder")
        }
        let unitPercentiles: [(String, Double?)] = [
            ("stateTrainPercentileMedian", candidate.stateTrainPercentileMedian),
            ("stateLocalPercentileMedian", candidate.stateLocalPercentileMedian),
            ("stateLocalPercentileQ90", candidate.stateLocalPercentileQ90),
            ("eventLocalPercentileMedian", candidate.eventLocalPercentileMedian),
            ("eventLocalPercentileQ90", candidate.eventLocalPercentileQ90),
        ]
        invalidFields.append(contentsOf: unitPercentiles.compactMap { name, value in
            guard let value, value < 0 || value > 1 else {
                return nil
            }
            return name
        })
        if let median = candidate.stateLocalPercentileMedian,
           let q90 = candidate.stateLocalPercentileQ90,
           median > q90 {
            invalidFields.append("stateLocalPercentileOrder")
        }
        if let median = candidate.eventLocalPercentileMedian,
           let q90 = candidate.eventLocalPercentileQ90,
           median > q90 {
            invalidFields.append("eventLocalPercentileOrder")
        }
        let optionalCounts: [(String, Int?)] = [
            ("profileSeedRunCount", candidate.profileSeedRunCount),
            ("profileMaxSeedRunLength", candidate.profileMaxSeedRunLength),
            ("hfSpikingMaxConsecutiveLargeISI", candidate.hfSpikingMaxConsecutiveLargeISI),
            ("hfSpikingMinSpikesRequired", candidate.hfSpikingMinSpikesRequired),
            ("hfSpikingEmbeddedBurstCount", candidate.hfSpikingEmbeddedBurstCount),
            ("hfSpikingEmbeddedBurstGroupCount", candidate.hfSpikingEmbeddedBurstGroupCount),
            ("stateCoreBurstRunLength", candidate.stateCoreBurstRunLength),
            ("hardBurstCoreISICount", candidate.hardBurstCoreISICount),
        ]
        invalidFields.append(contentsOf: optionalCounts.compactMap { name, value in
            guard let value, value < 0 else {
                return nil
            }
            return name
        })
        if !isProfile {
            let orderedQuantiles = [
                candidate.intraQ10Sec,
                candidate.intraQ40Sec,
                candidate.intraQ50Sec,
                candidate.intraQ90Sec,
                candidate.intraQ95Sec,
            ].compactMap { $0 }
            if zip(orderedQuantiles, orderedQuantiles.dropFirst()).contains(where: { $0 > $1 }) {
                invalidFields.append("intraQuantileOrder")
            }
            if let maximum = candidate.maxIntraISISec,
               orderedQuantiles.contains(where: { $0 > maximum }) {
                invalidFields.append("maxIntraISISec")
            }
        }
        guard invalidFields.isEmpty else {
            throw STPDResultPackageError.invalidInput(
                "candidate \(candidate.id) has invalid fields: \(invalidFields.joined(separator: ","))"
            )
        }
        guard candidate.startISIIndex <= candidate.endISIIndex,
              candidate.startSpikeIndex <= candidate.endSpikeIndex else {
            throw STPDResultPackageError.invalidInput(
                "candidate \(candidate.id) has inverted geometry"
            )
        }
        if isProfile {
            let validProfileClasses = Set(["train_profile", "dataset_profile"])
            guard validProfileClasses.contains(candidate.candidateClass),
                  candidate.startISIIndex == 0,
                  candidate.endISIIndex == 0,
                  candidate.startSpikeIndex == 0,
                  candidate.endSpikeIndex == 0,
                  candidate.nISI == 0 else {
                throw STPDResultPackageError.invalidInput(
                    "profile candidate \(candidate.id) has invalid class or sentinel geometry"
                )
            }
            if candidate.trainID == "__dataset__" {
                let totalSpikes = trainsByID.values.reduce(0) { $0 + $1.spikeCount }
                let totalIntervals = trainsByID.values.reduce(0) {
                    $0 + max(0, $1.spikeCount - 1)
                }
                guard candidate.candidateClass == "dataset_profile",
                      candidate.nSpikes == totalSpikes,
                      candidate.nValidISI <= totalIntervals else {
                    throw STPDResultPackageError.invalidInput(
                        "dataset profile \(candidate.id) has inconsistent aggregate counts"
                    )
                }
            } else {
                guard candidate.candidateClass == "train_profile",
                      let train = trainsByID[candidate.trainID],
                      candidate.trainName == train.name,
                      candidate.nSpikes == train.spikeCount,
                      candidate.nValidISI <= max(0, train.spikeCount - 1) else {
                    throw STPDResultPackageError.invalidInput(
                        "train profile \(candidate.id) has inconsistent train counts"
                    )
                }
            }
            return
        }
        guard candidate.trainID != "__dataset__" else {
            throw STPDResultPackageError.invalidInput(
                "non-profile candidate \(candidate.id) cannot use dataset sentinel ownership"
            )
        }
        guard let train = trainsByID[candidate.trainID],
              candidate.trainName == train.name else {
            throw STPDResultPackageError.invalidInput(
                "candidate \(candidate.id) references an unknown or mismatched train"
            )
        }
        guard candidate.startISIIndex >= 1,
              candidate.endISIIndex < train.spikeCount,
              candidate.startSpikeIndex >= 1,
              candidate.endSpikeIndex <= train.spikeCount,
              candidate.startSpikeIndex == candidate.startISIIndex,
              candidate.endSpikeIndex == candidate.endISIIndex + 1,
              candidate.nISI == candidate.endISIIndex - candidate.startISIIndex + 1,
              candidate.nSpikes == candidate.endSpikeIndex - candidate.startSpikeIndex + 1,
              candidate.nSpikes == candidate.nISI + 1,
              candidate.refractorySuspectCount <= candidate.nISI else {
            throw STPDResultPackageError.invalidInput(
                "candidate \(candidate.id) geometry or counts are inconsistent with its train"
            )
        }
        if let duration = candidate.durationSec {
            let expectedDuration =
                train.timestampsSec[candidate.endISIIndex] -
                train.timestampsSec[candidate.startISIIndex - 1]
            let tolerance = max(1e-12, abs(expectedDuration) * 1e-9)
            guard expectedDuration.isFinite,
                  abs(duration - expectedDuration) <= tolerance else {
                throw STPDResultPackageError.invalidInput(
                    "candidate \(candidate.id) duration contradicts its dataset span"
                )
            }
        }
        switch (candidate.burstSeedRunStartISI, candidate.burstSeedRunEndISI) {
        case (nil, nil):
            break
        case let (start?, end?):
            guard start >= candidate.startISIIndex,
                  start <= end,
                  end <= candidate.endISIIndex else {
                throw STPDResultPackageError.invalidInput(
                    "candidate \(candidate.id) burst seed run is outside its span"
                )
            }
        default:
            throw STPDResultPackageError.invalidInput(
                "candidate \(candidate.id) has incomplete burst seed-run geometry"
            )
        }
    }

    static func validateHFSuppressorReference(
        _ candidate: ClassicAnchorCandidate,
        candidatesByID: [String: ClassicAnchorCandidate]
    ) throws {
        guard candidate.suppressedByHFSpikingState == true else {
            guard candidate.hfSpikingSuppressorID == nil else {
                throw STPDResultPackageError.invalidInput(
                    "candidate \(candidate.id) has an HFS suppressor without suppression authority"
                )
            }
            return
        }
        guard candidate.finalLabel == .reject,
              let originalLabel = candidate.suppressedOriginalLabel,
              let originalFamily = ClassicAnchorLabel(rawValue: originalLabel),
              Set([
                  ClassicAnchorLabel.tonic,
                  .highFrequencyTonic,
                  .burst,
                  .highFrequencyBurst,
                  .longBurst,
              ]).contains(originalFamily),
              let suppressorID = candidate.hfSpikingSuppressorID,
              !suppressorID.isEmpty,
              let suppressor = candidatesByID[suppressorID],
              suppressor.trainID == candidate.trainID,
              suppressor.trainName == candidate.trainName,
              suppressor.finalLabel == .highFrequencySpiking,
              suppressor.suppressedByHFSpikingState != true,
              spansOverlap(
                lhsStart: candidate.startISIIndex,
                lhsEnd: candidate.endISIIndex,
                rhsStart: suppressor.startISIIndex,
                rhsEnd: suppressor.endISIIndex
              ) else {
            throw STPDResultPackageError.invalidInput(
                "candidate \(candidate.id) has an invalid HFS suppression relationship"
            )
        }
    }

    static func validateDirectFiniteDoubles(
        _ value: Any,
        entity: String
    ) throws {
        for child in Mirror(reflecting: value).children {
            if let number = child.value as? Double, !number.isFinite {
                throw STPDResultPackageError.invalidInput(
                    "\(entity) has a non-finite \(child.label ?? "numeric field")"
                )
            }
            let reflected = Mirror(reflecting: child.value)
            if reflected.displayStyle == .optional,
               let wrapped = reflected.children.first?.value as? Double,
               !wrapped.isFinite {
                throw STPDResultPackageError.invalidInput(
                    "\(entity) has a non-finite \(child.label ?? "optional numeric field")"
                )
            }
        }
    }

    static func validateEvent(
        _ event: ClassicAnchorEventAnnotation,
        train: SpikeTrain,
        candidateUIDBySourceID: [String: String],
        candidateBySourceID: [String: ClassicAnchorCandidate]
    ) throws {
        guard event.trainName == train.name else {
            throw STPDResultPackageError.invalidInput(
                "event \(event.id) train name does not match dataset"
            )
        }
        guard event.startISISecIndex >= 1,
              event.startISISecIndex <= event.endISISecIndex,
              event.endISISecIndex < train.spikeCount,
              event.startSpikeIndex == event.startISISecIndex,
              event.endSpikeIndex == event.endISISecIndex + 1 else {
            throw STPDResultPackageError.invalidInput(
                "event \(event.id) has invalid ISI/spike geometry"
            )
        }
        guard event.rawStartSec.isFinite,
              event.rawEndSec.isFinite,
              event.alignedStartSec.isFinite,
              event.alignedEndSec.isFinite,
              event.score.isFinite else {
            throw STPDResultPackageError.invalidInput(
                "event \(event.id) has non-finite required scientific values"
            )
        }
        let aligned = train.alignedTimestampsSec.count == train.spikeCount
            ? train.alignedTimestampsSec
            : train.timestampsSec
        let lowerTimestampIndex = event.startISISecIndex - 1
        let upperTimestampIndex = event.endISISecIndex
        guard canonicalEqual(event.rawStartSec, train.timestampsSec[lowerTimestampIndex]),
              canonicalEqual(event.rawEndSec, train.timestampsSec[upperTimestampIndex]),
              canonicalEqual(event.alignedStartSec, aligned[lowerTimestampIndex]),
              canonicalEqual(event.alignedEndSec, aligned[upperTimestampIndex]),
              canonicalEqual(event.durationSec, event.rawEndSec - event.rawStartSec) else {
            throw STPDResultPackageError.invalidInput(
                "event \(event.id) time geometry differs from dataset"
            )
        }

        let sourceIDs = Array(Set(event.sourceCandidateIDs + [event.candidateID]))
            .filter { !$0.isEmpty }
        guard !sourceIDs.isEmpty else {
            throw STPDResultPackageError.invalidInput(
                "event \(event.id) has no source candidate"
            )
        }
        let visible = Set(event.startISISecIndex...event.endISISecIndex)
        for sourceID in sourceIDs {
            guard candidateUIDBySourceID[sourceID] != nil,
                  let candidate = candidateBySourceID[sourceID] else {
                throw STPDResultPackageError.invalidInput(
                    "event \(event.id) references unknown candidate \(sourceID)"
                )
            }
            guard candidate.trainID == event.trainID,
                  candidate.trainName == event.trainName else {
                throw STPDResultPackageError.invalidInput(
                    "event \(event.id) source candidate \(sourceID) belongs to another train"
                )
            }
            let candidateSpan = Set(
                min(candidate.startISIIndex, candidate.endISIIndex)
                    ... max(candidate.startISIIndex, candidate.endISIIndex)
            )
            guard !candidateSpan.intersection(visible).isEmpty else {
                throw STPDResultPackageError.invalidInput(
                    "event \(event.id) source candidate \(sourceID) does not overlap its geometry"
                )
            }
        }
        guard Set(event.automaticSupportISIIndices).isSubset(of: visible) else {
            throw STPDResultPackageError.invalidInput(
                "event \(event.id) automatic support lies outside event geometry"
            )
        }
        var automaticSourceSupport = Set<Int>()
        for source in event.automaticEventSources {
            guard source.trainID == event.trainID,
                  candidateUIDBySourceID[source.candidateID] != nil,
                  let candidate = candidateBySourceID[source.candidateID],
                  source.score.isFinite else {
                throw STPDResultPackageError.invalidInput(
                    "event \(event.id) contains an invalid automatic evidence source"
                )
            }
            let sourceSupport = Set(source.supportISIIndices)
            guard sourceSupport.allSatisfy({ 1 <= $0 && $0 < train.spikeCount }) else {
                throw STPDResultPackageError.invalidInput(
                    "event \(event.id) automatic source \(source.candidateID) " +
                        "has support outside its train"
                )
            }
            let candidateSpan = Set(
                min(candidate.startISIIndex, candidate.endISIIndex)
                    ... max(candidate.startISIIndex, candidate.endISIIndex)
            )
            guard sourceSupport.isSubset(of: candidateSpan),
                  source.semanticTrack.rawValue == candidate.auditRecommendedTrackRawValue,
                  source.eventTrackClass == candidate.auditRecommendedEventTrackClass,
                  source.auditRecommendedSubtype == candidate.auditRecommendedSubtype,
                  source.auditReviewStatus == candidate.auditReviewStatus,
                  source.label == candidate.finalLabel,
                  source.lockLevel == candidate.anchorLockLevel,
                  source.stateTonicSubtype == candidate.stateTonicSubtype,
                  canonicalEqual(source.score, candidate.score),
                  source.priority == candidate.priority,
                  source.decisionPath == candidate.decisionPath else {
                throw STPDResultPackageError.invalidInput(
                    "event \(event.id) automatic source \(source.candidateID) " +
                        "does not match its candidate snapshot"
                )
            }
            automaticSourceSupport.formUnion(sourceSupport)
        }
        guard automaticSourceSupport.intersection(visible)
                == Set(event.automaticSupportISIIndices) else {
            throw STPDResultPackageError.invalidInput(
                "event \(event.id) aggregate automatic support does not equal " +
                    "the visible intersection of its immutable source baselines"
            )
        }
    }

    static func candidateIdentityComponents(
        _ candidate: ClassicAnchorCandidate,
        intrinsicUIDBySourceID: [String: String]
    ) -> [String] {
        let suppressorIntrinsicUID: String
        if let suppressorID = candidate.hfSpikingSuppressorID {
            suppressorIntrinsicUID = intrinsicUIDBySourceID[suppressorID] ?? ""
        } else {
            suppressorIntrinsicUID = ""
        }
        return candidateIntrinsicIdentityComponents(candidate) + [suppressorIntrinsicUID]
    }

    static func candidateIntrinsicIdentityComponents(
        _ candidate: ClassicAnchorCandidate
    ) -> [String] {
        [
            candidate.trainID,
            candidate.trainName,
            candidate.candidateLayer,
            candidate.candidateClass,
            candidate.finalLabel.rawValue,
            candidate.gateStatus,
            candidate.action,
            String(candidate.priority),
            STPDCanonicalValue.bool(candidate.selectedForAuto),
            candidate.selectionStatus,
            String(candidate.startISIIndex),
            String(candidate.endISIIndex),
            String(candidate.startSpikeIndex),
            String(candidate.endSpikeIndex),
            String(candidate.nISI),
            String(candidate.nValidISI),
            String(candidate.nSpikes),
            candidate.anchorFamily,
            candidate.anchorLockLevel.rawValue,
            STPDCanonicalValue.double(candidate.durationSec),
            STPDCanonicalValue.double(candidate.intraQ10Sec),
            STPDCanonicalValue.double(candidate.intraQ40Sec),
            STPDCanonicalValue.double(candidate.intraQ50Sec),
            STPDCanonicalValue.double(candidate.intraQ90Sec),
            STPDCanonicalValue.double(candidate.intraQ95Sec),
            STPDCanonicalValue.double(candidate.maxIntraISISec),
            STPDCanonicalValue.double(candidate.meanIntraISISec),
            STPDCanonicalValue.double(candidate.cv),
            STPDCanonicalValue.double(candidate.cv2),
            STPDCanonicalValue.double(candidate.lv),
            STPDCanonicalValue.double(candidate.preGapSec),
            STPDCanonicalValue.double(candidate.postGapSec),
            STPDCanonicalValue.double(candidate.preRatioQ90),
            STPDCanonicalValue.double(candidate.postRatioQ90),
            STPDCanonicalValue.double(candidate.edgeContrastMinQ90),
            STPDCanonicalValue.double(candidate.edgeContrastGeomQ90),
            STPDCanonicalValue.double(candidate.score),
            STPDCanonicalValue.double(candidate.anchorBandLowerSec),
            STPDCanonicalValue.double(candidate.anchorBandUpperSec),
            candidate.anchorBandSource.rawValue,
            STPDCanonicalValue.double(candidate.anchorContrastMinRequired),
            STPDCanonicalValue.double(candidate.anchorContrastGeomRequired),
            String(candidate.refractorySuspectCount),
            candidate.refractorySuspectAction?.rawValue ?? "",
            STPDCanonicalValue.double(candidate.profileSeedLowPercentileInTrain),
            STPDCanonicalValue.double(candidate.profileSeedHighPercentileInTrain),
            STPDCanonicalValue.double(candidate.profileSeedBandFraction),
            STPDCanonicalValue.int(candidate.profileSeedRunCount),
            STPDCanonicalValue.int(candidate.profileMaxSeedRunLength),
            STPDCanonicalValue.double(candidate.profileMedianISISec),
            STPDCanonicalValue.double(candidate.profileQ10ISISec),
            STPDCanonicalValue.double(candidate.profileQ25ISISec),
            STPDCanonicalValue.double(candidate.profileQ90ISISec),
            STPDCanonicalValue.double(candidate.profilePauseFraction),
            candidate.profilePhenotypePrior ?? "",
            STPDCanonicalValue.double(candidate.profileBridgeUpperSec),
            STPDCanonicalValue.double(candidate.profileBoundaryFloorSec),
            STPDCanonicalValue.bool(candidate.profileBoundaryFloorHard),
            STPDCanonicalValue.double(candidate.profileBurstContrastS),
            STPDCanonicalValue.double(candidate.profilePossibleContrastS),
            STPDCanonicalValue.double(candidate.hfSpikingQ80Sec),
            STPDCanonicalValue.double(candidate.hfSpikingQ80MaxSec),
            STPDCanonicalValue.double(candidate.hfSpikingQ90MaxSec),
            STPDCanonicalValue.double(candidate.hfSpikingShortUpperSec),
            STPDCanonicalValue.double(candidate.hfSpikingEpochBridgeSec),
            STPDCanonicalValue.double(candidate.hfSpikingToleratedGapSec),
            STPDCanonicalValue.double(candidate.hfSpikingPatternMaxISISec),
            STPDCanonicalValue.double(candidate.hfSpikingPauseBreakSec),
            STPDCanonicalValue.double(candidate.hfSpikingShortFraction),
            STPDCanonicalValue.double(candidate.hfSpikingQ90ShortFraction),
            STPDCanonicalValue.double(candidate.hfSpikingBridgeFraction),
            STPDCanonicalValue.double(candidate.hfSpikingLargeFraction),
            STPDCanonicalValue.double(candidate.hfSpikingToleratedFraction),
            STPDCanonicalValue.int(candidate.hfSpikingMaxConsecutiveLargeISI),
            STPDCanonicalValue.int(candidate.hfSpikingMinSpikesRequired),
            candidate.hfSpikingAcceptanceRoute ?? "",
            STPDCanonicalValue.bool(candidate.hfSpikingBurstDominated),
            STPDCanonicalValue.int(candidate.hfSpikingEmbeddedBurstCount),
            STPDCanonicalValue.int(candidate.hfSpikingEmbeddedBurstGroupCount),
            STPDCanonicalValue.double(candidate.hfSpikingEmbeddedBurstCoverage),
            STPDCanonicalValue.bool(candidate.hfSpikingBurstPacketLike),
            STPDCanonicalValue.bool(candidate.hfSpikingBurstPacketNeighbor),
            STPDCanonicalValue.bool(candidate.suppressedByHFSpikingState),
            candidate.suppressedOriginalLabel ?? "",
            STPDCanonicalValue.double(candidate.stateRegularityScore),
            STPDCanonicalValue.double(candidate.stateBurstSeedFraction),
            STPDCanonicalValue.double(candidate.stateLowTailFraction),
            STPDCanonicalValue.double(candidate.stateLocalStabilityScore),
            STPDCanonicalValue.int(candidate.stateCoreBurstRunLength),
            candidate.stateTonicSubtype ?? "",
            STPDCanonicalValue.bool(candidate.stateContinuityAuthorityFrozen),
            STPDCanonicalValue.bool(candidate.stateContinuityMergeTerminal),
            candidate.stateHighFrequencySubtype ?? "",
            STPDCanonicalValue.double(candidate.stateTrainPercentileMedian),
            STPDCanonicalValue.double(candidate.stateLocalPercentileMedian),
            STPDCanonicalValue.double(candidate.stateLocalPercentileQ90),
            STPDCanonicalValue.double(candidate.stateLocalRobustZMedian),
            STPDCanonicalValue.double(candidate.stateLocalRobustZAbsQ80),
            STPDCanonicalValue.double(candidate.stateLocalRobustZQ10),
            STPDCanonicalValue.int(candidate.burstSeedRunStartISI),
            STPDCanonicalValue.int(candidate.burstSeedRunEndISI),
            STPDCanonicalValue.double(candidate.burstSeedBandLowerSec),
            STPDCanonicalValue.double(candidate.burstSeedBandUpperSec),
            STPDCanonicalValue.double(candidate.burstBridgeBandUpperSec),
            STPDCanonicalValue.double(candidate.burstContrastRequired),
            STPDCanonicalValue.double(candidate.burstPossibleContrastRequired),
            STPDCanonicalValue.double(candidate.burstRequiredGapSec),
            STPDCanonicalValue.double(candidate.burstPossibleRequiredGapSec),
            STPDCanonicalValue.double(candidate.burstBoundaryFloorSec),
            STPDCanonicalValue.bool(candidate.burstBoundaryFloorHard),
            STPDCanonicalValue.bool(candidate.burstStrictBoundaryPass),
            STPDCanonicalValue.bool(candidate.burstPossibleBoundaryPass),
            STPDCanonicalValue.bool(candidate.burstBridgeCountPass),
            STPDCanonicalValue.bool(candidate.burstBridgeFractionPass),
            STPDCanonicalValue.bool(candidate.burstQ90BridgePass),
            candidate.burstSizeLabelBeforeReview ?? "",
            candidate.thresholdMode ?? "",
            STPDCanonicalValue.bool(candidate.hardThreshold),
            candidate.hardThresholdPattern ?? "",
            STPDCanonicalValue.double(candidate.hardBurstSeedUpperSec),
            STPDCanonicalValue.double(candidate.hardBurstBridgeUpperSec),
            STPDCanonicalValue.int(candidate.hardBurstCoreISICount),
            candidate.hardThresholdSource ?? "",
            STPDCanonicalValue.double(candidate.localBackgroundQ75Sec),
            STPDCanonicalValue.double(candidate.localCompressionQ90Ratio),
            STPDCanonicalValue.double(candidate.eventLocalMedianSec),
            STPDCanonicalValue.double(candidate.eventLocalPercentileMedian),
            STPDCanonicalValue.double(candidate.eventLocalPercentileQ90),
            STPDCanonicalValue.double(candidate.eventLocalRobustZMedian),
            STPDCanonicalValue.double(candidate.eventLocalRobustZAbsQ80),
            STPDCanonicalValue.double(candidate.eventLocalRobustZQ10),
            candidate.auditRecommendedTrackRawValue,
            candidate.auditRecommendedEventTrackClass,
            candidate.auditRecommendedFamily,
            candidate.auditRecommendedSubtype,
            candidate.auditRecommendedFinalClass,
            candidate.auditReviewStatus,
            STPDCanonicalValue.bool(candidate.auditReviewRequired),
            candidate.auditConfidenceTier,
            candidate.auditUncertaintyReason,
            candidate.auditLongBurstDefinitionStatus,
            candidate.failureReason,
            candidate.candidateDiagnosticClass,
        ]
    }

    static func eventIdentityComponents(
        _ event: ClassicAnchorEventAnnotation,
        sourceCandidateUIDs: [String],
        candidateUIDBySourceID: [String: String]
    ) -> [String] {
        let sourceEvidence = event.automaticEventSources.map { source in
            compositeKey([
                candidateUIDBySourceID[source.candidateID] ?? "",
                source.trainID,
                source.supportISIIndices.map(String.init).joined(separator: "|"),
                source.semanticTrack.rawValue,
                source.eventTrackClass,
                source.auditRecommendedSubtype,
                source.auditReviewStatus,
                source.label.rawValue,
                source.lockLevel.rawValue,
                source.stateTonicSubtype ?? "",
                STPDCanonicalValue.double(source.score),
                String(source.priority),
                source.decisionPath,
            ])
        }.sorted()
        return [
            event.trainID,
            event.trainName,
            event.semanticTrack.rawValue,
            event.eventTrackClass,
            event.label.rawValue,
            event.lockLevel.rawValue,
            event.stateTonicSubtype ?? "",
            String(event.startISISecIndex),
            String(event.endISISecIndex),
            String(event.startSpikeIndex),
            String(event.endSpikeIndex),
            STPDCanonicalValue.double(event.rawStartSec),
            STPDCanonicalValue.double(event.rawEndSec),
            STPDCanonicalValue.double(event.alignedStartSec),
            STPDCanonicalValue.double(event.alignedEndSec),
            STPDCanonicalValue.double(event.durationSec),
            STPDCanonicalValue.double(event.score),
            String(event.priority),
            sourceCandidateUIDs.joined(separator: "|"),
            event.automaticSupportISIIndices.map(String.init).joined(separator: "|"),
            event.auditRecommendedSubtype,
            event.auditReviewStatus,
            event.decisionPath,
        ] + sourceEvidence
    }

    static func eventProjectionComponents(_ event: ClassicAnchorEventAnnotation) -> [String] {
        let rawMap = Dictionary(
            uniqueKeysWithValues: Array(
                Set(event.sourceCandidateIDs + [event.candidateID] +
                    event.automaticEventSources.map(\.candidateID))
            ).map { ($0, $0) }
        )
        return [
            event.id,
            event.candidateID,
            event.sourceCandidateIDs.sorted().joined(separator: "|"),
        ] + eventIdentityComponents(
            event,
            sourceCandidateUIDs: event.sourceCandidateIDs.sorted(),
            candidateUIDBySourceID: rawMap
        )
    }

    static func projectionFingerprint(
        _ events: [ClassicAnchorEventAnnotation]
    ) -> [String] {
        events.map { compositeKey(eventProjectionComponents($0)) }.sorted()
    }

    static func isiProjectionComponents(_ row: ReviewedISIExportRow) -> [String] {
        [
            row.trainID,
            row.trainName,
            String(row.spikeIndex),
            STPDCanonicalValue.double(row.timestampSec),
            STPDCanonicalValue.double(row.alignedTimestampSec),
            String(row.isiIndex),
            STPDCanonicalValue.double(row.isiSec),
            row.autoPattern,
            row.autoSubtype,
            row.autoCandidateID,
            row.finalPattern,
            row.finalSubtype,
            row.finalSource,
            STPDCanonicalValue.bool(row.manualVetoSuppressed),
            row.reviewNote,
        ]
    }

    static func isiProjectionFingerprint(_ rows: [ReviewedISIExportRow]) -> [String] {
        rows.map { compositeKey(isiProjectionComponents($0)) }.sorted()
    }

    static func isiRowKey(_ row: ReviewedISIExportRow) -> String {
        "\(row.trainID)\u{1}\(row.isiIndex)"
    }

    static func isiEvidenceKey(trainID: String, isiIndex: Int) -> String {
        "\(trainID)\u{1}\(isiIndex)"
    }

    static func stableISIUID(
        datasetDigest: String,
        trainID: String,
        isiIndex: Int
    ) -> String {
        STPDStableIdentifier.make(
            prefix: "isi",
            domain: "stpd_isi_uid_v1",
            components: [
                datasetDigest,
                trainID,
                String(isiIndex),
            ]
        )
    }

    static func isiQCClass(
        _ value: Double,
        settings: SpikeQualitySettings
    ) -> String {
        let artifactTolerance = max(
            1e-12,
            abs(settings.artifactThresholdSec) * 1e-6
        )
        if value < settings.artifactThresholdSec - artifactTolerance {
            return "artifact_below_floor"
        }
        let refractoryTolerance = max(
            1e-12,
            abs(settings.refractorySuspectThresholdSec) * 1e-6
        )
        if settings.refractorySuspectThresholdSec
            > settings.artifactThresholdSec,
           value < settings.refractorySuspectThresholdSec
                - refractoryTolerance {
            return "refractory_suspect"
        }
        return "valid"
    }

    static func canonicalEqual(_ lhs: Double, _ rhs: Double) -> Bool {
        lhs.isFinite && rhs.isFinite &&
            STPDCanonicalValue.double(lhs) == STPDCanonicalValue.double(rhs)
    }

    static func canonicalEqual(_ lhs: Double?, _ rhs: Double?) -> Bool {
        switch (lhs, rhs) {
        case (nil, nil):
            return true
        case let (lhs?, rhs?):
            return canonicalEqual(lhs, rhs)
        default:
            return false
        }
    }

    static func contract(_ table: STPDResultTable) throws -> STPDResultTableContract {
        guard let contract = STPDResultSchema.tables.first(where: { $0.table == table }) else {
            throw STPDResultPackageError.missingTable(table.rawValue)
        }
        return contract
    }

    static func table(
        _ table: STPDResultTable,
        headers: [String],
        rows: [[String]]
    ) throws -> STPDResultTableData {
        try STPDResultTableData(
            contract: contract(table),
            headers: headers,
            rows: rows
        )
    }

    static func identityPrefix(_ identity: DetectionRunIdentity) -> [String] {
        [identity.runID, identity.settingsDigest]
    }

    static func compositeKey(_ components: [String]) -> String {
        components.map { "\(Data($0.utf8).count):\($0)" }.joined()
    }
}

private extension STPDResultPackageBuilder {
    static func runMetadataTable(
        input: STPDResultPackageInput,
        candidateCount: Int,
        finalEventCount: Int,
        finalISICount: Int
    ) throws -> STPDResultTableData {
        let identity = input.run.runIdentity
        let headers = [
            "run_id", "settings_digest", "dataset_digest", "result_schema_version",
            "detector_version", "build_identifier", "build_identifier_kind",
            "build_reproducibility_attested",
            "owner_name", "owner_email", "source_mode",
            "dataset_name", "dataset_source", "train_count", "spike_count", "task_event_count",
            "candidate_count", "final_event_count", "final_isi_count",
        ]
        return try table(
            .runMetadata,
            headers: headers,
            rows: [
                makeRow(headers, values: [
                    "run_id": identity.runID,
                    "settings_digest": identity.settingsDigest,
                    "dataset_digest": identity.datasetDigest,
                    "result_schema_version": identity.resultSchemaVersion,
                    "detector_version": identity.detectorVersion,
                    "build_identifier": identity.buildCommit,
                    "build_identifier_kind": "caller_supplied_unattested",
                    "build_reproducibility_attested": "false",
                    "owner_name": STPDResultPackageOwnership.ownerName,
                    "owner_email": STPDResultPackageOwnership.ownerEmail,
                    "source_mode": input.sourceMode.rawValue,
                    "dataset_name": input.dataset.name,
                    "dataset_source": input.dataset.sourceDescription,
                    "train_count": String(identity.trainCount),
                    "spike_count": String(identity.spikeCount),
                    "task_event_count": String(identity.taskEventCount),
                    "candidate_count": String(candidateCount),
                    "final_event_count": String(finalEventCount),
                    "final_isi_count": String(finalISICount),
                ])
            ]
        )
    }

    static func parametersTable(
        identity: DetectionRunIdentity
    ) throws -> STPDResultTableData {
        guard let snapshot = identity.settingsSnapshot else {
            throw STPDResultPackageError.invalidRunIdentity("settings snapshot is unavailable")
        }
        let headers = ["run_id", "settings_digest", "parameter_key", "requested_value"]
        let rows = snapshot.entries.map { entry in
            makeRow(headers, values: [
                "run_id": identity.runID,
                "settings_digest": identity.settingsDigest,
                "parameter_key": entry.key,
                "requested_value": entry.value,
            ])
        }
        return try table(.parametersReport, headers: headers, rows: rows)
    }

    static func resolvedParametersTable(
        identity: DetectionRunIdentity,
        run: ClassicAnchorDetectionRun,
        dataset: SpikeDataset
    ) throws -> STPDResultTableData {
        guard let snapshot = identity.settingsSnapshot else {
            throw STPDResultPackageError.invalidRunIdentity(
                "settings snapshot is unavailable"
            )
        }
        let headers = [
            "run_id", "settings_digest", "scope_type", "scope_id", "scope_name",
            "parameter_key", "requested_value", "adaptive_value", "effective_value",
            "source", "resolution_mode", "resolution_note",
            "histogram_value", "default_value",
        ]
        var rows: [[String]] = snapshot.entries.map { entry in
            makeRow(headers, values: [
                "run_id": identity.runID,
                "settings_digest": identity.settingsDigest,
                "scope_type": "run",
                "scope_id": identity.runID,
                "scope_name": "detector_run",
                "parameter_key": entry.key,
                "requested_value": entry.value,
                "adaptive_value": "",
                "effective_value": "",
                "source": "requested",
                "resolution_mode": "requested",
                "resolution_note": "",
                "histogram_value": "",
                "default_value": "",
            ])
        }
        func appendRunProvenance(
            key: String,
            effective: String,
            source: String = "detector_run_provenance"
        ) {
            rows.append(makeRow(headers, values: [
                "run_id": identity.runID,
                "settings_digest": identity.settingsDigest,
                "scope_type": "dataset",
                "scope_id": identity.datasetDigest,
                "scope_name": "dataset_prior",
                "parameter_key": key,
                "requested_value": "",
                "adaptive_value": "",
                "effective_value": effective,
                "source": source,
                "resolution_mode": "not_applicable",
                "resolution_note": "",
                "histogram_value": "",
                "default_value": "",
            ]))
        }
        let provenance = run.datasetRerunProvenance
        appendRunProvenance(
            key: "dataset_prior.stage_path",
            effective: provenance.stagePath.joined(separator: "|")
        )
        appendRunProvenance(
            key: "dataset_prior.source",
            effective: provenance.source
        )
        appendRunProvenance(
            key: "dataset_prior.train_count",
            effective: String(provenance.trainCount)
        )
        appendRunProvenance(
            key: "dataset_prior.bridge_expansion_used_dataset_summary",
            effective: STPDCanonicalValue.bool(provenance.bridgeExpansionUsedDatasetSummary)
        )
        appendRunProvenance(
            key: "dataset_prior.summary_applied_to_resolutions",
            effective: STPDCanonicalValue.bool(provenance.datasetSummaryAppliedToResolutions)
        )
        appendRunProvenance(
            key: "dataset_prior.rerun_executed",
            effective: STPDCanonicalValue.bool(provenance.rerunExecuted)
        )
        appendRunProvenance(
            key: "dataset_prior.rerun_train_count",
            effective: String(provenance.rerunTrainCount)
        )
        appendRunProvenance(
            key: "dataset_prior.summary_included_target_train",
            effective: STPDCanonicalValue.bool(provenance.datasetSummaryIncludedTargetTrain)
        )
        appendRunProvenance(
            key: "dataset_prior.applied_leave_one_out",
            effective: STPDCanonicalValue.bool(provenance.datasetPriorAppliedLeaveOneOut)
        )
        appendRunProvenance(
            key: "dataset_prior.bridge_applied_leave_one_out",
            effective: STPDCanonicalValue.bool(provenance.bridgeExpansionPriorAppliedLeaveOneOut)
        )
        appendRunProvenance(
            key: "dataset_prior.bridge_summary_included_target_train",
            effective: STPDCanonicalValue.bool(
                provenance.bridgeExpansionSummaryIncludedTargetTrain
            )
        )
        appendDatasetSeedSummary(
            provenance.initialDatasetSummary,
            prefix: "dataset_prior.initial",
            append: appendRunProvenance
        )
        appendDatasetSeedSummary(
            provenance.finalDatasetSummary,
            prefix: "dataset_prior.final",
            append: appendRunProvenance
        )

        for resolution in run.resolutions {
            let base = [
                "run_id": identity.runID,
                "settings_digest": identity.settingsDigest,
                "scope_type": "train",
                "scope_id": resolution.trainID,
                "scope_name": resolution.trainName,
            ]
            func append(
                key: String,
                effective: String,
                source: String,
                requested: String = "",
                adaptive: String = "",
                mode: String = "not_applicable",
                note: String = "",
                histogram: String = "",
                defaultValue: String = ""
            ) {
                rows.append(makeRow(headers, values: base.merging([
                    "parameter_key": key,
                    "requested_value": requested,
                    "adaptive_value": adaptive,
                    "effective_value": effective,
                    "source": source,
                    "resolution_mode": mode,
                    "resolution_note": note,
                    "histogram_value": histogram,
                    "default_value": defaultValue,
                ]) { _, new in new }))
            }
            append(
                key: "band.min_valid_isi_sec",
                effective: STPDCanonicalValue.double(resolution.minValidISISec),
                source: "requested"
            )
            append(
                key: "band.histogram_bin_width_sec",
                effective: STPDCanonicalValue.double(resolution.histogramBinWidthSec),
                source: "requested"
            )
            append(
                key: "band.valid_isi_count",
                effective: String(resolution.validISICount),
                source: "observed"
            )
            append(
                key: "structural_seed.origin",
                effective: resolution.structuralSeedSummary.origin.rawValue,
                source: "structural_seed_provenance"
            )
            append(
                key: "structural_seed.source",
                effective: resolution.structuralSeedSummary.source,
                source: "structural_seed_provenance"
            )
            append(
                key: "structural_seed.dataset_summary_included_target_train",
                effective: STPDCanonicalValue.bool(
                    resolution.structuralSeedSummary.datasetSummaryIncludedTargetTrain
                ),
                source: "structural_seed_provenance"
            )
            append(
                key: "structural_seed.burst_dataset_aggregatable",
                effective: STPDCanonicalValue.bool(
                    resolution.structuralSeedSummary.isBurstSeedDatasetAggregatable
                ),
                source: "structural_seed_provenance"
            )
            append(
                key: "structural_seed.tonic_dataset_aggregatable",
                effective: STPDCanonicalValue.bool(
                    resolution.structuralSeedSummary.isTonicSeedDatasetAggregatable
                ),
                source: "structural_seed_provenance"
            )
            append(
                key: "structural_seed.pause_dataset_aggregatable",
                effective: STPDCanonicalValue.bool(
                    resolution.structuralSeedSummary.isPauseSeedDatasetAggregatable
                ),
                source: "structural_seed_provenance"
            )
            for threshold in resolution.thresholdRows {
                append(
                    key: "adaptive.\(threshold.pattern.rawValue).\(threshold.field.rawValue)",
                    effective: STPDCanonicalValue.double(threshold.effectiveSec),
                    source: threshold.source.rawValue,
                    histogram: STPDCanonicalValue.double(threshold.histogramSec),
                    defaultValue: STPDCanonicalValue.double(threshold.defaultSec)
                )
            }
        }

        for evidence in run.resolvedThresholdEvidence {
            let profile = evidence.effectiveProfile
            let effectiveValues: [String: String] = [
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
            let base = [
                "run_id": identity.runID,
                "settings_digest": identity.settingsDigest,
                "scope_type": "train",
                "scope_id": evidence.trainID,
                "scope_name": evidence.trainName,
                "histogram_value": "",
                "default_value": "",
            ]
            func appendFinal(
                key: String,
                requested: String = "",
                adaptive: String = "",
                effective: String,
                source: String,
                mode: String,
                note: String = ""
            ) {
                rows.append(makeRow(headers, values: base.merging([
                    "parameter_key": key,
                    "requested_value": requested,
                    "adaptive_value": adaptive,
                    "effective_value": effective,
                    "source": source,
                    "resolution_mode": mode,
                    "resolution_note": note,
                ]) { _, new in new }))
            }
            appendFinal(
                key: "resolution.stage_path",
                effective: evidence.stagePath,
                source: "detector_stage",
                mode: "not_applicable"
            )
            for key in effectiveValues.keys.sorted() {
                let resolved = provenanceByKey[key]
                let notes = [
                    resolved?.note,
                    "post_clamp_effective_captured_at_authoritative_rerun",
                ].compactMap { $0 }
                appendFinal(
                    key: key,
                    requested: STPDCanonicalValue.double(resolved?.userValue),
                    adaptive: STPDCanonicalValue.double(resolved?.adaptiveValue),
                    effective: effectiveValues[key] ?? "",
                    source: resolved?.source.rawValue ?? "post_clamp_effective",
                    mode: resolved?.mode.rawValue ?? ThresholdMode.automatic.rawValue,
                    note: notes.joined(separator: "|")
                )
            }
            let unmatchedProvenance = evidence.resolutionProvenance
                .filter { effectiveValues[$0.key] == nil }
                .sorted { $0.key < $1.key }
            for resolved in unmatchedProvenance {
                appendFinal(
                    key: "resolver.\(resolved.key)",
                    requested: STPDCanonicalValue.double(resolved.userValue),
                    adaptive: STPDCanonicalValue.double(resolved.adaptiveValue),
                    effective: STPDCanonicalValue.double(resolved.effectiveValue),
                    source: resolved.source.rawValue,
                    mode: resolved.mode.rawValue,
                    note: resolved.note ?? ""
                )
            }
            for key in evidence.learnedProvenanceByKey.keys.sorted() {
                appendFinal(
                    key: "learned_provenance.\(key)",
                    effective: evidence.learnedProvenanceByKey[key] ?? "",
                    source: "learned_provenance",
                    mode: "not_applicable"
                )
            }
        }

        for train in dataset.trains {
            let quality = SpikeQualityAnalyzer.quality(
                for: train,
                settings: run.qualitySettings
            )
            let base = [
                "run_id": identity.runID,
                "settings_digest": identity.settingsDigest,
                "scope_type": "train",
                "scope_id": train.id,
                "scope_name": train.name,
                "source": "quality_analyzer",
                "requested_value": "",
                "adaptive_value": "",
                "resolution_mode": "not_applicable",
                "resolution_note": "",
                "histogram_value": "",
                "default_value": "",
            ]
            func appendQuality(_ key: String, _ value: String) {
                rows.append(makeRow(headers, values: base.merging([
                    "parameter_key": "quality.\(key)",
                    "effective_value": value,
                ]) { _, new in new }))
            }
            appendQuality("warning_level", quality.warningLevel.rawValue)
            appendQuality("warning_message", quality.warningMessage)
            appendQuality("spike_count", String(quality.spikeCount))
            appendQuality(
                "first_spike_sec",
                STPDCanonicalValue.double(quality.firstSpikeSec)
            )
            appendQuality(
                "last_spike_sec",
                STPDCanonicalValue.double(quality.lastSpikeSec)
            )
            appendQuality("duration_sec", STPDCanonicalValue.double(quality.durationSec))
            appendQuality(
                "firing_rate_hz",
                STPDCanonicalValue.double(quality.firingRateHz)
            )
            appendQuality(
                "raw_min_isi_sec",
                STPDCanonicalValue.double(quality.rawMinISISec)
            )
            appendQuality(
                "min_valid_isi_sec",
                STPDCanonicalValue.double(quality.minValidISISec)
            )
            appendQuality(
                "artifact_min_isi_sec",
                STPDCanonicalValue.double(quality.artifactMinISISec)
            )
            appendQuality(
                "median_isi_sec",
                STPDCanonicalValue.double(quality.medianISISec)
            )
            appendQuality(
                "max_isi_sec",
                STPDCanonicalValue.double(quality.maxISISec)
            )
            appendQuality(
                "duplicate_timestamp_count",
                String(quality.duplicateTimestampCount)
            )
            appendQuality(
                "zero_or_negative_isi_count",
                String(quality.zeroOrNegativeISICount)
            )
            appendQuality(
                "zero_or_negative_timestamp_step_count",
                String(quality.zeroOrNegativeTimestampStepCount)
            )
            appendQuality(
                "input_was_unsorted",
                STPDCanonicalValue.bool(quality.inputWasUnsorted)
            )
            appendQuality(
                "input_nonmonotonic_step_count",
                String(quality.inputNonmonotonicStepCount)
            )
            appendQuality(
                "input_duplicate_timestamp_step_count",
                String(quality.inputDuplicateTimestampStepCount)
            )
            appendQuality(
                "input_zero_or_negative_step_count",
                String(quality.inputZeroOrNegativeStepCount)
            )
            appendQuality(
                "dropped_duplicate_timestamp_count",
                String(quality.droppedDuplicateTimestampCount)
            )
            appendQuality(
                "duplicate_timestamp_policy",
                quality.duplicateTimestampPolicy.rawValue
            )
            appendQuality("artifact_isi_count", String(quality.artifactISICount))
            appendQuality(
                "artifact_fraction",
                STPDCanonicalValue.double(quality.artifactFraction)
            )
            appendQuality(
                "refractory_suspect_isi_count",
                String(quality.refractorySuspectISICount)
            )
            appendQuality(
                "refractory_suspect_fraction",
                STPDCanonicalValue.double(quality.refractorySuspectFraction)
            )
            appendQuality("valid_isi_count", String(quality.validISICount))
            appendQuality("percentile_status", quality.percentileStatus)
        }
        return try table(.resolvedParameters, headers: headers, rows: rows)
    }

    static func appendDatasetSeedSummary(
        _ summary: StructuralDatasetSeedSummary,
        prefix: String,
        append: (_ key: String, _ effective: String, _ source: String) -> Void
    ) {
        func add(_ suffix: String, _ value: String) {
            append("\(prefix).\(suffix)", value, "dataset_seed_summary")
        }
        add("train_count", String(summary.trainCount))
        add("seeded_train_count", String(summary.seededTrainCount))
        add("burst_anchor_count", String(summary.burstAnchorCount))
        add("burst_support_weight", STPDCanonicalValue.double(summary.burstSupportWeight))
        add("burst_seed_upper_sec", STPDCanonicalValue.double(summary.burstSeedUpperSec))
        add("burst_bridge_upper_sec", STPDCanonicalValue.double(summary.burstBridgeUpperSec))
        add("tonic_anchor_count", String(summary.tonicAnchorCount))
        add("tonic_support_weight", STPDCanonicalValue.double(summary.tonicSupportWeight))
        add("tonic_seed_lower_sec", STPDCanonicalValue.double(summary.tonicSeedLowerSec))
        add("tonic_seed_upper_sec", STPDCanonicalValue.double(summary.tonicSeedUpperSec))
        add("pause_anchor_count", String(summary.pauseAnchorCount))
        add("pause_pool_anchor_count", String(summary.pausePoolAnchorCount))
        add("pause_support_weight", STPDCanonicalValue.double(summary.pauseSupportWeight))
        add(
            "pause_pool_support_weight",
            STPDCanonicalValue.double(summary.pausePoolSupportWeight)
        )
        add("pause_seed_lower_sec", STPDCanonicalValue.double(summary.pauseSeedLowerSec))
        add("pause_seed_upper_sec", STPDCanonicalValue.double(summary.pauseSeedUpperSec))
        add("pause_pool_source", summary.pausePoolSource)
        add("source", summary.source)
        add("is_self_inclusive", STPDCanonicalValue.bool(summary.isSelfInclusive))
    }

    static func candidateLedgerTable(
        identity: DetectionRunIdentity,
        records: [STPDCandidateRecord]
    ) throws -> STPDResultTableData {
        let headers = [
            "run_id", "settings_digest", "candidate_uid", "source_candidate_id", "train_id",
            "train_name", "candidate_layer", "candidate_class", "start_isi_index",
            "end_isi_index", "start_spike_ordinal", "end_spike_ordinal", "n_isi",
            "n_valid_isi", "n_spikes", "anchor_family", "anchor_lock_level",
        ]
        let rows = records.map { record in
            let candidate = record.candidate
            return makeRow(headers, values: [
                "run_id": identity.runID,
                "settings_digest": identity.settingsDigest,
                "candidate_uid": record.uid,
                "source_candidate_id": candidate.id,
                "train_id": candidate.trainID,
                "train_name": candidate.trainName,
                "candidate_layer": candidate.candidateLayer,
                "candidate_class": candidate.candidateClass,
                "start_isi_index": String(candidate.startISIIndex),
                "end_isi_index": String(candidate.endISIIndex),
                "start_spike_ordinal": String(candidate.startSpikeIndex),
                "end_spike_ordinal": String(candidate.endSpikeIndex),
                "n_isi": String(candidate.nISI),
                "n_valid_isi": String(candidate.nValidISI),
                "n_spikes": String(candidate.nSpikes),
                "anchor_family": candidate.anchorFamily,
                "anchor_lock_level": candidate.anchorLockLevel.rawValue,
            ])
        }
        return try table(.candidateLedger, headers: headers, rows: rows)
    }

    static func candidateFeaturesTable(
        identity: DetectionRunIdentity,
        records: [STPDCandidateRecord],
        candidateUIDBySourceID: [String: String]
    ) throws -> STPDResultTableData {
        let headers = [
            "run_id", "settings_digest", "candidate_uid", "source_candidate_id",
            "duration_sec", "intra_q10_sec", "intra_q40_sec", "intra_q50_sec",
            "intra_q90_sec", "intra_q95_sec", "max_intra_isi_sec", "mean_intra_isi_sec",
            "cv", "cv2", "lv", "pre_gap_sec", "post_gap_sec", "pre_ratio_q90",
            "post_ratio_q90", "edge_contrast_min_q90", "edge_contrast_geom_q90", "score",
            "anchor_band_lower_sec", "anchor_band_upper_sec", "anchor_band_source",
            "anchor_band_semantics", "anchor_band_ordered",
            "anchor_contrast_min_required", "anchor_contrast_geom_required",
            "refractory_suspect_count", "refractory_suspect_action",
            "profile_seed_low_percentile", "profile_seed_high_percentile",
            "profile_seed_band_fraction", "profile_seed_run_count", "profile_max_seed_run_length",
            "profile_median_isi_sec", "profile_q10_isi_sec", "profile_q25_isi_sec",
            "profile_q90_isi_sec", "profile_pause_fraction", "profile_phenotype_prior",
            "profile_bridge_upper_sec", "profile_boundary_floor_sec",
            "profile_boundary_floor_hard", "profile_burst_contrast_s",
            "profile_possible_contrast_s",
            "hf_q80_sec", "hf_q80_max_sec", "hf_q90_max_sec", "hf_short_upper_sec",
            "hf_epoch_bridge_sec", "hf_tolerated_gap_sec", "hf_pattern_max_isi_sec",
            "hf_pause_break_sec", "hf_short_fraction", "hf_q90_short_fraction",
            "hf_bridge_fraction", "hf_large_fraction", "hf_tolerated_fraction",
            "hf_max_consecutive_large_isi", "hf_min_spikes_required",
            "hf_acceptance_route", "hf_burst_dominated",
            "hf_embedded_burst_count", "hf_embedded_burst_group_count",
            "hf_embedded_burst_coverage", "hf_burst_packet_like",
            "hf_burst_packet_neighbor", "suppressed_by_hf_state",
            "suppressed_original_label", "hf_suppressor_candidate_uid",
            "state_regularity_score", "state_burst_seed_fraction", "state_low_tail_fraction",
            "state_local_stability_score", "state_core_burst_run_length",
            "state_continuity_authority_frozen", "state_continuity_merge_terminal",
            "state_train_percentile_median", "state_local_percentile_median",
            "state_local_percentile_q90", "state_local_robust_z_median",
            "state_local_robust_z_abs_q80", "state_local_robust_z_q10",
            "burst_seed_run_start_isi", "burst_seed_run_end_isi", "burst_seed_band_lower_sec",
            "burst_seed_band_upper_sec", "burst_bridge_band_upper_sec",
            "burst_contrast_required", "burst_possible_contrast_required",
            "burst_required_gap_sec", "burst_possible_required_gap_sec",
            "burst_boundary_floor_sec", "burst_boundary_floor_hard",
            "burst_strict_boundary_pass", "burst_possible_boundary_pass",
            "burst_bridge_count_pass", "burst_bridge_fraction_pass", "burst_q90_bridge_pass",
            "burst_size_label_before_review",
            "threshold_mode", "hard_threshold", "hard_threshold_pattern",
            "hard_burst_seed_upper_sec", "hard_burst_bridge_upper_sec",
            "hard_burst_core_isi_count", "hard_threshold_source",
            "local_background_q75_sec", "local_compression_q90_ratio",
            "event_local_median_sec", "event_local_percentile_median",
            "event_local_percentile_q90", "event_local_robust_z_median",
            "event_local_robust_z_abs_q80", "event_local_robust_z_q10",
        ]
        let rows = records.map { record in
            let c = record.candidate
            return makeRow(headers, values: [
                "run_id": identity.runID,
                "settings_digest": identity.settingsDigest,
                "candidate_uid": record.uid,
                "source_candidate_id": c.id,
                "duration_sec": STPDCanonicalValue.double(c.durationSec),
                "intra_q10_sec": STPDCanonicalValue.double(c.intraQ10Sec),
                "intra_q40_sec": STPDCanonicalValue.double(c.intraQ40Sec),
                "intra_q50_sec": STPDCanonicalValue.double(c.intraQ50Sec),
                "intra_q90_sec": STPDCanonicalValue.double(c.intraQ90Sec),
                "intra_q95_sec": STPDCanonicalValue.double(c.intraQ95Sec),
                "max_intra_isi_sec": STPDCanonicalValue.double(c.maxIntraISISec),
                "mean_intra_isi_sec": STPDCanonicalValue.double(c.meanIntraISISec),
                "cv": STPDCanonicalValue.double(c.cv),
                "cv2": STPDCanonicalValue.double(c.cv2),
                "lv": STPDCanonicalValue.double(c.lv),
                "pre_gap_sec": STPDCanonicalValue.double(c.preGapSec),
                "post_gap_sec": STPDCanonicalValue.double(c.postGapSec),
                "pre_ratio_q90": STPDCanonicalValue.double(c.preRatioQ90),
                "post_ratio_q90": STPDCanonicalValue.double(c.postRatioQ90),
                "edge_contrast_min_q90": STPDCanonicalValue.double(c.edgeContrastMinQ90),
                "edge_contrast_geom_q90": STPDCanonicalValue.double(c.edgeContrastGeomQ90),
                "score": STPDCanonicalValue.double(c.score),
                "anchor_band_lower_sec": STPDCanonicalValue.double(c.anchorBandLowerSec),
                "anchor_band_upper_sec": STPDCanonicalValue.double(c.anchorBandUpperSec),
                "anchor_band_source": c.anchorBandSource.rawValue,
                "anchor_band_semantics": c.finalLabel == .profile
                    ? "profile_boundary_summary"
                    : "ordered_candidate_band",
                "anchor_band_ordered": STPDCanonicalValue.bool(
                    c.anchorBandUpperSec >= c.anchorBandLowerSec
                ),
                "anchor_contrast_min_required": STPDCanonicalValue.double(c.anchorContrastMinRequired),
                "anchor_contrast_geom_required": STPDCanonicalValue.double(c.anchorContrastGeomRequired),
                "refractory_suspect_count": String(c.refractorySuspectCount),
                "refractory_suspect_action": c.refractorySuspectAction?.rawValue ?? "",
                "profile_seed_low_percentile": STPDCanonicalValue.double(c.profileSeedLowPercentileInTrain),
                "profile_seed_high_percentile": STPDCanonicalValue.double(c.profileSeedHighPercentileInTrain),
                "profile_seed_band_fraction": STPDCanonicalValue.double(c.profileSeedBandFraction),
                "profile_seed_run_count": STPDCanonicalValue.int(c.profileSeedRunCount),
                "profile_max_seed_run_length": STPDCanonicalValue.int(c.profileMaxSeedRunLength),
                "profile_median_isi_sec": STPDCanonicalValue.double(c.profileMedianISISec),
                "profile_q10_isi_sec": STPDCanonicalValue.double(c.profileQ10ISISec),
                "profile_q25_isi_sec": STPDCanonicalValue.double(c.profileQ25ISISec),
                "profile_q90_isi_sec": STPDCanonicalValue.double(c.profileQ90ISISec),
                "profile_pause_fraction": STPDCanonicalValue.double(c.profilePauseFraction),
                "profile_phenotype_prior": c.profilePhenotypePrior ?? "",
                "profile_bridge_upper_sec":
                    STPDCanonicalValue.double(c.profileBridgeUpperSec),
                "profile_boundary_floor_sec":
                    STPDCanonicalValue.double(c.profileBoundaryFloorSec),
                "profile_boundary_floor_hard":
                    STPDCanonicalValue.bool(c.profileBoundaryFloorHard),
                "profile_burst_contrast_s":
                    STPDCanonicalValue.double(c.profileBurstContrastS),
                "profile_possible_contrast_s":
                    STPDCanonicalValue.double(c.profilePossibleContrastS),
                "hf_q80_sec": STPDCanonicalValue.double(c.hfSpikingQ80Sec),
                "hf_q80_max_sec": STPDCanonicalValue.double(c.hfSpikingQ80MaxSec),
                "hf_q90_max_sec": STPDCanonicalValue.double(c.hfSpikingQ90MaxSec),
                "hf_short_upper_sec":
                    STPDCanonicalValue.double(c.hfSpikingShortUpperSec),
                "hf_epoch_bridge_sec":
                    STPDCanonicalValue.double(c.hfSpikingEpochBridgeSec),
                "hf_tolerated_gap_sec":
                    STPDCanonicalValue.double(c.hfSpikingToleratedGapSec),
                "hf_pattern_max_isi_sec":
                    STPDCanonicalValue.double(c.hfSpikingPatternMaxISISec),
                "hf_pause_break_sec":
                    STPDCanonicalValue.double(c.hfSpikingPauseBreakSec),
                "hf_short_fraction": STPDCanonicalValue.double(c.hfSpikingShortFraction),
                "hf_q90_short_fraction":
                    STPDCanonicalValue.double(c.hfSpikingQ90ShortFraction),
                "hf_bridge_fraction": STPDCanonicalValue.double(c.hfSpikingBridgeFraction),
                "hf_large_fraction": STPDCanonicalValue.double(c.hfSpikingLargeFraction),
                "hf_tolerated_fraction":
                    STPDCanonicalValue.double(c.hfSpikingToleratedFraction),
                "hf_max_consecutive_large_isi": STPDCanonicalValue.int(c.hfSpikingMaxConsecutiveLargeISI),
                "hf_min_spikes_required":
                    STPDCanonicalValue.int(c.hfSpikingMinSpikesRequired),
                "hf_acceptance_route": c.hfSpikingAcceptanceRoute ?? "",
                "hf_burst_dominated": STPDCanonicalValue.bool(c.hfSpikingBurstDominated),
                "hf_embedded_burst_count": STPDCanonicalValue.int(c.hfSpikingEmbeddedBurstCount),
                "hf_embedded_burst_group_count":
                    STPDCanonicalValue.int(c.hfSpikingEmbeddedBurstGroupCount),
                "hf_embedded_burst_coverage": STPDCanonicalValue.double(c.hfSpikingEmbeddedBurstCoverage),
                "hf_burst_packet_like":
                    STPDCanonicalValue.bool(c.hfSpikingBurstPacketLike),
                "hf_burst_packet_neighbor":
                    STPDCanonicalValue.bool(c.hfSpikingBurstPacketNeighbor),
                "suppressed_by_hf_state":
                    STPDCanonicalValue.bool(c.suppressedByHFSpikingState),
                "suppressed_original_label": c.suppressedOriginalLabel ?? "",
                "hf_suppressor_candidate_uid":
                    c.hfSpikingSuppressorID.flatMap {
                        candidateUIDBySourceID[$0]
                    } ?? "",
                "state_regularity_score": STPDCanonicalValue.double(c.stateRegularityScore),
                "state_burst_seed_fraction": STPDCanonicalValue.double(c.stateBurstSeedFraction),
                "state_low_tail_fraction": STPDCanonicalValue.double(c.stateLowTailFraction),
                "state_local_stability_score": STPDCanonicalValue.double(c.stateLocalStabilityScore),
                "state_core_burst_run_length": STPDCanonicalValue.int(c.stateCoreBurstRunLength),
                "state_continuity_authority_frozen":
                    STPDCanonicalValue.bool(c.stateContinuityAuthorityFrozen),
                "state_continuity_merge_terminal":
                    STPDCanonicalValue.bool(c.stateContinuityMergeTerminal),
                "state_train_percentile_median": STPDCanonicalValue.double(c.stateTrainPercentileMedian),
                "state_local_percentile_median": STPDCanonicalValue.double(c.stateLocalPercentileMedian),
                "state_local_percentile_q90": STPDCanonicalValue.double(c.stateLocalPercentileQ90),
                "state_local_robust_z_median": STPDCanonicalValue.double(c.stateLocalRobustZMedian),
                "state_local_robust_z_abs_q80": STPDCanonicalValue.double(c.stateLocalRobustZAbsQ80),
                "state_local_robust_z_q10": STPDCanonicalValue.double(c.stateLocalRobustZQ10),
                "burst_seed_run_start_isi": STPDCanonicalValue.int(c.burstSeedRunStartISI),
                "burst_seed_run_end_isi": STPDCanonicalValue.int(c.burstSeedRunEndISI),
                "burst_seed_band_lower_sec": STPDCanonicalValue.double(c.burstSeedBandLowerSec),
                "burst_seed_band_upper_sec": STPDCanonicalValue.double(c.burstSeedBandUpperSec),
                "burst_bridge_band_upper_sec": STPDCanonicalValue.double(c.burstBridgeBandUpperSec),
                "burst_contrast_required": STPDCanonicalValue.double(c.burstContrastRequired),
                "burst_possible_contrast_required": STPDCanonicalValue.double(c.burstPossibleContrastRequired),
                "burst_required_gap_sec": STPDCanonicalValue.double(c.burstRequiredGapSec),
                "burst_possible_required_gap_sec": STPDCanonicalValue.double(c.burstPossibleRequiredGapSec),
                "burst_boundary_floor_sec": STPDCanonicalValue.double(c.burstBoundaryFloorSec),
                "burst_boundary_floor_hard": STPDCanonicalValue.bool(c.burstBoundaryFloorHard),
                "burst_strict_boundary_pass": STPDCanonicalValue.bool(c.burstStrictBoundaryPass),
                "burst_possible_boundary_pass": STPDCanonicalValue.bool(c.burstPossibleBoundaryPass),
                "burst_bridge_count_pass": STPDCanonicalValue.bool(c.burstBridgeCountPass),
                "burst_bridge_fraction_pass": STPDCanonicalValue.bool(c.burstBridgeFractionPass),
                "burst_q90_bridge_pass": STPDCanonicalValue.bool(c.burstQ90BridgePass),
                "burst_size_label_before_review": c.burstSizeLabelBeforeReview ?? "",
                "threshold_mode": c.thresholdMode ?? "",
                "hard_threshold": STPDCanonicalValue.bool(c.hardThreshold),
                "hard_threshold_pattern": c.hardThresholdPattern ?? "",
                "hard_burst_seed_upper_sec": STPDCanonicalValue.double(c.hardBurstSeedUpperSec),
                "hard_burst_bridge_upper_sec": STPDCanonicalValue.double(c.hardBurstBridgeUpperSec),
                "hard_burst_core_isi_count": STPDCanonicalValue.int(c.hardBurstCoreISICount),
                "hard_threshold_source": c.hardThresholdSource ?? "",
                "local_background_q75_sec": STPDCanonicalValue.double(c.localBackgroundQ75Sec),
                "local_compression_q90_ratio": STPDCanonicalValue.double(c.localCompressionQ90Ratio),
                "event_local_median_sec": STPDCanonicalValue.double(c.eventLocalMedianSec),
                "event_local_percentile_median": STPDCanonicalValue.double(c.eventLocalPercentileMedian),
                "event_local_percentile_q90": STPDCanonicalValue.double(c.eventLocalPercentileQ90),
                "event_local_robust_z_median": STPDCanonicalValue.double(c.eventLocalRobustZMedian),
                "event_local_robust_z_abs_q80": STPDCanonicalValue.double(c.eventLocalRobustZAbsQ80),
                "event_local_robust_z_q10": STPDCanonicalValue.double(c.eventLocalRobustZQ10),
            ])
        }
        return try table(.candidateFeatures, headers: headers, rows: rows)
    }

    static func finalDecisionsTable(
        identity: DetectionRunIdentity,
        records: [STPDCandidateRecord]
    ) throws -> STPDResultTableData {
        let headers = [
            "run_id", "settings_digest", "candidate_uid", "source_candidate_id",
            "final_label", "gate_status", "action", "priority", "selected_for_auto",
            "selection_status", "semantic_track", "event_track_class", "audit_family",
            "audit_subtype", "audit_final_class", "audit_review_status",
            "audit_review_required", "audit_confidence_tier", "audit_uncertainty_reason",
            "audit_long_burst_definition_status", "failure_reason",
            "candidate_diagnostic_class", "state_tonic_subtype",
            "state_high_frequency_subtype", "decision_path",
        ]
        let rows = records.map { record in
            let candidate = record.candidate
            return makeRow(headers, values: [
                "run_id": identity.runID,
                "settings_digest": identity.settingsDigest,
                "candidate_uid": record.uid,
                "source_candidate_id": candidate.id,
                "final_label": candidate.finalLabel.rawValue,
                "gate_status": candidate.gateStatus,
                "action": candidate.action,
                "priority": String(candidate.priority),
                "selected_for_auto": STPDCanonicalValue.bool(candidate.selectedForAuto),
                "selection_status": candidate.selectionStatus,
                "semantic_track": candidate.auditRecommendedTrackRawValue,
                "event_track_class": candidate.auditRecommendedEventTrackClass,
                "audit_family": candidate.auditRecommendedFamily,
                "audit_subtype": candidate.auditRecommendedSubtype,
                "audit_final_class": candidate.auditRecommendedFinalClass,
                "audit_review_status": candidate.auditReviewStatus,
                "audit_review_required": STPDCanonicalValue.bool(candidate.auditReviewRequired),
                "audit_confidence_tier": candidate.auditConfidenceTier,
                "audit_uncertainty_reason": candidate.auditUncertaintyReason,
                "audit_long_burst_definition_status": candidate.auditLongBurstDefinitionStatus,
                "failure_reason": candidate.failureReason,
                "candidate_diagnostic_class": candidate.candidateDiagnosticClass,
                "state_tonic_subtype": candidate.stateTonicSubtype ?? "",
                "state_high_frequency_subtype":
                    candidate.stateHighFrequencySubtype ?? "",
                "decision_path": candidate.decisionPath,
            ])
        }
        return try table(.finalDecisions, headers: headers, rows: rows)
    }

    static func eventsTable(
        identity: DetectionRunIdentity,
        records: [STPDEventRecord],
        evidenceUIDsByISIKey: [String: [String]],
        changedISIKeys: Set<String>
    ) throws -> STPDResultTableData {
        let headers = [
            "run_id", "settings_digest", "event_uid", "source_event_ids",
            "train_id", "train_name", "final_label", "state_tonic_subtype",
            "state_high_frequency_subtypes", "semantic_track", "event_track_class",
            "lock_level", "authority_origin",
            "start_isi_index", "end_isi_index", "start_spike_ordinal", "end_spike_ordinal",
            "raw_start_sec", "raw_end_sec", "aligned_start_sec", "aligned_end_sec",
            "duration_sec", "score", "priority", "source_candidate_uids",
            "unresolved_source_candidate_ids", "automatic_support_isi_indices",
            "review_evidence_uids", "review_evidence_present",
            "review_changed_projection",
            "audit_recommended_subtype", "audit_review_status", "decision_path",
        ]
        let rows = records.map { record in
            let event = record.event
            let evidenceUIDs = Set(
                (event.startISIIndex...event.endISIIndex)
                    .flatMap {
                        evidenceUIDsByISIKey[
                            isiEvidenceKey(trainID: event.trainID, isiIndex: $0)
                        ] ?? []
                    }
            ).sorted()
            let changedProjection =
                (event.startISIIndex...event.endISIIndex)
                .contains {
                    changedISIKeys.contains(
                        isiEvidenceKey(trainID: event.trainID, isiIndex: $0)
                    )
                }
            return makeRow(headers, values: [
                "run_id": identity.runID,
                "settings_digest": identity.settingsDigest,
                "event_uid": record.uid,
                "source_event_ids": STPDCanonicalValue.stringList(event.sourceEventIDs),
                "train_id": event.trainID,
                "train_name": event.trainName,
                "final_label": event.finalLabel,
                "state_tonic_subtype": event.stateTonicSubtype,
                "state_high_frequency_subtypes":
                    STPDCanonicalValue.stringList(event.stateHighFrequencySubtypes),
                "semantic_track": event.semanticTrack,
                "event_track_class": event.eventTrackClass,
                "lock_level": event.lockLevel,
                "authority_origin": event.authorityOrigin,
                "start_isi_index": String(event.startISIIndex),
                "end_isi_index": String(event.endISIIndex),
                "start_spike_ordinal": String(event.startSpikeOrdinal),
                "end_spike_ordinal": String(event.endSpikeOrdinal),
                "raw_start_sec": STPDCanonicalValue.double(event.rawStartSec),
                "raw_end_sec": STPDCanonicalValue.double(event.rawEndSec),
                "aligned_start_sec": STPDCanonicalValue.double(event.alignedStartSec),
                "aligned_end_sec": STPDCanonicalValue.double(event.alignedEndSec),
                "duration_sec":
                    STPDCanonicalValue.double(event.rawEndSec - event.rawStartSec),
                "score": STPDCanonicalValue.double(event.score),
                "priority": STPDCanonicalValue.int(event.priority),
                "source_candidate_uids":
                    STPDCanonicalValue.stringList(record.sourceCandidateUIDs),
                "unresolved_source_candidate_ids":
                    STPDCanonicalValue.stringList(record.unresolvedSourceCandidateIDs),
                "automatic_support_isi_indices": STPDCanonicalValue.stringList(
                    event.automaticSupportISIIndices.map(String.init)
                ),
                "review_evidence_uids": STPDCanonicalValue.stringList(evidenceUIDs),
                "review_evidence_present":
                    STPDCanonicalValue.bool(!evidenceUIDs.isEmpty),
                "review_changed_projection":
                    STPDCanonicalValue.bool(changedProjection),
                "audit_recommended_subtype": event.auditRecommendedSubtype,
                "audit_review_status": event.auditReviewStatus,
                "decision_path": event.decisionPath,
            ])
        }
        return try table(.eventsFinal, headers: headers, rows: rows)
    }

    static func isiLabelsTable(
        identity: DetectionRunIdentity,
        rows: [ReviewedISIExportRow],
        candidateUIDBySourceID: [String: String],
        evidenceUIDsByISIKey: [String: [String]],
        changedISIKeys: Set<String>,
        qualitySettings: SpikeQualitySettings,
        dataset: SpikeDataset
    ) throws -> STPDResultTableData {
        let headers = [
            "run_id", "settings_digest", "isi_uid", "train_id", "train_name", "isi_index",
            "left_spike_array_index", "right_spike_array_index",
            "left_spike_ordinal", "right_spike_ordinal",
            "timestamp_sec", "aligned_timestamp_sec", "isi_sec",
            "auto_pattern", "auto_subtype", "auto_source_candidate_id",
            "auto_candidate_uid", "final_pattern", "final_subtype", "final_source",
            "manual_veto_suppressed", "review_note", "review_evidence_uids",
            "review_evidence_present", "review_changed_projection",
            "isi_qc_class", "artifact_floor_status", "qc_refractory_suspect",
            "qc_artifact_threshold_sec", "qc_refractory_threshold_sec",
            "train_qc_warning_level", "train_qc_warning_message",
            "train_qc_duration_sec", "train_qc_firing_rate_hz",
            "train_qc_raw_min_isi_sec", "train_qc_min_valid_isi_sec",
            "train_qc_artifact_min_isi_sec", "train_qc_median_isi_sec",
            "train_qc_max_isi_sec", "train_qc_duplicate_timestamp_count",
            "train_qc_zero_or_negative_isi_count",
            "train_qc_zero_or_negative_timestamp_step_count",
            "train_qc_input_was_unsorted",
            "train_qc_input_nonmonotonic_step_count",
            "train_qc_input_duplicate_timestamp_step_count",
            "train_qc_input_zero_or_negative_step_count",
            "train_qc_dropped_duplicate_timestamp_count",
            "train_qc_duplicate_timestamp_policy",
            "train_qc_artifact_isi_count", "train_qc_artifact_fraction",
            "train_qc_refractory_suspect_isi_count",
            "train_qc_refractory_suspect_fraction",
            "train_qc_valid_isi_count", "train_qc_percentile_status",
        ]
        let trainsByID = Dictionary(
            uniqueKeysWithValues: dataset.trains.map { ($0.id, $0) }
        )
        let qualitiesByTrainID = Dictionary(
            uniqueKeysWithValues: dataset.trains.map { train in
                (
                    train.id,
                    SpikeQualityAnalyzer.quality(
                        for: train,
                        settings: qualitySettings
                    )
                )
            }
        )
        let materialized = try rows.map { row -> [String] in
            let sourceID = row.autoCandidateID
            let candidateUID = sourceID.isEmpty ? "" : candidateUIDBySourceID[sourceID]
            if !sourceID.isEmpty, candidateUID == nil {
                throw STPDResultPackageError.invalidInput(
                    "ISI row references unknown automatic candidate \(sourceID)"
                )
            }
            guard trainsByID[row.trainID] != nil,
                  let quality = qualitiesByTrainID[row.trainID] else {
                throw STPDResultPackageError.invalidInput(
                    "ISI row references unknown QC train \(row.trainID)"
                )
            }
            let key = isiRowKey(row)
            let evidenceUIDs = evidenceUIDsByISIKey[key] ?? []
            let qcClass = isiQCClass(
                row.isiSec,
                settings: qualitySettings
            )
            return makeRow(headers, values: [
                "run_id": identity.runID,
                "settings_digest": identity.settingsDigest,
                "isi_uid": stableISIUID(
                    datasetDigest: identity.datasetDigest,
                    trainID: row.trainID,
                    isiIndex: row.isiIndex
                ),
                "train_id": row.trainID,
                "train_name": row.trainName,
                "isi_index": String(row.isiIndex),
                "left_spike_array_index": String(row.spikeIndex - 1),
                "right_spike_array_index": String(row.spikeIndex),
                "left_spike_ordinal": String(row.spikeIndex),
                "right_spike_ordinal": String(row.spikeIndex + 1),
                "timestamp_sec": STPDCanonicalValue.double(row.timestampSec),
                "aligned_timestamp_sec": STPDCanonicalValue.double(row.alignedTimestampSec),
                "isi_sec": STPDCanonicalValue.double(row.isiSec),
                "auto_pattern": row.autoPattern,
                "auto_subtype": row.autoSubtype,
                "auto_source_candidate_id": sourceID,
                "auto_candidate_uid": candidateUID ?? "",
                "final_pattern": row.finalPattern,
                "final_subtype": row.finalSubtype,
                "final_source": row.finalSource,
                "manual_veto_suppressed": STPDCanonicalValue.bool(row.manualVetoSuppressed),
                "review_note": row.reviewNote,
                "review_evidence_uids": STPDCanonicalValue.stringList(evidenceUIDs),
                "review_evidence_present":
                    STPDCanonicalValue.bool(!evidenceUIDs.isEmpty),
                "review_changed_projection":
                    STPDCanonicalValue.bool(changedISIKeys.contains(key)),
                "isi_qc_class": qcClass,
                "artifact_floor_status":
                    qcClass == "artifact_below_floor"
                    ? "below_artifact_floor"
                    : "at_or_above_artifact_floor",
                "qc_refractory_suspect":
                    STPDCanonicalValue.bool(qcClass == "refractory_suspect"),
                "qc_artifact_threshold_sec":
                    STPDCanonicalValue.double(qualitySettings.artifactThresholdSec),
                "qc_refractory_threshold_sec":
                    STPDCanonicalValue.double(
                        qualitySettings.refractorySuspectThresholdSec
                    ),
                "train_qc_warning_level": quality.warningLevel.rawValue,
                "train_qc_warning_message": quality.warningMessage,
                "train_qc_duration_sec":
                    STPDCanonicalValue.double(quality.durationSec),
                "train_qc_firing_rate_hz":
                    STPDCanonicalValue.double(quality.firingRateHz),
                "train_qc_raw_min_isi_sec":
                    STPDCanonicalValue.double(quality.rawMinISISec),
                "train_qc_min_valid_isi_sec":
                    STPDCanonicalValue.double(quality.minValidISISec),
                "train_qc_artifact_min_isi_sec":
                    STPDCanonicalValue.double(quality.artifactMinISISec),
                "train_qc_median_isi_sec":
                    STPDCanonicalValue.double(quality.medianISISec),
                "train_qc_max_isi_sec":
                    STPDCanonicalValue.double(quality.maxISISec),
                "train_qc_duplicate_timestamp_count":
                    String(quality.duplicateTimestampCount),
                "train_qc_zero_or_negative_isi_count":
                    String(quality.zeroOrNegativeISICount),
                "train_qc_zero_or_negative_timestamp_step_count":
                    String(quality.zeroOrNegativeTimestampStepCount),
                "train_qc_input_was_unsorted":
                    STPDCanonicalValue.bool(quality.inputWasUnsorted),
                "train_qc_input_nonmonotonic_step_count":
                    String(quality.inputNonmonotonicStepCount),
                "train_qc_input_duplicate_timestamp_step_count":
                    String(quality.inputDuplicateTimestampStepCount),
                "train_qc_input_zero_or_negative_step_count":
                    String(quality.inputZeroOrNegativeStepCount),
                "train_qc_dropped_duplicate_timestamp_count":
                    String(quality.droppedDuplicateTimestampCount),
                "train_qc_duplicate_timestamp_policy":
                    quality.duplicateTimestampPolicy.rawValue,
                "train_qc_artifact_isi_count":
                    String(quality.artifactISICount),
                "train_qc_artifact_fraction":
                    STPDCanonicalValue.double(quality.artifactFraction),
                "train_qc_refractory_suspect_isi_count":
                    String(quality.refractorySuspectISICount),
                "train_qc_refractory_suspect_fraction":
                    STPDCanonicalValue.double(quality.refractorySuspectFraction),
                "train_qc_valid_isi_count":
                    String(quality.validISICount),
                "train_qc_percentile_status": quality.percentileStatus,
            ])
        }
        return try table(.isiLabelsFinal, headers: headers, rows: materialized)
    }

    static func diagnosticsTable(
        identity: DetectionRunIdentity,
        candidates: [STPDCandidateRecord],
        events: [STPDEventRecord],
        supplied: [STPDCandidateDiagnosticInput],
        candidateUIDBySourceID: [String: String]
    ) throws -> STPDResultTableData {
        let headers = [
            "run_id", "settings_digest", "candidate_uid", "stage_id",
            "source_candidate_id", "stage_name", "stage_ordinal", "evidence_kind",
            "status", "details", "event_uid", "automatic_source_id",
            "source_support_isi_indices", "source_semantic_track",
            "source_event_track_class", "source_label", "source_lock_level",
            "source_state_tonic_subtype", "source_score", "source_priority",
            "source_decision_path",
        ]
        var rows: [[String]] = candidates.map { record in
            let candidate = record.candidate
            let stageID = STPDStableIdentifier.make(
                prefix: "stage",
                domain: "stpd_candidate_diagnostic_stage_v1",
                components: [record.uid, "terminal_decision", "0"]
            )
            return makeRow(headers, values: [
                "run_id": identity.runID,
                "settings_digest": identity.settingsDigest,
                "candidate_uid": record.uid,
                "stage_id": stageID,
                "source_candidate_id": candidate.id,
                "stage_name": "terminal_decision",
                "stage_ordinal": "0",
                "evidence_kind": "candidate_terminal",
                "status": candidate.selectionStatus,
                "details": candidate.decisionPath,
            ])
        }
        for diagnostic in supplied {
            guard let candidateUID = candidateUIDBySourceID[diagnostic.sourceCandidateID] else {
                throw STPDResultPackageError.invalidInput(
                    "candidate diagnostic references unknown candidate \(diagnostic.sourceCandidateID)"
                )
            }
            let stageID = STPDStableIdentifier.make(
                prefix: "stage",
                domain: "stpd_candidate_diagnostic_stage_v1",
                components: [
                    candidateUID,
                    diagnostic.stageName,
                    String(diagnostic.stageOrdinal),
                ]
            )
            rows.append(makeRow(headers, values: [
                "run_id": identity.runID,
                "settings_digest": identity.settingsDigest,
                "candidate_uid": candidateUID,
                "stage_id": stageID,
                "source_candidate_id": diagnostic.sourceCandidateID,
                "stage_name": diagnostic.stageName,
                "stage_ordinal": String(diagnostic.stageOrdinal),
                "evidence_kind": "candidate_diagnostic",
                "status": diagnostic.status,
                "details": diagnostic.details,
            ]))
        }
        for record in events {
            let normalizedSources = record.event.automaticEventSources.sorted {
                if $0.candidateID != $1.candidateID {
                    return $0.candidateID < $1.candidateID
                }
                if $0.annotationID != $1.annotationID {
                    return $0.annotationID < $1.annotationID
                }
                return $0.supportISIIndices.lexicographicallyPrecedes(
                    $1.supportISIIndices
                )
            }
            for (ordinal, source) in normalizedSources.enumerated() {
                guard let candidateUID = candidateUIDBySourceID[source.candidateID] else {
                    throw STPDResultPackageError.invalidInput(
                        "event source references unknown candidate \(source.candidateID)"
                    )
                }
                let stageID = STPDStableIdentifier.make(
                    prefix: "stage",
                    domain: "stpd_event_source_evidence_stage_v1",
                    components: [
                        candidateUID,
                        record.uid,
                        source.annotationID,
                        source.supportISIIndices.map(String.init).joined(separator: "|"),
                    ]
                )
                rows.append(makeRow(headers, values: [
                    "run_id": identity.runID,
                    "settings_digest": identity.settingsDigest,
                    "candidate_uid": candidateUID,
                    "stage_id": stageID,
                    "source_candidate_id": source.candidateID,
                    "stage_name": "event_source",
                    "stage_ordinal": String(ordinal),
                    "evidence_kind": "event_source",
                    "status": "projected",
                    "details": source.decisionPath,
                    "event_uid": record.uid,
                    "automatic_source_id": source.annotationID,
                    "source_support_isi_indices": STPDCanonicalValue.stringList(
                        source.supportISIIndices.map(String.init)
                    ),
                    "source_semantic_track": source.semanticTrack.rawValue,
                    "source_event_track_class": source.eventTrackClass,
                    "source_label": source.label.rawValue,
                    "source_lock_level": source.lockLevel.rawValue,
                    "source_state_tonic_subtype": source.stateTonicSubtype ?? "",
                    "source_score": STPDCanonicalValue.double(source.score),
                    "source_priority": String(source.priority),
                    "source_decision_path": source.decisionPath,
                ]))
            }
        }
        return try table(.candidateDiagnosticAudit, headers: headers, rows: rows)
    }

    static func manualAnnotationsTable(
        identity: DetectionRunIdentity,
        annotations: [ManualAnnotation],
        manualUIDByUUID: [UUID: String],
        linkedISIKeysByEvidenceUID: [String: [String]],
        isiUIDByKey: [String: String]
    ) throws -> STPDResultTableData {
        let headers = [
            "run_id", "settings_digest", "annotation_id", "source_annotation_uuid",
            "train_id", "label", "polarity", "start_sec", "end_sec",
            "start_isi_index", "end_isi_index",
            "start_spike_array_index", "end_spike_array_index",
            "start_spike_ordinal", "end_spike_ordinal",
            "linked_isi_uids", "link_scope", "note", "created_at", "updated_at",
        ]
        let sorted = annotations.sorted {
            let lhs = manualAnnotationIdentityComponents($0)
            let rhs = manualAnnotationIdentityComponents($1)
            if lhs != rhs {
                return lhs.lexicographicallyPrecedes(rhs)
            }
            return $0.id.uuidString < $1.id.uuidString
        }
        let rows = try sorted.map { annotation -> [String] in
            guard let annotationID = manualUIDByUUID[annotation.id] else {
                throw STPDResultPackageError.invalidInput(
                    "manual annotation is missing its resolved authority identity"
                )
            }
            let linkedUIDs = try (linkedISIKeysByEvidenceUID[annotationID] ?? [])
                .map { key -> String in
                    guard let uid = isiUIDByKey[key] else {
                        throw STPDResultPackageError.invalidInput(
                            "manual annotation links an unknown public ISI"
                        )
                    }
                    return uid
                }
                .sorted()
            return makeRow(headers, values: [
                "run_id": identity.runID,
                "settings_digest": identity.settingsDigest,
                "annotation_id": annotationID,
                "source_annotation_uuid": annotation.id.uuidString.lowercased(),
                "train_id": annotation.trainID,
                "label": annotation.label.rawValue,
                "polarity": annotation.polarity.rawValue,
                "start_sec": STPDCanonicalValue.double(annotation.normalizedStartSec),
                "end_sec": STPDCanonicalValue.double(annotation.normalizedEndSec),
                "start_isi_index": STPDCanonicalValue.int(annotation.startISIIndex),
                "end_isi_index": STPDCanonicalValue.int(annotation.endISIIndex),
                "start_spike_array_index":
                    STPDCanonicalValue.int(annotation.startSpikeIndex),
                "end_spike_array_index":
                    STPDCanonicalValue.int(annotation.endSpikeIndex),
                "start_spike_ordinal":
                    STPDCanonicalValue.int(annotation.startSpikeIndex.map { $0 + 1 }),
                "end_spike_ordinal":
                    STPDCanonicalValue.int(annotation.endSpikeIndex.map { $0 + 1 }),
                "linked_isi_uids": STPDCanonicalValue.stringList(linkedUIDs),
                "link_scope": "public_projection",
                "note": annotation.note ?? "",
                "created_at": STPDCanonicalValue.date(annotation.createdAt),
                "updated_at": STPDCanonicalValue.date(annotation.updatedAt),
            ])
        }
        return try table(.manualAnnotations, headers: headers, rows: rows)
    }

    static func reviewStatusTable(
        identity: DetectionRunIdentity,
        reviews: [STPDCandidateReviewInput],
        candidateUIDBySourceID: [String: String],
        reviewUIDBySourceCandidateID: [String: String],
        linkedISIKeysByEvidenceUID: [String: [String]],
        isiUIDByKey: [String: String]
    ) throws -> STPDResultTableData {
        let headers = [
            "run_id", "settings_digest", "candidate_uid", "review_uid",
            "source_candidate_id", "status", "reviewer", "linked_isi_uids",
            "link_scope", "note", "reviewed_at",
        ]
        let rows = try reviews.map { review -> [String] in
            guard let candidateUID = candidateUIDBySourceID[review.sourceCandidateID] else {
                throw STPDResultPackageError.invalidInput(
                    "candidate review references unknown candidate \(review.sourceCandidateID)"
                )
            }
            guard let reviewUID =
                    reviewUIDBySourceCandidateID[review.sourceCandidateID] else {
                throw STPDResultPackageError.invalidInput(
                    "candidate review is missing its resolved authority identity"
                )
            }
            let linkedUIDs = try (linkedISIKeysByEvidenceUID[reviewUID] ?? [])
                .map { key -> String in
                    guard let uid = isiUIDByKey[key] else {
                        throw STPDResultPackageError.invalidInput(
                            "candidate review links an unknown public ISI"
                        )
                    }
                    return uid
                }
                .sorted()
            return makeRow(headers, values: [
                "run_id": identity.runID,
                "settings_digest": identity.settingsDigest,
                "candidate_uid": candidateUID,
                "review_uid": reviewUID,
                "source_candidate_id": review.sourceCandidateID,
                "status": review.status.rawValue,
                "reviewer": review.reviewer,
                "linked_isi_uids": STPDCanonicalValue.stringList(linkedUIDs),
                "link_scope": "public_projection",
                "note": review.note,
                "reviewed_at": STPDCanonicalValue.date(review.reviewedAt),
            ])
        }
        return try table(.reviewStatus, headers: headers, rows: rows)
    }

    static func hfsAuditTable(
        identity: DetectionRunIdentity,
        rows: [HFSBurstArbitrationAuditRow],
        candidateUIDBySourceID: [String: String],
        candidateBySourceID: [String: ClassicAnchorCandidate],
        dataset: SpikeDataset,
        finalEvents: [STPDEventRecord],
        sourceMode: STPDResultPackageSourceMode
    ) throws -> STPDResultTableData {
        let baseHeaders = [
            "run_id", "settings_digest", "audit_row_id", "hfs_candidate_uid",
            "hfs_root_lineage_uid", "hfs_root_reference_kind",
            "strongest_burst_candidate_uid",
            "strongest_long_burst_candidate_uid", "unresolved_candidate_ids",
            "package_final_event_count", "package_final_event_subtypes",
            "package_final_projection_differs_from_audit",
        ]
        let auditHeaders = HFSBurstArbitrationAuditRow.csvHeader.map { "audit_\($0)" }
        let headers = baseHeaders + auditHeaders
        let records = try rows.map { row -> (
            row: HFSBurstArbitrationAuditRow,
            fields: [String],
            identity: [String],
            rootLineageUID: String,
            packageEventSubtypes: [String]
        ) in
            try validateHFSRow(
                row,
                candidateUIDBySourceID: candidateUIDBySourceID,
                candidateBySourceID: candidateBySourceID,
                dataset: dataset
            )
            try validateHFSFinalCandidateSnapshot(
                row,
                candidateBySourceID: candidateBySourceID
            )
            let packageEventSubtypes = finalEvents
                .filter {
                    $0.event.trainID == row.trainID &&
                        (ClassicAnchorLabel(rawValue: $0.event.finalLabel)?
                            .isBurstEventFamily == true) &&
                        spansOverlap(
                            lhsStart: row.hfsStartISIIndex,
                            lhsEnd: row.hfsEndISIIndex,
                            rhsStart: $0.event.startISIIndex,
                            rhsEnd: $0.event.endISIIndex
                        )
                }
                .map { hfsBurstSubtype($0.event) }
                .sorted()
            if sourceMode == .automatic,
               packageEventSubtypes != row.finalSelectedEventSubtypes.sorted() {
                throw STPDResultPackageError.invalidInput(
                    "HFS audit row \(row.id) contradicts the automatic final-event projection"
                )
            }
            let rootLineageUID = try hfsRootLineageUID(
                row,
                datasetDigest: identity.datasetDigest,
                candidateBySourceID: candidateBySourceID
            )
            let fields = hfsAuditCanonicalFields(row)
            let identity = hfsAuditIdentityComponents(
                fields: fields,
                candidateUIDBySourceID: candidateUIDBySourceID,
                rootLineageUID: rootLineageUID
            )
            return (
                row,
                fields,
                identity,
                rootLineageUID,
                packageEventSubtypes
            )
        }
        var seenIdentities = Set<String>()
        for record in records {
            let key = compositeKey(record.identity)
            guard seenIdentities.insert(key).inserted else {
                throw STPDResultPackageError.invalidInput(
                    "duplicate HFS arbitration snapshot for \(record.row.hfsCandidateID)"
                )
            }
        }
        let sorted = records.sorted {
            let lhs = $0.identity
            let rhs = $1.identity
            if lhs != rhs {
                return lhs.lexicographicallyPrecedes(rhs)
            }
            return $0.row.id < $1.row.id
        }
        let materialized = sorted.map { record in
            let row = record.row
            let base = record.identity
            let auditRowID = STPDStableIdentifier.make(
                prefix: "hfs_audit",
                domain: "stpd_hfs_burst_audit_uid_v2",
                components: [identity.datasetDigest] + base
            )
            var values: [String: String] = [
                "run_id": identity.runID,
                "settings_digest": identity.settingsDigest,
                "audit_row_id": auditRowID,
                "hfs_candidate_uid": candidateUIDBySourceID[row.hfsCandidateID] ?? "",
                "hfs_root_lineage_uid": record.rootLineageUID,
                "hfs_root_reference_kind":
                    candidateUIDBySourceID[row.hfsRootCandidateID] == nil
                    ? "split_lineage"
                    : "candidate",
                "strongest_burst_candidate_uid":
                    row.strongestBurstCandidateID.flatMap { candidateUIDBySourceID[$0] } ?? "",
                "strongest_long_burst_candidate_uid":
                    row.strongestLongBurstCandidateID.flatMap {
                        candidateUIDBySourceID[$0]
                    } ?? "",
                "unresolved_candidate_ids": STPDCanonicalValue.stringList([]),
                "package_final_event_count":
                    String(record.packageEventSubtypes.count),
                "package_final_event_subtypes":
                    STPDCanonicalValue.stringList(record.packageEventSubtypes),
                "package_final_projection_differs_from_audit":
                    STPDCanonicalValue.bool(
                        record.packageEventSubtypes !=
                            row.finalSelectedEventSubtypes.sorted()
                    ),
            ]
            for (header, field) in zip(auditHeaders, record.fields) {
                values[header] = field
            }
            values["audit_audit_id"] = auditRowID
            return makeRow(headers, values: values)
        }
        return try table(.hfsBurstArbitrationAudit, headers: headers, rows: materialized)
    }

    static func consistencyTable(
        identity: DetectionRunIdentity,
        checks: [STPDConsistencyCheck]
    ) throws -> STPDResultTableData {
        let headers = [
            "run_id", "settings_digest", "check_id", "status", "severity", "details",
        ]
        let rows = checks.map { check in
            makeRow(headers, values: [
                "run_id": identity.runID,
                "settings_digest": identity.settingsDigest,
                "check_id": check.id,
                "status": check.status,
                "severity": check.severity,
                "details": check.details,
            ])
        }
        return try table(.resultConsistencyCheck, headers: headers, rows: rows)
    }

    static func manualAnnotationIdentityComponents(
        _ annotation: ManualAnnotation
    ) -> [String] {
        [
            annotation.trainID,
            annotation.label.rawValue,
            annotation.polarity.rawValue,
            STPDCanonicalValue.double(annotation.normalizedStartSec),
            STPDCanonicalValue.double(annotation.normalizedEndSec),
            STPDCanonicalValue.int(annotation.startISIIndex),
            STPDCanonicalValue.int(annotation.endISIIndex),
            STPDCanonicalValue.int(annotation.startSpikeIndex),
            STPDCanonicalValue.int(annotation.endSpikeIndex),
        ]
    }

    static func hfsAuditIdentityComponents(
        fields: [String],
        candidateUIDBySourceID: [String: String],
        rootLineageUID: String
    ) -> [String] {
        var identity = fields
        identity[0] = ""
        for index in [4, 16, 20] {
            let sourceID = identity[index]
            identity[index] = sourceID.isEmpty ? "" : candidateUIDBySourceID[sourceID] ?? ""
        }
        identity[5] = rootLineageUID
        identity[49] = hfsStableDecisionReason(identity[49])
        return identity
    }

    static func hfsStableDecisionReason(_ reason: String) -> String {
        let transientKeys = Set(["hfs_candidate", "suppressed_event_ids"])
        return reason
            .split(separator: ";", omittingEmptySubsequences: true)
            .map(String.init)
            .filter { token in
                let key = token.split(
                    separator: "=",
                    maxSplits: 1,
                    omittingEmptySubsequences: false
                ).first.map(String.init) ?? token
                return !transientKeys.contains(key)
            }
            .sorted()
            .joined(separator: ";")
    }

    static func hfsRootLineageUID(
        _ row: HFSBurstArbitrationAuditRow,
        datasetDigest: String,
        candidateBySourceID: [String: ClassicAnchorCandidate]
    ) throws -> String {
        let rootStart: Int
        let rootEnd: Int
        if let rootCandidate = candidateBySourceID[row.hfsRootCandidateID] {
            let start = min(rootCandidate.startISIIndex, rootCandidate.endISIIndex)
            let end = max(rootCandidate.startISIIndex, rootCandidate.endISIIndex)
            guard rootCandidate.trainID == row.trainID,
                  rootCandidate.trainName == row.trainName,
                  rootCandidate.finalLabel == .highFrequencySpiking,
                  start <= row.hfsStartISIIndex,
                  end >= row.hfsEndISIIndex else {
                throw STPDResultPackageError.invalidInput(
                    "HFS audit row \(row.id) root candidate does not contain its HFS span"
                )
            }
            rootStart = start
            rootEnd = end
        } else {
            let childPrefix = row.hfsRootCandidateID + "::state-split::"
            guard !row.hfsRootCandidateID.isEmpty,
                  row.hfsCandidateID.hasPrefix(childPrefix) else {
                throw STPDResultPackageError.invalidInput(
                    "HFS audit row \(row.id) has an unresolved root that is not its split lineage"
                )
            }
            let children = candidateBySourceID.values
                .filter { $0.id.hasPrefix(childPrefix) }
            guard !children.isEmpty,
                  children.contains(where: { $0.id == row.hfsCandidateID }) else {
                throw STPDResultPackageError.invalidInput(
                    "HFS audit row \(row.id) has no normalized current candidate in its split lineage"
                )
            }
            var childStarts: [Int] = []
            var childEnds: [Int] = []
            for child in children {
                guard let declared = hfsSplitGeometry(
                    candidateID: child.id,
                    rootID: row.hfsRootCandidateID
                ) else {
                    throw STPDResultPackageError.invalidInput(
                        "HFS split candidate \(child.id) has malformed lineage geometry"
                    )
                }
                let start = min(child.startISIIndex, child.endISIIndex)
                let end = max(child.startISIIndex, child.endISIIndex)
                guard child.trainID == row.trainID,
                      child.trainName == row.trainName,
                      child.finalLabel == .highFrequencySpiking,
                      declared.start == start,
                      declared.end == end else {
                    throw STPDResultPackageError.invalidInput(
                        "HFS split candidate \(child.id) contradicts its root, train, label, or geometry"
                    )
                }
                childStarts.append(start)
                childEnds.append(end)
            }
            guard let start = childStarts.min(),
                  let end = childEnds.max(),
                  start <= row.hfsStartISIIndex,
                  end >= row.hfsEndISIIndex else {
                throw STPDResultPackageError.invalidInput(
                    "HFS audit row \(row.id) lies outside its normalized split lineage"
                )
            }
            rootStart = start
            rootEnd = end
        }
        return STPDStableIdentifier.make(
            prefix: "hfs_lineage",
            domain: "stpd_hfs_root_lineage_uid_v2",
            components: [
                datasetDigest,
                row.trainID,
                ClassicAnchorLabel.highFrequencySpiking.rawValue,
                STPDCanonicalValue.int(rootStart),
                STPDCanonicalValue.int(rootEnd),
            ]
        )
    }

    static func hfsSplitGeometry(
        candidateID: String,
        rootID: String
    ) -> (start: Int, end: Int)? {
        let prefix = rootID + "::state-split::"
        guard !rootID.isEmpty, candidateID.hasPrefix(prefix) else {
            return nil
        }
        let suffix = candidateID.dropFirst(prefix.count)
        let parts = suffix.split(
            separator: "-",
            omittingEmptySubsequences: false
        )
        guard parts.count == 2,
              let start = Int(parts[0]),
              let end = Int(parts[1]),
              start >= 1,
              start <= end else {
            return nil
        }
        return (start, end)
    }

    static func spansOverlap(
        lhsStart: Int,
        lhsEnd: Int,
        rhsStart: Int,
        rhsEnd: Int
    ) -> Bool {
        max(min(lhsStart, lhsEnd), min(rhsStart, rhsEnd)) <=
            min(max(lhsStart, lhsEnd), max(rhsStart, rhsEnd))
    }

    static func hfsBurstSubtype(_ candidate: ClassicAnchorCandidate) -> String {
        switch candidate.finalLabel {
        case .burst:
            return "burst_i"
        case .possibleBurst:
            return candidate.arbitrationTrack == .event
                ? "burst_ii"
                : "possible_burst_review"
        case .highFrequencyBurst:
            return "hf_burst"
        case .longBurst:
            return "long_burst"
        default:
            return candidate.finalLabel.rawValue
        }
    }

    static func hfsBurstSubtype(_ event: ClassicAnchorEventAnnotation) -> String {
        switch event.label {
        case .burst:
            return "burst_i"
        case .possibleBurst:
            return event.semanticTrack == .event
                ? "burst_ii"
                : "possible_burst_review"
        case .highFrequencyBurst:
            return "hf_burst"
        case .longBurst:
            return "long_burst"
        default:
            return event.label.rawValue
        }
    }

    static func hfsBurstSubtype(_ event: STPDNormalizedEvent) -> String {
        guard let label = ClassicAnchorLabel(rawValue: event.finalLabel) else {
            return event.finalLabel
        }
        switch label {
        case .burst:
            return "burst_i"
        case .possibleBurst:
            return event.semanticTrack == ClassicAnchorSemanticTrack.event.rawValue
                ? "burst_ii"
                : "possible_burst_review"
        case .highFrequencyBurst:
            return "hf_burst"
        case .longBurst:
            return "long_burst"
        default:
            return label.rawValue
        }
    }

    static func strongestHFSProposal(
        in candidates: [ClassicAnchorCandidate]
    ) -> ClassicAnchorCandidate? {
        candidates.max { lhs, rhs in
            if lhs.priority != rhs.priority {
                return lhs.priority < rhs.priority
            }
            if lhs.score != rhs.score {
                return lhs.score < rhs.score
            }
            if lhs.nISI != rhs.nISI {
                return lhs.nISI < rhs.nISI
            }
            return lhs.id > rhs.id
        }
    }

    static func validateHFSFinalCandidateSnapshot(
        _ row: HFSBurstArbitrationAuditRow,
        candidateBySourceID: [String: ClassicAnchorCandidate]
    ) throws {
        guard let hfsCandidate = candidateBySourceID[row.hfsCandidateID] else {
            throw STPDResultPackageError.invalidInput(
                "HFS audit row \(row.id) has no normalized HFS candidate"
            )
        }
        let finalEvents = candidateBySourceID.values.filter {
            $0.trainID == row.trainID &&
                $0.selectedForAuto &&
                $0.isEligibleForAutoSelection &&
                $0.arbitrationTrack == .event &&
                $0.finalLabel.isBurstEventFamily &&
                spansOverlap(
                    lhsStart: row.hfsStartISIIndex,
                    lhsEnd: row.hfsEndISIIndex,
                    rhsStart: $0.startISIIndex,
                    rhsEnd: $0.endISIIndex
                )
        }
        let finalSubtypes = finalEvents.map(hfsBurstSubtype).sorted()
        let selectedBurstIICount = finalEvents.filter {
            $0.finalLabel == .possibleBurst && $0.arbitrationTrack == .event
        }.count
        let selectedLongBurstCount = finalEvents.filter {
            $0.finalLabel == .longBurst
        }.count
        guard row.allSelectedBurstEventCount == finalEvents.count,
              row.selectedBurstIICount == selectedBurstIICount,
              row.selectedLongBurstCount == selectedLongBurstCount,
              row.finalSelectedEventSubtypes.sorted() == finalSubtypes else {
            throw STPDResultPackageError.invalidInput(
                "HFS audit row \(row.id) final burst counts or subtypes " +
                    "contradict normalized terminal candidates"
            )
        }

        let hfsSelected =
            hfsCandidate.finalLabel == .highFrequencySpiking &&
            hfsCandidate.selectedForAuto
        let hasLongBurst = selectedLongBurstCount > 0
        let expectedDecision: HFSBurstArbitrationDecision
        if hfsSelected {
            if !finalEvents.isEmpty && row.suppressedBurstProposalCount > 0 {
                expectedDecision = .hfsSelectedWithMixedBurstOutcomes
            } else if hasLongBurst {
                expectedDecision = .hfsSelectedWithLongBurstConflict
            } else if !finalEvents.isEmpty {
                expectedDecision = .hfsSelectedWithBurstConflict
            } else if row.suppressedBurstProposalCount > 0 {
                expectedDecision = .hfsSelectedBurstProposalSuppressed
            } else if row.strongestBurstCandidateID != nil {
                expectedDecision = .hfsSelectedWithBurstConflict
            } else {
                expectedDecision = .hfsSelectedNoBurstWinner
            }
        } else if row.burstDominated && !finalEvents.isEmpty {
            expectedDecision = .burstSelectedHFSRejectedPacketDominance
        } else if hasLongBurst {
            expectedDecision = .longBurstSelectedHFSRejected
        } else if !finalEvents.isEmpty {
            expectedDecision = .burstSelectedHFSNotSelected
        } else if hfsCandidate.finalLabel == .reject ||
                    hfsCandidate.gateStatus.lowercased().contains("reject") {
            expectedDecision = .hfsRejectedNoBurstWinner
        } else {
            expectedDecision = .unresolvedReview
        }

        let expectedReview: Bool
        if row.burstPacketLike && !row.burstDominated {
            expectedReview = true
        } else if row.suppressedBurstProposalCount > 0 ||
                    row.strongestLongBurstCandidateID != nil ||
                    hasLongBurst {
            expectedReview = true
        } else {
            switch expectedDecision {
            case .hfsSelectedWithMixedBurstOutcomes,
                 .hfsSelectedWithLongBurstConflict,
                 .hfsSelectedWithBurstConflict,
                 .hfsSelectedBurstProposalSuppressed,
                 .hfsRejectedNoBurstWinner,
                 .unresolvedReview:
                expectedReview = true
            default:
                expectedReview = false
            }
        }
        let eventText = finalSubtypes.isEmpty
            ? "none"
            : finalSubtypes.joined(separator: "|")
        let reasonTokens = Set(
            row.decisionReason.split(separator: ";").map(String.init)
        )
        guard row.finalDecision == expectedDecision,
              row.requiresReview == expectedReview,
              reasonTokens.contains("decision=\(expectedDecision.rawValue)"),
              reasonTokens.contains("selected_event_subtypes=\(eventText)") else {
            throw STPDResultPackageError.invalidInput(
                "HFS audit row \(row.id) decision, review flag, or reason " +
                    "contradicts normalized terminal candidates"
            )
        }
    }

    static func hfsAuditCanonicalFields(_ row: HFSBurstArbitrationAuditRow) -> [String] {
        [
            "", row.pipelineStage, row.trainID, row.trainName,
            row.hfsCandidateID, row.hfsRootCandidateID,
            STPDCanonicalValue.int(row.hfsStartISIIndex),
            STPDCanonicalValue.int(row.hfsEndISIIndex),
            STPDCanonicalValue.int(row.conflictStartISIIndex),
            STPDCanonicalValue.int(row.conflictEndISIIndex),
            row.scoreScaleNote, STPDCanonicalValue.double(row.hfsRawScore),
            STPDCanonicalValue.int(row.hfsPriority),
            STPDCanonicalValue.bool(row.hfsSelectedForAuto),
            row.hfsSelectionStatus, row.hfsAcceptanceRoute ?? "",
            row.strongestBurstCandidateID ?? "", row.strongestBurstSubtype ?? "",
            STPDCanonicalValue.double(row.strongestBurstRawScore),
            STPDCanonicalValue.int(row.strongestBurstPriority),
            row.strongestLongBurstCandidateID ?? "",
            STPDCanonicalValue.double(row.longBurstRawScore),
            STPDCanonicalValue.int(row.longBurstPriority),
            STPDCanonicalValue.int(row.packetEvidenceEventCount),
            STPDCanonicalValue.int(row.packetCount),
            STPDCanonicalValue.double(row.packetCoverage),
            STPDCanonicalValue.int(row.allSelectedBurstEventCount),
            STPDCanonicalValue.int(row.selectedBurstIICount),
            STPDCanonicalValue.int(row.selectedLongBurstCount),
            STPDCanonicalValue.int(row.suppressedBurstProposalCount),
            STPDCanonicalValue.double(row.pauseLikeThresholdSec),
            STPDCanonicalValue.int(row.pauseLikeBreakCount),
            STPDCanonicalValue.int(row.pauseLikeBreakGroupCount),
            STPDCanonicalValue.double(row.pauseLikeBreakFraction),
            STPDCanonicalValue.double(row.seedBandLowerSec),
            STPDCanonicalValue.double(row.seedBandUpperSec),
            STPDCanonicalValue.double(row.bridgeBandUpperSec),
            STPDCanonicalValue.double(row.seedFraction),
            STPDCanonicalValue.double(row.bridgeFraction),
            STPDCanonicalValue.double(row.hfsShortFraction),
            STPDCanonicalValue.double(row.hfsBridgeFraction),
            STPDCanonicalValue.double(row.hfsLargeFraction),
            STPDCanonicalValue.double(row.hfsCV),
            STPDCanonicalValue.double(row.hfsLV),
            STPDCanonicalValue.bool(row.burstPacketLike),
            STPDCanonicalValue.bool(row.burstDominated),
            row.finalDecision.rawValue,
            STPDCanonicalValue.stringList(row.finalSelectedEventSubtypes.sorted()),
            STPDCanonicalValue.bool(row.requiresReview),
            row.decisionReason,
        ]
    }

    static func validateHFSRow(
        _ row: HFSBurstArbitrationAuditRow,
        candidateUIDBySourceID: [String: String],
        candidateBySourceID: [String: ClassicAnchorCandidate],
        dataset: SpikeDataset
    ) throws {
        try validateDirectFiniteDoubles(
            row,
            entity: "HFS audit row \(row.id)"
        )
        guard let train = dataset.trains.first(where: { $0.id == row.trainID }),
              train.name == row.trainName else {
            throw STPDResultPackageError.invalidInput(
                "HFS audit row \(row.id) references an unknown train or mismatched train name"
            )
        }
        let lastISIIndex = max(0, train.spikeCount - 1)
        guard row.hfsStartISIIndex >= 1,
              row.hfsStartISIIndex <= row.hfsEndISIIndex,
              row.hfsEndISIIndex <= lastISIIndex,
              row.conflictStartISIIndex >= 1,
              row.conflictStartISIIndex <= row.conflictEndISIIndex,
              row.conflictEndISIIndex <= lastISIIndex else {
            throw STPDResultPackageError.invalidInput(
                "HFS audit row \(row.id) has invalid ISI geometry"
            )
        }
        let requiredFinite: [Double?] = [
            row.hfsRawScore,
            row.strongestBurstRawScore,
            row.longBurstRawScore,
            row.packetCoverage,
            row.pauseLikeThresholdSec,
            row.pauseLikeBreakFraction,
            row.seedBandLowerSec,
            row.seedBandUpperSec,
            row.bridgeBandUpperSec,
            row.seedFraction,
            row.bridgeFraction,
            row.hfsShortFraction,
            row.hfsBridgeFraction,
            row.hfsLargeFraction,
            row.hfsCV,
            row.hfsLV,
        ]
        guard requiredFinite.allSatisfy({ $0 == nil || $0?.isFinite == true }) else {
            throw STPDResultPackageError.invalidInput(
                "HFS audit row \(row.id) contains a non-finite numeric value"
            )
        }
        let counts = [
            row.packetEvidenceEventCount,
            row.packetCount,
            row.allSelectedBurstEventCount,
            row.selectedBurstIICount,
            row.selectedLongBurstCount,
            row.suppressedBurstProposalCount,
            row.pauseLikeBreakCount,
            row.pauseLikeBreakGroupCount,
        ]
        guard counts.allSatisfy({ $0 >= 0 }) else {
            throw STPDResultPackageError.invalidInput(
                "HFS audit row \(row.id) contains a negative evidence count"
            )
        }
        let fractions: [Double?] = [
            row.packetCoverage,
            row.pauseLikeBreakFraction,
            row.seedFraction,
            row.bridgeFraction,
            row.hfsShortFraction,
            row.hfsBridgeFraction,
            row.hfsLargeFraction,
        ]
        guard fractions.allSatisfy({
            $0 == nil || (($0 ?? 0) >= 0 && ($0 ?? 0) <= 1)
        }) else {
            throw STPDResultPackageError.invalidInput(
                "HFS audit row \(row.id) contains a fraction outside 0...1"
            )
        }
        guard [row.pauseLikeThresholdSec, row.seedBandLowerSec, row.seedBandUpperSec,
               row.bridgeBandUpperSec].allSatisfy({ $0 == nil || ($0 ?? 0) > 0 }),
              [row.hfsCV, row.hfsLV].allSatisfy({ $0 == nil || ($0 ?? 0) >= 0 }) else {
            throw STPDResultPackageError.invalidInput(
                "HFS audit row \(row.id) contains a non-positive threshold or negative variability metric"
            )
        }
        if let lower = row.seedBandLowerSec, let upper = row.seedBandUpperSec,
           lower > upper {
            throw STPDResultPackageError.invalidInput(
                "HFS audit row \(row.id) has an inverted seed band"
            )
        }
        if let seedUpper = row.seedBandUpperSec, let bridgeUpper = row.bridgeBandUpperSec,
           seedUpper > bridgeUpper {
            throw STPDResultPackageError.invalidInput(
                "HFS audit row \(row.id) has a bridge upper bound below its seed upper bound"
            )
        }
        // The audit row intentionally mixes immutable proposal evidence captured before
        // arbitration (score, priority, acceptance route, and geometry) with terminal
        // selection fields. The normalized terminal candidate must preserve the former
        // while agreeing with the latter; otherwise the package cannot reconstruct the
        // HFS decision from a single, internally consistent lineage.
        guard let hfsCandidate = candidateBySourceID[row.hfsCandidateID],
              hfsCandidate.trainID == row.trainID,
              hfsCandidate.trainName == row.trainName,
              hfsCandidate.finalLabel == .highFrequencySpiking,
              canonicalEqual(hfsCandidate.score, row.hfsRawScore),
              hfsCandidate.priority == row.hfsPriority,
              hfsCandidate.selectedForAuto == row.hfsSelectedForAuto,
              hfsCandidate.selectionStatus == row.hfsSelectionStatus,
              hfsCandidate.hfSpikingAcceptanceRoute == row.hfsAcceptanceRoute,
              min(hfsCandidate.startISIIndex, hfsCandidate.endISIIndex) ==
                row.hfsStartISIIndex,
              max(hfsCandidate.startISIIndex, hfsCandidate.endISIIndex) ==
                row.hfsEndISIIndex else {
            throw STPDResultPackageError.invalidInput(
                "HFS audit row \(row.id) does not match its normalized HFS candidate"
            )
        }
        guard row.conflictStartISIIndex <= row.hfsStartISIIndex,
              row.conflictEndISIIndex >= row.hfsEndISIIndex else {
            throw STPDResultPackageError.invalidInput(
                "HFS audit row \(row.id) conflict span does not contain its HFS span"
            )
        }
        let strongestBurstSnapshotPresence = [
            row.strongestBurstCandidateID?.isEmpty == false,
            row.strongestBurstSubtype?.isEmpty == false,
            row.strongestBurstRawScore != nil,
            row.strongestBurstPriority != nil,
        ]
        guard strongestBurstSnapshotPresence.allSatisfy({ $0 }) ||
                strongestBurstSnapshotPresence.allSatisfy({ !$0 }) else {
            throw STPDResultPackageError.invalidInput(
                "HFS audit row \(row.id) has an incomplete strongest-burst snapshot"
            )
        }
        let strongestLongBurstSnapshotPresence = [
            row.strongestLongBurstCandidateID?.isEmpty == false,
            row.longBurstRawScore != nil,
            row.longBurstPriority != nil,
        ]
        guard strongestLongBurstSnapshotPresence.allSatisfy({ $0 }) ||
                strongestLongBurstSnapshotPresence.allSatisfy({ !$0 }) else {
            throw STPDResultPackageError.invalidInput(
                "HFS audit row \(row.id) has an incomplete strongest-long-burst snapshot"
            )
        }
        let sourceIDs = [
            row.hfsCandidateID,
            row.strongestBurstCandidateID,
            row.strongestLongBurstCandidateID,
        ].compactMap { $0 }.filter { !$0.isEmpty }
        guard sourceIDs.allSatisfy({ candidateUIDBySourceID[$0] != nil }) else {
            let unresolved = sourceIDs.filter {
                candidateUIDBySourceID[$0] == nil
            }.sorted()
            throw STPDResultPackageError.invalidInput(
                "HFS audit row \(row.id) references unknown candidates " +
                    unresolved.joined(separator: "|")
            )
        }
        if let rootCandidate = candidateBySourceID[row.hfsRootCandidateID] {
            guard rootCandidate.trainID == row.trainID,
                  rootCandidate.trainName == row.trainName,
                  rootCandidate.finalLabel == .highFrequencySpiking,
                  spansOverlap(
                    lhsStart: row.hfsStartISIIndex,
                    lhsEnd: row.hfsEndISIIndex,
                    rhsStart: rootCandidate.startISIIndex,
                    rhsEnd: rootCandidate.endISIIndex
                  ) else {
                throw STPDResultPackageError.invalidInput(
                    "HFS audit row \(row.id) root candidate is not an overlapping HFS candidate on its train"
                )
            }
        }
        let overlappingBurstProposals = candidateBySourceID.values.filter {
            $0.trainID == row.trainID &&
                $0.finalLabel.isBurstEventFamily &&
                $0.arbitrationTrack == .event &&
                spansOverlap(
                    lhsStart: row.hfsStartISIIndex,
                    lhsEnd: row.hfsEndISIIndex,
                    rhsStart: $0.startISIIndex,
                    rhsEnd: $0.endISIIndex
                )
        }
        let expectedStrongestBurst = strongestHFSProposal(
            in: overlappingBurstProposals
        )
        guard row.strongestBurstCandidateID == expectedStrongestBurst?.id else {
            throw STPDResultPackageError.invalidInput(
                "HFS audit row \(row.id) strongest-burst reference is not the strongest overlapping event-track burst proposal"
            )
        }
        let expectedStrongestLongBurst = strongestHFSProposal(
            in: overlappingBurstProposals.filter { $0.finalLabel == .longBurst }
        )
        guard row.strongestLongBurstCandidateID == expectedStrongestLongBurst?.id else {
            throw STPDResultPackageError.invalidInput(
                "HFS audit row \(row.id) strongest-long-burst reference is not the strongest overlapping long-burst proposal"
            )
        }
        if let strongestBurstCandidateID = row.strongestBurstCandidateID,
           let strongestBurst = candidateBySourceID[strongestBurstCandidateID],
           let strongestBurstSubtype = row.strongestBurstSubtype,
           let strongestBurstRawScore = row.strongestBurstRawScore,
           let strongestBurstPriority = row.strongestBurstPriority {
            guard strongestBurst.trainID == row.trainID,
                  strongestBurst.finalLabel.isBurstEventFamily,
                  strongestBurstSubtype == hfsBurstSubtype(strongestBurst),
                  canonicalEqual(strongestBurst.score, strongestBurstRawScore),
                  strongestBurst.priority == strongestBurstPriority else {
                throw STPDResultPackageError.invalidInput(
                    "HFS audit row \(row.id) strongest-burst snapshot contradicts its candidate"
                )
            }
        }
        if let strongestLongBurstCandidateID = row.strongestLongBurstCandidateID,
           let strongestLongBurst = candidateBySourceID[strongestLongBurstCandidateID],
           let longBurstRawScore = row.longBurstRawScore,
           let longBurstPriority = row.longBurstPriority {
            guard strongestLongBurst.trainID == row.trainID,
                  strongestLongBurst.finalLabel == .longBurst,
                  canonicalEqual(strongestLongBurst.score, longBurstRawScore),
                  strongestLongBurst.priority == longBurstPriority else {
                throw STPDResultPackageError.invalidInput(
                    "HFS audit row \(row.id) strongest-long-burst snapshot contradicts its candidate"
                )
            }
        }
        _ = try hfsRootLineageUID(
            row,
            datasetDigest: DetectionDatasetSnapshot.make(dataset: dataset).digest,
            candidateBySourceID: candidateBySourceID
        )
    }

    static func makeRow(
        _ headers: [String],
        values: [String: String]
    ) -> [String] {
        headers.map { values[$0] ?? "" }
    }
}

private enum STPDResultPackageValidator {
    static func validate(
        identity: DetectionRunIdentity,
        sourceMode: STPDResultPackageSourceMode,
        tables: [STPDResultTable: STPDResultTableData],
        expectedISICount: Int
    ) throws -> [STPDConsistencyCheck] {
        let tableSet = Set(tables.keys)
        let completeSet = Set(STPDResultTable.allCases)
        let preConsistencySet = completeSet.subtracting([.resultConsistencyCheck])
        guard tableSet == preConsistencySet || tableSet == completeSet else {
            let missing = completeSet.subtracting(tableSet).map(\.rawValue).sorted()
            let unexpected = tableSet.subtracting(completeSet).map(\.rawValue).sorted()
            throw STPDResultPackageError.invalidTable(
                table: "package",
                reason: "table set mismatch; missing=\(missing.joined(separator: "|")); " +
                    "unexpected=\(unexpected.joined(separator: "|"))"
            )
        }

        var checks: [STPDConsistencyCheck] = [
            pass(
                "required_table_set",
                details: "all required tables for this validation stage are present"
            )
        ]

        for table in tables.keys.sorted(by: { $0.rawValue < $1.rawValue }) {
            guard let data = tables[table] else {
                throw STPDResultPackageError.missingTable(table.rawValue)
            }
            try validateIdentity(identity, table: data)
            try validatePrimaryKey(table: data)
        }
        checks.append(pass("run_identity", details: "all rows carry the declared run identity"))
        checks.append(pass("primary_keys", details: "all table primary keys are unique and non-empty"))

        guard let metadata = tables[.runMetadata],
              metadata.rowCount == 1,
              metadata.value(row: 0, column: "source_mode") == sourceMode.rawValue else {
            throw STPDResultPackageError.invalidTable(
                table: STPDResultTable.runMetadata.rawValue,
                reason: "source_mode does not match the package input"
            )
        }
        checks.append(pass("source_mode", details: "metadata source mode is explicit and consistent"))

        let candidateUIDs = try values(
            table: required(.candidateLedger, in: tables),
            column: "candidate_uid"
        )
        let featureUIDs = try values(
            table: required(.candidateFeatures, in: tables),
            column: "candidate_uid"
        )
        let decisionUIDs = try values(
            table: required(.finalDecisions, in: tables),
            column: "candidate_uid"
        )
        guard candidateUIDs == featureUIDs, candidateUIDs == decisionUIDs else {
            throw STPDResultPackageError.invalidTable(
                table: "candidate tables",
                reason: "candidate ledger, features, and decisions do not have one-to-one UID coverage"
            )
        }
        checks.append(pass(
            "candidate_one_to_one",
            details: "ledger, feature, and decision candidate UID sets are identical"
        ))

        try validateCandidateForeignKeys(
            table: required(.candidateDiagnosticAudit, in: tables),
            columns: ["candidate_uid"],
            candidates: candidateUIDs
        )
        try validateCandidateForeignKeys(
            table: required(.reviewStatus, in: tables),
            columns: ["candidate_uid"],
            candidates: candidateUIDs
        )
        try validateCandidateForeignKeys(
            table: required(.isiLabelsFinal, in: tables),
            columns: ["auto_candidate_uid"],
            candidates: candidateUIDs
        )
        try validateCandidateForeignKeys(
            table: required(.hfsBurstArbitrationAudit, in: tables),
            columns: [
                "hfs_candidate_uid",
                "strongest_burst_candidate_uid", "strongest_long_burst_candidate_uid",
            ],
            candidates: candidateUIDs
        )
        try validateDelimitedCandidateForeignKeys(
            table: required(.eventsFinal, in: tables),
            column: "source_candidate_uids",
            candidates: candidateUIDs
        )
        checks.append(pass(
            "candidate_foreign_keys",
            details: "all normalized candidate references resolve to Candidate_ledger"
        ))

        let isiTable = try required(.isiLabelsFinal, in: tables)
        guard isiTable.rowCount == expectedISICount else {
            throw STPDResultPackageError.invalidTable(
                table: STPDResultTable.isiLabelsFinal.rawValue,
                reason: "expected \(expectedISICount) interval rows, got \(isiTable.rowCount)"
            )
        }
        checks.append(pass(
            "isi_complete_coverage",
            details: "exactly one row is present for each dataset ISI"
        ))

        let eventTable = try required(.eventsFinal, in: tables)
        let eventUIDs = try values(table: eventTable, column: "event_uid")
        try validateEventDiagnosticForeignKeys(
            table: required(.candidateDiagnosticAudit, in: tables),
            events: eventUIDs
        )
        checks.append(pass(
            "event_source_evidence",
            details: "every event-source diagnostic resolves to a final event"
        ))

        try validateReviewEvidence(
            sourceMode: sourceMode,
            manualTable: required(.manualAnnotations, in: tables),
            reviewTable: required(.reviewStatus, in: tables),
            eventTable: eventTable,
            isiTable: isiTable
        )
        checks.append(pass(
            "review_authority",
            details: "review evidence, ISI links, and authority flags are causally closed"
        ))

        try validateQCColumns(isiTable)
        checks.append(pass(
            "isi_qc_provenance",
            details: "authoritative ISI classifications, thresholds, and train QC snapshots are internally consistent"
        ))

        let sortedChecks = checks.sorted { $0.id < $1.id }
        if let consistencyTable = tables[.resultConsistencyCheck] {
            try validateConsistencyTable(consistencyTable, expected: sortedChecks)
        }
        return sortedChecks
    }

    private static func required(
        _ table: STPDResultTable,
        in tables: [STPDResultTable: STPDResultTableData]
    ) throws -> STPDResultTableData {
        guard let data = tables[table] else {
            throw STPDResultPackageError.missingTable(table.rawValue)
        }
        return data
    }

    private static func validateIdentity(
        _ identity: DetectionRunIdentity,
        table: STPDResultTableData
    ) throws {
        guard let runIndex = table.headers.firstIndex(of: "run_id"),
              let settingsIndex = table.headers.firstIndex(of: "settings_digest") else {
            throw STPDResultPackageError.invalidTable(
                table: table.contract.table.rawValue,
                reason: "identity columns are unavailable"
            )
        }
        for row in table.rows
            where row[runIndex] != identity.runID ||
                row[settingsIndex] != identity.settingsDigest {
            throw STPDResultPackageError.invalidTable(
                table: table.contract.table.rawValue,
                reason: "row identity differs from the declared detector run"
            )
        }
    }

    private static func validatePrimaryKey(
        table: STPDResultTableData
    ) throws {
        let indices = table.contract.primaryKey.compactMap {
            table.headers.firstIndex(of: $0)
        }
        guard indices.count == table.contract.primaryKey.count else {
            throw STPDResultPackageError.invalidTable(
                table: table.contract.table.rawValue,
                reason: "primary-key columns are unavailable"
            )
        }
        var seen = Set<String>()
        for row in table.rows {
            let parts = indices.map { row[$0] }
            guard parts.allSatisfy({ !$0.isEmpty }) else {
                throw STPDResultPackageError.invalidTable(
                    table: table.contract.table.rawValue,
                    reason: "primary-key value is empty"
                )
            }
            let key = parts.map { "\(Data($0.utf8).count):\($0)" }.joined()
            guard seen.insert(key).inserted else {
                throw STPDResultPackageError.duplicatePrimaryKey(
                    table: table.contract.table.rawValue,
                    key: parts.joined(separator: "|")
                )
            }
        }
    }

    private static func values(
        table: STPDResultTableData,
        column: String
    ) throws -> Set<String> {
        guard let index = table.headers.firstIndex(of: column) else {
            throw STPDResultPackageError.invalidTable(
                table: table.contract.table.rawValue,
                reason: "missing column \(column)"
            )
        }
        return Set(table.rows.map { $0[index] })
    }

    private static func validateCandidateForeignKeys(
        table: STPDResultTableData,
        columns: [String],
        candidates: Set<String>
    ) throws {
        for column in columns {
            guard let index = table.headers.firstIndex(of: column) else {
                throw STPDResultPackageError.invalidTable(
                    table: table.contract.table.rawValue,
                    reason: "missing candidate foreign-key column \(column)"
                )
            }
            for row in table.rows {
                let value = row[index]
                if !value.isEmpty, !candidates.contains(value) {
                    throw STPDResultPackageError.foreignKeyViolation(
                        table: table.contract.table.rawValue,
                        column: column,
                        value: value
                    )
                }
            }
        }
    }

    private static func validateDelimitedCandidateForeignKeys(
        table: STPDResultTableData,
        column: String,
        candidates: Set<String>
    ) throws {
        guard let index = table.headers.firstIndex(of: column) else {
            throw STPDResultPackageError.invalidTable(
                table: table.contract.table.rawValue,
                reason: "missing candidate foreign-key column \(column)"
            )
        }
        for row in table.rows {
            guard let values = STPDCanonicalValue.parseStringList(row[index]) else {
                throw STPDResultPackageError.invalidTable(
                    table: table.contract.table.rawValue,
                    reason: "\(column) is not a canonical string list"
                )
            }
            for value in values where !candidates.contains(value) {
                throw STPDResultPackageError.foreignKeyViolation(
                    table: table.contract.table.rawValue,
                    column: column,
                    value: value
                )
            }
        }
    }

    private static func validateEventDiagnosticForeignKeys(
        table: STPDResultTableData,
        events: Set<String>
    ) throws {
        let kindIndex = try columnIndex("evidence_kind", in: table)
        let eventIndex = try columnIndex("event_uid", in: table)
        for row in table.rows where row[kindIndex] == "event_source" {
            let eventUID = row[eventIndex]
            guard !eventUID.isEmpty, events.contains(eventUID) else {
                throw STPDResultPackageError.foreignKeyViolation(
                    table: table.contract.table.rawValue,
                    column: "event_uid",
                    value: eventUID
                )
            }
        }
    }

    private static func validateReviewEvidence(
        sourceMode: STPDResultPackageSourceMode,
        manualTable: STPDResultTableData,
        reviewTable: STPDResultTableData,
        eventTable: STPDResultTableData,
        isiTable: STPDResultTableData
    ) throws {
        let manualUIDs = try nonemptyValues(
            table: manualTable,
            column: "annotation_id"
        )
        let reviewUIDs = try nonemptyValues(
            table: reviewTable,
            column: "review_uid"
        )
        let evidenceUIDs = manualUIDs.union(reviewUIDs)
        let isiUIDs = try nonemptyValues(table: isiTable, column: "isi_uid")
        var evidenceByISIUID: [String: Set<String>] = [:]
        var anyChangedProjection = false

        let isiUIDIndex = try columnIndex("isi_uid", in: isiTable)
        let isiEvidenceIndex = try columnIndex("review_evidence_uids", in: isiTable)
        let isiPresentIndex = try columnIndex("review_evidence_present", in: isiTable)
        let isiChangedIndex = try columnIndex("review_changed_projection", in: isiTable)
        for row in isiTable.rows {
            let isiUID = row[isiUIDIndex]
            let rowEvidence = delimitedValues(row[isiEvidenceIndex])
            let evidencePresent = try parseBoolean(
                row[isiPresentIndex],
                table: isiTable,
                column: "review_evidence_present"
            )
            let changedProjection = try parseBoolean(
                row[isiChangedIndex],
                table: isiTable,
                column: "review_changed_projection"
            )
            let unresolvedEvidence = rowEvidence.subtracting(evidenceUIDs)
            guard unresolvedEvidence.isEmpty else {
                throw STPDResultPackageError.invalidTable(
                    table: isiTable.contract.table.rawValue,
                    reason: "ISI \(isiUID) references unresolved review evidence " +
                        unresolvedEvidence.sorted().joined(separator: "|")
                )
            }
            guard evidencePresent == !rowEvidence.isEmpty else {
                throw STPDResultPackageError.invalidTable(
                    table: isiTable.contract.table.rawValue,
                    reason: "ISI \(isiUID) review_evidence_present contradicts its evidence list"
                )
            }
            guard !changedProjection || evidencePresent else {
                throw STPDResultPackageError.invalidTable(
                    table: isiTable.contract.table.rawValue,
                    reason: "ISI \(isiUID) changes the public projection without review evidence"
                )
            }
            anyChangedProjection = anyChangedProjection || changedProjection
            evidenceByISIUID[isiUID] = rowEvidence
        }

        let eventEvidenceIndex = try columnIndex("review_evidence_uids", in: eventTable)
        let eventPresentIndex = try columnIndex("review_evidence_present", in: eventTable)
        let eventChangedIndex = try columnIndex("review_changed_projection", in: eventTable)
        for row in eventTable.rows {
            let rowEvidence = delimitedValues(row[eventEvidenceIndex])
            let evidencePresent = try parseBoolean(
                row[eventPresentIndex],
                table: eventTable,
                column: "review_evidence_present"
            )
            let changedProjection = try parseBoolean(
                row[eventChangedIndex],
                table: eventTable,
                column: "review_changed_projection"
            )
            guard rowEvidence.isSubset(of: evidenceUIDs),
                  evidencePresent == !rowEvidence.isEmpty,
                  !changedProjection || evidencePresent else {
                throw STPDResultPackageError.invalidTable(
                    table: eventTable.contract.table.rawValue,
                    reason: "event has unresolved evidence or contradictory review flags"
                )
            }
            anyChangedProjection = anyChangedProjection || changedProjection
        }

        try validateEvidenceLinks(
            table: manualTable,
            evidenceUIDColumn: "annotation_id",
            isiUIDs: isiUIDs,
            evidenceByISIUID: evidenceByISIUID
        )
        try validateEvidenceLinks(
            table: reviewTable,
            evidenceUIDColumn: "review_uid",
            isiUIDs: isiUIDs,
            evidenceByISIUID: evidenceByISIUID
        )

        switch sourceMode {
        case .automatic:
            guard manualTable.rows.isEmpty,
                  reviewTable.rows.isEmpty,
                  evidenceUIDs.isEmpty,
                  evidenceByISIUID.values.allSatisfy(\.isEmpty),
                  !anyChangedProjection else {
                throw STPDResultPackageError.invalidTable(
                    table: "review authority",
                    reason: "automatic mode contains review evidence"
                )
            }
        case .reviewed:
            guard !evidenceUIDs.isEmpty,
                  evidenceByISIUID.values.contains(where: { !$0.isEmpty }) else {
                throw STPDResultPackageError.invalidTable(
                    table: "review authority",
                    reason: "reviewed mode has no evidence-bearing ISI"
                )
            }
        }
    }

    private static func validateEvidenceLinks(
        table: STPDResultTableData,
        evidenceUIDColumn: String,
        isiUIDs: Set<String>,
        evidenceByISIUID: [String: Set<String>]
    ) throws {
        let evidenceIndex = try columnIndex(evidenceUIDColumn, in: table)
        let linksIndex = try columnIndex("linked_isi_uids", in: table)
        let scopeIndex = try columnIndex("link_scope", in: table)
        for row in table.rows {
            let evidenceUID = row[evidenceIndex]
            let linkedUIDs = delimitedValues(row[linksIndex])
            guard row[scopeIndex] == "public_projection",
                  linkedUIDs.isSubset(of: isiUIDs) else {
                throw STPDResultPackageError.invalidTable(
                    table: table.contract.table.rawValue,
                    reason: "\(evidenceUIDColumn) \(evidenceUID) links to an unknown ISI"
                )
            }
            for isiUID in linkedUIDs
            where evidenceByISIUID[isiUID]?.contains(evidenceUID) != true {
                throw STPDResultPackageError.invalidTable(
                    table: table.contract.table.rawValue,
                    reason: "\(evidenceUIDColumn) \(evidenceUID) is not attached to linked ISI \(isiUID)"
                )
            }
        }
    }

    private static func validateQCColumns(_ table: STPDResultTableData) throws {
        let classIndex = try columnIndex("isi_qc_class", in: table)
        let floorStatusIndex = try columnIndex("artifact_floor_status", in: table)
        let suspectIndex = try columnIndex("qc_refractory_suspect", in: table)
        let artifactThresholdIndex = try columnIndex(
            "qc_artifact_threshold_sec",
            in: table
        )
        let refractoryThresholdIndex = try columnIndex(
            "qc_refractory_threshold_sec",
            in: table
        )
        let isiIndex = try columnIndex("isi_sec", in: table)
        let trainIndex = try columnIndex("train_id", in: table)
        let snapshotColumns = table.headers.filter {
            $0.hasPrefix("train_qc_")
        }
        let snapshotIndices = try snapshotColumns.map {
            try columnIndex($0, in: table)
        }
        var snapshotByTrain: [String: [String]] = [:]
        var rowCountByTrain: [String: Int] = [:]
        var artifactCountByTrain: [String: Int] = [:]
        var refractoryCountByTrain: [String: Int] = [:]
        for row in table.rows {
            guard let isi = Double(row[isiIndex]),
                  isi.isFinite,
                  let artifactThreshold = Double(row[artifactThresholdIndex]),
                  artifactThreshold.isFinite,
                  artifactThreshold >= 0,
                  let refractoryThreshold = Double(row[refractoryThresholdIndex]),
                  refractoryThreshold.isFinite,
                  refractoryThreshold >= 0 else {
                throw STPDResultPackageError.invalidTable(
                    table: table.contract.table.rawValue,
                    reason: "ISI QC row contains an invalid ISI or QC threshold"
                )
            }
            let artifactTolerance = max(1e-12, abs(artifactThreshold) * 1e-6)
            let refractoryTolerance = max(
                1e-12,
                abs(refractoryThreshold) * 1e-6
            )
            let expectedClass: String
            if isi < artifactThreshold - artifactTolerance {
                expectedClass = "artifact_below_floor"
            } else if refractoryThreshold > artifactThreshold,
                      isi < refractoryThreshold - refractoryTolerance {
                expectedClass = "refractory_suspect"
            } else {
                expectedClass = "valid"
            }
            let suspect = try parseBoolean(
                row[suspectIndex],
                table: table,
                column: "qc_refractory_suspect"
            )
            guard row[classIndex] == expectedClass,
                  row[floorStatusIndex] ==
                    (expectedClass == "artifact_below_floor"
                        ? "below_artifact_floor"
                        : "at_or_above_artifact_floor"),
                  suspect == (expectedClass == "refractory_suspect") else {
                throw STPDResultPackageError.invalidTable(
                    table: table.contract.table.rawValue,
                    reason: "ISI QC row has contradictory classification fields"
                )
            }
            let trainID = row[trainIndex]
            let snapshot = snapshotIndices.map { row[$0] }
            if let prior = snapshotByTrain[trainID], prior != snapshot {
                throw STPDResultPackageError.invalidTable(
                    table: table.contract.table.rawValue,
                    reason: "train \(trainID) has inconsistent repeated QC snapshots"
                )
            }
            snapshotByTrain[trainID] = snapshot
            rowCountByTrain[trainID, default: 0] += 1
            artifactCountByTrain[trainID, default: 0] +=
                expectedClass == "artifact_below_floor" ? 1 : 0
            refractoryCountByTrain[trainID, default: 0] +=
                expectedClass == "refractory_suspect" ? 1 : 0
        }

        let countColumns = [
            "train_qc_duplicate_timestamp_count",
            "train_qc_zero_or_negative_isi_count",
            "train_qc_zero_or_negative_timestamp_step_count",
            "train_qc_input_nonmonotonic_step_count",
            "train_qc_input_duplicate_timestamp_step_count",
            "train_qc_input_zero_or_negative_step_count",
            "train_qc_dropped_duplicate_timestamp_count",
            "train_qc_artifact_isi_count",
            "train_qc_refractory_suspect_isi_count",
            "train_qc_valid_isi_count",
        ]
        let fractionColumns = [
            "train_qc_artifact_fraction",
            "train_qc_refractory_suspect_fraction",
        ]
        for (trainID, snapshot) in snapshotByTrain {
            let values = Dictionary(
                uniqueKeysWithValues: zip(snapshotColumns, snapshot)
            )
            for column in countColumns {
                guard let value = values[column],
                      let count = Int(value),
                      count >= 0 else {
                    throw STPDResultPackageError.invalidTable(
                        table: table.contract.table.rawValue,
                        reason: "train \(trainID) has invalid QC count \(column)"
                    )
                }
            }
            for column in fractionColumns {
                guard let value = values[column] else { continue }
                if value.isEmpty {
                    continue
                }
                guard let fraction = Double(value),
                      fraction.isFinite,
                      (0...1).contains(fraction) else {
                    throw STPDResultPackageError.invalidTable(
                        table: table.contract.table.rawValue,
                        reason: "train \(trainID) has invalid QC fraction \(column)"
                    )
                }
            }
            guard Int(values["train_qc_artifact_isi_count"] ?? "") ==
                    artifactCountByTrain[trainID],
                  Int(values["train_qc_refractory_suspect_isi_count"] ?? "") ==
                    refractoryCountByTrain[trainID],
                  (Int(values["train_qc_valid_isi_count"] ?? "") ?? -1) <=
                    (rowCountByTrain[trainID] ?? 0) else {
                throw STPDResultPackageError.invalidTable(
                    table: table.contract.table.rawValue,
                    reason: "train \(trainID) QC counts contradict its exported ISI rows"
                )
            }
        }
    }

    private static func validateConsistencyTable(
        _ table: STPDResultTableData,
        expected: [STPDConsistencyCheck]
    ) throws {
        let idIndex = try columnIndex("check_id", in: table)
        let statusIndex = try columnIndex("status", in: table)
        let severityIndex = try columnIndex("severity", in: table)
        let detailsIndex = try columnIndex("details", in: table)
        let actual = table.rows.map {
            [$0[idIndex], $0[statusIndex], $0[severityIndex], $0[detailsIndex]]
        }
        let required = expected.map { [$0.id, $0.status, $0.severity, $0.details] }
        guard actual == required else {
            throw STPDResultPackageError.invalidTable(
                table: table.contract.table.rawValue,
                reason: "consistency rows do not exactly match the checks that were executed"
            )
        }
    }

    private static func nonemptyValues(
        table: STPDResultTableData,
        column: String
    ) throws -> Set<String> {
        Set(try values(table: table, column: column).filter { !$0.isEmpty })
    }

    private static func delimitedValues(_ value: String) -> Set<String> {
        Set(STPDCanonicalValue.parseStringList(value) ?? [])
    }

    private static func parseBoolean(
        _ value: String,
        table: STPDResultTableData,
        column: String
    ) throws -> Bool {
        switch value {
        case "true":
            return true
        case "false":
            return false
        default:
            throw STPDResultPackageError.invalidTable(
                table: table.contract.table.rawValue,
                reason: "column \(column) contains non-boolean value \(value)"
            )
        }
    }

    private static func columnIndex(
        _ column: String,
        in table: STPDResultTableData
    ) throws -> Int {
        guard let index = table.headers.firstIndex(of: column) else {
            throw STPDResultPackageError.invalidTable(
                table: table.contract.table.rawValue,
                reason: "missing column \(column)"
            )
        }
        return index
    }

    private static func pass(
        _ id: String,
        details: String
    ) -> STPDConsistencyCheck {
        STPDConsistencyCheck(
            id: id,
            status: "pass",
            severity: "info",
            details: details
        )
    }
}
